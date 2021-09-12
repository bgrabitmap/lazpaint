// SPDX-License-Identifier: GPL-3.0-only
unit UClipboard;

{$mode objfpc}{$H+}
{$IFDEF DEBUG}
  { $DEFINE DEBUG_CLIPBOARD}
{$ENDIF}

{$DEFINE HTML_CLIPBOARD_FORMAT}
{$IFDEF DARWIN}
  {$DEFINE TIFF_CLIPBOARD_FORMAT}
{$ELSE}
  {$DEFINE BMP_CLIPBOARD_FORMAT}
  {$DEFINE PNG_CLIPBOARD_FORMAT}
{$ENDIF}
{$IFDEF WINDOWS}
  {$DEFINE PDN_CLIPBOARD_FORMAT}
{$ENDIF}

interface

uses
  Classes, SysUtils, BGRABitmap;

procedure CopyToClipboard(bmp: TBGRABitmap);
function GetBitmapFromClipboard: TBGRABitmap;
function ClipboardContainsBitmap: boolean;

implementation

uses Dialogs, BGRABitmapTypes, Clipbrd, Graphics, LCLIntf, LCLType, GraphType
    {$IFDEF PDN_CLIPBOARD_FORMAT}, math, BGRADNetDeserial{$ENDIF}
    {$IFDEF BMP_CLIPBOARD_FORMAT}, FPWriteBMP{$ENDIF}
    {$IFDEF HTML_CLIPBOARD_FORMAT}, UOnline{$ENDIF};

{$IFDEF DEBUG_CLIPBOARD}
const
  moreMimeTypes : array[0..6] of string =
     ('image/x-xbitmap','image/gif','image/jpeg','image/pjpeg','image/png','image/x-png','image/tiff');
{$ENDIF}

{$IFDEF TIFF_CLIPBOARD_FORMAT}
var
  tiffClipboardFormat: TClipboardFormat;
{$ENDIF}
{$IFDEF PNG_CLIPBOARD_FORMAT}
var
  pngClipboardFormat: TClipboardFormat;
{$ENDIF}
{$IFDEF PDN_CLIPBOARD_FORMAT}
var
  pdnClipboardFormat: TClipboardFormat;

function GetBitmapFromPaintDotNetMaskedSurface(deserial: TDotNetDeserialization): TBGRABitmap;
var width,height: integer;
    dataObj: TSerializedArray;
    data: pointer;
    dataSize: longword;
    mainObj: TSerializedClass;

    segments,segment,poly: TSerializedArray;
    point: TSerializedClass;
    mask: TBGRABitmap;
    polyPts: array of TPointF;
    polyList: array of array of TPointF;
    i: Integer;
    j: Integer;
    k: Integer;

    minx,miny: double;
    offset: TPointF;
begin
  result := nil;
  mainObj := deserial.FindClass('PaintDotNet.MaskedSurface');
  try
    width := StrToInt(deserial.GetSimpleField(mainObj,'surface\width'));
    height := StrToInt(deserial.GetSimpleField(mainObj,'surface\height'));
    dataObj := deserial.GetObjectField(mainObj, 'surface\scan0\chunk0') as TSerializedArray;
    if dataObj <> nil then
    begin
       data := dataObj.ItemPtr[0];
       dataSize := dataObj.ItemSize * dataObj.FieldCount;
       result := TBGRABitmap.Create(width,height);
       minx := 1e100;
       miny := 1e100;
       move(data^, result.Data^, min(dataSize, int64(result.NbPixels) * sizeof(TBGRAPixel)));
       if result.LineOrder = riloBottomToTop then result.VerticalFlip;

       mask := TBGRABitmap.Create(width,height, BGRABlack);
       polyList := nil;
       try
         segments := deserial.GetObjectField(mainObj,'geometryMask\polygonList\segments') as TSerializedArray;
         if segments <> nil then
         begin
           for i := 0 to segments.FieldCount-1 do
           begin
             segment := deserial.GetObjectField(segments,i) as TSerializedArray;
             if segment <> nil then
               for j := 0 to segment.FieldCount-1 do
               begin
                 poly := deserial.GetObjectField(segment,j) as TSerializedArray;
                 if poly <> nil then
                 begin
                   polyPts := nil;
                   setlength(polyPts, poly.FieldCount);
                   for k := 0 to high(polyPts) do
                   begin
                     point := deserial.GetObjectField(poly,k) as TSerializedClass;
                     polyPts[k] := PointF(StrToFloat(point.FieldByNameAsString['_x']),
                                          StrToFloat(point.FieldByNameAsString['_y']));
                     if polyPts[k].x < minx then minx := polyPts[k].x;
                     if polyPts[k].y < miny then miny := polyPts[k].y;
                   end;
                   setlength(polyList, length(polyList)+1);
                   polyList[high(polyList)] := polyPts;
                 end;
               end;
           end;
         end;

         offset := PointF(floor(minx)+0.5,floor(miny)+0.5);
         for i := 0 to high(polyList) do
         begin
           polyPts := polyList[i];
           for j := 0 to high(polyPts) do
             polyPts[j] -= offset;
           mask.FillPolyAntialias(polyPts,BGRAWhite);
         end;
         result.ApplyMask(mask);
       finally
         mask.Free;
       end;
    end;
  except
    on ex:Exception do
    begin
      //nothing
    end;
  end;
end;
{$ENDIF}

{$IFDEF HTML_CLIPBOARD_FORMAT}
var
  htmlClipboardFormat: TClipboardFormat;

function WideStringToStr(data: string): string;
var
  i: integer;
  isWidestring: boolean;
  w: WideString;
begin
  isWidestring := (length(data)>0) and ((length(data) and 1) = 0);
  i := 2;
  while (i <= length(data)) do
  begin
    if data[i] <> #0 then
    begin
      isWidestring := false;
      break;
    end;
    inc(i,2);
  end;
  if isWidestring then
  begin
    w := '';
    setlength(w, length(data) div 2);
    move(data[1],w[1],length(data));
    result := UTF8Encode(w);
  end
    else result := data;
end;

function HtmlEntitiesToText(data: string): string;
var p,start: integer;
  entity: string;
  charcode,errpos: integer;
begin
  p := 1;
  while p <= length(data)-1 do
  begin
    if (data[p]='&') and (data[p+1] in ['#','a'..'z','A'..'Z']) then
    begin
      start := p;
      inc(p);
      while (p < length(data)) and (data[p+1] in['0'..'9','a'..'z','A'..'Z']) do inc(p);
      entity := copy(data,start,p-start +1);
      if (p < length(data)) and (data[p+1] = ';') then inc(p);
      delete(data, start, p-start+1);
      p := start;
      case entity of
      '&nbsp': entity := #160;
      '&lt': entity := '<';
      '&gt': entity := '>';
      '&amp': entity := '&';
      else
        begin
          if copy(entity,1,2)='&#' then
          begin
            val(copy(entity,3,length(entity)-2),charcode,errpos);
            if (errpos = 0) and (charcode <= 127) then
              entity := char(charcode);
          end;
        end;
      end;
      insert(entity,data,p);
      inc(p,length(entity));
      continue;
    end;
    inc(p);
  end;
  result := data;
end;

function GetBitmapFromTag(tokens: TStringList): TBGRABitmap;
var
  i: Integer;
  stream: TMemoryStream;
  url: string;
begin
  result := nil;
  if tokens.Count > 0 then
  begin
    if UpperCase(tokens[0]) = 'IMG' then
    begin
      for i := 1 to tokens.count-3 do
        if (UpperCase(tokens[i])='SRC')
            and (tokens[i+1]='=') and (tokens[i+2][1] in ['''','"']) then
      begin
        url := HtmlEntitiesToText(copy(tokens[i+2],2,length(tokens[i+2])-2));
        if copy(url,1,8) = 'https://' then
          delete(url,5,1);
        stream := TMemoryStream.Create;
        try
          MyHttpGet(url,stream);
          stream.Position:= 0;
          result := TBGRABitmap.Create(stream);
        except on ex: exception do begin
            ShowMessage(ex.Message);
          end;
        end;
        stream.Free;
        if result <> nil then exit;
      end;
    end;
  end;
  result := nil;
end;

function GetBitmapFromHtml(data: string): TBGRABitmap;
var
  p: integer;
  inTag, inComment: boolean;
  tagTokens: TStringList;
  inStr1, inStr2, inId, inNum: integer;
begin
  result := nil;
  data := WideStringToStr(data);
  inTag := false;
  inComment := false;
  inStr1 := 0;
  inStr2 := 0;
  inId := 0;
  inNum := 0;
  tagTokens := TStringList.Create;
  p := 1;
  while p <= length(data) do
  begin
    if inComment then
    begin
      if data[p] = '-' then
      begin
        if copy(data,p,3) = '-->' then
        begin
          p += 3;
          inComment:= false;
          continue;
        end;
      end;
    end else
    if inStr1<>0 then
    begin
      if data[p] = '''' then
      begin
        tagTokens.add(copy(data,inStr1,p-inStr1+1));
        inStr1 := 0;
      end;
    end else
    if inStr2<>0 then
    begin
      if data[p] = '"' then
      begin
        tagTokens.add(copy(data,inStr2,p-inStr2+1));
        inStr2 := 0;
      end;
    end else
    begin
      if inId<>0 then
      begin
        if not (data[p] in['A'..'Z','a'..'z',':','.']) then
        begin
          tagTokens.add(copy(data,inId,p-inId));
          inId := 0;
        end else
        begin
          inc(p);
          continue;
        end;
      end;
      if inNum<>0 then
      begin
        if not (data[p] in['0'..'9','.']) then
        begin
          tagTokens.add(copy(data,inNum,p-inNum));
          inNum := 0;
        end else
        begin
          inc(p);
          continue;
        end;
      end;
      if data[p]='<' then
      begin
        if copy(data,p,4) = '<!--' then
        begin
          p += 4;
          inComment := true;
          continue;
        end else
          inTag := true;
      end else
      if inTag then
      begin
        if data[p] = '''' then
          inStr1 := p
        else if data[p] = '"' then
          inStr2 := p
        else if data[p] in ['A'..'Z','a'..'z'] then
          inId := p
        else if data[p] in ['0'..'9','+','-'] then
          inNum := p
        else if data[p] = '>' then
        begin
          inTag := false;
          result := GetBitmapFromTag(tagTokens);
          tagTokens.clear;
          if result <> nil then
          begin
            tagTokens.Free;
            exit;
          end;
        end else
          if data[p]>#32 then
            tagTokens.Add(data[p]);
      end;
    end;
    inc(p);
  end;
  tagTokens.Free;
end;
{$ENDIF}

function GetBitmapFromClipboard: TBGRABitmap;
var i: integer;
    Stream: TMemoryStream;
    data: string;

{$IFDEF DEBUG_CLIPBOARD}
    j: integer;
    pcf: TPredefinedClipboardFormat;
    mime, str: string;

    c: char;
    prevCok: boolean;
{$ENDIF}

{$IFDEF PDN_CLIPBOARD_FORMAT}
    deserial: TDotNetDeserialization;
{$ENDIF}

begin
  result := nil;

  {$IFDEF DEBUG_CLIPBOARD}
  str := 'clipboard.FormatCount = '+inttostr(clipboard.FormatCount)+lineending;
  for i := 0 to clipboard.FormatCount-1 do
  begin
    if str <> '' then str += ', ';
    str := str + '#'+inttostr(clipboard.Formats[i])+'=';
    mime := ClipboardFormatToMimeType(clipboard.Formats[i]);
    if mime = '' then
      for pcf := low(TPredefinedClipboardFormat) to high(TPredefinedClipboardFormat) do
        if clipboard.Formats[i] = PredefinedClipboardFormat(pcf) then
           mime := PredefinedClipboardMimeTypes[pcf];
    if mime = '' then
      for j := low(moreMimeTypes) to high(moreMimeTypes) do
        if clipboard.Formats[j] = RegisterClipboardFormat(moreMimeTypes[j]) then
           mime := moreMimeTypes[j];
    str += mime;

    stream := TMemoryStream.Create;
    Clipboard.GetFormat(Clipboard.Formats[i],Stream);

    str += '('+inttostr(stream.Size)+' bytes)';
    if (mime = 'DataObject') or (mime = 'text/html') or (mime = 'HTML Format') or (mime = 'text/plain') then
    begin
       if stream.Size > 1024 then
        setlength(data,1024) else
          setlength(data,stream.size);
      stream.Position:= 0;
      stream.read(data[1],length(data));

      str += '=[';
      prevCok := false;
      for j := 1 to length(data) do
      begin
        c := data[j];
        if c in[#32..#126] then
        begin
          str+= c;
          prevCok := true
        end else
        begin
          if not (prevCOk and (c = #0)) then
            str += ' '+inttohex(ord(c),2)+' ';
          prevCok := false;
        end;
      end;
      str += ']'+lineending;
    end;
    stream.Free;
  end;
  ShowMessage(str);
  {$ENDIF}

  {$IFDEF PDN_CLIPBOARD_FORMAT}
  for i := 0 to clipboard.FormatCount-1 do
    if Clipboard.Formats[i] = pdnClipboardFormat then
    begin
       Stream := TMemoryStream.Create;
       Clipboard.GetFormat(Clipboard.Formats[i],Stream);
       stream.Position := 0;
       deserial := TDotNetDeserialization.Create;
       deserial.LoadFromStream(stream);
       Stream.Free;
       try
         result := GetBitmapFromPaintDotNetMaskedSurface(deserial);
       except
       end;
       deserial.Free;
       if result <> nil then exit;
    end;
  {$ENDIF}

  {$IFDEF TIFF_CLIPBOARD_FORMAT}
  for i := 0 to clipboard.FormatCount-1 do
    if Clipboard.Formats[i] = tiffClipboardFormat then
    begin
      Stream := TMemoryStream.Create;
      Clipboard.GetFormat(Clipboard.Formats[i],Stream);
      Stream.Position := 0;
      try
        result := TBGRABitmap.Create;
        result.LoadFromStream(Stream);
        if result.Empty then result.AlphaFill(255);
      except
        on ex:exception do
        begin
          result := nil;
        end;
      end;
      Stream.Free;
      if result <> nil then exit;
    end;
  {$ENDIF}

  {$IFDEF PNG_CLIPBOARD_FORMAT}
  for i := 0 to clipboard.FormatCount-1 do
    if Clipboard.Formats[i] = pngClipboardFormat then
    begin
      Stream := TMemoryStream.Create;
      Clipboard.GetFormat(Clipboard.Formats[i],Stream);
      Stream.Position := 0;
      try
        result := TBGRABitmap.Create;
        result.LoadFromStream(Stream);
        if result.Empty then result.AlphaFill(255);
      except
        on ex:exception do
        begin
          result := nil;
        end;
      end;
      Stream.Free;
      if result <> nil then exit;
    end;
  {$ENDIF}

  for i := 0 to clipboard.FormatCount-1 do
    if Clipboard.Formats[i] = htmlClipboardFormat then
    begin
       Stream := TMemoryStream.Create;
       Clipboard.GetFormat(Clipboard.Formats[i],Stream);
       data := '';
       if stream.Size > 65536 then
        setlength(data,65536) else
          setlength(data,stream.size);
       stream.Position:= 0;
       stream.read(data[1],length(data));
       Stream.Free;
       try
         result := GetBitmapFromHtml(data);
       except
       end;
       if result <> nil then exit;
    end;

  for i := 0 to clipboard.FormatCount-1 do
    if (Clipboard.Formats[i] = PredefinedClipboardFormat(pcfBitmap)) then
    begin
       Stream := TMemoryStream.Create;
       Clipboard.GetFormat(Clipboard.Formats[i],Stream);
       Stream.Position := 0;
       try
         result := TBGRABitmap.Create(Stream);
         if result.Empty then result.AlphaFill(255);
       except
         on ex:exception do
         begin
           result := nil;
         end;
       end;
       Stream.Free;
       if result <> nil then exit;
    end;
end;

function ClipboardContainsBitmap: boolean;
var temp: TBGRABitmap;
begin
  temp := GetBitmapFromClipboard;
  if (temp=nil) or (temp.Width = 0) or (temp.Height= 0) or temp.Empty then result := false
    else result := true;
  temp.Free;
end;

procedure CopyToClipboard(bmp: TBGRABitmap);
var
  stream: TMemoryStream;
  {$IFDEF BMP_CLIPBOARD_FORMAT}
  bmpWriter: TFPWriterBMP;
  {$ENDIF}
begin
  Clipboard.Clear;

  {$IFDEF BMP_CLIPBOARD_FORMAT}
  stream := TMemoryStream.Create;
  bmpWriter := TFPWriterBMP.Create;
  bmpWriter.BitsPerPixel := 32;
  bmp.SaveToStream(stream, bmpWriter);
  bmpWriter.Free;
  Clipboard.AddFormat(PredefinedClipboardFormat(pcfBitmap), stream);
  stream.Free;
  {$ENDIF}

  {$IFDEF TIFF_CLIPBOARD_FORMAT}
  stream := TMemoryStream.Create;
  bmp.SaveToStreamAs(stream, ifTiff);
  Clipboard.AddFormat(tiffClipboardFormat, stream);
  stream.Free;
  {$ENDIF}

  {$IFDEF PNG_CLIPBOARD_FORMAT}
  stream := TMemoryStream.Create;
  bmp.SaveToStreamAs(stream, ifPng);
  Clipboard.AddFormat(pngClipboardFormat, stream);
  stream.Free;
  {$ENDIF}
end;

initialization

{$IFDEF TIFF_CLIPBOARD_FORMAT}
  tiffClipboardFormat := RegisterClipboardFormat({$IFDEF DARWIN}'public.tiff'{$ELSE}'image/tiff'{$ENDIF});
{$ENDIF}
{$IFDEF PNG_CLIPBOARD_FORMAT}
  pngClipboardFormat := RegisterClipboardFormat({$IFDEF DARWIN}'public.png'{$ELSE}{$IFDEF WINDOWS}'PNG'{$ELSE}'image/png'{$ENDIF}{$ENDIF});
{$ENDIF}
{$IFDEF HTML_CLIPBOARD_FORMAT}
  htmlClipboardFormat := RegisterClipboardFormat({$IFDEF DARWIN}'public.html'{$ELSE}'text/html'{$ENDIF});
{$ENDIF}
{$IFDEF PDN_CLIPBOARD_FORMAT}
  pdnClipboardFormat := RegisterClipboardFormat('PaintDotNet.MaskedSurface');
{$ENDIF}


end.

