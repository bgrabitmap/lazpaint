unit UClipboard;

{$mode objfpc}{$H+}
{ $DEFINE DEBUG_CLIPBOARD}

{$IFDEF DARWIN}
  {$DEFINE TIFF_CLIPBOARD_FORMAT}
{$ENDIF}

interface

uses
  Classes, SysUtils, BGRABitmap;

procedure CopyToClipboard(bmp: TBGRABitmap);
function GetBitmapFromClipboard: TBGRABitmap;
function ClipboardContainsBitmap: boolean;

implementation

uses Dialogs, BGRABitmapTypes, Clipbrd, Graphics, LCLIntf, LCLType,
    BGRADNetDeserial, math, GraphType

    , FPWriteBMP
{$IFDEF TIFF_CLIPBOARD_FORMAT}, FPReadTIFF, FPWriteTIFF
{$ENDIF};

{$IFDEF DEBUG_CLIPBOARD}
const
  moreMimeTypes : array[0..6] of string =
     ('image/x-xbitmap','image/gif','image/jpeg','image/pjpeg','image/png','image/x-png','image/tiff');
{$ENDIF}

{$IFDEF TIFF_CLIPBOARD_FORMAT}
var
  secondaryClipboardFormat: TClipboardFormat;
  secondaryReader: TFPReaderTiff;
  secondaryWriter: TFPWriterTiff;
{$ENDIF}

var
  bgraClipboardFormat: TClipboardFormat;

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

function SafeClipboardFormatToMimeType(FormatID: TClipboardFormat): string;
begin
  try
    result := ClipboardFormatToMimeType(FormatID);
  except
    on ex: Exception do
      result := '';
  end;
end;

function GetBitmapFromClipboard: TBGRABitmap;
var i: integer;
    Stream: TMemoryStream;

{$IFDEF DEBUG_CLIPBOARD}
    j: integer;
    pcf: TPredefinedClipboardFormat;
    str,mime: string;
    data : string;
    c: char;
{$ENDIF}

    deserial: TDotNetDeserialization;

begin
  result := nil;

  {$IFDEF DEBUG_CLIPBOARD}
  str := '';
  for i := 0 to clipboard.FormatCount-1 do
  begin
    if str <> '' then str += ', ';
    str := str + inttostr(clipboard.Formats[i])+'=';
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

    if (mime = 'PSP Selection Mask') or (mime = 'image/bmp') then
    begin
      stream := TMemoryStream.Create;
      Clipboard.GetFormat(Clipboard.Formats[i],Stream);

      str += '['+inttostr(stream.Size)+' o]';

       if stream.Size > 1024 then
        setlength(data,1024) else
          setlength(data,stream.size);
      stream.Position:= 0;
      stream.read(data[1],length(data));

      str += LineEnding+'Data=[';
      for j := 1 to length(data) do
      begin
        c := data[j];
        if c in[#32..#126] then
          str+= c else
        str += ' '+inttohex(ord(c),2)+' ';
      end;
      str += ']'+lineending+lineending;

      stream.Free;
    end;

  end;
  ShowMessage(str);
  {$ENDIF}

  for i := 0 to clipboard.FormatCount-1 do
    if SafeClipboardFormatToMimeType(Clipboard.Formats[i]) = 'PaintDotNet.MaskedSurface' then
    begin
       Stream := TMemoryStream.Create;
       Clipboard.GetFormat(Clipboard.Formats[i],Stream);
       stream.Position := 0;
       deserial := TDotNetDeserialization.Create;
       deserial.LoadFromStream(stream);
       Stream.Free;
       result := GetBitmapFromPaintDotNetMaskedSurface(deserial);
       deserial.Free;
       if result <> nil then exit;
    end;

  for i := 0 to clipboard.FormatCount-1 do
    if Clipboard.Formats[i] = bgraClipboardFormat then
    begin
       Stream := TMemoryStream.Create;
       Clipboard.GetFormat(Clipboard.Formats[i],Stream);
       stream.Position := 0;
       result := TBGRABitmap.Create;
       try
         result.Deserialize(Stream);
       except
         on ex:exception do
         begin
           FreeAndNil(result);
         end;
       end;
       Stream.Free;
       if result <> nil then exit;
    end;

  for i := 0 to clipboard.FormatCount-1 do
    if (Clipboard.Formats[i] = PredefinedClipboardFormat(pcfDelphiBitmap)) or
       (Clipboard.Formats[i] = PredefinedClipboardFormat(pcfBitmap)) then
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

  {$IFDEF TIFF_CLIPBOARD_FORMAT}
  for i := 0 to clipboard.FormatCount-1 do
    if Clipboard.Formats[i] = secondaryClipboardFormat then
    begin
      Stream := TMemoryStream.Create;
      Clipboard.GetFormat(Clipboard.Formats[i],Stream);
      Stream.Position := 0;
      try
        result := TBGRABitmap.Create;
        result.LoadFromStream(Stream, secondaryReader);
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
  bmpWriter: TFPWriterBMP;
begin
  Clipboard.Clear;

  stream := TMemoryStream.Create;
  bmpWriter := TFPWriterBMP.Create;
  bmpWriter.BitsPerPixel := 32;
  bmp.SaveToStream(stream, bmpWriter);
  bmpWriter.Free;
  Clipboard.AddFormat(PredefinedClipboardFormat(pcfBitmap), stream);
  stream.Free;

  stream := TMemoryStream.Create;
  bmp.Serialize(stream);
  Clipboard.AddFormat(bgraClipboardFormat, stream);
  stream.Free;

  {$IFDEF TIFF_CLIPBOARD_FORMAT}
  stream := TMemoryStream.Create;
  bmp.SaveToStream(stream, secondaryWriter);
  Clipboard.AddFormat(secondaryClipboardFormat, stream);
  stream.Free;
  {$ENDIF}
end;

initialization

  bgraClipboardFormat := RegisterClipboardFormat('TBGRABitmap');

{$IFDEF TIFF_CLIPBOARD_FORMAT}
  secondaryClipboardFormat := RegisterClipboardFormat('image/tiff');
  secondaryReader := TFPReaderTiff.Create;
  secondaryWriter := TFPWriterTiff.Create;
{$ENDIF}

finalization

{$IFDEF TIFF_CLIPBOARD_FORMAT}
  secondaryReader.free;
  secondaryWriter.free;
{$ENDIF}

end.

