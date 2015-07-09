unit BGRAThumbnail;

{$mode objfpc}{$H+}
{$i bgrabitmap.inc}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes, FPimage;

function GetBitmapThumbnail(ABitmap: TBGRABitmap; AWidth,AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil; AVerticalShrink : single = 1): TBGRABitmap;
function GetFileThumbnail(AFilenameUTF8: string; AWidth,AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
function GetStreamThumbnail(AStream: TStream; AWidth,AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ASuggestedExtensionUTF8: string = ''; ADest: TBGRABitmap= nil): TBGRABitmap; overload;
function GetStreamThumbnail(AStream: TStream; AReader: TFPCustomImageReader; AWidth,AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap; overload;

function GetOpenRasterThumbnail(AStream: TStream; AWidth,AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
function GetLazPaintThumbnail(AStream: TStream; AWidth, AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
function GetJpegThumbnail(AStream: TStream; AWidth,AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
function GetPsdThumbnail(AStream: TStream; AWidth,AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
function GetPngThumbnail(AStream: TStream; AWidth, AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
function GetPaintDotNetThumbnail(AStream: TStream; AWidth, AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
function GetBmpThumbnail(AStream: TStream; AWidth, AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
{$IFDEF BGRABITMAP_USE_LCL}
function GetIcoThumbnail(AStream: TStream; AWidth, AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
{$ENDIF}

function GetPcxThumbnail(AStream: TStream; AWidth, AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
function GetTargaThumbnail(AStream: TStream; AWidth, AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
function GetTiffThumbnail(AStream: TStream; AWidth, AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
function GetGifThumbnail(AStream: TStream; AWidth, AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
function GetXwdThumbnail(AStream: TStream; AWidth, AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
function GetXPixMapThumbnail(AStream: TStream; AWidth, AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
function GetBmpMioMapThumbnail(AStream: TStream; AWidth, AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;

procedure DrawThumbnailCheckers(bmp: TBGRABitmap; ARect: TRect);

implementation

uses Types, base64, BGRAUTF8, {$IFDEF BGRABITMAP_USE_LCL}Graphics, GraphType,{$ENDIF}
     DOM, XMLRead, FPReadJPEG, BGRAReadPng, BGRAReadGif, BGRAReadBMP,
     BGRAReadPSD, BGRAReadIco, UnzipperExt, BGRAReadLzp;

procedure DrawThumbnailCheckers(bmp: TBGRABitmap; ARect: TRect);
begin
  bmp.DrawCheckers(ARect, BGRA(255,255,255), BGRA(220,220,220));
end;

function GetBitmapThumbnail(ABitmap: TBGRABitmap; AWidth, AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap; AVerticalShrink: single
  ): TBGRABitmap;
var
  factorX, factorY, factor: single;
  xIcon,yIcon,wIcon,hIcon: Integer;
begin
  result := nil;
  try
    if (ABitmap <> nil) and (ABitmap.Width <> 0) and (ABitmap.Height <> 0) then
    begin
      If Assigned(ADest) then
      begin
        result := ADest;
        result.SetSize(AWidth,AHeight);
        result.Fill(ABackColor);
      end else
        result := TBGRABitmap.Create(AWidth,AHeight,ABackColor);
      factorX := result.Width/ABitmap.Width;
      factorY := result.Height/(ABitmap.Height*AVerticalShrink);
      if factorX < factorY then factor := factorX else factor := factorY;
      wIcon := round(ABitmap.Width*factor);
      hIcon := round(ABitmap.Height*AVerticalShrink*factor);
      xIcon:= (result.Width-wIcon) div 2;
      yIcon:= (result.Height-hIcon) div 2;
      if ACheckers then DrawThumbnailCheckers(result,Rect(xIcon,yIcon,xIcon+wIcon,yIcon+hIcon));
      if (ABackColor.alpha <> 0) or ACheckers then
        result.StretchPutImage(Rect(xIcon,yIcon,xIcon+wIcon,yIcon+hIcon),ABitmap,dmDrawWithTransparency) else
        result.StretchPutImage(Rect(xIcon,yIcon,xIcon+wIcon,yIcon+hIcon),ABitmap,dmSet);
    end;
  except
  end;
end;

function GetFileThumbnail(AFilenameUTF8: string; AWidth, AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap): TBGRABitmap;
var stream: TFileStreamUTF8;
begin
  try
    stream := TFileStreamUTF8.Create(AFilenameUTF8,fmOpenRead or fmShareDenyWrite);
  except
    result := nil;
    exit;
  end;
  try
    result := GetStreamThumbnail(stream, AWidth,AHeight,ABackColor,ACheckers,ExtractFileExt(AFilenameUTF8),ADest);
  finally
    stream.free;
  end;
end;

function GetStreamThumbnail(AStream: TStream; AWidth, AHeight: integer;
  ABackColor: TBGRAPixel; ACheckers: boolean; ASuggestedExtensionUTF8: string;
  ADest: TBGRABitmap): TBGRABitmap;
begin
  case DetectFileFormat(AStream,ASuggestedExtensionUTF8) of
    ifJpeg: result := GetJpegThumbnail(AStream, AWidth,AHeight, ABackColor, ACheckers, ADest);
    ifPng: result := GetPngThumbnail(AStream, AWidth,AHeight, ABackColor, ACheckers, ADest);
    ifGif: result := GetGifThumbnail(AStream, AWidth,AHeight, ABackColor, ACheckers, ADest);
    ifBmp: result := GetBmpThumbnail(AStream, AWidth,AHeight, ABackColor, ACheckers, ADest);
    {$IFDEF BGRABITMAP_USE_LCL}
    ifIco: result := GetIcoThumbnail(AStream, AWidth,AHeight, ABackColor, ACheckers, ADest);
    {$ENDIF}
    ifPcx: result := GetPcxThumbnail(AStream, AWidth,AHeight, ABackColor, ACheckers, ADest);
    ifPaintDotNet: result := GetPaintDotNetThumbnail(AStream, AWidth,AHeight, ABackColor, ACheckers, ADest);
    ifLazPaint: result := GetLazPaintThumbnail(AStream, AWidth,AHeight, ABackColor, ACheckers, ADest);
    ifOpenRaster: result := GetOpenRasterThumbnail(AStream, AWidth,AHeight, ABackColor, ACheckers, ADest);
    ifPsd: result := GetPsdThumbnail(AStream, AWidth,AHeight, ABackColor, ACheckers, ADest);
    ifTarga: result := GetTargaThumbnail(AStream, AWidth,AHeight, ABackColor, ACheckers, ADest);
    ifTiff: result := GetTiffThumbnail(AStream, AWidth,AHeight, ABackColor, ACheckers, ADest);
    ifXwd: result := GetXwdThumbnail(AStream, AWidth,AHeight, ABackColor, ACheckers, ADest);
    ifXPixMap: result := GetXPixMapThumbnail(AStream, AWidth,AHeight, ABackColor, ACheckers, ADest);
    ifBmpMioMap: result := GetBmpMioMapThumbnail(AStream, AWidth,AHeight, ABackColor, ACheckers, ADest);
    else
      result := nil;
  end;
end;

function GetStreamThumbnail(AStream: TStream; AReader: TFPCustomImageReader;
  AWidth, AHeight: integer; ABackColor: TBGRAPixel; ACheckers: boolean;
  ADest: TBGRABitmap): TBGRABitmap;
var bmp: TBGRABitmap;
begin
  bmp := TBGRABitmap.Create;
  try
    bmp.LoadFromStream(AStream, AReader);
  except
    FreeAndNil(bmp);
  end;
  if bmp = nil then
    result := nil
  else
  begin
    result := GetBitmapThumbnail(bmp, AWidth, AHeight, ABackColor, ACheckers, ADest);
    bmp.Free;
  end;
end;



function GetOpenRasterThumbnail(AStream: TStream; AWidth, AHeight: integer;
  ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap): TBGRABitmap;
var
  unzip: TUnzipperStreamUtf8;
  png: TMemoryStream;
begin
  result := nil;
  unzip := TUnzipperStreamUtf8.Create;
  try
    unzip.InputStream := AStream;
    png := TMemoryStream.Create;
    try
      if unzip.UnzipFileToStream('Thumbnails\thumbnail.png', png, False) then
      begin
        png.Position:= 0;
        result := GetPngThumbnail(png,AWidth,AHeight,ABackColor,ACheckers,ADest);
      end;
    finally
      png.Free;
    end;
  except
  end;
  unzip.Free;
end;

function GetLazPaintThumbnail(AStream: TStream; AWidth, AHeight: integer;
  ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap): TBGRABitmap;
var
  reader: TBGRAReaderLazPaint;
begin
  reader:= TBGRAReaderLazPaint.Create;
  reader.WantThumbnail := true;
  result := GetStreamThumbnail(AStream,reader,AWidth,AHeight,ABackColor,ACheckers,ADest);
  reader.Free;
end;

function GetJpegThumbnail(AStream: TStream; AWidth, AHeight: integer
  ; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap): TBGRABitmap;
var
  jpeg: TFPReaderJPEG;
begin
  jpeg := TFPReaderJPEG.Create;
  jpeg.Performance := jpBestSpeed;
  jpeg.MinWidth := AWidth;
  jpeg.MinHeight := AHeight;
  result := GetStreamThumbnail(AStream, jpeg, AWidth,AHeight,ABackColor,ACheckers,ADest);
  jpeg.Free;
end;

function GetPsdThumbnail(AStream: TStream; AWidth, AHeight: integer
  ; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap): TBGRABitmap;
var
  psd: TBGRAReaderPSD;
  bmp: TBGRABitmap;
begin
  psd:= TBGRAReaderPSD.Create;
  psd.MinifyHeight:= AHeight;
  bmp := TBGRABitmap.Create;
  try
    bmp.LoadFromStream(AStream, psd);
  except
    FreeAndNil(bmp);
  end;
  if bmp = nil then
    result := nil
  else
  begin
    result := GetBitmapThumbnail(bmp, AWidth, AHeight, ABackColor, ACheckers, ADest, psd.Height/bmp.Height);
    bmp.Free;
  end;
  psd.Free;
end;

function GetPngThumbnail(AStream: TStream; AWidth, AHeight: integer
  ; ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
var
  png: TBGRAReaderPNG;
  bmp: TBGRABitmap;
begin
  png:= TBGRAReaderPNG.Create;
  bmp := TBGRABitmap.Create;
  try
    png.MinifyHeight := AHeight;
    bmp.LoadFromStream(AStream, png);
  except
    FreeAndNil(bmp);
  end;
  if bmp = nil then
    result := nil
  else
  begin
    result := GetBitmapThumbnail(bmp, AWidth, AHeight, ABackColor, ACheckers, ADest, png.OriginalHeight/bmp.Height);
    bmp.Free;
  end;
  png.Free;
end;

function GetPaintDotNetThumbnail(AStream: TStream; AWidth, AHeight: integer;
  ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap): TBGRABitmap;
var
  {%H-}magic: packed array[0..6] of byte;
  xmlHeader: TMemoryStream;
  xmlHeaderSize: longint;
  doc: TXMLDocument;
  custom,thumb,pngNode: TDOMNode;
  png64: TStringStream;
  decode64: TBase64DecodingStream;
begin
  result := nil;
  if AStream.Read({%H-}magic,sizeof(magic)) <> sizeof(magic) then exit;
  if chr(magic[0])+chr(magic[1])+chr(magic[2])+chr(magic[3]) <> 'PDN3' then exit;
  xmlHeaderSize := magic[4] + (magic[5] shl 8) + (magic[6] shl 16);
  if xmlHeaderSize >= 10*1024*1024 then exit;
  xmlHeader:= TMemoryStream.Create;
  try
    if xmlHeader.CopyFrom(AStream,xmlHeaderSize) <> xmlHeaderSize then
    begin
      xmlHeader.Free;
      exit;
    end;
  except
    xmlHeader.Free;
    exit;
  end;
  xmlHeader.Position := 0;
  try
    XMLRead.ReadXMLFile(doc, xmlHeader);
  except
    xmlHeader.Free;
    exit;
  end;
  xmlHeader.Free;
  try
    custom := doc.DocumentElement.FindNode('custom');
    if Assigned(custom) then
    begin
      thumb := custom.FindNode('thumb');
      if Assigned(thumb) then
      begin
        pngNode := thumb.Attributes.GetNamedItem('png');
        if Assigned(pngNode) then
        begin
          png64 := TStringStream.Create(pngNode.NodeValue);
          try
            png64.Position := 0;
            decode64 := TBase64DecodingStream.Create(png64);
            try
              result := GetPngThumbnail(decode64,AWidth,AHeight,ABackColor,ACheckers, ADest);
            finally
              decode64.Free;
            end;
          finally
            png64.free;
          end;
        end;
      end;
    end;
  except
  end;
  doc.Free;
end;

function GetBmpThumbnail(AStream: TStream; AWidth, AHeight: integer;
  ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap): TBGRABitmap;
var
  bmpFormat: TBGRAReaderBMP;
  bmp: TBGRABitmap;
begin
  bmpFormat:= TBGRAReaderBMP.Create;
  bmpFormat.MinifyHeight := AHeight*2;
  bmp := TBGRABitmap.Create;
  try
    bmp.LoadFromStream(AStream, bmpFormat);
  except
    FreeAndNil(bmp);
  end;
  if bmp = nil then
    result := nil
  else
  begin
    if bmp.Height <= 0 then
      result := nil
    else
      result := GetBitmapThumbnail(bmp, AWidth, AHeight, ABackColor, ACheckers, ADest, bmpFormat.OriginalHeight/bmp.Height);
    bmp.Free;
  end;
  bmpFormat.Free;
end;

{$IFDEF BGRABITMAP_USE_LCL}
function GetIcoThumbnail(AStream: TStream; AWidth, AHeight: integer;
  ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap): TBGRABitmap;
var ico: TIcon; i,bestIdx: integer;
    height,width: word; format:TPixelFormat;
    bestHeight,bestWidth: integer; maxFormat: TPixelFormat;
    icoBmp: TBGRABitmap;
begin
  result := nil;
  ico := TIcon.Create;
  try
    ico.LoadFromStream(AStream);
  except
    ico.free;
    exit;
  end;
  bestIdx := -1;
  bestHeight := 0;
  bestWidth := 0;
  maxFormat := pfDevice;
  try
    for i := 0 to ico.Count-1 do
    begin
      ico.GetDescription(i,format,height,width);
      if (bestIdx = -1) or (abs(height-AHeight)+abs(width-AWidth) < abs(bestHeight-AHeight)+abs(bestWidth-AWidth)) or
      ((height = bestHeight) or (width = bestWidth) and (format > maxFormat)) then
      begin
        bestIdx := i;
        bestHeight := height;
        bestWidth := width;
        maxFormat := format;
      end;
    end;
    if (bestIdx = -1) or (bestWidth = 0) or (bestHeight = 0) then result := nil else
    begin
      ico.Current := bestIdx;
      icoBmp := TBGRABitmap.Create(bestWidth,bestHeight);
      icoBmp.Assign(ico);
      result := GetBitmapThumbnail(icoBmp, AWidth, AHeight, ABackColor, ACheckers, ADest);
      icoBmp.Free;
    end;
  except
  end;
  ico.Free;
end;
{$ENDIF}

function GetPcxThumbnail(AStream: TStream; AWidth, AHeight: integer;
  ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap): TBGRABitmap;
var
  reader: TFPCustomImageReader;
begin
  reader:= CreateBGRAImageReader(ifPcx);
  result := GetStreamThumbnail(AStream,reader,AWidth,AHeight,ABackColor,ACheckers,ADest);
  reader.Free;
end;

function GetTargaThumbnail(AStream: TStream; AWidth, AHeight: integer;
  ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap): TBGRABitmap;
var
  reader: TFPCustomImageReader;
begin
  reader:= CreateBGRAImageReader(ifTarga);
  result := GetStreamThumbnail(AStream,reader,AWidth,AHeight,ABackColor,ACheckers, ADest);
  reader.Free;
end;

function GetTiffThumbnail(AStream: TStream; AWidth, AHeight: integer;
  ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap): TBGRABitmap;
var
  reader: TFPCustomImageReader;
begin
  reader:= CreateBGRAImageReader(ifTiff);
  result := GetStreamThumbnail(AStream,reader,AWidth,AHeight,ABackColor,ACheckers,ADest);
  reader.Free;
end;

function GetGifThumbnail(AStream: TStream; AWidth, AHeight: integer;
  ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap): TBGRABitmap;
var
  reader: TFPCustomImageReader;
begin
  reader:= CreateBGRAImageReader(ifGif);
  result := GetStreamThumbnail(AStream,reader,AWidth,AHeight,ABackColor,ACheckers, ADest);
  reader.Free;
end;

function GetXwdThumbnail(AStream: TStream; AWidth, AHeight: integer;
  ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap): TBGRABitmap;
var
  reader: TFPCustomImageReader;
begin
  reader:= CreateBGRAImageReader(ifXwd);
  result := GetStreamThumbnail(AStream,reader,AWidth,AHeight,ABackColor,ACheckers, ADest);
  reader.Free;
end;

function GetXPixMapThumbnail(AStream: TStream; AWidth, AHeight: integer;
  ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap): TBGRABitmap;
var
  reader: TFPCustomImageReader;
begin
  reader:= CreateBGRAImageReader(ifXPixMap);
  result := GetStreamThumbnail(AStream,reader,AWidth,AHeight,ABackColor,ACheckers,ADest);
  reader.Free;
end;

function GetBmpMioMapThumbnail(AStream: TStream; AWidth, AHeight: integer;
  ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap): TBGRABitmap;
var
  reader: TFPCustomImageReader;
begin
  reader:= CreateBGRAImageReader(ifBmpMioMap);
  result := GetStreamThumbnail(AStream,reader,AWidth,AHeight,ABackColor,ACheckers,ADest);
  reader.Free;
end;

end.
