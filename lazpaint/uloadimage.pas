unit ULoadImage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazPaintType, BGRABitmap;

function LoadGifUTF8(AFilename: string): ArrayOfBGRABitmap;
function LoadGifSys(AFilename: string): ArrayOfBGRABitmap;
function LoadIcoUTF8(AFilename: string): ArrayOfBGRABitmap;
function LoadIcoSys(AFilename: string): ArrayOfBGRABitmap;
function LoadFlatImageUTF8(AFilename: string; out AFinalFilename: string; AAppendFrame: string): TBGRABitmap;
function LoadFlatImageSys(AFilename: string; out AFinalFilename: string; AAppendFrame: string): TBGRABitmap;
function LoadFlatLzpUTF8(AFilename: string): TBGRABitmap;
function LoadFlatLzpSys(AFilename: string): TBGRABitmap;
function AbleToLoadUTF8(AFilename: string): boolean;
function AbleToLoadSys(AFilename: string): boolean;
procedure FreeMultiImage(var images: ArrayOfBGRABitmap);

implementation

uses FileUtil, BGRAAnimatedGif, Graphics, UMultiImage, BGRACompressableBitmap;

procedure FreeMultiImage(var images: ArrayOfBGRABitmap);
var i: integer;
begin
  for i := 0 to high(images) do
    images[i].Free;
  images := nil;
end;

function LoadFlatImageUTF8(AFilename: string; out AFinalFilename: string; AAppendFrame: string): TBGRABitmap;
var finalFilenameSys: string;
begin
  result := LoadFlatImageSys(UTF8ToSys(AFilename),finalFilenameSys,UTF8ToSys(AAppendFrame));
  AFinalFilename:= SysToUTF8(finalFilenameSys);
end;

function LoadFlatImageSys(AFilename: string; out AFinalFilename: string; AAppendFrame: string): TBGRABitmap;
var
  ext: string;
  formMultiImage: TFMultiImage;
  multi: ArrayOfBGRABitmap;

  procedure ChooseMulti;
  begin
    if length(multi)=1 then
    begin
      result := multi[0];
      multi := nil;
    end else
    begin
      formMultiImage := TFMultiImage.Create(nil);
      try
        result := formMultiImage.ShowAndChoose(multi);
      finally
        formMultiImage.Free;
      end;
      FreeMultiImage(multi);
      if result <> nil then
        AFinalFilename += '.'+result.Caption+AAppendFrame;
    end;
  end;

begin
  AFinalFilename:= AFilename;
  result := nil;
  ext := LowerCase(ExtractFileExt(AFilename));
  if ext='.ico' then
  begin
    multi := LoadIcoSys(AFilename);
    ChooseMulti;
  end else
  if ext='.gif' then
  begin
    multi := LoadGifSys(AFilename);
    ChooseMulti;
  end else
  if ext='.lzp' then
  begin
    result := LoadFlatLzpSys(AFilename);
  end else
    result := TBGRABitmap.Create(AFilename);
end;

function LoadFlatLzpUTF8(AFilename: string): TBGRABitmap;
begin
  result := LoadFlatLzpSys(UTF8ToSys(AFilename));
end;

function LoadFlatLzpSys(AFilename: string): TBGRABitmap;
var
  comp: TBGRACompressableBitmap;
  stream: TFileStream;
begin
  result := nil;
  comp := TBGRACompressableBitmap.Create;
  try
    stream := TFileStream.Create(AFilename,fmOpenRead);
    try
      comp.ReadFromStream(stream);
      result := comp.GetBitmap;
    finally
      stream.Free;
    end;
  finally
    comp.Free;
  end;
end;

function AbleToLoadUTF8(AFilename: string): boolean;
begin
  result := AbleToLoadSys(UTF8ToSys(AFilename));
end;

function AbleToLoadSys(AFilename: string): boolean;
var ext: string;
begin
  ext := LowerCase(ExtractFileExt(AFilename));
  if (ext='.bmp') or (ext='.jpg') or (ext='.jpeg')
    or (ext='.png') or (ext='.pcx') or
    (ext='.gif') or (ext='.ico') or (ext='.pdn') or
    (ext='.lzp') or (ext='.ora') then
    result := true else
      result := false;
end;

function LoadGifUTF8(AFilename: string): ArrayOfBGRABitmap;
begin
  result := LoadGifSys(UTF8ToSys(AFilename));
end;

function LoadGifSys(AFilename: string): ArrayOfBGRABitmap;
var gif: TBGRAAnimatedGif; i: integer;
begin
   gif := TBGRAAnimatedGif.Create(AFilename);
   setlength(result,gif.Count);
   for i := 0 to gif.Count-1 do
   begin
     gif.CurrentImage:= i;
     result[i] := gif.MemBitmap.Duplicate as TBGRABitmap;
     result[i].Caption:= 'Frame'+IntToStr(i);
   end;
   gif.Free;
end;

function LoadIcoUTF8(AFilename: string): ArrayOfBGRABitmap;
begin
   result := LoadIcoSys(UTF8ToSys(AFilename));
end;

function LoadIcoSys(AFilename: string): ArrayOfBGRABitmap;
var ico: TIcon; i,resIdx,maxIdx: integer;
    height,width: word; format:TPixelFormat;
    maxHeight,maxWidth: word; maxFormat: TPixelFormat;
begin
  ico := TIcon.Create;
  ico.LoadFromFile(AFilename);
  maxIdx := 0;
  maxHeight := 0;
  maxWidth := 0;
  maxFormat := pfDevice;
  for i := 0 to ico.Count-1 do
  begin
    ico.GetDescription(i,format,height,width);
    if (height > maxHeight) or (width > maxWidth) or
    ((height = maxHeight) or (width = maxWidth) and (format > maxFormat)) then
    begin
      maxIdx := i;
      maxHeight := height;
      maxWidth := width;
      maxFormat := format;
    end;
  end;
  if (maxWidth = 0) or (maxHeight = 0) then result := nil else
  begin
    setlength(result,ico.Count);
    ico.Current := maxIdx;
    result[0] := TBGRABitmap.Create(maxWidth,maxHeight);
    result[0].GetImageFromCanvas(ico.Canvas,0,0);
    result[0].Caption := IntTostr(maxWidth)+'x'+IntToStr(maxHeight)+'x'+IntToStr(PIXELFORMAT_BPP[maxFormat]);
    resIdx := 1;
    for i := 0 to ico.Count-1 do
    if i <> maxIdx then
    begin
      ico.Current := i;
      ico.GetDescription(i,format,height,width);
      result[resIdx] := TBGRABitmap.Create(width,height);
      result[resIdx].GetImageFromCanvas(ico.Canvas,0,0);
      result[resIdx].Caption := IntTostr(width)+'x'+IntToStr(height)+'x'+IntToStr(PIXELFORMAT_BPP[format]);
      inc(resIdx);
    end;
  end;
  ico.Free;
end;


end.

