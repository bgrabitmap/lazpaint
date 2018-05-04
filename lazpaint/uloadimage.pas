unit ULoadImage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazPaintType, BGRABitmap;

function LoadFlatImageUTF8(AFilename: string; out AFinalFilename: string;
   AAppendFrame: string; ASkipDialog: boolean = false): TImageEntry;
procedure FreeMultiImage(var images: ArrayOfImageEntry);
function AbleToLoadUTF8(AFilename: string): boolean;

implementation

uses FileUtil, BGRAAnimatedGif, Graphics, UMultiImage,
  BGRAReadLzp, LCLProc, BGRABitmapTypes, BGRAReadPng,
  UFileSystem, BGRAIconCursor, BGRAReadTiff,
  Dialogs, UResourceStrings;

function LoadIcoMultiImageFromStream(AStream: TStream): ArrayOfImageEntry;
var ico: TBGRAIconCursor; i: integer;
begin
  ico := TBGRAIconCursor.Create;
  ico.LoadFromStream(AStream);
  setlength(result,ico.Count);
  for i := 0 to ico.Count-1 do
  begin
    result[i].bmp := ico.GetBitmap(i) as TBGRABitmap;
    result[i].bmp.Caption := IntTostr(ico.Width[i])+'x'+IntToStr(ico.Height[i])+' '+IntToStr(ico.BitDepth[i])+'bit';
    if Assigned(result[i].bmp.XorMask) then result[i].bmp.XorMask.Caption := result[i].bmp.Caption + ' (xor)';
    result[i].bpp := ico.BitDepth[i];
  end;
  ico.Free;
end;

function LoadGifMultiImageFromStream(AStream: TStream): ArrayOfImageEntry;
var gif: TBGRAAnimatedGif; i: integer;
begin
  gif := TBGRAAnimatedGif.Create(AStream);
  try
    setlength(result,gif.Count);
    for i := 0 to gif.Count-1 do
    begin
      gif.CurrentImage:= i;
      result[i].bmp := gif.MemBitmap.Duplicate as TBGRABitmap;
      result[i].bmp.Caption := 'Frame'+IntToStr(i);
      result[i].bpp := 0;
    end;
  finally
    gif.Free;
  end;
end;

function LoadTiffMultiImageFromStream(AStream: TStream): ArrayOfImageEntry;
var tiff: TBGRAReaderTiff;
  i: Integer;
begin
  tiff := TBGRAReaderTiff.Create;
  try
    tiff.LoadFromStream(AStream);
    setlength(result,tiff.ImageCount);
    for i := 0 to tiff.ImageCount-1 do
    begin
      result[i].bmp := (tiff.Images[i].Img as TBGRABitmap).Duplicate as TBGRABitmap;
      result[i].bmp.Caption := 'Image'+IntToStr(i);
      result[i].bpp := 0;
    end;
  finally
    tiff.Free;
  end;
end;

function LoadFlatLzpFromStream(AStream: TStream): TBGRABitmap;
var
  reader: TBGRAReaderLazPaint;
begin
  reader := TBGRAReaderLazPaint.Create;
  result := TBGRABitmap.Create;
  try
    result.LoadFromStream(AStream, reader);
  finally
    reader.Free;
    if (result.Width = 0) or (result.Height = 0) then FreeAndNil(result);
  end;
end;

function LoadPngFromStream(AStream: TStream): TBGRABitmap;
var
  reader: TBGRAReaderPNG;
begin
  reader := TBGRAReaderPNG.Create;
  result := TBGRABitmap.Create;
  try
    result.LoadFromStream(AStream, reader);
  except
    FreeAndNil(result);
  end;
  if result <> nil then
  begin
    if (result.Width = 0) or (result.Height = 0) then FreeAndNil(result);
  end;
  reader.Free;
end;

procedure FreeMultiImage(var images: ArrayOfImageEntry);
var i: integer;
begin
  for i := 0 to high(images) do
    images[i].bmp.Free;
  images := nil;
end;

function AbleToLoadUTF8(AFilename: string): boolean;
var
  s: TStream;
begin
  s := FileManager.CreateFileStream(AFilename, fmOpenRead or fmShareDenyWrite);
  try
    result := DefaultBGRAImageReader[DetectFileFormat(s, ExtractFileExt(AFilename))] <> nil;
  finally
    s.Free;
  end;
end;

function LoadFlatImageUTF8(AFilename: string; out AFinalFilename: string;
     AAppendFrame: string; ASkipDialog: boolean): TImageEntry;
var
  formMultiImage: TFMultiImage;
  multi: ArrayOfImageEntry;
  format : TBGRAImageFormat;
  s: TStream;

  procedure ChooseMulti(AStretch: boolean);
  begin
    if length(multi)=1 then
    begin
      result := multi[0];
      multi := nil;
    end else
    begin
      formMultiImage := TFMultiImage.Create(nil);
      try
        result := formMultiImage.ShowAndChoose(multi,AStretch, format);
      finally
        formMultiImage.Free;
      end;
      FreeMultiImage(multi);
      if (result.bmp <> nil) and (format = ifGif) then
        AFinalFilename += '.'+result.bmp.Caption+AAppendFrame;
    end;
  end;

begin
  result := TImageEntry.Empty;

  s := FileManager.CreateFileStream(AFilename, fmOpenRead or fmShareDenyWrite);
  try
    format := DetectFileFormat(s, ExtractFileExt(AFilename));
    AFinalFilename:= AFilename;
    if format in[ifIco,ifCur] then
    begin
      multi := LoadIcoMultiImageFromStream(s);
      if ASkipDialog then
      begin
        result := multi[0];
        multi[0] := TImageEntry.Empty;
        FreeMultiImage(multi);
      end
      else
        ChooseMulti(False);
    end else
    if (format = ifGif) and not ASkipDialog then
    begin
      multi := LoadGifMultiImageFromStream(s);
      ChooseMulti(True);
    end else
    if (format = ifTiff) and not ASkipDialog then
    begin
      multi := LoadTiffMultiImageFromStream(s);
      ChooseMulti(True);
    end else
    if format = ifLazPaint then
    begin
      result.bmp := LoadFlatLzpFromStream(s);
      result.bpp := 32; //always 32-bit
    end else
    if format = ifPng then
    begin
      result.bmp := LoadPngFromStream(s);
      result.bpp := 0;
    end else
    begin
      result.bmp := TBGRABitmap.Create(s);
      result.bpp := 0;
    end;
  finally
    s.Free;
  end;
end;

end.

