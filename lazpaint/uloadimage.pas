// SPDX-License-Identifier: GPL-3.0-only
unit ULoadImage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazPaintType, BGRABitmap, BGRALayers, BGRASVGOriginal;

function LoadFlatImageUTF8(AFilename: string; AEntryToLoad: integer = -1): TImageEntry;
procedure FreeMultiImage(var images: ArrayOfImageEntry);
function AbleToLoadUTF8(AFilename: string): boolean;
function LoadSVGOriginalUTF8(AFilename: string; AContainerWidth, AContainerHeight: integer;
  AScaleDPI: single): TBGRALayerSVGOriginal;

implementation

uses FileUtil, BGRAAnimatedGif, Graphics, UMultiImage,
  BGRAReadLzp, LCLProc, BGRABitmapTypes, BGRAReadPng,
  UFileSystem, BGRAIconCursor, BGRAReadTiff,
  Dialogs, URaw;

function LoadIcoEntryFromStream(AStream: TStream; AIndex: integer): TImageEntry;
var ico: TBGRAIconCursor;
begin
  if AIndex < 0 then raise exception.Create('Index out of bounds');
  result := TImageEntry.Empty;
  ico := TBGRAIconCursor.Create;
  try
    ico.LoadFromStream(AStream);
    result.bmp := ico.GetBitmap(AIndex) as TBGRABitmap;
    result.bmp.Caption := IntTostr(ico.Width[AIndex])+'x'+IntToStr(ico.Height[AIndex])+' '+IntToStr(ico.BitDepth[AIndex])+'bit';
    if Assigned(result.bmp.XorMask) then result.bmp.XorMask.Caption := result.bmp.Caption + ' (xor)';
    result.bpp := ico.BitDepth[AIndex];
    result.frameIndex:= AIndex;
    result.frameCount:= ico.Count;
  finally
    ico.Free;
  end;
end;

function LoadIcoMultiImageFromStream(AStream: TStream): ArrayOfImageEntry;
var ico: TBGRAIconCursor; i: integer;
begin
  ico := TBGRAIconCursor.Create;
  try
    ico.LoadFromStream(AStream);
    result := nil;
    setlength(result,ico.Count);
    for i := 0 to ico.Count-1 do
    begin
      result[i].bmp := ico.GetBitmap(i) as TBGRABitmap;
      result[i].bmp.Caption := IntTostr(ico.Width[i])+'x'+IntToStr(ico.Height[i])+' '+IntToStr(ico.BitDepth[i])+'bit';
      if Assigned(result[i].bmp.XorMask) then result[i].bmp.XorMask.Caption := result[i].bmp.Caption + ' (xor)';
      result[i].bpp := ico.BitDepth[i];
      result[i].frameIndex:= i;
      result[i].frameCount:= ico.Count;
    end;
  finally
    ico.Free;
  end;
end;

function LoadGifEntryFromStream(AStream: TStream; AIndex: integer): TImageEntry;
var gif: TBGRAAnimatedGif;
begin
  if AIndex < 0 then raise exception.Create('Index out of bounds');
  result := TImageEntry.Empty;
  gif := TBGRAAnimatedGif.Create(AStream);
  try
    gif.CurrentImage:= AIndex;
    result.bmp := gif.MemBitmap.Duplicate as TBGRABitmap;
    result.bmp.Caption := 'Frame'+IntToStr(AIndex+1);
    result.frameIndex := AIndex;
    result.frameCount := gif.Count;
  finally
    gif.Free;
  end;

end;

function LoadGifMultiImageFromStream(AStream: TStream): ArrayOfImageEntry;
var gif: TBGRAAnimatedGif; i: integer;
begin
  gif := TBGRAAnimatedGif.Create(AStream);
  try
    result := nil;
    setlength(result,gif.Count);
    for i := 0 to gif.Count-1 do
    begin
      gif.CurrentImage:= i;
      result[i].bmp := gif.MemBitmap.Duplicate as TBGRABitmap;
      result[i].bmp.Caption := 'Frame'+IntToStr(i+1);
      result[i].bpp := 0;
      result[i].frameIndex := i;
      result[i].frameCount := gif.Count;
    end;
  finally
    gif.Free;
  end;
end;

function LoadTiffEntryFromStream(AStream: TStream; AIndex: integer): TImageEntry;
var tiff: TBGRAReaderTiff;
begin
  if AIndex < 0 then raise exception.Create('Index out of bounds');
  result := TImageEntry.Empty;
  tiff := TBGRAReaderTiff.Create;
  try
    tiff.LoadFromStream(AStream);
    result.bmp := (tiff.Images[AIndex].Img as TBGRABitmap).Duplicate as TBGRABitmap;
    result.bmp.Caption := 'Image'+IntToStr(AIndex+1);
    result.frameIndex:= AIndex;
    result.frameCount:= tiff.ImageCount;
  finally
    tiff.Free;
  end;
end;

function LoadTiffMultiImageFromStream(AStream: TStream): ArrayOfImageEntry;
var tiff: TBGRAReaderTiff;
  i: Integer;
begin
  tiff := TBGRAReaderTiff.Create;
  try
    tiff.LoadFromStream(AStream);
    result := nil;
    setlength(result,tiff.ImageCount);
    for i := 0 to tiff.ImageCount-1 do
    begin
      result[i].bmp := (tiff.Images[i].Img as TBGRABitmap).Duplicate as TBGRABitmap;
      result[i].bmp.Caption := 'Image'+IntToStr(i+1);
      result[i].bpp := 0;
      result[i].frameIndex:= i;
      result[i].frameCount:= tiff.ImageCount;
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
  if IsRawFilename(AFilename) then exit(true);
  s := FileManager.CreateFileStream(AFilename, fmOpenRead or fmShareDenyWrite);
  try
    result := DefaultBGRAImageReader[DetectFileFormat(s, ExtractFileExt(AFilename))] <> nil;
  finally
    s.Free;
  end;
end;

function LoadSVGOriginalUTF8(AFilename: string; AContainerWidth, AContainerHeight: integer;
  AScaleDPI: single): TBGRALayerSVGOriginal;
var
  svg: TBGRALayerSVGOriginal;
  s: TStream;
begin
  s := FileManager.CreateFileStream(AFilename, fmOpenRead or fmShareDenyWrite);
  result := nil;
  try
    svg := TBGRALayerSVGOriginal.Create;
    svg.LoadSVGFromStream(s, AContainerWidth, AContainerHeight, AScaleDPI);
    result:= svg;
    svg:= nil;
  finally
    s.Free;
    svg.Free;
  end;
end;

function LoadFlatImageUTF8(AFilename: string; AEntryToLoad: integer): TImageEntry;
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
    end;
  end;

begin
  result := TImageEntry.Empty;

  s := FileManager.CreateFileStream(AFilename, fmOpenRead or fmShareDenyWrite);
  try
    format := DetectFileFormat(s, ExtractFileExt(AFilename));
    if IsRawFilename(AFilename) then
    begin
      result.bmp := GetRawStreamImage(s);
      result.bpp:= 0;
    end else
    if format in[ifIco,ifCur] then
    begin
      if AEntryToLoad <> -1 then
        result := LoadIcoEntryFromStream(s, AEntryToLoad)
      else
      begin
        multi := LoadIcoMultiImageFromStream(s);
        ChooseMulti(False);
      end;
    end else
    if format = ifGif then
    begin
      if AEntryToLoad <> -1 then
        result := LoadGifEntryFromStream(s, AEntryToLoad)
      else
      begin
        multi := LoadGifMultiImageFromStream(s);
        ChooseMulti(True);
      end;
    end else
    if format = ifTiff then
    begin
      if AEntryToLoad <> -1 then
        result := LoadTiffEntryFromStream(s, AEntryToLoad)
      else
      begin
        multi := LoadTiffMultiImageFromStream(s);
        ChooseMulti(True);
      end;
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

