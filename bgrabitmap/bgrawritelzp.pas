unit BGRAWriteLzp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPimage, BGRALzpCommon, BGRABitmapTypes, BGRABitmap;

type
  { TBGRAWriterLazPaint }

  TBGRAWriterLazPaint = class(TFPCustomImageWriter)
  private
    function GetCompression: TLzpCompression;
    function GetIncludeThumbnail: boolean;
    procedure SetCompression(AValue: TLzpCompression);
    procedure SetIncludeThumbnail(AValue: boolean);
    function WriteThumbnail(Str: TStream; Img: TFPCustomImage): boolean;
    protected
      CompressionMode: DWord;
      procedure InternalWrite(Str: TStream; Img: TFPCustomImage); override;
      function InternalWriteLayers({%H-}Str: TStream; {%H-}Img: TFPCustomImage): boolean; virtual;
      function GetNbLayers: integer; virtual;
    public
      Caption: string;
      constructor Create; override;
      class procedure WriteRLEImage(Str: TStream; Img: TFPCustomImage; ACaption: string= '');
      property Compression: TLzpCompression read GetCompression write SetCompression;
      property IncludeThumbnail: boolean read GetIncludeThumbnail write SetIncludeThumbnail;
  end;

implementation

uses BGRACompressableBitmap;

{ TBGRAWriterLazPaint }

function TBGRAWriterLazPaint.WriteThumbnail(Str: TStream; Img: TFPCustomImage): boolean;
var w,h: integer;
  thumbStream: TStream;
  OldResampleFilter: TResampleFilter;
  thumbnail: TBGRACustomBitmap;
begin
  result := false;
  if not (Img is TBGRACustomBitmap) then exit;
  if (Img.Width > LazpaintThumbMaxWidth) or
   (Img.Height > LazpaintThumbMaxHeight) then
  begin
    if Img.Width > LazpaintThumbMaxWidth then
    begin
      w := LazpaintThumbMaxWidth;
      h := round(Img.Height* (w/Img.Width));
    end else
    begin
      w := Img.Width;
      h := Img.Height;
    end;
    if h > LazpaintThumbMaxHeight then
    begin
      h := LazpaintThumbMaxHeight;
      w := round(Img.Width* (h/Img.Height));
    end;
    OldResampleFilter:= TBGRACustomBitmap(Img).ResampleFilter;
    TBGRACustomBitmap(Img).ResampleFilter:= rfMitchell;
    thumbnail := TBGRACustomBitmap(Img).Resample(w,h,rmFineResample);
    TBGRACustomBitmap(Img).ResampleFilter := OldResampleFilter;

    try
      thumbStream := TMemoryStream.Create;
      try
        thumbnail.SaveToStreamAsPng(thumbStream);
        thumbStream.Position:= 0;
        Str.CopyFrom(thumbStream, thumbStream.Size);
        result := true;
      finally
        thumbStream.Free;
      end;
    finally
      thumbnail.Free;
    end;
  end else
  begin
    thumbStream := TMemoryStream.Create;
    try
      TBGRACustomBitmap(Img).SaveToStreamAsPng(thumbStream);
      thumbStream.Position:= 0;
      Str.CopyFrom(thumbStream, thumbStream.Size);
      result := true;
    finally
      thumbStream.Free;
    end;
  end;
end;

function TBGRAWriterLazPaint.GetCompression: TLzpCompression;
begin
  if (CompressionMode and LAZPAINT_COMPRESSION_MASK) = LAZPAINT_COMPRESSION_MODE_ZSTREAM then
    result := lzpZStream
  else
    result := lzpRLE;
end;

function TBGRAWriterLazPaint.GetIncludeThumbnail: boolean;
begin
  result := (CompressionMode and LAZPAINT_THUMBNAIL_PNG) <> 0;
end;

procedure TBGRAWriterLazPaint.SetCompression(AValue: TLzpCompression);
begin
  if AValue = lzpZStream then
    CompressionMode := (CompressionMode and not LAZPAINT_COMPRESSION_MASK) or LAZPAINT_COMPRESSION_MODE_ZSTREAM
  else
    CompressionMode := (CompressionMode and not LAZPAINT_COMPRESSION_MASK) or LAZPAINT_COMPRESSION_MODE_RLE;
end;

procedure TBGRAWriterLazPaint.SetIncludeThumbnail(AValue: boolean);
begin
  if AValue then
    CompressionMode := CompressionMode or LAZPAINT_THUMBNAIL_PNG else
    CompressionMode := CompressionMode and not LAZPAINT_THUMBNAIL_PNG;
end;

procedure TBGRAWriterLazPaint.InternalWrite(Str: TStream; Img: TFPCustomImage);
var {%H-}header: TLazPaintImageHeader;
  compBmp: TBGRACompressableBitmap;
  startPos, endPos: int64;
begin
  startPos := str.Position;
  fillchar({%H-}header,sizeof(header),0);
  header.magic := LAZPAINT_MAGIC_HEADER;
  header.zero1 := 0;
  header.headerSize:= sizeof(header);
  header.width := Img.Width;
  header.height := img.Height;
  header.nbLayers:= GetNbLayers;
  header.previewOffset:= 0;
  header.zero2 := 0;
  header.compressionMode:= CompressionMode;
  header.reserved1:= 0;
  header.layersOffset:= 0;
  LazPaintImageHeader_SwapEndianIfNeeded(header);
  str.WriteBuffer(header,sizeof(header));
  LazPaintImageHeader_SwapEndianIfNeeded(header);

  if IncludeThumbnail then
    if not WriteThumbnail(Str, Img) then
    begin
      IncludeThumbnail := false;
      header.compressionMode:= CompressionMode;
    end;

  header.previewOffset:= Str.Position - startPos;
  if Compression = lzpRLE then
    WriteRLEImage(Str, Img)
  else
  begin
    compBmp := TBGRACompressableBitmap.Create(Img as TBGRABitmap);
    compBmp.WriteToStream(Str);
    compBmp.Free;
  end;

  endPos := str.Position;
  if InternalWriteLayers(Str, Img) then
  begin
    header.layersOffset := endPos - startPos;
    endPos := str.Position;
  end;

  str.Position:= startPos;
  LazPaintImageHeader_SwapEndianIfNeeded(header);
  str.WriteBuffer(header,sizeof(header));
  str.Position:= endPos;
end;

function TBGRAWriterLazPaint.InternalWriteLayers(Str: TStream;
  Img: TFPCustomImage): boolean;
begin
  result := false;
end;

function TBGRAWriterLazPaint.GetNbLayers: integer;
begin
  result := 1;
end;

constructor TBGRAWriterLazPaint.Create;
begin
  inherited Create;
  CompressionMode:= LAZPAINT_COMPRESSION_MODE_RLE;
end;

class procedure TBGRAWriterLazPaint.WriteRLEImage(Str: TStream;
  Img: TFPCustomImage; ACaption: string);
const PossiblePlanes = 4;
var
  PPlane,PPlaneCur: array[0..PossiblePlanes-1] of PByte;
  CompressedPlane: array[0..PossiblePlanes-1] of TMemoryStream;
  NbPixels, NbNonTranspPixels, NbOpaquePixels: integer;
  Colors: array[0..255] of Int32or64;
  ColorCount: Int32or64;
  CompressedRGB: array[0..3] of TMemoryStream;
  ColorTab: packed array[0..256*3-1] of byte;
  Indexed: PByte;
  NonRGBSize,RGBSize: int64;

  procedure OutputPlane(AIndex: integer);
  begin
    str.WriteDWord(NtoLE(DWord(CompressedPlane[AIndex].Size)));
    CompressedPlane[AIndex].Position:= 0;
    str.CopyFrom(CompressedPlane[AIndex],CompressedPlane[AIndex].Size);
  end;

  procedure OutputRGB(AIndex: integer);
  begin
    str.WriteDWord(NtoLE(DWord(CompressedRGB[AIndex].Size)));
    CompressedRGB[AIndex].Position:= 0;
    str.CopyFrom(CompressedRGB[AIndex],CompressedRGB[AIndex].Size);
  end;

  function BuildPalette: boolean;
  var n,i: Int32or64;
    lastColor,color,colorIndex: Int32or64;
    found: boolean;
  begin
    ColorCount := 0;
    ColorIndex := 0;
    lastColor := -1;
    GetMem(Indexed, NbNonTranspPixels);
    for n := 0 to NbNonTranspPixels-1 do
    begin
      color := (PPlane[0]+n)^+ ((PPlane[1]+n)^ shl 8)+ ((PPlane[2]+n)^ shl 16);
      if color = lastColor then
      begin
        (Indexed+n)^ := ColorIndex;
        continue;
      end;
      found := false;
      for i := 0 to ColorCount-1 do
      begin
        if colors[i] = color then
        begin
          found := true;
          ColorIndex := i;
          break;
        end;
      end;
      if not found then
      begin
        inc(ColorCount);
        if ColorCount > 256 then
        begin
          result := false;
          ReAllocMem(Indexed,0);
          exit;
        end;
        colors[colorCount-1] := color;
        ColorIndex := ColorCount-1;
      end;
      (Indexed+n)^ := ColorIndex;
      lastColor := color;
    end;
    result := true;
  end;

var
  i,x,y: integer;
  PlaneFlags: Byte;
  a: NativeInt;

begin
  NbPixels := Img.Width*img.Height;

  for i := 0 to PossiblePlanes-1 do
  begin
    getmem(PPlane[i],NbPixels);
    PPlaneCur[i] := PPlane[i];
    CompressedPlane[i] := nil;
  end;

  NbNonTranspPixels := 0;
  NbOpaquePixels:= 0;
  for y := 0 to img.Height-1 do
    for x := 0 to img.Width-1 do
    begin
      with img.Colors[x,y] do
      begin
        a := alpha shr 8;
        PPlaneCur[3]^ := a;
        inc(PPlaneCur[3]);
        if a = 0 then continue;
        if a = 255 then inc(NbOpaquePixels);

        inc(NbNonTranspPixels);
        PPlaneCur[0]^ := red shr 8;
        PPlaneCur[1]^ := green shr 8;
        PPlaneCur[2]^ := blue shr 8;
        inc(PPlaneCur[0]);
        inc(PPlaneCur[1]);
        inc(PPlaneCur[2]);
      end;
    end;

  PlaneFlags := 0;
  if NbOpaquePixels = NbPixels then PlaneFlags := PlaneFlags or LazpaintChannelNoAlpha;
  if CompareMem(PPlane[1],PPlane[0],NbNonTranspPixels) then PlaneFlags := PlaneFlags or LazpaintChannelGreenFromRed;
  if CompareMem(PPlane[2],PPlane[0],NbNonTranspPixels) then PlaneFlags := PlaneFlags or LazpaintChannelBlueFromRed else
  if CompareMem(PPlane[2],PPlane[1],NbNonTranspPixels) then PlaneFlags := PlaneFlags or LazpaintChannelBlueFromGreen;

  //if we cannot reduce to one plane, maybe we will have more luck with a palette
  for i := 0 to 3 do CompressedRGB[i] := nil;
  Indexed := nil;
  RGBSize := 0;
  if ((PlaneFlags and LazpaintChannelGreenFromRed) = 0) or
     ((PlaneFlags and (LazpaintChannelBlueFromRed or LazpaintChannelBlueFromGreen)) = 0) and (NbNonTranspPixels > 0) then
  begin
    if BuildPalette then
    begin
      if ColorCount shl 1 < NbNonTranspPixels then
      begin
        fillchar({%H-}ColorTab, sizeof(ColorTab), 0);
        for i := 0 to ColorCount-1 do
        begin
          colorTab[i] := Colors[i] and 255;
          colorTab[i+256] := (Colors[i] shr 8) and 255;
          colorTab[i+512] := (Colors[i] shr 16) and 255;
        end;
        CompressedRGB[0] := TMemoryStream.Create;
        EncodeLazRLE(colorTab[0], ColorCount, CompressedRGB[0]);
        if (PlaneFlags and LazpaintChannelGreenFromRed) = 0 then
        begin
          CompressedRGB[1] := TMemoryStream.Create;
          EncodeLazRLE(colorTab[256], ColorCount, CompressedRGB[1]);
        end;
        if (PlaneFlags and (LazpaintChannelBlueFromRed or LazpaintChannelBlueFromGreen)) = 0 then
        begin
          CompressedRGB[2] := TMemoryStream.Create;
          EncodeLazRLE(colorTab[512], ColorCount, CompressedRGB[2]);
        end;
        CompressedRGB[3] := TMemoryStream.Create;
        EncodeLazRLE(Indexed^,NbNonTranspPixels,CompressedRGB[3]);

        for i := 0 to 3 do
          if CompressedRGB[i] <> nil then
            inc(RGBSize,CompressedRGB[i].Size);
      end;
      ReAllocMem(Indexed,0);
    end;
  end;

  if (PlaneFlags and LazpaintChannelGreenFromRed) <> 0 then ReAllocMem(PPlane[1],0);
  if (PlaneFlags and (LazpaintChannelBlueFromRed or LazpaintChannelBlueFromGreen)) <> 0 then ReAllocMem(PPlane[2],0);

  NonRGBSize := 0;
  for i := 0 to PossiblePlanes-1 do
    if PPlane[i] <> nil then
    begin
      CompressedPlane[i] := TMemoryStream.Create;
      if i = 3 then
        EncodeLazRLE(PPlane[i]^, NbPixels,CompressedPlane[i])
      else
        EncodeLazRLE(PPlane[i]^, NbNonTranspPixels,CompressedPlane[i]);
      inc(NonRGBSize, CompressedPlane[i].Size);
    end;

  if (CompressedRGB[3] <> nil) and (NonRGBSize > RGBSize) then
    PlaneFlags:= PlaneFlags or LazpaintPalettedRGB;

  str.WriteDWord(NtoLE(DWord(img.width)));
  str.WriteDWord(NtoLE(DWord(img.Height)));
  str.WriteDWord(NtoLE(DWord(length(ACaption))));
  if length(ACaption)>0 then str.WriteBuffer(ACaption[1],length(ACaption));
  str.WriteByte(PlaneFlags);

  if (PlaneFlags and LazpaintChannelNoAlpha) = 0 then OutputPlane(3);
  if (PlaneFlags and LazpaintPalettedRGB) <> 0 then
  begin
    for i := 0 to 3 do
      if CompressedRGB[i] <> nil then
        OutputRGB(i);
  end else
  begin
    OutputPlane(0);
    if (PlaneFlags and LazpaintChannelGreenFromRed) = 0 then OutputPlane(1);
    if (PlaneFlags and (LazpaintChannelBlueFromRed or LazpaintChannelBlueFromGreen)) = 0 then OutputPlane(2);
  end;

  for i := 0 to PossiblePlanes-1 do
  begin
    freemem(PPlane[i]);
    CompressedPlane[i].Free;
  end;
  for i := 0 to 3 do
    CompressedRGB[i].Free;
end;

initialization

  DefaultBGRAImageWriter[ifLazPaint] := TBGRAWriterLazPaint;

end.
