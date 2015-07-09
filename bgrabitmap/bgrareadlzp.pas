unit BGRAReadLzp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPimage, BGRALzpCommon, BGRABitmapTypes, BGRABitmap;

type

  { TBGRAReaderLazPaint }

  TBGRAReaderLazPaint = class(TFPCustomImageReader)
  private
    FHeight: integer;
    FNbLayers: integer;
    FWidth: integer;
    FCaption: string;
    FDimensionsAlreadyFetched: boolean;
  protected
    procedure InternalRead(Str: TStream; Img: TFPCustomImage); override;
    procedure InternalReadLayers({%H-}str: TStream;{%H-}Img: TFPCustomImage); virtual;
    procedure InternalReadCompressableBitmap(str: TStream; Img: TFPCustomImage); virtual;
    function InternalCheck(Str: TStream): boolean; override;
  public
    WantThumbnail: boolean;
    class procedure LoadRLEImage(Str: TStream; Img: TFPCustomImage; out ACaption: string);
    property Width: integer read FWidth;
    property Height: integer read FHeight;
    property NbLayers: integer read FNbLayers;
    property Caption: string read FCaption;
  end;

implementation

uses BGRACompressableBitmap, BGRAReadPng;

{ TBGRAReaderLazPaint }

procedure TBGRAReaderLazPaint.InternalRead(Str: TStream; Img: TFPCustomImage);
var
  {%H-}header: TLazPaintImageHeader;
  oldPos: int64;
  png: TBGRAReaderPNG;

begin
  FCaption := '';
  FWidth:= 0;
  FHeight:= 0;
  FNbLayers:= 0;
  FDimensionsAlreadyFetched:= false;
  oldPos := str.Position;
  str.ReadBuffer({%H-}header.magic,sizeof(header.magic));
  if header.magic = LAZPAINT_MAGIC_HEADER then
  begin
    str.ReadBuffer(header.zero1, sizeof(header)-sizeof(header.magic));
    LazPaintImageHeader_SwapEndianIfNeeded(header);
    if (header.zero1 <> 0) or (header.zero2 <> 0) or
       (header.headerSize < $30) then raise exception.Create('Invalid file format');
    FWidth:= header.width;
    FHeight:= header.height;
    FNbLayers:= header.nbLayers;
    FDimensionsAlreadyFetched:= true;

    if WantThumbnail and ((header.compressionMode and LAZPAINT_THUMBNAIL_PNG) <> 0) then
      begin
        str.Position:= oldPos+header.headerSize;
        png := TBGRAReaderPNG.create;
        try
          png.ImageRead(Str,Img);
        except
          png.Free;
          raise exception.Create('Invalid file format');
        end;
        png.free;
        exit;
      end;

    if ((header.compressionMode and LAZPAINT_COMPRESSION_MASK) <> LAZPAINT_COMPRESSION_MODE_ZSTREAM) and
      ((header.compressionMode and LAZPAINT_COMPRESSION_MASK) <> LAZPAINT_COMPRESSION_MODE_RLE)  then raise exception.Create('Compression mode not supported');

    str.Position:= oldPos+header.previewOffset;
    if (header.compressionMode and LAZPAINT_COMPRESSION_MASK) = LAZPAINT_COMPRESSION_MODE_RLE then
      LoadRLEImage(Str, Img, FCaption)
    else
      InternalReadCompressableBitmap(str,Img);

    if header.layersOffset > 0 then
    begin
      Str.Position:= oldPos+header.layersOffset;
      InternalReadLayers(Str,Img);
    end;
  end else
  begin
    str.Position:= oldPos;
    InternalReadCompressableBitmap(str,Img);
    if Str.Position < Str.Size then InternalReadLayers(Str,Img);
  end;
end;

procedure TBGRAReaderLazPaint.InternalReadLayers(str: TStream;
  Img: TFPCustomImage);
begin
  //not implemented here
end;

procedure TBGRAReaderLazPaint.InternalReadCompressableBitmap(str: TStream;
  Img: TFPCustomImage);
var
  compressed: TBGRACompressableBitmap;
  bmp: TBGRABitmap;
begin
  compressed := TBGRACompressableBitmap.Create;
  try
    compressed.ReadFromStream(Str);
    bmp := compressed.GetBitmap;
    try
      FCaption := compressed.Caption;
      if (Img is TBGRACustomBitmap) then
        TBGRACustomBitmap(Img).Assign(bmp)
      else
        Img.Assign(bmp);
      if not FDimensionsAlreadyFetched then
        begin
          FDimensionsAlreadyFetched := true;
          FWidth:= bmp.width;
          FHeight:= bmp.height;
          FNbLayers:= 1;
        end;
    finally
      bmp.Free;
    end;
  finally
    compressed.Free;
  end;
end;

function TBGRAReaderLazPaint.InternalCheck(Str: TStream): boolean;
var {%H-}magic: packed array[0..7] of byte;
    magicAsText: string;
    oldPos: int64;
begin
  oldPos := str.Position;
  result := (str.Read({%H-}magic,sizeof(magic)) = sizeof(magic));
  str.Position:= oldPos;
  setlength(magicAsText, sizeof(magic));
  move(magic[0], magicAsText[1], sizeof(magic));
  result := (copy(magicAsText,1,8) = 'LazPaint') or
    (((magic[0] <> 0) or (magic[1] <> 0)) and (magic[2] = 0) and (magic[3] = 0) and
     ((magic[4] <> 0) or (magic[5] <> 0)) and (magic[6] = 0) and (magic[7] = 0));
end;

class procedure TBGRAReaderLazPaint.LoadRLEImage(Str: TStream; Img: TFPCustomImage; out ACaption: string);
var channelFlags: byte;
    w,h,NbPixels,nameLen,channelStreamSize: DWord;
    nextPosition: int64;
    PIndexed,PRed,PGreen,PBlue,PAlpha,
    PCurRed, PCurGreen, PCurBlue, PCurAlpha: PByte;
    PDest: PBGRAPixel;
    x,y: DWord;
    c: TFPColor;
    n,NbNonTransp: DWord;
    a,index: NativeInt;
    ColorTab: packed array[0..256*3-1] of byte;
begin
  w := LEtoN(str.ReadDWord);
  h := LEtoN(str.ReadDWord);
  nameLen := LEtoN(str.ReadDWord);
  setlength(ACaption, nameLen);
  str.ReadBuffer(ACaption[1], nameLen);
  channelFlags := str.ReadByte;
  NbPixels := w*h;

  PRed := nil;
  PGreen := nil;
  PBlue := nil;
  PAlpha := nil;

  try
    if (channelFlags and LazpaintChannelNoAlpha) = 0 then
      begin
        Getmem(PAlpha, NbPixels);
        channelStreamSize := LEtoN(str.ReadDWord);
        nextPosition:= str.Position+channelStreamSize;
        if (channelStreamSize > 0) and (NbPixels > 0) then DecodeLazRLE(Str, PAlpha^, NbPixels);
        Str.Position:= nextPosition;

        NbNonTransp := 0;
        PCurAlpha := PAlpha;
        for n := NbPixels-1 downto 0 do
        begin
          if PCurAlpha^ <> 0 then inc(NbNonTransp);
          inc(PCurAlpha);
        end;
      end else
        NbNonTransp:= NbPixels;

    if NbNonTransp > 0 then
    begin
      if (channelFlags and LazpaintPalettedRGB) <> 0 then
      begin
        Getmem(PIndexed, NbNonTransp);
        try
          Getmem(PRed, NbNonTransp);
          Getmem(PGreen, NbNonTransp);
          Getmem(PBlue, NbNonTransp);
          fillchar({%H-}ColorTab,sizeof(ColorTab),0);

          channelStreamSize := LEtoN(str.ReadDWord);
          nextPosition:= str.Position+channelStreamSize;
          DecodeLazRLE(Str, colorTab[0], 256);
          Str.Position:= nextPosition;

          if (channelFlags and LazPaintChannelGreenFromRed) <> 0 then
            move(ColorTab[0],colorTab[256], 256)
          else
          begin
            channelStreamSize := LEtoN(str.ReadDWord);
            nextPosition:= str.Position+channelStreamSize;
            DecodeLazRLE(Str, colorTab[256], 256);
            Str.Position:= nextPosition;
          end;
          if (channelFlags and LazPaintChannelBlueFromRed) <> 0 then
            move(ColorTab[0],colorTab[512], 256)
          else if (channelFlags and LazpaintChannelBlueFromGreen) <> 0 then
            move(ColorTab[256],colorTab[512], 256)
          else
          begin
            channelStreamSize := LEtoN(str.ReadDWord);
            nextPosition:= str.Position+channelStreamSize;
            DecodeLazRLE(Str, colorTab[512], 256);
            Str.Position:= nextPosition;
          end;

          channelStreamSize := LEtoN(str.ReadDWord);
          nextPosition:= str.Position+channelStreamSize;
          DecodeLazRLE(Str, PIndexed^, NbNonTransp);
          Str.Position:= nextPosition;

          for n := 0 to NbNonTransp-1 do
          begin
            index := (PIndexed+n)^;
            (PRed+n)^ := colorTab[index];
            (PGreen+n)^ := colorTab[index+256];
            (PBlue+n)^ := colorTab[index+512];
          end;
        finally
          FreeMem(PIndexed);
        end;
      end else
      begin
        Getmem(PRed, NbNonTransp);
        channelStreamSize := LEtoN(str.ReadDWord);
        nextPosition:= str.Position+channelStreamSize;
        DecodeLazRLE(Str, PRed^, NbNonTransp);
        Str.Position:= nextPosition;

        if (channelFlags and LazPaintChannelGreenFromRed) <> 0 then PGreen := PRed else
        begin
          Getmem(PGreen, NbNonTransp);
          channelStreamSize := LEtoN(str.ReadDWord);
          nextPosition:= str.Position+channelStreamSize;
          DecodeLazRLE(Str, PGreen^, NbNonTransp);
          Str.Position:= nextPosition;
        end;

        if (channelFlags and LazPaintChannelBlueFromRed) <> 0 then PBlue := PRed else
        if (channelFlags and LazPaintChannelBlueFromGreen) <> 0 then PBlue := PGreen else
        begin
          Getmem(PBlue, NbNonTransp);
          channelStreamSize := LEtoN(str.ReadDWord);
          nextPosition:= str.Position+channelStreamSize;
          DecodeLazRLE(Str, PBlue^, NbNonTransp);
          Str.Position:= nextPosition;
        end;
      end;
    end;

    Img.SetSize(w,h);

    if NbNonTransp > 0 then
    begin
      PCurRed := PRed;
      PCurGreen := PGreen;
      PCurBlue := PBlue;
      PCurAlpha := PAlpha;

      if Img is TBGRACustomBitmap then
        begin
          If PCurAlpha = nil then
            begin
              for y := 0 to h-1 do
              begin
                PDest := TBGRACustomBitmap(Img).ScanLine[y];
                for x := w-1 downto 0 do
                begin
                  PDest^ := BGRA(PCurRed^,PCurGreen^,PCurBlue^);
                  inc(PCurBlue);
                  inc(PCurGreen);
                  inc(PCurRed);
                  inc(PDest);
                end;
              end;
            end else
          for y := 0 to h-1 do
          begin
            PDest := TBGRACustomBitmap(Img).ScanLine[y];
            for x := w-1 downto 0 do
            begin
              if PCurAlpha^ = 0 then
                PDest^ := BGRAPixelTransparent
              else
              begin
                PDest^ := BGRA(PCurRed^,PCurGreen^,PCurBlue^,PCurAlpha^);
                inc(PCurBlue);
                inc(PCurGreen);
                inc(PCurRed);
              end;
              inc(PDest);
              inc(PCurAlpha);
            end;
          end;
        end else
        begin
          a := 255;
          for y := 0 to h-1 do
            for x := 0 to w-1 do
            begin
              if PCurAlpha <> nil then
              begin
                a := PCurAlpha^;
                inc(PCurAlpha);
              end;
              if a = 0 then
              begin
                img.Colors[x,y] := colTransparent;
              end else
              begin
                c.red := PCurRed^ + (PCurRed^ shl 8);
                c.green := PCurGreen^ + (PCurGreen^ shl 8);
                c.blue := PCurBlue^ + (PCurBlue^ shl 8);
                c.alpha := a + (a shl 8);
                Img.Colors[x,y] := c;
                inc(PCurBlue);
                inc(PCurGreen);
                inc(PCurRed);
              end;
            end;
        end;
    end else
    begin
      if Img is TBGRACustomBitmap then
        TBGRACustomBitmap(Img).FillTransparent else
      begin
        for y := 0 to h-1 do
          for x := 0 to w-1 do
            img.Colors[x,y] := colTransparent;
      end;
    end;
  finally
    If Assigned(PAlpha) then FreeMem(PAlpha);
    if Assigned(PBlue) and (PBlue <> PGreen) and (PBlue <> PRed) then FreeMem(PBlue);
    if Assigned(PGreen) and (PGreen <> PRed) then FreeMem(PGreen);
    If Assigned(PRed) then FreeMem(PRed);
  end;
end;

initialization

  if DefaultBGRAImageReader[ifLazPaint] = nil then
    DefaultBGRAImageReader[ifLazPaint] := TBGRAReaderLazPaint;

end.
