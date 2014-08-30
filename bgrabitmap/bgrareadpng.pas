{
    The original file before tweaking is:

    $Id: fpreadpng.pp,v 1.10 2003/10/19 21:09:51 luk Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    PNG reader implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

  Optimisations applied:
  - using "const" parameter for TColorData
  - direct pixel access with TBGRABitmap when possible
  - some fixes of hints and of initializations
  - vertical shrink option with MinifyHeight, OriginalHeight and VerticalShrinkFactor (useful for thumbnails)
 }
{$mode objfpc}{$h+}
unit BGRAReadPng;

interface

uses
  SysUtils,Classes, FPImage, FPImgCmn, PNGComn, ZStream, BGRABitmapTypes;

Type

  TSetPixelProc = procedure (x,y:integer; const CD : TColordata) of object;
  TConvertColorProc = function (const CD:TColorData) : TFPColor of object;
  TBGRAConvertColorProc = function (const CD:TColorData) : TBGRAPixel of object;
  THandleScanLineProc = procedure (const y : integer; const ScanLine : PByteArray) of object;

  { TBGRAReaderPNG }

  TBGRAReaderPNG = class (TFPCustomImageReader)
    private

      FHeader : THeaderChunk;
      ZData : TMemoryStream;  // holds compressed data until all blocks are read
      Decompress : TDeCompressionStream; // decompresses the data
      FPltte : boolean;     // if palette is used
      FCountScanlines : EightLong; //Number of scanlines to process for each pass
      FScanLineLength : EightLong; //Length of scanline for each pass
      FCurrentPass : byte;
      ByteWidth : byte;          // number of bytes to read for pixel information
      BitsUsed : EightLong; // bitmasks to use to split a byte into smaller parts
      BitShift : byte;  // shift right to do of the bits extracted with BitsUsed for 1 element
      CountBitsUsed : byte;  // number of bit groups (1 pixel) per byte (when bytewidth = 1)
      //CFmt : TColorFormat; // format of the colors to convert from
      StartX,StartY, DeltaX,DeltaY, StartPass,EndPass : integer;  // number and format of passes
      FSwitchLine, FCurrentLine, FPreviousLine : pByteArray;
      FPalette : TFPPalette;
      FSetPixel : TSetPixelProc;
      FConvertColor : TConvertColorProc;
      FBGRAConvertColor : TBGRAConvertColorProc;
      FHandleScanLine: THandleScanLineProc;
      FVerticalShrinkMask: DWord;
      FVerticalShrinkShr: Integer;
      function GetOriginalHeight: integer;
      function GetOriginalWidth: integer;
      function GetVerticalShrinkFactor: integer;
      procedure ReadChunk;
      procedure HandleData;
      procedure HandleUnknown;
      function ColorGray1 (const CD:TColorData) : TFPColor;
      function ColorGray2 (const CD:TColorData) : TFPColor;
      function ColorGray4 (const CD:TColorData) : TFPColor;
      function ColorGray8 (const CD:TColorData) : TFPColor;
      function ColorGray16 (const CD:TColorData) : TFPColor;
      function ColorGrayAlpha8 (const CD:TColorData) : TFPColor;
      function ColorGrayAlpha16 (const CD:TColorData) : TFPColor;
      function ColorColor8 (const CD:TColorData) : TFPColor;
      function ColorColor16 (const CD:TColorData) : TFPColor;
      function ColorColorAlpha8 (const CD:TColorData) : TFPColor;
      function ColorColorAlpha16 (const CD:TColorData) : TFPColor;

      function BGRAColorGray1 (const CD:TColorData) : TBGRAPixel;
      function BGRAColorGray2 (const CD:TColorData) : TBGRAPixel;
      function BGRAColorGray4 (const CD:TColorData) : TBGRAPixel;
      function BGRAColorGray8 (const CD:TColorData) : TBGRAPixel;
      function BGRAColorGray16 (const CD:TColorData) : TBGRAPixel;
      function BGRAColorGrayAlpha8 (const CD:TColorData) : TBGRAPixel;
      function BGRAColorGrayAlpha16 (const CD:TColorData) : TBGRAPixel;
      function BGRAColorColor8 (const CD:TColorData) : TBGRAPixel;
      function BGRAColorColor16 (const CD:TColorData) : TBGRAPixel;
      function BGRAColorColorAlpha8 (const CD:TColorData) : TBGRAPixel;
      function BGRAColorColorAlpha16 (const CD:TColorData) : TBGRAPixel;
    protected
      Chunk : TChunk;
      UseTransparent, EndOfFile : boolean;
      TransparentDataValue : TColorData;
      UsingBitGroup : byte;
      DataIndex : longword;
      DataBytes : TColorData;
      function CurrentLine(x:longword) : byte;
      function PrevSample (x:longword): byte;
      function PreviousLine (x:longword) : byte;
      function PrevLinePrevSample (x:longword): byte;
      procedure HandleChunk; virtual;
      procedure HandlePalette; virtual;
      procedure HandleAlpha; virtual;
      function CalcX (relX:integer) : integer;
      function CalcY (relY:integer) : integer;
      function CalcColor: TColorData;
      procedure HandleScanLine (const y : integer; const ScanLine : PByteArray); virtual;
      procedure BGRAHandleScanLine(const y: integer; const ScanLine: PByteArray);
      procedure BGRAHandleScanLineTr(const y: integer; const ScanLine: PByteArray);
      procedure DoDecompress; virtual;
      function  DoFilter(LineFilter:byte;index:longword; b:byte) : byte; virtual;
      procedure SetPalettePixel (x,y:integer; const CD : TColordata);
      procedure SetPalColPixel (x,y:integer; const CD : TColordata);
      procedure SetColorPixel (x,y:integer; const CD : TColordata);
      procedure SetColorTrPixel (x,y:integer; const CD : TColordata);
      procedure SetBGRAColorPixel (x,y:integer; const CD : TColordata);
      procedure SetBGRAColorTrPixel (x,y:integer; const CD : TColordata);
      function DecideSetPixel : TSetPixelProc; virtual;
      procedure InternalRead  ({%H-}Str:TStream; Img:TFPCustomImage); override;
      function  InternalCheck (Str:TStream) : boolean; override;
      //property ColorFormat : TColorformat read CFmt;
      property ConvertColor : TConvertColorProc read FConvertColor;
      property CurrentPass : byte read FCurrentPass;
      property Pltte : boolean read FPltte;
      property ThePalette : TFPPalette read FPalette;
      property Header : THeaderChunk read FHeader;
      property CountScanlines : EightLong read FCountScanlines;
      property ScanLineLength : EightLong read FScanLineLength;
    public
      MinifyHeight: integer;
      constructor create; override;
      destructor destroy; override;
      property VerticalShrinkFactor: integer read GetVerticalShrinkFactor;
      property OriginalWidth: integer read GetOriginalWidth;
      property OriginalHeight: integer read GetOriginalHeight;
  end;

implementation



const StartPoints : array[0..7, 0..1] of word =
         ((0,0),(0,0),(4,0),(0,4),(2,0),(0,2),(1,0),(0,1));
      Delta : array[0..7,0..1] of word =
         ((1,1),(8,8),(8,8),(4,8),(4,4),(2,4),(2,2),(1,2));
      BitsUsed1Depth : EightLong = ($80,$40,$20,$10,$08,$04,$02,$01);
      BitsUsed2Depth : EightLong = ($C0,$30,$0C,$03,0,0,0,0);
      BitsUsed4Depth : EightLong = ($F0,$0F,0,0,0,0,0,0);

constructor TBGRAReaderPNG.create;
begin
  inherited;
  chunk.acapacity := 0;
  chunk.data := nil;
  UseTransparent := False;
end;

destructor TBGRAReaderPNG.destroy;
begin
  with chunk do
    if acapacity > 0 then
      freemem (data);
  inherited;
end;

procedure TBGRAReaderPNG.ReadChunk;

var {%H-}ChunkHeader : TChunkHeader;
    readCRC : longword;
    l : longword;
begin
  TheStream.Read ({%H-}ChunkHeader,sizeof(ChunkHeader));
  with chunk do
    begin
    // chunk header
    with ChunkHeader do
      begin
      {$IFDEF ENDIAN_LITTLE}
      alength := swap(CLength);
      {$ELSE}
      alength := CLength;
      {$ENDIF}
      ReadType := CType;
      end;
    aType := low(TChunkTypes);
    while (aType < high(TChunkTypes)) and (ChunkTypes[aType] <> ReadType) do
      inc (aType);
    if alength > MaxChunkLength then
      raise PNGImageException.Create ('Invalid chunklength');
    if alength > acapacity then
      begin
      if acapacity > 0 then
        freemem (data);
      GetMem (data, alength);
      acapacity := alength;
      end;
    l := TheStream.read (data^, alength);
    if l <> alength then
      raise PNGImageException.Create ('Chunk length exceeds stream length');
    readCRC := 0;
    TheStream.Read (readCRC, sizeof(ReadCRC));
    l := CalculateCRC (All1Bits, ReadType, sizeOf(ReadType));
    l := CalculateCRC (l, data^, alength);
    {$IFDEF ENDIAN_LITTLE}
    l := swap(l xor All1Bits);
    {$ELSE}
    l := l xor All1Bits;
    {$ENDIF}
    if ReadCRC <> l then
      raise PNGImageException.Create ('CRC check failed');
    end;
end;

function TBGRAReaderPNG.GetVerticalShrinkFactor: integer;
begin
  result := 1 shl FVerticalShrinkShr;
end;

function TBGRAReaderPNG.GetOriginalHeight: integer;
begin
  result := Header.height;
end;

function TBGRAReaderPNG.GetOriginalWidth: integer;
begin
  result := header.Width;
end;

procedure TBGRAReaderPNG.HandleData;
var OldSize : longword;
begin
  OldSize := ZData.size;
  ZData.Size := OldSize;
  ZData.Size := ZData.Size + Chunk.aLength;
  ZData.Write (chunk.Data^, chunk.aLength);
end;

procedure TBGRAReaderPNG.HandleAlpha;
  procedure PaletteAlpha;
    var r : integer;
        a : word;
        c : TFPColor;
    begin
      with chunk do
        begin
        if alength > longword(ThePalette.count) then
          raise PNGImageException.create ('To much alpha values for palette');
        for r := 0 to alength-1 do
          begin
          c := ThePalette[r];
          a := data^[r];
          c.alpha := (a shl 8) + a;
          ThePalette[r] := c;
          end;
        end;
    end;
  procedure TransparentGray;
    var {%H-}a : word;
    begin
      move (chunk.data^[0], {%H-}a, 2);
      {$IFDEF ENDIAN_LITTLE}
      a := swap (a);
      {$ENDIF}
      TransparentDataValue := a;
      UseTransparent := True;
    end;
  procedure TransparentColor;
    var d : byte;
        {%H-}r,{%H-}g,{%H-}b : word;
        a : TColorData;
    begin
      with chunk do
        begin
        move (data^[0], {%H-}r, 2);
        move (data^[2], {%H-}g, 2);
        move (data^[4], {%H-}b, 2);
        end;
      {$IFDEF ENDIAN_LITTLE}
      r := swap (r);
      g := swap (g);
      b := swap (b);
      {$ENDIF}
      d := header.bitdepth;
      a := (TColorData(b) shl d) shl d;
      a := a + (TColorData(g) shl d) + r;
      TransparentDataValue := a;
      UseTransparent := True;
    end;
begin
  case header.ColorType of
    3 : PaletteAlpha;
    0 : TransparentGray;
    2 : TransparentColor;
  end;
end;

procedure TBGRAReaderPNG.HandlePalette;
var r : longword;
    c : TFPColor;
    t : word;
begin
  if header.colortype = 3 then
    with chunk do
      begin
      if TheImage.UsePalette then
        FPalette := TheImage.Palette
      else
        FPalette := TFPPalette.Create(0);
      c.Alpha := AlphaOpaque;
      if (aLength mod 3) > 0 then
        raise PNGImageException.Create ('Impossible length for PLTE-chunk');
      r := 0;
      ThePalette.count := 0;
      while r < alength do
        begin
        t := data^[r];
        c.red := t + (t shl 8);
        inc (r);
        t := data^[r];
        c.green := t + (t shl 8);
        inc (r);
        t := data^[r];
        c.blue := t + (t shl 8);
        inc (r);
        ThePalette.Add (c);
        end;
      end;
end;

procedure TBGRAReaderPNG.SetPalettePixel (x,y:integer; const CD : TColordata);
begin  // both PNG and palette have palette
  TheImage.Pixels[x,y] := CD;
end;

procedure TBGRAReaderPNG.SetPalColPixel (x,y:integer; const CD : TColordata);
begin  // PNG with palette, Img without
  TheImage.Colors[x,y] := ThePalette[CD];
end;

procedure TBGRAReaderPNG.SetColorPixel (x,y:integer; const CD : TColordata);
var c : TFPColor;
begin  // both PNG and Img work without palette, and no transparency colordata
  // c := ConvertColor (CD,CFmt);
  c := ConvertColor (CD);
  TheImage.Colors[x,y] := c;
end;

procedure TBGRAReaderPNG.SetColorTrPixel (x,y:integer; const CD : TColordata);
var c : TFPColor;
begin  // both PNG and Img work without palette, and there is a transparency colordata
  //c := ConvertColor (CD,CFmt);
  c := ConvertColor (CD);
  if TransparentDataValue = CD then
    c.alpha := alphaTransparent;
  TheImage.Colors[x,y] := c;
end;

procedure TBGRAReaderPNG.SetBGRAColorPixel(x, y: integer; const CD: TColordata);
var c: TBGRAPixel;
begin
  c := FBGRAConvertColor(CD);
  if c.alpha = 0 then TBGRACustomBitmap(TheImage).SetPixel(x,y,BGRAPixelTransparent)
  else TBGRACustomBitmap(TheImage).SetPixel(x,y,c);
end;

procedure TBGRAReaderPNG.SetBGRAColorTrPixel(x, y: integer; const CD: TColordata);
var c: TBGRAPixel;
begin
  if TransparentDataValue = CD then
    TBGRACustomBitmap(TheImage).SetPixel(x,y,BGRAPixelTransparent) else
  begin
    c := FBGRAConvertColor(CD);
    if c.alpha = 0 then TBGRACustomBitmap(TheImage).SetPixel(x,y,BGRAPixelTransparent)
    else TBGRACustomBitmap(TheImage).SetPixel(x,y,c);
  end;
end;

function TBGRAReaderPNG.CurrentLine(x:longword):byte;
begin
  result := FCurrentLine^[x];
end;

function TBGRAReaderPNG.PrevSample (x:longword): byte;
begin
  if x < byteWidth then
    result := 0
  else
    result := FCurrentLine^[x - bytewidth];
end;

function TBGRAReaderPNG.PreviousLine (x:longword) : byte;
begin
  result := FPreviousline^[x];
end;

function TBGRAReaderPNG.PrevLinePrevSample (x:longword): byte;
begin
  if x < byteWidth then
    result := 0
  else
    result := FPreviousLine^[x - bytewidth];
end;

function TBGRAReaderPNG.DoFilter(LineFilter:byte;index:longword; b:byte) : byte;
var diff : byte;
  procedure FilterSub;
  begin
    diff := PrevSample(index);
  end;
  procedure FilterUp;
  begin
    diff := PreviousLine(index);
  end;
  procedure FilterAverage;
  var l, p : word;
  begin
    l := PrevSample(index);
    p := PreviousLine(index);
    diff := (l + p) div 2;
  end;
  procedure FilterPaeth;
  var dl, dp, dlp : word; // index for previous and distances for:
      l, p, lp : byte;  // r:predictor, Left, Previous, LeftPrevious
      r : integer;
  begin
    l := PrevSample(index);
    lp := PrevLinePrevSample(index);
    p := PreviousLine(index);
    r := integer(l) + integer(p) - integer(lp);
    dl := abs (r - l);
    dlp := abs (r - lp);
    dp := abs (r - p);
    if (dl <= dp) and (dl <= dlp) then
      diff := l
    else if dp <= dlp then
      diff := p
    else
      diff := lp;
  end;
begin
  case LineFilter of
    0 : diff := 0;
    1 : FilterSub;
    2 : FilterUp;
    3 : FilterAverage;
    4 : FilterPaeth;
  end;
  result := (b + diff) mod $100;
end;

function TBGRAReaderPNG.DecideSetPixel : TSetPixelProc;
begin
  if Pltte then
    if TheImage.UsePalette then
      result := @SetPalettePixel
    else
      result := @SetPalColPixel
  else
    if UseTransparent then
    begin
      if TheImage is TBGRACustomBitmap then
        result := @SetBGRAColorTrPixel
      else
        result := @SetColorTrPixel
    end
    else
    begin
      if TheImage is TBGRACustomBitmap then
        result := @SetBGRAColorPixel
      else
        result := @SetColorPixel
    end;
end;

function TBGRAReaderPNG.CalcX (relX:integer) : integer;
begin
  result := StartX + (relX * deltaX);
end;

function TBGRAReaderPNG.CalcY (relY:integer) : integer;
begin
  result := StartY + (relY * deltaY);
end;

function TBGRAReaderPNG.CalcColor: TColorData;
var cd : longword;
    r : word;
    p : pbyte;
begin
  if UsingBitGroup = 0 then
    begin
    Databytes := 0;
    if Header.BitDepth = 16 then
      begin
       p := @Databytes;
       p^ := 0;
       for r:=0 to bytewidth-2 do
       begin
        inc(p);
        p^:=FCurrentLine^[Dataindex+r];
       end;
      end
    else move (FCurrentLine^[DataIndex], Databytes, bytewidth);
    {$IFDEF ENDIAN_BIG}
    Databytes:=swap(Databytes);
    {$ENDIF}
    inc (DataIndex,bytewidth);
    end;
  if bytewidth = 1 then
    begin
    cd := (Databytes and BitsUsed[UsingBitGroup]);
    result := cd shr ((CountBitsUsed-UsingBitGroup-1) * BitShift);
    inc (UsingBitgroup);
    if UsingBitGroup >= CountBitsUsed then
      UsingBitGroup := 0;
    end
  else
    result := Databytes;
end;

procedure TBGRAReaderPNG.HandleScanLine (const y : integer; const ScanLine : PByteArray);
var x, rx : integer;
    c : TColorData;
begin
  UsingBitGroup := 0;
  DataIndex := 0;
  X := StartX;
  if (UsingBitGroup = 0) and (Header.BitDepth <> 16) then
    case ByteWidth of
      1: if BitsUsed[0] = $ff then
         begin
           for rx := 0 to ScanlineLength[CurrentPass]-1 do
           begin
             FSetPixel (x,y,ScanLine^[DataIndex]);
             Inc(X, deltaX);
             inc(DataIndex);
           end;
           exit;
         end;
      2: begin
           for rx := 0 to ScanlineLength[CurrentPass]-1 do
           begin
             {$IFDEF ENDIAN_BIG}
             FSetPixel (x,y,swap(PWord(@ScanLine^[DataIndex])^));
             {$ELSE}
             FSetPixel (x,y,PWord(@ScanLine^[DataIndex])^);
             {$ENDIF}
             Inc(X, deltaX);
             inc(DataIndex,2);
           end;
           exit;
         end;
      4: begin
           for rx := 0 to ScanlineLength[CurrentPass]-1 do
           begin
             {$IFDEF ENDIAN_BIG}
             FSetPixel (x,y,swap(PDWord(@ScanLine^[DataIndex])^));
             {$ELSE}
             FSetPixel (x,y,PDWord(@ScanLine^[DataIndex])^);
             {$ENDIF}
             Inc(X, deltaX);
             inc(DataIndex,4);
           end;
           exit;
         end;
      8: begin
           for rx := 0 to ScanlineLength[CurrentPass]-1 do
           begin
             {$IFDEF ENDIAN_BIG}
             FSetPixel (x,y,swap(PQWord(@ScanLine^[DataIndex])^));
             {$ELSE}
             FSetPixel (x,y,PQWord(@ScanLine^[DataIndex])^);
             {$ENDIF}
             Inc(X, deltaX);
             inc(DataIndex,8);
           end;
           exit;
         end;
    end;

  for rx := 0 to ScanlineLength[CurrentPass]-1 do
    begin
    c := CalcColor;
    FSetPixel (x,y,c);
    Inc(X, deltaX);
    end
end;

procedure TBGRAReaderPNG.BGRAHandleScanLine (const y : integer; const ScanLine : PByteArray);
var x, rx : integer;
    c : TColorData;
    pdest: PBGRAPixel;
begin
  UsingBitGroup := 0;
  DataIndex := 0;
  if (UsingBitGroup = 0) and (Header.BitDepth <> 16) then
    case ByteWidth of
      1: if BitsUsed[0] = $ff then
         begin
           pdest := TBGRACustomBitmap(TheImage).ScanLine[y]+StartX;
           for rx := 0 to ScanlineLength[CurrentPass]-1 do
           begin
             pdest^ := FBGRAConvertColor(ScanLine^[DataIndex]);
             if pdest^.alpha = 0 then pdest^ := BGRAPixelTransparent;
             Inc(pdest, deltaX);
             inc(DataIndex);
           end;
           exit;
         end;
      2: begin
           pdest := TBGRACustomBitmap(TheImage).ScanLine[y]+StartX;
           for rx := 0 to ScanlineLength[CurrentPass]-1 do
           begin
             pdest^ := FBGRAConvertColor(
             {$IFDEF ENDIAN_BIG}
             swap(PWord(@ScanLine^[DataIndex])^)
             {$ELSE}
             PWord(@ScanLine^[DataIndex])^
             {$ENDIF} );
             if pdest^.alpha = 0 then pdest^ := BGRAPixelTransparent;
             Inc(pdest, deltaX);
             inc(DataIndex,2);
           end;
           exit;
         end;
      4: begin
           pdest := TBGRACustomBitmap(TheImage).ScanLine[y]+StartX;
           for rx := 0 to ScanlineLength[CurrentPass]-1 do
           begin
             pdest^ := FBGRAConvertColor(
             {$IFDEF ENDIAN_BIG}
             swap(PDWord(@ScanLine^[DataIndex])^)
             {$ELSE}
             PDWord(@ScanLine^[DataIndex])^
             {$ENDIF}  );
             if pdest^.alpha = 0 then pdest^ := BGRAPixelTransparent;
             Inc(pdest, deltaX);
             inc(DataIndex,4);
           end;
           exit;
         end;
      8: begin
           pdest := TBGRACustomBitmap(TheImage).ScanLine[y]+StartX;
           for rx := 0 to ScanlineLength[CurrentPass]-1 do
           begin
             pdest^ := FBGRAConvertColor(
             {$IFDEF ENDIAN_BIG}
             swap(PQWord(@ScanLine^[DataIndex])^)
             {$ELSE}
             PQWord(@ScanLine^[DataIndex])^
             {$ENDIF}  );
             if pdest^.alpha = 0 then pdest^ := BGRAPixelTransparent;
             Inc(pdest, deltaX);
             inc(DataIndex,8);
           end;
           exit;
         end;
    end;

  X := StartX;
  for rx := 0 to ScanlineLength[CurrentPass]-1 do
    begin
    c := CalcColor;
    FSetPixel (x,y,c);
    Inc(X, deltaX);
    end
end;

procedure TBGRAReaderPNG.BGRAHandleScanLineTr(const y: integer;
  const ScanLine: PByteArray);
var x, rx : integer;
    c : TColorData;
    pdest: PBGRAPixel;
begin
  UsingBitGroup := 0;
  DataIndex := 0;
  if (UsingBitGroup = 0) and (Header.BitDepth <> 16) then
    case ByteWidth of
      1: if BitsUsed[0] = $ff then
         begin
           pdest := TBGRACustomBitmap(TheImage).ScanLine[y]+StartX;
           for rx := 0 to ScanlineLength[CurrentPass]-1 do
           begin
             c := ScanLine^[DataIndex];
             if c = TransparentDataValue then
               pdest^ := BGRAPixelTransparent else
               begin
                 pdest^ := FBGRAConvertColor(c);
                 if pdest^.alpha = 0 then pdest^ := BGRAPixelTransparent;
             end;
             Inc(pdest, deltaX);
             inc(DataIndex);
           end;
           exit;
         end;
      2: begin
           pdest := TBGRACustomBitmap(TheImage).ScanLine[y]+StartX;
           for rx := 0 to ScanlineLength[CurrentPass]-1 do
           begin
             c :=
             {$IFDEF ENDIAN_BIG}
             swap(PWord(@ScanLine^[DataIndex])^)
             {$ELSE}
             PWord(@ScanLine^[DataIndex])^
             {$ENDIF} ;
             if c = TransparentDataValue then
               pdest^ := BGRAPixelTransparent else
               begin
                 pdest^ := FBGRAConvertColor(c);
                 if pdest^.alpha = 0 then pdest^ := BGRAPixelTransparent;
               end;
             Inc(pdest, deltaX);
             inc(DataIndex,2);
           end;
           exit;
         end;
      4: begin
           pdest := TBGRACustomBitmap(TheImage).ScanLine[y]+StartX;
           for rx := 0 to ScanlineLength[CurrentPass]-1 do
           begin
             c :=
             {$IFDEF ENDIAN_BIG}
             swap(PDWord(@ScanLine^[DataIndex])^)
             {$ELSE}
             PDWord(@ScanLine^[DataIndex])^
             {$ENDIF}  ;
             if c = TransparentDataValue then
               pdest^ := BGRAPixelTransparent else
               begin
                 pdest^ := FBGRAConvertColor(c);
                 if pdest^.alpha = 0 then pdest^ := BGRAPixelTransparent;
               end;
             Inc(pdest, deltaX);
             inc(DataIndex,4);
           end;
           exit;
         end;
      8: begin
           pdest := TBGRACustomBitmap(TheImage).ScanLine[y]+StartX;
           for rx := 0 to ScanlineLength[CurrentPass]-1 do
           begin
             c :=
             {$IFDEF ENDIAN_BIG}
             swap(PQWord(@ScanLine^[DataIndex])^)
             {$ELSE}
             PQWord(@ScanLine^[DataIndex])^
             {$ENDIF}  ;
             if c = TransparentDataValue then
               pdest^ := BGRAPixelTransparent else
               begin
                 pdest^ := FBGRAConvertColor(c);
                 if pdest^.alpha = 0 then pdest^ := BGRAPixelTransparent;
               end;
             Inc(pdest, deltaX);
             inc(DataIndex,8);
           end;
           exit;
         end;
    end;

  X := StartX;
  for rx := 0 to ScanlineLength[CurrentPass]-1 do
    begin
    c := CalcColor;
    FSetPixel (x,y,c);
    Inc(X, deltaX);
    end
end;

function TBGRAReaderPNG.ColorGray1 (const CD:TColorDAta) : TFPColor;
begin
  if CD = 0 then
    result := colBlack
  else
    result := colWhite;
end;

function TBGRAReaderPNG.ColorGray2 (const CD:TColorDAta) : TFPColor;
var c : NativeUint;
begin
  c := CD and 3;
  c := c + (c shl 2);
  c := c + (c shl 4);
  c := c + (c shl 8);
  with result do
    begin
    red := c;
    green := c;
    blue := c;
    alpha := alphaOpaque;
    end;
end;

function TBGRAReaderPNG.ColorGray4 (const CD:TColorDAta) : TFPColor;
var c : NativeUint;
begin
  c := CD and $F;
  c := c + (c shl 4);
  c := c + (c shl 8);
  with result do
    begin
    red := c;
    green := c;
    blue := c;
    alpha := alphaOpaque;
    end;
end;

function TBGRAReaderPNG.ColorGray8 (const CD:TColorDAta) : TFPColor;
var c : NativeUint;
begin
  c := CD and $FF;
  c := c + (c shl 8);
  with result do
    begin
    red := c;
    green := c;
    blue := c;
    alpha := alphaOpaque;
    end;
end;

function TBGRAReaderPNG.ColorGray16 (const CD:TColorDAta) : TFPColor;
var c : NativeUint;
begin
  c := CD and $FFFF;
  with result do
    begin
    red := c;
    green := c;
    blue := c;
    alpha := alphaOpaque;
    end;
end;

function TBGRAReaderPNG.ColorGrayAlpha8 (const CD:TColorData) : TFPColor;
var c : NativeUint;
begin
  c := CD and $00FF;
  c := c + (c shl 8);
  with result do
    begin
    red := c;
    green := c;
    blue := c;
    c := CD and $FF00;
    alpha := c + (c shr 8);
    end;
end;

function TBGRAReaderPNG.ColorGrayAlpha16 (const CD:TColorData) : TFPColor;
var c : NativeUint;
begin
  c := (CD shr 16) and $FFFF;
  with result do
    begin
    red := c;
    green := c;
    blue := c;
    alpha := CD and $FFFF;
    end;
end;

function TBGRAReaderPNG.ColorColor8 (const CD:TColorData) : TFPColor;
var c : NativeUint;
begin
  with result do
    begin
    c := CD and $FF;
    red := c + (c shl 8);
    c := (CD shr 8) and $FF;
    green := c + (c shl 8);
    c := (CD shr 16) and $FF;
    blue := c + (c shl 8);
    alpha := alphaOpaque;
    end;
end;

function TBGRAReaderPNG.ColorColor16 (const CD:TColorData) : TFPColor;
begin
  with result do
    begin
    red := CD and $FFFF;
    green := (CD shr 16) and $FFFF;
    blue := (CD shr 32) and $FFFF;
    alpha := alphaOpaque;
    end;
end;

function TBGRAReaderPNG.ColorColorAlpha8 (const CD:TColorData) : TFPColor;
var c : NativeUint;
begin
  with result do
    begin
    c := CD and $FF;
    red := c + (c shl 8);
    c := (CD shr 8) and $FF;
    green := c + (c shl 8);
    c := (CD shr 16) and $FF;
    blue := c + (c shl 8);
    c := (CD shr 24) and $FF;
    alpha := c + (c shl 8);
    end;
end;

function TBGRAReaderPNG.ColorColorAlpha16 (const CD:TColorData) : TFPColor;
begin
  with result do
    begin
    red := CD and $FFFF;
    green := (CD shr 16) and $FFFF;
    blue := (CD shr 32) and $FFFF;
    alpha := (CD shr 48) and $FFFF;
    end;
end;

function TBGRAReaderPNG.BGRAColorGray1(const CD: TColorData): TBGRAPixel;
begin
  if CD = 0 then
    result := BGRABlack
  else
    result := BGRAWhite;
end;

function TBGRAReaderPNG.BGRAColorGray2(const CD: TColorData): TBGRAPixel;
var c : NativeUint;
begin
  c := CD and 3;
  c := c + (c shl 2);
  c := c + (c shl 4);
  with result do
    begin
    red := c;
    green := c;
    blue := c;
    alpha := 255;
    end;
end;

function TBGRAReaderPNG.BGRAColorGray4(const CD: TColorData): TBGRAPixel;
var c : NativeUint;
begin
  c := CD and $F;
  c := c + (c shl 4);
  with result do
    begin
    red := c;
    green := c;
    blue := c;
    alpha := 255;
    end;
end;

function TBGRAReaderPNG.BGRAColorGray8(const CD: TColorData): TBGRAPixel;
var c : NativeUint;
begin
  c := CD and $FF;
  with result do
    begin
    red := c;
    green := c;
    blue := c;
    alpha := 255;
    end;
end;

function TBGRAReaderPNG.BGRAColorGray16(const CD: TColorData): TBGRAPixel;
var c : NativeUint;
begin
  c := (CD shr 8) and $FF;
  with result do
    begin
    red := c;
    green := c;
    blue := c;
    alpha := 255;
    end;
end;

function TBGRAReaderPNG.BGRAColorGrayAlpha8(const CD: TColorData): TBGRAPixel;
var c : NativeUint;
begin
  c := CD and $00FF;
  with result do
    begin
    red := c;
    green := c;
    blue := c;
    alpha := (CD shr 8) and $FF;
    end;
end;

function TBGRAReaderPNG.BGRAColorGrayAlpha16(const CD: TColorData): TBGRAPixel;
var c : NativeUint;
begin
  c := (CD shr 24) and $FF;
  with result do
    begin
    red := c;
    green := c;
    blue := c;
    alpha := (CD shr 8) and $FF;
    end;
end;

function TBGRAReaderPNG.BGRAColorColor8(const CD: TColorData): TBGRAPixel;
var temp: DWord;
begin
  temp := CD;
  temp := ((temp and $ff) shl 16) or
    (temp and $ff00) or ((temp shr 16) and $ff) or
    $ff000000;
  {$IFDEF ENDIAN_BIG}
  DWord(result) := swap(temp);
  {$ELSE}
  DWord(result) := temp;
  {$ENDIF}
end;

function TBGRAReaderPNG.BGRAColorColor16(const CD: TColorData): TBGRAPixel;
begin
  with result do
    begin
    red := CD shr 8 and $FF;
    green := (CD shr 24) and $FF;
    blue := (CD shr 40) and $FF;
    alpha := 255;
    end;
end;

function TBGRAReaderPNG.BGRAColorColorAlpha8(const CD: TColorData): TBGRAPixel;
var temp: DWord;
begin
  temp := CD;
  temp := ((temp and $ff) shl 16) or
    (temp and $ff00) or ((temp shr 16) and $ff) or
    (temp and $ff000000);
  {$IFDEF ENDIAN_BIG}
  DWord(result) := swap(temp);
  {$ELSE}
  DWord(result) := temp;
  {$ENDIF}
end;

function TBGRAReaderPNG.BGRAColorColorAlpha16(const CD: TColorData): TBGRAPixel;
begin
  with result do
    begin
    red := (CD shr 8) and $FF;
    green := (CD shr 24) and $FF;
    blue := (CD shr 40) and $FF;
    alpha := (CD shr 56) and $FF;
    end;
end;

procedure TBGRAReaderPNG.DoDecompress;

  procedure initVars;
  var r,d : integer;
  begin
    with Header do
      begin
      if interlace=0 then
        begin
        StartPass := 0;
        EndPass := 0;
        FCountScanlines[0] := Height;
        FScanLineLength[0] := Width;
        end
      else
        begin
        StartPass := 1;
        EndPass := 7;
        for r := 1 to 7 do
          begin
          d := Height div delta[r,1];
          if (height mod delta[r,1]) > startpoints[r,1] then
            inc (d);
          FCountScanlines[r] := d;
          d := width div delta[r,0];
          if (width mod delta[r,0]) > startpoints[r,0] then
            inc (d);
          FScanLineLength[r] := d;
          end;
        end;
      Fpltte := (ColorType = 3);
      case colortype of
        0 : case Bitdepth of
              1  : begin
                   FConvertColor := @ColorGray1; //CFmt := cfMono;
                   FBGRAConvertColor := @BGRAColorGray1; //CFmt := cfMono;
                   ByteWidth := 1;
                   end;
              2  : begin
                   FConvertColor := @ColorGray2; //CFmt := cfGray2;
                   FBGRAConvertColor := @BGRAColorGray2; //CFmt := cfGray2;
                   ByteWidth := 1;
                   end;
              4  : begin
                   FConvertColor := @ColorGray4; //CFmt := cfGray4;
                   FBGRAConvertColor := @BGRAColorGray4; //CFmt := cfGray4;
                   ByteWidth := 1;
                   end;
              8  : begin
                   FConvertColor := @ColorGray8; //CFmt := cfGray8;
                   FBGRAConvertColor := @BGRAColorGray8; //CFmt := cfGray8;
                   ByteWidth := 1;
                   end;
              16 : begin
                   FConvertColor := @ColorGray16; //CFmt := cfGray16;
                   FBGRAConvertColor := @BGRAColorGray16; //CFmt := cfGray16;
                   ByteWidth := 2;
                   end;
            end;
        2 : if BitDepth = 8 then
              begin
              FConvertColor := @ColorColor8; //CFmt := cfBGR24
              FBGRAConvertColor := @BGRAColorColor8; //CFmt := cfBGR24
              ByteWidth := 3;
              end
            else
              begin
              FConvertColor := @ColorColor16; //CFmt := cfBGR48;
              FBGRAConvertColor := @BGRAColorColor16; //CFmt := cfBGR48;
              ByteWidth := 6;
              end;
        3 : if BitDepth = 16 then
              ByteWidth := 2
            else
              ByteWidth := 1;
        4 : if BitDepth = 8 then
              begin
              FConvertColor := @ColorGrayAlpha8; //CFmt := cfGrayA16
              FBGRAConvertColor := @BGRAColorGrayAlpha8; //CFmt := cfGrayA16
              ByteWidth := 2;
              end
            else
              begin
              FConvertColor := @ColorGrayAlpha16; //CFmt := cfGrayA32;
              FBGRAConvertColor := @BGRAColorGrayAlpha16; //CFmt := cfGrayA32;
              ByteWidth := 4;
              end;
        6 : if BitDepth = 8 then
              begin
              FConvertColor := @ColorColorAlpha8; //CFmt := cfABGR32
              FBGRAConvertColor := @BGRAColorColorAlpha8; //CFmt := cfABGR32
              ByteWidth := 4;
              end
            else
              begin
              FConvertColor := @ColorColorAlpha16; //CFmt := cfABGR64;
              FBGRAConvertColor := @BGRAColorColorAlpha16; //CFmt := cfABGR64;
              ByteWidth := 8;
              end;
      end;
      //ByteWidth := BytesNeeded[CFmt];
      case BitDepth of
        1 : begin
            CountBitsUsed := 8;
            BitShift := 1;
            BitsUsed := BitsUsed1Depth;
            end;
        2 : begin
            CountBitsUsed := 4;
            BitShift := 2;
            BitsUsed := BitsUsed2Depth;
            end;
        4 : begin
            CountBitsUsed := 2;
            BitShift := 4;
            BitsUsed := BitsUsed4Depth;
            end;
        8 : begin
            CountBitsUsed := 1;
            BitShift := 0;
            BitsUsed[0] := $FF;
            end;
        end;
      end;
  end;

  procedure Decode;
  var y, rp, ry, rx, l : integer;
      lf : byte;
  begin
    FSetPixel := DecideSetPixel;
    if not Pltte and (TheImage is TBGRACustomBitmap) then
    begin
      if UseTransparent then
        FHandleScanLine := @BGRAHandleScanLineTr
      else
        FHandleScanLine := @BGRAHandleScanLine;
    end else
      FHandleScanLine := @HandleScanLine;
    for rp := StartPass to EndPass do
      begin
      FCurrentPass := rp;
      StartX := StartPoints[rp,0];
      StartY := StartPoints[rp,1];
      DeltaX := Delta[rp,0];
      DeltaY := Delta[rp,1];
      if bytewidth = 1 then
        begin
        l := (ScanLineLength[rp] div CountBitsUsed);
        if (ScanLineLength[rp] mod CountBitsUsed) > 0 then
          inc (l);
        end
      else
        l := ScanLineLength[rp]*ByteWidth;
      if (l>0) then
        begin
        GetMem (FPreviousLine, l);
        GetMem (FCurrentLine, l);
        fillchar (FCurrentLine^,l,0);
        try
          for ry := 0 to CountScanlines[rp]-1 do
            begin
            FSwitchLine := FCurrentLine;
            FCurrentLine := FPreviousLine;
            FPreviousLine := FSwitchLine;
            Y := CalcY(ry);
            lf := 0;
            Decompress.Read (lf, sizeof(lf));
            Decompress.Read (FCurrentLine^, l);
            if lf <> 0 then  // Do nothing when there is no filter used
              for rx := 0 to l-1 do
                FCurrentLine^[rx] := DoFilter (lf, rx, FCurrentLine^[rx]);
            if FVerticalShrinkShr <> 0 then
              begin
                if (y and FVerticalShrinkMask) = 0 then
                  FHandleScanLine (y shr FVerticalShrinkShr, FCurrentLine);
              end else
                FHandleScanLine (y, FCurrentLine);
            end;
        finally
          freemem (FPreviousLine);
          freemem (FCurrentLine);
        end;
        end;
      end;
  end;

begin
  InitVars;
  DeCode;
end;

procedure TBGRAReaderPNG.HandleChunk;
begin
  case chunk.AType of
    ctIHDR : raise PNGImageException.Create ('Second IHDR chunk found');
    ctPLTE : HandlePalette;
    ctIDAT : HandleData;
    ctIEND : EndOfFile := True;
    cttRNS : HandleAlpha;
    else HandleUnknown;
  end;
end;

procedure TBGRAReaderPNG.HandleUnknown;
begin
  if (chunk.readtype[0] in ['A'..'Z']) then
    raise PNGImageException.Create('Critical chunk '+chunk.readtype+' not recognized');
end;

procedure TBGRAReaderPNG.InternalRead (Str:TStream; Img:TFPCustomImage);
var outputHeight: integer;
begin
  {$ifdef FPC_Debug_Image}
  if Str<>TheStream then
    writeln('WARNING: TBGRAReaderPNG.InternalRead Str<>TheStream');
  {$endif}
  with Header do
  begin
    FVerticalShrinkShr := 0;
    FVerticalShrinkMask := 0;
    outputHeight := Height;
    if MinifyHeight <> 0 then
      begin
        while (outputHeight shr 1 >= MinifyHeight) and (FVerticalShrinkShr < 8) do
          begin
            outputHeight:= outputHeight shr 1;
            Inc(FVerticalShrinkShr);
          end;
        FVerticalShrinkMask:= (1 shl FVerticalShrinkShr)-1;
        outputHeight := (Height+FVerticalShrinkMask) shr FVerticalShrinkShr;
      end;
    Img.SetSize (Width, outputHeight);
  end;
  ZData := TMemoryStream.Create;
  try
    EndOfFile := false;
    while not EndOfFile do
      begin
      ReadChunk;
      HandleChunk;
      end;
    ZData.position:=0;
    Decompress := TDecompressionStream.Create (ZData);
    try
      DoDecompress;
    finally
      Decompress.Free;
    end;
  finally
    ZData.Free;
    if not img.UsePalette and assigned(FPalette) then
      begin
      FPalette.Free;
      end;
  end;
end;

function  TBGRAReaderPNG.InternalCheck (Str:TStream) : boolean;
var {%H-}SigCheck : array[0..7] of byte;
    r : integer;
begin
  try
    // Check Signature
    if Str.Read({%H-}SigCheck, SizeOf(SigCheck)) <> SizeOf(SigCheck) then
      raise PNGImageException.Create('This is not PNG-data');
    for r := 0 to 7 do
    begin
      If SigCheck[r] <> Signature[r] then
        raise PNGImageException.Create('This is not PNG-data');
    end;
    // Check IHDR
    ReadChunk;
    move (chunk.data^, FHeader, sizeof(Header));
    with header do
      begin
      {$IFDEF ENDIAN_LITTLE}
      Width := swap(width);
      height := swap (height);
      {$ENDIF}
      result := (width > 0) and (height > 0) and (compression = 0)
                and (filter = 0) and (Interlace in [0,1]);
      end;
  except
    result := false;
  end;
end;

initialization

  DefaultBGRAImageReader[ifPng] := TBGRAReaderPNG;

end.

