unit BGRAFilterScanner;

{$mode objfpc}{$H+}

interface

uses
  Classes, BGRABitmapTypes, BGRAFilterType;

type
  { TBGRAFilterScannerGrayscale }
  { Grayscale converts colored pixel into grayscale with same luminosity }
  TBGRAFilterScannerGrayscale = class(TBGRAFilterScannerPixelwise)
    class procedure ComputeFilterAt(ASource: PBGRAPixel; ADest: PBGRAPixel;
      ACount: integer; AGammaCorrection: boolean); override;
  end;

  { TBGRAFilterScannerNegative }

  TBGRAFilterScannerNegative = class(TBGRAFilterScannerPixelwise)
    class procedure ComputeFilterAt(ASource: PBGRAPixel; ADest: PBGRAPixel;
      ACount: integer; AGammaCorrection: boolean); override;
  end;

  { TBGRAFilterScannerSwapRedBlue }

  TBGRAFilterScannerSwapRedBlue = class(TBGRAFilterScannerPixelwise)
    class procedure ComputeFilterAt(ASource: PBGRAPixel; ADest: PBGRAPixel;
      ACount: integer; {%H-}AGammaCorrection: boolean); override;
  end;

  { TBGRAFilterScannerNormalize }
  { Normalize compute min-max of specified channel and apply an affine transformation
    to make it use the full range of values }
  TBGRAFilterScannerNormalize = class(TBGRAFilterScannerPixelwise)
  private
    minValRed, maxValRed, minValGreen, maxValGreen,
    minValBlue, maxValBlue, minAlpha, maxAlpha: word;
    addValRed, addValGreen, addValBlue, addAlpha: word;
    factorValRed, factorValGreen, factorValBlue, factorAlpha: int32or64;
    procedure DetermineNormalizationFactors(ABounds: TRect; AEachChannel: boolean);
  protected
    procedure DoComputeFilterAt(ASource: PBGRAPixel; ADest: PBGRAPixel;
            ACount: integer; {%H-}AGammaCorrection: boolean); override;
  public
    constructor Create(ASource: IBGRAScanner; AOffset: TPoint; ABounds: TRect;
            AEachChannel: boolean);
    class procedure ComputeFilterAt({%H-}ASource: PBGRAPixel; {%H-}ADest: PBGRAPixel;
            {%H-}ACount: integer; {%H-}AGammaCorrection: boolean); override;
  end;

  { TBGRA3X3FilterScanner }

  TBGRA3X3FilterScanner = class(TBGRAFilterScannerMultipixel)
  protected
    FSourceBorderColor,FDestinationBorderColor: TBGRAPixel;
    FAutoSourceBorderColor: boolean;
    function DoFilter3X3(PTop,PMiddle,PBottom: PBGRAPixel): TBGRAPixel; virtual; abstract;
    procedure DoComputeFilter(BufferX: Integer;
      const Buffers: array of PBGRAPixel; BufferWidth: integer;
      ADest: PBGRAPixel; ACount: integer); override;
  public
    constructor Create(ASource: IBGRAScanner; ABounds: TRect);
    constructor Create(ASource: TBGRACustomBitmap);
    property SourceBorderColor: TBGRAPixel read FSourceBorderColor write FSourceBorderColor;
    property DestinationBorderColor: TBGRAPixel read FDestinationBorderColor write FDestinationBorderColor;
    property AutoSourceBorderColor: boolean read FAutoSourceBorderColor write FAutoSourceBorderColor;
  end;

  { TBGRAContourScanner }
  { Filter contour compute a grayscale image, then for each pixel
    calculates the difference with surrounding pixels (in intensity and alpha)
    and draw black pixels when there is a difference }
  TBGRAContourScanner = class(TBGRA3X3FilterScanner)
  protected
    FGammaCorrection: boolean;
    FOpacity: byte;
    function DoFilter3X3(PTop,PMiddle,PBottom: PBGRAPixel): TBGRAPixel; override;
  public
    constructor Create(ASource: IBGRAScanner; ABounds: TRect;
                       AGammaCorrection: boolean = False);
    constructor Create(ASource: TBGRACustomBitmap;
                       AGammaCorrection: boolean = False);
    property Opacity: Byte read FOpacity write FOpacity;
  end;

  { TBGRASharpenScanner }

  TBGRASharpenScanner = class(TBGRA3X3FilterScanner)
  protected
    FAmount: integer;
    function DoFilter3X3(PTop,PMiddle,PBottom: PBGRAPixel): TBGRAPixel; override;
  public
    constructor Create(ASource: IBGRAScanner; ABounds: TRect;
                       AAmount: integer = 256);
    constructor Create(ASource: TBGRACustomBitmap;
                       AAmount: integer = 256);
  end;

  { TBGRAEmbossHightlightScanner }

  TBGRAEmbossHightlightScanner = class(TBGRA3X3FilterScanner)
  protected
    FFillSelection: boolean;
    FSourceChannel: TChannel;
    FChannelOffset: Byte;
    function DoFilter3X3(PTop,PMiddle,PBottom: PBGRAPixel): TBGRAPixel; override;
    procedure SetSourceChannel(AValue: TChannel);
  public
    constructor Create(ASource: IBGRAScanner; ABounds: TRect; ABoundsVisible: Boolean);
    constructor Create(ASource: TBGRACustomBitmap; ABoundsVisible: Boolean);
    property FillSelection: boolean read FFillSelection write FFillSelection;
    property SourceChannel: TChannel read FSourceChannel write SetSourceChannel;
  end;

implementation

uses BGRABlend, math, SysUtils;

{ TBGRAEmbossHightlightScanner }

procedure TBGRAEmbossHightlightScanner.SetSourceChannel(AValue: TChannel);
begin
  FSourceChannel:=AValue;
  case FSourceChannel of
  cRed: FChannelOffset:= TBGRAPixel_RedByteOffset;
  cGreen: FChannelOffset:= TBGRAPixel_GreenByteOffset;
  cBlue: FChannelOffset:= TBGRAPixel_BlueByteOffset;
  else {cAlpha:} FChannelOffset:= TBGRAPixel_AlphaByteOffset;
  end;
end;

function TBGRAEmbossHightlightScanner.DoFilter3X3(PTop, PMiddle,
  PBottom: PBGRAPixel): TBGRAPixel;
var
  sum: NativeInt;
  slope,h: byte;
  highlight: TBGRAPixel;
begin
  sum := (PByte(PTop)+FChannelOffset)^ + (PByte(PTop+1)+FChannelOffset)^+
         (PByte(PMiddle)+FChannelOffset)^ - (PByte(PMiddle+2)+FChannelOffset)^ -
         (PByte(PBottom+1)+FChannelOffset)^ - (PByte(PBottom+2)+FChannelOffset)^;
  sum := 128 - sum div 3;
  if sum > 255 then
    slope := 255
  else
  if sum < 1 then
    slope := 1
  else
    slope := sum;
  h := (PByte(PMiddle+1)+FChannelOffset)^;

  result.red   := slope;
  result.green := slope;
  result.blue  := slope;
  result.alpha := abs(slope - 128) * 2;

  if FFillSelection then
  begin
    highlight := BGRA(h shr 2, h shr 1, h, h shr 1);
    if result.red < highlight.red then
      result.red := highlight.red;
    if result.green < highlight.green then
      result.green := highlight.green;
    if result.blue < highlight.blue then
      result.blue := highlight.blue;
    if result.alpha < highlight.alpha then
      result.alpha := highlight.alpha;
  end;
end;

constructor TBGRAEmbossHightlightScanner.Create(ASource: IBGRAScanner;
  ABounds: TRect; ABoundsVisible: Boolean);
begin
  inherited Create(ASource,ABounds);
  SourceChannel := cGreen;
  FillSelection:= true;
  AutoSourceBorderColor := not ABoundsVisible;
end;

constructor TBGRAEmbossHightlightScanner.Create(ASource: TBGRACustomBitmap;
  ABoundsVisible: Boolean);
begin
  inherited Create(ASource);
  SourceChannel := cGreen;
  FillSelection:= true;
  AutoSourceBorderColor := not ABoundsVisible;
end;

{ TBGRA3X3FilterScanner }

procedure TBGRA3X3FilterScanner.DoComputeFilter(BufferX: Integer;
  const Buffers: array of PBGRAPixel; BufferWidth: integer; ADest: PBGRAPixel;
  ACount: integer);
var MiddleX: Integer;
  TopLine,MiddleLine,BottomLine: array[0..2] of TBGRAPixel;
  PTop,PMiddle,PBottom: PBGRAPixel;
  borderColor: TBGRAPixel;
begin
  if Buffers[1] = nil then
  begin
    FillDWord(ADest^, ACount, DWord(FDestinationBorderColor));
    exit;
  end;
  MiddleX := BufferX+1;
  while (ACount > 0) and (MiddleX < 0) do
  begin
    ADest^ := FDestinationBorderColor;
    Dec(ACount);
    Inc(ADest);
    Inc(MiddleX);
  end;
  if (ACount > 0) and (MiddleX = 0) and (MiddleX < BufferWidth) then
  begin
    MiddleLine[1] := Buffers[1][MiddleX];
    if AutoSourceBorderColor then borderColor := MiddleLine[1]
    else borderColor := FSourceBorderColor;

    TopLine[0] := borderColor;
    MiddleLine[0] := borderColor;
    BottomLine[0] := borderColor;
    if Buffers[0] = nil then TopLine[1] := borderColor else TopLine[1] := Buffers[0][MiddleX];
    if Buffers[2] = nil then BottomLine[1] := borderColor else BottomLine[1] := Buffers[2][MiddleX];
    inc(MiddleX);
    if MiddleX >= BufferWidth then
    begin
      TopLine[2] := borderColor;
      MiddleLine[2] := borderColor;
      BottomLine[2] := borderColor;
    end else
    begin
      if Buffers[0] = nil then TopLine[2] := borderColor else TopLine[2] := Buffers[0][MiddleX];
      MiddleLine[2] := Buffers[1][MiddleX];
      if Buffers[2] = nil then BottomLine[2] := borderColor else BottomLine[2] := Buffers[2][MiddleX];
    end;
    ADest^ := DoFilter3X3(@TopLine,@MiddleLine,@BottomLine);
    Dec(ACount);
    Inc(ADest);
  end;
  if (Buffers[0]<>nil) and (Buffers[2]<>nil) then
  begin
    while (ACount > 0) and (MiddleX+1 < BufferWidth) do
    begin
      ADest^ := DoFilter3X3(@Buffers[0][MiddleX-1],@Buffers[1][MiddleX-1],@Buffers[2][MiddleX-1]);
      Inc(MiddleX);
      Dec(ACount);
      Inc(ADest);
    end;
  end else
  begin
    if not AutoSourceBorderColor then
    begin
      TopLine[0] := FSourceBorderColor;
      TopLine[1] := FSourceBorderColor;
      TopLine[2] := FSourceBorderColor;
      BottomLine[0] := FSourceBorderColor;
      BottomLine[1] := FSourceBorderColor;
      BottomLine[2] := FSourceBorderColor;
    end;
    while (ACount > 0) and (MiddleX+1 < BufferWidth) do
    begin
      PMiddle:= @Buffers[1][MiddleX-1];
      if Buffers[0] = nil then
      begin
        if AutoSourceBorderColor then
        begin
          TopLine[0] := PMiddle[1];
          TopLine[1] := PMiddle[1];
          TopLine[2] := PMiddle[1];
        end;
        PTop := @TopLine;
      end
      else PTop := @Buffers[0][MiddleX-1];
      if Buffers[2] = nil then
      begin
        if AutoSourceBorderColor then
        begin
          BottomLine[0] := PMiddle[1];
          BottomLine[1] := PMiddle[1];
          BottomLine[2] := PMiddle[1];
        end;
        PBottom := @BottomLine;
      end
      else PBottom := @Buffers[2][MiddleX-1];
      ADest^ := DoFilter3X3(PTop,PMiddle,PBottom);
      Inc(MiddleX);
      Dec(ACount);
      Inc(ADest);
    end;
  end;
  if (ACount > 0) and (MiddleX < BufferWidth) then
  begin
    MiddleLine[1] := Buffers[1][MiddleX];
    if AutoSourceBorderColor then borderColor := MiddleLine[1]
    else borderColor := FSourceBorderColor;

    if Buffers[0] = nil then TopLine[0] := borderColor else TopLine[0] := Buffers[0][MiddleX-1];
    MiddleLine[0] := Buffers[1][MiddleX-1];
    if Buffers[2] = nil then BottomLine[0] := borderColor else BottomLine[0] := Buffers[2][MiddleX-1];
    if Buffers[0] = nil then TopLine[1] := borderColor else TopLine[1] := Buffers[0][MiddleX];
    if Buffers[2] = nil then BottomLine[1] := borderColor else BottomLine[1] := Buffers[2][MiddleX];
    inc(MiddleX);
    if MiddleX >= BufferWidth then
    begin
      TopLine[2] := borderColor;
      MiddleLine[2] := borderColor;
      BottomLine[2] := borderColor;
    end else
    begin
      if Buffers[0] = nil then TopLine[2] := borderColor else TopLine[2] := Buffers[0][MiddleX];
      MiddleLine[2] := Buffers[1][MiddleX];
      if Buffers[2] = nil then BottomLine[2] := borderColor else BottomLine[2] := Buffers[2][MiddleX];
    end;
    ADest^ := DoFilter3X3(@TopLine,@MiddleLine,@BottomLine);
    Dec(ACount);
    Inc(ADest);
  end;
  while (ACount > 0) do
  begin
    ADest^ := FDestinationBorderColor;
    Dec(ACount);
    Inc(ADest);
  end;
end;

constructor TBGRA3X3FilterScanner.Create(ASource: IBGRAScanner;
  ABounds: TRect);
begin
  inherited Create(ASource,ABounds,Point(-1,-1),3,3);
  FSourceBorderColor := BGRAPixelTransparent;
  FDestinationBorderColor := BGRAPixelTransparent;
  FAutoSourceBorderColor := False;
end;

constructor TBGRA3X3FilterScanner.Create(ASource: TBGRACustomBitmap);
begin
  inherited Create(ASource,Rect(0,0,ASource.Width,ASource.Height),Point(-1,-1),3,3);
  FSourceBorderColor := BGRAPixelTransparent;
  FDestinationBorderColor := BGRAPixelTransparent;
  FAutoSourceBorderColor := False;
  AllowDirectRead := true;
end;

{ TBGRASharpenScanner }

function TBGRASharpenScanner.DoFilter3X3(PTop, PMiddle, PBottom: PBGRAPixel): TBGRAPixel;
var
  sumR, sumG, sumB, sumA, nbA: NativeUInt;
  refPixel: TBGRAPixel;
  rgbDivShr1: NativeUint;
begin
  if FAmount = 0 then
  begin
    result := PMiddle[1];
    exit;
  end;
  //compute sum
  sumR   := 0;
  sumG   := 0;
  sumB   := 0;
  sumA   := 0;
  //RGBdiv := 0;
  nbA    := 0;

  {$hints off}
  with PTop[0] do if alpha <> 0 then begin sumR += red * alpha; sumG += green * alpha; sumB += blue * alpha; sumA += alpha; inc(nbA); end;
  with PTop[1] do if alpha <> 0 then begin sumR += red * alpha; sumG += green * alpha; sumB += blue * alpha; sumA += alpha; inc(nbA); end;
  with PTop[2] do if alpha <> 0 then begin sumR += red * alpha; sumG += green * alpha; sumB += blue * alpha; sumA += alpha; inc(nbA); end;
  with PMiddle[0] do if alpha <> 0 then begin sumR += red * alpha; sumG += green * alpha; sumB += blue * alpha; sumA += alpha; inc(nbA); end;
  with PMiddle[2] do if alpha <> 0 then begin sumR += red * alpha; sumG += green * alpha; sumB += blue * alpha; sumA += alpha; inc(nbA); end;
  with PBottom[0] do if alpha <> 0 then begin sumR += red * alpha; sumG += green * alpha; sumB += blue * alpha; sumA += alpha; inc(nbA); end;
  with PBottom[1] do if alpha <> 0 then begin sumR += red * alpha; sumG += green * alpha; sumB += blue * alpha; sumA += alpha; inc(nbA); end;
  with PBottom[2] do if alpha <> 0 then begin sumR += red * alpha; sumG += green * alpha; sumB += blue * alpha; sumA += alpha; inc(nbA); end;
   {$hints on}

  //we finally have an average pixel
  if (sumA = 0) then
    refPixel := BGRAPixelTransparent
  else
  begin
    rgbDivShr1:= sumA shr 1;
    refPixel.red   := (sumR + rgbDivShr1) div sumA;
    refPixel.green := (sumG + rgbDivShr1) div sumA;
    refPixel.blue  := (sumB + rgbDivShr1) div sumA;
    refPixel.alpha := (sumA + nbA shr 1) div nbA;
  end;

  //read the pixel at the center of the square
  if refPixel <> BGRAPixelTransparent then
  begin
    with PMiddle[1] do
    begin
      //compute sharpened pixel by adding the difference
      if FAmount<>256 then
        result := BGRA( max(0, min($FFFF, Int32or64(red shl 8) +
          FAmount*(red - refPixel.red))) shr 8,
            max(0, min($FFFF, Int32or64(green shl 8) +
          FAmount*(green - refPixel.green))) shr 8,
           max(0, min($FFFF, Int32or64(blue shl 8) +
          FAmount*(blue - refPixel.blue))) shr 8,
           max(0, min($FFFF, Int32or64(alpha shl 8) +
          FAmount*(alpha - refPixel.alpha))) shr 8 )
      else
        result := BGRA( max(0, min(255, (red shl 1) - refPixel.red)),
           max(0, min(255, (green shl 1) - refPixel.green)),
           max(0, min(255, (blue shl 1) - refPixel.blue)),
           max(0, min(255, (alpha shl 1) - refPixel.alpha)));
    end;
  end else
    result := PMiddle[1];
end;

constructor TBGRASharpenScanner.Create(ASource: IBGRAScanner;
  ABounds: TRect; AAmount: integer);
begin
  inherited Create(ASource,ABounds);
  FAmount:= AAmount;
end;

constructor TBGRASharpenScanner.Create(ASource: TBGRACustomBitmap;
  AAmount: integer);
begin
  inherited Create(ASource);
  FAmount:= AAmount;
end;

{ TBGRAContourScanner }

function TBGRAContourScanner.DoFilter3X3(PTop, PMiddle, PBottom: PBGRAPixel): TBGRAPixel;
var
  sum: NativeInt;
  slope: byte;
begin
  if FGammaCorrection then
  begin
    sum := (FastBGRAExpandedDiff(PTop[0],PBottom[2]) + FastBGRAExpandedDiff(PTop[1],PBottom[1]) +
        FastBGRAExpandedDiff(PTop[2],PBottom[0]) + FastBGRAExpandedDiff(PMiddle[0],PMiddle[2])) div 3;

    if sum >= 65535 then
      slope := 0
    else if sum <= 0 then
      slope := 255
    else slope := GammaCompressionTab[65535-sum];
  end else
  begin
      sum := (FastBGRALinearDiff(PTop[0],PBottom[2]) + FastBGRALinearDiff(PTop[1],PBottom[1]) +
        FastBGRALinearDiff(PTop[2],PBottom[0]) + FastBGRALinearDiff(PMiddle[0],PMiddle[2])) div 3;

    if sum >= 255 then
      slope := 0
    else if sum < 0 then
      slope := 255
    else slope := 255-sum;
  end;
  result.red := slope;
  result.green := slope;
  result.blue := slope;
  result.alpha := FOpacity;
end;

constructor TBGRAContourScanner.Create(ASource: IBGRAScanner;
  ABounds: TRect; AGammaCorrection: boolean);
begin
  inherited Create(ASource,ABounds);
  FGammaCorrection := AGammaCorrection;
  AutoSourceBorderColor:= True;
  FOpacity:= 255;
end;

constructor TBGRAContourScanner.Create(ASource: TBGRACustomBitmap;
  AGammaCorrection: boolean);
begin
  inherited Create(ASource);
  FGammaCorrection := AGammaCorrection;
  AutoSourceBorderColor:= True;
  FOpacity:= 255;
end;

{ TBGRAFilterScannerNormalize }

procedure TBGRAFilterScannerNormalize.DetermineNormalizationFactors(ABounds: TRect; AEachChannel: boolean);
var
  buffer: TBGRAPixelBuffer;
  p: PBGRAPixel;
  c: TExpandedPixel;
  yb, xb: LongInt;
begin
  if (ABounds.Right <= ABounds.Left) or (ABounds.Bottom <= ABounds.Top) then
  begin
    addValRed := 0;
    addValGreen := 0;
    addValBlue := 0;
    addAlpha := 0;
    factorValRed := 4096;
    factorValGreen := 4096;
    factorValBlue := 4096;
    factorAlpha := 4096;
    exit;
  end;
  maxValRed := 0;
  minValRed := 65535;
  maxValGreen := 0;
  minValGreen := 65535;
  maxValBlue := 0;
  minValBlue := 65535;
  maxAlpha  := 0;
  minAlpha  := 65535;
  buffer := nil;
  for yb := ABounds.Top to ABounds.Bottom do
  begin
    if Source.ProvidesScanline(rect(ABounds.Left,yb,ABounds.Right,yb+1)) then
      p := Source.GetScanlineAt(ABounds.Left,yb)
    else
    begin
      Source.ScanMoveTo(ABounds.Left,yb);
      AllocateBGRAPixelBuffer(buffer, ABounds.Right-ABounds.Left);
      p := @buffer[0];
      ScannerPutPixels(Source,p,ABounds.Right-ABounds.Left,dmSet);
    end;
    for xb := ABounds.Right-ABounds.Left-1 downto 0 do
    begin
      c := GammaExpansion(p[xb]);
      if c.red > maxValRed then
        maxValRed := c.red;
      if c.green > maxValGreen then
        maxValGreen := c.green;
      if c.blue > maxValBlue then
        maxValBlue := c.blue;
      if c.red < minValRed then
        minValRed := c.red;
      if c.green < minValGreen then
        minValGreen := c.green;
      if c.blue < minValBlue then
        minValBlue := c.blue;
      if c.alpha > maxAlpha then
        maxAlpha := c.alpha;
      if c.alpha < minAlpha then
        minAlpha := c.alpha;
    end;
  end;
  if not AEachChannel then
  begin
    minValRed   := min(min(minValRed, minValGreen), minValBlue);
    maxValRed   := max(max(maxValRed, maxValGreen), maxValBlue);
    minValGreen := minValRed;
    maxValGreen := maxValRed;
    minValBlue  := minValBlue;
    maxValBlue  := maxValBlue;
  end;
  if maxValRed > minValRed then
  begin
    factorValRed := 268431360 div (maxValRed - minValRed);
    addValRed    := 0;
  end else
  begin
    factorValRed := 0;
    if minValRed = 0 then
      addValRed := 0
    else addValRed := 65535;
  end;
  if maxValGreen > minValGreen then
  begin
    factorValGreen := 268431360 div (maxValGreen - minValGreen);
    addValGreen    := 0;
  end else
  begin
    factorValGreen := 0;
    if minValGreen = 0 then
      addValGreen := 0
    else addValGreen := 65535;
  end;
  if maxValBlue > minValBlue then
  begin
    factorValBlue := 268431360 div (maxValBlue - minValBlue);
    addValBlue    := 0;
  end else
  begin
    factorValBlue := 0;
    if minValBlue = 0 then
      addValBlue := 0 else
      addValBlue := 65535;
  end;
  if maxAlpha > minAlpha then
  begin
    factorAlpha := 268431360 div (maxAlpha - minAlpha);
    addAlpha    := 0;
  end else
  begin
    factorAlpha := 0;
    if minAlpha = 0 then
      addAlpha := 0 else
      addAlpha := 65535;
  end;
end;

procedure TBGRAFilterScannerNormalize.DoComputeFilterAt(ASource: PBGRAPixel;
  ADest: PBGRAPixel; ACount: integer; AGammaCorrection: boolean);
var
  c: TExpandedPixel;
begin
  While ACount > 0 do
  begin
    c := GammaExpansion(ASource^);
    Inc(ASource);
    c.red   := ((c.red - minValRed) * factorValRed + 2047) shr 12 + addValRed;
    c.green := ((c.green - minValGreen) * factorValGreen + 2047) shr 12 + addValGreen;
    c.blue  := ((c.blue - minValBlue) * factorValBlue + 2047) shr 12 + addValBlue;
    c.alpha := ((c.alpha - minAlpha) * factorAlpha + 2047) shr 12 + addAlpha;
    ADest^  := GammaCompression(c);
    Inc(ADest);
    dec(ACount);
  end;
end;

constructor TBGRAFilterScannerNormalize.Create(ASource: IBGRAScanner;
  AOffset: TPoint; ABounds: TRect; AEachChannel: boolean);
begin
  inherited Create(ASource,AOffset,True);
  DetermineNormalizationFactors(ABounds, AEachChannel);
end;

class procedure TBGRAFilterScannerNormalize.ComputeFilterAt(
  ASource: PBGRAPixel; ADest: PBGRAPixel; ACount: integer;
  AGammaCorrection: boolean);
begin
  raise exception.Create('Normalize filter scanner cannot be called as a class procedure');
end;

{ TBGRAFilterScannerSwapRedBlue }

class procedure TBGRAFilterScannerSwapRedBlue.ComputeFilterAt(
  ASource: PBGRAPixel; ADest: PBGRAPixel; ACount: integer;
  AGammaCorrection: boolean);
const RedMask = 255 shl TBGRAPixel_RedShift;
      BlueMask = 255 shl TBGRAPixel_BlueShift;
      GreenAndAlphaMask = (255 shl TBGRAPixel_GreenShift) or (255 shl TBGRAPixel_AlphaShift);
      RedMask64 = RedMask or (RedMask shl 32);
      BlueMask64 = BlueMask or (BlueMask shl 32);
      GreenAndAlphaMask64 = GreenAndAlphaMask or (GreenAndAlphaMask shl 32);
var
  temp: longword;
  temp64: QWord;
  oddN: boolean;
begin
  {$PUSH}{$WARNINGS OFF}
  if ACount <= 0 then exit;
  oddN := odd(ACount);
  ACount := ACount shr 1;
  if TBGRAPixel_RedShift > TBGRAPixel_BlueShift then
    while ACount > 0 do
    begin
      temp64 := PQWord(ASource)^;
      PQWord(ADest)^ := ((temp64 and BlueMask64) shl (TBGRAPixel_RedShift-TBGRAPixel_BlueShift)) or
                    ((temp64 and RedMask64) shr (TBGRAPixel_RedShift-TBGRAPixel_BlueShift)) or
                    (temp64 and GreenAndAlphaMask64);
      dec(ACount);
      inc(ASource,2);
      inc(ADest,2);
    end else
    while ACount > 0 do
    begin
      temp64 := PQWord(ASource)^;
      PQWord(ADest)^ := ((temp64 and BlueMask64) shr (TBGRAPixel_BlueShift-TBGRAPixel_RedShift)) or
                    ((temp64 and RedMask64) shl (TBGRAPixel_BlueShift-TBGRAPixel_RedShift)) or
                    (temp64 and GreenAndAlphaMask64);
      dec(ACount);
      inc(ASource,2);
      inc(ADest,2);
    end;
  if oddN then
  begin
    if TBGRAPixel_RedShift > TBGRAPixel_BlueShift then
    begin
      temp := PDWord(ASource)^;
      PDWord(ADest)^ := ((temp and BlueMask) shl (TBGRAPixel_RedShift-TBGRAPixel_BlueShift)) or
            ((temp and RedMask) shr (TBGRAPixel_RedShift-TBGRAPixel_BlueShift)) or
            (temp and GreenAndAlphaMask);
    end else
    begin
      temp := PDWord(ASource)^;
      PDWord(ADest)^ := ((temp and BlueMask) shr (TBGRAPixel_BlueShift-TBGRAPixel_RedShift)) or
            ((temp and RedMask) shl (TBGRAPixel_BlueShift-TBGRAPixel_RedShift)) or
            (temp and GreenAndAlphaMask);
    end;
  end;
  {$POP}
end;

{ TBGRAFilterScannerNegative }

class procedure TBGRAFilterScannerNegative.ComputeFilterAt(
  ASource: PBGRAPixel; ADest: PBGRAPixel; ACount: integer;
  AGammaCorrection: boolean);
begin
  if ADest = ASource then
  begin
    if AGammaCorrection then
      while ACount > 0 do
      begin
        with ADest^ do
          if alpha <> 0 then
          begin
            ADest^.red := GammaCompressionTab[not GammaExpansionTab[red]];
            ADest^.green := GammaCompressionTab[not GammaExpansionTab[green]];
            ADest^.blue := GammaCompressionTab[not GammaExpansionTab[blue]];
          end;
        Inc(ADest);
        dec(ACount);
      end else
      while ACount > 0 do
      begin
        if ADest^.alpha <> 0 then
          DWord(ADest^) := DWord(ADest^) xor (not ($ff shl TBGRAPixel_AlphaShift));
        Inc(ADest);
        dec(ACount);
      end;
  end else
    if AGammaCorrection then
      while ACount > 0 do
      begin
        with ASource^ do
          if alpha = 0 then
            ADest^ := BGRAPixelTransparent
          else
          begin
            ADest^.red := GammaCompressionTab[not GammaExpansionTab[red]];
            ADest^.green := GammaCompressionTab[not GammaExpansionTab[green]];
            ADest^.blue := GammaCompressionTab[not GammaExpansionTab[blue]];
            ADest^.alpha := alpha;
          end;
        inc(ASource);
        Inc(ADest);
        dec(ACount);
      end else
      while ACount > 0 do
      begin
        if ASource^.alpha = 0 then
          ADest^ := BGRAPixelTransparent
        else
          DWord(ADest^) := DWord(ASource^) xor (not ($ff shl TBGRAPixel_AlphaShift));
        inc(ASource);
        Inc(ADest);
        dec(ACount);
      end;
end;

{ TBGRAFilterScannerGrayscale }

class procedure TBGRAFilterScannerGrayscale.ComputeFilterAt(
  ASource: PBGRAPixel; ADest: PBGRAPixel; ACount: integer;
  AGammaCorrection: boolean);
begin
  if ASource = ADest then
  begin
    if AGammaCorrection then
      while ACount > 0 do
      begin
        if ADest^.alpha <> 0 then
          ADest^ := BGRAToGrayscale(ADest^);
        Inc(ADest);
        dec(ACount);
      end else
      while ACount > 0 do
      begin
        if ADest^.alpha <> 0 then
          ADest^ := BGRAToGrayscaleLinear(ADest^);
        Inc(ADest);
        dec(ACount);
      end;
  end else
    if AGammaCorrection then
      while ACount > 0 do
      begin
        ADest^ := BGRAToGrayscale(ASource^);
        inc(ASource);
        Inc(ADest);
        dec(ACount);
      end else
      while ACount > 0 do
      begin
        ADest^ := BGRAToGrayscaleLinear(ASource^);
        inc(ASource);
        Inc(ADest);
        dec(ACount);
      end;
end;

end.

