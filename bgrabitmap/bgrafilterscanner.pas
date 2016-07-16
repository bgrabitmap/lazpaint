unit BGRAFilterScanner;

{$mode objfpc}{$H+}

interface

uses
  Classes, BGRABitmapTypes;

const
    FilterScannerChunkSize = 16;

type
  TBGRAPixelBuffer = packed array of TBGRAPixel;

  { TBGRAFilterScanner }

  TBGRAFilterScanner = class(TBGRACustomScanner)
  private
    FCurX,FCurY: integer;
    FSource: IBGRAScanner;
    FOffset: TPoint;
    FVariablePixelBuffer: TBGRAPixelBuffer;
    FPixelBuffer: packed array[0..FilterScannerChunkSize-1] of TBGRAPixel;
    FPixelBufferPos: integer;
  protected
    procedure AllocateBuffer(var ABuffer: TBGRAPixelBuffer; ASize: integer);
  public
    constructor Create(ASource: IBGRAScanner; AOffset: TPoint);
    procedure ComputeFilter(ASource: IBGRAScanner; X,Y: Integer; ADest: PBGRAPixel; ACount: integer); virtual; abstract;
    function ScanAtInteger(X,Y: integer): TBGRAPixel; override;
    procedure ScanMoveTo(X,Y: Integer); override;
    function ScanNextPixel: TBGRAPixel; override;
    procedure ScanPutPixels(pdest: PBGRAPixel; count: integer; mode: TDrawMode); override;
    function IsScanPutPixelsDefined: boolean; override;
    function ScanAt(X,Y: Single): TBGRAPixel; override;
    property Source: IBGRAScanner read FSource;
    property Offset: TPoint read FOffset;
  end;

  { TBGRAFilterScannerPixelwise }

  TBGRAFilterScannerPixelwise = class(TBGRAFilterScanner)
    private
      FBuffer: TBGRAPixelBuffer;
      FGammaCorrection: boolean;
    protected
      procedure DoComputeFilterAt(ASource: PBGRAPixel; ADest: PBGRAPixel;
              ACount: integer; AGammaCorrection: boolean); virtual;
    public
      constructor Create(ASource: IBGRAScanner; AOffset: TPoint; AGammaCorrection: boolean = true);
      procedure ComputeFilter(ASource: IBGRAScanner; X, Y: Integer; ADest: PBGRAPixel;
        ACount: integer); override;
      class procedure ComputeFilterAt(ASource: PBGRAPixel; ADest: PBGRAPixel;
        ACount: integer; AGammaCorrection: boolean); virtual; abstract;
      class procedure ComputeFilterInplace(ABitmap: TBGRACustomBitmap; ABounds: TRect;
        AGammaCorrection: boolean);
      property GammaCorrection: boolean read FGammaCorrection write FGammaCorrection;
    end;

  { TBGRAFilterScannerGrayscale }

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

implementation

uses BGRABlend, math, SysUtils;

{ TBGRAFilterScannerNormalize }

procedure TBGRAFilterScannerNormalize.DetermineNormalizationFactors(ABounds: TRect; AEachChannel: boolean);
var
  buffer: TBGRAPixelBuffer;
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
  AllocateBuffer(buffer, ABounds.Right-ABounds.Left);
  for yb := ABounds.Top to ABounds.Bottom do
  begin
    Source.ScanMoveTo(ABounds.Left,yb);
    ScannerPutPixels(Source,@buffer[0],ABounds.Right-ABounds.Left,dmSet);
    for xb := ABounds.Right-ABounds.Left-1 downto 0 do
    begin
      c := GammaExpansion(buffer[xb]);
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
  raise exception.Create('Normalize filter cannot be called as a class procedure');
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

{ TBGRAFilterScannerPixelwise }

procedure TBGRAFilterScannerPixelwise.DoComputeFilterAt(ASource: PBGRAPixel;
  ADest: PBGRAPixel; ACount: integer; AGammaCorrection: boolean);
begin
  ComputeFilterAt(ASource,ADest,ACount,AGammaCorrection);
end;

constructor TBGRAFilterScannerPixelwise.Create(ASource: IBGRAScanner;
  AOffset: TPoint; AGammaCorrection: boolean);
begin
  inherited Create(ASource,AOffset);
  GammaCorrection := AGammaCorrection;
end;

procedure TBGRAFilterScannerPixelwise.ComputeFilter(ASource: IBGRAScanner; X,
  Y: Integer; ADest: PBGRAPixel; ACount: integer);
begin
  AllocateBuffer(FBuffer, ACount);
  ASource.ScanMoveTo(X,Y);
  ASource.ScanPutPixels(@FBuffer[0], ACount, dmSet);
  DoComputeFilterAt(@FBuffer[0],ADest,ACount,GammaCorrection);
end;

class procedure TBGRAFilterScannerPixelwise.ComputeFilterInplace(
  ABitmap: TBGRACustomBitmap; ABounds: TRect; AGammaCorrection: boolean);
var
  yb: LongInt;
  p: Pointer;
begin
  ABitmap.LoadFromBitmapIfNeeded;
  if (ABounds.Left = 0) and (ABounds.Top = 0) and
     (ABounds.Right = ABitmap.Width) and (ABounds.Bottom = ABitmap.Height) then
    ComputeFilterAt(ABitmap.Data,ABitmap.Data,ABitmap.NbPixels,AGammaCorrection)
  else
    for yb := ABounds.Top to ABounds.Bottom-1 do
    begin
      p := ABitmap.ScanLine[yb]+ABounds.Left;
      ComputeFilterAt(p,p,ABounds.Right-ABounds.Left,AGammaCorrection);
    end;
  ABitmap.InvalidateBitmap;
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

{ TBGRAFilterScanner }

procedure TBGRAFilterScanner.AllocateBuffer(var ABuffer: TBGRAPixelBuffer;
  ASize: integer);
begin
  if ASize > length(ABuffer) then
    setlength(ABuffer, max(length(ABuffer)*2,ASize));
end;

constructor TBGRAFilterScanner.Create(ASource: IBGRAScanner; AOffset: TPoint);
begin
  FSource := ASource;
  FOffset := AOffset;
  FPixelBufferPos := FilterScannerChunkSize;
end;

function TBGRAFilterScanner.ScanAtInteger(X, Y: integer): TBGRAPixel;
begin
  ScanMoveTo(X,Y);
  result := ScanNextPixel;
end;

procedure TBGRAFilterScanner.ScanMoveTo(X, Y: Integer);
begin
  FCurX := X;
  FCurY := Y;
  FPixelBufferPos := FilterScannerChunkSize;
end;

function TBGRAFilterScanner.ScanNextPixel: TBGRAPixel;
begin
  if FPixelBufferPos >= FilterScannerChunkSize then
  begin
    ComputeFilter(FSource,FCurX+FOffset.X,FCurY+FOffset.Y,@FPixelBuffer[0],FilterScannerChunkSize);
    FPixelBufferPos := 0;
  end;
  Result:= FPixelBuffer[FPixelBufferPos];
  inc(FPixelBufferPos);
  inc(FCurX);
end;

procedure TBGRAFilterScanner.ScanPutPixels(pdest: PBGRAPixel; count: integer;
  mode: TDrawMode);
begin
  if mode = dmSet then
  begin
    ComputeFilter(FSource,FCurX+FOffset.X,FCurY+FOffset.Y,pdest,count);
    inc(FCurX,count);
  end else
  begin
    AllocateBuffer(FVariablePixelBuffer, count);
    ComputeFilter(FSource,FCurX+FOffset.X,FCurY+FOffset.Y,@FVariablePixelBuffer[0],count);
    inc(FCurX,count);
    PutPixels(pdest, @FVariablePixelBuffer[0], count, mode, 255);
  end;
end;

function TBGRAFilterScanner.IsScanPutPixelsDefined: boolean;
begin
  Result:= true;
end;

function TBGRAFilterScanner.ScanAt(X, Y: Single): TBGRAPixel;
begin
  result := ScanAtInteger(round(X),round(Y));
end;

end.

