unit BGRAGradientScanner;

{$mode objfpc}{$H+}

interface

{ This unit contains scanners that generate gradients }

uses
  Classes, SysUtils, BGRABitmapTypes, BGRATransform;

type
  { TBGRASimpleGradientWithoutGammaCorrection }

  TBGRASimpleGradientWithoutGammaCorrection = class(TBGRACustomGradient)
  private
    FColor1,FColor2: TBGRAPixel;
  public
    constructor Create(Color1,Color2: TBGRAPixel);
    function GetColorAt(position: integer): TBGRAPixel; override;
    function GetColorAtF(position: single): TBGRAPixel; override;
    function GetAverageColor: TBGRAPixel; override;
    function GetMonochrome: boolean; override;
  end;

  { TBGRASimpleGradientWithGammaCorrection }

  TBGRASimpleGradientWithGammaCorrection = class(TBGRACustomGradient)
  private
    FColor1,FColor2: TBGRAPixel;
    ec1,ec2: TExpandedPixel;
  public
    constructor Create(Color1,Color2: TBGRAPixel);
    function GetColorAt(position: integer): TBGRAPixel; override;
    function GetColorAtF(position: single): TBGRAPixel; override;
    function GetAverageColor: TBGRAPixel; override;
    function GetMonochrome: boolean; override;
  end;

  THueGradientOption = (hgoRepeat, hgoPositiveDirection, hgoNegativeDirection, hgoHueCorrection, hgoLightnessCorrection);
  THueGradientOptions = set of THueGradientOption;

  { TBGRAHueGradient }

  TBGRAHueGradient = class(TBGRACustomGradient)
  private
    FColor1,FColor2: TBGRAPixel;
    hsla1,hsla2: THSLAPixel;
    hue1,hue2: longword;
    FOptions: THueGradientOptions;
    procedure Init(c1,c2: THSLAPixel; AOptions: THueGradientOptions);
  public
    constructor Create(Color1,Color2: TBGRAPixel; options: THueGradientOptions); overload;
    constructor Create(Color1,Color2: THSLAPixel; options: THueGradientOptions); overload;
    constructor Create(AHue1,AHue2: Word; Saturation,Lightness: Word; options: THueGradientOptions); overload;
    function GetColorAt(position: integer): TBGRAPixel; override;
    function GetColorAtF(position: single): TBGRAPixel; override;
    function GetAverageColor: TBGRAPixel; override;
    function GetMonochrome: boolean; override;
  end;

  { TBGRAMultiGradient }

  TBGRAMultiGradient = class(TBGRACustomGradient)
  private
    FColors: array of TBGRAPixel;
    FPositions: array of integer;
    FPositionsF: array of single;
    FEColors: array of TExpandedPixel;
    FCycle: Boolean;
    procedure Init(Colors: array of TBGRAPixel; Positions0To1: array of single; AGammaCorrection, ACycle: boolean);
  public
    GammaCorrection: boolean;
    constructor Create(Colors: array of TBGRAPixel; Positions0To1: array of single; AGammaCorrection: boolean; ACycle: boolean = false);
    function GetColorAt(position: integer): TBGRAPixel; override;
    function GetAverageColor: TBGRAPixel; override;
    function GetMonochrome: boolean; override;
  end;

  { TBGRAGradientScanner }

  TBGRAGradientScanner = class(TBGRACustomScanner)
  protected
    FGradientType: TGradientType;
    FOrigin1,FOrigin2: TPointF;
    FSinus: Boolean;
    u: TPointF;
    len,aFactor,aFactorF: single;
    mergedColor: TBGRAPixel;
    FGradient: TBGRACustomGradient;
    FGradientOwner: boolean;
    FHorizColor: TBGRAPixel;
    FVertical: boolean;
    FDotProduct,FDotProductPerp: Single;
    procedure Init(gtype: TGradientType; o1, o2: TPointF; Sinus: Boolean=False);
    procedure InitScanInline(x,y: integer);
    function ScanNextInline: TBGRAPixel; inline;
  public
    constructor Create(c1, c2: TBGRAPixel; gtype: TGradientType; o1, o2: TPointF;
                       gammaColorCorrection: boolean = True; Sinus: Boolean=False);
    constructor Create(gradient: TBGRACustomGradient; gtype: TGradientType; o1, o2: TPointF; Sinus: Boolean=False; AGradientOwner: Boolean=False);
    destructor Destroy; override;
    procedure ScanMoveTo(X, Y: Integer); override;
    function ScanNextPixel: TBGRAPixel; override;
    function ScanAt(X, Y: Single): TBGRAPixel; override;
    procedure ScanPutPixels(pdest: PBGRAPixel; count: integer; mode: TDrawMode); override;
    function IsScanPutPixelsDefined: boolean; override;
  end;

  { TBGRAConstantScanner }

  TBGRAConstantScanner = class(TBGRAGradientScanner)
    constructor Create(c: TBGRAPixel);
  end;

  { TBGRARandomScanner }

  TBGRARandomScanner = class(TBGRACustomScanner)
  private
    FOpacity: byte;
    FGrayscale: boolean;
  public
    constructor Create(AGrayscale: Boolean; AOpacity: byte);
    function ScanAtInteger({%H-}X, {%H-}Y: integer): TBGRAPixel; override;
    function ScanNextPixel: TBGRAPixel; override;
    function ScanAt({%H-}X, {%H-}Y: Single): TBGRAPixel; override;
  end;

  { TBGRAGradientTriangleScanner }

  TBGRAGradientTriangleScanner= class(TBGRACustomScanner)
  protected
    FMatrix: TAffineMatrix;
    FColor1,FDiff2,FDiff3,FStep: TColorF;
    FCurColor: TColorF;
  public
    constructor Create(pt1,pt2,pt3: TPointF; c1,c2,c3: TBGRAPixel);
    procedure ScanMoveTo(X,Y: Integer); override;
    procedure ScanMoveToF(X,Y: Single);
    function ScanAt(X,Y: Single): TBGRAPixel; override;
    function ScanNextPixel: TBGRAPixel; override;
  end;

  { TBGRASolidColorMaskScanner }

  TBGRASolidColorMaskScanner = class(TBGRACustomScanner)
  private
    FOffset: TPoint;
    FMask: TBGRACustomBitmap;
    FSolidColor: TBGRAPixel;
    FScanNext : TScanNextPixelFunction;
    FScanAt : TScanAtFunction;
  public
    constructor Create(AMask: TBGRACustomBitmap; AOffset: TPoint; ASolidColor: TBGRAPixel);
    destructor Destroy; override;
    function IsScanPutPixelsDefined: boolean; override;
    procedure ScanPutPixels(pdest: PBGRAPixel; count: integer; mode: TDrawMode); override;
    procedure ScanMoveTo(X,Y: Integer); override;
    function ScanNextPixel: TBGRAPixel; override;
    function ScanAt(X,Y: Single): TBGRAPixel; override;
    property Color: TBGRAPixel read FSolidColor write FSolidColor;
  end;

  { TBGRATextureMaskScanner }

  TBGRATextureMaskScanner = class(TBGRACustomScanner)
  private
    FOffset: TPoint;
    FMask: TBGRACustomBitmap;
    FTexture: IBGRAScanner;
    FMaskScanNext,FTextureScanNext : TScanNextPixelFunction;
    FMaskScanAt,FTextureScanAt : TScanAtFunction;
    FGlobalOpacity: Byte;
  public
    constructor Create(AMask: TBGRACustomBitmap; AOffset: TPoint; ATexture: IBGRAScanner; AGlobalOpacity: Byte = 255);
    destructor Destroy; override;
    function IsScanPutPixelsDefined: boolean; override;
    procedure ScanPutPixels(pdest: PBGRAPixel; count: integer; mode: TDrawMode); override;
    procedure ScanMoveTo(X,Y: Integer); override;
    function ScanNextPixel: TBGRAPixel; override;
    function ScanAt(X,Y: Single): TBGRAPixel; override;
  end;

  { TBGRAOpacityScanner }

  TBGRAOpacityScanner = class(TBGRACustomScanner)
  private
      FTexture: IBGRAScanner;
      FGlobalOpacity: Byte;
      FScanNext : TScanNextPixelFunction;
      FScanAt : TScanAtFunction;
  public
    constructor Create(ATexture: IBGRAScanner; AGlobalOpacity: Byte = 255);
    destructor Destroy; override;
    function IsScanPutPixelsDefined: boolean; override;
    procedure ScanPutPixels(pdest: PBGRAPixel; count: integer; mode: TDrawMode); override;
    procedure ScanMoveTo(X,Y: Integer); override;
    function ScanNextPixel: TBGRAPixel; override;
    function ScanAt(X,Y: Single): TBGRAPixel; override;
  end;

implementation

uses BGRABlend;

{ TBGRAConstantScanner }

constructor TBGRAConstantScanner.Create(c: TBGRAPixel);
begin
  inherited Create(c,c,gtLinear,PointF(0,0),PointF(0,0),false);
end;

{ TBGRARandomScanner }

constructor TBGRARandomScanner.Create(AGrayscale: Boolean; AOpacity: byte);
begin
  FGrayscale:= AGrayscale;
  FOpacity:= AOpacity;
end;

function TBGRARandomScanner.ScanAtInteger(X, Y: integer): TBGRAPixel;
begin
  Result:=ScanNextPixel;
end;

function TBGRARandomScanner.ScanNextPixel: TBGRAPixel;
begin
  if FGrayscale then
  begin
    result.red := random(256);
    result.green := result.red;
    result.blue := result.red;
    result.alpha:= FOpacity;
  end else
    Result:= BGRA(random(256),random(256),random(256),FOpacity);
end;

function TBGRARandomScanner.ScanAt(X, Y: Single): TBGRAPixel;
begin
  Result:=ScanNextPixel;
end;

{ TBGRAHueGradient }

procedure TBGRAHueGradient.Init(c1, c2: THSLAPixel; AOptions: THueGradientOptions);
begin
  FColor1 := HSLAToBGRA(c1);
  FColor2 := HSLAToBGRA(c2);
  FOptions:= AOptions;
  if (hgoLightnessCorrection in AOptions) then
  begin
    hsla1 := BGRAToGSBA(FColor1);
    hsla2 := BGRAToGSBA(FColor2);
  end else
  begin
    hsla1 := c1;
    hsla2 := c2;
  end;
  if not (hgoHueCorrection in AOptions) then
  begin
    hue1 := c1.hue;
    hue2 := c2.hue;
  end else
  begin
    hue1 := HtoG(c1.hue);
    hue2 := HtoG(c2.hue);
  end;
  if (hgoPositiveDirection in AOptions) and not (hgoNegativeDirection in AOptions) then
  begin
    if c2.hue <= c1.hue then hue2 += 65536;
  end else
  if not (hgoPositiveDirection in AOptions) and (hgoNegativeDirection in AOptions) then
  begin
    if c2.hue >= c1.hue then hue1 += 65536;
  end;
end;

constructor TBGRAHueGradient.Create(Color1, Color2: TBGRAPixel;options: THueGradientOptions);
begin
  Init(BGRAToHSLA(Color1),BGRAToHSLA(Color2),options);
end;

constructor TBGRAHueGradient.Create(Color1, Color2: THSLAPixel; options: THueGradientOptions);
begin
  Init(Color1,Color2, options);
end;

constructor TBGRAHueGradient.Create(AHue1, AHue2: Word; Saturation,
  Lightness: Word; options: THueGradientOptions);
begin
  Init(HSLA(AHue1,saturation,lightness), HSLA(AHue2,saturation,lightness), options);
end;

function TBGRAHueGradient.GetColorAt(position: integer): TBGRAPixel;
var b,b2: cardinal;
    interm: THSLAPixel;
begin
  if hgoRepeat in FOptions then
  begin
    position := position and $ffff;
    if position = 0 then
    begin
      result := FColor1;
      exit;
    end;
  end else
  begin
    if position <= 0 then
    begin
      result := FColor1;
      exit
    end else
    if position >= 65536 then
    begin
      result := FColor2;
      exit
    end;
  end;
  b      := position shr 2;
  b2     := 16384-b;
  interm.hue := ((hue1 * b2 + hue2 * b + 8191) shr 14) and $ffff;
  interm.saturation := (hsla1.saturation * b2 + hsla2.saturation * b + 8191) shr 14;
  interm.lightness := (hsla1.lightness * b2 + hsla2.lightness * b + 8191) shr 14;
  interm.alpha := (hsla1.alpha * b2 + hsla2.alpha * b + 8191) shr 14;
  if hgoLightnessCorrection in FOptions then
  begin
    if not (hgoHueCorrection in FOptions) then
      interm.hue := HtoG(interm.hue);
    result := GSBAToBGRA(interm);
  end else
  begin
    if hgoHueCorrection in FOptions then
      interm.hue := GtoH(interm.hue);
    result := HSLAToBGRA(interm);
  end;
end;

function TBGRAHueGradient.GetColorAtF(position: single): TBGRAPixel;
var b,b2: cardinal;
    interm: THSLAPixel;
begin
  if hgoRepeat in FOptions then
  begin
    position := frac(position);
    if position = 0 then
    begin
      result := FColor1;
      exit;
    end;
  end else
  begin
    if position <= 0 then
    begin
      result := FColor1;
      exit;
    end else
    if position >= 1 then
    begin
      result := FColor2;
      exit
    end;
  end;
  b      := round(position*16384);
  b2     := 16384-b;
  interm.hue := ((hue1 * b2 + hue2 * b + 8191) shr 14) and $ffff;
  interm.saturation := (hsla1.saturation * b2 + hsla2.saturation * b + 8191) shr 14;
  interm.lightness := (hsla1.lightness * b2 + hsla2.lightness * b + 8191) shr 14;
  interm.alpha := (hsla1.alpha * b2 + hsla2.alpha * b + 8191) shr 14;
  if hgoLightnessCorrection in FOptions then
  begin
    if not (hgoHueCorrection in FOptions) then
      interm.hue := HtoG(interm.hue);
    result := GSBAToBGRA(interm);
  end else
  begin
    if hgoHueCorrection in FOptions then
      interm.hue := GtoH(interm.hue);
    result := HSLAToBGRA(interm);
  end;
end;

function TBGRAHueGradient.GetAverageColor: TBGRAPixel;
begin
  Result:= GetColorAt(32768);
end;

function TBGRAHueGradient.GetMonochrome: boolean;
begin
  Result:= false;
end;

{ TBGRAMultiGradient }

procedure TBGRAMultiGradient.Init(Colors: array of TBGRAPixel;
  Positions0To1: array of single; AGammaCorrection, ACycle: boolean);
var
  i: Integer;
begin
  if length(Positions0To1) <> length(colors) then
    raise Exception.Create('Dimension mismatch');
  if length(Positions0To1) = 0 then
    raise Exception.Create('Empty gradient');
  setlength(FColors,length(Colors));
  setlength(FPositions,length(Positions0To1));
  setlength(FPositionsF,length(Positions0To1));
  setlength(FEColors,length(Colors));
  for i := 0 to high(colors) do
  begin
    FColors[i]:= colors[i];
    FPositions[i]:= round(Positions0To1[i]*65536);
    FPositionsF[i]:= Positions0To1[i];
    FEColors[i]:= GammaExpansion(colors[i]);
  end;
  GammaCorrection := AGammaCorrection;
  FCycle := ACycle;
  if FPositions[high(FPositions)] = FPositions[0] then FCycle := false;
end;

constructor TBGRAMultiGradient.Create(Colors: array of TBGRAPixel;
  Positions0To1: array of single; AGammaCorrection: boolean; ACycle: boolean);
begin
  Init(Colors,Positions0To1,AGammaCorrection, ACycle);
end;

function TBGRAMultiGradient.GetColorAt(position: integer): TBGRAPixel;
var i: integer;
    ec: TExpandedPixel;
begin
  if FCycle then
    position := (position-FPositions[0]) mod (FPositions[high(FPositions)] - FPositions[0]) + FPositions[0];
  if position <= FPositions[0] then
    result := FColors[0] else
  if position >= FPositions[high(FPositions)] then
    result := FColors[high(FColors)] else
  begin
    i := 0;
    while (i < high(FPositions)) and (position > FPositions[i+1]) do
      inc(i);

    if Position = FPositions[i+1] then
      result := FColors[i+1]
    else
    if GammaCorrection then
    begin
      ec.red := FEColors[i].red + (position-FPositions[i])*(FEColors[i+1].red-FEColors[i].red) div (FPositions[i+1]-FPositions[i]);
      ec.green := FEColors[i].green + (position-FPositions[i])*(FEColors[i+1].green-FEColors[i].green) div (FPositions[i+1]-FPositions[i]);
      ec.blue := FEColors[i].blue + (position-FPositions[i])*(FEColors[i+1].blue-FEColors[i].blue) div (FPositions[i+1]-FPositions[i]);
      ec.alpha := FEColors[i].alpha + (position-FPositions[i])*(FEColors[i+1].alpha-FEColors[i].alpha) div (FPositions[i+1]-FPositions[i]);
      result := GammaCompression(ec);
    end else
    begin
      result.red := FColors[i].red + (position-FPositions[i])*(FColors[i+1].red-FColors[i].red) div (FPositions[i+1]-FPositions[i]);
      result.green := FColors[i].green + (position-FPositions[i])*(FColors[i+1].green-FColors[i].green) div (FPositions[i+1]-FPositions[i]);
      result.blue := FColors[i].blue + (position-FPositions[i])*(FColors[i+1].blue-FColors[i].blue) div (FPositions[i+1]-FPositions[i]);
      result.alpha := FColors[i].alpha + (position-FPositions[i])*(FColors[i+1].alpha-FColors[i].alpha) div (FPositions[i+1]-FPositions[i]);
    end;
  end;
end;

function TBGRAMultiGradient.GetAverageColor: TBGRAPixel;
var sumR,sumG,sumB,sumA: integer;
  i: Integer;
begin
  sumR := 0;
  sumG := 0;
  sumB := 0;
  sumA := 0;
  for i := 0 to high(FColors) do
  begin
    sumR += FColors[i].red;
    sumG += FColors[i].green;
    sumB += FColors[i].blue;
    sumA += FColors[i].alpha;
  end;
  result := BGRA(sumR div length(FColors),sumG div length(FColors),
    sumB div length(FColors),sumA div length(FColors));
end;

function TBGRAMultiGradient.GetMonochrome: boolean;
var i: integer;
begin
  for i := 1 to high(FColors) do
    if FColors[i] <> FColors[0] then
    begin
      result := false;
      exit;
    end;
  Result:= true;
end;

{ TBGRASimpleGradientWithGammaCorrection }

constructor TBGRASimpleGradientWithGammaCorrection.Create(Color1,
  Color2: TBGRAPixel);
begin
  FColor1 := Color1;
  FColor2 := Color2;
  ec1 := GammaExpansion(Color1);
  ec2 := GammaExpansion(Color2);
end;

function TBGRASimpleGradientWithGammaCorrection.GetColorAt(position: integer
  ): TBGRAPixel;
var b,b2: cardinal;
    ec: TExpandedPixel;
begin
  if position <= 0 then
    result := FColor1 else
  if position >= 65536 then
    result := FColor2 else
  begin
    b      := position;
    b2     := 65536-b;
    ec.red := (ec1.red * b2 + ec2.red * b + 32767) shr 16;
    ec.green := (ec1.green * b2 + ec2.green * b + 32767) shr 16;
    ec.blue := (ec1.blue * b2 + ec2.blue * b + 32767) shr 16;
    ec.alpha := (ec1.alpha * b2 + ec2.alpha * b + 32767) shr 16;
    result := GammaCompression(ec);
  end;
end;

function TBGRASimpleGradientWithGammaCorrection.GetColorAtF(position: single): TBGRAPixel;
var b,b2: cardinal;
    ec: TExpandedPixel;
begin
  if position <= 0 then
    result := FColor1 else
  if position >= 1 then
    result := FColor2 else
  begin
    b      := round(position*65536);
    b2     := 65536-b;
    ec.red := (ec1.red * b2 + ec2.red * b + 32767) shr 16;
    ec.green := (ec1.green * b2 + ec2.green * b + 32767) shr 16;
    ec.blue := (ec1.blue * b2 + ec2.blue * b + 32767) shr 16;
    ec.alpha := (ec1.alpha * b2 + ec2.alpha * b + 32767) shr 16;
    result := GammaCompression(ec);
  end;
end;

function TBGRASimpleGradientWithGammaCorrection.GetAverageColor: TBGRAPixel;
begin
  result := GammaCompression(MergeBGRA(ec1,ec2));
end;

function TBGRASimpleGradientWithGammaCorrection.GetMonochrome: boolean;
begin
  Result:= (FColor1 = FColor2);
end;

{ TBGRASimpleGradientWithoutGammaCorrection }

constructor TBGRASimpleGradientWithoutGammaCorrection.Create(Color1,
  Color2: TBGRAPixel);
begin
  FColor1 := Color1;
  FColor2 := Color2;
end;

function TBGRASimpleGradientWithoutGammaCorrection.GetColorAt(position: integer
  ): TBGRAPixel;
var b,b2: cardinal;
begin
  if position <= 0 then
    result := FColor1 else
  if position >= 65536 then
    result := FColor2 else
  begin
    b      := position shr 6;
    b2     := 1024-b;
    result.red  := (FColor1.red * b2 + FColor2.red * b + 511) shr 10;
    result.green := (FColor1.green * b2 + FColor2.green * b + 511) shr 10;
    result.blue := (FColor1.blue * b2 + FColor2.blue * b + 511) shr 10;
    result.alpha := (FColor1.alpha * b2 + FColor2.alpha * b + 511) shr 10;
  end;
end;

function TBGRASimpleGradientWithoutGammaCorrection.GetColorAtF(position: single): TBGRAPixel;
var b,b2: cardinal;
begin
  if position <= 0 then
    result := FColor1 else
  if position >= 1 then
    result := FColor2 else
  begin
    b      := round(position*1024);
    b2     := 1024-b;
    result.red  := (FColor1.red * b2 + FColor2.red * b + 511) shr 10;
    result.green := (FColor1.green * b2 + FColor2.green * b + 511) shr 10;
    result.blue := (FColor1.blue * b2 + FColor2.blue * b + 511) shr 10;
    result.alpha := (FColor1.alpha * b2 + FColor2.alpha * b + 511) shr 10;
  end;
end;

function TBGRASimpleGradientWithoutGammaCorrection.GetAverageColor: TBGRAPixel;
begin
  result := MergeBGRA(FColor1,FColor2);
end;

function TBGRASimpleGradientWithoutGammaCorrection.GetMonochrome: boolean;
begin
  Result:= (FColor1 = FColor2);
end;

{ TBGRAGradientTriangleScanner }

constructor TBGRAGradientTriangleScanner.Create(pt1, pt2, pt3: TPointF; c1, c2,
  c3: TBGRAPixel);
var ec1,ec2,ec3: TExpandedPixel;
begin
  FMatrix := AffineMatrix(pt2.X-pt1.X, pt3.X-pt1.X, 0,
                          pt2.Y-pt1.Y, pt3.Y-pt1.Y, 0);
  if not IsAffineMatrixInversible(FMatrix) then
    FMatrix := AffineMatrix(0,0,0,0,0,0)
  else
    FMatrix := AffineMatrixInverse(FMatrix) * AffineMatrixTranslation(-pt1.x,-pt1.y);

  ec1 := GammaExpansion(c1);
  ec2 := GammaExpansion(c2);
  ec3 := GammaExpansion(c3);
  FColor1[1] := ec1.red;
  FColor1[2] := ec1.green;
  FColor1[3] := ec1.blue;
  FColor1[4] := ec1.alpha;
  FDiff2[1] := ec2.red - ec1.red;
  FDiff2[2] := ec2.green - ec1.green;
  FDiff2[3] := ec2.blue - ec1.blue;
  FDiff2[4] := ec2.alpha - ec1.alpha;
  FDiff3[1] := ec3.red - ec1.red;
  FDiff3[2] := ec3.green - ec1.green;
  FDiff3[3] := ec3.blue - ec1.blue;
  FDiff3[4] := ec3.alpha - ec1.alpha;
  FStep := FDiff2*FMatrix[1,1]+FDiff3*FMatrix[2,1];
end;

procedure TBGRAGradientTriangleScanner.ScanMoveTo(X, Y: Integer);
begin
  ScanMoveToF(X, Y);
end;

procedure TBGRAGradientTriangleScanner.ScanMoveToF(X, Y: Single);
var
  Cur: TPointF;
begin
  Cur := FMatrix*PointF(X,Y);
  FCurColor := FColor1+FDiff2*Cur.X+FDiff3*Cur.Y;
end;

function TBGRAGradientTriangleScanner.ScanAt(X, Y: Single): TBGRAPixel;
begin
  ScanMoveToF(X,Y);
  result := ScanNextPixel;
end;

function TBGRAGradientTriangleScanner.ScanNextPixel: TBGRAPixel;
var r,g,b,a: int64;
begin
  r := round(FCurColor[1]);
  g := round(FCurColor[2]);
  b := round(FCurColor[3]);
  a := round(FCurColor[4]);
  if r > 65535 then r := 65535 else
  if r < 0 then r := 0;
  if g > 65535 then g := 65535 else
  if g < 0 then g := 0;
  if b > 65535 then b := 65535 else
  if b < 0 then b := 0;
  if a > 65535 then a := 65535 else
  if a < 0 then a := 0;
  result.red := GammaCompressionTab[r];
  result.green := GammaCompressionTab[g];
  result.blue := GammaCompressionTab[b];
  result.alpha := a shr 8;
  FCurColor += FStep;
end;

{ TBGRAGradientScanner }

procedure TBGRAGradientScanner.Init(gtype: TGradientType; o1, o2: TPointF;
  Sinus: Boolean);
begin
  FGradientType:= gtype;
  FOrigin1 := o1;
  FOrigin2 := o2;
  FSinus := Sinus;

  //compute vector
  u.x := o2.x - o1.x;
  u.y := o2.y - o1.y;
  len := sqrt(sqr(u.x) + sqr(u.y));
  if len <> 0 then
  begin
    u.x /= len;
    u.y /= len;
    aFactor := 65536/len;
    aFactorF := 1/len;
  end
  else
  begin
    aFactor := 0;
    aFactorF := 0;
  end;

  FVertical := (((gtype =gtLinear) or (gtype=gtReflected)) and (o1.x=o2.x)) or FGradient.Monochrome;
  mergedColor := FGradient.GetAverageColor;
end;

procedure TBGRAGradientScanner.InitScanInline(x, y: integer);
var p: TPointF;
begin
  p.x := X - FOrigin1.x;
  p.y := Y - FOrigin1.y;
  FDotProduct := p.x * u.x + p.y * u.y;
  FDotProductPerp := p.x * u.y - p.y * u.x;
end;

function TBGRAGradientScanner.ScanNextInline: TBGRAPixel;
var
  a,a2: single;
  ai: integer;
begin
  if FGradientType >= gtDiamond then
  begin
    if FGradientType = gtRadial then
    begin
      a := sqrt(sqr(FDotProduct) + sqr(FDotProductPerp));
      FDotProduct += u.x;
      FDotProductPerp += u.y;
    end else
    begin
      a   := abs(FDotProduct);
      a2  := abs(FDotProductPerp);
      if a2 > a then a := a2;
      FDotProduct += u.x;
      FDotProductPerp += u.y;
    end;
  end else
  if FGradientType = gtReflected then
  begin
    a := abs(FDotProduct);
    FDotProduct += u.x;
  end else
  begin
    a := FDotProduct;
    FDotProduct += u.x;
  end;

  if FSinus then
  begin
    a *= aFactor;
    if a <= low(int64) then
      result := FGradient.GetAverageColor
    else
    if a >= high(int64) then
      result := FGradient.GetAverageColor
    else
    begin
      ai := Sin65536(round(a));
      result := FGradient.GetColorAt(ai);
    end;
  end else
    result := FGradient.GetColorAtF(a*aFactorF);
end;

constructor TBGRAGradientScanner.Create(c1, c2: TBGRAPixel;
  gtype: TGradientType; o1, o2: TPointF; gammaColorCorrection: boolean;
  Sinus: Boolean);
begin
  //transparent pixels have no color so
  //take it from other color
  if c1.alpha = 0 then
  begin
    c1.red   := c2.red;
    c1.green := c2.green;
    c1.blue  := c2.blue;
  end
  else
  if c2.alpha = 0 then
  begin
    c2.red   := c1.red;
    c2.green := c1.green;
    c2.blue  := c1.blue;
  end;

  if gammaColorCorrection then
  begin
    FGradient := TBGRASimpleGradientWithGammaCorrection.Create(c1,c2);
    FGradientOwner := true;
  end else
  begin
    FGradient := TBGRASimpleGradientWithoutGammaCorrection.Create(c1,c2);
    FGradientOwner := true;
  end;
  Init(gtype,o1,o2,Sinus);
end;

constructor TBGRAGradientScanner.Create(gradient: TBGRACustomGradient;
  gtype: TGradientType; o1, o2: TPointF; Sinus: Boolean; AGradientOwner: Boolean=False);
begin
  FGradient := gradient;
  FGradientOwner := AGradientOwner;
  Init(gtype,o1,o2,Sinus);
end;

destructor TBGRAGradientScanner.Destroy;
begin
  if FGradientOwner then
    FGradient.Free;
  inherited Destroy;
end;

procedure TBGRAGradientScanner.ScanMoveTo(X, Y: Integer);
begin
  InitScanInline(X,Y);
  if FVertical then
    FHorizColor := ScanNextInline;
end;

function TBGRAGradientScanner.ScanNextPixel: TBGRAPixel;
begin
  if FVertical then
    result := FHorizColor
  else
    result := ScanNextInline;
end;

function TBGRAGradientScanner.ScanAt(X, Y: Single): TBGRAPixel;
var p: TPointF;
    a,a2: single;
    ai: integer;
begin
  if len = 0 then
  begin
    result := mergedColor;
    exit;
  end;

  p.x := X - FOrigin1.x;
  p.y := Y - FOrigin1.y;
  case FGradientType of
    gtLinear:    a := p.x * u.x + p.y * u.y;
    gtReflected: a := abs(p.x * u.x + p.y * u.y);
    gtDiamond:
        begin
          a   := abs(p.x * u.x + p.y * u.y);
          a2  := abs(p.x * u.y - p.y * u.x);
          if a2 > a then a := a2;
        end;
    gtRadial:    a := sqrt(sqr(p.x * u.x + p.y * u.y) + sqr(p.x * u.y - p.y * u.x));
  end;

  if FSinus then
  begin
    a := a*aFactor;
    if a <= low(int64) then
      result := FGradient.GetAverageColor
    else
    if a >= high(int64) then
      result := FGradient.GetAverageColor
    else
    begin
      ai := Sin65536(round(a));
      result := FGradient.GetColorAt(ai);
    end;
  end else
    result := FGradient.GetColorAtF(a*aFactorF);
end;

procedure TBGRAGradientScanner.ScanPutPixels(pdest: PBGRAPixel; count: integer;
  mode: TDrawMode);
var c: TBGRAPixel;
begin
  if FVertical or (len = 0) then
  begin
    if FVertical then c := FHorizColor
      else c := mergedColor;
    case mode of
      dmDrawWithTransparency: DrawPixelsInline(pdest,c,count);
      dmLinearBlend: FastBlendPixelsInline(pdest,c,count);
      dmSet: FillDWord(pdest^,count,Longword(c));
      dmXor: XorInline(pdest,c,count);
      dmSetExceptTransparent: if c.alpha = 255 then FillDWord(pdest^,count,Longword(c));
    end;
    exit;
  end;

  case mode of
    dmDrawWithTransparency:
      while count > 0 do
      begin
        DrawPixelInlineWithAlphaCheck(pdest,ScanNextInline);
        inc(pdest);
        dec(count);
      end;
    dmLinearBlend:
      while count > 0 do
      begin
        FastBlendPixelInline(pdest,ScanNextInline);
        inc(pdest);
        dec(count);
      end;
    dmXor:
      while count > 0 do
      begin
        PDword(pdest)^ := PDword(pdest)^ xor DWord(ScanNextInline);
        inc(pdest);
        dec(count);
      end;
    dmSet:
      while count > 0 do
      begin
        pdest^ := ScanNextInline;
        inc(pdest);
        dec(count);
      end;
    dmSetExceptTransparent:
      while count > 0 do
      begin
        c := ScanNextInline;
        if c.alpha = 255 then pdest^ := c;
        inc(pdest);
        dec(count);
      end;
  end;
end;

function TBGRAGradientScanner.IsScanPutPixelsDefined: boolean;
begin
  result := true;
end;

{ TBGRATextureMaskScanner }

constructor TBGRATextureMaskScanner.Create(AMask: TBGRACustomBitmap;
  AOffset: TPoint; ATexture: IBGRAScanner; AGlobalOpacity: Byte);
begin
  FMask := AMask;
  FMaskScanNext := @FMask.ScanNextPixel;
  FMaskScanAt := @FMask.ScanAt;
  FOffset := AOffset;
  FTexture := ATexture;
  FTextureScanNext := @FTexture.ScanNextPixel;
  FTextureScanAt := @FTexture.ScanAt;
  FGlobalOpacity:= AGlobalOpacity;
end;

destructor TBGRATextureMaskScanner.Destroy;
begin
  fillchar(FMask,sizeof(FMask),0); //avoids interface deref
  fillchar(FTexture,sizeof(FTexture),0);
  inherited Destroy;
end;

function TBGRATextureMaskScanner.IsScanPutPixelsDefined: boolean;
begin
  Result:= true;
end;

procedure TBGRATextureMaskScanner.ScanPutPixels(pdest: PBGRAPixel;
  count: integer; mode: TDrawMode);
var c: TBGRAPixel;
    alpha: byte;
    MemMask, pmask, MemTex, ptex: pbgrapixel;

  function GetNext: TBGRAPixel; inline;
  begin
    alpha := pmask^.red;
    inc(pmask);
    result := ptex^;
    inc(ptex);
    result.alpha := ApplyOpacity(result.alpha,alpha);
  end;

  function GetNextWithGlobal: TBGRAPixel; inline;
  begin
    alpha := pmask^.red;
    inc(pmask);
    result := ptex^;
    inc(ptex);
    result.alpha := ApplyOpacity( ApplyOpacity(result.alpha,alpha), FGlobalOpacity );
  end;

begin
  getmem(MemMask, count*sizeof(TBGRAPixel));
  ScannerPutPixels(FMask,MemMask,count,dmSet);
  getmem(MemTex, count*sizeof(TBGRAPixel));
  ScannerPutPixels(FTexture,MemTex,count,dmSet);

  pmask := MemMask;
  ptex := MemTex;

  if FGlobalOpacity <> 255 then
  begin
    case mode of
      dmDrawWithTransparency:
        while count > 0 do
        begin
          DrawPixelInlineWithAlphaCheck(pdest,GetNextWithGlobal);
          inc(pdest);
          dec(count);
        end;
      dmLinearBlend:
        while count > 0 do
        begin
          FastBlendPixelInline(pdest,GetNextWithGlobal);
          inc(pdest);
          dec(count);
        end;
      dmXor:
        while count > 0 do
        begin
          PDword(pdest)^ := PDword(pdest)^ xor DWord(GetNextWithGlobal);
          inc(pdest);
          dec(count);
        end;
      dmSet:
        while count > 0 do
        begin
          pdest^ := GetNextWithGlobal;
          inc(pdest);
          dec(count);
        end;
      dmSetExceptTransparent:
        while count > 0 do
        begin
          c := GetNextWithGlobal;
          if c.alpha = 255 then pdest^ := c;
          inc(pdest);
          dec(count);
        end;
    end;
  end else
  begin
    case mode of
      dmDrawWithTransparency:
        while count > 0 do
        begin
          DrawPixelInlineWithAlphaCheck(pdest,GetNext);
          inc(pdest);
          dec(count);
        end;
      dmLinearBlend:
        while count > 0 do
        begin
          FastBlendPixelInline(pdest,GetNext);
          inc(pdest);
          dec(count);
        end;
      dmXor:
        while count > 0 do
        begin
          PDword(pdest)^ := PDword(pdest)^ xor DWord(GetNext);
          inc(pdest);
          dec(count);
        end;
      dmSet:
        while count > 0 do
        begin
          pdest^ := GetNext;
          inc(pdest);
          dec(count);
        end;
      dmSetExceptTransparent:
        while count > 0 do
        begin
          c := GetNext;
          if c.alpha = 255 then pdest^ := c;
          inc(pdest);
          dec(count);
        end;
    end;
  end;

  freemem(MemMask);
  freemem(MemTex);
end;

procedure TBGRATextureMaskScanner.ScanMoveTo(X, Y: Integer);
begin
  FMask.ScanMoveTo(X+FOffset.X,Y+FOffset.Y);
  FTexture.ScanMoveTo(X,Y);
end;

function TBGRATextureMaskScanner.ScanNextPixel: TBGRAPixel;
var alpha: byte;
begin
  alpha := FMaskScanNext.red;
  result := FTextureScanNext();
  result.alpha := ApplyOpacity( ApplyOpacity(result.alpha,alpha), FGlobalOpacity );
end;

function TBGRATextureMaskScanner.ScanAt(X, Y: Single): TBGRAPixel;
var alpha: byte;
begin
  alpha := FMaskScanAt(X+FOffset.X,Y+FOffset.Y).red;
  result := FTextureScanAt(X,Y);
  result.alpha := ApplyOpacity( ApplyOpacity(result.alpha,alpha), FGlobalOpacity );
end;

{ TBGRASolidColorMaskScanner }

constructor TBGRASolidColorMaskScanner.Create(AMask: TBGRACustomBitmap;
  AOffset: TPoint; ASolidColor: TBGRAPixel);
begin
  FMask := AMask;
  FScanNext := @FMask.ScanNextPixel;
  FScanAt := @FMask.ScanAt;
  FOffset := AOffset;
  FSolidColor := ASolidColor;
end;

destructor TBGRASolidColorMaskScanner.Destroy;
begin
  fillchar(FMask,sizeof(FMask),0); //avoids interface deref
  inherited Destroy;
end;

function TBGRASolidColorMaskScanner.IsScanPutPixelsDefined: boolean;
begin
  Result:= true;
end;

procedure TBGRASolidColorMaskScanner.ScanPutPixels(pdest: PBGRAPixel;
  count: integer; mode: TDrawMode);
var c: TBGRAPixel;
    alpha: byte;
    MemMask, pmask: pbgrapixel;

  function GetNext: TBGRAPixel; inline;
  begin
    alpha := pmask^.red;
    inc(pmask);
    result := FSolidColor;
    result.alpha := ApplyOpacity(result.alpha,alpha);
  end;

begin
  getmem(MemMask, count*sizeof(TBGRAPixel));
  ScannerPutPixels(FMask,MemMask,count,dmSet);

  pmask := MemMask;

  case mode of
    dmDrawWithTransparency:
      while count > 0 do
      begin
        DrawPixelInlineWithAlphaCheck(pdest,GetNext);
        inc(pdest);
        dec(count);
      end;
    dmLinearBlend:
      while count > 0 do
      begin
        FastBlendPixelInline(pdest,GetNext);
        inc(pdest);
        dec(count);
      end;
    dmXor:
      while count > 0 do
      begin
        PDword(pdest)^ := PDword(pdest)^ xor DWord(GetNext);
        inc(pdest);
        dec(count);
      end;
    dmSet:
      while count > 0 do
      begin
        pdest^ := GetNext;
        inc(pdest);
        dec(count);
      end;
    dmSetExceptTransparent:
      while count > 0 do
      begin
        c := GetNext;
        if c.alpha = 255 then pdest^ := c;
        inc(pdest);
        dec(count);
      end;
  end;

  freemem(MemMask);
end;

procedure TBGRASolidColorMaskScanner.ScanMoveTo(X, Y: Integer);
begin
  FMask.ScanMoveTo(X+FOffset.X,Y+FOffset.Y);
end;

function TBGRASolidColorMaskScanner.ScanNextPixel: TBGRAPixel;
var alpha: byte;
begin
  alpha := FScanNext.red;
  result := FSolidColor;
  result.alpha := ApplyOpacity(result.alpha,alpha);
end;

function TBGRASolidColorMaskScanner.ScanAt(X, Y: Single): TBGRAPixel;
var alpha: byte;
begin
  alpha := FScanAt(X+FOffset.X,Y+FOffset.Y).red;
  result := FSolidColor;
  result.alpha := ApplyOpacity(result.alpha,alpha);
end;

{ TBGRAOpacityScanner }

constructor TBGRAOpacityScanner.Create(ATexture: IBGRAScanner;
  AGlobalOpacity: Byte);
begin
  FTexture := ATexture;
  FScanNext := @FTexture.ScanNextPixel;
  FScanAt := @FTexture.ScanAt;
  FGlobalOpacity:= AGlobalOpacity;
end;

destructor TBGRAOpacityScanner.Destroy;
begin
  fillchar(FTexture,sizeof(FTexture),0);
  inherited Destroy;
end;

function TBGRAOpacityScanner.IsScanPutPixelsDefined: boolean;
begin
  Result:= true;
end;

procedure TBGRAOpacityScanner.ScanPutPixels(pdest: PBGRAPixel; count: integer;
  mode: TDrawMode);
var c: TBGRAPixel;
    MemTex, ptex: pbgrapixel;

  function GetNext: TBGRAPixel; inline;
  begin
    result := ptex^;
    inc(ptex);
    result.alpha := ApplyOpacity(result.alpha,FGlobalOpacity);
  end;

begin
  getmem(MemTex, count*sizeof(TBGRAPixel));
  ScannerPutPixels(FTexture,MemTex,count,dmSet);

  ptex := MemTex;

  case mode of
    dmDrawWithTransparency:
      while count > 0 do
      begin
        DrawPixelInlineWithAlphaCheck(pdest,GetNext);
        inc(pdest);
        dec(count);
      end;
    dmLinearBlend:
      while count > 0 do
      begin
        FastBlendPixelInline(pdest,GetNext);
        inc(pdest);
        dec(count);
      end;
    dmXor:
      while count > 0 do
      begin
        PDword(pdest)^ := PDword(pdest)^ xor DWord(GetNext);
        inc(pdest);
        dec(count);
      end;
    dmSet:
      while count > 0 do
      begin
        pdest^ := GetNext;
        inc(pdest);
        dec(count);
      end;
    dmSetExceptTransparent:
      while count > 0 do
      begin
        c := GetNext;
        if c.alpha = 255 then pdest^ := c;
        inc(pdest);
        dec(count);
      end;
  end;

  freemem(MemTex);
end;

procedure TBGRAOpacityScanner.ScanMoveTo(X, Y: Integer);
begin
  FTexture.ScanMoveTo(X,Y);
end;

function TBGRAOpacityScanner.ScanNextPixel: TBGRAPixel;
begin
  result := FScanNext();
  result.alpha := ApplyOpacity(result.alpha, FGlobalOpacity );
end;

function TBGRAOpacityScanner.ScanAt(X, Y: Single): TBGRAPixel;
begin
  result := FScanAt(X,Y);
  result.alpha := ApplyOpacity(result.alpha, FGlobalOpacity );
end;

initialization

  Randomize;

end.

