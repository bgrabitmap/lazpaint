unit BGRATransform;

{$mode objfpc}

interface

{ This unit contains bitmap transformations as classes and the TAffineMatrix record and functions. }

uses
  Classes, SysUtils, BGRABitmapTypes;

type
  { Contains an affine matrix, i.e. a matrix to transform linearly and translate TPointF coordinates }
  TAffineMatrix = BGRABitmapTypes.TAffineMatrix;

  { TAffineBox }

  TAffineBox = object
  private
    function GetAsPolygon: ArrayOfTPointF;
    function GetBottomRight: TPointF;
    function GetIsEmpty: boolean;
  public
    TopLeft, TopRight,
    BottomLeft: TPointF;
    class function EmptyBox: TAffineBox;
    class function AffineBox(ATopLeft, ATopRight, ABottomLeft: TPointF): TAffineBox;
    property BottomRight: TPointF read GetBottomRight;
    property IsEmpty: boolean read GetIsEmpty;
    property AsPolygon: ArrayOfTPointF read GetAsPolygon;
  end;

  { TBGRAAffineScannerTransform allow to transform any scanner. To use it,
    create this object with a scanner as parameter, call transformation
    procedures, and finally, use the newly created object as a scanner.

    You can transform a gradient or a bitmap. See TBGRAAffineBitmapTransform
    for bitmap specific transformation. }

  { TBGRAAffineScannerTransform }

  TBGRAAffineScannerTransform = class(TBGRACustomScanner)
  protected
    FScanner: IBGRAScanner;
    FScanAtFunc: TScanAtFunction;
    FCurX,FCurY: Single;
    FEmptyMatrix: Boolean;
    FMatrix: TAffineMatrix;
    procedure SetMatrix(AMatrix: TAffineMatrix);
    function InternalScanCurrentPixel: TBGRAPixel; virtual;
    function GetViewMatrix: TAffineMatrix;
    procedure SetViewMatrix(AValue: TAffineMatrix);
  public
    GlobalOpacity: Byte;
    constructor Create(AScanner: IBGRAScanner);
    procedure Reset;
    procedure Invert;
    procedure Translate(OfsX,OfsY: Single);
    procedure RotateDeg(AngleCW: Single);
    procedure RotateRad(AngleCCW: Single);
    procedure MultiplyBy(AMatrix: TAffineMatrix);
    procedure Fit(Origin,HAxis,VAxis: TPointF); virtual;
    procedure Scale(sx,sy: single); overload;
    procedure Scale(factor: single); overload;
    procedure ScanMoveTo(X, Y: Integer); override;
    procedure ScanMoveToF(X, Y: single); inline;
    function ScanNextPixel: TBGRAPixel; override;
    function ScanAt(X, Y: Single): TBGRAPixel; override;
    property Matrix: TAffineMatrix read FMatrix write SetMatrix;
    property ViewMatrix: TAffineMatrix read GetViewMatrix write SetViewMatrix;
  end;

  { If you don't want the bitmap to repeats itself, or want to specify the
    resample filter, or want to fit easily the bitmap on axes,
    use TBGRAAffineBitmapTransform instead of TBGRAAffineScannerTransform }

  { TBGRAAffineBitmapTransform }

  TBGRAAffineBitmapTransform = class(TBGRAAffineScannerTransform)
  protected
    FBitmap: TBGRACustomBitmap;
    FRepeatImageX,FRepeatImageY: boolean;
    FResampleFilter : TResampleFilter;
    FBuffer: PBGRAPixel;
    FBufferSize: Int32or64;
    FIncludeEdges: boolean;
    procedure Init(ABitmap: TBGRACustomBitmap; ARepeatImageX: Boolean= false; ARepeatImageY: Boolean= false; AResampleFilter: TResampleFilter = rfLinear; AIncludeEdges: boolean = false);
  public
    constructor Create(ABitmap: TBGRACustomBitmap; ARepeatImage: Boolean= false; AResampleFilter: TResampleFilter = rfLinear; AIncludeEdges: boolean = false);
    constructor Create(ABitmap: TBGRACustomBitmap; ARepeatImageX: Boolean; ARepeatImageY: Boolean; AResampleFilter: TResampleFilter = rfLinear; AIncludeEdges: boolean = false);
    destructor Destroy; override;
    function InternalScanCurrentPixel: TBGRAPixel; override;
    procedure ScanPutPixels(pdest: PBGRAPixel; count: integer; mode: TDrawMode); override;
    function IsScanPutPixelsDefined: boolean; override;
    procedure Fit(Origin, HAxis, VAxis: TPointF); override;
  end;

  { TBGRAQuadLinearScanner }

  TBGRAQuadLinearScanner = class(TBGRACustomScanner)
  private
    FPoints,FVectors: array[0..3] of TPointF;
    FInvLengths,FDets: array[0..3] of single;
    FCoeffs: array[0..3] of TPointF;
    aa,bb0,cc0,inv2aa: double;
    FSource: IBGRAScanner;
    FSourceMatrix: TAffineMatrix;
    FUVVector: TPointF;

    ScanParaBB, ScanParaCC, ScanParaBBInv: double;

    ScanVertV0,ScanVertVStep0,ScanVertDenom0,ScanVertDenomStep0: double;

    FShowC1, FShowC2: boolean;
    FScanFunc: TScanNextPixelFunction;
    FCurXF,FCurYF: single;
    FBuffer: PBGRAPixel;
    FBufferSize: Int32or64;
    FTextureInterpolation: Boolean;
    function GetCulling: TFaceCulling;
    function ScanGeneral: TBGRAPixel;
    procedure PrepareScanVert0;
    function ScanVert0: TBGRAPixel;
    procedure PrepareScanPara;
    function ScanPara: TBGRAPixel;
    function GetTexColorAt(u,v: Single; detNeg: boolean): TBGRAPixel; inline;
    procedure ScanMoveToF(X,Y: single); inline;
    procedure SetCulling(AValue: TFaceCulling);
    procedure Init(ASource: IBGRAScanner; const APoints: array of TPointF;
         ATextureInterpolation: boolean);
  public
    function ScanAt(X, Y: Single): TBGRAPixel; override;
    procedure ScanPutPixels(pdest: PBGRAPixel; count: integer; mode: TDrawMode); override;
    function IsScanPutPixelsDefined: boolean; override;
    procedure ScanMoveTo(X, Y: Integer); override;
    function ScanNextPixel: TBGRAPixel; override;
    constructor Create(ASource: IBGRAScanner;
      ASourceMatrix: TAffineMatrix; const APoints: array of TPointF;
      ATextureInterpolation: boolean = true);
    constructor Create(ASource: IBGRAScanner;
      const ATexCoords: array of TPointF; const APoints: array of TPointF;
      ATextureInterpolation: boolean = true);
    destructor Destroy; override;
    property Culling: TFaceCulling read GetCulling write SetCulling;
  end;

  { TBGRABitmapScanner }

  TBGRABitmapScanner = class(TBGRACustomScanner)
  protected
    FSource: TBGRACustomBitmap;
    FRepeatX,FRepeatY: boolean;
    FScanline: PBGRAPixel;
    FCurX: integer;
    FOrigin: TPoint;
  public
    constructor Create(ASource: TBGRACustomBitmap; ARepeatX,ARepeatY: boolean; AOrigin: TPoint);
    procedure ScanMoveTo(X, Y: Integer); override;
    function ScanNextPixel: TBGRAPixel; override;
    function ScanAt(X, Y: Single): TBGRAPixel; override;
  end;

  { TBGRAExtendedBorderScanner }

  TBGRAExtendedBorderScanner = class(TBGRACustomScanner)
  protected
    FSource: IBGRAScanner;
    FBounds: TRect;
  public
    constructor Create(ASource: IBGRAScanner; ABounds: TRect);
    function ScanAt(X,Y: Single): TBGRAPixel; override;
  end;

  { TBGRAScannerOffset }

  TBGRAScannerOffset = class(TBGRACustomScanner)
  protected
    FSource: IBGRAScanner;
    FOffset: TPoint;
  public
    constructor Create(ASource: IBGRAScanner; AOffset: TPoint);
    destructor Destroy; override;
    procedure ScanMoveTo(X, Y: Integer); override;
    function ScanNextPixel: TBGRAPixel; override;
    function ScanAt(X, Y: Single): TBGRAPixel; override;
    function IsScanPutPixelsDefined: boolean; override;
    procedure ScanPutPixels(pdest: PBGRAPixel; count: integer; mode: TDrawMode); override;
  end;


{---------------------- Affine matrix functions -------------------}
//fill a matrix
function AffineMatrix(m11,m12,m13,m21,m22,m23: single): TAffineMatrix;

//matrix multiplication
operator *(M,N: TAffineMatrix): TAffineMatrix;
operator =(M,N: TAffineMatrix): boolean;

//matrix multiplication by a vector (apply transformation to that vector)
operator *(M: TAffineMatrix; V: TPointF): TPointF;
operator *(M: TAffineMatrix; A: array of TPointF): ArrayOfTPointF;

//check if matrix is inversible
function IsAffineMatrixInversible(M: TAffineMatrix): boolean;

//check if the matrix is a translation (including the identity)
function IsAffineMatrixTranslation(M: TAffineMatrix): boolean;

//check if the matrix is a scaling (including a projection i.e. with factor 0)
function IsAffineMatrixScale(M: TAffineMatrix): boolean;

//check if the matrix is the identity
function IsAffineMatrixIdentity(M: TAffineMatrix): boolean;

//compute inverse (check if inversible before)
function AffineMatrixInverse(M: TAffineMatrix): TAffineMatrix;

//define a translation matrix
function AffineMatrixTranslation(OfsX,OfsY: Single): TAffineMatrix;

//define a scaling matrix
function AffineMatrixScale(sx,sy: single): TAffineMatrix;

function AffineMatrixSkewXDeg(AngleCW: single): TAffineMatrix;
function AffineMatrixSkewYDeg(AngleCW: single): TAffineMatrix;
function AffineMatrixSkewXRad(AngleCCW: single): TAffineMatrix;
function AffineMatrixSkewYRad(AngleCCW: single): TAffineMatrix;

//define a linear matrix
function AffineMatrixLinear(v1,v2: TPointF): TAffineMatrix;

//define a rotation matrix (positive radians are counter-clockwise)
//(assuming the y-axis is pointing down)
function AffineMatrixRotationRad(AngleCCW: Single): TAffineMatrix;

//Positive degrees are clockwise
//(assuming the y-axis is pointing down)
function AffineMatrixRotationDeg(AngleCW: Single): TAffineMatrix;

//define the identity matrix (that do nothing)
function AffineMatrixIdentity: TAffineMatrix;

function IsAffineMatrixOrthogonal(M: TAffineMatrix): boolean;

type
  { TBGRATriangleLinearMapping is a scanner that provides
    an optimized transformation for linear texture mapping
    on triangles }

  { TBGRATriangleLinearMapping }

  TBGRATriangleLinearMapping = class(TBGRACustomScanner)
  protected
    FScanner: IBGRAScanner;
    FMatrix: TAffineMatrix;
    FTexCoord1,FDiff2,FDiff3,FStep: TPointF;
    FCurTexCoord: TPointF;
    FScanAtFunc: TScanAtFunction;
  public
    constructor Create(AScanner: IBGRAScanner; pt1,pt2,pt3: TPointF; tex1,tex2,tex3: TPointF);
    procedure ScanMoveTo(X,Y: Integer); override;
    procedure ScanMoveToF(X,Y: Single);
    function ScanAt(X,Y: Single): TBGRAPixel; override;
    function ScanNextPixel: TBGRAPixel; override;
  end;

type
  TPerspectiveTransform = class;

  { TBGRAPerspectiveScannerTransform }

  TBGRAPerspectiveScannerTransform = class(TBGRACustomScanner)
  private
    FTexture: IBGRAScanner;
    FMatrix: TPerspectiveTransform;
    FScanAtProc: TScanAtFunction;
    function GetIncludeOppositePlane: boolean;
    procedure SetIncludeOppositePlane(AValue: boolean);
  public
    constructor Create(texture: IBGRAScanner; texCoord1,texCoord2: TPointF; const quad: array of TPointF);
    constructor Create(texture: IBGRAScanner; const texCoordsQuad: array of TPointF; const quad: array of TPointF);
    destructor Destroy; override;
    procedure ScanMoveTo(X, Y: Integer); override;
    function ScanAt(X, Y: Single): TBGRAPixel; override;
    function ScanNextPixel: TBGRAPixel; override;
    property IncludeOppositePlane: boolean read GetIncludeOppositePlane write SetIncludeOppositePlane;
  end;

  { TPerspectiveTransform }

  TPerspectiveTransform = class
  private
    sx ,shy ,w0 ,shx ,sy ,w1 ,tx ,ty ,w2 : single;
    scanDenom,scanNumX,scanNumY: single;
    FOutsideValue: TPointF;
    FIncludeOppositePlane: boolean;
    procedure Init;
  public
    constructor Create; overload;
    constructor Create(x1,y1,x2,y2: single; const quad: array of TPointF);
    constructor Create(const quad: array of TPointF; x1,y1,x2,y2: single);
    constructor Create(const srcQuad,destQuad: array of TPointF);
    function MapQuadToQuad(const srcQuad,destQuad: array of TPointF): boolean;
    function MapRectToQuad(x1,y1,x2,y2: single; const quad: array of TPointF): boolean;
    function MapQuadToRect(const quad: array of TPointF; x1,y1,x2,y2: single): boolean;
    function MapSquareToQuad(const quad: array of TPointF): boolean;
    function MapQuadToSquare(const quad: array of TPointF): boolean;
    procedure AssignIdentity;
    function Invert: boolean;
    procedure Translate(dx,dy: single);
    procedure MultiplyBy(a: TPerspectiveTransform);
    procedure PremultiplyBy(b: TPerspectiveTransform);
    function Duplicate: TPerspectiveTransform;
    function Apply(pt: TPointF): TPointF;
    procedure ScanMoveTo(x,y:single);
    function ScanNext: TPointF;
    property OutsideValue: TPointF read FOutsideValue write FOutsideValue;
    property IncludeOppositePlane: boolean read FIncludeOppositePlane write FIncludeOppositePlane;
  end;

type
  { TBGRATwirlScanner applies a twirl transformation.

    Note : this scanner handles integer coordinates only, so
    any further transformation applied after this one may not
    render correctly. }

  { TBGRATwirlScanner }

  TBGRATwirlScanner = Class(TBGRACustomScanner)
  protected
    FScanner: IBGRAScanner;
    FScanAtFunc: TScanAtFunction;
    FCenter: TPoint;
    FTurn, FRadius, FExponent: Single;
  public
    constructor Create(AScanner: IBGRAScanner; ACenter: TPoint; ARadius: single; ATurn: single = 1; AExponent: single = 3);
    function ScanAt(X, Y: Single): TBGRAPixel; override;
    property Radius: Single read FRadius;
    property Center: TPoint read FCenter;
    property Exponent: Single read FExponent;
  end;

  { TBGRASphereDeformationScanner }

  TBGRASphereDeformationScanner = Class(TBGRACustomScanner)
  protected
    FScanner: IBGRAScanner;
    FScanAtFunc: TScanAtFunction;
    FCenter: TPointF;
    FRadiusX, FRadiusY: Single;
  public
    constructor Create(AScanner: IBGRAScanner; ACenter: TPointF; ARadiusX,ARadiusY: single);
    function ScanAt(X, Y: Single): TBGRAPixel; override;
    property RadiusX: Single read FRadiusX;
    property RadiusY: Single read FRadiusY;
  end;

  { TBGRAVerticalCylinderDeformationScanner }

  TBGRAVerticalCylinderDeformationScanner = Class(TBGRACustomScanner)
  protected
    FScanner: IBGRAScanner;
    FScanAtFunc: TScanAtFunction;
    FCenterX: single;
    FRadiusX: Single;
  public
    constructor Create(AScanner: IBGRAScanner; ACenterX: single; ARadiusX: single);
    function ScanAt(X, Y: Single): TBGRAPixel; override;
    property RadiusX: Single read FRadiusX;
  end;


implementation

uses BGRABlend, Math;

function AffineMatrix(m11, m12, m13, m21, m22, m23: single): TAffineMatrix;
begin
  result[1,1] := m11;
  result[1,2] := m12;
  result[1,3] := m13;
  result[2,1] := m21;
  result[2,2] := m22;
  result[2,3] := m23;
end;

operator *(M, N: TAffineMatrix): TAffineMatrix;
begin
  result[1,1] := M[1,1]*N[1,1] + M[1,2]*N[2,1];
  result[1,2] := M[1,1]*N[1,2] + M[1,2]*N[2,2];
  result[1,3] := M[1,1]*N[1,3] + M[1,2]*N[2,3] + M[1,3];

  result[2,1] := M[2,1]*N[1,1] + M[2,2]*N[2,1];
  result[2,2] := M[2,1]*N[1,2] + M[2,2]*N[2,2];
  result[2,3] := M[2,1]*N[1,3] + M[2,2]*N[2,3] + M[2,3];
end;

operator=(M, N: TAffineMatrix): boolean;
begin
  result := CompareMem(@M,@N,SizeOf(TAffineMatrix));
end;

operator*(M: TAffineMatrix; V: TPointF): TPointF;
begin
  if isEmptyPointF(V) then
    result := EmptyPointF
  else
  begin
    result.X := V.X*M[1,1]+V.Y*M[1,2]+M[1,3];
    result.Y := V.X*M[2,1]+V.Y*M[2,2]+M[2,3];
  end;
end;

operator*(M: TAffineMatrix; A: array of TPointF): ArrayOfTPointF;
var
  i: NativeInt;
  ofs: TPointF;
begin
  setlength(result, length(A));
  if IsAffineMatrixTranslation(M) then
  begin
    ofs := PointF(M[1,3],M[2,3]);
    for i := 0 to high(A) do
      result[i] := A[i]+ofs;
  end else
    for i := 0 to high(A) do
      result[i] := M*A[i];
end;

function IsAffineMatrixInversible(M: TAffineMatrix): boolean;
begin
  result := M[1,1]*M[2,2]-M[1,2]*M[2,1] <> 0;
end;

function IsAffineMatrixTranslation(M: TAffineMatrix): boolean;
begin
  result := (m[1,1]=1) and (m[1,2]=0) and (m[2,1] = 0) and (m[2,2]=1);
end;

function IsAffineMatrixScale(M: TAffineMatrix): boolean;
begin
  result := (M[1,3]=0) and (M[2,3]=0) and
            (M[1,2]=0) and (M[2,1]=0);
end;

function IsAffineMatrixIdentity(M: TAffineMatrix): boolean;
begin
  result := IsAffineMatrixTranslation(M) and (M[1,3]=0) and (M[2,3]=0);
end;

function AffineMatrixInverse(M: TAffineMatrix): TAffineMatrix;
var det,f: single;
    linearInverse: TAffineMatrix;
begin
  det := M[1,1]*M[2,2]-M[1,2]*M[2,1];
  if det = 0 then
    raise Exception.Create('Not inversible');
  f := 1/det;
  linearInverse := AffineMatrix(M[2,2]*f,-M[1,2]*f,0,
                         -M[2,1]*f,M[1,1]*f,0);
  result := linearInverse * AffineMatrixTranslation(-M[1,3],-M[2,3]);
end;

function AffineMatrixTranslation(OfsX, OfsY: Single): TAffineMatrix;
begin
  result := AffineMatrix(1, 0, OfsX,
                         0, 1, OfsY);
end;

function AffineMatrixScale(sx, sy: single): TAffineMatrix;
begin
  result := AffineMatrix(sx, 0,   0,
                         0,  sy,  0);
end;

function AffineMatrixSkewXDeg(AngleCW: single): TAffineMatrix;
begin
  result := AffineMatrix(1,tan(AngleCW*Pi/180),0,
                         0,        1,          0);
end;

function AffineMatrixSkewYDeg(AngleCW: single): TAffineMatrix;
begin
  result := AffineMatrix(1,           0, 0,
                 tan(AngleCW*Pi/180), 1, 0)
end;

function AffineMatrixSkewXRad(AngleCCW: single): TAffineMatrix;
begin

  result := AffineMatrix(1,tan(-AngleCCW),0,
                         0,      1,       0);
end;

function AffineMatrixSkewYRad(AngleCCW: single): TAffineMatrix;
begin
  result := AffineMatrix(1,          0, 0,
                    tan(-angleCCW),  1, 0)
end;

function AffineMatrixLinear(v1,v2: TPointF): TAffineMatrix;
begin
  result := AffineMatrix(v1.x, v2.x, 0,
                         v1.y, v2.y, 0);
end;

function AffineMatrixRotationRad(AngleCCW: Single): TAffineMatrix;
begin
  result := AffineMatrix(cos(AngleCCW),  sin(AngleCCW), 0,
                         -sin(AngleCCW), cos(AngleCCW), 0);
end;

function AffineMatrixRotationDeg(AngleCW: Single): TAffineMatrix;
const DegToRad = -Pi/180;
begin
  result := AffineMatrixRotationRad(AngleCW*DegToRad);
end;

function AffineMatrixIdentity: TAffineMatrix;
begin
  result := AffineMatrix(1, 0, 0,
                         0, 1, 0);
end;

function IsAffineMatrixOrthogonal(M: TAffineMatrix): boolean;
begin
  result := PointF(M[1,1],M[2,1])*PointF(M[1,2],M[2,2]) = 0;
end;

{ TBGRAVerticalCylinderDeformationScanner }

constructor TBGRAVerticalCylinderDeformationScanner.Create(
  AScanner: IBGRAScanner; ACenterX: single; ARadiusX: single);
begin
  FScanner := AScanner;
  FScanAtFunc := @FScanner.ScanAt;
  FCenterX := ACenterX;
  FRadiusX := ARadiusX;
end;

function TBGRAVerticalCylinderDeformationScanner.ScanAt(X, Y: Single): TBGRAPixel;
var
  xn,len,fact: Single;
begin
  xn   := (x - FCenterX) / FRadiusX;
  len := abs(xn);
  if (len <= 1) then
  begin
    if (len > 0) then
    begin
      fact := 1 / len * arcsin(len) / (Pi / 2);
      xn    *= fact;
    end;
    result := FScanAtFunc(xn * FRadiusX + FCenterX, y);
  end
  else
    result := BGRAPixelTransparent;
end;

{ TBGRASphereDeformationScanner }

constructor TBGRASphereDeformationScanner.Create(AScanner: IBGRAScanner;
  ACenter: TPointF; ARadiusX, ARadiusY: single);
begin
  FScanner := AScanner;
  FScanAtFunc := @FScanner.ScanAt;
  FCenter := ACenter;
  FRadiusX := ARadiusX;
  FRadiusY := ARadiusY;
end;

function TBGRASphereDeformationScanner.ScanAt(X, Y: Single): TBGRAPixel;
var
  xn, yn, len,fact: Single;
begin
  xn   := (x - FCenter.X) / FRadiusX;
  yn   := (y - FCenter.Y) / FRadiusY;
  len := sqrt(sqr(xn) + sqr(yn));
  if (len <= 1) then
  begin
    if (len > 0) then
    begin
      fact := 1 / len * arcsin(len) / (Pi / 2);
      xn    *= fact;
      yn    *= fact;
    end;
    result := FScanAtFunc(xn * FRadiusX + FCenter.X, yn * FRadiusY + FCenter.Y);
  end
  else
    result := BGRAPixelTransparent;
end;

{ TBGRAExtendedBorderScanner }

constructor TBGRAExtendedBorderScanner.Create(ASource: IBGRAScanner;
  ABounds: TRect);
begin
  FSource := ASource;
  FBounds := ABounds;
end;

function TBGRAExtendedBorderScanner.ScanAt(X, Y: Single): TBGRAPixel;
begin
  if x < FBounds.Left then x := FBounds.Left;
  if y < FBounds.Top then y := FBounds.Top;
  if x > FBounds.Right-1 then x := FBounds.Right-1;
  if y > FBounds.Bottom-1 then y := FBounds.Bottom-1;
  result := FSource.ScanAt(X,Y);
end;

{ TAffineBox }

function TAffineBox.GetAsPolygon: ArrayOfTPointF;
begin
  result := PointsF([TopLeft,TopRight,BottomRight,BottomLeft]);
end;

function TAffineBox.GetBottomRight: TPointF;
begin
  if IsEmpty then
    result := EmptyPointF
  else
    result := TopRight + (BottomLeft-TopLeft);
end;

function TAffineBox.GetIsEmpty: boolean;
begin
  result := isEmptyPointF(TopRight) or isEmptyPointF(BottomLeft) or isEmptyPointF(TopLeft);
end;

class function TAffineBox.EmptyBox: TAffineBox;
begin
  result.TopLeft := EmptyPointF;
  result.TopRight := EmptyPointF;
  result.BottomLeft := EmptyPointF;
end;

class function TAffineBox.AffineBox(ATopLeft, ATopRight, ABottomLeft: TPointF): TAffineBox;
begin
  result.TopLeft := ATopLeft;
  result.TopRight := ATopRight;
  result.BottomLeft := ABottomLeft;
end;

{ TBGRAScannerOffset }

constructor TBGRAScannerOffset.Create(ASource: IBGRAScanner; AOffset: TPoint);
begin
  FSource := ASource;
  FOffset := AOffset;
end;

destructor TBGRAScannerOffset.Destroy;
begin
  fillchar(FSource,sizeof(FSource),0);
  inherited Destroy;
end;

procedure TBGRAScannerOffset.ScanMoveTo(X, Y: Integer);
begin
  FSource.ScanMoveTo(X-FOffset.X,Y-FOffset.Y);
end;

function TBGRAScannerOffset.ScanNextPixel: TBGRAPixel;
begin
  Result:=FSource.ScanNextPixel;
end;

function TBGRAScannerOffset.ScanAt(X, Y: Single): TBGRAPixel;
begin
  Result:=FSource.ScanAt(X - FOffset.X, Y - FOffset.Y);
end;

function TBGRAScannerOffset.IsScanPutPixelsDefined: boolean;
begin
  Result:=FSource.IsScanPutPixelsDefined;
end;

procedure TBGRAScannerOffset.ScanPutPixels(pdest: PBGRAPixel; count: integer;
  mode: TDrawMode);
begin
  FSource.ScanPutPixels(pdest, count, mode);
end;

{ TBGRABitmapScanner }

constructor TBGRABitmapScanner.Create(ASource: TBGRACustomBitmap; ARepeatX,
  ARepeatY: boolean; AOrigin: TPoint);
begin
  FSource := ASource;
  FRepeatX := ARepeatX;
  FRepeatY := ARepeatY;
  FScanline := nil;
  FOrigin := AOrigin;
end;

procedure TBGRABitmapScanner.ScanMoveTo(X, Y: Integer);
begin
  if (FSource.NbPixels = 0) then
  begin
    FScanline := nil;
    exit;
  end;
  Inc(Y,FOrigin.Y);
  if FRepeatY then
  begin
    Y := Y mod FSource.Height;
    if Y < 0 then Y += FSource.Height;
  end;
  if (Y < 0) or (Y >= FSource.Height) then
  begin
    FScanline := nil;
    exit;
  end;
  FScanline := FSource.Scanline[Y];
  FCurX := X+FOrigin.X;
  if FRepeatX then
  begin
    FCurX := FCurX mod FSource.Width;
    if FCurX < 0 then FCurX += FSource.Width;
  end;
end;

function TBGRABitmapScanner.ScanNextPixel: TBGRAPixel;
begin
  if (FScanline = nil) then
  begin
    result := BGRAPixelTransparent;
    exit;
  end;
  if FRepeatX then
  begin
    result := (FScanline+FCurX)^;
    inc(FCurX);
    if FCurX = FSource.Width then FCurX := 0;
  end else
  begin
    if (FCurX >= FSource.Width) then
    begin
      result := BGRAPixelTransparent;
      exit;
    end;
    if FCurX < 0 then
      result := BGRAPixelTransparent
    else
      result := (FScanline+FCurX)^;
    inc(FCurX);
  end;
end;

function TBGRABitmapScanner.ScanAt(X, Y: Single): TBGRAPixel;
begin
  Result := FSource.GetPixelCycle(X+FOrigin.X,Y+FOrigin.Y,rfLinear,FRepeatX,FRepeatY);
end;

{ TBGRATriangleLinearMapping }

constructor TBGRATriangleLinearMapping.Create(AScanner: IBGRAScanner; pt1, pt2,
  pt3: TPointF; tex1, tex2, tex3: TPointF);
begin
  FScanner := AScanner;
  FScanAtFunc := @FScanner.ScanAt;

  FMatrix := AffineMatrix(pt2.X-pt1.X, pt3.X-pt1.X, 0,
                          pt2.Y-pt1.Y, pt3.Y-pt1.Y, 0);
  if not IsAffineMatrixInversible(FMatrix) then
    FMatrix := AffineMatrix(0,0,0,0,0,0)
  else
    FMatrix := AffineMatrixInverse(FMatrix) * AffineMatrixTranslation(-pt1.x,-pt1.y);

  FTexCoord1 := tex1;
  FDiff2 := tex2-tex1;
  FDiff3 := tex3-tex1;
  FStep := FDiff2*FMatrix[1,1]+FDiff3*FMatrix[2,1];
end;

procedure TBGRATriangleLinearMapping.ScanMoveTo(X, Y: Integer);
begin
  ScanMoveToF(X, Y);
end;

procedure TBGRATriangleLinearMapping.ScanMoveToF(X, Y: Single);
var
  Cur: TPointF;
begin
  Cur := FMatrix*PointF(X,Y);
  FCurTexCoord := FTexCoord1+FDiff2*Cur.X+FDiff3*Cur.Y;
end;

function TBGRATriangleLinearMapping.ScanAt(X, Y: Single): TBGRAPixel;
begin
  ScanMoveToF(X,Y);
  result := ScanNextPixel;
end;

function TBGRATriangleLinearMapping.ScanNextPixel: TBGRAPixel;
begin
  result := FScanAtFunc(FCurTexCoord.X,FCurTexCoord.Y);
  FCurTexCoord += FStep;
end;

{ TBGRAAffineScannerTransform }

constructor TBGRAAffineScannerTransform.Create(AScanner: IBGRAScanner);
begin
  FScanner := AScanner;
  FScanAtFunc := @FScanner.ScanAt;
  GlobalOpacity := 255;
  Reset;
end;

procedure TBGRAAffineScannerTransform.Reset;
begin
  FMatrix := AffineMatrixIdentity;
  FEmptyMatrix := False;
end;

procedure TBGRAAffineScannerTransform.Invert;
begin
  if not FEmptyMatrix and IsAffineMatrixInversible(FMatrix) then
    FMatrix := AffineMatrixInverse(FMatrix) else
      FEmptyMatrix := True;
end;

function TBGRAAffineScannerTransform.GetViewMatrix: TAffineMatrix;
begin
  if FEmptyMatrix then
    result := AffineMatrixIdentity
  else
    result := AffineMatrixInverse(FMatrix);
end;

procedure TBGRAAffineScannerTransform.SetViewMatrix(AValue: TAffineMatrix);
begin
  Matrix := AValue;
  Invert;
end;

procedure TBGRAAffineScannerTransform.SetMatrix(AMatrix: TAffineMatrix);
begin
  FEmptyMatrix := False;
  FMatrix := AMatrix;
end;

//transformations are inverted because the effect on the resulting image
//is the inverse of the transformation. This is due to the fact
//that the matrix is applied to source coordinates, not destination coordinates
procedure TBGRAAffineScannerTransform.Translate(OfsX, OfsY: Single);
begin
  MultiplyBy(AffineMatrixTranslation(-OfsX,-OfsY));
end;

procedure TBGRAAffineScannerTransform.RotateDeg(AngleCW: Single);
begin
  MultiplyBy(AffineMatrixRotationDeg(-AngleCW));
end;

procedure TBGRAAffineScannerTransform.RotateRad(AngleCCW: Single);
begin
  MultiplyBy(AffineMatrixRotationRad(-AngleCCW));
end;

procedure TBGRAAffineScannerTransform.MultiplyBy(AMatrix: TAffineMatrix);
begin
  FMatrix *= AMatrix;
end;

procedure TBGRAAffineScannerTransform.Fit(Origin, HAxis, VAxis: TPointF);
begin
  SetMatrix(AffineMatrix(HAxis.X-Origin.X, VAxis.X-Origin.X, 0,
                         HAxis.Y-Origin.Y, VAxis.Y-Origin.Y, 0));
  Invert;
  Translate(Origin.X,Origin.Y);
end;

procedure TBGRAAffineScannerTransform.Scale(sx, sy: single);
begin
  if (sx=0) or (sy=0) then
  begin
    FEmptyMatrix := True;
    exit;
  end;

  MultiplyBy(AffineMatrixScale(1/sx,1/sy));
end;

procedure TBGRAAffineScannerTransform.Scale(factor: single);
begin
  Scale(factor,factor);
end;

procedure TBGRAAffineScannerTransform.ScanMoveTo(X, Y: Integer);
begin
  ScanMoveToF(X,Y);
end;

procedure TBGRAAffineScannerTransform.ScanMoveToF(X, Y: single);
Var Cur: TPointF;
begin
  Cur := FMatrix * PointF(X,Y);
  FCurX := Cur.X;
  FCurY := Cur.Y;
end;

function TBGRAAffineScannerTransform.InternalScanCurrentPixel: TBGRAPixel;
begin
  if FEmptyMatrix then
  begin
    result := BGRAPixelTransparent;
    exit;
  end;
  result := FScanAtFunc(FCurX,FCurY);
end;

function TBGRAAffineScannerTransform.ScanNextPixel: TBGRAPixel;
begin
  result := InternalScanCurrentPixel;
  FCurX += FMatrix[1,1];
  FCurY += FMatrix[2,1];
  if GlobalOpacity <> 255 then result.alpha := ApplyOpacity(result.alpha,GlobalOpacity);
end;

function TBGRAAffineScannerTransform.ScanAt(X, Y: Single): TBGRAPixel;
begin
  ScanMoveToF(X,Y);
  result := InternalScanCurrentPixel;
  if GlobalOpacity <> 255 then result.alpha := ApplyOpacity(result.alpha,GlobalOpacity);
end;

{ TBGRAQuadLinearScanner }

function TBGRAQuadLinearScanner.GetTexColorAt(u, v: Single; detNeg: boolean
  ): TBGRAPixel;
begin
  if detNeg then
  begin
    if not FShowC2 then
    begin
      result := BGRAPixelTransparent;
      exit;
    end;
  end else
    if not FShowC1 then
    begin
      result := BGRAPixelTransparent;
      exit;
    end;
  with (FSourceMatrix * PointF(u,v) + FUVVector*(u*v)) do
    if FTextureInterpolation then
      result := FSource.ScanAt(x,y)
    else
      result := FSource.ScanAtInteger(System.round(x),System.round(y));
end;

procedure TBGRAQuadLinearScanner.ScanMoveToF(X, Y: single);
begin
  FCurXF := X;
  FCurYF := Y;
  if (FVectors[0].x = 0) and (FVectors[2].x = 0) then
  begin
    PrepareScanVert0;
    FScanFunc := @ScanVert0;
  end else
  if aa = 0 then
  begin
    PrepareScanPara;
    FScanFunc := @ScanPara
  end
  else
    FScanFunc := @ScanGeneral;
end;

procedure TBGRAQuadLinearScanner.SetCulling(AValue: TFaceCulling);
begin
  FShowC1 := AValue in [fcKeepCW,fcNone];
  FShowC2 := AValue in [fcKeepCCW,fcNone];
end;

procedure TBGRAQuadLinearScanner.Init(ASource: IBGRAScanner;
  const APoints: array of TPointF; ATextureInterpolation: boolean);
var
  i: NativeInt;
  v: TPointF;
  len: single;
begin
  if length(APoints)<>4 then
    raise exception.Create('Expecting 4 points');
  FTextureInterpolation:= ATextureInterpolation;
  FSource := ASource;
  FSourceMatrix := AffineMatrixIdentity;
  FUVVector := PointF(0,0);
  for i := 0 to 3 do
  begin
    FPoints[i] := APoints[i];
    v := APoints[(i+1) mod 4] - APoints[i];
    len := sqrt(v*v);
    if len > 0 then FInvLengths[i] := 1/len
      else FInvLengths[i] := 0;
    FVectors[i] := v*FInvLengths[i];
  end;

  FCoeffs[0] := FPoints[0];
  FCoeffs[1] := FPoints[1]-FPoints[0];
  FCoeffs[2] := FPoints[3]-FPoints[0];
  FCoeffs[3] := FPoints[0]+FPoints[2]-FPoints[1]-FPoints[3];

  aa := VectDet(FCoeffs[3],FCoeffs[2]);
  bb0 := VectDet(FCoeffs[3],FCoeffs[0]) + VectDet(FCoeffs[1],FCoeffs[2]);
  cc0 := VectDet(FCoeffs[1],FCoeffs[0]);
  for i := 0 to 3 do
    FDets[i] := VectDet(FVectors[i],FVectors[(i+1) mod 4]);
  if aa <> 0 then inv2aa := 1/(2*aa) else inv2aa := 1;

  FShowC1 := true;
  FShowC2 := true;

  FBuffer := nil;
  FBufferSize := 0;

  ScanMoveToF(0,0);
end;

function TBGRAQuadLinearScanner.ScanAt(X, Y: Single): TBGRAPixel;
begin
  ScanMoveToF(X,Y);
  Result:= FScanFunc();
end;

procedure TBGRAQuadLinearScanner.ScanPutPixels(pdest: PBGRAPixel; count: integer;
  mode: TDrawMode);
var
  n: NativeInt;
  p: PBGRAPixel;
begin
  if mode = dmSet then
    p := pdest
  else
  begin
    if count > FBufferSize then
    begin
      FBufferSize := count;
      ReAllocMem(FBuffer, FBufferSize*sizeof(TBGRAPixel));
    end;
    p := FBuffer;
  end;
  for n := count-1 downto 0 do
  begin
    p^ := FScanFunc();
    inc(p);
  end;
  if mode <> dmSet then PutPixels(pdest,FBuffer,count,mode,255);
end;

function TBGRAQuadLinearScanner.IsScanPutPixelsDefined: boolean;
begin
  result := true;
end;

procedure TBGRAQuadLinearScanner.ScanMoveTo(X, Y: Integer);
begin
  ScanMoveToF(X,Y);
end;

function TBGRAQuadLinearScanner.ScanNextPixel: TBGRAPixel;
begin
  Result:= FScanFunc();
end;

function TBGRAQuadLinearScanner.ScanGeneral: TBGRAPixel;
var u1,u2,v1,v2,x,y: double;
  bb,cc,det,delta,denom: double;

  procedure ReturnC1C2; inline;
  var c1,c2: TBGRAPixel;
  begin
    with (FSourceMatrix * PointF(u1,v1) + FUVVector*(u1*v1)) do
      if FTextureInterpolation then
        c1 := FSource.ScanAt(x,y)
      else
        c1 := FSource.ScanAtInteger(System.round(x),System.round(y));
    with (FSourceMatrix * PointF(u2,v2) + FUVVector*(u2*v2)) do
      if FTextureInterpolation then
        c2 := FSource.ScanAt(x,y)
      else
        c2 := FSource.ScanAtInteger(System.round(x),System.round(y));
    result := MergeBGRA(c1,c2);
  end;

begin
  x := FCurXF;
  y := FCurYF;
  FCurXF += 1;
  if (Y = FPoints[0].y) and (FVectors[0].y = 0) then
  begin
    if FVectors[0].x = 0 then
    begin
      result := BGRAPixelTransparent;
      exit;
    end;
    u1 := (X - FPoints[0].x)/(FPoints[1].x-FPoints[0].x);
    if (u1 >= 0) and (u1 <= 1) then
    begin
      result := GetTexColorAt(u1,0,FDets[0]<0);
      exit;
    end;
  end;
  if (X = FPoints[1].x) and (FVectors[1].x = 0) then
  begin
    if FVectors[1].y = 0 then
    begin
      result := BGRAPixelTransparent;
      exit;
    end;
    v1 := (Y - FPoints[1].y)/(FPoints[2].y-FPoints[1].y);
    if (v1 >= 0) and (v1 <= 1) then
    begin
      result := GetTexColorAt(0,v1,FDets[1]<0);
      exit;
    end;
  end;
  if (Y = FPoints[2].y) and (FVectors[2].y = 0) then
  begin
    if FVectors[2].x = 0 then
    begin
      result := BGRAPixelTransparent;
      exit;
    end;
    u1 := (X - FPoints[3].x)/(FPoints[2].x-FPoints[3].x);
    if (u1 >= 0) and (u1 <= 1) then
    begin
      result := GetTexColorAt(u1,1,FDets[2]<0);
      exit;
    end;
  end;
  if (X = FPoints[3].x) and (FVectors[3].x = 0) then
  begin
    if FVectors[3].y = 0 then
    begin
      result := BGRAPixelTransparent;
      exit;
    end;
    v1 := (Y - FPoints[0].y)/(FPoints[3].y-FPoints[0].y);
    if (v1 >= 0) and (v1 <= 1) then
    begin
      result := GetTexColorAt(0,v1,FDets[3]<0);
      exit;
    end;
  end;

  bb := bb0 + x*FCoeffs[3].y - y*FCoeffs[3].x;
  cc := cc0 + x*FCoeffs[1].y - y*FCoeffs[1].x;
  if cc = 0 then
  begin
    v1 := -bb*2*inv2aa;
    denom := FCoeffs[1].x+FCoeffs[3].x*v1;
    if denom = 0 then
    begin
      result := BGRAPixelTransparent;
      exit;
    end
    else
      u1 := (x-FCoeffs[0].x-FCoeffs[2].x*v1)/denom;

    if (u1>=0) and (u1<=1) and (v1 >= 0) and (v1 <= 1) then
      result := GetTexColorAt(u1,v1,bb<0)
    else
      result := BGRAPixelTransparent;
  end else
  begin
    delta := bb*bb - 4*aa*cc;

    if delta < 0 then
    begin
      result := BGRAPixelTransparent;
      exit;
    end;
    det := sqrt(delta);
    v1 := (-bb+det)*inv2aa;
    if v1 = 0 then
      u1 := (FVectors[0]*FInvLengths[0])*(PointF(x,y)-FPoints[0])
    else if v1 = 1 then
      u1 := 1 - (FVectors[2]*FInvLengths[2])*(PointF(x,y)-FPoints[2])
    else
    begin
      denom := FCoeffs[1].x+FCoeffs[3].x*v1;
      if abs(denom)<1e-6 then
      begin
        u1 := (bb+det)*inv2aa;
        denom := FCoeffs[1].y+FCoeffs[3].y*u1;
        if denom = 0 then
        begin
          result := BGRAPixelTransparent;
          exit;
        end
        else v1 := (y-FCoeffs[0].y-FCoeffs[2].y*u1)/denom;
      end
      else u1 := (x-FCoeffs[0].x-FCoeffs[2].x*v1)/denom;
    end;

    v2 := (-bb-det)*inv2aa;
    if v2 = 0 then
      u2 := (FVectors[0]*FInvLengths[0])*(PointF(x,y)-FPoints[0])
    else if v2 = 1 then
      u2 := 1 - (FVectors[2]*FInvLengths[2])*(PointF(x,y)-FPoints[2])
    else
    begin
      denom := FCoeffs[1].x+FCoeffs[3].x*v2;
      if abs(denom)<1e-6 then
      begin
        u2 := (bb-det)*inv2aa;
        denom := FCoeffs[1].y+FCoeffs[3].y*u2;
        if denom = 0 then
        begin
          result := BGRAPixelTransparent;
          exit;
        end
        else v2 := (y-FCoeffs[0].y-FCoeffs[2].y*u2)/denom;
      end
      else u2 := (x-FCoeffs[0].x-FCoeffs[2].x*v2)/denom;
    end;

    if (u1 >= 0) and (u1 <= 1) and (v1 >= 0) and (v1 <= 1) and FShowC1 then
    begin
      if (u2 >= 0) and (u2 <= 1) and (v2 >= 0) and (v2 <= 1) and FShowC2 then
        ReturnC1C2
      else
        with (FSourceMatrix * PointF(u1,v1) + FUVVector*(u1*v1)) do
          if FTextureInterpolation then
            result := FSource.ScanAt(x,y)
          else
            result := FSource.ScanAtInteger(System.round(x),System.round(y));
    end
    else
    if (u2 >= 0) and (u2 <= 1) and (v2 >= 0) and (v2 <= 1) and FShowC2 then
    begin
      with (FSourceMatrix * PointF(u2,v2) + FUVVector*(u2*v2)) do
        if FTextureInterpolation then
          result := FSource.ScanAt(x,y)
        else
          result := FSource.ScanAtInteger(System.round(x),System.round(y));
    end
    else
      result := BGRAPixelTransparent;
  end;
end;

function TBGRAQuadLinearScanner.GetCulling: TFaceCulling;
begin
  if FShowC1 and FShowC2 then
    result := fcNone
  else if FShowC1 then
    result := fcKeepCW
  else
    result := fcKeepCCW;
end;

procedure TBGRAQuadLinearScanner.PrepareScanVert0;
begin
  if (FVectors[1].x <> 0) then
  begin
    ScanVertVStep0 := 1/(FPoints[2].x-FPoints[1].x);
    ScanVertV0 := (FCurXF-FPoints[1].x)*ScanVertVStep0;
    ScanVertDenom0 := (FPoints[1].y-FPoints[0].y)*(1-ScanVertV0) + (FPoints[2].y-FPoints[3].y)*ScanVertV0;
    ScanVertDenomStep0 := (FPoints[2].y-FPoints[3].y-FPoints[1].y+FPoints[0].y)*ScanVertVStep0;
  end
  else
  begin
    ScanVertV0 := 0;
    ScanVertVStep0 := EmptySingle;
  end;
end;

function TBGRAQuadLinearScanner.ScanVert0: TBGRAPixel;
var u: single;
begin
  FCurXF += 1;
  if ScanVertVStep0 = EmptySingle then
  begin
    result := BGRAPixelTransparent;
    exit;
  end;
  if (ScanVertV0 >= 0) and (ScanVertV0 <= 1) then
  begin
    if ScanVertDenom0 = 0 then
      result := BGRAPixelTransparent
    else
    begin
      u := (FCurYF-(FPoints[0].y*(1-ScanVertV0) + FPoints[3].y*ScanVertV0))/ScanVertDenom0;
      if (u >= 0) and (u <= 1) then
        result := GetTexColorAt(u,ScanVertV0,FDets[0]<0)
      else
        result := BGRAPixelTransparent;
    end;
  end else
    result := BGRAPixelTransparent;

  ScanVertV0 += ScanVertVStep0;
  ScanVertDenom0 += ScanVertDenomStep0;
end;

procedure TBGRAQuadLinearScanner.PrepareScanPara;
begin
  ScanParaBB := bb0 + FCurXF*FCoeffs[3].y - FCurYF*FCoeffs[3].x;
  ScanParaCC := cc0 + FCurXF*FCoeffs[1].y - FCurYF*FCoeffs[1].x;
  if ScanParaBB <> 0 then
    ScanParaBBInv := 1/ScanParaBB
  else
    ScanParaBBInv := 1;
end;

function TBGRAQuadLinearScanner.ScanPara: TBGRAPixel;
var
  u,v,denom: Single;
begin
  FCurXF += 1;

  if ScanParaBB = 0 then
    result := BGRAPixelTransparent
  else
  begin
    v := -ScanParaCC*ScanParaBBInv;
    denom := FCoeffs[1].x+FCoeffs[3].x*v;
    if denom = 0 then
      result := BGRAPixelTransparent
    else
    begin
      u := (FCurXF-1-FCoeffs[0].x-FCoeffs[2].x*v)/denom;

      if (u>=0) and (u<=1) and (v >= 0) and (v <= 1) then
        result := GetTexColorAt(u,v,FDets[0]<0)
      else
        result := BGRAPixelTransparent;
    end;
  end;

  if FCoeffs[3].y <> 0 then
  begin
    ScanParaBB += FCoeffs[3].y;
    if ScanParaBB <> 0 then
      ScanParaBBInv := 1/ScanParaBB
    else
      ScanParaBBInv := 1;
  end;
  ScanParaCC += FCoeffs[1].y;
end;

constructor TBGRAQuadLinearScanner.Create(ASource: IBGRAScanner;
  ASourceMatrix: TAffineMatrix; const APoints: array of TPointF;
  ATextureInterpolation: boolean);
begin
  Init(ASource, APoints, ATextureInterpolation);
  FSourceMatrix := ASourceMatrix;
end;

constructor TBGRAQuadLinearScanner.Create(ASource: IBGRAScanner;
  const ATexCoords: array of TPointF; const APoints: array of TPointF;
  ATextureInterpolation: boolean);
begin
  Init(ASource, APoints, ATextureInterpolation);
  FSourceMatrix := AffineMatrixTranslation(ATexCoords[0].x,ATexCoords[0].y)*
                AffineMatrixLinear(ATexCoords[1]-ATexCoords[0],ATexCoords[3]-ATexCoords[0]);
  FUVVector := ATexCoords[2] - (ATexCoords[1]+ATexCoords[3]-ATexCoords[0]);
end;

destructor TBGRAQuadLinearScanner.Destroy;
begin
  freemem(FBuffer);
  inherited Destroy;
end;

{ TBGRAAffineBitmapTransform }

procedure TBGRAAffineBitmapTransform.Init(ABitmap: TBGRACustomBitmap;
  ARepeatImageX: Boolean; ARepeatImageY: Boolean;
  AResampleFilter: TResampleFilter; AIncludeEdges: boolean = false);
begin
  if (ABitmap.Width = 0) or (ABitmap.Height = 0) then
    raise Exception.Create('Empty image');
  inherited Create(ABitmap);
  FBitmap := ABitmap;
  FRepeatImageX := ARepeatImageX;
  FRepeatImageY := ARepeatImageY;
  FResampleFilter:= AResampleFilter;
  FBuffer := nil;
  FBufferSize:= 0;
  FIncludeEdges := AIncludeEdges;
end;

constructor TBGRAAffineBitmapTransform.Create(ABitmap: TBGRACustomBitmap;
  ARepeatImage: Boolean; AResampleFilter: TResampleFilter = rfLinear; AIncludeEdges: boolean = false);
begin
  Init(ABitmap,ARepeatImage,ARepeatImage,AResampleFilter,AIncludeEdges);
end;

constructor TBGRAAffineBitmapTransform.Create(ABitmap: TBGRACustomBitmap;
  ARepeatImageX: Boolean; ARepeatImageY: Boolean;
  AResampleFilter: TResampleFilter; AIncludeEdges: boolean = false);
begin
  Init(ABitmap,ARepeatImageX,ARepeatImageY,AResampleFilter,AIncludeEdges);
end;

destructor TBGRAAffineBitmapTransform.Destroy;
begin
  FreeMem(FBuffer);
end;

function TBGRAAffineBitmapTransform.InternalScanCurrentPixel: TBGRAPixel;
begin
  result := FBitmap.GetPixelCycle(FCurX,FCurY,FResampleFilter,FRepeatImageX,FRepeatImageY);
end;

procedure TBGRAAffineBitmapTransform.ScanPutPixels(pdest: PBGRAPixel;
  count: integer; mode: TDrawMode);
const PrecisionShift = {$IFDEF CPU64}24{$ELSE}12{$ENDIF};
      Precision = 1 shl PrecisionShift;
var p: PBGRAPixel;
  n: integer;
  posXPrecision, posYPrecision: NativeInt;
  deltaXPrecision,deltaYPrecision: NativeInt;
  ix,iy,shrMask,w,h: NativeInt;
  py0: PByte;
  deltaRow: NativeInt;
begin
  w := FBitmap.Width;
  h := FBitmap.Height;
  if (w = 0) or (h = 0) then exit;

  if GlobalOpacity = 0 then
  begin
    if mode = dmSet then
      FillDWord(pdest^, count, DWord(BGRAPixelTransparent));
    exit;
  end;

  posXPrecision := round(FCurX*Precision);
  deltaXPrecision:= round(FMatrix[1,1]*Precision);
  posYPrecision := round(FCurY*Precision);
  deltaYPrecision:= round(FMatrix[2,1]*Precision);
  shrMask := -1;
  shrMask := shrMask shr PrecisionShift;
  shrMask := not shrMask;

  if mode = dmSet then
    p := pdest
  else
  begin
    if count > FBufferSize then
    begin
      FBufferSize := count;
      ReAllocMem(FBuffer, FBufferSize*sizeof(TBGRAPixel));
    end;
    p := FBuffer;
  end;

  if FResampleFilter = rfBox then
  begin
    posXPrecision += Precision shr 1;
    posYPrecision += Precision shr 1;
    py0 := PByte(FBitmap.ScanLine[0]);
    if FBitmap.LineOrder = riloTopToBottom then
      deltaRow := FBitmap.Width*sizeof(TBGRAPixel) else
      deltaRow := -FBitmap.Width*sizeof(TBGRAPixel);
    if FRepeatImageX or FRepeatImageY then
    begin
      for n := count-1 downto 0 do
      begin
        if posXPrecision < 0 then ix := (posXPrecision shr PrecisionShift) or shrMask else ix := posXPrecision shr PrecisionShift;
        if posYPrecision < 0 then iy := (posYPrecision shr PrecisionShift) or shrMask else iy := posYPrecision shr PrecisionShift;
        if FRepeatImageX then ix := PositiveMod(ix,w);
        if FRepeatImageY then iy := PositiveMod(iy,h);
        if (ix < 0) or (iy < 0) or (ix >= w) or (iy >= h) then
          p^ := BGRAPixelTransparent
        else
          p^ := (PBGRAPixel(py0 + iy*deltaRow)+ix)^;
        inc(p);
        posXPrecision += deltaXPrecision;
        posYPrecision += deltaYPrecision;
      end;
    end else
    begin
     for n := count-1 downto 0 do
     begin
       if posXPrecision < 0 then ix := (posXPrecision shr PrecisionShift) or shrMask else ix := posXPrecision shr PrecisionShift;
       if posYPrecision < 0 then iy := (posYPrecision shr PrecisionShift) or shrMask else iy := posYPrecision shr PrecisionShift;
       if (ix < 0) or (iy < 0) or (ix >= w) or (iy >= h) then
         p^ := BGRAPixelTransparent
       else
         p^ := (PBGRAPixel(py0 + iy*deltaRow)+ix)^;
       inc(p);
       posXPrecision += deltaXPrecision;
       posYPrecision += deltaYPrecision;
     end;
    end;
  end else
  begin
   if FRepeatImageX and FRepeatImageY then
   begin
     for n := count-1 downto 0 do
     begin
       if posXPrecision < 0 then ix := (posXPrecision shr PrecisionShift) or shrMask else ix := posXPrecision shr PrecisionShift;
       if posYPrecision < 0 then iy := (posYPrecision shr PrecisionShift) or shrMask else iy := posYPrecision shr PrecisionShift;
       p^ := FBitmap.GetPixelCycle256(ix,iy, (posXPrecision shr (PrecisionShift-8)) and 255, (posYPrecision shr (PrecisionShift-8)) and 255,FResampleFilter);
       inc(p);
       posXPrecision += deltaXPrecision;
       posYPrecision += deltaYPrecision;
     end;
   end else
   if FRepeatImageX or FRepeatImageY then
   begin
     for n := count-1 downto 0 do
     begin
       if posXPrecision < 0 then ix := (posXPrecision shr PrecisionShift) or shrMask else ix := posXPrecision shr PrecisionShift;
       if posYPrecision < 0 then iy := (posYPrecision shr PrecisionShift) or shrMask else iy := posYPrecision shr PrecisionShift;
       p^ := FBitmap.GetPixelCycle256(ix,iy, (posXPrecision shr (PrecisionShift-8)) and 255, (posYPrecision shr (PrecisionShift-8)) and 255,FResampleFilter, FRepeatImageX,FRepeatImageY);
       inc(p);
       posXPrecision += deltaXPrecision;
       posYPrecision += deltaYPrecision;
     end;
   end else
   begin
    for n := count-1 downto 0 do
    begin
      if posXPrecision < 0 then ix := (posXPrecision shr PrecisionShift) or shrMask else ix := posXPrecision shr PrecisionShift;
      if posYPrecision < 0 then iy := (posYPrecision shr PrecisionShift) or shrMask else iy := posYPrecision shr PrecisionShift;
      p^ := FBitmap.GetPixel256(ix,iy, (posXPrecision shr (PrecisionShift-8)) and 255, (posYPrecision shr (PrecisionShift-8)) and 255,FResampleFilter);
      inc(p);
      posXPrecision += deltaXPrecision;
      posYPrecision += deltaYPrecision;
    end;
   end;
  end;

  if GlobalOpacity < 255 then
  begin
    if mode = dmSet then
      p := pdest
    else
      p := FBuffer;
    for n := count-1 downto 0 do
    begin
      p^.alpha := ApplyOpacity(p^.alpha,GlobalOpacity);
      if p^.alpha = 0 then p^ := BGRAPixelTransparent;
      inc(p);
    end;
  end;

  if mode <> dmSet then PutPixels(pdest,FBuffer,count,mode,255);
end;

function TBGRAAffineBitmapTransform.IsScanPutPixelsDefined: boolean;
begin
  Result:=true;
end;

procedure TBGRAAffineBitmapTransform.Fit(Origin, HAxis, VAxis: TPointF);
begin
  if (FBitmap.Width = 0) or (FBitmap.Height = 0) then exit;
  Matrix := AffineMatrix(HAxis.X-Origin.X, VAxis.X-Origin.X, Origin.X,
                         HAxis.Y-Origin.Y, VAxis.Y-Origin.Y, Origin.Y);
  Invert;
  if FIncludeEdges then
  begin
    Matrix := AffineMatrixTranslation(-0.5,-0.5)*AffineMatrixScale(FBitmap.Width,FBitmap.Height)*Matrix;
  end else
    Matrix := AffineMatrixScale(FBitmap.Width-1,FBitmap.Height-1)*Matrix;
end;

{ TBGRAPerspectiveScannerTransform }

function TBGRAPerspectiveScannerTransform.GetIncludeOppositePlane: boolean;
begin
  if FMatrix = nil then
    result := false
  else
    result := FMatrix.IncludeOppositePlane;
end;

procedure TBGRAPerspectiveScannerTransform.SetIncludeOppositePlane(
  AValue: boolean);
begin
  if FMatrix <> nil then
    FMatrix.IncludeOppositePlane := AValue;
end;

constructor TBGRAPerspectiveScannerTransform.Create(texture: IBGRAScanner; texCoord1,texCoord2: TPointF; const quad: array of TPointF);
begin
  if DoesQuadIntersect(quad[0],quad[1],quad[2],quad[3]) or not IsConvex(quad,False) or (texCoord1.x = texCoord2.x) or (texCoord1.y = texCoord2.y) then
    FMatrix := nil
  else
  begin
    FMatrix := TPerspectiveTransform.Create(quad,texCoord1.x,texCoord1.y,texCoord2.x,texCoord2.y);
    FMatrix.OutsideValue := EmptyPointF;
  end;
  FTexture := texture;
  FScanAtProc:= @FTexture.ScanAt;
end;

constructor TBGRAPerspectiveScannerTransform.Create(texture: IBGRAScanner;
  const texCoordsQuad: array of TPointF; const quad: array of TPointF);
begin
  if DoesQuadIntersect(quad[0],quad[1],quad[2],quad[3]) or not IsConvex(quad,False) or
     DoesQuadIntersect(texCoordsQuad[0],texCoordsQuad[1],texCoordsQuad[2],texCoordsQuad[3]) or not IsConvex(texCoordsQuad,False) then
    FMatrix := nil
  else
  begin
    FMatrix := TPerspectiveTransform.Create(quad,texCoordsQuad);
    FMatrix.OutsideValue := EmptyPointF;
  end;
  FTexture := texture;
  FScanAtProc:= @FTexture.ScanAt;
end;

destructor TBGRAPerspectiveScannerTransform.Destroy;
begin
  FMatrix.free;
  inherited Destroy;
end;

procedure TBGRAPerspectiveScannerTransform.ScanMoveTo(X, Y: Integer);
begin
  if FMatrix = nil then exit;
  FMatrix.ScanMoveTo(X,Y);
end;

function TBGRAPerspectiveScannerTransform.ScanAt(X, Y: Single): TBGRAPixel;
var ptSource: TPointF;
begin
  if FMatrix = nil then
    result := BGRAPixelTransparent else
  begin
    ptSource := FMatrix.Apply(PointF(X,Y));
    if ptSource.x = EmptySingle then
      result := BGRAPixelTransparent
    else
      Result:= FScanAtProc(ptSource.X, ptSource.Y);
  end;
end;

function TBGRAPerspectiveScannerTransform.ScanNextPixel: TBGRAPixel;
var ptSource: TPointF;
begin
  if FMatrix = nil then
    result := BGRAPixelTransparent else
  begin
    ptSource := FMatrix.ScanNext;
    if ptSource.x = EmptySingle then
      result := BGRAPixelTransparent
    else
      Result:= FScanAtProc(ptSource.X, ptSource.Y);
  end;
end;

{ TPerspectiveTransform }

procedure TPerspectiveTransform.Init;
begin
  FOutsideValue := PointF(0,0);
  FIncludeOppositePlane:= True;
end;

constructor TPerspectiveTransform.Create;
begin
  Init;
  AssignIdentity;
end;

constructor TPerspectiveTransform.Create(x1, y1, x2, y2: single;
  const quad: array of TPointF);
begin
  Init;
  MapRectToQuad(x1 ,y1 ,x2 ,y2 ,quad );
end;

constructor TPerspectiveTransform.Create(const quad: array of TPointF; x1, y1,
  x2, y2: single);
begin
  Init;
  MapQuadToRect(quad, x1,y1,x2,y2);
end;

constructor TPerspectiveTransform.Create(const srcQuad,
  destQuad: array of TPointF);
begin
  Init;
  MapQuadToQuad(srcQuad,destQuad);
end;

{ Map a quad to quad. First compute quad to square, and then square to quad. }
function TPerspectiveTransform.MapQuadToQuad(const srcQuad,
  destQuad: array of TPointF): boolean;
var
  p : TPerspectiveTransform;
begin
  if not MapQuadToSquare(srcQuad ) then
  begin
    result:=false;
    exit;
  end;

  p := TPerspectiveTransform.Create;
  if not p.MapSquareToQuad(destQuad) then
  begin
    p.Free;
    result:=false;
    exit;
  end;

  //combine both transformations
  MultiplyBy(p);
  p.Free;
  result:=true;
end;

//Map a rectangle to a quad. Make a polygon for the rectangle, and map it.
function TPerspectiveTransform.MapRectToQuad(x1, y1, x2, y2: single;
  const quad: array of TPointF): boolean;
begin
  result := MapQuadToQuad([PointF(x1,y1),PointF(x2,y1),PointF(x2,y2),PointF(x1,y2)], quad);
end;

//Map a quad to a rectangle. Make a polygon for the rectangle, and map the quad into it.
function TPerspectiveTransform.MapQuadToRect(const quad: array of TPointF; x1,
  y1, x2, y2: single): boolean;
begin
 result := MapQuadToQuad(quad, [PointF(x1,y1),PointF(x2,y1),PointF(x2,y2),PointF(x1,y2)]);
end;

//Map a square to a quad
function TPerspectiveTransform.MapSquareToQuad(const quad: array of TPointF): boolean;
var
 d,d1,d2: TPointF;
 den ,u ,v : double;

begin
 d := quad[0]-quad[1]+quad[2]-quad[3];

  if (d.x = 0.0 ) and
    (d.y = 0.0 ) then
  begin
  // Affine case (parallelogram)
   sx :=quad[1].x - quad[0].x;
   shy:=quad[1].y - quad[0].y;
   w0 :=0.0;
   shx:=quad[2].x - quad[1].x;
   sy :=quad[2].y - quad[1].y;
   w1 :=0.0;
   tx :=quad[0].x;
   ty :=quad[0].y;
   w2 :=1.0;

  end
 else
  begin
   d1 := quad[1]-quad[2];
   d2 := quad[3]-quad[2];
   den:=d1.x * d2.y - d2.x * d1.y;

   if den = 0.0 then
   begin
    // Singular case
     sx :=0.0;
     shy:=0.0;
     w0 :=0.0;
     shx:=0.0;
     sy :=0.0;
     w1 :=0.0;
     tx :=0.0;
     ty :=0.0;
     w2 :=0.0;
     result:=false;
     exit;
   end;

  // General case
   u:=(d.x * d2.y - d.y * d2.x ) / den;
   v:=(d.y * d1.x - d.x * d1.y ) / den;

   sx :=quad[1].x - quad[0].x + u * quad[1].x;
   shy:=quad[1].y - quad[0].y + u * quad[1].y;
   w0 :=u;
   shx:=quad[3].x - quad[0].x + v * quad[3].x;
   sy :=quad[3].y - quad[0].y + v * quad[3].y;
   w1 :=v;
   tx :=quad[0].x;
   ty :=quad[0].y;
   w2 :=1.0;

  end;

 result:=true;

end;

//Map a quad to a square. Compute mapping from square to quad, then invert.
function TPerspectiveTransform.MapQuadToSquare(const quad: array of TPointF): boolean;
begin
 if not MapSquareToQuad(quad ) then
   result:=false
 else
  result := Invert;
end;

procedure TPerspectiveTransform.AssignIdentity;
begin
 sx :=1;
 shy:=0;
 w0 :=0;
 shx:=0;
 sy :=1;
 w1 :=0;
 tx :=0;
 ty :=0;
 w2 :=1;
end;

function TPerspectiveTransform.Invert: boolean;
var
 d0, d1, d2, d : double;
 copy : TPerspectiveTransform;

begin
 d0:= sy  * w2 - w1  * ty;
 d1:= w0  * ty - shy * w2;
 d2:= shy * w1 - w0  * sy;
 d := sx  * d0 + shx * d1 + tx * d2;

 if d = 0.0 then
 begin
   sx := 0.0;
   shy:= 0.0;
   w0 := 0.0;
   shx:= 0.0;
   sy := 0.0;
   w1 := 0.0;
   tx := 0.0;
   ty := 0.0;
   w2 := 0.0;
   result:= false;
   exit;
 end;

 d:= 1.0 / d;

 copy := Duplicate;

 sx :=d * d0;
 shy:=d * d1;
 w0 :=d * d2;
 shx:=d * (copy.w1  * copy.tx  - copy.shx * copy.w2 );
 sy :=d * (copy.sx  * copy.w2  - copy.w0  * copy.tx );
 w1 :=d * (copy.w0  * copy.shx - copy.sx  * copy.w1 );
 tx :=d * (copy.shx * copy.ty  - copy.sy  * copy.tx );
 ty :=d * (copy.shy * copy.tx  - copy.sx  * copy.ty );
 w2 :=d * (copy.sx  * copy.sy  - copy.shy * copy.shx );

 copy.free;

 result:=true;
end;

procedure TPerspectiveTransform.Translate(dx, dy: single);
begin
 tx:=tx + dx;
 ty:=ty + dy;
end;

procedure TPerspectiveTransform.MultiplyBy(a: TPerspectiveTransform);
var b: TPerspectiveTransform;
begin
  b := Duplicate;
  sx :=a.sx  * b.sx  + a.shx * b.shy + a.tx * b.w0;
  shx:=a.sx  * b.shx + a.shx * b.sy  + a.tx * b.w1;
  tx :=a.sx  * b.tx  + a.shx * b.ty  + a.tx * b.w2;
  shy:=a.shy * b.sx  + a.sy  * b.shy + a.ty * b.w0;
  sy :=a.shy * b.shx + a.sy  * b.sy  + a.ty * b.w1;
  ty :=a.shy * b.tx  + a.sy  * b.ty  + a.ty * b.w2;
  w0 :=a.w0  * b.sx  + a.w1  * b.shy + a.w2 * b.w0;
  w1 :=a.w0  * b.shx + a.w1  * b.sy  + a.w2 * b.w1;
  w2 :=a.w0  * b.tx  + a.w1  * b.ty  + a.w2 * b.w2;
  b.Free;
end;

procedure TPerspectiveTransform.PremultiplyBy(b: TPerspectiveTransform);
var
  a : TPerspectiveTransform;
 begin
  a := Duplicate;
  sx :=a.sx  * b.sx  + a.shx * b.shy + a.tx * b.w0;
  shx:=a.sx  * b.shx + a.shx * b.sy  + a.tx * b.w1;
  tx :=a.sx  * b.tx  + a.shx * b.ty  + a.tx * b.w2;
  shy:=a.shy * b.sx  + a.sy  * b.shy + a.ty * b.w0;
  sy :=a.shy * b.shx + a.sy  * b.sy  + a.ty * b.w1;
  ty :=a.shy * b.tx  + a.sy  * b.ty  + a.ty * b.w2;
  w0 :=a.w0  * b.sx  + a.w1  * b.shy + a.w2 * b.w0;
  w1 :=a.w0  * b.shx + a.w1  * b.sy  + a.w2 * b.w1;
  w2 :=a.w0  * b.tx  + a.w1  * b.ty  + a.w2 * b.w2;
  a.Free;
end;

function TPerspectiveTransform.Duplicate: TPerspectiveTransform;
begin
  result := TPerspectiveTransform.Create;
  result.sx :=sx;
  result.shy:=shy;
  result.w0 :=w0;
  result.shx:=shx;
  result.sy :=sy;
  result.w1 :=w1;
  result.tx :=tx;
  result.ty :=ty;
  result.w2 :=w2;
end;

function TPerspectiveTransform.Apply(pt: TPointF): TPointF;
var
  m : single;
begin
  m:= pt.x * w0 + pt.y * w1 + w2;
  if (m=0) or (not FIncludeOppositePlane and (m < 0)) then
    result := FOutsideValue
  else
  begin
   m := 1/m;
   result.x := m * (pt.x * sx  + pt.y * shx + tx );
   result.y := m * (pt.x * shy + pt.y * sy  + ty );
  end;
end;

procedure TPerspectiveTransform.ScanMoveTo(x, y: single);
begin
  ScanDenom := x * w0 + y * w1 + w2;
  ScanNumX := x * sx  + y * shx + tx;
  scanNumY := x * shy + y * sy  + ty;
end;

function TPerspectiveTransform.ScanNext: TPointF;
var m: single;
begin
  if (ScanDenom = 0) or (not FIncludeOppositePlane and (ScanDenom < 0)) then
    result := FOutsideValue
  else
  begin
   m := 1/scanDenom;
   result.x := m * ScanNumX;
   result.y := m * scanNumY;
  end;
  ScanDenom += w0;
  ScanNumX += sx;
  scanNumY += shy;
end;

{ TBGRATwirlScanner }

constructor TBGRATwirlScanner.Create(AScanner: IBGRAScanner; ACenter: TPoint; ARadius: single; ATurn: single = 1; AExponent: single = 3);
begin
  FScanner := AScanner;
  FScanAtFunc := @FScanner.ScanAt;
  FCenter := ACenter;
  FTurn := ATurn;
  FRadius := ARadius;
  FExponent := AExponent;
end;

function TBGRATwirlScanner.ScanAt(X, Y: Single): TBGRAPixel;
var p: TPoint;
    d: single;
    a,cosa,sina: integer;
begin
  p := Point(Round(X)-FCenter.X,Round(Y)-FCenter.Y);
  if (abs(p.x) < FRadius) and (abs(p.Y) < FRadius) then
  begin
    d := sqrt(p.x*p.x+p.y*p.y);
    if d < FRadius then
    begin
      d := (FRadius-d)/FRadius;
      if FExponent <> 1 then d := exp(ln(d)*FExponent);
      a := round(d*FTurn*65536);
      cosa := Cos65536(a)-32768;
      sina := Sin65536(a)-32768;
      result := FScanner.ScanAt((p.x*cosa+p.y*sina)/32768 + FCenter.X,
                                (-p.x*sina+p.y*cosa)/32768 + FCenter.Y);
      exit;
    end;
  end;
  result := FScanAtFunc(X,Y);
end;

end.

