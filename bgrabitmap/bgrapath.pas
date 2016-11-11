unit BGRAPath;

{$mode objfpc}{$H+}

interface

//todo: tangent interpolation

{ There are different conventions for angles.

  First is about the unit. It can be one of the following:
  - degrees (0..360)
  - radian (0..2*Pi)
  - tenth of degrees (0..3600)
  - from 0 to 65536

  Second is about the origin. It can be one of the following:
  - right-most position (this is the default origin for radian and 65536)
  - top-most position (this is the default origin for degrees)

  Third is about the sign. It can be one of the following:
  - positive is clockwise (this is the default for degrees)
  - positive is counterclockwise (this is the default for radian and 65536)

  TBGRAPath and TBGRACanvas2D follow HTML5 convention which is:
    (radian, right-most, clockwise) that can be shortened to (radian, clockwise)
    because right-most is the default for radian. This is abbreviated as "radCW".

  When radian are CCW, it is also specified in order to make it clear, even
  if it is the default convention in mathematics.

  In order to make things easier, there are some functions that accept angles
  in degrees. The convention used here is the usual degree convention:
    (degrees, top-most, clockwise) that can be shortened to (degree)
    because top-most and clockwise is the default for degrees.

  }

uses
  Classes, BGRABitmapTypes, BGRATransform;

type
  TBGRAPathElementType = (peNone, peMoveTo, peLineTo, peCloseSubPath,
    peQuadraticBezierTo, peCubicBezierTo, peArc, peOpenedSpline,
    peClosedSpline);

  TBGRAPathDrawProc = procedure(const APoints: array of TPointF; AClosed: boolean; AData: Pointer) of object;
  TBGRAPathFillProc = procedure(const APoints: array of TPointF; AData: pointer) of object;

  TBGRAPath = class;

  { TBGRAPathCursor }

  TBGRAPathCursor = class(TBGRACustomPathCursor)
  protected
    FPath: TBGRAPath;
    FDataPos: IntPtr;
    FAcceptedDeviation: single;
    FPathLength: single;
    FPathLengthComputed: boolean;
    FBounds: TRectF;
    FBoundsComputed: boolean;
    FArcPos: Single;

    FStartCoordinate: TPointF;
    FEndCoordinate: TPointF;
    FLoopClosedShapes,FLoopPath: boolean;

    FCurrentElementType: TBGRAPathElementType;
    FCurrentElement: Pointer;
    FCurrentElementArcPos,
    FCurrentElementArcPosScale: single;
    FCurrentElementStartCoord,
    FCurrentElementEndCoord: TPointF;
    FCurrentElementLength: single;
    FCurrentElementPoints: array of TPointF;
    FCurrentSegment: NativeInt;
    FCurrentSegmentPos: single;
    function GoToNextElement(ACanJump: boolean): boolean;
    function GoToPreviousElement(ACanJump: boolean): boolean;
    procedure MoveToEndOfElement;
    procedure MoveForwardInElement(ADistance: single);
    procedure MoveBackwardInElement(ADistance: single);
    function NeedPolygonalApprox: boolean;
    procedure OnPathFree; virtual;

    function GetLoopClosedShapes: boolean; override;
    function GetLoopPath: boolean; override;
    function GetStartCoordinate: TPointF; override;
    procedure SetLoopClosedShapes(AValue: boolean); override;
    procedure SetLoopPath(AValue: boolean); override;

    function GetArcPos: single; override;
    function GetCurrentTangent: TPointF; override;
    procedure SetArcPos(AValue: single); override;
    function GetBounds: TRectF; override;
    function GetPathLength: single; override;
    procedure PrepareCurrentElement; virtual;
    function GetCurrentCoord: TPointF; override;
    function GetPath: TBGRAPath; virtual;
  public
    constructor Create(APath: TBGRAPath; AAcceptedDeviation: single = 0.1);
    function MoveForward(ADistance: single; ACanJump: boolean = true): single; override;
    function MoveBackward(ADistance: single; ACanJump: boolean = true): single; override;
    destructor Destroy; override;
    property CurrentCoordinate: TPointF read GetCurrentCoord;
    property CurrentTangent: TPointF read GetCurrentTangent;
    property Position: single read GetArcPos write SetArcPos;
    property PathLength: single read GetPathLength;
    property Path: TBGRAPath read GetPath;
    property Bounds: TRectF read GetBounds;
    property StartCoordinate: TPointF read GetStartCoordinate;
    property LoopClosedShapes: boolean read GetLoopClosedShapes write SetLoopClosedShapes;
    property LoopPath: boolean read GetLoopPath write SetLoopPath;
    property AcceptedDeviation: single read FAcceptedDeviation;
  end;

  { TBGRAPath }

  TBGRAPath = class(IBGRAPath)
  protected
    FData: PByte;
    FDataCapacity: PtrInt;
    FDataPos: PtrInt;
    FLastSubPathElementType, FLastStoredElementType: TBGRAPathElementType;
    FLastMoveToDataPos: PtrInt;
    FLastCoord,FLastTransformedCoord,
    FSubPathStartCoord, FSubPathTransformedStartCoord: TPointF;
    FExpectedTransformedControlPoint: TPointF;
    FMatrix: TAffineMatrix; //this matrix must have a base of vectors
                            //orthogonal, of same length and with positive
                            //orientation in order to preserve arcs
    FScale,FAngleRadCW: single;
    FCursors: array of TBGRAPathCursor;
    FInternalDrawOffset: TPointF;
    procedure OnModify;
    procedure OnMatrixChange;
    procedure NeedSpace(count: integer);
    function AllocateElement(AElementType: TBGRAPathElementType;
  AExtraBytes: PtrInt = 0): Pointer;
    procedure Init;
    procedure DoClear;
    function CheckElementType(AElementType: TBGRAPathElementType): boolean;
    function GoToNextElement(var APos: PtrInt): boolean;
    function GoToPreviousElement(var APos: PtrInt): boolean;
    function PeekNextElement(APos: PtrInt): TBGRAPathElementType;
    function GetElementStartCoord(APos: PtrInt): TPointF;
    function GetElementEndCoord(APos: PtrInt): TPointF;
    function GetElementLength(APos: PtrInt; AAcceptedDeviation: single): Single;
    procedure GetElementAt(APos: PtrInt;
      out AElementType: TBGRAPathElementType; out AElement: pointer);
    function GetSvgString: string; virtual;
    procedure SetSvgString(const AValue: string); virtual;
    procedure RegisterCursor(ACursor: TBGRAPathCursor);
    procedure UnregisterCursor(ACursor: TBGRAPathCursor);
    function SetLastCoord(ACoord: TPointF): TPointF; inline;
    procedure ClearLastCoord;
    procedure BezierCurveFromTransformed(tcp1, cp2, pt:TPointF);
    procedure QuadraticCurveFromTransformed(tcp, pt: TPointF);
    function LastCoordDefined: boolean; inline;
    function GetPolygonalApprox(APos: IntPtr; AAcceptedDeviation: single; AIncludeFirstPoint: boolean): ArrayOfTPointF;
    function getPoints: ArrayOfTPointF;
    function getPoints(AMatrix: TAffineMatrix): ArrayOfTPointF;
    function getCursor: TBGRACustomPathCursor;
    procedure InternalDraw(ADrawProc: TBGRAPathDrawProc; const AMatrix: TAffineMatrix; AAcceptedDeviation: single; AData: pointer);
    procedure BitmapDrawSubPathProc(const APoints: array of TPointF; AClosed: boolean; AData: pointer);
    function CorrectAcceptedDeviation(AAcceptedDeviation: single; const AMatrix: TAffineMatrix): single;
  public
    constructor Create; overload;
    constructor Create(ASvgString: string); overload;
    constructor Create(const APoints: ArrayOfTPointF); overload;
    constructor Create(APath: IBGRAPath); overload;
    destructor Destroy; override;
    procedure beginPath;
    procedure beginSubPath;
    procedure closePath;
    procedure translate(x,y: single);
    procedure resetTransform;
    procedure rotate(angleRadCW: single); overload;
    procedure rotateDeg(angleDeg: single); overload;
    procedure rotate(angleRadCW: single; center: TPointF); overload;
    procedure rotateDeg(angleDeg: single; center: TPointF); overload;
    procedure scale(factor: single);
    procedure moveTo(x,y: single); overload;
    procedure lineTo(x,y: single); overload;
    procedure moveTo(const pt: TPointF); overload;
    procedure lineTo(const pt: TPointF); overload;
    procedure polyline(const pts: array of TPointF);
    procedure polylineTo(const pts: array of TPointF);
    procedure polygon(const pts: array of TPointF);
    procedure quadraticCurveTo(cpx,cpy,x,y: single); overload;
    procedure quadraticCurveTo(const cp,pt: TPointF); overload;
    procedure quadraticCurve(const curve: TQuadraticBezierCurve); overload;
    procedure quadraticCurve(p1,cp,p2: TPointF); overload;
    procedure smoothQuadraticCurveTo(x,y: single); overload;
    procedure smoothQuadraticCurveTo(const pt: TPointF); overload;
    procedure bezierCurveTo(cp1x,cp1y,cp2x,cp2y,x,y: single); overload;
    procedure bezierCurveTo(const cp1,cp2,pt: TPointF); overload;
    procedure bezierCurve(const curve: TCubicBezierCurve); overload;
    procedure bezierCurve(p1,cp1,cp2,p2: TPointF); overload;
    procedure smoothBezierCurveTo(cp2x,cp2y,x,y: single); overload;
    procedure smoothBezierCurveTo(const cp2,pt: TPointF); overload;
    procedure rect(x,y,w,h: single);
    procedure roundRect(x,y,w,h,radius: single);
    procedure arc(cx, cy, radius, startAngleRadCW, endAngleRadCW: single; anticlockwise: boolean); overload;
    procedure arc(cx, cy, radius, startAngleRadCW, endAngleRadCW: single); overload;
    procedure arcDeg(cx, cy, radius, startAngleDeg, endAngleDeg: single; anticlockwise: boolean); overload;
    procedure arcDeg(cx, cy, radius, startAngleDeg, endAngleDeg: single); overload;
    procedure arcTo(x1, y1, x2, y2, radius: single); overload;
    procedure arcTo(const p1,p2: TPointF; radius: single); overload;
    procedure arc(const arcDef: TArcDef); overload;
    procedure arc(cx, cy, rx,ry: single; xAngleRadCW, startAngleRadCW, endAngleRadCW: single); overload;
    procedure arc(cx, cy, rx,ry, xAngleRadCW, startAngleRadCW, endAngleRadCW: single; anticlockwise: boolean); overload;
    procedure arcTo(rx,ry, xAngleRadCW: single; largeArc, anticlockwise: boolean; x,y:single);
    procedure copyTo(dest: IBGRAPath);
    procedure addPath(const AValue: string); overload;
    procedure addPath(source: IBGRAPath); overload;
    procedure openedSpline(const pts: array of TPointF; style: TSplineStyle);
    procedure closedSpline(const pts: array of TPointF; style: TSplineStyle);
    property SvgString: string read GetSvgString write SetSvgString;
    function ComputeLength(AAcceptedDeviation: single = 0.1): single;
    function ToPoints(AAcceptedDeviation: single = 0.1): ArrayOfTPointF; overload;
    function ToPoints(AMatrix: TAffineMatrix; AAcceptedDeviation: single = 0.1): ArrayOfTPointF; overload;
    function IsEmpty: boolean;
    function GetBounds(AAcceptedDeviation: single = 0.1): TRectF;
    procedure SetPoints(const APoints: ArrayOfTPointF);
    procedure stroke(ABitmap: TBGRACustomBitmap; AColor: TBGRAPixel; AWidth: single; AAcceptedDeviation: single = 0.1);
    procedure stroke(ABitmap: TBGRACustomBitmap; ATexture: IBGRAScanner; AWidth: single; AAcceptedDeviation: single = 0.1);
    procedure stroke(ABitmap: TBGRACustomBitmap; x,y: single; AColor: TBGRAPixel; AWidth: single; AAcceptedDeviation: single = 0.1);
    procedure stroke(ABitmap: TBGRACustomBitmap; x,y: single; ATexture: IBGRAScanner; AWidth: single; AAcceptedDeviation: single = 0.1);
    procedure stroke(ABitmap: TBGRACustomBitmap; const AMatrix: TAffineMatrix; AColor: TBGRAPixel; AWidth: single; AAcceptedDeviation: single = 0.1);
    procedure stroke(ABitmap: TBGRACustomBitmap; const AMatrix: TAffineMatrix; ATexture: IBGRAScanner; AWidth: single; AAcceptedDeviation: single = 0.1);
    procedure stroke(ADrawProc: TBGRAPathDrawProc; const AMatrix: TAffineMatrix; AAcceptedDeviation: single = 0.1; AData: pointer = nil);
    procedure fill(ABitmap: TBGRACustomBitmap; AColor: TBGRAPixel; AAcceptedDeviation: single = 0.1);
    procedure fill(ABitmap: TBGRACustomBitmap; ATexture: IBGRAScanner; AAcceptedDeviation: single = 0.1);
    procedure fill(ABitmap: TBGRACustomBitmap; x,y: single; AColor: TBGRAPixel; AAcceptedDeviation: single = 0.1);
    procedure fill(ABitmap: TBGRACustomBitmap; x,y: single; ATexture: IBGRAScanner; AAcceptedDeviation: single = 0.1);
    procedure fill(ABitmap: TBGRACustomBitmap; const AMatrix: TAffineMatrix; AColor: TBGRAPixel; AAcceptedDeviation: single = 0.1);
    procedure fill(ABitmap: TBGRACustomBitmap; const AMatrix: TAffineMatrix; ATexture: IBGRAScanner; AAcceptedDeviation: single = 0.1);
    procedure fill(AFillProc: TBGRAPathFillProc; const AMatrix: TAffineMatrix; AAcceptedDeviation: single = 0.1; AData: pointer = nil);
    function CreateCursor(AAcceptedDeviation: single = 0.1): TBGRAPathCursor;
    procedure Fit(ARect: TRectF; AAcceptedDeviation: single = 0.1);
    procedure FitInto(ADest: TBGRAPath; ARect: TRectF; AAcceptedDeviation: single = 0.1);
  protected
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
    function _AddRef: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
    function _Release: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
  end;

{----------------------- Spline ------------------}

function SplineVertexToSide(y0, y1, y2, y3: single; t: single): single;
function ComputeBezierCurve(const curve: TCubicBezierCurve; AAcceptedDeviation: single = 0.1): ArrayOfTPointF; overload;
function ComputeBezierCurve(const curve: TQuadraticBezierCurve; AAcceptedDeviation: single = 0.1): ArrayOfTPointF; overload;
function ComputeBezierSpline(const spline: array of TCubicBezierCurve; AAcceptedDeviation: single = 0.1): ArrayOfTPointF; overload;
function ComputeBezierSpline(const spline: array of TQuadraticBezierCurve; AAcceptedDeviation: single = 0.1): ArrayOfTPointF; overload;
function ComputeClosedSpline(const points: array of TPointF; Style: TSplineStyle; AAcceptedDeviation: single = 0.1): ArrayOfTPointF;
function ComputeOpenedSpline(const points: array of TPointF; Style: TSplineStyle; EndCoeff: single = 0.25; AAcceptedDeviation: single = 0.1): ArrayOfTPointF;
function ClosedSplineStartPoint(const points: array of TPointF; Style: TSplineStyle): TPointF;

{ Compute points to draw an antialiased ellipse }
function ComputeEllipse(x,y,rx,ry: single; quality: single = 1): ArrayOfTPointF;
function ComputeArc65536(x, y, rx, ry: single; start65536,end65536: word; quality: single = 1): ArrayOfTPointF;
function ComputeArcRad(x, y, rx, ry: single; startRadCCW,endRadCCW: single; quality: single = 1): ArrayOfTPointF;
function ComputeArc(const arc: TArcDef; quality: single = 1): ArrayOfTPointF;
function ComputeRoundRect(x1,y1,x2,y2,rx,ry: single; quality: single = 1): ArrayOfTPointF; overload;
function ComputeRoundRect(x1,y1,x2,y2,rx,ry: single; options: TRoundRectangleOptions; quality: single = 1): ArrayOfTPointF; overload;

function Html5ArcTo(const p0, p1, p2: TPointF; radius: single): TArcDef;
function SvgArcTo(const p0: TPointF; rx, ry, xAngleRadCW: single; largeArc,
  anticlockwise: boolean; const p1: TPointF): TArcDef;
function ArcStartPoint(const arc: TArcDef): TPointF;
function ArcEndPoint(const arc: TArcDef): TPointF;
function IsLargeArc(const arc: TArcDef): boolean;

implementation

uses Math, BGRAResample, SysUtils;

type
  TStrokeData = record
    Bitmap: TBGRACustomBitmap;
    Texture: IBGRAScanner;
    Color: TBGRAPixel;
    Width: Single;
  end;

  PPathElementHeader = ^TPathElementHeader;
  TPathElementHeader = record
    ElementType: TBGRAPathElementType;
    PreviousElementType: TBGRAPathElementType;
  end;
  PMoveToElement = ^TMoveToElement;
  TMoveToElement = record
    StartCoordinate: TPointF;
    LoopDataPos: PtrInt; //if the path is closed
  end;
  PClosePathElement = ^TClosePathElement;
  TClosePathElement = type TMoveToElement;
  PQuadraticBezierToElement = ^TQuadraticBezierToElement;
  TQuadraticBezierToElement = record
    ControlPoint, Destination: TPointF;
  end;
  PCubicBezierToElement = ^TCubicBezierToElement;
  TCubicBezierToElement = record
    ControlPoint1, ControlPoint2, Destination: TPointF;
  end;
  PArcElement = ^TArcElement;
  TArcElement = TArcDef;

  PSplineElement = ^TSplineElement;
  TSplineElement = record
    SplineStyle: TSplineStyle;
    NbControlPoints: integer;
  end;

const
  PathElementSize : array[TBGRAPathElementType] of PtrInt =
  (0, Sizeof(TMoveToElement), Sizeof(TClosePathElement), sizeof(TPointF),
   sizeof(TQuadraticBezierToElement), sizeof(TCubicBezierToElement),
   sizeof(TArcElement), sizeof(TSplineElement)+sizeof(integer),
   sizeof(TSplineElement)+sizeof(integer));

function SplineVertexToSide(y0, y1, y2, y3: single; t: single): single;
var
  a0, a1, a2, a3: single;
  t2: single;
begin
  t2     := t * t;
  a0     := y3 - y2 - y0 + y1;
  a1     := y0 - y1 - a0;
  a2     := y2 - y0;
  a3     := y1;
  Result := a0 * t * t2 + a1 * t2 + a2 * t + a3;
end;

function ComputeCurvePartPrecision(pt1, pt2, pt3, pt4: TPointF; AAcceptedDeviation: single = 0.1): integer;
var
  len: single;
begin
  len    := sqr(pt1.x - pt2.x) + sqr(pt1.y - pt2.y);
  len    := max(len, sqr(pt3.x - pt2.x) + sqr(pt3.y - pt2.y));
  len    := max(len, sqr(pt3.x - pt4.x) + sqr(pt3.y - pt4.y));
  Result := round(sqrt(sqrt(len)/AAcceptedDeviation) * 0.9);
  if Result<=0 then Result:=1;
end;

function ComputeBezierCurve(const curve: TCubicBezierCurve; AAcceptedDeviation: single = 0.1): ArrayOfTPointF; overload;
begin
  result := curve.ToPoints(AAcceptedDeviation);
end;

function ComputeBezierCurve(const curve: TQuadraticBezierCurve; AAcceptedDeviation: single = 0.1): ArrayOfTPointF; overload;
begin
  result := curve.ToPoints(AAcceptedDeviation);
end;

function ComputeBezierSpline(const spline: array of TCubicBezierCurve; AAcceptedDeviation: single = 0.1): ArrayOfTPointF;
var
  curves: array of array of TPointF;
  nb: integer;
  lastPt: TPointF;
  i: Integer;
  j: Integer;

  procedure AddPt(pt: TPointF); inline;
  begin
    result[nb]:= pt;
    inc(nb);
    lastPt := pt;
  end;

  function EqLast(pt: TPointF): boolean;
  begin
    result := (pt.x = lastPt.x) and (pt.y = lastPt.y);
  end;

begin
  if length(spline)= 0 then
  begin
    setlength(result,0);
    exit;
  end;
  setlength(curves, length(spline));
  for i := 0 to high(spline) do
    curves[i] := ComputeBezierCurve(spline[i],AAcceptedDeviation);
  nb := length(curves[0]);
  lastPt := curves[0][high(curves[0])];
  for i := 1 to high(curves) do
  begin
    inc(nb,length(curves[i]));
    if EqLast(curves[i][0]) then dec(nb);
    lastPt := curves[i][high(curves[i])];
  end;
  setlength(result,nb);
  nb := 0;
  for j := 0 to high(curves[0]) do
    AddPt(curves[0][j]);
  for i := 1 to high(curves) do
  begin
    if not EqLast(curves[i][0]) then AddPt(curves[i][0]);
    for j := 1 to high(curves[i]) do
      AddPt(curves[i][j]);
  end;
end;

function ComputeBezierSpline(const spline: array of TQuadraticBezierCurve;
  AAcceptedDeviation: single = 0.1): ArrayOfTPointF;
var
  curves: array of array of TPointF;
  nb: integer;
  lastPt: TPointF;
  i: Integer;
  j: Integer;

  procedure AddPt(pt: TPointF); inline;
  begin
    result[nb]:= pt;
    inc(nb);
    lastPt := pt;
  end;

  function EqLast(pt: TPointF): boolean;
  begin
    result := (pt.x = lastPt.x) and (pt.y = lastPt.y);
  end;

begin
  if length(spline)= 0 then
  begin
    setlength(result,0);
    exit;
  end;
  setlength(curves, length(spline));
  for i := 0 to high(spline) do
    curves[i] := ComputeBezierCurve(spline[i],AAcceptedDeviation);
  nb := length(curves[0]);
  lastPt := curves[0][high(curves[0])];
  for i := 1 to high(curves) do
  begin
    inc(nb,length(curves[i]));
    if EqLast(curves[i][0]) then dec(nb);
    lastPt := curves[i][high(curves[i])];
  end;
  setlength(result,nb);
  nb := 0;
  for j := 0 to high(curves[0]) do
    AddPt(curves[0][j]);
  for i := 1 to high(curves) do
  begin
    if not EqLast(curves[i][0]) then AddPt(curves[i][0]);
    for j := 1 to high(curves[i]) do
      AddPt(curves[i][j]);
  end;
end;

function ComputeClosedSpline(const points: array of TPointF; Style: TSplineStyle; AAcceptedDeviation: single = 0.1): ArrayOfTPointF;
var
  i, j, nb, idx, pre: integer;
  ptPrev, ptPrev2, ptNext, ptNext2: TPointF;
  t: single;
  kernel: TWideKernelFilter;

begin
  if length(points) <= 2 then
  begin
    setlength(result,length(points));
    for i := 0 to high(result) do
      result[i] := points[i];
    exit;
  end;

  nb := 1;
  for i := 0 to high(points) do
  begin
    ptPrev2 := points[(i + length(points) - 1) mod length(points)];
    ptPrev  := points[i];
    ptNext  := points[(i + 1) mod length(points)];
    ptNext2 := points[(i + 2) mod length(points)];
    nb      += ComputeCurvePartPrecision(ptPrev2, ptPrev, ptNext, ptNext2, AAcceptedDeviation);
  end;

  kernel := CreateInterpolator(style);
  setlength(Result, nb);
  idx := 0;
  for i := 0 to high(points) do
  begin
    ptPrev2 := points[(i + length(points) - 1) mod length(points)];
    ptPrev  := points[i];
    ptNext  := points[(i + 1) mod length(points)];
    ptNext2 := points[(i + 2) mod length(points)];
    pre     := ComputeCurvePartPrecision(ptPrev2, ptPrev, ptNext, ptNext2, AAcceptedDeviation);
    if i=0 then
      j := 0
    else
      j := 1;
    while j <= pre do
    begin
      t := j/pre;
      result[idx] := ptPrev2*kernel.Interpolation(t+1) + ptPrev*kernel.Interpolation(t) +
                     ptNext*kernel.Interpolation(t-1)  + ptNext2*kernel.Interpolation(t-2);
      Inc(idx);
      inc(j);
    end;
  end;
  kernel.Free;
end;

function ComputeOpenedSpline(const points: array of TPointF; Style: TSplineStyle; EndCoeff: single; AAcceptedDeviation: single = 0.1): ArrayOfTPointF;
var
  i, j, nb, idx, pre: integer;
  ptPrev, ptPrev2, ptNext, ptNext2: TPointF;
  t: single;
  kernel: TWideKernelFilter;
begin
  if length(points) <= 2 then
  begin
    setlength(result,length(points));
    for i := 0 to high(result) do
      result[i] := points[i];
    exit;
  end;
  if style in[ssInsideWithEnds,ssCrossingWithEnds] then EndCoeff := 0;
  if EndCoeff < -0.3 then EndCoeff := -0.3;

  nb := 1;
  for i := 0 to high(points) - 1 do
  begin
    ptPrev  := points[i];
    ptNext  := points[i + 1];
    if i=0 then
      ptPrev2 := (ptPrev+(ptNext+points[i + 2])*EndCoeff)*(1/(1+2*EndCoeff))
    else
      ptPrev2 := points[i - 1];
    if i = high(points)-1 then
      ptNext2 := (ptNext+(ptPrev+points[i - 1])*EndCoeff)*(1/(1+2*EndCoeff))
    else
      ptNext2 := points[i + 2];
    nb      += ComputeCurvePartPrecision(ptPrev2, ptPrev, ptNext, ptNext2, AAcceptedDeviation);
  end;

  kernel := CreateInterpolator(style);
  if Style in[ssInsideWithEnds,ssCrossingWithEnds] then
  begin
    inc(nb,2);
    setlength(Result, nb);
    result[0] := points[0];
    idx := 1;
  end else
  begin
    idx := 0;
    setlength(Result, nb);
  end;
  for i := 0 to high(points) - 1 do
  begin
    ptPrev  := points[i];
    ptNext  := points[i + 1];
    if i=0 then
      ptPrev2 := (ptPrev+(ptNext+points[i + 2])*EndCoeff)*(1/(1+2*EndCoeff))
    else
      ptPrev2 := points[i - 1];
    if i = high(points)-1 then
      ptNext2 := (ptNext+(ptPrev+points[i - 1])*EndCoeff)*(1/(1+2*EndCoeff))
    else
      ptNext2 := points[i + 2];
    pre     := ComputeCurvePartPrecision(ptPrev2, ptPrev, ptNext, ptNext2, AAcceptedDeviation);
    if i=0 then
    begin
      j := 0;
    end else j := 1;
    while j <= pre do
    begin
      t := j/pre;
      result[idx] := ptPrev2*kernel.Interpolation(t+1) + ptPrev*kernel.Interpolation(t) +
                     ptNext*kernel.Interpolation(t-1)  + ptNext2*kernel.Interpolation(t-2);
      Inc(idx);
      inc(j);
    end;
  end;
  kernel.Free;
  if Style in[ssInsideWithEnds,ssCrossingWithEnds] then
    result[idx] := points[high(points)];
end;

function ClosedSplineStartPoint(const points: array of TPointF;
  Style: TSplineStyle): TPointF;
var
  kernel: TWideKernelFilter;
  ptPrev2: TPointF;
  ptPrev: TPointF;
  ptNext: TPointF;
  ptNext2: TPointF;
begin
  if length(points) = 0 then
    result := EmptyPointF
  else
  if length(points)<=2 then
    result := points[0]
  else
  begin
    kernel := CreateInterpolator(style);
    ptPrev2 := points[high(points)];
    ptPrev  := points[0];
    ptNext  := points[1];
    ptNext2 := points[2];
    result := ptPrev2*kernel.Interpolation(1) + ptPrev*kernel.Interpolation(0) +
              ptNext*kernel.Interpolation(-1)  + ptNext2*kernel.Interpolation(-2);
    kernel.free;
  end;
end;

function ComputeArc65536(x, y, rx, ry: single; start65536,end65536: word; quality: single): ArrayOfTPointF;
var i,nb: integer;
    arclen: integer;
    pos: word;
begin
  if end65536 > start65536 then
    arclen := end65536-start65536 else
    arclen := 65536-(start65536-end65536);

  if quality < 0 then quality := 0;

  nb := round(((rx+ry)*2*quality+8)*arclen/65536) and not 3;
  if arclen <= 16384 then
  begin
    if nb < 2 then nb := 2;
  end else
  if arclen <= 32768 then
  begin
    if nb < 3 then nb := 3;
  end else
  if arclen <= 32768+16384 then
  begin
    if nb < 4 then nb := 4;
  end else
    if nb < 5 then nb := 5;

  if nb > arclen+1 then nb := arclen+1;

  setlength(result,nb);
  for i := 0 to nb-1 do
  begin
    {$PUSH}{$R-}
    pos := start65536+int64(i)*arclen div (int64(nb)-1);
    {$POP}
    result[i] := PointF(x+rx*(Cos65536(pos)-32768)/32768,
                        y-ry*(Sin65536(pos)-32768)/32768);
  end;
end;

function ComputeEllipse(x, y, rx, ry: single; quality: single): ArrayOfTPointF;
begin
  result := ComputeArc65536(x,y,rx,ry,0,0,quality);
end;

function ComputeArcRad(x, y, rx, ry: single; startRadCCW, endRadCCW: single;
  quality: single): ArrayOfTPointF;
begin
  result := ComputeArc65536(x,y,rx,ry,round(startRadCCW*32768/Pi) and $ffff,round(endRadCCW*32768/Pi) and $ffff,quality);
  result[0] := PointF(x+cos(startRadCCW)*rx,y-sin(startRadCCW)*ry);
  result[high(result)] := PointF(x+cos(endRadCCW)*rx,y-sin(endRadCCW)*ry);
end;

function ComputeArc(const arc: TArcDef; quality: single): ArrayOfTPointF;
var startAngle,endAngle: single;
    i,n: integer;
    temp: TPointF;
    m: TAffineMatrix;
begin
  startAngle := -arc.startAngleRadCW;
  endAngle:= -arc.endAngleRadCW;
  if not arc.anticlockwise then
  begin
    result := ComputeArcRad(arc.center.x,arc.center.y,arc.radius.x,arc.radius.y,endAngle,startAngle,quality);
    n := length(result);
    if n>1 then
      for i := 0 to (n-2) div 2 do
      begin
        temp := result[i];
        result[i] := result[n-1-i];
        result[n-1-i] := temp;
      end;
  end else
    result := ComputeArcRad(arc.center.x,arc.center.y,arc.radius.x,arc.radius.y,startAngle,endAngle,quality);
  if arc.xAngleRadCW <> 0 then
  begin
    m := AffineMatrixTranslation(arc.center.x,arc.center.y)*AffineMatrixRotationRad(-arc.xAngleRadCW)*AffineMatrixTranslation(-arc.center.x,-arc.center.y);
    for i := 0 to high(result) do
      result[i] := m*result[i];
  end;
end;

function ComputeRoundRect(x1,y1,x2,y2,rx,ry: single; quality: single): ArrayOfTPointF;
begin
  result := ComputeRoundRect(x1,y1,x2,y2,rx,ry,[],quality);
end;

function ComputeRoundRect(x1, y1, x2, y2, rx, ry: single;
  options: TRoundRectangleOptions; quality: single): ArrayOfTPointF;
var q0,q1,q2,q3,q4: array of TPointF;
  temp: Single;
begin
  if x1 > x2 then
  begin
    temp := x1;
    x1 := x2;
    x2 := temp;
  end;
  if y1 > y2 then
  begin
    temp := y1;
    y1 := y2;
    y2 := temp;
  end;
  rx := abs(rx);
  ry := abs(ry);
  if 2*rx > x2-x1 then
    rx := (x2-x1)/2;
  if 2*ry > y2-y1 then
    ry := (y2-y1)/2;

  q0 := PointsF([PointF(x2,(y1+y2)/2)]);

  if rrTopRightBevel in options then
    q1 := PointsF([PointF(x2,y1+ry),PointF(x2-rx,y1)]) else
  if rrTopRightSquare in options then
    q1 := PointsF([PointF(x2,y1)])
  else
    q1 := ComputeArc65536(x2-rx,y1+ry,rx,ry,0,16384,quality);

  if rrTopLeftBevel in options then
    q2 := PointsF([PointF(x1+rx,y1),PointF(x1,y1+ry)]) else
  if rrTopLeftSquare in options then
    q2 := PointsF([PointF(x1,y1)])
  else
    q2 := ComputeArc65536(x1+rx,y1+ry,rx,ry,16384,32768,quality);

  if rrBottomLeftBevel in options then
    q3 := PointsF([PointF(x1,y2-ry),PointF(x1+rx,y2)]) else
  if rrBottomLeftSquare in options then
    q3 := PointsF([PointF(x1,y2)])
  else
    q3 := ComputeArc65536(x1+rx,y2-ry,rx,ry,32768,32768+16384,quality);

  if rrBottomRightBevel in options then
    q4 := PointsF([PointF(x2-rx,y2),PointF(x2,y2-ry)]) else
  if rrBottomRightSquare in options then
    q4 := PointsF([PointF(x2,y2)])
  else
    q4 := ComputeArc65536(x2-rx,y2-ry,rx,ry,32768+16384,0,quality);

  result := ConcatPointsF([q0,q1,q2,q3,q4]);
end;

function Html5ArcTo(const p0, p1, p2: TPointF; radius: single
  ): TArcDef;
var p3,p4,an,bn,cn,c: TPointF;
    dir, a2, b2, c2, cosx, sinx, d: single;
    anticlockwise: boolean;
begin
  result.center := p1;
  result.radius := PointF(0,0);
  result.xAngleRadCW:= 0;
  result.startAngleRadCW := 0;
  result.endAngleRadCW:= 0;
  result.anticlockwise:= false;

  radius := abs(radius);
  if (p0 = p1) or (p1 = p2) or (radius = 0) then exit;

  dir := (p2.x-p1.x)*(p0.y-p1.y) + (p2.y-p1.y)*(p1.x-p0.x);
  if dir = 0 then exit;

  a2 := (p0.x-p1.x)*(p0.x-p1.x) + (p0.y-p1.y)*(p0.y-p1.y);
  b2 := (p1.x-p2.x)*(p1.x-p2.x) + (p1.y-p2.y)*(p1.y-p2.y);
  c2 := (p0.x-p2.x)*(p0.x-p2.x) + (p0.y-p2.y)*(p0.y-p2.y);
  cosx := (a2+b2-c2)/(2*sqrt(a2*b2));

  sinx := sqrt(1 - cosx*cosx);
  if (sinx = 0) or (cosx = 1) then exit;
  d := radius / ((1 - cosx) / sinx);

  an := (p1-p0)*(1/sqrt(a2));
  bn := (p1-p2)*(1/sqrt(b2));
  p3 := p1 - an*d;
  p4 := p1 - bn*d;
  anticlockwise := (dir < 0);

  cn := PointF(an.y,-an.x)*radius;
  if not anticlockwise then cn := -cn;
  c := p3 + cn;

  result.center := c;
  result.radius:= PointF(radius,radius);
  result.startAngleRadCW := arctan2((p3.y-c.y), (p3.x-c.x));
  result.endAngleRadCW := arctan2((p4.y-c.y), (p4.x-c.x));
  result.anticlockwise:= anticlockwise;
end;

function SvgArcTo(const p0: TPointF; rx, ry, xAngleRadCW: single; largeArc,
  anticlockwise: boolean; const p1: TPointF): TArcDef;
var
    p0p,cp: TPointF;
    cross1,cross2,lambda: single;
begin
  if (rx=0) or (ry=0) or (p0 = p1) then
  begin
    result.radius := PointF(0,0);
    result.xAngleRadCW:= 0;
    result.anticlockwise := false;
    result.endAngleRadCW := 0;
    result.startAngleRadCW:= 0;
    result.center := p1;
    exit;
  end;
  result.xAngleRadCW := xAngleRadCW;
  result.anticlockwise := anticlockwise;
  p0p := AffineMatrixRotationRad(xAngleRadCW)*( (p0-p1)*0.5 );

  //ensure radius is big enough
  lambda := sqr(p0p.x/rx) + sqr(p0p.y/ry);
  if lambda > 1 then
  begin
    lambda := sqrt(lambda);
    rx *= lambda;
    ry *= lambda;
  end;
  result.radius := PointF(rx,ry);

  //compute center
  cross2 := sqr(rx*p0p.y) + sqr(ry*p0p.x);
  cross1 := sqr(rx*ry);
  if cross1 <= cross2 then
    cp := PointF(0,0)
  else
    cp := sqrt((cross1-cross2)/cross2)*
       PointF(rx*p0p.y/ry, -ry*p0p.x/rx);
  if largeArc <> anticlockwise then cp := -cp;

  result.center := AffineMatrixRotationRad(-xAngleRadCW)*cp +
                  (p0+p1)*0.5;
  result.startAngleRadCW := arctan2((p0p.y-cp.y)/ry,(p0p.x-cp.x)/rx);
  result.endAngleRadCW := arctan2((-p0p.y-cp.y)/ry,(-p0p.x-cp.x)/rx);
end;

function ArcStartPoint(const arc: TArcDef): TPointF;
begin
  result := AffineMatrixRotationRad(-arc.xAngleRadCW)*PointF(cos(arc.startAngleRadCW)*arc.radius.x,
                                                       sin(arc.startAngleRadCW)*arc.radius.y) + arc.center;
end;

function ArcEndPoint(const arc: TArcDef): TPointF;
begin
  result := AffineMatrixRotationRad(-arc.xAngleRadCW)*PointF(cos(arc.endAngleRadCW)*arc.radius.x,
                                                       sin(arc.endAngleRadCW)*arc.radius.y) + arc.center;
end;

function IsLargeArc(const arc: TArcDef): boolean;
var diff,a1,a2: single;
begin
  a1 := arc.startAngleRadCW - floor(arc.startAngleRadCW/(2*Pi))*(2*Pi);
  a2 := arc.endAngleRadCW - floor(arc.endAngleRadCW/(2*Pi))*(2*Pi);
  if not arc.anticlockwise then
    diff := a2 - a1
  else
    diff := a1 - a2;
  result := (diff < 0) or (diff >= Pi);
end;

{ TBGRAPathCursor }

function TBGRAPathCursor.GetCurrentCoord: TPointF;
begin
  case FCurrentElementType of
    peNone: result := EmptyPointF;
    peMoveTo,peLineTo,peCloseSubPath:
      if FCurrentElementLength <= 0 then
        result := FCurrentElementStartCoord
      else
        result := FCurrentElementStartCoord + (FCurrentElementEndCoord-FCurrentElementStartCoord)*(FCurrentElementArcPos/FCurrentElementLength);
    peCubicBezierTo,peQuadraticBezierTo,peArc,peOpenedSpline,peClosedSpline:
      begin
        NeedPolygonalApprox;
        if FCurrentSegment >= high(FCurrentElementPoints) then
          result := FCurrentElementEndCoord
        else
          result := FCurrentElementPoints[FCurrentSegment]+
          (FCurrentElementPoints[FCurrentSegment+1]-
           FCurrentElementPoints[FCurrentSegment])*FCurrentSegmentPos;
      end;
    else
      raise Exception.Create('Unknown element type');
  end;
end;

function TBGRAPathCursor.GetPath: TBGRAPath;
begin
  if not Assigned(FPath) then
    raise exception.Create('Path does not exist');
  result := FPath;
end;

procedure TBGRAPathCursor.MoveToEndOfElement;
begin
  FCurrentElementArcPos := FCurrentElementLength;
  if not NeedPolygonalApprox then exit;
  if length(FCurrentElementPoints) > 1 then
  begin
    FCurrentSegment := high(FCurrentElementPoints)-1;
    FCurrentSegmentPos := 1;
  end else
  begin
    FCurrentSegment := high(FCurrentElementPoints);
    FCurrentSegmentPos := 0;
  end;
end;

procedure TBGRAPathCursor.MoveForwardInElement(ADistance: single);
var segLen,rightSpace,remaining: single;
begin
  if not NeedPolygonalApprox then exit;
  ADistance *= FCurrentElementArcPosScale;
  remaining := ADistance;
  while remaining > 0 do
  begin
    if FCurrentSegment < high(FCurrentElementPoints) then
      segLen := VectLen(FCurrentElementPoints[FCurrentSegment+1]-FCurrentElementPoints[FCurrentSegment])
    else
      segLen := 0;
    rightSpace := segLen*(1-FCurrentSegmentPos);
    if (segLen > 0) and (remaining <= rightSpace) then
    begin
      FCurrentSegmentPos += remaining/segLen;
      exit;
    end else
    begin
      remaining -= rightSpace;
      if FCurrentSegment < high(FCurrentElementPoints)-1 then
      begin
        inc(FCurrentSegment);
        FCurrentSegmentPos := 0;
      end else
      begin
        FCurrentSegmentPos := 1;
        exit;
      end;
    end;
  end;
end;

procedure TBGRAPathCursor.MoveBackwardInElement(ADistance: single);
var
  segLen,leftSpace,remaining: Single;
begin
  if not NeedPolygonalApprox then exit;
  ADistance *= FCurrentElementArcPosScale;
  remaining := ADistance;
  while remaining > 0 do
  begin
    if FCurrentSegment < high(FCurrentElementPoints) then
      segLen := VectLen(FCurrentElementPoints[FCurrentSegment+1]-FCurrentElementPoints[FCurrentSegment])
    else
      segLen := 0;
    leftSpace := segLen*FCurrentSegmentPos;
    if (segLen > 0) and (remaining <= leftSpace) then
    begin
      FCurrentSegmentPos -= remaining/segLen;
      exit;
    end else
    begin
      remaining -= leftSpace;
      if FCurrentSegment > 0 then
      begin
        dec(FCurrentSegment);
        FCurrentSegmentPos := 1;
      end else
      begin
        FCurrentSegmentPos := 0;
        exit;
      end;
    end;
  end;
end;

function TBGRAPathCursor.NeedPolygonalApprox: boolean;
begin
  if not (FCurrentElementType in[peQuadraticBezierTo,peCubicBezierTo,peArc,
  peOpenedSpline,peClosedSpline])
  then
  begin
    result := false;
    exit;
  end;
  result := true;
  if FCurrentElementPoints = nil then
  begin
    FCurrentElementPoints := Path.GetPolygonalApprox(FDataPos, FAcceptedDeviation, True);
    if FCurrentElementType = peQuadraticBezierTo then
    begin
      if FCurrentElementLength <> 0 then
        FCurrentElementArcPosScale := PolylineLen(FCurrentElementPoints)/FCurrentElementLength;
    end;
  end;
end;

function TBGRAPathCursor.GetArcPos: single;
var pos: PtrInt;
begin
  if FArcPos = EmptySingle then
  begin
    FArcPos := FCurrentElementArcPos;
    pos := FDataPos;
    while Path.GoToPreviousElement(pos) do
      FArcPos += Path.GetElementLength(pos, FAcceptedDeviation);
  end;
  result := FArcPos;
end;

function TBGRAPathCursor.GetCurrentTangent: TPointF;
var idxStart,idxEnd: integer;
  seg: TPointF;
begin
  while FCurrentElementLength <= 0 do
  begin
    if not GoToNextElement(False) then
    begin
      result := EmptyPointF;
      exit;
    end;
  end;
  case FCurrentElementType of
    peMoveTo,peLineTo,peCloseSubPath:
      result := (FCurrentElementEndCoord-FCurrentElementStartCoord)*(1/FCurrentElementLength);
    peCubicBezierTo,peQuadraticBezierTo,peArc,peOpenedSpline,peClosedSpline:
      begin
        NeedPolygonalApprox;
        idxStart := FCurrentSegment;
        if idxStart >= high(FCurrentElementPoints) then
          idxStart:= high(FCurrentElementPoints)-1;
        idxEnd := idxStart+1;
        if idxStart < 0 then
        begin
          result := EmptyPointF;
          exit;
        end;
        seg := FCurrentElementPoints[idxEnd] - FCurrentElementPoints[idxStart];
        while (seg.x = 0) and (seg.y = 0) and (idxEnd < high(FCurrentElementPoints)) do
        begin
          inc(idxEnd);
          seg := FCurrentElementPoints[idxEnd] - FCurrentElementPoints[idxStart];
        end;
        while (seg.x = 0) and (seg.y = 0) and (idxStart > 0) do
        begin
          dec(idxStart);
          seg := FCurrentElementPoints[idxEnd] - FCurrentElementPoints[idxStart];
        end;
        if (seg.x = 0) and (seg.y = 0) then
          result := EmptyPointF
        else
          result := seg*(1/VectLen(seg));
      end;
    else result := EmptyPointF;
  end;
end;

procedure TBGRAPathCursor.SetArcPos(AValue: single);
var oldLoopClosedShapes,oldLoopPath: boolean;
begin
  if GetArcPos=AValue then Exit;
  if (AValue > PathLength) and (PathLength <> 0) then
    AValue := AValue - trunc(AValue/PathLength)*PathLength
  else if (AValue < 0) then
    AValue := AValue + (trunc(-AValue/PathLength)+1)*PathLength;
  oldLoopClosedShapes:= LoopClosedShapes;
  oldLoopPath:= LoopPath;
  LoopClosedShapes:= false;
  LoopPath:= false;
  MoveForward(AValue-GetArcPos, True);
  LoopClosedShapes:= oldLoopClosedShapes;
  LoopPath:= oldLoopPath;
end;

function TBGRAPathCursor.GetPathLength: single;
begin
  if not FPathLengthComputed then
  begin
    FPathLength := Path.ComputeLength(FAcceptedDeviation);
    FPathLengthComputed := true;
  end;
  result := FPathLength;
end;

procedure TBGRAPathCursor.OnPathFree;
begin
  FPath := nil;
end;

function TBGRAPathCursor.GetLoopClosedShapes: boolean;
begin
  result := FLoopClosedShapes;
end;

function TBGRAPathCursor.GetLoopPath: boolean;
begin
  result := FLoopPath;
end;

function TBGRAPathCursor.GetStartCoordinate: TPointF;
begin
  result := FStartCoordinate;
end;

procedure TBGRAPathCursor.SetLoopClosedShapes(AValue: boolean);
begin
  FLoopClosedShapes := AValue;
end;

procedure TBGRAPathCursor.SetLoopPath(AValue: boolean);
begin
  FLoopPath := AValue;
end;

procedure TBGRAPathCursor.PrepareCurrentElement;
begin
  Path.GetElementAt(FDataPos, FCurrentElementType, FCurrentElement);
  FCurrentElementLength := 0;
  FCurrentElementArcPos := 0;
  FCurrentElementPoints := nil;
  FCurrentSegment := 0;
  FCurrentSegmentPos := 0;
  FCurrentElementArcPosScale := 1;
  if FCurrentElementType = peNone then
  begin
    FCurrentElementStartCoord := EmptyPointF;
    FCurrentElementEndCoord := EmptyPointF;
  end
  else
  begin
    FCurrentElementStartCoord := Path.GetElementStartCoord(FDataPos);
    case FCurrentElementType of
      peLineTo, peCloseSubPath:
        begin
          FCurrentElementEndCoord := PPointF(FCurrentElement)^;
          FCurrentElementLength := VectLen(FCurrentElementEndCoord - FCurrentElementStartCoord);
        end;
      peQuadraticBezierTo: with PQuadraticBezierToElement(FCurrentElement)^ do
        begin
          FCurrentElementEndCoord := Destination;
          FCurrentElementLength := BGRABitmapTypes.BezierCurve(FCurrentElementStartCoord,ControlPoint,Destination).ComputeLength;
        end;
      peCubicBezierTo,peArc,peOpenedSpline,peClosedSpline:
        begin
          NeedPolygonalApprox;
          FCurrentElementEndCoord := FCurrentElementPoints[high(FCurrentElementPoints)];
          FCurrentElementLength := PolylineLen(FCurrentElementPoints);
        end;
    else
      FCurrentElementEndCoord := FCurrentElementStartCoord;
    end;
  end;
end;

function TBGRAPathCursor.GetBounds: TRectF;
begin
  if not FBoundsComputed then
  begin
    FBounds:= Path.GetBounds(FAcceptedDeviation);
    FBoundsComputed := true;
  end;
  result := FBounds;
end;

function TBGRAPathCursor.GoToNextElement(ACanJump: boolean): boolean;
begin
  if (FCurrentElementType = peCloseSubPath) and
   (PClosePathElement(FCurrentElement)^.LoopDataPos <> -1) and
   (  FLoopClosedShapes or
      (FLoopPath and (PClosePathElement(FCurrentElement)^.LoopDataPos = 0))
   ) then
  begin
    if PClosePathElement(FCurrentElement)^.LoopDataPos <> FDataPos then
    begin
      result := true;
      FDataPos := PClosePathElement(FCurrentElement)^.LoopDataPos;
      FArcPos := EmptySingle;
      PrepareCurrentElement;
    end else
      result := false;
  end;
  if not ACanJump and ((FCurrentElementType = peCloseSubPath)
   or (Path.PeekNextElement(FDataPos) = peMoveTo)) then
  begin
    result := false;
    exit;
  end;
  if Path.GoToNextElement(FDataPos) then
  begin
    result := true;
    PrepareCurrentElement;
  end
  else
  begin
    if ACanJump and FLoopPath and (FDataPos > 0) then
    begin
      result := true;
      FDataPos := 0;
      FArcPos := EmptySingle;
      PrepareCurrentElement;
    end else
      result := false;
  end;
end;

function TBGRAPathCursor.GoToPreviousElement(ACanJump: boolean): boolean;
var lastElemPos: IntPtr;
begin
  if (FCurrentElementType = peMoveTo) and (PMoveToElement(FCurrentElement)^.LoopDataPos <> -1) and
    ( FLoopClosedShapes or
      (FLoopPath and (FDataPos = 0))
    ) then
  with PMoveToElement(FCurrentElement)^ do
  begin
    if LoopDataPos <> -1 then
    begin
      result := true;
      FDataPos := LoopDataPos;
      FArcPos := EmptySingle;
      PrepareCurrentElement;
    end;
  end;
  if not ACanJump and (FCurrentElementType = peMoveTo) then
  begin
    result := false;
    exit;
  end;
  if Path.GoToPreviousElement(FDataPos) then
  begin
    result := true;
    PrepareCurrentElement;
  end
  else
  begin
    if FLoopPath then
    begin
      lastElemPos := FPath.FDataPos;
      if (lastElemPos > 0) and FPath.GoToPreviousElement(lastElemPos) then
      begin
        if lastElemPos > 0 then
        begin
          result := true;
          FDataPos := lastElemPos;
          PrepareCurrentElement;
          FArcPos := EmptySingle;
          exit;
        end;
      end;
    end;
    result := false;
  end;
end;

constructor TBGRAPathCursor.Create(APath: TBGRAPath; AAcceptedDeviation: single);
begin
  FPath := APath;
  FPathLengthComputed := false;
  FBoundsComputed:= false;
  FDataPos := 0;
  FArcPos:= 0;
  FAcceptedDeviation:= AAcceptedDeviation;
  Path.RegisterCursor(self);
  PrepareCurrentElement;

  FStartCoordinate := FCurrentElementStartCoord;
  if isEmptyPointF(FStartCoordinate) then
    raise exception.Create('Path does not has a starting coordinate');
  FEndCoordinate := Path.FLastTransformedCoord;
  if isEmptyPointF(FEndCoordinate) then
    raise exception.Create('Path does not has an ending coordinate');
end;

function TBGRAPathCursor.MoveForward(ADistance: single; ACanJump: boolean): single;
var newArcPos,step,remaining: single;
begin
  if ADistance < 0 then
  begin
    result := -MoveBackward(-ADistance, ACanJump);
    exit;
  end;
  result := 0;
  remaining := ADistance;
  while remaining > 0 do
  begin
    newArcPos := FCurrentElementArcPos + remaining;
    if newArcPos > FCurrentElementLength then
    begin
      step := FCurrentElementLength - FCurrentElementArcPos;
      result += step;
      remaining -= step;
      if not GoToNextElement(ACanJump) then
      begin
        MoveForwardInElement(step);
        FCurrentElementArcPos := FCurrentElementLength;
        FArcPos := PathLength;
        exit;
      end;
    end else
    begin
      MoveForwardInElement(remaining);
      FCurrentElementArcPos := newArcPos;
      result := ADistance;
      break;
    end;
  end;
  if FArcPos <> EmptySingle then
    FArcPos += result;
end;

function TBGRAPathCursor.MoveBackward(ADistance: single; ACanJump: boolean = true): single;
var
  remaining: Single;
  newArcPos: Single;
  step: Single;
begin
  if ADistance = 0 then
  begin
    result := 0;
    exit;
  end;
  if ADistance < 0 then
  begin
    result := -MoveForward(-ADistance, ACanJump);
    exit;
  end;
  result := 0;
  remaining := ADistance;
  while remaining > 0 do
  begin
    newArcPos := FCurrentElementArcPos - remaining;
    if newArcPos < 0 then
    begin
      step := FCurrentElementArcPos;
      result += step;
      remaining -= step;
      if not GoToPreviousElement(ACanJump) then
      begin
        MoveBackwardInElement(step);
        FCurrentElementArcPos := 0;
        FArcPos := 0;
        exit;
      end else
        MoveToEndOfElement;
    end else
    begin
      MoveBackwardInElement(remaining);
      FCurrentElementArcPos := newArcPos;
      result := ADistance;
      break;
    end;
  end;
  if FArcPos <> EmptySingle then
    FArcPos -= result;
end;

destructor TBGRAPathCursor.Destroy;
begin
  if Assigned(FPath) then
  begin
    FPath.UnregisterCursor(self);
  end;
  inherited Destroy;
end;

{ TBGRAPath }

function TBGRAPath.ComputeLength(AAcceptedDeviation: single): single;
var pos: PtrInt;
begin
  pos := 0;
  result := 0;
  repeat
    result += GetElementLength(pos, AAcceptedDeviation);
  until not GoToNextElement(pos);
end;

function TBGRAPath.ToPoints(AAcceptedDeviation: single): ArrayOfTPointF;
var sub: array of ArrayOfTPointF;
    temp: ArrayOfTPointF;
    nbSub,nbPts,curPt,curSub: NativeInt;
    startPos,pos: PtrInt;
    elemType: TBGRAPathElementType;
    elem: pointer;
begin
  pos := 0;
  nbSub := 0;
  repeat
    GetElementAt(pos, elemType, elem);
    if elem = nil then break;
    case elemType of
      peMoveTo,peLineTo,peCloseSubPath: begin
          inc(nbSub);
          while PeekNextElement(pos) in[peLineTo,peCloseSubPath] do
            GoToNextElement(pos);
        end;
      peQuadraticBezierTo, peCubicBezierTo, peArc, peOpenedSpline, peClosedSpline: inc(nbSub);
    end;
  until not GoToNextElement(pos);

  pos := 0;
  setlength(sub, nbSub);
  curSub := 0;
  repeat
    GetElementAt(pos, elemType, elem);
    if elem = nil then break;
    case elemType of
      peMoveTo,peLineTo,peCloseSubPath: begin
          startPos := pos;
          if (elemType = peMoveTo) and (curSub > 0) then
            nbPts := 2
          else
            nbPts := 1;
          while PeekNextElement(pos) in[peLineTo,peCloseSubPath] do
          begin
            GoToNextElement(pos);
            inc(nbPts);
          end;
          setlength(temp, nbPts);
          pos := startPos;
          if (elemType = peMoveTo) and (curSub > 0) then
          begin
            temp[0] := EmptyPointF;
            temp[1] := PPointF(elem)^;
            curPt := 2;
          end else
          begin
            temp[0] := PPointF(elem)^;
            curPt := 1;
          end;
          while PeekNextElement(pos) in[peLineTo,peCloseSubPath] do
          begin
            GoToNextElement(pos);
            GetElementAt(pos, elemType, elem);
            temp[curPt] := PPointF(elem)^;
            inc(curPt);
          end;
          sub[curSub] := temp;
          inc(curSub);
          temp := nil;
        end;
      peQuadraticBezierTo,peCubicBezierTo,peArc,
      peOpenedSpline, peClosedSpline:
        begin
          sub[curSub] := GetPolygonalApprox(pos, AAcceptedDeviation, False);
          inc(curSub);
        end;
    end;
  until not GoToNextElement(pos) or (curSub = nbSub);
  result := ConcatPointsF(sub);
end;

function TBGRAPath.ToPoints(AMatrix: TAffineMatrix; AAcceptedDeviation: single): ArrayOfTPointF;
begin
  AAcceptedDeviation:= CorrectAcceptedDeviation(AAcceptedDeviation,AMatrix);
  result := ToPoints(AAcceptedDeviation);
  if not IsAffineMatrixIdentity(AMatrix) then
    result := AMatrix*result;
end;

function TBGRAPath.IsEmpty: boolean;
begin
  result := FDataPos = 0;
end;

function TBGRAPath.GetBounds(AAcceptedDeviation: single): TRectF;
var empty: boolean;
    pos: PtrInt;
    elemType: TBGRAPathElementType;
    elem: pointer;
    temp: array of TPointF;
    i: integer;

  procedure Include(pt: TPointF);
  begin
    if empty then
    begin
      result.TopLeft := pt;
      result.BottomRight := pt;
      empty := false;
    end else
    begin
      if pt.x < result.Left then result.Left := pt.x
      else if pt.x > result.Right then result.Right := pt.x;
      if pt.y < result.Top then result.Top := pt.y
      else if pt.y > result.Bottom then result.Bottom := pt.y;
    end;
  end;

  procedure IncludeRect(r: TRectF);
  begin
    Include(r.TopLeft);
    Include(r.BottomRight);
  end;

begin
  empty := true;
  result := RectF(0,0,0,0);
  pos := 0;
  repeat
    GetElementAt(pos, elemType, elem);
    if elem = nil then break;
    case elemType of
      peMoveTo,peLineTo,peCloseSubPath: begin
          Include(PPointF(elem)^);
          while PeekNextElement(pos) in[peLineTo,peCloseSubPath] do
          begin
            GoToNextElement(pos);
            GetElementAt(pos, elemType, elem);
            Include(PPointF(elem)^);
          end;
        end;
      peCubicBezierTo:
        with PCubicBezierToElement(elem)^ do
          IncludeRect(BGRABitmapTypes.BezierCurve(GetElementStartCoord(pos),ControlPoint1,ControlPoint2,Destination).GetBounds);
      peQuadraticBezierTo:
        with PQuadraticBezierToElement(elem)^ do
          IncludeRect(BGRABitmapTypes.BezierCurve(GetElementStartCoord(pos),ControlPoint,Destination).GetBounds);
      peArc, peOpenedSpline, peClosedSpline:
        begin
          temp := GetPolygonalApprox(pos, AAcceptedDeviation, False);
          for i := 0 to high(temp) do
            Include(temp[i]);
        end;
    end;
  until not GoToNextElement(pos);
  if empty then raise exception.Create('Path is empty');
end;

procedure TBGRAPath.SetPoints(const APoints: ArrayOfTPointF);
var i: integer;
    nextIsMoveTo: boolean;
    startPoint: TPointF;
begin
  beginPath;
  if length(APoints) = 0 then exit;
  NeedSpace((sizeof(TPathElementHeader)+sizeof(TPointF))*length(APoints));
  nextIsMoveTo:= true;
  startPoint := EmptyPointF;
  for i := 0 to high(APoints) do
  begin
    if isEmptyPointF(APoints[i]) then
      nextIsMoveTo:= true
    else
    if nextIsMoveTo then
    begin
      startPoint := APoints[i];
      moveTo(startPoint);
      nextIsMoveTo:= false;
    end
    else
    begin
      with APoints[i] do
        if (x = startPoint.x) and (y = startPoint.y) then
          closePath
        else
          lineTo(APoints[i]);
    end;
  end;
end;

procedure TBGRAPath.stroke(ABitmap: TBGRACustomBitmap; AColor: TBGRAPixel;
  AWidth: single; AAcceptedDeviation: single);
begin
  stroke(ABitmap,AffineMatrixIdentity,AColor,AWidth,AAcceptedDeviation);
end;

procedure TBGRAPath.stroke(ABitmap: TBGRACustomBitmap; ATexture: IBGRAScanner;
  AWidth: single; AAcceptedDeviation: single);
begin
  stroke(ABitmap,AffineMatrixIdentity,ATexture,AWidth,AAcceptedDeviation);
end;

procedure TBGRAPath.stroke(ABitmap: TBGRACustomBitmap; x, y: single;
  AColor: TBGRAPixel; AWidth: single; AAcceptedDeviation: single);
begin
  stroke(ABitmap,AffineMatrixTranslation(x,y),AColor,AWidth,AAcceptedDeviation);
end;

procedure TBGRAPath.stroke(ABitmap: TBGRACustomBitmap; x, y: single;
  ATexture: IBGRAScanner; AWidth: single; AAcceptedDeviation: single);
begin
  stroke(ABitmap,AffineMatrixTranslation(x,y),ATexture,AWidth,AAcceptedDeviation);
end;

procedure TBGRAPath.stroke(ABitmap: TBGRACustomBitmap; const AMatrix: TAffineMatrix;
  AColor: TBGRAPixel; AWidth: single; AAcceptedDeviation: single);
var data: TStrokeData;
begin
  data.Bitmap := ABitmap;
  data.Texture := nil;
  data.Color := AColor;
  data.Width := AWidth;
  InternalDraw(@BitmapDrawSubPathProc, AMatrix, AAcceptedDeviation, @data);
end;

procedure TBGRAPath.stroke(ABitmap: TBGRACustomBitmap; const AMatrix: TAffineMatrix;
  ATexture: IBGRAScanner; AWidth: single; AAcceptedDeviation: single);
var data: TStrokeData;
begin
  data.Bitmap := ABitmap;
  data.Texture := ATexture;
  data.Color := BGRAPixelTransparent;
  data.Width := AWidth;
  InternalDraw(@BitmapDrawSubPathProc, AMatrix, AAcceptedDeviation, @data);
end;

procedure TBGRAPath.stroke(ADrawProc: TBGRAPathDrawProc;
  const AMatrix: TAffineMatrix; AAcceptedDeviation: single; AData: pointer);
begin
  InternalDraw(ADrawProc,AMatrix,AAcceptedDeviation,AData);
end;

procedure TBGRAPath.fill(ABitmap: TBGRACustomBitmap; AColor: TBGRAPixel;
  AAcceptedDeviation: single);
begin
  fill(ABitmap,AffineMatrixIdentity,AColor,AAcceptedDeviation);
end;

procedure TBGRAPath.fill(ABitmap: TBGRACustomBitmap; ATexture: IBGRAScanner;
  AAcceptedDeviation: single);
begin
  fill(ABitmap,AffineMatrixIdentity,ATexture,AAcceptedDeviation);
end;

procedure TBGRAPath.fill(ABitmap: TBGRACustomBitmap; x, y: single;
  AColor: TBGRAPixel; AAcceptedDeviation: single);
begin
  fill(ABitmap,AffineMatrixTranslation(x,y),AColor,AAcceptedDeviation);
end;

procedure TBGRAPath.fill(ABitmap: TBGRACustomBitmap; x, y: single;
  ATexture: IBGRAScanner; AAcceptedDeviation: single);
begin
  fill(ABitmap,AffineMatrixTranslation(x,y),ATexture,AAcceptedDeviation);
end;

procedure TBGRAPath.fill(ABitmap: TBGRACustomBitmap; const AMatrix: TAffineMatrix;
  AColor: TBGRAPixel; AAcceptedDeviation: single);
begin
  ABitmap.FillPolyAntialias(ToPoints(AMatrix,AAcceptedDeviation), AColor);
end;

procedure TBGRAPath.fill(ABitmap: TBGRACustomBitmap; const AMatrix: TAffineMatrix;
  ATexture: IBGRAScanner; AAcceptedDeviation: single);
begin
  ABitmap.FillPolyAntialias(ToPoints(AMatrix,AAcceptedDeviation), ATexture);
end;

procedure TBGRAPath.fill(AFillProc: TBGRAPathFillProc; const AMatrix: TAffineMatrix;
  AAcceptedDeviation: single; AData: pointer);
begin
  AFillProc(ToPoints(AMatrix,AAcceptedDeviation), AData);
end;

function TBGRAPath.CreateCursor(AAcceptedDeviation: single): TBGRAPathCursor;
begin
  result := TBGRAPathCursor.Create(self, AAcceptedDeviation);
end;

procedure TBGRAPath.Fit(ARect: TRectF; AAcceptedDeviation: single);
var
  temp: TBGRAPath;
begin
  temp := TBGRAPath.Create;
  copyTo(temp);
  temp.FitInto(self, ARect, AAcceptedDeviation);
  temp.Free;
end;

procedure TBGRAPath.FitInto(ADest: TBGRAPath; ARect: TRectF;
  AAcceptedDeviation: single);
var bounds: TRectF;
    zoomX,zoomY: single;
begin
  bounds := GetBounds(AAcceptedDeviation);
  ADest.beginPath;
  ADest.translate((ARect.Left+ARect.Right)*0.5, (ARect.Bottom+ARect.Top)*0.5);
  if bounds.Right-bounds.Left <> 0 then
  begin
    zoomX := (ARect.Right-ARect.Left)/(bounds.Right-bounds.Left);
    if bounds.Bottom-bounds.Top > 0 then
    begin
      zoomY := (ARect.Bottom-ARect.Top)/(bounds.Bottom-bounds.Top);
      if zoomY < zoomX then ADest.scale(zoomY) else ADest.scale(zoomX);
    end else
      ADest.scale(zoomX);
  end else
  if bounds.Bottom-bounds.Top > 0 then
  begin
    zoomY := (ARect.Bottom-ARect.Top)/(bounds.Bottom-bounds.Top);
    ADest.scale(zoomY);
  end;
  ADest.translate(-(bounds.Left+bounds.Right)*0.5, -(bounds.Bottom+bounds.Top)*0.5);
  copyTo(ADest);
  ADest.resetTransform;
end;

function TBGRAPath.GetSvgString: string;
const RadToDeg = 180/Pi;
var
  formats: TFormatSettings;
  lastPosF: TPointF;
  implicitCommand: char;

  function FloatToString(value: single): string;
  begin
    result := FloatToStrF(value,ffGeneral,7,0,formats)+' ';
  end;

  function CoordToString(const pt: TPointF): string;
  begin
    lastPosF := pt;
    result := FloatToString(pt.x)+FloatToString(pt.y);
  end;

  function BoolToString(value: boolean): string;
  begin
    if value then
      result := '1 ' else result := '0 ';
  end;

  procedure addCommand(command: char; parameters: string);
  begin
    if result <> '' then result += ' '; //optional whitespace
    if command <> implicitCommand then result += command;
    result += trim(parameters);
    if command = 'M' then implicitCommand:= 'L'
    else if command = 'm' then implicitCommand:= 'l'
    else if command in['z','Z'] then implicitCommand:= #0
    else implicitCommand := command;
  end;

var elemType: TBGRAPathElementType;
    elem: pointer;
    a: PArcElement;
    Pos: PtrInt;
    p1: TPointF;
    pts: array of TPointF;
    i: integer;
begin
  formats := DefaultFormatSettings;
  formats.DecimalSeparator := '.';

  result := '';
  Pos := 0;
  lastPosF := EmptyPointF;
  implicitCommand := #0;
  repeat
    GetElementAt(Pos, elemType, elem);
    if elem = nil then break;
    case elemType of
      peMoveTo: addCommand('M',CoordToString(PPointF(elem)^));
      peLineTo: addCommand('L',CoordToString(PPointF(elem)^));
      peCloseSubPath: addCommand('z','');
      peQuadraticBezierTo:
        with PQuadraticBezierToElement(elem)^ do
          addCommand('Q',CoordToString(ControlPoint)+CoordToString(Destination));
      peCubicBezierTo:
        with PCubicBezierToElement(elem)^ do
          addCommand('C',CoordToString(ControlPoint1)+
               CoordToString(ControlPoint2)+CoordToString(Destination));
      peArc:
        begin
          a := PArcElement(elem);
          p1 := ArcStartPoint(a^);
          if isEmptyPointF(lastPosF) or (p1 <> lastPosF) then
            addCommand('L',CoordToString(p1));
          addCommand('A',CoordToString(a^.radius)+
             FloatToString(a^.xAngleRadCW*RadToDeg)+
             BoolToString(IsLargeArc(a^))+
             BoolToString(not a^.anticlockwise)+
             CoordToString(ArcEndPoint(a^)));
        end;
      peOpenedSpline, peClosedSpline:
        begin
          pts := GetPolygonalApprox(Pos, 0.1,True);
          for i := 0 to high(pts) do
          begin
            if isEmptyPointF(lastPosF) then
              addCommand('M',CoordToString(pts[i]))
            else
              addCommand('L',CoordToString(pts[i]));
          end;
        end;
    end;
  until not GoToNextElement(Pos);
end;

procedure TBGRAPath.SetSvgString(const AValue: string);
begin
  resetTransform;
  beginPath;
  addPath(AValue);
end;

procedure TBGRAPath.RegisterCursor(ACursor: TBGRAPathCursor);
begin
  setlength(FCursors, length(FCursors)+1);
  FCursors[high(FCursors)] := ACursor;
end;

procedure TBGRAPath.UnregisterCursor(ACursor: TBGRAPathCursor);
var
  i,j: Integer;
begin
  for i := high(FCursors) downto 0 do
    if FCursors[i] = ACursor then
    begin
      for j := i to high(FCursors)-1 do
        FCursors[j] := FCursors[j+1];
      setlength(FCursors, length(FCursors)-1);
      exit;
    end;
end;

function TBGRAPath.SetLastCoord(ACoord: TPointF): TPointF;
begin
  FLastCoord := ACoord;
  FLastTransformedCoord := FMatrix*ACoord;
  result := FLastTransformedCoord;
end;

procedure TBGRAPath.ClearLastCoord;
begin
  FLastCoord := EmptyPointF;
  FLastTransformedCoord := EmptyPointF;
end;

procedure TBGRAPath.BezierCurveFromTransformed(tcp1, cp2, pt: TPointF);
begin
  with PCubicBezierToElement(AllocateElement(peCubicBezierTo))^ do
  begin
    ControlPoint1 := tcp1;
    ControlPoint2 := FMatrix*cp2;
    Destination := SetLastCoord(pt);
    FExpectedTransformedControlPoint := Destination + (Destination-ControlPoint2);
  end;
end;

procedure TBGRAPath.QuadraticCurveFromTransformed(tcp, pt: TPointF);
begin
  with PQuadraticBezierToElement(AllocateElement(peQuadraticBezierTo))^ do
  begin
    ControlPoint := tcp;
    Destination := SetLastCoord(pt);
    FExpectedTransformedControlPoint := Destination+(Destination-ControlPoint);
  end;
end;

function TBGRAPath.LastCoordDefined: boolean;
begin
  result := not isEmptyPointF(FLastTransformedCoord);
end;

function TBGRAPath.GetPolygonalApprox(APos: IntPtr; AAcceptedDeviation: single; AIncludeFirstPoint: boolean): ArrayOfTPointF;
var pts: ArrayOfTPointF;
  elemType: TBGRAPathElementType;
  elem: pointer;
  pt : TPointF;
  i: NativeInt;
begin
  GetElementAt(APos, elemType, elem);
  case elemType of
    peQuadraticBezierTo:
      with PQuadraticBezierToElement(elem)^ do
        result := BGRABitmapTypes.BezierCurve(GetElementStartCoord(APos),ControlPoint,Destination).ToPoints(AAcceptedDeviation, AIncludeFirstPoint);
    peCubicBezierTo:
      with PCubicBezierToElement(elem)^ do
        result := BGRABitmapTypes.BezierCurve(GetElementStartCoord(APos),ControlPoint1,ControlPoint2,Destination).ToPoints(AAcceptedDeviation, AIncludeFirstPoint);
    peArc:
      begin
        result := ComputeArc(PArcElement(elem)^, 0.1/AAcceptedDeviation);
        pt := GetElementStartCoord(APos);
        if pt <> result[0] then
        begin
          setlength(result, length(result)+1);
          for i := high(result) downto 1 do
            result[i] := result[i-1];
          result[0] := pt;
        end;
      end;
    peOpenedSpline, peClosedSpline:
      with PSplineElement(elem)^ do
      begin
        setlength(pts, NbControlPoints);
        move(Pointer(PSplineElement(elem)+1)^, pts[0], NbControlPoints*sizeof(TPointF));
        if elemType = peOpenedSpline then
          result := ComputeOpenedSpline(pts, SplineStyle, 0.25, AAcceptedDeviation)
        else
          result := ComputeClosedSpline(pts, SplineStyle, AAcceptedDeviation);
      end;
  end;
end;

function TBGRAPath.getPoints: ArrayOfTPointF;
begin
  result := ToPoints;
end;

function TBGRAPath.getPoints(AMatrix: TAffineMatrix): ArrayOfTPointF;
begin
  result := ToPoints(AMatrix);
end;

function TBGRAPath.getCursor: TBGRACustomPathCursor;
begin
  result := CreateCursor;
end;

procedure TBGRAPath.InternalDraw(ADrawProc: TBGRAPathDrawProc;
  const AMatrix: TAffineMatrix; AAcceptedDeviation: single; AData: pointer);
var
  nbSub: NativeInt;

  procedure OutputSub(subPathStartPos, subPathEndPos: IntPtr);
  var
    sub: array of ArrayOfTPointF;
    temp: ArrayOfTPointF;
    startPos,pos,nbPts,curPt,curSub: NativeInt;
    elemType: TBGRAPathElementType;
    elem: pointer;
  begin
    pos := subPathStartPos;
    setlength(sub, nbSub);
    curSub := 0;
    while (pos <= subPathEndPos) and (curSub < nbSub) do
    begin
      GetElementAt(pos, elemType, elem);
      if elem = nil then break;
      case elemType of
        peMoveTo,peLineTo,peCloseSubPath: begin
            startPos := pos;
            if (elemType = peMoveTo) and (curSub > 0) then
              nbPts := 2
            else
              nbPts := 1;
            while PeekNextElement(pos) in[peLineTo,peCloseSubPath] do
            begin
              GoToNextElement(pos);
              inc(nbPts);
            end;
            setlength(temp, nbPts);
            pos := startPos;
            if (elemType = peMoveTo) and (curSub > 0) then
            begin
              temp[0] := EmptyPointF;
              temp[1] := PPointF(elem)^;
              curPt := 2;
            end else
            begin
              temp[0] := PPointF(elem)^;
              curPt := 1;
            end;
            while PeekNextElement(pos) in[peLineTo,peCloseSubPath] do
            begin
              GoToNextElement(pos);
              GetElementAt(pos, elemType, elem);
              temp[curPt] := PPointF(elem)^;
              inc(curPt);
            end;
            sub[curSub] := temp;
            inc(curSub);
            temp := nil;
          end;
        peQuadraticBezierTo,peCubicBezierTo,peArc,
        peOpenedSpline, peClosedSpline:
          begin
            sub[curSub] := GetPolygonalApprox(pos, AAcceptedDeviation, False);
            inc(curSub);
          end;
      end;
      GoToNextElement(pos);
    end;
    temp := ConcatPointsF(sub);
    if not IsAffineMatrixIdentity(AMatrix) then
      temp := AMatrix*temp;
    if (elemType = peCloseSubPath) or ((curSub = 2) and (elemType = peClosedSpline)) then
      ADrawProc(temp, True, AData)
    else
      ADrawProc(temp, False, AData);
  end;

var
  subPathStartPos: IntPtr;
  prevPos,pos: PtrInt;
  elemType: TBGRAPathElementType;
  elem: pointer;
begin
  AAcceptedDeviation := CorrectAcceptedDeviation(AAcceptedDeviation, AMatrix);
  pos := 0;
  nbSub := 0;
  subPathStartPos := pos;
  repeat
    prevPos := pos;
    GetElementAt(pos, elemType, elem);
    if elem = nil then
    begin
      pos := prevPos;
      break;
    end;
    if (elemType = peMoveTo) and (nbSub > 0) then
    begin
      OutputSub(subPathStartPos,prevPos);
      nbSub := 0;
      subPathStartPos := pos;
    end;
    case elemType of
      peMoveTo,peLineTo,peCloseSubPath: begin
          inc(nbSub);
          while PeekNextElement(pos) in[peLineTo,peCloseSubPath] do
            GoToNextElement(pos);
        end;
      peQuadraticBezierTo, peCubicBezierTo, peArc, peOpenedSpline, peClosedSpline: inc(nbSub);
    end;
  until not GoToNextElement(pos);
  if nbSub > 0 then OutputSub(subPathStartPos,pos);
end;

procedure TBGRAPath.addPath(const AValue: string);
var p: integer;
    numberError: boolean;
    startCoord,lastCoord: TPointF;

  function parseFloat: single;
  var numberStart: integer;
      errPos: integer;
  begin
    while (p <= length(AValue)) and (AValue[p] in[#0..#32,',']) do inc(p);
    numberStart:= p;
    if (p <= length(AValue)) and (AValue[p] in['+','-']) then inc(p);
    while (p <= length(AValue)) and (AValue[p] in['0'..'9','.']) do inc(p);
    if (p <= length(AValue)) and (AValue[p] in['e','E']) then
    begin
      inc(p);
      if (p <= length(AValue)) and (AValue[p] in['+','-']) then inc(p);
      while (p <= length(AValue)) and (AValue[p] in['0'..'9','.']) do inc(p);
    end;
    val(copy(AValue,numberStart,p-numberStart),result,errPos);
    if errPos <> 0 then numberError := true;
  end;

  function parseCoord(relative: boolean): TPointF;
  begin
    result.x := parseFloat;
    result.y := parseFloat;
    if relative and not isEmptyPointF(lastCoord) then result += lastCoord;
    if isEmptyPointF(lastCoord) then startCoord := result;
  end;

var
  command,implicitCommand: char;
  relative: boolean;
  c1,c2,p1: TPointF;
  a: TArcDef;
  largeArc: boolean;
begin
  BeginSubPath;
  lastCoord := EmptyPointF;
  startCoord := EmptyPointF;
  p := 1;
  implicitCommand:= #0;
  while p <= length(AValue) do
  begin
    command := AValue[p];
    if (command in['0'..'9','.','+','-']) and (implicitCommand <> #0) then
      command := implicitCommand
    else
    begin
      inc(p);
    end;
    relative := (command = lowerCase(command));
    numberError := false;
    if upcase(command) in ['L','H','V','C','S','Q','T','A'] then
      implicitCommand:= command; //by default the command repeats
    case upcase(command) of
    'Z': begin
           closePath;
           implicitCommand:= #0;
           lastCoord := startCoord;
         end;
    'M': begin
           p1 := parseCoord(relative);
           if not numberError then
           begin
             moveTo(p1);
             lastCoord := p1;
           end;
           if relative then implicitCommand:= 'l' else
             implicitCommand:= 'L';
      end;
    'L': begin
           p1 := parseCoord(relative);
           if not numberError then
           begin
             lineTo(p1);
             lastCoord := p1;
           end;
      end;
    'H': begin
        if not isEmptyPointF(lastCoord) then
        begin
          p1 := lastCoord;
          if relative then p1.x += parseFloat
          else p1.x := parseFloat;
        end else
        begin
          p1 := PointF(parseFloat,0);
          lastCoord := p1;
          startCoord := p1;
        end;
        if not numberError then
        begin
          lineTo(p1);
          lastCoord := p1;
        end;
      end;
    'V': begin
        if not isEmptyPointF(lastCoord) then
        begin
          p1 := lastCoord;
          if relative then p1.y += parseFloat
          else p1.y := parseFloat;
        end else
        begin
          p1 := PointF(0,parseFloat);
          lastCoord := p1;
          startCoord := p1;
        end;
        if not numberError then
        begin
          lineTo(p1);
          lastCoord := p1;
        end;
      end;
    'C': begin
        c1 := parseCoord(relative);
        c2 := parseCoord(relative);
        p1 := parseCoord(relative);
        if not numberError then
        begin
          bezierCurveTo(c1,c2,p1);
          lastCoord := p1;
        end;
      end;
    'S': begin
        c2 := parseCoord(relative);
        p1 := parseCoord(relative);
        if not numberError then
        begin
          smoothBezierCurveTo(c2,p1);
          lastCoord := p1;
        end;
      end;
    'Q': begin
        c1 := parseCoord(relative);
        p1 := parseCoord(relative);
        if not numberError then
        begin
          quadraticCurveTo(c1,p1);
          lastCoord := p1;
        end;
      end;
    'T': begin
        p1 := parseCoord(relative);
        if not numberError then
        begin
          smoothQuadraticCurveTo(p1);
          lastCoord := p1;
        end;
    end;
    'A':
      begin
        a.radius.x := parseFloat;
        a.radius.y := parseFloat;
        a.xAngleRadCW := parseFloat*Pi/180;
        largeArc := parseFloat<>0;
        a.anticlockwise:= parseFloat=0;
        p1 := parseCoord(relative);
        if not numberError then
        begin
          arcTo(a.radius.x,a.radius.y,a.xAngleRadCW,largeArc,a.anticlockwise,p1.x,p1.y);
          lastCoord := p1;
        end;
      end;
    end;
  end;
end;

procedure TBGRAPath.addPath(source: IBGRAPath);
begin
  source.copyTo(self);
end;

procedure TBGRAPath.openedSpline(const pts: array of TPointF;
  style: TSplineStyle);
var elem: PSplineElement;
  i: NativeInt;
  p: PPointF;
begin
  if length(pts) <= 2 then
  begin
    polyline(pts);
    exit;
  end;
  if not LastCoordDefined then moveTo(pts[0]);
  elem := AllocateElement(peOpenedSpline, length(pts)*sizeof(TPointF));
  elem^.NbControlPoints := length(pts);
  elem^.SplineStyle := style;
  p := PPointF(elem+1);
  for i := 0 to high(pts)-1 do
  begin
    p^ := FMatrix*pts[i];
    inc(p);
  end;
  p^ := SetLastCoord(pts[high(pts)]);
  inc(p);
  PInteger(p)^ := length(pts);
end;

procedure TBGRAPath.closedSpline(const pts: array of TPointF;
  style: TSplineStyle);
var elem: PSplineElement;
  i: NativeInt;
  p: PPointF;
begin
  if length(pts) = 0 then exit;
  if not LastCoordDefined then moveTo(ClosedSplineStartPoint(pts, style));
  if length(pts) <= 2 then exit;
  elem := AllocateElement(peClosedSpline, length(pts)*sizeof(TPointF));
  elem^.NbControlPoints := length(pts);
  elem^.SplineStyle := style;
  p := PPointF(elem+1);
  for i := 0 to high(pts) do
  begin
    p^ := FMatrix*pts[i];
    inc(p);
  end;
  PInteger(p)^ := length(pts);
end;

procedure TBGRAPath.BitmapDrawSubPathProc(const APoints: array of TPointF;
  AClosed: boolean; AData: pointer);
begin
  with TStrokeData(AData^) do
  if AClosed then
  begin
    if Texture <> nil then
      Bitmap.DrawPolygonAntialias(APoints, Texture, Width)
    else
      Bitmap.DrawPolygonAntialias(APoints, Color, Width);
  end else
  begin
    if Texture <> nil then
      Bitmap.DrawPolyLineAntialiasAutocycle(APoints, Texture, Width)
    else
      Bitmap.DrawPolyLineAntialiasAutocycle(APoints, Color, Width);
  end;
end;

function TBGRAPath.CorrectAcceptedDeviation(AAcceptedDeviation: single;
  const AMatrix: TAffineMatrix): single;
var maxZoom: single;
begin
  //determine the zoom of the matrix
  maxZoom := Max(VectLen(PointF(AMatrix[1,1],AMatrix[2,1])),
     VectLen(PointF(AMatrix[1,2],AMatrix[2,2])));
  //make the accepted deviation smaller if the matrix zooms to avoid that
  // curves would look angular
  if maxZoom = 0 then
    result:= 1e10
  else
    result := AAcceptedDeviation / maxZoom;
end;

procedure TBGRAPath.OnModify;
begin
  if length(FCursors)> 0 then
      raise Exception.Create('You cannot modify the path when there are cursors');
end;

procedure TBGRAPath.OnMatrixChange;
begin
  //transformed coord are not changed,
  //but original coords are lost in the process.
  //this has a consequence when using
  //arc functions that rely on the previous
  //coordinate
  FLastCoord := EmptyPointF;
  FSubPathStartCoord := EmptyPointF;
end;

procedure TBGRAPath.NeedSpace(count: integer);
begin
  OnModify;
  if FDataPos + count > FDataCapacity then
  begin
    FDataCapacity := (FDataCapacity shl 1)+8;
    if FDataPos + count + 8 > FDataCapacity then
      FDataCapacity := FDataPos + count + 8;
    ReAllocMem(FData, FDataCapacity);
  end;
end;

function TBGRAPath.AllocateElement(AElementType: TBGRAPathElementType;
  AExtraBytes: PtrInt): Pointer;
var t: PtrInt;
begin
  if not (AElementType in [succ(peNone)..high(TBGRAPathElementType)]) then
    raise exception.Create('Invalid element type');
  OnModify;
  t := PathElementSize[AElementType]+AExtraBytes;
  NeedSpace(SizeOf(TPathElementHeader)+t);
  with PPathElementHeader(FData+FDataPos)^ do
  begin
    ElementType:= AElementType;
    PreviousElementType := FLastStoredElementType;
  end;
  result := FData+(FDataPos+SizeOf(TPathElementHeader));
  FLastSubPathElementType:= AElementType;
  FLastStoredElementType:= AElementType;
  Inc(FDataPos, sizeof(TPathElementHeader)+t);
end;

procedure TBGRAPath.Init;
begin
  FData := nil;
  FDataCapacity := 0;
  FLastMoveToDataPos := -1;
  beginPath;
  resetTransform;
end;

function TBGRAPath.GoToNextElement(var APos: PtrInt): boolean;
var newPos: PtrInt;
  p: PSplineElement;
  elemType: TBGRAPathElementType;
begin
  if APos >= FDataPos then
    result := false
  else
  begin
    elemType := PPathElementHeader(FData+APos)^.ElementType;
    newPos := APos + sizeof(TPathElementHeader) + PathElementSize[elemType];
    if elemType in[peOpenedSpline,peClosedSpline] then
    begin
      p := PSplineElement(FData+(APos+sizeof(TPathElementHeader)));
      newPos += p^.NbControlPoints * sizeof(TPointF); //extra
    end;
    if newPos < FDataPos then
    begin
      result := true;
      APos := newPos;
      if not CheckElementType(PPathElementHeader(FData+APos)^.ElementType) or
        not CheckElementType(PPathElementHeader(FData+APos)^.PreviousElementType) then
          raise exception.Create('Internal structure error');
    end
    else
      result := false;
  end;
end;

function TBGRAPath.GoToPreviousElement(var APos: PtrInt): boolean;
var lastElemType: TBGRAPathElementType;
begin
  if APos <= 0 then
    result := false
  else
  begin
    result := true;
    if (APos = FDataPos) then
      lastElemType := FLastStoredElementType
    else
      lastElemType := PPathElementHeader(FData+APos)^.PreviousElementType;

    if lastElemType in [peOpenedSpline,peClosedSpline] then
      dec(APos, (PInteger(FData+APos)-1)^ *sizeof(TPointF)); //extra
    dec(APos, sizeof(TPathElementHeader) + PathElementSize[lastElemType]);

    if not CheckElementType(PPathElementHeader(FData+APos)^.ElementType) or
      not CheckElementType(PPathElementHeader(FData+APos)^.PreviousElementType) then
        raise exception.Create('Internal structure error');
  end;
end;

function TBGRAPath.PeekNextElement(APos: PtrInt): TBGRAPathElementType;
begin
  if not GoToNextElement(APos) then
    result := peNone
  else
    result := PPathElementHeader(FData+APos)^.ElementType;
end;

function TBGRAPath.GetElementStartCoord(APos: PtrInt): TPointF;
var
  elemType: TBGRAPathElementType;
  elem: pointer;
begin
  GetElementAt(APos, elemType, elem);
  case elemType of
  peNone: raise exception.Create('No element');
  peMoveTo: result := PPointF(elem)^;
  else
    begin
      if not GoToPreviousElement(APos) then
        raise exception.Create('No previous element')
      else
      begin
        result := GetElementEndCoord(APos);
      end;
    end;
  end;
end;

function TBGRAPath.GetElementEndCoord(APos: PtrInt): TPointF;
var elemType: TBGRAPathElementType;
  elem: pointer;
begin
  GetElementAt(APos, elemType, elem);
  case elemType of
  peMoveTo,peLineTo,peCloseSubPath: result := PPointF(elem)^;
  peQuadraticBezierTo: result := PQuadraticBezierToElement(elem)^.Destination;
  peCubicBezierTo: result := PCubicBezierToElement(elem)^.Destination;
  peArc: result := ArcEndPoint(PArcElement(elem)^);
  peClosedSpline: result := PPointF(PSplineElement(elem)+1)^;
  peOpenedSpline: result := (PPointF(PSplineElement(elem)+1)+(PSplineElement(elem)^.NbControlPoints-1))^;
  else
    result := EmptyPointF;
  end;
end;

function TBGRAPath.GetElementLength(APos: PtrInt; AAcceptedDeviation: single): Single;
var elemType: TBGRAPathElementType;
  elem: pointer;
  pts: array of TPointF;
begin
  GetElementAt(APos, elemType, elem);
  case elemType of
  peMoveTo: result := 0;
  peLineTo,peCloseSubPath: result := VectLen(PPointF(elem)^ - GetElementStartCoord(APos))*FScale;
  peQuadraticBezierTo: with PQuadraticBezierToElement(elem)^ do
      result := BGRABitmapTypes.BezierCurve(GetElementStartCoord(APos),ControlPoint,Destination).ComputeLength;
  peCubicBezierTo: with PCubicBezierToElement(elem)^ do
      result := BGRABitmapTypes.BezierCurve(GetElementStartCoord(APos),ControlPoint1,ControlPoint2,Destination).ComputeLength(AAcceptedDeviation);
  peArc: begin
      result := VectLen(ArcStartPoint(PArcElement(elem)^) - GetElementStartCoord(APos));
      result += PolylineLen(ComputeArc(PArcElement(elem)^, 0.1/AAcceptedDeviation));
    end;
  peClosedSpline,peOpenedSpline:
    begin
      pts := GetPolygonalApprox(APos, AAcceptedDeviation, true);
      result := PolylineLen(pts) + VectLen(pts[0]-GetElementStartCoord(APos));
    end
  else
    result := 0;
  end;
end;

procedure TBGRAPath.GetElementAt(APos: PtrInt; out
  AElementType: TBGRAPathElementType; out AElement: pointer);
begin
  if APos >= FDataPos then
  begin
    AElementType := peNone;
    AElement := nil;
  end else
  begin
    AElementType:= PPathElementHeader(FData+APos)^.ElementType;
    AElement := FData+(APos+sizeof(TPathElementHeader));
  end;
end;

constructor TBGRAPath.Create;
begin
  Init;
end;

constructor TBGRAPath.Create(ASvgString: string);
begin
  Init;
  SvgString:= ASvgString;
end;

constructor TBGRAPath.Create(const APoints: ArrayOfTPointF);
begin
  Init;
  SetPoints(APoints);
end;

constructor TBGRAPath.Create(APath: IBGRAPath);
begin
  Init;
  APath.copyTo(self);
end;

destructor TBGRAPath.Destroy;
var i: integer;
begin
  for I := 0 to high(FCursors) do
    FCursors[i].OnPathFree;
  if Assigned(FData) then
  begin
    FreeMem(FData);
    FData := nil;
  end;
  inherited Destroy;
end;

procedure TBGRAPath.beginPath;
begin
  DoClear;
end;

procedure TBGRAPath.beginSubPath;
begin
  OnModify;
  FLastSubPathElementType := peNone;
  ClearLastCoord;
  FSubPathStartCoord := EmptyPointF;
  FExpectedTransformedControlPoint := EmptyPointF;
end;

procedure TBGRAPath.DoClear;
begin
  OnModify;
  FDataPos := 0;
  BeginSubPath;
end;

function TBGRAPath.CheckElementType(AElementType: TBGRAPathElementType): boolean;
begin
  result := AElementType <= high(TBGRAPathElementType);
end;

procedure TBGRAPath.closePath;
var
  moveToType: TBGRAPathElementType;
  moveToElem: pointer;
begin
  if (FLastSubPathElementType <> peNone) and (FLastSubPathElementType <> peCloseSubPath) then
  begin
    with PClosePathElement(AllocateElement(peCloseSubPath))^ do
    begin
      StartCoordinate := FSubPathTransformedStartCoord;
      LoopDataPos := FLastMoveToDataPos;
    end;
    if FLastMoveToDataPos <> -1 then
    begin
      GetElementAt(FLastMoveToDataPos,moveToType,moveToElem);
      PMoveToElement(moveToElem)^.LoopDataPos := FDataPos;
      FLastMoveToDataPos:= -1;
    end;
    FLastCoord := FSubPathStartCoord;
    FLastTransformedCoord := FSubPathTransformedStartCoord;
  end;
end;

procedure TBGRAPath.translate(x, y: single);
begin
  OnMatrixChange;
  FMatrix *= AffineMatrixTranslation(x,y);
end;

procedure TBGRAPath.resetTransform;
begin
  OnMatrixChange;
  FMatrix := AffineMatrixIdentity;
  FAngleRadCW := 0;
  FScale:= 1;
end;

procedure TBGRAPath.rotate(angleRadCW: single);
begin
  OnMatrixChange;
  FMatrix *= AffineMatrixRotationRad(-angleRadCW);
  FAngleRadCW += angleRadCW;
end;

procedure TBGRAPath.rotateDeg(angleDeg: single);
const degToRad = Pi/180;
begin
  rotate(angleDeg*degToRad);
end;

procedure TBGRAPath.rotate(angleRadCW: single; center: TPointF);
begin
  translate(center.x,center.y);
  rotate(angleRadCW);
  translate(-center.x,-center.y);
end;

procedure TBGRAPath.rotateDeg(angleDeg: single; center: TPointF);
begin
  translate(center.x,center.y);
  rotateDeg(angleDeg);
  translate(-center.x,-center.y);
end;

procedure TBGRAPath.scale(factor: single);
begin
  OnMatrixChange;
  FMatrix *= AffineMatrixScale(factor,factor);
  FScale *= factor;
end;

procedure TBGRAPath.moveTo(x, y: single);
begin
  moveTo(PointF(x,y));
end;

procedure TBGRAPath.lineTo(x, y: single);
begin
  lineTo(PointF(x,y));
end;

procedure TBGRAPath.moveTo(const pt: TPointF);
begin
  if FLastSubPathElementType <> peMoveTo then
  begin
    FLastMoveToDataPos:= FDataPos;
    with PMoveToElement(AllocateElement(peMoveTo))^ do
    begin
      StartCoordinate := SetLastCoord(pt);
      LoopDataPos := -1;
    end
  end else
    PMoveToElement(FData+(FDataPos-Sizeof(TMoveToElement)))^.StartCoordinate := SetLastCoord(pt);
  FSubPathStartCoord := FLastCoord;
  FSubPathTransformedStartCoord := FLastTransformedCoord;
end;

procedure TBGRAPath.lineTo(const pt: TPointF);
var lastTransfCoord, newTransfCoord: TPointF;
begin
  if LastCoordDefined then
  begin
    lastTransfCoord := FLastTransformedCoord;
    newTransfCoord := SetLastCoord(pt);
    if newTransfCoord <> lastTransfCoord then
      PPointF(AllocateElement(peLineTo))^ := newTransfCoord;
  end else
    moveTo(pt);
end;

procedure TBGRAPath.polyline(const pts: array of TPointF);
var i: integer;
begin
  if length(pts) = 0 then exit;
  NeedSpace((sizeof(TPathElementHeader)+sizeof(TPointF))*length(pts));
  moveTo(pts[0]);
  for i := 1 to high(pts) do lineTo(pts[i]);
end;

procedure TBGRAPath.polylineTo(const pts: array of TPointF);
var i: integer;
begin
  NeedSpace((sizeof(TPathElementHeader)+sizeof(TPointF))*length(pts));
  for i := 0 to high(pts) do lineTo(pts[i]);
end;

procedure TBGRAPath.polygon(const pts: array of TPointF);
var lastPt: integer;
begin
  if length(pts) = 0 then exit;
  lastPt := high(pts);
  while (lastPt > 1) and (pts[lastPt] = pts[0]) do dec(lastPt);
  if lastPt <> high(pts) then
    polyline(slice(pts,lastPt+1))
  else
    polyline(pts);
  closePath;
end;

procedure TBGRAPath.quadraticCurveTo(cpx, cpy, x, y: single);
begin
  quadraticCurveTo(PointF(cpx,cpy),PointF(x,y));
end;

procedure TBGRAPath.quadraticCurveTo(const cp, pt: TPointF);
begin
  if LastCoordDefined then
    QuadraticCurveFromTransformed(FMatrix*cp, pt) else
  begin
    lineTo(pt);
    FExpectedTransformedControlPoint := FMatrix*(pt+(pt-cp));
  end;
end;

procedure TBGRAPath.bezierCurveTo(cp1x, cp1y, cp2x, cp2y, x, y: single);
begin
  bezierCurveTo(PointF(cp1x,cp1y),PointF(cp2x,cp2y),PointF(x,y));
end;

procedure TBGRAPath.bezierCurveTo(const cp1, cp2, pt: TPointF);
begin
  if not LastCoordDefined then moveTo(cp1);
  BezierCurveFromTransformed(FMatrix*cp1, cp2, pt);
end;

procedure TBGRAPath.bezierCurve(const curve: TCubicBezierCurve);
begin
  moveTo(curve.p1);
  bezierCurveTo(curve.c1,curve.c2,curve.p2);
end;

procedure TBGRAPath.bezierCurve(p1, cp1, cp2, p2: TPointF);
begin
  moveTo(p1);
  bezierCurveTo(cp1,cp2,p2);
end;

procedure TBGRAPath.smoothBezierCurveTo(cp2x, cp2y, x, y: single);
begin
  smoothBezierCurveTo(PointF(cp2x,cp2y),PointF(x,y));
end;

procedure TBGRAPath.smoothBezierCurveTo(const cp2, pt: TPointF);
begin
  if (FLastSubPathElementType = peCubicBezierTo) and not isEmptyPointF(FExpectedTransformedControlPoint) then
    BezierCurveFromTransformed(FExpectedTransformedControlPoint,cp2,pt)
  else if LastCoordDefined then
    BezierCurveFromTransformed(FLastTransformedCoord,cp2,pt)
  else
    bezierCurveTo(cp2,cp2,pt);
end;

procedure TBGRAPath.quadraticCurve(const curve: TQuadraticBezierCurve);
begin
  moveTo(curve.p1);
  quadraticCurveTo(curve.c,curve.p2);
end;

procedure TBGRAPath.quadraticCurve(p1, cp, p2: TPointF);
begin
  moveTo(p1);
  quadraticCurveTo(cp,p2);
end;

procedure TBGRAPath.smoothQuadraticCurveTo(x, y: single);
begin
  smoothQuadraticCurveTo(PointF(x,y));
end;

procedure TBGRAPath.smoothQuadraticCurveTo(const pt: TPointF);
begin
  if (FLastSubPathElementType = peQuadraticBezierTo) and not isEmptyPointF(FExpectedTransformedControlPoint) then
    QuadraticCurveFromTransformed(FExpectedTransformedControlPoint,pt)
  else if LastCoordDefined then
    QuadraticCurveFromTransformed(FLastTransformedCoord,pt)
  else
    quadraticCurveTo(pt,pt);
end;

procedure TBGRAPath.rect(x, y, w, h: single);
begin
  moveTo(x,y);
  lineTo(x+w,y);
  lineTo(x+w,y+h);
  lineTo(x,y+h);
  closePath;
end;

procedure TBGRAPath.roundRect(x, y, w, h, radius: single);
begin
  if radius <= 0 then
  begin
    rect(x,y,w,h);
    exit;
  end;
  if (w <= 0) or (h <= 0) then exit;
  if radius*2 > w then radius := w/2;
  if radius*2 > h then radius := h/2;
  moveTo(x+radius,y);
  arcTo(PointF(x+w,y),PointF(x+w,y+h), radius);
  arcTo(PointF(x+w,y+h),PointF(x,y+h), radius);
  arcTo(PointF(x,y+h),PointF(x,y), radius);
  arcTo(PointF(x,y),PointF(x+w,y), radius);
  closePath;
end;

procedure TBGRAPath.arc(cx, cy, radius, startAngleRadCW, endAngleRadCW: single;
  anticlockwise: boolean);
begin
  arc(cx,cy,radius,radius,0,startAngleRadCW,endAngleRadCW,anticlockwise);
end;

procedure TBGRAPath.arc(cx, cy, radius, startAngleRadCW, endAngleRadCW: single);
begin
  arc(cx,cy,radius,startAngleRadCW,endAngleRadCW,false);
end;

procedure TBGRAPath.arcDeg(cx, cy, radius, startAngleDeg, endAngleDeg: single;
  anticlockwise: boolean);
const degToRad = Pi/180;
begin
  arc(cx,cy,radius,(startAngleDeg-90)*degToRad,(endAngleDeg-90)*degToRad,anticlockwise);
end;

procedure TBGRAPath.arcDeg(cx, cy, radius, startAngleDeg, endAngleDeg: single);
const degToRad = Pi/180;
begin
  arc(cx,cy,radius,(startAngleDeg-90)*degToRad,(endAngleDeg-90)*degToRad);
end;

procedure TBGRAPath.arcTo(x1, y1, x2, y2, radius: single);
begin
  arcTo(PointF(x1,y1), PointF(x2,y2), radius);
end;

procedure TBGRAPath.arcTo(const p1, p2: TPointF; radius: single);
var p0 : TPointF;
begin
  if IsEmptyPointF(FLastCoord) then
    p0 := p1 else p0 := FLastCoord;
  arc(Html5ArcTo(p0,p1,p2,radius));
end;

procedure TBGRAPath.arc(const arcDef: TArcDef);
var transformedArc: TArcElement;
begin
  if (arcDef.radius.x = 0) and (arcDef.radius.y = 0) then
    lineTo(arcDef.center)
  else
  begin
    if not LastCoordDefined then
      moveTo(ArcStartPoint(arcDef));
    transformedArc.anticlockwise := arcDef.anticlockwise;
    transformedArc.startAngleRadCW := arcDef.startAngleRadCW;
    transformedArc.endAngleRadCW := arcDef.endAngleRadCW;
    transformedArc.center := FMatrix*arcDef.center;
    transformedArc.radius := arcDef.radius*FScale;
    transformedArc.xAngleRadCW := arcDef.xAngleRadCW+FAngleRadCW;
    PArcElement(AllocateElement(peArc))^ := transformedArc;
	{$PUSH}{$OPTIMIZATION OFF}
    SetLastCoord(ArcEndPoint(arcDef));
	{$POP}
  end;
end;

procedure TBGRAPath.arc(cx, cy, rx, ry: single; xAngleRadCW, startAngleRadCW,
  endAngleRadCW: single);
begin
  arc(ArcDef(cx,cy,rx,ry,xAngleRadCW,startAngleRadCW,endAngleRadCW,false));
end;

procedure TBGRAPath.arc(cx, cy, rx, ry, xAngleRadCW, startAngleRadCW, endAngleRadCW: single;
  anticlockwise: boolean);
begin
  arc(ArcDef(cx,cy,rx,ry,xAngleRadCW,startAngleRadCW,endAngleRadCW,anticlockwise));
end;

procedure TBGRAPath.arcTo(rx, ry, xAngleRadCW: single; largeArc,
  anticlockwise: boolean; x, y: single);
begin
  if IsEmptyPointF(FLastCoord) then
    moveTo(x,y)
  else
    arc(SvgArcTo(FLastCoord, rx,ry, xAngleRadCW, largeArc, anticlockwise, PointF(x,y)));
end;

procedure TBGRAPath.copyTo(dest: IBGRAPath);
var pos: IntPtr;
    elemType: TBGRAPathElementType;
    elem: Pointer;
    pts: array of TPointF;
begin
  pos := 0;
  repeat
    GetElementAt(pos, elemType, elem);
    if elem = nil then break;
    case elemType of
      peMoveTo: dest.moveTo(PPointF(elem)^);
      peLineTo: dest.lineTo(PPointF(elem)^);
      peCloseSubPath: dest.closePath;
      peQuadraticBezierTo:
        with PQuadraticBezierToElement(elem)^ do
          dest.quadraticCurveTo(ControlPoint,Destination);
      peCubicBezierTo:
        with PCubicBezierToElement(elem)^ do
          dest.bezierCurveTo(ControlPoint1,ControlPoint2,Destination);
      peArc: dest.arc(PArcElement(elem)^);
      peOpenedSpline, peClosedSpline:
        begin
          with PSplineElement(elem)^ do
          begin
            setlength(pts, NbControlPoints);
            move(Pointer(PSplineElement(elem)+1)^, pts[0], NbControlPoints*sizeof(TPointF));
            if elemType = peOpenedSpline then
              dest.openedSpline(pts, SplineStyle)
            else
              dest.closedSpline(pts, SplineStyle);
            pts := nil;
          end;
        end;
    end;
  until not GoToNextElement(pos);
end;

function TBGRAPath.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
begin
  if GetInterface(iid, obj) then
    Result := S_OK
  else
    Result := longint(E_NOINTERFACE);
end;

{ There is no automatic reference counting, but it is compulsory to define these functions }
function TBGRAPath._AddRef: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
begin
  result := 0;
end;

function TBGRAPath._Release: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
begin
  result := 0;
end;

end.

