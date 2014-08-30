unit BGRAPath;

{$mode objfpc}{$H+}

interface

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
  TBGRAPathElementType = (peNone, peMoveTo, peLineTo, peCloseSubPath, peQuadraticBezierTo, peCubicBezierTo, peArc);
  PBGRAPathElementType = ^TBGRAPathElementType;

  { TBGRAPath }

  TBGRAPath = class(IBGRAPath)
  private
    function GetSvgString: string;
    procedure SetSvgString(const AValue: string);
  protected
    FData: pbyte;
    FDataSize: integer;
    FDataPos: integer;
    FLastElementType: TBGRAPathElementType;
    FLastCoord,
    FStartCoord: TPointF;
    FExpectedControlPoint: TPointF;
    FMatrix: TAffineMatrix; //this matrix must have a base of vectors
                            //orthogonal, of same length and with positive
                            //orientation in order to preserve arcs
    FScale,FAngleRadCW: single;
    procedure NeedSpace(count: integer);
    procedure StoreCoord(const pt: TPointF);
    function ReadCoord: TPointF;
    procedure StoreElementType(value: TBGRAPathElementType);
    function ReadElementType: TBGRAPathElementType;
    function ReadArcDef: TArcDef;
    procedure RewindFloat;
    procedure Init;
  public
    constructor Create; overload;
    constructor Create(ASvgString: string); overload;
    destructor Destroy; override;
    procedure beginPath;
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
    procedure polylineTo(const pts: array of TPointF);
    procedure quadraticCurveTo(cpx,cpy,x,y: single); overload;
    procedure quadraticCurveTo(const cp,pt: TPointF); overload;
    procedure quadraticCurve(const curve: TQuadraticBezierCurve); overload;
    procedure smoothQuadraticCurveTo(x,y: single); overload;
    procedure smoothQuadraticCurveTo(const pt: TPointF); overload;
    procedure bezierCurveTo(cp1x,cp1y,cp2x,cp2y,x,y: single); overload;
    procedure bezierCurveTo(const cp1,cp2,pt: TPointF); overload;
    procedure bezierCurve(const curve: TCubicBezierCurve); overload;
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
    procedure arc(cx, cy, rx,ry, xAngleRadCW, startAngleRadCW, endAngleRadCW: single); overload;
    procedure arc(cx, cy, rx,ry, xAngleRadCW, startAngleRadCW, endAngleRadCW: single; anticlockwise: boolean); overload;
    procedure arcTo(rx,ry, xAngleRadCW: single; largeArc, anticlockwise: boolean; x,y:single);
    procedure copyTo(dest: IBGRAPath);
    procedure addPath(const AValue: string); overload;
    procedure addPath(source: IBGRAPath); overload;
    property SvgString: string read GetSvgString write SetSvgString;
  protected
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
    function _AddRef: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
    function _Release: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
  end;

{----------------------- Spline ------------------}

function SplineVertexToSide(y0, y1, y2, y3: single; t: single): single;
function ComputeBezierCurve(const curve: TCubicBezierCurve): ArrayOfTPointF; overload;
function ComputeBezierCurve(const curve: TQuadraticBezierCurve): ArrayOfTPointF; overload;
function ComputeBezierSpline(const spline: array of TCubicBezierCurve): ArrayOfTPointF; overload;
function ComputeBezierSpline(const spline: array of TQuadraticBezierCurve): ArrayOfTPointF; overload;
function ComputeClosedSpline(const points: array of TPointF; Style: TSplineStyle): ArrayOfTPointF;
function ComputeOpenedSpline(const points: array of TPointF; Style: TSplineStyle; EndCoeff: single = 0.25): ArrayOfTPointF;

{ Compute points to draw an antialiased ellipse }
function ComputeEllipse(x,y,rx,ry: single; quality: single = 1): ArrayOfTPointF;
function ComputeArc65536(x, y, rx, ry: single; start65536,end65536: word; quality: single = 1): ArrayOfTPointF;
function ComputeArcRad(x, y, rx, ry: single; startRadCCW,endRadCCW: single; quality: single = 1): ArrayOfTPointF;
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

function ComputeCurvePrecision(pt1, pt2, pt3, pt4: TPointF): integer;
var
  len: single;
begin
  len    := sqr(pt1.x - pt2.x) + sqr(pt1.y - pt2.y);
  len    := max(len, sqr(pt3.x - pt2.x) + sqr(pt3.y - pt2.y));
  len    := max(len, sqr(pt3.x - pt4.x) + sqr(pt3.y - pt4.y));
  Result := round(sqrt(sqrt(len)) * 2);
  if Result<=0 then Result:=1;
end;

function ComputeBezierCurve(const curve: TCubicBezierCurve): ArrayOfTPointF; overload;
var
  t,f1,f2,f3,f4: single;
  i,nb: Integer;
begin
  nb := ComputeCurvePrecision(curve.p1,curve.c1,curve.c2,curve.p2);
  if nb <= 1 then nb := 2;
  setlength(result,nb);
  result[0] := curve.p1;
  result[nb-1] := curve.p2;
  for i := 1 to nb-2 do
  begin
    t := i/(nb-1);
    f1 := (1-t);
    f2 := f1*f1;
    f1 *= f2;
    f2 *= t*3;
    f4 := t*t;
    f3 := f4*(1-t)*3;
    f4 *= t;

    result[i] := PointF(f1*curve.p1.x + f2*curve.c1.x +
                  f3*curve.c2.x + f4*curve.p2.x,
                  f1*curve.p1.y + f2*curve.c1.y +
                  f3*curve.c2.y + f4*curve.p2.y);
  end;
end;

function ComputeBezierCurve(const curve: TQuadraticBezierCurve): ArrayOfTPointF; overload;
var
  t,f1,f2,f3: single;
  i,nb: Integer;
begin
  nb := ComputeCurvePrecision(curve.p1,curve.c,curve.c,curve.p2);
  if nb <= 1 then nb := 2;
  setlength(result,nb);
  result[0] := curve.p1;
  result[nb-1] := curve.p2;
  for i := 1 to nb-2 do
  begin
    t := i/(nb-1);
    f1 := (1-t);
    f3 := t;
    f2 := f1*f3*2;
    f1 *= f1;
    f3 *= f3;
    result[i] := PointF(f1*curve.p1.x + f2*curve.c.x + f3*curve.p2.x,
                  f1*curve.p1.y + f2*curve.c.y + f3*curve.p2.y);
  end;
end;

function ComputeBezierSpline(const spline: array of TCubicBezierCurve): ArrayOfTPointF;
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
    curves[i] := ComputeBezierCurve(spline[i]);
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

function ComputeBezierSpline(const spline: array of TQuadraticBezierCurve
  ): ArrayOfTPointF;
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
    curves[i] := ComputeBezierCurve(spline[i]);
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

function ComputeClosedSpline(const points: array of TPointF; Style: TSplineStyle): ArrayOfTPointF;
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
    nb      += ComputeCurvePrecision(ptPrev2, ptPrev, ptNext, ptNext2);
  end;

  kernel := CreateInterpolator(style);
  setlength(Result, nb);
  for i := 0 to high(points) do
  begin
    ptPrev2 := points[(i + length(points) - 1) mod length(points)];
    ptPrev  := points[i];
    ptNext  := points[(i + 1) mod length(points)];
    ptNext2 := points[(i + 2) mod length(points)];
    pre     := ComputeCurvePrecision(ptPrev2, ptPrev, ptNext, ptNext2);
    if i=0 then
    begin
      j := 0;
      idx := 0;
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
end;

function ComputeOpenedSpline(const points: array of TPointF; Style: TSplineStyle; EndCoeff: single): ArrayOfTPointF;
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
    nb      += ComputeCurvePrecision(ptPrev2, ptPrev, ptNext, ptNext2);
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
    pre     := ComputeCurvePrecision(ptPrev2, ptPrev, ptNext, ptNext2);
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
  result := AffineMatrixRotationRad(-arc.endAngleRadCW)*PointF(cos(arc.startAngleRadCW)*arc.radius.x,
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

{ TBGRAPath }

function TBGRAPath.GetSvgString: string;
const RadToDeg = 180/Pi;
var savedPos: integer;
    a: TArcDef;
    formats: TFormatSettings;
    lastPos,p1: TPointF;
    implicitCommand: char;

  function FloatToString(value: single): string;
  begin
    result := FloatToStrF(value,ffGeneral,7,0,formats)+' ';
  end;

  function CoordToString(const pt: TPointF): string;
  begin
    lastPos := pt;
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

var param: string;

begin
  formats := DefaultFormatSettings;
  formats.DecimalSeparator := '.';

  result := '';
  savedPos:= FDataPos;
  FDataPos := 0;
  lastPos := EmptyPointF;
  implicitCommand := #0;
  while FDataPos < savedPos do
  begin
    case ReadElementType of
    peMoveTo: addCommand('M',CoordToString(ReadCoord));
    peLineTo: addCommand('L',CoordToString(ReadCoord));
    peCloseSubPath: addCommand('z','');
    peQuadraticBezierTo:
      begin
        param := CoordToString(ReadCoord);
        param += CoordToString(ReadCoord);
        addCommand('Q',param);
      end;
    peCubicBezierTo:
      begin
        param := CoordToString(ReadCoord);
        param += CoordToString(ReadCoord);
        param += CoordToString(ReadCoord);
        addCommand('C',param);
      end;
    peArc:
      begin
        a := ReadArcDef;
        p1 := ArcStartPoint(a);
        if isEmptyPointF(lastPos) or (p1 <> lastPos) then
          addCommand('L',CoordToString(p1));
        param := CoordToString(a.radius);
        param += FloatToString(a.xAngleRadCW*RadToDeg);
        param += BoolToString(IsLargeArc(a));
        param += BoolToString(not a.anticlockwise);
        param += CoordToString(ArcEndPoint(a));
        addCommand('A',param);
      end;
    end;
  end;
  FDataPos := savedPos;
end;

procedure TBGRAPath.SetSvgString(const AValue: string);
begin
  resetTransform;
  beginPath;
  addPath(AValue);
end;

procedure TBGRAPath.addPath(const AValue: string);
var p: integer;
    numberError: boolean;

  function parseFloat: single;
  var numberStart: integer;
      errPos: integer;
  begin
    while (p <= length(AValue)) and (AValue[p] in[#0..#32,',']) do inc(p);
    numberStart:= p;
    if (p <= length(AValue)) and (AValue[p] in['+','-']) then inc(p);
    while (p <= length(AValue)) and (AValue[p] in['0'..'9','.']) do inc(p);
    if (p <= length(AValue)) and (AValue[p] in['e','E']) then inc(p);
    if (p <= length(AValue)) and (AValue[p] in['+','-']) then inc(p);
    while (p <= length(AValue)) and (AValue[p] in['0'..'9','.']) do inc(p);
    val(copy(AValue,numberStart,p-numberStart),result,errPos);
    if errPos <> 0 then numberError := true;
  end;

  function parseCoord(relative: boolean): TPointF;
  begin
    result := PointF(parseFloat,parseFloat);
    if relative and not isEmptyPointF(FLastCoord) then result += FLastCoord;
  end;

var
  command,implicitCommand: char;
  relative: boolean;
  c1,c2,p1: TPointF;
  a: TArcDef;
  largeArc: boolean;
begin
  FLastCoord := EmptyPointF;
  FStartCoord := EmptyPointF;
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
         end;
    'M': begin
           p1 := parseCoord(relative);
           if not numberError then moveTo(p1);
           if relative then implicitCommand:= 'l' else
             implicitCommand:= 'L';
      end;
    'L': begin
           p1 := parseCoord(relative);
           if not numberError then lineTo(p1);
      end;
    'H': begin
        if not isEmptyPointF(FLastCoord) then p1 := FLastCoord
        else p1 := PointF(0,0);
        if relative then p1.x += parseFloat
        else p1.x := parseFloat;
        if not numberError then lineTo(p1);
      end;
    'V': begin
        if not isEmptyPointF(FLastCoord) then p1 := FLastCoord
        else p1 := PointF(0,0);
        if relative then p1.y += parseFloat
        else p1.y := parseFloat;
        if not numberError then lineTo(p1);
      end;
    'C': begin
        c1 := parseCoord(relative);
        c2 := parseCoord(relative);
        p1 := parseCoord(relative);
        if not numberError then bezierCurveTo(c1,c2,p1);
      end;
    'S': begin
        c2 := parseCoord(relative);
        p1 := parseCoord(relative);
        if not numberError then smoothBezierCurveTo(c2,p1);
      end;
    'Q': begin
        c1 := parseCoord(relative);
        p1 := parseCoord(relative);
        if not numberError then quadraticCurveTo(c1,p1);
      end;
    'T': begin
        p1 := parseCoord(relative);
        if not numberError then smoothQuadraticCurveTo(p1);
      end;
    'A': begin
        a.radius := parseCoord(false);
        a.xAngleRadCW := parseFloat*Pi/180;
        largeArc := parseFloat<>0;
        a.anticlockwise:= parseFloat=0;
        p1 := parseCoord(relative);
        arcTo(a.radius.x,a.radius.y,a.xAngleRadCW,largeArc,a.anticlockwise,p1.x,p1.y);
      end;
    end;
  end;
end;

procedure TBGRAPath.addPath(source: IBGRAPath);
begin
  source.copyTo(self);
end;

procedure TBGRAPath.NeedSpace(count: integer);
begin
  if FDataPos + count > FDataSize then
  begin
    FDataSize := FDataSize*2+8;
    ReAllocMem(FData, FDataSize);
  end;
end;

procedure TBGRAPath.StoreCoord(const pt: TPointF);
begin
  NeedSpace(sizeof(single)*2);
  with FMatrix*pt do
  begin
    PSingle(FData+FDataPos)^ := x;
    PSingle(FData+FDataPos+sizeof(single))^ := y;
  end;
  Inc(FDataPos, sizeof(single)*2);
  FLastCoord := pt;
end;

function TBGRAPath.ReadCoord: TPointF;
begin
  result := PPointF(FData+FDataPos)^;
  inc(FDataPos,sizeof(TPointF));
end;

procedure TBGRAPath.StoreElementType(value: TBGRAPathElementType);
begin
  NeedSpace(sizeof(TBGRAPathElementType));
  PBGRAPathElementType(FData+FDataPos)^ := value;
  Inc(FDataPos, sizeof(TBGRAPathElementType));
  FLastElementType:= value;
end;

function TBGRAPath.ReadElementType: TBGRAPathElementType;
begin
  result := PBGRAPathElementType(FData+FDataPos)^;
  inc(FDataPos,sizeof(TBGRAPathElementType));
end;

function TBGRAPath.ReadArcDef: TArcDef;
begin
  result := PArcDef(FData+FDataPos)^;
  inc(FDataPos,sizeof(TArcDef));
end;

procedure TBGRAPath.RewindFloat;
begin
  if FDataPos >= sizeof(single) then dec(FDataPos, sizeof(Single));
end;

procedure TBGRAPath.Init;
begin
  FData := nil;
  FDataSize := 0;
  FDataPos := 0;
  FLastElementType := peNone;
  FLastCoord := EmptyPointF;
  FStartCoord := EmptyPointF;
  FExpectedControlPoint := EmptyPointF;
  resetTransform;
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

destructor TBGRAPath.Destroy;
begin
  if Assigned(FData) then
  begin
    FreeMem(FData);
    FData := nil;
  end;
  inherited Destroy;
end;

procedure TBGRAPath.beginPath;
begin
  FDataPos := 0;
end;

procedure TBGRAPath.closePath;
begin
  if (FLastElementType <> peNone) and (FLastElementType <> peCloseSubPath) then
  begin
    StoreElementType(peCloseSubPath);
    FLastCoord := FStartCoord;
  end;
end;

procedure TBGRAPath.translate(x, y: single);
begin
  FMatrix *= AffineMatrixTranslation(x,y);
end;

procedure TBGRAPath.resetTransform;
begin
  FMatrix := AffineMatrixIdentity;
  FAngleRadCW := 0;
  FScale:= 1;
end;

procedure TBGRAPath.rotate(angleRadCW: single);
begin
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
  if FLastElementType <> peMoveTo then
  begin
    StoreElementType(peMoveTo);
    StoreCoord(pt);
  end else
  begin
    RewindFloat;
    RewindFloat;
    StoreCoord(pt);
  end;
  FLastCoord := pt;
  FStartCoord := FLastCoord;
end;

procedure TBGRAPath.lineTo(const pt: TPointF);
begin
  if not isEmptyPointF(FLastCoord) then
  begin
    StoreElementType(peLineTo);
    StoreCoord(pt);
    FLastCoord := pt;
  end else
    moveTo(pt);
end;

procedure TBGRAPath.polylineTo(const pts: array of TPointF);
var i: integer;
begin
  NeedSpace((sizeof(TBGRAPathElementType)+2*sizeof(single))*length(pts));
  for i := 0 to high(pts) do with pts[i] do lineTo(x,y);
end;

procedure TBGRAPath.quadraticCurveTo(cpx, cpy, x, y: single);
begin
  quadraticCurveTo(PointF(cpx,cpy),PointF(x,y));
end;

procedure TBGRAPath.quadraticCurveTo(const cp, pt: TPointF);
begin
  if not isEmptyPointF(FLastCoord) then
  begin
    StoreElementType(peQuadraticBezierTo);
    StoreCoord(cp);
    StoreCoord(pt);
    FLastCoord := pt;
  end else
    lineTo(pt);
  FExpectedControlPoint := pt+(pt-cp);
end;

procedure TBGRAPath.bezierCurveTo(cp1x, cp1y, cp2x, cp2y, x, y: single);
begin
  bezierCurveTo(PointF(cp1x,cp1y),PointF(cp2x,cp2y),PointF(x,y));
end;

procedure TBGRAPath.bezierCurveTo(const cp1, cp2, pt: TPointF);
begin
  if isEmptyPointF(FLastCoord) then moveTo(cp1);
  StoreElementType(peCubicBezierTo);
  StoreCoord(cp1);
  StoreCoord(cp2);
  StoreCoord(pt);
  FLastCoord := pt;
  FExpectedControlPoint := pt + (pt-cp2);
end;

procedure TBGRAPath.bezierCurve(const curve: TCubicBezierCurve);
begin
  moveTo(curve.p1);
  bezierCurveTo(curve.c1,curve.c2,curve.p2);
end;

procedure TBGRAPath.smoothBezierCurveTo(cp2x, cp2y, x, y: single);
begin
  smoothBezierCurveTo(PointF(cp2x,cp2y),PointF(x,y));
end;

procedure TBGRAPath.smoothBezierCurveTo(const cp2, pt: TPointF);
begin
  if (FLastElementType = peCubicBezierTo) and not isEmptyPointF(FExpectedControlPoint) then
    bezierCurveTo(FExpectedControlPoint,cp2,pt)
  else if not isEmptyPointF(FLastCoord) then
    bezierCurveTo(FLastCoord,cp2,pt)
  else
    bezierCurveTo(cp2,cp2,pt);
end;

procedure TBGRAPath.quadraticCurve(const curve: TQuadraticBezierCurve);
begin
  moveTo(curve.p1);
  quadraticCurveTo(curve.c,curve.p2);
end;

procedure TBGRAPath.smoothQuadraticCurveTo(x, y: single);
begin
  smoothQuadraticCurveTo(PointF(x,y));
end;

procedure TBGRAPath.smoothQuadraticCurveTo(const pt: TPointF);
begin
  if (FLastElementType = peQuadraticBezierTo) and not isEmptyPointF(FExpectedControlPoint) then
    quadraticCurveTo(FExpectedControlPoint,pt)
  else if not isEmptyPointF(FLastCoord) then
    quadraticCurveTo(FLastCoord,pt)
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
  if isEmptyPointF(FLastCoord) then
    p0 := p1 else p0 := FLastCoord;
  arc(Html5ArcTo(p0,p1,p2,radius));
end;

procedure TBGRAPath.arc(const arcDef: TArcDef);
var transformedArc: TArcDef;
begin
  if (arcDef.radius.x = 0) and (arcDef.radius.y = 0) then
    lineTo(arcDef.center)
  else
  begin
    if isEmptyPointF(FLastCoord) then
      moveTo(ArcStartPoint(arcDef));
    StoreElementType(peArc);
    NeedSpace(sizeof(TArcDef));
    transformedArc.anticlockwise := arcDef.anticlockwise;
    transformedArc.startAngleRadCW := arcDef.startAngleRadCW;
    transformedArc.endAngleRadCW := arcDef.endAngleRadCW;
    transformedArc.center := FMatrix*arcDef.center;
    transformedArc.radius := arcDef.radius*FScale;
    transformedArc.xAngleRadCW := arcDef.xAngleRadCW+FAngleRadCW;
    PArcDef(FData+FDataPos)^ := transformedArc;
    inc(FDataPos, sizeof(TArcDef));
    FLastCoord := ArcEndPoint(arcDef);
  end;
end;

procedure TBGRAPath.arc(cx, cy, rx, ry, xAngleRadCW, startAngleRadCW,
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
  if isEmptyPointF(FLastCoord) then
    moveTo(x,y)
  else
    arc(SvgArcTo(FLastCoord, rx,ry, xAngleRadCW, largeArc, anticlockwise, PointF(x,y)));
end;

procedure TBGRAPath.copyTo(dest: IBGRAPath);
var savedPos: integer;
    cp1,cp2,p1: TPointF;
begin
  savedPos:= FDataPos;
  FDataPos := 0;
  while FDataPos < savedPos do
  begin
    case ReadElementType of
    peMoveTo: dest.moveTo(ReadCoord);
    peLineTo: dest.lineTo(ReadCoord);
    peCloseSubPath: dest.closePath;
    peQuadraticBezierTo:
      begin
        cp1 := ReadCoord;
        p1 := ReadCoord;
        dest.quadraticCurveTo(cp1,p1);
      end;
    peCubicBezierTo:
      begin
        cp1 := ReadCoord;
        cp2 := ReadCoord;
        p1 := ReadCoord;
        dest.bezierCurveTo(cp1,cp2,p1);
      end;
    peArc: dest.arc(ReadArcDef);
    end;
  end;
  FDataPos := savedPos;
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

