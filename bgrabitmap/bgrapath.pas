unit BGRAPath;

{$mode objfpc}{$H+}

interface

uses
  Classes, BGRABitmapTypes;

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
function ComputeRoundRect(x1,y1,x2,y2,rx,ry: single; quality: single = 1): ArrayOfTPointF; overload;
function ComputeRoundRect(x1,y1,x2,y2,rx,ry: single; options: TRoundRectangleOptions; quality: single = 1): ArrayOfTPointF; overload;

implementation

uses Math, BGRAResample;

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

{$PUSH}{$R-}
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
    pos := start65536+int64(i)*arclen div (int64(nb)-1);
    result[i] := PointF(x+rx*(Cos65536(pos)-32768)/32768,
                        y-ry*(Sin65536(pos)-32768)/32768);
  end;
end;
{$R+}

function ComputeEllipse(x, y, rx, ry: single; quality: single): ArrayOfTPointF;
begin
  result := ComputeArc65536(x,y,rx,ry,0,0,quality);
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


end.

