{=== Geometry types ===}

{$IFDEF INCLUDE_INTERFACE}
{$UNDEF INCLUDE_INTERFACE}
const
  {* Value indicating that there is nothing in the single-precision floating point value.
     It is also used as a separator in lists }
  EmptySingle: single = -3.402823e38;

type
  {* Pointer to a ''TPointF'' structure }
  PPointF = ^TPointF;
  {* Contains a point with single-precision floating point coordinates }
  {$ifdef VER3_1_1 }
  TPointF = Types.TPointF;
  {$else}
  TPointF = packed record x, y: single;
  end;
  {$endif}

  {$ifdef VER3_1_1 }
  TRectF = Types.TRectF;
  {$else}
  TRectF =
  {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
  packed
  {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
  record
    case Integer of
     0: (Left, Top, Right, Bottom: Single);
     1: (TopLeft, BottomRight: TPointF);
  end;
  {$endif}
  function RectF(Left, Top, Right, Bottom: Single): TRectF;

  const
    {** Value indicating that there is an empty ''TPointF'' structure.
        It is also used as a separator in lists of points }
    EmptyPointF: TPointF = (x: -3.402823e38; y: -3.402823e38);

  {----------------- Operators for TPointF --------------------}
  {** Creates a new structure with values ''x'' and ''y'' }
  function PointF(x, y: single): TPointF;
  {** Checks if the structure is empty (equal to ''EmptyPointF'') }
  function isEmptyPointF(const pt: TPointF): boolean;
  {** Checks if both ''x'' and ''y'' are equal }
  operator = (const pt1, pt2: TPointF): boolean; inline;
  {** Adds ''x'' and ''y'' components separately. It is like adding vectors }
  operator + (const pt1, pt2: TPointF): TPointF; inline;
  {** Subtract ''x'' and ''y'' components separately. It is like subtracting vectors }
  operator - (const pt1, pt2: TPointF): TPointF; inline;
  {** Returns a point with opposite values for ''x'' and ''y'' components }
  operator - (const pt2: TPointF): TPointF; inline;
  {** Scalar product: multiplies ''x'' and ''y'' components and returns the sum }
  operator * (const pt1, pt2: TPointF): single; inline;
  {** Multiplies both ''x'' and ''y'' by ''factor''. It scales the vector represented by (''x'',''y'') }
  operator * (const pt1: TPointF; factor: single): TPointF; inline;
  {** Multiplies both ''x'' and ''y'' by ''factor''. It scales the vector represented by (''x'',''y'') }
  operator * (factor: single; const pt1: TPointF): TPointF; inline;
  {** Returns the length of the vector (''dx'',''dy'') }
  function VectLen(dx,dy: single): single; overload;
  {** Returns the length of the vector represented by (''x'',''y'') }
  function VectLen(v: TPointF): single; overload;
  function VectDet(v1,v2: TPointF): double; inline;

type
  TFaceCulling = (fcNone, fcKeepCW, fcKeepCCW);
  {* Contains an array of points with single-precision floating point coordinates }
  ArrayOfTPointF = array of TPointF;

  {** Creates an array of ''TPointF'' }
  function PointsF(const pts: array of TPointF): ArrayOfTPointF;
  {** Concatenates arrays of ''TPointF'' }
  function ConcatPointsF(const APolylines: array of ArrayOfTPointF): ArrayOfTPointF;
  {** Compute the length of the polyline contained in the array.
      ''AClosed'' specifies if the last point is to be joined to the first one }
  function PolylineLen(const pts: array of TPointF; AClosed: boolean = false): single;

type
  {* A pen style can be dashed, dotted, etc. It is defined as a list of floating point number.
     The first number is the length of the first dash,
     the second number is the length of the first gap,
     the third number is the length of the second dash...
     It must have an even number of values. This is used as a complement
     to [[BGRABitmap Types imported from Graphics|TPenStyle]] }
  TBGRAPenStyle = array Of Single;

  {** Creates a pen style with the specified length for the dashes and the spaces }
  function BGRAPenStyle(dash1, space1: single; dash2: single=0; space2: single = 0; dash3: single=0; space3: single = 0; dash4 : single = 0; space4 : single = 0): TBGRAPenStyle;

type
  {* Different types of spline. A spline is a series of points that are used
     as control points to draw a curve. The first point and last point may
     or may not be the starting and ending point }
  TSplineStyle = (
    {** The curve is drawn inside the polygonal envelope without reaching the starting and ending points }
    ssInside,
    {** The curve is drawn inside the polygonal envelope and the starting and ending points are reached }
    ssInsideWithEnds,
    {** The curve crosses the polygonal envelope without reaching the starting and ending points }
    ssCrossing,
    {** The curve crosses the polygonal envelope and the starting and ending points are reached }
    ssCrossingWithEnds,
    {** The curve is outside the polygonal envelope (starting and ending points are reached) }
    ssOutside,
    {** The curve expands outside the polygonal envelope (starting and ending points are reached) }
    ssRoundOutside,
    {** The curve is outside the polygonal envelope and there is a tangeant at vertices (starting and ending points are reached) }
    ssVertexToSide);

  { TCubicBezierCurve }
  {* Definition of a Bézier curve of order 3. It has two control points ''c1'' and ''c2''. Those are not reached by the curve }
  TCubicBezierCurve = object
  private
    function SimpleComputePoints(AAcceptedDeviation: single = 0.1; AIncludeFirstPoint: boolean = true): ArrayOfTPointF;
  public
    {** Starting point (reached) }
    p1: TPointF;
    {** First control point (not reached by the curve) }
    c1: TPointF;
    {** Second control point (not reached by the curve) }
    c2: TPointF;
    {** Ending point (reached) }
    p2: TPointF;
    {** Computes the point at time ''t'', varying from 0 to 1 }
    function ComputePointAt(t: single): TPointF;
    {** Split the curve in two such that ''ALeft.p2'' = ''ARight.p1'' }
    procedure Split(out ALeft, ARight: TCubicBezierCurve);
    {** Compute an approximation of the length of the curve. ''AAcceptedDeviation'' indicates the
       maximum orthogonal distance that is ignored and approximated by a straight line. }
    function ComputeLength(AAcceptedDeviation: single = 0.1): single;
    {** Computes a polygonal approximation of the curve. ''AAcceptedDeviation'' indicates the
       maximum orthogonal distance that is ignored and approximated by a straight line.
       ''AIncludeFirstPoint'' indicates if the first point must be included in the array }
    function ToPoints(AAcceptedDeviation: single = 0.1; AIncludeFirstPoint: boolean = true): ArrayOfTPointF;
    function GetBounds: TRectF;
  end;

  {** Creates a structure for a cubic Bézier curve }
  function BezierCurve(origin, control1, control2, destination: TPointF) : TCubicBezierCurve; overload;

type
  { TQuadraticBezierCurve }
  {* Definition of a Bézier curve of order 2. It has one control point }
  TQuadraticBezierCurve = object
  private
    function SimpleComputePoints(AAcceptedDeviation: single = 0.1; AIncludeFirstPoint: boolean = true): ArrayOfTPointF;
    function ComputeExtremumPositionOutsideSegment: single;
  public
    {** Starting point (reached) }
    p1: TPointF;
    {** Control point (not reached by the curve) }
    c: TPointF;
    {** Ending point (reached) }
    p2: TPointF;
    {** Computes the point at time ''t'', varying from 0 to 1 }
    function ComputePointAt(t: single): TPointF;
    {** Split the curve in two such that ''ALeft.p2'' = ''ARight.p1'' }
    procedure Split(out ALeft, ARight: TQuadraticBezierCurve);
    {** Compute the '''exact''' length of the curve }
    function ComputeLength: single;
    {** Computes a polygonal approximation of the curve. ''AAcceptedDeviation'' indicates the
       maximum orthogonal distance that is ignored and approximated by a straight line.
       ''AIncludeFirstPoint'' indicates if the first point must be included in the array }
    function ToPoints(AAcceptedDeviation: single = 0.1; AIncludeFirstPoint: boolean = true): ArrayOfTPointF;
    function GetBounds: TRectF;
  end;

  {** Creates a structure for a quadratic Bézier curve }
  function BezierCurve(origin, control, destination: TPointF) : TQuadraticBezierCurve; overload;
  {** Creates a structure for a quadratic Bézier curve without curvature }
  function BezierCurve(origin, destination: TPointF) : TQuadraticBezierCurve; overload;

type
  {* Pointer to an arc definition }
  PArcDef = ^TArcDef;
  {* Definition of an arc of an ellipse }
  TArcDef = record
    {** Center of the ellipse }
    center: TPointF;
    {** Horizontal and vertical of the ellipse before rotation }
    radius: TPointF;
    {** Rotation of the ellipse }
    xAngleRadCW: single;
    {** Start and end angle, in radian and clockwise. See angle convention in ''BGRAPath'' }
    startAngleRadCW, endAngleRadCW: single;
    {** Specifies if the arc goes anticlockwise }
    anticlockwise: boolean
  end;

  {** Creates a structure for an arc definition }
  function ArcDef(cx, cy, rx,ry, xAngleRadCW, startAngleRadCW, endAngleRadCW: single; anticlockwise: boolean) : TArcDef;

type
  {* Possible options for drawing an arc of an ellipse (used in ''BGRACanvas'') }
  TArcOption = (
    {** Close the path by joining the ending and starting point together }
    aoClosePath,
    {** Draw a pie shape by joining the ending and starting point to the center of the ellipse }
    aoPie,
    {** Fills the shape }
    aoFillPath);
    {** Set of options for drawing an arc }
    TArcOptions = set of TArcOption;

  TBGRAArrowStyle = (asNone, asNormal, asCut, asTriangle, asHollowTriangle, asFlipped, asFlippedCut, asTail, asTailRepeat);

type
  {* Point in 3D with single-precision floating point coordinates }
  TPoint3D = record x,y,z: single;
  end;

  {----------------- Operators for TPoint3D ---------------}
  {** Creates a new structure with values (''x'',''y'',''z'') }
  function Point3D(x,y,z: single): TPoint3D;
  {** Checks if all components ''x'', ''y'' and ''z'' are equal }
  operator = (const v1,v2: TPoint3D): boolean; inline;
  {** Adds components separately. It is like adding vectors }
  operator + (const v1,v2: TPoint3D): TPoint3D; inline;
  {** Subtract components separately. It is like subtracting vectors }
  operator - (const v1,v2: TPoint3D): TPoint3D; inline;
  {** Returns a point with opposite values for all components }
  operator - (const v: TPoint3D): TPoint3D; inline;
  {** Scalar product: multiplies components and returns the sum }
  operator * (const v1,v2: TPoint3D): single; inline;
  {** Multiplies components by ''factor''. It scales the vector represented by (''x'',''y'',''z'') }
  operator * (const v1: TPoint3D; const factor: single): TPoint3D; inline;
  {** Multiplies components by ''factor''. It scales the vector represented by (''x'',''y'',''z'') }
  operator * (const factor: single; const v1: TPoint3D): TPoint3D; inline;
  {** Computes the vectorial product ''w''. It is perpendicular to both ''u'' and ''v'' }
  procedure VectProduct3D(u,v: TPoint3D; out w: TPoint3D);
  {** Normalize the vector, i.e. scale it so that its length be 1 }
  procedure Normalize3D(var v: TPoint3D); inline;

type
  {* Defition of a line in the euclidian plane }
  TLineDef = record
    {** Some point in the line }
    origin: TPointF;
    {** Vector indicating the direction }
    dir: TPointF;
  end;

  {----------- Line and polygon functions -----------}
  {** Computes the intersection of two lines. If they are parallel, returns
      the middle of the segment between the two origins }
  function IntersectLine(line1, line2: TLineDef): TPointF;
  {** Computes the intersection of two lines. If they are parallel, returns
      the middle of the segment between the two origins. The value ''parallel''
      is set to indicate if the lines were parallel }
  function IntersectLine(line1, line2: TLineDef; out parallel: boolean): TPointF;
  {** Checks if the polygon formed by the given points is convex. ''IgnoreAlign''
      specifies that if the points are aligned, it should still be considered as convex }
  function IsConvex(const pts: array of TPointF; IgnoreAlign: boolean = true): boolean;
  function IsClockwise(const pts: array of TPointF): boolean;
  {** Checks if the quad formed by the 4 given points intersects itself }
  function DoesQuadIntersect(pt1,pt2,pt3,pt4: TPointF): boolean;
  {** Checks if two segment intersect }
  function DoesSegmentIntersect(pt1,pt2,pt3,pt4: TPointF): boolean;

type
  TBGRACustomPathCursor = class;

  {* A path is the ability to define a contour with ''moveTo'', ''lineTo''...
     Even if it is an interface, it must not implement reference counting. }
  IBGRAPath = interface
    {** Closes the current path with a line to the starting point }
    procedure closePath;
    {** Moves to a location, disconnected from previous points }
    procedure moveTo(const pt: TPointF);
    {** Adds a line from the current point }
    procedure lineTo(const pt: TPointF);
    {** Adds a polyline from the current point }
    procedure polylineTo(const pts: array of TPointF);
    {** Adds a quadratic Bézier curve from the current point }
    procedure quadraticCurveTo(const cp,pt: TPointF);
    {** Adds a cubic Bézier curve from the current point }
    procedure bezierCurveTo(const cp1,cp2,pt: TPointF);
    {** Adds an arc. If there is a current point, it is connected to the beginning of the arc }
    procedure arc(const arcDef: TArcDef);
    {** Adds an opened spline. If there is a current point, it is connected to the beginning of the spline }
    procedure openedSpline(const pts: array of TPointF; style: TSplineStyle);
    {** Adds an closed spline. If there is a current point, it is connected to the beginning of the spline }
    procedure closedSpline(const pts: array of TPointF; style: TSplineStyle);
    {** Copy the content of this path to the specified destination }
    procedure copyTo(dest: IBGRAPath);
    {** Returns the content of the path as an array of points }
    function getPoints: ArrayOfTPointF;
    {** Returns a cursor to go through the path. The cursor must be freed by calling ''Free''. }
    function getCursor: TBGRACustomPathCursor;
  end;

  { TBGRACustomPathCursor }
  {* Class that contains a cursor to browse an existing path }
  TBGRACustomPathCursor = class
  protected
    function GetArcPos: single; virtual; abstract;
    function GetCurrentCoord: TPointF; virtual; abstract;
    function GetCurrentTangent: TPointF; virtual; abstract;
    function GetLoopClosedShapes: boolean; virtual; abstract;
    function GetLoopPath: boolean; virtual; abstract;
    function GetPathLength: single; virtual; abstract;
    function GetBounds: TRectF; virtual; abstract;
    function GetStartCoordinate: TPointF; virtual; abstract;
    procedure SetArcPos(AValue: single); virtual; abstract;
    procedure SetLoopClosedShapes(AValue: boolean); virtual; abstract;
    procedure SetLoopPath(AValue: boolean); virtual; abstract;
  public
    {** Go forward in the path, increasing the value of ''Position''. If ''ADistance'' is negative, then
        it goes backward instead. ''ACanJump'' specifies if the cursor can jump from one shape to another
        without a line or an arc. Otherwise, the cursor is stuck, and the return value is less than
        the value ''ADistance'' provided.  If all the way has been travelled, the
        return value is equal to ''ADistance'' }
    function MoveForward(ADistance: single; ACanJump: boolean = true): single; virtual; abstract;
    {** Go backward, decreasing the value of ''Position''. If ''ADistance'' is negative, then it goes
        forward instead. ''ACanJump'' specifies if the cursor can jump from one shape to another
        without a line or an arc. Otherwise, the cursor is stuck, and the return value is less than
        the value ''ADistance'' provided. If all the way has been travelled, the
        return value is equal to ''ADistance'' }
    function MoveBackward(ADistance: single; ACanJump: boolean = true): single; virtual; abstract;
    {** Returns the current coordinate in the path }
    property CurrentCoordinate: TPointF read GetCurrentCoord;
    {** Returns the tangent vector. It is a vector of length one that is parallel to the curve
        at the current point. A normal vector is easily deduced as PointF(y,-x) }
    property CurrentTangent: TPointF read GetCurrentTangent;
    {** Current position in the path, as a distance along the arc from the starting point of the path }
    property Position: single read GetArcPos write SetArcPos;
    {** Full arc length of the path }
    property PathLength: single read GetPathLength;
    {** Starting coordinate of the path }
    property StartCoordinate: TPointF read GetStartCoordinate;
    {** Specifies if the cursor loops when there is a closed shape }
    property LoopClosedShapes: boolean read GetLoopClosedShapes write SetLoopClosedShapes;
    {** Specifies if the cursor loops at the end of the path. Note that if it needs to jump to go
        to the beginning, it will be only possible if the parameter ''ACanJump'' is set to True
        when moving along the path }
    property LoopPath: boolean read GetLoopPath write SetLoopPath;
  end;

const
  {* A value for an empty rectangle }
  EmptyRect : TRect = (left:0; top:0; right:0; bottom: 0);
{* Checks if a point is in a rectangle. This follows usual convention: ''r.Right'' and
  ''r.Bottom'' are not considered to be included in the rectangle. }
function PtInRect(const pt: TPoint; r: TRect): boolean; overload;
{* Creates a rectangle with the specified ''width'' and ''height'' }
function RectWithSize(left,top,width,height: integer): TRect;

type
  {* Possible options for a round rectangle }
  TRoundRectangleOption = (
    {** specify that a corner is a square (not rounded) }
    rrTopLeftSquare,rrTopRightSquare,rrBottomRightSquare,rrBottomLeftSquare,
    {** specify that a corner is a bevel (cut) }
    rrTopLeftBevel,rrTopRightBevel,rrBottomRightBevel,rrBottomLeftBevel,
    {** default option, does nothing particular }
    rrDefault);
    {** A set of options for a round rectangle }
    TRoundRectangleOptions = set of TRoundRectangleOption;
  {* Order of polygons when rendered using ''TBGRAMultiShapeFiller''
     (in unit ''BGRAPolygon'') }
  TPolygonOrder = (
    {** No order, colors are mixed together }
    poNone,
    {** First polygon is on top }
    poFirstOnTop,
    {** Last polygon is on top }
    poLastOnTop);

  { TIntersectionInfo }
  {* Contains an intersection between an horizontal line and any shape. It
     is used when filling shapes }
  TIntersectionInfo = class
    interX: single;
    winding: integer;
    numSegment: integer;
    procedure SetValues(AInterX: Single; AWinding, ANumSegment: integer);
  end;
  {** An array of intersections between an horizontal line and any shape }
  ArrayOfTIntersectionInfo = array of TIntersectionInfo;

  {* Abstract class defining any shape that can be filled }
  TBGRACustomFillInfo = class
    public
      {** Returns true if one segment number can represent a curve and
          thus cannot be considered exactly straight }
      function SegmentsCurved: boolean; virtual; abstract;

      {** Returns integer bounds for the shape }
      function GetBounds: TRect; virtual; abstract;

      {** Check if the point is inside the shape }
      function IsPointInside(x,y: single; windingMode: boolean): boolean; virtual; abstract;

      {** Create an array that will contain computed intersections.
          To augment that array, use ''CreateIntersectionInfo'' for new items }
      function CreateIntersectionArray: ArrayOfTIntersectionInfo; virtual; abstract;
      {** Create a structure to define one single intersection }
      function CreateIntersectionInfo: TIntersectionInfo; virtual; abstract;
      {** Free an array of intersections }
      procedure FreeIntersectionArray(var inter: ArrayOfTIntersectionInfo); virtual; abstract;

      {** Fill an array ''inter'' with actual intersections with the shape at the y coordinate ''cury''.
          ''nbInter'' receives the number of computed intersections. ''windingMode'' specifies if
          the winding method must be used to determine what is inside of the shape }
      procedure ComputeAndSort(cury: single; var inter: ArrayOfTIntersectionInfo; out nbInter: integer; windingMode: boolean); virtual; abstract;
  end;

type
  {* Shape of a gradient }
  TGradientType = (
    {** The color changes along a certain vector and does not change along its perpendicular direction }
    gtLinear,
    {** The color changes like in ''gtLinear'' however it is symmetrical to a specified direction }
    gtReflected,
    {** The color changes along a diamond shape }
    gtDiamond,
    {** The color changes in a radial way from a given center }
    gtRadial);
const
  {** List of string to represent gradient types }
  GradientTypeStr : array[TGradientType] of string
  = ('Linear','Reflected','Diamond','Radial');
  {** Returns the gradient type represented by the given string }
  function StrToGradientType(str: string): TGradientType;

type
  { TBGRACustomGradient }
  {* Defines a gradient of color, not specifying its shape but only the
     series of colors }
  TBGRACustomGradient = class
  public
    {** Returns the color at a given ''position''. The reference range is
        from 0 to 65535, however values beyond are possible as well }
    function GetColorAt(position: integer): TBGRAPixel; virtual; abstract;
    function GetExpandedColorAt(position: integer): TExpandedPixel; virtual;
    {** Returns the color at a given ''position''. The reference range is
        from 0 to 1, however values beyond are possible as well }
    function GetColorAtF(position: single): TBGRAPixel; virtual;
    function GetExpandedColorAtF(position: single): TExpandedPixel; virtual;
    {** Returns the average color of the gradient }
    function GetAverageColor: TBGRAPixel; virtual; abstract;
    function GetAverageExpandedColor: TExpandedPixel; virtual;
    function GetMonochrome: boolean; virtual; abstract;
    {** This property is True if the gradient contains only one color,
        and thus is not really a gradient }
    property Monochrome: boolean read GetMonochrome;
  end;

{$ENDIF}

////////////////////////////////////////////////////////////////////////////////

{$IFDEF INCLUDE_IMPLEMENTATION}
{$UNDEF INCLUDE_IMPLEMENTATION}

function StrToGradientType(str: string): TGradientType;
var gt: TGradientType;
begin
  result := gtLinear;
  str := LowerCase(str);
  for gt := low(TGradientType) to high(TGradientType) do
    if str = LowerCase(GradientTypeStr[gt]) then
    begin
      result := gt;
      exit;
    end;
end;

{ TBGRACustomGradient }

function TBGRACustomGradient.GetExpandedColorAt(position: integer
  ): TExpandedPixel;
begin
  result := GammaExpansion(GetColorAt(position));
end;

function TBGRACustomGradient.GetColorAtF(position: single): TBGRAPixel;
begin
  position *= 65536;
  if position < low(integer) then
    result := GetColorAt(low(Integer))
  else if position > high(integer) then
    result := GetColorAt(high(Integer))
  else
    result := GetColorAt(round(position));
end;

function TBGRACustomGradient.GetExpandedColorAtF(position: single): TExpandedPixel;
begin
  position *= 65536;
  if position < low(integer) then
    result := GetExpandedColorAt(low(Integer))
  else if position > high(integer) then
    result := GetExpandedColorAt(high(Integer))
  else
    result := GetExpandedColorAt(round(position));
end;

function TBGRACustomGradient.GetAverageExpandedColor: TExpandedPixel;
begin
  result := GammaExpansion(GetAverageColor);
end;

{ TIntersectionInfo }

procedure TIntersectionInfo.SetValues(AInterX: Single; AWinding,
  ANumSegment: integer);
begin
  interX := AInterX;
  winding := AWinding;
  numSegment := ANumSegment;
end;

{********************** TRect functions **************************}

function PtInRect(const pt: TPoint; r: TRect): boolean;
var
  temp: integer;
begin
  if r.right < r.left then
  begin
    temp    := r.left;
    r.left  := r.right;
    r.Right := temp;
  end;
  if r.bottom < r.top then
  begin
    temp     := r.top;
    r.top    := r.bottom;
    r.bottom := temp;
  end;
  Result := (pt.X >= r.left) and (pt.Y >= r.top) and (pt.X < r.right) and
    (pt.y < r.bottom);
end;

function RectWithSize(left, top, width, height: integer): TRect;
begin
  result.left := left;
  result.top := top;
  result.right := left+width;
  result.bottom := top+height;
end;

{ Make a pen style. Need an even number of values. See TBGRAPenStyle }
function BGRAPenStyle(dash1, space1: single; dash2: single; space2: single;
  dash3: single; space3: single; dash4: single; space4: single): TBGRAPenStyle;
var
  i: Integer;
begin
  if dash4 <> 0 then
  begin
    setlength(result,8);
    result[6] := dash4;
    result[7] := space4;
    result[4] := dash3;
    result[5] := space3;
    result[2] := dash2;
    result[3] := space2;
  end else
  if dash3 <> 0 then
  begin
    setlength(result,6);
    result[4] := dash3;
    result[5] := space3;
    result[2] := dash2;
    result[3] := space2;
  end else
  if dash2 <> 0 then
  begin
    setlength(result,4);
    result[2] := dash2;
    result[3] := space2;
  end else
  begin
    setlength(result,2);
  end;
  result[0] := dash1;
  result[1] := space1;
  for i := 0 to high(result) do
    if result[i]=0 then
      raise exception.Create('Zero is not a valid value');
end;

//-------------- Bézier curves definitions ----------------
// See : http://en.wikipedia.org/wiki/B%C3%A9zier_curve

// Define a Bézier curve with two control points.
function BezierCurve(origin, control1, control2, destination: TPointF): TCubicBezierCurve;
begin
  result.p1 := origin;
  result.c1 := control1;
  result.c2 := control2;
  result.p2 := destination;
end;

// Define a Bézier curve with one control point.
function BezierCurve(origin, control, destination: TPointF
  ): TQuadraticBezierCurve;
begin
  result.p1 := origin;
  result.c := control;
  result.p2 := destination;
end;

//straight line
function BezierCurve(origin, destination: TPointF): TQuadraticBezierCurve;
begin
  result.p1 := origin;
  result.c := (origin+destination)*0.5;
  result.p2 := destination;
end;

function ArcDef(cx, cy, rx, ry, xAngleRadCW, startAngleRadCW, endAngleRadCW: single;
  anticlockwise: boolean): TArcDef;
begin
  result.center := PointF(cx,cy);
  result.radius := PointF(rx,ry);
  result.xAngleRadCW:= xAngleRadCW;
  result.startAngleRadCW := startAngleRadCW;
  result.endAngleRadCW:= endAngleRadCW;
  result.anticlockwise:= anticlockwise;
end;

{----------------- Operators for TPoint3D ---------------}
operator = (const v1, v2: TPoint3D): boolean; inline;
begin
  result := (v1.x=v2.x) and (v1.y=v2.y) and (v1.z=v2.z);
end;

operator * (const v1,v2: TPoint3D): single; inline;
begin
  result := v1.x*v2.x + v1.y*v2.y + v1.z*v2.z;
end;

operator * (const v1: TPoint3D; const factor: single): TPoint3D; inline;
begin
  result.x := v1.x*factor;
  result.y := v1.y*factor;
  result.z := v1.z*factor;
end;

operator - (const v1,v2: TPoint3D): TPoint3D; inline;
begin
  result.x := v1.x-v2.x;
  result.y := v1.y-v2.y;
  result.z := v1.z-v2.z;
end;

operator -(const v: TPoint3D): TPoint3D; inline;
begin
  result.x := -v.x;
  result.y := -v.y;
  result.z := -v.z;
end;

operator + (const v1,v2: TPoint3D): TPoint3D; inline;
begin
  result.x := v1.x+v2.x;
  result.y := v1.y+v2.y;
  result.z := v1.z+v2.z;
end;

operator*(const factor: single; const v1: TPoint3D): TPoint3D;
begin
  result.x := v1.x*factor;
  result.y := v1.y*factor;
  result.z := v1.z*factor;
end;

function Point3D(x, y, z: single): TPoint3D;
begin
  result.x := x;
  result.y := y;
  result.z := z;
end;

procedure Normalize3D(var v: TPoint3D); inline;
var len: double;
begin
  len := v*v;
  if len = 0 then exit;
  len := sqrt(len);
  v.x /= len;
  v.y /= len;
  v.z /= len;
end;

procedure VectProduct3D(u,v: TPoint3D; out w: TPoint3D);
begin
  w.x := u.y*v.z-u.z*v.y;
  w.y := u.z*v.x-u.x*v.z;
  w.z := u.x*v.Y-u.y*v.x;
end;

{----------------- Operators for TPointF --------------------}
operator =(const pt1, pt2: TPointF): boolean;
begin
  result := (pt1.x = pt2.x) and (pt1.y = pt2.y);
end;

operator -(const pt1, pt2: TPointF): TPointF;
begin
  result.x := pt1.x-pt2.x;
  result.y := pt1.y-pt2.y;
end;

operator -(const pt2: TPointF): TPointF;
begin
  result.x := -pt2.x;
  result.y := -pt2.y;
end;

operator +(const pt1, pt2: TPointF): TPointF;
begin
  result.x := pt1.x+pt2.x;
  result.y := pt1.y+pt2.y;
end;

operator *(const pt1, pt2: TPointF): single;
begin
  result := pt1.x*pt2.x + pt1.y*pt2.y;
end;

operator *(const pt1: TPointF; factor: single): TPointF;
begin
  result.x := pt1.x*factor;
  result.y := pt1.y*factor;
end;

operator *(factor: single; const pt1: TPointF): TPointF;
begin
  result.x := pt1.x*factor;
  result.y := pt1.y*factor;
end;

function RectF(Left, Top, Right, Bottom: Single): TRectF;
begin
  result.Left:= Left;
  result.Top:= Top;
  result.Right:= Right;
  result.Bottom:= Bottom;
end;

function PointF(x, y: single): TPointF;
begin
  Result.x := x;
  Result.y := y;
end;

function PointsF(const pts: array of TPointF): ArrayOfTPointF;
var
  i: Integer;
begin
  setlength(result, length(pts));
  for i := 0 to high(pts) do result[i] := pts[i];
end;

function ConcatPointsF(const APolylines: array of ArrayOfTPointF
  ): ArrayOfTPointF;
var
  i,pos,count:integer;
  j: Integer;
begin
  count := 0;
  for i := 0 to high(APolylines) do
    inc(count,length(APolylines[i]));
  setlength(result,count);
  pos := 0;
  for i := 0 to high(APolylines) do
    for j := 0 to high(APolylines[i]) do
    begin
      result[pos] := APolylines[i][j];
      inc(pos);
    end;
end;

function VectLen(v: TPointF): single;
begin
  result := sqrt(v*v);
end;

function VectDet(v1, v2: TPointF): double;
begin
  result := v1.x*v2.y - v1.y*v2.x;
end;

function VectLen(dx, dy: single): single;
begin
  result := sqrt(dx*dx+dy*dy);
end;

function PolylineLen(const pts: array of TPointF; AClosed: boolean): single;
var
  i: NativeInt;
begin
  result := 0;
  for i := 0 to high(pts)-1 do
    result += VectLen(pts[i+1]-pts[i]);
  if AClosed then
    result += VectLen(pts[0]-pts[high(pts)]);
end;

{ Check if a PointF structure is empty or should be treated as a list separator }
function isEmptyPointF(const pt: TPointF): boolean;
begin
  Result := (pt.x = EmptySingle) and (pt.y = EmptySingle);
end;

{----------- Line and polygon functions -----------}
{$PUSH}{$OPTIMIZATION OFF}
function IntersectLine(line1, line2: TLineDef): TPointF;
var parallel: boolean;
begin
  result := IntersectLine(line1,line2,parallel);
end;
{$POP}

function IntersectLine(line1, line2: TLineDef; out parallel: boolean): TPointF;
var divFactor: double;
begin
  parallel := false;
  //if lines are parallel
  if ((line1.dir.x = line2.dir.x) and (line1.dir.y = line2.dir.y)) or
     ((abs(line1.dir.y) < 1e-6) and (abs(line2.dir.y) < 1e-6)) then
  begin
       parallel := true;
       //return the center of the segment between line origins
       result.x := (line1.origin.x+line2.origin.x)/2;
       result.y := (line1.origin.y+line2.origin.y)/2;
  end else
  if abs(line1.dir.y) < 1e-6 then //line1 is horizontal
  begin
       result.y := line1.origin.y;
       result.x := line2.origin.x + (result.y - line2.origin.y)
               /line2.dir.y*line2.dir.x;
  end else
  if abs(line2.dir.y) < 1e-6 then //line2 is horizontal
  begin
       result.y := line2.origin.y;
       result.x := line1.origin.x + (result.y - line1.origin.y)
               /line1.dir.y*line1.dir.x;
  end else
  begin
       divFactor := line1.dir.x/line1.dir.y - line2.dir.x/line2.dir.y;
       if abs(divFactor) < 1e-6 then //almost parallel
       begin
            parallel := true;
            //return the center of the segment between line origins
            result.x := (line1.origin.x+line2.origin.x)/2;
            result.y := (line1.origin.y+line2.origin.y)/2;
       end else
       begin
         result.y := (line2.origin.x - line1.origin.x +
                  line1.origin.y*line1.dir.x/line1.dir.y -
                  line2.origin.y*line2.dir.x/line2.dir.y)
                  / divFactor;
         result.x := line1.origin.x + (result.y - line1.origin.y)
                 /line1.dir.y*line1.dir.x;
       end;
  end;
end;

{ Check if a polygon is convex, i.e. it always turns in the same direction }
function IsConvex(const pts: array of TPointF; IgnoreAlign: boolean = true): boolean;
var
  positive,negative,zero: boolean;
  product: single;
  i: Integer;
begin
  positive := false;
  negative := false;
  zero := false;
  for i := 0 to high(pts) do
  begin
    product := (pts[(i+1) mod length(pts)].x-pts[i].x)*(pts[(i+2) mod length(pts)].y-pts[i].y) -
               (pts[(i+1) mod length(pts)].y-pts[i].y)*(pts[(i+2) mod length(pts)].x-pts[i].x);
    if product > 0 then
    begin
      if negative then
      begin
        result := false;
        exit;
      end;
      positive := true;
    end else
    if product < 0 then
    begin
      if positive then
      begin
        result := false;
        exit;
      end;
      negative := true;
    end else
      zero := true;
  end;
  if not IgnoreAlign and zero then
    result := false
  else
    result := true;
end;

{ Check if two segments intersect }
function DoesSegmentIntersect(pt1,pt2,pt3,pt4: TPointF): boolean;
var
  seg1: TLineDef;
  seg1len: single;
  seg2: TLineDef;
  seg2len: single;
  inter: TPointF;
  pos1,pos2: single;
  para: boolean;

begin
  { Determine line definitions }
  seg1.origin := pt1;
  seg1.dir := pt2-pt1;
  seg1len := sqrt(sqr(seg1.dir.X)+sqr(seg1.dir.Y));
  if seg1len = 0 then
  begin
    result := false;
    exit;
  end;
  seg1.dir *= 1/seg1len;

  seg2.origin := pt3;
  seg2.dir := pt4-pt3;
  seg2len := sqrt(sqr(seg2.dir.X)+sqr(seg2.dir.Y));
  if seg2len = 0 then
  begin
    result := false;
    exit;
  end;
  seg2.dir *= 1/seg2len;

  //obviously parallel
  if seg1.dir = seg2.dir then
    result := false
  else
  begin
    //try to compute intersection
    inter := IntersectLine(seg1,seg2,para);
    if para then
      result := false
    else
    begin
      //check if intersections are inside the segments
      pos1 := (inter-seg1.origin)*seg1.dir;
      pos2 := (inter-seg2.origin)*seg2.dir;
      if (pos1 >= 0) and (pos1 <= seg1len) and
         (pos2 >= 0) and (pos2 <= seg2len) then
        result := true
      else
        result := false;
    end;
  end;
end;

function IsClockwise(const pts: array of TPointF): boolean;
var
  i: Integer;
begin
  for i := 0 to high(pts) do
  begin
    if (pts[(i+1) mod length(pts)].x-pts[i].x)*(pts[(i+2) mod length(pts)].y-pts[i].y) -
       (pts[(i+1) mod length(pts)].y-pts[i].y)*(pts[(i+2) mod length(pts)].x-pts[i].x) < 0 then
    begin
       result := false;
       exit;
    end;
  end;
  result := true;
end;

{ Check if a quaduadrilateral intersects itself }
function DoesQuadIntersect(pt1,pt2,pt3,pt4: TPointF): boolean;
begin
  result := DoesSegmentIntersect(pt1,pt2,pt3,pt4) or DoesSegmentIntersect(pt2,pt3,pt4,pt1);
end;

{------------------ Bezier curves ------------------------}

function ComputeBezierCurvePrecision(pt1, pt2, pt3, pt4: TPointF; AAcceptedDeviation: single = 0.1): integer;
var
  len: single;
begin
  len    := sqr(pt1.x - pt2.x) + sqr(pt1.y - pt2.y);
  len    := max(len, sqr(pt3.x - pt2.x) + sqr(pt3.y - pt2.y));
  len    := max(len, sqr(pt3.x - pt4.x) + sqr(pt3.y - pt4.y));
  Result := round(sqrt(sqrt(len)/ AAcceptedDeviation) * 1);
  if Result<=0 then Result:=1;
end;

{ TCubicBezierCurve }

function TCubicBezierCurve.SimpleComputePoints(AAcceptedDeviation: single;
  AIncludeFirstPoint: boolean = true): ArrayOfTPointF;
var
  t,step: single;
  i,nb: Integer;
begin
  nb := ComputeBezierCurvePrecision(p1,c1,c2,p2, AAcceptedDeviation/2);
  if nb <= 1 then nb := 2;
  if AIncludeFirstPoint then
  begin
    setlength(result,nb);
    result[0] := p1;
    result[nb-1] := p2;
    step := 1/(nb-1);
    t := 0;
    for i := 1 to nb-2 do
    begin
      t += step;
      result[i] := ComputePointAt(t);
    end;
  end else
  begin
    setlength(result,nb-1);
    result[nb-2] := p2;
    step := 1/(nb-1);
    t := 0;
    for i := 0 to nb-3 do
    begin
      t += step;
      result[i] := ComputePointAt(t);
    end;
  end;
end;

function TCubicBezierCurve.ComputePointAt(t: single): TPointF;
var
  f1,f2,f3,f4: single;
begin
  f1 := (1-t);
  f2 := f1*f1;
  f1 *= f2;
  f2 *= t*3;
  f4 := t*t;
  f3 := f4*(1-t)*3;
  f4 *= t;

  result.x := f1*p1.x + f2*c1.x +
              f3*c2.x + f4*p2.x;
  result.y := f1*p1.y + f2*c1.y +
              f3*c2.y + f4*p2.y;
end;

procedure TCubicBezierCurve.Split(out ALeft, ARight: TCubicBezierCurve);
var midc: TPointF;
begin
  ALeft.p1 := p1;
  ALeft.c1 := 0.5*(p1+c1);
  ARight.p2 := p2;
  ARight.c2 := 0.5*(p2+c2);
  midc := 0.5*(c1+c2);
  ALeft.c2 := 0.5*(ALeft.c1+midc);
  ARight.c1 := 0.5*(ARight.c2+midc);
  ALeft.p2 := 0.5*(ALeft.c2+ARight.c1);
  ARight.p1 := ALeft.p2;
end;

function TCubicBezierCurve.ComputeLength(AAcceptedDeviation: single): single;
var
  t,step: single;
  i,nb: Integer;
  curCoord,nextCoord: TPointF;
begin
  nb := ComputeBezierCurvePrecision(p1,c1,c2,p2, AAcceptedDeviation);
  if nb <= 1 then nb := 2;
  result := 0;
  curCoord := p1;
  step := 1/(nb-1);
  t := 0;
  for i := 1 to nb-2 do
  begin
    t += step;
    nextCoord := ComputePointAt(t);
    result += VectLen(nextCoord-curCoord);
    curCoord := nextCoord;
  end;
  result += VectLen(p2-curCoord);
end;

function TCubicBezierCurve.ToPoints(AAcceptedDeviation: single;
  AIncludeFirstPoint: boolean = true): ArrayOfTPointF;
begin
  result := SimpleComputePoints(AAcceptedDeviation, AIncludeFirstPoint);
end;

{//The following function computes by splitting the curve. It is slower than the simple function.
function TCubicBezierCurve.ToPoints(AAcceptedDeviation: single;
  ARelativeDeviation: boolean): ArrayOfTPointF;
  function ToPointsRec(const ACurve: TCubicBezierCurve): ArrayOfTPointF;
  var simpleLen2: single;
    v: TPointF;
    left,right: TCubicBezierCurve;
    subLeft,subRight: ArrayOfTPointF;
    maxDev,dev1,dev2: single;
    subLeftLen: integer;

    procedure ComputeExtremum;
    begin
      raise Exception.Create('Not implemented');
      result := nil;
    end;

  begin
    v := ACurve.p2-ACurve.p1;
    simpleLen2 := v*v;
    if simpleLen2 = 0 then
    begin
      if (ACurve.c1.x = ACurve.p1.x) and (ACurve.c1.y = ACurve.p1.y) and
         (ACurve.c2.x = ACurve.p2.x) and (ACurve.c2.y = ACurve.p2.y) then
      begin
        result := nil;
        exit;
      end;
      ACurve.Split(left,right);
    end else
    begin
      ACurve.Split(left,right);
      if not ARelativeDeviation then simpleLen2:= sqrt(simpleLen2);
      maxDev := AAcceptedDeviation*simpleLen2;
      if abs(PointF(v.y,-v.x) * (left.p2-ACurve.p1)) <= maxDev then
      begin
        dev1 := PointF(v.y,-v.x) * (ACurve.c1-ACurve.p1);
        dev2 := PointF(v.y,-v.x) * (ACurve.c2-ACurve.p2);
        if not ((Sign(dev1)<>Sign(dev2)) and ((abs(dev1) > maxDev) or (abs(dev2) > maxDev))) then
        begin
          result := nil;
          if ((ACurve.c1-ACurve.p1)*v < -maxDev) or
             ((ACurve.c1-ACurve.p2)*v > maxDev) or
             ((ACurve.c2-ACurve.p1)*v < -maxDev) or
             ((ACurve.c2-ACurve.p2)*v > maxDev) then
            ComputeExtremum;
          exit;
        end;
      end;
    end;
    subRight := ToPointsRec(right);
    subLeft := ToPointsRec(left);
    subLeftLen := length(subLeft);

    //avoid leaving a gap in memory
    result := subLeft;
    subLeft := nil;
    setlength(result, subLeftLen+1+length(subRight));
    result[subLeftLen] := left.p2;
    move(subRight[0], result[subLeftLen+1], length(subRight)*sizeof(TPointF));
  end;

var
  subLen: integer;

begin
  if (c1.x = p1.x) and (c1.y = p1.y) and
     (c1.x = c2.x) and (c1.y = c2.y) and
     (c1.x = p2.x) and (c1.y = p2.y) then
  begin
    setlength(result,1);
    result[0] := c1;
    exit;
  end else
  begin
    result := ToPointsRec(self);
    subLen := length(result);
    setlength(result, length(result)+2);
    move(result[0], result[1], subLen*sizeof(TPointF));
    result[0] := p1;
    result[high(result)] := p2;
  end;
end;}

function TCubicBezierCurve.GetBounds: TRectF;
const precision = 1e-5;

  procedure Include(pt: TPointF);
  begin
    if pt.x < result.Left then result.Left := pt.x
    else if pt.x > result.Right then result.Right := pt.x;
    if pt.y < result.Top then result.Top := pt.y
    else if pt.y > result.Bottom then result.Bottom := pt.y;
  end;

  procedure IncludeT(t: single);
  begin
    if (t > 0) and (t < 1) then
      Include(ComputePointAt(t));
  end;

  procedure IncludeABC(a,b,c: single);
  var b2ac, sqrtb2ac: single;
  begin
    if abs(a) < precision then
    begin
      if abs(b) < precision then exit;
      IncludeT(-c/b);
    end else
    begin
      b2ac := sqr(b) - 4 * a * c;
      if b2ac >= 0 then
      begin
        sqrtb2ac := sqrt(b2ac);
        IncludeT((-b + sqrtb2ac) / (2 * a));
        IncludeT((-b - sqrtb2ac) / (2 * a));
      end;
    end;
  end;

var
  va, vb, vc: TPointF;

begin
  result.TopLeft := p1;
  result.BottomRight := p1;
  Include(p2);

  vb := 6 * p1 - 12 * c1 + 6 * c2;
  va := -3 * p1 + 9 * c1 - 9 * c2 + 3 * p2;
  vc := 3 * c1 - 3 * p1;

  IncludeABC(va.x,vb.x,vc.x);
  IncludeABC(va.y,vb.y,vc.y);
end;

{ TQuadraticBezierCurve }

function TQuadraticBezierCurve.SimpleComputePoints(AAcceptedDeviation: single;
  AIncludeFirstPoint: boolean = true): ArrayOfTPointF;
var
  t,step: single;
  i,nb: Integer;
begin
  nb := ComputeBezierCurvePrecision(p1,c,c,p2, AAcceptedDeviation);
  if nb <= 1 then nb := 2;
  if AIncludeFirstPoint then
  begin
    setlength(result,nb);
    result[0] := p1;
    result[nb-1] := p2;
    step := 1/(nb-1);
    t := 0;
    for i := 1 to nb-2 do
    begin
      t += step;
      result[i] := ComputePointAt(t);
    end;
  end else
  begin
    setlength(result,nb-1);
    result[nb-2] := p2;
    step := 1/(nb-1);
    t := 0;
    for i := 0 to nb-3 do
    begin
      t += step;
      result[i] := ComputePointAt(t);
    end;
  end;
end;

function TQuadraticBezierCurve.ComputeExtremumPositionOutsideSegment: single;
var a,b: single;
  v: TPointF;
begin
  v := self.p2-self.p1;
  a := (self.p1-2*self.c+self.p2)*v;
  if a = 0 then //no solution
  begin
    result := -1;
    exit;
  end;
  b := (self.c-self.p1)*v;
  result := -b/a;
end;

function TQuadraticBezierCurve.ComputePointAt(t: single): TPointF;
var
  rev_t,f2,t2: single;
begin
  rev_t := (1-t);
  f2 := rev_t*t*2;
  rev_t *= rev_t;
  t2 := t*t;
  result.x := rev_t*p1.x + f2*c.x + t2*p2.x;
  result.y := rev_t*p1.y + f2*c.y + t2*p2.y;
end;

procedure TQuadraticBezierCurve.Split(out ALeft, ARight: TQuadraticBezierCurve);
begin
  ALeft.p1 := p1;
  ALeft.c := 0.5*(p1+c);
  ARight.p2 := p2;
  ARight.c := 0.5*(p2+c);
  ALeft.p2 := 0.5*(ALeft.c+ARight.c);
  ARight.p1 := ALeft.p2;
end;

function TQuadraticBezierCurve.ComputeLength: single;
var a,b: TPointF;
  A_,AB_,B_,Sabc,A_2,A_32,B_2,BA,
  divisor: single;
  extremumPos: single;
  extremum: TPointF;
begin
  a := p1 - 2*c + p2;
  b := 2*(c - p1);
  A_ := 4*(a*a);
  B_ := b*b;
  if (A_ = 0) or (B_ = 0) then
  begin
    result := VectLen(p2-p1);
    exit;
  end;
  AB_ := 4*(a*b);

  A_2 := sqrt(A_);
  B_2 := 2*sqrt(B_);
  BA := AB_/A_2;
  divisor := BA+B_2;
  if divisor <= 0 then
  begin
    extremumPos:= ComputeExtremumPositionOutsideSegment;
    if (extremumPos <= 0) or (extremumPos >= 1) then
      result := VectLen(p2-p1)
    else
    begin
      extremum := ComputePointAt(extremumPos);
      result := VectLen(extremum-p1)+VectLen(p2-extremum);
    end;
    exit;
  end;

  Sabc := 2*sqrt(A_+AB_+B_);
  A_32 := 2*A_*A_2;
  result := ( A_32*Sabc +
              A_2*AB_*(Sabc-B_2) +
              (4*B_*A_-AB_*AB_)*ln( (2*A_2+BA+Sabc)/divisor )
            )/(4*A_32);
end;

function TQuadraticBezierCurve.ToPoints(AAcceptedDeviation: single;
  AIncludeFirstPoint: boolean = true): ArrayOfTPointF;
begin
  result := SimpleComputePoints(AAcceptedDeviation, AIncludeFirstPoint);
end;

function TQuadraticBezierCurve.GetBounds: TRectF;
const precision = 1e-5;

  procedure Include(pt: TPointF);
  begin
    if pt.x < result.Left then result.Left := pt.x
    else if pt.x > result.Right then result.Right := pt.x;
    if pt.y < result.Top then result.Top := pt.y
    else if pt.y > result.Bottom then result.Bottom := pt.y;
  end;

  procedure IncludeT(t: single);
  begin
    if (t > 0) and (t < 1) then
      Include(ComputePointAt(t));
  end;

  procedure IncludeABC(a,b,c: single);
  var denom: single;
  begin
    denom := a-2*b+c;
    if abs(denom) < precision then exit;
    IncludeT((a-b)/denom);
  end;

begin
  result.TopLeft := p1;
  result.BottomRight := p1;
  Include(p2);

  IncludeABC(p1.x,c.x,p2.x);
  IncludeABC(p1.y,c.y,p2.y);
end;

{//The following function computes by splitting the curve. It is slower than the simple function
function TQuadraticBezierCurve.ToPoints(AAcceptedDeviation: single; ARelativeDeviation: boolean): ArrayOfTPointF;

  function ToPointsRec(const ACurve: TQuadraticBezierCurve): ArrayOfTPointF;
  var simpleLen2: single;
    v: TPointF;
    left,right: TQuadraticBezierCurve;
    subLeft,subRight: ArrayOfTPointF;
    subLeftLen: Integer;

    procedure ComputeExtremum;
    var
      t: single;
    begin
      t := ACurve.ComputeExtremumPositionOutsideSegment;
      if (t <= 0) or (t >= 1) then
        result := nil
      else
      begin
        setlength(result,1);
        result[0] := ACurve.ComputePointAt(t);
      end;
    end;

  begin
    v := ACurve.p2-ACurve.p1;
    simpleLen2 := v*v;
    if simpleLen2 = 0 then
    begin
      if (ACurve.c.x = ACurve.p1.x) and (ACurve.c.y = ACurve.p1.y) then
      begin
        result := nil;
        exit;
      end;
      ACurve.Split(left,right);
    end else
    begin
      ACurve.Split(left,right);
      if not ARelativeDeviation then simpleLen2:= sqrt(simpleLen2);
      if abs(PointF(v.y,-v.x) * (left.p2-ACurve.p1))
          <= AAcceptedDeviation*simpleLen2 then
      begin
        result := nil;
        if ((ACurve.c-ACurve.p1)*v < -AAcceptedDeviation*simpleLen2) or
           ((ACurve.c-ACurve.p2)*v > AAcceptedDeviation*simpleLen2) then
          ComputeExtremum;
        exit;
      end;
    end;
    subRight := ToPointsRec(right);
    subLeft := ToPointsRec(left);
    subLeftLen := length(subLeft);

    //avoid leaving a gap in memory
    result := subLeft;
    subLeft := nil;
    setlength(result, subLeftLen+1+length(subRight));
    result[subLeftLen] := left.p2;
    move(subRight[0], result[subLeftLen+1], length(subRight)*sizeof(TPointF));
  end;

var
  subLen: integer;

begin
  if (c.x = p1.x) and (c.y = p1.y) and
     (c.x = p2.x) and (c.y = p2.y) then
  begin
    setlength(result,1);
    result[0] := c;
    exit;
  end else
  begin
    result := ToPointsRec(self);
    subLen := length(result);
    setlength(result, length(result)+2);
    move(result[0], result[1], subLen*sizeof(TPointF));
    result[0] := p1;
    result[high(result)] := p2;
  end;
end;}
{$ENDIF}
