unit BGRAFillInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmapTypes;

const
  AntialiasPrecision = 16;
  AntialiasPrecisionShift = 4;

type
  TDensity = word;
  PDensity = ^TDensity;

type
  { TFillShapeInfo }

  TFillShapeInfo = class(TBGRACustomFillInfo)
    protected
      //compute intersections. the array must be big enough
      procedure ComputeIntersection(cury: single; var inter: ArrayOfTIntersectionInfo; var nbInter: integer); virtual;
      //sort from left to right
      procedure SortIntersection(var inter: ArrayOfTIntersectionInfo; nbInter: integer); virtual;
      //apply non-zero winding rule. it can change the number of intersections
      procedure ConvertFromNonZeroWinding(var inter: ArrayOfTIntersectionInfo; var nbInter: integer); virtual;
      //returns maximum of intersection per line
      function NbMaxIntersection: integer; virtual;

    public
      //returns true if the same segment number can be curved
      function SegmentsCurved: boolean; override;

      //returns integer bounds
      function GetBounds: TRect; override;

      //compute min-max to be drawn on destination bitmap according to cliprect. Returns false if
      //there is nothing to draw
      function ComputeMinMax(out minx,miny,maxx,maxy: integer; bmpDest: TBGRACustomBitmap): boolean; override;

      //check if the point is inside the filling zone
      function IsPointInside(x,y: single; windingMode: boolean): boolean; override;

      //create an array that will contain computed intersections.
      //you may augment, in this case, use CreateIntersectionInfo for new items
      function CreateIntersectionArray: ArrayOfTIntersectionInfo; override;
      function CreateIntersectionInfo: TIntersectionInfo; override; //creates a single info
      procedure FreeIntersectionArray(var inter: ArrayOfTIntersectionInfo); override;

      //fill a previously created array of intersections with actual intersections at the current y coordinate.
      //nbInter gets the number of computed intersections
      procedure ComputeAndSort(cury: single; var inter: ArrayOfTIntersectionInfo; out nbInter: integer; windingMode: boolean); override;

  end;

  { TFillEllipseInfo }

  TFillEllipseInfo = class(TFillShapeInfo)
  private
    FX, FY, FRX, FRY: single;
    function GetCenter: TPointF;
  protected
    function NbMaxIntersection: integer; override;
    procedure ComputeIntersection(cury: single;
      var inter: ArrayOfTIntersectionInfo; var nbInter: integer); override;
  public
    WindingFactor: integer;
    constructor Create(x, y, rx, ry: single);
    function GetBounds: TRect; override;
    function SegmentsCurved: boolean; override;
    property Center: TPointF read GetCenter;
    property RadiusX: single read FRX;
    property RadiusY: single read FRY;
  end;

  { TFillBorderEllipseInfo }

  TFillBorderEllipseInfo = class(TFillShapeInfo)
  private
    FInnerBorder, FOuterBorder: TFillEllipseInfo;
  protected
    function NbMaxIntersection: integer; override;
    procedure ComputeIntersection(cury: single;
      var inter: ArrayOfTIntersectionInfo; var nbInter: integer); override;
  public
    constructor Create(x, y, rx, ry, w: single);
    function GetBounds: TRect; override;
    function SegmentsCurved: boolean; override;
    destructor Destroy; override;
    property InnerBorder: TFillEllipseInfo read FInnerBorder;
    property OuterBorder: TFillEllipseInfo read FOuterBorder;
  end;

  { TFillRoundRectangleInfo }

  TFillRoundRectangleInfo = class(TFillShapeInfo)
  private
    FX1, FY1, FX2, FY2, FRX, FRY: single;
    FOptions: TRoundRectangleOptions;
    function GetBottomRight: TPointF;
    function GetTopLeft: TPointF;
  protected
    function NbMaxIntersection: integer; override;
    procedure ComputeIntersection(cury: single;
      var inter: ArrayOfTIntersectionInfo; var nbInter: integer); override;
  public
    WindingFactor: integer;
    constructor Create(x1, y1, x2, y2, rx, ry: single; options: TRoundRectangleOptions);
    function SegmentsCurved: boolean; override;
    function GetBounds: TRect; override;
    property TopLeft: TPointF read GetTopLeft;
    property BottomRight: TPointF read GetBottomRight;
    property RadiusX: single read FRX;
    property RadiusY: single read FRY;
  end;

  { TFillBorderRoundRectInfo }

  TFillBorderRoundRectInfo = class(TFillShapeInfo)
  protected
    FInnerBorder, FOuterBorder: TFillRoundRectangleInfo;
    function NbMaxIntersection: integer; override;
    procedure ComputeIntersection(cury: single;
      var inter: ArrayOfTIntersectionInfo; var nbInter: integer); override;
  public
    constructor Create(x1, y1, x2, y2, rx, ry, w: single; options: TRoundRectangleOptions);
    function GetBounds: TRect; override;
    function SegmentsCurved: boolean; override;
    destructor Destroy; override;
    property InnerBorder: TFillRoundRectangleInfo read FInnerBorder;
    property OuterBorder: TFillRoundRectangleInfo read FOuterBorder;
  end;

  TPolySlice = record
    y1,y2: single;
    segments: array of record
                y1,x1: single;
                slope: single;
                winding: integer;
                data: pointer;
                id: integer;
              end;
    nbSegments: integer;
  end;

  { TCustomFillPolyInfo }

  TCustomFillPolyInfo = class(TFillShapeInfo)
  private
    function GetNbPoints: integer;
  protected
    FPoints:      array of TPointF;
    FSlopes:      array of single;
    FEmptyPt:     array of boolean;
    FNext, FPrev: array of integer;
    function NbMaxIntersection: integer; override;
    procedure SetIntersectionValues(AInter: TIntersectionInfo; AInterX: Single; AWinding, ANumSegment: integer; {%H-}dy: single; {%H-}AData: pointer); virtual;
  public
    constructor Create(const points: array of TPointF);
    function CreateSegmentData(numPt,nextPt: integer; x,y: single): pointer; virtual;
    procedure FreeSegmentData(data: pointer); virtual;
    function GetBounds: TRect; override;
    property NbPoints: integer read GetNbPoints;
  end;

  { TFillPolyInfo }

  TFillPolyInfo = class(TCustomFillPolyInfo)
  protected
    FSlices:   array of TPolySlice;
    FCurSlice: integer;
    FMaxIntersection: integer;
    function NbMaxIntersection: integer; override;
    procedure ComputeIntersection(cury: single;
      var inter: ArrayOfTIntersectionInfo; var nbInter: integer); override;
  public
    constructor Create(const points: array of TPointF);
    destructor Destroy; override;
  end;

  POnePassRecord = ^TOnePassRecord;
  TOnePassRecord = record
                id: integer;
                data: pointer;
                slope: single;
                winding: integer;
                includeStartingPoint: boolean;
                originalY1: single;
                x1,y1,y2: single;
                next: POnePassRecord;
                nextWaiting: POnePassRecord;
                nextDrawing: POnePassRecord;
            end;

  { TOnePassFillPolyInfo }

  TOnePassFillPolyInfo = class(TCustomFillPolyInfo)
  private
    procedure InsertionSortByY;
    function PartitionByY(left, right: integer): integer;
    procedure QuickSortByY(left, right: integer);
    procedure SortByY;
  protected
    FOnePass: array of TOnePassRecord;
    FSortedByY: array of POnePassRecord;
    FFirstWaiting, FFirstDrawing: POnePassRecord;
    FShouldInitializeDrawing: boolean;
    procedure ComputeIntersection(cury: single;
      var inter: ArrayOfTIntersectionInfo; var nbInter: integer); override;
  public
    constructor Create(const points: array of TPointF);
    function CreateIntersectionArray: ArrayOfTIntersectionInfo; override;
    destructor Destroy; override;
  end;

  { TSimpleFillPolyInfo }

  TSimpleFillPolyInfo = class(TCustomFillPolyInfo)
  protected
    FSimple: array of record
                  winding: integer;
                  includeStartingPoint: boolean;
                  data: pointer;
    end;
    procedure ComputeIntersection(cury: single; var inter: ArrayOfTIntersectionInfo;
      var nbInter: integer); override;
  public
    constructor Create(const points: array of TPointF);
    destructor Destroy; override;
  end;

procedure AddDensity(dest: PDensity; start,count: integer; value : word); inline;
function DivByAntialiasPrecision(value: UInt32or64): UInt32or64; inline;
function DivByAntialiasPrecision256(value: UInt32or64): UInt32or64; inline;
function DivByAntialiasPrecision65536(value: UInt32or64): UInt32or64; inline;
procedure ComputeAliasedRowBounds(x1,x2: single; minx,maxx: integer; out ix1,ix2: integer);

function IsPointInPolygon(const points: ArrayOfTPointF; point: TPointF; windingMode: boolean): boolean;
function IsPointInEllipse(x,y,rx,ry: single; point: TPointF): boolean;
function IsPointInRoundRectangle(x1, y1, x2, y2, rx, ry: single; point: TPointF): boolean;
function IsPointInRectangle(x1, y1, x2, y2: single; point: TPointF): boolean;

implementation

uses Math;

procedure ComputeAliasedRowBounds(x1,x2: single; minx,maxx: integer; out ix1,ix2: integer);
begin
  if frac(x1)=0.5 then
    ix1 := trunc(x1) else
    ix1 := round(x1);
  if frac(x2)=0.5 then
    ix2 := trunc(x2)-1 else
    ix2 := round(x2)-1;

  if ix1 < minx then
    ix1 := minx;
  if ix2 >= maxx then
    ix2 := maxx;
end;

function IsPointInPolygon(const points: ArrayOfTPointF; point: TPointF
  ; windingMode: boolean): boolean;
var info: TBGRACustomFillInfo;
begin
  info := TSimpleFillPolyInfo.Create(points);
  result := info.IsPointInside(point.x+0.5,point.y+0.5,windingMode);
  info.free;
end;

function IsPointInEllipse(x, y, rx, ry: single; point: TPointF): boolean;
var info: TBGRACustomFillInfo;
begin
  info := TFillEllipseInfo.Create(x,y,rx,ry);
  result := info.IsPointInside(point.x+0.5,point.y+0.5,false);
  info.free;
end;

function IsPointInRoundRectangle(x1, y1, x2, y2, rx, ry: single; point: TPointF
  ): boolean;
var info: TBGRACustomFillInfo;
begin
  info := TFillRoundRectangleInfo.Create(x1, y1, x2, y2, rx, ry,[]);
  result := info.IsPointInside(point.x+0.5,point.y+0.5,false);
  info.free;
end;

function IsPointInRectangle(x1, y1, x2, y2: single; point: TPointF): boolean;
begin
  with point do
    result := (((x1<x) and (x2>x)) or ((x1>x) and (x2<x))) and
              (((y1<y) and (y2>y)) or ((y1>y) and (y2<y)));
end;

procedure AddDensity(dest: PDensity; start,count: integer; value: word);
var valueValue: longword;
    lastAdd: integer;
begin
  if count=0 then exit;
  inc(dest,start);
  if start and 1 = 1 then
  begin
    dest^ += value;
    inc(dest);
    dec(count);
  end;
  lastAdd := count and 1;
  count := count shr 1;
  if count > 0 then
  begin
    valueValue := value+(value shl 16);
    while count > 0 do
    begin
      plongword(dest)^ += valueValue;
      inc(dest,2);
      dec(count);
    end;
  end;
  if lastAdd <> 0 then
    dest^ += value;
end;

function DivByAntialiasPrecision(value: UInt32or64): UInt32or64;
begin             //
  result := value shr AntialiasPrecisionShift;// div AntialiasPrecision;
end;

function DivByAntialiasPrecision256(value: UInt32or64): UInt32or64;
begin             //
  result := value shr (AntialiasPrecisionShift+8);// div (256*AntialiasPrecision);
end;

function DivByAntialiasPrecision65536(value: UInt32or64): UInt32or64;
begin             //
  result := value shr (AntialiasPrecisionShift+16);//div (65536*AntialiasPrecision);
end;

{ TFillShapeInfo }

function TFillShapeInfo.GetBounds: TRect;
begin
  Result := rect(0, 0, 0, 0);
end;

function TFillShapeInfo.ComputeMinMax(out minx, miny, maxx, maxy: integer;
  bmpDest: TBGRACustomBitmap): boolean;
var clip,bounds: TRect;
begin
  result := true;
  bounds := GetBounds;

  if (bounds.Right <= bounds.left) or (bounds.bottom <= bounds.top) then
  begin
    result := false;
    exit;
  end;

  miny := bounds.top;
  maxy := bounds.bottom - 1;
  minx := bounds.left;
  maxx := bounds.right - 1;

  clip := bmpDest.ClipRect;

  if minx < clip.Left then
    minx := clip.Left;
  if maxx < clip.Left then
    result := false;

  if maxx > clip.Right - 1 then
    maxx := clip.Right- 1;
  if minx > clip.Right - 1 then
    result := false;

  if miny < clip.Top then
    miny := clip.Top;
  if maxy < clip.Top then
    result := false;

  if maxy > clip.Bottom - 1 then
    maxy := clip.Bottom - 1;
  if miny > clip.Bottom - 1 then
    result := false;
end;

function TFillShapeInfo.IsPointInside(x, y: single; windingMode: boolean
  ): boolean;
var
    inter : ArrayOfTIntersectionInfo;
    i,nbInter: integer;
begin
  inter := CreateIntersectionArray;
  ComputeAndSort(y,inter,nbInter,windingMode);
  i := 0;
  while i+1 < nbInter do
  begin
    if (inter[i].interX < x) and (inter[i+1].interX > x) then
    begin
      result := true;
      FreeIntersectionArray(inter);
      exit;
    end;
    inc(i,2);
  end;
  result := false;
  FreeIntersectionArray(inter);
end;

function TFillShapeInfo.NbMaxIntersection: integer;
begin
  Result := 0;
end;

function TFillShapeInfo.SegmentsCurved: boolean;
begin
  result := false;
end;

function TFillShapeInfo.CreateIntersectionInfo: TIntersectionInfo;
begin
  result := TIntersectionInfo.Create;
end;

procedure TFillShapeInfo.FreeIntersectionArray(
  var inter: ArrayOfTIntersectionInfo);
var
  i: Integer;
begin
  for i := 0 to high(inter) do
    inter[i].free;
  inter := nil;
end;

{$hints off}
procedure TFillShapeInfo.ComputeIntersection(cury: single;
      var inter: ArrayOfTIntersectionInfo; var nbInter: integer);
begin

end;
{$hints on}

procedure TFillShapeInfo.SortIntersection(var inter: ArrayOfTIntersectionInfo; nbInter: integer);
var
  i,j: Integer;
  tempInter: TIntersectionInfo;
begin
  for i := 1 to nbinter - 1 do
  begin
    j := i;
    while (j > 0) and (inter[j - 1].interX > inter[j].interX) do
    begin
      tempInter    := inter[j - 1];
      inter[j - 1] := inter[j];
      inter[j]     := tempInter;
      Dec(j);
    end;
  end;
end;

procedure TFillShapeInfo.ConvertFromNonZeroWinding(var inter: ArrayOfTIntersectionInfo; var nbInter: integer);
var windingSum,prevSum,i,nbAlternate: integer;
    tempInfo: TIntersectionInfo;
begin
  windingSum := 0;
  nbAlternate := 0;
  for i := 0 to nbInter-1 do
  begin
    prevSum := windingSum;
    windingSum += inter[i].winding;
    if (windingSum = 0) xor (prevSum = 0) then
    begin
      tempInfo := inter[nbAlternate];
      inter[nbAlternate] := inter[i];
      inter[i] := tempInfo;
      inc(nbAlternate);
    end;
  end;
  nbInter := nbAlternate;
end;

procedure TFillShapeInfo.ComputeAndSort(cury: single;
  var inter: ArrayOfTIntersectionInfo; out nbInter: integer; windingMode: boolean);
begin
  nbInter := 0;
  ComputeIntersection(cury,inter,nbInter);
  if nbInter < 2 then exit;
  SortIntersection(inter,nbInter);
  if windingMode then ConvertFromNonZeroWinding(inter,nbInter);
end;

function TFillShapeInfo.CreateIntersectionArray: ArrayOfTIntersectionInfo;
var
  i: Integer;
begin
  setlength(result, NbMaxIntersection);
  for i := 0 to high(result) do
    result[i] := CreateIntersectionInfo;
end;

function ComputeWinding(y1,y2: single): integer;
begin
    if y2 > y1 then result := 1 else
    if y2 < y1 then result := -1 else
      result := 0;
end;

type
  arrayOfSingle = array of single;

procedure InsertionSortSingles(var a: arrayOfSingle);
var i,j: integer;
    temp: single;
begin
  for i := 1 to high(a) do
  begin
    Temp := a[i];
    j := i;
    while (j>0) and (a[j-1]> Temp) do
    begin
      a[j] := a[j-1];
      dec(j);
    end;
    a[j] := Temp;
  end;
end;

function PartitionSingles(var a: arrayOfSingle; left,right: integer): integer;

  procedure Swap(idx1,idx2: integer); inline;
  var temp: single;
  begin
    temp := a[idx1];
    a[idx1] := a[idx2];
    a[idx2] := temp;
  end;

var pivotIndex: integer;
    pivotValue: single;
    storeIndex: integer;
    i: integer;

begin
  pivotIndex := left + random(right-left+1);
  pivotValue := a[pivotIndex];
  swap(pivotIndex,right);
  storeIndex := left;
  for i := left to right-1 do
    if a[i] <= pivotValue then
    begin
      swap(i,storeIndex);
      inc(storeIndex);
    end;
  swap(storeIndex,right);
  result := storeIndex;
end;

procedure QuickSortSingles(var a: arrayOfSingle; left,right: integer);
var pivotNewIndex: integer;
begin
  if right > left+9 then
  begin
    pivotNewIndex := PartitionSingles(a,left,right);
    QuickSortSingles(a,left,pivotNewIndex-1);
    QuickSortSingles(a,pivotNewIndex+1,right);
  end;
end;

procedure SortSingles(var a: arrayOfSingle);
begin
  if length(a) < 10 then InsertionSortSingles(a) else
  begin
    QuickSortSingles(a,0,high(a));
    InsertionSortSingles(a);
  end;
end;

procedure RemoveSingleDuplicates(var a: arrayOfSingle; var nb: integer);
var i,idx: integer;
begin
  idx := 0;
  for i := 1 to nb-1 do
  begin
    if a[i] <> a[idx] then
    begin
      inc(idx);
      a[idx] := a[i];
    end;
  end;
  nb := idx+1;
end;

function BinarySearchSingle(value: single; var a: arrayOfSingle; left,right: integer): integer;
var pivotIndex: integer;
    pivotValue: single;
begin
  pivotIndex := (left+right) div 2;
  pivotValue := a[pivotIndex];
  if value = pivotValue then
    result := pivotIndex else
  if value < pivotValue then
  begin
    if pivotIndex = left then result := left else
      result := BinarySearchSingle(value, a, left,pivotIndex-1);
  end else
  begin
    if pivotIndex = right then result := right+1 else
      result := BinarySearchSingle(value, a, pivotIndex+1, right);
  end;
end;

{ TCustomFillPolyInfo }

constructor TCustomFillPolyInfo.Create(const points: array of TPointF);
var
  i, j: integer;
  First, cur, nbP: integer;
begin
  setlength(FPoints, length(points));
  nbP := 0;
  for i := 0 to high(points) do
  if (i=0) or (points[i]<>points[i-1]) then
  begin
    FPoints[nbP] := points[i];
    inc(nbP);
  end;
  if (nbP>0) and (FPoints[nbP-1] = FPoints[0]) then dec(NbP);
  setlength(FPoints, nbP);

  //look for empty points, correct coordinate and successors
  setlength(FEmptyPt, length(FPoints));
  setlength(FNext, length(FPoints));

  cur   := -1;
  First := -1;
  for i := 0 to high(FPoints) do
    if not isEmptyPointF(FPoints[i]) then
    begin
      FEmptyPt[i]  := False;
      FPoints[i].x += 0.5;
      FPoints[i].y += 0.5;
      if cur <> -1 then
        FNext[cur] := i;
      if First = -1 then
        First := i;
      cur     := i;
    end
    else
    begin
      if (First <> -1) and (cur <> First) then
        FNext[cur] := First;

      FEmptyPt[i] := True;
      FNext[i] := -1;
      cur   := -1;
      First := -1;
    end;
  if (First <> -1) and (cur <> First) then
    FNext[cur] := First;

  setlength(FPrev, length(FPoints));
  for i := 0 to high(FPrev) do
    FPrev[i] := -1;
  for i := 0 to high(FNext) do
    if FNext[i] <> -1 then
      FPrev[FNext[i]] := i;

  setlength(FSlopes, length(FPoints));

  //compute slopes
  for i := 0 to high(FPoints) do
    if not FEmptyPt[i] then
    begin
      j := FNext[i];

      if FPoints[i].y <> FPoints[j].y then
        FSlopes[i] := (FPoints[j].x - FPoints[i].x) / (FPoints[j].y - FPoints[i].y)
      else
        FSlopes[i] := EmptySingle;
    end
    else
      FSlopes[i]    := EmptySingle;
end;

{$hints off}
function TCustomFillPolyInfo.CreateSegmentData(numPt,nextPt: integer; x, y: single
  ): pointer;
begin
  result := nil;
end;
{$hints on}

procedure TCustomFillPolyInfo.FreeSegmentData(data: pointer);
begin
  freemem(data);
end;

function TCustomFillPolyInfo.GetBounds: TRect;
var
  minx, miny, maxx, maxy, i: integer;
begin
  if length(FPoints) = 0 then
  begin
    result := rect(0,0,0,0);
    exit;
  end;
  miny := floor(FPoints[0].y);
  maxy := ceil(FPoints[0].y);
  minx := floor(FPoints[0].x);
  maxx := ceil(FPoints[0].x);
  for i := 1 to high(FPoints) do
    if not FEmptyPt[i] then
    begin
      if floor(FPoints[i].y) < miny then
        miny := floor(FPoints[i].y)
      else
      if ceil(FPoints[i].y) > maxy then
        maxy := ceil(FPoints[i].y);

      if floor(FPoints[i].x) < minx then
        minx := floor(FPoints[i].x)
      else
      if ceil(FPoints[i].x) > maxx then
        maxx := ceil(FPoints[i].x);
    end;
  Result := rect(minx, miny, maxx + 1, maxy + 1);
end;

function TCustomFillPolyInfo.GetNbPoints: integer;
begin
  result := length(FPoints);
end;

function TCustomFillPolyInfo.NbMaxIntersection: integer;
begin
  Result := length(FPoints);
end;

procedure TCustomFillPolyInfo.SetIntersectionValues(AInter: TIntersectionInfo;
  AInterX: Single; AWinding, ANumSegment: integer; dy: single; AData: pointer);
begin
  AInter.SetValues( AInterX, AWinding, ANumSegment );
end;

{ TFillPolyInfo }

function TFillPolyInfo.NbMaxIntersection: integer;
begin
  Result:= FMaxIntersection;
end;

procedure TFillPolyInfo.ComputeIntersection(cury: single;
  var inter: ArrayOfTIntersectionInfo; var nbInter: integer);
var
  j: integer;
begin
  if length(FSlices)=0 then exit;

  while (cury < FSlices[FCurSlice].y1) and (FCurSlice > 0) do dec(FCurSlice);
  while (cury > FSlices[FCurSlice].y2) and (FCurSlice < high(FSlices)) do inc(FCurSlice);
  with FSlices[FCurSlice] do
  if (cury >= y1) and (cury <= y2) then
  begin
    for j := 0 to nbSegments-1 do
    begin
      SetIntersectionValues(inter[nbinter], (cury - segments[j].y1) * segments[j].slope + segments[j].x1,
                                segments[j].winding, segments[j].id, cury - segments[j].y1, segments[j].data );
      Inc(nbinter);
    end;
  end;
end;

constructor TFillPolyInfo.Create(const points: array of TPointF);
  function AddSeg(numSlice: integer): integer;
  begin
    result := FSlices[numSlice].nbSegments;
    if length(FSlices[numSlice].segments)=FSlices[numSlice].nbSegments then
      setlength(FSlices[numSlice].segments,FSlices[numSlice].nbSegments*2+2);
    inc(FSlices[numSlice].nbSegments);
  end;

var
  yList: array of single;
  nbYList: integer;
  ya,yb,temp: single;
  sliceStart,sliceEnd,idxSeg: integer;
  i,j,k,idSeg: integer;

begin
  inherited Create(points);

  //slice
  nbYList:= length(FPoints);
  setlength(YList, nbYList);
  for i := 0 to high(FPoints) do
    YList[i] := FPoints[i].y;

  SortSingles(YList);
  RemoveSingleDuplicates(YList, nbYList);

  setlength(FSlices, nbYList-1);
  for i := 0 to high(FSlices) do
  begin
    FSlices[i].y1 := YList[i];
    FSlices[i].y2 := YList[i+1];
    FSlices[i].nbSegments := 0;
  end;

  idSeg := 0;
  for j := 0 to high(FSlopes) do
  begin
    if FSlopes[j]<>EmptySingle then
    begin
      k := FNext[j];

      ya := FPoints[j].y;
      yb := FPoints[k].y;
      if yb < ya then
      begin
        temp := ya;
        ya := yb;
        yb := temp;
      end;
      sliceStart := BinarySearchSingle(ya,YList,0,nbYList-1);
      sliceEnd := BinarySearchSingle(yb,YList,0,nbYList-1);
      if sliceEnd > high(FSlices) then sliceEnd := high(FSlices);
      for i := sliceStart to sliceEnd do
      begin
        if ((FPoints[j].y < FSlices[i].y2) and
           (FPoints[k].y > FSlices[i].y1)) or
           ((FPoints[k].y < FSlices[i].y2) and
           (FPoints[j].y > FSlices[i].y1)) then
        begin
          idxSeg := AddSeg(i);
          with FSlices[i].segments[idxSeg] do
          begin
            x1 := (FSlices[i].y1 - FPoints[j].y) * FSlopes[j] + FPoints[j].x;
            y1 := FSlices[i].y1;
            slope := FSlopes[j];
            winding := ComputeWinding(FPoints[j].y,FPoints[k].y);
            data := CreateSegmentData(j,k,x1,y1);
            inc(idSeg);
            id := idSeg;
          end;
        end;
      end;
    end;
  end;

  FCurSlice := 0;
  FMaxIntersection:= 0;
  for i := 0 to high(FSlices) do
    if FSlices[i].nbSegments > FMaxIntersection then
      FMaxIntersection:= FSlices[i].nbSegments;
end;

destructor TFillPolyInfo.Destroy;
var i,j: integer;
begin
  for i := 0 to high(FSlices) do
    with FSlices[i] do
      for j := 0 to nbSegments-1 do
        if segments[j].data <> nil then FreeSegmentData(segments[j].data);
  inherited Destroy;
end;

{ TOnePassFillPolyInfo }

function TOnePassFillPolyInfo.PartitionByY(left,right: integer): integer;

  procedure Swap(idx1,idx2: integer); inline;
  var temp: POnePassRecord;
  begin
    temp := FSortedByY[idx1];
    FSortedByY[idx1] := FSortedByY[idx2];
    FSortedByY[idx2] := temp;
  end;

var pivotIndex: integer;
    pivotValue: single;
    storeIndex: integer;
    i: integer;

begin
  pivotIndex := left + random(right-left+1);
  pivotValue := FSortedByY[pivotIndex]^.y1;
  swap(pivotIndex,right);
  storeIndex := left;
  for i := left to right-1 do
    if FSortedByY[i]^.y1 <= pivotValue then
    begin
      swap(i,storeIndex);
      inc(storeIndex);
    end;
  swap(storeIndex,right);
  result := storeIndex;
end;

procedure TOnePassFillPolyInfo.QuickSortByY(left,right: integer);
var pivotNewIndex: integer;
begin
  if right > left+9 then
  begin
    pivotNewIndex := PartitionByY(left,right);
    QuickSortByY(left,pivotNewIndex-1);
    QuickSortByY(pivotNewIndex+1,right);
  end;
end;

procedure TOnePassFillPolyInfo.InsertionSortByY;
var i,j: integer;
    tempValue: single;
    tempPtr: POnePassRecord;
begin
  for i := 1 to high(FSortedByY) do
  begin
    tempPtr := FSortedByY[i];
    TempValue := tempPtr^.y1;
    j := i;
    while (j>0) and (FSortedByY[j-1]^.y1 > TempValue) do
    begin
      FSortedByY[j] := FSortedByY[j-1];
      dec(j);
    end;
    FSortedByY[j] := tempPtr;
  end;
end;

procedure TOnePassFillPolyInfo.SortByY;
var i,nbSorted: integer;
begin
  setlength(FSortedByY, length(FPoints));
  nbSorted := 0;
  for i := 0 to high(FSortedByY) do
    if not FEmptyPt[i] then
    begin
      FSortedByY[nbSorted] := @FOnePass[i];
      inc(nbSorted);
    end;
  setlength(FSortedByY,nbSorted);
  if length(FSortedByY) < 10 then InsertionSortByY else
  begin
    QuickSortByY(0,high(FSortedByY));
    InsertionSortByY;
  end;
end;

procedure TOnePassFillPolyInfo.ComputeIntersection(cury: single;
  var inter: ArrayOfTIntersectionInfo; var nbInter: integer);
var
  p,pprev,pnext: POnePassRecord;
begin
  FShouldInitializeDrawing := true;

  p := FFirstWaiting;
  while p <> nil do
  begin
    if (cury >= p^.y1) then
    begin
      if cury <= p^.y2+1 then
      begin
        p^.nextDrawing := FFirstDrawing;
        FFirstDrawing := p;
      end;
    end
      else break;
    p := p^.nextWaiting;
  end;
  FFirstWaiting:= p;

  p := FFirstDrawing;
  pprev := nil;
  while p <> nil do
  begin
    pnext := p^.nextDrawing;
{    if p^.slope = EmptySingle then
      raise exception.Create('Unexpected');}
    if ((cury > p^.y1) and (cury <= p^.y2)) or
       (p^.includeStartingPoint and (cury = p^.y1)) then
    begin
{      if nbinter = length(inter) then
        raise exception.Create('too much'); }
      if inter[nbinter] = nil then inter[nbinter] := CreateIntersectionInfo;
      SetIntersectionValues(inter[nbinter], (cury - p^.y1)*p^.slope + p^.x1, p^.winding, p^.id, cury - p^.originalY1, p^.data);
      inc(nbinter);
    end else
    if (cury > p^.y2+1) then
    begin
      if pprev <> nil then
        pprev^.nextDrawing := pnext
      else
        FFirstDrawing:= pnext;
      p := pnext;
      continue;
    end;
    pprev := p;
    p := pnext;
  end;
end;

constructor TOnePassFillPolyInfo.Create(const points: array of TPointF);
var i,j: integer;
  p: POnePassRecord;
  temp: single;
begin
  inherited create(points);

  FShouldInitializeDrawing := true;
  setlength(FOnePass, length(FPoints));
  for i := 0 to high(FPoints) do
  if not FEmptyPt[i] then
  begin
    p := @FOnePass[i];
    j := FNext[i];
    p^.next := @FOnePass[FNext[i]];
    p^.id := i;
    p^.slope := FSlopes[i];
    if p^.slope <> EmptySingle then
      p^.data := CreateSegmentData(i, j, FPoints[i].x, FPoints[i].y);
    p^.y1 := FPoints[i].y;
    p^.y2 := FPoints[j].y;
    p^.originalY1 := p^.y1;
    p^.winding:= ComputeWinding(p^.y1,p^.y2);
    if p^.y1 < p^.y2 then
      p^.x1 := FPoints[i].x
    else
    begin
      temp := p^.y1;
      p^.y1 := p^.y2;
      p^.y2 := temp;
      p^.x1 := FPoints[j].x;
    end;
  end;

  SortByY;
end;

function TOnePassFillPolyInfo.CreateIntersectionArray: ArrayOfTIntersectionInfo;
var i: integer;
  p,pprev: POnePassRecord;
begin
  if FShouldInitializeDrawing then
  begin
    FShouldInitializeDrawing := false;
    FFirstWaiting:= nil;
    pprev := nil;
    for i := 0 to high(FSortedByY) do
    begin
      p := FSortedByY[i];
      if p^.winding > 0 then
        p^.includeStartingPoint := p^.next^.winding <= 0
      else if p^.winding < 0 then
        p^.includeStartingPoint := p^.next^.winding >= 0;
      if p^.slope <> EmptySingle then
      begin
        if pprev <> nil then
          pprev^.nextWaiting:= p
        else
          FFirstWaiting := p;
        pprev := p;
      end;
    end;
  end;

  setlength(result, NbMaxIntersection);
end;

destructor TOnePassFillPolyInfo.Destroy;
var i: integer;
begin
  for i := 0 to high(FOnePass) do
    if FOnePass[i].data<>nil then FreeSegmentData(FOnePass[i].data);
  FOnePass := nil;
  inherited Destroy;
end;

{ TSimpleFillPolyInfo }

procedure TSimpleFillPolyInfo.ComputeIntersection(cury: single;
  var inter: ArrayOfTIntersectionInfo; var nbInter: integer);
var i,j: integer;
begin
  for i := 0 to high(FPoints) do
    if FSlopes[i] <> EmptySingle then
    begin
      j := FNext[i];
      if ((cury > FPoints[i].y) and (cury <= FPoints[j].y)) or
        ((cury < FPoints[i].y) and (cury >= FPoints[j].y)) or
       (FSimple[i].includeStartingPoint and (cury = FPoints[i].y)) then
     begin
       SetIntersectionValues(inter[nbinter], (cury - FPoints[i].y)*FSlopes[i] + FPoints[i].x, FSimple[i].winding, i, cury - FPoints[i].y, FSimple[i].data);
       inc(nbinter);
     end;
    end;
end;

constructor TSimpleFillPolyInfo.Create(const points: array of TPointF);
var i,j: integer;
begin
  inherited Create(points);

  setlength(FSimple, length(FPoints));
  for i := 0 to high(FPoints) do
  begin
    j := FNext[i];
    if j <> -1 then
      FSimple[i].winding:= ComputeWinding(FPoints[i].y,FPoints[j].y)
    else
      FSimple[i].winding:= 0;
    if FSlopes[i] <> EmptySingle then
      FSimple[i].data := CreateSegmentData(i, j, FPoints[i].x, FPoints[i].y);
  end;
end;

destructor TSimpleFillPolyInfo.Destroy;
var i: integer;
begin
  for i := 0 to high(FSimple) do
    if FSimple[i].data <> nil then
      FreeSegmentData(FSimple[i].data);
  FSimple := nil;
  inherited Destroy;
end;

{ TFillEllipseInfo }

constructor TFillEllipseInfo.Create(x, y, rx, ry: single);
begin
  FX  := x + 0.5;
  FY  := y + 0.5;
  FRX := abs(rx);
  FRY := abs(ry);
  WindingFactor := 1;
end;

function TFillEllipseInfo.GetBounds: TRect;
begin
  Result := rect(floor(fx - frx), floor(fy - fry), ceil(fx + frx), ceil(fy + fry));
end;

function TFillEllipseInfo.SegmentsCurved: boolean;
begin
  Result:= true;
end;

function TFillEllipseInfo.GetCenter: TPointF;
begin
  result := PointF(FX-0.5,FY-0.5);
end;

function TFillEllipseInfo.NbMaxIntersection: integer;
begin
  Result := 2;
end;

procedure TFillEllipseInfo.ComputeIntersection(cury: single;
      var inter: ArrayOfTIntersectionInfo; var nbInter: integer);
var
  d: single;
begin
  if (FRY <= 0) or (FRX <= 0) then exit;
  d := sqr((cury - FY) / FRY);
  if d < 1 then
  begin
    d := sqrt(1 - d) * FRX;
    inter[nbinter].SetValues( FX - d, -windingFactor, 0);
    Inc(nbinter);
    inter[nbinter].SetValues( FX + d, windingFactor, 1);
    Inc(nbinter);
  end;
end;

{ TFillBorderEllipseInfo }

constructor TFillBorderEllipseInfo.Create(x, y, rx, ry, w: single);
begin
  if rx < 0 then
    rx := -rx;
  if ry < 0 then
    ry := -ry;
  FOuterBorder := TFillEllipseInfo.Create(x, y, rx + w / 2, ry + w / 2);
  if (rx > w / 2) and (ry > w / 2) then
  begin
    FInnerBorder := TFillEllipseInfo.Create(x, y, rx - w / 2, ry - w / 2);
    FInnerBorder.WindingFactor := -1;
  end
  else
    FInnerBorder := nil;
end;

function TFillBorderEllipseInfo.GetBounds: TRect;
begin
  Result := FOuterBorder.GetBounds;
end;

function TFillBorderEllipseInfo.SegmentsCurved: boolean;
begin
  Result:= FOuterBorder.SegmentsCurved;
  if FInnerBorder <> nil then result := result or FInnerBorder.SegmentsCurved;
end;

function TFillBorderEllipseInfo.NbMaxIntersection: integer;
begin
  Result := 4;
end;

procedure TFillBorderEllipseInfo.ComputeIntersection(cury: single;
      var inter: ArrayOfTIntersectionInfo; var nbInter: integer);
begin
  FOuterBorder.ComputeIntersection(cury, inter, nbInter);
  if FInnerBorder <> nil then
    FInnerBorder.ComputeIntersection(cury, inter, nbInter);
end;

destructor TFillBorderEllipseInfo.Destroy;
begin
  FOuterBorder.Free;
  if FInnerBorder <> nil then
    FInnerBorder.Free;
  inherited Destroy;
end;

{ TFillRoundRectangleInfo }

constructor TFillRoundRectangleInfo.Create(x1, y1, x2, y2, rx, ry: single; options: TRoundRectangleOptions);
var
  temp: Single;
begin
  if y1 > y2 then
  begin
    temp := y1;
    y1 := y2;
    y2 := temp;
  end;
  if x1 > x2 then
  begin
    temp := x1;
    x1 := x2;
    x2 := temp;
  end;
  FX1  := x1 + 0.5;
  FY1  := y1 + 0.5;
  FX2  := x2 + 0.5;
  FY2  := y2 + 0.5;
  FRX := abs(rx);
  FRY := abs(ry);
  if 2*FRX > x2-x1 then FRX := (x2-x1)/2;
  if 2*FRY > y2-y1 then FRY := (y2-y1)/2;
  FOptions:= options;
  WindingFactor := 1;
end;

function TFillRoundRectangleInfo.SegmentsCurved: boolean;
begin
  if (not (rrTopLeftSquare in FOptions) and not (rrTopLeftBevel in FOptions)) or
     (not (rrTopRightSquare in FOptions) and not (rrTopRightBevel in FOptions)) or
     (not (rrBottomRightSquare in FOptions) and not (rrBottomRightBevel in FOptions)) or
     (not (rrBottomLeftSquare in FOptions) and not (rrBottomLeftBevel in FOptions)) then
     result := true else result := false;
end;

function TFillRoundRectangleInfo.GetBounds: TRect;
begin
  result := rect(floor(fx1),floor(fy1),floor(fx2)+1,floor(fy2)+1);
end;

function TFillRoundRectangleInfo.GetBottomRight: TPointF;
begin
  result := PointF(FX2-0.5,FY2-0.5);
end;

function TFillRoundRectangleInfo.GetTopLeft: TPointF;
begin
  result := PointF(FX1-0.5,FY1-0.5);
end;

function TFillRoundRectangleInfo.NbMaxIntersection: integer;
begin
  result := 2;
end;

procedure TFillRoundRectangleInfo.ComputeIntersection(cury: single;
      var inter: ArrayOfTIntersectionInfo; var nbInter: integer);
var
  d,d2: single;
begin
  if (cury >= FY1) and (cury <= FY2) then
  begin
    if cury < FY1+FRY then
    begin
      d := abs((cury - (FY1+FRY)) / FRY);
      d2 := sqrt(1 - sqr(d)) * FRX;

      if rrTopLeftSquare in FOptions then
        inter[nbinter].interX := FX1 else
      if rrTopLeftBevel in FOptions then
        inter[nbinter].interX := FX1 + d*FRX
      else
        inter[nbinter].interX := FX1 + FRX - d2;
      inter[nbinter].winding := -windingFactor;
      inter[nbinter].numSegment := 0;
      Inc(nbinter);

      if rrTopRightSquare in FOptions then
        inter[nbinter].interX := FX2 else
      if rrTopRightBevel in FOptions then
        inter[nbinter].interX := FX2 - d*FRX
      else
        inter[nbinter].interX := FX2 - FRX + d2;
      inter[nbinter].winding := +windingFactor;
      inter[nbinter].numSegment := 1;
      Inc(nbinter);
    end else
    if cury > FY2-FRY then
    begin
      d := abs((cury - (FY2-FRY)) / FRY);
      d2 := sqrt(1 - sqr(d)) * FRX;

      if rrBottomLeftSquare in FOptions then
        inter[nbinter].interX := FX1 else
      if rrBottomLeftBevel in FOptions then
        inter[nbinter].interX := FX1 + d*FRX
      else
        inter[nbinter].interX := FX1 + FRX - d2;
      inter[nbinter].winding := -windingFactor;
      inter[nbinter].numSegment := 0;
      Inc(nbinter);

      if rrBottomRightSquare in FOptions then
        inter[nbinter].interX := FX2 else
      if rrBottomRightBevel in FOptions then
        inter[nbinter].interX := FX2 - d*FRX
      else
        inter[nbinter].interX := FX2 - FRX + d2;
      inter[nbinter].winding := +windingFactor;
      inter[nbinter].numSegment := 1;
      Inc(nbinter);
    end else
    begin
      inter[nbinter].interX := FX1;
      inter[nbinter].winding := -windingFactor;
      inter[nbinter].numSegment := 0;
      Inc(nbinter);
      inter[nbinter].interX := FX2;
      inter[nbinter].winding := +windingFactor;
      inter[nbinter].numSegment := 1;
      Inc(nbinter);
    end;
  end;
end;

{ TFillBorderRoundRectInfo }

constructor TFillBorderRoundRectInfo.Create(x1, y1, x2, y2, rx, ry, w: single; options: TRoundRectangleOptions);
var rdiff: single;
  temp: Single;
begin
  if y1 > y2 then
  begin
    temp := y1;
    y1 := y2;
    y2 := temp;
  end;
  if x1 > x2 then
  begin
    temp := x1;
    x1 := x2;
    x2 := temp;
  end;

  if rx < 0 then
    rx := -rx;
  if ry < 0 then
    ry := -ry;
  if 2*rx > x2-x1 then rx := (x2-x1)/2;
  if 2*ry > y2-y1 then ry := (y2-y1)/2;
  rdiff := w*(sqrt(2)-1);
  FOuterBorder := TFillRoundRectangleInfo.Create(x1-w/2,y1-w/2,x2+w/2,y2+w/2, rx+rdiff, ry+rdiff, options);
  if (abs(x2-x1) > w) and (abs(y2-y1) > w) then
  begin
    if (rx-rdiff <= 0) or (ry-rdiff <= 0) then
      FInnerBorder := TFillRoundRectangleInfo.Create(x1+w/2, y1+w/2, x2-w/2, y2-w/2, 0,0, options)
    else
      FInnerBorder := TFillRoundRectangleInfo.Create(x1+w/2, y1+w/2, x2-w/2, y2-w/2, rx-rdiff, ry-rdiff, options);
    FInnerBorder.WindingFactor := -1;
  end
  else
    FInnerBorder := nil;
end;

function TFillBorderRoundRectInfo.GetBounds: TRect;
begin
  result := FOuterBorder.GetBounds;
end;

function TFillBorderRoundRectInfo.SegmentsCurved: boolean;
begin
  Result:= FOuterBorder.SegmentsCurved;
  if FInnerBorder <> nil then result := result or FInnerBorder.SegmentsCurved;
end;

function TFillBorderRoundRectInfo.NbMaxIntersection: integer;
begin
  Result := 4;
end;

procedure TFillBorderRoundRectInfo.ComputeIntersection(cury: single;
      var inter: ArrayOfTIntersectionInfo; var nbInter: integer);
begin
  FOuterBorder.ComputeIntersection(cury, inter, nbInter);
  if FInnerBorder <> nil then
    FInnerBorder.ComputeIntersection(cury, inter, nbInter);
end;

destructor TFillBorderRoundRectInfo.Destroy;
begin
  FOuterBorder.Free;
  FInnerBorder.Free;
  inherited Destroy;
end;

initialization

  Randomize;

end.

