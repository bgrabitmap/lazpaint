// SPDX-License-Identifier: GPL-3.0-only
unit LCVectorPolyShapes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, LCVectorOriginal, BGRABitmapTypes, BGRALayerOriginal,
  BGRABitmap, BGRATransform, BGRAGradients, BGRAGraphics,
  BGRASVGShapes, BGRASVGType, BGRAUnits, BGRAPath;

type
  TArrowKind = (akNone, akTail, akTip, akNormal, akCut, akFlipped, akFlippedCut,
                akTriangle, akTriangleBack1, akTriangleBack2,
                akHollowTriangle, akHollowTriangleBack1, akHollowTriangleBack2);

const
  errShapeNotHandled = 'Shape not handled';
  ArrowKindToStr: array[TArrowKind] of string =
    ('none', 'tail', 'tip', 'normal', 'cut', 'flipped', 'flipped-cut',
     'triangle', 'triangle-back1', 'triangle-back2',
     'hollow-triangle', 'hollow-triangle-back1', 'hollow-triangle-back2');
  LineCapToStr: array[TPenEndCap] of string =
    ('round','square','flat');

function StrToArrowKind(AStr: string): TArrowKind;
function StrToLineCap(AStr: string): TPenEndCap;

type
  TCustomPolypointShape = class;
  TCustomPolypointPoint = record
    coord: TPointF;
    editorIndex: integer;
    data: cardinal;
  end;

  { TCustomPolypointShapeDiff }

  TCustomPolypointShapeDiff = class(TVectorShapeDiff)
  protected
    FStartPoints: array of TCustomPolypointPoint;
    FStartClosed: boolean;
    FStartArrowStartKind,FStartArrowEndKind: TArrowKind;
    FStartArrowSize: TPointF;
    FStartLineCap: TPenEndCap;
    FEndPoints: array of TCustomPolypointPoint;
    FEndClosed: boolean;
    FEndArrowStartKind,FEndArrowEndKind: TArrowKind;
    FEndArrowSize: TPointF;
    FEndLineCap: TPenEndCap;
  public
    constructor Create(AStartShape: TVectorShape); override;
    procedure ComputeDiff(AEndShape: TVectorShape); override;
    procedure Apply(AStartShape: TVectorShape); override;
    procedure Unapply(AEndShape: TVectorShape); override;
    procedure Append(ADiff: TVectorShapeDiff); override;
    function IsIdentity: boolean; override;
  end;

  { TCustomPolypointShape }

  TCustomPolypointShape = class(TVectorShape)
  private
    FClosed: boolean;
    function GetHoverPoint: integer;
    function GetLineCap: TPenEndCap;
    function GetPoint(AIndex: integer): TPointF;
    function GetPointCount: integer;
    function GetValidatedPointCount: integer;
    procedure SetArrowEndKind(AValue: TArrowKind);
    procedure SetArrowSize(AValue: TPointF);
    procedure SetArrowStartKind(AValue: TArrowKind);
    procedure SetCenterPoint(AValue: TPointF);
    procedure SetHoverCenter(AValue: boolean);
    procedure SetHoverPoint(AValue: integer);
    procedure SetLineCap(AValue: TPenEndCap);
    procedure SetPoint(AIndex: integer; AValue: TPointF);
  protected
    FPoints: array of TCustomPolypointPoint;
    FCenterPoint: TPointF;
    FCenterPointEditorIndex: integer;
    FCurPoint: integer;
    FAddingPoint, FAltPressed: boolean;
    FMousePos: TPointF;
    FHoverPoint: integer;
    FHoverCenter: boolean;
    FArrowStartKind,FArrowEndKind: TArrowKind;
    FArrowSize: TPointF;
    FViewMatrix, FViewMatrixInverse, FGridMatrix: TAffineMatrix;
    procedure OnMovePoint({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF; {%H-}AShift: TShiftState);
    procedure OnMoveCenterPoint({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF; {%H-}AShift: TShiftState);
    procedure OnStartMove({%H-}ASender: TObject; APointIndex: integer; {%H-}AShift: TShiftState);
    function GetCurve(AMatrix: TAffineMatrix): ArrayOfTPointF; virtual;
    function GetPath(AMatrix: TAffineMatrix): TBGRAPath; virtual; overload;
    function GetPath(const APoints: array of TPointF): TBGRAPath; overload;
    procedure SetUsermode(AValue: TVectorShapeUsermode); override;
    function GetClosed: boolean; virtual;
    procedure SetClosed(AValue: boolean); virtual;
    function PointsEqual(const APoint1, APoint2: TPointF): boolean;
    procedure OnHoverPoint({%H-}ASender: TObject; APointIndex: integer); virtual;
    procedure OnClickPoint({%H-}ASender: TObject; APointIndex: integer; {%H-}AShift: TShiftState); virtual;
    procedure DoClickPoint({%H-}APointIndex: integer; {%H-}AShift: TShiftState); virtual;
    function CanMovePoints: boolean; virtual;
    procedure InsertPointAuto(AShift: TShiftState);
    function ComputeStroke(APoints: ArrayOfTPointF; AClosed: boolean;
      AStrokeMatrix: TAffineMatrix): ArrayOfTPointF; override;
    function GetLoopStartIndex: integer;
    function GetLoopPointCount: integer;
    function GetIsFollowingMouse: boolean; override;
  public
    constructor Create(AContainer: TVectorOriginal); override;
    procedure Clear;
    function AddPoint(const APoint: TPointF): integer; virtual;
    function RemovePoint(AIndex: integer): boolean;
    procedure RemovePointRange(AFromIndex, AToIndexPlus1: integer);
    procedure InsertPoint(AIndex: integer; APoint: TPointF);
    function GetPointBounds(AMatrix: TAffineMatrix): TRectF;
    procedure MouseMove({%H-}Shift: TShiftState; X, Y: single; var {%H-}ACursor: TOriginalEditorCursor; var AHandled: boolean); override;
    procedure MouseDown(RightButton: boolean; {%H-}ClickCount: integer; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: single; var {%H-}ACursor: TOriginalEditorCursor; var AHandled: boolean); override;
    procedure KeyDown({%H-}Shift: TShiftState; Key: TSpecialKey; var AHandled: boolean); override;
    procedure KeyUp(Shift: TShiftState; Key: TSpecialKey; var AHandled: boolean); override;
    procedure QuickDefine(constref APoint1,APoint2: TPointF); override;
    procedure LoadFromStorage(AStorage: TBGRACustomOriginalStorage); override;
    procedure SaveToStorage(AStorage: TBGRACustomOriginalStorage); override;
    procedure ConfigureCustomEditor(AEditor: TBGRAOriginalEditor); override;
    procedure TransformFrame(const AMatrix: TAffineMatrix); override;
    class function Usermodes: TVectorShapeUsermodes; override;
    class function DefaultArrowSize: TPointF;
    property Points[AIndex:integer]: TPointF read GetPoint write SetPoint;
    property PointCount: integer read GetPointCount;
    property ValidatedPointCount: integer read GetValidatedPointCount;
    property Closed: boolean read GetClosed write SetClosed;
    property HoverPoint: integer read GetHoverPoint write SetHoverPoint;
    property HoverCenter: boolean read FHoverCenter write SetHoverCenter;
    property ArrowStartKind: TArrowKind read FArrowStartKind write SetArrowStartKind;
    property ArrowEndKind: TArrowKind read FArrowEndKind write SetArrowEndKind;
    property ArrowSize: TPointF read FArrowSize write SetArrowSize;
    property LineCap: TPenEndCap read GetLineCap write SetLineCap;
    property Center: TPointF read FCenterPoint write SetCenterPoint;
  end;

  { TPolylineShape }

  TPolylineShape = class(TCustomPolypointShape)
  public
    class function Fields: TVectorShapeFields; override;
    procedure Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix; ADraft: boolean); overload; override;
    function AppendToSVG(AContent: TSVGContent; ADefs: TSVGDefine): TSVGElement; override;
    function GetRenderBounds({%H-}ADestRect: TRect; AMatrix: TAffineMatrix; AOptions: TRenderBoundsOptions = []): TRectF; override;
    function PointInShape(APoint: TPointF): boolean; overload; override;
    function PointInShape(APoint: TPointF; ARadius: single): boolean; overload; override;
    function PointInBack(APoint: TPointF): boolean; overload; override;
    function PointInPen(APoint: TPointF): boolean; overload; override;
    function GetIsSlow(const {%H-}AMatrix: TAffineMatrix): boolean; override;
    class function StorageClassName: RawByteString; override;
  end;

  TCurveShape = class;

  { TCurveShapeDiff }

  TCurveShapeDiff = class(TVectorShapeDiff)
  protected
    FStartCosineAngle: single;
    FStartSplineStyle: TSplineStyle;
    FEndCosineAngle: single;
    FEndSplineStyle: TSplineStyle;
  public
    constructor Create(AStartShape: TVectorShape); override;
    procedure ComputeDiff(AEndShape: TVectorShape); override;
    procedure Apply(AStartShape: TVectorShape); override;
    procedure Unapply(AEndShape: TVectorShape); override;
    procedure Append(ADiff: TVectorShapeDiff); override;
    function IsIdentity: boolean; override;
  end;

  { TCurveShape }

  TCurveShape = class(TPolylineShape)
  private
    FCosineAngle: single;
    FSplineStyle: TSplineStyle;
    function GetCurveMode(AIndex: integer): TEasyBezierCurveMode;
    procedure SetCosineAngle(AValue: single);
    procedure SetCurveMode(AIndex: integer; AValue: TEasyBezierCurveMode);
    procedure SetSplineStyle(AValue: TSplineStyle);
  protected
    function GetCurve(AMatrix: TAffineMatrix): ArrayOfTPointF; override;
    function GetPath(AMatrix: TAffineMatrix): TBGRAPath; override;
    function CanMovePoints: boolean; override;
    procedure DoClickPoint(APointIndex: integer; {%H-}AShift: TShiftState); override;
  public
    class function Usermodes: TVectorShapeUsermodes; override;
    constructor Create(AContainer: TVectorOriginal); override;
    constructor CreateFrom(AContainer: TVectorOriginal; AShape: TVectorShape);
    class function CanCreateFrom(AShape: TVectorShape): boolean;
    function AddPoint(const APoint: TPointF): integer; overload; override;
    function AddPoint(const APoint: TPointF; AMode: TEasyBezierCurveMode): integer; overload;
    procedure KeyPress(UTF8Key: string; var AHandled: boolean); override;
    procedure LoadFromStorage(AStorage: TBGRACustomOriginalStorage); override;
    procedure SaveToStorage(AStorage: TBGRACustomOriginalStorage); override;
    class function StorageClassName: RawByteString; override;
    property SplineStyle: TSplineStyle read FSplineStyle write SetSplineStyle;
    property CurveMode[AIndex: integer]: TEasyBezierCurveMode read GetCurveMode write SetCurveMode;
    property CosineAngle: single read FCosineAngle write SetCosineAngle;
  end;

procedure ApplyArrowStyle(AArrow: TBGRACustomArrow; AStart: boolean; AKind: TArrowKind; ASize: TPointF);

implementation

uses BGRAPen, BGRAFillInfo, math, LCVectorialFill,
  BGRAArrow, LCVectorRectShapes, LCResourceString;

function StrToArrowKind(AStr: string): TArrowKind;
var
  ak: TArrowKind;
begin
  for ak := low(TArrowKind) to high(TArrowKind) do
    if CompareText(AStr, ArrowKindToStr[ak])=0 then exit(ak);
  result := akNone;
end;

function StrToLineCap(AStr: string): TPenEndCap;
var
  ec: TPenEndCap;
begin
  for ec := low(TPenEndCap) to high(TPenEndCap) do
    if CompareText(AStr, LineCapToStr[ec])=0 then exit(ec);
  result := pecRound;
end;

procedure ApplyArrowStyle(AArrow: TBGRACustomArrow; AStart: boolean; AKind: TArrowKind; ASize: TPointF);
var backOfs: single;
begin
  backOfs := 0;
  if (ASize.x = 0) or (ASize.y = 0) then AKind := akNone;
  if AKind in[akTriangleBack1,akHollowTriangleBack1] then backOfs := 0.25;
  if AKind in[akTriangleBack2,akHollowTriangleBack2] then backOfs := 0.50;
  case AKind of
  akTail: if AStart then AArrow.StartAsTail else AArrow.EndAsTail;
  akTip: if AStart then AArrow.StartAsTriangle else AArrow.EndAsTriangle;
  akNormal,akCut,akFlipped,akFlippedCut:
    if AStart then AArrow.StartAsClassic(AKind in[akFlipped,akFlippedCut], AKind in[akCut,akFlippedCut])
    else AArrow.EndAsClassic(AKind in[akFlipped,akFlippedCut], AKind in[akCut,akFlippedCut]);
  akTriangle,akTriangleBack1,akTriangleBack2:
    if AStart then AArrow.StartAsTriangle(backOfs) else AArrow.EndAsTriangle(backOfs);
  akHollowTriangle,akHollowTriangleBack1,akHollowTriangleBack2:
    if AStart then AArrow.StartAsTriangle(backOfs,False,True) else AArrow.EndAsTriangle(backOfs,False,True);
  else if AStart then AArrow.StartAsNone else AArrow.EndAsNone;
  end;
  if (AKind = akTip) and not ((ASize.x = 0) or (ASize.y = 0)) then
    ASize := ASize*(0.5/ASize.y);
  if AStart then AArrow.StartSize := ASize else AArrow.EndSize := ASize;
end;

procedure IncludePointF(var ARectF: TRectF; APointF: TPointF);
begin
  if APointF.x < ARectF.Left then ARectF.Left := APointF.x;
  if APointF.x > ARectF.Right then ARectF.Right := APointF.x;
  if APointF.y < ARectF.Top then ARectF.Top := APointF.y;
  if APointF.y > ARectF.Bottom then ARectF.Bottom := APointF.y;
end;

function GetPointsBoundsF(const APoints: array of TPointF): TRectF;
var
  i: Integer;
  firstPoint: Boolean;
begin
  result:= EmptyRectF;
  firstPoint := true;
  for i:= 0 to high(APoints) do
    if not isEmptyPointF(APoints[i]) then
    begin
      if firstPoint then
      begin
        result.TopLeft := APoints[i];
        result.BottomRight := APoints[i];
        firstPoint := false;
      end else
        IncludePointF(result, APoints[i]);
    end;
end;

{ TCurveShapeDiff }

constructor TCurveShapeDiff.Create(AStartShape: TVectorShape);
begin
  with (AStartShape as TCurveShape) do
  begin
    FStartCosineAngle:= FCosineAngle;
    FStartSplineStyle:= FSplineStyle;
  end;
end;

procedure TCurveShapeDiff.ComputeDiff(AEndShape: TVectorShape);
begin
  with (AEndShape as TCurveShape) do
  begin
    FEndCosineAngle:= FCosineAngle;
    FEndSplineStyle:= FSplineStyle;
  end;
end;

procedure TCurveShapeDiff.Apply(AStartShape: TVectorShape);
begin
  with (AStartShape as TCurveShape) do
  begin
    BeginUpdate;
    FCosineAngle := FEndCosineAngle;
    FSplineStyle := FEndSplineStyle;
    EndUpdate;
  end;
end;

procedure TCurveShapeDiff.Unapply(AEndShape: TVectorShape);
begin
  with (AEndShape as TCurveShape) do
  begin
    BeginUpdate;
    FCosineAngle := FStartCosineAngle;
    FSplineStyle := FStartSplineStyle;
    EndUpdate;
  end;
end;

procedure TCurveShapeDiff.Append(ADiff: TVectorShapeDiff);
var
  next: TCurveShapeDiff;
begin
  next := ADiff as TCurveShapeDiff;
  FEndCosineAngle:= next.FEndCosineAngle;
  FEndSplineStyle:= next.FEndSplineStyle;
end;

function TCurveShapeDiff.IsIdentity: boolean;
begin
  result := (FStartCosineAngle = FEndCosineAngle) and
    (FStartSplineStyle = FEndSplineStyle);
end;

{ TCustomPolypointShapeDiff }

constructor TCustomPolypointShapeDiff.Create(AStartShape: TVectorShape);
var
  i: Integer;
begin
  with (AStartShape as TCustomPolypointShape) do
  begin
    setlength(FStartPoints, length(FPoints));
    for i := 0 to high(FPoints) do FStartPoints[i] := FPoints[i];
    FStartClosed:= FClosed;
    FStartArrowStartKind := FArrowStartKind;
    FStartArrowEndKind:= FArrowEndKind;
    FStartArrowSize:= FArrowSize;
    FStartLineCap:= Stroker.LineCap;
  end;
end;

procedure TCustomPolypointShapeDiff.ComputeDiff(AEndShape: TVectorShape);
var
  i: Integer;
begin
  with (AEndShape as TCustomPolypointShape) do
  begin
    setlength(FEndPoints, length(FPoints));
    for i := 0 to high(FPoints) do FEndPoints[i] := FPoints[i];
    FEndClosed:= FClosed;
    FEndArrowStartKind := FArrowStartKind;
    FEndArrowEndKind:= FArrowEndKind;
    FEndArrowSize:= FArrowSize;
    FEndLineCap:= Stroker.LineCap;
  end;
end;

procedure TCustomPolypointShapeDiff.Apply(AStartShape: TVectorShape);
var
  i: Integer;
begin
  with (AStartShape as TCustomPolypointShape) do
  begin
    BeginUpdate;
    setlength(FPoints, length(FEndPoints));
    for i := 0 to high(FPoints) do FPoints[i] := FEndPoints[i];
    FClosed := FEndClosed;
    FArrowStartKind := FEndArrowStartKind;
    FArrowEndKind := FEndArrowEndKind;
    FArrowSize := FEndArrowSize;
    Stroker.LineCap:= FEndLineCap;
    EndUpdate;
  end;
end;

procedure TCustomPolypointShapeDiff.Unapply(AEndShape: TVectorShape);
var
  i: Integer;
begin
  with (AEndShape as TCustomPolypointShape) do
  begin
    BeginUpdate;
    setlength(FPoints, length(FStartPoints));
    for i := 0 to high(FPoints) do FPoints[i] := FStartPoints[i];
    FClosed := FStartClosed;
    FArrowStartKind := FStartArrowStartKind;
    FArrowEndKind := FStartArrowEndKind;
    FArrowSize := FStartArrowSize;
    Stroker.LineCap:= FStartLineCap;
    EndUpdate;
  end;
end;

procedure TCustomPolypointShapeDiff.Append(ADiff: TVectorShapeDiff);
var
  next: TCustomPolypointShapeDiff;
  i: Integer;
begin
  next := ADiff as TCustomPolypointShapeDiff;
  setlength(FEndPoints, length(next.FEndPoints));
  for i := 0 to high(FEndPoints) do FEndPoints[i] := next.FEndPoints[i];
  FEndClosed := next.FEndClosed;
  FEndArrowStartKind := next.FEndArrowStartKind;
  FEndArrowEndKind := next.FEndArrowEndKind;
  FEndArrowSize := next.FEndArrowSize;
  FEndLineCap:= next.FEndLineCap;
end;

function TCustomPolypointShapeDiff.IsIdentity: boolean;
var
  i: Integer;
begin
  result := (length(FStartPoints) = length(FEndPoints)) and
    (FStartClosed = FEndClosed) and
    (FStartArrowStartKind = FEndArrowStartKind) and
    (FStartArrowEndKind = FEndArrowEndKind) and
    (FStartArrowSize = FEndArrowSize) and
    (FStartLineCap = FEndLineCap);
  if result then
  begin
    for i := 0 to high(FStartPoints) do
      if (FStartPoints[i].coord<>FEndPoints[i].coord) or
         (FStartPoints[i].data<>FEndPoints[i].data) then
      begin
        result := false;
        break;
      end;
  end;
end;

{ TCustomPolypointShape }

function TCustomPolypointShape.GetClosed: boolean;
begin
  result := FClosed;
end;

function TCustomPolypointShape.GetPoint(AIndex: integer): TPointF;
begin
  if (AIndex < 0) or (AIndex >= length(FPoints)) then
    raise ERangeError.Create(rsIndexOutOfBounds);
  result := FPoints[AIndex].coord;
end;

function TCustomPolypointShape.GetLineCap: TPenEndCap;
begin
  result := Stroker.LineCap;
end;

function TCustomPolypointShape.GetHoverPoint: integer;
begin
  if (FHoverPoint >= 0) and (FHoverPoint < PointCount) and
     not Points[FHoverPoint].IsEmpty then
       result := FHoverPoint else result := -1;
end;

function TCustomPolypointShape.GetPointCount: integer;
begin
  result:= length(FPoints);
end;

function TCustomPolypointShape.GetValidatedPointCount: integer;
begin
  if (PointCount > 1) and FAddingPoint then
    result := PointCount - 1
  else
    result := PointCount;
end;

procedure TCustomPolypointShape.SetArrowEndKind(AValue: TArrowKind);
begin
  if FArrowEndKind=AValue then Exit;
  BeginUpdate(TCustomPolypointShapeDiff);
  FArrowEndKind:=AValue;
  EndUpdate;
end;

procedure TCustomPolypointShape.SetArrowSize(AValue: TPointF);
begin
  if FArrowSize=AValue then Exit;
  BeginUpdate(TCustomPolypointShapeDiff);
  FArrowSize:=AValue;
  EndUpdate;
end;

procedure TCustomPolypointShape.SetArrowStartKind(AValue: TArrowKind);
begin
  if FArrowStartKind=AValue then Exit;
  BeginUpdate(TCustomPolypointShapeDiff);
  FArrowStartKind:=AValue;
  EndUpdate;
end;

procedure TCustomPolypointShape.SetCenterPoint(AValue: TPointF);
var
  i: Integer;
  delta: TPointF;
begin
  if FCenterPoint=AValue then Exit;

  BeginUpdate(TCustomPolypointShapeDiff);
  delta := AValue - FCenterPoint;
  for i := 0 to PointCount-1 do
    Points[i] := Points[i]+delta;
  if vsfBackFill in Fields then
    BackFill.Transform(AffineMatrixTranslation(delta.x, delta.y));
  if vsfPenFill in Fields then
    PenFill.Transform(AffineMatrixTranslation(delta.x, delta.y));
  FCenterPoint:=AValue;
  EndUpdate;
end;

procedure TCustomPolypointShape.SetHoverCenter(AValue: boolean);
begin
  if FHoverCenter=AValue then Exit;
  BeginEditingUpdate;
  if AValue then FHoverPoint := -1;
  FHoverCenter:=AValue;
  EndEditingUpdate;
end;

procedure TCustomPolypointShape.SetHoverPoint(AValue: integer);
begin
  if (AValue < 0) or (AValue >= PointCount) or
     Points[AValue].IsEmpty then AValue := -1;
  if AValue <> FHoverPoint then
  begin
    BeginEditingUpdate;
    FHoverPoint := AValue;
    if AValue <> -1 then FHoverCenter:= false;
    EndEditingUpdate;
  end;
end;

procedure TCustomPolypointShape.SetLineCap(AValue: TPenEndCap);
begin
  if Stroker.LineCap=AValue then Exit;
  BeginUpdate(TCustomPolypointShapeDiff);
  Stroker.LineCap:=AValue;
  EndUpdate;
end;

procedure TCustomPolypointShape.SetClosed(AValue: boolean);
begin
  if AValue = FClosed then exit;
  BeginUpdate(TCustomPolypointShapeDiff);
  FClosed := AValue;
  EndUpdate;
end;

procedure TCustomPolypointShape.SetPoint(AIndex: integer; AValue: TPointF);
begin
  if (AIndex < 0) or (AIndex > length(FPoints)) then
    raise ERangeError.Create(rsIndexOutOfBounds);
  BeginUpdate(TCustomPolypointShapeDiff);
  if AIndex = length(FPoints) then
  begin
    setlength(FPoints, length(FPoints)+1);
    FPoints[AIndex].coord := AValue;
    FPoints[AIndex].editorIndex := -1;
    FPoints[AIndex].data := 0;
  end
  else
    FPoints[AIndex].coord := AValue;
  EndUpdate;
end;

procedure TCustomPolypointShape.OnMovePoint(ASender: TObject; APrevCoord,
  ANewCoord: TPointF; AShift: TShiftState);
begin
  if FCurPoint = -1 then exit;
  Points[FCurPoint] := ANewCoord;
end;

procedure TCustomPolypointShape.OnMoveCenterPoint(ASender: TObject; APrevCoord,
  ANewCoord: TPointF; AShift: TShiftState);
begin
  Center := ANewCoord;
end;

procedure TCustomPolypointShape.OnStartMove(ASender: TObject; APointIndex: integer;
  AShift: TShiftState);
var
  i: Integer;
begin
  FCurPoint:= -1;
  for i:= 0 to PointCount-1 do
    if FPoints[i].editorIndex = APointIndex then
    begin
      FCurPoint:= i;
      break;
    end;
end;

function TCustomPolypointShape.GetCurve(AMatrix: TAffineMatrix): ArrayOfTPointF;
var
  i: Integer;
  m: TAffineMatrix;
begin
  setlength(result, PointCount);
  m:= MatrixForPixelCentered(AMatrix);
  for i := 0 to PointCount-1 do
    result[i] := m*Points[i];
end;

function TCustomPolypointShape.GetPath(const APoints: array of TPointF): TBGRAPath;
var p: TPointF;
  subPoly: boolean;
begin
  result := TBGRAPath.Create;
  subPoly := true;
  for p in APoints do
  begin
    if isEmptyPointF(p) then
    begin
      if not result.IsEmpty and Closed then result.closePath;
      subPoly := true;
    end else
    begin
      if subPoly then
      begin
        result.moveTo(p);
        subPoly := false;
      end
      else
        result.lineTo(p);
    end;
  end;
  if not result.IsEmpty and Closed then result.closePath;
end;

function TCustomPolypointShape.GetPath(AMatrix: TAffineMatrix): TBGRAPath;
var
  pts: array of TPointF;
begin
  pts := GetCurve(AMatrix);
  result := GetPath(pts);
end;

class function TCustomPolypointShape.Usermodes: TVectorShapeUsermodes;
begin
  Result:= inherited Usermodes + [vsuCreate];
end;

class function TCustomPolypointShape.DefaultArrowSize: TPointF;
begin
  result := PointF(2,2);
end;

procedure TCustomPolypointShape.SetUsermode(AValue: TVectorShapeUsermode);
var
  add: Boolean;
begin
  add := AValue = vsuCreate;
  if add and (PointCount = 0) then exit;
  if FAddingPoint and not add then
  begin
    if (PointCount>1) and PointsEqual(Points[PointCount-1],Points[PointCount-2]) then
      RemovePoint(PointCount-1);
    FAddingPoint:= add;
  end else
  if not FAddingPoint and add then
  begin
    if not isEmptyPointF(FMousePos) then
      AddPoint(FMousePos)
    else
      AddPoint(Points[PointCount-1]);
    FAddingPoint:= add;
  end;
  inherited SetUsermode(AValue);
end;

function TCustomPolypointShape.PointsEqual(const APoint1, APoint2: TPointF
  ): boolean;
begin
  if isEmptyPointF(APoint1) then
    exit(isEmptyPointF(APoint2))
  else
  if isEmptyPointF(APoint2) then exit(false)
  else
    exit((APoint1.x = APoint2.x) and (APoint1.y = APoint2.y));
end;

procedure TCustomPolypointShape.OnHoverPoint(ASender: TObject;
  APointIndex: integer);
var
  i, newHoverPoint: Integer;
begin
  if APointIndex = FCenterPointEditorIndex then
  begin
    HoverCenter := true;
    exit;
  end;
  newHoverPoint:= -1;
  if APointIndex <> -1 then
  begin
    for i:= 0 to PointCount-1 do
      if FPoints[i].editorIndex = APointIndex then
      begin
        newHoverPoint:= i;
        break;
      end;
  end;
  HoverPoint := newHoverPoint;
  HoverCenter:= false;
end;

procedure TCustomPolypointShape.OnClickPoint(ASender: TObject;
  APointIndex: integer; AShift: TShiftState);
var
  i: Integer;
begin
  if APointIndex <> -1 then
  begin
    for i:= 0 to PointCount-1 do
      if FPoints[i].editorIndex = APointIndex then
      begin
        DoClickPoint(i, AShift);
        break;
      end;
  end;
end;

procedure TCustomPolypointShape.DoClickPoint(APointIndex: integer;
  AShift: TShiftState);
var
  nb: Integer;
begin
  if FAddingPoint and ((APointIndex = GetLoopStartIndex) or
     ((APointIndex = PointCount-2) and (ssRight in AShift))) then
  begin
    nb := GetLoopPointCount;
    if nb > 2 then
    begin
      BeginUpdate;
      RemovePoint(PointCount-1);
      if APointIndex < PointCount-2 then Closed := true;
      EndUpdate;
      UserMode := vsuEdit;
    end else
    begin
      if GetLoopStartIndex = 0 then
        Remove
      else
      begin
        BeginUpdate;
        while nb > 0 do
        begin
          RemovePoint(PointCount-1);
          dec(nb);
        end;
        RemovePoint(PointCount-1); //remove separator
      end;
    end;
  end;
end;

function TCustomPolypointShape.CanMovePoints: boolean;
begin
  result := true;
end;

procedure TCustomPolypointShape.InsertPointAuto(AShift: TShiftState);
var
  i,j, loopStart: Integer;
  bestSegmentIndex,bestPointIndex: integer;
  bestSegmentDist,bestPointDist, segmentLen, segmentPos: single;
  u, n, bestProjection: TPointF;
  segmentDist: single;
  isLooping: Boolean;
begin
  if isEmptyPointF(FMousePos) then exit;

  for i := 0 to PointCount-1 do
    if (Points[i] = FMousePos) and not (FAddingPoint and (i = PointCount-1)) then exit;

  bestSegmentIndex := -1;
  bestSegmentDist := MaxSingle;
  bestProjection := EmptyPointF;
  loopStart := 0;
  for i := 0 to PointCount-1 do
  if FAddingPoint and (i >= PointCount-2) then break else
  begin
    if IsEmptyPointF(Points[i]) then
    begin
      loopStart := i+1;
      continue;
    end;
    isLooping := (i = PointCount-1) or IsEmptyPointF(Points[i+1]);
    if isLooping and not Closed then break;
    if isLooping then
      j := loopStart
      else j := i+1;
    u := Points[j] - Points[i];
    segmentLen := VectLen(u);
    if segmentLen > 0 then
    begin
      u *= 1/segmentLen;
      segmentPos := (FMousePos-Points[i])*u;
      if (segmentPos > 0) and (segmentPos< segmentLen) then
      begin
        n := PointF(u.y,-u.x);
        segmentDist := abs((FMousePos-Points[i])*n);
        if segmentDist <= bestSegmentDist then
        begin
          bestSegmentDist := segmentDist;
          bestSegmentIndex := i;
          bestProjection := Points[i]+segmentPos*u;
        end;
      end;
    end;
  end;


  bestPointIndex := -1;
  bestPointDist := MaxSingle;
  if not FAddingPoint then
    for i := 0 to PointCount-1 do
      if ((i = 0) or isEmptyPointF(Points[i-1])) and
         ((i = PointCount-1) or isEmptyPointF(Points[i+1])) then
      begin
        segmentDist := VectLen(FMousePos-Points[i]);
        if segmentDist < bestPointDist then
        begin
          bestPointDist := segmentDist;
          bestPointIndex := i;
        end;
      end;

  if (bestPointIndex <> -1) and ((bestSegmentIndex = -1) or (bestPointDist < bestSegmentDist)) then
  begin
    InsertPoint(bestPointIndex+1, FMousePos);
    HoverPoint := bestPointIndex+1;
  end else
  if bestSegmentIndex <> -1 then
  begin
    if ssShift in AShift then
      InsertPoint(bestSegmentIndex+1, bestProjection)
    else
      InsertPoint(bestSegmentIndex+1, FMousePos);
    HoverPoint:= bestSegmentIndex+1;
  end;
end;

function TCustomPolypointShape.ComputeStroke(APoints: ArrayOfTPointF;
  AClosed: boolean; AStrokeMatrix: TAffineMatrix): ArrayOfTPointF;
begin
  if Stroker.Arrow = nil then
  begin
    Stroker.Arrow := TBGRAArrow.Create;
    Stroker.ArrowOwned:= true;
  end;
  Stroker.Arrow.LineCap:= LineCap;
  ApplyArrowStyle(Stroker.Arrow, true, ArrowStartKind, ArrowSize);
  ApplyArrowStyle(Stroker.Arrow, false, ArrowEndKind, ArrowSize);
  Result:=inherited ComputeStroke(APoints, AClosed, AStrokeMatrix);
  Stroker.Arrow.StartAsNone;
  Stroker.Arrow.EndAsNone;
end;

function TCustomPolypointShape.GetLoopStartIndex: integer;
var
  i: Integer;
begin
  for i := PointCount-1 downto 0 do
    if isEmptyPointF(Points[i]) then exit(i+1);
  exit(0);
end;

function TCustomPolypointShape.GetLoopPointCount: integer;
begin
  result := PointCount-GetLoopStartIndex;
end;

function TCustomPolypointShape.GetIsFollowingMouse: boolean;
begin
  Result:= Usermode = vsuCreate;
end;

constructor TCustomPolypointShape.Create(AContainer: TVectorOriginal);
begin
  inherited Create(AContainer);
  FMousePos := EmptyPointF;
  FClosed:= false;
  FHoverPoint:= -1;
  FCenterPoint := EmptyPointF;
end;

procedure TCustomPolypointShape.Clear;
begin
  RemovePointRange(0, PointCount);
end;

function TCustomPolypointShape.AddPoint(const APoint: TPointF): integer;
begin
  result := PointCount;
  Points[result] := APoint;
end;

function TCustomPolypointShape.RemovePoint(AIndex: integer): boolean;
begin
  if (AIndex < 0) or (AIndex >= PointCount) then exit(false);
  RemovePointRange(AIndex,AIndex+1);
  result := true;
end;

procedure TCustomPolypointShape.RemovePointRange(AFromIndex, AToIndexPlus1: integer);
var
  i, delCount: Integer;
begin
  if AFromIndex < 0 then AFromIndex:= 0;
  if AToIndexPlus1 > PointCount then AToIndexPlus1:= PointCount;
  if AFromIndex >= AToIndexPlus1 then exit;
  BeginUpdate(TCustomPolypointShapeDiff);
  delCount := AToIndexPlus1-AFromIndex;
  for i := AFromIndex to PointCount-DelCount-1 do
    FPoints[i] := FPoints[i+delCount];
  setlength(FPoints, PointCount-delCount);
  if (HoverPoint >= AFromIndex) and (HoverPoint < AToIndexPlus1) then HoverPoint := -1
  else if (HoverPoint <> -1) and (HoverPoint >= AToIndexPlus1) then HoverPoint := HoverPoint - delCount;
  EndUpdate;
end;

procedure TCustomPolypointShape.InsertPoint(AIndex: integer; APoint: TPointF);
var
  i: Integer;
begin
  if (AIndex < 0) or (AIndex > PointCount) then raise exception.Create(rsIndexOutOfBounds);
  BeginUpdate(TCustomPolypointShapeDiff);
  setlength(FPoints, PointCount+1);
  for i := PointCount-1 downto AIndex+1 do
    FPoints[i] := FPoints[i-1];
  FPoints[AIndex].coord := APoint;
  FPoints[AIndex].editorIndex:= -1;
  FPoints[AIndex].data := 0;
  if (HoverPoint <> -1) and (HoverPoint >= AIndex) then HoverPoint := HoverPoint + 1;
  EndUpdate;
end;

function TCustomPolypointShape.GetPointBounds(AMatrix: TAffineMatrix): TRectF;
begin
  result := GetPointsBoundsF(GetCurve(AMatrix));
end;

procedure TCustomPolypointShape.MouseMove(Shift: TShiftState; X, Y: single; var
  ACursor: TOriginalEditorCursor; var AHandled: boolean);
begin
  FMousePos := PointF(X,Y);
  if FAddingPoint then
  begin
    BeginUpdate;
    if (PointCount = 1) and (FMousePos <> Points[PointCount-1]) then
      Points[PointCount] := FMousePos
    else
      Points[PointCount-1] := FMousePos;
    FillFit;
    EndUpdate;
    AHandled:= true;
  end;
end;

procedure TCustomPolypointShape.MouseDown(RightButton: boolean;
  ClickCount: integer; Shift: TShiftState; X, Y: single; var ACursor: TOriginalEditorCursor; var
  AHandled: boolean);
begin
  FMousePos := PointF(X,Y);
  if FAddingPoint then
  begin
    if not RightButton then
    begin
      if (PointCount>1) and not PointsEqual(FMousePos,Points[PointCount-2]) then
      begin
        BeginUpdate;
        Points[PointCount-1] := FMousePos;
        AddPoint(FMousePos);
        EndUpdate;
      end;
    end else
      Usermode := vsuEdit;
    AHandled:= true;
  end else
  begin
    if (ssShift in Shift) and (Usermode = vsuEdit) then
    begin
      BeginUpdate;
      AddPoint(EmptyPointF);
      AddPoint(FMousePos);
      FillFit;
      EndUpdate;
      UserMode := vsuCreate;
      AHandled:= true;
    end;
  end;
end;

procedure TCustomPolypointShape.KeyDown(Shift: TShiftState; Key: TSpecialKey;
  var AHandled: boolean);
var
  nb, idx: Integer;
  dx, dy, d: TPointF;
begin
  if (Key = skDelete) and (FAddingPoint or (HoverPoint <> -1)) then
  begin
    if (HoverPoint <> -1) then
    begin
      BeginUpdate(TCustomPolypointShapeDiff);
      idx := HoverPoint;
      RemovePoint(idx);
      if ((idx = PointCount) or IsEmptyPointF(Points[idx])) and
         ((idx = 0) or IsEmptyPointF(Points[idx-1])) then
      begin
        if idx < PointCount then
          RemovePoint(idx)
        else if idx > 0 then
          RemovePoint(idx-1);
      end;
      EndUpdate;
      if PointCount = 0 then self.Remove;
    end;
    AHandled:= true;
  end else
  if (Key = skBackspace) and FAddingPoint then
  begin
    nb := GetLoopPointCount;
    if nb > 2 then
      RemovePoint(PointCount-2)
    else
    begin
      if GetLoopStartIndex = 0 then self.Remove
      else
      begin
        RemovePointRange(PointCount-3, PointCount);
        Usermode:= vsuEdit;
      end;
    end;
    AHandled:= true;
  end else
  if (Key = skInsert) then
  begin
    InsertPointAuto(Shift);
    AHandled := true;
  end else
  if (Key in [skLeft,skUp,skRight,skDown]) and ((HoverPoint <> -1) or HoverCenter) then
  begin
    if ssCtrl in Shift then
    begin
      dx := PointF(FGridMatrix[1,1], FGridMatrix[2,1]);
      dy := PointF(FGridMatrix[1,2], FGridMatrix[2,2]);
    end else
    begin
      dx := PointF(FViewMatrixInverse[1,1], FViewMatrixInverse[2,1]);
      dy := PointF(FViewMatrixInverse[1,2], FViewMatrixInverse[2,2]);
    end;
    case Key of
    skLeft: d := -dx;
    skRight: d := dx;
    skUp: d := -dy;
    skDown: d := dy;
    else d := PointF(0,0);
    end;
    if HoverCenter then
      Center := Center + d
    else
      Points[HoverPoint] := Points[HoverPoint] + d;
    AHandled := true;
  end else
  if Key = skAlt then
  begin
    BeginUpdate;
    FAltPressed := true;
    EndUpdate;
    AHandled := true;
  end
  else
    inherited KeyDown(Shift, Key, AHandled);
end;

procedure TCustomPolypointShape.KeyUp(Shift: TShiftState; Key: TSpecialKey;
  var AHandled: boolean);
begin
  if Key = skAlt then
  begin
    BeginUpdate;
    FAltPressed := false;
    EndUpdate;
    AHandled := true;
  end
  else inherited KeyUp(Shift, Key, AHandled);
end;

procedure TCustomPolypointShape.QuickDefine(constref APoint1, APoint2: TPointF);
begin
  BeginUpdate(TCustomPolypointShapeDiff);
  FPoints := nil;
  AddPoint(APoint1);
  if not PointsEqual(APoint1,APoint2) then
    AddPoint(APoint2);
  EndUpdate;
  FMousePos := APoint2;
end;

procedure TCustomPolypointShape.LoadFromStorage(AStorage: TBGRACustomOriginalStorage);
var
  x,y: Array of Single;
  i: Integer;
begin
  BeginUpdate;
  inherited LoadFromStorage(AStorage);
  Clear;
  x := AStorage.FloatArray['x'];
  y := AStorage.FloatArray['y'];
  setlength(FPoints, max(length(x),length(y)));
  for i := 0 to high(FPoints) do
  begin
    FPoints[i].coord := PointF(x[i],y[i]);
    FPoints[i].editorIndex := -1;
    FPoints[i].data := 0;
  end;
  FClosed:= AStorage.Bool['closed'];
  if AStorage.HasAttribute('arrow-size') then
    FArrowSize := AStorage.PointF['arrow-size']
  else FArrowSize := DefaultArrowSize;
  FArrowStartKind:= StrToArrowKind(AStorage.RawString['arrow-start-kind']);
  FArrowEndKind:= StrToArrowKind(AStorage.RawString['arrow-end-kind']);
  Stroker.LineCap := StrToLineCap(AStorage.RawString['line-cap']);
  EndUpdate;
end;

procedure TCustomPolypointShape.SaveToStorage(AStorage: TBGRACustomOriginalStorage);
var
  x,y: Array of Single;
  i: Integer;
begin
  inherited SaveToStorage(AStorage);
  setlength({%H-}x, PointCount);
  setlength({%H-}y, PointCount);
  for i:= 0 to PointCount-1 do
  begin
    x[i] := Points[i].x;
    y[i] := Points[i].y;
  end;
  AStorage.FloatArray['x'] := x;
  AStorage.FloatArray['y'] := y;
  AStorage.Bool['closed'] := Closed;
  if ArrowStartKind=akNone then AStorage.RemoveAttribute('arrow-start-kind')
  else AStorage.RawString['arrow-start-kind'] := ArrowKindToStr[ArrowStartKind];
  if ArrowEndKind=akNone then AStorage.RemoveAttribute('arrow-end-kind')
  else AStorage.RawString['arrow-end-kind'] := ArrowKindToStr[ArrowEndKind];
  if (ArrowStartKind=akNone) and (ArrowEndKind=akNone) then AStorage.RemoveAttribute('arrow-size')
  else AStorage.PointF['arrow-size'] := FArrowSize;
  AStorage.RawString['line-cap'] := LineCapToStr[Stroker.LineCap];
end;

procedure TCustomPolypointShape.ConfigureCustomEditor(AEditor: TBGRAOriginalEditor);
var
  i, nbTotal: Integer;
begin
  FViewMatrix := AEditor.Matrix;
  if not IsAffineMatrixInversible(FViewMatrix) then exit;
  FViewMatrixInverse := AffineMatrixInverse(FViewMatrix);
  FGridMatrix := AEditor.GridMatrix;

  AEditor.AddStartMoveHandler(@OnStartMove);
  AEditor.AddClickPointHandler(@OnClickPoint);
  AEditor.AddHoverPointHandler(@OnHoverPoint);

  FCenterPoint := PointF(0,0);
  nbTotal := 0;
  for i:= 0 to PointCount-1 do
    if isEmptyPointF(Points[i]) then
      FPoints[i].editorIndex := -1
    else if (FAddingPoint and (i = PointCount-1) and (GetLoopPointCount > 1)) then
    begin
      FPoints[i].editorIndex := -1;
      FCenterPoint += Points[i];
      inc(nbTotal);
    end
    else
    begin
      if CanMovePoints then
        FPoints[i].editorIndex := AEditor.AddPoint(Points[i], @OnMovePoint, false)
      else
        FPoints[i].editorIndex := AEditor.AddFixedPoint(Points[i], false);
      FCenterPoint += Points[i];
      if i = HoverPoint then
        AEditor.PointHighlighted[FPoints[i].editorIndex] := true;
      inc(nbTotal);
    end;

  if nbTotal > 0 then
    FCenterPoint *= 1/nbTotal
    else FCenterPoint := EmptyPointF;

  if ((FAddingPoint and (nbTotal > 2)) or (not FAddingPoint and (nbTotal > 1)))
     and not FAltPressed then
  begin
    FCenterPointEditorIndex := AEditor.AddPoint(FCenterPoint, @OnMoveCenterPoint, true);
    AEditor.PointHighlighted[FCenterPointEditorIndex] := HoverCenter;
  end else
    FCenterPointEditorIndex := -1;
end;

procedure TCustomPolypointShape.TransformFrame(const AMatrix: TAffineMatrix);
var
  i: Integer;
  m: TAffineMatrix;
begin
  BeginUpdate(TCustomPolypointShapeDiff);
  m := MatrixForPixelCentered(AMatrix);
  for i := 0 to PointCount-1 do
    FPoints[i].coord := m*FPoints[i].coord;
  EndUpdate;
end;

{ TPolylineShape }

class function TPolylineShape.Fields: TVectorShapeFields;
begin
  Result:= [vsfPenFill, vsfPenWidth, vsfPenStyle, vsfJoinStyle, vsfBackFill];
end;

procedure TPolylineShape.Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix;
  ADraft: boolean);
var
  pts: array of TPointF;
  backScan, penScan: TBGRACustomScanner;
begin
  if not GetBackVisible and not GetPenVisible then exit;
  pts := GetCurve(AMatrix);
  if GetBackVisible then
  begin
    if BackFill.FillType = vftSolid then backScan := nil
    else backScan := BackFill.CreateScanner(AMatrix, ADraft);

    if ADraft then
    begin
      if Assigned(backScan) then
        ADest.FillPoly(pts, backScan, dmDrawWithTransparency) else
        ADest.FillPoly(pts, BackFill.SolidColor, dmDrawWithTransparency);
    end
    else
    begin
      if Assigned(backScan) then
        ADest.FillPolyAntialias(pts, backScan) else
        ADest.FillPolyAntialias(pts, BackFill.SolidColor);
    end;

    backScan.Free;
  end;
  if GetPenVisible then
  begin
    if PenFill.FillType = vftSolid then penScan := nil
    else penScan := PenFill.CreateScanner(AMatrix, ADraft);

    pts := ComputeStroke(pts, Closed, AMatrix);
    if ADraft and (PenWidth > 4) then
    begin
      if Assigned(penScan) then
        ADest.FillPoly(pts, penScan, dmDrawWithTransparency) else
        ADest.FillPoly(pts, PenColor, dmDrawWithTransparency);
    end
    else
    begin
      if Assigned(penScan) then
        ADest.FillPolyAntialias(pts, penScan) else
        ADest.FillPolyAntialias(pts, PenColor);
    end;

    penScan.Free;
  end;
end;

function TPolylineShape.AppendToSVG(AContent: TSVGContent; ADefs: TSVGDefine): TSVGElement;
var
  p: TBGRAPath;
begin
  p := GetPath(AffineMatrixIdentity);
  result := AContent.AppendPath(p.SvgString);
  p.Free;
  ApplyStrokeStyleToSVG(result, ADefs);
  if PenVisible then
    result.strokeLineCapLCL := LineCap;
  ApplyFillStyleToSVG(result, ADefs);
end;

function TPolylineShape.GetRenderBounds(ADestRect: TRect; AMatrix: TAffineMatrix; AOptions: TRenderBoundsOptions): TRectF;
var
  pts: ArrayOfTPointF;
  xMargin, yMargin: single;
  fillBounds, penBounds: TRectF;
begin
  if not (GetBackVisible or (rboAssumeBackFill in AOptions)) and not GetPenVisible(rboAssumePenFill in AOptions) then
    result:= EmptyRectF
  else
  begin
    pts := GetCurve(AMatrix);
    if GetPenVisible(rboAssumePenFill in AOptions) then
    begin
      if (JoinStyle = pjsRound) and (ArrowStartKind = akNone) and (ArrowEndKind = akNone) then
      begin
        xMargin := (abs(AMatrix[1,1])+abs(AMatrix[1,2]))*PenWidth*0.5;
        yMargin := (abs(AMatrix[2,1])+abs(AMatrix[2,2]))*PenWidth*0.5;
        if LineCap = pecSquare then
        begin
          xMargin *= sqrt(2);
          yMargin *= sqrt(2);
        end;
        result := GetPointsBoundsF(pts);
        result.Left -= xMargin;
        result.Top -= yMargin;
        result.Right += xMargin;
        result.Bottom += yMargin;
      end else
      begin
        if GetBackVisible or (rboAssumeBackFill in AOptions) then fillBounds := GetPointsBoundsF(pts)
        else fillBounds := EmptyRectF;
        pts := ComputeStroke(pts, Closed, AMatrix);
        penBounds := GetPointsBoundsF(pts);
        result := fillBounds.Union(penBounds, true);
      end;
    end
    else
      result := GetPointsBoundsF(pts);
  end;
  result.Offset(0.5,0.5);
end;

function TPolylineShape.PointInShape(APoint: TPointF): boolean;
var
  pts: ArrayOfTPointF;
begin
  if not GetBackVisible and not GetPenVisible then exit(false);
  pts := GetCurve(AffineMatrixIdentity);
  if GetBackVisible and IsPointInPolygon(pts, APoint, true) then exit(true);
  if GetPenVisible then
  begin
    pts := ComputeStroke(pts, Closed, AffineMatrixIdentity);
    if IsPointInPolygon(pts, APoint, true) then exit(true);
  end;
  result := false;
end;

function TPolylineShape.PointInShape(APoint: TPointF; ARadius: single): boolean;
var
  pts: ArrayOfTPointF;
begin
  if not GetBackVisible and not GetPenVisible then exit(false);
  pts := GetCurve(AffineMatrixIdentity);
  pts := ComputeStrokeEnvelope(pts, Closed, ARadius*2);
  result := IsPointInPolygon(pts, APoint, true);
end;

function TPolylineShape.PointInBack(APoint: TPointF): boolean;
var
  pts: ArrayOfTPointF;
  scan: TBGRACustomScanner;
begin
  if GetBackVisible then
  begin
    pts := GetCurve(AffineMatrixIdentity);
    result := IsPointInPolygon(pts, APoint, true);
    if result and (BackFill.FillType = vftTexture) then
    begin
      scan := BackFill.CreateScanner(AffineMatrixIdentity, false);
      if scan.ScanAt(APoint.X,APoint.Y).alpha = 0 then result := false;
      scan.Free;
    end;
  end else
    result := false;
end;

function TPolylineShape.PointInPen(APoint: TPointF): boolean;
var
  pts: ArrayOfTPointF;
begin
  if GetBackVisible then
  begin
    pts := GetCurve(AffineMatrixIdentity);
    pts := ComputeStroke(pts, Closed, AffineMatrixIdentity);
    result := IsPointInPolygon(pts, APoint, true);
  end else
    result := false;
end;

function TPolylineShape.GetIsSlow(const AMatrix: TAffineMatrix): boolean;
var pts: ArrayOfTPointF;
  i: Integer;
  ptsBounds: TRectF;
  backSurface: Single;
  penLength, zoomFactor, penSurface, totalSurface: single;
begin
  if not GetPenVisible and not GetBackVisible or (PointCount = 0) then exit(false);

  setlength({%H-}pts, PointCount);
  for i := 0 to high(pts) do
    pts[i] := AMatrix * Points[i];

  if GetPenVisible then
  begin
    penLength := 0;
    zoomFactor := max(VectLen(AMatrix[1,1],AMatrix[2,1]), VectLen(AMatrix[1,2],AMatrix[2,2]));
    for i := 0 to high(pts) do
      if (i > 0) then
      begin
        if pts[i-1].IsEmpty then
        begin
          if not pts[i].IsEmpty and (LineCap <> pecFlat) then penLength += penWidth/2*zoomFactor;
        end else
        if pts[i].IsEmpty then
        begin
          if not pts[i-1].IsEmpty and (LineCap <> pecFlat) then penLength += penWidth/2*zoomFactor;
        end else
          penLength += VectLen(pts[i]-pts[i-1]);
      end;
    penSurface := penLength*PenWidth*zoomFactor;
  end else penSurface := 0;

  if GetBackVisible then
  begin
    ptsBounds := GetPointsBoundsF(pts);
    backSurface := ptsBounds.Width*ptsBounds.Height;
  end else
    backSurface := 0;

  if GetPenVisible and GetBackVisible then totalSurface := backSurface+penSurface/2
  else totalSurface := backSurface+penSurface;

  Result:= (PointCount > 40) or
           ((penSurface > 320*240) and PenFill.IsSlow(AMatrix)) or
           ((backSurface > 320*240) and BackFill.IsSlow(AMatrix)) or
           (totalSurface > 640*480);
end;

class function TPolylineShape.StorageClassName: RawByteString;
begin
  result := 'polyline';
end;

{ TCurveShape }

procedure TCurveShape.SetSplineStyle(AValue: TSplineStyle);
begin
  if FSplineStyle=AValue then Exit;
  BeginUpdate(TCurveShapeDiff);
  FSplineStyle:=AValue;
  EndUpdate;
end;

function TCurveShape.GetCurveMode(AIndex: integer): TEasyBezierCurveMode;
begin
  if (AIndex < 0) or (AIndex >= PointCount) then exit(cmCurve);
  result := TEasyBezierCurveMode(FPoints[AIndex].data);
end;

procedure TCurveShape.SetCosineAngle(AValue: single);
begin
  if FCosineAngle=AValue then Exit;
  BeginUpdate(TCurveShapeDiff);
  FCosineAngle:=AValue;
  EndUpdate;
end;

procedure TCurveShape.SetCurveMode(AIndex: integer; AValue: TEasyBezierCurveMode);
begin
  if (AIndex < 0) or (AIndex >= PointCount) then exit;
  if CurveMode[AIndex] = AValue then exit;
  BeginUpdate(TCustomPolypointShapeDiff);
  FPoints[AIndex].data := ord(AValue);
  EndUpdate
end;

function TCurveShape.GetCurve(AMatrix: TAffineMatrix): ArrayOfTPointF;
var
  pts: array of TPointF;
  cm: array of TEasyBezierCurveMode;
  i: Integer;
  eb: TEasyBezierCurve;
begin
  pts := inherited GetCurve(AMatrix);
  if FSplineStyle = ssEasyBezier then
  begin
    setlength({%H-}cm, PointCount);
    for i := 0 to PointCount-1 do
      cm[i] := CurveMode[i];
    eb := EasyBezierCurve(pts, Closed, cm, CosineAngle);
    result := eb.ToPoints;
  end else
  begin
    if Closed then result := ComputeClosedSpline(pts, FSplineStyle)
    else result := ComputeOpenedSpline(pts, FSplineStyle);
  end;
end;

function TCurveShape.GetPath(AMatrix: TAffineMatrix): TBGRAPath;
var
  pts: array of TPointF;
  cm: array of TEasyBezierCurveMode;
  i: Integer;
  eb: TEasyBezierCurve;
begin
  pts := inherited GetCurve(AMatrix);
  if FSplineStyle = ssEasyBezier then
  begin
    setlength({%H-}cm, PointCount);
    for i := 0 to PointCount-1 do
      cm[i] := CurveMode[i];
    eb := EasyBezierCurve(pts, Closed, cm, CosineAngle);
    result := TBGRAPath.Create;
    eb.CopyToPath(result);
  end else
  begin
    if Closed then pts := ComputeClosedSpline(pts, FSplineStyle)
    else pts := ComputeOpenedSpline(pts, FSplineStyle);
    result := GetPath(pts);
  end;
end;

function TCurveShape.CanMovePoints: boolean;
begin
  Result:= Usermode in [vsuCreate,vsuEdit];
end;

procedure TCurveShape.DoClickPoint(APointIndex: integer; AShift: TShiftState);
begin
  case Usermode of
  vsuCurveSetAuto: CurveMode[APointIndex] := cmAuto;
  vsuCurveSetCurve: CurveMode[APointIndex] := cmCurve;
  vsuCurveSetAngle: CurveMode[APointIndex] := cmAngle;
  else
    inherited DoClickPoint(APointIndex, AShift);
  end;
end;

class function TCurveShape.Usermodes: TVectorShapeUsermodes;
begin
  Result:=inherited Usermodes + [vsuCurveSetAuto, vsuCurveSetCurve, vsuCurveSetAngle];
end;

constructor TCurveShape.Create(AContainer: TVectorOriginal);
begin
  inherited Create(AContainer);
  FSplineStyle:= ssEasyBezier;
end;

constructor TCurveShape.CreateFrom(AContainer: TVectorOriginal;
  AShape: TVectorShape);
var
  r: TCustomRectShape;
  u, v: TPointF;
  p: TCustomPolypointShape;
  i: Integer;
  f: TVectorShapeFields;
  sq2m1: single;
begin
  Create(AContainer);
  if AShape is TEllipseShape then
  begin
    r := AShape as TCustomRectShape;
    u := r.XAxis-r.Origin;
    v := r.YAxis-r.Origin;
    sq2m1 := sqrt(2)-1;
    AddPoint(r.Origin-v+u*sq2m1);
    AddPoint(r.Origin-v*sq2m1+u);
    AddPoint(r.Origin+v*sq2m1+u);
    AddPoint(r.Origin+v+u*sq2m1);
    AddPoint(r.Origin+v-u*sq2m1);
    AddPoint(r.Origin+v*sq2m1-u);
    AddPoint(r.Origin-v*sq2m1-u);
    AddPoint(r.Origin-v-u*sq2m1);
    Closed := true;
  end else
  if AShape is TRectShape then
  begin
    r := AShape as TCustomRectShape;
    u := r.XAxis-r.Origin;
    v := r.YAxis-r.Origin;
    AddPoint(r.Origin-v-u, cmAngle);
    AddPoint(r.Origin-v+u, cmAngle);
    AddPoint(r.Origin+v+u, cmAngle);
    AddPoint(r.Origin+v-u, cmAngle);
    Closed := true;
  end else
  if (AShape is TPolylineShape) and not
     (AShape is TCurveShape) then
  begin
    p := AShape as TCustomPolypointShape;
    for i := 0 to p.PointCount-1 do
      AddPoint(p.Points[i], cmAngle);
    Closed := p.Closed;
  end else
    raise exception.Create(errShapeNotHandled);

  f := AShape.Fields;
  if vsfPenFill in f then PenFill.Assign(AShape.PenFill);
  if vsfPenWidth in f then PenWidth := AShape.PenWidth;
  if vsfPenStyle in f then PenStyle := AShape.PenStyle;
  if vsfJoinStyle in f then JoinStyle := AShape.JoinStyle;
  if vsfBackFill in f then BackFill.Assign(AShape.BackFill);
end;

class function TCurveShape.CanCreateFrom(AShape: TVectorShape): boolean;
begin
  result := (AShape is TEllipseShape) or
    (AShape is TRectShape) or
    ((AShape is TPolylineShape) and not
     (AShape is TCurveShape));
end;

function TCurveShape.AddPoint(const APoint: TPointF): integer;
begin
  if (PointCount > 1) and (APoint = Points[PointCount-1]) then
  begin
    BeginUpdate;
    CurveMode[PointCount-1] := CurveMode[PointCount-2];
    Result:=inherited AddPoint(APoint);
    EndUpdate;
  end
  else Result:=inherited AddPoint(APoint);
end;

function TCurveShape.AddPoint(const APoint: TPointF; AMode: TEasyBezierCurveMode): integer;
begin
  result := inherited AddPoint(APoint);
  CurveMode[result] := AMode;
end;

procedure TCurveShape.KeyPress(UTF8Key: string; var AHandled: boolean);
var
  targetPoint: Integer;
begin
  if HoverPoint<>-1 then
    targetPoint := HoverPoint
  else if FAddingPoint and (PointCount > 1) then
    targetPoint := PointCount-2
  else
    targetPoint := -1;
  if (targetPoint >= 0) and (targetPoint < PointCount) then
  begin
    if (UTF8Key = 'A') or (UTF8Key = 'a') then
    begin
      CurveMode[targetPoint] := cmAuto;
      AHandled := true;
    end else
    if (UTF8Key = 'S') or (UTF8Key = 's') then
    begin
      CurveMode[targetPoint] := cmCurve;
      AHandled:= true;
    end else
    if (UTF8Key = 'X') or (UTF8Key = 'x') then
    begin
      CurveMode[targetPoint] := cmAngle;
      AHandled:= true;
    end;
  end;
  if not AHandled then
    inherited KeyPress(UTF8Key, AHandled);
end;

procedure TCurveShape.LoadFromStorage(AStorage: TBGRACustomOriginalStorage);
var
  i: Integer;
  cm: array of Single;
begin
  BeginUpdate;
  inherited LoadFromStorage(AStorage);
  case AStorage.RawString['spline-style'] of
  'inside': SplineStyle := ssInside;
  'inside+ends': SplineStyle := ssInsideWithEnds;
  'crossing': SplineStyle := ssCrossing;
  'crossing+ends': SplineStyle := ssCrossingWithEnds;
  'outside': SplineStyle := ssOutside;
  'round-outside': SplineStyle := ssRoundOutside;
  'vertex-to-side': SplineStyle := ssVertexToSide;
  else
    {'easy-bezier'} SplineStyle := ssEasyBezier;
  end;
  if SplineStyle = ssEasyBezier then
  begin
    cm := AStorage.FloatArray['curve-mode'];
    for i := 0 to min(high(cm),PointCount-1) do
      case round(cm[i]) of
      1: CurveMode[i] := cmCurve;
      2: CurveMode[i] := cmAngle;
      end;
    if length(cm) < PointCount then
      for i:= length(cm) to PointCount-1 do
        CurveMode[i] := cmCurve;
  end;
  CosineAngle := AStorage.FloatDef['cosine-angle', EasyBezierDefaultMinimumDotProduct];
  EndUpdate;
end;

procedure TCurveShape.SaveToStorage(AStorage: TBGRACustomOriginalStorage);
var s: string;
  cm: array of single;
  i: Integer;
begin
  inherited SaveToStorage(AStorage);
  case SplineStyle of
    ssInside: s := 'inside';
    ssInsideWithEnds: s := 'inside+ends';
    ssCrossing: s := 'crossing';
    ssCrossingWithEnds: s := 'crossing+ends';
    ssOutside: s := 'outside';
    ssRoundOutside: s := 'round-outside';
    ssVertexToSide: s := 'vertex-to-side';
    ssEasyBezier: s := 'easy-bezier';
  else s := '';
  end;
  AStorage.RawString['spline-style'] := s;
  if SplineStyle = ssEasyBezier then
  begin
    setlength({%H-}cm, PointCount);
    for i := 0 to PointCount-1 do
      cm[i] := ord(CurveMode[i]);
    AStorage.FloatArray['curve-mode'] := cm;
  end;
  AStorage.Float['cosine-angle'] := CosineAngle;
end;

class function TCurveShape.StorageClassName: RawByteString;
begin
  Result:= 'curve';
end;

initialization

  RegisterVectorShape(TPolylineShape);
  RegisterVectorShape(TCurveShape);

end.

