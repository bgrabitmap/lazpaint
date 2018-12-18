unit LCVectorPolyShapes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, LCVectorOriginal, BGRABitmapTypes, BGRALayerOriginal,
  BGRABitmap, BGRATransform, BGRAGradients;

type
  { TCustomPolypointShape }

  TCustomPolypointShape = class(TVectorShape)
  private
    FClosed: boolean;
    function GetPoint(AIndex: integer): TPointF;
    function GetPointCount: integer;
    procedure SetPoint(AIndex: integer; AValue: TPointF);
  protected
    FPoints: array of record
               coord: TPointF;
               editorIndex: integer;
               data: pointer;
             end;
    FCenterPoint: TPointF;
    FCenterPointEditorIndex: integer;
    FCurPoint: integer;
    FAddingPoint: boolean;
    FMousePos: TPointF;
    FHoverPoint: integer;
    procedure OnMovePoint({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF; {%H-}AShift: TShiftState);
    procedure OnMoveCenterPoint({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF; {%H-}AShift: TShiftState);
    procedure OnStartMove({%H-}ASender: TObject; APointIndex: integer; {%H-}AShift: TShiftState);
    function GetCurve(AMatrix: TAffineMatrix): ArrayOfTPointF; virtual;
    procedure SetUsermode(AValue: TVectorShapeUsermode); override;
    function GetClosed: boolean; virtual;
    procedure SetClosed(AValue: boolean); virtual;
    function PointsEqual(const APoint1, APoint2: TPointF): boolean;
    procedure OnHoverPoint({%H-}ASender: TObject; APointIndex: integer); virtual;
    procedure OnClickPoint({%H-}ASender: TObject; APointIndex: integer; {%H-}AShift: TShiftState); virtual;
    procedure DoClickPoint({%H-}APointIndex: integer; {%H-}AShift: TShiftState); virtual;
    function CanMovePoints: boolean; virtual;
    procedure InsertPointAuto;
  public
    constructor Create(AContainer: TVectorOriginal); override;
    procedure Clear;
    destructor Destroy; override;
    function AddPoint(const APoint: TPointF): integer;
    function RemovePoint(AIndex: integer): boolean;
    procedure RemovePointRange(AFromIndex, AToIndexPlus1: integer);
    procedure InsertPoint(AIndex: integer; APoint: TPointF);
    procedure MouseMove({%H-}Shift: TShiftState; X, Y: single; var {%H-}ACursor: TOriginalEditorCursor; var AHandled: boolean); override;
    procedure MouseDown(RightButton: boolean; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: single; var {%H-}ACursor: TOriginalEditorCursor; var AHandled: boolean); override;
    procedure KeyDown({%H-}Shift: TShiftState; Key: TSpecialKey; var AHandled: boolean); override;
    procedure QuickDefine(const APoint1,APoint2: TPointF); override;
    procedure LoadFromStorage(AStorage: TBGRACustomOriginalStorage); override;
    procedure SaveToStorage(AStorage: TBGRACustomOriginalStorage); override;
    procedure ConfigureEditor(AEditor: TBGRAOriginalEditor); override;
    class function Usermodes: TVectorShapeUsermodes; override;
    property Points[AIndex:integer]: TPointF read GetPoint write SetPoint;
    property PointCount: integer read GetPointCount;
    property Closed: boolean read GetClosed write SetClosed;
    property HoverPoint: integer read FHoverPoint;
  end;

  { TPolylineShape }

  TPolylineShape = class(TCustomPolypointShape)
  protected
    function PenVisible(AAssumePenFill: boolean = false): boolean;
    function BackVisible: boolean;
  public
    class function Fields: TVectorShapeFields; override;
    procedure Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix; ADraft: boolean); override;
    function GetRenderBounds({%H-}ADestRect: TRect; AMatrix: TAffineMatrix; AOptions: TRenderBoundsOptions = []): TRectF; override;
    function PointInShape(APoint: TPointF): boolean; override;
    function GetIsSlow({%H-}AMatrix: TAffineMatrix): boolean; override;
    class function StorageClassName: RawByteString; override;
  end;

  { TCurveShape }

  TCurveShape = class(TPolylineShape)
  private
    FSplineStyle: TSplineStyle;
    function GetCurveMode(AIndex: integer): TEasyBezierCurveMode;
    procedure SetCurveMode(AIndex: integer; AValue: TEasyBezierCurveMode);
    procedure SetSplineStyle(AValue: TSplineStyle);
  protected
    function GetCurve(AMatrix: TAffineMatrix): ArrayOfTPointF; override;
    function CanMovePoints: boolean; override;
    procedure DoClickPoint(APointIndex: integer; {%H-}AShift: TShiftState); override;
  public
    class function Usermodes: TVectorShapeUsermodes; override;
    constructor Create(AContainer: TVectorOriginal); override;
    procedure KeyPress(UTF8Key: string; var AHandled: boolean); override;
    procedure LoadFromStorage(AStorage: TBGRACustomOriginalStorage); override;
    procedure SaveToStorage(AStorage: TBGRACustomOriginalStorage); override;
    class function StorageClassName: RawByteString; override;
    property SplineStyle: TSplineStyle read FSplineStyle write SetSplineStyle;
    property CurveMode[AIndex: integer]: TEasyBezierCurveMode read GetCurveMode write SetCurveMode;
  end;

implementation

uses BGRAPen, BGRAGraphics, BGRAFillInfo, BGRAPath, math, LCVectorialFill;

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

{ TCustomPolypointShape }

function TCustomPolypointShape.GetClosed: boolean;
begin
  result := FClosed;
end;

function TCustomPolypointShape.GetPoint(AIndex: integer): TPointF;
begin
  if (AIndex < 0) or (AIndex >= length(FPoints)) then
    raise ERangeError.Create('Index out of bounds');
  result := FPoints[AIndex].coord;
end;

function TCustomPolypointShape.GetPointCount: integer;
begin
  result:= length(FPoints);
end;

procedure TCustomPolypointShape.SetClosed(AValue: boolean);
begin
  if AValue = FClosed then exit;
  BeginUpdate;
  FClosed := AValue;
  EndUpdate;
end;

procedure TCustomPolypointShape.SetPoint(AIndex: integer; AValue: TPointF);
begin
  if (AIndex < 0) or (AIndex > length(FPoints)) then
    raise ERangeError.Create('Index out of bounds');
  BeginUpdate;
  if AIndex = length(FPoints) then
  begin
    setlength(FPoints, length(FPoints)+1);
    FPoints[AIndex].coord := AValue;
    FPoints[AIndex].editorIndex := -1;
    FPoints[AIndex].data := nil;
  end
  else
    FPoints[AIndex].coord := AValue;
  EndUpdate;
end;

procedure TCustomPolypointShape.OnMovePoint(ASender: TObject; APrevCoord,
  ANewCoord: TPointF; AShift: TShiftState);
begin
  if FCurPoint = -1 then exit;
  BeginUpdate;
  Points[FCurPoint] := ANewCoord;
  EndUpdate;
end;

procedure TCustomPolypointShape.OnMoveCenterPoint(ASender: TObject; APrevCoord,
  ANewCoord: TPointF; AShift: TShiftState);
var
  i: Integer;
  delta: TPointF;
begin
  BeginUpdate;
  delta := ANewCoord - APrevCoord;
  for i := 0 to PointCount-1 do
    Points[i] := Points[i]+delta;
  if vsfBackFill in Fields then
    BackFill.Transform(AffineMatrixTranslation(delta.x, delta.y));
  if vsfPenFill in Fields then
    PenFill.Transform(AffineMatrixTranslation(delta.x, delta.y));
  EndUpdate;
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

class function TCustomPolypointShape.Usermodes: TVectorShapeUsermodes;
begin
  Result:= inherited Usermodes + [vsuCreate];
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
  i: Integer;
begin
  FHoverPoint:= -1;
  if APointIndex <> -1 then
  begin
    for i:= 0 to PointCount-1 do
      if FPoints[i].editorIndex = APointIndex then
      begin
        FHoverPoint:= i;
        break;
      end;
  end;
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
begin
  //nothing
end;

function TCustomPolypointShape.CanMovePoints: boolean;
begin
  result := true;
end;

procedure TCustomPolypointShape.InsertPointAuto;
var
  bestSegmentIndex, i: Integer;
  bestSegmentDist, segmentLen, segmentPos: single;
  u, n: TPointF;
  segmentDist: single;
begin
  if isEmptyPointF(FMousePos) then exit;

  for i := 0 to PointCount-1 do
    if (Points[i] = FMousePos) and not (FAddingPoint and (i = PointCount-1)) then exit;

  bestSegmentIndex := -1;
  bestSegmentDist := MaxSingle;
  for i := 0 to PointCount-1 do
  if FAddingPoint and (i >= PointCount-2) then break else
  begin
    if (i = PointCount-1) and not Closed then break;
    u := Points[(i+1) mod PointCount] - Points[i];
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
        end;
      end;
    end;
  end;
  if bestSegmentIndex <> -1 then
  begin
    InsertPoint(bestSegmentIndex+1, FMousePos);
    FHoverPoint:= bestSegmentIndex+1;
  end;
end;

constructor TCustomPolypointShape.Create(AContainer: TVectorOriginal);
begin
  inherited Create(AContainer);
  FMousePos := EmptyPointF;
  FClosed:= false;
  FHoverPoint:= -1;
end;

procedure TCustomPolypointShape.Clear;
begin
  RemovePointRange(0, PointCount);
end;

destructor TCustomPolypointShape.Destroy;
var
  i: Integer;
begin
  for i := 0 to PointCount-1 do
  begin
    FreeMem(FPoints[i].data);
    FPoints[i].data := nil;
  end;
  inherited Destroy;
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
  BeginUpdate;
  for i := AFromIndex to AToIndexPlus1-1 do
  begin
    freemem(FPoints[i].data);
    FPoints[i].data := nil;
  end;
  delCount := AToIndexPlus1-AFromIndex;
  for i := AFromIndex to PointCount-DelCount-1 do
    FPoints[i] := FPoints[i+delCount];
  setlength(FPoints, PointCount-delCount);
  EndUpdate;
end;

procedure TCustomPolypointShape.InsertPoint(AIndex: integer; APoint: TPointF);
var
  i: Integer;
begin
  if (AIndex < 0) or (AIndex > PointCount) then raise exception.Create('Index out of bounds');
  BeginUpdate;
  setlength(FPoints, PointCount+1);
  for i := PointCount-1 downto AIndex+1 do
    FPoints[i] := FPoints[i-1];
  FPoints[AIndex].coord := APoint;
  FPoints[AIndex].editorIndex:= -1;
  FPoints[AIndex].data := nil;
  EndUpdate;
end;

procedure TCustomPolypointShape.MouseMove(Shift: TShiftState; X, Y: single; var
  ACursor: TOriginalEditorCursor; var AHandled: boolean);
begin
  FMousePos := PointF(X,Y);
  if FAddingPoint then
  begin
    Points[PointCount-1] := FMousePos;
    AHandled:= true;
  end;
end;

procedure TCustomPolypointShape.MouseDown(RightButton: boolean;
  Shift: TShiftState; X, Y: single; var ACursor: TOriginalEditorCursor; var
  AHandled: boolean);
begin
  if FAddingPoint then
  begin
    if not RightButton then
    begin
      if (PointCount>1) and not PointsEqual(Points[PointCount-1],Points[PointCount-2]) then
        AddPoint(Points[PointCount-1]);
    end else
      Usermode := vsuEdit;
    AHandled:= true;
  end;
end;

procedure TCustomPolypointShape.KeyDown(Shift: TShiftState; Key: TSpecialKey;
  var AHandled: boolean);
begin
  if (Key = skDelete) and (FAddingPoint or ((FHoverPoint >= 0) and (FHoverPoint < PointCount))) then
  begin
    if (FHoverPoint >= 0) and (FHoverPoint < PointCount) then
    begin
      BeginUpdate;
      RemovePoint(FHoverPoint);
      if (FHoverPoint < PointCount) and IsEmptyPointF(Points[FHoverPoint]) then RemovePoint(FHoverPoint);
      EndUpdate;
      if PointCount = 0 then self.Remove;
    end;
    AHandled:= true;
  end else
  if (Key = skBackspace) and FAddingPoint then
  begin
    If PointCount <= 2 then self.Remove else
    If isEmptyPointF(Points[PointCount-3]) then
    begin
      RemovePointRange(PointCount-3, PointCount);
      Usermode:= vsuEdit;
    end else
      RemovePoint(PointCount-2);
  end else
  if (Key = skInsert) then InsertPointAuto else
    inherited KeyDown(Shift, Key, AHandled);
end;

procedure TCustomPolypointShape.QuickDefine(const APoint1, APoint2: TPointF);
begin
  BeginUpdate;
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
    FPoints[i].data := nil;
  end;
  FClosed:= AStorage.Bool['closed'];
  EndUpdate;
end;

procedure TCustomPolypointShape.SaveToStorage(AStorage: TBGRACustomOriginalStorage);
var
  x,y: Array of Single;
  i: Integer;
begin
  inherited SaveToStorage(AStorage);
  setlength(x, PointCount);
  setlength(y, PointCount);
  for i:= 0 to PointCount-1 do
  begin
    x[i] := Points[i].x;
    y[i] := Points[i].y;
  end;
  AStorage.FloatArray['x'] := x;
  AStorage.FloatArray['y'] := y;
  AStorage.Bool['closed'] := Closed;
end;

procedure TCustomPolypointShape.ConfigureEditor(AEditor: TBGRAOriginalEditor);
var
  i, nb: Integer;
begin
  AEditor.AddStartMoveHandler(@OnStartMove);
  AEditor.AddClickPointHandler(@OnClickPoint);
  AEditor.AddHoverPointHandler(@OnHoverPoint);
  nb := 0;
  FCenterPoint := PointF(0,0);
  for i:= 0 to PointCount-1 do
    if isEmptyPointF(Points[i]) then
      FPoints[i].editorIndex := -1
    else if (FAddingPoint and ((i = 0) or (i = PointCount-1))) then
    begin
      FPoints[i].editorIndex := -1;
      FCenterPoint += Points[i];
      inc(nb);
    end
    else
    begin
      if CanMovePoints then
        FPoints[i].editorIndex := AEditor.AddPoint(Points[i], @OnMovePoint, false)
      else
        FPoints[i].editorIndex := AEditor.AddFixedPoint(Points[i], false);
      FCenterPoint += Points[i];
      inc(nb);
    end;

  if (FAddingPoint and (nb > 2)) or (not FAddingPoint and (nb > 1)) then
  begin
    FCenterPoint *= 1/nb;
    FCenterPointEditorIndex := AEditor.AddPoint(FCenterPoint, @OnMoveCenterPoint, true);
  end;
end;

{ TPolylineShape }

function TPolylineShape.PenVisible(AAssumePenFill: boolean): boolean;
begin
  result := (PenWidth>0) and not IsClearPenStyle(PenStyle) and (not PenFill.IsFullyTransparent or AAssumePenFill);
end;

function TPolylineShape.BackVisible: boolean;
begin
  result := not BackFill.IsFullyTransparent;
end;

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
  if not BackVisible and not PenVisible then exit;
  pts := GetCurve(AMatrix);
  if BackVisible then
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
  if PenVisible then
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
  end;
end;

function TPolylineShape.GetRenderBounds(ADestRect: TRect; AMatrix: TAffineMatrix; AOptions: TRenderBoundsOptions): TRectF;
var
  pts: ArrayOfTPointF;
  xMargin, yMargin: single;
  fillBounds, penBounds: TRectF;
begin
  if not (BackVisible or (rboAssumeBackFill in AOptions)) and not PenVisible(rboAssumePenFill in AOptions) then
    result:= EmptyRectF
  else
  begin
    pts := GetCurve(AMatrix);
    if PenVisible(rboAssumePenFill in AOptions) then
    begin
      if JoinStyle = pjsRound then
      begin
        xMargin := (abs(AMatrix[1,1])+abs(AMatrix[1,2]))*PenWidth*0.5;
        yMargin := (abs(AMatrix[2,1])+abs(AMatrix[2,2]))*PenWidth*0.5;
        result := GetPointsBoundsF(pts);
        result.Left -= xMargin;
        result.Top -= yMargin;
        result.Right += xMargin;
        result.Bottom += yMargin;
      end else
      begin
        if BackVisible or (rboAssumeBackFill in AOptions) then fillBounds := GetPointsBoundsF(pts)
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
  if not BackVisible and not PenVisible then exit;
  pts := GetCurve(AffineMatrixIdentity);
  if BackVisible and IsPointInPolygon(pts, APoint, true) then exit(true);
  if PenVisible then
  begin
    pts := ComputeStroke(pts, Closed, AffineMatrixIdentity);
    if IsPointInPolygon(pts, APoint, true) then exit(true);
  end;
  result := false;
end;

function TPolylineShape.GetIsSlow(AMatrix: TAffineMatrix): boolean;
begin
  Result:= PointCount > 40;
end;

class function TPolylineShape.StorageClassName: RawByteString;
begin
  result := 'polyline';
end;

{ TCurveShape }

procedure TCurveShape.SetSplineStyle(AValue: TSplineStyle);
begin
  if FSplineStyle=AValue then Exit;
  BeginUpdate;
  FSplineStyle:=AValue;
  EndUpdate;
end;

function TCurveShape.GetCurveMode(AIndex: integer): TEasyBezierCurveMode;
begin
  if (AIndex < 0) or (AIndex >= PointCount) then exit(cmCurve);
  if Assigned(FPoints[AIndex].data) then
    result := TEasyBezierCurveMode(FPoints[AIndex].data^)
  else
    result := cmAuto;
end;

procedure TCurveShape.SetCurveMode(AIndex: integer; AValue: TEasyBezierCurveMode);
begin
  if (AIndex < 0) or (AIndex >= PointCount) then exit;
  if CurveMode[AIndex] = AValue then exit;
  BeginUpdate;
  if FPoints[AIndex].data = nil then FPoints[AIndex].data := getmem(sizeof(TEasyBezierCurveMode));
  TEasyBezierCurveMode(FPoints[AIndex].data^) := AValue;
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
    setlength(cm, PointCount);
    for i := 0 to PointCount-1 do
      cm[i] := CurveMode[i];
    eb := EasyBezierCurve(pts, Closed, cm);
    result := eb.ToPoints;
  end else
  begin
    if Closed then result := ComputeClosedSpline(pts, FSplineStyle)
    else result := ComputeOpenedSpline(pts, FSplineStyle);
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

procedure TCurveShape.KeyPress(UTF8Key: string; var AHandled: boolean);
begin
  if (FHoverPoint >= 0) and (FHoverPoint < PointCount) then
  begin
    if (UTF8Key = 'A') or (UTF8Key = 'a') then
    begin
      CurveMode[FHoverPoint] := cmAuto;
      AHandled := true;
    end else
    if (UTF8Key = 'S') or (UTF8Key = 's') then
    begin
      CurveMode[FHoverPoint] := cmCurve;
      AHandled:= true;
    end else
    if (UTF8Key = 'X') or (UTF8Key = 'x') then
    begin
      CurveMode[FHoverPoint] := cmAngle;
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
    setlength(cm, PointCount);
    for i := 0 to PointCount-1 do
      cm[i] := ord(CurveMode[i]);
    AStorage.FloatArray['curve-mode'] := cm;
  end;
end;

class function TCurveShape.StorageClassName: RawByteString;
begin
  Result:= 'curve';
end;

initialization

  RegisterVectorShape(TPolylineShape);
  RegisterVectorShape(TCurveShape);

end.

