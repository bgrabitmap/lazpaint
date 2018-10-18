unit uvectororiginal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRALayerOriginal, fgl, BGRAGradientOriginal, BGRABitmapTypes,
  BGRAPen;

const
  InfiniteRect : TRect = (Left: -MaxLongInt; Top: -MaxLongInt; Right: MaxLongInt; Bottom: MaxLongInt);

type
  TShapeChangeEvent = procedure(ASender: TObject; ABounds: TRectF) of object;
  TShapeEditingChangeEvent = procedure(ASender: TObject) of object;

  TVectorShapeField = (vsfPenColor, vsfPenWidth, vsfPenStyle, vsfJoinStyle, vsfBackColor);
  TVectorShapeFields = set of TVectorShapeField;
  TVectorShapeUsermode = (vsuEdit, vsuCreate);
  TVectorShapeUsermodes = set of TVectorShapeUsermode;

  { TVectorShape }

  TVectorShape = class
  private
    FOnChange: TShapeChangeEvent;
    FOnEditingChange: TShapeEditingChangeEvent;
    FUpdateCount: integer;
    FBoundsBeforeUpdate: TRectF;
    FPenColor,FBackColor: TBGRAPixel;
    FPenWidth: single;
    FStroker: TBGRAPenStroker;
    FUsermode: TVectorShapeUsermode;
  protected
    procedure BeginUpdate;
    procedure EndUpdate;
    function GetPenColor: TBGRAPixel; virtual;
    function GetPenWidth: single; virtual;
    function GetPenStyle: TBGRAPenStyle; virtual;
    function GetJoinStyle: TPenJoinStyle;
    function GetBackColor: TBGRAPixel; virtual;
    procedure SetPenColor(AValue: TBGRAPixel); virtual;
    procedure SetPenWidth(AValue: single); virtual;
    procedure SetPenStyle({%H-}AValue: TBGRAPenStyle); virtual;
    procedure SetJoinStyle(AValue: TPenJoinStyle);
    procedure SetBackColor(AValue: TBGRAPixel); virtual;
    procedure SetUsermode(AValue: TVectorShapeUsermode); virtual;
    function ComputeStroke(APoints: ArrayOfTPointF; AClosed: boolean; AStrokeMatrix: TAffineMatrix): ArrayOfTPointF;
    function GetStroker: TBGRAPenStroker;
    property Stroker: TBGRAPenStroker read GetStroker;
  public
    constructor Create;
    destructor Destroy; override;
    procedure QuickDefine(const APoint1,APoint2: TPointF); virtual; abstract;
    procedure Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix; ADraft: boolean); virtual; abstract;
    function GetRenderBounds(ADestRect: TRect; AMatrix: TAffineMatrix): TRectF; virtual; abstract;
    function PointInShape(APoint: TPointF): boolean; virtual; abstract;
    procedure ConfigureEditor(AEditor: TBGRAOriginalEditor); virtual; abstract;
    procedure LoadFromStorage(AStorage: TBGRACustomOriginalStorage); virtual;
    procedure SaveToStorage(AStorage: TBGRACustomOriginalStorage); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: single; out ACursor: TOriginalEditorCursor; out AHandled: boolean); virtual;
    procedure MouseDown(RightButton: boolean; Shift: TShiftState; X, Y: single; out ACursor: TOriginalEditorCursor; out AHandled: boolean); virtual;
    procedure MouseUp(RightButton: boolean; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: single; out ACursor: TOriginalEditorCursor; out AHandled: boolean); virtual;
    class function StorageClassName: RawByteString; virtual; abstract;
    function GetIsSlow({%H-}AMatrix: TAffineMatrix): boolean; virtual;
    class function Fields: TVectorShapeFields; virtual;
    class function Usermodes: TVectorShapeUsermodes; virtual;
    property OnChange: TShapeChangeEvent read FOnChange write FOnChange;
    property OnEditingChange: TShapeEditingChangeEvent read FOnEditingChange write FOnEditingChange;
    property PenColor: TBGRAPixel read GetPenColor write SetPenColor;
    property BackColor: TBGRAPixel read GetBackColor write SetBackColor;
    property PenWidth: single read GetPenWidth write SetPenWidth;
    property PenStyle: TBGRAPenStyle read GetPenStyle write SetPenStyle;
    property JoinStyle: TPenJoinStyle read GetJoinStyle write SetJoinStyle;
    property Usermode: TVectorShapeUsermode read FUsermode write SetUsermode;
  end;
  TVectorShapes = specialize TFPGList<TVectorShape>;
  TVectorShapeAny = class of TVectorShape;

  { TCustomRectShape }

  TCustomRectShape = class(TVectorShape)
  protected
    FOrigin, FXAxis, FYAxis: TPointF;
    FOriginBackup,FXUnitBackup,FYUnitBackup,
    FXAxisBackup,FYAxisBackup: TPointF;
    FXSizeBackup,FYSizeBackup: single;
    procedure DoMoveXAxis(ANewCoord: TPointF; AShift: TShiftState; AFactor: single);
    procedure DoMoveYAxis(ANewCoord: TPointF; AShift: TShiftState; AFactor: single);
    procedure DoMoveXYCorner(ANewCoord: TPointF; AShift: TShiftState; AFactorX, AFactorY: single);
    procedure OnMoveOrigin({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF; {%H-}AShift: TShiftState);
    procedure OnMoveXAxis({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF; AShift: TShiftState);
    procedure OnMoveYAxis({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF; AShift: TShiftState);
    procedure OnMoveXAxisNeg({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF; AShift: TShiftState);
    procedure OnMoveYAxisNeg({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF; AShift: TShiftState);
    procedure OnMoveXYCorner({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF; AShift: TShiftState);
    procedure OnMoveXNegYCorner({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF; AShift: TShiftState);
    procedure OnMoveXYNegCorner({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF; AShift: TShiftState);
    procedure OnMoveXNegYNegCorner({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF; AShift: TShiftState);
    procedure OnStartMove({%H-}ASender: TObject; {%H-}APointIndex: integer; {%H-}AShift: TShiftState);
    function GetAffineBox(AMatrix: TAffineMatrix; APixelCentered: boolean): TAffineBox;
    function GetCornerPositition: single; virtual; abstract;
    function GetOrthoRect(AMatrix: TAffineMatrix; out ARect: TRectF): boolean;
  public
    procedure QuickDefine(const APoint1,APoint2: TPointF); override;
    procedure LoadFromStorage(AStorage: TBGRACustomOriginalStorage); override;
    procedure SaveToStorage(AStorage: TBGRACustomOriginalStorage); override;
    function GetRenderBounds({%H-}ADestRect: TRect; AMatrix: TAffineMatrix): TRectF; override;
    procedure ConfigureEditor(AEditor: TBGRAOriginalEditor); override;
  end;

  { TRectShape }

  TRectShape = class(TCustomRectShape)
  protected
    function PenVisible: boolean;
    function BackVisible: boolean;
    function GetCornerPositition: single; override;
  public
    class function Fields: TVectorShapeFields; override;
    procedure Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix; ADraft: boolean); override;
    function GetRenderBounds({%H-}ADestRect: TRect; AMatrix: TAffineMatrix): TRectF; override;
    function PointInShape(APoint: TPointF): boolean; override;
    function GetIsSlow(AMatrix: TAffineMatrix): boolean; override;
    class function StorageClassName: RawByteString; override;
  end;

  { TEllipseShape }

  TEllipseShape = class(TCustomRectShape)
  protected
    function PenVisible: boolean;
    function BackVisible: boolean;
    function GetCornerPositition: single; override;
  public
    constructor Create;
    class function Fields: TVectorShapeFields; override;
    procedure Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix; ADraft: boolean); override;
    function GetRenderBounds({%H-}ADestRect: TRect; AMatrix: TAffineMatrix): TRectF; override;
    function PointInShape(APoint: TPointF): boolean; override;
    function GetIsSlow(AMatrix: TAffineMatrix): boolean; override;
    class function StorageClassName: RawByteString; override;
  end;

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
             end;
    FCenterPoint: TPointF;
    FCenterPointEditorIndex: integer;
    FCurPoint: integer;
    FAddingPoint: boolean;
    FMousePos: TPointF;
    procedure OnMovePoint({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF; {%H-}AShift: TShiftState);
    procedure OnMoveCenterPoint({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF; {%H-}AShift: TShiftState);
    procedure OnStartMove({%H-}ASender: TObject; APointIndex: integer; {%H-}AShift: TShiftState);
    function GetCurve(AMatrix: TAffineMatrix): ArrayOfTPointF; virtual;
    procedure SetUsermode(AValue: TVectorShapeUsermode); override;
    function GetClosed: boolean; virtual;
    procedure SetClosed(AValue: boolean); virtual;
    function PointsEqual(const APoint1, APoint2: TPointF): boolean;
  public
    constructor Create;
    procedure AddPoint(const APoint: TPointF);
    procedure MouseMove(Shift: TShiftState; X, Y: single; out ACursor: TOriginalEditorCursor; out AHandled: boolean); override;
    procedure MouseDown(RightButton: boolean; Shift: TShiftState; X, Y: single; out ACursor: TOriginalEditorCursor; out AHandled: boolean); override;
    procedure QuickDefine(const APoint1,APoint2: TPointF); override;
    procedure LoadFromStorage(AStorage: TBGRACustomOriginalStorage); override;
    procedure SaveToStorage(AStorage: TBGRACustomOriginalStorage); override;
    function GetRenderBounds({%H-}ADestRect: TRect; AMatrix: TAffineMatrix): TRectF; override;
    procedure ConfigureEditor(AEditor: TBGRAOriginalEditor); override;
    class function Usermodes: TVectorShapeUsermodes; override;
    property Points[AIndex:integer]: TPointF read GetPoint write SetPoint;
    property PointCount: integer read GetPointCount;
    property Closed: boolean read GetClosed write SetClosed;
  end;

  { TPolylineShape }

  TPolylineShape = class(TCustomPolypointShape)
  protected
    function PenVisible: boolean;
    function BackVisible: boolean;
  public
    class function Fields: TVectorShapeFields; override;
    procedure Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix; ADraft: boolean); override;
    function GetRenderBounds({%H-}ADestRect: TRect; AMatrix: TAffineMatrix): TRectF; override;
    function PointInShape(APoint: TPointF): boolean; override;
    function GetIsSlow({%H-}AMatrix: TAffineMatrix): boolean; override;
    class function StorageClassName: RawByteString; override;
  end;

  { TCurveShape }

  TCurveShape = class(TPolylineShape)
  protected
    function GetCurve(AMatrix: TAffineMatrix): ArrayOfTPointF; override;
  public
    class function StorageClassName: RawByteString; override;
  end;

  TVectorOriginalSelectShapeEvent = procedure(ASender: TObject; AShape: TVectorShape; APreviousShape: TVectorShape) of object;

  TVectorOriginalEditor = class;

  { TVectorOriginal }

  TVectorOriginal = class(TBGRALayerCustomOriginal)
  protected
    FShapes: TVectorShapes;
    FDeletedShapes: TVectorShapes;
    FSelectedShape: TVectorShape;
    FFrozenShapesUnderSelection,
    FFrozenShapesOverSelection: TBGRABitmap;
    FFrozenShapesComputed: boolean;
    FFrozenShapeMatrix: TAffineMatrix;
    FOnSelectShape: TVectorOriginalSelectShapeEvent;
    procedure FreeDeletedShapes;
    procedure OnShapeChange(ASender: TObject; ABounds: TRectF);
    procedure OnShapeEditingChange(ASender: TObject);
    procedure DiscardFrozenShapes;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear;
    function AddShape(AShape: TVectorShape): integer; overload;
    function AddShape(AShape: TVectorShape; AUsermode: TVectorShapeUsermode): integer; overload;
    function RemoveShape(AShape: TVectorShape): boolean;
    procedure SelectShape(AIndex: integer); overload;
    procedure SelectShape(AShape: TVectorShape); overload;
    procedure DeselectShape;
    procedure MouseClick(APoint: TPointF);
    procedure Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix; ADraft: boolean); override;
    procedure ConfigureEditor(AEditor: TBGRAOriginalEditor); override;
    function CreateEditor: TBGRAOriginalEditor; override;
    function GetRenderBounds(ADestRect: TRect; {%H-}AMatrix: TAffineMatrix): TRect; override;
    procedure LoadFromStorage(AStorage: TBGRACustomOriginalStorage); override;
    procedure SaveToStorage(AStorage: TBGRACustomOriginalStorage); override;
    class function StorageClassName: RawByteString; override;
    property OnSelectShape: TVectorOriginalSelectShapeEvent read FOnSelectShape write FOnSelectShape;
    property SelectedShape: TVectorShape read FSelectedShape;
  end;

  { TVectorOriginalEditor }

  TVectorOriginalEditor = class(TBGRAOriginalEditor)
  protected
    FOriginal: TVectorOriginal;
  public
    constructor Create(AOriginal: TVectorOriginal);
    procedure MouseMove(Shift: TShiftState; X, Y: single; out ACursor: TOriginalEditorCursor; out AHandled: boolean); override;
    procedure MouseDown(RightButton: boolean; Shift: TShiftState; X, Y: single; out ACursor: TOriginalEditorCursor; out AHandled: boolean); override;
    procedure MouseUp(RightButton: boolean; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: single; out ACursor: TOriginalEditorCursor; out AHandled: boolean); override;
  end;

procedure RegisterVectorShape(AClass: TVectorShapeAny);
function GetVectorShapeByStorageClassName(AName: string): TVectorShapeAny;

implementation

uses math, BGRATransform, BGRAFillInfo, BGRAGraphics, BGRAPath;

var
  VectorShapeClasses: array of TVectorShapeAny;

procedure IncludePointF(var ARectF: TRectF; APointF: TPointF);
begin
  if APointF.x < ARectF.Left then ARectF.Left := APointF.x;
  if APointF.x > ARectF.Right then ARectF.Right := APointF.x;
  if APointF.y < ARectF.Top then ARectF.Top := APointF.y;
  if APointF.y > ARectF.Bottom then ARectF.Bottom := APointF.y;
end;

function GetVectorShapeByStorageClassName(AName: string): TVectorShapeAny;
var
  i: Integer;
begin
  for i := 0 to high(VectorShapeClasses) do
    if VectorShapeClasses[i].StorageClassName = AName then exit(VectorShapeClasses[i]);
  exit(nil);
end;

procedure RegisterVectorShape(AClass: TVectorShapeAny);
var
  i: Integer;
begin
  for i := 0 to high(VectorShapeClasses) do
    if VectorShapeClasses[i]=AClass then exit;
  if Assigned(GetVectorShapeByStorageClassName(AClass.StorageClassName)) then
    raise exception.Create('Duplicate class name "'+AClass.StorageClassName+'" for vector shape');
  setlength(VectorShapeClasses, length(VectorShapeClasses)+1);
  VectorShapeClasses[high(VectorShapeClasses)] := AClass;
end;

{ TCurveShape }

function TCurveShape.GetCurve(AMatrix: TAffineMatrix): ArrayOfTPointF;
var
  pts: array of TPointF;
begin
  pts := inherited GetCurve(AMatrix);
  if Closed then result := ComputeClosedSpline(pts, ssCrossingWithEnds)
  else result := ComputeOpenedSpline(pts, ssCrossingWithEnds);
end;

class function TCurveShape.StorageClassName: RawByteString;
begin
  Result:= 'curve';
end;

{ TVectorOriginalEditor }

constructor TVectorOriginalEditor.Create(AOriginal: TVectorOriginal);
begin
  inherited Create;
  FOriginal := AOriginal;
end;

procedure TVectorOriginalEditor.MouseMove(Shift: TShiftState; X, Y: single; out
  ACursor: TOriginalEditorCursor; out AHandled: boolean);
begin
  inherited MouseMove(Shift, X, Y, ACursor, AHandled);
  if not AHandled and Assigned(FOriginal.SelectedShape) then
    FOriginal.SelectedShape.MouseMove(Shift, X,Y, ACursor, AHandled);
end;

procedure TVectorOriginalEditor.MouseDown(RightButton: boolean;
  Shift: TShiftState; X, Y: single; out ACursor: TOriginalEditorCursor; out
  AHandled: boolean);
begin
  inherited MouseDown(RightButton, Shift, X, Y, ACursor, AHandled);
  if not AHandled and Assigned(FOriginal.SelectedShape) then
    FOriginal.SelectedShape.MouseDown(RightButton, Shift, X,Y, ACursor, AHandled);
end;

procedure TVectorOriginalEditor.MouseUp(RightButton: boolean;
  Shift: TShiftState; X, Y: single; out ACursor: TOriginalEditorCursor; out
  AHandled: boolean);
begin
  inherited MouseUp(RightButton, Shift, X, Y, ACursor, AHandled);
  if not AHandled and Assigned(FOriginal.SelectedShape) then
    FOriginal.SelectedShape.MouseUp(RightButton, Shift, X,Y, ACursor, AHandled);
end;

{ TPolylineShape }

function TPolylineShape.PenVisible: boolean;
begin
  result := (PenWidth>0) and (PenColor.alpha>0) and not IsClearPenStyle(PenStyle);
end;

function TPolylineShape.BackVisible: boolean;
begin
  result := BackColor.alpha <> 0;
end;

class function TPolylineShape.Fields: TVectorShapeFields;
begin
  Result:= [vsfPenColor, vsfPenWidth, vsfPenStyle, vsfJoinStyle, vsfBackColor];
end;

procedure TPolylineShape.Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix;
  ADraft: boolean);
var
  pts: array of TPointF;
begin
  if not BackVisible and not PenVisible then exit;
  pts := GetCurve(AMatrix);
  if BackVisible then
  begin
    if ADraft then
      ADest.FillPoly(pts, BackColor, dmDrawWithTransparency)
    else
      ADest.FillPolyAntialias(pts, BackColor);
  end;
  if PenVisible then
  begin
    pts := ComputeStroke(pts, true, AMatrix);
    if ADraft and (PenWidth > 4) then
      ADest.FillPoly(pts, PenColor, dmDrawWithTransparency)
    else
      ADest.FillPolyAntialias(pts, PenColor);
  end;
end;

function TPolylineShape.GetRenderBounds(ADestRect: TRect; AMatrix: TAffineMatrix): TRectF;
var
  i: Integer;
  pts: ArrayOfTPointF;
  xMargin, yMargin: single;
begin
  if not BackVisible and not PenVisible then
    result:= EmptyRectF
  else
  begin
    if PenVisible then
    begin
      if (JoinStyle <> pjsMiter) or (Stroker.MiterLimit <= 1) then
      begin
        xMargin := (abs(AMatrix[1,1])+abs(AMatrix[1,2]))*PenWidth*0.5;
        yMargin := (abs(AMatrix[2,1])+abs(AMatrix[2,2]))*PenWidth*0.5;
        result := inherited GetRenderBounds(ADestRect, AMatrix);
        result.Left -= xMargin;
        result.Top -= yMargin;
        result.Right += xMargin;
        result.Bottom += yMargin;
      end else
      begin
        pts := ComputeStroke(GetCurve(AMatrix), true, AMatrix);
        for i := 0 to high(pts) do
        begin
          if pts[i].x < result.Left then result.Left := pts[i].x;
          if pts[i].x > result.Right then result.Right := pts[i].x;
          if pts[i].y < result.Top then result.Top := pts[i].y;
          if pts[i].y > result.Bottom then result.Bottom := pts[i].y;
        end;
      end;
    end
    else
      result := inherited GetRenderBounds(ADestRect, AMatrix);
  end;
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
    pts := ComputeStroke(pts, true, AffineMatrixIdentity);
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
    FPoints[high(FPoints)].coord := AValue;
    FPoints[high(FPoints)].editorIndex := -1;
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
  EndUpdate;
end;

procedure TCustomPolypointShape.OnStartMove(ASender: TObject; APointIndex: integer;
  AShift: TShiftState);
var
  i: Integer;
begin
  FCurPoint:= -1;
  for i:= 0 to high(FPoints) do
    if FPoints[i].editorIndex = APointIndex then
    begin
      FCurPoint:= i;
      break;
    end;
end;

function TCustomPolypointShape.GetCurve(AMatrix: TAffineMatrix): ArrayOfTPointF;
var
  i: Integer;
begin
  setlength(result, PointCount);
  for i := 0 to PointCount-1 do
    result[i] := AMatrix*Points[i];
end;

class function TCustomPolypointShape.Usermodes: TVectorShapeUsermodes;
begin
  Result:=[vsuEdit,vsuCreate];
end;

procedure TCustomPolypointShape.SetUsermode(AValue: TVectorShapeUsermode);
var
  add: Boolean;
begin
  add := AValue = vsuCreate;
  if add and (length(FPoints) = 0) then exit;
  if FAddingPoint and not add then
  begin
    if (length(FPoints)>1) and PointsEqual(FPoints[high(FPoints)].coord,FPoints[high(FPoints)-1].coord) then
    begin
      BeginUpdate;
      setlength(FPoints, length(FPoints)-1);
      EndUpdate;
    end;
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

constructor TCustomPolypointShape.Create;
begin
  inherited Create;
  FMousePos := EmptyPointF;
  FClosed:= false;
end;

procedure TCustomPolypointShape.AddPoint(const APoint: TPointF);
begin
  Points[PointCount] := APoint;
end;

procedure TCustomPolypointShape.MouseMove(Shift: TShiftState; X, Y: single; out
  ACursor: TOriginalEditorCursor; out AHandled: boolean);
begin
  FMousePos := PointF(X,Y);
  if FAddingPoint then
  begin
    BeginUpdate;
    FPoints[high(FPoints)].coord := FMousePos;
    EndUpdate;
  end;
end;

procedure TCustomPolypointShape.MouseDown(RightButton: boolean;
  Shift: TShiftState; X, Y: single; out ACursor: TOriginalEditorCursor; out
  AHandled: boolean);
begin
  if FAddingPoint then
  begin
    if not RightButton then
    begin
      if (length(FPoints)>1) and not PointsEqual(FPoints[high(FPoints)].coord,FPoints[high(FPoints)-1].coord) then
        AddPoint(FPoints[high(FPoints)].coord);
    end else
      Usermode := vsuEdit;
    AHandled:= true;
  end;
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
  x := AStorage.FloatArray['x'];
  y := AStorage.FloatArray['y'];
  setlength(FPoints, max(length(x),length(y)));
  for i := 0 to high(FPoints) do
  begin
    FPoints[i].coord := PointF(x[i],y[i]);
    FPoints[i].editorIndex := -1;
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

function TCustomPolypointShape.GetRenderBounds(ADestRect: TRect;
  AMatrix: TAffineMatrix): TRectF;
var
  i: Integer;
  pts: ArrayOfTPointF;
  firstPoint: Boolean;
begin
  result:= EmptyRectF;
  firstPoint := true;
  pts := GetCurve(AMatrix);
  for i:= 0 to high(pts) do
    if not isEmptyPointF(pts[i]) then
    begin
      if firstPoint then
      begin
        result.TopLeft := pts[i];
        result.BottomRight := pts[i];
        firstPoint := false;
      end else
      with pts[i] do
      begin
        if x < result.Left then result.Left := x
        else if x > result.right then result.right := x;
        if y < result.top then result.top := y
        else if y > result.bottom then result.bottom := y;
      end;
    end;
end;

procedure TCustomPolypointShape.ConfigureEditor(AEditor: TBGRAOriginalEditor);
var
  i, nb: Integer;
begin
  AEditor.AddStartMoveHandler(@OnStartMove);
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
      FPoints[i].editorIndex := AEditor.AddPoint(Points[i], @OnMovePoint, false);
      FCenterPoint += Points[i];
      inc(nb);
    end;

  if nb > 0 then
  begin
    FCenterPoint *= 1/nb;
    FCenterPointEditorIndex := AEditor.AddPoint(FCenterPoint, @OnMoveCenterPoint, true);
  end;

end;

{ TEllipseShape }

function TEllipseShape.PenVisible: boolean;
begin
  result := (PenWidth>0) and (PenColor.alpha>0) and not IsClearPenStyle(PenStyle);
end;

function TEllipseShape.BackVisible: boolean;
begin
  result := BackColor.alpha <> 0;
end;

function TEllipseShape.GetCornerPositition: single;
begin
  result := sqrt(2)/2;
end;

constructor TEllipseShape.Create;
begin
  inherited Create;
  inherited SetJoinStyle(pjsRound);
end;

class function TEllipseShape.Fields: TVectorShapeFields;
begin
  Result:= [vsfPenColor, vsfPenWidth, vsfPenStyle, vsfBackColor];
end;

procedure TEllipseShape.Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix;
  ADraft: boolean);
var
  pts: Array of TPointF;
  orthoRect: TRectF;
  center, radius: TPointF;
  draftPen, isOrtho: Boolean;
begin
  isOrtho := GetOrthoRect(AMatrix, orthoRect);
  if isOrtho then
  begin
    center := (orthoRect.TopLeft+orthoRect.BottomRight)*0.5;
    radius := (orthoRect.BottomRight-orthoRect.TopLeft)*0.5;
    If BackVisible then
    begin
      if ADraft then
        ADest.FillEllipseInRect(rect(round(orthoRect.Left),round(orthoRect.Top),round(orthoRect.Right),round(orthoRect.Bottom)),
                                BackColor, dmDrawWithTransparency)
      else
        ADest.FillEllipseAntialias(center.x, center.y, radius.x, radius.y, BackColor);
    end;
    if PenVisible then
    begin
      if IsAffineMatrixScaledRotation(AMatrix) then
      begin
        ADest.CustomPenStyle := PenStyle;
        draftPen := ADraft and (PenWidth > 4);
        if draftPen then
          ADest.Ellipse(center.x, center.y, radius.x, radius.y, PenColor, PenWidth, dmDrawWithTransparency)
        else
          ADest.EllipseAntialias(center.x, center.y, radius.x, radius.y, PenColor, PenWidth);
        ADest.PenStyle := psSolid;
      end else
      begin
        pts := ComputeEllipse(AMatrix*FOrigin, AMatrix*FXAxis, AMatrix*FYAxis);
        pts := ComputeStroke(pts,true, AMatrix);
        if ADraft and (PenWidth > 4) then
          ADest.FillPoly(pts, PenColor, dmDrawWithTransparency)
        else
          ADest.FillPolyAntialias(pts, PenColor);
      end;
    end;
  end else
  begin
    pts := ComputeEllipse(AMatrix*FOrigin, AMatrix*FXAxis, AMatrix*FYAxis);
    If BackVisible then
    begin
      if ADraft then
        ADest.FillPoly(pts, BackColor, dmDrawWithTransparency)
      else
        ADest.FillPolyAntialias(pts, BackColor);
    end;
    if PenVisible then
    begin
      pts := ComputeStroke(pts,true, AMatrix);
      if ADraft and (PenWidth > 4) then
        ADest.FillPoly(pts, PenColor, dmDrawWithTransparency)
      else
        ADest.FillPolyAntialias(pts, PenColor);
    end;
  end;
end;

function TEllipseShape.GetRenderBounds({%H-}ADestRect: TRect; AMatrix: TAffineMatrix): TRectF;
var
  xMargin, yMargin: single;
begin
  if not BackVisible and not PenVisible then
    result:= EmptyRectF
  else
  begin
    result := inherited GetRenderBounds(ADestRect, AMatrix);
    if PenVisible then
    begin
      xMargin := (abs(AMatrix[1,1])+abs(AMatrix[1,2]))*PenWidth*0.5;
      yMargin := (abs(AMatrix[2,1])+abs(AMatrix[2,2]))*PenWidth*0.5;
      result.Left -= xMargin;
      result.Top -= yMargin;
      result.Right += xMargin;
      result.Bottom += yMargin;
    end;
  end;
end;

function TEllipseShape.PointInShape(APoint: TPointF): boolean;
var
  pts: ArrayOfTPointF;
begin
  pts := ComputeEllipse(FOrigin, FXAxis, FYAxis);
  if BackVisible and IsPointInPolygon(pts, APoint, true) then
    result := true else
  if PenVisible then
  begin
    pts := ComputeStroke(pts, true, AffineMatrixIdentity);
    result:= IsPointInPolygon(pts, APoint, true);
  end else
    result := false;
end;

function TEllipseShape.GetIsSlow(AMatrix: TAffineMatrix): boolean;
var
  ab: TAffineBox;
  backSurface, totalSurface, penSurface: Single;
begin
  if not PenVisible and not BackVisible then
    result := false
  else
  begin
    ab := GetAffineBox(AMatrix, true);
    backSurface := ab.Surface*Pi/4;
    if PenVisible then
    begin
      penSurface := (ab.Width+ab.Height)*(Pi/2)*PenWidth;
      if BackVisible then
        totalSurface:= backSurface+penSurface/2
      else
        totalSurface := penSurface;
    end else
      totalSurface := backSurface;
    result := totalSurface > 640*480;
  end;
end;

class function TEllipseShape.StorageClassName: RawByteString;
begin
  result := 'ellipse';
end;

{ TCustomRectShape }

procedure TCustomRectShape.DoMoveXAxis(ANewCoord: TPointF; AShift: TShiftState; AFactor: single);
var
  newSize: Single;
begin
  BeginUpdate;
  if (ssAlt in AShift) or (FXUnitBackup = PointF(0,0)) then
  begin
    FXAxis := FOriginBackup + AFactor*(ANewCoord - FOriginBackup);
    FYAxis := FYAxisBackup;
    FOrigin := FOriginBackup;
  end
  else
  begin
    newSize := AFactor*FXUnitBackup*(ANewCoord-FOriginBackup);
    if ssShift in AShift then
    begin
      FXAxis := FOriginBackup+FXUnitBackup*newSize;
      FYAxis := FYAxisBackup;
      FOrigin := FOriginBackup;
    end else
    begin
      FXAxis := FXAxisBackup + ((AFactor+1)*0.5)*(newSize-FXSizeBackup)*FXUnitBackup;
      FYAxis := FYAxisBackup + AFactor*(newSize-FXSizeBackup)*0.5*FXUnitBackup;
      FOrigin := FOriginBackup + AFactor*(newSize-FXSizeBackup)*0.5*FXUnitBackup;
    end;
  end;
  EndUpdate;
end;

procedure TCustomRectShape.DoMoveYAxis(ANewCoord: TPointF; AShift: TShiftState;
  AFactor: single);
var
  newSizeY: Single;
begin
  BeginUpdate;
  if (ssAlt in AShift) or (FYUnitBackup = PointF(0,0)) then
  begin
    FYAxis := FOriginBackup + AFactor*(ANewCoord - FOriginBackup);
    FXAxis := FXAxisBackup;
    FOrigin := FOriginBackup;
  end
  else
  begin
    newSizeY := AFactor*FYUnitBackup*(ANewCoord-FOriginBackup);
    if ssShift in AShift then
    begin
      FYAxis := FOriginBackup+FYUnitBackup*newSizeY;
      FXAxis := FXAxisBackup;
      FOrigin := FOriginBackup;
    end else
    begin
      FYAxis := FYAxisBackup + ((AFactor+1)*0.5)*(newSizeY-FYSizeBackup)*FYUnitBackup;
      FXAxis := FXAxisBackup + AFactor*(newSizeY-FYSizeBackup)*0.5*FYUnitBackup;
      FOrigin := FOriginBackup + AFactor*(newSizeY-FYSizeBackup)*0.5*FYUnitBackup;
    end;
  end;
  EndUpdate;
end;

procedure TCustomRectShape.DoMoveXYCorner(ANewCoord: TPointF;
  AShift: TShiftState; AFactorX, AFactorY: single);
var
  ratio, d, newScale, prevScale, scale: single;
  m: TAffineMatrix;
  newSize, prevCornerVect, newCornerVect, u1,v1,u2,v2: TPointF;
begin
  BeginUpdate;
  if (ssAlt in AShift) and (VectDet(FXUnitBackup,FYUnitBackup)<>0) and (FXSizeBackup<>0) and (FYSizeBackup<>0) then
  begin
    prevCornerVect := AFactorX*(FXAxisBackup - FOriginBackup) + AFactorY*(FYAxisBackup - FOriginBackup);
    newCornerVect := (ANewCoord - FOriginBackup)*(1/GetCornerPositition);
    newScale := VectLen(newCornerVect);
    prevScale := VectLen(prevCornerVect);
    if (prevScale > 0) then
    begin
      prevCornerVect *= 1/prevScale;
      if newScale > 0 then newCornerVect *= 1/newScale;
      scale := newScale/prevScale;

      u1 := prevCornerVect;
      v1 := PointF(-u1.y,u1.x);

      u2 := PointF(newCornerVect*u1, newCornerVect*v1);
      v2 := PointF(-u2.y,u2.x);

      m := AffineMatrixTranslation(FOriginBackup.x,FOriginBackup.y)*
           AffineMatrixScale(scale,scale)*
           AffineMatrix(u2,v2,PointF(0,0))*
           AffineMatrixTranslation(-FOriginBackup.x,-FOriginBackup.y);
      FOrigin := FOriginBackup;
      FXAxis := m * FXAxisBackup;
      FYAxis := m * FYAxisBackup;
    end;
  end else
  begin
    d := GetCornerPositition;
    m := AffineMatrixInverse(AffineMatrix(AFactorX*FXUnitBackup*d,AFactorY*FYUnitBackup*d,FOriginBackup));
    newSize := m*ANewCoord;
    if (ssShift in AShift) and (FXSizeBackup <> 0) and (FYSizeBackup <> 0) then
    begin
      ratio := (newSize.X/FXSizeBackup + newSize.Y/FYSizeBackup)/2;
      newSize.X := ratio*FXSizeBackup;
      newSize.Y := ratio*FYSizeBackup;
    end;
    FXAxis := FXAxisBackup + (AFactorX+1)*0.5*sqrt(d)*(newSize.X-FXSizeBackup)*FXUnitBackup + AFactorY*(newSize.Y-FYSizeBackup)*0.5*sqrt(d)*FYUnitBackup;
    FYAxis := FYAxisBackup + (AFactorY+1)*0.5*sqrt(d)*(newSize.Y-FYSizeBackup)*FYUnitBackup + AFactorX*(newSize.X-FXSizeBackup)*0.5*sqrt(d)*FXUnitBackup;
    FOrigin := FOriginBackup + AFactorX*(newSize.X-FXSizeBackup)*0.5*sqrt(d)*FXUnitBackup
                             + AFactorY*(newSize.Y-FYSizeBackup)*0.5*sqrt(d)*FYUnitBackup;
  end;
  EndUpdate;
end;

procedure TCustomRectShape.OnMoveOrigin(ASender: TObject; APrevCoord,
  ANewCoord: TPointF; AShift: TShiftState);
var
  delta: TPointF;
begin
  BeginUpdate;
  delta := ANewCoord - FOrigin;
  FOrigin := ANewCoord;
  FXAxis += delta;
  FYAxis += delta;
  EndUpdate;
end;

procedure TCustomRectShape.OnMoveXAxis(ASender: TObject; APrevCoord,
  ANewCoord: TPointF; AShift: TShiftState);
begin
  DoMoveXAxis(ANewCoord, AShift, 1);
end;

procedure TCustomRectShape.OnMoveYAxis(ASender: TObject; APrevCoord,
  ANewCoord: TPointF; AShift: TShiftState);
begin
  DoMoveYAxis(ANewCoord, AShift, 1);
end;

procedure TCustomRectShape.OnMoveXAxisNeg(ASender: TObject; APrevCoord,
  ANewCoord: TPointF; AShift: TShiftState);
begin
  DoMoveXAxis(ANewCoord, AShift, -1);
end;

procedure TCustomRectShape.OnMoveYAxisNeg(ASender: TObject; APrevCoord,
  ANewCoord: TPointF; AShift: TShiftState);
begin
  DoMoveYAxis(ANewCoord, AShift, -1);
end;

procedure TCustomRectShape.OnMoveXYCorner(ASender: TObject; APrevCoord,
  ANewCoord: TPointF; AShift: TShiftState);
begin
  DoMoveXYCorner(ANewCoord, AShift, 1, 1);
end;

procedure TCustomRectShape.OnMoveXNegYCorner(ASender: TObject; APrevCoord,
  ANewCoord: TPointF; AShift: TShiftState);
begin
  DoMoveXYCorner(ANewCoord, AShift, -1, 1);
end;

procedure TCustomRectShape.OnMoveXYNegCorner(ASender: TObject; APrevCoord,
  ANewCoord: TPointF; AShift: TShiftState);
begin
  DoMoveXYCorner(ANewCoord, AShift, 1, -1);
end;

procedure TCustomRectShape.OnMoveXNegYNegCorner(ASender: TObject; APrevCoord,
  ANewCoord: TPointF; AShift: TShiftState);
begin
  DoMoveXYCorner(ANewCoord, AShift, -1, -1);
end;

procedure TCustomRectShape.OnStartMove(ASender: TObject; APointIndex: integer;
  AShift: TShiftState);
begin
  FOriginBackup := FOrigin;
  FXAxisBackup := FXAxis;
  FXUnitBackup := FXAxis-FOrigin;
  FXSizeBackup := VectLen(FXUnitBackup);
  if FXSizeBackup <> 0 then FXUnitBackup := (1/FXSizeBackup)*FXUnitBackup;
  FYAxisBackup := FYAxis;
  FYUnitBackup := FYAxis-FOrigin;
  FYSizeBackup := VectLen(FYUnitBackup);
  if FYSizeBackup <> 0 then FYUnitBackup := (1/FYSizeBackup)*FYUnitBackup;
end;

function TCustomRectShape.GetAffineBox(AMatrix: TAffineMatrix; APixelCentered: boolean): TAffineBox;
var
  m: TAffineMatrix;
begin
  if not APixelCentered then
    m := AffineMatrixTranslation(0.5,0.5) * AMatrix
  else
    m := AMatrix;
  result := m * TAffineBox.AffineBox(FOrigin - (FXAxis - FOrigin) - (FYAxis - FOrigin),
      FXAxis - (FYAxis - FOrigin), FYAxis - (FXAxis - FOrigin));
end;

function TCustomRectShape.GetOrthoRect(AMatrix: TAffineMatrix; out ARect: TRectF): boolean;
var
  sx,sy: single;
  o,ox,oy: TPointF;
begin
  o := AMatrix*FOrigin;
  ox := AMatrix*FXAxis;
  oy := AMatrix*FYAxis;
  if (abs(ox.y-o.y)<1e-4) and (abs(oy.x-o.x)<1e-4) then
  begin
    sx := abs(ox.x-o.x);
    sy := abs(oy.y-o.y);
    ARect := RectF(o.x - sx, o.y - sy, o.x + sx, o.y + sy);
    exit(true);
  end else
  begin
    ARect := EmptyRectF;
    exit(false);
  end;
end;

procedure TCustomRectShape.QuickDefine(const APoint1, APoint2: TPointF);
begin
  BeginUpdate;
  FOrigin := (APoint1+APoint2)*0.5;
  FXAxis := PointF(APoint2.X,FOrigin.Y);
  FYAxis := PointF(FOrigin.X,APoint2.Y);
  EndUpdate;
end;

procedure TCustomRectShape.LoadFromStorage(AStorage: TBGRACustomOriginalStorage);
begin
  BeginUpdate;
  inherited LoadFromStorage(AStorage);
  FOrigin := AStorage.PointF['origin'];
  FXAxis := AStorage.PointF['x-axis'];
  FYAxis := AStorage.PointF['y-axis'];
  EndUpdate;
end;

procedure TCustomRectShape.SaveToStorage(AStorage: TBGRACustomOriginalStorage);
begin
  inherited SaveToStorage(AStorage);
  AStorage.PointF['origin'] := FOrigin;
  AStorage.PointF['x-axis'] := FXAxis;
  AStorage.PointF['y-axis'] := FYAxis;
end;

function TCustomRectShape.GetRenderBounds(ADestRect: TRect; AMatrix: TAffineMatrix): TRectF;
begin
  result := GetAffineBox(AMatrix, false).RectBoundsF;
end;

procedure TCustomRectShape.ConfigureEditor(AEditor: TBGRAOriginalEditor);
var
  d: Single;
  u, v: TPointF;
begin
  u := FXAxis - FOrigin;
  v := FYAxis - FOrigin;
  AEditor.AddStartMoveHandler(@OnStartMove);
  AEditor.AddPoint(FOrigin, @OnMoveOrigin, true);
  AEditor.AddArrow(FOrigin, FXAxis, @OnMoveXAxis);
  AEditor.AddArrow(FOrigin, FYAxis, @OnMoveYAxis);
  AEditor.AddArrow(FOrigin, FOrigin - u, @OnMoveXAxisNeg);
  AEditor.AddArrow(FOrigin, FOrigin - v, @OnMoveYAxisNeg);
  d := GetCornerPositition;
  if d <> 0 then
  begin
    AEditor.AddPoint(FOrigin + (u+v)*d, @OnMoveXYCorner, false);
    AEditor.AddPoint(FOrigin + (-u+v)*d, @OnMoveXNegYCorner, false);
    AEditor.AddPoint(FOrigin + (u-v)*d, @OnMoveXYNegCorner, false);
    AEditor.AddPoint(FOrigin + (-u-v)*d, @OnMoveXNegYNegCorner, false);
  end;
end;

{ TRectShape }

function TRectShape.PenVisible: boolean;
begin
  result := (PenWidth>0) and (PenColor.alpha>0) and not IsClearPenStyle(PenStyle);
end;

function TRectShape.BackVisible: boolean;
begin
  result := BackColor.alpha <> 0;
end;

function TRectShape.GetCornerPositition: single;
begin
  result := 1;
end;

function TRectShape.GetIsSlow(AMatrix: TAffineMatrix): boolean;
var
  ab: TAffineBox;
  backSurface, totalSurface, penSurface: Single;
begin
  if not PenVisible and not BackVisible then
    result := false
  else
  begin
    ab := GetAffineBox(AMatrix, true);
    backSurface := ab.Surface;
    if PenVisible then
    begin
      penSurface := (ab.Width+ab.Height)*2*PenWidth;
      if BackVisible then
        totalSurface:= backSurface+penSurface/2
      else
        totalSurface := penSurface;
    end else
      totalSurface := backSurface;
    result := totalSurface > 800*600;
  end;
end;

class function TRectShape.Fields: TVectorShapeFields;
begin
  Result:= [vsfPenColor, vsfPenWidth, vsfPenStyle, vsfJoinStyle, vsfBackColor];
end;

procedure TRectShape.Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix;
  ADraft: boolean);
var
  pts: Array of TPointF;
  orthoRect: TRectF;
begin
  pts := GetAffineBox(AMatrix, true).AsPolygon;
  If BackVisible then
  begin
    if GetOrthoRect(AMatrix, orthoRect) then
    begin
      if ADraft then
        ADest.FillRect(round(orthoRect.Left),round(orthoRect.Top),round(orthoRect.Right),round(orthoRect.Bottom), BackColor, dmDrawWithTransparency)
      else
        ADest.FillRectAntialias(orthoRect, BackColor);
    end else
    begin
      if ADraft then
        ADest.FillPoly(pts, BackColor, dmDrawWithTransparency)
      else
        ADest.FillPolyAntialias(pts, BackColor);
    end;
  end;
  if PenVisible then
  begin
    pts := ComputeStroke(pts,true, AMatrix);
    if ADraft and (PenWidth > 4) then
      ADest.FillPoly(pts, PenColor, dmDrawWithTransparency)
    else
      ADest.FillPolyAntialias(pts, PenColor);
  end;
end;

function TRectShape.GetRenderBounds(ADestRect: TRect; AMatrix: TAffineMatrix): TRectF;
var
  i: Integer;
  pts: ArrayOfTPointF;
  xMargin, yMargin: single;
begin
  if not BackVisible and not PenVisible then
    result:= EmptyRectF
  else
  begin
    result := inherited GetRenderBounds(ADestRect, AMatrix);
    if PenVisible then
    begin
      if (JoinStyle <> pjsMiter) or (Stroker.MiterLimit <= 1) then
      begin
        xMargin := (abs(AMatrix[1,1])+abs(AMatrix[1,2]))*PenWidth*0.5;
        yMargin := (abs(AMatrix[2,1])+abs(AMatrix[2,2]))*PenWidth*0.5;
        result.Left -= xMargin;
        result.Top -= yMargin;
        result.Right += xMargin;
        result.Bottom += yMargin;
      end else
      begin
        pts := ComputeStroke(GetAffineBox(AMatrix, false).AsPolygon, true, AMatrix);
        for i := 0 to high(pts) do
        begin
          if pts[i].x < result.Left then result.Left := pts[i].x;
          if pts[i].x > result.Right then result.Right := pts[i].x;
          if pts[i].y < result.Top then result.Top := pts[i].y;
          if pts[i].y > result.Bottom then result.Bottom := pts[i].y;
        end;
      end;
    end;
  end;
end;

function TRectShape.PointInShape(APoint: TPointF): boolean;
var
  pts: ArrayOfTPointF;
  box: TAffineBox;
begin
  box := GetAffineBox(AffineMatrixIdentity, true);
  if BackVisible and box.Contains(APoint) then
    result := true else
  if PenVisible then
  begin
    pts := ComputeStroke(box.AsPolygon, true, AffineMatrixIdentity);
    result:= IsPointInPolygon(pts, APoint, true);
  end else
    result := false;
end;

class function TRectShape.StorageClassName: RawByteString;
begin
  result := 'rect';
end;

{ TVectorShape }

function TVectorShape.GetIsSlow(AMatrix: TAffineMatrix): boolean;
begin
  result := false;
end;

class function TVectorShape.Fields: TVectorShapeFields;
begin
  result := [];
end;

function TVectorShape.GetJoinStyle: TPenJoinStyle;
begin
  result := Stroker.JoinStyle;
end;

procedure TVectorShape.SetJoinStyle(AValue: TPenJoinStyle);
begin
  BeginUpdate;
  Stroker.JoinStyle := AValue;
  EndUpdate;
end;

procedure TVectorShape.SetUsermode(AValue: TVectorShapeUsermode);
begin
  if FUsermode=AValue then Exit;
  FUsermode:=AValue;
  if Assigned(FOnEditingChange) then FOnEditingChange(self);
end;

class function TVectorShape.Usermodes: TVectorShapeUsermodes;
begin
  result := [vsuEdit];
end;

procedure TVectorShape.BeginUpdate;
begin
  if FUpdateCount = 0 then
    FBoundsBeforeUpdate := GetRenderBounds(InfiniteRect, AffineMatrixIdentity);
  FUpdateCount += 1;
end;

procedure TVectorShape.EndUpdate;
var
  boundsAfter: TRectF;
begin
  if FUpdateCount > 0 then
  begin
    FUpdateCount -= 1;
    if FUpdateCount = 0 then
    begin
      if Assigned(FOnChange) then
      begin
        boundsAfter := GetRenderBounds(InfiniteRect, AffineMatrixIdentity);
        FOnChange(self, boundsAfter.Union(FBoundsBeforeUpdate, true));
      end;
    end;
  end;
end;

function TVectorShape.GetPenColor: TBGRAPixel;
begin
  result := FPenColor;
end;

function TVectorShape.GetPenWidth: single;
begin
  result := FPenWidth;
end;

function TVectorShape.GetPenStyle: TBGRAPenStyle;
begin
  result := Stroker.CustomPenStyle;
end;

function TVectorShape.GetBackColor: TBGRAPixel;
begin
  result := FBackColor;
end;

function TVectorShape.ComputeStroke(APoints: ArrayOfTPointF; AClosed: boolean; AStrokeMatrix: TAffineMatrix): ArrayOfTPointF;
begin
  Stroker.StrokeMatrix := AStrokeMatrix;
  if AClosed then
    result := Stroker.ComputePolygon(APoints, PenWidth)
  else
    result := Stroker.ComputePolyline(APoints, PenWidth, PenColor);
end;

function TVectorShape.GetStroker: TBGRAPenStroker;
begin
  if FStroker = nil then FStroker := TBGRAPenStroker.Create;
  result := FStroker;
end;

procedure TVectorShape.SetPenColor(AValue: TBGRAPixel);
begin
  if AValue.alpha = 0 then AValue := BGRAPixelTransparent;
  if FPenColor = AValue then exit;
  BeginUpdate;
  FPenColor := AValue;
  EndUpdate;
end;

procedure TVectorShape.SetPenWidth(AValue: single);
begin
  if AValue < 0 then AValue := 0;
  if FPenWidth = AValue then exit;
  BeginUpdate;
  FPenWidth := AValue;
  EndUpdate;
end;

procedure TVectorShape.SetPenStyle(AValue: TBGRAPenStyle);
begin
  BeginUpdate;
  Stroker.CustomPenStyle := AValue;
  EndUpdate;
end;

procedure TVectorShape.SetBackColor(AValue: TBGRAPixel);
begin
  if AValue.alpha = 0 then AValue := BGRAPixelTransparent;
  if FBackColor = AValue then exit;
  BeginUpdate;
  FBackColor := AValue;
  EndUpdate;
end;

constructor TVectorShape.Create;
begin
  FPenColor := BGRAPixelTransparent;
  FPenWidth := 1;
  FBackColor := BGRAPixelTransparent;
  FStroker := nil;
  FOnChange := nil;
  FOnEditingChange := nil;
  FUsermode:= vsuEdit;
end;

destructor TVectorShape.Destroy;
begin
  FreeAndNil(FStroker);
  inherited Destroy;
end;

procedure TVectorShape.LoadFromStorage(AStorage: TBGRACustomOriginalStorage);
var
  f: TVectorShapeFields;
begin
  f := Fields;
  if f <> [] then
  begin
    BeginUpdate;
    if vsfPenColor in f then PenColor := AStorage.Color['pen-color'];
    if vsfPenWidth in f then PenWidth := AStorage.FloatDef['pen-width', 0];
    if vsfPenStyle in f then PenStyle := AStorage.FloatArray['pen-style'];
    if vsfJoinStyle in f then
      case AStorage.RawString['join-style'] of
      'round': JoinStyle := pjsRound;
      'bevel': JoinStyle := pjsBevel;
      else JoinStyle := pjsMiter;
      end;
    if vsfBackColor in f then BackColor := AStorage.Color['back-color'];
    EndUpdate;
  end;
end;

procedure TVectorShape.SaveToStorage(AStorage: TBGRACustomOriginalStorage);
var
  f: TVectorShapeFields;
begin
  f := Fields;
  if vsfPenColor in f then AStorage.Color['pen-color'] := PenColor;
  if vsfPenWidth in f then AStorage.Float['pen-width'] := PenWidth;
  if vsfPenStyle in f then AStorage.FloatArray['pen-style'] := PenStyle;
  if vsfJoinStyle in f then
    case JoinStyle of
    pjsRound: AStorage.RawString['join-style'] := 'round';
    pjsBevel: AStorage.RawString['join-style'] := 'bevel';
    else AStorage.RawString['join-style'] := 'miter';
    end;
  if vsfBackColor in f then AStorage.Color['back-color'] := BackColor;
end;

procedure TVectorShape.MouseMove(Shift: TShiftState; X, Y: single; out
  ACursor: TOriginalEditorCursor; out AHandled: boolean);
begin
  //nothing
end;

procedure TVectorShape.MouseDown(RightButton: boolean; Shift: TShiftState; X,
  Y: single; out ACursor: TOriginalEditorCursor; out AHandled: boolean);
begin
  //nothing
end;

procedure TVectorShape.MouseUp(RightButton: boolean; Shift: TShiftState; X,
  Y: single; out ACursor: TOriginalEditorCursor; out AHandled: boolean);
begin
  //nothing
end;

{ TVectorOriginal }

procedure TVectorOriginal.FreeDeletedShapes;
var
  i: Integer;
begin
  for i := 0 to FDeletedShapes.Count-1 do
    FDeletedShapes[i].Free;
  FDeletedShapes.Clear
end;

procedure TVectorOriginal.OnShapeChange(ASender: TObject; ABounds: TRectF);
begin
  if ASender <> FSelectedShape then DiscardFrozenShapes;
  NotifyChange(ABounds);
end;

procedure TVectorOriginal.OnShapeEditingChange(ASender: TObject);
begin
  NotifyEditorChange;
end;

procedure TVectorOriginal.DiscardFrozenShapes;
begin
  FFrozenShapesComputed:= false;
  FreeAndNil(FFrozenShapesUnderSelection);
  FreeAndNil(FFrozenShapesOverSelection);
end;

constructor TVectorOriginal.Create;
begin
  inherited Create;
  FShapes := TVectorShapes.Create;
  FDeletedShapes := TVectorShapes.Create;
  FSelectedShape := nil;
  FFrozenShapesUnderSelection := nil;
  FFrozenShapesOverSelection := nil;
  FFrozenShapesComputed:= false;
end;

destructor TVectorOriginal.Destroy;
begin
  FreeAndNil(FShapes);
  FreeAndNil(FDeletedShapes);
  FreeAndNil(FFrozenShapesUnderSelection);
  FreeAndNil(FFrozenShapesOverSelection);
  FSelectedShape := nil;
  inherited Destroy;
end;

procedure TVectorOriginal.Clear;
var
  i: Integer;
begin
  if FShapes.Count > 0 then
  begin
    for i := 0 to FShapes.Count-1 do
      FDeletedShapes.Add(FShapes[i]);
    FShapes.Clear;
    NotifyChange;
  end;
end;

function TVectorOriginal.AddShape(AShape: TVectorShape): integer;
begin
  result:= FShapes.Add(AShape);
  AShape.OnChange := @OnShapeChange;
  AShape.OnEditingChange := @OnShapeEditingChange;
  DiscardFrozenShapes;
  NotifyChange(AShape.GetRenderBounds(InfiniteRect, AffineMatrixIdentity));
end;

function TVectorOriginal.AddShape(AShape: TVectorShape;
  AUsermode: TVectorShapeUsermode): integer;
begin
  result := AddShape(AShape);
  AShape.Usermode:= AUsermode;
  SelectShape(result);
end;

function TVectorOriginal.RemoveShape(AShape: TVectorShape): boolean;
var
  idx: LongInt;
  r: TRectF;
begin
  idx := FShapes.IndexOf(AShape);
  if idx = -1 then exit(false);
  if AShape = SelectedShape then DeselectShape;
  AShape.OnChange := nil;
  AShape.OnEditingChange := nil;
  r := AShape.GetRenderBounds(InfiniteRect, AffineMatrixIdentity);
  FShapes.Delete(idx);
  FDeletedShapes.Add(AShape);
  DiscardFrozenShapes;
  NotifyChange(r);
end;

procedure TVectorOriginal.SelectShape(AIndex: integer);
begin
  if (AIndex < 0) or (AIndex >= FShapes.Count) then
    raise ERangeError.Create('Index out of bounds');
  SelectShape(FShapes[AIndex]);
end;

procedure TVectorOriginal.SelectShape(AShape: TVectorShape);
var
  prev: TVectorShape;
begin
  if FSelectedShape <> AShape then
  begin
    if AShape <> nil then
      if FShapes.IndexOf(AShape)=-1 then
        raise exception.Create('Shape not found');
    prev := FSelectedShape;
    if Assigned(prev) then prev.Usermode := vsuEdit;
    FSelectedShape := AShape;
    DiscardFrozenShapes;
    NotifyEditorChange;
    if Assigned(FOnSelectShape) then
      FOnSelectShape(self, FSelectedShape, prev);
  end;
end;

procedure TVectorOriginal.DeselectShape;
begin
  SelectShape(nil);
end;

procedure TVectorOriginal.MouseClick(APoint: TPointF);
var
  i: LongInt;
begin
  for i:= FShapes.Count-1 downto 0 do
    if FShapes[i].PointInShape(APoint) then
    begin
      SelectShape(i);
      exit;
    end;
  DeselectShape;
end;

procedure TVectorOriginal.Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix;
  ADraft: boolean);
var
  i: Integer;
  idxSelected: LongInt;
begin
  if AMatrix <> FFrozenShapeMatrix then DiscardFrozenShapes;
  idxSelected := FShapes.IndexOf(FSelectedShape);
  if idxSelected = -1 then
  begin
    FSelectedShape := nil;
    DiscardFrozenShapes;
  end;
  if FFrozenShapesComputed then
  begin
    ADest.PutImage(0,0,FFrozenShapesUnderSelection, dmSet);
    FSelectedShape.Render(ADest, AMatrix, ADraft);
    ADest.PutImage(0,0,FFrozenShapesOverSelection, dmDrawWithTransparency);
  end else
  begin
    if idxSelected <> -1 then
    begin
      if idxSelected > 0 then
      begin
        FreeAndNil(FFrozenShapesUnderSelection);
        FFrozenShapesUnderSelection := TBGRABitmap.Create(ADest.Width,ADest.Height);
        for i:= 0 to idxSelected-1 do
          FShapes[i].Render(FFrozenShapesUnderSelection, AMatrix, false);
        ADest.PutImage(0,0,FFrozenShapesUnderSelection, dmSet);
      end;
      FSelectedShape.Render(ADest, AMatrix, ADraft);
      if idxSelected < FShapes.Count-1 then
      begin
        FreeAndNil(FFrozenShapesOverSelection);
        FFrozenShapesOverSelection := TBGRABitmap.Create(ADest.Width,ADest.Height);
        for i:= idxSelected+1 to FShapes.Count-1 do
          FShapes[i].Render(FFrozenShapesOverSelection, AMatrix, false);
        ADest.PutImage(0,0,FFrozenShapesOverSelection, dmDrawWithTransparency);
      end;
      FFrozenShapesComputed := true;
      FFrozenShapeMatrix := AMatrix;
    end else
    begin
      for i:= 0 to FShapes.Count-1 do
        FShapes[i].Render(ADest, AMatrix, ADraft);
    end;
  end;
end;

procedure TVectorOriginal.ConfigureEditor(AEditor: TBGRAOriginalEditor);
begin
  inherited ConfigureEditor(AEditor);
  if Assigned(FSelectedShape) then
  begin
    if FShapes.IndexOf(FSelectedShape)=-1 then
    begin
      FSelectedShape := nil;
      DiscardFrozenShapes;
    end
    else
      FSelectedShape.ConfigureEditor(AEditor);
  end;
  //no more reference to event handlers
  FreeDeletedShapes;
end;

function TVectorOriginal.CreateEditor: TBGRAOriginalEditor;
begin
  Result:= TVectorOriginalEditor.Create(self);
end;

function TVectorOriginal.GetRenderBounds(ADestRect: TRect;
  AMatrix: TAffineMatrix): TRect;
var
  area, shapeArea: TRectF;
  i: Integer;
begin
  area:= EmptyRectF;

  for i:= 0 to FShapes.Count-1 do
  begin
    shapeArea := FShapes[i].GetRenderBounds(ADestRect, AMatrix);
    area := area.Union(shapeArea, true);
  end;

  if IsEmptyRectF(area) then
    result := EmptyRect
  else
    result := rect(floor(area.Left),floor(area.Top),ceil(area.Right),ceil(area.Bottom));
end;

procedure TVectorOriginal.LoadFromStorage(AStorage: TBGRACustomOriginalStorage);
var
  nb: LongInt;
  i: Integer;
  obj: TBGRACustomOriginalStorage;
  objClassName: String;
  shapeClass: TVectorShapeAny;
  shape: TVectorShape;
begin
  Clear;
  nb := AStorage.Int['count'];
  for i:= 0 to nb-1 do
  begin
    obj := AStorage.OpenObject('shape'+inttostr(i+1));
    if obj <> nil then
    begin
      objClassName := obj.RawString['class'];
      if objClassName = '' then raise exception.Create('Shape class not defined');
      shapeClass:= GetVectorShapeByStorageClassName(objClassName);
      if shapeClass = nil then raise exception.Create('Unknown shape class "'+objClassName+'"');
      shape := shapeClass.Create;
      shape.LoadFromStorage(obj);
      shape.OnChange := @OnShapeChange;
      shape.OnEditingChange := @OnShapeEditingChange;
      FShapes.Add(shape);
      obj.Free;
    end;
  end;
  NotifyChange;
end;

procedure TVectorOriginal.SaveToStorage(AStorage: TBGRACustomOriginalStorage);
var
  nb: LongInt;
  i: Integer;
  obj: TBGRACustomOriginalStorage;
begin
  nb := AStorage.Int['count'];
  for i := 0 to nb-1 do AStorage.RemoveObject('shape'+inttostr(i+1));
  AStorage.Int['count'] := 0;

  for i := 0 to FShapes.Count-1 do
  begin
    obj := AStorage.CreateObject('shape'+inttostr(i+1));
    try
      FShapes[i].SaveToStorage(obj);
      AStorage.Int['count'] := i+1;
    finally
      obj.Free;
    end;
  end;

end;

class function TVectorOriginal.StorageClassName: RawByteString;
begin
  result := 'vector';
end;

end.

