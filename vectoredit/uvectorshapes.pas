unit uvectorshapes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, uvectororiginal, BGRABitmapTypes, BGRALayerOriginal,
  BGRABitmap, BGRATransform, BGRAGradients;

type
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
    function AllowShearTransform: boolean; virtual;
  public
    procedure QuickDefine(const APoint1,APoint2: TPointF); override;
    procedure LoadFromStorage(AStorage: TBGRACustomOriginalStorage); override;
    procedure SaveToStorage(AStorage: TBGRACustomOriginalStorage); override;
    function GetRenderBounds({%H-}ADestRect: TRect; AMatrix: TAffineMatrix; {%H-}AOptions: TRenderBoundsOptions = []): TRectF; override;
    procedure ConfigureEditor(AEditor: TBGRAOriginalEditor); override;
  end;

  { TRectShape }

  TRectShape = class(TCustomRectShape)
  protected
    function PenVisible(AAssumePenFill: boolean = false): boolean;
    function BackVisible: boolean;
    function GetCornerPositition: single; override;
  public
    class function Fields: TVectorShapeFields; override;
    procedure Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix; ADraft: boolean); override;
    function GetRenderBounds({%H-}ADestRect: TRect; AMatrix: TAffineMatrix; AOptions: TRenderBoundsOptions = []): TRectF; override;
    function PointInShape(APoint: TPointF): boolean; override;
    function GetIsSlow(AMatrix: TAffineMatrix): boolean; override;
    class function StorageClassName: RawByteString; override;
  end;

  { TEllipseShape }

  TEllipseShape = class(TCustomRectShape)
  protected
    function PenVisible(AAssumePenFill: boolean = false): boolean;
    function BackVisible: boolean;
    function GetCornerPositition: single; override;
  public
    constructor Create(AContainer: TVectorOriginal); override;
    class function Fields: TVectorShapeFields; override;
    procedure Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix; ADraft: boolean); override;
    function GetRenderBounds({%H-}ADestRect: TRect; AMatrix: TAffineMatrix; AOptions: TRenderBoundsOptions = []): TRectF; override;
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

  TPhongShapeKind = (pskRectangle, pskRoundRectangle, pskHalfSphere, pskConeTop, pskConeSide,
                     pskHorizCylinder, pskVertCylinder);

const
  DefaultPhongShapeAltitudePercent = 20;
  DefaultPhongBorderSizePercent = 20;

type
  { TPhongShape }

  TPhongShape = class(TCustomRectShape)
  private
    FShapeKind: TPhongShapeKind;
    FLightPosition: TPointF;
    FShapeAltitudePercent: single;
    FBorderSizePercent: single;
    procedure OnMoveLightPos({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF;
      {%H-}AShift: TShiftState);
    procedure SetBorderSizePercent(AValue: single);
    procedure SetLightPosition(AValue: TPointF);
    procedure SetShapeAltitudePercent(AValue: single);
    procedure SetShapeKind(AValue: TPhongShapeKind);
    function BackVisible: boolean;
    function AllowShearTransform: boolean; override;
  public
    constructor Create(AContainer: TVectorOriginal); override;
    destructor Destroy; override;
    function GetCornerPositition: single; override;
    class function Fields: TVectorShapeFields; override;
    procedure ConfigureEditor(AEditor: TBGRAOriginalEditor); override;
    procedure LoadFromStorage(AStorage: TBGRACustomOriginalStorage); override;
    procedure SaveToStorage(AStorage: TBGRACustomOriginalStorage); override;
    procedure Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix; ADraft: boolean); override;
    function GetRenderBounds({%H-}ADestRect: TRect; AMatrix: TAffineMatrix; AOptions: TRenderBoundsOptions = []): TRectF; override;
    function PointInShape(APoint: TPointF): boolean; override;
    function GetIsSlow(AMatrix: TAffineMatrix): boolean; override;
    class function StorageClassName: RawByteString; override;
    property ShapeKind: TPhongShapeKind read FShapeKind write SetShapeKind;
    property LightPosition: TPointF read FLightPosition write SetLightPosition;
    property ShapeAltitudePercent: single read FShapeAltitudePercent write SetShapeAltitudePercent;
    property BorderSizePercent: single read FBorderSizePercent write SetBorderSizePercent;
  end;

implementation

uses BGRAPen, BGRAGraphics, BGRAFillInfo, BGRAPath, math;

function MatrixForPixelCentered(const AMatrix: TAffineMatrix): TAffineMatrix;
begin
  result := AffineMatrixTranslation(-0.5,-0.5) * AMatrix * AffineMatrixTranslation(0.5,0.5);
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

{ TCustomRectShape }

procedure TCustomRectShape.DoMoveXAxis(ANewCoord: TPointF; AShift: TShiftState; AFactor: single);
var
  newSize: Single;
  u: TPointF;
begin
  BeginUpdate;
  if AllowShearTransform and ((ssAlt in AShift) or (FXUnitBackup = PointF(0,0))) then
  begin
    FXAxis := FOriginBackup + AFactor*(ANewCoord - FOriginBackup);
    FYAxis := FYAxisBackup;
    FOrigin := FOriginBackup;
  end else
  if FXUnitBackup = PointF(0,0) then
  begin
    u := ANewCoord - FOriginBackup;
    FXAxis := FOriginBackup + u;
    FYAxis := FOriginBackup + PointF(-u.y,u.x);
    FOrigin := FOriginBackup;
  end else
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
  u: TPointF;
begin
  BeginUpdate;
  if AllowShearTransform and ((ssAlt in AShift) or (FYUnitBackup = PointF(0,0))) then
  begin
    FYAxis := FOriginBackup + AFactor*(ANewCoord - FOriginBackup);
    FXAxis := FXAxisBackup;
    FOrigin := FOriginBackup;
  end else
  if FYUnitBackup = PointF(0,0) then
  begin
    u := ANewCoord - FOriginBackup;
    FXAxis := FOriginBackup + PointF(u.y,-u.x);
    FYAxis := FOriginBackup + u;
    FOrigin := FOriginBackup;
  end else
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
  ratio, d: single;
  m: TAffineMatrix;
  newSize, prevCornerVect, newCornerVect: TPointF;
begin
  BeginUpdate;
  if (ssAlt in AShift) and (VectDet(FXUnitBackup,FYUnitBackup)<>0) and (FXSizeBackup<>0) and (FYSizeBackup<>0) then
  begin
    prevCornerVect := AFactorX*(FXAxisBackup - FOriginBackup) + AFactorY*(FYAxisBackup - FOriginBackup);
    newCornerVect := (ANewCoord - FOriginBackup)*(1/GetCornerPositition);
    m := AffineMatrixTranslation(FOriginBackup.x,FOriginBackup.y)*
         AffineMatrixScaledRotation(prevCornerVect, newCornerVect)*
         AffineMatrixTranslation(-FOriginBackup.x,-FOriginBackup.y);
    FOrigin := FOriginBackup;
    FXAxis := m * FXAxisBackup;
    FYAxis := m * FYAxisBackup;
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
  if vsfBackFill in Fields then
    BackFill.Transform(AffineMatrixTranslation(delta.x, delta.y));
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
    m := AffineMatrixTranslation(0.5,0.5) * MatrixForPixelCentered(AMatrix)
  else
    m := MatrixForPixelCentered(AMatrix);
  result := m * TAffineBox.AffineBox(FOrigin - (FXAxis - FOrigin) - (FYAxis - FOrigin),
      FXAxis - (FYAxis - FOrigin), FYAxis - (FXAxis - FOrigin));
end;

function TCustomRectShape.GetOrthoRect(AMatrix: TAffineMatrix; out ARect: TRectF): boolean;
var
  sx,sy: single;
  o,ox,oy: TPointF;
  m: TAffineMatrix;
begin
  m := MatrixForPixelCentered(AMatrix);
  o := m*FOrigin;
  ox := m*FXAxis;
  oy := m*FYAxis;
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

function TCustomRectShape.AllowShearTransform: boolean;
begin
  result := true;
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

function TCustomRectShape.GetRenderBounds(ADestRect: TRect; AMatrix: TAffineMatrix; AOptions: TRenderBoundsOptions): TRectF;
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
  AEditor.AddPoint(FOrigin, @OnMoveOrigin, true);
end;

{ TRectShape }

function TRectShape.PenVisible(AAssumePenFill: boolean): boolean;
begin
  result := (PenWidth>0) and not IsClearPenStyle(PenStyle) and ((PenColor.alpha>0) or AAssumePenFill);
end;

function TRectShape.BackVisible: boolean;
begin
  result := BackFill.IsGradient or BackFill.IsTexture or
            (BackFill.IsSolid and (BackFill.SolidColor.alpha <> 0));
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
    result := (totalSurface > 800*600) or ((totalSurface > 320*240) and BackFill.IsSlow(AMatrix));
  end;
end;

class function TRectShape.Fields: TVectorShapeFields;
begin
  Result:= [vsfPenColor, vsfPenWidth, vsfPenStyle, vsfJoinStyle, vsfBackFill];
end;

procedure TRectShape.Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix;
  ADraft: boolean);
var
  pts: Array of TPointF;
  orthoRect: TRectF;
  r: TRect;
  backScan: TBGRACustomScanner;
begin
  pts := GetAffineBox(AMatrix, true).AsPolygon;
  If BackVisible then
  begin
    if BackFill.IsSolid then backScan := nil
    else backScan := BackFill.CreateScanner(AMatrix, ADraft);

    if GetOrthoRect(AMatrix, orthoRect) then
    begin
      if ADraft then
      begin
        r:= rect(round(orthoRect.Left),round(orthoRect.Top),round(orthoRect.Right),round(orthoRect.Bottom));
        if Assigned(backScan) then
          ADest.FillRect(r, backScan, dmDrawWithTransparency) else
          ADest.FillRect(r, BackFill.SolidColor, dmDrawWithTransparency)
      end
      else
      begin
        if Assigned(backScan) then
          ADest.FillRectAntialias(orthoRect, backScan) else
          ADest.FillRectAntialias(orthoRect, BackFill.SolidColor);
      end;
    end else
    begin
      if ADraft then
      begin
        if Assigned(backScan) then
          ADest.FillPoly(pts, backScan, dmDrawWithTransparency) else
          ADest.FillPoly(pts, BackFill.SolidColor, dmDrawWithTransparency)
      end
      else
      begin
        if Assigned(backScan) then
          ADest.FillPolyAntialias(pts, backScan) else
          ADest.FillPolyAntialias(pts, BackFill.SolidColor);
      end;
    end;

    backScan.Free;
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

function TRectShape.GetRenderBounds(ADestRect: TRect; AMatrix: TAffineMatrix; AOptions: TRenderBoundsOptions): TRectF;
var
  i: Integer;
  pts: ArrayOfTPointF;
  xMargin, yMargin: single;
begin
  if not (BackVisible or (rboAssumeBackFill in AOptions)) and not PenVisible(rboAssumePenFill in AOptions) then
    result:= EmptyRectF
  else
  begin
    result := inherited GetRenderBounds(ADestRect, AMatrix, AOptions);
    if PenVisible(rboAssumePenFill in AOptions) then
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

{ TEllipseShape }

function TEllipseShape.PenVisible(AAssumePenFill: boolean): boolean;
begin
  result := (PenWidth>0) and not IsClearPenStyle(PenStyle) and ((PenColor.alpha>0) or AAssumePenFill);
end;

function TEllipseShape.BackVisible: boolean;
begin
  result := BackFill.IsGradient or BackFill.IsTexture or
            (BackFill.IsSolid and (BackFill.SolidColor.alpha <> 0));
end;

function TEllipseShape.GetCornerPositition: single;
begin
  result := sqrt(2)/2;
end;

constructor TEllipseShape.Create(AContainer: TVectorOriginal);
begin
  inherited Create(AContainer);
  inherited SetJoinStyle(pjsRound);
end;

class function TEllipseShape.Fields: TVectorShapeFields;
begin
  Result:= [vsfPenColor, vsfPenWidth, vsfPenStyle, vsfBackFill];
end;

procedure TEllipseShape.Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix;
  ADraft: boolean);
var
  pts: Array of TPointF;
  orthoRect: TRectF;
  center, radius: TPointF;
  draftPen, isOrtho: Boolean;
  r: TRect;
  backScan: TBGRACustomScanner;
  penZoom: Single;
  m: TAffineMatrix;
begin
  isOrtho := GetOrthoRect(AMatrix, orthoRect);
  if isOrtho then
  begin
    center := (orthoRect.TopLeft+orthoRect.BottomRight)*0.5;
    radius := (orthoRect.BottomRight-orthoRect.TopLeft)*0.5;
    If BackVisible then
    begin
      if BackFill.IsSolid then backScan := nil
      else backScan := BackFill.CreateScanner(AMatrix, ADraft);

      if ADraft then
      begin
        r := rect(round(orthoRect.Left),round(orthoRect.Top),round(orthoRect.Right),round(orthoRect.Bottom));
        if Assigned(backScan) then
          ADest.FillEllipseInRect(r, backScan, dmDrawWithTransparency) else
          ADest.FillEllipseInRect(r, BackFill.SolidColor, dmDrawWithTransparency)
      end
      else
      begin
        if Assigned(backScan) then
          ADest.FillEllipseAntialias(center.x, center.y, radius.x, radius.y, backScan) else
          ADest.FillEllipseAntialias(center.x, center.y, radius.x, radius.y, BackFill.SolidColor);
       end;

      backScan.Free;
    end;
    if PenVisible then
    begin
      if IsAffineMatrixScaledRotation(AMatrix) then
      begin
        penZoom := VectLen(AMatrix[1,1],AMatrix[2,1]);
        ADest.CustomPenStyle := PenStyle;
        draftPen := ADraft and (PenWidth > 4);
        if draftPen then
          ADest.Ellipse(center.x, center.y, radius.x, radius.y, PenColor, PenWidth*penZoom, dmDrawWithTransparency)
        else
          ADest.EllipseAntialias(center.x, center.y, radius.x, radius.y, PenColor, PenWidth*penZoom);
        ADest.PenStyle := psSolid;
      end else
      begin
        m:= MatrixForPixelCentered(AMatrix);
        pts := ComputeEllipse(m*FOrigin, m*FXAxis, m*FYAxis);
        pts := ComputeStroke(pts,true, AMatrix);
        if ADraft and (PenWidth > 4) then
          ADest.FillPoly(pts, PenColor, dmDrawWithTransparency)
        else
          ADest.FillPolyAntialias(pts, PenColor);
      end;
    end;
  end else
  begin
    m:= MatrixForPixelCentered(AMatrix);
    pts := ComputeEllipse(m*FOrigin, m*FXAxis, m*FYAxis);
    If BackVisible then
    begin
      if BackFill.IsSolid then backScan := nil
      else backScan := BackFill.CreateScanner(AMatrix, ADraft);

      if ADraft then
      begin
        if Assigned(backScan) then
          ADest.FillPoly(pts, backScan, dmDrawWithTransparency) else
          ADest.FillPoly(pts, BackFill.SolidColor, dmDrawWithTransparency)
      end
      else
      begin
        if Assigned(backScan) then
          ADest.FillPolyAntialias(pts, backScan) else
          ADest.FillPolyAntialias(pts, BackFill.SolidColor)
      end;

      backScan.Free;
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

function TEllipseShape.GetRenderBounds({%H-}ADestRect: TRect; AMatrix: TAffineMatrix; AOptions: TRenderBoundsOptions): TRectF;
var
  xMargin, yMargin: single;
begin
  if not (BackVisible or (rboAssumeBackFill in AOptions)) and not PenVisible(rboAssumePenFill in AOptions) then
    result:= EmptyRectF
  else
  begin
    result := inherited GetRenderBounds(ADestRect, AMatrix, AOptions);
    if PenVisible(rboAssumePenFill in AOptions) then
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
    result := (totalSurface > 640*480) or ((totalSurface > 320*240) and BackFill.IsSlow(AMatrix));
  end;
end;

class function TEllipseShape.StorageClassName: RawByteString;
begin
  result := 'ellipse';
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
  result := (PenWidth>0) and not IsClearPenStyle(PenStyle) and ((PenColor.alpha>0) or AAssumePenFill);
end;

function TPolylineShape.BackVisible: boolean;
begin
  result := BackFill.IsGradient or BackFill.IsTexture or
            (BackFill.IsSolid and (BackFill.SolidColor.alpha <> 0));
end;

class function TPolylineShape.Fields: TVectorShapeFields;
begin
  Result:= [vsfPenColor, vsfPenWidth, vsfPenStyle, vsfJoinStyle, vsfBackFill];
end;

procedure TPolylineShape.Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix;
  ADraft: boolean);
var
  pts: array of TPointF;
  backScan: TBGRACustomScanner;
begin
  if not BackVisible and not PenVisible then exit;
  pts := GetCurve(AMatrix);
  if BackVisible then
  begin
    if BackFill.IsSolid then backScan := nil
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
    pts := ComputeStroke(pts, Closed, AMatrix);
    if ADraft and (PenWidth > 4) then
      ADest.FillPoly(pts, PenColor, dmDrawWithTransparency)
    else
      ADest.FillPolyAntialias(pts, PenColor);
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

{ TPhongShape }

procedure TPhongShape.SetShapeKind(AValue: TPhongShapeKind);
begin
  if FShapeKind=AValue then Exit;
  BeginUpdate;
  FShapeKind:=AValue;
  EndUpdate;
end;

procedure TPhongShape.OnMoveLightPos(ASender: TObject; APrevCoord,
  ANewCoord: TPointF; AShift: TShiftState);
begin
  LightPosition := ANewCoord;
end;

procedure TPhongShape.SetBorderSizePercent(AValue: single);
begin
  if FBorderSizePercent=AValue then Exit;
  BeginUpdate;
  FBorderSizePercent:=AValue;
  EndUpdate;
end;

procedure TPhongShape.SetLightPosition(AValue: TPointF);
begin
  if FLightPosition=AValue then Exit;
  BeginUpdate;
  FLightPosition:=AValue;
  EndUpdate;
end;

procedure TPhongShape.SetShapeAltitudePercent(AValue: single);
begin
  if FShapeAltitudePercent=AValue then Exit;
  BeginUpdate;
  FShapeAltitudePercent:=AValue;
  EndUpdate;
end;

function TPhongShape.BackVisible: boolean;
begin
  result := BackFill.IsGradient or BackFill.IsTexture or
            (BackFill.IsSolid and (BackFill.SolidColor.alpha <> 0));
end;

function TPhongShape.AllowShearTransform: boolean;
begin
  Result:= false;
end;

constructor TPhongShape.Create(AContainer: TVectorOriginal);
begin
  inherited Create(AContainer);
  FShapeKind:= pskRectangle;
  FLightPosition := PointF(0,0);
  FShapeAltitudePercent:= DefaultPhongShapeAltitudePercent;
  FBorderSizePercent:= DefaultPhongBorderSizePercent;
end;

destructor TPhongShape.Destroy;
begin
  inherited Destroy;
end;

function TPhongShape.GetCornerPositition: single;
begin
  if ShapeKind in [pskHalfSphere,pskConeTop] then
    result := sqrt(2)/2
  else
    result := 1;
end;

class function TPhongShape.Fields: TVectorShapeFields;
begin
  Result:= [vsfBackFill];
end;

procedure TPhongShape.ConfigureEditor(AEditor: TBGRAOriginalEditor);
begin
  inherited ConfigureEditor(AEditor);
  AEditor.AddPoint(FLightPosition, @OnMoveLightPos, true);
end;

procedure TPhongShape.LoadFromStorage(AStorage: TBGRACustomOriginalStorage);
begin
  BeginUpdate;
  inherited LoadFromStorage(AStorage);
  LightPosition := AStorage.PointF['light-pos'];
  if isEmptyPointF(LightPosition) then LightPosition := PointF(0,0);
  case AStorage.RawString['shape-kind'] of
    'round-rectangle': ShapeKind:= pskRoundRectangle;
    'half-sphere': ShapeKind := pskHalfSphere;
    'cone-top': ShapeKind := pskConeTop;
    'cone-side': ShapeKind := pskConeSide;
    'horizontal-cylinder': ShapeKind := pskHorizCylinder;
    'vertical-cylinder': ShapeKind := pskVertCylinder;
  else
    {'rectangle'} ShapeKind:= pskRectangle;
  end;
  ShapeAltitudePercent := AStorage.FloatDef['shape-altitude-percent', DefaultPhongShapeAltitudePercent];
  if ShapeKind in[pskRectangle,pskRoundRectangle] then
    BorderSizePercent := AStorage.FloatDef['border-size-percent', DefaultPhongBorderSizePercent]
  else
    BorderSizePercent := DefaultPhongBorderSizePercent;
  EndUpdate;
end;

procedure TPhongShape.SaveToStorage(AStorage: TBGRACustomOriginalStorage);
begin
  inherited SaveToStorage(AStorage);
  AStorage.PointF['light-pos'] := LightPosition;
  case ShapeKind of
    pskRectangle: AStorage.RawString['shape-kind'] := 'rectangle';
    pskRoundRectangle: AStorage.RawString['shape-kind'] := 'round-rectangle';
    pskHalfSphere: AStorage.RawString['shape-kind'] := 'half-sphere';
    pskConeTop: AStorage.RawString['shape-kind'] := 'cone-top';
    pskConeSide: AStorage.RawString['shape-kind'] := 'cone-side';
    pskHorizCylinder: AStorage.RawString['shape-kind'] := 'horizontal-cylinder';
    pskVertCylinder: AStorage.RawString['shape-kind'] := 'vertical-cylinder';
  end;
  AStorage.Float['shape-altitude-percent'] := ShapeAltitudePercent;
  if ShapeKind in[pskRectangle,pskRoundRectangle] then
    AStorage.Float['border-size-percent'] := FBorderSizePercent;
end;

procedure TPhongShape.Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix;
  ADraft: boolean);
var
  ab,abRaster: TAffineBox;
  mapWidth,mapHeight: integer;
  shader: TPhongShading;
  approxFactor,borderSize: single;
  m: TAffineMatrix;
  h: single;
  map,raster: TBGRABitmap;
  u,v,lightPosF: TPointF;
  scan: TBGRACustomScanner;
  rectRenderF,rectRasterF: TRectF;
  rectRender,rectRaster, prevClip: TRect;
begin
  if not BackVisible then exit;

  //determine final render bounds
  rectRenderF := GetRenderBounds(InfiniteRect,AMatrix);
  if IsEmptyRectF(rectRenderF) then exit;

  rectRender := rect(floor(rectRenderF.Left),floor(rectRenderF.Top),ceil(rectRenderF.Right),ceil(rectRenderF.Bottom));
  rectRender.Intersect(ADest.ClipRect);
  if IsRectEmpty(rectRender) then exit;

  //determine map size before transform
  ab := GetAffineBox(AMatrix, false);
  if ab.Width > ab.Height then
  begin
    if ab.Width = 0 then exit;
    mapWidth := ceil(ab.Width);
    mapHeight := ceil(ab.Surface/ab.Width);
  end else
  begin
    mapWidth := ceil(ab.Surface/ab.Height);
    mapHeight := ceil(ab.Height);
  end;
  approxFactor := 1;
  if ADraft then
  begin
    if mapWidth > 300 then approxFactor:= min(approxFactor, 300/mapWidth);
    if mapHeight > 300 then approxFactor:= min(approxFactor, 300/mapHeight);
  end;
  mapWidth:= ceil(mapWidth*approxFactor);
  mapHeight:= ceil(mapHeight*approxFactor);

  //determine map transform
  u := (ab.TopRight-ab.TopLeft)*(1/ab.Width);
  v := (ab.BottomLeft-ab.TopLeft)*(1/ab.Height);
  m := AffineMatrix(u,v,ab.TopLeft)*AffineMatrixScale(ab.Width/mapWidth,ab.Height/mapHeight);
  borderSize := FBorderSizePercent/200*min(ab.Width,ab.Height);

  try
    //create height map
    map := nil;

    case ShapeKind of
      pskRoundRectangle: begin
        map := CreateRoundRectanglePreciseMap(mapWidth,mapHeight,
                      round(borderSize*mapWidth/ab.Width),
                      round(borderSize*mapHeight/ab.Height),[]);
        h := FShapeAltitudePercent*approxFactor;
      end;
      pskHalfSphere: begin
        map := CreateSpherePreciseMap(mapWidth,mapHeight);
        h := FShapeAltitudePercent/100*sqrt(mapWidth*mapHeight);
      end;
      pskConeTop: begin
        map := CreateConePreciseMap(mapWidth,mapHeight);
        h := FShapeAltitudePercent/100*sqrt(mapWidth*mapHeight);
      end;
      pskConeSide: begin
        map := CreateVerticalConePreciseMap(mapWidth,mapHeight);
        h := FShapeAltitudePercent/100*mapWidth;
      end;
      pskHorizCylinder: begin
        map := CreateHorizontalCylinderPreciseMap(mapWidth,mapHeight);
        h := FShapeAltitudePercent/100*mapHeight;
      end;
      pskVertCylinder: begin
        map := CreateVerticalCylinderPreciseMap(mapWidth,mapHeight);
        h := FShapeAltitudePercent/100*mapWidth;
      end;
    else
      {pskRectangle: }begin
        map := CreateRectanglePreciseMap(mapWidth,mapHeight,
                      round(borderSize*mapWidth/ab.Width),
                      round(borderSize*mapHeight/ab.Height),[]);
        h := FShapeAltitudePercent*approxFactor;
      end;
    end;

    abRaster := AffineMatrixInverse(m)*TAffineBox.AffineBox(rectRenderF);
    rectRasterF := abRaster.RectBoundsF;
    rectRaster := rect(floor(rectRasterF.Left),floor(rectRasterF.Top),ceil(rectRasterF.Right),ceil(rectRasterF.Bottom));

    raster := nil;
    shader := nil;
    if IntersectRect(rectRaster, rectRaster, rect(0,0,mapWidth,mapHeight)) then
    try
      shader:= TPhongShading.Create;
      lightPosF := AffineMatrixTranslation(-rectRaster.Left,-rectRaster.Top)
                    *AffineMatrixInverse(m)*AMatrix
                    *PointF(FLightPosition.x,FLightPosition.y);
      shader.LightPosition := Point(round(lightPosF.x),round(lightPosF.y));
      shader.LightPositionZ := round(100*power(approxFactor,1.18));
      if h*3/2 > shader.LightPositionZ then
       shader.LightPositionZ := round(h*3/2);

      raster := TBGRABitmap.Create(rectRaster.Width,rectRaster.Height);
      if BackFill.IsSolid then
        shader.Draw(raster,map,round(h),-rectRaster.Left,-rectRaster.Top,BackFill.SolidColor)
      else
      begin
        scan := BackFill.CreateScanner(AffineMatrixTranslation(-rectRaster.left,-rectRaster.top)*AffineMatrixInverse(m)*AMatrix,ADraft);
        shader.DrawScan(raster,map,round(h),-rectRaster.Left,-rectRaster.Top,scan);
        scan.Free;
      end;

      prevClip := ADest.ClipRect;
      ADest.ClipRect := rectRender;
      if ADraft then
        ADest.PutImageAffine(m*AffineMatrixTranslation(rectRaster.Left,rectRaster.Top),raster,rfBox,dmDrawWithTransparency)
      else
        ADest.PutImageAffine(m*AffineMatrixTranslation(rectRaster.Left,rectRaster.Top),raster,rfHalfCosine,dmDrawWithTransparency);
      ADest.ClipRect := prevClip;

    finally
      raster.Free;
      shader.Free;
    end;
  finally
    map.Free;
  end;
end;

function TPhongShape.GetRenderBounds(ADestRect: TRect; AMatrix: TAffineMatrix;
  AOptions: TRenderBoundsOptions): TRectF;
begin
  if not (BackVisible or (rboAssumeBackFill in AOptions)) then
    result:= EmptyRectF
  else
    result := inherited GetRenderBounds(ADestRect, AMatrix, AOptions);
end;

function TPhongShape.PointInShape(APoint: TPointF): boolean;
var
  box: TAffineBox;
  pts: ArrayOfTPointF;
begin
  if not BackVisible then exit(false);
  if ShapeKind in [pskHalfSphere, pskConeTop] then
  begin
    pts := ComputeEllipse(FOrigin, FXAxis, FYAxis);
    result := IsPointInPolygon(pts, APoint, true);
  end else
  if ShapeKind = pskConeSide then
  begin
    pts:= PointsF([FOrigin - (FYAxis-FOrigin), FYAxis + (FXAxis-FOrigin), FYAxis - (FXAxis-FOrigin)]);
    result := IsPointInPolygon(pts, APoint, true);
  end else
  begin
    box := GetAffineBox(AffineMatrixIdentity, true);
    result:= box.Contains(APoint);
  end;
end;

function TPhongShape.GetIsSlow(AMatrix: TAffineMatrix): boolean;
var
  ab: TAffineBox;
begin
  if not BackVisible then exit(false);
  ab := GetAffineBox(AMatrix, true);
  result := ab.Surface > 320*240;
end;

class function TPhongShape.StorageClassName: RawByteString;
begin
  result := 'phong';
end;

initialization

  RegisterVectorShape(TRectShape);
  RegisterVectorShape(TEllipseShape);
  RegisterVectorShape(TPolylineShape);
  RegisterVectorShape(TCurveShape);
  RegisterVectorShape(TPhongShape);

end.

