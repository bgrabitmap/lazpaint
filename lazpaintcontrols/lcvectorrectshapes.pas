unit LCVectorRectShapes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, LCVectorOriginal, BGRABitmapTypes, BGRALayerOriginal,
  BGRABitmap, BGRATransform, BGRAGradients;

type
  { TCustomRectShape }

  TCustomRectShape = class(TVectorShape)
  protected
    FOrigin, FXAxis, FYAxis: TPointF;
    FOriginBackup,FXUnitBackup,FYUnitBackup,
    FXAxisBackup,FYAxisBackup: TPointF;
    FXSizeBackup,FYSizeBackup: single;
    FFixedRatio: single;
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
    function GetCornerPositition: single; virtual; abstract;
    function GetOrthoRect(AMatrix: TAffineMatrix; out ARect: TRectF): boolean;
    function AllowShearTransform: boolean; virtual;
    function ShowArrows: boolean; virtual;
    procedure SetOrigin(AValue: TPointF);
    function GetHeight: single;
    function GetWidth: single;
    procedure SetHeight(AValue: single);
    procedure SetWidth(AValue: single);
    procedure SetFixedRatio(AValue: single);
    procedure EnsureRatio(ACenterX,ACenterY: single);
  public
    procedure QuickDefine(const APoint1,APoint2: TPointF); override;
    function SuggestGradientBox(AMatrix: TAffineMatrix): TAffineBox; override;
    procedure LoadFromStorage(AStorage: TBGRACustomOriginalStorage); override;
    procedure SaveToStorage(AStorage: TBGRACustomOriginalStorage); override;
    function GetRenderBounds({%H-}ADestRect: TRect; AMatrix: TAffineMatrix; {%H-}AOptions: TRenderBoundsOptions = []): TRectF; override;
    procedure ConfigureCustomEditor(AEditor: TBGRAOriginalEditor); override;
    function GetAffineBox(AMatrix: TAffineMatrix; APixelCentered: boolean): TAffineBox;
    property Origin: TPointF read FOrigin write SetOrigin;
    property XAxis: TPointF read FXAxis;
    property YAxis: TPointF read FYAxis;
    property Width: single read GetWidth write SetWidth;
    property Height: single read GetHeight write SetHeight;
    property FixedRatio: single read FFixedRatio write SetFixedRatio;
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
  protected
    function AllowShearTransform: boolean; override;
  public
    constructor Create(AContainer: TVectorOriginal); override;
    destructor Destroy; override;
    function GetCornerPositition: single; override;
    class function Fields: TVectorShapeFields; override;
    class function PreferPixelCentered: boolean; override;
    procedure ConfigureCustomEditor(AEditor: TBGRAOriginalEditor); override;
    procedure MouseDown(RightButton: boolean; Shift: TShiftState; X, Y: single; var ACursor: TOriginalEditorCursor; var AHandled: boolean); override;
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

uses BGRAPen, BGRAGraphics, BGRAFillInfo, BGRAPath, math, LCVectorialFill;

{ TCustomRectShape }

procedure TCustomRectShape.SetOrigin(AValue: TPointF);
var
  delta: TPointF;
begin
  if FOrigin=AValue then Exit;
  BeginUpdate;
  delta := AValue - FOrigin;
  FOrigin := AValue;
  FXAxis += delta;
  FYAxis += delta;
  if vsfBackFill in Fields then
    BackFill.Transform(AffineMatrixTranslation(delta.x, delta.y));
  if vsfPenFill in Fields then
    PenFill.Transform(AffineMatrixTranslation(delta.x, delta.y));
  EndUpdate;
end;

function TCustomRectShape.GetHeight: single;
begin
  result := VectLen(YAxis-Origin);
end;

function TCustomRectShape.GetWidth: single;
begin
  result := VectLen(XAxis-Origin);
end;

procedure TCustomRectShape.SetHeight(AValue: single);
var u,v: TPointF;
  h,w: single;
begin
  h := GetHeight;
  if h <> 0 then v := (YAxis-Origin)*(1/h)
  else
  begin
    w := GetWidth;
    if w <> 0 then
    begin
      u := (XAxis-Origin)*(1/w);
      v := PointF(-u.y,u.x);
    end else
      v := PointF(0,1/2);
  end;
  FYAxis := Origin + v*AValue;
end;

procedure TCustomRectShape.SetWidth(AValue: single);
var u,v: TPointF;
  h,w: single;
begin
  w := GetWidth;
  if w <> 0 then u := (XAxis-Origin)*(1/w)
  else
  begin
    h := GetHeight;
    if h <> 0 then
    begin
      v := (YAxis-Origin)*(1/h);
      u := PointF(v.y,-v.x);
    end else
      u := PointF(1/2,0);
  end;
  FXAxis := Origin + u*AValue;
end;

procedure TCustomRectShape.EnsureRatio(ACenterX,ACenterY: single);
var
  h, w, curRatio,ratioFactor,fracPower: Single;
  refPoint, newRefPoint: TPointF;
begin
  if (FFixedRatio<>EmptySingle) and (FFixedRatio<>0) then
  begin
    h := Height;
    w := Width;
    if h = 0 then
      Height := w/FFixedRatio
    else if w = 0 then
      Width := h*FFixedRatio
    else
    begin
      curRatio := Width/Height;
      if FFixedRatio <> curRatio then
      begin
        ratioFactor := FFixedRatio/curRatio;
        BeginUpdate;
        refPoint := Origin + (XAxis-Origin)*ACenterX + (YAxis-Origin)*ACenterY;
        if (ACenterX=0) and (ACenterY=0) then fracPower := 1/2
        else fracPower := abs(ACenterY)/(abs(ACenterX)+abs(ACenterY));
        Width := Width*Power(ratioFactor, fracPower);
        if (ACenterX=0) and (ACenterY=0) then fracPower := 1/2
        else fracPower := abs(ACenterX)/(abs(ACenterX)+abs(ACenterY));
        Height := Height/Power(ratioFactor, fracPower);
        newRefPoint := Origin + (XAxis-Origin)*ACenterX + (YAxis-Origin)*ACenterY;
        Origin := Origin + (refPoint-newRefPoint);
        EndUpdate;
      end;
    end;
  end;
end;

procedure TCustomRectShape.SetFixedRatio(AValue: single);
begin
  if FFixedRatio=AValue then Exit;
  FFixedRatio:=AValue;
  EnsureRatio(0,0);
end;

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
  EnsureRatio(-AFactor,0);
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
  EnsureRatio(0,-AFactor);
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
    m := AffineMatrix(AFactorX*FXUnitBackup*d,AFactorY*FYUnitBackup*d,FOriginBackup);
    if IsAffineMatrixInversible(m) then
    begin
      m := AffineMatrixInverse(m);
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
  end;
  EnsureRatio(-AFactorX,-AFactorY);
  EndUpdate;
end;

procedure TCustomRectShape.OnMoveOrigin(ASender: TObject; APrevCoord,
  ANewCoord: TPointF; AShift: TShiftState);
begin
  Origin := ANewCoord;
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

function TCustomRectShape.ShowArrows: boolean;
begin
  result := true;
end;

procedure TCustomRectShape.QuickDefine(const APoint1, APoint2: TPointF);
begin
  BeginUpdate;
  FOrigin := (APoint1+APoint2)*0.5;
  FXAxis := PointF(APoint2.X,FOrigin.Y);
  FYAxis := PointF(FOrigin.X,APoint2.Y);
  EnsureRatio(-1,-1);
  EndUpdate;
end;

function TCustomRectShape.SuggestGradientBox(AMatrix: TAffineMatrix): TAffineBox;
begin
  Result:= GetAffineBox(AMatrix,False);
end;

procedure TCustomRectShape.LoadFromStorage(AStorage: TBGRACustomOriginalStorage);
begin
  BeginUpdate;
  inherited LoadFromStorage(AStorage);
  FOrigin := AStorage.PointF['origin'];
  FXAxis := AStorage.PointF['x-axis'];
  FYAxis := AStorage.PointF['y-axis'];
  FFixedRatio := AStorage.Float['fixed-ratio'];
  EndUpdate;
end;

procedure TCustomRectShape.SaveToStorage(AStorage: TBGRACustomOriginalStorage);
begin
  inherited SaveToStorage(AStorage);
  AStorage.PointF['origin'] := FOrigin;
  AStorage.PointF['x-axis'] := FXAxis;
  AStorage.PointF['y-axis'] := FYAxis;
  AStorage.Float['fixed-ratio'] := FFixedRatio;
end;

function TCustomRectShape.GetRenderBounds(ADestRect: TRect; AMatrix: TAffineMatrix; AOptions: TRenderBoundsOptions): TRectF;
begin
  result := GetAffineBox(AMatrix, false).RectBoundsF;
end;

procedure TCustomRectShape.ConfigureCustomEditor(AEditor: TBGRAOriginalEditor);
var
  d: Single;
  u, v: TPointF;
  idxOrig, idxX,idxY,idxXNeg,idxYNeg: Integer;
begin
  u := FXAxis - FOrigin;
  v := FYAxis - FOrigin;
  AEditor.AddStartMoveHandler(@OnStartMove);
  d := GetCornerPositition;
  if d <> 0 then
  begin
    AEditor.AddPoint(FOrigin + (u+v)*d, @OnMoveXYCorner, false);
    AEditor.AddPoint(FOrigin + (-u+v)*d, @OnMoveXNegYCorner, false);
    AEditor.AddPoint(FOrigin + (u-v)*d, @OnMoveXYNegCorner, false);
    AEditor.AddPoint(FOrigin + (-u-v)*d, @OnMoveXNegYNegCorner, false);
  end;
  if ShowArrows then
  begin
    idxX := AEditor.AddArrow(FOrigin, FXAxis, @OnMoveXAxis);
    idxY := AEditor.AddArrow(FOrigin, FYAxis, @OnMoveYAxis);
    idxXNeg := AEditor.AddArrow(FOrigin, FOrigin - u, @OnMoveXAxisNeg);
    idxYNeg := AEditor.AddArrow(FOrigin, FOrigin - v, @OnMoveYAxisNeg);
  end else
  begin
    idxX := AEditor.AddPoint(FXAxis, @OnMoveXAxis);
    idxY := AEditor.AddPoint(FYAxis, @OnMoveYAxis);
    idxXNeg := AEditor.AddPoint(FOrigin - u, @OnMoveXAxisNeg);
    idxYNeg := AEditor.AddPoint(FOrigin - v, @OnMoveYAxisNeg);
  end;
  idxOrig := AEditor.AddPoint(FOrigin, @OnMoveOrigin, true);
  if ShowArrows then
  begin
    AEditor.SetHitBox(idxX, TAffineBox.AffineBox(Origin + (XAxis-Origin)*0.667 - (YAxis-Origin)*0.667,
      Origin + (XAxis-Origin) - (YAxis-Origin)*0.667,
      Origin + (XAxis-Origin)*0.667 + (YAxis-Origin)*0.667) );
    AEditor.SetHitBox(idxY, TAffineBox.AffineBox(Origin - (XAxis-Origin)*0.667 + (YAxis-Origin)*0.667,
      Origin + (XAxis-Origin)*0.667 + (YAxis-Origin)*0.667,
      Origin - (XAxis-Origin)*0.667 + (YAxis-Origin)) );
    AEditor.SetHitBox(idxXNeg, TAffineBox.AffineBox(Origin - (XAxis-Origin) - (YAxis-Origin)*0.667,
      Origin - (XAxis-Origin)*0.667 - (YAxis-Origin)*0.667,
      Origin - (XAxis-Origin) + (YAxis-Origin)*0.667) );
    AEditor.SetHitBox(idxYNeg, TAffineBox.AffineBox(Origin - (XAxis-Origin)*0.667 - (YAxis-Origin),
      Origin + (XAxis-Origin)*0.667 - (YAxis-Origin),
      Origin - (XAxis-Origin)*0.667 - (YAxis-Origin)*0.667) );
    AEditor.SetHitBox(idxOrig, TAffineBox.AffineBox(Origin - (XAxis-Origin)*0.667 - (YAxis-Origin)*0.667,
      Origin + (XAxis-Origin)*0.667 - (YAxis-Origin)*0.667,
      Origin - (XAxis-Origin)*0.667 + (YAxis-Origin)*0.667));
  end;
end;

{ TRectShape }

function TRectShape.PenVisible(AAssumePenFill: boolean): boolean;
begin
  result := (PenWidth>0) and not IsClearPenStyle(PenStyle) and (not PenFill.IsFullyTransparent or AAssumePenFill);
end;

function TRectShape.BackVisible: boolean;
begin
  result := not BackFill.IsFullyTransparent;
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
    result := (totalSurface > 800*600) or ((totalSurface > 320*240) and (BackFill.IsSlow(AMatrix) or (BackFill.FillType = vftGradient)));
  end;
end;

class function TRectShape.Fields: TVectorShapeFields;
begin
  Result:= [vsfPenFill, vsfPenWidth, vsfPenStyle, vsfJoinStyle, vsfBackFill];
end;

procedure TRectShape.Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix;
  ADraft: boolean);
var
  pts: Array of TPointF;
  orthoRect: TRectF;
  r: TRect;
  backScan, penScan: TBGRACustomScanner;
  temp: TBGRABitmap;
  i: Integer;
begin
  pts := GetAffineBox(AMatrix, true).AsPolygon;
  If BackVisible then
  begin
    if (BackFill.FillType = vftSolid) then backScan := nil
    else backScan := BackFill.CreateScanner(AMatrix, ADraft);

    if GetOrthoRect(AMatrix, orthoRect) then
    begin
      if ADraft then
      begin
        r:= rect(round(orthoRect.Left+0.5),round(orthoRect.Top+0.5),round(orthoRect.Right+0.5),round(orthoRect.Bottom+0.5));
        if Assigned(backScan) then
          ADest.FillRect(r, backScan, dmDrawWithTransparency) else
          ADest.FillRect(r, BackFill.SolidColor, dmDrawWithTransparency)
      end
      else
      begin
        if Assigned(backScan) then
        begin
          if BackFill.FillType = vftGradient then
          begin
            with orthoRect do
              r := rect(floor(Left),floor(Top),ceil(Right),ceil(Bottom));
            temp := TBGRABitmap.Create(0,0);
            temp.SetSize(r.Width,r.Height);
            temp.FillRect(0,0,r.Width,r.Height,backScan,dmSet,Point(r.Left,r.Top),daFloydSteinberg);
            temp.ScanOffset := Point(-r.Left,-r.Top);
            ADest.FillRectAntialias(orthoRect, temp);
            temp.Free;
          end else
            ADest.FillRectAntialias(orthoRect, backScan);
        end else
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
        begin
          if BackFill.FillType = vftGradient then
          begin
            r := rect(floor(pts[0].x),floor(pts[0].y),ceil(pts[0].x),ceil(pts[0].y));
            for i := 1 to high(pts) do
              r.Union(rect(floor(pts[i].x),floor(pts[i].y),ceil(pts[i].x),ceil(pts[i].y)));
            temp := TBGRABitmap.Create(0,0);
            temp.SetSize(r.Width,r.Height);
            temp.FillRect(0,0,r.Width,r.Height,backScan,dmSet,Point(r.Left,r.Top),daFloydSteinberg);
            temp.ScanOffset := Point(-r.Left,-r.Top);
            ADest.FillPolyAntialias(pts, temp);
            temp.Free;
          end else
            ADest.FillPolyAntialias(pts, backScan);
        end else
          ADest.FillPolyAntialias(pts, BackFill.SolidColor);
      end;
    end;

    backScan.Free;
  end;
  if PenVisible then
  begin
    if (PenFill.FillType = vftSolid) then penScan := nil
    else penScan := PenFill.CreateScanner(AMatrix, ADraft);

    pts := ComputeStroke(pts,true, AMatrix);
    if ADraft and (PenWidth > 4) then
    begin
      if Assigned(penScan) then
        ADest.FillPoly(pts, penScan, dmDrawWithTransparency) else
        ADest.FillPoly(pts, PenColor, dmDrawWithTransparency)
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
  result := (PenWidth>0) and not IsClearPenStyle(PenStyle) and (not PenFill.IsFullyTransparent or AAssumePenFill);
end;

function TEllipseShape.BackVisible: boolean;
begin
  result := not BackFill.IsFullyTransparent;
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
  Result:= [vsfPenFill, vsfPenWidth, vsfPenStyle, vsfBackFill];
end;

procedure TEllipseShape.Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix;
  ADraft: boolean);
var
  pts: Array of TPointF;
  orthoRect: TRectF;
  center, radius: TPointF;
  draftPen, isOrtho: Boolean;
  r: TRect;
  backScan, penScan: TBGRACustomScanner;
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
      if BackFill.FillType = vftSolid then backScan := nil
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
      if PenFill.FillType = vftSolid then penScan := nil
      else penScan := PenFill.CreateScanner(AMatrix, ADraft);
      draftPen := ADraft and (PenWidth > 4);

      if IsAffineMatrixScaledRotation(AMatrix) and not (draftPen and Assigned(penScan)) then
      begin
        penZoom := VectLen(AMatrix[1,1],AMatrix[2,1]);
        ADest.CustomPenStyle := PenStyle;
        if draftPen then
          ADest.Ellipse(center.x, center.y, radius.x, radius.y, PenColor, PenWidth*penZoom, dmDrawWithTransparency)
        else
        begin
          if Assigned(penScan) then
            ADest.EllipseAntialias(center.x, center.y, radius.x, radius.y, penScan, PenWidth*penZoom) else
            ADest.EllipseAntialias(center.x, center.y, radius.x, radius.y, PenColor, PenWidth*penZoom);
        end;
        ADest.PenStyle := psSolid;
      end else
      begin
        m:= MatrixForPixelCentered(AMatrix);
        pts := ComputeEllipse(m*FOrigin, m*FXAxis, m*FYAxis);
        pts := ComputeStroke(pts,true, AMatrix);
        if draftPen then
        begin
          if Assigned(penScan) then
            ADest.FillPoly(pts, penScan, dmDrawWithTransparency) else
            ADest.FillPoly(pts, PenColor, dmDrawWithTransparency)
        end
        else
        begin
          if Assigned(penScan) then
            ADest.FillPolyAntialias(pts, penScan) else
            ADest.FillPolyAntialias(pts, PenColor);
        end;
      end;

      penScan.Free;
    end;
  end else
  begin
    m:= MatrixForPixelCentered(AMatrix);
    pts := ComputeEllipse(m*FOrigin, m*FXAxis, m*FYAxis);
    If BackVisible then
    begin
      if BackFill.FillType = vftSolid then backScan := nil
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
      if PenFill.FillType = vftSolid then penScan := nil
      else penScan := PenFill.CreateScanner(AMatrix, ADraft);

      pts := ComputeStroke(pts,true, AMatrix);
      if ADraft and (PenWidth > 4) then
      begin
        if Assigned(penScan) then
          ADest.FillPoly(pts, penScan, dmDrawWithTransparency) else
          ADest.FillPoly(pts, PenColor, dmDrawWithTransparency)
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
  result := not BackFill.IsFullyTransparent;
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

class function TPhongShape.PreferPixelCentered: boolean;
begin
  Result:= false;
end;

procedure TPhongShape.ConfigureCustomEditor(AEditor: TBGRAOriginalEditor);
var
  idxLight: Integer;
begin
  inherited ConfigureCustomEditor(AEditor);
  idxLight := AEditor.AddPoint(FLightPosition, @OnMoveLightPos, true);
  if AEditor is TVectorOriginalEditor then
    TVectorOriginalEditor(AEditor).AddLabel(idxLight, LightPositionCaption, taCenter, tlTop);
end;

procedure TPhongShape.MouseDown(RightButton: boolean; Shift: TShiftState; X,
  Y: single; var ACursor: TOriginalEditorCursor; var AHandled: boolean);
begin
  inherited MouseDown(RightButton, Shift, X, Y, ACursor, AHandled);
  if not AHandled then
  begin
    if RightButton then
    begin
      LightPosition := PointF(x,y);
      AHandled := true;
    end;
  end;
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
      shader.AmbientFactor := 0.5;
      shader.NegativeDiffusionFactor := 0.15;
      lightPosF := AffineMatrixTranslation(-rectRaster.Left,-rectRaster.Top)
                    *AffineMatrixInverse(m)*AMatrix
                    *PointF(FLightPosition.x,FLightPosition.y);
      shader.LightPosition := Point(round(lightPosF.x),round(lightPosF.y));
      shader.LightPositionZ := round(100*power(approxFactor,1.18));
      if h*3/2 > shader.LightPositionZ then
       shader.LightPositionZ := round(h*3/2);

      raster := TBGRABitmap.Create(rectRaster.Width,rectRaster.Height);
      if BackFill.FillType = vftSolid then
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
  RegisterVectorShape(TPhongShape);

end.

