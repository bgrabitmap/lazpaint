// SPDX-License-Identifier: GPL-3.0-only
unit LCVectorRectShapes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, LCVectorOriginal, BGRABitmapTypes, BGRALayerOriginal,
  BGRABitmap, BGRATransform, BGRAGradients, BGRASVGShapes, BGRASVGType, BGRAUnits;

type
  TCustomRectShape = class;

  { TCustomRectShapeDiff }

  TCustomRectShapeDiff = class(TVectorShapeDiff)
  protected
    FStartOrigin, FStartXAxis, FStartYAxis: TPointF;
    FStartFixedRatio: Single;
    FEndOrigin, FEndXAxis, FEndYAxis: TPointF;
    FEndFixedRatio: Single;
  public
    constructor Create(AStartShape: TVectorShape); override;
    procedure ComputeDiff(AEndShape: TVectorShape); override;
    procedure Apply(AStartShape: TVectorShape); override;
    procedure Unapply(AEndShape: TVectorShape); override;
    procedure Append(ADiff: TVectorShapeDiff); override;
    function IsIdentity: boolean; override;
  end;

  { TCustomRectShape }

  TCustomRectShape = class(TVectorShape)
  private
    procedure SetXAxis(AValue: TPointF);
    procedure SetYAxis(AValue: TPointF);
  protected
    FOrigin, FXAxis, FYAxis: TPointF;
    FOriginBackup,FXUnitBackup,FYUnitBackup,
    FXAxisBackup,FYAxisBackup: TPointF;
    FXSizeBackup,FYSizeBackup: single;
    FMatrixBackup: TAffineMatrix;
    FFixedRatio: single;
    FDisableHitBox: boolean;
    procedure DoMoveXAxis(ANewCoord: TPointF; AShift: TShiftState; AFactor: single);
    procedure DoMoveYAxis(ANewCoord: TPointF; AShift: TShiftState; AFactor: single);
    procedure DoMoveXYCorner(ANewCoord: TPointF; AShift: TShiftState; AFactorX, AFactorY: single);
    procedure OnMoveOrigin({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF; {%H-}AShift: TShiftState);
    procedure OnMoveXAxis({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF; AShift: TShiftState);
    procedure OnMoveYAxis({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF; AShift: TShiftState);
    procedure OnMoveXAxisNeg({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF; AShift: TShiftState);
    procedure OnMoveYAxisNeg({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF; AShift: TShiftState);
    procedure OnMoveXAxisAlt({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF; AShift: TShiftState);
    procedure OnMoveYAxisAlt({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF; AShift: TShiftState);
    procedure OnMoveXAxisNegAlt({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF; AShift: TShiftState);
    procedure OnMoveYAxisNegAlt({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF; AShift: TShiftState);
    procedure OnMoveXYCorner({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF; AShift: TShiftState);
    procedure OnMoveXNegYCorner({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF; AShift: TShiftState);
    procedure OnMoveXYNegCorner({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF; AShift: TShiftState);
    procedure OnMoveXNegYNegCorner({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF; AShift: TShiftState);
    procedure OnMoveXYCornerAlt({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF; AShift: TShiftState);
    procedure OnMoveXNegYCornerAlt({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF; AShift: TShiftState);
    procedure OnMoveXYNegCornerAlt({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF; AShift: TShiftState);
    procedure OnMoveXNegYNegCornerAlt({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF; AShift: TShiftState);
    procedure OnStartMove({%H-}ASender: TObject; {%H-}APointIndex: integer; {%H-}AShift: TShiftState);
    procedure UpdateFillFromRectDiff;
    function GetCornerPositition: single; virtual; abstract;
    function GetOrthoRect(AMatrix: TAffineMatrix; out ARect: TRectF): boolean;
    function ShowArrows: boolean; virtual;
    procedure SetOrigin(AValue: TPointF);
    function GetHeight: single;
    function GetWidth: single;
    procedure SetHeight(AValue: single);
    procedure SetWidth(AValue: single);
    procedure SetFixedRatio(AValue: single);
    procedure EnsureRatio(ACenterX,ACenterY: single);
  public
    procedure QuickDefine(constref APoint1,APoint2: TPointF); override;
    function SuggestGradientBox(AMatrix: TAffineMatrix): TAffineBox; override;
    procedure LoadFromStorage(AStorage: TBGRACustomOriginalStorage); override;
    procedure SaveToStorage(AStorage: TBGRACustomOriginalStorage); override;
    function GetRenderBounds({%H-}ADestRect: TRect; AMatrix: TAffineMatrix; {%H-}AOptions: TRenderBoundsOptions = []): TRectF; override;
    procedure ConfigureCustomEditor(AEditor: TBGRAOriginalEditor); override;
    function GetAffineBox(const AMatrix: TAffineMatrix; APixelCentered: boolean): TAffineBox;
    procedure TransformFrame(const AMatrix: TAffineMatrix); override;
    procedure AlignTransform(const AMatrix: TAffineMatrix); override;
    property Origin: TPointF read FOrigin write SetOrigin;
    property XAxis: TPointF read FXAxis write SetXAxis;
    property YAxis: TPointF read FYAxis write SetYAxis;
    property Width: single read GetWidth write SetWidth;
    property Height: single read GetHeight write SetHeight;
    property FixedRatio: single read FFixedRatio write SetFixedRatio;
  end;

  { TRectShape }

  TRectShape = class(TCustomRectShape)
  protected
    function GetCornerPositition: single; override;
  public
    class function Fields: TVectorShapeFields; override;
    function AppendToSVG(AContent: TSVGContent; ADefs: TSVGDefine): TSVGElement; override;
    procedure Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix; ADraft: boolean); overload; override;
    function GetRenderBounds({%H-}ADestRect: TRect; AMatrix: TAffineMatrix; AOptions: TRenderBoundsOptions = []): TRectF; override;
    function PointInShape(APoint: TPointF): boolean; overload; override;
    function PointInShape(APoint: TPointF; ARadius: single): boolean; overload; override;
    function PointInBack(APoint: TPointF): boolean; overload; override;
    function PointInPen(APoint: TPointF): boolean; overload; override;
    function GetIsSlow(const AMatrix: TAffineMatrix): boolean; override;
    class function StorageClassName: RawByteString; override;
  end;

  { TEllipseShape }

  TEllipseShape = class(TCustomRectShape)
  protected
    function GetCornerPositition: single; override;
  public
    constructor Create(AContainer: TVectorOriginal); override;
    class function Fields: TVectorShapeFields; override;
    function AppendToSVG(AContent: TSVGContent; ADefs: TSVGDefine): TSVGElement; override;
    function GetAlignBounds(const {%H-}ALayoutRect: TRect; const AMatrix: TAffineMatrix): TRectF; override;
    procedure Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix; ADraft: boolean); overload; override;
    function GetRenderBounds({%H-}ADestRect: TRect; AMatrix: TAffineMatrix; AOptions: TRenderBoundsOptions = []): TRectF; override;
    function PointInShape(APoint: TPointF): boolean; overload; override;
    function PointInShape(APoint: TPointF; ARadius: single): boolean; overload; override;
    function PointInBack(APoint: TPointF): boolean; overload; override;
    function PointInPen(APoint: TPointF): boolean; overload; override;
    function GetIsSlow(const AMatrix: TAffineMatrix): boolean; override;
    class function StorageClassName: RawByteString; override;
  end;

  TPhongShapeKind = (pskRectangle, pskRoundRectangle, pskHalfSphere, pskConeTop, pskConeSide,
                     pskHorizCylinder, pskVertCylinder);

const
  DefaultPhongShapeAltitudePercent = 20;
  DefaultPhongBorderSizePercent = 20;

type
  TPhongShape = class;

  { TPhongShapeDiff }

  TPhongShapeDiff = class(TVectorShapeDiff)
  protected
    FStartShapeKind: TPhongShapeKind;
    FStartLightPosition: TPointF;
    FStartShapeAltitudePercent,FStartBorderSizePercent: single;
    FEndShapeKind: TPhongShapeKind;
    FEndLightPosition: TPointF;
    FEndShapeAltitudePercent,FEndBorderSizePercent: single;
  public
    constructor Create(AStartShape: TVectorShape); override;
    procedure ComputeDiff(AEndShape: TVectorShape); override;
    procedure Apply(AStartShape: TVectorShape); override;
    procedure Unapply(AEndShape: TVectorShape); override;
    procedure Append(ADiff: TVectorShapeDiff); override;
    function IsIdentity: boolean; override;
  end;

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
    function GetEnvelope: ArrayOfTPointF;
  public
    constructor Create(AContainer: TVectorOriginal); override;
    destructor Destroy; override;
    function GetCornerPositition: single; override;
    class function Fields: TVectorShapeFields; override;
    class function PreferPixelCentered: boolean; override;
    function AppendToSVG(AContent: TSVGContent; ADefs: TSVGDefine): TSVGElement; override;
    function GetAlignBounds(const ALayoutRect: TRect; const AMatrix: TAffineMatrix): TRectF; override;
    procedure ConfigureCustomEditor(AEditor: TBGRAOriginalEditor); override;
    procedure MouseDown(RightButton: boolean; {%H-}ClickCount: integer; Shift: TShiftState; X, Y: single; var ACursor: TOriginalEditorCursor; var AHandled: boolean); override;
    procedure LoadFromStorage(AStorage: TBGRACustomOriginalStorage); override;
    procedure SaveToStorage(AStorage: TBGRACustomOriginalStorage); override;
    procedure Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix; ADraft: boolean); overload; override;
    function GetRenderBounds({%H-}ADestRect: TRect; AMatrix: TAffineMatrix; AOptions: TRenderBoundsOptions = []): TRectF; override;
    function PointInShape(APoint: TPointF): boolean; overload; override;
    function PointInShape(APoint: TPointF; ARadius: single): boolean; overload; override;
    function PointInBack(APoint: TPointF): boolean; overload; override;
    function GetIsSlow(const AMatrix: TAffineMatrix): boolean; override;
    function GetGenericCost: integer; override;
    procedure Transform(const AMatrix: TAffineMatrix); override;
    function AllowShearTransform: boolean; override;
    class function StorageClassName: RawByteString; override;
    property ShapeKind: TPhongShapeKind read FShapeKind write SetShapeKind;
    property LightPosition: TPointF read FLightPosition write SetLightPosition;
    property ShapeAltitudePercent: single read FShapeAltitudePercent write SetShapeAltitudePercent;
    property BorderSizePercent: single read FBorderSizePercent write SetBorderSizePercent;
  end;

implementation

uses BGRAPen, BGRAGraphics, BGRAFillInfo, BGRAPath, math, LCVectorialFill, LCResourceString;

{ TPhongShapeDiff }

constructor TPhongShapeDiff.Create(AStartShape: TVectorShape);
begin
  with (AStartShape as TPhongShape) do
  begin
    FStartShapeKind:= ShapeKind;
    FStartLightPosition:= LightPosition;
    FStartShapeAltitudePercent:= ShapeAltitudePercent;
    FStartBorderSizePercent:= BorderSizePercent;
  end;
end;

procedure TPhongShapeDiff.ComputeDiff(AEndShape: TVectorShape);
begin
  with (AEndShape as TPhongShape) do
  begin
    FEndShapeKind:= ShapeKind;
    FEndLightPosition:= LightPosition;
    FEndShapeAltitudePercent:= ShapeAltitudePercent;
    FEndBorderSizePercent:= BorderSizePercent;
  end;
end;

procedure TPhongShapeDiff.Apply(AStartShape: TVectorShape);
begin
  with (AStartShape as TPhongShape) do
  begin
    BeginUpdate;
    FShapeKind := FEndShapeKind;
    FLightPosition := FEndLightPosition;
    FShapeAltitudePercent := FEndShapeAltitudePercent;
    FBorderSizePercent := FEndBorderSizePercent;
    EndUpdate;
  end;
end;

procedure TPhongShapeDiff.Unapply(AEndShape: TVectorShape);
begin
  with (AEndShape as TPhongShape) do
  begin
    BeginUpdate;
    FShapeKind := FStartShapeKind;
    FLightPosition := FStartLightPosition;
    FShapeAltitudePercent := FStartShapeAltitudePercent;
    FBorderSizePercent := FStartBorderSizePercent;
    EndUpdate;
  end;
end;

procedure TPhongShapeDiff.Append(ADiff: TVectorShapeDiff);
var
  next: TPhongShapeDiff;
begin
  next := ADiff as TPhongShapeDiff;
  FEndShapeKind := next.FEndShapeKind;
  FEndLightPosition := next.FEndLightPosition;
  FEndShapeAltitudePercent := next.FEndShapeAltitudePercent;
  FEndBorderSizePercent := next.FEndBorderSizePercent;
end;

function TPhongShapeDiff.IsIdentity: boolean;
begin
  result := (FStartShapeKind = FEndShapeKind) and
    (FStartLightPosition = FEndLightPosition) and
    (FStartShapeAltitudePercent = FEndShapeAltitudePercent) and
    (FStartBorderSizePercent = FEndBorderSizePercent);
end;

{ TCustomRectShapeDiff }

constructor TCustomRectShapeDiff.Create(AStartShape: TVectorShape);
begin
  with (AStartShape as TCustomRectShape) do
  begin
    FStartOrigin := Origin;
    FStartXAxis := XAxis;
    FStartYAxis := YAxis;
    FStartFixedRatio := FixedRatio;
  end;
end;

procedure TCustomRectShapeDiff.ComputeDiff(AEndShape: TVectorShape);
begin
  with (AEndShape as TCustomRectShape) do
  begin
    FEndOrigin := Origin;
    FEndXAxis := XAxis;
    FEndYAxis := YAxis;
    FEndFixedRatio := FixedRatio;
  end;
end;

procedure TCustomRectShapeDiff.Apply(AStartShape: TVectorShape);
begin
  with (AStartShape as TCustomRectShape) do
  begin
    BeginUpdate;
    FOrigin := FEndOrigin;
    FXAxis := FEndXAxis;
    FYAxis := FEndYAxis;
    FFixedRatio := FEndFixedRatio;
    EndUpdate;
  end;
end;

procedure TCustomRectShapeDiff.Unapply(AEndShape: TVectorShape);
begin
  with (AEndShape as TCustomRectShape) do
  begin
    BeginUpdate;
    FOrigin := FStartOrigin;
    FXAxis := FStartXAxis;
    FYAxis := FStartYAxis;
    FFixedRatio := FStartFixedRatio;
    EndUpdate;
  end;
end;

procedure TCustomRectShapeDiff.Append(ADiff: TVectorShapeDiff);
var
  next: TCustomRectShapeDiff;
begin
  next := ADiff as TCustomRectShapeDiff;
  FEndOrigin := next.FEndOrigin;
  FEndXAxis := next.FEndXAxis;
  FEndYAxis := next.FEndYAxis;
  FEndFixedRatio := next.FEndFixedRatio;
end;

function TCustomRectShapeDiff.IsIdentity: boolean;
begin
  result := (FStartOrigin = FEndOrigin) and
  (FStartXAxis = FEndXAxis) and
  (FStartYAxis = FEndYAxis) and
  (FStartFixedRatio = FEndFixedRatio);
end;

{ TCustomRectShape }

procedure TCustomRectShape.SetOrigin(AValue: TPointF);
var
  delta: TPointF;
  t: TAffineMatrix;
begin
  if FOrigin=AValue then Exit;
  BeginUpdate(TCustomRectShapeDiff);
  delta := AValue - FOrigin;
  t := AffineMatrixTranslation(delta.x, delta.y);
  FOrigin := AValue;
  FXAxis := t*FXAxis;
  FYAxis := t*FYAxis;
  TransformFill(t, False);
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
        BeginUpdate(TCustomRectShapeDiff);
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

procedure TCustomRectShape.SetXAxis(AValue: TPointF);
begin
  if FXAxis=AValue then Exit;
  BeginUpdate(TCustomRectShapeDiff);
  FXAxis:=AValue;
  EndUpdate;
end;

procedure TCustomRectShape.SetYAxis(AValue: TPointF);
begin
  if FYAxis=AValue then Exit;
  BeginUpdate(TCustomRectShapeDiff);
  FYAxis:=AValue;
  EndUpdate;
end;

procedure TCustomRectShape.DoMoveXAxis(ANewCoord: TPointF; AShift: TShiftState; AFactor: single);
var
  newSize: Single;
  u: TPointF;
begin
  BeginUpdate(TCustomRectShapeDiff);
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
  UpdateFillFromRectDiff;
  EndUpdate;
end;

procedure TCustomRectShape.DoMoveYAxis(ANewCoord: TPointF; AShift: TShiftState;
  AFactor: single);
var
  newSizeY: Single;
  u: TPointF;
begin
  BeginUpdate(TCustomRectShapeDiff);
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
  UpdateFillFromRectDiff;
  EndUpdate;
end;

procedure TCustomRectShape.DoMoveXYCorner(ANewCoord: TPointF;
  AShift: TShiftState; AFactorX, AFactorY: single);
var
  ratio, d: single;
  m: TAffineMatrix;
  newSize, prevCornerVect, newCornerVect: TPointF;
  angle,deltaAngle, zoom: single;
begin
  BeginUpdate(TCustomRectShapeDiff);
  if (ssAlt in AShift) and (VectDet(FXUnitBackup,FYUnitBackup)<>0) and (FXSizeBackup<>0) and (FYSizeBackup<>0) then
  begin
    prevCornerVect := AFactorX*(FXAxisBackup - FOriginBackup) + AFactorY*(FYAxisBackup - FOriginBackup);
    newCornerVect := (ANewCoord - FOriginBackup)*(1/GetCornerPositition);
    m := AffineMatrixScaledRotation(prevCornerVect, newCornerVect);
    if not (ssShift in AShift) then
    begin
      angle := arctan2(-m[2,1],m[1,1])*2/Pi;
      deltaAngle := 0;
      if abs(frac(angle)) < 0.1 then deltaAngle := -frac(angle)
      else if frac(angle) > 0.9 then deltaAngle := +1-frac(angle)
      else if frac(angle) < -0.9 then deltaAngle := -1-frac(angle)
      else if abs(frac(angle)-0.5) < 0.1 then deltaAngle := 0.5-frac(angle)
      else if abs(frac(angle)+0.5) < 0.1 then deltaAngle := -0.5-frac(angle);
      if deltaAngle <> 0 then
      begin
        angle := (angle+deltaAngle)*Pi/2;
        zoom := VectLen(m[1,1],m[2,1]);
        m := AffineMatrixRotationRad(angle)*AffineMatrixScale(zoom,zoom);
      end;
    end;
    m := AffineMatrixTranslation(FOriginBackup.x,FOriginBackup.y)*m
        *AffineMatrixTranslation(-FOriginBackup.x,-FOriginBackup.y);
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
  UpdateFillFromRectDiff;
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

procedure TCustomRectShape.OnMoveXAxisAlt(ASender: TObject; APrevCoord,
  ANewCoord: TPointF; AShift: TShiftState);
begin
  DoMoveXAxis(ANewCoord, AShift+[ssAlt], 1);
end;

procedure TCustomRectShape.OnMoveYAxisAlt(ASender: TObject; APrevCoord,
  ANewCoord: TPointF; AShift: TShiftState);
begin
  DoMoveYAxis(ANewCoord, AShift+[ssAlt], 1);
end;

procedure TCustomRectShape.OnMoveXAxisNegAlt(ASender: TObject; APrevCoord,
  ANewCoord: TPointF; AShift: TShiftState);
begin
  DoMoveXAxis(ANewCoord, AShift+[ssAlt], -1);
end;

procedure TCustomRectShape.OnMoveYAxisNegAlt(ASender: TObject; APrevCoord,
  ANewCoord: TPointF; AShift: TShiftState);
begin
  DoMoveYAxis(ANewCoord, AShift+[ssAlt], -1);
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

procedure TCustomRectShape.OnMoveXYCornerAlt(ASender: TObject; APrevCoord,
  ANewCoord: TPointF; AShift: TShiftState);
begin
  DoMoveXYCorner(ANewCoord, AShift+[ssAlt], 1, 1);
end;

procedure TCustomRectShape.OnMoveXNegYCornerAlt(ASender: TObject; APrevCoord,
  ANewCoord: TPointF; AShift: TShiftState);
begin
  DoMoveXYCorner(ANewCoord, AShift+[ssAlt], -1, 1);
end;

procedure TCustomRectShape.OnMoveXYNegCornerAlt(ASender: TObject; APrevCoord,
  ANewCoord: TPointF; AShift: TShiftState);
begin
  DoMoveXYCorner(ANewCoord, AShift+[ssAlt], 1, -1);
end;

procedure TCustomRectShape.OnMoveXNegYNegCornerAlt(ASender: TObject;
  APrevCoord, ANewCoord: TPointF; AShift: TShiftState);
begin
  DoMoveXYCorner(ANewCoord, AShift+[ssAlt], -1, -1);
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
  FMatrixBackup := AffineMatrix(FXAxis-FOrigin, FYAxis-FOrigin, FOrigin);
end;

procedure TCustomRectShape.UpdateFillFromRectDiff;
var
  newMatrix, matrixDiff: TAffineMatrix;
begin
  newMatrix := AffineMatrix(FXAxis-FOrigin, FYAxis-FOrigin, FOrigin);
  if IsAffineMatrixInversible(newMatrix) and IsAffineMatrixInversible(FMatrixBackup) then
  begin
    matrixDiff := newMatrix*AffineMatrixInverse(FMatrixBackup);
    TransformFill(matrixDiff, True);
    FMatrixBackup := newMatrix;
  end;
end;

function TCustomRectShape.GetAffineBox(const AMatrix: TAffineMatrix; APixelCentered: boolean): TAffineBox;
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

procedure TCustomRectShape.TransformFrame(const AMatrix: TAffineMatrix);
var
  m: TAffineMatrix;
begin
  BeginUpdate(TCustomRectShapeDiff);
  m := MatrixForPixelCentered(AMatrix);
  FOrigin := m*FOrigin;
  FXAxis := m*FXAxis;
  FYAxis := m*FYAxis;
  EndUpdate;
end;

procedure TCustomRectShape.AlignTransform(const AMatrix: TAffineMatrix);
begin
  Origin := AMatrix*Origin;
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

function TCustomRectShape.ShowArrows: boolean;
begin
  result := true;
end;

procedure TCustomRectShape.QuickDefine(constref APoint1, APoint2: TPointF);
begin
  BeginUpdate(TCustomRectShapeDiff);
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
  idx,idxOrig, idxX,idxY,idxXNeg,idxYNeg: Integer;
begin
  u := FXAxis - FOrigin;
  v := FYAxis - FOrigin;
  AEditor.AddStartMoveHandler(@OnStartMove);
  d := GetCornerPositition;
  if d <> 0 then
  begin
    idx := AEditor.AddPoint(FOrigin + (u+v)*d, @OnMoveXYCorner, false);
    AEditor.AddPointAlternateMove(idx, @OnMoveXYCornerAlt);
    idx := AEditor.AddPoint(FOrigin + (-u+v)*d, @OnMoveXNegYCorner, false);
    AEditor.AddPointAlternateMove(idx, @OnMoveXNegYCornerAlt);
    idx := AEditor.AddPoint(FOrigin + (u-v)*d, @OnMoveXYNegCorner, false);
    AEditor.AddPointAlternateMove(idx, @OnMoveXYNegCornerAlt);
    idx := AEditor.AddPoint(FOrigin + (-u-v)*d, @OnMoveXNegYNegCorner, false);
    AEditor.AddPointAlternateMove(idx, @OnMoveXNegYNegCornerAlt);
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
  AEditor.AddPointAlternateMove(idxX, @OnMoveXAxisAlt);
  AEditor.AddPointAlternateMove(idxY, @OnMoveYAxisAlt);
  AEditor.AddPointAlternateMove(idxXNeg, @OnMoveXAxisNegAlt);
  AEditor.AddPointAlternateMove(idxYNeg, @OnMoveYAxisNegAlt);
  idxOrig := AEditor.AddPoint(FOrigin, @OnMoveOrigin, true);
  if ShowArrows and not FDisableHitBox then
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

function TRectShape.GetCornerPositition: single;
begin
  result := 1;
end;

function TRectShape.GetIsSlow(const AMatrix: TAffineMatrix): boolean;
var
  ab: TAffineBox;
  backSurface, totalSurface, penSurface: Single;
begin
  if not GetPenVisible and not GetBackVisible then
    result := false
  else
  begin
    ab := GetAffineBox(AMatrix, true);
    backSurface := ab.Surface;
    if GetPenVisible then
    begin
      penSurface := (ab.Width+ab.Height)*2*PenWidth;
      if GetBackVisible then
        totalSurface:= backSurface+penSurface/2
      else
        totalSurface := penSurface;
    end else
      totalSurface := backSurface;
    result := (totalSurface > 800*600) or
              ((backSurface > 320*240) and GetBackVisible and BackFill.IsSlow(AMatrix)) or
              ((penSurface > 320*240) and GetPenVisible and PenFill.IsSlow(AMatrix));
  end;
end;

class function TRectShape.Fields: TVectorShapeFields;
begin
  Result:= [vsfPenFill, vsfPenWidth, vsfPenStyle, vsfJoinStyle, vsfBackFill];
end;

function TRectShape.AppendToSVG(AContent: TSVGContent; ADefs: TSVGDefine): TSVGElement;
var
  topLeft, u, v: TPointF;
  w, h: Single;
  m: TAffineMatrix;

  function ApproxPointEqual(const APoint1, APoint2: TPointF): boolean;
  var
    precision: Single;
  begin
    precision := (VectLen(APoint1) + VectLen(APoint2))*1e-6;
    result := VectLen(APoint2-APoint1) <= precision;
  end;

begin
  topLeft := Origin - (XAxis - Origin) - (YAxis - Origin);
  w := Width*2; h := Height*2;
  if (XAxis.y <> 0) or (YAxis.x <> 0) then
  begin
    u := XAxis - Origin;
    if w > 0 then u *= (2/w);
    v := YAxis - Origin;
    if h > 0 then v *= (2/h);
    m := AffineMatrixTranslation(topLeft.X, topLeft.Y) *
        AffineMatrix(u, v, PointF(0, 0)) *
        AffineMatrixTranslation(-topLeft.X, -topLeft.Y);
  end else
    m := AffineMatrixIdentity;
  if not PenVisible and (BackFill.FillType = vftTexture) and
    (BackFill.TextureRepetition = trNone) and Assigned(BackFill.Texture) and
    ApproxPointEqual(Origin + PointF(0.5, 0.5), BackFill.TextureMatrix * PointF(BackFill.Texture.Width/2, BackFill.Texture.Height/2)) and
    ApproxPointEqual(XAxis + PointF(0.5, 0.5), BackFill.TextureMatrix * PointF(BackFill.Texture.Width, BackFill.Texture.Height/2)) and
    ApproxPointEqual(YAxis + PointF(0.5, 0.5), BackFill.TextureMatrix * PointF(BackFill.Texture.Width/2, BackFill.Texture.Height)) then
  begin
    result := AContent.AppendImage(topLeft, PointF(w,h), BackFill.Texture, false);
    result.opacity:= BackFill.TextureOpacity/255;
    result.Matrix[cuPixel] := m;
  end else
  begin
    result := AContent.AppendRect(topLeft, PointF(w, h));
    result.Matrix[cuPixel] := m;
    ApplyStrokeStyleToSVG(result, ADefs);
    ApplyFillStyleToSVG(result, ADefs);
  end;
end;

procedure TRectShape.Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix;
  ADraft: boolean);
const GradientDithering = false;
var
  pts: Array of TPointF;
  orthoRect: TRectF;
  r: TRect;
  backScan, penScan: TBGRACustomScanner;
  temp: TBGRABitmap;
  i: Integer;
begin
  pts := GetAffineBox(AMatrix, true).AsPolygon;
  If GetBackVisible and (Width <> 0) and (Height <> 0) then
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
          if (BackFill.FillType = vftGradient) and GradientDithering then
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
  if GetPenVisible then
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
  if not (GetBackVisible or (rboAssumeBackFill in AOptions)) and not GetPenVisible(rboAssumePenFill in AOptions) then
    result:= EmptyRectF
  else
  begin
    result := inherited GetRenderBounds(ADestRect, AMatrix, AOptions);
    if GetPenVisible(rboAssumePenFill in AOptions) then
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
        if not IsEmptyPointF(pts[i]) then
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
  if GetBackVisible and box.Contains(APoint) then
    result := true else
  if GetPenVisible then
  begin
    pts := ComputeStroke(box.AsPolygon, true, AffineMatrixIdentity);
    result:= IsPointInPolygon(pts, APoint, true);
  end else
    result := false;
end;

function TRectShape.PointInShape(APoint: TPointF; ARadius: single): boolean;
var
  pts: ArrayOfTPointF;
  box: TAffineBox;
begin
  if GetPenVisible or GetBackVisible then
  begin
    box := GetAffineBox(AffineMatrixIdentity, true);
    pts := ComputeStrokeEnvelope(box.AsPolygon, true, ARadius*2);
    result:= IsPointInPolygon(pts, APoint, true);
  end
  else result := false;
end;

function TRectShape.PointInBack(APoint: TPointF): boolean;
var
  box: TAffineBox;
  scan: TBGRACustomScanner;
begin
  if GetBackVisible then
  begin
    box := GetAffineBox(AffineMatrixIdentity, true);
    result := box.Contains(APoint);
    if result and (BackFill.FillType = vftTexture) then
    begin
      scan := BackFill.CreateScanner(AffineMatrixIdentity, false);
      if scan.ScanAt(APoint.X,APoint.Y).alpha = 0 then result := false;
      scan.Free;
    end;
  end else
    result := false;
end;

function TRectShape.PointInPen(APoint: TPointF): boolean;
var
  pts: ArrayOfTPointF;
begin
  if GetPenVisible then
  begin
    pts := GetAffineBox(AffineMatrixIdentity, true).AsPolygon;
    pts := ComputeStroke(pts,true, AffineMatrixIdentity);
    result:= IsPointInPolygon(pts, APoint, true);
  end else
    result := false;
end;

class function TRectShape.StorageClassName: RawByteString;
begin
  result := 'rect';
end;

{ TEllipseShape }

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

function TEllipseShape.AppendToSVG(AContent: TSVGContent; ADefs: TSVGDefine): TSVGElement;
var
  u, v: TPointF;
  rx, ry: Single;
begin
  rx := Width; ry := Height;
  if rx <> ry then
    result := AContent.AppendEllipse(Origin, PointF(rx, ry))
    else result := AContent.AppendCircle(Origin, rx);
  if (XAxis.y <> 0) or (YAxis.x <> 0) then
  begin
    u := XAxis - Origin;
    if rx > 0 then u *= (1/rx);
    v := YAxis - Origin;
    if ry > 0 then v *= (1/ry);
    result.matrix[cuPixel] := AffineMatrixTranslation(Origin.X, Origin.Y) *
                              AffineMatrix(u, v, PointF(0, 0)) *
                              AffineMatrixTranslation(-Origin.X, -Origin.Y);
  end;
  ApplyStrokeStyleToSVG(result, ADefs);
  ApplyFillStyleToSVG(result, ADefs);
end;

function TEllipseShape.GetAlignBounds(const ALayoutRect: TRect;
  const AMatrix: TAffineMatrix): TRectF;
var
  m: TAffineMatrix;
  pts: ArrayOfTPointF;
  i: Integer;
  zoom: Single;

  procedure IncludePoint(const APoint: TPointF);
  begin
    if APoint.x < result.Left then result.Left := APoint.x else
    if APoint.x > result.Right then result.Right := APoint.x;
    if APoint.y < result.Top then result.Top := APoint.y else
    if APoint.y > result.Bottom then result.Bottom := APoint.y;
  end;

begin
  m:= AffineMatrixTranslation(0.5,0.5)*MatrixForPixelCentered(AMatrix);
  pts := ComputeEllipse(m*FOrigin, m*FXAxis, m*FYAxis);
  if pts = nil then exit(EmptyRectF);
  result.TopLeft := pts[0];
  result.BottomRight := pts[0];
  for i := 0 to high(pts) do IncludePoint(pts[i]);
  IncludePoint(m*XAxis);
  IncludePoint(m*YAxis);
  IncludePoint(m*(Origin-(XAxis-Origin)));
  IncludePoint(m*(Origin-(YAxis-Origin)));
  if GetPenVisible then
  begin
    zoom := (VectLen(AMatrix[1,1],AMatrix[2,1])+VectLen(AMatrix[1,2],AMatrix[2,2]))/2;
    result.Left -= zoom*PenWidth/2;
    result.Right += zoom*PenWidth/2;
    result.Top -= zoom*PenWidth/2;
    result.Bottom += zoom*PenWidth/2;
  end;
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
    If GetBackVisible then
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
    if GetPenVisible then
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
    If GetBackVisible then
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
    if GetPenVisible then
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
  if not (GetBackVisible or (rboAssumeBackFill in AOptions)) and not GetPenVisible(rboAssumePenFill in AOptions) then
    result:= EmptyRectF
  else
  begin
    result := inherited GetRenderBounds(ADestRect, AMatrix, AOptions);
    if GetPenVisible(rboAssumePenFill in AOptions) then
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
  if GetBackVisible and IsPointInPolygon(pts, APoint, true) then
    result := true else
  if GetPenVisible then
  begin
    pts := ComputeStroke(pts, true, AffineMatrixIdentity);
    result:= IsPointInPolygon(pts, APoint, true);
  end else
    result := false;
end;

function TEllipseShape.PointInShape(APoint: TPointF; ARadius: single): boolean;
var
  pts: ArrayOfTPointF;
begin
  if GetPenVisible or GetBackVisible then
  begin
    pts := ComputeEllipse(FOrigin, FXAxis, FYAxis);
    pts := ComputeStrokeEnvelope(pts, true, ARadius*2);
    result:= IsPointInPolygon(pts, APoint, true);
  end else
    result := false;
end;

function TEllipseShape.PointInBack(APoint: TPointF): boolean;
var
  pts: ArrayOfTPointF;
  scan: TBGRACustomScanner;
begin
  if GetBackVisible then
  begin
    pts := ComputeEllipse(FOrigin, FXAxis, FYAxis);
    result:= IsPointInPolygon(pts, APoint, true);
    if result and (BackFill.FillType = vftTexture) then
    begin
      scan := BackFill.CreateScanner(AffineMatrixIdentity, false);
      if scan.ScanAt(APoint.X,APoint.Y).alpha = 0 then result := false;
      scan.Free;
    end;
  end else
    result := false;
end;

function TEllipseShape.PointInPen(APoint: TPointF): boolean;
var
  pts: ArrayOfTPointF;
begin
  if GetPenVisible then
  begin
    pts := ComputeEllipse(FOrigin, FXAxis, FYAxis);
    pts := ComputeStroke(pts,true, AffineMatrixIdentity);
    result:= IsPointInPolygon(pts, APoint, true);
  end else
    result := false;
end;

function TEllipseShape.GetIsSlow(const AMatrix: TAffineMatrix): boolean;
var
  ab: TAffineBox;
  backSurface, totalSurface, penSurface: Single;
begin
  if not GetPenVisible and not GetBackVisible then
    result := false
  else
  begin
    ab := GetAffineBox(AMatrix, true);
    backSurface := ab.Surface*Pi/4;
    if GetPenVisible then
    begin
      penSurface := (ab.Width+ab.Height)*(Pi/2)*PenWidth;
      if GetBackVisible then
        totalSurface:= backSurface+penSurface/2
      else
        totalSurface := penSurface;
    end else
      totalSurface := backSurface;
    result := (totalSurface > 640*480) or
              ((backSurface > 320*240) and GetBackVisible and BackFill.IsSlow(AMatrix)) or
              ((penSurface > 320*240) and GetPenVisible and PenFill.IsSlow(AMatrix));
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
  BeginUpdate(TPhongShapeDiff);
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
  BeginUpdate(TPhongShapeDiff);
  FBorderSizePercent:=AValue;
  EndUpdate;
end;

procedure TPhongShape.SetLightPosition(AValue: TPointF);
begin
  if FLightPosition=AValue then Exit;
  BeginUpdate(TPhongShapeDiff);
  FLightPosition:=AValue;
  EndUpdate;
end;

procedure TPhongShape.SetShapeAltitudePercent(AValue: single);
begin
  if FShapeAltitudePercent=AValue then Exit;
  BeginUpdate(TPhongShapeDiff);
  FShapeAltitudePercent:=AValue;
  EndUpdate;
end;

function TPhongShape.GetEnvelope: ArrayOfTPointF;
var
  box: TAffineBox;
begin
  case ShapeKind of
    pskHalfSphere, pskConeTop: result := ComputeEllipse(FOrigin, FXAxis, FYAxis);
    pskConeSide: result := PointsF([FOrigin - (FYAxis-FOrigin), FYAxis + (FXAxis-FOrigin), FYAxis - (FXAxis-FOrigin)]);
  else
    begin
      box := GetAffineBox(AffineMatrixIdentity, true);
      result := box.AsPolygon;
    end;
  end;
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

function TPhongShape.AppendToSVG(AContent: TSVGContent; ADefs: TSVGDefine): TSVGElement;
var
  u, v: TPointF;
  rx, ry: Single;
  p: TBGRAPath;
begin
  rx := Width; ry := Height;
  case ShapeKind of
    pskHalfSphere, pskConeTop:
        if rx <> ry then
          result := AContent.AppendEllipse(Origin, PointF(rx, ry))
          else result := AContent.AppendCircle(Origin, rx);
    pskConeSide: begin
      p := TBGRAPath.Create;
      p.moveTo(Origin.x, origin.y - ry);
      p.lineTo(Origin.x + rx, Origin.y + ry);
      p.lineTo(Origin.x - rx, Origin.y + ry);
      result := AContent.AppendPath(p);
      p.Free;
    end
    else {pskRectangle, pskRoundRectangle, pskHorizCylinder, pskVertCylinder}
      result := AContent.AppendRect(Origin.x - rx, Origin.y - ry, rx*2, ry*2);
  end;

  if (XAxis.y <> 0) or (YAxis.x <> 0) then
  begin
    u := XAxis - Origin;
    if rx > 0 then u *= (1/rx);
    v := YAxis - Origin;
    if ry > 0 then v *= (1/ry);
    result.matrix[cuPixel] := AffineMatrixTranslation(Origin.X, Origin.Y) *
                              AffineMatrix(u, v, PointF(0, 0)) *
                              AffineMatrixTranslation(-Origin.X, -Origin.Y);
  end;
  result.strokeNone;
  ApplyFillStyleToSVG(result, ADefs);
end;

function TPhongShape.GetAlignBounds(const ALayoutRect: TRect;
  const AMatrix: TAffineMatrix): TRectF;
var
  m: TAffineMatrix;
  pts: ArrayOfTPointF;
  i: Integer;

  procedure IncludePoint(const APoint: TPointF);
  begin
    if APoint.x < result.Left then result.Left := APoint.x else
    if APoint.x > result.Right then result.Right := APoint.x;
    if APoint.y < result.Top then result.Top := APoint.y else
    if APoint.y > result.Bottom then result.Bottom := APoint.y;
  end;

begin
  m:= AffineMatrixTranslation(0.5,0.5)*MatrixForPixelCentered(AMatrix);
  if ShapeKind in[pskHalfSphere,pskConeTop] then
  begin
    pts := ComputeEllipse(m*FOrigin, m*FXAxis, m*FYAxis);
    if pts = nil then exit(EmptyRectF);
    result.TopLeft := pts[0];
    result.BottomRight := pts[0];
    for i := 0 to high(pts) do IncludePoint(pts[i]);
    IncludePoint(m*XAxis);
    IncludePoint(m*YAxis);
    IncludePoint(m*(Origin-(XAxis-Origin)));
    IncludePoint(m*(Origin-(YAxis-Origin)));
  end else
  if ShapeKind = pskConeSide then
  begin
    result.TopLeft := m*Origin;
    result.BottomRight := m*Origin;
    IncludePoint(m*(XAxis+(YAxis-Origin)));
    IncludePoint(m*(Origin-(XAxis-Origin)+(YAxis-Origin)));
    IncludePoint(m*(Origin-(YAxis-Origin)));
  end else
    result := inherited GetAlignBounds(ALayoutRect,AMatrix);
end;

procedure TPhongShape.ConfigureCustomEditor(AEditor: TBGRAOriginalEditor);
var
  idxLight: Integer;
begin
  inherited ConfigureCustomEditor(AEditor);
  idxLight := AEditor.AddPoint(FLightPosition, @OnMoveLightPos, true);
  if AEditor is TVectorOriginalEditor then
    TVectorOriginalEditor(AEditor).AddLabel(idxLight, rsLightPosition, taCenter, tlTop);
end;

procedure TPhongShape.MouseDown(RightButton: boolean; ClickCount: integer; Shift: TShiftState; X,
  Y: single; var ACursor: TOriginalEditorCursor; var AHandled: boolean);
begin
  inherited MouseDown(RightButton, ClickCount, Shift, X, Y, ACursor, AHandled);
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
  m,mInv: TAffineMatrix;
  h, lightPosZ: single;
  map,raster: TBGRABitmap;
  u,v,lightPosF: TPointF;
  scan: TBGRACustomScanner;
  rectRenderF,rectRasterF: TRectF;
  rectRender,rectRaster, prevClip: TRect;
begin
  if not GetBackVisible then exit;

  //determine final render bounds
  rectRenderF := GetRenderBounds(InfiniteRect,AMatrix);
  if IsEmptyRectF(rectRenderF) then exit;

  rectRender := rect(floor(rectRenderF.Left),floor(rectRenderF.Top),ceil(rectRenderF.Right),ceil(rectRenderF.Bottom));
  rectRender.Intersect(ADest.ClipRect);
  if IsRectEmpty(rectRender) then exit;

  //determine map size before transform
  ab := GetAffineBox(AMatrix, false);
  if (ab.Width = 0) or (ab.Height = 0) then exit;
  if ab.Width > ab.Height then
  begin
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
    if mapWidth > 100 then approxFactor:= min(approxFactor, 100/mapWidth);
    if mapHeight > 100 then approxFactor:= min(approxFactor, 100/mapHeight);
  end else
  begin
    if mapWidth > 800 then approxFactor:= min(approxFactor, 800/mapWidth);
    if mapHeight > 800 then approxFactor:= min(approxFactor, 800/mapHeight);
  end;
  mapWidth:= ceil(mapWidth*approxFactor);
  mapHeight:= ceil(mapHeight*approxFactor);

  //determine map transform
  u := (ab.TopRight-ab.TopLeft)*(1/ab.Width);
  v := (ab.BottomLeft-ab.TopLeft)*(1/ab.Height);
  m := AffineMatrix(u,v,ab.TopLeft)*AffineMatrixScale(ab.Width/mapWidth,ab.Height/mapHeight);
  borderSize := FBorderSizePercent/200*min(ab.Width,ab.Height);
  if not IsAffineMatrixInversible(m) then exit;
  mInv := AffineMatrixInverse(m);

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

    abRaster := mInv*TAffineBox.AffineBox(rectRenderF);
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
                    *mInv*AMatrix*FLightPosition;
      lightPosZ := 100*Power(approxFactor,1.1);
      if h*3/2 > lightPosZ then lightposZ := h*3/2;
      shader.LightPosition3D := Point3D(lightPosF.x,lightPosF.y,lightPosZ);

      raster := TBGRABitmap.Create(rectRaster.Width,rectRaster.Height);
      if BackFill.FillType = vftSolid then
        shader.Draw(raster,map,h,-rectRaster.Left,-rectRaster.Top,BackFill.SolidColor)
      else
      begin
        scan := BackFill.CreateScanner(AffineMatrixTranslation(-rectRaster.left,-rectRaster.top)*mInv*AMatrix,ADraft);
        shader.DrawScan(raster,map,h,-rectRaster.Left,-rectRaster.Top,scan);
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
  if not (GetBackVisible or (rboAssumeBackFill in AOptions)) then
    result:= EmptyRectF
  else
    result := inherited GetRenderBounds(ADestRect, AMatrix, AOptions);
end;

function TPhongShape.PointInShape(APoint: TPointF): boolean;
var
  pts: ArrayOfTPointF;
begin
  if not GetBackVisible then exit(false);
  pts := GetEnvelope;
  result := IsPointInPolygon(pts, APoint, true);
end;

function TPhongShape.PointInShape(APoint: TPointF; ARadius: single): boolean;
var
  pts: ArrayOfTPointF;
begin
  if GetBackVisible then
  begin
    pts := ComputeStrokeEnvelope(GetEnvelope, true, ARadius*2);
    result:= IsPointInPolygon(pts, APoint, true);
  end
    else result := false;
end;

function TPhongShape.PointInBack(APoint: TPointF): boolean;
var
  scan: TBGRACustomScanner;
begin
  result := PointInShape(APoint);
  if result and (BackFill.FillType = vftTexture) then
  begin
    scan := BackFill.CreateScanner(AffineMatrixIdentity, false);
    if scan.ScanAt(APoint.X,APoint.Y).alpha = 0 then result := false;
    scan.Free;
  end;
end;

function TPhongShape.GetIsSlow(const AMatrix: TAffineMatrix): boolean;
var
  ab: TAffineBox;
begin
  if not GetBackVisible then exit(false);
  ab := GetAffineBox(AMatrix, true);
  result := ab.Surface > 320*240;
end;

function TPhongShape.GetGenericCost: integer;
begin
  Result:= 10;
end;

procedure TPhongShape.Transform(const AMatrix: TAffineMatrix);
begin
  BeginUpdate(TPhongShapeDiff);
  LightPosition := AMatrix*LightPosition;
  inherited Transform(AMatrix);
  EndUpdate;
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

