unit LCVectorShapes;

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

uses BGRAPen, BGRAGraphics, BGRAFillInfo, BGRAPath, math, LCVectorialFill;

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

procedure TPhongShape.ConfigureEditor(AEditor: TBGRAOriginalEditor);
var
  idxLight: Integer;
begin
  inherited ConfigureEditor(AEditor);
  idxLight := AEditor.AddPoint(FLightPosition, @OnMoveLightPos, true);
  if AEditor is TVectorOriginalEditor then
    TVectorOriginalEditor(AEditor).AddLabel(idxLight, 'Light position', taCenter, tlTop);
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

