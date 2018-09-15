unit uvectororiginal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRALayerOriginal, fgl, BGRAGradientOriginal, BGRABitmapTypes;

type
  TShapeChangeEvent = procedure(ASender: TObject; ABounds: TRectF) of object;

  { TVectorShape }

  TVectorShape = class
  protected
    FOnChange: TShapeChangeEvent;
    FUpdateCount: integer;
    FBoundsBeforeUpdate: TRectF;
    procedure BeginUpdate;
    procedure EndUpdate;
    function GetBackColor: TBGRAPixel; virtual; abstract;
    function GetPenColor: TBGRAPixel; virtual; abstract;
    function GetPenWidth: single; virtual; abstract;
    procedure SetBackColor(AValue: TBGRAPixel); virtual; abstract;
    procedure SetPenColor(AValue: TBGRAPixel); virtual; abstract;
    procedure SetPenWidth(AValue: single); virtual; abstract;
    function ComputeStroke(APoints: ArrayOfTPointF; AClosed: boolean): ArrayOfTPointF;
  public
    procedure QuickDefine(const APoint1,APoint2: TPointF); virtual; abstract;
    procedure Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix; ADraft: boolean); virtual; abstract;
    function GetRenderBounds(AMatrix: TAffineMatrix): TRectF; virtual; abstract;
    function PointInShape(APoint: TPointF): boolean; virtual; abstract;
    procedure ConfigureEditor(AEditor: TBGRAOriginalEditor); virtual; abstract;
    procedure LoadFromStorage(AStorage: TBGRACustomOriginalStorage); virtual; abstract;
    procedure SaveToStorage(AStorage: TBGRACustomOriginalStorage); virtual; abstract;
    class function StorageClassName: RawByteString; virtual; abstract;
    function GetIsSlow(AMatrix: TAffineMatrix): boolean; virtual;
    property OnChange: TShapeChangeEvent read FOnChange write FOnChange;
    property PenColor: TBGRAPixel read GetPenColor write SetPenColor;
    property BackColor: TBGRAPixel read GetBackColor write SetBackColor;
    property PenWidth: single read GetPenWidth write SetPenWidth;
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
    procedure OnMoveOrigin(ASender: TObject; APrevCoord, ANewCoord: TPointF; AShift: TShiftState);
    procedure OnMoveXAxis(ASender: TObject; APrevCoord, ANewCoord: TPointF; AShift: TShiftState);
    procedure OnMoveYAxis(ASender: TObject; APrevCoord, ANewCoord: TPointF; AShift: TShiftState);
    procedure OnMoveXAxisNeg(ASender: TObject; APrevCoord, ANewCoord: TPointF; AShift: TShiftState);
    procedure OnMoveYAxisNeg(ASender: TObject; APrevCoord, ANewCoord: TPointF; AShift: TShiftState);
    procedure OnStartMove(ASender: TObject; APointIndex: integer; AShift: TShiftState);
    function GetAffineBox(AMatrix: TAffineMatrix; APixelCentered: boolean): TAffineBox;
  public
    procedure QuickDefine(const APoint1,APoint2: TPointF); override;
    function GetRenderBounds(AMatrix: TAffineMatrix): TRectF; override;
  end;

  { TRectShape }

  TRectShape = class(TCustomRectShape)
  protected
    FPenColor,FBackColor: TBGRAPixel;
    FPenWidth: single;
    function GetBackColor: TBGRAPixel; override;
    function GetPenColor: TBGRAPixel; override;
    function GetPenWidth: single; override;
    procedure SetBackColor(AValue: TBGRAPixel); override;
    procedure SetPenColor(AValue: TBGRAPixel); override;
    procedure SetPenWidth(AValue: single); override;
    function PenVisible: boolean;
    function BackVisible: boolean;
  public
    constructor Create;
    procedure Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix; ADraft: boolean); override;
    function GetRenderBounds(AMatrix: TAffineMatrix): TRectF; override;
    function PointInShape(APoint: TPointF): boolean; override;
    procedure ConfigureEditor(AEditor: TBGRAOriginalEditor); override;
    procedure LoadFromStorage(AStorage: TBGRACustomOriginalStorage); override;
    procedure SaveToStorage(AStorage: TBGRACustomOriginalStorage); override;
    function GetIsSlow(AMatrix: TAffineMatrix): boolean; override;
    class function StorageClassName: RawByteString; override;
  end;

  TVectorOriginalSelectShapeEvent = procedure(ASender: TObject; AShape: TVectorShape; APreviousShape: TVectorShape) of object;

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
    procedure DiscardFrozenShapes;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear;
    function AddShape(AShape: TVectorShape): integer;
    function RemoveShape(AShape: TVectorShape): boolean;
    procedure SelectShape(AIndex: integer);
    procedure DeselectShape;
    procedure MouseClick(APoint: TPointF);
    procedure Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix; ADraft: boolean); override;
    procedure ConfigureEditor(AEditor: TBGRAOriginalEditor); override;
    function GetRenderBounds(ADestRect: TRect; {%H-}AMatrix: TAffineMatrix): TRect; override;
    procedure LoadFromStorage(AStorage: TBGRACustomOriginalStorage); override;
    procedure SaveToStorage(AStorage: TBGRACustomOriginalStorage); override;
    class function StorageClassName: RawByteString; override;
    property OnSelectShape: TVectorOriginalSelectShapeEvent read FOnSelectShape write FOnSelectShape;
    property SelectedShape: TVectorShape read FSelectedShape;
  end;

procedure RegisterVectorShape(AClass: TVectorShapeAny);
function GetVectorShapeByStorageClassName(AName: string): TVectorShapeAny;

implementation

uses math, BGRATransform, BGRAFillInfo, BGRAPen, BGRAGraphics, BGRAPolygon;

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

{ TCustomRectShape }

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
var newSize: single;
begin
  BeginUpdate;
  if (ssAlt in AShift) or (FXUnitBackup = PointF(0,0)) then
  begin
    FXAxis := ANewCoord;
    FYAxis := FYAxisBackup;
    FOrigin := FOriginBackup;
  end
  else
  begin
    newSize := FXUnitBackup*(ANewCoord-FOriginBackup);
    if ssShift in AShift then
    begin
      FXAxis := FOrigin+FXUnitBackup*newSize;
      FYAxis := FYAxisBackup;
      FOrigin := FOriginBackup;
    end else
    begin
      FXAxis := FOriginBackup+FXUnitBackup*newSize;
      FYAxis := FYAxisBackup + (newSize-FXSizeBackup)*0.5*FXUnitBackup;
      FOrigin := FOriginBackup + (newSize-FXSizeBackup)*0.5*FXUnitBackup;
    end;
  end;
  EndUpdate;
end;

procedure TCustomRectShape.OnMoveYAxis(ASender: TObject; APrevCoord,
  ANewCoord: TPointF; AShift: TShiftState);
var newSize: single;
begin
  BeginUpdate;
  if (ssAlt in AShift) or (FYUnitBackup = PointF(0,0)) then
  begin
    FYAxis := ANewCoord;
    FXAxis := FXAxisBackup;
    FOrigin := FOriginBackup;
  end
  else
  begin
    newSize := FYUnitBackup*(ANewCoord-FOriginBackup);
    if ssShift in AShift then
    begin
      FYAxis := FOrigin+FYUnitBackup*newSize;
      FXAxis := FXAxisBackup;
      FOrigin := FOriginBackup;
    end else
    begin
      FYAxis := FOriginBackup+FYUnitBackup*newSize;
      FXAxis := FXAxisBackup + (newSize-FYSizeBackup)*0.5*FYUnitBackup;
      FOrigin := FOriginBackup + (newSize-FYSizeBackup)*0.5*FYUnitBackup;
    end;
  end;
  EndUpdate;
end;

procedure TCustomRectShape.OnMoveXAxisNeg(ASender: TObject; APrevCoord,
  ANewCoord: TPointF; AShift: TShiftState);
var newSize: single;
begin
  BeginUpdate;
  if (ssAlt in AShift) or (FXUnitBackup = PointF(0,0)) then
  begin
    FXAxis := FOriginBackup - (ANewCoord - FOriginBackup);
    FYAxis := FYAxisBackup;
    FOrigin := FOriginBackup;
  end
  else
  begin
    newSize := -FXUnitBackup*(ANewCoord-FOriginBackup);
    if ssShift in AShift then
    begin
      FXAxis := FOrigin+FXUnitBackup*newSize;
      FYAxis := FYAxisBackup;
      FOrigin := FOriginBackup;
    end else
    begin
      FXAxis := FXAxisBackup;
      FYAxis := FYAxisBackup - (newSize-FXSizeBackup)*0.5*FXUnitBackup;
      FOrigin := FOriginBackup - (newSize-FXSizeBackup)*0.5*FXUnitBackup;
    end;
  end;
  EndUpdate;
end;

procedure TCustomRectShape.OnMoveYAxisNeg(ASender: TObject; APrevCoord,
  ANewCoord: TPointF; AShift: TShiftState);
var newSize: single;
begin
  BeginUpdate;
  if (ssAlt in AShift) or (FYUnitBackup = PointF(0,0)) then
  begin
    FYAxis := FOriginBackup - (ANewCoord - FOriginBackup);
    FXAxis := FXAxisBackup;
    FOrigin := FOriginBackup;
  end
  else
  begin
    newSize := -FYUnitBackup*(ANewCoord-FOriginBackup);
    if ssShift in AShift then
    begin
      FYAxis := FOrigin+FYUnitBackup*newSize;
      FXAxis := FXAxisBackup;
      FOrigin := FOriginBackup;
    end else
    begin
      FYAxis := FYAxisBackup;
      FXAxis := FXAxisBackup - (newSize-FYSizeBackup)*0.5*FYUnitBackup;
      FOrigin := FOriginBackup - (newSize-FYSizeBackup)*0.5*FYUnitBackup;
    end;
  end;
  EndUpdate;
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

procedure TCustomRectShape.QuickDefine(const APoint1, APoint2: TPointF);
begin
  BeginUpdate;
  FOrigin := (APoint1+APoint2)*0.5;
  FXAxis := PointF(APoint2.X,FOrigin.Y);
  FYAxis := PointF(FOrigin.X,APoint2.Y);
  EndUpdate;
end;

function TCustomRectShape.GetRenderBounds(AMatrix: TAffineMatrix): TRectF;
begin
  result := GetAffineBox(AMatrix, false).RectBoundsF;
end;

{ TRectShape }

function TRectShape.GetBackColor: TBGRAPixel;
begin
  result := FBackColor;
end;

function TRectShape.GetPenColor: TBGRAPixel;
begin
  result := FPenColor;
end;

function TRectShape.GetPenWidth: single;
begin
  result := FPenWidth;
end;

procedure TRectShape.SetBackColor(AValue: TBGRAPixel);
begin
  if AValue.alpha = 0 then AValue := BGRAPixelTransparent;
  if FBackColor = AValue then exit;
  BeginUpdate;
  FBackColor := AValue;
  EndUpdate;
end;

procedure TRectShape.SetPenColor(AValue: TBGRAPixel);
begin
  if AValue.alpha = 0 then AValue := BGRAPixelTransparent;
  if FPenColor = AValue then exit;
  BeginUpdate;
  FPenColor := AValue;
  EndUpdate;
end;

procedure TRectShape.SetPenWidth(AValue: single);
begin
  if FPenWidth = AValue then exit;
  BeginUpdate;
  FPenWidth := AValue;
  EndUpdate;
end;

function TRectShape.PenVisible: boolean;
begin
  result := (FPenWidth>0) and (FPenColor.alpha>0);
end;

function TRectShape.BackVisible: boolean;
begin
  result := FBackColor.alpha <> 0;
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

constructor TRectShape.Create;
begin
  FPenColor := BGRAPixelTransparent;
  FBackColor := BGRABlack;
  FPenWidth := 0;
end;

procedure TRectShape.Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix;
  ADraft: boolean);
var
  pts: Array of TPointF;
  multi: TBGRAMultishapeFiller;
begin
  pts := GetAffineBox(AMatrix, true).AsPolygon;

  multi := TBGRAMultishapeFiller.Create;
  multi.PolygonOrder:= poLastOnTop;
  multi.FillMode:= fmWinding;
  multi.Antialiasing:= not ADraft;
  If BackVisible then
  begin
    multi.AddPolygon(pts, FBackColor);
  end;
  if PenVisible then
  begin
    pts := ComputeStroke(pts,true);
    multi.AddPolygon(pts, FPenColor);
  end;
  multi.Draw(ADest);
  multi.Free;
end;

function TRectShape.GetRenderBounds(AMatrix: TAffineMatrix): TRectF;
begin
  if not BackVisible and not PenVisible then
    result:= EmptyRectF
  else
  begin
    result := inherited GetRenderBounds(AMatrix);
    if PenVisible then
    begin
      result.Left -= PenWidth*0.5;
      result.Top -= PenWidth*0.5;
      result.Right += PenWidth*0.5;
      result.Bottom += PenWidth*0.5;
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
    pts := ComputeStroke(box.AsPolygon, true);
    result:= IsPointInPolygon(pts, APoint, true);
  end else
    result := false;
end;

procedure TRectShape.ConfigureEditor(AEditor: TBGRAOriginalEditor);
begin
  AEditor.AddStartMoveHandler(@OnStartMove);
  AEditor.AddPoint(FOrigin, @OnMoveOrigin, true);
  AEditor.AddArrow(FOrigin, FXAxis, @OnMoveXAxis);
  AEditor.AddArrow(FOrigin, FYAxis, @OnMoveYAxis);
  AEditor.AddArrow(FOrigin, FOrigin - (FXAxis-FOrigin), @OnMoveXAxisNeg);
  AEditor.AddArrow(FOrigin, FOrigin - (FYAxis-FOrigin), @OnMoveYAxisNeg);
end;

procedure TRectShape.LoadFromStorage(AStorage: TBGRACustomOriginalStorage);
begin
  BeginUpdate;
  FOrigin := AStorage.PointF['origin'];
  FXAxis := AStorage.PointF['x-axis'];
  FYAxis := AStorage.PointF['y-axis'];
  FPenColor := AStorage.Color['pen-color'];
  FBackColor := AStorage.Color['back-color'];
  FPenWidth:= AStorage.Float['pen-width'];
  EndUpdate;
end;

procedure TRectShape.SaveToStorage(AStorage: TBGRACustomOriginalStorage);
begin
  AStorage.PointF['origin'] := FOrigin;
  AStorage.PointF['x-axis'] := FXAxis;
  AStorage.PointF['y-axis'] := FYAxis;
  AStorage.Color['pen-color'] := FPenColor;
  AStorage.Color['back-color'] := FBackColor;
  if PenVisible then
    AStorage.Float['pen-width'] := FPenWidth
  else
    AStorage.Float['pen-width'] := 0;
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

procedure TVectorShape.BeginUpdate;
begin
  FUpdateCount += 1;
  FBoundsBeforeUpdate := GetRenderBounds(AffineMatrixIdentity);
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
        boundsAfter := GetRenderBounds(AffineMatrixIdentity);
        FOnChange(self, boundsAfter.Union(FBoundsBeforeUpdate, true));
      end;
    end;
  end;
end;

function TVectorShape.ComputeStroke(APoints: ArrayOfTPointF; AClosed: boolean): ArrayOfTPointF;
var
  opt: TBGRAPolyLineOptions;
begin
  if AClosed then opt := [plCycle] else opt := [];
  result := ComputeWidePolylinePoints(APoints, PenWidth, PenColor, pecRound, pjsRound, SolidPenStyle, opt);
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
  DiscardFrozenShapes;
  NotifyChange(AShape.GetRenderBounds(AffineMatrixIdentity));
end;

function TVectorOriginal.RemoveShape(AShape: TVectorShape): boolean;
var
  idx: LongInt;
  r: TRectF;
begin
  idx := FShapes.IndexOf(AShape);
  if idx = -1 then exit(false);
  if AShape = SelectedShape then DeselectShape;
  r := AShape.GetRenderBounds(AffineMatrixIdentity);
  FShapes.Delete(idx);
  FDeletedShapes.Add(AShape);
  DiscardFrozenShapes;
  NotifyChange(r);
end;

procedure TVectorOriginal.SelectShape(AIndex: integer);
var
  prev: TVectorShape;
begin
  if (AIndex < 0) or (AIndex >= FShapes.Count) then
    raise ERangeError.Create('Index out of bounds');
  if FSelectedShape <> FShapes[AIndex] then
  begin
    prev := FSelectedShape;
    FSelectedShape := FShapes[AIndex];
    DiscardFrozenShapes;
    NotifyEditorChange;
    if Assigned(FOnSelectShape) then
      FOnSelectShape(self, FSelectedShape, prev);
  end;
end;

procedure TVectorOriginal.DeselectShape;
var
  prev: TVectorShape;
begin
  if FSelectedShape <> nil then
  begin
    prev := FSelectedShape;
    FSelectedShape := nil;
    DiscardFrozenShapes;
    NotifyEditorChange;
    if Assigned(FOnSelectShape) then
      FOnSelectShape(self, FSelectedShape, prev);
  end;
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

function TVectorOriginal.GetRenderBounds(ADestRect: TRect;
  AMatrix: TAffineMatrix): TRect;
var
  area, shapeArea: TRectF;
  i: Integer;
begin
  area:= EmptyRectF;

  for i:= 0 to FShapes.Count-1 do
  begin
    shapeArea := FShapes[i].GetRenderBounds(AMatrix);
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

