// SPDX-License-Identifier: GPL-3.0-only
unit LCVectorMultishape;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCVectorRectShapes, LCVectorOriginal, BGRALayerOriginal,
  BGRATransform, BGRABitmap, BGRABitmapTypes, BGRAPen, fgl, LCVectorialFill;

type
  TShapeChangeHandlerMap = specialize TFPGMap<Pointer, TShapeChangeEvent>;
  TShapeDiffMap = specialize TFPGMap<integer, TVectorShapeDiff>;
  TIntegerList = specialize TFPGList<integer>;

  { TMultiSelectionShapesDiff }

  TMultiSelectionShapesDiff = class(TCustomMultiSelectionDiff)
  protected
    FDiffs: TShapeDiffMap;
    FSelectedIds: TIntegerList;
    function GetShapeById(AContainer: TVectorShape; AId: integer): TVectorShape;
    function GetShapeCount: integer; override;
    function GetShapeId(AIndex: integer): integer; override;
  public
    constructor Create(AStartShape: TVectorShape); override;
    procedure ComputeDiff(AEndShape: TVectorShape); override;
    procedure Apply(AStartShape: TVectorShape); override;
    procedure Unapply(AEndShape: TVectorShape); override;
    function CanAppend(ADiff: TVectorShapeDiff): boolean; override;
    procedure Append(ADiff: TVectorShapeDiff); override; //does not preserve ADiff
    procedure AppendForShape(AShape: TVectorShape; var ADiff: TVectorShapeDiff); //does not preserve ADiff
    function IsIdentity: boolean; override;
    destructor Destroy; override;
    property ShapeCount: integer read GetShapeCount;
    property ShapeId[AIndex: integer]: integer read GetShapeId;
  end;

  { TVectorMultiselection }

  TVectorMultiselection = class(TCustomRectShape, IVectorMultishape)
  protected
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
    function _AddRef: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
    function _Release: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
  protected
    FShapes: TVectorShapes;
    FOldChangeHandler: TShapeChangeHandlerMap;
    FOnSelectionChange: TNotifyEvent;
    FUpdatingFromShape, FInMultiTranformFill: Boolean;
    function GetCornerPositition: single; override;
    procedure RestoreChangeHandler(AShape: TVectorShape);
    procedure AttachChangeHandler(AShape: TVectorShape);
    procedure ContainedShape_Change(ASender: TObject; ABounds: TRectF; ADiff: TVectorShapeDiff);
    procedure UpdateFromShapes;
    procedure UpdateFrameFromShapes;
    procedure FillChange({%H-}ASender: TObject; var ADiff: TCustomVectorialFillDiff); override;
    procedure FillBeforeChange({%H-}ASender: TObject); override;
    procedure SetPenStyle(AValue: TBGRAPenStyle); override;
    procedure SetPenWidth(AValue: single); override;
    procedure SetJoinStyle(AValue: TPenJoinStyle); override;
    procedure SetOutlineWidth(AValue: single); override;
    function GetIsFront: boolean; override;
    function GetIsBack: boolean; override;
    function GetPenVisible(AAssumePenFill: boolean = False): boolean; override;
    function GetBackVisible: boolean; override;
    function GetOutlineVisible: boolean; override;
    procedure NotifySelectionChanged;
    procedure InternalMoveToIndex(AFirst: integer);
  public
    constructor Create(AContainer: TVectorOriginal); override;
    class function StorageClassName: RawByteString; override;
    destructor Destroy; override;
    procedure BeginUpdate(ADiffHandler: TVectorShapeDiffAny=nil); override;
    procedure EndUpdate; override;
    procedure BringToFront; override;
    procedure SendToBack; override;
    procedure MoveUp(APassNonIntersectingShapes: boolean); override;
    procedure MoveDown(APassNonIntersectingShapes: boolean); override;
    procedure ClearShapes;
    procedure AddShape(AShape: TVectorShape);
    procedure RemoveShape(AShape: TVectorShape);
    function ContainsShape(AShape: TVectorShape): boolean;
    function ShapeCount: integer;
    function GetShape(AIndex: integer): TVectorShape;
    function SetShapes(AShapes: TVectorShapes): boolean;
    function FrontShape: TVectorShape;
    function BackShape: TVectorShape;
    function GetShapeById(AId: integer): TVectorShape;
    function MultiFields: TVectorShapeFields; override;
    procedure TransformFrame(const AMatrix: TAffineMatrix); override;
    procedure TransformFill(const AMatrix: TAffineMatrix; ABackOnly: boolean); override;
    function AllowShearTransform: boolean; override;
    procedure LoadFromStorage(AStorage: TBGRACustomOriginalStorage); override;
    procedure SaveToStorage(AStorage: TBGRACustomOriginalStorage); override;
    procedure Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix; ADraft: boolean); overload; override;
    procedure Render(ADest: TBGRABitmap; ARenderOffset: TPoint; AMatrix: TAffineMatrix; ADraft: boolean); overload; override;
    function GetRenderBounds({%H-}ADestRect: TRect; AMatrix: TAffineMatrix; {%H-}AOptions: TRenderBoundsOptions = []): TRectF; override;
    function GetAlignBounds(const ALayoutRect: TRect; const AMatrix: TAffineMatrix): TRectF; override;
    function SuggestGradientBox(AMatrix: TAffineMatrix): TAffineBox; override;
    procedure ConfigureCustomEditor(AEditor: TBGRAOriginalEditor); override;
    function PointInShape(APoint: TPointF): boolean; overload; override;
    function PointInShape(APoint: TPointF; ARadius: single): boolean; overload; override;
    function PointInBack(APoint: TPointF): boolean; overload; override;
    function PointInPen(APoint: TPointF): boolean; overload; override;
    function GetIsSlow(const AMatrix: TAffineMatrix): boolean; override;
    function GetAsMultishape: IVectorMultishape; override;
    procedure SetOnSelectionChange(AHandler: TNotifyEvent);
    function GetOnSelectionChange: TNotifyEvent;
    property OnSelectionChange: TNotifyEvent read GetOnSelectionChange write SetOnSelectionChange;
  end;

implementation

function Shapes_CompareDepth(const Item1, Item2: TVectorShape): Integer;
var
  idx1, idx2: Integer;
begin
  if Assigned(Item1.Container) and Assigned(Item2.Container) then
  begin
    idx1 := Item1.Container.IndexOfShape(Item1);
    idx2 := Item2.Container.IndexOfShape(Item2);
    result := idx1 - idx2;
  end;
end;

{ TMultiSelectionShapesDiff }

function TMultiSelectionShapesDiff.GetShapeCount: integer;
begin
  result := FSelectedIds.Count;
end;

function TMultiSelectionShapesDiff.GetShapeId(AIndex: integer): integer;
begin
  result := FSelectedIds.Items[AIndex];
end;

function TMultiSelectionShapesDiff.GetShapeById(AContainer: TVectorShape; AId: integer): TVectorShape;
begin
  result := AContainer.Container.FindShapeById(AId);
end;

constructor TMultiSelectionShapesDiff.Create(AStartShape: TVectorShape);
var
  i: Integer;
begin
  if not (AStartShape is TVectorMultiselection) then
    raise exception.Create('Expecting TVectorMultishape');
  FDiffs := TShapeDiffMap.Create;
  FSelectedIds := TIntegerList.Create;
  with AStartShape.GetAsMultishape do
  begin
    for i := 0 to ShapeCount-1 do
      FSelectedIds.Add(GetShape(i).Id);
  end;
end;

procedure TMultiSelectionShapesDiff.ComputeDiff(AEndShape: TVectorShape);
begin
  //nothing
end;

procedure TMultiSelectionShapesDiff.Apply(AStartShape: TVectorShape);
var
  s: TVectorShape;
  i: Integer;
begin
  for i := 0 to FDiffs.Count-1 do
  begin
    s := GetShapeById(AStartShape, FDiffs.Keys[i]);
    if Assigned(s) then
      FDiffs.Data[i].Apply(s);
  end;
end;

procedure TMultiSelectionShapesDiff.Unapply(AEndShape: TVectorShape);
var
  s: TVectorShape;
  i: Integer;
begin
  for i := FDiffs.Count-1 downto 0 do
  begin
    s := GetShapeById(AEndShape, FDiffs.Keys[i]);
    if Assigned(s) then
      FDiffs.Data[i].Unapply(s);
  end;
end;

function TMultiSelectionShapesDiff.CanAppend(ADiff: TVectorShapeDiff): boolean;
var
  other: TMultiSelectionShapesDiff;
  i, j: Integer;
begin
  if not (ADiff is TMultiSelectionShapesDiff) then exit(false);
  other := TMultiSelectionShapesDiff(ADiff);
  for i := 0 to other.FDiffs.Count-1 do
    for j := 0 to FDiffs.Count-1 do
      if FDiffs.Keys[j] = other.FDiffs.Keys[i] then
      begin
        if not FDiffs.Data[j].CanAppend(other.FDiffs.Data[i]) then
          exit(false);
      end;
  result := true;
end;

procedure TMultiSelectionShapesDiff.Append(ADiff: TVectorShapeDiff);
var found: boolean;
  other: TMultiSelectionShapesDiff;
  otherKey, i, j: integer;
  otherData: TVectorShapeDiff;
  toCopy: TShapeDiffMap;
begin
  if not (ADiff is TMultiSelectionShapesDiff) then raise exception.Create('Unexpected diff type');
  other := TMultiSelectionShapesDiff(ADiff);
  toCopy := TShapeDiffMap.Create;
  for i := 0 to other.FDiffs.Count-1 do
  begin
    found := false;
    otherKey := other.FDiffs.Keys[i];
    otherData := other.FDiffs.Data[i];
    for j := 0 to FDiffs.Count-1 do
      if FDiffs.Keys[j] = otherKey then
      begin
        FDiffs.Data[j].Append(otherData);
        found := true;
        break;
      end;
    if not found then toCopy.Add(otherKey, otherData);
  end;
  for i := 0 to toCopy.Count-1 do
  begin
    FDiffs.Add(toCopy.Keys[i], toCopy.Data[i]);
    other.FDiffs.Remove(toCopy.Keys[i]);
  end;
  toCopy.Free;
end;

procedure TMultiSelectionShapesDiff.AppendForShape(AShape: TVectorShape;
  var ADiff: TVectorShapeDiff);
var
  idx: Integer;
begin
  idx := FDiffs.IndexOf(AShape.Id);
  if idx <> -1 then
    FDiffs.Data[idx].Append(ADiff)
  else
  begin
    FDiffs.Add(AShape.Id, ADiff);
    ADiff := nil;
  end;
end;

function TMultiSelectionShapesDiff.IsIdentity: boolean;
var
  i: Integer;
begin
  for i := 0 to FDiffs.Count-1 do
    if not FDiffs.Data[i].IsIdentity then exit(false);
  result := true;
end;

destructor TMultiSelectionShapesDiff.Destroy;
var
  i: Integer;
begin
  for i := 0 to FDiffs.Count-1 do
    FDiffs.Data[i].Free;
  FDiffs.Free;
  FSelectedIds.Free;
  inherited Destroy;
end;

{ TVectorMultiselection }

function TVectorMultiselection.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
begin
  if GetInterface(iid, obj) then
    Result := S_OK
  else
    Result := longint(E_NOINTERFACE);
end;

function TVectorMultiselection._AddRef: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
begin
  result := 0;
end;

function TVectorMultiselection._Release: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
begin
  result := 0;
end;

procedure TVectorMultiselection.TransformFill(const AMatrix: TAffineMatrix;
  ABackOnly: boolean);
var
  i: Integer;
begin
  FInMultiTranformFill := true;
  BeginUpdate(TMultiSelectionShapesDiff);
  for i := 0 to FShapes.Count-1 do
  begin
    FShapes[i].BeginUpdate;
    FShapes[i].TransformFrame(AMatrix);
    FShapes[i].TransformFill(AMatrix, ABackOnly);
    FShapes[i].EndUpdate;
  end;
  inherited TransformFill(AMatrix, ABackOnly);
  EndUpdate;
  FInMultiTranformFill := false;
end;

procedure TVectorMultiselection.ContainedShape_Change(ASender: TObject;
  ABounds: TRectF; ADiff: TVectorShapeDiff);
var
  contained: TMultiSelectionShapesDiff;
begin
  if Assigned(ADiff) then
  begin
    contained := TMultiSelectionShapesDiff(AddDiffHandler(TMultiSelectionShapesDiff));
    contained.AppendForShape(ASender as TVectorShape, ADiff);
    ADiff.Free;
  end else
  begin
    if not IsUpdating and Assigned(OnChange) then
      OnChange(self, ABounds, nil);
  end;
end;

procedure TVectorMultiselection.UpdateFromShapes;
var
  i: Integer;
  found: boolean;
begin
  if FShapes.Count > 0 then
  begin
    FUpdatingFromShape := true;
    BeginEditingUpdate;
    UpdateFrameFromShapes;

    found := false;
    for i := 0 to FShapes.Count-1 do
      if (vsfPenFill in FShapes[i].Fields) and
         FShapes[i].PenVisible then
      begin
        PenFill.Assign(FShapes[i].PenFill);
        found := true;
        break;
      end;
    if not found then PenFill.Clear;

    found := false;
    for i := 0 to FShapes.Count-1 do
      if (vsfPenStyle in FShapes[i].Fields) and
         FShapes[i].PenVisible then
      begin
        Stroker.CustomPenStyle := FShapes[i].PenStyle;
        found := true;
        break;
      end;
    if not found then Stroker.CustomPenStyle := ClearPenStyle;

    for i := 0 to FShapes.Count-1 do
      if (vsfPenWidth in FShapes[i].Fields) and
         FShapes[i].PenVisible then
      begin
        FPenWidth := FShapes[i].PenWidth;
        break;
      end;

    for i := 0 to FShapes.Count-1 do
      if vsfJoinStyle in FShapes[i].Fields then
      begin
        Stroker.JoinStyle := FShapes[i].JoinStyle;
        break;
      end;

    found := false;
    for i := 0 to FShapes.Count-1 do
      if (vsfBackFill in FShapes[i].Fields) and
         FShapes[i].BackVisible then
      begin
        BackFill.Assign(FShapes[i].BackFill);
        found := true;
        break;
      end;
    if not found then BackFill.Clear;

    found := false;
    for i := 0 to FShapes.Count-1 do
      if (vsfOutlineFill in FShapes[i].Fields) and
         FShapes[i].OutlineVisible then
      begin
        OutlineFill.Assign(FShapes[i].OutlineFill);
        found := true;
        break;
      end;
    if not found then OutlineFill.Clear;

    for i := 0 to FShapes.Count-1 do
      if (vsfOutlineWidth in FShapes[i].Fields) and
         FShapes[i].OutlineVisible then
      begin
        FOutlineWidth:= FShapes[i].OutlineWidth;
        break;
      end;

    EndEditingUpdate;
    FUpdatingFromShape := false;
  end;
end;

procedure TVectorMultiselection.UpdateFrameFromShapes;
var
  rF: TRectF;
  i: Integer;
begin
  BeginEditingUpdate;
  rF := EmptyRectF;
  for i := 0 to FShapes.Count-1 do
    rF := rF.Union(FShapes[i].GetAlignBounds(InfiniteRect, AffineMatrixIdentity), true);
  FOrigin := (rF.TopLeft + rf.BottomRight)*0.5;
  FXAxis := FOrigin + PointF(rF.Width/2, 0);
  FYAxis := FOrigin + PointF(0, rF.Height/2);
  EndEditingUpdate;
end;

procedure TVectorMultiselection.FillChange(ASender: TObject;
  var ADiff: TCustomVectorialFillDiff);
var
  i: Integer;
begin
  if FUpdatingFromShape or FInMultiTranformFill then exit;
  if FFillChangeWithoutUpdate then exit;
  BeginUpdate;
  AddFillDiffHandler(ASender as TVectorialFill, ADiff);
  if ASender = PenFill then
  begin
    for i := 0 to FShapes.Count-1 do
      if vsfPenFill in FShapes[i].Fields then
      begin
        if not PenFill.IsFullyTransparent or FShapes[i].BackVisible or FShapes[i].OutlineVisible then
        FShapes[i].PenFill.Assign(PenFill);
      end;
  end else
  if ASender = BackFill then
  begin
    for i := 0 to FShapes.Count-1 do
      if vsfBackFill in FShapes[i].Fields then
      begin
        if not BackFill.IsFullyTransparent or FShapes[i].PenVisible or FShapes[i].OutlineVisible then
          FShapes[i].BackFill.Assign(BackFill);
      end;
  end else
  if ASender = OutlineFill then
  begin
    for i := 0 to FShapes.Count-1 do
      if vsfOutlineFill in FShapes[i].Fields then
      begin
        if not OutlineFill.IsFullyTransparent or FShapes[i].PenVisible or FShapes[i].BackVisible then
          FShapes[i].OutlineFill.Assign(OutlineFill);
      end;
  end;
  EndUpdate;
end;

procedure TVectorMultiselection.FillBeforeChange(ASender: TObject);
begin
  //nothing
end;

procedure TVectorMultiselection.SetPenStyle(AValue: TBGRAPenStyle);
var
  i: Integer;
begin
  if PenStyleEqual(AValue, PenStyle) then exit;
  BeginUpdate;
  inherited SetPenStyle(AValue);
  for i := 0 to FShapes.Count-1 do
    if vsfPenStyle in FShapes[i].Fields then
    begin
      if not IsClearPenStyle(AValue) or FShapes[i].BackVisible then
        FShapes[i].PenStyle := AValue;
    end;
  EndUpdate;
end;

procedure TVectorMultiselection.SetPenWidth(AValue: single);
var
  i: Integer;
begin
  if AValue < 0 then AValue := 0;
  if AValue = PenWidth then exit;
  BeginUpdate;
  inherited SetPenWidth(AValue);
  for i := 0 to FShapes.Count-1 do
    if vsfPenWidth in FShapes[i].Fields then
    begin
      if (AValue > 0) or FShapes[i].BackVisible then
        FShapes[i].PenWidth := AValue;
    end;
  EndUpdate;
end;

procedure TVectorMultiselection.SetJoinStyle(AValue: TPenJoinStyle);
var
  i: Integer;
begin
  if AValue = JoinStyle then exit;
  BeginUpdate;
  inherited SetJoinStyle(AValue);
  for i := 0 to FShapes.Count-1 do
    if vsfJoinStyle in FShapes[i].Fields then
      FShapes[i].JoinStyle := AValue;
  EndUpdate;
end;

procedure TVectorMultiselection.SetOutlineWidth(AValue: single);
var
  i: Integer;
begin
  if AValue < 0 then AValue := 0;
  if AValue = OutlineWidth then exit;
  BeginUpdate;
  inherited SetOutlineWidth(AValue);
  for i := 0 to FShapes.Count-1 do
    if vsfOutlineWidth in FShapes[i].Fields then
      FShapes[i].OutlineWidth := AValue;
  EndUpdate;
end;

function TVectorMultiselection.GetIsFront: boolean;
var
  i, containerIdx: Integer;
  s: TVectorShape;
begin
  s := FrontShape;
  if not Assigned(s) or not s.IsFront then exit(false);
  containerIdx := Container.IndexOfShape(s);
  for i := FShapes.Count-2 downto 0 do
  begin
    dec(containerIdx);
    if Container.IndexOfShape(FShapes[i]) <> containerIdx then
      exit(false);
  end;
  result := true;
end;

function TVectorMultiselection.GetIsBack: boolean;
var
  i, containerIdx: Integer;
  s: TVectorShape;
begin
  s := BackShape;
  if not Assigned(s) or not s.IsBack then exit(false);
  containerIdx := Container.IndexOfShape(s);
  for i := 1 to FShapes.Count-1 do
  begin
    inc(containerIdx);
    if Container.IndexOfShape(FShapes[i]) <> containerIdx then
      exit(false);
  end;
  result := true;
end;

function TVectorMultiselection.GetPenVisible(AAssumePenFill: boolean): boolean;
var
  i: Integer;
begin
  for i := 0 to ShapeCount-1 do
    if FShapes[i].PenVisible then exit(true);
  result := false;
end;

function TVectorMultiselection.GetBackVisible: boolean;
var
  i: Integer;
begin
  for i := 0 to ShapeCount-1 do
    if FShapes[i].BackVisible then exit(true);
  result := false;
end;

function TVectorMultiselection.GetOutlineVisible: boolean;
var
  i: Integer;
begin
  for i := 0 to ShapeCount-1 do
    if FShapes[i].OutlineVisible then exit(true);
  Result:= false;
end;

procedure TVectorMultiselection.NotifySelectionChanged;
begin
  if OnSelectionChange <> nil then
    OnSelectionChange(self);
end;

procedure TVectorMultiselection.InternalMoveToIndex(AFirst: integer);
var fromIndex, toIndex: array of integer;
  i: Integer;
begin
  if Container = nil then exit;
  setlength(fromIndex, ShapeCount);
  setlength(toIndex, ShapeCount);
  for i := 0 to ShapeCount-1 do
  begin
    fromIndex[i] := Container.IndexOfShape(FShapes[i]);
    toIndex[i] := AFirst + i;
  end;
  Container.MoveShapeToIndex(fromIndex, toIndex);
end;

function TVectorMultiselection.GetCornerPositition: single;
begin
  result := 1;
end;

procedure TVectorMultiselection.RestoreChangeHandler(AShape: TVectorShape);
var
  handlerIndex: Integer;
begin
  if AShape.OnChange <> @ContainedShape_Change then exit;
  handlerIndex := FOldChangeHandler.IndexOf(AShape);
  if handlerIndex <> -1 then
  begin
    AShape.OnChange:= FOldChangeHandler.Data[handlerIndex];
    FOldChangeHandler.Delete(handlerIndex);
  end
  else
    AShape.OnChange:= nil;
end;

procedure TVectorMultiselection.AttachChangeHandler(AShape: TVectorShape);
begin
  if AShape.OnChange <> @ContainedShape_Change then
  begin
    FOldChangeHandler.Add(AShape, AShape.OnChange);
    AShape.OnChange:= @ContainedShape_Change;
  end;
end;

function TVectorMultiselection.AllowShearTransform: boolean;
var
  i: Integer;
begin
  for i := 0 to FShapes.Count-1 do
    if not FShapes[i].AllowShearTransform then exit(false);
  result := true;
end;

procedure TVectorMultiselection.LoadFromStorage(AStorage: TBGRACustomOriginalStorage);
begin
  raise exception.Create('Cannot be deserialized');
end;

procedure TVectorMultiselection.SaveToStorage(AStorage: TBGRACustomOriginalStorage);
begin
  raise exception.Create('Cannot be serialized');
end;

procedure TVectorMultiselection.Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix;
  ADraft: boolean);
var
  i: Integer;
begin
  for i := 0 to FShapes.Count-1 do
    FShapes[i].Render(ADest, AMatrix, ADraft);
end;

procedure TVectorMultiselection.Render(ADest: TBGRABitmap; ARenderOffset: TPoint;
  AMatrix: TAffineMatrix; ADraft: boolean);
var
  i: Integer;
begin
  for i := 0 to FShapes.Count-1 do
    FShapes[i].Render(ADest, ARenderOffset, AMatrix, ADraft);
end;

function TVectorMultiselection.GetRenderBounds(ADestRect: TRect;
  AMatrix: TAffineMatrix; AOptions: TRenderBoundsOptions): TRectF;
var
  i: Integer;
begin
  result := EmptyRectF;
  for i := 0 to FShapes.Count-1 do
    result := result.Union(FShapes[i].GetRenderBounds(ADestRect, AMatrix, AOptions), true);
end;

function TVectorMultiselection.GetAlignBounds(const ALayoutRect: TRect;
  const AMatrix: TAffineMatrix): TRectF;
var
  i: Integer;
begin
  result := EmptyRectF;
  for i := 0 to FShapes.Count-1 do
    result := result.Union(FShapes[i].GetAlignBounds(ALayoutRect, AMatrix), true);
end;

function TVectorMultiselection.SuggestGradientBox(AMatrix: TAffineMatrix): TAffineBox;
var
  i: Integer;
  r: TRectF;
begin
  if FShapes.Count = 1 then
    result := FShapes[0].SuggestGradientBox(AMatrix)
  else
  begin
    r := EmptyRectF;
    for i := 0 to FShapes.Count-1 do
      r := r.Union(FShapes[i].SuggestGradientBox(AMatrix).RectBoundsF, true);
    result := TAffineBox.AffineBox(r);
  end;
end;

procedure TVectorMultiselection.ConfigureCustomEditor(AEditor: TBGRAOriginalEditor);
var
  i: Integer;
  ab: TAffineBox;
begin
  for i := 0 to FShapes.Count-1 do
  begin
    ab := FShapes[i].SuggestGradientBox(AffineMatrixIdentity);
    AEditor.AddPolyline(ab.AsPolygon, true, opsDash);
  end;
  inherited ConfigureCustomEditor(AEditor);
end;

function TVectorMultiselection.PointInShape(APoint: TPointF): boolean;
var
  i: LongInt;
begin
  for i := FShapes.Count-1 downto 0 do
    if FShapes[i].PointInShape(APoint) then exit(true);
  result := false;
end;

function TVectorMultiselection.PointInShape(APoint: TPointF; ARadius: single): boolean;
var
  i: LongInt;
begin
  for i := FShapes.Count-1 downto 0 do
    if FShapes[i].PointInShape(APoint, ARadius) then exit(true);
  result := false;
end;

function TVectorMultiselection.PointInBack(APoint: TPointF): boolean;
var
  i: LongInt;
begin
  for i := FShapes.Count-1 downto 0 do
    if FShapes[i].PointInBack(APoint) then exit(true);
  result := false;
end;

function TVectorMultiselection.PointInPen(APoint: TPointF): boolean;
var
  i: LongInt;
begin
  for i := FShapes.Count-1 downto 0 do
    if FShapes[i].PointInPen(APoint) then exit(true);
  result := false;
end;

function TVectorMultiselection.GetIsSlow(const AMatrix: TAffineMatrix): boolean;
var
  i: LongInt;
begin
  if FShapes.Count >= 5 then exit(true);
  for i := 0 to FShapes.Count-1 do
    if FShapes[i].GetIsSlow(AMatrix) then exit(true);
  result := false;
end;

function TVectorMultiselection.GetAsMultishape: IVectorMultishape;
begin
  Result:= self;
end;

procedure TVectorMultiselection.SetOnSelectionChange(AHandler: TNotifyEvent);
begin
  FOnSelectionChange := AHandler;
end;

function TVectorMultiselection.GetOnSelectionChange: TNotifyEvent;
begin
  result := FOnSelectionChange;
end;

class function TVectorMultiselection.StorageClassName: RawByteString;
begin
  result := 'multishape';
end;

procedure TVectorMultiselection.ClearShapes;
var
  i: Integer;
begin
  if FShapes.Count = 0 then exit;
  for i := 0 to FShapes.Count-1 do
    RestoreChangeHandler(FShapes[i]);
  FShapes.Clear;
  UpdateFromShapes;
  NotifySelectionChanged;
end;

procedure TVectorMultiselection.AddShape(AShape: TVectorShape);
begin
  if ContainsShape(AShape) then exit;
  FShapes.Add(AShape);
  FShapes.Sort(@Shapes_CompareDepth);
  AttachChangeHandler(AShape);
  UpdateFromShapes;
  NotifySelectionChanged;
end;

procedure TVectorMultiselection.RemoveShape(AShape: TVectorShape);
begin
  RestoreChangeHandler(AShape);
  FShapes.Remove(AShape);
  UpdateFromShapes;
  NotifySelectionChanged;
end;

function TVectorMultiselection.ContainsShape(AShape: TVectorShape): boolean;
begin
  result := FShapes.IndexOf(AShape) <> -1;
end;

function TVectorMultiselection.ShapeCount: integer;
begin
  result := FShapes.Count;
end;

function TVectorMultiselection.GetShape(AIndex: integer): TVectorShape;
begin
  if (AIndex < 0) or (AIndex >= FShapes.Count) then
    raise exception.Create('Index out of bounds');
  result := FShapes[AIndex];
end;

function TVectorMultiselection.SetShapes(AShapes: TVectorShapes): boolean;
var
  i: Integer;
  different: Boolean;
begin
  different := false;
  for i := 0 to FShapes.Count-1 do
    if AShapes.IndexOf(FShapes[i]) = -1 then
    begin
      different := true;
      break;
    end;
  for i := 0 to AShapes.Count-1 do
    if FShapes.IndexOf(AShapes[i]) = -1 then
    begin
      different := true;
      break;
    end;
  if not different then exit(false);

  for i := 0 to FShapes.Count-1 do
    RestoreChangeHandler(FShapes[i]);
  FShapes.Clear;

  for i := 0 to AShapes.Count-1 do
  begin
    FShapes.Add(AShapes[i]);
    AttachChangeHandler(AShapes[i]);
  end;
  FShapes.Sort(@Shapes_CompareDepth);
  UpdateFromShapes;
  NotifySelectionChanged;
  exit(true);
end;

function TVectorMultiselection.FrontShape: TVectorShape;
begin
  if FShapes.Count > 0 then
    result := FShapes[FShapes.Count-1]
    else result := nil;
end;

function TVectorMultiselection.BackShape: TVectorShape;
begin
  if FShapes.Count > 0 then
    result := FShapes[0]
    else result := nil;
end;

function TVectorMultiselection.GetShapeById(AId: integer): TVectorShape;
var
  i: Integer;
begin
  for i := 0 to FShapes.Count-1 do
    if FShapes[i].Id = AId then exit(FShapes[i]);
  result := nil;
end;

function TVectorMultiselection.MultiFields: TVectorShapeFields;
var
  i: Integer;
begin
  result := [];
  for i := 0 to FShapes.Count-1 do
    result += FShapes[i].Fields;
end;

constructor TVectorMultiselection.Create(AContainer: TVectorOriginal);
begin
  inherited Create(AContainer);
  FShapes := TVectorShapes.Create;
  FOldChangeHandler := TShapeChangeHandlerMap.Create;
  FDisableHitBox:= true;
end;

procedure TVectorMultiselection.TransformFrame(const AMatrix: TAffineMatrix);
var
  i: Integer;
begin
  BeginUpdate(TMultiSelectionShapesDiff);
  for i := 0 to FShapes.Count-1 do
    FShapes[i].TransformFrame(AMatrix);
  inherited TransformFrame(AMatrix);
  EndUpdate;
end;

destructor TVectorMultiselection.Destroy;
begin
  ClearShapes;
  FShapes.Free;
  FOldChangeHandler.Free;
  inherited Destroy;
end;

procedure TVectorMultiselection.BeginUpdate(ADiffHandler: TVectorShapeDiffAny);
var
  i: Integer;
begin
  inherited BeginUpdate(ADiffHandler);
  for i := 0 to FShapes.Count-1 do
    FShapes[i].BeginUpdate;
end;

procedure TVectorMultiselection.EndUpdate;
var
  i: Integer;
begin
  for i := 0 to FShapes.Count-1 do
    FShapes[i].EndUpdate;
  inherited EndUpdate;
end;

procedure TVectorMultiselection.BringToFront;
begin
  if Assigned(Container) then
    InternalMoveToIndex(Container.ShapeCount - ShapeCount);
end;

procedure TVectorMultiselection.SendToBack;
begin
  InternalMoveToIndex(0);
end;

procedure TVectorMultiselection.MoveUp(APassNonIntersectingShapes: boolean);
var
  topIndex, i: Integer;
  curBounds: TRectF;
  touch: Boolean;
begin
  if Container = nil then exit;
  topIndex := Container.IndexOfShape(FrontShape);
  while topIndex < Container.ShapeCount-1 do
  begin
    inc(topIndex);
    curBounds := Container.Shape[topIndex].GetAlignBounds(InfiniteRect, AffineMatrixIdentity);
    if not APassNonIntersectingShapes then break;
    touch := false;
    for i := 0 to ShapeCount-1 do
      if FShapes[i].GetAlignBounds(InfiniteRect, AffineMatrixIdentity).IntersectsWith(curBounds) then
      begin
        touch := true;
        break;
      end;
    if touch then break;
  end;
  InternalMoveToIndex(topIndex + 1 - ShapeCount);
end;

procedure TVectorMultiselection.MoveDown(APassNonIntersectingShapes: boolean);
var
  bottomIndex, i: Integer;
  curBounds: TRectF;
  touch: Boolean;
begin
  if Container = nil then exit;
  bottomIndex := Container.IndexOfShape(FrontShape);
  while bottomIndex > 0 do
  begin
    dec(bottomIndex);
    curBounds := Container.Shape[bottomIndex].GetAlignBounds(InfiniteRect, AffineMatrixIdentity);
    if not APassNonIntersectingShapes then break;
    touch := false;
    for i := 0 to ShapeCount-1 do
      if FShapes[i].GetAlignBounds(InfiniteRect, AffineMatrixIdentity).IntersectsWith(curBounds) then
      begin
        touch := true;
        break;
      end;
    if touch then break;
  end;
  InternalMoveToIndex(bottomIndex);
end;

initialization

  VectorMultiselectionFactory := TVectorMultiselection;

end.

