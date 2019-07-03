unit UToolLayer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UTool, BGRABitmap, BGRABitmapTypes,
  BGRATransform, BGRALayers, ULayerAction, UImageDiff;

type
  { TToolMoveLayer }

  TToolMoveLayer = class(TGenericTool)
  protected
    handMoving: boolean;
    handOrigin: TPoint;
    FStartLayerOffset: TPoint;
    FStartLayerMatrix: TAffineMatrix;
    FStartLayerOffsetDefined: boolean;
    FLayerBounds: TRect;
    FLayerBoundsDefined: boolean;
    function GetIsSelectingTool: boolean; override;
    function DoToolDown({%H-}toolDest: TBGRABitmap; pt: TPoint; {%H-}ptF: TPointF;
      {%H-}rightBtn: boolean): TRect; override;
    function DoToolMove({%H-}toolDest: TBGRABitmap; pt: TPoint; {%H-}ptF: TPointF): TRect; override;
    procedure DoToolMoveAfter(pt: TPoint; {%H-}ptF: TPointF); override;
    function UseOriginal: boolean;
    procedure NeedLayerBounds;
    function GetAction: TLayerAction; override;
    function DoGetToolDrawingLayer: TBGRABitmap; override;
  public
    function ToolUp: TRect; override;
    function ToolKeyDown(var key: Word): TRect; override;
    function Render(VirtualScreen: TBGRABitmap; {%H-}VirtualScreenWidth,
      {%H-}VirtualScreenHeight: integer;
      BitmapToVirtualScreen: TBitmapToVirtualScreenFunction): TRect; override;
  end;

  { TToolRotateLayer }

  TToolRotateLayer = class(TGenericTool)
  private
    function GetActualAngle: single;
    function GetOriginalLayerBounds: TRect;
    function GetTransformCenter: TPointF;
    procedure SetTransformCenter(AValue: TPointF);
    procedure NeedOriginal;
  protected
    FOriginalInit: boolean;
    FBackupLayer: TReplaceLayerByImageOriginalDifference;
    FInitialOriginalMatrix: TAffineMatrix;
    FInitialLayerBounds: TRect;
    FInitialLayerBoundsDefined: boolean;

    FTransformCenter: TPointF;
    FTransformCenterDefined: boolean;
    FPreviousTransformCenter: TPointF;
    FAngle,FActualAngle,FPreviousActualAngle: single;
    FPreviousFilter: TResampleFilter;
    FTransforming: boolean;
    FPreviousMousePos: TPointF;
    FCtrlDown: boolean;
    FLastUpdateRect: TRect;
    FLastUpdateRectDefined: boolean;
    function GetIsSelectingTool: boolean; override;
    function DoToolDown({%H-}toolDest: TBGRABitmap; {%H-}pt: TPoint; ptF: TPointF;
      rightBtn: boolean): TRect; override;
    function DoToolMove({%H-}toolDest: TBGRABitmap; {%H-}pt: TPoint; ptF: TPointF): TRect;
      override;
    function UpdateTransform: TRect;
    procedure CancelTransform;
    procedure ValidateTransform;
    property TransformCenter: TPointF read GetTransformCenter write SetTransformCenter;
    function GetAction: TLayerAction; override;
    function DoGetToolDrawingLayer: TBGRABitmap; override;
  public
    constructor Create(AManager: TToolManager); override;
    destructor Destroy; override;
    function ToolKeyDown(var key: Word): TRect; override;
    function ToolKeyUp(var key: Word): TRect; override;
    function ToolUp: TRect; override;
    function Render(VirtualScreen: TBGRABitmap; {%H-}VirtualScreenWidth,
      {%H-}VirtualScreenHeight: integer;
      BitmapToVirtualScreen: TBitmapToVirtualScreenFunction): TRect; override;
  end;

  { TToolZoomLayer }

  TToolZoomLayer = class(TGenericTool)
  private
    function GetActualZoom: single;
    function GetOriginalLayerBounds: TRect;
    function GetTransformCenter: TPointF;
    procedure SetTransformCenter(AValue: TPointF);
    procedure NeedOriginal;
  protected
    FOriginalInit: boolean;
    FBackupLayer: TReplaceLayerByImageOriginalDifference;
    FInitialOriginalMatrix: TAffineMatrix;
    FInitialLayerBounds: TRect;
    FInitialLayerBoundsDefined: boolean;

    FTransformCenter: TPointF;
    FTransformCenterDefined: boolean;
    FPreviousTransformCenter: TPointF;
    FZoom,FActualZoom,FPreviousActualZoom: single;
    FPreviousFilter: TResampleFilter;
    FTransforming: boolean;
    FPreviousMousePos: TPointF;
    FCtrlDown: boolean;
    FLastUpdateRect: TRect;
    FLastUpdateRectDefined: boolean;
    function GetIsSelectingTool: boolean; override;
    function DoToolDown({%H-}toolDest: TBGRABitmap; {%H-}pt: TPoint; ptF: TPointF;
      rightBtn: boolean): TRect; override;
    function DoToolMove({%H-}toolDest: TBGRABitmap; {%H-}pt: TPoint; ptF: TPointF): TRect;
      override;
    function UpdateTransform: TRect;
    procedure CancelTransform;
    procedure ValidateTransform;
    property TransformCenter: TPointF read GetTransformCenter write SetTransformCenter;
    function GetAction: TLayerAction; override;
    function DoGetToolDrawingLayer: TBGRABitmap; override;
  public
    constructor Create(AManager: TToolManager); override;
    destructor Destroy; override;
    function ToolKeyDown(var key: Word): TRect; override;
    function ToolKeyUp(var key: Word): TRect; override;
    function ToolUp: TRect; override;
    function Render(VirtualScreen: TBGRABitmap; {%H-}VirtualScreenWidth,
      {%H-}VirtualScreenHeight: integer;
      BitmapToVirtualScreen: TBitmapToVirtualScreenFunction): TRect; override;
  end;

implementation

uses LazPaintType, ugraph, LCLType, Types, BGRALayerOriginal;

{ TToolRotateLayer }

function TToolRotateLayer.GetActualAngle: single;
begin
  if FCtrlDown then
    result := round(FAngle/15)*15
  else
    result := FAngle;
end;

function TToolRotateLayer.GetOriginalLayerBounds: TRect;
begin
  if not FInitialLayerBoundsDefined then
  begin
    FInitialLayerBounds := GetToolDrawingLayer.GetImageBounds;
    FInitialLayerBoundsDefined := true;
  end;
  result := FInitialLayerBounds;
end;

function TToolRotateLayer.GetTransformCenter: TPointF;
var bounds: TRect;
begin
  if not FTransformCenterDefined then
  begin
    bounds := GetOriginalLayerBounds;
    if IsRectEmpty(bounds) then
      FTransformCenter := PointF(Manager.Image.Width/2 - 0.5,Manager.Image.Height/2 - 0.5)
    else
    begin
      with bounds do
        FTransformCenter := PointF((Left+Right)/2 - 0.5, (Top+Bottom)/2 - 0.5);
      with Manager.Image.LayerOffset[Manager.Image.CurrentLayerIndex] do
        FTransformCenter += PointF(X,Y);
    end;
    FTransformCenterDefined := true;
  end;
  result := FTransformCenter;
end;

procedure TToolRotateLayer.SetTransformCenter(AValue: TPointF);
begin
  FTransformCenter := AValue;
end;

procedure TToolRotateLayer.NeedOriginal;
var
  layered: TBGRALayeredBitmap;
  layerIdx: Integer;
begin
  if FOriginalInit then exit;
  GetAction;
  layerIdx := Manager.Image.CurrentLayerIndex;
  layered := Manager.Image.CurrentState.LayeredBitmap;
  if not (Manager.Image.LayerOriginalDefined[layerIdx] and
     Manager.Image.LayerOriginalKnown[layerIdx]) then
  begin
    if Assigned(FBackupLayer) then raise exception.Create('Backup layer already assigned');
    FBackupLayer:= TReplaceLayerByImageOriginalDifference.Create(Manager.Image.CurrentState, layerIdx);
  end;
  FInitialOriginalMatrix := layered.LayerOriginalMatrix[layerIdx];
  FOriginalInit := true;
end;

function TToolRotateLayer.DoToolDown(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF; rightBtn: boolean): TRect;
begin
  with Manager.Image.LayerOffset[Manager.Image.CurrentLayerIndex] do
    ptF += PointF(X,Y);

  if not FTransforming and not rightBtn then
  begin
    FTransforming := true;
    FPreviousMousePos := ptF;
    if FCtrlDown then result := UpdateTransform
     else result := EmptyRect;
  end else
  if rightBtn then
  begin
    FTransformCenter := ptF;
    FAngle := 0;
    FActualAngle:= GetActualAngle;
    result := UpdateTransform;
    if IsRectEmpty(result) then result := OnlyRenderChange;
  end else
    result := EmptyRect;
end;

function TToolRotateLayer.DoToolMove(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF): TRect;
var angleDiff: single;
begin
  with Manager.Image.LayerOffset[Manager.Image.CurrentLayerIndex] do
    ptF += PointF(X,Y);
  if FTransforming then
  begin
    angleDiff := ComputeAngle(ptF.X-TransformCenter.X,ptF.Y-TransformCenter.Y)-
               ComputeAngle(FPreviousMousePos.X-TransformCenter.X,FPreviousMousePos.Y-TransformCenter.Y);
    FAngle += angleDiff;
    FActualAngle:= GetActualAngle;
    result := UpdateTransform;
    FPreviousMousePos := ptF;
  end else
    result := EmptyRect;
end;

function TToolRotateLayer.UpdateTransform: TRect;
begin
  if (FActualAngle = FPreviousActualAngle) and ((FActualAngle = 0) or (TransformCenter = FPreviousTransformCenter)) then
  begin
    result := EmptyRect;
    exit;
  end;
  FPreviousActualAngle := FActualAngle;
  FPreviousTransformCenter := TransformCenter;
  result := EmptyRect;
  NeedOriginal;
  Manager.Image.LayerOriginalMatrix[Manager.Image.CurrentLayerIndex] :=
    AffineMatrixTranslation(TransformCenter.X,TransformCenter.Y)*
    AffineMatrixRotationDeg(FActualAngle)*
    AffineMatrixTranslation(-TransformCenter.X,-TransformCenter.Y)*
    FInitialOriginalMatrix;
end;

procedure TToolRotateLayer.CancelTransform;
begin
  if FOriginalInit then
  begin
    Manager.Image.LayerOriginalMatrix[Manager.Image.CurrentLayerIndex] := FInitialOriginalMatrix;
    if Assigned(FBackupLayer) then
    begin
      FBackupLayer.UnapplyTo(Manager.Image.CurrentState);
      FreeAndNil(FBackupLayer);
    end;
    FOriginalInit := false;
  end;
  Manager.QueryExitTool;
end;

procedure TToolRotateLayer.ValidateTransform;
var
  transform: TAffineMatrix;
begin
  if FOriginalInit then
  begin
    if Assigned(FBackupLayer) then
    begin
      transform := Manager.Image.LayerOriginalMatrix[Manager.Image.CurrentLayerIndex];
      Manager.Image.LayerOriginalMatrix[Manager.Image.CurrentLayerIndex] := FInitialOriginalMatrix;
      Manager.Image.CurrentState.LayeredBitmap.LayerOriginalMatrix[Manager.Image.CurrentLayerIndex] := transform;
      Manager.Image.CurrentState.LayeredBitmap.RenderLayerFromOriginal(Manager.Image.CurrentLayerIndex);
      FBackupLayer.nextMatrix := transform;
      Manager.Image.AddUndo(FBackupLayer);
      FBackupLayer := nil;
    end;
    FOriginalInit := false;
  end;
  Manager.QueryExitTool;
end;

function TToolRotateLayer.GetAction: TLayerAction;
begin
  result := GetIdleAction;
end;

function TToolRotateLayer.DoGetToolDrawingLayer: TBGRABitmap;
begin
  Result:= Manager.Image.CurrentLayerReadOnly   //do not modify layer data directly and ignore selection
end;

constructor TToolRotateLayer.Create(AManager: TToolManager);
begin
  inherited Create(AManager);
  FCtrlDown:= false;
  FTransformCenterDefined := false;
  FLastUpdateRectDefined:= false;

  FAngle:= 0;
  FPreviousActualAngle := 0;
end;

destructor TToolRotateLayer.Destroy;
begin
  ValidateTransform;
  inherited Destroy;
end;

function TToolRotateLayer.ToolKeyDown(var key: Word): TRect;
begin
  if key = VK_CONTROL then
  begin
    FCtrlDown:= true;
    if FTransforming then
    begin
      FActualAngle := GetActualAngle;
      result := UpdateTransform;
    end
     else result := EmptyRect;
    Key := 0;
  end else
  if Key = VK_RETURN then
  begin
    if FActualAngle = 0 then CancelTransform
    else ValidateTransform;
    result := OnlyRenderChange;
    key := 0;
  end else
  if Key = VK_ESCAPE then
  begin
    CancelTransform;
    result := OnlyRenderChange;
    key := 0;
  end else
    result := EmptyRect;
end;

function TToolRotateLayer.ToolKeyUp(var key: Word): TRect;
begin
  if key = VK_CONTROL then
  begin
    FCtrlDown := false;
    Key := 0;
  end;
  result := EmptyRect;
end;

function TToolRotateLayer.ToolUp: TRect;
begin
  if FTransforming then
  begin
    FTransforming := false;
    result := UpdateTransform;
  end else
    Result:=EmptyRect;
end;

function TToolRotateLayer.Render(VirtualScreen: TBGRABitmap;
  VirtualScreenWidth, VirtualScreenHeight: integer;
  BitmapToVirtualScreen: TBitmapToVirtualScreenFunction): TRect;
begin
  with Manager.Image.LayerOffset[Manager.Image.CurrentLayerIndex] do
    Result:= NicePoint(VirtualScreen,BitmapToVirtualScreen(TransformCenter-PointF(X,Y)));
end;

function TToolRotateLayer.GetIsSelectingTool: boolean;
begin
  result := false;
end;

{ TToolZoomLayer }

function TToolZoomLayer.GetActualZoom: single;
const log125 = 0.321928095;
      log15 = 0.584962501;
var
  logZoom, fracZoom: single;
  baseZoom: single;
  invZoom: boolean;
begin
  if FCtrlDown then
  begin
    logZoom := ln(FZoom)/ln(2);
    if logZoom < 0 then
    begin
      invZoom := true;
      logZoom := -logZoom;
    end else invZoom := false;
    fracZoom := frac(logZoom);
    baseZoom := 1 shl trunc(logZoom);

    if fracZoom < log125/2 then result := baseZoom else
    if fracZoom < (log125+log15)/2 then result := baseZoom*1.25 else
    if fracZoom < (log15+1)/2 then result := baseZoom*1.5 else
      result := baseZoom*2;

    if invZoom then result := 1/result;
  end
  else
    result := FZoom;
end;

function TToolZoomLayer.GetOriginalLayerBounds: TRect;
begin
  if not FInitialLayerBoundsDefined then
  begin
    FInitialLayerBounds := GetToolDrawingLayer.GetImageBounds;
    FInitialLayerBoundsDefined := true;
  end;
  result := FInitialLayerBounds;
end;

function TToolZoomLayer.GetTransformCenter: TPointF;
var bounds: TRect;
begin
  if not FTransformCenterDefined then
  begin
    bounds := GetOriginalLayerBounds;
    if IsRectEmpty(bounds) then
      FTransformCenter := PointF(Manager.Image.Width/2 - 0.5,Manager.Image.Height/2 - 0.5)
    else
    begin
      with bounds do
        FTransformCenter := PointF((Left+Right)/2 - 0.5, (Top+Bottom)/2 - 0.5);
      with Manager.Image.LayerOffset[Manager.Image.CurrentLayerIndex] do
        FTransformCenter += PointF(X,Y);
    end;
    FTransformCenterDefined := true;
  end;
  result := FTransformCenter;
end;

procedure TToolZoomLayer.SetTransformCenter(AValue: TPointF);
begin
  FTransformCenter := AValue;
end;

procedure TToolZoomLayer.NeedOriginal;
var
  layered: TBGRALayeredBitmap;
  layerIdx: Integer;
begin
  if FOriginalInit then exit;
  GetAction;
  layerIdx := Manager.Image.CurrentLayerIndex;
  layered := Manager.Image.CurrentState.LayeredBitmap;
  if not (Manager.Image.LayerOriginalDefined[layerIdx] and
     Manager.Image.LayerOriginalKnown[layerIdx]) then
  begin
    if Assigned(FBackupLayer) then raise exception.Create('Backup layer already assigned');
    FBackupLayer:= TReplaceLayerByImageOriginalDifference.Create(Manager.Image.CurrentState, layerIdx);
  end;
  FInitialOriginalMatrix := layered.LayerOriginalMatrix[layerIdx];
  FOriginalInit := true;
end;

function TToolZoomLayer.DoToolDown(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF; rightBtn: boolean): TRect;
begin
  with Manager.Image.LayerOffset[Manager.Image.CurrentLayerIndex] do
    ptF += PointF(X,Y);

  if not FTransforming and not rightBtn then
  begin
    FTransforming := true;
    FPreviousMousePos := ptF;
    if FCtrlDown then result := UpdateTransform
     else result := EmptyRect;
  end else
  if rightBtn then
  begin
    FTransformCenter := ptF;
    FZoom := 1;
    FActualZoom:= GetActualZoom;
    result := UpdateTransform;
    if IsRectEmpty(result) then result := OnlyRenderChange;
  end else
    result := EmptyRect;
end;

function TToolZoomLayer.DoToolMove(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF): TRect;
var dist, prevDist: single;
begin
  with Manager.Image.LayerOffset[Manager.Image.CurrentLayerIndex] do
    ptF += PointF(X,Y);
  if FTransforming then
  begin
    dist := VectLen(ptF-TransformCenter);
    prevDist := VectLen(FPreviousMousePos-TransformCenter);
    if (prevDist <> 0) and (dist <> 0) then
    begin
      FZoom *= dist/prevDist;
      FActualZoom:= GetActualZoom;
      result := UpdateTransform;
    end
    else result := EmptyRect;
    FPreviousMousePos := ptF;
  end else
    result := EmptyRect;
end;

function TToolZoomLayer.UpdateTransform: TRect;
begin
  if (FActualZoom = FPreviousActualZoom) and ((FActualZoom = 1) or (TransformCenter = FPreviousTransformCenter)) then
  begin
    result := EmptyRect;
    exit;
  end;
  FPreviousActualZoom := FActualZoom;
  FPreviousTransformCenter := TransformCenter;
  result := EmptyRect;
  NeedOriginal;
  Manager.Image.LayerOriginalMatrix[Manager.Image.CurrentLayerIndex] :=
    AffineMatrixTranslation(TransformCenter.X,TransformCenter.Y)*
    AffineMatrixScale(FActualZoom,FActualZoom)*
    AffineMatrixTranslation(-TransformCenter.X,-TransformCenter.Y)*
    FInitialOriginalMatrix;
end;

procedure TToolZoomLayer.CancelTransform;
begin
  if FOriginalInit then
  begin
    Manager.Image.LayerOriginalMatrix[Manager.Image.CurrentLayerIndex] := FInitialOriginalMatrix;
    if Assigned(FBackupLayer) then
    begin
      FBackupLayer.UnapplyTo(Manager.Image.CurrentState);
      FreeAndNil(FBackupLayer);
    end;
    FOriginalInit := false;
  end;
  Manager.QueryExitTool;
end;

procedure TToolZoomLayer.ValidateTransform;
var
  transform: TAffineMatrix;
begin
  if FOriginalInit then
  begin
    if Assigned(FBackupLayer) then
    begin
      transform := Manager.Image.LayerOriginalMatrix[Manager.Image.CurrentLayerIndex];
      Manager.Image.LayerOriginalMatrix[Manager.Image.CurrentLayerIndex] := FInitialOriginalMatrix;
      Manager.Image.CurrentState.LayeredBitmap.LayerOriginalMatrix[Manager.Image.CurrentLayerIndex] := transform;
      Manager.Image.CurrentState.LayeredBitmap.RenderLayerFromOriginal(Manager.Image.CurrentLayerIndex);
      FBackupLayer.nextMatrix := transform;
      Manager.Image.AddUndo(FBackupLayer);
      FBackupLayer := nil;
    end;
    FOriginalInit := false;
  end;
  Manager.QueryExitTool;
end;

function TToolZoomLayer.GetAction: TLayerAction;
begin
  result := GetIdleAction;
end;

function TToolZoomLayer.DoGetToolDrawingLayer: TBGRABitmap;
begin
  Result:= Manager.Image.CurrentLayerReadOnly   //do not modify layer data directly and ignore selection
end;

constructor TToolZoomLayer.Create(AManager: TToolManager);
begin
  inherited Create(AManager);
  FCtrlDown:= false;
  FTransformCenterDefined := false;
  FLastUpdateRectDefined:= false;

  FZoom:= 1;
  FPreviousActualZoom := 1;
end;

destructor TToolZoomLayer.Destroy;
begin
  ValidateTransform;
  inherited Destroy;
end;

function TToolZoomLayer.ToolKeyDown(var key: Word): TRect;
begin
  if key = VK_CONTROL then
  begin
    FCtrlDown:= true;
    if FTransforming then
    begin
      FActualZoom := GetActualZoom;
      result := UpdateTransform;
    end
     else result := EmptyRect;
    Key := 0;
  end else
  if Key = VK_RETURN then
  begin
    if FActualZoom = 0 then CancelTransform
    else ValidateTransform;
    result := OnlyRenderChange;
    key := 0;
  end else
  if Key = VK_ESCAPE then
  begin
    CancelTransform;
    result := OnlyRenderChange;
    key := 0;
  end else
    result := EmptyRect;
end;

function TToolZoomLayer.ToolKeyUp(var key: Word): TRect;
begin
  if key = VK_CONTROL then
  begin
    FCtrlDown := false;
    Key := 0;
  end;
  result := EmptyRect;
end;

function TToolZoomLayer.ToolUp: TRect;
begin
  if FTransforming then
  begin
    FTransforming := false;
    result := UpdateTransform;
  end else
    Result:=EmptyRect;
end;

function TToolZoomLayer.Render(VirtualScreen: TBGRABitmap;
  VirtualScreenWidth, VirtualScreenHeight: integer;
  BitmapToVirtualScreen: TBitmapToVirtualScreenFunction): TRect;
begin
  with Manager.Image.LayerOffset[Manager.Image.CurrentLayerIndex] do
    Result:= NicePoint(VirtualScreen,BitmapToVirtualScreen(TransformCenter-PointF(X,Y)));
end;

function TToolZoomLayer.GetIsSelectingTool: boolean;
begin
  result := false;
end;

{ TToolMoveLayer }

function TToolMoveLayer.GetIsSelectingTool: boolean;
begin
  result := false;
end;

function TToolMoveLayer.DoToolDown(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF; rightBtn: boolean): TRect;
var idx: integer;
begin
  result := EmptyRect;
  if not handMoving then
  begin
    handMoving := true;
    handOrigin := pt;
    if not FStartLayerOffsetDefined then
    begin
      FStartLayerOffsetDefined := true;
      idx := Manager.Image.CurrentLayerIndex;
      NeedLayerBounds;
      FStartLayerOffset := Manager.Image.LayerOffset[idx];
      FStartLayerMatrix := Manager.Image.LayerOriginalMatrix[idx];
    end;
  end;
end;

function TToolMoveLayer.DoToolMove(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF): TRect;
var idx: integer;
  prev: TPoint;
begin
  if handMoving and ((handOrigin.X <> pt.X) or (handOrigin.Y <> pt.Y)) then
  begin
    idx := Manager.Image.CurrentLayerIndex;
    if UseOriginal then
    begin
      Manager.Image.LayerOriginalMatrix[idx] :=
          AffineMatrixTranslation(pt.X-HandOrigin.X,pt.Y-HandOrigin.Y)*Manager.Image.LayerOriginalMatrix[idx];
      result := OnlyRenderChange;
    end else
    begin
      prev := Manager.Image.LayerOffset[idx];
      Manager.Image.SetLayerOffset(idx, Point(prev.X+pt.X-HandOrigin.X,
                                         prev.Y+pt.Y-HandOrigin.Y), FLayerBounds);
      result := OnlyRenderChange;
    end;
  end else
    result := EmptyRect;
end;

procedure TToolMoveLayer.DoToolMoveAfter(pt: TPoint; ptF: TPointF);
begin
  if handMoving then handOrigin := pt;
end;

function TToolMoveLayer.UseOriginal: boolean;
begin
  with Manager.Image do
    result := LayerOriginalDefined[CurrentLayerIndex] and
              LayerOriginalKnown[CurrentLayerIndex];
end;

procedure TToolMoveLayer.NeedLayerBounds;
const
  VeryBigValue = maxLongInt div 2;
var
  idx: Integer;
begin
  GetAction;
  idx := Manager.Image.CurrentLayerIndex;
  if not FLayerBoundsDefined then
  begin
    if UseOriginal then
    begin
      FLayerBounds := Manager.Image.LayerOriginal[idx].GetRenderBounds(
                        Rect(-VeryBigValue,-VeryBigValue,VeryBigValue,VeryBigValue),
                        AffineMatrixIdentity);
      if FLayerBounds.Left = -VeryBigValue then FLayerBounds.Left := 0;
      if FLayerBounds.Top = -VeryBigValue then FLayerBounds.Top := 0;
      if FLayerBounds.Right = VeryBigValue then FLayerBounds.Right := Manager.Image.Width;
      if FLayerBounds.Bottom = VeryBigValue then FLayerBounds.Bottom := Manager.Image.Height;
    end
    else
      FLayerBounds := Manager.Image.LayerBitmap[idx].GetImageBounds;
    FLayerBoundsDefined := true;
  end;
end;

function TToolMoveLayer.GetAction: TLayerAction;
begin
  result := GetIdleAction;
end;

function TToolMoveLayer.DoGetToolDrawingLayer: TBGRABitmap;
begin
  Result:= Manager.Image.CurrentLayerReadOnly;   //do not modify layer data directly and ignore selection
end;

function TToolMoveLayer.ToolUp: TRect;
begin
  handMoving := false;
  result := EmptyRect;
end;

function TToolMoveLayer.ToolKeyDown(var key: Word): TRect;
var idx: integer;
begin
  if key = VK_RETURN then
  begin
    Manager.QueryExitTool;
    result := EmptyRect;
    Key := 0;
  end
  else if key = VK_ESCAPE then
  begin
    if FStartLayerOffsetDefined then
    begin
      idx := Manager.Image.CurrentLayerIndex;
      if UseOriginal then
        Manager.Image.LayerOriginalMatrix[idx] := FStartLayerMatrix
      else
        Manager.Image.SetLayerOffset(idx, FStartLayerOffset, FLayerBounds);
      result := OnlyRenderChange;
    end else
      result := EmptyRect;
    Manager.QueryExitTool;
    Key := 0;
  end else
    Result:=inherited ToolKeyDown(key);
end;

function TToolMoveLayer.Render(VirtualScreen: TBGRABitmap; VirtualScreenWidth,
  VirtualScreenHeight: integer;
  BitmapToVirtualScreen: TBitmapToVirtualScreenFunction): TRect;
var
  idx, i: integer;
  m: TAffineMatrix;
  ab: TAffineBox;
  ptsF: ArrayOfTPointF;
  pts: array of TPoint;
begin
  NeedLayerBounds;

  if UseOriginal then
  begin
    idx := Manager.Image.CurrentLayerIndex;
    m := Manager.Image.LayerOriginalMatrix[idx];
    with Manager.Image.LayerOffset[idx] do
      m := AffineMatrixTranslation(-x,-y)*m;
  end else m := AffineMatrixIdentity;

  ab := TAffineBox.AffineBox(BitmapToVirtualScreen(m*PointF(FLayerBounds.Left-0.499,FLayerBounds.Top-0.499)),
            BitmapToVirtualScreen(m*PointF(FLayerBounds.Right-0.501,FLayerBounds.Top-0.499)),
            BitmapToVirtualScreen(m*PointF(FLayerBounds.Left-0.499,FLayerBounds.Bottom-0.501)));
  ptsF := ab.AsPolygon;
  setlength(pts, length(ptsF));
  for i := 0 to high(pts) do
    pts[i] := ptsF[i].Round;

  result := TRect.Union(pts);
  result.Inflate(1,1);

  if Assigned(VirtualScreen) then
    virtualScreen.DrawpolygonAntialias(pts,BGRA(230,255,230,255),BGRA(0,0,0,255),FrameDashLength);
end;

initialization

  RegisterTool(ptMoveLayer,TToolMoveLayer);
  RegisterTool(ptRotateLayer,TToolRotateLayer);
  RegisterTool(ptZoomLayer,TToolZoomLayer);

end.

