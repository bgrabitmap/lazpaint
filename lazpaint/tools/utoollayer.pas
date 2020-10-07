// SPDX-License-Identifier: GPL-3.0-only
unit UToolLayer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UTool, BGRABitmap, BGRABitmapTypes,
  BGRATransform, BGRALayers, ULayerAction, UImageDiff,
  UImageType, UStateType;

type
  { TToolMoveLayer }

  TToolMoveLayer = class(TGenericTool)
  protected
    handMoving: boolean;
    handOriginF: TPointF;
    originalTransformBefore: TAffineMatrix;
    layerOffsetBefore: TPoint;
    FStartLayerOffset: TPoint;
    FStartLayerMatrix: TAffineMatrix;
    FStartLayerOffsetDefined: boolean;
    FLayerBounds: TRect;
    FLayerBoundsDefined: boolean;
    function GetIsSelectingTool: boolean; override;
    function DoToolDown({%H-}toolDest: TBGRABitmap; {%H-}pt: TPoint; ptF: TPointF;
      {%H-}rightBtn: boolean): TRect; override;
    function DoToolMove({%H-}toolDest: TBGRABitmap; {%H-}pt: TPoint; ptF: TPointF): TRect; override;
    function DoToolKeyDown(var key: Word): TRect; override;
    function UseOriginal: boolean;
    procedure NeedLayerBounds;
    function GetAction: TLayerAction; override;
    function DoGetToolDrawingLayer: TBGRABitmap; override;
    procedure OnTryStop({%H-}sender: TCustomLayerAction); override;
    function FixLayerOffset: boolean; override;
    function DoTranslate(dx,dy: single): TRect;
    procedure SaveOffsetBefore;
  public
    constructor Create(AManager: TToolManager); override;
    function ToolUp: TRect; override;
    function GetContextualToolbars: TContextualToolbars; override;
    function ToolCommand(ACommand: TToolCommand): boolean; override;
    function ToolProvideCommand(ACommand: TToolCommand): boolean; override;
    function Render(VirtualScreen: TBGRABitmap; {%H-}VirtualScreenWidth,
      {%H-}VirtualScreenHeight: integer;
      BitmapToVirtualScreen: TBitmapToVirtualScreenFunction): TRect; override;
  end;

  { TToolTransformLayer }

  TToolTransformLayer = class(TGenericTool)
  private
    function GetInitialLayerBounds: TRect;
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
    FPreviousFilter: TResampleFilter;
    FTransforming: boolean;
    FPreviousMousePos: TPointF;
    FSnapDown: boolean;
    FLastUpdateRect: TRect;
    FLastUpdateRectDefined: boolean;
    FOriginalBounds: TRect;
    FOriginalBoundsDefined: boolean;
    function GetIsSelectingTool: boolean; override;
    function DoToolDown({%H-}toolDest: TBGRABitmap; {%H-}pt: TPoint; ptF: TPointF;
      rightBtn: boolean): TRect; override;
    function DoToolMove({%H-}toolDest: TBGRABitmap; {%H-}pt: TPoint; ptF: TPointF): TRect; override;
    function DoToolKeyDown(var key: Word): TRect; override;
    function DoToolKeyUp(var key: Word): TRect; override;
    procedure CancelTransform;
    procedure ValidateTransform;
    function TransformOk: boolean; virtual; abstract;
    function UpdateTransform: TRect; virtual; abstract;
    procedure TransformCenterChanged; virtual; abstract;
    function MouseChangesTransform(APrevPos, ANewPos: TPointF): boolean; virtual; abstract;
    function CtrlChangesTransform: boolean; virtual; abstract;
    property TransformCenter: TPointF read GetTransformCenter write SetTransformCenter;
    function GetAction: TLayerAction; override;
    function DoGetToolDrawingLayer: TBGRABitmap; override;
    procedure OnTryStop({%H-}sender: TCustomLayerAction); override;
  public
    constructor Create(AManager: TToolManager); override;
    destructor Destroy; override;
    function GetContextualToolbars: TContextualToolbars; override;
    function ToolCommand(ACommand: TToolCommand): boolean; override;
    function ToolProvideCommand(ACommand: TToolCommand): boolean; override;
    function ToolUp: TRect; override;
    function Render(VirtualScreen: TBGRABitmap; {%H-}VirtualScreenWidth,
      {%H-}VirtualScreenHeight: integer;
      BitmapToVirtualScreen: TBitmapToVirtualScreenFunction): TRect; override;
  end;

  { TToolZoomLayer }

  TToolZoomLayer = class(TToolTransformLayer)
  private
    FZoom,FActualZoom,FPreviousActualZoom: single;
    function GetActualZoom: single;
  protected
    function TransformOk: boolean; override;
    function UpdateTransform: TRect; override;
    procedure TransformCenterChanged; override;
    function MouseChangesTransform(APrevPos, ANewPos: TPointF): boolean; override;
    function CtrlChangesTransform: boolean; override;
  public
    constructor Create(AManager: TToolManager); override;
  end;

  { TToolRotateLayer }

  TToolRotateLayer = class(TToolTransformLayer)
  private
    FAngle,FActualAngle,FPreviousActualAngle: single;
    function GetActualAngle: single;
  protected
    function TransformOk: boolean; override;
    function UpdateTransform: TRect; override;
    procedure TransformCenterChanged; override;
    function MouseChangesTransform(APrevPos, ANewPos: TPointF): boolean; override;
    function CtrlChangesTransform: boolean; override;
  public
    constructor Create(AManager: TToolManager); override;
  end;

implementation

uses LazPaintType, ugraph, LCLType, Types, BGRALayerOriginal, math, LCVectorOriginal;

const
  VeryBigValue = maxLongInt div 2;

{ TToolMoveLayer }

function TToolMoveLayer.GetIsSelectingTool: boolean;
begin
  result := false;
end;

function TToolMoveLayer.DoToolDown(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF; rightBtn: boolean): TRect;
begin
  result := EmptyRect;
  if not handMoving then
  begin
    GetAction;
    handMoving := true;
    handOriginF := ptF;
    if UseOriginal then Manager.Image.DraftOriginal := true;
    SaveOffsetBefore;
  end;
end;

function TToolMoveLayer.DoToolMove(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF): TRect;
var
  dx, dy: Single;
begin
  result := EmptyRect;
  if handMoving then
  begin
    dx := ptF.X-HandOriginF.X;
    dy := ptF.Y-HandOriginF.Y;
    if ssSnap in ShiftState then
    begin
      dx := round(dx);
      dy := round(dy);
    end;
    result := DoTranslate(dx,dy);
  end;
end;

function TToolMoveLayer.UseOriginal: boolean;
begin
  with Manager.Image do
    result := LayerOriginalDefined[CurrentLayerIndex] and
              LayerOriginalKnown[CurrentLayerIndex];
end;

procedure TToolMoveLayer.NeedLayerBounds;
var
  idx: Integer;
begin
  idx := Manager.Image.CurrentLayerIndex;
  if not FLayerBoundsDefined then
  begin
    if UseOriginal then
    begin
      if Manager.Image.LayerOriginal[idx] is TVectorOriginal then
        FLayerBounds := TVectorOriginal(Manager.Image.LayerOriginal[idx]).GetAlignBounds(
                          Rect(-VeryBigValue,-VeryBigValue,VeryBigValue,VeryBigValue),
                          AffineMatrixIdentity)
      else
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

procedure TToolMoveLayer.OnTryStop(sender: TCustomLayerAction);
begin
  //nothing
end;

function TToolMoveLayer.FixLayerOffset: boolean;
begin
  Result:= false;
end;

function TToolMoveLayer.DoTranslate(dx, dy: single): TRect;
var
  idx: integer;
  newTransform: TAffineMatrix;
  newOfs: TPoint;
begin
  idx := Manager.Image.CurrentLayerIndex;
  if not FStartLayerOffsetDefined then
  begin
    FStartLayerOffsetDefined := true;
    NeedLayerBounds;
    FStartLayerOffset := Manager.Image.LayerOffset[idx];
    FStartLayerMatrix := Manager.Image.LayerOriginalMatrix[idx];
  end;

  if UseOriginal then
  begin
    newTransform := AffineMatrixTranslation(dx,dy)*originalTransformBefore;
    if Manager.Image.LayerOriginalMatrix[idx] <> newTransform then
    begin
      Manager.Image.LayerOriginalMatrix[idx] := newTransform;
      result := OnlyRenderChange;
    end;
  end else
  begin
    newOfs := Point(layerOffsetBefore.X+round(dx),
                    layerOffsetBefore.Y+round(dy));
    if Manager.Image.LayerOffset[idx]<>newOfs then
    begin
      Manager.Image.SetLayerOffset(idx, newOfs, FLayerBounds);
      result := OnlyRenderChange;
    end;
  end;
end;

procedure TToolMoveLayer.SaveOffsetBefore;
var
  idx: Integer;
begin
  idx := Manager.Image.CurrentLayerIndex;
  if UseOriginal then
    originalTransformBefore := Manager.Image.LayerOriginalMatrix[idx]
  else
    originalTransformBefore := AffineMatrixIdentity;
  layerOffsetBefore := Manager.Image.LayerOffset[idx];
end;

constructor TToolMoveLayer.Create(AManager: TToolManager);
begin
  inherited Create(AManager);
  handMoving := false;
  FStartLayerOffsetDefined:= false;
end;

function TToolMoveLayer.ToolUp: TRect;
begin
  handMoving := false;
  result := EmptyRect;
  if UseOriginal then Manager.Image.DraftOriginal := false;
end;

function TToolMoveLayer.DoToolKeyDown(var key: Word): TRect;
  function Translate(dx,dy: integer): TRect;
  begin
    if handMoving or (ssAlt in ShiftState) then exit(EmptyRect);
    key := 0;
    GetAction;
    SaveOffsetBefore;
    if ssSnap in ShiftState then
    begin
      dx := dx*max(Manager.Image.Width div 20, 2);
      dy := dy*max(Manager.Image.Height div 20, 2);
    end;
    result := DoTranslate(dx,dy);
  end;

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
  end
  else if key = VK_LEFT then result := Translate(-1, 0)
  else if key = VK_RIGHT then result := Translate(1, 0)
  else if key = VK_UP then result := Translate(0,-1)
  else if key = VK_DOWN then result := Translate(0,1)
  else
    Result:=inherited DoToolKeyDown(key);
end;

function TToolMoveLayer.GetContextualToolbars: TContextualToolbars;
begin
  Result:= [];
end;

function TToolMoveLayer.ToolCommand(ACommand: TToolCommand): boolean;
var
  actualBounds: TRect;
  idx: Integer;
  orig: TBGRALayerCustomOriginal;
begin
  if not ToolProvideCommand(ACommand) then exit(false);
  idx := Manager.Image.CurrentLayerIndex;
  case ACommand of
  tcAlignLeft,tcAlignRight,tcAlignTop,tcAlignBottom,tcCenterHorizontally,tcCenterVertically:
      if handMoving then exit(false) else
      begin
        NeedLayerBounds;
        if UseOriginal then
        begin
          orig := Manager.Image.LayerOriginal[idx];
          if orig is TVectorOriginal then
            actualBounds := TVectorOriginal(orig).GetAlignBounds(
                          Rect(-VeryBigValue,-VeryBigValue,VeryBigValue,VeryBigValue),
                          Manager.Image.LayerOriginalMatrix[idx])
          else
            actualBounds := orig.GetRenderBounds(
                          Rect(-VeryBigValue,-VeryBigValue,VeryBigValue,VeryBigValue),
                          Manager.Image.LayerOriginalMatrix[idx]);
        end
        else
        begin
          actualBounds := FLayerBounds;
          actualBounds.Offset(Manager.Image.LayerOffset[idx]);
        end;
        GetAction;
        SaveOffsetBefore;
        case ACommand of
          tcAlignLeft: DoTranslate(-actualBounds.Left, 0);
          tcAlignRight: DoTranslate(Manager.Image.Width-actualBounds.Right, 0);
          tcAlignTop: DoTranslate(0, -actualBounds.Top);
          tcAlignBottom: DoTranslate(0, Manager.Image.Height-actualBounds.Bottom);
          tcCenterHorizontally: DoTranslate((Manager.Image.Width-(actualBounds.Left+actualBounds.Right)) div 2, 0);
          tcCenterVertically: DoTranslate(0, (Manager.Image.Height-(actualBounds.Top+actualBounds.Bottom)) div 2);
        end;
      end;
  tcMoveDown: Manager.Image.MoveLayer(idx, idx-1);
  tcMoveToBack: Manager.Image.MoveLayer(idx, 0);
  tcMoveUp: Manager.Image.MoveLayer(idx, idx+1);
  tcMoveToFront: Manager.Image.MoveLayer(idx, Manager.Image.NbLayers-1);
  end;
  result := true;
end;

function TToolMoveLayer.ToolProvideCommand(ACommand: TToolCommand): boolean;
begin
  case ACommand of
  tcAlignLeft,tcAlignRight,tcAlignTop,tcAlignBottom,tcCenterHorizontally,tcCenterVertically:
    result := not handMoving;
  tcMoveDown,tcMoveToBack: result := Manager.Image.CurrentLayerIndex > 0;
  tcMoveUp,tcMoveToFront: result := Manager.Image.CurrentLayerIndex < Manager.Image.NbLayers-1;
  else result := false;
  end;
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

  m := AffineMatrixTranslation(-0.5,-0.5)*m;
  ab := TAffineBox.AffineBox(BitmapToVirtualScreen(m*PointF(FLayerBounds.Left+0.001,FLayerBounds.Top+0.001)),
            BitmapToVirtualScreen(m*PointF(FLayerBounds.Right-0.001,FLayerBounds.Top+0.001)),
            BitmapToVirtualScreen(m*PointF(FLayerBounds.Left+0.001,FLayerBounds.Bottom-0.001)));
  ptsF := ab.AsPolygon;
  setlength(pts, length(ptsF));
  for i := 0 to high(pts) do
    pts[i] := ptsF[i].Round;

  result := TRect.Union(pts);
  result.Inflate(1,1);

  if Assigned(VirtualScreen) then
    virtualScreen.DrawpolygonAntialias(pts,BGRA(230,255,230,255),BGRA(0,0,0,255),
      FrameDashLength*Manager.CanvasScale);
end;

{ TToolTransformLayer }

function TToolTransformLayer.GetInitialLayerBounds: TRect;
begin
  if not FInitialLayerBoundsDefined then
  begin
    FInitialLayerBounds := GetToolDrawingLayer.GetImageBounds;
    with Manager.Image.LayerOffset[Manager.Image.CurrentLayerIndex] do
      FInitialLayerBounds.Offset(X,Y);
    FInitialLayerBoundsDefined := true;
  end;
  result := FInitialLayerBounds;
end;

function TToolTransformLayer.GetTransformCenter: TPointF;
var bounds: TRect;
begin
  if not FTransformCenterDefined then
  begin
    bounds := GetInitialLayerBounds;
    if IsRectEmpty(bounds) then
      FTransformCenter := PointF(Manager.Image.Width/2 - 0.5,Manager.Image.Height/2 - 0.5)
    else
    begin
      with bounds do
        FTransformCenter := PointF((Left+Right)/2 - 0.5, (Top+Bottom)/2 - 0.5);
    end;
    FTransformCenterDefined := true;
  end;
  result := FTransformCenter;
end;

procedure TToolTransformLayer.SetTransformCenter(AValue: TPointF);
begin
  FTransformCenter := AValue;
end;

procedure TToolTransformLayer.NeedOriginal;
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
    FBackupLayer:= TReplaceLayerByImageOriginalDifference.Create(Manager.Image.CurrentState, layerIdx, true);
  end;
  FInitialOriginalMatrix := layered.LayerOriginalMatrix[layerIdx];
  FOriginalInit := true;
end;

function TToolTransformLayer.DoToolDown(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF; rightBtn: boolean): TRect;
begin
  with Manager.Image.LayerOffset[Manager.Image.CurrentLayerIndex] do
    ptF += PointF(X,Y);

  if not FTransforming and not rightBtn then
  begin
    FTransforming := true;
    FPreviousMousePos := ptF;
    if FSnapDown then
    begin
      result := UpdateTransform;
      if IsRectEmpty(result) then result := OnlyRenderChange;
    end else result := EmptyRect;
    Manager.Image.DraftOriginal := true;
  end else
  if rightBtn then
  begin
    if FSnapDown then
    begin
      if Manager.Image.ZoomFactor > 4 then
      begin
        ptF.X := round(ptF.X*2)/2;
        ptF.Y := round(ptF.Y*2)/2;
      end else
      begin
        ptF.X := round(ptF.X);
        ptF.Y := round(ptF.Y);
      end;
    end;
    FTransformCenter := ptF;
    TransformCenterChanged;
    result := UpdateTransform;
    if IsRectEmpty(result) then result := OnlyRenderChange;
  end else
    result := EmptyRect;
end;

function TToolTransformLayer.DoToolMove(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF): TRect;
begin
  with Manager.Image.LayerOffset[Manager.Image.CurrentLayerIndex] do
    ptF += PointF(X,Y);
  if FTransforming then
  begin
    If MouseChangesTransform(FPreviousMousePos, ptF) then
    begin
      result := UpdateTransform;
      if result.IsEmpty then result := OnlyRenderChange;
    end
    else result := EmptyRect;
    FPreviousMousePos := ptF;
  end else
    result := EmptyRect;
end;

procedure TToolTransformLayer.CancelTransform;
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

procedure TToolTransformLayer.ValidateTransform;
var
  transform: TAffineMatrix;
  layerIdx: Integer;
  invTransformDiff: TCustomImageDifference;
  r: TRect;
begin
  if FOriginalInit then
  begin
    if Assigned(FBackupLayer) then
    begin
      layerIdx := Manager.Image.CurrentLayerIndex;
      transform := Manager.Image.LayerOriginalMatrix[layerIdx];
      invTransformDiff := Manager.Image.CurrentState.ComputeLayerMatrixDifference(layerIdx,
                          transform, FInitialOriginalMatrix);
      FBackupLayer.nextMatrix := transform;
      Manager.Image.AddUndo(invTransformDiff);
      Manager.Image.AddUndo(FBackupLayer);
      r := EmptyRect;
      Manager.Image.CurrentState.LayeredBitmap.RenderLayerFromOriginalIfNecessary(layerIdx, false, r);
      FBackupLayer := nil;
    end;
    FOriginalInit := false;
  end;
  Manager.QueryExitTool;
end;

function TToolTransformLayer.GetAction: TLayerAction;
begin
  result := GetIdleAction;
end;

function TToolTransformLayer.DoGetToolDrawingLayer: TBGRABitmap;
begin
  Result:= Manager.Image.CurrentLayerReadOnly   //do not modify layer data directly and ignore selection
end;

procedure TToolTransformLayer.OnTryStop(sender: TCustomLayerAction);
begin
  //nothing
end;

constructor TToolTransformLayer.Create(AManager: TToolManager);
begin
  inherited Create(AManager);
  FSnapDown:= false;
  FTransformCenterDefined := false;
  FLastUpdateRectDefined:= false;
end;

destructor TToolTransformLayer.Destroy;
begin
  if TransformOk then ValidateTransform
  else CancelTransform;
  inherited Destroy;
end;

function TToolTransformLayer.GetContextualToolbars: TContextualToolbars;
begin
  Result:= [];
end;

function TToolTransformLayer.ToolCommand(ACommand: TToolCommand): boolean;
begin
  if not ToolProvideCommand(ACommand) then exit(false);
  case ACommand of
  tcMoveDown: Manager.Image.MoveLayer(Manager.Image.CurrentLayerIndex, Manager.Image.CurrentLayerIndex-1);
  tcMoveToBack: Manager.Image.MoveLayer(Manager.Image.CurrentLayerIndex, 0);
  tcMoveUp: Manager.Image.MoveLayer(Manager.Image.CurrentLayerIndex, Manager.Image.CurrentLayerIndex+1);
  tcMoveToFront: Manager.Image.MoveLayer(Manager.Image.CurrentLayerIndex, Manager.Image.NbLayers-1);
  end;
  result := true;
end;

function TToolTransformLayer.ToolProvideCommand(ACommand: TToolCommand): boolean;
begin
  case ACommand of
  tcMoveDown,tcMoveToBack: result := Manager.Image.CurrentLayerIndex > 0;
  tcMoveUp,tcMoveToFront: result := Manager.Image.CurrentLayerIndex < Manager.Image.NbLayers-1;
  else result := false;
  end;
end;

function TToolTransformLayer.DoToolKeyDown(var key: Word): TRect;
begin
  if key = VK_CONTROL then
  begin
    FSnapDown:= true;
    if FTransforming and CtrlChangesTransform then
    begin
      result := UpdateTransform;
      if result.IsEmpty then result := OnlyRenderChange;
    end
      else result := EmptyRect;
    Key := 0;
  end else
  if Key = VK_RETURN then
  begin
    if TransformOk then ValidateTransform
    else CancelTransform;
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

function TToolTransformLayer.DoToolKeyUp(var key: Word): TRect;
begin
  if key = VK_CONTROL then
  begin
    FSnapDown := false;
    if FTransforming and CtrlChangesTransform then
    begin
      result := UpdateTransform;
      if result.IsEmpty then result := OnlyRenderChange;
    end
      else result := EmptyRect;
    Key := 0;
  end else
    result := EmptyRect;
end;

function TToolTransformLayer.ToolUp: TRect;
begin
  if FTransforming then
  begin
    FTransforming := false;
    result := UpdateTransform;
    if result.IsEmpty then result := OnlyRenderChange;
    Manager.Image.DraftOriginal := false;
  end else
    Result:=EmptyRect;
end;

function TToolTransformLayer.Render(VirtualScreen: TBGRABitmap;
  VirtualScreenWidth, VirtualScreenHeight: integer;
  BitmapToVirtualScreen: TBitmapToVirtualScreenFunction): TRect;
var
  idx, i: integer;
  m: TAffineMatrix;
  ab: TAffineBox;
  ptsF: ArrayOfTPointF;
  pts: array of TPoint;
  ptsRect: TRect;
begin
  idx := Manager.Image.CurrentLayerIndex;
  if not FOriginalBoundsDefined then
  begin
    if Manager.Image.LayerOriginalDefined[idx] then
    begin
      if Manager.Image.LayerOriginal[idx] is TVectorOriginal then
        FOriginalBounds := TVectorOriginal(Manager.Image.LayerOriginal[idx]).GetAlignBounds(
                          Rect(-VeryBigValue,-VeryBigValue,VeryBigValue,VeryBigValue),
                          AffineMatrixIdentity)
      else
        FOriginalBounds := Manager.Image.LayerOriginal[idx].GetRenderBounds(
                          Rect(-VeryBigValue,-VeryBigValue,VeryBigValue,VeryBigValue),
                          AffineMatrixIdentity);
      if FOriginalBounds.Left = -VeryBigValue then FOriginalBounds.Left := 0;
      if FOriginalBounds.Top = -VeryBigValue then FOriginalBounds.Top := 0;
      if FOriginalBounds.Right = VeryBigValue then FOriginalBounds.Right := Manager.Image.Width;
      if FOriginalBounds.Bottom = VeryBigValue then FOriginalBounds.Bottom := Manager.Image.Height;
    end
    else
      FOriginalBounds := GetInitialLayerBounds;
  end;
  m := Manager.Image.LayerOriginalMatrix[idx];
  with Manager.Image.LayerOffset[idx] do
    m := AffineMatrixTranslation(-x,-y)*m;
  m := AffineMatrixTranslation(-0.5,-0.5)*m;

  with Manager.Image.LayerOffset[idx] do
    Result:= NicePoint(VirtualScreen,BitmapToVirtualScreen(TransformCenter-PointF(X,Y)));

  ab := TAffineBox.AffineBox(BitmapToVirtualScreen(m*PointF(FOriginalBounds.Left+0.001,FOriginalBounds.Top+0.001)),
            BitmapToVirtualScreen(m*PointF(FOriginalBounds.Right-0.001,FOriginalBounds.Top+0.001)),
            BitmapToVirtualScreen(m*PointF(FOriginalBounds.Left+0.001,FOriginalBounds.Bottom-0.001)));
  ptsF := ab.AsPolygon;
  setlength(pts, length(ptsF));
  for i := 0 to high(pts) do
    pts[i] := ptsF[i].Round;

  ptsRect := TRect.Union(pts);
  ptsRect.Inflate(1,1);
  Result.Union(ptsRect);

  if Assigned(VirtualScreen) then
    virtualScreen.DrawpolygonAntialias(pts,BGRA(230,255,230,255),BGRA(0,0,0,255),
      FrameDashLength*Manager.CanvasScale);
end;

function TToolTransformLayer.GetIsSelectingTool: boolean;
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
  if FSnapDown then
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

function TToolZoomLayer.TransformOk: boolean;
begin
  result := FActualZoom <> 0;
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
    AffineMatrixTranslation(TransformCenter.X+0.5,TransformCenter.Y+0.5)*
    AffineMatrixScale(FActualZoom,FActualZoom)*
    AffineMatrixTranslation(-TransformCenter.X-0.5,-TransformCenter.Y-0.5)*
    FInitialOriginalMatrix;
end;

procedure TToolZoomLayer.TransformCenterChanged;
begin
  FZoom := 1;
  FActualZoom:= GetActualZoom;
end;

function TToolZoomLayer.MouseChangesTransform(APrevPos, ANewPos: TPointF): boolean;
var
  dist, prevDist: Single;
begin
  dist := VectLen(ANewPos-TransformCenter);
  prevDist := VectLen(APrevPos-TransformCenter);
  if (prevDist <> 0) and (dist <> 0) then
  begin
    FZoom *= dist/prevDist;
    FActualZoom:= GetActualZoom;
    result := true;
  end
  else result := false;
end;

function TToolZoomLayer.CtrlChangesTransform: boolean;
var
  newActualZoom: Single;
begin
  newActualZoom := GetActualZoom;
  if FActualZoom<>newActualZoom then
  begin
    FActualZoom := newActualZoom;
    result := true;
  end else
    result := false;
end;

constructor TToolZoomLayer.Create(AManager: TToolManager);
begin
  inherited Create(AManager);
  FZoom:= 1;
  FPreviousActualZoom := 1;
end;

{ TToolRotateLayer }

function TToolRotateLayer.GetActualAngle: single;
begin
  if FSnapDown then
    result := round(FAngle/15)*15
  else
    result := FAngle;
end;

function TToolRotateLayer.TransformOk: boolean;
begin
  result := true;
end;

procedure TToolRotateLayer.TransformCenterChanged;
begin
  FAngle := 0;
  FActualAngle:= GetActualAngle;
end;

function TToolRotateLayer.MouseChangesTransform(APrevPos, ANewPos: TPointF): boolean;
var
  angleDiff, newActualAngle: Single;
begin
  angleDiff := ComputeAngle(ANewPos.X-TransformCenter.X,ANewPos.Y-TransformCenter.Y)-
             ComputeAngle(APrevPos.X-TransformCenter.X,APrevPos.Y-TransformCenter.Y);
  FAngle += angleDiff;
  newActualAngle := GetActualAngle;
  if newActualAngle <> FActualAngle then
  begin
    FActualAngle:= newActualAngle;
    result := true;
  end
  else result := false;
end;

function TToolRotateLayer.CtrlChangesTransform: boolean;
var
  newActualAngle: Single;
begin
  newActualAngle := GetActualAngle;
  if newActualAngle<>FActualAngle then
  begin
    FActualAngle := newActualAngle;
    result := true;
  end else
    result := false;
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
    AffineMatrixTranslation(TransformCenter.X+0.5,TransformCenter.Y+0.5)*
    AffineMatrixRotationDeg(FActualAngle)*
    AffineMatrixTranslation(-TransformCenter.X-0.5,-TransformCenter.Y-0.5)*
    FInitialOriginalMatrix;
end;

constructor TToolRotateLayer.Create(AManager: TToolManager);
begin
  inherited Create(AManager);
  FAngle:= 0;
  FPreviousActualAngle := 0;
end;

initialization

  RegisterTool(ptMoveLayer,TToolMoveLayer);
  RegisterTool(ptRotateLayer,TToolRotateLayer);
  RegisterTool(ptZoomLayer,TToolZoomLayer);

end.

