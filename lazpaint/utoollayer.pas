unit UToolLayer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UTool, BGRABitmap, BGRABitmapTypes, UImageType;

type
  { TToolMoveLayer }

  TToolMoveLayer = class(TGenericTool)
  protected
    handMoving: boolean;
    handOrigin: TPoint;
    FOriginalLayerOffset: TPoint;
    FOriginalLayerOffsetDefined: boolean;
    FLayerBounds: TRect;
    FLayerBoundsDefined: boolean;
    function GetIsSelectingTool: boolean; override;
    function DoToolDown({%H-}toolDest: TBGRABitmap; pt: TPoint; {%H-}ptF: TPointF;
      {%H-}rightBtn: boolean): TRect; override;
    function DoToolMove({%H-}toolDest: TBGRABitmap; pt: TPoint; {%H-}ptF: TPointF): TRect; override;
    procedure DoToolMoveAfter(pt: TPoint; {%H-}ptF: TPointF); override;
    procedure OnTryStop({%H-}sender: TCustomLayerAction); override;
    procedure ApplyMoveLayer;
  public
    destructor Destroy; override;
    function GetToolDrawingLayer: TBGRABitmap; override;
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
    function GetRotationCenter: TPointF;
    procedure SetRotationCenter(AValue: TPointF);
  protected
    FOriginalLayerBounds: TRect;
    FOriginalLayerBoundsDefined: boolean;
    FRotationCenter: TPointF;
    FRotationCenterDefined: boolean;
    FFilter: TResampleFilter;
    FPreviousRotationCenter: TPointF;
    FAngle,FActualAngle,FPreviousActualAngle: single;
    FPreviousFilter: TResampleFilter;
    FRotating: boolean;
    FPreviousMousePos: TPointF;
    FCtrlDown: boolean;
    FLastUpdateRect: TRect;
    FLastUpdateRectDefined: boolean;
    function GetIsSelectingTool: boolean; override;
    function DoToolDown({%H-}toolDest: TBGRABitmap; {%H-}pt: TPoint; ptF: TPointF;
      rightBtn: boolean): TRect; override;
    function DoToolMove({%H-}toolDest: TBGRABitmap; {%H-}pt: TPoint; ptF: TPointF): TRect;
      override;
    function UpdateRotation: TRect;
    property RotationCenter: TPointF read GetRotationCenter write SetRotationCenter;
    property OriginalLayerBounds: TRect read GetOriginalLayerBounds;
    procedure OnTryStop({%H-}sender: TCustomLayerAction); override;
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

uses LazPaintType, ugraph, LCLType, Types;

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
  if not FOriginalLayerBoundsDefined then
  begin
    FOriginalLayerBounds := GetToolDrawingLayer.GetImageBounds;
    FOriginalLayerBoundsDefined := true;
  end;
  result := FOriginalLayerBounds;
end;

function TToolRotateLayer.GetRotationCenter: TPointF;
begin
  if not FRotationCenterDefined then
  begin
    if IsRectEmpty(OriginalLayerBounds) then
      FRotationCenter := PointF(Manager.Image.Width/2 - 0.5,Manager.Image.Height/2 - 0.5)
    else
    with OriginalLayerBounds do
      FRotationCenter := PointF((Left+Right)/2 - 0.5, (Top+Bottom)/2 - 0.5);
    FRotationCenterDefined := true;
  end;
  result := FRotationCenter;
end;

procedure TToolRotateLayer.SetRotationCenter(AValue: TPointF);
begin
  FRotationCenter := AValue;
end;

function TToolRotateLayer.DoToolDown(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF; rightBtn: boolean): TRect;
begin
  if not FRotating and not rightBtn then
  begin
    FRotating := true;
    FPreviousMousePos := ptF;
    if toolDest.NbPixels > 65536 then
      FFilter := rfBox
    else
      FFilter:= rfCosine;
    if FCtrlDown then result := UpdateRotation
     else result := EmptyRect;
  end else
  if rightBtn then
  begin
    FRotationCenter := ptF;
    FAngle := 0;
    FActualAngle:= GetActualAngle;
    result := UpdateRotation;
    if IsRectEmpty(result) then result := OnlyRenderChange;
  end else
    result := EmptyRect;
end;

function TToolRotateLayer.DoToolMove(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF): TRect;
var angleDiff: single;
begin
  if FRotating then
  begin
    angleDiff := ComputeAngle(ptF.X-RotationCenter.X,ptF.Y-RotationCenter.Y)-
               ComputeAngle(FPreviousMousePos.X-RotationCenter.X,FPreviousMousePos.Y-RotationCenter.Y);
    FAngle += angleDiff;
    FActualAngle:= GetActualAngle;
    result := UpdateRotation;
    FPreviousMousePos := ptF;
  end;
end;

function TToolRotateLayer.UpdateRotation: TRect;
var origin,haxis,vaxis: TPointF;
begin
  if (FActualAngle = FPreviousActualAngle) and ((FActualAngle = 0) or (RotationCenter = FPreviousRotationCenter)) and
    (FPreviousFilter = FFilter) then
  begin
    result := EmptyRect;
    exit;
  end;
  FPreviousActualAngle := FActualAngle;
  FPreviousRotationCenter := RotationCenter;
  FPreviousFilter := FFilter;
  result := EmptyRect;
  if not FLastUpdateRectDefined then
  begin
    GetToolDrawingLayer.FillTransparent;
    result := rect(0,0,GetToolDrawingLayer.Width,GetToolDrawingLayer.Height);
  end else
  if not IsRectEmpty(FLastUpdateRect) then
  begin
    GetToolDrawingLayer.FillRect(FLastUpdateRect,BGRAPixelTransparent,dmSet);
    result := FLastUpdateRect;
  end;
  FLastUpdateRect := GetToolDrawingLayer.GetImageAngleBounds(0,0,Action.BackupDrawingLayer,FActualAngle,RotationCenter.X,RotationCenter.Y,True);
  FLastUpdateRectDefined:= true;
  GetToolDrawingLayer.ComputeImageAngleAxes(0,0,Action.BackupDrawingLayer.Width,Action.BackupDrawingLayer.Height,FActualAngle,RotationCenter.X,RotationCenter.Y,True,
  origin,haxis,vaxis);
  GetToolDrawingLayer.PutImageAffine(origin,haxis,vaxis,Action.BackupDrawingLayer,FLastUpdateRect,FFilter,dmSet,255);
  result := RectUnion(result,FLastUpdateRect);
end;

procedure TToolRotateLayer.OnTryStop(sender: TCustomLayerAction);
begin
  //nothing
end;

constructor TToolRotateLayer.Create(AManager: TToolManager);
begin
  inherited Create(AManager);
  FAngle:= 0;
  FPreviousActualAngle := 0;
  FCtrlDown:= false;
  FOriginalLayerBoundsDefined:= false;
  FRotationCenterDefined := false;
  FLastUpdateRectDefined:= false;
  FFilter := rfCosine;
  FPreviousFilter := FFilter;
end;

destructor TToolRotateLayer.Destroy;
begin
  ValidateActionPartially;
  inherited Destroy;
end;

function TToolRotateLayer.ToolKeyDown(var key: Word): TRect;
begin
  if key = VK_CONTROL then
  begin
    FCtrlDown:= true;
    if FRotating then
    begin
      FActualAngle := GetActualAngle;
      result := UpdateRotation;
    end
     else result := EmptyRect;
    Key := 0;
  end else
  if Key = VK_RETURN then
  begin
    if FActualAngle = 0 then CancelActionPartially
     else ValidateActionPartially;
    result := OnlyRenderChange;
    manager.QueryExitTool;
    key := 0;
  end else
  if Key = VK_ESCAPE then
  begin
    CancelActionPartially;
    result := OnlyRenderChange;
    manager.QueryExitTool;
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
  if FRotating then
  begin
    FRotating := false;
    FFilter := rfCosine;
    result := UpdateRotation;
  end else
    Result:=EmptyRect;
end;

function TToolRotateLayer.Render(VirtualScreen: TBGRABitmap;
  VirtualScreenWidth, VirtualScreenHeight: integer;
  BitmapToVirtualScreen: TBitmapToVirtualScreenFunction): TRect;
begin
  Result:= NicePoint(VirtualScreen,BitmapToVirtualScreen(RotationCenter));
end;

function TToolRotateLayer.GetIsSelectingTool: boolean;
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
    if not FOriginalLayerOffsetDefined then
    begin
      FOriginalLayerOffsetDefined := true;
      idx := Manager.Image.currentImageLayerIndex;
      if not FLayerBoundsDefined then
      begin
        FLayerBounds := Manager.Image.LayerBitmap[idx].GetImageBounds;
        FLayerBoundsDefined := true;
      end;
      FOriginalLayerOffset := Manager.Image.LayerOffset[idx];
      GetAction;
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
    idx := Manager.Image.currentImageLayerIndex;
    prev := Manager.Image.LayerOffset[idx];
    Manager.Image.SetLayerOffset(idx, Point(prev.X+pt.X-HandOrigin.X,
                                       prev.Y+pt.Y-HandOrigin.Y), FLayerBounds);
    result := OnlyRenderChange;
  end else
    result := EmptyRect;
end;

procedure TToolMoveLayer.DoToolMoveAfter(pt: TPoint; ptF: TPointF);
begin
  if handMoving then handOrigin := pt;
end;

procedure TToolMoveLayer.OnTryStop(sender: TCustomLayerAction);
begin
  //nothing
end;

procedure TToolMoveLayer.ApplyMoveLayer;
var finalOffset: TPoint;
  idx: integer;
begin
  if FOriginalLayerOffsetDefined then
  begin
    idx := Manager.Image.currentImageLayerIndex;
    finalOffset := Manager.Image.LayerOffset[idx];
    Manager.Image.SetLayerOffset(idx, FOriginalLayerOffset, FLayerBounds);
    Manager.Image.ApplyLayerOffset(finalOffset.x,finalOffset.y);
    FOriginalLayerOffsetDefined := false;
  end;
end;

destructor TToolMoveLayer.Destroy;
begin
  ApplyMoveLayer;
  inherited Destroy;
end;

function TToolMoveLayer.GetToolDrawingLayer: TBGRABitmap;
begin
  Result:= Manager.Image.SelectedImageLayerReadOnly;   //do not create a selection layer
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
    if FOriginalLayerOffsetDefined then
    begin
      idx := Manager.Image.currentImageLayerIndex;
      Manager.Image.LayerOffset[idx] := FOriginalLayerOffset;
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
var pt1,pt2:TPoint;
  ptF1,ptF2: TPointF;
  penWidth,i,idx: integer;
begin
  idx := Manager.Image.currentImageLayerIndex;
  if not FLayerBoundsDefined then
  begin
    Action;
    FLayerBounds := Manager.Image.LayerBitmap[idx].GetImageBounds;
    FLayerBoundsDefined := true;
  end;

  ptF1 := BitmapToVirtualScreen(PointF(FLayerBounds.Left-0.5,FLayerBounds.Top-0.5));
  ptF2 := BitmapToVirtualScreen(PointF(FLayerBounds.Right-0.5,FLayerBounds.Bottom-0.5));
  pt1 := point(round(ptF1.x),round(ptF1.y));
  pt2 := point(round(ptF2.X)-1,round(ptF2.Y)-1);

  if Manager.Image.ZoomFactor > 3 then penWidth := 2 else penWidth := 1;
  result := rect(pt1.x-(penWidth-1),pt1.y-(penWidth-1),pt2.x+1+(penWidth-1),pt2.y+1+(penWidth-1));

  if Assigned(VirtualScreen) then
  begin
    for i := 0 to penWidth-1 do
      virtualScreen.DrawpolylineAntialias([point(pt1.x-(penWidth-1)+i,pt1.y-(penWidth-1)+i),
               point(pt2.x+i,pt1.y-(penWidth-1)+i),point(pt2.x+i,pt2.y+i),
               point(pt1.x-(penWidth-1)+i,pt2.y+i),point(pt1.x-(penWidth-1)+i,pt1.y-(penWidth-1)+i)],BGRA(230,255,230,212),BGRA(0,0,0,192),FrameDashLength,False);
  end;
end;

initialization

  RegisterTool(ptMoveLayer,TToolMoveLayer);
  RegisterTool(ptRotateLayer,TToolRotateLayer);

end.

