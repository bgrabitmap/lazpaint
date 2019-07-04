unit UToolSelect;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, utool, utoolpolygon, utoolbasic, BGRABitmapTypes, BGRABitmap,
  ULayerAction, LCVectorOriginal;

type
  { TVectorialSelectTool }

  TVectorialSelectTool = class(TVectorialTool)
  protected
    function GetIsSelectingTool: boolean; override;
    procedure AssignShapeStyle; override;
    function RoundCoordinate(ptF: TPointF): TPointF; override;
    function UpdateShape(toolDest: TBGRABitmap): TRect; override;
    procedure QuickDefineEnd; override;
    function BigImage: boolean;
  end;

  { TToolSelectRect }

  TToolSelectRect = class(TVectorialSelectTool)
  protected
    function CreateShape: TVectorShape; override;
  public
    function Render(VirtualScreen: TBGRABitmap; {%H-}VirtualScreenWidth, {%H-}VirtualScreenHeight: integer; BitmapToVirtualScreen: TBitmapToVirtualScreenFunction):TRect; override;
  end;

  { TToolSelectEllipse }

  TToolSelectEllipse = class(TVectorialSelectTool)
  protected
    function CreateShape: TVectorShape; override;
  public
    function Render(VirtualScreen: TBGRABitmap; {%H-}VirtualScreenWidth, {%H-}VirtualScreenHeight: integer; BitmapToVirtualScreen: TBitmapToVirtualScreenFunction):TRect; override;
  end;

  { TToolSelectPoly }

  TToolSelectPoly = class(TToolGenericPolygon)
  protected
    function HandDrawingPolygonView({%H-}toolDest: TBGRABitmap): TRect; override;
    function FinalPolygonView(toolDest: TBGRABitmap): TRect; override;
    function GetIsSelectingTool: boolean; override;
    function GetFillColor: TBGRAPixel; override;
  end;

  { TToolSelectSpline }

  TToolSelectSpline = class(TToolGenericSpline)
  protected
    function HandDrawingPolygonView(toolDest: TBGRABitmap): TRect; override;
    function FinalPolygonView({%H-}toolDest: TBGRABitmap): TRect; override;
    function GetIsSelectingTool: boolean; override;
    function GetFillColor: TBGRAPixel; override;
  end;

  { TToolMagicWand }

  TToolMagicWand = class(TGenericTool)
  protected
    function GetIsSelectingTool: boolean; override;
    function DoToolDown(toolDest: TBGRABitmap; pt: TPoint; {%H-}ptF: TPointF;
      rightBtn: boolean): TRect; override;
  end;

  { TToolSelectionPen }

  TToolSelectionPen = class(TToolPen)
  protected
    function GetIsSelectingTool: boolean; override;
    function StartDrawing(toolDest: TBGRABitmap; ptF: TPointF; rightBtn: boolean): TRect; override;
    function ContinueDrawing(toolDest: TBGRABitmap; originF, destF: TPointF): TRect; override;
  end;

  { TTransformSelectionTool }

  TTransformSelectionTool = class(TGenericTool)
  protected
    function GetIsSelectingTool: boolean; override;
    function GetAction: TLayerAction; override;
    function FixSelectionTransform: boolean; override;
    function DoGetToolDrawingLayer: TBGRABitmap; override;
  end;

  { TToolMoveSelection }

  TToolMoveSelection = class(TTransformSelectionTool)
  protected
    handMoving: boolean;
    handOrigin: TPoint;
    function DoToolDown({%H-}toolDest: TBGRABitmap; pt: TPoint; {%H-}ptF: TPointF;
      {%H-}rightBtn: boolean): TRect; override;
    function DoToolMove({%H-}toolDest: TBGRABitmap; pt: TPoint; {%H-}ptF: TPointF): TRect; override;
    procedure DoToolMoveAfter(pt: TPoint; {%H-}ptF: TPointF); override;
  public
    function ToolUp: TRect; override;
    destructor Destroy; override;
  end;

  { TToolRotateSelection }

  TToolRotateSelection = class(TTransformSelectionTool)
  protected
    class var HintShowed: boolean;
    handMoving: boolean;
    handOrigin: TPointF;
    snapRotate: boolean;
    snapAngle: single;
    FOriginalTransform: TAffineMatrix;
    FCurrentAngle: single;
    FCurrentCenter: TPointF;
    function DoToolDown({%H-}toolDest: TBGRABitmap; {%H-}pt: TPoint; ptF: TPointF;
      rightBtn: boolean): TRect; override;
    function DoToolMove({%H-}toolDest: TBGRABitmap; {%H-}pt: TPoint; ptF: TPointF): TRect; override;
    function GetStatusText: string; override;
    procedure UpdateTransform;
  public
    constructor Create(AManager: TToolManager); override;
    function ToolKeyDown(var key: Word): TRect; override;
    function ToolKeyUp(var key: Word): TRect; override;
    function ToolUp: TRect; override;
    function Render(VirtualScreen: TBGRABitmap; {%H-}VirtualScreenWidth, {%H-}VirtualScreenHeight: integer; BitmapToVirtualScreen: TBitmapToVirtualScreenFunction):TRect; override;
    destructor Destroy; override;
  end;

implementation

uses types, ugraph, LCLType, LazPaintType, Math, BGRATransform, BGRAPath,
  BGRAPen, LCVectorRectShapes;

{ TVectorialSelectTool }

function TVectorialSelectTool.GetIsSelectingTool: boolean;
begin
  Result:= true;
end;

procedure TVectorialSelectTool.AssignShapeStyle;
var
  f: TVectorShapeFields;
begin
  f:= FShape.Fields;
  if vsfPenFill in f then FShape.PenFill.Clear;
  if vsfPenStyle in f Then FShape.PenStyle := ClearPenStyle;
  if vsfBackFill in f then
  begin
    if FSwapColor then
      FShape.BackFill.SetSolid(BGRABlack)
    else
      FShape.BackFill.SetSolid(BGRAWhite);
  end;
  if FShape is TCustomRectShape then
  begin
    if Manager.ToolRatio = 0 then
      TCustomRectShape(FShape).FixedRatio:= EmptySingle
    else
      TCustomRectShape(FShape).FixedRatio:= Manager.ToolRatio;
  end;
end;

function TVectorialSelectTool.RoundCoordinate(ptF: TPointF): TPointF;
begin
  Result:= PointF(floor(ptF.x)+0.5,floor(ptF.y)+0.5);
end;

function TVectorialSelectTool.UpdateShape(toolDest: TBGRABitmap): TRect;
begin
  if BigImage and FQuickDefine then
    result := OnlyRenderChange
  else
    Result:= inherited UpdateShape(toolDest);
end;

procedure TVectorialSelectTool.QuickDefineEnd;
begin
  UpdateShape(GetToolDrawingLayer);
end;

function TVectorialSelectTool.BigImage: boolean;
begin
  result := GetToolDrawingLayer.NbPixels > 480000;
end;

{ TToolSelectRect }

function TToolSelectRect.CreateShape: TVectorShape;
begin
  result := TRectShape.Create(nil);
end;

function TToolSelectRect.Render(VirtualScreen: TBGRABitmap; VirtualScreenWidth,
  VirtualScreenHeight: integer;
  BitmapToVirtualScreen: TBitmapToVirtualScreenFunction): TRect;
var
  ab: TAffineBox;
  ptsF: ArrayOfTPointF;
  pts: array of TPoint;
  i: Integer;
  abBounds: TRect;
begin
  Result:= inherited Render(VirtualScreen, VirtualScreenWidth,
      VirtualScreenHeight, BitmapToVirtualScreen);

  if BigImage and FQuickDefine then
  begin
    ab := TCustomRectShape(FShape).GetAffineBox(FEditor.Matrix, true);
    abBounds := ab.RectBounds;
    abBounds.Inflate(1,1);
    result := RectUnion(result, abBounds);
    if Assigned(VirtualScreen) then
    begin
      ptsF := ab.AsPolygon;
      setlength(pts, length(ptsF));
      for i := 0 to high(ptsF) do
        pts[i] := ptsF[i].Round;
      VirtualScreen.DrawPolygonAntialias(pts,BGRAWhite,BGRABlack,FrameDashLength);
    end;
  end;
end;

{ TToolSelectEllipse }

function TToolSelectEllipse.CreateShape: TVectorShape;
begin
  result := TEllipseShape.Create(nil);
end;

function TToolSelectEllipse.Render(VirtualScreen: TBGRABitmap;
  VirtualScreenWidth, VirtualScreenHeight: integer;
  BitmapToVirtualScreen: TBitmapToVirtualScreenFunction): TRect;
var
  ab: TAffineBox;
  ptsF: ArrayOfTPointF;
  pts: array of TPoint;
  i: Integer;
  abBounds: TRect;
begin
  Result:= inherited Render(VirtualScreen, VirtualScreenWidth,
      VirtualScreenHeight, BitmapToVirtualScreen);

  if BigImage and FQuickDefine then
  begin
    ab := TCustomRectShape(FShape).GetAffineBox(FEditor.Matrix, true);
    abBounds := ab.RectBounds;
    abBounds.Inflate(1,1);
    result := RectUnion(result, abBounds);
    if Assigned(VirtualScreen) then
    begin
      with TCustomRectShape(FShape) do
        ptsF := BGRAPath.ComputeEllipse(FEditor.Matrix*Origin,
                    FEditor.Matrix*XAxis,FEditor.Matrix*YAxis);
      setlength(pts, length(ptsF));
      for i := 0 to high(ptsF) do
        pts[i] := ptsF[i].Round;
      VirtualScreen.DrawPolygonAntialias(pts,BGRAWhite,BGRABlack,FrameDashLength);
    end;
  end;
end;

{ TTransformSelectionTool }

function TTransformSelectionTool.GetIsSelectingTool: boolean;
begin
  result := true;
end;

function TTransformSelectionTool.GetAction: TLayerAction;
begin
  Result:= nil;
end;

function TTransformSelectionTool.FixSelectionTransform: boolean;
begin
  Result:= false;
end;

function TTransformSelectionTool.DoGetToolDrawingLayer: TBGRABitmap;
begin
  result := Manager.Image.SelectionMaskReadonly;
end;

{ TToolRotateSelection }

function TToolRotateSelection.DoToolDown(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF; rightBtn: boolean): TRect;
begin
  result := EmptyRect;
  if not handMoving and not Manager.Image.SelectionMaskEmpty then
  begin
    if rightBtn then
    begin
      if FCurrentAngle <> 0 then
      begin
        FCurrentAngle := 0;
        FCurrentCenter := ptF;
        UpdateTransform;
      end else
      begin
        FCurrentCenter := ptF;
        UpdateTransform;
      end;
      result := OnlyRenderChange;
    end else
    begin
      handMoving := true;
      handOrigin := ptF;
    end;
  end;
end;

function TToolRotateSelection.DoToolMove(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF): TRect;
var angleDiff: single;
begin
  if not HintShowed then
  begin
    Manager.ToolPopup(tpmCtrlRestrictRotation);
    HintShowed:= true;
  end;
  if handMoving and ((handOrigin.X <> ptF.X) or (handOrigin.Y <> ptF.Y)) then
  begin
    angleDiff := ComputeAngle(ptF.X-FCurrentCenter.X,ptF.Y-FCurrentCenter.Y)-
                 ComputeAngle(handOrigin.X-FCurrentCenter.X,handOrigin.Y-FCurrentCenter.Y);
    if snapRotate then
    begin
      snapAngle += angleDiff;
      FCurrentAngle := round(snapAngle/15)*15;
    end
     else
       FCurrentAngle := FCurrentAngle + angleDiff;
    UpdateTransform;
    handOrigin := ptF;
    result := OnlyRenderChange;
  end else
    result := EmptyRect;
end;

function TToolRotateSelection.GetStatusText: string;
begin
  Result:= 'α = '+FloatToStrF(FCurrentAngle,ffFixed,5,1);
end;

procedure TToolRotateSelection.UpdateTransform;
begin
  Manager.Image.SelectionTransform := AffineMatrixTranslation(FCurrentCenter.X,FCurrentCenter.Y)*
                                   AffineMatrixRotationDeg(FCurrentAngle)*
                                   AffineMatrixTranslation(-FCurrentCenter.X,-FCurrentCenter.Y)*FOriginalTransform;
end;

constructor TToolRotateSelection.Create(AManager: TToolManager);
begin
  inherited Create(AManager);
  FCurrentCenter := Manager.Image.SelectionTransform * Manager.Image.GetSelectionMaskCenter;
  FOriginalTransform := Manager.Image.SelectionTransform;
  FCurrentAngle := 0;
end;

function TToolRotateSelection.ToolKeyDown(var key: Word): TRect;
begin
  result := EmptyRect;
  if key = VK_CONTROL then
  begin
    if not snapRotate then
    begin
      snapRotate := true;
      snapAngle := FCurrentAngle;

      if handMoving then
      begin
        FCurrentAngle := round(snapAngle/15)*15;
        UpdateTransform;
        result := OnlyRenderChange;
      end;
    end;
    Key := 0;
  end else
  if key = VK_ESCAPE then
  begin
    if FCurrentAngle <> 0 then
    begin
      FCurrentAngle := 0;
      UpdateTransform;
      result := OnlyRenderChange;
    end;
    Key := 0;
  end;
end;

function TToolRotateSelection.ToolKeyUp(var key: Word): TRect;
begin
  if key = VK_CONTROL then
  begin
    snapRotate := false;
    Key := 0;
  end;
  result := EmptyRect;
end;

function TToolRotateSelection.ToolUp: TRect;
begin
  handMoving:= false;
  Result:= EmptyRect;
end;

function TToolRotateSelection.Render(VirtualScreen: TBGRABitmap;
  VirtualScreenWidth, VirtualScreenHeight: integer; BitmapToVirtualScreen: TBitmapToVirtualScreenFunction): TRect;
var pictureRotateCenter: TPointF;
begin
  pictureRotateCenter := BitmapToVirtualScreen(FCurrentCenter);
  result := NicePoint(VirtualScreen, pictureRotateCenter.X,pictureRotateCenter.Y);
end;

destructor TToolRotateSelection.Destroy;
begin
  if handMoving then handMoving := false;
  inherited Destroy;
end;

{ TToolMoveSelection }

function TToolMoveSelection.DoToolDown(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF; rightBtn: boolean): TRect;
begin
  if not handMoving then
  begin
    handMoving := true;
    handOrigin := pt;
  end;
  result := EmptyRect;
end;

function TToolMoveSelection.DoToolMove(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF): TRect;
var dx,dy: integer;
begin
  result := EmptyRect;
  if handMoving and ((handOrigin.X <> pt.X) or (handOrigin.Y <> pt.Y)) then
  begin
    dx := pt.X-HandOrigin.X;
    dy := pt.Y-HandOrigin.Y;
    Manager.Image.SelectionTransform := AffineMatrixTranslation(dx,dy) * Manager.Image.SelectionTransform;
    result := OnlyRenderChange;
  end;
end;

procedure TToolMoveSelection.DoToolMoveAfter(pt: TPoint; ptF: TPointF);
begin
  if handMoving then handOrigin := pt;
end;

function TToolMoveSelection.ToolUp: TRect;
begin
  handMoving := false;
  result := EmptyRect;
end;

destructor TToolMoveSelection.Destroy;
begin
  if handMoving then handMoving := false;
  inherited Destroy;
end;

{ TToolSelectSpline }

function TToolSelectSpline.HandDrawingPolygonView(toolDest: TBGRABitmap): TRect;
var
  splinePoints: ArrayOfTPointF;
begin
  if Manager.ToolSplineEasyBezier then
  begin
    NeedCurveMode;
    splinePoints := EasyBezierCurve(polygonPoints,True,FCurveMode,EasyBezierMinimumDotProduct).ToPoints;
  end else
    splinePoints := toolDest.ComputeClosedSpline(polygonPoints,Manager.ToolSplineStyle);
  FRenderedPolygonPoints := splinePoints;

  if length(splinePoints) > 2 then
  begin
    toolDest.FillPolyAntialias(splinePoints, fillColor);
    result := GetShapeBounds(splinePoints,1);
  end else
    result := EmptyRect;
end;

function TToolSelectSpline.FinalPolygonView(toolDest: TBGRABitmap): TRect;
begin
  if FAfterHandDrawing then
    result := HandDrawingPolygonView(toolDest)
  else
    result := EmptyRect;
end;

function TToolSelectSpline.GetIsSelectingTool: boolean;
begin
  Result:= true;
end;

function TToolSelectSpline.GetFillColor: TBGRAPixel;
begin
  if swapedColor then
    result := BGRABlack
  else
    result := BGRAWhite;
end;

{ TToolSelectionPen }

function TToolSelectionPen.GetIsSelectingTool: boolean;
begin
  Result:= true;
end;

function TToolSelectionPen.StartDrawing(toolDest: TBGRABitmap; ptF: TPointF;
  rightBtn: boolean): TRect;
begin
  if rightBtn then penColor := BGRABlack else penColor := BGRAWhite;
  toolDest.DrawLineAntialias(ptF.X,ptF.Y,ptF.X,ptF.Y,penColor,Manager.ToolPenWidth,True);
  result := GetShapeBounds([ptF],Manager.ToolPenWidth+1);
end;

function TToolSelectionPen.ContinueDrawing(toolDest: TBGRABitmap; originF,
  destF: TPointF): TRect;
begin
  toolDest.DrawLineAntialias(destF.X,destF.Y,originF.X,originF.Y,penColor,Manager.ToolPenWidth,False);
  result := GetShapeBounds([destF,originF],Manager.ToolPenWidth+1);
end;

{ TToolMagicWand }

function TToolMagicWand.GetIsSelectingTool: boolean;
begin
  Result:= true;
end;

function TToolMagicWand.DoToolDown(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF; rightBtn: boolean): TRect;
var penColor: TBGRAPixel;
begin
  if not Manager.Image.CurrentLayerVisible then
  begin
    result := EmptyRect;
    exit;
  end;
  if rightBtn then penColor := BGRABlack else penColor := BGRAWhite;
  Manager.Image.CurrentLayerReadOnly.ParallelFloodFill(pt.X,pt.Y,toolDest,penColor,fmDrawWithTransparency,Manager.ToolTolerance);
  result := rect(0,0,toolDest.Width,toolDest.Height);
  Action.NotifyChange(toolDest, result);
  ValidateAction;
end;

{ TToolSelectPoly }

function TToolSelectPoly.HandDrawingPolygonView(toolDest: TBGRABitmap): TRect;
begin
  result := EmptyRect;
  //nothing
end;

function TToolSelectPoly.FinalPolygonView(toolDest: TBGRABitmap): TRect;
var
  i: Integer;
begin
  if length(polygonPoints) > 2 then
  begin
    toolDest.FillPolyAntialias(polygonPoints, fillColor);
    result := GetShapeBounds(polygonPoints,1);
  end else
    result := EmptyRect;
  setlength(FRenderedPolygonPoints, length(polygonPoints));
  for i := 0 to high(polygonPoints) do
    FRenderedPolygonPoints[i] := polygonPoints[i];
end;

function TToolSelectPoly.GetIsSelectingTool: boolean;
begin
  result := true;
end;

function TToolSelectPoly.GetFillColor: TBGRAPixel;
begin
  if swapedColor then
    result := BGRABlack
  else
    result := BGRAWhite;
end;

initialization

  RegisterTool(ptMagicWand,TToolMagicWand);
  RegisterTool(ptSelectPen,TToolSelectionPen);
  RegisterTool(ptSelectRect,TToolSelectRect);
  RegisterTool(ptSelectEllipse,TToolSelectEllipse);
  RegisterTool(ptSelectPoly,TToolSelectPoly);
  RegisterTool(ptSelectSpline,TToolSelectSpline);
  RegisterTool(ptMoveSelection,TToolMoveSelection);
  RegisterTool(ptRotateSelection,TToolRotateSelection);

end.

