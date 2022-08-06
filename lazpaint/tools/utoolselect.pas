// SPDX-License-Identifier: GPL-3.0-only
unit UToolSelect;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, BGRABitmapTypes, BGRABitmap,
  UTool, UToolBasic, UToolVectorial, UToolPolygon,
  ULayerAction, LCVectorOriginal;

type
  { TVectorialSelectTool }

  TVectorialSelectTool = class(TVectorialTool)
  protected
    function GetIsSelectingTool: boolean; override;
    procedure AssignShapeStyle({%H-}AMatrix: TAffineMatrix; {%H-}AAlwaysFit: boolean); override;
    function RoundCoordinate(constref ptF: TPointF): TPointF; override;
    function UpdateShape(toolDest: TBGRABitmap): TRect; override;
    procedure ShapeChange({%H-}ASender: TObject; ABounds: TRectF; ADiff: TVectorShapeDiff); override;
    procedure QuickDefineEnd; override;
    function BigImage: boolean;
  public
    function GetContextualToolbars: TContextualToolbars; override;
  end;

  { TToolSelectRect }

  TToolSelectRect = class(TVectorialSelectTool)
  protected
    function ShapeClass: TVectorShapeAny; override;
  public
    function Render(VirtualScreen: TBGRABitmap; {%H-}VirtualScreenWidth, {%H-}VirtualScreenHeight: integer; BitmapToVirtualScreen: TBitmapToVirtualScreenFunction):TRect; override;
    function GetContextualToolbars: TContextualToolbars; override;
  end;

  { TToolSelectEllipse }

  TToolSelectEllipse = class(TVectorialSelectTool)
  protected
    function ShapeClass: TVectorShapeAny; override;
    function GetGridMatrix: TAffineMatrix; override;
  public
    function Render(VirtualScreen: TBGRABitmap; {%H-}VirtualScreenWidth, {%H-}VirtualScreenHeight: integer; BitmapToVirtualScreen: TBitmapToVirtualScreenFunction):TRect; override;
    function GetContextualToolbars: TContextualToolbars; override;
  end;

  { TToolSelectPoly }

  TToolSelectPoly = class(TToolPolygon)
  protected
    procedure AssignShapeStyle(AMatrix: TAffineMatrix; AAlwaysFit: boolean); override;
    function GetIsSelectingTool: boolean; override;
  public
    function GetContextualToolbars: TContextualToolbars; override;
  end;

  { TToolSelectSpline }

  TToolSelectSpline = class(TToolSpline)
  protected
    procedure AssignShapeStyle(AMatrix: TAffineMatrix; AAlwaysFit: boolean); override;
    function GetIsSelectingTool: boolean; override;
  public
    function GetContextualToolbars: TContextualToolbars; override;
  end;

  { TToolMagicWand }

  TToolMagicWand = class(TGenericTool)
  protected
    function GetIsSelectingTool: boolean; override;
    function DoToolDown(toolDest: TBGRABitmap; pt: TPoint; {%H-}ptF: TPointF;
      rightBtn: boolean): TRect; override;
  public
    function GetContextualToolbars: TContextualToolbars; override;
  end;

  { TToolSelectionPen }

  TToolSelectionPen = class(TToolPen)
  protected
    function GetIsSelectingTool: boolean; override;
    function GetUniversalBrush(ARightButton: boolean): TUniversalBrush; override;
  public
    function GetContextualToolbars: TContextualToolbars; override;
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
    handOriginF: TPointF;
    selectionTransformBefore: TAffineMatrix;
    function DoToolDown({%H-}toolDest: TBGRABitmap; {%H-}pt: TPoint; ptF: TPointF;
      {%H-}rightBtn: boolean): TRect; override;
    function DoToolMove({%H-}toolDest: TBGRABitmap; {%H-}pt: TPoint; ptF: TPointF): TRect; override;
  public
    constructor Create(AManager: TToolManager); override;
    function ToolUp: TRect; override;
    destructor Destroy; override;
  end;

  { TToolRotateSelection }

  TToolRotateSelection = class(TTransformSelectionTool)
  protected
    class var HintShown: boolean;
    FHandRotating, FHandTranslating: boolean;
    FHandOrigin: TPointF;
    FSnapMode: boolean;
    FUnsnappedAngle: single;
    FOriginalTransform: TAffineMatrix;
    FCurrentAngle: single;
    FCurrentCenter: TPointF;
    FOffsetBeforeMove, FFinalOffset: TPointF;
    function DoToolDown({%H-}toolDest: TBGRABitmap; {%H-}pt: TPoint; ptF: TPointF;
      rightBtn: boolean): TRect; override;
    function DoToolMove({%H-}toolDest: TBGRABitmap; {%H-}pt: TPoint; ptF: TPointF): TRect; override;
    function DoToolKeyDown(var key: Word): TRect; override;
    function DoToolKeyUp(var key: Word): TRect; override;
    function GetStatusText: string; override;
    procedure UpdateTransform;
  public
    class procedure ForgetHintShown;
    constructor Create(AManager: TToolManager); override;
    function ToolUp: TRect; override;
    function Render(VirtualScreen: TBGRABitmap; {%H-}VirtualScreenWidth, {%H-}VirtualScreenHeight: integer; BitmapToVirtualScreen: TBitmapToVirtualScreenFunction):TRect; override;
    destructor Destroy; override;
  end;

implementation

uses types, ugraph, LCLType, LazPaintType, Math, BGRATransform, BGRAPath,
  BGRAPen, LCVectorRectShapes, Controls, BGRAGrayscaleMask;

procedure AssignSelectShapeStyle(AShape: TVectorShape; ASwapColor: boolean);
var
  f: TVectorShapeFields;
begin
  f:= AShape.MultiFields;
  if vsfPenFill in f then AShape.PenFill.Clear;
  if vsfPenStyle in f Then AShape.PenStyle := ClearPenStyle;
  if vsfBackFill in f then
  begin
    if ASwapColor then
      AShape.BackFill.SetSolid(BGRABlack)
    else
      AShape.BackFill.SetSolid(BGRAWhite);
  end;
end;

{ TToolSelectSpline }

procedure TToolSelectSpline.AssignShapeStyle(AMatrix: TAffineMatrix; AAlwaysFit: boolean);
begin
  FShape.BeginUpdate;
  inherited AssignShapeStyle(AMatrix, AAlwaysFit);
  AssignSelectShapeStyle(FShape, FSwapColor);
  FShape.EndUpdate;
end;

function TToolSelectSpline.GetIsSelectingTool: boolean;
begin
  Result:= true;
end;

function TToolSelectSpline.GetContextualToolbars: TContextualToolbars;
begin
  Result:= [ctSplineStyle, ctCloseShape];
end;

{ TToolSelectPoly }

procedure TToolSelectPoly.AssignShapeStyle(AMatrix: TAffineMatrix; AAlwaysFit: boolean);
begin
  FShape.BeginUpdate;
  inherited AssignShapeStyle(AMatrix, AAlwaysFit);
  AssignSelectShapeStyle(FShape, FSwapColor);
  FShape.EndUpdate;
end;

function TToolSelectPoly.GetIsSelectingTool: boolean;
begin
  Result:= true;
end;

function TToolSelectPoly.GetContextualToolbars: TContextualToolbars;
begin
  Result:= [];
end;

{ TVectorialSelectTool }

function TVectorialSelectTool.GetIsSelectingTool: boolean;
begin
  Result:= true;
end;

procedure TVectorialSelectTool.AssignShapeStyle(AMatrix: TAffineMatrix; AAlwaysFit: boolean);
begin
  AssignSelectShapeStyle(FShape, FSwapColor);
  if FShape is TCustomRectShape then
  begin
    if Manager.ShapeRatio = 0 then
      TCustomRectShape(FShape).FixedRatio:= EmptySingle
    else
      TCustomRectShape(FShape).FixedRatio:= Manager.ShapeRatio;
  end;
end;

function TVectorialSelectTool.RoundCoordinate(constref ptF: TPointF): TPointF;
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

procedure TVectorialSelectTool.ShapeChange(ASender: TObject; ABounds: TRectF;
  ADiff: TVectorShapeDiff);
begin
  if BigImage and FQuickDefine then
  begin
    ADiff.Free;
    exit;
  end;
  inherited ShapeChange(ASender, ABounds, ADiff);
end;

procedure TVectorialSelectTool.QuickDefineEnd;
var
  toolDest: TBGRABitmap;
  r: TRect;
begin
  toolDest := GetToolDrawingLayer;
  r := UpdateShape(toolDest);
  Action.NotifyChange(toolDest, r);
end;

function TVectorialSelectTool.BigImage: boolean;
begin
  result := Manager.Image.Width*Manager.Image.Height > 480000;
end;

function TVectorialSelectTool.GetContextualToolbars: TContextualToolbars;
begin
  Result:= [];
end;

{ TToolSelectRect }

function TToolSelectRect.ShapeClass: TVectorShapeAny;
begin
  result := TRectShape;
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
    ab := TCustomRectShape(FShape).GetAffineBox(
      AffineMatrixTranslation(0.5,0.5)*FEditor.Matrix*AffineMatrixTranslation(-0.5,-0.5), false);
    abBounds := ab.RectBounds;
    abBounds.Inflate(1,1);
    result := RectUnion(result, abBounds);
    if Assigned(VirtualScreen) then
    begin
      ptsF := ab.AsPolygon;
      pts := nil;
      setlength(pts, length(ptsF));
      for i := 0 to high(ptsF) do
        pts[i] := (ptsF[i]+PointF(0.5,0.5)).Round;
      VirtualScreen.DrawPolygonAntialias(pts,BGRAWhite,BGRABlack,
        FrameDashLength*Manager.CanvasScale);
    end;
  end;
end;

function TToolSelectRect.GetContextualToolbars: TContextualToolbars;
begin
  Result:= [ctRatio];
end;

{ TToolSelectEllipse }

function TToolSelectEllipse.ShapeClass: TVectorShapeAny;
begin
  result := TEllipseShape;
end;

function TToolSelectEllipse.GetGridMatrix: TAffineMatrix;
begin
  result := AffineMatrixScale(0.5,0.5);
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
    ab := TCustomRectShape(FShape).GetAffineBox(
      AffineMatrixTranslation(0.5,0.5)*FEditor.Matrix*AffineMatrixTranslation(-0.5,-0.5), false);
    abBounds := ab.RectBounds;
    abBounds.Inflate(1,1);
    result := RectUnion(result, abBounds);
    if Assigned(VirtualScreen) then
    begin
      with TCustomRectShape(FShape) do
        ptsF := BGRAPath.ComputeEllipse(FEditor.Matrix*Origin,
                    FEditor.Matrix*XAxis,FEditor.Matrix*YAxis);
      pts := nil;
      setlength(pts, length(ptsF));
      for i := 0 to high(ptsF) do
        pts[i] := ptsF[i].Round;
      VirtualScreen.DrawPolygonAntialias(pts,BGRAWhite,BGRABlack,
        FrameDashLength*Manager.CanvasScale);
    end;
  end;
end;

function TToolSelectEllipse.GetContextualToolbars: TContextualToolbars;
begin
  Result:= [ctRatio];
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
  if not FHandRotating and not FHandTranslating and not Manager.Image.SelectionMaskEmpty then
  begin
    if rightBtn then
    begin
      if FSnapMode then
      begin
        ptF.x := round(ptF.x*2)/2;
        ptF.y := round(ptF.y*2)/2;
      end;
      FCurrentAngle := 0;
      FFinalOffset := PointF(0, 0);
      FCurrentCenter := ptF;
      UpdateTransform;
      result := OnlyRenderChange;
    end else
    begin
      if VectLen(ptF - (FCurrentCenter + FFinalOffset)) < SelectionMaxPointDistance then
        FHandTranslating:= true
      else
        FHandRotating := true;
      FHandOrigin := ptF;
      FOffsetBeforeMove := FFinalOffset;
    end;
  end;
end;

function TToolRotateSelection.DoToolMove(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF): TRect;
var angleDiff: single;
  finalCenter, newOfs: TPointF;
begin
  if not HintShown then
  begin
    Manager.ToolPopup(tpmHoldKeyRestrictRotation, VK_CONTROL);
    HintShown:= true;
  end;
  if FHandRotating and ((FHandOrigin.X <> ptF.X) or (FHandOrigin.Y <> ptF.Y)) then
  begin
    finalCenter := FCurrentCenter + FFinalOffset;
    angleDiff := ComputeAngle(ptF.X - finalCenter.X, ptF.Y - finalCenter.Y)-
                 ComputeAngle(FHandOrigin.X - finalCenter.X, FHandOrigin.Y - finalCenter.Y);
    if FSnapMode then
    begin
      FUnsnappedAngle += angleDiff;
      FCurrentAngle := round(FUnsnappedAngle/15)*15;
    end else
      FCurrentAngle := FCurrentAngle + angleDiff;
    UpdateTransform;
    FHandOrigin := ptF;
    result := OnlyRenderChange;
  end else
  if FHandTranslating and ((FHandOrigin.X <> ptF.X) or (FHandOrigin.Y <> ptF.Y)) then
  begin
    newOfs := FOffsetBeforeMove + ptF - FHandOrigin;
    if FSnapMode then
    begin
      newOfs.X := round(newOfs.x*2)/2;
      newOfs.Y := round(newOfs.y*2)/2;
    end;
    if newOfs <> FFinalOffset then
    begin
      FFinalOffset := newOfs;
      UpdateTransform;
      result := OnlyRenderChange;
    end else
      result := EmptyRect;
  end else
  begin
    if VectLen(ptF - (FCurrentCenter + FFinalOffset)) < SelectionMaxPointDistance then
      Cursor := crSizeAll else Cursor := crDefault;
    result := EmptyRect;
  end;
end;

function TToolRotateSelection.GetStatusText: string;
begin
  Result:= 'α = '+FloatToStrF(FCurrentAngle,ffFixed,5,1) + '|' +
           'Δx = '+FloatToStrF(FFinalOffset.X,ffFixed,6,1) + '|' +
           'Δy = '+FloatToStrF(FFinalOffset.Y,ffFixed,6,1);
end;

procedure TToolRotateSelection.UpdateTransform;
begin
  Manager.Image.SelectionTransform := AffineMatrixTranslation(FFinalOffset.X, FFinalOffset.Y) *
                                   AffineMatrixTranslation(FCurrentCenter.X,FCurrentCenter.Y) *
                                   AffineMatrixRotationDeg(FCurrentAngle) *
                                   AffineMatrixTranslation(-FCurrentCenter.X,-FCurrentCenter.Y) *
                                   FOriginalTransform;
end;

class procedure TToolRotateSelection.ForgetHintShown;
begin
  HintShown:= false;
end;

constructor TToolRotateSelection.Create(AManager: TToolManager);
begin
  inherited Create(AManager);
  FCurrentCenter := Manager.Image.SelectionTransform * Manager.Image.GetSelectionMaskCenter;
  FOriginalTransform := Manager.Image.SelectionTransform;
  FCurrentAngle := 0;
  FFinalOffset := PointF(0, 0);
end;

function TToolRotateSelection.DoToolKeyDown(var key: Word): TRect;
begin
  result := EmptyRect;
  if key = VK_CONTROL then
  begin
    if not FSnapMode then
    begin
      FSnapMode := true;
      FUnsnappedAngle := FCurrentAngle;

      if FHandRotating then
      begin
        FCurrentAngle := round(FUnsnappedAngle/15)*15;
        UpdateTransform;
        result := OnlyRenderChange;
      end else
      if FHandTranslating then
      begin
        FFinalOffset.x := round(FFinalOffset.x*2)/2;
        FFinalOffset.y := round(FFinalOffset.y*2)/2;
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
      FFinalOffset := PointF(0, 0);
      UpdateTransform;
      result := OnlyRenderChange;
    end;
    Key := 0;
  end;
end;

function TToolRotateSelection.DoToolKeyUp(var key: Word): TRect;
begin
  if key = VK_CONTROL then
  begin
    FSnapMode := false;
    Key := 0;
  end;
  result := EmptyRect;
end;

function TToolRotateSelection.ToolUp: TRect;
begin
  FHandRotating:= false;
  FHandTranslating:= false;
  Result:= EmptyRect;
end;

function TToolRotateSelection.Render(VirtualScreen: TBGRABitmap;
  VirtualScreenWidth, VirtualScreenHeight: integer; BitmapToVirtualScreen: TBitmapToVirtualScreenFunction): TRect;
var pictureRotateCenter: TPointF;
begin
  pictureRotateCenter := BitmapToVirtualScreen(FCurrentCenter + FFinalOffset);
  result := NicePoint(VirtualScreen, pictureRotateCenter.X,pictureRotateCenter.Y);
end;

destructor TToolRotateSelection.Destroy;
begin
  if FHandRotating then FHandRotating := false;
  if FHandTranslating then FHandTranslating := false;
  inherited Destroy;
end;

{ TToolMoveSelection }

function TToolMoveSelection.DoToolDown(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF; rightBtn: boolean): TRect;
begin
  if not handMoving and not Manager.Image.SelectionMaskEmpty then
  begin
    handMoving := true;
    handOriginF := ptF;
    selectionTransformBefore := Manager.Image.SelectionTransform;
  end;
  result := EmptyRect;
end;

function TToolMoveSelection.DoToolMove(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF): TRect;
var dx,dy: single;
  newSelTransform: TAffineMatrix;
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
    newSelTransform := AffineMatrixTranslation(dx,dy) * selectionTransformBefore;
    if Manager.Image.SelectionTransform <> newSelTransform then
    begin
      Manager.Image.SelectionTransform := newSelTransform;
      result := OnlyRenderChange;
    end;
  end;
end;

constructor TToolMoveSelection.Create(AManager: TToolManager);
begin
  inherited Create(AManager);
  handMoving := false;
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

{ TToolSelectionPen }

function TToolSelectionPen.GetIsSelectingTool: boolean;
begin
  Result:= true;
end;

function TToolSelectionPen.GetUniversalBrush(ARightButton: boolean): TUniversalBrush;
begin
  if ARightButton then
    TBGRABitmap.SolidBrush(result, BGRABlack, dmLinearBlend)
  else
    TBGRABitmap.SolidBrush(result, BGRAWhite, dmLinearBlend);
end;

function TToolSelectionPen.GetContextualToolbars: TContextualToolbars;
begin
  Result:= [ctPenWidth, ctAliasing];
end;

{ TToolMagicWand }

function TToolMagicWand.GetIsSelectingTool: boolean;
begin
  Result:= true;
end;

function TToolMagicWand.DoToolDown(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF; rightBtn: boolean): TRect;
var penColor: TBGRAPixel;
  ofs: TPoint;
  mask: TGrayscaleMask;
  source: TBGRABitmap;
  targetRect: TRect;
  psource: PBGRAPixel;
  yb, xb: Integer;
  pmask: PByte;
  compareColor: TExpandedPixel;
  diff, diffDiv: integer;
begin
  if not Manager.Image.CurrentLayerVisible then
  begin
    result := EmptyRect;
    exit;
  end;
  if rightBtn then penColor := BGRABlack else penColor := BGRAWhite;
  ofs := Manager.Image.LayerOffset[Manager.Image.CurrentLayerIndex];
  source := Manager.Image.CurrentLayerReadOnly;
  targetRect := RectWithSize(ofs.x, ofs.y, source.Width, source.Height);
  targetRect.Intersect( rect(0,0,toolDest.Width,toolDest.Height) );
  if not targetRect.IsEmpty then
  begin
    if ffProgressive in Manager.FloodFillOptions then
    begin
      mask := TGrayscaleMask.Create(targetRect.Width, targetRect.Height, 0);
      Manager.Image.CurrentLayerReadOnly.ParallelFloodFill(pt.X-ofs.X, pt.Y-ofs.Y,
        mask, ByteMaskWhite, fmDrawWithTransparency, Manager.Tolerance,
        ofs.X - targetRect.Left, ofs.Y - targetRect.Top);
      compareColor := GammaExpansion(source.GetPixel(pt.X-ofs.X, pt.Y-ofs.Y));
      diffDiv := Manager.Tolerance + (Manager.Tolerance shl 8) + 1;
      for yb := 0 to mask.Height-1 do
      begin
        psource := PBGRAPixel(source.GetPixelAddress(targetRect.Left - ofs.x, yb + targetRect.Top - ofs.y));
        pmask := mask.ScanLine[yb];
        for xb := mask.Width-1 downto 0 do
        begin
          if pmask^ <> 0 then
          begin
            diff := ExpandedDiff(psource^.ToExpanded, compareColor);
            pmask^ := (pmask^ * (diffDiv - diff) + (diffDiv shr 1)) div diffDiv;
          end;
          inc(pmask);
          inc(psource);
        end;
      end;
      toolDest.FillMask(targetRect.Left, targetRect.Top, mask, penColor, dmDrawWithTransparency);
      mask.Free;
    end else
      source.ParallelFloodFill(pt.X-ofs.X, pt.Y-ofs.Y,
        toolDest, penColor, fmDrawWithTransparency, Manager.Tolerance, ofs.X, ofs.Y);
  end;
  result := targetRect;
  Action.NotifyChange(toolDest, result);
  ValidateAction;
end;

function TToolMagicWand.GetContextualToolbars: TContextualToolbars;
begin
  Result:= [ctTolerance];
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

