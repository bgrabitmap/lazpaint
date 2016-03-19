unit UToolFloodFill;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, utool, utoolbasic, BGRABitmap, BGRABitmapTypes;

type

  { TToolFloodFill }

  TToolFloodFill = class(TGenericTool)
  protected
    penColor: TBGRAPixel;
    function GetIsSelectingTool: boolean; override;
  public
    function DoToolDown(toolDest: TBGRABitmap; pt: TPoint; {%H-}ptF: TPointF;
      rightBtn: boolean): TRect; override;
  end;

  { TToolGradient }

  TToolGradient = class(TToolRectangle)
  private
    lastRenderWasHighQuality: boolean;
  protected
    function UpdateShape(toolDest: TBGRABitmap; HighQuality: boolean): TRect;
    function UpdateShape(toolDest: TBGRABitmap): TRect; override;
    function ShouldFinishShapeWhenFirstMouseUp: boolean; override;
    function FinishShape(ToolDest: TBGRABitmap): TRect; override;
    function BorderTest(ptF: TPointF): TRectangularBorderTest; override;
    function RenderAllCornerPositions: boolean; override;
    function DoToolDown(toolDest: TBGRABitmap; pt: TPoint; ptF: TPointF;
      rightBtn: boolean): TRect; override;
  public
    function ToolUp: TRect; override;
    function Render(VirtualScreen: TBGRABitmap; VirtualScreenWidth, VirtualScreenHeight: integer; BitmapToVirtualScreen: TBitmapToVirtualScreenFunction):TRect; override;
  end;

implementation

uses ugraph, LazPaintType;

{ TToolGradient }

function TToolGradient.UpdateShape(toolDest: TBGRABitmap; HighQuality: boolean
  ): TRect;
var ditherAlgo: TDitheringAlgorithm;
begin
  lastRenderWasHighQuality:= HighQuality;
   if HighQuality then
     ditherAlgo:= daFloydSteinberg
   else
     ditherAlgo:= daNearestNeighbor;
   ClearShape;
   toolDest.GradientFill(0,0,toolDest.Width,toolDest.Height, penColor, fillColor,
     Manager.ToolGradientType, pointf(rectOrigin.X,rectOrigin.Y), pointf(rectDest.X,rectDest.Y),
     dmDrawWithTransparency, True, Manager.ToolGradientSine,
     ditherAlgo);
   result := rect(0,0,Manager.Image.Width,Manager.Image.Height);
end;

function TToolGradient.UpdateShape(toolDest: TBGRABitmap): TRect;
begin
   result := UpdateShape(toolDest,false);
end;

function TToolGradient.ShouldFinishShapeWhenFirstMouseUp: boolean;
begin
  Result:=false;
end;

function TToolGradient.FinishShape(ToolDest: TBGRABitmap): TRect;
begin
  Result:= UpdateShape(toolDest, lastRenderWasHighQuality);
end;

function TToolGradient.BorderTest(ptF: TPointF): TRectangularBorderTest;
begin
  Result:=inherited BorderTest(ptF);
  if (result <> [btOriginY,btOriginX]) and (result <> [btDestY,btDestX]) then
    result := [];
end;

function TToolGradient.RenderAllCornerPositions: boolean;
begin
  Result:=false;
end;

function TToolGradient.DoToolDown(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF; rightBtn: boolean): TRect;
begin
  Result:=inherited DoToolDown(toolDest, pt, ptF, rightBtn);
  if rectMovingPoint then lastRenderWasHighQuality := false;
end;

function TToolGradient.ToolUp: TRect;
begin
  result := inherited ToolUp;
  if not lastRenderWasHighQuality then
    result := RectUnion(result, UpdateShape(GetToolDrawingLayer,true));
end;

function TToolGradient.Render(VirtualScreen: TBGRABitmap;
  VirtualScreenWidth, VirtualScreenHeight: integer; BitmapToVirtualScreen: TBitmapToVirtualScreenFunction): TRect;
var ptF: TPointF;
begin
  result := inherited Render(VirtualScreen, VirtualScreenWidth,VirtualScreenHeight, BitmapToVirtualScreen);
  if rectDrawing then
  begin
    ptF := BitmapToVirtualScreen(PointF(rectOrigin.X,rectOrigin.Y));
    result := RectUnion(result,NicePoint(VirtualScreen, ptF));
    ptF := BitmapToVirtualScreen(PointF(rectDest.X,rectDest.Y));
    result := RectUnion(result,NicePoint(VirtualScreen, ptF));
  end;
end;

{ TToolFloodFill }

function TToolFloodFill.GetIsSelectingTool: boolean;
begin
  Result:= false;
end;

function TToolFloodFill.DoToolDown(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF; rightBtn: boolean): TRect;
var
   floodFillMask,floodFillTex: TBGRABitmap;
begin
  if rightBtn then penColor := Manager.ToolBackColor else penColor := Manager.ToolForeColor;
  if Manager.GetToolTextureAfterAlpha <> nil then
  begin
    floodFillMask := TBGRABitmap.Create(toolDest.Width,toolDest.Height,BGRABlack);
    floodFillTex := Manager.GetToolTextureAfterAlpha.GetPart(rect(0,0,toolDest.Width,toolDest.Height)) as TBGRABitmap;
    toolDest.ParallelFloodFill(pt.X,pt.Y,floodFillMask,BGRAWhite,fmSet,Manager.ToolTolerance);
    floodFillTex.ApplyMask(floodFillMask);
    toolDest.PutImage(0,0,floodFillTex,dmDrawWithTransparency);
    floodFillMask.Free;
    floodFillTex.Free;
  end else
    if Manager.ToolFloodFillOptionProgressive then
      toolDest.FloodFill(pt.X,pt.Y,penColor,fmProgressive,Manager.ToolTolerance) else
        toolDest.FloodFill(pt.X,pt.Y,penColor,fmDrawWithTransparency,Manager.ToolTolerance);
  Manager.Image.LayerMayChangeCompletely(toolDest);
  ValidateAction;
  result := OnlyRenderChange;
end;

initialization

  RegisterTool(ptFloodFill, TToolFloodFill);
  RegisterTool(ptGradient, TToolGradient);

end.

