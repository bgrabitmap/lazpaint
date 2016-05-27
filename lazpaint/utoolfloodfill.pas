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
  protected
    function UpdateShape(toolDest: TBGRABitmap; HighQuality: boolean): TRect;
    function UpdateShape(toolDest: TBGRABitmap): TRect; override;
    function ShouldFinishShapeWhenFirstMouseUp: boolean; override;
    function FinishShape(ToolDest: TBGRABitmap): TRect; override;
    function BorderTest(ptF: TPointF): TRectangularBorderTest; override;
    function RenderAllCornerPositions: boolean; override;
    function LeaveMovingPoint: TRect; override;
    function GetStatusText: string; override;
  public
    function Render(VirtualScreen: TBGRABitmap; VirtualScreenWidth, VirtualScreenHeight: integer; BitmapToVirtualScreen: TBitmapToVirtualScreenFunction):TRect; override;
  end;

implementation

uses ugraph, LazPaintType, BGRAGradientScanner;

{ TToolGradient }

function TToolGradient.UpdateShape(toolDest: TBGRABitmap; HighQuality: boolean
  ): TRect;
var ditherAlgo: TDitheringAlgorithm;
   g: TBGRACustomGradient;
begin
   if HighQuality then
     ditherAlgo:= daFloydSteinberg
   else
     ditherAlgo:= daNearestNeighbor;
   ClearShape;
   case Manager.ToolGradientColorspace of
     gcsLinearRgb: g := TBGRASimpleGradientWithoutGammaCorrection.Create(penColor, fillColor);
     gcsHueCW: g := TBGRAHueGradient.Create(penColor, fillColor, [hgoPositiveDirection]);
     gcsHueCCW: g := TBGRAHueGradient.Create(penColor, fillColor, [hgoNegativeDirection]);
     gcsCorrectedHueCW: g := TBGRAHueGradient.Create(penColor, fillColor, [hgoPositiveDirection, hgoLightnessCorrection]);
     gcsCorrectedHueCCW: g := TBGRAHueGradient.Create(penColor, fillColor, [hgoNegativeDirection, hgoLightnessCorrection]);
   else
     g := TBGRASimpleGradientWithGammaCorrection.Create(penColor, fillColor);
   end;
   toolDest.GradientFill(0,0,toolDest.Width,toolDest.Height, g,
     Manager.ToolGradientType, pointf(rectOrigin.X,rectOrigin.Y), pointf(rectDest.X,rectDest.Y),
     dmDrawWithTransparency, Manager.ToolGradientSine, ditherAlgo);
   g.Free;
   result := rect(0,0,toolDest.Width,toolDest.Height);
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
  Result:= UpdateShape(toolDest, not rectDrawing and not rectMovingPoint);
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

function TToolGradient.LeaveMovingPoint: TRect;
begin
  result := FinishShape(GetToolDrawingLayer);
end;

function TToolGradient.GetStatusText: string;
begin
  if rectDrawing or afterRectDrawing then
    result := 'x1 = '+inttostr(round(rectOrigin.x))+'|y1 = '+inttostr(round(rectOrigin.y))+'|'+
    'x2 = '+inttostr(round(rectDest.x))+' |y2 = '+inttostr(round(rectDest.y))+'|'+
    'Δ = '+inttostr(round(sqrt(sqr(rectDest.x-rectOrigin.x)+sqr(rectDest.y-rectOrigin.y))))
  else
    Result:=inherited GetStatusText;
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

