unit utoolfloodfill;

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
    function UpdateShape(toolDest: TBGRABitmap): TRect; override;
    function NeedClearShape: boolean; override;
  public
    function ToolUp: TRect; override;
    procedure Render(VirtualScreen: TBGRABitmap; BitmapToVirtualScreen: TBitmapToVirtualScreenFunction); override;
  end;

implementation

uses ugraph;

{ TToolGradient }

function TToolGradient.UpdateShape(toolDest: TBGRABitmap): TRect;
begin
   ClearShape;
   toolDest.GradientFill(0,0,toolDest.Width,toolDest.Height, penColor, fillColor,
     Manager.ToolGradientType, pointf(rectOrigin.X,rectOrigin.Y), pointf(rectDest.X,rectDest.Y), dmDrawWithTransparency, True, Manager.ToolGradientSine);
   result := rect(0,0,Manager.Image.Width,Manager.Image.Height);
end;

function TToolGradient.NeedClearShape: boolean;
begin
  Result:=true;
end;

function TToolGradient.ToolUp: TRect;
begin
  inherited ToolUp;
  result := ToolRepaintOnly;
end;

procedure TToolGradient.Render(VirtualScreen: TBGRABitmap;
  BitmapToVirtualScreen: TBitmapToVirtualScreenFunction);
begin
  inherited Render(VirtualScreen, BitmapToVirtualScreen);
  if rectDrawing then
  begin
    NicePoint(VirtualScreen, BitmapToVirtualScreen(PointF(rectOrigin.X,rectOrigin.Y)) );
    NicePoint(VirtualScreen, BitmapToVirtualScreen(PointF(rectDest.X,rectDest.Y)) );
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
  Manager.Image.RetrieveSelectionIfLayerEmpty;
  if Manager.ToolTexture <> nil then
  begin
    floodFillMask := TBGRABitmap.Create(toolDest.Width,toolDest.Height,BGRABlack);
    floodFillTex := Manager.ToolTexture.GetPart(rect(0,0,toolDest.Width,toolDest.Height)) as TBGRABitmap;
    toolDest.ParallelFloodFill(pt.X,pt.Y,floodFillMask,BGRAWhite,fmSet,Manager.ToolTolerance);
    floodFillTex.ApplyMask(floodFillMask);
    toolDest.PutImage(0,0,floodFillTex,dmDrawWithTransparency);
    floodFillMask.Free;
    floodFillTex.Free;
  end else
    if Manager.ToolFloodFillOptionProgressive then
      toolDest.FloodFill(pt.X,pt.Y,penColor,fmProgressive,Manager.ToolTolerance) else
        toolDest.FloodFill(pt.X,pt.Y,penColor,fmDrawWithTransparency,Manager.ToolTolerance);
  Manager.Image.ImageMayChangeCompletely;
  Manager.Image.SaveLayerOrSelectionUndo;
  result := ToolRepaintOnly;
end;

initialization

  RegisterTool(ptFloodFill, TToolFloodFill);
  RegisterTool(ptGradient, TToolGradient);

end.

