unit UToolFloodFill;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, utool, utoolbasic, BGRABitmap, BGRABitmapTypes,
  BGRAGradientOriginal, LCVectorOriginal;

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

  TToolGradient = class(TVectorialTool)
  protected
    function CreateShape: TVectorShape; override;
    procedure AssignShapeStyle; override;
    procedure QuickDefineShape(AStart,AEnd: TPointF); override;
    function SlowShape: boolean; override;
    function GetStatusText: string; override;
  end;

implementation

uses ugraph, LazPaintType, BGRAGradientScanner, LCVectorRectShapes;

{ TToolGradient }

function TToolGradient.CreateShape: TVectorShape;
begin
  result := TRectShape.Create(nil);
  result.QuickDefine(PointF(-0.5,-0.5),PointF(Manager.Image.Width-0.5,Manager.Image.Height-0.5));
  result.PenFill.Clear;
  result.BackFill.SetGradient(TBGRALayerGradientOriginal.Create,true);
  result.Usermode := vsuEditBackFill;
end;

procedure TToolGradient.AssignShapeStyle;
begin
  with FShape.BackFill.Gradient do
  begin
    StartColor := Manager.ToolForeColor;
    EndColor := Manager.ToolBackColor;
    GradientType := Manager.ToolGradientType;
    case Manager.ToolGradientColorspace of
     gcsLinearRgb: ColorInterpolation := ciStdRGB;
     gcsHueCW: ColorInterpolation := ciLinearHSLPositive;
     gcsHueCCW: ColorInterpolation := ciLinearHSLNegative;
     gcsCorrectedHueCW: ColorInterpolation := ciGSBPositive;
     gcsCorrectedHueCCW: ColorInterpolation := ciGSBPositive;
    else
      ColorInterpolation := ciLinearRGB;
    end;
    if Manager.ToolGradientSine then
      Repetition := grSine
    else
      Repetition := grPad;
  end;
end;

procedure TToolGradient.QuickDefineShape(AStart, AEnd: TPointF);
begin
  FShape.BackFill.Gradient.Origin := AStart;
  FShape.BackFill.Gradient.XAxis := AEnd;
end;

function TToolGradient.GetStatusText: string;
begin
  if Assigned(FShape) then
  begin
    with FShape.BackFill.Gradient do
      result := 'x1 = '+FloatToStrF(Origin.x,ffFixed,6,1)+'|y1 = '+FloatToStrF(Origin.y,ffFixed,6,1)+'|'+
      'x2 = '+FloatToStrF(XAxis.x,ffFixed,6,1)+'|y2 = '+FloatToStrF(XAxis.y,ffFixed,6,1)+'|'+
      'Δx = '+FloatToStrF(abs(XAxis.x-Origin.x),ffFixed,6,1)+'|Δy = '+FloatToStrF(abs(XAxis.y-Origin.y),ffFixed,6,1);
  end
  else
    Result:=inherited GetStatusText;
end;

function TToolGradient.SlowShape: boolean;
begin
  Result:= true;
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
  Action.NotifyChange(toolDest, rect(0,0,toolDest.Width,toolDest.Height));
  ValidateAction;
  result := OnlyRenderChange;
end;

initialization

  RegisterTool(ptFloodFill, TToolFloodFill);
  RegisterTool(ptGradient, TToolGradient);

end.

