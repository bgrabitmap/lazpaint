unit UToolFloodFill;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UTool, UToolVectorial, BGRABitmap, BGRABitmapTypes,
  BGRAGradientOriginal, BGRALayerOriginal, LCVectorOriginal, UStateType, LCVectorialFill;

type

  { TToolFloodFill }

  TToolFloodFill = class(TGenericTool)
  protected
    function GetIsSelectingTool: boolean; override;
  public
    function DoToolDown(toolDest: TBGRABitmap; pt: TPoint; {%H-}ptF: TPointF;
      rightBtn: boolean): TRect; override;
    function GetContextualToolbars: TContextualToolbars; override;
  end;

  { TToolGradient }

  TToolGradient = class(TVectorialTool)
  protected
    function CreateShape: TVectorShape; override;
    procedure DrawCustomShape(ADest: TBGRABitmap; AMatrix: TAffineMatrix; ADraft: boolean); override;
    procedure AssignShapeStyle({%H-}AMatrix: TAffineMatrix; {%H-}AAlwaysFit: boolean); override;
    procedure QuickDefineShape(AStart,AEnd: TPointF); override;
    function SlowShape: boolean; override;
    function GetStatusText: string; override;
    function ReplaceLayerAndAddShape(out ARect: TRect): TCustomImageDifference; override;
    function GetAllowedBackFillTypes: TVectorialFillTypes; override;
  public
    function GetContextualToolbars: TContextualToolbars; override;
  end;

implementation

uses ugraph, LazPaintType, BGRAGradientScanner, LCVectorRectShapes,
  BGRATransform, UImageDiff;

{ TToolGradient }

function TToolGradient.CreateShape: TVectorShape;
begin
  result := TRectShape.Create(nil);
  result.PenFill.Clear;
  result.BackFill.SetGradient(TBGRALayerGradientOriginal.Create,true);
  result.Usermode := vsuEditBackFill;
end;

procedure TToolGradient.DrawCustomShape(ADest: TBGRABitmap;
  AMatrix: TAffineMatrix; ADraft: boolean);
begin
  FShape.BackFill.Gradient.Render(ADest,AMatrix,ADraft,dmDrawWithTransparency);
end;

procedure TToolGradient.AssignShapeStyle(AMatrix: TAffineMatrix; AAlwaysFit: boolean);
begin
  if Manager.BackFill.FillType = vftGradient then
    FShape.BackFill.AssignExceptGeometry(Manager.BackFill);
end;

procedure TToolGradient.QuickDefineShape(AStart, AEnd: TPointF);
begin
  FShape.QuickDefine(PointF(-0.5,-0.5),PointF(Manager.Image.Width-0.5,Manager.Image.Height-0.5));
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

function TToolGradient.ReplaceLayerAndAddShape(out ARect: TRect): TCustomImageDifference;
var
  gradientOrig: TBGRALayerCustomOriginal;
begin
  if Manager.Image.CurrentLayerEmpty then
  begin
    gradientOrig := FShape.BackFill.Gradient.Duplicate;
    result := TReplaceLayerByCustomOriginalDifference.Create(Manager.Image.CurrentState,
      Manager.Image.CurrentLayerIndex, false, gradientOrig);
    ARect := rect(0,0,Manager.Image.Width,Manager.Image.Height);
  end else
    Result:= inherited ReplaceLayerAndAddShape(ARect);
end;

function TToolGradient.GetAllowedBackFillTypes: TVectorialFillTypes;
begin
  result := [vftGradient];
end;

function TToolGradient.GetContextualToolbars: TContextualToolbars;
begin
  Result:= [ctBackFill];
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
  b: TUniversalBrush;
begin
  if rightBtn then b := GetBackUniversalBrush
  else b := GetForeUniversalBrush;
  toolDest.FloodFill(pt.X, pt.Y, b, ffProgressive in Manager.FloodFillOptions, Manager.Tolerance*$101);
  ReleaseUniversalBrushes;
  Action.NotifyChange(toolDest, rect(0,0,toolDest.Width,toolDest.Height));
  ValidateAction;
  result := OnlyRenderChange;
end;

function TToolFloodFill.GetContextualToolbars: TContextualToolbars;
begin
  Result:= [ctFill,ctTolerance];
end;

initialization

  RegisterTool(ptFloodFill, TToolFloodFill);
  RegisterTool(ptGradient, TToolGradient);

end.

