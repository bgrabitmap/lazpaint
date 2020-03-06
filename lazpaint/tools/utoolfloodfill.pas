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
  BGRATransform, UImageDiff, BGRAPen;

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
    result := GetGradientStatusText(FShape.BackFill.Gradient, VectorTransform(True))
    else Result:= inherited GetStatusText;
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
  f: TVectorialFill;
  orig: TBGRALayerCustomOriginal;
  diff: TCustomImageDifference;
  rectShape: TRectShape;
  homogeneous: Boolean;
begin
  homogeneous := toolDest.Equals(toolDest.GetPixel(0,0));
  if Manager.Image.SelectionMaskEmpty and homogeneous then
  begin
    CancelAction;
    if rightBtn then f := Manager.BackFill.Duplicate
    else f := Manager.ForeFill.Duplicate;
    try
      f.ApplyOpacity(Manager.GetPressureB);
      f.FitGeometry(SuggestGradientBox);
      case f.FillType of
      vftGradient: orig := f.Gradient.Duplicate;
      else
        begin
          orig := TVectorOriginal.Create;
          rectShape := TRectShape.Create(nil);
          rectShape.QuickDefine(PointF(-0.5,-0.5), PointF(Manager.Image.Width-0.5,Manager.Image.Height-0.5));
          rectShape.PenStyle := ClearPenStyle;
          rectShape.BackFill.Assign(f);
          TVectorOriginal(orig).AddShape(rectShape);
        end;
      end;
      diff := TReplaceLayerByCustomOriginalDifference.Create(Manager.Image.CurrentState,
                    Manager.Image.CurrentLayerIndex, false, orig);
      Manager.Image.AddUndo(diff);
      Manager.Image.ImageMayChangeCompletely;
    finally
      f.Free;
    end;
  end else
  begin
    if rightBtn then b := GetBackUniversalBrush
    else b := GetForeUniversalBrush;
    if homogeneous then toolDest.Fill(b)
      else toolDest.FloodFill(pt.X, pt.Y, b,
                    ffProgressive in Manager.FloodFillOptions, Manager.Tolerance*$101);
    ReleaseUniversalBrushes;
    Action.NotifyChange(toolDest, rect(0,0,toolDest.Width,toolDest.Height));
    ValidateAction;
  end;
  result := OnlyRenderChange;
end;

function TToolFloodFill.GetContextualToolbars: TContextualToolbars;
begin
  Result:= [ctPenFill,ctBackFill,ctTolerance];
end;

initialization

  RegisterTool(ptFloodFill, TToolFloodFill);
  RegisterTool(ptGradient, TToolGradient);

end.

