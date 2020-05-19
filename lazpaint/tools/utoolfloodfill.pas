// SPDX-License-Identifier: GPL-3.0-only
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
    function ShapeClass: TVectorShapeAny; override;
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
  BGRATransform, UImageDiff, BGRAPen, UScripting, BGRABlend;

{ TToolGradient }

function TToolGradient.ShapeClass: TVectorShapeAny;
begin
  result := TRectShape;
end;

function TToolGradient.CreateShape: TVectorShape;
begin
  result := inherited CreateShape;
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
  if (FShape.BackFill.FillType = vftGradient) and
    (Manager.Image.CurrentLayerEmpty or FShape.BackFill.Gradient.IsOpaque) then
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
  params: TVariableSet;
  vectOrig: TVectorOriginal;
  i: Integer;
  ptOrig: TPointF;
  newColor: TBGRAPixel;
begin
  result := OnlyRenderChange;
  homogeneous := toolDest.Equals(toolDest.GetPixel(0,0));
  if Manager.Image.SelectionMaskEmpty then
  begin
    if homogeneous then
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
      exit;
    end else
    if GetCurrentLayerKind = lkVectorial then
    begin
      if rightBtn then f := Manager.BackFill else f := Manager.ForeFill;
      if f.FillType = vftSolid then
      begin
        vectOrig := TVectorOriginal(Manager.Image.LayerOriginal[Manager.Image.CurrentLayerIndex]);
        ptOrig := AffineMatrixInverse(Manager.Image.LayerOriginalMatrix[Manager.Image.CurrentLayerIndex]) * ptF;
        for i := vectOrig.ShapeCount-1 downto 0 do
        begin
          if (vsfPenFill in vectOrig.Shape[i].MultiFields) and
            vectOrig.Shape[i].PointInPen(ptOrig) then
          begin
            if not (vectOrig.Shape[i].PenFill.FillType = vftSolid) then break;
            CancelAction;
            Manager.Image.CurrentState.DiscardOriginalDiff:= false;
            try
              newColor := vectOrig.Shape[i].PenFill.SolidColor;
              DrawPixelsInline(@newColor, f.SolidColor, 1);
              vectOrig.Shape[i].PenFill.SetSolid(newColor);
            finally
              Manager.Image.CurrentState.DiscardOriginalDiff:= true;
            end;
            exit;
          end;
          if (vsfBackFill in vectOrig.Shape[i].MultiFields) and
            vectOrig.Shape[i].PointInBack(ptOrig) then
          begin
            if not (vectOrig.Shape[i].BackFill.FillType = vftSolid) then break;
            CancelAction;
            Manager.Image.CurrentState.DiscardOriginalDiff:= false;
            try
              newColor := vectOrig.Shape[i].BackFill.SolidColor;
              DrawPixelsInline(@newColor, f.SolidColor, 1);
              vectOrig.Shape[i].BackFill.SetSolid(newColor);
            finally
              Manager.Image.CurrentState.DiscardOriginalDiff:= true;
            end;
            exit;
          end;
        end;
        if (toolDest.GetPixel(pt.X,pt.Y).alpha = 0)
          and Assigned(Manager.Scripting) then
        begin
          CancelAction;
          params := TVariableSet.Create('ImageFillBackground');
          params.Pixels['BackColor'] := f.SolidColor;
          Manager.Scripting.CallScriptFunction(params);
          params.Free;
          exit;
        end;
      end;
    end;
  end;

  if rightBtn then b := GetBackUniversalBrush
  else b := GetForeUniversalBrush;
  if homogeneous then toolDest.Fill(b)
    else toolDest.FloodFill(pt.X, pt.Y, b,
                  ffProgressive in Manager.FloodFillOptions, Manager.Tolerance*$101);
  ReleaseUniversalBrushes;
  Action.NotifyChange(toolDest, rect(0,0,toolDest.Width,toolDest.Height));
  ValidateAction;
end;

function TToolFloodFill.GetContextualToolbars: TContextualToolbars;
begin
  Result:= [ctPenFill,ctBackFill,ctTolerance];
end;

initialization

  RegisterTool(ptFloodFill, TToolFloodFill);
  RegisterTool(ptGradient, TToolGradient);

end.

