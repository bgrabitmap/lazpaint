unit UToolText;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, utool, utoolbasic, LCLType, Graphics, BGRABitmap, BGRABitmapTypes, BGRATextFX,
  BGRAGradients, LCVectorOriginal;

type

  { TToolText }

  TToolText = class(TVectorialTool)
  protected
    FMatrix: TAffineMatrix;
    FPrevShadow: boolean;
    FPrevShadowOffset: TPoint;
    FPrevShadowRadius: single;
    function CreateShape: TVectorShape; override;
    function AlwaysRasterizeShape: boolean; override;
    procedure IncludeShadowBounds(var ARect: TRect);
    function GetCustomShapeBounds(ADestBounds: TRect; AMatrix: TAffineMatrix; ADraft: boolean): TRect; override;
    procedure DrawCustomShape(ADest: TBGRABitmap; AMatrix: TAffineMatrix; ADraft: boolean); override;
    procedure ShapeChange(ASender: TObject; ABounds: TRectF); override;
    procedure ShapeEditingChange(ASender: TObject); override;
    procedure AssignShapeStyle(AMatrix: TAffineMatrix); override;
    function SlowShape: boolean; override;
    procedure QuickDefineEnd; override;
    function RoundCoordinate(ptF: TPointF): TPointF; override;
  public
    constructor Create(AManager: TToolManager); override;
    function ToolKeyDown(var key: Word): TRect; override;
    function ToolCopy: boolean; override;
    function ToolCut: boolean; override;
    function ToolPaste: boolean; override;
    function ToolProvideCopy: boolean; override;
    function ToolProvideCut: boolean; override;
    function ToolProvidePaste: boolean; override;
  end;

implementation

uses LCVectorTextShapes, BGRALayerOriginal, BGRATransform, BGRAGrayscaleMask,
  ugraph, math;

{ TToolText }

function TToolText.CreateShape: TVectorShape;
begin
  result := TTextShape.Create(nil);
end;

function TToolText.AlwaysRasterizeShape: boolean;
begin
  Result:= Manager.ToolTextShadow;
end;

procedure TToolText.IncludeShadowBounds(var ARect: TRect);
var
  shadowRect: TRect;
begin
  if Manager.ToolTextShadow then
  begin
    shadowRect := ARect;
    shadowRect.Inflate(ceil(Manager.ToolTextBlur),ceil(Manager.ToolTextBlur));
    shadowRect.Offset(Manager.ToolTextShadowOffset.X,Manager.ToolTextShadowOffset.Y);
    ARect := RectUnion(ARect, shadowRect);
  end;
end;

function TToolText.GetCustomShapeBounds(ADestBounds: TRect; AMatrix: TAffineMatrix; ADraft: boolean): TRect;
begin
  Result:= inherited GetCustomShapeBounds(ADestBounds, AMatrix, ADraft);
  IncludeShadowBounds(result);
  result.Intersect(ADestBounds);
end;

procedure TToolText.DrawCustomShape(ADest: TBGRABitmap; AMatrix: TAffineMatrix; ADraft: boolean);
var
  temp: TBGRABitmap;
  blur, gray, grayShape: TGrayscaleMask;
  shapeBounds, blurBounds, r, actualShapeBounds: TRect;
begin
  if Manager.ToolTextShadow then
  begin
    shapeBounds := GetCustomShapeBounds(rect(0,0,ADest.Width,ADest.Height),AMatrix,ADraft);
    shapeBounds.Intersect(ADest.ClipRect);
    if (shapeBounds.Width > 0) and (shapeBounds.Height > 0) then
    begin
      temp := TBGRABitmap.Create(shapeBounds.Width,shapeBounds.Height);
      inherited DrawCustomShape(temp, AffineMatrixTranslation(-shapeBounds.Left,-shapeBounds.Top)*AMatrix, ADraft);
      actualShapeBounds := temp.GetImageBounds;
      if not actualShapeBounds.IsEmpty then
      begin
        actualShapeBounds.Offset(shapeBounds.Left,shapeBounds.Top);
        grayShape := TGrayscaleMask.Create;
        grayShape.CopyFrom(temp, cAlpha);

        blurBounds := actualShapeBounds;
        blurBounds.Inflate(ceil(Manager.ToolTextBlur),ceil(Manager.ToolTextBlur));
        blurBounds.Offset(Manager.ToolTextShadowOffset.X,Manager.ToolTextShadowOffset.Y);
        r := ADest.ClipRect;
        r.Inflate(ceil(Manager.ToolTextBlur),ceil(Manager.ToolTextBlur));
        blurBounds.Intersect(r);
        gray := TGrayscaleMask.Create(blurBounds.Width,blurBounds.Height);
        gray.PutImage(shapeBounds.Left-blurBounds.Left+Manager.ToolTextShadowOffset.X,
                      shapeBounds.Top-blurBounds.Top+Manager.ToolTextShadowOffset.Y,grayShape,dmSet);
        grayShape.Free;
        blur := gray.FilterBlurRadial(Manager.ToolTextBlur,Manager.ToolTextBlur, rbFast) as TGrayscaleMask;
        gray.Free;
        ADest.FillMask(blurBounds.Left,blurBounds.Top,blur,BGRABlack,dmDrawWithTransparency);
        blur.Free;
      end;
      ADest.PutImage(shapeBounds.Left,shapeBounds.Top,temp,dmDrawWithTransparency);
      temp.Free;
    end;
    FPrevShadow := true;
    FPrevShadowRadius := Manager.ToolTextBlur;
    FPrevShadowOffset := Manager.ToolTextShadowOffset;
  end else
  begin
    inherited DrawCustomShape(ADest, AMatrix, ADraft);
    FPrevShadow := false;
  end;
end;

procedure TToolText.ShapeChange(ASender: TObject; ABounds: TRectF);
var
  r: TRect;
  posF: TPointF;
begin
  posF := AffineMatrixInverse(FMatrix)*(FShape as TTextShape).LightPosition;
  Manager.ToolLightPosition := posF.Round;
  with ABounds do r := rect(floor(Left),floor(Top),ceil(Right),ceil(Bottom));
  IncludeShadowBounds(r);
  inherited ShapeChange(ASender, RectF(r.Left,r.Top,r.Right,r.Bottom));
end;

procedure TToolText.ShapeEditingChange(ASender: TObject);
begin
  with (FShape as TTextShape) do
    Manager.ToolTextAlign := ParagraphAlignment;
  inherited ShapeEditingChange(ASender);
end;

procedure TToolText.AssignShapeStyle(AMatrix: TAffineMatrix);
var
  r: TRect;
  toolDest: TBGRABitmap;
  zoom: Single;
begin
  FMatrix := AMatrix;
  with TTextShape(FShape) do
  begin
    zoom := (VectLen(AMatrix[1,1],AMatrix[2,1])+VectLen(AMatrix[1,2],AMatrix[2,2]))/2;
    FontEmHeight:= zoom*Manager.ToolTextFont.Size*ScreenInfo.PixelsPerInchY/72;
    FontName:= Manager.ToolTextFont.Name;
    FontStyle:= Manager.ToolTextFont.Style;

    if Manager.GetToolTexture <> nil then
      FShape.PenFill.SetTexture(Manager.GetToolTexture,AffineMatrixIdentity,Manager.ToolTextureOpacity)
    else
    begin
      if FSwapColor then
        FShape.PenFill.SetSolid(Manager.ToolBackColor)
      else
        FShape.PenFill.SetSolid(Manager.ToolForeColor);
    end;

    if Manager.ToolTextOutline and (Manager.ToolTextOutlineWidth>0) and
       (Manager.ToolBackColor.alpha > 0) then
    begin
      if FSwapColor then
        FShape.OutlineFill.SetSolid(Manager.ToolForeColor)
      else
        FShape.OutlineFill.SetSolid(Manager.ToolBackColor);
      OutlineWidth := Manager.ToolTextOutlineWidth;
    end
    else
      OutlineFill.Clear;

    LightPosition := AMatrix*PointF(Manager.ToolLightPosition.X,Manager.ToolLightPosition.Y);
    AltitudePercent:= Manager.ToolShapeAltitude;
    ParagraphAlignment:= Manager.ToolTextAlign;
    PenPhong := Manager.ToolTextPhong;
  end;
  if (Manager.ToolTextShadow <> FPrevShadow) or
     (Manager.ToolTextBlur <> FPrevShadowRadius) or
     (Manager.ToolTextShadowOffset <> FPrevShadowOffset) then
  begin
    toolDest := GetToolDrawingLayer;
    r:= UpdateShape(toolDest);
    Action.NotifyChange(toolDest, r);
  end;
end;

function TToolText.SlowShape: boolean;
begin
  Result:= true;
end;

procedure TToolText.QuickDefineEnd;
begin
  FShape.Usermode := vsuEditText;
end;

function TToolText.RoundCoordinate(ptF: TPointF): TPointF;
begin
  result := PointF(floor(ptF.x)+0.5,floor(ptF.y)+0.5);
end;

constructor TToolText.Create(AManager: TToolManager);
begin
  inherited Create(AManager);
  FMatrix := AffineMatrixIdentity;
end;

function TToolText.ToolKeyDown(var key: Word): TRect;
var
  keyUtf8: TUTF8Char;
  handled: Boolean;
begin
  if Key = VK_SPACE then
  begin
    keyUtf8:= ' ';
    result := ToolKeyPress(keyUtf8);
    Key := 0;
  end else
  if (Key = VK_ESCAPE) and Assigned(FShape) then
  begin
    result := ValidateShape;
    Key := 0;
  end else
  if (Key = VK_RETURN) and Assigned(FShape) then
  begin
    handled := false;
    FShape.KeyDown(FShiftState, skReturn, handled);
    if handled then Key := 0;
  end else
    Result:=inherited ToolKeyDown(key);
end;

function TToolText.ToolCopy: boolean;
begin
  Result:= Assigned(FShape) and TTextShape(FShape).CopySelection;
end;

function TToolText.ToolCut: boolean;
begin
  Result:= Assigned(FShape) and TTextShape(FShape).CutSelection;
end;

function TToolText.ToolPaste: boolean;
begin
  Result:= Assigned(FShape) and TTextShape(FShape).PasteSelection;
end;

function TToolText.ToolProvideCopy: boolean;
begin
  Result:= Assigned(FShape) and TTextShape(FShape).HasSelection;
end;

function TToolText.ToolProvideCut: boolean;
begin
  Result:= Assigned(FShape) and TTextShape(FShape).HasSelection;
end;

function TToolText.ToolProvidePaste: boolean;
begin
  Result:= Assigned(FShape);
end;

initialization

    RegisterTool(ptText, TToolText);

end.

