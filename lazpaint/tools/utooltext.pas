// SPDX-License-Identifier: GPL-3.0-only
unit UToolText;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UTool, UToolVectorial, LCLType, Graphics, BGRABitmap, BGRABitmapTypes, BGRATextFX,
  BGRAGradients, LCVectorOriginal;

type

  { TToolText }

  TToolText = class(TVectorialTool)
  protected
    FMatrix: TAffineMatrix;
    FPrevShadow: boolean;
    FPrevShadowOffset: TPoint;
    FPrevShadowRadius: single;
    function ShapeClass: TVectorShapeAny; override;
    function AlwaysRasterizeShape: boolean; override;
    procedure IncludeShadowBounds(var ARect: TRect);
    function GetCustomShapeBounds(ADestBounds: TRect; AMatrix: TAffineMatrix; ADraft: boolean): TRect; override;
    procedure DrawCustomShape(ADest: TBGRABitmap; AMatrix: TAffineMatrix; ADraft: boolean); override;
    procedure ShapeChange(ASender: TObject; ABounds: TRectF; ADiff: TVectorShapeDiff); override;
    procedure ShapeEditingChange(ASender: TObject); override;
    procedure AssignShapeStyle(AMatrix: TAffineMatrix; AAlwaysFit: boolean); override;
    procedure QuickDefineEnd; override;
    function RoundCoordinate(constref ptF: TPointF): TPointF; override;
    function DoToolKeyDown(var key: Word): TRect; override;
  public
    constructor Create(AManager: TToolManager); override;
    function GetContextualToolbars: TContextualToolbars; override;
    function ToolCommand(ACommand: TToolCommand): boolean; override;
    function ToolProvideCommand(ACommand: TToolCommand): boolean; override;
  end;

implementation

uses LCVectorTextShapes, BGRALayerOriginal, BGRATransform, BGRAGrayscaleMask,
  ugraph, math;

{ TToolText }

function TToolText.ShapeClass: TVectorShapeAny;
begin
  result := TTextShape;
end;

function TToolText.AlwaysRasterizeShape: boolean;
begin
  Result:= Manager.TextShadow;
end;

procedure TToolText.IncludeShadowBounds(var ARect: TRect);
var
  shadowRect: TRect;
begin
  if Manager.TextShadow then
  begin
    shadowRect := ARect;
    shadowRect.Inflate(ceil(Manager.TextShadowBlurRadius),ceil(Manager.TextShadowBlurRadius));
    shadowRect.Offset(Manager.TextShadowOffset.X,Manager.TextShadowOffset.Y);
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
  if Manager.TextShadow then
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
        blurBounds.Inflate(ceil(Manager.TextShadowBlurRadius),ceil(Manager.TextShadowBlurRadius));
        blurBounds.Offset(Manager.TextShadowOffset.X,Manager.TextShadowOffset.Y);
        r := ADest.ClipRect;
        r.Inflate(ceil(Manager.TextShadowBlurRadius),ceil(Manager.TextShadowBlurRadius));
        blurBounds.Intersect(r);
        gray := TGrayscaleMask.Create(blurBounds.Width,blurBounds.Height);
        gray.PutImage(shapeBounds.Left-blurBounds.Left+Manager.TextShadowOffset.X,
                      shapeBounds.Top-blurBounds.Top+Manager.TextShadowOffset.Y,grayShape,dmSet);
        grayShape.Free;
        blur := gray.FilterBlurRadial(Manager.TextShadowBlurRadius,Manager.TextShadowBlurRadius, rbFast) as TGrayscaleMask;
        gray.Free;
        ADest.FillMask(blurBounds.Left,blurBounds.Top,blur,BGRABlack,dmDrawWithTransparency);
        blur.Free;
      end;
      ADest.PutImage(shapeBounds.Left,shapeBounds.Top,temp,dmDrawWithTransparency);
      temp.Free;
    end;
    FPrevShadow := true;
    FPrevShadowRadius := Manager.TextShadowBlurRadius;
    FPrevShadowOffset := Manager.TextShadowOffset;
  end else
  begin
    inherited DrawCustomShape(ADest, AMatrix, ADraft);
    FPrevShadow := false;
  end;
end;

procedure TToolText.ShapeChange(ASender: TObject; ABounds: TRectF; ADiff: TVectorShapeDiff);
var
  r: TRect;
  posF: TPointF;
begin
  posF := AffineMatrixInverse(FMatrix)*(FShape as TTextShape).LightPosition;
  Manager.LightPosition := posF;
  with ABounds do r := rect(floor(Left),floor(Top),ceil(Right),ceil(Bottom));
  IncludeShadowBounds(r);
  inherited ShapeChange(ASender, RectF(r.Left,r.Top,r.Right,r.Bottom), ADiff);
end;

procedure TToolText.ShapeEditingChange(ASender: TObject);
begin
  with (FShape as TTextShape) do
    Manager.TextAlign := ParagraphAlignment;
  inherited ShapeEditingChange(ASender);
end;

procedure TToolText.AssignShapeStyle(AMatrix: TAffineMatrix; AAlwaysFit: boolean);
var
  r: TRect;
  toolDest: TBGRABitmap;
  zoom: Single;
begin
  inherited AssignShapeStyle(AMatrix, AAlwaysFit);
  FMatrix := AMatrix;
  with TTextShape(FShape) do
  begin
    zoom := (VectLen(AMatrix[1,1],AMatrix[2,1])+VectLen(AMatrix[1,2],AMatrix[2,2]))/2;
    FontEmHeight:= zoom*Manager.TextFontSize*Manager.Image.DPI/72;
    FontName:= Manager.TextFontName;
    FontStyle:= Manager.TextFontStyle;
    Aliased := Manager.ShapeOptionAliasing;
    LightPosition := AMatrix*Manager.LightPosition;
    AltitudePercent:= Manager.PhongShapeAltitude;
    ParagraphAlignment:= Manager.TextAlign;
    PenPhong := Manager.TextPhong;
  end;
  if (Manager.TextShadow <> FPrevShadow) or
     (FPrevShadow and
     ((Manager.TextShadowBlurRadius <> FPrevShadowRadius) or
     (Manager.TextShadowOffset <> FPrevShadowOffset))) then
  begin
    toolDest := GetToolDrawingLayer;
    r:= UpdateShape(toolDest);
    Action.NotifyChange(toolDest, r);
  end;
end;

procedure TToolText.QuickDefineEnd;
begin
  FShape.Usermode := vsuEditText;
end;

function TToolText.RoundCoordinate(constref ptF: TPointF): TPointF;
begin
  result := PointF(floor(ptF.x)+0.5,floor(ptF.y)+0.5);
end;

constructor TToolText.Create(AManager: TToolManager);
begin
  inherited Create(AManager);
  FMatrix := AffineMatrixIdentity;
end;

function TToolText.GetContextualToolbars: TContextualToolbars;
begin
  Result:= [ctPenFill,ctText,ctOutlineFill,ctOutlineWidth,ctAliasing];
  if Manager.TextPhong then include(result, ctAltitude);
end;

function TToolText.DoToolKeyDown(var key: Word): TRect;
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
    if FShape.Usermode = vsuEditText then
      FShape.Usermode := vsuEdit
    else
      result := ValidateShape;
    Key := 0;
  end else
  if (Key = VK_RETURN) and Assigned(FShape) then
  begin
    handled := false;
    FShape.KeyDown(ShiftState, skReturn, handled);
    if not handled then ValidateShape;
    Key := 0;
  end else
    Result:=inherited DoToolKeyDown(key);
end;

function TToolText.ToolCommand(ACommand: TToolCommand): boolean;
begin
  if Assigned(FShape) and (FShape.Usermode = vsuEditText) then
    case ACommand of
    tcCopy: Result:= TTextShape(FShape).CopySelection;
    tcCut: Result:= TTextShape(FShape).CutSelection;
    tcPaste: Result:= TTextShape(FShape).PasteSelection;
    tcDelete: Result:= TTextShape(FShape).DeleteSelection;
    else
      result := inherited ToolCommand(ACommand);
    end
  else
    case ACommand of
    tcDelete:
      if Assigned(FShape) then
      begin
        CancelShape;
        result := true;
      end else result := false;
    else result := inherited ToolCommand(ACommand);
    end;
end;

function TToolText.ToolProvideCommand(ACommand: TToolCommand): boolean;
begin
  if Assigned(FShape) and (FShape.Usermode = vsuEditText) then
    case ACommand of
    tcCopy,tcCut,tcDelete: result := TTextShape(FShape).HasSelection;
    tcPaste: result := true;
    else
      result := inherited ToolProvideCommand(ACommand);
    end
  else
    case ACommand of
    tcDelete: result := Assigned(FShape);
    else result := inherited ToolProvideCommand(ACommand);
    end;
end;

initialization

    RegisterTool(ptText, TToolText);

end.

