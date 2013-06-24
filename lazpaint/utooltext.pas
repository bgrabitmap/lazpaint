unit UToolText;

{$mode objfpc}{$H+}

interface

uses
  Types, Classes, SysUtils, utool, utoolbasic, LCLType, Graphics, BGRABitmap, BGRABitmapTypes, BGRATextFX,
  BGRAGradients;

type

  { TToolText }

  TToolText = class(TGenericTool)
  protected
    FText: string;
    FTextPos: TPoint;
    FAntialias: boolean;
    FMovingText: boolean;
    FEditing: boolean;
    TextRendererFX: TBGRATextEffectFontRenderer;
    TextFX: TBGRATextEffect;
    TextFX_Text: string;
    TextFX_Font: TFont;
    TextFX_Antialias: boolean;
    shader: TPhongShading;
    prevToolTextPhong: boolean;
    previousRect: TRect;
    function GetIsSelectingTool: boolean; override;
    function DoToolDown({%H-}toolDest: TBGRABitmap; pt: TPoint; {%H-}ptF: TPointF;
      rightBtn: boolean): TRect; override;
    function DoToolMove({%H-}toolDest: TBGRABitmap; pt: TPoint; {%H-}ptF: TPointF): TRect;
      override;
    function EndEdit: TRect;
    function DoToolUpdate(toolDest: TBGRABitmap): TRect; override;
    function TextSize: TSize;
    procedure UpdateTextFX;
  public
    function ToolKeyDown(key: Word): TRect; override;
    function ToolKeyPress(key: TUTF8Char): TRect; override;
    function ToolUp: TRect; override;
    procedure Render(VirtualScreen: TBGRABitmap; BitmapToVirtualScreen: TBitmapToVirtualScreenFunction); override;
    constructor Create(AManager: TToolManager); override;
    destructor Destroy; override;
  end;

implementation

uses ugraph, uresourcestrings, BGRAText;

{ TToolText }

function TToolText.GetIsSelectingTool: boolean;
begin
  Result:=false;
end;

function TToolText.DoToolDown(toolDest: TBGRABitmap; pt: TPoint; ptF: TPointF;
  rightBtn: boolean): TRect;
begin
  if rightBtn then
  begin
    if Manager.ToolTextPhong then
    begin
      Manager.ToolLightPosition := pt;
      result := ToolUpdate;
    end else
    if FEditing then
    begin
      result := EndEdit;
    end else
      result := EmptyRect;
    if IsRectEmpty(result) then
      result := ToolRepaintOnly;
    exit;
  end;
  if not FEditing then
  begin
    FEditing := true;
    FMovingText := true;
    FTextPos := pt;
    FText := '';
    result := ToolUpdate;
  end else
  begin
    if (pt.X <> FTextPos.x) or (pt.y <> FTextPos.y) then
    begin
      FTextPos := pt;
      result := ToolUpdate;
    end
    else
      result := EmptyRect;
    FMovingText := true;
  end;
end;

function TToolText.DoToolMove(toolDest: TBGRABitmap; pt: TPoint; ptF: TPointF
  ): TRect;
begin
  result := EmptyRect;
  if FMovingText then
  begin
    if (pt.X <> FTextPos.x) or (pt.y <> FTextPos.y) then
    begin
      FTextPos := pt;
      result := ToolUpdate;
    end;
  end;
end;

function TToolText.DoToolUpdate(toolDest: TBGRABitmap): TRect;
var hPhong: integer;
  currentRect: TRect;
begin
  ToolUpdateNeeded := false;
  if not FEditing then
  begin
    result := EmptyRect;
    if Manager.ToolTextPhong <> prevToolTextPhong then
      result := ToolRepaintOnly;
    exit;
  end;
  RestoreBackupDrawingLayer;

  UpdateTextFX;
  if TextFX <> nil then
  begin
    hPhong := round(Manager.ToolShapeAltitude/100 * TextFX.TextSize.cy*0.10);
    currentRect := EmptyRect;
    if Manager.ToolTextShadow then
      currentRect := RectUnion(currentRect, TextFX.DrawShadow(toolDest,FTextPos.X+Manager.ToolTextShadowOffset.X,FTextPos.Y+Manager.ToolTextShadowOffset.Y,Manager.ToolTextBlur,BGRABlack,Manager.ToolTextAlign) );
    if Manager.ToolTextOutline then
      currentRect := RectUnion(currentRect, TextFX.DrawOutline(toolDest,FTextPos.X,FTextPos.Y,Manager.ToolBackColor,Manager.ToolTextAlign));
    if Manager.ToolTextPhong then
    begin
      if Manager.GetToolTextureAfterAlpha = nil then
        currentRect := RectUnion(currentRect, TextFX.DrawShaded(toolDest,FTextPos.X,FTextPos.Y,shader,hPhong,Manager.ToolForeColor,Manager.ToolTextAlign) )
      else
        currentRect := RectUnion(currentRect, TextFX.DrawShaded(toolDest,FTextPos.X,FTextPos.Y,shader,hPhong,Manager.GetToolTextureAfterAlpha,Manager.ToolTextAlign));
    end else
    begin
      if Manager.GetToolTextureAfterAlpha = nil then
        currentRect := RectUnion(currentRect, TextFX.Draw(toolDest,FTextPos.X,FTextPos.Y,Manager.ToolForeColor,Manager.ToolTextAlign) )
      else
        currentRect := RectUnion(currentRect, TextFX.Draw(toolDest,FTextPos.X,FTextPos.Y,Manager.GetToolTextureAfterAlpha,Manager.ToolTextAlign) );
    end;
  end else
  if TextRendererFX <> nil then
  begin
    if Manager.GetToolTexture = nil then
      TextRendererFX.TextOut(toolDest,FTextPos.X,FTextPos.Y,FText,Manager.ToolForeColor,Manager.ToolTextAlign)
    else
      TextRendererFX.TextOut(toolDest,FTextPos.X,FTextPos.Y,FText,Manager.GetToolTextureAfterAlpha,Manager.ToolTextAlign);
    currentRect := rect(0,0,toolDest.Width,toolDest.Height);
  end else
    currentRect := EmptyRect;
  result := RectUnion(currentRect,previousRect);
  previousRect := currentRect;
  if IsRectEmpty(result) then result := ToolRepaintOnly; //no text but update carret
end;

function TToolText.TextSize: TSize;
begin
  if TextFX <> nil then
    result := TextFX.TextSize
  else
  if TextRendererFX <> nil then
  begin
    result := TextRendererFX.TextSize(FText);
    if result.cy = 0 then result.cy := TextRendererFX.TextSize('Hg').cy;
  end
  else
    result := Size(0,0);
end;

procedure TToolText.UpdateTextFX;
var isVectorOutline: boolean;
begin
  isVectorOutline:= (Manager.ToolTextOutlineWidth <> TBGRATextEffect.OutlineWidth) and Manager.ToolTextOutline;
  if not isVectorOutline or Manager.ToolTextPhong then
  begin
    if (TextFX = nil) or (FText <> TextFX_Text) or (TextFX_Font = nil) or not TextFX_Font.IsEqual(Manager.ToolTextFont)
     or (TextFX_Antialias <> FAntialias) then
    begin
      FreeAndNil(TextFX);
      TextFX_Text := FText;
      TextFX_Antialias:= FAntialias;
      TextFX_Font.Assign(Manager.ToolTextFont);
      TextFX := TBGRATextEffect.Create(TextFX_Text,TextFX_Font,TextFX_Antialias);
    end;
  end else
  begin
    FreeAndNil(TextFX);
    if TextRendererFX = nil then TextRendererFX := TBGRATextEffectFontRenderer.Create(shader,false);
    TextRendererFX.FontQuality := fqFineAntialiasing;
    TextRendererFX.FontEmHeight := Manager.ToolTextFont.Height * FontEmHeightSign;
    TextRendererFX.FontName := manager.ToolTextFont.Name;
    TextRendererFX.FontOrientation := 0;
    TextRendererFX.FontStyle := manager.ToolTextFont.Style;
    TextRendererFX.OutlineWidth := Manager.ToolTextOutlineWidth;
    TextRendererFX.OuterOutlineOnly := True;
    TextRendererFX.OutlineVisible := manager.ToolTextOutline;
    TextRendererFX.OutlineColor := Manager.ToolBackColor;
    TextRendererFX.ShaderActive := Manager.ToolTextPhong;
    TextRendererFX.ShadowVisible := Manager.ToolTextShadow;
    TextRendererFX.ShadowOffset := Manager.ToolTextShadowOffset;
    TextRendererFX.ShadowRadius := Manager.ToolTextBlur;
  end;
  shader.LightPosition := Manager.ToolLightPosition;
  shader.LightPositionZ := Manager.ToolLightAltitude;
end;

function TToolText.EndEdit: TRect;
begin
  result := EmptyRect;
  if FEditing then
  begin
    FEditing := false;
    if ToolUpdateNeeded then
      result := ToolUpdate
    else
      result := ToolRepaintOnly; //hide carret
    if FText <> '' then
      ValidateAction
    else
      CancelAction;
  end;
end;

function TToolText.ToolKeyDown(key: Word): TRect;
begin
  result := EmptyRect;
  if key = VK_ESCAPE then
  begin
    if FEditing then
    begin
      CancelAction;
      FEditing := false;
      result := PreviousRect;
      if IsRectEmpty(result) then result := ToolRepaintOnly;
      PreviousRect := EmptyRect;
    end;
  end else
  if Key = VK_SPACE then
  begin
    result := ToolKeyPress(' ');
  end else
  if Key = VK_BACK then
  begin
    if FEditing then
    begin
      if FText <> '' then
      begin
        delete(FText,length(FText),1);
        while (length(FText)> 0) and (byte(FText[length(FText)]) and $80 <> 0) do
          delete(FText,length(FText),1);
        ToolUpdateNeeded := True;
      end;
      result := ToolRepaintOnly; //later
    end;
  end;
end;

function TToolText.ToolKeyPress(key: TUTF8Char): TRect;
var ty,maxy: integer;
begin
  if (key = #13) and FEditing then
  begin
    ty := TextSize.cy;
    maxy := GetToolDrawingLayer.Height;
    result := EndEdit;
    FTextPos.Y := FTextPos.Y+ty;
    if FTextPos.Y < maxy then
    begin
      FEditing := true;
      FMovingText := false;
      FText := '';
      UpdateTextFX;
    end;
  end else
  if FEditing then
  begin
    FText += key;
    ToolUpdateNeeded := true;
    result := ToolRepaintOnly; //later
  end else
    result := EmptyRect;
end;

function TToolText.ToolUp: TRect;
begin
  if FMovingText then
    FMovingText := false;
  Result:= EmptyRect;
end;

procedure TToolText.Render(VirtualScreen: TBGRABitmap;
  BitmapToVirtualScreen: TBitmapToVirtualScreenFunction);
var
  origin: TPointF;
  caretTop,caretBottom: TPointF;
  lightPosition: TPointF;
  textWidth: integer;
begin
  if FEditing then
  begin
    origin := BitmapToVirtualScreen(PointF(FTextPos.X,FTextPos.Y));
    NicePoint(VirtualScreen,origin);
    with TextSize do
    begin
      textWidth := cx;
      caretTop := PointF(FTextPos.X,FTextPos.Y);
      caretBottom := PointF(caretTop.X,caretTop.Y+cy);
    end;
    case Manager.ToolTextAlign of
    taLeftJustify: begin
                     caretTop.X += textWidth;
                     caretBottom.X += textWidth;
                   end;
    taCenter: begin
                caretTop.X += textWidth div 2;
                caretBottom.X += textWidth div 2;
              end;
    end;
    caretTop := BitmapToVirtualScreen(caretTop);
    caretBottom := BitmapToVirtualScreen(caretBottom);
    NiceLine(VirtualScreen,caretTop.X,caretTop.Y,caretBottom.x,caretBottom.y);
  end;
  if Manager.ToolTextPhong then
  begin
    lightPosition := BitmapToVirtualScreen(PointF(Manager.ToolLightPosition.X,Manager.ToolLightPosition.Y));
    NicePoint(VirtualScreen, lightPosition.X,lightPosition.Y);
    if lightPosition.Y > virtualScreen.Height/2 then
      NiceText(VirtualScreen, round(lightPosition.X),round(lightPosition.Y-6), rsLightPosition, taCenter, tlBottom)
    else
      NiceText(VirtualScreen, round(lightPosition.X),round(lightPosition.Y+6), rsLightPosition, taCenter, tlTop);
  end;
  prevToolTextPhong := Manager.ToolTextPhong;
end;

constructor TToolText.Create(AManager: TToolManager);
begin
  inherited Create(AManager);
  FAntialias := true;
  TextFX_Font := TFont.Create;

  shader := TPhongShading.Create;
  shader.AmbientFactor := 0.6;
  shader.NegativeDiffusionFactor := 0.15;
  previousRect := EmptyRect;
end;

destructor TToolText.Destroy;
begin
  EndEdit;
  TextFX.Free;
  TextFX_Font.Free;
  TextRendererFX.Free;
  shader.Free;
  inherited Destroy;
end;

initialization

    RegisterTool(ptText, TToolText);

end.

