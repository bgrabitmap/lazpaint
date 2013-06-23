unit utooltext;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, utool, utoolbasic, LCLType, Graphics, BGRABitmap, BGRABitmapTypes, BGRATextFX,
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
    FBackup: TBGRABitmap;
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

uses types, ugraph, uresourcestrings;

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
var h: integer;
  currentRect: TRect;
begin
  ToolUpdateNeeded := false;
  if not FEditing then
  begin
    result := EmptyRect;
    exit;
  end;
  if FBackup = nil then
  begin
    Manager.Image.SaveLayerOrSelectionUndo;
    FBackup := toolDest.Duplicate as TBGRABitmap;
  end else
    toolDest.PutImage(0,0,FBackup,dmSet);

  UpdateTextFX;
  currentRect := RectOfs(TextFX.Bounds,FTextPos.X,FTextPos.Y);
  if Manager.ToolTextShadow then
  begin
    TextFX.DrawShadow(toolDest,FTextPos.X+Manager.ToolTextShadowOffset.X,FTextPos.Y+Manager.ToolTextShadowOffset.Y,Manager.ToolTextBlur,BGRABlack);
    currentRect := RectUnion(currentRect, RectOfs(TextFX.ShadowBounds[Manager.ToolTextBlur],FTextPos.X+Manager.ToolTextShadowOffset.X,FTextPos.Y+Manager.ToolTextShadowOffset.Y) );
  end;
  if Manager.ToolTextOutline then
    TextFX.DrawOutline(toolDest,FTextPos.X,FTextPos.Y,Manager.ToolBackColor);

  shader.LightPosition := Manager.ToolLightPosition;
  shader.LightPositionZ := Manager.ToolLightAltitude;

  if Manager.ToolTextPhong then
  begin
    h := round(Manager.ToolShapeAltitude/100 * TextFX.Height*0.10);
    if Manager.ToolTexture = nil then
      currentRect := RectUnion(currentRect, TextFX.DrawShaded(toolDest,FTextPos.X,FTextPos.Y,shader,h,Manager.ToolForeColor) )
    else
      currentRect := RectUnion(currentRect, TextFX.DrawShaded(toolDest,FTextPos.X,FTextPos.Y,shader,h,Manager.ToolTexture));
  end else
  begin
    if Manager.ToolTexture = nil then
      TextFX.Draw(toolDest,FTextPos.X,FTextPos.Y,Manager.ToolForeColor)
    else
      TextFX.Draw(toolDest,FTextPos.X,FTextPos.Y,Manager.ToolTexture);
  end;

  result := RectUnion(currentRect,previousRect);
  previousRect := currentRect;
  if IsRectEmpty(result) then result := ToolRepaintOnly; //no text but update carret
end;

procedure TToolText.UpdateTextFX;
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
    FreeAndNil(FBackup);
    Manager.Image.SaveLayerOrSelectionUndo;
  end;
end;

function TToolText.ToolKeyDown(key: Word): TRect;
begin
  result := EmptyRect;
  if key = VK_ESCAPE then
  begin
    if FEditing then
    begin
      if FBackup <> nil then
      begin
        GetToolDrawingLayer.PutImage(0,0,FBackup,dmSet);
        FreeAndNil(FBackup);
      end;
      FEditing := false;
      result := PreviousRect;
      PreviousRect := EmptyRect;
    end;
  end else
  if key = VK_RETURN then
  begin
    result := EndEdit;
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
begin
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
begin
  if FEditing then
  begin
    origin := BitmapToVirtualScreen(PointF(FTextPos.X,FTextPos.Y));
    if TextFX <> nil then
    begin
      caretTop := BitmapToVirtualScreen(PointF(FTextPos.X+TextFX.Width,FTextPos.Y));
      caretBottom := BitmapToVirtualScreen(PointF(FTextPos.X+TextFX.Width,FTextPos.Y+TextFX.Height));
      NicePoint(VirtualScreen,origin);
      NiceLine(VirtualScreen,caretTop.X,caretTop.Y,caretBottom.x,caretBottom.y);
    end;
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
  FBackup := nil;
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
  FBackup.Free;
  TextFX.Free;
  TextFX_Font.Free;
  shader.Free;
  inherited Destroy;
end;

initialization

    RegisterTool(ptText, TToolText);

end.

