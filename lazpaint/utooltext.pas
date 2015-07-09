unit UToolText;

{$mode objfpc}{$H+}

interface

uses
  Types, Classes, SysUtils, utool, utoolbasic, LCLType, Graphics, BGRABitmap, BGRABitmapTypes, BGRATextFX,
  BGRAGradients, ULayerAction;

type

  { TToolText }

  TToolText = class(TGenericTool)
  protected
    FText: string;
    FShiftKey, FCtrlKey: boolean;
    FTextPos: TPoint;
    FTextCharPos, FTextSelStart: integer;
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
    function TextSizeOf(s: string): TSize;
    procedure UpdateTextFX;
    function GetAction: TLayerAction; override;
    function DeleteSelection: boolean;
    procedure SelectionFollows;
    procedure GetCaretPos(AIndex:integer; out caretTop,caretBottom:TPointF);
  public
    function ToolKeyDown(var key: Word): TRect; override;
    function ToolKeyUp(var key: Word): TRect; override;
    function ToolKeyPress(var key: TUTF8Char): TRect; override;
    function ToolUp: TRect; override;
    function ToolCopy: boolean; override;
    function ToolCut: boolean; override;
    function ToolPaste: boolean; override;
    function ToolProvideCopy: boolean; override;
    function ToolProvideCut: boolean; override;
    function ToolProvidePaste: boolean; override;
    function Render(VirtualScreen: TBGRABitmap; VirtualScreenWidth, VirtualScreenHeight: integer; BitmapToVirtualScreen: TBitmapToVirtualScreenFunction):TRect; override;
    constructor Create(AManager: TToolManager); override;
    destructor Destroy; override;
  end;

implementation

uses ugraph, uresourcestrings, BGRAText, LazPaintType, Math, LCLProc, Clipbrd;

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
      result := OnlyRenderChange;
    exit;
  end;
  if not FEditing then
  begin
    FEditing := true;
    FMovingText := true;
    FTextPos := pt;
    FText := '';
    FTextCharPos:= 1;
    FTextSelStart:= 1;
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
  oldClip: TRect;
begin
  ToolUpdateNeeded := false;
  if not FEditing then
  begin
    if Manager.ToolTextPhong <> prevToolTextPhong then
      result := OnlyRenderChange else
      result := EmptyRect;
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
    with TextRendererFX.TextSize(FText) do
      currentRect := rect(0,0,cx,cy);
    if not IsRectEmpty(currentRect) then
    begin
      OffsetRect(currentRect,FTextPos.X,FTextPos.Y);
      currentRect.Left -= (currentRect.Bottom-currentRect.top) div 2; //text advance
      currentRect.Right += (currentRect.Bottom-currentRect.top) div 2; //text advance
      InflateRect(currentRect, (Manager.ToolTextOutlineWidth+1) div 2, (Manager.ToolTextOutlineWidth+1) div 2);

      oldClip := toolDest.ClipRect;
      toolDest.ClipRect := currentRect;
      if Manager.GetToolTexture = nil then
        TextRendererFX.TextOut(toolDest,FTextPos.X,FTextPos.Y,FText,Manager.ToolForeColor,Manager.ToolTextAlign)
      else
        TextRendererFX.TextOut(toolDest,FTextPos.X,FTextPos.Y,FText,Manager.GetToolTextureAfterAlpha,Manager.ToolTextAlign);
      toolDest.ClipRect := oldClip;
    end;
  end else
    currentRect := EmptyRect;

  Action.NotifyChange(toolDest,currentRect);
  result := RectUnion(currentRect,previousRect);
  previousRect := currentRect;
  if IsRectEmpty(result) then result := OnlyRenderChange; //no text but update caret
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

function TToolText.TextSizeOf(s: string): TSize;
begin
  if s = FText then result := TextSize else
  begin
    if TextRendererFX <> nil then
    begin
      result := TextRendererFX.TextSize(s);
      if result.cy = 0 then result.cy := TextRendererFX.TextSize('Hg').cy;
    end
    else
      result := Size(0,0);
  end;
end;

procedure TToolText.UpdateTextFX;
var isVectorOutline, allowShader: boolean;
begin
  isVectorOutline:= (Manager.ToolTextOutlineWidth <> TBGRATextEffect.OutlineWidth) and Manager.ToolTextOutline;
  allowShader := Manager.ToolTextPhong and (Manager.ToolTextFont.Size < 300);
  if (not isVectorOutline and (Manager.ToolTextFont.Size < 120)) or allowShader then
  begin
    if (TextFX = nil) or (FText <> TextFX_Text) or (TextFX_Font = nil) or not TextFX_Font.IsEqual(Manager.ToolTextFont)
     or (TextFX_Antialias <> FAntialias) then
    begin
      FreeAndNil(TextFX);
      TextFX_Text := FText;
      TextFX_Antialias:= FAntialias;
      TextFX_Font.Assign(Manager.ToolTextFont);
      TextFX := TBGRATextEffect.Create(TextFX_Text,TextFX_Font,TextFX_Antialias);
      TextFX.ShadowQuality := rbBox;
    end;
  end else
  begin
    FreeAndNil(TextFX);
  end;
  if TextRendererFX = nil then TextRendererFX := TBGRATextEffectFontRenderer.Create(shader,false);
  {$IFDEF LINUX}
  TextRendererFX.FontQuality := fqSystem;
  {$ELSE}
  TextRendererFX.FontQuality := fqFineAntialiasing;
  {$ENDIF}
  TextRendererFX.ShadowQuality := rbBox;
  TextRendererFX.VectorizedFontRenderer.MaxFontResolution := 1000;
  TextRendererFX.FontEmHeight := Manager.ToolTextFont.Height * FontEmHeightSign;
  TextRendererFX.FontName := manager.ToolTextFont.Name;
  TextRendererFX.FontOrientation := 0;
  TextRendererFX.FontStyle := manager.ToolTextFont.Style;
  TextRendererFX.OutlineWidth := Manager.ToolTextOutlineWidth;
  TextRendererFX.OuterOutlineOnly := True;
  TextRendererFX.OutlineVisible := manager.ToolTextOutline;
  TextRendererFX.OutlineColor := Manager.ToolBackColor;
  TextRendererFX.ShaderActive := allowShader;
  TextRendererFX.ShadowVisible := Manager.ToolTextShadow;
  TextRendererFX.ShadowOffset := Manager.ToolTextShadowOffset;
  TextRendererFX.ShadowRadius := Manager.ToolTextBlur;

  shader.LightPosition := Manager.ToolLightPosition;
  shader.LightPositionZ := Manager.ToolLightAltitude;
end;

function TToolText.GetAction: TLayerAction;
begin
  Result:=inherited GetAction;
  result.AllChangesNotified:= true;
end;

function TToolText.DeleteSelection: boolean;
begin
  if FTextCharPos <> FTextSelStart then
  begin
    UTF8Delete(FText, min(FTextCharPos,FTextSelStart), abs(FTextSelStart-FTextCharPos));
    FTextCharPos := min(FTextCharPos,FTextSelStart);
    FTextSelStart:= FTextCharPos;
    result := true;
  end else
    result := false;
end;

procedure TToolText.SelectionFollows;
begin
  if not FShiftKey then FTextSelStart := FTextCharPos;
end;

procedure TToolText.GetCaretPos(AIndex:integer; out caretTop, caretBottom: TPointF);
var textwidth : integer;
begin
  textWidth := TextSize.cx;
  with TextSizeOf(UTF8Copy(FText,1,AIndex-1)) do
  begin
    caretTop := PointF(FTextPos.X+cx,FTextPos.Y);
    caretBottom := PointF(caretTop.X,caretTop.Y+cy);
  end;
  case Manager.ToolTextAlign of
  taRightJustify: begin
                 caretTop.X -= textWidth;
                 caretBottom.X -= textWidth;
               end;
  taCenter: begin
            caretTop.X -= textWidth div 2;
            caretBottom.X -= textWidth div 2;
          end;
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
      result := OnlyRenderChange; //hide carret
    if FText <> '' then
      ValidateActionPartially
    else
      CancelActionPartially;
  end;
end;

function TToolText.ToolKeyDown(var key: Word): TRect;
var keyUtf8: TUTF8Char;
begin
  result := EmptyRect;
  if Key = VK_SHIFT then
  begin
    FShiftKey:= true;
    Key := 0;
  end else
  if Key = VK_CONTROL then
  begin
    FCtrlKey:= true;
    Key := 0;
  end else
  if key = VK_ESCAPE then
  begin
    if FEditing then
    begin
      CancelActionPartially;
      FEditing := false;
      result := PreviousRect;
      if IsRectEmpty(result) then result := OnlyRenderChange;
      PreviousRect := EmptyRect;
      Key := 0;
    end;
  end else
  if Key = VK_RIGHT then
  begin
    if FEditing and (FTextCharPos < length(FText)+1) then
    begin
      if FCtrlKey then
      begin
        while (FTextCharPos <= length(FText)) and
            (UTF8CharStart(@FText[1],length(FText),FTextCharPos-1)^ in ['a'..'z','A'..'Z','0'..'9',#128..#255]) do
          inc(FTextCharPos);
        while (FTextCharPos <= length(FText)) and
            not (UTF8CharStart(@FText[1],length(FText),FTextCharPos-1)^ in ['a'..'z','A'..'Z','0'..'9',#128..#255]) do
          inc(FTextCharPos);
      end else
        inc(FTextCharPos);
      SelectionFollows;
      result := OnlyRenderChange;
      Key := 0;
    end else
    if FEditing and not FShiftKey then
    begin
      SelectionFollows;
      result := OnlyRenderChange;
      Key := 0;
    end;
  end else
  if Key = VK_LEFT then
  begin
    if FEditing and (FTextCharPos > 1) then
    begin
      if FCtrlKey then
      begin
        while (FTextCharPos > 1) and
            not (UTF8CharStart(@FText[1],length(FText),FTextCharPos-1-1)^ in ['a'..'z','A'..'Z','0'..'9',#128..#255]) do
          dec(FTextCharPos);
        while (FTextCharPos > 1) and
            (UTF8CharStart(@FText[1],length(FText),FTextCharPos-1-1)^ in ['a'..'z','A'..'Z','0'..'9',#128..#255]) do
          dec(FTextCharPos);
      end else
        dec(FTextCharPos);
      SelectionFollows;
      result := OnlyRenderChange;
      Key := 0;
    end else
    if FEditing and not FShiftKey then
    begin
      SelectionFollows;
      result := OnlyRenderChange;
      Key := 0;
    end;
  end else
  if Key = VK_HOME then
  begin
    if FEditing and (FTextCharPos > 1) then
    begin
      FTextCharPos := 1;
      SelectionFollows;
      result := OnlyRenderChange;
      Key := 0;
    end else
    if FEditing and not FShiftKey then
    begin
      SelectionFollows;
      result := OnlyRenderChange;
      Key := 0;
    end;
  end else
  if Key = VK_END then
  begin
    if FEditing and (FTextCharPos < length(FText)+1) then
    begin
      FTextCharPos := length(FText)+1;
      SelectionFollows;
      result := OnlyRenderChange;
      Key := 0;
    end else
    if FEditing and not FShiftKey then
    begin
      SelectionFollows;
      result := OnlyRenderChange;
      Key := 0;
    end;
  end else
  if Key = VK_SPACE then
  begin
    keyUtf8:= ' ';
    result := ToolKeyPress(keyUtf8);
    Key := 0;
  end else
  if Key = VK_BACK then
  begin
    if FEditing then
    begin
      if DeleteSelection then
        ToolUpdateNeeded := True else
      if (FText <> '') and (FTextCharPos > 1) then
      begin
        UTF8Delete(FText,FTextCharPos-1,1);
        dec(FTextCharPos);
        FTextSelStart:= FTextCharPos;
        ToolUpdateNeeded := True;
      end;
      result := EmptyRect;
      Key := 0;
    end;
  end else
  if Key = VK_DELETE then
  begin
    if FEditing then
    begin
      if DeleteSelection then
        ToolUpdateNeeded := True else
      if FTextCharPos <= length(FText) then
      begin
        UTF8Delete(FText,FTextCharPos,1);
        ToolUpdateNeeded := True;
      end;
      result := EmptyRect;
      Key := 0;
    end;
  end else
  Result:=inherited ToolKeyDown(key);
end;

function TToolText.ToolKeyUp(var key: Word): TRect;
begin
  if Key = VK_SHIFT then
  begin
    FShiftKey:= false;
    Key := 0;
  end else
  if Key = VK_CONTROL then
  begin
    FCtrlKey:= false;
    Key := 0;
  end else
    Result:=inherited ToolKeyUp(key);
end;

function TToolText.ToolKeyPress(var key: TUTF8Char): TRect;
var ty,maxy: integer;
begin
  if key = #8 then
  begin
    key := '';
    //handled as a keycode
  end else
  if (key = #13) and FEditing then
  begin
    ty := TextSize.cy;
    maxy := GetToolDrawingLayer.Height;

    if ToolUpdateNeeded then
      result := ToolUpdate
    else
      result := OnlyRenderChange; //hide carret

    FTextPos.Y := FTextPos.Y+ty;
    if FTextPos.Y >= maxy then
      FEditing := false;

    if FText <> '' then
      ValidateActionPartially
    else
      CancelActionPartially;

    if FEditing then
    begin
      FMovingText := false;
      FText := '';
      FTextCharPos:= 1;
      FTextSelStart:= 1;
      UpdateTextFX;
    end;
    key := '';
  end else
  if FEditing then
  begin
    if (length(key) = 1) and (key[1] in[#0..#31]) then
    begin
      result := EmptyRect;
      exit;
    end;
    DeleteSelection;
    UTF8Insert(key, FText, FTextCharPos);
    key := '';
    inc(FTextCharPos);
    FTextSelStart:= FTextCharPos;
    ToolUpdateNeeded := true;
    result := EmptyRect;
  end else
    result := EmptyRect;
end;

function TToolText.ToolUp: TRect;
begin
  if FMovingText then
    FMovingText := false;
  Result:= EmptyRect;
end;

function TToolText.ToolCopy: boolean;
begin
  if FTextCharPos <> FTextSelStart then
    Clipboard.AsText := UTF8Copy(FText, min(FTextCharPos,FTextSelStart), abs(FTextSelStart-FTextCharPos));
  Result:=true;
end;

function TToolText.ToolCut: boolean;
begin
  if FTextCharPos <> FTextSelStart then
  begin
    Clipboard.AsText := UTF8Copy(FText, min(FTextCharPos,FTextSelStart), abs(FTextSelStart-FTextCharPos));
    DeleteSelection;
    ToolUpdateNeeded := true;
  end;
  Result:=true;
end;

function TToolText.ToolPaste: boolean;
var txt: string;
  i : integer;
begin
  txt := Clipboard.AsText;
  for i := 1 to length(txt) do
    if txt[i] in[#0..#31] then
    begin
      txt := copy(txt,1,i-1);
      break;
    end;
  if txt<>'' then
  begin
    DeleteSelection;
    UTF8Insert(txt, FText, FTextCharPos);
    FTextSelStart:=FTextCharPos;
    inc(FTextCharPos, length(txt));
    ToolUpdateNeeded := true;
  end;
  Result:=true;
end;

function TToolText.ToolProvideCopy: boolean;
begin
  Result:= FEditing and (FTextCharPos <> FTextSelStart);
end;

function TToolText.ToolProvideCut: boolean;
begin
  Result:= FEditing and (FTextCharPos <> FTextSelStart);
end;

function TToolText.ToolProvidePaste: boolean;
begin
  Result:=true;
end;

function TToolText.Render(VirtualScreen: TBGRABitmap;
  VirtualScreenWidth, VirtualScreenHeight: integer; BitmapToVirtualScreen: TBitmapToVirtualScreenFunction): TRect;
var
  origin: TPointF;
  caretTop,caretBottom,selTop,selBottom: TPointF;
  lightPosition: TPointF;
  bounds: TRect;
begin
  result := EmptyRect;
  if FEditing then
  begin
    origin := BitmapToVirtualScreen(PointF(FTextPos.X,FTextPos.Y));
    result := RectUnion(result,NicePoint(VirtualScreen,origin));
    GetCaretPos(FTextCharPos, caretTop,caretBottom);
    caretTop := BitmapToVirtualScreen(caretTop);
    caretBottom := BitmapToVirtualScreen(caretBottom);
    if FTextSelStart <> FTextCharPos then
    begin
      GetCaretPos(FTextSelStart, selTop,selBottom);
      selTop := BitmapToVirtualScreen(selTop);
      selBottom := BitmapToVirtualScreen(selBottom);
      bounds.left := min(round(selTop.x),round(caretTop.x));
      bounds.right := max(round(selTop.x),round(caretTop.x))+1;
      bounds.top := min(round(selTop.y),round(caretTop.y));
      bounds.Bottom := max(round(selBottom.y),round(caretBottom.y))+1;
      result := RectUnion(result,bounds);
      if virtualScreen<> nil then
      begin
        VirtualScreen.FillRect(bounds,BGRA(128,128,255,128),dmDrawWithTransparency);
        VirtualScreen.DrawPolyLineAntialias([Point(round(selTop.x),round(selTop.y)),
          Point(round(selTop.x),round(selBottom.y)),Point(round(caretTop.x),round(caretBottom.y)),
          Point(round(caretTop.x),round(caretTop.y)),Point(round(selTop.x),round(selTop.y))],BGRA(255,255,255,160),
          BGRA(0,0,0,160),2,False);
      end;
    end;
    NiceLine(VirtualScreen,caretTop.X,caretTop.Y,caretBottom.x,caretBottom.y);
    result := RectUnion(result,rect(floor(caretTop.x)-1,floor(caretTop.y)-1,
      ceil(caretTop.x)+2,ceil(caretTop.y)+2));
    result := RectUnion(result,rect(floor(caretBottom.x)-1,floor(caretBottom.y)-1,
      ceil(caretBottom.x)+2,ceil(caretBottom.y)+2));
  end;
  if Manager.ToolTextPhong then
  begin
    lightPosition := BitmapToVirtualScreen(PointF(Manager.ToolLightPosition.X,Manager.ToolLightPosition.Y));
    result := RectUnion(result,NicePoint(VirtualScreen, lightPosition.X,lightPosition.Y));
    if lightPosition.Y > virtualScreenHeight/2 then
      result := RectUnion(result,NiceText(VirtualScreen, round(lightPosition.X),round(lightPosition.Y-6), VirtualScreenWidth,VirtualScreenHeight, rsLightPosition, taCenter, tlBottom))
    else
      result := RectUnion(result,NiceText(VirtualScreen, round(lightPosition.X),round(lightPosition.Y+6), VirtualScreenWidth,VirtualScreenHeight, rsLightPosition, taCenter, tlTop));
  end;
  prevToolTextPhong := Manager.ToolTextPhong;
end;

constructor TToolText.Create(AManager: TToolManager);
begin
  inherited Create(AManager);
  {$IFDEF LINUX}
  FAntialias := false;
  {$ELSE}
  FAntialias := true;
  {$ENDIF}
  TextFX_Font := TFont.Create;

  shader := TPhongShading.Create;
  shader.AmbientFactor := 0.6;
  shader.NegativeDiffusionFactor := 0.15;
  previousRect := EmptyRect;
  FShiftKey:= false;
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

