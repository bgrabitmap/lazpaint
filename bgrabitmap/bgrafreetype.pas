unit BGRAFreeType;

{$mode objfpc}{$H+}

{
  Font rendering units : BGRAText, BGRATextFX, BGRAVectorize, BGRAFreeType

  This units provide a font renderer with FreeType fonts, using the integrated FreeType font engine in Lazarus.
  The simplest way to render effects is to use TBGRAFreeTypeFontRenderer class.
  To do this, create an instance of this class and assign it to a TBGRABitmap.FontRenderer property. Now functions
  to draw text like TBGRABitmap.TextOut will use the chosen renderer.

  >> Note that you need to define the default FreeType font collection
  >> using EasyLazFreeType unit.

  To set the effects, keep a variable containing
  the TBGRAFreeTypeFontRenderer class and modify ShadowVisible and other effects parameters. The FontHinted property
  allows you to choose if the font is snapped to pixels to make it more readable.

  TBGRAFreeTypeDrawer class is the class that provides basic FreeType drawing
  by deriving the TFreeTypeDrawer type. You can use it directly, but it is not
  recommended, because there are less text layout parameters. However, it is
  necessary if you want to create TBGRATextEffect objects using FreeType fonts.
}

interface

{$i bgrabitmap.inc}

uses
  Types, Classes, SysUtils, BGRAGraphics, BGRABitmapTypes, EasyLazFreeType, FPimage,
  BGRACustomTextFX, BGRAPhongTypes;

type
  TBGRAFreeTypeDrawer = class;

  //this is the class to assign to FontRenderer property of TBGRABitmap
  { TBGRAFreeTypeFontRenderer }

  TBGRAFreeTypeFontRenderer = class(TBGRACustomFontRenderer)
  private
    FDrawer: TBGRAFreeTypeDrawer;
    FFont: TFreeTypeFont;
    function GetCollection: TCustomFreeTypeFontCollection;
    function GetDrawer(ASurface: TBGRACustomBitmap): TBGRAFreeTypeDrawer;
    function GetShaderLightPosition: TPoint;
    procedure SetShaderLightPosition(AValue: TPoint);
  protected
    FShaderOwner: boolean;
    FShader: TCustomPhongShading;
    procedure UpdateFont;
    procedure Init;
  public
    FontHinted: boolean;

    ShaderActive: boolean;

    ShadowVisible: boolean;
    ShadowColor: TBGRAPixel;
    ShadowRadius: integer;
    ShadowOffset: TPoint;
    ShadowQuality: TRadialBlurType;

    OutlineColor: TBGRAPixel;
    OutlineVisible,OuterOutlineOnly: boolean;
    OutlineTexture: IBGRAScanner;

    constructor Create;
    constructor Create(AShader: TCustomPhongShading; AShaderOwner: boolean);
    function GetFontPixelMetric: TFontPixelMetric; override;
    procedure TextOutAngle({%H-}ADest: TBGRACustomBitmap; {%H-}x, {%H-}y: single; {%H-}orientation: integer; {%H-}s: string; {%H-}c: TBGRAPixel; {%H-}align: TAlignment); override;
    procedure TextOutAngle({%H-}ADest: TBGRACustomBitmap; {%H-}x, {%H-}y: single; {%H-}orientation: integer; {%H-}s: string; {%H-}texture: IBGRAScanner; {%H-}align: TAlignment); override;
    procedure TextOut(ADest: TBGRACustomBitmap; x, y: single; s: string; texture: IBGRAScanner; align: TAlignment); override;
    procedure TextOut(ADest: TBGRACustomBitmap; x, y: single; s: string; c: TBGRAPixel; align: TAlignment); override;
    procedure TextRect(ADest: TBGRACustomBitmap; ARect: TRect; x, y: integer; s: string; style: TTextStyle; c: TBGRAPixel); override;
    procedure TextRect(ADest: TBGRACustomBitmap; ARect: TRect; x, y: integer; s: string; style: TTextStyle; texture: IBGRAScanner); override;
    function TextSize(s: string): TSize; override;
    destructor Destroy; override;
    property Collection: TCustomFreeTypeFontCollection read GetCollection;
    property ShaderLightPosition: TPoint read GetShaderLightPosition write SetShaderLightPosition;
  end;

  { TBGRAFreeTypeDrawer }

  TBGRAFreeTypeDrawer = class(TFreeTypeDrawer)
  private
    FMask: TBGRACustomBitmap;
    FColor: TBGRAPixel;
    FInCreateTextEffect: boolean;
    procedure RenderDirectly(x, y, tx: integer; data: pointer);
    procedure RenderDirectlyClearType(x, y, tx: integer; data: pointer);
    function ShadowActuallyVisible :boolean;
    function OutlineActuallyVisible: boolean;
    function ShaderActuallyActive : boolean;
  public
    Destination: TBGRACustomBitmap;
    ClearTypeRGBOrder: boolean;
    Texture: IBGRAScanner;

    Shader: TCustomPhongShading;
    ShaderActive: boolean;

    ShadowVisible: boolean;
    ShadowColor: TBGRAPixel;
    ShadowRadius: integer;
    ShadowOffset: TPoint;
    ShadowQuality: TRadialBlurType;

    OutlineColor: TBGRAPixel;
    OutlineVisible,OuterOutlineOnly: boolean;
    OutlineTexture: IBGRAScanner;

    constructor Create(ADestination: TBGRACustomBitmap);
    procedure DrawText(AText: string; AFont: TFreeTypeRenderableFont; x,y: single; AColor: TFPColor); override; overload;
    procedure DrawText(AText: string; AFont: TFreeTypeRenderableFont; x,y: single; AColor: TBGRAPixel); overload;
    procedure DrawText(AText: string; AFont: TFreeTypeRenderableFont; x,y: single; AColor: TBGRAPixel; AAlign: TFreeTypeAlignments); overload;
    { If this code does not compile, you probably have an older version of Lazarus. To fix the problem,
      go into "bgrabitmap.inc" and comment the compiler directives }
    {$IFDEF BGRABITMAP_USE_LCL12}
    procedure DrawTextWordBreak(AText: string; AFont: TFreeTypeRenderableFont; x, y, AMaxWidth: Single; AColor: TBGRAPixel; AAlign: TFreeTypeAlignments); overload;
    procedure DrawTextRect(AText: string; AFont: TFreeTypeRenderableFont; X1,Y1,X2,Y2: Single; AColor: TBGRAPixel; AAlign: TFreeTypeAlignments); overload;
    {$ENDIF}
    {$IFDEF BGRABITMAP_USE_LCL15}
    procedure DrawGlyph(AGlyph: integer; AFont: TFreeTypeRenderableFont; x,y: single; AColor: TFPColor); override; overload;
    procedure DrawGlyph(AGlyph: integer; AFont: TFreeTypeRenderableFont; x,y: single; AColor: TBGRAPixel); overload;
    procedure DrawGlyph(AGlyph: integer; AFont: TFreeTypeRenderableFont; x,y: single; AColor: TBGRAPixel; AAlign: TFreeTypeAlignments); overload;
    {$ENDIF}
    function CreateTextEffect(AText: string; AFont: TFreeTypeRenderableFont): TBGRACustomTextEffect;
    destructor Destroy; override;
  end;


implementation

uses BGRABlend, Math;

{ TBGRAFreeTypeFontRenderer }

function TBGRAFreeTypeFontRenderer.GetCollection: TCustomFreeTypeFontCollection;
begin
  result := EasyLazFreeType.FontCollection;
end;

function TBGRAFreeTypeFontRenderer.GetDrawer(ASurface: TBGRACustomBitmap): TBGRAFreeTypeDrawer;
begin
  result := FDrawer;
  result.ShadowColor := ShadowColor;
  result.ShadowOffset := ShadowOffset;
  result.ShadowRadius := ShadowRadius;
  result.ShadowVisible := ShadowVisible;
  result.ShadowQuality := ShadowQuality;
  result.ClearTypeRGBOrder := FontQuality <> fqFineClearTypeBGR;
  result.Destination := ASurface;
  result.OutlineColor := OutlineColor;
  result.OutlineVisible := OutlineVisible;
  result.OuterOutlineOnly := OuterOutlineOnly;
  result.OutlineTexture := OutlineTexture;
  if ShaderActive then result.Shader := FShader
   else result.Shader := nil;
end;

function TBGRAFreeTypeFontRenderer.GetShaderLightPosition: TPoint;
begin
  if FShader = nil then
    result := point(0,0)
  else
    result := FShader.LightPosition;
end;

procedure TBGRAFreeTypeFontRenderer.SetShaderLightPosition(AValue: TPoint);
begin
  if FShader <> nil then
    FShader.LightPosition := AValue;
end;

procedure TBGRAFreeTypeFontRenderer.UpdateFont;
var fts: TFreeTypeStyles;
  filename: string;
begin
  fts := [];
  if fsBold in FontStyle then fts += [ftsBold];
  if fsItalic in FontStyle then fts += [ftsItalic];
  try
    filename := FontName;
    {$IFDEF BGRABITMAP_USE_LCL12}
    FFont.SetNameAndStyle(filename,fts);
    {$ELSE}
    FFont.Name := filename;
    FFont.Style := fts;
    {$ENDIF}
  except
    on ex: exception do
    begin
    end;
  end;
  if FontEmHeight >= 0 then
    FFont.SizeInPixels := FontEmHeight
  else
    FFont.LineFullHeight := -FontEmHeight;
  case FontQuality of
    fqSystem:
    begin
      FFont.Quality := grqMonochrome;
      FFont.ClearType := false;
    end;
    fqSystemClearType:
    begin
      FFont.Quality:= grqLowQuality;
      FFont.ClearType:= true;
    end;
    fqFineAntialiasing:
    begin
      FFont.Quality:= grqHighQuality;
      FFont.ClearType:= false;
    end;
    fqFineClearTypeRGB,fqFineClearTypeBGR:
    begin
      FFont.Quality:= grqHighQuality;
      FFont.ClearType:= true;
    end;
  end;
  FFont.Hinted := FontHinted;
  {$IFDEF BGRABITMAP_USE_LCL12}
    FFont.StrikeOutDecoration := fsStrikeOut in FontStyle;
    FFont.UnderlineDecoration := fsUnderline in FontStyle;
  {$ENDIF}
end;

procedure TBGRAFreeTypeFontRenderer.Init;
begin
  ShaderActive := true;

  FDrawer := TBGRAFreeTypeDrawer.Create(nil);
  FFont := TFreeTypeFont.Create;
  FontHinted:= True;

  ShadowColor := BGRABlack;
  ShadowVisible := false;
  ShadowOffset := Point(5,5);
  ShadowRadius := 5;
  ShadowQuality:= rbFast;
end;

constructor TBGRAFreeTypeFontRenderer.Create;
begin
  Init;
end;

constructor TBGRAFreeTypeFontRenderer.Create(AShader: TCustomPhongShading;
  AShaderOwner: boolean);
begin
  Init;
  FShader := AShader;
  FShaderOwner := AShaderOwner;
end;

function TBGRAFreeTypeFontRenderer.GetFontPixelMetric: TFontPixelMetric;
begin
  UpdateFont;
  result.Baseline := round(FFont.Ascent);
  result.CapLine:= round(FFont.Ascent*0.2);
  result.DescentLine:= round(FFont.Ascent+FFont.Descent);
  result.Lineheight := round(FFont.LineFullHeight);
  result.xLine := round(FFont.Ascent*0.45);
  result.Defined := True;
end;

procedure TBGRAFreeTypeFontRenderer.TextOutAngle(ADest: TBGRACustomBitmap; x,
  y: single; orientation: integer; s: string; c: TBGRAPixel; align: TAlignment);
begin

end;

procedure TBGRAFreeTypeFontRenderer.TextOutAngle(ADest: TBGRACustomBitmap; x,
  y: single; orientation: integer; s: string; texture: IBGRAScanner;
  align: TAlignment);
begin

end;

procedure TBGRAFreeTypeFontRenderer.TextOut(ADest: TBGRACustomBitmap; x,
  y: single; s: string; texture: IBGRAScanner; align: TAlignment);
begin
  FDrawer.Texture := texture;
  TextOut(ADest,x,y,s,BGRAWhite,align);
  FDrawer.Texture := nil;
end;

procedure TBGRAFreeTypeFontRenderer.TextOut(ADest: TBGRACustomBitmap; x,
  y: single; s: string; c: TBGRAPixel; align: TAlignment);
var
  ftaAlign: TFreeTypeAlignments;
begin
  UpdateFont;
  ftaAlign:= [ftaTop];
  case align of
  taLeftJustify: ftaAlign += [ftaLeft];
  taCenter: ftaAlign += [ftaCenter];
  taRightJustify: ftaAlign += [ftaRight];
  end;
  GetDrawer(ADest).DrawText(s,FFont,x,y,BGRAToFPColor(c),ftaAlign);
end;

procedure TBGRAFreeTypeFontRenderer.TextRect(ADest: TBGRACustomBitmap;
  ARect: TRect; x, y: integer; s: string; style: TTextStyle; c: TBGRAPixel);
var align: TFreeTypeAlignments;
    intersectedClip,previousClip: TRect;
begin
  previousClip := ADest.ClipRect;
  if style.Clipping then
  begin
    intersectedClip := rect(0,0,0,0);
    if not IntersectRect(intersectedClip, previousClip, ARect) then exit;
    ADest.ClipRect := intersectedClip;
  end;
  UpdateFont;
  align := [];
  case style.Alignment of
  taCenter: begin ARect.Left := x; align += [ftaCenter]; end;
  taRightJustify: begin ARect.Left := x; align += [ftaRight]; end;
  else
    align += [ftaLeft];
  end;
  case style.Layout of
  {$IFDEF BGRABITMAP_USE_LCL12}
    tlCenter: begin ARect.Top := y; align += [ftaVerticalCenter]; end;
  {$ENDIF}
  tlBottom: begin ARect.top := y; align += [ftaBottom]; end;
  else align += [ftaTop];
  end;
  try
    {$IFDEF BGRABITMAP_USE_LCL12}
      if style.Wordbreak then
        GetDrawer(ADest).DrawTextRect(s, FFont, ARect.Left,ARect.Top,ARect.Right,ARect.Bottom,BGRAToFPColor(c),align)
      else
    {$ENDIF}
    begin
      case style.Layout of
      tlCenter: y := (ARect.Top+ARect.Bottom) div 2;
      tlBottom: y := ARect.Bottom;
      else
        y := ARect.Top;
      end;
      case style.Alignment of
      taLeftJustify: GetDrawer(ADest).DrawText(s,FFont,ARect.Left,y,BGRAToFPColor(c),align);
      taCenter: GetDrawer(ADest).DrawText(s,FFont,(ARect.Left+ARect.Right-1) div 2,y,BGRAToFPColor(c),align);
      taRightJustify: GetDrawer(ADest).DrawText(s,FFont,ARect.Right,y,BGRAToFPColor(c),align);
      end;
    end;
  finally
    if style.Clipping then
      ADest.ClipRect := previousClip;
  end;
end;

procedure TBGRAFreeTypeFontRenderer.TextRect(ADest: TBGRACustomBitmap;
  ARect: TRect; x, y: integer; s: string; style: TTextStyle;
  texture: IBGRAScanner);
begin
  FDrawer.Texture := texture;
  TextRect(ADest,ARect,x,y,s,style,BGRAWhite);
  FDrawer.Texture := nil;
end;

function TBGRAFreeTypeFontRenderer.TextSize(s: string): TSize;
begin
  UpdateFont;
  result.cx := round(FFont.TextWidth(s));
  result.cy := round(FFont.LineFullHeight);
end;

destructor TBGRAFreeTypeFontRenderer.Destroy;
begin
  FDrawer.Free;
  FFont.Free;
  if FShaderOwner then FShader.Free;
  inherited Destroy;
end;

{ TBGRAFreeTypeDrawer }

procedure TBGRAFreeTypeDrawer.RenderDirectly( x,y,tx: integer;
                          data: pointer );
var psrc: pbyte;
    pdest: PBGRAPixel;
    c: TBGRAPixel;
begin
  if Destination <> nil then
  begin
    //ensure rendering in bounds
    if (y < 0) or (y >= Destination.height) or (x < 0) or (x > Destination.width-tx) then exit;

    psrc := pbyte(data);
    pdest := Destination.ScanLine[y]+x;
    if Texture = nil then
    begin
      c := FColor;
      while tx > 0 do
      begin
        DrawPixelInlineWithAlphaCheck(pdest,c,psrc^);
        inc(psrc);
        inc(pdest);
        dec(tx);
      end;
    end else
    begin
      Texture.ScanMoveTo(x,y);
      while tx > 0 do
      begin
        DrawPixelInlineWithAlphaCheck(pdest,Texture.ScanNextPixel,psrc^);
        inc(psrc);
        inc(pdest);
        dec(tx);
      end;
    end;
  end;
end;

procedure TBGRAFreeTypeDrawer.RenderDirectlyClearType(x, y, tx: integer; data: pointer);
var xb: integer;
    psrc: pbyte;
    pdest: PBGRAPixel;
begin
  if Destination <> nil then
  begin
    tx := tx div 3;
    if tx=0 then exit;
    if (FMask <> nil) and (FMask.Width <> tx) then
      FMask.SetSize(tx,1)
    else if FMask = nil then FMask := BGRABitmapFactory.create(tx,1);

    pdest := FMask.Data;
    psrc := pbyte(data);
    pdest^.red := (psrc^ + psrc^ + (psrc+1)^) div 3;
    pdest^.green := (psrc^+ (psrc+1)^ + (psrc+2)^) div 3;
    if tx > 1 then
      pdest^.blue := ((psrc+1)^ + (psrc+2)^ + (psrc+3)^) div 3
    else
      pdest^.blue := ((psrc+1)^ + (psrc+2)^ + (psrc+2)^) div 3;
    inc(pdest);
    inc(psrc,3);
    for xb := 1 to tx-2 do
    begin
      pdest^.red := ((psrc-1)^+ psrc^ + (psrc+1)^) div 3;
      pdest^.green := (psrc^+ (psrc+1)^ + (psrc+2)^) div 3;
      pdest^.blue := ((psrc+1)^ + (psrc+2)^ + (psrc+3)^) div 3;
      inc(pdest);
      inc(psrc,3);
    end;
    if tx > 1 then
    begin
      pdest^.red := ((psrc-1)^+ psrc^ + (psrc+1)^) div 3;
      pdest^.green := (psrc^+ (psrc+1)^ + (psrc+2)^) div 3;
      pdest^.blue := ((psrc+1)^ + (psrc+2)^ + (psrc+2)^) div 3;
    end;
    BGRAFillClearTypeRGBMask(Destination,x div 3,y,FMask,FColor,Texture,ClearTypeRGBOrder);
  end;
end;

function TBGRAFreeTypeDrawer.ShadowActuallyVisible: boolean;
begin
  result := ShadowVisible and (ShadowColor.alpha <> 0);
end;

function TBGRAFreeTypeDrawer.OutlineActuallyVisible: boolean;
begin
  result := ((OutlineTexture <> nil) or (OutlineColor.alpha <> 0)) and OutlineVisible;
end;

function TBGRAFreeTypeDrawer.ShaderActuallyActive: boolean;
begin
  result := (Shader <> nil) and ShaderActive;
end;

constructor TBGRAFreeTypeDrawer.Create(ADestination: TBGRACustomBitmap);
begin
  Destination := ADestination;
  ClearTypeRGBOrder:= true;
  ShaderActive := true;
  ShadowQuality:= rbFast;
end;

procedure TBGRAFreeTypeDrawer.DrawText(AText: string;
  AFont: TFreeTypeRenderableFont; x, y: single; AColor: TFPColor);
var fx: TBGRACustomTextEffect;
  procedure DoOutline;
  begin
    if OutlineActuallyVisible then
    begin
      if OutlineTexture <> nil then
        fx.DrawOutline(Destination,round(x),round(y), OutlineTexture)
      else
        fx.DrawOutline(Destination,round(x),round(y), OutlineColor);
    end;
  end;
begin
  if not FInCreateTextEffect and (ShadowActuallyVisible or OutlineActuallyVisible or ShaderActuallyActive) then
  begin
    fx := CreateTextEffect(AText, AFont);
    fx.ShadowQuality := ShadowQuality;
    y -= AFont.Ascent;
    if ShadowActuallyVisible then fx.DrawShadow(Destination, round(x+ShadowOffset.X),round(y+ShadowOffset.Y), ShadowRadius, ShadowColor);
    if OuterOutlineOnly then DoOutline;

    if texture <> nil then
    begin
      if ShaderActuallyActive then
        fx.DrawShaded(Destination,floor(x),floor(y), Shader, round(fx.TextSize.cy*0.05), texture)
      else
        fx.Draw(Destination,round(x),round(y), texture);
    end else
    begin
      if ShaderActuallyActive then
        fx.DrawShaded(Destination,floor(x),floor(y), Shader, round(fx.TextSize.cy*0.05), FPColorToBGRA(AColor))
      else
        fx.Draw(Destination,round(x),round(y), FPColorToBGRA(AColor));
    end;
    if not OuterOutlineOnly then DoOutline;
    fx.Free;
  end else
  begin
    FColor := FPColorToBGRA(AColor);
    if AFont.ClearType then
      AFont.RenderText(AText, x, y, Destination.ClipRect, @RenderDirectlyClearType)
    else
      AFont.RenderText(AText, x, y, Destination.ClipRect, @RenderDirectly);
  end;
end;

procedure TBGRAFreeTypeDrawer.DrawText(AText: string;
  AFont: TFreeTypeRenderableFont; x, y: single; AColor: TBGRAPixel);
begin
  DrawText(AText, AFont, x,y, BGRAToFPColor(AColor));
end;

procedure TBGRAFreeTypeDrawer.DrawText(AText: string;
  AFont: TFreeTypeRenderableFont; x, y: single; AColor: TBGRAPixel;
  AAlign: TFreeTypeAlignments);
begin
  DrawText(AText, AFont, x,y, BGRAToFPColor(AColor), AAlign);
end;

{$IFDEF BGRABITMAP_USE_LCL12}
procedure TBGRAFreeTypeDrawer.DrawTextWordBreak(AText: string;
  AFont: TFreeTypeRenderableFont; x, y, AMaxWidth: Single; AColor: TBGRAPixel;
  AAlign: TFreeTypeAlignments);
begin
  DrawTextWordBreak(AText,AFont,x,y,AMaxWidth,BGRAToFPColor(AColor),AAlign);
end;

procedure TBGRAFreeTypeDrawer.DrawTextRect(AText: string;
  AFont: TFreeTypeRenderableFont; X1, Y1, X2, Y2: Single; AColor: TBGRAPixel;
  AAlign: TFreeTypeAlignments);
begin
  DrawTextRect(AText,AFont,X1,Y1,X2,Y2,BGRAToFPColor(AColor),AAlign);
end;
{$ENDIF}

{$IFDEF BGRABITMAP_USE_LCL15}
procedure TBGRAFreeTypeDrawer.DrawGlyph(AGlyph: integer;
  AFont: TFreeTypeRenderableFont; x, y: single; AColor: TFPColor);
var f: TFreeTypeFont;
begin
  if not (AFont is TFreeTypeFont) then exit;
  f := TFreeTypeFont(Afont);
  FColor := FPColorToBGRA(AColor);
  if AFont.ClearType then
    f.RenderGlyph(AGlyph, x, y, Destination.ClipRect, @RenderDirectlyClearType)
  else
    f.RenderGlyph(AGlyph, x, y, Destination.ClipRect, @RenderDirectly);
end;

procedure TBGRAFreeTypeDrawer.DrawGlyph(AGlyph: integer;
  AFont: TFreeTypeRenderableFont; x, y: single; AColor: TBGRAPixel);
begin
  DrawGlyph(AGlyph, AFont, x,y, BGRAToFPColor(AColor));
end;

procedure TBGRAFreeTypeDrawer.DrawGlyph(AGlyph: integer;
  AFont: TFreeTypeRenderableFont; x, y: single; AColor: TBGRAPixel;
  AAlign: TFreeTypeAlignments);
begin
  DrawGlyph(AGlyph, AFont, x,y, BGRAToFPColor(AColor), AAlign);
end;
{$ENDIF}

function TBGRAFreeTypeDrawer.CreateTextEffect(AText: string;
  AFont: TFreeTypeRenderableFont): TBGRACustomTextEffect;
var
  mask: TBGRACustomBitmap;
  tx,ty,marginHoriz,marginVert: integer;
  tempDest: TBGRACustomBitmap;
  tempTex: IBGRAScanner;
  tempClearType: boolean;
begin
  FInCreateTextEffect:= True;
  try
    tx := ceil(AFont.TextWidth(AText));
    ty := ceil(AFont.TextHeight(AText));
    marginHoriz := ty div 2;
    marginVert := 1;
    mask := BGRABitmapFactory.Create(tx+2*marginHoriz,ty+2*marginVert,BGRABlack);
    tempDest := Destination;
    tempTex := Texture;
    tempClearType:= AFont.ClearType;
    Destination := mask;
    Texture := nil;
    AFont.ClearType := false;
    DrawText(AText,AFont,marginHoriz,marginVert,BGRAWhite,[ftaTop,ftaLeft]);
    Destination := tempDest;
    Texture := tempTex;
    AFont.ClearType := tempClearType;
    mask.ConvertToLinearRGB;
    result := TBGRACustomTextEffect.Create(mask, true,tx,ty,point(-marginHoriz,-marginVert));
  finally
    FInCreateTextEffect:= false;
  end;
end;

destructor TBGRAFreeTypeDrawer.Destroy;
begin
  FMask.Free;
  inherited Destroy;
end;

end.
