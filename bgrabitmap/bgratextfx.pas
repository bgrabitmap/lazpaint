unit BGRATextFX;

{$mode objfpc}{$H+}

{
  Font rendering units : BGRAText, BGRATextFX, BGRAVectorize, BGRAFreeType

  This unit provide text effects. The simplest way to render effects is to use TBGRATextEffectFontRenderer class.
  To do this, create an instance of this class and assign it to a TBGRABitmap.FontRenderer property. Now functions
  to draw text like TBGRABitmap.TextOut will use the chosen renderer. To set the effects, keep a variable containing
  the TBGRATextEffectFontRenderer class and modify ShadowVisible and other effects parameters.

  The TBGRATextEffectFontRenderer class makes use of other classes depending on the situation. For example,
  TBGRATextEffect, which is also in this unit, provides effects on a text mask. But the renderer also uses
  BGRAVectorize unit in order to have big texts or to rotate them at will.

  Note that you may need TBGRATextEffect if you want to have more control over text effects, especially
  if you always draw the same text. Keeping the same TBGRATextEffect object will avoid creating the text
  mask over and over again.

  TextShadow function is a simple function to compute an image containing a text with shadow.

}

interface

uses
  Classes, SysUtils, Graphics, Types, BGRABitmapTypes, BGRAPhongTypes, BGRAText, BGRAVectorize;

type
  TBGRATextEffect = class;

  { TBGRATextEffectFontRenderer }

  TBGRATextEffectFontRenderer = class(TCustomLCLFontRenderer)
  private
    function GetShaderLightPosition: TPoint;
    function GetVectorizedRenderer: TBGRAVectorizedFontRenderer;
    procedure SetShaderLightPosition(AValue: TPoint);
  protected
    FShaderOwner: boolean;
    FShader: TCustomPhongShading;
    FVectorizedRenderer: TBGRAVectorizedFontRenderer;
    function ShadowActuallyVisible :boolean;
    function ShaderActuallyActive: boolean;
    function OutlineActuallyVisible: boolean;
    procedure Init;
    function VectorizedFontNeeded: boolean;
    procedure InternalTextOut(ADest: TBGRACustomBitmap; x, y: single; s: string; c: TBGRAPixel; texture: IBGRAScanner; align: TAlignment);
  public
    ShaderActive: boolean;

    ShadowVisible: boolean;
    ShadowColor: TBGRAPixel;
    ShadowRadius: integer;
    ShadowOffset: TPoint;

    OutlineColor: TBGRAPixel;
    OutlineWidth: single;
    OutlineVisible,OuterOutlineOnly: boolean;
    OutlineTexture: IBGRAScanner;
    constructor Create;
    constructor Create(AShader: TCustomPhongShading; AShaderOwner: boolean);
    destructor Destroy; override;
    procedure TextOutAngle(ADest: TBGRACustomBitmap; x, y: single; orientation: integer;
      s: string; texture: IBGRAScanner; align: TAlignment); override;
    procedure TextOutAngle(ADest: TBGRACustomBitmap; x, y: single; orientation: integer;
      s: string; c: TBGRAPixel; align: TAlignment); override;
    procedure TextOut(ADest: TBGRACustomBitmap; x, y: single; s: string;
      texture: IBGRAScanner; align: TAlignment); override;
    procedure TextOut(ADest: TBGRACustomBitmap; x, y: single; s: string; c: TBGRAPixel;
      align: TAlignment); override;
    function TextSize(sUTF8: string): TSize; override;
    property Shader: TCustomPhongShading read FShader;
    property ShaderLightPosition: TPoint read GetShaderLightPosition write SetShaderLightPosition;
    property VectorizedFontRenderer: TBGRAVectorizedFontRenderer read GetVectorizedRenderer;
  end;

  { TBGRATextEffect }

  TBGRATextEffect = class
  private
    function GetBounds: TRect;
    function GetMaskHeight: integer;
    class function GetOutlineWidth: integer; static;
    function GetShadowBounds(ARadius: integer): TRect;
    function GetMaskWidth: integer;
    function GetTextHeight: integer;
    function GetTextWidth: integer;
  protected
    FTextMask: TBGRACustomBitmap;
    FShadowRadius: integer;
    FOutlineMask, FShadowMask, FShadingMask : TBGRACustomBitmap;
    FShadingAltitude: integer;
    FShadingRounded: boolean;
    FTextSize: TSize;
    FOffset: TPoint;
    function DrawMaskMulticolored(ADest: TBGRACustomBitmap; AMask: TBGRACustomBitmap; X,Y: Integer; const AColors: array of TBGRAPixel): TRect;
    function DrawMask(ADest: TBGRACustomBitmap; AMask: TBGRACustomBitmap; X,Y: Integer; AColor: TBGRAPixel): TRect;
    function DrawMask(ADest: TBGRACustomBitmap; AMask: TBGRACustomBitmap; X,Y: Integer; ATexture: IBGRAScanner): TRect;
    function InternalDrawShaded(ADest: TBGRACustomBitmap; X,Y: integer; Shader: TCustomPhongShading; Altitude: integer; AColor: TBGRAPixel; ATexture: IBGRAScanner; ARounded: Boolean): TRect;
    procedure InitImproveReadability(AText: string; Font: TFont; SubOffsetX,SubOffsetY: single);
    procedure Init(AText: string; Font: TFont; Antialiasing: boolean; SubOffsetX,SubOffsetY: single; GrainX, GrainY: Integer);
    procedure InitWithFontName(AText: string; AFontName: string; AFullHeight: integer; AStyle: TFontStyles; Antialiasing: boolean; SubOffsetX,SubOffsetY: single);
  public
    constructor Create(AText: string; Font: TFont; Antialiasing: boolean);
    constructor Create(AText: string; Font: TFont; Antialiasing: boolean; SubOffsetX,SubOffsetY: single);
    constructor Create(AText: string; Font: TFont; Antialiasing: boolean; SubOffsetX,SubOffsetY: single; GrainX, GrainY: Integer);
    constructor Create(AText: string; AFontName: string; AFullHeight: integer; Antialiasing: boolean);
    constructor Create(AText: string; AFontName: string; AFullHeight: integer; Antialiasing: boolean; SubOffsetX,SubOffsetY: single);
    constructor Create(AText: string; AFontName: string; AFullHeight: integer; AStyle: TFontStyles; Antialiasing: boolean);
    constructor Create(AText: string; AFontName: string; AFullHeight: integer; AStyle: TFontStyles; Antialiasing: boolean; SubOffsetX,SubOffsetY: single);
    constructor Create(AMask: TBGRACustomBitmap; AMaskOwner: boolean; AWidth,AHeight: integer; AOffset: TPoint);
    procedure ApplySphere;
    procedure ApplyVerticalCylinder;
    procedure ApplyHorizontalCylinder;
    function Draw(ADest: TBGRACustomBitmap; X,Y: integer; AColor: TBGRAPixel): TRect;
    function Draw(ADest: TBGRACustomBitmap; X,Y: integer; ATexture: IBGRAScanner): TRect;
    function Draw(ADest: TBGRACustomBitmap; X, Y: integer; AColor: TBGRAPixel; AAlign: TAlignment): TRect;
    function Draw(ADest: TBGRACustomBitmap; X, Y: integer; ATexture: IBGRAScanner; AAlign: TAlignment): TRect;

    function DrawShaded(ADest: TBGRACustomBitmap; X,Y: integer; Shader: TCustomPhongShading; Altitude: integer; AColor: TBGRAPixel; ARounded: Boolean = true): TRect;
    function DrawShaded(ADest: TBGRACustomBitmap; X,Y: integer; Shader: TCustomPhongShading; Altitude: integer; ATexture: IBGRAScanner; ARounded: Boolean = true): TRect;
    function DrawShaded(ADest: TBGRACustomBitmap; X, Y: integer; Shader: TCustomPhongShading; Altitude: integer; AColor: TBGRAPixel; AAlign: TAlignment; ARounded: Boolean = true): TRect;
    function DrawShaded(ADest: TBGRACustomBitmap; X, Y: integer; Shader: TCustomPhongShading; Altitude: integer; ATexture: IBGRAScanner; AAlign: TAlignment; ARounded: Boolean = true): TRect;

    function DrawMulticolored(ADest: TBGRACustomBitmap; X,Y: integer; const AColors: array of TBGRAPixel): TRect;
    function DrawMulticolored(ADest: TBGRACustomBitmap; X,Y: integer; const AColors: array of TBGRAPixel; AAlign: TAlignment): TRect;
    function DrawOutline(ADest: TBGRACustomBitmap; X,Y: integer; AColor: TBGRAPixel): TRect;
    function DrawOutline(ADest: TBGRACustomBitmap; X,Y: integer; ATexture: IBGRAScanner): TRect;
    function DrawOutline(ADest: TBGRACustomBitmap; X,Y: integer; AColor: TBGRAPixel; AAlign: TAlignment): TRect;
    function DrawOutline(ADest: TBGRACustomBitmap; X,Y: integer; ATexture: IBGRAScanner; AAlign: TAlignment): TRect;
    function DrawShadow(ADest: TBGRACustomBitmap; X,Y,Radius: integer; AColor: TBGRAPixel): TRect;
    function DrawShadow(ADest: TBGRACustomBitmap; X,Y,Radius: integer; AColor: TBGRAPixel; AAlign: TAlignment): TRect;
    destructor Destroy; override;
    property TextMask: TBGRACustomBitmap read FTextMask;
    property TextMaskOffset: TPoint read FOffset;
    property Width: integer read GetTextWidth; deprecated;
    property Height: integer read GetTextHeight; deprecated;
    property MaskWidth: integer read GetMaskWidth;
    property MaskHeight: integer read GetMaskHeight;
    property TextSize: TSize read FTextSize;
    property TextWidth: integer read GetTextWidth;
    property TextHeight: integer read GetTextHeight;
    property Bounds: TRect read GetBounds;
    property ShadowBounds[ARadius: integer]: TRect read GetShadowBounds;
    class property OutlineWidth: integer read GetOutlineWidth;
  end;

function TextShadow(AWidth,AHeight: Integer; AText: String; AFontHeight: Integer; ATextColor,AShadowColor: TBGRAPixel;
    AOffSetX,AOffSetY: Integer; ARadius: Integer = 0; AFontStyle: TFontStyles = []; AFontName: String = 'Default'; AShowText: Boolean = True; AFontQuality: TBGRAFontQuality = fqFineAntialiasing): TBGRACustomBitmap;

procedure BGRATextOutImproveReadability(bmp: TBGRACustomBitmap; AFont: TFont; xf,yf: single; text: string; color: TBGRAPixel; tex: IBGRAScanner; align: TAlignment; mode : TBGRATextOutImproveReadabilityMode);

implementation

uses BGRAGradientScanner, GraphType, Math, BGRAGrayscaleMask;

const DefaultOutlineWidth = 3;

procedure BGRATextOutImproveReadability(bmp: TBGRACustomBitmap; AFont: TFont; xf,yf: single; text: string; color: TBGRAPixel; tex: IBGRAScanner; align: TAlignment; mode : TBGRATextOutImproveReadabilityMode);
var
  useClearType,clearTypeRGBOrder: boolean;
  metric: TFontPixelMetric;
  deltaX: single;
  x,y,yb,cury,fromy: integer;
  toAdd: integer;
  lines: array[0..3] of integer;
  parts: array[0..3] of TGrayscaleMask;
  n,nbLines: integer;
  alphaMax: NativeUint;
  ptrPart: TBGRACustomBitmap;
  pmask: PByte;
  fx: TBGRATextEffect;
  FxFont: TFont;
  prevCenter, newCenter, diffCenter: single;
  xThird: integer;

begin
  useClearType:= mode in[irClearTypeRGB,irClearTypeBGR];
  clearTypeRGBOrder := mode <> irClearTypeBGR;
  deltaX := xf-floor(xf);
  x := round(floor(xf));

  FxFont := TFont.Create;
  FxFont.Assign(AFont);
  FxFont.Height := fxFont.Height*FontAntialiasingLevel;
  metric := GetFontPixelMetric(FxFont);
  if not metric.Defined or (metric.Lineheight < 8*FontAntialiasingLevel) or (metric.Lineheight >= 24*FontAntialiasingLevel) then
  begin
    fxFont.Free;
    if useClearType then
    begin
      if ClearTypeRGBOrder then
        BGRATextOut(bmp, AFont, fqFineClearTypeRGB, xf,yf, text, color, tex, align)
      else
        BGRATextOut(bmp, AFont, fqFineClearTypeBGR, xf,yf, text, color, tex, align)
    end else
      BGRATextOut(bmp, AFont, fqFineAntialiasing, xf,yf, text, color, tex, align);
    exit;
  end;

  if (metric.Baseline-metric.xLine) mod FontAntialiasingLevel >= FontAntialiasingLevel div 3 then
  begin
    toAdd := FontAntialiasingLevel- ((metric.Baseline-metric.xLine) mod FontAntialiasingLevel);
    for yb := 1 to toAdd div 2 do
    begin
      if metric.xLine > 0 then dec(metric.xLine);
      if metric.Baseline < metric.Lineheight then inc(metric.Baseline);
    end;
  end;
  if metric.CapLine >= metric.xLine then metric.CapLine := -1 else
  begin
    if (metric.xLine-metric.CapLine) mod FontAntialiasingLevel >= FontAntialiasingLevel div 2 then
    begin
      toAdd := FontAntialiasingLevel - (metric.xLine-metric.CapLine) mod FontAntialiasingLevel;
      metric.CapLine -= toAdd;
      if metric.CapLine <= 0 then metric.CapLine := -1;
    end;
  end;

  nbLines := 0;
  lines[nbLines] := metric.CapLine+1;
  inc(nbLines);
  lines[nbLines] := metric.xLine+1;
  inc(nbLines);
  lines[nbLines] := metric.Baseline+1;
  inc(nbLines);
  lines[nbLines] := metric.Lineheight+1;
  inc(nbLines);

  if not useClearType then
    fx := TBGRATextEffect.Create(text,FxFont,False,deltaX*FontAntialiasingLevel,0,FontAntialiasingLevel,FontAntialiasingLevel) else
    fx := TBGRATextEffect.Create(text,FxFont,False,0,0,3,0);

  if fx.TextMask = nil then
  begin
    fx.Free;
    FxFont.Free;
    exit;
  end;
  alphaMax := 0;
  prevCenter := 0;
  newCenter := 0;
  for yb := 0 to nbLines-1 do
  begin
    if yb= 0 then fromy := 0
     else fromy := lines[yb-1];

    if lines[yb] > fromy then
    begin
      ptrPart := fx.TextMask.GetPtrBitmap(fromy,lines[yb]);
      if useClearType then
        parts[yb] := TGrayscaleMask.CreateDownSample(ptrPart,round(ptrPart.Width/FontAntialiasingLevel*3),round(ptrPart.Height/FontAntialiasingLevel))
      else
        parts[yb] := TGrayscaleMask.CreateDownSample(ptrPart,round(ptrPart.Width/FontAntialiasingLevel),round(ptrPart.Height/FontAntialiasingLevel));
      ptrPart.Free;

      if alphaMax < 255 then
      begin
        pmask := parts[yb].Data;
        for n := parts[yb].NbPixels-1 downto 0 do
        begin
          if pmask^ > alphaMax then alphaMax := pmask^;
          inc(pmask);
        end;
      end;

      if yb < 2 then
      begin
        newCenter += parts[yb].Height;
        prevCenter += lines[yb]-fromy;
      end else
      if yb = 2 then
      begin
        newCenter += parts[yb].Height/2;
        prevCenter += (lines[yb]-fromy)/2;
      end;
    end else
      parts[yb] := nil;
  end;

  prevCenter /= FontAntialiasingLevel;
  diffCenter := prevCenter-newCenter;
  y := round( yf + diffCenter );

  xThird := 0;
  if useClearType then
  begin
    case align of
    taCenter: xThird:= xThird+round(((fx.TextMaskOffset.x-fx.TextWidth/2)/FontAntialiasingLevel+deltaX)*3);
    taRightJustify: xThird:= xThird+round(((fx.TextMaskOffset.x-fx.TextWidth)/FontAntialiasingLevel+deltaX)*3);
    else xThird:= xThird+round((fx.TextMaskOffset.x/FontAntialiasingLevel+deltaX)*3);
    end;
  end else
  begin
    case align of
    taCenter: x:= x+round((fx.TextMaskOffset.x-fx.TextWidth/2)/FontAntialiasingLevel);
    taRightJustify: x:= x+round((fx.TextMaskOffset.x-fx.TextWidth)/FontAntialiasingLevel);
    else x:= x+round(fx.TextMaskOffset.x/FontAntialiasingLevel);
    end;
  end;
  cury := y+round(fx.TextMaskOffset.y/FontAntialiasingLevel);
  for yb := 0 to nbLines-1 do
  if parts[yb] <> nil then
  begin
    if (alphaMax > 0) and (alphaMax < 255) then
    begin
      pmask := parts[yb].data;
      for n := parts[yb].NbPixels-1 downto 0 do
      begin
        pmask^ := pmask^*255 div alphaMax;
        inc(pmask);
      end;
    end;
    if useClearType then
      BGRAFillClearTypeGrayscaleMask(bmp,x,cury,xThird,parts[yb],color,tex,ClearTypeRGBOrder)
    else if mode = irMask then
      parts[yb].Draw(bmp,x,cury)
    else
    begin
      if tex <> nil then
        parts[yb].DrawAsAlpha(bmp,x,cury,tex) else
        parts[yb].DrawAsAlpha(bmp,x,cury,color);
    end;
    inc(cury,parts[yb].Height);
    parts[yb].Free;
  end;

  fx.Free;
  FxFont.Free;
end;

procedure BGRAReplace(var Destination: TBGRACustomBitmap; Temp: TObject);
begin
  Destination.Free;
  Destination := Temp as TBGRACustomBitmap;
end;

function TextShadow(AWidth,AHeight: Integer; AText: String; AFontHeight: Integer; ATextColor,AShadowColor: TBGRAPixel;
  AOffSetX,AOffSetY: Integer; ARadius: Integer = 0; AFontStyle: TFontStyles = []; AFontName: String = 'Default'; AShowText: Boolean = True; AFontQuality: TBGRAFontQuality = fqFineAntialiasing): TBGRACustomBitmap;
var
  bmpOut,bmpSdw: TBGRACustomBitmap; OutTxtSize: TSize; OutX,OutY: Integer;
begin
  bmpOut:= BGRABitmapFactory.Create(AWidth,AHeight);
  bmpOut.FontAntialias:= True;
  bmpOut.FontHeight:= AFontHeight;
  bmpOut.FontStyle:= AFontStyle;
  bmpOut.FontName:= AFontName;
  bmpOut.FontQuality:= AFontQuality;

  OutTxtSize:= bmpOut.TextSize(AText);
  OutX:= Round(AWidth/2) - Round(OutTxtSize.cx/2);
  OutY:= Round(AHeight/2) - Round(OutTxtSize.cy/2);

  bmpSdw:= BGRABitmapFactory.Create(OutTxtSize.cx+2*ARadius,OutTxtSize.cy+2*ARadius);
  bmpSdw.FontAntialias:= True;
  bmpSdw.FontHeight:= AFontHeight;
  bmpSdw.FontStyle:= AFontStyle;
  bmpSdw.FontName:= AFontName;
  bmpSdw.FontQuality:= AFontQuality;

  bmpSdw.TextOut(ARadius,ARadius,AText,AShadowColor);
  BGRAReplace(bmpSdw,bmpSdw.FilterBlurRadial(ARadius,rbFast));
  bmpOut.PutImage(OutX+AOffSetX-ARadius,OutY+AOffSetY-ARadius,bmpSdw,dmDrawWithTransparency);
  bmpSdw.Free;

  if AShowText = True then bmpOut.TextOut(OutX,OutY,AText,ATextColor);

  Result:= bmpOut;
end;

{ TBGRATextEffectFontRenderer }

function TBGRATextEffectFontRenderer.GetShaderLightPosition: TPoint;
begin
  if FShader = nil then
    result := point(0,0)
  else
    result := FShader.LightPosition;
end;

function TBGRATextEffectFontRenderer.GetVectorizedRenderer: TBGRAVectorizedFontRenderer;
begin
  FVectorizedRenderer.FontEmHeight := FontEmHeight;
  FVectorizedRenderer.FontName := FontName;
  FVectorizedRenderer.FontOrientation:= FontOrientation;
  FVectorizedRenderer.FontQuality := FontQuality;
  FVectorizedRenderer.FontStyle:= FontStyle;

  FVectorizedRenderer.ShadowColor := ShadowColor;
  FVectorizedRenderer.ShadowVisible := ShadowVisible;
  FVectorizedRenderer.ShadowOffset := ShadowOffset;
  FVectorizedRenderer.ShadowRadius := ShadowRadius;

  FVectorizedRenderer.OutlineColor := OutlineColor;
  FVectorizedRenderer.OutlineVisible := OutlineVisible;
  FVectorizedRenderer.OutlineWidth := OutlineWidth;
  FVectorizedRenderer.OutlineTexture := OutlineTexture;
  FVectorizedRenderer.OuterOutlineOnly := OuterOutlineOnly;
  result := FVectorizedRenderer;
end;

procedure TBGRATextEffectFontRenderer.SetShaderLightPosition(AValue: TPoint);
begin
  if FShader <> nil then
    FShader.LightPosition := AValue;
end;

function TBGRATextEffectFontRenderer.ShadowActuallyVisible: boolean;
begin
  result := ShadowVisible and (ShadowColor.alpha <> 0);
end;

function TBGRATextEffectFontRenderer.ShaderActuallyActive: boolean;
begin
  result := (FShader <> nil) and ShaderActive;
end;

function TBGRATextEffectFontRenderer.OutlineActuallyVisible: boolean;
begin
  result := (OutlineWidth <> 0) and ((OutlineTexture <> nil) or (OutlineColor.alpha <> 0)) and OutlineVisible;
end;

procedure TBGRATextEffectFontRenderer.Init;
begin
  ShaderActive := true;

  ShadowColor := BGRABlack;
  ShadowVisible := false;
  ShadowOffset := Point(5,5);
  ShadowRadius := 5;

  OutlineColor := BGRAPixelTransparent;
  OutlineVisible := True;
  OutlineWidth:= DefaultOutlineWidth;
  OuterOutlineOnly:= false;
  FVectorizedRenderer := TBGRAVectorizedFontRenderer.Create;
end;

function TBGRATextEffectFontRenderer.VectorizedFontNeeded: boolean;
var bAntialiasing, bBigFont, bSpecialOutline, bOriented, bEffectVectorizedSupported: boolean;
  textsz: TSize;
begin
  bAntialiasing := FontQuality in [fqFineAntialiasing,fqFineClearTypeRGB,fqFineClearTypeBGR];
  textsz := inherited TextSize('Hg');
  bBigFont := (not OutlineActuallyVisible and (textsz.cy >= 24)) or
     (OutlineActuallyVisible and (textsz.cy > 42));
  bSpecialOutline:= OutlineActuallyVisible and (abs(OutlineWidth) <> DefaultOutlineWidth);
  bOriented := FontOrientation <> 0;
  bEffectVectorizedSupported := OutlineActuallyVisible or ShadowActuallyVisible;
  if ShaderActuallyActive and (FontOrientation = 0) then
    result := false //shader not supported by vectorized font
  else
    result := bSpecialOutline or
              (bAntialiasing and bBigFont) or
              (bOriented and bEffectVectorizedSupported);
end;

procedure TBGRATextEffectFontRenderer.InternalTextOut(ADest: TBGRACustomBitmap;
  x, y: single; s: string; c: TBGRAPixel; texture: IBGRAScanner;
  align: TAlignment);
var fx: TBGRATextEffect;
  procedure DoOutline;
  begin
    if OutlineActuallyVisible then
    begin
      if OutlineTexture <> nil then
        fx.DrawOutline(ADest,round(x),round(y), OutlineTexture, align)
      else
        fx.DrawOutline(ADest,round(x),round(y), OutlineColor, align);
    end;
  end;
begin
  UpdateFont;
  if (FFont.Orientation <> 0) or (not ShaderActuallyActive and not ShadowActuallyVisible and not OutlineActuallyVisible) then
  begin
    if texture <> nil then
      inherited TextOut(ADest,x,y,s,texture,align)
    else
      inherited TextOut(ADest,x,y,s,c,align);
    exit;
  end;
  fx := TBGRATextEffect.Create(s, FFont, FontQuality in[fqFineAntialiasing,fqFineClearTypeBGR,fqFineClearTypeRGB], x-floor(x),y-floor(y));
  if ShadowActuallyVisible then
    fx.DrawShadow(ADest,round(x)+ShadowOffset.X,round(y)+ShadowOffset.Y,ShadowRadius,ShadowColor, align);
  if OuterOutlineOnly then DoOutline;
  if texture <> nil then
  begin
    if ShaderActuallyActive then
      fx.DrawShaded(ADest,floor(x),floor(y), Shader, round(fx.TextSize.cy*0.05), texture, align)
    else
      fx.Draw(ADest,round(x),round(y), texture, align);
  end else
  begin
    if ShaderActuallyActive then
      fx.DrawShaded(ADest,floor(x),floor(y), Shader, round(fx.TextSize.cy*0.05), c, align)
    else
      fx.Draw(ADest,round(x),round(y), c, align);
  end;
  if not OuterOutlineOnly then DoOutline;
  fx.Free;
end;

constructor TBGRATextEffectFontRenderer.Create;
begin
  inherited Create;
  FShader := nil;
  FShaderOwner:= false;
  Init;
end;

constructor TBGRATextEffectFontRenderer.Create(AShader: TCustomPhongShading;
  AShaderOwner: boolean);
begin
  inherited Create;
  Init;
  FShader := AShader;
  FShaderOwner := AShaderOwner;
end;

destructor TBGRATextEffectFontRenderer.Destroy;
begin
  if FShaderOwner then FShader.Free;
  FVectorizedRenderer.Free;
  inherited Destroy;
end;

procedure TBGRATextEffectFontRenderer.TextOutAngle(ADest: TBGRACustomBitmap; x,
  y: single; orientation: integer; s: string; texture: IBGRAScanner;
  align: TAlignment);
begin
  VectorizedFontRenderer.TextOutAngle(ADest, x, y, orientation, s, texture, align);
end;

procedure TBGRATextEffectFontRenderer.TextOutAngle(ADest: TBGRACustomBitmap; x,
  y: single; orientation: integer; s: string; c: TBGRAPixel; align: TAlignment);
begin
  VectorizedFontRenderer.TextOutAngle(ADest, x, y, orientation, s, c, align);
end;

procedure TBGRATextEffectFontRenderer.TextOut(ADest: TBGRACustomBitmap; x,
  y: single; s: string; texture: IBGRAScanner; align: TAlignment);
begin
  if VectorizedFontNeeded then
    VectorizedFontRenderer.TextOut(ADest,x,y,s,texture,align)
  else
    InternalTextOut(ADest,x,y,s,BGRAPixelTransparent,texture,align);
end;

procedure TBGRATextEffectFontRenderer.TextOut(ADest: TBGRACustomBitmap; x,
  y: single; s: string; c: TBGRAPixel; align: TAlignment);
begin
  if VectorizedFontNeeded then
    VectorizedFontRenderer.TextOut(ADest,x,y,s,c,align)
  else
    InternalTextOut(ADest,x,y,s,c,nil,align);
end;

function TBGRATextEffectFontRenderer.TextSize(sUTF8: string): TSize;
begin
  if VectorizedFontNeeded then
    result := VectorizedFontRenderer.TextSize(sUTF8)
  else
  begin
    result := inherited TextSize(sUTF8);
  end;
end;

{ TBGRATextEffect }

function TBGRATextEffect.GetBounds: TRect;
begin
  if TextMask = nil then
    result := EmptyRect else
  with TextMaskOffset do
    result := rect(X,Y,X+TextMask.Width,Y+TextMask.Height);
end;

function TBGRATextEffect.GetMaskHeight: integer;
begin
  if FTextMask = nil then
    result := 0
  else
    result := FTextMask.Height;
end;

class function TBGRATextEffect.GetOutlineWidth: integer; static;
begin
  result := DefaultOutlineWidth;
end;

function TBGRATextEffect.GetShadowBounds(ARadius: integer): TRect;
begin
  result := Bounds;
  if (ARadius > 0) and not IsRectEmpty(result) then
  begin
    result.left -= ARadius;
    result.top -= ARadius;
    result.right += ARadius;
    result.bottom += ARadius;
  end;
end;

function TBGRATextEffect.GetMaskWidth: integer;
begin
  if FTextMask = nil then
    result := 0
  else
    result := FTextMask.Width;
end;

function TBGRATextEffect.GetTextHeight: integer;
begin
  result := FTextSize.cy;
end;

function TBGRATextEffect.GetTextWidth: integer;
begin
  result := FTextSize.cx;
end;

function TBGRATextEffect.DrawMaskMulticolored(ADest: TBGRACustomBitmap;
  AMask: TBGRACustomBitmap; X, Y: Integer; const AColors: array of TBGRAPixel
  ): TRect;
var
  scan: TBGRASolidColorMaskScanner;
  xb,yb,startX,numColor: integer;
  p0,p: PBGRAPixel;
  emptyCol, nextCol: boolean;
begin
  if (AMask = nil) or (length(AColors)=0) then
  begin
    result := EmptyRect;
    exit;
  end;
  if (length(AColors)=0) then
  begin
    result := DrawMask(ADest,AMask,X,Y,AColors[0]);
    exit;
  end;
  scan := TBGRASolidColorMaskScanner.Create(AMask,Point(-X,-Y),AColors[0]);
  numColor := 0;
  startX := -1;
  p0 := AMask.data;
  for xb := 0 to AMask.Width-1 do
  begin
    p := p0;

    if startX=-1 then
    begin
      emptyCol := true;
      for yb := AMask.Height-1 downto 0 do
      begin
        if (p^<>BGRABlack) then
        begin
          emptyCol := false;
          break;
        end;
        inc(p, AMask.Width);
      end;

      if not emptyCol then
      begin
        if startX=-1 then
          startX := xb;
      end else
      begin
        if startX<>-1 then
        begin
          ADest.FillRect(X+startX,Y,X+xb,Y+AMask.Height,scan,dmDrawWithTransparency);
          inc(numColor);
          if numColor = length(AColors) then
            numColor := 0;
          scan.Color := AColors[numColor];
          startX := -1;
        end;
      end;

    end else
    begin
      emptyCol := true;
      nextCol := true;
      for yb := AMask.Height-1 downto 0 do
      begin
        if (p^<>BGRABlack) then
        begin
          emptyCol := false;
          if ((p-1)^<>BGRABlack) then
          begin
            nextCol := false;
            break;
          end;
        end;
        inc(p, AMask.Width);
      end;
      if nextCol or emptyCol then
      begin
        ADest.FillRect(X+startX,Y,X+xb,Y+AMask.Height,scan,dmDrawWithTransparency);
        inc(numColor);
        if numColor = length(AColors) then
          numColor := 0;
        scan.Color := AColors[numColor];
        if emptyCol then startX := -1
         else startX := xb;
      end;
    end;

    inc(p0);
  end;
  if startX<>-1 then
    ADest.FillRect(X+startX,Y,X+AMask.Width,Y+AMask.Height,scan,dmDrawWithTransparency);
  scan.Free;
  result := rect(X,Y,X+AMask.Width,Y+AMask.Height);
end;

function TBGRATextEffect.DrawMask(ADest: TBGRACustomBitmap;
  AMask: TBGRACustomBitmap; X, Y: Integer; AColor: TBGRAPixel): TRect;
var
  scan: TBGRACustomScanner;
begin
  if AMask = nil then
  begin
    result := EmptyRect;
    exit;
  end;
  scan := TBGRASolidColorMaskScanner.Create(AMask,Point(-X,-Y),AColor);
  ADest.FillRect(X,Y,X+AMask.Width,Y+AMask.Height,scan,dmDrawWithTransparency);
  scan.Free;
  result := rect(X,Y,X+AMask.Width,Y+AMask.Height);
end;

function TBGRATextEffect.DrawMask(ADest: TBGRACustomBitmap;
  AMask: TBGRACustomBitmap; X, Y: Integer; ATexture: IBGRAScanner): TRect;
var
  scan: TBGRACustomScanner;
begin
  if AMask = nil then
  begin
    result := EmptyRect;
    exit;
  end;
  scan := TBGRATextureMaskScanner.Create(AMask,Point(-X,-Y),ATexture);
  ADest.FillRect(X,Y,X+AMask.Width,Y+AMask.Height,scan,dmDrawWithTransparency);
  scan.Free;
  result := rect(X,Y,X+AMask.Width,Y+AMask.Height);
end;

function TBGRATextEffect.InternalDrawShaded(ADest: TBGRACustomBitmap; X,
  Y: integer; Shader: TCustomPhongShading; Altitude: integer;
  AColor: TBGRAPixel; ATexture: IBGRAScanner; ARounded: Boolean): TRect;
var
  WithMargin,Map: TBGRACustomBitmap;
  p: PBGRAPixel;
  n,maxv: integer;
  v,blurRadius: single;
  iBlurRadius: integer;
begin
  if (FTextMask = nil) or (FTextMask.Width = 0) or (FTextMask.Height = 0) then
  begin
    result := EmptyRect;
    exit;
  end;

  if (FShadingMask <> nil) and ((FShadingAltitude <> Altitude) or (FShadingRounded <> ARounded)) then
    FreeAndNil(FShadingMask);

  if FShadingMask = nil then
  begin
    FShadingRounded := ARounded;
    FShadingAltitude := Altitude;

    if ARounded then blurRadius := Altitude
      else blurRadius := Altitude*0.5;

    iBlurRadius := ceil(blurRadius);

    WithMargin := BGRABitmapFactory.Create(FTextMask.Width+iBlurRadius*2, FTextMask.Height+iBlurRadius*2,BGRABlack);
    WithMargin.PutImage(iBlurRadius,iBlurRadius,FTextMask,dmSet);
    if (iBlurRadius <> blurRadius) and (blurRadius < 3) then
      Map := WithMargin.FilterBlurRadial(round(blurRadius*10),rbPrecise)
    else
      Map := WithMargin.FilterBlurRadial(iBlurRadius,rbFast);

    p := Map.Data;
    maxv := 0;
    for n := Map.NbPixels-1 downto 0 do
    begin
      if p^.green > maxv then
        maxv := p^.green;
      inc(p);
    end;

    if maxv > 0 then
    begin
      p := Map.Data;
      for n := Map.NbPixels-1 downto 0 do
      begin
        v := p^.green/maxv;
        if ARounded then
        begin
          if v <= 0.5 then
            v := v*v*2 else
            v := 1-(1-v)*(1-v)*2;
        end;
        p^ := MapHeightToBGRA( v, p^.alpha);
        inc(p);
      end;
    end;

    Map.ApplyMask(WithMargin);
    WithMargin.Free;
    BGRAReplace(Map, Map.GetPart(rect(iBlurRadius,iBlurRadius,Map.Width-iBlurRadius,Map.Height-iBlurRadius)));
    FShadingMask := Map;
  end;

  inc(X, FOffset.X);
  Inc(Y, FOffset.Y);
  if ATexture <> nil then
    Shader.DrawScan(ADest,FShadingMask,Altitude,X,Y, ATexture)
  else
    Shader.Draw(ADest,FShadingMask,Altitude,X,Y, AColor);
  result := rect(X,Y, X+FShadingMask.Width,Y+FShadingMask.Height);
end;

procedure TBGRATextEffect.InitImproveReadability(AText: string; Font: TFont;
  SubOffsetX, SubOffsetY: single);
var size: TSize;
  overhang: integer;
begin
  if SubOffsetX < 0 then SubOffsetX := 0;
  if SubOffsetY < 0 then SubOffsetY := 0;
  size := BGRATextSize(Font, fqFineAntialiasing, AText, FontAntialiasingLevel);
  FTextSize := size;
  if size.cy = 0 then FTextSize.cy := BGRATextSize(Font, fqFineAntialiasing, 'Hg', FontAntialiasingLevel).cy;
  overhang := size.cy div 2;
  size.cx += 2*overhang + ceil(SubOffsetX);
  size.cy += 2 + ceil(SubOffsetY);

  FOffset := Point(-overhang,-1); //include overhang
  FTextMask := BGRABitmapFactory.Create(size.cx,size.cy,BGRABlack);
  BGRATextOutImproveReadability(FTextMask, Font, overhang+SubOffsetX,1+SubOffsetY, AText, BGRAWhite, nil, taLeftJustify, irMask);
end;

function TBGRATextEffect.Draw(ADest: TBGRACustomBitmap; X, Y: integer;
  AColor: TBGRAPixel; AAlign: TAlignment): TRect;
begin
  Case AAlign of
  taRightJustify: result := Draw(ADest,X-TextSize.cx,Y,AColor);
  taCenter: result := Draw(ADest,X-TextSize.cx div 2,Y,AColor);
  else result := Draw(ADest,X,Y,AColor);
  end;
end;

function TBGRATextEffect.Draw(ADest: TBGRACustomBitmap; X, Y: integer;
  ATexture: IBGRAScanner; AAlign: TAlignment): TRect;
begin
  Case AAlign of
  taRightJustify: result := Draw(ADest,X-TextSize.cx,Y,ATexture);
  taCenter: result := Draw(ADest,X-TextSize.cx div 2,Y,ATexture);
  else result := Draw(ADest,X,Y,ATexture);
  end;
end;

function TBGRATextEffect.DrawShaded(ADest: TBGRACustomBitmap; X, Y: integer;
  Shader: TCustomPhongShading; Altitude: integer; AColor: TBGRAPixel;
  ARounded: Boolean): TRect;
begin
  result := InternalDrawShaded(ADest,X,Y,Shader,Altitude,AColor,nil,ARounded);
end;

function TBGRATextEffect.DrawShaded(ADest: TBGRACustomBitmap; X, Y: integer;
  Shader: TCustomPhongShading; Altitude: integer; ATexture: IBGRAScanner;
  ARounded: Boolean): TRect;
begin
  result := InternalDrawShaded(ADest,X,Y,Shader,Altitude,BGRAPixelTransparent,ATexture,ARounded);
end;

function TBGRATextEffect.DrawShaded(ADest: TBGRACustomBitmap; X, Y: integer;
  Shader: TCustomPhongShading; Altitude: integer; AColor: TBGRAPixel;
  AAlign: TAlignment; ARounded: Boolean): TRect;
begin
  Case AAlign of
  taLeftJustify: result := DrawShaded(ADest,X,Y,Shader,Altitude,AColor,ARounded);
  taRightJustify: result := DrawShaded(ADest,X-TextSize.cx,Y,Shader,Altitude,AColor,ARounded);
  taCenter: result := DrawShaded(ADest,X-TextSize.cx div 2,Y,Shader,Altitude,AColor,ARounded);
  else
    result := EmptyRect;
  end;
end;

function TBGRATextEffect.DrawShaded(ADest: TBGRACustomBitmap; X, Y: integer;
  Shader: TCustomPhongShading; Altitude: integer; ATexture: IBGRAScanner;
  AAlign: TAlignment; ARounded: Boolean): TRect;
begin
  Case AAlign of
  taLeftJustify: result := DrawShaded(ADest,X,Y,Shader,Altitude,ATexture,ARounded);
  taRightJustify: result := DrawShaded(ADest,X-TextSize.cx,Y,Shader,Altitude,ATexture,ARounded);
  taCenter: result := DrawShaded(ADest,X-TextSize.cx div 2,Y,Shader,Altitude,ATexture,ARounded);
  else
    result := EmptyRect;
  end;
end;

constructor TBGRATextEffect.Create(AText: string; Font: TFont;
  Antialiasing: boolean; SubOffsetX,SubOffsetY: single);
begin
  Init(AText, Font, Antialiasing, SubOffsetX, SubOffsetY, 0,0);
end;

constructor TBGRATextEffect.Create(AText: string; Font: TFont;
  Antialiasing: boolean; SubOffsetX, SubOffsetY: single; GrainX, GrainY: Integer
  );
begin
  Init(AText, Font, Antialiasing, SubOffsetX, SubOffsetY, GrainX, GrainY);
end;

constructor TBGRATextEffect.Create(AText: string; AFontName: string;
  AFullHeight: integer; Antialiasing: boolean);
begin
  InitWithFontName(AText, AFontName, AFullHeight, [], Antialiasing, 0, 0);
end;

constructor TBGRATextEffect.Create(AText: string; AFontName: string;
  AFullHeight: integer; Antialiasing: boolean; SubOffsetX, SubOffsetY: single);
begin
  InitWithFontName(AText, AFontName, AFullHeight, [], Antialiasing, SubOffsetX, SubOffsetY);
end;

constructor TBGRATextEffect.Create(AText: string; AFontName: string;
  AFullHeight: integer; AStyle: TFontStyles; Antialiasing: boolean);
begin
  InitWithFontName(AText, AFontName, AFullHeight, AStyle, Antialiasing, 0, 0);
end;

constructor TBGRATextEffect.Create(AText: string; AFontName: string;
  AFullHeight: integer; AStyle: TFontStyles; Antialiasing: boolean; SubOffsetX,
  SubOffsetY: single);
begin
  InitWithFontName(AText, AFontName, AFullHeight, AStyle, Antialiasing, SubOffsetX, SubOffsetY);
end;

constructor TBGRATextEffect.Create(AMask: TBGRACustomBitmap; AMaskOwner: boolean; AWidth,
  AHeight: integer; AOffset: TPoint);
begin
  FTextSize := Size(AWidth,AHeight);
  FOffset := AOffset;
  if not AMaskOwner then
    FTextMask := AMask.Duplicate()
  else
    FTextMask := AMask;
end;

procedure TBGRATextEffect.Init(AText: string; Font: TFont; Antialiasing: boolean; SubOffsetX,SubOffsetY: single; GrainX, GrainY: Integer);
const FXAntialiasingLevel = FontAntialiasingLevel;
var temp: TBGRACustomBitmap;
    size: TSize;
    p: PBGRAPixel;
    n,v,maxAlpha: integer;
    alpha: byte;
    sizeX,sizeY: integer;
    onePixel: integer;
    quality: TBGRAFontQuality;
    iSubX,iSubY: integer;
begin
  if Antialiasing and Assigned(BGRATextOutImproveReadabilityProc) then
  begin
    InitImproveReadability(AText, Font, SubOffsetX,SubOffsetY);
    exit;
  end;
  if Antialiasing then
    quality := fqFineAntialiasing
  else
    quality := fqSystem;
  size := BGRAOriginalTextSize(Font,quality,AText,FXAntialiasingLevel);
  if (size.cx = 0) or (size.cy = 0) then
  begin
    size := BGRATextSize(Font,quality,'Hg',FXAntialiasingLevel);
    FTextSize.cx := 0;
    FTextSize.cy := size.cy;
    FOffset := Point(0,0);
    exit;
  end;
  FTextSize := size;

  sizeX := size.cx+size.cy;
  sizeY := size.cy;

  iSubX := 0;
  iSubY := 0;
  if SubOffsetX < 0 then SubOffsetX := 0;
  if SubOffsetY < 0 then SubOffsetY := 0;

  if Antialiasing then
  begin
    sizeX := (sizeX + FXAntialiasingLevel-1);
    sizeX -= sizeX mod FXAntialiasingLevel;

    sizeY := (sizeY + FXAntialiasingLevel-1);
    sizeY -= sizeY mod FXAntialiasingLevel;

    if SubOffsetX <> 0 then
    begin
      sizeX += ceil(SubOffsetX*FXAntialiasingLevel);
      iSubX := round(SubOffsetX*FXAntialiasingLevel);
    end;
    if SubOffsetY <> 0 then
    begin
      sizeY += ceil(SubOffsetY*FXAntialiasingLevel);
      iSubY := round(SubOffsetY*FXAntialiasingLevel);
    end;

    OnePixel := FXAntialiasingLevel;
  end else
  begin
    OnePixel := 1;

    if SubOffsetX <> 0 then
    begin
      iSubX := round(SubOffsetX);
      sizeX += iSubX;
    end;
    if SubOffsetY <> 0 then
    begin
      iSubY := round(SubOffsetY);
      sizeY += iSubY;
    end;
  end;
  FOffset := Point(-size.cy div 2,-OnePixel); //include overhang

  if GrainX > 0 then
  begin
    SizeX := SizeX+ (GrainX-1);
    SizeX -= SizeX mod GrainX;
  end;
  if GrainY > 0 then
  begin
    SizeY := SizeY+ (GrainY-1);
    SizeY -= SizeY mod GrainY;
  end;
  temp := BGRABitmapFactory.Create(sizeX, sizeY+2*OnePixel,clBlack);
  temp.Canvas.Font := Font;
  temp.Canvas.Font.Height := Font.Height*OnePixel;
  temp.Canvas.Font.Color := clWhite;
  temp.Canvas.Font.Quality := FontDefaultQuality;
  temp.Canvas.Brush.Style := bsClear;
  temp.Canvas.TextOut(-FOffset.X+iSubX, -FOffset.Y+iSubY, AText);

  if Antialiasing then
  begin
    FTextSize.cx := round(FTextSize.cx/FXAntialiasingLevel);
    FTextSize.cy := round(FTextSize.cy/FXAntialiasingLevel);
    FOffset := Point(round(FOffset.X/FXAntialiasingLevel),round(FOffset.Y/FXAntialiasingLevel));

    FTextMask := temp.Resample(round(temp.width/FXAntialiasingLevel),round(temp.Height/FXAntialiasingLevel),rmSimpleStretch);

    maxAlpha := 0;
    p := FTextMask.Data;
    for n := FTextMask.NbPixels - 1 downto 0 do
    begin
      alpha    := P^.green;
      if alpha > maxAlpha then maxAlpha := alpha;
      Inc(p);
    end;
    if maxAlpha <> 0 then
    begin
      p := FTextMask.Data;
      for n := FTextMask.NbPixels - 1 downto 0 do
      begin
        v:= integer(p^.green * 255) div maxAlpha;
        p^.red := v;
        p^.green := v;
        p^.blue := v;
        Inc(p);
      end;
    end;
    temp.Free;
  end
  else
  begin
    FTextMask := temp;
    p := FTextMask.data;
    for n := FTextMask.NbPixels-1 downto 0 do
    begin
      alpha := GammaExpansionTab[P^.green] shr 8;
      p^.green := alpha;
      p^.red := alpha;
      p^.blue := alpha;
    end;
  end;
end;

procedure TBGRATextEffect.InitWithFontName(AText: string; AFontName: string;
  AFullHeight: integer; AStyle: TFontStyles; Antialiasing: boolean; SubOffsetX, SubOffsetY: single);
var lFont: TFont;
begin
  lFont := TFont.Create;
  lFont.Name := AFontName;
  lFont.Height := AFullHeight * FontFullHeightSign;
  lFont.Style := AStyle;
  Init(AText, lFont, Antialiasing, SubOffsetX, SubOffsetY, 0,0);
  lFont.Free;
end;

constructor TBGRATextEffect.Create(AText: string; Font: TFont;
  Antialiasing: boolean);
begin
  Init(AText, Font, Antialiasing, 0,0,0,0);
end;

procedure TBGRATextEffect.ApplySphere;
var sphere: TBGRACustomBitmap;
begin
  if FTextMask = nil then exit;
  FreeAndNil(FOutlineMask);
  FreeAndNil(FShadowMask);
  FShadowRadius := 0;
  sphere := FTextMask.FilterSphere;
  FTextMask.Fill(BGRABlack);
  FTextMask.PutImage(0,0,sphere,dmDrawWithTransparency);
  sphere.Free;
end;

procedure TBGRATextEffect.ApplyVerticalCylinder;
begin
  if FTextMask = nil then exit;
  FreeAndNil(FOutlineMask);
  FreeAndNil(FShadowMask);
  FShadowRadius := 0;
  BGRAReplace(FTextMask,FTextMask.FilterCylinder);
end;

procedure TBGRATextEffect.ApplyHorizontalCylinder;
begin
  if FTextMask = nil then exit;
  FreeAndNil(FOutlineMask);
  FreeAndNil(FShadowMask);
  FShadowRadius := 0;
  BGRAReplace(FTextMask,FTextMask.RotateCW);
  BGRAReplace(FTextMask,FTextMask.FilterCylinder);
  BGRAReplace(FTextMask,FTextMask.RotateCCW);
end;

function TBGRATextEffect.Draw(ADest: TBGRACustomBitmap; X, Y: integer;
  AColor: TBGRAPixel): TRect;
begin
  result := DrawMask(ADest,FTextMask,X+FOffset.X,Y+FOffset.Y,AColor);
end;

function TBGRATextEffect.Draw(ADest: TBGRACustomBitmap; X, Y: integer;
  ATexture: IBGRAScanner): TRect;
begin
  result := DrawMask(ADest,FTextMask,X+FOffset.X,Y+FOffset.Y,ATexture);
end;

function TBGRATextEffect.DrawMulticolored(ADest: TBGRACustomBitmap; X,
  Y: integer; const AColors: array of TBGRAPixel): TRect;
begin
  result := DrawMaskMulticolored(ADest,FTextMask,X+FOffset.X,Y+FOffset.Y,AColors);
end;

function TBGRATextEffect.DrawMulticolored(ADest: TBGRACustomBitmap; X,
  Y: integer; const AColors: array of TBGRAPixel; AAlign: TAlignment): TRect;
begin
  Case AAlign of
  taRightJustify: result := DrawMulticolored(ADest,X-TextSize.cx,Y,AColors);
  taCenter: result := DrawMulticolored(ADest,X-TextSize.cx div 2,Y,AColors);
  else result := DrawMulticolored(ADest,X,Y,AColors);
  end;
end;

function TBGRATextEffect.DrawOutline(ADest: TBGRACustomBitmap; X, Y: integer;
  AColor: TBGRAPixel): TRect;
begin
  if (FTextMask = nil) or (FTextMask.Width = 0) or (FTextMask.Height = 0) then
  begin
    result := EmptyRect;
    exit;
  end;
  if FOutlineMask = nil then
  begin
    FOutlineMask := FTextMask.FilterContour;
    FOutlineMask.LinearNegative;
  end;
  result := DrawMask(ADest,FOutlineMask,X+FOffset.X,Y+FOffset.Y,AColor);
end;

function TBGRATextEffect.DrawOutline(ADest: TBGRACustomBitmap; X, Y: integer;
  ATexture: IBGRAScanner): TRect;
begin
  if (FTextMask = nil) or (FTextMask.Width = 0) or (FTextMask.Height = 0) then
  begin
    result := EmptyRect;
    exit;
  end;
  if FOutlineMask = nil then
  begin
    FOutlineMask := FTextMask.FilterContour;
    FOutlineMask.LinearNegative;
  end;
  result := DrawMask(ADest,FOutlineMask,X+FOffset.X,Y+FOffset.Y,ATexture);
end;

function TBGRATextEffect.DrawOutline(ADest: TBGRACustomBitmap; X, Y: integer;
  AColor: TBGRAPixel; AAlign: TAlignment): TRect;
begin
  Case AAlign of
  taRightJustify: result := DrawOutline(ADest,X-TextSize.cx,Y,AColor);
  taCenter: result := DrawOutline(ADest,X-TextSize.cx div 2,Y,AColor);
  else result := DrawOutline(ADest,X,Y,AColor);
  end;
end;

function TBGRATextEffect.DrawOutline(ADest: TBGRACustomBitmap; X, Y: integer;
  ATexture: IBGRAScanner; AAlign: TAlignment): TRect;
begin
  Case AAlign of
  taRightJustify: result := DrawOutline(ADest,X-TextSize.cx,Y,ATexture);
  taCenter: result := DrawOutline(ADest,X-TextSize.cx div 2,Y,ATexture);
  else result := DrawOutline(ADest,X,Y,ATexture);
  end;
end;

function TBGRATextEffect.DrawShadow(ADest: TBGRACustomBitmap; X, Y,
  Radius: integer; AColor: TBGRAPixel): TRect;
begin
  if (Radius <= 0) or (FTextMask = nil) or (FTextMask.Width = 0) or (FTextMask.Height = 0) then
  begin
    result := Draw(ADest,X,Y,AColor);
    exit;
  end;
  if FShadowRadius <> Radius then
  begin
    FShadowRadius := Radius;
    FreeAndNil(FShadowMask);
    FShadowMask := BGRABitmapFactory.Create(FTextMask.Width+Radius*2,FTextMask.Height+Radius*2,BGRABlack);
    FShadowMask.PutImage(Radius,Radius,FTextMask,dmSet);
    BGRAReplace(FShadowMask, FShadowMask.FilterBlurRadial(Radius,rbFast));
  end;
  Inc(X,FOffset.X-Radius);
  Inc(Y,FOffset.Y-Radius);
  DrawMask(ADest,FShadowMask,X,Y,AColor);
  result := rect(X,Y,X+FShadowMask.Width,Y+FShadowMask.Height);
end;

function TBGRATextEffect.DrawShadow(ADest: TBGRACustomBitmap; X, Y,
  Radius: integer; AColor: TBGRAPixel; AAlign: TAlignment): TRect;
begin
  Case AAlign of
  taRightJustify: result := DrawShadow(ADest,X-TextSize.cx,Y,Radius,AColor);
  taCenter: result := DrawShadow(ADest,X-TextSize.cx div 2,Y,Radius,AColor);
  else result := DrawShadow(ADest,X,Y,Radius,AColor);
  end;
end;

destructor TBGRATextEffect.Destroy;
begin
  FShadowMask.free;
  textMask.Free;
  FOutlineMask.Free;
  FShadingMask.Free;
  inherited Destroy;
end;

initialization

  BGRATextOutImproveReadabilityProc := @BGRATextOutImproveReadability;

end.

