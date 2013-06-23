unit BGRATextFX;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Types, BGRABitmapTypes, BGRAPhongTypes;

type

  { TBGRATextEffect }

  TBGRATextEffect = class
  private
    function GetBounds: TRect;
    function GetHeight: integer;
    function GetShadowBounds(ARadius: integer): TRect;
    function GetWidth: integer;
  protected
    FTextMask: TBGRACustomBitmap;
    FShadowRadius: integer;
    FOutlineMask, FShadowMask, FShadingMask : TBGRACustomBitmap;
    FShadingAltitude: integer;
    FShadingRounded: boolean;
    FWidth,FHeight: integer;
    FOffset: TPoint;
    procedure DrawMaskMulticolored(ADest: TBGRACustomBitmap; AMask: TBGRACustomBitmap; X,Y: Integer; const AColors: array of TBGRAPixel);
    procedure DrawMask(ADest: TBGRACustomBitmap; AMask: TBGRACustomBitmap; X,Y: Integer; AColor: TBGRAPixel);
    procedure DrawMask(ADest: TBGRACustomBitmap; AMask: TBGRACustomBitmap; X,Y: Integer; ATexture: IBGRAScanner);
    function InternalDrawShaded(ADest: TBGRACustomBitmap; X,Y: integer; Shader: TCustomPhongShading; Altitude: integer; AColor: TBGRAPixel; ATexture: IBGRAScanner; ARounded: Boolean): TRect;
    procedure Init(AText: string; Font: TFont; Antialiasing: boolean; SubOffsetX,SubOffsetY: single; GrainX, GrainY: Integer);
  public
    constructor Create(AText: string; Font: TFont; Antialiasing: boolean);
    constructor Create(AText: string; Font: TFont; Antialiasing: boolean; SubOffsetX,SubOffsetY: single);
    constructor Create(AText: string; Font: TFont; Antialiasing: boolean; SubOffsetX,SubOffsetY: single; GrainX, GrainY: Integer);
    procedure ApplySphere;
    procedure ApplyVerticalCylinder;
    procedure ApplyHorizontalCylinder;
    procedure Draw(ADest: TBGRACustomBitmap; X,Y: integer; AColor: TBGRAPixel);
    procedure Draw(ADest: TBGRACustomBitmap; X,Y: integer; ATexture: IBGRAScanner);
    procedure Draw(ADest: TBGRACustomBitmap; X, Y: integer; AColor: TBGRAPixel; AAlign: TAlignment);
    procedure Draw(ADest: TBGRACustomBitmap; X, Y: integer; ATexture: IBGRAScanner; AAlign: TAlignment);

    function DrawShaded(ADest: TBGRACustomBitmap; X,Y: integer; Shader: TCustomPhongShading; Altitude: integer; AColor: TBGRAPixel; ARounded: Boolean = true): TRect;
    function DrawShaded(ADest: TBGRACustomBitmap; X,Y: integer; Shader: TCustomPhongShading; Altitude: integer; ATexture: IBGRAScanner; ARounded: Boolean = true): TRect;
    function DrawShaded(ADest: TBGRACustomBitmap; X, Y: integer; Shader: TCustomPhongShading; Altitude: integer; AColor: TBGRAPixel; AAlign: TAlignment; ARounded: Boolean = true): TRect;
    function DrawShaded(ADest: TBGRACustomBitmap; X, Y: integer; Shader: TCustomPhongShading; Altitude: integer; ATexture: IBGRAScanner; AAlign: TAlignment; ARounded: Boolean = true): TRect;

    procedure DrawMulticolored(ADest: TBGRACustomBitmap; X,Y: integer; const AColors: array of TBGRAPixel);
    procedure DrawMulticolored(ADest: TBGRACustomBitmap; X,Y: integer; const AColors: array of TBGRAPixel; AAlign: TAlignment);
    procedure DrawOutline(ADest: TBGRACustomBitmap; X,Y: integer; AColor: TBGRAPixel);
    procedure DrawOutline(ADest: TBGRACustomBitmap; X,Y: integer; ATexture: IBGRAScanner);
    procedure DrawOutline(ADest: TBGRACustomBitmap; X,Y: integer; AColor: TBGRAPixel; AAlign: TAlignment);
    procedure DrawOutline(ADest: TBGRACustomBitmap; X,Y: integer; ATexture: IBGRAScanner; AAlign: TAlignment);
    procedure DrawShadow(ADest: TBGRACustomBitmap; X,Y,Radius: integer; AColor: TBGRAPixel);
    procedure DrawShadow(ADest: TBGRACustomBitmap; X,Y,Radius: integer; AColor: TBGRAPixel; AAlign: TAlignment);
    destructor Destroy; override;
    property TextMask: TBGRACustomBitmap read FTextMask;
    property TextMaskOffset: TPoint read FOffset;
    property Width: integer read GetWidth;
    property Height: integer read GetHeight;
    property Bounds: TRect read GetBounds;
    property ShadowBounds[ARadius: integer]: TRect read GetShadowBounds;
  end;

function TextShadow(AWidth,AHeight: Integer; AText: String; AFontHeight: Integer; ATextColor,AShadowColor: TBGRAPixel;
    AOffSetX,AOffSetY: Integer; ARadius: Integer = 0; AFontStyle: TFontStyles = []; AFontName: String = 'Default'; AShowText: Boolean = True; AFontQuality: TBGRAFontQuality = fqFineAntialiasing): TBGRACustomBitmap;

procedure BGRATextOutImproveReadability(bmp: TBGRACustomBitmap; AFont: TFont; xf,yf: single; text: string; color: TBGRAPixel; tex: IBGRAScanner; align: TAlignment; useClearType: boolean; ClearTypeRGBOrder: boolean);

implementation

uses BGRAGradientScanner, BGRAText, GraphType, Math;

procedure BGRATextOutImproveReadability(bmp: TBGRACustomBitmap; AFont: TFont; xf,yf: single; text: string; color: TBGRAPixel; tex: IBGRAScanner; align: TAlignment; useClearType: boolean; ClearTypeRGBOrder: boolean);
var
  metric: TFontPixelMetric;
  deltaX: single;
  x,y,yb,cury,fromy: integer;
  toAdd: integer;
  lines: array[0..3] of integer;
  parts: array[0..3] of TBGRACustomBitmap;
  n,nbLines,v: integer;
  alphaMax: byte;
  ptrPart: TBGRACustomBitmap;
  pmask: PBGRAPixel;
  fx: TBGRATextEffect;
  FxFont: TFont;
  prevCenter, newCenter, diffCenter: single;
  xThird: integer;

begin
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
        BGRATextOut(bmp, AFont, fqFineClearTypeRGB, xf,yf, text, color, tex, align) else
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
        parts[yb] := ptrPart.Resample(round(ptrPart.Width/FontAntialiasingLevel*3),round(ptrPart.Height/FontAntialiasingLevel),rmSimpleStretch)
      else
        parts[yb] := ptrPart.Resample(round(ptrPart.Width/FontAntialiasingLevel),round(ptrPart.Height/FontAntialiasingLevel),rmSimpleStretch);
      ptrPart.Free;

      if alphaMax < 255 then
      begin
        pmask := parts[yb].data;
        for n := parts[yb].NbPixels-1 downto 0 do
        begin
          v := pmask^.green;
          if v > alphaMax then alphaMax := v;
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
    taCenter: xThird:= xThird+round(((fx.TextMaskOffset.x-fx.Width/2)/FontAntialiasingLevel+deltaX)*3);
    taRightJustify: xThird:= xThird+round(((fx.TextMaskOffset.x-fx.Width)/FontAntialiasingLevel+deltaX)*3);
    else xThird:= xThird+round((fx.TextMaskOffset.x/FontAntialiasingLevel+deltaX)*3);
    end;
  end else
  begin
    case align of
    taCenter: x:= x+round((fx.TextMaskOffset.x-fx.Width/2)/FontAntialiasingLevel);
    taRightJustify: x:= x+round((fx.TextMaskOffset.x-fx.Width)/FontAntialiasingLevel);
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
        v := integer(pmask^.green)*255 div alphaMax;
        if v > 255 then v := 255;
        pmask^.green := v;
        pmask^.red := v;
        pmask^.blue := v;
        inc(pmask);
      end;
    end;
    if useClearType then
    begin
      if tex <> nil then
        bmp.FillClearTypeMask(x,cury,xThird,parts[yb],tex,ClearTypeRGBOrder) else
        bmp.FillClearTypeMask(x,cury,xThird,parts[yb],color,ClearTypeRGBOrder);
    end else
    begin
      if tex <> nil then
        bmp.FillMask(x,cury,parts[yb],tex) else
        bmp.FillMask(x,cury,parts[yb],color);
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

{ TBGRATextEffect }

function TBGRATextEffect.GetBounds: TRect;
begin
  if TextMask = nil then
    result := EmptyRect else
  with TextMaskOffset do
    result := rect(X,Y,X+TextMask.Width,Y+TextMask.Height);
end;

function TBGRATextEffect.GetHeight: integer;
begin
  result := FHeight;
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

function TBGRATextEffect.GetWidth: integer;
begin
  result := FWidth;
end;

procedure TBGRATextEffect.DrawMaskMulticolored(ADest: TBGRACustomBitmap;
  AMask: TBGRACustomBitmap; X, Y: Integer; const AColors: array of TBGRAPixel);
var
  scan: TBGRASolidColorMaskScanner;
  xb,yb,startX,numColor: integer;
  p0,p: PBGRAPixel;
  emptyCol, nextCol: boolean;
begin
  if (AMask = nil) or (length(AColors)=0) then exit;
  if (length(AColors)=0) then
  begin
    DrawMask(ADest,AMask,X,Y,AColors[0]);
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
end;

procedure TBGRATextEffect.DrawMask(ADest: TBGRACustomBitmap; AMask: TBGRACustomBitmap; X,
  Y: Integer; AColor: TBGRAPixel);
var
  scan: TBGRACustomScanner;
begin
  if AMask = nil then exit;
  scan := TBGRASolidColorMaskScanner.Create(AMask,Point(-X,-Y),AColor);
  ADest.FillRect(X,Y,X+AMask.Width,Y+AMask.Height,scan,dmDrawWithTransparency);
  scan.Free;
end;

procedure TBGRATextEffect.DrawMask(ADest: TBGRACustomBitmap; AMask: TBGRACustomBitmap; X,
  Y: Integer; ATexture: IBGRAScanner);
var
  scan: TBGRACustomScanner;
begin
  if AMask = nil then exit;
  scan := TBGRATextureMaskScanner.Create(AMask,Point(-X,-Y),ATexture);
  ADest.FillRect(X,Y,X+AMask.Width,Y+AMask.Height,scan,dmDrawWithTransparency);
  scan.Free;
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
  if FTextMask = nil then
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

  if ATexture <> nil then
    Shader.DrawScan(ADest,FShadingMask,Altitude,X+FOffset.X,Y+FOffset.Y, ATexture)
  else
    Shader.Draw(ADest,FShadingMask,Altitude,X+FOffset.X,Y+FOffset.Y, AColor);
  result := rect(X+FOffset.X,Y+FOffset.Y, X+FOffset.X+FShadingMask.Width,Y+FOffset.Y+FShadingMask.Height);
end;

procedure TBGRATextEffect.Draw(ADest: TBGRACustomBitmap; X, Y: integer;
  AColor: TBGRAPixel; AAlign: TAlignment);
begin
  Case AAlign of
  taLeftJustify: Draw(ADest,X,Y,AColor);
  taRightJustify: Draw(ADest,X-Width,Y,AColor);
  taCenter: Draw(ADest,X-Width div 2,Y,AColor);
  end;
end;

procedure TBGRATextEffect.Draw(ADest: TBGRACustomBitmap; X, Y: integer;
  ATexture: IBGRAScanner; AAlign: TAlignment);
begin
  Case AAlign of
  taLeftJustify: Draw(ADest,X,Y,ATexture);
  taRightJustify: Draw(ADest,X-Width,Y,ATexture);
  taCenter: Draw(ADest,X-Width div 2,Y,ATexture);
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
  taRightJustify: result := DrawShaded(ADest,X-Width,Y,Shader,Altitude,AColor,ARounded);
  taCenter: result := DrawShaded(ADest,X-Width div 2,Y,Shader,Altitude,AColor,ARounded);
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
  taRightJustify: result := DrawShaded(ADest,X-Width,Y,Shader,Altitude,ATexture,ARounded);
  taCenter: result := DrawShaded(ADest,X-Width div 2,Y,Shader,Altitude,ATexture,ARounded);
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

procedure TBGRATextEffect.Init(AText: string; Font: TFont;
  Antialiasing: boolean; SubOffsetX,SubOffsetY: single; GrainX, GrainY: Integer);
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
  if Antialiasing then
    quality := fqFineAntialiasing
  else
    quality := fqSystem;
  size := BGRAOriginalTextSize(Font,quality,AText,FontAntialiasingLevel);
  if (size.cx = 0) or (size.cy = 0) then
  begin
    size := BGRATextSize(Font,quality,'Hg',FontAntialiasingLevel);
    FWidth := 0;
    FHeight := size.cy;
    FOffset := Point(0,0);
    exit;
  end;

  sizeX := size.cx+size.cy;
  sizeY := size.cy;

  iSubX := 0;
  iSubY := 0;
  if SubOffsetX < 0 then SubOffsetX := 0;
  if SubOffsetY < 0 then SubOffsetY := 0;

  if Antialiasing then
  begin
    sizeX := (sizeX + FontAntialiasingLevel-1);
    sizeX -= sizeX mod FontAntialiasingLevel;

    sizeY := (sizeY + FontAntialiasingLevel-1);
    sizeY -= sizeY mod FontAntialiasingLevel;

    if SubOffsetX <> 0 then
    begin
      sizeX += ceil(SubOffsetX*FontAntialiasingLevel);
      iSubX := round(SubOffsetX*FontAntialiasingLevel);
    end;
    if SubOffsetY <> 0 then
    begin
      sizeY += ceil(SubOffsetY*FontAntialiasingLevel);
      iSubY := round(SubOffsetY*FontAntialiasingLevel);
    end;

    OnePixel := FontAntialiasingLevel;
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
    FWidth := round(size.cx/FontAntialiasingLevel);
    FHeight := round(size.cy/FontAntialiasingLevel);
    FOffset := Point(round(FOffset.X/FontAntialiasingLevel),round(FOffset.Y/FontAntialiasingLevel));

    FTextMask := temp.Resample(round(temp.width/FontAntialiasingLevel),round(temp.Height/FontAntialiasingLevel),rmSimpleStretch);

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
    FWidth := size.cx;
    FHeight := size.cy;

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

procedure TBGRATextEffect.Draw(ADest: TBGRACustomBitmap; X, Y: integer;
  AColor: TBGRAPixel);
begin
  if FTextMask = nil then exit;
  DrawMask(ADest,FTextMask,X+FOffset.X,Y+FOffset.Y,AColor);
end;

procedure TBGRATextEffect.Draw(ADest: TBGRACustomBitmap; X, Y: integer;
  ATexture: IBGRAScanner);
begin
  if FTextMask = nil then exit;
  DrawMask(ADest,FTextMask,X+FOffset.X,Y+FOffset.Y,ATexture);
end;

procedure TBGRATextEffect.DrawMulticolored(ADest: TBGRACustomBitmap; X, Y: integer;
  const AColors: array of TBGRAPixel);
begin
  if FTextMask = nil then exit;
  DrawMaskMulticolored(ADest,FTextMask,X+FOffset.X,Y+FOffset.Y,AColors);
end;

procedure TBGRATextEffect.DrawMulticolored(ADest: TBGRACustomBitmap; X,
  Y: integer; const AColors: array of TBGRAPixel; AAlign: TAlignment);
begin
  Case AAlign of
  taLeftJustify: DrawMulticolored(ADest,X,Y,AColors);
  taRightJustify: DrawMulticolored(ADest,X-Width,Y,AColors);
  taCenter: DrawMulticolored(ADest,X-Width div 2,Y,AColors);
  end;
end;

procedure TBGRATextEffect.DrawOutline(ADest: TBGRACustomBitmap; X, Y: integer;
  AColor: TBGRAPixel);
begin
  if FTextMask = nil then exit;
  if FOutlineMask = nil then
  begin
    FOutlineMask := FTextMask.FilterContour;
    FOutlineMask.LinearNegative;
  end;
  DrawMask(ADest,FOutlineMask,X+FOffset.X,Y+FOffset.Y,AColor);
end;

procedure TBGRATextEffect.DrawOutline(ADest: TBGRACustomBitmap; X, Y: integer;
  ATexture: IBGRAScanner);
begin
  if FTextMask = nil then exit;
  if FOutlineMask = nil then
  begin
    FOutlineMask := FTextMask.FilterContour;
    FOutlineMask.LinearNegative;
  end;
  DrawMask(ADest,FOutlineMask,X+FOffset.X,Y+FOffset.Y,ATexture);
end;

procedure TBGRATextEffect.DrawOutline(ADest: TBGRACustomBitmap; X, Y: integer;
  AColor: TBGRAPixel; AAlign: TAlignment);
begin
  Case AAlign of
  taLeftJustify: DrawOutline(ADest,X,Y,AColor);
  taRightJustify: DrawOutline(ADest,X-Width,Y,AColor);
  taCenter: DrawOutline(ADest,X-Width div 2,Y,AColor);
  end;
end;

procedure TBGRATextEffect.DrawOutline(ADest: TBGRACustomBitmap; X, Y: integer;
  ATexture: IBGRAScanner; AAlign: TAlignment);
begin
  Case AAlign of
  taLeftJustify: DrawOutline(ADest,X,Y,ATexture);
  taRightJustify: DrawOutline(ADest,X-Width,Y,ATexture);
  taCenter: DrawOutline(ADest,X-Width div 2,Y,ATexture);
  end;
end;

procedure TBGRATextEffect.DrawShadow(ADest: TBGRACustomBitmap; X, Y,Radius: integer;
  AColor: TBGRAPixel);
begin
  if Radius <= 0 then
  begin
    Draw(ADest,X,Y,AColor);
    exit;
  end;
  if FTextMask = nil then exit;
  if FShadowRadius <> Radius then
  begin
    FShadowRadius := Radius;
    FreeAndNil(FShadowMask);
    FShadowMask := BGRABitmapFactory.Create(FTextMask.Width+Radius*2,FTextMask.Height+Radius*2,BGRABlack);
    FShadowMask.PutImage(Radius,Radius,FTextMask,dmSet);
    BGRAReplace(FShadowMask, FShadowMask.FilterBlurRadial(Radius,rbFast));
  end;
  DrawMask(ADest,FShadowMask,X-Radius+FOffset.X,Y-Radius+FOffset.Y,AColor)
end;

procedure TBGRATextEffect.DrawShadow(ADest: TBGRACustomBitmap; X, Y,
  Radius: integer; AColor: TBGRAPixel; AAlign: TAlignment);
begin
  Case AAlign of
  taLeftJustify: DrawShadow(ADest,X,Y,Radius,AColor);
  taRightJustify: DrawShadow(ADest,X-Width,Y,Radius,AColor);
  taCenter: DrawShadow(ADest,X-Width div 2,Y,Radius,AColor);
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

end.

