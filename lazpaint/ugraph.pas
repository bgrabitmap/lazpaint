unit UGraph;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, bgrabitmap, bgrabitmaptypes, LazPaintType, Graphics, BGRALayers, LCLType,
  BCButton;

const FrameDashLength = 4;
  NicePointMaxRadius = 4;

function RectUnion(const rect1,Rect2: TRect): TRect;
function RectOfs(const ARect: TRect; ofsX,ofsY: integer): TRect;
function GetShapeBounds(const pts: array of TPointF; width: single): TRect;
function DoPixelate(source: TBGRABitmap; pixelSize: integer; quality: string): TBGRABitmap;
procedure DrawCheckers(bmp : TBGRABitmap; ARect: TRect);
procedure DrawGrid(bmp: TBGRABitmap; sizex,sizey: single);
function ComputeAngle(dx,dy: single): single;
function GetSelectionCenter(bmp: TBGRABitmap): TPointF;
procedure ComputeSelectionMask(image: TBGRABitmap; destMask: TBGRABitmap; ARect: TRect);
procedure SubstractMask(image: TBGRABitmap; DestX,DestY: Integer; mask: TBGRABitmap; SourceMaskRect: TRect);
function NicePointBounds(x,y: single): TRect;
function NicePoint(bmp: TBGRABitmap; x,y: single; alpha: byte = 192):TRect; overload;
function NicePoint(bmp: TBGRABitmap; ptF: TPointF; alpha: byte = 192):TRect; overload;
procedure NiceLine(bmp: TBGRABitmap; x1,y1,x2,y2: single; alpha: byte = 192);
function NiceText(bmp: TBGRABitmap; x,y,bmpWidth,bmpHeight: integer; s: string; align: TAlignment = taLeftJustify; valign: TTextLayout = tlTop): TRect;
function ComputeColorCircle(tx,ty: integer; light: word; hueCorrection: boolean = true): TBGRABitmap;
function ChangeCanvasSize(bmp: TBGRABitmap; newWidth,newHeight: integer; anchor: string; background: TBGRAPixel; repeatImage: boolean; flipMode: boolean = false): TBGRABitmap; overload;

procedure RenderCloudsOn(bmp: TBGRABitmap; color: TBGRAPixel);
procedure RenderWaterOn(bmp: TBGRABitmap; waterColor, skyColor: TBGRAPixel);

function CreateMetalFloorTexture(tx: integer): TBGRABitmap;
function CreatePlastikTexture(tx,ty: integer): TBGRABitmap;
function CreateCamouflageTexture(tx,ty: integer): TBGRABitmap;
function CreateSnowPrintTexture(tx,ty: integer): TBGRABitmap;
function CreateRoundStoneTexture(tx,ty: integer): TBGRABitmap;
function CreateStoneTexture(tx,ty: integer): TBGRABitmap;
function CreateWaterTexture(tx,ty: integer): TBGRABitmap;
function CreateMarbleTexture(tx,ty: integer): TBGRABitmap;
function CreateWoodTexture(tx,ty: integer): TBGRABitmap;
function CreateVerticalWoodTexture(tx,ty: integer): TBGRABitmap;

function ClearTypeFilter(source: TBGRACustomBitmap): TBGRACustomBitmap;
function ClearTypeInverseFilter(source: TBGRACustomBitmap): TBGRACustomBitmap;

function DoResample(source :TBGRABitmap; newWidth, newHeight: integer; StretchMode: TResampleMode): TBGRABitmap;
procedure DrawArrow(ACanvas: TCanvas; ARect: TRect; AStart: boolean; AKind: string; ALineCap: TPenEndCap; State: TOwnerDrawState);
procedure ApplyArrowStyle(AStart: boolean; AKind: string; ABmp: TBGRABitmap; ASize: TPointF);
procedure BCAssignSystemStyle(AButton: TBCButton);

implementation

uses GraphType, math, Types, LCLProc, FileUtil, dialogs, BGRAAnimatedGif,
  BGRAGradients, BGRATextFX, uresourcestrings, uscaledpi, BCTypes;

procedure BCAssignSystemState(AState: TBCButtonState; AFontColor, ATopColor, AMiddleTopColor, AMiddleBottomColor, ABottomColor, ABorderColor: TColor);
begin
  with AState do
  begin
    Border.Style := bboSolid;
    Border.Color := ABorderColor;
    Border.ColorOpacity := 255;
    FontEx.Color := AFontColor;
    FontEx.Style := [];
    FontEx.Shadow := True;
    FontEx.ShadowColor := clBlack;
    FontEx.ShadowColorOpacity := 192;
    FontEx.ShadowOffsetX := 1;
    FontEx.ShadowOffsetY := 1;
    FontEx.ShadowRadius := 2;
    Background.Gradient1EndPercent := 60;
    Background.Style := bbsGradient;
    // Gradient1
    with Background.Gradient1 do
    begin
      GradientType := gtLinear;
      StartColor := ATopColor;
      EndColor := AMiddleTopColor;
      Point1XPercent := 0;
      Point1YPercent := 0;
      Point2XPercent := 0;
      Point2YPercent := 100;
    end;
    // Gradient2
    with Background.Gradient2 do
    begin
      StartColor := AMiddleBottomColor;
      EndColor := ABottomColor;
      GradientType := gtLinear;
      Point1XPercent := 0;
      Point1YPercent := 0;
      Point2XPercent := 0;
      Point2YPercent := 100;
    end;
  end;
end;

procedure BCAssignSystemStyle(AButton: TBCButton);

  function MergeColor(AColor1,AColor2:TColor):TColor;
  begin
    result:= BGRAToColor(MergeBGRAWithGammaCorrection(ColorToBGRA(ColorToRGB(AColor1)),1,
    ColorToBGRA(ColorToRGB(AColor2)),1));
  end;

  function HoverColor(AColor1: TColor): TColor;
  var hsla1, hsla2: THSLAPixel;
  begin
    hsla1 := BGRAToHSLA(ColorToBGRA(ColorToRGB(AColor1)));
    hsla2 := BGRAToHSLA(ColorToBGRA(ColorToRGB(clHighlight)));
    hsla1.hue := hsla2.hue;
    hsla1.saturation:= hsla2.saturation;
    result := BGRAToColor(HSLAToBGRA(hsla1));
  end;

var highlight: TColor;
begin
  {$IFDEF DARWIN}
  highlight := MergeColor(clBtnFace,clWhite);
  {$ELSE}
  highlight := clBtnHighlight;
  {$ENDIF}
  with AButton do
  begin
    Rounding.RoundX := 6;
    Rounding.RoundY := 6;
    BCAssignSystemState(StateNormal, clBtnText, clBtnFace, highlight, clBtnFace, clBtnShadow, clBtnShadow);
    BCAssignSystemState(StateHover, HoverColor(clBtnText), HoverColor(clBtnFace), HoverColor(highlight), HoverColor(clBtnFace), HoverColor(clBtnShadow), HoverColor(clBtnShadow));
    BCAssignSystemState(StateClicked, HoverColor(clBtnText), HoverColor(MergeColor(clBtnFace,clBtnShadow)), HoverColor(clBtnFace), HoverColor(MergeColor(clBtnFace,clBtnShadow)), HoverColor(clBtnShadow), HoverColor(clBtnShadow));
  end;
end;

function RectUnion(const rect1, Rect2: TRect): TRect;
begin
  if IsRectEmpty(rect1) then
  begin
    if IsRectEmpty(rect2) then
      result := EmptyRect
    else
      result:= rect2;
  end else
  begin
    result := rect1;
    if not IsRectEmpty(rect2) then
      UnionRect(result,result,rect2);
  end;
end;

function RectOfs(const ARect: TRect; ofsX, ofsY: integer): TRect;
begin
  result := ARect;
  OffsetRect(result,ofsX,ofsY);
end;

function GetShapeBounds(const pts: array of TPointF; width: single): TRect;
var ix,iy,i: integer;
begin
  width /= 2;
  result.Left := high(Integer);
  result.Top := high(Integer);
  result.Right := low(Integer);
  result.Bottom := low(Integer);
  for i := 0 to high(pts) do
  if not isEmptyPointF(pts[i]) then
  begin
    ix := floor(pts[i].x - width);
    iy := floor(pts[i].y - width);
    if ix < result.left then result.left := ix;
    if iy < result.Top then result.top := iy;
    ix := ceil(pts[i].x + width)+1;
    iy := ceil(pts[i].y + width)+1;
    if ix > result.right then result.right := ix;
    if iy > result.bottom then result.bottom := iy;
  end;
  if (result.right <= result.left) or (result.bottom <= result.top) then
    result := EmptyRect;
end;

function DoPixelate(source: TBGRABitmap; pixelSize: integer; quality: string): TBGRABitmap;
var
  filter: TResampleFilter;
  useFilter: boolean;
begin
  if quality = rsMitchell then
    filter := rfMitchell else
  if quality = rsSpline then
    filter := rfSpline
  else
    filter := rfLinear;
  useFilter := quality <> rsFast;

  result := source.FilterPixelate(pixelSize,useFilter,filter) as TBGRABitmap;
end;

procedure DrawCheckers(bmp: TBGRABitmap; ARect: TRect);
const tx = 8; ty = 8; //must be a power of 2
      xMask = tx*2-1;
var xcount,patY,w,n,patY1,patY2m1,patX,patX1: NativeInt;
    oddColor, evenColor: TBGRAPixel;
    pdest: PBGRAPixel;
    delta: PtrInt;
    actualRect: TRect;
begin
  oddColor := BGRA(220,220,220);
  evenColor := BGRA(255,255,255);
  actualRect := ARect;
  IntersectRect(actualRect, ARect, bmp.ClipRect);
  w := actualRect.Right-actualRect.Left;
  if (w <= 0) or (actualRect.Bottom <= actualRect.Top) then exit;
  delta := bmp.Width;
  if bmp.LineOrder = riloBottomToTop then delta := -delta;
  delta := (delta-w)*SizeOf(TBGRAPixel);
  pdest := bmp.ScanLine[actualRect.Top]+actualRect.left;
  patY1 := actualRect.Top - ARect.Top;
  patY2m1 := actualRect.Bottom - ARect.Top-1;
  patX1 := (actualRect.Left - ARect.Left) and xMask;
  for patY := patY1 to patY2m1 do
  begin
    xcount := w;
    if patY and ty = 0 then
       patX := patX1
    else
       patX := (patX1+tx) and xMask;
    while xcount > 0 do
    begin
      if patX and tx = 0 then
      begin
        n := 8-patX;
        if n > xcount then n := xcount;
        FillDWord(pdest^,n,DWord(evenColor));
        dec(xcount,n);
        inc(pdest,n);
        patX := tx;
      end else
      begin
        n := 16-patX;
        if n > xcount then n := xcount;
        FillDWord(pdest^,n,DWord(oddColor));
        dec(xcount,n);
        inc(pdest,n);
        patX := 0;
      end;
    end;
    inc(pbyte(pdest),delta);
  end;
  bmp.InvalidateBitmap;
end; 

procedure DrawGrid(bmp: TBGRABitmap; sizex, sizey: single);
var xb,yb: integer;
    imgGrid: TBGRABitmap;
    alpha: byte;
begin
    imgGrid := TBGRABitmap.Create(bmp.Width,1);
    alpha := min(96,round((abs(sizex)+abs(sizey))*(96/16/2)));
    imgGrid.DrawLineAntialias(0,0,imgGrid.width-1,0,BGRA(255,255,255,alpha),BGRA(0,0,0,alpha),
        min(3,max(1,round(sizex/8))),true);
    for yb := 1 to trunc(bmp.Height/sizey) do
     bmp.PutImage(0,round(yb*sizey),imgGrid,dmFastBlend);
    imgGrid.Free;

    imgGrid := TBGRABitmap.Create(1,bmp.Height);
    imgGrid.DrawLineAntialias(0,0,0,imgGrid.height-1,BGRA(0,0,0,alpha),BGRA(255,255,255,alpha),
      min(3,max(1,round(sizey/8))),true);
    for xb := 1 to trunc(bmp.Width/sizex) do
     bmp.PutImage(round(xb*sizex),0,imgGrid,dmFastBlend);
    imgGrid.Free;
end;

procedure RenderCloudsOn(bmp: TBGRABitmap; color: TBGRAPixel);
const minDensity=180; maxDensity=240;
var i,k,x,y: integer;
    fact,radius: single;
    tempBmp: TBGRABitmap;
    ptemp: PBGRAPixel;
begin
   if color.alpha = 0 then exit;

   tempBmp := TBGRABitmap.Create(bmp.width,bmp.Height,BGRABlack);
   fact := (bmp.width+bmp.Height)/15;
   for i := 120 downto 20 do
   begin
      for k := 1 to 2 do
      begin
        radius := ((i+random(50))/100)*fact;
        x := random(bmp.Width);
        y := random(bmp.Height);
        tempBmp.GradientFill(floor(x-radius),floor(y-radius),ceil(x+radius),ceil(y+radius),BGRA(255,255,255,128),BGRAPixelTransparent,gtRadial,pointf(x,y),pointf(x+radius+0.5,y),dmFastBlend,false);
      end;
   end;

   ptemp := tempBmp.Data;
   for i := tempBmp.nbPixels-1 downto 0 do
   begin
      if ptemp^.red < minDensity then ptemp^:= BGRAPixelTransparent else
      if ptemp^.red > maxDensity then ptemp^:= color else
        ptemp^ := BGRA(color.red,color.green,color.blue,color.alpha*(ptemp^.red-minDensity) div (maxDensity-minDensity));
      inc(ptemp);
   end;
   bmp.PutImage(0,0,tempBmp,dmDrawWithTransparency);
   tempBmp.free;
end;

procedure RenderWaterOn(bmp: TBGRABitmap; waterColor, skyColor: TBGRAPixel);
var Noise,Temp: TBGRABitmap;
    Phong: TPhongShading;
begin
   Noise := CreateCyclicPerlinNoiseMap(bmp.Width,bmp.Height,1,1,1.2);
   Temp := Noise.FilterBlurRadial(1,rbFast) as TBGRABitmap;
   Noise.Free;
   Noise := Temp;
   Noise.ApplyGlobalOpacity(waterColor.alpha);
   waterColor.alpha := 255;

   Phong := TPhongShading.Create;
   Phong.NegativeDiffusionFactor := 0.1;
   Phong.AmbientFactor := 0.7;
   Phong.LightSourceDistanceFactor := 0;
   Phong.LightDestFactor := 0;
   Phong.LightSourceIntensity := 300;
   Phong.LightPosition := Point(-500,-500);
   Phong.LightColor := skyColor;
   Phong.Draw(bmp,Noise,30,0,0,waterColor);
   Noise.Free;
   Phong.Free;
end;

function Interp256(value1,value2,position: integer): integer; inline;
begin
     result := (value1*(256-position)+value2*position) shr 8;
end;

function Interp256(color1,color2: TBGRAPixel; position: integer): TBGRAPixel; inline;
begin
     result.red := Interp256(color1.red,color2.red,position);
     result.green := Interp256(color1.green,color2.green,position);
     result.blue := Interp256(color1.blue,color2.blue,position);
     result.alpha := Interp256(color1.alpha,color2.alpha,position);
end;

function CreateWoodTexture(tx,ty: integer): TBGRABitmap;
var
  colorOscillation, globalColorVariation: integer;
  p: PBGRAPixel;
  i: Integer;
begin
  result := CreateCyclicPerlinNoiseMap(tx,ty,1.5,1.5,1,rfBestQuality);
  p := result.Data;
  for i := 0 to result.NbPixels-1 do
  begin
    colorOscillation := round(sqrt((sin(p^.red*Pi/16)+1)/2)*256);
    globalColorVariation := p^.red;
    p^:= Interp256( Interp256(BGRA(247,188,120),BGRA(255,218,170),colorOscillation),
                    Interp256(BGRA(157,97,60),BGRA(202,145,112),colorOscillation), globalColorVariation);
    inc(p);
  end;
end;

function CreateVerticalWoodTexture(tx, ty: integer): TBGRABitmap;
var
  globalPos: single;
  colorOscillation, globalColorVariation: integer;
  p: PBGRAPixel;
  i: Integer;
  x,nbVertical: integer;
begin
  result := CreateCyclicPerlinNoiseMap(tx,ty,1,1,1,rfBestQuality);
  p := result.Data;
  x := 0;
  nbVertical := tx div 128;
  if nbVertical = 0 then nbVertical := 1;
  for i := 0 to result.NbPixels-1 do
  begin
    globalPos := p^.red*Pi/32 + nbVertical*x*2*Pi/tx*8;
    colorOscillation := round(sqrt((sin(globalPos)+1)/2)*256);
    globalColorVariation := p^.red; //round(sin(globalPos/8)*128+128);
    p^:= Interp256( Interp256(BGRA(247,188,120),BGRA(255,218,170),colorOscillation),
                    Interp256(BGRA(157,97,60),BGRA(202,145,112),colorOscillation), globalColorVariation);
    inc(p);
    inc(x);
    if x = tx then x := 0;
  end;
end;

function ClearTypeFilter(source: TBGRACustomBitmap): TBGRACustomBitmap;
var
  mul3,temp: TBGRACustomBitmap;
  xb,yb: integer;
  pmul3,pdest: PBGRAPixel;
  a: byte;
begin
  source.ResampleFilter := rfSpline;
  mul3 := source.Resample(source.Width*3-2,source.Height);
  temp := source.NewBitmap(source.Width*3,source.Height);
  temp.PutImage(1,0,mul3,dmSet);
  for yb := 0 to temp.Height-1 do
  begin
    temp.SetPixel(0,yb,temp.GetPixel(1,yb));
    temp.SetPixel(temp.Width-1,yb,temp.GetPixel(temp.Width-2,yb));
  end;
  mul3.free;
  mul3 := temp;
  result := source.NewBitmap(source.Width,source.Height);
  for yb := 0 to result.Height-1 do
  begin
    pmul3 := mul3.ScanLine[yb];
    pdest := result.ScanLine[yb];
    for xb := result.width-1 downto 0 do
    begin
      a := (pmul3+1)^.alpha;
      if a = 0 then pdest^:= BGRAPixelTransparent
      else
      begin
        pdest^.alpha := a;
        if pmul3^.alpha = 0 then pdest^.red := 128 else
          pdest^.red := pmul3^.red;
        pdest^.green := (pmul3+1)^.green;
        if (pmul3+2)^.alpha = 0 then pdest^.blue := 128 else
          pdest^.blue := (pmul3+2)^.blue;
      end;
      inc(pdest);
      inc(pmul3,3);
    end;
  end;
  mul3.free;
end;

function ClearTypeInverseSubFilter(source: TBGRACustomBitmap): TBGRACustomBitmap;
const
    blueA = 20;
    blueB = 0;
    blueC = 2;
    redA = 20;
    redB = 0;
    redC = 2;

    maxV = 255;

var yb,xb: integer;
    psrc,pdest,pgray: PBGRAPixel;
    a,v: integer;
    grayscale,temp: TBGRACustomBitmap;
    function Merge3(c1, c2, c3: TBGRAPixel): TBGRAPixel;
    var c123: cardinal;
    begin
      if (c1.alpha = 0) then
        Result := MergeBGRA(c2,c3)
      else
      if (c2.alpha = 0) then
        Result := MergeBGRA(c1,c3)
      else
      if (c3.alpha = 0) then
        Result := MergeBGRA(c1,c2)
      else
      begin
        c123 := c1.alpha + c2.alpha + c3.alpha;
        Result.red   := (c1.red * c1.alpha + c2.red * c2.alpha + c3.red * c3.alpha + c123 shr 1) div c123;
        Result.green := (c1.green * c1.alpha + c2.green * c2.alpha + c3.green * c3.alpha + c123 shr 1) div c123;
        Result.blue  := (c1.blue * c1.alpha + c2.blue * c2.alpha + c3.blue * c3.alpha + c123 shr 1) div c123;
        Result.alpha := (c123 + 1) div 3;
      end;
    end;

begin
  if source.width <= 1 then
  begin
    result := source.duplicate;
    exit;
  end;
  grayscale := source;
  temp := source.NewBitmap(source.Width,source.Height);
  for yb := 0 to source.Height-1 do
  begin
    psrc := source.Scanline[yb];
    pgray := grayscale.ScanLine[yb];
    pdest := temp.Scanline[yb];
    pdest^.red := psrc^.red;
    pdest^.green := psrc^.green;
    pdest^.alpha := psrc^.alpha;
    a := (psrc^.alpha*blueA) - ((psrc+1)^.alpha*(blueB));
    if a > 0 then
    begin
      v := ((integer(psrc^.blue)*blueA)*psrc^.alpha - integer((psrc+1)^.blue*blueB)*(psrc+1)^.alpha) div a;
      if v >= maxV then
        pdest^.blue := 255 else
      if v > 0 then
        pdest^.blue := v
      else
        pdest^.blue := 0;
    end
    else
      pdest^.blue := psrc^.blue;
    inc(pdest);
    inc(psrc);
    inc(pgray);
    for xb := source.width-3 downto 0 do
    begin
      pdest^.green := psrc^.green;
      pdest^.alpha := psrc^.alpha;

      a := (psrc^.alpha*redA) - ((psrc-1)^.alpha*(redB));
      if a > 0 then
      begin
        v := ((integer(psrc^.red)*redA)*psrc^.alpha - integer((psrc-1)^.red*redB+((pgray-1)^.green-pgray^.green)*redC)*(psrc-1)^.alpha) div a;
        if v >= maxV then
          pdest^.red := 255 else
        if v > 0 then
          pdest^.red := v
        else
          pdest^.red := 0;
      end
      else
        pdest^.red := psrc^.red;

      a := (psrc^.alpha*blueA) - ((psrc+1)^.alpha*(blueB));
      if a > 0 then
      begin
        v := ((integer(psrc^.blue)*blueA)*psrc^.alpha - integer((psrc+1)^.blue*blueB+((pgray+1)^.green-pgray^.green)*blueC)*(psrc+1)^.alpha) div a;
        if v >= maxV then
          pdest^.blue := 255 else
        if v > 0 then
          pdest^.blue := v
        else
          pdest^.blue := 0;
      end
      else
        pdest^.blue := psrc^.blue;
      inc(pdest);
      inc(psrc);
      inc(pgray);
    end;
    pdest^.green := psrc^.green;
    pdest^.blue := psrc^.blue;
    pdest^.alpha := psrc^.alpha;

    a := (psrc^.alpha*redA) - ((psrc-1)^.alpha*(redB));
    if a > 0 then
    begin
      v := ((integer(psrc^.red)*redA)*psrc^.alpha - integer((psrc-1)^.red*redB)*(psrc-1)^.alpha) div a;
      if v >= maxV then
        pdest^.red := 255 else
      if v > 0 then
        pdest^.red := v
      else
        pdest^.red := 0;
    end
    else
      pdest^.red := psrc^.red;
  end;

  result := temp;
end;

function ClearTypeSharpenFilter(source, diffbmp: TBGRACustomBitmap): TBGRACustomBitmap;
const
  factnum = 3;
  factdenom = 5;
var
  xb,yb,maxx: integer;
  psrc,pdest,pdiff: PBGRAPixel;
  d1,d2 : integer;

  function clamp(value: integer) : byte;
  begin
    if value <= 0 then result := 0
    else if value >= 255 then result := 255
    else result := value;
  end;

  function adjustDiff(ref,v1,v2: integer): integer;
  begin
    v1 -= ref;
    v2 -= ref;
    result := v1+v2;
  end;

begin
  if diffbmp = nil then diffbmp := source;
  if (source.width <= 1) or (diffbmp.width <> source.width) or (diffbmp.height <> source.height) then
  begin
    result := source.Duplicate();
    exit;
  end;
  result := source.NewBitmap(source.Width,source.Height);
  for yb := 0 to result.Height-1 do
  begin
    psrc := source.ScanLine[yb];
    pdest := result.ScanLine[yb];
    pdiff := diffbmp.ScanLine[yb];
    maxx := result.width-1;
    for xb := 0 to maxx do
    begin
      if psrc^.alpha <> 0 then
      begin
        if (xb > 0) and ((psrc-1)^.alpha <>0) and (xb < maxx) and ((psrc+1)^.alpha <>0) then
        begin
          d1 := BGRADiff((pdiff-1)^,pdiff^);
          d2 := BGRADiff((pdiff+1)^,pdiff^);
          if (d1 > 20) and (d2 > 20) and (d1+d2 > 100) then
          begin
            pdest^.red := clamp(psrc^.red - (adjustDiff(psrc^.red,(psrc+1)^.red,(psrc-1)^.red))*factnum div (2*factdenom));
            pdest^.green := psrc^.green;
            pdest^.blue := clamp(psrc^.blue - (adjustDiff(psrc^.blue,(psrc+1)^.blue,(psrc-1)^.blue))*factnum div (2*factdenom));
            pdest^.alpha := psrc^.alpha;
          end
            else
              pdest^ := psrc^;
        end else
        if (xb < maxx) and ((psrc+1)^.alpha <>0) then
        begin
          pdest^.red := clamp(psrc^.red - ((psrc+1)^.red-psrc^.red) *factnum div factdenom);
          pdest^.green := psrc^.green;
          pdest^.blue := clamp(psrc^.blue - ((psrc+1)^.blue-psrc^.blue) *factnum div factdenom);
          pdest^.alpha := psrc^.alpha;
        end else
        if (xb > 0) and ((psrc-1)^.alpha <>0) then
        begin
          pdest^.red := clamp(psrc^.red - ((psrc-1)^.red-psrc^.red)*factnum div factdenom);
          pdest^.green := psrc^.green;
          pdest^.blue := clamp(psrc^.blue - ((psrc-1)^.blue-psrc^.blue)*factnum div factdenom);
          pdest^.alpha := psrc^.alpha;
        end else
          pdest^ := psrc^;
      end else
        pdest^ := BGRAPixelTransparent;

      inc(pdest);
      inc(psrc);
      inc(pdiff);
    end;
  end;
end;

function ClearTypeRemoveContradiction(source: TBGRACustomBitmap): TBGRACustomBitmap;
var
  xb,yb: integer;
  dr,db: integer;
  ratio: single;
  psrc,pdest: PBGRAPixel;

begin
  if source.width <= 1 then
  begin
    result := source.Duplicate();
    exit;
  end;
  result := source.NewBitmap(source.Width,source.Height);
  for yb := 0 to result.Height-1 do
  begin
    psrc := source.ScanLine[yb];
    pdest := result.ScanLine[yb];
    pdest^ := psrc^;
    for xb := result.width-2 downto 0 do
    begin
      (pdest+1)^ := (psrc+1)^;
      if (psrc^.alpha > 10) and ((psrc+1)^.alpha > 10) then
      begin
        dr := psrc^.red-(psrc+1)^.red;
        db := psrc^.blue-(psrc+1)^.blue;
        if ((db < 0) and (dr > 0)) or
           ((db > 0) and (dr < 0)) then
        begin
          ratio := abs(dr/db);
          if (ratio > 0.2) and (ratio < 5) then
          begin
            dr := (psrc^.red*psrc^.alpha+(psrc+1)^.red*(psrc+1)^.alpha) div (psrc^.alpha+(psrc+1)^.alpha);
            db := (psrc^.blue*psrc^.alpha+(psrc+1)^.blue*(psrc+1)^.alpha) div (psrc^.alpha+(psrc+1)^.alpha);
            pdest^.red := dr;
            pdest^.blue := db;
            (pdest+1)^.red := dr;
            (pdest+1)^.blue := db;
          end;
        end;
      end;
      inc(pdest);
      inc(psrc);
    end;
  end;
end;

function ClearTypeInverseFilter(source: TBGRACustomBitmap): TBGRACustomBitmap;
var
  mul3,temp: TBGRACustomBitmap;
  xb,yb: integer;
  pmul3,pdest: PBGRAPixel;
  a: byte;
begin
  source.ResampleFilter := rfSpline;
  mul3 := source.Resample(source.Width*3-2,source.Height);
  temp := source.NewBitmap(source.Width*3,source.Height);
  temp.PutImage(1,0,mul3,dmSet);
  for yb := 0 to temp.Height-1 do
  begin
    temp.SetPixel(0,yb,temp.GetPixel(1,yb));
    temp.SetPixel(temp.Width-1,yb,temp.GetPixel(temp.Width-2,yb));
  end;
  mul3.free;
  mul3 := temp;
  result := source.NewBitmap(source.Width,source.Height);
  for yb := 0 to result.Height-1 do
  begin
    pmul3 := mul3.ScanLine[yb];
    pdest := result.ScanLine[yb];
    for xb := result.width-1 downto 0 do
    begin
      a := (pmul3+1)^.alpha;
      if a = 0 then pdest^:= BGRAPixelTransparent
      else
      begin
        pdest^.alpha := a;
        if (pmul3+2)^.alpha = 0 then pdest^.red := 128 else
          pdest^.red := (pmul3+2)^.red;
        pdest^.green := (pmul3+1)^.green;
        if pmul3^.alpha = 0 then pdest^.blue := 128 else
          pdest^.blue := pmul3^.blue;
      end;
      inc(pdest);
      inc(pmul3,3);
    end;
  end;
  mul3.free;

  temp := ClearTypeRemoveContradiction(result);
  result.free;
  result := temp;

  temp := result;
  result := ClearTypeSharpenFilter(temp,source);
  temp.Free;

  temp := ClearTypeRemoveContradiction(result);
  result.free;
  result := temp;
end;

function DoResample(source: TBGRABitmap; newWidth, newHeight: integer;
  StretchMode: TResampleMode): TBGRABitmap;
begin
  result := source.Resample(newWidth,newHeight,StretchMode) as TBGRABitmap;
end;

procedure DrawArrow(ACanvas: TCanvas; ARect: TRect; AStart: boolean; AKind: string; ALineCap: TPenEndCap; State: TOwnerDrawState);
var bmp : TBGRABitmap;
  c,c2: TBGRAPixel;
  x1,x2,xm1,xm2,y,w,temp: single;
begin
  if odSelected in State then
  begin
    c2 := ColorToBGRA(ColorToRGB(clHighlight));
    c := ColorToBGRA(ColorToRGB(clHighlightText));
  end else
  begin
    c2 := ColorToBGRA(ColorToRGB(clWindow));
    c := ColorToBGRA(ColorToRGB(clWindowText));
  end;
  with Size(ARect) do bmp:= TBGRABitmap.Create(cx,cy,c2);
  ApplyArrowStyle(AStart,AKind,bmp,PointF(1.5,1.5));
  bmp.LineCap := ALineCap;
  w := bmp.Height/5;
  if w > 0 then
  begin
    x1 := w*2.5;
    x2 := 0;
    xm1 := 0;
    xm2 := w*2.5;
    if (AKind = 'Normal') or (AKind = 'Cut') then x1 -= w*0.7 else
    if (AKind = 'Flipped') or (AKind = 'FlippedCut') then x1 += w*0.7;
    if not AStart then
    begin
      temp := x1;
      x1 := -x2;
      x2 := -temp;
    end else
    begin
      xm1 := (bmp.Width-0.5)-xm1;
      xm2 := (bmp.Width-0.5)-xm2;
    end;
    x1 -= 0.5;
    x2 += bmp.Width-0.5;
    y := (bmp.Height-1)/2;
    if (AKind='Tail') or (AKind='None') or (AKind = 'Tip') then w *= 2;
    bmp.DrawLineAntialias(x1,y,x2,y,c,w);
    if bmp.Width > bmp.Height*2 then
      bmp.GradientFill(0,0,bmp.width,bmp.height,c2,BGRAPixelTransparent,gtLinear,PointF(xm1,0),PointF(xm2,0),dmDrawWithTransparency);
  end;
  ACanvas.Draw(ARect.Left,ARect.Top,bmp.Bitmap);
  bmp.Free;
end;

procedure ApplyArrowStyle(AStart: boolean; AKind: string; ABmp: TBGRABitmap; ASize: TPointF);
var backOfs: single;
begin
  backOfs := 0;
  if (ASize.x = 0) or (ASize.y = 0) then AKind := 'None';
  if (length(AKind)>0) and (AKind[length(AKind)] in['1'..'9']) then backOfs := (ord(AKind[length(AKind)])-ord('0'))*0.25;
  case AKind of
  'Tail': if AStart then ABmp.ArrowStartAsTail else ABmp.ArrowEndAsTail;
  'Tip': if AStart then ABmp.ArrowStartAsTriangle else ABmp.ArrowEndAsTriangle;
  'Normal','Cut','Flipped','FlippedCut': if AStart then ABmp.ArrowStartAsClassic((AKind='Flipped') or (AKind='FlippedCut'),(AKind='Cut') or (AKind='FlippedCut'))
    else ABmp.ArrowEndAsClassic((AKind='Flipped') or (AKind='FlippedCut'),(AKind='Cut') or (AKind='FlippedCut'));
  'Triangle','TriangleBack1','TriangleBack2': if AStart then ABmp.ArrowStartAsTriangle(backOfs) else ABmp.ArrowEndAsTriangle(backOfs);
  'HollowTriangle','HollowTriangleBack1','HollowTriangleBack2': if AStart then ABmp.ArrowStartAsTriangle(backOfs,False,True) else ABmp.ArrowEndAsTriangle(backOfs,False,True);
  else if AStart then ABmp.ArrowStartAsNone else ABmp.ArrowEndAsNone;
  end;
  if (AKind = 'Tip') and not ((ASize.x = 0) or (ASize.y = 0)) then
    ASize := ASize*(0.5/ASize.y);
  if AStart then ABmp.ArrowStartSize := ASize
  else ABmp.ArrowEndSize := ASize;
end;

function CreateMarbleTexture(tx,ty: integer): TBGRABitmap;
var
  colorOscillation: integer;
  p: PBGRAPixel;
  i: Integer;
begin
  result := CreateCyclicPerlinNoiseMap(tx,ty,0.5,0.5,0.8,rfBestQuality);
  p := result.Data;
  for i := 0 to result.NbPixels-1 do
  begin
    colorOscillation := round(sqrt(sqrt((sin(p^.red*Pi/128+0.5)+1)/2))*256);
    p^ := Interp256(BGRA(161,117,105),BGRA(218,197,180),colorOscillation);
    inc(p);
  end;
end;

function CreateWaterTexture(tx,ty: integer): TBGRABitmap;
const blurSize = 5;
var
  temp: TBGRABitmap;
  phong: TPhongShading;
begin
  result := CreateCyclicPerlinNoiseMap(tx,ty,1,1,1.2,rfBestQuality);
  temp:= result.GetPart(rect(-blurSize,-blurSize,tx+blurSize,ty+blurSize)) as TBGRABitmap;
  BGRAReplace(temp,temp.FilterBlurRadial(blurSize,rbFast));
  phong := TPhongShading.Create;
  phong.LightSourceDistanceFactor := 0;
  phong.LightDestFactor := 0;
  phong.LightSourceIntensity := 150;
  phong.LightPositionZ := 80;
  phong.LightColor := BGRA(105,233,240);
  phong.NegativeDiffusionFactor := 0.3;
  phong.SpecularIndex := 20;
  phong.AmbientFactor := 0.4;
  phong.Draw(result,temp,20,-blurSize,-blurSize,BGRA(28,139,166));
  phong.Free;
  temp.Free;
end;

function CreateStoneTexture(tx,ty: integer): TBGRABitmap;
var
  temp: TBGRABitmap;
  phong: TPhongShading;
begin
  result := CreateCyclicPerlinNoiseMap(tx,ty,1,1,0.6);
  temp:= result.GetPart(rect(-2,-2,tx+2,ty+2)) as TBGRABitmap;
  phong := TPhongShading.Create;
  phong.LightSourceDistanceFactor := 0;
  phong.LightDestFactor := 0;
  phong.LightSourceIntensity := 100;
  phong.LightPositionZ := 100;
  phong.NegativeDiffusionFactor := 0.3;
  phong.AmbientFactor := 0.5;
  phong.Draw(result,temp,30,-2,-2,BGRA(170,170,170));
  phong.Free;
  temp.Free;
end;

function CreateRoundStoneTexture(tx,ty: integer): TBGRABitmap;
var
  temp: TBGRABitmap;
  phong: TPhongShading;
begin
  result := CreateCyclicPerlinNoiseMap(tx,ty,1,1,1.2,rfBestQuality);
  temp:= result.GetPart(rect(-2,-2,tx+2,ty+2)) as TBGRABitmap;
  BGRAReplace(temp,temp.FilterBlurRadial(2,rbFast));
  phong := TPhongShading.Create;
  phong.LightSourceDistanceFactor := 0;
  phong.LightDestFactor := 0;
  phong.LightSourceIntensity := 70;
  phong.LightPositionZ := 100;
  phong.NegativeDiffusionFactor := 0;
  phong.SpecularIndex := 10;
  phong.AmbientFactor := 0.5;
  phong.LightColor := BGRA(255,255,192);
  phong.Draw(result,temp,30,-2,-2,BGRA(170,170,170));
  phong.Free;
  temp.Free;
end;

function CreateSnowPrintTexture(tx,ty: integer): TBGRABitmap;
var
  v: single;
  p: PBGRAPixel;
  i: Integer;

  temp: TBGRABitmap;
  phong: TPhongShading;
begin
  result := CreateCyclicPerlinNoiseMap(tx,ty,1,1,1.2,rfBestQuality);

  p := result.Data;
  for i := 0 to result.NbPixels-1 do
  begin
    v := p^.red;
    if v > 80 then v := (v-80)/10+80;
    if v < 50 then v := 50-(50-v)/10;
    p^ := MapHeightToBGRA(v/255,255);
    inc(p);
  end;

  temp:= result.GetPart(rect(-2,-2,tx+2,ty+2)) as TBGRABitmap;
  phong := TPhongShading.Create;
  phong.LightSourceDistanceFactor := 0;
  phong.LightDestFactor := 0;
  phong.LightSourceIntensity := 100;
  phong.LightPositionZ := 100;
  phong.NegativeDiffusionFactor := 0.3;
  phong.Draw(result,temp,30,-2,-2,BGRAWhite);
  phong.Free;
  temp.Free;
end;

function CreateCamouflageTexture(tx,ty: integer): TBGRABitmap;
var
  v: integer;
  p: PBGRAPixel;
  i: Integer;

  temp: TBGRABitmap;
begin
  result := CreateCyclicPerlinNoiseMap(tx,ty,1,1,1,rfBestQuality);

  p := result.Data;
  for i := 0 to result.NbPixels-1 do
  begin
    v := p^.red;
    if v < 64 then p^:= BGRA(31,33,46) else
    if v < 128 then p^:= BGRA(89,71,57) else
    if v < 192 then p^:= BGRA(80,106,67) else
      p^:= BGRA(161,157,121);
    inc(p);
  end;

  temp := result.getPart(rect(-2,-2,tx+2,ty+2)) as TBGRABitmap;
  BGRAReplace(temp,temp.FilterMedian(moMediumSmooth));
  result.PutImage(-2,-2,temp,dmSet);
  temp.Free;
end;

function CreatePlastikTexture(tx,ty: integer): TBGRABitmap;
const blurSize = 2;
var
  temp: TBGRABitmap;
  phong: TPhongShading;
  p: PBGRAPixel;
  i: Integer;
  v: Byte;
begin
  result := CreateCyclicPerlinNoiseMap(tx,ty,1,1,1);

  p := result.Data;
  for i := 0 to result.NbPixels-1 do
  begin
    v := p^.red;
    if v < 32 then v:= v*2 else
    if (v > 32) and (v < 224) then v := (v-32) div 2 + 64 else
    if v >= 224 then v:= (v-224)*2+(224-32) div 2;
    p^:= BGRA(v,v,v);
    inc(p);
  end;

  temp:= result.GetPart(rect(-blurSize,-blurSize,tx+blurSize,ty+blurSize)) as TBGRABitmap;
  BGRAReplace(temp,temp.FilterNormalize(False));
  BGRAReplace(temp,temp.FilterBlurMotion(ty div 6,90,False));
  BGRAReplace(temp,temp.FilterBlurRadial(blurSize,rbFast));

  phong := TPhongShading.Create;
  phong.LightSourceDistanceFactor := 0;
  phong.LightDestFactor := 0;
  phong.LightSourceIntensity := 300;
  phong.LightPositionZ := 10;
  phong.NegativeDiffusionFactor := 0;
  phong.AmbientFactor := 0.6;
  phong.SpecularIndex := 25;
  phong.SpecularFactor:= 10;
  phong.Draw(result,temp,10,-blurSize,-blurSize,BGRA(58,206,113));
  phong.Free;
  temp.Free;
end;

function CreateMetalFloorTexture(tx: integer): TBGRABitmap;
var
  temp,noise: TBGRABitmap;
  phong: TPhongShading;
  ty: integer;
begin
  ty := tx div 2;
  result := TBGRABitmap.Create(tx,ty,BGRABlack);
  result.FillEllipseAntialias(tx*1.2/8,ty/2,tx/20,ty/3,BGRA(240,240,240));
  result.FillEllipseAntialias(tx*2.8/8,ty/2,tx/20,ty/3,BGRA(240,240,240));
  result.FillEllipseAntialias(tx*3/4,ty*1.2/4,ty/3,tx/20,BGRA(240,240,240));
  result.FillEllipseAntialias(tx*3/4,ty*2.8/4,ty/3,tx/20,BGRA(240,240,240));
  BGRAReplace(result,result.FilterBlurRadial(1,rbFast));

  noise := CreateCyclicPerlinNoiseMap(tx,ty,1,1,1);
  noise.FillRect(0,0,tx,ty,BGRA(0,0,0,220),dmLinearBlend);
  result.BlendImage(0,0,noise,boAdditive);
  noise.free;

  temp:= result.GetPart(rect(-2,-2,tx+2,ty+2)) as TBGRABitmap;
  phong := TPhongShading.Create;
  phong.LightSourceDistanceFactor := 0;
  phong.LightDestFactor := 0;
  phong.LightSourceIntensity := 100;
  phong.LightPositionZ := 80;
  phong.NegativeDiffusionFactor := 0;
  phong.AmbientFactor := 0.5;
  phong.Draw(result,temp,10,-2,-2,BGRA(116,116,116));
  phong.Free;
  temp.Free;
end;

function ComputeAngle(dx, dy: single): single;
begin
     if dy = 0 then
     begin
       if dx < 0 then result := 180 else result := 0;
     end else
     if dx = 0 then
     begin
       if dy < 0 then result := -90 else result := 90;
     end else
     begin
       result := ArcTan(dy/dx)*180/Pi;
       if dx < 0 then result += 180;
     end;
end;

function GetSelectionCenter(bmp: TBGRABitmap): TPointF;
var xb,yb: integer; p: PBGRAPixel;
    xsum,ysum,asum,alpha: single;
begin
    if bmp = nil then
    begin
      result := pointF(0,0);
      exit;
    end;
    xsum := 0;
    ysum := 0;
    asum := 0;
    for yb := 0 to bmp.Height-1 do
    begin
      p := bmp.ScanLine[yb];
      for xb := 0 to bmp.Width-1 do
      begin
        alpha := p^.red/255; inc(p);
        xsum += xb*alpha;
        ysum += yb*alpha;
        asum += alpha;
      end;
    end;
    if asum = 0 then
      result := pointF(bmp.width/2-0.5, bmp.Height/2-0.5) else
       result := pointF(xsum/asum,ysum/asum);
end;

procedure ComputeSelectionMask(image: TBGRABitmap; destMask: TBGRABitmap; ARect: TRect);
var
   aimage: byte;
   xb,yb: integer; pimage, pmask: PBGRAPixel;
begin
    IntersectRect(ARect, ARect,rect(0,0,image.Width,image.Height));
    IntersectRect(ARect, ARect,rect(0,0,destMask.Width,destMask.Height));
    for yb := ARect.Top to ARect.Bottom-1 do
    begin
      pimage := image.ScanLine[yb]+ARect.Left;
      pmask := destMask.ScanLine[yb]+ARect.Left;
      for xb := ARect.Left to ARect.Right-1 do
      begin
        aimage := pimage^.alpha;
        pmask^ := BGRA(aimage,aimage,aimage,255);
        if aimage <> 0 then pimage^.alpha := 255;
        inc(pimage);
        inc(pmask);
      end;
    end;
end;

procedure SubstractMask(image: TBGRABitmap; DestX,DestY: Integer; mask: TBGRABitmap; SourceMaskRect: TRect);
var
   xb,yb: integer;
   pimage, pmask: PBGRAPixel;
   aimage, amask: byte;
   Delta: TPoint;
begin
    if not IntersectRect(SourceMaskRect,SourceMaskRect,rect(0,0,mask.Width,mask.Height)) then exit;
    Delta.X := - SourceMaskRect.Left + DestX;
    Delta.Y := - SourceMaskRect.Top + DestY;
    OffsetRect(SourceMaskRect, Delta.x, Delta.y);
    if not IntersectRect(SourceMaskRect,SourceMaskRect,rect(0,0,image.Width,image.Height)) then exit;
    OffsetRect(SourceMaskRect, -Delta.x, -Delta.y);
    for yb := SourceMaskRect.Top to SourceMaskRect.Bottom-1 do
    begin
      pimage := image.ScanLine[yb+Delta.Y]+SourceMaskRect.Left+Delta.X;
      pmask := mask.ScanLine[yb]+SourceMaskRect.Left;
      for xb := SourceMaskRect.Left to SourceMaskRect.Right-1 do
      begin
        amask := pmask^.red;
        if amask <> 0 then
        begin
             aimage := pimage^.alpha;
             if aimage > amask then
               pimage^.alpha := aimage-amask else
                 pimage^ := BGRAPixelTransparent;
        end;
        inc(pimage);
        inc(pmask);
      end;
    end;
end;

function NicePointBounds(x,y: single): TRect;
begin
  result := rect(floor(x)-NicePointMaxRadius-1,floor(y)-NicePointMaxRadius-1,
  ceil(x)+NicePointMaxRadius+2,ceil(y)+NicePointMaxRadius+2);
end;

function NicePoint(bmp: TBGRABitmap; x, y: single; alpha: byte = 192): TRect;
begin
    result := NicePointBounds(x,y);
    if not Assigned(bmp) then exit;
    bmp.EllipseAntialias(x,y,NicePointMaxRadius,NicePointMaxRadius,BGRA(0,0,0,alpha),1);
    bmp.EllipseAntialias(x,y,NicePointMaxRadius-1,NicePointMaxRadius-1,BGRA(255,255,255,alpha),1);
    bmp.EllipseAntialias(x,y,NicePointMaxRadius-2,NicePointMaxRadius-2,BGRA(0,0,0,alpha),1);
end;

function NicePoint(bmp: TBGRABitmap; ptF: TPointF; alpha: byte = 192): TRect;
begin
  result := NicePoint(bmp,ptF.x,ptF.y,alpha);
end;

procedure NiceLine(bmp: TBGRABitmap; x1, y1, x2, y2: single; alpha: byte = 192);
begin
  if not Assigned(bmp) then exit;
  bmp.DrawLineAntialias(round(x1), round(y1), round(x2), round(y2),BGRA(0,0,0,alpha),3,True);
  bmp.DrawLineAntialias(round(x1), round(y1), round(x2), round(y2),BGRA(255,255,255,alpha),1,True);
end;

function NiceText(bmp: TBGRABitmap; x, y, bmpWidth,bmpHeight: integer; s: string; align: TAlignment; valign: TTextLayout): TRect;
var fx: TBGRATextEffect;
    f: TFont;
    ofs: integer;
    previousClip: TRect;
begin
  f := TFont.Create;
  f.Name := 'Arial';
  f.Height := DoScaleY(16,OriginalDPI);
  ofs := DoScaleX(4,OriginalDPI);
  fx := TBGRATextEffect.Create(s,f,true);
  if valign = tlBottom then y := y-fx.TextSize.cy else
  if valign = tlCenter then y := y-fx.TextSize.cy div 2;
  if y+fx.TextSize.cy > bmpHeight then y := bmpHeight-fx.TextSize.cy;
  if y < 0 then y := 0;
  if align = taRightJustify then x := x-fx.TextSize.cx else
  if align = taCenter then x := x-fx.TextSize.cx div 2;
  if x+fx.TextSize.cx > bmpWidth then x := bmpWidth-fx.TextSize.cx;
  if x < 0 then x := 0;
  result := rect(x,y,x+fx.TextWidth+2*ofs,y+fx.TextHeight+2*ofs);
  if Assigned(bmp) then
  begin
    previousClip := bmp.ClipRect;
    bmp.ClipRect := result;
    fx.DrawShadow(bmp,x+ofs,y+ofs,ofs,BGRABlack);
    fx.DrawOutline(bmp,x,y,BGRABlack);
    fx.Draw(bmp,x,y,BGRAWhite);
    bmp.ClipRect := previousClip;
  end;
  fx.Free;
  f.Free;
end;

function ComputeColorCircle(tx, ty: integer; light: word; hueCorrection: boolean = true): TBGRABitmap;
var xb,yb : integer;
    pdest: PBGRAPixel;
    angle,xc,yc: single;
    ec: TExpandedPixel;
    c: TBGRAPixel;
    gray,level: Word;
begin
  result := TBGRABitmap.Create(tx,ty);
  result.FillEllipseAntialias(tx/2-0.5,ty/2-0.5,tx/2,ty/2,BGRABlack);
  xc := tx/2-0.5;
  yc := ty/2-0.5;
  for yb := 0 to ty-1 do
  begin
    pdest := result.scanline[yb];
    For xb := 0 to tx-1 do
    begin
      if pdest^.alpha <> 0 then
      begin
        ec.alpha := $FFFF;
        angle := ComputeAngle(xb-xc,yb-yc);
        if angle < 0 then angle += 360;
        if hueCorrection then
          angle := GtoH(round(angle/360*65536) and 65535)/65536*360;
        if angle < 60 then
        begin
          ec.red := $FFFF;
          ec.green := round(angle/60*$FFFF);
          ec.blue := $0000;
        end else
        if angle < 120 then
        begin
          ec.red := $FFFF-round((angle-60)/60*$FFFF);
          ec.green := $FFFF;
          ec.blue := $0000;
        end else
        if angle < 180 then
        begin
          ec.red := $0000;
          ec.green := $FFFF;
          ec.blue := round((angle-120)/60*$FFFF);
        end else
        if angle < 240 then
        begin
          ec.red := $0000;
          ec.green := $FFFFF-round((angle-180)/60*$FFFF);
          ec.blue := $FFFF;
        end else
        if angle < 300 then
        begin
          ec.red := round((angle-240)/60*$FFFF);
          ec.green := $0000;
          ec.blue := $FFFF;
        end else
        begin
          ec.red := $FFFF;
          ec.green := $0000;
          ec.blue := $FFFFF-round((angle-300)/60*$FFFF);
        end;
        gray := min($FFFF,max(0,$FFFF - round((sqrt(sqr((xb-xc)/(tx/2))+sqr((yb-yc)/(ty/2)))*1.2-0.1)*$FFFF)));
        level := max(max(ec.red,ec.green),ec.blue);
        {$hints off}
        ec.red := (ec.red*($FFFF-gray)+level*gray) shr 16;
        ec.green := (ec.green*($FFFF-gray)+level*gray) shr 16;
        ec.blue := (ec.blue*($FFFF-gray)+level*gray) shr 16;
        {$hints on}
        ec.red := (ec.red*light) shr 16;
        ec.green := (ec.green*light) shr 16;
        ec.blue := (ec.blue*light) shr 16;
        c := GammaCompression(ec);
        c.alpha := pdest^.alpha;
        pdest^ := c;
      end;
      inc(pdest);
    end;
  end;
end;

function ChangeCanvasSize(bmp: TBGRABitmap; newWidth, newHeight: integer;
  anchor: string; background: TBGRAPixel; repeatImage: boolean; flipMode: boolean = false): TBGRABitmap;
var origin: TPoint;
    xb,yb: integer;
    dx,dy: integer;
    minx,miny,maxx,maxy: integer;
    flippedImages: array[Boolean,Boolean] of TBGRABitmap;
begin
   if (newWidth < 1) or (newHeight < 1) then
     raise exception.Create('Invalid canvas size');
   origin := Point((newWidth-bmp.Width) div 2,(newHeight-bmp.Height) div 2);
   anchor := UTF8LowerCase(anchor);
   if (anchor='topleft') or (anchor='top') or (anchor='topright') then origin.Y := 0;
   if (anchor='bottomleft') or (anchor='bottom') or (anchor='bottomright') then origin.Y := newHeight-bmp.Height;
   if (anchor='topleft') or (anchor='left') or (anchor='bottomleft') then origin.X := 0;
   if (anchor='topright') or (anchor='right') or (anchor='bottomright') then origin.X := newWidth-bmp.Width;
   result := TBGRABitmap.Create(newWidth,newHeight, background);
   dx := bmp.Width;
   dy := bmp.Height;
   if repeatImage then
   begin
     minx := (0-origin.X-bmp.Width+1) div bmp.Width;
     miny := (0-origin.Y-bmp.Height+1) div bmp.Height;
     maxx := (newWidth-origin.X+bmp.Width-1) div bmp.Width;
     maxy := (newHeight-origin.Y+bmp.Height-1) div bmp.Height;
   end else
   begin
     minx := 0;
     miny := 0;
     maxx := 0;
     maxy := 0;
   end;
   if flipMode and repeatImage then
   begin
     flippedImages[false,false] := bmp;
     if (minx <> 0) or (miny <> 0) or (maxx <> 0) or (maxy <> 0) then
     begin
       flippedImages[true,false] := bmp.Duplicate as TBGRABitmap;
       flippedImages[true,false].HorizontalFlip;
       flippedImages[true,true] := flippedImages[true,false].Duplicate as TBGRABitmap;
       flippedImages[true,true].VerticalFlip;
       flippedImages[false,true] := bmp.Duplicate as TBGRABitmap;
       flippedImages[false,true].VerticalFlip;
     end else
     begin
       flippedImages[true,false] := nil;  //never used
       flippedImages[true,true] := nil;
       flippedImages[false,true] := nil;
     end;
     for xb := minx to maxx do
       for yb := miny to maxy do
        result.PutImage(origin.x+xb*dx,origin.Y+yb*dy,flippedImages[odd(xb),odd(yb)],dmSet);
     flippedImages[true,false].free;
     flippedImages[true,true].free;
     flippedImages[false,true].free;
   end else
   begin
     for xb := minx to maxx do
       for yb := miny to maxy do
        result.PutImage(origin.x+xb*dx,origin.Y+yb*dy,bmp,dmSet);
   end;
end;

initialization

  Randomize;

end.
