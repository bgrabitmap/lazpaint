unit BGRAText;

{$mode objfpc}{$H+}

interface

{ Text functions use a temporary bitmap where the operating system text drawing is used.
  Then it is scaled down (if antialiasing is activated), and colored.

  These routines are rather slow. }

uses
  Classes, Types, SysUtils, Graphics, BGRABitmapTypes;

procedure BGRATextOut(bmp: TBGRACustomBitmap; Font: TFont; Quality: TBGRAFontQuality; xf, yf: single; s: string;
  c: TBGRAPixel; tex: IBGRAScanner; align: TAlignment; CustomAntialiasingLevel: Integer = 0);

procedure BGRATextOutAngle(bmp: TBGRACustomBitmap; Font: TFont; Quality: TBGRAFontQuality; xf, yf: single; orientation: integer;
  s: string; c: TBGRAPixel; tex: IBGRAScanner; align: TAlignment; CustomAntialiasingLevel: Integer = 0);

procedure BGRATextRect(bmp: TBGRACustomBitmap; Font: TFont; Quality: TBGRAFontQuality; ARect: TRect; x, y: integer;
  s: string; style: TTextStyle; c: TBGRAPixel; tex: IBGRAScanner; CustomAntialiasingLevel: Integer = 0);

function BGRATextSize(Font: TFont; Quality: TBGRAFontQuality; s: string; CustomAntialiasingLevel: Integer): TSize;

function BGRAOriginalTextSize(Font: TFont; Quality: TBGRAFontQuality; s: string; CustomAntialiasingLevel: integer): TSize;

function GetFontHeightSign(AFont: TFont): integer;
function FontEmHeightSign: integer;
function FontFullHeightSign: integer;

procedure BGRAFillClearTypeMask(dest: TBGRACustomBitmap; x,y: integer; xThird: integer; mask: TBGRACustomBitmap; color: TBGRAPixel; texture: IBGRAScanner = nil; RGBOrder: boolean=true);
procedure BGRAFillClearTypeRGBMask(dest: TBGRACustomBitmap; x,y: integer; mask: TBGRACustomBitmap; color: TBGRAPixel; texture: IBGRAScanner = nil; KeepRGBOrder: boolean=true);

const FontAntialiasingLevel = 6;
const FontDefaultQuality = fqAntialiased;

function GetFontPixelMetric(AFont: TFont): TFontPixelMetric;

implementation

uses Math, BGRABlend;

const MaxPixelMetricCount = 100;

var
  TempBmp: TBitmap;
  FontHeightSignComputed: boolean;
  FontHeightSignValue: integer;
  FontPixelMetricArray: array[0..MaxPixelMetricCount-1] of record
                          usage: integer;
                          name: string;
                          height: integer;
                          italic: boolean;
                          bold: boolean;
                          metric: TFontPixelMetric;
                        end;
  FontPixelMetricCount: integer;

procedure ComputeFontVerticalBounds(text: string; font: TFont; out top, bottom, totalHeight: integer);
var
  xb,yb: integer;
  pmask: PBGRAPixel;
  nbPix: array of integer;
  nbCur: integer;
  mean: integer;
  mask: TBGRACustomBitmap;
  size: TSize;
begin
  size := BGRAOriginalTextSize(font,fqSystem,text,FontAntialiasingLevel);
  mask := BGRABitmapFactory.Create(size.cx,size.cy,BGRABlack);
  mask.Canvas.Font := font;
  mask.Canvas.Font.Quality := fqAntialiased;
  mask.Canvas.Font.Color := clWhite;
  mask.Canvas.Font.Style := font.style * [fsBold,fsItalic];
  mask.Canvas.Brush.Style := bsClear;
  mask.Canvas.TextOut(0,0,text);
  top := -1;
  bottom := -1;
  totalHeight:= mask.Height;

  mean := 0;
  setlength(nbPix, mask.Height);
  for yb := 0 to mask.Height-1 do
  begin
    pmask := mask.scanline[yb];
    nbCur := 0;
    for xb := 0 to mask.Width-1 do
    begin
      if (pmask^.green > 0) then inc(nbCur);
      inc(pmask);
    end;
    nbPix[yb] := nbCur;
    inc(mean,nbCur);
  end;
  mean := (mean+ (mask.Height div 2)) div mask.Height;

  for yb := 0 to high(nbPix) do
  begin
    if nbPix[yb]> mean div 3 then
    begin
      if top = -1 then top := yb
      else bottom := yb+1;
    end;
  end;
  mask.Free;
end;

function ComputeFontPixelMetric(AFont: TFont): TFontPixelMetric;
begin
  ComputeFontVerticalBounds('acemu',AFont,result.xLine,result.Baseline,result.Lineheight);
  ComputeFontVerticalBounds('gDjSO',AFont,result.CapLine,result.DescentLine,result.Lineheight);
  if result.xLine = -1 then result.xLine := result.CapLine else
  if result.CapLine = -1 then result.CapLine := result.xLine;
  if result.DescentLine = -1 then result.DescentLine := result.Baseline else
  if result.Baseline = -1 then result.Baseline := result.DescentLine;
  result.Defined := (result.xLine <> -1) and (result.CapLine <> -1) and (result.Baseline <> -1) and (result.DescentLine <> -1) and
     (result.Lineheight <> -1);
end;

function ComparePixelMetric(index: integer; font: TFont): integer;
begin
  if (index < 0) or (index >= FontPixelMetricCount) then
    result := 0
  else
  begin
    with FontPixelMetricArray[index] do
      if (name = font.Name) and (height = font.Height) then
        result := 0 else
      if (height > font.Height) then
        result := 1 else
      if (height < font.Height) then
        result := -1 else
      if name > font.Name then
        result := 1 else
      if name < font.Name then
        result := -1
      else result := 0;
  end;
end;

procedure FindPixelMetricPos(AFont: TFont; out startPos,endPos: integer);
var middle,iStart,iEnd: integer;
begin
  if FontPixelMetricCount = 0 then
  begin
    startPos := 0;
    endPos := 0;
  end;
  iStart:= 0;
  iEnd:= FontPixelMetricCount;
  while iStart < iEnd do
  begin
    middle := (iStart+iEnd) div 2;
    if ComparePixelMetric(middle,AFont) >= 0 then
      iEnd := middle
    else
      iStart := middle+1;
  end;
  startPos := iStart;

  iStart:= startPos;
  iEnd:= FontPixelMetricCount;
  while iStart < iEnd do
  begin
    middle := (iStart+iEnd) div 2;
    if ComparePixelMetric(middle,AFont) <= 0 then
      iStart := middle+1
    else
      iEnd := middle;
  end;
  endPos := iEnd;
end;

procedure RemoveOldPixelMetric;
var sum,nb,i: integer;
begin
  if FontPixelMetricCount = 0 then exit;
  sum := 0;
  for i := 0 to FontPixelMetricCount-1 do
    sum += FontPixelMetricArray[i].usage;
  sum := sum div FontPixelMetricCount;
  nb := 0;
  for i := 0 to FontPixelMetricCount-1 do
  begin
    if FontPixelMetricArray[i].usage > sum then
    begin
      FontPixelMetricArray[nb] := FontPixelMetricArray[i];
      inc(nb);
    end;
  end;
  FontPixelMetricCount := nb;
end;

function GetFontPixelMetric(AFont: TFont): TFontPixelMetric;
var i,startPos,endPos: integer;
begin
  FindPixelMetricPos(AFont,startPos,endPos);
  for i := startPos to endPos-1 do
    if (FontPixelMetricArray[i].bold = AFont.bold) and
      (FontPixelMetricArray[i].italic = AFont.Italic) then
    begin
      result := FontPixelMetricArray[i].metric;
      inc(FontPixelMetricArray[i].usage);
      exit;
    end;
  if FontPixelMetricCount = MaxPixelMetricCount then RemoveOldPixelMetric;
  for i := FontPixelMetricCount downto endPos+1 do
    FontPixelMetricArray[i] := FontPixelMetricArray[i-1];
  inc(FontPixelMetricCount);
  with FontPixelMetricArray[endPos]do
  begin
    italic := AFont.Italic;
    bold := AFont.Bold;
    usage := 1;
    name := AFont.Name;
    height:= AFont.Height;
    metric := ComputeFontPixelMetric(AFont);
    result := metric;
  end;
end;

function GetFontHeightSign(AFont: TFont): integer;
var
  HeightP1, HeightM1: integer;
begin
  if FontHeightSignComputed then
  begin
    result := FontHeightSignValue;
    exit;
  end;

  if tempBmp = nil then tempBmp := TBitmap.Create;
  tempBmp.Canvas.Font.Assign(AFont);
  tempBmp.Canvas.Font.Height := 20;
  HeightP1  := tempBmp.Canvas.TextExtent('Hg').cy;
  tempBmp.Canvas.Font.Height := -20;
  HeightM1  := tempBmp.Canvas.TextExtent('Hg').cy;

  if HeightP1 > HeightM1 then
    FontHeightSignValue := 1
  else
    FontHeightSignValue := -1;
  FontHeightSignComputed := true;
  result := FontHeightSignValue;
end;

function FontEmHeightSign: integer;
var f: TFont;
begin
  if FontHeightSignComputed then
  begin
    result := FontHeightSignValue;
    exit;
  end;
  f:= TFont.Create;
  f.Name := 'Arial';
  result := GetFontHeightSign(f);
  f.Free;
end;

function FontFullHeightSign: integer;
begin
  result := -FontEmHeightSign;
end;

procedure BGRAFillClearTypeMask(dest: TBGRACustomBitmap; x,y: integer; xThird: integer; mask: TBGRACustomBitmap; color: TBGRAPixel; texture: IBGRAScanner; RGBOrder: boolean);
var
  pdest: PBGRAPixel;
  ClearTypePixel: array[0..2] of byte;
  curThird: integer;

  procedure OutputPixel; inline;
  begin
    if texture <> nil then
      color := texture.ScanNextPixel;
    if RGBOrder then
      ClearTypeDrawPixel(pdest, ClearTypePixel[0],ClearTypePixel[1],ClearTypePixel[2], color)
    else
      ClearTypeDrawPixel(pdest, ClearTypePixel[2],ClearTypePixel[1],ClearTypePixel[0], color);
  end;

  procedure NextAlpha(alphaValue: byte); inline;
  begin
    ClearTypePixel[curThird] := alphaValue;
    inc(curThird);
    if curThird = 3 then
    begin
      OutputPixel;
      curThird := 0;
      Fillchar(ClearTypePixel, sizeof(ClearTypePixel),0);
      inc(pdest);
    end;
  end;

  procedure EndRow; inline;
  begin
    if curThird > 0 then OutputPixel;
  end;

var
  yMask,n: integer;
  a: byte;
  pmask: PBGRAPixel;
  dx:integer;
  miny,maxy,minx,minxThird,maxx,alphaMinX,alphaMaxX,alphaLineLen: integer;
  leftOnSide, rightOnSide: boolean;
  countBetween: integer;
  v1,v2,v3: byte;

  procedure StartRow; inline;
  begin
    pdest := dest.Scanline[yMask+y]+minx;
    if texture <> nil then
      texture.ScanMoveTo(minx,yMask+y);

    curThird := minxThird;
    ClearTypePixel[0] := 0;
    ClearTypePixel[1] := 0;
    ClearTypePixel[2] := 0;
  end;

begin
  alphaLineLen := mask.Width+2;

  xThird -= 1; //for first subpixel

  if xThird >= 0 then dx := xThird div 3
   else dx := -((-xThird+2) div 3);
  x += dx;
  xThird -= dx*3;

  if y >= dest.ClipRect.Top then miny := 0
    else miny := dest.ClipRect.Top-y;
  if y+mask.Height-1 < dest.ClipRect.Bottom then
    maxy := mask.Height-1 else
      maxy := dest.ClipRect.Bottom-1-y;

  if x >= dest.ClipRect.Left then
  begin
    minx := x;
    minxThird := xThird;
    alphaMinX := 0;
    leftOnSide := false;
  end else
  begin
    minx := dest.ClipRect.Left;
    minxThird := 0;
    alphaMinX := (dest.ClipRect.Left-x)*3 - xThird;
    leftOnSide := true;
  end;

  if x*3+xThird+mask.Width-1 < dest.ClipRect.Right*3 then
  begin
    maxx := (x*3+xThird+mask.Width-1) div 3;
    alphaMaxX := alphaLineLen-1;
    rightOnSide := false;
  end else
  begin
    maxx := dest.ClipRect.Right-1;
    alphaMaxX := maxx*3+2 - (x*3+xThird);
    rightOnSide := true;
  end;

  countBetween := alphaMaxX-alphaMinX-1;

  if (alphaMinX <= alphaMaxX) then
  begin
    for yMask := miny to maxy do
    begin
      StartRow;

      if leftOnSide then
      begin
        pmask := mask.ScanLine[yMask]+(alphaMinX-1);
        a := pmask^.green div 3;
        v1 := a+a;
        v2 := a;
        v3 := 0;
        inc(pmask);
      end else
      begin
        pmask := mask.ScanLine[yMask];
        v1 := 0;
        v2 := 0;
        v3 := 0;
      end;

      for n := countBetween-1 downto 0 do
      begin
        a := pmask^.green div 3;
        v1 += a;
        v2 += a;
        v3 += a;
        inc(pmask);

        NextAlpha(v1);
        v1 := v2;
        v2 := v3;
        v3 := 0;
      end;

      if rightOnSide then
      begin
        a := pmask^.green div 3;
        v1 += a;
        v2 += a+a;
      end;

      NextAlpha(v1);
      NextAlpha(v2);

      EndRow;
    end;
  end;
end;

procedure BGRAFillClearTypeRGBMask(dest: TBGRACustomBitmap; x, y: integer;
  mask: TBGRACustomBitmap; color: TBGRAPixel; texture: IBGRAScanner;
  KeepRGBOrder: boolean);
var
  minx,miny,maxx,maxy,countx,n,yb: integer;
  pdest,psrc: PBGRAPixel;
begin
  if y >= dest.ClipRect.Top then miny := 0
    else miny := dest.ClipRect.Top-y;
  if y+mask.Height-1 < dest.ClipRect.Bottom then
    maxy := mask.Height-1 else
      maxy := dest.ClipRect.Bottom-1-y;

  if x >= dest.ClipRect.Left then minx := 0
    else minx := dest.ClipRect.Left-x;
  if x+mask.Width-1 < dest.ClipRect.Right then
    maxx := mask.Width-1 else
      maxx := dest.ClipRect.Right-1-x;

  countx := maxx-minx+1;
  if countx <= 0 then exit;

  for yb := miny to maxy do
  begin
    pdest := dest.ScanLine[y+yb]+(x+minx);
    psrc := mask.ScanLine[yb]+minx;
    if texture <> nil then
      texture.ScanMoveTo(x+minx, y+yb);
    if KeepRGBOrder then
    begin
      for n := countx-1 downto 0 do
      begin
        if texture <> nil then color := texture.ScanNextPixel;
        ClearTypeDrawPixel(pdest, psrc^.red, psrc^.green, psrc^.blue, color);
        inc(pdest);
        inc(psrc);
      end;
    end else
    begin
      for n := countx-1 downto 0 do
      begin
        if texture <> nil then color := texture.ScanNextPixel;
        ClearTypeDrawPixel(pdest, psrc^.blue, psrc^.green, psrc^.red, color);
        inc(pdest);
        inc(psrc);
      end;
    end;
  end;
end;

function BGRAOriginalTextSize(Font: TFont; Quality: TBGRAFontQuality; s: string; CustomAntialiasingLevel: Integer): TSize;
begin
  if tempBmp = nil then tempBmp := TBitmap.Create;
  tempBmp.Canvas.Font := Font;
  if Quality in[fqFineClearTypeBGR,fqFineClearTypeRGB,fqFineAntialiasing] then tempBmp.Canvas.Font.Height := Font.Height*CustomAntialiasingLevel else
    tempBmp.Canvas.Font.Height := Font.Height;
  Result.cx := 0;
  Result.cy := 0;
  tempBmp.Canvas.Font.GetTextSize(s, Result.cx, Result.cy);
end;

function BGRATextSize(Font: TFont; Quality: TBGRAFontQuality; s: string; CustomAntialiasingLevel: Integer): TSize;
begin
  result := BGRAOriginalTextSize(Font, Quality, s, CustomAntialiasingLevel);
  if Quality in[fqFineClearTypeBGR,fqFineClearTypeRGB,fqFineAntialiasing] then
  begin
    result.cx := ceil(Result.cx/CustomAntialiasingLevel);
    result.cy := ceil(Result.cy/CustomAntialiasingLevel);
  end;
end;

procedure FilterOriginalText(Quality: TBGRAFontQuality; CustomAntialiasingLevel: Integer; var temp: TBGRACustomBitmap;
  c: TBGRAPixel; tex: IBGRAScanner);
var
  resampled: TBGRACustomBitmap;
  P:       PBGRAPixel;
  n,xb,yb,v: integer;
  alpha, maxAlpha: integer;
begin
  case Quality of
  fqFineClearTypeBGR,fqFineClearTypeRGB:
    begin
      if (temp.Height < CustomAntialiasingLevel*8) and (temp.Height >= CustomAntialiasingLevel*3) then
      begin
        temp.ResampleFilter := rfSpline;
        resampled := temp.Resample(round(temp.width/CustomAntialiasingLevel*3),round(temp.Height/CustomAntialiasingLevel),rmFineResample);
      end else
        resampled := temp.Resample(round(temp.width/CustomAntialiasingLevel*3),round(temp.Height/CustomAntialiasingLevel),rmSimpleStretch);

      maxAlpha := 0;
      p := resampled.Data;
      for n := resampled.NbPixels - 1 downto 0 do
      begin
        alpha    := P^.green;
        if alpha > maxAlpha then maxAlpha := alpha;
        Inc(p);
      end;
      if maxAlpha <> 0 then
      begin
        p := resampled.Data;
        for n := resampled.NbPixels - 1 downto 0 do
        begin
          v:= integer(p^.green * 255) div maxAlpha;
          p^.red := v;
          p^.green := v;
          p^.blue := v;
          Inc(p);
        end;
      end;
      temp.Free;
      temp := resampled;
    end;
  fqFineAntialiasing:
    begin
      if (temp.Height < CustomAntialiasingLevel*8) and (temp.Height >= CustomAntialiasingLevel*3) then
      begin
        temp.ResampleFilter := rfSpline;
        resampled := temp.Resample(round(temp.width/CustomAntialiasingLevel),round(temp.Height/CustomAntialiasingLevel),rmFineResample);
      end else
        resampled := temp.Resample(round(temp.width/CustomAntialiasingLevel),round(temp.Height/CustomAntialiasingLevel),rmSimpleStretch);

      maxAlpha := 0;
      if tex = nil then
      begin
        p := resampled.Data;
        for n := resampled.NbPixels - 1 downto 0 do
        begin
          alpha    := P^.green;
          if alpha > maxAlpha then maxAlpha := alpha;
          if alpha = 0 then
            p^:= BGRAPixelTransparent else
          begin
            p^.red   := c.red;
            p^.green := c.green;
            p^.blue  := c.blue;
            p^.alpha := alpha;
          end;
          Inc(p);
        end;

        if maxAlpha <> 0 then
        begin
          p := resampled.Data;
          for n := resampled.NbPixels - 1 downto 0 do
          begin
            p^.alpha := integer(p^.alpha * c.alpha) div maxAlpha;
            Inc(p);
          end;
        end;
      end else
      begin
        p := resampled.Data;
        for n := resampled.NbPixels - 1 downto 0 do
        begin
          alpha    := P^.green;
          if alpha > maxAlpha then maxAlpha := alpha;
          Inc(p);
        end;
        if maxAlpha = 0 then
          resampled.FillTransparent
        else
          for yb := 0 to resampled.Height-1 do
          begin
            p := resampled.ScanLine[yb];
            tex.ScanMoveTo(0,yb);
            for xb := 0 to resampled.Width-1 do
            begin
              c := tex.ScanNextPixel;
              alpha    := integer(P^.green*c.alpha) div maxAlpha;
              if alpha = 0 then
                p^:= BGRAPixelTransparent else
              begin
                c.alpha := alpha;
                p^ := c;
              end;
              Inc(p);
            end;
          end;
      end;

      temp.Free;
      temp := resampled;
    end;
  fqSystem:
    begin
      if tex = nil then
      begin
        p := temp.Data;
        for n := temp.NbPixels - 1 downto 0 do
        begin
          alpha    := GammaExpansionTab[P^.green] shr 8;
          alpha    := (c.alpha * alpha) div (255);
          if alpha = 0 then p^:= BGRAPixelTransparent else
          begin
            p^.red   := c.red;
            p^.green := c.green;
            p^.blue  := c.blue;
            p^.alpha := alpha;
          end;
          Inc(p);
        end;
      end else
      begin
        for yb := 0 to temp.Height-1 do
        begin
          p := temp.Scanline[yb];
          tex.ScanMoveTo(0,yb);
          for xb := 0 to temp.Width-1 do
          begin
            c := tex.ScanNextPixel;
            alpha    := GammaExpansionTab[P^.green] shr 8;
            alpha    := (c.alpha * alpha) div (255);
            if alpha = 0 then p^:= BGRAPixelTransparent else
            begin
              p^.red   := c.red;
              p^.green := c.green;
              p^.blue  := c.blue;
              p^.alpha := alpha;
            end;
            Inc(p);
          end;
        end;
      end;
    end;
  end;
end;

procedure BGRATextOut(bmp: TBGRACustomBitmap; Font: TFont; Quality: TBGRAFontQuality; xf, yf: single; s: string;
  c: TBGRAPixel; tex: IBGRAScanner; align: TAlignment; CustomAntialiasingLevel: Integer = 0);
var
  size: TSize;
  temp: TBGRACustomBitmap;
  xMargin,xThird: integer;
  tempSize: TSize;
  subX,subY: integer;
  x,y :integer;
  deltaX: single;
begin
  if CustomAntialiasingLevel = 0 then
    CustomAntialiasingLevel:= FontAntialiasingLevel;

  if Font.Orientation mod 3600 <> 0 then
  begin
    BGRATextOutAngle(bmp,Font,Quality,xf,yf,Font.Orientation,s,c,tex,align);
    exit;
  end;

  size := BGRAOriginalTextSize(Font,Quality,s,CustomAntialiasingLevel);
  if (size.cx = 0) or (size.cy = 0) then
    exit;

  if (size.cy >= 144) and (Quality in[fqFineAntialiasing,fqFineClearTypeBGR,fqFineClearTypeRGB]) and (CustomAntialiasingLevel > 4) then
  begin
    BGRATextOut(bmp,Font,Quality,xf,yf,s,c,tex,align,4);
    exit;
  end;

  if Quality in[fqFineAntialiasing,fqFineClearTypeBGR,fqFineClearTypeRGB] then
  begin
    case align of
      taLeftJustify: ;
      taCenter: xf -= size.cx/2/CustomAntialiasingLevel;
      taRightJustify: xf -= size.cx/CustomAntialiasingLevel;
    end;
  end else
  begin
    case align of
      taLeftJustify: ;
      taCenter: xf -= size.cx/2;
      taRightJustify: xf -= size.cx;
    end;
  end;

  x := round(xf);
  y := round(yf);

  xThird := 0;
  tempSize.cx := size.cx;
  tempSize.cy := size.cy;
  if Quality in[fqFineAntialiasing,fqFineClearTypeBGR,fqFineClearTypeRGB] then
  begin
    tempSize.cx += CustomAntialiasingLevel-1;
    tempSize.cx -= tempSize.cx mod CustomAntialiasingLevel;
    tempSize.cy += CustomAntialiasingLevel-1;
    tempSize.cy -= tempSize.cy mod CustomAntialiasingLevel;

    deltaX := xf-floor(xf);
    if Quality in [fqFineClearTypeBGR,fqFineClearTypeRGB] then
    begin
      xThird := floor(deltaX*3) mod 3;
      deltaX -= xThird/3;
    end;
    subX := round(CustomAntialiasingLevel*deltaX);
    x := round(floor(xf));
    if subX <> 0 then inc(tempSize.cx, CustomAntialiasingLevel);
    subY := round(CustomAntialiasingLevel*(yf-floor(yf)));
    y := round(floor(yf));
    if subY <> 0 then inc(tempSize.cy, CustomAntialiasingLevel);
  end else
  begin
    subX := 0;
    subY := 0;
  end;

  xMargin := size.cy div 2;
  if Quality in[fqFineAntialiasing,fqFineClearTypeBGR,fqFineClearTypeRGB] then
  begin
    xMargin += CustomAntialiasingLevel-1;
    xMargin -= xMargin mod CustomAntialiasingLevel;
  end;
  tempSize.cx += xMargin*2;

  temp := bmp.NewBitmap(tempSize.cx, tempSize.cy, BGRABlack);
  temp.Canvas.Font := Font;
  if Quality in[fqFineAntialiasing,fqFineClearTypeBGR,fqFineClearTypeRGB] then temp.Canvas.Font.Height := Font.Height*CustomAntialiasingLevel
   else temp.Canvas.Font.Height := Font.Height;
  temp.Canvas.Font.Color := clWhite;
  temp.Canvas.Brush.Style := bsClear;
  temp.Canvas.TextOut(xMargin+subX, subY, s);

  FilterOriginalText(Quality,CustomAntialiasingLevel, temp,c,tex);

  if Quality in [fqFineClearTypeBGR,fqFineClearTypeRGB] then
    BGRAFillClearTypeMask(bmp,x-round(xMargin/CustomAntialiasingLevel),y,xThird, temp,c,tex,Quality=fqFineClearTypeRGB)
  else
  begin
    if Quality = fqSystemClearType then
      BGRAFillClearTypeRGBMask(bmp,x-xMargin,y, temp,c,tex)
    else if Quality = fqFineAntialiasing then
      bmp.PutImage(x-round(xMargin/CustomAntialiasingLevel), y, temp, dmDrawWithTransparency)
    else bmp.PutImage(x-xMargin, y, temp, dmDrawWithTransparency);
  end;
  temp.Free;
end;

procedure BGRATextOutAngle(bmp: TBGRACustomBitmap; Font: TFont; Quality: TBGRAFontQuality; xf, yf: single; orientation: integer;
  s: string; c: TBGRAPixel; tex: IBGRAScanner; align: TAlignment; CustomAntialiasingLevel: Integer = 0);
var
  x,y: integer;
  deltaX,deltaY: integer;
  size: TSize;
  temp: TBGRACustomBitmap;
  TopRight,BottomRight,BottomLeft: TPointF;
  Top: Single;
  Left: Single;
  cosA,sinA: single;
  rotBounds: TRect;
  sizeFactor: integer;
  TempFont: TFont;
  oldOrientation: integer;

  procedure rotBoundsAdd(pt: TPointF);
  begin
    if pt.x < Left then Left := pt.x;
    if pt.y < Top then Top := pt.y;
    if floor(pt.X) < rotBounds.Left then rotBounds.Left := floor(pt.X/sizeFactor)*sizeFactor;
    if floor(pt.Y) < rotBounds.Top then rotBounds.Top := floor(pt.Y/sizeFactor)*sizeFactor;
    if ceil(pt.X) > rotBounds.Right then rotBounds.Right := ceil(pt.X/sizeFactor)*sizeFactor;
    if ceil(pt.Y) > rotBounds.Bottom then rotBounds.Bottom := ceil(pt.Y/sizeFactor)*sizeFactor;
  end;

begin
  if CustomAntialiasingLevel = 0 then
    CustomAntialiasingLevel:= FontAntialiasingLevel;

  if orientation mod 3600 = 0 then
  begin
    oldOrientation := Font.Orientation;
    Font.Orientation := 0;
    BGRATextOut(bmp,Font,Quality,xf,yf,s,c,tex,align);
    Font.Orientation := oldOrientation;
    exit;
  end;
  TempFont := TFont.Create;
  TempFont.Assign(Font);
  TempFont.Orientation := orientation;
  TempFont.Height := Font.Height;
  size := BGRAOriginalTextSize(TempFont,Quality,s,CustomAntialiasingLevel);
  if (size.cx = 0) or (size.cy = 0) then
  begin
    tempFont.Free;
    exit;
  end;
  if Quality in[fqFineAntialiasing,fqFineClearTypeBGR,fqFineClearTypeRGB] then
    sizeFactor := CustomAntialiasingLevel
  else
    sizeFactor := 1;

  cosA := cos(orientation*Pi/1800);
  sinA := sin(orientation*Pi/1800);
  TopRight := PointF(cosA*size.cx,-sinA*size.cx);
  BottomRight := PointF(cosA*size.cx+sinA*size.cy,cosA*size.cy-sinA*size.cx);
  BottomLeft := PointF(sinA*size.cy,cosA*size.cy);
  rotBounds := rect(0,0,0,0);
  Top := 0;
  Left := 0;
  rotBoundsAdd(TopRight);
  rotBoundsAdd(BottomRight);
  rotBoundsAdd(BottomLeft);
  inc(rotBounds.Right);
  inc(rotBounds.Bottom);

  xf += Left/sizeFactor;
  yf += Top/sizeFactor;
  case align of
    taLeftJustify: ;
    taCenter:
      begin
        xf -= TopRight.x/2/sizeFactor;
        yf -= TopRight.y/2/sizeFactor;
      end;
    taRightJustify:
      begin
        xf -= TopRight.x/sizeFactor;
        yf -= TopRight.y/sizeFactor;
      end;
  end;
  deltaX := round((xf - floor(xf))*sizeFactor);
  x := floor(xf);
  deltaY := round((yf - floor(yf))*sizeFactor);
  y := floor(yf);
  if deltaX <> 0 then rotBounds.Right += sizeFactor;
  if deltaY <> 0 then rotBounds.Bottom += sizeFactor;

  temp := bmp.NewBitmap(rotBounds.Right-rotBounds.Left,rotBounds.Bottom-rotBounds.Top, BGRABlack);
  temp.Canvas.Font := Font;
  temp.Canvas.Font.Color := clWhite;
  temp.Canvas.Font.Orientation := orientation;
  temp.Canvas.Font.Height := round(Font.Height*sizeFactor);
  temp.Canvas.Brush.Style := bsClear;
  temp.Canvas.TextOut(-rotBounds.Left+deltaX, -rotBounds.Top+deltaY, s);

  FilterOriginalText(Quality,CustomAntialiasingLevel,temp,c,tex);

  if Quality in [fqFineClearTypeRGB,fqFineClearTypeBGR] then
    BGRAFillClearTypeMask(bmp, x, y, 0, temp, c,tex,Quality = fqFineClearTypeRGB) else
  begin
    if Quality = fqSystemClearType then
      BGRAFillClearTypeRGBMask(bmp, x, y, temp, c,tex)
    else
      bmp.PutImage(x, y, temp, dmDrawWithTransparency);
  end;
  temp.Free;
  tempFont.Free;
end;

procedure BGRATextRect(bmp: TBGRACustomBitmap; Font: TFont; Quality: TBGRAFontQuality; ARect: TRect; x, y: integer;
  s: string; style: TTextStyle; c: TBGRAPixel; tex: IBGRAScanner; CustomAntialiasingLevel: Integer = 0);
var
  lim: TRect;
  tx, ty: integer;
  temp:   TBGRACustomBitmap;
  sizeFactor: integer;
  cr: TRect;
begin
  if CustomAntialiasingLevel = 0 then
    CustomAntialiasingLevel:= FontAntialiasingLevel;

  cr := bmp.ClipRect;
  if ARect.Left < cr.Left then
    lim.Left := cr.Left else lim.Left := ARect.Left;
  if ARect.Top < cr.Top then
    lim.Top := cr.Top else lim.Top := ARect.Top;
  if ARect.Right > cr.Right then
    lim.Right := cr.Right else lim.Right := ARect.Right;
  if ARect.Bottom > cr.Bottom then
    lim.Bottom := cr.Bottom else lim.Bottom := ARect.Bottom;

  tx := lim.Right - lim.Left;
  ty := lim.Bottom - lim.Top;
  if (tx <= 0) or (ty <= 0) then
    exit;

  if Quality in[fqFineAntialiasing,fqFineClearTypeBGR,fqFineClearTypeRGB] then
    sizeFactor := CustomAntialiasingLevel
  else
    sizeFactor := 1;

  temp := bmp.NewBitmap(tx*sizeFactor, ty*sizeFactor, BGRABlack);
  temp.Canvas.Font := Font;
  temp.Canvas.Font.Orientation := 0;
  if Quality in[fqFineAntialiasing,fqFineClearTypeBGR,fqFineClearTypeRGB] then temp.Canvas.Font.Height := Font.Height*CustomAntialiasingLevel
     else temp.Canvas.Font.Height := Font.Height;
  temp.Canvas.Font.Color := clWhite;
  temp.Canvas.Brush.Style := bsClear;
  temp.Canvas.TextRect(rect(lim.Left-ARect.Left, lim.Top-ARect.Top, (ARect.Right-ARect.Left)*sizeFactor, (ARect.Bottom-ARect.Top)*sizeFactor), (x - lim.Left)*sizeFactor, (y - lim.Top)*sizeFactor, s, style);

  FilterOriginalText(Quality,CustomAntialiasingLevel,temp,c,tex);
  if Quality in [fqFineClearTypeBGR,fqFineClearTypeRGB] then
    BGRAFillClearTypeMask(bmp,lim.Left, lim.Top, 0, temp, c,tex,Quality = fqFineClearTypeRGB)
  else if Quality = fqSystemClearType then
    BGRAFillClearTypeRGBMask(bmp,lim.Left, lim.Top, temp, c,tex)
  else
    bmp.PutImage(lim.Left, lim.Top, temp, dmDrawWithTransparency);
  temp.Free;
end;

initialization

  tempBmp := nil;

finalization

  tempBmp.Free;

end.

