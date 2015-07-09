{ This file contains Lape test scripts }

procedure TestDone(s: string);
begin
  ShowMessage(s);
end;

var w,h: integer;

///////////////// pixel

procedure TestPixelAndLine;
var i : integer;
  r : TRect;
  c1,c2: TBGRAPixel;
  pt : TPoint;
begin
  FillBitmap(CSSBlack);
  Antialiasing := False;
  for i := 1 to 10 do
    DrawLine(random(w),random(h),random(w),random(h),MergeBGRA(StrToBGRA('Blue'), 11-i, StrToBGRA('Lime'), i));
  Antialiasing := True;
  for i := 1 to 10 do
    DrawLine(random(w),random(h),random(w),random(h),BGRA(0,255,0,i*255 div 10));
  Antialiasing := False;
  for i := 1 to 10 do
    EraseLine(random(w),random(h),random(w),random(h),255);
  Antialiasing := True;
  for i := 1 to 10 do
    EraseLine(random(w),random(h),random(w),random(h),255);
  
  r := RectWithSize(w div 4, h div 4, w div 2, h div 2);
  c1 := MergeBGRA([CSSYellow,CSSWhite]);
  c2 := MergeBGRA([CSSYellow,CSSRed]);
  for i := 1 to 100 do
  begin
    pt := Point(random(w),random(h));
    if PtInRect(pt,r) then
      DrawPixel(pt.x,pt.y,c1) 
	else
	  DrawPixel(pt.x,pt.y,c2);
  end;
  for i := 1 to 100 do
    ErasePixel(random(w),random(h),128);
  TestDone('DrawPixel+ErasePixel+DrawLine[Antialias]+EraseLine[Antialias]+MergeBGRA');
end;

procedure TestForEachPixel;
  procedure PixelProc(x,y: Int32; var pix: TBGRAPixel);
  var hsla: THSLAPixel;
  begin
    hsla.hue := (x shl 16) div w;
	hsla.saturation := ((y shl 17) div h) and 65535;
	hsla.alpha := 65535;
	if y > h shr 1 then
	begin
	  hsla.lightness := 48000;
	  pix := GSBAToBGRA(hsla);
	end
	else
	begin
	  hsla.lightness := 32768;
	  pix := HSLAToBGRA(hsla);
	end;
  end;
begin
  ForEachPixel(@PixelProc);
  TestDone('ForEachPixel+HSLAToBGRA+GSBAToBGRA');
end;

procedure TestScanline;
var x,y,v: integer;
  p: PBGRAPixel;
  c: TBGRAPixel;
  ec: TExpandedPixel;
begin
  for y := 0 to h-1 do
  begin
    p := GetScanLine(y);
	v := (h-1-y)*65536*2 div h;
	if v > 65535 then ec.red := 65535 else ec.red := v;
	if v > 65535 then ec.green := v-65536 else ec.green := 0;
	ec.blue := 0;
	ec.alpha := 65535;	
	c := GammaCompression(ec);
    for x := w-1 downto 0 do
	begin
	  p^ := c;
	  inc(p);
	end;
  end;
  TestDone('ScanLine+GammaCompression: yellow-red gradient');
end;

procedure TestFillRect;
const r = 10;
var i : integer;
	c: TBGRAPixel;
begin
  FillBitmap(CSSBlack);
  c := BGRA(255,255,255,0);
  for i := 1 to 10 do
  begin
    Antialiasing := Odd(i);
	if Antialiasing then
	begin
      DrawMode := dmNormal;	
      FillRoundRect(random(w),random(h),random(w),random(h),r,r,CSSOrange);
	end
	else
	begin
      DrawMode := dmXor;
      FillRoundRect(random(w),random(h),random(w),random(h),r,r,c);
	end;
  end;
  DrawMode := dmXor;
  for i := 1 to 10 do
    FillRect(random(w),random(h),random(w),random(h),c);
  DrawMode := dmNormal;
  TestDone('FillRect+FillRoundRect (r=' + IntToStr(r)+')');
  Antialiasing := true;
end;

procedure TestRect;
const n = 5; r = 10;
var i : integer;
	c: TBGRAPixel;
begin
  FillBitmap(CSSWhite);
  for i := 1 to n do
    Rectangle(random(w),random(h),random(w),random(h),BGRA(128,160,255),BGRA(0,0,255));
  for i := 1 to n do
  begin
    Antialiasing := Odd(i);
    RoundRect(random(w),random(h),random(w),random(h),r,r,BGRA(128,160,255),BGRA(0,0,255));
  end;
  for i := 1 to n do
    Rectangle(random(w),random(h),random(w),random(h),CSSBlack);
  for i := 1 to n do
  begin
    Antialiasing := Odd(i);
    RoundRect(random(w),random(h),random(w),random(h),r,r,CSSBlack);
  end;
  TestDone('Rectangle+RoundRect (r=' + IntToStr(r)+')');
  Antialiasing := true;
end;

procedure TestEllipse;
const n = 5;
var i : integer;
	c: TBGRAPixel;
begin
  FillBitmap(CSSBlack);
  for i := 1 to n do
  begin
    Antialiasing := Odd(i);
    Ellipse(random(w),random(h),random(w)/2,random(h)/2,CSSMaroon,CSSRed);
  end;
  for i := 1 to n do
  begin
    Antialiasing := Odd(i);
    FillEllipse(random(w),random(h),random(w)/2,random(h)/2,BGRA(128,160,255,64));
  end;
  for i := 1 to n do
  begin
    Antialiasing := Odd(i);
    Ellipse(random(w),random(h),random(w)/2,random(h)/2,CSSPaleTurquoise);
  end;
  TestDone('FillEllipse+Ellipse');
  Antialiasing := true;
end;

///////////// text

procedure TestTextOut;
var x,y,i,txtw,txth: integer;
  c: TBGRAPixel;
  text: string;
begin
  text := 'Hello ' + BGRAToStr(BGRA(0,0,255));
  FillBitmap(CSSWhite);
  x := BitmapWidth div 2;
  y := 0;
  txth := BitmapHeight div 5;
  SetFontFullHeight(txth);
  txtw := TextWidth(text);
  if txtw > BitmapWidth then SetFontFullHeight(txth*BitmapWidth div txtw);
  
  TextAlignment := taCenter;
  FontStyle := [];            TextOut(x,y,text,CSSBlack); inc(y, txth);
  FontStyle := [fsBold];      TextOut(x,y,text,CSSBlack); inc(y, txth);
  FontStyle := [fsItalic];    TextOut(x,y,text,CSSBlack); inc(y, txth);
  FontStyle := [fsStrikeOut]; TextOut(x,y,text,CSSBlack); inc(y, txth);
  FontStyle := [fsUnderline]; TextOut(x,y,text,CSSBlack); inc(y, txth);
  
  for i := 1 to 100 do
  begin
    x := random(w);
	y := random(h);
	FillRect(x-5,y-5,x+5,y+5,GetPixel(x,y));
  end;
  
  FontStyle := [];
  TextAlignment := taLeft;
  TestDone('TextOut+GetPixel');
end;

procedure TestTextOutAngle;
var x,y,i : integer;
begin
  FillBitmap(CSSWhite);
  x := w div 2;
  y := h div 2;
  SetFontEmHeight(20);
  TextLayout := tlCenter;
  
  SetClipRect(0,0,w,y);
  for i := 0 to 5 do
    TextOutAngle(x,y,i*3600 div 6, '  Text with angle',BGRA(192,192,192));
  SetClipRect(0,y,w,h);
  for i := 0 to 5 do
    TextOutAngle(x,y,i*3600 div 6, '  Text with angle',CSSBlack);
  NoClip;
	
  TextLayout := tlTop;

  TextLayout := tlBottom;
  TextAlignment := taCenter;
  TextOut(x, BitmapHeight, 'Text in all directions', CSSBlack);
  TextLayout := tlTop;
  TextAlignment := taLeft;
  
  TestDone('TextOutAngle+Clipping');
end;

procedure TestTextRect;
var r : TRect;  
begin
  FillBitmap(CSSWhite);
  r := rect(0,0,w,h);
  SetFontEmHeight(20);
  TextLayout := tlTop;
  TextAlignment := taLeft;
  TextRect(r, 'Top-left',CSSBlack);
  TextLayout := tlCenter;
  TextAlignment := taCenter;
  TextRect(r, 'Center',CSSBlack);
  TextLayout := tlBottom;
  TextAlignment := taRight;
  TextRect(r, 'Bottom-Right',CSSBlack);
  TextLayout := tlTop;
  TextAlignment := taLeft;
  FillBitmapAlpha(224);
  
  Antialiasing := False;
  DrawPolygon([Point(w div 2,0),Point(w-1,h-1),Point(0,h-1)],CSSRed);
  Antialiasing := True;
  DrawPolygon([Point(w div 2,h-1),Point(w-1,0),Point(0,0)],CSSGreen);
  Antialiasing := False;
  ErasePolygonOutline([Point(0,h div 2),Point(w-1,0),Point(w-1,h-1)],192);
  Antialiasing := True;
  ErasePolygonOutline([Point(w-1,h div 2),Point(0,0),Point(0,h-1)],192);
  
  TestDone('TextRect+DrawPolygon[Antialias]+ErasePolygonOutline[Antialias]');
end;

/////////////////// bitmap

procedure TestBitmap;
var mainBitmap, sprite, sprite2: TBGRABitmap;
  i: integer;
  
  procedure PixelSwapRedBlue(x,y: Int32; var pix: TBGRAPixel);
  var oldRed: byte;
  begin
    oldRed := pix.red;
    pix.red := pix.blue;
    pix.blue := oldRed;
  end;
  
begin  
  FillBitmap(CSSWhite);
  mainBitmap := SelectedBitmap;

  sprite := CreateBitmap(32,32);
  sprite.Select;
  for i := 1 to 100 do
    SetPixel(random(BitmapWidth),random(BitmapHeight),CSSBlack);	

  mainBitmap.Select;
  for i := 1 to 50 do
    PutImage(random(BitmapWidth),random(BitmapHeight), sprite, i*255 div 50);	

  sprite.Select;
  SetBitmapSize(16,16);
  FillTransparent;
  for i := 1 to 100 do
    SetPixel(random(BitmapWidth),random(BitmapHeight),CSSBlack);	

  mainBitmap.Select;
  for i := 1 to 50 do
    PutImage(random(BitmapWidth),random(BitmapHeight), sprite, i*255 div 50);	
  sprite.Free;

  sprite := CreateBitmap('testimage.png');
  sprite2 := sprite.Duplicate;
  sprite2.Select;
  ForEachPixel(@PixelSwapRedBlue);
  
  mainBitmap.Select;
  for i := 1 to 50 do
  begin
    PutImage(random(BitmapWidth),random(BitmapHeight), sprite, i*255 div 50);
    PutImage(random(BitmapWidth),random(BitmapHeight), sprite2, i*255 div 50);
  end;
  sprite.Free;
  sprite2.Free;
  
  TestDone('CreateBitmap+PutImage');
end;

procedure TestColors;
var x: integer;
  
  procedure DoStuff(var x: integer; pixProc1,pixProc2: TForEachPixelProc);
  var
    mainBitmap, sprite,sprite2: TBGRABitmap;
    tx,ty: integer;  
  begin
  mainBitmap := SelectedBitmap;  
  sprite := CreateBitmap('testimage.png');
  sprite.Select;
  tx := BitmapWidth;
  ty := BitmapHeight;  
  sprite2 := sprite.Duplicate;
  
  sprite.Select;
  ForEachPixel(@pixProc1);  
  mainBitmap.Select;
  PutImage(x,0,sprite);
  sprite.Select;
  ForEachPixel(@pixProc1);  
  mainBitmap.Select;
  PutImage(x,ty,sprite);
  sprite.Select;
  ForEachPixel(@pixProc1);  
  mainBitmap.Select;
  PutImage(x,2*ty,sprite);
  inc(x, tx);

  sprite2.Select;
  ForEachPixel(@pixProc2);  
  mainBitmap.Select;
  PutImage(x,0,sprite2);
  sprite2.Select;
  ForEachPixel(@pixProc2);  
  mainBitmap.Select;
  PutImage(x,ty,sprite2);
  sprite2.Select;
  ForEachPixel(@pixProc2);  
  mainBitmap.Select;
  PutImage(x,2*ty,sprite2);
  inc(x, tx);
  
  sprite.Free;
  sprite2.Free;
  end;

  procedure IntensityNotExpanded(x,y: Int32; var pix: TBGRAPixel);
  begin
    pix := SetIntensity(pix, GetIntensity(pix)*3 div 4);
  end;

  procedure IntensityExpanded(x,y: Int32; var pix: TBGRAPixel);
  begin //should be the same, but via explicit conversion between TBGRAPixel and TExpandedPixel
    pix := GammaCompression(SetIntensity(GammaExpansion(pix), GetIntensity(GammaExpansion(pix))*3 div 4));
  end;

  procedure LightnessNotExpanded(x,y: Int32; var pix: TBGRAPixel);
  begin
    pix := SetLightness(pix, GetLightness(pix)*4 div 3);
  end;

  procedure LightnessExpanded(x,y: Int32; var pix: TBGRAPixel);
  begin //should be the same, but via explicit conversion between TBGRAPixel and TExpandedPixel
    pix := GammaCompression(SetLightness(GammaExpansion(pix), GetLightness(GammaExpansion(pix))*4 div 3));
  end;
  
  procedure HSLANotExpanded(x,y: Int32; var pix: TBGRAPixel);
  var hsla: THSLAPixel;
  begin
    hsla := BGRAToHSLA(pix);
	hsla.hue := hsla.hue+5000;
    pix := HSLAToBGRA(hsla);
  end;

  procedure HSLAExpanded(x,y: Int32; var pix: TBGRAPixel);
  //should be the same, but via explicit conversion between TBGRAPixel and TExpandedPixel
  var hsla: THSLAPixel;
  begin
    hsla := ExpandedToHSLA(GammaExpansion(pix));
	hsla.hue := hsla.hue+5000;
    pix := GammaCompression(HSLAToExpanded(hsla));
  end;  

  procedure GSBANotExpanded(x,y: Int32; var pix: TBGRAPixel);
  var GSBA: TGSBAPixel;
  begin
    GSBA := BGRAToGSBA(pix);
	GSBA.hue := GSBA.hue+5000;
    pix := GSBAToBGRA(GSBA);
  end;

  procedure GSBAExpanded(x,y: Int32; var pix: TBGRAPixel);
  //should be the same, but via explicit conversion between TBGRAPixel and TExpandedPixel
  var GSBA: TGSBAPixel;
  begin
    GSBA := ExpandedToGSBA(GammaExpansion(pix));
	GSBA.hue := GSBA.hue+5000;
    pix := GammaCompression(GSBAToExpanded(GSBA));
  end;  

  procedure GrayscaleNotExpanded(x,y: Int32; var pix: TBGRAPixel);
  begin
    pix := BGRAToGrayscale(pix);
  end;

  procedure GrayscaleExpanded(x,y: Int32; var pix: TBGRAPixel);
  //should be the same, but via explicit conversion between TBGRAPixel and TExpandedPixel
  begin    
    pix := GammaCompression(ExpandedToGrayscale(GammaExpansion(pix)));
  end;  
  
begin
  FillBitmap(CSSWhite);
  x := 0;

  DoStuff(x, @IntensityNotExpanded, @IntensityExpanded);
  DoStuff(x, @LightnessNotExpanded, @LightnessExpanded);
  DoStuff(x, @HSLANotExpanded, @HSLAExpanded);
  DoStuff(x, @GrayscaleNotExpanded, @GrayscaleExpanded);
  DoStuff(x, @GSBANotExpanded, @GSBAExpanded);
        
  TestDone('Intensity,Lightness,HSLA,Grayscale,GSBA');
end;

////////////////// extended geometry

function RandomPointF: TPointF;
begin
  result := PointF((random(w*10-1)-4)/10,(random(h*10-1)-4)/10);
end;

function RandomX: single;
begin
  result := (random(w*10-1)-4)/10;
end;

function RandomY: single;
begin
  result := (random(h*10-1)-4)/10;
end;

procedure TestRectF;
const n = 3; r= 10;
var i: integer;
  c,c2: TBGRAPixel;
begin
  FillBitmap(CSSWhite);
  c := BGRA(0,0,0,128);
  c2 := BGRA(0,128,0,128);
  for i := 1 to n do
    FillRectF(RandomX,RandomY,RandomX,RandomY, c2);
  for i := 1 to n do
    RectangleF(RandomX,RandomY,RandomX,RandomY, c,3);
  for i := 1 to n do
    RectangleF(RandomX,RandomY,RandomX,RandomY, c,3,c2);

  for i := 1 to n do
    FillRoundRectF(RandomX,RandomY,RandomX,RandomY,r,r, c2);
  for i := 1 to n do
    RoundRectF(RandomX,RandomY,RandomX,RandomY,r,r, c,3);
  for i := 1 to n do
    RoundRectF(RandomX,RandomY,RandomX,RandomY,r,r, c,3,c2);
	
  for i := 1 to n do
    FillEllipseF(RandomX,RandomY,RandomX/2,RandomY/2, c2);
  for i := 1 to n do
    EllipseF(RandomX,RandomY,RandomX/2,RandomY/2, c,3);
  for i := 1 to n do
    EllipseF(RandomX,RandomY,RandomX/2,RandomY/2, c,3,c2);
  
  TestDone('RectangleF/RoundRectF/EllipseF');
end;

procedure TestLineF;
const n = 3;
var i: integer;
  c,c2: TBGRAPixel;
begin
  FillBitmap(CSSWhite);
  c := BGRA(0,0,0,128);
  c2 := BGRA(0,128,0,128);

  for i := 1 to n do
    DrawLineF(RandomX,RandomY,RandomX,RandomY, c,3);

  for i := 1 to n do
    FillPolyF([RandomPointF,RandomPointF,RandomPointF], c2);
  for i := 1 to n do
    DrawPolyLineF([RandomPointF,RandomPointF,RandomPointF], c,3);
  for i := 1 to n do
    DrawPolygonF([RandomPointF,RandomPointF,RandomPointF], c,3);

  for i := 1 to n do
    DrawPolyLineF([RandomPointF,RandomPointF,RandomPointF], c,3,c2);
  for i := 1 to n do
    DrawPolygonF([RandomPointF,RandomPointF,RandomPointF], c,3,c2);
	
  TestDone('DrawLineF, PolyLineF, PolygonF');
end;

///////////////// tests

begin
  w := BitmapWidth;
  h := BitmapHeight;
  
  TestBitmap;
  TestColors;
  
  TestRectF;
  TestLineF;
  Antialiasing := false;
  TestRectF;
  TestLineF;
  DrawMode := dmSet;
  TestRectF;
  TestLineF;
  DrawMode := dmNormal;
  Antialiasing := true;
  
  TestPixelAndLine;
  TestForEachPixel;  
  TestScanline;
  TestFillRect;
  TestRect;
  TestEllipse;  
  
  TestTextOut;
  TestTextOutAngle;
  TestTextRect;  
  FillTransparent;
end;