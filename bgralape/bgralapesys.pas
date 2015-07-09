{ This file contains definitions used in Lape scripts
  written using the script language }

type
  TBGRAPixel = packed record blue,green,red,alpha: byte; end;
  PBGRAPixel = ^TBGRAPixel;
  TExpandedPixel = packed record red, green, blue, alpha: word; end;
  THSLAPixel = packed record hue, saturation, lightness, alpha: word; end;
  TGSBAPixel = THSLAPixel;
  TDrawMode = (dmSet,dmSetExceptTransparent,dmLinearBlend,dmDrawWithTransparency,dmXor);
  TForEachPixelProc = procedure(x,y: Int32; var APixel: TBGRAPixel);
  TFontStyle = (fsBold, fsItalic, fsStrikeOut, fsUnderline);
  TFontStyles = set of TFontStyle;
  TTextAlignment = (taLeft, taRight, taCenter);
  TTextLayout = (tlTop, tlCenter, tlBottom);
  TRect = record Left,Top,Right,Bottom : Int32; end;
  TPoint = record x,y: Int32; end;
  TPointF = record x,y: single; end;
  TBGRABitmap = record _Handle: Int32; end;

implementation

//synonyms
const  
  taLeftJustify = taLeft;
  taRightJustify = taRight;
  dmNormal = dmDrawWithTransparency;
  dmLinear = dmLinearBlend;
  dmFastBlend = dmLinearBlend;

const 
  CSSTransparent : TBGRAPixel = [0,0,0,0];
  CSSWhite : TBGRAPixel = [255,255,255,255];
  CSSBlack : TBGRAPixel = [0,0,0,255];
  
  //Red colors
  CSSIndianRed: TBGRAPixel = [92,92,205,255];
  CSSLightCoral: TBGRAPixel = [128,128,240,255];
  CSSSalmon: TBGRAPixel = [114,128,250,255];
  CSSDarkSalmon: TBGRAPixel = [122,150,233,255];
  CSSRed: TBGRAPixel = [0,0,255,255];
  CSSCrimson: TBGRAPixel = [60,20,220,255];
  CSSFireBrick: TBGRAPixel = [34,34,178,255];
  CSSDarkRed: TBGRAPixel = [0,0,139,255];

  //Pink colors
  CSSPink: TBGRAPixel = [203,192,255,255];
  CSSLightPink: TBGRAPixel = [193,182,255,255];
  CSSHotPink: TBGRAPixel = [180,105,255,255];
  CSSDeepPink: TBGRAPixel = [147,20,255,255];
  CSSMediumVioletRed: TBGRAPixel = [133,21,199,255];
  CSSPaleVioletRed: TBGRAPixel = [147,112,219,255];

  //Orange colors
  CSSLightSalmon: TBGRAPixel = [122,160,255,255];
  CSSCoral: TBGRAPixel = [80,127,255,255];
  CSSTomato: TBGRAPixel = [71,99,255,255];
  CSSOrangeRed: TBGRAPixel = [0,69,255,255];
  CSSDarkOrange: TBGRAPixel = [0,140,255,255];
  CSSOrange: TBGRAPixel = [0,165,255,255];

  //Yellow colors
  CSSGold: TBGRAPixel = [0,215,255,255];
  CSSYellow: TBGRAPixel = [0,255,255,255];
  CSSLightYellow: TBGRAPixel = [224,255,255,255];
  CSSLemonChiffon: TBGRAPixel = [205,250,255,255];
  CSSLightGoldenrodYellow: TBGRAPixel = [210,250,250,255];
  CSSPapayaWhip: TBGRAPixel = [213,239,255,255];
  CSSMoccasin: TBGRAPixel = [181,228,255,255];
  CSSPeachPuff: TBGRAPixel = [185,218,255,255];
  CSSPaleGoldenrod: TBGRAPixel = [170,232,238,255];
  CSSKhaki: TBGRAPixel = [140,230,240,255];
  CSSDarkKhaki: TBGRAPixel = [107,183,189,255];

  //Purple colors
  CSSLavender: TBGRAPixel = [250,230,230,255];
  CSSThistle: TBGRAPixel = [216,191,216,255];
  CSSPlum: TBGRAPixel = [221,160,221,255];
  CSSViolet: TBGRAPixel = [238,130,238,255];
  CSSOrchid: TBGRAPixel = [214,112,218,255];
  CSSFuchsia: TBGRAPixel = [255,0,255,255];
  CSSMagenta: TBGRAPixel = [255,0,255,255];
  CSSMediumOrchid: TBGRAPixel = [211,85,186,255];
  CSSMediumPurple: TBGRAPixel = [219,112,147,255];
  CSSBlueViolet: TBGRAPixel = [226,43,138,255];
  CSSDarkViolet: TBGRAPixel = [211,0,148,255];
  CSSDarkOrchid: TBGRAPixel = [204,50,153,255];
  CSSDarkMagenta: TBGRAPixel = [139,0,139,255];
  CSSPurple: TBGRAPixel = [128,0,128,255];
  CSSIndigo: TBGRAPixel = [130,0,75,255];
  CSSDarkSlateBlue: TBGRAPixel = [139,61,72,255];
  CSSSlateBlue: TBGRAPixel = [205,90,106,255];
  CSSMediumSlateBlue: TBGRAPixel = [238,104,123,255];

  //Green colors
  CSSGreenYellow: TBGRAPixel = [47,255,173,255];
  CSSChartreuse: TBGRAPixel = [0,255,127,255];
  CSSLawnGreen: TBGRAPixel = [0,252,124,255];
  CSSLime: TBGRAPixel = [0,255,0,255];
  CSSLimeGreen: TBGRAPixel = [50,205,50,255];
  CSSPaleGreen: TBGRAPixel = [152,251,152,255];
  CSSLightGreen: TBGRAPixel = [144,238,144,255];
  CSSMediumSpringGreen: TBGRAPixel = [154,250,0,255];
  CSSSpringGreen: TBGRAPixel = [127,255,0,255];
  CSSMediumSeaGreen: TBGRAPixel = [113,179,60,255];
  CSSSeaGreen: TBGRAPixel = [87,139,46,255];
  CSSForestGreen: TBGRAPixel = [34,139,34,255];
  CSSGreen: TBGRAPixel = [0,128,0,255];
  CSSDarkGreen: TBGRAPixel = [0,100,0,255];
  CSSYellowGreen: TBGRAPixel = [50,205,154,255];
  CSSOliveDrab: TBGRAPixel = [35,142,107,255];
  CSSOlive: TBGRAPixel = [0,128,128,255];
  CSSDarkOliveGreen: TBGRAPixel = [47,107,85,255];
  CSSMediumAquamarine: TBGRAPixel = [170,205,102,255];
  CSSDarkSeaGreen: TBGRAPixel = [143,188,143,255];
  CSSLightSeaGreen: TBGRAPixel = [170,178,32,255];
  CSSDarkCyan: TBGRAPixel = [139,139,0,255];
  CSSTeal: TBGRAPixel = [128,128,0,255];

  //Blue/Cyan colors
  CSSAqua: TBGRAPixel = [255,255,0,255];
  CSSCyan: TBGRAPixel = [255,255,0,255];
  CSSLightCyan: TBGRAPixel = [255,255,224,255];
  CSSPaleTurquoise: TBGRAPixel = [238,238,175,255];
  CSSAquamarine: TBGRAPixel = [212,255,127,255];
  CSSTurquoise: TBGRAPixel = [208,224,64,255];
  CSSMediumTurquoise: TBGRAPixel = [204,209,72,255];
  CSSDarkTurquoise: TBGRAPixel = [209,206,0,255];
  CSSCadetBlue: TBGRAPixel = [160,158,95,255];
  CSSSteelBlue: TBGRAPixel = [180,130,70,255];
  CSSLightSteelBlue: TBGRAPixel = [222,196,176,255];
  CSSPowderBlue: TBGRAPixel = [230,224,176,255];
  CSSLightBlue: TBGRAPixel = [230,216,173,255];
  CSSSkyBlue: TBGRAPixel = [235,206,135,255];
  CSSLightSkyBlue: TBGRAPixel = [250,206,135,255];
  CSSDeepSkyBlue: TBGRAPixel = [255,191,0,255];
  CSSDodgerBlue: TBGRAPixel = [255,144,30,255];
  CSSCornflowerBlue: TBGRAPixel = [237,149,100,255];
  CSSRoyalBlue: TBGRAPixel = [255,105,65,255];
  CSSBlue: TBGRAPixel = [255,0,0,255];
  CSSMediumBlue: TBGRAPixel = [205,0,0,255];
  CSSDarkBlue: TBGRAPixel = [139,0,0,255];
  CSSNavy: TBGRAPixel = [128,0,0,255];
  CSSMidnightBlue: TBGRAPixel = [112,25,25,255];

  //Brown colors
  CSSCornsilk: TBGRAPixel = [220,248,255,255];
  CSSBlanchedAlmond: TBGRAPixel = [205,235,255,255];
  CSSBisque: TBGRAPixel = [196,228,255,255];
  CSSNavajoWhite: TBGRAPixel = [173,222,255,255];
  CSSWheat: TBGRAPixel = [179,222,245,255];
  CSSBurlyWood: TBGRAPixel = [135,184,222,255];
  CSSTan: TBGRAPixel = [140,180,210,255];
  CSSRosyBrown: TBGRAPixel = [143,143,188,255];
  CSSSandyBrown: TBGRAPixel = [96,164,244,255];
  CSSGoldenrod: TBGRAPixel = [32,165,218,255];
  CSSDarkGoldenrod: TBGRAPixel = [11,134,184,255];
  CSSPeru: TBGRAPixel = [63,133,205,255];
  CSSChocolate: TBGRAPixel = [30,105,210,255];
  CSSSaddleBrown: TBGRAPixel = [19,69,139,255];
  CSSSienna: TBGRAPixel = [45,82,160,255];
  CSSBrown: TBGRAPixel = [42,42,165,255];
  CSSMaroon: TBGRAPixel = [0,0,128,255];

  //White colors
  CSSSnow: TBGRAPixel = [250,250,255,255];
  CSSHoneydew: TBGRAPixel = [240,255,250,255];
  CSSMintCream: TBGRAPixel = [250,255,245,255];
  CSSAzure: TBGRAPixel = [255,255,240,255];
  CSSAliceBlue: TBGRAPixel = [255,248,240,255];
  CSSGhostWhite: TBGRAPixel = [255,248,248,255];
  CSSWhiteSmoke: TBGRAPixel = [245,245,245,255];
  CSSSeashell: TBGRAPixel = [255,245,238,255];
  CSSBeige: TBGRAPixel = [220,245,245,255];
  CSSOldLace: TBGRAPixel = [230,245,253,255];
  CSSFloralWhite: TBGRAPixel = [240,250,255,255];
  CSSIvory: TBGRAPixel = [240,255,255,255];
  CSSAntiqueWhite: TBGRAPixel = [215,235,250,255];
  CSSLinen: TBGRAPixel = [230,240,250,255];
  CSSLavenderBlush: TBGRAPixel = [245,240,255,255];
  CSSMistyRose: TBGRAPixel = [255,228,255,255];

  //Gray colors
  CSSGainsboro: TBGRAPixel = [220,220,220,255];
  CSSLightGray: TBGRAPixel = [211,211,211,255];
  CSSSilver: TBGRAPixel = [192,192,192,255];
  CSSDarkGray: TBGRAPixel = [169,169,169,255];
  CSSGray: TBGRAPixel = [128,128,128,255];
  CSSDimGray: TBGRAPixel = [105,105,105,255];
  CSSLightSlateGray: TBGRAPixel = [153,136,119,255];
  CSSSlateGray: TBGRAPixel = [144,128,112,255];
  CSSDarkSlateGray: TBGRAPixel = [79,79,47,255];

var
  FontName: string = 'Arial';
  FontStyle: TFontStyles;  
  TextAlignment: TTextAlignment;
  TextLayout: TTextLayout;
  DrawMode: TDrawMode = dmDrawWithTransparency;
  Antialiasing: boolean = true;

function Odd(Value: Int32): boolean;
begin
  result := (Value and 1) <> 0;
end;
  
function Even(Value: Int32): boolean;
begin
  result := (Value and 1) = 0;
end;
  
procedure TextOut(x, y: single; sUTF8: string; c: TBGRAPixel); override;
begin
  _SetFontName(FontName);
  _SetFontStyle(fsBold in FontStyle, fsItalic in FontStyle, fsStrikeOut in FontStyle, fsUnderline in FontStyle);
  _SetTextAlignment(Int32(TextAlignment));
  _SetTextLayout(Int32(TextLayout));
  inherited(x,y,sUTF8,c);
end;  

procedure TextOutAngle(x, y, angle: single; sUTF8: string; c: TBGRAPixel); override;
begin
  _SetFontName(FontName);
  _SetFontStyle(fsBold in FontStyle, fsItalic in FontStyle, fsStrikeOut in FontStyle, fsUnderline in FontStyle);
  _SetTextAlignment(Int32(TextAlignment));
  _SetTextLayout(Int32(TextLayout));
  inherited(x,y,angle,sUTF8,c);
end;  
  
procedure TextRect(left,top,right,bottom: Int32; sUTF8: string; c: TBGRAPixel); override;
begin
  _SetFontName(FontName);
  _SetFontStyle(fsBold in FontStyle, fsItalic in FontStyle, fsStrikeOut in FontStyle, fsUnderline in FontStyle);
  _SetTextAlignment(Int32(TextAlignment));
  _SetTextLayout(Int32(TextLayout));
  inherited(left,top,right,bottom,sUTF8,c);
end;  

procedure TextRect(ARect: TRect; sUTF8: string; c: TBGRAPixel); overload;
begin
  TextRect(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, sUTF8, c);
end;
  
procedure ForEachPixel(APixelProc: TForEachPixelProc); overload;
var 
  x,y,w,h: integer;
  p: PBGRAPixel;
  bmp: TBGRABitmap;
begin
  w := BitmapWidth;
  h := BitmapHeight;
  bmp := SelectedBitmap;
  bmp._Lock;
  for y := 0 to h-1 do
  begin
    bmp.Select;
    p := GetScanLine(y);
	for x := 0 to w-1 do
	begin
	  APixelProc(x,y,p^);
	  inc(p);
	end;
  end;
  bmp._Unlock;
end;

procedure FillTransparent;
begin
  FillBitmap(CSSTransparent);
end;

function Rect(left,top,right,bottom: Int32): TRect;
begin
  result.Left := left;
  result.Top := top;
  result.Right := right;
  result.Bottom := bottom;
end;

function RectWithSize(left,top,width,height: Int32): TRect;
begin
  result.Left := left;
  result.Top := top;
  result.Right := left+width;
  result.Bottom := top+height;
end;

function Point(x,y: Int32): TPoint;
begin
  result.x := x;
  result.y := y;
end;

procedure SetClipRect(left,top,right,bottom: Int32); overload;
begin
  SetClipRect(rect(left,top,right,bottom));
end;

procedure DrawPixel(x,y : Int32; c: TBGRAPixel); overload;
begin
  _DrawPixel(x,y,c,Int32(DrawMode));
end;

procedure DrawLine(x1,y1,x2,y2: Int32; c: TBGRAPixel); overload;
begin
  _DrawLine(x1,y1,x2,y2, c, Int32(DrawMode), Antialiasing);
end;  

procedure DrawPolyLine(const points: array of TPoint; c: TBGRAPixel); overload;
begin
  _DrawPolyLine(points, c, Int32(DrawMode), Antialiasing);
end;  

procedure DrawPolygon(const points: array of TPoint; c: TBGRAPixel); overload;
begin
  _DrawPolygon(points, c, Int32(DrawMode), Antialiasing);
end;  

procedure EraseLine(x1,y1,x2,y2: Int32; alpha: byte); overload;
begin
  _EraseLine(x1,y1,x2,y2, alpha, Antialiasing);
end;  

procedure ErasePolyLine(const points: array of TPoint; alpha: byte); overload;
begin
  _ErasePolyLine(points, alpha, Antialiasing);
end;  

procedure ErasePolygonOutline(const points: array of TPoint; alpha: byte); overload;
begin
  _ErasePolygonOutline(points, alpha, Antialiasing);
end;  



procedure FillRect(left,top,right,bottom: Int32; c: TBGRAPixel); overload;
begin
  _FillRect(left,top,right,bottom, c, Int32(DrawMode));
end;

procedure FillRect(ARect: TRect; c: TBGRAPixel); overload;
begin
  _FillRect(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, c, Int32(DrawMode));
end;

procedure Rectangle(left,top,right,bottom: Int32; c: TBGRAPixel); overload;
begin
  _Rectangle(left,top,right,bottom, c, Int32(DrawMode));
end;

procedure Rectangle(ARect: TRect; c: TBGRAPixel); overload;
begin
  _Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, c, Int32(DrawMode));
end;

procedure Rectangle(left,top,right,bottom: Int32; c,fillcolor: TBGRAPixel); overload;
begin
  _RectangleWithFill(left,top,right,bottom, c,fillcolor, Int32(DrawMode));
end;

procedure Rectangle(ARect: TRect; c,fillcolor: TBGRAPixel); overload;
begin
  _RectangleWithFill(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, c,fillcolor, Int32(DrawMode));
end;


procedure FillRoundRect(left,top,right,bottom: Int32; rx,ry: single; c: TBGRAPixel); overload;
begin
  _FillRoundRect(left,top,right,bottom,rx,ry, c, Int32(DrawMode), Antialiasing);
end;

procedure FillRoundRect(ARect: TRect; rx,ry: single; c: TBGRAPixel); overload;
begin
  _FillRoundRect(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom,rx,ry, c, Int32(DrawMode), Antialiasing);
end;

procedure RoundRect(left,top,right,bottom: Int32; rx,ry: single; c: TBGRAPixel); overload;
begin
  _RoundRect(left,top,right,bottom, rx,ry,c, Int32(DrawMode), Antialiasing);
end;

procedure RoundRect(ARect: TRect; rx,ry: single; c: TBGRAPixel); overload;
begin
  _RoundRect(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, rx,ry,c, Int32(DrawMode), Antialiasing);
end;

procedure RoundRect(left,top,right,bottom: Int32; rx,ry: single; c,fillcolor: TBGRAPixel); overload;
begin
  _RoundRectWithFill(left,top,right,bottom, rx,ry,c,fillcolor, Int32(DrawMode), Antialiasing);
end;

procedure RoundRect(ARect: TRect; rx,ry: single; c,fillcolor: TBGRAPixel); overload;
begin
  _RoundRectWithFill(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, rx,ry,c,fillcolor, Int32(DrawMode), Antialiasing);
end;



procedure FillEllipse(x,y: integer; rx,ry: single; c: TBGRAPixel);
begin
  _FillEllipseInRect(round(x-rx+0.5),round(y-ry+0.5),round(x+rx+0.5),round(y+ry+0.5),c, Int32(DrawMode), Antialiasing);
end;

procedure Ellipse(x,y: integer; rx,ry: single; c: TBGRAPixel);
begin
  _EllipseInRect(round(x-rx+0.5),round(y-ry+0.5),round(x+rx+0.5),round(y+ry+0.5),c, Int32(DrawMode), Antialiasing);
end;

procedure Ellipse(x,y: integer; rx,ry: single; c,fillcolor: TBGRAPixel); overload;
begin
  _EllipseInRectWithFill(round(x-rx+0.5),round(y-ry+0.5),round(x+rx+0.5),round(y+ry+0.5),c,fillcolor, Int32(DrawMode), Antialiasing);
end;


procedure PutImage(x,y: integer; bmp: TBGRABitmap; alpha: byte = 255); overload;
begin
  _PutImage(x,y, bmp, Int32(DrawMode), alpha);
end;

function BGRAToGrayscale(c: TBGRAPixel): TBGRAPixel;
begin
  result := GrayscaleToBGRA(GetLightness(c));
  result.alpha := c.alpha;
end;

function ExpandedToGrayscale(ec: TExpandedPixel): TExpandedPixel;
begin
  result := GrayscaleToExpanded(GetLightness(ec));
  result.alpha := ec.alpha;
end;


function PointF(x, y: single): TPointF;
begin
  Result.x := x;
  Result.y := y;
end;

function VectEq(const pt1, pt2: TPointF): boolean;
begin
  result := (pt1.x = pt2.x) and (pt1.y = pt2.y);
end;

function VectSub(const pt1, pt2: TPointF): TPointF;
begin
  result.x := pt1.x-pt2.x;
  result.y := pt1.y-pt2.y;
end;

function VectNeg(const pt2: TPointF): TPointF;
begin
  result.x := -pt2.x;
  result.y := -pt2.y;
end;

function VectAdd(const pt1, pt2: TPointF): TPointF;
begin
  result.x := pt1.x+pt2.x;
  result.y := pt1.y+pt2.y;
end;

function VectDot(const pt1, pt2: TPointF): single;
begin
  result := pt1.x*pt2.x + pt1.y*pt2.y;
end;

function VectScale(const pt1: TPointF; factor: single): TPointF;
begin
  result.x := pt1.x*factor;
  result.y := pt1.y*factor;
end;

function VectScale(factor: single; const pt1: TPointF): TPointF; overload;
begin
  result.x := pt1.x*factor;
  result.y := pt1.y*factor;
end;

function VectLen(dx, dy: single): single;
begin
  result := sqrt(dx*dx+dy*dy);
end;

function VectLen(v: TPointF): single; overload;
begin
  result := sqrt(v.x*v.x+v.y*v.y);
end;

procedure FillRectF(left, top, right, bottom: single; c: TBGRAPixel);
begin
  _FillRectF(left,top,right,bottom,c,Int32(DrawMode),Antialiasing);
end;

procedure RectangleF(left, top, right, bottom: single; c: TBGRAPixel; w: single);
begin
  _RectangleF(left,top,right,bottom,c,w,Int32(DrawMode),Antialiasing);
end;
 
procedure RectangleF(left, top, right, bottom: single; c: TBGRAPixel; w: single; fillcolor: TBGRAPixel); overload;
begin
  _RectangleF(left,top,right,bottom,c,w,fillcolor,Int32(DrawMode),Antialiasing);
end;

procedure FillRoundRectF(left, top, right, bottom,rx,ry: single; c: TBGRAPixel);
begin
  _FillRoundRectF(left,top,right,bottom,rx,ry,c,Int32(DrawMode),Antialiasing);
end;

procedure RoundRectF(left, top, right, bottom,rx,ry: single; c: TBGRAPixel; w: single);
begin
  _RoundRectF(left,top,right,bottom,rx,ry,c,w,Int32(DrawMode),Antialiasing);
end;
 
procedure RoundRectF(left, top, right, bottom,rx,ry: single; c: TBGRAPixel; w: single; fillcolor: TBGRAPixel); overload;
begin
  _RoundRectF(left,top,right,bottom,rx,ry,c,w,fillcolor,Int32(DrawMode),Antialiasing);
end;

procedure FillEllipseF(x,y,rx,ry: single; c: TBGRAPixel);
begin
  _FillEllipseF(x,y,rx,ry,c,Int32(DrawMode),Antialiasing);
end;

procedure EllipseF(x,y,rx,ry: single; c: TBGRAPixel; w: single);
begin
  _EllipseF(x,y,rx,ry,c,w,Int32(DrawMode),Antialiasing);
end;
 
procedure EllipseF(x,y,rx,ry: single; c: TBGRAPixel; w: single; fillcolor: TBGRAPixel); overload;
begin
  _EllipseF(x,y,rx,ry,c,w,fillcolor,Int32(DrawMode),Antialiasing);
end;

procedure DrawLineF(x1,y1,x2,y2: single; c: TBGRAPixel; w: single);
begin
  _DrawLineF(x1,y1,x2,y2,c,w,Int32(DrawMode),Antialiasing);
end;
 
procedure DrawPolyLineF(const pts: array of TPointF; c: TBGRAPixel; w: single);
begin
  _DrawPolyLineF(pts,c,w,Int32(DrawMode),Antialiasing);
end;
 
procedure DrawPolygonF(const pts: array of TPointF; c: TBGRAPixel; w: single);
begin
  _DrawPolygonF(pts,c,w,Int32(DrawMode),Antialiasing);
end;

procedure DrawPolyLineF(const pts: array of TPointF; c: TBGRAPixel; w: single; fillcolor: TBGRAPixel); overload;
begin
  _DrawPolyLineF(pts,c,w,fillcolor,Int32(DrawMode),Antialiasing);
end;
 
procedure DrawPolygonF(const pts: array of TPointF; c: TBGRAPixel; w: single; fillcolor: TBGRAPixel); overload;
begin
  _DrawPolygonF(pts,c,w,fillcolor,Int32(DrawMode),Antialiasing);
end;
 
procedure FillPolyF(const pts: array of TPointF; c: TBGRAPixel);
begin
  _FillPolyF(pts,c,Int32(DrawMode),Antialiasing);
end;
