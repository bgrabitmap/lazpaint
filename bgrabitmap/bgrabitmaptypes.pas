{
 /**************************************************************************\
                                bgrabitmaptypes.pas
                                -------------------
                   This unit defines basic types and it must be
                   included in the 'uses' clause.

       --> Include BGRABitmap and BGRABitmapTypes in the 'uses' clause.

 ****************************************************************************
 *                                                                          *
 *  This file is part of BGRABitmap library which is distributed under the  *
 *  modified LGPL.                                                          *
 *                                                                          *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,   *
 *  for details about the copyright.                                        *
 *                                                                          *
 *  This program is distributed in the hope that it will be useful,         *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of          *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    *
 *                                                                          *
 ****************************************************************************
}

{
 to do :
   - Canvas2D to do
   - ZenGL emulation
   - 3D emulation
   - box blur
   - power integer using shr and multiplication for phong


}

unit BGRABitmapTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, Types, Graphics, FPImage, FPImgCanv, GraphType;

type
  //pointer for direct pixel access
  PBGRAPixel = ^TBGRAPixel;

  Int32or64 = {$IFDEF CPU64}Int64{$ELSE}LongInt{$ENDIF};
  UInt32or64 = {$IFDEF CPU64}UInt64{$ELSE}LongWord{$ENDIF};

  //pixel structure
  TBGRAPixel = packed record
    blue, green, red, alpha: byte;
  end;

  //gamma expanded values
  TExpandedPixel = packed record
    red, green, blue, alpha: word;
  end;

  //pixel color defined in HSL colorspace
  THSLAPixel = packed record
    hue, saturation, lightness, alpha: word;
  end;

  //general purpose color variable with floating point values
  TColorF = packed array[1..4] of single;
  
  { These types are used as parameters }

  TDrawMode = (dmSet,                   //replace pixels
               dmSetExceptTransparent,  //draw pixels with alpha=255
               dmLinearBlend,           //blend without gamma correction
               dmDrawWithTransparency,  //normal blending with gamma correction
               dmXor);                  //bitwise xor for all channels
  TChannel = (cRed, cGreen, cBlue, cAlpha);
  TChannels = set of TChannel;
               
  //floodfill option
  TFloodfillMode = (fmSet,                   //set pixels
                    fmDrawWithTransparency,  //draw fill color with transparency
                    fmProgressive);          //draw fill color with transparency according to similarity with start color

  TResampleMode = (rmSimpleStretch,   //low quality resample
                   rmFineResample);   //use resample filters and pixel-centered coordinates
  TResampleFilter = (rfBox,           //equivalent of stretch with high quality
                     rfLinear,        //linear interpolation
                     rfHalfCosine,    //mix of rfLinear and rfCosine
                     rfCosine,        //cosine-like interpolation
                     rfBicubic,       //simple bi-cubic filter (blur)
                     rfMitchell,      //downsizing interpolation
                     rfSpline,        //upsizing interpolation
                     rfLanczos2,      //Lanczos with radius 2
                     rfLanczos3,      //Lanczos with radius 3
                     rfLanczos4,      //Lanczos with radius 4
                     rfBestQuality);  //mix of rfMitchell and rfSpline

const
  ResampleFilterStr : array[TResampleFilter] of string =
   ('Box','Linear','HalfCosine','Cosine','Bicubic','Mitchell','Spline',
    'Lanczos2','Lanczos3','Lanczos4','BestQuality');

function StrToResampleFilter(str: string): TResampleFilter;

type
  TBGRAImageFormat = (ifUnknown, ifJpeg, ifPng, ifGif, ifBmp, ifIco, ifPcx, ifPaintDotNet, ifLazPaint, ifOpenRaster,
    ifPsd, ifTarga, ifTiff, ifXwd, ifXPixMap, ifBmpMioMap);

var
  DefaultBGRAImageReader: array[TBGRAImageFormat] of TFPCustomImageReaderClass;
  DefaultBGRAImageWriter: array[TBGRAImageFormat] of TFPCustomImageWriterClass;

type
  TBGRAFontQuality = (fqSystem, fqSystemClearType, fqFineAntialiasing, fqFineClearTypeRGB, fqFineClearTypeBGR);

  TMedianOption = (moNone, moLowSmooth, moMediumSmooth, moHighSmooth);
  TRadialBlurType = (rbNormal, rbDisk, rbCorona, rbPrecise, rbFast);
  TSplineStyle = (ssInside, ssInsideWithEnds, ssCrossing, ssCrossingWithEnds,
    ssOutside, ssRoundOutside, ssVertexToSide);
  
  //Advanced blending modes
  //see : http://www.brighthub.com/multimedia/photography/articles/18301.aspx
  //and : http://www.pegtop.net/delphi/articles/blendmodes/  
  TBlendOperation = (boLinearBlend, boTransparent,                                  //blending
    boLighten, boScreen, boAdditive, boLinearAdd, boColorDodge, boDivide, boNiceGlow, boSoftLight, boHardLight, //lighting
    boGlow, boReflect, boOverlay, boDarkOverlay, boDarken, boMultiply, boColorBurn, //masking
    boDifference, boLinearDifference, boExclusion, boLinearExclusion, boSubtract, boLinearSubtract, boSubtractInverse, boLinearSubtractInverse,
    boNegation, boLinearNegation, boXor);         //negative

const
  boGlowMask = boGlow;
  boLinearMultiply = boMultiply;
  boNonLinearOverlay = boDarkOverlay;
  EmptyRect : TRect = (left:0; top:0; right:0; bottom: 0);

const
  BlendOperationStr : array[TBlendOperation] of string =
   ('LinearBlend', 'Transparent',
    'Lighten', 'Screen', 'Additive', 'LinearAdd', 'ColorDodge', 'Divide', 'NiceGlow', 'SoftLight', 'HardLight',
    'Glow', 'Reflect', 'Overlay', 'DarkOverlay', 'Darken', 'Multiply', 'ColorBurn',
    'Difference', 'LinearDifference', 'Exclusion', 'LinearExclusion', 'Subtract', 'LinearSubtract', 'SubtractInverse', 'LinearSubtractInverse',
    'Negation', 'LinearNegation', 'Xor');

function StrToBlendOperation(str: string): TBlendOperation;

type
  TGradientType = (gtLinear, gtReflected, gtDiamond, gtRadial);
const
  GradientTypeStr : array[TGradientType] of string =
  ('Linear','Reflected','Diamond','Radial');
function StrToGradientType(str: string): TGradientType;
 
type
  { A pen style is defined as a list of floating number. The first number is the length of the first dash,
    the second number is the length of the first gap, the third number is the length of the second dash... 
    It must have an even number of values. }
  TBGRAPenStyle = Array Of Single;
  TRoundRectangleOption = (rrTopLeftSquare,rrTopRightSquare,rrBottomRightSquare,rrBottomLeftSquare,
                           rrTopLeftBevel,rrTopRightBevel,rrBottomRightBevel,rrBottomLeftBevel,rrDefault);
  TRoundRectangleOptions = set of TRoundRectangleOption;
  TPolygonOrder = (poNone, poFirstOnTop, poLastOnTop); //see TBGRAMultiShapeFiller in BGRAPolygon
  
function BGRAPenStyle(dash1, space1: single; dash2: single=0; space2: single = 0; dash3: single=0; space3: single = 0; dash4 : single = 0; space4 : single = 0): TBGRAPenStyle;  
  
{ Point, polygon and curve structures }
type
  PPointF = ^TPointF;
  TPointF = packed record
    x, y: single;
  end;
  ArrayOfTPointF = array of TPointF;
  TArcOption = (aoClosePath, aoPie, aoFillPath);
  TArcOptions = set of TArcOption;

  TCubicBezierCurve = record
    p1,c1,c2,p2: TPointF;
  end;
  TQuadraticBezierCurve = record
    p1,c,p2: TPointF;
  end;

  TArcDef = record
    center: TPointF;
    radius: TPointF;
    xAngleRadCW, startAngleRadCW, endAngleRadCW: single; //see convention in BGRAPath
    anticlockwise: boolean
  end;
  PArcDef = ^TArcDef;

  TPoint3D = record
    x,y,z: single;
  end;

  TBGRATypeWriterAlignment = (twaTopLeft, twaTop, twaTopRight,
                              twaLeft, twaMiddle, twaRight,
                              twaBottomLeft, twaBottom, twaBottomRight);
  TBGRATypeWriterOutlineMode = (twoPath, twoFill, twoStroke, twoFillOverStroke, twoStrokeOverFill, twoFillThenStroke, twoStrokeThenFill);

function ConcatPointsF(const APolylines: array of ArrayOfTPointF): ArrayOfTPointF;

function Point3D(x,y,z: single): TPoint3D;
operator = (const v1,v2: TPoint3D): boolean; inline;
operator * (const v1,v2: TPoint3D): single; inline;
operator * (const v1: TPoint3D; const factor: single): TPoint3D; inline;
operator - (const v1,v2: TPoint3D): TPoint3D; inline;
operator - (const v: TPoint3D): TPoint3D; inline;
operator + (const v1,v2: TPoint3D): TPoint3D; inline;
procedure VectProduct3D(u,v: TPoint3D; out w: TPoint3D);
procedure Normalize3D(var v: TPoint3D); inline;

function BezierCurve(origin, control1, control2, destination: TPointF) : TCubicBezierCurve; overload;
function BezierCurve(origin, control, destination: TPointF) : TQuadraticBezierCurve; overload;
function BezierCurve(origin, destination: TPointF) : TQuadraticBezierCurve; overload;
function ArcDef(cx, cy, rx,ry, xAngleRadCW, startAngleRadCW, endAngleRadCW: single; anticlockwise: boolean) : TArcDef;

{ Useful constants }
const
  dmFastBlend = dmLinearBlend;
  EmptySingle: single = -3.402823e38;                        //used as a separator in floating point lists
  EmptyPointF: TPointF = (x: -3.402823e38; y: -3.402823e38); //used as a separator in TPointF lists
  BGRAPixelTransparent: TBGRAPixel = (blue: 0; green: 0; red: 0; alpha: 0);
  BGRAWhite: TBGRAPixel = (blue: 255; green: 255; red: 255; alpha: 255);
  BGRABlack: TBGRAPixel = (blue: 0; green: 0; red: 0; alpha: 255);

  { This color is needed for drawing black shapes on the standard TCanvas, because
    when drawing with pure black, there is no way to know if something has been
    drawn or if it is transparent }
  clBlackOpaque = TColor($010000);

{$DEFINE INCLUDE_COLOR_CONST}
{$i csscolorconst.inc}

type
  TBGRAColorDefinition = record
    Name: string;
    Color: TBGRAPixel;
  end;

  { TBGRAColorList }

  TBGRAColorList = class
  protected
    FFinished: boolean;
    FNbColors: integer;
    FColors: array of TBGRAColorDefinition;
    function GetByIndex(Index: integer): TBGRAPixel;
    function GetByName(Name: string): TBGRAPixel;
    function GetName(Index: integer): string;
  public
    constructor Create;
    procedure Add(Name: string; const Color: TBGRAPixel);
    procedure Finished;
    function IndexOf(Name: string): integer;
    function IndexOfColor(const AColor: TBGRAPixel; AMaxDiff: Word = 0): integer;

    property ByName[Name: string]: TBGRAPixel read GetByName;
    property ByIndex[Index: integer]: TBGRAPixel read GetByIndex; default;
    property Name[Index: integer]: string read GetName;
    property Count: integer read FNbColors;
  end;

var
  VGAColors, CSSColors: TBGRAColorList;

function isEmptyPointF(pt: TPointF): boolean;

type
  TFontPixelMetric = record
    Defined: boolean;
    Baseline, xLine, CapLine, DescentLine, Lineheight: integer;
  end;

  { A scanner is like an image, but its content has no limit and can be calculated on the fly.
    It must not implement reference counting. }
  IBGRAScanner = interface
    procedure ScanMoveTo(X,Y: Integer);
    function ScanNextPixel: TBGRAPixel;
    function ScanAt(X,Y: Single): TBGRAPixel;
    function ScanAtInteger(X,Y: integer): TBGRAPixel;
    procedure ScanPutPixels(pdest: PBGRAPixel; count: integer; mode: TDrawMode);
    function IsScanPutPixelsDefined: boolean;
  end;

  { A path is the ability to define a contour with moveTo, lineTo...
    It must not implement reference counting. }
  IBGRAPath = interface
    procedure closePath;
    procedure moveTo(const pt: TPointF);
    procedure lineTo(const pt: TPointF);
    procedure polylineTo(const pts: array of TPointF);
    procedure quadraticCurveTo(const cp,pt: TPointF);
    procedure bezierCurveTo(const cp1,cp2,pt: TPointF);
    procedure arc(const arcDef: TArcDef);
    procedure copyTo(dest: IBGRAPath);
  end;

  TScanAtFunction = function (X,Y: Single): TBGRAPixel of object;
  TScanAtIntegerFunction = function (X,Y: Integer): TBGRAPixel of object;
  TScanNextPixelFunction = function: TBGRAPixel of object;
  TBGRACustomGradient = class;

  TBGRACustomFillInfo = class;
  TBGRACustomFontRenderer = class;

  { TBGRACustomBitmap }

  TBGRACustomBitmap = class(TFPCustomImage,IBGRAScanner) // a bitmap can be used as a scanner
  private
    function GetFontAntialias: Boolean;
    procedure SetFontAntialias(const AValue: Boolean);
  protected
     { accessors to properies }
     function GetFontRenderer: TBGRACustomFontRenderer; virtual; abstract;
     procedure SetFontRenderer(AValue: TBGRACustomFontRenderer); virtual; abstract;
     function GetHeight: integer; virtual; abstract;
     function GetWidth: integer; virtual; abstract;
     function GetDataPtr: PBGRAPixel; virtual; abstract;
     function GetNbPixels: integer; virtual; abstract;
     function CheckEmpty: boolean; virtual; abstract;
     function GetHasTransparentPixels: boolean; virtual; abstract;
     function GetAverageColor: TColor; virtual; abstract;
     function GetAveragePixel: TBGRAPixel; virtual; abstract;
     procedure SetCanvasOpacity(AValue: byte); virtual; abstract;
     function GetScanLine(y: integer): PBGRAPixel; virtual; abstract;
     function GetRefCount: integer; virtual; abstract;
     function GetBitmap: TBitmap; virtual; abstract;
     function GetLineOrder: TRawImageLineOrder; virtual; abstract;
     function GetCanvasFP: TFPImageCanvas; virtual; abstract;
     function GetCanvasDrawModeFP: TDrawMode; virtual; abstract;
     procedure SetCanvasDrawModeFP(const AValue: TDrawMode); virtual; abstract;
     function GetCanvas: TCanvas; virtual; abstract;
     function GetCanvasOpacity: byte; virtual; abstract;
     function GetCanvasAlphaCorrection: boolean; virtual; abstract;
     procedure SetCanvasAlphaCorrection(const AValue: boolean); virtual; abstract;
     function GetFontHeight: integer; virtual; abstract;
     procedure SetFontHeight(AHeight: integer); virtual; abstract;
     function GetFontFullHeight: integer; virtual; abstract;
     procedure SetFontFullHeight(AHeight: integer); virtual; abstract;
     function GetPenStyle: TPenStyle; virtual; abstract;
     procedure SetPenStyle(const AValue: TPenStyle); virtual; abstract;
     function GetCustomPenStyle: TBGRAPenStyle; virtual; abstract;
     procedure SetCustomPenStyle(const AValue: TBGRAPenStyle); virtual; abstract;
     function GetClipRect: TRect; virtual; abstract;
     procedure SetClipRect(const AValue: TRect); virtual; abstract;
     function GetFontPixelMetric: TFontPixelMetric; virtual; abstract;
     procedure ClearTransparentPixels; virtual; abstract;

  public
     Caption:   string;  //user defined caption

     //font style
     FontName: string;
     FontStyle: TFontStyles;
     FontQuality : TBGRAFontQuality;
     FontOrientation: integer;

     //line style
     LineCap:   TPenEndCap;
     JoinStyle: TPenJoinStyle;
     JoinMiterLimit: single;

     FillMode:  TFillMode;  //winding or alternate
     LinearAntialiasing: boolean;

     { The resample filter is used when resizing the bitmap, and
       scan interpolation filter is used when the bitmap is used
       as a scanner (IBGRAScanner) }
     ResampleFilter,
     ScanInterpolationFilter: TResampleFilter;
     ScanOffset: TPoint;

     constructor Create; virtual; abstract; overload;
     constructor Create(ABitmap: TBitmap); virtual; abstract; overload;
     constructor Create(AWidth, AHeight: integer; Color: TColor); virtual; abstract; overload;
     constructor Create(AWidth, AHeight: integer; Color: TBGRAPixel); virtual; abstract; overload;
     constructor Create(AFilename: string); virtual; abstract; overload;
     constructor Create(AFilename: string; AIsUtf8Filename: boolean); virtual; abstract; overload;
     constructor Create(AStream: TStream); virtual; abstract; overload;

     function NewBitmap(AWidth, AHeight: integer): TBGRACustomBitmap; virtual; abstract; overload;
     function NewBitmap(AWidth, AHeight: integer; Color: TBGRAPixel): TBGRACustomBitmap; virtual; abstract; overload;
     function NewBitmap(Filename: string): TBGRACustomBitmap; virtual; abstract; overload;

     //there are UTF8 functions that are different from standard function as those
     //depend on TFPCustomImage that does not clearly handle UTF8
     procedure LoadFromFile(const filename: string); virtual;
     procedure LoadFromFileUTF8(const filenameUTF8: string); virtual;
     procedure LoadFromFileUTF8(const filenameUTF8: string; AHandler: TFPCustomImageReader); virtual;
     procedure LoadFromStream(Str: TStream); virtual; overload;
     procedure LoadFromStream(Str: TStream; Handler: TFPCustomImageReader); virtual; overload;
     procedure SaveToFile(const filename: string); virtual; overload;
     procedure SaveToFileUTF8(const filenameUTF8: string); virtual; overload;
     procedure SaveToFile(const filename: string; Handler:TFPCustomImageWriter); virtual; overload;
     procedure SaveToFileUTF8(const filenameUTF8: string; Handler:TFPCustomImageWriter); virtual; overload;
     procedure SaveToStreamAsPng(Str: TStream); virtual; abstract;
     procedure Assign(ARaster: TRasterImage); virtual; abstract; overload;
     procedure Assign(MemBitmap: TBGRACustomBitmap); virtual; abstract; overload;
     procedure Serialize(AStream: TStream); virtual; abstract;
     procedure Deserialize(AStream: TStream); virtual; abstract;

     {Pixel functions}
     procedure SetPixel(x, y: int32or64; c: TColor); virtual; abstract; overload;
     procedure XorPixel(x, y: int32or64; c: TBGRAPixel); virtual; abstract; overload;
     procedure SetPixel(x, y: int32or64; c: TBGRAPixel); virtual; abstract; overload;
     procedure DrawPixel(x, y: int32or64; c: TBGRAPixel); virtual; abstract; overload;
     procedure DrawPixel(x, y: int32or64; ec: TExpandedPixel); virtual; abstract; overload;
     procedure FastBlendPixel(x, y: int32or64; c: TBGRAPixel); virtual; abstract;
     procedure ErasePixel(x, y: int32or64; alpha: byte); virtual; abstract;
     procedure AlphaPixel(x, y: int32or64; alpha: byte); virtual; abstract;
     function GetPixel(x, y: int32or64): TBGRAPixel; virtual; abstract; overload;
     function GetPixel256(x, y, fracX256,fracY256: int32or64; AResampleFilter: TResampleFilter = rfLinear; smoothBorder: boolean = true): TBGRAPixel; virtual; abstract;
     function GetPixel(x, y: single; AResampleFilter: TResampleFilter = rfLinear; smoothBorder: boolean = true): TBGRAPixel; virtual; abstract; overload;
     function GetPixelCycle(x, y: int32or64): TBGRAPixel; virtual; overload;
     function GetPixelCycle(x, y: single; AResampleFilter: TResampleFilter = rfLinear): TBGRAPixel; virtual; abstract; overload;
     function GetPixelCycle(x, y: single; AResampleFilter: TResampleFilter; repeatX: boolean; repeatY: boolean): TBGRAPixel; virtual; abstract; overload;
     function GetPixelCycle256(x, y, fracX256,fracY256: int32or64; AResampleFilter: TResampleFilter = rfLinear): TBGRAPixel; virtual; abstract; overload;
     function GetPixelCycle256(x, y, fracX256,fracY256: int32or64; AResampleFilter: TResampleFilter; repeatX: boolean; repeatY: boolean): TBGRAPixel; virtual; abstract; overload;

     {Line primitives}
     procedure SetHorizLine(x, y, x2: int32or64; c: TBGRAPixel); virtual; abstract;
     procedure XorHorizLine(x, y, x2: int32or64; c: TBGRAPixel); virtual; abstract;
     procedure DrawHorizLine(x, y, x2: int32or64; c: TBGRAPixel); virtual; abstract; overload;
     procedure DrawHorizLine(x, y, x2: int32or64; ec: TExpandedPixel); virtual; abstract; overload;
     procedure DrawHorizLine(x, y, x2: int32or64; texture: IBGRAScanner); virtual; abstract; overload;
     procedure FastBlendHorizLine(x, y, x2: int32or64; c: TBGRAPixel); virtual; abstract;
     procedure AlphaHorizLine(x, y, x2: int32or64; alpha: byte); virtual; abstract;
     procedure SetVertLine(x, y, y2: int32or64; c: TBGRAPixel); virtual; abstract;
     procedure XorVertLine(x, y, y2: int32or64; c: TBGRAPixel); virtual; abstract;
     procedure DrawVertLine(x, y, y2: int32or64; c: TBGRAPixel); virtual; abstract;
     procedure AlphaVertLine(x, y, y2: int32or64; alpha: byte); virtual; abstract;
     procedure FastBlendVertLine(x, y, y2: int32or64; c: TBGRAPixel); virtual; abstract;
     procedure DrawHorizLineDiff(x, y, x2: int32or64; c, compare: TBGRAPixel; maxDiff: byte); virtual; abstract;

     {Shapes}
     procedure DrawPath(APath: IBGRAPath; c: TBGRAPixel; w: single); virtual; abstract;
     procedure DrawPath(APath: IBGRAPath; texture: IBGRAScanner; w: single); virtual; abstract;

     procedure DrawLine(x1, y1, x2, y2: integer; c: TBGRAPixel; DrawLastPixel: boolean); virtual; abstract;
     procedure DrawLineAntialias(x1, y1, x2, y2: integer; c: TBGRAPixel; DrawLastPixel: boolean); virtual; abstract; overload;
     procedure DrawLineAntialias(x1, y1, x2, y2: integer; c1, c2: TBGRAPixel; dashLen: integer; DrawLastPixel: boolean); virtual; abstract; overload;
     procedure DrawLineAntialias(x1, y1, x2, y2: integer; c1, c2: TBGRAPixel; dashLen: integer; DrawLastPixel: boolean; var DashPos: integer); virtual; abstract; overload;
     procedure DrawLineAntialias(x1, y1, x2, y2: single; c: TBGRAPixel; w: single); virtual; abstract; overload;
     procedure DrawLineAntialias(x1, y1, x2, y2: single; texture: IBGRAScanner; w: single); virtual; abstract; overload;
     procedure DrawLineAntialias(x1, y1, x2, y2: single; c: TBGRAPixel; w: single; Closed: boolean); virtual; abstract; overload;
     procedure DrawLineAntialias(x1, y1, x2, y2: single; texture: IBGRAScanner; w: single; Closed: boolean); virtual; abstract; overload;

     procedure DrawPolyLineAntialias(const points: array of TPoint; c: TBGRAPixel; DrawLastPixel: boolean); virtual; overload;
     procedure DrawPolyLineAntialias(const points: array of TPoint; c1, c2: TBGRAPixel; dashLen: integer; DrawLastPixel: boolean); virtual; overload;
     procedure DrawPolyLineAntialias(const points: array of TPointF; c: TBGRAPixel; w: single); virtual; abstract; overload;
     procedure DrawPolyLineAntialias(const points: array of TPointF; texture: IBGRAScanner; w: single); virtual; abstract; overload;
     procedure DrawPolyLineAntialias(const points: array of TPointF; c: TBGRAPixel; w: single; Closed: boolean); virtual; abstract; overload;
     procedure DrawPolygonAntialias(const points: array of TPointF; c: TBGRAPixel; w: single); virtual; abstract; overload;
     procedure DrawPolygonAntialias(const points: array of TPointF; texture: IBGRAScanner; w: single); virtual; abstract; overload;

     procedure EraseLine(x1, y1, x2, y2: integer; alpha: byte; DrawLastPixel: boolean); virtual; abstract;
     procedure EraseLineAntialias(x1, y1, x2, y2: integer; alpha: byte; DrawLastPixel: boolean); virtual; abstract; overload;
     procedure EraseLineAntialias(x1, y1, x2, y2: single; alpha: byte; w: single); virtual; abstract; overload;
     procedure EraseLineAntialias(x1, y1, x2, y2: single; alpha: byte; w: single; Closed: boolean); virtual; abstract; overload;
     procedure ErasePolyLineAntialias(const points: array of TPointF; alpha: byte; w: single); virtual; abstract; overload;

     procedure FillPath(APath: IBGRAPath; c: TBGRAPixel); virtual; abstract;
     procedure FillPath(APath: IBGRAPath; texture: IBGRAScanner); virtual; abstract;

     procedure FillTriangleLinearColor(pt1,pt2,pt3: TPointF; c1,c2,c3: TBGRAPixel); virtual; abstract; overload;
     procedure FillTriangleLinearColorAntialias(pt1,pt2,pt3: TPointF; c1,c2,c3: TBGRAPixel); virtual; abstract; overload;
     procedure FillTriangleLinearMapping(pt1,pt2,pt3: TPointF; texture: IBGRAScanner; tex1, tex2, tex3: TPointF; TextureInterpolation: Boolean= True); virtual; abstract; overload;
     procedure FillTriangleLinearMappingLightness(pt1,pt2,pt3: TPointF; texture: IBGRAScanner; tex1, tex2, tex3: TPointF; light1,light2,light3: word; TextureInterpolation: Boolean= True); virtual; abstract; overload;
     procedure FillTriangleLinearMappingAntialias(pt1,pt2,pt3: TPointF; texture: IBGRAScanner; tex1, tex2, tex3: TPointF); virtual; abstract; overload;

     procedure FillQuadLinearColor(pt1,pt2,pt3,pt4: TPointF; c1,c2,c3,c4: TBGRAPixel); virtual; abstract; overload;
     procedure FillQuadLinearColorAntialias(pt1,pt2,pt3,pt4: TPointF; c1,c2,c3,c4: TBGRAPixel); virtual; abstract; overload;
     procedure FillQuadLinearMapping(pt1,pt2,pt3,pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF; TextureInterpolation: Boolean= True);  virtual; abstract; overload;
     procedure FillQuadLinearMappingLightness(pt1,pt2,pt3,pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF; light1,light2,light3,light4: word; TextureInterpolation: Boolean= True); virtual; abstract; overload;
     procedure FillQuadLinearMappingAntialias(pt1,pt2,pt3,pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF); virtual; abstract; overload;
     procedure FillQuadPerspectiveMapping(pt1,pt2,pt3,pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF); virtual; abstract; overload;
     procedure FillQuadPerspectiveMapping(pt1,pt2,pt3,pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF; ACleanBorders: TRect); virtual; abstract; overload;
     procedure FillQuadPerspectiveMappingAntialias(pt1,pt2,pt3,pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF); virtual; abstract; overload;
     procedure FillQuadPerspectiveMappingAntialias(pt1,pt2,pt3,pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF; ACleanBorders: TRect); virtual; abstract; overload;

     procedure FillPolyLinearColor(const points: array of TPointF; AColors: array of TBGRAPixel);  virtual; abstract; overload;
     procedure FillPolyLinearMapping(const points: array of TPointF; texture: IBGRAScanner; texCoords: array of TPointF; TextureInterpolation: Boolean); virtual; abstract; overload;
     procedure FillPolyLinearMappingLightness(const points: array of TPointF; texture: IBGRAScanner; texCoords: array of TPointF; lightnesses: array of word; TextureInterpolation: Boolean); virtual; abstract; overload;
     procedure FillPolyPerspectiveMapping(const points: array of TPointF; const pointsZ: array of single; texture: IBGRAScanner; texCoords: array of TPointF; TextureInterpolation: Boolean; zbuffer: psingle = nil); virtual; abstract; overload;
     procedure FillPolyPerspectiveMappingLightness(const points: array of TPointF; const pointsZ: array of single; texture: IBGRAScanner; texCoords: array of TPointF; lightnesses: array of word; TextureInterpolation: Boolean; zbuffer: psingle = nil); virtual; abstract; overload;

     procedure FillPoly(const points: array of TPointF; c: TBGRAPixel; drawmode: TDrawMode); virtual; abstract;
     procedure FillPoly(const points: array of TPointF; texture: IBGRAScanner; drawmode: TDrawMode); virtual; abstract;
     procedure FillPolyAntialias(const points: array of TPointF; c: TBGRAPixel); virtual; abstract;
     procedure FillPolyAntialias(const points: array of TPointF; texture: IBGRAScanner); virtual; abstract;
     procedure ErasePoly(const points: array of TPointF; alpha: byte); virtual; abstract;
     procedure ErasePolyAntialias(const points: array of TPointF; alpha: byte); virtual; abstract;

     procedure FillShape(shape: TBGRACustomFillInfo; c: TBGRAPixel; drawmode: TDrawMode); virtual; abstract;
     procedure FillShape(shape: TBGRACustomFillInfo; texture: IBGRAScanner; drawmode: TDrawMode); virtual; abstract;
     procedure FillShapeAntialias(shape: TBGRACustomFillInfo; c: TBGRAPixel); virtual; abstract;
     procedure FillShapeAntialias(shape: TBGRACustomFillInfo; texture: IBGRAScanner); virtual; abstract;
     procedure EraseShape(shape: TBGRACustomFillInfo; alpha: byte); virtual; abstract;
     procedure EraseShapeAntialias(shape: TBGRACustomFillInfo; alpha: byte); virtual; abstract;


     procedure EllipseAntialias(x, y, rx, ry: single; c: TBGRAPixel; w: single); virtual; abstract;
     procedure EllipseAntialias(x, y, rx, ry: single; texture: IBGRAScanner; w: single); virtual; abstract;
     procedure EllipseAntialias(x, y, rx, ry: single; c: TBGRAPixel; w: single; back: TBGRAPixel); virtual; abstract;
     procedure FillEllipseAntialias(x, y, rx, ry: single; c: TBGRAPixel); virtual; abstract;
     procedure FillEllipseAntialias(x, y, rx, ry: single; texture: IBGRAScanner); virtual; abstract;
     procedure FillEllipseLinearColorAntialias(x, y, rx, ry: single; outercolor, innercolor: TBGRAPixel); virtual; abstract;
     procedure EraseEllipseAntialias(x, y, rx, ry: single; alpha: byte); virtual; abstract;

     procedure Rectangle(x, y, x2, y2: integer; c: TBGRAPixel; mode: TDrawMode); virtual; abstract; overload;
     procedure Rectangle(x, y, x2, y2: integer; BorderColor, FillColor: TBGRAPixel; mode: TDrawMode); virtual; abstract; overload;
     procedure Rectangle(x, y, x2, y2: integer; c: TColor); virtual; overload;
     procedure Rectangle(r: TRect; c: TBGRAPixel; mode: TDrawMode); virtual; overload;
     procedure Rectangle(r: TRect; BorderColor, FillColor: TBGRAPixel; mode: TDrawMode); virtual;overload;
     procedure Rectangle(r: TRect; c: TColor); virtual; overload;
     procedure RectangleAntialias(x, y, x2, y2: single; c: TBGRAPixel; w: single); virtual; overload;
     procedure RectangleAntialias(x, y, x2, y2: single; c: TBGRAPixel; w: single; back: TBGRAPixel); virtual; abstract; overload;
     procedure RectangleAntialias(x, y, x2, y2: single; texture: IBGRAScanner; w: single); virtual; abstract; overload;

     procedure RoundRect(X1, Y1, X2, Y2: integer; DX, DY: integer; BorderColor, FillColor: TBGRAPixel); virtual; abstract;
     procedure RoundRectAntialias(x,y,x2,y2,rx,ry: single; c: TBGRAPixel; w: single; options: TRoundRectangleOptions = []); virtual; abstract;
     procedure RoundRectAntialias(x,y,x2,y2,rx,ry: single; pencolor: TBGRAPixel; w: single; fillcolor: TBGRAPixel; options: TRoundRectangleOptions = []); virtual; abstract;
     procedure RoundRectAntialias(x,y,x2,y2,rx,ry: single; penTexture: IBGRAScanner; w: single; fillTexture: IBGRAScanner; options: TRoundRectangleOptions = []); virtual; abstract;
     procedure RoundRectAntialias(x,y,x2,y2,rx,ry: single; texture: IBGRAScanner; w: single; options: TRoundRectangleOptions = []); virtual; abstract;
     procedure FillRoundRectAntialias(x,y,x2,y2,rx,ry: single; c: TBGRAPixel; options: TRoundRectangleOptions = []); virtual; abstract;
     procedure FillRoundRectAntialias(x,y,x2,y2,rx,ry: single; texture: IBGRAScanner; options: TRoundRectangleOptions = []); virtual; abstract;
     procedure EraseRoundRectAntialias(x,y,x2,y2,rx,ry: single; alpha: byte; options: TRoundRectangleOptions = []); virtual; abstract;

     procedure FillRect(r: TRect; c: TColor); virtual; overload;
     procedure FillRect(r: TRect; c: TBGRAPixel; mode: TDrawMode); virtual; overload;
     procedure FillRect(r: TRect; texture: IBGRAScanner; mode: TDrawMode); virtual; overload;
     procedure FillRect(x, y, x2, y2: integer; c: TColor); virtual; overload;
     procedure FillRect(x, y, x2, y2: integer; c: TBGRAPixel; mode: TDrawMode); virtual; abstract; overload;
     procedure FillRect(x, y, x2, y2: integer; texture: IBGRAScanner; mode: TDrawMode); virtual; abstract; overload;
     procedure FillRectAntialias(x, y, x2, y2: single; c: TBGRAPixel); virtual; abstract;
     procedure FillRectAntialias(x, y, x2, y2: single; texture: IBGRAScanner); virtual; abstract;
     procedure EraseRectAntialias(x, y, x2, y2: single; alpha: byte); virtual; abstract;
     procedure AlphaFillRect(x, y, x2, y2: integer; alpha: byte); virtual; abstract;

     procedure TextOut(x, y: single; sUTF8: string; c: TBGRAPixel; align: TAlignment); virtual; abstract; overload;
     procedure TextOut(x, y: single; sUTF8: string; texture: IBGRAScanner; align: TAlignment); virtual; abstract; overload;
     procedure TextOutAngle(x, y: single; orientationTenthDegCCW: integer; sUTF8: string; c: TBGRAPixel; align: TAlignment); virtual; abstract;
     procedure TextOutAngle(x, y: single; orientationTenthDegCCW: integer; sUTF8: string; texture: IBGRAScanner; align: TAlignment); virtual; abstract;
     procedure TextOut(x, y: single; sUTF8: string; c: TBGRAPixel); virtual; overload;
     procedure TextOut(x, y: single; sUTF8: string; c: TColor); virtual; overload;
     procedure TextOut(x, y: single; sUTF8: string; texture: IBGRAScanner); virtual; overload;
     procedure TextRect(ARect: TRect; x, y: integer; sUTF8: string; style: TTextStyle; c: TBGRAPixel); virtual; abstract; overload;
     procedure TextRect(ARect: TRect; x, y: integer; sUTF8: string; style: TTextStyle; texture: IBGRAScanner); virtual; abstract; overload;
     procedure TextRect(ARect: TRect; sUTF8: string; halign: TAlignment; valign: TTextLayout; c: TBGRAPixel); virtual; overload;
     procedure TextRect(ARect: TRect; sUTF8: string; halign: TAlignment; valign: TTextLayout; texture: IBGRAScanner); virtual; overload;
     function TextSize(sUTF8: string): TSize; virtual; abstract;

     {Spline}
     function ComputeClosedSpline(const APoints: array of TPointF; AStyle: TSplineStyle): ArrayOfTPointF; virtual; abstract;
     function ComputeOpenedSpline(const APoints: array of TPointF; AStyle: TSplineStyle): ArrayOfTPointF; virtual; abstract;
     function ComputeBezierCurve(const curve: TCubicBezierCurve): ArrayOfTPointF; virtual; abstract;
     function ComputeBezierCurve(const curve: TQuadraticBezierCurve): ArrayOfTPointF; virtual; abstract;
     function ComputeBezierSpline(const spline: array of TCubicBezierCurve): ArrayOfTPointF; virtual; abstract;
     function ComputeBezierSpline(const spline: array of TQuadraticBezierCurve): ArrayOfTPointF; virtual; abstract;

     function ComputeWidePolyline(const points: array of TPointF; w: single): ArrayOfTPointF; virtual; abstract;
     function ComputeWidePolyline(const points: array of TPointF; w: single; Closed: boolean): ArrayOfTPointF; virtual; abstract;
     function ComputeWidePolygon(const points: array of TPointF; w: single): ArrayOfTPointF; virtual; abstract;

     function ComputeEllipse(x,y,rx,ry: single): ArrayOfTPointF; deprecated;
     function ComputeEllipse(x,y,rx,ry,w: single): ArrayOfTPointF; deprecated;
     function ComputeEllipseContour(x,y,rx,ry: single; quality: single = 1): ArrayOfTPointF; virtual; abstract;
     function ComputeEllipseBorder(x,y,rx,ry,w: single; quality: single = 1): ArrayOfTPointF; virtual; abstract;
     function ComputeArc65536(x,y,rx,ry: single; start65536,end65536: word; quality: single = 1): ArrayOfTPointF; virtual; abstract;
     function ComputeArcRad(x,y,rx,ry: single; startRad,endRad: single; quality: single = 1): ArrayOfTPointF; virtual; abstract;
     function ComputeRoundRect(x1,y1,x2,y2,rx,ry: single; quality: single = 1): ArrayOfTPointF; virtual; abstract;
     function ComputeRoundRect(x1,y1,x2,y2,rx,ry: single; options: TRoundRectangleOptions; quality: single = 1): ArrayOfTPointF; virtual; abstract;
     function ComputePie65536(x,y,rx,ry: single; start65536,end65536: word; quality: single = 1): ArrayOfTPointF; virtual; abstract;
     function ComputePieRad(x,y,rx,ry: single; startRad,endRad: single; quality: single = 1): ArrayOfTPointF; virtual; abstract;

     {Filling}
     procedure FillTransparent; virtual;
     procedure NoClip; virtual; abstract;
     procedure ApplyGlobalOpacity(alpha: byte); virtual; abstract;
     procedure Fill(c: TColor); virtual; overload;
     procedure Fill(c: TBGRAPixel); virtual; overload;
     procedure Fill(texture: IBGRAScanner; mode: TDrawMode); virtual; abstract; overload;
     procedure Fill(texture: IBGRAScanner); virtual; abstract; overload;
     procedure Fill(c: TBGRAPixel; start, Count: integer); virtual; abstract; overload;
     procedure DrawPixels(c: TBGRAPixel; start, Count: integer); virtual; abstract;
     procedure AlphaFill(alpha: byte); virtual; overload;
     procedure AlphaFill(alpha: byte; start, Count: integer); virtual; abstract; overload;
     procedure FillMask(x,y: integer; AMask: TBGRACustomBitmap; color: TBGRAPixel); virtual; overload;
     procedure FillMask(x,y: integer; AMask: TBGRACustomBitmap; texture: IBGRAScanner); virtual; overload;
     procedure FillMask(x,y: integer; AMask: TBGRACustomBitmap; color: TBGRAPixel; ADrawMode: TDrawMode); virtual; abstract; overload;
     procedure FillMask(x,y: integer; AMask: TBGRACustomBitmap; texture: IBGRAScanner; ADrawMode: TDrawMode); virtual; abstract; overload;
     procedure FillClearTypeMask(x,y: integer; xThird: integer; AMask: TBGRACustomBitmap; color: TBGRAPixel; ARGBOrder: boolean = true); virtual; abstract; overload;
     procedure FillClearTypeMask(x,y: integer; xThird: integer; AMask: TBGRACustomBitmap; texture: IBGRAScanner; ARGBOrder: boolean = true); virtual; abstract; overload;
     procedure ReplaceColor(before, after: TColor); virtual; abstract; overload;
     procedure ReplaceColor(before, after: TBGRAPixel); virtual; abstract; overload;
     procedure ReplaceTransparent(after: TBGRAPixel); virtual; abstract; overload;
     procedure FloodFill(X, Y: integer; Color: TBGRAPixel;
       mode: TFloodfillMode; Tolerance: byte = 0); virtual;
     procedure ParallelFloodFill(X, Y: integer; Dest: TBGRACustomBitmap; Color: TBGRAPixel;
       mode: TFloodfillMode; Tolerance: byte = 0); virtual; abstract;
     procedure GradientFill(x, y, x2, y2: integer; c1, c2: TBGRAPixel;
       gtype: TGradientType; o1, o2: TPointF; mode: TDrawMode;
       gammaColorCorrection: boolean = True; Sinus: Boolean=False); virtual; abstract;
     procedure GradientFill(x, y, x2, y2: integer; gradient: TBGRACustomGradient;
       gtype: TGradientType; o1, o2: TPointF; mode: TDrawMode;
       Sinus: Boolean=False); virtual; abstract;
     function CreateBrushTexture(ABrushStyle: TBrushStyle; APatternColor, ABackgroundColor: TBGRAPixel;
                AWidth: integer = 8; AHeight: integer = 8; APenWidth: single = 1): TBGRACustomBitmap; virtual; abstract;

     {Canvas drawing functions}
     procedure DataDrawTransparent(ACanvas: TCanvas; Rect: TRect;
       AData: Pointer; ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer); virtual; abstract;
     procedure DataDrawOpaque(ACanvas: TCanvas; Rect: TRect; AData: Pointer;
       ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer); virtual; abstract;
     procedure GetImageFromCanvas(CanvasSource: TCanvas; x, y: integer); virtual; abstract;
     procedure Draw(ACanvas: TCanvas; x, y: integer; Opaque: boolean = True); virtual; abstract;
     procedure Draw(ACanvas: TCanvas; Rect: TRect; Opaque: boolean = True); virtual; abstract;
     procedure DrawPart(ARect: TRect; Canvas: TCanvas; x, y: integer; Opaque: boolean); virtual;
     function GetPart(ARect: TRect): TBGRACustomBitmap; virtual; abstract;
     function GetPtrBitmap(Top,Bottom: Integer): TBGRACustomBitmap; virtual; abstract;
     procedure InvalidateBitmap; virtual; abstract;         //call if you modify with Scanline
     procedure LoadFromBitmapIfNeeded; virtual; abstract;   //call to ensure that bitmap data is up to date

     {BGRA bitmap functions}
     procedure CrossFade(ARect: TRect; Source1, Source2: IBGRAScanner; AFadePosition: byte; mode: TDrawMode = dmDrawWithTransparency); virtual; abstract;
     procedure CrossFade(ARect: TRect; Source1, Source2: IBGRAScanner; AFadeMask: IBGRAScanner; mode: TDrawMode = dmDrawWithTransparency); virtual; abstract;
     procedure PutImage(x, y: integer; Source: TBGRACustomBitmap; mode: TDrawMode; AOpacity: byte = 255); virtual; abstract;
     procedure StretchPutImage(ARect: TRect; Source: TBGRACustomBitmap; mode: TDrawMode; AOpacity: byte = 255); virtual; abstract;
     procedure PutImageSubpixel(x, y: single; Source: TBGRACustomBitmap);
     procedure PutImagePart(x,y: integer; Source: TBGRACustomBitmap; SourceRect: TRect; mode: TDrawMode; AOpacity: byte = 255);
     procedure PutImageAffine(Origin,HAxis,VAxis: TPointF; Source: TBGRACustomBitmap; AOpacity: Byte=255; ACorrectBlur: Boolean = false); overload;
     procedure PutImageAffine(Origin,HAxis,VAxis: TPointF; Source: TBGRACustomBitmap; AResampleFilter: TResampleFilter; AOpacity: Byte=255); overload;
     procedure PutImageAffine(Origin,HAxis,VAxis: TPointF; Source: TBGRACustomBitmap; AOutputBounds: TRect; AResampleFilter: TResampleFilter; AMode: TDrawMode; AOpacity: Byte=255); virtual; abstract; overload;
     procedure PutImageAffine(Origin,HAxis,VAxis: TPointF; Source: TBGRACustomBitmap; AOutputBounds: TRect; AOpacity: Byte=255; ACorrectBlur: Boolean = false); overload;
     function GetImageAffineBounds(Origin,HAxis,VAxis: TPointF; Source: TBGRACustomBitmap): TRect;
     procedure PutImageAngle(x,y: single; Source: TBGRACustomBitmap; angle: single; AOutputBounds: TRect; imageCenterX: single = 0; imageCenterY: single = 0; AOpacity: Byte=255; ARestoreOffsetAfterRotation: boolean = false; ACorrectBlur: Boolean = false); overload;
     procedure PutImageAngle(x,y: single; Source: TBGRACustomBitmap; angle: single; imageCenterX: single = 0; imageCenterY: single = 0; AOpacity: Byte=255; ARestoreOffsetAfterRotation: boolean = false; ACorrectBlur: Boolean = false); overload;
     procedure PutImageAngle(x,y: single; Source: TBGRACustomBitmap; angle: single; AOutputBounds: TRect; AResampleFilter: TResampleFilter; imageCenterX: single = 0; imageCenterY: single = 0; AOpacity: Byte=255; ARestoreOffsetAfterRotation: boolean = false); overload;
     procedure PutImageAngle(x,y: single; Source: TBGRACustomBitmap; angle: single; AResampleFilter: TResampleFilter; imageCenterX: single = 0; imageCenterY: single = 0; AOpacity: Byte=255; ARestoreOffsetAfterRotation: boolean = false); overload;
     procedure ComputeImageAngleAxes(x,y,w,h,angle: single; imageCenterX,imageCenterY: single; ARestoreOffsetAfterRotation: boolean;
       out Origin,HAxis,VAxis: TPointF);
     function GetImageAngleBounds(x,y: single; Source: TBGRACustomBitmap; angle: single; imageCenterX: single = 0; imageCenterY: single = 0; ARestoreOffsetAfterRotation: boolean = false): TRect;
     procedure BlendImage(x, y: integer; Source: TBGRACustomBitmap; operation: TBlendOperation); virtual; abstract;
     procedure BlendImageOver(x, y: integer; Source: TBGRACustomBitmap; operation: TBlendOperation; AOpacity: byte = 255;
         ALinearBlend: boolean = false); virtual; abstract;
     function Duplicate(DuplicateProperties: Boolean = False): TBGRACustomBitmap; virtual; abstract;
     function Equals(comp: TBGRACustomBitmap): boolean; virtual; abstract;
     function Equals(comp: TBGRAPixel): boolean; virtual; abstract;
     function Resample(newWidth, newHeight: integer;
       mode: TResampleMode = rmFineResample): TBGRACustomBitmap; virtual; abstract;
     procedure VerticalFlip; virtual; overload;
     procedure VerticalFlip(ARect: TRect); virtual; abstract; overload;
     procedure HorizontalFlip; virtual; overload;
     procedure HorizontalFlip(ARect: TRect); virtual; abstract; overload;
     function RotateCW: TBGRACustomBitmap; virtual; abstract;
     function RotateCCW: TBGRACustomBitmap; virtual; abstract;
     procedure Negative; virtual; abstract;
     procedure NegativeRect(ABounds: TRect); virtual; abstract;
     procedure LinearNegative; virtual; abstract;
     procedure LinearNegativeRect(ABounds: TRect); virtual; abstract;
     procedure InplaceGrayscale; virtual; abstract;
     procedure InplaceGrayscale(ABounds: TRect); virtual; abstract;
     procedure ConvertToLinearRGB; virtual; abstract;
     procedure ConvertFromLinearRGB; virtual; abstract;
     procedure SwapRedBlue; virtual; abstract;
     procedure GrayscaleToAlpha; virtual; abstract;
     procedure AlphaToGrayscale; virtual; abstract;
     procedure ApplyMask(mask: TBGRACustomBitmap); overload;
     procedure ApplyMask(mask: TBGRACustomBitmap; ARect: TRect); overload;
     procedure ApplyMask(mask: TBGRACustomBitmap; ARect: TRect; AMaskRectTopLeft: TPoint); virtual; abstract; overload;
     function GetImageBounds(Channel: TChannel = cAlpha; ANothingValue: Byte = 0): TRect; virtual; abstract;
     function GetImageBounds(Channels: TChannels; ANothingValue: Byte = 0): TRect; virtual; abstract;
     function GetDifferenceBounds(ABitmap: TBGRACustomBitmap): TRect; virtual; abstract;
     function MakeBitmapCopy(BackgroundColor: TColor): TBitmap; virtual; abstract;

     {Filters}
     function FilterSmartZoom3(Option: TMedianOption): TBGRACustomBitmap; virtual; abstract;
     function FilterMedian(Option: TMedianOption): TBGRACustomBitmap; virtual; abstract;
     function FilterSmooth: TBGRACustomBitmap; virtual; abstract;
     function FilterSharpen(Amount: single = 1): TBGRACustomBitmap; virtual; abstract;
     function FilterSharpen(ABounds: TRect; Amount: single = 1): TBGRACustomBitmap; virtual; abstract;
     function FilterContour: TBGRACustomBitmap; virtual; abstract;
     function FilterPixelate(pixelSize: integer; useResample: boolean; filter: TResampleFilter = rfLinear): TBGRACustomBitmap; virtual; abstract;
     function FilterBlurRadial(radius: integer;
       blurType: TRadialBlurType): TBGRACustomBitmap; virtual; abstract;
     function FilterBlurRadial(ABounds: TRect; radius: integer;
       blurType: TRadialBlurType): TBGRACustomBitmap; virtual; abstract;
     function FilterBlurMotion(distance: integer; angle: single;
       oriented: boolean): TBGRACustomBitmap; virtual; abstract;
     function FilterBlurMotion(ABounds: TRect; distance: integer; angle: single;
       oriented: boolean): TBGRACustomBitmap; virtual; abstract;
     function FilterCustomBlur(mask: TBGRACustomBitmap): TBGRACustomBitmap; virtual; abstract;
     function FilterCustomBlur(ABounds: TRect; mask: TBGRACustomBitmap): TBGRACustomBitmap; virtual; abstract;
     function FilterEmboss(angle: single): TBGRACustomBitmap; virtual; abstract;
     function FilterEmboss(angle: single; ABounds: TRect): TBGRACustomBitmap; virtual; abstract;
     function FilterEmbossHighlight(FillSelection: boolean): TBGRACustomBitmap; virtual; abstract;
     function FilterEmbossHighlight(FillSelection: boolean; BorderColor: TBGRAPixel): TBGRACustomBitmap; virtual; abstract;
     function FilterEmbossHighlight(FillSelection: boolean; BorderColor: TBGRAPixel; var Offset: TPoint): TBGRACustomBitmap; virtual; abstract;
     function FilterGrayscale: TBGRACustomBitmap; virtual; abstract;
     function FilterGrayscale(ABounds: TRect): TBGRACustomBitmap; virtual; abstract;
     function FilterNormalize(eachChannel: boolean = True): TBGRACustomBitmap; virtual; abstract;
     function FilterNormalize(ABounds: TRect; eachChannel: boolean = True): TBGRACustomBitmap; virtual; abstract;
     function FilterRotate(origin: TPointF; angle: single; correctBlur: boolean = false): TBGRACustomBitmap; virtual; abstract;
     function FilterSphere: TBGRACustomBitmap; virtual; abstract;
     function FilterTwirl(ACenter: TPoint; ARadius: Single; ATurn: Single=1; AExponent: Single=3): TBGRACustomBitmap; virtual; abstract;
     function FilterTwirl(ABounds: TRect; ACenter: TPoint; ARadius: Single; ATurn: Single=1; AExponent: Single=3): TBGRACustomBitmap; virtual; abstract;
     function FilterCylinder: TBGRACustomBitmap; virtual; abstract;
     function FilterPlane: TBGRACustomBitmap; virtual; abstract;

     property Data: PBGRAPixel Read GetDataPtr;
     property Width: integer Read GetWidth;
     property Height: integer Read GetHeight;
     property NbPixels: integer Read GetNbPixels;
     property Empty: boolean Read CheckEmpty;

     property ScanLine[y: integer]: PBGRAPixel Read GetScanLine;
     property RefCount: integer Read GetRefCount;
     property Bitmap: TBitmap Read GetBitmap; //don't forget to call InvalidateBitmap before if you changed something with Scanline
     property HasTransparentPixels: boolean Read GetHasTransparentPixels;
     property AverageColor: TColor Read GetAverageColor;
     property AveragePixel: TBGRAPixel Read GetAveragePixel;
     property LineOrder: TRawImageLineOrder Read GetLineOrder;
     property CanvasFP: TFPImageCanvas read GetCanvasFP;
     property CanvasDrawModeFP: TDrawMode read GetCanvasDrawModeFP write SetCanvasDrawModeFP;
     property Canvas: TCanvas Read GetCanvas;
     property CanvasOpacity: byte Read GetCanvasOpacity Write SetCanvasOpacity;
     property CanvasAlphaCorrection: boolean
       Read GetCanvasAlphaCorrection Write SetCanvasAlphaCorrection;

     property FontHeight: integer Read GetFontHeight Write SetFontHeight;
     property PenStyle: TPenStyle read GetPenStyle Write SetPenStyle;
     property CustomPenStyle: TBGRAPenStyle read GetCustomPenStyle write SetCustomPenStyle;
     property ClipRect: TRect read GetClipRect write SetClipRect;
     property FontAntialias: Boolean read GetFontAntialias write SetFontAntialias; //antialiasing (it's different from TFont antialiasing mode)
     property FontFullHeight: integer read GetFontFullHeight write SetFontFullHeight;
     property FontPixelMetric: TFontPixelMetric read GetFontPixelMetric;
     property FontRenderer: TBGRACustomFontRenderer read GetFontRenderer write SetFontRenderer;

     //IBGRAScanner
     function ScanAtInteger(X,Y: integer): TBGRAPixel; virtual; abstract;
     procedure ScanMoveTo(X,Y: Integer); virtual; abstract;
     function ScanNextPixel: TBGRAPixel; virtual; abstract;
     function ScanAt(X,Y: Single): TBGRAPixel; virtual; abstract;
     procedure ScanPutPixels(pdest: PBGRAPixel; count: integer; mode: TDrawMode); virtual;
     function IsScanPutPixelsDefined: boolean; virtual;

  protected
     //interface
     function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
     function _AddRef: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
     function _Release: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};

  end;

  { TBGRACustomScanner }

  TBGRACustomScanner = class(IBGRAScanner)
  private
    FCurX,FCurY: integer;
  public
    function ScanAtInteger(X,Y: integer): TBGRAPixel; virtual;
    procedure ScanMoveTo(X,Y: Integer); virtual;
    function ScanNextPixel: TBGRAPixel; virtual;
    function ScanAt(X,Y: Single): TBGRAPixel; virtual; abstract;
    procedure ScanPutPixels(pdest: PBGRAPixel; count: integer; mode: TDrawMode); virtual;
    function IsScanPutPixelsDefined: boolean; virtual;
  protected
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
    function _AddRef: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
    function _Release: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
  end;

  { TBGRACustomGradient }

  TBGRACustomGradient = class
  public
    function GetColorAt(position: integer): TBGRAPixel; virtual; abstract;
    function GetColorAtF(position: single): TBGRAPixel; virtual;
    function GetAverageColor: TBGRAPixel; virtual; abstract;
    function GetMonochrome: boolean; virtual; abstract;
    property Monochrome: boolean read GetMonochrome;
  end;

  { TIntersectionInfo }

  TIntersectionInfo = class
    interX: single;
    winding: integer;
    numSegment: integer;
    procedure SetValues(AInterX: Single; AWinding, ANumSegment: integer);
  end;
  ArrayOfTIntersectionInfo = array of TIntersectionInfo;

  TBGRACustomFillInfo = class
    public
      //returns true if the same segment number can be curved
      function SegmentsCurved: boolean; virtual; abstract;

      //returns integer bounds
      function GetBounds: TRect; virtual; abstract;

      //compute min-max to be drawn on destination bitmap according to cliprect. Returns false if
      //there is nothing to draw
      function ComputeMinMax(out minx,miny,maxx,maxy: integer; bmpDest: TBGRACustomBitmap): boolean; virtual; abstract;

      //check if the point is inside the filling zone
      function IsPointInside(x,y: single; windingMode: boolean): boolean; virtual; abstract;

      //create an array that will contain computed intersections.
      //you may augment, in this case, use CreateIntersectionInfo for new items
      function CreateIntersectionArray: ArrayOfTIntersectionInfo; virtual; abstract;
      function CreateIntersectionInfo: TIntersectionInfo; virtual; abstract; //creates a single info
      procedure FreeIntersectionArray(var inter: ArrayOfTIntersectionInfo); virtual; abstract;

      //fill a previously created array of intersections with actual intersections at the current y coordinate.
      //nbInter gets the number of computed intersections
      procedure ComputeAndSort(cury: single; var inter: ArrayOfTIntersectionInfo; out nbInter: integer; windingMode: boolean); virtual; abstract;
  end;

  { TBGRACustomFontRenderer }

  TBGRACustomFontRenderer = class
    FontName: string;
    FontStyle: TFontStyles;
    FontQuality : TBGRAFontQuality;
    FontOrientation: integer;
    FontEmHeight: integer; //negative for full height
    function GetFontPixelMetric: TFontPixelMetric; virtual; abstract;
    procedure TextOutAngle(ADest: TBGRACustomBitmap; x, y: single; orientationTenthDegCCW: integer; sUTF8: string; c: TBGRAPixel; align: TAlignment); virtual; abstract;
    procedure TextOutAngle(ADest: TBGRACustomBitmap; x, y: single; orientationTenthDegCCW: integer; sUTF8: string; texture: IBGRAScanner; align: TAlignment); virtual; abstract;
    procedure TextOut(ADest: TBGRACustomBitmap; x, y: single; sUTF8: string; texture: IBGRAScanner; align: TAlignment); virtual; abstract;
    procedure TextOut(ADest: TBGRACustomBitmap; x, y: single; sUTF8: string; c: TBGRAPixel; align: TAlignment); virtual; abstract;
    procedure TextRect(ADest: TBGRACustomBitmap; ARect: TRect; x, y: integer; sUTF8: string; style: TTextStyle; c: TBGRAPixel); virtual; abstract;
    procedure TextRect(ADest: TBGRACustomBitmap; ARect: TRect; x, y: integer; sUTF8: string; style: TTextStyle; texture: IBGRAScanner); virtual; abstract;
    procedure CopyTextPathTo({%H-}ADest: IBGRAPath; {%H-}x, {%H-}y: single; {%H-}s: string; {%H-}align: TAlignment); virtual; //optional
    function TextSize(sUTF8: string): TSize; virtual; abstract;
  end;

type
  TBGRABitmapAny = class of TBGRACustomBitmap;  //used to create instances of the same type (see NewBitmap)
  TBGRATextOutImproveReadabilityMode = (irMask, irNormal, irClearTypeRGB, irClearTypeBGR);

var
  BGRABitmapFactory : TBGRABitmapAny;
  BGRATextOutImproveReadabilityProc : procedure (bmp: TBGRACustomBitmap; AFont: TFont; xf,yf: single; text: string; color: TBGRAPixel; tex: IBGRAScanner; align: TAlignment; mode : TBGRATextOutImproveReadabilityMode);

function CheckPutImageBounds(x, y, tx, ty: integer; out minxb, minyb, maxxb, maxyb, ignoreleft: integer; const cliprect: TRect): boolean; inline;

{ Color functions }
function GetIntensity(c: TExpandedPixel): word; inline;
function SetIntensity(c: TExpandedPixel; intensity: word): TExpandedPixel;
function GetLightness(const c: TExpandedPixel): word; inline;
function SetLightness(c: TExpandedPixel; lightness: word): TExpandedPixel;
function SetLightness(c: TExpandedPixel; lightness: word; curLightness: word): TExpandedPixel; //if you already know the current lightness of the color
function ApplyLightnessFast(color: TBGRAPixel; lightness: word): TBGRAPixel; inline;
function ApplyIntensityFast(color: TBGRAPixel; lightness: longword): TBGRAPixel;
function CombineLightness(lightness1,lightness2: Int32or64): Int32or64;
function BGRAToHSLA(c: TBGRAPixel): THSLAPixel;
function ExpandedToHSLA(const ec: TExpandedPixel): THSLAPixel;
function BGRAToGSBA(c: TBGRAPixel): THSLAPixel;
function HSLAToExpanded(const c: THSLAPixel): TExpandedPixel;
function HSLAToBGRA(const c: THSLAPixel): TBGRAPixel;
function GtoH(ghue: word): word;
function HtoG(hue: word): word;
function HueDiff(h1, h2: word): word;
function GetHue(ec: TExpandedPixel): word;
function ColorImportance(ec: TExpandedPixel): word;
function GSBAToBGRA(c: THSLAPixel): TBGRAPixel;
function GSBAToHSLA(c: THSLAPixel): THSLAPixel;
function GammaExpansion(c: TBGRAPixel): TExpandedPixel; inline;
function GammaCompression(const ec: TExpandedPixel): TBGRAPixel; inline;
function GammaCompression(red,green,blue,alpha: word): TBGRAPixel; inline;
function BGRAToGrayscale(c: TBGRAPixel): TBGRAPixel;
function GrayscaleToBGRA(lightness: word): TBGRAPixel;
function MergeBGRA(const colors: array of TBGRAPixel): TBGRAPixel; overload;
function MergeBGRAWithGammaCorrection(c1: TBGRAPixel; weight1: byte; c2: TBGRAPixel; weight2: byte): TBGRAPixel;
function MergeBGRA(c1, c2: TBGRAPixel): TBGRAPixel; overload;
function MergeBGRA(c1: TBGRAPixel; weight1: integer; c2: TBGRAPixel; weight2: integer): TBGRAPixel; overload;
function MergeBGRA(ec1, ec2: TExpandedPixel): TExpandedPixel; overload;
function BGRA(red, green, blue, alpha: byte): TBGRAPixel; overload; inline;
function BGRA(red, green, blue: byte): TBGRAPixel; overload; inline;
function HSLA(hue, saturation, lightness, alpha: word): THSLAPixel; overload; inline;
function HSLA(hue, saturation, lightness: word): THSLAPixel; overload; inline;
function ColorToBGRA(color: TColor): TBGRAPixel; overload;
function ColorToBGRA(color: TColor; opacity: byte): TBGRAPixel; overload;
function BGRAToFPColor(AValue: TBGRAPixel): TFPColor; inline;
function FPColorToBGRA(AValue: TFPColor): TBGRAPixel;
function BGRAToColor(c: TBGRAPixel): TColor;
operator = (const c1, c2: TBGRAPixel): boolean; inline;
function ExpandedDiff(ec1, ec2: TExpandedPixel): word;
function BGRAWordDiff(c1, c2: TBGRAPixel): word;
function BGRADiff(c1, c2: TBGRAPixel): byte;
operator - (const c1, c2: TColorF): TColorF; inline;
operator + (const c1, c2: TColorF): TColorF; inline;
operator * (const c1, c2: TColorF): TColorF; inline;
operator * (const c1: TColorF; factor: single): TColorF; inline;
function ColorF(red,green,blue,alpha: single): TColorF;
function BGRAToStr(c: TBGRAPixel; AColorList: TBGRAColorList = nil; AMaxDiff: Word= 0): string;
function StrToBGRA(str: string): TBGRAPixel; //full parse
function StrToBGRA(str: string; const DefaultColor: TBGRAPixel): TBGRAPixel; //full parse with default when error or missing values
function PartialStrToBGRA(str: string; const fallbackValues: TBGRAPixel; out error: boolean): TBGRAPixel; //partial parse allowed
procedure TryStrToBGRA(str: string; var parsedValue: TBGRAPixel; out missingValues: boolean; out error: boolean);

{ Get height [0..1] stored in a TBGRAPixel }
function MapHeight(Color: TBGRAPixel): Single;

{ Get TBGRAPixel to store height [0..1] }
function MapHeightToBGRA(Height: Single; Alpha: Byte): TBGRAPixel;


{ Gamma conversion arrays. Should be used as readonly }
var
  // TBGRAPixel -> TExpandedPixel
  GammaExpansionTab:   packed array[0..255] of word;
  
  // TExpandedPixel -> TBGRAPixel
  GammaCompressionTab: packed array[0..65535] of byte;

{ Point functions }
function PointF(x, y: single): TPointF;
function PointsF(const pts: array of TPointF): ArrayOfTPointF;
operator = (const pt1, pt2: TPointF): boolean; inline;
operator - (const pt1, pt2: TPointF): TPointF; inline;
operator - (const pt2: TPointF): TPointF; inline;
operator + (const pt1, pt2: TPointF): TPointF; inline;
operator * (const pt1, pt2: TPointF): single; inline; //scalar product
operator * (const pt1: TPointF; factor: single): TPointF; inline;
operator * (factor: single; const pt1: TPointF): TPointF; inline;
function PtInRect(const pt: TPoint; r: TRect): boolean; overload;
function RectWithSize(left,top,width,height: integer): TRect;
function VectLen(dx,dy: single): single; overload;
function VectLen(v: TPointF): single; overload;

{ Line and polygon functions }
type
    TLineDef = record
       origin, dir: TPointF;
    end;

function IntersectLine(line1, line2: TLineDef): TPointF;
function IntersectLine(line1, line2: TLineDef; out parallel: boolean): TPointF;
function IsConvex(const pts: array of TPointF; IgnoreAlign: boolean = true): boolean;
function DoesQuadIntersect(pt1,pt2,pt3,pt4: TPointF): boolean;
function DoesSegmentIntersect(pt1,pt2,pt3,pt4: TPointF): boolean;

{ Cyclic functions }
function PositiveMod(value, cycle: Int32or64): Int32or64; inline; overload;

{ Sin65536 and Cos65536 are fast routines to compute sine and cosine as integer values.
  They use a table to store already computed values. The return value is an integer
  ranging from 0 to 65536, so the mean value is 32768 and the half amplitude is
  32768 instead of 1. The input has a period of 65536, so you can supply any integer
  without applying a modulo. }
procedure PrecalcSin65536; // compute all values now
function Sin65536(value: word): Int32or64; inline;
function Cos65536(value: word): Int32or64; inline;
function ByteSqrt(value: byte): byte; inline;

function DetectFileFormat(AFilenameUTF8: string): TBGRAImageFormat;
function DetectFileFormat(AStream: TStream; ASuggestedExtensionUTF8: string = ''): TBGRAImageFormat;
function SuggestImageFormat(AFilenameOrExtensionUTF8: string): TBGRAImageFormat;
function CreateBGRAImageReader(AFormat: TBGRAImageFormat): TFPCustomImageReader;
function CreateBGRAImageWriter(AFormat: TBGRAImageFormat; AHasTransparentPixels: boolean): TFPCustomImageWriter;

implementation

uses Math, SysUtils, FileUtil, lazutf8classes, LCLProc,
  FPReadTiff, FPReadXwd, FPReadXPM,
  FPWriteTiff, FPWriteJPEG, FPWritePNG, FPWriteBMP, FPWritePCX,
  FPWriteTGA, FPWriteXPM;

function StrToResampleFilter(str: string): TResampleFilter;
var f: TResampleFilter;
begin
  result := rfLinear;
  str := LowerCase(str);
  for f := low(TResampleFilter) to high(TResampleFilter) do
    if CompareText(str,ResampleFilterStr[f])=0 then
    begin
      result := f;
      exit;
    end;
end;

function StrToBlendOperation(str: string): TBlendOperation;
var op: TBlendOperation;
begin
  result := boTransparent;
  str := LowerCase(str);
  for op := low(TBlendOperation) to high(TBlendOperation) do
    if str = LowerCase(BlendOperationStr[op]) then
    begin
      result := op;
      exit;
    end;
end;

function StrToGradientType(str: string): TGradientType;
var gt: TGradientType;
begin
  result := gtLinear;
  str := LowerCase(str);
  for gt := low(TGradientType) to high(TGradientType) do
    if str = LowerCase(GradientTypeStr[gt]) then
    begin
      result := gt;
      exit;
    end;
end;

{ Make a pen style. Need an even number of values. See TBGRAPenStyle }
function BGRAPenStyle(dash1, space1: single; dash2: single; space2: single;
  dash3: single; space3: single; dash4: single; space4: single): TBGRAPenStyle;
var
  i: Integer;
begin
  if dash4 <> 0 then
  begin
    setlength(result,8);
    result[6] := dash4;
    result[7] := space4;
    result[4] := dash3;
    result[5] := space3;
    result[2] := dash2;
    result[3] := space2;
  end else
  if dash3 <> 0 then
  begin
    setlength(result,6);
    result[4] := dash3;
    result[5] := space3;
    result[2] := dash2;
    result[3] := space2;
  end else
  if dash2 <> 0 then
  begin
    setlength(result,4);
    result[2] := dash2;
    result[3] := space2;
  end else
  begin
    setlength(result,2);
  end;
  result[0] := dash1;
  result[1] := space1;
  for i := 0 to high(result) do
    if result[i]=0 then
      raise exception.Create('Zero is not a valid value');
end;

{ Bézier curves definitions. See : http://en.wikipedia.org/wiki/B%C3%A9zier_curve }

function ConcatPointsF(const APolylines: array of ArrayOfTPointF
  ): ArrayOfTPointF;
var
  i,pos,count:integer;
  j: Integer;
begin
  count := 0;
  for i := 0 to high(APolylines) do
    inc(count,length(APolylines[i]));
  setlength(result,count);
  pos := 0;
  for i := 0 to high(APolylines) do
    for j := 0 to high(APolylines[i]) do
    begin
      result[pos] := APolylines[i][j];
      inc(pos);
    end;
end;

operator-(const v: TPoint3D): TPoint3D;
begin
  result.x := -v.x;
  result.y := -v.y;
  result.z := -v.z;
end;

operator + (const v1,v2: TPoint3D): TPoint3D; inline;
begin
  result.x := v1.x+v2.x;
  result.y := v1.y+v2.y;
  result.z := v1.z+v2.z;
end;

operator - (const v1,v2: TPoint3D): TPoint3D; inline;
begin
  result.x := v1.x-v2.x;
  result.y := v1.y-v2.y;
  result.z := v1.z-v2.z;
end;

operator * (const v1: TPoint3D; const factor: single): TPoint3D; inline;
begin
  result.x := v1.x*factor;
  result.y := v1.y*factor;
  result.z := v1.z*factor;
end;

function Point3D(x, y, z: single): TPoint3D;
begin
  result.x := x;
  result.y := y;
  result.z := z;
end;

operator=(const v1, v2: TPoint3D): boolean;
begin
  result := (v1.x=v2.x) and (v1.y=v2.y) and (v1.z=v2.z);
end;

operator * (const v1,v2: TPoint3D): single; inline;
begin
  result := v1.x*v2.x + v1.y*v2.y + v1.z*v2.z;
end;

procedure Normalize3D(var v: TPoint3D); inline;
var len: double;
begin
  len := v*v;
  if len = 0 then exit;
  len := sqrt(len);
  v.x /= len;
  v.y /= len;
  v.z /= len;
end;

procedure VectProduct3D(u,v: TPoint3D; out w: TPoint3D);
begin
  w.x := u.y*v.z-u.z*v.y;
  w.y := u.z*v.x-u.x*v.z;
  w.z := u.x*v.Y-u.y*v.x;
end;

// Define a Bézier curve with two control points.
function BezierCurve(origin, control1, control2, destination: TPointF): TCubicBezierCurve;
begin
  result.p1 := origin;
  result.c1 := control1;
  result.c2 := control2;
  result.p2 := destination;
end;

// Define a Bézier curve with one control point.
function BezierCurve(origin, control, destination: TPointF
  ): TQuadraticBezierCurve;
begin
  result.p1 := origin;
  result.c := control;
  result.p2 := destination;
end;

//straight line
function BezierCurve(origin, destination: TPointF): TQuadraticBezierCurve;
begin
  result.p1 := origin;
  result.c := (origin+destination)*0.5;
  result.p2 := destination;
end;

function ArcDef(cx, cy, rx, ry, xAngleRadCW, startAngleRadCW, endAngleRadCW: single;
  anticlockwise: boolean): TArcDef;
begin
  result.center := PointF(cx,cy);
  result.radius := PointF(rx,ry);
  result.xAngleRadCW:= xAngleRadCW;
  result.startAngleRadCW := startAngleRadCW;
  result.endAngleRadCW:= endAngleRadCW;
  result.anticlockwise:= anticlockwise;
end;

{ Check if a PointF structure is empty or should be treated as a list separator }
function isEmptyPointF(pt: TPointF): boolean;
begin
  Result := (pt.x = EmptySingle) and (pt.y = EmptySingle);
end;

{ TBGRACustomFontRenderer }

procedure TBGRACustomFontRenderer.CopyTextPathTo(ADest: IBGRAPath; x, y: single; s: string; align: TAlignment);
begin
end;

{ TIntersectionInfo }

procedure TIntersectionInfo.SetValues(AInterX: Single; AWinding,
  ANumSegment: integer);
begin
  interX := AInterX;
  winding := AWinding;
  numSegment := ANumSegment;
end;

{ TBGRACustomGradient }

function TBGRACustomGradient.GetColorAtF(position: single): TBGRAPixel;
begin
  position *= 65536;
  if position < low(integer) then
    result := GetColorAt(low(Integer))
  else if position > high(integer) then
    result := GetColorAt(high(Integer))
  else
    result := GetColorAt(round(position));
end;

{ TBGRAColorList }

function TBGRAColorList.GetByIndex(Index: integer): TBGRAPixel;
begin
  if (Index < 0) or (Index >= FNbColors) then
    result := BGRAPixelTransparent
  else
    result := FColors[Index].Color;
end;

function TBGRAColorList.GetByName(Name: string): TBGRAPixel;
var i: integer;
begin
  i := IndexOf(Name);
  if i = -1 then
    result := BGRAPixelTransparent
  else
    result := FColors[i].Color;
end;

function TBGRAColorList.GetName(Index: integer): string;
begin
  if (Index < 0) or (Index >= FNbColors) then
    result := ''
  else
    result := FColors[Index].Name;
end;

constructor TBGRAColorList.Create;
begin
  FNbColors:= 0;
  FColors := nil;
  FFinished:= false;
end;

procedure TBGRAColorList.Add(Name: string; const Color: TBGRAPixel);
begin
  if FFinished then
    raise Exception.Create('This list is already finished');
  if length(FColors) = FNbColors then
    SetLength(FColors, FNbColors*2+1);
  FColors[FNbColors].Name := Name;
  FColors[FNbColors].Color := Color;
  inc(FNbColors);
end;

procedure TBGRAColorList.Finished;
begin
  if FFinished then exit;
  FFinished := true;
  SetLength(FColors, FNbColors);
end;

function TBGRAColorList.IndexOf(Name: string): integer;
var i: integer;
begin
  for i := 0 to FNbColors-1 do
    if CompareText(Name, FColors[i].Name) = 0 then
    begin
      result := i;
      exit;
    end;
  result := -1;
end;

function TBGRAColorList.IndexOfColor(const AColor: TBGRAPixel; AMaxDiff: Word = 0): integer;
var i: integer;
  MinDiff,CurDiff: Word;
begin
  if AMaxDiff = 0 then
  begin
    for i := 0 to FNbColors-1 do
      if AColor = FColors[i].Color then
      begin
        result := i;
        exit;
      end;
    result := -1;
  end else
  begin
    MinDiff := AMaxDiff;
    result := -1;
    for i := 0 to FNbColors-1 do
    begin
      CurDiff := BGRAWordDiff(AColor,FColors[i].Color);
      if CurDiff <= MinDiff then
      begin
        result := i;
        MinDiff := CurDiff;
        if MinDiff = 0 then exit;
      end;
    end;
  end;
end;

{ TBGRACustomBitmap }

function TBGRACustomBitmap.GetFontAntialias: Boolean;
begin
  result := FontQuality <> fqSystem;
end;

procedure TBGRACustomBitmap.SetFontAntialias(const AValue: Boolean);
begin
  if AValue and not FontAntialias then
    FontQuality := fqFineAntialiasing
  else if not AValue and (FontQuality <> fqSystem) then
    FontQuality := fqSystem;
end;

{ These declaration make sure that these methods are virtual }
procedure TBGRACustomBitmap.LoadFromFile(const filename: string);
begin
  LoadFromFileUTF8(SysToUtf8(filename));
end;

procedure TBGRACustomBitmap.LoadFromFileUTF8(const filenameUTF8: string);
var
  Stream: TStream;
  format: TBGRAImageFormat;
  reader: TFPCustomImageReader;
begin
  stream := TFileStreamUTF8.Create(filenameUTF8,fmOpenRead or fmShareDenyWrite);
  try
    format := DetectFileFormat(Stream, ExtractFileExt(filenameUTF8));
    reader := CreateBGRAImageReader(format);
    try
      LoadFromStream(stream, reader);
    finally
      reader.Free;
    end;
  finally
    ClearTransparentPixels;
    stream.Free;
  end;
end;

procedure TBGRACustomBitmap.LoadFromFileUTF8(const filenameUTF8: string;
  AHandler: TFPCustomImageReader);
var
  Stream: TStream;
begin
  stream := TFileStreamUTF8.Create(filenameUTF8,fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(stream, AHandler);
  finally
    ClearTransparentPixels;
    stream.Free;
  end;
end;

procedure TBGRACustomBitmap.SaveToFile(const filename: string);
begin
  SaveToFileUTF8(SysToUtf8(filename));
end;

procedure TBGRACustomBitmap.SaveToFileUTF8(const filenameUTF8: string);
var
  writer: TFPCustomImageWriter;
  format: TBGRAImageFormat;
begin
  format := SuggestImageFormat(filenameUTF8);
  writer := CreateBGRAImageWriter(Format, HasTransparentPixels);
  try
    SaveToFileUTF8(filenameUTF8, writer);
  finally
    writer.free;
  end;
end;

procedure TBGRACustomBitmap.SaveToFile(const filename: string;
  Handler: TFPCustomImageWriter);
begin
  SaveToFileUTF8(SysToUtf8(filename),Handler);
end;

procedure TBGRACustomBitmap.SaveToFileUTF8(const filenameUTF8: string;
  Handler: TFPCustomImageWriter);
var
  stream: TFileStreamUTF8;
begin
   stream := TFileStreamUTF8.Create(filenameUTF8,fmCreate);
   try
     SaveToStream(stream, Handler);
   finally
     stream.Free;
   end;
end;

procedure TBGRACustomBitmap.LoadFromStream(Str: TStream);
var
  format: TBGRAImageFormat;
  reader: TFPCustomImageReader;
begin
  format := DetectFileFormat(Str);
  reader := CreateBGRAImageReader(format);
  try
    LoadFromStream(Str,reader);
  finally
    reader.Free;
  end;
end;

{ LoadFromStream uses TFPCustomImage routine, which uses
  Colors property to access pixels. That's why the
  FP drawing mode is temporarily changed to load
  bitmaps properly }
procedure TBGRACustomBitmap.LoadFromStream(Str: TStream;
  Handler: TFPCustomImageReader);
var
  OldDrawMode: TDrawMode;
begin
  OldDrawMode := CanvasDrawModeFP;
  CanvasDrawModeFP := dmSet;
  try
    inherited LoadFromStream(Str, Handler);
  finally
    CanvasDrawModeFP := OldDrawMode;
  end;
end;

{ Look for a pixel considering the bitmap is repeated in both directions }
function TBGRACustomBitmap.GetPixelCycle(x, y: int32or64): TBGRAPixel;
begin
  if (Width = 0) or (Height = 0) then
    Result := BGRAPixelTransparent
  else
    Result := (Scanline[PositiveMod(y,Height)] + PositiveMod(x,Width))^;
end;

{ Pixel polylines are constructed by concatenation }
procedure TBGRACustomBitmap.DrawPolyLineAntialias(const points: array of TPoint;
  c: TBGRAPixel; DrawLastPixel: boolean);
var i: integer;
begin
   if length(points) = 1 then
   begin
     if DrawLastPixel then DrawPixel(points[0].x,points[0].y,c);
   end
   else
     for i := 0 to high(points)-1 do
       DrawLineAntialias(points[i].x,points[i].Y,points[i+1].x,points[i+1].y,c,DrawLastPixel and (i=high(points)-1));
end;

procedure TBGRACustomBitmap.DrawPolyLineAntialias(const points: array of TPoint; c1,
  c2: TBGRAPixel; dashLen: integer; DrawLastPixel: boolean);
var i: integer;
  DashPos: integer;
begin
   DashPos := 0;
   if length(points) = 1 then
   begin
     if DrawLastPixel then DrawPixel(points[0].x,points[0].y,c1);
   end
   else
     for i := 0 to high(points)-1 do
       DrawLineAntialias(points[i].x,points[i].Y,points[i+1].x,points[i+1].y,c1,c2,dashLen,DrawLastPixel and (i=high(points)-1),DashPos);
end;

{ Following functions are defined for convenience }
procedure TBGRACustomBitmap.Rectangle(x, y, x2, y2: integer; c: TColor);
begin
  Rectangle(x, y, x2, y2, ColorToBGRA(c), dmSet);
end;

procedure TBGRACustomBitmap.Rectangle(r: TRect; c: TBGRAPixel; mode: TDrawMode
  );
begin
  Rectangle(r.left, r.top, r.right, r.bottom, c, mode);
end;

procedure TBGRACustomBitmap.Rectangle(r: TRect; BorderColor,
  FillColor: TBGRAPixel; mode: TDrawMode);
begin
  Rectangle(r.left, r.top, r.right, r.bottom, BorderColor, FillColor, mode);
end;

procedure TBGRACustomBitmap.Rectangle(r: TRect; c: TColor);
begin
  Rectangle(r.left, r.top, r.right, r.bottom, c);
end;

procedure TBGRACustomBitmap.RectangleAntialias(x, y, x2, y2: single;
  c: TBGRAPixel; w: single);
begin
  RectangleAntialias(x, y, x2, y2, c, w, BGRAPixelTransparent);
end;

procedure TBGRACustomBitmap.FillRect(r: TRect; c: TColor);
begin
  FillRect(r.Left, r.top, r.right, r.bottom, c);
end;

procedure TBGRACustomBitmap.FillRect(r: TRect; c: TBGRAPixel; mode: TDrawMode);
begin
  FillRect(r.Left, r.top, r.right, r.bottom, c, mode);
end;

procedure TBGRACustomBitmap.FillRect(r: TRect; texture: IBGRAScanner;
  mode: TDrawMode);
begin
  FillRect(r.Left, r.top, r.right, r.bottom, texture, mode);
end;

procedure TBGRACustomBitmap.FillRect(x, y, x2, y2: integer; c: TColor);
begin
  FillRect(x, y, x2, y2, ColorToBGRA(c), dmSet);
end;

procedure TBGRACustomBitmap.TextOut(x, y: single; sUTF8: string; c: TBGRAPixel);
begin
  TextOut(x, y, sUTF8, c, taLeftJustify);
end;

procedure TBGRACustomBitmap.TextOut(x, y: single; sUTF8: string; c: TColor);
begin
  TextOut(x, y, sUTF8, ColorToBGRA(c));
end;

procedure TBGRACustomBitmap.TextOut(x, y: single; sUTF8: string;
  texture: IBGRAScanner);
begin
  TextOut(x, y, sUTF8, texture, taLeftJustify);
end;

procedure TBGRACustomBitmap.TextRect(ARect: TRect; sUTF8: string;
  halign: TAlignment; valign: TTextLayout; c: TBGRAPixel);
var
  style: TTextStyle;
begin
  {$hints off}
  FillChar(style,sizeof(style),0);
  {$hints on}
  style.Alignment := halign;
  style.Layout := valign;
  style.Wordbreak := true;
  style.ShowPrefix := false;
  style.Clipping := false;
  TextRect(ARect,ARect.Left,ARect.Top,sUTF8,style,c);
end;

procedure TBGRACustomBitmap.TextRect(ARect: TRect; sUTF8: string;
  halign: TAlignment; valign: TTextLayout; texture: IBGRAScanner);
var
  style: TTextStyle;
begin
  {$hints off}
  FillChar(style,sizeof(style),0);
  {$hints on}
  style.Alignment := halign;
  style.Layout := valign;
  style.Wordbreak := true;
  style.ShowPrefix := false;
  style.Clipping := false;
  TextRect(ARect,ARect.Left,ARect.Top,sUTF8,style,texture);
end;

function TBGRACustomBitmap.ComputeEllipse(x, y, rx, ry: single): ArrayOfTPointF;
begin
  result := ComputeEllipseContour(x,y,rx,ry);
end;

function TBGRACustomBitmap.ComputeEllipse(x, y, rx, ry, w: single
  ): ArrayOfTPointF;
begin
  result := ComputeEllipseBorder(x,y,rx,ry,w);
end;

procedure TBGRACustomBitmap.FillTransparent;
begin
  Fill(BGRAPixelTransparent);
end;

procedure TBGRACustomBitmap.Fill(c: TColor);
begin
  Fill(ColorToBGRA(c));
end;

procedure TBGRACustomBitmap.Fill(c: TBGRAPixel);
begin
  Fill(c, 0, NbPixels);
end;

procedure TBGRACustomBitmap.AlphaFill(alpha: byte);
begin
  AlphaFill(alpha, 0, NbPixels);
end;

procedure TBGRACustomBitmap.FillMask(x, y: integer; AMask: TBGRACustomBitmap;
  color: TBGRAPixel);
begin
  FillMask(x,y, AMask, color, dmDrawWithTransparency);
end;

procedure TBGRACustomBitmap.FillMask(x, y: integer; AMask: TBGRACustomBitmap;
  texture: IBGRAScanner);
begin
  FillMask(x,y, AMask, texture, dmDrawWithTransparency);
end;

procedure TBGRACustomBitmap.FloodFill(X, Y: integer; Color: TBGRAPixel;
  mode: TFloodfillMode; Tolerance: byte);
begin
  ParallelFloodFill(X,Y,Self,Color,mode,Tolerance);
end;

procedure TBGRACustomBitmap.DrawPart(ARect: TRect; Canvas: TCanvas; x,
  y: integer; Opaque: boolean);
var
  partial: TBGRACustomBitmap;
begin
  partial := GetPart(ARect);
  if partial <> nil then
  begin
    partial.Draw(Canvas, x, y, Opaque);
    partial.Free;
  end;
end;

procedure TBGRACustomBitmap.PutImageSubpixel(x, y: single; Source: TBGRACustomBitmap);
begin
  PutImageAngle(x,y,source,0);
end;

procedure TBGRACustomBitmap.PutImagePart(x, y: integer;
  Source: TBGRACustomBitmap; SourceRect: TRect; mode: TDrawMode; AOpacity: byte);
var w,h,sourcex,sourcey,nx,ny,xb,yb,destx,desty: integer;
    oldClip,newClip: TRect;
begin
  if (Source = nil) or (AOpacity = 0) then exit;
  w := SourceRect.Right-SourceRect.Left;
  h := SourceRect.Bottom-SourceRect.Top;
  if (w <= 0) or (h <= 0) or (Source.Width = 0) or (Source.Height = 0) then exit;
  sourcex := PositiveMod(SourceRect.Left, Source.Width);
  sourcey := PositiveMod(SourceRect.Top, Source.Height);
  nx := (sourceX+w + Source.Width-1) div Source.Width;
  ny := (sourceY+h + Source.Height-1) div Source.Height;

  oldClip := ClipRect;
  newClip := rect(x,y,x+w,y+h);
  if not IntersectRect(newClip,newClip,oldClip) then exit;

  ClipRect := newClip;

  desty := y-sourcey;
  for yb := 0 to ny-1 do
  begin
    destx := x-sourcex;
    for xb := 0 to nx-1 do
    begin
      self.PutImage(destx,desty,Source,mode,AOpacity);
      inc(destx,Source.Width);
    end;
    inc(desty,Source.Height);
  end;

  ClipRect := oldClip;
end;

procedure TBGRACustomBitmap.PutImageAffine(Origin, HAxis, VAxis: TPointF;
  Source: TBGRACustomBitmap; AOpacity: Byte; ACorrectBlur: Boolean);
begin
  if ACorrectBlur then
    PutImageAffine(Origin,HAxis,VAxis,Source,rfCosine,AOpacity)
  else
    PutImageAffine(Origin,HAxis,VAxis,Source,rfLinear,AOpacity);
end;

procedure TBGRACustomBitmap.PutImageAffine(Origin, HAxis, VAxis: TPointF;
  Source: TBGRACustomBitmap; AResampleFilter: TResampleFilter; AOpacity: Byte);
var outputBounds: TRect;
begin
  if (Source = nil) or (AOpacity = 0) then exit;
  if (abs(Origin.x-round(Origin.x))<1e-6) and (abs(Origin.y-round(Origin.Y))<1e-6) and
     (abs(HAxis.x-(Origin.x+Source.Width))<1e-6) and (abs(HAxis.y-origin.y)<1e-6) and
     (abs(VAxis.x-Origin.x)<1e-6) and (abs(VAxis.y-(Origin.y+Source.Height))<1e-6) then
  begin
    PutImage(round(origin.x),round(origin.y),Source,dmDrawWithTransparency,AOpacity);
    exit;
  end;
  outputBounds := GetImageAffineBounds(Origin,HAxis,VAxis,Source);
  PutImageAffine(Origin,HAxis,VAxis,Source,outputBounds,AResampleFilter,dmDrawWithTransparency,AOpacity);
end;

procedure TBGRACustomBitmap.PutImageAffine(Origin, HAxis, VAxis: TPointF;
  Source: TBGRACustomBitmap; AOutputBounds: TRect; AOpacity: Byte;
  ACorrectBlur: Boolean);
begin
  if ACorrectBlur then
    PutImageAffine(Origin,HAxis,VAxis,Source,AOutputBounds,rfCosine,dmDrawWithTransparency, AOpacity)
  else
    PutImageAffine(Origin,HAxis,VAxis,Source,AOutputBounds,rfLinear,dmDrawWithTransparency,AOpacity);
end;

{ Returns the area that contains the affine transformed image }
function TBGRACustomBitmap.GetImageAffineBounds(Origin, HAxis, VAxis: TPointF;
  Source: TBGRACustomBitmap): TRect;
var minx,miny,maxx,maxy: integer;
    vx,vy,pt1: TPointF;
    sourceBounds: TRect;

  //include specified point in the bounds
  procedure Include(pt: TPointF);
  begin
    if floor(pt.X) < minx then minx := floor(pt.X);
    if floor(pt.Y) < miny then miny := floor(pt.Y);
    if ceil(pt.X) > maxx then maxx := ceil(pt.X);
    if ceil(pt.Y) > maxy then maxy := ceil(pt.Y);
  end;

begin
  result := EmptyRect;
  if (Source = nil) then exit;
  sourceBounds := source.GetImageBounds;
  if IsRectEmpty(sourceBounds) then exit;

  if (abs(Origin.x-round(Origin.x))<1e-6) and (abs(Origin.y-round(Origin.Y))<1e-6) and
     (abs(HAxis.x-(Origin.x+Source.Width))<1e-6) and (abs(HAxis.y-origin.y)<1e-6) and
     (abs(VAxis.x-Origin.x)<1e-6) and (abs(VAxis.y-(Origin.y+Source.Height))<1e-6) then
  begin
    result := sourceBounds;
    OffsetRect(result,round(origin.x),round(origin.y));
    IntersectRect(result,result,ClipRect);
    exit;
  end;

  { Compute bounds }
  vx := (HAxis-Origin)*(1/source.Width);
  vy := (VAxis-Origin)*(1/source.Height);
  pt1 := Origin+vx*sourceBounds.Left+vy*sourceBounds.Top;
  minx := floor(pt1.X);
  miny := floor(pt1.Y);
  maxx := ceil(pt1.X);
  maxy := ceil(pt1.Y);
  Include(Origin+vx*sourceBounds.Right+vy*sourceBounds.Top);
  Include(Origin+vx*sourceBounds.Right+vy*sourceBounds.Bottom);
  Include(Origin+vx*sourceBounds.Left+vy*sourceBounds.Bottom);

  result := rect(minx,miny,maxx+1,maxy+1);
  IntersectRect(result,result,ClipRect);
end;

procedure TBGRACustomBitmap.PutImageAngle(x, y: single;
  Source: TBGRACustomBitmap; angle: single; AOutputBounds: TRect;
  imageCenterX: single; imageCenterY: single; AOpacity: Byte;
  ARestoreOffsetAfterRotation: boolean; ACorrectBlur: Boolean);
begin
  if ACorrectBlur then
    PutImageAngle(x,y,Source,angle,AOutputBounds,rfCosine,imageCenterX,imageCenterY,AOpacity,ARestoreOffsetAfterRotation)
  else
    PutImageAngle(x,y,Source,angle,AOutputBounds,rfLinear,imageCenterX,imageCenterY,AOpacity,ARestoreOffsetAfterRotation);
end;

procedure TBGRACustomBitmap.PutImageAngle(x, y: single;
  Source: TBGRACustomBitmap; angle: single; imageCenterX: single;
  imageCenterY: single; AOpacity: Byte; ARestoreOffsetAfterRotation: boolean; ACorrectBlur: Boolean);
begin
  if ACorrectBlur then
    PutImageAngle(x,y,Source,angle,rfCosine,imageCenterX,imageCenterY,AOpacity,ARestoreOffsetAfterRotation)
  else
    PutImageAngle(x,y,Source,angle,rfLinear,imageCenterX,imageCenterY,AOpacity,ARestoreOffsetAfterRotation);
end;

procedure TBGRACustomBitmap.PutImageAngle(x, y: single;
  Source: TBGRACustomBitmap; angle: single; AOutputBounds: TRect;
  AResampleFilter: TResampleFilter; imageCenterX: single; imageCenterY: single; AOpacity: Byte;
  ARestoreOffsetAfterRotation: boolean);
var
  Origin,HAxis,VAxis: TPointF;
begin
  if (source = nil) or (AOpacity=0) then exit;
  ComputeImageAngleAxes(x,y,source.Width,source.Height,angle,imageCenterX,imageCenterY,ARestoreOffsetAfterRotation,
     Origin,HAxis,VAxis);
  PutImageAffine(Origin,HAxis,VAxis,source,AOutputBounds,AResampleFilter,dmDrawWithTransparency,AOpacity);
end;

procedure TBGRACustomBitmap.PutImageAngle(x, y: single;
  Source: TBGRACustomBitmap; angle: single; AResampleFilter: TResampleFilter;
  imageCenterX: single; imageCenterY: single; AOpacity: Byte;
  ARestoreOffsetAfterRotation: boolean);
var
  Origin,HAxis,VAxis: TPointF;
begin
  if (source = nil) or (AOpacity=0) then exit;
  ComputeImageAngleAxes(x,y,source.Width,source.Height,angle,imageCenterX,imageCenterY,ARestoreOffsetAfterRotation,
     Origin,HAxis,VAxis);
  PutImageAffine(Origin,HAxis,VAxis,source,AResampleFilter,AOpacity);
end;

procedure TBGRACustomBitmap.ComputeImageAngleAxes(x, y, w, h,
  angle: single; imageCenterX, imageCenterY: single;
  ARestoreOffsetAfterRotation: boolean; out Origin, HAxis, VAxis: TPointF);
var
  cosa,sina: single;

  { Compute rotated coordinates }
  function Coord(relX,relY: single): TPointF;
  begin
    relX -= imageCenterX;
    relY -= imageCenterY;
    result.x := relX*cosa-relY*sina+x;
    result.y := relY*cosa+relX*sina+y;
    if ARestoreOffsetAfterRotation then
    begin
      result.x += imageCenterX;
      result.y += imageCenterY;
    end;
  end;

begin
  cosa := cos(-angle*Pi/180);
  sina := -sin(-angle*Pi/180);
  Origin := Coord(0,0);
  HAxis := Coord(w,0);
  VAxis := Coord(0,h);
end;

function TBGRACustomBitmap.GetImageAngleBounds(x, y: single;
  Source: TBGRACustomBitmap; angle: single; imageCenterX: single;
  imageCenterY: single; ARestoreOffsetAfterRotation: boolean): TRect;
var
  cosa,sina: single;

  { Compute rotated coordinates }
  function Coord(relX,relY: single): TPointF;
  begin
    relX -= imageCenterX;
    relY -= imageCenterY;
    result.x := relX*cosa-relY*sina+x;
    result.y := relY*cosa+relX*sina+y;
    if ARestoreOffsetAfterRotation then
    begin
      result.x += imageCenterX;
      result.y += imageCenterY;
    end;
  end;

begin
  if (source = nil) then
  begin
    result := EmptyRect;
    exit;
  end;
  cosa := cos(-angle*Pi/180);
  sina := -sin(-angle*Pi/180);
  result := GetImageAffineBounds(Coord(0,0),Coord(source.Width,0),Coord(0,source.Height),source);
end;

procedure TBGRACustomBitmap.VerticalFlip;
begin
  VerticalFlip(rect(0,0,Width,Height));
end;

procedure TBGRACustomBitmap.HorizontalFlip;
begin
  HorizontalFlip(rect(0,0,Width,Height));
end;

procedure TBGRACustomBitmap.ApplyMask(mask: TBGRACustomBitmap);
begin
  ApplyMask(mask, Rect(0,0,Width,Height), Point(0,0));
end;

procedure TBGRACustomBitmap.ApplyMask(mask: TBGRACustomBitmap; ARect: TRect);
begin
  ApplyMask(mask, ARect, ARect.TopLeft);
end;

{ Interface gateway }
function TBGRACustomBitmap.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
begin
  if GetInterface(iid, obj) then
    Result := S_OK
  else
    Result := longint(E_NOINTERFACE);
end;

{ There is no automatic reference counting, but it is compulsory to define these functions }
function TBGRACustomBitmap._AddRef: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
begin
  result := 0;
end;

function TBGRACustomBitmap._Release: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
begin
  result := 0;
end;

{$hints off}
procedure TBGRACustomBitmap.ScanPutPixels(pdest: PBGRAPixel; count: integer;
  mode: TDrawMode);
begin
  //do nothing
end;
{$hints on}

function TBGRACustomBitmap.IsScanPutPixelsDefined: boolean;
begin
  result := False;
end;

{********************** End of TBGRACustomBitmap **************************}

{ TBGRACustomScanner }
{ The abstract class record the position so that a derived class
  need only to redefine ScanAt }

function TBGRACustomScanner.ScanAtInteger(X, Y: integer): TBGRAPixel;
begin
  result := ScanAt(X,Y);
end;

procedure TBGRACustomScanner.ScanMoveTo(X, Y: Integer);
begin
  FCurX := X;
  FCurY := Y;
end;

{ Call ScanAt to determine pixel value }
function TBGRACustomScanner.ScanNextPixel: TBGRAPixel;
begin
  result := ScanAt(FCurX,FCurY);
  Inc(FCurX);
end;

{$hints off}
procedure TBGRACustomScanner.ScanPutPixels(pdest: PBGRAPixel; count: integer;
  mode: TDrawMode);
begin
  //do nothing
end;
{$hints on}

function TBGRACustomScanner.IsScanPutPixelsDefined: boolean;
begin
  result := false;
end;

{ Interface gateway }
function TBGRACustomScanner.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
begin
  if GetInterface(iid, obj) then
    Result := S_OK
  else
    Result := longint(E_NOINTERFACE);
end;

{ There is no automatic reference counting, but it is compulsory to define these functions }
function TBGRACustomScanner._AddRef: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
begin
  result := 0;
end;

function TBGRACustomScanner._Release: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
begin
  result := 0;
end;

{********************** End of TBGRACustomScanner **************************}

{ The gamma correction is approximated here by a power function }
const
  GammaExpFactor   = 1.7; //exponent
  redWeightShl10   = 306; // = 0.299
  greenWeightShl10 = 601; // = 0.587
  blueWeightShl10  = 117; // = 0.114

var
  GammaLinearFactor: single;

procedure InitGamma;
var
  i: integer;
{$IFDEF WINCE}
  j,prevpos,curpos,midpos: integer;
{$ENDIF}
begin
  //the linear factor is used to normalize expanded values in the range 0..65535
  GammaLinearFactor := 65535 / power(255, GammaExpFactor);

{$IFDEF WINCE}
  curpos := 0;
  GammaExpansionTab[0] := 0;
  GammaCompressionTab[0] := 0;
  for i := 0 to 255 do
  begin
    prevpos := curpos;
    curpos := round(power(i, GammaExpFactor) * GammaLinearFactor);
    if i = 1 then curpos := 1; //to avoid information loss
    GammaExpansionTab[i] := curpos;
    midpos := (prevpos+1+curpos) div 2;
    for j := prevpos+1 to midpos-1 do
      GammaCompressionTab[j] := i-1;
    for j := midpos to curpos do
      GammaCompressionTab[j] := i;
  end;
{$ELSE}
  for i := 0 to 255 do
    GammaExpansionTab[i] := round(power(i, GammaExpFactor) * GammaLinearFactor);

  for i := 0 to 65535 do
    GammaCompressionTab[i] := round(power(i / GammaLinearFactor, 1 / GammaExpFactor));

  GammaExpansionTab[1]   := 1; //to avoid information loss
  GammaCompressionTab[1] := 1;
{$ENDIF}
end;

{************************** Color functions **************************}

function CheckPutImageBounds(x, y, tx, ty: integer; out minxb, minyb, maxxb,
  maxyb, ignoreleft: integer; const cliprect: TRect): boolean;
var x2,y2: integer;
begin
  if (x >= cliprect.Right) or (y >= cliprect.Bottom) or (x <= cliprect.Left-tx) or
    (y <= cliprect.Top-ty) or (ty <= 0) or (tx <= 0) then
  begin
    result := false;
    exit;
  end;

  x2 := x + tx - 1;
  y2 := y + ty - 1;

  if y < cliprect.Top then
    minyb := cliprect.Top
  else
    minyb := y;
  if y2 >= cliprect.Bottom then
    maxyb := cliprect.Bottom - 1
  else
    maxyb := y2;

  if x < cliprect.Left then
  begin
    ignoreleft := cliprect.Left-x;
    minxb      := cliprect.Left;
  end
  else
  begin
    ignoreleft := 0;
    minxb      := x;
  end;
  if x2 >= cliprect.Right then
    maxxb := cliprect.Right - 1
  else
    maxxb := x2;

  result := true;
end;

{ The intensity is defined here as the maximum value of any color component }
function GetIntensity(c: TExpandedPixel): word; inline;
begin
  Result := c.red;
  if c.green > Result then
    Result := c.green;
  if c.blue > Result then
    Result := c.blue;
end;

function SetIntensity(c: TExpandedPixel; intensity: word): TExpandedPixel;
var
  curIntensity: word;
begin
  curIntensity := GetIntensity(c);
  if curIntensity = 0 then //suppose it's gray if there is no color information
    Result := c
  else
  begin
    //linear interpolation to reached wanted intensity
    Result.red   := (c.red * intensity + (curIntensity shr 1)) div curIntensity;
    Result.green := (c.green * intensity + (curIntensity shr 1)) div curIntensity;
    Result.blue  := (c.blue * intensity + (curIntensity shr 1)) div curIntensity;
    Result.alpha := c.alpha;
  end;
end;

{ The lightness here is defined as the subjective sensation of luminosity, where
  blue is the darkest component and green the lightest }
function GetLightness(const c: TExpandedPixel): word; inline;
begin
  Result := (c.red * redWeightShl10 + c.green * greenWeightShl10 +
    c.blue * blueWeightShl10 + 512) shr 10;
end;

function SetLightness(c: TExpandedPixel; lightness: word): TExpandedPixel;
var
  curLightness: word;
begin
  curLightness := GetLightness(c);
  if lightness = curLightness then
  begin //no change
    Result := c;
    exit;
  end;
  result := SetLightness(c, lightness, curLightness);
end;

function SetLightness(c: TExpandedPixel; lightness: word; curLightness: word): TExpandedPixel;
var
  AddedWhiteness, maxBeforeWhite: word;
  clip: boolean;
begin
  if lightness = curLightness then
  begin //no change
    Result := c;
    exit;
  end;
  if lightness = 65535 then //set to white
  begin
    Result.red   := 65535;
    Result.green := 65535;
    Result.blue  := 65535;
    Result.alpha := c.alpha;
    exit;
  end;
  if lightness = 0 then  //set to black
  begin
    Result.red   := 0;
    Result.green := 0;
    Result.blue  := 0;
    Result.alpha := c.alpha;
    exit;
  end;
  if curLightness = 0 then  //set from black
  begin
    Result.red   := lightness;
    Result.green := lightness;
    Result.blue  := lightness;
    Result.alpha := c.alpha;
    exit;
  end;
  if lightness < curLightness then //darker is easy
  begin
    result.alpha:= c.alpha;
    result.red := (c.red * lightness + (curLightness shr 1)) div curLightness;
    result.green := (c.green * lightness + (curLightness shr 1)) div curLightness;
    result.blue := (c.blue * lightness + (curLightness shr 1)) div curLightness;
    exit;
  end;
  //lighter and grayer
  Result := c;
  AddedWhiteness := lightness - curLightness;
  maxBeforeWhite := 65535 - AddedWhiteness;
  clip   := False;
  if Result.red <= maxBeforeWhite then
    Inc(Result.red, AddedWhiteness)
  else
  begin
    Result.red := 65535;
    clip := True;
  end;
  if Result.green <= maxBeforeWhite then
    Inc(Result.green, AddedWhiteness)
  else
  begin
    Result.green := 65535;
    clip := True;
  end;
  if Result.blue <= maxBeforeWhite then
    Inc(Result.blue, AddedWhiteness)
  else
  begin
    Result.blue := 65535;
    clip := True;
  end;

  if clip then //light and whiter
  begin
    curLightness   := GetLightness(Result);
    addedWhiteness := lightness - curLightness;
    maxBeforeWhite := 65535 - curlightness;
    Result.red     := Result.red + addedWhiteness * (65535 - Result.red) div
      maxBeforeWhite;
    Result.green   := Result.green + addedWhiteness * (65535 - Result.green) div
      maxBeforeWhite;
    Result.blue    := Result.blue + addedWhiteness * (65535 - Result.blue) div
      maxBeforeWhite;
  end;
end;

function ApplyLightnessFast(color: TBGRAPixel; lightness: word): TBGRAPixel;
var
  r,g,b: word;
  lightness256: byte;
begin
  if lightness <= 32768 then
  begin
    if lightness = 32768 then
      result := color else
    begin
      lightness256 := GammaCompressionTab[lightness shl 1];
      result := BGRA(color.red * lightness256 shr 8, color.green*lightness256 shr 8,
                     color.blue * lightness256 shr 8, color.alpha);
    end;
  end else
  begin
    if lightness = 65535 then
      result := BGRA(255,255,255,color.alpha) else
    begin
      lightness -= 32767;
      r := GammaExpansionTab[color.red];
      g := GammaExpansionTab[color.green];
      b := GammaExpansionTab[color.blue];
      result := BGRA(GammaCompressionTab[ r + (not r)*lightness shr 15 ],
                     GammaCompressionTab[ g + (not g)*lightness shr 15 ],
                     GammaCompressionTab[ b + (not b)*lightness shr 15 ],
                     color.alpha);
    end;
  end;
end;

function CombineLightness(lightness1,lightness2: Int32or64): Int32or64;
{$ifdef CPUI386} {$asmmode intel} assembler;
  asm
    imul edx
    shl edx, 17
    shr eax, 15
    or edx, eax
    mov result, edx
  end;
{$ELSE}
begin
  result := int64(lightness1)*lightness2 shr 15;
end;
{$ENDIF}

function ApplyIntensityFast(color: TBGRAPixel; lightness: longword): TBGRAPixel;
var
    maxValue,invMaxValue,r,g,b: longword;
    lightness256: byte;
begin
  if lightness <= 32768 then
  begin
    if lightness = 32768 then
      result := color else
    begin
      lightness256 := GammaCompressionTab[lightness shl 1];
      result := BGRA(color.red * lightness256 shr 8, color.green*lightness256 shr 8,
                     color.blue * lightness256 shr 8, color.alpha);
    end;
  end else
  begin
    r := CombineLightness(GammaExpansionTab[color.red], lightness);
    g := CombineLightness(GammaExpansionTab[color.green], lightness);
    b := CombineLightness(GammaExpansionTab[color.blue], lightness);
    maxValue := r;
    if g > maxValue then maxValue := g;
    if b > maxValue then maxValue := b;
    if maxValue <= 65535 then
      result := BGRA(GammaCompressionTab[r],
                     GammaCompressionTab[g],
                     GammaCompressionTab[b],
                     color.alpha)
    else
    begin
      invMaxValue := (longword(2147483647)+longword(maxValue-1)) div maxValue;
      maxValue := (maxValue-65535) shr 1;
      r := r*invMaxValue shr 15 + maxValue;
      g := g*invMaxValue shr 15 + maxValue;
      b := b*invMaxValue shr 15 + maxValue;
      if r >= 65535 then result.red := 255 else
        result.red := GammaCompressionTab[r];
      if g >= 65535 then result.green := 255 else
        result.green := GammaCompressionTab[g];
      if b >= 65535 then result.blue := 255 else
        result.blue := GammaCompressionTab[b];
      result.alpha := color.alpha;
    end;
  end;
end;

{ Conversion from RGB value to HSL colorspace. See : http://en.wikipedia.org/wiki/HSL_color_space }
function BGRAToHSLA(c: TBGRAPixel): THSLAPixel;
begin
  result := ExpandedToHSLA(GammaExpansion(c));
end;

procedure ExpandedToHSLAInline(r,g,b: Int32Or64; var dest: THSLAPixel); inline;
const
  deg60  = 10922;
  deg120 = 21845;
  deg240 = 43690;
var
  min, max, minMax: Int32or64;
  UMinMax,UTwiceLightness: UInt32or64;
begin
  if g > r then
  begin
    max := g;
    min := r;
  end
  else
  begin
    max := r;
    min := g;
  end;
  if b > max then
    max := b
  else
  if b < min then
    min  := b;
  minMax := max - min;

  if minMax = 0 then
    dest.hue := 0
  else
  if max = r then
    {$PUSH}{$RANGECHECKS OFF}
    dest.hue := ((g - b) * deg60) div minMax
    {$POP}
  else
  if max = g then
    dest.hue := ((b - r) * deg60) div minMax + deg120
  else
    {max = b} dest.hue := ((r - g) * deg60) div minMax + deg240;
  UTwiceLightness := max + min;
  if min = max then
    dest.saturation := 0
  else
  begin
    UMinMax:= minMax;
    if UTwiceLightness < 65536 then
      dest.saturation := (UMinMax shl 16) div (UTwiceLightness + 1)
    else
      dest.saturation := (UMinMax shl 16) div (131072 - UTwiceLightness);
  end;
  dest.lightness := UTwiceLightness shr 1;
end;

function ExpandedToHSLA(const ec: TExpandedPixel): THSLAPixel;
begin
  result.alpha := ec.alpha;
  ExpandedToHSLAInline(ec.red,ec.green,ec.blue,result);
end;

function HtoG(hue: word): word;
const
  segmentDest: array[0..5] of NativeUInt =
     (13653, 10923, 8192, 13653, 10923, 8192);
  segmentSrc: array[0..5] of NativeUInt =
     (10923, 10922, 10923, 10923, 10922, 10923);
var
  h,g: NativeUInt;
begin
  h := hue;
  if h < segmentSrc[0] then
    g := h * segmentDest[0] div segmentSrc[0]
  else
  begin
    g := segmentDest[0];
    h -= segmentSrc[0];
    if h < segmentSrc[1] then
      g += h * segmentDest[1] div segmentSrc[1]
    else
    begin
      g += segmentDest[1];
      h -= segmentSrc[1];
      if h < segmentSrc[2] then
        g += h * segmentDest[2] div segmentSrc[2]
      else
      begin
        g += segmentDest[2];
        h -= segmentSrc[2];
        if h < segmentSrc[3] then
          g += h * segmentDest[3] div segmentSrc[3]
        else
        begin
          g += segmentDest[3];
          h -= segmentSrc[3];
          if h < segmentSrc[4] then
            g += h * segmentDest[4] div segmentSrc[4]
          else
          begin
            g += segmentDest[4];
            h -= segmentSrc[4];
            g += h * segmentDest[5] div segmentSrc[5];
          end;
        end;
      end;
    end;
  end;
  result := g;
end;

function GtoH(ghue: word): word;
const
  segment: array[0..5] of NativeUInt =
     (13653, 10923, 8192, 13653, 10923, 8192);
var g: NativeUint;
begin
  g := ghue;
  if g < segment[0] then
    result := g * 10923 div segment[0]
  else
  begin
    g -= segment[0];
    if g < segment[1] then
      result := g * (21845-10923) div segment[1] + 10923
    else
    begin
      g -= segment[1];
      if g < segment[2] then
        result := g * (32768-21845) div segment[2] + 21845
      else
      begin
        g -= segment[2];
        if g < segment[3] then
          result := g * (43691-32768) div segment[3] + 32768
        else
        begin
          g -= segment[3];
          if g < segment[4] then
            result := g * (54613-43691) div segment[4] + 43691
          else
          begin
            g -= segment[4];
            result := g * (65536-54613) div segment[5] + 54613;
          end;
        end;
      end;
    end;
  end;
end;

function BGRAToGSBA(c: TBGRAPixel): THSLAPixel;
var lightness: UInt32Or64;
    red,green,blue: Int32or64;
begin
  red   := GammaExpansionTab[c.red];
  green := GammaExpansionTab[c.green];
  blue  := GammaExpansionTab[c.blue];
  result.alpha := c.alpha shl 8 + c.alpha;

  lightness := (red * redWeightShl10 + green * greenWeightShl10 +
    blue * blueWeightShl10 + 512) shr 10;

  ExpandedToHSLAInline(red,green,blue,result);
  if result.lightness > 32768 then
    result.saturation := result.saturation* UInt32or64(not result.lightness) div 32767;
  result.lightness := lightness;
  result.hue := HtoG(result.hue);
end;

function HSLAToExpanded(const c: THSLAPixel): TExpandedPixel;
const
  deg30  = 4096;
  deg60  = 8192;
  deg120 = deg60 * 2;
  deg180 = deg60 * 3;
  deg240 = deg60 * 4;
  deg360 = deg60 * 6;

  function ComputeColor(p, q: Int32or64; h: Int32or64): Int32or64; inline;
  begin
    if h < deg180 then
    begin
      if h < deg60 then
        Result := p + ((q - p) * h + deg30) div deg60
      else
        Result := q
    end else
    begin
      if h < deg240 then
        Result := p + ((q - p) * (deg240 - h) + deg30) div deg60
      else
        Result := p;
    end;
  end;

var
  q, p, L, S, H: Int32or64;
begin
  L := c.lightness;
  S := c.saturation;
  if S = 0 then  //gray
  begin
    result.red   := L;
    result.green := L;
    result.blue  := L;
    result.alpha := c.alpha;
    exit;
  end;
  {$hints off}
  if L < 32768 then
    q := (L shr 1) * ((65535 + S) shr 1) shr 14
  else
    q := L + S - ((L shr 1) *
      (S shr 1) shr 14);
  {$hints on}
  if q > 65535 then q := 65535;
  p   := (L shl 1) - q;
  if p > 65535 then p := 65535;
  H := c.hue * deg360 shr 16;
  result.green := ComputeColor(p, q, H);
  inc(H, deg120);
  if H > deg360 then Dec(H, deg360);
  result.red   := ComputeColor(p, q, H);
  inc(H, deg120);
  if H > deg360 then Dec(H, deg360);
  result.blue  := ComputeColor(p, q, H);
  result.alpha := c.alpha;
end;

{ Conversion from HSL colorspace to RGB. See : http://en.wikipedia.org/wiki/HSL_color_space }
function HSLAToBGRA(const c: THSLAPixel): TBGRAPixel;
var ec: TExpandedPixel;
begin
  ec := HSLAToExpanded(c);
  Result := GammaCompression(ec);
end;

function HueDiff(h1, h2: word): word;
begin
  result := abs(integer(h1)-integer(h2));
  if result > 32768 then result := 65536-result;
end;

function GetHue(ec: TExpandedPixel): word;
const
  deg60  = 8192;
  deg120 = deg60 * 2;
  deg240 = deg60 * 4;
  deg360 = deg60 * 6;
var
  min, max, minMax: integer;
  r,g,b: integer;
begin
  r := ec.red;
  g := ec.green;
  b := ec.blue;
  min := r;
  max := r;
  if g > max then
    max := g
  else
  if g < min then
    min := g;
  if b > max then
    max := b
  else
  if b < min then
    min  := b;
  minMax := max - min;

  if minMax = 0 then
    Result := 0
  else
  if max = r then
    Result := (((g - b) * deg60) div
      minMax + deg360) mod deg360
  else
  if max = g then
    Result := ((b - r) * deg60) div minMax + deg120
  else
    {max = b} Result :=
      ((r - g) * deg60) div minMax + deg240;

  Result   := (Result shl 16) div deg360; //normalize
end;

function ColorImportance(ec: TExpandedPixel): word;
var min,max: word;
begin
  min := ec.red;
  max := ec.red;
  if ec.green > max then
    max := ec.green
  else
  if ec.green < min then
    min := ec.green;
  if ec.blue > max then
    max := ec.blue
  else
  if ec.blue < min then
    min  := ec.blue;
  result := max - min;
end;

function GSBAToBGRA(c: THSLAPixel): TBGRAPixel;
var ec: TExpandedPixel;
    lightness: word;
begin
  c.hue := GtoH(c.hue);
  lightness := c.lightness;
  c.lightness := 32768;
  ec := HSLAToExpanded(c);
  result := GammaCompression(SetLightness(ec, lightness));
end;

function GSBAToHSLA(c: THSLAPixel): THSLAPixel;
begin
  result := BGRAToHSLA(GSBAToBGRA(c));
end;

{ Apply gamma correction using conversion tables }
function GammaExpansion(c: TBGRAPixel): TExpandedPixel;
begin
  Result.red   := GammaExpansionTab[c.red];
  Result.green := GammaExpansionTab[c.green];
  Result.blue  := GammaExpansionTab[c.blue];
  Result.alpha := c.alpha shl 8 + c.alpha;
end;

function GammaCompression(const ec: TExpandedPixel): TBGRAPixel;
begin
  Result.red   := GammaCompressionTab[ec.red];
  Result.green := GammaCompressionTab[ec.green];
  Result.blue  := GammaCompressionTab[ec.blue];
  Result.alpha := ec.alpha shr 8;
end;

function GammaCompression(red, green, blue, alpha: word): TBGRAPixel;
begin
  Result.red   := GammaCompressionTab[red];
  Result.green := GammaCompressionTab[green];
  Result.blue  := GammaCompressionTab[blue];
  Result.alpha := alpha shr 8;
end;

// Conversion to grayscale by taking into account
// different color weights
function BGRAToGrayscale(c: TBGRAPixel): TBGRAPixel;
var
  ec:    TExpandedPixel;
  gray:  word;
  cgray: byte;
begin
  if c.alpha = 0 then
  begin
    result := BGRAPixelTransparent;
    exit;
  end;
  //gamma expansion
  ec    := GammaExpansion(c);
  //gray composition
  gray  := (ec.red * redWeightShl10 + ec.green * greenWeightShl10 +
    ec.blue * blueWeightShl10 + 512) shr 10;
  //gamma compression
  cgray := GammaCompressionTab[gray];
  Result.red := cgray;
  Result.green := cgray;
  Result.blue := cgray;
  Result.alpha := c.alpha;
end;

function GrayscaleToBGRA(lightness: word): TBGRAPixel;
begin
  result.red := GammaCompressionTab[lightness];
  result.green := result.red;
  result.blue := result.red;
  result.alpha := $ff;
end;

function MergeBGRA(const colors: array of TBGRAPixel): TBGRAPixel;
var
  sumR,sumG,sumB,sumA: longword;
  i: integer;
begin
  if length(colors)<=0 then
  begin
    result := BGRAPixelTransparent;
    exit;
  end;
  sumR := 0;
  sumG := 0;
  sumB := 0;
  sumA := 0;
  for i := 0 to high(colors) do
  with colors[i] do
  begin
    sumR += red*alpha;
    sumG += green*alpha;
    sumB += blue*alpha;
    sumA += alpha;
  end;
  if sumA > 0 then
  begin
    result.red := (sumR + sumA shr 1) div sumA;
    result.green := (sumG + sumA shr 1) div sumA;
    result.blue := (sumB + sumA shr 1) div sumA;
    result.alpha := sumA div longword(length(colors));
  end
  else
    result := BGRAPixelTransparent;
end;

{ Merge linearly two colors of same importance }
function MergeBGRA(c1, c2: TBGRAPixel): TBGRAPixel;
var c12: cardinal;
begin
  if (c1.alpha = 0) then
    Result := c2
  else
  if (c2.alpha = 0) then
    Result := c1
  else
  begin
    c12 := c1.alpha + c2.alpha;
    Result.red   := (c1.red * c1.alpha + c2.red * c2.alpha + c12 shr 1) div c12;
    Result.green := (c1.green * c1.alpha + c2.green * c2.alpha + c12 shr 1) div c12;
    Result.blue  := (c1.blue * c1.alpha + c2.blue * c2.alpha + c12 shr 1) div c12;
    Result.alpha := (c12 + 1) shr 1;
  end;
end;

function MergeBGRA(c1: TBGRAPixel; weight1: integer; c2: TBGRAPixel;
  weight2: integer): TBGRAPixel;
var
    f1,f2,f12: int64;
begin
  if (weight1 = 0) then
  begin
    if (weight2 = 0) then
      result := BGRAPixelTransparent
    else
      Result := c2
  end
  else
  if (weight2 = 0) then
    Result := c1
  else
  if (weight1+weight2 = 0) then
    Result := BGRAPixelTransparent
  else
  begin
    f1 := int64(c1.alpha)*weight1;
    f2 := int64(c2.alpha)*weight2;
    f12 := f1+f2;
    if f12 = 0 then
      result := BGRAPixelTransparent
    else
    begin
      Result.red   := (c1.red * f1 + c2.red * f2 + f12 shr 1) div f12;
      Result.green := (c1.green * f1 + c2.green * f2 + f12 shr 1) div f12;
      Result.blue  := (c1.blue * f1 + c2.blue * f2 + f12 shr 1) div f12;
      {$hints off}
      Result.alpha := (f12 + ((weight1+weight2) shr 1)) div (weight1+weight2);
      {$hints on}
    end;
  end;
end;

function MergeBGRAWithGammaCorrection(c1: TBGRAPixel; weight1: byte; c2: TBGRAPixel;
  weight2: byte): TBGRAPixel;
var
    w1,w2,f1,f2,f12,a: UInt32or64;
begin
  w1 := weight1;
  w2 := weight2;
  if (w1 = 0) then
  begin
    if (w2 = 0) then
      result := BGRAPixelTransparent
    else
      Result := c2
  end
  else
  if (w2 = 0) then
    Result := c1
  else
  begin
    f1 := c1.alpha*w1;
    f2 := c2.alpha*w2;
    a := (f1+f2 + ((w1+w2) shr 1)) div (w1+w2);
    if a = 0 then
    begin
      result := BGRAPixelTransparent;
      exit;
    end else
      Result.alpha := a;
    {$IFNDEF CPU64}
    if (f1 >= 32768) or (f2 >= 32768) then
    begin
      f1 := f1 shr 1;
      f2 := f2 shr 1;
    end;
    {$ENDIF}
    f12 := f1+f2;
    Result.red   := GammaCompressionTab[(GammaExpansionTab[c1.red] * f1 + GammaExpansionTab[c2.red] * f2) div f12];
    Result.green := GammaCompressionTab[(GammaExpansionTab[c1.green] * f1 + GammaExpansionTab[c2.green] * f2) div f12];
    Result.blue  := GammaCompressionTab[(GammaExpansionTab[c1.blue] * f1 + GammaExpansionTab[c2.blue] * f2) div f12];
  end;
end;

{ Merge two colors of same importance }
function MergeBGRA(ec1, ec2: TExpandedPixel): TExpandedPixel;
var c12: cardinal;
begin
  if (ec1.alpha = 0) then
    Result := ec2
  else
  if (ec2.alpha = 0) then
    Result := ec1
  else
  begin
    c12 := ec1.alpha + ec2.alpha;
    Result.red   := (int64(ec1.red) * ec1.alpha + int64(ec2.red) * ec2.alpha + c12 shr 1) div c12;
    Result.green := (int64(ec1.green) * ec1.alpha + int64(ec2.green) * ec2.alpha + c12 shr 1) div c12;
    Result.blue  := (int64(ec1.blue) * ec1.alpha + int64(ec2.blue) * ec2.alpha + c12 shr 1) div c12;
    Result.alpha := (c12 + 1) shr 1;
  end;
end;

function BGRA(red, green, blue, alpha: byte): TBGRAPixel;
begin
  Result.red   := red;
  Result.green := green;
  Result.blue  := blue;
  Result.alpha := alpha;
end;

function BGRA(red, green, blue: byte): TBGRAPixel; overload;
begin
  Result.red   := red;
  Result.green := green;
  Result.blue  := blue;
  Result.alpha := 255;
end;

{ Convert a TColor value to a TBGRAPixel value. Note that
  you need to call ColorToRGB first if you use a system
  color identifier like clWindow. }
{$PUSH}{$R-}

function HSLA(hue, saturation, lightness, alpha: word): THSLAPixel;
begin
  Result.hue   := hue;
  Result.saturation := saturation;
  Result.lightness  := lightness;
  Result.alpha := alpha;
end;

function HSLA(hue, saturation, lightness: word): THSLAPixel;
begin
  Result.hue   := hue;
  Result.saturation := saturation;
  Result.lightness  := lightness;
  Result.alpha := $ffff;
end;

function ColorToBGRA(color: TColor): TBGRAPixel; overload;
begin
  Result.red   := color;
  Result.green := color shr 8;
  Result.blue  := color shr 16;
  Result.alpha := 255;
end;

function ColorToBGRA(color: TColor; opacity: byte): TBGRAPixel; overload;
begin
  Result.red   := color;
  Result.green := color shr 8;
  Result.blue  := color shr 16;
  Result.alpha := opacity;
end;
{$POP}

{ Conversion from TFPColor to TBGRAPixel assuming TFPColor
  is already gamma compressed }
function FPColorToBGRA(AValue: TFPColor): TBGRAPixel;
begin
  with AValue do
    Result := BGRA(red shr 8, green shr 8, blue shr 8, alpha shr 8);
end;

function BGRAToFPColor(AValue: TBGRAPixel): TFPColor; inline;
begin
  result.red := AValue.red shl 8 + AValue.red;
  result.green := AValue.green shl 8 + AValue.green;
  result.blue := AValue.blue shl 8 + AValue.blue;
  result.alpha := AValue.alpha shl 8 + AValue.alpha;
end;

function BGRAToColor(c: TBGRAPixel): TColor;
begin
  Result := c.red + (c.green shl 8) + (c.blue shl 16);
end;

operator = (const c1, c2: TBGRAPixel): boolean;
begin
  if (c1.alpha = 0) and (c2.alpha = 0) then
    Result := True
  else
    Result := (c1.alpha = c2.alpha) and (c1.red = c2.red) and
      (c1.green = c2.green) and (c1.blue = c2.blue);
end;

function LessStartSlope65535(value: word): word;
var factor: word;
begin
  factor := 4096 - (not value)*3 shr 7;
  result := value*factor shr 12;
end;

function ExpandedDiff(ec1, ec2: TExpandedPixel): word;
var
  CompRedAlpha1, CompGreenAlpha1, CompBlueAlpha1, CompRedAlpha2,
  CompGreenAlpha2, CompBlueAlpha2: integer;
  DiffAlpha: word;
  ColorDiff: word;
  TempHueDiff: word;
begin
  CompRedAlpha1 := ec1.red * ec1.alpha shr 16; //gives 0..65535
  CompGreenAlpha1 := ec1.green * ec1.alpha shr 16;
  CompBlueAlpha1 := ec1.blue * ec1.alpha shr 16;
  CompRedAlpha2 := ec2.red * ec2.alpha shr 16;
  CompGreenAlpha2 := ec2.green * ec2.alpha shr 16;
  CompBlueAlpha2 := ec2.blue * ec2.alpha shr 16;
  Result    := (Abs(CompRedAlpha2 - CompRedAlpha1)*redWeightShl10 +
    Abs(CompBlueAlpha2 - CompBlueAlpha1)*blueWeightShl10 +
    Abs(CompGreenAlpha2 - CompGreenAlpha1)*greenWeightShl10) shr 10;
  ColorDiff := min(ColorImportance(ec1),ColorImportance(ec2));
  if ColorDiff > 0 then
  begin
    TempHueDiff := HueDiff(HtoG(GetHue(ec1)),HtoG(GetHue(ec2)));
    if TempHueDiff < 32768 then
      TempHueDiff := LessStartSlope65535(TempHueDiff shl 1) shr 4
    else
      TempHueDiff := TempHueDiff shr 3;
    Result := ((Result shr 4)* (not ColorDiff) + TempHueDiff*ColorDiff) shr 12;
  end;
  DiffAlpha := Abs(integer(ec2.Alpha) - integer(ec1.Alpha));
  if DiffAlpha > Result then
    Result := DiffAlpha;
end;

function BGRAWordDiff(c1, c2: TBGRAPixel): word;
begin
  result := ExpandedDiff(GammaExpansion(c1),GammaExpansion(c2));
end;

function BGRADiff(c1,c2: TBGRAPixel): byte;
begin
  result := ExpandedDiff(GammaExpansion(c1),GammaExpansion(c2)) shr 8;
end;

operator-(const c1, c2: TColorF): TColorF;
begin
  result[1] := c1[1]-c2[1];
  result[2] := c1[2]-c2[2];
  result[3] := c1[3]-c2[3];
  result[4] := c1[4]-c2[4];
end;

operator+(const c1, c2: TColorF): TColorF;
begin
  result[1] := c1[1]+c2[1];
  result[2] := c1[2]+c2[2];
  result[3] := c1[3]+c2[3];
  result[4] := c1[4]+c2[4];
end;

operator*(const c1, c2: TColorF): TColorF;
begin
  result[1] := c1[1]*c2[1];
  result[2] := c1[2]*c2[2];
  result[3] := c1[3]*c2[3];
  result[4] := c1[4]*c2[4];
end;

operator*(const c1: TColorF; factor: single): TColorF;
begin
  result[1] := c1[1]*factor;
  result[2] := c1[2]*factor;
  result[3] := c1[3]*factor;
  result[4] := c1[4]*factor;
end;

function ColorF(red, green, blue, alpha: single): TColorF;
begin
  result[1] := red;
  result[2] := green;
  result[3] := blue;
  result[4] := alpha;
end;

{ Write a color in hexadecimal format RRGGBBAA or using the name in a color list }
function BGRAToStr(c: TBGRAPixel; AColorList: TBGRAColorList = nil; AMaxDiff: Word= 0): string;
var idx: integer;
begin
  if Assigned(AColorList) then
  begin
    idx := AColorList.IndexOfColor(c, AMaxDiff);
    if idx<> -1 then
    begin
      result := AColorList.Name[idx];
      exit;
    end;
  end;
  result := IntToHex(c.red,2)+IntToHex(c.green,2)+IntToHex(c.Blue,2)+IntToHex(c.Alpha,2);
end;

type
    arrayOfString = array of string;

function SimpleParseFuncParam(str: string; var flagError: boolean): arrayOfString;
var idxOpen,start,cur: integer;
begin
    result := nil;
    idxOpen := pos('(',str);
    if idxOpen = 0 then
    begin
      start := 1;
      //find first space
      while (start <= length(str)) and (str[start]<>' ') do inc(start);
    end else
      start := idxOpen+1;
    cur := start;
    while cur <= length(str) do
    begin
       if str[cur] in[',',')'] then
       begin
         setlength(result,length(result)+1);
         result[high(result)] := trim(copy(str,start,cur-start));
         start := cur+1;
         if str[cur] = ')' then exit;
       end;
       inc(cur);
    end;
    if idxOpen <> 0 then flagError := true; //should exit on ')'
    if start <= length(str) then
    begin
      setlength(result,length(result)+1);
      result[high(result)] := copy(str,start,length(str)-start+1);
    end;
end;

function ParseColorValue(str: string; var flagError: boolean): byte;
var pourcent,unclipped,{%H-}errPos: integer;
begin
  if str = '' then result := 0 else
  begin
    if str[length(str)]='%' then
    begin
      val(copy(str,1,length(str)-1),pourcent,errPos);
      if errPos <> 0 then flagError := true;
      if pourcent < 0 then result := 0 else
      if pourcent > 100 then result := 255 else
        result := pourcent*255 div 100;
    end else
    begin
      val(str,unclipped,errPos);
      if errPos <> 0 then flagError := true;
      if unclipped < 0 then result := 0 else
      if unclipped > 255 then result := 255 else
        result := unclipped;
    end;
  end;
end;

//this function returns the parsed value only if it contains no error nor missing values, otherwise
//it returns BGRAPixelTransparent
function StrToBGRA(str: string): TBGRAPixel;
var missingValues, error: boolean;
begin
  result := BGRABlack;
  TryStrToBGRA(str, result, missingValues, error);
  if missingValues or error then result := BGRAPixelTransparent;
end;

//this function changes the content of parsedValue depending on available and parsable information.
//set parsedValue to the fallback values before calling this function.
//missing values are expressed by empty string or by '?', for example 'rgb(255,?,?,?)' will change only the red value.
//note that if alpha is not expressed by the string format, it will be opaque. So 'rgb(255,?,?)' will change the red value and the alpha value.
//the last parameter of rgba() is a floating point number where 1 is opaque and 0 is transparent.
procedure TryStrToBGRA(str: string; var parsedValue: TBGRAPixel; out missingValues: boolean; out error: boolean);
var errPos: integer;
    values: array of string;
    alphaF: single;
    idx: integer;
begin
  str := Trim(str);
  error := false;
  if (str = '') or (str = '?') then
  begin
    missingValues := true;
    exit;
  end else
    missingValues := false;
  str := StringReplace(lowerCase(str),'grey','gray',[]);

  //VGA color names
  idx := VGAColors.IndexOf(str);
  if idx <> -1 then
  begin
    parsedValue := VGAColors[idx];
    exit;
  end;
  if str='transparent' then parsedValue := BGRAPixelTransparent else
  begin
    //check CSS color
    idx := CSSColors.IndexOf(str);
    if idx <> -1 then
    begin
      parsedValue := CSSColors[idx];
      exit;
    end;

    //CSS RGB notation
    if (copy(str,1,4)='rgb(') or (copy(str,1,5)='rgba(') or
      (copy(str,1,4)='rgb ') or (copy(str,1,5)='rgba ') then
    begin
      values := SimpleParseFuncParam(str,error);
      if (length(values)=3) or (length(values)=4) then
      begin
        if (values[0] <> '') and (values[0] <> '?') then
           parsedValue.red := ParseColorValue(values[0], error)
        else
           missingValues := true;
        if (values[1] <> '') and (values[1] <> '?') then
           parsedValue.green := ParseColorValue(values[1], error)
        else
           missingValues := true;
        if (values[2] <> '') and (values[2] <> '?') then
           parsedValue.blue := ParseColorValue(values[2], error)
        else
           missingValues := true;
        if length(values)=4 then
        begin
          if (values[3] <> '') and (values[3] <> '?') then
          begin
            val(values[3],alphaF,errPos);
            if errPos <> 0 then
            begin
               parsedValue.alpha := 255;
               error := true;
            end
            else
            begin
              if alphaF < 0 then
                parsedValue.alpha := 0 else
              if alphaF > 1 then
                parsedValue.alpha := 255
              else
                parsedValue.alpha := round(alphaF*255);
            end;
          end else
            missingValues := true;
        end else
          parsedValue.alpha := 255;
      end else
        error := true;
      exit;
    end;

    //remove HTML notation header
    if str[1]='#' then delete(str,1,1);

    //add alpha if missing (if you want an undefined alpha use '??' or '?')
    if length(str)=6 then str += 'FF';
    if length(str)=3 then str += 'F';

    //hex notation
    if length(str)=8 then
    begin
      if copy(str,1,2) <> '??' then
      begin
        val('$'+copy(str,1,2),parsedValue.red,errPos);
        if errPos <> 0 then error := true;
      end else missingValues := true;
      if copy(str,3,2) <> '??' then
      begin
        val('$'+copy(str,3,2),parsedValue.green,errPos);
        if errPos <> 0 then error := true;
      end else missingValues := true;
      if copy(str,5,2) <> '??' then
      begin
        val('$'+copy(str,5,2),parsedValue.blue,errPos);
        if errPos <> 0 then error := true;
      end else missingValues := true;
      if copy(str,7,2) <> '??' then
      begin
        val('$'+copy(str,7,2),parsedValue.alpha,errPos);
        if errPos <> 0 then
        begin
          error := true;
          parsedValue.alpha := 255;
        end;
      end else missingValues := true;
    end else
    if length(str)=4 then
    begin
      if str[1] <> '?' then
      begin
        val('$'+str[1],parsedValue.red,errPos);
        if errPos <> 0 then error := true;
        parsedValue.red *= $11;
      end else missingValues := true;
      if str[2] <> '?' then
      begin
        val('$'+str[2],parsedValue.green,errPos);
        if errPos <> 0 then error := true;
        parsedValue.green *= $11;
      end else missingValues := true;
      if str[3] <> '?' then
      begin
        val('$'+str[3],parsedValue.blue,errPos);
        if errPos <> 0 then error := true;
        parsedValue.blue *= $11;
      end else missingValues := true;
      if str[4] <> '?' then
      begin
        val('$'+str[4],parsedValue.alpha,errPos);
        if errPos <> 0 then
        begin
          error := true;
          parsedValue.alpha := 255;
        end else
          parsedValue.alpha *= $11;
      end else missingValues := true;
    end else
      error := true; //string format not recognised
  end;

end;

//this function returns the values that can be read from the string, otherwise
//it fills the gaps with the fallback values. The error boolean is True only
//if there was invalid values, it is not set to True if there was missing values.
function PartialStrToBGRA(str: string; const fallbackValues: TBGRAPixel; out
  error: boolean): TBGRAPixel;
var missingValues: boolean;
begin
  result := fallbackValues;
  TryStrToBGRA(str, result, missingValues, error);
end;

{ Read a color, for example in hexadecimal format RRGGBB(AA) or RGB(A). Partial colors are not accepted by this function. }
function StrToBGRA(str: string; const DefaultColor: TBGRAPixel): TBGRAPixel;
var missingValues, error: boolean;
begin
  result := BGRABlack;
  TryStrToBGRA(str, result, missingValues, error);
  if missingValues or error then result := DefaultColor;
end;

function MapHeight(Color: TBGRAPixel): Single;
var intval: integer;
begin
  intval := color.Green shl 16 + color.red shl 8 + color.blue;
  result := intval*5.960464832810452e-8;
end;

function MapHeightToBGRA(Height: Single; Alpha: Byte): TBGRAPixel;
var intval: integer;
begin
  if Height >= 1 then result := BGRA(255,255,255,alpha) else
  if Height <= 0 then result := BGRA(0,0,0,alpha) else
  begin
    intval := round(Height*16777215);
    result := BGRA(intval shr 8,intval shr 16,intval,alpha);
  end;
end;

{********************** Point functions **************************}

function PointF(x, y: single): TPointF;
begin
  Result.x := x;
  Result.y := y;
end;

function PointsF(const pts: array of TPointF): ArrayOfTPointF;
var
  i: Integer;
begin
  setlength(result, length(pts));
  for i := 0 to high(pts) do result[i] := pts[i];
end;

operator =(const pt1, pt2: TPointF): boolean;
begin
  result := (pt1.x = pt2.x) and (pt1.y = pt2.y);
end;

operator-(const pt1, pt2: TPointF): TPointF;
begin
  result.x := pt1.x-pt2.x;
  result.y := pt1.y-pt2.y;
end;

operator-(const pt2: TPointF): TPointF;
begin
  result.x := -pt2.x;
  result.y := -pt2.y;
end;

operator+(const pt1, pt2: TPointF): TPointF;
begin
  result.x := pt1.x+pt2.x;
  result.y := pt1.y+pt2.y;
end;

operator*(const pt1, pt2: TPointF): single;
begin
  result := pt1.x*pt2.x + pt1.y*pt2.y;
end;

operator*(const pt1: TPointF; factor: single): TPointF;
begin
  result.x := pt1.x*factor;
  result.y := pt1.y*factor;
end;

operator*(factor: single; const pt1: TPointF): TPointF;
begin
  result.x := pt1.x*factor;
  result.y := pt1.y*factor;
end;

function PtInRect(const pt: TPoint; r: TRect): boolean;
var
  temp: integer;
begin
  if r.right < r.left then
  begin
    temp    := r.left;
    r.left  := r.right;
    r.Right := temp;
  end;
  if r.bottom < r.top then
  begin
    temp     := r.top;
    r.top    := r.bottom;
    r.bottom := temp;
  end;
  Result := (pt.X >= r.left) and (pt.Y >= r.top) and (pt.X < r.right) and
    (pt.y < r.bottom);
end;

function RectWithSize(left, top, width, height: integer): TRect;
begin
  result.left := left;
  result.top := top;
  result.right := left+width;
  result.bottom := top+height;
end;

function VectLen(dx, dy: single): single;
begin
  result := sqrt(dx*dx+dy*dy);
end;

function VectLen(v: TPointF): single;
begin
  result := sqrt(v.x*v.x+v.y*v.y);
end;
{$OPTIMIZATION OFF}  // Modif J.P  5/2013
function IntersectLine(line1, line2: TLineDef): TPointF;
var parallel: boolean;
begin
  result := IntersectLine(line1,line2,parallel);
end;
{$OPTIMIZATION ON}

function IntersectLine(line1, line2: TLineDef; out parallel: boolean): TPointF;
var divFactor: double;
begin
  parallel := false;
  //if lines are parallel
  if ((line1.dir.x = line2.dir.x) and (line1.dir.y = line2.dir.y)) or
     ((abs(line1.dir.y) < 1e-6) and (abs(line2.dir.y) < 1e-6)) then
  begin
       parallel := true;
       //return the center of the segment between line origins
       result.x := (line1.origin.x+line2.origin.x)/2;
       result.y := (line1.origin.y+line2.origin.y)/2;
  end else
  if abs(line1.dir.y) < 1e-6 then //line1 is horizontal
  begin
       result.y := line1.origin.y;
       result.x := line2.origin.x + (result.y - line2.origin.y)
               /line2.dir.y*line2.dir.x;
  end else
  if abs(line2.dir.y) < 1e-6 then //line2 is horizontal
  begin
       result.y := line2.origin.y;
       result.x := line1.origin.x + (result.y - line1.origin.y)
               /line1.dir.y*line1.dir.x;
  end else
  begin
       divFactor := line1.dir.x/line1.dir.y - line2.dir.x/line2.dir.y;
       if abs(divFactor) < 1e-6 then //almost parallel
       begin
            parallel := true;
            //return the center of the segment between line origins
            result.x := (line1.origin.x+line2.origin.x)/2;
            result.y := (line1.origin.y+line2.origin.y)/2;
       end else
       begin
         result.y := (line2.origin.x - line1.origin.x +
                  line1.origin.y*line1.dir.x/line1.dir.y -
                  line2.origin.y*line2.dir.x/line2.dir.y)
                  / divFactor;
         result.x := line1.origin.x + (result.y - line1.origin.y)
                 /line1.dir.y*line1.dir.x;
       end;
  end;
end;

{ Check if a polygon is convex, i.e. it always turns in the same direction }
function IsConvex(const pts: array of TPointF; IgnoreAlign: boolean = true): boolean;
var
  positive,negative,zero: boolean;
  product: single;
  i: Integer;
begin
  positive := false;
  negative := false;
  zero := false;
  for i := 0 to high(pts) do
  begin
    product := (pts[(i+1) mod length(pts)].x-pts[i].x)*(pts[(i+2) mod length(pts)].y-pts[i].y) -
               (pts[(i+1) mod length(pts)].y-pts[i].y)*(pts[(i+2) mod length(pts)].x-pts[i].x);
    if product > 0 then
    begin
      if negative then
      begin
        result := false;
        exit;
      end;
      positive := true;
    end else
    if product < 0 then
    begin
      if positive then
      begin
        result := false;
        exit;
      end;
      negative := true;
    end else
      zero := true;
  end;
  if not IgnoreAlign and zero then
    result := false
  else
    result := true;
end;

{ Check if two segments intersect }
function DoesSegmentIntersect(pt1,pt2,pt3,pt4: TPointF): boolean;
var
  seg1: TLineDef;
  seg1len: single;
  seg2: TLineDef;
  seg2len: single;
  inter: TPointF;
  pos1,pos2: single;
  para: boolean;

begin
  { Determine line definitions }
  seg1.origin := pt1;
  seg1.dir := pt2-pt1;
  seg1len := sqrt(sqr(seg1.dir.X)+sqr(seg1.dir.Y));
  if seg1len = 0 then
  begin
    result := false;
    exit;
  end;
  seg1.dir *= 1/seg1len;

  seg2.origin := pt3;
  seg2.dir := pt4-pt3;
  seg2len := sqrt(sqr(seg2.dir.X)+sqr(seg2.dir.Y));
  if seg2len = 0 then
  begin
    result := false;
    exit;
  end;
  seg2.dir *= 1/seg2len;

  //obviously parallel
  if seg1.dir = seg2.dir then
    result := false
  else
  begin
    //try to compute intersection
    inter := IntersectLine(seg1,seg2,para);
    if para then
      result := false
    else
    begin
      //check if intersections are inside the segments
      pos1 := (inter-seg1.origin)*seg1.dir;
      pos2 := (inter-seg2.origin)*seg2.dir;
      if (pos1 >= 0) and (pos1 <= seg1len) and
         (pos2 >= 0) and (pos2 <= seg2len) then
        result := true
      else
        result := false;
    end;
  end;
end;

{ Check if a quaduadrilateral intersects itself }
function DoesQuadIntersect(pt1,pt2,pt3,pt4: TPointF): boolean;
begin
  result := DoesSegmentIntersect(pt1,pt2,pt3,pt4) or DoesSegmentIntersect(pt2,pt3,pt4,pt1);
end;

{************************** Cyclic functions *******************}

// Get the cyclic value in the range [0..cycle-1]
function PositiveMod(value, cycle: Int32or64): Int32or64; inline;
begin
  result := value mod cycle;
  if result < 0 then //modulo can be negative
    Inc(result, cycle);
end;

{ Table of precalc values. Note : the value is stored for
  the first half of the cycle, and values are stored 'minus 1'
  in order to stay in the range 0..65535 }
var
  sinTab65536: packed array of word;
  byteSqrtTab: packed array of word;

function Sin65536(value: word): Int32or64;
var b: integer;
begin
  //allocate array
  if sinTab65536 = nil then
    setlength(sinTab65536,32768);

  if value >= 32768 then //function is upside down after half-period
  begin
    b := value xor 32768;
    if sinTab65536[b] = 0 then //precalc
      sinTab65536[b] := round((sin(b*2*Pi/65536)+1)*65536/2)-1;
    result := not sinTab65536[b];
  end else
  begin
    b := value;
    if sinTab65536[b] = 0 then //precalc
      sinTab65536[b] := round((sin(b*2*Pi/65536)+1)*65536/2)-1;
    {$hints off}
    result := sinTab65536[b]+1;
    {$hints on}
  end;
end;

function Cos65536(value: word): Int32or64;
begin
  {$PUSH}{$R-}
  result := Sin65536(value+16384); //cosine is translated
  {$POP}
end;

procedure PrecalcSin65536;
var
  i: Integer;
begin
  for i := 0 to 32767 do Sin65536(i);
end;

procedure PrecalcByteSqrt;
var i: integer;
begin
  if byteSqrtTab = nil then
  begin
    setlength(byteSqrtTab,256);
    for i := 0 to 255 do
      byteSqrtTab[i] := round(sqrt(i/255)*255);
  end;
end;

function ByteSqrt(value: byte): byte; inline;
begin
  if byteSqrtTab = nil then PrecalcByteSqrt;
  result := ByteSqrtTab[value];
end;

function DetectFileFormat(AFilenameUTF8: string): TBGRAImageFormat;
var stream: TFileStreamUTF8;
begin
  try
    stream := TFileStreamUTF8.Create(AFilenameUTF8,fmOpenRead or fmShareDenyWrite);
  except
    result := ifUnknown;
    exit;
  end;
  try
    result := DetectFileFormat(stream, ExtractFileExt(AFilenameUTF8));
  finally
    stream.Free;
  end;
end;

function DetectFileFormat(AStream: TStream; ASuggestedExtensionUTF8: string
  ): TBGRAImageFormat;
var
  scores: array[TBGRAImageFormat] of integer;
  imageFormat,bestImageFormat: TBGRAImageFormat;
  bestScore: integer;

  procedure DetectFromStream;
  var
    {%H-}magic: packed array[0..7] of byte;
    {%H-}dwords: packed array[0..9] of DWORD;
    magicAsText: string;

    streamStartPos, maxFileSize: Int64;
    expectedFileSize: DWord;

    procedure DetectTarga;
    var
      paletteCount: integer;
      {%H-}targaPixelFormat: packed record pixelDepth: byte; imgDescriptor: byte; end;
    begin
      if (magic[1] in[$00,$01]) and (magic[2] in[0,1,2,3,9,10,11]) and (maxFileSize >= 18) then
      begin
        paletteCount:= magic[5] + magic[6] shl 8;
        if ((paletteCount = 0) and (magic[7] = 0)) or
          (magic[7] in [16,24,32]) then //check palette bit count
        begin
          AStream.Position:= streamStartPos+16;
          if AStream.Read({%H-}targaPixelFormat,2) = 2 then
          begin
            if (targaPixelFormat.pixelDepth in [8,16,24,32]) and
              (targaPixelFormat.imgDescriptor and 15 < targaPixelFormat.pixelDepth) then
                inc(scores[ifTarga],2);
          end;
        end;
      end;
    end;

    procedure DetectLazPaint;
    var
      w,h: dword;
      i: integer;
    begin
      if (copy(magicAsText,1,8) = 'LazPaint') then //with header
      begin
        AStream.Position:= streamStartPos+8;
        if AStream.Read(dwords,10*4) = 10*4 then
        begin
          for i := 0 to 6 do dwords[i] := LEtoN(dwords[i]);
          if (dwords[0] = 0) and (dwords[1] <= expectedFileSize) and (dwords[5] <= expectedFileSize) and
             (dwords[9] <= expectedFileSize) and
            (dwords[6] = 0) then inc(scores[ifLazPaint],2);
        end;
      end else //without header
      if ((magic[0] <> 0) or (magic[1] <> 0)) and (magic[2] = 0) and (magic[3] = 0) and
         ((magic[4] <> 0) or (magic[5] <> 0)) and (magic[6] = 0) and (magic[7] = 0) then
      begin
        w := magic[0] + (magic[1] shl 8);
        h := magic[4] + (magic[5] shl 8);
        AStream.Position:= streamStartPos+8;
        if AStream.Read(dwords,4) = 4 then
        begin
          dwords[0] := LEtoN(dwords[0]);
          if (dwords[0] > 0) and (dwords[0] < 65536) then
          begin
            if 12+dwords[0] < expectedFileSize then
            begin
              AStream.Position:= streamStartPos+12+dwords[0];
              if AStream.Read(dwords,6*4) = 6*4 then
              begin
                for i := 0 to 5 do dwords[i] := LEtoN(dwords[i]);
                if (dwords[0] <= w) and (dwords[1] <= h) and
                  (dwords[2] <= w) and (dwords[3] <= h) and
                  (dwords[2] >= dwords[0]) and (dwords[3] >= dwords[1]) and
                  ((dwords[4] = 0) or (dwords[4] = 1)) and
                  (dwords[5] > 0) then inc(scores[ifLazPaint],1);
              end;
            end;
          end;
        end;
      end;
    end;

  begin
    fillchar({%H-}magic, sizeof(magic), 0);
    fillchar({%H-}dwords, sizeof(dwords), 0);

    streamStartPos:= AStream.Position;
    maxFileSize:= AStream.Size - streamStartPos;
    if maxFileSize < 8 then exit;
    if AStream.Read(magic,sizeof(magic)) <> sizeof(magic) then
    begin
      fillchar(scores,sizeof(scores),0);
      exit;
    end;
    setlength(magicAsText,sizeof(magic));
    move(magic[0],magicAsText[1],sizeof(magic));

    if (magic[0] = $ff) and (magic[1] = $d8) then
    begin
         inc(scores[ifJpeg]);
         if (magic[2] = $ff) and (magic[3] >= $c0) then inc(scores[ifJpeg]);
    end;

    if (magic[0] = $89) and (magic[1] = $50) and (magic[2] = $4e) and
      (magic[3] = $47) and (magic[4] = $0d) and (magic[5] = $0a) and
      (magic[6] = $1a) and (magic[7] = $0a) then inc(scores[ifPng],2);

    if (copy(magicAsText,1,6)='GIF87a') or (copy(magicAsText,1,6)='GIF89a') then inc(scores[ifGif],2);

    if (magic[0] = $0a) and (magic[1] in [0,2,3,4,5]) and (magic[2] in[0,1]) and (magic[3] in[1,2,4,8]) then
      inc(scores[ifPcx],2);

    if (copy(magicAsText,1,2)='BM') then
    begin
      inc(scores[ifBmp]);
      expectedFileSize:= magic[2] + (magic[3] shl 8) + (magic[4] shl 16) + (magic[5] shl 24);
      if expectedFileSize = maxFileSize then inc(scores[ifBmp]);
    end else
    if (copy(magicAsText,1,2)='RL') then
    begin
      inc(scores[ifBmpMioMap]);
      if (magic[2] in[0,1]) and (magic[3] = 0) then inc(scores[ifBmpMioMap]);
    end;

    if (magic[0] = $00) and (magic[1] = $00) and (magic[2] in[$01,$02]) and (magic[3] = $00) and
      (magic[4] + (magic[5] shl 8) > 0) then inc(scores[ifIco]);

    if (copy(magicAsText,1,4) = 'PDN3') then
    begin
      expectedFileSize:= 6 + (magic[4] + (magic[5] shl 8) + (magic[6] shl 16)) + 2;
      if expectedFileSize <= maxFileSize then
      begin
        inc(scores[ifPaintDotNet]);
        if magic[7] = $3c then inc(scores[ifPaintDotNet]);
      end;
    end;

    DetectLazPaint;

    if (magic[0] = $50) and (magic[1] = $4b) and (magic[2] = $03) and (magic[3] = $04) then
    begin
      if DefaultBGRAImageReader[ifOpenRaster] = nil then inc(scores[ifOpenRaster]) else
      with CreateBGRAImageReader(ifOpenRaster) do
        try
          if CheckContents(AStream) then inc(scores[ifOpenRaster],2);
        finally
          Free;
        end;
    end;

    if (copy(magicAsText,1,4) = '8BPS') and (magic[4] = $00) and (magic[5] = $01) then inc(scores[ifPsd],2);

    DetectTarga;

    if (copy(magicAsText,1,2)='II') and (magic[2] = 42) and (magic[3]=0) then inc(scores[ifTiff]) else
    if (copy(magicAsText,1,2)='MM') and (magic[2] = 0) and (magic[3]=42) then inc(scores[ifTiff]);

    if (copy(magicAsText,1,8) = '/* XPM *') or (copy(magicAsText,1,6) = '! XPM2') then inc(scores[ifXPixMap]);

    AStream.Position := streamStartPos;
  end;

var
  extFormat: TBGRAImageFormat;

begin
  result := ifUnknown;
  for imageFormat:= low(TBGRAImageFormat) to high(TBGRAImageFormat) do
    scores[imageFormat] := 0;

  ASuggestedExtensionUTF8:= UTF8LowerCase(ASuggestedExtensionUTF8);
  if (ASuggestedExtensionUTF8 <> '') and (UTF8Copy(ASuggestedExtensionUTF8,1,1) <> '.') then
    ASuggestedExtensionUTF8 := '.'+ASuggestedExtensionUTF8;

  extFormat:= SuggestImageFormat(ASuggestedExtensionUTF8);
  if extFormat <> ifUnknown then inc(scores[extFormat]);

  If AStream <> nil then DetectFromStream;

  bestScore := 0;
  bestImageFormat:= ifUnknown;
  for imageFormat:=low(TBGRAImageFormat) to high(TBGRAImageFormat) do
    if scores[imageFormat] > bestScore then
    begin
      bestScore:= scores[imageFormat];
      bestImageFormat:= imageFormat;
    end;
  result := bestImageFormat;
end;

function SuggestImageFormat(AFilenameOrExtensionUTF8: string): TBGRAImageFormat;
var ext: string;
begin
  result := ifUnknown;

  ext := ExtractFileName(AFilenameOrExtensionUTF8);
  if pos('.', ext) <> 0 then ext := ExtractFileExt(ext) else ext := '.'+ext;
  ext := UTF8LowerCase(ext);

  if (ext = '.jpg') or (ext = '.jpeg') then result := ifJpeg else
  if (ext = '.png') then result := ifPng else
  if (ext = '.gif') then result := ifGif else
  if (ext = '.pcx') then result := ifPcx else
  if (ext = '.bmp') then result := ifBmp else
  if (ext = '.ico') or (ext = '.cur') then result := ifIco else
  if (ext = '.pdn') then result := ifPaintDotNet else
  if (ext = '.lzp') then result := ifLazPaint else
  if (ext = '.ora') then result := ifOpenRaster else
  if (ext = '.psd') then result := ifPsd else
  if (ext = '.tga') then result := ifTarga else
  if (ext = '.tif') or (ext = '.tiff') then result := ifTiff else
  if (ext = '.xwd') then result := ifXwd else
  if (ext = '.xpm') then result := ifXPixMap;
end;

function CreateBGRAImageReader(AFormat: TBGRAImageFormat): TFPCustomImageReader;
begin
  if DefaultBGRAImageReader[AFormat] = nil then
  begin
    case AFormat of
      ifUnknown: raise exception.Create('The image format is unknown.');
      ifOpenRaster: raise exception.Create('You need to call BGRAOpenRaster.RegisterOpenRasterFormat to read this image.');
      ifPaintDotNet: raise exception.Create('You need to call BGRAPaintNet.RegisterPaintNetFormat to read this image.');
    else
      raise exception.Create('The image reader is not registered for this image format.');
    end;
  end;
  result := DefaultBGRAImageReader[AFormat].Create;
end;

function CreateBGRAImageWriter(AFormat: TBGRAImageFormat; AHasTransparentPixels: boolean): TFPCustomImageWriter;
begin
  if DefaultBGRAImageWriter[AFormat] = nil then
  begin
    case AFormat of
      ifUnknown: raise exception.Create('The image format is unknown');
      ifOpenRaster: raise exception.Create('You need to call BGRAOpenRaster.RegisterOpenRasterFormat to write with this image format.');
    else
      raise exception.Create('The image writer is not registered for this image format.');
    end;
  end;

  if AFormat = ifPng then
  begin
    result := TFPWriterPNG.Create;
    TFPWriterPNG(result).Indexed := false;
    TFPWriterPNG(result).WordSized := false;
    TFPWriterPNG(result).UseAlpha := AHasTransparentPixels;
  end else
  if AFormat = ifBmp then
  begin
    result := TFPWriterBMP.Create;
    if AHasTransparentPixels then
      TFPWriterBMP(result).BitsPerPixel := 32 else
      TFPWriterBMP(result).BitsPerPixel := 24;
  end else
  if AFormat = ifXPixMap then
  begin
    result := TFPWriterXPM.Create;
    TFPWriterXPM(result).ColorCharSize := 2;
  end else
    result := DefaultBGRAImageWriter[AFormat].Create;
end;

initialization

  InitGamma;
  {$DEFINE INCLUDE_COLOR_LIST}
  {$I csscolorconst.inc}
  DefaultBGRAImageWriter[ifJpeg] := TFPWriterJPEG;
  DefaultBGRAImageWriter[ifPng] := TFPWriterPNG;
  DefaultBGRAImageWriter[ifBmp] := TFPWriterBMP;
  DefaultBGRAImageWriter[ifPcx] := TFPWriterPCX;
  DefaultBGRAImageWriter[ifTarga] := TFPWriterTarga;
  DefaultBGRAImageWriter[ifXPixMap] := TFPWriterXPM;
  DefaultBGRAImageWriter[ifTiff] := TFPWriterTiff;
  //writing XWD not implemented

  DefaultBGRAImageReader[ifTiff] := TFPReaderTiff;
  DefaultBGRAImageReader[ifXwd] := TFPReaderXWD;
  //the other readers are registered by their unit

finalization

  CSSColors.Free;
  VGAColors.Free;

end.