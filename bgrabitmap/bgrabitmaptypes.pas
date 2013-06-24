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
                   rmFineResample);   //use resample filters 
  TResampleFilter = (rfLinear,        //linear interpolation
                     rfHalfCosine,    //mix of rfLinear and rfCosine
                     rfCosine,        //cosine-like interpolation
                     rfBicubic,       //simple bi-cubic filter (blur)
                     rfMitchell,      //downsizing interpolation
                     rfSpline,        //upsizing interpolation
                     rfBestQuality);  //mix of rfMitchell and rfSpline

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
    procedure Add(Name: string; Color: TBGRAPixel);
    procedure Finished;
    function IndexOf(Name: string): integer;

    property ByName[Name: string]: TBGRAPixel read GetByName;
    property ByIndex[Index: integer]: TBGRAPixel read GetByIndex; default;
    property Name[Index: integer]: string read GetName;
    property Count: integer read FNbColors;
  end;

var
  CSSColors: TBGRAColorList;

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
     function LoadAsBmp32(Str: TStream): boolean; virtual; abstract;
     function LoadFromFileAsBmp32(filename: string): boolean;

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
     constructor Create(AStream: TStream); virtual; abstract; overload;

     function NewBitmap(AWidth, AHeight: integer): TBGRACustomBitmap; virtual; abstract; overload;
     function NewBitmap(AWidth, AHeight: integer; Color: TBGRAPixel): TBGRACustomBitmap; virtual; abstract; overload;
     function NewBitmap(Filename: string): TBGRACustomBitmap; virtual; abstract; overload;

     procedure LoadFromFile(const filename: string); virtual;
     procedure LoadFromStream(Str: TStream); virtual;
     procedure LoadFromStream(Str: TStream; Handler: TFPCustomImageReader); virtual;
     procedure SaveToFile(const filename: string); virtual;
     procedure SaveToFile(const filename: string; Handler:TFPCustomImageWriter); virtual;
     procedure SaveToStreamAsPng(Str: TStream); virtual; abstract;
     procedure Assign(ABitmap: TBitmap); virtual; abstract; overload;
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
     function GetPixel(x, y: int32or64): TBGRAPixel; virtual; abstract;
     function GetPixel(x, y: single; AResampleFilter: TResampleFilter = rfLinear): TBGRAPixel; virtual; abstract; overload;
     function GetPixelCycle(x, y: int32or64): TBGRAPixel; virtual;
     function GetPixelCycle(x, y: single; AResampleFilter: TResampleFilter = rfLinear): TBGRAPixel; virtual; abstract; overload;
     function GetPixelCycle(x, y: single; AResampleFilter: TResampleFilter; repeatX: boolean; repeatY: boolean): TBGRAPixel; virtual; abstract; overload;

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
     procedure FillRect(x, y, x2, y2: integer; texture: IBGRAScanner; mode: TDrawMode); virtual; abstract;
     procedure FillRectAntialias(x, y, x2, y2: single; c: TBGRAPixel); virtual; abstract;
     procedure FillRectAntialias(x, y, x2, y2: single; texture: IBGRAScanner); virtual; abstract;
     procedure EraseRectAntialias(x, y, x2, y2: single; alpha: byte); virtual; abstract;
     procedure AlphaFillRect(x, y, x2, y2: integer; alpha: byte); virtual; abstract;

     procedure TextOut(x, y: single; s: string; c: TBGRAPixel; align: TAlignment); virtual; abstract; overload;
     procedure TextOut(x, y: single; s: string; texture: IBGRAScanner; align: TAlignment); virtual; abstract; overload;
     procedure TextOutAngle(x, y: single; orientation: integer; s: string; c: TBGRAPixel; align: TAlignment); virtual; abstract;
     procedure TextOutAngle(x, y: single; orientation: integer; s: string; texture: IBGRAScanner; align: TAlignment); virtual; abstract;
     procedure TextOut(x, y: single; s: string; c: TBGRAPixel); virtual; overload;
     procedure TextOut(x, y: single; s: string; c: TColor); virtual; overload;
     procedure TextOut(x, y: single; s: string; texture: IBGRAScanner); virtual; overload;
     procedure TextRect(ARect: TRect; x, y: integer; s: string; style: TTextStyle; c: TBGRAPixel); virtual; abstract; overload;
     procedure TextRect(ARect: TRect; x, y: integer; s: string; style: TTextStyle; texture: IBGRAScanner); virtual; abstract; overload;
     procedure TextRect(ARect: TRect; s: string; halign: TAlignment; valign: TTextLayout; c: TBGRAPixel); virtual; overload;
     procedure TextRect(ARect: TRect; s: string; halign: TAlignment; valign: TTextLayout; texture: IBGRAScanner); virtual; overload;
     function TextSize(s: string): TSize; virtual; abstract;

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
     procedure FillMask(x,y: integer; AMask: TBGRACustomBitmap; color: TBGRAPixel); virtual; abstract; overload;
     procedure FillMask(x,y: integer; AMask: TBGRACustomBitmap; texture: IBGRAScanner); virtual; abstract; overload;
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
     procedure PutImage(x, y: integer; Source: TBGRACustomBitmap; mode: TDrawMode; AOpacity: byte = 255); virtual; abstract;
     procedure PutImageSubpixel(x, y: single; Source: TBGRACustomBitmap);
     procedure PutImagePart(x,y: integer; Source: TBGRACustomBitmap; SourceRect: TRect; mode: TDrawMode; AOpacity: byte = 255);
     procedure PutImageAffine(Origin,HAxis,VAxis: TPointF; Source: TBGRACustomBitmap; AOpacity: Byte=255); virtual; abstract;
     procedure PutImageAngle(x,y: single; Source: TBGRACustomBitmap; angle: single; imageCenterX: single = 0; imageCenterY: single = 0; AOpacity: Byte=255; ARestoreOffsetAfterRotation: boolean = false); virtual; abstract;
     procedure BlendImage(x, y: integer; Source: TBGRACustomBitmap; operation: TBlendOperation); virtual; abstract;
     procedure BlendImageOver(x, y: integer; Source: TBGRACustomBitmap; operation: TBlendOperation; AOpacity: byte = 255;
         ALinearBlend: boolean = false); virtual; abstract;
     function Duplicate(DuplicateProperties: Boolean = False): TBGRACustomBitmap; virtual; abstract;
     function Equals(comp: TBGRACustomBitmap): boolean; virtual; abstract;
     function Equals(comp: TBGRAPixel): boolean; virtual; abstract;
     function Resample(newWidth, newHeight: integer;
       mode: TResampleMode = rmFineResample): TBGRACustomBitmap; virtual; abstract;
     procedure VerticalFlip; virtual; abstract;
     procedure HorizontalFlip; virtual; abstract;
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
     procedure ApplyMask(mask: TBGRACustomBitmap); virtual; abstract;
     function GetImageBounds(Channel: TChannel = cAlpha; ANothingValue: Byte = 0): TRect; virtual; abstract;
     function GetImageBounds(Channels: TChannels): TRect; virtual; abstract;
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
     function FilterRotate(origin: TPointF; angle: single): TBGRACustomBitmap; virtual; abstract;
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

     //interface
     function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
     function _AddRef: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
     function _Release: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};

     //IBGRAScanner
     function ScanAtInteger(X,Y: integer): TBGRAPixel; virtual; abstract;
     procedure ScanMoveTo(X,Y: Integer); virtual; abstract;
     function ScanNextPixel: TBGRAPixel; virtual; abstract;
     function ScanAt(X,Y: Single): TBGRAPixel; virtual; abstract;
     procedure ScanPutPixels(pdest: PBGRAPixel; count: integer; mode: TDrawMode); virtual;
     function IsScanPutPixelsDefined: boolean; virtual;
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

  TBGRACustomFontRenderer = class
    FontName: string;
    FontStyle: TFontStyles;
    FontQuality : TBGRAFontQuality;
    FontOrientation: integer;
    FontEmHeight: integer; //negative for full height
    function GetFontPixelMetric: TFontPixelMetric; virtual; abstract;
    procedure TextOutAngle(ADest: TBGRACustomBitmap; x, y: single; orientation: integer; s: string; c: TBGRAPixel; align: TAlignment); virtual; abstract;
    procedure TextOutAngle(ADest: TBGRACustomBitmap; x, y: single; orientation: integer; s: string; texture: IBGRAScanner; align: TAlignment); virtual; abstract;
    procedure TextOut(ADest: TBGRACustomBitmap; x, y: single; s: string; texture: IBGRAScanner; align: TAlignment); virtual; abstract;
    procedure TextOut(ADest: TBGRACustomBitmap; x, y: single; s: string; c: TBGRAPixel; align: TAlignment); virtual; abstract;
    procedure TextRect(ADest: TBGRACustomBitmap; ARect: TRect; x, y: integer; s: string; style: TTextStyle; c: TBGRAPixel); virtual; abstract;
    procedure TextRect(ADest: TBGRACustomBitmap; ARect: TRect; x, y: integer; s: string; style: TTextStyle; texture: IBGRAScanner); virtual; abstract;
    function TextSize(s: string): TSize; virtual; abstract;
  end;

type
  TBGRABitmapAny = class of TBGRACustomBitmap;  //used to create instances of the same type (see NewBitmap)
  TBGRATextOutImproveReadabilityMode = (irMask, irNormal, irClearTypeRGB, irClearTypeBGR);

var
  BGRABitmapFactory : TBGRABitmapAny;
  BGRATextOutImproveReadabilityProc : procedure (bmp: TBGRACustomBitmap; AFont: TFont; xf,yf: single; text: string; color: TBGRAPixel; tex: IBGRAScanner; align: TAlignment; mode : TBGRATextOutImproveReadabilityMode);

{ Color functions }
function GetIntensity(c: TExpandedPixel): word; inline;
function SetIntensity(c: TExpandedPixel; intensity: word): TExpandedPixel;
function GetLightness(c: TExpandedPixel): word; inline;
function SetLightness(c: TExpandedPixel; lightness: word): TExpandedPixel;
function ApplyLightnessFast(color: TBGRAPixel; lightness: word): TBGRAPixel; inline;
function ApplyIntensityFast(color: TBGRAPixel; lightness: longword): TBGRAPixel;
function CombineLightness(lightness1,lightness2: Int32or64): Int32or64;
function BGRAToHSLA(c: TBGRAPixel): THSLAPixel;
function ExpandedToHSLA(ec: TExpandedPixel): THSLAPixel; inline;
function BGRAToGSBA(c: TBGRAPixel): THSLAPixel;
function HSLAToExpanded(c: THSLAPixel): TExpandedPixel;
function HSLAToBGRA(c: THSLAPixel): TBGRAPixel;
function GtoH(ghue: word): word;
function HtoG(hue: word): word;
function HueDiff(h1, h2: word): word;
function GetHue(ec: TExpandedPixel): word;
function ColorImportance(ec: TExpandedPixel): word;
function GSBAToBGRA(c: THSLAPixel): TBGRAPixel;
function GSBAToHSLA(c: THSLAPixel): THSLAPixel;
function GammaExpansion(c: TBGRAPixel): TExpandedPixel; inline;
function GammaCompression(ec: TExpandedPixel): TBGRAPixel; inline;
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
function BGRAToStr(c: TBGRAPixel): string;
function StrToBGRA(str: string): TBGRAPixel;
function StrToBGRA(str: string; DefaultColor: TBGRAPixel): TBGRAPixel;

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
function PtInRect(pt: TPoint; r: TRect): boolean;
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

implementation

uses Math, SysUtils, FPReadBMP;

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

{ Check if a PointF structure is empty or should be treated as a list separator }
function isEmptyPointF(pt: TPointF): boolean;
begin
  Result := (pt.x = EmptySingle) and (pt.y = EmptySingle);
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

procedure TBGRAColorList.Add(Name: string; Color: TBGRAPixel);
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

function TBGRACustomBitmap.LoadFromFileAsBmp32(filename: string): boolean;
var stream: TFileStream;
begin
  stream := nil;
  result := false;
  try
    stream:= TFileStream.Create(filename,fmOpenRead);
    result := LoadAsBmp32(stream);
  except
    on ex: exception do
    begin
    end;
  end;
  stream.Free;
end;

{ These declaration make sure that these methods are virtual }
procedure TBGRACustomBitmap.LoadFromFile(const filename: string);
begin
  if CompareText(copy(filename,length(filename)-3,4),'.bmp')=0 then
    if LoadFromFileAsBmp32(filename) then exit;
  inherited LoadFromFile(filename);
end;

procedure TBGRACustomBitmap.SaveToFile(const filename: string);
begin
  inherited SaveToFile(filename);
end;

procedure TBGRACustomBitmap.SaveToFile(const filename: string;
  Handler: TFPCustomImageWriter);
begin
  inherited SaveToFile(filename, Handler);
end;

{ LoadFromStream uses TFPCustomImage routine, which uses
  Colors property to access pixels. That's why the
  FP drawing mode is temporarily changed to load
  bitmaps properly }
procedure TBGRACustomBitmap.LoadFromStream(Str: TStream);
var
  OldDrawMode: TDrawMode;
begin
  OldDrawMode := CanvasDrawModeFP;
  CanvasDrawModeFP := dmSet;
  try
    if not LoadAsBmp32(Str) then
      inherited LoadFromStream(Str);
  finally
    CanvasDrawModeFP := OldDrawMode;
  end;
end;

{ See above }
procedure TBGRACustomBitmap.LoadFromStream(Str: TStream;
  Handler: TFPCustomImageReader);
var
  OldDrawMode: TDrawMode;
begin
  OldDrawMode := CanvasDrawModeFP;
  CanvasDrawModeFP := dmSet;
  try
    if Handler is TFPReaderBMP then
    begin
      if not LoadAsBmp32(Str) then
        inherited LoadFromStream(Str, Handler);
    end else
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

procedure TBGRACustomBitmap.TextOut(x, y: single; s: string; c: TBGRAPixel);
begin
  TextOut(x, y, s, c, taLeftJustify);
end;

procedure TBGRACustomBitmap.TextOut(x, y: single; s: string; c: TColor);
begin
  TextOut(x, y, s, ColorToBGRA(c));
end;

procedure TBGRACustomBitmap.TextOut(x, y: single; s: string;
  texture: IBGRAScanner);
begin
  TextOut(x, y, s, texture, taLeftJustify);
end;

procedure TBGRACustomBitmap.TextRect(ARect: TRect; s: string;
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
  TextRect(ARect,ARect.Left,ARect.Top,s,style,c);
end;

procedure TBGRACustomBitmap.TextRect(ARect: TRect; s: string;
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
  TextRect(ARect,ARect.Left,ARect.Top,s,style,texture);
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
  if Source = nil then exit;
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
function GetLightness(c: TExpandedPixel): word; inline;
begin
  Result := (c.red * redWeightShl10 + c.green * greenWeightShl10 +
    c.blue * blueWeightShl10 + 512) shr 10;
end;

function SetLightness(c: TExpandedPixel; lightness: word): TExpandedPixel;
var
  curLightness: word;
  AddedWhiteness, maxBeforeWhite: word;
  clip: boolean;
begin
  curLightness := GetLightness(c);
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
    Result := SetIntensity(c, (GetIntensity(c) * lightness + (curLightness shr 1)) div
      curLightness);
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

function ExpandedToHSLA(ec: TExpandedPixel): THSLAPixel;
const
  deg60  = 8192;
  deg120 = deg60 * 2;
  deg240 = deg60 * 4;
  deg360 = deg60 * 6;
var
  min, max, minMax: integer;
  twiceLightness: integer;
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
    Result.hue := 0
  else
  if max = r then
    Result.hue := (((g - b) * deg60) div
      minMax + deg360) mod deg360
  else
  if max = g then
    Result.hue := ((b - r) * deg60) div minMax + deg120
  else
    {max = b} Result.hue :=
      ((r - g) * deg60) div minMax + deg240;
  twiceLightness := max + min;
  if min = max then
    Result.saturation := 0
  else
  {$hints off}
  if twiceLightness < 65536 then
    Result.saturation := (int64(minMax) shl 16) div (twiceLightness + 1)
  else
    Result.saturation := (int64(minMax) shl 16) div (131072 - twiceLightness);
  {$hints on}
  Result.lightness := twiceLightness shr 1;
  Result.alpha := ec.alpha;
  Result.hue   := (Result.hue shl 16) div deg360;
end;

function HtoG(hue: word): word;
const
  segmentDest: array[0..5] of word =
     (13653, 10923, 8192, 13653, 10923, 8192);
  segmentSrc: array[0..5] of word =
     (10923, 10922, 10923, 10923, 10922, 10923);
begin
  if hue < segmentSrc[0] then
    result := hue * segmentDest[0] div segmentSrc[0]
  else
  begin
    result := segmentDest[0];
    hue -= segmentSrc[0];
    if hue < segmentSrc[1] then
      result += hue * segmentDest[1] div segmentSrc[1]
    else
    begin
      result += segmentDest[1];
      hue -= segmentSrc[1];
      if hue < segmentSrc[2] then
        result += hue * segmentDest[2] div segmentSrc[2]
      else
      begin
        result += segmentDest[2];
        hue -= segmentSrc[2];
        if hue < segmentSrc[3] then
          result += hue * segmentDest[3] div segmentSrc[3]
        else
        begin
          result += segmentDest[3];
          hue -= segmentSrc[3];
          if hue < segmentSrc[4] then
            result += hue * segmentDest[4] div segmentSrc[4]
          else
          begin
            result += segmentDest[4];
            hue -= segmentSrc[4];
            result += hue * segmentDest[5] div segmentSrc[5];
          end;
        end;
      end;
    end;
  end;
end;

function GtoH(ghue: word): word;
const
  segment: array[0..5] of word =
     (13653, 10923, 8192, 13653, 10923, 8192);
begin
  if ghue < segment[0] then
    result := ghue * 10923 div segment[0]
  else
  begin
    ghue -= segment[0];
    if ghue < segment[1] then
      result := ghue * (21845-10923) div segment[1] + 10923
    else
    begin
      ghue -= segment[1];
      if ghue < segment[2] then
        result := ghue * (32768-21845) div segment[2] + 21845
      else
      begin
        ghue -= segment[2];
        if ghue < segment[3] then
          result := ghue * (43691-32768) div segment[3] + 32768
        else
        begin
          ghue -= segment[3];
          if ghue < segment[4] then
            result := ghue * (54613-43691) div segment[4] + 43691
          else
          begin
            ghue -= segment[4];
            result := ghue * (65536-54613) div segment[5] + 54613;
          end;
        end;
      end;
    end;
  end;
end;

function BGRAToGSBA(c: TBGRAPixel): THSLAPixel;
var ec: TExpandedPixel;
    lightness: word;
begin
  ec := GammaExpansion(c);
  lightness := GetLightness(ec);

  result := ExpandedToHSLA(ec);
  if result.lightness > 32768 then
    result.saturation := result.saturation* word(65535-result.lightness) div 32767;
  result.lightness := lightness;
  result.hue := HtoG(result.hue);
end;

function HSLAToExpanded(c: THSLAPixel): TExpandedPixel;
const
  deg30  = 4096;
  deg60  = 8192;
  deg120 = deg60 * 2;
  deg180 = deg60 * 3;
  deg240 = deg60 * 4;
  deg360 = deg60 * 6;

  function ComputeColor(p, q: integer; h: integer): word; inline;
  begin
    if h > deg360 then
      Dec(h, deg360);
    if h < deg60 then
      Result := p + ((q - p) * h + deg30) div deg60
    else
    if h < deg180 then
      Result := q
    else
    if h < deg240 then
      Result := p + ((q - p) * (deg240 - h) + deg30) div deg60
    else
      Result := p;
  end;

var
  q, p: integer;
begin
  c.hue := c.hue * deg360 shr 16;
  if c.saturation = 0 then  //gray
  begin
    result.red   := c.lightness;
    result.green := c.lightness;
    result.blue  := c.lightness;
    result.alpha := c.alpha;
    exit;
  end;
  {$hints off}
  if c.lightness < 32768 then
    q := (c.lightness shr 1) * ((65535 + c.saturation) shr 1) shr 14
  else
    q := c.lightness + c.saturation - ((c.lightness shr 1) *
      (c.saturation shr 1) shr 14);
  {$hints on}
  if q > 65535 then
    q := 65535;
  p   := c.lightness * 2 - q;
  if p > 65535 then
    p      := 65535;
  result.red   := ComputeColor(p, q, c.hue + deg120);
  result.green := ComputeColor(p, q, c.hue);
  result.blue  := ComputeColor(p, q, c.hue + deg240);
  result.alpha := c.alpha;
end;

{ Conversion from HSL colorspace to RGB. See : http://en.wikipedia.org/wiki/HSL_color_space }
function HSLAToBGRA(c: THSLAPixel): TBGRAPixel;
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

function GammaCompression(ec: TExpandedPixel): TBGRAPixel;
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
    f1,f2: word;
    f12: longword;
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
  begin
    f1 := c1.alpha*weight1 shr 1;
    f2 := c2.alpha*weight2 shr 1;
    f12 := f1+f2;
    if f12 = 0 then
      result := BGRAPixelTransparent
    else
    begin
      Result.red   := GammaCompressionTab[(GammaExpansionTab[c1.red] * f1 + GammaExpansionTab[c2.red] * f2) div f12];
      Result.green := GammaCompressionTab[(GammaExpansionTab[c1.green] * f1 + GammaExpansionTab[c2.green] * f2) div f12];
      Result.blue  := GammaCompressionTab[(GammaExpansionTab[c1.blue] * f1 + GammaExpansionTab[c2.blue] * f2) div f12];
      Result.alpha := (c1.alpha*weight1+c2.alpha*weight2 + ((weight1+weight2) shr 1)) div (weight1+weight2);
    end;
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

{ Write a color in hexadecimal format RRGGBBAA }
function BGRAToStr(c: TBGRAPixel): string;
begin
  result := IntToHex(c.red,2)+IntToHex(c.green,2)+IntToHex(c.Blue,2)+IntToHex(c.Alpha,2);
end;

type
    arrayOfString = array of string;

function SimpleParseFuncParam(str: string): arrayOfString;
var idxOpen,start,cur: integer;
begin
    result := nil;
    idxOpen := pos('(',str);
    if idxOpen = 0 then exit;
    start := idxOpen+1;
    cur := start;
    while cur <= length(str) do
    begin
       if str[cur] in[',',')'] then
       begin
         setlength(result,length(result)+1);
         result[high(result)] := copy(str,start,cur-start);
         start := cur+1;
       end;
       inc(cur);
    end;
    if start <= length(str) then
    begin
      setlength(result,length(result)+1);
      result[high(result)] := copy(str,start,length(str)-start+1);
    end;
end;

function ParseColorValue(str: string): byte;
var pourcent,unclipped,{%H-}errPos: integer;
begin
  if str = '' then result := 0 else
  begin
    if str[length(str)]='%' then
    begin
      val(copy(str,1,length(str)-1),pourcent,errPos);
      if pourcent < 0 then result := 0 else
      if pourcent > 100 then result := 255 else
        result := pourcent*255 div 100;
    end else
    begin
      val(str,unclipped,errPos);
      if unclipped < 0 then result := 0 else
      if unclipped > 255 then result := 255 else
        result := unclipped;
    end;
  end;
end;

function StrToBGRA(str: string): TBGRAPixel;
begin
  result := StrToBGRA(str, BGRAPixelTransparent);
end;

{ Read a color in hexadecimal format RRGGBB(AA) or RGB(A) }
function StrToBGRA(str: string; DefaultColor: TBGRAPixel): TBGRAPixel;
var errPos: integer;
    values: array of string;
    alphaF: single;
    idx: integer;
begin
  if str = '' then
  begin
    result := DefaultColor;
    exit;
  end;
  str := lowerCase(str);

  //VGA color names
  if str='black' then result := BGRA(0,0,0) else
  if str='silver' then result := BGRA(192,192,192) else
  if str='gray' then result := BGRA(128,128,128) else
  if str='grey' then result := BGRA(128,128,128) else
  if str='white' then result := BGRA(255,255,255) else
  if str='maroon' then result := BGRA(128,0,0) else
  if str='red' then result := BGRA(255,0,0) else
  if str='purple' then result := BGRA(128,0,128) else
  if str='fuchsia' then result := BGRA(255,0,255) else
  if str='green' then result := BGRA(0,128,0) else
  if str='lime' then result := BGRA(0,255,0) else
  if str='olive' then result := BGRA(128,128,0) else
  if str='yellow' then result := BGRA(255,255,0) else
  if str='navy' then result := BGRA(0,0,128) else
  if str='blue' then result := BGRA(0,0,255) else
  if str='teal' then result := BGRA(0,128,128) else
  if str='aqua' then result := BGRA(0,255,255) else
  if str='transparent' then result := DefaultColor else
  begin
    //check CSS color
    idx := CSSColors.IndexOf(str);
    if idx <> -1 then
    begin
      result := CSSColors[idx];
      exit;
    end;

    //CSS RGB notation
    if (copy(str,1,4)='rgb(') or (copy(str,1,5)='rgba(') then
    begin
      values := SimpleParseFuncParam(str);
      if (length(values)=3) or (length(values)=4) then
      begin
        result.red := ParseColorValue(values[0]);
        result.green := ParseColorValue(values[1]);
        result.blue := ParseColorValue(values[2]);
        if length(values)=4 then
        begin
          val(values[3],alphaF,errPos);
          if alphaF < 0 then
            result.alpha := 0 else
          if alphaF > 1 then
            result.alpha := 255
          else
            result.alpha := round(alphaF*255);
        end else
          result.alpha := 255;
      end else
        result := DefaultColor;
      exit;
    end;

    //remove HTML notation header
    if str[1]='#' then delete(str,1,1);

    //add alpha if missing
    if length(str)=6 then str += 'FF';
    if length(str)=3 then str += 'F';

    //hex notation
    if length(str)=8 then
    begin
      val('$'+copy(str,1,2),result.red,errPos);
      if errPos <> 0 then
      begin
        result := DefaultColor;
        exit;
      end;
      val('$'+copy(str,3,2),result.green,errPos);
      if errPos <> 0 then
      begin
        result := DefaultColor;
        exit;
      end;
      val('$'+copy(str,5,2),result.blue,errPos);
      if errPos <> 0 then
      begin
        result := DefaultColor;
        exit;
      end;
      val('$'+copy(str,7,2),result.alpha,errPos);
      if errPos <> 0 then
      begin
        result := DefaultColor;
        exit;
      end;
    end else
    if length(str)=4 then
    begin
      val('$'+copy(str,1,1),result.red,errPos);
      if errPos <> 0 then
      begin
        result := DefaultColor;
        exit;
      end;
      val('$'+copy(str,2,1),result.green,errPos);
      if errPos <> 0 then
      begin
        result := DefaultColor;
        exit;
      end;
      val('$'+copy(str,3,1),result.blue,errPos);
      if errPos <> 0 then
      begin
        result := DefaultColor;
        exit;
      end;
      val('$'+copy(str,4,1),result.alpha,errPos);
      if errPos <> 0 then
      begin
        result := DefaultColor;
        exit;
      end;
      result.red *= $11;
      result.green *= $11;
      result.blue *= $11;
      result.alpha *= $11;
    end else
      result := DefaultColor;
  end;

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

function PtInRect(pt: TPoint; r: TRect): boolean;
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
  result := Sin65536(value+16384); //cosine is translated
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

initialization

  InitGamma;
  CSSColors := TBGRAColorList.Create;
  CSSColors.Add('AliceBlue',CSSAliceBlue);
  CSSColors.Add('AntiqueWhite',CSSAntiqueWhite);
  CSSColors.Add('Aqua',CSSAqua);
  CSSColors.Add('Aquamarine',CSSAquamarine);
  CSSColors.Add('Azure',CSSAzure);
  CSSColors.Add('Beige',CSSBeige);
  CSSColors.Add('Bisque',CSSBisque);
  CSSColors.Add('Black',CSSBlack);
  CSSColors.Add('BlanchedAlmond',CSSBlanchedAlmond);
  CSSColors.Add('Blue',CSSBlue);
  CSSColors.Add('BlueViolet',CSSBlueViolet);
  CSSColors.Add('Brown',CSSBrown);
  CSSColors.Add('BurlyWood',CSSBurlyWood);
  CSSColors.Add('CadetBlue',CSSCadetBlue);
  CSSColors.Add('Chartreuse',CSSChartreuse);
  CSSColors.Add('Chocolate',CSSChocolate);
  CSSColors.Add('Coral',CSSCoral);
  CSSColors.Add('CornflowerBlue',CSSCornflowerBlue);
  CSSColors.Add('Cornsilk',CSSCornsilk);
  CSSColors.Add('Crimson',CSSCrimson);
  CSSColors.Add('Cyan',CSSCyan);
  CSSColors.Add('DarkBlue',CSSDarkBlue);
  CSSColors.Add('DarkCyan',CSSDarkCyan);
  CSSColors.Add('DarkGoldenrod',CSSDarkGoldenrod);
  CSSColors.Add('DarkGray',CSSDarkGray);
  CSSColors.Add('DarkGreen',CSSDarkGreen);
  CSSColors.Add('DarkKhaki',CSSDarkKhaki);
  CSSColors.Add('DarkMagenta',CSSDarkMagenta);
  CSSColors.Add('DarkOliveGreen',CSSDarkOliveGreen);
  CSSColors.Add('DarkOrange',CSSDarkOrange);
  CSSColors.Add('DarkOrchid',CSSDarkOrchid);
  CSSColors.Add('DarkRed',CSSDarkRed);
  CSSColors.Add('DarkSalmon',CSSDarkSalmon);
  CSSColors.Add('DarkSeaGreen',CSSDarkSeaGreen);
  CSSColors.Add('DarkSlateBlue',CSSDarkSlateBlue);
  CSSColors.Add('DarkSlateGray',CSSDarkSlateGray);
  CSSColors.Add('DarkTurquoise',CSSDarkTurquoise);
  CSSColors.Add('DarkViolet',CSSDarkViolet);
  CSSColors.Add('DeepPink',CSSDeepPink);
  CSSColors.Add('DeepSkyBlue',CSSDeepSkyBlue);
  CSSColors.Add('DimGray',CSSDimGray);
  CSSColors.Add('DodgerBlue',CSSDodgerBlue);
  CSSColors.Add('FireBrick',CSSFireBrick);
  CSSColors.Add('FloralWhite',CSSFloralWhite);
  CSSColors.Add('ForestGreen',CSSForestGreen);
  CSSColors.Add('Fuchsia',CSSFuchsia);
  CSSColors.Add('Gainsboro',CSSGainsboro);
  CSSColors.Add('GhostWhite',CSSGhostWhite);
  CSSColors.Add('Gold',CSSGold);
  CSSColors.Add('Goldenrod',CSSGoldenrod);
  CSSColors.Add('Gray',CSSGray);
  CSSColors.Add('Green',CSSGreen);
  CSSColors.Add('GreenYellow',CSSGreenYellow);
  CSSColors.Add('Honeydew',CSSHoneydew);
  CSSColors.Add('HotPink',CSSHotPink);
  CSSColors.Add('IndianRed',CSSIndianRed);
  CSSColors.Add('Indigo',CSSIndigo);
  CSSColors.Add('Ivory',CSSIvory);
  CSSColors.Add('Khaki',CSSKhaki);
  CSSColors.Add('Lavender',CSSLavender);
  CSSColors.Add('LavenderBlush',CSSLavenderBlush);
  CSSColors.Add('LawnGreen',CSSLawnGreen);
  CSSColors.Add('LemonChiffon',CSSLemonChiffon);
  CSSColors.Add('LightBlue',CSSLightBlue);
  CSSColors.Add('LightCoral',CSSLightCoral);
  CSSColors.Add('LightCyan',CSSLightCyan);
  CSSColors.Add('LightGoldenrodYellow',CSSLightGoldenrodYellow);
  CSSColors.Add('LightGray',CSSLightGray);
  CSSColors.Add('LightGreen',CSSLightGreen);
  CSSColors.Add('LightPink',CSSLightPink);
  CSSColors.Add('LightSalmon',CSSLightSalmon);
  CSSColors.Add('LightSeaGreen',CSSLightSeaGreen);
  CSSColors.Add('LightSkyBlue',CSSLightSkyBlue);
  CSSColors.Add('LightSlateGray',CSSLightSlateGray);
  CSSColors.Add('LightSteelBlue',CSSLightSteelBlue);
  CSSColors.Add('LightYellow',CSSLightYellow);
  CSSColors.Add('Lime',CSSLime);
  CSSColors.Add('LimeGreen',CSSLimeGreen);
  CSSColors.Add('Linen',CSSLinen);
  CSSColors.Add('Magenta',CSSMagenta);
  CSSColors.Add('Maroon',CSSMaroon);
  CSSColors.Add('MediumAquamarine',CSSMediumAquamarine);
  CSSColors.Add('MediumBlue',CSSMediumBlue);
  CSSColors.Add('MediumOrchid',CSSMediumOrchid);
  CSSColors.Add('MediumPurple',CSSMediumPurple);
  CSSColors.Add('MediumSeaGreen',CSSMediumSeaGreen);
  CSSColors.Add('MediumSlateBlue',CSSMediumSlateBlue);
  CSSColors.Add('MediumSpringGreen',CSSMediumSpringGreen);
  CSSColors.Add('MediumTurquoise',CSSMediumTurquoise);
  CSSColors.Add('MediumVioletRed',CSSMediumVioletRed);
  CSSColors.Add('MidnightBlue',CSSMidnightBlue);
  CSSColors.Add('MintCream',CSSMintCream);
  CSSColors.Add('MistyRose',CSSMistyRose);
  CSSColors.Add('Moccasin',CSSMoccasin);
  CSSColors.Add('NavajoWhite',CSSNavajoWhite);
  CSSColors.Add('Navy',CSSNavy);
  CSSColors.Add('OldLace',CSSOldLace);
  CSSColors.Add('Olive',CSSOlive);
  CSSColors.Add('OliveDrab',CSSOliveDrab);
  CSSColors.Add('Orange',CSSOrange);
  CSSColors.Add('OrangeRed',CSSOrangeRed);
  CSSColors.Add('Orchid',CSSOrchid);
  CSSColors.Add('PaleGoldenrod',CSSPaleGoldenrod);
  CSSColors.Add('PaleGreen',CSSPaleGreen);
  CSSColors.Add('PaleTurquoise',CSSPaleTurquoise);
  CSSColors.Add('PaleVioletRed',CSSPaleVioletRed);
  CSSColors.Add('PapayaWhip',CSSPapayaWhip);
  CSSColors.Add('PeachPuff',CSSPeachPuff);
  CSSColors.Add('Peru',CSSPeru);
  CSSColors.Add('Pink',CSSPink);
  CSSColors.Add('Plum',CSSPlum);
  CSSColors.Add('PowderBlue',CSSPowderBlue);
  CSSColors.Add('Purple',CSSPurple);
  CSSColors.Add('Red',CSSRed);
  CSSColors.Add('RosyBrown',CSSRosyBrown);
  CSSColors.Add('RoyalBlue',CSSRoyalBlue);
  CSSColors.Add('SaddleBrown',CSSSaddleBrown);
  CSSColors.Add('Salmon',CSSSalmon);
  CSSColors.Add('SandyBrown',CSSSandyBrown);
  CSSColors.Add('SeaGreen',CSSSeaGreen);
  CSSColors.Add('Seashell',CSSSeashell);
  CSSColors.Add('Sienna',CSSSienna);
  CSSColors.Add('Silver',CSSSilver);
  CSSColors.Add('SkyBlue',CSSSkyBlue);
  CSSColors.Add('SlateBlue',CSSSlateBlue);
  CSSColors.Add('SlateGray',CSSSlateGray);
  CSSColors.Add('Snow',CSSSnow);
  CSSColors.Add('SpringGreen',CSSSpringGreen);
  CSSColors.Add('SteelBlue',CSSSteelBlue);
  CSSColors.Add('Tan',CSSTan);
  CSSColors.Add('Teal',CSSTeal);
  CSSColors.Add('Thistle',CSSThistle);
  CSSColors.Add('Tomato',CSSTomato);
  CSSColors.Add('Turquoise',CSSTurquoise);
  CSSColors.Add('Violet',CSSViolet);
  CSSColors.Add('Wheat',CSSWheat);
  CSSColors.Add('White',CSSWhite);
  CSSColors.Add('WhiteSmoke',CSSWhiteSmoke);
  CSSColors.Add('Yellow',CSSYellow);
  CSSColors.Add('YellowGreen',CSSYellowGreen);
  CSSColors.Finished;

finalization

  CSSColors.Free;

end.
