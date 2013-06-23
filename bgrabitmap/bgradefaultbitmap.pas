{
 /**************************************************************************\
                             bgradefaultbitmap.pas
                             ---------------------
                 This unit defines basic operations on bitmaps.
                 It should NOT be added to the 'uses' clause.
                 Some operations may be slow, so there are
                 accelerated versions for some routines.

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

unit BGRADefaultBitmap;

{$mode objfpc}{$H+}

interface

{ This unit contains TBGRADefaultBitmap class. This class contains basic drawing routines,
  and call functions from other units to perform advanced drawing functions. }

uses
  Classes, SysUtils, Types, FPImage, Graphics, BGRABitmapTypes, GraphType, FPImgCanv, BGRACanvas, BGRACanvas2D, FPWritePng;

type
  { TBGRADefaultBitmap }

  TBGRADefaultBitmap = class(TBGRACustomBitmap)
  private
    { Bounds checking which are shared by drawing functions. These functions check
      if the coordinates are visible and return true if it is the case, swap
      coordinates if necessary and make them fit into the clipping rectangle }
    function CheckHorizLineBounds(var x, y, x2: integer): boolean; inline;
    function CheckVertLineBounds(var x, y, y2: integer; out delta: integer): boolean; inline;
    function CheckRectBounds(var x,y,x2,y2: integer; minsize: integer): boolean; inline;
    function CheckClippedRectBounds(var x,y,x2,y2: integer): boolean; inline;
    function CheckPutImageBounds(x, y, tx, ty: integer; out minxb, minyb, maxxb, maxyb, ignoreleft: integer): boolean; inline;
    function CheckAntialiasRectBounds(var x,y,x2,y2: single; w: single): boolean;
    function GetCanvasBGRA: TBGRACanvas;
    function GetCanvas2D: TBGRACanvas2D;
  protected
    FRefCount: integer; //reference counter (not related to interface reference counter)

    //Pixel data
    FData:      PBGRAPixel;              //pointer to pixels
    FWidth, FHeight, FNbPixels: integer; //dimensions
    FDataModified: boolean;              //if data image has changed so TBitmap should be updated
    FLineOrder: TRawImageLineOrder;
    FClipRect:  TRect;                   //clipping (can be the whole image if there is no clipping)

    //Scan
    FScanPtr : PBGRAPixel;          //current scan address
    FScanCurX,FScanCurY: integer;   //current scan coordinates

    //LCL bitmap object
    FBitmap:   TBitmap;
    FBitmapModified: boolean;         //if TBitmap has changed so pixel data should be updated
    FCanvasOpacity: byte;             //opacity used with standard canvas functions
    FAlphaCorrectionNeeded: boolean;  //the alpha channel is not correct because standard functions do not
                                      //take it into account

    //FreePascal drawing routines
    FCanvasFP: TFPImageCanvas;
    FCanvasDrawModeFP: TDrawMode;
    FCanvasPixelProcFP: procedure(x, y: integer; col: TBGRAPixel) of object;

    //canvas-like with antialiasing and texturing
    FCanvasBGRA: TBGRACanvas;
    FCanvas2D: TBGRACanvas2D;

    //drawing options
    FEraseMode: boolean;      //when polygons are erased instead of drawn
    FFont: TFont;             //font parameters
    FFontHeight: integer;
    FFontHeightSign: integer; //sign correction

    { Pen style can be defined by PenStyle property of by CustomPenStyle property.
      When PenStyle property is assigned, CustomPenStyle property is assigned the actual
      pen pattern. }
    FCustomPenStyle:  TBGRAPenStyle;
    FPenStyle: TPenStyle;

    //Pixel data
    function GetRefCount: integer; override;
    function GetScanLine(y: integer): PBGRAPixel; override; //don't forget to call InvalidateBitmap after modifications
    function LoadFromRawImage(ARawImage: TRawImage; DefaultOpacity: byte;
      AlwaysReplaceAlpha: boolean = False; RaiseErrorOnInvalidPixelFormat: boolean = True): boolean;
    function GetDataPtr: PBGRAPixel; override;
    procedure ClearTransparentPixels;
    function GetScanlineFast(y: integer): PBGRAPixel; inline;
    function GetLineOrder: TRawImageLineOrder; override;
    function GetNbPixels: integer; override;
    function GetWidth: integer; override;
    function GetHeight: integer; override;

    //LCL bitmap object
    function GetBitmap: TBitmap; override;
    function GetCanvas: TCanvas; override;
    procedure DiscardBitmapChange; inline;
    procedure DoAlphaCorrection;
    procedure SetCanvasOpacity(AValue: byte); override;
    function GetCanvasOpacity: byte; override;
    function GetCanvasAlphaCorrection: boolean; override;
    procedure SetCanvasAlphaCorrection(const AValue: boolean); override;

    //FreePascal drawing routines
    function GetCanvasFP: TFPImageCanvas; override;
    procedure SetCanvasDrawModeFP(const AValue: TDrawMode); override;
    function GetCanvasDrawModeFP: TDrawMode; override;

    {Allocation routines}
    procedure ReallocData; virtual;
    procedure FreeData; virtual;

    procedure RebuildBitmap; virtual;
    procedure FreeBitmap; virtual;

    procedure Init; virtual;

    {TFPCustomImage}
    procedure SetInternalColor(x, y: integer; const Value: TFPColor); override;
    function GetInternalColor(x, y: integer): TFPColor; override;
    procedure SetInternalPixel(x, y: integer; Value: integer); override;
    function GetInternalPixel(x, y: integer): integer; override;

    {Image functions}
    function FineResample(NewWidth, NewHeight: integer): TBGRACustomBitmap;
    function SimpleStretch(NewWidth, NewHeight: integer): TBGRACustomBitmap;
    function CheckEmpty: boolean; override;
    function GetHasTransparentPixels: boolean; override;
    function GetAverageColor: TColor; override;
    function GetAveragePixel: TBGRAPixel; override;
    function CreateAdaptedPngWriter: TFPWriterPNG;
    function LoadAsBmp32(Str: TStream): boolean; override;

    //drawing
    function GetCustomPenStyle: TBGRAPenStyle; override;
    procedure SetCustomPenStyle(const AValue: TBGRAPenStyle); override;
    procedure SetPenStyle(const AValue: TPenStyle); override;
    function GetPenStyle: TPenStyle; override;

    procedure UpdateFont;
    function GetFontHeight: integer; override;
    procedure SetFontHeight(AHeight: integer); override;
    function GetFontFullHeight: integer; override;
    procedure SetFontFullHeight(AHeight: integer); override;
    function GetFontPixelMetric: TFontPixelMetric; override;

    function GetClipRect: TRect; override;
    procedure SetClipRect(const AValue: TRect); override;

    function GetPixelCycleInline(ix,iy: integer; iFactX,iFactY: integer): TBGRAPixel; inline;

  public
    {Reference counter functions}
    function NewReference: TBGRACustomBitmap;
    procedure FreeReference;
    function GetUnique: TBGRACustomBitmap;

    {TFPCustomImage override}
    constructor Create(AWidth, AHeight: integer); override;
    procedure SetSize(AWidth, AHeight: integer); override;

    {Constructors}
    constructor Create; override;
    constructor Create(ABitmap: TBitmap); override;
    constructor Create(AWidth, AHeight: integer; Color: TColor); override;
    constructor Create(AWidth, AHeight: integer; Color: TBGRAPixel); override;
    constructor Create(AFilename: string); override;
    constructor Create(AStream: TStream); override;
    destructor Destroy; override;

    {Loading functions}
    function NewBitmap(AWidth, AHeight: integer): TBGRACustomBitmap; override;
    function NewBitmap(AWidth, AHeight: integer; Color: TBGRAPixel): TBGRACustomBitmap; override;
    function NewBitmap(Filename: string): TBGRACustomBitmap; override;

    procedure LoadFromFile(const filename: string); override;
    procedure SaveToFile(const filename: string); override;
    procedure SaveToStreamAsPng(Str: TStream); override;
    procedure Assign(ABitmap: TBitmap); override; overload;
    procedure Assign(MemBitmap: TBGRACustomBitmap);override; overload;
    procedure Serialize(AStream: TStream); override;
    procedure Deserialize(AStream: TStream); override;
    class procedure SerializeEmpty(AStream: TStream);

    {Pixel functions}
    function PtInClipRect(x, y: integer): boolean; inline;
    procedure SetPixel(x, y: integer; c: TColor); override;
    procedure SetPixel(x, y: integer; c: TBGRAPixel); override;
    procedure XorPixel(x, y: integer; c: TBGRAPixel); override;
    procedure DrawPixel(x, y: integer; c: TBGRAPixel); override;
    procedure DrawPixel(x, y: integer; ec: TExpandedPixel); override;
    procedure FastBlendPixel(x, y: integer; c: TBGRAPixel); override;
    procedure ErasePixel(x, y: integer; alpha: byte); override;
    procedure AlphaPixel(x, y: integer; alpha: byte); override;
    function GetPixel(x, y: integer): TBGRAPixel; override;
    function GetPixel(x, y: single; AResampleFilter: TResampleFilter = rfLinear): TBGRAPixel; override;
    function GetPixelCycle(x, y: single; AResampleFilter: TResampleFilter = rfLinear): TBGRAPixel; override;
    function GetPixelCycle(x, y: single; AResampleFilter: TResampleFilter;
      repeatX: boolean; repeatY: boolean): TBGRAPixel; override; overload;

    {Line primitives}
    procedure SetHorizLine(x, y, x2: integer; c: TBGRAPixel); override;
    procedure XorHorizLine(x, y, x2: integer; c: TBGRAPixel); override;
    procedure DrawHorizLine(x, y, x2: integer; c: TBGRAPixel); override;
    procedure DrawHorizLine(x, y, x2: integer; ec: TExpandedPixel); override;
    procedure DrawHorizLine(x, y, x2: integer; texture: IBGRAScanner); override;
    procedure FastBlendHorizLine(x, y, x2: integer; c: TBGRAPixel); override;
    procedure AlphaHorizLine(x, y, x2: integer; alpha: byte); override;
    procedure SetVertLine(x, y, y2: integer; c: TBGRAPixel); override;
    procedure XorVertLine(x, y, y2: integer; c: TBGRAPixel); override;
    procedure DrawVertLine(x, y, y2: integer; c: TBGRAPixel); override;
    procedure AlphaVertLine(x, y, y2: integer; alpha: byte); override;
    procedure FastBlendVertLine(x, y, y2: integer; c: TBGRAPixel); override;
    procedure DrawHorizLineDiff(x, y, x2: integer; c, compare: TBGRAPixel;
      maxDiff: byte); override;

    {Shapes}
    procedure DrawLine(x1, y1, x2, y2: integer; c: TBGRAPixel; DrawLastPixel: boolean); override;
    procedure DrawLineAntialias(x1, y1, x2, y2: integer; c: TBGRAPixel; DrawLastPixel: boolean); override;
    procedure DrawLineAntialias(x1, y1, x2, y2: integer; c1, c2: TBGRAPixel; dashLen: integer; DrawLastPixel: boolean); override;
    procedure DrawLineAntialias(x1, y1, x2, y2: integer; c1, c2: TBGRAPixel; dashLen: integer; DrawLastPixel: boolean; var DashPos: integer); override;
    procedure DrawLineAntialias(x1, y1, x2, y2: single; c: TBGRAPixel; w: single); override;
    procedure DrawLineAntialias(x1, y1, x2, y2: single; texture: IBGRAScanner; w: single); override;
    procedure DrawLineAntialias(x1, y1, x2, y2: single; c: TBGRAPixel; w: single; Closed: boolean); override;
    procedure DrawLineAntialias(x1, y1, x2, y2: single; texture: IBGRAScanner; w: single; Closed: boolean); override;

    procedure DrawPolyLineAntialias(const points: array of TPointF; c: TBGRAPixel; w: single); override;
    procedure DrawPolyLineAntialias(const points: array of TPointF; texture: IBGRAScanner; w: single); override;
    procedure DrawPolyLineAntialias(const points: array of TPointF; c: TBGRAPixel; w: single; Closed: boolean); override;
    procedure DrawPolygonAntialias(const points: array of TPointF; c: TBGRAPixel; w: single); override;
    procedure DrawPolygonAntialias(const points: array of TPointF; texture: IBGRAScanner; w: single); override;

    procedure EraseLine(x1, y1, x2, y2: integer; alpha: byte; DrawLastPixel: boolean); override;
    procedure EraseLineAntialias(x1, y1, x2, y2: integer; alpha: byte; DrawLastPixel: boolean); override;
    procedure EraseLineAntialias(x1, y1, x2, y2: single; alpha: byte; w: single); override;
    procedure EraseLineAntialias(x1, y1, x2, y2: single; alpha: byte; w: single; Closed: boolean); override;
    procedure ErasePolyLineAntialias(const points: array of TPointF; alpha: byte; w: single); override;

    procedure FillTriangleLinearColor(pt1,pt2,pt3: TPointF; c1,c2,c3: TBGRAPixel); override;
    procedure FillTriangleLinearColorAntialias(pt1,pt2,pt3: TPointF; c1,c2,c3: TBGRAPixel); override;
    procedure FillTriangleLinearMapping(pt1,pt2,pt3: TPointF; texture: IBGRAScanner; tex1, tex2, tex3: TPointF; TextureInterpolation: Boolean= True); override;
    procedure FillTriangleLinearMappingLightness(pt1,pt2,pt3: TPointF; texture: IBGRAScanner; tex1, tex2, tex3: TPointF; light1,light2,light3: word; TextureInterpolation: Boolean= True); override;
    procedure FillTriangleLinearMappingAntialias(pt1,pt2,pt3: TPointF; texture: IBGRAScanner; tex1, tex2, tex3: TPointF); override;

    procedure FillQuadLinearColor(pt1,pt2,pt3,pt4: TPointF; c1,c2,c3,c4: TBGRAPixel); override;
    procedure FillQuadLinearColorAntialias(pt1,pt2,pt3,pt4: TPointF; c1,c2,c3,c4: TBGRAPixel); override;
    procedure FillQuadLinearMapping(pt1,pt2,pt3,pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF; TextureInterpolation: Boolean= True); override;
    procedure FillQuadLinearMappingLightness(pt1,pt2,pt3,pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF; light1,light2,light3,light4: word; TextureInterpolation: Boolean= True); override;
    procedure FillQuadLinearMappingAntialias(pt1,pt2,pt3,pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF); override;
    procedure FillQuadPerspectiveMapping(pt1,pt2,pt3,pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF); override;
    procedure FillQuadPerspectiveMappingAntialias(pt1,pt2,pt3,pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF); override;

    procedure FillPolyLinearMapping(const points: array of TPointF; texture: IBGRAScanner; texCoords: array of TPointF; TextureInterpolation: Boolean); override;
    procedure FillPolyLinearMappingLightness(const points: array of TPointF; texture: IBGRAScanner; texCoords: array of TPointF; lightnesses: array of word; TextureInterpolation: Boolean); override;
    procedure FillPolyLinearColor(const points: array of TPointF; AColors: array of TBGRAPixel); override;
    procedure FillPolyPerspectiveMapping(const points: array of TPointF; const pointsZ: array of single; texture: IBGRAScanner; texCoords: array of TPointF; TextureInterpolation: Boolean; zbuffer: psingle = nil); override;
    procedure FillPolyPerspectiveMappingLightness(const points: array of TPointF; const pointsZ: array of single; texture: IBGRAScanner; texCoords: array of TPointF; lightnesses: array of word; TextureInterpolation: Boolean; zbuffer: psingle = nil); override;
    procedure FillPoly(const points: array of TPointF; c: TBGRAPixel; drawmode: TDrawMode); override;
    procedure FillPoly(const points: array of TPointF; texture: IBGRAScanner; drawmode: TDrawMode); override;
    procedure FillPolyAntialias(const points: array of TPointF; c: TBGRAPixel); override;
    procedure FillPolyAntialias(const points: array of TPointF; texture: IBGRAScanner); override;
    procedure ErasePoly(const points: array of TPointF; alpha: byte); override;
    procedure ErasePolyAntialias(const points: array of TPointF; alpha: byte); override;

    procedure EllipseAntialias(x, y, rx, ry: single; c: TBGRAPixel; w: single); override;
    procedure EllipseAntialias(x, y, rx, ry: single; texture: IBGRAScanner; w: single); override;
    procedure EllipseAntialias(x, y, rx, ry: single; c: TBGRAPixel; w: single; back: TBGRAPixel); override;
    procedure FillEllipseAntialias(x, y, rx, ry: single; c: TBGRAPixel); override;
    procedure FillEllipseAntialias(x, y, rx, ry: single; texture: IBGRAScanner); override;
    procedure FillEllipseLinearColorAntialias(x, y, rx, ry: single; outercolor, innercolor: TBGRAPixel); override;
    procedure EraseEllipseAntialias(x, y, rx, ry: single; alpha: byte); override;

    procedure Rectangle(x, y, x2, y2: integer; c: TBGRAPixel; mode: TDrawMode); override;
    procedure Rectangle(x, y, x2, y2: integer; BorderColor, FillColor: TBGRAPixel; mode: TDrawMode); override;
    procedure RectangleAntialias(x, y, x2, y2: single; c: TBGRAPixel; w: single; back: TBGRAPixel); override;
    procedure RectangleAntialias(x, y, x2, y2: single; texture: IBGRAScanner; w: single); override;

    procedure RoundRectAntialias(x,y,x2,y2,rx,ry: single; c: TBGRAPixel; w: single; options: TRoundRectangleOptions = []); override;
    procedure RoundRectAntialias(x,y,x2,y2,rx,ry: single; texture: IBGRAScanner; w: single; options: TRoundRectangleOptions = []); override;
    procedure RoundRectAntialias(x,y,x2,y2,rx,ry: single; pencolor: TBGRAPixel; w: single; fillcolor: TBGRAPixel; options: TRoundRectangleOptions = []); override;
    procedure RoundRectAntialias(x,y,x2,y2,rx,ry: single; penTexture: IBGRAScanner; w: single; fillTexture: IBGRAScanner; options: TRoundRectangleOptions = []); override;

    procedure FillRect(x, y, x2, y2: integer; c: TBGRAPixel; mode: TDrawMode); override;
    procedure FillRect(x, y, x2, y2: integer; texture: IBGRAScanner; mode: TDrawMode); override;
    procedure FillRectAntialias(x, y, x2, y2: single; c: TBGRAPixel); override;
    procedure EraseRectAntialias(x, y, x2, y2: single; alpha: byte); override;
    procedure FillRectAntialias(x, y, x2, y2: single; texture: IBGRAScanner); override;
    procedure FillRoundRectAntialias(x,y,x2,y2,rx,ry: single; c: TBGRAPixel; options: TRoundRectangleOptions = []); override;
    procedure FillRoundRectAntialias(x,y,x2,y2,rx,ry: single; texture: IBGRAScanner; options: TRoundRectangleOptions = []); override;
    procedure EraseRoundRectAntialias(x,y,x2,y2,rx,ry: single; alpha: byte; options: TRoundRectangleOptions = []); override;
    procedure AlphaFillRect(x, y, x2, y2: integer; alpha: byte); override;
    procedure RoundRect(X1, Y1, X2, Y2: integer; DX, DY: integer;
      BorderColor, FillColor: TBGRAPixel); override;

    procedure TextOutAngle(x, y: single; orientation: integer; s: string; c: TBGRAPixel; align: TAlignment); override;
    procedure TextOutAngle(x, y: single; orientation: integer; s: string; texture: IBGRAScanner; align: TAlignment); override;
    procedure TextOut(x, y: single; s: string; texture: IBGRAScanner; align: TAlignment); override;
    procedure TextOut(x, y: single; s: string; c: TBGRAPixel; align: TAlignment); override;
    procedure TextRect(ARect: TRect; x, y: integer; s: string; style: TTextStyle; c: TBGRAPixel); override;
    procedure TextRect(ARect: TRect; x, y: integer; s: string; style: TTextStyle; texture: IBGRAScanner); override;
    function TextSize(s: string): TSize; override;

    {Spline}
    function ComputeClosedSpline(const APoints: array of TPointF; AStyle: TSplineStyle): ArrayOfTPointF; override;
    function ComputeOpenedSpline(const APoints: array of TPointF; AStyle: TSplineStyle): ArrayOfTPointF; override;

    function ComputeBezierCurve(const ACurve: TCubicBezierCurve): ArrayOfTPointF; override;
    function ComputeBezierCurve(const ACurve: TQuadraticBezierCurve): ArrayOfTPointF; override;
    function ComputeBezierSpline(const ASpline: array of TCubicBezierCurve): ArrayOfTPointF; override;
    function ComputeBezierSpline(const ASpline: array of TQuadraticBezierCurve): ArrayOfTPointF; override;

    function ComputeWidePolyline(const points: array of TPointF; w: single): ArrayOfTPointF; override;
    function ComputeWidePolyline(const points: array of TPointF; w: single; Closed: boolean): ArrayOfTPointF; override;
    function ComputeWidePolygon(const points: array of TPointF; w: single): ArrayOfTPointF; override;

    function ComputeEllipseContour(x,y,rx,ry: single; quality: single = 1): ArrayOfTPointF; override;
    function ComputeEllipseBorder(x,y,rx,ry,w: single; quality: single = 1): ArrayOfTPointF; override;
    function ComputeArc65536(x,y,rx,ry: single; start65536,end65536: word; quality: single = 1): ArrayOfTPointF; override;
    function ComputeArcRad(x,y,rx,ry: single; startRad,endRad: single; quality: single = 1): ArrayOfTPointF; override;
    function ComputeRoundRect(x1,y1,x2,y2,rx,ry: single; quality: single = 1): ArrayOfTPointF; override;
    function ComputeRoundRect(x1,y1,x2,y2,rx,ry: single; options: TRoundRectangleOptions; quality: single = 1): ArrayOfTPointF; override;
    function ComputePie65536(x,y,rx,ry: single; start65536,end65536: word; quality: single = 1): ArrayOfTPointF; override;
    function ComputePieRad(x,y,rx,ry: single; startRad,endRad: single; quality: single = 1): ArrayOfTPointF; override;

    {Filling}
    procedure NoClip; override;
    procedure Fill(texture: IBGRAScanner; mode: TDrawMode); override;
    procedure Fill(texture: IBGRAScanner); override;
    procedure Fill(c: TBGRAPixel; start, Count: integer); override;
    procedure DrawPixels(c: TBGRAPixel; start, Count: integer); override;
    procedure AlphaFill(alpha: byte; start, Count: integer); override;
    procedure FillMask(x,y: integer; AMask: TBGRACustomBitmap; color: TBGRAPixel); override;
    procedure FillMask(x,y: integer; AMask: TBGRACustomBitmap; texture: IBGRAScanner); override;
    procedure FillClearTypeMask(x,y: integer; xThird: integer; AMask: TBGRACustomBitmap; color: TBGRAPixel; ARGBOrder: boolean = true); override;
    procedure FillClearTypeMask(x,y: integer; xThird: integer; AMask: TBGRACustomBitmap; texture: IBGRAScanner; ARGBOrder: boolean = true); override;
    procedure ReplaceColor(before, after: TColor); override;
    procedure ReplaceColor(before, after: TBGRAPixel); override;
    procedure ReplaceTransparent(after: TBGRAPixel); override;
    procedure ParallelFloodFill(X, Y: integer; Dest: TBGRACustomBitmap; Color: TBGRAPixel;
      mode: TFloodfillMode; Tolerance: byte = 0); override;
    procedure GradientFill(x, y, x2, y2: integer; c1, c2: TBGRAPixel;
      gtype: TGradientType; o1, o2: TPointF; mode: TDrawMode;
      gammaColorCorrection: boolean = True; Sinus: Boolean=False); override;
    procedure GradientFill(x, y, x2, y2: integer; gradient: TBGRACustomGradient;
      gtype: TGradientType; o1, o2: TPointF; mode: TDrawMode;
      Sinus: Boolean=False); override;
    function CreateBrushTexture(ABrushStyle: TBrushStyle; APatternColor, ABackgroundColor: TBGRAPixel;
                AWidth: integer = 8; AHeight: integer = 8; APenWidth: single = 1): TBGRACustomBitmap; override;
    function ScanAtInteger(X,Y: integer): TBGRAPixel; override;
    procedure ScanMoveTo(X,Y: Integer); override;
    function ScanNextPixel: TBGRAPixel; override;
    function ScanAt(X,Y: Single): TBGRAPixel; override;
    function IsScanPutPixelsDefined: boolean; override;
    procedure ScanPutPixels(pdest: PBGRAPixel; count: integer; mode: TDrawMode); override;

    {Canvas drawing functions}
    procedure DataDrawTransparent(ACanvas: TCanvas; Rect: TRect;
      AData: Pointer; ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer); override;
    procedure DataDrawOpaque(ACanvas: TCanvas; Rect: TRect; AData: Pointer;
      ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer); override;
    procedure GetImageFromCanvas(CanvasSource: TCanvas; x, y: integer); override;
    procedure Draw(ACanvas: TCanvas; x, y: integer; Opaque: boolean = True); override;
    procedure Draw(ACanvas: TCanvas; Rect: TRect; Opaque: boolean = True); override;
    procedure InvalidateBitmap; override;         //call if you modify with Scanline
    procedure LoadFromBitmapIfNeeded; override;   //call to ensure that bitmap data is up to date

    {BGRA bitmap functions}
    procedure PutImage(x, y: integer; Source: TBGRACustomBitmap; mode: TDrawMode; AOpacity: byte = 255); override;
    procedure PutImageAngle(x,y: single; Source: TBGRACustomBitmap; angle: single; imageCenterX: single = 0; imageCenterY: single = 0; AOpacity: Byte=255; ARestoreOffsetAfterRotation: boolean = false); override;
    procedure PutImageAffine(Origin,HAxis,VAxis: TPointF; Source: TBGRACustomBitmap; AOpacity: Byte=255); override;
    procedure BlendImage(x, y: integer; Source: TBGRACustomBitmap; operation: TBlendOperation); override;
    procedure BlendImageOver(x, y: integer; Source: TBGRACustomBitmap; operation: TBlendOperation; AOpacity: byte = 255;
        ALinearBlend: boolean = false); override;

    function GetPart(ARect: TRect): TBGRACustomBitmap; override;
    function GetPtrBitmap(Top,Bottom: Integer): TBGRACustomBitmap; override;
    function Duplicate(DuplicateProperties: Boolean = False) : TBGRACustomBitmap; override;
    procedure CopyPropertiesTo(ABitmap: TBGRADefaultBitmap);
    function Equals(comp: TBGRACustomBitmap): boolean; override;
    function Equals(comp: TBGRAPixel): boolean; override;
    function GetImageBounds(Channel: TChannel = cAlpha; ANothingValue: Byte = 0): TRect; override;
    function GetImageBounds(Channels: TChannels): TRect; override;
    function GetDifferenceBounds(ABitmap: TBGRACustomBitmap): TRect; override;
    function MakeBitmapCopy(BackgroundColor: TColor): TBitmap; override;

    function Resample(newWidth, newHeight: integer;
      mode: TResampleMode = rmFineResample): TBGRACustomBitmap; override;
    procedure VerticalFlip; override;
    procedure HorizontalFlip; override;
    function RotateCW: TBGRACustomBitmap; override;
    function RotateCCW: TBGRACustomBitmap; override;
    procedure Negative; override;
    procedure LinearNegative; override;
    procedure SwapRedBlue; override;
    procedure GrayscaleToAlpha; override;
    procedure AlphaToGrayscale; override;
    procedure ApplyMask(mask: TBGRACustomBitmap); override;
    procedure ApplyGlobalOpacity(alpha: byte); override;
    procedure ConvertToLinearRGB; override;
    procedure ConvertFromLinearRGB; override;

    {Filters}
    function FilterSmartZoom3(Option: TMedianOption): TBGRACustomBitmap; override;
    function FilterMedian(Option: TMedianOption): TBGRACustomBitmap; override;
    function FilterSmooth: TBGRACustomBitmap; override;
    function FilterSharpen: TBGRACustomBitmap; override;
    function FilterContour: TBGRACustomBitmap; override;
    function FilterBlurRadial(radius: integer;
      blurType: TRadialBlurType): TBGRACustomBitmap; override;
    function FilterPixelate(pixelSize: integer; useResample: boolean; filter: TResampleFilter = rfLinear): TBGRACustomBitmap; override;
    function FilterBlurMotion(distance: integer; angle: single;
      oriented: boolean): TBGRACustomBitmap; override;
    function FilterCustomBlur(mask: TBGRACustomBitmap): TBGRACustomBitmap; override;
    function FilterEmboss(angle: single): TBGRACustomBitmap; override;
    function FilterEmbossHighlight(FillSelection: boolean): TBGRACustomBitmap; override;
    function FilterEmbossHighlight(FillSelection: boolean; BorderColor: TBGRAPixel): TBGRACustomBitmap; override;
    function FilterEmbossHighlight(FillSelection: boolean; BorderColor: TBGRAPixel; var Offset: TPoint): TBGRACustomBitmap; override;
    function FilterGrayscale: TBGRACustomBitmap; override;
    function FilterNormalize(eachChannel: boolean = True): TBGRACustomBitmap; override;
    function FilterRotate(origin: TPointF; angle: single): TBGRACustomBitmap; override;
    function FilterSphere: TBGRACustomBitmap; override;
    function FilterTwirl(ACenter: TPoint; ARadius: Single; ATurn: Single=1; AExponent: Single=3): TBGRACustomBitmap; override;
    function FilterCylinder: TBGRACustomBitmap; override;
    function FilterPlane: TBGRACustomBitmap; override;

    property CanvasBGRA: TBGRACanvas read GetCanvasBGRA;
    property Canvas2D: TBGRACanvas2D read GetCanvas2D;
  end;

  { TBGRAPtrBitmap }

  TBGRAPtrBitmap = class(TBGRADefaultBitmap)
  protected
    procedure ReallocData; override;
    procedure FreeData; override;
  public
    constructor Create(AWidth, AHeight: integer; AData: Pointer); overload;
    function Duplicate(DuplicateProperties: Boolean = False): TBGRACustomBitmap; override;
    procedure SetDataPtr(AData: Pointer);
    property LineOrder: TRawImageLineOrder Read FLineOrder Write FLineOrder;
  end;

var
  DefaultTextStyle: TTextStyle;

procedure BGRAGradientFill(bmp: TBGRACustomBitmap; x, y, x2, y2: integer;
  c1, c2: TBGRAPixel; gtype: TGradientType; o1, o2: TPointF; mode: TDrawMode;
  gammaColorCorrection: boolean = True; Sinus: Boolean=False);

implementation

uses Math, LCLIntf, LCLType,
  BGRABlend, BGRAFilters, BGRAPen, BGRAText, BGRATextFX, BGRAGradientScanner,
  BGRAResample, BGRATransform, BGRAPolygon, BGRAPolygonAliased,
  BGRAPath, FPReadPcx, FPWritePcx, FPReadXPM, FPWriteXPM;

type
  TBitmapTracker = class(TBitmap)
  protected
    FUser: TBGRADefaultBitmap;
    procedure Changed(Sender: TObject); override;
  public
    constructor Create(AUser: TBGRADefaultBitmap); overload;
  end;

constructor TBitmapTracker.Create(AUser: TBGRADefaultBitmap);
begin
  FUser := AUser;
  inherited Create;
end;

procedure TBitmapTracker.Changed(Sender: TObject);
begin
  FUser.FBitmapModified := True;
  inherited Changed(Sender);
end;

{ TBGRADefaultBitmap }

function TBGRADefaultBitmap.CheckEmpty: boolean;
var
  i: integer;
  p: PBGRAPixel;
begin
  p := Data;
  for i := NbPixels - 1 downto 0 do
  begin
    if p^.alpha <> 0 then
    begin
      Result := False;
      exit;
    end;
    Inc(p);
  end;
  Result := True;
end;

function TBGRADefaultBitmap.GetCanvasAlphaCorrection: boolean;
begin
  Result := (FCanvasOpacity <> 0);
end;

function TBGRADefaultBitmap.GetCustomPenStyle: TBGRAPenStyle;
begin
  result := DuplicatePenStyle(FCustomPenStyle);
end;

procedure TBGRADefaultBitmap.SetCanvasAlphaCorrection(const AValue: boolean);
begin
  if AValue then
  begin
    if FCanvasOpacity = 0 then
      FCanvasOpacity := 255;
  end
  else
    FCanvasOpacity := 0;
end;

procedure TBGRADefaultBitmap.SetCanvasDrawModeFP(const AValue: TDrawMode);
begin
  FCanvasDrawModeFP := AValue;
  Case AValue of
  dmLinearBlend: FCanvasPixelProcFP := @FastBlendPixel;
  dmDrawWithTransparency: FCanvasPixelProcFP := @DrawPixel;
  dmXor: FCanvasPixelProcFP:= @XorPixel;
  else FCanvasPixelProcFP := @SetPixel;
  end;
end;

function TBGRADefaultBitmap.GetCanvasDrawModeFP: TDrawMode;
begin
  Result:= FCanvasDrawModeFP;
end;

procedure TBGRADefaultBitmap.SetCustomPenStyle(const AValue: TBGRAPenStyle);
begin
  FCustomPenStyle := DuplicatePenStyle(AValue);
end;

procedure TBGRADefaultBitmap.SetPenStyle(const AValue: TPenStyle);
begin
  Case AValue of
  psSolid: CustomPenStyle := SolidPenStyle;
  psDash: CustomPenStyle := DashPenStyle;
  psDot: CustomPenStyle := DotPenStyle;
  psDashDot: CustomPenStyle := DashDotPenStyle;
  psDashDotDot: CustomPenStyle := DashDotDotPenStyle;
  else CustomPenStyle := ClearPenStyle;
  end;
  FPenStyle := AValue;
end;

function TBGRADefaultBitmap.GetPenStyle: TPenStyle;
begin
  Result:= FPenStyle;
end;

{ Update font properties to internal TFont object }
procedure TBGRADefaultBitmap.UpdateFont;
begin
  if FFont.Name <> FontName then
    FFont.Name := FontName;
  if FFont.Style <> FontStyle then
    FFont.Style := FontStyle;
  if FFont.Height <> FFontHeight * FFontHeightSign then
    FFont.Height := FFontHeight * FFontHeightSign;
  if FFont.Orientation <> FontOrientation then
    FFont.Orientation := FontOrientation;
  if FontQuality = fqSystemClearType then
    FFont.Quality := fqCleartype
  else
    FFont.Quality := FontDefaultQuality;
end;

procedure TBGRADefaultBitmap.SetFontHeight(AHeight: integer);
begin
  FFontHeight := AHeight;
end;

function TBGRADefaultBitmap.GetFontFullHeight: integer;
begin
  if FontHeight < 0 then
    result := -FontHeight
  else
    result := TextSize('Hg').cy;
end;

procedure TBGRADefaultBitmap.SetFontFullHeight(AHeight: integer);
begin
  if AHeight > 0 then
    FontHeight := -AHeight
  else
    FontHeight := 1;
end;

function TBGRADefaultBitmap.GetFontPixelMetric: TFontPixelMetric;
var fxFont: TFont;
begin
  UpdateFont;
  if FontQuality = fqSystem then
    result := BGRAText.GetFontPixelMetric(FFont)
  else
  begin
    FxFont := TFont.Create;
    FxFont.Assign(FFont);
    FxFont.Height := fxFont.Height*FontAntialiasingLevel;
    Result:= BGRAText.GetFontPixelMetric(FxFont);
    if Result.Baseline <> -1 then Result.Baseline:= round((Result.Baseline-1)/FontAntialiasingLevel);
    if Result.CapLine <> -1 then Result.CapLine:= round(Result.CapLine/FontAntialiasingLevel);
    if Result.DescentLine <> -1 then Result.DescentLine:= round((Result.DescentLine-1)/FontAntialiasingLevel);
    if Result.Lineheight <> -1 then Result.Lineheight:= round(Result.Lineheight/FontAntialiasingLevel);
    if Result.xLine <> -1 then Result.xLine:= round(Result.xLine/FontAntialiasingLevel);
  end;
end;

{ Get scanline without checking bounds nor updated from TBitmap }
function TBGRADefaultBitmap.GetScanlineFast(y: integer): PBGRAPixel; inline;
begin
  Result := FData;
  if FLineOrder = riloBottomToTop then
    y := FHeight - 1 - y;
  Inc(Result, FWidth * y);
end;

function TBGRADefaultBitmap.GetScanLine(y: integer): PBGRAPixel;
begin
  if (y < 0) or (y >= Height) then
    raise ERangeError.Create('Scanline: out of bounds')
  else
  begin
    LoadFromBitmapIfNeeded;
    Result := GetScanLineFast(y);
  end;
end;

{------------------------- Reference counter functions ------------------------}
{ These functions are not related to reference counting for interfaces :
  a reference must be explicitely freed with FreeReference }

{ Add a new reference and gives a pointer to it }
function TBGRADefaultBitmap.NewReference: TBGRACustomBitmap;
begin
  Inc(FRefCount);
  Result := self;
end;

{ Free the current reference, and free the bitmap if necessary }
procedure TBGRADefaultBitmap.FreeReference;
begin
  if self = nil then
    exit;

  if FRefCount > 0 then
  begin
    Dec(FRefCount);
    if FRefCount = 0 then
    begin
      self.Destroy;
    end;
  end;
end;

{ Make sure there is only one copy of the bitmap and return
  the new pointer for it. If the bitmap is already unique,
  then it does nothing }
function TBGRADefaultBitmap.GetUnique: TBGRACustomBitmap;
begin
  if FRefCount > 1 then
  begin
    Dec(FRefCount);
    Result := self.Duplicate;
  end
  else
    Result := self;
end;

{ Creates a new bitmap. Internally, it uses the same type so that if you
  use an optimized version, you get a new bitmap with the same optimizations }
function TBGRADefaultBitmap.NewBitmap(AWidth, AHeight: integer): TBGRACustomBitmap;
var
  BGRAClass: TBGRABitmapAny;
begin
  BGRAClass := TBGRABitmapAny(self.ClassType);
  if BGRAClass = TBGRAPtrBitmap then
    BGRAClass := TBGRADefaultBitmap;
  Result      := BGRAClass.Create(AWidth, AHeight);
end;

function TBGRADefaultBitmap.NewBitmap(AWidth, AHeight: integer;
  Color: TBGRAPixel): TBGRACustomBitmap;
var
  BGRAClass: TBGRABitmapAny;
begin
  BGRAClass := TBGRABitmapAny(self.ClassType);
  if BGRAClass = TBGRAPtrBitmap then
    BGRAClass := TBGRADefaultBitmap;
  Result      := BGRAClass.Create(AWidth, AHeight, Color);
end;

{ Creates a new bitmap and loads it contents from a file }
function TBGRADefaultBitmap.NewBitmap(Filename: string): TBGRACustomBitmap;
var
  BGRAClass: TBGRABitmapAny;
begin
  BGRAClass := TBGRABitmapAny(self.ClassType);
  Result    := BGRAClass.Create(Filename);
end;

{----------------------- TFPCustomImage override ------------------------------}

{ Creates a new bitmap, initialize properties and bitmap data }
constructor TBGRADefaultBitmap.Create(AWidth, AHeight: integer);
begin
  Init;
  inherited Create(AWidth, AHeight);
  if FData <> nil then
    FillTransparent;
end;

{ Set the size of the current bitmap. All data is lost during the process }
procedure TBGRADefaultBitmap.SetSize(AWidth, AHeight: integer);
begin
  if (AWidth = Width) and (AHeight = Height) then
    exit;
  inherited SetSize(AWidth, AHeight);
  if AWidth < 0 then
    AWidth := 0;
  if AHeight < 0 then
    AHeight := 0;
  FWidth    := AWidth;
  FHeight   := AHeight;
  FNbPixels := AWidth * AHeight;
  if FNbPixels < 0 then // 2 Go limit
    raise EOutOfMemory.Create('Image too big');
  FreeBitmap;
  ReallocData;
  NoClip;
end;

{---------------------- Constructors ---------------------------------}

constructor TBGRADefaultBitmap.Create;
begin
  Init;
  inherited Create(0, 0);
end;

constructor TBGRADefaultBitmap.Create(ABitmap: TBitmap);
begin
  Init;
  inherited Create(ABitmap.Width, ABitmap.Height);
  Assign(ABitmap);
end;

constructor TBGRADefaultBitmap.Create(AWidth, AHeight: integer; Color: TColor);
begin
  Init;
  inherited Create(AWidth, AHeight);
  Fill(Color);
end;

constructor TBGRADefaultBitmap.Create(AWidth, AHeight: integer; Color: TBGRAPixel);
begin
  Init;
  inherited Create(AWidth, AHeight);
  Fill(Color);
end;

destructor TBGRADefaultBitmap.Destroy;
begin
  FreeData;
  FFont.Free;
  FBitmap.Free;
  FCanvasFP.Free;
  FCanvasBGRA.Free;
  FCanvas2D.Free;
  inherited Destroy;
end;

{------------------------- Loading functions ----------------------------------}

constructor TBGRADefaultBitmap.Create(AFilename: string);
begin
  Init;
  inherited Create(0, 0);  
  LoadFromFile(Afilename);
end;

constructor TBGRADefaultBitmap.Create(AStream: TStream);
begin
  Init;
  inherited Create(0, 0);  
  LoadFromStream(AStream);
end;

procedure TBGRADefaultBitmap.Assign(ABitmap: TBitmap);
var TempBmp: TBitmap;
    ConvertOk: boolean;
begin
  DiscardBitmapChange;
  SetSize(ABitmap.Width, ABitmap.Height);
  if not LoadFromRawImage(ABitmap.RawImage,0,False,False) then
  begin //try to convert
    TempBmp := TBitmap.Create;
    TempBmp.Width := ABitmap.Width;
    TempBmp.Height := ABitmap.Height;
    TempBmp.Canvas.Draw(0,0,ABitmap);
    ConvertOk := LoadFromRawImage(TempBmp.RawImage,0,False,False);
    TempBmp.Free;
    if not ConvertOk then
      raise Exception.Create('Unable to convert image to 24 bit');
  end;
  If Empty then AlphaFill(255); // if bitmap seems to be empty, assume
                                // it is an opaque bitmap without alpha channel
end;

procedure TBGRADefaultBitmap.Assign(MemBitmap: TBGRACustomBitmap);
begin
  DiscardBitmapChange;
  SetSize(MemBitmap.Width, MemBitmap.Height);
  PutImage(0, 0, MemBitmap, dmSet);
end;

procedure TBGRADefaultBitmap.Serialize(AStream: TStream);
var lWidth,lHeight: integer;
begin
  lWidth := NtoLE(Width);
  lHeight := NtoLE(Height);
  AStream.Write(lWidth,sizeof(lWidth));
  AStream.Write(lHeight,sizeof(lHeight));
  AStream.Write(Data^, NbPixels*sizeof(TBGRAPixel));
end;

{$hints off}
procedure TBGRADefaultBitmap.Deserialize(AStream: TStream);
var lWidth,lHeight: integer;
begin
  AStream.Read(lWidth,sizeof(lWidth));
  AStream.Read(lHeight,sizeof(lHeight));
  lWidth := LEtoN(lWidth);
  lHeight := LEtoN(lHeight);
  SetSize(lWidth,lHeight);
  AStream.Read(Data^, NbPixels*sizeof(TBGRAPixel));
end;
{$hints on}

class procedure TBGRADefaultBitmap.SerializeEmpty(AStream: TStream);
var zero: integer;
begin
  zero := 0;
  AStream.Write(zero,sizeof(zero));
  AStream.Write(zero,sizeof(zero));
end;

procedure TBGRADefaultBitmap.LoadFromFile(const filename: string);
var
  OldDrawMode: TDrawMode;
begin
  OldDrawMode := CanvasDrawModeFP;
  CanvasDrawModeFP := dmSet;
  ClipRect := rect(0,0,Width,Height);
  try
    inherited LoadFromfile(filename);
  finally
    CanvasDrawModeFP := OldDrawMode;
    ClearTransparentPixels;
  end;
end;

procedure TBGRADefaultBitmap.SaveToFile(const filename: string);
var
  ext:    string;
  writer: TFPCustomImageWriter;
begin
  ext := AnsiLowerCase(ExtractFileExt(filename));

  { When saving to PNG, define some parameters so that the
    image be readable by most programs }
  if ext = '.png' then
    writer := CreateAdaptedPngWriter
  else
  if (ext='.xpm') and (Width*Height > 32768) then //xpm is slow so avoid big images
    raise exception.Create('Image is too big to be saved as XPM') else
      writer := nil;

  if writer <> nil then //use custom writer if defined
  begin
    inherited SaveToFile(Filename, writer);
    writer.Free;
  end
  else
    inherited SaveToFile(Filename);
end;

procedure TBGRADefaultBitmap.SaveToStreamAsPng(Str: TStream);
var writer: TFPWriterPNG;
begin
  writer := CreateAdaptedPngWriter;
  SaveToStream(Str,writer);
  writer.Free;
end;

{------------------------- Clipping -------------------------------}

{ Check if a point is in the clipping rectangle }
function TBGRADefaultBitmap.PtInClipRect(x, y: integer): boolean;
begin
  result := (x >= FClipRect.Left) and (y >= FClipRect.Top) and (x < FClipRect.Right) and (y < FClipRect.Bottom);
end;

procedure TBGRADefaultBitmap.NoClip;
begin
  FClipRect := rect(0,0,FWidth,FHeight);
end;

procedure TBGRADefaultBitmap.Fill(texture: IBGRAScanner; mode: TDrawMode);
begin
  FillRect(FClipRect.Left,FClipRect.Top,FClipRect.Right,FClipRect.Bottom,texture,mode);
end;

function TBGRADefaultBitmap.GetClipRect: TRect;
begin
  Result:= FClipRect;
end;

procedure TBGRADefaultBitmap.SetClipRect(const AValue: TRect);
begin
  IntersectRect(FClipRect,AValue,Rect(0,0,FWidth,FHeight));
end;

function TBGRADefaultBitmap.GetPixelCycleInline(ix, iy: integer; iFactX,
  iFactY: integer): TBGRAPixel;
var
  ixMod1,ixMod2: integer;
  w1,w2,w3,w4,alphaW: cardinal;
  bSum, gSum, rSum: cardinal;
  aSum: cardinal;

  c:    TBGRAPixel;
  scan: PBGRAPixel;
begin
  w4 := (iFactX*iFactY+127) shr 8;
  w3 := iFactY-w4;
  w1 := cardinal(256-iFactX)-w3;
  w2 := iFactX-w4;

  rSum   := 0;
  gSum   := 0;
  bSum   := 0;
  aSum   := 0;

  scan := GetScanlineFast(PositiveMod(iy,Height));

  ixMod1 := PositiveMod(ix,Width); //apply cycle
  c      := (scan + ixMod1)^;
  alphaW := c.alpha * w1;
  aSum   += alphaW;

  rSum   += c.red * alphaW;
  gSum   += c.green * alphaW;
  bSum   += c.blue * alphaW;

  Inc(ix);
  ixMod2 := PositiveMod(ix,Width); //apply cycle
  c      := (scan + ixMod2)^;
  alphaW := c.alpha * w2;
  aSum   += alphaW;

  rSum   += c.red * alphaW;
  gSum   += c.green * alphaW;
  bSum   += c.blue * alphaW;

  Inc(iy);
  scan := GetScanlineFast(PositiveMod(iy,Height));

  c      := (scan + ixMod2)^;
  alphaW := c.alpha * w4;
  aSum   += alphaW;

  rSum   += c.red * alphaW;
  gSum   += c.green * alphaW;
  bSum   += c.blue * alphaW;

  c      := (scan + ixMod1)^;
  alphaW := c.alpha * w3;
  aSum   += alphaW;

  rSum   += c.red * alphaW;
  gSum   += c.green * alphaW;
  bSum   += c.blue * alphaW;

  if (aSum < 128) then
    Result := BGRAPixelTransparent
  else
  begin
    Result.red   := (rSum + aSum shr 1) div aSum;
    Result.green := (gSum + aSum shr 1) div aSum;
    Result.blue  := (bSum + aSum shr 1) div aSum;
    Result.alpha := (aSum + 128) shr 8;
  end;
end;
{-------------------------- Pixel functions -----------------------------------}

procedure TBGRADefaultBitmap.SetPixel(x, y: integer; c: TBGRAPixel);
begin
  if not PtInClipRect(x,y) then exit;
  LoadFromBitmapIfNeeded;
  (GetScanlineFast(y) +x)^ := c;
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.XorPixel(x, y: integer; c: TBGRAPixel);
var
  p : PDWord;
begin
  if not PtInClipRect(x,y) then exit;
  LoadFromBitmapIfNeeded;
  p := PDWord(GetScanlineFast(y) +x);
  p^ := p^ xor DWord(c);
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.SetPixel(x, y: integer; c: TColor);
var
  p: PByte;
begin
  if not PtInClipRect(x,y) then exit;
  LoadFromBitmapIfNeeded;
  p  := PByte(GetScanlineFast(y) + x);
  p^ := c shr 16;
  Inc(p);
  p^ := c shr 8;
  Inc(p);
  p^ := c;
  Inc(p);
  p^ := 255;
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.DrawPixel(x, y: integer; c: TBGRAPixel);
begin
  if not PtInClipRect(x,y) then exit;
  LoadFromBitmapIfNeeded;
  DrawPixelInlineWithAlphaCheck(GetScanlineFast(y) + x, c);
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.DrawPixel(x, y: integer; ec: TExpandedPixel);
begin
  if not PtInClipRect(x,y) then exit;
  LoadFromBitmapIfNeeded;
  DrawExpandedPixelInlineWithAlphaCheck(GetScanlineFast(y) + x, ec);
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.FastBlendPixel(x, y: integer; c: TBGRAPixel);
begin
  if not PtInClipRect(x,y) then exit;
  LoadFromBitmapIfNeeded;
  FastBlendPixelInline(GetScanlineFast(y) + x, c);
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.ErasePixel(x, y: integer; alpha: byte);
begin
  if not PtInClipRect(x,y) then exit;
  LoadFromBitmapIfNeeded;
  ErasePixelInline(GetScanlineFast(y) + x, alpha);
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.AlphaPixel(x, y: integer; alpha: byte);
begin
  if not PtInClipRect(x,y) then exit;
  LoadFromBitmapIfNeeded;
  if alpha = 0 then
    (GetScanlineFast(y) +x)^ := BGRAPixelTransparent
  else
    (GetScanlineFast(y) +x)^.alpha := alpha;
  InvalidateBitmap;
end;

function TBGRADefaultBitmap.GetPixel(x, y: integer): TBGRAPixel;
begin
  if (x < 0) or (x >= Width) or (y < 0) or (y >= Height) then //it is possible to read pixels outside of the cliprect
    Result := BGRAPixelTransparent
  else
  begin
    LoadFromBitmapIfNeeded;
    Result := (GetScanlineFast(y) + x)^;
  end;
end;

{$hints off}
{ This function compute an interpolated pixel at floating point coordinates }
function TBGRADefaultBitmap.GetPixel(x, y: single; AResampleFilter: TResampleFilter = rfLinear): TBGRAPixel;
var
  ix, iy: integer;
  w1,w2,w3,w4,alphaW: cardinal;
  rSum, gSum, bSum: cardinal; //rgbDiv = aSum
  aSum: cardinal;
  c:    TBGRAPixel;
  scan: PBGRAPixel;
  factX,factY: single;
  iFactX,iFactY: integer;
begin
  ix := floor(x);
  iy := floor(y);
  factX := x-ix; //distance from integer coordinate
  factY := y-iy;

  //if the coordinate is integer, then call standard GetPixel function
  if (factX = 0) and (factY = 0) then
  begin
    Result := GetPixel(ix, iy);
    exit;
  end;
  LoadFromBitmapIfNeeded;

  rSum   := 0;
  gSum   := 0;
  bSum   := 0;
  aSum   := 0;

  //apply interpolation filter
  factX := FineInterpolation( factX, AResampleFilter );
  factY := FineInterpolation( factY, AResampleFilter );

  iFactX := round(factX*256); //integer values for fractionnal part
  iFactY := round(factY*256);

  w4 := (iFactX*iFactY+127) shr 8;
  w3 := iFactY-w4;
  w1 := (256-iFactX)-w3;
  w2 := iFactX-w4;

  { For each pixel around the coordinate, compute
    the weight for it and multiply values by it before
    adding to the sum }
  if (iy >= 0) and (iy < Height) then
  begin
    scan := GetScanlineFast(iy);

    if (ix >= 0) and (ix < Width) then
    begin
      c      := (scan + ix)^;
      alphaW := c.alpha * w1;
      aSum   += alphaW;
      rSum   += c.red * alphaW;
      gSum   += c.green * alphaW;
      bSum   += c.blue * alphaW;
    end;

    Inc(ix);
    if (ix >= 0) and (ix < Width) then
    begin
      c      := (scan + ix)^;
      alphaW := c.alpha * w2;
      aSum   += alphaW;
      rSum   += c.red * alphaW;
      gSum   += c.green * alphaW;
      bSum   += c.blue * alphaW;
    end;
  end
  else
  begin
    Inc(ix);
  end;

  Inc(iy);
  if (iy >= 0) and (iy < Height) then
  begin
    scan := GetScanlineFast(iy);

    if (ix >= 0) and (ix < Width) then
    begin
      c      := (scan + ix)^;
      alphaW := c.alpha * w4;
      aSum   += alphaW;
      rSum   += c.red * alphaW;
      gSum   += c.green * alphaW;
      bSum   += c.blue * alphaW;
    end;

    Dec(ix);
    if (ix >= 0) and (ix < Width) then
    begin
      c      := (scan + ix)^;
      alphaW := c.alpha * w3;
      aSum   += alphaW;
      rSum   += c.red * alphaW;
      gSum   += c.green * alphaW;
      bSum   += c.blue * alphaW;
    end;
  end;

  if aSum < 128 then //if there is no alpha
    Result := BGRAPixelTransparent
  else
  begin
    Result.red   := (rSum + aSum shr 1) div aSum;
    Result.green := (gSum + aSum shr 1) div aSum;
    Result.blue  := (bSum + aSum shr 1) div aSum;
    Result.alpha := (aSum + 128) shr 8;
  end;
end;

{ Same as GetPixel(single,single,TResampleFilter) but with coordinate cycle, supposing the image repeats itself in both directions }
function TBGRADefaultBitmap.GetPixelCycle(x, y: single; AResampleFilter: TResampleFilter = rfLinear): TBGRAPixel;
var
  ix, iy: integer;
  iFactX,iFactY: integer;
begin
  LoadFromBitmapIfNeeded;
  iFactX := round(x*256);
  iFactY := round(y*256);
  ix := (iFactX shr 8)+ScanOffset.X;
  iy := (iFactY shr 8)+ScanOffset.Y;
  iFactX := iFactX and 255;
  iFactY := iFactY and 255;

  if (iFactX = 0) and (iFactY = 0) then
  begin
    result := (ScanLine[PositiveMod(iy, FHeight)]+PositiveMod(ix, FWidth))^;
    exit;
  end;

  if ScanInterpolationFilter <> rfLinear then
  begin
    iFactX := FineInterpolation256( iFactX, ScanInterpolationFilter );
    iFactY := FineInterpolation256( iFactY, ScanInterpolationFilter );
  end;

  result := GetPixelCycleInline(ix,iy, iFactX,iFactY);
end;

function TBGRADefaultBitmap.GetPixelCycle(x, y: single;
  AResampleFilter: TResampleFilter; repeatX: boolean; repeatY: boolean
  ): TBGRAPixel;
var
  alpha: byte;
begin
  alpha := 255;
  if not repeatX then
  begin
    if (x < -0.5) or (x > Width-0.5) then
    begin
      result := BGRAPixelTransparent;
      exit;
    end;
    if x < 0 then
    begin
      alpha := round((0.5+x)*510);
      x := 0;
    end
    else
    if x > Width-1 then
    begin
      alpha := round((Width-0.5-x)*510);
      x := Width-1;
    end;
  end;
  if not repeatY then
  begin
    if (y < -0.5) or (y > Height-0.5) then
    begin
      result := BGRAPixelTransparent;
      exit;
    end;
    if y < 0 then
    begin
      alpha := round((0.5+y)*2*alpha);
      y := 0;
    end
    else
    if y > Height-1 then
    begin
      alpha := round((Height-0.5-y)*2*alpha);
      y := Height-1;
    end;
  end;
  result := GetPixelCycle(x,y,AResampleFilter);
  if alpha<>255 then
    result.alpha := ApplyOpacity(result.alpha,alpha);
end;

{$hints on}

procedure TBGRADefaultBitmap.InvalidateBitmap;
begin
  FDataModified := True;
end;

function TBGRADefaultBitmap.GetBitmap: TBitmap;
begin
  if FAlphaCorrectionNeeded and CanvasAlphaCorrection then
    LoadFromBitmapIfNeeded;
  if FDataModified or (FBitmap = nil) then
  begin
    RebuildBitmap;
    FDataModified := False;
  end;
  Result := FBitmap;
end;

function TBGRADefaultBitmap.GetCanvas: TCanvas;
begin
  Result := Bitmap.Canvas;
end;

function TBGRADefaultBitmap.GetCanvasFP: TFPImageCanvas;
begin
  {$warnings off}
  if FCanvasFP = nil then
    FCanvasFP := TFPImageCanvas.Create(self);
  {$warnings on}
  result := FCanvasFP;
end;

{ Load raw image data. It must be 32bit or 24 bits per pixel}
function TBGRADefaultBitmap.LoadFromRawImage(ARawImage: TRawImage;
  DefaultOpacity: byte; AlwaysReplaceAlpha: boolean; RaiseErrorOnInvalidPixelFormat: boolean): boolean;
var
  psource_byte, pdest_byte,
  psource_first, pdest_first: PByte;
  psource_delta, pdest_delta: integer;

  n: integer;
  mustSwapRedBlue, mustReverse32: boolean;

  procedure CopyAndSwapIfNecessary(psrc: PBGRAPixel; pdest: PBGRAPixel; count: integer);
  begin
    if mustReverse32 then
    begin
      while count > 0 do
      begin
        pdest^.blue := psrc^.alpha;
        pdest^.green := psrc^.red;
        pdest^.red := psrc^.green;
        pdest^.alpha := psrc^.blue;
        dec(count);
        inc(pdest);
        inc(psrc);
      end;
    end else
    if mustSwapRedBlue then
    begin
      while count > 0 do
      begin
        pdest^.red := psrc^.blue;
        pdest^.green := psrc^.green;
        pdest^.blue := psrc^.red;
        pdest^.alpha := psrc^.alpha;
        dec(count);
        inc(pdest);
        inc(psrc);
      end;
    end else
      move(psrc^,pdest^,count*sizeof(TBGRAPixel));
  end;

  procedure CopyRGBAndSwapIfNecessary(psrc: PByte; pdest: PBGRAPixel; count: integer);
  begin
    if mustSwapRedBlue then
    begin
      while count > 0 do
      begin
        pdest^.blue := (psrc+2)^;
        pdest^.green := (psrc+1)^;
        pdest^.red := psrc^;
        pdest^.alpha := DefaultOpacity;
        inc(psrc,3);
        inc(pdest);
        dec(count);
      end;
    end else
    begin
      while count > 0 do
      begin
        PWord(pdest)^ := PWord(psrc)^;
        pdest^.red := (psrc+2)^;
        pdest^.alpha := DefaultOpacity;
        inc(psrc,3);
        inc(pdest);
        dec(count);
      end;
    end;
  end;     

  procedure CopyAndSwapIfNecessaryAndSetAlpha(psrc: PBGRAPixel; pdest: PBGRAPixel; count: integer);
  begin
    if mustReverse32 then
    begin
      while count > 0 do
      begin
        pdest^.blue := psrc^.alpha;
        pdest^.green := psrc^.red;
        pdest^.red := psrc^.green;
        pdest^.alpha := DefaultOpacity; //use default opacity
        inc(psrc);
        inc(pdest);
        dec(count);
      end;
    end else
    if mustSwapRedBlue then
    begin
      while count > 0 do
      begin
        pdest^.red := psrc^.blue;
        pdest^.green := psrc^.green;
        pdest^.blue := psrc^.red;
        pdest^.alpha := DefaultOpacity; //use default opacity
        inc(psrc);
        inc(pdest);
        dec(count);
      end;
    end else
    begin
      while count > 0 do
      begin
        PWord(pdest)^ := PWord(psrc)^;
        pdest^.red := psrc^.red;
        pdest^.alpha := DefaultOpacity; //use default opacity
        inc(psrc);
        inc(pdest);
        dec(count);
      end;
    end;
  end;

  procedure CopyAndSwapIfNecessaryAndReplaceAlpha(psrc: PBGRAPixel; pdest: PBGRAPixel; count: integer);
  var OpacityOrMask, OpacityAndMask, sourceval: Longword;
  begin
    OpacityOrMask := NtoLE(longword(DefaultOpacity) shl 24);
    OpacityAndMask := NtoLE($FFFFFF);
    if mustReverse32 then
    begin
      OpacityAndMask := NtoBE($FFFFFF);
      while count > 0 do
      begin
        sourceval := plongword(psrc)^ and OpacityAndMask;
        if (sourceval <> 0) and (psrc^.blue{=alpha} = 0) then //if not black but transparent
        begin
          pdest^.blue := psrc^.alpha;
          pdest^.green := psrc^.red;
          pdest^.red := psrc^.green;
          pdest^.alpha := DefaultOpacity; //use default opacity
        end
        else
        begin
          pdest^.blue := psrc^.alpha;
          pdest^.green := psrc^.red;
          pdest^.red := psrc^.green;
          pdest^.alpha := psrc^.blue;
        end;
        dec(count);
        inc(pdest);
        inc(psrc);
      end;
    end else
    if mustSwapRedBlue then
    begin
      while count > 0 do
      begin
        sourceval := plongword(psrc)^ and OpacityAndMask;
        if (sourceval <> 0) and (psrc^.alpha = 0) then //if not black but transparent
        begin
          pdest^.red := psrc^.blue;
          pdest^.green := psrc^.green;
          pdest^.blue := psrc^.red;
          pdest^.alpha := DefaultOpacity; //use default opacity
        end
        else
        begin
          pdest^.red := psrc^.blue;
          pdest^.green := psrc^.green;
          pdest^.blue := psrc^.red;
          pdest^.alpha := psrc^.alpha;
        end;
        dec(count);
        inc(pdest);
        inc(psrc);
      end;
    end else
    begin
      while count > 0 do
      begin
        sourceval := plongword(psrc)^ and OpacityAndMask;
        if (sourceval <> 0) and (psrc^.alpha = 0) then //if not black but transparent
          plongword(pdest)^ := sourceval or OpacityOrMask //use default opacity
        else
          pdest^ := psrc^;
        dec(count);
        inc(pdest);
        inc(psrc);
      end;
    end;
  end;

begin
  if (ARawImage.Description.Width <> cardinal(Width)) or
    (ARawImage.Description.Height <> cardinal(Height)) then
    raise Exception.Create('Bitmap size is inconsistant');

  DiscardBitmapChange;
  if (Height=0) or (Width=0) then
  begin
    result := true;
    exit;
  end;

  if ARawImage.Description.LineOrder = riloTopToBottom then
  begin
    psource_first := ARawImage.Data;
    psource_delta := ARawImage.Description.BytesPerLine;
  end else
  begin
    psource_first := ARawImage.Data + (ARawImage.Description.Height-1) * ARawImage.Description.BytesPerLine;
    psource_delta := -ARawImage.Description.BytesPerLine;
  end;

  if ((ARawImage.Description.RedShift = 0) and
    (ARawImage.Description.BlueShift = 16) and
    (ARawImage.Description.ByteOrder = riboLSBFirst)) or
    ((ARawImage.Description.RedShift = 24) and
    (ARawImage.Description.BlueShift = 8) and
    (ARawImage.Description.ByteOrder = riboMSBFirst)) then
    mustSwapRedBlue:= true
  else
  begin
    mustSwapRedBlue:= false;
    if ((ARawImage.Description.RedShift = 8) and
      (ARawImage.Description.GreenShift = 16) and
      (ARawImage.Description.BlueShift = 24) and
      (ARawImage.Description.ByteOrder = riboLSBFirst)) or
      ((ARawImage.Description.RedShift = 16) and
      (ARawImage.Description.GreenShift = 8) and
      (ARawImage.Description.BlueShift = 0) and
      (ARawImage.Description.ByteOrder = riboMSBFirst)) then
        mustReverse32 := true
      else
        mustReverse32 := false;
  end;

  if self.LineOrder = riloTopToBottom then
  begin
    pdest_first := PByte(self.Data);
    pdest_delta := self.Width*sizeof(TBGRAPixel);
  end else
  begin
    pdest_first := PByte(self.Data) + (self.Height-1)*self.Width*sizeof(TBGRAPixel);
    pdest_delta := -self.Width*sizeof(TBGRAPixel);
  end;

  { 32 bits per pixel }
  if (ARawImage.Description.BitsPerPixel = 32) and
    (ARawImage.DataSize >= longword(NbPixels) * 4) then
  begin
    { If there is an alpha channel }
    if (ARawImage.Description.AlphaPrec = 8) and not AlwaysReplaceAlpha then
    begin
      if DefaultOpacity = 0 then
      begin
        if ARawImage.Description.LineOrder = FLineOrder then
          CopyAndSwapIfNecessary(PBGRAPixel(ARawImage.Data), FData, NbPixels) else
        begin
          psource_byte := psource_first;
          pdest_byte := pdest_first;
          for n := FHeight-1 downto 0 do
          begin
            CopyAndSwapIfNecessary(PBGRAPixel(psource_byte), PBGRAPixel(pdest_byte), FWidth);
            inc(psource_byte, psource_delta);
            inc(pdest_byte, pdest_delta);
          end;
        end;
      end
      else
      begin
        psource_byte := psource_first;
        pdest_byte := pdest_first;
        for n := FHeight-1 downto 0 do
        begin
          CopyAndSwapIfNecessaryAndReplaceAlpha(PBGRAPixel(psource_byte), PBGRAPixel(pdest_byte), FWidth);
          inc(psource_byte, psource_delta);
          inc(pdest_byte, pdest_delta);
        end;
      end;
    end
    else
    begin { If there isn't any alpha channel }
      psource_byte := psource_first;
      pdest_byte := pdest_first;
      for n := FHeight-1 downto 0 do
      begin
        CopyAndSwapIfNecessaryAndSetAlpha(PBGRAPixel(psource_byte), PBGRAPixel(pdest_byte), FWidth);
        inc(psource_byte, psource_delta);
        inc(pdest_byte, pdest_delta);
      end;
    end;
  end
  else
  { 24 bit per pixel }
  if (ARawImage.Description.BitsPerPixel = 24) then
  begin
    psource_byte := psource_first;
    pdest_byte := pdest_first;
    for n := FHeight-1 downto 0 do
    begin
      CopyRGBAndSwapIfNecessary(psource_byte, PBGRAPixel(pdest_byte), FWidth);
      inc(psource_byte, psource_delta);
      inc(pdest_byte, pdest_delta);
    end;
  end
  else
  begin
    if RaiseErrorOnInvalidPixelFormat then
      raise Exception.Create('Invalid raw image format (' + IntToStr(
        ARawImage.Description.Depth) + ' found)') else
    begin
      result := false;
      exit;
    end;
  end;

  InvalidateBitmap;
  result := true;
end;

procedure TBGRADefaultBitmap.LoadFromBitmapIfNeeded;
begin
  if FBitmapModified then
  begin
    if FBitmap <> nil then
      LoadFromRawImage(FBitmap.RawImage, FCanvasOpacity);
    DiscardBitmapChange;
  end;
  if FAlphaCorrectionNeeded then
  begin
    DoAlphaCorrection;
  end;
end;

procedure TBGRADefaultBitmap.DiscardBitmapChange; inline;
begin
  FBitmapModified := False;
end;

{ Initialize properties }
procedure TBGRADefaultBitmap.Init;
begin
  FRefCount  := 1;
  FBitmap    := nil;
  FCanvasFP  := nil;
  FCanvasBGRA := nil;
  CanvasDrawModeFP := dmDrawWithTransparency;
  FData      := nil;
  FWidth     := 0;
  FHeight    := 0;
  FLineOrder := riloTopToBottom;
  FCanvasOpacity := 255;
  FAlphaCorrectionNeeded := False;
  FEraseMode := False;
  FillMode := fmWinding;

  FFont     := TFont.Create;
  FontName  := 'Arial';
  FontStyle := [];
  FontAntialias := False;
  FFontHeight := 20;
  FFontHeightSign := GetFontHeightSign(FFont);

  PenStyle := psSolid;
  LineCap := pecRound;
  JoinStyle := pjsBevel;
  JoinMiterLimit := 2;
  ResampleFilter := rfHalfCosine;
  ScanInterpolationFilter := rfLinear;
  ScanOffset := Point(0,0);
end;

procedure TBGRADefaultBitmap.SetInternalColor(x, y: integer; const Value: TFPColor);
begin
  FCanvasPixelProcFP(x,y, FPColorToBGRA(Value));
end;

function TBGRADefaultBitmap.GetInternalColor(x, y: integer): TFPColor;
begin
  if (x < 0) or (y < 0) or (x >= Width) or (y >= Height) then exit;
  result := BGRAToFPColor((Scanline[y] + x)^);
end;

procedure TBGRADefaultBitmap.SetInternalPixel(x, y: integer; Value: integer);
var
  c: TFPColor;
begin
  if not PtInClipRect(x,y) then exit;
  c  := Palette.Color[Value];
  (Scanline[y] + x)^ := FPColorToBGRA(c);
  InvalidateBitmap;
end;

function TBGRADefaultBitmap.GetInternalPixel(x, y: integer): integer;
var
  c: TFPColor;
begin
  if (x < 0) or (y < 0) or (x >= Width) or (y >= Height) then exit;
  c := BGRAToFPColor((Scanline[y] + x)^);
  Result := palette.IndexOf(c);
end;

procedure TBGRADefaultBitmap.Draw(ACanvas: TCanvas; x, y: integer; Opaque: boolean);
begin
  if self = nil then
    exit;
  if Opaque then
    DataDrawOpaque(ACanvas, Rect(X, Y, X + Width, Y + Height), Data,
      FLineOrder, FWidth, FHeight)
  else
  begin
    LoadFromBitmapIfNeeded;
    if Empty then
      exit;
    ACanvas.Draw(X, Y, Bitmap);
  end;
end;

procedure TBGRADefaultBitmap.Draw(ACanvas: TCanvas; Rect: TRect; Opaque: boolean);
begin
  if self = nil then
    exit;
  if Opaque then
    DataDrawOpaque(ACanvas, Rect, Data, FLineOrder, FWidth, FHeight)
  else
  begin
    LoadFromBitmapIfNeeded;
    if Empty then
      exit;
    ACanvas.StretchDraw(Rect, Bitmap);
  end;
end;

{---------------------------- Line primitives ---------------------------------}

function TBGRADefaultBitmap.CheckHorizLineBounds(var x,y,x2: integer): boolean; inline;
var
  temp: integer;
begin
  if (x2 < x) then
  begin
    temp := x;
    x    := x2;
    x2   := temp;
  end;
  if (x >= FClipRect.Right) or (x2 < FClipRect.Left) or (y < FClipRect.Top) or (y >= FClipRect.Bottom) then
  begin
    result := false;
    exit;
  end;
  if x < FClipRect.Left then
    x := FClipRect.Left;
  if x2 >= FClipRect.Right then
    x2 := FClipRect.Right - 1;
  result := true;
end;

procedure TBGRADefaultBitmap.SetHorizLine(x, y, x2: integer; c: TBGRAPixel);
begin
  if not CheckHorizLineBounds(x,y,x2) then exit;
  FillInline(scanline[y] + x, c, x2 - x + 1);
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.XorHorizLine(x, y, x2: integer; c: TBGRAPixel);
begin
  if not CheckHorizLineBounds(x,y,x2) then exit;
  XorInline(scanline[y] + x, c, x2 - x + 1);
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.DrawHorizLine(x, y, x2: integer; c: TBGRAPixel);
begin
  if not CheckHorizLineBounds(x,y,x2) then exit;
  DrawPixelsInline(scanline[y] + x, c, x2 - x + 1);
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.DrawHorizLine(x, y, x2: integer; ec: TExpandedPixel
  );
begin
  if not CheckHorizLineBounds(x,y,x2) then exit;
  DrawExpandedPixelsInline(scanline[y] + x, ec, x2 - x + 1);
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.DrawHorizLine(x, y, x2: integer;
  texture: IBGRAScanner);
begin
  if not CheckHorizLineBounds(x,y,x2) then exit;
  texture.ScanMoveTo(x,y);
  ScannerPutPixels(texture,scanline[y] + x, x2 - x + 1,dmDrawWithTransparency);
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.FastBlendHorizLine(x, y, x2: integer; c: TBGRAPixel);
begin
  if not CheckHorizLineBounds(x,y,x2) then exit;
  FastBlendPixelsInline(scanline[y] + x, c, x2 - x + 1);
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.AlphaHorizLine(x, y, x2: integer; alpha: byte);
begin
  if alpha = 0 then
  begin
    SetHorizLine(x, y, x2, BGRAPixelTransparent);
    exit;
  end;
  if not CheckHorizLineBounds(x,y,x2) then exit;
  AlphaFillInline(scanline[y] + x, alpha, x2 - x + 1);
  InvalidateBitmap;
end;

function TBGRADefaultBitmap.CheckVertLineBounds(var x,y,y2: integer; out delta: integer): boolean; inline;
var
  temp: integer;
begin
  if FLineOrder = riloBottomToTop then
    delta := -Width
  else
    delta := Width;

  if (y2 < y) then
  begin
    temp := y;
    y    := y2;
    y2   := temp;
  end;

  if y < FClipRect.Top then
    y := FClipRect.Top;
  if y2 >= FClipRect.Bottom then
    y2 := FClipRect.Bottom - 1;

  if (y >= FClipRect.Bottom) or (y2 < FClipRect.Top) or (x < FClipRect.Left) or (x >= FClipRect.Right) then
  begin
    result := false;
    exit;
  end;

  result := true;
end;

procedure TBGRADefaultBitmap.SetVertLine(x, y, y2: integer; c: TBGRAPixel);
var
  n, delta: integer;
  p: PBGRAPixel;
begin
  if not CheckVertLineBounds(x,y,y2,delta) then exit;
  p    := scanline[y] + x;
  for n := y2 - y downto 0 do
  begin
    p^ := c;
    Inc(p, delta);
  end;
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.XorVertLine(x, y, y2: integer; c: TBGRAPixel);
var
  n, delta: integer;
  p: PBGRAPixel;
begin
  if not CheckVertLineBounds(x,y,y2,delta) then exit;
  p    := scanline[y] + x;
  for n := y2 - y downto 0 do
  begin
    PDword(p)^ := PDword(p)^ xor DWord(c);
    Inc(p, delta);
  end;
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.DrawVertLine(x, y, y2: integer; c: TBGRAPixel);
var
  n, delta: integer;
  p: PBGRAPixel;
begin
  if c.alpha = 255 then
  begin
    SetVertLine(x,y,y2,c);
    exit;
  end;
  if not CheckVertLineBounds(x,y,y2,delta) or (c.alpha=0) then exit;
  p    := scanline[y] + x;
  for n := y2 - y downto 0 do
  begin
    DrawPixelInlineNoAlphaCheck(p, c);
    Inc(p, delta);
  end;
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.AlphaVertLine(x, y, y2: integer; alpha: byte);
var
  n, delta: integer;
  p: PBGRAPixel;
begin
  if alpha = 0 then
  begin
    SetVertLine(x, y, y2, BGRAPixelTransparent);
    exit;
  end;
  if not CheckVertLineBounds(x,y,y2,delta) then exit;
  p    := scanline[y] + x;
  for n := y2 - y downto 0 do
  begin
    p^.alpha := alpha;
    Inc(p, delta);
  end;
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.FastBlendVertLine(x, y, y2: integer; c: TBGRAPixel);
var
  n, delta: integer;
  p: PBGRAPixel;
begin
  if not CheckVertLineBounds(x,y,y2,delta) then exit;
  p    := scanline[y] + x;
  for n := y2 - y downto 0 do
  begin
    FastBlendPixelInline(p, c);
    Inc(p, delta);
  end;
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.DrawHorizLineDiff(x, y, x2: integer;
  c, compare: TBGRAPixel; maxDiff: byte);
begin
  if not CheckHorizLineBounds(x,y,x2) then exit;
  DrawPixelsInlineDiff(scanline[y] + x, c, x2 - x + 1, compare, maxDiff);
  InvalidateBitmap;
end;

{---------------------------- Lines ---------------------------------}
{ Call appropriate functions }

procedure TBGRADefaultBitmap.DrawLine(x1, y1, x2, y2: integer;
  c: TBGRAPixel; DrawLastPixel: boolean);
begin
  BGRADrawLineAliased(self,x1,y1,x2,y2,c,DrawLastPixel);
end;

procedure TBGRADefaultBitmap.DrawLineAntialias(x1, y1, x2, y2: integer;
  c: TBGRAPixel; DrawLastPixel: boolean);
begin
  BGRADrawLineAntialias(self,x1,y1,x2,y2,c,DrawLastPixel);
end;

procedure TBGRADefaultBitmap.DrawLineAntialias(x1, y1, x2, y2: integer;
  c1, c2: TBGRAPixel; dashLen: integer; DrawLastPixel: boolean);
var DashPos: integer;
begin
  DashPos := 0;
  BGRADrawLineAntialias(self,x1,y1,x2,y2,c1,c2,dashLen,DrawLastPixel,DashPos);
end;

procedure TBGRADefaultBitmap.DrawLineAntialias(x1, y1, x2, y2: integer; c1,
  c2: TBGRAPixel; dashLen: integer; DrawLastPixel: boolean; var DashPos: integer);
begin
  BGRADrawLineAntialias(self,x1,y1,x2,y2,c1,c2,dashLen,DrawLastPixel,DashPos);
end;

procedure TBGRADefaultBitmap.DrawLineAntialias(x1, y1, x2, y2: single;
  c: TBGRAPixel; w: single);
begin
  BGRAPolyLine(self,[PointF(x1,y1),PointF(x2,y2)],w,c,LineCap,JoinStyle,FCustomPenStyle,[],nil,JoinMiterLimit);
end;

procedure TBGRADefaultBitmap.DrawLineAntialias(x1, y1, x2, y2: single;
  texture: IBGRAScanner; w: single);
begin
  BGRAPolyLine(self,[PointF(x1,y1),PointF(x2,y2)],w,BGRAPixelTransparent,LineCap,JoinStyle,FCustomPenStyle,[],texture,JoinMiterLimit);
end;

procedure TBGRADefaultBitmap.DrawLineAntialias(x1, y1, x2, y2: single;
  c: TBGRAPixel; w: single; closed: boolean);
var
  options: TBGRAPolyLineOptions;
begin
  if not closed then options := [plRoundCapOpen] else options := [];
  BGRAPolyLine(self,[PointF(x1,y1),PointF(x2,y2)],w,c,pecRound,pjsRound,FCustomPenStyle,options,nil,JoinMiterLimit);
end;

procedure TBGRADefaultBitmap.DrawLineAntialias(x1, y1, x2, y2: single;
  texture: IBGRAScanner; w: single; Closed: boolean);
var
  options: TBGRAPolyLineOptions;
  c: TBGRAPixel;
begin
  if not closed then
  begin
    options := [plRoundCapOpen];
    c := BGRAWhite; //needed for alpha junction
  end else
  begin
    options := [];
    c := BGRAPixelTransparent;
  end;
  BGRAPolyLine(self,[PointF(x1,y1),PointF(x2,y2)],w,c,pecRound,pjsRound,FCustomPenStyle,options,texture,JoinMiterLimit);
end;

procedure TBGRADefaultBitmap.DrawPolyLineAntialias(const points: array of TPointF;
  c: TBGRAPixel; w: single);
begin
  BGRAPolyLine(self,points,w,c,LineCap,JoinStyle,FCustomPenStyle,[],nil,JoinMiterLimit);
end;

procedure TBGRADefaultBitmap.DrawPolyLineAntialias(
  const points: array of TPointF; texture: IBGRAScanner; w: single);
begin
  BGRAPolyLine(self,points,w,BGRAPixelTransparent,LineCap,JoinStyle,FCustomPenStyle,[],texture,JoinMiterLimit);
end;

procedure TBGRADefaultBitmap.DrawPolyLineAntialias(const points: array of TPointF;
  c: TBGRAPixel; w: single; Closed: boolean);
var
  options: TBGRAPolyLineOptions;
begin
  if not closed then options := [plRoundCapOpen] else options := [];
  BGRAPolyLine(self,points,w,c,pecRound,JoinStyle,FCustomPenStyle,options,nil,JoinMiterLimit);
end;

procedure TBGRADefaultBitmap.DrawPolygonAntialias(const points: array of TPointF;
  c: TBGRAPixel; w: single);
begin
   BGRAPolyLine(self,points,w,c,LineCap,JoinStyle,FCustomPenStyle,[plCycle],nil,JoinMiterLimit);
end;

procedure TBGRADefaultBitmap.DrawPolygonAntialias(
  const points: array of TPointF; texture: IBGRAScanner; w: single);
begin
  BGRAPolyLine(self,points,w,BGRAPixelTransparent,LineCap,JoinStyle,FCustomPenStyle,[plCycle],texture,JoinMiterLimit);
end;

procedure TBGRADefaultBitmap.EraseLine(x1, y1, x2, y2: integer; alpha: byte;
  DrawLastPixel: boolean);
begin
  BGRAEraseLineAliased(self,x1,y1,x2,y2,alpha,DrawLastPixel);
end;

procedure TBGRADefaultBitmap.EraseLineAntialias(x1, y1, x2, y2: integer;
  alpha: byte; DrawLastPixel: boolean);
begin
  BGRAEraseLineAntialias(self,x1,y1,x2,y2,alpha,DrawLastPixel);
end;

procedure TBGRADefaultBitmap.EraseLineAntialias(x1, y1, x2, y2: single;
  alpha: byte; w: single; Closed: boolean);
begin
  FEraseMode := True;
  DrawLineAntialias(x1, y1, x2, y2, BGRA(0, 0, 0, alpha), w, Closed);
  FEraseMode := False;
end;

procedure TBGRADefaultBitmap.ErasePolyLineAntialias(const points: array of TPointF;
  alpha: byte; w: single);
begin
  FEraseMode := True;
  DrawPolyLineAntialias(points, BGRA(0,0,0,alpha),w);
  FEraseMode := False;
end;

{------------------------ Shapes ----------------------------------------------}
{ Call appropriate functions }

procedure TBGRADefaultBitmap.FillTriangleLinearColor(pt1, pt2, pt3: TPointF;
  c1, c2, c3: TBGRAPixel);
begin
  FillPolyLinearColor([pt1,pt2,pt3],[c1,c2,c3]);
end;

procedure TBGRADefaultBitmap.FillTriangleLinearColorAntialias(pt1, pt2,
  pt3: TPointF; c1, c2, c3: TBGRAPixel);
var
  grad: TBGRAGradientTriangleScanner;
begin
  grad := TBGRAGradientTriangleScanner.Create(pt1,pt2,pt3, c1,c2,c3);
  FillPolyAntialias([pt1,pt2,pt3],grad);
  grad.Free;
end;

procedure TBGRADefaultBitmap.FillTriangleLinearMapping(pt1, pt2, pt3: TPointF;
  texture: IBGRAScanner; tex1, tex2, tex3: TPointF; TextureInterpolation: Boolean= True);
begin
  FillPolyLinearMapping([pt1,pt2,pt3],texture,[tex1,tex2,tex3],TextureInterpolation);
end;

procedure TBGRADefaultBitmap.FillTriangleLinearMappingLightness(pt1, pt2,
  pt3: TPointF; texture: IBGRAScanner; tex1, tex2, tex3: TPointF; light1,
  light2, light3: word; TextureInterpolation: Boolean);
begin
  FillPolyLinearMappingLightness([pt1,pt2,pt3],texture,[tex1,tex2,tex3],[light1,light2,light3],TextureInterpolation);
end;

procedure TBGRADefaultBitmap.FillTriangleLinearMappingAntialias(pt1, pt2,
  pt3: TPointF; texture: IBGRAScanner; tex1, tex2, tex3: TPointF);
var
  mapping: TBGRATriangleLinearMapping;
begin
  mapping := TBGRATriangleLinearMapping.Create(texture, pt1,pt2,pt3, tex1, tex2, tex3);
  FillPolyAntialias([pt1,pt2,pt3],mapping);
  mapping.Free;
end;

procedure TBGRADefaultBitmap.FillQuadLinearColor(pt1, pt2, pt3, pt4: TPointF;
  c1, c2, c3, c4: TBGRAPixel);
var
  center: TPointF;
  centerColor: TBGRAPixel;
  multi: TBGRAMultishapeFiller;
begin
  if not IsConvex([pt1,pt2,pt3,pt4]) then //need to merge colors
  begin
    multi := TBGRAMultishapeFiller.Create;
    multi.AddQuadLinearColor(pt1,pt2,pt3,pt4,c1,c2,c3,c4);
    multi.Antialiasing:= false;
    multi.Draw(self);
    multi.Free;
    exit;
  end;
  center := (pt1+pt2+pt3+pt4)*(1/4);
  centerColor := GammaCompression( MergeBGRA(MergeBGRA(GammaExpansion(c1),GammaExpansion(c2)),
                    MergeBGRA(GammaExpansion(c3),GammaExpansion(c4))) );
  FillTriangleLinearColor(pt1,pt2,center, c1,c2,centerColor);
  FillTriangleLinearColor(pt2,pt3,center, c2,c3,centerColor);
  FillTriangleLinearColor(pt3,pt4,center, c3,c4,centerColor);
  FillTriangleLinearColor(pt4,pt1,center, c4,c1,centerColor);
end;

procedure TBGRADefaultBitmap.FillQuadLinearColorAntialias(pt1, pt2, pt3,
  pt4: TPointF; c1, c2, c3, c4: TBGRAPixel);
var multi : TBGRAMultishapeFiller;
begin
  multi := TBGRAMultishapeFiller.Create;
  multi.AddQuadLinearColor(pt1, pt2, pt3, pt4, c1, c2, c3, c4);
  multi.Draw(self);
  multi.free;
end;

procedure TBGRADefaultBitmap.FillQuadLinearMapping(pt1, pt2, pt3, pt4: TPointF;
  texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF; TextureInterpolation: Boolean= True);
var
  center: TPointF;
  centerTex: TPointF;
begin
  center := (pt1+pt2+pt3+pt4)*(1/4);
  centerTex := (tex1+tex2+tex3+tex4)*(1/4);
  FillTriangleLinearMapping(pt1,pt2,center, texture,tex1,tex2,centerTex, TextureInterpolation);
  FillTriangleLinearMapping(pt2,pt3,center, texture,tex2,tex3,centerTex, TextureInterpolation);
  FillTriangleLinearMapping(pt3,pt4,center, texture,tex3,tex4,centerTex, TextureInterpolation);
  FillTriangleLinearMapping(pt4,pt1,center, texture,tex4,tex1,centerTex, TextureInterpolation);
end;

procedure TBGRADefaultBitmap.FillQuadLinearMappingLightness(pt1, pt2, pt3,
  pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF; light1,
  light2, light3, light4: word; TextureInterpolation: Boolean);
var
  center: TPointF;
  centerTex: TPointF;
  centerLight: word;
begin
  center := (pt1+pt2+pt3+pt4)*(1/4);
  centerTex := (tex1+tex2+tex3+tex4)*(1/4);
  centerLight := (light1+light2+light3+light4) div 4;
  FillTriangleLinearMappingLightness(pt1,pt2,center, texture,tex1,tex2,centerTex, light1,light2,centerLight, TextureInterpolation);
  FillTriangleLinearMappingLightness(pt2,pt3,center, texture,tex2,tex3,centerTex, light2,light3,centerLight, TextureInterpolation);
  FillTriangleLinearMappingLightness(pt3,pt4,center, texture,tex3,tex4,centerTex, light3,light4,centerLight, TextureInterpolation);
  FillTriangleLinearMappingLightness(pt4,pt1,center, texture,tex4,tex1,centerTex, light4,light1,centerLight, TextureInterpolation);
end;

procedure TBGRADefaultBitmap.FillQuadLinearMappingAntialias(pt1, pt2, pt3,
  pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF);
var multi : TBGRAMultishapeFiller;
begin
  multi := TBGRAMultishapeFiller.Create;
  multi.AddQuadLinearMapping(pt1, pt2, pt3, pt4, texture, tex1,tex2,tex3,tex4);
  multi.Draw(self);
  multi.free;
end;

procedure TBGRADefaultBitmap.FillQuadPerspectiveMapping(pt1, pt2, pt3,
  pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF);
var
  persp: TBGRAPerspectiveScannerTransform;
begin
  persp := TBGRAPerspectiveScannerTransform.Create(texture,[tex1,tex2,tex3,tex4],[pt1,pt2,pt3,pt4]);
  FillPoly([pt1,pt2,pt3,pt4],persp,dmDrawWithTransparency);
  persp.Free;
end;

procedure TBGRADefaultBitmap.FillQuadPerspectiveMappingAntialias(pt1, pt2, pt3,
  pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF);
var
  persp: TBGRAPerspectiveScannerTransform;
begin
  persp := TBGRAPerspectiveScannerTransform.Create(texture,[tex1,tex2,tex3,tex4],[pt1,pt2,pt3,pt4]);
  FillPolyAntialias([pt1,pt2,pt3,pt4],persp);
  persp.Free;
end;

procedure TBGRADefaultBitmap.FillPolyLinearMapping(const points: array of TPointF;
  texture: IBGRAScanner; texCoords: array of TPointF;
  TextureInterpolation: Boolean);
begin
  PolygonLinearTextureMappingAliased(self,points,texture,texCoords,TextureInterpolation, FillMode = fmWinding);
end;

procedure TBGRADefaultBitmap.FillPolyLinearMappingLightness(
  const points: array of TPointF; texture: IBGRAScanner;
  texCoords: array of TPointF; lightnesses: array of word;
  TextureInterpolation: Boolean);
begin
  PolygonLinearTextureMappingAliasedWithLightness(self,points,texture,texCoords,TextureInterpolation,lightnesses,FillMode = fmWinding);
end;

procedure TBGRADefaultBitmap.FillPolyLinearColor(
  const points: array of TPointF; AColors: array of TBGRAPixel);
begin
  PolygonLinearColorGradientAliased(self,points,AColors, FillMode = fmWinding);
end;

procedure TBGRADefaultBitmap.FillPolyPerspectiveMapping(
  const points: array of TPointF; const pointsZ: array of single;
  texture: IBGRAScanner; texCoords: array of TPointF;
  TextureInterpolation: Boolean; zbuffer: psingle);
begin
  PolygonPerspectiveTextureMappingAliased(self,points,pointsZ,texture,texCoords,TextureInterpolation, FillMode = fmWinding, zbuffer);
end;

procedure TBGRADefaultBitmap.FillPolyPerspectiveMappingLightness(
  const points: array of TPointF; const pointsZ: array of single;
  texture: IBGRAScanner; texCoords: array of TPointF;
  lightnesses: array of word; TextureInterpolation: Boolean; zbuffer: psingle);
begin
  PolygonPerspectiveTextureMappingAliasedWithLightness(self,points,pointsZ,texture,texCoords,TextureInterpolation,lightnesses, FillMode = fmWinding, zbuffer);
end;

procedure TBGRADefaultBitmap.FillPoly(const points: array of TPointF;
  c: TBGRAPixel; drawmode: TDrawMode);
begin
  BGRAPolygon.FillPolyAliased(self, points, c, FEraseMode, FillMode = fmWinding, drawmode);
end;

procedure TBGRADefaultBitmap.FillPoly(const points: array of TPointF;
  texture: IBGRAScanner; drawmode: TDrawMode);
begin
  BGRAPolygon.FillPolyAliasedWithTexture(self, points, texture, FillMode = fmWinding, drawmode);
end;

procedure TBGRADefaultBitmap.EraseLineAntialias(x1, y1, x2, y2: single;
  alpha: byte; w: single);
begin
  FEraseMode := True;
  DrawLineAntialias(x1,y1,x2,y2, BGRA(0,0,0,alpha),w);
  FEraseMode := False;
end;

procedure TBGRADefaultBitmap.FillPolyAntialias(const points: array of TPointF; c: TBGRAPixel);
begin
  BGRAPolygon.FillPolyAntialias(self, points, c, FEraseMode, FillMode = fmWinding);
end;

procedure TBGRADefaultBitmap.FillPolyAntialias(const points: array of TPointF;
  texture: IBGRAScanner);
begin
  BGRAPolygon.FillPolyAntialiasWithTexture(self, points, texture, FillMode = fmWinding);
end;

procedure TBGRADefaultBitmap.ErasePoly(const points: array of TPointF;
  alpha: byte);
begin
  BGRAPolygon.FillPolyAliased(self, points, BGRA(0, 0, 0, alpha), True, FillMode = fmWinding, dmDrawWithTransparency);
end;

procedure TBGRADefaultBitmap.ErasePolyAntialias(const points: array of TPointF; alpha: byte);
begin
  FEraseMode := True;
  FillPolyAntialias(points, BGRA(0, 0, 0, alpha));
  FEraseMode := False;
end;

procedure TBGRADefaultBitmap.EllipseAntialias(x, y, rx, ry: single;
  c: TBGRAPixel; w: single);
begin
  if IsClearPenStyle(FCustomPenStyle) or (c.alpha = 0) then exit;
  if IsSolidPenStyle(FCustomPenStyle) then
    BGRAPolygon.BorderEllipseAntialias(self, x, y, rx, ry, w, c, FEraseMode)
  else
    DrawPolygonAntialias(ComputeEllipseContour(x,y,rx,ry),c,w);
end;

procedure TBGRADefaultBitmap.EllipseAntialias(x, y, rx, ry: single;
  texture: IBGRAScanner; w: single);
begin
  if IsClearPenStyle(FCustomPenStyle) then exit;
  if IsSolidPenStyle(FCustomPenStyle) then
    BGRAPolygon.BorderEllipseAntialiasWithTexture(self, x, y, rx, ry, w, texture)
  else
    DrawPolygonAntialias(ComputeEllipseContour(x,y,rx,ry),texture,w);
end;

procedure TBGRADefaultBitmap.EllipseAntialias(x, y, rx, ry: single;
  c: TBGRAPixel; w: single; back: TBGRAPixel);
var multi: TBGRAMultishapeFiller;
    hw: single;
begin
  if w=0 then exit;
  rx := abs(rx);
  ry := abs(ry);
  hw := w/2;
  if (rx <= hw) or (ry <= hw) then
  begin
    FillEllipseAntialias(x,y,rx+hw,ry+hw,c);
    exit;
  end;
  { use multishape filler for fine junction between polygons }
  multi := TBGRAMultishapeFiller.Create;
  if not IsClearPenStyle(FCustomPenStyle) and (c.alpha <> 0) then
  begin
    if IsSolidPenStyle(FCustomPenStyle) then
    begin
      multi.AddEllipse(x,y,rx-hw,ry-hw,back);
      multi.AddEllipseBorder(x,y,rx,ry,w,c)
    end
    else
    begin
      multi.AddEllipse(x,y,rx,ry,back);
      multi.AddPolygon(ComputeWidePolygon(ComputeEllipseContour(x,y,rx,ry),w),c);
      multi.PolygonOrder := poLastOnTop;
    end;
  end;
  multi.Draw(self);
  multi.Free;
end;

procedure TBGRADefaultBitmap.FillEllipseAntialias(x, y, rx, ry: single; c: TBGRAPixel);
begin
  BGRAPolygon.FillEllipseAntialias(self, x, y, rx, ry, c, FEraseMode);
end;

procedure TBGRADefaultBitmap.FillEllipseAntialias(x, y, rx, ry: single;
  texture: IBGRAScanner);
begin
  BGRAPolygon.FillEllipseAntialiasWithTexture(self, x, y, rx, ry, texture);
end;

procedure TBGRADefaultBitmap.FillEllipseLinearColorAntialias(x, y, rx,
  ry: single; outercolor, innercolor: TBGRAPixel);
var
    grad: TBGRAGradientScanner;
    affine: TBGRAAffineScannerTransform;
begin
  if (rx=0) or (ry=0) then exit;
  if rx=ry then
  begin
    grad := TBGRAGradientScanner.Create(innercolor,outercolor,gtRadial,PointF(x,y),PointF(x+rx,y),True);
    FillEllipseAntialias(x,y,rx,ry,grad);
    grad.Free;
  end else
  begin
    grad := TBGRAGradientScanner.Create(innercolor,outercolor,gtRadial,PointF(0,0),PointF(1,0),True);
    affine := TBGRAAffineScannerTransform.Create(grad);
    affine.Scale(rx,ry);
    affine.Translate(x,y);
    FillEllipseAntialias(x,y,rx,ry,affine);
    affine.Free;
    grad.Free;
  end;
end;

procedure TBGRADefaultBitmap.EraseEllipseAntialias(x, y, rx, ry: single; alpha: byte);
begin
  FEraseMode := True;
  FillEllipseAntialias(x, y, rx, ry, BGRA(0, 0, 0, alpha));
  FEraseMode := False;
end;

procedure TBGRADefaultBitmap.RectangleAntialias(x, y, x2, y2: single;
  c: TBGRAPixel; w: single; back: TBGRAPixel);
var
  bevel: single;
  multi: TBGRAMultishapeFiller;
  hw: single;
begin
  if IsClearPenStyle(FCustomPenStyle) or (c.alpha=0) or (w=0) then
  begin
    if back <> BGRAPixelTransparent then
      FillRectAntialias(x,y,x2,y2,back);
    exit;
  end;

  hw := w/2;
  if not CheckAntialiasRectBounds(x,y,x2,y2,w) then
  begin
    if JoinStyle = pjsBevel then
    begin
      bevel := (2-sqrt(2))*hw;
      FillRoundRectAntialias(x - hw, y - hw, x2 + hw, y2 + hw, bevel,bevel, c, [rrTopLeftBevel, rrTopRightBevel, rrBottomLeftBevel, rrBottomRightBevel]);
    end else
    if JoinStyle = pjsRound then
     FillRoundRectAntialias(x - hw, y - hw, x2 + hw, y2 + hw, hw,hw, c)
    else
     FillRectAntialias(x - hw, y - hw, x2 + hw, y2 + hw, c);
    exit;
  end;

  { use multishape filler for fine junction between polygons }
  multi := TBGRAMultishapeFiller.Create;
  multi.FillMode := FillMode;
  if (JoinStyle = pjsMiter) and IsSolidPenStyle(FCustomPenStyle) then
    multi.AddRectangleBorder(x,y,x2,y2,w,c)
  else
    multi.AddPolygon(ComputeWidePolygon([Pointf(x,y),Pointf(x2,y),Pointf(x2,y2),Pointf(x,y2)],w),c);

  if (frac(x + hw) = 0.5) and (frac(y + hw)=0.5) and (frac(x2 - hw)=0.5) and (frac(y2 - hw)=0.5) then
    FillRect(ceil(x + hw), ceil(y + hw), ceil(x2 - hw), ceil(y2 - hw), back, dmDrawWithTransparency)
  else
    multi.AddRectangle(x + hw, y + hw, x2 - hw, y2 - hw, back);
  multi.Draw(self);
  multi.Free;
end;

procedure TBGRADefaultBitmap.RectangleAntialias(x, y, x2, y2: single;
  texture: IBGRAScanner; w: single);
var
  bevel,hw: single;
  multi: TBGRAMultishapeFiller;
begin
  if IsClearPenStyle(FCustomPenStyle) or (w=0) then exit;

  hw := w/2;
  if not CheckAntialiasRectBounds(x,y,x2,y2,w) then
  begin
    if JoinStyle = pjsBevel then
    begin
      bevel := (2-sqrt(2))*hw;
      FillRoundRectAntialias(x - hw, y - hw, x2 + hw, y2 + hw, bevel,bevel, texture, [rrTopLeftBevel, rrTopRightBevel, rrBottomLeftBevel, rrBottomRightBevel]);
    end else
    if JoinStyle = pjsRound then
     FillRoundRectAntialias(x - hw, y - hw, x2 + hw, y2 + hw, hw,hw, texture)
    else
     FillRectAntialias(x - hw, y - hw, x2 + hw, y2 + hw, texture);
    exit;
  end;

  { use multishape filler for fine junction between polygons }
  multi := TBGRAMultishapeFiller.Create;
  multi.FillMode := FillMode;
  if (JoinStyle = pjsMiter) and IsSolidPenStyle(FCustomPenStyle) then
    multi.AddRectangleBorder(x,y,x2,y2,w, texture)
  else
    multi.AddPolygon(ComputeWidePolygon([Pointf(x,y),Pointf(x2,y),Pointf(x2,y2),Pointf(x,y2)],w), texture);
  multi.Draw(self);
  multi.Free;
end;

procedure TBGRADefaultBitmap.RoundRectAntialias(x, y, x2, y2, rx, ry: single;
   c: TBGRAPixel; w: single; options: TRoundRectangleOptions);
begin
  if IsClearPenStyle(FCustomPenStyle) or (c.alpha = 0) then exit;
  if IsSolidPenStyle(FCustomPenStyle) then
    BGRAPolygon.BorderRoundRectangleAntialias(self,x,y,x2,y2,rx,ry,w,options,c,False)
  else
    DrawPolygonAntialias(BGRAPath.ComputeRoundRect(x,y,x2,y2,rx,ry,options),c,w);
end;

procedure TBGRADefaultBitmap.RoundRectAntialias(x, y, x2, y2, rx, ry: single;
  pencolor: TBGRAPixel; w: single; fillcolor: TBGRAPixel;
  options: TRoundRectangleOptions);
var
  multi: TBGRAMultishapeFiller;
begin
  if IsClearPenStyle(FCustomPenStyle) or (pencolor.alpha = 0) then
  begin
    FillRoundRectAntialias(x,y,x2,y2,rx,ry,fillColor,options);
    exit;
  end;
  if IsSolidPenStyle(FCustomPenStyle) then
    BGRAPolygon.BorderAndFillRoundRectangleAntialias(self,x,y,x2,y2,rx,ry,w,options,pencolor,fillcolor,nil,nil,False)
  else
  begin
    multi := TBGRAMultishapeFiller.Create;
    multi.PolygonOrder := poLastOnTop;
    multi.AddRoundRectangle(x,y,x2,y2,rx,ry,fillColor,options);
    multi.AddPolygon(ComputeWidePolygon(BGRAPath.ComputeRoundRect(x,y,x2,y2,rx,ry,options),w),pencolor);
    multi.Draw(self);
    multi.Free;
  end;
end;

procedure TBGRADefaultBitmap.RoundRectAntialias(x, y, x2, y2, rx, ry: single;
  penTexture: IBGRAScanner; w: single; fillTexture: IBGRAScanner;
  options: TRoundRectangleOptions);
var
  multi: TBGRAMultishapeFiller;
begin
  if IsClearPenStyle(FCustomPenStyle) then
  begin
    FillRoundRectAntialias(x,y,x2,y2,rx,ry,fillTexture,options);
    exit;
  end else
  if IsSolidPenStyle(FCustomPenStyle) then
    BGRAPolygon.BorderAndFillRoundRectangleAntialias(self,x,y,x2,y2,rx,ry,w,options,BGRAPixelTransparent,BGRAPixelTransparent,pentexture,filltexture,False)
  else
  begin
    multi := TBGRAMultishapeFiller.Create;
    multi.PolygonOrder := poLastOnTop;
    multi.AddRoundRectangle(x,y,x2,y2,rx,ry,fillTexture,options);
    multi.AddPolygon(ComputeWidePolygon(ComputeRoundRect(x,y,x2,y2,rx,ry,options),w),penTexture);
    multi.Draw(self);
    multi.Free;
  end;
end;

procedure TBGRADefaultBitmap.RoundRectAntialias(x, y, x2, y2, rx, ry: single;
  texture: IBGRAScanner; w: single; options: TRoundRectangleOptions);
begin
  if IsClearPenStyle(FCustomPenStyle) then exit;
  if IsSolidPenStyle(FCustomPenStyle) then
    BGRAPolygon.BorderRoundRectangleAntialiasWithTexture(self,x,y,x2,y2,rx,ry,w,options,texture)
  else
    DrawPolygonAntialias(BGRAPath.ComputeRoundRect(x,y,x2,y2,rx,ry,options),texture,w);
end;

function TBGRADefaultBitmap.CheckRectBounds(var x, y, x2, y2: integer; minsize: integer): boolean; inline;
var
  temp: integer;
begin
  //swap coordinates if needed
  if (x > x2) then
  begin
    temp := x;
    x    := x2;
    x2   := temp;
  end;
  if (y > y2) then
  begin
    temp := y;
    y    := y2;
    y2   := temp;
  end;
  if (x2 - x <= minsize) or (y2 - y <= minsize) then
  begin
    result := false;
    exit;
  end else
    result := true;
end;

procedure TBGRADefaultBitmap.Rectangle(x, y, x2, y2: integer;
  c: TBGRAPixel; mode: TDrawMode);
begin
  if not CheckRectBounds(x,y,x2,y2,1) then exit;
  case mode of
    dmFastBlend:
    begin
      FastBlendHorizLine(x, y, x2 - 1, c);
      FastBlendHorizLine(x, y2 - 1, x2 - 1, c);
      if y2 - y > 2 then
      begin
        FastBlendVertLine(x, y + 1, y2 - 2, c);
        FastBlendVertLine(x2 - 1, y + 1, y2 - 2, c);
      end;
    end;
    dmDrawWithTransparency:
    begin
      DrawHorizLine(x, y, x2 - 1, c);
      DrawHorizLine(x, y2 - 1, x2 - 1, c);
      if y2 - y > 2 then
      begin
        DrawVertLine(x, y + 1, y2 - 2, c);
        DrawVertLine(x2 - 1, y + 1, y2 - 2, c);
      end;
    end;
    dmSet:
    begin
      SetHorizLine(x, y, x2 - 1, c);
      SetHorizLine(x, y2 - 1, x2 - 1, c);
      if y2 - y > 2 then
      begin
        SetVertLine(x, y + 1, y2 - 2, c);
        SetVertLine(x2 - 1, y + 1, y2 - 2, c);
      end;
    end;
    dmXor:
    begin
      XorHorizLine(x, y, x2 - 1, c);
      XorHorizLine(x, y2 - 1, x2 - 1, c);
      if y2 - y > 2 then
      begin
        XorVertLine(x, y + 1, y2 - 2, c);
        XorVertLine(x2 - 1, y + 1, y2 - 2, c);
      end;
    end;
    dmSetExceptTransparent: if (c.alpha = 255) then
        Rectangle(x, y, x2, y2, c, dmSet);
  end;
end;

procedure TBGRADefaultBitmap.Rectangle(x, y, x2, y2: integer;
  BorderColor, FillColor: TBGRAPixel; mode: TDrawMode);
begin
  if not CheckRectBounds(x,y,x2,y2,1) then exit;
  Rectangle(x, y, x2, y2, BorderColor, mode);
  FillRect(x + 1, y + 1, x2 - 1, y2 - 1, FillColor, mode);
end;

function TBGRADefaultBitmap.CheckClippedRectBounds(var x, y, x2, y2: integer): boolean; inline;
var
  temp: integer;
begin
  if (x > x2) then
  begin
    temp := x;
    x    := x2;
    x2   := temp;
  end;
  if (y > y2) then
  begin
    temp := y;
    y    := y2;
    y2   := temp;
  end;
  if (x >= FClipRect.Right) or (x2 <= FClipRect.Left) or (y >= FClipRect.Bottom) or (y2 <= FClipRect.Top) then
  begin
    result := false;
    exit;
  end;
  if x < FClipRect.Left then
    x := FClipRect.Left;
  if x2 > FClipRect.Right then
    x2 := FClipRect.Right;
  if y < FClipRect.Top then
    y := FClipRect.Top;
  if y2 > FClipRect.Bottom then
    y2 := FClipRect.Bottom;
  if (x2 - x <= 0) or (y2 - y <= 0) then
  begin
    result := false;
    exit;
  end else
    result := true;
end;

procedure TBGRADefaultBitmap.FillRect(x, y, x2, y2: integer; c: TBGRAPixel;
  mode: TDrawMode);
var
  yb, tx, delta: integer;
  p: PBGRAPixel;
begin
  if not CheckClippedRectBounds(x,y,x2,y2) then exit;
  tx := x2 - x;
  Dec(x2);
  Dec(y2);

  if mode = dmSetExceptTransparent then
  begin
    if (c.alpha = 255) then
      FillRect(x, y, x2, y2, c, dmSet);
  end else
  begin
    if (mode <> dmSet) and (c.alpha = 0) then exit;

    p := Scanline[y] + x;
    if FLineOrder = riloBottomToTop then
      delta := -Width
    else
      delta := Width;

    case mode of
      dmFastBlend:
        for yb := y2 - y downto 0 do
        begin
          FastBlendPixelsInline(p, c, tx);
          Inc(p, delta);
        end;
      dmDrawWithTransparency:
        for yb := y2 - y downto 0 do
        begin
          DrawPixelsInline(p, c, tx);
          Inc(p, delta);
        end;
      dmSet:
        for yb := y2 - y downto 0 do
        begin
          FillInline(p, c, tx);
          Inc(p, delta);
        end;
      dmXor:
        for yb := y2 - y downto 0 do
        begin
          XorInline(p, c, tx);
          Inc(p, delta);
        end;
    end;

    InvalidateBitmap;
  end;
end;

procedure TBGRADefaultBitmap.FillRect(x, y, x2, y2: integer;
  texture: IBGRAScanner; mode: TDrawMode);
var
  yb, tx, delta: integer;
  p: PBGRAPixel;
begin
  if not CheckClippedRectBounds(x,y,x2,y2) then exit;
  tx := x2 - x;
  Dec(x2);
  Dec(y2);

  p := Scanline[y] + x;
  if FLineOrder = riloBottomToTop then
    delta := -Width
  else
    delta := Width;

  for yb := y to y2 do
  begin
    texture.ScanMoveTo(x,yb);
    ScannerPutPixels(texture, p, tx, mode);
    Inc(p, delta);
  end;

  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.AlphaFillRect(x, y, x2, y2: integer; alpha: byte);
var
  yb, tx, delta: integer;
  p: PBGRAPixel;
begin
  if alpha = 0 then
  begin
    FillRect(x, y, x2, y2, BGRAPixelTransparent, dmSet);
    exit;
  end;

  if not CheckClippedRectBounds(x,y,x2,y2) then exit;
  tx := x2 - x;
  Dec(x2);
  Dec(y2);

  p := Scanline[y] + x;
  if FLineOrder = riloBottomToTop then
    delta := -Width
  else
    delta := Width;
  for yb := y2 - y downto 0 do
  begin
    AlphaFillInline(p, alpha, tx);
    Inc(p, delta);
  end;
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.FillRectAntialias(x, y, x2, y2: single; c: TBGRAPixel);
var tx,ty: single;
begin
  tx := x2-x;
  ty := y2-y;
  if (tx=0) or (ty=0) then exit;
  if (abs(tx) > 2) and (abs(ty) > 2) then
  begin
    if (tx < 0) then
    begin
      tx := -tx;
      x := x2;
      x2 := x+tx;
    end;
    if (ty < 0) then
    begin
      ty := -ty;
      y := y2;
      y2 := y+ty;
    end;
    FillRectAntialias(x,y,x2,ceil(y)+0.5,c);
    FillRectAntialias(x,ceil(y)+0.5,ceil(x)+0.5,floor(y2)-0.5,c);
    FillRectAntialias(floor(x2)-0.5,ceil(y)+0.5,x2,floor(y2)-0.5,c);
    FillRectAntialias(x,floor(y2)-0.5,x2,y2,c);
    FillRect(ceil(x)+1,ceil(y)+1,floor(x2),floor(y2),c,dmDrawWithTransparency);
  end else
    FillPolyAntialias([pointf(x, y), pointf(x2, y), pointf(x2, y2), pointf(x, y2)], c);
end;

procedure TBGRADefaultBitmap.EraseRectAntialias(x, y, x2, y2: single;
  alpha: byte);
begin
  ErasePolyAntialias([pointf(x, y), pointf(x2, y), pointf(x2, y2), pointf(x, y2)], alpha);
end;

procedure TBGRADefaultBitmap.FillRectAntialias(x, y, x2, y2: single;
  texture: IBGRAScanner);
begin
  FillPolyAntialias([pointf(x, y), pointf(x2, y), pointf(x2, y2), pointf(x, y2)], texture);
end;

procedure TBGRADefaultBitmap.FillRoundRectAntialias(x, y, x2, y2, rx,ry: single;
  c: TBGRAPixel; options: TRoundRectangleOptions);
begin
  BGRAPolygon.FillRoundRectangleAntialias(self,x,y,x2,y2,rx,ry,options,c,False);
end;

procedure TBGRADefaultBitmap.FillRoundRectAntialias(x, y, x2, y2, rx,
  ry: single; texture: IBGRAScanner; options: TRoundRectangleOptions);
begin
  BGRAPolygon.FillRoundRectangleAntialiasWithTexture(self,x,y,x2,y2,rx,ry,options,texture);
end;

procedure TBGRADefaultBitmap.EraseRoundRectAntialias(x, y, x2, y2, rx,
  ry: single; alpha: byte; options: TRoundRectangleOptions);
begin
  BGRAPolygon.FillRoundRectangleAntialias(self,x,y,x2,y2,rx,ry,options,BGRA(0,0,0,alpha),True);
end;

procedure TBGRADefaultBitmap.RoundRect(X1, Y1, X2, Y2: integer;
  DX, DY: integer; BorderColor, FillColor: TBGRAPixel);
begin
  BGRARoundRectAliased(self,X1,Y1,X2,Y2,DX,DY,BorderColor,FillColor);
end;

{------------------------- Text functions ---------------------------------------}

procedure TBGRADefaultBitmap.TextOutAngle(x, y: single; orientation: integer;
  s: string; c: TBGRAPixel; align: TAlignment);
begin
  UpdateFont;
  BGRAText.BGRATextOutAngle(self,FFont,FontQuality,x,y,orientation,s,c,nil,align);
end;

procedure TBGRADefaultBitmap.TextOutAngle(x, y: single; orientation: integer;
  s: string; texture: IBGRAScanner; align: TAlignment);
begin
  UpdateFont;
  BGRAText.BGRATextOutAngle(self,FFont,FontQuality,x,y,orientation,s,BGRAPixelTransparent,texture,align);
end;

procedure TBGRADefaultBitmap.TextOut(x, y: single; s: string;
  texture: IBGRAScanner; align: TAlignment);
begin
  UpdateFont;

  if (FontQuality in[fqFineAntialiasing,fqFineClearTypeBGR,fqFineClearTypeRGB]) and (FFont.Orientation mod 3600 = 0) then
    BGRATextFX.BGRATextOutImproveReadability(self,FFont,x,y,s,BGRAPixelTransparent,texture,align,
     FontQuality in[fqFineClearTypeRGB,fqFineClearTypeBGR], FontQuality = fqFineClearTypeRGB) else

    BGRAText.BGRATextOut(self,FFont,FontQuality,x,y,s,BGRAPixelTransparent,texture,align);
end;

procedure TBGRADefaultBitmap.TextOut(x, y: single; s: string;
  c: TBGRAPixel; align: TAlignment);
begin
  UpdateFont;

  if (FontQuality in[fqFineAntialiasing,fqFineClearTypeBGR,fqFineClearTypeRGB]) and (FFont.Orientation mod 3600 = 0) then
    BGRATextFX.BGRATextOutImproveReadability(self,FFont,x,y,s,c,nil,align,
    FontQuality in[fqFineClearTypeRGB,fqFineClearTypeBGR], FontQuality = fqFineClearTypeRGB) else

    BGRAText.BGRATextOut(self,FFont,FontQuality,x,y,s,c,nil,align);
end;

procedure TBGRADefaultBitmap.TextRect(ARect: TRect; x, y: integer;
  s: string; style: TTextStyle; c: TBGRAPixel);
begin
  UpdateFont;
  BGRAText.BGRATextRect(self,FFont,FontQuality,ARect,x,y,s,style,c,nil);
end;

procedure TBGRADefaultBitmap.TextRect(ARect: TRect; x, y: integer; s: string;
  style: TTextStyle; texture: IBGRAScanner);
begin
  UpdateFont;
  BGRAText.BGRATextRect(self,FFont,FontQuality,ARect,x,y,s,style,BGRAPixelTransparent,texture);
end;

function TBGRADefaultBitmap.TextSize(s: string): TSize;
begin
  UpdateFont;
  result := BGRAText.BGRATextSize(FFont,FontQuality,s,FontAntialiasingLevel);
  if (result.cy >= 24) and FontAntialias then
    result := BGRAText.BGRATextSize(FFont,FontQuality,s,4);
end;

{---------------------------- Curves ----------------------------------------}

function TBGRADefaultBitmap.ComputeClosedSpline(const APoints: array of TPointF; AStyle: TSplineStyle): ArrayOfTPointF;
begin
  result := BGRAPath.ComputeClosedSpline(APoints, AStyle);
end;

function TBGRADefaultBitmap.ComputeOpenedSpline(const APoints: array of TPointF; AStyle: TSplineStyle): ArrayOfTPointF;
begin
  result := BGRAPath.ComputeOpenedSpline(APoints, AStyle);
end;

function TBGRADefaultBitmap.ComputeBezierCurve(const ACurve: TCubicBezierCurve
  ): ArrayOfTPointF;
begin
  Result:= BGRAPath.ComputeBezierCurve(ACurve);
end;

function TBGRADefaultBitmap.ComputeBezierCurve(
  const ACurve: TQuadraticBezierCurve): ArrayOfTPointF;
begin
  Result:= BGRAPath.ComputeBezierCurve(ACurve);
end;

function TBGRADefaultBitmap.ComputeBezierSpline(
  const ASpline: array of TCubicBezierCurve): ArrayOfTPointF;
begin
  Result:= BGRAPath.ComputeBezierSpline(ASpline);
end;

function TBGRADefaultBitmap.ComputeBezierSpline(
  const ASpline: array of TQuadraticBezierCurve): ArrayOfTPointF;
begin
  Result:= BGRAPath.ComputeBezierSpline(ASpline);
end;

function TBGRADefaultBitmap.ComputeWidePolyline(const points: array of TPointF;
  w: single): ArrayOfTPointF;
begin
  Result:= BGRAPen.ComputeWidePolylinePoints(points,w,BGRAWhite,LineCap,JoinStyle,FCustomPenStyle,[],JoinMiterLimit);
end;

function TBGRADefaultBitmap.ComputeWidePolyline(const points: array of TPointF;
  w: single; Closed: boolean): ArrayOfTPointF;
var
  options: TBGRAPolyLineOptions;
begin
  if not closed then options := [plRoundCapOpen] else options := [];
  Result:= BGRAPen.ComputeWidePolylinePoints(points,w,BGRAWhite,pecRound,pjsRound,FCustomPenStyle,options,JoinMiterLimit);
end;

function TBGRADefaultBitmap.ComputeWidePolygon(const points: array of TPointF;
  w: single): ArrayOfTPointF;
begin
  Result:= BGRAPen.ComputeWidePolylinePoints(points,w,BGRAWhite,LineCap,JoinStyle,FCustomPenStyle,[plCycle],JoinMiterLimit);
end;

function TBGRADefaultBitmap.ComputeEllipseContour(x, y, rx, ry: single; quality: single): ArrayOfTPointF;
begin
  result := BGRAPath.ComputeEllipse(x,y,rx,ry, quality);
end;

function TBGRADefaultBitmap.ComputeEllipseBorder(x, y, rx, ry, w: single; quality: single): ArrayOfTPointF;
begin
  result := ComputeWidePolygon(ComputeEllipseContour(x,y,rx,ry, quality),w);
end;

function TBGRADefaultBitmap.ComputeArc65536(x, y, rx, ry: single; start65536,
  end65536: word; quality: single): ArrayOfTPointF;
begin
  result := BGRAPath.ComputeArc65536(x,y,rx,ry,start65536,end65536,quality);
end;

function TBGRADefaultBitmap.ComputeArcRad(x, y, rx, ry: single; startRad,
  endRad: single; quality: single): ArrayOfTPointF;
begin
  result := BGRAPath.ComputeArc65536(x,y,rx,ry,round(startRad*32768/Pi),round(endRad*32768/Pi),quality);
end;

function TBGRADefaultBitmap.ComputeRoundRect(x1, y1, x2, y2, rx, ry: single; quality: single): ArrayOfTPointF;
begin
  result := BGRAPath.ComputeRoundRect(x1,y1,x2,y2,rx,ry,quality);
end;

function TBGRADefaultBitmap.ComputeRoundRect(x1, y1, x2, y2, rx, ry: single;
  options: TRoundRectangleOptions; quality: single): ArrayOfTPointF;
begin
  Result:= BGRAPath.ComputeRoundRect(x1,y1,x2,y2,rx,ry,options,quality);
end;

function TBGRADefaultBitmap.ComputePie65536(x, y, rx, ry: single; start65536,
  end65536: word; quality: single): ArrayOfTPointF;
begin
  result := BGRAPath.ComputeArc65536(x,y,rx,ry,start65536,end65536,quality);
  if (start65536 <> end65536) then
  begin
    setlength(result,length(result)+1);
    result[high(result)] := PointF(x,y);
  end;
end;

function TBGRADefaultBitmap.ComputePieRad(x, y, rx, ry: single; startRad,
  endRad: single; quality: single): ArrayOfTPointF;
begin
  result := self.ComputePie65536(x,y,rx,ry,round(startRad*32768/Pi),round(endRad*32768/Pi),quality);
end;

{---------------------------------- Fill ---------------------------------}

procedure TBGRADefaultBitmap.Fill(texture: IBGRAScanner);
begin
  FillRect(FClipRect.Left,FClipRect.Top,FClipRect.Right,FClipRect.Bottom,texture,dmSet);
end;

procedure TBGRADefaultBitmap.Fill(c: TBGRAPixel; start, Count: integer);
begin
  if start < 0 then
  begin
    Count += start;
    start := 0;
  end;
  if start >= nbPixels then
    exit;
  if start + Count > nbPixels then
    Count := nbPixels - start;

  FillInline(Data + start, c, Count);
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.AlphaFill(alpha: byte; start, Count: integer);
begin
  if alpha = 0 then
    Fill(BGRAPixelTransparent, start, Count);
  if start < 0 then
  begin
    Count += start;
    start := 0;
  end;
  if start >= nbPixels then
    exit;
  if start + Count > nbPixels then
    Count := nbPixels - start;

  AlphaFillInline(Data + start, alpha, Count);
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.FillMask(x, y: integer; AMask: TBGRACustomBitmap;
  color: TBGRAPixel);
var
  scan: TBGRACustomScanner;
begin
  if (AMask = nil) or (color.alpha = 0) then exit;
  scan := TBGRASolidColorMaskScanner.Create(AMask,Point(-X,-Y),color);
  self.FillRect(X,Y,X+AMask.Width,Y+AMask.Height,scan,dmDrawWithTransparency);
  scan.Free;
end;

procedure TBGRADefaultBitmap.FillMask(x, y: integer; AMask: TBGRACustomBitmap;
  texture: IBGRAScanner);
var
  scan: TBGRACustomScanner;
begin
  if AMask = nil then exit;
  scan := TBGRATextureMaskScanner.Create(AMask,Point(-X,-Y),texture);
  self.FillRect(X,Y,X+AMask.Width,Y+AMask.Height,scan,dmDrawWithTransparency);
  scan.Free;
end;

procedure TBGRADefaultBitmap.FillClearTypeMask(x, y: integer; xThird: integer;
  AMask: TBGRACustomBitmap; color: TBGRAPixel; ARGBOrder: boolean);
begin
  BGRAFillClearTypeMask(self,x, y, xThird, AMask, color, nil, ARGBOrder);
end;

procedure TBGRADefaultBitmap.FillClearTypeMask(x, y: integer; xThird: integer;
  AMask: TBGRACustomBitmap; texture: IBGRAScanner; ARGBOrder: boolean);
begin
  BGRAFillClearTypeMask(self,x, y, xThird, AMask, BGRAPixelTransparent, texture, ARGBOrder);
end;

{ Replace color without taking alpha channel into account }
procedure TBGRADefaultBitmap.ReplaceColor(before, after: TColor);
var
  p: PLongWord;
  n: integer;
  colorMask,beforeBGR, afterBGR: longword;
begin
  colorMask := NtoLE($00FFFFFF);
  beforeBGR := NtoLE((before and $FF shl 16) + (before and $FF00) + (before shr 16 and $FF));
  afterBGR  := NtoLE((after and $FF shl 16) + (after and $FF00) + (after shr 16 and $FF));

  p := PLongWord(Data);
  for n := NbPixels - 1 downto 0 do
  begin
    if p^ and colorMask = beforeBGR then
      p^ := (p^ and not ColorMask) or afterBGR;
    Inc(p);
  end;
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.ReplaceColor(before, after: TBGRAPixel);
var
  p: PBGRAPixel;
  n: integer;
begin
  if before.alpha = 0 then
  begin
    ReplaceTransparent(after);
    exit;
  end;
  p := Data;
  for n := NbPixels - 1 downto 0 do
  begin
    if p^ = before then
      p^ := after;
    Inc(p);
  end;
  InvalidateBitmap;
end;

{ Replace transparent pixels by the specified color }
procedure TBGRADefaultBitmap.ReplaceTransparent(after: TBGRAPixel);
var
  p: PBGRAPixel;
  n: integer;
begin
  p := Data;
  for n := NbPixels - 1 downto 0 do
  begin
    if p^.alpha = 0 then
      p^ := after;
    Inc(p);
  end;
  InvalidateBitmap;
end;

{ General purpose FloodFill. It can be used to fill inplace or to
  fill a destination bitmap according to the content of the current bitmap.

  The first pixel encountered is taken as a reference, further pixels
  are compared to this pixel. If the distance between next colors and
  the first color is lower than the tolerance, then the floodfill continues.

  It uses an array of bits to store visited places to avoid filling twice
  the same area. It also uses a stack of positions to remember where
  to continue after a place is completely filled.

  The first direction to be checked is horizontal, then
  it checks pixels on the line above and on the line below. }
procedure TBGRADefaultBitmap.ParallelFloodFill(X, Y: integer;
  Dest: TBGRACustomBitmap; Color: TBGRAPixel; mode: TFloodfillMode;
  Tolerance: byte);
var
  S:     TBGRAPixel;
  SX, EX, I: integer;
  Added: boolean;

  Visited: array of longword;
  VisitedLineSize: integer;

  Stack:      array of integer;
  StackCount: integer;

  function CheckPixel(AX, AY: integer): boolean; inline;
  var
    ComparedColor: TBGRAPixel;
  begin
    if Visited[AX shr 5 + AY * VisitedLineSize] and (1 shl (AX and 31)) <> 0 then
      Result := False
    else
    begin
      ComparedColor := GetPixel(AX, AY);
      Result := BGRADiff(ComparedColor, S) <= Tolerance;
    end;
  end;

  procedure SetVisited(X1, AY, X2: integer);
  var
    StartMask, EndMask: longword;
    StartPos, EndPos:   integer;
  begin
    if X2 < X1 then
      exit;
    StartMask := $FFFFFFFF shl (X1 and 31);
    if X2 and 31 = 31 then
      EndMask := $FFFFFFFF
    else
      EndMask := 1 shl ((X2 and 31) + 1) - 1;
    StartPos := X1 shr 5 + AY * VisitedLineSize;
    EndPos := X2 shr 5 + AY * VisitedLineSize;
    if StartPos = EndPos then
      Visited[StartPos] := Visited[StartPos] or (StartMask and EndMask)
    else
    begin
      Visited[StartPos] := Visited[StartPos] or StartMask;
      Visited[EndPos]   := Visited[EndPos] or EndMask;
      if EndPos - StartPos > 1 then
        FillDWord(Visited[StartPos + 1], EndPos - StartPos - 1, $FFFFFFFF);
    end;
  end;

  procedure Push(AX, AY: integer); inline;
  begin
    if StackCount + 1 >= High(Stack) then
      SetLength(Stack, Length(Stack) shl 1);

    Stack[StackCount] := AX;
    Inc(StackCount);
    Stack[StackCount] := AY;
    Inc(StackCount);
  end;

  procedure Pop(var AX, AY: integer); inline;
  begin
    Dec(StackCount);
    AY := Stack[StackCount];
    Dec(StackCount);
    AX := Stack[StackCount];
  end;

begin
  if PtInClipRect(X,Y) then
  begin
    S := GetPixel(X, Y);

    VisitedLineSize := (Width + 31) shr 5;
    SetLength(Visited, VisitedLineSize * Height);
    FillDWord(Visited[0], Length(Visited), 0);

    SetLength(Stack, 2);
    StackCount := 0;

    Push(X, Y);
    repeat
      Pop(X, Y);
      if not CheckPixel(X, Y) then
        Continue;

      SX := X;
      while (SX > FClipRect.Left) and CheckPixel(Pred(SX), Y) do
        Dec(SX);
      EX := X;
      while (EX < Pred(FClipRect.Right)) and CheckPixel(Succ(EX), Y) do
        Inc(EX);

      SetVisited(SX, Y, EX);
      if mode = fmSet then
        dest.SetHorizLine(SX, Y, EX, Color)
      else
      if mode = fmDrawWithTransparency then
        dest.DrawHorizLine(SX, Y, EX, Color)
      else
        dest.DrawHorizLineDiff(SX, Y, EX, Color, S, Tolerance);

      Added := False;
      if Y > FClipRect.Top then
        for I := SX to EX do
          if CheckPixel(I, Pred(Y)) then
          begin
            if Added then //do not add twice the same segment
              Continue;
            Push(I, Pred(Y));
            Added := True;
          end
          else
            Added := False;

      Added := False;
      if Y < Pred(FClipRect.Bottom) then
        for I := SX to EX do
          if CheckPixel(I, Succ(Y)) then
          begin
            if Added then //do not add twice the same segment
              Continue;
            Push(I, Succ(Y));
            Added := True;
          end
          else
            Added := False;
    until StackCount <= 0;
  end;
end;

procedure TBGRADefaultBitmap.GradientFill(x, y, x2, y2: integer;
  c1, c2: TBGRAPixel; gtype: TGradientType; o1, o2: TPointF; mode: TDrawMode;
  gammaColorCorrection: boolean = True; Sinus: Boolean=False);
begin
  BGRAGradientFill(self, x, y, x2, y2, c1, c2, gtype, o1, o2, mode, gammaColorCorrection, Sinus);
end;

procedure TBGRADefaultBitmap.GradientFill(x, y, x2, y2: integer;
  gradient: TBGRACustomGradient; gtype: TGradientType; o1, o2: TPointF;
  mode: TDrawMode; Sinus: Boolean);
var
  scanner: TBGRAGradientScanner;
begin
  scanner := TBGRAGradientScanner.Create(gradient,gtype,o1,o2,sinus);
  FillRect(x,y,x2,y2,scanner,mode);
  scanner.Free;
end;

function TBGRADefaultBitmap.CreateBrushTexture(ABrushStyle: TBrushStyle; APatternColor, ABackgroundColor: TBGRAPixel;
                AWidth: integer = 8; AHeight: integer = 8; APenWidth: single = 1): TBGRACustomBitmap;
begin
  result := BGRAPen.CreateBrushTexture(self,ABrushStyle,APatternColor,ABackgroundColor,AWidth,AHeight,APenWidth);
end;

function TBGRADefaultBitmap.ScanAtInteger(X, Y: integer): TBGRAPixel;
begin
  if FData <> nil then
    result := (GetScanlineFast(PositiveMod(Y+ScanOffset.Y, FHeight))+PositiveMod(X+ScanOffset.X, FWidth))^
  else
    result := BGRAPixelTransparent;
end;

{ Scanning procedures for IBGRAScanner interface }
procedure TBGRADefaultBitmap.ScanMoveTo(X, Y: Integer);
begin
  if FData = nil then exit;
  LoadFromBitmapIfNeeded;
  FScanCurX := PositiveMod(X+ScanOffset.X, FWidth);
  FScanCurY := PositiveMod(Y+ScanOffset.Y, FHeight);
  FScanPtr := ScanLine[FScanCurY];
end;

function TBGRADefaultBitmap.ScanNextPixel: TBGRAPixel;
begin
  if FData <> nil then
  begin
    result := (FScanPtr+FScanCurX)^;
    inc(FScanCurX);
    if FScanCurX = FWidth then //cycle
      FScanCurX := 0;
  end
  else
    result := BGRAPixelTransparent;
end;

function TBGRADefaultBitmap.ScanAt(X, Y: Single): TBGRAPixel;
var
  ix, iy: integer;
  iFactX,iFactY: integer;
begin
  if FData = nil then
  begin
    result := BGRAPixelTransparent;
    exit;
  end;
  iFactX := round(x*256);
  iFactY := round(y*256);
  ix := (iFactX shr 8)+ScanOffset.X;
  iy := (iFactY shr 8)+ScanOffset.Y;
  iFactX := iFactX and 255;
  iFactY := iFactY and 255;

  if (iFactX = 0) and (iFactY = 0) then
  begin
    result := (GetScanlineFast(PositiveMod(iy, FHeight))+PositiveMod(ix, FWidth))^;
    exit;
  end;

  if ScanInterpolationFilter <> rfLinear then
  begin
    iFactX := FineInterpolation256( iFactX, ScanInterpolationFilter );
    iFactY := FineInterpolation256( iFactY, ScanInterpolationFilter );
  end;

  result := GetPixelCycleInline(ix,iy, iFactX,iFactY);
end;

function TBGRADefaultBitmap.IsScanPutPixelsDefined: boolean;
begin
  Result:= true;
end;

procedure TBGRADefaultBitmap.ScanPutPixels(pdest: PBGRAPixel; count: integer;
  mode: TDrawMode);
var
  i,nbCopy: Integer;
  c: TBGRAPixel;
begin
  case mode of
    dmLinearBlend:
      for i := 0 to count-1 do
      begin
        FastBlendPixelInline(pdest, ScanNextPixel);
        inc(pdest);
      end;
    dmDrawWithTransparency:
      for i := 0 to count-1 do
      begin
        DrawPixelInlineWithAlphaCheck(pdest, ScanNextPixel);
        inc(pdest);
      end;
    dmSet:
      while count > 0 do
      begin
        nbCopy := FWidth-FScanCurX;
        if count < nbCopy then nbCopy := count;
        move((FScanPtr+FScanCurX)^,pdest^,nbCopy*sizeof(TBGRAPixel));
        inc(pdest,nbCopy);
        inc(FScanCurX,nbCopy);
        if FScanCurX = FWidth then FScanCurX := 0;
        dec(count,nbCopy);
      end;
    dmSetExceptTransparent:
      for i := 0 to count-1 do
      begin
        c := ScanNextPixel;
        if c.alpha = 255 then pdest^ := c;
        inc(pdest);
      end;
    dmXor:
      for i := 0 to count-1 do
      begin
        PDWord(pdest)^ := PDWord(pdest)^ xor DWord(ScanNextPixel);
        inc(pdest);
      end;
  end;
end;

{ General purpose pixel drawing function }
procedure TBGRADefaultBitmap.DrawPixels(c: TBGRAPixel; start, Count: integer);
var
  p: PBGRAPixel;
begin
  if c.alpha = 0 then
    exit;
  if c.alpha = 255 then
  begin
    Fill(c,start,Count);
    exit;
  end;

  if start < 0 then
  begin
    Count += start;
    start := 0;
  end;
  if start >= nbPixels then
    exit;
  if start + Count > nbPixels then
    Count := nbPixels - start;

  p := Data + start;
  DrawPixelsInline(p,c,Count);
  InvalidateBitmap;
end;

{------------------------- End fill ------------------------------}

procedure TBGRADefaultBitmap.DoAlphaCorrection;
var
  p: PBGRAPixel;
  n: integer;
begin
  if CanvasAlphaCorrection then
  begin
    p := FData;
    for n := NbPixels - 1 downto 0 do
    begin
      if (longword(p^) and $FFFFFF <> 0) and (p^.alpha = 0) then
        p^.alpha := FCanvasOpacity;
      Inc(p);
    end;
  end;
  FAlphaCorrectionNeeded := False;
  InvalidateBitmap;
end;

{ Ensure that transparent pixels have all channels to zero }
procedure TBGRADefaultBitmap.ClearTransparentPixels;
var
  p: PBGRAPixel;
  n: integer;
begin
  p := FData;
  for n := NbPixels - 1 downto 0 do
  begin
    if (p^.alpha = 0) then
      p^ := BGRAPixelTransparent;
    Inc(p);
  end;
  InvalidateBitmap;
end;

function TBGRADefaultBitmap.CheckPutImageBounds(x,y,tx,ty: integer; out minxb,minyb,maxxb,maxyb,ignoreleft: integer): boolean inline;
var x2,y2: integer;
begin
  if (x >= FClipRect.Right) or (y >= FClipRect.Bottom) or (x <= FClipRect.Left-tx) or
    (y <= FClipRect.Top-ty) or (Height = 0) or (ty = 0) or (tx = 0) then
  begin
    result := false;
    exit;
  end;

  x2 := x + tx - 1;
  y2 := y + ty - 1;

  if y < FClipRect.Top then
    minyb := FClipRect.Top
  else
    minyb := y;
  if y2 >= FClipRect.Bottom then
    maxyb := FClipRect.Bottom - 1
  else
    maxyb := y2;

  if x < FClipRect.Left then
  begin
    ignoreleft := FClipRect.Left-x;
    minxb      := FClipRect.Left;
  end
  else
  begin
    ignoreleft := 0;
    minxb      := x;
  end;
  if x2 >= FClipRect.Right then
    maxxb := FClipRect.Right - 1
  else
    maxxb := x2;

  result := true;
end;

function TBGRADefaultBitmap.CheckAntialiasRectBounds(var x, y, x2, y2: single;
  w: single): boolean;
var
  temp: Single;
begin
  if (x > x2) then
  begin
    temp := x;
    x    := x2;
    x2   := temp;
  end;
  if (y > y2) then
  begin
    temp := y;
    y    := y2;
    y2   := temp;
  end;

  result := (x2 - x > w) and (y2 - y > w);
end;

function TBGRADefaultBitmap.GetCanvasBGRA: TBGRACanvas;
begin
  if FCanvasBGRA = nil then
    FCanvasBGRA := TBGRACanvas.Create(self);
  result := FCanvasBGRA;
end;

function TBGRADefaultBitmap.GetCanvas2D: TBGRACanvas2D;
begin
  if FCanvas2D = nil then
    FCanvas2D := TBGRACanvas2D.Create(self);
  result := FCanvas2D;
end;

procedure TBGRADefaultBitmap.PutImage(x, y: integer; Source: TBGRACustomBitmap;
  mode: TDrawMode; AOpacity: byte);
var
  yb, minxb, minyb, maxxb, maxyb, ignoreleft, copycount, sourcewidth,
  i, delta_source, delta_dest: integer;
  psource, pdest: PBGRAPixel;
  tempPixel: TBGRAPixel;

begin
  if (source = nil) or (AOpacity = 0) then exit;
  sourcewidth := Source.Width;

  if not CheckPutImageBounds(x,y,sourcewidth,source.height,minxb,minyb,maxxb,maxyb,ignoreleft) then exit;

  copycount := maxxb - minxb + 1;

  psource := Source.ScanLine[minyb - y] + ignoreleft;
  if Source.LineOrder = riloBottomToTop then
    delta_source := -sourcewidth
  else
    delta_source := sourcewidth;

  pdest := Scanline[minyb] + minxb;
  if FLineOrder = riloBottomToTop then
    delta_dest := -Width
  else
    delta_dest := Width;

  case mode of
    dmSet:
    begin
      if AOpacity <> 255 then
      begin
        for yb := minyb to maxyb do
        begin
          CopyPixelsWithOpacity(pdest, psource, AOpacity, copycount);
          Inc(psource, delta_source);
          Inc(pdest, delta_dest);
        end;
      end
      else
      begin
        copycount *= sizeof(TBGRAPixel);
        for yb := minyb to maxyb do
        begin
          move(psource^, pdest^, copycount);
          Inc(psource, delta_source);
          Inc(pdest, delta_dest);
        end;
      end;
      InvalidateBitmap;
    end;
    dmSetExceptTransparent:
    begin
      Dec(delta_source, copycount);
      Dec(delta_dest, copycount);
      for yb := minyb to maxyb do
      begin
        if AOpacity <> 255 then
        begin
          for i := copycount - 1 downto 0 do
          begin
            if psource^.alpha = 255 then
            begin
              tempPixel := psource^;
              tempPixel.alpha := ApplyOpacity(tempPixel.alpha,AOpacity);
              FastBlendPixelInline(pdest,tempPixel);
            end;
            Inc(pdest);
            Inc(psource);
          end;
        end else
          for i := copycount - 1 downto 0 do
          begin
            if psource^.alpha = 255 then
              pdest^ := psource^;
            Inc(pdest);
            Inc(psource);
          end;
        Inc(psource, delta_source);
        Inc(pdest, delta_dest);
      end;
      InvalidateBitmap;
    end;
    dmDrawWithTransparency:
    begin
      Dec(delta_source, copycount);
      Dec(delta_dest, copycount);
      for yb := minyb to maxyb do
      begin
        if AOpacity <> 255 then
        begin
          for i := copycount - 1 downto 0 do
          begin
            DrawPixelInlineWithAlphaCheck(pdest, psource^, AOpacity);
            Inc(pdest);
            Inc(psource);
          end;
        end
        else
          for i := copycount - 1 downto 0 do
          begin
            DrawPixelInlineWithAlphaCheck(pdest, psource^);
            Inc(pdest);
            Inc(psource);
          end;
        Inc(psource, delta_source);
        Inc(pdest, delta_dest);
      end;
      InvalidateBitmap;
    end;
    dmFastBlend:
    begin
      Dec(delta_source, copycount);
      Dec(delta_dest, copycount);
      for yb := minyb to maxyb do
      begin
        if AOpacity <> 255 then
        begin
          for i := copycount - 1 downto 0 do
          begin
            FastBlendPixelInline(pdest, psource^, AOpacity);
            Inc(pdest);
            Inc(psource);
          end;
        end else
          for i := copycount - 1 downto 0 do
          begin
            FastBlendPixelInline(pdest, psource^);
            Inc(pdest);
            Inc(psource);
          end;
        Inc(psource, delta_source);
        Inc(pdest, delta_dest);
      end;
      InvalidateBitmap;
    end;
    dmXor:
    begin
      if AOpacity <> 255 then
      begin
        Dec(delta_source, copycount);
        Dec(delta_dest, copycount);
        for yb := minyb to maxyb do
        begin
          for i := copycount - 1 downto 0 do
          begin
            FastBlendPixelInline(pdest, TBGRAPixel(PDWord(pdest)^ xor PDword(psource)^), AOpacity);
            Inc(pdest);
            Inc(psource);
          end;
          Inc(psource, delta_source);
          Inc(pdest, delta_dest);
        end;
      end else
      begin
        for yb := minyb to maxyb do
        begin
          XorPixels(pdest, psource, copycount);
          Inc(psource, delta_source);
          Inc(pdest, delta_dest);
        end;
      end;
      InvalidateBitmap;
    end;
  end;
end;

procedure TBGRADefaultBitmap.BlendImage(x, y: integer; Source: TBGRACustomBitmap;
  operation: TBlendOperation);
var
  yb, minxb, minyb, maxxb, maxyb, ignoreleft, copycount, sourcewidth,
  delta_source, delta_dest: integer;
  psource, pdest: PBGRAPixel;
begin
  sourcewidth := Source.Width;

  if not CheckPutImageBounds(x,y,sourcewidth,source.height,minxb,minyb,maxxb,maxyb,ignoreleft) then exit;

  copycount := maxxb - minxb + 1;

  psource := Source.ScanLine[minyb - y] + ignoreleft;
  if Source.LineOrder = riloBottomToTop then
    delta_source := -sourcewidth
  else
    delta_source := sourcewidth;

  pdest := Scanline[minyb] + minxb;
  if FLineOrder = riloBottomToTop then
    delta_dest := -Width
  else
    delta_dest := Width;

  for yb := minyb to maxyb do
  begin
    BlendPixels(pdest, psource, operation, copycount);
    Inc(psource, delta_source);
    Inc(pdest, delta_dest);
  end;
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.BlendImageOver(x, y: integer;
  Source: TBGRACustomBitmap; operation: TBlendOperation; AOpacity: byte; ALinearBlend: boolean);
var
  yb, minxb, minyb, maxxb, maxyb, ignoreleft, copycount, sourcewidth,
  delta_source, delta_dest: integer;
  psource, pdest: PBGRAPixel;
begin
  sourcewidth := Source.Width;

  if not CheckPutImageBounds(x,y,sourcewidth,source.height,minxb,minyb,maxxb,maxyb,ignoreleft) then exit;

  copycount := maxxb - minxb + 1;

  psource := Source.ScanLine[minyb - y] + ignoreleft;
  if Source.LineOrder = riloBottomToTop then
    delta_source := -sourcewidth
  else
    delta_source := sourcewidth;

  pdest := Scanline[minyb] + minxb;
  if FLineOrder = riloBottomToTop then
    delta_dest := -Width
  else
    delta_dest := Width;

  for yb := minyb to maxyb do
  begin
    BlendPixelsOver(pdest, psource, operation, copycount, AOpacity, ALinearBlend);
    Inc(psource, delta_source);
    Inc(pdest, delta_dest);
  end;
  InvalidateBitmap;
end;

{ Draw an image wih an angle. Use an affine transformation to do this. }
procedure TBGRADefaultBitmap.PutImageAngle(x, y: single;
  Source: TBGRACustomBitmap; angle: single; imageCenterX: single;
  imageCenterY: single; AOpacity: Byte; ARestoreOffsetAfterRotation: boolean);
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
  PutImageAffine(Coord(0,0),Coord(source.Width,0),Coord(0,source.Height),source,AOpacity);
end;

{ Draw an image with an affine transformation (rotation, scale, translate).
  Parameters are the bitmap origin, the end of the horizontal axis and the end of the vertical axis. }
procedure TBGRADefaultBitmap.PutImageAffine(Origin,HAxis,VAxis: TPointF; Source: TBGRACustomBitmap; AOpacity: Byte);
var affine: TBGRAAffineBitmapTransform;
    minx,miny,maxx,maxy: integer;
    pt4: TPointF;

  //include specified point in the bounds
  procedure Include(pt: TPointF);
  begin
    if floor(pt.X) < minx then minx := floor(pt.X);
    if floor(pt.Y) < miny then miny := floor(pt.Y);
    if ceil(pt.X) > maxx then maxx := ceil(pt.X);
    if ceil(pt.Y) > maxy then maxy := ceil(pt.Y);
  end;

begin
  if (abs(Origin.x-round(Origin.x))<1e-6) and (abs(Origin.y-round(Origin.Y))<1e-6) and
     (abs(HAxis.x-(Origin.x+Source.Width))<1e-6) and (abs(HAxis.y-origin.y)<1e-6) and
     (abs(VAxis.x-Origin.x)<1e-6) and (abs(VAxis.y-(Origin.y+Source.Height))<1e-6) then
  begin
    PutImage(round(origin.x),round(origin.y),Source,dmDrawWithTransparency,AOpacity);
    exit;
  end;

  { Create affine transformation }
  affine := TBGRAAffineBitmapTransform.Create(Source);
  affine.GlobalOpacity := AOpacity;
  affine.Fit(Origin,HAxis,VAxis);

  { Compute bounds }
  pt4.x := VAxis.x+HAxis.x-Origin.x;
  pt4.y := VAxis.y+HAxis.y-Origin.y;
  minx := floor(Origin.X);
  miny := floor(Origin.Y);
  maxx := ceil(Origin.X);
  maxy := ceil(Origin.Y);
  Include(HAxis);
  Include(VAxis);
  Include(pt4);

  { Use the affine transformation as a scanner }
  FillRect(minx,miny,maxx+1,maxy+1,affine,dmDrawWithTransparency);
  affine.Free;
end;

{ Duplicate bitmap content. Optionally, bitmap properties can be also duplicated }
function TBGRADefaultBitmap.Duplicate(DuplicateProperties: Boolean = False): TBGRACustomBitmap;
var Temp: TBGRADefaultBitmap;
begin
  LoadFromBitmapIfNeeded;
  Temp := NewBitmap(Width, Height) as TBGRADefaultBitmap;
  Temp.PutImage(0, 0, self, dmSet);
  Temp.Caption := self.Caption;
  if DuplicateProperties then
    CopyPropertiesTo(Temp);
  Result := Temp;
end;

{ Copy properties only }
procedure TBGRADefaultBitmap.CopyPropertiesTo(ABitmap: TBGRADefaultBitmap);
begin
  ABitmap.CanvasOpacity := CanvasOpacity;
  ABitmap.CanvasDrawModeFP := CanvasDrawModeFP;
  ABitmap.PenStyle := PenStyle;
  ABitmap.CustomPenStyle := CustomPenStyle;
  ABitmap.FontHeight := FontHeight;
  ABitmap.FontName := FontName;
  ABitmap.FontStyle := FontStyle;
  ABitmap.FontAntialias := FontAntialias;
  ABitmap.FontOrientation := FontOrientation;
  ABitmap.LineCap := LineCap;
  ABitmap.JoinStyle := JoinStyle;
  ABitmap.FillMode := FillMode;
  ABitmap.ClipRect := ClipRect;
end;

{ Check if two bitmaps have the same content }
function TBGRADefaultBitmap.Equals(comp: TBGRACustomBitmap): boolean;
var
  yb, xb: integer;
  pself, pcomp: PBGRAPixel;
begin
  if comp = nil then
    Result := False
  else
  if (comp.Width <> Width) or (comp.Height <> Height) then
    Result := False
  else
  begin
    Result := True;
    for yb := 0 to Height - 1 do
    begin
      pself := ScanLine[yb];
      pcomp := comp.Scanline[yb];
      for xb := 0 to Width - 1 do
      begin
        if pself^ <> pcomp^ then
        begin
          Result := False;
          exit;
        end;
        Inc(pself);
        Inc(pcomp);
      end;
    end;
  end;
end;

{ Check if a bitmap is filled wih the specified color }
function TBGRADefaultBitmap.Equals(comp: TBGRAPixel): boolean;
var
  i: integer;
  p: PBGRAPixel;
begin
  p := Data;
  for i := NbPixels - 1 downto 0 do
  begin
    if p^ <> comp then
    begin
      Result := False;
      exit;
    end;
    Inc(p);
  end;
  Result := True;
end;

{----------------------------- Filters -----------------------------------------}
{ Call the appropriate function }

function TBGRADefaultBitmap.FilterSmartZoom3(Option: TMedianOption): TBGRACustomBitmap;
begin
  Result := BGRAFilters.FilterSmartZoom3(self, Option);
end;

function TBGRADefaultBitmap.FilterMedian(Option: TMedianOption): TBGRACustomBitmap;
begin
  Result := BGRAFilters.FilterMedian(self, option);
end;

function TBGRADefaultBitmap.FilterSmooth: TBGRACustomBitmap;
begin
  Result := BGRAFilters.FilterBlurRadialPrecise(self, 0.3);
end;

function TBGRADefaultBitmap.FilterSphere: TBGRACustomBitmap;
begin
  Result := BGRAFilters.FilterSphere(self);
end;

function TBGRADefaultBitmap.FilterTwirl(ACenter: TPoint; ARadius: Single; ATurn: Single=1; AExponent: Single=3): TBGRACustomBitmap;
begin
  Result := BGRAFilters.FilterTwirl(self, ACenter, ARadius, ATurn, AExponent);
end;

function TBGRADefaultBitmap.FilterCylinder: TBGRACustomBitmap;
begin
  Result := BGRAFilters.FilterCylinder(self);
end;

function TBGRADefaultBitmap.FilterPlane: TBGRACustomBitmap;
begin
  Result := BGRAFilters.FilterPlane(self);
end;

function TBGRADefaultBitmap.FilterSharpen: TBGRACustomBitmap;
begin
  Result := BGRAFilters.FilterSharpen(self);
end;

function TBGRADefaultBitmap.FilterContour: TBGRACustomBitmap;
begin
  Result := BGRAFilters.FilterContour(self);
end;

function TBGRADefaultBitmap.FilterBlurRadial(radius: integer;
  blurType: TRadialBlurType): TBGRACustomBitmap;
begin
  Result := BGRAFilters.FilterBlurRadial(self, radius, blurType);
end;

function TBGRADefaultBitmap.FilterPixelate(pixelSize: integer;
  useResample: boolean; filter: TResampleFilter): TBGRACustomBitmap;
begin
  Result:= BGRAFilters.FilterPixelate(self, pixelSize, useResample, filter);
end;

function TBGRADefaultBitmap.FilterBlurMotion(distance: integer;
  angle: single; oriented: boolean): TBGRACustomBitmap;
begin
  Result := BGRAFilters.FilterBlurMotion(self, distance, angle, oriented);
end;

function TBGRADefaultBitmap.FilterCustomBlur(mask: TBGRACustomBitmap):
TBGRACustomBitmap;
begin
  Result := BGRAFilters.FilterBlur(self, mask);
end;

function TBGRADefaultBitmap.FilterEmboss(angle: single): TBGRACustomBitmap;
begin
  Result := BGRAFilters.FilterEmboss(self, angle);
end;

function TBGRADefaultBitmap.FilterEmbossHighlight(FillSelection: boolean):
TBGRACustomBitmap;
begin
  Result := BGRAFilters.FilterEmbossHighlight(self, FillSelection, BGRAPixelTransparent);
end;

function TBGRADefaultBitmap.FilterEmbossHighlight(FillSelection: boolean;
  BorderColor: TBGRAPixel): TBGRACustomBitmap;
begin
  Result := BGRAFilters.FilterEmbossHighlight(self, FillSelection, BorderColor);
end;

function TBGRADefaultBitmap.FilterEmbossHighlight(FillSelection: boolean;
  BorderColor: TBGRAPixel; var Offset: TPoint): TBGRACustomBitmap;
begin
  Result := BGRAFilters.FilterEmbossHighlightOffset(self, FillSelection, BorderColor, Offset);
end;

function TBGRADefaultBitmap.FilterGrayscale: TBGRACustomBitmap;
begin
  Result := BGRAFilters.FilterGrayscale(self);
end;

function TBGRADefaultBitmap.FilterNormalize(eachChannel: boolean = True):
TBGRACustomBitmap;
begin
  Result := BGRAFilters.FilterNormalize(self, eachChannel);
end;

function TBGRADefaultBitmap.FilterRotate(origin: TPointF;
  angle: single): TBGRACustomBitmap;
begin
  Result := BGRAFilters.FilterRotate(self, origin, angle);
end;

function TBGRADefaultBitmap.GetHasTransparentPixels: boolean;
var
  p: PBGRAPixel;
  n: integer;
begin
  p := Data;
  for n := NbPixels - 1 downto 0 do
  begin
    if p^.alpha <> 255 then
    begin
      Result := True;
      exit;
    end;
    Inc(p);
  end;
  Result := False;
end;

function TBGRADefaultBitmap.GetAverageColor: TColor;
var
  pix: TBGRAPixel;
begin
  pix := GetAveragePixel;
  {$hints off}
  if pix.alpha = 0 then
    result := clNone else
     result := pix.red + pix.green shl 8 + pix.blue shl 16;
  {$hints on}
end;

function TBGRADefaultBitmap.GetAveragePixel: TBGRAPixel;
var
  n:     integer;
  p:     PBGRAPixel;
  r, g, b, sum: double;
  alpha: double;
begin
  sum := 0;
  r   := 0;
  g   := 0;
  b   := 0;
  p   := Data;
  for n := NbPixels - 1 downto 0 do
  begin
    alpha := p^.alpha / 255;
    sum   += alpha;
    r     += p^.red * alpha;
    g     += p^.green * alpha;
    b     += p^.blue * alpha;
    Inc(p);
  end;
  if sum = 0 then
    Result := BGRAPixelTransparent
  else
    Result := BGRA(round(r / sum),round(g / sum),round(b / sum),round(sum*255/NbPixels));
end;

function TBGRADefaultBitmap.CreateAdaptedPngWriter: TFPWriterPNG;
begin
  result := TFPWriterPNG.Create;
  result.Indexed := False;
  result.UseAlpha := HasTransparentPixels;
  result.WordSized := false;
end;

{$hints off}
function TBGRADefaultBitmap.LoadAsBmp32(Str: TStream): boolean;
var OldPos: int64;
    fileHeader: TBitmapFileHeader;
    infoHeader: TBitmapInfoHeader;
    dataSize: integer;
begin
  OldPos := Str.Position;
  result := false;
  try
    if Str.Read(fileHeader,sizeof(fileHeader)) <> sizeof(fileHeader) then
      raise exception.Create('Inuable to read file header');
    if fileHeader.bfType = $4D42 then
    begin
      if Str.Read(infoHeader,sizeof(infoHeader)) <> sizeof(infoHeader) then
        raise exception.Create('Inuable to read info header');

      if (infoHeader.biPlanes = 1) and (infoHeader.biBitCount = 32) and (infoHeader.biCompression = 0) then
      begin
        SetSize(infoHeader.biWidth,infoHeader.biHeight);
        Str.Position := OldPos+fileHeader.bfOffBits;
        dataSize := NbPixels*sizeof(TBGRAPixel);
        if Str.Read(Data^, dataSize) <> dataSize then
        Begin
          SetSize(0,0);
          raise exception.Create('Unable to read data');
        end;
        result := true;
      end;
    end;

  except
    on ex:exception do
    begin

    end;
  end;
  Str.Position := OldPos;

end;
{$hints on}

procedure TBGRADefaultBitmap.SetCanvasOpacity(AValue: byte);
begin
  LoadFromBitmapIfNeeded;
  FCanvasOpacity := AValue;
end;

function TBGRADefaultBitmap.GetDataPtr: PBGRAPixel;
begin
  LoadFromBitmapIfNeeded;
  Result := FData;
end;

{----------------------------- Resample ---------------------------------------}

function TBGRADefaultBitmap.FineResample(NewWidth, NewHeight: integer):
TBGRACustomBitmap;
begin
  Result := BGRAResample.FineResample(self, NewWidth, NewHeight, ResampleFilter);
end;

function TBGRADefaultBitmap.SimpleStretch(NewWidth, NewHeight: integer):
TBGRACustomBitmap;
begin
  Result := BGRAResample.SimpleStretch(self, NewWidth, NewHeight);
end;

function TBGRADefaultBitmap.Resample(newWidth, newHeight: integer;
  mode: TResampleMode): TBGRACustomBitmap;
begin
  case mode of
    rmFineResample: Result  := FineResample(newWidth, newHeight);
    rmSimpleStretch: Result := SimpleStretch(newWidth, newHeight);
    else
      Result := nil;
  end;
end;

{-------------------------------- Data functions ------------------------}

{ Flip vertically the bitmap. Use a temporary line to store top line,
  assign bottom line to top line, then assign temporary line to bottom line.

  It is an involution, i.e it does nothing when applied twice }
procedure TBGRADefaultBitmap.VerticalFlip;
var
  yb:     integer;
  line:   PBGRAPixel;
  linesize: integer;
  PStart: PBGRAPixel;
  PEnd:   PBGRAPixel;
begin
  if FData = nil then
    exit;

  LoadFromBitmapIfNeeded;
  linesize := Width * sizeof(TBGRAPixel);
  line     := nil;
  getmem(line, linesize);
  PStart := Data;
  PEnd   := Data + (Height - 1) * Width;
  for yb := 0 to (Height div 2) - 1 do
  begin
    move(PStart^, line^, linesize);
    move(PEnd^, PStart^, linesize);
    move(line^, PEnd^, linesize);
    Inc(PStart, Width);
    Dec(PEnd, Width);
  end;
  freemem(line);
  InvalidateBitmap;
end;

{ Flip horizontally. Swap left pixels with right pixels on each line.

  It is an involution, i.e it does nothing when applied twice}
procedure TBGRADefaultBitmap.HorizontalFlip;
var
  yb, xb: integer;
  PStart: PBGRAPixel;
  PEnd:   PBGRAPixel;
  temp:   TBGRAPixel;
begin
  if FData = nil then
    exit;

  LoadFromBitmapIfNeeded;
  for yb := 0 to Height - 1 do
  begin
    PStart := Scanline[yb];
    PEnd   := PStart + Width;
    for xb := 0 to (Width div 2) - 1 do
    begin
      Dec(PEnd);
      temp    := PStart^;
      PStart^ := PEnd^;
      PEnd^   := temp;
      Inc(PStart);
    end;
  end;
  InvalidateBitmap;
end;

{ Return a new bitmap rotated in a clock wise direction. }
function TBGRADefaultBitmap.RotateCW: TBGRACustomBitmap;
var
  psrc, pdest: PBGRAPixel;
  yb, xb: integer;
  delta: integer;
begin
  LoadFromBitmapIfNeeded;
  Result := NewBitmap(Height, Width);
  if Result.LineOrder = riloTopToBottom then
    delta := Result.Width
  else
    delta := -Result.Width;
  for yb := 0 to Height - 1 do
  begin
    psrc  := Scanline[yb];
    pdest := Result.Scanline[0] + (Height - 1 - yb);
    for xb := 0 to Width - 1 do
    begin
      pdest^ := psrc^;
      Inc(psrc);
      Inc(pdest, delta);
    end;
  end;
end;

{ Return a new bitmap rotated in a counter clock wise direction. }
function TBGRADefaultBitmap.RotateCCW: TBGRACustomBitmap;
var
  psrc, pdest: PBGRAPixel;
  yb, xb: integer;
  delta: integer;
begin
  LoadFromBitmapIfNeeded;
  Result := NewBitmap(Height, Width);
  if Result.LineOrder = riloTopToBottom then
    delta := Result.Width
  else
    delta := -Result.Width;
  for yb := 0 to Height - 1 do
  begin
    psrc  := Scanline[yb];
    pdest := Result.Scanline[Width - 1] + yb;
    for xb := 0 to Width - 1 do
    begin
      pdest^ := psrc^;
      Inc(psrc);
      Dec(pdest, delta);
    end;
  end;
end;

{ Compute negative with gamma correction. A negative contains
  complentary colors (black becomes white etc.).

  It is an involution, i.e it does nothing when applied twice }
procedure TBGRADefaultBitmap.Negative;
var
  p: PBGRAPixel;
  n: integer;
begin
  LoadFromBitmapIfNeeded;
  p := Data;
  for n := NbPixels - 1 downto 0 do
  begin
    if p^.alpha <> 0 then
    begin
      p^.red   := GammaCompressionTab[not GammaExpansionTab[p^.red]];
      p^.green := GammaCompressionTab[not GammaExpansionTab[p^.green]];
      p^.blue  := GammaCompressionTab[not GammaExpansionTab[p^.blue]];
    end;
    Inc(p);
  end;
  InvalidateBitmap;
end;

{ Compute negative without gamma correction.

  It is an involution, i.e it does nothing when applied twice }
procedure TBGRADefaultBitmap.LinearNegative;
var
  p: PBGRAPixel;
  n: integer;
begin
  LoadFromBitmapIfNeeded;
  p := Data;
  for n := NbPixels - 1 downto 0 do
  begin
    if p^.alpha <> 0 then
    begin
      p^.red   := not p^.red;
      p^.green := not p^.green;
      p^.blue  := not p^.blue;
    end;
    Inc(p);
  end;
  InvalidateBitmap;
end;

{ Swap red and blue channels. Useful when RGB order is swapped.

  It is an involution, i.e it does nothing when applied twice }
procedure TBGRADefaultBitmap.SwapRedBlue;
var
  n:    integer;
  temp: longword;
  p:    PLongword;
begin
  LoadFromBitmapIfNeeded;
  p := PLongword(Data);
  n := NbPixels;
  if n = 0 then
    exit;
  repeat
    temp := LEtoN(p^);
    p^   := NtoLE(((temp and $FF) shl 16) or ((temp and $FF0000) shr 16) or
      temp and $FF00FF00);
    Inc(p);
    Dec(n);
  until n = 0;
  InvalidateBitmap;
end;

{ Convert a grayscale image into a black image with alpha value }
procedure TBGRADefaultBitmap.GrayscaleToAlpha;
var
  n:    integer;
  temp: longword;
  p:    PLongword;
begin
  LoadFromBitmapIfNeeded;
  p := PLongword(Data);
  n := NbPixels;
  if n = 0 then
    exit;
  repeat
    temp := LEtoN(p^);
    p^   := NtoLE((temp and $FF) shl 24);
    Inc(p);
    Dec(n);
  until n = 0;
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.AlphaToGrayscale;
var
  n:    integer;
  temp: longword;
  p:    PLongword;
begin
  LoadFromBitmapIfNeeded;
  p := PLongword(Data);
  n := NbPixels;
  if n = 0 then
    exit;
  repeat
    temp := LEtoN(p^ shr 24);
    p^   := NtoLE(temp or (temp shl 8) or (temp shl 16) or $FF000000);
    Inc(p);
    Dec(n);
  until n = 0;
  InvalidateBitmap;
end;

{ Apply a mask to the bitmap. It means that alpha channel is
  changed according to grayscale values of the mask.

  See : http://wiki.lazarus.freepascal.org/BGRABitmap_tutorial_5 }
procedure TBGRADefaultBitmap.ApplyMask(mask: TBGRACustomBitmap);
var
  p, pmask: PBGRAPixel;
  yb, xb:   integer;
begin
  if (Mask.Width <> Width) or (Mask.Height <> Height) then
    exit;

  LoadFromBitmapIfNeeded;
  for yb := 0 to Height - 1 do
  begin
    p     := Scanline[yb];
    pmask := Mask.Scanline[yb];
    for xb := Width - 1 downto 0 do
    begin
      p^.alpha := ApplyOpacity(p^.alpha, pmask^.red);
      Inc(p);
      Inc(pmask);
    end;
  end;
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.ApplyGlobalOpacity(alpha: byte);
var
  p: PBGRAPixel;
  i: integer;
begin
  if alpha = 0 then
    FillTransparent
  else
  if alpha <> 255 then
  begin
    p := Data;
    for i := NbPixels - 1 downto 0 do
    begin
      p^.alpha := ApplyOpacity(p^.alpha, alpha);
      Inc(p);
    end;
  end;
end;

procedure TBGRADefaultBitmap.ConvertToLinearRGB;
var p: PBGRAPixel;
    n: integer;
begin
  p := Data;
  for n := NbPixels-1 downto 0 do
  begin
    p^.red := GammaExpansionTab[p^.red] shr 8;
    p^.green := GammaExpansionTab[p^.green] shr 8;
    p^.blue := GammaExpansionTab[p^.blue] shr 8;
    inc(p);
  end;
end;

procedure TBGRADefaultBitmap.ConvertFromLinearRGB;
var p: PBGRAPixel;
    n: integer;
begin
  p := Data;
  for n := NbPixels-1 downto 0 do
  begin
    p^.red := GammaCompressionTab[p^.red shl 8 + p^.red];
    p^.green := GammaCompressionTab[p^.green shl 8 + p^.green];
    p^.blue := GammaCompressionTab[p^.blue shl 8 + p^.blue];
    inc(p);
  end;
end;

{ Get bounds of non zero values of specified channel }
function TBGRADefaultBitmap.GetImageBounds(Channel: TChannel = cAlpha; ANothingValue: Byte = 0): TRect;
var
  minx, miny, maxx, maxy: integer;
  xb, yb: integer;
  p:      pbyte;
  offset: integer;
begin
  maxx := -1;
  maxy := -1;
  minx := self.Width;
  miny := self.Height;
  case Channel of
    cBlue: offset  := 0;
    cGreen: offset := 1;
    cRed: offset   := 2;
    else
      offset := 3;
  end;
  for yb := 0 to self.Height - 1 do
  begin
    p := PByte(self.ScanLine[yb]) + offset;
    for xb := 0 to self.Width - 1 do
    begin
      if p^ <> ANothingValue then
      begin
        if xb < minx then
          minx := xb;
        if yb < miny then
          miny := yb;
        if xb > maxx then
          maxx := xb;
        if yb > maxy then
          maxy := yb;
      end;
      Inc(p, sizeof(TBGRAPixel));
    end;
  end;
  if minx > maxx then
  begin
    Result.left   := 0;
    Result.top    := 0;
    Result.right  := 0;
    Result.bottom := 0;
  end
  else
  begin
    Result.left   := minx;
    Result.top    := miny;
    Result.right  := maxx + 1;
    Result.bottom := maxy + 1;
  end;
end;

function TBGRADefaultBitmap.GetImageBounds(Channels: TChannels): TRect;
var c: TChannel;
begin
  result := rect(0,0,0,0);
  for c := low(TChannel) to high(TChannel) do
    if c in Channels then
      UnionRect(result,result,GetImageBounds(c));
end;

function TBGRADefaultBitmap.GetDifferenceBounds(ABitmap: TBGRACustomBitmap): TRect;
var
  minx, miny, maxx, maxy: integer;
  xb, yb: integer;
  p, p2:  PBGRAPixel;
begin
  if (ABitmap.Width <> Width) or (ABitmap.Height <> Height) then
  begin
    result := rect(0,0,Width,Height);
    if ABitmap.Width > result.Right then result.Right := ABitmap.Width;
    if ABitmap.Height > result.bottom then result.bottom := ABitmap.Height;
    exit;
  end;
  maxx := -1;
  maxy := -1;
  minx := self.Width;
  miny := self.Height;
  for yb := 0 to self.Height - 1 do
  begin
    p := self.ScanLine[yb];
    p2 := ABitmap.ScanLine[yb];
    for xb := 0 to self.Width - 1 do
    begin
      if p^ <> p2^ then
      begin
        if xb < minx then
          minx := xb;
        if yb < miny then
          miny := yb;
        if xb > maxx then
          maxx := xb;
        if yb > maxy then
          maxy := yb;
      end;
      Inc(p);
      Inc(p2);
    end;
  end;
  if minx > maxx then
  begin
    Result.left   := 0;
    Result.top    := 0;
    Result.right  := 0;
    Result.bottom := 0;
  end
  else
  begin
    Result.left   := minx;
    Result.top    := miny;
    Result.right  := maxx + 1;
    Result.bottom := maxy + 1;
  end;
end;

{ Make a copy of the transparent bitmap to a TBitmap with a background color
  instead of transparency }
function TBGRADefaultBitmap.MakeBitmapCopy(BackgroundColor: TColor): TBitmap;
var
  opaqueCopy: TBGRACustomBitmap;
begin
  Result     := TBitmap.Create;
  Result.Width := Width;
  Result.Height := Height;
  opaqueCopy := NewBitmap(Width, Height);
  opaqueCopy.Fill(ColorToRGB(BackgroundColor));
  opaqueCopy.PutImage(0, 0, self, dmDrawWithTransparency);
  opaqueCopy.Draw(Result.canvas, 0, 0, True);
  opaqueCopy.Free;
end;

{ Get a part of the image with repetition in both directions. It means
  that if the bounds are within the image, the result is just that part
  of the image, but if the bounds are bigger than the image, the image
  is tiled. }
function TBGRADefaultBitmap.GetPart(ARect: TRect): TBGRACustomBitmap;
var
  copywidth, copyheight, widthleft, heightleft, curxin, curyin, xdest,
  ydest, tx, ty: integer;
begin
  tx := ARect.Right - ARect.Left;
  ty := ARect.Bottom - ARect.Top;

  if (tx <= 0) or (ty <= 0) then
  begin
    result := nil;
    exit;
  end;

  LoadFromBitmapIfNeeded;
  if ARect.Left >= Width then
    ARect.Left := ARect.Left mod Width
  else
  if ARect.Left < 0 then
    ARect.Left := Width - ((-ARect.Left) mod Width);
  ARect.Right  := ARect.Left + tx;

  if ARect.Top >= Height then
    ARect.Top := ARect.Top mod Height
  else
  if ARect.Top < 0 then
    ARect.Top  := Height - ((-ARect.Top) mod Height);
  ARect.Bottom := ARect.Top + ty;

  if (ARect.Left = 0) and (ARect.Top = 0) and
     (ARect.Right = Width) and
    (ARect.Bottom = Height) then
  begin
    result := Duplicate;
    exit;
  end;

  result     := NewBitmap(tx, ty);
  heightleft := result.Height;
  curyin     := ARect.Top;
  ydest      := -ARect.Top;
  while heightleft > 0 do
  begin
    if curyin + heightleft > Height then
      copyheight := Height - curyin
    else
      copyheight := heightleft;

    widthleft := result.Width;
    curxin    := ARect.Left;
    xdest     := -ARect.Left;
    while widthleft > 0 do
    begin
      if curxin + widthleft > Width then
        copywidth := Width - curxin
      else
        copywidth := widthleft;

      result.PutImage(xdest, ydest, self, dmSet);

      curxin := 0;
      Dec(widthleft, copywidth);
      Inc(xdest, Width);
    end;
    curyin := 0;
    Dec(heightleft, copyheight);
    Inc(ydest, Height);
  end;
end;

function TBGRADefaultBitmap.GetPtrBitmap(Top, Bottom: Integer
  ): TBGRACustomBitmap;
var temp: integer;
    ptrbmp: TBGRAPtrBitmap;
begin
  if Top > Bottom then
  begin
    temp := Top;
    Top := Bottom;
    Bottom := Temp;
  end;
  if Top < 0 then Top := 0;
  if Bottom > Height then Bottom := Height;
  if Top >= Bottom then
    result := nil
  else
  begin
    if LineOrder = riloTopToBottom then
      ptrbmp := TBGRAPtrBitmap.Create(Width,Bottom-Top,ScanLine[Top]) else
      ptrbmp := TBGRAPtrBitmap.Create(Width,Bottom-Top,ScanLine[Bottom-1]);
    ptrbmp.LineOrder := LineOrder;
    result := ptrbmp;
  end;
end;

{ Draw BGRA data to a canvas with transparency }
procedure TBGRADefaultBitmap.DataDrawTransparent(ACanvas: TCanvas;
  Rect: TRect; AData: Pointer; ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer);
var
  Temp:     TBitmap;
  RawImage: TRawImage;
  BitmapHandle, MaskHandle: HBitmap;
begin
  RawImage.Init;
  RawImage.Description.Init_BPP32_B8G8R8A8_BIO_TTB(AWidth, AHeight);
  RawImage.Description.LineOrder := ALineOrder;
  RawImage.Data     := PByte(AData);
  RawImage.DataSize := AWidth * AHeight * sizeof(TBGRAPixel);
  if not RawImage_CreateBitmaps(RawImage, BitmapHandle, MaskHandle, False) then
    raise FPImageException.Create('Failed to create bitmap handle');
  Temp := TBitmap.Create;
  Temp.Handle := BitmapHandle;
  Temp.MaskHandle := MaskHandle;
  ACanvas.StretchDraw(Rect, Temp);
  Temp.Free;
end;

{ Draw BGRA data to a canvas without transparency }
procedure TBGRADefaultBitmap.DataDrawOpaque(ACanvas: TCanvas;
  Rect: TRect; AData: Pointer; ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer);
var
  Temp:      TBitmap;
  RawImage:  TRawImage;
  BitmapHandle, MaskHandle: HBitmap;
  TempData:  Pointer;
  x, y:      integer;
  PTempData: PByte;
  PSource:   PByte;
  ADataSize: integer;
  ALineEndMargin: integer;
  CreateResult: boolean;
  {$IFDEF DARWIN}
  TempShift: Byte;
  {$ENDIF}
begin
  if (AHeight = 0) or (AWidth = 0) then
    exit;

  ALineEndMargin := (4 - ((AWidth * 3) and 3)) and 3;
  ADataSize      := (AWidth * 3 + ALineEndMargin) * AHeight;

     {$HINTS OFF}
  GetMem(TempData, ADataSize);
     {$HINTS ON}
  PTempData := TempData;
  PSource   := AData;

{$IFDEF DARWIN} //swap red and blue values
  for y := 0 to AHeight - 1 do
  begin
    for x := 0 to AWidth - 1 do
    begin
      PTempData^ := (PSource+2)^;
      (PTempData+1)^ := (PSource+1)^;
      (PTempData+2)^ := PSource^;
      inc(PTempData,3);
      inc(PSource,4);
    end;
    Inc(PTempData, ALineEndMargin);
  end;
{$ELSE}
  for y := 0 to AHeight - 1 do
  begin
    for x := 0 to AWidth - 1 do
    begin
      PWord(PTempData)^ := PWord(PSource)^;
      (PTempData+2)^ := (PSource+2)^;
      Inc(PTempData,3);
      Inc(PSource, 4);
    end;
    Inc(PTempData, ALineEndMargin);
  end;
{$ENDIF}

  RawImage.Init;
  RawImage.Description.Init_BPP24_B8G8R8_BIO_TTB(AWidth, AHeight);
{$IFDEF DARWIN}
  TempShift := RawImage.Description.RedShift;
  RawImage.Description.RedShift := RawImage.Description.BlueShift;
  RawImage.Description.BlueShift := TempShift;
{$ENDIF}

  RawImage.Description.LineOrder := ALineOrder;
  RawImage.Description.LineEnd := rileDWordBoundary;

  if integer(RawImage.Description.BytesPerLine) <> AWidth * 3 + ALineEndMargin then
  begin
    FreeMem(TempData);
    raise FPImageException.Create('Line size is inconsistant');
  end;
  RawImage.Data     := PByte(TempData);
  RawImage.DataSize := ADataSize;

  CreateResult := RawImage_CreateBitmaps(RawImage, BitmapHandle, MaskHandle, False);
  FreeMem(TempData);

  if not CreateResult then
    raise FPImageException.Create('Failed to create bitmap handle');

  Temp := TBitmap.Create;
  Temp.Handle := BitmapHandle;
  Temp.MaskHandle := MaskHandle;
  ACanvas.StretchDraw(Rect, Temp);
  Temp.Free;
end;

{-------------------------- Allocation routines -------------------------------}

procedure TBGRADefaultBitmap.ReallocData;
begin
  FreeBitmap;
  ReAllocMem(FData, NbPixels * sizeof(TBGRAPixel));
  if (NbPixels > 0) and (FData = nil) then
    raise EOutOfMemory.Create('TBGRADefaultBitmap: Not enough memory');
  InvalidateBitmap;
  FScanPtr := nil;
end;

procedure TBGRADefaultBitmap.FreeData;
begin
  freemem(FData);
  FData := nil;
end;

procedure TBGRADefaultBitmap.RebuildBitmap;
var
  RawImage: TRawImage;
  BitmapHandle, MaskHandle: HBitmap;
begin
  if FBitmap <> nil then
    FBitmap.Free;

  FBitmap := TBitmapTracker.Create(self);

  if (FWidth > 0) and (FHeight > 0) then
  begin
    RawImage.Init;
    RawImage.Description.Init_BPP32_B8G8R8A8_BIO_TTB(FWidth, FHeight);
    RawImage.Description.LineOrder := FLineOrder;
    RawImage.Data     := PByte(FData);
    RawImage.DataSize := FWidth * FHeight * sizeof(TBGRAPixel);
    if not RawImage_CreateBitmaps(RawImage, BitmapHandle, MaskHandle, False) then
      raise FPImageException.Create('Failed to create bitmap handle');
    FBitmap.Handle     := BitmapHandle;
    FBitmap.MaskHandle := MaskHandle;
  end;

  FBitmap.Canvas.AntialiasingMode := amOff;
  FBitmapModified := False;
end;

procedure TBGRADefaultBitmap.FreeBitmap;
begin
  FreeAndNil(FBitmap);
end;

procedure TBGRADefaultBitmap.GetImageFromCanvas(CanvasSource: TCanvas; x, y: integer);
var
  bmp: TBitmap;
  subBmp: TBGRACustomBitmap;
  subRect: TRect;
  cw,ch: integer;
begin
  DiscardBitmapChange;
  cw := CanvasSource.Width;
  ch := CanvasSource.Height;
  if (x < 0) or (y < 0) or (x+Width > cw) or
    (y+Height > ch) then
  begin
    FillTransparent;
    if (x+Width <= 0) or (y+Height <= 0) or
      (x >= cw) or (y >= ch) then
      exit;

    if (x > 0) then subRect.Left := x else subRect.Left := 0;
    if (y > 0) then subRect.Top := y else subRect.Top := 0;
    if (x+Width > cw) then subRect.Right := cw else
      subRect.Right := x+Width;
    if (y+Height > ch) then subRect.Bottom := ch else
      subRect.Bottom := y+Height;

    subBmp := NewBitmap(subRect.Right-subRect.Left,subRect.Bottom-subRect.Top);
    subBmp.GetImageFromCanvas(CanvasSource,subRect.Left,subRect.Top);
    PutImage(subRect.Left-x,subRect.Top-y,subBmp,dmSet);
    subBmp.Free;
    exit;
  end;
  bmp := TBitmap.Create;
  bmp.PixelFormat := pf24bit;
  bmp.Width := Width;
  bmp.Height := Height;
  bmp.Canvas.CopyRect(Classes.rect(0, 0, Width, Height), CanvasSource,
    Classes.rect(x, y, x + Width, y + Height));
  LoadFromRawImage(bmp.RawImage, 255, True);
  bmp.Free;
  InvalidateBitmap;
end;

function TBGRADefaultBitmap.GetNbPixels: integer;
begin
  result := FNbPixels;
end;

function TBGRADefaultBitmap.GetWidth: integer;
begin
  Result := FWidth;
end;

function TBGRADefaultBitmap.GetHeight: integer;
begin
  Result:= FHeight;
end;

function TBGRADefaultBitmap.GetRefCount: integer;
begin
  result := FRefCount;
end;

function TBGRADefaultBitmap.GetLineOrder: TRawImageLineOrder;
begin
  result := FLineOrder;
end;

function TBGRADefaultBitmap.GetCanvasOpacity: byte;
begin
  result:= FCanvasOpacity;
end;

function TBGRADefaultBitmap.GetFontHeight: integer;
begin
  result := FFontHeight;
end;

{ TBGRAPtrBitmap }

procedure TBGRAPtrBitmap.ReallocData;
begin
  //nothing
end;

procedure TBGRAPtrBitmap.FreeData;
begin
  FData := nil;
end;

constructor TBGRAPtrBitmap.Create(AWidth, AHeight: integer; AData: Pointer);
begin
  inherited Create(AWidth, AHeight);
  SetDataPtr(AData);
end;

function TBGRAPtrBitmap.Duplicate(DuplicateProperties: Boolean = False): TBGRACustomBitmap;
begin
  Result := NewBitmap(Width, Height);
  if DuplicateProperties then CopyPropertiesTo(TBGRADefaultBitmap(Result));
end;

procedure TBGRAPtrBitmap.SetDataPtr(AData: Pointer);
begin
  FData := AData;
end;

procedure BGRAGradientFill(bmp: TBGRACustomBitmap; x, y, x2, y2: integer;
  c1, c2: TBGRAPixel; gtype: TGradientType; o1, o2: TPointF; mode: TDrawMode;
  gammaColorCorrection: boolean = True; Sinus: Boolean=False);
var
  gradScan : TBGRAGradientScanner;
begin
  //handles transparency
  if (c1.alpha = 0) and (c2.alpha = 0) then
  begin
    bmp.FillRect(x, y, x2, y2, BGRAPixelTransparent, mode);
    exit;
  end;

  gradScan := TBGRAGradientScanner.Create(c1,c2,gtype,o1,o2,gammaColorCorrection,Sinus);
  bmp.FillRect(x,y,x2,y2,gradScan,mode);
  gradScan.Free;
end;

initialization

  with DefaultTextStyle do
  begin
    Alignment  := taLeftJustify;
    Layout     := tlTop;
    WordBreak  := True;
    SingleLine := True;
    Clipping   := True;
    ShowPrefix := False;
    Opaque     := False;
  end;

  ImageHandlers.RegisterImageWriter ('Personal Computer eXchange', 'pcx', TFPWriterPcx);
  ImageHandlers.RegisterImageReader ('Personal Computer eXchange', 'pcx', TFPReaderPcx);

  ImageHandlers.RegisterImageWriter ('X Pixmap', 'xpm', TFPWriterXPM);
  ImageHandlers.RegisterImageReader ('X Pixmap', 'xpm', TFPReaderXPM);

end.

