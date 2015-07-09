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
  SysUtils, Classes, Types, FPImage, BGRAGraphics, BGRABitmapTypes, FPImgCanv,
  BGRACanvas, BGRACanvas2D, BGRAArrow, BGRAPen;

type
  TBGRAPtrBitmap = class;
  {=== TBGRABitmap reference ===}
  { TBGRADefaultBitmap }
  {* This class is the base for all ''TBGRABitmap'' classes. It implements most
     function to the exception from implementations specific to the
     widgetset }{ in the doc, it is presented as
  TBGRABitmap = class(TBGRACustomBitmap)
  }
  TBGRADefaultBitmap = class(TBGRACustomBitmap)
  private
    { Bounds checking which are shared by drawing functions. These functions check
      if the coordinates are visible and return true if it is the case, swap
      coordinates if necessary and make them fit into the clipping rectangle }
    function CheckHorizLineBounds(var x, y, x2: int32or64): boolean; inline;
    function CheckVertLineBounds(var x, y, y2: int32or64; out delta: int32or64): boolean; inline;
    function CheckRectBounds(var x,y,x2,y2: integer; minsize: integer): boolean; inline;
    function CheckClippedRectBounds(var x,y,x2,y2: integer): boolean; inline;
    function CheckAntialiasRectBounds(var x,y,x2,y2: single; w: single): boolean;
    function GetCanvasBGRA: TBGRACanvas;
    function GetCanvas2D: TBGRACanvas2D;
  protected
    FRefCount: integer; //reference counter (not related to interface reference counter)

    //Pixel data
    FData:      PBGRAPixel;              //pointer to pixels
    FWidth, FHeight, FNbPixels: integer; //dimensions
    FScanWidth, FScanHeight: integer;    //possibility to reduce the zone being scanned
    FDataModified: boolean;              //if data image has changed so TBitmap should be updated
    FLineOrder: TRawImageLineOrder;
    FClipRect:  TRect;                   //clipping (can be the whole image if there is no clipping)

    //Scan
    FScanPtr : PBGRAPixel;          //current scan address
    FScanCurX,FScanCurY: integer;   //current scan coordinates

    //GUI bitmap object
    FBitmap:   TBitmap;
    FBitmapModified: boolean;         //if TBitmap has changed so pixel data should be updated
    FCanvasOpacity: byte;             //opacity used with standard canvas functions
    FAlphaCorrectionNeeded: boolean;  //the alpha channel is not correct because standard functions do not
                                      //take it into account

    //FreePascal drawing routines
    FCanvasFP: TFPImageCanvas;
    FCanvasDrawModeFP: TDrawMode;
    FCanvasPixelProcFP: procedure(x, y: int32or64; col: TBGRAPixel) of object;

    //canvas-like with antialiasing and texturing
    FCanvasBGRA: TBGRACanvas;
    FCanvas2D: TBGRACanvas2D;

    //drawing options
    FEraseMode: boolean;      //when polygons are erased instead of drawn
    FFontHeight: integer;
    FFontRenderer: TBGRACustomFontRenderer;

    { Pen style can be defined by PenStyle property of by CustomPenStyle property.
      When PenStyle property is assigned, CustomPenStyle property is assigned the actual
      pen pattern. }
    FCustomPenStyle:  TBGRAPenStyle;
    FPenStyle: TPenStyle;
    FArrow: TBGRAArrow;
    FLineCap: TPenEndCap;

    //Pixel data
    function GetRefCount: integer; override;
    function GetScanLine(y: integer): PBGRAPixel; override; //don't forget to call InvalidateBitmap after modifications
    function LoadFromRawImage(ARawImage: TRawImage; DefaultOpacity: byte;
      AlwaysReplaceAlpha: boolean = False; RaiseErrorOnInvalidPixelFormat: boolean = True): boolean; virtual; abstract;
    function GetDataPtr: PBGRAPixel; override;
    procedure ClearTransparentPixels; override;
    function GetScanlineFast(y: integer): PBGRAPixel; inline;
    function GetLineOrder: TRawImageLineOrder; override;
    procedure SetLineOrder(AValue: TRawImageLineOrder); virtual;
    function GetNbPixels: integer; override;
    function GetWidth: integer; override;
    function GetHeight: integer; override;

    //GUI bitmap object
    function GetBitmap: TBitmap; override;
    function GetCanvas: TCanvas; override;
    procedure DiscardBitmapChange; inline;
    procedure DoAlphaCorrection;
    procedure SetCanvasOpacity(AValue: byte); override;
    function GetCanvasOpacity: byte; override;
    function GetCanvasAlphaCorrection: boolean; override;
    procedure SetCanvasAlphaCorrection(const AValue: boolean); override;
    procedure DoLoadFromBitmap; virtual;

    //FreePascal drawing routines
    function GetCanvasFP: TFPImageCanvas; override;
    procedure SetCanvasDrawModeFP(const AValue: TDrawMode); override;
    function GetCanvasDrawModeFP: TDrawMode; override;

    {Allocation routines}
    procedure ReallocData; virtual;
    procedure FreeData; virtual;
    function CreatePtrBitmap(AWidth,AHeight: integer; AData: PBGRAPixel): TBGRAPtrBitmap; virtual;

    procedure RebuildBitmap; virtual; abstract;
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

    //drawing
    function GetCustomPenStyle: TBGRAPenStyle; override;
    procedure SetCustomPenStyle(const AValue: TBGRAPenStyle); override;
    procedure SetPenStyle(const AValue: TPenStyle); override;
    function GetPenStyle: TPenStyle; override;
    function GetLineCap: TPenEndCap; override;
    procedure SetLineCap(AValue: TPenEndCap); override;
    function GetArrowEndSize: TPointF; override;
    function GetArrowStartSize: TPointF; override;
    procedure SetArrowEndSize(AValue: TPointF); override;
    procedure SetArrowStartSize(AValue: TPointF); override;
    function GetArrowEndOffset: single; override;
    function GetArrowStartOffset: single; override;
    procedure SetArrowEndOffset(AValue: single); override;
    procedure SetArrowStartOffset(AValue: single); override;
    function GetArrowEndRepeat: integer; override;
    function GetArrowStartRepeat: integer; override;
    procedure SetArrowEndRepeat(AValue: integer); override;
    procedure SetArrowStartRepeat(AValue: integer); override;

    function GetFontHeight: integer; override;
    procedure SetFontHeight(AHeight: integer); override;
    function GetFontFullHeight: integer; override;
    procedure SetFontFullHeight(AHeight: integer); override;
    function GetFontPixelMetric: TFontPixelMetric; override;
    function GetFontRenderer: TBGRACustomFontRenderer; override;
    procedure SetFontRenderer(AValue: TBGRACustomFontRenderer); override;
    function CreateDefaultFontRenderer: TBGRACustomFontRenderer; virtual; abstract;
    function GetFontAnchorVerticalOffset: single;
    function GetFontAnchorRotatedOffset: TPointF;
    function GetFontAnchorRotatedOffset(ACustomOrientation: integer): TPointF;

    function GetClipRect: TRect; override;
    procedure SetClipRect(const AValue: TRect); override;

    function InternalGetPixelCycle256(ix,iy: int32or64; iFactX,iFactY: int32or64): TBGRAPixel;
    function InternalGetPixel256(ix,iy: int32or64; iFactX,iFactY: int32or64; smoothBorder: boolean): TBGRAPixel;
    function GetPolyLineOption: TBGRAPolyLineOptions;
    function GetArrow: TBGRAArrow;
    procedure SetArrowStart(AStyle: TBGRAArrowStyle; ATipStyle: TPenJoinStyle = pjsMiter; ARelativePenWidth: single = 1; ATriangleBackOffset: single = 0); override;
    procedure SetArrowEnd(AStyle: TBGRAArrowStyle; ATipStyle: TPenJoinStyle = pjsMiter; ARelativePenWidth: single = 1; ATriangleBackOffset: single = 0); override;
    procedure InternalTextOutCurved(ACursor: TBGRACustomPathCursor; sUTF8: string; AColor: TBGRAPixel; ATexture: IBGRAScanner; AAlign: TAlignment; ALetterSpacing: single);

    procedure InternalArc(cx,cy,rx,ry: single; StartAngleRad,EndAngleRad: Single; ABorderColor: TBGRAPixel; w: single;
      AFillColor: TBGRAPixel; AOptions: TArcOptions; ADrawChord: boolean = false; ATexture: IBGRAScanner = nil); override;

  public
    {** Provides a canvas with opacity and antialiasing }
    property CanvasBGRA: TBGRACanvas read GetCanvasBGRA;
    {** Provides a canvas with 2d transformation and similar to HTML5. }
    property Canvas2D: TBGRACanvas2D read GetCanvas2D;
    {** For more properties, see parent class [[TBGRACustomBitmap and IBGRAScanner#TBGRACustomBitmap|TBGRACustomBitmap]] }

    {==== Reference counting ====}

    {** Adds a reference (this reference count is not the same as
        the reference count of an interface, it changes only by
        explicit calls }
    function NewReference: TBGRACustomBitmap;
    {** Free a reference. When the resulting reference count gets
        to zero, the image is freed. The initial reference count
        is equal to 1 }
    procedure FreeReference;
    {** Returns an object with a reference count equal to 1. Duplicate
        this bitmap if necessary }
    function GetUnique: TBGRACustomBitmap;

    {==== Constructors ====}

    {------------------------- Constructors from TFPCustomImage----------------}
    {** Creates a new bitmap, initialize properties and bitmap data }
    constructor Create(AWidth, AHeight: integer); override;
    {** Can only be called with an existing instance of ''TBGRABitmap''.
        Sets the dimensions of an existing ''TBGRABitmap'' instance. }
    procedure SetSize(AWidth, AHeight: integer); override;

    {------------------------- Constructors from TBGRACustomBitmap-------------}
    {** Creates an image of width and height equal to zero. In this case,
        ''Data'' = '''nil''' }
    constructor Create; override;
    {** Creates an image by copying the content of a ''TFPCustomImage'' }
    constructor Create(AFPImage: TFPCustomImage); override;
    {** Creates an image by copying the content of a ''TBitmap'' }
    constructor Create(ABitmap: TBitmap); override;
    {** Creates an image of dimensions ''AWidth'' and ''AHeight'' and fills it with the opaque color ''Color'' }
    constructor Create(AWidth, AHeight: integer; Color: TColor); override;
    {** Creates an image of dimensions ''AWidth'' and ''AHeight'' and fills it with ''Color'' }
    constructor Create(AWidth, AHeight: integer; Color: TBGRAPixel); override;

    {** Creates an image by loading its content from the file ''AFilename''.
        The encoding of the string is the default one for the operating system.
        It is recommended to use the next constructor and UTF8 encoding }
    constructor Create(AFilename: string); override;

    {** Creates an image by loading its content from the file ''AFilename''.
        The boolean ''AIsUtf8Filename'' specifies if UTF8 encoding is assumed
        for the filename }
    constructor Create(AFilename: string; AIsUtf8: boolean); override;

    {** Creates an image by loading its content from the stream ''AStream'' }
    constructor Create(AStream: TStream); override;
    {** Free the object and all its resources }
    destructor Destroy; override;

    {------------------------- Quasi-constructors -----------------------------}
    {** Can only be called from an existing instance of ''TBGRABitmap''.
        Creates a new instance with dimensions ''AWidth'' and ''AHeight'',
        containing transparent pixels. }
    function NewBitmap(AWidth, AHeight: integer): TBGRACustomBitmap; override;

    {** Can only be called from an existing instance of ''TBGRABitmap''.
        Creates a new instance with dimensions ''AWidth'' and ''AHeight'',
        and fills it with Color }
    function NewBitmap(AWidth, AHeight: integer; Color: TBGRAPixel): TBGRACustomBitmap; override;

    {** Can only be called from an existing instance of ''TBGRABitmap''.
        Creates a new instance with by loading its content
        from the file ''Filename''. The encoding of the string
        is the default one for the operating system }
    function NewBitmap(Filename: string): TBGRACustomBitmap; override;

    {** Can only be called from an existing instance of ''TBGRABitmap''.
        Creates a new instance with by loading its content
        from the file ''Filename'' }
    function NewBitmap(Filename: string; AIsUtf8: boolean): TBGRACustomBitmap; override;

    {** Can only be called from an existing instance of ''TBGRABitmap''.
        Creates an image by copying the content of a ''TFPCustomImage'' }
    function NewBitmap(AFPImage: TFPCustomImage): TBGRACustomBitmap; override;

    {** Assign the content of the specified ''Source''. It can be a ''TBGRACustomBitmap'' or
        a ''TFPCustomImage'' }
    procedure Assign(Source: TPersistent); override;
    {** Stores the image in the stream without compression nor header }
    procedure Serialize(AStream: TStream); override;
    {** Reads the image in a stream that was previously serialized }
    procedure Deserialize(AStream: TStream); override;
    {** Stores an empty image (of size zero) }
    class procedure SerializeEmpty(AStream: TStream);

    {* Example:
       <syntaxhighlight>
     * var bmp1, bmp2: TBGRABitmap;
     * begin
     *   bmp1 := TBGRABitmap.Create(100,100);
     *   bmp2 := bmp1.NewBitmap(100,100) as TBGRABitmap;
     *   ...
     * end;</syntaxhighlight>
       See tutorial 2 on [[BGRABitmap_tutorial_2|how to load and display an image]].
     * See reference on [[TBGRACustomBitmap_and_IBGRAScanner#Load_and_save_files|loading and saving files]] }

    {==== Pixel functions ====}
    {** Checks if the specified point is in the clipping rectangle ''ClipRect'' }
    function PtInClipRect(x, y: int32or64): boolean; inline;
    {** Sets the pixel by replacing the content at (''x'',''y'') with the specified color.
        Alpha value is set to 255 (opaque) }
    procedure SetPixel(x, y: int32or64; c: TColor); override;
    {** Sets the pixel at (''x'',''y'') with the specified content }
    procedure SetPixel(x, y: int32or64; c: TBGRAPixel); override;
    {** Applies a logical '''xor''' to the content of the pixel with the specified value.
        This includes the alpha channel, so if you want to preserve the opacity, provide
        a color ''c'' with alpha channel equal to zero }
    procedure XorPixel(x, y: int32or64; c: TBGRAPixel); override;
    {** Draws a pixel with gamma correction at (''x'',''y''). Pixel is supplied
        in sRGB colorspace }
    procedure DrawPixel(x, y: int32or64; c: TBGRAPixel); override;
    {** Draws a pixel with the specified ''ADrawMode'' at (''x'',''y'').
        Pixel is supplied in sRGB colorspace. Gamma correction may be applied
        depending on the draw mode }{inherited
    procedure DrawPixel(x, y: int32or64; c: TBGRAPixel; ADrawMode: TDrawMode); overload;
  }{** Draws a pixel with gamma correction at (''x'',''y''). Pixel is supplied
        in gamma expanded colorspace }
    procedure DrawPixel(x, y: int32or64; ec: TExpandedPixel); override;
    {** Draws a pixel without gamma correction at (''x'',''y''). Pixel is supplied
        in sRGB colorspace }
    procedure FastBlendPixel(x, y: int32or64; c: TBGRAPixel); override;
    {** Erase the content of the pixel by reducing the value of the
        alpha channel. ''alpha'' specifies how much to decrease.
        If the resulting alpha reaches zero, the content
        is replaced by ''BGRAPixelTransparent'' }
    procedure ErasePixel(x, y: int32or64; alpha: byte); override;
    {** Sets the alpha value at (''x'',''y''). If ''alpha'' = 0, the
        pixel is replaced by ''BGRAPixelTransparent'' }
    procedure AlphaPixel(x, y: int32or64; alpha: byte); override;
    {** Returns the content of the specified pixel. If it is out of the
        bounds of the picture, the result is ''BGRAPixelTransparent'' }
    function GetPixel(x, y: int32or64): TBGRAPixel; override;
    {** Computes the value of the pixel at a floating point coordiante
        by interpolating the values of the pixels around it.
      * There is a one pixel wide margin around the pixel where the pixels are
        still considered inside. If ''smoothBorder'' is set to true, pixel fade
        to transparent.
      * If it is more out of the bounds, the result is ''BGRAPixelTransparent''.
      * ''AResampleFilter'' specifies how pixels must be interpolated. Accepted
        values are ''rfBox'', ''rfLinear'', ''rfHalfCosine'' and ''rfCosine'' }
    function GetPixel(x, y: single; AResampleFilter: TResampleFilter = rfLinear; smoothBorder: boolean = true): TBGRAPixel; override;
    {** Similar to previous ''GetPixel'' function, but the fractional part of
        the coordinate is supplied with a number from 0 to 255. The actual
        coordinate is (''x'' + ''fracX256''/256, ''y'' + ''fracY256''/256) }
    function GetPixel256(x, y, fracX256,fracY256: int32or64; AResampleFilter: TResampleFilter = rfLinear; smoothBorder: boolean = true): TBGRAPixel; override;
    {** Computes the value of the pixel at a floating point coordiante
        by interpolating the values of the pixels around it. If the pixel
        is out of bounds, the image is repeated.
      * ''AResampleFilter'' specifies how pixels must be interpolated. Accepted
        values are ''rfBox'', ''rfLinear'', ''rfHalfCosine'' and ''rfCosine'' }
    function GetPixelCycle(x, y: single; AResampleFilter: TResampleFilter = rfLinear): TBGRAPixel; override;
    {** Similar to previous ''GetPixel'' function, but the fractional part of
        the coordinate is supplied with a number from 0 to 255. The actual
        coordinate is (''x'' + ''fracX256''/256, ''y'' + ''fracY256''/256) }
    function GetPixelCycle256(x, y, fracX256,fracY256: int32or64; AResampleFilter: TResampleFilter = rfLinear): TBGRAPixel; override;
    {** Computes the value of the pixel at a floating point coordiante
        by interpolating the values of the pixels around it. ''repeatX'' and
        ''repeatY'' specifies if the image is to be repeated or not.
      * ''AResampleFilter'' specifies how pixels must be interpolated. Accepted
        values are ''rfBox'', ''rfLinear'', ''rfHalfCosine'' and ''rfCosine'' }
    function GetPixelCycle(x, y: single; AResampleFilter: TResampleFilter; repeatX: boolean; repeatY: boolean): TBGRAPixel; override;
    {** Similar to previous ''GetPixel'' function, but the fractional part of
        the coordinate is supplied with a number from 0 to 255. The actual
        coordinate is (''x'' + ''fracX256''/256, ''y'' + ''fracY256''/256) }
    function GetPixelCycle256(x, y, fracX256,fracY256: int32or64; AResampleFilter: TResampleFilter; repeatX: boolean; repeatY: boolean): TBGRAPixel; override;

    {==== Drawing lines and polylines (integer coordinates) ====}
    {* These functions do not take into account current pen style/cap/join.
       See [[BGRABitmap tutorial 13|coordinate system]]. }

    {** Replaces the content of the pixels at line ''y'' and
        at columns ''x'' to ''x2'' included, using specified color }
    procedure SetHorizLine(x, y, x2: int32or64; c: TBGRAPixel); override;
    {** Applies xor to the pixels at line ''y'' and
        at columns ''x'' to ''x2'' included, using specified color.
        This includes the alpha channel, so if you want to preserve the
        opacity, provide a color ''c'' with alpha channel equal to zero }
    procedure XorHorizLine(x, y, x2: int32or64; c: TBGRAPixel); override;
    {** Draws an horizontal line with gamma correction at line ''y'' and
        at columns ''x'' to ''x2'' included, using specified color }
    procedure DrawHorizLine(x, y, x2: int32or64; c: TBGRAPixel); override;
    {** Draws an horizontal line with gamma correction at line ''y'' and
        at columns ''x'' to ''x2'' included, using specified color }
    procedure DrawHorizLine(x, y, x2: int32or64; ec: TExpandedPixel); override;
    {** Draws an horizontal line with gamma correction at line ''y'' and
        at columns ''x'' to ''x2'' included, using specified scanner
        to get the source colors }{inherited
    procedure DrawHorizLine(x, y, x2: int32or64; texture: IBGRAScanner); overload;
   }{** Draws an horizontal line without gamma correction at line ''y'' and
        at columns ''x'' to ''x2'' included, using specified color }
    procedure FastBlendHorizLine(x, y, x2: int32or64; c: TBGRAPixel); override;
    {** Draws an horizontal line at line ''y'' and
        at columns ''x'' to ''x2'' included, using specified scanner
        and the specified ''ADrawMode'' }
    procedure HorizLine(x, y, x2: int32or64; texture: IBGRAScanner; ADrawMode : TDrawMode); override;
    {** Draws an horizontal line at line ''y'' and
        at columns ''x'' to ''x2'' included, using specified color
        and the specified ''ADrawMode'' }{inherited
    procedure HorizLine(x,y,x2: Int32or64; c: TBGRAPixel; ADrawMode: TDrawMode); overload;
    }
    {** Replaces the alpha value of the pixels at line ''y'' and
        at columns ''x'' to ''x2'' included }
    procedure AlphaHorizLine(x, y, x2: int32or64; alpha: byte); override;
    {** Draws an horizontal line with gamma correction at line ''y'' and
        at columns ''x'' to ''x2'' included, using specified color,
        and with a transparency that increases with the color difference
        with ''compare''. If the difference is greater than ''maxDiff'',
        pixels are not changed }
    procedure DrawHorizLineDiff(x, y, x2: int32or64; c, compare: TBGRAPixel;
      maxDiff: byte); override;

    {** Replaces a vertical line at column ''x'' and at row ''y'' to ''y2'' }
    procedure SetVertLine(x, y, y2: int32or64; c: TBGRAPixel); override;
    {** Xors a vertical line at column ''x'' and at row ''y'' to ''y2'' }
    procedure XorVertLine(x, y, y2: int32or64; c: TBGRAPixel); override;
    {** Draws a vertical line with gamma correction at column ''x'' and at row ''y'' to ''y2'' }
    procedure DrawVertLine(x, y, y2: int32or64; c: TBGRAPixel); override;
    {** Draws a vertical line without gamma correction at column ''x'' and at row ''y'' to ''y2'' }
    procedure FastBlendVertLine(x, y, y2: int32or64; c: TBGRAPixel); override;
    {** Replace alpha values in a vertical line at column ''x'' and at row ''y'' to ''y2'' }
    procedure AlphaVertLine(x, y, y2: int32or64; alpha: byte); override;
    {** Draws a vertical line with the specified draw mode at column ''x'' and at row ''y'' to ''y2'' }{inherited
    procedure VertLine(x,y,y2: Int32or64; c: TBGRAPixel; ADrawMode: TDrawMode);
    }

    {** Draws an aliased line from (x1,y1) to (x2,y2) using Bresenham's algorithm
        ''c'' specifies the color. ''DrawLastPixel'' specifies if (x2,y2) must be drawn.
        ''ADrawMode'' specifies the mode to use when drawing the pixels }
    procedure DrawLine(x1, y1, x2, y2: integer; c: TBGRAPixel; DrawLastPixel: boolean; ADrawMode: TDrawMode = dmDrawWithTransparency); override;
    {** Draws an antialiased line from (x1,y1) to (x2,y2) using an improved version of Bresenham's algorithm
        ''c'' specifies the color. ''DrawLastPixel'' specifies if (x2,y2) must be drawn }
    procedure DrawLineAntialias(x1, y1, x2, y2: integer; c: TBGRAPixel; DrawLastPixel: boolean); override;
    {** Draws an antialiased line with two colors ''c1'' and ''c2'' as dashes of lenght ''dashLen'' }
    procedure DrawLineAntialias(x1, y1, x2, y2: integer; c1, c2: TBGRAPixel; dashLen: integer; DrawLastPixel: boolean); override;
    {** Draws an antialiased line with two colors ''c1'' and ''c2'' as dashes of lenght ''dashLen''.
        ''DashPos'' can be used to specify the start dash position and to retrieve the dash position at the end
        of the line, in order to draw a polyline with consistent dashes }
    procedure DrawLineAntialias(x1, y1, x2, y2: integer; c1, c2: TBGRAPixel; dashLen: integer; DrawLastPixel: boolean; var DashPos: integer); override;

    {** Erases the line from (x1,y1) to (x2,y2) using Bresenham's algorithm.
        ''alpha'' specifies how much to decrease. If ''alpha'' = 0, nothing
        is changed and if ''alpha'' = 255, all pixels become transparent.
        ''DrawListPixel'' specifies if (x2,y2) must be changed }
    procedure EraseLine(x1, y1, x2, y2: integer; alpha: byte; DrawLastPixel: boolean); override;
    {** Erases the line from (x1,y1) to (x2,y2) width antialiasing.
        ''alpha'' specifies how much to decrease. If ''alpha'' = 0, nothing
        is changed and if ''alpha'' = 255, all pixels become transparent.
        ''DrawListPixel'' specifies if (x2,y2) must be changed }
    procedure EraseLineAntialias(x1, y1, x2, y2: integer; alpha: byte; DrawLastPixel: boolean); override;

    {==== Drawing lines and polylines (floating point coordinates) ====}
    {* These functions use the current pen style/cap/join. The parameter ''w''
       specifies the width of the line and the base unit for dashes.
       See [[BGRABitmap tutorial 13|coordinate system]]. }

    {** Draws a line from (x1,y1) to (x2,y2) using current pen style/cap/join }
    procedure DrawLineAntialias(x1, y1, x2, y2: single; c: TBGRAPixel; w: single); override;
    {** Draws a line from (x1,y1) to (x2,y2) using current pen style/cap/join.
        ''texture'' specifies the source color to use when filling the line }
    procedure DrawLineAntialias(x1, y1, x2, y2: single; texture: IBGRAScanner; w: single); override;
    {** Draws a line from (x1,y1) to (x2,y2) using current pen style/cap/join.
        ''Closed'' specifies if the end of the line is closed. If it is not closed,
        a space is left so that the next line can fit }
    procedure DrawLineAntialias(x1, y1, x2, y2: single; c: TBGRAPixel; w: single; Closed: boolean); override;
    {** Same as above with ''texture'' specifying the source color to use when filling the line }
    procedure DrawLineAntialias(x1, y1, x2, y2: single; texture: IBGRAScanner; w: single; Closed: boolean); override;

    {** Draws a polyline using current pen style/cap/join }
    procedure DrawPolyLineAntialias(const points: array of TPointF; c: TBGRAPixel; w: single); override;
    {** Draws a polyline using current pen style/cap/join.
        ''texture'' specifies the source color to use when filling the line }
    procedure DrawPolyLineAntialias(const points: array of TPointF; texture: IBGRAScanner; w: single); override;
    {** Draws a polyline using current pen style/cap/join.
        ''Closed'' specifies if the end of the line is closed. If it is not closed,
        a space is left so that the next line can fit }
    procedure DrawPolyLineAntialias(const points: array of TPointF; c: TBGRAPixel; w: single; Closed: boolean); override;
    {** Draws a polyline using current pen style/cap/join.
        ''fillcolor'' specifies a color to fill the polygon formed by the points }
    procedure DrawPolyLineAntialias(const points: array of TPointF; c: TBGRAPixel; w: single; fillcolor: TBGRAPixel); override;
    {** Draws a polyline using current pen style/cap/join.
        The last point considered as a join with the first point if it has
        the same coordinate }
    procedure DrawPolyLineAntialiasAutocycle(const points: array of TPointF; c: TBGRAPixel; w: single); override;
    {** Draws a polygon using current pen style/cap/join.
        The polygon is always closed. You don't need to set the last point
        to be the same as the first point }
    procedure DrawPolygonAntialias(const points: array of TPointF; c: TBGRAPixel; w: single); override;
    {** Draws a polygon using current pen style/cap/join.
        The polygon is always closed. You don't need to set the last point
        to be the same as the first point }
    procedure DrawPolygonAntialias(const points: array of TPointF; texture: IBGRAScanner; w: single); override;
    {** Draws a filled polygon using current pen style/cap/join.
        The polygon is always closed. You don't need to set the last point
        to be the same as the first point. }
    procedure DrawPolygonAntialias(const points: array of TPointF; c: TBGRAPixel; w: single; fillcolor: TBGRAPixel); override;

    {** Erases a line from (x1,y1) to (x2,y2) using current pen style/cap/join }
    procedure EraseLineAntialias(x1, y1, x2, y2: single; alpha: byte; w: single); override;
    {** Erases a line from (x1,y1) to (x2,y2) using current pen style/cap/join.
        ''Closed'' specifies if the end of the line is closed. If it is not closed,
        a space is left so that the next line can fit }
    procedure EraseLineAntialias(x1, y1, x2, y2: single; alpha: byte; w: single; Closed: boolean); override;
    {** Erases a polyline using current pen style/cap/join }
    procedure ErasePolyLineAntialias(const points: array of TPointF; alpha: byte; w: single); override;

    {==== Rectangles (integer coordinates) ====}
    {* The integer coordinates of rectangles interpreted such that
       that the bottom/right pixels are not drawn. The width is equal
       to x2-x, and pixels are drawn from x to x2-1. If x = x2, then nothing
       is drawn. See [[BGRABitmap tutorial 13|coordinate system]].
     * These functions do not take into account current pen style/cap/join.
       They draw a continuous 1-pixel width border }

    {** Draw a size border of a rectangle,
        using the specified ''mode'' }
    procedure Rectangle(x, y, x2, y2: integer; c: TBGRAPixel; mode: TDrawMode); override;
    {** Draw a filled rectangle with a border of color ''BorderColor'',
        using the specified ''mode'' }
    procedure Rectangle(x, y, x2, y2: integer; BorderColor, FillColor: TBGRAPixel; mode: TDrawMode); override;
    {** Fills completely a rectangle, without any border, with the specified ''mode'' }
    procedure FillRect(x, y, x2, y2: integer; c: TBGRAPixel; mode: TDrawMode); override; overload;
    {** Fills completely a rectangle, without any border, with the specified ''texture'' and
        with the specified ''mode'' }
    procedure FillRect(x, y, x2, y2: integer; texture: IBGRAScanner; mode: TDrawMode); override; overload;
    {** Sets the alpha value within the specified rectangle }
    procedure AlphaFillRect(x, y, x2, y2: integer; alpha: byte); override;
    {** Draws a filled round rectangle, with corners having an elliptical diameter of ''DX'' and ''DY'' }
    procedure RoundRect(X1, Y1, X2, Y2: integer; DX, DY: integer; BorderColor, FillColor: TBGRAPixel; ADrawMode: TDrawMode = dmDrawWithTransparency); override;
    {** Draws a round rectangle, with corners having an elliptical diameter of ''DX'' and ''DY'' }
    procedure RoundRect(X1, Y1, X2, Y2: integer; DX, DY: integer; BorderColor: TBGRAPixel; ADrawMode: TDrawMode = dmDrawWithTransparency); override;

    {==== Rectangles and ellipses (floating point coordinates) ====}
    {* These functions use the current pen style/cap/join. The parameter ''w''
       specifies the width of the line and the base unit for dashes
     * The coordinates are pixel-centered, so that when filling a rectangle,
       if the supplied values are integers, the border will be half transparent.
       If you want the border to be completely filled, you can subtract/add
       0.5 to the coordinates to include the remaining thin border.
       See [[BGRABitmap tutorial 13|coordinate system]]. }

    {** Draws a rectangle with antialiasing and fills it with color ''back''.
        Note that the pixel (x2,y2) is included contrary to integer coordinates }
    procedure RectangleAntialias(x, y, x2, y2: single; c: TBGRAPixel; w: single; back: TBGRAPixel); override;
    {** Draws a rectangle with antialiasing. Note that the pixel (x2,y2) is
        included contrary to integer coordinates }
    procedure RectangleAntialias(x, y, x2, y2: single; texture: IBGRAScanner; w: single); override;
    {** Fills a rectangle with antialiasing. For example (-0.5,-0.5,0.5,0.5)
        fills one pixel }
    procedure FillRectAntialias(x, y, x2, y2: single; c: TBGRAPixel); override;
    {** Fills a rectangle with a texture }
    procedure FillRectAntialias(x, y, x2, y2: single; texture: IBGRAScanner); override;
    {** Erases the content of a rectangle with antialiasing }
    procedure EraseRectAntialias(x, y, x2, y2: single; alpha: byte); override;

    {** Draws a rounded rectangle border with antialiasing. The corners have an
        elliptical radius of ''rx'' and ''ry''. ''options'' specifies how to
        draw the corners. See [[BGRABitmap Geometry types|geometry types]] }
    procedure RoundRectAntialias(x,y,x2,y2,rx,ry: single; c: TBGRAPixel; w: single; options: TRoundRectangleOptions = []); override;
    {** Draws a rounded rectangle border with the specified texture.
        The corners have an elliptical radius of ''rx'' and ''ry''.
        ''options'' specifies how to draw the corners.
        See [[BGRABitmap Geometry types|geometry types]] }
    procedure RoundRectAntialias(x,y,x2,y2,rx,ry: single; texture: IBGRAScanner; w: single; options: TRoundRectangleOptions = []); override;
    {** Draws and fills a round rectangle }
    procedure RoundRectAntialias(x,y,x2,y2,rx,ry: single; pencolor: TBGRAPixel; w: single; fillcolor: TBGRAPixel; options: TRoundRectangleOptions = []); override;
    {** Draws and fills a round rectangle with textures }
    procedure RoundRectAntialias(x,y,x2,y2,rx,ry: single; penTexture: IBGRAScanner; w: single; fillTexture: IBGRAScanner; options: TRoundRectangleOptions = []); override;

    {** Fills a rounded rectangle with antialiasing. The corners have an
        elliptical radius of ''rx'' and ''ry''. ''options'' specifies how to
        draw the corners. See [[BGRABitmap Geometry types|geometry types]] }
    procedure FillRoundRectAntialias(x,y,x2,y2,rx,ry: single; c: TBGRAPixel; options: TRoundRectangleOptions = []); override;
    {** Fills a rounded rectangle with a texture }
    procedure FillRoundRectAntialias(x,y,x2,y2,rx,ry: single; texture: IBGRAScanner; options: TRoundRectangleOptions = []); override;
    {** Erases the content of a rounded rectangle with a texture }
    procedure EraseRoundRectAntialias(x,y,x2,y2,rx,ry: single; alpha: byte; options: TRoundRectangleOptions = []); override;

    {** Draws an ellipse with antialising. ''rx'' is the horizontal radius and
        ''ry'' the vertical radius }
    procedure EllipseAntialias(x, y, rx, ry: single; c: TBGRAPixel; w: single); override;
    {** Draws an ellipse border with a ''texture'' }
    procedure EllipseAntialias(x, y, rx, ry: single; texture: IBGRAScanner; w: single); override;
    {** Draws and fills an ellipse }
    procedure EllipseAntialias(x, y, rx, ry: single; c: TBGRAPixel; w: single; back: TBGRAPixel); override;
    {** Fills an ellipse }
    procedure FillEllipseAntialias(x, y, rx, ry: single; c: TBGRAPixel); override;
    {** Fills an ellipse with a ''texture'' }
    procedure FillEllipseAntialias(x, y, rx, ry: single; texture: IBGRAScanner); override;
    {** Fills an ellipse with a gradient of color. ''outercolor'' specifies
        the end color of the gradient on the border of the ellipse and
        ''innercolor'' the end color of the gradient at the center of the
        ellipse }
    procedure FillEllipseLinearColorAntialias(x, y, rx, ry: single; outercolor, innercolor: TBGRAPixel); override;
    {** Erases the content of an ellipse }
    procedure EraseEllipseAntialias(x, y, rx, ry: single; alpha: byte); override;

    {==== Polygons and path ====}
    procedure FillPoly(const points: array of TPointF; c: TBGRAPixel; drawmode: TDrawMode); override;
    procedure FillPoly(const points: array of TPointF; texture: IBGRAScanner; drawmode: TDrawMode); override;
    procedure FillPolyAntialias(const points: array of TPointF; c: TBGRAPixel); override;
    procedure FillPolyAntialias(const points: array of TPointF; texture: IBGRAScanner); override;
    procedure ErasePoly(const points: array of TPointF; alpha: byte); override;
    procedure ErasePolyAntialias(const points: array of TPointF; alpha: byte); override;

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
    procedure FillQuadPerspectiveMapping(pt1,pt2,pt3,pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF; ACleanBorders: TRect); override;
    procedure FillQuadPerspectiveMappingAntialias(pt1,pt2,pt3,pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF); override;
    procedure FillQuadPerspectiveMappingAntialias(pt1,pt2,pt3,pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF; ACleanBorders: TRect); override;

    procedure FillPolyLinearMapping(const points: array of TPointF; texture: IBGRAScanner; texCoords: array of TPointF; TextureInterpolation: Boolean); override;
    procedure FillPolyLinearMappingLightness(const points: array of TPointF; texture: IBGRAScanner; texCoords: array of TPointF; lightnesses: array of word; TextureInterpolation: Boolean); override;
    procedure FillPolyLinearColor(const points: array of TPointF; AColors: array of TBGRAPixel); override;
    procedure FillPolyPerspectiveMapping(const points: array of TPointF; const pointsZ: array of single; texture: IBGRAScanner; texCoords: array of TPointF; TextureInterpolation: Boolean; zbuffer: psingle = nil); override;
    procedure FillPolyPerspectiveMappingLightness(const points: array of TPointF; const pointsZ: array of single; texture: IBGRAScanner; texCoords: array of TPointF; lightnesses: array of word; TextureInterpolation: Boolean; zbuffer: psingle = nil); override;

    procedure FillShape(shape: TBGRACustomFillInfo; c: TBGRAPixel; drawmode: TDrawMode); override;
    procedure FillShape(shape: TBGRACustomFillInfo; texture: IBGRAScanner; drawmode: TDrawMode); override;
    procedure FillShapeAntialias(shape: TBGRACustomFillInfo; c: TBGRAPixel); override;
    procedure FillShapeAntialias(shape: TBGRACustomFillInfo; texture: IBGRAScanner); override;
    procedure EraseShape(shape: TBGRACustomFillInfo; alpha: byte); override;
    procedure EraseShapeAntialias(shape: TBGRACustomFillInfo; alpha: byte); override;

    procedure DrawPath(APath: IBGRAPath; c: TBGRAPixel; w: single); override;
    procedure DrawPath(APath: IBGRAPath; texture: IBGRAScanner; w: single); override;
    procedure FillPath(APath: IBGRAPath; c: TBGRAPixel); override;
    procedure FillPath(APath: IBGRAPath; texture: IBGRAScanner); override;
    procedure ErasePath(APath: IBGRAPath; alpha: byte); override;

    { Draws the UTF8 encoded string, with color c.
      If align is taLeftJustify, (x,y) is the top-left corner.
      If align is taCenter, (x,y) is at the top and middle of the text.
      If align is taRightJustify, (x,y) is the top-right corner.
      The value of FontOrientation is taken into account, so that the text may be rotated. }
    procedure TextOut(x, y: single; sUTF8: string; c: TBGRAPixel; align: TAlignment); override; overload;

    { Same as above functions, except that the text is filled using texture.
      The value of FontOrientation is taken into account, so that the text may be rotated. }
    procedure TextOut(x, y: single; sUTF8: string; texture: IBGRAScanner; align: TAlignment); override; overload;

    { Same as above, except that the orientation is specified, overriding the value of the property FontOrientation. }
    procedure TextOutAngle(x, y: single; orientationTenthDegCCW: integer; sUTF8: string; c: TBGRAPixel; align: TAlignment); override; overload;
    procedure TextOutAngle(x, y: single; orientationTenthDegCCW: integer; sUTF8: string; texture: IBGRAScanner; align: TAlignment); override; overload;

    procedure TextOutCurved(ACursor: TBGRACustomPathCursor; sUTF8: string; AColor: TBGRAPixel; AAlign: TAlignment; ALetterSpacing: single); override; overload;
    procedure TextOutCurved(ACursor: TBGRACustomPathCursor; sUTF8: string; ATexture: IBGRAScanner; AAlign: TAlignment; ALetterSpacing: single); override; overload;

    { Draw the UTF8 encoded string at the coordinate (x,y), clipped inside the rectangle ARect.
      Additional style information is provided by the style parameter.
      The color c or texture is used to fill the text. No rotation is applied. }
    procedure TextRect(ARect: TRect; x, y: integer; sUTF8: string; style: TTextStyle; c: TBGRAPixel); override; overload;
    procedure TextRect(ARect: TRect; x, y: integer; sUTF8: string; style: TTextStyle; texture: IBGRAScanner); override; overload;

    { Returns the total size of the string provided using the current font.
      Orientation is not taken into account, so that the width is along the text.  }
    function TextSize(sUTF8: string): TSize; override;

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
    procedure FillMask(x,y: integer; AMask: TBGRACustomBitmap; color: TBGRAPixel; ADrawMode: TDrawMode); override;
    procedure FillMask(x,y: integer; AMask: TBGRACustomBitmap; texture: IBGRAScanner; ADrawMode: TDrawMode); override;
    procedure FillClearTypeMask(x,y: integer; xThird: integer; AMask: TBGRACustomBitmap; color: TBGRAPixel; ARGBOrder: boolean = true); override;
    procedure FillClearTypeMask(x,y: integer; xThird: integer; AMask: TBGRACustomBitmap; texture: IBGRAScanner; ARGBOrder: boolean = true); override;
    procedure ReplaceColor(before, after: TColor); override;
    procedure ReplaceColor(before, after: TBGRAPixel); override;
    procedure ReplaceColor(ABounds: TRect; before, after: TColor); override;
    procedure ReplaceColor(ABounds: TRect; before, after: TBGRAPixel); override;
    procedure ReplaceTransparent(after: TBGRAPixel); override;
    procedure ReplaceTransparent(ABounds: TRect; after: TBGRAPixel); override;
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
    procedure Draw(ACanvas: TCanvas; x, y: integer; Opaque: boolean = True); override;
    procedure Draw(ACanvas: TCanvas; Rect: TRect; Opaque: boolean = True); override;
    procedure InvalidateBitmap; override;         //call if you modify with Scanline
    procedure LoadFromBitmapIfNeeded; override;   //call to ensure that bitmap data is up to date

    {BGRA bitmap functions}
    procedure CrossFade(ARect: TRect; Source1, Source2: IBGRAScanner; AFadePosition: byte; mode: TDrawMode = dmDrawWithTransparency); override;
    procedure CrossFade(ARect: TRect; Source1, Source2: IBGRAScanner; AFadeMask: IBGRAScanner; mode: TDrawMode = dmDrawWithTransparency); override;
    procedure PutImage(x, y: integer; Source: TBGRACustomBitmap; mode: TDrawMode; AOpacity: byte = 255); override;
    procedure PutImageAffine(Origin,HAxis,VAxis: TPointF; Source: TBGRACustomBitmap; AOutputBounds: TRect; AResampleFilter: TResampleFilter; AMode: TDrawMode; AOpacity: Byte=255); override;

    procedure StretchPutImage(ARect: TRect; Source: TBGRACustomBitmap; mode: TDrawMode; AOpacity: byte = 255); override;

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
    function GetImageBounds(Channels: TChannels; ANothingValue: Byte = 0): TRect; override;
    function GetDifferenceBounds(ABitmap: TBGRACustomBitmap): TRect; override;
    function MakeBitmapCopy(BackgroundColor: TColor): TBitmap; override;

    function Resample(newWidth, newHeight: integer;
      mode: TResampleMode = rmFineResample): TBGRACustomBitmap; override;
    procedure VerticalFlip(ARect: TRect); override; overload;
    procedure HorizontalFlip(ARect: TRect); override; overload;
    function RotateCW: TBGRACustomBitmap; override;
    function RotateCCW: TBGRACustomBitmap; override;
    procedure Negative; override;
    procedure NegativeRect(ABounds: TRect); override;
    procedure LinearNegative; override;
    procedure LinearNegativeRect(ABounds: TRect); override;
    procedure InplaceGrayscale; override;
    procedure InplaceGrayscale(ABounds: TRect); override;
    procedure SwapRedBlue; override;
    procedure GrayscaleToAlpha; override;
    procedure AlphaToGrayscale; override;
    procedure ApplyMask(mask: TBGRACustomBitmap; ARect: TRect; AMaskRectTopLeft: TPoint); override;
    procedure ApplyGlobalOpacity(alpha: byte); override;
    procedure ApplyGlobalOpacity(ABounds: TRect; alpha: byte); override;
    procedure ConvertToLinearRGB; override;
    procedure ConvertFromLinearRGB; override;
    procedure DrawCheckers(ARect: TRect; AColorEven,AColorOdd: TBGRAPixel);

    {Filters}
    function FilterSmartZoom3(Option: TMedianOption): TBGRACustomBitmap; override;
    function FilterMedian(Option: TMedianOption): TBGRACustomBitmap; override;
    function FilterSmooth: TBGRACustomBitmap; override;
    function FilterSharpen(Amount: single = 1): TBGRACustomBitmap; override;
    function FilterSharpen(ABounds: TRect; Amount: single = 1): TBGRACustomBitmap; override;
    function FilterContour: TBGRACustomBitmap; override;
    function FilterPixelate(pixelSize: integer; useResample: boolean; filter: TResampleFilter = rfLinear): TBGRACustomBitmap; override;
    function FilterBlurRadial(radius: integer;
      blurType: TRadialBlurType): TBGRACustomBitmap; override;
    function FilterBlurRadial(ABounds: TRect; radius: integer;
      blurType: TRadialBlurType): TBGRACustomBitmap; override;
    function FilterBlurMotion(distance: integer; angle: single;
      oriented: boolean): TBGRACustomBitmap; override;
    function FilterBlurMotion(ABounds: TRect; distance: integer; angle: single;
      oriented: boolean): TBGRACustomBitmap; override;
    function FilterCustomBlur(mask: TBGRACustomBitmap): TBGRACustomBitmap; override;
    function FilterCustomBlur(ABounds: TRect; mask: TBGRACustomBitmap): TBGRACustomBitmap; override;
    function FilterEmboss(angle: single): TBGRACustomBitmap; override;
    function FilterEmboss(angle: single; ABounds: TRect): TBGRACustomBitmap; override;
    function FilterEmbossHighlight(FillSelection: boolean): TBGRACustomBitmap; override;
    function FilterEmbossHighlight(FillSelection: boolean; BorderColor: TBGRAPixel): TBGRACustomBitmap; override;
    function FilterEmbossHighlight(FillSelection: boolean; BorderColor: TBGRAPixel; var Offset: TPoint): TBGRACustomBitmap; override;
    function FilterGrayscale: TBGRACustomBitmap; override;
    function FilterGrayscale(ABounds: TRect): TBGRACustomBitmap; override;
    function FilterNormalize(eachChannel: boolean = True): TBGRACustomBitmap; override;
    function FilterNormalize(ABounds: TRect; eachChannel: boolean = True): TBGRACustomBitmap; override;
    function FilterRotate(origin: TPointF; angle: single; correctBlur: boolean = false): TBGRACustomBitmap; override;
    function FilterSphere: TBGRACustomBitmap; override;
    function FilterTwirl(ACenter: TPoint; ARadius: Single; ATurn: Single=1; AExponent: Single=3): TBGRACustomBitmap; override;
    function FilterTwirl(ABounds: TRect; ACenter: TPoint; ARadius: Single; ATurn: Single=1; AExponent: Single=3): TBGRACustomBitmap; override;
    function FilterCylinder: TBGRACustomBitmap; override;
    function FilterPlane: TBGRACustomBitmap; override;
  end;

  { TBGRAPtrBitmap }

  TBGRAPtrBitmap = class(TBGRADefaultBitmap)
  protected
    function GetLineOrder: TRawImageLineOrder; override;
    procedure SetLineOrder(AValue: TRawImageLineOrder); override;
    procedure ReallocData; override;
    procedure FreeData; override;
    procedure CannotResize;
    procedure NotImplemented;
    procedure RebuildBitmap; override;

    function CreateDefaultFontRenderer: TBGRACustomFontRenderer; override; //to override
    function LoadFromRawImage({%H-}ARawImage: TRawImage; {%H-}DefaultOpacity: byte;
      {%H-}AlwaysReplaceAlpha: boolean=False; {%H-}RaiseErrorOnInvalidPixelFormat: boolean
      =True): boolean; override; //to override
  public
    constructor Create(AWidth, AHeight: integer; AData: Pointer); overload;
    function Duplicate(DuplicateProperties: Boolean = False): TBGRACustomBitmap; override;
    procedure SetDataPtr(AData: Pointer);
    property LineOrder: TRawImageLineOrder Read GetLineOrder Write SetLineOrder;

    procedure DataDrawTransparent({%H-}ACanvas: TCanvas; {%H-}Rect: TRect; {%H-}AData: Pointer;
      {%H-}ALineOrder: TRawImageLineOrder; {%H-}AWidth, {%H-}AHeight: integer); override; //to override
    procedure DataDrawOpaque({%H-}ACanvas: TCanvas; {%H-}Rect: TRect; {%H-}AData: Pointer;
      {%H-}ALineOrder: TRawImageLineOrder; {%H-}AWidth, {%H-}AHeight: integer); override; //to override
    procedure GetImageFromCanvas({%H-}CanvasSource: TCanvas; {%H-}x, {%H-}y: integer); override; //to override

    procedure Assign({%H-}Source: TPersistent); override;
    procedure TakeScreenshot({%H-}ARect: TRect); override;
    procedure TakeScreenshotOfPrimaryMonitor; override;
    procedure LoadFromDevice({%H-}DC: System.THandle); override;
    procedure LoadFromDevice({%H-}DC: System.THandle; {%H-}ARect: TRect); override;
  end;

var
  DefaultTextStyle: TTextStyle;

procedure BGRAGradientFill(bmp: TBGRACustomBitmap; x, y, x2, y2: integer;
  c1, c2: TBGRAPixel; gtype: TGradientType; o1, o2: TPointF; mode: TDrawMode;
  gammaColorCorrection: boolean = True; Sinus: Boolean=False);

type

  { TBitmapTracker }

  TBitmapTracker = class(TBitmap)
  protected
    FUser: TBGRADefaultBitmap;
    procedure Changed(Sender: TObject); override;
  public
    constructor Create(AUser: TBGRADefaultBitmap); overload;
  end;

implementation

uses Math, BGRAUTF8, BGRABlend, BGRAFilters, BGRAGradientScanner,
  BGRAResample, BGRATransform, BGRAPolygon, BGRAPolygonAliased,
  BGRAPath, FPReadPcx, FPWritePcx, FPReadXPM, FPWriteXPM;

{ TBitmapTracker }

constructor TBitmapTracker.Create(AUser: TBGRADefaultBitmap);
begin
  FUser := AUser;
  inherited Create;
end;

procedure TBitmapTracker.Changed(Sender: TObject);
begin
  if FUser <> nil then
    FUser.FBitmapModified := True;
  inherited Changed(Sender);
end;

{ TBGRADefaultBitmap }

function TBGRADefaultBitmap.CheckEmpty: boolean;
const
  alphaMask = $ff shl TBGRAPixel_AlphaShift;
var
  i: integer;
  p: PBGRAPixel;
begin
  p := Data;
  for i := (NbPixels shr 1) - 1 downto 0 do
  begin
    if PInt64(p)^ and (alphaMask or (alphaMask shl 32)) <> 0 then
    begin
      Result := False;
      exit;
    end;
    Inc(p,2);
  end;
  if Odd(NbPixels) and (p^.alpha <> 0) then
  begin
    Result := false;
    exit;
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

procedure TBGRADefaultBitmap.DoLoadFromBitmap;
begin
  //nothing
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

function TBGRADefaultBitmap.GetLineCap: TPenEndCap;
begin
  result := FLineCap;
end;

procedure TBGRADefaultBitmap.SetLineCap(AValue: TPenEndCap);
begin
  if AValue <> FLineCap then
  begin
    FLineCap:= AValue;
    if Assigned(FArrow) then FArrow.LineCap := AValue;
  end;
end;

function TBGRADefaultBitmap.GetArrowEndSize: TPointF;
begin
  result := GetArrow.EndSize;
end;

function TBGRADefaultBitmap.GetArrowStartSize: TPointF;
begin
  result := GetArrow.StartSize;
end;

procedure TBGRADefaultBitmap.SetArrowEndSize(AValue: TPointF);
begin
  GetArrow.EndSize := AValue;
end;

procedure TBGRADefaultBitmap.SetArrowStartSize(AValue: TPointF);
begin
  GetArrow.StartSize := AValue;
end;

function TBGRADefaultBitmap.GetArrowEndOffset: single;
begin
  result := GetArrow.EndOffsetX;
end;

function TBGRADefaultBitmap.GetArrowStartOffset: single;
begin
  result := GetArrow.StartOffsetX;
end;

procedure TBGRADefaultBitmap.SetArrowEndOffset(AValue: single);
begin
  GetArrow.EndOffsetX := AValue;
end;

procedure TBGRADefaultBitmap.SetArrowStartOffset(AValue: single);
begin
  GetArrow.StartOffsetX := AValue;
end;

function TBGRADefaultBitmap.GetArrowEndRepeat: integer;
begin
  result := GetArrow.EndRepeatCount;
end;

function TBGRADefaultBitmap.GetArrowStartRepeat: integer;
begin
  result := GetArrow.StartRepeatCount;
end;

procedure TBGRADefaultBitmap.SetArrowEndRepeat(AValue: integer);
begin
  GetArrow.EndRepeatCount := AValue;
end;

procedure TBGRADefaultBitmap.SetArrowStartRepeat(AValue: integer);
begin
  GetArrow.StartRepeatCount := AValue;
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
begin
  result := FontRenderer.GetFontPixelMetric;
end;

function TBGRADefaultBitmap.GetFontRenderer: TBGRACustomFontRenderer;
begin
  if FFontRenderer = nil then FFontRenderer := CreateDefaultFontRenderer;
  if FFontRenderer = nil then raise exception.Create('No font renderer');
  result := FFontRenderer;
  result.FontName := FontName;
  result.FontStyle := FontStyle;
  result.FontQuality := FontQuality;
  result.FontOrientation := FontOrientation;
  result.FontEmHeight := FFontHeight;
end;

procedure TBGRADefaultBitmap.SetFontRenderer(AValue: TBGRACustomFontRenderer);
begin
  if AValue = FFontRenderer then exit;
  FFontRenderer.Free;
  FFontRenderer := AValue
end;

function TBGRADefaultBitmap.GetFontAnchorVerticalOffset: single;
begin
  case FontVerticalAnchor of
  fvaTop: result := 0;
  fvaCenter: result := FontFullHeight*0.5;
  fvaCapLine: result := FontPixelMetric.CapLine;
  fvaCapCenter: result := (FontPixelMetric.CapLine+FontPixelMetric.Baseline)*0.5;
  fvaXLine: result := FontPixelMetric.xLine;
  fvaXCenter: result := (FontPixelMetric.xLine+FontPixelMetric.Baseline)*0.5;
  fvaBaseline: result := FontPixelMetric.Baseline;
  fvaDescentLine: result := FontPixelMetric.DescentLine;
  fvaBottom: result := FontFullHeight;
  else
    result := 0;
  end;
end;

function TBGRADefaultBitmap.GetFontAnchorRotatedOffset: TPointF;
begin
  result := GetFontAnchorRotatedOffset(FontOrientation);
end;

function TBGRADefaultBitmap.GetFontAnchorRotatedOffset(
  ACustomOrientation: integer): TPointF;
begin
  result := PointF(0, GetFontAnchorVerticalOffset);
  if ACustomOrientation <> 0 then
    result := AffineMatrixRotationDeg(-ACustomOrientation*0.1)*result;
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

{ Creates a new bitmap with dimensions AWidth and AHeight and filled with
  transparent pixels. Internally, it uses the same type so that if you
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

{ Can only be called from an existing instance of TBGRABitmap.
  Creates a new instance with dimensions AWidth and AHeight,
  and fills it with Color. }
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

{ Creates a new bitmap and loads it contents from a file.
  The encoding of the string is the default one for the operating system.
  It is recommended to use the next function and UTF8 encoding }
function TBGRADefaultBitmap.NewBitmap(Filename: string): TBGRACustomBitmap;
var
  BGRAClass: TBGRABitmapAny;
begin
  BGRAClass := TBGRABitmapAny(self.ClassType);
  Result    := BGRAClass.Create(Filename);
end;

{ Creates a new bitmap and loads it contents from a file.
  It is recommended to use UTF8 encoding }
function TBGRADefaultBitmap.NewBitmap(Filename: string; AIsUtf8: boolean): TBGRACustomBitmap;
var
  BGRAClass: TBGRABitmapAny;
begin
  BGRAClass := TBGRABitmapAny(self.ClassType);
  Result    := BGRAClass.Create(Filename,AIsUtf8);
end;

function TBGRADefaultBitmap.NewBitmap(AFPImage: TFPCustomImage): TBGRACustomBitmap;
var
  BGRAClass: TBGRABitmapAny;
begin
  BGRAClass := TBGRABitmapAny(self.ClassType);
  Result    := BGRAClass.Create(AFPImage);
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
  FScanWidth := FWidth;
  FScanHeight:= FHeight;
  FNbPixels := AWidth * AHeight;
  if FNbPixels < 0 then // 2 Go limit
    raise EOutOfMemory.Create('Image too big');
  FreeBitmap;
  ReallocData;
  NoClip;
end;

{---------------------- Constructors ---------------------------------}

{ Creates an image of width and height equal to zero. }
constructor TBGRADefaultBitmap.Create;
begin
  Init;
  inherited Create(0, 0);
end;

constructor TBGRADefaultBitmap.Create(AFPImage: TFPCustomImage);
begin
  Init;
  inherited Create(AFPImage.Width, AFPImage.Height);
  Assign(AFPImage);
end;

{ Creates an image of dimensions AWidth and AHeight and filled with transparent pixels. }
constructor TBGRADefaultBitmap.Create(ABitmap: TBitmap);
begin
  Init;
  inherited Create(ABitmap.Width, ABitmap.Height);
  Assign(ABitmap);
end;

{ Creates an image of dimensions AWidth and AHeight and fills it with the opaque color Color. }
constructor TBGRADefaultBitmap.Create(AWidth, AHeight: integer; Color: TColor);
begin
  Init;
  inherited Create(AWidth, AHeight);
  Fill(Color);
end;

{ Creates an image of dimensions AWidth and AHeight and fills it with Color. }
constructor TBGRADefaultBitmap.Create(AWidth, AHeight: integer; Color: TBGRAPixel);
begin
  Init;
  inherited Create(AWidth, AHeight);
  Fill(Color);
end;

{ Creates an image by loading its content from the file AFilename.
  The encoding of the string is the default one for the operating system.
  It is recommended to use the next constructor and UTF8 encoding. }
constructor TBGRADefaultBitmap.Create(AFilename: string);
begin
  Init;
  inherited Create(0, 0);
  LoadFromFile(Afilename);
end;

{ Free the object and all its resources }
destructor TBGRADefaultBitmap.Destroy;
begin
  FFontRenderer.Free;
  FCanvasFP.Free;
  FCanvasBGRA.Free;
  FCanvas2D.Free;
  FArrow.Free;
  FreeData;
  FreeBitmap;
  inherited Destroy;
end;

{------------------------- Loading functions ----------------------------------}

{ Creates an image by loading its content from the file AFilename.
  The boolean AIsUtf8Filename specifies if UTF8 encoding is assumed for the filename. }
constructor TBGRADefaultBitmap.Create(AFilename: string; AIsUtf8: boolean);
begin
  Init;
  inherited Create(0, 0);
  if AIsUtf8 then
    LoadFromFileUTF8(Afilename)
  else
    LoadFromFile(Afilename);
end;

{ Creates an image by loading its content from the stream AStream. }
constructor TBGRADefaultBitmap.Create(AStream: TStream);
begin
  Init;
  inherited Create(0, 0);  
  LoadFromStream(AStream);
end;

procedure TBGRADefaultBitmap.Serialize(AStream: TStream);
var lWidth,lHeight,y: integer;
begin
  lWidth := NtoLE(Width);
  lHeight := NtoLE(Height);
  AStream.Write(lWidth,sizeof(lWidth));
  AStream.Write(lHeight,sizeof(lHeight));
  If TBGRAPixel_RGBAOrder then SwapRedBlue;
  for y := 0 to Height-1 do
    AStream.Write(ScanLine[y]^, Width*sizeof(TBGRAPixel));
  If TBGRAPixel_RGBAOrder then SwapRedBlue;
end;

procedure TBGRADefaultBitmap.Deserialize(AStream: TStream);
var lWidth,lHeight,y: integer;
begin
  AStream.Read({%H-}lWidth,sizeof(lWidth));
  AStream.Read({%H-}lHeight,sizeof(lHeight));
  lWidth := LEtoN(lWidth);
  lHeight := LEtoN(lHeight);
  SetSize(lWidth,lHeight);
  for y := 0 to Height-1 do
    AStream.Read(ScanLine[y]^, Width*sizeof(TBGRAPixel));
  If TBGRAPixel_RGBAOrder then SwapRedBlue;
end;

class procedure TBGRADefaultBitmap.SerializeEmpty(AStream: TStream);
var zero: integer;
begin
  zero := 0;
  AStream.Write(zero,sizeof(zero));
  AStream.Write(zero,sizeof(zero));
end;

procedure TBGRADefaultBitmap.Assign(Source: TPersistent);
var pdest: PBGRAPixel;
  x,y: NativeInt;
begin
  if Source is TBGRACustomBitmap then
  begin
    DiscardBitmapChange;
    SetSize(TBGRACustomBitmap(Source).Width, TBGRACustomBitmap(Source).Height);
    PutImage(0, 0, TBGRACustomBitmap(Source), dmSet);
  end else
  if Source is TFPCustomImage then
  begin
    DiscardBitmapChange;
    SetSize(TFPCustomImage(Source).Width, TFPCustomImage(Source).Height);
    for y := 0 to TFPCustomImage(Source).Height-1 do
    begin
      pdest := ScanLine[y];
      for x := 0 to TFPCustomImage(Source).Width-1 do
      begin
        pdest^ := FPColorToBGRA(TFPCustomImage(Source).Colors[x,y]);
        inc(pdest);
      end;
    end;
  end else
    inherited Assign(Source);
end;

{------------------------- Clipping -------------------------------}

{ Check if a point is in the clipping rectangle }
function TBGRADefaultBitmap.PtInClipRect(x, y: int32or64): boolean;
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

function TBGRADefaultBitmap.InternalGetPixelCycle256(ix, iy: int32or64; iFactX,
  iFactY: int32or64): TBGRAPixel;
var
  ixMod1,ixMod2: int32or64;
  w1,w2,w3,w4,alphaW: UInt32or64;
  bSum, gSum, rSum: UInt32or64;
  aSum: UInt32or64;

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

  scan := GetScanlineFast(iy);

  ixMod1 := ix;
  c      := (scan + ix)^;
  alphaW := c.alpha * w1;
  aSum   += alphaW;

  rSum   += c.red * alphaW;
  gSum   += c.green * alphaW;
  bSum   += c.blue * alphaW;

  ixMod2 := ix+1;
  if ixMod2=Width then ixMod2 := 0;
  c      := (scan + ixMod2)^;
  alphaW := c.alpha * w2;
  aSum   += alphaW;

  rSum   += c.red * alphaW;
  gSum   += c.green * alphaW;
  bSum   += c.blue * alphaW;

  Inc(iy);
  if iy = Height then iy := 0;
  scan := GetScanlineFast(iy);

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

function TBGRADefaultBitmap.InternalGetPixel256(ix, iy: int32or64; iFactX,
  iFactY: int32or64; smoothBorder: boolean): TBGRAPixel;
var
  w1,w2,w3,w4,alphaW: cardinal;
  rSum, gSum, bSum: cardinal; //rgbDiv = aSum
  aSum, aDiv: cardinal;
  c:    TBGRAPixel;
  scan: PBGRAPixel;
begin
  rSum   := 0;
  gSum   := 0;
  bSum   := 0;
  aSum   := 0;
  aDiv   := 0;

  w4 := (iFactX*iFactY+127) shr 8;
  w3 := iFactY-w4;
  {$PUSH}{$HINTS OFF}
  w1 := (256-iFactX)-w3;
  {$POP}
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
      aDiv   += w1;
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
      aDiv   += w2;
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
      aDiv   += w4;
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
      aDiv   += w3;
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
    if smoothBorder or (aDiv = 256) then
      Result.alpha := (aSum + 128) shr 8
    else
      Result.alpha := (aSum + aDiv shr 1) div aDiv;
  end;
end;

function TBGRADefaultBitmap.GetPolyLineOption: TBGRAPolyLineOptions;
begin
  result := [];
  if Assigned(FArrow) and FArrow.IsStartDefined then result += [plNoStartCap];
  if Assigned(FArrow) and FArrow.IsEndDefined then result += [plNoEndCap];
end;

function TBGRADefaultBitmap.GetArrow: TBGRAArrow;
begin
  if FArrow = nil then
  begin
    FArrow := TBGRAArrow.Create;
    FArrow.LineCap := LineCap;
  end;
  result := FArrow;
end;

{-------------------------- Pixel functions -----------------------------------}

procedure TBGRADefaultBitmap.SetPixel(x, y: int32or64; c: TBGRAPixel);
begin
  if not PtInClipRect(x,y) then exit;
  LoadFromBitmapIfNeeded;
  (GetScanlineFast(y) +x)^ := c;
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.XorPixel(x, y: int32or64; c: TBGRAPixel);
var
  p : PDWord;
begin
  if not PtInClipRect(x,y) then exit;
  LoadFromBitmapIfNeeded;
  p := PDWord(GetScanlineFast(y) +x);
  p^ := p^ xor DWord(c);
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.SetPixel(x, y: int32or64; c: TColor);
var
  p: PBGRAPixel;
begin
  if not PtInClipRect(x,y) then exit;
  LoadFromBitmapIfNeeded;
  p  := GetScanlineFast(y) + x;
  RedGreenBlue(c, p^.red,p^.green,p^.blue);
  p^.alpha := 255;
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.DrawPixel(x, y: int32or64; c: TBGRAPixel);
begin
  if not PtInClipRect(x,y) then exit;
  LoadFromBitmapIfNeeded;
  DrawPixelInlineWithAlphaCheck(GetScanlineFast(y) + x, c);
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.DrawPixel(x, y: int32or64; ec: TExpandedPixel);
begin
  if not PtInClipRect(x,y) then exit;
  LoadFromBitmapIfNeeded;
  DrawExpandedPixelInlineWithAlphaCheck(GetScanlineFast(y) + x, ec);
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.FastBlendPixel(x, y: int32or64; c: TBGRAPixel);
begin
  if not PtInClipRect(x,y) then exit;
  LoadFromBitmapIfNeeded;
  FastBlendPixelInline(GetScanlineFast(y) + x, c);
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.ErasePixel(x, y: int32or64; alpha: byte);
begin
  if not PtInClipRect(x,y) then exit;
  LoadFromBitmapIfNeeded;
  ErasePixelInline(GetScanlineFast(y) + x, alpha);
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.AlphaPixel(x, y: int32or64; alpha: byte);
begin
  if not PtInClipRect(x,y) then exit;
  LoadFromBitmapIfNeeded;
  if alpha = 0 then
    (GetScanlineFast(y) +x)^ := BGRAPixelTransparent
  else
    (GetScanlineFast(y) +x)^.alpha := alpha;
  InvalidateBitmap;
end;

function TBGRADefaultBitmap.GetPixel(x, y: int32or64): TBGRAPixel;
begin
  if (x < 0) or (x >= Width) or (y < 0) or (y >= Height) then //it is possible to read pixels outside of the cliprect
    Result := BGRAPixelTransparent
  else
  begin
    LoadFromBitmapIfNeeded;
    Result := (GetScanlineFast(y) + x)^;
  end;
end;

function TBGRADefaultBitmap.GetPixel256(x, y, fracX256, fracY256: int32or64;
  AResampleFilter: TResampleFilter; smoothBorder: boolean = true): TBGRAPixel;
begin
  if (fracX256 = 0) and (fracY256 = 0) then
    result := GetPixel(x,y)
  else if AResampleFilter = rfBox then
  begin
    if fracX256 >= 128 then inc(x);
    if fracY256 >= 128 then inc(y);
    result := GetPixel(x,y);
  end else
  begin
    LoadFromBitmapIfNeeded;
    result := InternalGetPixel256(x,y,FineInterpolation256(fracX256,AResampleFilter),FineInterpolation256(fracY256,AResampleFilter),smoothBorder);
  end;
end;

{$hints off}
{ This function compute an interpolated pixel at floating point coordinates }
function TBGRADefaultBitmap.GetPixel(x, y: single; AResampleFilter: TResampleFilter = rfLinear; smoothBorder: boolean = true): TBGRAPixel;
var
  ix, iy: Int32or64;
  iFactX,iFactY: Int32or64;
begin
  ix := round(x*256);
  if (ix<= -256) or (ix>=Width shl 8) then
  begin
    result := BGRAPixelTransparent;
    exit;
  end;
  iy := round(y*256);
  if (iy<= -256) or (iy>=Height shl 8) then
  begin
    result := BGRAPixelTransparent;
    exit;
  end;

  iFactX := ix and 255; //distance from integer coordinate
  iFactY := iy and 255;
  if ix<0 then ix := -1 else ix := ix shr 8;
  if iy<0 then iy := -1 else iy := iy shr 8;

  //if the coordinate is integer, then call standard GetPixel function
  if (iFactX = 0) and (iFactY = 0) then
  begin
    Result := (GetScanlineFast(iy)+ix)^;
    exit;
  end;

  LoadFromBitmapIfNeeded;
  result := InternalGetPixel256(ix,iy,FineInterpolation256(iFactX,AResampleFilter),FineInterpolation256(iFactY,AResampleFilter),smoothBorder);
end;

{ Same as GetPixel(single,single,TResampleFilter) but with coordinate cycle, supposing the image repeats itself in both directions }
function TBGRADefaultBitmap.GetPixelCycle(x, y: single; AResampleFilter: TResampleFilter = rfLinear): TBGRAPixel;
var
  ix, iy: Int32or64;
  iFactX,iFactY: Int32or64;
begin
  if FData = nil then
  begin
    result := BGRAPixelTransparent;
    exit;
  end;
  LoadFromBitmapIfNeeded;
  ix := round(x*256);
  iy := round(y*256);
  iFactX := ix and 255;
  iFactY := iy and 255;
  ix := PositiveMod(ix, FWidth shl 8) shr 8;
  iy := PositiveMod(iy, FHeight shl 8) shr 8;
  if (iFactX = 0) and (iFactY = 0) then
  begin
    result := (GetScanlineFast(iy)+ix)^;
    exit;
  end;
  if ScanInterpolationFilter <> rfLinear then
  begin
    iFactX := FineInterpolation256( iFactX, ScanInterpolationFilter );
    iFactY := FineInterpolation256( iFactY, ScanInterpolationFilter );
  end;
  result := InternalGetPixelCycle256(ix,iy, iFactX,iFactY);
end;

function TBGRADefaultBitmap.GetPixelCycle(x, y: single;
  AResampleFilter: TResampleFilter; repeatX: boolean; repeatY: boolean
  ): TBGRAPixel;
var
  ix, iy: Int32or64;
  iFactX,iFactY: Int32or64;
begin
  if FData = nil then
  begin
    result := BGRAPixelTransparent;
    exit;
  end;
  ix := round(x*256);
  iy := round(y*256);
  iFactX := ix and 255;
  iFactY := iy and 255;
  if ix < 0 then ix := -((iFactX-ix) shr 8)
  else ix := ix shr 8;
  if iy < 0 then iy := -((iFactY-iy) shr 8)
  else iy := iy shr 8;
  result := GetPixelCycle256(ix,iy,iFactX,iFactY,AResampleFilter,repeatX,repeatY);
end;

function TBGRADefaultBitmap.GetPixelCycle256(x, y, fracX256,
  fracY256: int32or64; AResampleFilter: TResampleFilter): TBGRAPixel;
begin
  if (fracX256 = 0) and (fracY256 = 0) then
    result := GetPixelCycle(x,y)
  else if AResampleFilter = rfBox then
  begin
    if fracX256 >= 128 then inc(x);
    if fracY256 >= 128 then inc(y);
    result := GetPixelCycle(x,y);
  end else
  begin
    LoadFromBitmapIfNeeded;
    result := InternalGetPixelCycle256(PositiveMod(x,FWidth),PositiveMod(y,FHeight),FineInterpolation256(fracX256,AResampleFilter),FineInterpolation256(fracY256,AResampleFilter));
  end;
end;

function TBGRADefaultBitmap.GetPixelCycle256(x, y, fracX256,
  fracY256: int32or64; AResampleFilter: TResampleFilter; repeatX: boolean;
  repeatY: boolean): TBGRAPixel;
begin
  if not repeatX and not repeatY then
    result := GetPixel256(x,y,fracX256,fracY256,AResampleFilter)
  else if repeatX and repeatY then
    result := GetPixelCycle256(x,y,fracX256,fracY256,AResampleFilter)
  else
  begin
    if not repeatX then
    begin
      if x < 0 then
      begin
        if x < -1 then
        begin
          result := BGRAPixelTransparent;
          exit;
        end;
        result := GetPixelCycle256(0,y,0,fracY256,AResampleFilter);
        result.alpha:= result.alpha*fracX256 shr 8;
        if result.alpha = 0 then
          result := BGRAPixelTransparent;
        exit;
      end;
      if x >= FWidth-1 then
      begin
        if x >= FWidth then
        begin
          result := BGRAPixelTransparent;
          exit;
        end;
        result := GetPixelCycle256(FWidth-1,y,0,fracY256,AResampleFilter);
        result.alpha:= result.alpha*(256-fracX256) shr 8;
        if result.alpha = 0 then
          result := BGRAPixelTransparent;
        exit;
      end;
    end else
    begin
      if y < 0 then
      begin
        if y < -1 then
        begin
          result := BGRAPixelTransparent;
          exit;
        end;
        result := GetPixelCycle256(x,0,fracX256,0,AResampleFilter);
        result.alpha:= result.alpha*fracY256 shr 8;
        if result.alpha = 0 then
          result := BGRAPixelTransparent;
        exit;
      end;
      if y >= FHeight-1 then
      begin
        if y >= FHeight then
        begin
          result := BGRAPixelTransparent;
          exit;
        end;
        result := GetPixelCycle256(x,FHeight-1,fracX256,0,AResampleFilter);
        result.alpha:= result.alpha*(256-fracY256) shr 8;
        if result.alpha = 0 then
          result := BGRAPixelTransparent;
        exit;
      end;
    end;
    result := GetPixelCycle256(x,y,fracX256,fracY256,AResampleFilter);
  end;
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

procedure TBGRADefaultBitmap.LoadFromBitmapIfNeeded;
begin
  if FBitmapModified then
  begin
    DoLoadFromBitmap;
    DiscardBitmapChange;
  end;
  if FAlphaCorrectionNeeded then
  begin
    DoAlphaCorrection;
  end;
end;

procedure TBGRADefaultBitmap.CrossFade(ARect: TRect; Source1, Source2: IBGRAScanner; AFadePosition: byte; mode: TDrawMode = dmDrawWithTransparency);
var constScanner: TBGRAConstantScanner;
begin
  if AFadePosition = 0 then
    FillRect(ARect, Source1, mode) else
  if AFadePosition = 255 then
    FillRect(ARect, Source2, mode) else
  begin
    constScanner := TBGRAConstantScanner.Create(BGRA(AFadePosition,AFadePosition,AFadePosition,255));
    CrossFade(ARect, Source1,Source2, constScanner, mode);
    constScanner.Free;
  end;
end;

procedure TBGRADefaultBitmap.CrossFade(ARect: TRect; Source1, Source2: IBGRAScanner; AFadeMask: IBGRAScanner; mode: TDrawMode = dmDrawWithTransparency);
var xb,yb: NativeInt;
  pdest: PBGRAPixel;
  c: TBGRAPixel;
  fadePos: byte;
begin
  if not IntersectRect(ARect,ARect,ClipRect) then exit;
  for yb := ARect.top to ARect.Bottom-1 do
  begin
    pdest := GetScanlineFast(yb)+ARect.Left;
    Source1.ScanMoveTo(ARect.left, yb);
    Source2.ScanMoveTo(ARect.left, yb);
    AFadeMask.ScanMoveTo(ARect.left, yb);
    for xb := ARect.left to ARect.Right-1 do
    begin
      fadePos := AFadeMask.ScanNextPixel.green;
      c := MergeBGRAWithGammaCorrection(Source1.ScanNextPixel,not fadePos,Source2.ScanNextPixel,fadePos);
      case mode of
      dmSet: pdest^ := c;
      dmDrawWithTransparency: DrawPixelInlineWithAlphaCheck(pdest, c);
      dmLinearBlend: FastBlendPixelInline(pdest,c);
      dmSetExceptTransparent: if c.alpha = 255 then pdest^ := c;
      end;
      inc(pdest);
    end;
  end;
  InvalidateBitmap;
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
  FScanWidth := FWidth;
  FScanHeight:= FHeight;
  FLineOrder := riloTopToBottom;
  FCanvasOpacity := 255;
  FAlphaCorrectionNeeded := False;
  FEraseMode := False;
  FillMode := fmWinding;

  FontName  := 'Arial';
  FontStyle := [];
  FontAntialias := False;
  FontVerticalAnchor:= fvaTop;
  FFontHeight := 20;

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
  if (self = nil) or (Width = 0) or (Height = 0) then exit;
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
  if (self = nil) or (Width = 0) or (Height = 0) then exit;
  if Opaque then
    DataDrawOpaque(ACanvas, Rect, Data, FLineOrder, FWidth, FHeight)
  else
  begin
    LoadFromBitmapIfNeeded;
    ACanvas.StretchDraw(Rect, Bitmap);
  end;
end;

{---------------------------- Line primitives ---------------------------------}

function TBGRADefaultBitmap.CheckHorizLineBounds(var x,y,x2: int32or64): boolean; inline;
var
  temp: int32or64;
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

procedure TBGRADefaultBitmap.SetHorizLine(x, y, x2: int32or64; c: TBGRAPixel);
begin
  if not CheckHorizLineBounds(x,y,x2) then exit;
  FillInline(scanline[y] + x, c, x2 - x + 1);
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.XorHorizLine(x, y, x2: int32or64; c: TBGRAPixel);
begin
  if not CheckHorizLineBounds(x,y,x2) then exit;
  XorInline(scanline[y] + x, c, x2 - x + 1);
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.DrawHorizLine(x, y, x2: int32or64; c: TBGRAPixel);
begin
  if not CheckHorizLineBounds(x,y,x2) then exit;
  DrawPixelsInline(scanline[y] + x, c, x2 - x + 1);
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.DrawHorizLine(x, y, x2: int32or64; ec: TExpandedPixel
  );
begin
  if not CheckHorizLineBounds(x,y,x2) then exit;
  DrawExpandedPixelsInline(scanline[y] + x, ec, x2 - x + 1);
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.HorizLine(x, y, x2: int32or64;
  texture: IBGRAScanner; ADrawMode : TDrawMode);
begin
  if not CheckHorizLineBounds(x,y,x2) then exit;
  texture.ScanMoveTo(x,y);
  ScannerPutPixels(texture,scanline[y] + x, x2 - x + 1,ADrawMode);
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.FastBlendHorizLine(x, y, x2: int32or64; c: TBGRAPixel);
begin
  if not CheckHorizLineBounds(x,y,x2) then exit;
  FastBlendPixelsInline(scanline[y] + x, c, x2 - x + 1);
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.AlphaHorizLine(x, y, x2: int32or64; alpha: byte);
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

function TBGRADefaultBitmap.CheckVertLineBounds(var x,y,y2: int32or64; out delta: int32or64): boolean; inline;
var
  temp: int32or64;
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

procedure TBGRADefaultBitmap.SetVertLine(x, y, y2: int32or64; c: TBGRAPixel);
var
  n, delta: int32or64;
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

procedure TBGRADefaultBitmap.XorVertLine(x, y, y2: int32or64; c: TBGRAPixel);
var
  n, delta: int32or64;
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

procedure TBGRADefaultBitmap.DrawVertLine(x, y, y2: int32or64; c: TBGRAPixel);
var
  n, delta: int32or64;
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

procedure TBGRADefaultBitmap.AlphaVertLine(x, y, y2: int32or64; alpha: byte);
var
  n, delta: int32or64;
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

procedure TBGRADefaultBitmap.FastBlendVertLine(x, y, y2: int32or64; c: TBGRAPixel);
var
  n, delta: int32or64;
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

procedure TBGRADefaultBitmap.DrawHorizLineDiff(x, y, x2: int32or64;
  c, compare: TBGRAPixel; maxDiff: byte);
begin
  if not CheckHorizLineBounds(x,y,x2) then exit;
  DrawPixelsInlineDiff(scanline[y] + x, c, x2 - x + 1, compare, maxDiff);
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.SetArrowStart(AStyle: TBGRAArrowStyle;
  ATipStyle: TPenJoinStyle; ARelativePenWidth: single; ATriangleBackOffset: single);
begin
  GetArrow.SetStart(AStyle,ATipStyle,ARelativePenWidth,ATriangleBackOffset);
end;

procedure TBGRADefaultBitmap.SetArrowEnd(AStyle: TBGRAArrowStyle;
  ATipStyle: TPenJoinStyle; ARelativePenWidth: single; ATriangleBackOffset: single);
begin
  GetArrow.SetEnd(AStyle,ATipStyle,ARelativePenWidth,ATriangleBackOffset);
end;

procedure TBGRADefaultBitmap.InternalTextOutCurved(
  ACursor: TBGRACustomPathCursor; sUTF8: string; AColor: TBGRAPixel;
  ATexture: IBGRAScanner; AAlign: TAlignment; ALetterSpacing: single);
var
  pstr: pchar;
  left,charlen: integer;
  nextchar: string;
  charwidth, angle, textlen: single;
begin
  if (ATexture = nil) and (AColor.alpha = 0) then exit;
  sUTF8 := CleanTextOutString(sUTF8);
  if sUTF8 = '' then exit;
  pstr := @sUTF8[1];
  left := length(sUTF8);
  if AALign<> taLeftJustify then
  begin
    textlen := TextSize(sUTF8).cx + (UTF8Length(sUTF8)-1)*ALetterSpacing;
    case AAlign of
      taCenter: ACursor.MoveBackward(textlen*0.5);
      taRightJustify: ACursor.MoveBackward(textlen);
    end;
  end;
  while left > 0 do
  begin
    charlen := UTF8CharacterLength(pstr);
    setlength(nextchar, charlen);
    move(pstr^, nextchar[1], charlen);
    inc(pstr,charlen);
    dec(left,charlen);
    charwidth := TextSize(nextchar).cx;
    ACursor.MoveForward(charwidth);
    ACursor.MoveBackward(charwidth, false);
    ACursor.MoveForward(charwidth*0.5);
    with ACursor.CurrentTangent do angle := arctan2(y,x);
    with ACursor.CurrentCoordinate do
    begin
      if ATexture = nil then
        TextOutAngle(x,y, round(-angle*1800/Pi), nextchar, AColor, taCenter)
      else
        TextOutAngle(x,y, round(-angle*1800/Pi), nextchar, ATexture, taCenter);
    end;
    ACursor.MoveForward(charwidth*0.5 + ALetterSpacing);
  end;
end;

procedure TBGRADefaultBitmap.InternalArc(cx, cy, rx, ry: single; StartAngleRad,
  EndAngleRad: Single; ABorderColor: TBGRAPixel; w: single; AFillColor: TBGRAPixel; AOptions: TArcOptions;
  ADrawChord: boolean; ATexture: IBGRAScanner);
var
  pts, ptsFill: array of TPointF;
  temp: single;
  multi: TBGRAMultishapeFiller;
begin
  if (rx = 0) or (ry = 0) then exit;
  if ADrawChord then AOptions := AOptions+[aoClosePath];
  if not (aoFillPath in AOptions) then
    AFillColor := BGRAPixelTransparent;

  if (ABorderColor.alpha = 0) and (AFillColor.alpha = 0) then exit;

  if abs(StartAngleRad-EndAngleRad) >= 2*PI - 1e-6 then
  begin
    if aoPie in AOptions then
      EndAngleRad:= StartAngleRad+2*PI
    else
      EllipseAntialias(cx,cy,rx,ry,ABorderColor,w,AFillColor);
    exit;
  end;

  if EndAngleRad < StartAngleRad then
  begin
    temp := StartAngleRad;
    StartAngleRad:= EndAngleRad;
    EndAngleRad:= temp;
  end;

  pts := ComputeArcRad(cx,cy,rx,ry,StartAngleRad,EndAngleRad);
  if aoPie in AOptions then pts := ConcatPointsF([PointsF([PointF(cx,cy)]),pts]);

  multi := TBGRAMultishapeFiller.Create;
  multi.PolygonOrder := poLastOnTop;
  if AFillColor.alpha <> 0 then
  begin
    if not (aoPie in AOptions) and (length(pts)>=2) then ptsFill := ConcatPointsF([PointsF([(pts[0]+pts[high(pts)])*0.5]),pts])
     else ptsFill := pts;
    if ATexture <> nil then
      multi.AddPolygon(ptsFill, ATexture)
    else
      multi.AddPolygon(ptsFill, AFillColor);
  end;
  if ABorderColor.alpha <> 0 then
  begin
    if [aoPie,aoClosePath]*AOptions <> [] then
      multi.AddPolygon(ComputeWidePolygon(pts,w), ABorderColor)
    else
      multi.AddPolygon(ComputeWidePolyline(pts,w), ABorderColor);
  end;
  multi.Antialiasing := true;
  multi.Draw(self);
  multi.Free;
end;

procedure TBGRADefaultBitmap.DrawPath(APath: IBGRAPath; c: TBGRAPixel; w: single);
var tempPath: TBGRAPath;
begin
  tempPath := TBGRAPath.Create(APath);
  tempPath.stroke(self, c, w);
  tempPath.Free;
end;

procedure TBGRADefaultBitmap.DrawPath(APath: IBGRAPath; texture: IBGRAScanner; w: single);
var tempPath: TBGRAPath;
begin
  tempPath := TBGRAPath.Create(APath);
  tempPath.stroke(self, texture, w);
  tempPath.Free;
end;

{---------------------------- Lines ---------------------------------}
{ Call appropriate functions }

procedure TBGRADefaultBitmap.DrawLine(x1, y1, x2, y2: integer;
  c: TBGRAPixel; DrawLastPixel: boolean; ADrawMode: TDrawMode);
begin
  BGRADrawLineAliased(self,x1,y1,x2,y2,c,DrawLastPixel,ADrawMode);
end;

procedure TBGRADefaultBitmap.DrawLineAntialias(x1, y1, x2, y2: integer;
  c: TBGRAPixel; DrawLastPixel: boolean);
begin
  BGRADrawLineAntialias(self,x1,y1,x2,y2,c,DrawLastPixel,LinearAntialiasing);
end;

procedure TBGRADefaultBitmap.DrawLineAntialias(x1, y1, x2, y2: integer;
  c1, c2: TBGRAPixel; dashLen: integer; DrawLastPixel: boolean);
var DashPos: integer;
begin
  DashPos := 0;
  BGRADrawLineAntialias(self,x1,y1,x2,y2,c1,c2,dashLen,DrawLastPixel,DashPos,LinearAntialiasing);
end;

procedure TBGRADefaultBitmap.DrawLineAntialias(x1, y1, x2, y2: integer; c1,
  c2: TBGRAPixel; dashLen: integer; DrawLastPixel: boolean; var DashPos: integer);
begin
  BGRADrawLineAntialias(self,x1,y1,x2,y2,c1,c2,dashLen,DrawLastPixel,DashPos,LinearAntialiasing);
end;

procedure TBGRADefaultBitmap.DrawLineAntialias(x1, y1, x2, y2: single;
  c: TBGRAPixel; w: single);
begin
  if Assigned(FArrow) then
    BGRAPen.BGRAPolyLine(self,[PointF(x1,y1),PointF(x2,y2)],w,c,LineCap,JoinStyle,FCustomPenStyle,GetPolyLineOption,nil,JoinMiterLimit,@FArrow.ComputeStartAt,FArrow.StartOffsetX,@FArrow.ComputeEndAt,FArrow.EndOffsetX)
  else
    BGRAPen.BGRAPolyLine(self,[PointF(x1,y1),PointF(x2,y2)],w,c,LineCap,JoinStyle,FCustomPenStyle,GetPolyLineOption,nil,JoinMiterLimit);
end;

procedure TBGRADefaultBitmap.DrawLineAntialias(x1, y1, x2, y2: single;
  texture: IBGRAScanner; w: single);
begin
  if Assigned(FArrow) then
    BGRAPen.BGRAPolyLine(self,[PointF(x1,y1),PointF(x2,y2)],w,BGRAPixelTransparent,LineCap,JoinStyle,FCustomPenStyle,GetPolyLineOption,texture,JoinMiterLimit,@FArrow.ComputeStartAt,FArrow.StartOffsetX,@FArrow.ComputeEndAt,FArrow.EndOffsetX)
  else
    BGRAPen.BGRAPolyLine(self,[PointF(x1,y1),PointF(x2,y2)],w,BGRAPixelTransparent,LineCap,JoinStyle,FCustomPenStyle,GetPolyLineOption,texture,JoinMiterLimit);
end;

procedure TBGRADefaultBitmap.DrawLineAntialias(x1, y1, x2, y2: single;
  c: TBGRAPixel; w: single; Closed: boolean);
var
  options: TBGRAPolyLineOptions;
begin
  if not closed then options := [plRoundCapOpen] else options := [];
  options += GetPolyLineOption;
  if Assigned(FArrow) then
    BGRAPen.BGRAPolyLine(self,[PointF(x1,y1),PointF(x2,y2)],w,c,pecRound,pjsRound,FCustomPenStyle,options,nil,JoinMiterLimit,@FArrow.ComputeStartAt,FArrow.StartOffsetX,@FArrow.ComputeEndAt,FArrow.EndOffsetX)
  else
    BGRAPen.BGRAPolyLine(self,[PointF(x1,y1),PointF(x2,y2)],w,c,pecRound,pjsRound,FCustomPenStyle,options,nil,JoinMiterLimit)
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
  options += GetPolyLineOption;
  if Assigned(FArrow) then
    BGRAPen.BGRAPolyLine(self,[PointF(x1,y1),PointF(x2,y2)],w,c,pecRound,pjsRound,FCustomPenStyle,options,texture,JoinMiterLimit,@FArrow.ComputeStartAt,FArrow.StartOffsetX,@FArrow.ComputeEndAt,FArrow.EndOffsetX)
  else
    BGRAPen.BGRAPolyLine(self,[PointF(x1,y1),PointF(x2,y2)],w,c,pecRound,pjsRound,FCustomPenStyle,options,texture,JoinMiterLimit);
end;

procedure TBGRADefaultBitmap.DrawPolyLineAntialias(const points: array of TPointF;
  c: TBGRAPixel; w: single);
begin
  if Assigned(FArrow) then
    BGRAPen.BGRAPolyLine(self,points,w,c,LineCap,JoinStyle,FCustomPenStyle,GetPolyLineOption,nil,JoinMiterLimit,@FArrow.ComputeStartAt,FArrow.StartOffsetX,@FArrow.ComputeEndAt,FArrow.EndOffsetX)
  else
    BGRAPen.BGRAPolyLine(self,points,w,c,LineCap,JoinStyle,FCustomPenStyle,GetPolyLineOption,nil,JoinMiterLimit)
end;

procedure TBGRADefaultBitmap.DrawPolyLineAntialias(
  const points: array of TPointF; texture: IBGRAScanner; w: single);
begin
  if Assigned(FArrow) then
    BGRAPen.BGRAPolyLine(self,points,w,BGRAPixelTransparent,LineCap,JoinStyle,FCustomPenStyle,GetPolyLineOption,texture,JoinMiterLimit,@FArrow.ComputeStartAt,FArrow.StartOffsetX,@FArrow.ComputeEndAt,FArrow.EndOffsetX)
  else
    BGRAPen.BGRAPolyLine(self,points,w,BGRAPixelTransparent,LineCap,JoinStyle,FCustomPenStyle,GetPolyLineOption,texture,JoinMiterLimit);
end;

procedure TBGRADefaultBitmap.DrawPolyLineAntialias(const points: array of TPointF;
  c: TBGRAPixel; w: single; Closed: boolean);
var
  options: TBGRAPolyLineOptions;
begin
  if not closed then options := [plRoundCapOpen] else options := [];
  options += GetPolyLineOption;
  if Assigned(FArrow) then
    BGRAPen.BGRAPolyLine(self,points,w,c,pecRound,JoinStyle,FCustomPenStyle,options,nil,JoinMiterLimit,@FArrow.ComputeStartAt,FArrow.StartOffsetX,@FArrow.ComputeEndAt,FArrow.EndOffsetX)
  else
    BGRAPen.BGRAPolyLine(self,points,w,c,pecRound,JoinStyle,FCustomPenStyle,options,nil,JoinMiterLimit);
end;

procedure TBGRADefaultBitmap.DrawPolyLineAntialias(
  const points: array of TPointF; c: TBGRAPixel; w: single;
  fillcolor: TBGRAPixel);
var multi: TBGRAMultishapeFiller;
begin
  multi := TBGRAMultishapeFiller.Create;
  multi.PolygonOrder := poLastOnTop;
  multi.AddPolygon(points,fillcolor);
  multi.AddPolygon(ComputeWidePolyline(points,w),c);
  if LinearAntialiasing then
    multi.Draw(self,dmLinearBlend)
  else
    multi.Draw(self,dmDrawWithTransparency);
  multi.Free;
end;

procedure TBGRADefaultBitmap.DrawPolyLineAntialiasAutocycle(
  const points: array of TPointF; c: TBGRAPixel; w: single);
begin
  if Assigned(FArrow) then
    BGRAPen.BGRAPolyLine(self,points,w,c,LineCap,JoinStyle,FCustomPenStyle,GetPolyLineOption+[plAutoCycle],nil,JoinMiterLimit,@FArrow.ComputeStartAt,FArrow.StartOffsetX,@FArrow.ComputeEndAt,FArrow.EndOffsetX)
  else
    BGRAPen.BGRAPolyLine(self,points,w,c,LineCap,JoinStyle,FCustomPenStyle,GetPolyLineOption+[plAutoCycle],nil,JoinMiterLimit)
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

procedure TBGRADefaultBitmap.DrawPolygonAntialias(
  const points: array of TPointF; c: TBGRAPixel; w: single;
  fillcolor: TBGRAPixel);
var multi: TBGRAMultishapeFiller;
begin
  multi := TBGRAMultishapeFiller.Create;
  multi.PolygonOrder := poLastOnTop;
  multi.AddPolygon(points,fillcolor);
  multi.AddPolygon(ComputeWidePolygon(points,w),c);
  if LinearAntialiasing then
    multi.Draw(self,dmLinearBlend)
  else
    multi.Draw(self,dmDrawWithTransparency);
  multi.Free;
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

procedure TBGRADefaultBitmap.FillPath(APath: IBGRAPath; c: TBGRAPixel);
begin
  FillPolyAntialias(APath.getPoints,c);
end;

procedure TBGRADefaultBitmap.FillPath(APath: IBGRAPath; texture: IBGRAScanner);
begin
  FillPolyAntialias(APath.getPoints,texture);
end;

procedure TBGRADefaultBitmap.ErasePath(APath: IBGRAPath; alpha: byte);
begin
  ErasePolyAntialias(APath.getPoints,alpha);
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

procedure TBGRADefaultBitmap.FillQuadPerspectiveMapping(pt1, pt2, pt3,
  pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF;
  ACleanBorders: TRect);
var
  persp: TBGRAPerspectiveScannerTransform;
  clean: TBGRAExtendedBorderScanner;
begin
  clean := TBGRAExtendedBorderScanner.Create(texture,ACleanBorders);
  persp := TBGRAPerspectiveScannerTransform.Create(clean,[tex1,tex2,tex3,tex4],[pt1,pt2,pt3,pt4]);
  FillPoly([pt1,pt2,pt3,pt4],persp,dmDrawWithTransparency);
  persp.Free;
  clean.Free;
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

procedure TBGRADefaultBitmap.FillQuadPerspectiveMappingAntialias(pt1, pt2, pt3,
  pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF;
  ACleanBorders: TRect);
var
  persp: TBGRAPerspectiveScannerTransform;
  clean: TBGRAExtendedBorderScanner;
begin
  clean := TBGRAExtendedBorderScanner.Create(texture,ACleanBorders);
  persp := TBGRAPerspectiveScannerTransform.Create(clean,[tex1,tex2,tex3,tex4],[pt1,pt2,pt3,pt4]);
  FillPolyAntialias([pt1,pt2,pt3,pt4],persp);
  persp.Free;
  clean.Free;
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
  BGRAPolygon.FillPolyAntialias(self, points, c, FEraseMode, FillMode = fmWinding, LinearAntialiasing);
end;

procedure TBGRADefaultBitmap.FillPolyAntialias(const points: array of TPointF;
  texture: IBGRAScanner);
begin
  BGRAPolygon.FillPolyAntialiasWithTexture(self, points, texture, FillMode = fmWinding, LinearAntialiasing);
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

procedure TBGRADefaultBitmap.FillShape(shape: TBGRACustomFillInfo; c: TBGRAPixel;
  drawmode: TDrawMode);
begin
  BGRAPolygon.FillShapeAliased(self, shape, c, FEraseMode, nil, FillMode = fmWinding, drawmode);
end;

procedure TBGRADefaultBitmap.FillShape(shape: TBGRACustomFillInfo;
  texture: IBGRAScanner; drawmode: TDrawMode);
begin
  BGRAPolygon.FillShapeAliased(self, shape, BGRAPixelTransparent, false, texture, FillMode = fmWinding, drawmode);
end;

procedure TBGRADefaultBitmap.FillShapeAntialias(shape: TBGRACustomFillInfo;
  c: TBGRAPixel);
begin
  BGRAPolygon.FillShapeAntialias(self, shape, c, FEraseMode, nil, FillMode = fmWinding, LinearAntialiasing);
end;

procedure TBGRADefaultBitmap.FillShapeAntialias(shape: TBGRACustomFillInfo;
  texture: IBGRAScanner);
begin
  BGRAPolygon.FillShapeAntialiasWithTexture(self, shape, texture, FillMode = fmWinding, LinearAntialiasing);
end;

procedure TBGRADefaultBitmap.EraseShape(shape: TBGRACustomFillInfo; alpha: byte);
begin
  BGRAPolygon.FillShapeAliased(self, shape, BGRA(0, 0, 0, alpha), True, nil, FillMode = fmWinding, dmDrawWithTransparency);
end;

procedure TBGRADefaultBitmap.EraseShapeAntialias(shape: TBGRACustomFillInfo;
  alpha: byte);
begin
  FEraseMode := True;
  FillShapeAntialias(shape, BGRA(0, 0, 0, alpha));
  FEraseMode := False;
end;

procedure TBGRADefaultBitmap.EllipseAntialias(x, y, rx, ry: single;
  c: TBGRAPixel; w: single);
begin
  if IsClearPenStyle(FCustomPenStyle) or (c.alpha = 0) then exit;
  if IsSolidPenStyle(FCustomPenStyle) then
    BGRAPolygon.BorderEllipseAntialias(self, x, y, rx, ry, w, c, FEraseMode, LinearAntialiasing)
  else
    DrawPolygonAntialias(ComputeEllipseContour(x,y,rx,ry),c,w);
end;

procedure TBGRADefaultBitmap.EllipseAntialias(x, y, rx, ry: single;
  texture: IBGRAScanner; w: single);
begin
  if IsClearPenStyle(FCustomPenStyle) then exit;
  if IsSolidPenStyle(FCustomPenStyle) then
    BGRAPolygon.BorderEllipseAntialiasWithTexture(self, x, y, rx, ry, w, texture, LinearAntialiasing)
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
  BGRAPolygon.FillEllipseAntialias(self, x, y, rx, ry, c, FEraseMode, LinearAntialiasing);
end;

procedure TBGRADefaultBitmap.FillEllipseAntialias(x, y, rx, ry: single;
  texture: IBGRAScanner);
begin
  BGRAPolygon.FillEllipseAntialiasWithTexture(self, x, y, rx, ry, texture, LinearAntialiasing);
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
    BGRAPolygon.BorderRoundRectangleAntialias(self,x,y,x2,y2,rx,ry,w,options,c,False, LinearAntialiasing)
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
    BGRAPolygon.BorderRoundRectangleAntialiasWithTexture(self,x,y,x2,y2,rx,ry,w,options,texture, LinearAntialiasing)
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
    if (mode <> dmSet) and (mode <> dmXor) and (c.alpha = 0) then exit;

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
        if DWord(c) = 0 then exit
        else
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
  if (abs(tx)<1e-3) or (abs(ty)<1e-3) then exit;
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
  BGRAPolygon.FillRoundRectangleAntialias(self,x,y,x2,y2,rx,ry,options,c,False, LinearAntialiasing);
end;

procedure TBGRADefaultBitmap.FillRoundRectAntialias(x, y, x2, y2, rx,
  ry: single; texture: IBGRAScanner; options: TRoundRectangleOptions);
begin
  BGRAPolygon.FillRoundRectangleAntialiasWithTexture(self,x,y,x2,y2,rx,ry,options,texture, LinearAntialiasing);
end;

procedure TBGRADefaultBitmap.EraseRoundRectAntialias(x, y, x2, y2, rx,
  ry: single; alpha: byte; options: TRoundRectangleOptions);
begin
  BGRAPolygon.FillRoundRectangleAntialias(self,x,y,x2,y2,rx,ry,options,BGRA(0,0,0,alpha),True, LinearAntialiasing);
end;

procedure TBGRADefaultBitmap.RoundRect(X1, Y1, X2, Y2: integer;
  DX, DY: integer; BorderColor, FillColor: TBGRAPixel; ADrawMode: TDrawMode = dmDrawWithTransparency);
begin
  BGRARoundRectAliased(self,X1,Y1,X2,Y2,DX,DY,BorderColor,FillColor,nil,ADrawMode);
end;

procedure TBGRADefaultBitmap.RoundRect(X1, Y1, X2, Y2: integer; DX,
  DY: integer; BorderColor: TBGRAPixel; ADrawMode: TDrawMode);
begin
  BGRARoundRectAliased(self,X1,Y1,X2,Y2,DX,DY,BorderColor,BGRAPixelTransparent,nil,ADrawMode,true);
end;

{------------------------- Text functions ---------------------------------------}

procedure TBGRADefaultBitmap.TextOutAngle(x, y: single; orientationTenthDegCCW: integer;
  sUTF8: string; c: TBGRAPixel; align: TAlignment);
begin
  with (PointF(x,y)-GetFontAnchorRotatedOffset(orientationTenthDegCCW)) do
    FontRenderer.TextOutAngle(self,x,y,orientationTenthDegCCW,CleanTextOutString(sUTF8),c,align);
end;

procedure TBGRADefaultBitmap.TextOutAngle(x, y: single; orientationTenthDegCCW: integer;
  sUTF8: string; texture: IBGRAScanner; align: TAlignment);
begin
  with (PointF(x,y)-GetFontAnchorRotatedOffset(orientationTenthDegCCW)) do
    FontRenderer.TextOutAngle(self,x,y,orientationTenthDegCCW,CleanTextOutString(sUTF8),texture,align);
end;

procedure TBGRADefaultBitmap.TextOutCurved(ACursor: TBGRACustomPathCursor; sUTF8: string; AColor: TBGRAPixel; AAlign: TAlignment; ALetterSpacing: single);
begin
  InternalTextOutCurved(ACursor, sUTF8, AColor, nil, AAlign, ALetterSpacing);
end;

procedure TBGRADefaultBitmap.TextOutCurved(ACursor: TBGRACustomPathCursor; sUTF8: string; ATexture: IBGRAScanner; AAlign: TAlignment; ALetterSpacing: single);
begin
  InternalTextOutCurved(ACursor, sUTF8, BGRAPixelTransparent, ATexture, AAlign, ALetterSpacing);
end;

procedure TBGRADefaultBitmap.TextOut(x, y: single; sUTF8: string;
  texture: IBGRAScanner; align: TAlignment);
begin
  FontRenderer.TextOut(self,x,y,CleanTextOutString(sUTF8),texture,align);
end;

procedure TBGRADefaultBitmap.TextOut(x, y: single; sUTF8: string;
  c: TBGRAPixel; align: TAlignment);
begin
  with (PointF(x,y)-GetFontAnchorRotatedOffset) do
    FontRenderer.TextOut(self,x,y,CleanTextOutString(sUTF8),c,align);
end;

procedure TBGRADefaultBitmap.TextRect(ARect: TRect; x, y: integer;
  sUTF8: string; style: TTextStyle; c: TBGRAPixel);
begin
  with (PointF(x,y)-GetFontAnchorRotatedOffset(0)) do
    FontRenderer.TextRect(self,ARect,round(x),round(y),sUTF8,style,c);
end;

procedure TBGRADefaultBitmap.TextRect(ARect: TRect; x, y: integer; sUTF8: string;
  style: TTextStyle; texture: IBGRAScanner);
begin
  with (PointF(x,y)-GetFontAnchorRotatedOffset(0)) do
    FontRenderer.TextRect(self,ARect,round(x),round(y),sUTF8,style,texture);
end;

{ Returns the total size of the string provided using the current font.
  Orientation is not taken into account, so that the width is along the text.  }
function TBGRADefaultBitmap.TextSize(sUTF8: string): TSize;
begin
  result := FontRenderer.TextSize(sUTF8);
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
  if Assigned(FArrow) then
    Result:= BGRAPen.ComputeWidePolylinePoints(points,w,BGRAWhite,LineCap,JoinStyle,FCustomPenStyle,GetPolyLineOption,JoinMiterLimit,@FArrow.ComputeStartAt,FArrow.StartOffsetX,@FArrow.ComputeEndAt,FArrow.EndOffsetX)
  else
    Result:= BGRAPen.ComputeWidePolylinePoints(points,w,BGRAWhite,LineCap,JoinStyle,FCustomPenStyle,GetPolyLineOption,JoinMiterLimit)
end;

function TBGRADefaultBitmap.ComputeWidePolyline(const points: array of TPointF;
  w: single; Closed: boolean): ArrayOfTPointF;
var
  options: TBGRAPolyLineOptions;
begin
  if not closed then options := [plRoundCapOpen] else options := [];
  options += GetPolyLineOption;
  if Assigned(FArrow) then
    Result:= BGRAPen.ComputeWidePolylinePoints(points,w,BGRAWhite,pecRound,pjsRound,FCustomPenStyle,options,JoinMiterLimit,@FArrow.ComputeStartAt,FArrow.StartOffsetX,@FArrow.ComputeEndAt,FArrow.EndOffsetX)
  else
    Result:= BGRAPen.ComputeWidePolylinePoints(points,w,BGRAWhite,pecRound,pjsRound,FCustomPenStyle,options,JoinMiterLimit);
end;

function TBGRADefaultBitmap.ComputeWidePolygon(const points: array of TPointF;
  w: single): ArrayOfTPointF;
begin
  Result:= BGRAPen.ComputeWidePolylinePoints(points,w,BGRAWhite,LineCap,JoinStyle,FCustomPenStyle,GetPolyLineOption+[plCycle],JoinMiterLimit);
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
  result := BGRAPath.ComputeArcRad(x,y,rx,ry,startRad,endRad,quality);
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
  color: TBGRAPixel; ADrawMode: TDrawMode);
var
  scan: TBGRACustomScanner;
begin
  if (AMask = nil) or (color.alpha = 0) then exit;
  scan := TBGRASolidColorMaskScanner.Create(AMask,Point(-X,-Y),color);
  self.FillRect(X,Y,X+AMask.Width,Y+AMask.Height,scan,ADrawMode);
  scan.Free;
end;

procedure TBGRADefaultBitmap.FillMask(x, y: integer; AMask: TBGRACustomBitmap;
  texture: IBGRAScanner; ADrawMode: TDrawMode);
var
  scan: TBGRACustomScanner;
begin
  if AMask = nil then exit;
  scan := TBGRATextureMaskScanner.Create(AMask,Point(-X,-Y),texture);
  self.FillRect(X,Y,X+AMask.Width,Y+AMask.Height,scan,ADrawMode);
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
  rAfter,gAfter,bAfter,rBefore,gBefore,bBefore: byte;
begin
  colorMask := LongWord(BGRA(255,255,255,0));
  RedGreenBlue(before, rBefore,gBefore,bBefore);
  RedGreenBlue(after, rAfter,gAfter,bAfter);
  beforeBGR := LongWord(BGRA(rBefore,gBefore,bBefore,0));
  afterBGR  := LongWord(BGRA(rAfter,gAfter,bAfter,0));

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
    if PDWord(p)^ = DWord(before) then
      p^ := after;
    Inc(p);
  end;
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.ReplaceColor(ABounds: TRect; before, after: TColor);
var p: PLongWord;
  xb,yb,xcount: integer;

  colorMask,beforeBGR, afterBGR: longword;
  rAfter,gAfter,bAfter,rBefore,gBefore,bBefore: byte;
begin
  colorMask := LongWord(BGRA(255,255,255,0));
  RedGreenBlue(before, rBefore,gBefore,bBefore);
  RedGreenBlue(after, rAfter,gAfter,bAfter);
  beforeBGR := LongWord(BGRA(rBefore,gBefore,bBefore,0));
  afterBGR  := LongWord(BGRA(rAfter,gAfter,bAfter,0));

  if not IntersectRect(ABounds,ABounds,ClipRect) then exit;
  xcount := ABounds.Right-ABounds.Left;
  for yb := ABounds.Top to ABounds.Bottom-1 do
  begin
    p := PLongWord(ScanLine[yb]+ABounds.Left);
    for xb := xcount-1 downto 0 do
    begin
      if p^ and colorMask = beforeBGR then
        p^ := (p^ and not ColorMask) or afterBGR;
      Inc(p);
    end;
  end;
  InvalidateBitmap;
end;

procedure TBGRADefaultBitmap.ReplaceColor(ABounds: TRect; before,
  after: TBGRAPixel);
var p: PBGRAPixel;
  xb,yb,xcount: integer;
begin
  if before.alpha = 0 then
  begin
    ReplaceTransparent(ABounds,after);
    exit;
  end;
  if not IntersectRect(ABounds,ABounds,ClipRect) then exit;
  xcount := ABounds.Right-ABounds.Left;
  for yb := ABounds.Top to ABounds.Bottom-1 do
  begin
    p := ScanLine[yb]+ABounds.Left;
    for xb := xcount-1 downto 0 do
    begin
      if PDWord(p)^ = DWord(before) then
        p^ := after;
      Inc(p);
    end;
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

procedure TBGRADefaultBitmap.ReplaceTransparent(ABounds: TRect;
  after: TBGRAPixel);
var p: PBGRAPixel;
  xb,yb,xcount: integer;
begin
  if not IntersectRect(ABounds,ABounds,ClipRect) then exit;
  xcount := ABounds.Right-ABounds.Left;
  for yb := ABounds.Top to ABounds.Bottom-1 do
  begin
    p := ScanLine[yb]+ABounds.Left;
    for xb := xcount-1 downto 0 do
    begin
      if p^.alpha = 0 then
        p^ := after;
      Inc(p);
    end;
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
  if (FScanWidth <> 0) and (FScanHeight <> 0) then
    result := (GetScanlineFast(PositiveMod(Y+ScanOffset.Y, FScanHeight))+PositiveMod(X+ScanOffset.X, FScanWidth))^
  else
    result := BGRAPixelTransparent;
end;

{ Scanning procedures for IBGRAScanner interface }
procedure TBGRADefaultBitmap.ScanMoveTo(X, Y: Integer);
begin
  if (FScanWidth = 0) or (FScanHeight = 0) then exit;
  LoadFromBitmapIfNeeded;
  FScanCurX := PositiveMod(X+ScanOffset.X, FScanWidth);
  FScanCurY := PositiveMod(Y+ScanOffset.Y, FScanHeight);
  FScanPtr := ScanLine[FScanCurY];
end;

function TBGRADefaultBitmap.ScanNextPixel: TBGRAPixel;
begin
  if (FScanWidth <> 0) and (FScanHeight <> 0) then
  begin
    result := (FScanPtr+FScanCurX)^;
    inc(FScanCurX);
    if FScanCurX = FScanWidth then //cycle
      FScanCurX := 0;
  end
  else
    result := BGRAPixelTransparent;
end;

function TBGRADefaultBitmap.ScanAt(X, Y: Single): TBGRAPixel;
var
  ix, iy: Int32or64;
  iFactX,iFactY: Int32or64;
begin
  if (FScanWidth = 0) or (FScanHeight = 0) then
  begin
    result := BGRAPixelTransparent;
    exit;
  end;
  LoadFromBitmapIfNeeded;
  ix := round(x*256);
  iy := round(y*256);
  iFactX := ix and 255;
  iFactY := iy and 255;
  ix := PositiveMod(ix+(ScanOffset.X shl 8), FScanWidth shl 8) shr 8;
  iy := PositiveMod(iy+(ScanOffset.Y shl 8), FScanHeight shl 8) shr 8;
  if (iFactX = 0) and (iFactY = 0) then
  begin
    result := (GetScanlineFast(iy)+ix)^;
    exit;
  end;
  if ScanInterpolationFilter <> rfLinear then
  begin
    iFactX := FineInterpolation256( iFactX, ScanInterpolationFilter );
    iFactY := FineInterpolation256( iFactY, ScanInterpolationFilter );
  end;
  result := InternalGetPixelCycle256(ix,iy, iFactX,iFactY);
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
        nbCopy := FScanWidth-FScanCurX;
        if count < nbCopy then nbCopy := count;
        move((FScanPtr+FScanCurX)^,pdest^,nbCopy*sizeof(TBGRAPixel));
        inc(pdest,nbCopy);
        inc(FScanCurX,nbCopy);
        if FScanCurX = FScanWidth then FScanCurX := 0;
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
  colormask: longword;
begin
  if CanvasAlphaCorrection then
  begin
    p := FData;
    colormask := longword(BGRA(255,255,255,0));
    for n := NbPixels - 1 downto 0 do
    begin
      if (longword(p^) and colormask <> 0) and (p^.alpha = 0) then
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

  if not CheckPutImageBounds(x,y,sourcewidth,source.height,minxb,minyb,maxxb,maxyb,ignoreleft,FClipRect) then exit;

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

  if not CheckPutImageBounds(x,y,sourcewidth,source.height,minxb,minyb,maxxb,maxyb,ignoreleft,FClipRect) then exit;

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

  if not CheckPutImageBounds(x,y,sourcewidth,source.height,minxb,minyb,maxxb,maxyb,ignoreleft,FClipRect) then exit;

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

{ Draw an image with an affine transformation (rotation, scale, translate).
  Parameters are the bitmap origin, the end of the horizontal axis and the end of the vertical axis.
  The output bounds correspond to the pixels that will be affected in the destination. }
procedure TBGRADefaultBitmap.PutImageAffine(Origin, HAxis, VAxis: TPointF;
  Source: TBGRACustomBitmap; AOutputBounds: TRect; AResampleFilter: TResampleFilter; AMode: TDrawMode; AOpacity: Byte);
var affine: TBGRAAffineBitmapTransform;
    SourceBounds: TRect;
begin
  if (Source = nil) or (AOpacity = 0) then exit;
  IntersectRect(AOutputBounds,AOutputBounds,ClipRect);
  if IsRectEmpty(AOutputBounds) then exit;

  if (abs(Origin.x-round(Origin.x))<1e-6) and (abs(Origin.y-round(Origin.Y))<1e-6) and
     (abs(HAxis.x-(Origin.x+Source.Width))<1e-6) and (abs(HAxis.y-origin.y)<1e-6) and
     (abs(VAxis.x-Origin.x)<1e-6) and (abs(VAxis.y-(Origin.y+Source.Height))<1e-6) then
  begin
    SourceBounds := AOutputBounds;
    OffsetRect(SourceBounds, -round(origin.x),-round(origin.y));
    IntersectRect(SourceBounds,SourceBounds,rect(0,0,Source.Width,Source.Height));
    PutImagePart(round(origin.x)+SourceBounds.Left,round(origin.y)+SourceBounds.Top,Source,SourceBounds,AMode,AOpacity);
    exit;
  end;

  { Create affine transformation }
  affine := TBGRAAffineBitmapTransform.Create(Source, false, AResampleFilter);
  affine.GlobalOpacity := AOpacity;
  affine.Fit(Origin,HAxis,VAxis);
  FillRect(AOutputBounds,affine,AMode);
  affine.Free;
end;

procedure TBGRADefaultBitmap.StretchPutImage(ARect: TRect;
  Source: TBGRACustomBitmap; mode: TDrawMode; AOpacity: byte);
begin
  If (Source = nil) or (AOpacity = 0) then exit;
  if (ARect.Right-ARect.Left = Source.Width) and (ARect.Bottom-ARect.Top = Source.Height) then
     PutImage(ARect.Left,ARect.Top,Source,mode,AOpacity)
  else
     BGRAResample.StretchPutImage(Source, ARect.Right-ARect.Left, ARect.Bottom-ARect.Top, self, ARect.left,ARect.Top, mode, AOpacity);
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

function TBGRADefaultBitmap.FilterTwirl(ABounds: TRect; ACenter: TPoint;
  ARadius: Single; ATurn: Single; AExponent: Single): TBGRACustomBitmap;
begin
  result := BGRAFilters.FilterTwirl(self, ABounds, ACenter, ARadius, ATurn, AExponent);
end;

function TBGRADefaultBitmap.FilterCylinder: TBGRACustomBitmap;
begin
  Result := BGRAFilters.FilterCylinder(self);
end;

function TBGRADefaultBitmap.FilterPlane: TBGRACustomBitmap;
begin
  Result := BGRAFilters.FilterPlane(self);
end;

function TBGRADefaultBitmap.FilterSharpen(Amount: single = 1): TBGRACustomBitmap;
begin
  Result := BGRAFilters.FilterSharpen(self,round(Amount*256));
end;

function TBGRADefaultBitmap.FilterSharpen(ABounds: TRect; Amount: single
  ): TBGRACustomBitmap;
begin
  Result := BGRAFilters.FilterSharpen(self,ABounds,round(Amount*256));
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

function TBGRADefaultBitmap.FilterBlurRadial(ABounds: TRect; radius: integer;
  blurType: TRadialBlurType): TBGRACustomBitmap;
var task: TFilterTask;
begin
  task := BGRAFilters.CreateRadialBlurTask(self, ABounds, radius, blurType);
  try
    result := task.Execute;
  finally
    task.Free;
  end;
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

function TBGRADefaultBitmap.FilterBlurMotion(ABounds: TRect; distance: integer;
  angle: single; oriented: boolean): TBGRACustomBitmap;
var task: TFilterTask;
begin
  task := BGRAFilters.CreateMotionBlurTask(self,ABounds,distance,angle,oriented);
  try
    Result := task.Execute;
  finally
    task.Free;
  end;
end;

function TBGRADefaultBitmap.FilterCustomBlur(mask: TBGRACustomBitmap):
TBGRACustomBitmap;
begin
  Result := BGRAFilters.FilterBlur(self, mask);
end;

function TBGRADefaultBitmap.FilterCustomBlur(ABounds: TRect;
  mask: TBGRACustomBitmap): TBGRACustomBitmap;
var task: TFilterTask;
begin
  task := BGRAFilters.CreateBlurTask(self, ABounds, mask);
  try
    result := task.Execute;
  finally
    task.Free;
  end;
end;

function TBGRADefaultBitmap.FilterEmboss(angle: single): TBGRACustomBitmap;
begin
  Result := BGRAFilters.FilterEmboss(self, angle);
end;

function TBGRADefaultBitmap.FilterEmboss(angle: single; ABounds: TRect): TBGRACustomBitmap;
begin
  Result := BGRAFilters.FilterEmboss(self, angle, ABounds);
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

function TBGRADefaultBitmap.FilterGrayscale(ABounds: TRect): TBGRACustomBitmap;
begin
  Result := BGRAFilters.FilterGrayscale(self, ABounds);
end;

function TBGRADefaultBitmap.FilterNormalize(eachChannel: boolean = True):
TBGRACustomBitmap;
begin
  Result := BGRAFilters.FilterNormalize(self, eachChannel);
end;

function TBGRADefaultBitmap.FilterNormalize(ABounds: TRect; eachChannel: boolean): TBGRACustomBitmap;
begin
  Result := BGRAFilters.FilterNormalize(self, ABounds, eachChannel);
end;

function TBGRADefaultBitmap.FilterRotate(origin: TPointF;
  angle: single; correctBlur: boolean): TBGRACustomBitmap;
begin
  Result := BGRAFilters.FilterRotate(self, origin, angle, correctBlur);
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
     result := RGBToColor(pix.red,pix.green,pix.blue);
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
procedure TBGRADefaultBitmap.VerticalFlip(ARect: TRect);
var
  yb,h2:     integer;
  line:   PBGRAPixel;
  linesize, delta: integer;
  PStart: PBGRAPixel;
  PEnd:   PBGRAPixel;
begin
  if FData = nil then
    exit;

  if (ARect.Right <= ARect.Left) or (ARect.Bottom <= ARect.Top) then exit;
  if not IntersectRect(ARect, ARect, rect(0,0,Width,Height)) then exit;
  LoadFromBitmapIfNeeded;
  linesize := (ARect.Right-ARect.Left) * sizeof(TBGRAPixel);
  line     := nil;
  getmem(line, linesize);
  PStart := GetScanlineFast(ARect.Top)+ARect.Left;
  PEnd   := GetScanlineFast(ARect.Bottom-1)+ARect.Left;
  h2 := (ARect.Bottom-ARect.Top) div 2;
  if LineOrder = riloTopToBottom then delta := +Width else delta := -Width;
  for yb := h2-1 downto 0 do
  begin
    move(PStart^, line^, linesize);
    move(PEnd^, PStart^, linesize);
    move(line^, PEnd^, linesize);
    Inc(PStart, delta);
    Dec(PEnd, delta);
  end;
  freemem(line);
  InvalidateBitmap;
end;

{ Flip horizontally. Swap left pixels with right pixels on each line.

  It is an involution, i.e it does nothing when applied twice}
procedure TBGRADefaultBitmap.HorizontalFlip(ARect: TRect);
var
  yb, xb, w: integer;
  PStart: PBGRAPixel;
  PEnd:   PBGRAPixel;
  temp:   TBGRAPixel;
begin
  if FData = nil then
    exit;

  if (ARect.Right <= ARect.Left) or (ARect.Bottom <= ARect.Top) then exit;
  if not IntersectRect(ARect, ARect, rect(0,0,Width,Height)) then exit;
  w := ARect.Right-ARect.Left;
  LoadFromBitmapIfNeeded;
  for yb := ARect.Top to ARect.Bottom-1 do
  begin
    PStart := GetScanlineFast(yb)+ARect.Left;
    PEnd   := PStart + w;
    for xb := 0 to (w div 2) - 1 do
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

  It is NOT EXACTLY an involution, when applied twice, some color information is lost }
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

procedure TBGRADefaultBitmap.NegativeRect(ABounds: TRect);
var p: PBGRAPixel;
  xb,yb,xcount: integer;
begin
  if not IntersectRect(ABounds,ABounds,ClipRect) then exit;
  xcount := ABounds.Right-ABounds.Left;
  for yb := ABounds.Top to ABounds.Bottom-1 do
  begin
    p := ScanLine[yb]+ABounds.Left;
    for xb := xcount-1 downto 0 do
    begin
      if p^.alpha <> 0 then
      begin
        p^.red   := GammaCompressionTab[not GammaExpansionTab[p^.red]];
        p^.green := GammaCompressionTab[not GammaExpansionTab[p^.green]];
        p^.blue  := GammaCompressionTab[not GammaExpansionTab[p^.blue]];
      end;
      Inc(p);
    end;
  end;
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

procedure TBGRADefaultBitmap.LinearNegativeRect(ABounds: TRect);
var p: PBGRAPixel;
  xb,yb,xcount: integer;
begin
  if not IntersectRect(ABounds,ABounds,ClipRect) then exit;
  xcount := ABounds.Right-ABounds.Left;
  for yb := ABounds.Top to ABounds.Bottom-1 do
  begin
    p := ScanLine[yb]+ABounds.Left;
    for xb := xcount-1 downto 0 do
    begin
      if p^.alpha <> 0 then
      begin
        p^.red   := not p^.red;
        p^.green := not p^.green;
        p^.blue  := not p^.blue;
      end;
      Inc(p);
    end;
  end;
end;

procedure TBGRADefaultBitmap.InplaceGrayscale;
begin
  InplaceGrayscale(rect(0,0,Width,Height));
end;

procedure TBGRADefaultBitmap.InplaceGrayscale(ABounds: TRect);
var
  task: TFilterTask;
begin
  task := CreateGrayscaleTask(self, ABounds);
  task.Destination := self;
  task.Execute;
  task.Free;
end;

{ Swap red and blue channels. Useful when RGB order is swapped.

  It is an involution, i.e it does nothing when applied twice }
procedure TBGRADefaultBitmap.SwapRedBlue;
const RedMask = 255 shl TBGRAPixel_RedShift;
      BlueMask = 255 shl TBGRAPixel_BlueShift;
      GreenAndAlphaMask = (255 shl TBGRAPixel_GreenShift) or (255 shl TBGRAPixel_AlphaShift);
      RedMask64 = RedMask or (RedMask shl 32);
      BlueMask64 = BlueMask or (BlueMask shl 32);
      GreenAndAlphaMask64 = GreenAndAlphaMask or (GreenAndAlphaMask shl 32);
var
  n: NativeInt;
  p: PLongword;
  temp: longword;
  temp64: QWord;
  oddN: boolean;
begin
  {$PUSH}{$WARNINGS OFF}
  LoadFromBitmapIfNeeded;
  p := PLongword(Data);
  n := NbPixels;
  if n = 0 then
    exit;
  oddN := odd(n);
  n := n shr 1;
  if TBGRAPixel_RedShift > TBGRAPixel_BlueShift then
    while n > 0 do
    begin
      temp64 := PQWord(p)^;
      PQWord(p)^ := ((temp64 and BlueMask64) shl (TBGRAPixel_RedShift-TBGRAPixel_BlueShift)) or
                    ((temp64 and RedMask64) shr (TBGRAPixel_RedShift-TBGRAPixel_BlueShift)) or
                    (temp64 and GreenAndAlphaMask64);
      dec(n);
      inc(p,2);
    end else
    while n > 0 do
    begin
      temp64 := PQWord(p)^;
      PQWord(p)^ := ((temp64 and BlueMask64) shr (TBGRAPixel_BlueShift-TBGRAPixel_RedShift)) or
                    ((temp64 and RedMask64) shl (TBGRAPixel_BlueShift-TBGRAPixel_RedShift)) or
                    (temp64 and GreenAndAlphaMask64);
      dec(n);
      inc(p,2);
    end;
  n := NativeInt(oddN);
  if TBGRAPixel_RedShift > TBGRAPixel_BlueShift then
    while n > 0 do
    begin
      temp := p^;
      p^ := ((temp and BlueMask) shl (TBGRAPixel_RedShift-TBGRAPixel_BlueShift)) or
            ((temp and RedMask) shr (TBGRAPixel_RedShift-TBGRAPixel_BlueShift)) or
            (temp and GreenAndAlphaMask);
      dec(n);
      inc(p);
    end else
    while n > 0 do
    begin
      temp := p^;
      p^ := ((temp and BlueMask) shr (TBGRAPixel_BlueShift-TBGRAPixel_RedShift)) or
            ((temp and RedMask) shl (TBGRAPixel_BlueShift-TBGRAPixel_RedShift)) or
            (temp and GreenAndAlphaMask);
      dec(n);
      inc(p);
    end;
  InvalidateBitmap;
  {$POP}
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
procedure TBGRADefaultBitmap.ApplyMask(mask: TBGRACustomBitmap; ARect: TRect; AMaskRectTopLeft: TPoint);
var
  p, pmask: PBGRAPixel;
  yb, xb:   integer;
  MaskOffsetX,MaskOffsetY,w: integer;
  opacity: NativeUint;
begin
  if (ARect.Right <= ARect.Left) or (ARect.Bottom <= ARect.Top) then exit;
  IntersectRect(ARect, ARect, rect(0,0,Width,Height));
  MaskOffsetX := AMaskRectTopLeft.x - ARect.Left;
  MaskOffsetY := AMaskRectTopLeft.y - ARect.Top;
  OffsetRect(ARect, MaskOffsetX, MaskOffsetY);
  IntersectRect(ARect, ARect, rect(0,0,mask.Width,mask.Height));
  OffsetRect(ARect, -MaskOffsetX, -MaskOffsetY);

  LoadFromBitmapIfNeeded;
  w := ARect.Right-ARect.Left-1;
  for yb := ARect.Top to ARect.Bottom - 1 do
  begin
    p     := Scanline[yb]+ARect.Left;
    pmask := Mask.Scanline[yb+MaskOffsetY]+ARect.Left+MaskOffsetX;
    for xb := w downto 0 do
    begin
      opacity := ApplyOpacity(p^.alpha, pmask^.red);
      if opacity = 0 then p^ := BGRAPixelTransparent
      else p^.alpha := opacity;
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

procedure TBGRADefaultBitmap.ApplyGlobalOpacity(ABounds: TRect; alpha: byte);
var p: PBGRAPixel;
  xb,yb,xcount: integer;
begin
  if not IntersectRect(ABounds,ABounds,ClipRect) then exit;
  xcount := ABounds.Right-ABounds.Left;
  for yb := ABounds.Top to ABounds.Bottom-1 do
  begin
    p := ScanLine[yb]+ABounds.Left;
    for xb := xcount-1 downto 0 do
    begin
      p^.alpha := ApplyOpacity(p^.alpha, alpha);
      Inc(p);
    end;
  end;
  InvalidateBitmap;
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

procedure TBGRADefaultBitmap.DrawCheckers(ARect: TRect; AColorEven,
  AColorOdd: TBGRAPixel);
const tx = 8; ty = 8; //must be a power of 2
      xMask = tx*2-1;
var xcount,patY,w,n,patY1,patY2m1,patX,patX1: NativeInt;
    pdest: PBGRAPixel;
    delta: PtrInt;
    actualRect: TRect;
begin
  actualRect := ARect;
  IntersectRect(actualRect, ARect, self.ClipRect);
  w := actualRect.Right-actualRect.Left;
  if (w <= 0) or (actualRect.Bottom <= actualRect.Top) then exit;
  delta := self.Width;
  if self.LineOrder = riloBottomToTop then delta := -delta;
  delta := (delta-w)*SizeOf(TBGRAPixel);
  pdest := self.ScanLine[actualRect.Top]+actualRect.left;
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
        FillDWord(pdest^,n,DWord(AColorEven));
        dec(xcount,n);
        inc(pdest,n);
        patX := tx;
      end else
      begin
        n := 16-patX;
        if n > xcount then n := xcount;
        FillDWord(pdest^,n,DWord(AColorOdd));
        dec(xcount,n);
        inc(pdest,n);
        patX := 0;
      end;
    end;
    inc(pbyte(pdest),delta);
  end;
  self.InvalidateBitmap;
end;

{ Get bounds of non zero values of specified channel }
function TBGRADefaultBitmap.GetImageBounds(Channel: TChannel = cAlpha; ANothingValue: Byte = 0): TRect;
begin
  result := GetImageBounds([Channel], ANothingValue);
end;

function TBGRADefaultBitmap.GetImageBounds(Channels: TChannels; ANothingValue: Byte = 0): TRect;
var
  minx, miny, maxx, maxy: integer;
  xb, xb2, yb: integer;
  p:      PDWord;
  colorMask, colorZeros: DWord;
begin
  maxx := -1;
  maxy := -1;
  minx := self.Width;
  miny := self.Height;
  colorMask := 0;
  colorZeros := 0;
  if cBlue in Channels then
  begin
    colorMask := colorMask or longword(BGRA(0,0,255,0));
    colorZeros:= colorZeros or longword(BGRA(0,0,ANothingValue,0));
  end;
  if cGreen in Channels then
  begin
    colorMask := colorMask or longword(BGRA(0,255,0,0));
    colorZeros:= colorZeros or longword(BGRA(0,ANothingValue,0,0));
  end;
  if cRed in Channels then
  begin
    colorMask := colorMask or longword(BGRA(255,0,0,0));
    colorZeros:= colorZeros or longword(BGRA(ANothingValue,0,0,0));
  end;
  if cAlpha in Channels then
  begin
    colorMask := colorMask or longword(BGRA(0,0,0,255));
    colorZeros:= colorZeros or longword(BGRA(0,0,0,ANothingValue));
  end;
  colorMask := NtoLE(colorMask);
  colorZeros := NtoLE(colorZeros);
  for yb := 0 to self.Height - 1 do
  begin
    p := PDWord(self.ScanLine[yb]);
    for xb := 0 to self.Width - 1 do
    begin
      if (p^ and colorMask) <> colorZeros then
      begin
        if xb < minx then
          minx := xb;
        if yb < miny then
          miny := yb;
        if xb > maxx then
          maxx := xb;
        if yb > maxy then
          maxy := yb;

        inc(p, self.width-1-xb);
        for xb2 := self.Width-1 downto xb+1 do
        begin
          if (p^ and colorMask) <> colorZeros then
          begin
            if xb2 > maxx then
              maxx := xb2;
            break;
          end;
          dec(p);
        end;
        break;
      end;
      Inc(p);
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
      ptrbmp := CreatePtrBitmap(Width,Bottom-Top,ScanLine[Top]) else
      ptrbmp := CreatePtrBitmap(Width,Bottom-Top,ScanLine[Bottom-1]);
    ptrbmp.LineOrder := LineOrder;
    result := ptrbmp;
  end;
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

function TBGRADefaultBitmap.CreatePtrBitmap(AWidth, AHeight: integer;
  AData: PBGRAPixel): TBGRAPtrBitmap;
begin
  result := TBGRAPtrBitmap.Create(AWidth,AHeight,AData);
end;

procedure TBGRADefaultBitmap.FreeBitmap;
begin
  FreeAndNil(FBitmap);
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

procedure TBGRADefaultBitmap.SetLineOrder(AValue: TRawImageLineOrder);
begin
  FLineOrder := AValue;
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

function TBGRAPtrBitmap.GetLineOrder: TRawImageLineOrder;
begin
  result := inherited GetLineOrder;
end;

procedure TBGRAPtrBitmap.SetLineOrder(AValue: TRawImageLineOrder);
begin
  inherited SetLineOrder(AValue);
end;

procedure TBGRAPtrBitmap.ReallocData;
begin
  //nothing
end;

procedure TBGRAPtrBitmap.FreeData;
begin
  FData := nil;
end;

procedure TBGRAPtrBitmap.CannotResize;
begin
  raise exception.Create('A pointer bitmap cannot be resized');
end;

procedure TBGRAPtrBitmap.NotImplemented;
begin
  raise exception.Create('Not implemented');
end;

procedure TBGRAPtrBitmap.RebuildBitmap;
begin
  NotImplemented;
end;

function TBGRAPtrBitmap.CreateDefaultFontRenderer: TBGRACustomFontRenderer;
begin
  result := nil;
  NotImplemented;
end;

function TBGRAPtrBitmap.LoadFromRawImage(ARawImage: TRawImage;
  DefaultOpacity: byte; AlwaysReplaceAlpha: boolean;
  RaiseErrorOnInvalidPixelFormat: boolean): boolean;
begin
  result := false;
  NotImplemented;
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

procedure TBGRAPtrBitmap.DataDrawTransparent(ACanvas: TCanvas; Rect: TRect;
  AData: Pointer; ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer);
begin
  NotImplemented;
end;

procedure TBGRAPtrBitmap.DataDrawOpaque(ACanvas: TCanvas; Rect: TRect;
  AData: Pointer; ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer);
begin
  NotImplemented;
end;

procedure TBGRAPtrBitmap.GetImageFromCanvas(CanvasSource: TCanvas; x, y: integer
  );
begin
  NotImplemented;
end;

procedure TBGRAPtrBitmap.Assign(Source: TPersistent);
begin
  CannotResize;
end;

procedure TBGRAPtrBitmap.TakeScreenshot(ARect: TRect);
begin
  CannotResize;
end;

procedure TBGRAPtrBitmap.TakeScreenshotOfPrimaryMonitor;
begin
  CannotResize;
end;

procedure TBGRAPtrBitmap.LoadFromDevice(DC: System.THandle);
begin
  CannotResize;
end;

procedure TBGRAPtrBitmap.LoadFromDevice(DC: System.THandle; ARect: TRect);
begin
  CannotResize;
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

end.

