{=== TBGRACustomBitmap and IBGRAScanner ===}
{$IFDEF INCLUDE_SCANNER_INTERFACE}
{$UNDEF INCLUDE_SCANNER_INTERFACE}
type
  TBGRACustomBitmap = class;

{==== IBGRAScanner ====}
  {* Interface for a scanner. A scanner is like an image, but its content
     has no limit and it can be calculated on the fly. It is like a
     infinite readonly image.
   *
   * Note: it must not implement reference counting even if it is an interface
   *
   * ''TBGRACustomBitmap'' implements this interface and the content is repeated
     horizontally and vertically. There are also various classes
     in ''BGRAGradientScanner'' unit that generate gradients on the fly and
     in ''BGRATransform'' unit that provide geometrical transformations of images }
  IBGRAScanner = interface
    {** Move to the position (''X'',''Y'') for the next call to ''ScanNextPixel'' }
    procedure ScanMoveTo(X,Y: Integer);
    {** Scan the pixel at the current location and increments ''X'' }
    function ScanNextPixel: TBGRAPixel;
    function ScanNextExpandedPixel: TExpandedPixel;
    {** Scan at any location using floating point coordinates }
    function ScanAt(X,Y: Single): TBGRAPixel;
    function ScanAtExpanded(X,Y: Single): TExpandedPixel;
    {** Scan at any location using integer coordinates }
    function ScanAtInteger(X,Y: integer): TBGRAPixel;
    function ScanAtIntegerExpanded(X,Y: integer): TExpandedPixel;
    {** Copy a row of pixels from ''X'' to ''X''+''count''-1 to a specified destination
        ''pdest''. ''mode'' indicates how to combine with existing data }
    procedure ScanPutPixels(pdest: PBGRAPixel; count: integer; mode: TDrawMode);
    {** Returns True if the function ''ScanPutPixels'' is available. Otherwise
        you need to call ''ScanNextPixel'' and combine pixels for example
        with ''SetPixel'' }
    function IsScanPutPixelsDefined: boolean;
    {** Returns the corresponding OpenGL texture. The value is ''nil'' if no texture is associated. **}
    function GetTextureGL: IUnknown;
  end;

  {** A type of function of a scanner that returns the content at floating point coordinates }
  TScanAtFunction = function (X,Y: Single): TBGRAPixel of object;
  {** A type of function of a scanner that returns the content at integer coordinates }
  TScanAtIntegerFunction = function (X,Y: Integer): TBGRAPixel of object;
  {** A type of function of a scanner that returns the next pixel }
  TScanNextPixelFunction = function: TBGRAPixel of object;

  { TBGRACustomScanner }
  {* Base class for implementing ''IBGRAScanner'' interface }
  TBGRACustomScanner = class(IBGRAScanner)
  private
    FCurX,FCurY: integer;
  public
    function ScanAtInteger(X,Y: integer): TBGRAPixel; virtual;
    function ScanAtIntegerExpanded(X,Y: integer): TExpandedPixel; virtual;
    procedure ScanMoveTo(X,Y: Integer); virtual;
    function ScanNextPixel: TBGRAPixel; virtual;
    function ScanNextExpandedPixel: TExpandedPixel; virtual;
    function ScanAt(X,Y: Single): TBGRAPixel; virtual; abstract;
    function ScanAtExpanded(X,Y: Single): TExpandedPixel; virtual;
    procedure ScanPutPixels(pdest: PBGRAPixel; count: integer; mode: TDrawMode); virtual;
    function IsScanPutPixelsDefined: boolean; virtual;
    function GetTextureGL: IUnknown; virtual;
  protected
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
    function _AddRef: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
    function _Release: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
  end;
{$ENDIF}

{$IFDEF INCLUDE_INTERFACE}
{$UNDEF INCLUDE_INTERFACE}
type
{==== TBGRACustomBitmap ====}

  { TBGRACustomBitmap }
  {* This is the base class for ''TBGRABitmap''. It is the direct parent of
     ''TBGRADefaultBitmap'' class, which is the parent of the diverse
     implementations. A bitmap can be used as a scanner using the interface
     ''IBGRAScanner'' }
  TBGRACustomBitmap = class(TFPCustomImage,IBGRAScanner)
  private
    function GetFontAntialias: Boolean;
    procedure SetFontAntialias(const AValue: Boolean);
  protected
     { accessors to properies }
     function GetArrowEndRepeat: integer; virtual; abstract;
     function GetArrowStartRepeat: integer; virtual; abstract;
     procedure SetArrowEndRepeat(AValue: integer); virtual; abstract;
     procedure SetArrowStartRepeat(AValue: integer); virtual; abstract;
     function GetArrowEndOffset: single; virtual; abstract;
     function GetArrowStartOffset: single; virtual; abstract;
     procedure SetArrowEndOffset(AValue: single); virtual; abstract;
     procedure SetArrowStartOffset(AValue: single); virtual; abstract;
     function GetArrowEndSize: TPointF; virtual; abstract;
     function GetArrowStartSize: TPointF; virtual; abstract;
     procedure SetArrowEndSize(AValue: TPointF); virtual; abstract;
     procedure SetArrowStartSize(AValue: TPointF); virtual; abstract;
     function GetLineCap: TPenEndCap; virtual; abstract;
     procedure SetLineCap(AValue: TPenEndCap); virtual; abstract;
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
     procedure SetArrowStart(AStyle: TBGRAArrowStyle; ATipStyle: TPenJoinStyle = pjsMiter; ARelativePenWidth: single = 1; ATriangleBackOffset: single = 0); virtual; abstract;
     procedure SetArrowEnd(AStyle: TBGRAArrowStyle; ATipStyle: TPenJoinStyle = pjsMiter; ARelativePenWidth: single = 1; ATriangleBackOffset: single = 0); virtual; abstract;

     procedure InternalArc(cx,cy,rx,ry: single; const StartPoint,EndPoint: TPointF; ABorderColor: TBGRAPixel; w: single; AFillColor: TBGRAPixel; AOptions: TArcOptions; ADrawChord: boolean = false; ATexture: IBGRAScanner = nil); overload;
     procedure InternalArc(cx,cy,rx,ry: single; StartAngleRad,EndAngleRad: Single; ABorderColor: TBGRAPixel; w: single; AFillColor: TBGRAPixel; AOptions: TArcOptions; ADrawChord: boolean = false; ATexture: IBGRAScanner = nil); overload; virtual; abstract;
     procedure InternalArcInRect(r: TRect; StartAngleRad,EndAngleRad: Single; ABorderColor : TBGRAPixel; w: single; AFillColor: TBGRAPixel; AOptions: TArcOptions; ADrawChord: boolean = false; ATexture: IBGRAScanner = nil);
     procedure InternalFillArcInRect(r: TRect; StartAngleRad,EndAngleRad: Single; AFillColor: TBGRAPixel; AOptions: TArcOptions; ATexture: IBGRAScanner = nil);

     function GetTextureGL: IUnknown; virtual;

  public
     {** User defined caption. It does not appear on the image }
     Caption:   string;

     {** Method to use when filling polygons (winding or alternate).
         See [[BGRABitmap Types imported from Graphics|BGRAGraphics]] }
     FillMode:  TFillMode;

     {** Specifies if linear antialiasing must be used when drawing
         antialiased shapes }
     LinearAntialiasing: boolean;

     {** Resample filter is used when resizing the bitmap. See [[BGRABitmap Miscellaneous types#Images and resampling|resampling types]] }
     ResampleFilter : TResampleFilter;

     {** Scan interpolation filter is used when the bitmap is used
         as a scanner (interface ''IBGRAScanner'') }
     ScanInterpolationFilter: TResampleFilter;
     {** Offset to apply when the image is scanned using ''IBGRAScanner'' interface }
     ScanOffset: TPoint;

     {** Width of the image in pixels }
     property Width: integer Read GetWidth;
     {** Height of the image in pixels }
     property Height: integer Read GetHeight;

     {** Clipping rectangle for all drawing functions }
     property ClipRect: TRect read GetClipRect write SetClipRect;

     {** Total number of pixels. It is always true that ''NbPixels'' = ''Width'' * ''Height'' }
     property NbPixels: integer Read GetNbPixels;

     {** Returns the address of the left-most pixel of any line.
         The parameter y ranges from 0 to Height-1 }
     property ScanLine[y: integer]: PBGRAPixel Read GetScanLine;

     {** Indicates the order in which lines are stored in memory.
         If it is equal to ''riloTopToBottom'', the first line is the top line.
         If it is equal to ''riloBottomToTop'', the first line is the bottom line.
         See [[BGRABitmap Miscellaneous types|miscellaneous types]] }
     property LineOrder: TRawImageLineOrder Read GetLineOrder;

     {** Provides a pointer to the first pixel in memory.
         Depending on the ''LineOrder'' property, this can be the top-left pixel or the bottom-left pixel.
         There is no padding between scanlines, so the start of the next line is at the address ''Data'' + ''Width''.
         See [[BGRABitmap tutorial 4]] }
     property Data: PBGRAPixel Read GetDataPtr;

     {** Number of references to this image. It is increased by the function
         ''NewReference'' and decreased by the function ''FreeReference'' }
     property RefCount: integer Read GetRefCount;

     {** Returns True if the bitmap only contains transparent pixels or has a size of zero }
     property Empty: boolean Read CheckEmpty;

     {** Returns True if there are transparent or semitransparent pixels,
         and so if the image would be stored with an alpha channel }
     property HasTransparentPixels: boolean Read GetHasTransparentPixels;

     {** Average color of the image }
     property AverageColor: TColor Read GetAverageColor;
     {** Average color (including alpha) of the image }
     property AveragePixel: TBGRAPixel Read GetAveragePixel;

     {** Canvas compatible with FreePascal }
     property CanvasFP: TFPImageCanvas read GetCanvasFP;
     {** Draw mode to used when image is access using FreePascal functions
         and ''Colors'' property }
     property CanvasDrawModeFP: TDrawMode read GetCanvasDrawModeFP write SetCanvasDrawModeFP;

     {** Bitmap in a format compatible with the current GUI.
         Don't forget to call ''InvalidateBitmap'' before using it
         if you changed something with direct pixel access (''Scanline''
         and ''Data'') }
     property Bitmap: TBitmap Read GetBitmap;
     {** Canvas provided by the GUI }
     property Canvas: TCanvas Read GetCanvas;
     {** Opacity to apply to changes made using GUI functions, provided
         ''CanvasAlphaCorrection'' is set to ''True'' }
     property CanvasOpacity: byte Read GetCanvasOpacity Write SetCanvasOpacity;
     {** Specifies if the alpha values must be corrected after GUI access
         to the bitmap }
     property CanvasAlphaCorrection: boolean Read GetCanvasAlphaCorrection Write SetCanvasAlphaCorrection;

  public {----------- pen style ----------------}
     {** How to join segments. See [[BGRABitmap Types imported from Graphics|BGRAGraphics]] }
     JoinStyle: TPenJoinStyle;
     {** Limit for the extension of the segments when joining them
         with ''pjsMiter'' join style, expressed in multiples of the width
         of the pen }
     JoinMiterLimit: single;

     {** Pen style. See [[BGRABitmap Types imported from Graphics|BGRAGraphics]] }
     property PenStyle: TPenStyle read GetPenStyle Write SetPenStyle;
     {** Custom pen style. See [[BGRABitmap Geometry types|geometric types]] }
     property CustomPenStyle: TBGRAPenStyle read GetCustomPenStyle write SetCustomPenStyle;
     {** How to draw the ends of a line }
     property LineCap: TPenEndCap read GetLineCap write SetLineCap;

     {** Size of arrows at the start of the line }
     property ArrowStartSize: TPointF read GetArrowStartSize write SetArrowStartSize;
     {** Size of arrows at the end of the line }
     property ArrowEndSize: TPointF read GetArrowEndSize write SetArrowEndSize;
     {** Offset of the arrow from the start of the line }
     property ArrowStartOffset: single read GetArrowStartOffset write SetArrowStartOffset;
     {** Offset of the arrow from the end of the line }
     property ArrowEndOffset: single read GetArrowEndOffset write SetArrowEndOffset;
     {** Number of times to repeat the starting arrow }
     property ArrowStartRepeat: integer read GetArrowStartRepeat write SetArrowStartRepeat;
     {** Number of times to repeat the ending arrow }
     property ArrowEndRepeat: integer read GetArrowEndRepeat write SetArrowEndRepeat;

  public {-------------------font style------------------------}
     {** Specifies the font to use. Unless the font renderer accept otherwise,
         the name is in human readable form, like 'Arial', 'Times New Roman', ... }
     FontName: string;
     {** Specifies the set of styles to be applied to the font.
         These can be ''fsBold'', ''fsItalic'', ''fsStrikeOut'', ''fsUnderline''.
         So the value [''fsBold'',''fsItalic''] means that the font must be bold and italic.
         See [[BGRABitmap Miscellaneous types|miscellaneous types]] }
     FontStyle: TFontStyles;

     {** Specifies the quality of rendering. Default value is ''fqSystem''.
         See [[BGRABitmap Miscellaneous types|miscellaneous types]] }
     FontQuality : TBGRAFontQuality;

     {** Specifies the rotation of the text, for functions that support text rotation.
         It is expressed in tenth of degrees, positive values going counter-clockwise. }
     FontOrientation: integer;

     {** Specifies how the font is vertically aligned relative to the start coordinate.
         See [[BGRABitmap Miscellaneous types|miscellaneous types]]}
     FontVerticalAnchor: TFontVerticalAnchor;

     {** Specifies the height of the font in pixels without taking into account
         additional line spacing. A negative value means that it is the
         full height instead (see below) }
     property FontHeight: integer Read GetFontHeight Write SetFontHeight;

     {** Specifies the height of the font in pixels, taking into account the
         additional line spacing defined for the font }
     property FontFullHeight: integer read GetFontFullHeight write SetFontFullHeight;

     {** Simplified property to specify the quality (see ''FontQuality'') }
     property FontAntialias: Boolean read GetFontAntialias write SetFontAntialias;
     {** Returns measurement for the current font in pixels }
     property FontPixelMetric: TFontPixelMetric read GetFontPixelMetric;

     {** Specifies the font renderer. When working with the LCL,
         by default it is an instance of ''TLCLFontRenderer'' of
         unit ''BGRAText''. Other renderers are provided in ''BGRATextFX''
         unit and ''BGRAVectorize'' unit. Additionally, ''BGRAFreeType''
         provides a renderer independent from the LCL.
       *
       * Once you assign a renderer, it will automatically be freed when
         the bitmap is freed. The renderers may provide additional styling
         for the font, not accessible with the properties in this class
       *
       * See [[BGRABitmap tutorial Font rendering|font rendering]]}
     property FontRenderer: TBGRACustomFontRenderer read GetFontRenderer write SetFontRenderer;

  public
     constructor Create; virtual; abstract; overload;
     constructor Create(AFPImage: TFPCustomImage); virtual; abstract; overload;
     constructor Create(ABitmap: TBitmap; AUseTransparent: boolean = true); virtual; abstract; overload;
     constructor Create(AWidth, AHeight: integer; Color: TColor); virtual; abstract; overload;
     constructor Create(AWidth, AHeight: integer; Color: TBGRAPixel); virtual; abstract; overload;
     constructor Create(AFilename: string); virtual; abstract; overload;
     constructor Create(AFilename: string; AIsUtf8Filename: boolean); virtual; abstract; overload;
     constructor Create(AStream: TStream); virtual; abstract; overload;

     function NewBitmap(AWidth, AHeight: integer): TBGRACustomBitmap; virtual; abstract; overload;
     function NewBitmap(AWidth, AHeight: integer; Color: TBGRAPixel): TBGRACustomBitmap; virtual; abstract; overload;
     function NewBitmap(Filename: string): TBGRACustomBitmap; virtual; abstract; overload;
     function NewBitmap(Filename: string; AIsUtf8: boolean): TBGRACustomBitmap; virtual; abstract; overload;
     function NewBitmap(AFPImage: TFPCustomImage): TBGRACustomBitmap; virtual; abstract; overload;

     //there are UTF8 functions that are different from standard function as those
     //depend on TFPCustomImage that does not clearly handle UTF8
  {==== Load and save files ====}
     {** Load image from a file. ''filename'' is an ANSI string }
     procedure LoadFromFile(const filename: string); virtual;
     {** Load image from a file with the specified image reader. ''filename'' is an ANSI string }
     procedure LoadFromFile(const filename:String; Handler:TFPCustomImageReader); virtual;
     {** Load image from a file. ''filename'' is an UTF8 string }
     procedure LoadFromFileUTF8(const filenameUTF8: string); virtual;
     {** Load image from a file with the specified image reader. ''filename'' is an UTF8 string }
     procedure LoadFromFileUTF8(const filenameUTF8: string; AHandler: TFPCustomImageReader); virtual;
     {** Load image from a stream. Format is detected automatically }
     procedure LoadFromStream(Str: TStream); virtual; overload;
     {** Load image from a stream. The specified image reader is used }
     procedure LoadFromStream(Str: TStream; Handler: TFPCustomImageReader); virtual; overload;

     {** Save image to a file. The format is guessed from the file extension. ''filename'' is an ANSI string }
     procedure SaveToFile(const filename: string); virtual; overload;
     {** Save image to a file with the specified image writer. ''filename'' is an ANSI string }
     procedure SaveToFile(const filename: string; Handler:TFPCustomImageWriter); virtual; overload;
     {** Save image to a file. The format is guessed from the file extension. ''filename'' is an ANSI string }
     procedure SaveToFileUTF8(const filenameUTF8: string); virtual; overload;
     {** Save image to a file with the specified image writer. ''filename'' is an UTF8 string }
     procedure SaveToFileUTF8(const filenameUTF8: string; Handler:TFPCustomImageWriter); virtual; overload;

     {** Save image to a stream with the specified image writer }{inherited
     procedure SaveToStream (Str:TStream; Handler:TFPCustomImageWriter);
    }{** Save image to a stream in the specified image format }
     procedure SaveToStreamAs(Str: TStream; AFormat: TBGRAImageFormat); virtual;
     {** Save image to a stream in PNG format }
     procedure SaveToStreamAsPng(Str: TStream); virtual;

     procedure Serialize(AStream: TStream); virtual; abstract;
     procedure Deserialize(AStream: TStream); virtual; abstract;

     {** Gets the content of the specified device context }
     procedure LoadFromDevice(DC: System.THandle); virtual; abstract; overload;
     {** Gets the content from the specified rectangular area of a device context }
     procedure LoadFromDevice(DC: System.THandle; ARect: TRect); virtual; abstract; overload;
     {** Fills the content with a screenshot of the primary monitor }
     procedure TakeScreenshotOfPrimaryMonitor; virtual; abstract;
     {** Fills the content with a screenshot of the specified rectangular area of the desktop
         (it can be from any screen) }
     procedure TakeScreenshot(ARect: TRect); virtual; abstract;
     {** For more methods, see derived class [[TBGRABitmap class|TBGRABitmap]] }

     {Pixel functions}
     procedure SetPixel(x, y: int32or64; c: TColor); virtual; abstract; overload;
     procedure XorPixel(x, y: int32or64; c: TBGRAPixel); virtual; abstract; overload;
     procedure SetPixel(x, y: int32or64; c: TBGRAPixel); virtual; abstract; overload;
     procedure DrawPixel(x, y: int32or64; c: TBGRAPixel); virtual; abstract; overload;
     procedure DrawPixel(x, y: int32or64; c: TBGRAPixel; ADrawMode: TDrawMode); overload;
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
     procedure DrawHorizLine(x, y, x2: int32or64; texture: IBGRAScanner); overload;
     procedure FastBlendHorizLine(x, y, x2: int32or64; c: TBGRAPixel); virtual; abstract;
     procedure HorizLine(x,y,x2: Int32or64; c: TBGRAPixel; ADrawMode: TDrawMode); overload;
     procedure HorizLine(x,y,x2: Int32or64; texture: IBGRAScanner; ADrawMode: TDrawMode); virtual; abstract; overload;
     procedure DrawHorizLineDiff(x, y, x2: int32or64; c, compare: TBGRAPixel; maxDiff: byte); virtual; abstract;
     procedure AlphaHorizLine(x, y, x2: int32or64; alpha: byte); virtual; abstract;

     procedure SetVertLine(x, y, y2: int32or64; c: TBGRAPixel); virtual; abstract;
     procedure XorVertLine(x, y, y2: int32or64; c: TBGRAPixel); virtual; abstract;
     procedure DrawVertLine(x, y, y2: int32or64; c: TBGRAPixel); virtual; abstract;
     procedure AlphaVertLine(x, y, y2: int32or64; alpha: byte); virtual; abstract;
     procedure FastBlendVertLine(x, y, y2: int32or64; c: TBGRAPixel); virtual; abstract;
     procedure VertLine(x,y,y2: Int32or64; c: TBGRAPixel; ADrawMode: TDrawMode);

     {Shapes}
     procedure DrawPath(APath: IBGRAPath; c: TBGRAPixel; w: single); virtual; abstract;
     procedure DrawPath(APath: IBGRAPath; texture: IBGRAScanner; w: single); virtual; abstract;

     procedure ArrowStartAsNone;
     procedure ArrowStartAsClassic(AFlipped: boolean = false; ACut: boolean = false; ARelativePenWidth: single = 1);
     procedure ArrowStartAsTriangle(ABackOffset: single = 0; ARounded: boolean = false; AHollow: boolean = false; AHollowPenWidth: single = 0.5);
     procedure ArrowStartAsTail;

     procedure ArrowEndAsNone;
     procedure ArrowEndAsClassic(AFlipped: boolean = false; ACut: boolean = false; ARelativePenWidth: single = 1);
     procedure ArrowEndAsTriangle(ABackOffset: single = 0; ARounded: boolean = false; AHollow: boolean = false; AHollowPenWidth: single = 0.5);
     procedure ArrowEndAsTail;

     procedure DrawLine(x1, y1, x2, y2: integer; c: TBGRAPixel; DrawLastPixel: boolean; ADrawMode: TDrawMode= dmDrawWithTransparency); virtual; abstract;
     procedure DrawLineAntialias(x1, y1, x2, y2: integer; c: TBGRAPixel; DrawLastPixel: boolean); virtual; abstract; overload;
     procedure DrawLineAntialias(x1, y1, x2, y2: integer; c1, c2: TBGRAPixel; dashLen: integer; DrawLastPixel: boolean); virtual; abstract; overload;
     procedure DrawLineAntialias(x1, y1, x2, y2: integer; c1, c2: TBGRAPixel; dashLen: integer; DrawLastPixel: boolean; var DashPos: integer); virtual; abstract; overload;
     procedure DrawLineAntialias(x1, y1, x2, y2: single; c: TBGRAPixel; w: single); virtual; abstract; overload;
     procedure DrawLineAntialias(x1, y1, x2, y2: single; texture: IBGRAScanner; w: single); virtual; abstract; overload;
     procedure DrawLineAntialias(x1, y1, x2, y2: single; c: TBGRAPixel; w: single; Closed: boolean); virtual; abstract; overload;
     procedure DrawLineAntialias(x1, y1, x2, y2: single; texture: IBGRAScanner; w: single; Closed: boolean); virtual; abstract; overload;

     procedure DrawPolyLine(const points: array of TPoint; c: TBGRAPixel; DrawLastPixel: boolean; ADrawMode: TDrawMode=dmDrawWithTransparency);
     procedure DrawPolyLineAntialias(const points: array of TPoint; c: TBGRAPixel; DrawLastPixel: boolean); virtual; overload;
     procedure DrawPolyLineAntialias(const points: array of TPoint; c1, c2: TBGRAPixel; dashLen: integer; DrawLastPixel: boolean); virtual; overload;
     procedure DrawPolyLineAntialias(const points: array of TPointF; c: TBGRAPixel; w: single); virtual; abstract; overload;
     procedure DrawPolyLineAntialias(const points: array of TPointF; texture: IBGRAScanner; w: single); virtual; abstract; overload;
     procedure DrawPolyLineAntialias(const points: array of TPointF; c: TBGRAPixel; w: single; Closed: boolean); virtual; abstract; overload;
     procedure DrawPolyLineAntialias(const points: array of TPointF; c: TBGRAPixel; w: single; fillcolor: TBGRAPixel); virtual; abstract; overload;
     procedure DrawPolyLineAntialiasAutocycle(const points: array of TPointF; c: TBGRAPixel; w: single); virtual; abstract; overload;
     procedure DrawPolygon(const points: array of TPoint; c: TBGRAPixel; ADrawMode: TDrawMode=dmDrawWithTransparency);
     procedure DrawPolygonAntialias(const points: array of TPoint; c: TBGRAPixel); overload;
     procedure DrawPolygonAntialias(const points: array of TPointF; c: TBGRAPixel; w: single); virtual; abstract; overload;
     procedure DrawPolygonAntialias(const points: array of TPointF; texture: IBGRAScanner; w: single); virtual; abstract; overload;
     procedure DrawPolygonAntialias(const points: array of TPointF; c: TBGRAPixel; w: single; fillcolor: TBGRAPixel); virtual; abstract; overload;

     procedure EraseLine(x1, y1, x2, y2: integer; alpha: byte; DrawLastPixel: boolean); virtual; abstract;
     procedure EraseLineAntialias(x1, y1, x2, y2: integer; alpha: byte; DrawLastPixel: boolean); virtual; abstract; overload;
     procedure EraseLineAntialias(x1, y1, x2, y2: single; alpha: byte; w: single); virtual; abstract; overload;
     procedure EraseLineAntialias(x1, y1, x2, y2: single; alpha: byte; w: single; Closed: boolean); virtual; abstract; overload;
     procedure ErasePolyLine(const points: array of TPoint; alpha: byte; DrawLastPixel: boolean);
     procedure ErasePolyLineAntialias(const points: array of TPoint; alpha: byte; DrawLastPixel: boolean); overload;
     procedure ErasePolyLineAntialias(const points: array of TPointF; alpha: byte; w: single); virtual; abstract; overload;
     procedure ErasePolygonOutline(const points: array of TPoint; alpha: byte);
     procedure ErasePolygonOutlineAntialias(const points: array of TPoint; alpha: byte);

     procedure FillPath(APath: IBGRAPath; c: TBGRAPixel); virtual; abstract;
     procedure FillPath(APath: IBGRAPath; texture: IBGRAScanner); virtual; abstract;
     procedure ErasePath(APath: IBGRAPath; alpha: byte); virtual; abstract;

     procedure FillTriangleLinearColor(pt1,pt2,pt3: TPointF; c1,c2,c3: TBGRAPixel); virtual; abstract; overload;
     procedure FillTriangleLinearColorAntialias(pt1,pt2,pt3: TPointF; c1,c2,c3: TBGRAPixel); virtual; abstract; overload;
     procedure FillTriangleLinearMapping(pt1,pt2,pt3: TPointF; texture: IBGRAScanner; tex1, tex2, tex3: TPointF; TextureInterpolation: Boolean= True); virtual; abstract; overload;
     procedure FillTriangleLinearMappingLightness(pt1,pt2,pt3: TPointF; texture: IBGRAScanner; tex1, tex2, tex3: TPointF; light1,light2,light3: word; TextureInterpolation: Boolean= True); virtual; abstract; overload;
     procedure FillTriangleLinearMappingAntialias(pt1,pt2,pt3: TPointF; texture: IBGRAScanner; tex1, tex2, tex3: TPointF); virtual; abstract; overload;

     procedure FillQuadLinearColor(pt1,pt2,pt3,pt4: TPointF; c1,c2,c3,c4: TBGRAPixel); virtual; abstract; overload;
     procedure FillQuadLinearColorAntialias(pt1,pt2,pt3,pt4: TPointF; c1,c2,c3,c4: TBGRAPixel); virtual; abstract; overload;
     procedure FillQuadLinearMapping(pt1,pt2,pt3,pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF; TextureInterpolation: Boolean= True; ACulling: TFaceCulling = fcNone);  virtual; abstract; overload;
     procedure FillQuadLinearMappingLightness(pt1,pt2,pt3,pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF; light1,light2,light3,light4: word; TextureInterpolation: Boolean= True); virtual; abstract; overload;
     procedure FillQuadLinearMappingAntialias(pt1,pt2,pt3,pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF; ACulling: TFaceCulling = fcNone); virtual; abstract; overload;
     procedure FillQuadPerspectiveMapping(pt1,pt2,pt3,pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF; ADrawMode: TDrawMode = dmDrawWithTransparency); virtual; abstract; overload;
     procedure FillQuadPerspectiveMapping(pt1,pt2,pt3,pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF; ACleanBorders: TRect; ADrawMode: TDrawMode = dmDrawWithTransparency); virtual; abstract; overload;
     procedure FillQuadPerspectiveMappingAntialias(pt1,pt2,pt3,pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF); virtual; abstract; overload;
     procedure FillQuadPerspectiveMappingAntialias(pt1,pt2,pt3,pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF; ACleanBorders: TRect); virtual; abstract; overload;
     procedure FillQuadAffineMapping(Orig,HAxis,VAxis: TPointF; AImage: TBGRACustomBitmap; APixelCenteredCoordinates: boolean = true; ADrawMode: TDrawMode = dmDrawWithTransparency); virtual; abstract;
     procedure FillQuadAffineMappingAntialias(Orig,HAxis,VAxis: TPointF; AImage: TBGRACustomBitmap; APixelCenteredCoordinates: boolean = true); virtual; abstract;

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

     procedure Arc(cx,cy,rx,ry: single; const StartPoint,EndPoint: TPointF; AColor: TBGRAPixel; w: single; ADrawChord: boolean; AFillColor: TBGRAPixel); overload;
     procedure Arc(cx,cy,rx,ry: single; StartAngleRad,EndAngleRad: Single; AColor: TBGRAPixel; w: single; ADrawChord: boolean; AFillColor: TBGRAPixel); overload;
     procedure FillChord(cx,cy,rx,ry: single; const StartPoint,EndPoint: TPointF; AFillColor: TBGRAPixel); overload;
     procedure FillChord(cx,cy,rx,ry: single; StartAngleRad,EndAngleRad: Single; AFillColor: TBGRAPixel); overload;
     procedure FillChord(cx,cy,rx,ry: single; const StartPoint,EndPoint: TPointF; texture: IBGRAScanner); overload;
     procedure FillChord(cx,cy,rx,ry: single; StartAngleRad,EndAngleRad: Single; texture: IBGRAScanner); overload;
     procedure FillChordInRect(const ARect: TRect; StartAngleRad,EndAngleRad: Single; AFillColor: TBGRAPixel); overload;
     procedure FillChordInRect(const ARect: TRect; StartAngleRad,EndAngleRad: Single; texture: IBGRAScanner); overload;

     procedure Pie(cx,cy,rx,ry: single; const StartPoint,EndPoint: TPointF; AColor: TBGRAPixel; w: single; AFillColor: TBGRAPixel); overload;
     procedure Pie(cx,cy,rx,ry: single; StartAngleRad,EndAngleRad: Single; AColor: TBGRAPixel; w: single; AFillColor: TBGRAPixel); overload;
     procedure FillPie(cx,cy,rx,ry: single; const StartPoint,EndPoint: TPointF; AFillColor: TBGRAPixel); overload;
     procedure FillPie(cx,cy,rx,ry: single; StartAngleRad,EndAngleRad: Single; AFillColor: TBGRAPixel); overload;
     procedure FillPie(cx,cy,rx,ry: single; const StartPoint,EndPoint: TPointF; texture: IBGRAScanner); overload;
     procedure FillPie(cx,cy,rx,ry: single; StartAngleRad,EndAngleRad: Single; texture: IBGRAScanner); overload;
     procedure FillPieInRect(const ARect: TRect; StartAngleRad,EndAngleRad: Single; AFillColor: TBGRAPixel); overload;
     procedure FillPieInRect(const ARect: TRect; StartAngleRad,EndAngleRad: Single; texture: IBGRAScanner); overload;

     procedure Rectangle(x, y, x2, y2: integer; c: TBGRAPixel; mode: TDrawMode); virtual; abstract; overload;
     procedure Rectangle(x, y, x2, y2: integer; BorderColor, FillColor: TBGRAPixel; mode: TDrawMode); virtual; abstract; overload;
     procedure Rectangle(x, y, x2, y2: integer; c: TColor); virtual; overload;
     procedure Rectangle(r: TRect; c: TBGRAPixel; mode: TDrawMode); virtual; overload;
     procedure Rectangle(r: TRect; BorderColor, FillColor: TBGRAPixel; mode: TDrawMode); virtual;overload;
     procedure Rectangle(r: TRect; c: TColor); virtual; overload;
     procedure RectangleAntialias(x, y, x2, y2: single; c: TBGRAPixel; w: single); virtual; overload;
     procedure RectangleAntialias(x, y, x2, y2: single; c: TBGRAPixel; w: single; back: TBGRAPixel); virtual; abstract; overload;
     procedure RectangleAntialias(x, y, x2, y2: single; texture: IBGRAScanner; w: single); virtual; abstract; overload;
     procedure RectangleWithin(x1,y1,x2,y2: single; ABorderColor: TBGRAPixel; w: single; AFillColor: TBGRAPixel; APixelCenteredCoordinates: boolean = true); overload;
     procedure RectangleWithin(r: TRect; ABorderColor: TBGRAPixel; w: single; AFillColor: TBGRAPixel); overload;

     procedure RoundRect(X1, Y1, X2, Y2: integer; DX, DY: integer; BorderColor, FillColor: TBGRAPixel; ADrawMode: TDrawMode = dmDrawWithTransparency); virtual; abstract; overload;
     procedure RoundRect(X1, Y1, X2, Y2: integer; DX, DY: integer; BorderColor: TBGRAPixel; ADrawMode: TDrawMode = dmDrawWithTransparency); virtual; abstract; overload;
     procedure RoundRectAntialias(x,y,x2,y2,rx,ry: single; c: TBGRAPixel; w: single; options: TRoundRectangleOptions = []); virtual; abstract;
     procedure RoundRectAntialias(x,y,x2,y2,rx,ry: single; pencolor: TBGRAPixel; w: single; fillcolor: TBGRAPixel; options: TRoundRectangleOptions = []); virtual; abstract;
     procedure RoundRectAntialias(x,y,x2,y2,rx,ry: single; penTexture: IBGRAScanner; w: single; fillTexture: IBGRAScanner; options: TRoundRectangleOptions = []); virtual; abstract;
     procedure RoundRectAntialias(x,y,x2,y2,rx,ry: single; texture: IBGRAScanner; w: single; options: TRoundRectangleOptions = []); virtual; abstract;
     procedure FillRoundRect(X1, Y1, X2, Y2: integer; DX, DY: integer; FillColor: TBGRAPixel; ADrawMode: TDrawMode = dmDrawWithTransparency); virtual;
     procedure FillRoundRectAntialias(x,y,x2,y2,rx,ry: single; c: TBGRAPixel; options: TRoundRectangleOptions = []; pixelCenteredCoordinates: boolean = true); virtual; abstract;
     procedure FillRoundRectAntialias(x,y,x2,y2,rx,ry: single; texture: IBGRAScanner; options: TRoundRectangleOptions = []; pixelCenteredCoordinates: boolean = true); virtual; abstract;
     procedure EraseRoundRectAntialias(x,y,x2,y2,rx,ry: single; alpha: byte; options: TRoundRectangleOptions = []; pixelCenteredCoordinates: boolean = true); virtual; abstract;

     procedure EllipseInRect(r: TRect; BorderColor: TBGRAPixel; ADrawMode: TDrawMode = dmDrawWithTransparency); virtual; overload;
     procedure EllipseInRect(r: TRect; BorderColor,FillColor: TBGRAPixel; ADrawMode: TDrawMode = dmDrawWithTransparency); virtual; overload;
     procedure FillEllipseInRect(r: TRect; FillColor: TBGRAPixel; ADrawMode: TDrawMode = dmDrawWithTransparency); virtual;

     procedure FillRect(r: TRect; c: TColor); virtual; overload;
     procedure FillRect(r: TRect; c: TBGRAPixel; mode: TDrawMode); virtual; overload;
     procedure FillRect(r: TRect; texture: IBGRAScanner; mode: TDrawMode); virtual; overload;
     procedure FillRect(x, y, x2, y2: integer; c: TColor); virtual; overload;
     procedure FillRect(x, y, x2, y2: integer; c: TBGRAPixel; mode: TDrawMode); virtual; abstract; overload;
     procedure FillRect(x, y, x2, y2: integer; texture: IBGRAScanner; mode: TDrawMode); virtual; abstract; overload;
     procedure FillRect(x, y, x2, y2: integer; texture: IBGRAScanner; mode: TDrawMode; ditheringAlgorithm: TDitheringAlgorithm); virtual; abstract; overload;
     procedure FillRectAntialias(x, y, x2, y2: single; c: TBGRAPixel; pixelCenteredCoordinates: boolean = true); virtual; abstract;
     procedure FillRectAntialias(x, y, x2, y2: single; texture: IBGRAScanner; pixelCenteredCoordinates: boolean = true); virtual; abstract;
     procedure EraseRectAntialias(x, y, x2, y2: single; alpha: byte; pixelCenteredCoordinates: boolean = true); virtual; abstract;
     procedure AlphaFillRect(x, y, x2, y2: integer; alpha: byte); virtual; abstract;

     procedure TextOut(x, y: single; sUTF8: string; c: TBGRAPixel; align: TAlignment); virtual; abstract; overload;
     procedure TextOut(x, y: single; sUTF8: string; texture: IBGRAScanner; align: TAlignment); virtual; abstract; overload;
     procedure TextOutCurved(ACursor: TBGRACustomPathCursor; sUTF8: string; AColor: TBGRAPixel; AAlign: TAlignment; ALetterSpacing: single); virtual; abstract; overload;
     procedure TextOutCurved(ACursor: TBGRACustomPathCursor; sUTF8: string; ATexture: IBGRAScanner; AAlign: TAlignment; ALetterSpacing: single); virtual; abstract; overload;
     procedure TextOutCurved(APath: IBGRAPath; sUTF8: string; AColor: TBGRAPixel; AAlign: TAlignment; ALetterSpacing: single); virtual; overload;
     procedure TextOutCurved(APath: IBGRAPath; sUTF8: string; ATexture: IBGRAScanner; AAlign: TAlignment; ALetterSpacing: single); virtual; overload;
     procedure TextOutAngle(x, y: single; orientationTenthDegCCW: integer; sUTF8: string; c: TBGRAPixel; align: TAlignment); virtual; abstract;
     procedure TextOutAngle(x, y: single; orientationTenthDegCCW: integer; sUTF8: string; texture: IBGRAScanner; align: TAlignment); virtual; abstract;
     procedure TextRect(ARect: TRect; x, y: integer; sUTF8: string; style: TTextStyle; c: TBGRAPixel); virtual; abstract; overload;
     procedure TextRect(ARect: TRect; x, y: integer; sUTF8: string; style: TTextStyle; texture: IBGRAScanner); virtual; abstract; overload;
     function TextSize(sUTF8: string): TSize; virtual; abstract;

     { Draw the UTF8 encoded string, (x,y) being the top-left corner. The color c or texture is used to fill the text.
       The value of FontOrientation is taken into account, so that the text may be rotated. }
     procedure TextOut(x, y: single; sUTF8: string; c: TBGRAPixel); virtual; overload;
     procedure TextOut(x, y: single; sUTF8: string; c: TColor); virtual; overload;
     procedure TextOut(x, y: single; sUTF8: string; texture: IBGRAScanner); virtual; overload;

     { Draw the UTF8 encoded string in the rectangle ARect. Text is wrapped if necessary.
       The position depends on the specified horizontal alignment halign and vertical alignement valign.
       The color c or texture is used to fill the text. No rotation is applied. }
     procedure TextRect(ARect: TRect; sUTF8: string; halign: TAlignment; valign: TTextLayout; c: TBGRAPixel); virtual; overload;
     procedure TextRect(ARect: TRect; sUTF8: string; halign: TAlignment; valign: TTextLayout; texture: IBGRAScanner); virtual; overload;

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
     procedure ApplyGlobalOpacity(alpha: byte); virtual; abstract; overload;
     procedure ApplyGlobalOpacity(ARect: TRect; alpha: byte); virtual; abstract; overload;
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
     procedure ReplaceColor(ARect: TRect; before, after: TColor); virtual; abstract; overload;
     procedure ReplaceColor(ARect: TRect; before, after: TBGRAPixel); virtual; abstract; overload;
     procedure ReplaceTransparent(after: TBGRAPixel); virtual; abstract; overload;
     procedure ReplaceTransparent(ABounds: TRect; after: TBGRAPixel); virtual; abstract; overload;
     procedure FloodFill(X, Y: integer; Color: TBGRAPixel;
       mode: TFloodfillMode; Tolerance: byte = 0); virtual;
     procedure ParallelFloodFill(X, Y: integer; Dest: TBGRACustomBitmap; Color: TBGRAPixel;
       mode: TFloodfillMode; Tolerance: byte = 0); virtual; abstract;
     procedure GradientFill(x, y, x2, y2: integer; c1, c2: TBGRAPixel;
       gtype: TGradientType; o1, o2: TPointF; mode: TDrawMode;
       gammaColorCorrection: boolean = True; Sinus: Boolean=False;
       ditherAlgo: TDitheringAlgorithm = daNearestNeighbor); virtual; abstract; overload;
     procedure GradientFill(x, y, x2, y2: integer; gradient: TBGRACustomGradient;
       gtype: TGradientType; o1, o2: TPointF; mode: TDrawMode;
       Sinus: Boolean=False;
       ditherAlgo: TDitheringAlgorithm = daNearestNeighbor); virtual; abstract; overload;
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
     procedure DrawPart(ARect: TRect; ACanvas: TCanvas; x, y: integer; Opaque: boolean); virtual;
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
     procedure SwapRedBlue; virtual; abstract; overload;
     procedure SwapRedBlue(ARect: TRect); virtual; abstract; overload;
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
     function FilterBlurRadial(radius: single; blurType: TRadialBlurType): TBGRACustomBitmap; virtual; abstract; overload;
     function FilterBlurRadial(ABounds: TRect; radius: single; blurType: TRadialBlurType): TBGRACustomBitmap; virtual; abstract; overload;
     function FilterBlurRadial(radiusX, radiusY: single; blurType: TRadialBlurType): TBGRACustomBitmap; virtual; abstract; overload;
     function FilterBlurRadial(ABounds: TRect; radiusX, radiusY: single; blurType: TRadialBlurType): TBGRACustomBitmap; virtual; abstract; overload;
     function FilterBlurMotion(distance: single; angle: single; oriented: boolean): TBGRACustomBitmap; virtual; abstract;
     function FilterBlurMotion(ABounds: TRect; distance: single; angle: single;
       oriented: boolean): TBGRACustomBitmap; virtual; abstract; function FilterCustomBlur(mask: TBGRACustomBitmap): TBGRACustomBitmap; virtual; abstract;
     function FilterCustomBlur(ABounds: TRect; mask: TBGRACustomBitmap): TBGRACustomBitmap; virtual; abstract;
     function FilterEmboss(angle: single; AStrength: integer= 64; AOptions: TEmbossOptions = []): TBGRACustomBitmap; virtual; abstract;
     function FilterEmboss(angle: single; ABounds: TRect; AStrength: integer= 64; AOptions: TEmbossOptions = []): TBGRACustomBitmap; virtual; abstract;
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

     //IBGRAScanner
     function ScanAtInteger(X,Y: integer): TBGRAPixel; virtual; abstract;
     function ScanAtIntegerExpanded(X,Y: integer): TExpandedPixel; virtual;
     procedure ScanMoveTo(X,Y: Integer); virtual; abstract;
     function ScanNextPixel: TBGRAPixel; virtual; abstract;
     function ScanNextExpandedPixel: TExpandedPixel; virtual;
     function ScanAt(X,Y: Single): TBGRAPixel; virtual; abstract;
     function ScanAtExpanded(X,Y: Single): TExpandedPixel; virtual;
     procedure ScanPutPixels(pdest: PBGRAPixel; count: integer; mode: TDrawMode); virtual;
     function IsScanPutPixelsDefined: boolean; virtual;
  protected
     //interface
     function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
     function _AddRef: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
     function _Release: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};

  end;

type
  TBGRABitmapAny = class of TBGRACustomBitmap;  //used to create instances of the same type (see NewBitmap)

var
  BGRABitmapFactory : TBGRABitmapAny;
{$ENDIF}

{$IFDEF INCLUDE_IMPLEMENTATION}
{$UNDEF INCLUDE_IMPLEMENTATION}
{ TBGRACustomScanner }
{ The abstract class record the position so that a derived class
  need only to redefine ScanAt }

function TBGRACustomScanner.ScanAtInteger(X, Y: integer): TBGRAPixel;
begin
  result := ScanAt(X,Y);
end;

function TBGRACustomScanner.ScanAtIntegerExpanded(X, Y: integer
  ): TExpandedPixel;
begin
  result := GammaExpansion(ScanAtInteger(X,Y));
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

function TBGRACustomScanner.ScanNextExpandedPixel: TExpandedPixel;
begin
  result := GammaExpansion(ScanNextPixel);
end;

function TBGRACustomScanner.ScanAtExpanded(X, Y: Single): TExpandedPixel;
begin
  result := GammaExpansion(ScanAt(X,Y));
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

function TBGRACustomScanner.GetTextureGL: IUnknown;
begin
  result := nil;
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

function TBGRACustomBitmap.GetTextureGL: IUnknown;
begin
  result := nil;
end;

procedure TBGRACustomBitmap.InternalArc(cx, cy, rx, ry: single;
  const StartPoint, EndPoint: TPointF; ABorderColor: TBGRAPixel; w: single; AFillColor: TBGRAPixel; AOptions: TArcOptions;
  ADrawChord: boolean; ATexture: IBGRAScanner);
var angle1,angle2: single;
begin
  if (rx = 0) or (ry = 0) then exit;
  angle1 := arctan2(-(StartPoint.y-cy)/ry,(StartPoint.x-cx)/rx);
  angle2 := arctan2(-(EndPoint.y-cy)/ry,(EndPoint.x-cx)/rx);
  if angle1 = angle2 then angle2 := angle1+2*Pi;
  InternalArc(cx,cy,rx,ry, angle1,angle2,
              ABorderColor,w,AFillColor, AOptions, ADrawChord, ATexture);
end;

procedure TBGRACustomBitmap.InternalArcInRect(r: TRect; StartAngleRad,
  EndAngleRad: Single; ABorderColor: TBGRAPixel; w: single; AFillColor: TBGRAPixel; AOptions: TArcOptions;
  ADrawChord: boolean; ATexture: IBGRAScanner);
var
  temp: LongInt;
begin
  if r.right = r.left then exit;
  if r.bottom = r.top then exit;
  if r.right < r.left then
  begin
    temp := r.left;
    r.left := r.right;
    r.right := temp;
  end;
  if r.Bottom < r.Top then
  begin
    temp := r.Top;
    r.Top := r.Bottom;
    r.Bottom := temp;
  end;
  InternalArc((r.left+r.right-1)/2,(r.top+r.bottom-1)/2,
             (r.right-r.left-1)/2,(r.bottom-r.top-1)/2,
             StartAngleRad,EndAngleRad,
             ABorderColor,w,AFillColor,
             AOptions, ADrawChord, ATexture);
end;

procedure TBGRACustomBitmap.InternalFillArcInRect(r: TRect; StartAngleRad,
  EndAngleRad: Single; AFillColor: TBGRAPixel; AOptions: TArcOptions;
  ATexture: IBGRAScanner);
var
  temp: LongInt;
begin
  if r.right = r.left then exit;
  if r.bottom = r.top then exit;
  if r.right < r.left then
  begin
    temp := r.left;
    r.left := r.right;
    r.right := temp;
  end;
  if r.Bottom < r.Top then
  begin
    temp := r.Top;
    r.Top := r.Bottom;
    r.Bottom := temp;
  end;
  InternalArc((r.left+r.right-1)/2,(r.top+r.bottom-1)/2,
             (r.right-r.left)/2,(r.bottom-r.top)/2,
             StartAngleRad,EndAngleRad,
             BGRAPixelTransparent,0,AFillColor,
             AOptions, False, ATexture);
end;

{ These declaration make sure that these methods are virtual }
procedure TBGRACustomBitmap.LoadFromFile(const filename: string);
begin
  LoadFromFileUTF8(SysToUtf8(filename));
end;

procedure TBGRACustomBitmap.LoadFromFile(const filename: String;
  Handler: TFPCustomImageReader);
begin
  LoadFromFileUTF8(SysToUtf8(filename),Handler);
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
  if (format = ifXPixMap) and (Width*Height > 32768) then //xpm is slow so avoid big images
    raise exception.Create('Image is too big to be saved as XPM');
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

procedure TBGRACustomBitmap.SaveToStreamAsPng(Str: TStream);
var writer: TFPCustomImageWriter;
begin
  writer := CreateBGRAImageWriter(ifPNG, HasTransparentPixels);
  SaveToStream(Str,writer);
  writer.Free;
end;

procedure TBGRACustomBitmap.SaveToStreamAs(Str: TStream;
  AFormat: TBGRAImageFormat);
var handler: TFPCustomImageWriter;
begin
  handler := CreateBGRAImageWriter(AFormat, HasTransparentPixels);
  try
    SaveToStream(Str, handler)
  finally
    handler.Free;
  end;
end;

procedure TBGRACustomBitmap.DrawPixel(x, y: int32or64; c: TBGRAPixel;
  ADrawMode: TDrawMode);
begin
  case ADrawMode of
  dmSet: SetPixel(x,y,c);
  dmSetExceptTransparent: if c.alpha = 255 then SetPixel(x,y,c);
  dmLinearBlend: FastBlendPixel(x,y,c);
  dmDrawWithTransparency: DrawPixel(x,y,c);
  dmXor: XorPixel(x,y,c);
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

procedure TBGRACustomBitmap.DrawHorizLine(x, y, x2: int32or64;
  texture: IBGRAScanner);
begin
  HorizLine(x,y,x2,texture,dmDrawWithTransparency);
end;

procedure TBGRACustomBitmap.HorizLine(x, y, x2: Int32or64; c: TBGRAPixel;
  ADrawMode: TDrawMode);
begin
  case ADrawMode of
    dmSet: SetHorizLine(x,y,x2,c);
    dmSetExceptTransparent: if c.alpha = 255 then SetHorizLine(x,y,x2,c);
    dmXor: XorHorizLine(x,y,x2,c);
    dmLinearBlend: FastBlendHorizLine(x,y,x2,c);
    dmDrawWithTransparency: DrawHorizLine(x,y,x2,c);
  end;
end;

procedure TBGRACustomBitmap.VertLine(x, y, y2: Int32or64; c: TBGRAPixel;
  ADrawMode: TDrawMode);
begin
  case ADrawMode of
    dmSet: SetVertLine(x,y,y2,c);
    dmSetExceptTransparent: if c.alpha = 255 then SetVertLine(x,y,y2,c);
    dmXor: XorVertLine(x,y,y2,c);
    dmLinearBlend: FastBlendVertLine(x,y,y2,c);
    dmDrawWithTransparency: DrawVertLine(x,y,y2,c);
  end;
end;

procedure TBGRACustomBitmap.ArrowStartAsNone;
begin
  SetArrowStart(asNone);
end;

procedure TBGRACustomBitmap.ArrowStartAsClassic(AFlipped: boolean; ACut: boolean; ARelativePenWidth: single);
var join: TPenJoinStyle;
begin
  if (LineCap = pecRound) and not ACut then join := pjsRound else join := pjsMiter;
  if ACut then
  begin
    if AFlipped then
      SetArrowStart(asFlippedCut,join,ARelativePenWidth)
    else
      SetArrowStart(asCut,join,ARelativePenWidth)
  end
  else
  begin
    if AFlipped then
      SetArrowStart(asFlipped,join,ARelativePenWidth)
    else
      SetArrowStart(asNormal,join,ARelativePenWidth)
  end;
end;

procedure TBGRACustomBitmap.ArrowStartAsTriangle(ABackOffset: single; ARounded: boolean; AHollow: boolean;
  AHollowPenWidth: single);
var join: TPenJoinStyle;
begin
  if ARounded then join := pjsRound else join := pjsMiter;
  if AHollow then
    SetArrowStart(asHollowTriangle, join,AHollowPenWidth, ABackOffset)
  else
    SetArrowStart(asTriangle, join,1,ABackOffset);
end;

procedure TBGRACustomBitmap.ArrowStartAsTail;
begin
  SetArrowStart(asTail);
end;

procedure TBGRACustomBitmap.ArrowEndAsNone;
begin
  SetArrowEnd(asNone);
end;

procedure TBGRACustomBitmap.ArrowEndAsClassic(AFlipped: boolean; ACut: boolean; ARelativePenWidth: single);
var join: TPenJoinStyle;
begin
  if (LineCap = pecRound) and not ACut then join := pjsRound else join := pjsMiter;
  if ACut then
  begin
    if AFlipped then
      SetArrowEnd(asFlippedCut,join,ARelativePenWidth)
    else
      SetArrowEnd(asCut,join,ARelativePenWidth)
  end
  else
  begin
    if AFlipped then
      SetArrowEnd(asFlipped,join,ARelativePenWidth)
    else
      SetArrowEnd(asNormal,join,ARelativePenWidth)
  end;
end;

procedure TBGRACustomBitmap.ArrowEndAsTriangle(ABackOffset: single; ARounded: boolean; AHollow: boolean;
  AHollowPenWidth: single);
var join: TPenJoinStyle;
begin
  if ARounded then join := pjsRound else join := pjsMiter;
  if AHollow then
    SetArrowEnd(asHollowTriangle, join,AHollowPenWidth, ABackOffset)
  else
    SetArrowEnd(asTriangle, join,1, ABackOffset);
end;

procedure TBGRACustomBitmap.ArrowEndAsTail;
begin
  SetArrowEnd(asTail);
end;

procedure TBGRACustomBitmap.DrawPolyLine(const points: array of TPoint;
  c: TBGRAPixel; DrawLastPixel: boolean; ADrawMode: TDrawMode);
var i: integer;
begin
   if length(points) = 1 then
   begin
     if DrawLastPixel then DrawPixel(points[0].x,points[0].y,c,ADrawMode);
   end
   else
     for i := 0 to high(points)-1 do
       DrawLine(points[i].x,points[i].Y,points[i+1].x,points[i+1].y,c,DrawLastPixel and (i=high(points)-1),ADrawMode);
end;

{ Pixel polylines are constructed by concatenation }
procedure TBGRACustomBitmap.DrawPolyLineAntialias(const points: array of TPoint;
  c: TBGRAPixel; DrawLastPixel: boolean);
var i: integer;
begin
   if length(points) = 1 then
   begin
     if DrawLastPixel then DrawLineAntialias(points[0].x,points[0].y,points[0].x,points[0].y,c,true);
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

procedure TBGRACustomBitmap.DrawPolygon(const points: array of TPoint;
  c: TBGRAPixel; ADrawMode: TDrawMode);
var i: integer;
begin
   if length(points) = 1 then
   begin
     DrawPixel(points[0].x,points[0].y,c,ADrawMode);
   end
   else
   begin
     for i := 0 to high(points)-1 do
       DrawLine(points[i].x,points[i].Y,points[i+1].x,points[i+1].y,c,false,ADrawMode);
     DrawLine(points[high(points)].x,points[high(points)].Y,points[0].x,points[0].y,c,false,ADrawMode);
   end;
end;

procedure TBGRACustomBitmap.DrawPolygonAntialias(const points: array of TPoint;
  c: TBGRAPixel);
var i: integer;
begin
   if length(points) = 1 then
   begin
     DrawLineAntialias(points[0].x,points[0].y,points[0].x,points[0].y,c,true);
   end
   else
   begin
     for i := 0 to high(points)-1 do
       DrawLineAntialias(points[i].x,points[i].Y,points[i+1].x,points[i+1].y,c,false);
     DrawLineAntialias(points[high(points)].x,points[high(points)].Y,points[0].x,points[0].y,c,false);
   end;
end;

procedure TBGRACustomBitmap.ErasePolyLine(const points: array of TPoint; alpha: byte;
  DrawLastPixel: boolean);
var i: integer;
begin
   if length(points) = 1 then
   begin
     if DrawLastPixel then ErasePixel(points[0].x,points[0].y,alpha);
   end
   else
     for i := 0 to high(points)-1 do
       EraseLine(points[i].x,points[i].Y,points[i+1].x,points[i+1].y,alpha,DrawLastPixel and (i=high(points)-1));
end;

procedure TBGRACustomBitmap.ErasePolyLineAntialias(
  const points: array of TPoint; alpha: byte; DrawLastPixel: boolean);
var i: integer;
begin
   if length(points) = 1 then
   begin
     if DrawLastPixel then ErasePixel(points[0].x,points[0].y,alpha);
   end
   else
     for i := 0 to high(points)-1 do
       EraseLineAntialias(points[i].x,points[i].Y,points[i+1].x,points[i+1].y,alpha,DrawLastPixel and (i=high(points)-1));
end;

procedure TBGRACustomBitmap.ErasePolygonOutline(const points: array of TPoint;
  alpha: byte);
var i: integer;
begin
   if length(points) = 1 then
   begin
     ErasePixel(points[0].x,points[0].y,alpha);
   end
   else
   begin
     for i := 0 to high(points)-1 do
       EraseLine(points[i].x,points[i].Y,points[i+1].x,points[i+1].y,alpha,false);
     EraseLine(points[high(points)].x,points[high(points)].Y,points[0].x,points[0].y,alpha,false);
   end;
end;

procedure TBGRACustomBitmap.ErasePolygonOutlineAntialias(
  const points: array of TPoint; alpha: byte);
var i: integer;
begin
   if length(points) = 1 then
   begin
     ErasePixel(points[0].x,points[0].y,alpha);
   end
   else
   begin
     for i := 0 to high(points)-1 do
       EraseLineAntialias(points[i].x,points[i].Y,points[i+1].x,points[i+1].y,alpha,false);
     EraseLineAntialias(points[high(points)].x,points[high(points)].Y,points[0].x,points[0].y,alpha,false);
   end;
end;

procedure TBGRACustomBitmap.Arc(cx, cy, rx, ry: single; const StartPoint,
  EndPoint: TPointF; AColor: TBGRAPixel; w: single; ADrawChord: boolean;
  AFillColor: TBGRAPixel);
begin
  InternalArc(cx,cy,rx,ry,StartPoint,EndPoint,AColor,w,AFillColor,[aoFillPath],ADrawChord);
end;

procedure TBGRACustomBitmap.Arc(cx, cy, rx, ry: single; StartAngleRad,
  EndAngleRad: Single; AColor: TBGRAPixel; w: single; ADrawChord: boolean;
  AFillColor: TBGRAPixel);
begin
  InternalArc(cx,cy,rx,ry,StartAngleRad,EndAngleRad,AColor,w,AFillColor,[aoFillPath],ADrawChord);
end;

procedure TBGRACustomBitmap.FillChord(cx, cy, rx, ry: single; const StartPoint,
  EndPoint: TPointF; AFillColor: TBGRAPixel);
begin
  InternalArc(cx,cy,rx,ry,StartPoint,EndPoint,BGRAPixelTransparent,0,AFillColor,[aoFillPath]);
end;

procedure TBGRACustomBitmap.FillChord(cx, cy, rx, ry: single; StartAngleRad,
  EndAngleRad: Single; AFillColor: TBGRAPixel);
begin
  InternalArc(cx,cy,rx,ry,StartAngleRad,EndAngleRad,BGRAPixelTransparent,0,AFillColor,[aoFillPath]);
end;

procedure TBGRACustomBitmap.FillChord(cx, cy, rx, ry: single; const StartPoint,
  EndPoint: TPointF; texture: IBGRAScanner);
begin
  InternalArc(cx,cy,rx,ry,StartPoint,EndPoint,BGRAPixelTransparent,0,BGRAWhite,[aoFillPath],False,texture);
end;

procedure TBGRACustomBitmap.FillChord(cx, cy, rx, ry: single; StartAngleRad,
  EndAngleRad: Single; texture: IBGRAScanner);
begin
  InternalArc(cx,cy,rx,ry,StartAngleRad,EndAngleRad,BGRAPixelTransparent,0,BGRAWhite,[aoFillPath],False,texture);
end;

procedure TBGRACustomBitmap.FillChordInRect(const ARect: TRect; StartAngleRad,
  EndAngleRad: Single; AFillColor: TBGRAPixel);
begin
  InternalFillArcInRect(ARect,StartAngleRad,EndAngleRad,AFillColor,[aoFillPath]);
end;

procedure TBGRACustomBitmap.FillChordInRect(const ARect: TRect; StartAngleRad,
  EndAngleRad: Single; texture: IBGRAScanner);
begin
  InternalFillArcInRect(ARect,StartAngleRad,EndAngleRad,BGRAWhite,[aoFillPath],texture);
end;

procedure TBGRACustomBitmap.Pie(cx, cy, rx, ry: single; const StartPoint,
  EndPoint: TPointF; AColor: TBGRAPixel; w: single; AFillColor: TBGRAPixel);
begin
  InternalArc(cx,cy,rx,ry,StartPoint,EndPoint,AColor,w,AFillColor,[aoFillPath,aoPie]);
end;

procedure TBGRACustomBitmap.Pie(cx, cy, rx, ry: single; StartAngleRad,
  EndAngleRad: Single; AColor: TBGRAPixel; w: single; AFillColor: TBGRAPixel);
begin
  InternalArc(cx,cy,rx,ry,StartAngleRad,EndAngleRad,AColor,w,AFillColor,[aoFillPath,aoPie]);
end;

procedure TBGRACustomBitmap.FillPie(cx, cy, rx, ry: single; const StartPoint,
  EndPoint: TPointF; AFillColor: TBGRAPixel);
begin
  InternalArc(cx,cy,rx,ry,StartPoint,EndPoint,BGRAPixelTransparent,0,AFillColor,[aoFillPath,aoPie]);
end;

procedure TBGRACustomBitmap.FillPie(cx, cy, rx, ry: single; StartAngleRad,
  EndAngleRad: Single; AFillColor: TBGRAPixel);
begin
  InternalArc(cx,cy,rx,ry,StartAngleRad,EndAngleRad,BGRAPixelTransparent,0,AFillColor,[aoFillPath,aoPie]);
end;

procedure TBGRACustomBitmap.FillPie(cx, cy, rx, ry: single; const StartPoint,
  EndPoint: TPointF; texture: IBGRAScanner);
begin
  InternalArc(cx,cy,rx,ry,StartPoint,EndPoint,BGRAPixelTransparent,0,BGRAWhite,[aoFillPath,aoPie],False,texture);
end;

procedure TBGRACustomBitmap.FillPie(cx, cy, rx, ry: single; StartAngleRad,
  EndAngleRad: Single; texture: IBGRAScanner);
begin
  InternalArc(cx,cy,rx,ry,StartAngleRad,EndAngleRad,BGRAPixelTransparent,0,BGRAWhite,[aoFillPath,aoPie],False,texture);
end;

procedure TBGRACustomBitmap.FillPieInRect(const ARect: TRect; StartAngleRad,
  EndAngleRad: Single; AFillColor: TBGRAPixel);
begin
  InternalFillArcInRect(ARect,StartAngleRad,EndAngleRad,AFillColor,[aoFillPath,aoPie]);
end;

procedure TBGRACustomBitmap.FillPieInRect(const ARect: TRect; StartAngleRad,
  EndAngleRad: Single; texture: IBGRAScanner);
begin
  InternalFillArcInRect(ARect,StartAngleRad,EndAngleRad,BGRAWhite,[aoFillPath,aoPie],texture);
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

procedure TBGRACustomBitmap.RectangleWithin(x1, y1, x2, y2: single;
  ABorderColor: TBGRAPixel; w: single; AFillColor: TBGRAPixel;
  APixelCenteredCoordinates: boolean);
begin
  if not APixelCenteredCoordinates then
  begin
    x1 -= 0.5;
    y1 -= 0.5;
    x2 -= 0.5;
    y2 -= 0.5;
  end;
  RectangleAntialias(x1+w*0.5,y1+w*0.5,x2-w*0.5,y2-w*0.5, ABorderColor, w, AFillColor);
end;

procedure TBGRACustomBitmap.RectangleWithin(r: TRect; ABorderColor: TBGRAPixel;
  w: single; AFillColor: TBGRAPixel);
begin
  RectangleWithin(r.left,r.top,r.right,r.bottom,ABorderColor,w,AFillColor,false);
end;

procedure TBGRACustomBitmap.FillRoundRect(X1, Y1, X2, Y2: integer; DX,
  DY: integer; FillColor: TBGRAPixel; ADrawMode: TDrawMode);
begin
  RoundRect(X1,Y1,X2,Y2,DX,DY,FillColor,FillColor,ADrawMode);
end;

procedure TBGRACustomBitmap.EllipseInRect(r: TRect; BorderColor: TBGRAPixel;
  ADrawMode: TDrawMode);
begin
  RoundRect(r.left,r.top,r.right,r.bottom,abs(r.right-r.left),abs(r.bottom-r.top),BorderColor,ADrawMode);
end;

procedure TBGRACustomBitmap.EllipseInRect(r: TRect; BorderColor,
  FillColor: TBGRAPixel; ADrawMode: TDrawMode);
begin
  RoundRect(r.left,r.top,r.right,r.bottom,abs(r.right-r.left),abs(r.bottom-r.top),BorderColor,FillColor,ADrawMode);
end;

procedure TBGRACustomBitmap.FillEllipseInRect(r: TRect; FillColor: TBGRAPixel;
  ADrawMode: TDrawMode);
begin
  FillRoundRect(r.left,r.top,r.right,r.bottom,abs(r.right-r.left),abs(r.bottom-r.top),FillColor,ADrawMode);
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

procedure TBGRACustomBitmap.TextOutCurved(APath: IBGRAPath; sUTF8: string;
  AColor: TBGRAPixel; AAlign: TAlignment; ALetterSpacing: single);
var cursor: TBGRACustomPathCursor;
begin
  cursor := APath.getCursor;
  if cursor = nil then exit;
  case AAlign of
    taCenter: cursor.Position := cursor.PathLength*0.5;
    taRightJustify: cursor.Position:= cursor.PathLength;
  end;
  TextOutCurved(cursor, sUTF8, AColor, AAlign, ALetterSpacing);
  cursor.free;
end;

procedure TBGRACustomBitmap.TextOutCurved(APath: IBGRAPath; sUTF8: string;
  ATexture: IBGRAScanner; AAlign: TAlignment; ALetterSpacing: single);
var cursor: TBGRACustomPathCursor;
begin
  cursor := APath.getCursor;
  if cursor = nil then exit;
  case AAlign of
    taCenter: cursor.Position := cursor.PathLength*0.5;
    taRightJustify: cursor.Position:= cursor.PathLength;
  end;
  TextOutCurved(cursor, sUTF8, ATexture, AAlign, ALetterSpacing);
  cursor.free;
end;

{ Draw the UTF8 encoded string, (x,y) being the top-left corner. The color c is used to fill the text.
  The value of FontOrientation is taken into account, so that the text may be rotated. }
procedure TBGRACustomBitmap.TextOut(x, y: single; sUTF8: string; c: TBGRAPixel);
begin
  TextOut(x, y, sUTF8, c, taLeftJustify);
end;

{ Draw the UTF8 encoded string, (x,y) being the top-left corner. The color c is used to fill the text.
  The value of FontOrientation is taken into account, so that the text may be rotated. }
procedure TBGRACustomBitmap.TextOut(x, y: single; sUTF8: string; c: TColor);
begin
  TextOut(x, y, sUTF8, ColorToBGRA(c));
end;

{ Draw the UTF8 encoded string, (x,y) being the top-left corner. The texture is used to fill the text.
  The value of FontOrientation is taken into account, so that the text may be rotated. }
procedure TBGRACustomBitmap.TextOut(x, y: single; sUTF8: string;
  texture: IBGRAScanner);
begin
  TextOut(x, y, sUTF8, texture, taLeftJustify);
end;

{ Draw the UTF8 encoded string in the rectangle ARect. Text is wrapped if necessary.
  The position depends on the specified horizontal alignment halign and vertical alignement valign.
  The color c is used to fill the text. No rotation is applied. }
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

{ Draw the UTF8 encoded string in the rectangle ARect. Text is wrapped if necessary.
  The position depends on the specified horizontal alignment halign and vertical alignement valign.
  The texture is used to fill the text. No rotation is applied. }
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

procedure TBGRACustomBitmap.DrawPart(ARect: TRect; ACanvas: TCanvas; x,
  y: integer; Opaque: boolean);
var
  partial: TBGRACustomBitmap;
begin
  partial := GetPart(ARect);
  if partial <> nil then
  begin
    partial.Draw(ACanvas, x, y, Opaque);
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

function TBGRACustomBitmap.ScanAtIntegerExpanded(X, Y: integer): TExpandedPixel;
begin
  result := GammaExpansion(ScanAtInteger(X,Y));
end;

function TBGRACustomBitmap.ScanNextExpandedPixel: TExpandedPixel;
begin
  result := GammaExpansion(ScanNextPixel);
end;

function TBGRACustomBitmap.ScanAtExpanded(X, Y: Single): TExpandedPixel;
begin
  result := GammaExpansion(ScanAt(X,Y));
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
{$ENDIF}
