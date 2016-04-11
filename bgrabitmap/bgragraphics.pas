unit BGRAGraphics;
{=== Types imported from Graphics ===}
{$mode objfpc}{$H+}

interface

{$I bgrabitmap.inc}

{$IFDEF BGRABITMAP_USE_LCL}
uses Graphics, GraphType, FPImage;

type
  PColor = Graphics.PColor;
  TColor = Graphics.TColor;
  TAntialiasingMode = Graphics.TAntialiasingMode;
  TGradientDirection = Graphics.TGradientDirection;
  TPenEndCap = Graphics.TPenEndCap;
  TPenJoinStyle = Graphics.TPenJoinStyle;
  TPenStyle = Graphics.TPenStyle;

const
  amDontCare = Graphics.amDontCare;
  amOn = Graphics.amOn;
  amOff = Graphics.amOff;

  gdVertical = Graphics.gdVertical;
  gdHorizontal = Graphics.gdHorizontal;

  pecRound = Graphics.pecRound;
  pecSquare = Graphics.pecSquare;
  pecFlat = Graphics.pecFlat;

  pjsRound = Graphics.pjsRound;
  pjsBevel = Graphics.pjsBevel;
  pjsMiter = Graphics.pjsMiter;

  psSolid = Graphics.psSolid;
  psDash = Graphics.psDash;
  psDot = Graphics.psDot;
  psDashDot = Graphics.psDashDot;
  psDashDotDot = Graphics.psDashDotDot;
  psClear = Graphics.psClear;
  psInsideframe = Graphics.psInsideframe;
  psPattern = Graphics.psPattern;

  tmAuto = Graphics.tmAuto;
  tmFixed = Graphics.tmFixed;

type
  TPen = Graphics.TPen;
  TTextLayout = Graphics.TTextLayout;
  TTextStyle = Graphics.TTextStyle;

  TFillStyle = Graphics.TFillStyle;
  TFillMode = Graphics.TFillMode;
  TBrushStyle = Graphics.TBrushStyle;

const
  tlTop = Graphics.tlTop;
  tlCenter = Graphics.tlCenter;
  tlBottom = Graphics.tlBottom;

  fsSurface = GraphType.fsSurface;
  fsBorder = GraphType.fsBorder;

  fmAlternate = Graphics.fmAlternate;
  fmWinding = Graphics.fmWinding;

  bsSolid = Graphics.bsSolid;
  bsClear = Graphics.bsClear;
  bsHorizontal = Graphics.bsHorizontal;
  bsVertical = Graphics.bsVertical;
  bsFDiagonal = Graphics.bsFDiagonal;
  bsBDiagonal = Graphics.bsBDiagonal;
  bsCross = Graphics.bsCross;
  bsDiagCross = Graphics.bsDiagCross;

type
  TBrush = Graphics.TBrush;
  TCanvas = Graphics.TCanvas;
  TGraphic = Graphics.TGraphic;
  TRawImage = GraphType.TRawImage;
  TBitmap = Graphics.TBitmap;

  TRasterImage = Graphics.TRasterImage;

  TFontStyle = Graphics.TFontStyle;
  TFontStyles = Graphics.TFontStyles;
  TFontQuality = Graphics.TFontQuality;

type
  TFont = Graphics.TFont;

const
  fsBold = Graphics.fsBold;
  fsItalic = Graphics.fsItalic;
  fsStrikeOut = Graphics.fsStrikeOut;
  fsUnderline = Graphics.fsUnderline;

  fqDefault = Graphics.fqDefault;
  fqDraft = Graphics.fqDraft;
  fqProof = Graphics.fqProof;
  fqNonAntialiased = Graphics.fqNonAntialiased;
  fqAntialiased = Graphics.fqAntialiased;
  fqCleartype = Graphics.fqCleartype;
  fqCleartypeNatural = Graphics.fqCleartypeNatural;

  clNone = Graphics.clNone;

  clBlack   = Graphics.clBlack;
  clMaroon  = Graphics.clMaroon;
  clGreen   = Graphics.clGreen;
  clOlive   = Graphics.clOlive;
  clNavy    = Graphics.clNavy;
  clPurple  = Graphics.clPurple;
  clTeal    = Graphics.clTeal;
  clGray    = Graphics.clGray;
  clSilver  = Graphics.clSilver;
  clRed     = Graphics.clRed;
  clLime    = Graphics.clLime;
  clYellow  = Graphics.clYellow;
  clBlue    = Graphics.clBlue;
  clFuchsia = Graphics.clFuchsia;
  clAqua    = Graphics.clAqua;
  clLtGray  = Graphics.clLtGray; // clSilver alias
  clDkGray  = Graphics.clDkGray; // clGray alias
  clWhite   = Graphics.clWhite;

function FPColorToTColor(const FPColor: TFPColor): TColor; inline;
function TColorToFPColor(const c: TColor): TFPColor; inline;
function ColorToRGB(c: TColor): TColor; inline;
function RGBToColor(R, G, B: Byte): TColor; inline;
procedure RedGreenBlue(rgb: TColor; out Red, Green, Blue: Byte); inline;// does not work on system color
function clRgbBtnHighlight: TColor;
function clRgbBtnShadow: TColor;

implementation

function FPColorToTColor(const FPColor: TFPColor): TColor;
begin
  result := Graphics.FPColorToTColor(FPColor);
end;

function TColorToFPColor(const c: TColor): TFPColor;
begin
  result := Graphics.TColorToFPColor(c);
end;

function ColorToRGB(c: TColor): TColor;
begin
  result := Graphics.ColorToRGB(c);
end;

function RGBToColor(R, G, B: Byte): TColor;
begin
  result := Graphics.RGBToColor(R, G, B);
end;

procedure RedGreenBlue(rgb: TColor; out Red, Green, Blue: Byte);
begin
  Graphics.RedGreenBlue(rgb, Red, Green, Blue);
end;

function clRgbBtnHighlight: TColor;
begin
  result := Graphics.ColorToRGB(clBtnHighlight);
end;

function clRgbBtnShadow: TColor;
begin
  result := Graphics.ColorToRGB(clBtnShadow);
end;

{$ELSE}

uses
  Classes, FPCanvas, FPImage
  {$DEFINE INCLUDE_USES}
  {$IFDEF BGRABITMAP_USE_FPGUI}
    {$i bgrafpgui.inc}
  {$ELSE}
    {$i bgranogui.inc}
  {$ENDIF}
;

{$DEFINE INCLUDE_INTERFACE}
{$IFDEF BGRABITMAP_USE_FPGUI}
  {$i bgrafpgui.inc}
{$ELSE}
  {$i bgranogui.inc}
{$ENDIF}

type
  {* Pointer to a ''TColor'' value }
  PColor = ^TColor;
  {* Contains a color stored as RGB. The red/green/blue values
   range from 0 to 255. The formula to get the color value is:
   * ''color'' = ''red'' + (''green'' '''shl''' 8) + (''blue'' '''shl''' 16)
   *except with fpGUI where it is:
   * ''color'' = (''red'' '''shl''' 16) + (''green'' '''shl''' 8) + ''blue'' }{import
  TColor = Int32;
  }
  {** Converts a ''TFPColor'' into a ''TColor'' value }
  function FPColorToTColor(const FPColor: TFPColor): TColor;
  {** Converts a ''TColor'' into a ''TFPColor'' value }
  function TColorToFPColor(const c: TColor): TFPColor;

type
  {* Direction of change in a gradient }
  TGradientDirection = (
    {** Color changes vertically }
    gdVertical,
    {** Color changes horizontally }
    gdHorizontal);

  {* Antialiasing mode for a Canvas }
  TAntialiasingMode = (
    {** It does not matter if there is antialiasing or not }
    amDontCare,
    {** Antialiasing is required (BGRACanvas provide it) }
    amOn,
    {** Antialiasing is disabled }
    amOff);

  {* How to draw the end of line }
  TPenEndCap = TFPPenEndCap;

const
    {** Draw a half-disk at the end of the line. The diameter of the disk is
        equal to the pen width. }
    pecRound = FPCanvas.pecRound;
    {** Draw a half-square. The size of the square is equal to the pen width.
        This is visually equivalent to extend the line of half the pen width }
    pecSquare = FPCanvas.pecSquare;
    {** The line ends exactly at the end point }
    pecFlat = FPCanvas.pecFlat;

type
  {* How to join segments. This makes sense only for geometric pens (that
     have a certain width) }
  TPenJoinStyle = TFPPenJoinStyle;

const
    {** Segments are joined by filling the gap with an arc }
    pjsRound = FPCanvas.pjsRound;
    {** Segments are joind by filling the gap with an intermediary segment }
    pjsBevel = FPCanvas.pjsBevel;
    {** Segments are joined by extending them up to their intersection.
        There is a miter limit so that if the intersection is too far,
        an intermediary segment is used }
    pjsMiter = FPCanvas.pjsMiter;

type
  {* Style to use for the pen. The unit for the pattern is the width of the
     line }
  TPenStyle = TFPPenStyle;

const
  {** Pen is continuous }
  psSolid = FPCanvas.psSolid;
  {** Pen is dashed. The dash have a length of 3 unit and the gaps of 1 unit }
  psDash = FPCanvas.psDash;
  {** Pen is dotted. The dots have a length of 1 unit and the gaps of 1 unit }
  psDot = FPCanvas.psDot;
  {** Pattern is a dash of length 3 followed by a dot of length 1, separated by a gap of length 1 }
  psDashDot = FPCanvas.psDashDot;
  {** Dash of length 3, and two dots of length 1 }
  psDashDotDot = FPCanvas.psDashDotDot;
  {** Pen is not drawn }
  psClear = FPCanvas.psClear;
  {** Not used. Provided for compatibility }
  psInsideframe = FPCanvas.psInsideframe;
  {** Custom pattern used }
  psPattern = FPCanvas.psPattern;

type
  TTransparentMode = (
    tmAuto,
    tmFixed
    );

  { TPen }
  {* A class containing a pen }
  TPen = class(TFPCustomPen)
  private
    FEndCap: TPenEndCap;
    FJoinStyle: TPenJoinStyle;
    function GetColor: TColor;
    procedure SetColor(AValue: TColor);
  public
    constructor Create; override;
    {** Color of the pen }
    property Color: TColor read GetColor write SetColor;
    {** End cap of the pen: how to draw the ends of the lines }
    property EndCap;
    {** Join style: how to join the segments of a polyline }
    property JoinStyle;
    {** Pen style: solid, dash, dot... }{inherited
    property Style : TPenStyle read write;
   }{** Pen width in pixels }{inherited
    property Width : Integer read write;
   }
  end;

type
  {* Vertical position of a text }
  TTextLayout = (tlTop, tlCenter, tlBottom);
  {* Styles to describe how a text is drawn in a rectangle }
  TTextStyle = packed record
    {** Horizontal alignment }
    Alignment : TAlignment;

    {** Vertical alignment }
    Layout    : TTextLayout;

    {** If WordBreak is false then process #13, #10 as
        standard chars and perform no Line breaking }
    SingleLine: boolean;

    {** Clip Text to passed Rectangle }
    Clipping  : boolean;

    {** Replace #9 by apropriate amount of spaces (default is usually 8) }
    ExpandTabs: boolean;

    {** Process first single '&' per line as an underscore and draw '&&' as '&' }
    ShowPrefix: boolean;

    {** If line of text is too long too fit between left and right boundaries
        try to break into multiple lines between words. See also ''EndEllipsis'' }
    Wordbreak : boolean;

    {** Fills background with current brush }
    Opaque    : boolean;

    {** Use the system font instead of canvas font }
    SystemFont: Boolean;

    {** For RightToLeft text reading (Text Direction) }
    RightToLeft: Boolean;

    {** If line of text is too long to fit between left and right boundaries
        truncates the text and adds "...". If Wordbreak is set as well,
        Workbreak will dominate }
    EndEllipsis: Boolean;
  end;

  {* Option for floodfill (used in BGRACanvas) }
  TFillStyle =
    (
      {** Fill up to the color (it fills all except the specified color) }
      fsSurface,
      {** Fill the specified color (it fills only connected pixels of this color) }
      fsBorder
    );
  {* How to handle polygons that intersect with themselves and
     overlapping polygons }
  TFillMode = (
    {** Each time a boundary is found, it enters or exit the filling zone }
    fmAlternate,
    {** Adds or subtract 1 depending on the order of the points of the
        polygons (clockwise or counter clockwise) and fill when the
        result is non-zero. So, to draw a hole, you must specify the points
        of the hole in the opposite order }
    fmWinding);

  {* Pattern when filling with a brush. It is used in BGRACanvas but can
     also be created with TBGRABitmap.CreateBrushTexture function }
  TBrushStyle = TFPBrushStyle;

const
  {** Fill with the current color }
  bsSolid = FPCanvas.bsSolid;
  {** Does not fill at all }
  bsClear = FPCanvas.bsClear;
  {** Draw horizontal lines }
  bsHorizontal = FPCanvas.bsHorizontal;
  {** Draw vertical lines }
  bsVertical = FPCanvas.bsVertical;
  {** Draw diagonal lines from top-left to bottom-right }
  bsFDiagonal = FPCanvas.bsFDiagonal;
  {** Draw diagonal lines from bottom-left to top-right }
  bsBDiagonal = FPCanvas.bsBDiagonal;
  {** Draw both horizontal and vertical lines }
  bsCross = FPCanvas.bsCross;
  {** Draw both diagonal lines }
  bsDiagCross = FPCanvas.bsDiagCross;

type

  { TBrush }
  {* A class describing a brush }
  TBrush = class(TFPCustomBrush)
  private
    function GetColor: TColor;
    procedure SetColor(AValue: TColor);
    public
      constructor Create; override;
      {** Color of the brush }
      property Color: TColor read GetColor write SetColor;
      {** Style of the brush: solid, diagonal lines, horizontal lines... }{inherited
      property Style : TBrushStyle read write;
      }
  end;

type
  TGraphic = class;

  { TCanvas }
  {* A surface on which to draw }
  TCanvas = class
  protected
    FCanvas: TGUICanvas;
  public
    constructor Create(ACanvas: TGUICanvas);
    {** Draw an image with top-left corner at (''x'',''y'') }
    procedure Draw(x,y: integer; AImage: TGraphic);
    {** Draw and stretch an image within the rectangle ''ARect'' }
    procedure StretchDraw(ARect: TRect; AImage: TGraphic);
    property GUICanvas: TGUICanvas read FCanvas;
  end;

  { TGraphic }
  {* A class containing any element that can be drawn within rectangular bounds }
  TGraphic = class(TPersistent)
  protected
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); virtual; abstract;
    function GetEmpty: Boolean; virtual; abstract;
    function GetHeight: Integer; virtual; abstract;
    function GetWidth: Integer; virtual; abstract;
    function GetTransparent: Boolean; virtual; abstract;
    procedure SetTransparent(Value: Boolean); virtual; abstract;
    procedure SetHeight(Value: Integer); virtual; abstract;
    procedure SetWidth(Value: Integer); virtual; abstract;
    function GetMimeType: string; virtual;
  public
    constructor Create; virtual;
    {** Load the content from a given file }
    procedure LoadFromFile({%H-}const Filename: string); virtual;
    {** Load the content from a given stream }
    procedure LoadFromStream(Stream: TStream); virtual; abstract;
    {** Saves the content to a file }
    procedure SaveToFile({%H-}const Filename: string); virtual;
    {** Saves the content into a given stream }
    procedure SaveToStream(Stream: TStream); virtual; abstract;
    {** Returns the list of possible file extensions }
    class function GetFileExtensions: string; virtual;
    {** Clears the content }
    procedure Clear; virtual;
  public
    {** Returns if the content is completely empty }
    property Empty: Boolean read GetEmpty;
    {** Returns the height of the bounding rectangle }
    property Height: Integer read GetHeight write SetHeight;
    {** Returns the width of the bounding rectangle }
    property Width: Integer read GetWidth write SetWidth;
    {** Gets or sets if it is drawn with transparency }
    property Transparent: Boolean read GetTransparent write SetTransparent;
  end;

  { TBitmap }
  {* Contains a bitmap }
  TBitmap = class(TGraphic)
  private
    FHeight: integer;
    FWidth: integer;
    FInDraw: boolean;
    FTransparent: boolean;
    FTransparentColor: TColor;
    FTransparentMode: TTransparentMode;
    function GetCanvas: TCanvas;
    function GetRawImage: TRawImage;
    procedure SetTransparentColor(AValue: TColor);
    procedure SetTransparentMode(AValue: TTransparentMode);
  protected
    FRawImage: TRawImage;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    procedure Changed(Sender: TObject); virtual;
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    procedure SetHeight(Value: Integer); override;
    procedure SetWidth(Value: Integer); override;
    function GetEmpty: Boolean; override;
    function GetTransparent: Boolean; override;
    procedure SetTransparent({%H-}Value: Boolean); override;
    function GetMimeType: string; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure LoadFromStream({%H-}Stream: TStream); override;
    procedure SaveToStream({%H-}Stream: TStream); override;
    {** Width of the bitmap in pixels }
    property Width: integer read GetWidth write SetWidth;
    {** Height of the bitmap in pixels }
    property Height: integer read GetHeight write SetHeight;
    property RawImage: TRawImage read GetRawImage;
    property Canvas: TCanvas read GetCanvas;
    property TransparentColor: TColor read FTransparentColor
             write SetTransparentColor default clDefault;
    property TransparentMode: TTransparentMode read FTransparentMode
             write SetTransparentMode default tmAuto;
  end;

  TRasterImage = TBitmap;

  {* Available font styles }
  TFontStyle = (
    {** Font is bold }
    fsBold,
    {** Font is italic }
    fsItalic,
    {** An horizontal line is drawn in the middle of the text }
    fsStrikeOut,
    {** Text is underlined }
    fsUnderline);
  {** A combination of font styles }
  TFontStyles = set of TFontStyle;
  {* Quality to use when font is rendered by the system }
  TFontQuality = (fqDefault, fqDraft, fqProof, fqNonAntialiased, fqAntialiased, fqCleartype, fqCleartypeNatural);

  { TFont }
  {* Contains the description of a font }
  TFont = class(TFPCustomFont)
  private
    FPixelsPerInch, FHeight: Integer;
    FQuality: TFontQuality;
    FStyle: TFontStyles;
    function GetColor: TColor;
    function GetHeight: Integer;
    function GetSize: Integer;
    function GetStyle: TFontStyles;
    procedure SetColor(AValue: TColor);
    procedure SetHeight(AValue: Integer);
    procedure SetQuality(AValue: TFontQuality);
    procedure SetStyle(AValue: TFontStyles);
  protected
    procedure SetSize(AValue: Integer); override;
  public
    constructor Create; override;
    {** Pixels per inches }
    property PixelsPerInch: Integer read FPixelsPerInch write FPixelsPerInch;
    {** Color of the font }
    property Color: TColor read GetColor write SetColor;
    {** Height of the font in pixels. When the number is negative, it indicates a size in pixels }
    property Height: Integer read GetHeight write SetHeight;
    {** Size of the font in inches. When the number is negative, it indicates a height in inches }
    property Size: Integer read GetSize write SetSize;
    {** Quality of the font rendering }
    property Quality: TFontQuality read FQuality write SetQuality;
    {** Style to apply to the text }
    property Style: TFontStyles read GetStyle write SetStyle;
  end;

{* Multiply and divide the number allowing big intermediate number and rounding the result }
function MulDiv(nNumber, nNumerator, nDenominator: Integer): Integer;
{* Round the number using math convention }
function MathRound(AValue: ValReal): Int64; inline;

implementation

uses sysutils, BGRAUTF8;

{$DEFINE INCLUDE_IMPLEMENTATION}
{$IFDEF BGRABITMAP_USE_FPGUI}
  {$i bgrafpgui.inc}
{$ELSE}
  {$i bgranogui.inc}
{$ENDIF}

function MathRound(AValue: ValReal): Int64; inline;
begin
  if AValue >= 0 then
    Result := Trunc(AValue + 0.5)
  else
    Result := Trunc(AValue - 0.5);
end;

function MulDiv(nNumber, nNumerator, nDenominator: Integer): Integer;
begin
  if nDenominator = 0 then
    Result := -1
  else
    Result := MathRound(int64(nNumber) * int64(nNumerator) / nDenominator);
end;

function FPColorToTColor(const FPColor: TFPColor): TColor;
begin
  {$IFDEF BGRABITMAP_USE_FPGUI}
  Result:=((FPColor.Blue shr 8) and $ff)
       or (FPColor.Green and $ff00)
       or ((FPColor.Red shl 8) and $ff0000);
  {$ELSE}
  Result:=((FPColor.Red shr 8) and $ff)
       or (FPColor.Green and $ff00)
       or ((FPColor.Blue shl 8) and $ff0000);
  {$ENDIF}
end;

function TColorToFPColor(const c: TColor): TFPColor;
begin
  {$IFDEF BGRABITMAP_USE_FPGUI}
  Result.Blue:=(c and $ff);
  Result.Blue:=Result.Blue+(Result.Blue shl 8);
  Result.Green:=(c and $ff00);
  Result.Green:=Result.Green+(Result.Green shr 8);
  Result.Red:=(c and $ff0000) shr 8;
  Result.Red:=Result.Red+(Result.Red shr 8);
  {$ELSE}
  Result.Red:=(c and $ff);
  Result.Red:=Result.Red+(Result.Red shl 8);
  Result.Green:=(c and $ff00);
  Result.Green:=Result.Green+(Result.Green shr 8);
  Result.Blue:=(c and $ff0000) shr 8;
  Result.Blue:=Result.Blue+(Result.Blue shr 8);
  {$ENDIF}
  Result.Alpha:=FPImage.alphaOpaque;
end;

{ TGraphic }

function TGraphic.GetMimeType: string;
begin
  result := '';
end;

constructor TGraphic.Create;
begin
  //nothing
end;

procedure TGraphic.LoadFromFile(const Filename: string);
var
  Stream: TStream;
begin
  Stream := TFileStreamUTF8.Create(Filename, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TGraphic.SaveToFile(const Filename: string);
var
  Stream: TStream;
begin
  Stream := TFileStreamUTF8.Create(Filename, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

class function TGraphic.GetFileExtensions: string;
begin
  result := '';
end;

procedure TGraphic.Clear;
begin
  //nothing
end;

{ TCanvas }

constructor TCanvas.Create(ACanvas: TGUICanvas);
begin
  FCanvas := ACanvas;
end;

procedure TCanvas.Draw(x, y: integer; AImage: TGraphic);
begin
  if AImage is TBitmap then
    FCanvas.DrawImage(x,y, TBitmap(AImage).RawImage)
  else
    AImage.Draw(self, rect(x,y,x+AImage.Width,y+AImage.Height));
end;

procedure TCanvas.StretchDraw(ARect: TRect; AImage: TGraphic);
begin
  if AImage is TBitmap then
    FCanvas.StretchDraw(ARect.Left,ARect.Top,ARect.Right-ARect.Left,ARect.Bottom-ARect.Top, TBitmap(AImage).RawImage)
  else
    AImage.Draw(self, ARect);
end;

{ TPen }

procedure TPen.SetColor(AValue: TColor);
begin
  FPColor := TColorToFPColor(AValue);
end;

function TPen.GetColor: TColor;
begin
  result := FPColorToTColor(FPColor);
end;

constructor TPen.Create;
begin
  inherited Create;
  Mode := pmCopy;
  Style := psSolid;
  Width := 1;
  FPColor := colBlack;
  FEndCap:= pecRound;
  FJoinStyle:= pjsRound;
end;

{ TBrush }

function TBrush.GetColor: TColor;
begin
  result := FPColorToTColor(FPColor);
end;

procedure TBrush.SetColor(AValue: TColor);
begin
  FPColor := TColorToFPColor(AValue);
end;

constructor TBrush.Create;
begin
  inherited Create;
  FPColor := colWhite;
end;

{ TFont }

function TFont.GetColor: TColor;
begin
  result := FPColorToTColor(FPColor);
end;

function TFont.GetHeight: Integer;
begin
  result := FHeight;
end;

function TFont.GetSize: Integer;
begin
  Result := inherited Size;
end;

function TFont.GetStyle: TFontStyles;
begin
  result := FStyle;
end;

procedure TFont.SetColor(AValue: TColor);
begin
  FPColor := TColorToFPColor(AValue);
end;

procedure TFont.SetHeight(AValue: Integer);
begin
  if Height <> AValue then
  begin
    FHeight := AValue;
    inherited SetSize(-MulDiv(AValue, 72, FPixelsPerInch));
  end;
end;

procedure TFont.SetQuality(AValue: TFontQuality);
begin
  if FQuality=AValue then Exit;
  FQuality:=AValue;
end;

procedure TFont.SetSize(AValue: Integer);
begin
  if Size <> AValue then
  begin
    inherited SetSize(AValue);
    FHeight := -MulDiv(AValue, FPixelsPerInch, 72);
  end;
end;

procedure TFont.SetStyle(AValue: TFontStyles);
begin
  if FStyle <> AValue then
  begin
    FStyle := AValue;
    inherited SetFlags(5, fsBold in FStyle);
    inherited SetFlags(6, fsItalic in FStyle);
    inherited SetFlags(7, fsUnderline in FStyle);
    inherited SetFlags(8, fsStrikeOut in FStyle);
  end;
end;

constructor TFont.Create;
begin
  FPixelsPerInch := GetScreenDPIY;
  FQuality := fqDefault;
  FPColor := colBlack;
end;

{ TBitmap }

procedure TBitmap.SetWidth(Value: Integer);
begin
  if FWidth=Value then Exit;
  FWidth:=Value;
end;

function TBitmap.GetEmpty: Boolean;
begin
  result := (Width = 0) or (Height = 0);
end;

function TBitmap.GetTransparent: Boolean;
begin
  result := FTransparent;
end;

procedure TBitmap.SetTransparent(Value: Boolean);
begin
  if Value = FTransparent then exit;
  FTransparent:= Value;
end;

procedure TBitmap.SetTransparentColor(AValue: TColor);
begin
  if FTransparentColor = AValue then exit;
  FTransparentColor := AValue;

  if AValue = clDefault
  then FTransparentMode := tmAuto
  else FTransparentMode := tmFixed;
end;

procedure TBitmap.SetTransparentMode(AValue: TTransparentMode);
begin
  if AValue = TransparentMode then exit;
  FTransparentMode := AValue;

  if AValue = tmAuto
  then TransparentColor := clDefault
end;

function TBitmap.GetMimeType: string;
begin
  Result:= 'image/bmp';
end;

procedure TBitmap.Changed(Sender: TObject);
begin
  //nothing
end;

procedure TBitmap.LoadFromStream(Stream: TStream);
begin
  raise exception.Create('Not implemented');
end;

procedure TBitmap.SaveToStream(Stream: TStream);
begin
  raise exception.Create('Not implemented');
end;

procedure TBitmap.SetHeight(Value: Integer);
begin
  if FHeight=Value then Exit;
  FHeight:=Value;
end;

function TBitmap.GetRawImage: TRawImage;
begin
  FRawImage.BGRASetSizeAndTransparency(FWidth, FHeight, FTransparent);
  result := FRawImage;
end;

procedure TBitmap.Draw(ACanvas: TCanvas; const Rect: TRect);
begin
  if FInDraw then exit;
  FInDraw := true;
  ACanvas.StretchDraw(Rect, self);
  FInDraw := false;
end;

function TBitmap.GetHeight: Integer;
begin
  result := FHeight;
end;

function TBitmap.GetWidth: Integer;
begin
  result := FWidth;
end;

function TBitmap.GetCanvas: TCanvas;
begin
  result := nil;
  raise exception.Create('Canvas not available');
end;

constructor TBitmap.Create;
begin
  FRawImage := TRawImage.Create;
  FTransparent:= false;
end;

destructor TBitmap.Destroy;
begin
  FRawImage.Free;
  inherited Destroy;
end;

{$ENDIF}

end.

