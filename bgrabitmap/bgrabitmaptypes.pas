{
 /**************************************************************************\
                                bgrabitmaptypes.pas
                                -------------------
                   This unit defines basic types and it must be
                   included in the 'uses' clause.

       --> Include BGRABitmap and BGRABitmapTypes in the 'uses' clause.
	       If you are using LCL types, add also BGRAGraphics unit.

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

unit BGRABitmapTypes;

{$mode objfpc}{$H+}
{$i bgrabitmap.inc}

interface

uses
  Classes, Types, BGRAGraphics,
  FPImage, FPImgCanv{$IFDEF BGRABITMAP_USE_LCL}, GraphType{$ENDIF};

type
  Int32or64 = {$IFDEF CPU64}Int64{$ELSE}LongInt{$ENDIF};
  UInt32or64 = {$IFDEF CPU64}UInt64{$ELSE}LongWord{$ENDIF};

{=== Miscellaneous types ===}

type
  {* Options when doing a floodfill (also called bucket fill) }
  TFloodfillMode = (
    {** Pixels that are filled are replaced }
    fmSet,
    {** Pixels that are filled are drawn upon with the fill color }
    fmDrawWithTransparency,
    {** Pixels that are filled are drawn upon to the extent that the color underneath is similar to
        the start color. The more different the different is, the less it is drawn upon }
    fmProgressive);

  {* Specifies how much smoothing is applied to the computation of the median }
  TMedianOption = (moNone, moLowSmooth, moMediumSmooth, moHighSmooth);
  {* Specifies the shape of a predefined blur }
  TRadialBlurType = (
    {** Gaussian-like, pixel importance decreases progressively }
    rbNormal,
    {** Disk blur, pixel importance does not decrease progressively }
    rbDisk,
    {** Pixel are considered when they are at a certain distance }
    rbCorona,
    {** Gaussian-like, but 10 times smaller than ''rbNormal'' }
    rbPrecise,
    {** Gaussian-like but simplified to be computed faster }
    rbFast,
    {** Box blur, pixel importance does not decrease progressively
        and the pixels are included when they are in a square.
        This is much faster than ''rbFast'' however you may get
        square shapes in the resulting image }
    rbBox);

  // checks the bounds of an image in the given clipping rectangle
  function CheckPutImageBounds(x, y, tx, ty: integer; out minxb, minyb, maxxb, maxyb, ignoreleft: integer; const cliprect: TRect): boolean;

{==== Imported from GraphType ====}
//if this unit is defined, otherwise
//define here the types used by the library.
{$IFDEF BGRABITMAP_USE_LCL}
  type
    { Order of the lines in an image }
    TRawImageLineOrder = GraphType.TRawImageLineOrder;
    { Order of the bits in a byte containing pixel values }
    TRawImageBitOrder = GraphType.TRawImageBitOrder;
    { Order of the bytes in a group of byte containing pixel values }
    TRawImageByteOrder = GraphType.TRawImageByteOrder;
    { Definition of a single line 3D bevel }
    TGraphicsBevelCut = GraphType.TGraphicsBevelCut;

  const
    riloTopToBottom = GraphType.riloTopToBottom;   // The first line (line 0) is the top line
    riloBottomToTop = GraphType.riloBottomToTop;   // The first line (line 0) is the bottom line

    riboBitsInOrder = GraphType.riboBitsInOrder;   // Bit 0 is pixel 0
    riboReversedBits = GraphType.riboReversedBits; // Bit 0 is pixel 7 (Bit 1 is pixel 6, ...)

    riboLSBFirst = GraphType.riboLSBFirst; // least significant byte first (little endian)
    riboMSBFirst = GraphType.riboMSBFirst; // most significant byte first (big endian)

    fsSurface = GraphType.fsSurface; //type is defined as Graphics.TFillStyle
    fsBorder = GraphType.fsBorder;

    bvNone = GraphType.bvNone;
    bvLowered = GraphType.bvLowered;
    bvRaised = GraphType.bvRaised;
    bvSpace = GraphType.bvSpace;
{$ELSE}
  type
    {* Order of the lines in an image }
    TRawImageLineOrder = (
      {** The first line in memory (line 0) is the top line }
      riloTopToBottom,
      {** The first line in memory (line 0) is the bottom line }
      riloBottomToTop);

    {* Order of the bits in a byte containing pixel values }
    TRawImageBitOrder = (
      {** The lowest bit is on the left. So with a monochrome picture, bit 0 would be pixel 0 }
      riboBitsInOrder,
      {** The lowest bit is on the right. So with a momochrome picture, bit 0 would be pixel 7 (bit 1 would be pixel 6, ...) }
      riboReversedBits);

    {* Order of the bytes in a group of byte containing pixel values }
    TRawImageByteOrder = (
      {** Least significant byte first (little endian) }
      riboLSBFirst,
      {** most significant byte first (big endian) }
      riboMSBFirst);

    {* Definition of a single line 3D bevel }
    TGraphicsBevelCut =
    (
      {** No bevel }
      bvNone,
      {** Shape is lowered, light is on the bottom-right corner }
      bvLowered,
      {** Shape is raised, light is on the top-left corner }
      bvRaised,
      {** Shape is at the same level, there is no particular lighting }
      bvSpace);
{$ENDIF}

{$DEFINE INCLUDE_INTERFACE}
{$I bgrapixel.inc}

{$DEFINE INCLUDE_INTERFACE}
{$I geometrytypes.inc}

{$DEFINE INCLUDE_INTERFACE}
{$i csscolorconst.inc}

{$DEFINE INCLUDE_SCANNER_INTERFACE }
{$I bgracustombitmap.inc}

{==== Integer math ====}

  {* Computes the value modulo cycle, and if the ''value'' is negative, the result
     is still positive }
  function PositiveMod(value, cycle: Int32or64): Int32or64; inline; overload;

  { Sin65536 and Cos65536 are fast routines to compute sine and cosine as integer values.
    They use a table to store already computed values. The return value is an integer
    ranging from 0 to 65536, so the mean value is 32768 and the half amplitude is
    32768 instead of 1. The input has a period of 65536, so you can supply any integer
    without applying a modulo. }

  { Compute all values now }
  procedure PrecalcSin65536;

  {* Returns an integer approximation of the sine. Value ranges from 0 to 65535,
     where 65536 corresponds to the next cycle }
  function Sin65536(value: word): Int32or64; inline;
  {* Returns an integer approximation of the cosine. Value ranges from 0 to 65535,
     where 65536 corresponds to the next cycle }
  function Cos65536(value: word): Int32or64; inline;

  {* Returns the square root of the given byte, considering that
     255 is equal to unity }
  function ByteSqrt(value: byte): byte; inline;

{==== Types provided for fonts ====}
type
  {* Quality to be used to render text }
  TBGRAFontQuality = (
    {** Use the system capabilities. It is rather fast however it may be
        not be smoothed. }
    fqSystem,
    {** Use the system capabilities to render with ClearType. This quality is
        of course better than fqSystem however it may not be perfect.}
    fqSystemClearType,
    {** Garanties a high quality antialiasing. }
    fqFineAntialiasing,
    {** Fine antialiasing with ClearType in assuming an LCD display in red/green/blue order }
    fqFineClearTypeRGB,
    {** Fine antialiasing with ClearType in assuming an LCD display in blue/green/red order }
    fqFineClearTypeBGR);

  {* Measurements of a font }
  TFontPixelMetric = record
    {** The values have been computed }
    Defined: boolean;
    {** Position of the baseline, where most letters lie }
    Baseline,
    {** Position of the top of the small letters (x being one of them) }
    xLine,
    {** Position of the top of the UPPERCASE letters }
    CapLine,
    {** Position of the bottom of letters like g and p }
    DescentLine,
    {** Total line height including line spacing defined by the font }
    Lineheight: integer;
  end;

  {* Vertical anchoring of the font. When text is drawn, a start coordinate
      is necessary. Text can be positioned in different ways. This enum
      defines what position it is regarding the font }
  TFontVerticalAnchor = (
    {** The top of the font. Everything will be drawn below the start coordinate. }
    fvaTop,
    {** The center of the font }
    fvaCenter,
    {** The top of capital letters }
    fvaCapLine,
    {** The center of capital letters }
    fvaCapCenter,
    {** The top of small letters }
    fvaXLine,
    {** The center of small letters }
    fvaXCenter,
    {** The baseline, the bottom of most letters }
    fvaBaseline,
    {** The bottom of letters that go below the baseline }
    fvaDescentLine,
    {** The bottom of the font. Everything will be drawn above the start coordinate }
    fvaBottom);

  {* Definition of a function that handles work-break }
  TWordBreakHandler = procedure(var ABeforeUTF8, AAfterUTF8: string) of object;

  {* Alignment for a typewriter, that does not have any more information
     than a square shape containing glyphs }
  TBGRATypeWriterAlignment = (twaTopLeft, twaTop, twaTopRight, twaLeft, twaMiddle, twaRight, twaBottomLeft, twaBottom, twaBottomRight);
  {* How a typewriter must render its content on a Canvas2d }
  TBGRATypeWriterOutlineMode = (twoPath, twoFill, twoStroke, twoFillOverStroke, twoStrokeOverFill, twoFillThenStroke, twoStrokeThenFill);

  { TBGRACustomFontRenderer }
  {* Abstract class for all font renderers }
  TBGRACustomFontRenderer = class
    {** Specifies the font to use. Unless the font renderer accept otherwise,
        the name is in human readable form, like 'Arial', 'Times New Roman', ...  }
    FontName: string;

    {** Specifies the set of styles to be applied to the font.
        These can be fsBold, fsItalic, fsStrikeOut, fsUnderline.
        So the value [fsBold,fsItalic] means that the font must be bold and italic }
    FontStyle: TFontStyles;

    {** Specifies the quality of rendering. Default value is fqSystem }
    FontQuality : TBGRAFontQuality;

    {** Specifies the rotation of the text, for functions that support text rotation.
        It is expressed in tenth of degrees, positive values going counter-clockwise }
    FontOrientation: integer;

    {** Specifies the height of the font without taking into account additional line spacing.
        A negative value means that it is the full height instead }
    FontEmHeight: integer;

    {** Returns measurement for the current font in pixels }
    function GetFontPixelMetric: TFontPixelMetric; virtual; abstract;

    {** Returns the total size of the string provided using the current font.
        Orientation is not taken into account, so that the width is along the text }
    function TextSize(sUTF8: string): TSize; virtual; abstract;

    {** Draws the UTF8 encoded string, with color ''c''.
        If align is taLeftJustify, (''x'',''y'') is the top-left corner.
        If align is taCenter, (''x'',''y'') is at the top and middle of the text.
        If align is taRightJustify, (''x'',''y'') is the top-right corner.
        The value of ''FontOrientation'' is taken into account, so that the text may be rotated }
    procedure TextOut(ADest: TBGRACustomBitmap; x, y: single; sUTF8: string; c: TBGRAPixel; align: TAlignment); virtual; abstract;

    {** Same as above functions, except that the text is filled using texture.
        The value of ''FontOrientation'' is taken into account, so that the text may be rotated }
    procedure TextOut(ADest: TBGRACustomBitmap; x, y: single; sUTF8: string; texture: IBGRAScanner; align: TAlignment); virtual; abstract;

    {** Same as above, except that the orientation is specified, overriding the value of the property ''FontOrientation'' }
    procedure TextOutAngle(ADest: TBGRACustomBitmap; x, y: single; orientationTenthDegCCW: integer; sUTF8: string; c: TBGRAPixel; align: TAlignment); virtual; abstract;
    {** Same as above, except that the orientation is specified, overriding the value of the property ''FontOrientation'' }
    procedure TextOutAngle(ADest: TBGRACustomBitmap; x, y: single; orientationTenthDegCCW: integer; sUTF8: string; texture: IBGRAScanner; align: TAlignment); virtual; abstract;

    {** Draw the UTF8 encoded string at the coordinate (''x'',''y''), clipped inside the rectangle ''ARect''.
        Additional style information is provided by the style parameter.
        The color ''c'' is used to fill the text. No rotation is applied. }
    procedure TextRect(ADest: TBGRACustomBitmap; ARect: TRect; x, y: integer; sUTF8: string; style: TTextStyle; c: TBGRAPixel); virtual; abstract;

    {** Same as above except a ''texture'' is used to fill the text }
    procedure TextRect(ADest: TBGRACustomBitmap; ARect: TRect; x, y: integer; sUTF8: string; style: TTextStyle; texture: IBGRAScanner); virtual; abstract;

    {** Copy the path for the UTF8 encoded string into ''ADest''.
        If ''align'' is ''taLeftJustify'', (''x'',''y'') is the top-left corner.
        If ''align'' is ''taCenter'', (''x'',''y'') is at the top and middle of the text.
        If ''align'' is ''taRightJustify'', (''x'',''y'') is the top-right corner. }
    procedure CopyTextPathTo({%H-}ADest: IBGRAPath; {%H-}x, {%H-}y: single; {%H-}s: string; {%H-}align: TAlignment); virtual; //optional
  end;

  {* Output mode for the improved renderer for readability. This is used by the font renderer based on LCL in ''BGRAText'' }
  TBGRATextOutImproveReadabilityMode = (irMask, irNormal, irClearTypeRGB, irClearTypeBGR);

{** Removes line ending and tab characters from a string (for a function
    like ''TextOut'' that does not handle this). this works with UTF8 strings
    as well }
function CleanTextOutString(s: string): string;
{** Remove the line ending at the specified position or return False.
    This works with UTF8 strings however the index is the byte index }
function RemoveLineEnding(var s: string; indexByte: integer): boolean;
{** Remove the line ending at the specified position or return False.
    The index is the character index, that may be different from the
    byte index }
function RemoveLineEndingUTF8(var sUTF8: string; indexUTF8: integer): boolean;
{** Default word break handler, that simply divide when there is a space }
procedure BGRADefaultWordBreakHandler(var ABefore, AAfter: string);

{==== Images and resampling ====}

type
  {* How the resample is to be computed }
  TResampleMode = (
    {** Low quality resample by repeating pixels, stretching them }
    rmSimpleStretch,
    {** Use resample filters. This gives high
        quality resampling however this the proportion changes slightly because
        the first and last pixel are considered to occupy only half a unit as
        they are considered as the border of the picture
        (pixel-centered coordinates) }
    rmFineResample);

  {* List of resample filter to be used with ''rmFineResample'' }
  TResampleFilter = (
    {** Equivalent of simple stretch with high quality and pixel-centered coordinates }
    rfBox,
    {** Linear interpolation giving slow transition between pixels }
    rfLinear,
    {** Mix of ''rfLinear'' and ''rfCosine'' giving medium speed stransition between pixels }
    rfHalfCosine,
    {** Cosine-like interpolation giving fast transition between pixels }
    rfCosine,
    {** Simple bi-cubic filter (blurry) }
    rfBicubic,
    {** Mitchell filter, good for downsizing interpolation }
    rfMitchell,
    {** Spline filter, good for upsizing interpolation, however slightly blurry }
    rfSpline,
    {** Lanczos with radius 2, blur is corrected }
    rfLanczos2,
    {** Lanczos with radius 3, high contrast }
    rfLanczos3,
    {** Lanczos with radius 4, high contrast }
    rfLanczos4,
    {** Best quality using rfMitchell or rfSpline }
    rfBestQuality);

const
  {** List of strings to represent resample filters }
  ResampleFilterStr : array[TResampleFilter] of string =
   ('Box','Linear','HalfCosine','Cosine','Bicubic','Mitchell','Spline',
    'Lanczos2','Lanczos3','Lanczos4','BestQuality');

  {** Gives the sample filter represented by a string }
  function StrToResampleFilter(str: string): TResampleFilter;

type
  {* List of image formats }
  TBGRAImageFormat = (
    {** Unknown format }
    ifUnknown,
    {** JPEG format, opaque, lossy compression }
    ifJpeg,
    {** PNG format, transparency, lossless compression }
    ifPng,
    {** GIF format, single transparent color, lossless in theory but only low number of colors allowed }
    ifGif,
    {** BMP format, transparency, no compression. Note that transparency is
        not supported by all BMP readers so it is not recommended to avoid
        storing images with transparency in this format }
    ifBmp,
    {** ICO format, contains different sizes of the same image }
    ifIco,
    {** PCX format, opaque, rudimentary lossless compression }
    ifPcx,
    {** Paint.NET format, layers, lossless compression }
    ifPaintDotNet,
    {** LazPaint format, layers, lossless compression }
    ifLazPaint,
    {** OpenRaster format, layers, lossless compression }
    ifOpenRaster,
    {** Photoshop format, layers, rudimentary lossless compression }
    ifPsd,
    {** Targa format (TGA), transparency, rudimentary lossless compression }
    ifTarga,
    {** TIFF format, limited support }
    ifTiff,
    {** X-Window capture, limited support }
    ifXwd,
    {** X-Pixmap, text encoded image, limited support }
    ifXPixMap,
    {** iGO BMP, limited support }
    ifBmpMioMap);

var
  {** List of stream readers for images }
  DefaultBGRAImageReader: array[TBGRAImageFormat] of TFPCustomImageReaderClass;
  {** List of stream writers for images }
  DefaultBGRAImageWriter: array[TBGRAImageFormat] of TFPCustomImageWriterClass;

  {** Detect the file format of a given file }
  function DetectFileFormat(AFilenameUTF8: string): TBGRAImageFormat;
  {** Detect the file format of a given stream. ''ASuggestedExtensionUTF8'' can
      be provided to guess the format }
  function DetectFileFormat(AStream: TStream; ASuggestedExtensionUTF8: string = ''): TBGRAImageFormat;
  {** Returns the file format that is most likely to be stored in the
      given filename (according to its extension) }
  function SuggestImageFormat(AFilenameOrExtensionUTF8: string): TBGRAImageFormat;
  {** Returns a likely image extension for the format }
  function SuggestImageExtension(AFormat: TBGRAImageFormat): string;
  {** Create an image reader for the given format }
  function CreateBGRAImageReader(AFormat: TBGRAImageFormat): TFPCustomImageReader;
  {** Create an image writer for the given format. ''AHasTransparentPixels''
      specifies if alpha channel must be supported }
  function CreateBGRAImageWriter(AFormat: TBGRAImageFormat; AHasTransparentPixels: boolean): TFPCustomImageWriter;

{$DEFINE INCLUDE_INTERFACE}
{$I bgracustombitmap.inc}

implementation

uses Math, SysUtils, BGRAUTF8,
  FPReadTiff, FPReadXwd, FPReadXPM,
  FPWriteTiff, FPWriteJPEG, BGRAWritePNG, FPWriteBMP, FPWritePCX,
  FPWriteTGA, FPWriteXPM;

{$DEFINE INCLUDE_IMPLEMENTATION}
{$I geometrytypes.inc}

{$DEFINE INCLUDE_IMPLEMENTATION}
{$I csscolorconst.inc}

{$DEFINE INCLUDE_IMPLEMENTATION}
{$I bgracustombitmap.inc}

{$DEFINE INCLUDE_IMPLEMENTATION}
{$I bgrapixel.inc}

function CleanTextOutString(s: string): string;
var idxIn, idxOut: integer;
begin
  setlength(result, length(s));
  idxIn := 1;
  idxOut := 1;
  while IdxIn <= length(s) do
  begin
    if not (s[idxIn] in[#13,#10,#9]) then //those characters are always 1 byte long so it is the same with UTF8
    begin
      result[idxOut] := s[idxIn];
      inc(idxOut);
    end;
    inc(idxIn);
  end;
  setlength(result, idxOut-1);
end;

function RemoveLineEnding(var s: string; indexByte: integer): boolean;
begin //we can ignore UTF8 character length because #13 and #10 are always 1 byte long
      //so this function can be applied to UTF8 strings as well
  result := false;
  if length(s) >= indexByte then
  begin
    if s[indexByte] in[#13,#10] then
    begin
      result := true;
      if length(s) >= indexByte+1 then
      begin
        if (s[indexByte+1] <> s[indexByte]) and (s[indexByte+1] in[#13,#10]) then
          delete(s,indexByte,2)
        else
          delete(s,indexByte,1);
      end
        else
          delete(s,indexByte,1);
    end;
  end;
end;

function RemoveLineEndingUTF8(var sUTF8: string; indexUTF8: integer): boolean;
var indexByte: integer;
    pIndex: PChar;
begin
  pIndex := UTF8CharStart(@sUTF8[1],length(sUTF8),indexUTF8);
  if pIndex = nil then
  begin
    result := false;
    exit;
  end;
  indexByte := pIndex - @sUTF8[1];
  result := RemoveLineEnding(sUTF8, indexByte);
end;

procedure BGRADefaultWordBreakHandler(var ABefore, AAfter: string);
var p: integer;
begin
  if (AAfter <> '') and (ABefore <> '') and (AAfter[1]<> ' ') and (ABefore[length(ABefore)] <> ' ') then
  begin
    p := length(ABefore);
    while (p > 1) and (ABefore[p-1] <> ' ') do dec(p);
    if p > 1 then //can put the word after
    begin
      AAfter := copy(ABefore,p,length(ABefore)-p+1)+AAfter;
      ABefore := copy(ABefore,1,p-1);
    end else
    begin //cannot put the word after, so before

    end;
  end;
  while (ABefore <> '') and (ABefore[length(ABefore)] =' ') do delete(ABefore,length(ABefore),1);
  while (AAfter <> '') and (AAfter[1] =' ') do delete(AAfter,1,1);
end;


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

{ TBGRACustomFontRenderer }

procedure TBGRACustomFontRenderer.CopyTextPathTo(ADest: IBGRAPath; x, y: single; s: string; align: TAlignment);
begin {optional implementation} end;


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
  if (ASuggestedExtensionUTF8 <> '') and (ASuggestedExtensionUTF8[1] <> '.') then //first UTF8 char is in first pos
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

function SuggestImageExtension(AFormat: TBGRAImageFormat): string;
begin
  case AFormat of
    ifJpeg: result := 'jpg';
    ifPng: result := 'png';
    ifGif: result := 'gif';
    ifBmp: result := 'bmp';
    ifIco: result := 'ico';
    ifPcx: result := 'pcx';
    ifPaintDotNet: result := 'pdn';
    ifLazPaint: result := 'lzp';
    ifOpenRaster: result := 'ora';
    ifPsd: result := 'psd';
    ifTarga: result := 'tga';
    ifTiff: result := 'tif';
    ifXwd: result := 'xwd';
    ifXPixMap: result := 'xpm';
    ifBmpMioMap: result := 'bmp';
    else result := '?';
  end;
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
    result := TBGRAWriterPNG.Create;
    TBGRAWriterPNG(result).UseAlpha := AHasTransparentPixels;
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

  {$DEFINE INCLUDE_INIT}
  {$I bgrapixel.inc}

  {$DEFINE INCLUDE_INIT}
  {$I csscolorconst.inc}
  
  DefaultBGRAImageWriter[ifJpeg] := TFPWriterJPEG;
  DefaultBGRAImageWriter[ifPng] := TBGRAWriterPNG;
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

  {$DEFINE INCLUDE_FINAL}
  {$I csscolorconst.inc}

  {$DEFINE INCLUDE_FINAL}
  {$I bgrapixel.inc}
end.
