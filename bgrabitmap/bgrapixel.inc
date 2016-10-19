{=== Pixel types and functions ===}

{$IFDEF INCLUDE_INTERFACE}
{$UNDEF INCLUDE_INTERFACE}
type
  {* Pointer for direct pixel access. Data is stored as a sequence of ''TBGRAPixel''.
     See [[BGRABitmap tutorial 4]] }
  PBGRAPixel = ^TBGRAPixel;

  {$IFNDEF BGRABITMAP_BGRAPIXEL}
    {$IFDEF BGRABITMAP_USE_LCL}
      {$IFDEF LCLgtk}
        {$DEFINE BGRABITMAP_RGBAPIXEL}
      {$ENDIF}
      {$IFDEF LCLgtk2}
        {$DEFINE BGRABITMAP_RGBAPIXEL}
      {$ENDIF}
      {$IFDEF DARWIN}
        {$DEFINE BGRABITMAP_RGBAPIXEL}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}

  {* Each pixel is a sequence of 4 bytes containing blue, green, red and alpha channel.
     Values range from 0 to 255, color is in sRGB colorspace. The alpha value of 0
     is transparent and 255 is opaque. In the bitmap data, when the pixel is fully transparent,
	 the RGB values are supposed to be set to zero. }

  { TBGRAPixel }

  TBGRAPixel = packed record
  private
    function GetClassIntensity: word;
    function GetClassLightness: word;
    procedure SetClassIntensity(AValue: word);
    procedure SetClassLightness(AValue: word);
  public
    {$IFDEF BGRABITMAP_RGBAPIXEL}
    red, green, blue, alpha: byte;
    {$ELSE}
    blue, green, red, alpha: byte;
    {$ENDIF}
    procedure FromRGB(ARed,AGreen,ABlue: Byte; AAlpha: Byte = 255);
    procedure FromColor(AColor: TColor; AAlpha: Byte = 255);
    procedure FromString(AStr: string);
    procedure FromFPColor(AColor: TFPColor);
    procedure ToRGB(out ARed,AGreen,ABlue,AAlpha: Byte); overload;
    procedure ToRGB(out ARed,AGreen,ABlue: Byte); overload;
    function ToColor: TColor;
    function ToString: string;
    function ToGrayscale(AGammaCorrection: boolean = true): TBGRAPixel;
    function ToFPColor: TFPColor;
    class Operator := (Source: TBGRAPixel): TColor;
    class Operator := (Source: TColor): TBGRAPixel;
    property Intensity: word read GetClassIntensity write SetClassIntensity;
    property Lightness: word read GetClassLightness write SetClassLightness;
  end;
  TBGRAPixelBuffer = packed array of TBGRAPixel;

procedure AllocateBGRAPixelBuffer(var ABuffer: TBGRAPixelBuffer; ASize: integer);

const
  {$IFDEF BGRABITMAP_RGBAPIXEL}
  TBGRAPixel_RGBAOrder = True;
  TBGRAPixel_RedByteOffset = 0;
  TBGRAPixel_GreenByteOffset = 1;
  TBGRAPixel_BlueByteOffset = 2;
  {$ELSE}
  TBGRAPixel_RGBAOrder = False;
  TBGRAPixel_BlueByteOffset = 0;
  TBGRAPixel_GreenByteOffset = 1;
  TBGRAPixel_RedByteOffset = 2;
  {$ENDIF}
  TBGRAPixel_AlphaByteOffset = 3;
  {$IFDEF ENDIAN_LITTLE}
  TBGRAPixel_RedShift = TBGRAPixel_RedByteOffset*8;
  TBGRAPixel_GreenShift = TBGRAPixel_GreenByteOffset*8;
  TBGRAPixel_BlueShift = TBGRAPixel_BlueByteOffset*8;
  TBGRAPixel_AlphaShift = TBGRAPixel_AlphaByteOffset*8;
  {$ELSE}
  TBGRAPixel_RedShift = 24 - TBGRAPixel_RedByteOffset*8;
  TBGRAPixel_GreenShift = 24 - TBGRAPixel_GreenByteOffset*8;
  TBGRAPixel_BlueShift = 24 - TBGRAPixel_BlueByteOffset*8;
  TBGRAPixel_AlphaShift = 24 - TBGRAPixel_AlphaByteOffset*8;
  {$ENDIF}

  {** Creates a pixel with given RGBA values }
  function BGRA(red, green, blue, alpha: byte): TBGRAPixel; overload; inline;
  {** Creates a opaque pixel with given RGB values }
  function BGRA(red, green, blue: byte): TBGRAPixel; overload; inline;
  {** Checks if two pixels are equal. If they are both transparent,
      RGB values are ignored }
  operator = (const c1, c2: TBGRAPixel): boolean; inline;
  {** Returns the intensity of a pixel. The intensity is the
     maximum value reached by any component }
  function GetIntensity(c: TBGRAPixel): word; inline;
  {** Sets the intensity of a pixel }
  function SetIntensity(c: TBGRAPixel; intensity: word): TBGRAPixel;
  {** Returns the lightness of a pixel. The lightness is the
     perceived brightness, 0 being black and 65535 being white }
  function GetLightness(c: TBGRAPixel): word;
  {** Sets the lightness of a pixel }
  function SetLightness(c: TBGRAPixel; lightness: word): TBGRAPixel;
  {** Sets the lightness quickly, by fading towards black if ''lightness'' is
      less than 32768, and fading towards white if ''lightness'' is more
      than 32768 }
  function ApplyLightnessFast(color: TBGRAPixel; lightness: word): TBGRAPixel; inline;
  {** Sets the intensity quickly, by fading towards black if ''lightness'' is
      less than 32768, and multiplying all components if ''lightness'' is more
      than 32768. In case of saturation, it fades towards white }
  function ApplyIntensityFast(color: TBGRAPixel; lightness: longword): TBGRAPixel;
  {** Combines two lightnesses together. A value of 32768 is neutral. The
      result may exceed 65535 }
  function CombineLightness(lightness1,lightness2: Int32or64): Int32or64;
  {** Converts a color into grayscale }
  function BGRAToGrayscale(c: TBGRAPixel): TBGRAPixel;
  function BGRAToGrayscaleLinear(c: TBGRAPixel): TBGRAPixel;
  {** Create a gray color with the given ''lightness'' }
  function GrayscaleToBGRA(lightness: word): TBGRAPixel;
  {** Merge two colors without gamma correction }
  function MergeBGRA(c1, c2: TBGRAPixel): TBGRAPixel; overload;
  {** Merge two colors without gamma correction. ''weight1'' and ''weight2''
      indicates the weight of the color barycentre }
  function MergeBGRA(c1: TBGRAPixel; weight1: integer; c2: TBGRAPixel; weight2: integer): TBGRAPixel; overload;
  {** Merge two colors with gamma correction. ''weight1'' and ''weight2''
      indicates the weight of the color barycentre }
  function MergeBGRAWithGammaCorrection(c1: TBGRAPixel; weight1: byte; c2: TBGRAPixel; weight2: byte): TBGRAPixel;
  {** Converts a ''TColor'' value into an opaque pixel }
  function ColorToBGRA(color: TColor): TBGRAPixel; overload;
  {** Converts a ''TColor'' value into a pixel with given ''opacity'' }
  function ColorToBGRA(color: TColor; opacity: byte): TBGRAPixel; overload;
  {** Converts a pixel into a TColor value, discarding the alpha value }
  function BGRAToColor(c: TBGRAPixel): TColor;
  {** Converts a ''TFPColor'' value into a pixel. Note that even if
      ''TFPColor'' have 16-bit values, they are not considered as
      gamma expanded }
  function FPColorToBGRA(AValue: TFPColor): TBGRAPixel;
  {** Converts a pixel into a ''TFPColor'' }
  function BGRAToFPColor(AValue: TBGRAPixel): TFPColor; inline;
  function Color16BitToBGRA(AColor: Word): TBGRAPixel;
  function BGRAToColor16Bit(const AColor: TBGRAPixel): Word;
  {** Computes the difference (with gamma correction) between two pixels,
      taking into account all dimensions, including transparency. The
      result ranges from 0 to 65535 }
  function BGRAWordDiff(c1, c2: TBGRAPixel): word;
  {** Computes the difference (with gamma correction) between two pixels,
      taking into account all dimensions, including transparency. The
      result ranges from 0 to 255 }
  function BGRADiff(c1, c2: TBGRAPixel): byte;
  function FastBGRALinearDiff(c1,c2: TBGRAPixel): byte;
  function FastBGRAExpandedDiff(c1,c2: TBGRAPixel): word;

type
  {* Array of pixels }
  ArrayOfTBGRAPixel = array of TBGRAPixel;
  {** Merge given colors without gamma correction }
  function MergeBGRA(const colors: array of TBGRAPixel): TBGRAPixel; overload;

{ Get height [0..1] stored in a TBGRAPixel }
function MapHeight(Color: TBGRAPixel): Single;

{ Get TBGRAPixel to store height [0..1] }
function MapHeightToBGRA(Height: Single; Alpha: Byte): TBGRAPixel;

type
  {* Possible modes when drawing a pixel over another one }
  TDrawMode = (
    {** The pixel is replaced }
    dmSet,
    {** The pixel is replaced if the pixel over has an alpha value of 255 }
    dmSetExceptTransparent,
    {** The pixel is blend over the other one according to alpha values,
        however no gamma correction is applied. In other words, the color
        space is assumed to be linear }
    dmLinearBlend,
    {** The pixel is blend over the other one according to alpha values,
        and a gamma correction is applied. In other word, the color
        space is assumed to be sRGB }
    dmDrawWithTransparency,
    {** Values of all channels are combined with Xor. This is useful to
        compute the binary difference, however it is not something that makes
        much sense to display on the screen }
    dmXor);

const
  {** An alias for the linear blend, because it is faster than blending
      with gamma correction }
  dmFastBlend = dmLinearBlend;

type
  {* Advanced blending modes. See [http://www.brighthub.com/multimedia/photography/articles/18301.aspx Paint.NET blend modes]
    and [http://www.pegtop.net/delphi/articles/blendmodes/ Formulas]. Blending layers has two steps. The first one is
    to apply the blend operations listed below, and the second is the actual merging of the colors }
  TBlendOperation = (
    {** Simple blend, except that it forces a linear merge so it is equivalent to ''dmLinearBlend'' }
    boLinearBlend,
    {** Simple blend. It is equivalent to ''dmLinearBlend'' or ''dmDrawWithTransparency'' }
    boTransparent,
    {** Lighting blend modes (tends to increase the luminosity) }
    boLighten, boScreen, boAdditive, boLinearAdd, boColorDodge, boDivide, boNiceGlow, boSoftLight, boHardLight,
    {** Masking blend modes (tends to decrease the luminosity) }
    boGlow, boReflect, boOverlay, boDarkOverlay, boDarken, boMultiply, boColorBurn,
    {** Difference blend modes }
    boDifference, boLinearDifference, boExclusion, boLinearExclusion, boSubtract, boLinearSubtract, boSubtractInverse, boLinearSubtractInverse,
    {** Negation blend modes }
    boNegation, boLinearNegation,
    {** Xor blend mode. It is sightly different from ''dmXor'' because the alpha value is used like in other blends modes }
    boXor,
    {** Additional blend modes **}
    boSvgSoftLight);

const
  {** Alias to glow that express that this blend mode masks the part where the top layer is black }
  boGlowMask = boGlow;
  {** Alias because linear or non linear multiply modes are identical }
  boLinearMultiply = boMultiply;
  {** Alias to express that dark overlay is simply an overlay with gamma correction }
  boNonLinearOverlay = boDarkOverlay;

const
  {** String constants for blend modes }
  BlendOperationStr : array[TBlendOperation] of string
  = ('LinearBlend', 'Transparent',
     'Lighten', 'Screen', 'Additive', 'LinearAdd', 'ColorDodge', 'Divide', 'NiceGlow', 'SoftLight', 'HardLight',
     'Glow', 'Reflect', 'Overlay', 'DarkOverlay', 'Darken', 'Multiply', 'ColorBurn',
     'Difference', 'LinearDifference', 'Exclusion', 'LinearExclusion', 'Subtract', 'LinearSubtract', 'SubtractInverse', 'LinearSubtractInverse',
     'Negation', 'LinearNegation', 'Xor', 'SvgSoftLight');

  {** Returns the blend mode expressed by the string }
  function StrToBlendOperation(str: string): TBlendOperation;

type
  {* Specifies how a palette handles the alpha channel }
  TAlphaChannelPaletteOption = (
    {** The alpha channel is ignored. The alpha channel is considered to be stored elsewhere }
    acIgnore,
    {** One entry is allocated the fully transparent color }
    acTransparentEntry,
    {** The alpha channel is fully embedded in the palette so that a color is identified by its four RGBA channels }
    acFullChannelInPalette);

  {* Dithering algorithms that specifies how to handle colors that are not found in the palette }
  TDitheringAlgorithm = (
    {** The nearest color is to be used instead }
    daNearestNeighbor,
    {** The nearest color may be used however another color may be used to compensate for the error,
        following Floyd-Steinberg algorithm }
    daFloydSteinberg);

{$DEFINE INCLUDE_INTERFACE}
{$i basiccolorspace.inc}

{$ENDIF}

{$IFDEF INCLUDE_IMPLEMENTATION}
{$UNDEF INCLUDE_IMPLEMENTATION}

{$DEFINE INCLUDE_IMPLEMENTATION}
{$i basiccolorspace.inc}

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

{************************** Color functions **************************}

procedure AllocateBGRAPixelBuffer(var ABuffer: TBGRAPixelBuffer; ASize: integer);
begin
  if ASize > length(ABuffer) then
    setlength(ABuffer, max(length(ABuffer)*2,ASize));
end;

function BGRA(red, green, blue, alpha: byte): TBGRAPixel;
begin
  DWord(result) := (red shl TBGRAPixel_RedShift) or
                   (green shl TBGRAPixel_GreenShift) or
                   (blue shl TBGRAPixel_BlueShift) or
                   (alpha shl TBGRAPixel_AlphaShift);
end;

function BGRA(red, green, blue: byte): TBGRAPixel; overload;
begin
  DWord(result) := (red shl TBGRAPixel_RedShift) or
                   (green shl TBGRAPixel_GreenShift) or
                   (blue shl TBGRAPixel_BlueShift) or
                   (255 shl TBGRAPixel_AlphaShift);
end;

operator = (const c1, c2: TBGRAPixel): boolean;
begin
  if (c1.alpha = 0) and (c2.alpha = 0) then
    Result := True
  else
    Result := (c1.alpha = c2.alpha) and (c1.red = c2.red) and
      (c1.green = c2.green) and (c1.blue = c2.blue);
end;

function GetIntensity(c: TBGRAPixel): word;
begin
  Result := c.red;
  if c.green > Result then
    Result := c.green;
  if c.blue > Result then
    Result := c.blue;
  result := GammaExpansionTab[Result];
end;

function SetIntensity(c: TBGRAPixel; intensity: word): TBGRAPixel;
begin
  result := GammaCompression(SetIntensity(GammaExpansion(c),intensity));
end;

function GetLightness(c: TBGRAPixel): word;
begin
  result := GetLightness(GammaExpansion(c));
end;

function SetLightness(c: TBGRAPixel; lightness: word): TBGRAPixel;
begin
  result := GammaCompression(SetLightness(GammaExpansion(c),lightness));
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

function BGRAToGrayscaleLinear(c: TBGRAPixel): TBGRAPixel;
var
  gray:  byte;
begin
  if c.alpha = 0 then
  begin
    result := BGRAPixelTransparent;
    exit;
  end;
  //gray composition
  gray  := (c.red * redWeightShl10 + c.green * greenWeightShl10 +
    c.blue * blueWeightShl10 + 512) shr 10;
  //gamma compression
  Result.red := gray;
  Result.green := gray;
  Result.blue := gray;
  Result.alpha := c.alpha;
end;

function GrayscaleToBGRA(lightness: word): TBGRAPixel;
begin
  result.red := GammaCompressionTab[lightness];
  result.green := result.red;
  result.blue := result.red;
  result.alpha := $ff;
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

{ Convert a TColor value to a TBGRAPixel value. Note that
  you need to call ColorToRGB first if you use a system
  color identifier like clWindow. }
{$PUSH}{$R-}
function ColorToBGRA(color: TColor): TBGRAPixel; overload;
begin
  RedGreenBlue(color, Result.red,Result.green,Result.blue);
  Result.alpha := 255;
end;

function ColorToBGRA(color: TColor; opacity: byte): TBGRAPixel; overload;
begin
  RedGreenBlue(color, Result.red,Result.green,Result.blue);
  Result.alpha := opacity;
end;
{$POP}

function BGRAToColor(c: TBGRAPixel): TColor;
begin
  Result := RGBToColor(c.red, c.green, c.blue);
end;

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

function Color16BitToBGRA(AColor: Word): TBGRAPixel;
begin
  result := BGRA( ((AColor and $F800) shr 11)*255 div 31,
                  ((AColor and $07e0) shr 5)*255 div 63,
                  (AColor and $001f)*255 div 31 );
end;

function BGRAToColor16Bit(const AColor: TBGRAPixel): Word;
begin
  result := (((AColor.Red * 31 + 64) div 255) shl 11) +
            (((AColor.green * 63 + 64) div 255) shl 5) +
             ((AColor.blue * 31 + 64) div 255);
end;

function BGRAWordDiff(c1, c2: TBGRAPixel): word;
begin
  result := ExpandedDiff(GammaExpansion(c1),GammaExpansion(c2));
end;

function BGRADiff(c1,c2: TBGRAPixel): byte;
begin
  result := ExpandedDiff(GammaExpansion(c1),GammaExpansion(c2)) shr 8;
end;

function FastBGRALinearDiff(c1, c2: TBGRAPixel): byte;
begin
  result := max(min((abs(c1.red-c2.red)+(abs(c1.green-c2.green) shl 1)+abs(c1.blue-c2.blue)) shr 2,
             min(c1.alpha,c2.alpha)), abs(c1.alpha-c2.alpha));
end;

function FastBGRAExpandedDiff(c1, c2: TBGRAPixel): word;
var wa1,wa2: word;
begin
  wa1 := c1.alpha shl 8 + c1.alpha;
  wa2 := (c2.alpha shl 8) + c2.alpha;
  result := max(min((abs(GammaExpansionTab[c1.red]-GammaExpansionTab[c2.red])+
               (abs(GammaExpansionTab[c1.green]-GammaExpansionTab[c2.green]) shl 1)+
               abs(GammaExpansionTab[c1.blue]-GammaExpansionTab[c2.blue])) shr 2,
             min(wa1,wa2)),
             abs(wa1-wa2));
end;

function MergeBGRA(const colors: array of TBGRAPixel): TBGRAPixel;
var
  sumR,sumG,sumB,sumA: NativeUInt;
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
    {$PUSH}{$R-}
    result := BGRA(intval shr 8,intval shr 16,intval,alpha);
    {$POP}
  end;
end;
{$ENDIF}

{$IFDEF INCLUDE_INIT}
{$UNDEF INCLUDE_INIT}
  BGRASetGamma();
{$ENDIF}
