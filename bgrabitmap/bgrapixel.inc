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

  {$IFDEF BGRABITMAP_RGBAPIXEL}
  TBGRAPixel = packed record red, green, blue, alpha: byte;
               end;
  {$ELSE}
  {* Each pixel is a sequence of 4 bytes containing blue, green, red and alpha channel.
     Values range from 0 to 255, color is in sRGB colorspace. The alpha value of 0
     is transparent and 255 is opaque. In the bitmap data, when the pixel is fully transparent,
	 the RGB values are supposed to be set to zero. }
  TBGRAPixel = packed record blue, green, red, alpha: byte;
               end;
  {$ENDIF}

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
  {** Computes the difference (with gamma correction) between two pixels,
      taking into account all dimensions, including transparency. The
      result ranges from 0 to 65535 }
  function BGRAWordDiff(c1, c2: TBGRAPixel): word;
  {** Computes the difference (with gamma correction) between two pixels,
      taking into account all dimensions, including transparency. The
      result ranges from 0 to 255 }
  function BGRADiff(c1, c2: TBGRAPixel): byte;

  function Color16BitToBGRA(AColor: Word): TBGRAPixel;
  function BGRAToColor16Bit(const AColor: TBGRAPixel): Word;

type
  {* Array of pixels }
  ArrayOfTBGRAPixel = array of TBGRAPixel;
  {** Merge given colors without gamma correction }
  function MergeBGRA(const colors: array of TBGRAPixel): TBGRAPixel; overload;

type
  PExpandedPixel = ^TExpandedPixel;
  {* Stores a gamma expanded RGB color. Values range from 0 to 65535 }
  TExpandedPixel = packed record red, green, blue, alpha: word;
                   end;

  {** Converts a pixel from sRGB to gamma expanded RGB }
  function GammaExpansion(c: TBGRAPixel): TExpandedPixel; inline;
  {** Converts a pixel from gamma expanded RGB to sRGB }
  function GammaCompression(const ec: TExpandedPixel): TBGRAPixel; inline; overload;
  {** Converts a pixel from gamma expanded RGB to sRGB }
  function GammaCompression(red,green,blue,alpha: word): TBGRAPixel; inline; overload;
  {** Returns the intensity of an gamma-expanded pixel. The intensity is the
     maximum value reached by any component }
  function GetIntensity(const c: TExpandedPixel): word; inline;
  {** Sets the intensity of a gamma-expanded pixel }
  function SetIntensity(const c: TExpandedPixel; intensity: word): TExpandedPixel;
  {** Returns the lightness of an gamma-expanded pixel. The lightness is the
     perceived brightness, 0 being black and 65535 being white }
  function GetLightness(const c: TExpandedPixel): word; inline;
  {** Sets the lightness of a gamma-expanded pixel }
  function SetLightness(const c: TExpandedPixel; lightness: word): TExpandedPixel;
  {** Sets the lightness of a gamma expanded pixel, provided you already know the current
     value of lightness ''curLightness''. It is a bit faster than the previous function }
  function SetLightness(const c: TExpandedPixel; lightness: word; curLightness: word): TExpandedPixel;
  {** Returns the importance of the color. It is similar to saturation
      in HSL colorspace, except it is gamma corrected. A value of zero indicates
      a black/gray/white, and a value of 65535 indicates a bright color }
  function ColorImportance(ec: TExpandedPixel): word;
  {** Merge two gamma expanded pixels (so taking into account gamma correction) }
  function MergeBGRA(ec1, ec2: TExpandedPixel): TExpandedPixel; overload;
  {** Computes the difference (with gamma correction) between two pixels,
      taking into account all dimensions, including transparency. The
      result ranges from 0 to 65535 }
  function ExpandedDiff(ec1, ec2: TExpandedPixel): word;

type
  {* Pixel color defined in HSL colorspace. Values range from 0 to 65535 }
  THSLAPixel = packed record
    {** Hue of the pixel. Extremum values 0 and 65535 are red }
    hue: word;
    {** Saturation of the color. 0 is gray and 65535 is the brightest color }
    saturation: word;
    {** Lightness of the color. 0 is black, 32768 is normal, and 65535 is white }
    lightness: word;
    {** Opacity of the pixel. 0 is transparent and 65535 is opaque }
    alpha: word;
  end;

  {** Creates a pixel with given HSLA values, where A stands for alpha }
  function HSLA(hue, saturation, lightness, alpha: word): THSLAPixel; overload; inline;
  {** Creates an opaque pixel with given HSL values }
  function HSLA(hue, saturation, lightness: word): THSLAPixel; overload; inline;
  {** Converts a pixel from sRGB to HSL color space }
  function BGRAToHSLA(c: TBGRAPixel): THSLAPixel;
  {** Converts a pixel from gamma expanded RGB to HSL color space }
  function ExpandedToHSLA(const ec: TExpandedPixel): THSLAPixel;
  {** Converts a pixel from HSL colorspace to gamma expanded RGB }
  function HSLAToExpanded(const c: THSLAPixel): TExpandedPixel;
  {** Converts a pixel from HSL colorspace to sRGB }
  function HSLAToBGRA(const c: THSLAPixel): TBGRAPixel;
  {** Computes the hue difference }
  function HueDiff(h1, h2: word): word;
  {** Returns the hue of a gamma expanded pixel }
  function GetHue(ec: TExpandedPixel): word;

type
  {* Pixel color defined in corrected HSL colorspace. G stands for corrected hue
     and B stands for actual brightness. Values range from 0 to 65535 }
  TGSBAPixel = THSLAPixel;

  {** Converts a pixel from sRGB to correct HSL color space }
  function BGRAToGSBA(c: TBGRAPixel): TGSBAPixel;
  {** Converts a pixel from gamma expanded RGB to correct HSL color space }
  function ExpandedToGSBA(ec: TExpandedPixel): TGSBAPixel;
  {** Converts a G hue (GSBA) to a H hue (HSLA) }
  function GtoH(ghue: word): word;
  {** Converts a H hue (HSLA) to a G hue (GSBA) }
  function HtoG(hue: word): word;
  {** Converts a pixel from corrected HSL to sRGB }
  function GSBAToBGRA(c: TGSBAPixel): TBGRAPixel;
  {** Converts a pixel from correct HSL to gamma expanded RGB }
  function GSBAToExpanded(c: TGSBAPixel): TExpandedPixel;
  {** Converts a pixel from correct HSL to usual HSL }
  function GSBAToHSLA(c: TGSBAPixel): THSLAPixel;

type
  {* General purpose color variable with single-precision floating point values }
  TColorF = packed array[1..4] of single;
  ArrayOfTColorF = array of TColorF;

  {** Creates a TColorF structure }
  function ColorF(red,green,blue,alpha: single): TColorF;
  function BGRAToColorF(c: TBGRAPixel; AGammaExpansion: boolean): TColorF; overload;
  function BGRAToColorF(const a: array of TBGRAPixel; AGammaExpansion: boolean): ArrayOfTColorF; overload;
  function ColorFToBGRA(c: TColorF; AGammaCompression: boolean): TBGRAPixel;
  function GammaCompressionF(c: TColorF): TColorF;
  {** Subtract each component separately }
  operator - (const c1, c2: TColorF): TColorF; inline;
  {** Add each component separately }
  operator + (const c1, c2: TColorF): TColorF; inline;
  {** Multiply each component separately }
  operator * (const c1, c2: TColorF): TColorF; inline;
  {** Multiply each component by ''factor'' }
  operator * (const c1: TColorF; factor: single): TColorF; inline;

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
    boXor);

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
     'Negation', 'LinearNegation', 'Xor');

  {** Returns the blend mode expressed by the string }
  function StrToBlendOperation(str: string): TBlendOperation;

type
  {* Possible channels in a bitmap using any RGBA colorspace }
  TChannel = (cRed, cGreen, cBlue, cAlpha);
  {** Combination of channels }
  TChannels = set of TChannel;
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

procedure BGRASetGamma(AGamma: single = 1.7);
function BGRAGetGamma: single;

{$ENDIF}

{$IFDEF INCLUDE_IMPLEMENTATION}
{$UNDEF INCLUDE_IMPLEMENTATION}
{ The gamma correction is approximated here by a power function }
var
  GammaExpFactor   : single; //exponent

const
  redWeightShl10   = 306; // = 0.299
  greenWeightShl10 = 601; // = 0.587
  blueWeightShl10  = 117; // = 0.114

procedure BGRASetGamma(AGamma: single);
var
  GammaLinearFactor: single;
  i,j,prevpos,nextpos,midpos: NativeInt;
begin
  GammaExpFactor := AGamma;
  //the linear factor is used to normalize expanded values in the range 0..65535
  GammaLinearFactor := 65535 / power(255, GammaExpFactor);
  GammaExpansionTab[0] := 0;
  GammaCompressionTab[0] := 0;
  nextpos := 0;
  for i := 0 to 255 do
  begin
    prevpos := nextpos;
    midpos := round(power(i, GammaExpFactor) * GammaLinearFactor);
    if i = 255 then
      nextpos := 65536
    else
      nextpos := round(power(i+0.5, GammaExpFactor) * GammaLinearFactor);
    GammaExpansionTab[i] := midpos;
    for j := prevpos to nextpos-1 do
      GammaCompressionTab[j] := i;
  end;
end;

function BGRAGetGamma: single;
begin
  result := GammaExpFactor;
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

{************************** Color functions **************************}

{ The intensity is defined here as the maximum value of any color component }
function GetIntensity(const c: TExpandedPixel): word; inline;
begin
  Result := c.red;
  if c.green > Result then
    Result := c.green;
  if c.blue > Result then
    Result := c.blue;
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

function SetIntensity(const c: TExpandedPixel; intensity: word): TExpandedPixel;
var
  curIntensity: word;
begin
  curIntensity := GetIntensity(c);
  if curIntensity = 0 then //suppose it's gray if there is no color information
  begin
    Result.red := intensity;
    Result.green := intensity;
    Result.blue := intensity;
    result.alpha := c.alpha;
  end
  else
  begin
    //linear interpolation to reached wanted intensity
    Result.red   := (c.red * intensity + (curIntensity shr 1)) div curIntensity;
    Result.green := (c.green * intensity + (curIntensity shr 1)) div curIntensity;
    Result.blue  := (c.blue * intensity + (curIntensity shr 1)) div curIntensity;
    Result.alpha := c.alpha;
  end;
end;

function SetIntensity(c: TBGRAPixel; intensity: word): TBGRAPixel;
begin
  result := GammaCompression(SetIntensity(GammaExpansion(c),intensity));
end;

function GetLightness(c: TBGRAPixel): word;
begin
  result := GetLightness(GammaExpansion(c));
end;

{ The lightness here is defined as the subjective sensation of luminosity, where
  blue is the darkest component and green the lightest }
function GetLightness(const c: TExpandedPixel): word; inline;
begin
  Result := (c.red * redWeightShl10 + c.green * greenWeightShl10 +
    c.blue * blueWeightShl10 + 512) shr 10;
end;

function SetLightness(const c: TExpandedPixel; lightness: word): TExpandedPixel;
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

function SetLightness(c: TBGRAPixel; lightness: word): TBGRAPixel;
begin
  result := GammaCompression(SetLightness(GammaExpansion(c),lightness));
end;

function SetLightness(const c: TExpandedPixel; lightness: word; curLightness: word): TExpandedPixel;
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

function BGRAToGSBA(c: TBGRAPixel): TGSBAPixel;
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

function ExpandedToGSBA(ec: TExpandedPixel): TGSBAPixel;
var lightness: UInt32Or64;
    red,green,blue: Int32or64;
begin
  red   := ec.red;
  green := ec.green;
  blue  := ec.blue;
  result.alpha := ec.alpha;

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

function GSBAToBGRA(c: TGSBAPixel): TBGRAPixel;
var ec: TExpandedPixel;
    lightness: word;
begin
  c.hue := GtoH(c.hue);
  lightness := c.lightness;
  c.lightness := 32768;
  ec := HSLAToExpanded(c);
  result := GammaCompression(SetLightness(ec, lightness));
end;

function GSBAToExpanded(c: TGSBAPixel): TExpandedPixel;
var lightness: word;
begin
  c.hue := GtoH(c.hue);
  lightness := c.lightness;
  c.lightness := 32768;
  result := SetLightness(HSLAToExpanded(c),lightness);
end;

function GSBAToHSLA(c: TGSBAPixel): THSLAPixel;
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
  Result := RGBToColor(c.red, c.green, c.blue);
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

function BGRAToColorF(c: TBGRAPixel; AGammaExpansion: boolean): TColorF;
const OneOver255 = 1/255;
      OneOver65535 = 1/65535;
begin
  if not AGammaExpansion then
  begin
    result[1] := c.red*OneOver255;
    result[2] := c.green*OneOver255;
    result[3] := c.blue*OneOver255;
    result[4] := c.alpha*OneOver255;
  end else
  with GammaExpansion(c) do
  begin
    result[1] := red*OneOver65535;
    result[2] := green*OneOver65535;
    result[3] := blue*OneOver65535;
    result[4] := alpha*OneOver65535;
  end;
end;

function BGRAToColorF(const a: array of TBGRAPixel; AGammaExpansion: boolean
  ): ArrayOfTColorF;
var
  i: NativeInt;
begin
  setlength(result, length(a));
  for i := 0 to high(a) do
    result[i] := BGRAToColorF(a[i],AGammaExpansion);
end;

function ColorFToBGRA(c: TColorF; AGammaCompression: boolean): TBGRAPixel;
begin
  if not AGammaCompression then
  begin
    result.red := Min(255,Max(0,round(c[1]*255)));
    result.green := Min(255,Max(0,round(c[1]*255)));
    result.blue := Min(255,Max(0,round(c[1]*255)));
  end else
  begin
    result.red := GammaCompressionTab[Min(65535,Max(0,round(c[1]*65535)))];
    result.green := GammaCompressionTab[Min(65535,Max(0,round(c[1]*65535)))];
    result.blue := GammaCompressionTab[Min(65535,Max(0,round(c[1]*65535)))];
  end;
  result.alpha := Min(255,Max(0,round(c[4]*255)));
end;

function GammaCompressionF(c: TColorF): TColorF;
var inv: single;
begin
  inv := 1/GammaExpFactor;
  result := ColorF(power(c[1],inv),power(c[2],inv),power(c[3],inv),c[4]);
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
