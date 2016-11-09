{$IFDEF INCLUDE_INTERFACE}
{$UNDEF INCLUDE_INTERFACE}
type
  {* Possible channels in a bitmap using any RGBA colorspace }
  TChannel = (cRed, cGreen, cBlue, cAlpha);
  {** Combination of channels }
  TChannels = set of TChannel;

{ Gamma conversion arrays. Should be used as readonly }
var
  // TBGRAPixel -> TExpandedPixel
  GammaExpansionTab:   packed array[0..255] of word;

  // TExpandedPixel -> TBGRAPixel
  GammaCompressionTab: packed array[0..65535] of byte;

procedure BGRASetGamma(AGamma: single = 1.7);
function BGRAGetGamma: single;

type
  PExpandedPixel = ^TExpandedPixel;
  { TExpandedPixel }
  {* Stores a gamma expanded RGB color. Values range from 0 to 65535 }
  TExpandedPixel = packed record
    red, green, blue, alpha: word;
  end;
  TExpandedPixelBuffer = packed array of TExpandedPixel;

  procedure AllocateExpandedPixelBuffer(var ABuffer: TExpandedPixelBuffer; ASize: integer);

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
  {* General purpose color variable with single-precision floating point values }
  TColorF = packed array[1..4] of single;
  ArrayOfTColorF = array of TColorF;

  {** Creates a TColorF structure }
  function ColorF(red,green,blue,alpha: single): TColorF;
  function BGRAToColorF(c: TBGRAPixel; AGammaExpansion: boolean): TColorF; overload;
  function BGRAToColorF(const a: array of TBGRAPixel; AGammaExpansion: boolean): ArrayOfTColorF; overload;
  function ColorFToBGRA(c: TColorF; AGammaCompression: boolean): TBGRAPixel;
  function GammaCompressionF(c: TColorF): TColorF;
  function GammaExpansionF(c: TColorF): TColorF;
  {** Subtract each component separately }
  operator - (const c1, c2: TColorF): TColorF; inline;
  {** Add each component separately }
  operator + (const c1, c2: TColorF): TColorF; inline;
  {** Multiply each component separately }
  operator * (const c1, c2: TColorF): TColorF; inline;
  {** Multiply each component by ''factor'' }
  operator * (const c1: TColorF; factor: single): TColorF; inline;

type
  {* Pixel color defined in HSL colorspace. Values range from 0 to 65535 }

  { THSLAPixel }

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
  {** Converts a pixel from HSL colorspace to sRGB }
  function HSLAToBGRA(const c: THSLAPixel): TBGRAPixel;
  {** Converts a pixel from HSL colorspace to gamma expanded RGB }
  function HSLAToExpanded(const c: THSLAPixel): TExpandedPixel;
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
  { TBGRAPixelHelper }

  TBGRAPixelHelper = record helper for TBGRAPixel
    function ToExpanded: TExpandedPixel;
    procedure FromExpanded(const AValue: TExpandedPixel);
    function ToHSLAPixel: THSLAPixel;
    procedure FromHSLAPixel(const AValue: THSLAPixel);
    function ToGSBAPixel: TGSBAPixel;
    procedure FromGSBAPixel(const AValue: TGSBAPixel);
    function ToColorF(AGammaExpansion: boolean): TColorF;
    procedure FromColorF(const AValue: TColorF; AGammaCompression: boolean);
  end;

  { TExpandedPixelHelper }

  TExpandedPixelHelper = record helper for TExpandedPixel
    function ToBGRAPixel: TBGRAPixel;
    procedure FromBGRAPixel(AValue: TBGRAPixel);
    function ToHSLAPixel: THSLAPixel;
    procedure FromHSLAPixel(AValue: THSLAPixel);
  end;

  { THSLAPixelHelper }

  THSLAPixelHelper = record helper for THSLAPixel
    function ToBGRAPixel: TBGRAPixel;
    procedure FromBGRAPixel(AValue: TBGRAPixel);
    function ToExpanded: TExpandedPixel;
    procedure FromExpanded(AValue: TExpandedPixel);
  end;

Operator := (Source: TExpandedPixel): TBGRAPixel;
{$ENDIF}


{$IFDEF INCLUDE_IMPLEMENTATION}
{$UNDEF INCLUDE_IMPLEMENTATION}
{ TBGRAPixel }

function TBGRAPixel.GetClassIntensity: word;
begin
  result := GetIntensity(self);
end;

function TBGRAPixel.GetClassLightness: word;
begin
  result := GetLightness(self);
end;

procedure TBGRAPixel.SetClassIntensity(AValue: word);
begin
  self := SetIntensity(self, AValue);
end;

procedure TBGRAPixel.SetClassLightness(AValue: word);
begin
  self := SetLightness(self, AValue);
end;

procedure TBGRAPixel.FromRGB(ARed, AGreen, ABlue: Byte; AAlpha: Byte);
begin
  red := ARed;
  green := AGreen;
  blue := ABlue;
  alpha := AAlpha;
end;

procedure TBGRAPixel.FromColor(AColor: TColor; AAlpha: Byte);
begin
  if AColor = clNone then
    Self := BGRAPixelTransparent
  else
  begin
    RedGreenBlue(ColorToRGB(AColor), red,green,blue);
    alpha := AAlpha;
  end;
end;

procedure TBGRAPixel.FromString(AStr: string);
begin
  Self := StrToBGRA(AStr);
end;

procedure TBGRAPixel.FromFPColor(AColor: TFPColor);
begin
  self := FPColorToBGRA(AColor);
end;

procedure TBGRAPixel.ToRGB(out ARed, AGreen, ABlue, AAlpha: Byte);
begin
  ARed := red;
  AGreen := green;
  ABlue := blue;
  AAlpha := alpha;
end;

procedure TBGRAPixel.ToRGB(out ARed, AGreen, ABlue: Byte);
begin
  ARed := red;
  AGreen := green;
  ABlue := blue
end;

function TBGRAPixel.ToColor: TColor;
begin
  if alpha = 0 then
    result := clNone
  else
    result := RGBToColor(red,green,blue);
end;

function TBGRAPixel.ToString: string;
begin
  result := BGRAToStr(Self, CSSColors);
end;

function TBGRAPixel.ToGrayscale(AGammaCorrection: boolean): TBGRAPixel;
begin
  if AGammaCorrection then
    result := BGRAToGrayscale(self)
  else
    result := BGRAToGrayscaleLinear(self);
end;

function TBGRAPixel.ToFPColor: TFPColor;
begin
  result := BGRAToFPColor(Self);
end;

class operator TBGRAPixel.:=(Source: TBGRAPixel): TColor;
begin
  result := Source.ToColor;
end;

class operator TBGRAPixel.:=(Source: TColor): TBGRAPixel;
begin
  result.FromColor(Source);
end;

{ The gamma correction is approximated here by a power function }
var
  GammaExpFactor   : single; //exponent

const
  redWeightShl10   = 306; // = 0.299
  greenWeightShl10 = 601; // = 0.587
  blueWeightShl10  = 117; // = 0.114

procedure BGRANoGamma;
var i: integer;
begin
  GammaExpFactor := 1;
  for i := 0 to 255 do
    GammaExpansionTab[i] := (i shl 8) + i;
  for i := 0 to 65535 do
    GammaCompressionTab[i] := i shr 8;
end;

procedure BGRASetGamma(AGamma: single);
var
  GammaLinearFactor: single;
  i,j,prevpos,nextpos,midpos: NativeInt;
begin
  if AGamma = 1 then
  begin
    BGRANoGamma;
    exit;
  end;
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
  GammaCompressionTab[0] := 0;
end;

function BGRAGetGamma: single;
begin
  result := GammaExpFactor;
end;

procedure AllocateExpandedPixelBuffer(var ABuffer: TExpandedPixelBuffer;
  ASize: integer);
begin
  if ASize > length(ABuffer) then
    setlength(ABuffer, max(length(ABuffer)*2,ASize));
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

{ The intensity is defined here as the maximum value of any color component }
function GetIntensity(const c: TExpandedPixel): word; inline;
begin
  Result := c.red;
  if c.green > Result then
    Result := c.green;
  if c.blue > Result then
    Result := c.blue;
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

function ColorF(red, green, blue, alpha: single): TColorF;
begin
  result[1] := red;
  result[2] := green;
  result[3] := blue;
  result[4] := alpha;
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

function GammaExpansionF(c: TColorF): TColorF;
begin
  result := ColorF(power(c[1],GammaExpFactor),power(c[2],GammaExpFactor),power(c[3],GammaExpFactor),c[4]);
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

{ THSLAPixel }

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
  end else
  begin
    max := r;
    min := g;
  end;
  if b > max then
    max := b else
  if b < min then
    min := b;
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
    dest.saturation := 0 else
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

{ Conversion from HSL colorspace to RGB. See : http://en.wikipedia.org/wiki/HSL_color_space }
function HSLAToBGRA(const c: THSLAPixel): TBGRAPixel;
var ec: TExpandedPixel;
begin
  ec := HSLAToExpanded(c);
  Result := GammaCompression(ec);
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

{ TGSBAPixel }

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

{ TBGRAPixelHelper }

function TBGRAPixelHelper.ToExpanded: TExpandedPixel;
begin
  result := GammaExpansion(self);
end;

procedure TBGRAPixelHelper.FromExpanded(const AValue: TExpandedPixel);
begin
  Self := GammaCompression(AValue);
end;

function TBGRAPixelHelper.ToHSLAPixel: THSLAPixel;
begin
  result := BGRAToHSLA(Self);
end;

procedure TBGRAPixelHelper.FromHSLAPixel(const AValue: THSLAPixel);
begin
  Self := HSLAToBGRA(AValue);
end;

function TBGRAPixelHelper.ToGSBAPixel: TGSBAPixel;
begin
  result := BGRAToGSBA(Self);
end;

procedure TBGRAPixelHelper.FromGSBAPixel(const AValue: TGSBAPixel);
begin
  Self := GSBAToBGRA(AValue);
end;

function TBGRAPixelHelper.ToColorF(AGammaExpansion: boolean): TColorF;
begin
  result := BGRAToColorF(Self,AGammaExpansion);
end;

procedure TBGRAPixelHelper.FromColorF(const AValue: TColorF;
    AGammaCompression: boolean);
begin
  Self := ColorFToBGRA(AValue,AGammaCompression);
end;

{ TExpandedPixelHelper }

function TExpandedPixelHelper.ToBGRAPixel: TBGRAPixel;
begin
  result := GammaCompression(Self);
end;

procedure TExpandedPixelHelper.FromBGRAPixel(AValue: TBGRAPixel);
begin
  Self := GammaExpansion(AValue);
end;

function TExpandedPixelHelper.ToHSLAPixel: THSLAPixel;
begin
  result := ExpandedToHSLA(Self);
end;

procedure TExpandedPixelHelper.FromHSLAPixel(AValue: THSLAPixel);
begin
  Self := HSLAToExpanded(AValue);
end;

operator :=(Source: TExpandedPixel): TBGRAPixel;
begin
  result := GammaCompression(Source);
end;

{ THSLAPixelHelper }

function THSLAPixelHelper.ToBGRAPixel: TBGRAPixel;
begin
  result := HSLAToBGRA(self);
end;

procedure THSLAPixelHelper.FromBGRAPixel(AValue: TBGRAPixel);
begin
  self := BGRAToHSLA(AValue);
end;

function THSLAPixelHelper.ToExpanded: TExpandedPixel;
begin
  result := HSLAToExpanded(Self);
end;

procedure THSLAPixelHelper.FromExpanded(AValue: TExpandedPixel);
begin
  Self := ExpandedToHSLA(AValue);
end;
{$ENDIF}
