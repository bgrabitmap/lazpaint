unit BGRAColorInt;

{$mode objfpc}{$H+}
{$ifdef CPUI386}
  {$define BGRACOLORINT_USEASM}
{$endif}
{$ifdef DARWIN}
  {$undef BGRACOLORINT_USEASM}
{$endif}

interface

uses
  BGRABitmapTypes;

type
  TColorInt65536 = packed record
    r,g,b,a: integer;
  end;

function ColorInt65536(r,g,b,a: integer): TColorInt65536; inline; overload;
function ColorInt65536(r,g,b: integer): TColorInt65536; inline; overload;
function ColorFToColorInt65536(colorF: TColorF): TColorInt65536; inline;
function ColorInt65536ToColorF(color: TColorInt65536): TColorF;
operator +(const color1,color2: TColorInt65536): TColorInt65536; inline;
operator *(const color1,color2: TColorInt65536): TColorInt65536;
operator *(const color1: TColorInt65536; factor65536: integer): TColorInt65536;
function ColorIntToBGRA(const AColor: TColorInt65536; AGammaCompression: boolean = false): TBGRAPixel;
function BGRAToColorInt(const AColor: TBGRAPixel; AGammaExpansion: boolean = false): TColorInt65536;
function BGRAToColorIntMultiply(const color1: TBGRAPixel; const color2: TColorInt65536): TColorInt65536;

implementation

function ColorInt65536(r, g, b, a: integer): TColorInt65536;
begin
  result.r := r;
  result.g := g;
  result.b := b;
  result.a := a;
end;

function ColorInt65536(r, g, b: integer): TColorInt65536;
begin
  result.r := r;
  result.g := g;
  result.b := b;
  result.a := 65536;
end;

function ColorFToColorInt65536(colorF: TColorF): TColorInt65536;
begin
  result.r := round(colorF[1]*65536);
  result.g := round(colorF[2]*65536);
  result.b := round(colorF[3]*65536);
  result.a := round(colorF[4]*65536);
end;

function ColorInt65536ToColorF(color: TColorInt65536): TColorF;
const oneOver65536 = 1/65536;
begin
  result[1] := color.r*oneOver65536;
  result[2] := color.g*oneOver65536;
  result[3] := color.b*oneOver65536;
  result[4] := color.a*oneOver65536;
end;

operator+(const color1, color2: TColorInt65536): TColorInt65536;
begin
  result.r := color1.r+color2.r;
  result.g := color1.g+color2.g;
  result.b := color1.b+color2.b;
  result.a := color1.a+color2.a;
end;

operator*(const color1, color2: TColorInt65536): TColorInt65536;
{$ifdef BGRACOLORINT_USEASM} {$asmmode intel} assembler;
  asm
    push edx
    push ebx
    push esi
    mov ebx, Color1
    mov esi, Color2
    //ecx = @result

    mov eax, [ebx] //r
    imul [esi]
    shl edx, 16
    shr eax, 16
    or edx, eax
    mov [ecx], edx

    mov eax, [ebx+4] //g
    imul [esi+4]
    shl edx, 16
    shr eax, 16
    or edx, eax
    mov [ecx+4], edx

    mov eax, [ebx+8] //b
    imul [esi+8]
    shl edx, 16
    shr eax, 16
    or edx, eax
    mov [ecx+8], edx

    mov eax, [ebx+12] //a
    imul [esi+12]
    shl edx, 16
    shr eax, 16
    or edx, eax
    mov [ecx+12], edx

    pop esi
    pop ebx
    pop edx
  end;
{$ELSE}
begin
  result.r := int64(color1.r)*color2.r shr 16;
  result.g := int64(color1.g)*color2.g shr 16;
  result.b := int64(color1.b)*color2.b shr 16;
  result.a := int64(color1.a)*color2.a shr 16;
end;
{$ENDIF}

operator*(const color1: TColorInt65536; factor65536: integer): TColorInt65536;
{$ifdef BGRACOLORINT_USEASM} {$asmmode intel} assembler;
  asm
    push edx
    push ebx
    push esi
    mov ebx, Color1
    mov esi, factor65536
    //ecx = @result

    mov eax, [ebx] //r
    imul esi
    shl edx, 16
    shr eax, 16
    or edx, eax
    mov [ecx], edx

    mov eax, [ebx+4] //g
    imul esi
    shl edx, 16
    shr eax, 16
    or edx, eax
    mov [ecx+4], edx

    mov eax, [ebx+8] //b
    imul esi
    shl edx, 16
    shr eax, 16
    or edx, eax
    mov [ecx+8], edx

    mov eax, [ebx+12] //a
    imul esi
    shl edx, 16
    shr eax, 16
    or edx, eax
    mov [ecx+12], edx

    pop esi
    pop ebx
    pop edx
  end;
{$else}
var prod: int64;
begin
  prod := int64(color1.r)*factor65536;
  if prod >= 0 then result.r := prod shr 16
  else result.r := -((-prod) shr 16);
  prod := int64(color1.g)*factor65536;
  if prod >= 0 then result.g := prod shr 16
  else result.g := -((-prod) shr 16);
  prod := int64(color1.b)*factor65536;
  if prod >= 0 then result.b := prod shr 16
  else result.b := -((-prod) shr 16);
  prod := int64(color1.a)*factor65536;
  if prod >= 0 then result.a := prod shr 16
  else result.a := -((-prod) shr 16);
end;
{$endif}

function BGRAToColorInt(const AColor: TBGRAPixel; AGammaExpansion: boolean): TColorInt65536;
begin
  if AGammaExpansion then
  begin
    result.r := GammaExpansionTab[AColor.red] + (AColor.red shr 7);
    result.g := GammaExpansionTab[AColor.green] + (AColor.green shr 7);
    result.b := GammaExpansionTab[AColor.blue] + (AColor.blue shr 7);
  end else
  begin
    result.r := AColor.red shl 8 + AColor.red + (AColor.red shr 7);
    result.g := AColor.green shl 8 + AColor.green + (AColor.green shr 7);
    result.b := AColor.blue shl 8 + AColor.blue + (AColor.blue shr 7);
  end;
  result.a := AColor.alpha shl 8 + AColor.alpha+ (AColor.alpha shr 7);
end;

function BGRAToColorIntMultiply(const color1: TBGRAPixel;
  const color2: TColorInt65536): TColorInt65536;
{$ifdef BGRACOLORINT_USEASM} {$asmmode intel} assembler;
  asm
    push ebx
    push esi

    mov esi, Color2
    mov ebx, result
    mov ecx, [Color1]

    mov eax, ecx
    shr eax, TBGRAPixel_RedShift
    and eax, 255
    mov edx, eax
    shr edx, 7
    add eax, edx
    imul [esi]
    shl edx, 24
    shr eax, 8
    or edx, eax
    mov [ebx], edx

    mov eax, ecx
    shr eax, TBGRAPixel_GreenShift
    and eax, 255
    mov edx, eax
    shr edx, 7
    add eax, edx
    imul [esi+4]
    shl edx, 24
    shr eax, 8
    or edx, eax
    mov [ebx+4], edx

    mov eax, ecx
    shr eax, TBGRAPixel_BlueShift
    and eax, 255
    mov edx, eax
    shr edx, 7
    add eax, edx
    imul [esi+8]
    shl edx, 24
    shr eax, 8
    or edx, eax
    mov [ebx+8], edx

    mov eax, ecx
    shr eax, TBGRAPixel_AlphaShift
    and eax, 255
    mov edx, eax
    shr edx, 7
    add eax, edx
    imul [esi+12]
    shl edx, 24
    shr eax, 8
    or edx, eax
    mov [ebx+12], edx

    pop esi
    pop ebx
  end;
{$ELSE}
begin
  result.r := int64(color2.r)*(color1.red shr 7+color1.red) shr 8;
  result.g := int64(color2.g)*(color1.green shr 7+color1.green) shr 8;
  result.b := int64(color2.b)*(color1.blue shr 7+color1.blue) shr 8;
  result.a := int64(color2.a)*(color1.alpha shr 7+color1.alpha) shr 8;
end;
{$ENDIF}

function ColorIntToBGRA(const AColor: TColorInt65536; AGammaCompression: boolean): TBGRAPixel;
var maxValue,invMaxValue,r,g,b: integer;
begin
  if AColor.a <= 0 then
    result.alpha := 0;
  if AColor.a >= 65536 then
    result.alpha := 255
  else
    result.alpha := AColor.a shr 8 - (AColor.a shr 15);

  maxValue := AColor.r;
  if AColor.g > maxValue then maxValue := AColor.g;
  if AColor.b > maxValue then maxValue := AColor.b;

  if maxValue <= 0 then
  begin
    result.red := 0;
    result.green := 0;
    result.blue := 0;
    exit;
  end;

  if AGammaCompression then
  begin
    if maxValue <= 65535 then
    begin
      if AColor.r <= 0 then result.red := 0 else
        result.red := GammaCompressionTab[AColor.r - (AColor.r shr 15)];

      if AColor.g <= 0 then result.green := 0 else
        result.green :=GammaCompressionTab[AColor.g - (AColor.g shr 15)];

      if AColor.b <= 0 then result.blue := 0 else
        result.blue := GammaCompressionTab[AColor.b - (AColor.b shr 15)];
      exit;
    end;

    invMaxValue := (1073741824+maxValue-1) div maxValue;

    maxValue := (maxValue-65535) shr 1;
    if AColor.r < 0 then r := maxValue else
      r := AColor.r*invMaxValue shr 14 + maxValue;
    if AColor.g < 0 then g := maxValue else
      g := AColor.g*invMaxValue shr 14 + maxValue;
    if AColor.b < 0 then b := maxValue else
      b := AColor.b*invMaxValue shr 14 + maxValue;

    if r >= 65535 then result.red := 255 else
      result.red := GammaCompressionTab[r];
    if g >= 65535 then result.green := 255 else
        result.green := GammaCompressionTab[g];
    if b >= 65535 then result.blue := 255 else
      result.blue := GammaCompressionTab[b];
  end else
  begin
    if maxValue <= 65535 then
    begin
      if AColor.r <= 0 then result.red := 0 else
        result.red := AColor.r shr 8 - (AColor.r shr 15);

      if AColor.g <= 0 then result.green := 0 else
        result.green := AColor.g shr 8 - (AColor.g shr 15);

      if AColor.b <= 0 then result.blue := 0 else
        result.blue := AColor.b shr 8 - (AColor.b shr 15);
      exit;
    end;

    invMaxValue := (1073741824+maxValue-1) div maxValue;

    maxValue := (maxValue-65535) shr 9;
    if AColor.r < 0 then r := maxValue else
      r := AColor.r*invMaxValue shr 22 + maxValue;
    if AColor.g < 0 then g := maxValue else
      g := AColor.g*invMaxValue shr 22 + maxValue;
    if AColor.b < 0 then b := maxValue else
      b := AColor.b*invMaxValue shr 22 + maxValue;

    if r >= 255 then result.red := 255 else
      result.red := r;
    if g >= 255 then result.green := 255 else
        result.green := g;
    if b >= 255 then result.blue := 255 else
      result.blue := b;
  end;
end;


end.

