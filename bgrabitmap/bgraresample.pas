unit BGRAResample;

{$mode objfpc}{$H+}

interface

{ This unit provides resampling functions, i.e. resizing of bitmaps with or
  without interpolation filters.

  SimpleStretch does a fast stretch by splitting the image into zones defined
  by integers. This can be quite ugly.

  FineResample uses floating point coordinates to get an antialiased resample.
  It can use minimal interpolation (4 pixels when upsizing) for simple interpolation
  filters (linear and cosine-like) or wide kernel resample for complex interpolation.
  In this cas, it calls WideKernelResample.

  WideKernelResample can be called by custom filter kernel, derived
  from TWideKernelFilter. It is slower of course than simple interpolation. }

uses
  SysUtils, BGRABitmapTypes;

{------------------------------- Simple stretch ------------------------------------}

function SimpleStretch(bmp: TBGRACustomBitmap;
  NewWidth, NewHeight: integer): TBGRACustomBitmap;

{---------------------------- Interpolation filters --------------------------------}

function FineInterpolation(t: single; ResampleFilter: TResampleFilter): single;
function FineInterpolation256(t256: integer; ResampleFilter: TResampleFilter): integer;

type
  TWideKernelFilter = class
    function Interpolation(t: single): single; virtual; abstract;
    function ShouldCheckRange: boolean; virtual; abstract;
    function KernelWidth: single; virtual; abstract;
  end;

  TMitchellKernel = class(TWideKernelFilter)
    function Interpolation(t: single): single; override;
    function ShouldCheckRange: boolean; override;
    function KernelWidth: single; override;
  end;

  { TSplineKernel }

  TSplineKernel = class(TWideKernelFilter)
  public
    Coeff: single;
    constructor Create;
    constructor Create(ACoeff: single);
    function Interpolation(t: single): single; override;
    function ShouldCheckRange: boolean; override;
    function KernelWidth: single; override;
  end;

  { TCubicKernel }

  TCubicKernel = class(TWideKernelFilter)
    function pow3(x: single): single; inline;
    function Interpolation(t: single): single; override;
    function ShouldCheckRange: boolean; override;
    function KernelWidth: single; override;
  end;

function CreateInterpolator(style: TSplineStyle): TWideKernelFilter;

{-------------------------------- Fine resample ------------------------------------}

function FineResample(bmp: TBGRACustomBitmap;
  NewWidth, NewHeight: integer; ResampleFilter: TResampleFilter): TBGRACustomBitmap;

function WideKernelResample(bmp: TBGRACustomBitmap;
  NewWidth, NewHeight: integer; ResampleFilterSmaller, ResampleFilterLarger: TWideKernelFilter): TBGRACustomBitmap;

implementation

uses GraphType, Math;

{-------------------------------- Simple stretch ------------------------------------}

function FastSimpleStretchLarger(bmp: TBGRACustomBitmap;
  xFactor, yFactor: integer): TBGRACustomBitmap;
var
  y_src, yb, y_dest: integer;

  x_src, xb: integer;
  srcColor:  TBGRAPixel;

  PSrc:  PBGRAPixel;
  PDest: array of PBGRAPixel;
  temp:  PBGRAPixel;

begin
  if (xFactor < 1) or (yFactor < 1) then
    raise ERangeError.Create('FastSimpleStretchLarger: New dimensions must be greater or equal (*'+IntToStr(xFactor)+'x*'+IntToStr(yFactor)+')');

  Result := bmp.NewBitmap(bmp.Width * xFactor, bmp.Height * yFactor);
  if (Result.Width = 0) or (Result.Height = 0) then
    exit;

  bmp.LoadFromBitmapIfNeeded;

  SetLength(PDest, yFactor);
  y_dest := 0;
  for y_src := 0 to bmp.Height - 1 do
  begin
    PSrc := bmp.Scanline[y_src];
    for yb := 0 to yFactor - 1 do
      PDest[yb] := Result.scanLine[y_dest + yb];

    for x_src := 0 to bmp.Width - 1 do
    begin
      srcColor := PSrc^;
      Inc(PSrc);

      for yb := 0 to yFactor - 1 do
      begin
        temp := PDest[yb];
        for xb := 0 to xFactor - 1 do
        begin
          temp^ := srcColor;
          Inc(temp);
        end;
        PDest[yb] := temp;
      end;
    end;
    Inc(y_dest, yFactor);
  end;

  Result.InvalidateBitmap;
end;

function SimpleStretchLarger(bmp: TBGRACustomBitmap;
  newWidth, newHeight: integer): TBGRACustomBitmap;
var
  x_src, y_src: integer;
  inc_x_dest, mod_x_dest, acc_x_dest, inc_y_dest, mod_y_dest, acc_y_dest: integer;
  x_dest, y_dest, prev_x_dest, prev_y_dest: integer;

  xb, yb:      integer;
  srcColor:    TBGRAPixel;
  PDest, PSrc: PBGRAPixel;
  delta, lineDelta: integer;

begin
  if (newWidth < bmp.Width) or (newHeight < bmp.Height) then
    raise ERangeError.Create('SimpleStretchLarger: New dimensions must be greater or equal ('+IntToStr(bmp.Width)+'x'+IntToStr(bmp.Height)+'->'+IntToStr(newWidth)+'x'+IntToStr(newHeight)+')');

  if ((newWidth div bmp.Width) * bmp.Width = newWidth) and
    ((newHeight div bmp.Height) * bmp.Height = newHeight) then
  begin
    Result := FastSimpleStretchLarger(bmp, newWidth div bmp.Width,
      newHeight div bmp.Height);
    exit;
  end;

  Result := bmp.NewBitmap(NewWidth, NewHeight);
  if (newWidth = 0) or (newHeight = 0) then
    exit;

  bmp.LoadFromBitmapIfNeeded;

  inc_x_dest := newwidth div bmp.Width;
  mod_x_dest := newwidth mod bmp.Width;
  inc_y_dest := newheight div bmp.Height;
  mod_y_dest := newheight mod bmp.Height;

  y_dest     := 0;
  acc_y_dest := bmp.Height div 2;
  if Result.LineOrder = riloTopToBottom then
    lineDelta := newWidth
  else
    lineDelta := -newWidth;
  for y_src := 0 to bmp.Height - 1 do
  begin
    prev_y_dest := y_dest;
    Inc(y_dest, inc_y_dest);
    Inc(acc_y_dest, mod_y_dest);
    if acc_y_dest >= bmp.Height then
    begin
      Dec(acc_y_dest, bmp.Height);
      Inc(y_dest);
    end;

    PSrc := bmp.Scanline[y_src];

    x_dest     := 0;
    acc_x_dest := bmp.Width div 2;
    for x_src := 0 to bmp.Width - 1 do
    begin
      prev_x_dest := x_dest;
      Inc(x_dest, inc_x_dest);
      Inc(acc_x_dest, mod_x_dest);
      if acc_x_dest >= bmp.Width then
      begin
        Dec(acc_x_dest, bmp.Width);
        Inc(x_dest);
      end;

      srcColor := PSrc^;
      Inc(PSrc);

      PDest := Result.scanline[prev_y_dest] + prev_x_dest;
      delta := lineDelta - (x_dest - prev_x_dest);
      for yb := prev_y_dest to y_dest - 1 do
      begin
        for xb := prev_x_dest to x_dest - 1 do
        begin
          PDest^ := srcColor;
          Inc(PDest);
        end;
        Inc(PDest, delta);
      end;
    end;
  end;
  Result.InvalidateBitmap;
end;

function SimpleStretchSmallerFactor2(source: TBGRACustomBitmap): TBGRACustomBitmap;
var xb,yb: integer;
    pdest: PBGRAPixel;
    psrc1,psrc2: PBGRAPixel;
    asum: integer;
    a1,a2,a3,a4: integer;
    newWidth,newHeight: integer;
begin
  newWidth := source.Width div 2;
  newHeight := source.Height div 2;
  result := source.NewBitmap(newWidth,newHeight);
  for yb := 0 to newHeight-1 do
  begin
    pdest := result.ScanLine[yb];
    psrc1 := source.Scanline[yb shl 1];
    psrc2 := source.Scanline[yb shl 1+1];
    for xb := newWidth-1 downto 0 do
    begin
      asum := psrc1^.alpha + (psrc1+1)^.alpha + psrc2^.alpha + (psrc2+1)^.alpha;
      if asum = 0 then
        pdest^ := BGRAPixelTransparent
      else if asum = 1020 then
      begin
        pdest^.alpha := 255;
        pdest^.red := (psrc1^.red + (psrc1+1)^.red + psrc2^.red + (psrc2+1)^.red + 2) shr 2;
        pdest^.green := (psrc1^.green + (psrc1+1)^.green + psrc2^.green + (psrc2+1)^.green+ 2) shr 2;
        pdest^.blue := (psrc1^.blue + (psrc1+1)^.blue + psrc2^.blue + (psrc2+1)^.blue+ 2) shr 2;
      end else
      begin
        pdest^.alpha := asum shr 2;
        a1 := psrc1^.alpha;
        a2 := (psrc1+1)^.alpha;
        a3 := psrc2^.alpha;
        a4 := (psrc2+1)^.alpha;
        pdest^.red := (psrc1^.red*a1 + (psrc1+1)^.red*a2 + psrc2^.red*a3 + (psrc2+1)^.red*a4 + (asum shr 1)) div asum;
        pdest^.green := (psrc1^.green*a1 + (psrc1+1)^.green*a2 + psrc2^.green*a3 + (psrc2+1)^.green*a4+ (asum shr 1)) div asum;
        pdest^.blue := (psrc1^.blue*a1 + (psrc1+1)^.blue*a2 + psrc2^.blue*a3 + (psrc2+1)^.blue*a4+ (asum shr 1)) div asum;
      end;
      inc(psrc1,2);
      inc(psrc2,2);
      inc(pdest);
    end;
  end;
end;

function SimpleStretchSmallerFactor4(source: TBGRACustomBitmap): TBGRACustomBitmap;
var xb,yb: integer;
    pdest: PBGRAPixel;
    psrc1,psrc2,psrc3,psrc4: PBGRAPixel;
    asum: integer;
    a1,a2,a3,a4,
    a5,a6,a7,a8,
    a9,a10,a11,a12,
    a13,a14,a15,a16: integer;
    newWidth,newHeight: integer;
begin
  newWidth := source.Width div 4;
  newHeight := source.Height div 4;
  result := source.NewBitmap(newWidth,newHeight);
  for yb := 0 to newHeight-1 do
  begin
    pdest := result.ScanLine[yb];
    psrc1 := source.Scanline[yb shl 2];
    psrc2 := source.Scanline[yb shl 2+1];
    psrc3 := source.Scanline[yb shl 2+2];
    psrc4 := source.Scanline[yb shl 2+3];
    for xb := newWidth-1 downto 0 do
    begin
      asum := psrc1^.alpha + (psrc1+1)^.alpha + (psrc1+2)^.alpha + (psrc1+3)^.alpha +
              psrc2^.alpha + (psrc2+1)^.alpha + (psrc2+2)^.alpha + (psrc2+3)^.alpha +
              psrc3^.alpha + (psrc3+1)^.alpha + (psrc3+2)^.alpha + (psrc3+3)^.alpha +
              psrc4^.alpha + (psrc4+1)^.alpha + (psrc4+2)^.alpha + (psrc4+3)^.alpha;
      if asum = 0 then
        pdest^ := BGRAPixelTransparent
      else if asum = 4080 then
      begin
        pdest^.alpha := 255;
        pdest^.red := (psrc1^.red + (psrc1+1)^.red + (psrc1+2)^.red + (psrc1+3)^.red +
              psrc2^.red + (psrc2+1)^.red + (psrc2+2)^.red + (psrc2+3)^.red +
              psrc3^.red + (psrc3+1)^.red + (psrc3+2)^.red + (psrc3+3)^.red +
              psrc4^.red + (psrc4+1)^.red + (psrc4+2)^.red + (psrc4+3)^.red + 8) shr 4;
        pdest^.green := (psrc1^.green + (psrc1+1)^.green + (psrc1+2)^.green + (psrc1+3)^.green +
              psrc2^.green + (psrc2+1)^.green + (psrc2+2)^.green + (psrc2+3)^.green +
              psrc3^.green + (psrc3+1)^.green + (psrc3+2)^.green + (psrc3+3)^.green +
              psrc4^.green + (psrc4+1)^.green + (psrc4+2)^.green + (psrc4+3)^.green + 8) shr 4;
        pdest^.blue := (psrc1^.blue + (psrc1+1)^.blue + (psrc1+2)^.blue + (psrc1+3)^.blue +
              psrc2^.blue + (psrc2+1)^.blue + (psrc2+2)^.blue + (psrc2+3)^.blue +
              psrc3^.blue + (psrc3+1)^.blue + (psrc3+2)^.blue + (psrc3+3)^.blue +
              psrc4^.blue + (psrc4+1)^.blue + (psrc4+2)^.blue + (psrc4+3)^.blue + 8) shr 4;
      end else
      begin
        pdest^.alpha := asum shr 4;
        a1 := psrc1^.alpha;
        a2 := (psrc1+1)^.alpha;
        a3 := (psrc1+2)^.alpha;
        a4 := (psrc1+3)^.alpha;
        a5 := psrc2^.alpha;
        a6 := (psrc2+1)^.alpha;
        a7 := (psrc2+2)^.alpha;
        a8 := (psrc2+3)^.alpha;
        a9 := psrc3^.alpha;
        a10 := (psrc3+1)^.alpha;
        a11 := (psrc3+2)^.alpha;
        a12 := (psrc3+3)^.alpha;
        a13 := psrc4^.alpha;
        a14 := (psrc4+1)^.alpha;
        a15 := (psrc4+2)^.alpha;
        a16 := (psrc4+3)^.alpha;
        pdest^.red := (psrc1^.red*a1 + (psrc1+1)^.red*a2 + (psrc1+2)^.red*a3 + (psrc1+3)^.red*a4 +
              psrc2^.red*a5 + (psrc2+1)^.red*a6 + (psrc2+2)^.red*a7 + (psrc2+3)^.red*a8 +
              psrc3^.red*a9 + (psrc3+1)^.red*a10 + (psrc3+2)^.red*a11 + (psrc3+3)^.red*a12 +
              psrc4^.red*a13 + (psrc4+1)^.red*a14 + (psrc4+2)^.red*a15 + (psrc4+3)^.red*a16 + (asum shr 1)) div asum;
        pdest^.green := (psrc1^.green*a1 + (psrc1+1)^.green*a2 + (psrc1+2)^.green*a3 + (psrc1+3)^.green*a4 +
              psrc2^.green*a5 + (psrc2+1)^.green*a6 + (psrc2+2)^.green*a7 + (psrc2+3)^.green*a8 +
              psrc3^.green*a9 + (psrc3+1)^.green*a10 + (psrc3+2)^.green*a11 + (psrc3+3)^.green*a12 +
              psrc4^.green*a13 + (psrc4+1)^.green*a14 + (psrc4+2)^.green*a15 + (psrc4+3)^.green*a16 + (asum shr 1)) div asum;
        pdest^.blue := (psrc1^.blue*a1 + (psrc1+1)^.blue*a2 + (psrc1+2)^.blue*a3 + (psrc1+3)^.blue*a4 +
              psrc2^.blue*a5 + (psrc2+1)^.blue*a6 + (psrc2+2)^.blue*a7 + (psrc2+3)^.blue*a8 +
              psrc3^.blue*a9 + (psrc3+1)^.blue*a10 + (psrc3+2)^.blue*a11 + (psrc3+3)^.blue*a12 +
              psrc4^.blue*a13 + (psrc4+1)^.blue*a14 + (psrc4+2)^.blue*a15 + (psrc4+3)^.blue*a16 + (asum shr 1)) div asum;
      end;
      inc(psrc1,4);
      inc(psrc2,4);
      inc(psrc3,4);
      inc(psrc4,4);
      inc(pdest);
    end;
  end;
end;

function SimpleStretchSmallerFactor(source: TBGRACustomBitmap; fx,fy: integer): TBGRACustomBitmap;
var xb,yb,ys,iy,ix: integer;
    pdest: PBGRAPixel;
    psrc: array of PBGRAPixel;
    psrci: PBGRAPixel;
    asum,maxsum: integer;
    newWidth,newHeight: integer;
    r,g,b,nbi: integer;
begin
  newWidth := source.Width div fx;
  newHeight := source.Height div fy;
  result := source.NewBitmap(newWidth,newHeight);
  ys := 0;
  maxsum := 255*fx*fy;
  nbi := fx*fy;
  setlength(psrc, fy);
  for yb := 0 to newHeight-1 do
  begin
    pdest := result.ScanLine[yb];
    for iy := fy-1 downto 0 do
    begin
      psrc[iy] := source.Scanline[ys];
      inc(ys);
    end;
    for xb := newWidth-1 downto 0 do
    begin
      asum := 0;
      for iy := fy-1 downto 0 do
      begin
        psrci := psrc[iy];
        for ix := fx-1 downto 0 do
          asum += (psrci+ix)^.alpha;
      end;
      if asum = 0 then
        pdest^ := BGRAPixelTransparent
      else if asum = maxsum then
      begin
        pdest^.alpha := 255;
        r := 0;
        g := 0;
        b := 0;
        for iy := fy-1 downto 0 do
        begin
          psrci := psrc[iy];
          for ix := fx-1 downto 0 do
          begin
            with (psrci+ix)^ do
            begin
              r += red;
              g += green;
              b += blue;
            end;
          end;
        end;
        pdest^.red := (r + (nbi shr 1)) div nbi;
        pdest^.green := (g + (nbi shr 1)) div nbi;
        pdest^.blue := (b + (nbi shr 1)) div nbi;
      end else
      begin
        pdest^.alpha := (asum + (nbi shr 1)) div nbi;
        r := 0;
        g := 0;
        b := 0;
        for iy := fy-1 downto 0 do
        begin
          psrci := psrc[iy];
          for ix := fx-1 downto 0 do
          begin
            with (psrci+ix)^ do
            begin
              r += integer(red)*integer(alpha);
              g += integer(green)*integer(alpha);
              b += integer(blue)*integer(alpha);
            end;
          end;
        end;
        pdest^.red := (r + (asum shr 1)) div asum;
        pdest^.green := (g + (asum shr 1)) div asum;
        pdest^.blue := (b + (asum shr 1)) div asum;
      end;
      for iy := fy-1 downto 0 do
        inc(psrc[iy],fx);
      inc(pdest);
    end;
  end;
end;

function SimpleStretchSmaller(bmp: TBGRACustomBitmap;
  newWidth, newHeight: integer): TBGRACustomBitmap;
var
  x_dest, y_dest: integer;
  inc_x_src, mod_x_src, acc_x_src, inc_y_src, mod_y_src, acc_y_src: integer;
  x_src, y_src, prev_x_src, prev_y_src: integer;
  x_src2, y_src2: integer;

  xb, yb: integer;
  v1, v2, v3, v4, v4shr1: int64;
  nb,a:     integer;
  pdest, psrc, psrcscan: PBGRAPixel;
  lineDelta, delta: integer;

begin
  if (newWidth > bmp.Width) or (newHeight > bmp.Height) then
    raise ERangeError.Create('SimpleStretchSmaller: New dimensions must be smaller or equal ('+IntToStr(bmp.Width)+'x'+IntToStr(bmp.Height)+'->'+IntToStr(newWidth)+'x'+IntToStr(newHeight)+')');

  if (newWidth = 0) or (newHeight = 0) or (bmp.Width = 0) or (bmp.Height = 0) then
  begin
    Result := bmp.NewBitmap(NewWidth, NewHeight);
    exit;
  end;

  if (newWidth*2 = bmp.Width) and (newHeight*2 = bmp.Height) then
  begin
    result := SimpleStretchSmallerFactor2(bmp);
    exit
  end
  else
  if (newWidth*4 = bmp.Width) and (newHeight*4 = bmp.Height) then
  begin
    result := SimpleStretchSmallerFactor4(bmp);
    exit;
  end
  else
  if (newWidth < bmp.Width) and (newHeight < bmp.Height) and
     (bmp.Width mod newWidth = 0) and (bmp.Height mod newHeight = 0) then
  begin
    result := SimpleStretchSmallerFactor(bmp, bmp.Width div newWidth, bmp.Height div newHeight);
    exit;
  end;

  Result := bmp.NewBitmap(NewWidth, NewHeight);

  bmp.LoadFromBitmapIfNeeded;

  inc_x_src := bmp.Width div newWidth;
  mod_x_src := bmp.Width mod newWidth;
  inc_y_src := bmp.Height div newHeight;
  mod_y_src := bmp.Height mod newHeight;

  if bmp.lineOrder = riloTopToBottom then
    lineDelta := bmp.Width
  else
    lineDelta := -bmp.Width;

  y_src     := 0;
  acc_y_src := 0;
  for y_dest := 0 to newHeight - 1 do
  begin
    PDest := Result.ScanLine[y_dest];

    prev_y_src := y_src;
    Inc(y_src, inc_y_src);
    Inc(acc_y_src, mod_y_src);
    if acc_y_src >= newHeight then
    begin
      Dec(acc_y_src, newHeight);
      Inc(y_src);
    end;
    if y_src > prev_y_src then
      y_src2 := y_src - 1
    else
      y_src2 := y_src;
    psrcscan := bmp.Scanline[prev_y_src];

    x_src     := 0;
    acc_x_src := 0;
    for x_dest := 0 to newWidth - 1 do
    begin
      prev_x_src := x_src;
      Inc(x_src, inc_x_src);
      Inc(acc_x_src, mod_x_src);
      if acc_x_src >= newWidth then
      begin
        Dec(acc_x_src, newWidth);
        Inc(x_src);
      end;
      if x_src > prev_x_src then
        x_src2 := x_src - 1
      else
        x_src2 := x_src;

      v1    := 0;
      v2    := 0;
      v3    := 0;
      v4    := 0;
      nb    := 0;
      delta := lineDelta - (x_src2 - prev_x_src + 1);

      PSrc  := psrcscan + prev_x_src;
      for yb := prev_y_src to y_src2 do
      begin
        for xb := prev_x_src to x_src2 do
        begin
          with PSrc^ do
          begin
            a := alpha;
                    {$HINTS OFF}
            v1 += integer(red) * a;
            v2 += integer(green) * a;
            v3 += integer(blue) * a;
                    {$HINTS ON}
          end;
          v4 += a;
          Inc(PSrc);
          Inc(nb);
        end;
        Inc(PSrc, delta);
      end;

      if (v4 <> 0) and (nb <> 0) then
      begin
        v4shr1  := v4 shr 1;
        with PDest^ do
        begin
          red   := (v1 + v4shr1) div v4;
          green := (v2 + v4shr1) div v4;
          blue  := (v3 + v4shr1) div v4;
          alpha := (v4 + (nb shr 1)) div nb;
        end;
      end
      else
       PDest^ := BGRAPixelTransparent;

      Inc(PDest);
    end;
  end;
  Result.InvalidateBitmap;
end;

function SimpleStretch(bmp: TBGRACustomBitmap;
  NewWidth, NewHeight: integer): TBGRACustomBitmap;
var
  temp, newtemp: TBGRACustomBitmap;
begin
  if (NewWidth = bmp.Width) and (NewHeight = bmp.Height) then
    Result := bmp.Duplicate
  else
  if (NewWidth >= bmp.Width) and (NewHeight >= bmp.Height) then
    Result := SimpleStretchLarger(bmp, NewWidth, NewHeight)
  else
  if (NewWidth <= bmp.Width) and (NewHeight <= bmp.Height) then
    Result := SimpleStretchSmaller(bmp, NewWidth, NewHeight)
  else
  begin
    temp := bmp;

    if NewWidth < bmp.Width then
    begin
      newtemp := SimpleStretchSmaller(temp, NewWidth, temp.Height);
      if (temp <> bmp) then
        temp.Free;
      temp := newtemp;
    end;

    if NewHeight < bmp.Height then
    begin
      newtemp := SimpleStretchSmaller(temp, temp.Width, NewHeight);
      if (temp <> bmp) then
        temp.Free;
      temp := newtemp;
    end;

    if NewWidth > bmp.Width then
    begin
      newtemp := SimpleStretchLarger(temp, NewWidth, temp.Height);
      if (temp <> bmp) then
        temp.Free;
      temp := newtemp;
    end;

    if NewHeight > bmp.Height then
    begin
      newtemp := SimpleStretchLarger(temp, temp.Width, NewHeight);
      if (temp <> bmp) then
        temp.Free;
      temp := newtemp;
    end;

    if temp <> bmp then
      Result := temp
    else
      Result := bmp.Duplicate;
  end;
end;

{---------------------------- Interpolation filters ----------------------------------------}

function FineInterpolation(t: single; ResampleFilter: TResampleFilter): single;
begin
  if ResampleFilter = rfLinear then
    result := t else
  begin
    if t <= 0.5 then
      result := t*t*2 else
      result := 1-(1-t)*(1-t)*2;
    if ResampleFilter <> rfCosine then result := (result+t)*0.5;
  end;
end;

function FineInterpolation256(t256: integer; ResampleFilter: TResampleFilter): integer;
begin
  if ResampleFilter = rfLinear then
    result := t256 else
  begin
    if t256 <= 128 then
      result := (t256*t256) shr 7 else
      result := 256 - (((256-t256)*(256-t256)) shr 7);
    if ResampleFilter <> rfCosine then result := (result+t256) shr 1;
  end;
end;

{ TCubicKernel }

function TCubicKernel.pow3(x: single): single;
begin
  if x <= 0.0 then
   result:=0.0
  else
   result:=x * x * x;
end;

function TCubicKernel.Interpolation(t: single): single;
const globalfactor = 1/6;
begin
   if t > 2 then
     result := 0
   else
     result:= globalfactor *
       (pow3(t + 2 ) - 4 * pow3(t + 1 ) + 6 * pow3(t ) - 4 * pow3(t - 1 ) );
end;

function TCubicKernel.ShouldCheckRange: boolean;
begin
  Result:= false;
end;

function TCubicKernel.KernelWidth: single;
begin
  Result:= 2;
end;

{ TMitchellKernel }

function TMitchellKernel.Interpolation(t: single): single;
var
  tt, ttt: single;
const OneEighteenth = 1 / 18;
begin
  t := Abs(t);
  tt := Sqr(t);
  ttt := tt * t;
  if t < 1 then Result := (21 * ttt - 36 * tt + 16 ) * OneEighteenth
  else if t < 2 then Result := (- 7 * ttt + 36 * tt - 60 * t + 32) * OneEighteenth
  else Result := 0;
end;

function TMitchellKernel.ShouldCheckRange: Boolean;
begin
  Result := True;
end;

function TMitchellKernel.KernelWidth: single;
begin
  Result := 2;
end;

{ TSplineKernel }

constructor TSplineKernel.Create;
begin
  coeff := 0.5;
end;

constructor TSplineKernel.Create(ACoeff: single);
begin
  Coeff := ACoeff;
end;

function TSplineKernel.Interpolation(t: single): single;
var
  tt, ttt: single;
begin
  t := Abs(t);
  tt := Sqr(t);
  ttt := tt * t;
  if t < 1 then
    Result := (2 - Coeff) * ttt - (3 - Coeff) * tt + 1
  else if t < 2 then
    Result := -Coeff * (ttt - 5 * tt + 8 * t - 4)
  else
    Result := 0;
end;

function TSplineKernel.ShouldCheckRange: Boolean;
begin
  Result := True;
end;

function TSplineKernel.KernelWidth: single;
begin
  Result := 2;
end;

{--------------------------------------------- Fine resample ------------------------------------------------}

function FineResampleLarger(bmp: TBGRACustomBitmap;
  newWidth, newHeight: integer; ResampleFilter: TResampleFilter): TBGRACustomBitmap;
type
  TInterpolationEntry = record
    isrc1,isrc2,factCorr: integer;
  end;
var
  yb, xb: integer;
  pdest,psrc1,psrc2:  PBGRAPixel;
  xsrc, ysrc, xfactor, yfactor: double;
  xTab,yTab: array of TInterpolationEntry;
  xInfo,yInfo: TInterpolationEntry;
  cUpLeft, cUpRight, cLowLeft, cLowRight: TBGRAPixel;
  factHoriz, factVert: single;
  fUpLeft, fUpRight, fLowLeft, fLowRight: integer;
  faUpLeft, faUpRight, faLowLeft, faLowRight: integer;
  rSum, gSum, bSum, aSum: integer;
  temp:   TBGRACustomBitmap;
begin
  if (newWidth < bmp.Width) or (newHeight < bmp.Height) then
    raise ERangeError.Create('FineResampleLarger: New dimensions must be greater or equal ('+IntToStr(bmp.Width)+'x'+IntToStr(bmp.Height)+'->'+IntToStr(newWidth)+'x'+IntToStr(newHeight)+')');

  if (newWidth = 0) or (newHeight = 0) then
  begin
    Result := bmp.NewBitmap(NewWidth, NewHeight);
    exit;
  end;

  bmp.LoadFromBitmapIfNeeded;

  if (bmp.Width = 1) and (bmp.Height = 1) then
  begin
    Result := bmp.NewBitmap(NewWidth, NewHeight);
    Result.Fill(bmp.GetPixel(0, 0));
    exit;
  end
  else
  if bmp.Width = 1 then
  begin
    temp := bmp.NewBitmap(2, bmp.Height);
    temp.PutImage(0, 0, bmp, dmSet);
    temp.PutImage(1, 0, bmp, dmSet);
    Result := FineResampleLarger(temp, 2, newHeight, ResampleFilter);
    temp.Free;
    temp := Result;
    Result := SimpleStretch(temp, newWidth,temp.Height);
    temp.Free;
    exit;
  end
  else
  if bmp.Height = 1 then
  begin
    temp := bmp.NewBitmap(bmp.Width, 2);
    temp.PutImage(0, 0, bmp, dmSet);
    temp.PutImage(0, 1, bmp, dmSet);
    Result := FineResampleLarger(temp, newWidth, 2, ResampleFilter);
    temp.Free;
    temp := Result;
    Result := SimpleStretch(temp, temp.Width,newHeight);
    temp.Free;
    exit;
  end;

  Result := bmp.NewBitmap(NewWidth, NewHeight);
  yfactor := (bmp.Height - 1) / (newHeight - 1);
  xfactor := (bmp.Width - 1) / (newWidth - 1);

  setlength(yTab, newHeight);
  for yb := 0 to newHeight - 1 do
  begin
    ysrc     := yb * yfactor;
    factVert := frac(ysrc);
    yTab[yb].isrc1   := floor(ysrc);
    yTab[yb].isrc2 := min(bmp.Height-1, ceil(ysrc));
    yTab[yb].factCorr := round(FineInterpolation(factVert,ResampleFilter)*256);
  end;
  setlength(xTab, newWidth);
  for xb := 0 to newWidth - 1 do
  begin
    xsrc     := xb * xfactor;
    factHoriz := frac(xsrc);
    xTab[xb].isrc1   := floor(xsrc);
    xTab[xb].isrc2 := min(bmp.Width-1,ceil(xsrc));
    xTab[xb].factCorr := round(FineInterpolation(factHoriz,ResampleFilter)*256);
  end;

  for yb := 0 to newHeight - 1 do
  begin
    pdest    := Result.Scanline[yb];
    yInfo    := yTab[yb];
    psrc1    := bmp.scanline[yInfo.isrc1];
    psrc2    := bmp.scanline[yInfo.isrc2];
    for xb := 0 to newWidth - 1 do
    begin
      xInfo  := xTab[xb];

      cUpLeft   := (psrc1 + xInfo.isrc1)^;
      cUpRight  := (psrc1 + xInfo.isrc2)^;
      cLowLeft  := (psrc2 + xInfo.isrc1)^;
      cLowRight := (psrc2 + xInfo.isrc2)^;

      fLowRight := (xInfo.factCorr * yInfo.factCorr + 128) shr 8;
      fLowLeft := yInfo.factCorr - fLowRight;
      fUpRight := xInfo.factCorr - fLowRight;
      fUpLeft := (256 - xInfo.factCorr) - fLowLeft;

      faUpLeft   := fUpLeft * cUpLeft.alpha;
      faUpRight  := fUpRight * cUpRight.alpha;
      faLowLeft  := fLowLeft * cLowLeft.alpha;
      faLowRight := fLowRight * cLowRight.alpha;

      rSum := cUpLeft.red * faUpLeft + cUpRight.red * faUpRight +
        cLowLeft.red * faLowLeft + cLowRight.red * faLowRight;
      gSum := cUpLeft.green * faUpLeft + cUpRight.green * faUpRight +
        cLowLeft.green * faLowLeft + cLowRight.green * faLowRight;
      bSum := cUpLeft.blue * faUpLeft + cUpRight.blue * faUpRight +
        cLowLeft.blue * faLowLeft + cLowRight.blue * faLowRight;
      aSum := cUpLeft.alpha * fUpLeft + cUpRight.alpha * fUpRight +
        cLowLeft.alpha * fLowLeft + cLowRight.alpha * fLowRight;

      if aSum = 0 then
        pdest^ := BGRAPixelTransparent
      else
        pdest^ := BGRA((rSum + aSum shr 1) div aSum, (gSum + aSum shr 1) div aSum,
          (bSum + aSum shr 1) div aSum, (aSum + 128) shr 8);
      Inc(pdest);

    end;
  end;
end;

function FineResampleSmaller(bmp: TBGRACustomBitmap;
  newWidth, newHeight: integer): TBGRACustomBitmap;
var
  yb, xb, yb2, xb2: integer;
  pdest, psrc:      PBGRAPixel;
  lineDelta, delta: integer;
  xsrc1, ysrc1, xsrc2, ysrc2, xfactor, yfactor: double;
  ixsrc1, ixsrc2, iysrc1, iysrc2, ixsrc1p1, ixsrc2m1, iysrc1p1, iysrc2m1: integer;
  cBorder, cFull, cUpLeft, cUpRight, cLowLeft, cLowRight: TBGRAPixel;
  factHoriz1, factHoriz2, factVert1, factVert2, Sum, fUpLeft, fUpRight,
  fLowLeft, fLowRight, faUpLeft, faUpRight, faLowLeft, faLowRight: single;
  rSum, gSum, bSum, aSum: double;
begin
  if (newWidth > bmp.Width) or (newHeight > bmp.Height) then
    raise ERangeError.Create('FineResampleSmaller: New dimensions must be smaller or equal ('+IntToStr(bmp.Width)+'x'+IntToStr(bmp.Height)+'->'+IntToStr(newWidth)+'x'+IntToStr(newHeight)+')');
  Result := bmp.NewBitmap(NewWidth, NewHeight);
  if (newWidth = 0) or (newHeight = 0) or (bmp.Width = 0) or (bmp.Height = 0) then
    exit;

  bmp.LoadFromBitmapIfNeeded;

  if bmp.lineOrder = riloTopToBottom then
    lineDelta := bmp.Width
  else
    lineDelta := -bmp.Width;

  yfactor := bmp.Height / newHeight;
  xfactor := bmp.Width / newWidth;
  for yb := 0 to newHeight - 1 do
  begin
    pdest  := Result.Scanline[yb];
    ysrc1  := yb * yfactor;
    ysrc2  := (yb + 1) * yfactor;
    iysrc1 := trunc(ysrc1);
    if (int(ysrc2) = int(ysrc1)) or (ysrc2 = iysrc1 + 1) then
    begin
      iysrc2    := iysrc1;
      factVert1 := 1;
      factVert2 := 0;
    end
    else
    begin
      iysrc2    := trunc(ysrc2);
      factVert1 := 1 - frac(ysrc1);
      factVert2 := frac(ysrc2);
    end;
    for xb := 0 to newWidth - 1 do
    begin
      xsrc1  := xb * xfactor;
      xsrc2  := (xb + 1) * xfactor;
      ixsrc1 := trunc(xsrc1);
      if (int(xsrc2) = int(xsrc1)) or (xsrc2 = ixsrc1 + 1) then
      begin
        ixsrc2     := ixsrc1;
        factHoriz1 := 1;
        factHoriz2 := 0;
      end
      else
      begin
        ixsrc2     := trunc(xsrc2);
        factHoriz1 := 1 - frac(xsrc1);
        factHoriz2 := frac(xsrc2);
      end;

      cUpLeft   := bmp.GetPixel(ixsrc1, iysrc1);
      cUpRight  := bmp.GetPixel(ixsrc2, iysrc1);
      cLowLeft  := bmp.GetPixel(ixsrc1, iysrc2);
      cLowRight := bmp.GetPixel(ixsrc2, iysrc2);

      fUpLeft   := factHoriz1 * factVert1;
      fUpRight  := factHoriz2 * factVert1;
      fLowLeft  := factHoriz1 * factVert2;
      fLowRight := factHoriz2 * factVert2;

      faUpLeft   := fUpLeft * cUpLeft.alpha;
      faUpRight  := fUpRight * cUpRight.alpha;
      faLowLeft  := fLowLeft * cLowLeft.alpha;
      faLowRight := fLowRight * cLowRight.alpha;

      Sum  := fUpLeft + fUpRight + fLowLeft + fLowRight;
      rSum := cUpLeft.red * faUpLeft + cUpRight.red * faUpRight +
        cLowLeft.red * faLowLeft + cLowRight.red * faLowRight;
      gSum := cUpLeft.green * faUpLeft + cUpRight.green * faUpRight +
        cLowLeft.green * faLowLeft + cLowRight.green * faLowRight;
      bSum := cUpLeft.blue * faUpLeft + cUpRight.blue * faUpRight +
        cLowLeft.blue * faLowLeft + cLowRight.blue * faLowRight;
      aSum := cUpLeft.alpha * fUpLeft + cUpRight.alpha * fUpRight +
        cLowLeft.alpha * fLowLeft + cLowRight.alpha * fLowRight;

      ixsrc1p1 := ixsrc1 + 1;
      ixsrc2m1 := ixsrc2 - 1;
      iysrc1p1 := iysrc1 + 1;
      iysrc2m1 := iysrc2 - 1;

      if ixsrc2m1 >= ixsrc1p1 then
      begin
        psrc := bmp.scanline[iysrc1] + ixsrc1p1;
        for xb2 := ixsrc1p1 to ixsrc2m1 do
        begin
          cBorder := psrc^;
          Inc(psrc);
          rSum += cBorder.red * cBorder.alpha * factVert1;
          gSum += cBorder.green * cBorder.alpha * factVert1;
          bSum += cBorder.blue * cBorder.alpha * factVert1;
          aSum += cBorder.alpha * factVert1;
          Sum  += factVert1;
        end;

        if (factVert2 <> 0) and (iysrc2 < bmp.Height) then
        begin
          psrc := bmp.scanline[iysrc2] + ixsrc1p1;
          for xb2 := ixsrc1p1 to ixsrc2m1 do
          begin
            cBorder := psrc^;
            Inc(psrc);
            rSum += cBorder.red * cBorder.alpha * factVert2;
            gSum += cBorder.green * cBorder.alpha * factVert2;
            bSum += cBorder.blue * cBorder.alpha * factVert2;
            aSum += cBorder.alpha * factVert2;
            Sum  += factVert2;
          end;
        end;
      end;

      if iysrc2m1 >= iysrc1p1 then
      begin
        psrc := bmp.scanline[iysrc1p1] + ixsrc1;
        for yb2 := iysrc1p1 to iysrc2m1 do
        begin
          cBorder := psrc^;
          Inc(psrc, lineDelta);
          rSum += cBorder.red * cBorder.alpha * factHoriz1;
          gSum += cBorder.green * cBorder.alpha * factHoriz1;
          bSum += cBorder.blue * cBorder.alpha * factHoriz1;
          aSum += cBorder.alpha * factHoriz1;
          Sum  += factHoriz1;
        end;

        if (factHoriz2 <> 0) and (ixsrc2 < bmp.Width) then
        begin
          psrc := bmp.scanline[iysrc1p1] + ixsrc2;
          for yb2 := iysrc1p1 to iysrc2m1 do
          begin
            cBorder := psrc^;
            Inc(psrc, lineDelta);
            rSum += cBorder.red * cBorder.alpha * factHoriz2;
            gSum += cBorder.green * cBorder.alpha * factHoriz2;
            bSum += cBorder.blue * cBorder.alpha * factHoriz2;
            aSum += cBorder.alpha * factHoriz2;
            Sum  += factHoriz2;
          end;
        end;
      end;

      if (ixsrc2m1 >= ixsrc1p1) and (iysrc2m1 >= iysrc1p1) then
      begin
        delta := lineDelta - (ixsrc2m1 - ixsrc1p1 + 1);
        psrc  := bmp.scanline[iysrc1p1] + ixsrc1p1;
        for yb2 := iysrc1p1 to iysrc2m1 do
        begin
          for xb2 := ixsrc1p1 to ixsrc2m1 do
          begin
            cFull := psrc^;
            rSum  += cFull.red * cFull.alpha;
            gSum  += cFull.green * cFull.alpha;
            bSum  += cFull.blue * cFull.alpha;
            aSum  += cFull.alpha;
            Sum   += 1;
            Inc(psrc);
          end;
          Inc(psrc, delta);
        end;
      end;

      if aSum = 0 then
        pdest^ := BGRAPixelTransparent
      else
        pdest^ := BGRA(round(rSum / aSum), round(gSum / aSum),
          round(bSum / aSum), round(aSum / Sum));
      Inc(pdest);

    end;
  end;
end;

function CreateInterpolator(style: TSplineStyle): TWideKernelFilter;
begin
  case Style of
    ssInside, ssInsideWithEnds: result := TCubicKernel.Create;
    ssCrossing, ssCrossingWithEnds: result := TMitchellKernel.Create;
    ssOutside: result := TSplineKernel.Create(0.5);
    ssRoundOutside: result := TSplineKernel.Create(0.75);
    ssVertexToSide: result := TSplineKernel.Create(1);
  else
    raise Exception.Create('Unknown spline style');
  end;
end;

function FineResample(bmp: TBGRACustomBitmap;
  NewWidth, NewHeight: integer; ResampleFilter: TResampleFilter): TBGRACustomBitmap;
var
  temp, newtemp: TBGRACustomBitmap;
  tempFilter1,tempFilter2: TWideKernelFilter;
begin
  case ResampleFilter of
    rfBicubic: //blur
    begin
      tempFilter1 := TCubicKernel.Create;
      result := WideKernelResample(bmp,NewWidth,NewHeight,tempFilter1,tempFilter1);
      tempFilter1.Free;
      exit;
    end;
    rfMitchell:
    begin
      tempFilter1 := TMitchellKernel.Create;
      result := WideKernelResample(bmp,NewWidth,NewHeight,tempFilter1,tempFilter1);
      tempFilter1.Free;
      exit;
    end;
    rfSpline:
    begin
      tempFilter1 := TSplineKernel.Create;
      result := WideKernelResample(bmp,NewWidth,NewHeight,tempFilter1,tempFilter1);
      tempFilter1.Free;
      exit;
    end;
    rfBestQuality:
    begin
      tempFilter1 := TSplineKernel.Create;
      tempFilter2 := TMitchellKernel.Create;
      result := WideKernelResample(bmp,NewWidth,NewHeight,tempFilter2,tempFilter1);
      tempFilter1.Free;
      tempFilter2.Free;
      exit;
    end;
  end;

  if (NewWidth = bmp.Width) and (NewHeight = bmp.Height) then
    Result := bmp.Duplicate
  else
  if (NewWidth >= bmp.Width) and (NewHeight >= bmp.Height) then
    Result := FineResampleLarger(bmp, NewWidth, NewHeight, ResampleFilter)
  else
  if (NewWidth <= bmp.Width) and (NewHeight <= bmp.Height) then
    Result := FineResampleSmaller(bmp, NewWidth, NewHeight)
  else
  begin
    temp := bmp;

    if NewWidth < bmp.Width then
    begin
      newtemp := FineResampleSmaller(temp, NewWidth, temp.Height);
      if (temp <> bmp) then
        temp.Free;
      temp := newtemp;
    end;

    if NewHeight < bmp.Height then
    begin
      newtemp := FineResampleSmaller(temp, temp.Width, NewHeight);
      if (temp <> bmp) then
        temp.Free;
      temp := newtemp;
    end;

    if NewWidth > bmp.Width then
    begin
      newtemp := FineResampleLarger(temp, NewWidth, temp.Height, ResampleFilter);
      if (temp <> bmp) then
        temp.Free;
      temp := newtemp;
    end;

    if NewHeight > bmp.Height then
    begin
      newtemp := FineResampleLarger(temp, temp.Width, NewHeight, ResampleFilter);
      if (temp <> bmp) then
        temp.Free;
      temp := newtemp;
    end;

    if temp <> bmp then
      Result := temp
    else
      Result := bmp.Duplicate;
  end;
end;

{------------------------ Wide kernel filtering adapted from Graphics32 ---------------------------}

function Constrain(const Value, Lo, Hi: Integer): Integer;
begin
  if Value < Lo then
  	Result := Lo
  else if Value > Hi then
  	Result := Hi
  else
  	Result := Value;
end;

type
  TPointRec = record
    Pos: Integer;
    Weight: Single;
  end;

  TCluster = array of TPointRec;
  TMappingTable = array of TCluster;

{$warnings off}
function BuildMappingTable(
  DstLo, DstHi: Integer;
  ClipLo, ClipHi: Integer;
  SrcLo, SrcHi: Integer;
  KernelSmaller,KernelLarger: TWideKernelFilter): TMappingTable;
Const FullEdge = false;
var
  SrcW, DstW, ClipW: Integer;
  FilterWidth: Single;
  Scale, OldScale: Single;
  Center: Single;
  Left, Right: Integer;
  I, J, K: Integer;
  Weight: Single;
begin
  SrcW := SrcHi - SrcLo;
  DstW := DstHi - DstLo;
  ClipW := ClipHi - ClipLo;
  if SrcW = 0 then
  begin
    Result := nil;
    Exit;
  end
  else if SrcW = 1 then
  begin
    SetLength(Result, ClipW);
    for I := 0 to ClipW - 1 do
    begin
      SetLength(Result[I], 1);
      Result[I][0].Pos := 0;
      Result[I][0].Weight := 1;
    end;
    Exit;
  end;
  SetLength(Result, ClipW);
  if ClipW = 0 then Exit;

  if FullEdge then Scale := DstW / SrcW
  else Scale := (DstW - 1) / (SrcW - 1);

  K := 0;

  if Scale = 0 then
  begin
    SetLength(Result[0], 1);
    Result[0][0].Pos := (SrcLo + SrcHi) div 2;
    Result[0][0].Weight := 1;
  end
  else if Scale < 1 then
  begin
    FilterWidth := KernelSmaller.KernelWidth;
    OldScale := Scale;
    Scale := 1 / Scale;
    FilterWidth := FilterWidth * Scale;
    for I := 0 to ClipW - 1 do
    begin
      if FullEdge then
        Center := SrcLo - 0.5 + (I - DstLo + ClipLo + 0.5) * Scale
      else
        Center := SrcLo + (I - DstLo + ClipLo) * Scale;
      Left := Floor(Center - FilterWidth);
      Right := Ceil(Center + FilterWidth);
      for J := Left to Right do
      begin
        Weight := KernelSmaller.Interpolation((Center - J) * OldScale) * OldScale;
        if Weight <> 0 then
        begin
          K := Length(Result[I]);
          SetLength(Result[I], K + 1);
          Result[I][K].Pos := Constrain(J, SrcLo, SrcHi - 1);
          Result[I][K].Weight := Weight;
        end;
      end;
      if Length(Result[I]) = 0 then
      begin
        SetLength(Result[I], 1);
        Result[I][0].Pos := Floor(Center);
        Result[I][0].Weight := 1;
      end;
    end;
  end
  else // scale > 1
  begin
    FilterWidth := KernelLarger.KernelWidth;
    Scale := 1 / Scale;
    for I := 0 to ClipW - 1 do
    begin
      if FullEdge then
        Center := SrcLo - 0.5 + (I - DstLo + ClipLo + 0.5) * Scale
      else
        Center := SrcLo + (I - DstLo + ClipLo) * Scale;
      Left := Floor(Center - FilterWidth);
      Right := Ceil(Center + FilterWidth);
      for J := Left to Right do
      begin
        Weight := KernelLarger.Interpolation(Center - j);
        if Weight <> 0 then
        begin
          K := Length(Result[I]);
          SetLength(Result[I], k + 1);
          Result[I][K].Pos := Constrain(j, SrcLo, SrcHi - 1);
          Result[I][K].Weight := Weight;
        end;
      end;
    end;
  end;
end;
{$warnings on}

function WideKernelResample(bmp: TBGRACustomBitmap;
  NewWidth, NewHeight: integer; ResampleFilterSmaller, ResampleFilterLarger: TWideKernelFilter): TBGRACustomBitmap;
type
  TSum = record
    sumR,sumG,sumB,sumA: single;
  end;

var
  mapX,mapY: TMappingTable;
  xb,yb,xc,yc,MapXLoPos,MapXHiPos: integer;
  clusterX,clusterY: TCluster;
  verticalSum: array of TSum;
  scanlinesSrc: array of PBGRAPixel;
  sum: TSum;
  c: TBGRAPixel;
  w,wa: single;
  pdest: PBGRAPixel;
begin
  result := bmp.NewBitmap(NewWidth,NewHeight);
  if (NewWidth=0) or (NewHeight=0) then exit;
  mapX := BuildMappingTable(0,NewWidth,0,NewWidth,0,bmp.Width,ResampleFilterSmaller,ResampleFilterLarger);
  mapY := BuildMappingTable(0,NewHeight,0,NewHeight,0,bmp.Height,ResampleFilterSmaller,ResampleFilterLarger);

  MapXLoPos := MapX[0][0].Pos;
  MapXHiPos := MapX[NewWidth - 1][High(MapX[NewWidth - 1])].Pos;

  setlength(verticalSum, MapXHiPos-MapXLoPos+1);

  setlength(scanlinesSrc, bmp.Height);
  for yb := 0 to bmp.Height-1 do
    scanlinesSrc[yb] := bmp.ScanLine[yb];

  for yb := 0 to NewHeight-1 do
  begin
    clusterY := mapY[yb];

    for xb := MapXLoPos to MapXHiPos do
    begin
      fillchar(verticalSum[xb - MapXLoPos],sizeof(verticalSum[xb - MapXLoPos]),0);
      for yc := 0 to high(clusterY) do
      with verticalSum[xb - MapXLoPos] do
      begin
        c := (scanlinesSrc[clusterY[yc].Pos]+xb)^;
        w := clusterY[yc].Weight;
        wa := w * c.alpha;
        sumA += wa;
        sumR += c.red * wa;
        sumG += c.green * wa;
        sumB += c.blue * wa;
      end;
    end;

    pdest := result.Scanline[yb];

    for xb := 0 to NewWidth-1 do
    begin
      clusterX := mapX[xb];
      {$hints off}
      fillchar(sum,sizeof(sum),0);
      {$hints on}
      for xc := 0 to high(clusterX) do
      begin
        w := clusterX[xc].Weight;
        with verticalSum[ClusterX[xc].Pos - MapXLoPos] do
        begin
          sum.sumA += sumA*w;
          sum.sumR += sumR*w;
          sum.sumG += sumG*w;
          sum.sumB += sumB*w;
        end;
      end;

      if sum.sumA < 0.5 then
        pdest^ := BGRAPixelTransparent else
      begin
        c.red := constrain(round(sum.sumR/sum.sumA),0,255);
        c.green := constrain(round(sum.sumG/sum.sumA),0,255);
        c.blue := constrain(round(sum.sumB/sum.sumA),0,255);
        if sum.sumA > 255 then
          c.alpha := 255 else
          c.alpha := round(sum.sumA);
        pdest^ := c;
      end;
      inc(pdest);
    end;
  end;

end;

end.

