unit BGRAResample;

{$mode objfpc}{$H+}

interface

{ This unit provides resampling functions, i.e. resizing of bitmaps with or
  without interpolation filters.

  SimpleStretch does a boxed resample with limited antialiasing.

  FineResample uses floating point coordinates to get an antialiased resample.
  It can use minimal interpolation (4 pixels when upsizing) for simple interpolation
  filters (linear and cosine-like) or wide kernel resample for complex interpolation.
  In this cas, it calls WideKernelResample.

  WideKernelResample can be called by custom filter kernel, derived
  from TWideKernelFilter. It is slower of course than simple interpolation. }

uses
  Types, SysUtils, BGRABitmapTypes;

{------------------------------- Simple stretch ------------------------------------}

function SimpleStretch(bmp: TBGRACustomBitmap;
  NewWidth, NewHeight: integer): TBGRACustomBitmap;
procedure StretchPutImage(bmp: TBGRACustomBitmap;
  NewWidth, NewHeight: integer; dest: TBGRACustomBitmap; OffsetX,OffsetY: Integer; ADrawMode: TDrawMode; AOpacity: byte);
procedure DownSamplePutImage(source: TBGRACustomBitmap; factorX,factorY: integer; dest: TBGRACustomBitmap; OffsetX,OffsetY: Integer; ADrawMode: TDrawMode);
function DownSample(source: TBGRACustomBitmap; factorX,factorY: integer): TBGRACustomBitmap;

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

  { TLanczosKernel }

  TLanczosKernel = class(TWideKernelFilter)
  private
    FNumberOfLobes: integer;
    FFactor: ValReal;
    procedure SetNumberOfLobes(AValue: integer);
  public
    constructor Create(ANumberOfLobes: integer);
    function Interpolation(t: single): single; override;
    function ShouldCheckRange: boolean; override;
    function KernelWidth: single; override;

    property NumberOfLobes : integer read FNumberOfLobes write SetNumberOfLobes;
  end;

function CreateInterpolator(style: TSplineStyle): TWideKernelFilter;

{-------------------------------- Fine resample ------------------------------------}

function FineResample(bmp: TBGRACustomBitmap;
  NewWidth, NewHeight: integer; ResampleFilter: TResampleFilter): TBGRACustomBitmap;

function WideKernelResample(bmp: TBGRACustomBitmap;
  NewWidth, NewHeight: integer; ResampleFilterSmaller, ResampleFilterLarger: TWideKernelFilter): TBGRACustomBitmap;

implementation

uses Math, BGRABlend;

function SimpleStretch(bmp: TBGRACustomBitmap;
  newWidth, newHeight: integer): TBGRACustomBitmap;
begin
  if (NewWidth = bmp.Width) and (NewHeight = bmp.Height) then
  begin
    Result := bmp.Duplicate;
    exit;
  end;
  Result := bmp.NewBitmap(NewWidth, NewHeight);
  StretchPutImage(bmp, newWidth,newHeight, result, 0,0, dmSet, 255);
end;

procedure StretchPutImage(bmp: TBGRACustomBitmap; NewWidth, NewHeight: integer;
  dest: TBGRACustomBitmap; OffsetX, OffsetY: Integer; ADrawMode: TDrawMode; AOpacity: byte);
type
  TTransitionState = (tsNone, tsPlain, tsLeft, tsMiddle, tsRight);
var
  x_src,y_src, y_src2, prev_y_src, prev_y_src2: NativeInt;
  inc_x_src, mod_x_src, acc_x_src, inc_y_src, mod_y_src, acc_y_src,
  acc_x_src2, acc_y_src2: NativeInt;
  x_dest, y_dest: NativeInt;

  PDest, PSrc1, PSrc2: PBGRAPixel;
  vertColors: packed array[1..2] of TBGRAPixel;
  DeltaSrcX: NativeInt;
  targetRect: TRect;
  tempData: PBGRAPixel;
  prevHorizTransition,horizTransition,prevVertTransition,vertTransition: TTransitionState;
  horizSlightlyDifferent,vertSlightlyDifferent: boolean;

  procedure LinearMix(PSrc: PBGRAPixel; DeltaSrc: integer; AccSrcQuarter: boolean;
        PDest: PBGRAPixel; slightlyDifferent: boolean; var transition: TTransitionState);
  var
    asum: NativeInt;
    a1,a2: NativeInt;
    newTransition: TTransitionState;
  begin
    if DeltaSrc=0 then
    begin
      PDest^ := PSrc^;
      transition:= tsPlain;
    end
    else
    begin
      if slightlyDifferent then
      begin
        if AccSrcQuarter then newTransition:= tsRight else
          newTransition:= tsLeft;
      end else
        newTransition:= tsMiddle;

      if (newTransition = tsMiddle) or ((newTransition = tsRight) and (transition = tsLeft)) or
        ((newTransition = tsLeft) and (transition = tsRight)) then
      begin
        transition:= tsMiddle;
        asum := psrc^.alpha + (psrc+DeltaSrc)^.alpha;
        if asum = 0 then
          pdest^ := BGRAPixelTransparent
        else if asum = 510 then
        begin
          pdest^.alpha := 255;
          pdest^.red := (psrc^.red + (psrc+DeltaSrc)^.red + 1) shr 1;
          pdest^.green := (psrc^.green + (psrc+DeltaSrc)^.green + 1) shr 1;
          pdest^.blue := (psrc^.blue + (psrc+DeltaSrc)^.blue + 1) shr 1;
        end else
        begin
          pdest^.alpha := asum shr 1;
          a1 := psrc^.alpha;
          a2 := (psrc+DeltaSrc)^.alpha;
          pdest^.red := (psrc^.red*a1 + (psrc+DeltaSrc)^.red*a2 + (asum shr 1)) div asum;
          pdest^.green := (psrc^.green*a1 + (psrc+DeltaSrc)^.green*a2 + (asum shr 1)) div asum;
          pdest^.blue := (psrc^.blue*a1 + (psrc+DeltaSrc)^.blue*a2 + (asum shr 1)) div asum;
        end;
      end else
      if newTransition = tsRight then
      begin
        transition := tsRight;
        asum := psrc^.alpha + (psrc+DeltaSrc)^.alpha*3;
        if asum = 0 then
          pdest^ := BGRAPixelTransparent
        else if asum = 1020 then
        begin
          pdest^.alpha := 255;
          pdest^.red := (psrc^.red + (psrc+DeltaSrc)^.red*3 + 2) shr 2;
          pdest^.green := (psrc^.green + (psrc+DeltaSrc)^.green*3 + 2) shr 2;
          pdest^.blue := (psrc^.blue + (psrc+DeltaSrc)^.blue*3 + 2) shr 2;
        end else
        begin
          pdest^.alpha := asum shr 2;
          a1 := psrc^.alpha;
          a2 := (psrc+DeltaSrc)^.alpha;
          pdest^.red := (psrc^.red*a1 + (psrc+DeltaSrc)^.red*a2*3 + (asum shr 1)) div asum;
          pdest^.green := (psrc^.green*a1 + (psrc+DeltaSrc)^.green*a2*3 + (asum shr 1)) div asum;
          pdest^.blue := (psrc^.blue*a1 + (psrc+DeltaSrc)^.blue*a2*3 + (asum shr 1)) div asum;
        end;
      end else
      begin
        transition:= tsLeft;
        asum := psrc^.alpha*3 + (psrc+DeltaSrc)^.alpha;
        if asum = 0 then
          pdest^ := BGRAPixelTransparent
        else if asum = 1020 then
        begin
          pdest^.alpha := 255;
          pdest^.red := (psrc^.red*3 + (psrc+DeltaSrc)^.red + 2) shr 2;
          pdest^.green := (psrc^.green*3 + (psrc+DeltaSrc)^.green + 2) shr 2;
          pdest^.blue := (psrc^.blue*3 + (psrc+DeltaSrc)^.blue + 2) shr 2;
        end else
        begin
          pdest^.alpha := asum shr 2;
          a1 := psrc^.alpha;
          a2 := (psrc+DeltaSrc)^.alpha;
          pdest^.red := (psrc^.red*a1*3 + (psrc+DeltaSrc)^.red*a2 + (asum shr 1)) div asum;
          pdest^.green := (psrc^.green*a1*3 + (psrc+DeltaSrc)^.green*a2 + (asum shr 1)) div asum;
          pdest^.blue := (psrc^.blue*a1*3 + (psrc+DeltaSrc)^.blue*a2 + (asum shr 1)) div asum;
        end;
      end;
    end;
  end;

begin
  if (newWidth <= 0) or (newHeight <= 0) or (bmp.Width <= 0)
    or (bmp.Height <= 0) then
    exit;

  targetRect := rect(0,0,NewWidth,NewHeight);
  if OffsetX < dest.ClipRect.Left then targetRect.Left:= dest.ClipRect.Left-OffsetX;
  if OffsetY < dest.ClipRect.Top then targetRect.Top:= dest.ClipRect.Top-OffsetY;
  if OffsetX+NewWidth > dest.ClipRect.Right then targetRect.Right := dest.ClipRect.Right-OffsetX;
  if OffsetY+NewHeight > dest.ClipRect.Bottom then targetRect.Bottom := dest.ClipRect.Bottom-OffsetY;
  if (targetRect.Right <= targetRect.Left) or (targetRect.Bottom <= targetRect.Top) then exit;

  bmp.LoadFromBitmapIfNeeded;

  if (ADrawMode <> dmSet) or (AOpacity <> 255) then
     getmem(tempData, (targetRect.Right-targetRect.Left)*sizeof(TBGRAPixel) )
  else
      tempData := nil;

  inc_x_src := bmp.Width div newwidth;
  mod_x_src := bmp.Width mod newwidth;
  inc_y_src := bmp.Height div newheight;
  mod_y_src := bmp.Height mod newheight;

  prev_y_src := -1;
  prev_y_src2 := -1;

  acc_y_src := targetRect.Top*mod_y_src;
  y_src     := targetRect.Top*inc_y_src + (acc_y_src div NewHeight);
  acc_y_src := acc_y_src mod NewHeight;

  y_src     := y_src+ (bmp.Height div 4) div newheight;
  acc_y_src := acc_y_src+ (bmp.Height div 4) mod newheight;

  y_src2     := y_src+ (bmp.Height div 2) div newheight;
  acc_y_src2 := acc_y_src+ (bmp.Height div 2) mod newheight;
  if acc_y_src2 > NewHeight then
  begin
    dec(acc_y_src2, NewHeight);
    inc(y_src2);
  end;
  horizSlightlyDifferent := (NewWidth > bmp.Width*2 div 3) and (NewWidth < bmp.Width*4 div 3);
  prevVertTransition:= tsNone;
  vertSlightlyDifferent := (NewHeight > bmp.Height*2 div 3) and (NewHeight < bmp.Height*4 div 3);
  for y_dest := targetRect.Top to targetRect.Bottom - 1 do
  begin
    if (y_src = prev_y_src) and (y_src2 = prev_y_src2) and not vertSlightlyDifferent then
    begin
      if tempData = nil then
        move((dest.ScanLine[y_dest-1+OffsetY]+OffsetX+targetRect.Left)^,(dest.ScanLine[y_dest+OffsetY]+OffsetX+targetRect.Left)^,(targetRect.Right-targetRect.Left)*sizeof(TBGRAPixel))
      else
        PutPixels(dest.ScanLine[y_dest+OffsetY]+OffsetX+targetRect.Left,tempData,targetRect.right-targetRect.left,ADrawMode,AOpacity);
    end else
    begin
      if tempData = nil then
         PDest := dest.ScanLine[y_dest+OffsetY]+OffsetX+targetRect.Left
      else
        PDest := tempData;
      PSrc1 := bmp.Scanline[y_src];

      acc_x_src := targetRect.Left*mod_x_src;
      x_src     := targetRect.Left*inc_x_src + (acc_x_src div NewWidth);
      acc_x_src := acc_x_src mod NewWidth;

      x_src     := x_src+ (bmp.Width div 4) div NewWidth;
      acc_x_src := acc_x_src+ (bmp.Width div 4) mod NewWidth;

      DeltaSrcX := (bmp.Width div 2) div NewWidth;
      acc_x_src2 := acc_x_src+ (bmp.Width div 2) mod NewWidth;
      if acc_x_src2 > NewWidth then
      begin
        dec(acc_x_src2, NewWidth);
        inc(DeltaSrcX);
      end;
      inc(Psrc1, x_src);
      prevHorizTransition := tsNone;

      if y_src2=y_src then
      begin
        horizTransition:= prevHorizTransition;
        for x_dest := targetRect.left to targetRect.right - 1 do
        begin
          LinearMix(psrc1, DeltaSrcX, acc_x_src2 >= NewWidth shr 2, PDest, horizSlightlyDifferent, horizTransition);

          Inc(PSrc1, inc_x_src);
          Inc(acc_x_src, mod_x_src);
          if acc_x_src >= newWidth then
          begin
            Dec(acc_x_src, newWidth);
            Inc(PSrc1);
            dec(DeltaSrcX);
          end;
          Inc(acc_x_src2, mod_x_src);
          if acc_x_src2 >= newWidth then
          begin
            Dec(acc_x_src2, newWidth);
            Inc(DeltaSrcX);
          end;
          inc(PDest);
        end;
        prevVertTransition:= tsPlain;
      end else
      begin
        PSrc2 := bmp.Scanline[y_src2]+x_src;
        for x_dest := targetRect.left to targetRect.right - 1 do
        begin
          horizTransition:= prevHorizTransition;
          LinearMix(psrc1, DeltaSrcX, acc_x_src2 >= NewWidth shr 2, @vertColors[1], horizSlightlyDifferent, horizTransition);
          horizTransition:= prevHorizTransition;
          LinearMix(psrc2, DeltaSrcX, acc_x_src2 >= NewWidth shr 2, @vertColors[2], horizSlightlyDifferent, horizTransition);
          prevHorizTransition:= horizTransition;
          vertTransition:= prevVertTransition;
          LinearMix(@vertColors[1],1,acc_y_src2 >= NewHeight shr 2,PDest,vertSlightlyDifferent,vertTransition);

          Inc(PSrc1, inc_x_src);
          Inc(PSrc2, inc_x_src);
          Inc(acc_x_src, mod_x_src);
          if acc_x_src >= newWidth then
          begin
            Dec(acc_x_src, newWidth);
            Inc(PSrc1);
            Inc(PSrc2);
            dec(DeltaSrcX);
          end;
          Inc(acc_x_src2, mod_x_src);
          if acc_x_src2 >= newWidth then
          begin
            Dec(acc_x_src2, newWidth);
            Inc(DeltaSrcX);
          end;
          inc(PDest);
        end;
        prevVertTransition:= vertTransition;
      end;

      if tempData <> nil then
         PutPixels(dest.ScanLine[y_dest+OffsetY]+OffsetX+targetRect.Left,tempData,targetRect.right-targetRect.left,ADrawMode,AOpacity);
    end;

    prev_y_src := y_src;
    prev_y_src2 := y_src2;

    Inc(y_src, inc_y_src);
    Inc(acc_y_src, mod_y_src);
    if acc_y_src >= newheight then
    begin
      Dec(acc_y_src, newheight);
      Inc(y_src);
    end;

    Inc(y_src2, inc_y_src);
    Inc(acc_y_src2, mod_y_src);
    if acc_y_src2 >= newheight then
    begin
      Dec(acc_y_src2, newheight);
      Inc(y_src2);
    end;
  end;
  dest.InvalidateBitmap;
  if Assigned(tempData) then FreeMem(tempData);
end;

procedure DownSamplePutImage2(source: TBGRACustomBitmap;
  dest: TBGRACustomBitmap; OffsetX, OffsetY: Integer; ADrawMode: TDrawMode);
const factorX = 2; factorY = 2; nbi= factorX*factorY;
var xb,yb,ys: NativeInt;
    pdest: PBGRAPixel;
    psrc1,psrc2: PBGRAPixel;
    asum,maxsum: NativeUInt;
    newWidth,newHeight: NativeInt;
    r,g,b: NativeUInt;
begin
  if (source.Width mod factorX <> 0) or (source.Height mod factorY <> 0) then
     raise exception.Create('Source size must be a multiple of factorX and factorY');
  newWidth := source.Width div factorX;
  newHeight := source.Height div factorY;
  ys := 0;
  maxsum := 255*NativeInt(factorX)*NativeInt(factorY);
  for yb := 0 to newHeight-1 do
  begin
    pdest := dest.ScanLine[yb+OffsetY]+OffsetX;
    psrc1 := source.Scanline[ys]; inc(ys);
    psrc2 := source.Scanline[ys]; inc(ys);
    for xb := newWidth-1 downto 0 do
    begin
      asum := 0;
      asum := psrc1^.alpha + psrc2^.alpha + (psrc1+1)^.alpha + (psrc2+1)^.alpha;
      if asum = maxsum then
      begin
        pdest^.alpha := 255;
        r := psrc1^.red + psrc2^.red + (psrc1+1)^.red + (psrc2+1)^.red;
        g := psrc1^.green + psrc2^.green + (psrc1+1)^.green + (psrc2+1)^.green;
        b := psrc1^.blue + psrc2^.blue + (psrc1+1)^.blue + (psrc2+1)^.blue;
        inc(psrc1,factorX); inc(psrc2,factorX);
        pdest^.red := (r + (nbi shr 1)) shr 2;
        pdest^.green := (g + (nbi shr 1)) shr 2;
        pdest^.blue := (b + (nbi shr 1)) shr 2;
      end else
      if ADrawMode <> dmSetExceptTransparent then
      begin
        if asum = 0 then
        begin
          if ADrawMode = dmSet then
            pdest^ := BGRAPixelTransparent;
          inc(psrc1,factorX); inc(psrc2,factorX);
        end
        else
        begin
          r := psrc1^.red*psrc1^.alpha + psrc2^.red*psrc2^.alpha + (psrc1+1)^.red*(psrc1+1)^.alpha + (psrc2+1)^.red*(psrc2+1)^.alpha;
          g := psrc1^.green*psrc1^.alpha + psrc2^.green*psrc2^.alpha + (psrc1+1)^.green*(psrc1+1)^.alpha + (psrc2+1)^.green*(psrc2+1)^.alpha;
          b := psrc1^.blue*psrc1^.alpha + psrc2^.blue*psrc2^.alpha + (psrc1+1)^.blue*(psrc1+1)^.alpha + (psrc2+1)^.blue*(psrc2+1)^.alpha;
          inc(psrc1,factorX); inc(psrc2,factorX);
          if ADrawMode = dmSet then
          begin
            pdest^.alpha := (asum + (nbi shr 1)) shr 2;
            pdest^.red := (r + (asum shr 1)) div asum;
            pdest^.green := (g + (asum shr 1)) div asum;
            pdest^.blue := (b + (asum shr 1)) div asum;
          end
          else
          begin
            if ADrawMode = dmDrawWithTransparency then
              DrawPixelInlineWithAlphaCheck(pdest,BGRA((r + (asum shr 1)) div asum,
                 (g + (asum shr 1)) div asum,
                 (b + (asum shr 1)) div asum,
                 (asum + (nbi shr 1)) shr 2)) else
             if ADrawMode = dmFastBlend then
               FastBlendPixelInline(pdest,BGRA((r + (asum shr 1)) div asum,
                  (g + (asum shr 1)) div asum,
                  (b + (asum shr 1)) div asum,
                  (asum + (nbi shr 1)) shr 2));
          end;
        end;
      end;
      inc(pdest);
    end;
  end;
end;

procedure DownSamplePutImage3(source: TBGRACustomBitmap;
  dest: TBGRACustomBitmap; OffsetX, OffsetY: Integer; ADrawMode: TDrawMode);
const factorX = 3; factorY = 3; nbi= factorX*factorY;
var xb,yb,ys: NativeInt;
    pdest: PBGRAPixel;
    psrc1,psrc2,psrc3: PBGRAPixel;
    asum,maxsum: NativeUInt;
    newWidth,newHeight: NativeInt;
    r,g,b: NativeUInt;
begin
  if (source.Width mod factorX <> 0) or (source.Height mod factorY <> 0) then
     raise exception.Create('Source size must be a multiple of factorX and factorY');
  newWidth := source.Width div factorX;
  newHeight := source.Height div factorY;
  ys := 0;
  maxsum := 255*NativeInt(factorX)*NativeInt(factorY);
  for yb := 0 to newHeight-1 do
  begin
    pdest := dest.ScanLine[yb+OffsetY]+OffsetX;
    psrc1 := source.Scanline[ys]; inc(ys);
    psrc2 := source.Scanline[ys]; inc(ys);
    psrc3 := source.Scanline[ys]; inc(ys);
    for xb := newWidth-1 downto 0 do
    begin
      asum := 0;
      asum := psrc1^.alpha + psrc2^.alpha + psrc3^.alpha
           + (psrc1+1)^.alpha + (psrc2+1)^.alpha + (psrc3+1)^.alpha
           + (psrc1+2)^.alpha + (psrc2+2)^.alpha + (psrc3+2)^.alpha;
      if asum = maxsum then
      begin
        pdest^.alpha := 255;
        r := psrc1^.red + psrc2^.red + psrc3^.red
           + (psrc1+1)^.red + (psrc2+1)^.red + (psrc3+1)^.red
           + (psrc1+2)^.red + (psrc2+2)^.red + (psrc3+2)^.red;
        g := psrc1^.green + psrc2^.green + psrc3^.green
           + (psrc1+1)^.green + (psrc2+1)^.green + (psrc3+1)^.green
           + (psrc1+2)^.green + (psrc2+2)^.green + (psrc3+2)^.green;
        b := psrc1^.blue + psrc2^.blue + psrc3^.blue
           + (psrc1+1)^.blue + (psrc2+1)^.blue + (psrc3+1)^.blue
           + (psrc1+2)^.blue + (psrc2+2)^.blue + (psrc3+2)^.blue;
        inc(psrc1,factorX); inc(psrc2,factorX); inc(psrc3,factorX);
        pdest^.red := (r + (nbi shr 1)) div 9;
        pdest^.green := (g + (nbi shr 1)) div 9;
        pdest^.blue := (b + (nbi shr 1)) div 9;
      end else
      if ADrawMode <> dmSetExceptTransparent then
      begin
        if asum = 0 then
        begin
          if ADrawMode = dmSet then
            pdest^ := BGRAPixelTransparent;
          inc(psrc1,factorX); inc(psrc2,factorX); inc(psrc3,factorX);
        end
        else
        begin
          r := psrc1^.red*psrc1^.alpha + psrc2^.red*psrc2^.alpha + psrc3^.red*psrc3^.alpha
            + (psrc1+1)^.red*(psrc1+1)^.alpha + (psrc2+1)^.red*(psrc2+1)^.alpha + (psrc3+1)^.red*(psrc3+1)^.alpha
            + (psrc1+2)^.red*(psrc1+2)^.alpha + (psrc2+2)^.red*(psrc2+2)^.alpha + (psrc3+2)^.red*(psrc3+2)^.alpha;
          g := psrc1^.green*psrc1^.alpha + psrc2^.green*psrc2^.alpha + psrc3^.green*psrc3^.alpha
            + (psrc1+1)^.green*(psrc1+1)^.alpha + (psrc2+1)^.green*(psrc2+1)^.alpha + (psrc3+1)^.green*(psrc3+1)^.alpha
            + (psrc1+2)^.green*(psrc1+2)^.alpha + (psrc2+2)^.green*(psrc2+2)^.alpha + (psrc3+2)^.green*(psrc3+2)^.alpha;
          b := psrc1^.blue*psrc1^.alpha + psrc2^.blue*psrc2^.alpha + psrc3^.blue*psrc3^.alpha
            + (psrc1+1)^.blue*(psrc1+1)^.alpha + (psrc2+1)^.blue*(psrc2+1)^.alpha + (psrc3+1)^.blue*(psrc3+1)^.alpha
            + (psrc1+2)^.blue*(psrc1+2)^.alpha + (psrc2+2)^.blue*(psrc2+2)^.alpha + (psrc3+2)^.blue*(psrc3+2)^.alpha;
          inc(psrc1,factorX); inc(psrc2,factorX); inc(psrc3,factorX);
          if ADrawMode = dmSet then
          begin
            pdest^.alpha := (asum + (nbi shr 1)) div 9;
            pdest^.red := (r + (asum shr 1)) div asum;
            pdest^.green := (g + (asum shr 1)) div asum;
            pdest^.blue := (b + (asum shr 1)) div asum;
          end
          else
          begin
            if ADrawMode = dmDrawWithTransparency then
              DrawPixelInlineWithAlphaCheck(pdest,BGRA((r + (asum shr 1)) div asum,
                 (g + (asum shr 1)) div asum,
                 (b + (asum shr 1)) div asum,
                 (asum + (nbi shr 1)) div 9)) else
             if ADrawMode = dmFastBlend then
               FastBlendPixelInline(pdest,BGRA((r + (asum shr 1)) div asum,
                  (g + (asum shr 1)) div asum,
                  (b + (asum shr 1)) div asum,
                  (asum + (nbi shr 1)) div 9));
          end;
        end;
      end;
      inc(pdest);
    end;
  end;
end;

procedure DownSamplePutImage(source: TBGRACustomBitmap; factorX, factorY: integer;
  dest: TBGRACustomBitmap; OffsetX, OffsetY: Integer; ADrawMode: TDrawMode);
var xb,yb,ys,iy,ix: NativeInt;
    pdest,psrci: PBGRAPixel;
    psrc: array of PBGRAPixel;
    asum,maxsum: NativeUInt;
    newWidth,newHeight: NativeInt;
    r,g,b,nbi: NativeUInt;
begin
  if ADrawMode = dmXor then raise exception.Create('dmXor drawmode not supported');
  if (factorX = 2) and (factorY = 2) then
  begin
     DownSamplePutImage2(source,dest,OffsetX,OffsetY,ADrawMode);
     exit;
  end;
  if (factorX = 3) and (factorY = 3) then
  begin
     DownSamplePutImage3(source,dest,OffsetX,OffsetY,ADrawMode);
     exit;
  end;
  if (source.Width mod factorX <> 0) or (source.Height mod factorY <> 0) then
     raise exception.Create('Source size must be a multiple of factorX and factorY');
  newWidth := source.Width div factorX;
  newHeight := source.Height div factorY;
  ys := 0;
  maxsum := 255*NativeInt(factorX)*NativeInt(factorY);
  nbi := factorX*factorY;
  setlength(psrc, factorY);
  for yb := 0 to newHeight-1 do
  begin
    pdest := dest.ScanLine[yb+OffsetY]+OffsetX;
    for iy := factorY-1 downto 0 do
    begin
      psrc[iy] := source.Scanline[ys];
      inc(ys);
    end;
    for xb := newWidth-1 downto 0 do
    begin
      asum := 0;
      for iy := factorY-1 downto 0 do
      begin
        psrci := psrc[iy];
        for ix := factorX-1 downto 0 do
          asum += (psrci+ix)^.alpha;
      end;
      if asum = maxsum then
      begin
        pdest^.alpha := 255;
        r := 0;
        g := 0;
        b := 0;
        for iy := factorY-1 downto 0 do
          for ix := factorX-1 downto 0 do
          begin
            with psrc[iy]^ do
            begin
              r += red;
              g += green;
              b += blue;
            end;
            inc(psrc[iy]);
          end;
        pdest^.red := (r + (nbi shr 1)) div nbi;
        pdest^.green := (g + (nbi shr 1)) div nbi;
        pdest^.blue := (b + (nbi shr 1)) div nbi;
      end else
      if ADrawMode <> dmSetExceptTransparent then
      begin
        if asum = 0 then
        begin
          if ADrawMode = dmSet then
            pdest^ := BGRAPixelTransparent;
          for iy := factorY-1 downto 0 do
            inc(psrc[iy],factorX);
        end
        else
        begin
          r := 0;
          g := 0;
          b := 0;
          for iy := factorY-1 downto 0 do
            for ix := factorX-1 downto 0 do
            begin
              with psrc[iy]^ do
              begin
                r += red*alpha;
                g += green*alpha;
                b += blue*alpha;
              end;
              inc(psrc[iy]);
            end;
          if ADrawMode = dmSet then
          begin
            pdest^.alpha := (asum + (nbi shr 1)) div nbi;
            pdest^.red := (r + (asum shr 1)) div asum;
            pdest^.green := (g + (asum shr 1)) div asum;
            pdest^.blue := (b + (asum shr 1)) div asum;
          end
          else
          begin
            if ADrawMode = dmDrawWithTransparency then
              DrawPixelInlineWithAlphaCheck(pdest,BGRA((r + (asum shr 1)) div asum,
                 (g + (asum shr 1)) div asum,
                 (b + (asum shr 1)) div asum,
                 (asum + (nbi shr 1)) div nbi)) else
             if ADrawMode = dmFastBlend then
               FastBlendPixelInline(pdest,BGRA((r + (asum shr 1)) div asum,
                  (g + (asum shr 1)) div asum,
                  (b + (asum shr 1)) div asum,
                  (asum + (nbi shr 1)) div nbi));
          end;
        end;
      end;
      inc(pdest);
    end;
  end;
end;

function DownSample(source: TBGRACustomBitmap; factorX, factorY: integer): TBGRACustomBitmap;
begin
  if (source.Width mod factorX <> 0) or (source.Height mod factorY <> 0) then
     raise exception.Create('Source size must be a multiple of factorX and factorY');
  result := source.NewBitmap(source.Width div factorX, source.Height div factorY);
  DownSamplePutImage(source,factorX,factorY,result,0,0,dmSet);
end;

{---------------------------- Interpolation filters ----------------------------------------}

function FineInterpolation(t: single; ResampleFilter: TResampleFilter): single;
begin
  if ResampleFilter <= rfLinear then
  begin
    if ResampleFilter = rfBox then
    begin
       result := round(t);
    end else
      result := t;
  end else
  begin
    if t <= 0.5 then
      result := t*t*2 else
      result := 1-(1-t)*(1-t)*2;
    if ResampleFilter <> rfCosine then result := (result+t)*0.5;
  end;
end;

function FineInterpolation256(t256: integer; ResampleFilter: TResampleFilter): integer;
begin
  if ResampleFilter <= rfLinear then
  begin
    if ResampleFilter = rfBox then
    begin
      if t256 < 128 then
        result := 0
      else
        result := 256;
    end
    else
      result := t256;
  end else
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

{ TLanczosKernel }
{ by stab }
procedure TLanczosKernel.SetNumberOfLobes(AValue: integer);
begin
  if AValue < 1 then AValue := 1;
  if FNumberOfLobes=AValue then Exit;
  FNumberOfLobes:=AValue;
  if AValue = 1 then FFactor := 1.5 else FFactor := AValue;
end;

constructor TLanczosKernel.Create(ANumberOfLobes: integer);
begin
  NumberOfLobes:= ANumberOfLobes;
end;

function TLanczosKernel.Interpolation(t: single): single;
var Pi_t: ValReal;
begin
  if t = 0 then
    Result := 1
  else if t < FNumberOfLobes then
  begin
    Pi_t := pi * t;
    Result := FFactor * sin(Pi_t) * sin(Pi_t / FNumberOfLobes) /
      (Pi_t * Pi_t)
  end
  else
    Result := 0;
end;

function TLanczosKernel.ShouldCheckRange: boolean;
begin
  Result := True;
end;

function TLanczosKernel.KernelWidth: single;
begin
  Result := FNumberOfLobes;
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
  if (NewWidth = bmp.Width) and (NewHeight = bmp.Height) then
  begin
    Result := bmp.Duplicate;
    exit;
  end;
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
    rfLanczos2,rfLanczos3,rfLanczos4:
    begin
      tempFilter1 := TLanczosKernel.Create(ord(ResampleFilter)-ord(rfLanczos2)+2);
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

