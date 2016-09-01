unit BGRAFilters;

{$mode objfpc}{$H+}

interface

{ Here are some filters that can be applied to a bitmap. The filters
  take a source image as a parameter and gives a filtered image as
  a result. }

uses
  Classes, BGRABitmapTypes, BGRAFilterType, BGRAFilterBlur;

type
  TFilterTask = BGRAFilterType.TFilterTask;

/////////////////////// PIXELWISE FILTERS ////////////////////////////////
type
  { TGrayscaleTask }
  { Grayscale converts colored pixel into grayscale with same luminosity }
  TGrayscaleTask = class(TFilterTask)
  private
    FBounds: TRect;
  public
    constructor Create(bmp: TBGRACustomBitmap; ABounds: TRect);
  protected
    procedure DoExecute; override;
  end;

{ Grayscale converts colored pixel into grayscale with same luminosity }
function FilterGrayscale(bmp: TBGRACustomBitmap): TBGRACustomBitmap;
function FilterGrayscale(bmp: TBGRACustomBitmap; ABounds: TRect): TBGRACustomBitmap;
function CreateGrayscaleTask(bmp: TBGRACustomBitmap; ABounds: TRect): TFilterTask;

{ Normalize use the whole available range of values, making dark colors darkest possible
  and light colors lightest possible }
function FilterNormalize(bmp: TBGRACustomBitmap;
  eachChannel: boolean = True): TBGRACustomBitmap;
function FilterNormalize(bmp: TBGRACustomBitmap; ABounds: TRect;
  eachChannel: boolean = True): TBGRACustomBitmap;

////////////////////// 3X3 FILTERS ////////////////////////////////////////////

{ Sharpen filter add more contrast between pixels }
function FilterSharpen(bmp: TBGRACustomBitmap; AAmount: integer = 256): TBGRACustomBitmap;
function FilterSharpen(bmp: TBGRACustomBitmap; ABounds: TRect; AAmount: integer = 256): TBGRACustomBitmap;

{ Compute a contour, as if the image was drawn with a 2 pixels-wide black pencil }
function FilterContour(bmp: TBGRACustomBitmap): TBGRACustomBitmap;

{ Emboss filter compute a color difference in the angle direction }
function FilterEmboss(bmp: TBGRACustomBitmap; angle: single; AStrength: integer= 64; AOptions: TEmbossOptions = []): TBGRACustomBitmap;
function FilterEmboss(bmp: TBGRACustomBitmap; angle: single; ABounds: TRect; AStrength: integer= 64; AOptions: TEmbossOptions = []): TBGRACustomBitmap;

{ Emboss highlight computes a sort of emboss with 45 degrees angle and
  with standard selection color (white/black and filled with blue) }
function FilterEmbossHighlight(bmp: TBGRACustomBitmap;
  FillSelection: boolean; DefineBorderColor: TBGRAPixel): TBGRACustomBitmap;
function FilterEmbossHighlightOffset(bmp: TBGRACustomBitmap;
  FillSelection: boolean; DefineBorderColor: TBGRAPixel; var Offset: TPoint): TBGRACustomBitmap;

{ The median filter consist in calculating the median value of pixels. Here
  a square of 9x9 pixel is considered. The median allow to select the most
  representative colors. The option parameter allow to choose to smooth the
  result or not. }
function FilterMedian(bmp: TBGRACustomBitmap; Option: TMedianOption): TBGRACustomBitmap;

//////////////////////// DEFORMATION FILTERS /////////////////////////////////

{ Distort the image as if it were on a sphere }
function FilterSphere(bmp: TBGRACustomBitmap): TBGRACustomBitmap;

{ Twirl distortion, i.e. a progressive rotation }
function FilterTwirl(bmp: TBGRACustomBitmap; ACenter: TPoint; ARadius: Single; ATurn: Single=1; AExponent: Single=3): TBGRACustomBitmap;
function FilterTwirl(bmp: TBGRACustomBitmap; ABounds: TRect; ACenter: TPoint; ARadius: Single; ATurn: Single=1; AExponent: Single=3): TBGRACustomBitmap;

{ Distort the image as if it were on a vertical cylinder }
function FilterCylinder(bmp: TBGRACustomBitmap): TBGRACustomBitmap;

{ Compute a plane projection towards infinity (SLOW) }
function FilterPlane(bmp: TBGRACustomBitmap): TBGRACustomBitmap;

{ Rotate filter rotate the image and clip it in the bounding rectangle }
function FilterRotate(bmp: TBGRACustomBitmap; origin: TPointF;
  angle: single; correctBlur: boolean = false): TBGRACustomBitmap;

///////////////////////// BLUR FILTERS //////////////////////////////////////

{ A radial blur applies a blur with a circular influence, i.e, each pixel
  is merged with pixels within the specified radius. There is an exception
  with rbFast blur, the optimization entails an hyperbolic shape. }
type TRadialBlurTask = BGRAFilterBlur.TRadialBlurTask;
function FilterBlurRadial(bmp: TBGRACustomBitmap; radius: single; blurType: TRadialBlurType): TBGRACustomBitmap;
function FilterBlurRadial(bmp: TBGRACustomBitmap; radiusX: single; radiusY: single; blurType: TRadialBlurType): TBGRACustomBitmap;
function CreateRadialBlurTask(ABmp: TBGRACustomBitmap; ABounds: TRect; ARadius: single; ABlurType: TRadialBlurType): TRadialBlurTask;
function CreateRadialBlurTask(ABmp: TBGRACustomBitmap; ABounds: TRect; ARadiusX,ARadiusY: single; ABlurType: TRadialBlurType): TRadialBlurTask;

{ The precise blur allow to specify the blur radius with subpixel accuracy }
function FilterBlurRadialPrecise(bmp: TBGRACustomBitmap; radius: single): TBGRACustomBitmap; deprecated 'Use FilterBlurRadial with blurType:=rbPrecise and radius multiplied by 10';
function CreateRadialPreciseBlurTask(ABmp: TBGRACustomBitmap; ABounds: TRect; ARadius: single): TRadialBlurTask; deprecated 'Use CreateRadialBlurTask with blurType:=rbPrecise and radius multiplied by 10';

{ Motion blur merge pixels in a direction. The oriented parameter specifies
  if the weights of the pixels are the same along the line or not. }
type TMotionBlurTask = BGRAFilterBlur.TMotionBlurTask;
function FilterBlurMotion(bmp: TBGRACustomBitmap; distance: single; angle: single; oriented: boolean): TBGRACustomBitmap;
function CreateMotionBlurTask(ABmp: TBGRACustomBitmap; ABounds: TRect; ADistance,AAngle: single; AOriented: boolean): TMotionBlurTask;

{ General purpose blur filter, with a blur mask as parameter to describe
  how pixels influence each other }
function FilterBlur(bmp: TBGRACustomBitmap; AMask: TBGRACustomBitmap; AMaskIsThreadSafe: boolean = false): TBGRACustomBitmap;
function CreateBlurTask(ABmp: TBGRACustomBitmap; ABounds: TRect; AMask: TBGRACustomBitmap; AMaskIsThreadSafe: boolean = false): TFilterTask;

////////////////////////////// OTHER FILTERS /////////////////////////////////

{ SmartZoom x3 is a filter that upsizes 3 times the picture and add
  pixels that could be logically expected (horizontal, vertical, diagonal lines) }
function FilterSmartZoom3(bmp: TBGRACustomBitmap;
  Option: TMedianOption): TBGRACustomBitmap;

function FilterPixelate(bmp: TBGRACustomBitmap; pixelSize: integer; useResample: boolean; filter: TResampleFilter = rfLinear): TBGRACustomBitmap;

implementation

uses Math, BGRATransform, Types, SysUtils, BGRAFilterScanner;

/////////////////////// PIXELWISE FILTERS ////////////////////////////////

{ TGrayscaleTask }

constructor TGrayscaleTask.Create(bmp: TBGRACustomBitmap; ABounds: TRect);
begin
  SetSource(bmp);
  FBounds := ABounds;
end;

procedure TGrayscaleTask.DoExecute;
var
  yb: LongInt;
begin
  if IsRectEmpty(FBounds) then exit;
  for yb := FBounds.Top to FBounds.bottom - 1 do
  begin
    if GetShouldStop(yb) then break;
    TBGRAFilterScannerGrayscale.ComputeFilterAt(FSource.scanline[yb] + FBounds.left,
            Destination.scanline[yb] + FBounds.left, FBounds.right-FBounds.left, true);
  end;
  Destination.InvalidateBitmap;
end;

{ Filter grayscale applies BGRAToGrayscale function to all pixels }
function FilterGrayscale(bmp: TBGRACustomBitmap): TBGRACustomBitmap;
begin
  result := FilterGrayscale(bmp,rect(0,0,bmp.width,bmp.Height));
end;

function FilterGrayscale(bmp: TBGRACustomBitmap; ABounds: TRect): TBGRACustomBitmap;
var scanner: TBGRAFilterScannerGrayscale;
begin
  result := bmp.NewBitmap(bmp.Width,bmp.Height);
  scanner := TBGRAFilterScannerGrayscale.Create(bmp,Point(0,0),True);
  result.FillRect(ABounds,scanner,dmSet);
  scanner.Free;
end;

function CreateGrayscaleTask(bmp: TBGRACustomBitmap; ABounds: TRect): TFilterTask;
begin
  result := TGrayscaleTask.Create(bmp,ABounds);
end;

function FilterNormalize(bmp: TBGRACustomBitmap; eachChannel: boolean
  ): TBGRACustomBitmap;
begin
  result := FilterNormalize(bmp, rect(0,0,bmp.Width,bmp.Height), eachChannel);
end;

{ Normalize compute min-max of specified channel and apply an affine transformation
  to make it use the full range of values }
function FilterNormalize(bmp: TBGRACustomBitmap; ABounds: TRect;
  eachChannel: boolean = True): TBGRACustomBitmap;
var scanner: TBGRAFilterScannerNormalize;
begin
  Result := bmp.NewBitmap(bmp.Width, bmp.Height);
  if not IntersectRect(ABounds,ABounds,rect(0,0,bmp.Width,bmp.Height)) then exit;
  scanner := TBGRAFilterScannerNormalize.Create(bmp,Point(0,0),ABounds,eachChannel);
  result.FillRect(ABounds,scanner,dmSet);
  scanner.Free;
end;

////////////////////// 3X3 FILTERS ////////////////////////////////////////////

{ This filter compute for each pixel the mean of the eight surrounding pixels,
  then the difference between this average pixel and the pixel at the center
  of the square. Finally the difference is added to the new pixel, exagerating
  its difference with its neighbours. }
function FilterSharpen(bmp: TBGRACustomBitmap; ABounds: TRect; AAmount: integer = 256): TBGRACustomBitmap;
var scanner: TBGRAFilterScanner;
begin
  Result := bmp.NewBitmap(bmp.Width, bmp.Height);
  if IsRectEmpty(ABounds) then exit;
  scanner := TBGRASharpenScanner.Create(bmp,ABounds,AAmount);
  result.FillRect(ABounds,scanner,dmSet);
  scanner.Free;
end;

function FilterSharpen(bmp: TBGRACustomBitmap; AAmount: integer
  ): TBGRACustomBitmap;
begin
  result := FilterSharpen(bmp,rect(0,0,bmp.Width,bmp.Height),AAmount);
end;

{ Filter contour computes for each pixel
  the grayscale difference with surrounding pixels (in intensity and alpha)
  and draw black pixels when there is a difference }
function FilterContour(bmp: TBGRACustomBitmap): TBGRACustomBitmap;
var scanner: TBGRAContourScanner;
begin
  result := bmp.NewBitmap(bmp.Width, bmp.Height);
  scanner := TBGRAContourScanner.Create(bmp,rect(0,0,bmp.width,bmp.height));
  result.Fill(scanner);
  scanner.Free;
end;

function FilterEmboss(bmp: TBGRACustomBitmap; angle: single; AStrength: integer; AOptions: TEmbossOptions): TBGRACustomBitmap;
begin
  result := FilterEmboss(bmp, angle, rect(0,0,bmp.Width,bmp.Height), AStrength, AOptions);
end;

{ Emboss filter computes the difference between each pixel and the surrounding pixels
  in the specified direction. }
function FilterEmboss(bmp: TBGRACustomBitmap; angle: single; ABounds: TRect; AStrength: integer; AOptions: TEmbossOptions): TBGRACustomBitmap;
var
  yb, xb: NativeInt;
  dx, dy: single;
  idx, idy: NativeInt;
  x256,y256: NativeInt;
  cMiddle: TBGRAPixel;
  hMiddle: THSLAPixel;

  tempPixel, refPixel: TBGRAPixel;
  pdest: PBGRAPixel;

  bounds: TRect;
  psrc: PBGRAPixel;
  redDiff,greenDiff,blueDiff: NativeUInt;
  diff: NativeInt;
begin
  if IsRectEmpty(ABounds) then exit;
  //compute pixel position and weight
  dx   := cos(angle * Pi / 180);
  dy   := sin(angle * Pi / 180);
  idx := floor(dx);
  idy := floor(dy);
  x256 := trunc((dx-idx)*256);
  y256 := trunc((dy-idy)*256);

  Result := bmp.NewBitmap(bmp.Width, bmp.Height);

  bounds := bmp.GetImageBounds;

  if not IntersectRect(bounds, bounds, ABounds) then exit;
  bounds.Left   := max(0, bounds.Left - 1);
  bounds.Top    := max(0, bounds.Top - 1);
  bounds.Right  := min(bmp.Width, bounds.Right + 1);
  bounds.Bottom := min(bmp.Height, bounds.Bottom + 1);

  if not (eoTransparent in AOptions) then
  begin
    if eoPreserveHue in AOptions then
      Result.PutImagePart(ABounds.left,ABounds.top,bmp,ABounds,dmSet)
    else
      Result.FillRect(ABounds,BGRA(128, 128, 128, 255),dmSet);
  end;

  //loop through destination
  for yb := bounds.Top to bounds.bottom - 1 do
  begin
    pdest := Result.scanline[yb] + bounds.Left;
    psrc := bmp.ScanLine[yb]+bounds.Left;

    for xb := bounds.Left+idx to bounds.Right-1+idx do
    begin
      refPixel := bmp.GetPixel256(xb,yb+idy,x256,y256);
      cMiddle := psrc^;
      inc(psrc);

      if eoPreserveHue in AOptions then
      begin
        {$push}{$hints off}
        diff := ((refPixel.red * refPixel.alpha - cMiddle.red * cMiddle.alpha)+
                 (refPixel.green * refPixel.alpha - cMiddle.green * cMiddle.alpha)+
                 (refPixel.blue * refPixel.alpha - cMiddle.blue * cMiddle.alpha))* AStrength div 128;
        {$pop}
        if diff > 0 then
          hMiddle := BGRAToHSLA(refPixel)
        else
          hMiddle := BGRAToHSLA(cMiddle);
        hMiddle.lightness := min(65535,max(0,hMiddle.lightness+diff));
        if eoTransparent in AOptions then
          hMiddle.alpha := min(65535,abs(diff));
        pdest^ := HSLAToBGRA(hMiddle);
      end else
      begin
        {$push}{$hints off}
        redDiff := NativeUInt(max(0, 65536 + (refPixel.red * refPixel.alpha - cMiddle.red * cMiddle.alpha) * AStrength div 64)) shr 9;
        greenDiff := NativeUInt(max(0, 65536 + (refPixel.green * refPixel.alpha - cMiddle.green * cMiddle.alpha) * AStrength div 64)) shr 9;
        blueDiff := NativeUInt(max(0, 65536 + (refPixel.blue * refPixel.alpha - cMiddle.blue * cMiddle.alpha) * AStrength div 64)) shr 9;
        {$pop}
        if (redDiff <> 128) or (greenDiff <> 128) or (blueDiff <> 128) then
        begin
          tempPixel.red := min(255, redDiff);
          tempPixel.green := min(255, greenDiff);
          tempPixel.blue := min(255, blueDiff);
          if eoTransparent in AOptions then
          begin
            tempPixel.alpha := min(255,abs(NativeInt(redDiff-128))+abs(NativeInt(greenDiff-128))+abs(NativeInt(blueDiff-128)));
            pdest^ := tempPixel;
          end else
          begin
            tempPixel.alpha := 255;
            pdest^ := tempPixel;
          end;
        end;
      end;

      Inc(pdest);
    end;
  end;
  Result.InvalidateBitmap;
end;

{ Like general emboss, but with fixed direction and automatic color with transparency }
function FilterEmbossHighlight(bmp: TBGRACustomBitmap;
  FillSelection: boolean; DefineBorderColor: TBGRAPixel): TBGRACustomBitmap;
var
  bounds: TRect;
  borderColorOverride: boolean;
  borderColorLevel: Int32or64;
  scan: TBGRAEmbossHightlightScanner;
begin
  borderColorOverride := DefineBorderColor.alpha <> 0;
  borderColorLevel := DefineBorderColor.red;

  Result    := bmp.NewBitmap(bmp.Width, bmp.Height);

  if borderColorOverride then
    bounds := bmp.GetImageBounds(cRed, borderColorLevel)
  else
    bounds := bmp.GetImageBounds(cRed);
  if (bounds.Right <= bounds.Left) or (bounds.Bottom <= Bounds.Top) then
    exit;
  bounds.Left   := max(0, bounds.Left - 1);
  bounds.Top    := max(0, bounds.Top - 1);
  bounds.Right  := min(bmp.Width, bounds.Right + 1);
  bounds.Bottom := min(bmp.Height, bounds.Bottom + 1);

  scan := TBGRAEmbossHightlightScanner.Create(bmp, bounds, borderColorOverride);
  scan.FillSelection := FillSelection;
  if borderColorOverride then scan.SourceBorderColor := DefineBorderColor;
  Result.FillRect(bounds, scan, dmSet);
  scan.Free;
end;

function FilterEmbossHighlightOffset(bmp: TBGRACustomBitmap;
  FillSelection: boolean; DefineBorderColor: TBGRAPixel; var Offset: TPoint): TBGRACustomBitmap;
var
  bounds: TRect;
  borderColorOverride: boolean;
  borderColorLevel: int32or64;
  scan: TBGRAEmbossHightlightScanner;
begin
  borderColorOverride := DefineBorderColor.alpha <> 0;
  borderColorLevel := DefineBorderColor.red;

  if borderColorOverride then
    bounds := bmp.GetImageBounds(cRed, borderColorLevel)
  else
    bounds := bmp.GetImageBounds(cRed);
  if (bounds.Right <= bounds.Left) or (bounds.Bottom <= Bounds.Top) then
  begin
    Result    := bmp.NewBitmap(0, 0);
    exit;
  end;
  bounds.Left   := max(0, bounds.Left - 1);
  bounds.Top    := max(0, bounds.Top - 1);
  bounds.Right  := min(bmp.Width, bounds.Right + 1);
  bounds.Bottom := min(bmp.Height, bounds.Bottom + 1);

  Result    := bmp.NewBitmap(bounds.Right-Bounds.Left+1, bounds.Bottom-Bounds.Top+1);
  inc(Offset.X, bounds.Left);
  inc(Offset.Y, bounds.Top);

  scan := TBGRAEmbossHightlightScanner.Create(bmp, bounds, borderColorOverride);
  scan.FillSelection := FillSelection;
  if borderColorOverride then scan.SourceBorderColor := DefineBorderColor;
  Result.FillRect(rect(0,0,result.Width,result.Height), scan, dmSet, Offset);
  scan.Free;
end;

{ For each component, sort values to get the median }
function FilterMedian(bmp: TBGRACustomBitmap;
  Option: TMedianOption): TBGRACustomBitmap;

  function ComparePixLt(p1, p2: TBGRAPixel): boolean;
  begin
    if (p1.red + p1.green + p1.blue = p2.red + p2.green + p2.blue) then
      Result := (int32or64(p1.red) shl 8) + (int32or64(p1.green) shl 16) +
        int32or64(p1.blue) < (int32or64(p2.red) shl 8) + (int32or64(p2.green) shl 16) +
        int32or64(p2.blue)
    else
      Result := (p1.red + p1.green + p1.blue) < (p2.red + p2.green + p2.blue);
  end;

const
  nbpix = 9;
var
  yb, xb:    int32or64;
  dx, dy, n, i, j, k: int32or64;
  a_pixels:  array[0..nbpix - 1] of TBGRAPixel;
  tempPixel, refPixel: TBGRAPixel;
  tempValue: byte;
  sumR, sumG, sumB, sumA, BGRAdiv, nbA: uint32or64;
  tempAlpha: word;
  bounds:    TRect;
  pdest:     PBGRAPixel;
begin
  Result := bmp.NewBitmap(bmp.Width, bmp.Height);

  bounds := bmp.GetImageBounds;
  if (bounds.Right <= bounds.Left) or (bounds.Bottom <= Bounds.Top) then
    exit;
  bounds.Left   := max(0, bounds.Left - 1);
  bounds.Top    := max(0, bounds.Top - 1);
  bounds.Right  := min(bmp.Width, bounds.Right + 1);
  bounds.Bottom := min(bmp.Height, bounds.Bottom + 1);

  for yb := bounds.Top to bounds.bottom - 1 do
  begin
    pdest := Result.scanline[yb] + bounds.left;
    for xb := bounds.left to bounds.right - 1 do
    begin
      n := 0;
      for dy := -1 to 1 do
        for dx := -1 to 1 do
        begin
          a_pixels[n] := bmp.GetPixel(xb + dx, yb + dy);
          if a_pixels[n].alpha = 0 then
            a_pixels[n] := BGRAPixelTransparent;
          Inc(n);
        end;
      for i := 1 to n - 1 do
      begin
        j := i;
        while (j > 1) and (a_pixels[j].alpha < a_pixels[j - 1].alpha) do
        begin
          tempValue := a_pixels[j].alpha;
          a_pixels[j].alpha := a_pixels[j - 1].alpha;
          a_pixels[j - 1].alpha := tempValue;
          Dec(j);
        end;
        j := i;
        while (j > 1) and (a_pixels[j].red < a_pixels[j - 1].red) do
        begin
          tempValue := a_pixels[j].red;
          a_pixels[j].red := a_pixels[j - 1].red;
          a_pixels[j - 1].red := tempValue;
          Dec(j);
        end;
        j := i;
        while (j > 1) and (a_pixels[j].green < a_pixels[j - 1].green) do
        begin
          tempValue := a_pixels[j].green;
          a_pixels[j].green := a_pixels[j - 1].green;
          a_pixels[j - 1].green := tempValue;
          Dec(j);
        end;
        j := i;
        while (j > 1) and (a_pixels[j].blue < a_pixels[j - 1].blue) do
        begin
          tempValue := a_pixels[j].blue;
          a_pixels[j].blue := a_pixels[j - 1].blue;
          a_pixels[j - 1].blue := tempValue;
          Dec(j);
        end;
      end;

      refPixel := a_pixels[n div 2];

      if option in [moLowSmooth, moMediumSmooth, moHighSmooth] then
      begin
        sumR    := 0;
        sumG    := 0;
        sumB    := 0;
        sumA    := 0;
        BGRAdiv := 0;
        nbA     := 0;

        case option of
          moHighSmooth, moMediumSmooth:
          begin
            j := 2;
            k := 2;
          end;
          else
          begin
            j := 1;
            k := 1;
          end;
        end;

         {$hints off}
        for i := -k to j do
        begin
          tempPixel := a_pixels[n div 2 + i];
          tempAlpha := tempPixel.alpha;
          if (option = moMediumSmooth) and ((i = -k) or (i = j)) then
            tempAlpha := tempAlpha div 2;

          sumR    += tempPixel.red * tempAlpha;
          sumG    += tempPixel.green * tempAlpha;
          sumB    += tempPixel.blue * tempAlpha;
          BGRAdiv += tempAlpha;

          sumA += tempAlpha;
          Inc(nbA);
        end;
         {$hints on}
        if option = moMediumSmooth then
          Dec(nbA);

        if (BGRAdiv = 0) then
          refPixel := BGRAPixelTransparent
        else
        begin
          refPixel.red   := round(sumR / BGRAdiv);
          refPixel.green := round(sumG / BGRAdiv);
          refPixel.blue  := round(sumB / BGRAdiv);
          refPixel.alpha := round(sumA / nbA);
        end;
      end;

      pdest^ := refPixel;
      Inc(pdest);
    end;
  end;
end;

//////////////////////// DEFORMATION FILTERS /////////////////////////////////

{ Compute the distance for each pixel to the center of the bitmap,
  calculate the corresponding angle with arcsin, use this angle
  to determine a distance from the center in the source bitmap }
function FilterSphere(bmp: TBGRACustomBitmap): TBGRACustomBitmap;
var
  cx, cy: single;
  scanner: TBGRASphereDeformationScanner;
begin
  Result := bmp.NewBitmap(bmp.Width, bmp.Height);
  cx     := bmp.Width / 2 - 0.5;
  cy     := bmp.Height / 2 - 0.5;
  scanner := TBGRASphereDeformationScanner.Create(bmp,PointF(cx,cy),bmp.Width/2,bmp.Height/2);
  result.FillEllipseAntialias(cx,cy,bmp.Width/2-0.5,bmp.Height/2-0.5,scanner);
  scanner.Free;
end;

{ Applies twirl scanner. See TBGRATwirlScanner }
function FilterTwirl(bmp: TBGRACustomBitmap; ABounds: TRect; ACenter: TPoint; ARadius: Single; ATurn: Single=1; AExponent: Single=3): TBGRACustomBitmap;
var twirl: TBGRATwirlScanner;
begin
  twirl := TBGRATwirlScanner.Create(bmp,ACenter,ARadius,ATurn,AExponent);
  Result := bmp.NewBitmap(bmp.Width, bmp.Height);
  result.FillRect(ABounds, twirl, dmSet);
  twirl.free;
end;

function FilterTwirl(bmp: TBGRACustomBitmap; ACenter: TPoint;
  ARadius: Single; ATurn: Single; AExponent: Single): TBGRACustomBitmap;
begin
  result := FilterTwirl(bmp,rect(0,0,bmp.Width,bmp.Height),ACenter,ARadius,ATurn,AExponent);
end;

{ Compute the distance for each pixel to the vertical axis of the bitmap,
  calculate the corresponding angle with arcsin, use this angle
  to determine a distance from the vertical axis in the source bitmap }
function FilterCylinder(bmp: TBGRACustomBitmap): TBGRACustomBitmap;
var
  cx: single;
  scanner: TBGRAVerticalCylinderDeformationScanner;
begin
  Result := bmp.NewBitmap(bmp.Width, bmp.Height);
  cx     := bmp.Width / 2 - 0.5;
  scanner := TBGRAVerticalCylinderDeformationScanner.Create(bmp,cx,bmp.Width/2);
  result.Fill(scanner);
  scanner.Free;
end;

function FilterPlane(bmp: TBGRACustomBitmap): TBGRACustomBitmap;
const resampleGap=0.6;
var
  cy, x1, x2, y1, y2, z1, z2, h: single;
  yb: int32or64;
  resampledBmp: TBGRACustomBitmap;
  resampledBmpWidth: int32or64;
  resampledFactor,newResampleFactor: single;
  sub,resampledSub: TBGRACustomBitmap;
  partRect: TRect;
  resampleSizeY : int32or64;
begin
  resampledBmp := bmp.Resample(bmp.Width*2,bmp.Height*2,rmSimpleStretch);
  resampledBmpWidth := resampledBmp.Width;
  resampledFactor := 2;
  Result := bmp.NewBitmap(bmp.Width, bmp.Height*2);
  cy     := result.Height / 2 - 0.5;
  h      := 1;
  for yb := 0 to ((Result.Height-1) div 2) do
  begin
    y1 := (cy - (yb-0.5)) / (cy+0.5);
    y2 := (cy - (yb+0.5)) / (cy+0.5);
    if y2 <= 0 then continue;
    z1 := h/y1;
    z2 := h/y2;
    newResampleFactor := 1/(z2-z1)*1.5;

    x1 := (z1+1)/2;
    x2 := (z2+1)/2;
    if newResampleFactor <= resampledFactor*resampleGap then
    begin
      resampledFactor := newResampleFactor;
      if resampledBmp <> bmp then resampledBmp.Free;
      if (x2-x1 >= 1) then resampleSizeY := 1 else
        resampleSizeY := round(1+((x2-x1)-1)/(1/bmp.Height-1)*(bmp.Height-1));
      resampledBmp := bmp.Resample(max(1,round(bmp.Width*resampledFactor)),resampleSizeY,rmSimpleStretch);
      resampledBmpWidth := resampledBmp.Width;
    end;

    partRect := Rect(round(-resampledBmpWidth/2*z1+resampledBmpWidth/2),floor(x1*resampledBmp.Height),
       round(resampledBmpWidth/2*z1+resampledBmpWidth/2),floor(x2*resampledBmp.Height)+1);
    if x2-x1 > 1 then
    begin
      partRect.Top := 0;
      partRect.Bottom := 1;
    end;
    sub := resampledBmp.GetPart(partRect);
    if sub <> nil then
    begin
      resampledSub := sub.Resample(bmp.Width,1,rmFineResample);
      result.PutImage(0,yb,resampledSub,dmSet);
      result.PutImage(0,Result.Height-1-yb,resampledSub,dmSet);
      resampledSub.free;
      sub.free;
    end;
  end;
  if resampledBmp <> bmp then resampledBmp.Free;

  if result.Height <> bmp.Height then
  begin
    resampledBmp := result.Resample(bmp.Width,bmp.Height,rmSimpleStretch);
    result.free;
    result := resampledBmp;
  end;
end;

{ Rotates the image. To do this, loop through the destination and
  calculates the position in the source bitmap with an affine transformation }
function FilterRotate(bmp: TBGRACustomBitmap; origin: TPointF;
  angle: single; correctBlur: boolean): TBGRACustomBitmap;
begin
  Result := bmp.NewBitmap(bmp.Width, bmp.Height);
  Result.PutImageAngle(0,0,bmp,angle,origin.x,origin.y,255,true,correctBlur);
end;

///////////////////////// BLUR FILTERS //////////////////////////////////////

function FilterBlurRadial(bmp: TBGRACustomBitmap; radius: single; blurType: TRadialBlurType): TBGRACustomBitmap;
var task: TFilterTask;
begin
  task := CreateRadialBlurTask(bmp,rect(0,0,bmp.Width,bmp.Height),radius,blurTYpe);
  result := task.Execute;
  task.Free;
end;

function FilterBlurRadial(bmp: TBGRACustomBitmap; radiusX: single; radiusY: single; blurType: TRadialBlurType): TBGRACustomBitmap;
var task: TFilterTask;
begin
  task := CreateRadialBlurTask(bmp,rect(0,0,bmp.Width,bmp.Height),radiusX,radiusY,blurTYpe);
  result := task.Execute;
  task.Free;
end;

function CreateRadialBlurTask(ABmp: TBGRACustomBitmap; ABounds: TRect; ARadius: single; ABlurType: TRadialBlurType): TRadialBlurTask;
begin
  result := TRadialBlurTask.Create(ABmp,ABounds,ARadius,ABlurType);
end;

function CreateRadialBlurTask(ABmp: TBGRACustomBitmap; ABounds: TRect;
  ARadiusX,ARadiusY: single; ABlurType: TRadialBlurType): TRadialBlurTask;
begin
  result := TRadialBlurTask.Create(ABmp,ABounds,ARadiusX,ARadiusY,ABlurType);
end;

{ Precise blur }

function FilterBlurRadialPrecise(bmp: TBGRACustomBitmap; radius: single): TBGRACustomBitmap;
var task: TRadialBlurTask;
begin
  task := CreateRadialPreciseBlurTask(bmp,rect(0,0,bmp.Width,bmp.Height),radius);
  result := task.Execute;
  task.Free;
end;

function CreateRadialPreciseBlurTask(ABmp: TBGRACustomBitmap; ABounds: TRect; ARadius: single): TRadialBlurTask;
begin
  result := TRadialBlurTask.Create(ABmp,ABounds,ARadius*10,rbPrecise);
end;

function FilterBlurMotion(bmp: TBGRACustomBitmap; distance: single; angle: single; oriented: boolean): TBGRACustomBitmap;
var task: TFilterTask;
begin
  task := CreateMotionBlurTask(bmp, rect(0,0,bmp.Width,bmp.Height), distance, angle, oriented);
  result := task.Execute;
  task.Free;
end;

function CreateMotionBlurTask(ABmp: TBGRACustomBitmap; ABounds: TRect;
  ADistance, AAngle: single; AOriented: boolean): TMotionBlurTask;
begin
  result := TMotionBlurTask.Create(ABmp,ABounds,ADistance,AAngle,AOriented);
end;

function FilterBlur(bmp: TBGRACustomBitmap; AMask: TBGRACustomBitmap; AMaskIsThreadSafe: boolean = false): TBGRACustomBitmap;
var task: TFilterTask;
begin
  task := TCustomBlurTask.Create(bmp,rect(0,0,bmp.Width,bmp.Height), AMask, AMaskIsThreadSafe);
  result := task.Execute;
  task.Free;
end;

function CreateBlurTask(ABmp: TBGRACustomBitmap; ABounds: TRect;
  AMask: TBGRACustomBitmap; AMaskIsThreadSafe: boolean = false): TFilterTask;
begin
  result := TCustomBlurTask.Create(ABmp, ABounds, AMask, AMaskIsThreadSafe);
end;

///////////////////////////////////// OTHER FILTERS ///////////////////////////

function FilterSmartZoom3(bmp: TBGRACustomBitmap;
  Option: TMedianOption): TBGRACustomBitmap;
type
  TSmartDiff = record
    d, cd, sd, b, a: single;
  end;

var
  xb, yb: Int32or64;
  diag1, diag2, h1, h2, v1, v2: TSmartDiff;
  c,c1,c2:      TBGRAPixel;
  temp, median: TBGRACustomBitmap;

  function ColorDiff(c1, c2: TBGRAPixel): single;
  var
    max1, max2: Int32or64;
  begin
    if (c1.alpha = 0) and (c2.alpha = 0) then
    begin
      Result := 0;
      exit;
    end
    else
    if (c1.alpha = 0) or (c2.alpha = 0) then
    begin
      Result := 1;
      exit;
    end;
    max1 := c1.red;
    if c1.green > max1 then
      max1 := c1.green;
    if c1.blue > max1 then
      max1 := c1.blue;

    max2 := c2.red;
    if c2.green > max2 then
      max2 := c2.green;
    if c2.blue > max2 then
      max2 := c2.blue;

    if (max1 = 0) or (max2 = 0) then
    begin
      Result := 0;
      exit;
    end;
    Result := (abs(c1.red / max1 - c2.red / max2) +
      abs(c1.green / max1 - c2.green / max2) + abs(c1.blue / max1 - c2.blue / max2)) / 3;
  end;

  function RGBDiff(c1, c2: TBGRAPixel): single;
  begin
    if (c1.alpha = 0) and (c2.alpha = 0) then
    begin
      Result := 0;
      exit;
    end
    else
    if (c1.alpha = 0) or (c2.alpha = 0) then
    begin
      Result := 1;
      exit;
    end;
    Result := (abs(c1.red - c2.red) + abs(c1.green - c2.green) +
      abs(c1.blue - c2.blue)) / 3 / 255;
  end;

  function smartDiff(x1, y1, x2, y2: Int32or64): TSmartDiff;
  var
    c1, c2, c1m, c2m: TBGRAPixel;
  begin
    c1  := bmp.GetPixel(x1, y1);
    c2  := bmp.GetPixel(x2, y2);
    c1m := median.GetPixel(x1, y1);
    c2m := median.GetPixel(x2, y2);
    Result.d := RGBDiff(c1, c2);
    Result.cd := ColorDiff(c1, c2);
    Result.a := c1.alpha / 255 * c2.alpha / 255;
    Result.d := Result.d * Result.a + (1 - Result.a) *
      (1 + abs(c1.alpha - c2.alpha) / 255) / 5;
    Result.b := RGBDiff(c1, c1m) * c1.alpha / 255 * c1m.alpha / 255 +
      RGBDiff(c2, c2m) * c2.alpha / 255 * c2m.alpha / 255 +
      (abs(c1.alpha - c1m.alpha) + abs(c2.alpha - c2m.alpha)) / 255 / 4;
    Result.sd := Result.d + Result.cd * 3;
  end;

var
  diff: single;

begin
  median := FilterMedian(bmp, moNone);

  temp   := bmp.Resample(bmp.Width * 3, bmp.Height * 3, rmSimpleStretch);
  Result := FilterMedian(temp, Option);
  temp.Free;

  for yb := 0 to bmp.Height - 2 do
    for xb := 0 to bmp.Width - 2 do
    begin
      diag1 := smartDiff(xb, yb, xb + 1, yb + 1);
      diag2 := smartDiff(xb, yb + 1, xb + 1, yb);

      h1 := smartDiff(xb, yb, xb + 1, yb);
      h2 := smartDiff(xb, yb + 1, xb + 1, yb + 1);
      v1 := smartDiff(xb, yb, xb, yb + 1);
      v2 := smartDiff(xb + 1, yb, xb + 1, yb + 1);

      diff := diag1.sd - diag2.sd;
      if abs(diff) < 3 then
        diff -= (diag1.b - diag2.b) * (3 - abs(diff)) / 2;
      //which diagonal to highlight?
      if abs(diff) < 0.2 then
        diff := 0;

      if diff < 0 then
      begin
        //same color?
        if diag1.cd < 0.3 then
        begin
          c1 := bmp.GetPixel(xb, yb);
          c2 := bmp.GetPixel(xb + 1, yb + 1);
          c := MergeBGRA(c1, c2);
          //restore
          Result.SetPixel(xb * 3 + 2, yb * 3 + 2, bmp.GetPixel(xb, yb));
          Result.SetPixel(xb * 3 + 3, yb * 3 + 3, bmp.GetPixel(xb + 1, yb + 1));

          if (diag1.sd < h1.sd) and (diag1.sd < v2.sd) then
            Result.SetPixel(xb * 3 + 3, yb * 3 + 2, c);
          if (diag1.sd < h2.sd) and (diag1.sd < v1.sd) then
            Result.SetPixel(xb * 3 + 2, yb * 3 + 3, c);
        end;
      end
      else
      if diff > 0 then
      begin
        //same color?
        if diag2.cd < 0.3 then
        begin
          c1 := bmp.GetPixel(xb, yb + 1);
          c2 := bmp.GetPixel(xb + 1, yb);
          c := MergeBGRA(c1, c2);
          //restore
          Result.SetPixel(xb * 3 + 3, yb * 3 + 2, bmp.GetPixel(xb + 1, yb));
          Result.SetPixel(xb * 3 + 2, yb * 3 + 3, bmp.GetPixel(xb, yb + 1));

          if (diag2.sd < h1.sd) and (diag2.sd < v1.sd) then
            Result.SetPixel(xb * 3 + 2, yb * 3 + 2, c);
          if (diag2.sd < h2.sd) and (diag2.sd < v2.sd) then
            Result.SetPixel(xb * 3 + 3, yb * 3 + 3, c);

        end;
      end;
    end;

  median.Free;
end;

function FilterPixelate(bmp: TBGRACustomBitmap; pixelSize: integer;
  useResample: boolean; filter: TResampleFilter): TBGRACustomBitmap;
var yb,xb, xs,ys, tx,ty: Int32or64;
    psrc,pdest: PBGRAPixel;
    temp,stretched: TBGRACustomBitmap;
    oldfilter: TResampleFilter;
begin
  if pixelSize < 1 then
  begin
    result := bmp.Duplicate;
    exit;
  end;
  result := bmp.NewBitmap(bmp.Width,bmp.Height);

  tx := (bmp.Width+pixelSize-1) div pixelSize;
  ty := (bmp.Height+pixelSize-1) div pixelSize;
  if not useResample then
  begin
    temp := bmp.NewBitmap(tx,ty);

    xs := (bmp.Width mod pixelSize) div 2;
    ys := (bmp.Height mod pixelSize) div 2;

    for yb := 0 to temp.height-1 do
    begin
      pdest := temp.ScanLine[yb];
      psrc := bmp.scanline[ys]+xs;
      inc(ys,pixelSize);
      for xb := temp.width-1 downto 0 do
      begin
        pdest^ := psrc^;
        inc(pdest);
        inc(psrc,pixelSize);
      end;
    end;
    temp.InvalidateBitmap;
  end else
  begin
    oldfilter := bmp.ResampleFilter;
    bmp.ResampleFilter := filter;
    temp := bmp.Resample(tx,ty,rmFineResample);
    bmp.ResampleFilter := oldfilter;
  end;
  stretched := temp.Resample(temp.Width*pixelSize,temp.Height*pixelSize,rmSimpleStretch);
  temp.free;
  if bmp.Width mod pixelSize = 0 then
    xs := 0
  else
    xs := (-pixelSize+(bmp.Width mod pixelSize)) div 2;
  if bmp.Height mod pixelSize = 0 then
    ys := 0
  else
    ys := (-pixelSize+(bmp.Height mod pixelSize)) div 2;
  result.PutImage(xs,ys,stretched,dmSet);
  stretched.Free;
end;

end.

