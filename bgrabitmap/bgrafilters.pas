unit BGRAFilters;

{$mode objfpc}{$H+}

interface

{ Here are some filters that can be applied to a bitmap. The filters
  take a source image as a parameter and gives a filtered image as
  a result. }

uses
  Classes, BGRABitmapTypes;

type
  TCheckShouldStopFunc = function(ACurrentY: integer) : boolean of object;

  { TFilterTask }

  TFilterTask = class
  private
    FCheckShouldStop: TCheckShouldStopFunc;
    procedure SetDestination(AValue: TBGRACustomBitmap);
  protected
    FDestination: TBGRACustomBitmap;
    FSource: TBGRACustomBitmap;
    FCurrentY: integer;
    function GetShouldStop(ACurrentY: integer): boolean;
    procedure DoExecute; virtual; abstract;
  public
    function Execute: TBGRACustomBitmap;
    property Destination: TBGRACustomBitmap read FDestination write SetDestination;
    property CheckShouldStop: TCheckShouldStopFunc read FCheckShouldStop write FCheckShouldStop;
    property CurrentY: integer read FCurrentY;
  end;

{ The median filter consist in calculating the median value of pixels. Here
  a square of 9x9 pixel is considered. The median allow to select the most
  representative colors. The option parameter allow to choose to smooth the
  result or not. }
function FilterMedian(bmp: TBGRACustomBitmap;
  Option: TMedianOption): TBGRACustomBitmap;

{ SmartZoom x3 is a filter that upsizes 3 times the picture and add
  pixels that could be logically expected (horizontal, vertical, diagonal lines) }
function FilterSmartZoom3(bmp: TBGRACustomBitmap;
  Option: TMedianOption): TBGRACustomBitmap;

{ Sharpen filter add more contrast between pixels }
function FilterSharpen(bmp: TBGRACustomBitmap; AAmount: integer = 256): TBGRACustomBitmap;
function FilterSharpen(bmp: TBGRACustomBitmap; ABounds: TRect; AAmount: integer = 256): TBGRACustomBitmap;

{ A radial blur applies a blur with a circular influence, i.e, each pixel
  is merged with pixels within the specified radius. There is an exception
  with rbFast blur, the optimization entails an hyperbolic shape. }
function FilterBlurRadial(bmp: TBGRACustomBitmap; radius: integer;
  blurType: TRadialBlurType): TBGRACustomBitmap;
function CreateRadialBlurTask(ABmp: TBGRACustomBitmap; ABounds: TRect; ARadius: integer;
  ABlurType: TRadialBlurType): TFilterTask;

{ The precise blur allow to specify the blur radius with subpixel accuracy }
function FilterBlurRadialPrecise(bmp: TBGRACustomBitmap; radius: single): TBGRACustomBitmap;
function CreateRadialPreciseBlurTask(ABmp: TBGRACustomBitmap; ABounds: TRect; ARadius: single): TFilterTask;

{ Motion blur merge pixels in a direction. The oriented parameter specifies
  if the weights of the pixels are the same along the line or not. }
function FilterBlurMotion(bmp: TBGRACustomBitmap; distance: single;
  angle: single; oriented: boolean): TBGRACustomBitmap;
function CreateMotionBlurTask(ABmp: TBGRACustomBitmap; ABounds: TRect; ADistance,AAngle: single; AOriented: boolean): TFilterTask;

{ General purpose blur filter, with a blur mask as parameter to describe
  how pixels influence each other }
function FilterBlur(bmp: TBGRACustomBitmap; blurMask: TBGRACustomBitmap): TBGRACustomBitmap;
function CreateBlurTask(ABmp: TBGRACustomBitmap; ABounds: TRect; AMask: TBGRACustomBitmap; AMaskIsThreadSafe: boolean = false): TFilterTask;

function FilterPixelate(bmp: TBGRACustomBitmap; pixelSize: integer; useResample: boolean; filter: TResampleFilter = rfLinear): TBGRACustomBitmap;

{ Emboss filter compute a color difference in the angle direction }
function FilterEmboss(bmp: TBGRACustomBitmap; angle: single): TBGRACustomBitmap;
function FilterEmboss(bmp: TBGRACustomBitmap; angle: single; ABounds: TRect): TBGRACustomBitmap;

{ Emboss highlight computes a sort of emboss with 45 degrees angle and
  with standard selection color (white/black and filled with blue) }
function FilterEmbossHighlight(bmp: TBGRACustomBitmap;
  FillSelection: boolean; DefineBorderColor: TBGRAPixel): TBGRACustomBitmap;
function FilterEmbossHighlightOffset(bmp: TBGRACustomBitmap;
  FillSelection: boolean; DefineBorderColor: TBGRAPixel; var Offset: TPoint): TBGRACustomBitmap;

{ Normalize use the whole available range of values, making dark colors darkest possible
  and light colors lightest possible }
function FilterNormalize(bmp: TBGRACustomBitmap;
  eachChannel: boolean = True): TBGRACustomBitmap;
function FilterNormalize(bmp: TBGRACustomBitmap; ABounds: TRect;
  eachChannel: boolean = True): TBGRACustomBitmap;

{ Rotate filter rotate the image and clip it in the bounding rectangle }
function FilterRotate(bmp: TBGRACustomBitmap; origin: TPointF;
  angle: single; correctBlur: boolean = false): TBGRACustomBitmap;

{ Grayscale converts colored pixel into grayscale with same luminosity }
function FilterGrayscale(bmp: TBGRACustomBitmap): TBGRACustomBitmap;
function FilterGrayscale(bmp: TBGRACustomBitmap; ABounds: TRect): TBGRACustomBitmap;
function CreateGrayscaleTask(bmp: TBGRACustomBitmap; ABounds: TRect): TFilterTask;

{ Compute a contour, as if the image was drawn with a 2 pixels-wide black pencil }
function FilterContour(bmp: TBGRACustomBitmap): TBGRACustomBitmap;

{ Distort the image as if it were on a sphere }
function FilterSphere(bmp: TBGRACustomBitmap): TBGRACustomBitmap;

{ Twirl distortion, i.e. a progressive rotation }
function FilterTwirl(bmp: TBGRACustomBitmap; ACenter: TPoint; ARadius: Single; ATurn: Single=1; AExponent: Single=3): TBGRACustomBitmap;
function FilterTwirl(bmp: TBGRACustomBitmap; ABounds: TRect; ACenter: TPoint; ARadius: Single; ATurn: Single=1; AExponent: Single=3): TBGRACustomBitmap;

{ Distort the image as if it were on a vertical cylinder }
function FilterCylinder(bmp: TBGRACustomBitmap): TBGRACustomBitmap;

{ Compute a plane projection towards infinity (SLOW) }
function FilterPlane(bmp: TBGRACustomBitmap): TBGRACustomBitmap;

implementation

uses Math, GraphType, Dialogs, BGRATransform, Types, SysUtils;

type
  { TGrayscaleTask }

  TGrayscaleTask = class(TFilterTask)
  private
    FBounds: TRect;
  public
    constructor Create(bmp: TBGRACustomBitmap; ABounds: TRect);
  protected
    procedure DoExecute; override;
  end;

  { TRadialBlurTask }

  TRadialBlurTask = class(TFilterTask)
  private
    FBounds: TRect;
    FRadius: integer;
    FBlurType: TRadialBlurType;
  public
    constructor Create(bmp: TBGRACustomBitmap; ABounds: TRect; radius: integer;
                       blurType: TRadialBlurType);
  protected
    procedure DoExecute; override;
  end;

  { TCustomBlurTask }

  TCustomBlurTask = class(TFilterTask)
  private
    FBounds: TRect;
    FMask: TBGRACustomBitmap;
    FMaskOwned: boolean;
  public
    constructor Create(bmp: TBGRACustomBitmap; ABounds: TRect; AMask: TBGRACustomBitmap; AMaskIsThreadSafe: boolean = false);
    destructor Destroy; override;
  protected
    procedure DoExecute; override;
  end;

  { TRadialPreciseBlurTask }

  TRadialPreciseBlurTask = class(TFilterTask)
  private
    FBounds: TRect;
    FRadius: Single;
  public
    constructor Create(bmp: TBGRACustomBitmap; ABounds: TRect; radius: single);
  protected
    procedure DoExecute; override;
  end;

  { TMotionBlurTask }

  TMotionBlurTask = class(TFilterTask)
  private
    FBounds: TRect;
    FDistance,FAngle: single;
    FOriented: boolean;
  public
    constructor Create(ABmp: TBGRACustomBitmap; ABounds: TRect; ADistance, AAngle: single; AOriented: boolean);
  protected
    procedure DoExecute; override;
  end;

procedure FilterBlurRadial(bmp: TBGRACustomBitmap; ABounds: TRect; radius: integer;
  blurType: TRadialBlurType; ADestination: TBGRACustomBitmap; ACheckShouldStop: TCheckShouldStopFunc); forward;
procedure FilterBlurRadialPrecise(bmp: TBGRACustomBitmap; ABounds: TRect;
  radius: single; ADestination: TBGRACustomBitmap; ACheckShouldStop: TCheckShouldStopFunc); forward;
procedure FilterBlurMotion(bmp: TBGRACustomBitmap; ABounds: TRect; distance: single;
  angle: single; oriented: boolean; ADestination: TBGRACustomBitmap; ACheckShouldStop: TCheckShouldStopFunc); forward;
procedure FilterBlur(bmp: TBGRACustomBitmap; ABounds: TRect;
   blurMask: TBGRACustomBitmap; ADestination: TBGRACustomBitmap; ACheckShouldStop: TCheckShouldStopFunc); forward;

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

{ This filter compute for each pixel the mean of the eight surrounding pixels,
  then the difference between this average pixel and the pixel at the center
  of the square. Finally the difference is added to the new pixel, exagerating
  its difference with its neighbours. }
function FilterSharpen(bmp: TBGRACustomBitmap; ABounds: TRect; AAmount: integer = 256): TBGRACustomBitmap;
var
  yb, xcount: Int32or64;
  dx, dy: Int32or64;
  a_pixels: array[-2..1,-2..1] of PBGRAPixel;
  sumR, sumG, sumB, sumA, {RGBdiv, }nbA: UInt32or64;
  refPixel: TBGRAPixel;
  pdest,ptempPixel:    PBGRAPixel;
  bounds:   TRect;
  Amount256: boolean;
  lastXincluded: boolean;
  alpha,rgbDivShr1: uint32or64;
begin
  if IsRectEmpty(ABounds) then exit;
  Amount256 := AAmount = 256;
  Result := bmp.NewBitmap(bmp.Width, bmp.Height);

  //determine where pixels are in the bitmap
  bounds := bmp.GetImageBounds;
  if not IntersectRect(bounds, bounds,ABounds) then exit;
  bounds.Left   := max(0, bounds.Left - 1);
  bounds.Top    := max(0, bounds.Top - 1);
  bounds.Right  := min(bmp.Width, bounds.Right + 1);
  bounds.Bottom := min(bmp.Height, bounds.Bottom + 1);
  lastXincluded:= bounds.Right < bmp.Width;

  //loop through the destination bitmap
  for yb := bounds.Top to bounds.Bottom - 1 do
  begin
    pdest := Result.scanline[yb] + bounds.Left;
    fillchar({%H-}a_pixels,sizeof(a_pixels),0);
    for dy := -1 to 1 do
      if (yb+dy >= bounds.Top) and (yb+dy < bounds.Bottom) then
        a_pixels[dy,1] := bmp.ScanLine[yb+dy]+bounds.Left else
          a_pixels[dy,1] := nil;
    xcount := bounds.right-bounds.left;
    while xcount > 0 do
    begin
      dec(xcount);

      //for each pixel, read eight surrounding pixels in the source bitmap
      for dy := -1 to 1 do
        for dx := -1 to 0 do
          a_pixels[dy,dx] := a_pixels[dy,dx+1];
      if (xcount > 0) or lastXincluded then
      begin
        for dy := -1 to 1 do
          if a_pixels[dy,0] <> nil then a_pixels[dy,1] := a_pixels[dy,0]+1;
      end;

      //compute sum
      sumR   := 0;
      sumG   := 0;
      sumB   := 0;
      sumA   := 0;
      //RGBdiv := 0;
      nbA    := 0;

       {$hints off}
      for dy := -1 to 1 do
        for dx := -1 to 1 do
        if (dx<>0) or (dy<>0) then
        begin
          ptempPixel := a_pixels[dy,dx];
          if ptempPixel <> nil then
          begin
            alpha := ptempPixel^.alpha;
            sumR      += ptempPixel^.red * alpha;
            sumG      += ptempPixel^.green * alpha;
            sumB      += ptempPixel^.blue * alpha;
            //RGBdiv    += alpha;
            sumA      += alpha;
            Inc(nbA);
          end;
        end;
       {$hints on}

      //we finally have an average pixel
      if ({RGBdiv}sumA = 0) then
        refPixel := BGRAPixelTransparent
      else
      begin
        rgbDivShr1:= {RGBDiv}sumA shr 1;
        refPixel.red   := (sumR + rgbDivShr1) div {RGBdiv}sumA;
        refPixel.green := (sumG + rgbDivShr1) div {RGBdiv}sumA;
        refPixel.blue  := (sumB + rgbDivShr1) div {RGBdiv}sumA;
        refPixel.alpha := (sumA + nbA shr 1) div nbA;
      end;

      //read the pixel at the center of the square
      ptempPixel := a_pixels[0,0];
      if refPixel <> BGRAPixelTransparent then
      begin
        //compute sharpened pixel by adding the difference
        if not Amount256 then
          pdest^ := BGRA( max(0, min($FFFF, Int32or64(ptempPixel^.red shl 8) +
            AAmount*(ptempPixel^.red - refPixel.red))) shr 8,
              max(0, min($FFFF, Int32or64(ptempPixel^.green shl 8) +
            AAmount*(ptempPixel^.green - refPixel.green))) shr 8,
             max(0, min($FFFF, Int32or64(ptempPixel^.blue shl 8) +
            AAmount*(ptempPixel^.blue - refPixel.blue))) shr 8,
             max(0, min($FFFF, Int32or64(ptempPixel^.alpha shl 8) +
            AAmount*(ptempPixel^.alpha - refPixel.alpha))) shr 8 )
        else
          pdest^ := BGRA( max(0, min(255, (ptempPixel^.red shl 1) - refPixel.red)),
             max(0, min(255, (ptempPixel^.green shl 1) - refPixel.green)),
             max(0, min(255, (ptempPixel^.blue shl 1) - refPixel.blue)),
             max(0, min(255, (ptempPixel^.alpha shl 1) - refPixel.alpha)));
      end else
        pdest^ := ptempPixel^;
      Inc(pdest);
    end;
  end;
  Result.InvalidateBitmap;
end;

function FilterSharpen(bmp: TBGRACustomBitmap; AAmount: integer
  ): TBGRACustomBitmap;
begin
  result := FilterSharpen(bmp,rect(0,0,bmp.Width,bmp.Height),AAmount);
end;

{ Precise blur builds a blur mask with a gradient fill and use
  general purpose blur }
procedure FilterBlurRadialPrecise(bmp: TBGRACustomBitmap;
  ABounds: TRect; radius: single; ADestination: TBGRACustomBitmap; ACheckShouldStop: TCheckShouldStopFunc);
var
  blurShape: TBGRACustomBitmap;
  intRadius: integer;
begin
  if radius = 0 then
  begin
    ADestination.PutImagePart(ABounds.Left,ABounds.Top,bmp,ABounds,dmSet);
    exit;
  end;
  intRadius := ceil(radius);
  blurShape := bmp.NewBitmap(2 * intRadius + 1, 2 * intRadius + 1);
  blurShape.GradientFill(0, 0, blurShape.Width, blurShape.Height, BGRAWhite,
    BGRABlack, gtRadial, pointF(intRadius, intRadius), pointF(
    intRadius - radius - 1, intRadius), dmSet);
  FilterBlur(bmp, ABounds, blurShape, ADestination, ACheckShouldStop);
  blurShape.Free;
end;

function FilterBlurRadialPrecise(bmp: TBGRACustomBitmap; radius: single
  ): TBGRACustomBitmap;
begin
  result := bmp.NewBitmap(bmp.Width,bmp.Height);
  FilterBlurRadialPrecise(bmp, rect(0,0,bmp.Width,bmp.Height), radius, result, nil);
end;

function CreateRadialPreciseBlurTask(ABmp: TBGRACustomBitmap; ABounds: TRect;
  ARadius: single): TFilterTask;
begin
  result := TRadialPreciseBlurTask.Create(ABmp,ABounds,ARadius);
end;

{ This is a clever solution for fast computing of the blur
  effect : it stores an array of vertical sums forming a square
  around the pixel which moves with it. For each new pixel,
  the vertical sums are kept except for the last column of
  the square }
procedure FilterBlurFast(bmp: TBGRACustomBitmap; ABounds: TRect;
  radius: integer; ADestination: TBGRACustomBitmap; ACheckShouldStop: TCheckShouldStopFunc);

  type
    TRowSum = record
      sumR,sumG,sumB,rgbDiv,sumA,aDiv: uint32or64;
    end;

  function ComputeAverage(sum: TRowSum): TBGRAPixel;
  begin
    result.alpha:= (sum.sumA+sum.aDiv shr 1) div sum.aDiv;
    result.red := (sum.sumR+sum.rgbDiv shr 1) div sum.rgbDiv;
    result.green := (sum.sumG+sum.rgbDiv shr 1) div sum.rgbDiv;
    result.blue := (sum.sumB+sum.rgbDiv shr 1) div sum.rgbDiv;
  end;

  {$I blurfast.inc}

{ Normal radial blur compute a blur mask with a GradientFill and
  then posterize to optimize general purpose blur }
procedure FilterBlurRadialNormal(bmp: TBGRACustomBitmap;
  ABounds: TRect; radius: integer; ADestination: TBGRACustomBitmap; ACheckShouldStop: TCheckShouldStopFunc);
var
  blurShape: TBGRACustomBitmap;
  n: Int32or64;
  p: PBGRAPixel;
begin
  if radius = 0 then
  begin
    ADestination.PutImagePart(ABounds.Left,ABounds.Top,bmp,ABounds,dmSet);
    exit;
  end;
  blurShape := bmp.NewBitmap(2 * radius + 1, 2 * radius + 1);
  blurShape.GradientFill(0, 0, blurShape.Width, blurShape.Height, BGRAWhite,
    BGRABlack, gtRadial, pointF(radius, radius), pointF(-0.5, radius), dmSet);
  p := blurShape.Data;
  for n := 0 to blurShape.NbPixels-1 do
  begin
    p^.red := p^.red and $F0;
    p^.green := p^.red;
    p^.blue := p^.red;
    inc(p);
  end;
  FilterBlur(bmp, ABounds, blurShape, ADestination, ACheckShouldStop);
  blurShape.Free;
end;

{ Blur disk creates a disk mask with a FillEllipse }
procedure FilterBlurDisk(bmp: TBGRACustomBitmap; ABounds: TRect; radius: integer; ADestination: TBGRACustomBitmap; ACheckShouldStop: TCheckShouldStopFunc);
var
  blurShape: TBGRACustomBitmap;
begin
  if radius = 0 then
  begin
    ADestination.PutImagePart(ABounds.Left,ABounds.Top,bmp,ABounds,dmSet);
    exit;
  end;
  blurShape := bmp.NewBitmap(2 * radius + 1, 2 * radius + 1);
  blurShape.Fill(BGRABlack);
  blurShape.FillEllipseAntialias(radius, radius, radius + 0.5, radius + 0.5, BGRAWhite);
  FilterBlur(bmp, ABounds, blurShape, ADestination, ACheckShouldStop);
  blurShape.Free;
end;

{ Corona blur use a circle as mask }
procedure FilterBlurCorona(bmp: TBGRACustomBitmap; ABounds: TRect; radius: integer; ADestination: TBGRACustomBitmap; ACheckShouldStop: TCheckShouldStopFunc);
var
  blurShape: TBGRACustomBitmap;
begin
  if radius = 0 then
  begin
    ADestination.PutImagePart(ABounds.Left,ABounds.Top,bmp,ABounds,dmSet);
    exit;
  end;
  blurShape := bmp.NewBitmap(2 * radius + 1, 2 * radius + 1);
  blurShape.Fill(BGRABlack);
  blurShape.EllipseAntialias(radius, radius, radius, radius, BGRAWhite, 1);
  FilterBlur(bmp, ABounds, blurShape, ADestination, ACheckShouldStop);
  blurShape.Free;
end;

procedure FilterBlurRadial(bmp: TBGRACustomBitmap; ABounds: TRect; radius: integer;
  blurType: TRadialBlurType; ADestination: TBGRACustomBitmap; ACheckShouldStop: TCheckShouldStopFunc);
begin
  if radius = 0 then
  begin
    ADestination.PutImagePart(ABounds.Left,ABounds.Top,bmp,ABounds,dmSet);
    exit;
  end;
  case blurType of
    rbCorona:  FilterBlurCorona(bmp, ABounds, radius, ADestination, ACheckShouldStop);
    rbDisk:    FilterBlurDisk(bmp, ABounds, radius, ADestination, ACheckShouldStop);
    rbNormal:  FilterBlurRadialNormal(bmp, ABounds, radius, ADestination, ACheckShouldStop);
    rbFast:    FilterBlurFast(bmp, ABounds, radius, ADestination, ACheckShouldStop);
    rbPrecise: FilterBlurRadialPrecise(bmp, ABounds, radius / 10, ADestination, ACheckShouldStop);
  end;
end;

function FilterBlurRadial(bmp: TBGRACustomBitmap; radius: integer;
  blurType: TRadialBlurType): TBGRACustomBitmap;
begin
  result := bmp.NewBitmap(bmp.width,bmp.Height);
  FilterBlurRadial(bmp, rect(0,0,bmp.Width,bmp.height), radius, blurType,result,nil);
end;

function CreateRadialBlurTask(ABmp: TBGRACustomBitmap; ABounds: TRect; ARadius: integer;
  ABlurType: TRadialBlurType): TFilterTask;
begin
  result := TRadialBlurTask.Create(ABmp,ABounds,ARadius,ABlurType);
end;

{ This filter draws an antialiased line to make the mask, and
  if the motion blur is oriented, does a GradientFill to orient it }
procedure FilterBlurMotion(bmp: TBGRACustomBitmap; ABounds: TRect; distance: single;
  angle: single; oriented: boolean; ADestination: TBGRACustomBitmap; ACheckShouldStop: TCheckShouldStopFunc);
var
  blurShape: TBGRACustomBitmap;
  intRadius: integer;
  dx, dy, d: single;
begin
  if distance < 1e-6 then
  begin
    ADestination.PutImagePart(ABounds.Left,ABounds.Top,bmp,ABounds,dmSet);
    exit;
  end;
  intRadius := ceil(distance / 2);
  blurShape := bmp.NewBitmap(2 * intRadius + 1, 2 * intRadius + 1);
  d  := distance / 2;
  dx := cos(angle * Pi / 180);
  dy := sin(angle * Pi / 180);
  blurShape.Fill(BGRABlack);
  blurShape.DrawLineAntialias(intRadius - dx * d, intRadius - dy *
    d, intRadius + dx * d, intRadius + dy * d, BGRAWhite, 1, True);
  if oriented then
    blurShape.GradientFill(0, 0, blurShape.Width, blurShape.Height,
      BGRAPixelTransparent, BGRABlack, gtRadial, pointF(intRadius -
      dx * d, intRadius - dy * d),
      pointF(intRadius + dx * (d + 0.5), intRadius + dy * (d + 0.5)),
      dmFastBlend, False);
  FilterBlur(bmp, ABounds, blurShape, ADestination, ACheckShouldStop);
  blurShape.Free;
end;

function FilterBlurMotion(bmp: TBGRACustomBitmap; distance: single;
  angle: single; oriented: boolean): TBGRACustomBitmap;
begin
  result := bmp.NewBitmap(bmp.Width,bmp.Height);
  FilterBlurMotion(bmp,rect(0,0,bmp.Width,bmp.Height),distance,angle,oriented,result,nil);
end;

function CreateMotionBlurTask(ABmp: TBGRACustomBitmap; ABounds: TRect;
  ADistance, AAngle: single; AOriented: boolean): TFilterTask;
begin
  result := TMotionBlurTask.Create(ABmp,ABounds,ADistance,AAngle,AOriented);
end;

{ General purpose blur : compute pixel sum according to the mask and then
  compute only difference while scanning from the left to the right }
procedure FilterBlurSmallMask(bmp: TBGRACustomBitmap;
  blurMask: TBGRACustomBitmap; ABounds: TRect; ADestination: TBGRACustomBitmap; ACheckShouldStop: TCheckShouldStopFunc); forward;
procedure FilterBlurSmallMaskWithShift(bmp: TBGRACustomBitmap;
  blurMask: TBGRACustomBitmap; maskShift: integer; ABounds: TRect; ADestination: TBGRACustomBitmap; ACheckShouldStop: TCheckShouldStopFunc); forward;
procedure FilterBlurBigMask(bmp: TBGRACustomBitmap;
  blurMask: TBGRACustomBitmap; ABounds: TRect; ADestination: TBGRACustomBitmap; ACheckShouldStop: TCheckShouldStopFunc); forward;
procedure FilterBlurMask64(bmp: TBGRACustomBitmap;
  blurMask: TBGRACustomBitmap; ABounds: TRect; ADestination: TBGRACustomBitmap; ACheckShouldStop: TCheckShouldStopFunc); forward;

//make sure value is in the range 0..255
function clampByte(value: Int32or64): byte; inline;
begin
  if value < 0 then result := 0 else
  if value > 255 then result := 255 else
    result := value;
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

function FilterBlur(bmp: TBGRACustomBitmap; blurMask: TBGRACustomBitmap): TBGRACustomBitmap;
begin
  result := bmp.NewBitmap(bmp.Width,bmp.Height);
  FilterBlur(bmp,rect(0,0,bmp.Width,bmp.Height),blurMask,result,nil);
end;

function CreateBlurTask(ABmp: TBGRACustomBitmap; ABounds: TRect;
  AMask: TBGRACustomBitmap; AMaskIsThreadSafe: boolean): TFilterTask;
begin
  result := TCustomBlurTask.Create(ABmp,ABounds,AMask,AMaskIsThreadSafe);
end;

procedure FilterBlur(bmp: TBGRACustomBitmap;
  ABounds: TRect; blurMask: TBGRACustomBitmap; ADestination: TBGRACustomBitmap; ACheckShouldStop: TCheckShouldStopFunc);
{$IFDEF CPU64}
begin
    FilterBlurMask64(bmp,blurMask,ABounds,ADestination,ACheckShouldStop);
end;
{$ELSE}
var
  maskSum: int64;
  i: Int32or64;
  p: PBGRAPixel;
  maskShift: integer;
begin
  maskSum := 0;
  p := blurMask.data;
  for i := 0 to blurMask.NbPixels-1 do
  begin
    inc(maskSum,p^.red);
    inc(p);
  end;
  maskShift := 0;
  while maskSum > 32768 do
  begin
    inc(maskShift);
    maskSum := maskSum shr 1;
  end;
  //check if sum can be stored in a 32-bit signed integer
  if maskShift = 0 then
    FilterBlurSmallMask(bmp,blurMask,ABounds,ADestination,ACheckShouldStop) else
  {$IFDEF CPU32}
  if maskShift < 8 then
    FilterBlurSmallMaskWithShift(bmp,blurMask,maskShift,ABounds,ADestination,ACheckShouldStop) else
  {$ENDIF}
    FilterBlurBigMask(bmp,blurMask,ABounds,ADestination,ACheckShouldStop);
end;
{$ENDIF}

//32-bit blur with shift
procedure FilterBlurSmallMaskWithShift(bmp: TBGRACustomBitmap;
  blurMask: TBGRACustomBitmap; maskShift: integer; ABounds: TRect; ADestination: TBGRACustomBitmap; ACheckShouldStop: TCheckShouldStopFunc);

  var
    sumR, sumG, sumB, sumA, Adiv, RGBdiv : integer;

  function ComputeAverage: TBGRAPixel; inline;
  begin
    result.alpha := (sumA + Adiv shr 1) div Adiv;
    if result.alpha = 0 then
      result := BGRAPixelTransparent
    else
    begin
      result.red   := clampByte((sumR + RGBdiv shr 1) div RGBdiv);
      result.green := clampByte((sumG + RGBdiv shr 1) div RGBdiv);
      result.blue  := clampByte((sumB + RGBdiv shr 1) div RGBdiv);
    end;
  end;

  {$define PARAM_MASKSHIFT}
  {$I blurnormal.inc}

//32-bit blur
procedure FilterBlurSmallMask(bmp: TBGRACustomBitmap;
  blurMask: TBGRACustomBitmap; ABounds: TRect; ADestination: TBGRACustomBitmap; ACheckShouldStop: TCheckShouldStopFunc);

  var
    sumR, sumG, sumB, sumA, Adiv : integer;

  function ComputeAverage: TBGRAPixel; inline;
  begin
    result.alpha := (sumA + Adiv shr 1) div Adiv;
    if result.alpha = 0 then
      result := BGRAPixelTransparent
    else
    begin
      result.red   := clampByte((sumR + sumA shr 1) div sumA);
      result.green := clampByte((sumG + sumA shr 1) div sumA);
      result.blue  := clampByte((sumB + sumA shr 1) div sumA);
    end;
  end;

  {$I blurnormal.inc}

//64-bit blur
procedure FilterBlurMask64(bmp: TBGRACustomBitmap;
  blurMask: TBGRACustomBitmap; ABounds: TRect; ADestination: TBGRACustomBitmap; ACheckShouldStop: TCheckShouldStopFunc);

  var
    sumR, sumG, sumB, sumA, Adiv : int64;

  function ComputeAverage: TBGRAPixel; inline;
  begin
    result.alpha := (sumA + Adiv shr 1) div Adiv;
    if result.alpha = 0 then
      result := BGRAPixelTransparent
    else
    begin
      result.red   := clampByte((sumR + sumA shr 1) div sumA);
      result.green := clampByte((sumG + sumA shr 1) div sumA);
      result.blue  := clampByte((sumB + sumA shr 1) div sumA);
    end;
  end;

  {$I blurnormal.inc}

//floating point blur
procedure FilterBlurBigMask(bmp: TBGRACustomBitmap;
  blurMask: TBGRACustomBitmap; ABounds: TRect; ADestination: TBGRACustomBitmap; ACheckShouldStop: TCheckShouldStopFunc);

  var
    sumR, sumG, sumB, sumA, Adiv : single;

  function ComputeAverage: TBGRAPixel; inline;
  begin
    result.alpha := round(sumA/Adiv);
    if result.alpha = 0 then
      result := BGRAPixelTransparent
    else
    begin
      result.red   := clampByte(round(sumR/sumA));
      result.green := clampByte(round(sumG/sumA));
      result.blue  := clampByte(round(sumB/sumA));
    end;
  end;

  {$I blurnormal.inc}
function FilterEmboss(bmp: TBGRACustomBitmap; angle: single): TBGRACustomBitmap;
begin
  result := FilterEmboss(bmp, angle, rect(0,0,bmp.Width,bmp.Height));
end;

{ Emboss filter computes the difference between each pixel and the surrounding pixels
  in the specified direction. }
function FilterEmboss(bmp: TBGRACustomBitmap; angle: single; ABounds: TRect): TBGRACustomBitmap;
var
  yb, xb: Int32or64;
  dx, dy: single;
  idx1, idy1, idx2, idy2, idx3, idy3, idx4, idy4: Int32or64;
  w:      array[1..4] of single;
  iw:     uint32or64;
  c:      array[0..4] of TBGRAPixel;

  i:     Int32or64;
  sumR, sumG, sumB, sumA, RGBdiv, Adiv: UInt32or64;
  tempPixel, refPixel: TBGRAPixel;
  pdest: PBGRAPixel;

  bounds: TRect;
  onHorizBorder: boolean;
  psrc: array[-1..1] of PBGRAPixel;
begin
  if IsRectEmpty(ABounds) then exit;
  //compute pixel position and weight
  dx   := cos(angle * Pi / 180);
  dy   := sin(angle * Pi / 180);
  idx1 := floor(dx);
  idy1 := floor(dy);
  idx2 := ceil(dx);
  idy2 := ceil(dy);
  idx3 := idx1;
  idy3 := idy2;
  idx4 := idx2;
  idy4 := idy1;

  w[1] := (1 - abs(idx1 - dx)) * (1 - abs(idy1 - dy));
  w[2] := (1 - abs(idx2 - dx)) * (1 - abs(idy2 - dy));
  w[3] := (1 - abs(idx3 - dx)) * (1 - abs(idy3 - dy));
  w[4] := (1 - abs(idx4 - dx)) * (1 - abs(idy4 - dy));

  //fill with gray
  Result := bmp.NewBitmap(bmp.Width, bmp.Height);
  Result.Fill(BGRA(128, 128, 128, 255));

  bounds := bmp.GetImageBounds;
  if not IntersectRect(bounds, bounds, ABounds) then exit;
  bounds.Left   := max(0, bounds.Left - 1);
  bounds.Top    := max(0, bounds.Top - 1);
  bounds.Right  := min(bmp.Width, bounds.Right + 1);
  bounds.Bottom := min(bmp.Height, bounds.Bottom + 1);

  //loop through destination
  for yb := bounds.Top to bounds.bottom - 1 do
  begin
    pdest := Result.scanline[yb] + bounds.Left;
    onHorizBorder:= (yb=0) or (yb=bmp.Height-1);
    psrc[0] := bmp.ScanLine[yb]+bounds.Left;
    if (yb>0) then psrc[-1] := bmp.ScanLine[yb-1]+bounds.Left else psrc[-1] := nil;
    if (yb<bmp.Height-1) then psrc[1] := bmp.ScanLine[yb+1]+bounds.Left else psrc[1] := nil;
    for xb := bounds.Left to bounds.Right - 1 do
    begin
      c[0] := psrc[0]^;
      if onHorizBorder or (xb=0) or (xb=bmp.Width-1) then
      begin
        c[1] := bmp.getPixel(xb + idx1, yb + idy1);
        c[2] := bmp.getPixel(xb + idx2, yb + idy2);
        c[3] := bmp.getPixel(xb + idx3, yb + idy3);
        c[4] := bmp.getPixel(xb + idx4, yb + idy4);
      end else
      begin
        c[1] := (psrc[idy1]+idx1)^;
        c[2] := (psrc[idy2]+idx2)^;
        c[3] := (psrc[idy3]+idx3)^;
        c[4] := (psrc[idy4]+idx4)^;
      end;

      sumR   := 0;
      sumG   := 0;
      sumB   := 0;
      sumA   := 0;
      Adiv   := 0;
      RGBdiv := 0;

      //compute sum
       {$hints off}
      for i := 1 to 4 do
      begin
        tempPixel := c[i];
        if tempPixel.alpha = 0 then
          tempPixel := c[0];
        iw     := round(w[i] * tempPixel.alpha);
        sumR   += tempPixel.red * iw;
        sumG   += tempPixel.green * iw;
        sumB   += tempPixel.blue * iw;
        RGBdiv += iw;
        sumA   += iw;
        Adiv   += round(w[i] * 255);
      end;
       {$hints on}

      //average
      if (Adiv = 0) or (RGBdiv = 0) then
        refPixel := c[0]
      else
      begin
        refPixel.red   := (sumR + RGBdiv shr 1) div RGBdiv;
        refPixel.green := (sumG + RGBdiv shr 1) div RGBdiv;
        refPixel.blue  := (sumB + RGBdiv shr 1) div RGBdiv;
        refPixel.alpha := (sumA * 255 + Adiv shr 1) div Adiv;
      end;

      //difference with center pixel
       {$hints off}
      tempPixel.red := max(0, min(512 * 255, 65536 + refPixel.red *
        refPixel.alpha - c[0].red * c[0].alpha)) shr 9;
      tempPixel.green := max(0, min(512 * 255, 65536 + refPixel.green *
        refPixel.alpha - c[0].green * c[0].alpha)) shr 9;
      tempPixel.blue := max(0, min(512 * 255, 65536 + refPixel.blue *
        refPixel.alpha - c[0].blue * c[0].alpha)) shr 9;
       {$hints on}
      tempPixel.alpha := 255;
      pdest^ := tempPixel;
      Inc(pdest);
      inc(psrc[0]);
      if psrc[-1] <> nil then inc(psrc[-1]);
      if psrc[1] <> nil then inc(psrc[1]);
    end;
  end;
  Result.InvalidateBitmap;
end;

{ Like general emboss, but with fixed direction and automatic color with transparency }
function FilterEmbossHighlight(bmp: TBGRACustomBitmap;
  FillSelection: boolean; DefineBorderColor: TBGRAPixel): TBGRACustomBitmap;
var
  yb, xb: Int32or64;
  c0,c1,c2,c3,c4,c5,c6: Int32or64;

  bmpWidth, bmpHeight: Int32or64;
  slope, h: byte;
  sum:      Int32or64;
  tempPixel, highlight: TBGRAPixel;
  pdest, psrcUp, psrc, psrcDown: PBGRAPixel;

  bounds: TRect;
  borderColorOverride: boolean;
  borderColorLevel: Int32or64;

  currentBorderColor: Int32or64;
begin
  borderColorOverride := DefineBorderColor.alpha <> 0;
  borderColorLevel := DefineBorderColor.red;

  bmpWidth  := bmp.Width;
  bmpHeight := bmp.Height;
  Result    := bmp.NewBitmap(bmpWidth, bmpHeight);

  if borderColorOverride then
    bounds := bmp.GetImageBounds(cRed, borderColorLevel)
  else
    bounds := bmp.GetImageBounds(cRed);
  if (bounds.Right <= bounds.Left) or (bounds.Bottom <= Bounds.Top) then
    exit;
  bounds.Left   := max(0, bounds.Left - 1);
  bounds.Top    := max(0, bounds.Top - 1);
  bounds.Right  := min(bmpWidth, bounds.Right + 1);
  bounds.Bottom := min(bmpHeight, bounds.Bottom + 1);

  currentBorderColor := borderColorLevel;
  for yb := bounds.Top to bounds.Bottom - 1 do
  begin
    pdest := Result.scanline[yb] + bounds.Left;

    if yb > 0 then
      psrcUp := bmp.Scanline[yb - 1] + bounds.Left
    else
      psrcUp := nil;
    psrc := bmp.scanline[yb] + bounds.Left;
    if yb < bmpHeight - 1 then
      psrcDown := bmp.scanline[yb + 1] + bounds.Left
    else
      psrcDown := nil;

    for xb := bounds.Left to bounds.Right - 1 do
    begin
      c0 := pbyte(psrc)^;
      if not borderColorOverride then currentBorderColor := c0;
      if (xb = 0) then
      begin
        c1 := currentBorderColor;
        c2 := currentBorderColor;
      end
      else
      begin
        if psrcUp <> nil then
          c1 := pbyte(psrcUp - 1)^
        else
          c1 := currentBorderColor;
        c2 := pbyte(psrc - 1)^;
      end;
      if psrcUp <> nil then
      begin
        c3 := pbyte(psrcUp)^;
        Inc(psrcUp);
      end
      else
       c3 := currentBorderColor;

      if (xb = bmpWidth - 1) then
      begin
        c4 := currentBorderColor;
        c5 := currentBorderColor;
      end
      else
      begin
        if psrcDown <> nil then
          c4 := pbyte(psrcDown + 1)^
        else
          c4 := currentBorderColor;
        c5 := pbyte(psrc + 1)^;
      end;
      if psrcDown <> nil then
      begin
        c6 := pbyte(psrcDown)^;
        Inc(psrcDown);
      end
      else
        c6 := currentBorderColor;
      Inc(psrc);

      sum := c4+c5+c6-c1-c2-c3;
      sum := 128 + sum div 3;
      if sum > 255 then
        slope := 255
      else
      if sum < 1 then
        slope := 1
      else
        slope := sum;
      h := c0;

      tempPixel.red   := slope;
      tempPixel.green := slope;
      tempPixel.blue  := slope;
      tempPixel.alpha := abs(slope - 128) * 2;

      if fillSelection then
      begin
        highlight := BGRA(h shr 2, h shr 1, h, h shr 1);
        if tempPixel.red < highlight.red then
          tempPixel.red := highlight.red;
        if tempPixel.green < highlight.green then
          tempPixel.green := highlight.green;
        if tempPixel.blue < highlight.blue then
          tempPixel.blue := highlight.blue;
        if tempPixel.alpha < highlight.alpha then
          tempPixel.alpha := highlight.alpha;
      end;

      pdest^ := tempPixel;
      Inc(pdest);
    end;
  end;
  Result.InvalidateBitmap;
end;

function FilterEmbossHighlightOffset(bmp: TBGRACustomBitmap;
  FillSelection: boolean; DefineBorderColor: TBGRAPixel; var Offset: TPoint): TBGRACustomBitmap;
var
  yb, xb: int32or64;
  c0,c1,c2,c3,c4,c5,c6: int32or64;

  bmpWidth, bmpHeight: int32or64;
  slope, h: byte;
  sum:      int32or64;
  tempPixel, highlight: TBGRAPixel;
  pdest, psrcUp, psrc, psrcDown: PBGRAPixel;

  bounds: TRect;
  borderColorOverride: boolean;
  borderColorLevel: int32or64;

  currentBorderColor: int32or64;
begin
  borderColorOverride := DefineBorderColor.alpha <> 0;
  borderColorLevel := DefineBorderColor.red;

  bmpWidth  := bmp.Width;
  bmpHeight := bmp.Height;

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
  bounds.Right  := min(bmpWidth, bounds.Right + 1);
  bounds.Bottom := min(bmpHeight, bounds.Bottom + 1);

  Result    := bmp.NewBitmap(bounds.Right-Bounds.Left+1, bounds.Bottom-Bounds.Top+1);
  inc(Offset.X, bounds.Left);
  inc(Offset.Y, bounds.Top);

  currentBorderColor := borderColorLevel;
  for yb := bounds.Top to bounds.Bottom - 1 do
  begin
    pdest := Result.scanline[yb-Bounds.Top];

    if yb > 0 then
      psrcUp := bmp.Scanline[yb - 1] + bounds.Left
    else
      psrcUp := nil;
    psrc := bmp.scanline[yb] + bounds.Left;
    if yb < bmpHeight - 1 then
      psrcDown := bmp.scanline[yb + 1] + bounds.Left
    else
      psrcDown := nil;

    for xb := bounds.Left to bounds.Right - 1 do
    begin
      c0 := pbyte(psrc)^;
      if not borderColorOverride then currentBorderColor := c0;
      if (xb = 0) then
      begin
        c1 := currentBorderColor;
        c2 := currentBorderColor;
      end
      else
      begin
        if psrcUp <> nil then
          c1 := pbyte(psrcUp - 1)^
        else
          c1 := currentBorderColor;
        c2 := pbyte(psrc - 1)^;
      end;
      if psrcUp <> nil then
      begin
        c3 := pbyte(psrcUp)^;
        Inc(psrcUp);
      end
      else
       c3 := currentBorderColor;

      if (xb = bmpWidth - 1) then
      begin
        c4 := currentBorderColor;
        c5 := currentBorderColor;
      end
      else
      begin
        if psrcDown <> nil then
          c4 := pbyte(psrcDown + 1)^
        else
          c4 := currentBorderColor;
        c5 := pbyte(psrc + 1)^;
      end;
      if psrcDown <> nil then
      begin
        c6 := pbyte(psrcDown)^;
        Inc(psrcDown);
      end
      else
        c6 := currentBorderColor;
      Inc(psrc);

      sum := c4+c5+c6-c1-c2-c3;
      sum := 128 + sum div 3;
      if sum > 255 then
        slope := 255
      else
      if sum < 1 then
        slope := 1
      else
        slope := sum;
      h := c0;

      tempPixel.red   := slope;
      tempPixel.green := slope;
      tempPixel.blue  := slope;
      tempPixel.alpha := abs(slope - 128) * 2;

      if fillSelection then
      begin
        highlight := BGRA(h shr 2, h shr 1, h, h shr 1);
        if tempPixel.red < highlight.red then
          tempPixel.red := highlight.red;
        if tempPixel.green < highlight.green then
          tempPixel.green := highlight.green;
        if tempPixel.blue < highlight.blue then
          tempPixel.blue := highlight.blue;
        if tempPixel.alpha < highlight.alpha then
          tempPixel.alpha := highlight.alpha;
      end;

      pdest^ := tempPixel;
      Inc(pdest);
    end;
  end;
  Result.InvalidateBitmap;
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
var
  psrc, pdest: PBGRAPixel;
  c: TExpandedPixel;
  xcount,xb,yb: int32or64;
  minValRed, maxValRed, minValGreen, maxValGreen, minValBlue, maxValBlue,
  minAlpha, maxAlpha, addValRed, addValGreen, addValBlue, addAlpha: word;
  factorValRed, factorValGreen, factorValBlue, factorAlpha: int32or64;
begin
  if not IntersectRect(ABounds,ABounds,rect(0,0,bmp.Width,bmp.Height)) then exit;
  Result := bmp.NewBitmap(bmp.Width, bmp.Height);
  bmp.LoadFromBitmapIfNeeded;
  maxValRed := 0;
  minValRed := 65535;
  maxValGreen := 0;
  minValGreen := 65535;
  maxValBlue := 0;
  minValBlue := 65535;
  maxAlpha  := 0;
  minAlpha  := 65535;
  xcount := ABounds.Right-ABounds.Left;
  for yb := ABounds.Top to ABounds.Bottom-1 do
  begin
    psrc := bmp.ScanLine[yb]+ABounds.Left;
    for xb := xcount-1 downto 0 do
    begin
      c := GammaExpansion(psrc^);
      Inc(psrc);
      if c.red > maxValRed then
        maxValRed := c.red;
      if c.green > maxValGreen then
        maxValGreen := c.green;
      if c.blue > maxValBlue then
        maxValBlue := c.blue;
      if c.red < minValRed then
        minValRed := c.red;
      if c.green < minValGreen then
        minValGreen := c.green;
      if c.blue < minValBlue then
        minValBlue := c.blue;

      if c.alpha > maxAlpha then
        maxAlpha := c.alpha;
      if c.alpha < minAlpha then
        minAlpha := c.alpha;
    end;
  end;
  if not eachChannel then
  begin
    minValRed   := min(min(minValRed, minValGreen), minValBlue);
    maxValRed   := max(max(maxValRed, maxValGreen), maxValBlue);
    minValGreen := minValRed;
    maxValGreen := maxValRed;
    minValBlue  := minValBlue;
    maxValBlue  := maxValBlue;
  end;
  if maxValRed > minValRed then
  begin
    factorValRed := 268431360 div (maxValRed - minValRed);
    addValRed    := 0;
  end
  else
  begin
    factorValRed := 0;
    if minValRed = 0 then
      addValRed := 0
    else
      addValRed := 65535;
  end;
  if maxValGreen > minValGreen then
  begin
    factorValGreen := 268431360 div (maxValGreen - minValGreen);
    addValGreen    := 0;
  end
  else
  begin
    factorValGreen := 0;
    if minValGreen = 0 then
      addValGreen := 0
    else
      addValGreen := 65535;
  end;
  if maxValBlue > minValBlue then
  begin
    factorValBlue := 268431360 div (maxValBlue - minValBlue);
    addValBlue    := 0;
  end
  else
  begin
    factorValBlue := 0;
    if minValBlue = 0 then
      addValBlue := 0
    else
      addValBlue := 65535;
  end;
  if maxAlpha > minAlpha then
  begin
    factorAlpha := 268431360 div (maxAlpha - minAlpha);
    addAlpha    := 0;
  end
  else
  begin
    factorAlpha := 0;
    if minAlpha = 0 then
      addAlpha := 0
    else
      addAlpha := 65535;
  end;

  for yb := ABounds.Top to ABounds.Bottom-1 do
  begin
    psrc := bmp.ScanLine[yb]+ABounds.Left;
    pdest := Result.ScanLine[yb]+ABounds.Left;
    for xb := xcount-1 downto 0 do
    begin
      c := GammaExpansion(psrc^);
      Inc(psrc);
      c.red   := ((c.red - minValRed) * factorValRed + 2047) shr 12 + addValRed;
      c.green := ((c.green - minValGreen) * factorValGreen + 2047) shr 12 + addValGreen;
      c.blue  := ((c.blue - minValBlue) * factorValBlue + 2047) shr 12 + addValBlue;
      c.alpha := ((c.alpha - minAlpha) * factorAlpha + 2047) shr 12 + addAlpha;
      pdest^  := GammaCompression(c);
      Inc(pdest);
    end;
  end;
  Result.InvalidateBitmap;
end;

{ Rotates the image. To do this, loop through the destination and
  calculates the position in the source bitmap with an affine transformation }
function FilterRotate(bmp: TBGRACustomBitmap; origin: TPointF;
  angle: single; correctBlur: boolean): TBGRACustomBitmap;
var
  bounds:     TRect;
  pdest:      PBGRAPixel;
  xsrc, ysrc: single;
  savexysrc, pt: TPointF;
  dx, dy:     single;
  xb, yb:     int32or64;
  minx, miny, maxx, maxy: single;
  rf : TResampleFilter;

  function RotatePos(x, y: single): TPointF;
  var
    px, py: single;
  begin
    px     := x - origin.x;
    py     := y - origin.y;
    Result := PointF(origin.x + px * dx + py * dy, origin.y - px * dy + py * dx);
  end;

begin
  bounds := bmp.GetImageBounds;
  if (bounds.Right <= bounds.Left) or (bounds.Bottom <= Bounds.Top) then
  begin
    Result := bmp.NewBitmap(bmp.Width, bmp.Height);
    exit;
  end;

  Result := bmp.NewBitmap(bmp.Width, bmp.Height);
  if correctBlur then rf := rfHalfCosine else rf := rfLinear;

  //compute new bounding rectangle
  dx   := cos(angle * Pi / 180);
  dy   := -sin(angle * Pi / 180);
  pt   := RotatePos(bounds.left, bounds.top);
  minx := pt.x;
  miny := pt.y;
  maxx := pt.x;
  maxy := pt.y;
  pt   := RotatePos(bounds.Right - 1, bounds.top);
  if pt.x < minx then
    minx := pt.x
  else
  if pt.x > maxx then
    maxx := pt.x;
  if pt.y < miny then
    miny := pt.y
  else
  if pt.y > maxy then
    maxy := pt.y;
  pt     := RotatePos(bounds.Right - 1, bounds.bottom - 1);
  if pt.x < minx then
    minx := pt.x
  else
  if pt.x > maxx then
    maxx := pt.x;
  if pt.y < miny then
    miny := pt.y
  else
  if pt.y > maxy then
    maxy := pt.y;
  pt     := RotatePos(bounds.left, bounds.bottom - 1);
  if pt.x < minx then
    minx := pt.x
  else
  if pt.x > maxx then
    maxx := pt.x;
  if pt.y < miny then
    miny := pt.y
  else
  if pt.y > maxy then
    maxy := pt.y;

  bounds.left   := max(0, floor(minx));
  bounds.top    := max(0, floor(miny));
  bounds.right  := min(bmp.Width, ceil(maxx) + 1);
  bounds.bottom := min(bmp.Height, ceil(maxy) + 1);

  //reciproqual
  dy   := -dy;
  pt   := RotatePos(bounds.left, bounds.top);
  xsrc := pt.x;
  ysrc := pt.y;
  for yb := bounds.Top to bounds.bottom - 1 do
  begin
    pdest     := Result.scanline[yb] + bounds.left;
    savexysrc := pointf(xsrc, ysrc);
    for xb := bounds.left to bounds.right - 1 do
    begin
      pdest^ := bmp.GetPixel(xsrc, ysrc, rf);
      Inc(pdest);
      xsrc += dx;
      ysrc -= dy;
    end;
    xsrc := savexysrc.x + dy;
    ysrc := savexysrc.y + dx;
  end;
  Result.InvalidateBitmap;
end;

{ Filter grayscale applies BGRAToGrayscale function to all pixels }
procedure FilterGrayscale(bmp: TBGRACustomBitmap; ABounds: TRect; ADestination: TBGRACustomBitmap; ACheckShouldStop: TCheckShouldStopFunc);
var
  pdest, psrc: PBGRAPixel;
  xb, yb:      int32or64;

begin
  if IsRectEmpty(ABounds) then exit;

  for yb := ABounds.Top to ABounds.bottom - 1 do
  begin
    if Assigned(ACheckShouldStop) and ACheckShouldStop(yb) then break;
    pdest := ADestination.scanline[yb] + ABounds.left;
    psrc  := bmp.scanline[yb] + ABounds.left;
    for xb := ABounds.left to ABounds.right - 1 do
    begin
      pdest^ := BGRAToGrayscale(psrc^);
      Inc(pdest);
      Inc(psrc);
    end;
  end;
  ADestination.InvalidateBitmap;
end;

function FilterGrayscale(bmp: TBGRACustomBitmap): TBGRACustomBitmap;
begin
  result := FilterGrayscale(bmp, rect(0,0,bmp.width,bmp.Height));
end;

function FilterGrayscale(bmp: TBGRACustomBitmap; ABounds: TRect): TBGRACustomBitmap;
begin
  result := bmp.NewBitmap(bmp.Width,bmp.Height);
  FilterGrayscale(bmp,ABounds,result,nil);
end;

function CreateGrayscaleTask(bmp: TBGRACustomBitmap; ABounds: TRect
  ): TFilterTask;
begin
  result := TGrayscaleTask.Create(bmp,ABounds);
end;

{ Filter contour compute a grayscale image, then for each pixel
  calculates the difference with surrounding pixels (in intensity and alpha)
  and draw black pixels when there is a difference }
function FilterContour(bmp: TBGRACustomBitmap): TBGRACustomBitmap;
var
  yb, xb: int32or64;
  c:      array[0..8] of TBGRAPixel;

  i, bmpWidth, bmpHeight: int32or64;
  slope: byte;
  sum:   int32or64;
  tempPixel: TBGRAPixel;
  pdest, psrcUp, psrc, psrcDown: PBGRAPixel;

  bounds: TRect;
  gray:   TBGRACustomBitmap;
begin
  bmpWidth  := bmp.Width;
  bmpHeight := bmp.Height;
  Result    := bmp.NewBitmap(bmpWidth, bmpHeight);
  gray      := bmp.FilterGrayscale;

  bounds := rect(0, 0, bmp.Width, bmp.Height);
  for yb := bounds.Top to bounds.Bottom - 1 do
  begin
    pdest := Result.scanline[yb] + bounds.Left;

    if yb > 0 then
      psrcUp := gray.Scanline[yb - 1] + bounds.Left
    else
      psrcUp := nil;
    psrc := gray.scanline[yb] + bounds.Left;
    if yb < bmpHeight - 1 then
      psrcDown := gray.scanline[yb + 1] + bounds.Left
    else
      psrcDown := nil;

    for xb := bounds.Left to bounds.Right - 1 do
    begin
      c[0] := psrc^;
      if (xb = 0) then
      begin
        c[1] := c[0];
        c[2] := c[0];
        c[4] := c[0];
      end
      else
      begin
        if psrcUp <> nil then
          c[1] := (psrcUp - 1)^
        else
          c[1] := c[0];
        c[2] := (psrc - 1)^;
        if psrcDown <> nil then
          c[4] := (psrcDown - 1)^
        else
          c[4] := c[0];
      end;
      if psrcUp <> nil then
      begin
        c[3] := psrcUp^;
        Inc(psrcUp);
      end
      else
        c[3] := c[0];

      if (xb = bmpWidth - 1) then
      begin
        c[5] := c[0];
        c[6] := c[0];
        c[8] := c[0];
      end
      else
      begin
        if psrcDown <> nil then
          c[5] := (psrcDown + 1)^
        else
          c[5] := c[0];
        c[6] := (psrc + 1)^;
        if psrcUp <> nil then
          c[8] := psrcUp^
        else //+1 before
          c[8] := c[0];
      end;
      if psrcDown <> nil then
      begin
        c[7] := psrcDown^;
        Inc(psrcDown);
      end
      else
        c[7] := c[0];
      Inc(psrc);

      sum := 0;
      for i := 1 to 4 do
        sum += abs(c[i].red - c[i + 4].red) + abs(c[i].alpha - c[i + 4].alpha);

      if sum > 255 then
        slope := 255
      else
      if sum < 0 then
        slope := 0
      else
        slope := sum;

      tempPixel.red := 255 - slope;
      tempPixel.green := 255 - slope;
      tempPixel.blue := 255 - slope;
      tempPixel.alpha := 255;
      pdest^ := tempPixel;
      Inc(pdest);
    end;
  end;
  Result.InvalidateBitmap;
  gray.Free;
end;

{ Compute the distance for each pixel to the center of the bitmap,
  calculate the corresponding angle with arcsin, use this angle
  to determine a distance from the center in the source bitmap }
function FilterSphere(bmp: TBGRACustomBitmap): TBGRACustomBitmap;
var
  cx, cy, x, y, len, fact: single;
  xb, yb: int32or64;
  mask:   TBGRACustomBitmap;
begin
  Result := bmp.NewBitmap(bmp.Width, bmp.Height);
  cx     := bmp.Width / 2 - 0.5;
  cy     := bmp.Height / 2 - 0.5;
  for yb := 0 to Result.Height - 1 do
    for xb := 0 to Result.Width - 1 do
    begin
      x   := (xb - cx) / (cx + 0.5);
      y   := (yb - cy) / (cy + 0.5);
      len := sqrt(sqr(x) + sqr(y));
      if (len <= 1) then
      begin
        if (len > 0) then
        begin
          fact := 1 / len * arcsin(len) / (Pi / 2);
          x    *= fact;
          y    *= fact;
        end;
        Result.setpixel(xb, yb, bmp.Getpixel(x * cx + cx, y * cy + cy));
      end;
    end;
  mask := bmp.NewBitmap(bmp.Width, bmp.Height);
  Mask.Fill(BGRABlack);
  Mask.FillEllipseAntialias(cx, cy, cx, cy, BGRAWhite);
  Result.ApplyMask(mask);
  Mask.Free;
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
  cx, cy, x, y, len, fact: single;
  xb, yb: int32or64;
begin
  Result := bmp.NewBitmap(bmp.Width, bmp.Height);
  cx     := bmp.Width / 2 - 0.5;
  cy     := bmp.Height / 2 - 0.5;
  for yb := 0 to Result.Height - 1 do
    for xb := 0 to Result.Width - 1 do
    begin
      x   := (xb - cx) / (cx + 0.5);
      y   := (yb - cy) / (cy + 0.5);
      len := abs(x);
      if (len <= 1) then
      begin
        if (len > 0) then
        begin
          fact := 1 / len * arcsin(len) / (Pi / 2);
          x    *= fact;
        end;
        Result.setpixel(xb, yb, bmp.Getpixel(x * cx + cx, y * cy + cy));
      end;
    end;
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

constructor TGrayscaleTask.Create(bmp: TBGRACustomBitmap; ABounds: TRect);
begin
  FSource := bmp;
  FBounds := ABounds;
end;

procedure TGrayscaleTask.DoExecute;
begin
  FilterGrayscale(FSource,FBounds,Destination,@GetShouldStop);
end;

{ TCustomBlurTask }

constructor TCustomBlurTask.Create(bmp: TBGRACustomBitmap; ABounds: TRect;
  AMask: TBGRACustomBitmap; AMaskIsThreadSafe: boolean);
begin
  FSource := bmp;
  FBounds := ABounds;
  if AMaskIsThreadSafe then
  begin
    FMask := AMask;
    FMaskOwned := false;
  end else
  begin
    FMask := AMask.Duplicate;
    FMaskOwned := true;
  end;
end;

destructor TCustomBlurTask.Destroy;
begin
  If FMaskOwned then FreeAndNil(FMask);
  inherited Destroy;
end;

procedure TCustomBlurTask.DoExecute;
begin
  FilterBlur(FSource,FBounds,FMask,Destination,@GetShouldStop);
end;

constructor TMotionBlurTask.Create(ABmp: TBGRACustomBitmap; ABounds: TRect;
  ADistance, AAngle: single; AOriented: boolean);
begin
  FSource := ABmp;
  FBounds := ABounds;
  FDistance := ADistance;
  FAngle := AAngle;
  FOriented:= AOriented;
end;

procedure TMotionBlurTask.DoExecute;
begin
  FilterBlurMotion(FSource,FBounds,FDistance,FAngle,FOriented,Destination,@GetShouldStop);
end;

constructor TRadialPreciseBlurTask.Create(bmp: TBGRACustomBitmap;
  ABounds: TRect; radius: single);
begin
  FSource := bmp;
  FBounds := ABounds;
  FRadius := radius;
end;

procedure TRadialPreciseBlurTask.DoExecute;
begin
  FilterBlurRadialPrecise(FSource,FBounds,FRadius,Destination,@GetShouldStop);
end;

{ TRadialBlurTask }

constructor TRadialBlurTask.Create(bmp: TBGRACustomBitmap; ABounds: TRect;
  radius: integer; blurType: TRadialBlurType);
begin
  FSource := bmp;
  FBounds := ABounds;
  FRadius := radius;
  FBlurType:= blurType;
end;

procedure TRadialBlurTask.DoExecute;
begin
  FilterBlurRadial(FSource,FBounds,FRadius,FBlurType,Destination,@GetShouldStop);
end;

{ TFilterTask }

function TFilterTask.GetShouldStop(ACurrentY: integer): boolean;
begin
  FCurrentY:= ACurrentY;
  if Assigned(FCheckShouldStop) then
    result := FCheckShouldStop(ACurrentY)
  else
    result := false;
end;

function TFilterTask.Execute: TBGRACustomBitmap;
var DestinationOwned: boolean;
begin
  FCurrentY := 0;
  if Destination = nil then
  begin
    FDestination := FSource.NewBitmap(FSource.Width,FSource.Height);
    DestinationOwned:= true;
  end else
    DestinationOwned:= false;
  try
    DoExecute;
    result := Destination;
    FDestination := nil;
  except
    on ex: exception do
    begin
      if DestinationOwned then FreeAndNil(FDestination);
      raise ex;
    end;
  end;
end;

procedure TFilterTask.SetDestination(AValue: TBGRACustomBitmap);
begin
  if FDestination <> nil then
    raise exception.Create('Destination is already defined');
  FDestination := AValue;
end;

end.

