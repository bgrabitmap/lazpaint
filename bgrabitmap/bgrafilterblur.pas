unit BGRAFilterBlur;

{$mode objfpc}{$H+}

interface

uses
  Classes, BGRABitmapTypes, BGRAFilterType;

type
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

  { TRadialBlurTask }

  TRadialBlurTask = class(TFilterTask)
  private
    FBounds: TRect;
    FRadiusX,FRadiusY: single;
    FBlurType: TRadialBlurType;
  public
    constructor Create(bmp: TBGRACustomBitmap; ABounds: TRect; radius: single;
                       blurType: TRadialBlurType);
    constructor Create(bmp: TBGRACustomBitmap; ABounds: TRect; radiusX,radiusY: single;
                       blurType: TRadialBlurType);
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

implementation

uses Types, Math, SysUtils;

procedure FilterBlur(bmp: TBGRACustomBitmap; ABounds: TRect;
   blurMask: TBGRACustomBitmap; ADestination: TBGRACustomBitmap; ACheckShouldStop: TCheckShouldStopFunc); forward;
procedure FilterBlurMotion(bmp: TBGRACustomBitmap; ABounds: TRect; distance: single;
  angle: single; oriented: boolean; ADestination: TBGRACustomBitmap; ACheckShouldStop: TCheckShouldStopFunc); forward;
procedure FilterBlurRadial(bmp: TBGRACustomBitmap; ABounds: TRect; radiusX,radiusY: single;
  blurType: TRadialBlurType; ADestination: TBGRACustomBitmap; ACheckShouldStop: TCheckShouldStopFunc); forward;

type
  { TBoxBlurTask }

  TBoxBlurTask = class(TFilterTask)
  private
    FBounds: TRect;
    FRadiusX,FRadiusY: single;
  public
    constructor Create(bmp: TBGRACustomBitmap; ABounds: TRect; radius: single);
    constructor Create(bmp: TBGRACustomBitmap; ABounds: TRect; radiusX,radiusY: single);
  protected
    {$IFNDEF CPU64}
    procedure DoExecuteNormal;
    {$ENDIF}
    procedure DoExecute64;
    procedure DoExecute; override;
  end;

{ TCustomBlurTask }

constructor TCustomBlurTask.Create(bmp: TBGRACustomBitmap; ABounds: TRect;
  AMask: TBGRACustomBitmap; AMaskIsThreadSafe: boolean);
begin
  SetSource(bmp);
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

{ TMotionBlurTask }

constructor TMotionBlurTask.Create(ABmp: TBGRACustomBitmap; ABounds: TRect;
  ADistance, AAngle: single; AOriented: boolean);
begin
  SetSource(ABmp);
  FBounds := ABounds;
  FDistance := ADistance;
  FAngle := AAngle;
  FOriented:= AOriented;
end;

procedure TMotionBlurTask.DoExecute;
begin
  FilterBlurMotion(FSource,FBounds,FDistance,FAngle,FOriented,Destination,@GetShouldStop);
end;

{ TRadialBlurTask }

constructor TRadialBlurTask.Create(bmp: TBGRACustomBitmap; ABounds: TRect;
  radius: single; blurType: TRadialBlurType);
begin
  SetSource(bmp);
  FBounds := ABounds;
  FRadiusX := radius;
  FRadiusY := radius;
  FBlurType:= blurType;
end;

constructor TRadialBlurTask.Create(bmp: TBGRACustomBitmap; ABounds: TRect;
  radiusX, radiusY: single; blurType: TRadialBlurType);
begin
  SetSource(bmp);
  FBounds := ABounds;
  FRadiusX := radiusX;
  FRadiusY := radiusY;
  FBlurType:= blurType;
end;

procedure TRadialBlurTask.DoExecute;
begin
  FilterBlurRadial(FSource,FBounds,FRadiusX,FRadiusY,FBlurType,Destination,@GetShouldStop);
end;

function FilterBlurBox(bmp: TBGRACustomBitmap; ABounds: TRect; radiusX,radiusY: single;
         ADestination: TBGRACustomBitmap; ACheckShouldStop: TCheckShouldStopFunc=nil): TBGRACustomBitmap;
var task: TBoxBlurTask;
begin
  task := TBoxBlurTask.Create(bmp, ABounds, radiusX,radiusY);
  task.CheckShouldStop := ACheckShouldStop;
  task.Destination := ADestination;
  result := task.Execute;
  task.Free;
end;

{ This is a clever solution for fast computing of the blur
  effect : it stores an array of vertical sums forming a square
  around the pixel which moves with it. For each new pixel,
  the vertical sums are kept except for the last column of
  the square }
procedure FilterBlurFast(bmp: TBGRACustomBitmap; ABounds: TRect;
  radiusX,radiusY: single; ADestination: TBGRACustomBitmap; ACheckShouldStop: TCheckShouldStopFunc);
 {$IFDEF CPU64}{$DEFINE FASTBLUR_DOUBLE}{$ENDIF}
  type
    PRowSum = ^TRowSum;
    TRowSum = record
      sumR,sumG,sumB,rgbDiv,sumA,aDiv: NativeUInt;
    end;
    TExtendedRowValue = {$IFDEF FASTBLUR_DOUBLE}double{$ELSE}uint64{$ENDIF};
    TExtendedRowSum = record
      sumR,sumG,sumB,rgbDiv,sumA,aDiv: TExtendedRowValue;
    end;

  function ComputeExtendedAverage(const sum: TExtendedRowSum): TBGRAPixel; inline;
  {$IFDEF FASTBLUR_DOUBLE}
  var v: uint32or64;
  {$ELSE}
  var rgbDivShr1: TExtendedRowValue;
  {$ENDIF}
  begin
    {$IFDEF FASTBLUR_DOUBLE}
    v := round(sum.sumA/sum.aDiv);
    if v > 255 then result.alpha := 255 else result.alpha := v;
    v := round(sum.sumR/sum.rgbDiv);
    if v > 255 then result.red := 255 else result.red := v;
    v := round(sum.sumG/sum.rgbDiv);
    if v > 255 then result.green := 255 else result.green := v;
    v := round(sum.sumB/sum.rgbDiv);
    if v > 255 then result.blue := 255 else result.blue := v;
    {$ELSE}
    rgbDivShr1:= sum.rgbDiv shr 1;
    DWord(result) := (((sum.sumA+sum.aDiv shr 1) div sum.aDiv) shl TBGRAPixel_AlphaShift)
    or (((sum.sumR+rgbDivShr1) div sum.rgbDiv) shl TBGRAPixel_RedShift)
    or (((sum.sumG+rgbDivShr1) div sum.rgbDiv) shl TBGRAPixel_GreenShift)
    or (((sum.sumB+rgbDivShr1) div sum.rgbDiv) shl TBGRAPixel_BlueShift);
    {$ENDIF}
  end;

  function ComputeClampedAverage(const sum: TRowSum): TBGRAPixel;
  var v: UInt32or64;
  begin
    v := (sum.sumA+sum.aDiv shr 1) div sum.aDiv;
    if v > 255 then result.alpha := 255 else result.alpha := v;
    v := (sum.sumR+sum.rgbDiv shr 1) div sum.rgbDiv;
    if v > 255 then result.red := 255 else result.red := v;
    v := (sum.sumG+sum.rgbDiv shr 1) div sum.rgbDiv;
    if v > 255 then result.green := 255 else result.green := v;
    v := (sum.sumB+sum.rgbDiv shr 1) div sum.rgbDiv;
    if v > 255 then result.blue := 255 else result.blue := v;
  end;

  function ComputeAverage(const sum: TRowSum): TBGRAPixel; inline;
  var rgbDivShr1: NativeUInt;
  begin
    rgbDivShr1:= sum.rgbDiv shr 1;
    DWord(result) := (((sum.sumA+sum.aDiv shr 1) div sum.aDiv) shl TBGRAPixel_AlphaShift)
    or (((sum.sumR+rgbDivShr1) div sum.rgbDiv) shl TBGRAPixel_RedShift)
    or (((sum.sumG+rgbDivShr1) div sum.rgbDiv) shl TBGRAPixel_GreenShift)
    or (((sum.sumB+rgbDivShr1) div sum.rgbDiv) shl TBGRAPixel_BlueShift);
  end;

  {$I blurfast.inc}

{ Normal radial blur compute a blur mask with a GradientFill and
  then posterize to optimize general purpose blur }
procedure FilterBlurRadialNormal(bmp: TBGRACustomBitmap;
  ABounds: TRect; radiusX,radiusY: single; ADestination: TBGRACustomBitmap; ACheckShouldStop: TCheckShouldStopFunc);
var
  blurShape: TBGRACustomBitmap;
  n: Int32or64;
  p: PBGRAPixel;
  maxRadius: single;
  temp: TBGRACustomBitmap;
begin
  if (radiusX <= 0) and (radiusY <= 0) then
  begin
    ADestination.PutImagePart(ABounds.Left,ABounds.Top,bmp,ABounds,dmSet);
    exit;
  end;
  maxRadius:= max(radiusX,radiusY);
  blurShape := bmp.NewBitmap(2 * ceil(maxRadius) + 1, 2 * ceil(maxRadius) + 1);
  blurShape.GradientFill(0, 0, blurShape.Width, blurShape.Height, BGRAWhite,
    BGRABlack, gtRadial, pointF(ceil(maxRadius), ceil(maxRadius)), pointF(ceil(maxRadius)-maxRadius-0.5, ceil(maxRadius)), dmSet);
  if (ceil(radiusX)<>ceil(radiusY)) then
  begin
    temp := blurShape.Resample(2 * ceil(radiusX) + 1, 2 * ceil(radiusY) + 1);
    blurShape.Free;
    blurShape := temp;
    temp := nil;
  end;
  if (radiusX > 10) or (radiusY > 10) then
  begin
    p := blurShape.Data;
    for n := 0 to blurShape.NbPixels-1 do
    begin
      p^.red := p^.red and $F0;
      p^.green := p^.red;
      p^.blue := p^.red;
      inc(p);
    end;
  end;
  FilterBlur(bmp, ABounds, blurShape, ADestination, ACheckShouldStop);
  blurShape.Free;
end;

{ Blur disk creates a disk mask with a FillEllipse }
procedure FilterBlurDisk(bmp: TBGRACustomBitmap; ABounds: TRect; radiusX,radiusY: single; ADestination: TBGRACustomBitmap; ACheckShouldStop: TCheckShouldStopFunc);
var
  blurShape: TBGRACustomBitmap;
begin
  if (radiusX <= 0) and (radiusY <= 0) then
  begin
    ADestination.PutImagePart(ABounds.Left,ABounds.Top,bmp,ABounds,dmSet);
    exit;
  end;
  blurShape := bmp.NewBitmap(2 * ceil(radiusX) + 1, 2 * ceil(radiusY) + 1);
  blurShape.Fill(BGRABlack);
  blurShape.FillEllipseAntialias(ceil(radiusX), ceil(radiusY), radiusX + 0.5, radiusY + 0.5, BGRAWhite);
  FilterBlur(bmp, ABounds, blurShape, ADestination, ACheckShouldStop);
  blurShape.Free;
end;

{ Corona blur use a circle as mask }
procedure FilterBlurCorona(bmp: TBGRACustomBitmap; ABounds: TRect; radiusX,radiusY: single; ADestination: TBGRACustomBitmap; ACheckShouldStop: TCheckShouldStopFunc);
var
  blurShape: TBGRACustomBitmap;
begin
  if (radiusX <= 0) and (radiusY <= 0) then
  begin
    ADestination.PutImagePart(ABounds.Left,ABounds.Top,bmp,ABounds,dmSet);
    exit;
  end;
  blurShape := bmp.NewBitmap(2 * ceil(radiusX) + 1, 2 * ceil(radiusY) + 1);
  blurShape.Fill(BGRABlack);
  blurShape.EllipseAntialias(ceil(radiusX), ceil(radiusY), radiusX, radiusY, BGRAWhite, 1);
  FilterBlur(bmp, ABounds, blurShape, ADestination, ACheckShouldStop);
  blurShape.Free;
end;

procedure FilterBlurRadial(bmp: TBGRACustomBitmap; ABounds: TRect; radius: single;
  blurType: TRadialBlurType; ADestination: TBGRACustomBitmap; ACheckShouldStop: TCheckShouldStopFunc);
begin
  if radius = 0 then
  begin
    ADestination.PutImagePart(ABounds.Left,ABounds.Top,bmp,ABounds,dmSet);
    exit;
  end;
  case blurType of
    rbCorona:  FilterBlurCorona(bmp, ABounds, radius,radius, ADestination, ACheckShouldStop);
    rbDisk:    FilterBlurDisk(bmp, ABounds, radius,radius, ADestination, ACheckShouldStop);
    rbNormal:  FilterBlurRadialNormal(bmp, ABounds, radius,radius, ADestination, ACheckShouldStop);
    rbFast:    FilterBlurFast(bmp, ABounds, radius,radius, ADestination, ACheckShouldStop);
    rbPrecise: FilterBlurRadialNormal(bmp, ABounds, radius / 10 + 0.5, radius / 10 + 0.5, ADestination, ACheckShouldStop);
    rbBox:     FilterBlurBox(bmp, ABounds, radius,radius, ADestination, ACheckShouldStop);
  end;
end;

procedure FilterBlurRadial(bmp: TBGRACustomBitmap; ABounds: TRect; radiusX,radiusY: single;
  blurType: TRadialBlurType; ADestination: TBGRACustomBitmap; ACheckShouldStop: TCheckShouldStopFunc);
begin
  if (radiusX <= 0) and (radiusY <= 0) then
  begin
    ADestination.PutImagePart(ABounds.Left,ABounds.Top,bmp,ABounds,dmSet);
    exit;
  end;
  if radiusX < 0 then radiusX := 0;
  if radiusY < 0 then radiusY := 0;
  case blurType of
    rbCorona:  FilterBlurCorona(bmp, ABounds, radiusX,radiusY, ADestination, ACheckShouldStop);
    rbDisk:    FilterBlurDisk(bmp, ABounds, radiusX,radiusY, ADestination, ACheckShouldStop);
    rbNormal:  FilterBlurRadialNormal(bmp, ABounds, radiusX,radiusY, ADestination, ACheckShouldStop);
    rbFast:    FilterBlurFast(bmp, ABounds, radiusX,radiusY, ADestination, ACheckShouldStop);
    rbPrecise: FilterBlurRadialNormal(bmp, ABounds, radiusX / 10 + 0.5, radiusY/10 + 0.5, ADestination, ACheckShouldStop);
    rbBox:     FilterBlurBox(bmp, ABounds, radiusX,radiusY, ADestination, ACheckShouldStop);
  end;
end;

function FilterBlurRadial(bmp: TBGRACustomBitmap; radius: single;
  blurType: TRadialBlurType): TBGRACustomBitmap;
begin
  if blurType = rbBox then
  begin
    result := FilterBlurBox(bmp,rect(0,0,bmp.Width,bmp.Height),radius,radius,nil);
  end else
  begin
    result := bmp.NewBitmap(bmp.width,bmp.Height);
    FilterBlurRadial(bmp, rect(0,0,bmp.Width,bmp.height), radius, blurType,result,nil);
  end;
end;

function FilterBlurRadial(bmp: TBGRACustomBitmap; radiusX: single;
  radiusY: single; blurType: TRadialBlurType): TBGRACustomBitmap;
begin
  if blurType = rbBox then
  begin
    result := FilterBlurBox(bmp,rect(0,0,bmp.Width,bmp.Height),radiusX,radiusY,nil);
  end else
  begin
    result := bmp.NewBitmap(bmp.width,bmp.Height);
    FilterBlurRadial(bmp, rect(0,0,bmp.Width,bmp.height), radiusX,radiusY, blurType,result,nil);
  end;
end;

function CreateRadialBlurTask(ABmp: TBGRACustomBitmap; ABounds: TRect; ARadius: single;
  ABlurType: TRadialBlurType): TFilterTask;
begin
  if ABlurType = rbBox then
    result := TBoxBlurTask.Create(ABmp,ABounds,ARadius)
  else
    result := TRadialBlurTask.Create(ABmp,ABounds,ARadius,ABlurType);
end;

function CreateRadialBlurTask(ABmp: TBGRACustomBitmap; ABounds: TRect;
  ARadiusX, ARadiusY: single; ABlurType: TRadialBlurType): TFilterTask;
begin
  if ABlurType = rbBox then
    result := TBoxBlurTask.Create(ABmp,ABounds,ARadiusX,ARadiusY)
  else
    result := TRadialBlurTask.Create(ABmp,ABounds,ARadiusX,ARadiusY,ABlurType);
end;

{ This filter draws an antialiased line to make the mask, and
  if the motion blur is oriented, does a GradientFill to orient it }
procedure FilterBlurMotion(bmp: TBGRACustomBitmap; ABounds: TRect; distance: single;
  angle: single; oriented: boolean; ADestination: TBGRACustomBitmap; ACheckShouldStop: TCheckShouldStopFunc);
var
  blurShape: TBGRACustomBitmap;
  intRadius: integer;
  dx, dy, r: single;
begin
  if distance < 1e-6 then
  begin
    ADestination.PutImagePart(ABounds.Left,ABounds.Top,bmp,ABounds,dmSet);
    exit;
  end;
  dx := cos(angle * Pi / 180);
  dy := sin(angle * Pi / 180);
  if not oriented and (abs(dx)<1e-6) then
    FilterBlurBox(bmp, ABounds,0,distance/2, ADestination, ACheckShouldStop)
  else if not oriented and (abs(dy)<1e-6) then
    FilterBlurBox(bmp, ABounds,distance/2,0, ADestination, ACheckShouldStop)
  else
  begin
    r  := distance / 2;
    intRadius := ceil(r);
    blurShape := bmp.NewBitmap(2 * intRadius + 1, 2 * intRadius + 1);

    blurShape.Fill(BGRABlack);
    blurShape.DrawLineAntialias(intRadius - dx * r, intRadius - dy *
      r, intRadius + dx * r, intRadius + dy * r, BGRAWhite, 1, True);
    if oriented then
      blurShape.GradientFill(0, 0, blurShape.Width, blurShape.Height,
        BGRAPixelTransparent, BGRABlack, gtRadial, pointF(intRadius -
        dx * r, intRadius - dy * r),
        pointF(intRadius + dx * (r + 0.5), intRadius + dy * (r + 0.5)),
        dmFastBlend, False);
    FilterBlur(bmp, ABounds, blurShape, ADestination, ACheckShouldStop);
    blurShape.Free;
  end;
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
function clampByte(value: NativeInt): NativeUInt; inline;
begin
  if value <= 0 then result := 0 else
  if value >= 255 then result := 255 else
    result := value;
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
    sumR, sumG, sumB, sumA, Adiv, RGBdiv : NativeInt;

  function ComputeAverage: TBGRAPixel; inline;
  var temp,rgbDivShr1: NativeInt;
  begin
    temp := sumA + Adiv shr 1;
    if temp < Adiv then
      result := BGRAPixelTransparent
    else
    begin
      rgbDivShr1 := RGBdiv shr 1;
      result.alpha := temp div Adiv;
      result.red   := clampByte((sumR + rgbDivShr1) div RGBdiv);
      result.green := clampByte((sumG + rgbDivShr1) div RGBdiv);
      result.blue  := clampByte((sumB + rgbDivShr1) div RGBdiv);
    end;
  end;

  {$define PARAM_MASKSHIFT}
  {$I blurnormal.inc}

//32-bit blur
procedure FilterBlurSmallMask(bmp: TBGRACustomBitmap;
  blurMask: TBGRACustomBitmap; ABounds: TRect; ADestination: TBGRACustomBitmap; ACheckShouldStop: TCheckShouldStopFunc);

  var
    sumR, sumG, sumB, sumA, Adiv : NativeInt;

  function ComputeAverage: TBGRAPixel; inline;
  var temp,sumAShr1: NativeInt;
  begin
    temp := sumA + Adiv shr 1;
    if temp < Adiv then
      result := BGRAPixelTransparent
    else
    begin
      sumAShr1 := sumA shr 1;
      result.alpha := temp div Adiv;
      result.red   := clampByte((sumR + sumAShr1) div sumA);
      result.green := clampByte((sumG + sumAShr1) div sumA);
      result.blue  := clampByte((sumB + sumAShr1) div sumA);
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

constructor TBoxBlurTask.Create(bmp: TBGRACustomBitmap; ABounds: TRect;
  radius: single);
begin
  SetSource(bmp);
  FBounds := ABounds;
  FRadiusX := radius;
  FRadiusY := radius;
end;

constructor TBoxBlurTask.Create(bmp: TBGRACustomBitmap; ABounds: TRect;
  radiusX, radiusY: single);
begin
  SetSource(bmp);
  FBounds := ABounds;
  FRadiusX := max(radiusX,0);
  FRadiusY := max(radiusY,0);
end;

procedure TBoxBlurTask.DoExecute64;
const
  factMainX = 16;
  factMainY = 16;
type
  TAccumulator = UInt64;
{$i blurbox.inc}

{$IFNDEF CPU64}
procedure TBoxBlurTask.DoExecuteNormal;
const
  factMainX = 16;
  factMainY = 16;
type
  TAccumulator = NativeUInt;
{$i blurbox.inc}
{$ENDIF}

procedure TBoxBlurTask.DoExecute;
{$IFDEF CPU64}
begin
  DoExecute64;
end;
{$ELSE}
const
    factMainX = 16;
    factMainY = 16;
var totalSum: UInt64;
  factExtraX,factExtraY: NativeUInt;
begin
  totalSum := (2*ceil(FRadiusX)+1)*(2*ceil(FRadiusY)+1);
  factExtraX := trunc(frac(FRadiusX+0.5/factMainX)*factMainX);
  factExtraY := trunc(frac(FRadiusY+0.5/factMainY)*factMainY);
  if factExtraX > 0 then totalSum *= factMainX;
  if factExtraY > 0 then totalSum *= factMainY;
  totalSum *= 256*256;
  if totalSum > high(NativeUInt) then
    DoExecute64
  else
    DoExecuteNormal;
end;
{$ENDIF}

end.

