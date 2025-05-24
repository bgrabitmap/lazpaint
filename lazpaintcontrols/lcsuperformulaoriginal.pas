unit LCSuperformulaOriginal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRALayerOriginal, BGRABitmap, BGRABitmapTypes, Math,
  Types;

const
    MaxDenominator = 20;

type
  { TSuperformulaOriginal }

  TSuperformulaOriginal = class(TBGRALayerCustomOriginal)
  private
    FSpikeOverlap: boolean;
    Fa: double;
    Fb: double;
    FBackColor: TBGRAPixel;
    FPenColor: TBGRAPixel;
    FLineWidth: double;
    Fm: double;
    FMRational: boolean;
    FMultiplier: double;
    Fn1: double;
    Fn2: double;
    Fn3: double;
    FDiff: TBGRAOriginalStorageDiff;
    FUpdateCount: integer;
    function FloatToFraction(ARatio: single; out num, denom: integer; AMaxDenominator: integer): string;
    function GetRadius: double;
    function GetSize: double;
    function GetSizeWithoutMultiplier: double;
    procedure SetA(AValue: double);
    procedure SetB(AValue: double);
    procedure SetBackColor(AValue: TBGRAPixel);
    procedure SetPenColor(AValue: TBGRAPixel);
    procedure SetLineWidth(AValue: double);
    procedure SetM(AValue: double);
    procedure SetMultiplier(AValue: double);
    procedure SetN1(AValue: double);
    procedure SetN2(AValue: double);
    procedure SetN3(AValue: double);
    procedure SetMRational(AValue: boolean);
    procedure SetSize(AValue: double);
    procedure SetSpikeOverlap(AValue: boolean);
  protected
    procedure GetCurve(AMatrix: TAffineMatrix; out ABackPoints: ArrayOfTPointF;
      out APenOutlinePoints: ArrayOfTPointF);
    function PenVisible: boolean;
    function BackVisible: boolean;
    procedure BeginUpdate;
    procedure EndUpdate;
  public
    constructor Create; override;
    procedure Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix;
      ADraft: boolean); override;
    function GetRenderBounds(ADestRect: TRect; {%H-}AMatrix: TAffineMatrix): TRect; override;
    procedure GetMFraction(out ANumerator, ADenominator: integer);
    procedure LoadFromStorage(AStorage: TBGRACustomOriginalStorage); override;
    procedure SaveToStorage(AStorage: TBGRACustomOriginalStorage); override;
    class function StorageClassName: RawByteString; override;
    property SpikeOverlap: boolean read FSpikeOverlap write SetSpikeOverlap;
    property a: double read Fa write SetA;
    property b: double read Fb write SetB;
    property m: double read Fm write SetM;
    property mRational: boolean read FMRational write SetMRational;
    property n1: double read Fn1 write SetN1;
    property n2: double read Fn2 write SetN2;
    property n3: double read Fn3 write SetN3;
    property Radius: double read GetRadius;
    property LineWidth: double read FLineWidth write SetLineWidth;
    property PenColor: TBGRAPixel read FPenColor write SetPenColor;
    property BackColor: TBGRAPixel read FBackColor write SetBackColor;
    property Multiplier: double read FMultiplier write SetMultiplier;
    property Size: double read GetSize write SetSize;
  end;

implementation

uses BGRATransform, BGRAPen, BGRAGraphics;

var
  MAX_LOG: double = 0.0;
  MIN_LOG: double = 0.0;
  MAX_R: double = 100.0;

function SafePower(a, b: double; out c: double): boolean;
var
  tmp: double;
begin
  if a < 0 then
  begin
    result := SafePower(-a, b, c);
    c := -c;
    exit;
  end;

  Result := True;
  if a = 0 then
  begin
    if b = 0 then
      c := 1
    else
      c := 0;
    exit;
  end;

  if MAX_LOG = 0.0 then
    MAX_LOG := ln(MaxDouble);
  if MIN_LOG = 0.0 then
    MIN_LOG := ln(MinDouble);

  // ln(a^b) = b ln(a)
  tmp := b * ln(a);
  if tmp > MAX_LOG then
    Result := False
  else
  if tmp < MIN_LOG then
    c := 0.0
  else
    c := exp(tmp);
end;

function ComputeR(theta, a, b, m, n1, n2, n3: double): double;
const
  EPS = 1E-9;
var
  c, pc, s, ps: double;
begin
  if (a = 0) or (b = 0) or (m = 0) or (n1 = 0) or (n2 = 0) or (n3 = 0) then
    exit(0);

  c := abs(cos(m * theta / 4) / a);
  if c < EPS then
    pc := 0
  else
  if not SafePower(c, n2, pc) then
  begin
    Result := MAX_R;
    exit;
  end;

  s := abs(sin(m * theta / 4) / b);
  if s < EPS then
    ps := 0
  else
  if not SafePower(s, n3, ps) then
  begin
    Result := MAX_R;
    exit;
  end;

  if pc + ps < EPS then
    Result := 0
  else
  if not SafePower(pc + ps, -1 / n1, Result) then
    Result := MAX_R;

  if Result > MAX_R then
    Result := MAX_R;
end;

{ TSuperformulaOriginal }

procedure TSuperformulaOriginal.SetA(AValue: double);
begin
  if Fa = AValue then
    Exit;
  BeginUpdate;
  Fa := AValue;
  EndUpdate;
end;

procedure TSuperformulaOriginal.SetB(AValue: double);
begin
  if Fb = AValue then
    Exit;
  BeginUpdate;
  Fb := AValue;
  EndUpdate;
end;

procedure TSuperformulaOriginal.SetBackColor(AValue: TBGRAPixel);
begin
  if FBackColor = AValue then
    Exit;
  BeginUpdate;
  FBackColor := AValue;
  EndUpdate;
end;

procedure TSuperformulaOriginal.SetPenColor(AValue: TBGRAPixel);
begin
  if FPenColor = AValue then
    Exit;
  BeginUpdate;
  FPenColor := AValue;
  EndUpdate;
end;

procedure TSuperformulaOriginal.SetLineWidth(AValue: double);
begin
  if FLineWidth = AValue then
    Exit;
  BeginUpdate;
  FLineWidth := AValue;
  EndUpdate;
end;

procedure TSuperformulaOriginal.SetM(AValue: double);
begin
  if Fm = AValue then
    Exit;
  BeginUpdate;
  Fm := AValue;
  EndUpdate;
end;

procedure TSuperformulaOriginal.SetMultiplier(AValue: double);
begin
  if FMultiplier = AValue then
    Exit;
  BeginUpdate;
  FMultiplier := AValue;
  EndUpdate;
end;

procedure TSuperformulaOriginal.SetMRational(AValue: boolean);
begin
  if FMRational=AValue then Exit;
  BeginUpdate;
  FMRational:=AValue;
  EndUpdate;
end;

procedure TSuperformulaOriginal.SetSize(AValue: double);
var
  curSizeWithoutMultiplier: Double;
begin
  curSizeWithoutMultiplier := GetSizeWithoutMultiplier;
  if curSizeWithoutMultiplier = 0 then exit;
  Multiplier:= AValue/curSizeWithoutMultiplier;
end;

procedure TSuperformulaOriginal.SetN1(AValue: double);
begin
  if Fn1 = AValue then
    Exit;
  BeginUpdate;
  Fn1 := AValue;
  EndUpdate;
end;

procedure TSuperformulaOriginal.SetN2(AValue: double);
begin
  if Fn2 = AValue then
    Exit;
  BeginUpdate;
  Fn2 := AValue;
  EndUpdate;
end;

procedure TSuperformulaOriginal.SetN3(AValue: double);
begin
  if Fn3 = AValue then
    Exit;
  BeginUpdate;
  Fn3 := AValue;
  EndUpdate;
end;

procedure TSuperformulaOriginal.SetSpikeOverlap(AValue: boolean);
begin
  if FSpikeOverlap=AValue then Exit;
  BeginUpdate;
  FSpikeOverlap:=AValue;
  EndUpdate;
end;

procedure TSuperformulaOriginal.GetCurve(AMatrix: TAffineMatrix;
  out ABackPoints: ArrayOfTPointF; out APenOutlinePoints: ArrayOfTPointF);
var
  i, num, denom, precision, turns: integer;
  r, theta, usedM, approxM, correction: double;
  stroker: TBGRACustomPenStroker;
begin
  ABackPoints := nil;
  APenOutlinePoints := nil;
  FloatToFraction(m, num, denom, MaxDenominator);
  approxM := num/denom;
  precision := max(num * 100, 100 * 3);
  if precision > 3000 then
    precision := (3000 div num)*num;
  if mRational then
  begin
    usedM := approxM;
    correction := 1;
  end else
  begin
    usedM:= m;
    correction := approxM / m;
  end;
  turns := denom * (1 + integer(SpikeOverlap and odd(num) and ((a <> b) or (n2 <> n3))));
  SetLength(ABackPoints, precision * turns);
  for i := 0 to precision * turns - 1 do
  begin
    theta := i * 2 * Pi * correction / precision;
    r := ComputeR(theta, a, b, usedM, n1, n2, n3) * multiplier;
    ABackPoints[i] := AMatrix * PointF(r * cos(theta), r * sin(theta));
  end;
  if PenVisible then
  begin
    stroker := TBGRAPenStroker.Create;
    try
      stroker.StrokeMatrix := AMatrix;
      stroker.JoinStyle := pjsMiter;
      APenOutlinePoints := stroker.ComputePolygon(ABackPoints, LineWidth);
    finally
      stroker.Free;
    end;
  end;
  if not BackVisible then
     ABackPoints := nil;
end;

function TSuperformulaOriginal.PenVisible: boolean;
begin
  result := (LineWidth > 0) and (PenColor.alpha > 0);
end;

function TSuperformulaOriginal.BackVisible: boolean;
begin
  result := BackColor.alpha > 0;
end;

procedure TSuperformulaOriginal.BeginUpdate;
begin
  if FUpdateCount = 0 then
  begin
    FDiff := TBGRAOriginalStorageDiff.Create(self);
  end;
  Inc(FUpdateCount);
end;

procedure TSuperformulaOriginal.EndUpdate;
begin
  if FUpdateCount = 0 then exit;
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
  begin
    if Assigned(FDiff) then
       FDiff.ComputeDifference(self);
    NotifyChange(FDiff);
    FDiff := nil;
  end;
end;

constructor TSuperformulaOriginal.Create;
begin
  inherited Create;
  FSpikeOverlap:= true;
  Fa := 1;
  Fb := 1;
  Fm := 24;
  Fn1 := 2;
  Fn2 := 1;
  Fn3 := 2;
  FLineWidth := 0;
  FPenColor := BGRA($00, $40, $80);
  FBackColor := BGRA($00, $80, $C0);
  FMultiplier := 200;
  FMRational:= true;
  FUpdateCount:= 0;
  FDiff := nil;
end;

function TSuperformulaOriginal.FloatToFraction(ARatio: single; out num,
  denom: integer; AMaxDenominator: integer): string;

  procedure InvFrac;
  var temp: integer;
  begin
    temp := num;
    num := denom;
    denom := temp;
  end;

  procedure AddFrac(AValue: integer);
  begin
    inc(num, AValue*denom);
  end;

const MaxDev = 6;
var
  dev: array[1..MaxDev] of integer;
  devCount, i: integer;
  curVal, remain: Single;

begin
  if ARatio < 0 then ARatio := -ARatio;
  curVal := ARatio;
  devCount := 0;
  repeat
    inc(devCount);
    dev[devCount] := trunc(curVal);
    remain := frac(curVal);
    if abs(remain) < 1e-3 then break;
    if devCount = MaxDev then
    begin
      if remain > 0.5 then inc(dev[devCount]);
      break;
    end;
    curVal := 1/remain;
  until false;
  repeat
    num := dev[devCount];
    denom := 1;
    for i := devCount-1 downto 1 do
    begin
      InvFrac;
      AddFrac(dev[i]);
    end;
    if ((num >= denom) and (denom <= AMaxDenominator))
       or ((num < denom) and (num <= AMaxDenominator))
       or (devCount = 1) then break;
    dec(devCount);
  until false;
  result := IntToStr(num)+':'+IntToStr(denom);
end;

function TSuperformulaOriginal.GetRadius: double;
begin
  result := 1;
end;

function TSuperformulaOriginal.GetSize: double;
begin
  result := GetSizeWithoutMultiplier * Multiplier;
end;

function TSuperformulaOriginal.GetSizeWithoutMultiplier: double;
const SizePrecision = 50;
var
  r, factor: Double;
  i: Integer;
begin
  if m = 0 then exit(0);
  factor := 2*Pi/m/SizePrecision;
  r := 0;
  for i := 0 to SizePrecision-1 do
  begin
    r += ComputeR(i * factor, a, b, m, n1, n2, n3);
  end;
  r /= SizePrecision;
  result := r;
end;

procedure TSuperformulaOriginal.Render(ADest: TBGRABitmap;
  AMatrix: TAffineMatrix; ADraft: boolean);
var
  backPoints, penOutlinePoints: ArrayOfTPointF;
begin
  try
    GetCurve(AMatrix, backPoints, penOutlinePoints);
    if ADraft then
    begin
      if backPoints <> nil then
         ADest.FillPoly(backPoints, FBackColor, dmDrawWithTransparency, false);
      if penOutlinePoints <> nil then
         ADest.FillPoly(penOutlinePoints, FPenColor, dmDrawWithTransparency, false);
    end else
    begin
      if backPoints <> nil then
         ADest.FillPolyAntialias(backPoints, FBackColor, false);
      if penOutlinePoints <> nil then
         ADest.FillPolyAntialias(penOutlinePoints, FPenColor, false);
    end;
  except
    // ignore exceptions
  end;
end;

function TSuperformulaOriginal.GetRenderBounds(ADestRect: TRect;
  AMatrix: TAffineMatrix): TRect;
var
  backPoints, penOutlinePoints: ArrayOfTPointF;
  resultF: TRectF;
  ptF: TPointF;
begin
  GetCurve(AMatrix, backPoints, penOutlinePoints);
  if (backPoints = nil) and (penOutlinePoints = nil) then
    exit(EmptyRect);
  resultF.Left := MaxSingle;
  resultF.Top := MaxSingle;
  resultF.Right := -MaxSingle;
  resultF.Bottom := -MaxSingle;
  if backPoints <> nil then
    for ptF in backPoints do resultF.Include(ptF);
  if penOutlinePoints <> nil then
    for ptF in penOutlinePoints do resultF.Include(ptF);
  result.Left := floor(resultF.Left);
  result.Top := floor(resultF.Top);
  result.Right := ceil(resultF.Right);
  result.Bottom := ceil(resultF.Bottom);
end;

procedure TSuperformulaOriginal.GetMFraction(out ANumerator,
  ADenominator: integer);
begin
  FloatToFraction(m, ANumerator, ADenominator, MaxDenominator);
end;

procedure TSuperformulaOriginal.LoadFromStorage(
  AStorage: TBGRACustomOriginalStorage);
begin
  FSpikeOverlap:= AStorage.BoolDef['spike-overlap', false];
  Fa := AStorage.Float['a'];
  Fb := AStorage.Float['b'];
  Fm := AStorage.Float['m'];
  FMRational:= AStorage.BoolDef['m-rational', true];
  Fn1 := AStorage.Float['n1'];
  Fn2 := AStorage.Float['n2'];
  Fn3 := AStorage.Float['n3'];
  FMultiplier := AStorage.Float['multiplier'];
  FLineWidth := AStorage.Float['line-width'];

  FPenColor := AStorage.Color['pen-color'];
  FBackColor := AStorage.Color['back-color'];
end;

procedure TSuperformulaOriginal.SaveToStorage(
  AStorage: TBGRACustomOriginalStorage);
begin
  AStorage.Bool['spike-overlap'] := FSpikeOverlap;
  AStorage.Float['a'] := Fa;
  AStorage.Float['b'] := Fb;
  AStorage.Float['m'] := Fm;
  AStorage.Bool['m-rational'] := FMRational;
  AStorage.Float['n1'] := Fn1;
  AStorage.Float['n2'] := Fn2;
  AStorage.Float['n3'] := Fn3;
  AStorage.Float['multiplier'] := FMultiplier;
  AStorage.Float['line-width'] := FLineWidth;

  AStorage.Color['pen-color'] := FPenColor;
  AStorage.Color['back-color'] := FBackColor;
end;

class function TSuperformulaOriginal.StorageClassName: RawByteString;
begin
  Result := 'superformula';
end;

initialization
  RegisterLayerOriginal(TSuperformulaOriginal);

end.
