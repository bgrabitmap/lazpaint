unit UColorFilters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UFilterConnector, BGRABitmap, BGRAFilters, UScripting;

procedure Colorize(FilterConnector: TFilterConnector; hueF, satF: single; useGSBA: boolean);
procedure ShiftColors(FilterConnector: TFilterConnector; hueShiftF, satShiftF: single; useGSBA: boolean; evenRows: boolean = true; oddRows: boolean = true);
procedure FilterIntensity(FilterConnector: TFilterConnector; multiply, shift: single);
procedure FilterLightness(FilterConnector: TFilterConnector; multiply, shift: single);
procedure FilterComplementaryColor(bmp: TBGRABitmap; ARect : TRect);
procedure FilterAdjustCurves(FilterConnector: TFilterConnector; AParams: TVariableSet = nil);

type
  { TAdjustCurvesTask }

  TAdjustCurvesTask = class(TFilterTask)
  private
    FFilterConnector: TFilterConnector;
    FParams: TVariableSet;
  protected
    procedure DoExecute; override;
  public
    constructor Create(AFilterConnector: TFilterConnector; AParams: TVariableSet = nil);
  end;

implementation

uses BGRABitmapTypes;

procedure Colorize(FilterConnector: TFilterConnector; hueF, satF: single; useGSBA: boolean);
var n: NativeInt;
    psrc,pdest: PBGRAPixel;
    hsl: THSLAPixel;
    ec: TExpandedPixel;

    pselect: PBGRAPixel;
    selection: TBGRABitmap;
    alpha: byte;
    hue,sat: word;
begin
    psrc := FilterConnector.BackupLayer.Data;
    pdest := FilterConnector.ActiveLayer.Data;

    hue := round(hueF*65536/360) and 65535;
    if satF < 0 then satF := 0;
    if satF > 1 then satF := 1;
    sat := round(satF*65535);

    selection := FilterConnector.CurrentSelection;
    if selection = nil then
    begin
      alpha := 255;
      pselect := nil;
    end else pselect := selection.Data;

    for n := 0 to FilterConnector.ActiveLayer.NbPixels-1 do
    begin
      if pselect <> nil then
      begin
        alpha := pselect^.green;
        inc(pselect);
      end;
      if alpha <> 0 then
      begin
        ec := GammaExpansion(psrc^);
        hsl.alpha := ec.alpha;
        hsl.lightness := GetLightness(ec);
        hsl.hue := hue;
        hsl.saturation := sat;
        if useGSBA then
          pdest^ := GSBAToBGRA(hsl)
        else
          pdest^ := HSLAToBGRA(hsl);
        if alpha <> 255 then
          pdest^ := MergeBGRAWithGammaCorrection(pdest^,alpha,psrc^,not alpha);
      end else
        pdest^ := psrc^;
      inc(psrc);
      inc(pdest);
    end;
    FilterConnector.ActiveLayer.InvalidateBitmap;
    FilterConnector.InvalidateActiveLayer;
end;

procedure ShiftColors(FilterConnector: TFilterConnector; hueShiftF, satShiftF: single; useGSBA: boolean; evenRows: boolean = true; oddRows: boolean = true);
var yb,xb: NativeInt;
    psrc,pdest: PBGRAPixel;
    hsl: THSLAPixel;

    pselect: PBGRAPixel;
    selection: TBGRABitmap;
    alpha: byte;
    hueShift,satMul: NativeUInt;
    satMulF: single;
    satValue: UInt32or64;
begin
    hueShift := round(hueShiftF*65536/360) and 65535;

    satMulF := exp(satShiftF);
    if satMulF < 0 then satMulF := 0;
    if satMulF > 16 then satMulF := 16;
    satMul := round(satMulF*4096);

    selection := FilterConnector.CurrentSelection;
    pselect := nil;
    alpha := 255;

    for yb := 0 to FilterConnector.ActiveLayer.Height-1 do
    if (oddRows and odd(yb)) or (evenRows and not odd(yb)) then
    begin
      if selection <> nil then pselect := selection.ScanLine[yb];
      psrc := FilterConnector.BackupLayer.ScanLine[yb];
      pdest := FilterConnector.ActiveLayer.ScanLine[yb];
      if not useGSBA then
      begin
        for xb := FilterConnector.ActiveLayer.Width-1 downto 0 do
        begin
          if pselect <> nil then
          begin
            alpha := pselect^.green;
            inc(pselect);
          end;
          if alpha <> 0 then
          begin
            hsl := BGRAToHSLA(psrc^);
            {$PUSH}{$RANGECHECKS OFF}
            hsl.hue := hsl.hue + hueShift;
            {$POP}
            satValue:= hsl.saturation*satMul shr 12;
            if satValue > 65535 then satValue:= 65535;
            hsl.saturation := satValue;
            pdest^ := HSLAToBGRA(hsl);
            if alpha <> 255 then
              pdest^ := MergeBGRAWithGammaCorrection(pdest^,alpha,psrc^,not alpha);
          end;
          inc(psrc);
          inc(pdest);
        end;
      end else
      begin
        for xb := FilterConnector.ActiveLayer.Width-1 downto 0 do
        begin
          if pselect <> nil then
          begin
            alpha := pselect^.green;
            inc(pselect);
          end;
          if alpha <> 0 then
          begin
            hsl := BGRAToGSBA(psrc^);
            {$PUSH}{$RANGECHECKS OFF}
            hsl.hue := hsl.hue + hueShift;
            {$POP}
            satValue := hsl.saturation*satMul shr 12;
            if satValue > 65535 then satValue:= 65535;
            hsl.saturation := satValue;
            pdest^ := GSBAToBGRA(hsl);
            if alpha <> 255 then
              pdest^ := MergeBGRAWithGammaCorrection(pdest^,alpha,psrc^,not alpha);
          end;
          inc(psrc);
          inc(pdest);
        end;
      end;
    end;
    FilterConnector.ActiveLayer.InvalidateBitmap;
    FilterConnector.InvalidateActiveLayer;
end;

procedure FilterIntensity(FilterConnector: TFilterConnector; multiply, shift: single);
var n: integer;
    psrc,pdest: PBGRAPixel;
    ec: TExpandedPixel;
    newIntensity: single;

    pselect: PBGRAPixel;
    selection: TBGRABitmap;
    alpha: byte;

begin
    if not Assigned(FilterConnector) then exit;

    multiply := exp(multiply);
    shift := shift*65535;

    psrc := FilterConnector.BackupLayer.Data;
    pdest := FilterConnector.ActiveLayer.Data;

    selection := FilterConnector.CurrentSelection;
    if selection = nil then
    begin
      alpha := 255;
      pselect := nil;
    end else pselect := selection.Data;

    for n := 0 to FilterConnector.ActiveLayer.NbPixels-1 do
    begin
      if pselect <> nil then
      begin
        alpha := pselect^.green;
        inc(pselect);
      end;
      if alpha <> 0 then
      begin
        if PDWord(psrc)^ and NtoLE($FFFFFF) = 0 then //black
        begin
          ec.red := 1;
          ec.green := 1;
          ec.blue := 1;
          ec.alpha := psrc^.alpha shl 8 + psrc^.alpha;
          newIntensity := ((-32767.5+shift)*multiply+32767.5)*0.5;
        end else
        begin
          ec := GammaExpansion(psrc^);
          newIntensity := (GetIntensity(ec)-32767.5+shift)*multiply+32767.5;
        end;
        if newIntensity < 0 then newIntensity := 0;
        if newIntensity > 65535 then newIntensity := 65535;
        pdest^ := GammaCompression(SetIntensity(ec,round(newIntensity)));
        if alpha <> 255 then
          pdest^ := MergeBGRAWithGammaCorrection(pdest^,alpha,psrc^,not alpha);
      end;
      inc(psrc);
      inc(pdest);
    end;
    FilterConnector.ActiveLayer.InvalidateBitmap;
    FilterConnector.InvalidateActiveLayer;
end;

procedure FilterLightness(FilterConnector: TFilterConnector; multiply, shift: single);
var n: integer;
    psrc,pdest: PBGRAPixel;
    ec: TExpandedPixel;
    newIntensity: single;

    pselect: PBGRAPixel;
    selection: TBGRABitmap;
    alpha: byte;
    curLightness: NativeInt;

begin
    if not Assigned(FilterConnector) then exit;

    multiply := exp(multiply);
    shift := shift*65535;

    psrc := FilterConnector.BackupLayer.Data;
    pdest := FilterConnector.ActiveLayer.Data;

    selection := FilterConnector.CurrentSelection;
    if selection = nil then
    begin
      alpha := 255;
      pselect := nil;
    end else pselect := selection.Data;

    for n := 0 to FilterConnector.ActiveLayer.NbPixels-1 do
    begin
      if pselect <> nil then
      begin
        alpha := pselect^.green;
        inc(pselect);
      end;
      if alpha <> 0 then
      begin
        ec := GammaExpansion(psrc^);
        curLightness:= GetLightness(ec);
        newIntensity := (curLightness-32767.5+shift)*multiply+32767.5;
        if newIntensity < 0 then newIntensity := 0;
        if newIntensity > 65535 then newIntensity := 65535;
        pdest^ := GammaCompression(SetLightness(ec,round(newIntensity),curLightness));
        if alpha <> 255 then
          pdest^ := MergeBGRAWithGammaCorrection(pdest^,alpha,psrc^,not alpha);
      end;
      inc(psrc);
      inc(pdest);
    end;
    FilterConnector.ActiveLayer.InvalidateBitmap;
    FilterConnector.InvalidateActiveLayer;
end;

procedure FilterComplementaryColor(bmp: TBGRABitmap; ARect: TRect);
var y,count: integer;
    p: PBGRAPixel;
    gsba: THSLAPixel;
begin
  if (ARect.Right <= ARect.Left) or (ARect.Bottom <= ARect.Top) then exit;
  for y := ARect.Top to ARect.Bottom-1 do
  begin
    p := bmp.ScanLine[y]+ARect.Left;
    for count := ARect.Right-ARect.Left-1 downto 0 do
    begin
      gsba := BGRAToGSBA(p^);
      gsba.hue := (gsba.hue + 32768) and 65535;
      p^ := GSBAToBGRA(gsba);
      inc(p);
    end;
  end;
  bmp.InvalidateBitmap;
end;

procedure FilterAdjustCurves(FilterConnector: TFilterConnector; AParams: TVariableSet = nil);
var task: TAdjustCurvesTask;
begin
  task := TAdjustCurvesTask.Create(FilterConnector,AParams);
  task.Execute;
  task.Free;
  FilterConnector.InvalidateActiveLayer;
end;

{ TAdjustCurvesTask }

procedure TAdjustCurvesTask.DoExecute;
type
  TCurveInfo = record
    x,y: array of NativeInt;
    slope: array of record
      mulFactor, addBeforeShift, bitShift: NativeInt;
    end;
    maxValue, count: NativeInt;
  end;
  function CurveDefined(const ACurve: TCurveInfo): boolean;
  begin
    result:= ACurve.x <> nil;
  end;
  function GetCurve(ASubset: TVariableSet; AFactor: NativeInt): TCurveInfo;
  var XList,YList: TScriptVariableReference;
      i,pointCount: NativeInt;
      trivial: boolean;
      slopeValue,slopeX,slopeY: double;
  begin
    result.count := 0;
    result.maxValue := 0;
    if Assigned(ASubset) then
    begin
      XList := ASubset.GetVariable('X');
      YList := ASubset.GetVariable('Y');
      if ASubset.IsReferenceDefined(XList) and
        ASubset.IsReferenceDefined(YList) then
      begin
        pointCount := ASubset.GetListCount(XList);
        if ASubset.Booleans['Posterize'] then trivial := false
        else
          begin
            trivial := true;
            for i := 0 to pointCount-1 do
              if abs(ASubset.GetFloatAt(XList, i)-ASubset.GetFloatAt(YList, i)) > 1e-6 then trivial := false;
          end;
        if not trivial then
        begin
          setlength(result.x,pointCount);
          setlength(result.y,pointCount);
          setlengtH(result.slope,pointCount-1);
          for i := 0 to pointCount-1 do
          begin
            result.x[i] := trunc(ASubset.GetFloatAt(XList, i)*AFactor+0.5);
            result.y[i] := trunc(ASubset.GetFloatAt(YList, i)*AFactor+0.5);
          end;
          if ASubset.Booleans['Posterize'] then
          begin
            for i := 0 to pointCount-2 do
            begin
              result.slope[i].addBeforeShift := result.y[i];
              result.slope[i].mulFactor := 0;
              result.slope[i].bitShift := 0;
            end;
          end else
          for i := 0 to pointCount-2 do
          begin
            slopeX := result.x[i+1]-result.x[i];
            slopeY := result.y[i+1]-result.y[i];
            result.slope[i].addBeforeShift := result.y[i];
            if slopeX = 0 then slopeValue := 1 else
              slopeValue:= slopeY/slopeX;
            if abs(slopeValue-1) < 1e-6 then
            begin
              result.slope[i].mulFactor := 1;
              result.slope[i].bitShift := 0;
            end else
            if abs(slopeValue) < 1e-6 then
            begin
              result.slope[i].mulFactor := 0;
              result.slope[i].bitShift := 0;
            end else
            begin
              result.slope[i].bitShift:= 0;
              while (result.slope[i].bitShift < 15) and (round(slopeValue) < 16383) and
                 (abs(result.slope[i].addBeforeShift) < $20000000) do
              begin
                inc(result.slope[i].bitShift);
                slopeValue *= 2;
                result.slope[i].addBeforeShift *= 2;
              end;
              result.slope[i].mulFactor:= trunc(slopeValue+0.5);
            end;
          end;
          result.maxValue:= AFactor;
          result.count:= pointCount;
        end;
      end;
    end;

  end;

  function Transform(AValue: NativeInt; const ACurve: TCurveInfo): NativeInt;
  var minIndex,maxIndex,middle: NativeInt;
  begin
    if ACurve.x = nil then
    begin
      result := AValue;
      exit;
    end;
    if AValue >= ACurve.x[high(ACurve.x)] then
    begin
      result := ACurve.y[high(ACurve.x)];
      exit;
    end;
    minIndex := 0;
    maxIndex := ACurve.count-1;
    while minIndex+1 < maxIndex do
    begin
      middle := (minIndex+maxIndex) shr 1;
      if AValue < ACurve.x[middle] then
        maxIndex := middle else
      if AValue > ACurve.x[middle] then
        minIndex := middle else
      begin
        result := ACurve.y[middle];
        if result < 0 then result := 0;
        if result > ACurve.maxValue then result := ACurve.maxValue;
        exit;
      end;
    end;
    result := (AValue-ACurve.x[minIndex])*ACurve.slope[minIndex].mulFactor+ACurve.slope[minIndex].addBeforeShift;
    if result < 0 then result := 0 else
    begin
      result := result shr ACurve.slope[minIndex].bitShift;
      if result > ACurve.maxValue then result := ACurve.maxValue;
    end;
  end;

var
  RGBATab: packed array[0..3,0..255] of word;
  HSTab: packed array[0..1,0..1023] of word;
  RGBCurves: array[0..3] of TCurveInfo;
  HSLCurves: array[0..3] of TCurveInfo;
  AlphaCurve: TCurveInfo;
  ApplyRGBA,ApplyHS,ApplyL: boolean;

  y,x,n: NativeInt;
  psrc,pdest: PBGRAPixel;

  pselect: PBGRAPixel;
  selection: TBGRABitmap;
  alpha: byte;
  ec: TExpandedPixel;
  hsl: THSLAPixel;
  curLightness: word;

  Params: TVariableSet;

begin
  if not Assigned(FFilterConnector) then exit;
  if FParams = nil then Params := FFilterConnector.Parameters
    else Params := FParams;
  RGBCurves[0] := GetCurve(Params.Subsets['Red'],65535);
  RGBCurves[1] := GetCurve(Params.Subsets['Green'],65535);
  RGBCurves[2] := GetCurve(Params.Subsets['Blue'],65535);
  HSLCurves[0] := GetCurve(Params.Subsets['Hue'],65535);
  HSLCurves[1] := GetCurve(Params.Subsets['Saturation'],65535);
  HSLCurves[2] := GetCurve(Params.Subsets['Lightness'],65535);
  ApplyL  := CurveDefined(HSLCurves[2]);
  ApplyHS := CurveDefined(HSLCurves[0]) or CurveDefined(HSLCurves[1]);
  AlphaCurve := GetCurve(Params.Subsets['Alpha'],65535);
  ApplyRGBA:= CurveDefined(RGBCurves[0]) or CurveDefined(RGBCurves[1]) or CurveDefined(RGBCurves[2]) or CurveDefined(AlphaCurve);

  selection := FFilterConnector.CurrentSelection;
  if selection = nil then
  begin
    alpha := 255;
    pselect := nil;
  end;

  if ApplyRGBA then
    for n := 0 to 255 do
    begin
      RGBATab[0,n] := Transform(GammaExpansionTab[n],RGBCurves[0]);
      RGBATab[1,n] := Transform(GammaExpansionTab[n],RGBCurves[1]);
      RGBATab[2,n] := Transform(GammaExpansionTab[n],RGBCurves[2]);
      RGBATab[3,n] := Transform(n+(n shl 8),AlphaCurve);
    end;
  if ApplyHS then
    for n := 0 to 1023 do
    begin
      HSTab[0,n] := Transform((n shl 6) + (n shr 4),HSLCurves[0]);
      HSTab[1,n] := Transform((n shl 6) + (n shr 4),HSLCurves[1]);
    end;

  for y := FFilterConnector.WorkArea.Top to FFilterConnector.WorkArea.Bottom-1 do
  begin
    if Assigned(CheckShouldStop) and CheckShouldStop(y) then break;
    psrc := FFilterConnector.BackupLayer.ScanLine[y]+FFilterConnector.WorkArea.Left;
    pdest := FFilterConnector.ActiveLayer.ScanLine[y]+FFilterConnector.WorkArea.Left;
    if assigned(selection) then pselect := selection.ScanLine[y]+FFilterConnector.WorkArea.Left;
    x := FFilterConnector.WorkArea.Right-FFilterConnector.WorkArea.Left;
    while x > 0 do
    begin
      if pselect <> nil then
      begin
        alpha := pselect^.green;
        inc(pselect);
        if alpha = 0 then
        begin
          inc(psrc);
          inc(pdest);
          dec(x);
          continue;
        end;
      end;
      if ApplyRGBA then
      begin
        ec.red := RGBATab[0,psrc^.red];
        ec.green := RGBATab[1,psrc^.green];
        ec.blue := RGBATab[2,psrc^.blue];
        ec.alpha := RGBATab[3,psrc^.alpha];
      end else
        ec := GammaExpansion(psrc^);
      if ApplyL then
      begin
        curLightness:= GetLightness(ec);
        ec := SetLightness(ec,Transform(curLightness,HSLCurves[2]),curLightness);
      end;
      if ApplyHS then
      begin
        hsl := ExpandedToHSLA(ec);
        hsl.hue := HSTab[0,hsl.hue shr 6];
        hsl.saturation := HSTab[1,hsl.saturation shr 6];
        ec := HSLAToExpanded(hsl);
      end;
      if alpha <> 255 then
        pdest^ := MergeBGRAWithGammaCorrection(GammaCompression(ec),alpha,psrc^,not alpha)
      else
        pdest^ := GammaCompression(ec);
      inc(psrc);
      inc(pdest);
      dec(x);
    end;
  end;
  FFilterConnector.ActiveLayer.InvalidateBitmap;
end;

constructor TAdjustCurvesTask.Create(AFilterConnector: TFilterConnector;
  AParams: TVariableSet);
begin
  FFilterConnector := AFilterConnector;
  FParams := AParams;
  Destination := AFilterConnector.ActiveLayer;
end;

end.

