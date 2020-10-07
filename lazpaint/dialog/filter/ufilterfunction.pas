// SPDX-License-Identifier: GPL-3.0-only
unit UFilterFunction;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, fpexprpars, UFilterConnector, BGRABitmap,
  BGRABitmapTypes, UScripting;

const
  StatsName: array[1..10] of string =
  ('red','green','blue','alpha','hue','saturation','lightness','L','a','b');

type
  TLabABitmap = specialize TGenericUniversalBitmap<TLabA,TLabAColorspace>;

  { TFFilterFunction }

  TFFilterFunction = class(TForm)
    Button_Cancel: TButton;
    Button_OK: TButton;
    CheckBox_Gamma: TCheckBox;
    CheckBox_GSBA: TCheckBox;
    Edit_Alpha: TEdit;
    Edit_Blue: TEdit;
    Edit_Green: TEdit;
    Edit_Hue: TEdit;
    Edit_L: TEdit;
    Edit_Lightness: TEdit;
    Edit_b: TEdit;
    Edit_Red: TEdit;
    Edit_Saturation: TEdit;
    Edit_a: TEdit;
    Label_BlueEquals: TLabel;
    Label_GreenEquals: TLabel;
    Label_HueEquals: TLabel;
    Label_bEquals: TLabel;
    Label_LEquals: TLabel;
    Label_LightnessEquals: TLabel;
    Label_RedEquals: TLabel;
    Label_aEquals: TLabel;
    Label_SaturationEquals: TLabel;
    Label_Variables: TLabel;
    Label_AlphaEquals: TLabel;
    PageControl_Color: TPageControl;
    PanelLab: TPanel;
    PanelLabelLab: TPanel;
    PanelLabelRGB: TPanel;
    PanelLabelHSL: TPanel;
    PanelRGB: TPanel;
    PanelHSL: TPanel;
    TabSheet_Lab: TTabSheet;
    TabSheet_RGB: TTabSheet;
    TabSheet_HSL: TTabSheet;
    Timer1: TTimer;
    Timer_AdjustVerticalSize: TTimer;
    procedure Button_CancelClick(Sender: TObject);
    procedure Button_OKClick(Sender: TObject);
    procedure CheckBox_GammaChange(Sender: TObject);
    procedure CheckBox_GSBAChange(Sender: TObject);
    procedure Edit_aChange(Sender: TObject);
    procedure Edit_AlphaChange(Sender: TObject);
    procedure Edit_bChange(Sender: TObject);
    procedure Edit_BlueChange(Sender: TObject);
    procedure Edit_GreenChange(Sender: TObject);
    procedure Edit_HueChange(Sender: TObject);
    procedure Edit_LChange(Sender: TObject);
    procedure Edit_LightnessChange(Sender: TObject);
    procedure Edit_RedChange(Sender: TObject);
    procedure Edit_SaturationChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PageControl_ColorChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer_AdjustVerticalSizeTimer(Sender: TObject);
  private
    { private declarations }
    FRedExpr, FGreenExpr, FBlueExpr, FAlphaExpr,
    FHueExpr, FSaturationExpr, FLightnessExpr,
    FLExpr, FaExpr, FbExpr: TFPExpressionParser;
    FRedError, FGreenError, FBlueError, FAlphaError,
    FHueError, FSaturationError, FLightnessError,
    FLError, FaError, FbError: boolean;
    FComputing: boolean;
    FSourceAsLab: TLabABitmap;
    FComputedImage: TBGRABitmap;
    FComputedLines: integer;
    FFilterConnector: TFilterConnector;
    FInitializing: boolean;
    FStats: array[low(StatsName)..high(StatsName)] of record
        min,max,sum,avg: single;
        count: integer;
        computed: boolean;
      end;
    procedure UpdateExpr(AExpr: TFPExpressionParser; AEdit: TEdit;
      var AError: boolean);
    procedure InitParams;
    procedure PreviewNeeded;
    function CreateExpr: TFPExpressionParser;
    function ExprResultToFloat(const AResult: TFPExpressionResult): single;
    procedure ExprFunctionMin_Call(Var Result : TFPExpressionResult; Const Args : TExprParameterArray);
    procedure ExprFunctionMax_Call(Var Result : TFPExpressionResult; Const Args : TExprParameterArray);
    procedure StatsNotComputed(AFrom,ATo: integer);
    procedure NeedStats(AStatIndex: integer);
    function ReplaceStats(AExpr: string): string;
  public
    { public declarations }
  end;

function ShowFilterFunctionDlg(AFilterConnector: TObject): TScriptResult;

implementation

uses UMac, LazPaintType, math;

function ShowFilterFunctionDlg(AFilterConnector: TObject): TScriptResult;
var
  FFilterFunction: TFFilterFunction;
begin
  FFilterFunction:= TFFilterFunction.create(nil);
  FFilterFunction.FFilterConnector := AFilterConnector as TFilterConnector;
  try
    if FFilterFunction.FFilterConnector.ActiveLayer <> nil then
    begin
      if Assigned(FFilterFunction.FFilterConnector.Parameters) and
        FFilterFunction.FFilterConnector.Parameters.Booleans['Validate'] then
      begin
        FFilterFunction.InitParams;
        FFilterFunction.PreviewNeeded;
        while FFilterFunction.FComputing do FFilterFunction.Timer1Timer(FFilterFunction);
        FFilterFunction.FFilterConnector.ValidateAction;
        result := srOk;
      end else
      begin
        if FFilterFunction.showModal = mrOk then result := srOk
        else result:= srCancelledByUser;
      end;
    end
    else
      result := srException;
  finally
    FFilterFunction.free;
  end;
end;

function LocateIdentifier(AExpr: string; AVar: string): integer;
var i: integer;
  inStr: boolean;
begin
  result := 0;
  if (AExpr = '') or (AVar = '') then exit;
  inStr := false;
  for i := 1 to length(AExpr)-length(AVar)+1 do
  if AExpr[i] = '''' then
  begin
    inStr := not inStr
  end else
  if (UpCase(AExpr[i]) = UpCase(AVar[1])) and not inStr then
  begin
    if (i = 1) or not (AExpr[i-1] in['a'..'z','A'..'Z','_','0'..'9']) then
    begin
      if (i+length(AVar) = length(AExpr)+1) or
        not (AExpr[i+length(AVar)] in ['a'..'z','A'..'Z','_','0'..'9']) then
      begin
        if CompareText(copy(AExpr,i,length(AVar)), AVar) = 0 then
        begin
          result := i;
          exit;
        end;
      end;
    end;
  end;
end;

function ContainsIdentifier(AExpr: string; AVar: string): boolean;
begin
  result := LocateIdentifier(AExpr, AVar) <> 0;
end;

function ReplaceIdentifier(AExpr: string; AVar, ANewVar: string): string;
var
  idx: Integer;
begin
  if LowerCase(ANewVar).Contains(LowerCase(AVar)) then exit;
  result := AExpr;
  repeat
    idx := LocateIdentifier(result, AVar);
    if idx = 0 then break;
    delete(result, idx, length(AVar));
    insert(ANewVar, result, idx);
  until false;
end;

{ TFFilterFunction }

procedure TFFilterFunction.FormCreate(Sender: TObject);
begin
  CheckOKCancelBtns(Button_OK,Button_Cancel);

  FRedExpr := CreateExpr;
  FRedError:= false;
  FGreenExpr := CreateExpr;
  FGreenError := false;
  FBlueExpr := CreateExpr;
  FBlueError := false;
  FAlphaExpr := CreateExpr;
  FAlphaError := false;
  FHueExpr := CreateExpr;
  FHueError := false;
  FSaturationExpr := CreateExpr;
  FSaturationError := false;
  FLightnessExpr := CreateExpr;
  FLightnessError := false;
  FLExpr := CreateExpr;
  FLError := false;
  FaExpr := CreateExpr;
  FaError := false;
  FbExpr := CreateExpr;
  FbError := false;
  Label_RedEquals.Caption := 'red (0..1) = ';
  Label_GreenEquals.Caption := 'green (0..1) = ';
  Label_BlueEquals.Caption := 'blue (0..1) = ';
  Label_AlphaEquals.Caption := 'alpha (0..1) = ';
  Label_HueEquals.Caption := 'hue (0..1) = ';
  Label_SaturationEquals.Caption := 'saturation (0..1) = ';
  Label_LightnessEquals.Caption := 'lightness (0..1) = ';
  Label_LEquals.Caption := 'L (0..1) = ';
  Label_aEquals.Caption := 'a (-1..1) = ';
  Label_bEquals.Caption := 'b (-1..1) = ';
  Label_Variables.Caption := Label_Variables.Caption+' x,y,width,height,random,min,max,avg';

  StatsNotComputed(low(FStats), high(FStats));
end;

procedure TFFilterFunction.FormDestroy(Sender: TObject);
begin
  FSourceAsLab.Free;
  FComputedImage.Free;
  FRedExpr.Free;
  FGreenExpr.Free;
  FBlueExpr.Free;
  FAlphaExpr.Free;
  FHueExpr.Free;
  FSaturationExpr.Free;
  FLightnessExpr.Free;
  FLExpr.Free;
  FaExpr.Free;
  FbExpr.Free;
end;

procedure TFFilterFunction.FormShow(Sender: TObject);
begin
  InitParams;
  PreviewNeeded;
  Timer_AdjustVerticalSize.Interval:= 50;
  Timer_AdjustVerticalSize.Enabled := true;
end;

procedure TFFilterFunction.PageControl_ColorChange(Sender: TObject);
begin
  if not FInitializing then PreviewNeeded;
end;

procedure TFFilterFunction.Timer1Timer(Sender: TObject);
const
  TimeGrain = 150/1000/60/60/24;
  oneOver255 = 1/255;
  oneOver65535 = 1/65535;
  oneOver65536 = 1/65536;
type
  TExprValues = record
    Red, Green, Blue, Alpha,
    X, Y, Random,
    Hue, Saturation, Lightness,
    L, a, b: TExprFloat;
  end;

  TEvaluateFunc = function: TFPExpressionResult of object;

  TExprVariables = record
    Evaluate: TEvaluateFunc;
    XValue, YValue,
    RedValue, GreenValue, BlueValue, AlphaValue,
    HueValue, SaturationValue, LightnessValue,
    LValue, aValue, bValue,
    RandomValue: TFPExprIdentifierDef;
    XYUsed, RGBUsed, AlphaUsed, HSLUsed, LabUsed, RandomUsed: boolean;
    IsCopySrc: boolean;
    CopyOffset: PtrInt;
    IsConstant: boolean;
    ConstantValue: Word;
    ConstantValueF: single;
  end;

var
  values: TExprValues;
  rgbUsedByAny, hslUsedByAny, labUsedByAny, xyUsedByAny: boolean;
  rgbUsedInExpr, hslUsedInExpr, xyUsedInExpr: boolean;
  src: packed record
    red,green,blue,alpha: word;
    L,a,b,filler: word;
    x,y: word;
    case boolean of
    false: (hslaValue: THSLAPixel);
    true: (gsbaValue: TGSBAPixel);
  end;

  procedure InitUsedValues(var AVars: TExprVariables); inline;
  begin
    if AVars.XYUsed then
    begin
      AVars.XValue.AsFloat := values.x;
      AVars.YValue.AsFloat := values.y;
    end;
    if AVars.RGBUsed then
    begin
      AVars.RedValue.AsFloat := values.Red;
      AVars.GreenValue.AsFloat := values.Green;
      AVars.BlueValue.AsFloat := values.Blue;
    end;
    AVars.AlphaValue.AsFloat := values.Alpha;
    if AVars.HSLUsed then
    begin
      AVars.HueValue.AsFloat := values.Hue;
      AVars.SaturationValue.AsFloat := values.Saturation;
      AVars.LightnessValue.AsFloat := values.Lightness;
    end;
    if AVars.LabUsed then
    begin
      AVars.LValue.AsFloat := values.L;
      AVars.aValue.AsFloat := values.a;
      AVars.bValue.AsFloat := values.b;
    end;
    if AVars.RandomUsed then AVars.RandomValue.AsFloat := Random;
  end;

  function ComputeExpr(var AVars: TExprVariables; AFactor: integer = 65535): integer; inline;
  var {%H-}code: integer;
    floatValue: single;
  begin
    if AVars.IsCopySrc then exit((PWord(@src) + AVars.CopyOffset)^) else
    if AVars.IsConstant then exit(AVars.ConstantValue);
    InitUsedValues(AVars);
    with AVars.Evaluate do
    begin
      case ResultType of
      rtFloat: if resFloat < 0 then result := 0 else
          if resFloat > 1 then result := AFactor else
              result := round(ResFloat*AFactor);
      rtInteger: if ResInteger <= 0 then result := 0 else result := AFactor;
      rtBoolean: if ResBoolean then result := AFactor else result := 0;
      rtDateTime: result := 0;
      rtString: begin
                  val(ResString, floatValue, code);
                  if floatValue < 0 then result := 0 else
                  if floatValue > 1 then result := AFactor else
                    result := round(floatValue*AFactor);
                end;
      else result := 0;
      end;
    end;
    if result < 0 then result := 0;
    if result > 65535 then dec(result, 65536);
  end;

  function ComputeExprF(var AVars: TExprVariables): single; inline;
  var {%H-}code: integer;
  begin
    if AVars.IsCopySrc then
    begin
      case AVars.CopyOffset of
      4: result := values.L;
      5: result := values.a;
      6: result := values.b;
      else exit((PWord(@src) + AVars.CopyOffset)^ / 65535);
      end;
    end else
    if AVars.IsConstant then exit(AVars.ConstantValueF);
    InitUsedValues(AVars);
    with AVars.Evaluate do
    begin
      case ResultType of
      rtFloat: result := ResFloat;
      rtInteger: result := ResInteger;
      rtBoolean: if ResBoolean then result := 1 else result := 0;
      rtDateTime: result := 0;
      rtString: val(ResString, result, code);
      else result := 0;
      end;
    end;
  end;

  procedure PrepareXY(AExpr: TFPExpressionParser; out AVars: TExprVariables);
  var exprComp: string;
    i: Integer;
  begin
    with AVars do
    begin
      Evaluate := @AExpr.Evaluate;

      XYUsed := ContainsIdentifier(AExpr.Expression,'x') or
         ContainsIdentifier(AExpr.Expression,'y');
      RGBUsed := (ContainsIdentifier(AExpr.Expression,'red') or
         ContainsIdentifier(AExpr.Expression,'green') or
         ContainsIdentifier(AExpr.Expression,'blue'));
      HSLUsed := (ContainsIdentifier(AExpr.Expression,'hue') or
         ContainsIdentifier(AExpr.Expression,'saturation') or
         ContainsIdentifier(AExpr.Expression,'lightness'));
      LabUsed := (ContainsIdentifier(AExpr.Expression,'L') or
         ContainsIdentifier(AExpr.Expression,'a') or
         ContainsIdentifier(AExpr.Expression,'b'));
      RandomUsed:= ContainsIdentifier(AExpr.Expression,'random');
      AlphaUsed:= ContainsIdentifier(AExpr.Expression,'alpha');

      XValue := AExpr.IdentifierByName('x');
      YValue:= AExpr.IdentifierByName('y');
      RedValue:= AExpr.IdentifierByName('red');
      GreenValue:= AExpr.IdentifierByName('green');
      BlueValue:= AExpr.IdentifierByName('blue');
      AlphaValue:= AExpr.IdentifierByName('alpha');
      HueValue:= AExpr.IdentifierByName('hue');
      SaturationValue:= AExpr.IdentifierByName('saturation');
      LightnessValue:= AExpr.IdentifierByName('lightness');
      LValue:= AExpr.IdentifierByName('L');
      aValue:= AExpr.IdentifierByName('a');
      bValue:= AExpr.IdentifierByName('b');
      RandomValue:= AExpr.IdentifierByName('random');
      AExpr.IdentifierByName('width').AsInteger := FFilterConnector.BackupLayer.Width;
      AExpr.IdentifierByName('height').AsInteger := FFilterConnector.BackupLayer.Height;

      IsCopySrc:= false;
      IsConstant := false;
      for i := low(StatsName) to high(StatsName) do
        if ContainsIdentifier(AExpr.Expression, 'min_'+StatsName[i]) or
           ContainsIdentifier(AExpr.Expression, 'max_'+StatsName[i]) or
           ContainsIdentifier(AExpr.Expression, 'avg_'+StatsName[i]) then
        begin
          NeedStats(i);
          AExpr.IdentifierByName('min_'+StatsName[i]).AsFloat := FStats[i].min;
          AExpr.IdentifierByName('max_'+StatsName[i]).AsFloat := FStats[i].max;
          AExpr.IdentifierByName('avg_'+StatsName[i]).AsFloat := FStats[i].avg;
        end;

      if not HSLUsed and not RGBUsed and not LabUsed and not XYUsed and not RandomUsed and not AlphaUsed then
      begin
        ConstantValue := ComputeExpr(AVars);
        ConstantValueF := ComputeExprF(AVars);
        IsConstant := true; //set flag after computing value
      end else
      begin
        ConstantValue := 0;
        ConstantValueF := 0;
        IsConstant := false;
      end;

      exprComp := LowerCase(trim(AExpr.Expression));
      CopyOffset:= 0;
      IsCopySrc:= true;
      case exprComp of
      'red': copyOffset := 0;
      'green': copyOffset := 1;
      'blue': copyOffset := 2;
      'alpha': copyOffset := 3;
      'l': copyOffset := 4;
      'a': copyOffset := 5;
      'b': copyOffset := 6;
      'x': copyOffset := 8;
      'y': copyOffset := 9;
      'hue': copyOffset := 10;
      'saturation': copyOffset := 11;
      'lightness': copyOffset := 12;
      else IsCopySrc:= false;
      end;

      if RGBUsed then rgbUsedByAny:= true;
      if HSLUsed then hslUsedByAny:= true;
      if LabUsed then labUsedByAny:= true;
      if XYUsed then xyUsedByAny:= true;
      if RGBUsed and not IsCopySrc then rgbUsedInExpr:= true;
      if HSLUsed and not IsCopySrc then hslUsedInExpr:= true;
      if XYUsed and not IsCopySrc then xyUsedInExpr:= true;
    end;
  end;

var PrevDate: TDateTime;
  x,y,w,h,xcount: integer;
  pdest,psrc: PBGRAPixel;
  psrcLab: PLabA;
  RedVars, GreenVars, BlueVars, AlphaVars,
  HueVars, SaturationVars, LightnessVars,
  LVars, aVars, bVars: TExprVariables;
  prevComputedLines: integer;
  gsba,rgbMode,hslMode,labMode,gammaCorr: boolean;
  labValue: TLabA;
  converter: TBridgedConversion;

begin
  Timer1.Enabled:= false;
  if FComputing then
  begin
    if FComputedImage = nil then
    begin
      FComputedImage := TBGRABitmap.Create(FFilterConnector.BackupLayer.Width,FFilterConnector.BackupLayer.Height);
      FComputedLines := FFilterConnector.WorkArea.Top;
      FFilterConnector.RestoreBackup;
    end;
    gsba := CheckBox_GSBA.Checked;
    gammaCorr := CheckBox_Gamma.Checked;
    PrevDate := Now;
    prevComputedLines:= FComputedLines;
    try
      rgbMode := PageControl_Color.ActivePage = TabSheet_RGB;
      hslMode := PageControl_Color.ActivePage = TabSheet_HSL;
      labMode := PageControl_Color.ActivePage = TabSheet_Lab;
      w := FFilterConnector.BackupLayer.Width;
      h := FFilterConnector.BackupLayer.Height;
      rgbUsedByAny := false;
      hslUsedByAny := false;
      labUsedByAny := false;
      xyUsedByAny := false;
      rgbUsedInExpr := false;
      hslUsedInExpr := false;
      xyUsedInExpr := false;
      fillchar({%H-}values, sizeOf(values), 0);
      if rgbMode then
      begin
        PrepareXY(FRedExpr, RedVars);
        PrepareXY(FGreenExpr, GreenVars);
        PrepareXY(FBlueExpr, BlueVars);
      end else
      if hslMode then
      begin
        PrepareXY(FHueExpr, HueVars);
        PrepareXY(FSaturationExpr, SaturationVars);
        PrepareXY(FLightnessExpr, LightnessVars);
      end else
      if labMode then
      begin
        PrepareXY(FLExpr, LVars);
        PrepareXY(FaExpr, aVars);
        PrepareXY(FbExpr, bVars);
      end
      else raise exception.Create('Unknown selected page');
      PrepareXY(FAlphaExpr, AlphaVars);

      if labUsedByAny then
      begin
        if Assigned(FSourceAsLab) and ((FSourceAsLab.Width <> FFilterConnector.WorkArea.Width)
          or (FSourceAsLab.Height <> FFilterConnector.WorkArea.Height)) then FreeAndNil(FSourceAsLab);
        if FSourceAsLab = nil then
        begin
          Screen.Cursor := crHourGlass;
          FSourceAsLab := TLabABitmap.Create(FFilterConnector.WorkArea.Width, FFilterConnector.WorkArea.Height, BGRAPixelTransparent);
          converter := FFilterConnector.BackupLayer.Colorspace.GetBridgedConversion(FSourceAsLab.Colorspace);
          for y := FFilterConnector.WorkArea.Top to FFilterConnector.WorkArea.Bottom-1 do
            converter.Convert(FFilterConnector.BackupLayer.ScanLine[y] + FFilterConnector.WorkArea.Left,
              FSourceAsLab.ScanLine[y - FFilterConnector.WorkArea.Top], FSourceAsLab.Width,
              sizeof(TBGRAPixel), sizeof(TLabA), nil);
          Screen.Cursor := crDefault;
        end;
      end;

      while FComputedLines < FFilterConnector.WorkArea.Bottom do
      begin
        y := FComputedLines;
        psrc := FFilterConnector.BackupLayer.ScanLine[y]+FFilterConnector.WorkArea.Left;
        if labUsedByAny then psrcLab:= PLabA(FSourceAsLab.GetPixelAddress(0, y - FFilterConnector.WorkArea.Top));
        pdest := FComputedImage.ScanLine[y]+FFilterConnector.WorkArea.Left;
        xcount := FFilterConnector.WorkArea.Right - FFilterConnector.WorkArea.Left;
        src.y := (y*65535+(h shr 1)) div h;
        if xyUsedInExpr then values.Y := y/h;
        try
          for x := 0 to xcount-1 do
          begin
            if xyUsedByAny then
            begin
              src.x := ((x + FFilterConnector.WorkArea.Left)*65535+(w shr 1)) div w;
              if xyUsedInExpr then values.X := src.x*oneOver65535;
            end;
            if rgbUsedByAny then
            begin
              if gammaCorr then
              begin
                src.red := GammaExpansionTab[psrc^.red];
                src.green := GammaExpansionTab[psrc^.green];
                src.blue := GammaExpansionTab[psrc^.blue];
              end else
              begin
                src.red := psrc^.red + (psrc^.red shl 8);
                src.green := psrc^.green + (psrc^.green shl 8);
                src.blue := psrc^.blue + (psrc^.blue shl 8);
              end;
              if rgbUsedInExpr then
              begin
                values.Red := src.red *oneOver65535;
                values.Green := src.green *oneOver65535;
                values.Blue := src.blue *oneOver65535;
              end;
            end;
            if hslUsedByAny then
            begin
              if gsba then src.gsbaValue := BGRAToGSBA(psrc^) else
              if gammaCorr then src.hslaValue := BGRAToHSLA(psrc^) else
              with psrc^.ToStdHSLA do
              begin
                src.hslaValue.hue := round(hue*(65536/360)) mod 65536;
                src.hslaValue.saturation := round(saturation*65535);
                src.hslaValue.lightness := round(lightness*65535);
                src.hslaValue.alpha := psrc^.alpha + (psrc^.alpha shl 8);
              end;
              if hslUsedInExpr then
              with src.hslaValue do
              begin
                values.Hue := hue*oneOver65536;
                values.Saturation := saturation*oneOver65535;
                values.Lightness := lightness*oneOver65535;
              end;
            end;
            if labUsedByAny then
            begin
              labValue := psrcLab^;
              inc(psrcLab);
              values.L := labValue.L/100;
              values.a := labValue.a/127;
              values.b := labValue.b/127;
              src.L := min(65535,max(0,round(values.L*65535)));
              src.a := min(65535,max(0,round(values.a*65535)));
              src.b := min(65535,max(0,round(values.b*65535)));
            end;
            src.alpha := psrc^.alpha + (psrc^.alpha shl 8);
            values.Alpha := psrc^.alpha * oneOver255;
            if rgbMode then
            begin
              if gammaCorr then
              begin
                pdest^.red := GammaCompressionTab[ComputeExpr(RedVars)];
                pdest^.green := GammaCompressionTab[ComputeExpr(GreenVars)];
                pdest^.blue := GammaCompressionTab[ComputeExpr(BlueVars)];
                pdest^.alpha := ComputeExpr(AlphaVars) shr 8;
              end else
              begin
                pdest^.red := ComputeExpr(RedVars) shr 8;
                pdest^.green := ComputeExpr(GreenVars) shr 8;
                pdest^.blue := ComputeExpr(BlueVars) shr 8;
                pdest^.alpha := ComputeExpr(AlphaVars) shr 8;
              end;
              inc(pdest);
              inc(psrc);
            end else
            if hslMode then
            begin
              if gsba then
                pdest^ := TGSBAPixel.New(
                            ComputeExpr(HueVars, 65536),
                            ComputeExpr(SaturationVars),
                            ComputeExpr(LightnessVars),
                            ComputeExpr(AlphaVars)) else
              if gammaCorr then
                pdest^ := THSLAPixel.New(
                            ComputeExpr(HueVars, 65536),
                            ComputeExpr(SaturationVars),
                            ComputeExpr(LightnessVars),
                            ComputeExpr(AlphaVars))
              else
                pdest^ := TStdHSLA.New(
                            ComputeExpr(HueVars, 65536)*(360/65536),
                            ComputeExpr(SaturationVars)*oneOver65535,
                            ComputeExpr(LightnessVars)*oneOver65535,
                            ComputeExpr(AlphaVars)*oneOver65535);
              inc(pdest);
              inc(psrc);
            end else
            begin
              pdest^ := TLabA.New(
                            ComputeExprF(LVars)*100,
                            ComputeExprF(aVars)*127,
                            ComputeExprF(bVars)*127,
                            ComputeExprF(AlphaVars));
              inc(pdest);
              inc(psrc);
            end;
          end;
        except
          on ex: exception do
          begin
            break;
          end;
        end;
        Inc(FComputedLines);
        if Now-PrevDate > TimeGrain then break;
      end;
    except
      on ex: exception do
      begin

      end;
    end;
    FFilterConnector.PutImage(FComputedImage, rect(0,prevComputedLines,FComputedImage.Width,FComputedLines), True,False);
    if FComputedLines = FFilterConnector.WorkArea.Bottom then
    begin
      FreeAndNil(FComputedImage);
      FComputing := false;
      Button_OK.Enabled := true;
    end;
    Timer1.Interval := 15;
    Timer1.Enabled := True;
  end;
end;

procedure TFFilterFunction.Timer_AdjustVerticalSizeTimer(Sender: TObject);
begin
  PanelLabelRGB.ChildSizing.TopBottomSpacing:= Edit_Red.Top + (Edit_Red.Height - Label_RedEquals.Height) div 2;
  PanelLabelRGB.ChildSizing.VerticalSpacing:= (Edit_Green.Top - Edit_Red.Top) - Label_RedEquals.Height;
  PanelLabelHSL.ChildSizing.TopBottomSpacing := PanelLabelRGB.ChildSizing.TopBottomSpacing;
  PanelLabelHSL.ChildSizing.VerticalSpacing := PanelLabelRGB.ChildSizing.VerticalSpacing;
  PanelLabelLab.ChildSizing.TopBottomSpacing := PanelLabelRGB.ChildSizing.TopBottomSpacing;
  PanelLabelLab.ChildSizing.VerticalSpacing := PanelLabelRGB.ChildSizing.VerticalSpacing;
  PageControl_Color.Height := PanelRGB.Top + Edit_Blue.Top + Edit_Blue.Height +
    TabSheet_RGB.ChildSizing.VerticalSpacing +
    CheckBox_Gamma.Height + TabSheet_RGB.ChildSizing.TopBottomSpacing +
    (PageControl_Color.Height - TabSheet_RGB.Height);
  ClientHeight := PageControl_Color.Top + PageControl_Color.Height +
    Label_Variables.Top + (ClientHeight - Edit_Alpha.Top);
  Timer_AdjustVerticalSize.Enabled := false;
end;

procedure TFFilterFunction.UpdateExpr(AExpr: TFPExpressionParser; AEdit: TEdit; var AError: boolean);
begin
  if AExpr.Expression = Trim(AEdit.Text) then exit;
  try
    AExpr.Expression := ReplaceStats(Trim(AEdit.Text));
    AEdit.Color := clWindow;
    AEdit.Font.Color := clWindowText;
    AError:= length(AExpr.Expression) = 0;
  except
    on ex:exception do
    begin
      AEdit.Color := clRed;
      AEdit.Font.Color := clWhite;
      AError:= true;
    end;
  end;
  if not FInitializing then PreviewNeeded;
end;

procedure TFFilterFunction.PreviewNeeded;
begin
  Timer1.Enabled := False;
  FreeAndNil(FComputedImage);
  FComputing := false;
  Button_OK.Enabled := false;

  if not FAlphaError and not FGreenError and not FBlueError and not FRedError
    and not FHueError and not FSaturationError and not FLightnessError then
  begin
    FComputing := True;
    FComputedLines := 0;
    Timer1.Interval := 200;
    Timer1.Enabled := True;
  end;
end;

procedure TFFilterFunction.InitParams;
begin
  FInitializing:= true;
  Edit_Red.Text := 'red';
  Edit_Green.Text := 'green';
  Edit_Blue.Text := 'blue';
  Edit_Alpha.Text := 'alpha';
  Edit_Hue.Text := 'hue';
  Edit_Saturation.Text := 'saturation';
  Edit_Lightness.Text := 'lightness';
  Edit_L.Text := 'L';
  Edit_a.Text := 'a';
  Edit_b.Text := 'b';
  CheckBox_Gamma.Checked := true;

  if Assigned(FFilterConnector.Parameters) then
    with FFilterConnector.Parameters do
    begin
      if IsDefined('Red') then Edit_Red.Text := Strings['Red'];
      if IsDefined('Green') then Edit_Green.Text := Strings['Green'];
      if IsDefined('Blue') then Edit_Blue.Text := Strings['Blue'];
      if IsDefined('Alpha') then Edit_Alpha.Text := Strings['Alpha'];
      if IsDefined('Hue') then Edit_Hue.Text := Strings['Hue'];
      if IsDefined('Saturation') then Edit_Saturation.Text := Strings['Saturation'];
      if IsDefined('Lightness') then Edit_Lightness.Text := Strings['Lightness'];
      if IsDefined('L') then Edit_Hue.Text := Strings['L'];
      if IsDefined('a') then Edit_Saturation.Text := Strings['a'];
      if IsDefined('b') then Edit_Lightness.Text := Strings['b'];
      if IsDefined('GammaCorrection') then CheckBox_Gamma.Checked:= Booleans['GammaCorrection'];
      if IsDefined('CorrectedHue') then CheckBox_GSBA.Checked:= Booleans['CorrectedHue'];
      if IsDefined('Hue') or IsDefined('Saturation') or IsDefined('Lightness') then
        PageControl_Color.ActivePage := TabSheet_HSL else
      if IsDefined('L') or IsDefined('a') or IsDefined('b') then
        PageControl_Color.ActivePage := TabSheet_Lab;
    end;

  Edit_RedChange(nil);
  Edit_GreenChange(nil);
  Edit_BlueChange(nil);
  Edit_AlphaChange(nil);
  Edit_HueChange(nil);
  Edit_SaturationChange(nil);
  Edit_LightnessChange(nil);
  Edit_LChange(nil);
  Edit_aChange(nil);
  Edit_bChange(nil);
  FInitializing:= false;
end;

function TFFilterFunction.CreateExpr: TFPExpressionParser;
var
  i: Integer;
begin
  result := TFPExpressionParser.Create(nil);
  result.BuiltIns := AllBuiltIns - [bcAggregate];
  result.Identifiers.AddFloatVariable('x',0);
  result.Identifiers.AddFloatVariable('y',0);
  result.Identifiers.AddIntegerVariable('width',1);
  result.Identifiers.AddIntegerVariable('height',1);
  result.Identifiers.AddFloatVariable('red',0);
  result.Identifiers.AddFloatVariable('green',0);
  result.Identifiers.AddFloatVariable('blue',0);
  result.Identifiers.AddFloatVariable('alpha',0);
  result.Identifiers.AddFloatVariable('hue',0);
  result.Identifiers.AddFloatVariable('saturation',0);
  result.Identifiers.AddFloatVariable('lightness',0);
  result.Identifiers.AddFloatVariable('L',0);
  result.Identifiers.AddFloatVariable('a',0);
  result.Identifiers.AddFloatVariable('b',0);
  result.Identifiers.AddFloatVariable('random',0);
  result.Identifiers.AddFunction('min', 'F', 'FF', @ExprFunctionMin_Call);
  result.Identifiers.AddFunction('max', 'F', 'FF', @ExprFunctionMax_Call);
  for i := low(StatsName) to high(StatsName) do
  begin
    result.Identifiers.AddFloatVariable('min_'+StatsName[i],0);
    result.Identifiers.AddFloatVariable('max_'+StatsName[i],0);
    result.Identifiers.AddFloatVariable('avg_'+StatsName[i],0);
  end;
end;

function TFFilterFunction.ExprResultToFloat(const AResult: TFPExpressionResult): single;
var {%H-}code: integer;
begin
  case AResult.ResultType of
  rtFloat: result := AResult.ResFloat;
  rtInteger: result := AResult.ResInteger;
  rtBoolean: if AResult.ResBoolean then result := 1 else result := 0;
  rtDateTime: result := 0;
  rtString: val(AResult.ResString, result, code);
  else result := 0;
  end;
end;

procedure TFFilterFunction.ExprFunctionMin_Call(
  var Result: TFPExpressionResult; const Args: TExprParameterArray);
begin
  result.ResultType:= rtFloat;
  result.ResFloat := min(ExprResultToFloat(Args[0]), ExprResultToFloat(Args[1]));
end;

procedure TFFilterFunction.ExprFunctionMax_Call(
  var Result: TFPExpressionResult; const Args: TExprParameterArray);
begin
  result.ResultType:= rtFloat;
  result.ResFloat := max(ExprResultToFloat(Args[0]), ExprResultToFloat(Args[1]));
end;

procedure TFFilterFunction.StatsNotComputed(AFrom,ATo: integer);
var i: integer;
begin
  for i := AFrom to ATo do
    FStats[i].computed := false;
end;

procedure TFFilterFunction.NeedStats(AStatIndex: integer);
const
  oneOver255 = 1/255;
  oneOver65535 = 1/65535;
  oneOver65536 = 1/65536;

  procedure AggregateStat(AIndex: integer; AValue: single);
  begin
    FStats[AIndex].min := min(FStats[AIndex].min, AValue);
    FStats[AIndex].max := max(FStats[AIndex].max, AValue);
    FStats[AIndex].sum += AValue;
    inc(FStats[AIndex].count);
  end;

  procedure StartCompute(AFrom,ATo: integer);
  var
    i: Integer;
  begin
    for i := AFrom to ATo do
    begin
      FStats[i].min := 1;
      FStats[i].max := 0;
      FStats[i].sum := 0;
      FStats[i].avg := 0;
      FStats[i].count := 0;
    end;
  end;

  procedure EndCompute(AFrom,ATo: integer);
  var
    i: Integer;
  begin
    for i := AFrom to ATo do
    begin
      if FStats[i].count > 0 then
        FStats[i].avg := FStats[i].sum / FStats[i].count;
      FStats[i].computed:= true;
    end;
  end;

var
  y, x: LongInt;
  p: PBGRAPixel;
  gammaCorr, gsba: Boolean;
  ec: TExpandedPixel;
  r,g,b,h,s,l: single;

begin
  if not FStats[AStatIndex].computed then
  begin
    gammaCorr := CheckBox_Gamma.Checked;
    gsba := CheckBox_GSBA.Checked;
    if (AStatIndex >= 1) and (AStatIndex <= 4) then
    begin
      StartCompute(1,4);
      for y := FFilterConnector.WorkArea.Top to FFilterConnector.WorkArea.Bottom-1 do
      begin
        p := FFilterConnector.BackupLayer.ScanLine[y] + FFilterConnector.WorkArea.Left;
        for x := FFilterConnector.WorkArea.Left to FFilterConnector.WorkArea.Right-1 do
        begin
          if p^.alpha > 0 then
          begin
            if gammaCorr then
            begin
              ec := p^.ToExpanded;
              r := ec.red*oneOver65535;
              g := ec.green*oneOver65535;
              b := ec.blue*oneOver65535;
            end else
            begin
              r := p^.red*oneOver255;
              g := p^.green*oneOver255;
              b := p^.blue*oneOver255;
            end;
            AggregateStat(1, r);
            AggregateStat(2, g);
            AggregateStat(3, b);
          end;
          AggregateStat(4, p^.blue*oneOver255);
          inc(p);
        end;
      end;
      EndCompute(1,4);
    end else
    if (AStatIndex >= 5) and (AStatIndex <= 7) then
    begin
      StartCompute(5,7);
      for y := FFilterConnector.WorkArea.Top to FFilterConnector.WorkArea.Bottom-1 do
      begin
        p := FFilterConnector.BackupLayer.ScanLine[y] + FFilterConnector.WorkArea.Left;
        for x := FFilterConnector.WorkArea.Left to FFilterConnector.WorkArea.Right-1 do
        begin
          if p^.alpha > 0 then
          begin
            if gsba then
            with p^.ToGSBAPixel do
            begin
              h := hue*oneOver65536;
              s := saturation*oneOver65535;
              l := lightness*oneOver65535;
            end else
            if gammaCorr then
            with p^.ToHSLAPixel do
            begin
              h := hue*oneOver65536;
              s := saturation*oneOver65535;
              l := lightness*oneOver65535;
            end else
            with p^.ToStdHSLA do
            begin
              h := hue/360;
              s := saturation;
              l := lightness;
            end;
            AggregateStat(5, h);
            AggregateStat(6, s);
            AggregateStat(7, l);
          end;
          inc(p);
        end;
      end;
      EndCompute(5,7);
    end else
    if (AStatIndex >= 8) and (AStatIndex <= 10) then
    begin
      StartCompute(8,10);
      for y := FFilterConnector.WorkArea.Top to FFilterConnector.WorkArea.Bottom-1 do
      begin
        p := FFilterConnector.BackupLayer.ScanLine[y] + FFilterConnector.WorkArea.Left;
        for x := FFilterConnector.WorkArea.Left to FFilterConnector.WorkArea.Right-1 do
        begin
          if p^.alpha > 0 then
            with p^.ToLabA do
            begin
              AggregateStat(8, L/100);
              AggregateStat(9, a/127);
              AggregateStat(10, b/127);
            end;
          inc(p);
        end;
      end;
      EndCompute(8,10);
    end;
  end;
end;

function TFFilterFunction.ReplaceStats(AExpr: string): string;
var
  i: Integer;
begin
  result := AExpr;
  for i := low(StatsName) to high(StatsName) do
  begin
    result := ReplaceIdentifier(result, 'min('+StatsName[i]+')','min_'+StatsName[i]);
    result := ReplaceIdentifier(result, 'max('+StatsName[i]+')','max_'+StatsName[i]);
    result := ReplaceIdentifier(result, 'avg('+StatsName[i]+')','avg_'+StatsName[i]);
  end;
end;

procedure TFFilterFunction.Button_OKClick(Sender: TObject);
begin
  FFilterConnector.ValidateAction;
  ModalResult := mrOK;
end;

procedure TFFilterFunction.CheckBox_GammaChange(Sender: TObject);
begin
  if not FInitializing then
  begin
    StatsNotComputed(1,3);
    if not CheckBox_GSBA.Checked then StatsNotComputed(5,7);
    PreviewNeeded;
  end;
end;

procedure TFFilterFunction.CheckBox_GSBAChange(Sender: TObject);
begin
  if not FInitializing then
  begin
    StatsNotComputed(5,7);
    PreviewNeeded;
  end;
end;

procedure TFFilterFunction.Edit_aChange(Sender: TObject);
begin
  UpdateExpr(FaExpr,Edit_a,FaError);
end;

procedure TFFilterFunction.Button_CancelClick(Sender: TObject);
begin
  Timer1.Enabled:= false;
end;

procedure TFFilterFunction.Edit_AlphaChange(Sender: TObject);
begin
  UpdateExpr(FAlphaExpr,Edit_Alpha,FAlphaError);
end;

procedure TFFilterFunction.Edit_bChange(Sender: TObject);
begin
  UpdateExpr(FbExpr,Edit_b,FbError);
end;

procedure TFFilterFunction.Edit_BlueChange(Sender: TObject);
begin
  UpdateExpr(FBlueExpr,Edit_Blue,FBlueError);
end;

procedure TFFilterFunction.Edit_GreenChange(Sender: TObject);
begin
  UpdateExpr(FGreenExpr,Edit_Green,FGreenError);
end;

procedure TFFilterFunction.Edit_HueChange(Sender: TObject);
begin
  UpdateExpr(FHueExpr,Edit_Hue,FHueError);
end;

procedure TFFilterFunction.Edit_LChange(Sender: TObject);
begin
  UpdateExpr(FLExpr,Edit_L,FLError);
end;

procedure TFFilterFunction.Edit_LightnessChange(Sender: TObject);
begin
  UpdateExpr(FLightnessExpr,Edit_Lightness,FLightnessError);
end;

procedure TFFilterFunction.Edit_RedChange(Sender: TObject);
begin
  UpdateExpr(FRedExpr,Edit_Red,FRedError);
end;

procedure TFFilterFunction.Edit_SaturationChange(Sender: TObject);
begin
  UpdateExpr(FSaturationExpr,Edit_Saturation,FSaturationError);
end;

{$R *.lfm}

initialization

  randomize;

end.

