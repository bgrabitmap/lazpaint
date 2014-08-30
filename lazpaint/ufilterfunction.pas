unit UFilterFunction;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, fpexprpars, UFilterConnector, BGRABitmap,
  BGRABitmapTypes;

type

  { TFFilterFunction }

  TFFilterFunction = class(TForm)
    Button_Cancel: TButton;
    Button_OK: TButton;
    CheckBox_GSBA: TCheckBox;
    Edit_Alpha: TEdit;
    Edit_Blue: TEdit;
    Edit_Lightness: TEdit;
    Edit_Green: TEdit;
    Edit_Saturation: TEdit;
    Edit_Red: TEdit;
    Edit_Hue: TEdit;
    Label_Variables: TLabel;
    Label_AlphaEquals: TLabel;
    Label_BlueEquals: TLabel;
    Label_LightnessEquals: TLabel;
    Label_GreenEquals: TLabel;
    Label_SaturationEquals: TLabel;
    Label_RedEquals: TLabel;
    Label_HueEquals: TLabel;
    PageControl_Color: TPageControl;
    TabSheet_RGB: TTabSheet;
    TabSheet_HSL: TTabSheet;
    Timer1: TTimer;
    procedure Button_CancelClick(Sender: TObject);
    procedure Button_OKClick(Sender: TObject);
    procedure CheckBox_GSBAChange(Sender: TObject);
    procedure Edit_AlphaChange(Sender: TObject);
    procedure Edit_BlueChange(Sender: TObject);
    procedure Edit_GreenChange(Sender: TObject);
    procedure Edit_HueChange(Sender: TObject);
    procedure Edit_LightnessChange(Sender: TObject);
    procedure Edit_RedChange(Sender: TObject);
    procedure Edit_SaturationChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PageControl_ColorChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    FRedExpr,FGreenExpr,FBlueExpr,FAlphaExpr,FHueExpr,FSaturationExpr,FLightnessExpr: TFPExpressionParser;
    FRedError,FGreenError,FBlueError,FAlphaError,FHueError,FSaturationError,FLightnessError: boolean;
    FExprChanged: boolean;
    FComputing: boolean;
    FComputedImage: TBGRABitmap;
    FComputedLines: integer;
    FFilterConnector: TFilterConnector;
    FInitializing: boolean;
    procedure UpdateExpr(AExpr: TFPExpressionParser; AEdit: TEdit;
      var AError: boolean);
    procedure PreviewNeeded;
    function CreateExpr: TFPExpressionParser;
  public
    { public declarations }
  end;

function ShowFilterFunctionDlg(AFilterConnector: TObject):boolean;

implementation

uses UScaleDPI, UMac, LazPaintType;

function ShowFilterFunctionDlg(AFilterConnector: TObject): boolean;
var
  FFilterFunction: TFFilterFunction;
begin
  result := false;
  FFilterFunction:= TFFilterFunction.create(nil);
  FFilterFunction.FFilterConnector := AFilterConnector as TFilterConnector;
  try
    if FFilterFunction.FFilterConnector.ActiveLayer <> nil then
      result:= (FFilterFunction.showModal = mrOk)
    else
      result := false;
  finally
    FFilterFunction.free;
  end;
end;

{ TFFilterFunction }

procedure TFFilterFunction.FormCreate(Sender: TObject);
begin
  ScaleDPI(Self,OriginalDPI);

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

  FInitializing := true;
  Edit_Red.Text := 'red';
  Edit_RedChange(nil);
  Edit_Green.Text := 'green';
  Edit_GreenChange(nil);
  Edit_Blue.Text := 'blue';
  Edit_BlueChange(nil);
  Edit_Alpha.Text := 'alpha';
  Edit_AlphaChange(nil);
  Edit_Hue.Text := 'hue';
  Edit_HueChange(nil);
  Edit_Saturation.Text := 'saturation';
  Edit_SaturationChange(nil);
  Edit_Lightness.Text := 'lightness';
  Edit_LightnessChange(nil);
  Label_RedEquals.Caption := 'red =';
  Label_GreenEquals.Caption := 'green =';
  Label_BlueEquals.Caption := 'blue =';
  Label_AlphaEquals.Caption := 'alpha =';
  Label_HueEquals.Caption := 'hue =';
  Label_SaturationEquals.Caption := 'saturation =';
  Label_LightnessEquals.Caption := 'lightness =';

  FExprChanged:= false;
  Label_Variables.Caption := Label_Variables.Caption+' x,y,width,height,random';
  FInitializing := false;
end;

procedure TFFilterFunction.FormDestroy(Sender: TObject);
begin
  FComputedImage.Free;
  FRedExpr.Free;
  FGreenExpr.Free;
  FBlueExpr.Free;
  FAlphaExpr.Free;
  FHueExpr.Free;
  FSaturationExpr.Free;
  FLightnessExpr.Free;
end;

procedure TFFilterFunction.PageControl_ColorChange(Sender: TObject);
begin
  if not FInitializing and FExprChanged then PreviewNeeded;
end;

procedure TFFilterFunction.Timer1Timer(Sender: TObject);
const
  TimeGrain = 100/1000/60/60/24;
  oneOver255 = 1/255;
  oneOver65535 = 1/65535;
  oneOver65536 = 1/65536;
type
  TExprValues = record
    Red,Green,Blue,Alpha,Hue,Saturation,Lightness: TExprFloat;
  end;

  TExprVariables = record
    XValue,YValue,RedValue,GreenValue,BlueValue,AlphaValue,
    HueValue,SaturationValue,LightnessValue,RandomValue: TFPExprIdentifierDef;
    IsIdentity,RandomUsed: boolean;
  end;

var
  rgbUsed,hslUsed: boolean;

  function ContainsIdentifier(AExpr: string; AVar: string): boolean;
  var i: integer;
  begin
    result := false;
    if (AExpr = '') or (AVar = '') then exit;
    for i := 1 to length(AExpr)-length(AVar)+1 do
    if UpCase(AExpr[i]) = UpCase(AVar[1]) then
    begin
      if (i = 1) or not (AExpr[i-1] in['a'..'z','A'..'Z']) then
      begin
        if (i+length(AVar) = length(AExpr)+1) or
          not (AExpr[i+length(AVar)] in ['a'..'z','A'..'Z']) then
        begin
          if CompareText(copy(AExpr,i,length(AVar)), AVar) = 0 then
          begin
            result := true;
            exit;
          end;
        end;
      end;
    end;
  end;

  procedure PrepareXY(AExpr: TFPExpressionParser; out AVars: TExprVariables; AIdentityExpr: string);
  begin
    with AVars do
    begin
      IsIdentity := (CompareText(trim(AExpr.Expression), AIdentityExpr)= 0);
      if ContainsIdentifier(AExpr.Expression,'hue') or
         ContainsIdentifier(AExpr.Expression,'saturation') or
         ContainsIdentifier(AExpr.Expression,'lightness') then hslUsed:= true;
      if ContainsIdentifier(AExpr.Expression,'red') or
         ContainsIdentifier(AExpr.Expression,'green') or
         ContainsIdentifier(AExpr.Expression,'blue') then rgbUsed:= true;
      RandomUsed:= ContainsIdentifier(AExpr.Expression,'random');
      XValue := AExpr.IdentifierByName('x');
      YValue:= AExpr.IdentifierByName('y');
      RedValue:= AExpr.IdentifierByName('red');
      GreenValue:= AExpr.IdentifierByName('green');
      BlueValue:= AExpr.IdentifierByName('blue');
      AlphaValue:= AExpr.IdentifierByName('alpha');
      HueValue:= AExpr.IdentifierByName('hue');
      SaturationValue:= AExpr.IdentifierByName('saturation');
      LightnessValue:= AExpr.IdentifierByName('lightness');
      RandomValue:= AExpr.IdentifierByName('random');
      AExpr.IdentifierByName('width').AsInteger := FFilterConnector.BackupLayer.Width;
      AExpr.IdentifierByName('height').AsInteger := FFilterConnector.BackupLayer.Height;
    end;
  end;

  function ComputeExpr(AExpr: TFPExpressionParser; AVars: TExprVariables; AX,AY: Integer; const AValues: TExprValues): integer;
  var {%H-}code: integer;
    floatValue: single;
  begin
    AVars.XValue.AsFloat := AX/FFilterConnector.BackupLayer.Width;
    AVars.YValue.AsFloat := AY/FFilterConnector.BackupLayer.Height;
    if rgbUsed then
    begin
      AVars.RedValue.AsFloat := AValues.Red;
      AVars.GreenValue.AsFloat := AValues.Green;
      AVars.BlueValue.AsFloat := AValues.Blue;
    end;
    AVars.AlphaValue.AsFloat := AValues.Alpha;
    if hslUsed then
    begin
      AVars.HueValue.AsFloat := AValues.Hue;
      AVars.SaturationValue.AsFloat := AValues.Saturation;
      AVars.LightnessValue.AsFloat := AValues.Lightness;
    end;
    if AVars.RandomUsed then AVars.RandomValue.AsFloat := Random;
    with AExpr.Evaluate do
    begin
      case ResultType of
      rtFloat: if resFloat < 0 then result := 0 else
          if resFloat > 1 then result := 65535 else
              result := round(ResFloat*65535);
      rtInteger: if ResInteger <= 0 then result := 0 else result := 65535;
      rtBoolean: if ResBoolean then result := 65535 else result := 0;
      rtDateTime: result := 0;
      rtString: begin
                  val(ResString, floatValue, code);
                  if floatValue < 0 then result := 0 else
                  if floatValue > 1 then result := 65535 else
                    result := round(floatValue*65535);
                end;
      else result := 0;
      end;
    end;
  end;

var PrevDate: TDateTime;
  x,y,xcount: integer;
  pdest,psrc: PBGRAPixel;
  RedVars,GreenVars,BlueVars,AlphaVars,HueVars,SaturationVars,LightnessVars: TExprVariables;
  prevComputedLines: integer;
  values: TExprValues;
  gsba,rgbMode: boolean;
  srcHslaValue,hslaValue: THSLAPixel;

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
    PrevDate := Now;
    prevComputedLines:= FComputedLines;
    try
      rgbMode := PageControl_Color.ActivePage = TabSheet_RGB;
      hslUsed := false;
      rgbUsed:= false;
      if rgbMode then
      begin
        PrepareXY(FRedExpr,RedVars,'red');
        PrepareXY(FGreenExpr,GreenVars,'green');
        PrepareXY(FBlueExpr,BlueVars,'blue');
      end else
      begin
        PrepareXY(FHueExpr,HueVars,'hue');
        PrepareXY(FSaturationExpr,SaturationVars,'saturation');
        PrepareXY(FLightnessExpr,LightnessVars,'lightness');
      end;
      PrepareXY(FAlphaExpr,AlphaVars,'alpha');
      while FComputedLines < FFilterConnector.WorkArea.Bottom do
      begin
        y := FComputedLines;
        psrc := FFilterConnector.BackupLayer.ScanLine[y]+FFilterConnector.WorkArea.Left;
        pdest := FComputedImage.ScanLine[y]+FFilterConnector.WorkArea.Left;
        xcount := FFilterConnector.WorkArea.Right - FFilterConnector.WorkArea.Left;
        try
          if rgbMode then
          begin
            for x := 0 to xcount-1 do
            begin
              if gsba then
              begin
                if rgbUsed then
                with GammaExpansion(psrc^) do
                begin
                  values.Red := red *oneOver65535;
                  values.Green := green *oneOver65535;
                  values.Blue := blue *oneOver65535;
                  values.Alpha := alpha *oneOver65535;
                end else
                  values.Alpha := psrc^.alpha *oneOver255;

                if hslUsed then
                with BGRAToGSBA(psrc^) do
                begin
                  values.Hue := hue*oneOver65536;
                  values.Saturation := saturation*oneOver65535;
                  values.Lightness := lightness*oneOver65535;
                end;
                if RedVars.IsIdentity then pdest^.red := psrc^.red else pdest^.red := GammaCompressionTab[ComputeExpr(FRedExpr,RedVars,x,y,values)];
                if GreenVars.IsIdentity then pdest^.green := psrc^.green else pdest^.green := GammaCompressionTab[ComputeExpr(FGreenExpr,GreenVars,x,y,values)];
                if BlueVars.IsIdentity then pdest^.blue := psrc^.blue else pdest^.blue := GammaCompressionTab[ComputeExpr(FBlueExpr,BlueVars,x,y,values)];
                if AlphaVars.IsIdentity then pdest^.alpha := psrc^.alpha else pdest^.alpha := GammaCompressionTab[ComputeExpr(FAlphaExpr,AlphaVars,x,y,values)];
              end else
              begin
                if rgbUsed then
                with psrc^ do
                begin
                  values.Red := red *oneOver255;
                  values.Green := green *oneOver255;
                  values.Blue := blue *oneOver255;
                  values.Alpha := alpha *oneOver255;
                end else
                  values.Alpha := psrc^.alpha *oneOver255;

                if hslUsed then
                with BGRAToHSLA(psrc^) do
                begin
                  values.Hue := hue*oneOver65536;
                  values.Saturation := saturation*oneOver65535;
                  values.Lightness := lightness*oneOver65535;
                end;
                if RedVars.IsIdentity then pdest^.red := psrc^.red else pdest^.red := ComputeExpr(FRedExpr,RedVars,x,y,values) shr 8;
                if GreenVars.IsIdentity then pdest^.green := psrc^.green else pdest^.green := ComputeExpr(FGreenExpr,GreenVars,x,y,values) shr 8;
                if BlueVars.IsIdentity then pdest^.blue := psrc^.blue else pdest^.blue := ComputeExpr(FBlueExpr,BlueVars,x,y,values) shr 8;
                if AlphaVars.IsIdentity then pdest^.alpha := psrc^.alpha else pdest^.alpha := ComputeExpr(FAlphaExpr,AlphaVars,x,y,values) shr 8;
              end;
              inc(pdest);
              inc(psrc);
            end;
          end else
          begin
            for x := 0 to xcount-1 do
            begin
              if gsba then
              begin
                if rgbUsed then
                with GammaExpansion(psrc^) do
                begin
                  values.Red := red *oneOver65535;
                  values.Green := green *oneOver65535;
                  values.Blue := blue *oneOver65535;
                  values.Alpha := alpha *oneOver65535;
                end else
                  values.Alpha := psrc^.alpha *oneOver255;

                srcHslaValue := BGRAToGSBA(psrc^);
                with srcHslaValue do
                begin
                  values.Hue := hue*oneOver65536;
                  values.Saturation := saturation*oneOver65535;
                  values.Lightness := lightness*oneOver65535;
                end;
                if HueVars.IsIdentity then hslaValue.hue := srcHslaValue.hue else hslaValue.hue := ComputeExpr(FHueExpr,HueVars,x,y,values);
                if SaturationVars.IsIdentity then hslaValue.saturation := srcHslaValue.saturation else hslaValue.saturation := ComputeExpr(FSaturationExpr,SaturationVars,x,y,values);
                if LightnessVars.IsIdentity then hslaValue.lightness := srcHslaValue.lightness else hslaValue.lightness := ComputeExpr(FLightnessExpr,LightnessVars,x,y,values);
                if AlphaVars.IsIdentity then hslaValue.alpha := srcHslaValue.alpha else hslaValue.alpha := ComputeExpr(FAlphaExpr,AlphaVars,x,y,values);
                pdest^ := GSBAToBGRA(hslaValue);
              end else
              begin
                if rgbUsed then
                with psrc^ do
                begin
                  values.Red := red *oneOver255;
                  values.Green := green *oneOver255;
                  values.Blue := blue *oneOver255;
                  values.Alpha := alpha *oneOver255;
                end else
                  values.Alpha := psrc^.alpha *oneOver255;

                srcHslaValue := BGRAToHSLA(psrc^);
                with srcHslaValue do
                begin
                  values.Hue := hue*oneOver65536;
                  values.Saturation := saturation*oneOver65535;
                  values.Lightness := lightness*oneOver65535;
                end;
                if HueVars.IsIdentity then hslaValue.hue := srcHslaValue.hue else hslaValue.hue := ComputeExpr(FHueExpr,HueVars,x,y,values);
                if SaturationVars.IsIdentity then hslaValue.saturation := srcHslaValue.saturation else hslaValue.saturation := ComputeExpr(FSaturationExpr,SaturationVars,x,y,values);
                if LightnessVars.IsIdentity then hslaValue.lightness := srcHslaValue.lightness else hslaValue.lightness := ComputeExpr(FLightnessExpr,LightnessVars,x,y,values);
                if AlphaVars.IsIdentity then hslaValue.alpha := srcHslaValue.alpha else hslaValue.alpha := ComputeExpr(FAlphaExpr,AlphaVars,x,y,values);
                pdest^ := HSLAToBGRA(hslaValue);
              end;
              inc(pdest);
              inc(psrc);
            end;
          end;
        except
          on ex: exception do
          begin
            //nothing
          end;
        end;
        Inc(FComputedLines);
        if Now-PrevDate > TimeGrain then break;
      end;
      Timer1.Interval := 5;
      Timer1.Enabled := True;
    except
      on ex: exception do
      begin

      end;
    end;
    FFilterConnector.PutImage(FComputedImage, rect(0,prevComputedLines,FComputedImage.Width,FComputedLines), True,False);
    if FComputedLines = FComputedImage.Height then
    begin
      FreeAndNil(FComputedImage);
      FComputing := false;
      Button_OK.Enabled := true;
    end;
  end;
end;

procedure TFFilterFunction.UpdateExpr(AExpr: TFPExpressionParser; AEdit: TEdit; var AError: boolean);
begin
  if AExpr.Expression = Trim(AEdit.Text) then exit;
  FExprChanged:= true;
  try
    AExpr.Expression := Trim(AEdit.Text);
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

  if not FAlphaError and not FGreenError and not FBlueError and not FRedError then
  begin
    FComputing := True;
    FComputedLines := 0;
    Timer1.Interval := 200;
    Timer1.Enabled := True;
  end;
end;

function TFFilterFunction.CreateExpr: TFPExpressionParser;
begin
  result := TFPExpressionParser.Create(nil);
  result.BuiltIns := AllBuiltIns;
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
  result.Identifiers.AddFloatVariable('random',0);
end;

procedure TFFilterFunction.Button_OKClick(Sender: TObject);
begin
  FFilterConnector.ValidateAction;
  ModalResult := mrOK;
end;

procedure TFFilterFunction.CheckBox_GSBAChange(Sender: TObject);
begin
  if not FInitializing and FExprChanged then PreviewNeeded;
end;

procedure TFFilterFunction.Button_CancelClick(Sender: TObject);
begin
  Timer1.Enabled:= false;
end;

procedure TFFilterFunction.Edit_AlphaChange(Sender: TObject);
begin
  UpdateExpr(FAlphaExpr,Edit_Alpha,FAlphaError);
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

initialization
  {$I ufilterfunction.lrs}

  randomize;

end.

