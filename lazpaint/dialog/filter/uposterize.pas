// SPDX-License-Identifier: GPL-3.0-only
unit uposterize;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Spin, ExtCtrls, UFilterConnector, UScripting, LazPaintType;

type

  { TFPosterize }

  TFPosterize = class(TForm)
    Button_Cancel: TButton;
    Button_OK: TButton;
    CheckBox_ByLightness: TCheckBox;
    Label_Levels: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    SpinEdit_Levels: TSpinEdit;
    procedure Button_OKClick(Sender: TObject);
    procedure CheckBox_ByLightnessChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpinEdit_LevelsChange(Sender: TObject);
  private
    FInitializing: boolean;
    FFilterConnector: TFilterConnector;
    procedure OnTryStopAction({%H-}sender: TFilterConnector);
    procedure InitParams;
    procedure PreviewNeeded;
    { private declarations }
  public
    { public declarations }
  end;

function ShowPosterizeDlg(AInstance: TLazPaintCustomInstance; AParameters: TVariableSet): TScriptResult;

implementation

uses BGRABitmapTypes, LCScaleDPI, UMac, UColorFilters, math;

function ShowPosterizeDlg(AInstance: TLazPaintCustomInstance; AParameters: TVariableSet): TScriptResult;
var FPosterize: TFPosterize;
  topmostInfo: TTopMostInfo;
begin
  FPosterize := TFPosterize.Create(nil);
  try
    FPosterize.FFilterConnector := TFilterConnector.Create(AInstance, AParameters, false);
    FPosterize.FFilterConnector.OnTryStopAction := @FPosterize.OnTryStopAction;
  except
    on ex: exception do
    begin
      AInstance.ShowError('ShowPosterizeDlg',ex.Message);
      result := srException;
      exit;
    end;
  end;
  topmostInfo := AInstance.HideTopmost;
  try
    if Assigned(FPosterize.FFilterConnector.Parameters) and
       FPosterize.FFilterConnector.Parameters.Booleans['Validate'] then
    begin
      FPosterize.InitParams;
      FPosterize.PreviewNeeded;
      FPosterize.FFilterConnector.ValidateAction;
      result := srOk;
    end else
    begin
      if FPosterize.ShowModal = mrOK then
        result := srOk
      else
        result := srCancelledByUser;
    end;
  finally
    AInstance.ShowTopmost(topmostInfo);
    FPosterize.FFilterConnector.OnTryStopAction := nil;
    FreeAndNil(FPosterize.FFilterConnector);
    FPosterize.Free;
  end;
end;

{ TFPosterize }

procedure TFPosterize.FormCreate(Sender: TObject);
begin
  ScaleControl(Self,OriginalDPI);

  CheckOKCancelBtns(Button_OK{,Button_Cancel});
  CheckSpinEdit(SpinEdit_Levels);
end;

procedure TFPosterize.Button_OKClick(Sender: TObject);
begin
  FFilterConnector.ValidateAction;
  FFilterConnector.LazPaintInstance.Config.SetDefaultPosterizeLevels(SpinEdit_Levels.Value);
  FFilterConnector.LazPaintInstance.Config.SetDefaultPosterizeByLightness(CheckBox_ByLightness.Checked);
  ModalResult := mrOK;
end;

procedure TFPosterize.CheckBox_ByLightnessChange(Sender: TObject);
begin
  if not FInitializing then PreviewNeeded;
end;

procedure TFPosterize.FormShow(Sender: TObject);
begin
  InitParams;
  PreviewNeeded;
  Top := FFilterConnector.LazPaintInstance.MainFormBounds.Top;
end;

procedure TFPosterize.SpinEdit_LevelsChange(Sender: TObject);
begin
  if not FInitializing then PreviewNeeded;
end;

procedure TFPosterize.OnTryStopAction(sender: TFilterConnector);
begin
  if self.visible then Close;
end;

procedure TFPosterize.InitParams;
begin
  FInitializing := true;
  if Assigned(FFilterConnector.Parameters) and
     FFilterConnector.Parameters.IsDefined('Levels') then
    SpinEdit_Levels.Value := FFilterConnector.Parameters.Integers['Levels']
  else
    SpinEdit_Levels.Value := FFilterConnector.LazPaintInstance.Config.DefaultPosterizeLevels;
  if Assigned(FFilterConnector.Parameters) and
     FFilterConnector.Parameters.IsDefined('ByLightness') then
    CheckBox_ByLightness.Checked := FFilterConnector.Parameters.Booleans['ByLightness']
  else
    CheckBox_ByLightness.Checked := FFilterConnector.LazPaintInstance.Config.DefaultPosterizeByLightness;
  FInitializing := false;
end;

procedure TFPosterize.PreviewNeeded;
var params:TVariableSet;
  levels: integer;

  procedure AddPosterize(AChannel :string);
  var
    pointList: TScriptVariableReference;
    i: integer;
  begin
    with params.AddSubset(AChannel) do
    begin
      Booleans['Posterize'] := true;
      pointList := AddPointList('Points');
      for i := 0 to levels-1 do
        AppendPoint(pointList, PointF(i/levels, i/(levels-1)));
    end;
  end;

begin
  levels := min(SpinEdit_Levels.Value, SpinEdit_Levels.MaxValue);
  params := TVariableSet.Create('');
  if CheckBox_ByLightness.Checked then
    AddPosterize('Lightness') else
  begin
    AddPosterize('Red');
    AddPosterize('Green');
    AddPosterize('Blue');
  end;
  FilterAdjustCurves(FFilterConnector, params);
  params.Free;
end;

{$R *.lfm}

end.

