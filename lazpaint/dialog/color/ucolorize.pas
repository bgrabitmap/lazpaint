// SPDX-License-Identifier: GPL-3.0-only
unit UColorize;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, Spin, ExtCtrls, BGRABitmap, LCScaleDPI, lazpainttype,
  ufilterconnector, uscripting;

type

  { TFColorize }

  TFColorize = class(TForm)
    Button_Cancel: TButton;
    Button_OK: TButton;
    CheckBox_GSBA: TCheckBox;
    Combo_Preset: TComboBox;
    FloatSpinEdit_Hue: TFloatSpinEdit;
    FloatSpinEdit_Saturation: TFloatSpinEdit;
    Label_Hue: TLabel;
    Label_Colorness: TLabel;
    Label_Preset: TLabel;
    Panel1: TPanel;
    ToolBar_AddRemove: TToolBar;
    ToolButton_Remove: TToolButton;
    ToolButton_Add: TToolButton;
    TrackBar_Hue: TTrackBar;
    TrackBar_Saturation: TTrackBar;
    procedure Button_OKClick(Sender: TObject);
    procedure CheckBox_GSBAChange(Sender: TObject);
    procedure Combo_PresetChange(Sender: TObject);
    procedure FloatSpinEdit_HueChange(Sender: TObject);
    procedure FloatSpinEdit_SaturationChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ToolButton_RemoveClick(Sender: TObject);
    procedure ToolButton_AddClick(Sender: TObject);
    procedure TrackBar_Change(Sender: TObject);
  private
    { private declarations }
    FInitialized: boolean;
    FInstance: TLazPaintCustomInstance;
    FFilterConnector: TFilterConnector;
    FUpdatingSpinEdit,FInComboPreset: boolean;
    FSelectedPresetName: string;
    procedure AdjustLabels(ALabel1, ALabel2: TLabel; ATrack1, ATrack2: TTrackBar);
    function GetChosenHue: Word;
    function GetChosenHueDegF: double;
    function GetChosenSatF: double;
    function GetChosenSaturation: Word;
    procedure OnTryStopAction({%H-}sender: TFilterConnector);
    procedure SetChosenHue(AValue: Word);
    procedure SetChosenHueDegF(AValue: double);
    procedure SetChosenSatF(AValue: double);
    procedure SetChosenSaturation(AValue: Word);
    procedure UpdateSpinEdit;
    procedure LoadParameters(AParams: TVariableSet);
    procedure UpdateComboPreset(AIndex: integer);
  public
    { public declarations }
    function ShowModal: integer; override;
    function ShowModal(AInstance: TLazPaintCustomInstance; AParameters: TVariableSet): integer;
    procedure ApplyChosenColor;
    property ChosenHue: Word read GetChosenHue write SetChosenHue;
    property ChosenSaturation: Word read GetChosenSaturation write SetChosenSaturation;
    property ChosenHueDegF: double read GetChosenHueDegF write SetChosenHueDegF;
    property ChosenSatF: double read GetChosenSatF write SetChosenSatF;
  end;

implementation

uses umac, BGRABitmapTypes, uresourcestrings, UColorFilters;

{ TFColorize }

procedure TFColorize.FormCreate(Sender: TObject);
begin
  ScaleControl(Self,OriginalDPI);

  CheckOKCancelBtns(Button_OK,Button_Cancel);
  CheckFloatSpinEdit(FloatSpinEdit_Hue);
  CheckFloatSpinEdit(FloatSpinEdit_Saturation);
  TrackBar_Hue.Min := 0;
  TrackBar_Hue.Max := TrackBar_Hue.Width and not 3;
  TrackBar_Hue.Position := TrackBar_Hue.Max div 2;
  TrackBar_Hue.Frequency := TrackBar_Hue.Max div 4;
  TrackBar_Saturation.Max := TrackBar_Saturation.Width and not 3;
  TrackBar_Saturation.Position := TrackBar_Saturation.Max div 2;
  TrackBar_Saturation.Frequency := TrackBar_Saturation.Max div 4;
  FInitialized := true;
  UpdateSpinEdit;
end;

procedure TFColorize.Button_OKClick(Sender: TObject);
begin
  ApplyChosenColor;
  FFilterConnector.ValidateAction;
  FFilterConnector.Parameters.Floats['Hue'] := ChosenHueDegF;
  FFilterConnector.Parameters.Floats['Saturation'] := ChosenSatF;
  FFilterConnector.Parameters.Booleans['Correction'] := CheckBox_GSBA.Checked;
end;

procedure TFColorize.CheckBox_GSBAChange(Sender: TObject);
begin
  if FInitialized and Visible then
  begin
    Combo_Preset.ItemIndex := -1;
    ApplyChosenColor;
  end;
end;

procedure TFColorize.Combo_PresetChange(Sender: TObject);
begin
  if FInComboPreset then exit;
  if Combo_Preset.ItemIndex <> -1 then
  begin
    FSelectedPresetName := FInstance.Config.ColorizePreset[Combo_Preset.ItemIndex].Strings['Name'];
    LoadParameters(FInstance.Config.ColorizePreset[Combo_Preset.ItemIndex]);
    ApplyChosenColor;
  end;
end;

procedure TFColorize.FloatSpinEdit_HueChange(Sender: TObject);
begin
  if FUpdatingSpinEdit then exit;
  FUpdatingSpinEdit := true;
  ChosenHueDegF := FloatSpinEdit_Hue.Value;
  Combo_Preset.ItemIndex := -1;
  FUpdatingSpinEdit := false;
end;

procedure TFColorize.FloatSpinEdit_SaturationChange(Sender: TObject);
begin
  if FUpdatingSpinEdit then exit;
  FUpdatingSpinEdit := true;
  ChosenSatF := FloatSpinEdit_Saturation.Value;
  Combo_Preset.ItemIndex := -1;
  FUpdatingSpinEdit := false;
end;

procedure TFColorize.AdjustLabels(ALabel1, ALabel2: TLabel; ATrack1,
  ATrack2: TTrackBar);
var
  LabelMaxRight: integer;
begin
  LabelMaxRight:= ALabel1.BoundsRect.Right;
  if ALabel2.BoundsRect.Right > LabelMaxRight then
    LabelMaxRight := ALabel2.BoundsRect.Right;
  if (LabelMaxRight > ATrack1.Left) or (LabelMaxRight > ATrack2.Left) then
  begin
    ATrack1.SetBounds(LabelMaxRight,ATrack1.Top,ATrack1.BoundsRect.Right-LabelMaxRight,ATrack1.Height);
    ATrack2.SetBounds(LabelMaxRight,ATrack2.Top,ATrack2.BoundsRect.Right-LabelMaxRight,ATrack2.Height);
  end;
end;

procedure TFColorize.FormShow(Sender: TObject);
begin
  AdjustLabels(Label_Hue,Label_Colorness, TrackBar_Hue,TrackBar_Saturation);
  If Assigned(FFilterConnector) then LoadParameters(FFilterConnector.Parameters);
  ApplyChosenColor;
  Top := FInstance.MainFormBounds.Top;
  UpdateComboPreset(FInstance.Config.IndexOfColorizePreset(FSelectedPresetName));
  ToolBar_AddRemove.Images := FInstance.Icons[DoScaleX(16, OriginalDPI)];
end;

procedure TFColorize.ToolButton_RemoveClick(Sender: TObject);
begin
  if Combo_Preset.ItemIndex <> -1 then
  begin
    FInstance.Config.RemoveColorizePreset(Combo_Preset.ItemIndex);
    UpdateComboPreset(-1);
  end;
end;

procedure TFColorize.ToolButton_AddClick(Sender: TObject);
var s: string; idx: integer;
begin
  s := Trim(InputBox(rsColors,rsPresetName,''));
  if s <> '' then
  begin
    idx := FInstance.Config.AddColorizePreset(s,ChosenHueDegF,ChosenSatF,CheckBox_GSBA.Checked);
    UpdateComboPreset(idx);
  end;
end;

procedure TFColorize.TrackBar_Change(Sender: TObject);
begin
  if FInitialized then
  begin
    UpdateSpinEdit;
    ApplyChosenColor;
    Combo_Preset.ItemIndex := -1;
  end;
end;

procedure TFColorize.OnTryStopAction(sender: TFilterConnector);
begin
  if self.visible then Close;
end;

function TFColorize.GetChosenHue: Word;
begin
  result := round(TrackBar_Hue.Position/TrackBar_Hue.Max*65535);
end;

function TFColorize.GetChosenHueDegF: double;
begin
  result := round(ChosenHue/65536*3600)/10;
end;

function TFColorize.GetChosenSatF: double;
begin
  result := round(ChosenSaturation/65535*1000)/1000;
end;

function TFColorize.GetChosenSaturation: Word;
begin
  result := round(TrackBar_Saturation.Position/TrackBar_Saturation.Max*65535);
end;

procedure TFColorize.SetChosenHue(AValue: Word);
begin
  TrackBar_Hue.Position := round(AValue/65535*TrackBar_Hue.Max);
end;

procedure TFColorize.SetChosenHueDegF(AValue: double);
begin
  ChosenHue:= round(AValue*65536/360) and 65535;
end;

procedure TFColorize.SetChosenSatF(AValue: double);
begin
  ChosenSaturation:= round(AValue*65535);
end;

procedure TFColorize.SetChosenSaturation(AValue: Word);
begin
  TrackBar_Saturation.Position := round(AValue/65535*TrackBar_Saturation.Max);
end;

procedure TFColorize.UpdateSpinEdit;
begin
  if FUpdatingSpinEdit then exit;
  FUpdatingSpinEdit:= true;
  FloatSpinEdit_Hue.Value := ChosenHueDegF;
  FloatSpinEdit_Saturation.Value := ChosenSatF;
  FloatSpinEdit_Hue.Update;
  FloatSpinEdit_Saturation.Update;
  FUpdatingSpinEdit:= false;
end;

procedure TFColorize.LoadParameters(AParams: TVariableSet);
var OldInitialized: boolean;
begin
  OldInitialized := FInitialized;
  FInitialized := false;
  if AParams.IsDefined('Hue') then
    ChosenHueDegF := AParams.Floats['Hue'];
  if AParams.IsDefined('Saturation') then
    ChosenSatF := AParams.Floats['Saturation'];
  if AParams.IsDefined('Correction') then
    CheckBox_GSBA.Checked := AParams.Booleans['Correction'];
  UpdateSpinEdit;
  FInitialized := OldInitialized;
end;

procedure TFColorize.UpdateComboPreset(AIndex: integer);
var i: integer;
begin
  if FInComboPreset then exit;
  FInComboPreset := true;
  Combo_Preset.Clear;
  for i := 0 to FInstance.Config.ColorizePresetCount-1 do
    Combo_Preset.AddItem(FInstance.Config.ColorizePreset[i].Strings['Name'], FInstance.Config.ColorizePreset[i]);
  Combo_Preset.ItemIndex := AIndex;
  if Combo_Preset.ItemIndex <> -1 then
    FSelectedPresetName := FInstance.Config.ColorizePreset[Combo_Preset.ItemIndex].Strings['Name'];
  FInComboPreset := False;
end;

function TFColorize.ShowModal: integer;
begin
  if (FFilterConnector = nil) or (FFilterConnector.ActiveLayer = nil) then
  begin
    if FInstance <> nil then
      FInstance.ShowMessage(rsLazPaint,rsNoActiveLayer) else
      ShowMessage(rsNoActiveLayer);
    result := mrAbort
  end
  else
    Result:=inherited ShowModal;
end;

function TFColorize.ShowModal(AInstance: TLazPaintCustomInstance; AParameters: TVariableSet): integer;
var gsbaOptionFromConfig: boolean;
    topmostInfo: TTopMostInfo;
    h, s: Double; corr: boolean;
begin
  try
    FFilterConnector := TFilterConnector.Create(AInstance,AParameters,false);
    FFilterConnector.OnTryStopAction := @OnTryStopAction;
  except
    on ex: exception do
    begin
      AInstance.ShowError('Colorize',ex.Message);
      result := mrAbort;
      exit;
    end;
  end;
  try
    FInstance := AInstance;
    if AParameters.Booleans['Validate'] then
    begin
      if AParameters.IsDefined('Hue') then h := AParameters.Floats['Hue']
      else h := ChosenHueDegF;
      if AParameters.IsDefined('Saturation') then s := AParameters.Floats['Saturation']
      else s := ChosenSatF;
      if AParameters.IsDefined('Correction') then corr := AParameters.Booleans['Correction']
      else corr := AInstance.Config.DefaultUseGSBA;
      Colorize(FFilterConnector, h, s, corr);
      FFilterConnector.ValidateAction;
      result := mrOk;
    end else
    begin
      if AParameters.IsDefined('Correction') then
      begin
        gsbaOptionFromConfig:= false;
        self.CheckBox_GSBA.Checked := AParameters.Booleans['Correction'];
      end else
      begin
        gsbaOptionFromConfig:= true;
        self.CheckBox_GSBA.Checked := AInstance.Config.DefaultUseGSBA;
      end;
      topmostInfo := AInstance.HideTopmost;
      try
        result := self.ShowModal;
      except
        on ex: exception do
        begin
          AInstance.ShowError('Colorize',ex.Message);
          result := mrAbort;
        end;
      end;
      AInstance.ShowTopmost(topmostInfo);
      if (result = mrOK) and gsbaOptionFromConfig then
        AInstance.Config.SetDefaultUseGSBA(self.CheckBox_GSBA.Checked);
    end;
  finally
    FreeAndNil(FFilterConnector);
    FInstance := nil;
  end;
end;

procedure TFColorize.ApplyChosenColor;
begin
  Colorize(FFilterConnector, ChosenHueDegF, ChosenSatF, CheckBox_GSBA.Checked);
end;

{$R *.lfm}

end.

