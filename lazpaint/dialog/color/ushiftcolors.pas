// SPDX-License-Identifier: GPL-3.0-only
unit UShiftColors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, Spin, ExtCtrls, BGRABitmap, LazPaintType, LCScaleDPI,
  ufilterconnector, uscripting;

type

  { TFShiftColors }

  TFShiftColors = class(TForm)
    Button_Cancel: TButton;
    Button_OK: TButton;
    CheckBox_GSBA: TCheckBox;
    CheckBox_Preview: TCheckBox;
    FloatSpinEdit_Hue: TFloatSpinEdit;
    FloatSpinEdit_Saturation: TFloatSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    TimerDrawPendingRows: TTimer;
    TrackBar_Hue: TTrackBar;
    TrackBar_Saturation: TTrackBar;
    procedure Button_OKClick(Sender: TObject);
    procedure CheckBox_GSBAChange(Sender: TObject);
    procedure CheckBox_PreviewChange(Sender: TObject);
    procedure FloatSpinEdit_HueChange(Sender: TObject);
    procedure FloatSpinEdit_SaturationChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TimerDrawPendingRowsTimer(Sender: TObject);
    procedure TrackBar_Change(Sender: TObject);
  private
    { private declarations }
    FInitialized: boolean;
    FInstance: TLazPaintCustomInstance;
    FFilterConnector: TFilterConnector;
    FUpdatingSpinEdit: boolean;
    FOddRows: boolean;
    FPendingRows: boolean;
    FComputedImage: TBGRABitmap;
    function GetChosenHueShiftF: single;
    function GetChosenSatShiftF: single;
    procedure SetChosenHueShiftF(AValue: single);
    procedure SetChosenSatShiftF(AValue: single);
    procedure UpdateSpinEdit;
    function GetChosenHueShift: integer;
    procedure OnTryStopAction({%H-}sender: TFilterConnector);
    procedure SetChosenHueShift(AValue: integer);
    procedure LoadParameters;
    procedure HalfApplyChosenShift;
    procedure PreviewNeeded;
    procedure DisplayComputedImage;
    procedure StoreComputedImage;
  public
    { public declarations }
    function ShowModal: integer; override;
    function ShowModal(AInstance: TLazPaintCustomInstance; AParameters: TVariableSet): integer;
    property ChosenHueShift: integer read GetChosenHueShift write SetChosenHueShift;
    property ChosenHueShiftF: single read GetChosenHueShiftF write SetChosenHueShiftF;
    property ChosenSatShiftF: single read GetChosenSatShiftF write SetChosenSatShiftF;
  end;

implementation

uses umac, BGRABitmapTypes, uresourcestrings, UColorFilters;

{ TFShiftColors }

procedure TFShiftColors.FormCreate(Sender: TObject);
begin
  ScaleControl(Self,OriginalDPI);

  CheckFloatSpinEdit(FloatSpinEdit_Saturation);
  CheckFloatSpinEdit(FloatSpinEdit_Hue);
  CheckOKCancelBtns(Button_OK,Button_Cancel);
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

procedure TFShiftColors.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FComputedImage);
end;

procedure TFShiftColors.FormShow(Sender: TObject);
begin
  LoadParameters;
  HalfApplyChosenShift;
  HalfApplyChosenShift;
  FPendingRows:= false;
  Top := FInstance.MainFormBounds.Top;
end;

procedure TFShiftColors.TimerDrawPendingRowsTimer(Sender: TObject);
begin
  TimerDrawPendingRows.Enabled := false;
  if FPendingRows then
  begin
    HalfApplyChosenShift;
    FPendingRows:= false;
  end;
  Button_OK.Enabled := true;
  CheckBox_Preview.Enabled := true;
end;

procedure TFShiftColors.TrackBar_Change(Sender: TObject);
begin
  if FInitialized then
  begin
    UpdateSpinEdit;
    PreviewNeeded;
  end;
end;

procedure TFShiftColors.UpdateSpinEdit;
begin
  if FUpdatingSpinEdit then exit;
  FUpdatingSpinEdit:= true;
  FloatSpinEdit_Hue.Value := ChosenHueShiftF;
  FloatSpinEdit_Saturation.Value := ChosenSatShiftF;
  FloatSpinEdit_Hue.Update;
  FloatSpinEdit_Saturation.Update;
  FUpdatingSpinEdit:= false;
end;

function TFShiftColors.GetChosenHueShiftF: single;
begin
  result := round(ChosenHueShift/65536*3600)/10;
end;

function TFShiftColors.GetChosenSatShiftF: single;
begin
  result := round((TrackBar_Saturation.Position/TrackBar_Saturation.Max*4-2)*1000)/1000;
end;

procedure TFShiftColors.SetChosenHueShiftF(AValue: single);
begin
  ChosenHueShift := round(AValue*65536/360);
end;

procedure TFShiftColors.SetChosenSatShiftF(AValue: single);
begin
  TrackBar_Saturation.Position := round((AValue+2)/4*TrackBar_Saturation.Max);
end;

procedure TFShiftColors.OnTryStopAction(sender: TFilterConnector);
begin
  if self.visible then Close;
end;

function TFShiftColors.GetChosenHueShift: integer;
begin
  result := round((TrackBar_Hue.Position/TrackBar_Hue.Max-0.5)*65536);
end;

procedure TFShiftColors.SetChosenHueShift(AValue: integer);
begin
  TrackBar_Hue.Position := round((AValue/65536+0.5)*TrackBar_Hue.Max);
end;

procedure TFShiftColors.LoadParameters;
var OldInitialized: boolean;
begin
  If Assigned(FFilterConnector) then
  begin
    OldInitialized := FInitialized;
    FInitialized := false;
    if FFilterConnector.Parameters.IsDefined('Hue') then
      ChosenHueShiftF := FFilterConnector.Parameters.Floats['Hue'];
    if FFilterConnector.Parameters.IsDefined('Saturation') then
      ChosenSatShiftF := FFilterConnector.Parameters.Floats['Saturation'];
    if FFilterConnector.Parameters.IsDefined('Correction') then
      CheckBox_GSBA.Checked := FFilterConnector.Parameters.Booleans['Correction'];
    UpdateSpinEdit;

    FreeAndNil(FComputedImage);
    Button_OK.Caption := rsOK;
    Button_OK.Enabled := True;
    Button_Cancel.Caption := rsCancel;
    CheckBox_Preview.Caption := rsPreview;
    CheckBox_Preview.Checked := True;
    CheckBox_Preview.Enabled := True;
    FInitialized := OldInitialized;
  end;
end;

function TFShiftColors.ShowModal: integer;
begin
  TimerDrawPendingRows.Enabled:= false;
  if (FFilterConnector = nil) or (FFilterConnector.ActiveLayer = nil) then
  begin
    if FInstance <> nil then
      FInstance.ShowMessage(rsLazPaint,rsNoActiveLayer) else
      ShowMessage(rsNoActiveLayer);
    result := mrAbort
  end
  else
    Result:=inherited ShowModal;
  TimerDrawPendingRows.Enabled:= false;
end;

function TFShiftColors.ShowModal(AInstance: TLazPaintCustomInstance; AParameters: TVariableSet): integer;
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
      AInstance.ShowError('ShiftColors',ex.Message);
      result := mrAbort;
      exit;
    end;
  end;
  try
    FInstance := AInstance;
    if AParameters.Booleans['Validate'] then
    begin
      if AParameters.IsDefined('Hue') then h := AParameters.Floats['Hue']
      else h := FloatSpinEdit_Hue.Value;
      if AParameters.IsDefined('Saturation') then s := AParameters.Floats['Saturation']
      else s := FloatSpinEdit_Saturation.Value;
      if AParameters.IsDefined('Correction') then corr := AParameters.Booleans['Correction']
      else corr := AInstance.Config.DefaultUseGSBA;
      ShiftColors(FFilterConnector, h, s, corr);
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
          AInstance.ShowError('ShiftColors',ex.Message);
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

procedure TFShiftColors.Button_OKClick(Sender: TObject);
begin
  Button_OK.Enabled := false;
  CheckBox_Preview.Enabled := false;

  if not CheckBox_Preview.Checked then
    DisplayComputedImage
  else if FPendingRows then
  begin
    HalfApplyChosenShift;
    FPendingRows := false;
  end;
  FFilterConnector.ValidateAction;
  FFilterConnector.Parameters.Floats['Hue'] := FloatSpinEdit_Hue.Value;
  FFilterConnector.Parameters.Floats['Saturation'] := FloatSpinEdit_Saturation.Value;
  FFilterConnector.Parameters.Booleans['Correction'] := CheckBox_GSBA.Checked;
end;

procedure TFShiftColors.CheckBox_GSBAChange(Sender: TObject);
begin
  if FInitialized and Visible then PreviewNeeded;
end;

procedure TFShiftColors.CheckBox_PreviewChange(Sender: TObject);
begin
  if not FInitialized then exit;
  if CheckBox_Preview.Checked then
    DisplayComputedImage
  else begin
    StoreComputedImage;
    FFilterConnector.RestoreBackup;
  end;
end;

procedure TFShiftColors.FloatSpinEdit_HueChange(Sender: TObject);
begin
  if FUpdatingSpinEdit then exit;
  FUpdatingSpinEdit := true;
  ChosenHueShiftF:= FloatSpinEdit_Hue.Value;
  FUpdatingSpinEdit := false;
end;

procedure TFShiftColors.FloatSpinEdit_SaturationChange(Sender: TObject);
begin
  if FUpdatingSpinEdit then exit;
  FUpdatingSpinEdit := true;
  ChosenSatShiftF := FloatSpinEdit_Saturation.Value;
  FUpdatingSpinEdit := false;
end;

procedure TFShiftColors.HalfApplyChosenShift;
begin
  ShiftColors(FFilterConnector,ChosenHueShiftF,ChosenSatShiftF,CheckBox_GSBA.Checked,not FOddRows,FOddRows);
  FOddRows:= not FOddRows;
end;

procedure TFShiftColors.PreviewNeeded;
begin
  Button_OK.Enabled := false;

  FInitialized := false;
  CheckBox_Preview.Enabled := false;
  CheckBox_Preview.Checked := true;
  FInitialized := true;
  FreeAndNil(FComputedImage);

  HalfApplyChosenShift;
  FPendingRows := true;
  TimerDrawPendingRows.Enabled := true;
end;

procedure TFShiftColors.DisplayComputedImage;
begin
  if FComputedImage <> nil then
    FFilterConnector.PutImage(FComputedImage, false, false);
end;

procedure TFShiftColors.StoreComputedImage;
begin
  if not FPendingRows and (FComputedImage = nil) then
    FComputedImage := FFilterConnector.ActiveLayer.Duplicate;
end;

{$R *.lfm}

end.

