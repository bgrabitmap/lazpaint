unit UShiftColors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, Spin, ExtCtrls, BGRABitmap, LazPaintType, uscaledpi,
  ufilterconnector, uscripting;

type

  { TFShiftColors }

  TFShiftColors = class(TForm)
    Button_Cancel: TButton;
    Button_OK: TButton;
    CheckBox_GSBA: TCheckBox;
    FloatSpinEdit_Hue: TFloatSpinEdit;
    FloatSpinEdit_Saturation: TFloatSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    TimerDrawPendingRows: TTimer;
    TrackBar_Hue: TTrackBar;
    TrackBar_Saturation: TTrackBar;
    procedure Button_OKClick(Sender: TObject);
    procedure CheckBox_GSBAChange(Sender: TObject);
    procedure FloatSpinEdit_HueChange(Sender: TObject);
    procedure FloatSpinEdit_SaturationChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
    procedure ParametersChanged;
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
  ScaleDPI(Self,OriginalDPI);

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
end;

procedure TFShiftColors.TrackBar_Change(Sender: TObject);
begin
  if FInitialized then
  begin
    UpdateSpinEdit;
    ParametersChanged;
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
begin
  try
    FFilterConnector := TFilterConnector.Create(AInstance,AParameters);
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
    if AParameters.IsDefined('Correction') and AParameters.IsDefined('Hue') and AParameters.IsDefined('Saturation') then
    begin
      ShiftColors(FFilterConnector, AParameters.Floats['Hue'], AParameters.Floats['Saturation'], AParameters.Booleans['Correction']);
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
  if FPendingRows then
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
  if FInitialized and Visible then ParametersChanged;
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

procedure TFShiftColors.ParametersChanged;
begin
  HalfApplyChosenShift;
  FPendingRows:= true;
  TimerDrawPendingRows.Enabled := true;
end;

{$R *.lfm}

end.

