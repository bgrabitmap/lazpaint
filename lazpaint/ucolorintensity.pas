unit UColorintensity;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, Spin, BGRABitmap, LazPaintType, uscaledpi,
  uresourcestrings, ufilterconnector, uscripting;

type
  TColorIntensityMode = (ciIntensity, ciLightness);

  { TFColorIntensity }

  TFColorIntensity = class(TForm)
    Button_Cancel: TButton;
    Button_OK: TButton;
    FloatSpinEdit_Shift: TFloatSpinEdit;
    FloatSpinEdit_Factor: TFloatSpinEdit;
    Label_Multiply: TLabel;
    Label_Shift: TLabel;
    TrackBar_Multiply: TTrackBar;
    TrackBar_Shift: TTrackBar;
    procedure Button_OKClick(Sender: TObject);
    procedure FloatSpinEdit_FactorChange(Sender: TObject);
    procedure FloatSpinEdit_ShiftChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TrackBar_Change(Sender: TObject);
  private
    { private declarations }
    FInitialized: boolean;
    FInstance: TLazPaintCustomInstance;
    FFilterConnector: TFilterConnector;
    FMode: TColorIntensityMode;
    FUpdatingSpinEdit: boolean;
    FShiftCaption: string;
    FMultiplyCaption: string;
    procedure UpdateSpinEdit;
    function GetChosenFactor: single;
    function GetChosenShift: single;
    procedure OnTryStopAction({%H-}sender: TFilterConnector);
    procedure SetChosenFactor(AValue: single);
    procedure SetChosenShift(AValue: single);
    procedure AdjustLabels(ALabel1,ALabel2: TLabel; ATrack1,ATrack2:TTrackBar);
    procedure LoadParameters;
  public
    { public declarations }
    function ShowModal: integer; override;
    function ShowModal(AInstance: TLazPaintCustomInstance; AMode: TColorIntensityMode; AParameters: TVariableSet): integer;
    procedure ApplyChosenIntensity;
    property ChosenFactor: single read GetChosenFactor write SetChosenFactor;
    property ChosenShift: single read GetChosenShift write SetChosenShift;

  end;

implementation

uses umac, BGRABitmapTypes, UColorFilters;

{ TFColorIntensity }

procedure TFColorIntensity.FormCreate(Sender: TObject);
begin
  ScaleDPI(Self,OriginalDPI);

  FShiftCaption:= Label_Shift.Caption;
  FMultiplyCaption := Label_Multiply.Caption;
  CheckFloatSpinEdit(FloatSpinEdit_Factor);
  CheckFloatSpinEdit(FloatSpinEdit_Shift);
  CheckOKCancelBtns(Button_OK,Button_Cancel);
  TrackBar_Multiply.Min := 0;
  TrackBar_Multiply.Max := TrackBar_Multiply.Width and not 3;
  TrackBar_Multiply.Position := TrackBar_Multiply.Max div 2;
  TrackBar_Multiply.Frequency := TrackBar_Multiply.Max div 4;
  TrackBar_Shift.Max := TrackBar_Shift.Width and not 3;
  TrackBar_Shift.Position := TrackBar_Shift.Max div 2;
  TrackBar_Shift.Frequency := TrackBar_Shift.Max div 4;
  FMode := ciIntensity;
  FInitialized := true;
  UpdateSpinEdit;
end;

procedure TFColorIntensity.Button_OKClick(Sender: TObject);
begin
  ApplyChosenIntensity;
  FFilterConnector.ValidateAction;
  FFilterConnector.Parameters.Floats['Factor'] := FloatSpinEdit_Factor.Value;
  FFilterConnector.Parameters.Floats['Shift'] := FloatSpinEdit_Shift.Value;
end;

procedure TFColorIntensity.FloatSpinEdit_FactorChange(Sender: TObject);
begin
  FUpdatingSpinEdit := true;
  ChosenFactor := FloatSpinEdit_Factor.Value;
  FUpdatingSpinEdit := false;
end;

procedure TFColorIntensity.FloatSpinEdit_ShiftChange(Sender: TObject);
begin
  FUpdatingSpinEdit := true;
  ChosenShift := FloatSpinEdit_Shift.Value;
  FUpdatingSpinEdit := false;
end;

procedure TFColorIntensity.FormShow(Sender: TObject);
begin
  AdjustLabels(Label_Multiply,Label_Shift, TrackBar_Multiply,TrackBar_Shift);
  if FMode = ciIntensity then
  begin
    Caption := rsIntensity;
    Label_Multiply.Caption := FMultiplyCaption;
    Label_Shift.Caption := FShiftCaption;
  end
  else
  begin
    Caption := rsLightness;
    Label_Multiply.Caption := rsContrast;
    Label_Shift.Caption := rsBrightness;
  end;
  LoadParameters;
  ApplyChosenIntensity;
  Top := FInstance.MainFormBounds.Top;
end;

procedure TFColorIntensity.TrackBar_Change(Sender: TObject);
begin
  if FInitialized then
  begin
    UpdateSpinEdit;
    ApplyChosenIntensity;
  end;
end;

procedure TFColorIntensity.UpdateSpinEdit;
begin
  if FUpdatingSpinEdit then exit;
  FUpdatingSpinEdit:= true;
  FloatSpinEdit_Shift.Value := round(ChosenShift*1000)/1000;
  FloatSpinEdit_Factor.Value := round(ChosenFactor*1000)/1000;
  FloatSpinEdit_Shift.Update;
  FloatSpinEdit_Factor.Update;
  FUpdatingSpinEdit:= false;
end;

procedure TFColorIntensity.OnTryStopAction(sender: TFilterConnector);
begin
  if self.visible then Close;
end;

function TFColorIntensity.GetChosenFactor: single;
begin
  result := (TrackBar_Multiply.Position/TrackBar_Multiply.Max-0.5)*4;
end;

function TFColorIntensity.GetChosenShift: single;
begin
  result := (TrackBar_Shift.Position/TrackBar_Shift.Max-0.5)*2;
end;

procedure TFColorIntensity.SetChosenFactor(AValue: single);
begin
  TrackBar_Multiply.Position := round((AValue/4+0.5)*TrackBar_Multiply.Max);
end;

procedure TFColorIntensity.SetChosenShift(AValue: single);
begin
  TrackBar_Shift.Position := round((AValue/2+0.5)*TrackBar_Shift.Max);
end;

procedure TFColorIntensity.AdjustLabels(ALabel1, ALabel2: TLabel; ATrack1,
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

procedure TFColorIntensity.LoadParameters;
var OldInitialized: boolean;
begin
  If Assigned(FFilterConnector) then
  begin
    OldInitialized := FInitialized;
    FInitialized := false;
    if FFilterConnector.Parameters.IsDefined('Factor') then
      ChosenFactor := FFilterConnector.Parameters.Floats['Factor'];
    if FFilterConnector.Parameters.IsDefined('Shift') then
      ChosenShift := FFilterConnector.Parameters.Floats['Shift'];
    UpdateSpinEdit;
    FInitialized := OldInitialized;
  end;
end;

function TFColorIntensity.ShowModal: integer;
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

function TFColorIntensity.ShowModal(AInstance: TLazPaintCustomInstance;
  AMode: TColorIntensityMode; AParameters: TVariableSet): integer;
var topmostInfo: TTopMostInfo;
begin
  FMode := AMode;
  try
    FFilterConnector := TFilterConnector.Create(AInstance,AParameters);
    FFilterConnector.OnTryStopAction := @OnTryStopAction;
  except
    on ex: exception do
    begin
      AInstance.ShowError('ColorIntensity',ex.Message);
      result := mrAbort;
      exit;
    end;
  end;
  try
    FInstance := AInstance;
    if AParameters.IsDefined('Factor') and AParameters.IsDefined('Shift') then
    begin
      case FMode of
        ciIntensity: FilterIntensity(FFilterConnector, AParameters.Floats['Factor'],AParameters.Floats['Shift']);
        ciLightness: FilterLightness(FFilterConnector, AParameters.Floats['Factor'],AParameters.Floats['Shift']);
      end;
      FFilterConnector.ValidateAction;
      result := mrOk;
    end else
    begin
      topmostInfo := AInstance.HideTopmost;
      try
        result := self.ShowModal;
      except
        on ex: exception do
        begin
          AInstance.ShowError('ColorIntensity',ex.Message);
          result := mrAbort;
        end;
      end;
      AInstance.ShowTopmost(topmostInfo);
    end;
  finally
    FreeAndNil(FFilterConnector);
    FInstance := nil;
  end;
end;

procedure TFColorIntensity.ApplyChosenIntensity;
begin
  case FMode of
    ciIntensity: FilterIntensity(FFilterConnector, ChosenFactor,ChosenShift);
    ciLightness: FilterLightness(FFilterConnector, ChosenFactor,ChosenShift);
  end;
end;

{$R *.lfm}

end.

