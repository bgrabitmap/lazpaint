// SPDX-License-Identifier: GPL-3.0-only
unit URadialBlur;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Spin, ExtCtrls, BGRABitmap, BGRABitmapTypes, LazPaintType, LCScaleDPI,
  UFilterConnector, UFilterThread, UScripting;

type

  { TFRadialBlur }

  TFRadialBlur = class(TForm)
    Button_OK: TButton;
    Button_Cancel: TButton;
    Button3: TButton;
    Panel1: TPanel;
    Panel2: TPanel;
    SpinEdit_Radius: TFloatSpinEdit;
    Image1: TImage;
    Label_Radius: TLabel;
    Timer1: TTimer;
    procedure Button_OKClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpinEdit_RadiusChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FInitializing: boolean;
    FFilterConnector: TFilterConnector;
    FThreadManager: TFilterThreadManager;
    FLastRadius: single;
    FVars: TVariableSet;
    procedure PreviewNeeded;
    procedure UpdateStep;
    procedure OnTaskEvent({%H-}ASender: TObject; AEvent: TThreadManagerEvent);
  public
    blurType: TRadialBlurType;
  end;

function ShowRadialBlurDlg(AFilterConnector: TObject; ABlurType:TRadialBlurType;
  ACaption: string = ''): TScriptResult;

implementation

uses UMac, BGRAFilters;

function ShowRadialBlurDlg(AFilterConnector: TObject;
  ABlurType: TRadialBlurType; ACaption: string): TScriptResult;
var
  RadialBlur: TFRadialBlur;
begin
  RadialBlur:= TFRadialBlur.create(nil);
  RadialBlur.FFilterConnector := AFilterConnector as TFilterConnector;
  RadialBlur.FThreadManager := TFilterThreadManager.Create(RadialBlur.FFilterConnector);
  RadialBlur.FThreadManager.OnEvent := @RadialBlur.OnTaskEvent;
  RadialBlur.FVars := RadialBlur.FFilterConnector.Parameters;
  if ACaption<>'' then RadialBlur.Caption := ACaption;
  try
    RadialBlur.blurType := ABlurType;
    if RadialBlur.ShowModal = mrOk then result := srOk
    else result := srCancelledByUser;
  finally
    RadialBlur.FThreadManager.Free;
    RadialBlur.free;
  end;
end;

{ TFRadialBlur }

procedure TFRadialBlur.Button_OKClick(Sender: TObject);
begin
  if not FFilterConnector.ActionDone then
  begin
    FFilterConnector.ValidateAction;
    FFilterConnector.lazPaintInstance.Config.SetDefaultBlurRadius(SpinEdit_Radius.Value);
  end;
  ModalResult := mrOK;
end;

procedure TFRadialBlur.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  FThreadManager.Quit;
  CanClose := FThreadManager.ReadyToClose;
end;

procedure TFRadialBlur.FormCreate(Sender: TObject);
begin
  ScaleControl(Self,OriginalDPI);

  blurType := rbNormal;
  CheckOKCancelBtns(Button_OK{,Button_Cancel});
  CheckFloatSpinEdit(SpinEdit_Radius);
  SpinEdit_Radius.Constraints.MinWidth := DoScaleX(70, OriginalDPI);
end;

procedure TFRadialBlur.FormShow(Sender: TObject);
begin
  FInitializing := True;
  if Assigned(FVars) and FVars.IsDefined('Radius') then
    SpinEdit_Radius.Value := FVars.Floats['Radius']
  else if Assigned(FVars) and FVars.IsDefined('RadiusX') and FVars.IsDefined('RadiusY') then
    SpinEdit_Radius.Value := (FVars.Floats['RadiusX']+FVars.Floats['RadiusY'])/2
  else
    SpinEdit_Radius.Value := FFilterConnector.LazPaintInstance.Config.DefaultBlurRadius;
  UpdateStep;
  FInitializing := False;
  PreviewNeeded;
  Top := FFilterConnector.LazPaintInstance.MainFormBounds.Top;
end;

procedure TFRadialBlur.SpinEdit_RadiusChange(Sender: TObject);
begin
  if not FInitializing and (SpinEdit_Radius.Value <> FLastRadius) then
  begin
    UpdateStep;
    PreviewNeeded;
  end;
end;

procedure TFRadialBlur.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled:= false;
  FThreadManager.RegularCheck;
  Timer1.Interval := 200;
  Timer1.Enabled:= true;
end;

procedure TFRadialBlur.PreviewNeeded;
begin
  FLastRadius:= SpinEdit_Radius.Value;
  FThreadManager.WantPreview(CreateRadialBlurTask(FFilterConnector.BackupLayer,FFilterConnector.WorkArea, FLastRadius, blurType));
end;

procedure TFRadialBlur.UpdateStep;
var deci: integer;
begin
  deci := round(SpinEdit_Radius.Value*10) mod 10;
  if (SpinEdit_Radius.Value < 2) or ((deci <> 0) and (deci <> 5)) then
    SpinEdit_Radius.Increment := 0.1 else
  if (SpinEdit_Radius.Value < 8) or (deci<>0) then
    SpinEdit_Radius.Increment := 0.5 else
    SpinEdit_Radius.Increment := 1;
end;

procedure TFRadialBlur.OnTaskEvent(ASender: TObject; AEvent: TThreadManagerEvent
  );
begin
  case AEvent of
  tmeAbortedTask,tmeCompletedTask:
    begin
      Timer1.Enabled := false;
      if FThreadManager.ReadyToClose then
        Close
      else
        if AEvent = tmeCompletedTask then Button_OK.Enabled := true;
    end;
  tmeStartingNewTask:
    begin
      Timer1.Enabled := false;
      Timer1.Interval := 100;
      Timer1.Enabled := true;
      Button_OK.Enabled := false;
    end;
  end;
end;

{$R *.lfm}

end.

