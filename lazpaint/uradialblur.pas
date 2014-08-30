unit URadialBlur;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Spin, ExtCtrls, BGRABitmap, BGRABitmapTypes, LazPaintType, UScaleDPI,
  UFilterConnector, UFilterThread;

type

  { TFRadialBlur }

  TFRadialBlur = class(TForm)
    Button_OK: TButton;
    Button_Cancel: TButton;
    Button3: TButton;
    Image1: TImage;
    SpinEdit_Radius: TSpinEdit;
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
    FLastRadius: integer;
    procedure PreviewNeeded;
    procedure OnTaskEvent({%H-}ASender: TObject; AEvent: TThreadManagerEvent);
  public
    blurType: TRadialBlurType;
  end;

function ShowRadialBlurDlg(AFilterConnector: TObject; blurType:TRadialBlurType):boolean;

implementation

uses UMac, BGRAFilters;

function ShowRadialBlurDlg(AFilterConnector: TObject; blurType:TRadialBlurType):boolean;
var
  RadialBlur: TFRadialBlur;
begin
  result := false;
  RadialBlur:= TFRadialBlur.create(nil);
  RadialBlur.FFilterConnector := AFilterConnector as TFilterConnector;
  RadialBlur.FThreadManager := TFilterThreadManager.Create(RadialBlur.FFilterConnector);
  RadialBlur.FThreadManager.OnEvent := @RadialBlur.OnTaskEvent;
  try
    RadialBlur.blurType := blurType;
    result:= (RadialBlur.ShowModal = mrOk);
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
  ScaleDPI(Self,OriginalDPI);

  blurType := rbNormal;
  CheckOKCancelBtns(Button_OK,Button_Cancel);
  CheckSpinEdit(SpinEdit_Radius);
end;

procedure TFRadialBlur.FormShow(Sender: TObject);
begin
  FInitializing := True;
  SpinEdit_Radius.Value := FFilterConnector.LazPaintInstance.Config.DefaultBlurRadius;
  FInitializing := False;
  PreviewNeeded;
  Top := FFilterConnector.LazPaintInstance.MainFormBounds.Top;
end;

procedure TFRadialBlur.SpinEdit_RadiusChange(Sender: TObject);
begin
  if not FInitializing and (SpinEdit_Radius.Value <> FLastRadius) then PreviewNeeded;
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

