unit URadialBlur;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Spin, ExtCtrls, BGRABitmap, BGRABitmapTypes, LazPaintType, UScaleDPI,
  UFilterConnector;

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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpinEdit_RadiusChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FInitializing: boolean;
    FFilterConnector: TFilterConnector;
    FThread: TThread;
    procedure PreviewNeeded;
    procedure ClearThread;
    procedure OnBlurDone({%H-}ASender: TThread; AFilteredLayer: TBGRABitmap);
  public
    blurType: TRadialBlurType;
  end;

function ShowRadialBlurDlg(AFilterConnector: TObject; blurType:TRadialBlurType):boolean;

implementation

uses UMac, BGRAFilters, UFilterThread;

function ShowRadialBlurDlg(AFilterConnector: TObject; blurType:TRadialBlurType):boolean;
var
  RadialBlur: TFRadialBlur;
begin
  result := false;
  RadialBlur:= TFRadialBlur.create(nil);
  RadialBlur.FFilterConnector := AFilterConnector as TFilterConnector;
  try
    RadialBlur.blurType := blurType;
    result:= (RadialBlur.ShowModal = mrOk);
  finally
    RadialBlur.free;
  end;
end;

type
  { TRadialBlurThread }

  TRadialBlurThread = class(TFilterThread)
  protected
    FBlurType: TRadialBlurType;
    FRadius: integer;
    function CreateFilterTask: TFilterTask; override;
  public
    constructor Create(AConnector: TFilterConnector; ABlurType: TRadialBlurType; ARadius: integer; ASuspended: boolean);
  end;

{ TRadialBlurThread }

function TRadialBlurThread.CreateFilterTask: TFilterTask;
begin
  result := CreateRadialBlurTask(FilterConnector.BackupLayer,FilterConnector.WorkArea, FRadius, FBlurType)
end;

constructor TRadialBlurThread.Create(AConnector: TFilterConnector;
  ABlurType: TRadialBlurType; ARadius: integer; ASuspended: boolean);
begin
  inherited Create(AConnector, True);
  FBlurType:= ABlurType;
  FRadius := ARadius;
  if not ASuspended then Start;
end;

{ TFRadialBlur }

procedure TFRadialBlur.Button_OKClick(Sender: TObject);
begin
  FFilterConnector.ValidateAction;
  FFilterConnector.lazPaintInstance.Config.SetDefaultBlurRadius(SpinEdit_Radius.Value);
  ModalResult := mrOK;
end;

procedure TFRadialBlur.FormCreate(Sender: TObject);
begin
  ScaleDPI(Self,OriginalDPI);

  FThread := nil;
  blurType := rbNormal;
  CheckOKCancelBtns(Button_OK,Button_Cancel);
  CheckSpinEdit(SpinEdit_Radius);
end;

procedure TFRadialBlur.FormDestroy(Sender: TObject);
begin
  ClearThread;
end;

procedure TFRadialBlur.FormShow(Sender: TObject);
begin
  FInitializing := True;
  SpinEdit_Radius.Value := FFilterConnector.LazPaintInstance.Config.DefaultBlurRadius;
  FInitializing := False;
  PreviewNeeded;
end;

procedure TFRadialBlur.SpinEdit_RadiusChange(Sender: TObject);
begin
  if not FInitializing then PreviewNeeded;
end;

procedure TFRadialBlur.Timer1Timer(Sender: TObject);
var filteredLayer: TBGRABitmap;
begin
  if FThread <> nil then
  begin
    Timer1.Enabled:= false;
    filteredLayer := (FThread as TFilterThread).FilteredLayer;
    if filteredLayer <> nil then
      FFilterConnector.PutImage(filteredLayer,False);
    Timer1.Enabled:= true;
  end;
end;

procedure TFRadialBlur.PreviewNeeded;
var blurThread: TFilterThread;
begin
  ClearThread;
  Button_OK.Enabled := false;
  blurThread := TRadialBlurThread.Create(FFilterConnector,blurType,SpinEdit_Radius.Value, True);
  blurThread.OnFilterDone:= @OnBlurDone;
  FThread := blurThread;
  FThread.Start;
end;

procedure TFRadialBlur.ClearThread;
begin
  if FThread <> nil then
  begin
    FThread.Terminate;
    FThread := nil; //let the thread free itself
  end;
end;

procedure TFRadialBlur.OnBlurDone(ASender: TThread; AFilteredLayer: TBGRABitmap
  );
begin
  FFilterConnector.PutImage(AFilteredLayer,False);
  Button_OK.Enabled := true;
  ClearThread;
end;

initialization
  {$I uradialblur.lrs}

end.

