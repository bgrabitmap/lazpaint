unit utwirl;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Spin, ExtCtrls, BGRABitmap, LazPaintType, uscaledpi;

type

  { TFTwirl }

  TFTwirl = class(TForm)
    Button_Cancel: TButton;
    Button_OK: TButton;
    Label2: TLabel;
    Label3: TLabel;
    SpinEdit_Angle: TSpinEdit;
    SpinEdit_Radius: TSpinEdit;
    Timer1: TTimer;
    procedure Button_OKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpinEdit_AngleChange(Sender: TObject);
    procedure SpinEdit_RadiusChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    backupSource: TBGRABitmap;
    procedure PreviewNeeded;
    function ComputeFilteredLayer: TBGRABitmap;
  public
    { public declarations }
    LazPaintInstance: TLazPaintCustomInstance;
    sourceLayer, filteredLayer: TBGRABitmap;
  end; 

function ShowTwirlDlg(Instance: TLazPaintCustomInstance; layer:TBGRABitmap; out filteredLayer: TBGRABitmap):boolean;

implementation

uses umac, BGRABitmapTypes;

function ShowTwirlDlg(Instance: TLazPaintCustomInstance; layer:TBGRABitmap; out filteredLayer: TBGRABitmap):boolean;
var
  FTwirl: TFTwirl;
begin
  filteredLayer := nil;
  result := false;
  FTwirl:= TFTwirl.create(nil);
  FTwirl.LazPaintInstance := Instance;
  try
    FTwirl.sourceLayer := layer;
    result:= (FTwirl.showModal = mrOk);
    filteredLayer := FTwirl.filteredLayer;
  finally
    FTwirl.free;
  end;
end;

{ TFTwirl }

procedure TFTwirl.FormCreate(Sender: TObject);
begin
  ScaleDPI(Self,OriginalDPI);

  CheckSpinEdit(SpinEdit_Radius);
  CheckSpinEdit(SpinEdit_Angle);
  CheckOKCancelBtns(Button_OK,Button_Cancel);
  filteredLayer := nil;
end;

procedure TFTwirl.FormDestroy(Sender: TObject);
begin
  if backupSource <> nil then
  begin
    sourceLayer.PutImage(0,0,backupSource,dmSet);
    FreeAndNil(backupSource);
    LazPaintInstance.NotifyImageChangeCompletely(False);
  end;
end;

procedure TFTwirl.FormShow(Sender: TObject);
begin
  SpinEdit_Radius.Value := round(LazPaintInstance.Config.DefaultTwirlRadius);
  SpinEdit_Angle.Value := round(LazPaintInstance.Config.DefaultTwirlTurn*360);
  PreviewNeeded;
end;

procedure TFTwirl.SpinEdit_AngleChange(Sender: TObject);
begin
  PreviewNeeded;
end;

procedure TFTwirl.SpinEdit_RadiusChange(Sender: TObject);
begin
  PreviewNeeded;
end;

procedure TFTwirl.Timer1Timer(Sender: TObject);
var temp: TBGRABitmap;
begin
  Timer1.Enabled := false;
  if sourceLayer = nil then exit;

  temp := ComputeFilteredLayer;
  if backupSource = nil then
    backupSource := sourceLayer.Duplicate as TBGRABitmap;
  sourceLayer.PutImage(0,0,temp,dmSet);
  temp.Free;
  LazPaintInstance.NotifyImageChangeCompletely(True);
end;

procedure TFTwirl.PreviewNeeded;
begin
  Timer1.Enabled := false;
  Timer1.Enabled := True;
end;

function TFTwirl.ComputeFilteredLayer: TBGRABitmap;
var usedSource: TBGRABitmap;
begin
  if backupSource <> nil then
    usedSource := backupSource
  else
    usedSource := sourceLayer;
  result := usedSource.FilterTwirl(Point(sourceLayer.Width div 2,sourceLayer.Height div 2),
      SpinEdit_Radius.Value,SpinEdit_Angle.Value/360) as TBGRABitmap;
end;

procedure TFTwirl.Button_OKClick(Sender: TObject);
begin
  if sourceLayer <> nil then
  begin
    LazPaintInstance.Config.SetDefaultTwirlRadius(SpinEdit_Radius.Value);
    LazPaintInstance.Config.SetDefaultTwirlTurn(SpinEdit_Angle.Value/360);
    filteredLayer := ComputeFilteredLayer;
    ModalResult := mrOK;
  end else
    ModalResult := mrCancel;
end;

initialization
  {$I utwirl.lrs}

end.

