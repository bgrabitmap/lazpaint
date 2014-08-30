unit unoisefilter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Spin, BGRABitmap, BGRABitmapTypes, UFilterConnector;

type
  { TFNoiseFilter }

  TFNoiseFilter = class(TForm)
    Button_Cancel: TButton;
    Button_OK: TButton;
    Label_Opacity: TLabel;
    Radio_RGBNoise: TRadioButton;
    Radio_GrayscaleNoise: TRadioButton;
    SpinEdit_Alpha: TSpinEdit;
    procedure Button_OKClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var {%H-}CanClose: boolean);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure Radio_NoiseChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpinEdit_AlphaChange(Sender: TObject);
  private
    { private declarations }
    FComputedLayer: TBGRABitmap;
    FClosing: boolean;
  public
    FInitializing: boolean;
    FFilterConnector: TFilterConnector;
    procedure ComputeFilteredLayer;
    procedure PreviewNeeded(ARecomputeRandom: boolean);
  end;

function ShowNoiseFilterDlg(AFilterConnector: TObject):boolean;

implementation

uses BGRAGradientScanner, umac, UScaleDPI, LazPaintType;

function ShowNoiseFilterDlg(AFilterConnector: TObject):boolean;
var
  FNoiseFilter: TFNoiseFilter;
begin
  result := false;
  FNoiseFilter:= TFNoiseFilter.create(nil);
  FNoiseFilter.FFilterConnector := AFilterConnector as TFilterConnector;
  try
    if FNoiseFilter.FFilterConnector.ActiveLayer <> nil then
      result:= (FNoiseFilter.showModal = mrOk)
    else
      result := false;
  finally
    FNoiseFilter.free;
  end;
end;

{$R *.lfm}

{ TFNoiseFilter }

procedure TFNoiseFilter.Button_OKClick(Sender: TObject);
begin
  FFilterConnector.ValidateAction;
  ModalResult := mrOK;
end;

procedure TFNoiseFilter.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  FClosing := true;
end;

procedure TFNoiseFilter.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FComputedLayer);
end;

procedure TFNoiseFilter.FormHide(Sender: TObject);
begin
  FreeAndNil(FComputedLayer);
end;

procedure TFNoiseFilter.Radio_NoiseChange(Sender: TObject);
begin
  if FInitializing then exit;
  PreviewNeeded(true);
end;

procedure TFNoiseFilter.FormCreate(Sender: TObject);
begin
  ScaleDPI(Self,OriginalDPI);

  CheckOKCancelBtns(Button_OK,Button_Cancel);
  CheckSpinEdit(SpinEdit_Alpha);
end;

procedure TFNoiseFilter.FormShow(Sender: TObject);
begin
  FInitializing := true;
  FClosing := false;
  Top := FFilterConnector.LazPaintInstance.MainFormBounds.Top;
  if FFilterConnector.LazPaintInstance.BlackAndWhite then
  begin
    Radio_GrayscaleNoise.Checked := true;
    Radio_RGBNoise.Enabled := False;
  end;
  FInitializing := false;
  PreviewNeeded(True);
end;

procedure TFNoiseFilter.SpinEdit_AlphaChange(Sender: TObject);
begin
  if FInitializing or FClosing then exit;
  if FComputedLayer = nil then ComputeFilteredLayer;
  FComputedLayer.AlphaFill(SpinEdit_Alpha.Value);
  PreviewNeeded(False);
end;

procedure TFNoiseFilter.ComputeFilteredLayer;
var scan: TBGRARandomScanner;
begin
  scan := TBGRARandomScanner.Create(Radio_GrayscaleNoise.Checked, SpinEdit_Alpha.Value);
  if FComputedLayer = nil then
    FComputedLayer := TBGRABitmap.Create(FFilterConnector.ActiveLayer.Width,FFilterConnector.ActiveLayer.Height);
  FComputedLayer.FillRect(FFilterConnector.WorkArea,scan,dmSet);
  scan.Free;
end;

procedure TFNoiseFilter.PreviewNeeded(ARecomputeRandom: boolean);
begin
  if ARecomputeRandom or (FComputedLayer = nil) then ComputeFilteredLayer;
  FFilterConnector.PutImage(FComputedLayer,Radio_RGBNoise.Checked,False);
end;

end.

