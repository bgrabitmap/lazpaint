unit UPixelate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Spin, BGRABitmap, LazPaintType, uscaledpi, ufilterconnector;

type

  { TFPixelate }

  TFPixelate = class(TForm)
    Button_Cancel: TButton;
    Button_OK: TButton;
    ComboBox_Quality: TComboBox;
    Label_Quality: TLabel;
    Label_PixelSize: TLabel;
    SpinEdit_PixelSize: TSpinEdit;
    procedure Button_OKClick(Sender: TObject);
    procedure ComboBox_QualityChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpinEdit_PixelSizeChange(Sender: TObject);
  private
    { private declarations }
    FInitializing: boolean;
    FFilterConnector: TFilterConnector;
    function ComputeFilteredLayer: TBGRABitmap;
    procedure PreviewNeeded;
  public
  end;

function ShowPixelateDlg(AFilterConnector: TObject):boolean;

implementation

uses umac, BGRABitmapTypes, uresourcestrings, ugraph;

function ShowPixelateDlg(AFilterConnector: TObject):boolean;
var
  FPixelate: TFPixelate;
begin
  result := false;
  FPixelate:= TFPixelate.create(nil);
  FPixelate.FFilterConnector := AFilterConnector as TFilterConnector;
  try
    if FPixelate.FFilterConnector.ActiveLayer <> nil then
      result:= (FPixelate.showModal = mrOk)
    else
      result := false;
  finally
    FPixelate.free;
  end;
end;

{ TFPixelate }

procedure TFPixelate.FormCreate(Sender: TObject);
begin
  ScaleDPI(Self,OriginalDPI);

  CheckOKCancelBtns(Button_OK,Button_Cancel);
  CheckSpinEdit(SpinEdit_PixelSize);

  FInitializing := true;
  with ComboBox_Quality.Items do begin
    Add(rsFast);
    Add(rsLinear);
    Add(rsMitchell);
    Add(rsSpline);
  end;
  FInitializing := false;
end;

procedure TFPixelate.Button_OKClick(Sender: TObject);
begin
  FFilterConnector.ValidateAction;
  FFilterConnector.LazPaintInstance.Config.SetDefaultPixelateSize(SpinEdit_PixelSize.Value);
  FFilterConnector.LazPaintInstance.Config.SetDefaultPixelateQuality(ComboBox_Quality.Text);
  ModalResult := mrOK;
end;

procedure TFPixelate.ComboBox_QualityChange(Sender: TObject);
begin
  if not FInitializing then PreviewNeeded;
end;

procedure TFPixelate.FormShow(Sender: TObject);
begin
  FInitializing := true;
  SpinEdit_PixelSize.Value := FFilterConnector.LazPaintInstance.Config.DefaultPixelateSize;
  ComboBox_Quality.ItemIndex := ComboBox_Quality.Items.IndexOf(FFilterConnector.LazPaintInstance.Config.DefaultPixelateQuality);
  FInitializing := false;
  PreviewNeeded;
  Top := FFilterConnector.LazPaintInstance.MainFormBounds.Top;
end;

procedure TFPixelate.SpinEdit_PixelSizeChange(Sender: TObject);
begin
  if not FInitializing then PreviewNeeded;
end;

function TFPixelate.ComputeFilteredLayer: TBGRABitmap;
begin
  result := DoPixelate(FFilterConnector.BackupLayer,SpinEdit_PixelSize.Value,ComboBox_Quality.Text);
end;

procedure TFPixelate.PreviewNeeded;
begin
  FFilterConnector.PutImage(ComputeFilteredLayer,False,true);
end;

{$R *.lfm}

end.

