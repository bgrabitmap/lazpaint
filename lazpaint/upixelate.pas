unit upixelate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Spin, BGRABitmap, LazPaintType, uscaledpi;

type

  { TFPixelate }

  TFPixelate = class(TForm)
    Button_Cancel: TButton;
    Button_OK: TButton;
    ComboBox_Quality: TComboBox;
    Label1: TLabel;
    Label3: TLabel;
    SpinEdit_PixelSize: TSpinEdit;
    procedure Button_OKClick(Sender: TObject);
    procedure ComboBox_QualityChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpinEdit_PixelSizeChange(Sender: TObject);
  private
    { private declarations }
    backupSource: TBGRABitmap;
    FLazPaintInstance: TLazPaintCustomInstance;
    procedure SetLazPaintInstance(const AValue: TLazPaintCustomInstance);
    function ComputeFilteredLayer: TBGRABitmap;
    procedure PreviewNeeded;
  public
    { public declarations }
    sourceLayer, filteredLayer: TBGRABitmap;
    property LazPaintInstance: TLazPaintCustomInstance read FlazPaintInstance write SetlazPaintInstance;
  end; 

function ShowPixelateDlg(Instance: TLazPaintCustomInstance; layer:TBGRABitmap; out filteredLayer: TBGRABitmap):boolean;

implementation

uses umac, BGRABitmapTypes, uresourcestrings, ugraph;

function ShowPixelateDlg(Instance: TLazPaintCustomInstance; layer:TBGRABitmap; out filteredLayer: TBGRABitmap):boolean;
var
  FPixelate: TFPixelate;
begin
  filteredLayer := nil;
  result := false;
  FPixelate:= TFPixelate.create(nil);
  FPixelate.LazPaintInstance := Instance;
  try
    FPixelate.sourceLayer := layer;
    result:= (FPixelate.showModal = mrOk);
    filteredLayer := FPixelate.filteredLayer;
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

  with ComboBox_Quality.Items do begin
    Add(rsFast);
    Add(rsLinear);
    Add(rsMitchell);
    Add(rsSpline);
  end;

  filteredLayer := nil;
  backupSource := nil;
end;

procedure TFPixelate.Button_OKClick(Sender: TObject);
begin
  if sourceLayer <> nil then
  begin
    filteredLayer := ComputeFilteredLayer;
    LazPaintInstance.Config.SetDefaultPixelateSize(SpinEdit_PixelSize.Value);
    LazPaintInstance.Config.SetDefaultPixelateQuality(ComboBox_Quality.Text);
    ModalResult := mrOK;
  end else
    ModalResult := mrCancel;
end;

procedure TFPixelate.ComboBox_QualityChange(Sender: TObject);
begin
  PreviewNeeded;
end;

procedure TFPixelate.FormDestroy(Sender: TObject);
begin
  if backupSource <> nil then
  begin
    sourceLayer.PutImage(0,0,backupSource,dmSet);
    FreeAndNil(backupSource);
    LazPaintInstance.NotifyImageChangeCompletely(False);
  end;
end;

procedure TFPixelate.FormShow(Sender: TObject);
begin
  PreviewNeeded;
end;

procedure TFPixelate.SpinEdit_PixelSizeChange(Sender: TObject);
begin
  PreviewNeeded;
end;

procedure TFPixelate.SetlazPaintInstance(const AValue: TLazPaintCustomInstance);
begin
  if FlazPaintInstance=AValue then exit;
  FlazPaintInstance:=AValue;
  SpinEdit_PixelSize.Value := LazPaintInstance.Config.DefaultPixelateSize;
  ComboBox_Quality.ItemIndex := ComboBox_Quality.Items.IndexOf(LazPaintInstance.Config.DefaultPixelateQuality);
end;

function TFPixelate.ComputeFilteredLayer: TBGRABitmap;
var usedSource: TBGRABitmap;
begin
  if backupSource <> nil then
    usedSource := backupSource
  else
    usedSource := sourceLayer;

  result := DoPixelate(usedSource,SpinEdit_PixelSize.Value,ComboBox_Quality.Text);
end;

procedure TFPixelate.PreviewNeeded;
var temp: TBGRABitmap;
begin
  if sourceLayer = nil then exit;
  temp := ComputeFilteredLayer;
  if backupSource = nil then
    backupSource := sourceLayer.Duplicate as TBGRABitmap;
  sourceLayer.PutImage(0,0,temp,dmSet);
  temp.Free;
  LazPaintInstance.NotifyImageChangeCompletely(True);
end;

initialization
  {$I upixelate.lrs}

end.

