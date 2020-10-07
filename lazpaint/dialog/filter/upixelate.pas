// SPDX-License-Identifier: GPL-3.0-only
unit UPixelate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Spin, ExtCtrls, BGRABitmap, LazPaintType, LCScaleDPI,
  UFilterConnector, UScripting;

type

  { TFPixelate }

  TFPixelate = class(TForm)
    Button_Cancel: TButton;
    Button_OK: TButton;
    ComboBox_Quality: TComboBox;
    Label_Quality: TLabel;
    Label_PixelSize: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
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
    procedure InitParams;
    procedure PreviewNeeded;
  public
  end;

function ShowPixelateDlg(AFilterConnector: TObject): TScriptResult;

implementation

uses umac, BGRABitmapTypes, uresourcestrings, ugraph;

function ShowPixelateDlg(AFilterConnector: TObject): TScriptResult;
var
  FPixelate: TFPixelate;
begin
  FPixelate:= TFPixelate.create(nil);
  FPixelate.FFilterConnector := AFilterConnector as TFilterConnector;
  try
    if FPixelate.FFilterConnector.ActiveLayer <> nil then
    begin
      if Assigned(FPixelate.FFilterConnector.Parameters) and
        FPixelate.FFilterConnector.Parameters.Booleans['Validate'] then
      begin
        FPixelate.InitParams;
        FPixelate.PreviewNeeded;
        FPixelate.FFilterConnector.ValidateAction;
        result := srOk;
      end else
      begin
        if FPixelate.showModal = mrOk then result := srOk
        else result := srCancelledByUser;
      end;
    end
    else
      result := srException;
  finally
    FPixelate.free;
  end;
end;

{ TFPixelate }

procedure TFPixelate.FormCreate(Sender: TObject);
begin
  ScaleControl(Self,OriginalDPI);

  CheckOKCancelBtns(Button_OK{,Button_Cancel});
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
var
  qualityStr: TCaption;
begin
  FFilterConnector.ValidateAction;
  FFilterConnector.LazPaintInstance.Config.SetDefaultPixelateSize(SpinEdit_PixelSize.Value);
  qualityStr := ComboBox_Quality.Text;
  if qualityStr = rsFast then qualityStr := 'Fast' else
  if qualityStr = rsLinear then qualityStr := 'Linear' else
  if qualityStr = rsMitchell then qualityStr := 'Mitchell' else
  if qualityStr = rsSpline then qualityStr := 'Spline' else
    qualityStr := '';
  FFilterConnector.LazPaintInstance.Config.SetDefaultPixelateQuality(qualityStr);
  ModalResult := mrOK;
end;

procedure TFPixelate.ComboBox_QualityChange(Sender: TObject);
begin
  if not FInitializing then PreviewNeeded;
end;

procedure TFPixelate.FormShow(Sender: TObject);
begin
  InitParams;
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

procedure TFPixelate.InitParams;
var
  qualityStr: String;
begin
  FInitializing := true;
  if Assigned(FFilterConnector.Parameters) and
    FFilterConnector.Parameters.IsDefined('PixelSize') then
    SpinEdit_PixelSize.Value := FFilterConnector.Parameters.Integers['PixelSize']
  else
    SpinEdit_PixelSize.Value := FFilterConnector.LazPaintInstance.Config.DefaultPixelateSize;

  if Assigned(FFilterConnector.Parameters) and
    FFilterConnector.Parameters.IsDefined('Quality') then
    qualityStr := FFilterConnector.Parameters.Strings['Quality']
  else
    qualityStr := FFilterConnector.LazPaintInstance.Config.DefaultPixelateQuality;

  if qualityStr = 'Fast' then qualityStr := rsFast else
  if qualityStr = 'Linear' then qualityStr := rsLinear else
  if qualityStr = 'Mitchell' then qualityStr := rsMitchell else
  if qualityStr = 'Spline' then qualityStr := rsSpline else
    qualityStr := '';

  ComboBox_Quality.ItemIndex := ComboBox_Quality.Items.IndexOf(qualityStr);
  FInitializing := false;
end;

procedure TFPixelate.PreviewNeeded;
begin
  FFilterConnector.PutImage(ComputeFilteredLayer,False,true);
end;

{$R *.lfm}

end.

