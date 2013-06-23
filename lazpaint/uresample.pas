unit uresample;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Spin, LazPaintType, uscaledpi, uresourcestrings;

type

  { TFResample }

  TFResample = class(TForm)
    Button_OK: TButton;
    Button_Cancel: TButton;
    CheckBox_Ratio: TCheckBox;
    ComboBox_Quality: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    SpinEdit_Width: TSpinEdit;
    SpinEdit_Height: TSpinEdit;
    procedure Button_OKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpinEdit_HeightChange(Sender: TObject);
    procedure SpinEdit_WidthChange(Sender: TObject);
  public
    LazPaintInstance: TLazPaintCustomInstance;
  end; 

function ShowResampleDialog(Instance: TLazPaintCustomInstance):boolean;

implementation

uses BGRABitmap, BGRABitmapTypes, umac;

{ TFResample }

function ShowResampleDialog(Instance: TLazPaintCustomInstance):boolean;
var
  Resample: TFResample;
begin
  result := false;
  Resample := nil;
  try
    Resample:= TFResample.create(nil);
    Resample.LazPaintInstance := Instance;
    result:= (Resample.ShowModal = mrOk);
  except
    on ex:Exception do
      ShowMessage('ShowResampleDialog: '+ex.Message);
  end;
  Resample.free;
end;

procedure TFResample.FormCreate(Sender: TObject);
begin
  ScaleDPI(Self,OriginalDPI);

  CheckOKCancelBtns(Button_OK,Button_Cancel);
  CheckSpinEdit(SpinEdit_Width);
  CheckSpinEdit(SpinEdit_Height);
  with ComboBox_Quality.Items do begin
    Add(rsFast);
    Add(rsLinear);
    Add(rsHalfCosine);
    Add(rsCosine);
    Add(rsMitchell);
    Add(rsSpline);
    Add(rsBestQuality);
  end;
end;

procedure TFResample.FormShow(Sender: TObject);
var idxQuality: integer;
begin
  idxQuality := LazPaintInstance.Config.DefaultResampleQuality;
  if (idxQuality >= 0) and (idxQuality < ComboBox_Quality.Items.Count) then
    ComboBox_Quality.ItemIndex := idxQuality else
      ComboBox_Quality.ItemIndex := 0;
  CheckBox_Ratio.Checked := LazPaintInstance.Config.DefaultResampleKeepAspectRatio;

  SpinEdit_Width.Value := LazPaintInstance.Image.Width;
  SpinEdit_Height.Value := LazPaintInstance.Image.Height;
end;

procedure TFResample.SpinEdit_HeightChange(Sender: TObject);
begin
  if CheckBox_Ratio.Checked and (LazPaintInstance.Image.Height <> 0) then
    SpinEdit_Width.Value := round(SpinEdit_Height.Value/LazPaintInstance.Image.Height*
                                   LazPaintInstance.Image.Width);
end;

procedure TFResample.SpinEdit_WidthChange(Sender: TObject);
begin
  if CheckBox_Ratio.Checked and (LazPaintInstance.Image.Width <> 0) then
    SpinEdit_Height.Value := round(SpinEdit_Width.Value/LazPaintInstance.Image.Width*
                      LazPaintInstance.Image.Height);
end;

procedure TFResample.Button_OKClick(Sender: TObject);
begin
  if (SpinEdit_Width.Value = LazPaintInstance.Image.Width) and
    (SpinEdit_Height.Value = LazPaintInstance.Image.Height) then
    ModalResult := mrCancel
  else
  begin
    LazPaintInstance.Image.SaveLayerOrSelectionUndo; //just in case
    LazPaintInstance.Config.SetDefaultResampleQuality(ComboBox_Quality.ItemIndex);
    LazPaintInstance.Config.SetDefaultResampleKeepAspectRatio(CheckBox_Ratio.Checked);
    LazPaintInstance.Image.Resample(SpinEdit_Width.Value,SpinEdit_Height.Value,ComboBox_Quality.Text);

    ModalResult := mrOK;
  end;
end;

initialization
  {$I uresample.lrs}

end.

