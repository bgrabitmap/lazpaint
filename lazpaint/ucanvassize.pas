unit UCanvassize;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Spin, LazPaintType, uscaledpi, uresourcestrings, BGRABitmap, BGRALayers, uimage;

type
  { TFCanvasSize }

  TFCanvasSize = class(TForm)
    Button_Cancel: TButton;
    Button_OK: TButton;
    CheckBox_FlipMode: TCheckBox;
    ComboBox_Anchor: TComboBox;
    Label_Anchor: TLabel;
    Label_Width: TLabel;
    Label_Height: TLabel;
    SpinEdit_Height: TSpinEdit;
    SpinEdit_Width: TSpinEdit;
    procedure Button_OKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    LazPaintInstance: TLazPaintCustomInstance;
    canvasSizeResult: TLayeredBitmapAndSelection;
    repeatImage: boolean;
  end;

implementation

uses ugraph, bgrabitmaptypes, umac;

function ChangeLayeredImageCanvasSize(layeredBmp: TLazPaintImage; newWidth,
  newHeight: integer; anchor: string; background: TBGRAPixel;
  repeatImage: boolean; flipMode: boolean): TBGRALayeredBitmap;
var i,idx: integer;
begin
  result := TBGRALayeredBitmap.Create;
  for i := 0 to layeredbmp.NbLayers-1 do
  begin
    idx := result.AddOwnedLayer(ChangeCanvasSize(layeredbmp.LayerBitmap[i],newwidth,newHeight,anchor,background,repeatImage,flipMode),
      layeredBmp.BlendOperation[i],layeredbmp.LayerOpacity[i]);
    result.LayerName[idx] := layeredbmp.LayerName[i];
    result.LayerVisible[idx] := layeredbmp.LayerVisible[i];
  end;
end;

{ TFCanvasSize }

procedure TFCanvasSize.FormCreate(Sender: TObject);
begin
  ScaleDPI(Self,OriginalDPI);

  SpinEdit_Width.MaxValue := MaxImageWidth;
  SpinEdit_Height.MaxValue := MaxImageHeight;
  CheckOKCancelBtns(Button_OK,Button_Cancel);
  CheckSpinEdit(SpinEdit_Width);
  CheckSpinEdit(SpinEdit_Height);
  ComboBox_Anchor.ItemIndex := 4;
  repeatImage := false;
end;

procedure TFCanvasSize.Button_OKClick(Sender: TObject);
var anchor: string;
begin
  if (SpinEdit_Width.Value = LazPaintInstance.Image.Width) and
    (SpinEdit_Height.Value = LazPaintInstance.Image.Height) then
    ModalResult := mrCancel else
    begin
      if ComboBox_Anchor.ItemIndex = -1 then anchor := 'Middle' else
        anchor := ComboBox_Anchor.Items[ComboBox_Anchor.ItemIndex];

      canvasSizeResult.layeredBitmap := ChangeLayeredImageCanvasSize(LazPaintInstance.Image,SpinEdit_Width.Value,
         SpinEdit_Height.Value,anchor,BGRAPixelTransparent, repeatImage, CheckBox_FlipMode.Checked);
      if LazPaintInstance.Image.SelectionReadonly <> nil then
        canvasSizeResult.selection := ChangeCanvasSize(LazPaintInstance.Image.SelectionReadonly,SpinEdit_Width.Value,
          SpinEdit_Height.Value,anchor,BGRABlack, repeatImage, CheckBox_FlipMode.Checked);
      if LazPaintInstance.Image.SelectionLayerReadonly <> nil then
        canvasSizeResult.selectionLayer := ChangeCanvasSize(LazPaintInstance.Image.SelectionLayerReadonly,
           SpinEdit_Width.Value,SpinEdit_Height.Value,anchor,BGRAPixelTransparent, repeatImage, CheckBox_FlipMode.Checked);

      ModalResult := mrOK;
    end;
end;

procedure TFCanvasSize.FormShow(Sender: TObject);
begin
  canvasSizeResult.layeredBitmap := nil;
  canvasSizeResult.selection := nil;
  canvasSizeResult.selectionLayer := nil;

  SpinEdit_Width.Value := LazPaintInstance.Image.Width;
  SpinEdit_Height.Value := LazPaintInstance.Image.Height;
  if repeatImage then
  begin
    Caption := rsRepeatImage;
    CheckBox_FlipMode.Visible := true;
    ClientHeight := CheckBox_FlipMode.Top+CheckBox_FlipMode.Height+6+(ClientHeight-Button_OK.Top);
  end else
  begin
    Caption := rsCanvasSize;
    CheckBox_FlipMode.Visible := false;
    ClientHeight := CheckBox_FlipMode.Top+(ClientHeight-Button_OK.Top);
  end;
end;

initialization
  {$I ucanvassize.lrs}

end.

