unit uradialblur;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Spin, ExtCtrls, BGRABitmap, BGRABitmapTypes, LazPaintType, uscaledpi;

type

  { TFRadialBlur }

  TFRadialBlur = class(TForm)
    Button_OK: TButton;
    Button_Cancel: TButton;
    Button3: TButton;
    Image1: TImage;
    SpinEdit_Radius: TSpinEdit;
    Label3: TLabel;
    procedure Button_OKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FlazPaintInstance: TLazPaintCustomInstance;
    procedure SetlazPaintInstance(const AValue: TLazPaintCustomInstance);
  public
    blurType: TRadialBlurType;
    sourceLayer, filteredLayer: TBGRABitmap;
    property LazPaintInstance: TLazPaintCustomInstance read FlazPaintInstance write SetlazPaintInstance;
  end;

function ShowRadialBlurDlg(Instance: TLazPaintCustomInstance; layer:TBGRABitmap; out filteredLayer: TBGRABitmap;blurType:TRadialBlurType):boolean;

implementation

uses umac;

{ TFRadialBlur }

function ShowRadialBlurDlg(Instance: TLazPaintCustomInstance; layer:TBGRABitmap; out filteredLayer: TBGRABitmap;blurType:TRadialBlurType):boolean;
var
  RadialBlur: TFRadialBlur;
begin
  filteredLayer := nil;
  result := false;
  RadialBlur:= TFRadialBlur.create(nil);
  RadialBlur.lazPaintInstance := Instance;
  try
    RadialBlur.blurType := blurType;
    RadialBlur.sourceLayer := layer;
    result:= (RadialBlur.ShowModal = mrOk);
    filteredLayer := RadialBlur.filteredLayer;
  finally
    RadialBlur.free;
  end;
end;

procedure TFRadialBlur.Button_OKClick(Sender: TObject);
begin
    if sourceLayer <> nil then
    begin
      filteredLayer := sourceLayer.FilterBlurRadial(SpinEdit_Radius.Value, blurType) as TBGRABitmap;
      lazPaintInstance.Config.SetDefaultBlurRadius(SpinEdit_Radius.Value);
      ModalResult := mrOK;
    end else
      ModalResult := mrCancel;
end;

procedure TFRadialBlur.FormCreate(Sender: TObject);
begin
  ScaleDPI(Self,OriginalDPI);

  blurType := rbNormal;
  CheckOKCancelBtns(Button_OK,Button_Cancel);
  CheckSpinEdit(SpinEdit_Radius);
  filteredLayer := nil;
end;

procedure TFRadialBlur.SetlazPaintInstance(const AValue: TLazPaintCustomInstance);
begin
  if FlazPaintInstance=AValue then exit;
  FlazPaintInstance:=AValue;
  SpinEdit_Radius.Value := LazPaintInstance.Config.DefaultBlurRadius;
end;

initialization
  {$I uradialblur.lrs}

end.

