// SPDX-License-Identifier: GPL-3.0-only
unit ugeometricbrush;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, BGRAVirtualScreen, UBrushType, BGRABitmap,
  LazPaintType;

type

  { TFGeometricBrush }

  TFGeometricBrush = class(TForm)
    BGRAVirtualScreen1: TBGRAVirtualScreen;
    Button_Cancel: TButton;
    Button_OK: TButton;
    CheckBox_IsGradient: TCheckBox;
    Label1: TLabel;
    TrackBar_SideCount: TTrackBar;
    procedure BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure CheckBox_IsGradientChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TrackBar_SideCountChange(Sender: TObject);
  private
    { private declarations }
    FBrush: TLazPaintBrush;
  public
    { public declarations }
  end;

function ShowGeometricBrushDialog(AInstance: TLazPaintCustomInstance) : TLazPaintBrush;

implementation

uses BGRABitmapTypes, LCScaleDPI, umac;

function ShowGeometricBrushDialog(AInstance: TLazPaintCustomInstance): TLazPaintBrush;
var f:TFGeometricBrush;
  title: string;
begin
  result := nil;
  try
    f := TFGeometricBrush.Create(nil);
    title := f.Caption;
    if f.ShowModal = mrOK then
      begin
        result := f.FBrush;
        f.FBrush := nil;
      end;
    f.Free;
  except
    on ex:exception do
      AInstance.ShowError(title, ex.Message);
  end;
end;

{ TFGeometricBrush }

procedure TFGeometricBrush.TrackBar_SideCountChange(Sender: TObject);
begin
  if TrackBar_SideCount.Position = TrackBar_SideCount.Max then
    FBrush.SideCount := 0
  else
    FBrush.SideCount := TrackBar_SideCount.Position;
  BGRAVirtualScreen1.RedrawBitmap;
end;

procedure TFGeometricBrush.CheckBox_IsGradientChange(Sender: TObject);
begin
  FBrush.IsGradient := CheckBox_IsGradient.Checked;
  BGRAVirtualScreen1.RedrawBitmap;
end;

procedure TFGeometricBrush.BGRAVirtualScreen1Redraw(Sender: TObject;
  Bitmap: TBGRABitmap);
begin
  FBrush.Size := Bitmap.Width * 0.7;
  FBrush.Put(Bitmap, round(Bitmap.width/2),round(Bitmap.Height/2), BGRABlack);
end;

procedure TFGeometricBrush.FormCreate(Sender: TObject);
begin
  ScaleControl(Self,OriginalDPI);
  CheckOKCancelBtns(Button_OK,Button_Cancel);

  FBrush := TLazPaintBrush.Create;
  CheckBox_IsGradient.Checked := FBrush.IsGradient;
  if FBrush.SideCount = 0 then
    TrackBar_SideCount.Position := TrackBar_SideCount.Max
  else
    TrackBar_SideCount.Position := FBrush.SideCount;
end;

procedure TFGeometricBrush.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FBrush);
end;

{$R *.lfm}

end.

