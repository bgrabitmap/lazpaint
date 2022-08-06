// SPDX-License-Identifier: GPL-3.0-only
unit ULoading;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, BCPanel, types, BGRABitmap;

type

  { TFLoading }

  TFLoading = class(TForm)
    BGRAPanel1: TBCPanel;
    Timer1: TTimer;
    procedure BGRAPanel1AfterRenderBCPanel(Sender: TObject;
      const ABGRA: TBGRABitmap; {%H-}ARect: TRect);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    { private declarations }
  public
    LoadingStatus: string;
    WantedTimeOut: integer;
    { public declarations }
    procedure SetTimeOut(AMillisecond: integer);
    procedure ShowMessage(AMessage: string; AMillisecond: integer = 0);
    procedure HideMessage;
  end;

procedure MessagePopup(AMessage: string; AMillisecond: integer);
procedure MessagePopupForever(AMessage: string);
procedure MessagePopupHide;

implementation

uses BGRALayers, BGRAReadLzp, LCScaleDPI, LazPaintType, BGRABitmapTypes;

const MarginTopBottom = 3;
      MarginLeftRight = 3;

var PopupWindow: TFLoading;
    PopupFontFullHeight: integer;

procedure MessagePopup(AMessage: string; AMillisecond: integer);
begin
  if AMillisecond <= 0 then AMillisecond:= 1000;
  if PopupWindow= nil then
    PopupWindow := TFLoading.Create(nil);
  PopupWindow.ShowMessage(AMessage, AMillisecond);
end;

procedure MessagePopupForever(AMessage: string);
begin
  if PopupWindow= nil then
    PopupWindow := TFLoading.Create(nil);
  PopupWindow.ShowMessage(AMessage, 0);
  PopupWindow.SetTimeOut(0);
end;

procedure MessagePopupHide;
begin
  if PopupWindow <> nil then FreeAndNil(PopupWindow);
end;

{ TFLoading }

procedure TFLoading.FormCreate(Sender: TObject);
begin
end;

procedure TFLoading.BGRAPanel1AfterRenderBCPanel(Sender: TObject;
  const ABGRA: TBGRABitmap; ARect: TRect);
begin
  {$IFDEF LINUX}
  ABGRA.FontQuality := fqSystemClearType;
  {$ELSE}
  ABGRA.FontQuality := fqFineAntialiasing;
  {$ENDIF}
  ABGRA.FontFullHeight:= PopupFontFullHeight;
  ABGRA.TextOut(MarginLeftRight,MarginTopBottom,LoadingStatus,BGRABlack);
  if WantedTimeOut <> 0 then SetTimeOut(WantedTimeOut);
end;

procedure TFLoading.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled:= false;
  HideMessage;
end;

procedure TFLoading.ShowMessage(AMessage: string; AMillisecond: integer);
var bmp: TBGRABitmap;
begin
  bmp := TBGRABitmap.Create(0,0);
  {$IFDEF LINUX}
  bmp.FontQuality := fqSystemClearType;
  {$ELSE}
  bmp.FontQuality := fqFineAntialiasing;
  {$ENDIF}
  bmp.FontFullHeight:= PopupFontFullHeight;
  self.LoadingStatus := AMessage;
  with bmp.TextSize(AMessage) do
  begin
    self.ClientWidth := cx+2*MarginLeftRight;
    self.ClientHeight := cy+2*MarginTopBottom;
  end;
  bmp.Free;
  self.Left := (Screen.Width-self.Width) div 2;
  self.Top := (Screen.Height-self.Height) div 2;
  if not self.Visible then self.Show else BGRAPanel1.UpdateControl;
  if AMillisecond <> 0 then
    SetTimeOut(AMillisecond);
  WantedTimeOut := AMillisecond;
end;

procedure TFLoading.HideMessage;
begin
  if self.Visible then self.Hide;
  Update;
end;

procedure TFLoading.SetTimeOut(AMillisecond: integer);
begin
  if AMillisecond = 0 then
    Timer1.Enabled:= false
  else
    begin
      Timer1.Enabled := false;
      Timer1.Interval := AMillisecond;
      Timer1.Enabled := true;
    end;
end;

{$R *.lfm}

initialization
  PopupFontFullHeight := DoScaleY(20,OriginalDPI);

finalization

  PopupWindow.Free;

end.

