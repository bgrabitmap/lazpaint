unit ULoading;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, BCPanel;

type

  { TFLoading }

  TFLoading = class(TForm)
    BGRAPanel1: TBCPanel;
    Label_LoadingStatus: TLabel;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure ShowMessage(AMessage: string);
    procedure HideMessage;
    procedure SetTimoutOut(AMillisecond: integer);
  end;

procedure MessagePopup(AMessage: string; AMillisecond: integer);

implementation

uses BGRALayers;

const MarginTopBottom = 3;
      MarginLeftRight = 3;

var PopupWindow: TFLoading;

procedure MessagePopup(AMessage: string; AMillisecond: integer);
begin
  if AMillisecond <= 0 then AMillisecond:= 1000;
  if PopupWindow= nil then
    PopupWindow := TFLoading.Create(nil);
  PopupWindow.ShowMessage(AMessage);
  PopupWindow.SetTimoutOut(AMillisecond);
end;

{ TFLoading }

procedure TFLoading.FormCreate(Sender: TObject);
begin
  Label_LoadingStatus.Left := MarginLeftRight;
  Label_LoadingStatus.Top := MarginTopBottom;
  self.ClientWidth := Label_LoadingStatus.Width+2*MarginLeftRight;
  self.ClientHeight := Label_LoadingStatus.Height+2*MarginTopBottom;
end;

procedure TFLoading.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled:= false;
  HideMessage;
end;

procedure TFLoading.ShowMessage(AMessage: string);
begin
  if not self.Visible then self.Show;
  Label_LoadingStatus.Caption := AMessage;
  self.ClientWidth := Label_LoadingStatus.Width+2*MarginLeftRight;
  self.ClientHeight := Label_LoadingStatus.Height+2*MarginTopBottom;
  self.Left := (Screen.Width-self.Width) div 2;
  self.Top := (Screen.Height-self.Height) div 2;
  Update;
end;

procedure TFLoading.HideMessage;
begin
  if self.Visible then self.Hide;
  Update;
end;

procedure TFLoading.SetTimoutOut(AMillisecond: integer);
begin
  Timer1.Interval := AMillisecond;
  Timer1.Enabled := true;
end;

initialization
  {$I uloading.lrs}

finalization

  PopupWindow.Free;

end.

