// SPDX-License-Identifier: GPL-3.0-only
unit USharpen;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Spin, ExtCtrls, UFilterConnector, UScripting;

type
  TSharpenMode = (smSharpen);

  { TFSharpen }

  TFSharpen = class(TForm)
    Button_Cancel: TButton;
    Button_OK: TButton;
    Label_Amount: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    SpinEdit_Amount: TSpinEdit;
    procedure Button_OKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpinEdit_AmountChange(Sender: TObject);
  protected
    procedure OnTryStopAction({%H-}sender: TFilterConnector);
  private
    { private declarations }
    FMode :TSharpenMode;
    FInitializing: boolean;
    FFilterConnector: TFilterConnector;
    procedure InitParams;
    procedure PreviewNeeded;
  end;

function ShowSharpenDlg(AFilterConnector: TObject; AMode : TSharpenMode): TScriptResult;

implementation

uses LCScaleDPI, UMac, LazPaintType, BGRABitmap, BGRABitmapTypes;

function ShowSharpenDlg(AFilterConnector: TObject; AMode : TSharpenMode): TScriptResult;
var FSharpen: TFSharpen;
begin
  FSharpen := TFSharpen.Create(nil);
  FSharpen.FMode := AMode;
  try
    FSharpen.FFilterConnector := AFilterConnector as TFilterConnector;
    FSharpen.FFilterConnector.OnTryStopAction := @FSharpen.OnTryStopAction;
  except
    on ex: exception do
    begin
      (AFilterConnector as TFilterConnector).LazPaintInstance.ShowError('ShowSharpenDlg',ex.Message);
      result := srException;
      exit;
    end;
  end;
  try
    if Assigned(FSharpen.FFilterConnector.Parameters) and
      FSharpen.FFilterConnector.Parameters.Booleans['Validate'] then
    begin
      FSharpen.InitParams;
      FSharpen.PreviewNeeded;
      FSharpen.FFilterConnector.ValidateAction;
      result := srOk;
    end else
    begin
      if FSharpen.ShowModal = mrOK then
        result := srOk else result := srCancelledByUser;
    end;
  finally
    FSharpen.FFilterConnector.OnTryStopAction := nil;
    FSharpen.Free;
  end;
end;

{ TFSharpen }

procedure TFSharpen.FormCreate(Sender: TObject);
begin
  ScaleControl(Self,OriginalDPI);

  CheckOKCancelBtns(Button_OK{,Button_Cancel});
  CheckSpinEdit(SpinEdit_Amount);
  FMode := smSharpen;
end;

procedure TFSharpen.FormShow(Sender: TObject);
var idxSlash: integer;
begin
  InitParams;
  PreviewNeeded;
  idxSlash:= Pos('/',Caption);
  if idxSlash <> 0 then
  begin
    if FMode = smSharpen then Caption := Trim(copy(Caption,1,idxSlash-1)) else
        Caption := Trim(Copy(Caption,idxSlash+1,length(Caption)-idxSlash));
  end;
  Top := FFilterConnector.LazPaintInstance.MainFormBounds.Top;
end;

procedure TFSharpen.SpinEdit_AmountChange(Sender: TObject);
begin
  if not FInitializing then PreviewNeeded;
end;

procedure TFSharpen.Button_OKClick(Sender: TObject);
begin
  FFilterConnector.ValidateAction;
  FFilterConnector.LazPaintInstance.Config.SetDefaultSharpenAmount(SpinEdit_Amount.Value/100);
  ModalResult := mrOK;
end;

procedure TFSharpen.OnTryStopAction(sender: TFilterConnector);
begin
  if self.visible then Close;
end;

procedure TFSharpen.InitParams;
begin
  FInitializing := true;
  if Assigned(FFilterConnector.Parameters) and FFilterConnector.Parameters.IsDefined('Amount') then
    SpinEdit_Amount.Value := round(FFilterConnector.Parameters.Floats['Amount']*100)
  else
     SpinEdit_Amount.Value := round(FFilterConnector.LazPaintInstance.Config.DefaultSharpenAmount*100);
  FInitializing := false;
end;

procedure TFSharpen.PreviewNeeded;
var filtered: TBGRABitmap;
begin
  if FMode = smSharpen then
  begin
    filtered := FFilterConnector.BackupLayer.FilterSharpen(FFilterConnector.WorkArea,SpinEdit_Amount.Value/100) as TBGRABitmap;
    FFilterConnector.PutImage(filtered,False,True);
  end;
end;

{$R *.lfm}

end.

