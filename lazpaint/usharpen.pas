unit USharpen;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Spin, UFilterConnector;

type
  TSharpenMode = (smSharpen);

  { TFSharpen }

  TFSharpen = class(TForm)
    Button_Cancel: TButton;
    Button_OK: TButton;
    Label_Amount: TLabel;
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
    procedure PreviewNeeded;
  end;

function ShowSharpenDlg(AFilterConnector: TObject; AMode : TSharpenMode):boolean;

implementation

uses UScaleDPI, UMac, LazPaintType, BGRABitmap, BGRABitmapTypes;

function ShowSharpenDlg(AFilterConnector: TObject; AMode : TSharpenMode): boolean;
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
      result := false;
      exit;
    end;
  end;
  try
    result := FSharpen.ShowModal = mrOK;
  finally
    FSharpen.FFilterConnector.OnTryStopAction := nil;
    FSharpen.Free;
  end;
end;

{ TFSharpen }

procedure TFSharpen.FormCreate(Sender: TObject);
begin
  ScaleDPI(Self,OriginalDPI);

  CheckOKCancelBtns(Button_OK,Button_Cancel);
  CheckSpinEdit(SpinEdit_Amount);
  FMode := smSharpen;
end;

procedure TFSharpen.FormShow(Sender: TObject);
var idxSlash: integer;
begin
  FInitializing := true;
  SpinEdit_Amount.Value := round(FFilterConnector.LazPaintInstance.Config.DefaultSharpenAmount*100);
  FInitializing := false;
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

procedure TFSharpen.PreviewNeeded;
var filtered: TBGRABitmap;
begin
  if FMode = smSharpen then
    filtered := FFilterConnector.BackupLayer.FilterSharpen(FFilterConnector.WorkArea,SpinEdit_Amount.Value/100) as TBGRABitmap;
  FFilterConnector.PutImage(filtered,False,True);
end;

{$R *.lfm}

end.

