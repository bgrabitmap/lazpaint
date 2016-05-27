unit UCustomblur;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ExtDlgs,  bgrabitmap, LazPaintType, UScaleDPI,
  UResourceStrings, UFilterConnector, UFilterThread, ubrowseimages;

type

  { TFCustomBlur }

  TFCustomBlur = class(TForm)
    Button_LoadMask: TButton;
    Button_EditMask: TButton;
    Button_OK: TButton;
    Button_Cancel: TButton;
    btnLoadMask: TButton;
    Image1: TImage;
    OpenPictureDialog1: TOpenPictureDialog;
    Timer1: TTimer;
    procedure Button_EditMaskClick(Sender: TObject);
    procedure Button_LoadMaskClick(Sender: TObject);
    procedure Button_OKClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LoadMask(filenameUTF8: string);
    procedure PreviewNeeded;
    procedure Timer1Timer(Sender: TObject);
  private
    FBrowseImages: TFBrowseImages;
    subConfig: TStringStream;
    FLazPaintInstance: TLazPaintCustomInstance;
    FFilterConnector: TFilterConnector;
    FThreadManager: TFilterThreadManager;
    procedure GenerateDefaultMask;
    procedure SetLazPaintInstance(const AValue: TLazPaintCustomInstance);
    procedure OnTaskEvent({%H-}ASender: TObject; AEvent: TThreadManagerEvent);
  public
    function ShowDlg(AFilterConnector: TObject): boolean;
    property LazPaintInstance: TLazPaintCustomInstance read FLazPaintInstance write SetLazPaintInstance;
  end;

implementation

uses umac,BGRABitmapTypes, BGRAFilters;

{ TFCustomBlur }

procedure TFCustomBlur.FormCreate(Sender: TObject);
begin
  ScaleDPI(Self,OriginalDPI);

  CheckOKCancelBtns(Button_OK,Button_Cancel);
  subConfig := TStringStream.Create('[Tool]'+LineEnding+
    'ForeColor=FFFFFFFF'+LineEnding+
    'BackColor=000000FF'+LineEnding+
    'PenWidth=1');
end;

procedure TFCustomBlur.FormDestroy(Sender: TObject);
begin
  subConfig.Free;
  FreeAndNil(FBrowseImages);
end;

procedure TFCustomBlur.FormShow(Sender: TObject);
begin
  PreviewNeeded;
end;

procedure TFCustomBlur.LoadMask(filenameUTF8: string);
var loadedImg, grayscale: TBGRABitmap;
    bmp: TBitmap;
begin
  loadedImg := TBGRABitmap.Create(filenameUTF8,True);
  grayscale := loadedImg.FilterGrayscale as TBGRABitmap;
  loadedImg.Free;

  bmp := grayscale.MakeBitmapCopy(clBlack);
  grayscale.free;
  Image1.Picture.Assign(bmp);
  bmp.Free;
end;

procedure TFCustomBlur.PreviewNeeded;
var mask: TBGRABitmap;
begin
  mask := TBGRABitmap.Create(Image1.Picture.Width,Image1.Picture.Height,BGRABlack);
  mask.Canvas.Draw(0,0,image1.picture.bitmap);
  FThreadManager.WantPreview(CreateBlurTask(FFilterConnector.BackupLayer,FFilterConnector.WorkArea, mask, false));
  mask.Free;
end;

procedure TFCustomBlur.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled:= false;
  FThreadManager.RegularCheck;
  Timer1.Interval := 200;
  Timer1.Enabled:= true;
end;

procedure TFCustomBlur.GenerateDefaultMask;
var bmp: TBitmap;
    defaultMask: TBGRABitmap;
begin
  defaultMask := TBGRABitmap.Create(11,11);
  defaultMask.GradientFill(0,0,11,11,BGRAWhite,BGRABlack,gtRadial,pointf(5,5),pointf(-0.5,5),dmSet);
  bmp := defaultMask.MakeBitmapCopy(clBlack);
  defaultMask.Free;
  Image1.Picture.Assign(bmp);
  bmp.Free;
end;

procedure TFCustomBlur.SetLazPaintInstance(const AValue: TLazPaintCustomInstance);
var
  defaultMaskFilenameUTF8: String;
begin
  FLazPaintInstance := AValue;
  defaultMaskFilenameUTF8 := LazPaintInstance.Config.DefaultCustomBlurMaskUTF8;
  if (defaultMaskFilenameUTF8 = '') or not FileExistsUTF8(defaultMaskFilenameUTF8) then
    GenerateDefaultMask else
  begin
    try
      LoadMask(defaultMaskFilenameUTF8);
    except
      on ex: Exception do
      begin
        LazPaintInstance.Config.SetDefaultCustomBlurMaskUTF8('');
        GenerateDefaultMask;
      end;
    end;
  end;
end;

procedure TFCustomBlur.OnTaskEvent(ASender: TObject; AEvent: TThreadManagerEvent
  );
begin
  case AEvent of
  tmeAbortedTask,tmeCompletedTask:
    begin
      Timer1.Enabled := false;
      if FThreadManager.ReadyToClose then
        Close
      else
        if AEvent = tmeCompletedTask then Button_OK.Enabled := true;
    end;
  tmeStartingNewTask:
    begin
      Timer1.Enabled := false;
      Timer1.Interval := 100;
      Timer1.Enabled := true;
      Button_OK.Enabled := false;
    end;
  end;
end;

function TFCustomBlur.ShowDlg(AFilterConnector: TObject): boolean;
begin
  FFilterConnector := AFilterConnector as TFilterConnector;
  FThreadManager := TFilterThreadManager.Create(FFilterConnector);
  FThreadManager.OnEvent := @OnTaskEvent;
  try
    if FFilterConnector.ActiveLayer <> nil then
      result:= (ShowModal = mrOk)
    else
      result := false;
  finally
    FFilterConnector := nil;
    FThreadManager.Free;
  end;
end;

procedure TFCustomBlur.Button_LoadMaskClick(Sender: TObject);
var filenameUTF8: string;
begin
  filenameUTF8 := '';
  if LazPaintInstance.Config.DefaultUseImageBrowser then
  begin
    if not assigned(FBrowseImages) then
    begin
      FBrowseImages := TFBrowseImages.Create(self);
      FBrowseImages.LazPaintInstance := LazPaintInstance;
      FBrowseImages.AllowMultiSelect := false;
    end;
    if FBrowseImages.ShowModal = mrOK then
      filenameUTF8 := FBrowseImages.Filename;
  end else
  begin
    if OpenPictureDialog1.Execute then filenameUTF8 := OpenPictureDialog1.FileName;
  end;
  if filenameUTF8 <> '' then
    begin
      try
        LoadMask(filenameUTF8);
        LazPaintInstance.Config.SetDefaultCustomBlurMaskUTF8(filenameUTF8);
        self.Update;
        PreviewNeeded;
      except
        on ex:Exception do
        begin
          LazPaintInstance.ShowError('LoadMask',ex.Message);
        end;
      end;
    end;
end;

procedure TFCustomBlur.Button_EditMaskClick(Sender: TObject);
var bgraBmp: TBGRABitmap;
    bmpCopy: TBitmap;
begin
  bgraBmp := TBGRABitmap.Create(Image1.Picture.Width,Image1.Picture.Height);
  bgraBmp.Canvas.Draw(0,0,image1.picture.bitmap);
  bgraBmp.AlphaFill(255);
  try
    LazPaintInstance.EditBitmap(bgraBmp,subConfig,rsEditMask);
    bgraBmp.InplaceGrayscale;
    bmpCopy := bgraBmp.MakeBitmapCopy(clBlack);
    try
      Image1.Picture.Assign(bmpCopy);
    finally
      bmpCopy.Free;
    end;
  except on ex: exception do
    LazPaintInstance.ShowError('EditMask', ex.Message);
  end;
  bgraBmp.Free;
  self.Update;
  PreviewNeeded;
end;

procedure TFCustomBlur.Button_OKClick(Sender: TObject);
begin
  if not FFilterConnector.ActionDone then FFilterConnector.ValidateAction;
  ModalResult := mrOK;
end;

procedure TFCustomBlur.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  FThreadManager.Quit;
  CanClose := FThreadManager.ReadyToClose;
end;

{$R *.lfm}

end.

