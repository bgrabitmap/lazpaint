unit UCustomblur;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ExtDlgs,  bgrabitmap, LazPaintType, UScaleDPI,
  UResourceStrings, UFilterConnector;

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
    procedure Button_EditMaskClick(Sender: TObject);
    procedure Button_LoadMaskClick(Sender: TObject);
    procedure Button_OKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LoadMask(filename: string);
    procedure UpdatePreview;
  private
    subConfig: TStringStream;
    FLazPaintInstance: TLazPaintCustomInstance;
    FFilterConnector: TFilterConnector;
    procedure GenerateDefaultMask;
    procedure SetLazPaintInstance(const AValue: TLazPaintCustomInstance);
  public
    function ShowDlg(AFilterConnector: TObject): boolean;
    property LazPaintInstance: TLazPaintCustomInstance read FLazPaintInstance write SetLazPaintInstance;
  end;

implementation

uses umac,BGRABitmapTypes;

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
end;

procedure TFCustomBlur.FormShow(Sender: TObject);
begin
  UpdatePreview;
end;

procedure TFCustomBlur.LoadMask(filename: string);
var loadedImg, grayscale: TBGRABitmap;
    bmp: TBitmap;
begin
  loadedImg := TBGRABitmap.Create(UTF8ToSys( filename ));
  grayscale := loadedImg.FilterGrayscale as TBGRABitmap;
  loadedImg.Free;

  bmp := grayscale.MakeBitmapCopy(clBlack);
  grayscale.free;
  Image1.Picture.Assign(bmp);
  bmp.Free;
end;

procedure TFCustomBlur.UpdatePreview;
var mask,temp: TBGRABitmap;
begin
    mask := TBGRABitmap.Create(Image1.Picture.Width,Image1.Picture.Height);
    mask.Canvas.Draw(0,0,image1.picture.bitmap);
    mask.AlphaFill(255);
    temp := FFilterConnector.BackupLayer.FilterCustomBlur(FFilterConnector.WorkArea, mask) as TBGRABitmap;
    mask.Free;
    FFilterConnector.PutImage(temp,False);
    temp.Free;
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
  defaultMaskFilename: String;
begin
  FLazPaintInstance := AValue;
  defaultMaskFilename := LazPaintInstance.Config.DefaultCustomBlurMask;
  if (defaultMaskFilename = '') or not FileExists(defaultMaskFilename) then
    GenerateDefaultMask else
  begin
    try
      LoadMask(defaultMaskFilename);
    except
      on ex: Exception do
      begin
        LazPaintInstance.Config.SetDefaultCustomBlurMask('');
        GenerateDefaultMask;
      end;
    end;
  end;
end;

function TFCustomBlur.ShowDlg(AFilterConnector: TObject): boolean;
begin
  FFilterConnector := AFilterConnector as TFilterConnector;
  try
    if FFilterConnector.ActiveLayer <> nil then
      result:= (ShowModal = mrOk)
    else
      result := false;
  finally
    FFilterConnector := nil;
  end;
end;

procedure TFCustomBlur.Button_LoadMaskClick(Sender: TObject);
var filename: string;
begin
  if not OpenPictureDialog1.Execute then exit;
  try
    filename := OpenPictureDialog1.FileName;
    LoadMask(filename);
    LazPaintInstance.Config.SetDefaultCustomBlurMask(filename);
    self.Update;
    UpdatePreview;
  except
    on ex:Exception do
    begin
      MessageDlg(ex.Message,mtError,[mbOk],0);
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
    ShowMessage(ex.Message);
  end;
  bgraBmp.Free;
  self.Update;
  UpdatePreview;
end;

procedure TFCustomBlur.Button_OKClick(Sender: TObject);
begin
  FFilterConnector.ValidateAction;
  ModalResult := mrOK;
end;

initialization
  {$I ucustomblur.lrs}

end.

