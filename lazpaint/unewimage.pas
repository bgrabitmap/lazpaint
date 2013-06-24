unit UNewimage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Spin, uimage, LazPaintType, uscaledpi, BGRABitmap;

type

  { TFNewImage }

  TFNewImage = class(TForm)
    Button_OK: TButton;
    Button_Cancel: TButton;
    Label_Width: TLabel;
    Label_Height: TLabel;
    SpinEdit_Height: TSpinEdit;
    SpinEdit_Width: TSpinEdit;
    procedure Button_OKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
    LazPaintInstance: TLazPaintCustomInstance;
    newImageResult: TBGRABitmap;
  end; 

function ShowNewImageDlg(Instance: TLazPaintCustomInstance; out bitmap: TBGRABitmap):boolean;

implementation

uses umac;

{ TFNewImage }

function ShowNewImageDlg(Instance: TLazPaintCustomInstance; out bitmap: TBGRABitmap):boolean;
var
  NewImage: TFNewImage;
begin
  bitmap := nil;
  result := false;
  NewImage := nil;
  try
    NewImage:= TFNewImage.create(nil);
    NewImage.LazPaintInstance := Instance;
    result:= (NewImage.ShowModal = mrOk);
    bitmap:= NewImage.newImageResult;
  except
    on ex:Exception do
    begin
      ShowMessage('ShowNewImageDlg: '+ex.Message);
      result := false;
    end;
  end;
  NewImage.free;
end;

procedure TFNewImage.Button_OKClick(Sender: TObject);
begin
  newImageResult := LazPaintInstance.MakeNewBitmapReplacement(SpinEdit_Width.Value, SpinEdit_Height.Value);
  LazPaintInstance.Config.SetDefaultImageWidth(SpinEdit_Width.Value);
  LazPaintInstance.Config.SetDefaultImageHeight(SpinEdit_Height.Value);
  ModalResult:= mrOk;
end;

procedure TFNewImage.FormCreate(Sender: TObject);
begin
  ScaleDPI(Self,OriginalDPI);

  SpinEdit_Width.MaxValue := MaxImageWidth;
  SpinEdit_Height.MaxValue := MaxImageHeight;
  CheckOKCancelBtns(Button_OK,Button_Cancel);
  CheckSpinEdit(SpinEdit_Width);
  CheckSpinEdit(SpinEdit_Height);
  newImageResult := nil;
end;

procedure TFNewImage.FormShow(Sender: TObject);
begin
  SpinEdit_Width.Value := LazPaintInstance.Config.DefaultImageWidth;
  SpinEdit_Height.Value := LazPaintInstance.Config.DefaultImageHeight;
end;

initialization
  {$I unewimage.lrs}

end.

