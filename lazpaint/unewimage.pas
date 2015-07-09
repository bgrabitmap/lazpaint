unit UNewimage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Spin, Buttons, ComCtrls, BGRAVirtualScreen, uimage,
  LazPaintType, uscaledpi, BGRABitmap;

const
  shadowOffsetX = 3;
  shadowOffsetY = 3;
  shadowBlur= 3;

type

  { TFNewImage }

  TFNewImage = class(TForm)
    ToolBar8: TToolBar;
    ToolButton23: TToolButton;
    vsPreview: TBGRAVirtualScreen;
    Button_OK: TButton;
    Button_Cancel: TButton;
    Label_Width: TLabel;
    Label_Height: TLabel;
    SpinEdit_Height: TSpinEdit;
    SpinEdit_Width: TSpinEdit;
    procedure ToolButton23Click(Sender: TObject);
    procedure vsPreviewRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure Button_OKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpinEdit_SizeChange(Sender: TObject);
  private
    procedure UpdatePreview;
  public
    LazPaintInstance: TLazPaintCustomInstance;
    newImageResult: TBGRABitmap;
  end; 

function ShowNewImageDlg(Instance: TLazPaintCustomInstance; out tx,ty: integer):boolean;

implementation

uses umac, BGRABitmapTypes;

{ TFNewImage }

function ShowNewImageDlg(Instance: TLazPaintCustomInstance; out tx,ty: integer):boolean;
var
  NewImage: TFNewImage;
begin
  tx := 0;
  ty := 0;
  result := false;
  NewImage := nil;
  try
    NewImage:= TFNewImage.create(nil);
    NewImage.LazPaintInstance := Instance;
    result:= (NewImage.ShowModal = mrOk);
    tx:= NewImage.SpinEdit_Width.Value;
    ty:= NewImage.SpinEdit_Height.Value;
  except
    on ex:Exception do
    begin
      Instance.ShowError('ShowNewImageDlg',ex.Message);
      result := false;
    end;
  end;
  NewImage.free;
end;

procedure TFNewImage.Button_OKClick(Sender: TObject);
begin
  LazPaintInstance.Config.SetDefaultImageWidth(SpinEdit_Width.Value);
  LazPaintInstance.Config.SetDefaultImageHeight(SpinEdit_Height.Value);
  ModalResult:= mrOk;
end;

procedure TFNewImage.vsPreviewRedraw(Sender: TObject;
  Bitmap: TBGRABitmap);
var
  tx,ty,px,py,x,y: NativeInt;
  ratio: double;
  sx,sy: NativeInt;
  blur: TBGRACustomBitmap;
begin
  sx := vsPreview.Width- shadowOffsetX - shadowBlur;
  sy := vsPreview.Height- shadowOffsetY - shadowBlur;
  tx := SpinEdit_Width.Value;
  ty := SpinEdit_Height.Value;
  if (tx > 0) and (ty > 0) then
  begin
    ratio := tx/ty;
    if sx/ratio < vsPreview.Height then
    begin
      px := sx;
      py := round(sx/ratio);
      if py <= 0 then py := 1;
    end else
    begin
      px := round(sy*ratio);
      if px <= 0 then px := 1;
      py := sy;
    end;
    x := (sx-px) div 2;
    y := (sy-py) div 2;
    Bitmap.FillRect(x+shadowOffsetX,y+shadowOffsetY,x+shadowOffsetX+px,y+shadowOffsetY+py,BGRA(0,0,0,192),dmDrawWithTransparency);
    blur := bitmap.FilterBlurRadial(shadowBlur,rbFast);
    Bitmap.PutImage(0,0, blur,dmSet);
    blur.free;
    if (px = 1) or (py = 1) then
      Bitmap.FillRect(x,y,x+px,y+py,BGRABlack,dmSet)
    else
      Bitmap.Rectangle(x,y,x+px,y+py,BGRABlack,BGRAWhite,dmSet);
  end;
end;

procedure TFNewImage.ToolButton23Click(Sender: TObject);
var tx,ty: integer;
begin
  tx := SpinEdit_Width.Value;
  ty := SpinEdit_Height.Value;
  SpinEdit_Width.Value := ty;
  SpinEdit_Height.Value := tx;
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

  UpdatePreview;
  SafeSetFocus(SpinEdit_Width);
  SpinEdit_Width.SelectAll;
end;

procedure TFNewImage.SpinEdit_SizeChange(Sender: TObject);
begin
  UpdatePreview;
end;

procedure TFNewImage.UpdatePreview;
begin
  vsPreview.DiscardBitmap;
end;

{$R *.lfm}

end.

