unit UNewimage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Spin, Buttons, ComCtrls, ExtCtrls, BGRAVirtualScreen, BGRAShape,
  uimage, LazPaintType, uscaledpi, BGRABitmap, BGRABitmapTypes;

const
  shadowOffsetX = 3;
  shadowOffsetY = 3;
  shadowBlur= 3;

type
  TLastEnteredValue = (valWidth,valHeight,valRatio);

  { TFNewImage }

  TFNewImage = class(TForm)
    BGRAShape1: TBGRAShape;
    BGRAShape10: TBGRAShape;
    BGRAShape2: TBGRAShape;
    BGRAShape3: TBGRAShape;
    BGRAShape4: TBGRAShape;
    BGRAShape5: TBGRAShape;
    BGRAShape6: TBGRAShape;
    BGRAShape7: TBGRAShape;
    BGRAShape8: TBGRAShape;
    BGRAShape9: TBGRAShape;
    ComboBox_Ratio: TComboBox;
    Image1: TImage;
    Label_MemoryRequiredValue: TLabel;
    Label_Height1: TLabel;
    Label_MemoryRequired: TLabel;
    ToolBar8: TToolBar;
    ToolButton23: TToolButton;
    vsPreview: TBGRAVirtualScreen;
    Button_OK: TButton;
    Button_Cancel: TButton;
    Label_Width: TLabel;
    Label_Height: TLabel;
    SpinEdit_Height: TSpinEdit;
    SpinEdit_Width: TSpinEdit;
    procedure BGRAShapeClick(Sender: TObject);
    procedure ComboBox_RatioChange(Sender: TObject);
    procedure SpinEdit_HeightChange(Sender: TObject);
    procedure ToolButton23Click(Sender: TObject);
    procedure vsPreviewRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure Button_OKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpinEdit_WidthChange(Sender: TObject);
  private
    FLastEnteredValue: TLastEnteredValue;
    FRatio: double;
    FRecomputing: boolean;
    FBackColor: TBGRAPixel;
    procedure UpdatePreview;
  public
    LazPaintInstance: TLazPaintCustomInstance;
    newImageResult: TBGRABitmap;
  end; 

function ShowNewImageDlg(Instance: TLazPaintCustomInstance; out tx,ty: integer):boolean;

implementation

uses umac, UMySLV, UResourceStrings;

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
    begin
      Bitmap.DrawCheckers(rect(x,y,x+px,y+py),BGRAWhite,CSSSilver);
      Bitmap.Rectangle(x,y,x+px,y+py,BGRABlack,FBackColor,dmDrawWithTransparency);
    end;
  end;
end;

procedure TFNewImage.ToolButton23Click(Sender: TObject);
var tx,ty: integer;
  s: string;
  idxCol: integer;
begin
  if FRecomputing then exit;
  FRecomputing:= true;
  tx := SpinEdit_Width.Value;
  ty := SpinEdit_Height.Value;
  SpinEdit_Width.Value := ty;
  SpinEdit_Height.Value := tx;
  if FRatio <> 0 then
  begin
    FRatio := 1/FRatio;
    s := ComboBox_Ratio.Text;
    idxCol := pos(':',s);
    if idxCol <> 0 then
    begin
      s := copy(s,idxCol+1,length(s)-idxCol)+':'+ copy(s,1,idxCol-1);
      ComboBox_Ratio.Text := s;
    end;
  end;
  FRecomputing:= false;
  if FLastEnteredValue = valWidth then
    FLastEnteredValue:= valHeight else
  if FLastEnteredValue = valHeight then
    FLastEnteredValue:= valWidth;
  UpdatePreview;
end;

procedure TFNewImage.ComboBox_RatioChange(Sender: TObject);
var s: string;
  idxCol: integer;
  num,denom: double;
  errPos: integer;
begin
  if FRecomputing then exit;
  s := stringreplace(ComboBox_Ratio.Text,FormatSettings.DecimalSeparator,'.',[rfReplaceAll]);
  if s = '' then
  begin
    FRatio := 0;
    exit;
  end;

  idxCol := pos(':',s);
  if idxCol = 0 then exit;
  val(copy(s,1,idxCol-1),num,errPos);
  if errPos <> 0 then exit;
  if num < 0 then exit;
  val(copy(s,idxCol+1,length(s)-idxCol),denom,errPos);
  if errPos <> 0 then exit;
  if denom <= 0 then exit;
  FRatio := num/denom;

  FRecomputing:= true;
  if FRatio <> 0 then
  begin
    if FLastEnteredValue = valHeight then
      SpinEdit_Width.Value := round(SpinEdit_Height.Value*FRatio)
    else
      SpinEdit_Height.Value := round(SpinEdit_Width.Value/FRatio);
  end else
    FLastEnteredValue:= valRatio;
  FRecomputing:= false;
  UpdatePreview;
end;

procedure TFNewImage.BGRAShapeClick(Sender: TObject);
begin
  with (Sender as TBGRAShape) do
    FBackColor:= ColorToBGRA(FillColor,FillOpacity);
  UpdatePreview;
end;

procedure TFNewImage.SpinEdit_HeightChange(Sender: TObject);
begin
  if FRecomputing then exit;
  FRecomputing:= true;
  if (FLastEnteredValue = valRatio) and (FRatio <> 0) then
  begin
    SpinEdit_Width.Value := round(SpinEdit_Height.Value*FRatio);
  end else
  begin
    FLastEnteredValue := valWidth;
    FRatio := 0;
    ComboBox_Ratio.Text := '';
  end;
  FRecomputing:= false;
  UpdatePreview;
  FLastEnteredValue:= valHeight;
end;

procedure TFNewImage.FormCreate(Sender: TObject);
begin
  ScaleDPI(Self,OriginalDPI);

  FRecomputing := true;
  SpinEdit_Width.MaxValue := MaxImageWidth;
  SpinEdit_Height.MaxValue := MaxImageHeight;
  FLastEnteredValue:= valWidth;
  FRecomputing := false;
  CheckOKCancelBtns(Button_OK,Button_Cancel);
  CheckSpinEdit(SpinEdit_Width);
  CheckSpinEdit(SpinEdit_Height);
  newImageResult := nil;
  FBackColor:= BGRAPixelTransparent;
end;

procedure TFNewImage.FormShow(Sender: TObject);
begin
  ToolBar8.Images := LazPaintInstance.Icons[DoScaleY(16,OriginalDPI)];
  Label_MemoryRequiredValue.Left := Label_MemoryRequired.BoundsRect.Right + DoScaleX(4,OriginalDPI);

  FRecomputing := true;
  SpinEdit_Width.Value := LazPaintInstance.Config.DefaultImageWidth;
  SpinEdit_Height.Value := LazPaintInstance.Config.DefaultImageHeight;
  FRecomputing := false;

  UpdatePreview;
  SafeSetFocus(SpinEdit_Width);
  SpinEdit_Width.SelectAll;
end;

procedure TFNewImage.SpinEdit_WidthChange(Sender: TObject);
begin
  if FRecomputing then exit;
  FRecomputing:= true;
  if (FLastEnteredValue = valRatio) and (FRatio <> 0) then
  begin
    SpinEdit_Height.Value := round(SpinEdit_Width.Value/FRatio);
  end else
  begin
    FLastEnteredValue := valWidth;
    FRatio := 0;
    ComboBox_Ratio.Text := '';
  end;
  FRecomputing:= false;
  UpdatePreview;
  FLastEnteredValue:= valWidth;
end;

procedure TFNewImage.UpdatePreview;
begin
  vsPreview.DiscardBitmap;
  Label_MemoryRequiredValue.Caption := FileSizeToStr(int64(SpinEdit_Width.Value)*SpinEdit_Height.Value*Sizeof(TBGRAPixel),rsBytes);
end;

{$R *.lfm}

end.

