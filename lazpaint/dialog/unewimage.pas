// SPDX-License-Identifier: GPL-3.0-only
unit UNewimage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Spin, Buttons, ComCtrls, ExtCtrls, BGRAVirtualScreen, BGRAShape,
  uimage, LazPaintType, LCScaleDPI, BGRABitmap, BGRABitmapTypes;

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
    ComboBox_BitDepth: TComboBox;
    Image1: TImage;
    Label_BitDepth: TLabel;
    Label_MemoryRequiredValue: TLabel;
    Label_Height1: TLabel;
    Label_MemoryRequired: TLabel;
    ToolBar_Ratio: TToolBar;
    ToolBar_Rotate: TToolBar;
    ToolButton_ClearRatio: TToolButton;
    ToolButton_Rotate: TToolButton;
    vsPreview: TBGRAVirtualScreen;
    Button_OK: TButton;
    Button_Cancel: TButton;
    Label_Width: TLabel;
    Label_Height: TLabel;
    SpinEdit_Height: TSpinEdit;
    SpinEdit_Width: TSpinEdit;
    procedure BGRAShapeClick(Sender: TObject);
    procedure ComboBox_BitDepthChange(Sender: TObject);
    procedure ComboBox_RatioChange(Sender: TObject);
    procedure ComboBox_RatioEnter(Sender: TObject);
    procedure ComboBox_RatioExit(Sender: TObject);
    procedure SpinEdit_HeightChange(Sender: TObject);
    procedure ToolButton_ClearRatioClick(Sender: TObject);
    procedure ToolButton_RotateClick(Sender: TObject);
    procedure vsPreviewRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure Button_OKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpinEdit_WidthChange(Sender: TObject);
  private
    FLazPaintInstance: TLazPaintCustomInstance;
    FLastEnteredValue: TLastEnteredValue;
    FRatio: double;
    FRatioWasChanged: boolean;
    FRecomputing: boolean;
    FBackColor: TBGRAPixel;
    procedure SetLazPaintInstance(AValue: TLazPaintCustomInstance);
    procedure ThemeChanged(Sender: TObject);
    procedure UpdatePreview;
    function GetBitDepth: integer;
  public
    ForIcon: boolean;
    newImageResult: TBGRABitmap;
    destructor Destroy; override;
    property LazPaintInstance: TLazPaintCustomInstance read FLazPaintInstance write SetLazPaintInstance;
  end; 

function ShowNewImageDlg(AInstance: TLazPaintCustomInstance; AForIcon: boolean; out tx,ty,bpp: integer; out back: TBGRAPixel):boolean;

implementation

uses umac, UMySLV, UResourceStrings, UGraph;

{ TFNewImage }

function ShowNewImageDlg(AInstance: TLazPaintCustomInstance; AForIcon: boolean; out tx,ty,bpp: integer; out back: TBGRAPixel):boolean;
var
  NewImage: TFNewImage;
begin
  tx := 0;
  ty := 0;
  result := false;
  NewImage := nil;
  try
    Application.ProcessMessages; //avoid unexpected exit on linux
    NewImage:= TFNewImage.Create(nil);
    NewImage.LazPaintInstance := AInstance;
    NewImage.ForIcon := AForIcon;
    result:= (NewImage.ShowModal = mrOk);
    tx:= NewImage.SpinEdit_Width.Value;
    ty:= NewImage.SpinEdit_Height.Value;
    back:= NewImage.FBackColor;
    bpp := NewImage.GetBitDepth;
  except
    on ex:Exception do
    begin
      AInstance.ShowError('ShowNewImageDlg',ex.Message);
      result := false;
    end;
  end;
  NewImage.free;
end;

procedure TFNewImage.Button_OKClick(Sender: TObject);
begin
  if ForIcon then
  begin
    LazPaintInstance.Config.SetDefaultIconImageWidth(SpinEdit_Width.Value);
    LazPaintInstance.Config.SetDefaultIconImageHeight(SpinEdit_Height.Value);
    LazPaintInstance.Config.SetDefaultIconImageBackgroundColor(FBackColor);
  end else
  begin
    LazPaintInstance.Config.SetDefaultImageWidth(SpinEdit_Width.Value);
    LazPaintInstance.Config.SetDefaultImageHeight(SpinEdit_Height.Value);
    LazPaintInstance.Config.SetDefaultImageBackgroundColor(FBackColor);
  end;
  ModalResult:= mrOk;
end;

procedure TFNewImage.vsPreviewRedraw(Sender: TObject;
  Bitmap: TBGRABitmap);
var
  tx,ty,px,py,x,y: NativeInt;
  ratio: double;
  sx,sy: NativeInt;
  blur: TBGRACustomBitmap;
  scaling: single;
  scaledShadowOfsX, scaledShadowOfsY, scaledShadowBlur: integer;
begin
  scaling := DoScaleX(60, OriginalDPI)/60 * GetCanvasScaleFactor;
  scaledShadowOfsX := round(shadowOffsetX*scaling);
  scaledShadowOfsY := round(shadowOffsetY*scaling);
  scaledShadowBlur := round(shadowBlur*scaling);
  sx := Bitmap.Width- scaledShadowOfsX - scaledShadowBlur;
  sy := Bitmap.Height- scaledShadowOfsY - scaledShadowBlur;
  tx := SpinEdit_Width.Value;
  ty := SpinEdit_Height.Value;
  if (tx > 0) and (ty > 0) then
  begin
    ratio := tx/ty;
    if sx/ratio < Bitmap.Height then
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
    Bitmap.FillRect(x+scaledShadowOfsX, y+scaledShadowOfsY,
      x+scaledShadowOfsX+px, y+scaledShadowOfsY+py,
      BGRA(0,0,0,192), dmDrawWithTransparency);
    blur := bitmap.FilterBlurRadial(scaledShadowBlur,rbFast);
    Bitmap.PutImage(0,0, blur,dmSet);
    blur.free;
    if (px = 1) or (py = 1) then
      Bitmap.FillRect(x,y,x+px,y+py,BGRABlack,dmSet)
    else
    begin
      ugraph.DrawCheckers(Bitmap,rect(x,y,x+px,y+py),scaling);
      Bitmap.Rectangle(x,y,x+px,y+py,BGRABlack,FBackColor,dmDrawWithTransparency);
    end;
  end;
end;

procedure TFNewImage.ToolButton_RotateClick(Sender: TObject);
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
begin
  if FRecomputing then exit;
  FRatio := ComputeRatio(ComboBox_Ratio.Text);
  if FRatio = 0 then exit;

  FRatioWasChanged := true;
  FRecomputing:= true;
  if FLastEnteredValue = valHeight then
    SpinEdit_Width.Value := round(SpinEdit_Height.Value*FRatio)
  else
    SpinEdit_Height.Value := round(SpinEdit_Width.Value/FRatio);
  FRecomputing:= false;

  UpdatePreview;
end;

procedure TFNewImage.ComboBox_RatioEnter(Sender: TObject);
begin
  FRatioWasChanged := false;
end;

procedure TFNewImage.ComboBox_RatioExit(Sender: TObject);
begin
  if FRatioWasChanged then
    FLastEnteredValue := valRatio;
end;

procedure TFNewImage.BGRAShapeClick(Sender: TObject);
begin
  with (Sender as TBGRAShape) do
    FBackColor:= ColorToBGRA(FillColor,FillOpacity);
  UpdatePreview;
end;

procedure TFNewImage.ComboBox_BitDepthChange(Sender: TObject);
begin
  if FRecomputing then exit;
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
    FLastEnteredValue:= valHeight;
    FRatio := 0;
    ComboBox_Ratio.Text := '';
  end;
  FRecomputing:= false;
  UpdatePreview;
end;

procedure TFNewImage.ToolButton_ClearRatioClick(Sender: TObject);
begin
  ComboBox_Ratio.ItemIndex:= 0;
  ComboBox_RatioChange(ComboBox_Ratio);
end;

procedure TFNewImage.FormCreate(Sender: TObject);
begin
  ScaleControl(Self,OriginalDPI);
  vsPreview.BitmapAutoScale:= false;

  FRecomputing := true;
  SpinEdit_Width.MaxValue := MaxImageWidth;
  SpinEdit_Height.MaxValue := MaxImageHeight;
  FLastEnteredValue:= valWidth;
  FRecomputing := false;
  CheckOKCancelBtns(Button_OK,Button_Cancel);
  CheckSpinEdit(SpinEdit_Width);
  CheckSpinEdit(SpinEdit_Height);
  newImageResult := nil;
end;

procedure TFNewImage.FormShow(Sender: TObject);
begin
  ToolBar_Rotate.Images := LazPaintInstance.Icons[DoScaleY(16,OriginalDPI)];
  ToolBar_Ratio.Images := ToolBar_Rotate.Images;
  Label_MemoryRequiredValue.Left := Label_MemoryRequired.BoundsRect.Right + DoScaleX(4,OriginalDPI);

  FRecomputing := true;
  if ForIcon then
  begin
    SpinEdit_Width.Value := LazPaintInstance.Config.DefaultIconImageWidth;
    SpinEdit_Height.Value := LazPaintInstance.Config.DefaultIconImageHeight;
    SpinEdit_Width.Increment := 16;
    SpinEdit_Height.Increment := 16;
    FBackColor:= LazPaintInstance.Config.DefaultIconImageBackgroundColor;
    ToolBar_Rotate.Visible := false;
    Label_BitDepth.Visible := true;
    ComboBox_BitDepth.Visible := true;
    ComboBox_BitDepth.Text := IntToStr(LazPaintInstance.Config.DefaultIconImageBitDepth);
  end else
  begin
    SpinEdit_Width.Value := LazPaintInstance.Config.DefaultImageWidth;
    SpinEdit_Height.Value := LazPaintInstance.Config.DefaultImageHeight;
    SpinEdit_Width.Increment := 10;
    SpinEdit_Height.Increment := 10;
    FBackColor:= LazPaintInstance.Config.DefaultImageBackgroundColor;
    ToolBar_Rotate.Visible := true;
    Label_BitDepth.Visible := false;
    ComboBox_BitDepth.Visible := false;
    ComboBox_BitDepth.Text := '32';
  end;
  if SpinEdit_Width.Value = SpinEdit_Height.Value then
  begin
    ComboBox_Ratio.Text := '1:1';
    FRatio := ComputeRatio(ComboBox_Ratio.Text);
    FLastEnteredValue := valRatio;
  end;
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
end;

procedure TFNewImage.UpdatePreview;
begin
  vsPreview.DiscardBitmap;
  Label_MemoryRequiredValue.Caption := FileSizeToStr(int64((SpinEdit_Width.Value*GetBitDepth+7) div 8)*SpinEdit_Height.Value,rsBytes);
  if FBackColor.alpha = 0 then
    ToolBar_Rotate.Color := MergeBGRA(ColorToBGRA(clSilver), ColorToBGRA(clWhite))
    else ToolBar_Rotate.Color := ColorToBGRA(FBackColor);
end;

procedure TFNewImage.SetLazPaintInstance(AValue: TLazPaintCustomInstance);
begin
  if FLazPaintInstance=AValue then Exit;
  if Assigned(FLazPaintInstance) then
    FLazPaintInstance.RegisterThemeListener(@ThemeChanged, false);
  FLazPaintInstance:=AValue;
  if Assigned(FLazPaintInstance) then
    FLazPaintInstance.RegisterThemeListener(@ThemeChanged, true);
end;

procedure TFNewImage.ThemeChanged(Sender: TObject);
begin
  vsPreview.DiscardBitmap;
end;

function TFNewImage.GetBitDepth: integer;
begin
  result := StrToInt(ComboBox_BitDepth.Text);
end;

destructor TFNewImage.Destroy;
begin
  if Assigned(FLazPaintInstance) then
    FLazPaintInstance.RegisterThemeListener(@ThemeChanged, false);
  inherited Destroy;
end;

{$R *.lfm}

end.

