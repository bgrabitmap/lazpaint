// SPDX-License-Identifier: GPL-3.0-only
unit UResample;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Spin, ComCtrls, BGRAVirtualScreen, LazPaintType, LCScaleDPI,
  uresourcestrings, BGRABitmap, uscripting;

type

  { TFResample }

  TFResample = class(TForm)
    Button_OK: TButton;
    Button_Cancel: TButton;
    CheckBox_Ratio: TCheckBox;
    ComboBox_MUnit: TComboBox;
    ComboBox_Quality: TComboBox;
    Label_Quality: TLabel;
    Label_Width: TLabel;
    Label_Height: TLabel;
    SpinEdit_Width: TSpinEdit;
    SpinEdit_Height: TSpinEdit;
    ToolBar8: TToolBar;
    ToolButton23: TToolButton;
    vsPreview: TBGRAVirtualScreen;
    procedure Button_OKClick(Sender: TObject);
    procedure ComboBox_MUnitChange(Sender: TObject);
    procedure CheckBox_RatioChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpinEdit_HeightChange(Sender: TObject);
    procedure SpinEdit_WidthChange(Sender: TObject);
    procedure ToolButton23Click(Sender: TObject);
    procedure vsPreviewRedraw(Sender: TObject; Bitmap: TBGRABitmap);
  private
    FLazPaintInstance: TLazPaintCustomInstance;
    FIgnoreInput: boolean;
    FLockedAspectRatio: single;
    FParameters: TVariableSet;
    FMUnit: integer;
    procedure SetLazPaintInstance(AValue: TLazPaintCustomInstance);
    procedure ThemeChanged(Sender: TObject);
    procedure UpdatePreview;
    procedure ComputeAspectRatio;
    function NewHeight: integer;
    function NewWidth: integer;
  public
    destructor Destroy; override;
    property LazPaintInstance: TLazPaintCustomInstance read FLazPaintInstance write SetLazPaintInstance;
  end;

function ShowResampleDialog(Instance: TLazPaintCustomInstance; AParameters: TVariableSet):boolean;

implementation

uses ugraph, BGRABitmapTypes, umac, uimage;

{ TFResample }

function ShowResampleDialog(Instance: TLazPaintCustomInstance; AParameters: TVariableSet):boolean;
var
  Resample: TFResample;
  topmostInfo: TTopMostInfo;
begin
  result := false;
  Resample := nil;
  topmostInfo := instance.HideTopmost;
  try
    Resample:= TFResample.create(nil);
    Resample.LazPaintInstance := Instance;
    Resample.FParameters := AParameters;
    result:= (Resample.ShowModal = mrOk);
  except
    on ex:Exception do
      Instance.ShowError('ShowResampleDialog',ex.Message);
  end;
  instance.ShowTopmost(topmostInfo);
  Resample.free;
end;

procedure TFResample.FormCreate(Sender: TObject);
begin
  FIgnoreInput := true;
  ScaleControl(Self,OriginalDPI);
  vsPreview.BitmapAutoScale:= false;

  SpinEdit_Width.MaxValue := MaxImageWidth;
  SpinEdit_Height.MaxValue := MaxImageHeight;
  CheckOKCancelBtns(Button_OK,Button_Cancel);
  CheckSpinEdit(SpinEdit_Width);
  CheckSpinEdit(SpinEdit_Height);
  with ComboBox_Quality.Items do begin
    Add(rsFast);
    Add(rsLinear);
    Add(rsHalfCosine);
    Add(rsCosine);
    Add(rsMitchell);
    Add(rsSpline);
    Add(rsBestQuality);
    Add(StringReplace(rsLanczos,'%1','2',[]));
    Add(StringReplace(rsLanczos,'%1','3',[]));
    Add(StringReplace(rsLanczos,'%1','4',[]));
  end;
  with ComboBox_MUnit do begin
    Clear;
    Items.Add (rsPx) ;
    Items.Add (rsPercent);
  end;
  FIgnoreInput := false;
end;

procedure TFResample.FormShow(Sender: TObject);
var idxQuality: integer;
begin
  ToolBar8.Images := LazPaintInstance.Icons[DoScaleY(16,OriginalDPI)];
  FIgnoreInput := true;
  idxQuality := LazPaintInstance.Config.DefaultResampleQuality;
  if (idxQuality >= 0) and (idxQuality < ComboBox_Quality.Items.Count) then
    ComboBox_Quality.ItemIndex := idxQuality else
      ComboBox_Quality.ItemIndex := 0;
  CheckBox_Ratio.Checked := LazPaintInstance.Config.DefaultResampleKeepAspectRatio;
  FMUnit:=0;
  ComboBox_MUnit.ItemIndex:= FMUnit;
  SpinEdit_Width.Value := LazPaintInstance.Image.Width;
  SpinEdit_Height.Value := LazPaintInstance.Image.Height;
  if LazPaintInstance.Image.Height = 0 then
    FLockedAspectRatio:= 1
  else
    FLockedAspectRatio:= LazPaintInstance.Image.Width/LazPaintInstance.Image.Height;
  UpdatePreview;
  FIgnoreInput := false;
end;

procedure TFResample.SpinEdit_HeightChange(Sender: TObject);
begin
  if FIgnoreInput then exit;
  FIgnoreInput:= true;
    case FMUnit of
    0:  if CheckBox_Ratio.Checked and (LazPaintInstance.Image.Height <> 0) then
          SpinEdit_Width.Value := round(SpinEdit_Height.Value*FLockedAspectRatio);
    1:  if CheckBox_Ratio.Checked and (LazPaintInstance.Image.Height <> 0) and (LazPaintInstance.Image.Width <> 0) then
          SpinEdit_Width.Value := round(NewHeight*FLockedAspectRatio/LazPaintInstance.Image.Width*100);
    end;
  FIgnoreInput:= false;
  UpdatePreview;
end;

procedure TFResample.SpinEdit_WidthChange(Sender: TObject);
begin
  if FIgnoreInput then exit;
  FIgnoreInput:= true;
  case FMUnit of
    0:  if CheckBox_Ratio.Checked and (LazPaintInstance.Image.Width <> 0) then
            SpinEdit_Height.Value := round(SpinEdit_Width.Value/FLockedAspectRatio);
    1:  if CheckBox_Ratio.Checked and (LazPaintInstance.Image.Width <> 0) and (LazPaintInstance.Image.Height <> 0) then
            SpinEdit_Height.Value:= round(NewWidth/FLockedAspectRatio/LazPaintInstance.Image.Height*100);
  end;
  FIgnoreInput:= false;
  UpdatePreview;
end;

procedure TFResample.ToolButton23Click(Sender: TObject);
var tx,ty: integer;
begin
  if FLockedAspectRatio <> 0 then
  begin
    FIgnoreInput := true;
    tx := SpinEdit_Width.Value;
    ty := SpinEdit_Height.Value;
    SpinEdit_Width.Value := ty;
    SpinEdit_Height.Value := tx;
    ComputeAspectRatio;
    UpdatePreview;
    FIgnoreInput := false;
  end;
end;

function TFResample.NewWidth: integer;
begin
  case FMUnit of
    0: Result:=SpinEdit_Width.Value;
    1: Result:=round(SpinEdit_Width.Value*LazPaintInstance.Image.Width/100);
  else
    Result:=SpinEdit_Width.Value;
  end;
  if result <= 1 then result := 1;
end;

destructor TFResample.Destroy;
begin
  if Assigned(FLazPaintInstance) then
    FLazPaintInstance.RegisterThemeListener(@ThemeChanged, false);
  inherited Destroy;
end;

function TFResample.NewHeight: integer;
begin
  case FMUnit of
    0: Result:=SpinEdit_Height.Value;
    1: Result:=round(SpinEdit_Height.Value*LazPaintInstance.Image.Height/100);
  else
    Result:=SpinEdit_Height.Value;
  end;
  if result <= 1 then result := 1;
end;

procedure TFResample.vsPreviewRedraw(Sender: TObject; Bitmap: TBGRABitmap);
var
  tx,ty,px,py,x,y,px2,py2,x2,y2: NativeInt;
  ratio,zoom,scaling: double;
  deltaX: NativeInt;
begin
  scaling := DoScaleX(60, OriginalDPI)/60 * GetCanvasScaleFactor;

  deltaX := Bitmap.Width-Bitmap.Height;
  if deltaX < 0 then deltaX := 0;
  tx := NewWidth;
  ty := NewHeight;
  if LazPaintInstance.Image.Width > tx then tx := LazPaintInstance.Image.Width;
  if LazPaintInstance.Image.Height > ty then ty := LazPaintInstance.Image.Height;
  if (tx > 0) and (ty > 0) then
  begin
    ratio := tx/ty;
    if (Bitmap.Width-deltaX)/ratio < Bitmap.Height then
      zoom := (Bitmap.Width-deltaX)/tx
    else
      zoom := Bitmap.height/ty;

    px := round(NewWidth*zoom);
    py := round(NewHeight*zoom);
    if px < 1 then px := 1;
    if py < 1 then py := 1;
    x := Bitmap.Width-px;
    y := (Bitmap.height-py) div 2;

    px2 := round(LazPaintInstance.Image.Width*zoom);
    py2 := round(LazPaintInstance.Image.Height*zoom);
    x2 := 0;
    y2 := (Bitmap.height-py2) div 2;

    if (px = 1) or (py = 1) then
      Bitmap.FillRect(x,y,x+px,y+py,BGRA(0,0,0,192),dmDrawWithTransparency)
    else
    begin
      Bitmap.Rectangle(x,y,x+px,y+py,BGRA(0,0,0,192),dmDrawWithTransparency);
      DrawCheckers(Bitmap, rect(x+1,y+1,x+px-1,y+py-1), scaling);
    end;
    Bitmap.StretchPutImage(rect(x,y,x+px,y+py),LazPaintInstance.Image.RenderedImage,dmDrawWithTransparency);

    if (px2 = 1) or (py2 = 1) then
      Bitmap.DrawLineAntialias(x2,y2,x2+px2-1,y2+py2-1,BGRA(0,0,0,160),BGRA(255,255,255,160),round(scaling),True)
    else
      Bitmap.DrawPolyLineAntialias([Point(x2,y2),Point(x2+px2-1,y2),Point(x2+px2-1,y2+py2-1),Point(x2,y2+py2-1),Point(x2,y2)],BGRA(0,0,0,160),BGRA(255,255,255,160),round(scaling),False);
    Bitmap.StretchPutImage(rect(x2,y2,x2+px2,y2+py2),LazPaintInstance.Image.RenderedImage,dmDrawWithTransparency,48);
  end;
end;

procedure TFResample.UpdatePreview;
begin
  vsPreview.RedrawBitmap;
end;

procedure TFResample.SetLazPaintInstance(AValue: TLazPaintCustomInstance);
begin
  if FLazPaintInstance=AValue then Exit;
  if Assigned(FLazPaintInstance) then
    FLazPaintInstance.RegisterThemeListener(@ThemeChanged, false);
  FLazPaintInstance:=AValue;
  if Assigned(FLazPaintInstance) then
    FLazPaintInstance.RegisterThemeListener(@ThemeChanged, true);
end;

procedure TFResample.ThemeChanged(Sender: TObject);
begin
  vsPreview.DiscardBitmap;
end;

procedure TFResample.ComputeAspectRatio;
begin
  if (NewWidth >= 1) and (NewHeight >= 1) then
    FLockedAspectRatio:= NewWidth/NewHeight;
end;

procedure TFResample.Button_OKClick(Sender: TObject);
var filter: TResampleFilter;
begin
  if ((FMUnit=0) and (SpinEdit_Width.Value = LazPaintInstance.Image.Width) and
        (SpinEdit_Height.Value = LazPaintInstance.Image.Height))
  or
     ((FMUnit=1) and (SpinEdit_Width.Value = 100) and
        (SpinEdit_Height.Value = 100))
    then
    ModalResult := mrCancel
  else
  begin
    Button_OK.Enabled := false;
    filter := CaptionToResampleFilter(ComboBox_Quality.Text);
    FParameters.Integers['Width']:=NewWidth;
    FParameters.Integers['Height']:=NewHeight;
    if not FParameters.IsDefined('Quality') then LazPaintInstance.Config.SetDefaultResampleQuality(ComboBox_Quality.ItemIndex);
    FParameters.Strings['Quality'] := ResampleFilterStr[filter];
    LazPaintInstance.Config.SetDefaultResampleKeepAspectRatio(CheckBox_Ratio.Checked);
    LazPaintInstance.Image.Resample(NewWidth, NewHeight,filter);

    ModalResult := mrOK;
  end;
end;

procedure TFResample.ComboBox_MUnitChange(Sender: TObject);
begin
  if FMUnit= ComboBox_MUnit.ItemIndex then exit;
  FMUnit:= ComboBox_MUnit.ItemIndex;
  FIgnoreInput:=True;
  case FMUnit of
    0: begin //pixels
         SpinEdit_Width.Value:=  round (LazPaintInstance.Image.Width*SpinEdit_Width.Value/100);
         SpinEdit_Height.Value:= round (LazPaintInstance.Image.Height*SpinEdit_Height.Value/100);
       end;
    1: begin //percent
         SpinEdit_Width.Value:= round (SpinEdit_Width.Value/ LazPaintInstance.Image.Width*100);
         SpinEdit_Height.Value:= round (SpinEdit_Height.Value/ LazPaintInstance.Image.Height*100);
       end;
  end;
  FIgnoreInput:=False;
end;

procedure TFResample.CheckBox_RatioChange(Sender: TObject);
begin
  ComputeAspectRatio;
end;

{$R *.lfm}

end.

