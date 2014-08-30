unit UCanvassize;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Spin, BGRAVirtualScreen, LazPaintType, uscaledpi, uresourcestrings,
  BGRABitmap, BGRALayers, uimage;

type
  { TFCanvasSize }

  TFCanvasSize = class(TForm)
    Button_Cancel: TButton;
    Button_OK: TButton;
    CheckBox_FlipMode: TCheckBox;
    ComboBox_Anchor: TComboBox;
    ComboBox_MUnit: TComboBox;
    Label_Anchor: TLabel;
    Label_Width: TLabel;
    Label_Height: TLabel;
    SpinEdit_Height: TSpinEdit;
    SpinEdit_Width: TSpinEdit;
    vsPreview: TBGRAVirtualScreen;
    procedure Button_OKClick(Sender: TObject);
    procedure CheckBox_FlipModeChange(Sender: TObject);
    procedure ComboBox_AnchorChange(Sender: TObject);
    procedure ComboBox_MUnitChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpinEdit_HeightChange(Sender: TObject);
    procedure SpinEdit_WidthChange(Sender: TObject);
    procedure vsPreviewRedraw(Sender: TObject; Bitmap: TBGRABitmap);
  private
    { private declarations }
    FIgnoreInput: boolean;
    FMUnit: integer;
    function GetSelectedAnchor: string;
  public
    { public declarations }
    LazPaintInstance: TLazPaintCustomInstance;
    canvasSizeResult: TLayeredBitmapAndSelection;
    repeatImage: boolean;
    property SelectedAnchor: string read GetSelectedAnchor;
  end;

implementation

uses ugraph, bgrabitmaptypes, umac;

function ChangeLayeredImageCanvasSize(layeredBmp: TLazPaintImage; newWidth,
  newHeight: integer; anchor: string; background: TBGRAPixel;
  repeatImage: boolean; flipMode: boolean): TBGRALayeredBitmap;
var i,idx: integer;
begin
  result := TBGRALayeredBitmap.Create;
  for i := 0 to layeredbmp.NbLayers-1 do
  begin
    idx := result.AddOwnedLayer(ChangeCanvasSize(layeredbmp.LayerBitmap[i],newwidth,newHeight,anchor,background,repeatImage,flipMode),
      layeredBmp.BlendOperation[i],layeredbmp.LayerOpacity[i]);
    result.LayerName[idx] := layeredbmp.LayerName[i];
    result.LayerVisible[idx] := layeredbmp.LayerVisible[i];
  end;
end;

{ TFCanvasSize }

procedure TFCanvasSize.FormCreate(Sender: TObject);
begin
  FIgnoreInput:= true;
  ScaleDPI(Self,OriginalDPI);

  SpinEdit_Width.MaxValue := MaxImageWidth;
  SpinEdit_Height.MaxValue := MaxImageHeight;
  CheckOKCancelBtns(Button_OK,Button_Cancel);
  CheckSpinEdit(SpinEdit_Width);
  CheckSpinEdit(SpinEdit_Height);
  ComboBox_Anchor.ItemIndex := 4;
  repeatImage := false;
  with ComboBox_MUnit do begin
    Clear;
    Items.Add (rsPx) ;
    Items.Add (rsPercent);
  end;
  ComboBox_MUnit.ItemIndex:=0;
  FIgnoreInput:= false;
end;

procedure TFCanvasSize.Button_OKClick(Sender: TObject);
var
  tx, ty: integer;
begin
    case FMUnit of
    0: begin //pixels
         tx:=SpinEdit_Width.Value;
         ty:= SpinEdit_Height.Value;
       end;
    1: begin //percent
         tx:= round ((SpinEdit_Width.Value/100)* LazPaintInstance.Image.Width);
         ty:= round ((SpinEdit_Height.Value/100)* LazPaintInstance.Image.Height);
         if (SpinEdit_Width.Value>0) and (tx=0) then tx:=1;
         if (SpinEdit_Height.Value>0) and (ty=0) then ty:=1;
       end;
  end;
  if (tx = LazPaintInstance.Image.Width) and
    (ty = LazPaintInstance.Image.Height) then
    ModalResult := mrCancel else
    begin

      canvasSizeResult.layeredBitmap := ChangeLayeredImageCanvasSize(LazPaintInstance.Image,tx,
         ty,SelectedAnchor,BGRAPixelTransparent, repeatImage, CheckBox_FlipMode.Checked);
      if LazPaintInstance.Image.SelectionReadonly <> nil then
        canvasSizeResult.selection := ChangeCanvasSize(LazPaintInstance.Image.SelectionReadonly,tx,
          ty,SelectedAnchor,BGRABlack, repeatImage, CheckBox_FlipMode.Checked);
      if LazPaintInstance.Image.SelectionLayerReadonly <> nil then
        canvasSizeResult.selectionLayer := ChangeCanvasSize(LazPaintInstance.Image.SelectionLayerReadonly,
           tx,ty,SelectedAnchor,BGRAPixelTransparent, repeatImage, CheckBox_FlipMode.Checked);

      ModalResult := mrOK;
    end;
end;

procedure TFCanvasSize.CheckBox_FlipModeChange(Sender: TObject);
begin
  if FIgnoreInput then exit;
  vsPreview.RedrawBitmap;
end;

procedure TFCanvasSize.ComboBox_AnchorChange(Sender: TObject);
begin
  if FIgnoreInput then exit;
  vsPreview.RedrawBitmap;
end;

procedure TFCanvasSize.ComboBox_MUnitChange(Sender: TObject);
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

procedure TFCanvasSize.FormShow(Sender: TObject);
begin
  FIgnoreInput:= true;
  canvasSizeResult.layeredBitmap := nil;
  canvasSizeResult.selection := nil;
  canvasSizeResult.selectionLayer := nil;
  case FMUnit of
    0: begin //pixels
         SpinEdit_Width.Value := LazPaintInstance.Image.Width;
         SpinEdit_Height.Value := LazPaintInstance.Image.Height;
       end;
    1: begin //percent
         SpinEdit_Width.Value := 100;
         SpinEdit_Height.Value := 100;
       end;
  end;  //case
  if repeatImage then
  begin
    Caption := rsRepeatImage;
    CheckBox_FlipMode.Visible := true;
    ClientHeight := CheckBox_FlipMode.Top+CheckBox_FlipMode.Height+6+(ClientHeight-Button_OK.Top);
  end else
  begin
    Caption := rsCanvasSize;
    CheckBox_FlipMode.Visible := false;
    ClientHeight := CheckBox_FlipMode.Top+(ClientHeight-Button_OK.Top);
  end;

  vsPreview.RedrawBitmap;
  FIgnoreInput:= false;
end;

procedure TFCanvasSize.SpinEdit_HeightChange(Sender: TObject);
begin
  if FIgnoreInput then exit;
  vsPreview.RedrawBitmap;
end;

procedure TFCanvasSize.SpinEdit_WidthChange(Sender: TObject);
begin
  if FIgnoreInput then exit;
  vsPreview.RedrawBitmap;
end;

procedure TFCanvasSize.vsPreviewRedraw(Sender: TObject; Bitmap: TBGRABitmap);
var
  tx,ty,px,py,x,y,px2,py2,x2,y2: NativeInt;
  ratio,zoom: double;
  anchor: string;
begin
  case FMUnit of
    0: begin //pixels
         tx:=SpinEdit_Width.Value;
         ty:= SpinEdit_Height.Value;
       end;
    1: begin //percent
         tx:= round ((SpinEdit_Width.Value/100)* LazPaintInstance.Image.Width);
         ty:= round ((SpinEdit_Height.Value/100)* LazPaintInstance.Image.Height);
         if (SpinEdit_Width.Value>0) and (tx=0) then tx:=1;
         if (SpinEdit_Height.Value>0) and (ty=0) then ty:=1;
       end;
  end;

  px := tx;
  py := ty;

  if LazPaintInstance.Image.Width > tx then tx := LazPaintInstance.Image.Width;
  if LazPaintInstance.Image.Height > ty then ty := LazPaintInstance.Image.Height;

  if (tx > 0) and (ty > 0) then
  begin
    ratio := tx/ty;
    if vsPreview.Width/ratio < vsPreview.Height then
      zoom := vsPreview.Width/tx
    else
      zoom := vsPreview.height/ty;

    px := round(px*zoom);
    py := round(py*zoom);
    x := (vsPreview.Width-px) div 2;
    y := (vsPreview.height-py) div 2;

    px2 := round(LazPaintInstance.Image.Width*zoom);
    py2 := round(LazPaintInstance.Image.Height*zoom);

    x2 := (vsPreview.Width-px2) div 2;
    y2 := (vsPreview.height-py2) div 2;
    anchor := LowerCase(SelectedAnchor);
    if (anchor='topleft') or (anchor='top') or (anchor='topright') then y2 := y;
    if (anchor='bottomleft') or (anchor='bottom') or (anchor='bottomright') then y2 := y+py-py2;
    if (anchor='topleft') or (anchor='left') or (anchor='bottomleft') then x2 := x;
    if (anchor='topright') or (anchor='right') or (anchor='bottomright') then x2 := x+px-px2;

    Bitmap.StretchPutImage(rect(x2,y2,x2+px2,y2+py2),LazPaintInstance.Image.RenderedImage,dmDrawWithTransparency,128);
    Bitmap.ClipRect := rect(x,y,x+px,y+py);
    DrawCheckers(Bitmap,rect(x,y,x+px,y+py));
    Bitmap.StretchPutImage(rect(x2,y2,x2+px2,y2+py2),LazPaintInstance.Image.RenderedImage,dmDrawWithTransparency);
    Bitmap.NoClip;

    if (px2 = 1) or (py2 = 1) then
      Bitmap.DrawLineAntialias(x2,y2,x2+px2-1,y2+py2-1,BGRA(0,0,0,160),BGRA(255,255,255,160),1,True)
    else
      Bitmap.DrawPolyLineAntialias([Point(x2,y2),Point(x2+px2-1,y2),Point(x2+px2-1,y2+py2-1),Point(x2,y2+py2-1),Point(x2,y2)],BGRA(0,0,0,160),BGRA(255,255,255,160),1,False);

    if (px = 1) or (py = 1) then
      Bitmap.DrawLineAntialias(x,y,x+px-1,y+py-1,BGRA(0,0,0,160),BGRA(255,255,255,160),1,True)
    else
      Bitmap.DrawPolyLineAntialias([Point(x,y),Point(x+px-1,y),Point(x+px-1,y+py-1),Point(x,y+py-1),Point(x,y)],BGRA(0,0,0,160),BGRA(255,255,255,160),1,False);

  end;
end;

function TFCanvasSize.GetSelectedAnchor: string;
begin
  if ComboBox_Anchor.ItemIndex = -1 then result := 'Middle' else
    result := ComboBox_Anchor.Items[ComboBox_Anchor.ItemIndex];
end;

{$R *.lfm}

end.

