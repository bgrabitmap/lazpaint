// SPDX-License-Identifier: GPL-3.0-only
unit uprint;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, PrintersDlgs, LResources, Forms, Controls,
  Graphics, Dialogs, StdCtrls, Spin, BGRAVirtualScreen, BGRABitmap,
  BGRABitmapTypes, LazPaintType, BGRATransform;

type

  { TFPrint }

  TFPrint = class(TForm)
    BGRAVirtualScreen1: TBGRAVirtualScreen;
    Button_ZoomFit: TButton;
    Button_ConfigurePrinter: TButton;
    Button_Print: TButton;
    CheckBox_Ratio: TCheckBox;
    ComboBox_Orientation: TComboBox;
    GroupBox_ImageSize: TGroupBox;
    GroupBox_Margins: TGroupBox;
    Label_DpiX: TLabel;
    Label_DpiY: TLabel;
    Label_Left: TLabel;
    Label_Width: TLabel;
    Label_Height: TLabel;
    Label_Top: TLabel;
    Label_Right: TLabel;
    Label_Bottom: TLabel;
    Label_Orientation: TLabel;
    Label_SelectedPrinterAndPaper: TLabel;
    Label_PrinterAndPaper: TLabel;
    PrinterSetupDialog1: TPrinterSetupDialog;
    SpinEdit_DpiX: TSpinEdit;
    SpinEdit_DpiY: TSpinEdit;
    SpinEdit_Width: TSpinEdit;
    SpinEdit_Height: TSpinEdit;
    SpinEdit_Top: TSpinEdit;
    SpinEdit_Bottom: TSpinEdit;
    SpinEdit_Left: TSpinEdit;
    SpinEdit_Right: TSpinEdit;
    procedure BGRAVirtualScreen1MouseDown(Sender: TObject;
      Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure BGRAVirtualScreen1MouseMove(Sender: TObject; {%H-}Shift: TShiftState;
      X, Y: Integer);
    procedure BGRAVirtualScreen1MouseUp(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure BGRAVirtualScreen1Resize(Sender: TObject);
    procedure Button_ConfigurePrinterClick(Sender: TObject);
    procedure Button_PrintClick(Sender: TObject);
    procedure Button_ZoomFitClick(Sender: TObject);
    procedure CheckBox_RatioChange(Sender: TObject);
    procedure ComboBox_OrientationChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Label_SelectedPrinterAndPaperClick(Sender: TObject);
    procedure SpinEdit_Change(Sender: TObject);
    procedure SpinEdit_DPIChange(Sender: TObject);
    procedure SpinEdit_SizeChange(Sender: TObject);
  private
    function GetDpiX: single;
    function GetDpiY: single;
    function GetRotatedSpinBottom: TSpinEdit;
    function GetRotatedSpinTop: TSpinEdit;
    function GetRotatedSpinRight: TSpinEdit;
    function GetRotatedSpinLeft: TSpinEdit;
  private
    { private declarations }
    FInitializing: boolean;
    FImagePos, FImageSize: TPointF;
    FPreviewTransform: TAffineMatrix;
    FHoveringImage, FMovingImage: boolean;
    FWantedImagePos: TPointF;
    FPrevMousePos: TPoint;
    FDpiAspectRatio,FAspectRatio: single;
    invZoom: TPointF;
    FLabelCount: TLabel;
    FPrintCount: integer;
    property RotatedSpinTop: TSpinEdit read GetRotatedSpinTop;
    property RotatedSpinLeft: TSpinEdit read GetRotatedSpinLeft;
    property RotatedSpinRight: TSpinEdit read GetRotatedSpinRight;
    property RotatedSpinBottom: TSpinEdit read GetRotatedSpinBottom;
    property DpiX: single read GetDpiX;
    property DpiY: single read GetDpiY;
  public
    Instance: TLazPaintCustomInstance;
    { public declarations }
    procedure UpdatePrinterConfig;
    procedure UpdatePrintMargins;
    procedure UpdatePrintPreview;
    procedure UpdateImageSize(AUpdateX,AUpdateY: boolean);
  end;

implementation

uses printers, UResourceStrings, Types, LCScaleDPI, umac, ULoading;

var
  marginLeft, marginRight, marginTop, marginBottom: integer;

function marginLeftInPoints: single;
begin
  result := marginLeft / 25.4 * 72;
end;

function marginTopInPoints: single;
begin
  result := marginTop / 25.4 * 72;
end;

function marginRightInPoints: single;
begin
  result := marginRight / 25.4 * 72;
end;

function marginBottomInPoints: single;
begin
  result := marginBottom / 25.4 * 72;
end;

function unrotatedMarginLeft: integer;
begin
  if Printer.Orientation in[poPortrait,poReversePortrait] then
    result := marginLeft else result := marginBottom;
end;

function unrotatedMarginRight: integer;
begin
  if Printer.Orientation in[poPortrait,poReversePortrait] then
    result := marginRight else result := marginTop;
end;

function unrotatedMarginBottom: integer;
begin
  if Printer.Orientation in[poPortrait,poReversePortrait] then
    result := marginBottom else result := marginRight;
end;

function unrotatedMarginTop: integer;
begin
  if Printer.Orientation in[poPortrait,poReversePortrait] then
    result := marginTop else result := marginLeft;
end;

function unrotatedMarginLeftInPoints: single;
begin
  result := unrotatedMarginLeft / 25.4 * 72;
end;

function unrotatedMarginTopInPoints: single;
begin
  result := unrotatedMarginTop / 25.4 * 72;
end;

function unrotatedMarginTopLeftInPoints: TPointF;
begin
  result.x := unrotatedMarginLeftInPoints;
  result.y := unrotatedMarginTopInPoints;
end;

function unrotatedMarginRightInPoints: single;
begin
  result := unrotatedMarginRight / 25.4 * 72;
end;

function unrotatedMarginBottomInPoints: single;
begin
  result := unrotatedMarginBottom / 25.4 * 72;
end;

function unrotatedMarginBottomRightInPoints: TPointF;
begin
  result.x := unrotatedMarginRightInPoints;
  result.y := unrotatedMarginBottomInPoints;
end;

function unrotatedTotalMarginInPoints: TPointF;
begin
  result := unrotatedMarginTopLeftInPoints + unrotatedMarginBottomRightInPoints;
end;

function paperWidthInPoints: single;
begin
  result := printer.PaperSize.Width * 72 / printer.XDPI;
end;

function paperHeightInPoints: single;
begin
  result := printer.PaperSize.Height * 72 / printer.YDPI;
end;

function paperSizeInPoints: TPointF;
begin
  result.x := paperWidthInPoints;
  result.y := paperHeightInPoints;
end;

{ TFPrint }

procedure TFPrint.FormShow(Sender: TObject);
var gap: integer;
begin
  FInitializing:= true;
  gap := Label_SelectedPrinterAndPaper.Left-(Label_PrinterAndPaper.Left + Label_PrinterAndPaper.Width);
  Label_PrinterAndPaper.AutoSize := true;
  Label_PrinterAndPaper.Top := Label_SelectedPrinterAndPaper.Top+(Label_SelectedPrinterAndPaper.Height-Label_PrinterAndPaper.Height) div 2;
  Label_SelectedPrinterAndPaper.Left := Label_PrinterAndPaper.Left + Label_PrinterAndPaper.Width+gap;
  Label_SelectedPrinterAndPaper.Width := Button_ConfigurePrinter.Left - Label_SelectedPrinterAndPaper.Left - gap;

  ComboBox_Orientation.Items.Clear;
  ComboBox_Orientation.Items.Add(rsPortait);
  ComboBox_Orientation.Items.Add(rsLandscape);

  GroupBox_Margins.Caption := GroupBox_Margins.Caption+' (mm)';
  if instance.Image.Height = 0 then FAspectRatio := 1 else
    FAspectRatio:= instance.Image.Width / instance.Image.Height;
  FDpiAspectRatio:=1;

  Label_DpiX.Caption := 'DPI';
  Label_DpiY.Caption := 'DPI y';

  FInitializing := false;
  UpdateImageSize(true,true);
  UpdatePrinterConfig;
end;

procedure TFPrint.Label_SelectedPrinterAndPaperClick(Sender: TObject);
begin
  Button_ConfigurePrinterClick(Sender);
end;

procedure TFPrint.SpinEdit_Change(Sender: TObject);
begin
  if not FInitializing then
    begin
      marginLeft:= RotatedSpinLeft.Value;
      marginTop:= RotatedSpinTop.Value;
      marginRight:= RotatedSpinRight.Value;
      marginBottom:= RotatedSpinBottom.Value;
      UpdatePrintPreview;
    end;
end;

procedure TFPrint.SpinEdit_DPIChange(Sender: TObject);
begin
  if FInitializing then exit;
  if CheckBox_Ratio.Checked then
    begin
      FInitializing := true;
      if (Sender = SpinEdit_DpiX) and (FDpiAspectRatio <> 0) then
        SpinEdit_DpiY.Value := round(SpinEdit_DpiX.Value/FDpiAspectRatio)
      else if (Sender = SpinEdit_DpiY) then
        SpinEdit_DpiX.Value := round(SpinEdit_DpiY.Value*FDpiAspectRatio);
      FInitializing := false;
    end;
  UpdateImageSize((Sender = SpinEdit_DpiX) or CheckBox_Ratio.Checked, (Sender = SpinEdit_DpiY) or CheckBox_Ratio.Checked);
  UpdatePrintPreview;
end;

procedure TFPrint.SpinEdit_SizeChange(Sender: TObject);
begin
  if not FInitializing then
  begin
    if Sender = SpinEdit_Width then
        FImageSize.x := SpinEdit_Width.Value/25.4*72;
    if Sender = SpinEdit_Height then
        FImageSize.y := SpinEdit_Height.Value/25.4*72;
    if CheckBox_Ratio.Checked then
      begin
        FInitializing := true;
        if (Sender = SpinEdit_Width) and (FAspectRatio <> 0) then
          begin
            FImageSize.y := FImageSize.x / FAspectRatio;
            SpinEdit_Height.Value := round(FImageSize.y / 72 * 25.4);
          end;
        if Sender = SpinEdit_Height then
          begin
            FImageSize.x := FImageSize.y * FAspectRatio;
            SpinEdit_Width.Value := round(FImageSize.x / 72 * 25.4);
          end;
        FInitializing := false;
      end;
    FInitializing := true;
    if (Sender = SpinEdit_Width) or CheckBox_Ratio.Checked then
      begin
        if FImageSize.x <> 0 then
          SpinEdit_DpiX.Value := round(Instance.Image.Width / FImageSize.x * 72);
      end;
    if (Sender = SpinEdit_Height) or CheckBox_Ratio.Checked then
      begin
        if FImageSize.y <> 0 then
          SpinEdit_DpiY.Value := round(Instance.Image.Height / FImageSize.y * 72);
      end;
    FInitializing := false;
    UpdatePrintPreview;
  end;
end;

function TFPrint.GetDpiX: single;
begin
  result := SpinEdit_DpiX.Value;
end;

function TFPrint.GetDpiY: single;
begin
  result := SpinEdit_DpiY.Value;
end;

function TFPrint.GetRotatedSpinBottom: TSpinEdit;
begin
  if printer.Orientation in[poPortrait,poReversePortrait] then
    result := SpinEdit_Bottom
  else
    result := SpinEdit_Left;
end;

function TFPrint.GetRotatedSpinLeft: TSpinEdit;
begin
  if printer.Orientation in[poPortrait,poReversePortrait] then
    result := SpinEdit_Left
  else
    result := SpinEdit_Top;
end;

function TFPrint.GetRotatedSpinRight: TSpinEdit;
begin
  if printer.Orientation in[poPortrait,poReversePortrait] then
    result := SpinEdit_Right
  else
    result := SpinEdit_Bottom;

end;

function TFPrint.GetRotatedSpinTop: TSpinEdit;
begin
  if printer.Orientation in[poPortrait,poReversePortrait] then
    result := SpinEdit_Top
  else
    result := SpinEdit_Right;
end;

procedure TFPrint.UpdatePrinterConfig;
begin
  FInitializing := true;
  Label_SelectedPrinterAndPaper.Caption := ' ' + printer.PrinterName + ' (' + printer.PaperSize.PaperName + ')';
  if printer.Orientation in[poPortrait,poReversePortrait] then
    ComboBox_Orientation.ItemIndex := 0 else
    ComboBox_Orientation.ItemIndex := 1;

  FInitializing := false;
  UpdatePrintMargins;
end;

procedure TFPrint.UpdatePrintMargins;
begin
  FInitializing := true;
  RotatedSpinTop.Value := marginTop;
  RotatedSpinLeft.Value := marginLeft;
  RotatedSpinRight.Value := marginRight;
  RotatedSpinBottom.Value := marginBottom;
  FInitializing := false;
  UpdatePrintPreview;
end;

procedure TFPrint.UpdatePrintPreview;
begin
  BGRAVirtualScreen1.DiscardBitmap;
end;

procedure TFPrint.UpdateImageSize(AUpdateX, AUpdateY: boolean);
begin
  FInitializing := true;
  if AUpdateX and (DpiX <> 0) then FImageSize.x := Instance.Image.Width / DpiX * 72;
  if AUpdateY and (DpiY <> 0) then FImageSize.y := Instance.Image.Height / DpiY * 72;
  if AUpdateX then SpinEdit_Width.Value := round(FImageSize.X / 72 * 25.4);
  if AUpdateY then SpinEdit_Height.Value := round(FImageSize.Y / 72 * 25.4);
  FInitializing := false;
end;

procedure TFPrint.Button_ConfigurePrinterClick(Sender: TObject);
begin
  if PrinterSetupDialog1.Execute then UpdatePrinterConfig;
end;

procedure TFPrint.Button_PrintClick(Sender: TObject);
var FPrintTransform: TAffineMatrix;
  marTopLeft,marBottomRight,imgTopLeft,imgBottomRight: TPointF;
  area: TRect;
begin
  if (unrotatedTotalMarginInPoints.x >= paperSizeInPoints.x) or
    (unrotatedTotalMarginInPoints.y >= paperSizeInPoints.y) then exit;

  if FLabelCount = nil then
  begin
    FLabelCount := TLabel.Create(self);
    FLabelCount.Left := Button_Print.Left + Button_Print.Width;
    FLabelCount.Top := Button_Print.Top;
    FLabelCount.AutoSize := false;
    FLabelCount.Height := Button_Print.Height;
    FLabelCount.Width := BGRAVirtualScreen1.Left - FLabelCount.Left;
    FLabelCount.Layout := tlCenter;
    FLabelCount.Alignment := taCenter;
    InsertControl(FLabelCount);
  end;

  FLabelCount.Caption:= '...';
  MessagePopupForever(rsActionInProgress);
  Self.Enabled:= false;
  Application.ProcessMessages;
  try
    FPrintTransform := AffineMatrixScale(Printer.XDPI/72, Printer.YDPI/72);
    Printer.BeginDoc;
    if not Instance.Image.RenderedImage.Empty then
    begin
      marTopLeft := FPrintTransform*unrotatedMarginTopLeftInPoints;
      marBottomRight := FPrintTransform*(paperSizeInPoints - unrotatedMarginBottomRightInPoints);
      area := rect(round(marTopLeft.x),round(marTopLeft.y),round(marBottomRight.x),round(marBottomRight.y));
      Printer.Canvas.ClipRect := area;
      Printer.Canvas.Clipping := true;
      imgTopLeft := FPrintTransform*FImagePos;
      imgBottomRight := FPrintTransform*(FImagePos+FImageSize);
      Printer.Canvas.StretchDraw(rect(round(imgTopLeft.x),round(imgTopLeft.y),
        round(imgBottomRight.x),round(imgBottomRight.y)), Instance.Image.RenderedImage.Bitmap);
      Printer.Canvas.Clipping := false;
    end;
    Printer.EndDoc;
    MessagePopup(rsOkay, 4000);
    inc(FPrintCount);
  except on ex:exception do
    begin
      Instance.ShowError(Caption, ex.Message);
      if Printer.Printing then Printer.Abort;
    end;
  end;
  Self.Enabled := true;
  FLabelCount.Caption := IntToStr(FPrintCount);
end;

procedure TFPrint.Button_ZoomFitClick(Sender: TObject);
var maxImageSize: TPointF;
  ratio: single;
begin
  maxImageSize := paperSizeInPoints - unrotatedTotalMarginInPoints;
  if (maxImageSize.x <= 0) or (maxImageSize.y <= 0) or (FImageSize.x <= 0) or (FImageSize.y <= 0) then exit;
  if CheckBox_Ratio.Checked then
  begin
    ratio := maxImageSize.x/FImageSize.x;
    if FImageSize.y*ratio > maxImageSize.y then
      ratio := maxImageSize.y/FImageSize.y;
    FImageSize := FImageSize*ratio;
  end else
    FImageSize := maxImageSize;

  FInitializing := true;
  SpinEdit_Height.Value := round(FImageSize.y / 72 * 25.4);
  SpinEdit_Width.Value := round(FImageSize.x / 72 * 25.4);
  if FImageSize.x <> 0 then
    SpinEdit_DpiX.Value := round(Instance.Image.Width / FImageSize.x * 72);
  if FImageSize.y <> 0 then
    SpinEdit_DpiY.Value := round(Instance.Image.Height / FImageSize.y * 72);
  FInitializing := false;
  UpdatePrintPreview;
end;

procedure TFPrint.CheckBox_RatioChange(Sender: TObject);
begin
  if not FInitializing then
  begin
    if CheckBox_Ratio.Checked then
    begin
      if FImageSize.y = 0 then FAspectRatio := 1 else
         FAspectRatio:= FImageSize.x/FImageSize.y;
      if DpiY = 0 then FDpiAspectRatio := 1 else
         FDpiAspectRatio:= DpiX/DpiY;
    end;

    Label_DpiY.Visible := not (CheckBox_Ratio.Checked and (FDpiAspectRatio = 1));
    SpinEdit_DpiY.Visible := not (CheckBox_Ratio.Checked and (FDpiAspectRatio = 1));

    if SpinEdit_DpiY.visible then
      Label_DpiX.Caption := 'DPI x'
    else
      Label_DpiX.Caption := 'DPI';
  end;
end;

procedure TFPrint.BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap
  );
var
  ratio, scaling: single;
  x,y,w,h: integer;
  marTopLeft,marBottomRight,
  imgTopLeft,imgBottomRight: TPointF;
  area,imgRect,bounds: TRect;
  zoom: TPointF;
begin
  if (printer.PaperSize.Height = 0) or (printer.PaperSize.Width = 0) then exit;
  scaling := DoScaleX(60, OriginalDPI)/60 * BGRAVirtualScreen1.GetCanvasScaleFactor;
  ratio := printer.PaperSize.Width/printer.PaperSize.Height;
  if Bitmap.Height * ratio > Bitmap.Width then
    begin
      w := Bitmap.Width;
      h := round(w / ratio);
    end else
    begin
      h := Bitmap.Height;
      w := round(h * ratio);
    end;
  x := (Bitmap.Width-w) div 2;
  y := (Bitmap.Height-h) div 2;
  Bitmap.Rectangle(x,y,x+w,y+h,BGRABlack,BGRAWhite,dmSet);

  if (unrotatedTotalMarginInPoints.x >= paperSizeInPoints.x) or
    (unrotatedTotalMarginInPoints.y >= paperSizeInPoints.y) then
  begin
    Button_Print.Enabled := false;
    exit;
  end;
  Button_Print.Enabled := true;

  Bitmap.ClipRect := rect(x,y,x+w,y+h);
  zoom := PointF(w/paperWidthInPoints,h/paperHeightInPoints);
  FPreviewTransform := AffineMatrixTranslation(x,y) *
        AffineMatrixScale(zoom.x, zoom.y);
  if zoom.x > 0 then invZoom.x := 1/zoom.x else invZoom.x := 0;
  if zoom.y > 0 then invZoom.y := 1/zoom.y else invZoom.y := 0;

  marTopLeft := FPreviewTransform*unrotatedMarginTopLeftInPoints;
  marBottomRight := FPreviewTransform*(paperSizeInPoints - unrotatedMarginBottomRightInPoints);
  area := rect(round(marTopLeft.x),round(marTopLeft.y),round(marBottomRight.x),round(marBottomRight.y));
  Bitmap.RectangleAntialias(area.left, area.top, area.right, area.bottom, BGRA(128,160,192,128),
    scaling);
  if IntersectRect(area,area,Bitmap.ClipRect) then
  begin
    Bitmap.ClipRect := area;
    if FImagePos.x + FImageSize.x > paperSizeInPoints.x - unrotatedMarginBottomRightInPoints.x then
      FImagePos.x := paperSizeInPoints.x - unrotatedMarginBottomRightInPoints.x - FImageSize.x;
    if FImagePos.y + FImageSize.y > paperSizeInPoints.y - unrotatedMarginBottomRightInPoints.y then
      FImagePos.y := paperSizeInPoints.y - unrotatedMarginBottomRightInPoints.y - FImageSize.y;
    if FImagePos.x < unrotatedMarginTopLeftInPoints.x then FImagePos.x := unrotatedMarginTopLeftInPoints.x;
    if FImagePos.y < unrotatedMarginTopLeftInPoints.y then FImagePos.y := unrotatedMarginTopLeftInPoints.y;
    imgTopLeft := FPreviewTransform*FImagePos;
    imgBottomRight := FPreviewTransform*(FImagePos+FImageSize);
    imgRect := rect(round(imgTopLeft.x),round(imgTopLeft.y),round(imgBottomRight.x),round(imgBottomRight.y));
    Bitmap.StretchPutImage(imgRect,Instance.Image.RenderedImage,dmDrawWithTransparency);
    Bitmap.NoClip;
    bounds := Bitmap.ClipRect;
    InflateRect(bounds,1,1);
    IntersectRect(imgRect, imgRect,bounds);
    Bitmap.DrawPolyLineAntialias([imgRect.TopLeft,Point(imgRect.Right-1,imgRect.Top),Point(imgRect.Right-1, imgRect.Bottom-1),
                                  Point(imgRect.left, imgRect.Bottom-1),imgRect.TopLeft], BGRA(0,0,0,128),
                                  BGRA(255,255,255,128), round(2*scaling), False);
  end;
  Bitmap.NoClip;
end;

procedure TFPrint.BGRAVirtualScreen1Resize(Sender: TObject);
begin
  BGRAVirtualScreen1.DiscardBitmap;
end;

procedure TFPrint.BGRAVirtualScreen1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  imgTopLeft,imgBottomRight: TPointF;
begin
  if FMovingImage then
  begin
    FWantedImagePos += PointF((x-FPrevMousePos.x)*invZoom.x,(y-FPrevMousePos.y)*invZoom.y);
    FImagePos := FWantedImagePos;
    BGRAVirtualScreen1.DiscardBitmap;
  end else
  begin
    imgTopLeft := FPreviewTransform*FImagePos;
    imgBottomRight := FPreviewTransform*(FImagePos+FImageSize);
    if (X >= imgTopLeft.X) and (X <= imgBottomRight.X) and
       (Y >= imgTopLeft.Y) and (Y <= imgBottomRight.Y) then
    begin
      FHoveringImage:= true;
      BGRAVirtualScreen1.Cursor := crSizeAll;
    end else
    begin
      FHoveringImage:= false;
      BGRAVirtualScreen1.Cursor := crDefault;
    end;
  end;
  FPrevMousePos := Point(x,y);
end;

procedure TFPrint.BGRAVirtualScreen1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button= mbLeft) and FMovingImage then
  begin
    FMovingImage:= false;
  end;
end;

procedure TFPrint.BGRAVirtualScreen1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and FHoveringImage then
  begin
    FWantedImagePos := FImagePos;
    FMovingImage := true;
  end;
end;

procedure TFPrint.ComboBox_OrientationChange(Sender: TObject);
begin
  if not FInitializing then
    begin
      if ComboBox_Orientation.ItemIndex = 0 then
        Printer.Orientation := poPortrait
      else
        printer.Orientation:= poLandscape;
      UpdatePrintMargins;
    end;
end;

procedure TFPrint.FormCreate(Sender: TObject);
begin
  ScaleControl(Self,OriginalDPI);
  BGRAVirtualScreen1.BitmapAutoScale:= false;
  BGRAVirtualScreen1.Color := clDkGray;

  CheckSpinEdit(SpinEdit_DpiY);
  CheckSpinEdit(SpinEdit_DpiX);
  CheckSpinEdit(SpinEdit_Left);
  CheckSpinEdit(SpinEdit_Right);
  CheckSpinEdit(SpinEdit_Top);
  CheckSpinEdit(SpinEdit_Bottom);
  CheckSpinEdit(SpinEdit_Width);
  CheckSpinEdit(SpinEdit_Height);

end;

{$R *.lfm}

initialization

  marginLeft := 10;
  marginTop := 10;
  marginRight := 10;
  marginBottom := 10;

end.

