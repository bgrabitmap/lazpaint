unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, ExtDlgs, Menus, BGRAVirtualScreen,
  BCTrackbarUpdown, BCPanel, BGRAImageList, BCButton, BGRALazPaint, BGRABitmap,
  BGRABitmapTypes, BGRATransform, BGRALayerOriginal, BGRAGraphics,
  uvectororiginal, uvectorialfill;

const
  EditorPointSize = 8;
  PenStyleToStr : array[TPenStyle] of string = ('─────', '─ ─ ─ ─', '···············', '─ · ─ · ─', '─ ·· ─ ·· ╴', 'InsideFrame', 'Pattern', 'Clear');

type
  TPaintTool = (ptHand, ptRectangle, ptEllipse, ptPolyline, ptCurve, ptPolygon, ptClosedCurve);

const
  PaintToolClass : array[TPaintTool] of TVectorShapeAny =
    (nil, TRectShape, TEllipseShape, TPolylineShape, TCurveShape, TPolylineShape, TCurveShape);

function IsCreateShapeTool(ATool: TPaintTool): boolean;

const
  SplineStyleToStr : array[TSplineStyle] of string =
    ('Inside','Inside + ends','Crossing','Crossing + ends','Outside','Round outside','Vertex to side','Easy Bézier');

type
  { TForm1 }

  TForm1 = class(TForm)
    BackImage: TImage;
    ButtonLoadTex: TBCButton;
    ButtonNoTex: TBCButton;
    ButtonSwapGradColor: TBCButton;
    ButtonPenStyle: TBCButton;
    Label2: TLabel;
    PanelBackFill: TBCPanel;
    RadioButtonGradient: TRadioButton;
    RadioButtonNone: TRadioButton;
    RadioButtonSolid: TRadioButton;
    RadioButtonTex: TRadioButton;
    ShapeBackColor: TShape;
    ShapeBackEndColor: TShape;
    ShapeBackStartColor: TShape;
    UpDownBackAlpha: TBCTrackbarUpdown;
    UpDownBackEndAlpha: TBCTrackbarUpdown;
    UpDownBackStartAlpha: TBCTrackbarUpdown;
    UpDownPenWidth: TBCTrackbarUpdown;
    PanelBasicStyle: TBCPanel;
    PanelFile: TBCPanel;
    PanelExtendedStyle: TBCPanel;
    BCPanelToolChoice: TBCPanel;
    BCPanelToolbar: TBCPanel;
    BGRAImageList1: TBGRAImageList;
    BGRAVirtualScreen1: TBGRAVirtualScreen;
    ButtonOpenFile: TBCButton;
    ButtonSaveFile: TBCButton;
    ButtonSaveAs: TBCButton;
    ButtonNewFile: TBCButton;
    ColorDialog1: TColorDialog;
    Label1: TLabel;
    Label3: TLabel;
    OpenDialog1: TOpenDialog;
    OpenPictureDialog1: TOpenPictureDialog;
    SaveDialog1: TSaveDialog;
    ShapePenColor: TShape;
    ToolBar1: TToolBar;
    ToolButtonPolyline: TToolButton;
    ToolButtonCurve: TToolButton;
    ToolButtonMove: TToolButton;
    ToolButtonClosedCurve: TToolButton;
    ToolButtonPolygon: TToolButton;
    ToolButtonRectangle: TToolButton;
    ToolButtonEllipse: TToolButton;
    UpDownPenAlpha: TBCTrackbarUpdown;
    procedure ButtonSwapGradColorClick(Sender: TObject);
    procedure ShapeBackGradColorMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure UpDownBackGradAlphaChange(Sender: TObject; AByUser: boolean);
    procedure UpDownPenWidthChange(Sender: TObject; AByUser: boolean);
    procedure BGRAVirtualScreen1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BGRAVirtualScreen1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure BGRAVirtualScreen1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure ButtonLoadTexClick(Sender: TObject);
    procedure ButtonNewFileClick(Sender: TObject);
    procedure ButtonNoTexClick(Sender: TObject);
    procedure ButtonOpenFileClick(Sender: TObject);
    procedure ButtonSaveAsClick(Sender: TObject);
    procedure ButtonSaveFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure RadioButtonBackChange(Sender: TObject);
    procedure ShapeBackColorMouseUp(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure ShapePenColorMouseUp(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure ToolButtonClick(Sender: TObject);
    procedure UpDownBackAlphaChange(Sender: TObject; AByUser: boolean);
    procedure UpDownPenAlphaChange(Sender: TObject; AByUser: boolean);
  private
    FPenColor, FBackColor: TBGRAPixel;
    FBackGradStartColor, FBackGradEndColor: TBGRAPixel;
    FBackTexture: TBGRABitmap;
    FPenWidth: single;
    FPenStyle: TBGRAPenStyle;
    FFlattened: TBGRABitmap;
    FLastEditorBounds: TRect;
    FUpdatingFromShape: boolean;
    FUpdatingSpinEditPenWidth: boolean;
    FCurrentTool: TPaintTool;
    FSplineStyle: TSplineStyle;
    FComboboxSplineStyle: TComboBox;
    FUpdatingComboboxSplineStyle : boolean;
    FPenStyleMenu: TPopupMenu;
    procedure ComboBoxSplineStyleChange(Sender: TObject);
    function GetBackTexture: TBGRABitmap;
    function GetPenColor: TBGRAPixel;
    function GetPenStyle: TBGRAPenStyle;
    function GetPenWidth: single;
    function GetSplineStyle: TSplineStyle;
    function GetVectorTransform: TAffineMatrix;
    procedure ImageChange(ARectF: TRectF);
    procedure OnEditingChange({%H-}ASender: TObject; AOriginal: TBGRALayerCustomOriginal);
    procedure OnOriginalChange({%H-}ASender: TObject; AOriginal: TBGRALayerCustomOriginal);
    procedure OnSelectShape(ASender: TObject; AShape: TVectorShape; APreviousShape: TVectorShape);
    procedure OnClickPenStyle(ASender: TObject);
    procedure SetBackColor(AValue: TBGRAPixel);
    procedure SetBackGradEndColor(AValue: TBGRAPixel);
    procedure SetBackGradStartColor(AValue: TBGRAPixel);
    procedure SetBackTexture(AValue: TBGRABitmap);
    procedure SetCurrentTool(AValue: TPaintTool);
    procedure SetPenColor(AValue: TBGRAPixel);
    procedure SetPenStyle(AValue: TBGRAPenStyle);
    procedure SetPenWidth(AValue: single);
    procedure SetSplineStyle(AValue: TSplineStyle);
    procedure UpdateViewCursor(ACursor: TOriginalEditorCursor);
    procedure RenderAndUpdate(ADraft: boolean);
    procedure UpdateFlattenedImage(ARect: TRect; AUpdateView: boolean = true);
    procedure UpdateView(AImageChangeRect: TRect);
    procedure UpdateToolbarFromShape(AShape: TVectorShape);
    procedure UpdateTitleBar;
    procedure ImageChangesCompletely;
    function CreateShape(const APoint1, APoint2: TPointF): TVectorShape;
    function CreateBackFill: TVectorialFill;
    procedure RemoveExtendedStyleControls;
    procedure UpdateBackComponentsVisibility;
    procedure UpdateShapeBackFill;
    { private declarations }
  public
    { public declarations }
    img: TBGRALazPaintImage;
    filename: string;
    vectorOriginal: TVectorOriginal;
    zoom: TAffineMatrix;
    newShape: TVectorShape;
    justDown: boolean;
    newStartPoint: TPointF;
    newButton: TMouseButton;
    vectorLayer: Integer;
    mouseState: TShiftState;
    baseCaption: string;
    property vectorTransform: TAffineMatrix read GetVectorTransform;
    property penColor: TBGRAPixel read GetPenColor write SetPenColor;
    property backColor: TBGRAPixel read FBackColor write SetBackColor;
    property backGradStartColor: TBGRAPixel read FBackGradStartColor write SetBackGradStartColor;
    property backGradEndColor: TBGRAPixel read FBackGradEndColor write SetBackGradEndColor;
    property backTexture: TBGRABitmap read GetBackTexture write SetBackTexture;
    property penWidth: single read GetPenWidth write SetPenWidth;
    property penStyle: TBGRAPenStyle read GetPenStyle write SetPenStyle;
    property splineStyle: TSplineStyle read GetSplineStyle write SetSplineStyle;
    property currentTool: TPaintTool read FCurrentTool write SetCurrentTool;
  end;

var
  Form1: TForm1;

implementation

uses math, LCLType, BGRAPen, BGRAThumbnail, BGRAGradientScanner,
  BGRAGradientOriginal;

function IsCreateShapeTool(ATool: TPaintTool): boolean;
begin
  result := PaintToolClass[ATool] <> nil;
end;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  item: TMenuItem;
  ps: TPenStyle;
begin
  baseCaption:= Caption;

  img := TBGRALazPaintImage.Create(640,480);
  filename := '';
  vectorOriginal := TVectorOriginal.Create;
  vectorLayer := img.AddLayerFromOwnedOriginal(vectorOriginal);
  img.LayerOriginalMatrix[vectorLayer] := AffineMatrixScale(1,1);
  vectorOriginal.OnSelectShape:= @OnSelectShape;
  img.OnOriginalEditingChange:= @OnEditingChange;
  img.OnOriginalChange:= @OnOriginalChange;

  zoom := AffineMatrixScale(1,1);
  FPenStyleMenu := TPopupMenu.Create(nil);
  item:= TMenuItem.Create(FPenStyleMenu); item.Caption := 'Clear';
  item.OnClick := @OnClickPenStyle;       item.Tag := ord(psClear);
  FPenStyleMenu.Items.Add(item);
  for ps := psSolid to psDashDotDot do
  begin
    item:= TMenuItem.Create(FPenStyleMenu); item.Caption := PenStyleToStr[ps];
    item.OnClick := @OnClickPenStyle;       item.Tag := ord(ps);
    FPenStyleMenu.Items.Add(item);
  end;
  ButtonPenStyle.DropDownMenu := FPenStyleMenu;

  newShape:= nil;
  penColor := BGRABlack;
  backColor := CSSDodgerBlue;
  backGradStartColor := CSSRed;
  backGradEndColor := CSSYellow;
  penWidth := 5;
  penStyle := SolidPenStyle;
  currentTool:= ptRectangle;
  splineStyle:= ssEasyBezier;
  UpdateTitleBar;
end;

procedure TForm1.BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
var
  topLeftF, bottomRightF: TPointF;
  zoomBounds: TRect;
begin
  topLeftF := zoom*PointF(0,0);
  bottomRightF := zoom*PointF(img.Width,img.Height);
  zoomBounds := Rect(round(topLeftF.X),round(topLeftF.Y),round(bottomRightF.X),round(bottomRightF.Y));
  Bitmap.DrawCheckers(zoomBounds, CSSWhite,CSSSilver);
  if FFlattened = nil then
    UpdateFlattenedImage(rect(0,0,img.Width,img.Height), false);
  Bitmap.StretchPutImage(zoomBounds, FFlattened, dmLinearBlend);
  FLastEditorBounds := img.DrawEditor(Bitmap, vectorLayer, zoom, EditorPointSize);
end;

procedure TForm1.ButtonLoadTexClick(Sender: TObject);
var
  newTex: TBGRABitmap;
begin
  if OpenPictureDialog1.Execute then
  begin
    try
      newTex := TBGRABitmap.Create(OpenPictureDialog1.FileName, true);
      backTexture := newTex;
      newTex.FreeReference;
      RadioButtonTex.Checked:= true;
    except
      on ex: exception do
        ShowMessage(ex.Message);
    end;
  end;
  if RadioButtonTex.Checked and (backTexture = nil) then
    RadioButtonNone.Checked:= true;
end;

procedure TForm1.ButtonNewFileClick(Sender: TObject);
begin
  if Assigned(vectorOriginal) then
  begin
    vectorOriginal.Clear;
    filename := '';
    UpdateTitleBar;
  end;
end;

procedure TForm1.ButtonNoTexClick(Sender: TObject);
begin
  backTexture := nil;
  if RadioButtonTex.Checked then RadioButtonNone.Checked := true;
end;

procedure TForm1.ButtonOpenFileClick(Sender: TObject);
var
  openedImg: TBGRALazPaintImage;
  openedLayer: Integer;
  openedLayerOriginal: TBGRALayerCustomOriginal;
begin
  if OpenDialog1.Execute then
  begin
    openedImg := TBGRALazPaintImage.Create;
    try
      openedImg.LoadFromFile(OpenDialog1.FileName);
      if openedImg.NbLayers <> 1 then raise exception.Create('Expecting one layer only');
      openedLayer := 0;
      openedLayerOriginal := openedImg.LayerOriginal[openedLayer];
      if (openedLayerOriginal = nil) or not (openedLayerOriginal is TVectorOriginal) then
        raise exception.Create('Not a vectorial image');

      img.Free;
      img := openedImg;
      openedImg := nil;
      vectorLayer:= openedLayer;
      vectorOriginal := TVectorOriginal(openedLayerOriginal);
      vectorOriginal.OnSelectShape:= @OnSelectShape;
      img.OnOriginalEditingChange:= @OnEditingChange;
      img.OnOriginalChange:= @OnOriginalChange;
      filename:= OpenDialog1.FileName;
      UpdateTitleBar;
      ImageChangesCompletely;
    except
      on ex: exception do
        ShowMessage(ex.Message);
    end;
    openedImg.Free;
  end;
end;

procedure TForm1.ButtonSaveAsClick(Sender: TObject);
begin
  if not Assigned(img) then exit;
  if SaveDialog1.Execute then
  begin
    try
      img.SaveToFile(SaveDialog1.FileName);
      filename := SaveDialog1.FileName;
      UpdateTitleBar;
    except
      on ex: exception do
        ShowMessage(ex.Message);
    end;
  end;
end;

procedure TForm1.ButtonSaveFileClick(Sender: TObject);
begin
  if filename = '' then
    ButtonSaveAsClick(Sender)
  else
  begin
    try
      img.SaveToFile(filename);
    except
      on ex: exception do
        ShowMessage(ex.Message);
    end;
  end;
end;

procedure TForm1.BGRAVirtualScreen1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  imgPtF: TPointF;
  cur: TOriginalEditorCursor;
  handled: boolean;
begin
  mouseState:= Shift;
  imgPtF := AffineMatrixInverse(zoom)*PointF(X,Y);
  img.MouseDown(Button=mbRight, Shift, imgPtF.x, imgPtF.y, cur, handled);
  UpdateViewCursor(cur);
  if handled then exit;

  if not justDown and not Assigned(newShape) then
  begin
    newStartPoint := AffineMatrixInverse(vectorTransform)*imgPtF;
    newButton := Button;
    justDown := true;
  end;
end;

procedure TForm1.UpDownPenWidthChange(Sender: TObject; AByUser: boolean);
begin
  if FUpdatingSpinEditPenWidth then exit;
  penWidth := UpDownPenWidth.Value*0.1;
end;

procedure TForm1.ShapeBackGradColorMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ColorDialog1.Color := (Sender as TShape).Brush.Color;
  if ColorDialog1.Execute then
  begin
    if Sender = ShapeBackEndColor then
      backGradEndColor := ColorToBGRA(ColorDialog1.Color, backGradEndColor.alpha)
    else
      backGradStartColor := ColorToBGRA(ColorDialog1.Color, backGradStartColor.alpha);
  end;
end;

procedure TForm1.ButtonSwapGradColorClick(Sender: TObject);
var
  temp, c: TBGRAPixel;
begin
  temp := FBackGradStartColor;
  FBackGradStartColor := FBackGradEndColor;
  FBackGradEndColor := temp;
  c := FBackGradStartColor; c.alpha:= 255;
  ShapeBackStartColor.Brush.Color := c.ToColor;
  UpDownBackStartAlpha.Value:= FBackGradStartColor.alpha;
  c := FBackGradEndColor; c.alpha:= 255;
  ShapeBackEndColor.Brush.Color := c.ToColor;
  UpDownBackEndAlpha.Value:= FBackGradEndColor.alpha;
  UpdateShapeBackFill;
end;

procedure TForm1.UpDownBackGradAlphaChange(Sender: TObject; AByUser: boolean);
begin
  if AByUser then
  begin
    if Sender = UpDownBackEndAlpha then
      FBackGradEndColor.alpha := (Sender as TBCTrackbarUpdown).Value
    else
      FBackGradStartColor.alpha := (Sender as TBCTrackbarUpdown).Value;
    if RadioButtonGradient.Checked then UpdateShapeBackFill;
  end;
end;

procedure TForm1.BGRAVirtualScreen1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  imgPtF, ptF: TPointF;
  prevRF, rF: TRectF;
  cur: TOriginalEditorCursor;
  handled: boolean;
begin
  mouseState:= Shift;
  imgPtF := AffineMatrixInverse(zoom)*PointF(X,Y);
  img.MouseMove(Shift, imgPtF.X, imgPtF.Y, cur, handled);
  UpdateViewCursor(cur);

  ptF := AffineMatrixInverse(vectorTransform)*imgPtF;
  if justDown and not Assigned(newShape) and IsCreateShapeTool(currentTool) then
  begin
    vectorOriginal.DeselectShape;
    newShape := CreateShape(newStartPoint,ptF);
    rF := newShape.GetRenderBounds(InfiniteRect, vectorTransform);
    ImageChange(rF);
    justDown := false;
  end else
  if Assigned(newShape) then
  begin
    prevRF := newShape.GetRenderBounds(InfiniteRect, vectorTransform);
    newShape.QuickDefine(newStartPoint,ptF);
    rF := newShape.GetRenderBounds(InfiniteRect, vectorTransform);
    ImageChange(rF.Union(prevRF, true));
  end;
end;

procedure TForm1.BGRAVirtualScreen1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  rF: TRectF;
  imgPtF: TPointF;
  handled: boolean;
  cur: TOriginalEditorCursor;
  addedShape: TVectorShape;
begin
  mouseState:= Shift;
  imgPtF := AffineMatrixInverse(zoom)*PointF(X,Y);
  img.MouseUp(Button = mbRight, Shift, imgPtF.X, imgPtF.Y, cur, handled);
  if handled then RenderAndUpdate(false);
  UpdateViewCursor(cur);

  if justDown and (Button = newButton) then
  begin
    if IsCreateShapeTool(currentTool) and (vsuCreate in PaintToolClass[currentTool].Usermodes) then
    begin
      vectorOriginal.AddShape(CreateShape(newStartPoint,newStartPoint), vsuCreate);
    end else
      vectorOriginal.MouseClick(newStartPoint);
    justDown:= false;
  end
  else if Assigned(newShape) and (Button = newButton) then
  begin
    rF := newShape.GetRenderBounds(InfiniteRect, vectorTransform);
    if not IsEmptyRectF(rF) or (vsuCreate in newShape.Usermodes) then
    begin
      addedShape := newShape;
      newShape := nil;
      vectorOriginal.AddShape(addedShape, vsuCreate);
    end
    else
      FreeAndNil(newShape);
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  RemoveExtendedStyleControls;
  img.Free;
  FFlattened.Free;
  FBackTexture.FreeReference;
  FPenStyleMenu.Free;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  if Key = VK_RETURN then
  begin
    Key := 0;
    if Assigned(vectorOriginal) then
      vectorOriginal.DeselectShape;
  end else
  if Key = VK_DELETE then
  begin
    Key := 0;
    if Assigned(vectorOriginal) and Assigned(vectorOriginal.SelectedShape) then
      vectorOriginal.RemoveShape(vectorOriginal.SelectedShape);
  end;
end;

procedure TForm1.RadioButtonBackChange(Sender: TObject);
begin
  if FUpdatingFromShape then exit;
  if ((Sender = RadioButtonNone) and RadioButtonNone.Checked) or
     ((Sender = RadioButtonSolid) and RadioButtonSolid.Checked) or
     ((Sender = RadioButtonGradient) and RadioButtonGradient.Checked) then
  begin
    UpdateShapeBackFill;
  end else
  if (Sender = RadioButtonTex) and RadioButtonTex.Checked then
  begin
    if backTexture = nil then
      ButtonLoadTexClick(Sender)
    else
    UpdateShapeBackFill;
  end;
  UpdateBackComponentsVisibility;
end;

procedure TForm1.ShapeBackColorMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ColorDialog1.Color := ShapeBackColor.Brush.Color;
  if ColorDialog1.Execute then
  begin
    if backColor.alpha <> 0 then
      backColor := ColorToBGRA(ColorDialog1.Color, backColor.alpha)
    else
      backColor := ColorDialog1.Color;
  end;
end;

procedure TForm1.ShapePenColorMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ColorDialog1.Color := ShapePenColor.Brush.Color;
  if ColorDialog1.Execute then
  begin
    if penColor.alpha <> 0 then
      penColor := ColorToBGRA(ColorDialog1.Color, penColor.alpha)
    else
      penColor := ColorDialog1.Color;
  end;
end;

procedure TForm1.ToolButtonClick(Sender: TObject);
begin
  currentTool := ptHand;
  if ToolButtonEllipse.Down then currentTool:= ptEllipse;
  if ToolButtonRectangle.Down then currentTool:= ptRectangle;
  if ToolButtonPolyline.Down then currentTool:= ptPolyline;
  if ToolButtonCurve.Down then currentTool:= ptCurve;
  if ToolButtonPolygon.Down then currentTool:= ptPolygon;
  if ToolButtonClosedCurve.Down then currentTool:= ptClosedCurve;
  if IsCreateShapeTool(currentTool) then
  begin
    if Assigned(vectorOriginal) and (vectorOriginal.SelectedShape <> nil) then vectorOriginal.DeselectShape
    else UpdateToolbarFromShape(nil);

    if currentTool in [ptPolyline, ptCurve] then
      RadioButtonNone.Checked := true;
  end;
end;

procedure TForm1.UpDownBackAlphaChange(Sender: TObject; AByUser: boolean);
begin
  if AByUser then
  begin
    FBackColor:= ColorToBGRA(ShapeBackColor.Brush.Color, UpDownBackAlpha.Value);
    if RadioButtonSolid.Checked then UpdateShapeBackFill;
  end;
end;

procedure TForm1.UpDownPenAlphaChange(Sender: TObject; AByUser: boolean);
begin
  if AByUser then
  begin
    FPenColor:= ColorToBGRA(ShapePenColor.Brush.Color, UpDownPenAlpha.Value);
    if Assigned(vectorOriginal) and Assigned(vectorOriginal.SelectedShape) then
      vectorOriginal.SelectedShape.PenColor:= FPenColor;
  end;
end;

procedure TForm1.ComboBoxSplineStyleChange(Sender: TObject);
begin
  if FUpdatingComboboxSplineStyle then exit;
  if FComboboxSplineStyle.ItemIndex <> -1 then
    splineStyle:= TSplineStyle(FComboboxSplineStyle.ItemIndex);
end;

function TForm1.GetBackTexture: TBGRABitmap;
begin
  result := FBackTexture;
end;

function TForm1.GetPenColor: TBGRAPixel;
begin
  result := FPenColor;
end;

function TForm1.GetPenStyle: TBGRAPenStyle;
begin
  result := FPenStyle;
end;

function TForm1.GetPenWidth: single;
begin
  result := FPenWidth;
end;

function TForm1.GetSplineStyle: TSplineStyle;
begin
  result := FSplineStyle;
end;

function TForm1.GetVectorTransform: TAffineMatrix;
begin
  if vectorLayer<>-1 then
    result:= AffineMatrixTranslation(-0.5,-0.5)*img.LayerOriginalMatrix[vectorLayer]*AffineMatrixTranslation(0.5,0.5)
  else
    result:= AffineMatrixIdentity;
end;

procedure TForm1.ImageChange(ARectF: TRectF);
var
  changeRect: TRect;
begin
  if not IsEmptyRectF(ARectF) then
  begin
    changeRect := rect(floor(ARectF.Left),floor(ARectF.Top),ceil(ARectF.Right),ceil(ARectF.Bottom));
    UpdateFlattenedImage(changeRect);
  end;
end;

procedure TForm1.OnEditingChange(ASender: TObject;
  AOriginal: TBGRALayerCustomOriginal);
begin
  if AOriginal <> vectorOriginal then exit;
  UpdateView(EmptyRect);
end;

procedure TForm1.OnOriginalChange(ASender: TObject; AOriginal: TBGRALayerCustomOriginal);
var
  slowShape: boolean;
begin
  if AOriginal <> vectorOriginal then exit;
  slowShape := false;
  if mouseState * [ssLeft,ssRight] <> [] then
  begin
    if Assigned(vectorOriginal) and Assigned(vectorOriginal.SelectedShape) then
      slowShape := vectorOriginal.SelectedShape.GetIsSlow(vectorTransform);
  end;
  RenderAndUpdate(slowShape);
end;

procedure TForm1.OnSelectShape(ASender: TObject; AShape: TVectorShape;
  APreviousShape: TVectorShape);
begin
  if ASender <> vectorOriginal then exit;
  UpdateToolbarFromShape(AShape);
  if APreviousShape <> nil then
    if IsEmptyRectF(APreviousShape.GetRenderBounds(InfiniteRect, vectorTransform)) then
    begin
      vectorOriginal.RemoveShape(APreviousShape);
      ShowMessage('Empty shape has been deleted');
    end;
end;

procedure TForm1.OnClickPenStyle(ASender: TObject);
begin
  Case TPenStyle((ASender as TMenuItem).Tag) of
  psSolid: penStyle := SolidPenStyle;
  psDash: penStyle := DashPenStyle;
  psDot: penStyle := DotPenStyle;
  psDashDot: penStyle := DashDotPenStyle;
  psDashDotDot: penStyle := DashDotDotPenStyle;
  else penStyle := ClearPenStyle;
  end;
end;

procedure TForm1.SetBackColor(AValue: TBGRAPixel);
begin
  FBackColor := AValue;
  ShapeBackColor.Brush.Color := AValue.ToColor;
  UpDownBackAlpha.Value := AValue.alpha;
  if not FUpdatingFromShape and RadioButtonSolid.Checked then
    UpdateShapeBackFill;
end;

procedure TForm1.SetBackGradEndColor(AValue: TBGRAPixel);
begin
  FBackGradEndColor := AValue;
  UpDownBackEndAlpha.Value := AValue.alpha;
  AValue.alpha := 255;
  ShapeBackEndColor.Brush.Color := AValue.ToColor;
  if not FUpdatingFromShape and RadioButtonGradient.Checked then
    UpdateShapeBackFill;
end;

procedure TForm1.SetBackGradStartColor(AValue: TBGRAPixel);
begin
  FBackGradStartColor := AValue;
  UpDownBackStartAlpha.Value := AValue.alpha;
  AValue.alpha := 255;
  ShapeBackStartColor.Brush.Color := AValue.ToColor;
  if not FUpdatingFromShape and RadioButtonGradient.Checked then
    UpdateShapeBackFill;
end;

procedure TForm1.SetBackTexture(AValue: TBGRABitmap);
var
  thumb: TBGRABitmap;
  bmpThumb: TBitmap;
begin
  if AValue = FBackTexture then exit;

  if Assigned(FBackTexture) then
  begin
    FBackTexture.FreeReference;
    FBackTexture := nil;
  end;
  if Assigned(AValue) then
    FBackTexture := AValue.NewReference as TBGRABitmap;

  if Assigned(FBackTexture) then
  begin
    thumb := GetBitmapThumbnail(FBackTexture, BackImage.Width,BackImage.Height,BGRAPixelTransparent,true);
    try
      bmpThumb := thumb.MakeBitmapCopy(clBtnFace);
      try
        BackImage.Picture.Assign(bmpThumb);
      finally
        bmpThumb.Free;
      end;
    finally
      thumb.Free;
    end;
    BackImage.Visible := true;
  end else
  begin
    BackImage.Picture.Clear;
    BackImage.Visible := false;
  end;

  if not FUpdatingFromShape and RadioButtonTex.Checked then
    UpdateShapeBackFill;
end;

procedure TForm1.SetCurrentTool(AValue: TPaintTool);
begin
  if FCurrentTool=AValue then Exit;
  FCurrentTool:=AValue;
  ToolButtonRectangle.Down := FCurrentTool = ptRectangle;
  ToolButtonEllipse.Down := FCurrentTool = ptEllipse;
  ToolButtonPolygon.Down := FCurrentTool = ptPolygon;
  ToolButtonClosedCurve.Down := FCurrentTool = ptClosedCurve;
  ToolButtonPolyline.Down := FCurrentTool = ptPolyline;
  ToolButtonCurve.Down := FCurrentTool = ptCurve;
end;

procedure TForm1.SetPenColor(AValue: TBGRAPixel);
begin
  FPenColor := AValue;
  ShapePenColor.Brush.Color := BGRA(AValue.red,AValue.green,AValue.blue).ToColor;
  UpDownPenAlpha.Value := AValue.alpha;
  if not FUpdatingFromShape and Assigned(vectorOriginal) then
  begin
    if Assigned(vectorOriginal.SelectedShape) then
      vectorOriginal.SelectedShape.PenColor := AValue;
  end;
end;

procedure TForm1.SetPenStyle(AValue: TBGRAPenStyle);
begin
  FPenStyle := AValue;
  ButtonPenStyle.Caption:= PenStyleToStr[BGRAToPenStyle(AValue)];
  if not FUpdatingFromShape and Assigned(vectorOriginal) and Assigned(vectorOriginal.SelectedShape) then
    vectorOriginal.SelectedShape.PenStyle := FPenStyle;
end;

procedure TForm1.SetPenWidth(AValue: single);
var
  cur: single;
begin
  FPenWidth := AValue;
  cur := UpDownPenWidth.Value*0.1;
  if AValue <> cur then
  begin
    FUpdatingSpinEditPenWidth:= true;
    UpDownPenWidth.Value := Round(AValue*10);
    FUpdatingSpinEditPenWidth:= false;
  end;
  if not FUpdatingFromShape and Assigned(vectorOriginal) and Assigned(vectorOriginal.SelectedShape) then
    vectorOriginal.SelectedShape.PenWidth:= penWidth;
end;

procedure TForm1.SetSplineStyle(AValue: TSplineStyle);
begin
  FSplineStyle := AValue;
  if Assigned(FComboboxSplineStyle) then
  begin
    FUpdatingComboboxSplineStyle := true;
    FComboboxSplineStyle.ItemIndex:= ord(FSplineStyle);
    FUpdatingComboboxSplineStyle := false;
  end;
  if not FUpdatingFromShape and Assigned(vectorOriginal) and Assigned(vectorOriginal.SelectedShape) and
    (vectorOriginal.SelectedShape is TCurveShape) then
    TCurveShape(vectorOriginal.SelectedShape).SplineStyle := FSplineStyle;
end;

procedure TForm1.UpdateViewCursor(ACursor: TOriginalEditorCursor);
begin
  case ACursor of
    oecDefault: BGRAVirtualScreen1.Cursor := crDefault;
    oecMove: BGRAVirtualScreen1.Cursor := crSizeAll;
    oecMoveN: BGRAVirtualScreen1.Cursor := crSizeN;
    oecMoveS: BGRAVirtualScreen1.Cursor := crSizeS;
    oecMoveE: BGRAVirtualScreen1.Cursor := crSizeE;
    oecMoveW: BGRAVirtualScreen1.Cursor := crSizeW;
    oecMoveNE: BGRAVirtualScreen1.Cursor := crSizeNE;
    oecMoveSW: BGRAVirtualScreen1.Cursor := crSizeSW;
    oecMoveNW: BGRAVirtualScreen1.Cursor := crSizeNW;
    oecMoveSE: BGRAVirtualScreen1.Cursor := crSizeSE;
  end;
end;

procedure TForm1.RenderAndUpdate(ADraft: boolean);
var
  renderedRect: TRect;
begin
  renderedRect := img.RenderOriginalsIfNecessary(ADraft);
  UpdateFlattenedImage(renderedRect);
end;

procedure TForm1.UpdateFlattenedImage(ARect: TRect; AUpdateView: boolean);
var
  shapeRectF: TRectF;
  shapeRect: TRect;
begin
  if FFlattened = nil then
    FFlattened := img.ComputeFlatImage
  else
  if not IsRectEmpty(ARect) then
  begin
    FFlattened.FillRect(ARect,BGRAPixelTransparent,dmSet);
    FFlattened.ClipRect := ARect;
    img.Draw(FFlattened, 0,0);
    FFlattened.NoClip;
  end;

  if Assigned(newShape) and not IsRectEmpty(ARect) then
  begin
    shapeRectF := newShape.GetRenderBounds(InfiniteRect, vectorTransform);
    with shapeRectF do
      shapeRect := rect(floor(Left),floor(Top),ceil(Right),ceil(Bottom));
    if IntersectRect(shapeRect, shapeRect, ARect) then
    begin
      FFlattened.ClipRect := shapeRect;
      newShape.Render(FFlattened, vectorTransform, newShape.GetIsSlow(vectorTransform));
      FFlattened.NoClip;
    end;
  end;

  if AUpdateView then
    UpdateView(ARect);
end;

procedure TForm1.UpdateView(AImageChangeRect: TRect);
var
  viewRectF: TRectF;
  viewRect, newEditorBounds: TRect;
begin
  if IsRectEmpty(AImageChangeRect) then
  begin
    viewRectF := EmptyRectF;
    viewRect := EmptyRect;
  end
  else
  begin
    with AImageChangeRect do
      viewRectF := RectF(zoom* PointF(Left,Top), zoom* PointF(Right,Bottom));
    viewRect := rect(floor(viewRectF.Left),floor(viewRectF.Top),ceil(viewRectF.Right),ceil(viewRectF.Bottom));
  end;

  if not IsRectEmpty(FLastEditorBounds) then
  begin
    if IsRectEmpty(viewRect) then viewRect := FLastEditorBounds else
      UnionRect(viewRect,viewRect,FLastEditorBounds);
  end;
  if Assigned(img) then
  begin
    newEditorBounds := img.GetEditorBounds(vectorLayer, zoom, EditorPointSize);
    if not IsRectEmpty(newEditorBounds) then
    begin
      if IsRectEmpty(viewRect) then viewRect := newEditorBounds else
        UnionRect(viewRect,viewRect,newEditorBounds);
    end;
  end;

  if not IsRectEmpty(viewRect) then
  begin
    viewRect.Inflate(1,1);
    BGRAVirtualScreen1.RedrawBitmap(viewRect);
  end;
end;

procedure TForm1.UpdateToolbarFromShape(AShape: TVectorShape);
const ControlMargin = 8;
var
  f: TVectorShapeFields;
  showSplineStyle: boolean;
  nextControlPos: TPoint;
  s: TSplineStyle;
  texSource: TBGRABitmap;
begin
  RemoveExtendedStyleControls;

  if AShape <> nil then
  begin
    FUpdatingFromShape := true;
    f := AShape.Fields;
    if vsfPenColor in f then penColor := AShape.PenColor;
    if vsfPenWidth in f then penWidth:= AShape.PenWidth;
    if vsfPenStyle in f then penStyle:= AShape.PenStyle;

    if vsfBackFill in f then
    begin
      if AShape.BackFill.IsTexture then
      begin
        texSource := AShape.BackFill.Texture;
        if Assigned(texSource) then backTexture := texSource;
        RadioButtonTex.Checked := true;
      end else
      if AShape.BackFill.IsSolid and (AShape.BackFill.SolidColor.alpha <> 0) then
      begin
        backColor := AShape.BackFill.SolidColor;
        RadioButtonSolid.Checked := true;
      end else
      if AShape.BackFill.IsGradient then
      begin
        backGradStartColor := AShape.BackFill.Gradient.StartColor;
        backGradEndColor := AShape.BackFill.Gradient.EndColor;
        RadioButtonGradient.Checked := true;
      end else
        RadioButtonNone.Checked := true;
      UpdateBackComponentsVisibility;
    end;

    if AShape is TCurveShape then
    begin
      showSplineStyle:= true;
      splineStyle:= TCurveShape(AShape).SplineStyle;
    end else
      showSplineStyle:= false;
    FUpdatingFromShape := false;
  end else
  begin
    if IsCreateShapeTool(currentTool) then
    begin
      f := PaintToolClass[currentTool].Fields;
      showSplineStyle:= PaintToolClass[currentTool] = TCurveShape;
    end
    else
    begin
      f := [];
      showSplineStyle:= false;
    end;
  end;
  UpDownPenWidth.Enabled := vsfPenWidth in f;
  ButtonPenStyle.Enabled:= vsfPenStyle in f;

  nextControlPos := Point(ControlMargin,ShapeBackColor.Top);
  if showSplineStyle then
  begin
    FComboboxSplineStyle := TComboBox.Create(nil);
    FComboboxSplineStyle.Style := csDropDownList;
    FComboboxSplineStyle.Left := nextControlPos.X;
    FComboboxSplineStyle.Top := nextControlPos.Y;
    for s := low(SplineStyleToStr) to high(SplineStyleToStr) do
      FComboboxSplineStyle.Items.Add(SplineStyleToStr[s]);
    FComboboxSplineStyle.ItemIndex := ord(splineStyle);
    FComboboxSplineStyle.Width := 120;
    FComboboxSplineStyle.OnChange:= @ComboBoxSplineStyleChange;
    PanelExtendedStyle.InsertControl(FComboboxSplineStyle);
    nextControlPos.X := FComboboxSplineStyle.Left + FComboboxSplineStyle.Width + ControlMargin;
  end;
end;

procedure TForm1.UpdateTitleBar;
begin
  if filename = '' then
    Caption := baseCaption + ' - New image - ' + inttostr(img.Width)+'x'+inttostr(img.Height)
  else
    Caption := baseCaption + ' - ' + filename + ' - ' + inttostr(img.Width)+'x'+inttostr(img.Height);
end;

procedure TForm1.ImageChangesCompletely;
begin
  FreeAndNil(FFlattened);
  BGRAVirtualScreen1.DiscardBitmap;
end;

function TForm1.CreateShape(const APoint1,APoint2: TPointF): TVectorShape;
var
  vectorFill: TVectorialFill;
begin
  if not IsCreateShapeTool(currentTool) then
    raise exception.Create('No shape type selected');
  result := PaintToolClass[currentTool].Create(vectorOriginal);
  result.PenColor := penColor;

  if vsfBackFill in result.Fields then
  begin
    vectorFill := CreateBackFill;
    result.BackFill := vectorFill;
    vectorFill.Free;
  end;

  result.PenWidth := penWidth;
  result.PenStyle := penStyle;
  if currentTool in[ptClosedCurve,ptPolygon] then
    TCustomPolypointShape(result).Closed := true;
  if result is TCurveShape then TCurveShape(result).SplineStyle:= splineStyle;
  result.QuickDefine(APoint1,APoint2);
end;

function TForm1.CreateBackFill: TVectorialFill;
var
  grad: TBGRALayerGradientOriginal;
begin
  if RadioButtonSolid.Checked then
    result := TVectorialFill.CreateAsSolid(FBackColor)
  else if RadioButtonTex.Checked then
    result := TVectorialFill.CreateAsTexture(backTexture, AffineMatrixIdentity)
  else if RadioButtonGradient.Checked then
  begin
    grad := TBGRALayerGradientOriginal.Create;
    grad.Origin := PointF(0,0);
    grad.XAxis := PointF(img.Width,img.Height);
    grad.StartColor := FBackGradStartColor;
    grad.EndColor := FBackGradEndColor;
    result := TVectorialFill.CreateAsGradient(grad, true);
  end
  else result := nil; //none
end;

procedure TForm1.RemoveExtendedStyleControls;
begin
  if Assigned(FComboboxSplineStyle) then
  begin
    PanelExtendedStyle.RemoveControl(FComboboxSplineStyle);
    FreeAndNil(FComboboxSplineStyle);
  end;
end;

procedure TForm1.UpdateBackComponentsVisibility;
begin
  ShapeBackColor.Visible := RadioButtonSolid.Checked;
  UpDownBackAlpha.Visible := RadioButtonSolid.Checked;
  ShapeBackStartColor.Visible := RadioButtonGradient.Checked;
  UpDownBackStartAlpha.Visible := RadioButtonGradient.Checked;
  ButtonSwapGradColor.Visible := RadioButtonGradient.Checked;
  ShapeBackEndColor.Visible := RadioButtonGradient.Checked;
  UpDownBackEndAlpha.Visible := RadioButtonGradient.Checked;
  ButtonLoadTex.Visible := RadioButtonTex.Checked;
  ButtonNoTex.Visible := RadioButtonTex.Checked;
  BackImage.Visible := RadioButtonTex.Checked;
end;

procedure TForm1.UpdateShapeBackFill;
var
  vectorFill: TVectorialFill;
  wasTex: Boolean;
begin
  if Assigned(vectorOriginal) and Assigned(vectorOriginal.SelectedShape) and
    (vsfBackFill in vectorOriginal.SelectedShape.Fields) then
  begin
    if RadioButtonTex.Checked and vectorOriginal.SelectedShape.BackFill.IsTexture then
      vectorFill := TVectorialFill.CreateAsTexture(FBackTexture, vectorOriginal.SelectedShape.BackFill.TextureMatrix)
    else if RadioButtonGradient.Checked and vectorOriginal.SelectedShape.BackFill.IsGradient then
    begin
      vectorFill := vectorOriginal.SelectedShape.BackFill.Duplicate;
      vectorFill.Gradient.StartColor := FBackGradStartColor;
      vectorFill.Gradient.EndColor := FBackGradEndColor;
    end else
      vectorFill := CreateBackFill;

    wasTex := vectorOriginal.SelectedShape.BackFill.IsTexture;
    vectorOriginal.SelectedShape.BackFill:= vectorFill;
    if wasTex or (Assigned(vectorFill) and vectorFill.IsTexture) then vectorOriginal.RemoveUnusedTextures;
    vectorFill.Free;
  end;
end;

end.

