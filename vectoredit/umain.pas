unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Spin, BGRAVirtualScreen, BGRALazPaint, BGRABitmap,
  BGRABitmapTypes, BGRATransform, BGRALayerOriginal, uvectororiginal;

type

  { TForm1 }

  TForm1 = class(TForm)
    BGRAVirtualScreen1: TBGRAVirtualScreen;
    CheckBoxPen: TCheckBox;
    CheckBoxBack: TCheckBox;
    ColorDialog1: TColorDialog;
    FloatSpinEditPenWidth: TFloatSpinEdit;
    Label3: TLabel;
    Panel1: TPanel;
    ShapePenColor: TShape;
    ShapeBackColor: TShape;
    procedure BGRAVirtualScreen1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BGRAVirtualScreen1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure BGRAVirtualScreen1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FloatSpinEditPenWidthChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ShapeBackColorMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ShapePenColorMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FPenColor, FBackColor: TBGRAPixel;
    function GetBackColor: TBGRAPixel;
    function GetPenColor: TBGRAPixel;
    function GetPenWidth: single;
    function GetVectorTransform: TAffineMatrix;
    procedure ImageChange(ARectF: TRectF);
    procedure OnEditingChange(ASender: TObject; AOriginal: TBGRALayerCustomOriginal);
    procedure OnOriginalChange(ASender: TObject; AOriginal: TBGRALayerCustomOriginal);
    procedure SetBackColor(AValue: TBGRAPixel);
    procedure SetPenColor(AValue: TBGRAPixel);
    procedure SetPenWidth(AValue: single);
    procedure UpdateViewCursor(ACursor: TOriginalEditorCursor);
    procedure RenderAndUpdate(ADraft: boolean);
    { private declarations }
  public
    { public declarations }
    img: TBGRALazPaintImage;
    vectorOriginal: TVectorOriginal;
    zoom: TAffineMatrix;
    newShape: TVectorShape;
    justDown: boolean;
    newStartPoint: TPointF;
    newButton: TMouseButton;
    vectorLayer: Integer;
    mouseState: TShiftState;
    property vectorTransform: TAffineMatrix read GetVectorTransform;
    property penColor: TBGRAPixel read GetPenColor write SetPenColor;
    property backColor: TBGRAPixel read GetBackColor write SetBackColor;
    property penWidth: single read GetPenWidth write SetPenWidth;
  end;

var
  Form1: TForm1;

implementation

uses math, LCLType;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  img := TBGRALazPaintImage.Create(800,600);
  vectorOriginal := TVectorOriginal.Create;
  vectorLayer := img.AddLayerFromOwnedOriginal(vectorOriginal);
  zoom := AffineMatrixScale(4,4);
  img.OnOriginalEditingChange:= @OnEditingChange;
  img.OnOriginalChange:= @OnOriginalChange;
  newShape:= nil;
  penColor := BGRABlack;
  backColor := CSSDodgerBlue;
end;

procedure TForm1.BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
var
  topLeftF, bottomRightF: TPointF;
  zoomBounds, r: TRect;
  flattened: TBGRABitmap;
  rF: TRectF;
begin
  topLeftF := zoom*PointF(0,0);
  bottomRightF := zoom*PointF(img.Width,img.Height);
  zoomBounds := Rect(round(topLeftF.X),round(topLeftF.Y),round(bottomRightF.X),round(bottomRightF.Y));
  Bitmap.DrawCheckers(zoomBounds, CSSWhite,CSSSilver);
  flattened := img.ComputeFlatImage;
  if Assigned(newShape) then
  begin
    rF := newShape.GetRenderBounds(vectorTransform);
    with rF do
      r := rect(floor(Left),floor(Top),ceil(Right),ceil(Bottom));
    flattened.ClipRect := r;
    newShape.Render(flattened, vectorTransform, false);
    flattened.NoClip;
  end;
  Bitmap.StretchPutImage(zoomBounds, flattened, dmDrawWithTransparency);
  img.DrawEditor(Bitmap, vectorLayer, zoom, 8);
  flattened.Free;
end;

procedure TForm1.FloatSpinEditPenWidthChange(Sender: TObject);
begin
  if Assigned(vectorOriginal) then
  begin
    if Assigned(vectorOriginal.SelectedShape) then
      vectorOriginal.SelectedShape.PenWidth:= FloatSpinEditPenWidth.Value;
  end;
end;

procedure TForm1.BGRAVirtualScreen1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  imgPtF,ptF: TPointF;
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
    ptF := AffineMatrixInverse(vectorTransform)*imgPtF;
    newStartPoint := ptF;
    newButton := Button;
    justDown := true;
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
  if justDown and not Assigned(newShape) then
  begin
    vectorOriginal.DeselectShape;
    newShape := TRectShape.Create;
    newShape.penColor := penColor;
    newShape.backColor := backColor;
    newShape.penWidth := penWidth;
    newShape.QuickDefine(newStartPoint,ptF);
    rF := newShape.GetRenderBounds(vectorTransform);
    ImageChange(rF);
    justDown := false;
  end else
  if Assigned(newShape) then
  begin
    prevRF := newShape.GetRenderBounds(vectorTransform);
    newShape.QuickDefine(newStartPoint,ptF);
    rF := newShape.GetRenderBounds(vectorTransform);
    ImageChange(rF.Union(prevRF, true));
  end;
end;

procedure TForm1.BGRAVirtualScreen1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  rF: TRectF;
  idxShape: Integer;
  imgPtF: TPointF;
  handled: boolean;
  cur: TOriginalEditorCursor;
begin
  mouseState:= Shift;
  imgPtF := AffineMatrixInverse(zoom)*PointF(X,Y);
  img.MouseUp(Button = mbRight, Shift, imgPtF.X, imgPtF.Y, cur, handled);
  if handled then RenderAndUpdate(false);
  UpdateViewCursor(cur);

  if justDown and (Button = newButton) then
  begin
    vectorOriginal.MouseClick(newStartPoint);
    justDown:= false;
  end
  else if Assigned(newShape) and (Button = newButton) then
  begin
    rF := newShape.GetRenderBounds(vectorTransform);
    if not IsEmptyRectF(rF) then
    begin
      idxShape := vectorOriginal.AddShape(newShape);
      RenderAndUpdate(false);
      vectorOriginal.SelectShape(idxShape);
    end
    else
      newShape.Free;

    newShape := nil;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  img.Free;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  if Key = VK_RETURN then
  begin
    Key := 0;
    if Assigned(vectorOriginal) then
      vectorOriginal.DeselectShape;
  end;
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

function TForm1.GetBackColor: TBGRAPixel;
begin
  result := FBackColor;
end;

function TForm1.GetPenColor: TBGRAPixel;
begin
  result := FPenColor;
end;

function TForm1.GetPenWidth: single;
begin
  result := FloatSpinEditPenWidth.Value;
end;

function TForm1.GetVectorTransform: TAffineMatrix;
begin
  if vectorLayer<>-1 then
    result:= img.LayerOriginalMatrix[vectorLayer]
  else
    result:= AffineMatrixIdentity;
end;

procedure TForm1.ImageChange(ARectF: TRectF);
var
  r, vr: TRect;
  viewRectF: TRectF;
begin
  if not IsEmptyRectF(ARectF) then
  begin
    r := rect(floor(ARectF.Left),floor(ARectF.Top),ceil(ARectF.Right),ceil(ARectF.Bottom));
    viewRectF := RectF(zoom* PointF(r.Left,r.Top), zoom* PointF(r.Right,r.Bottom));
    vr := rect(floor(viewRectF.Left),floor(viewRectF.Top),ceil(viewRectF.Right),ceil(viewRectF.Bottom));
    vr.Inflate(1,1);
    BGRAVirtualScreen1.RedrawBitmap(vR);
  end;
end;

procedure TForm1.OnEditingChange(ASender: TObject;
  AOriginal: TBGRALayerCustomOriginal);
begin
  BGRAVirtualScreen1.DiscardBitmap;
end;

procedure TForm1.OnOriginalChange(ASender: TObject; AOriginal: TBGRALayerCustomOriginal);
begin
  RenderAndUpdate(mouseState * [ssLeft,ssRight] <> []);
end;

procedure TForm1.SetBackColor(AValue: TBGRAPixel);
begin
  FBackColor := AValue;
  if FBackColor.alpha = 0 then
    CheckBoxBack.Checked := false
  else
  begin
    CheckBoxBack.Checked := true;
    ShapeBackColor.Brush.Color := AValue.ToColor;
  end;
  if Assigned(vectorOriginal) then
  begin
    if Assigned(vectorOriginal.SelectedShape) then
      vectorOriginal.SelectedShape.BackColor := AValue;
  end;
end;

procedure TForm1.SetPenColor(AValue: TBGRAPixel);
begin
  FPenColor := AValue;
  if FPenColor.alpha = 0 then
    CheckBoxPen.Checked := false
  else
  begin
    CheckBoxPen.Checked := true;
    ShapePenColor.Brush.Color := AValue.ToColor;
  end;
  if Assigned(vectorOriginal) then
  begin
    if Assigned(vectorOriginal.SelectedShape) then
      vectorOriginal.SelectedShape.PenColor := AValue;
  end;
end;

procedure TForm1.SetPenWidth(AValue: single);
begin
  FloatSpinEditPenWidth.Value := AValue;
end;

procedure TForm1.UpdateViewCursor(ACursor: TOriginalEditorCursor);
begin
  case ACursor of
    oecDefault: BGRAVirtualScreen1.Cursor := crDefault;
    oecMove: BGRAVirtualScreen1.Cursor := crSizeAll;
  end;
end;

procedure TForm1.RenderAndUpdate(ADraft: boolean);
var
  renderedRect, vR: TRect;
  viewRectF: TRectF;
begin
  renderedRect := img.RenderOriginalsIfNecessary(ADraft);
  if not IsRectEmpty(renderedRect) then
  begin
    with renderedRect do
      viewRectF := RectF(zoom* PointF(Left,Top), zoom* PointF(Right,Bottom));
    vR := rect(floor(viewRectF.Left),floor(viewRectF.Top),ceil(viewRectF.Right),ceil(viewRectF.Bottom));
    vR.Inflate(1,1);
    BGRAVirtualScreen1.RedrawBitmap(vR);
  end;
end;

end.

