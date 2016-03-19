unit aa_demo_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, BGRABitmapTypes, BGRABitmap, lmessages, Spin;

type

  { TForm1 }

  TForm1 = class(TForm)
    SpinEdit_Gamma: TFloatSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label_PixelSizeValue: TLabel;
    Panel1: TPanel;
    TrackBar_PixelSize: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure SpinEdit_GammaChange(Sender: TObject);
    procedure TrackBar_PixelSizeChange(Sender: TObject);
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
  private
    { private declarations }
  public
    { public declarations }
    pts: array[0..2] of TPointF;
    MovingPointIndex: integer;
    MovingOrigin: TPointF;
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

procedure NicePoint(bmp: TBGRABitmap; x, y: single);
begin
    bmp.EllipseAntialias(x,y,4,4,BGRA(0,0,0,192),1);
    bmp.EllipseAntialias(x,y,3,3,BGRA(255,255,255,192),1);
    bmp.EllipseAntialias(x,y,2,2,BGRA(0,0,0,192),1);
end;

{ TForm1 }

procedure TForm1.FormPaint(Sender: TObject);
var zoomTx,zoomTy,tx,ty,pixSize: integer;
    bmp,bmpPreview: TBGRABitmap;
    i: Integer;
    x,y: integer;

  function coordToBmp(pt: TPointF): TPointF;
  begin
      result := (pt+PointF(0.5,0.5))*(1/pixSize)-PointF(0.5,0.5);
  end;

begin
  zoomTx := ClientWidth;
  zoomTy := Panel1.Top;
  pixSize := TrackBar_PixelSize.Position;
  tx := (zoomTx+pixSize-1) div pixSize;
  ty := (zoomTy+pixSize-1) div pixSize;

  //draw triangle
  bmp := TBGRABitmap.Create(tx,ty,BGRAWhite);
  bmp.FillPolyAntialias([coordToBmp(pts[0]),coordToBmp(pts[1]),coordToBmp(pts[2])],BGRABlack);

  //draw lower-left preview
  bmpPreview := TBGRABitmap.Create(panel1.left,panel1.height,BGRAWhite);
  x := (bmpPreview.Width-bmp.width) div 2;
  y := (bmpPreview.Height-bmp.Height) div 2;
  if x < 1 then x := 1;
  if y < 1 then y := 1;
  bmpPreview.Rectangle(x-1,y-1,x+bmp.width+1,y+bmp.height+1,BGRA(255,0,0),dmSet);
  bmpPreview.PutImage(x,y,bmp,dmSet);
  bmpPreview.Draw(Canvas,0,panel1.top);
  bmpPreview.Free;

  //zoom
  BGRAReplace(bmp,bmp.Resample(tx*pixSize,ty*pixSize,rmSimpleStretch));

  bmp.DrawPolygonAntialias(pts,BGRA(0,128,128,192),1);
  for i := 0 to 2 do
    NicePoint(bmp,pts[i].x,pts[i].y);
  BGRAReplace(bmp,bmp.GetPart(rect(0,0,zoomTx,zoomTy)));
  bmp.Draw(Canvas,0,0);

  bmp.free;
end;

procedure TForm1.SpinEdit_GammaChange(Sender: TObject);
begin
  BGRASetGamma(SpinEdit_Gamma.Value);
  Invalidate;
end;

procedure TForm1.TrackBar_PixelSizeChange(Sender: TObject);
begin
  Label_PixelSizeValue.Caption := '= ' + IntToStr(TrackBar_PixelSize.Position);
  Invalidate;
end;

procedure TForm1.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  //
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  pts[0] := PointF(57,100);
  pts[1] := PointF(369,170);
  pts[2] := PointF(143,310);
  Label_PixelSizeValue.Caption := '= ' + IntToStr(TrackBar_PixelSize.Position);
  MovingPointIndex := -1;
  SpinEdit_Gamma.Value := BGRAGetGamma;
end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var maxDist,dist: single;
    mousePos,vect: TPointF;
    i: Integer;
begin
  if Button <> mbLeft then exit;

  //select point to move
  MovingPointIndex := -1;
  maxDist := 10;
  mousePos := PointF(X,Y);
  MovingOrigin := mousePos;

  for i := 0 to high(pts) do
  begin
    vect := pts[i] - mousePos;
    dist := sqrt(vect*vect);
    if dist < maxDist then
    begin
      maxDist := dist;
      MovingPointIndex := i;
    end;
  end;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  mousePos: TPointF;
  i: Integer;
begin
  if ssLeft in Shift then
  begin
    mousePos := PointF(X,Y);
    if MovingPointIndex <> -1 then
      pts[MovingPointIndex] += mousePos-MovingOrigin else
    begin
      for i := 0 to high(pts) do
        pts[i] += mousePos-MovingOrigin;
    end;
    Invalidate;
    MovingOrigin := mousePos;
  end;
end;

procedure TForm1.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then MovingPointIndex := -1;
end;

end.

