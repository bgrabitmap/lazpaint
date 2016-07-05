unit alpha_gradient_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, BGRABitmapTypes, BGRABitmap, LMessages;

type

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    ellipses: array of record
                x,y,w,h: integer;
                c: TBGRAPixel;
              end;

    pts: array[0..2] of TPointF;
    MovingPointIndex: integer;
    MovingOrigin: TPointF;
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;

  end;

var
  Form1: TForm1; 

implementation

uses BGRAGradientScanner, BGRATransform;

procedure NicePoint(bmp: TBGRABitmap; x, y: single);
begin
    bmp.EllipseAntialias(x,y,4,4,BGRA(0,0,0,192),1);
    bmp.EllipseAntialias(x,y,3,3,BGRA(255,255,255,192),1);
    bmp.EllipseAntialias(x,y,2,2,BGRA(0,0,0,192),1);
end;

{$R *.lfm}

type

  { TMultiplyGradient }

  TMultiplyGradient = class(TBGRACustomScanner)
    function ScanAt(X, Y: Single): TBGRAPixel; override;
  end;

{ TMultiplyGradient }

function TMultiplyGradient.ScanAt(X, Y: Single): TBGRAPixel;
var fvalue: single;
    value: integer;
begin
  fvalue := abs(x*y*255);
  if fvalue > 255 then
    value := 255
  else
    value := round(fvalue);

  result := BGRA(value,value,value,255);
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  setlength(ellipses,100);
  for i := 0 to high(ellipses) do
    with ellipses[i] do
    begin
      x := random(65536);
      y := random(65536);
      w := random(100)+5;
      h := random(100)+5;
      c := BGRA(random(256),random(256),random(256),random(128)+64);
    end;

  pts[0] := PointF(250,200);
  pts[1] := PointF(300,150);
  pts[2] := PointF(300,250);
  MovingPointIndex := -1;
end;

procedure TForm1.FormPaint(Sender: TObject);
const ellipseRadius = 160;
var bmp: TBGRABitmap;
    tx,ty: integer;
    i: Integer;
    ellipseLayer: TBGRABitmap;
    gradient: TBGRAGradientScanner;
    multigrad: TBGRAMultiGradient;
    affine: TBGRAAffineScannerTransform;
    multiply: TMultiplyGradient;
    mask: TBGRABitmap;
    ellipseLayerOffset: TPointF;
begin
  tx := ClientWidth;
  ty := ClientHeight;
  if (tx=0) or (ty=0) then exit;

  //create background
  bmp := TBGRABitmap.Create(tx,ty, BGRAWhite);
  for i := 0 to high(ellipses) do
  with ellipses[i] do
    bmp.FillEllipseAntialias(x mod tx,y mod ty,w/2,h/2,c);

  //create center red-yellow ellipse
  ellipseLayerOffset := PointF((tx-(2*ellipseRadius+1))/2,(ty-(2*ellipseRadius+1))/2);
  ellipseLayer := TBGRABitmap.Create(2*ellipseRadius +1 + 1,2*ellipseRadius +1 +1);
  multiGrad := TBGRAMultiGradient.Create([BGRA(0,64,0),BGRA(160,160,0),BGRA(128,0,0)],[0,1/2,1],True);
  gradient := TBGRAGradientScanner.Create(multiGrad,gtRadial,PointF(0,0),PointF(1,0));
  affine := TBGRAAffineScannerTransform.Create(gradient);
  affine.Scale(ellipseRadius*1.2,ellipseRadius*0.8);
  affine.RotateDeg(30);
  affine.Translate(ellipseRadius+frac(ellipseLayerOffset.X),ellipseRadius+frac(ellipseLayerOffset.Y));
  ellipseLayer.FillEllipseAntialias(ellipseRadius+frac(ellipseLayerOffset.X),ellipseRadius+frac(ellipseLayerOffset.Y),
         ellipseRadius,ellipseRadius,BGRA(192,128,0));
  ellipseLayer.FillEllipseAntialias(ellipseRadius+frac(ellipseLayerOffset.X),ellipseRadius+frac(ellipseLayerOffset.Y),
         ellipseRadius,ellipseRadius,affine);
  affine.Free;
  gradient.Free;
  multiGrad.Free;

  //apply multiply mask
  multiply := TMultiplyGradient.Create;
  affine := TBGRAAffineScannerTransform.Create(multiply);
  affine.Fit(pts[0],pts[1],pts[2]);
  affine.Translate(-trunc(ellipseLayerOffset.X),-trunc(ellipseLayerOffset.Y));
  mask := TBGRABitmap.Create(ellipseLayer.Width,ellipseLayer.Height);
  mask.Fill(affine);
  ellipseLayer.ApplyMask(mask);
  mask.Free;
  affine.Free;
  multiply.Free;

  bmp.PutImage(trunc(ellipseLayerOffset.X),trunc(ellipseLayerOffset.Y),ellipseLayer,dmDrawWithTransparency);
  ellipseLayer.Free;

  for i := 0 to 2 do
    NicePoint(bmp,pts[i].x,pts[i].y);

  bmp.Draw(Canvas,0,0);
  bmp.Free;
end;

procedure TForm1.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  //
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
  for i := 0 to 2 do
  begin
    vect := pts[i] - mousePos;
    dist := sqrt(vect*vect);
    if dist < maxDist then
    begin
      maxDist := dist;
      MovingPointIndex := i;
      MovingOrigin := mousePos;
    end;
  end;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  mousePos: TPointF;
begin
  if MovingPointIndex <> -1 then
  begin
    mousePos := PointF(X,Y);
    pts[MovingPointIndex] += mousePos-MovingOrigin;
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

