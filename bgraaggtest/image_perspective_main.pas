unit image_perspective_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, BGRABitmap, BGRABitmapTypes, LMessages, EpikTimer;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Radio_InterpBox: TRadioButton;
    Radio_InterpLinear: TRadioButton;
    Radio_InterpHalfCosine: TRadioButton;
    Radio_InterpCosine: TRadioButton;
    Radio_Perspective: TRadioButton;
    Radio_LinearAntialias: TRadioButton;
    Radio_Linear: TRadioButton;
    Radio_AffineAntialias: TRadioButton;
    Radio_Affine: TRadioButton;
    Radio_PerspectiveAntialias: TRadioButton;
    TrackBar1: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    { private declarations }
    procedure FormPaint(Sender: TObject);
    procedure RadioButtonChange(Sender: TObject);
    procedure WMEraseBkgnd(var {%H-}Message: TLMEraseBkgnd); message LM_ERASEBKGND;
  public
    { public declarations }
    MovingPointIndex: Integer;
    MovingOrigin: TPointF;
    pts: array[0..3] of TPointF;
    image: TBGRABitmap;
    stopwatch: TEpikTimer;
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
var bmp: TBGRABitmap;
    tx,ty,i: Integer;
    texPos: array of TPointF;
begin
  tx := ClientWidth;
  ty := clientHeight;

  if Radio_InterpBox.Checked then
    image.ScanInterpolationFilter := rfBox else
  if Radio_InterpLinear.Checked then
    image.ScanInterpolationFilter := rfLinear else
  if Radio_InterpHalfCosine.Checked then
    image.ScanInterpolationFilter := rfHalfCosine else
  if Radio_InterpCosine.Checked then
    image.ScanInterpolationFilter := rfCosine;

  bmp := TBGRABitmap.Create(tx,ty,BGRAWhite);

  stopwatch.clear;
  stopwatch.start;

  texPos := PointsF([PointF(0,0),PointF(image.width-1,0),
              PointF(image.width-1,image.Height-1),PointF(0,image.Height-1)]);
  if Radio_Perspective.Checked or Radio_PerspectiveAntialias.Checked then
  begin
    if Radio_PerspectiveAntialias.Checked then
      bmp.FillQuadPerspectiveMappingAntialias(pts[0],pts[1],pts[2],pts[3], image,
                texPos[0],texPos[1],texPos[2],texPos[3])
    else
      bmp.FillQuadPerspectiveMapping(pts[0],pts[1],pts[2],pts[3], image,
              texPos[0],texPos[1],texPos[2],texPos[3]);
  end else
  if Radio_LinearAntialias.Checked then
  begin
    bmp.FillQuadLinearMappingAntialias(pts[0],pts[1],pts[2],pts[3], image,
        texPos[0],texPos[1],texPos[2],texPos[3]);
  end
  else if Radio_Linear.Checked then
  begin
    bmp.FillQuadLinearMapping(pts[0],pts[1],pts[2],pts[3], image,
        texPos[0],texPos[1],texPos[2],texPos[3]);
  end
  else if Radio_Affine.Checked then
  begin
    pts[2] := pts[1]+(pts[3]-pts[0]);
    bmp.FillQuadAffineMapping(pts[0],pts[1],pts[3],image);
  end else if Radio_AffineAntialias.checked then
  begin
    pts[2] := pts[1]+(pts[3]-pts[0]);
    bmp.FillQuadAffineMappingAntialias(pts[0],pts[1],pts[3],image);
  end;

  stopwatch.stop;
  //bmp.DrawPolygonAntialias(pts,BGRA(0,0,0,64),1);
  bmp.textOut(0,0,inttostr(round(stopwatch.Elapsed*1000))+' ms',BGRABlack);

  for i := 0 to 3 do
    NicePoint(bmp,pts[i].x,pts[i].y);
  bmp.Draw(Canvas,0,0);

  bmp.free;
end;

procedure TForm1.RadioButtonChange(Sender: TObject);
begin
  invalidate;
end;

procedure TForm1.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  //
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  pts[0] := PointF(50,50);
  pts[1] := PointF(clientwidth-150,50);
  pts[2] := PointF(clientwidth-150,clientheight-150);
  pts[3] := PointF(120,clientheight-200);
  MovingPointIndex := -1;
  image := TBGRABitmap.Create('spheres.png');
  stopwatch := TEpikTimer.Create(Self);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  image.free;
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

