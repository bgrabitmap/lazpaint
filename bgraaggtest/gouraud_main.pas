unit gouraud_main; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, BGRABitmap, BGRABitmapTypes, LMessages;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    Panel1: TPanel;
    TrackBar1: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    { private declarations }
    procedure FormPaint(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
  public
    { public declarations }
    MovingPointIndex: Integer;
    MovingOrigin: TPointF;
    pts: array[0..2] of TPointF;
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

uses BGRAPolygon;

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
    c: TPointF;
    multi: TBGRAMultishapeFiller;
    opacity: byte;
begin
  tx := ClientWidth;
  ty := Panel1.Top;

  bmp := TBGRABitmap.Create(tx,ty,BGRAWhite);

  opacity := TrackBar1.Position;
  c := (pts[0]+pts[1]+pts[2])*(1/3);
  multi := TBGRAMultishapeFiller.Create;
  multi.AddQuadLinearColor(pts[0],c,pts[2],pts[2]+(pts[0]-c),
    BGRA(0,0,255,opacity),BGRA(255,255,255,opacity),BGRA(255,0,0,opacity),BGRA(0,0,0,opacity));
  multi.AddQuadLinearColor(pts[0],c,pts[1],pts[1]+(pts[0]-c),
    BGRA(0,0,255,opacity),BGRA(255,255,255,opacity),BGRA(0,255,0,opacity),BGRA(0,0,0,opacity));
  multi.AddQuadLinearColor(pts[2],c,pts[1],pts[1]+(pts[2]-c),
    BGRA(255,0,0,opacity),BGRA(255,255,255,opacity),BGRA(0,255,0,opacity),BGRA(0,0,0,opacity));
  multi.Draw(bmp);
  multi.free;

  for i := 0 to 2 do
    NicePoint(bmp,pts[i].x,pts[i].y);
  bmp.Draw(Canvas,0,0);

  bmp.free;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  Invalidate;
end;

procedure TForm1.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  //
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  pts[0] := PointF(150,10);
  pts[1] := PointF(370,140);
  pts[2] := PointF(50,260);
  MovingPointIndex := -1;
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

