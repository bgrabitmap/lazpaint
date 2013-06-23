unit bspline_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  BGRABitmap, BGRABitmapTypes, LMessages, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    CheckBox_Closed: TCheckBox;
    Panel1: TPanel;
    Radio_Crossing: TRadioButton;
    Radio_Inside: TRadioButton;
    Radio_Outside: TRadioButton;
    Radio_Rounded: TRadioButton;
    Radio_VertexToSide: TRadioButton;
    procedure CheckBox_Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormResize(Sender: TObject);
    procedure Radio_Change(Sender: TObject);
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
  public
    { public declarations }
    pts: array of TPointF;
    MovingPointIndex: Integer;
    MovingOrigin: TPointF;
    PreviousSize: TPointF;
  end;

var
  Form1: TForm1; 

implementation

uses BGRAResample;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  setlength(pts,6);
  pts[1] := PointF(clientwidth/2,clientheight/2);
  pts[0] := pts[1] + pointF(0,75);
  pts[2] := PointF(100,100);
  pts[3] := pointF(clientwidth-100,100);
  pts[4] := pointF(clientwidth-100,clientheight-100);
  pts[5] := pointF(100,clientheight-100);
  MovingPointIndex := -1;
end;

procedure TForm1.CheckBox_Change(Sender: TObject);
begin
  Invalidate;
end;

procedure TForm1.FormPaint(Sender: TObject);
var bmp: TBGRABitmap;
  i: Integer;
  style: TSplineStyle;
begin
  PreviousSize := PointF(ClientWidth,clientheight);
  bmp := TBGRABitmap.Create(clientwidth,panel1.top,BGRAWhite);

  bmp.DrawPolygonAntialias(pts,BGRA(102,148,179),1);
  for i := 0 to high(pts) do
    bmp.FillEllipseAntialias(pts[i].x,pts[i].y,5,5,BGRA(102,148,179));

  if Radio_Inside.Checked then style := ssInside else
  if Radio_Crossing.Checked then style := ssCrossing else
  if Radio_Outside.checked then style := ssOutside else
  if Radio_Rounded.Checked then style := ssRoundOutside else
    style := ssVertexToSide;

  if CheckBox_Closed.Checked then
    bmp.DrawPolygonAntialias(bmp.ComputeClosedSpline(pts, style),BGRABlack,2) else
    bmp.DrawPolyLineAntialias(bmp.ComputeOpenedSpline(pts, style),BGRABlack,2);

  bmp.draw(Canvas,0,0);

  bmp.Free;
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

procedure TForm1.FormResize(Sender: TObject);
var factor: TPointF;
  i: Integer;
begin
  if (clientWidth > 0) and (clientheight > 0) and (previousSize.X > 0) and (previousSize.Y > 0) then
  begin
    factor.X := clientWidth/PreviousSize.X;
    factor.Y := clientheight/PreviousSize.Y;
    for i := 0 to high(pts) do
    begin
      pts[i].x *= factor.X;
      pts[i].y *= factor.y;
    end;
  end;
end;

procedure TForm1.Radio_Change(Sender: TObject);
begin
  Invalidate;
end;

procedure TForm1.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  //
end;

end.

