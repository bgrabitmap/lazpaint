unit bspline_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  BGRABitmap, BGRABitmapTypes, LMessages, ExtCtrls, BGRAPath;

type

  { TForm1 }

  TForm1 = class(TForm)
    CheckBox_CanJump: TCheckBox;
    CheckBox_Closed: TCheckBox;
    Panel1: TPanel;
    Radio_Bezier2: TRadioButton;
    Radio_Bezier3: TRadioButton;
    Radio_Crossing: TRadioButton;
    Radio_Inside: TRadioButton;
    Radio_Outside: TRadioButton;
    Radio_Rounded: TRadioButton;
    Radio_VertexToSide: TRadioButton;
    Timer1: TTimer;
    procedure CheckBox_Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure FormResize(Sender: TObject);
    procedure Radio_Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure WMEraseBkgnd(var {%H-}Message: TLMEraseBkgnd); message LM_ERASEBKGND;

  public
    { public declarations }
    pts: array of TPointF;
    MovingPointIndex: Integer;
    MovingOrigin: TPointF;
    PreviousSize: TPointF;
    FPath: TBGRAPath;
    FPathCursor: TBGRAPathCursor;
    FPathThumbnail: TBGRAPath;
    FPathSpeed: single;
    FPathPos: single;
    procedure PathChange;
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

uses math;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  h: Integer;
begin
  h := clientheight-Panel1.Height;
  setlength(pts,7);
  pts[1] := PointF(clientwidth/2,h/2);
  pts[0] := pts[1] + pointF(0,75);
  pts[2] := PointF(100,100);
  pts[3] := pointF(clientwidth-100,100);
  pts[4] := pointF(clientwidth-100,h-100);
  pts[5] := pointF(100,h-100);
  pts[6] := pointF(100,pts[0].y);
  MovingPointIndex := -1;
  FPathSpeed := 4;
  FPathPos := 0;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FPathCursor);
  FreeAndNil(FPath);
end;

procedure TForm1.CheckBox_Change(Sender: TObject);
begin
  PathChange;
end;

procedure TForm1.FormPaint(Sender: TObject);
var bmp: TBGRABitmap;
  i: Integer;
  style: TSplineStyle;
  nbPoints: integer;
  pt,tangent: TPointF;
  closed: boolean;
  thumbRect: TRect;
begin
  PreviousSize := PointF(ClientWidth,clientheight);
  bmp := TBGRABitmap.Create(clientwidth,panel1.top,BGRAWhite);
  closed := CheckBox_Closed.Checked;

  if Radio_Bezier2.Checked then
    nbPoints := ((length(pts)-1+integer(closed)) div 2)*2+1-integer(closed)
  else if Radio_Bezier3.Checked then
    nbPoints := ((length(pts)-1+integer(closed)) div 3)*3+1-integer(closed)
  else
    nbPoints := length(pts);

  if FPath = nil then
  begin
    FPath := TBGRAPath.Create;
    if Radio_Bezier2.Checked then
    begin
      FPath.moveTo(pts[0]);
      for i := 1 to (nbPoints-1+integer(closed)) div 2 do
        FPath.quadraticCurveTo(pts[2*(i-1)+1],pts[(2*(i-1)+2) mod nbPoints]);
      if closed then FPath.closePath;
    end
    else
    if Radio_Bezier3.Checked then
    begin
      FPath.moveTo(pts[0]);
      for i := 1 to (nbPoints-1+integer(closed)) div 3 do
        FPath.bezierCurveTo(pts[3*(i-1)+1],pts[3*(i-1)+2],pts[(3*(i-1)+3) mod nbPoints]);
      if closed then FPath.closePath;
    end
    else
    begin
      if Radio_Inside.Checked then style := ssInsideWithEnds else
      if Radio_Crossing.Checked then style := ssCrossingWithEnds else
      if Radio_Outside.checked then style := ssOutside else
      if Radio_Rounded.Checked then style := ssRoundOutside else
        style := ssVertexToSide;

      if closed then
        FPath.closedSpline(slice(pts,nbPoints), style)
      else
        FPath.openedSpline(slice(pts,nbPoints), style);
    end;
  end;

  if Assigned(FPath) then
  begin
    FPath.fill(bmp, BGRA(250,250,230));

    if closed then
      bmp.DrawPolygonAntialias(slice(pts,nbPoints),BGRA(102,148,179),1)
    else
      bmp.DrawPolyLineAntialias(slice(pts,nbPoints),BGRA(102,148,179),1);
    for i := 0 to nbPoints-1 do
      bmp.FillEllipseAntialias(pts[i].x,pts[i].y,5,5,BGRA(102,148,179));

    FPath.stroke(bmp, BGRABlack, 2);

    if FPathCursor = nil then
    begin
      FPathCursor := FPath.CreateCursor;
      FPathCursor.LoopPath:= true;
      FPathCursor.Position := FPathPos*FPathCursor.PathLength;
    end;

    thumbRect := recT(bmp.Width-128,0,bmp.Width,128);
    if FPathThumbnail = nil then
    begin
      FPathThumbnail := TBGRAPath.Create;
      FPath.FitInto(FPathThumbnail, RectF(0,0,thumbRect.Right-thumbRect.Left-1,thumbRect.Bottom-thumbRect.Top-1));
    end;

    bmp.FillRect(thumbRect, BGRA(102,148,179,128), dmDrawWithTransparency);
    FPathThumbnail.stroke(bmp, thumbRect.Left, thumbRect.Top, BGRABlack, 1);

    with FPathCursor.Bounds do
      bmp.RectangleAntialias(Left,Top,Right,Bottom,CSSFireBrick,1.5);

    //bmp.TextOut(0,bmp.FontFullHeight, IntToStr(length(comp_pts))+' points', BGRABlack);
    //bmp.DrawPolyLineAntialiasAutocycle(FPath.ToPoints(0.1),BGRABlack,1);

    bmp.ArrowEndAsClassic;
    pt := FPathCursor.CurrentCoordinate;
    tangent := FPathCursor.CurrentTangent;
    bmp.DrawLineAntialias(pt.x,pt.y,pt.x+tangent.x*40*Sign(FPathSpeed),pt.y+tangent.y*40*Sign(FPathSpeed),CSSFireBrick,3);
    bmp.DrawLineAntialias(pt.x,pt.y,pt.x-tangent.y*40*Sign(FPathSpeed),pt.y+tangent.x*40*Sign(FPathSpeed),CSSFireBrick,3);
    bmp.ArrowEndAsNone;

    bmp.TextOut(0,0, 'Length: ' + IntToStr(round(FPathCursor.PathLength)), BGRABlack);
    if FPathCursor.PathLength > 0 then
      bmp.TextOut(0,bmp.FontFullHeight, IntToStr(round(FPathCursor.Position / FPathCursor.PathLength*100))+'%', BGRABlack);
  end;

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
    PathChange;
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
  i,ph: Integer;
begin
  ph := Panel1.Height;
  if (clientWidth > 0) and (clientheight-ph > 0) and
  (previousSize.X > 0) and (previousSize.Y-ph > 0) then
  begin
    factor.X := clientWidth/PreviousSize.X;
    factor.Y := (clientheight-ph)/(PreviousSize.Y-ph);
    for i := 0 to high(pts) do
    begin
      pts[i].x *= factor.X;
      pts[i].y *= factor.y;
    end;
    PreviousSize := PointF(ClientWidth,clientheight);
    PathChange;
  end;
end;

procedure TForm1.Radio_Change(Sender: TObject);
begin
  PathChange;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if Assigned(FPathCursor) then
  begin
    if FPathCursor.MoveForward(FPathSpeed, CheckBox_CanJump.Checked) <> FPathSpeed then
      FPathSpeed:= -FPathSpeed;
    if FPathCursor.PathLength > 0 then
      FPathPos := FPathCursor.Position/FPathCursor.PathLength;
    invalidate;
  end;
end;

procedure TForm1.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  //
end;

procedure TForm1.PathChange;
begin
  FreeAndNil(FPathCursor);
  FreeAndNil(FPathThumbnail);
  FreeAndNil(FPath);
  Invalidate;
end;

end.

