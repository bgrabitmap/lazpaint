unit utest17;

{$mode objfpc}{$H+}

interface

uses
  Graphics, Classes, SysUtils, utest, BGRABitmap, BGRABitmapTypes;

type

  { TTest17 }

  TTest17 = class(TTest)
    time: double;
    dashstyle: TPenStyle;
    constructor Create;
    procedure OnPaint(Canvas: TCanvas; Left,Top,Width,Height: Integer); override;
    procedure OnTimer(Width,Height: Integer; ElapsedSec: Double); override;
  end;

implementation

{ TTest17 }

constructor TTest17.Create;
begin
  Name := 'Line primitives';
  dashstyle := psSolid;
end;

procedure TTest17.OnPaint(Canvas: TCanvas; Left,Top,Width, Height: Integer);
var
  image: TBGRABitmap;
  c : TBGRAPixel;
  angleofs: single;

  procedure DrawLines(x,y: single; title: string);
  var
    penwidth: single;
    i: Integer;
    ca,sa: single;
  begin
    image.textout(round(x),round(y),title,c);
    y += image.TextSize(title).cy+10;
    for i := 1 to 2 do
    begin
      ca := cos(sin(angleofs)/2);
      sa := sin(sin(angleofs)/2);
      penwidth := i*8+1;
      image.PenStyle := dashstyle;
      image.DrawPolyLineAntialias([PointF(x+40,y),PointF(x,y),PointF(x+sa*6*penwidth,y+ca*6*penwidth)],ColorToBGRA(ColorToRGB(clHighlight)),penwidth);
      if penwidth > 5 then
      begin
        image.penStyle := psSolid;
        image.DrawPolyLineAntialias([PointF(x+40,y),PointF(x,y),PointF(x+sa*6*penwidth,y+ca*6*penwidth)],ColorToBGRA(ColorToRGB(clHighlightText)),1);
      end;
      y += penwidth*2+6*penwidth+5;
      angleofs += Pi*0.6;
    end;
  end;

begin
    angleofs := time;
    image := TBGRABitmap.Create(Width,Height,ColorToBGRA(ColorToRGB(clBtnFace)));
    c := ColorToBGRA(ColorToRGB(clWindowText));

    image.LineCap := pecFlat;
    DrawLines(30,20,'Flat cap');
    image.LineCap := pecSquare;
    DrawLines(115,Height-200,'Square');
    image.LineCap := pecRound;
    DrawLines(190,20,'Round cap');

    image.LineCap := pecFlat;
    image.JoinStyle := pjsBevel;
    DrawLines(330,20,'Bevel join');
    image.JoinStyle := pjsMiter;
    DrawLines(430,Height-200,'Miter join');
    image.JoinStyle := pjsRound;
    DrawLines(530,20,'Round join');

    image.Draw(Canvas,Left,Top,True);
    image.free;
end;

procedure TTest17.OnTimer(Width, Height: Integer;
  ElapsedSec: Double);
var prev5: integer;
begin
  prev5 := trunc(time/5);
  time := time+0.1;
  if prev5 <> trunc(time/5) then
  begin
    if dashstyle = psDashDotDot then
      dashstyle := psSolid else
       dashstyle := succ(dashstyle);
  end;
end;

end.

