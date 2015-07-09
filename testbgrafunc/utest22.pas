unit utest22;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes, Graphics, utest;

type
  { TTest22 }

  TTest22 = class(TTest)
  protected
    virtualScreen,pac: TBGRABitmap;
  public
    constructor Create;
    destructor Destroy; override;
    procedure OnPaint(Canvas: TCanvas; Left,Top,Width,Height: Integer); override;
    procedure OnTimer(Width, Height: Integer; ElapsedSec: Double); override;
  end;

implementation

{ TTest22 }

constructor TTest22.Create;
begin
  inherited Create;
  randomize;
  Name := 'Fast clipping (rectangle)';
  pac := TBGRABitmap.Create('..'+pathdelim+'img'+pathdelim+'pac_d2.bmp');
  pac.ReplaceColor(pac.GetPixel(0,0),BGRAPixelTransparent);
end;

destructor TTest22.Destroy;
begin
  pac.free;
  virtualscreen.free;
end;

procedure TTest22.OnPaint(Canvas: TCanvas; Left,Top,Width, Height: Integer);
var c: TBGRAPixel; x1,y1,x2,y2,x3,y3: integer; w: single;
begin
  if (virtualscreen <> nil) and ((virtualscreen.width <> width) or (virtualscreen.Height <> height)) then
    FreeAndNil(virtualScreen);

  if virtualscreen = nil then
    virtualscreen := TBGRABitmap.Create(Width,Height,BGRABlack);

  virtualscreen.ClipRect := rect(Width div 4,Height div 4, Width*3 div 4, Height*3 div 4);
  c := BGRA(random(256),random(256),random(256),random(128)+128);
  x1 := random(width);
  y1 := random(height);
  x2 := random(width);
  y2 := random(height);
  x3 := random(width);
  y3 := random(height);
  w := random(width+height)/500;

  case random(12) of
  0: virtualscreen.DrawHorizLine(x1,y1,x2,c);
  1: virtualscreen.SetHorizLine(x1,y1,x2,c);
  2: virtualscreen.DrawLine(x1,y1,x2,y2,c,True);
  3: virtualscreen.DrawLineAntialias(x1,y1,x2,y2,c,w);
  4: virtualscreen.EraseLineAntialias(x1,y1,x2,y2,c.alpha,w);
  5: if random(2)= 0 then
       virtualscreen.FillPoly([PointF(x1,y1),PointF(x2,y2),PointF(x3,y3)],c,dmDrawWithTransparency)
    else virtualscreen.FillPolyAntialias([PointF(x1,y1),PointF(x2,y2),PointF(x3,y3)],c);
  6: virtualscreen.FillEllipseAntialias(x1,y1,x2-x1,y2-y1,c);
  7: virtualscreen.Rectangle(x1,y1,x2-x1,y2-y1,c,dmDrawWithTransparency);
  8: virtualscreen.RoundRectAntialias(x1,y1,x2,y2,abs((x2-x1)/6),abs((y2-y1)/6),c,w);
  9: virtualscreen.FillRect(x1,y1,x2-x1,y2-y1,c,dmDrawWithTransparency);
  10: virtualscreen.TextOutAngle(x1,y1,random(3600),'Some text',c,taCenter);
  11: virtualScreen.PutImage(x1,y1,pac,dmDrawWithTransparency);
  end;

  virtualscreen.NoClip;
  virtualscreen.SetPixel(random(width),random(height),c);

  //draw virtualscreen opaque on canvas
  virtualscreen.Draw(Canvas,Left,Top,True);
end;

procedure TTest22.OnTimer(Width, Height: Integer;
  ElapsedSec: Double);
begin
  //nothing
end;

end.

