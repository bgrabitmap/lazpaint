unit utest26;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, utest, Graphics, BGRABitmap, BGRABitmapTypes;

const
  nbPoints = 9;

type
  { TTest26 }

  TTest26 = class(TTest)
  protected
    virtualScreen,backgroundTile: TBGRABitmap;
    pts: array of TPointF;
    dirs: array of TPointF;

  public
    constructor Create;
    destructor Destroy; override;
    procedure OnPaint(Canvas: TCanvas; Left,Top,Width,Height: Integer); override;
    procedure OnTimer(Width,Height: Integer; ElapsedSec: Double); override;
  end;

implementation

{ TTest26 }

constructor TTest26.Create;
begin
  inherited Create;
  Name := 'Gradient shapes (antialiased or not)';
  backgroundTile := TBGRABitmap.Create('..'+pathdelim+'img'+pathdelim+'diamondback.png');
  randomize;
  virtualScreen := nil;
end;

destructor TTest26.Destroy;
begin
  virtualScreen.Free;
  backgroundTile.Free;
  inherited Destroy;
end;

procedure TTest26.OnPaint(Canvas: TCanvas; Left,Top,Width, Height: Integer);
begin
  if pts = nil then exit;

  if (virtualscreen <> nil) and ((virtualscreen.width <> width) or (virtualscreen.Height <> height)) then
    FreeAndNil(virtualScreen);

  if virtualscreen = nil then
    virtualscreen := TBGRABitmap.Create(Width,Height);

  virtualScreen.Fill(backgroundTile);

  virtualScreen.FillQuadLinearColor(pts[3],pts[4],pts[5],pts[6],BGRA(0,192,0),BGRA(0,128,255),BGRA(255,128,0),BGRA(255,255,255));
  virtualScreen.FillTriangleLinearColorAntialias(pts[0],pts[1],pts[2],BGRA(255,0,0),BGRA(255,255,0),BGRA(255,0,255));
  virtualScreen.FillEllipseLinearColorAntialias(pts[7].x,pts[7].y,pts[8].x/4,pts[8].y/4,BGRABlack,BGRAWhite);

  virtualScreen.draw(Canvas,Left,Top);
end;

procedure TTest26.OnTimer(Width, Height: Integer; ElapsedSec: Double);
var i: integer;
    moveFactor: single;
begin
  if pts = nil then
  begin
    setlength(pts,nbPoints);
    setlength(dirs,nbPoints);
    for i := 0 to NbPoints-1 do
    begin
      pts[i] := pointf(random(Width),random(Height));
      dirs[i] := pointf((random(Width)-width/2)/20,(random(Height)-height/2)/20);
    end;
  end;
  moveFactor := ElapsedSec*20;
  for i := 0 to NbPoints-1 do
  begin
    pts[i].x := pts[i].x+dirs[i].x*moveFactor;
    if pts[i].x < 0 then
    begin
      pts[i].x := 0;
      dirs[i].x := abs(dirs[i].x);
    end;
    if pts[i].x > width-1 then
    begin
      pts[i].x := width-1;
      dirs[i].x := -abs(dirs[i].x);
    end;
    pts[i].y := pts[i].y+dirs[i].y*moveFactor;
    if pts[i].y < 0 then
    begin
      pts[i].y := 0;
      dirs[i].y := abs(dirs[i].y);
    end;
    if pts[i].y > height-1 then
    begin
      pts[i].y := height-1;
      dirs[i].y := -abs(dirs[i].y);
    end;
  end;
end;

end.

