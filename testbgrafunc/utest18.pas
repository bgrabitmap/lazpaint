unit utest18;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, utest, Graphics, BGRABitmap, BGRABitmapTypes;

const
  nbPoints = 3;

type
  { TTest18 }

  TTest18 = class(TTest)
  protected
    virtualScreen,texture,backgroundTile,background: TBGRABitmap;
    pts: array of TPointF;
    dirs: array of TPointF;
    angle: single;

  public
    constructor Create;
    destructor Destroy; override;
    procedure OnPaint(Canvas: TCanvas; Left,Top,Width,Height: Integer); override;
    procedure OnTimer(Width,Height: Integer; ElapsedSec: Double); override;
  end;

implementation

{ TTest18 }

constructor TTest18.Create;
begin
  inherited Create;
  Name := 'Affine image transformation';
  texture := TBGRABitmap.Create('..'+pathdelim+'img'+pathdelim+'pac_d1.bmp');
  texture.ReplaceColor(texture.GetPixel(0,0),BGRAPixelTransparent);
  backgroundTile := TBGRABitmap.Create('..'+pathdelim+'img'+pathdelim+'diamondback.png');
  randomize;
  virtualScreen := nil;
  background := nil;
end;

destructor TTest18.Destroy;
begin
  texture.free;
  virtualScreen.Free;
  backgroundTile.Free;
  background.Free;
  inherited Destroy;
end;

procedure TTest18.OnPaint(Canvas: TCanvas; Left,Top,Width, Height: Integer);
begin
  if pts = nil then exit;

  if (virtualscreen <> nil) and ((virtualscreen.width <> width) or (virtualscreen.Height <> height)) then
  begin
    FreeAndNil(virtualScreen);
    FreeAndNil(background);
  end;

  if virtualscreen = nil then
  begin
    virtualscreen := TBGRABitmap.Create(Width,Height);
    background := backgroundTile.GetPart(rect(0,0,Width,Height)) as TBGRABitmap;
  end;

  virtualScreen.PutImage(0,0,background,dmSet);
  virtualScreen.PutImageAffine(pts[1],pts[0],pts[2],texture);

  virtualScreen.DrawPolyLineAntialias(pts,BGRAWhite,3);
  virtualScreen.DrawPolyLineAntialias(pts,BGRABlack,1);

  virtualScreen.PutImageAngle(virtualScreen.Width div 2,virtualScreen.Height div 2,texture,angle,texture.Width/2,texture.Height/2);

  virtualScreen.draw(Canvas,Left,Top);
end;

procedure TTest18.OnTimer(Width, Height: Integer; ElapsedSec: Double);
var i: integer;
    moveFactor: single;
begin
  angle += ElapsedSec*20;
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

