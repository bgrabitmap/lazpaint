unit utest8;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, BGRABitmap, BGRABitmapTypes, BGRATransform, utest;

type
  { TTest8 }

  TTest8 = class(TTest)
  private
    backgroundTile: TBGRABitmap;
    pacman,virtualScreen: TBGRABitmap;
    twirled: TBGRABitmap;
    alphaFactor: byte;
    time: single;
    procedure DrawPacman(x,y: integer; palphaFactor: byte);
  public
    constructor Create;
    destructor Destroy; override;
    procedure OnPaint(Canvas: TCanvas; Left,Top,Width,Height: Integer); override;
    procedure OnTimer(Width,Height: Integer; ElapsedSec: Double); override;
  end;

implementation

uses Math;

{ TTest8 }

procedure TTest8.DrawPacman(x, y: integer; palphaFactor: byte);
begin
   virtualScreen.PutImage(x-Pacman.width div 2,y-Pacman.height div 2,Pacman,dmDrawWithTransparency,palphaFactor);
end;

constructor TTest8.Create;
begin
  inherited Create;
  Name := 'Global opacity applied to pacman, background scrolling and twirl';
  virtualScreen := nil;
  twirled := nil;
  pacman := TBGRABitmap.Create('..'+pathdelim+'img'+pathdelim+'pac_d1.bmp');
  pacman.ReplaceColor(pacman.GetPixel(0,0),BGRAPixelTransparent);
  time := Pi/4;

  backgroundTile := TBGRABitmap.Create('..'+pathdelim+'img'+pathdelim+'diamondback.png');
end;

destructor TTest8.Destroy;
begin
  virtualScreen.Free;
  twirled.free;
  pacman.free;
  backgroundTile.free;
  inherited Destroy;
end;

procedure TTest8.OnPaint(Canvas: TCanvas; Left,Top,Width, Height: Integer);
var twirl: TBGRATwirlScanner;
begin
  if time=0 then exit;

  if (virtualscreen <> nil) and ((virtualscreen.width <> width) or (virtualscreen.Height <> height)) then
  begin
    FreeAndNil(virtualScreen);
    FreeAndNil(twirled);
  end;

  if virtualscreen = nil then
  begin
    virtualscreen := TBGRABitmap.Create(Width,Height);
    twirled := TBGRABitmap.Create(Width,Height);
  end;

  virtualscreen.Fill(backgroundTile);
  DrawPacman(virtualScreen.Width div 4,virtualScreen.Height div 2, alphaFactor);
  DrawPacman(3*virtualScreen.Width div 4,virtualScreen.Height div 2, 255-alphaFactor);

  twirl := TBGRATwirlScanner.Create(virtualscreen,point(width div 2, height div 2),min(Width,Height)/3,0.3,2);
  twirled.Fill(virtualscreen);
  twirled.FillRect(floor(twirl.Center.X-twirl.Radius),floor(twirl.Center.Y-twirl.Radius),
    ceil(twirl.Center.X+twirl.Radius),ceil(twirl.Center.Y+twirl.Radius),twirl,dmSet);
  twirled.Draw(Canvas,Left,Top,True);
  twirl.Free;
end;

procedure TTest8.OnTimer(Width, Height: Integer; ElapsedSec: Double);
begin
  time := time+ElapsedSec;
  alphaFactor := round((sin(time)+1)/2*255);
  backgroundTile.ScanOffset := point(round((sin(time*0.3)+1)/2*1023) mod backgroundTile.Width,
     round((sin(time*1.7)+1)/2*63) mod backgroundTile.Height);
end;

end.

