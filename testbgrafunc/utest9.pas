unit utest9;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, utestback, Graphics, BGRABitmap, BGRABitmapTypes;

type
  { TTest9 }

  TTest9 = class(TTestBack)
  protected
    pacman,virtualScreen,masked: TBGRABitmap;
    time: single;
    lightPos1,lightPos2: TPointF;
  public
    constructor Create;
    destructor Destroy; override;
    procedure OnPaint(Canvas: TCanvas; Left,Top,Width,Height: Integer); override;
    procedure OnTimer(Width,Height: Integer; ElapsedSec: Double); override;
  end;

implementation

uses math;

{ TTest9 }

constructor TTest9.Create;
begin
  inherited Create;
  Name := 'Simple mask, ellipse and radial gradient';
  virtualScreen := nil;
  masked := nil;
  pacman := TBGRABitmap.Create('..'+pathdelim+'img'+pathdelim+'pac_d1.bmp');
  pacman.ReplaceColor(pacman.GetPixel(0,0),BGRAPixelTransparent);
  time := 0;
end;

destructor TTest9.Destroy;
begin
  virtualScreen.Free;
  masked.Free;
  pacman.free;
  inherited Destroy;
end;

procedure TTest9.OnPaint(Canvas: TCanvas; Left,Top,Width, Height: Integer);
var mask: TBGRABitmap; x,y: single;
begin
  UpdateBackground(Width,Height);
  if time = 0 then exit;

  if (virtualscreen <> nil) and ((virtualscreen.width <> width) or (virtualscreen.Height <> height)) then
  begin
    FreeAndNil(virtualScreen);
    FreeAndNil(masked);
  end;

  if virtualscreen = nil then
    virtualscreen := TBGRABitmap.Create(Width,Height);

  if masked = nil then
  begin
    masked := backgroundImg.Duplicate as TBGRABitmap;
    masked.PutImage(virtualScreen.Width div 2,virtualScreen.Height div 2, pacman, dmDrawWithTransparency);
  end;

  virtualscreen.PutImage(0,0,backgroundImg,dmSet);
  virtualScreen.FillRect(0,0,virtualScreen.Width,virtualScreen.Height,BGRA(0,0,0,192),dmDrawWithTransparency);

  mask := TBGRABitmap.Create(Width,Height,BGRABlack);
  mask.FillEllipseAntialias(lightPos1.X*Width,lightPos1.Y*Height,40,40,BGRAWhite);
  x := lightPos2.X*Width;
  y := lightPos2.Y*Height;
  mask.GradientFill(floor(x-60),floor(y-60),ceil(x+60),ceil(y+60),BGRAWhite,BGRAPixelTransparent,gtRadial,pointf(x,y),pointf(x+60,y),dmDrawWithTransparency,True,False);
  virtualScreen.FillMask(0,0,mask,masked);
  mask.free;

  virtualscreen.Draw(Canvas,Left,Top,True);
end;

procedure TTest9.OnTimer(Width, Height: Integer; ElapsedSec: Double);
begin
  time := time+ElapsedSec*0.5;
  lightPos1 := pointF((sin(time)+1)/4+0.2,(cos(time*0.3)+1)/4+0.3);
  lightPos2 := pointF((sin(time*0.7+1)+1)/4+0.4,(cos(time*0.5+2)+1)/4+0.3);
end;

end.

