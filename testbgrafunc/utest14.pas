unit utest14;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, BGRABitmap, BGRABitmapTypes, utest;

type
  { TTest14 }

  TTest14 = class(TTest)
  protected
    virtualScreen: TBGRABitmap;
    time: single;
    lightPos1,lightPos2: TPointF;
  public
    constructor Create;
    destructor Destroy; override;
    procedure OnPaint(Canvas: TCanvas; Left,Top,Width,Height: Integer); override;
    procedure OnTimer(Width,Height: Integer; ElapsedSec: Double); override;
  end;

implementation

{ TTest14 }

constructor TTest14.Create;
begin
  inherited Create;
  Name := 'Layer blending: adding red and green gives yellow';
  virtualScreen := nil;
  time := 0;
end;

destructor TTest14.Destroy;
begin
  virtualScreen.Free;
  inherited Destroy;
end;

procedure TTest14.OnPaint(Canvas: TCanvas; Left,Top,Width, Height: Integer);
var layer: TBGRABitmap; x,y: single; radius: integer;
begin
  if (virtualscreen <> nil) and ((virtualscreen.width <> width) or (virtualscreen.Height <> height)) then
    FreeAndNil(virtualScreen);

  if virtualscreen = nil then
    virtualscreen := TBGRABitmap.Create(Width,Height);

  if time = 0 then exit;

  radius := (width+height) div 8;
  virtualScreen.Fill(BGRABlack);
  x := lightPos1.X*Width;
  y := lightPos1.Y*Height;
  virtualScreen.FillEllipseAntialias(x,y,radius,radius,BGRA(255,0,0,255));
  layer := TBGRABitmap.Create(Width,Height,BGRABlack);
  x := lightPos2.X*Width;
  y := lightPos2.Y*Height;
  layer.FillEllipseAntialias(x,y,radius,radius,BGRA(0,255,0,255));
  virtualScreen.BlendImage(0,0,layer,boAdditive);
  layer.Free;

  virtualscreen.Draw(Canvas,Left,Top,True);
end;

procedure TTest14.OnTimer(Width, Height: Integer; ElapsedSec: Double);
begin
  time := time+ElapsedSec*0.5;
  lightPos1 := pointF((sin(time)+1)/4+0.2,(cos(time*0.3)+1)/4+0.3);
  lightPos2 := pointF((sin(time*0.7+1)+1)/4+0.4,(cos(time*0.5+2)+1)/4+0.3);
end;

end.

