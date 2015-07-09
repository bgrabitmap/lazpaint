unit utest6;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, utestback, Graphics, BGRABitmap, BGRABitmapTypes;

type
  { TTest6 }

  TTest6 = class(TTestBack)
  protected
    image,virtualScreen: TBGRABitmap;
    zoomFactor: single;
    time: single;
    procedure DrawImage(x,y: integer; pzoomfactor: single; mode : TResampleMode);
  public
    constructor Create;
    destructor Destroy; override;
    procedure OnPaint(Canvas: TCanvas; Left,Top,Width,Height: Integer); override;
    procedure OnTimer(Width,Height: Integer; ElapsedSec: Double); override;
  end;

implementation

{ TTest6 }

procedure TTest6.DrawImage(x, y: integer; pzoomfactor: single; mode : TResampleMode);
var temp: TBGRABitmap;
    width,height: integer;
begin
   if pzoomfactor = 0 then exit;
   width := (round(image.width*pzoomfactor)+1) and not 1; //even size for centering
   if width <= 0 then width := 1;
   height := (round(image.height*pzoomfactor)+1) and not 1; //even size for centering
   if height <= 0 then height := 1;
   temp := image.Resample(width,height,mode) as TBGRABitmap;
   virtualScreen.PutImage(x-width div 2,y-height div 2,temp,dmDrawWithTransparency);
   temp.Free;
end;

constructor TTest6.Create;
begin
  inherited Create;
  Name := 'Simple stretch vs fine resample';
  virtualScreen := nil;
  image := TBGRABitmap.Create('..'+pathdelim+'img'+pathdelim+'resampletest.png');
  time := Pi/4;
end;

destructor TTest6.Destroy;
begin
  virtualScreen.Free;
  image.free;
  inherited Destroy;
end;

procedure TTest6.OnPaint(Canvas: TCanvas; Left,Top,Width, Height: Integer);
begin
  UpdateBackground(Width,Height);

  if (virtualscreen <> nil) and ((virtualscreen.width <> width) or (virtualscreen.Height <> height)) then
  FreeAndNil(virtualScreen);

  if virtualscreen = nil then
    virtualscreen := TBGRABitmap.Create(Width,Height);

  virtualscreen.PutImage(0,0,backgroundImg,dmSet);
  DrawImage(virtualScreen.Width div 4,virtualScreen.Height div 2, zoomFactor, rmSimpleStretch);
  image.ResampleFilter := rfHalfCosine;
  DrawImage(3*virtualScreen.Width div 4,virtualScreen.Height div 2, zoomFactor, rmFineResample);
  virtualscreen.Draw(Canvas,Left,Top,True);
end;

procedure TTest6.OnTimer(Width, Height: Integer; ElapsedSec: Double);
begin
  time := time+ElapsedSec*0.5;
  zoomFactor := (sin(time)+1)*Height/image.Height/2;
end;

end.

