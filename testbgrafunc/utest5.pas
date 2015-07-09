unit utest5;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes, Graphics, utestpacrect;

type
  { TTest5 }

  TTest5 = class(TTestPacRect)
  protected
    virtualScreen: TBGRABitmap;
  public
    constructor Create;
    procedure OnPaint(Canvas: TCanvas; Left,Top,Width,Height: Integer); override;
  end;

implementation

{ TTest5 }

constructor TTest5.Create;
begin
  inherited Create;
  Name := 'TBGRABitmap.PutImage. This is the recommended way. Non-flickering pacmans walking with a rectangle. Rectangle opacity always there with the rendering capacities on BGRABitmap.';
  virtualScreen := nil;
end;

procedure TTest5.OnPaint(Canvas: TCanvas; Left,Top,Width, Height: Integer);
var i: integer;
begin
  if backgroundImg = nil then exit;

  if (virtualscreen <> nil) and ((virtualscreen.width <> width) or (virtualscreen.Height <> height)) then
    FreeAndNil(virtualScreen);

  if virtualscreen = nil then
    virtualscreen := TBGRABitmap.Create(Width,Height);

  //draw background opaque in BGRA bitmap
  virtualscreen.PutImage(0,0,backgroundImg,dmSet);

  //draw sprites transparent in BGRA bitmap
  for i := 0 to high(pacLoc) do
    virtualscreen.PutImage(pacLoc[i].x,pacLoc[i].y,pacImg[numPacImg],dmDrawWithTransparency);

  //draw virtualscreen opaque on canvas
  virtualscreen.Draw(Canvas,Left,Top,True);
end;

end.
