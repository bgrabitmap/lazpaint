unit utest4;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes, Graphics, utestpacrect;

type
  { TTest4 }

  TTest4 = class(TTestPacRect)
  protected
    virtualScreen: TBitmap;
  public
    constructor Create;
    procedure OnPaint(Canvas: TCanvas; Left,Top,Width,Height: Integer); override;
  end;

implementation

{ TTest4 }

constructor TTest4.Create;
begin
  inherited Create;
  Name := 'TBGRABitmap.Draw(Bitmap). Non-flickering pacmans walking with a rectangle. Rectangle opacity depends on the rendering capacities of BGRABitmap on TBitmap.';
  virtualScreen := nil;
end;

procedure TTest4.OnPaint(Canvas: TCanvas; Left,Top,Width, Height: Integer);
var i: integer;
begin
  if backgroundImg = nil then exit;

  if (virtualscreen <> nil) and ((virtualscreen.width <> width) or (virtualscreen.Height <> height)) then
    FreeAndNil(virtualScreen);

  if virtualscreen = nil then
  begin
    virtualscreen := TBitmap.Create;
    virtualscreen.width := Width;
    virtualscreen.Height := height;
  end;

  //draw background opaque on bitmap canvas
  backgroundImg.Draw(virtualScreen.Canvas,0,0,true);

  //draw sprites transparent on bitmap canvas
  for i := 0 to high(pacLoc) do
    pacImg[numPacImg].Draw(virtualscreen.Canvas,pacLoc[i].x,pacLoc[i].y,false);

  //draw virtualscreen as TBitmap
  Canvas.Draw(Left,Top,VirtualScreen);
end;

end.
