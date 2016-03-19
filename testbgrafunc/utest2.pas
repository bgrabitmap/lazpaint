unit utest2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes, Graphics, utestpacrect;

type
  { TTest2 }

  TTest2 = class(TTestPacRect)
  protected
    virtualScreen: TBitmap;
  public
    constructor Create;
    procedure OnPaint(Canvas: TCanvas; Left,Top,Width,Height: Integer); override;
  end;

implementation

{ TTest2 }

constructor TTest2.Create;
begin
  inherited Create;
  Name := 'Canvas.Draw(TBitmap) on TBitmap. NOT RECOMMENDED! Non-flickering pacmans walking with a rectangle. Rectangle opacity depends on the standard Canvas rendering capacities on bitmap canvas.';
  virtualScreen := nil;
end;

procedure TTest2.OnPaint(Canvas: TCanvas; Left,Top,Width, Height: Integer);
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

  //draw background as TBitmap on bitmap Canvas
  virtualscreen.Canvas.Draw(0,0,backgroundImg.Bitmap);

  //draw sprites as TBitmaps on bitmap Canvas
  for i := 0 to high(pacLoc) do
    virtualscreen.Canvas.Draw(pacLoc[i].x,pacLoc[i].y,pacImg[numPacImg].Bitmap);

  //draw virtualscreen as TBitmap
  Canvas.Draw(Left,Top,virtualscreen);
end;

end.
