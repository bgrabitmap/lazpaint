unit utest1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes, Graphics, utestpacrect;

type
  { TTest1 }

  TTest1 = class(TTestPacRect)
  public
    constructor Create;
    procedure OnPaint(Canvas: TCanvas; Left,Top,Width,Height: Integer); override;
  end;

implementation

{ TTest1 }

constructor TTest1.Create;
begin
  inherited Create;
  Name := 'Canvas.Draw(TBitmap) on Form. NOT RECOMMENDED! Flickering pacmans walking with a rectangle. Rectangle opacity depends on the standard Canvas rendering capacities on windows.';
end;

procedure TTest1.OnPaint(Canvas: TCanvas; Left,Top,Width, Height: Integer);
var i: integer;
begin
  if backgroundImg = nil then exit;

  //draw background as TBitmap
  Canvas.Draw(Left,Top,backgroundImg.Bitmap);

  //draw sprites as TBitmaps
  for i := 0 to high(pacLoc) do
    Canvas.Draw(Left+pacLoc[i].x,Top+pacLoc[i].y,pacImg[numPacImg].Bitmap);
end;

end.

