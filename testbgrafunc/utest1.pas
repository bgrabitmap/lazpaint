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
    procedure OnPaint(Canvas: TCanvas; Width,Height: Integer); override;
  end;

implementation

{ TTest1 }

constructor TTest1.Create;
begin
  inherited Create;
  Name := 'Canvas.Draw(TBitmap) on Form. Flickering pacmans walking with a rectangle. Rectangle opacity depends on the standard Canvas rendering capacities on windows.';
end;

procedure TTest1.OnPaint(Canvas: TCanvas; Width, Height: Integer);
var i: integer;
begin
  if backgroundImg = nil then exit;

  //draw background as TBitmap
  Canvas.Draw(0,0,backgroundImg.Bitmap);

  //draw sprites as TBitmaps
  for i := 0 to high(pacLoc) do
    Canvas.Draw(pacLoc[i].x,pacLoc[i].y,pacImg[numPacImg].Bitmap);
end;

end.

