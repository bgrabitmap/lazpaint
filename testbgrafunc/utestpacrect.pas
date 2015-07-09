unit utestpacrect;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes, Graphics, utest;

const NbPacman = 20;

type
  { TTestPacRect }

  TTestPacRect = class(TTest)
  public
    constructor Create;
    destructor Destroy; override;
    procedure OnTimer(Width,Height: Integer; ElapsedSec: Double); override;
  protected
    pacLoc: array of TPoint;
    pacImg: array of TBGRABitmap;
    pacSize : TPoint;
    backgroundTile,backgroundImg: TBGRABitmap;
    numPacImg: integer;
    function AddTranspRectTo(filename: string): TBGRABitmap;
  end;

implementation

{ TTestPacRect }

constructor TTestPacRect.Create;
begin
  inherited Create;
  randomize;
  setlength(pacImg,3);
  pacImg[0] := AddTranspRectTo('..'+pathdelim+'img'+pathdelim+'pac_d1.bmp');
  pacImg[1] := AddTranspRectTo('..'+pathdelim+'img'+pathdelim+'pac_d2.bmp');
  pacImg[2] := AddTranspRectTo('..'+pathdelim+'img'+pathdelim+'pac_d3.bmp');
  pacImg[3] := AddTranspRectTo('..'+pathdelim+'img'+pathdelim+'pac_d2.bmp');

  backgroundTile := TBGRABitmap.Create('..'+pathdelim+'img'+pathdelim+'diamondback.png');
  backgroundImg := nil;

  numPacImg := 0;
  pacSize := Point(pacImg[0].Width,pacImg[0].Height);
end;

destructor TTestPacRect.Destroy;
var i: integer;
begin
  backgroundImg.Free;
  backgroundTile.Free;
  for i := 0 to high(pacImg) do
    pacImg[i].Free;
  inherited Destroy;
end;

procedure TTestPacRect.OnTimer(Width, Height: Integer; ElapsedSec: Double);
var i: integer;
begin
  if pacLoc = nil then
  begin
    setlength(pacLoc,NbPacman);
    for i := 0 to high(pacLoc) do
      pacLoc[i] := Point(random(Width+pacSize.X)-pacSize.X,random(Height+pacSize.Y)-pacSize.Y);
  end else
  begin
    for i := 0 to high(pacLoc) do
      if (pacLoc[i].Y <= -pacSize.Y) or (pacLoc[i].Y >= Height) then
        pacLoc[i] := Point(random(Width+pacSize.X)-pacSize.X,random(Height+pacSize.Y)-pacSize.Y)
      else
      begin
        inc(pacLoc[i].X,4);
        if pacLoc[i].X >= Width then
        begin
          pacLoc[i].X := -pacSize.X;
          pacLoc[i].Y := random(Height+pacSize.Y)-pacSize.Y;
        end;
      end;
  end;
  numPacImg := (numPacImg+1) mod length(pacImg);

  if (backgroundImg <> nil) and ((backgroundImg.Width <> Width) or (backgroundImg.Height <> Height)) then
    FreeAndNil(backgroundImg);

  if backgroundImg = nil then
    backgroundImg := backgroundTile.GetPart(rect(0,0,Width,Height)) as TBGRABitmap;
end;

function TTestPacRect.AddTranspRectTo(filename: string): TBGRABitmap;
var originalImage: TBGRABitmap;
begin
  originalImage := TBGRABitmap.Create(filename);
  originalImage.ReplaceColor(originalImage.GetPixel(0,0),BGRAPixelTransparent);
  result := TBGRABitmap.Create(originalImage.Width+originalImage.Height,OriginalImage.Height);
  result.PutImage(0,0,originalImage,dmSet);
  result.Rectangle(originalImage.Width,0,result.Width,result.Height,BGRABlack,BGRA(0,0,0,64),dmDrawWithTransparency);
  originalImage.Free;
end;

end.

