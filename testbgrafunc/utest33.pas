unit utest33;

{$mode objfpc}

interface

uses
  Classes, SysUtils, utest, Graphics, BGRABitmap, BGRABitmapTypes;

type

  { TTest33 }

  TTest33 = class(TTest)
    virtualScreen: TBGRABitmap;

    constructor Create;
    destructor Destroy; override;
    procedure OnPaint(Canvas: TCanvas; Left,Top,Width, Height: Integer); override;
    procedure OnTimer(Width, Height: Integer; ElapsedSec: Double);
      override;
  end;

implementation

{ TTest33 }

constructor TTest33.Create;
begin
  inherited Create;
  Name := 'Text vertical anchor';
  virtualScreen := nil;
end;

destructor TTest33.Destroy;
begin
  virtualscreen.free;
  inherited Destroy;
end;

procedure TTest33.OnPaint(Canvas: TCanvas; Left,Top,Width, Height: Integer);
const
  FontVerticalAnchorToStr: array[TFontVerticalAnchor] of string
    = ('Top of the font with space for Âccents', 'Center of the font', 'Top of the UPPÊRCASE letters',
       'Center of the UPPERCASE letters', 'Top of x and other small letters',
       'Center of x and other small letters', 'Baseline of the text',
       'Descent line for g q, etc.', 'Bottom of the font including bottom line spacing');
var y,h: integer;
  textanchor: TFontVerticalAnchor;
begin
  if (virtualscreen <> nil) and ((virtualscreen.width <> width) or (virtualscreen.Height <> height)) then
    FreeAndNil(virtualScreen);

  if virtualscreen = nil then
    virtualscreen := TBGRABitmap.Create(Width,Height);

  virtualScreen.Fill(CSSLightSteelBlue);
  virtualScreen.FontName := 'Times New Roman';
  h := Height div 20;
  virtualScreen.FontHeight := h;

  y := 0;
  textanchor:= low(TFontVerticalAnchor);

  repeat
    y += h;
    virtualScreen.HorizLine(0,y,virtualScreen.Width,BGRA(255,255,255),dmDrawWithTransparency);
    virtualScreen.FontVerticalAnchor := textanchor;
    virtualScreen.TextOut(h,y, FontVerticalAnchorToStr[textanchor], BGRABlack);
    y += h;
    if textanchor >= high(TFontVerticalAnchor) then break;
    textanchor:= succ(textanchor);
  until false;

  //draw virtualscreen opaque on canvas
  virtualscreen.Draw(Canvas,Left,Top,True);
end;

procedure TTest33.OnTimer(Width, Height: Integer;
  ElapsedSec: Double);
begin
  //nothing
end;

end.

