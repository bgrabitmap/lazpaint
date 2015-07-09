unit utest10;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, utest, Graphics, BGRABitmap, BGRABitmapTypes;

const
  sinTabPrecision = 12;

type
  { TTest10 }

  TTest10 = class(TTest)
  protected
    virtualScreen: TBGRABitmap;
    time: single;
  public
    constructor Create;
    destructor Destroy; override;
    procedure OnPaint(Canvas: TCanvas; Left,Top,Width,Height: Integer); override;
    procedure OnTimer(Width,Height: Integer; ElapsedSec: Double); override;
  end;

implementation

{ TTest10 }

constructor TTest10.Create;
begin
  inherited Create;
  Name := 'Plasma rendered by Scanline access';
  virtualScreen := nil;
  time := 0;
end;

destructor TTest10.Destroy;
begin
  virtualScreen.Free;
  inherited Destroy;
end;

procedure TTest10.OnPaint(Canvas: TCanvas; Left,Top,Width, Height: Integer);
var xb,yb: integer;
    p: PBGRAPixel;
    value: integer;
    center: TPointF;
    intTime1, intTime2, intTime3: integer;
begin
  if (virtualscreen <> nil) and ((virtualscreen.width <> width) or (virtualscreen.Height <> height)) then
    FreeAndNil(virtualScreen);

  if virtualscreen = nil then
    virtualscreen := TBGRABitmap.Create(Width,Height);

  center := pointF((sin(time)+1)/4+0.2,(cos(time*0.3)+1)/4+0.3);
  intTime1 := round(time*65536*7/10);
  intTime2 := round(time*65536*11/10);
  intTime3 := round(time*65536*13/10);
  for yb := 0 to virtualscreen.Height-1 do
  begin
    p := virtualScreen.ScanLine[yb];
    for xb := 0 to virtualScreen.Width-1 do
    begin
      value := Sin65536(
                        (xb shl 8 + (height-yb)*(Sin65536(intTime2 shr 2) shr 8 - 128) + intTime1 shr 5 + Sin65536(yb shl 16 div height + intTime3) shr 4)*
                        ( Sin65536(intTime1 shr 3) shr 9 - 64 + 256 )*2 div Width);
      value += Sin65536(yb * ( Sin65536(intTime1 shr 2) shr 9 - 64 + 256 )*256 div Height + intTime1 shr 1 +
                        xb*(Sin65536(intTime2 shr 4) shr 8 - 128) );

      value += Sin65536( round( sqrt(sqr(xb/width-center.x)+sqr(yb/height-center.y))
                          * ( (Sin65536(intTime3 shr 3)+ 2*65536)  ) ) + intTime2 shr 2);

      value := value div (3*128);

      if value > 255 then
      begin
        if value < 511 then value := value-256 else
          value := 255;
        p^.red := 255;
        p^.green := 64+value shr 1;
        p^.blue := 0;
      end else
      begin
        p^.red := value;
        p^.green := value shr 2;
        p^.blue := 0;
      end;
      p^.alpha := 255;
      inc(p);
    end;
  end;
  virtualScreen.InvalidateBitmap;

  virtualscreen.Draw(Canvas,Left,Top,True);
end;

procedure TTest10.OnTimer(Width, Height: Integer; ElapsedSec: Double);
begin
  time := time+ElapsedSec;
end;

end.

