unit URainType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmapTypes, BGRABitmap;

type

  { TRainRenderer }

  TRainRenderer = class
  protected
    FWind: single;
    FDensity: integer;
    rainData: array of record
        x,ystart,yend: single;
        rainWidth, rainSpeed: single;
        grad: TBGRACustomGradient;
        active: boolean;
        inactiveTime: double;
      end;
    procedure ClearRainData;
    procedure NeedRainArray(w, h, iRainProba: integer; rainSizeX,
      rainSizeY: single);
    procedure PrepareRainArray(nbRain: integer; ScaleX: single);
    function PrepareRainDrop(i: integer; rainSizeX, rainSizeY: single): single;
  public
    constructor Create(AWind: single; ADensity: integer); //Example: -0.5, 2
    procedure RainElapse(elapsed: double; rainProba: single; w, h: integer);
    procedure RenderRain(Bitmap: TBGRABitmap);
    destructor Destroy; override;
  end;

implementation

uses BGRAGradientScanner;

{ TRainRenderer }

constructor TRainRenderer.Create(AWind: single; ADensity: integer);
begin
  FWind := AWind;
  FDensity:= ADensity;
end;

destructor TRainRenderer.Destroy;
begin
  ClearRainData;
  inherited Destroy;
end;

procedure TRainRenderer.ClearRainData;
var i: integer;
begin
  for i := 0 to high(rainData) do
    rainData[i].grad.Free;
  rainData := nil;
end;

procedure TRainRenderer.RenderRain(Bitmap: TBGRABitmap);
var
  i,h2: Integer;
  scan: TBGRAGradientScanner;
begin
  h2 := Bitmap.Height div 2;
  for i:= 0 to high(rainData) do
  with rainData[i] do
  if active then
  begin
    scan := TBGRAGradientScanner.Create(grad, gtLinear, PointF(0,ystart),PointF(0,yend));
    Bitmap.DrawLineAntialias(x+(ystart-h2)*FWind,ystart,x+(yend-h2)*FWind,yend,scan,rainWidth,true);
    scan.Free;
  end;
end;

//returns raindrop height
function TRainRenderer.PrepareRainDrop(i: integer; rainSizeX,rainSizeY: single): single;
var dist: single;
begin
  with rainData[i] do
  begin
    dist := (random(100)+10)/10;
    rainSpeed := 1/dist;
    rainWidth := rainSizeX/dist;
    if rainWidth < 1 then rainWidth := 1;
    result := rainSizeY/dist*(random(50)+75)/100;
  end;
end;

procedure TRainRenderer.NeedRainArray(w, h, iRainProba: integer; rainSizeX,rainSizeY: single);
var
  nbRain: Integer;
  i: Integer;
begin
  nbRain := (w+round(abs(FWind)*h)) *FDensity;
  if length(rainData)<> nbRain then
  begin
    PrepareRainArray(nbRain,1/FDensity);
    for i := 0 to high(rainData) do
    with rainData[i] do
    begin
      x -= abs(FWind)*h/2;
      if random(1000) < iRainProba then
      begin
        active := true;
        ystart := Random(h*2)-h/2;
        yend := ystart + PrepareRainDrop(i, rainSizeX,rainSizeY);
      end;
    end;
  end;
end;

procedure TRainRenderer.RainElapse(elapsed: double; rainProba: single; w,h: integer);
var
  i: integer;
  rainSizeY,rainSizeX: single;
  delta: single;
  iRainProba: integer;
begin
  iRainProba := round(rainProba*1000);
  rainSizeY := 2+h*rainProba;
  rainSizeX := 7*rainProba;
  if rainSizeX < 4 then rainSizeX := 4;

  NeedRainArray(w,h, iRainProba, rainSizeX,rainSizeY);
  for i := 0 to high(rainData) do
  with rainData[i] do
  if active then
  begin
    delta := h*rainSpeed*elapsed;
    ystart += delta;
    yend += delta;
    if ystart >= h then
    begin
      if random(1000) < iRainProba then
      begin
        yend := -(ystart-h);
        ystart := yend - PrepareRainDrop(i, rainSizeX,rainSizeY);
      end else
      begin
        active := false;
        inactiveTime:= 0;
      end;
    end;
  end else
  begin
    inactiveTime+= elapsed;
    if inactiveTime > 0.5 then
    begin
      inactiveTime -= 0.5;
      if random(1000) < iRainProba then
      begin
        active := true;
        ystart := -random(h)/2;
        yend := ystart + PrepareRainDrop(i, rainSizeX,rainSizeY);
      end;
    end;
  end;
end;

procedure TRainRenderer.PrepareRainArray(nbRain: integer; ScaleX: single);
var
  i: Integer;
begin
  ClearRainData;
  setlength(rainData, nbRain);
  for i := 0 to high(rainData) do
  with rainData[i] do
  begin
    x := i*scaleX;
    grad := TBGRAMultiGradient.Create([BGRAPixelTransparent, BGRA(255,255,255,random(20)+50), BGRAPixelTransparent],[0,0.9,1],True);
    active:= false;
    inactiveTime := 0;
  end;
end;

end.

