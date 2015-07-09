unit utest16;

{$mode objfpc}{$H+}

interface

uses
  Graphics, Classes, SysUtils, utest, BGRAGradients, BGRABitmap, BGRABitmapTypes;

type

  { TTest16 }

  TTest16 = class(TTest)
  private
    phong: TPhongShading;
    chartTime,lightTime: double;
    lightPos1: TPointF;
    RoundCorners : boolean;
    series: array of record
      val1,val2: integer;
    end;
    color1,color2: TBGRAPixel;
    procedure NewChart;
  public
    constructor Create;
    destructor Destroy; override;
    procedure OnPaint(Canvas: TCanvas; Left,Top,Width,Height: Integer); override;
    procedure OnTimer(Width,Height: Integer; ElapsedSec: Double); override;
  end;

implementation

uses types;

{ TTest16 }

constructor TTest16.Create;
begin
  Name := 'Phong shading primitives';
  lightTime := 0;
  chartTime := 0;

  randomize;
  phong := TPhongShading.Create;
  NewChart;
end;

procedure TTest16.NewChart;
var
  i: Integer;

  procedure RandomSwap(var color: TBGRAPixel);
  var temp: byte;
  begin
    if random(2)= 0 then
    begin
      temp := color.Red;
      color.red := color.Green;
      color.green := temp;
    end;
    if random(2)= 0 then
    begin
      temp := color.green;
      color.green := color.blue;
      color.blue := temp;
    end;
  end;

begin
  setlength(series, 4+random(6));
  for i := 0 to high(series) do
  begin
    series[i].val1 := random(50);
    series[i].val2 := random(50);
  end;
  color1 := BGRA(255,160,0);
  RandomSwap(color1);
  color2 := BGRA(140,180,255);
  RandomSwap(Color2);
  RoundCorners := not RoundCorners;
end;

destructor TTest16.Destroy;
begin
  phong.Free;

  inherited Destroy;
end;

procedure TTest16.OnPaint(Canvas: TCanvas; Left,Top,Width, Height: Integer);
var
  borderSize: integer;
  barPrecalc: TBGRABitmap;

  procedure DrawBar(image: TBGRABitmap; bounds: TRect; color : TBGRAPixel);
  begin
    if barPrecalc <> nil then
      phong.Draw(image,barPrecalc,borderSize,bounds.Left,bounds.Top,color)
    else
      phong.DrawRectangle(image,bounds,borderSize,borderSize,color,RoundCorners,[rmoNoBottomBorder]);
  end;

const
  chartTitle = 'Chart example';

var
  image: TBGRABitmap;
  i: integer;
  maxVal: integer;
  r: TRect;
  gbounds: TRect;
  s: TSize;

begin
  if lightTime = 0 then exit;
  barPrecalc := nil;

  //create background
  image := TBGRABitmap.Create(Width,Height);
  image.GradientFill(0,0,image.Width,image.Height,BGRA(255,255,255),BGRA(128,128,128),gtLinear,PointF(0,0),PointF(image.Width,Image.Height),dmSet);

  gbounds := rect(round(image.Width*0.1),round(image.Height*0.2),round(image.Width*0.9),round(image.Height*0.98));
  image.Rectangle(gbounds,BGRA(0,0,0,96),dmDrawWithTransparency);
  borderSize := round((gbounds.right-gbounds.left)/length(series)*0.06);
  if borderSize = 0 then borderSize := 1;

  //init phong
  phong.LightPosition := Point(round(lightPos1.X*Width),round(lightPos1.Y*Height));
  phong.LightPositionZ := 2*borderSize;

  //draw title
  image.FontHeight := gbounds.Top div 4;
  s := image.TextSize(chartTitle);
  phong.DrawSphere(image, rect((gbounds.Left+gbounds.Right-s.cx*2) div 2,(gbounds.Top-s.cy*2) div 2,
    (gbounds.Left+gbounds.Right+s.cx*2) div 2,(gbounds.Top+s.cy*2) div 2),10,BGRA(0,255,0,192));
  image.TextOut((gbounds.Left+gbounds.Right) div 2+1, (gbounds.Top-s.cy) div 2+1,ChartTitle,BGRABlack,taCenter);
  image.TextOut((gbounds.Left+gbounds.Right) div 2-1, (gbounds.Top-s.cy) div 2-1,ChartTitle,BGRAWhite,taCenter);

  //compute max value
  maxVal := 1;
  for i := 0 to high(series) do
  begin
    if series[i].val1 > maxVal then maxVal := series[i].Val1;
    if series[i].val2 > maxVal then maxVal := series[i].Val2;
  end;

  if RoundCorners then
    barPrecalc := CreateRoundRectangleMap(round((0.4-0.1)/length(series)*(gbounds.Right-gBounds.Left)),gBounds.Bottom-gBounds.Top,borderSize,[rmoNoBottomBorder])
  else
    barPrecalc := CreateRectangleMap(round((0.4-0.1)/length(series)*(gbounds.Right-gBounds.Left)),gBounds.Bottom-gBounds.Top,borderSize,[rmoNoBottomBorder]);
  image.ClipRect := gBounds;

  //draw bars
  for i := 0 to high(series) do
  begin
    r.Left := gbounds.Left+round((i+0.1)/length(series)*(gbounds.Right-gBounds.Left));
    r.Right := gbounds.Left+round((i+0.4)/length(series)*(gbounds.Right-gBounds.Left));
    r.top := gbounds.Top+round((gbounds.Bottom-gBounds.Top)*(1-series[i].val1/maxVal));
    r.bottom := gbounds.Bottom;
    DrawBar(image,r,color1);

    r.Left := gbounds.Left+round((i+0.43)/length(series)*(gbounds.Right-gBounds.Left));
    r.Right := gbounds.Left+round((i+0.73)/length(series)*(gbounds.Right-gBounds.Left));
    r.top := gbounds.Top+round((gbounds.Bottom-gBounds.Top)*(1-series[i].val2/maxVal));
    r.bottom := gbounds.Bottom;
    DrawBar(image,r,color2);
  end;

  image.NoClip;
  barPrecalc.Free;

  //draw chart
  image.Draw(Canvas,Left,Top,True);

  image.free;
end;

procedure TTest16.OnTimer(Width, Height: Integer;
  ElapsedSec: Double);
begin
  lightTime := lightTime+ElapsedSec*0.2;
  lightPos1 := pointF((sin(lightTime*0.7+1)+1)/2,(cos(lightTime*0.5+2)+1)/4-0.3);

  chartTime := chartTime+ElapsedSec;
  if chartTime > 10 then
  begin
    chartTime := chartTime-10;
    NewChart;
  end;
end;

end.

