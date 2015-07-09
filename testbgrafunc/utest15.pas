unit utest15;

{$mode objfpc}{$H+}

interface

uses
  Graphics, Classes, SysUtils, utest, BGRAGradients, BGRABitmap, BGRABitmapTypes;

type

  { TTest15 }

  TTest15 = class(TTest)
  private
    phong: TPhongShading;
    virtualScreen, map, colorMap: TBGRABitmap;
    time: double;
    lightPos1: TPointF;
    ShouldGenerateMap: Boolean;
    procedure GenerateMap;
  public
    constructor Create;
    destructor Destroy; override;
    procedure OnPaint(Canvas: TCanvas; Left,Top,Width,Height: Integer); override;
    procedure OnTimer(Width,Height: Integer; ElapsedSec: Double); override;
  end;

implementation

{ TTest15 }

constructor TTest15.Create;
begin
  Name := 'Phong shading and Perlin noise';
  virtualScreen := nil;
  time := 0;

  randomize;
  phong := TPhongShading.Create;
  phong.LightPositionZ := 128;
  phong.SpecularIndex := 20;
  phong.SpecularFactor := 0.5;
  phong.LightSourceIntensity := 100;
  phong.AmbientFactor := 0.3;
  phong.NegativeDiffusionFactor := 0.3;
  ShouldGenerateMap:= false;
end;

destructor TTest15.Destroy;
begin
  phong.Free;
  virtualScreen.Free;
  inherited Destroy;
end;

procedure TTest15.OnPaint(Canvas: TCanvas; Left,Top,Width, Height: Integer);
const lightSize= 20;
begin
  if (virtualscreen <> nil) and ((virtualscreen.width <> width) or (virtualscreen.Height <> height)) then
  begin
    FreeAndNil(virtualScreen);
    FreeAndNil(map);
    FreeAndNil(colorMap);
  end;
  if virtualscreen = nil then
  begin
    virtualscreen := TBGRABitmap.Create(Width,Height);
  end;

  if ShouldGenerateMap then
  begin
    GenerateMap;
    ShouldGenerateMap := false;
  end;

  if map <> nil then
  begin
    phong.LightPosition.X := round(lightPos1.x * Width);
    phong.LightPosition.Y := round(lightPos1.y * Height);
    phong.Draw(virtualScreen,map,64,0,0,colorMap);
    virtualScreen.GradientFill(phong.LightPosition.X-lightSize,phong.LightPosition.Y-lightSize,
      phong.LightPosition.X+lightSize,phong.LightPosition.Y+lightSize, BGRA(255,255,240,255),BGRAPixelTransparent,
      gtRadial,PointF(phong.LightPosition.X,phong.LightPosition.Y),PointF(phong.LightPosition.X+lightSize,phong.LightPosition.Y),
      dmDrawWithTransparency);
    virtualScreen.Draw(Canvas,Left,Top,True);
  end else
  begin
    virtualScreen.Fill(BGRABlack);
    virtualScreen.TextOut(Width div 2, height div 2-virtualScreen.FontHeight div 2,'Generating map...',BGRAWhite,taCenter);
    virtualScreen.Draw(Canvas,Left,Top,True);
    ShouldGenerateMap := True;
  end;
end;

procedure TTest15.GenerateMap;
var
  i: Integer;
  p: PBGRAPixel;
  pcol: PBGRAPixel;
  temp: TBGRABitmap;
begin
  FreeAndNil(map);
  FreeAndNil(colorMap);
  map := CreatePerlinNoiseMap(virtualscreen.Width,virtualscreen.Height,2,2);
  colorMap := TBGRABitmap.Create(map.width,map.Height,BGRAWhite);
  p := map.Data;
  pcol := colorMap.Data;
  for i := 0 to map.NbPixels-1 do
  begin
    if p^.red < 64 then pcol^ := BGRA(0,128,255) else //water
    if p^.red < 96 then pcol^ := BGRA(240,200,100) else //beach
    if p^.red < 128 then pcol^ := BGRA(100,190,0) else //grass
    if p^.red < 220 then pcol^ := BGRA(180,120,90) else //mountain
      pcol^ := BGRAWhite; //snow

    if p^.red < 128 then //water and beach less
      p^ := MapHeightToBGRA((128-(128-p^.red)/4)/255,255);

    inc(pcol);
    inc(p);
  end;

  temp := colorMap.FilterBlurRadial(2,rbCorona) as TBGRABitmap;
  colorMap.Free;
  colorMap := temp;
end;

procedure TTest15.OnTimer(Width, Height: Integer;
  ElapsedSec: Double);
begin
  time := time+ElapsedSec;
  lightPos1 := pointF((sin(time*0.7+1)+1)/4+0.4,(cos(time*0.5+2)+1)/4+0.3);
end;

end.

