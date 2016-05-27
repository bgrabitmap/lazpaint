unit utexture;

{$mode objfpc}{$H+}

interface

{ This unit creates textures. It is mainly based on Perlin noise maps,
  which are random maps that have a natural look. Then a color is applied
  by linear interpolation. The water texture is achieved by using
  a Phong lighting based on the Perlin noise map. }

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes;

function CreateGrassTexture(tx,ty: integer): TBGRABitmap;
function CreateVerticalWoodTexture(tx, ty: integer): TBGRABitmap;
function CreateWoodTexture(tx,ty: integer): TBGRABitmap;
function CreateWaterTexture(tx,ty: integer): TBGRABitmap;

implementation

uses BGRAOpenGL, BGRAGradients;

function Interp256(value1,value2,position: integer): integer; inline;
begin
     result := (value1*(256-position)+value2*position) shr 8;
end;

function Interp256(color1,color2: TBGRAPixel; position: integer): TBGRAPixel; inline;
begin
     result.red := Interp256(color1.red,color2.red,position);
     result.green := Interp256(color1.green,color2.green,position);
     result.blue := Interp256(color1.blue,color2.blue,position);
     result.alpha := Interp256(color1.alpha,color2.alpha,position);
end;

function CreateWoodTexture(tx,ty: integer): TBGRABitmap;
var
  colorOscillation, globalColorVariation: integer;
  p: PBGRAPixel;
  i: Integer;
begin
  result := CreateCyclicPerlinNoiseMap(tx,ty,1.5,1.5,1,rfBestQuality);
  p := result.Data;
  for i := 0 to result.NbPixels-1 do
  begin
    colorOscillation := round(sqrt((sin(p^.red*Pi/16)+1)/2)*256);
    globalColorVariation := p^.red;
    p^:= Interp256( Interp256(BGRA(247,188,120),BGRA(255,218,170),colorOscillation),
                    Interp256(BGRA(157,97,60),BGRA(202,145,112),colorOscillation), globalColorVariation);
    inc(p);
  end;
  BGRAReplace(result, TBGLBitmap.Create(result));
end;

function CreateVerticalWoodTexture(tx, ty: integer): TBGRABitmap;
var
  globalPos: single;
  colorOscillation, globalColorVariation: integer;
  p: PBGRAPixel;
  i: Integer;
  x,nbVertical: integer;
begin
  result := CreateCyclicPerlinNoiseMap(tx,ty,1,1,1,rfBestQuality);
  p := result.Data;
  x := 0;
  nbVertical := tx div 128;
  if nbVertical = 0 then nbVertical := 1;
  for i := 0 to result.NbPixels-1 do
  begin
    globalPos := p^.red*Pi/32 + nbVertical*x*2*Pi/tx*8;
    colorOscillation := round(sqrt((sin(globalPos)+1)/2)*256);
    globalColorVariation := p^.red; //round(sin(globalPos/8)*128+128);
    p^:= Interp256( Interp256(BGRA(247,188,120),BGRA(255,218,170),colorOscillation),
                    Interp256(BGRA(157,97,60),BGRA(202,145,112),colorOscillation), globalColorVariation);
    inc(p);
    inc(x);
    if x = tx then x := 0;
  end;
  BGRAReplace(result, TBGLBitmap.Create(result));
end;

function CreateGrassTexture(tx,ty: integer): TBGRABitmap;
var
  p: PBGRAPixel;
  i: Integer;
begin
  result := CreateCyclicPerlinNoiseMap(tx,ty,1,1,1,rfBestQuality);
  p := result.Data;
  for i := 0 to result.NbPixels-1 do
  begin
    p^ := Interp256( BGRA(0,128,0), BGRA(192,255,0), p^.red );
    inc(p);
  end;
  BGRAReplace(result, TBGLBitmap.Create(result));
end;

function CreateWaterTexture(tx,ty: integer): TBGRABitmap;
const blurSize = 5;
var
  map: TBGRABitmap;
  phong: TPhongShading;
begin
  result := TBGLBitmap.Create(tx,ty);
  map := CreateCyclicPerlinNoiseMap(tx,ty,1,1,1.2,rfBestQuality);
  BGRAReplace(map,map.GetPart(rect(-blurSize,-blurSize,tx+blurSize,ty+blurSize)));
  BGRAReplace(map,map.FilterBlurRadial(blurSize,rbFast));
  phong := TPhongShading.Create;
  phong.LightSourceDistanceFactor := 0;
  phong.LightDestFactor := 0;
  phong.LightSourceIntensity := 150;
  phong.LightPositionZ := 80;
  phong.LightColor := BGRA(105,233,240);
  phong.NegativeDiffusionFactor := 0.3;
  phong.SpecularIndex := 20;
  phong.AmbientFactor := 0.4;
  phong.Draw(result,map,20,-blurSize,-blurSize,BGRA(28,139,166));
  phong.Free;
  map.Free;
end;


end.

