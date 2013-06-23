unit utexture;

{$mode objfpc}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes;

function CreateGreenTexture: TBGRABitmap;
function CreateMarbleTexture(tx,ty: integer): TBGRABitmap;
function CreateWoodTexture(tx,ty: integer): TBGRABitmap;

implementation

uses BGRAGradients;

function Interp256(value1,value2,position: integer): integer; inline; overload;
begin
     result := (value1*(256-position) + value2*position) shr 8;
end;

function Interp256(color1,color2: TBGRAPixel; position: integer): TBGRAPixel; inline; overload;
begin
     result.red := Interp256(color1.red,color2.red, position);
     result.green := Interp256(color1.green,color2.green, position);
     result.blue := Interp256(color1.blue,color2.blue, position);
     result.alpha := Interp256(color1.alpha,color2.alpha, position);
end;

function CreateGreenTexture: TBGRABitmap;
var
  p: PBGRAPixel;
  i: Integer;
begin
  result := CreatePerlinNoiseMap(256,256);
  BGRAReplace(result,result.Resample(32,32));
  p := result.data;
  for i := 1 to result.NbPixels do
  begin
    p^ := Interp256( BGRA(0,128,0), BGRA(192,255,0), p^.red );
    inc(p);
  end;
end;

function CreateMarbleTexture(tx,ty: integer): TBGRABitmap;
var
  colorOscillation: integer;
  p: PBGRAPixel;
  i: Integer;
begin
  result := CreateCyclicPerlinNoiseMap(tx,ty,0.5,0.5,0.8,rfBestQuality);
  p := result.Data;
  for i := 0 to result.NbPixels-1 do
  begin
    colorOscillation := round(sqrt(sqrt((sin(p^.red*Pi/128+0.5)+1)/2))*256);
    p^ := Interp256(BGRA(161,117,105),BGRA(218,197,180),colorOscillation);
    inc(p);
  end;
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
end;

end.

