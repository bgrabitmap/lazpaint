unit utest31;

{$mode objfpc}

interface

uses
  Classes, SysUtils, utest, Graphics, BGRABitmap, BGRABitmapTypes, BGRAGradients,
  BGRATextFX;

const
  nbPoints=3;

type

  { TTest31 }

  TTest31 = class(TTest)
    virtualScreen: TBGRABitmap;
    pts: array of TPointF;
    dirs: array of TPointF;
    hsla1,hsla2: THSLAPixel;

    textfx: TBGRATextEffect;
    phong: TPhongShading;
    wood: TBGRABitmap;

    textfx_multi: TBGRATextEffect;
    colorArray: array of TBGRAPixel;
    colorTime: double;

    textfx_lightpos: TBGRATextEffect;
    time: double;
    lightPos1: TPointF;

    constructor Create;
    destructor Destroy; override;
    procedure OnPaint(Canvas: TCanvas; Left,Top,Width, Height: Integer); override;
    procedure OnTimer(Width, Height: Integer; ElapsedSec: Double);
      override;
  end;

implementation

uses utexture;

{ TTest31 }

constructor TTest31.Create;
begin
  inherited Create;
  Name := 'Gradient and text effect';
  virtualScreen := nil;
  hsla1.alpha := $ffff;
  hsla1.hue := 0;
  hsla1.lightness := $8000;
  hsla1.saturation := $C000;
  hsla2 := hsla1;
  hsla2.hue := $2000;

  phong := TPhongShading.Create;
  phong.LightPositionZ := 128;
  phong.SpecularIndex := 10;
  phong.SpecularFactor := 0.6;
  phong.LightSourceIntensity := 300;
  phong.AmbientFactor := 0.45;
  phong.NegativeDiffusionFactor := 0.3;

  setlength(colorArray,6);
  colorArray[0] := BGRA(255,128,128);
  colorArray[1] := BGRA(255,160,0);
  colorArray[2] := BGRA(255,255,0);
  colorArray[3] := BGRA(160,255,0);
  colorArray[4] := BGRA(0,255,160);
  colorArray[5] := BGRA(128,128,255);
end;

destructor TTest31.Destroy;
begin
  phong.free;
  textfx.Free;
  textfx_lightpos.Free;
  textfx_multi.Free;
  wood.free;
  virtualScreen.free;
  inherited Destroy;
end;

procedure TTest31.OnPaint(Canvas: TCanvas; Left,Top,Width, Height: Integer);
var font: TFont; h, r: integer;
begin
  if pts = nil then exit;

  if (virtualscreen <> nil) and ((virtualscreen.width <> width) or (virtualscreen.Height <> height)) then
  begin
    FreeAndNil(virtualScreen);
    FreeAndNil(textfx);
    FreeAndNil(textfx_lightpos);
    FreeAndNil(textfx_multi);
    FreeAndNil(wood);
  end;

  if virtualscreen = nil then
    virtualscreen := TBGRABitmap.Create(Width,Height);

  if wood = nil then
  begin
    r := round(sqrt(Width*Height)/4);
    if r < 64 then r:= 64;
    wood := CreateWoodTexture(r,r);
  end;

  font := TFont.Create;
  font.Name := 'Arial';
  if textfx = nil then
  begin
    font.Bold := true;
    font.Height := Height div 4;
    textfx := TBGRATextEffect.Create('BGRA text effect',font,True);
    font.Bold := false;
  end;
  if textfx_lightpos = nil then
  begin
    font.Height := Height div 20;
    textfx_lightpos := TBGRATextEffect.Create('Light position',font,True);
  end;
  if textfx_multi = nil then
  begin
    font.Height := Height div 12;
    font.Name := 'Lucida Console';
    font.Bold := true;
    textfx_multi := TBGRATextEffect.Create('Multicolor',font,True);
  end;
  font.Free;

  virtualScreen.GradientFill(0,0,Width,Height,HSLAToBGRA(hsla1),HSLAToBGRA(hsla2),gtLinear,pts[0],pts[1],dmSet);

  phong.LightPosition.X := round(lightPos1.x * Width);
  phong.LightPosition.Y := round(lightPos1.y * Height);
  h := round(textfx.Height *0.07);
  phong.LightPositionZ := h*4;
  textfx.DrawShaded(virtualScreen,Width div 2,Height div 2-textfx.Height div 2, phong, h,wood, taCenter, True);

  r := textfx_lightpos.height div 8;
  textfx_lightpos.DrawShadow(virtualScreen, phong.LightPosition.X+2*r,phong.LightPosition.Y+8+2*r,r,BGRABlack,taCenter);
  textfx_lightpos.DrawOutline(virtualScreen, phong.LightPosition.X,phong.LightPosition.Y+8,BGRABlack,taCenter);
  textfx_lightpos.Draw(virtualScreen, phong.LightPosition.X,phong.LightPosition.Y+8,BGRAWhite,taCenter);
  virtualscreen.FillEllipseLinearColorAntialias(phong.LightPosition.X,phong.LightPosition.Y,4,4,BGRABlack,BGRAWhite);

  textfx_multi.DrawShadow(virtualScreen, round(pts[2].x),round(pts[2].y), round(textfx_multi.Height/8), BGRA(255,255,255,255),taCenter);
  textfx_multi.DrawMulticolored(virtualScreen, round(pts[2].x),round(pts[2].y), colorArray,taCenter);

  //draw virtualscreen opaque on canvas
  virtualscreen.Draw(Canvas,Left,Top,True);
end;

procedure TTest31.OnTimer(Width, Height: Integer;
  ElapsedSec: Double);
var i: integer;
    moveFactor: single;
    c,c2: TBGRAPixel;
begin
  if pts = nil then
  begin
    setlength(pts,nbPoints);
    setlength(dirs,nbPoints);
    for i := 0 to NbPoints-1 do
    begin
      pts[i] := pointf(random(Width),random(Height));
      dirs[i] := pointf((random(Width)-width/2)/20,(random(Height)-height/2)/20);
    end;
    dirs[2].x /= 10;
    dirs[2].y /= 10;
  end;
  moveFactor := ElapsedSec*20;
  for i := 0 to NbPoints-1 do
  begin
    pts[i].x := pts[i].x+dirs[i].x*moveFactor;
    if pts[i].x < 0 then
    begin
      pts[i].x := 0;
      dirs[i].x := abs(dirs[i].x);
    end;
    if pts[i].x > width-1 then
    begin
      pts[i].x := width-1;
      dirs[i].x := -abs(dirs[i].x);
    end;
    pts[i].y := pts[i].y+dirs[i].y*moveFactor;
    if pts[i].y < 0 then
    begin
      pts[i].y := 0;
      dirs[i].y := abs(dirs[i].y);
    end;
    if pts[i].y > height-1 then
    begin
      pts[i].y := height-1;
      dirs[i].y := -abs(dirs[i].y);
    end;
  end;
  hsla1.hue += round(moveFactor*40);
  hsla2.hue += round(moveFactor*40);

  time := time+ElapsedSec;
  lightPos1 := pointF((sin(time*0.7+1)+1)/4+0.4,(cos(time*0.5+2)+1)/4+0.3);

  colorTime += ElapsedSec;
  if colorTime > 0.3 then
  begin
    colorTime -= 0.3;
    c := colorArray[high(colorArray)];
    for i := 0 to high(colorArray) do
    begin
      c2 := colorArray[i];
      colorArray[i]:= c;
      c:= c2;
    end;
  end;
end;

end.

