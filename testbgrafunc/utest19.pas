unit utest19;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, utest, Graphics, BGRABitmap, BGRABitmapTypes, ucube3d, BGRAGradientScanner;

type
  { TTest19 }

  TTest19 = class(TTest)
  protected
    virtualScreen,texture,backgroundTile,background: TBGRABitmap;
    gradient: TBGRAGradientScanner;
    scene: TCubeScene3D;

  public
    constructor Create(TextureInterpolation: boolean; GradientTexture: boolean);
    destructor Destroy; override;
    procedure OnPaint(Canvas: TCanvas; Left,Top,Width,Height: Integer); override;
    procedure OnTimer(Width,Height: Integer; ElapsedSec: Double); override;
  end;

implementation

uses utexture, BGRAScene3D;

{ TTest19 }

constructor TTest19.Create(TextureInterpolation: boolean; GradientTexture: boolean);
begin
  inherited Create;

  scene := TCubeScene3D.Create;
  scene.RenderingOptions.TextureInterpolation := TextureInterpolation;
//  scene.RenderingOptions.LightingInterpolation := liHighQuality;

  if TextureInterpolation then
    Name := 'Perspective' else
    Name := 'Linear and perspective';

  Name += ' texture mapping';

  if gradientTexture then
  begin
    name += ' with gradient';
    gradient := TBGRAGradientScanner.Create(BGRAWhite,BGRABlack,gtRadial,PointF(0.5,0.5),PointF(0.7,0.5),True,True);
    scene.SetCubeTexture(gradient,1,1);
  end else
  begin
    texture := CreateGreenTexture;
    if TextureInterpolation then Name += ' with texture interpolation' else
          Name += ' without texture interpolation';
    texture.ScanInterpolationFilter := rfHalfCosine;
    scene.SetCubeTexture(texture);
  end;

  backgroundTile := TBGRABitmap.Create('..'+pathdelim+'img'+pathdelim+'diamondback.png');
  randomize;
  virtualScreen := nil;
  background := nil;
end;

destructor TTest19.Destroy;
begin
  scene.free;
  gradient.free;
  texture.free;
  virtualScreen.Free;
  backgroundTile.Free;
  background.Free;
  inherited Destroy;
end;

procedure TTest19.OnPaint(Canvas: TCanvas; Left,Top,Width, Height: Integer);
begin
  if (virtualscreen <> nil) and ((virtualscreen.width <> width) or (virtualscreen.Height <> height)) then
  begin
    FreeAndNil(virtualScreen);
    FreeAndNil(background);
  end;

  if virtualscreen = nil then
  begin
    virtualscreen := TBGRABitmap.Create(Width,Height);
    background := backgroundTile.GetPart(rect(0,0,Width,Height)) as TBGRABitmap;
  end;

  virtualScreen.PutImage(0,0,background,dmSet);


  if scene <> nil then
  begin
    scene.Surface := virtualScreen;

    if (gradient <> nil) or scene.RenderingOptions.TextureInterpolation then
    begin
      scene.Render;
    end
    else
    begin
      scene.DefaultLightingNormal := lnNone;
      scene.RenderingOptions.PerspectiveMode := pmLinearMapping;
      scene.ViewCenter := PointF(virtualScreen.Width/4,virtualScreen.Height/2);
      scene.Render;
      scene.RenderingOptions.PerspectiveMode := pmPerspectiveMapping;
      scene.ViewCenter := PointF(virtualScreen.Width*3/4,virtualScreen.Height/2);
      scene.Render;
    end;
  end;

  virtualScreen.draw(Canvas,Left,Top);
end;

procedure TTest19.OnTimer(Width, Height: Integer; ElapsedSec: Double);
var
  moveFactor: single;
begin
  moveFactor := ElapsedSec*20;

  with scene.cube.MainPart do
  begin
    RotateYRad(0.02*moveFactor);
    RotateXRad(-0.01*moveFactor);
    RotateZRad(0.005*moveFactor);
  end;
end;

end.

