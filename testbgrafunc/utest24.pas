unit utest24;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, utest, Graphics, BGRABitmap, BGRABitmapTypes, ucube3d;

type
  { TTest24 }

  TTest24 = class(TTest)
  protected
    virtualScreen,backgroundTile,texture,background: TBGRABitmap;
    scene: TCubeScene3D;

  public
    constructor Create;
    destructor Destroy; override;
    procedure OnPaint(Canvas: TCanvas; Left,Top,Width,Height: Integer); override;
    procedure OnTimer(Width,Height: Integer; ElapsedSec: Double); override;
  end;

implementation

uses utexture, BGRAScene3D;

{ TTest24 }

constructor TTest24.Create;
begin
  inherited Create;
  Name := 'Multi-polygon antialiasing with perspective mapping';
  backgroundTile := TBGRABitmap.Create('..'+pathdelim+'img'+pathdelim+'diamondback.png');
  texture := CreateMarbleTexture(256,256);
  texture.ScanInterpolationFilter:= rfLinear;
  virtualScreen := nil;
  background := nil;
  scene := TCubeScene3D.Create();
  scene.RenderingOptions.AntialiasingMode := am3dMultishape;
  scene.SetCubeTexture(texture);
end;

destructor TTest24.Destroy;
begin
  scene.free;
  virtualScreen.Free;
  backgroundTile.Free;
  background.Free;
  texture.free;
  inherited Destroy;
end;

procedure TTest24.OnPaint(Canvas: TCanvas; Left,Top,Width, Height: Integer);
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
    scene.Render;
  end;

  virtualScreen.draw(Canvas,Left,Top);
end;

procedure TTest24.OnTimer(Width, Height: Integer; ElapsedSec: Double);
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


