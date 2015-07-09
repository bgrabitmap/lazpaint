unit utest23;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, utest, Graphics, BGRABitmap, BGRABitmapTypes, ucube3d;

type
  { TTest23 }

  TTest23 = class(TTest)
  protected
    virtualScreen,backgroundTile,background: TBGRABitmap;
    scene: TCubeScene3D;

  public
    constructor Create;
    destructor Destroy; override;
    procedure OnPaint(Canvas: TCanvas; Left,Top,Width,Height: Integer); override;
    procedure OnTimer(Width,Height: Integer; ElapsedSec: Double); override;
  end;

implementation

uses BGRAScene3D;

{ TTest23 }

constructor TTest23.Create;
begin
  inherited Create;
  Name := 'Multi-polygon antialiasing';
  backgroundTile := TBGRABitmap.Create('..'+pathdelim+'img'+pathdelim+'diamondback.png');
  virtualScreen := nil;
  background := nil;
  scene := TCubeScene3D.Create();
  scene.RenderingOptions.AntialiasingMode := am3dMultishape;
end;

destructor TTest23.Destroy;
begin
  scene.free;
  virtualScreen.Free;
  backgroundTile.Free;
  background.Free;
  inherited Destroy;
end;

procedure TTest23.OnPaint(Canvas: TCanvas; Left,Top,Width, Height: Integer);
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

procedure TTest23.OnTimer(Width, Height: Integer; ElapsedSec: Double);
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


