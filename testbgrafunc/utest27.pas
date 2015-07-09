unit utest27;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, utest, Graphics, BGRABitmap, BGRABitmapTypes, BGRAScene3D, utore3d;

type
  { TTest27 }

  TTest27 = class(TTest)
  protected
    virtualScreen,backgroundTile,background: TBGRABitmap;
    scene: TToreScene3D;
  public
    constructor Create(lightNormal: TLightingNormal3D; lightInterp: TLightingInterpolation3D);
    destructor Destroy; override;
    procedure OnPaint(Canvas: TCanvas; Left,Top,Width,Height: Integer); override;
    procedure OnTimer(Width,Height: Integer; ElapsedSec: Double); override;
  end;

implementation

{ TTest27 }

constructor TTest27.Create(lightNormal: TLightingNormal3D; lightInterp: TLightingInterpolation3D);
begin
  inherited Create;
  Name := 'Torus with ';
  case lightNormal of
    lnFace: Name += 'flat';
    lnVertex: Name += 'vertex';
    lnFaceVertexMix: Name += 'half-vertex';
    lnNone: Name += 'no lighting';
  end;
  if lightNormal <> lnNone then
    case lightInterp of
      liLowQuality: if lightNormal <> lnFace then Name += ' low-quality shading'
                  else Name += ' shading';
      liAlwaysHighQuality: Name += ' high-quality shading';
    end;
  backgroundTile := TBGRABitmap.Create('..'+pathdelim+'img'+pathdelim+'diamondback.png');
  virtualScreen := nil;
  background := nil;
  scene := TToreScene3D.Create();
  scene.RenderingOptions.AntialiasingMode := am3dNone;
  scene.RenderingOptions.LightingInterpolation := lightInterp;
  scene.DefaultLightingNormal:= lightNormal;
end;

destructor TTest27.Destroy;
begin
  scene.free;
  virtualScreen.Free;
  backgroundTile.Free;
  background.Free;
  inherited Destroy;
end;

procedure TTest27.OnPaint(Canvas: TCanvas; Left,Top,Width, Height: Integer);
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

procedure TTest27.OnTimer(Width, Height: Integer; ElapsedSec: Double);
var
  moveFactor: single;
begin
  moveFactor := ElapsedSec*20;
  scene.angle := scene.angle + moveFactor;
end;

end.


