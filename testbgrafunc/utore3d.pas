unit utore3d;

{$mode objfpc}

interface

uses
  Classes, SysUtils, BGRAScene3D;

type

  { TToreScene3D }

  TToreScene3D = class(TBGRAScene3D)
  private
    shape,lightObj: IBGRAObject3D;
    procedure CreateLight;
    procedure CreateShape;
  public
    angle: single;
    constructor Create;
    procedure Render; override;
  end;

implementation

uses BGRABitmapTypes, BGRAMatrix3D;

{ TToreScene3D }

constructor TToreScene3D.Create;
begin
  inherited Create;
  CreateLight;
  CreateShape;
end;

procedure TToreScene3D.Render;
begin
  with shape.MainPart do
  begin
    ResetTransform;
    Translate(sin(angle*Pi/180+Pi/4)*10,0,0);
    Scale(20);
    RotateZDeg(angle*0.3);
    RotateXDeg(angle*0.1+90);
  end;
  with lightObj.MainPart do
  begin
    ResetTransform;
    RotateYDeg(angle);
    Translate(60,0,0);
  end;
  inherited Render;
end;

procedure TToreScene3D.CreateShape;
const NbX = 40;
      NbY = 20;
var
  v: array[0..NbY-1,0..NbX-1] of IBGRAVertex3D;
  i,j: integer;
  m: TMatrix3D;
  p,ofs: TPoint3D;
begin
  shape := CreateObject(BGRA(255,0,0));

  m := MatrixRotateY(2*Pi/NbY);

  p := Point3D(0.3,0,0);
  ofs := Point3D(1,0,0);
  for i := 0 to NbY-1 do
  begin
    v[i,0] := shape.MainPart.Add(p+ofs);
    p := m*p;
  end;

  m := MatrixRotateZ(2*Pi/NbX);
  for j := 1 to NbX-1 do
    for i := 0 to NbY-1 do
      v[i,j] := shape.MainPart.Add(m*v[i,j-1].SceneCoord);

  for j := 0 to NbX-1 do
    for i := 0 to NbY-1 do
      shape.AddFace([v[i,j],v[i,(j+1)mod NbX],v[(i+1) mod NbY,(j+1)mod NbX],v[(i+1) mod NbY,j]]);
end;

procedure TToreScene3D.CreateLight;
var v: array of IBGRAVertex3D;
    lamp: IBGRAMaterial3D;
begin
  lightObj := CreateObject(BGRAWhite);
  DefaultLightingNormal := lnVertex;
  DefaultMaterial.SpecularIndex := 10;
 { if DefaultLightingNormal = lnFace then
  begin
    AmbiantLightness := 0.4;
    AddPointLight(lightObj.Vertices.Add(0,0,0),40,0.8,-0.3)
  end
  else
  begin  }
    AmbiantLightColor := BGRA(128,128,128);
    AddPointLight(lightObj.MainPart.Add(0,0,0),40,BGRA(128,128,64),-0.3);
  //end;

  v := lightObj.MainPart.Add([-1,-2,-2, 1,-1,-1, 1,1,-1, -1,2,-2,
                          -1,-2,+2, 1,-1,+1, 1,1,+1, -1,2,+2]);

  lamp := CreateMaterial;
  lamp.LightThroughFactor := 0.0001;
  lightObj.Material := lamp;
  lightObj.AddFace([v[0],v[1],v[2],v[3]],true);
  lightObj.AddFace([v[4],v[5],v[1],v[0]],true);
  lightObj.AddFace([v[7],v[6],v[5],v[4]],true);
  lightObj.AddFace([v[3],v[2],v[6],v[7]],true);
  lightObj.AddFace([v[1],v[5],v[6],v[2]],true);
  //lightObj.AddFace([v[4],v[0],v[3],v[7]],true);

  AddPointLight(lightObj.MainPart.Add(5,0,0),4,1,-0.3);
end;

end.

