unit ex2;

{$mode objfpc}{$H+}

interface

{ This example draws a box. It comes with different flavors :
  - e2lNone : without lighting but with water
  - e2lLightness : with lighting and water
  - e2lColored : with two lights of different colors, but without water
 
 In this example, faces are textured. Each texture is generated with utexture unit. 
 Because we want to have a shiny effect on the box, we need to create different material.
 The parameter of CreateMaterial is the exponent of the specular light, which is the
 concentration of the reflected beam. The lamps are not shiny, but we must see some of the 
 light through them, that's why the LightThroughFactor is set to a small non zero number.
 
 The lighting normals are the direction considered to be orthogonal to the surface. The box
 uses lnFace, which means that each face is considered to be flat. On the contrary, lamps
 are rounded, so their lighting normals are lnVertex, which means that the surface is considered
 to be rounded between faces, using vertices as a reference. The example 3 show the difference
 between these modes.
}

uses
  Classes, SysUtils, BGRAScene3D, BGRABitmap, BGRABitmapTypes,
  BGRAOpenGL3D;

type
  TExample2Lighting = (e2lNone,e2lLightness,e2lColored);

  { TExample2 }

  TExample2 = class(TBGLScene3D)
  private
    water,wood,vWood: TBGRABitmap;
    box,ground,light1,light2: IBGRAObject3D;
    alpha: integer;
    cury: single;
    FLighting: TExample2Lighting;
    procedure CreateScene;
    procedure ApplyTexCoord(face: IBGRAFace3D; Times: integer = 1);
    procedure SetLighting(AValue: TExample2Lighting);
  public
    constructor Create(ALighting: TExample2Lighting);
    procedure Elapse;
    destructor Destroy; override;
    property Lighting: TExample2Lighting read FLighting write SetLighting;
  end;

implementation

uses utexture;

const texSize = 256;

{ TExample2 }

constructor TExample2.Create(ALighting: TExample2Lighting);
begin
  inherited Create;

  //create textures
  water := CreateWaterTexture(texSize,texSize);
  vWood := CreateVerticalWoodTexture(texSize,texSize);
  wood := CreateWoodTexture(texSize,texSize);

  FLighting:= ALighting;
  CreateScene;
end;

procedure TExample2.Elapse;
var dy: single;
begin
  if light1 <> nil then light1.MainPart.RotateYDeg(1,False);
  if light2 <> nil then light2.MainPart.RotateYDeg(-1.3,False);
  if ground <> nil then
  begin
    dy := cos(alpha*Pi/180)*0.05;
    cury += dy;
    ground.MainPart.Translate(0,dy,0,False);
    ViewPoint := Point3D(ViewPoint.x,-40+cury,ViewPoint.z);
    LookAt(Point3D(0,cury,0),Point3D(0,-1,0));
    inc(alpha);
    if alpha = 360 then alpha := 0;
  end;
end;

procedure TExample2.CreateScene;
var
  base,v: array of IBGRAVertex3D;
  lamp,shiny: IBGRAMaterial3D;
begin
  Clear;

  shiny := CreateMaterial(500);
  lamp := CreateMaterial;
  lamp.LightThroughFactor := 0.01;

  //create wooden box
  box := CreateObject(vWood);
  with box do
  begin
    v := MainPart.Add([-1,-1,-1, 1,-1,-1, 1,1,-1, -1,1,-1,
                       -1,-1,+1, 1,-1,+1, 1,1,+1, -1,1,+1]);

    ApplyTexCoord(AddFace([v[0],v[1],v[2],v[3]]));
    ApplyTexCoord(AddFace([v[4],v[5],v[1],v[0]],wood));
    ApplyTexCoord(AddFace([v[5],v[4],v[7],v[6]]));
    ApplyTexCoord(AddFace([v[3],v[2],v[6],v[7]],wood));
    ApplyTexCoord(AddFace([v[1],v[5],v[6],v[2]]));
    ApplyTexCoord(AddFace([v[4],v[0],v[3],v[7]]));

    MainPart.Scale(20);
  end;

  DefaultLightingNormal:= lnFace;

  if Lighting = e2lColored then
  begin
    ViewPoint := Point3D(0,0,-150);
    AmbiantLightColor := BGRA(192,192,192);
    box.Material := shiny;

    //lights
    light1 := CreateHalfSphere(10, BGRA(255,128,0), 8,8);
    with light1 do
    begin
      AddPointLight(MainPart.Add(0,0,-5),60,BGRA(255,128,0),0);
      MainPart.Translate(-100,-50,0);
      MainPart.LookAt(Point3D(0,0,0),Point3D(0,-1,0));
      Material := lamp;
      LightingNormal := lnVertex;
    end;
    light2 := CreateHalfSphere(10, BGRA(0,128,255), 8,8);
    with light2 do
    begin
      AddPointLight(MainPart.Add(0,0,-5),100,BGRA(0,128,255),0);
      MainPart.Translate(50,0,-100);
      MainPart.LookAt(Point3D(0,0,0),Point3D(0,-1,0));
      Material := lamp;
      LightingNormal := lnVertex;
    end;
  end else
  begin
    //create ground
    ground := CreateObject(water);
    if Lighting = e2lLightness then
    begin
      with ground do
      begin
        base := MainPart.Add([-50,0,-50, -50,0,50, 50,0,50, 50,0,-50]);
        ApplyTexCoord(AddFace(base,True),2);
      end;
      ViewPoint := Point3D(-40,-40,-100);
      AmbiantLightness := 0.25;
      with CreateObject do
        AddPointLight(MainPart.Add(-100,-80,0),100,1.25, -0.15);
    end else
    begin
      AmbiantLightness := 1;
      with ground do
      begin
        base := MainPart.Add([-50,0,-50, -50,0,50, 50,0,50, 50,0,-50]);
        MainPart.Scale(2);
        ApplyTexCoord(AddFace(base,True),2);
      end;
      ViewPoint := Point3D(0,-40,-120);
    end;
    RenderingOptions.PerspectiveMode:= pmZBuffer;
  end;

  RenderingOptions.TextureInterpolation := false;
end;

procedure TExample2.ApplyTexCoord(face: IBGRAFace3D; Times: integer);
begin
  with face do
  begin
    TexCoord[0] := PointF(0,0);
    TexCoord[1] := PointF(texSize*Times-1,0);
    TexCoord[2] := PointF(texSize*Times-1,texSize*Times-1);
    TexCoord[3] := PointF(0,texSize*Times-1);
  end;
end;

procedure TExample2.SetLighting(AValue: TExample2Lighting);
begin
  if FLighting=AValue then Exit;
  FLighting:=AValue;
  CreateScene;
end;

destructor TExample2.Destroy;
begin
  water.free;
  wood.free;
  vWood.free;
  inherited Destroy;
end;

end.

