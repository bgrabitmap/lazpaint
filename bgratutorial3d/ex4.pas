unit ex4;

{$mode objfpc}{$H+}

interface

{ This example demonstrate how to load 3D objects and show them with
  appropriate material and color. 
  
  The LoadObjectFromFile returns an 3D object defined by an OBJ file.
  Some things are ignored, like texture information or normal information.
  But when a material name is used, the function UseMaterial is called.  
  So here it is overriden in order to give the best material possible.
  
  Some objects have specific effects. For example, the teapot is defined
  as Biface so that the reflected light is computed outside and inside
  the glass. 
  
  The helicopter is divided into two parts. The upper part contains the
  rotor in order to rotate it, so that the helicopter seems to fly.
  
  The lamp contains in fact 4 lamps, and so 4 light sources are created. 
  
  }

uses
  Classes, SysUtils, BGRAScene3D, BGRABitmapTypes,
  BGRAOpenGL3D, BGRAOpenGL;

type

  { TExample4 }

  TExample4 = class(TBGLScene3D)
  protected
    lamp,shiny,reflect: IBGRAMaterial3D;
    rotated: IBGRAPart3D;
    rotateCenter: TPoint3D;
    message: string;
    procedure UseMaterial(materialname: string; face: IBGRAFace3D); override;
    procedure CreateScene;
  public
    procedure Render; override;
    procedure Elapse;
    procedure RenderGL(ACanvas: TBGLCustomCanvas; AMaxZ: single=1000); override;
    procedure NextModel;
    constructor Create;
  end;

implementation

uses BGRATextFX;

var
    numObj: integer= 3;

const
   objList: array[0..9] of string = ( 'ciseau.obj',
      'fourche.obj', 'pelle.obj', 'helico.obj', 'mario.obj', 'helice.obj',
      'lampe.obj', 'teapot.obj', 'roue.obj', 'trumpet.obj');

{ TExample4 }

constructor TExample4.Create;
begin
  inherited Create;

  //create shiny material using saturation of diffusion (1.3 .. 1.5)
  shiny := CreateMaterial;
  shiny.SaturationLow := 1.3;
  shiny.SaturationHigh := 1.5;
  reflect := CreateMaterial(50);
  lamp := CreateMaterial;
  lamp.LightThroughFactor := 0.05;

  CreateScene;
end;

procedure TExample4.UseMaterial(materialname: string; face: IBGRAFace3D);
var color : TBGRAPixel;
begin
  if (materialname = 'globes') then
  begin
    color := BGRA(255,240,220);
    face.Material := lamp;
  end else
  if (materialname = 'bone') then
  begin
    color := BGRA(255,240,220);
  end else
  if (materialname = 'bronze') then
  begin
    color := CSSSaddleBrown;
    face.Material := reflect;
  end else
  if materialname = 'grey' then
  begin
    color := BGRA(230,192,80);
    face.Material := shiny;
  end else
  begin
     color := StrToBGRA(materialname);
     if (objList[numObj] <> 'helice.obj') and (color.red = color.green) and (color.green = color.blue) then
     begin
       if (color.alpha <> 255) or (color.red = 0) then
         face.Material := reflect
       else
         face.Material := shiny;
     end else
       if color.alpha <> 255 then
         face.Material := reflect;
  end;
  face.SetColor( color );
end;

procedure TExample4.CreateScene;
var obj: IBGRAObject3D;
    r: single;
    i: integer;
    filename: string;
begin
  Clear;

  filename := 'obj'+PathDelim+objList[numObj];
  if not fileexists(filename) and fileexists('..'+PathDelim+'..'+PathDelim+filename) then
    filename := '..'+PathDelim+'..'+PathDelim+filename;
  if not FileExists(filename) then
    begin
      message := 'File not found : '+ filename;
      exit;
    end;

  obj := LoadObjectFromFile(filename, objList[numObj] <> 'teapot.obj');

  if objList[numObj] = 'helico.obj' then
  begin
    with obj.MainPart do
    begin
      rotated := CreatePart;
      rotateCenter := Point3D(0,0,0);
      for i := VertexCount-1 downto 0 do
          if (Vertex[i].SceneCoord.y >= 22.2) then
          begin
            rotated.Add(Vertex[i]);
            rotateCenter += Vertex[i].SceneCoord;
          end;
      rotateCenter *= 1/rotated.VertexCount;
      obj.SeparatePart(rotated);
      obj.MainPart.Scale(2,2,2);
    end;
  end else
    rotated := nil;

  obj.LightingNormal := lnVertex;
  if objList[numObj] = 'teapot.obj' then
    for i := 0 to obj.FaceCount-1 do
      obj.Face[i].Biface := true;

  with obj.MainPart.BoundingBox do
    obj.MainPart.Translate((min+max)*(-1/2), False);
  r := obj.MainPart.Radius;
  if r <> 0 then obj.MainPart.Scale(40/r, False);
  if objList[numObj] = 'lampe.obj' then
  begin
    obj.MainPart.RotateXDeg(180, False);
    obj.MainPart.Scale(1.5,1.5,1.5);
  end else
  if objList[numObj] = 'mario.obj' then
    obj.MainPart.RotateXDeg(90, False)
  else
  begin
    obj.MainPart.RotateXDeg(180-20, False);
    obj.MainPart.RotateYDeg(-20, False);
    if objList[numObj] = 'trumpet.obj' then
       obj.MainPart.Scale(2,2,2,False);
  end;

  if objList[numObj] = 'lampe.obj' then
  begin
    AmbiantLightness := 0.7;
    AddPointLight(obj.MainPart.Add(0,7.7,0),10);
    AddPointLight(obj.MainPart.Add(1.9,6.5,0),10);
    AddPointLight(obj.MainPart.Add(-0.9,6.5,1.5),10);
    AddPointLight(obj.MainPart.Add(-0.9,6.5,-1.7),10);
  end
  else
  begin
    //set ambiant lightness to dark (1 is normal lightness)
    AmbiantLightness := 0.5;
    if objList[numObj] = 'helice.obj' then
      AddDirectionalLight(Point3D(1,1,1),0.75,-0.5)
    else
      AddDirectionalLight(Point3D(1,1,1),1,-0.5); //add a directional light from top-left, maximum lightness will be 0.5 + 1 = 1.5
  end;

  RenderingOptions.PerspectiveMode:= pmZBuffer;
  if objList[numObj] = 'helice.obj' then
    RenderingOptions.LightingInterpolation := liAlwaysHighQuality
  else
    RenderingOptions.LightingInterpolation := liSpecularHighQuality;
end;

procedure TExample4.Render;
var fx: TBGRATextEffect;
begin
  if objList[numObj] = 'teapot.obj' then
    Surface.GradientFill(0,0,Surface.Width,Surface.Height,BGRABlack,BGRA(70,100,100),gtLinear,PointF(0,0),PointF(0,Surface.Height),dmSet) else
  if objList[numObj] = 'lampe.obj' then
    Surface.Fill(BGRA(0,0,60));

  inherited Render;

  if message <> '' then
  begin
    fx := TBGRATextEffect.Create(message,'Arial',20,True);
    fx.DrawOutline(Surface,Surface.Width div 2,Surface.Height div 2-fx.TextHeight div 2,BGRABlack,taCenter);
    fx.Draw(Surface,Surface.Width div 2,Surface.Height div 2-fx.TextHeight div 2,BGRAWhite,taCenter);
    fx.Free;
  end else
    Surface.TextOut(Surface.Width,0,objList[numObj],BGRAWhite,taRightJustify);
end;

procedure TExample4.Elapse;
begin
  if rotated <> nil then
  begin
    rotated.Translate(-rotateCenter,false);
    rotated.RotateYDeg(20,False);
    rotated.Translate(rotateCenter,false);
  end;
end;

procedure TExample4.RenderGL(ACanvas: TBGLCustomCanvas; AMaxZ: single);
begin
  if objList[numObj] = 'teapot.obj' then
    ACanvas.FillRectLinearColor(0,0,BGLCanvas.Width,BGLCanvas.Height,
       BGRABlack,BGRABlack,
       BGRA(70,100,100),BGRA(70,100,100),
       False) else
  if objList[numObj] = 'lampe.obj' then
    ACanvas.Fill(BGRA(0,0,60));

  inherited RenderGL(ACanvas, AMaxZ);
end;

procedure TExample4.NextModel;
begin
  inc(numObj);
  if numObj = length(objList) then numObj := 0;

  CreateScene;
end;

end.

