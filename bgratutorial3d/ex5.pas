unit ex5;

{$mode objfpc}{$H+}

interface

{ This example is just a box on a grass ground. It aims at showing
  how to create a first-person view. Scene rotation is thus
  handled differently in umain }

uses
  Classes, SysUtils, BGRAScene3D, BGRABitmap, BGRABitmapTypes,
  BGRAOpenGL3D, BGRAOpenGL;

type
  { TExample5 }

  TExample5 = class(TBGLScene3D)
    grass,wood,vWood: TBGRABitmap;
    box,ground,light1,light2: IBGRAObject3D;

    constructor Create;
    procedure ApplyTexCoord(face: IBGRAFace3D; Times: integer = 1);
    procedure Render; override;
    procedure RenderGL(ACanvas: TBGLCustomCanvas; AMaxZ: single=1000); override;
    destructor Destroy; override;
  end;

implementation

uses utexture;

const texSize = 256;

{ TExample5 }

constructor TExample5.Create;
var
  base,v: array of IBGRAVertex3D;
begin
  inherited Create;

  //create textures
  grass := CreateGrassTexture(texSize,texSize);
  vWood := CreateVerticalWoodTexture(texSize,texSize);
  wood := CreateWoodTexture(texSize,texSize);

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

    MainPart.Translate(0,-1,0,False);
    MainPart.Scale(20,False);
    MainPart.Translate(0,0,200,FAlse);
  end;

  //create ground
  ground := CreateObject(grass);
  AmbiantLightness := 0.5;
  with ground do
  begin
    base := MainPart.Add([-1,0,-1, -1,0,1, 1,0,1, 1,0,-1]);
    MainPart.Scale(1000);
    ApplyTexCoord(AddFace(base,True),10);
  end;

  Camera.ViewPoint := Point3D(0,-20,0);
  Camera.LookAt(Point3D(0,-20,20),Point3D(0,-1,0));

  RenderingOptions.PerspectiveMode:= pmZBuffer;
  RenderingOptions.TextureInterpolation := false;
end;

procedure TExample5.ApplyTexCoord(face: IBGRAFace3D; Times: integer);
begin
  with face do
  begin
    TexCoord[0] := PointF(0,0);
    TexCoord[1] := PointF(texSize*Times-1,0);
    TexCoord[2] := PointF(texSize*Times-1,texSize*Times-1);
    TexCoord[3] := PointF(0,texSize*Times-1);
  end;
end;

procedure TExample5.Render;
begin
  Surface.GradientFill(0,0,Surface.Width,Surface.Height,
          CSSSkyBlue,
          MergeBGRA(CSSBlue,CSSSkyBlue),
          gtLinear,PointF(0,0),PointF(0,Surface.Height),dmSet,
          False);
  inherited Render;
end;

procedure TExample5.RenderGL(ACanvas: TBGLCustomCanvas; AMaxZ: single);
begin
  ACanvas.FillRectLinearColor(0,0,BGLCanvas.Width,BGLCanvas.Height,
          CSSSkyBlue,CSSSkyBlue,
          MergeBGRA(CSSBlue,CSSSkyBlue),MergeBGRA(CSSBlue,CSSSkyBlue),
          False);
  inherited RenderGL(ACanvas, AMaxZ);
end;

destructor TExample5.Destroy;
begin
  grass.free;
  wood.free;
  vWood.free;
  inherited Destroy;
end;

end.

