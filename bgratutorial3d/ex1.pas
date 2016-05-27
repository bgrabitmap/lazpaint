unit ex1;

{$mode objfpc}{$H+}

interface

{ This is a simple 3D objet, a pyramid with a hexagonal base. The object is designed in the constructor.
  The CreateObject method returns an interface to an empty object. An interface is similar to a class
  except you don't have to call Free.
  
  A 3D object has a MainPart property, which contains the vertices describing the objet. It can contains subparts,
  but here it is not the case. Parts can be rotated and scaled relative to their container. The MainPart is rotated
  and scaled relative to the whole scene.
  
  Faces are created using vertices, so that they will follow the movements of these vertices. Here the default color
  SandColor is defined for the whole object.
  
  In order to make it attractive, a lighting is defined. The simplest way is to use a directional light, so that
  you don't have to bother with the coordinates of the light source. 
  
  The background is filled with a gradient. Note that it is a vertical gradient which is very fast to draw, because
  each scanline is filled with one color. }

uses
  Classes, SysUtils, BGRAScene3D,
  BGRAOpenGL, BGRAOpenGL3D,
  BGRABitmapTypes;

type

  { TExample1 }

  TExample1 = class(TBGLScene3D)
    SandColor: TBGRAPixel;
    constructor Create;
    procedure Render; override;
    procedure RenderGL(ACanvas: TBGLCustomCanvas; AMaxZ: single=1000); override;
  end;

implementation

{ TExample1 }

constructor TExample1.Create;
var
  base: array of IBGRAVertex3D;
  top: IBGRAVertex3D;
begin
  inherited Create;

  SandColor := BGRA(255,240,128);

  //create a pyramid
  with CreateObject(SandColor) do
  begin
    top := MainPart.Add(0,-15,0);
    //pyramid base is in a clockwise order if we look the pyramid from under
    base := MainPart.Add([-20,15,-20, 0,15,-30, 20,15,-20, 20,15,20, 0,15,30, -20,15,20]);
    AddFace(base);
    //add four faces, the three vertices are in a clockwise order
    AddFace([base[0],top,base[1]]);
    AddFace([base[1],top,base[2]]);
    AddFace([base[2],top,base[3]]);
    AddFace([base[3],top,base[4]]);
    AddFace([base[4],top,base[5]]);
    AddFace([base[5],top,base[0]]);

    MainPart.Scale(1.3);
    MainPart.RotateYDeg(30);
    MainPart.RotateXDeg(20);
    MainPart.Translate(0,-5,0);
  end;

  //set ambiant lightness to dark (1 is normal lightness, 2 is complete whiteness)
  AmbiantLightness := 0.5;
  //add a directional light from top-left, maximum lightness will be 0.5 + 1 = 1.5
  AddDirectionalLight(Point3D(1,1,1),0.5);

  //we can have high quality antialiasing because it is a simple scene
  RenderingOptions.PerspectiveMode := pmLinearMapping;
end;

procedure TExample1.Render;
begin
  //fill background
  Surface.GradientFill(0,0,Surface.Width,Surface.Height,
          MergeBGRA(SandColor,1,BGRABlack,1),
          MergeBGRA(SandColor,1,BGRABlack,2),
          gtLinear,PointF(0,0),PointF(0,Surface.Height),dmSet);

  inherited Render;
end;

procedure TExample1.RenderGL(ACanvas: TBGLCustomCanvas; AMaxZ: single);
begin
  //fill background
  ACanvas.FillRectLinearColor(0,0,BGLCanvas.Width,BGLCanvas.Height,
          MergeBGRA(SandColor,1,BGRABlack,1),MergeBGRA(SandColor,1,BGRABlack,1),
          MergeBGRA(SandColor,1,BGRABlack,2),MergeBGRA(SandColor,1,BGRABlack,2),
          False);

  inherited RenderGL(ACanvas, AMaxZ);
end;

end.

