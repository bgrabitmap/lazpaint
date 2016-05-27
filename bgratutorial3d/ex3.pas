unit ex3;

{$mode objfpc}{$H+}

interface

{ This example is a cylinder. It aims at showing the difference 
  between lighting normals modes. 
  
  The lighting normals are the direction considered to be orthogonal 
  to the surface. The box uses lnFace, which means that each face is 
  considered to be flat. On the contrary, lamps   are rounded, so 
  their lighting normals are lnVertex, which means that the surface 
  is considered to be rounded between faces, using vertices as a 
  reference.  
  
  Depending on the value of DefaultLightingNormal, the cylinder will 
  look polygonal or rounded. The value lnFaceVertexMix is an 
  intermediate between Face and Vertex, which allows to have some 
  light diffusion effect while keeping an angle between faces. 
  
  The SaturationLow and SaturationHigh properties of the material allows
  to create a shiny effect without actually computing reflected light. 
  Simply when a pixel is very bright, it turns into white, as if we
  could see the reflection of some white beam. }

uses
  Classes, SysUtils, BGRAScene3D, BGRABitmapTypes,
  BGRAOpenGL3D;

type

  { TExample3 }

  TExample3 = class(TBGLScene3D)
    constructor Create;
  end;

implementation

uses BGRAMatrix3D;

{ TExample3 }

constructor TExample3.Create;
const
  radius = 20;
  topY = -20;
  bottomY = 20;
  precision = 40;
var
  bottom,top: array[1..precision] of IBGRAVertex3D;
  topCoord,bottomCoord: TPoint3D;
  rotateMatrix: TMatrix3D;
  i,j: Integer;
begin
  inherited Create;

  DefaultMaterial.SpecularIndex := 50;
  DefaultMaterial.AutoSpecularColor:= true;

  //create a cylinder
  with CreateObject(BGRA(0,0,255)) do
  begin
    for j := 1 to 2 do
    begin
      //top and bottom coordinates
      topCoord := Point3D(radius,topY,0);
      bottomCoord := Point3D(radius,bottomY,0);
      //rotating around the Y axis
      rotateMatrix := MatrixRotateY(2*Pi/precision);
      //create the vertices
      for i := 1 to precision do
      begin
        //store in reverse order the second time
        if j = 2 then
        begin
          top[precision+1-i] := MainPart.Add(topCoord);
          bottom[precision+1-i] := MainPart.Add(bottomCoord);
        end
        else
        begin
          top[i] := MainPart.Add(topCoord);
          bottom[i] := MainPart.Add(bottomCoord);
        end;
        topCoord := rotateMatrix*topCoord;
        bottomCoord := rotateMatrix*bottomCoord;
      end;

      //add faces : the second time, there will be in the opposite direction because of the reverse order
      for i := 1 to precision do
        AddFace([top[i],top[(i mod precision)+1],bottom[(i mod precision)+1],bottom[i]]);
    end;

    //apply a rotation to show the top of the cylinder
    MainPart.RotateXDeg(30);
  end;

  //set ambiant lightness to dark (1 is normal lightness, 2 is complete whiteness)
  AmbiantLightness := 0.5;
  //add a directional light from top-left, maximum lightness will be 0.5 + 1 = 1.5
  AddDirectionalLight(Point3D(1,1,1),1,-0.5);
end;

end.

