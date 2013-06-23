unit ucube3d;

{$mode objfpc}

interface

uses
  Classes, SysUtils, BGRAScene3D, BGRABitmapTypes;

type

  { TCubeScene3D }

  TCubeScene3D = class(TBGRAScene3D)
    cube: IBGRAObject3D;
    constructor Create;
    procedure SetCubeTexture(ATexture: TBGRACustomBitmap);
    procedure SetCubeTexture(ATexture: IBGRAScanner; tx,ty: single);
  end;

implementation

{ TCubeScene3D }

constructor TCubeScene3D.Create;
var v: arrayOfIBGRAVertex3D;
begin
  inherited Create;

  cube := CreateObject;
  v := cube.MainPart.Add([-1,-1,-1, 1,-1,-1, 1,1,-1, -1,1,-1,
                          -1,-1,+1, 1,-1,+1, 1,1,+1, -1,1,+1]);

  cube.AddFace([v[0],v[1],v[2],v[3]],BGRA(255,0,0));
  cube.AddFace([v[4],v[5],v[1],v[0]],BGRA(128,160,255));
  cube.AddFace([v[7],v[6],v[5],v[4]],BGRA(96,224,0));
  cube.AddFace([v[3],v[2],v[6],v[7]],BGRA(192,0,255));
  cube.AddFace([v[1],v[5],v[6],v[2]],BGRA(255,192,0));
  cube.AddFace([v[4],v[0],v[3],v[7]],BGRAWhite);

  cube.MainPart.Scale(20);

  AmbiantLightness := 0.5;
  AddDirectionalLight(Point3D(1,1,1),1,-0.5);
end;

procedure TCubeScene3D.SetCubeTexture(ATexture: TBGRACustomBitmap);
begin
  SetCubeTexture(ATexture,ATexture.Width-1,ATexture.Height-1);
end;

procedure TCubeScene3D.SetCubeTexture(ATexture: IBGRAScanner; tx, ty: single);
var
  i: Integer;
begin
  for i := 0 to cube.FaceCount-1 do
  with cube.Face[i] do
  begin
    Texture := ATexture;
    TexCoord[0] := PointF(0,0);
    TexCoord[1] := PointF(tx,0);
    TexCoord[2] := PointF(tx,ty);
    TexCoord[3] := PointF(0,ty);
  end;
end;

end.

