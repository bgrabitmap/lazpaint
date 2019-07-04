unit UToolPhong;

{$mode objfpc}

interface

uses
  Classes, SysUtils, utool, utoolbasic, BGRABitmapTypes, BGRABitmap, BGRAGradients,
  LCVectorOriginal;

type
  { TToolPhong }

  TToolPhong = class(TVectorialTool)
  protected
    procedure ShapeChange({%H-}ASender: TObject; ABounds: TRectF); override;
    procedure AssignShapeStyle; override;
    function CreateShape: TVectorShape; override;
    function SlowShape: boolean; override;
  end;

implementation

uses ugraph, Graphics, LazPaintType, LCVectorRectShapes;

{ TToolPhong }

procedure TToolPhong.ShapeChange(ASender: TObject; ABounds: TRectF);
begin
  with (FShape as TPhongShape) do
    Manager.ToolLightPosition := Point(round(LightPosition.X),round(LightPosition.Y));
  inherited ShapeChange(ASender, ABounds);
end;

procedure TToolPhong.AssignShapeStyle;
begin
  inherited AssignShapeStyle;
  with (FShape as TPhongShape) do
  begin
    if Manager.ToolShapeType = 'RoundRectangle' then ShapeKind:= pskRoundRectangle else
    if Manager.ToolShapeType = 'Sphere' then ShapeKind := pskHalfSphere else
    if Manager.ToolShapeType = 'Cone' then ShapeKind := pskConeTop else
    if Manager.ToolShapeType = 'VerticalCone' then ShapeKind := pskConeSide else
    if Manager.ToolShapeType = 'VerticalCylinder' then ShapeKind := pskVertCylinder else
    if Manager.ToolShapeType = 'HorizontalCylinder' then ShapeKind := pskHorizCylinder
    else ShapeKind := pskRectangle;
    LightPosition := PointF(Manager.ToolLightPosition.X,Manager.ToolLightPosition.Y);
    ShapeAltitudePercent := Manager.ToolShapeAltitude;
    BorderSizePercent:= Manager.ToolShapeBorderSize;
  end;
end;

function TToolPhong.CreateShape: TVectorShape;
begin
  result := TPhongShape.Create(nil);
end;

function TToolPhong.SlowShape: boolean;
begin
  Result:= true;
end;

initialization

  RegisterTool(ptPhong,TToolPhong);

end.

