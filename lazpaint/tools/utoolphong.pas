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
    FMatrix: TAffineMatrix;
    procedure ShapeChange({%H-}ASender: TObject; ABounds: TRectF); override;
    procedure AssignShapeStyle(AMatrix: TAffineMatrix); override;
    function CreateShape: TVectorShape; override;
    function SlowShape: boolean; override;
  public
    constructor Create(AManager: TToolManager); override;
  end;

implementation

uses ugraph, Graphics, LazPaintType, LCVectorRectShapes, BGRATransform;

{ TToolPhong }

constructor TToolPhong.Create(AManager: TToolManager);
begin
  inherited Create(AManager);
  FMatrix := AffineMatrixIdentity;
end;

procedure TToolPhong.ShapeChange(ASender: TObject; ABounds: TRectF);
var
  posF: TPointF;
begin
  posF := AffineMatrixInverse(FMatrix)*(FShape as TPhongShape).LightPosition;
  Manager.ToolLightPosition := posF;
  inherited ShapeChange(ASender, ABounds);
end;

procedure TToolPhong.AssignShapeStyle(AMatrix: TAffineMatrix);
begin
  inherited AssignShapeStyle(AMatrix);
  FMatrix := AMatrix;
  with (FShape as TPhongShape) do
  begin
    if Manager.ToolShapeType = 'RoundRectangle' then ShapeKind:= pskRoundRectangle else
    if Manager.ToolShapeType = 'Sphere' then ShapeKind := pskHalfSphere else
    if Manager.ToolShapeType = 'Cone' then ShapeKind := pskConeTop else
    if Manager.ToolShapeType = 'VerticalCone' then ShapeKind := pskConeSide else
    if Manager.ToolShapeType = 'VerticalCylinder' then ShapeKind := pskVertCylinder else
    if Manager.ToolShapeType = 'HorizontalCylinder' then ShapeKind := pskHorizCylinder
    else ShapeKind := pskRectangle;
    LightPosition := AMatrix*Manager.ToolLightPosition;
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

