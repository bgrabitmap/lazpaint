// SPDX-License-Identifier: GPL-3.0-only
unit UToolPhong;

{$mode objfpc}

interface

uses
  Classes, SysUtils, UTool, UToolVectorial, BGRABitmapTypes, BGRABitmap, BGRAGradients,
  LCVectorOriginal;

type
  { TToolPhong }

  TToolPhong = class(TVectorialTool)
  protected
    FMatrix: TAffineMatrix;
    procedure ShapeChange({%H-}ASender: TObject; ABounds: TRectF; ADiff: TVectorShapeDiff); override;
    procedure AssignShapeStyle(AMatrix: TAffineMatrix; AAlwaysFit: boolean); override;
    function ShapeClass: TVectorShapeAny; override;
  public
    constructor Create(AManager: TToolManager); override;
    function GetContextualToolbars: TContextualToolbars; override;
  end;

implementation

uses ugraph, Graphics, LazPaintType, LCVectorRectShapes, BGRATransform;

{ TToolPhong }

constructor TToolPhong.Create(AManager: TToolManager);
begin
  inherited Create(AManager);
  FMatrix := AffineMatrixIdentity;
end;

function TToolPhong.GetContextualToolbars: TContextualToolbars;
begin
  Result:= [ctBackFill,ctPhong,ctAltitude];
end;

procedure TToolPhong.ShapeChange(ASender: TObject; ABounds: TRectF; ADiff: TVectorShapeDiff);
var
  posF: TPointF;
begin
  posF := AffineMatrixInverse(FMatrix)*(FShape as TPhongShape).LightPosition;
  Manager.LightPosition := posF;
  inherited ShapeChange(ASender, ABounds, ADiff);
end;

procedure TToolPhong.AssignShapeStyle(AMatrix: TAffineMatrix; AAlwaysFit: boolean);
begin
  inherited AssignShapeStyle(AMatrix, AAlwaysFit);
  FMatrix := AMatrix;
  with (FShape as TPhongShape) do
  begin
    ShapeKind := Manager.PhongShapeKind;
    LightPosition := AMatrix*Manager.LightPosition;
    ShapeAltitudePercent := Manager.PhongShapeAltitude;
    BorderSizePercent:= Manager.PhongShapeBorderSize;
  end;
end;

function TToolPhong.ShapeClass: TVectorShapeAny;
begin
  result := TPhongShape;
end;

initialization

  RegisterTool(ptPhong,TToolPhong);

end.

