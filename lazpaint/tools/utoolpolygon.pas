// SPDX-License-Identifier: GPL-3.0-only
unit UToolPolygon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UTool, UToolVectorial, BGRABitmap, BGRABitmapTypes,
  LCVectorOriginal, LCLType;

const
  EasyBezierMinimumDotProduct = 0.5;

type
  { TToolRectangle }

  TToolRectangle = class(TVectorialTool)
  protected
    function ShapeClass: TVectorShapeAny; override;
  end;

  { TToolEllipse }

  TToolEllipse = class(TVectorialTool)
  protected
    function ShapeClass: TVectorShapeAny; override;
    function GetGridMatrix: TAffineMatrix; override;
  end;

  { TToolPolygon }

  TToolPolygon = class(TVectorialTool)
  protected
    class var RightClickHintShown: boolean;
    class var RemovePointHintShown: boolean;
    initiallyClosed : boolean;
    function ShapeClass: TVectorShapeAny; override;
    function CreateShape: TVectorShape; override;
    function ShouldCloseShape: boolean; virtual;
    procedure UpdateManagerCloseShape({%H-}AClose: boolean); virtual;
    procedure AssignShapeStyleClosed(AShape: TVectorShape); virtual;
    procedure AssignShapeStyle(AMatrix: TAffineMatrix; AAlwaysFit: boolean); override;
    procedure UpdateUserMode; virtual;
    procedure ShapeValidated; override;
    function DoToolKeyDown(var key: Word): TRect; override;
    function RoundCoordinate(constref ptF: TPointF): TPointF; override;
  public
    class procedure ForgetHintShown;
    function ToolUp: TRect; override;
    function ToolKeyPress(var key: TUTF8Char): TRect; override;
  end;

  { TToolPolyline }

  TToolPolyline = class(TToolPolygon)
  protected
    function CreateShape: TVectorShape; override;
    function ShouldCloseShape: boolean; override;
    procedure AssignShapeStyleClosed(AShape: TVectorShape); override;
    procedure UpdateManagerCloseShape({%H-}AClose: boolean); override;
    function GetManagerShapeOptions: TShapeOptions; override;
    function HasBrush: boolean; override;
  public
    function HasPen: boolean; override;
    function GetContextualToolbars: TContextualToolbars; override;
  end;

  { TToolSpline }

  TToolSpline = class(TToolPolygon)
  private
    FCurrentMode: TToolSplineMode;
    FNextCurveMode: TEasyBezierCurveMode;
    FCurveModeHintShown: Boolean;
    function GetCurrentMode: TToolSplineMode;
    procedure SetCurrentMode(AValue: TToolSplineMode);
  protected
    function ShapeClass: TVectorShapeAny; override;
    function CreateShape: TVectorShape; override;
    procedure AssignShapeStyle(AMatrix: TAffineMatrix; AAlwaysFit: boolean); override;
    procedure UpdateUserMode; override;
  public
    constructor Create(AManager: TToolManager); override;
    function ToolKeyPress(var key: TUTF8Char): TRect; override;
    property CurrentMode: TToolSplineMode read GetCurrentMode write SetCurrentMode;
  end;

  { TToolOpenedCurve }

  TToolOpenedCurve = class(TToolSpline)
  protected
    function ShouldCloseShape: boolean; override;
    procedure UpdateManagerCloseShape({%H-}AClose: boolean); override;
    function GetManagerShapeOptions: TShapeOptions; override;
    function HasBrush: boolean; override;
  public
    function HasPen: boolean; override;
    function GetContextualToolbars: TContextualToolbars; override;
  end;

implementation

uses LazPaintType, LCVectorRectShapes, LCVectorPolyShapes, BGRATransform;

{ TToolOpenedCurve }

function TToolOpenedCurve.ShouldCloseShape: boolean;
begin
  result := false;
end;

procedure TToolOpenedCurve.UpdateManagerCloseShape(AClose: boolean);
begin
  //nothing
end;

function TToolOpenedCurve.GetManagerShapeOptions: TShapeOptions;
begin
  Result:= manager.ShapeOptions - [toFillShape] + [toDrawShape];
end;

function TToolOpenedCurve.HasPen: boolean;
begin
  Result:= true;
end;

function TToolOpenedCurve.HasBrush: boolean;
begin
  Result:= false;
end;

function TToolOpenedCurve.GetContextualToolbars: TContextualToolbars;
begin
  Result:= inherited GetContextualToolbars - [ctShape, ctCloseShape];
end;

{ TToolPolyline }

function TToolPolyline.CreateShape: TVectorShape;
begin
  Result:=inherited CreateShape;
  inherited AssignShapeStyleClosed(Result);
end;

function TToolPolyline.ShouldCloseShape: boolean;
begin
  result := false;
end;

procedure TToolPolyline.AssignShapeStyleClosed(AShape: TVectorShape);
begin
  //nothing
end;

procedure TToolPolyline.UpdateManagerCloseShape(AClose: boolean);
begin
  //nothing
end;

function TToolPolyline.GetManagerShapeOptions: TShapeOptions;
begin
  Result:= manager.ShapeOptions - [toFillShape] + [toDrawShape];
end;

function TToolPolyline.HasPen: boolean;
begin
  Result:= true;
end;

function TToolPolyline.HasBrush: boolean;
begin
  Result:= false;
end;

function TToolPolyline.GetContextualToolbars: TContextualToolbars;
begin
  Result:= inherited GetContextualToolbars - [ctShape, ctCloseShape];
end;

{ TToolEllipse }

function TToolEllipse.ShapeClass: TVectorShapeAny;
begin
  result := TEllipseShape;
end;

function TToolEllipse.GetGridMatrix: TAffineMatrix;
begin
  Result:= AffineMatrixScale(0.5, 0.5);
end;

{ TToolRectangle }

function TToolRectangle.ShapeClass: TVectorShapeAny;
begin
  result := TRectShape;
end;

{ TToolSpline }

function TToolSpline.GetCurrentMode: TToolSplineMode;
begin
  if Assigned(FShape) then
    FCurrentMode := ToolSplineModeFromShape(FShape);
  result := FCurrentMode;
end;

procedure TToolSpline.SetCurrentMode(AValue: TToolSplineMode);
begin
  if FCurrentMode = AValue then exit;
  FCurrentMode := AValue;
  UpdateUserMode;
end;

function TToolSpline.ShapeClass: TVectorShapeAny;
begin
  result := TCurveShape;
end;

procedure TToolSpline.UpdateUserMode;
var
  c: TCurveShape;
begin
  if FShape = nil then exit;
  if FQuickDefine then
  begin
    FShape.Usermode := vsuCreate;
    exit;
  end;
  c := TCurveShape(FShape);
  case FCurrentMode of
  tsmMovePoint: if not (c.Usermode in [vsuEdit,vsuCreate]) then c.Usermode := vsuEdit;
  tsmCurveModeAuto: if c.Usermode <> vsuCreate then c.Usermode := vsuCurveSetAuto else
                    if c.PointCount > 1 then c.CurveMode[c.PointCount-2] := cmAuto;
  tsmCurveModeAngle: if c.Usermode <> vsuCreate then c.Usermode := vsuCurveSetAngle else
                     if c.PointCount > 1 then c.CurveMode[c.PointCount-2] := cmAngle;
  tsmCurveModeSpline: if c.Usermode <> vsuCreate then c.Usermode := vsuCurveSetCurve else
                      if c.PointCount > 1 then c.CurveMode[c.PointCount-2] := cmCurve;
  end;
end;

function TToolSpline.CreateShape: TVectorShape;
begin
  result := inherited CreateShape;
  TCurveShape(result).CosineAngle:= EasyBezierMinimumDotProduct;
  if not FCurveModeHintShown then
  begin
    Manager.ToolPopup(tpmCurveModeHint);
    FCurveModeHintShown := true;
  end;
end;

procedure TToolSpline.AssignShapeStyle(AMatrix: TAffineMatrix; AAlwaysFit: boolean);
begin
  inherited AssignShapeStyle(AMatrix, AAlwaysFit);
  TCurveShape(FShape).SplineStyle:= Manager.SplineStyle;
end;

constructor TToolSpline.Create(AManager: TToolManager);
begin
  inherited Create(AManager);
  FNextCurveMode := cmAuto;
end;

function TToolSpline.ToolKeyPress(var key: TUTF8Char): TRect;
begin
  if (Key='z') or (Key = 'Z') then
  begin
    CurrentMode:= tsmMovePoint;
    result := OnlyRenderChange;
    Key := #0;
  end else
  begin
    Result:=inherited ToolKeyPress(key);
    if Key='x' then Key := #0;
  end;
end;

{ TToolPolygon }

function TToolPolygon.ShapeClass: TVectorShapeAny;
begin
  result := TPolylineShape;
end;

function TToolPolygon.CreateShape: TVectorShape;
begin
  result := inherited CreateShape;
  initiallyClosed := ShouldCloseShape;
  if not RightClickHintShown then
  begin
    Manager.ToolPopup(tpmRightClickFinishShape);
    RightClickHintShown := true;
  end;
end;

function TToolPolygon.ShouldCloseShape: boolean;
begin
  result := toCloseShape in Manager.ShapeOptions;
end;

procedure TToolPolygon.UpdateManagerCloseShape(AClose: boolean);
var
  opt: TShapeOptions;
begin
  opt := Manager.ShapeOptions;
  if AClose then
    include(opt, toCloseShape)
  else
    exclude(opt, toCloseShape);
  Manager.ShapeOptions:= opt;
end;

procedure TToolPolygon.AssignShapeStyleClosed(AShape: TVectorShape);
begin
  (AShape as TCustomPolypointShape).Closed := ShouldCloseShape;
end;

procedure TToolPolygon.AssignShapeStyle(AMatrix: TAffineMatrix; AAlwaysFit: boolean);
begin
  inherited AssignShapeStyle(AMatrix, AAlwaysFit);
  AssignShapeStyleClosed(TCustomPolypointShape(FShape));
  TCustomPolypointShape(FShape).ArrowStartKind := Manager.ArrowStart;
  TCustomPolypointShape(FShape).ArrowEndKind := Manager.ArrowEnd;
  TCustomPolypointShape(FShape).ArrowSize := Manager.ArrowSize;
  TCustomPolypointShape(FShape).LineCap:= Manager.LineCap;
  UpdateUserMode;
end;

procedure TToolPolygon.UpdateUserMode;
begin
  if FShape = nil then exit;
  if FQuickDefine then FShape.Usermode := vsuCreate;
end;

procedure TToolPolygon.ShapeValidated;
begin
  inherited ShapeValidated;
  if not initiallyClosed then UpdateManagerCloseShape(False);
end;

function TToolPolygon.ToolUp: TRect;
begin
  Result:=inherited ToolUp;
  if Assigned(FShape) then
  begin
    UpdateManagerCloseShape((FShape as TCustomPolypointShape).Closed);
    if not RemovePointHintShown and ((FShape as TCustomPolypointShape).ValidatedPointCount >= 3) then
    begin
      Manager.ToolPopup(tpmBackspaceRemoveLastPoint);
      RemovePointHintShown := true;
    end;
  end;
end;

function TToolPolygon.ToolKeyPress(var key: TUTF8Char): TRect;
var
  keyCode: Word;
begin
  if (Key='i') or (Key='I') then
  begin
    keyCode := VK_INSERT;
    ToolKeyDown(keyCode);
    if keyCode = 0 then key := #0;
    keyCode := VK_INSERT;
    ToolKeyUp(keyCode);
    result := EmptyRect;
  end else
    Result:=inherited ToolKeyPress(key);
end;

function TToolPolygon.DoToolKeyDown(var key: Word): TRect;
begin
  if (key = VK_RETURN) and Assigned(FShape)
   and (FShape.Usermode = vsuCreate) then
  begin
    FShape.Usermode:= vsuEdit;
    result := OnlyRenderChange;
    key := 0;
    exit;
  end else
    Result:=inherited DoToolKeyDown(key);
end;

function TToolPolygon.RoundCoordinate(constref ptF: TPointF): TPointF;
begin
  If Editor.GridActive then
    result := Editor.SnapToGrid(ptF, false)
  else
    result := ptF;
end;

class procedure TToolPolygon.ForgetHintShown;
begin
  RemovePointHintShown := false;
  RemovePointHintShown := false;
end;

initialization

  RegisterTool(ptRect,TToolRectangle);
  RegisterTool(ptEllipse,TToolEllipse);
  RegisterTool(ptPolygon,TToolPolygon);
  RegisterTool(ptSpline,TToolSpline);
  RegisterTool(ptPolyline,TToolPolyline);
  RegisterTool(ptOpenedCurve,TToolOpenedCurve);

end.

