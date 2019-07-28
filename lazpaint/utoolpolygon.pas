unit UToolPolygon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, utool, utoolbasic, BGRABitmap, BGRABitmapTypes,
  LCVectorOriginal, LCLType;

const
  EasyBezierMinimumDotProduct = 0.5;

type
  TToolSplineMode = (tsmMovePoint, tsmCurveModeAuto, tsmCurveModeAngle, tsmCurveModeSpline);

  { TToolPolygon }

  TToolPolygon = class(TVectorialTool)
  protected
    function CreateShape: TVectorShape; override;
    procedure AssignShapeStyle(AMatrix: TAffineMatrix); override;
    procedure UpdateUserMode; virtual;
  end;

  { TToolSpline }

  TToolSpline = class(TToolPolygon)
  private
    FCurrentMode: TToolSplineMode;
    FNextCurveMode: TEasyBezierCurveMode;
    FCurveModeHintShown: Boolean;
    function GetCurrentMode: TToolSplineMode;
    procedure SetCurrentMode(AValue: TToolSplineMode);
    procedure UpdateUserMode; override;
  protected
    function CreateShape: TVectorShape; override;
    procedure AssignShapeStyle(AMatrix: TAffineMatrix); override;
  public
    constructor Create(AManager: TToolManager); override;
    function ToolKeyPress(var key: TUTF8Char): TRect; override;
    property CurrentMode: TToolSplineMode read GetCurrentMode write SetCurrentMode;
  end;

implementation

uses LazPaintType, LCVectorPolyShapes;

{ TToolSpline }

function TToolSpline.GetCurrentMode: TToolSplineMode;
var
  c: TCurveShape;
begin
  if Assigned(FShape) then
  begin
    c := TCurveShape(FShape);
    case c.Usermode of
    vsuEdit: FCurrentMode := tsmMovePoint;
    vsuCreate: if c.PointCount > 1 then
               begin
                 case c.CurveMode[c.PointCount-2] of
                   cmAuto: FCurrentMode := tsmCurveModeAuto;
                   cmAngle: FCurrentMode := tsmCurveModeAngle;
                   cmCurve: FCurrentMode := tsmCurveModeSpline;
                 end;
               end else
                 result := tsmCurveModeAuto;
    vsuCurveSetAuto: FCurrentMode := tsmCurveModeAuto;
    vsuCurveSetAngle: FCurrentMode := tsmCurveModeAngle;
    vsuCurveSetCurve: FCurrentMode := tsmCurveModeSpline;
    end;
  end;
  result := FCurrentMode;
end;

procedure TToolSpline.SetCurrentMode(AValue: TToolSplineMode);
begin
  if FCurrentMode = AValue then exit;
  FCurrentMode := AValue;
  UpdateUserMode;
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
  result := TCurveShape.Create(nil);
  result.Usermode := vsuCreate;
  TCurveShape(result).CosineAngle:= EasyBezierMinimumDotProduct;
  if not FCurveModeHintShown then
  begin
    Manager.ToolPopup(tpmCurveModeHint);
    FCurveModeHintShown := true;
  end;
end;

procedure TToolSpline.AssignShapeStyle(AMatrix: TAffineMatrix);
begin
  inherited AssignShapeStyle(AMatrix);
  TCurveShape(FShape).SplineStyle:= Manager.ToolSplineStyle;
end;

constructor TToolSpline.Create(AManager: TToolManager);
begin
  inherited Create(AManager);
  FNextCurveMode := cmAuto;
end;

function TToolSpline.ToolKeyPress(var key: TUTF8Char): TRect;
begin
  Result:=inherited ToolKeyPress(key);
  if Key='x' then Key := #0;
end;

{ TToolPolygon }

function TToolPolygon.CreateShape: TVectorShape;
begin
  result := TPolylineShape.Create(nil);
end;

procedure TToolPolygon.AssignShapeStyle(AMatrix: TAffineMatrix);
begin
  inherited AssignShapeStyle(AMatrix);
  TCustomPolypointShape(FShape).Closed := Manager.ToolOptionCloseShape;
  TCustomPolypointShape(FShape).ArrowStartKind := StrToArrowKind(Manager.ToolArrowStart);
  TCustomPolypointShape(FShape).ArrowEndKind := StrToArrowKind(Manager.ToolArrowEnd);
  TCustomPolypointShape(FShape).ArrowSize := Manager.ToolArrowSize;
  UpdateUserMode;
end;

procedure TToolPolygon.UpdateUserMode;
begin
  if FShape = nil then exit;
  if FQuickDefine then FShape.Usermode := vsuCreate;
end;

initialization

  RegisterTool(ptPolygon,TToolPolygon);
  RegisterTool(ptSpline,TToolSpline);

end.

