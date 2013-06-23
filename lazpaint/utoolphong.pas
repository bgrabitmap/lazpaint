unit utoolphong;

{$mode objfpc}

interface

uses
  Classes, SysUtils, utool, utoolbasic, BGRABitmapTypes, BGRABitmap, BGRAGradients;

type
  { TToolPhong }

  TToolPhong = class(TToolRectangular)
    constructor Create(AManager: TToolManager); override;
    destructor Destroy; override;
  protected
    shader: TPhongShading;
    function UpdateShape(toolDest: TBGRABitmap): TRect; override;
    function FinishShape({%H-}toolDest: TBGRABitmap): TRect; override;
    procedure PrepareDrawing(rightBtn: boolean); override;
    function DoToolDown(toolDest: TBGRABitmap; pt: TPoint; ptF: TPointF;
      rightBtn: boolean): TRect; override;
    function NeedClearShape: boolean; override;
  public
    procedure Render(VirtualScreen: TBGRABitmap;
      BitmapToVirtualScreen: TBitmapToVirtualScreenFunction); override;
  end;

implementation

uses ugraph, uresourcestrings, Graphics;

{ TToolPhong }

constructor TToolPhong.Create(AManager: TToolManager);
begin
  inherited Create(AManager);
  shader := TPhongShading.Create;
  shader.AmbientFactor := 0.5;
  shader.NegativeDiffusionFactor := 0.15;
end;

destructor TToolPhong.Destroy;
begin
  shader.Free;
  inherited Destroy;
end;

function TToolPhong.UpdateShape(toolDest: TBGRABitmap): TRect;
var bounds : TRect;
    map: TBGRABitmap;
    temp: integer;
    h: integer;
begin
  ClearShape;
  shader.LightPosition := Manager.ToolLightPosition;
  shader.LightPositionZ := Manager.ToolLightAltitude;

  bounds := rect(rectOrigin.x,rectOrigin.y,rectDest.x,rectDest.y);
  if Bounds.Right < Bounds.Left then
  begin
    temp := Bounds.Left;
    bounds.Left := bounds.Right;
    Bounds.Right := temp;
  end;
  if Bounds.Bottom < Bounds.Top then
  begin
    temp := Bounds.Bottom;
    bounds.Bottom := bounds.Top;
    Bounds.Top := temp;
  end;
  if (bounds.left = bounds.right) or (bounds.top = bounds.bottom) then
  begin
    result := EmptyRect;
    exit;
  end else
    result := bounds;
  if Manager.ToolShapeType = 'RoundRectangle' then
  begin
    map := CreateRoundRectanglePreciseMap(bounds.right-bounds.left,bounds.bottom-bounds.top,Manager.ToolShapeBorderSize,[]);
    h := Manager.ToolShapeAltitude;
  end
  else if Manager.ToolShapeType = 'Sphere' then
  begin
    map := CreateSpherePreciseMap(bounds.right-bounds.left,bounds.bottom-bounds.top);
    h := round(Manager.ToolShapeAltitude/100*sqrt((bounds.right-bounds.left)*(bounds.bottom-bounds.top)));
  end
  else if Manager.ToolShapeType = 'Cone' then
  begin
    map := CreateConePreciseMap(bounds.right-bounds.left,bounds.bottom-bounds.top);
    h := round(Manager.ToolShapeAltitude/100*sqrt((bounds.right-bounds.left)*(bounds.bottom-bounds.top)));
  end
  else if Manager.ToolShapeType = 'VerticalCone' then
  begin
    map := CreateVerticalConePreciseMap(bounds.right-bounds.left,bounds.bottom-bounds.top);
    h := round(Manager.ToolShapeAltitude/100*(bounds.right-bounds.left));
  end
  else if Manager.ToolShapeType = 'VerticalCylinder' then
  begin
    map := CreateVerticalCylinderPreciseMap(bounds.right-bounds.left,bounds.bottom-bounds.top);
    h := round(Manager.ToolShapeAltitude/100*(bounds.right-bounds.left));
  end
  else if Manager.ToolShapeType = 'HorizontalCylinder' then
  begin
    map := CreateHorizontalCylinderPreciseMap(bounds.right-bounds.left,bounds.bottom-bounds.top);
    h := round(Manager.ToolShapeAltitude/100*(bounds.bottom-bounds.top));
  end
  else
  begin
    map := CreateRectanglePreciseMap(bounds.right-bounds.left,bounds.bottom-bounds.top,Manager.ToolShapeBorderSize,[]);
    h := Manager.ToolShapeAltitude;
  end;

  if h*3 div 2 > shader.LightPositionZ then
     shader.LightPositionZ := h*3 div 2;

  if Manager.ToolTexture <> nil then
    shader.DrawScan(toolDest,map,h,bounds.left,bounds.top,Manager.ToolTexture)
  else
    shader.Draw(toolDest,map,h,bounds.left,bounds.top,penColor);

  map.free;
end;

function TToolPhong.FinishShape(toolDest: TBGRABitmap): TRect;
begin
  //nothing
  result := EmptyRect;
end;

procedure TToolPhong.PrepareDrawing(rightBtn: boolean);
begin
   if rightBtn then
   begin
     penColor := Manager.ToolBackColor;
     fillColor := Manager.ToolForeColor;
   end else
   begin
     penColor := Manager.ToolForeColor;
     fillColor := Manager.ToolBackColor;
   end;
end;

function TToolPhong.DoToolDown(toolDest: TBGRABitmap; pt: TPoint; ptF: TPointF;
  rightBtn: boolean): TRect;
begin
  if rightBtn then
  begin
    Manager.ToolLightPosition := pt;
    result := ToolRepaintOnly;
    exit;
  end;
  inherited DoToolDown(toolDest,pt,ptF,rightBtn);
end;

function TToolPhong.NeedClearShape: boolean;
begin
  result := true;
end;

procedure TToolPhong.Render(VirtualScreen: TBGRABitmap;
  BitmapToVirtualScreen: TBitmapToVirtualScreenFunction);
var lightPosition: TPointF;
begin
  lightPosition := BitmapToVirtualScreen(PointF(Manager.ToolLightPosition.X,Manager.ToolLightPosition.Y));
  NicePoint(VirtualScreen, lightPosition.X,lightPosition.Y);
  if lightPosition.Y > virtualScreen.Height/2 then
    NiceText(VirtualScreen, round(lightPosition.X),round(lightPosition.Y-6), rsLightPosition, taCenter, tlBottom)
  else
    NiceText(VirtualScreen, round(lightPosition.X),round(lightPosition.Y+6), rsLightPosition, taCenter, tlTop);
end;

initialization

  RegisterTool(ptPhong,TToolPhong);

end.

