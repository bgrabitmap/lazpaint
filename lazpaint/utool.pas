unit utool;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, BGRABitmap, BGRABitmapTypes, uimage, LCLType;

type TPaintToolType = (ptHand, ptPen, ptColorPicker, ptEraser,
                   ptRect, ptEllipse, ptPolygon, ptSpline,
                   ptFloodFill, ptGradient, ptPhong,
                   ptSelectPen, ptSelectRect, ptSelectEllipse, ptSelectPoly, ptSelectSpline,
                   ptMoveSelection, ptRotateSelection, ptMagicWand, ptDeformation, ptTextureMapping,
                   ptText);

const
  ToolRepaintOnly : TRect = (left:-1;top:-1;right:0;bottom:0);

type
  TToolManager = class;
  TBitmapToVirtualScreenFunction = function(PtF: TPointF): TPointF of object;

  { TGenericTool }

  TGenericTool = class
  protected
    FManager: TToolManager;
    function GetIsSelectingTool: boolean; virtual; abstract;
    function DoToolDown(toolDest: TBGRABitmap; pt: TPoint; ptF: TPointF; rightBtn: boolean): TRect; virtual;
    function DoToolMove(toolDest: TBGRABitmap; pt: TPoint; ptF: TPointF): TRect; virtual;
    procedure DoToolMoveAfter(pt: TPoint; ptF: TPointF); virtual;
    function DoToolUpdate(toolDest: TBGRABitmap): TRect; virtual;
  public
    ToolUpdateNeeded: boolean;
    constructor Create(AManager: TToolManager); virtual;
    procedure BeforeGridSizeChange; virtual;
    procedure AfterGridSizeChange(NewNbX,NewNbY: Integer); virtual;
    function ToolUpdate: TRect;
    function ToolDown(X,Y: single; rightBtn: boolean): TRect;
    function ToolMove(X,Y: single): TRect;
    procedure ToolMoveAfter(X,Y: single);
    function ToolKeyDown(key: Word): TRect; virtual;
    function ToolKeyUp(key: Word): TRect; virtual;
    function ToolKeyPress(key: TUTF8Char): TRect; virtual;
    function ToolUp: TRect; virtual;
    function GetToolDrawingLayer: TBGRABitmap; virtual;
    procedure Render(VirtualScreen: TBGRABitmap; BitmapToVirtualScreen: TBitmapToVirtualScreenFunction); virtual;
    property Manager : TToolManager read FManager;
    property IsSelectingTool: boolean read GetIsSelectingTool;
  end;

  TToolClass = class of TGenericTool;

  { TToolManager }

  TToolManager = class
  private
    FShouldExitTool: boolean;
    FImage: TLazPaintImage;
    FCurrentTool : TGenericTool;
    currentToolType : TPaintToolType;
    FToolDeformationGridNbX,FToolDeformationGridNbY: integer;
    FToolForeColor, FToolBackColor: TBGRAPixel;
    function GetToolBackColor: TBGRAPixel;
    function GetToolForeColor: TBGRAPixel;
    procedure SetControlsVisible(Controls: TList; Visible: Boolean);
  protected
    function CheckExitTool: boolean;
  public
    BitmapToVirtualScreen: TBitmapToVirtualScreenFunction;
    PenWidthControls, EraserControls, ToleranceControls,
    ShapeControls, JoinStyleControls, SplineStyleControls,
    LineCapControls, GradientControls, DeformationControls,
    TextControls, PhongControls, AltitudeControls: TList;

    BlackAndWhite: boolean;

    //tools configuration
    ToolPenWidth: Single;
    ToolCurrentCursorPos: TPointF;
    ToolOptionDrawShape, ToolOptionFillShape, ToolOptionCloseShape: boolean;
    ToolEraserAlpha, ToolTolerance: byte;
    ToolFloodFillOptionProgressive: boolean;
    ToolGradientType: TGradientType;
    ToolGradientSine: boolean;
    ToolLineCap: TPenEndCap;
    ToolJoinStyle: TPenJoinStyle;
    ToolSplineStyle: TSplineStyle;
    ToolPenStyle: TPenStyle;
    ToolTexture: TBGRABitmap;
    ToolDeformationGridMoveWithoutDeformation: boolean;
    ToolTextOutline,ToolTextShadow,ToolTextPhong: boolean;
    ToolTextFont: TFont;
    ToolTextBlur: integer;
    ToolTextShadowOffset: TPoint;
    ToolLightPosition: TPoint;
    ToolLightAltitude: integer;
    ToolShapeAltitude: integer;
    ToolShapeBorderSize: integer;
    ToolShapeType: string;

    constructor Create(AImage: TLazPaintImage; ABitmapToVirtualScreen: TBitmapToVirtualScreenFunction = nil; ABlackAndWhite : boolean = false);
    destructor Destroy; override;

    function GetCurrentToolType: TPaintToolType;
    procedure SetCurrentToolType(tool: TPaintToolType);

    function ToolDown(X,Y: single; rightBtn: boolean): boolean; overload;
    function ToolMove(X,Y: single): boolean; overload;
    procedure ToolMoveAfter(X,Y: single); overload;
    function ToolDown(coord: TPointF; rightBtn: boolean): boolean; overload;
    function ToolMove(coord: TPointF): boolean; overload;
    procedure ToolMoveAfter(coord: TPointF); overload;
    function ToolKeyDown(key: Word): boolean;
    function ToolKeyUp(key: Word): boolean;
    function ToolKeyPress(key: TUTF8Char): boolean;
    function ToolUp: boolean;
    procedure ToolClose(ReopenImmediatly: boolean = true);
    procedure ToolOpen;
    function ToolUpdate: boolean;
    function ToolUpdateNeeded: boolean;

    function IsSelectingTool: boolean;
    procedure QueryExitTool;

    procedure RenderTool(formBitmap: TBGRABitmap);

    property Image: TLazPaintImage read FImage;
    property CurrentTool: TGenericTool read FCurrentTool;

    property ToolDeformationGridNbX: integer read FToolDeformationGridNbX;
    property ToolDeformationGridNbY: integer read FToolDeformationGridNbY;
    property ToolForeColor: TBGRAPixel read GetToolForeColor write FToolForeColor;
    property ToolBackColor: TBGRAPixel read GetToolBackColor write FToolBackColor;

    procedure SetToolDeformationGridSize(NbX,NbY: integer);
   end;

procedure RegisterTool(ATool: TPaintToolType; AClass: TToolClass);

implementation

uses Types, Controls, ugraph, BGRAPolygon;

var
   PaintTools: array[TPaintToolType] of TToolClass;

procedure RegisterTool(ATool: TPaintToolType; AClass: TToolClass);
begin
  PaintTools[ATool] := AClass;
end;

{ TGenericTool }

{$hints off}
function TGenericTool.DoToolDown(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF; rightBtn: boolean): TRect;
begin
  result := EmptyRect;
end;
{$hints on}

{$hints off}
function TGenericTool.DoToolMove(toolDest: TBGRABitmap; pt: TPoint; ptF: TPointF): TRect;
begin
  result := EmptyRect;
end;
{$hints on}

{$hints off}
procedure TGenericTool.DoToolMoveAfter(pt: TPoint; ptF: TPointF);
begin
  //nothing
end;

{$hints on}

constructor TGenericTool.Create(AManager: TToolManager);
begin
  FManager := AManager;
end;

procedure TGenericTool.BeforeGridSizeChange;
begin
  //nothing
end;

{$hints off}
function TGenericTool.DoToolUpdate(toolDest: TBGRABitmap): TRect;
begin
  result := EmptyRect;
  //nothing
end;

procedure TGenericTool.AfterGridSizeChange(NewNbX,NewNbY: Integer);
begin
 //nothing
end;

{$hints on}

function TGenericTool.ToolUpdate: TRect;
begin
  result := DoToolUpdate(GetToolDrawingLayer);
end;

function TGenericTool.ToolDown(X, Y: single; rightBtn: boolean): TRect;
var
  toolDest: TBGRABitmap;
  pt: TPoint;
  ptF: TPointF;
begin
  result := EmptyRect;
  toolDest := GetToolDrawingLayer;
  if toolDest = nil then exit;
  toolDest.JoinStyle := Manager.ToolJoinStyle;
  toolDest.LineCap := Manager.ToolLineCap;
  toolDest.PenStyle := Manager.ToolPenStyle;
  pt := Point(round(x),round(y));
  ptF := PointF(x,y);
  result := DoToolDown(toolDest,pt,ptF,rightBtn);
end;

function TGenericTool.ToolMove(X, Y: single): TRect;
var
  toolDest: TBGRABitmap;
  pt: TPoint;
  ptF: TPointF;
begin
  result := EmptyRect;
  toolDest := GetToolDrawingLayer;
  if toolDest = nil then exit;
  toolDest.JoinStyle := Manager.ToolJoinStyle;
  toolDest.LineCap := Manager.ToolLineCap;
  toolDest.PenStyle := Manager.ToolPenStyle;
  pt := Point(round(x),round(y));
  ptF := PointF(x,y);
  Manager.ToolCurrentCursorPos := ptF;
  result := DoToolMove(toolDest,pt,ptF);
end;

procedure TGenericTool.ToolMoveAfter(X, Y: single);
var
  pt: TPoint;
  ptF: TPointF;
begin
  pt := Point(round(x),round(y));
  ptF := PointF(x,y);
  DoToolMoveAfter(pt,ptF);
end;

{$hints off}
function TGenericTool.ToolKeyDown(key: Word): TRect;
begin
  result := EmptyRect;
  //defined later
end;

function TGenericTool.ToolKeyUp(key: Word): TRect;
begin
  result := EmptyRect;
  //defined later
end;

function TGenericTool.ToolKeyPress(key: TUTF8Char): TRect;
begin
  result := EmptyRect;
  //defined later
end;

{$hints on}

function TGenericTool.ToolUp: TRect;
begin
  result := EmptyRect;
  //defined later
end;

function TGenericTool.GetToolDrawingLayer: TBGRABitmap;
begin
  if IsSelectingTool then
  begin
    Manager.Image.QuerySelection;
    result := Manager.Image.currentSelection;
    if result = nil then
      raise exception.Create('Selection not created');
  end
  else
  begin
    if Manager.Image.SelectionEmpty then Manager.Image.ReleaseSelection;
    result := Manager.Image.GetDrawingLayer;
  end;
end;

{$hints off}
procedure TGenericTool.Render(VirtualScreen: TBGRABitmap; BitmapToVirtualScreen: TBitmapToVirtualScreenFunction);
begin
  //nothing
end;

{$hints on}

{ TToolManager }

function TToolManager.GetCurrentToolType: TPaintToolType;
begin
  result := currentToolType;
end;

procedure TToolManager.SetControlsVisible(Controls: TList; Visible: Boolean);
var i: integer;
begin
  if Visible then
  begin
    for i := 0 to Controls.Count-1 do
      (TObject(Controls[i]) as TControl).Visible := Visible;
  end else
  begin
    for i := Controls.Count-1 downto 0 do
      (TObject(Controls[i]) as TControl).Visible := Visible;
  end;
end;

function TToolManager.CheckExitTool: boolean;
begin
  if FShouldExitTool then
  begin
    FShouldExitTool:= false;
    SetCurrentToolType(ptHand);
    result := true;
  end else
    result := false;
end;

function TToolManager.GetToolBackColor: TBGRAPixel;
begin
  if BlackAndWhite then
    result := BGRAToGrayscale(FToolBackColor)
  else
    result := FToolBackColor;
end;

function TToolManager.GetToolForeColor: TBGRAPixel;
begin
  if BlackAndWhite then
    result := BGRAToGrayscale(FToolForeColor)
  else
    result := FToolForeColor;
end;

constructor TToolManager.Create(AImage: TLazPaintImage; ABitmapToVirtualScreen: TBitmapToVirtualScreenFunction; ABlackAndWhite : boolean);
begin
  FImage:= AImage;
  BitmapToVirtualScreen := ABitmapToVirtualScreen;
  FShouldExitTool:= false;

  ToolForeColor := BGRABlack;
  ToolBackColor := BGRA(0,0,255);
  ToolPenWidth := 5;
  ToolOptionDrawShape := True;
  ToolOptionFillShape := True;
  ToolOptionCloseShape := True;
  ToolTolerance := 64;
  BlackAndWhite := ABlackAndWhite;

  ToolGradientType := gtLinear;
  ToolGradientSine := false;
  ToolFloodFillOptionProgressive := true;
  ToolLineCap := pecRound;
  ToolJoinStyle := pjsRound;
  ToolPenStyle := psSolid;
  ToolEraserAlpha := 255;
  ToolSplineStyle := ssRoundOutside;
  ToolTextOutline := False;
  ToolTextShadow := true;
  ToolTextFont := TFont.Create;
  ToolTextFont.Height := -13;
  ToolTextFont.Name := 'Arial';
  ToolTextShadowOffset := Point(5,5);
  ToolTextBlur := 4;
  ToolTextPhong := False;
  ToolLightPosition := Point(0,0);
  ToolLightAltitude := 100;
  ToolShapeAltitude := 50;
  ToolShapeBorderSize := 20;
  ToolShapeType := 'Rectangle';

  FToolDeformationGridNbX := 5;
  FToolDeformationGridNbY := 5;
  ToolDeformationGridMoveWithoutDeformation := false;

  PenWidthControls := TList.Create;
  ShapeControls := TList.Create;
  LineCapControls := TList.Create;
  JoinStyleControls := TList.Create;
  SplineStyleControls := TList.Create;
  EraserControls := TList.Create;
  ToleranceControls := TList.Create;
  GradientControls := TList.Create;
  DeformationControls := TList.Create;
  TextControls := TList.Create;
  PhongControls := TList.Create;
  AltitudeControls := TList.Create;

  currentToolType := ptHand;
  FCurrentTool := PaintTools[ptHand].Create(Self);
end;

destructor TToolManager.Destroy;
begin
  CurrentTool.Free;

  PenWidthControls.Free;
  ShapeControls.Free;
  LineCapControls.Free;
  JoinStyleControls.Free;
  SplineStyleControls.Free;
  EraserControls.Free;
  ToleranceControls.Free;
  GradientControls.Free;
  DeformationControls.Free;
  TextControls.Free;
  PhongControls.Free;
  AltitudeControls.Free;

  ToolTexture.Free;
  ToolTextFont.Free;
  inherited Destroy;
end;

procedure TToolManager.SetCurrentToolType(tool: TPaintToolType);
var showPenwidth, showShape, showLineCap, showJoinStyle, showSplineStyle, showEraserOption, showTolerance, showGradient, showDeformation,
    showText, showPhong, showAltitude: boolean;
begin
  if tool <> currentToolType then
  begin
    FreeAndNil(FCurrentTool);
    currentToolType:= tool;
    if PaintTools[currentToolType] <> nil then
      FCurrentTool := PaintTools[currentToolType].Create(self);
  end;

  showPenwidth := false;
  showShape := false;
  showEraserOption := false;
  showTolerance := false;
  showGradient := false;
  showDeformation := false;
  showLineCap := false;
  showJoinStyle:= false;
  showSplineStyle:= false;
  showText := false;
  showPhong := false;
  showAltitude:= false;

  case currentToolType of
  ptRotateSelection:
    begin
      Image.SelectionRotateCenter := GetSelectionCenter(Image.currentSelection);
    end;
  ptPen,ptSelectPen: showPenwidth := true;
  ptEraser: begin showPenwidth := true; showEraserOption := true; end;
  ptRect, ptEllipse, ptPolygon, ptSpline:
    begin
      showShape := true;
      showPenwidth := true;
      showLineCap := currentToolType in[ptPolygon,ptSpline];
      showJoinStyle:= currentToolType in[ptRect,ptPolygon];
      showSplineStyle:= currentToolType = ptSpline;
    end;
  ptFloodFill,ptMagicWand: showTolerance := true;
  ptGradient: showGradient := true;
  ptDeformation: showDeformation:= true;
  ptText: begin showText := True; showAltitude := ToolTextPhong; end;
  ptPhong: begin showPhong := true; showAltitude:= true; end;
  end;

  if Image.SelectionEmpty then Image.ReleaseSelection;

  SetControlsVisible(PenWidthControls, showPenwidth);
  SetControlsVisible(SplineStyleControls, showSplineStyle);
  SetControlsVisible(JoinStyleControls, showJoinStyle);
  SetControlsVisible(LineCapControls, showLineCap);
  SetControlsVisible(ShapeControls, showShape);
  SetControlsVisible(EraserControls, showEraserOption);
  SetControlsVisible(ToleranceControls, showTolerance);
  SetControlsVisible(GradientControls, showGradient);
  SetControlsVisible(DeformationControls, showDeformation);
  SetControlsVisible(TextControls, showText);
  SetControlsVisible(PhongControls, showPhong);
  SetControlsVisible(AltitudeControls, showAltitude);
end;

{ tool implementation }

procedure TToolManager.SetToolDeformationGridSize(NbX, NbY: integer);
begin
  if (NbX <> ToolDeformationGridNbX) or (NbY <> ToolDeformationGridNbY) then
  begin
    CurrentTool.BeforeGridSizeChange;
    FToolDeformationGridNbX := NbX;
    FToolDeformationGridNbY := NbY;
    CurrentTool.AfterGridSizeChange(NbX,NbY);
  end;
end;

function TToolManager.ToolDown(X,Y: single; rightBtn: boolean): boolean; overload;
var changed: TRect;
begin
  if currentTool <> nil then
    changed := currentTool.ToolDown(X,Y,rightBtn)
  else
    changed := EmptyRect;
  Image.ImageMayChange(changed);
  result := not IsRectEmpty(changed);
  if CheckExitTool then result := true;
end;

function TToolManager.ToolMove(X,Y: single): boolean; overload;
var changed: TRect;
begin
  if currentTool <> nil then
    changed := currentTool.ToolMove(X,Y)
  else
    changed := EmptyRect;
  Image.ImageMayChange(changed);
  result := not IsRectEmpty(changed);
  if CheckExitTool then result := true;
end;

procedure TToolManager.ToolMoveAfter(X, Y: single); overload;
begin
  if currentTool <> nil then
    currentTool.ToolMoveAfter(X,Y);
end;

function TToolManager.ToolKeyDown(key: Word): boolean;
var changed: TRect;
begin
  if currentTool <> nil then
    changed := currentTool.ToolKeyDown(key)
  else
    changed := EmptyRect;
  Image.ImageMayChange(changed);
  result := not IsRectEmpty(changed);
  if CheckExitTool then result := true;
end;

function TToolManager.ToolKeyUp(key: Word): boolean;
var changed: TRect;
begin
  if currentTool <> nil then
    changed := currentTool.ToolKeyUp(key)
  else
    changed := EmptyRect;
  Image.ImageMayChange(changed);
  result := not IsRectEmpty(changed);
  if CheckExitTool then result := true;
end;

function TToolManager.ToolKeyPress(key: TUTF8Char): boolean;
var changed: TRect;
begin
  if currentTool <> nil then
    changed := currentTool.ToolKeyPress(key)
  else
    changed := EmptyRect;
  Image.ImageMayChange(changed);
  result := not IsRectEmpty(changed);
  if CheckExitTool then result := true;
end;

function TToolManager.ToolUp: boolean;
var changed: TRect;
begin
  if currentTool <> nil then
    changed := currentTool.ToolUp
  else
    changed := EmptyRect;
  Image.ImageMayChange(changed);
  result := not IsRectEmpty(changed);
  if CheckExitTool then result := true;
end;

procedure TToolManager.ToolClose(ReopenImmediatly: boolean = true);
begin
  if CurrentTool <> nil then
  begin
    FreeAndNil(FCurrentTool);
    if ReopenImmediatly then ToolOpen;
  end;
end;

procedure TToolManager.ToolOpen;
begin
  if (FCurrentTool = nil) and (PaintTools[currentToolType] <> nil) then
    FCurrentTool := PaintTools[currentToolType].Create(self);
end;

function TToolManager.ToolUpdate: boolean;
var changed: TRect;
begin
  if currentTool <> nil then
    changed := currentTool.ToolUpdate
  else
    changed := EmptyRect;
  Image.ImageMayChange(changed);
  result := not IsRectEmpty(changed);
  if CheckExitTool then result := true;
end;

function TToolManager.ToolUpdateNeeded: boolean;
begin
  if CurrentTool <> nil then
    result := currentTool.ToolUpdateNeeded
  else
    result := false;
  if CheckExitTool then
    result := true;
end;

function TToolManager.IsSelectingTool: boolean;
begin
  if CurrentTool <> nil then
    result := currentTool.IsSelectingTool
  else
    result := false;
end;

procedure TToolManager.QueryExitTool;
begin
  FShouldExitTool:= true;
end;

procedure TToolManager.RenderTool(formBitmap: TBGRABitmap);
begin
  if CurrentTool <> nil then
    currentTool.Render(formBitmap,BitmapToVirtualScreen);
end;

function TToolManager.ToolDown(coord: TPointF; rightBtn: boolean): boolean; overload;
begin
  result := ToolDown(coord.x,coord.y,rightBtn)
end;

function TToolManager.ToolMove(coord: TPointF): boolean; overload;
begin
  result := ToolMove(coord.x,coord.y)
end;

procedure TToolManager.ToolMoveAfter(coord: TPointF); overload;
begin
  ToolMoveAfter(coord.x,coord.y);
end;

initialization
  fillchar(PaintTools,sizeof(PaintTools),0);

end.

