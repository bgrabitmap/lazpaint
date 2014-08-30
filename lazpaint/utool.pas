unit UTool;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, BGRABitmap, BGRABitmapTypes, uimage,
  UImageType, ULayerAction, LCLType, Controls;

type TPaintToolType = (ptHand, ptMoveLayer,ptRotateLayer, ptPen, ptColorPicker, ptEraser,
                   ptRect, ptEllipse, ptPolygon, ptSpline,
                   ptFloodFill, ptGradient, ptPhong,
                   ptSelectPen, ptSelectRect, ptSelectEllipse, ptSelectPoly, ptSelectSpline,
                   ptMoveSelection, ptRotateSelection, ptMagicWand, ptDeformation, ptTextureMapping, ptLayerMapping,
                   ptText);

const
  PaintToolTypeStr : array[TPaintToolType] of string = ('Hand', 'MoveLayer', 'RotateLayer', 'Pen', 'ColorPicker', 'Eraser',
                   'Rect', 'Ellipse', 'Polygon', 'Spline',
                   'FloodFill', 'Gradient', 'Phong',
                   'SelectPen', 'SelectRect', 'SelectEllipse', 'SelectPoly', 'SelectSpline',
                   'MoveSelection', 'RotateSelection', 'MagicWand', 'Deformation', 'TextureMapping', 'LayerMapping',
                   'Text');

function StrToPaintToolType(const s: ansistring): TPaintToolType;

type
  TToolManager = class;
  TBitmapToVirtualScreenFunction = function(PtF: TPointF): TPointF of object;

  TEraserMode = (emEraseAlpha, emSoften);

  { TGenericTool }

  TGenericTool = class
  private
    FAction: TLayerAction;
    function GetLayerOffset: TPoint;
  protected
    FManager: TToolManager;
    FLastToolDrawingLayer: TBGRABitmap;
    function GetAction: TLayerAction; virtual;
    function GetIsSelectingTool: boolean; virtual; abstract;
    function DoToolDown(toolDest: TBGRABitmap; pt: TPoint; ptF: TPointF; rightBtn: boolean): TRect; virtual;
    function DoToolMove(toolDest: TBGRABitmap; pt: TPoint; ptF: TPointF): TRect; virtual;
    procedure DoToolMoveAfter(pt: TPoint; ptF: TPointF); virtual;
    function DoToolUpdate(toolDest: TBGRABitmap): TRect; virtual;
    procedure OnTryStop(sender: TCustomLayerAction); virtual;
    function SelectionMaxPointDistance: single;
  public
    ToolUpdateNeeded: boolean;
    Cursor: TCursor;
    constructor Create(AManager: TToolManager); virtual;
    destructor Destroy; override;
    procedure ValidateAction;
    procedure ValidateActionPartially;
    procedure CancelAction;
    procedure CancelActionPartially;
    procedure BeforeGridSizeChange; virtual;
    procedure AfterGridSizeChange(NewNbX,NewNbY: Integer); virtual;
    function ToolUpdate: TRect;
    function ToolDown(X,Y: single; rightBtn: boolean): TRect;
    function ToolMove(X,Y: single): TRect;
    procedure ToolMoveAfter(X,Y: single);
    function ToolKeyDown(var key: Word): TRect; virtual;
    function ToolKeyUp(var key: Word): TRect; virtual;
    function ToolKeyPress(var key: TUTF8Char): TRect; virtual;
    function ToolUp: TRect; virtual;
    function ToolCopy: boolean; virtual;
    function ToolCut: boolean; virtual;
    function ToolPaste: boolean; virtual;
    function ToolProvideCopy: boolean; virtual;
    function ToolProvideCut: boolean; virtual;
    function ToolProvidePaste: boolean; virtual;
    function GetToolDrawingLayer: TBGRABitmap; virtual;
    procedure RestoreBackupDrawingLayer;
    function GetBackupLayerIfExists: TBGRABitmap;
    function Render(VirtualScreen: TBGRABitmap; VirtualScreenWidth, VirtualScreenHeight: integer; BitmapToVirtualScreen: TBitmapToVirtualScreenFunction): TRect; virtual;
    property Manager : TToolManager read FManager;
    property IsSelectingTool: boolean read GetIsSelectingTool;
    property Action : TLayerAction read GetAction;
    property LayerOffset : TPoint read GetLayerOffset;
    property LastToolDrawingLayer: TBGRABitmap read FLastToolDrawingLayer;
  end;

  TToolClass = class of TGenericTool;

  TToolPopupMessage= (tpmNone,tpmHoldShiftForSquare, tpmHoldCtrlSnapToPixel,
    tpmReturnValides, tpmBackspaceRemoveLastPoint, tpmCtrlRestrictRotation,
    tpmAltShiftScaleMode, tpmCurveModeHint, tpmBlendOpBackground);

  TOnToolChangedHandler = procedure(sender: TToolManager; ANewToolType: TPaintToolType) of object;
  TOnPopupToolHandler = procedure(sender: TToolManager; APopupMessage: TToolPopupMessage) of object;

  { TToolManager }

  TToolManager = class
  private
    FShouldExitTool: boolean;
    FOnToolChangedHandler: TOnToolChangedHandler;
    FOnPopupToolHandler: TOnPopupToolHandler;
    FImage: TLazPaintImage;
    FCurrentTool : TGenericTool;
    FCurrentToolType : TPaintToolType;
    FSleepingTool: TGenericTool;
    FSleepingToolType: TPaintToolType;
    FToolDeformationGridNbX,FToolDeformationGridNbY: integer;
    FToolForeColor, FToolBackColor: TBGRAPixel;
    FReturnValidatesHintShowed: boolean;
    FToolTexture: TBGRABitmap;
    FToolTextureAfterAlpha: TBGRABitmap;
    FToolTextureOpactiy: byte;

    function GetCursor: TCursor;
    function GetToolBackColor: TBGRAPixel;
    function GetToolForeColor: TBGRAPixel;
    function GetToolPenWidth: single;
    function GetToolSleeping: boolean;
    function GetToolTextureOpacity: byte;
    procedure SetControlsVisible(Controls: TList; Visible: Boolean);
    procedure SetToolPenWidth(AValue: single);
    procedure SetToolTextureOpacity(AValue: byte);
    procedure ToolCloseAndReopenImmediatly;
  protected
    function CheckExitTool: boolean;
    procedure NotifyImageOrSelectionChanged(ALayer: TBGRABitmap; ARect: TRect);
    function ToolCanBeUsed: boolean;
    procedure InternalSetCurrentToolType(tool: TPaintToolType);
    function InternalBitmapToVirtualScreen(PtF: TPointF): TPointF;
    function AddLayerOffset(ARect: TRect) : TRect;
  public
    BitmapToVirtualScreen: TBitmapToVirtualScreenFunction;
    PenWidthControls, EraserControls, ToleranceControls,
    ShapeControls, JoinStyleControls, SplineStyleControls,
    LineCapControls, GradientControls, DeformationControls,
    TextControls, PhongControls, AltitudeControls,
    PerspectiveControls,PenColorOrTexControls: TList;

    BlackAndWhite: boolean;

    //tools configuration
    ToolNormalPenWidth, ToolEraserWidth: Single;
    ToolEraserMode: TEraserMode;
    ToolCurrentCursorPos: TPointF;
    ToolOptionDrawShape, ToolOptionFillShape, ToolOptionCloseShape: boolean;
    ToolEraserAlpha, ToolTolerance: byte;
    ToolFloodFillOptionProgressive: boolean;
    ToolGradientType: TGradientType;
    ToolGradientSine: boolean;
    ToolLineCap: TPenEndCap;
    ToolJoinStyle: TPenJoinStyle;
    ToolSplineStyle: TSplineStyle;
    ToolSplineEasyBezier: boolean;
    ToolPenStyle: TPenStyle;
    ToolPerspectiveRepeat,ToolPerspectiveTwoPlanes: boolean;
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
    ToolTextOutlineWidth: integer;
    ToolTextAlign: TAlignment;

    constructor Create(AImage: TLazPaintImage; ABitmapToVirtualScreen: TBitmapToVirtualScreenFunction = nil; ABlackAndWhite : boolean = false);
    destructor Destroy; override;

    function GetCurrentToolType: TPaintToolType;
    function SetCurrentToolType(tool: TPaintToolType): boolean;
    procedure ToolWakeUp;
    procedure ToolSleep;

    function ToolDown(X,Y: single; rightBtn: boolean): boolean; overload;
    function ToolMove(X,Y: single): boolean; overload;
    procedure ToolMoveAfter(X,Y: single); overload;
    function ToolDown(coord: TPointF; rightBtn: boolean): boolean; overload;
    function ToolMove(coord: TPointF): boolean; overload;
    procedure ToolMoveAfter(coord: TPointF); overload;
    function ToolKeyDown(var key: Word): boolean;
    function ToolKeyUp(var key: Word): boolean;
    function ToolKeyPress(var key: TUTF8Char): boolean;
    function ToolCopy: boolean;
    function ToolCut: boolean;
    function ToolPaste: boolean;
    function ToolProvideCopy: boolean;
    function ToolProvideCut: boolean;
    function ToolProvidePaste: boolean;
    function ToolUp: boolean;
    procedure ToolCloseDontReopen;
    procedure ToolOpen;
    function ToolUpdate: boolean;
    function ToolUpdateNeeded: boolean;
    procedure ToolPopup(AMessage: TToolPopupMessage);
    procedure HintReturnValidates;

    function IsSelectingTool: boolean;
    procedure QueryExitTool;

    procedure RenderTool(formBitmap: TBGRABitmap);
    function GetRenderBounds(VirtualScreenWidth, VirtualScreenHeight: integer): TRect;

    property Image: TLazPaintImage read FImage;
    property CurrentTool: TGenericTool read FCurrentTool;

    property ToolDeformationGridNbX: integer read FToolDeformationGridNbX;
    property ToolDeformationGridNbY: integer read FToolDeformationGridNbY;
    property ToolForeColor: TBGRAPixel read GetToolForeColor write FToolForeColor;
    property ToolBackColor: TBGRAPixel read GetToolBackColor write FToolBackColor;

    function SetToolDeformationGridSize(NbX,NbY: integer): boolean;
    procedure SetToolTexture(ATexture: TBGRABitmap);
    function GetToolTextureAfterAlpha: TBGRABitmap;
    function GetToolTexture: TBGRABitmap;
    function BorrowToolTexture: TBGRABitmap;

    property OnToolChanged: TOnToolChangedHandler read FOnToolChangedHandler write FOnToolChangedHandler;
    property OnPopup: TOnPopupToolHandler read FOnPopupToolHandler write FOnPopupToolHandler;
    property Cursor: TCursor read GetCursor;
    property ToolSleeping: boolean read GetToolSleeping;
    property ToolTextureOpacity: byte read GetToolTextureOpacity write SetToolTextureOpacity;
    property ToolPenWidth: single read GetToolPenWidth write SetToolPenWidth;
   end;

procedure RegisterTool(ATool: TPaintToolType; AClass: TToolClass);
function ToolPopupMessageToStr(AMessage :TToolPopupMessage): string;

implementation

uses Types, ugraph, uscaledpi, LazPaintType, UCursors, BGRATextFX, ULoading, uresourcestrings;

function StrToPaintToolType(const s: ansistring): TPaintToolType;
var pt: TPaintToolType;
    ls: ansistring;
begin
  result := ptHand;
  ls:= LowerCase(s);
  for pt := low(TPaintToolType) to high(TPaintToolType) do
    if ls = LowerCase(PaintToolTypeStr[pt]) then
    begin
      result := pt;
      break;
    end;
end;

var
   PaintTools: array[TPaintToolType] of TToolClass;

procedure RegisterTool(ATool: TPaintToolType; AClass: TToolClass);
begin
  PaintTools[ATool] := AClass;
end;

function ToolPopupMessageToStr(AMessage: TToolPopupMessage): string;
begin
  case AMessage of
  tpmHoldShiftForSquare: result := rsHoldShiftForSquare;
  tpmHoldCtrlSnapToPixel: result := rsHoldCtrlSnapToPixel;
  tpmReturnValides: result := rsReturnValides;
  tpmBackspaceRemoveLastPoint: result := rsBackspaceRemoveLastPoint;
  tpmCtrlRestrictRotation: result := rsCtrlRestrictRotation;
  tpmAltShiftScaleMode: result := rsAltShiftScaleMode;
  tpmCurveModeHint: result := rsCurveModeHint;
  tpmBlendOpBackground: result := rsBlendOpNotUsedForBackground;
  else
    result := '';
  end;
end;

procedure TToolManager.HintReturnValidates;
begin
  if not FReturnValidatesHintShowed then
  begin
    ToolPopup(tpmReturnValides);
    FReturnValidatesHintShowed:= true;
  end;
end;

{ TGenericTool }

{$hints off}

function TGenericTool.GetLayerOffset: TPoint;
begin
  if IsSelectingTool or not Assigned(Manager.Image) then
    result := Point(0,0)
  else
    if GetToolDrawingLayer = Manager.Image.SelectedImageLayerReadOnly then
      result := Manager.Image.LayerOffset[Manager.Image.currentImageLayerIndex]
    else
      result := Point(0,0);
end;

function TGenericTool.GetAction: TLayerAction;
begin
  if not Assigned(FAction) then
  begin
    FAction := TLayerAction.Create(Manager.Image);
    FAction.OnTryStop := @OnTryStop;
  end;
  result := FAction;
end;

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
  inherited Create;
  FManager := AManager;
  FAction := nil;
  Cursor := crDefault;
end;

destructor TGenericTool.Destroy;
begin
  FAction.Free;
  inherited Destroy;
end;

procedure TGenericTool.ValidateAction;
begin
  if Assigned(FAction) then
  begin
    FAction.Validate;
    FreeAndNil(FAction);
  end;
end;

procedure TGenericTool.ValidateActionPartially;
begin
  if Assigned(FAction) then FAction.PartialValidate;
end;

procedure TGenericTool.CancelAction;
begin
  if FAction <> nil then
    FreeAndNil(FAction);
end;

procedure TGenericTool.CancelActionPartially;
begin
  if Assigned(FAction) then FAction.PartialCancel;
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

procedure TGenericTool.OnTryStop(sender: TCustomLayerAction);
begin
  Manager.ToolCloseAndReopenImmediatly;
end;

function TGenericTool.SelectionMaxPointDistance: single;
begin
  result := DoScaleX(10,OriginalDPI);
  result /= Manager.Image.ZoomFactor;
end;

procedure TGenericTool.AfterGridSizeChange(NewNbX,NewNbY: Integer);
begin
 //nothing
end;

{$hints on}

function TGenericTool.ToolUpdate: TRect;
var toolDest :TBGRABitmap;
begin
  toolDest := GetToolDrawingLayer;
  if toolDest = nil then exit;
  toolDest.JoinStyle := Manager.ToolJoinStyle;
  toolDest.LineCap := Manager.ToolLineCap;
  toolDest.PenStyle := Manager.ToolPenStyle;
  result := DoToolUpdate(toolDest);
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
  if toolDest = Manager.Image.SelectedImageLayerReadOnly then
  begin
    x -= LayerOffset.x;
    y -= LayerOffset.y;
  end;
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
  pt := Point(round(x),round(y));
  ptF := PointF(x,y);
  Manager.ToolCurrentCursorPos := ptF;
  result := EmptyRect;
  toolDest := GetToolDrawingLayer;
  if toolDest = nil then exit;
  toolDest.JoinStyle := Manager.ToolJoinStyle;
  toolDest.LineCap := Manager.ToolLineCap;
  toolDest.PenStyle := Manager.ToolPenStyle;
  if toolDest = Manager.Image.SelectedImageLayerReadOnly then
  begin
    x -= LayerOffset.x;
    y -= LayerOffset.y;
    pt := Point(round(x),round(y));
    ptF := PointF(x,y);
  end;
  result := DoToolMove(toolDest,pt,ptF);
end;

procedure TGenericTool.ToolMoveAfter(X, Y: single);
var
  pt: TPoint;
  ptF: TPointF;
begin
  x -= LayerOffset.x;
  y -= LayerOffset.y;
  pt := Point(round(x),round(y));
  ptF := PointF(x,y);
  DoToolMoveAfter(pt,ptF);
end;

{$hints off}
function TGenericTool.ToolKeyDown(var key: Word): TRect;
begin
  result := EmptyRect;
  //defined later
end;

function TGenericTool.ToolKeyUp(var key: Word): TRect;
begin
  result := EmptyRect;
  //defined later
end;

function TGenericTool.ToolKeyPress(var key: TUTF8Char): TRect;
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

function TGenericTool.ToolCopy: boolean;
begin
  result := false;
end;

function TGenericTool.ToolCut: boolean;
begin
  result := false;
end;

function TGenericTool.ToolPaste: boolean;
begin
  result := false;
end;

function TGenericTool.ToolProvideCopy: boolean;
begin
  result := false;
end;

function TGenericTool.ToolProvideCut: boolean;
begin
  result := false;
end;

function TGenericTool.ToolProvidePaste: boolean;
begin
  result := false;
end;

function TGenericTool.GetToolDrawingLayer: TBGRABitmap;
begin
  if Action = nil then
  begin
    result := nil;
  end else
  if IsSelectingTool then
  begin
    Action.QuerySelection;
    result := Action.CurrentSelection;
    if result = nil then
      raise exception.Create('Selection not created');
  end
  else
  begin
    result := Action.DrawingLayer;
  end;
  FLastToolDrawingLayer := result;
end;

procedure TGenericTool.RestoreBackupDrawingLayer;
begin
  if Assigned(FAction) then
  begin
    if IsSelectingTool then
      Action.RestoreSelection
    else
      Action.RestoreDrawingLayer;
  end;
end;

function TGenericTool.GetBackupLayerIfExists: TBGRABitmap;
begin
  if Action = nil then
  begin
    result := nil;
    exit;
  end;
  if IsSelectingTool then
    result := Action.BackupSelection
  else
    result := Action.BackupDrawingLayer;
end;

{$hints off}
function TGenericTool.Render(VirtualScreen: TBGRABitmap; VirtualScreenWidth, VirtualScreenHeight: integer; BitmapToVirtualScreen: TBitmapToVirtualScreenFunction): TRect;
begin
  result := EmptyRect;
end;

{$hints on}

{ TToolManager }

function TToolManager.GetCurrentToolType: TPaintToolType;
begin
  result := FCurrentToolType;
end;

function TToolManager.SetCurrentToolType(tool: TPaintToolType): boolean;
begin
  if not ToolSleeping then
  begin
    InternalSetCurrentToolType(tool);
    result := true;
  end
  else result := false;
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

procedure TToolManager.SetToolPenWidth(AValue: single);
begin
  if GetCurrentToolType = ptEraser then
    ToolEraserWidth := AValue else ToolNormalPenWidth := AValue;
end;

procedure TToolManager.SetToolTextureOpacity(AValue: byte);
begin
  if AValue = FToolTextureOpactiy then exit;
  FreeAndNil(FToolTextureAfterAlpha);
  FToolTextureOpactiy := AValue;
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

procedure TToolManager.NotifyImageOrSelectionChanged(ALayer: TBGRABitmap; ARect: TRect);
begin
  if (CurrentTool <> nil) and not IsRectEmpty(ARect) then
  begin
    if CurrentTool.IsSelectingTool then
      Image.SelectionMayChange(ARect)
    else if ALayer <> nil then
      Image.LayerMayChange(ALayer, ARect)
    else
      Image.ImageMayChange(AddLayerOffset(ARect))
  end;
end;

function TToolManager.ToolCanBeUsed: boolean;
begin
  result := (currentTool <> nil) and (CurrentTool.IsSelectingTool or Image.CurrentLayerVisible);
end;

function TToolManager.GetToolBackColor: TBGRAPixel;
begin
  if BlackAndWhite then
    result := BGRAToGrayscale(FToolBackColor)
  else
    result := FToolBackColor;
end;

function TToolManager.GetCursor: TCursor;
var toolCursor: TCursor;
begin
  case GetCurrentToolType of
  ptHand, ptMoveSelection: result := crSizeAll;
  ptRotateSelection,ptRotateLayer: result := crCustomRotate;
  ptPen: result := crCustomCrosshair;
  ptRect,ptEllipse,ptSelectRect,ptSelectEllipse: result := crCustomCrosshair;
  ptColorPicker: result := crCustomColorPicker;
  ptFloodFill: result := crCustomFloodfill;
  ptSelectPen: result := crHandPoint;
  ptEraser: result := crDefault;
  else result := crDefault;
  end;

  if CurrentTool <> nil then
    toolCursor := CurrentTool.Cursor
  else
    toolCursor := crDefault;
  if toolCursor <> crDefault then result := toolCursor;
end;

function TToolManager.GetToolForeColor: TBGRAPixel;
begin
  if BlackAndWhite then
    result := BGRAToGrayscale(FToolForeColor)
  else
    result := FToolForeColor;
end;

function TToolManager.GetToolPenWidth: single;
begin
  if GetCurrentToolType = ptEraser then
    result := ToolEraserWidth else result := ToolNormalPenWidth;
end;

function TToolManager.GetToolSleeping: boolean;
begin
  result := FSleepingTool <> nil;
end;

function TToolManager.GetToolTextureOpacity: byte;
begin
  result := FToolTextureOpactiy;
end;

constructor TToolManager.Create(AImage: TLazPaintImage; ABitmapToVirtualScreen: TBitmapToVirtualScreenFunction; ABlackAndWhite : boolean);
begin
  FImage:= AImage;
  BitmapToVirtualScreen := ABitmapToVirtualScreen;
  FShouldExitTool:= false;

  ToolForeColor := BGRABlack;
  ToolBackColor := BGRA(0,0,255);
  ToolNormalPenWidth := 5;
  ToolEraserWidth := 10;
  ToolEraserMode := emEraseAlpha;
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
  ToolSplineEasyBezier := true;
  ToolTextOutline := False;
  ToolTextShadow := true;
  ToolTextFont := TFont.Create;
  ToolTextFont.Height := -13;
  ToolTextFont.Name := 'Arial';
  ToolTextAlign := taLeftJustify;
  ToolTextPhong := False;
  ToolTextBlur := 4;
  ToolTextShadowOffset := Point(5,5);
  ToolTextOutlineWidth := TBGRATextEffect.OutlineWidth;
  ToolLightPosition := Point(0,0);
  ToolLightAltitude := 100;
  ToolShapeAltitude := 50;
  ToolShapeBorderSize := 20;
  ToolShapeType := 'Rectangle';
  ToolPerspectiveRepeat := false;
  ToolPerspectiveTwoPlanes := false;
  FToolTextureOpactiy:= 255;
  FToolTexture := nil;
  FToolTextureAfterAlpha := nil;

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
  PerspectiveControls := TList.Create;
  PenColorOrTexControls := TList.Create;

  FCurrentToolType := ptHand;
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
  PerspectiveControls.Free;
  PenColorOrTexControls.Free;

  FToolTexture.Free;
  FToolTextureAfterAlpha.Free;
  ToolTextFont.Free;
  inherited Destroy;
end;

procedure TToolManager.InternalSetCurrentToolType(tool: TPaintToolType);
var showPenwidth, showShape, showLineCap, showJoinStyle, showSplineStyle, showEraserOption, showTolerance, showGradient, showDeformation,
    showText, showPhong, showAltitude, showPerspective, showColorOrTex: boolean;
begin
  if (tool <> FCurrentToolType) or (FCurrentTool=nil) then
  begin
    FreeAndNil(FCurrentTool);
    FCurrentToolType:= tool;
    if PaintTools[FCurrentToolType] <> nil then
      FCurrentTool := PaintTools[FCurrentToolType].Create(self);
  end;

  showColorOrTex:= true;
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
  showPerspective := false;

  case FCurrentToolType of
  ptPen,ptSelectPen: showPenwidth := true;
  ptSelectSpline: showSplineStyle := true;
  ptEraser: begin showPenwidth := true; showEraserOption := true; showColorOrTex := false; end;
  ptRect, ptEllipse, ptPolygon, ptSpline:
    begin
      showShape := true;
      showPenwidth := true;
      showLineCap := FCurrentToolType in[ptPolygon,ptSpline];
      showJoinStyle:= FCurrentToolType in[ptRect,ptPolygon];
      showSplineStyle:= FCurrentToolType = ptSpline;
    end;
  ptFloodFill,ptMagicWand: showTolerance := true;
  ptGradient: showGradient := true;
  ptDeformation: showDeformation:= true;
  ptText: begin showText := True; showAltitude := ToolTextPhong; end;
  ptPhong: begin showPhong := true; showAltitude:= true; end;
  ptTextureMapping,ptLayerMapping: showPerspective := true;
  end;
  if FCurrentToolType in [ptSelectEllipse,ptSelectPen,ptSelectPoly,ptSelectRect,
    ptSelectSpline,ptDeformation,ptMagicWand,ptMoveSelection,ptRotateSelection] then
    showColorOrTex:= false;

  if not IsSelectingTool then
    Image.ReleaseEmptySelection;

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
  SetControlsVisible(PerspectiveControls, showPerspective);
  SetControlsVisible(PenColorOrTexControls, showColorOrTex);

  Image.RenderMayChange(rect(0,0,Image.Width,Image.Height),True);
  If Assigned(FOnToolChangedHandler) then
    FOnToolChangedHandler(self, tool);
end;

function TToolManager.InternalBitmapToVirtualScreen(PtF: TPointF): TPointF;
begin
  if Assigned(FCurrentTool) then
  begin
    ptF.x += FCurrentTool.LayerOffset.X;
    ptF.y += FCurrentTool.LayerOffset.Y;
  end;
  result := BitmapToVirtualScreen(ptF);
end;

function TToolManager.AddLayerOffset(ARect: TRect): TRect;
begin
  result := ARect;
  if (result.Left = OnlyRenderChange.Left) and
    (result.Top = OnlyRenderChange.Top) and
    (result.Right = OnlyRenderChange.Right) and
    (result.Bottom = OnlyRenderChange.Bottom) then exit;
  if Assigned(FCurrentTool) then
    OffsetRect(result, FCurrentTool.LayerOffset.X,FCurrentTool.LayerOffset.Y);
end;

procedure TToolManager.ToolWakeUp;
begin
  if FSleepingTool <> nil then
  begin
    FreeAndNil(FCurrentTool);
    FCurrentTool := FSleepingTool;
    FSleepingTool := nil;
    FCurrentToolType := FSleepingToolType;
    InternalSetCurrentToolType(FCurrentToolType);
  end;
end;

procedure TToolManager.ToolSleep;
begin
  if (FSleepingTool = nil) and (FCurrentToolType <> ptHand) then
  begin
    FSleepingTool := FCurrentTool;
    FSleepingToolType := FCurrentToolType;
    FCurrentTool := nil;
    InternalSetCurrentToolType(ptHand);
  end;
end;

{ tool implementation }

function TToolManager.SetToolDeformationGridSize(NbX, NbY: integer): boolean;
begin
  result := false;
  if NbX < 3 then NbX := 3;
  if NbY < 3 then NbY := 3;
  if (NbX <> ToolDeformationGridNbX) or (NbY <> ToolDeformationGridNbY) then
  begin
    CurrentTool.BeforeGridSizeChange;
    FToolDeformationGridNbX := NbX;
    FToolDeformationGridNbY := NbY;
    CurrentTool.AfterGridSizeChange(NbX,NbY);
    result := true;
  end;
end;

procedure TToolManager.SetToolTexture(ATexture: TBGRABitmap);
begin
  if ATexture = FToolTexture then exit;
  FreeAndNil(FToolTexture);
  FToolTexture := ATexture;
  FreeAndNil(FToolTextureAfterAlpha);
end;

function TToolManager.GetToolTextureAfterAlpha: TBGRABitmap;
begin
  if (FToolTextureAfterAlpha = nil) and (FToolTexture <> nil) then
  begin
    FToolTextureAfterAlpha := FToolTexture.Duplicate as TBGRABitmap;
    FToolTextureAfterAlpha.ApplyGlobalOpacity(FToolTextureOpactiy);
  end;
  result := FToolTextureAfterAlpha;
end;

function TToolManager.GetToolTexture: TBGRABitmap;
begin
  result := FToolTexture;
end;

function TToolManager.BorrowToolTexture: TBGRABitmap;
begin
  result := FToolTexture;
  FToolTexture := nil;
  FreeAndNil(FToolTextureAfterAlpha);
end;

function TToolManager.ToolDown(X,Y: single; rightBtn: boolean): boolean; overload;
var changed: TRect;
begin
  if ToolCanBeUsed then
    changed := currentTool.ToolDown(X,Y,rightBtn)
  else
    changed := EmptyRect;
  result := not IsRectEmpty(changed);
  if IsOnlyRenderChange(changed) then changed := EmptyRect;

  if CheckExitTool then result := true;
  if result then NotifyImageOrSelectionChanged(currentTool.LastToolDrawingLayer, changed);
end;

function TToolManager.ToolMove(X,Y: single): boolean; overload;
var changed: TRect;
begin
  if ToolCanBeUsed then
    changed := currentTool.ToolMove(X,Y)
  else
    changed := EmptyRect;
  result := not IsRectEmpty(changed);
  if IsOnlyRenderChange(changed) then changed := EmptyRect;

  if CheckExitTool then result := true;
  if result then NotifyImageOrSelectionChanged(currentTool.LastToolDrawingLayer, changed);
end;

procedure TToolManager.ToolMoveAfter(X, Y: single); overload;
begin
  if ToolCanBeUsed then
    currentTool.ToolMoveAfter(X,Y);
end;

function TToolManager.ToolKeyDown(var key: Word): boolean;
var changed: TRect;
begin
  if ToolCanBeUsed then
    changed := currentTool.ToolKeyDown(key)
  else
    changed := EmptyRect;
  result := not IsRectEmpty(changed);
  if IsOnlyRenderChange(changed) then changed := EmptyRect;

  if CheckExitTool then result := true;
  if result then NotifyImageOrSelectionChanged(currentTool.LastToolDrawingLayer, changed);
end;

function TToolManager.ToolKeyUp(var key: Word): boolean;
var changed: TRect;
begin
  if ToolCanBeUsed then
    changed := currentTool.ToolKeyUp(key)
  else
    changed := EmptyRect;
  result := not IsRectEmpty(changed);
  if IsOnlyRenderChange(changed) then changed := EmptyRect;

  if CheckExitTool then result := true;
  if result then NotifyImageOrSelectionChanged(currentTool.LastToolDrawingLayer, changed);
end;

function TToolManager.ToolKeyPress(var key: TUTF8Char): boolean;
var changed: TRect;
begin
  if ToolCanBeUsed then
    changed := currentTool.ToolKeyPress(key)
  else
    changed := EmptyRect;
  result := not IsRectEmpty(changed);
  if IsOnlyRenderChange(changed) then changed := EmptyRect;

  if CheckExitTool then result := true;
  if result then NotifyImageOrSelectionChanged(currentTool.LastToolDrawingLayer, changed);
end;

function TToolManager.ToolCopy: boolean;
begin
  if Assigned(FCurrentTool) then
    result := FCurrentTool.ToolCopy
  else
    result := false;
end;

function TToolManager.ToolCut: boolean;
begin
  if Assigned(FCurrentTool) then
    result := FCurrentTool.ToolCut
  else
    result := false;
end;

function TToolManager.ToolPaste: boolean;
begin
  if Assigned(FCurrentTool) then
    result := FCurrentTool.ToolPaste
  else
    result := false;
end;

function TToolManager.ToolProvideCopy: boolean;
begin
  if Assigned(FCurrentTool) then
    result := FCurrentTool.ToolProvideCopy
  else
    result := false;
end;

function TToolManager.ToolProvideCut: boolean;
begin
  if Assigned(FCurrentTool) then
    result := FCurrentTool.ToolProvideCut
  else
    result := false;
end;

function TToolManager.ToolProvidePaste: boolean;
begin
  if Assigned(FCurrentTool) then
    result := FCurrentTool.ToolProvidePaste
  else
    result := false;
end;

function TToolManager.ToolUp: boolean;
var changed: TRect;
begin
  if ToolCanBeUsed then
    changed := currentTool.ToolUp
  else
    changed := EmptyRect;
  result := not IsRectEmpty(changed);
  if IsOnlyRenderChange(changed) then changed := EmptyRect;

  if CheckExitTool then result := true;
  if result then NotifyImageOrSelectionChanged(currentTool.LastToolDrawingLayer, changed);
end;

procedure TToolManager.ToolCloseDontReopen;
begin
  if CurrentTool <> nil then
    FreeAndNil(FCurrentTool);
end;

procedure TToolManager.ToolCloseAndReopenImmediatly;
begin
  if CurrentTool <> nil then
  begin
    FreeAndNil(FCurrentTool);
    ToolOpen;
  end;
end;

procedure TToolManager.ToolOpen;
begin
  if (FCurrentTool = nil) and (PaintTools[FCurrentToolType] <> nil) then
    FCurrentTool := PaintTools[FCurrentToolType].Create(self);
end;

function TToolManager.ToolUpdate: boolean;
var changed: TRect;
begin
  if ToolCanBeUsed then
    changed := currentTool.ToolUpdate
  else
    changed := EmptyRect;
  result := not IsRectEmpty(changed);
  if IsOnlyRenderChange(changed) then changed := EmptyRect;

  if CheckExitTool then result := true;
  if result then NotifyImageOrSelectionChanged(CurrentTool.LastToolDrawingLayer, changed);
end;

function TToolManager.ToolUpdateNeeded: boolean;
begin
  if ToolCanBeUsed then
    result := currentTool.ToolUpdateNeeded
  else
    result := false;
  if CheckExitTool then
    result := true;
end;

procedure TToolManager.ToolPopup(AMessage: TToolPopupMessage);
begin
  if Assigned(FOnPopupToolHandler) then
    FOnPopupToolHandler(self, AMessage);
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
  if ToolCanBeUsed then
    Image.RenderMayChange(currentTool.Render(formBitmap,formBitmap.Width,formBitmap.Height, @InternalBitmapToVirtualScreen));
end;

function TToolManager.GetRenderBounds(VirtualScreenWidth, VirtualScreenHeight: integer): TRect;
begin
  if ToolCanBeUsed then
    result := currentTool.Render(nil,VirtualScreenWidth,VirtualScreenHeight, @InternalBitmapToVirtualScreen)
  else
    result := EmptyRect;
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

