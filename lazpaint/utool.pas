unit UTool;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, BGRABitmap, BGRABitmapTypes, uimage,
  UImageType, ULayerAction, LCLType, Controls, UBrushType, UConfig;

type TPaintToolType = (ptHand, ptMoveLayer,ptRotateLayer, ptPen, ptBrush, ptClone, ptColorPicker, ptEraser,
                   ptRect, ptEllipse, ptPolygon, ptSpline,
                   ptFloodFill, ptGradient, ptPhong,
                   ptSelectPen, ptSelectRect, ptSelectEllipse, ptSelectPoly, ptSelectSpline,
                   ptMoveSelection, ptRotateSelection, ptMagicWand, ptDeformation, ptTextureMapping, ptLayerMapping,
                   ptText);

const
  PaintToolTypeStr : array[TPaintToolType] of string = ('Hand', 'MoveLayer', 'RotateLayer', 'Pen', 'Brush', 'Clone', 'ColorPicker', 'Eraser',
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
  TGradientColorspace = (gcsRgb, gcsLinearRgb, gcsHueCW, gcsHueCCW, gcsCorrectedHueCW, gcsCorrectedHueCCW);

function GradientColorSpaceToDisplay(AValue: TGradientColorspace): string;
function DisplayToGradientColorSpace(AValue: string): TGradientColorspace;

type
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
    function GetStatusText: string; virtual;
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
    property StatusText: string read GetStatusText;
  end;

  TToolClass = class of TGenericTool;

  TToolPopupMessage= (tpmNone,tpmHoldShiftForSquare, tpmHoldCtrlSnapToPixel,
    tpmReturnValides, tpmBackspaceRemoveLastPoint, tpmCtrlRestrictRotation,
    tpmAltShiftScaleMode, tpmCurveModeHint, tpmBlendOpBackground,
    tpmRightClickForSource);

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
    FToolBrushInfoList: TList;
    FToolBrushInfoListChanged: boolean;
    FConfigProvider: IConfigProvider;

    function GetCursor: TCursor;
    function GetToolBackColor: TBGRAPixel;
    function GetToolBrushAt(AIndex: integer): TLazPaintBrush;
    function GetToolBrushCount: integer;
    function GetToolBrushInfo: TLazPaintBrush;
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
    procedure InternalSetCurrentToolType(tool: TPaintToolType);
    function InternalBitmapToVirtualScreen(PtF: TPointF): TPointF;
    function AddLayerOffset(ARect: TRect) : TRect;
  public
    BitmapToVirtualScreen: TBitmapToVirtualScreenFunction;
    PenWidthControls, EraserControls, ToleranceControls,
    ShapeControls, JoinStyleControls, SplineStyleControls,
    LineCapControls, GradientControls, DeformationControls,
    TextControls, PhongControls, AltitudeControls,
    PerspectiveControls,PenColorControls,TextureControls,
    BrushControls: TList;

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
    ToolGradientColorspace: TGradientColorspace;
    ToolLineCap: TPenEndCap;
    ToolArrowStart: string;
    ToolArrowEnd: string;
    ToolArrowSize: TPointF;
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
    ToolBrushInfoIndex: integer;
    ToolBrushSpacing: integer;

    constructor Create(AImage: TLazPaintImage; AConfigProvider: IConfigProvider; ABitmapToVirtualScreen: TBitmapToVirtualScreenFunction = nil; ABlackAndWhite : boolean = false);
    destructor Destroy; override;
    procedure ReloadBrushes;
    procedure SaveBrushes;

    function GetCurrentToolType: TPaintToolType;
    function SetCurrentToolType(tool: TPaintToolType): boolean;
    function ToolCanBeUsed: boolean;
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
    procedure AddBrush(brush: TLazPaintBrush);
    procedure RemoveBrushAt(index: integer);

    property OnToolChanged: TOnToolChangedHandler read FOnToolChangedHandler write FOnToolChangedHandler;
    property OnPopup: TOnPopupToolHandler read FOnPopupToolHandler write FOnPopupToolHandler;
    property Cursor: TCursor read GetCursor;
    property ToolSleeping: boolean read GetToolSleeping;
    property ToolTextureOpacity: byte read GetToolTextureOpacity write SetToolTextureOpacity;
    property ToolPenWidth: single read GetToolPenWidth write SetToolPenWidth;
    property ToolBrushInfo: TLazPaintBrush read GetToolBrushInfo;
    property ToolBrushAt[AIndex: integer]: TLazPaintBrush read GetToolBrushAt;
    property ToolBrushCount: integer read GetToolBrushCount;
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

function GradientColorSpaceToDisplay(AValue: TGradientColorspace): string;
begin
  case AValue of
    gcsLinearRgb: result := rsLinearRGB;
    gcsHueCW: result := rsHueCW;
    gcsHueCCW: result := rsHueCCW;
    gcsCorrectedHueCW: result := rsCorrectedHueCW;
    gcsCorrectedHueCCW: result := rsCorrectedHueCCW;
  else
    result := rsRGB;
  end;
end;

function DisplayToGradientColorSpace(AValue: string): TGradientColorspace;
begin
  if AValue=rsLinearRGB then result := gcsLinearRgb else
  if AValue=rsHueCW then result := gcsHueCW else
  if AValue=rsHueCCW then result := gcsHueCCW else
  if AValue=rsCorrectedHueCW then result := gcsCorrectedHueCW else
  if AValue=rsCorrectedHueCCW then result := gcsCorrectedHueCCW
  else
    result := gcsRgb;
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
  tpmRightClickForSource: result := rsRightClickForSource;
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

function TGenericTool.GetStatusText: string;
begin
  result := '';
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
  result := (currentTool <> nil) and ((FCurrentToolType = ptHand) or CurrentTool.IsSelectingTool or Image.CurrentLayerVisible);
end;

function TToolManager.GetToolBackColor: TBGRAPixel;
begin
  if BlackAndWhite then
    result := BGRAToGrayscale(FToolBackColor)
  else
    result := FToolBackColor;
end;

function TToolManager.GetToolBrushAt(AIndex: integer): TLazPaintBrush;
begin
  if (FToolBrushInfoList = nil) or (AIndex < 0) or (AIndex >= FToolBrushInfoList.Count) then
    result := nil
  else
    result := TObject(FToolBrushInfoList[AIndex]) as TLazPaintBrush;
end;

function TToolManager.GetToolBrushCount: integer;
begin
  if Assigned(FToolBrushInfoList) then
    result := FToolBrushInfoList.Count
  else
    result := 0;
end;

function TToolManager.GetToolBrushInfo: TLazPaintBrush;
begin
  if (ToolBrushInfoIndex < 0) or (ToolBrushInfoIndex > FToolBrushInfoList.Count) then
    ToolBrushInfoIndex := 0;
  if ToolBrushInfoIndex > FToolBrushInfoList.Count then
    result := nil
  else
    result := TObject(FToolBrushInfoList[ToolBrushInfoIndex]) as TLazPaintBrush;
end;

function TToolManager.GetCursor: TCursor;
var toolCursor: TCursor;
begin
  case GetCurrentToolType of
  ptHand, ptMoveSelection: result := crSizeAll;
  ptRotateSelection,ptRotateLayer: result := crCustomRotate;
  ptPen,ptBrush,ptClone: result := crCustomCrosshair;
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

constructor TToolManager.Create(AImage: TLazPaintImage; AConfigProvider: IConfigProvider; ABitmapToVirtualScreen: TBitmapToVirtualScreenFunction; ABlackAndWhite : boolean);
begin
  FImage:= AImage;
  BitmapToVirtualScreen := ABitmapToVirtualScreen;
  FShouldExitTool:= false;
  FConfigProvider := AConfigProvider;

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

  ToolBrushSpacing := 1;
  ReloadBrushes;

  ToolGradientType := gtLinear;
  ToolGradientSine := false;
  ToolGradientColorspace := gcsRgb;
  ToolFloodFillOptionProgressive := true;
  ToolLineCap := pecRound;
  ToolJoinStyle := pjsRound;
  ToolArrowStart := 'None';
  ToolArrowEnd := 'None';
  ToolArrowSize := PointF(2,2);
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
  PenColorControls := TList.Create;
  TextureControls := TList.Create;
  BrushControls := TList.Create;

  FCurrentToolType := ptHand;
  FCurrentTool := PaintTools[ptHand].Create(Self);
end;

destructor TToolManager.Destroy;
var
  i: Integer;
begin
  SaveBrushes;
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
  PenColorControls.Free;
  TextureControls.Free;
  BrushControls.Free;

  for i := 0 to ToolBrushCount do
    ToolBrushAt[i].Free;
  FToolBrushInfoList.Free;

  FToolTexture.Free;
  FToolTextureAfterAlpha.Free;
  ToolTextFont.Free;
  inherited Destroy;
end;

procedure TToolManager.ReloadBrushes;
var
  i: Integer;
  bi: TLazPaintBrush;
begin
  If Assigned(FToolBrushInfoList) then
  begin
    for i := 0 to FToolBrushInfoList.Count-1 do
      TObject(FToolBrushInfoList[i]).Free;
    FToolBrushInfoList.Clear;
  end else
    FToolBrushInfoList := TList.Create;
  if Assigned(FConfigProvider) and (FConfigProvider.GetConfig <> nil) then
  begin
    for i := 0 to FConfigProvider.GetConfig.BrushCount-1 do
    begin
      bi := TLazPaintBrush.Create;
      try
        bi.AsString := FConfigProvider.GetConfig.BrushInfo[i];
      except
        continue;
      end;
      FToolBrushInfoList.Add(bi);
    end;
  end;
  if FToolBrushInfoList.Count = 0 then
  begin
    FToolBrushInfoList.Add(TLazPaintBrush.Create(0,True));
    FToolBrushInfoList.Add(TLazPaintBrush.CreateFromStream64('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'));
  end;
  FToolBrushInfoListChanged := false;
end;

procedure TToolManager.SaveBrushes;
var
  i: Integer;
  infos: TStringList;
begin
  if Assigned(FConfigProvider) and FToolBrushInfoListChanged then
  begin
    infos := TStringList.Create;
    try
      for i := 0 to ToolBrushCount-1 do
        infos.Add(ToolBrushAt[i].AsString);
      FConfigProvider.GetConfig.SetBrushes(infos);
    except
    end;
    infos.Free;
  end;
  FToolBrushInfoListChanged := false;
end;

procedure TToolManager.InternalSetCurrentToolType(tool: TPaintToolType);
var showPenwidth, showShape, showLineCap, showJoinStyle, showSplineStyle, showEraserOption, showTolerance, showGradient, showDeformation,
    showText, showPhong, showAltitude, showPerspective, showColor, showTexture, showBrush: boolean;
begin
  if (tool <> FCurrentToolType) or (FCurrentTool=nil) then
  begin
    FreeAndNil(FCurrentTool);
    FCurrentToolType:= tool;
    if PaintTools[FCurrentToolType] <> nil then
      FCurrentTool := PaintTools[FCurrentToolType].Create(self);
  end;

  showColor:= true;
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
  showBrush:= false;
  showTexture := false;

  case FCurrentToolType of
  ptPen,ptSelectPen,ptBrush,ptClone:
    begin
      showPenwidth := true;
      showBrush:= FCurrentToolType in[ptBrush,ptClone];
    end;
  ptSelectSpline: showSplineStyle := true;
  ptEraser: begin showPenwidth := true; showEraserOption := true; showColor := false; end;
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
    ptSelectSpline,ptDeformation,ptClone,ptMagicWand,ptMoveSelection,ptRotateSelection] then
    showColor:= false;
  If FCurrentToolType in [ptPen,ptRect,ptEllipse,ptPolygon,ptSpline,ptFloodFill,
    ptPhong,ptText,ptHand] then
    showTexture:= true;

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
  SetControlsVisible(PenColorControls, showColor);
  SetControlsVisible(TextureControls, showTexture);
  SetControlsVisible(BrushControls, showBrush);

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

procedure TToolManager.AddBrush(brush: TLazPaintBrush);
begin
  ToolBrushInfoIndex := FToolBrushInfoList.Add(brush);
  FToolBrushInfoListChanged := true;
end;

procedure TToolManager.RemoveBrushAt(index: integer);
begin
  if Assigned(FToolBrushInfoList) then
  begin
    if (index >= 1) and (index < ToolBrushCount) then
    begin
      ToolBrushAt[index].Free;
      FToolBrushInfoList.Delete(index);
      if index < ToolBrushInfoIndex then dec(ToolBrushInfoIndex)
      else if index = ToolBrushInfoIndex then
        begin
          if ToolBrushInfoIndex >= ToolBrushCount then
            dec(ToolBrushInfoIndex);
        end;
      FToolBrushInfoListChanged := true;
    end;
  end;
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

