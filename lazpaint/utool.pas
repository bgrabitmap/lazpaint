unit UTool;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, BGRABitmap, BGRABitmapTypes, uimage, LCLType, Controls;

type TPaintToolType = (ptHand, ptPen, ptColorPicker, ptEraser,
                   ptRect, ptEllipse, ptPolygon, ptSpline,
                   ptFloodFill, ptGradient, ptPhong,
                   ptSelectPen, ptSelectRect, ptSelectEllipse, ptSelectPoly, ptSelectSpline,
                   ptMoveSelection, ptRotateSelection, ptMagicWand, ptDeformation, ptTextureMapping, ptLayerMapping,
                   ptText);

const
  PaintToolTypeStr : array[TPaintToolType] of string = ('Hand', 'Pen', 'ColorPicker', 'Eraser',
                   'Rect', 'Ellipse', 'Polygon', 'Spline',
                   'FloodFill', 'Gradient', 'Phong',
                   'SelectPen', 'SelectRect', 'SelectEllipse', 'SelectPoly', 'SelectSpline',
                   'MoveSelection', 'RotateSelection', 'MagicWand', 'Deformation', 'TextureMapping', 'LayerMapping',
                   'Text');

function StrToPaintToolType(const s: ansistring): TPaintToolType;

const
  ToolRepaintOnly : TRect = (left:-1;top:-1;right:0;bottom:0);

type
  TToolManager = class;
  TBitmapToVirtualScreenFunction = function(PtF: TPointF): TPointF of object;

  { TGenericTool }

  TGenericTool = class
  private
    FAction: TLayerAction;
  protected
    FManager: TToolManager;
    function GetAction: TLayerAction; virtual;
    function GetIsSelectingTool: boolean; virtual; abstract;
    function DoToolDown(toolDest: TBGRABitmap; pt: TPoint; ptF: TPointF; rightBtn: boolean): TRect; virtual;
    function DoToolMove(toolDest: TBGRABitmap; pt: TPoint; ptF: TPointF): TRect; virtual;
    procedure DoToolMoveAfter(pt: TPoint; ptF: TPointF); virtual;
    function DoToolUpdate(toolDest: TBGRABitmap): TRect; virtual;
    procedure OnTryStop(sender: TLayerAction); virtual;
    function SelectionMaxPointDistance: single;
  public
    ToolUpdateNeeded: boolean;
    Cursor: TCursor;
    constructor Create(AManager: TToolManager); virtual;
    destructor Destroy; override;
    procedure ValidateAction;
    procedure CancelAction;
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
    procedure RestoreBackupDrawingLayer;
    function GetBackupLayerIfExists: TBGRABitmap;
    procedure Render(VirtualScreen: TBGRABitmap; BitmapToVirtualScreen: TBitmapToVirtualScreenFunction); virtual;
    property Manager : TToolManager read FManager;
    property IsSelectingTool: boolean read GetIsSelectingTool;
    property Action : TLayerAction read GetAction;
  end;

  TToolClass = class of TGenericTool;

  TToolPopupMessage= (tpmNone,tpmHoldShiftForSquare, tpmHoldCtrlSnapToPixel,
    tpmReturnValides, tpmBackspaceRemoveLastPoint, tpmCtrlRestrictRotation,
    tpmAltShiftScaleMode);

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
    function GetToolSleeping: boolean;
    function GetToolTextureOpacity: byte;
    procedure SetControlsVisible(Controls: TList; Visible: Boolean);
    procedure SetToolTextureOpacity(AValue: byte);
    procedure ToolCloseAndReopenImmediatly;
  protected
    function CheckExitTool: boolean;
    procedure NotifyImageOrSelectionChanged(ARect: TRect);
    function ToolCanBeUsed: boolean;
  public
    ShownZoom: single;

    BitmapToVirtualScreen: TBitmapToVirtualScreenFunction;
    PenWidthControls, EraserControls, ToleranceControls,
    ShapeControls, JoinStyleControls, SplineStyleControls,
    LineCapControls, GradientControls, DeformationControls,
    TextControls, PhongControls, AltitudeControls,
    PerspectiveControls: TList;

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
    procedure SetCurrentToolType(tool: TPaintToolType);
    procedure ToolWakeUp;
    procedure ToolSleep;

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
    procedure ToolCloseDontReopen;
    procedure ToolOpen;
    function ToolUpdate: boolean;
    function ToolUpdateNeeded: boolean;
    procedure ToolPopup(AMessage: TToolPopupMessage);
    procedure HintReturnValidates;

    function IsSelectingTool: boolean;
    procedure QueryExitTool;

    procedure RenderTool(formBitmap: TBGRABitmap);

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
   end;

procedure RegisterTool(ATool: TPaintToolType; AClass: TToolClass);
function ToolPopupMessageToStr(AMessage :TToolPopupMessage): string;

implementation

uses Types, ugraph, BGRAPolygon, uscaledpi, LazPaintType, UCursors, BGRATextFX, ULoading, uresourcestrings;

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

procedure TGenericTool.CancelAction;
begin
  if FAction <> nil then
    FreeAndNil(FAction);
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

procedure TGenericTool.OnTryStop(sender: TLayerAction);
begin
  Manager.ToolCloseAndReopenImmediatly;
end;

function TGenericTool.SelectionMaxPointDistance: single;
begin
  result := DoScaleX(10,OriginalDPI);
  if Manager.ShownZoom <> 0 then result /= Manager.ShownZoom;
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
  if Action = nil then
  begin
    result := nil;
    exit;
  end;
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
end;

procedure TGenericTool.RestoreBackupDrawingLayer;
var backup: TBGRABitmap;
begin
  if Assigned(FAction) then
  begin
    if IsSelectingTool then
    begin
      backup := Action.BackupSelection;
      if backup = nil then Action.CurrentSelection.Fill(BGRABlack) else
        Action.CurrentSelection.PutImage(0,0,backup,dmSet);
    end
    else
    begin
      backup := Action.BackupDrawingLayer;
      if backup = nil then Action.DrawingLayer.Fill(BGRAPixelTransparent) else
        Action.DrawingLayer.PutImage(0,0,backup,dmSet);
    end;
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
procedure TGenericTool.Render(VirtualScreen: TBGRABitmap; BitmapToVirtualScreen: TBitmapToVirtualScreenFunction);
begin
  //nothing
end;

{$hints on}

{ TToolManager }

function TToolManager.GetCurrentToolType: TPaintToolType;
begin
  result := FCurrentToolType;
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

procedure TToolManager.NotifyImageOrSelectionChanged(ARect: TRect);
begin
  if CurrentTool <> nil then
  begin
    if CurrentTool.IsSelectingTool then
      Image.SelectionMayChange(ARect)
    else
      Image.ImageMayChange(ARect);
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
  ptRotateSelection: result := crCustomRotate;
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

  FToolTexture.Free;
  FToolTextureAfterAlpha.Free;
  ToolTextFont.Free;
  inherited Destroy;
end;

procedure TToolManager.SetCurrentToolType(tool: TPaintToolType);
var showPenwidth, showShape, showLineCap, showJoinStyle, showSplineStyle, showEraserOption, showTolerance, showGradient, showDeformation,
    showText, showPhong, showAltitude, showPerspective: boolean;
begin
  if (tool <> FCurrentToolType) or (FCurrentTool=nil) then
  begin
    FreeAndNil(FCurrentTool);
    FCurrentToolType:= tool;
    if PaintTools[FCurrentToolType] <> nil then
      FCurrentTool := PaintTools[FCurrentToolType].Create(self);
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
  showPerspective := false;

  case FCurrentToolType of
  ptPen,ptSelectPen: showPenwidth := true;
  ptSelectSpline: showSplineStyle := true;
  ptEraser: begin showPenwidth := true; showEraserOption := true; end;
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

  If Assigned(FOnToolChangedHandler) then FOnToolChangedHandler(self, tool);
end;

procedure TToolManager.ToolWakeUp;
begin
  if FSleepingTool <> nil then
  begin
    FreeAndNil(FCurrentTool);
    FCurrentTool := FSleepingTool;
    FSleepingTool := nil;
    FCurrentToolType := FSleepingToolType;
    SetCurrentToolType(FCurrentToolType);
  end;
end;

procedure TToolManager.ToolSleep;
begin
  if (FSleepingTool = nil) and (FCurrentToolType <> ptHand) then
  begin
    FSleepingTool := FCurrentTool;
    FSleepingToolType := FCurrentToolType;
    FCurrentTool := nil;
    SetCurrentToolType(ptHand);
  end;
end;

{ tool implementation }

function TToolManager.SetToolDeformationGridSize(NbX, NbY: integer): boolean;
begin
  result := false;
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
  NotifyImageOrSelectionChanged(changed);
  result := not IsRectEmpty(changed);
  if CheckExitTool then result := true;
end;

function TToolManager.ToolMove(X,Y: single): boolean; overload;
var changed: TRect;
begin
  if ToolCanBeUsed then
    changed := currentTool.ToolMove(X,Y)
  else
    changed := EmptyRect;
  NotifyImageOrSelectionChanged(changed);
  result := not IsRectEmpty(changed);
  if CheckExitTool then result := true;
end;

procedure TToolManager.ToolMoveAfter(X, Y: single); overload;
begin
  if ToolCanBeUsed then
    currentTool.ToolMoveAfter(X,Y);
end;

function TToolManager.ToolKeyDown(key: Word): boolean;
var changed: TRect;
begin
  if ToolCanBeUsed then
    changed := currentTool.ToolKeyDown(key)
  else
    changed := EmptyRect;
  NotifyImageOrSelectionChanged(changed);
  result := not IsRectEmpty(changed);
  if CheckExitTool then result := true;
end;

function TToolManager.ToolKeyUp(key: Word): boolean;
var changed: TRect;
begin
  if ToolCanBeUsed then
    changed := currentTool.ToolKeyUp(key)
  else
    changed := EmptyRect;
  NotifyImageOrSelectionChanged(changed);
  result := not IsRectEmpty(changed);
  if CheckExitTool then result := true;
end;

function TToolManager.ToolKeyPress(key: TUTF8Char): boolean;
var changed: TRect;
begin
  if ToolCanBeUsed then
    changed := currentTool.ToolKeyPress(key)
  else
    changed := EmptyRect;
  NotifyImageOrSelectionChanged(changed);
  result := not IsRectEmpty(changed);
  if CheckExitTool then result := true;
end;

function TToolManager.ToolUp: boolean;
var changed: TRect;
begin
  if ToolCanBeUsed then
    changed := currentTool.ToolUp
  else
    changed := EmptyRect;
  NotifyImageOrSelectionChanged(changed);
  result := not IsRectEmpty(changed);
  if CheckExitTool then result := true;
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
  NotifyImageOrSelectionChanged(changed);
  result := not IsRectEmpty(changed);
  if CheckExitTool then result := true;
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

