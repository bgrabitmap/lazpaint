// SPDX-License-Identifier: GPL-3.0-only
unit UTool;

{$mode objfpc}{$H+}

interface

uses
  Classes, Types, SysUtils, Graphics, BGRABitmap, BGRABitmapTypes, uimage, UImageType,
  ULayerAction, LCLType, Controls, UBrushType, UConfig, LCVectorPolyShapes,
  BGRAGradientScanner, BGRALayerOriginal, LCVectorRectShapes, UScripting,
  LCVectorialFill, BGRAGradientOriginal;

const
  VK_SNAP = {$IFDEF DARWIN}VK_LWIN{$ELSE}VK_CONTROL{$ENDIF};
  VK_SNAP2 = {$IFDEF DARWIN}VK_RWIN{$ELSE}VK_CONTROL{$ENDIF};
  ssSnap = {$IFDEF DARWIN}ssMeta{$ELSE}ssCtrl{$ENDIF};

type TPaintToolType = (ptHand,ptHotSpot, ptMoveLayer,ptRotateLayer,ptZoomLayer,
                   ptPen, ptBrush, ptClone, ptColorPicker, ptEraser,
                   ptEditShape, ptRect, ptEllipse, ptPolygon, ptSpline, ptPolyline, ptOpenedCurve,
                   ptFloodFill, ptGradient, ptPhong,
                   ptSelectPen, ptSelectRect, ptSelectEllipse, ptSelectPoly, ptSelectSpline,
                   ptMoveSelection, ptRotateSelection, ptMagicWand, ptDeformation, ptTextureMapping, ptLayerMapping,
                   ptText);

const
  PaintToolTypeStr : array[TPaintToolType] of string = ('Hand','HotSpot', 'MoveLayer','RotateLayer','ZoomLayer',
                   'Pen', 'Brush', 'Clone', 'ColorPicker', 'Eraser',
                   'EditShape', 'Rect', 'Ellipse', 'Polygon', 'Spline', 'Polyline', 'OpenedCurve',
                   'FloodFill', 'Gradient', 'Phong',
                   'SelectPen', 'SelectRect', 'SelectEllipse', 'SelectPoly', 'SelectSpline',
                   'MoveSelection', 'RotateSelection', 'MagicWand', 'Deformation', 'TextureMapping', 'LayerMapping',
                   'Text');

function StrToPaintToolType(const s: ansistring): TPaintToolType;

type
  TContextualToolbar = (ctPenFill, ctPenWidth, ctPenStyle, ctJoinStyle, ctLineCap,
    ctCloseShape, ctSplineStyle, ctShape, ctRatio, ctBackFill,
    ctBrush, ctEraserOption, ctAliasing, ctTolerance, ctDeformation, ctPerspective,
    ctText, ctOutlineWidth, ctOutlineFill, ctTextShadow, ctPhong, ctAltitude);
  TContextualToolbars = set of TContextualToolbar;

type
  TToolManager = class;
  TBitmapToVirtualScreenFunction = function(PtF: TPointF): TPointF of object;

  TEraserMode = (emEraseAlpha, emSharpen, emSoften, emLighten, emDarken);
  TToolCommand = (tcCut, tcCopy, tcPaste, tcDelete, tcFinish, tcMoveUp, tcMoveDown, tcMoveToFront, tcMoveToBack,
    tcAlignLeft, tcCenterHorizontally, tcAlignRight, tcAlignTop, tcCenterVertically, tcAlignBottom,
    tcShapeToSpline, tcForeAdjustToShape, tcBackAdjustToShape, tcOutlineAdjustToShape,
    tcForeEditGradTexPoints, tcBackEditGradTexPoints, tcOutlineEditGradTexPoints);

  TDeformationGridMode = (gmDeform, gmMovePointWithoutDeformation);

const
  MaxPenWidth = 999.9;
  MinPenWidth = 1;
  MaxArrowSize = 9.9;
  MinArrowSize = 1;
  MaxBrushSpacing = 99;
  MinPhongShapeAltitude = 1;
  MaxPhongShapeAltitude = 100;
  MinPhongBorderSize = 1;
  MaxPhongBorderSize = 100;
  MinDeformationGridSize = 3;

function GradientInterpolationToDisplay(AValue: TBGRAColorInterpolation): string;
function DisplayToGradientInterpolation(AValue: string): TBGRAColorInterpolation;

type
  TLayerKind = (lkUnknown, lkEmpty, lkBitmap, lkTransformedBitmap, lkGradient, lkVectorial, lkSVG, lkOther);

  { TGenericTool }

  TGenericTool = class
  private
    FShiftState: TShiftState;
    FAction: TLayerAction;
    FForeFill, FBackFill: TVectorialFill;
    FBackFillScan, FForeFillScan: TBGRACustomScanner;
    function GetUniversalBrush(ASource: TVectorialFill; var ADest: TVectorialFill; var AScan: TBGRACustomScanner): TUniversalBrush;
  protected
    FManager: TToolManager;
    FLastToolDrawingLayer: TBGRABitmap;
    FValidating, FCanceling: boolean;
    function GetAction: TLayerAction; virtual;
    function GetIdleAction: TLayerAction; virtual;
    function GetLayerOffset: TPoint; virtual;
    function GetIsSelectingTool: boolean; virtual; abstract;
    function FixSelectionTransform: boolean; virtual;
    function FixLayerOffset: boolean; virtual;
    function DoToolDown(toolDest: TBGRABitmap; pt: TPoint; ptF: TPointF; rightBtn: boolean): TRect; virtual;
    function DoToolMove(toolDest: TBGRABitmap; pt: TPoint; ptF: TPointF): TRect; virtual;
    function DoToolKeyDown(var key: Word): TRect; virtual;
    function DoToolKeyUp(var key: Word): TRect; virtual;
    function DoToolUpdate({%H-}toolDest: TBGRABitmap): TRect; virtual;
    procedure OnTryStop({%H-}sender: TCustomLayerAction); virtual;
    function SelectionMaxPointDistance: single;
    function GetStatusText: string; virtual;
    function DoGetToolDrawingLayer: TBGRABitmap; virtual;
    function GetCurrentLayerKind: TLayerKind;
    function GetIsForeEditGradTexPoints: boolean; virtual;
    function GetIsBackEditGradTexPoints: boolean; virtual;
    function GetIsOutlineEditGradTexPoints: boolean; virtual;
    function GetAllowedBackFillTypes: TVectorialFillTypes; virtual;
    function GetAllowedForeFillTypes: TVectorialFillTypes; virtual;
    function GetAllowedOutlineFillTypes: TVectorialFillTypes; virtual;
    property ShiftState: TShiftState read FShiftState;
  public
    ToolUpdateNeeded: boolean;
    Cursor: TCursor;
    constructor Create(AManager: TToolManager); virtual;
    destructor Destroy; override;
    function GetForeUniversalBrush: TUniversalBrush;
    function GetBackUniversalBrush: TUniversalBrush;
    procedure ReleaseUniversalBrushes; virtual;
    procedure ValidateAction;
    procedure ValidateActionPartially;
    procedure CancelAction;
    procedure CancelActionPartially;
    function HasPen: boolean; virtual;
    function ToolUpdate: TRect;
    function ToolDown(X,Y: single; rightBtn: boolean): TRect;
    function ToolMove(X,Y: single): TRect;
    function ToolKeyDown(var key: Word): TRect;
    function ToolKeyUp(var key: Word): TRect;
    function ToolKeyPress(var key: TUTF8Char): TRect; virtual;
    function ToolUp: TRect; virtual;
    function ToolCommand({%H-}ACommand: TToolCommand): boolean; virtual;
    function ToolProvideCommand({%H-}ACommand: TToolCommand): boolean; virtual;
    function SuggestGradientBox: TAffineBox; virtual;
    function GetContextualToolbars: TContextualToolbars; virtual;
    function GetToolDrawingLayer: TBGRABitmap;
    procedure RestoreBackupDrawingLayer;
    function GetBackupLayerIfExists: TBGRABitmap;
    function Render(VirtualScreen: TBGRABitmap; VirtualScreenWidth, VirtualScreenHeight: integer; BitmapToVirtualScreen: TBitmapToVirtualScreenFunction): TRect; virtual;
    property Manager : TToolManager read FManager;
    property IsSelectingTool: boolean read GetIsSelectingTool;
    property Action : TLayerAction read GetAction;
    property LayerOffset : TPoint read GetLayerOffset;
    property LastToolDrawingLayer: TBGRABitmap read FLastToolDrawingLayer;
    property StatusText: string read GetStatusText;
    property Validating: boolean read FValidating;
    property Canceling: boolean read FCanceling;
    property ForeUniversalBrush: TUniversalBrush read GetForeUniversalBrush;
    property IsForeEditGradTexPoints: boolean read GetIsForeEditGradTexPoints;
    property IsBackEditGradTexPoints: boolean read GetIsBackEditGradTexPoints;
    property IsOutlineEditGradTexPoints: boolean read GetIsOutlineEditGradTexPoints;
    property AllowedForeFillTypes: TVectorialFillTypes read GetAllowedForeFillTypes;
    property AllowedBackFillTypes: TVectorialFillTypes read GetAllowedBackFillTypes;
    property AllowedOutlineFillTypes: TVectorialFillTypes read GetAllowedOutlineFillTypes;
  end;

  { TReadonlyTool }

  TReadonlyTool = class(TGenericTool)
  protected
    function GetAction: TLayerAction; override;
    function GetIsSelectingTool: boolean; override;
    function DoGetToolDrawingLayer: TBGRABitmap; override;
  end;

  TToolClass = class of TGenericTool;

  TToolPopupMessage= (tpmNone, tpmHoldKeyForSquare, tpmHoldKeySnapToPixel,
    tpmReturnValides, tpmBackspaceRemoveLastPoint, tpmRightClickFinishShape, tpmHoldKeyRestrictRotation,
    tpmHoldKeysScaleMode, tpmCurveModeHint, tpmBlendOpBackground,
    tpmRightClickForSource, tpmNothingToBeDeformed);

  TOnToolChangedHandler = procedure(sender: TToolManager; ANewToolType: TPaintToolType) of object;
  TOnPopupToolHandler = procedure(sender: TToolManager; APopupMessage: TToolPopupMessage; AKey: Word; AAlways: boolean) of object;
  TOnQueryColorTargetHandler = procedure(sender: TToolManager; ATarget: TVectorialFill) of object;

  TShapeOption = (toAliasing, toDrawShape, toFillShape, toCloseShape);
  TShapeOptions = set of TShapeOption;

  TFloodFillOption = (ffProgressive, ffFillAll);
  TFloodFillOptions = set of TFloodFillOption;

  TPerspectiveOption = (poRepeat, poTwoPlanes);
  TPerspectiveOptions = set of TPerspectiveOption;

  { TToolManager }

  TToolManager = class
  private
    FConfigProvider: IConfigProvider;
    FOnQueryColorTarget: TOnQueryColorTargetHandler;
    FShouldExitTool: boolean;
    FImage: TLazPaintImage;
    FBlackAndWhite: boolean;
    FScriptContext: TScriptContext;
    FToolPressure: single;
    FInTool, FInToolUpdate, FInSwapFill: boolean;
    FCurrentTool : TGenericTool;
    FCurrentToolType : TPaintToolType;
    FToolCurrentCursorPos: TPointF;
    FSleepingTool: TGenericTool;
    FSleepingToolType: TPaintToolType;
    FOnToolChangedHandler: TOnToolChangedHandler;
    FOnToolRenderChanged: TNotifyEvent;
    FOnToolbarChanged: TNotifyEvent;
    FOnPopupToolHandler: TOnPopupToolHandler;

    FForeFill, FBackFill, FOutlineFill: TVectorialFill;
    FForeLastGradient, FBackLastGradient, FOutlineLastGradient: TBGRALayerGradientOriginal;
    FEraserMode: TEraserMode;
    FEraserAlpha: byte;
    FBrushInfoList: TList;
    FBrushInfoListChanged: boolean;
    FBrushIndex: integer;
    FBrushSpacing: integer;
    FPenStyle: TPenStyle;
    FJoinStyle: TPenJoinStyle;
    FNormalPenWidth, FEraserWidth: Single;
    FShapeOptions: TShapeOptions;
    FTextFontName: string;
    FTextFontSize: single;
    FTextFontStyle: TFontStyles;
    FTextAlign: TAlignment;
    FTextVerticalAlign: TTextLayout;
    FTextBidiMode: TFontBidiMode;
    FTextOutline: boolean;
    FTextOutlineWidth: single;
    FTextPhong: boolean;
    FLightPosition: TPointF;
    FLightAltitude: integer;
    FTextShadow: boolean;
    FTextShadowBlurRadius: single;
    FTextShadowOffset: TPoint;
    FLineCap: TPenEndCap;
    FArrowStart,FArrowEnd: TArrowKind;
    FArrowSize: TPointF;
    FSplineStyle: TSplineStyle;
    FPhongShapeAltitude: integer;
    FPhongShapeBorderSize: integer;
    FPhongShapeKind: TPhongShapeKind;
    FDeformationGridNbX,FDeformationGridNbY: integer;
    FDeformationGridMode: TDeformationGridMode;
    FTolerance: byte;
    FFloodFillOptions: TFloodFillOptions;
    FPerspectiveOptions: TPerspectiveOptions;
    FShapeRatio: Single;

    FOnFillChanged: TNotifyEvent;
    FOnEraserChanged: TNotifyEvent;
    FOnJoinStyleChanged: TNotifyEvent;
    FOnLineCapChanged: TNotifyEvent;
    FOnPenStyleChanged: TNotifyEvent;
    FOnPenWidthChanged: TNotifyEvent;
    FOnBrushChanged, FOnBrushListChanged: TNotifyEvent;
    FOnPhongShapeChanged: TNotifyEvent;
    FOnSplineStyleChanged: TNotifyEvent;
    FOnTextFontChanged, FOnTextAlignChanged: TNotifyEvent;
    FOnTextOutlineChanged: TNotifyEvent;
    FOnTextPhongChanged, FOnLightChanged: TNotifyEvent;
    FOnTextShadowChanged: TNotifyEvent;
    FOnShapeOptionChanged, FOnShapeRatioChanged: TNotifyEvent;
    FOnDeformationGridChanged: TNotifyEvent;
    FOnToleranceChanged: TNotifyEvent;
    FOnFloodFillOptionChanged: TNotifyEvent;
    FOnPerspectiveOptionChanged: TNotifyEvent;

    procedure FillChange(ASender: TObject;
      var {%H-}ADiff: TCustomVectorialFillDiff);
    function GetAllowedBackFillTypes: TVectorialFillTypes;
    function GetAllowedForeFillTypes: TVectorialFillTypes;
    function GetAllowedOutlineFillTypes: TVectorialFillTypes;
    function GetCursor: TCursor;
    function GetBackColor: TBGRAPixel;
    function GetBrushAt(AIndex: integer): TLazPaintBrush;
    function GetBrushCount: integer;
    function GetBrushInfo: TLazPaintBrush;
    function GetForeColor: TBGRAPixel;
    function GetMaxDeformationGridSize: TSize;
    function GetOutlineColor: TBGRAPixel;
    function GetShapeOptionAliasing: boolean;
    function GetPenWidth: single;
    function GetToolSleeping: boolean;
    function GetTextFontName: string;
    function GetTextFontSize: single;
    function GetTextFontStyle: TFontStyles;
    function ScriptGetAliasing(AVars: TVariableSet): TScriptResult;
    function ScriptGetArrowEnd(AVars: TVariableSet): TScriptResult;
    function ScriptGetArrowSize(AVars: TVariableSet): TScriptResult;
    function ScriptGetArrowStart(AVars: TVariableSet): TScriptResult;
    function ScriptGetBackColor(AVars: TVariableSet): TScriptResult;
    function ScriptGetOutlineColor(AVars: TVariableSet): TScriptResult;
    function ScriptGetBrushCount(AVars: TVariableSet): TScriptResult;
    function ScriptGetBrushIndex(AVars: TVariableSet): TScriptResult;
    function ScriptGetBrushSpacing(AVars: TVariableSet): TScriptResult;
    function ScriptGetDeformationGridMode(AVars: TVariableSet): TScriptResult;
    function ScriptGetDeformationGridSize(AVars: TVariableSet): TScriptResult;
    function ScriptGetEraserAlpha(AVars: TVariableSet): TScriptResult;
    function ScriptGetEraserMode(AVars: TVariableSet): TScriptResult;
    function ScriptGetFloodFillOptions(AVars: TVariableSet): TScriptResult;
    function ScriptGetFontName(AVars: TVariableSet): TScriptResult;
    function ScriptGetFontSize(AVars: TVariableSet): TScriptResult;
    function ScriptGetFontStyle(AVars: TVariableSet): TScriptResult;
    function ScriptGetGradientInterpolation(AVars: TVariableSet; AFill: TVectorialFill): TScriptResult;
    function ScriptGetGradientRepetition(AVars: TVariableSet; AFill: TVectorialFill): TScriptResult;
    function ScriptGetGradientType(AVars: TVariableSet; AFill: TVectorialFill): TScriptResult;
    function ScriptGetGradientColors(AVars: TVariableSet; AFill: TVectorialFill): TScriptResult;
    function ScriptGetBackGradientInterpolation(AVars: TVariableSet): TScriptResult;
    function ScriptGetBackGradientRepetition(AVars: TVariableSet): TScriptResult;
    function ScriptGetBackGradientType(AVars: TVariableSet): TScriptResult;
    function ScriptGetBackGradientColors(AVars: TVariableSet): TScriptResult;
    function ScriptGetForeGradientInterpolation(AVars: TVariableSet): TScriptResult;
    function ScriptGetForeGradientRepetition(AVars: TVariableSet): TScriptResult;
    function ScriptGetForeGradientType(AVars: TVariableSet): TScriptResult;
    function ScriptGetForeGradientColors(AVars: TVariableSet): TScriptResult;
    function ScriptGetOutlineGradientInterpolation(AVars: TVariableSet): TScriptResult;
    function ScriptGetOutlineGradientRepetition(AVars: TVariableSet): TScriptResult;
    function ScriptGetOutlineGradientType(AVars: TVariableSet): TScriptResult;
    function ScriptGetOutlineGradientColors(AVars: TVariableSet): TScriptResult;
    function ScriptGetTextureRepetition(AVars: TVariableSet; AFill: TVectorialFill): TScriptResult;
    function ScriptGetTextureOpacity(AVars: TVariableSet; AFill: TVectorialFill): TScriptResult;
    function ScriptGetBackTextureRepetition(AVars: TVariableSet): TScriptResult;
    function ScriptGetBackTextureOpacity(AVars: TVariableSet): TScriptResult;
    function ScriptGetForeTextureRepetition(AVars: TVariableSet): TScriptResult;
    function ScriptGetForeTextureOpacity(AVars: TVariableSet): TScriptResult;
    function ScriptGetOutlineTextureRepetition(AVars: TVariableSet): TScriptResult;
    function ScriptGetOutlineTextureOpacity(AVars: TVariableSet): TScriptResult;
    function ScriptGetJoinStyle(AVars: TVariableSet): TScriptResult;
    function ScriptGetLightPosition(AVars: TVariableSet): TScriptResult;
    function ScriptGetLineCap(AVars: TVariableSet): TScriptResult;
    function ScriptGetForeColor(AVars: TVariableSet): TScriptResult;
    function ScriptGetPenStyle(AVars: TVariableSet): TScriptResult;
    function ScriptGetPenWidth(AVars: TVariableSet): TScriptResult;
    function ScriptGetPerspectiveOptions(AVars: TVariableSet): TScriptResult;
    function ScriptGetPhongShapeAltitude(AVars: TVariableSet): TScriptResult;
    function ScriptGetPhongShapeBorderSize(AVars: TVariableSet): TScriptResult;
    function ScriptGetPhongShapeKind(AVars: TVariableSet): TScriptResult;
    function ScriptGetShapeOptions(AVars: TVariableSet): TScriptResult;
    function ScriptGetShapeRatio(AVars: TVariableSet): TScriptResult;
    function ScriptGetSplineStyle(AVars: TVariableSet): TScriptResult;
    function ScriptGetTextAlign(AVars: TVariableSet): TScriptResult;
    function ScriptGetTextVerticalAlign(AVars: TVariableSet): TScriptResult;
    function ScriptGetTextBidiMode(AVars: TVariableSet): TScriptResult;
    function ScriptGetTextOutline(AVars: TVariableSet): TScriptResult;
    function ScriptGetTextPhong(AVars: TVariableSet): TScriptResult;
    function ScriptGetTolerance(AVars: TVariableSet): TScriptResult;
    function ScriptSetAliasing(AVars: TVariableSet): TScriptResult;
    function ScriptSetArrowEnd(AVars: TVariableSet): TScriptResult;
    function ScriptSetArrowSize(AVars: TVariableSet): TScriptResult;
    function ScriptSetArrowStart(AVars: TVariableSet): TScriptResult;
    function ScriptSetBackColor(AVars: TVariableSet): TScriptResult;
    function ScriptSetOutlineColor(AVars: TVariableSet): TScriptResult;
    function ScriptSetBrushIndex(AVars: TVariableSet): TScriptResult;
    function ScriptSetBrushSpacing(AVars: TVariableSet): TScriptResult;
    function ScriptSetDeformationGridMode(AVars: TVariableSet): TScriptResult;
    function ScriptSetDeformationGridSize(AVars: TVariableSet): TScriptResult;
    function ScriptSetEraserAlpha(AVars: TVariableSet): TScriptResult;
    function ScriptSetEraserMode(AVars: TVariableSet): TScriptResult;
    function ScriptSetFloodFillOptions(AVars: TVariableSet): TScriptResult;
    function ScriptSetFontName(AVars: TVariableSet): TScriptResult;
    function ScriptSetFontSize(AVars: TVariableSet): TScriptResult;
    function ScriptSetFontStyle(AVars: TVariableSet): TScriptResult;
    function ScriptSetGradientInterpolation(AVars: TVariableSet; AFill: TVectorialFill): TScriptResult;
    function ScriptSetGradientRepetition(AVars: TVariableSet; AFill: TVectorialFill): TScriptResult;
    function ScriptSetGradientType(AVars: TVariableSet; AFill: TVectorialFill): TScriptResult;
    function ScriptSetGradientColors(AVars: TVariableSet; AFill: TVectorialFill): TScriptResult;
    function ScriptSetBackGradientInterpolation(AVars: TVariableSet): TScriptResult;
    function ScriptSetBackGradientRepetition(AVars: TVariableSet): TScriptResult;
    function ScriptSetBackGradientType(AVars: TVariableSet): TScriptResult;
    function ScriptSetBackGradientColors(AVars: TVariableSet): TScriptResult;
    function ScriptSetOutlineGradientInterpolation(AVars: TVariableSet): TScriptResult;
    function ScriptSetOutlineGradientRepetition(AVars: TVariableSet): TScriptResult;
    function ScriptSetOutlineGradientType(AVars: TVariableSet): TScriptResult;
    function ScriptSetOutlineGradientColors(AVars: TVariableSet): TScriptResult;
    function ScriptSetForeGradientInterpolation(AVars: TVariableSet): TScriptResult;
    function ScriptSetForeGradientRepetition(AVars: TVariableSet): TScriptResult;
    function ScriptSetForeGradientType(AVars: TVariableSet): TScriptResult;
    function ScriptSetForeGradientColors(AVars: TVariableSet): TScriptResult;
    function ScriptSetTexture(AVars: TVariableSet; AFill: TVectorialFill): TScriptResult;
    function ScriptSetTextureRepetition(AVars: TVariableSet; AFill: TVectorialFill): TScriptResult;
    function ScriptSetTextureOpacity(AVars: TVariableSet; AFill: TVectorialFill): TScriptResult;
    function ScriptSetBackTexture(AVars: TVariableSet): TScriptResult;
    function ScriptSetBackTextureRepetition(AVars: TVariableSet): TScriptResult;
    function ScriptSetBackTextureOpacity(AVars: TVariableSet): TScriptResult;
    function ScriptSetForeTexture(AVars: TVariableSet): TScriptResult;
    function ScriptSetForeTextureRepetition(AVars: TVariableSet): TScriptResult;
    function ScriptSetForeTextureOpacity(AVars: TVariableSet): TScriptResult;
    function ScriptSetOutlineTexture(AVars: TVariableSet): TScriptResult;
    function ScriptSetOutlineTextureRepetition(AVars: TVariableSet): TScriptResult;
    function ScriptSetOutlineTextureOpacity(AVars: TVariableSet): TScriptResult;
    function ScriptSetJoinStyle(AVars: TVariableSet): TScriptResult;
    function ScriptSetLightPosition(AVars: TVariableSet): TScriptResult;
    function ScriptSetLineCap(AVars: TVariableSet): TScriptResult;
    function ScriptSetForeColor(AVars: TVariableSet): TScriptResult;
    function ScriptSetPenStyle(AVars: TVariableSet): TScriptResult;
    function ScriptSetPenWidth(AVars: TVariableSet): TScriptResult;
    function ScriptSetPerspectiveOptions(AVars: TVariableSet): TScriptResult;
    function ScriptSetPhongShapeAltitude(AVars: TVariableSet): TScriptResult;
    function ScriptSetPhongShapeBorderSize(AVars: TVariableSet): TScriptResult;
    function ScriptSetPhongShapeKind(AVars: TVariableSet): TScriptResult;
    function ScriptSetShapeOptions(AVars: TVariableSet): TScriptResult;
    function ScriptSetShapeRatio(AVars: TVariableSet): TScriptResult;
    function ScriptSetSplineStyle(AVars: TVariableSet): TScriptResult;
    function ScriptSetTextAlign(AVars: TVariableSet): TScriptResult;
    function ScriptSetTextVerticalAlign(AVars: TVariableSet): TScriptResult;
    function ScriptSetTextBidiMode(AVars: TVariableSet): TScriptResult;
    function ScriptSetTextOutline(AVars: TVariableSet): TScriptResult;
    function ScriptSetTextPhong(AVars: TVariableSet): TScriptResult;
    function ScriptSetTolerance(AVars: TVariableSet): TScriptResult;
    procedure SetBrushIndex(AValue: integer);
    procedure SetBrushSpacing(AValue: integer);
    function SetControlsVisible(AControls: TList; AVisible: Boolean; AName: string = ''): boolean;
    procedure SetArrowEnd(AValue: TArrowKind);
    procedure SetArrowSize(AValue: TPointF);
    procedure SetArrowStart(AValue: TArrowKind);
    procedure SetBackColor(AValue: TBGRAPixel);
    procedure SetDeformationGridMode(AValue: TDeformationGridMode);
    procedure SetEraserAlpha(AValue: byte);
    procedure SetEraserMode(AValue: TEraserMode);
    procedure SetFloodFillOptions(AValue: TFloodFillOptions);
    procedure SetForeColor(AValue: TBGRAPixel);
    procedure SetJoinStyle(AValue: TPenJoinStyle);
    procedure SetLightAltitude(AValue: integer);
    procedure SetLightPosition(AValue: TPointF);
    procedure SetLineCap(AValue: TPenEndCap);
    procedure SetOnQueryColorTarget(AValue: TOnQueryColorTargetHandler);
    procedure SetOutlineColor(AValue: TBGRAPixel);
    procedure SetPerspectiveOptions(AValue: TPerspectiveOptions);
    procedure SetPhongShapeAltitude(AValue: integer);
    procedure SetPhongShapeBorderSize(AValue: integer);
    procedure SetPhongShapeKind(AValue: TPhongShapeKind);
    procedure SetShapeOptions(AValue: TShapeOptions);
    procedure SetPenStyle(AValue: TPenStyle);
    procedure SetPenWidth(AValue: single);
    procedure SetShapeRatio(AValue: Single);
    procedure SetSplineStyle(AValue: TSplineStyle);
    procedure SetTextAlign(AValue: TAlignment);
    procedure SetTextVerticalAlign(AValue: TTextLayout);
    procedure SetTextBidiMode(AValue: TFontBidiMode);
    procedure SetTextFontStyle(AValue: TFontStyles);
    procedure SetTextPhong(AValue: boolean);
    procedure SetTextShadow(AValue: boolean);
    procedure SetTextShadowBlurRadius(AValue: single);
    procedure SetTextShadowOffset(AValue: TPoint);
    procedure SetTolerance(AValue: byte);
    procedure ToolCloseAndReopenImmediatly;
  protected
    function CheckExitTool: boolean;
    procedure NotifyImageOrSelectionChanged(ALayer: TBGRABitmap; ARect: TRect);
    procedure InternalSetCurrentToolType(tool: TPaintToolType);
    function InternalBitmapToVirtualScreen(PtF: TPointF): TPointF;
    function AddLayerOffset(ARect: TRect) : TRect;
    procedure RegisterScriptFunctions(ARegister: boolean);
  public
    BitmapToVirtualScreen: TBitmapToVirtualScreenFunction;
    PenWidthControls, AliasingControls, EraserControls, ToleranceControls,
    ShapeControls, PenStyleControls, JoinStyleControls, SplineStyleControls,
    CloseShapeControls, LineCapControls, DeformationControls,
    TextControls, TextShadowControls, PhongControls, AltitudeControls,
    PerspectiveControls,FillControls,OutlineFillControls,
    BrushControls, RatioControls, DonateControls: TList;
    CanvasScale: integer;
    PenWidthVisible: boolean;

    constructor Create(AImage: TLazPaintImage; AConfigProvider: IConfigProvider;
      ABitmapToVirtualScreen: TBitmapToVirtualScreenFunction = nil;
      ABlackAndWhite : boolean = false;
      AScriptContext: TScriptContext = nil);
    destructor Destroy; override;
    procedure LoadFromConfig;
    procedure SaveToConfig;
    procedure ReloadBrushes;
    procedure SaveBrushes;
    function ApplyPressure(AColor: TBGRAPixel): TBGRAPixel;
    function ApplyPressure(AOpacity: byte): byte;
    procedure SetPressure(APressure: single);
    function GetPressureB: Byte;
    procedure StepPenSize(ADecrease: boolean);

    function GetCurrentToolType: TPaintToolType;
    function SetCurrentToolType(tool: TPaintToolType): boolean;
    function UpdateContextualToolbars: boolean;
    function GetContextualToolbars: TContextualToolbars;
    function ToolCanBeUsed: boolean;
    function ToolHasLineCap: boolean;
    procedure ToolWakeUp;
    procedure ToolSleep;

    function ToolDown(X,Y: single; ARightBtn: boolean; APressure: single): boolean; overload;
    function ToolMove(X,Y: single; APressure: single): boolean; overload;
    function ToolDown(ACoord: TPointF; ARightBtn: boolean; APressure: single): boolean; overload;
    function ToolMove(ACoord: TPointF; APressure: single): boolean; overload;
    function ToolKeyDown(var key: Word): boolean;
    function ToolKeyUp(var key: Word): boolean;
    function ToolKeyPress(var key: TUTF8Char): boolean;
    function ToolCommand(ACommand: TToolCommand): boolean; virtual;
    function ToolProvideCommand(ACommand: TToolCommand): boolean; virtual;
    function ToolUp: boolean;
    procedure ToolCloseDontReopen;
    procedure ToolOpen;
    function ToolUpdate: boolean;
    function ToolUpdateNeeded: boolean;
    procedure ToolPopup(AMessage: TToolPopupMessage; AKey: Word = 0; AAlways: boolean = false);

    function IsSelectingTool: boolean;
    function DisplayFilledSelection: boolean;
    function IsForeEditGradTexPoints: boolean;
    function IsBackEditGradTexPoints: boolean;
    function IsOutlineEditGradTexPoints: boolean;
    procedure QueryExitTool;
    procedure QueryColorTarget(ATarget: TVectorialFill);

    function RenderTool(formBitmap: TBGRABitmap): TRect;
    function GetRenderBounds(VirtualScreenWidth, VirtualScreenHeight: integer): TRect;
    function SuggestGradientBox: TAffineBox;

    function SwapToolColors: boolean;
    procedure NeedBackGradient;
    procedure NeedForeGradient;
    procedure NeedOutlineGradient;
    procedure AddBrush(brush: TLazPaintBrush);
    procedure RemoveBrushAt(index: integer);
    procedure SetTextFont(AName: string; ASize: single; AStyle: TFontStyles);
    procedure SetTextFont(AFont: TFont);
    procedure SetTextOutline(AEnabled: boolean; AWidth: single);
    function GetDeformationGridSize: TSize;
    procedure SetDeformationGridSize(ASize: TSize);

    property Image: TLazPaintImage read FImage;
    property Scripting: TScriptContext read FScriptContext;
    property BlackAndWhite: boolean read FBlackAndWhite write FBlackAndWhite;
    property CurrentTool: TGenericTool read FCurrentTool;
    property ToolCurrentCursorPos: TPointF read FToolCurrentCursorPos;
    property ToolSleeping: boolean read GetToolSleeping;
    property Cursor: TCursor read GetCursor;

    property ForeFill: TVectorialFill read FForeFill;
    property AllowedForeFillTypes: TVectorialFillTypes read GetAllowedForeFillTypes;
    property BackFill: TVectorialFill read FBackFill;
    property AllowedBackFillTypes: TVectorialFillTypes read GetAllowedBackFillTypes;
    property OutlineFill: TVectorialFill read FOutlineFill;
    property AllowedOutlineFillTypes: TVectorialFillTypes read GetAllowedOutlineFillTypes;
    property ForeColor: TBGRAPixel read GetForeColor write SetForeColor;
    property BackColor: TBGRAPixel read GetBackColor write SetBackColor;
    property OutlineColor: TBGRAPixel read GetOutlineColor write SetOutlineColor;
    property ForeLastGradient: TBGRALayerGradientOriginal read FForeLastGradient;
    property BackLastGradient: TBGRALayerGradientOriginal read FBackLastGradient;
    property OutlineLastGradient: TBGRALayerGradientOriginal read FOutlineLastGradient;
    property EraserMode: TEraserMode read FEraserMode write SetEraserMode;
    property EraserAlpha: byte read FEraserAlpha write SetEraserAlpha;
    property PenWidth: single read GetPenWidth write SetPenWidth;
    property PenStyle: TPenStyle read FPenStyle write SetPenStyle;
    property JoinStyle: TPenJoinStyle read FJoinStyle write SetJoinStyle;
    property ShapeOptions: TShapeOptions read FShapeOptions write SetShapeOptions;
    property ShapeOptionAliasing: boolean read GetShapeOptionAliasing;
    property ShapeRatio: Single read FShapeRatio write SetShapeRatio;
    property BrushInfo: TLazPaintBrush read GetBrushInfo;
    property BrushAt[AIndex: integer]: TLazPaintBrush read GetBrushAt;
    property BrushCount: integer read GetBrushCount;
    property BrushIndex: integer read FBrushIndex write SetBrushIndex;
    property BrushSpacing: integer read FBrushSpacing write SetBrushSpacing;
    property TextFontName: string read GetTextFontName;
    property TextFontSize: single read GetTextFontSize;
    property TextFontStyle: TFontStyles read GetTextFontStyle write SetTextFontStyle;
    property TextAlign: TAlignment read FTextAlign write SetTextAlign;
    property TextVerticalAlign: TTextLayout read FTextVerticalAlign write SetTextVerticalAlign;
    property TextBidiMode: TFontBidiMode read FTextBidiMode write SetTextBidiMode;
    property TextOutline: boolean read FTextOutline;
    property TextOutlineWidth: single read FTextOutlineWidth;
    property TextPhong: boolean read FTextPhong write SetTextPhong;
    property LightPosition: TPointF read FLightPosition write SetLightPosition;
    property LightAltitude: integer read FLightAltitude write SetLightAltitude;
    property TextShadow: boolean read FTextShadow write SetTextShadow;
    property TextShadowBlurRadius: single read FTextShadowBlurRadius write SetTextShadowBlurRadius;
    property TextShadowOffset: TPoint read FTextShadowOffset write SetTextShadowOffset;
    property LineCap: TPenEndCap read FLineCap write SetLineCap;
    property ArrowStart: TArrowKind read FArrowStart write SetArrowStart;
    property ArrowEnd: TArrowKind read FArrowEnd write SetArrowEnd;
    property ArrowSize: TPointF read FArrowSize write SetArrowSize;
    property SplineStyle: TSplineStyle read FSplineStyle write SetSplineStyle;
    property PhongShapeAltitude: integer read FPhongShapeAltitude write SetPhongShapeAltitude;
    property PhongShapeBorderSize: integer read FPhongShapeBorderSize write SetPhongShapeBorderSize;
    property PhongShapeKind: TPhongShapeKind read FPhongShapeKind write SetPhongShapeKind;
    property DeformationGridNbX: integer read FDeformationGridNbX;
    property DeformationGridNbY: integer read FDeformationGridNbY;
    property DeformationGridSize: TSize read GetDeformationGridSize write SetDeformationGridSize;
    property MaxDeformationGridSize: TSize read GetMaxDeformationGridSize;
    property DeformationGridMode: TDeformationGridMode read FDeformationGridMode write SetDeformationGridMode;
    property Tolerance: byte read FTolerance write SetTolerance;
    property FloodFillOptions: TFloodFillOptions read FFloodFillOptions write SetFloodFillOptions;
    property PerspectiveOptions: TPerspectiveOptions read FPerspectiveOptions write SetPerspectiveOptions;

    property OnToolChanged: TOnToolChangedHandler read FOnToolChangedHandler write FOnToolChangedHandler;
    property OnToolRenderChanged: TNotifyEvent read FOnToolRenderChanged write FOnToolRenderChanged;
    property OnToolbarChanged: TNotifyEvent read FOnToolbarChanged write FOnToolbarChanged;
    property OnPopup: TOnPopupToolHandler read FOnPopupToolHandler write FOnPopupToolHandler;
    property OnEraserChanged: TNotifyEvent read FOnEraserChanged write FOnEraserChanged;
    property OnFillChanged: TNotifyEvent read FOnFillChanged write FOnFillChanged;
    property OnQueryColorTarget: TOnQueryColorTargetHandler read FOnQueryColorTarget write SetOnQueryColorTarget;
    property OnPenWidthChanged: TNotifyEvent read FOnPenWidthChanged write FOnPenWidthChanged;
    property OnBrushChanged: TNotifyEvent read FOnBrushChanged write FOnBrushChanged;
    property OnBrushListChanged: TNotifyEvent read FOnBrushListChanged write FOnBrushListChanged;
    property OnPenStyleChanged: TNotifyEvent read FOnPenStyleChanged write FOnPenStyleChanged;
    property OnJoinStyleChanged: TNotifyEvent read FOnJoinStyleChanged write FOnJoinStyleChanged;
    property OnShapeOptionChanged: TNotifyEvent read FOnShapeOptionChanged write FOnShapeOptionChanged;
    property OnShapeRatioChanged: TNotifyEvent read FOnShapeRatioChanged write FOnShapeRatioChanged;
    property OnTextFontChanged: TNotifyEvent read FOnTextFontChanged write FOnTextFontChanged;
    property OnTextAlignChanged: TNotifyEvent read FOnTextAlignChanged write FOnTextAlignChanged;
    property OnTextOutlineChanged: TNotifyEvent read FOnTextOutlineChanged write FOnTextOutlineChanged;
    property OnTextPhongChanged: TNotifyEvent read FOnTextPhongChanged write FOnTextPhongChanged;
    property OnLightChanged: TNotifyEvent read FOnLightChanged write FOnLightChanged;
    property OnTextShadowChanged: TNotifyEvent read FOnTextShadowChanged write FOnTextShadowChanged;
    property OnLineCapChanged: TNotifyEvent read FOnLineCapChanged write FOnLineCapChanged;
    property OnSplineStyleChanged: TNotifyEvent read FOnSplineStyleChanged write FOnSplineStyleChanged;
    property OnPhongShapeChanged: TNotifyEvent read FOnPhongShapeChanged write FOnPhongShapeChanged;
    property OnDeformationGridChanged: TNotifyEvent read FOnDeformationGridChanged write FOnDeformationGridChanged;
    property OnToleranceChanged: TNotifyEvent read FOnToleranceChanged write FOnToleranceChanged;
    property OnFloodFillOptionChanged: TNotifyEvent read FOnFloodFillOptionChanged write FOnFloodFillOptionChanged;
    property OnPerspectiveOptionChanged: TNotifyEvent read FOnPerspectiveOptionChanged write FOnPerspectiveOptionChanged;
  end;

procedure RegisterTool(ATool: TPaintToolType; AClass: TToolClass);
function ToolPopupMessageToStr(AMessage :TToolPopupMessage; AKey: Word = 0): string;

implementation

uses UGraph, LCScaleDPI, LazPaintType, UCursors, BGRATextFX, ULoading, UResourceStrings,
  BGRATransform, LCVectorOriginal, BGRASVGOriginal, math, ULoadImage, LCVectorTextShapes;

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

function GradientInterpolationToDisplay(AValue: TBGRAColorInterpolation): string;
begin
  case AValue of
    ciLinearRGB: result := rsLinearRGB;
    ciLinearHSLPositive: result := rsHueCW;
    ciLinearHSLNegative: result := rsHueCCW;
    ciGSBPositive: result := rsCorrectedHueCW;
    ciGSBNegative: result := rsCorrectedHueCCW;
  else
    result := rsRGB;
  end;
end;

function DisplayToGradientInterpolation(AValue: string): TBGRAColorInterpolation;
begin
  if AValue=rsLinearRGB then result := ciLinearRGB else
  if AValue=rsHueCW then result := ciLinearHSLPositive else
  if AValue=rsHueCCW then result := ciLinearHSLNegative else
  if AValue=rsCorrectedHueCW then result := ciGSBPositive else
  if AValue=rsCorrectedHueCCW then result := ciGSBNegative
  else
    result := ciStdRGB;
end;

function GradientInterpolationToStr(AValue: TBGRAColorInterpolation): string;
begin
  case AValue of
    ciStdRGB: result := 'StdRGB';
    ciLinearHSLPositive: result := 'LinearHSLPositive';
    ciLinearHSLNegative: result := 'LinearHSLNegative';
    ciGSBPositive: result := 'GSBPositive';
    ciGSBNegative: result := 'GSBNegative';
  else
    result := 'LinearRGB';
  end;
end;

function StrToGradientInterpolation(AValue: string): TBGRAColorInterpolation;
begin
  if AValue='StdRGB' then result := ciStdRGB else
  if AValue='LinearHSLPositive' then result := ciLinearHSLPositive else
  if AValue='LinearHSLNegative' then result := ciLinearHSLNegative else
  if AValue='GSBPositive' then result := ciGSBPositive else
  if AValue='GSBNegative' then result := ciGSBNegative
  else
    result := ciLinearRGB;
end;

function GradientRepetitionToStr(AValue: TBGRAGradientRepetition): string;
begin
  case AValue of
    grRepeat: result:= 'Repeat';
    grReflect: result:= 'Reflect';
    grSine: result:= 'Sine';
    else result := 'Pad';
  end;
end;

function StrToGradientRepetition(AValue: string): TBGRAGradientRepetition;
begin
  case AValue of
    'Repeat': result:= grRepeat;
    'Reflect': result:= grReflect;
    'Sine': result:= grSine;
    else result := grPad;
  end;
end;

function GradientToConfigStr(AGradient: TBGRALayerGradientOriginal): string;
var
  vars: TVariableSet;
begin
  vars := TVariableSet.Create('');
  vars.Pixels['StartColor'] := AGradient.StartColor;
  vars.Pixels['EndColor'] := AGradient.EndColor;
  vars.Strings['GradientType'] := GradientTypeStr[AGradient.GradientType];
  vars.Strings['ColorInterpolation'] := GradientInterpolationToStr(AGradient.ColorInterpolation);
  vars.Strings['Repetition'] := GradientRepetitionToStr(AGradient.Repetition);
  result := vars.VariablesAsString;
  vars.Free;
end;

procedure AssignGradientFromConfigStr(AGradient: TBGRALayerGradientOriginal; AValue: string);
var
  vars: TVariableSet;
begin
  vars := TVariableSet.Create('', AValue);
  if vars.IsDefined('StartColor') then AGradient.StartColor := vars.Pixels['StartColor'];
  if vars.IsDefined('EndColor') then AGradient.EndColor := vars.Pixels['EndColor'];
  if vars.IsDefined('GradientType') then AGradient.GradientType := StrToGradientType(vars.Strings['GradientType']);
  if vars.IsDefined('ColorInterpolation') then AGradient.ColorInterpolation := StrToGradientInterpolation(vars.Strings['ColorInterpolation']);
  if vars.IsDefined('Repetition') then AGradient.Repetition := StrToGradientRepetition(vars.Strings['Repetition']);
  vars.Free;
end;

var
   PaintTools: array[TPaintToolType] of TToolClass;

procedure RegisterTool(ATool: TPaintToolType; AClass: TToolClass);
begin
  PaintTools[ATool] := AClass;
end;

function ReplaceKey(AText: string; AKey: Word; AParam: integer = 1): string;
begin
  if AKey = VK_SHIFT then result := StringReplace(AText, '%'+inttostr(AParam), rsShift, []) else
  if AKey = VK_CONTROL then result := StringReplace(AText, '%'+inttostr(AParam), {$IFDEF DARWIN}rsCmd{$ELSE}rsCtrl{$ENDIF}, []) else
  if AKey = VK_MENU then result := StringReplace(AText, '%'+inttostr(AParam), rsAlt, []) else
    result := AText;

end;

function ToolPopupMessageToStr(AMessage: TToolPopupMessage; AKey: Word = 0): string;
begin
  case AMessage of
  tpmHoldKeyForSquare: result := ReplaceKey(rsHoldKeyForSquare, AKey);
  tpmHoldKeySnapToPixel: result := ReplaceKey(rsHoldKeySnapToPixel, AKey);
  tpmReturnValides: result := rsReturnValides;
  tpmBackspaceRemoveLastPoint: result := rsBackspaceRemoveLastPoint;
  tpmRightClickFinishShape: result := rsRightClickFinishShape;
  tpmHoldKeyRestrictRotation: result := ReplaceKey(rsHoldKeyRestrictRotation, AKey);
  tpmHoldKeysScaleMode: result := ReplaceKey(ReplaceKey(rsHoldKeysScaleMode, AKey, 2), VK_MENU);
  tpmCurveModeHint: result := rsCurveModeHint;
  tpmBlendOpBackground: result := rsBlendOpNotUsedForBackground;
  tpmRightClickForSource: result := rsRightClickForSource;
  tpmNothingToBeDeformed: result := rsNothingToBeDeformed;
  else
    result := '';
  end;
end;

{ TReadonlyTool }

function TReadonlyTool.GetAction: TLayerAction;
begin
  Result:= nil;
end;

function TReadonlyTool.GetIsSelectingTool: boolean;
begin
  result := false;
end;

function TReadonlyTool.DoGetToolDrawingLayer: TBGRABitmap;
begin
  if Manager.Image.SelectionMaskEmpty or not assigned(Manager.Image.SelectionLayerReadonly) then
    Result:= Manager.Image.CurrentLayerReadOnly
  else
    Result:= Manager.Image.SelectionLayerReadonly;
end;

{ TGenericTool }

{$hints off}

function TGenericTool.GetLayerOffset: TPoint;
begin
  if IsSelectingTool or not Assigned(Manager.Image) then
    result := Point(0,0)
  else
    if GetToolDrawingLayer = Manager.Image.CurrentLayerReadOnly then
      result := Manager.Image.LayerOffset[Manager.Image.CurrentLayerIndex]
    else
      result := Point(0,0);
end;

function TGenericTool.GetUniversalBrush(ASource: TVectorialFill;
  var ADest: TVectorialFill; var AScan: TBGRACustomScanner): TUniversalBrush;
begin
  FreeAndNil(AScan);
  FreeAndNil(ADest);
  case ASource.FillType of
  vftNone: TBGRABitmap.IdleBrush(result);
  vftSolid: TBGRABitmap.SolidBrush(result, Manager.ApplyPressure(ASource.SolidColor));
  else
    begin
      ADest := ASource.Duplicate;
      ADest.FitGeometry(TAffineBox.AffineBox(RectF(0,0,Manager.Image.Width,Manager.Image.Height)));
      ADest.ApplyOpacity(Manager.GetPressureB);
      AScan := ADest.CreateScanner(AffineMatrixIdentity, false);
      TBGRABitmap.ScannerBrush(result, AScan);
    end;
  end;
end;

function TGenericTool.GetAllowedBackFillTypes: TVectorialFillTypes;
begin
  result := [vftSolid,vftGradient,vftTexture];
end;

function TGenericTool.GetAllowedForeFillTypes: TVectorialFillTypes;
begin
  result := [vftSolid,vftGradient,vftTexture];
end;

function TGenericTool.GetAllowedOutlineFillTypes: TVectorialFillTypes;
begin
  result := [vftSolid,vftGradient,vftTexture];
end;

function TGenericTool.GetIsBackEditGradTexPoints: boolean;
begin
  result := false;
end;

function TGenericTool.GetIsOutlineEditGradTexPoints: boolean;
begin
  result := false;
end;

function TGenericTool.GetIsForeEditGradTexPoints: boolean;
begin
  result := false;
end;

function TGenericTool.GetForeUniversalBrush: TUniversalBrush;
begin
  result := GetUniversalBrush(Manager.ForeFill, FForeFill, FForeFillScan);
end;

function TGenericTool.GetBackUniversalBrush: TUniversalBrush;
begin
  result := GetUniversalBrush(Manager.BackFill, FBackFill, FBackFillScan);
end;

function TGenericTool.GetStatusText: string;
begin
  result := '';
end;

function TGenericTool.DoGetToolDrawingLayer: TBGRABitmap;
begin
  if Action = nil then
    result := nil
  else if IsSelectingTool then
  begin
    Action.QuerySelection;
    result := Action.CurrentSelection;
    if result = nil then
      raise exception.Create('Selection not created');
  end
  else
    result := Action.DrawingLayer;
end;

function TGenericTool.GetCurrentLayerKind: TLayerKind;
var
  c: TBGRALayerOriginalAny;
begin
  if not Manager.Image.LayerOriginalDefined[Manager.Image.CurrentLayerIndex] then
  begin
    if Manager.Image.CurrentLayerEmpty then exit(lkEmpty)
    else exit(lkBitmap);
  end else
  if not Manager.Image.LayerOriginalKnown[Manager.Image.CurrentLayerIndex] then
   exit(lkUnknown)
  else
  begin
    c := Manager.Image.LayerOriginalClass[Manager.Image.CurrentLayerIndex];
    if c = TVectorOriginal then exit(lkVectorial) else
    if c = TBGRALayerImageOriginal then exit(lkTransformedBitmap) else
    if c = TBGRALayerGradientOriginal then exit(lkGradient) else
    if c = TBGRALayerSVGOriginal then exit(lkSVG) else
      exit(lkOther);
  end;
end;

function TGenericTool.GetAction: TLayerAction;
begin
  if not Assigned(FAction) then
  begin
    FAction := Manager.Image.CreateAction(not IsSelectingTool And Manager.Image.SelectionMaskEmpty,
                                          IsSelectingTool or not Manager.Image.SelectionMaskEmpty);
    FAction.OnTryStop := @OnTryStop;
    FAction.ChangeBoundsNotified:= true;
  end;
  result := FAction;
end;

function TGenericTool.GetIdleAction: TLayerAction;
begin
  if not Assigned(FAction) then
  begin
    FAction := Manager.Image.CreateAction(false);
    FAction.OnTryStop := @OnTryStop;
    FAction.ChangeBoundsNotified:= true;
  end;
  result := FAction;
end;

function TGenericTool.FixSelectionTransform: boolean;
begin
  result:= true;
end;

function TGenericTool.FixLayerOffset: boolean;
begin
  result:= true;
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

function TGenericTool.DoToolKeyDown(var key: Word): TRect;
begin
  result := EmptyRect;
  //defined later
end;

function TGenericTool.DoToolKeyUp(var key: Word): TRect;
begin
  result := EmptyRect;
  //defined later
end;

{$hints on}

constructor TGenericTool.Create(AManager: TToolManager);
begin
  inherited Create;
  FManager := AManager;
  FAction := nil;
  FShiftState:= [];
  Cursor := crDefault;
end;

destructor TGenericTool.Destroy;
begin
  FAction.Free;
  inherited Destroy;
end;

procedure TGenericTool.ReleaseUniversalBrushes;
begin
  FreeAndNil(FForeFillScan);
  FreeAndNil(FForeFill);
  FreeAndNil(FBackFillScan);
  FreeAndNil(FBackFill);
end;

procedure TGenericTool.ValidateAction;
begin
  if Assigned(FAction) then
  begin
    FValidating := true;
    FAction.Validate;
    FValidating := false;
    FreeAndNil(FAction);
  end;
end;

procedure TGenericTool.ValidateActionPartially;
begin
  if Assigned(FAction) then
  begin
    FValidating := true;
    FAction.PartialValidate;
    FValidating := false;
  end;
end;

procedure TGenericTool.CancelAction;
begin
  if FAction <> nil then
  begin
    FCanceling := true;
    FreeAndNil(FAction);
    FCanceling := false;
  end;
end;

procedure TGenericTool.CancelActionPartially;
begin
  if Assigned(FAction) then
  begin
    FCanceling := true;
    FAction.PartialCancel;
    FCanceling := false;
  end;
end;

function TGenericTool.HasPen: boolean;
begin
  result := (toDrawShape in Manager.ShapeOptions) or not (ctShape in GetContextualToolbars);
end;

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

function TGenericTool.ToolUpdate: TRect;
var toolDest :TBGRABitmap;
begin
  toolDest := GetToolDrawingLayer;
  if toolDest = nil then
  begin
    result := EmptyRect;
    exit;
  end;
  toolDest.JoinStyle := Manager.JoinStyle;
  toolDest.LineCap := Manager.LineCap;
  toolDest.PenStyle := Manager.PenStyle;
  result := DoToolUpdate(toolDest);
end;

function TGenericTool.ToolDown(X, Y: single; rightBtn: boolean): TRect;
var
  toolDest: TBGRABitmap;
  ptF: TPointF;
begin
  result := EmptyRect;
  toolDest := GetToolDrawingLayer;
  if toolDest = nil then exit;
  toolDest.JoinStyle := Manager.JoinStyle;
  toolDest.LineCap := Manager.LineCap;
  toolDest.PenStyle := Manager.PenStyle;
  ptF := PointF(x,y);
  if toolDest = Manager.Image.CurrentLayerReadOnly then
  begin
    if FixLayerOffset then
    begin
      ptF.x -= LayerOffset.x;
      ptF.y -= LayerOffset.y;
    end;
  end else if FixSelectionTransform and ((toolDest = Manager.Image.SelectionMaskReadonly)
    or (toolDest = Manager.Image.SelectionLayerReadonly)) and
      IsAffineMatrixInversible(Manager.Image.SelectionTransform) then
    ptF := AffineMatrixInverse(Manager.Image.SelectionTransform)*ptF;

  result := DoToolDown(toolDest,ptF.Round,ptF,rightBtn);
end;

function TGenericTool.ToolMove(X, Y: single): TRect;
var
  toolDest: TBGRABitmap;
  ptF: TPointF;
begin
  ptF := PointF(x,y);
  Manager.FToolCurrentCursorPos := ptF;
  result := EmptyRect;
  toolDest := GetToolDrawingLayer;
  if toolDest = nil then exit;
  toolDest.JoinStyle := Manager.JoinStyle;
  toolDest.LineCap := Manager.LineCap;
  toolDest.PenStyle := Manager.PenStyle;
  if toolDest = Manager.Image.CurrentLayerReadOnly then
  begin
    if FixLayerOffset then
    begin
      ptF.x -= LayerOffset.x;
      ptF.y -= LayerOffset.y;
    end;
  end else if FixSelectionTransform and ((toolDest = Manager.Image.SelectionMaskReadonly)
    or (toolDest = Manager.Image.SelectionLayerReadonly)) and
      IsAffineMatrixInversible(Manager.Image.SelectionTransform) then
    ptF := AffineMatrixInverse(Manager.Image.SelectionTransform)*ptF;

  result := DoToolMove(toolDest,ptF.Round,ptF);
end;

{$hints off}
function TGenericTool.ToolKeyDown(var key: Word): TRect;
var
  key2: Word;
begin
  if key = VK_SHIFT then
  begin
    Include(FShiftState, ssShift);
    //do not reset Key to preserve typing ^o or "o
  end else
  if (key = VK_MENU) then
    Include(FShiftState, ssAlt);

  if (Key = VK_SNAP) or (Key = VK_SNAP2) then
  begin
    key2 := VK_CONTROL;
    Include(FShiftState, ssSnap);
    result := DoToolKeyDown(key2);
    if key2 = 0 then key := 0;
  end else
    result := DoToolKeyDown(key);
end;

function TGenericTool.ToolKeyUp(var key: Word): TRect;
var
  key2: word;
begin
  if (key = VK_SHIFT) and (ssShift in FShiftState) then
  begin
    Exclude(FShiftState, ssShift);
    //do not reset key to preserve typing ^o or "o
  end else
  if (key = VK_MENU) and (ssAlt in FShiftState) then
    Exclude(FShiftState, ssAlt);

  //propagate in all cases to know when keys are released for unicode input
  if (Key = VK_SNAP) or (Key = VK_SNAP2) then
  begin
    key2 := VK_CONTROL;
    Exclude(FShiftState, ssSnap);
    result := DoToolKeyUp(key2);
    if key2 = 0 then key := 0;
  end else
    result := DoToolKeyUp(key);
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

function TGenericTool.ToolCommand(ACommand: TToolCommand): boolean;
begin
  result := false;
end;

function TGenericTool.ToolProvideCommand(ACommand: TToolCommand): boolean;
begin
  result := false;
end;

function TGenericTool.SuggestGradientBox: TAffineBox;
var
  m: TAffineMatrix;
begin
  result := TAffineBox.AffineBox(RectF(PointF(0,0),PointF(Manager.Image.Width,Manager.Image.Height)));
  if not IsSelectingTool and Manager.Image.SelectionMaskEmpty then
  begin
    m := Manager.Image.LayerOriginalMatrix[Manager.Image.CurrentLayerIndex];
    result := AffineMatrixInverse(m)*result;
  end;
end;

function TGenericTool.GetContextualToolbars: TContextualToolbars;
begin
  result := [ctPenFill, ctBackFill];
end;

function TGenericTool.GetToolDrawingLayer: TBGRABitmap;
begin
  result := DoGetToolDrawingLayer;
  FLastToolDrawingLayer := result;
end;

procedure TGenericTool.RestoreBackupDrawingLayer;
begin
  if Assigned(FAction) then
  begin
    if IsSelectingTool then
      Action.RestoreSelectionMask
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

function TToolManager.SetControlsVisible(AControls: TList; AVisible: Boolean; AName: string): boolean;
  procedure SetVisibility(AControl: TControl; AVisible: boolean);
  begin
    if AControl.Visible <> AVisible then
    begin
      AControl.Visible := AVisible;
      result := true;
    end;
  end;

var i: integer;
begin
  result := false;
  if AName <> '' then
  begin
    for i := AControls.Count-1 downto 0 do
      if (TObject(AControls[i]) as TControl).Name <> AName then
        SetVisibility(TObject(AControls[i]) as TControl, False);
    for i := 0 to AControls.Count-1 do
      if (TObject(AControls[i]) as TControl).Name = AName then
        SetVisibility(TObject(AControls[i]) as TControl, True);
  end else
  begin
    if AVisible then
    begin
      for i := 0 to AControls.Count-1 do
        SetVisibility(TObject(AControls[i]) as TControl, True);
    end else
      for i := AControls.Count-1 downto 0 do
        SetVisibility(TObject(AControls[i]) as TControl, False);
  end;
end;

procedure TToolManager.SetArrowEnd(AValue: TArrowKind);
begin
  if FArrowEnd=AValue then Exit;
  FArrowEnd:=AValue;
  ToolUpdate;
  if Assigned(FOnLineCapChanged) then FOnLineCapChanged(self);
end;

procedure TToolManager.SetArrowSize(AValue: TPointF);
begin
  if AValue.x < MinArrowSize then AValue.x := MinArrowSize;
  if AValue.x > MaxArrowSize then AValue.x := MaxArrowSize;
  if AValue.y < MinArrowSize then AValue.y := MinArrowSize;
  if AValue.y > MaxArrowSize then AValue.y := MaxArrowSize;
  if FArrowSize=AValue then Exit;
  FArrowSize:=AValue;
  ToolUpdate;
  if Assigned(FOnLineCapChanged) then FOnLineCapChanged(self);
end;

procedure TToolManager.SetArrowStart(AValue: TArrowKind);
begin
  if FArrowStart=AValue then Exit;
  FArrowStart:=AValue;
  ToolUpdate;
  if Assigned(FOnLineCapChanged) then FOnLineCapChanged(self);
end;

procedure TToolManager.SetBackColor(AValue: TBGRAPixel);
begin
  FBackFill.SolidColor := AValue;
end;

procedure TToolManager.SetDeformationGridMode(AValue: TDeformationGridMode);
begin
  if FDeformationGridMode=AValue then Exit;
  FDeformationGridMode:=AValue;
  ToolUpdate;
  if Assigned(FOnDeformationGridChanged) then FOnDeformationGridChanged(self);
end;

procedure TToolManager.SetEraserAlpha(AValue: byte);
begin
  if FEraserAlpha=AValue then Exit;
  FEraserAlpha:=AValue;
  ToolUpdate;
  if Assigned(FOnEraserChanged) then FOnEraserChanged(self);
end;

procedure TToolManager.SetEraserMode(AValue: TEraserMode);
begin
  if FEraserMode=AValue then Exit;
  FEraserMode:=AValue;
  ToolUpdate;
  if Assigned(FOnEraserChanged) then FOnEraserChanged(self);
end;

procedure TToolManager.SetFloodFillOptions(AValue: TFloodFillOptions);
begin
  if FFloodFillOptions=AValue then Exit;
  FFloodFillOptions:=AValue;
  ToolUpdate;
  if Assigned(FOnFloodFillOptionChanged) then FOnFloodFillOptionChanged(self);
end;

procedure TToolManager.SetForeColor(AValue: TBGRAPixel);
begin
  FForeFill.SolidColor := AValue;
end;

procedure TToolManager.SetJoinStyle(AValue: TPenJoinStyle);
begin
  if FJoinStyle=AValue then Exit;
  FJoinStyle:=AValue;
  ToolUpdate;
  if Assigned(FOnJoinStyleChanged) then FOnJoinStyleChanged(self);
end;

procedure TToolManager.SetLightAltitude(AValue: integer);
begin
  if FLightAltitude=AValue then Exit;
  FLightAltitude:=AValue;
  ToolUpdate;
  if Assigned(FOnLightChanged) then FOnLightChanged(self);
end;

procedure TToolManager.SetLightPosition(AValue: TPointF);
begin
  if FLightPosition=AValue then Exit;
  FLightPosition:=AValue;
  ToolUpdate;
  if Assigned(FOnLightChanged) then FOnLightChanged(self);
end;

procedure TToolManager.SetLineCap(AValue: TPenEndCap);
begin
  if FLineCap=AValue then Exit;
  FLineCap:=AValue;
  ToolUpdate;
  if Assigned(FOnLineCapChanged) then FOnLineCapChanged(self);
end;

procedure TToolManager.SetOnQueryColorTarget(AValue: TOnQueryColorTargetHandler);
begin
  if FOnQueryColorTarget=AValue then Exit;
  FOnQueryColorTarget:=AValue;
end;

procedure TToolManager.SetOutlineColor(AValue: TBGRAPixel);
begin
  FOutlineFill.SolidColor := AValue;
end;

procedure TToolManager.SetPerspectiveOptions(AValue: TPerspectiveOptions);
begin
  if FPerspectiveOptions=AValue then Exit;
  FPerspectiveOptions:=AValue;
  ToolUpdate;
  if Assigned(FOnPerspectiveOptionChanged) then FOnPerspectiveOptionChanged(self);
end;

procedure TToolManager.SetPhongShapeAltitude(AValue: integer);
begin
  if AValue < MinPhongShapeAltitude then AValue := MinPhongShapeAltitude;
  if AValue > MaxPhongShapeAltitude then AValue := MaxPhongShapeAltitude;
  if FPhongShapeAltitude=AValue then Exit;
  FPhongShapeAltitude:=AValue;
  ToolUpdate;
  if Assigned(FOnPhongShapeChanged) then FOnPhongShapeChanged(self);
end;

procedure TToolManager.SetPhongShapeBorderSize(AValue: integer);
begin
  if AValue < MinPhongBorderSize then AValue := MinPhongBorderSize;
  if AValue > MaxPhongBorderSize then AValue := MaxPhongBorderSize;
  if FPhongShapeBorderSize=AValue then Exit;
  FPhongShapeBorderSize:=AValue;
  ToolUpdate;
  if Assigned(FOnPhongShapeChanged) then FOnPhongShapeChanged(self);
end;

procedure TToolManager.SetPhongShapeKind(AValue: TPhongShapeKind);
begin
  if FPhongShapeKind=AValue then Exit;
  FPhongShapeKind:=AValue;
  ToolUpdate;
  if Assigned(FOnPhongShapeChanged) then FOnPhongShapeChanged(self);
end;

procedure TToolManager.SetShapeOptions(AValue: TShapeOptions);
begin
  if FShapeOptions=AValue then Exit;
  FShapeOptions:=AValue;
  ToolUpdate;
  if Assigned(FOnShapeOptionChanged) then FOnShapeOptionChanged(self);
end;

procedure TToolManager.SetPenStyle(AValue: TPenStyle);
begin
  if FPenStyle=AValue then Exit;
  FPenStyle:=AValue;
  ToolUpdate;
  if Assigned(FOnPenStyleChanged) then FOnPenStyleChanged(self);
end;

procedure TToolManager.SetPenWidth(AValue: single);
begin
  if AValue < MinPenWidth then AValue := MinPenWidth;
  if AValue > MaxPenWidth then AValue := MaxPenWidth;
  if GetCurrentToolType = ptEraser then
  begin
    if FEraserWidth <> AValue then
    begin
      FEraserWidth := AValue;
      ToolUpdate;
      if Assigned(FOnPenWidthChanged) then FOnPenWidthChanged(self);
    end;
  end else
  begin
    if FNormalPenWidth <> AValue then
    begin
      FNormalPenWidth := AValue;
      ToolUpdate;
      if Assigned(FOnPenWidthChanged) then FOnPenWidthChanged(self);
    end;
  end;
end;

procedure TToolManager.SetShapeRatio(AValue: Single);
begin
  if FShapeRatio=AValue then Exit;
  FShapeRatio:=AValue;
  ToolUpdate;
  if Assigned(FOnShapeRatioChanged) then FOnShapeRatioChanged(self);
end;

procedure TToolManager.SetSplineStyle(AValue: TSplineStyle);
begin
  if FSplineStyle=AValue then Exit;
  FSplineStyle:=AValue;
  ToolUpdate;
  if Assigned(FOnSplineStyleChanged) then FOnSplineStyleChanged(self);
end;

procedure TToolManager.SetTextAlign(AValue: TAlignment);
begin
  if FTextAlign=AValue then Exit;
  FTextAlign:=AValue;
  ToolUpdate;
  if Assigned(FOnTextAlignChanged) then FOnTextAlignChanged(self);
end;

procedure TToolManager.SetTextVerticalAlign(AValue: TTextLayout);
begin
  if FTextVerticalAlign=AValue then Exit;
  FTextVerticalAlign:=AValue;
  ToolUpdate;
  if Assigned(FOnTextAlignChanged) then FOnTextAlignChanged(self);
end;

procedure TToolManager.SetTextBidiMode(AValue: TFontBidiMode);
begin
  if FTextBidiMode=AValue then Exit;
  FTextBidiMode:=AValue;
  ToolUpdate;
  if Assigned(FOnTextAlignChanged) then FOnTextAlignChanged(self);
end;

procedure TToolManager.SetTextFontStyle(AValue: TFontStyles);
begin
  if FTextFontStyle=AValue then Exit;
  FTextFontStyle:=AValue;
  ToolUpdate;
  if Assigned(FOnTextFontChanged) then FOnTextFontChanged(self);
end;

procedure TToolManager.SetTextPhong(AValue: boolean);
begin
  if FTextPhong=AValue then Exit;
  FTextPhong:=AValue;
  ToolUpdate;
  if Assigned(FOnTextPhongChanged) then FOnTextPhongChanged(self);
end;

procedure TToolManager.SetTextShadow(AValue: boolean);
begin
  if FTextShadow=AValue then Exit;
  FTextShadow:=AValue;
  ToolUpdate;
  if Assigned(FOnTextShadowChanged) then FOnTextShadowChanged(self);
end;

procedure TToolManager.SetTextShadowBlurRadius(AValue: single);
begin
  if FTextShadowBlurRadius=AValue then Exit;
  FTextShadowBlurRadius:=AValue;
  ToolUpdate;
  if Assigned(FOnTextShadowChanged) then FOnTextShadowChanged(self);
end;

procedure TToolManager.SetTextShadowOffset(AValue: TPoint);
begin
  if FTextShadowOffset=AValue then Exit;
  FTextShadowOffset:=AValue;
  ToolUpdate;
  if Assigned(FOnTextShadowChanged) then FOnTextShadowChanged(self);
end;

procedure TToolManager.SetTolerance(AValue: byte);
begin
  if FTolerance=AValue then Exit;
  FTolerance:=AValue;
  ToolUpdate;
  if Assigned(FOnToleranceChanged) then FOnToleranceChanged(self);
end;

function TToolManager.CheckExitTool: boolean;
begin
  if FShouldExitTool then
  begin
    FShouldExitTool:= false;
    if FCurrentToolType in[ptRect,ptEllipse,ptPolygon,ptSpline,ptText,ptPhong,ptGradient] then
      SetCurrentToolType(ptEditShape)
    else
      SetCurrentToolType(ptHand);
    result := true;
  end else
    result := false;
end;

procedure TToolManager.NotifyImageOrSelectionChanged(ALayer: TBGRABitmap; ARect: TRect);
begin
  if (CurrentTool <> nil) and not IsRectEmpty(ARect) then
  begin
    if Assigned(CurrentTool.FAction) then
      if not IsOnlyRenderChange(ARect) then
        CurrentTool.FAction.NotifyChange(ALayer, ARect);

    if Assigned(ALayer) then
    begin
      if ALayer = Image.CurrentLayerReadOnly then
        Image.ImageMayChange(AddLayerOffset(ARect))
      else
        Image.LayerMayChange(ALayer, ARect);
    end
  end;
end;

function TToolManager.ToolCanBeUsed: boolean;
begin
  result := (FCurrentToolType = ptHand) or ((CurrentTool <> nil) and (CurrentTool.IsSelectingTool or Image.CurrentLayerVisible));
end;

function TToolManager.ToolHasLineCap: boolean;
var
  contextualToolbars: TContextualToolbars;
begin
  contextualToolbars := GetContextualToolbars;
  result := (ctLineCap in contextualToolbars) and CurrentTool.HasPen and
            (not (toCloseShape in ShapeOptions) or not (ctCloseShape in contextualToolbars));
end;

function TToolManager.GetBackColor: TBGRAPixel;
begin
  if BlackAndWhite then
    result := BGRAToGrayscale(FBackFill.AverageColor)
  else
    result := FBackFill.AverageColor;
end;

function TToolManager.GetBrushAt(AIndex: integer): TLazPaintBrush;
begin
  if (FBrushInfoList = nil) or (AIndex < 0) or (AIndex >= FBrushInfoList.Count) then
    result := nil
  else
    result := TObject(FBrushInfoList[AIndex]) as TLazPaintBrush;
end;

function TToolManager.GetBrushCount: integer;
begin
  if Assigned(FBrushInfoList) then
    result := FBrushInfoList.Count
  else
    result := 0;
end;

function TToolManager.GetBrushInfo: TLazPaintBrush;
begin
  if (FBrushIndex < 0) or (FBrushIndex > FBrushInfoList.Count) then
    FBrushIndex := 0;
  if FBrushIndex > FBrushInfoList.Count then
    result := nil
  else
    result := TObject(FBrushInfoList[FBrushIndex]) as TLazPaintBrush;
end;

function TToolManager.GetCursor: TCursor;
var toolCursor: TCursor;
begin
  case GetCurrentToolType of
  ptHand, ptMoveSelection, ptZoomLayer: result := crSizeAll;
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

procedure TToolManager.FillChange(ASender: TObject;
  var ADiff: TCustomVectorialFillDiff);
begin
  if FInToolUpdate or FInSwapFill then exit;
  ToolUpdate;
  if Assigned(FOnFillChanged) then FOnFillChanged(self);
  if (ASender = FBackFill) and (FBackFill.FillType = vftGradient) then
  begin
    FBackLastGradient.Free;
    FBackLastGradient := FBackFill.Gradient.Duplicate as TBGRALayerGradientOriginal;
  end else
  if (ASender = FForeFill) and (FForeFill.FillType = vftGradient) then
  begin
    FForeLastGradient.Free;
    FForeLastGradient := FForeFill.Gradient.Duplicate as TBGRALayerGradientOriginal;
  end else
  if (ASender = FOutlineFill) and (FOutlineFill.FillType = vftGradient) then
  begin
    FOutlineLastGradient.Free;
    FOutlineLastGradient := FOutlineFill.Gradient.Duplicate as TBGRALayerGradientOriginal;
  end;
end;

function TToolManager.GetAllowedBackFillTypes: TVectorialFillTypes;
begin
  if Assigned(CurrentTool) then result := CurrentTool.AllowedBackFillTypes
  else result := [vftSolid,vftGradient,vftTexture];
end;

function TToolManager.GetAllowedForeFillTypes: TVectorialFillTypes;
begin
  if Assigned(CurrentTool) then result := CurrentTool.AllowedForeFillTypes
  else result := [vftSolid,vftGradient,vftTexture];
end;

function TToolManager.GetAllowedOutlineFillTypes: TVectorialFillTypes;
begin
  if Assigned(CurrentTool) then result := CurrentTool.AllowedOutlineFillTypes
  else result := [vftSolid,vftGradient,vftTexture];
end;

function TToolManager.GetForeColor: TBGRAPixel;
begin
  if BlackAndWhite then
    result := BGRAToGrayscale(FForeFill.AverageColor)
  else
    result := FForeFill.AverageColor;
end;

function TToolManager.GetMaxDeformationGridSize: TSize;
begin
  result.cx := Max(MinDeformationGridSize,Min(image.Width div 2,50)+1);
  result.cy := Max(MinDeformationGridSize,Min(image.Height div 2,50)+1);
end;

function TToolManager.GetOutlineColor: TBGRAPixel;
begin
  if BlackAndWhite then
    result := BGRAToGrayscale(FOutlineFill.AverageColor)
  else
    result := FOutlineFill.AverageColor;
end;

function TToolManager.GetShapeOptionAliasing: boolean;
begin
  result := toAliasing in FShapeOptions;
end;

function TToolManager.GetPenWidth: single;
begin
  if GetCurrentToolType = ptEraser then
    result := FEraserWidth else result := FNormalPenWidth;
end;

function TToolManager.GetToolSleeping: boolean;
begin
  result := FSleepingTool <> nil;
end;

function TToolManager.GetTextFontName: string;
begin
  result := FTextFontName;
end;

function TToolManager.GetTextFontSize: single;
begin
  result := FTextFontSize;
end;

function TToolManager.GetTextFontStyle: TFontStyles;
begin
  result := FTextFontStyle;
end;

function TToolManager.ScriptGetAliasing(AVars: TVariableSet): TScriptResult;
begin
  AVars.Booleans['Result'] := toAliasing in ShapeOptions;
  result := srOk;
end;

function TToolManager.ScriptGetArrowEnd(AVars: TVariableSet): TScriptResult;
begin
  AVars.Strings['Result'] := CSSToPascalCase(ArrowKindToStr[ArrowEnd]);
  result := srOk;
end;

function TToolManager.ScriptGetArrowSize(AVars: TVariableSet): TScriptResult;
begin
  AVars.Points2D['Result'] := ArrowSize;
  result := srOk;
end;

function TToolManager.ScriptGetArrowStart(AVars: TVariableSet): TScriptResult;
begin
  AVars.Strings['Result'] := CSSToPascalCase(ArrowKindToStr[ArrowStart]);
  result := srOk;
end;

function TToolManager.ScriptGetBackColor(AVars: TVariableSet): TScriptResult;
begin
  AVars.Pixels['Result'] := BackColor;
  result := srOk;
end;

function TToolManager.ScriptGetOutlineColor(AVars: TVariableSet): TScriptResult;
begin
  AVars.Pixels['Result'] := OutlineColor;
  result := srOk;
end;

function TToolManager.ScriptGetBrushCount(AVars: TVariableSet): TScriptResult;
begin
  AVars.Integers['Result'] := BrushCount;
  result := srOk;
end;

function TToolManager.ScriptGetBrushIndex(AVars: TVariableSet): TScriptResult;
begin
  AVars.Integers['Result'] := BrushIndex;
  result := srOk;
end;

function TToolManager.ScriptGetBrushSpacing(AVars: TVariableSet): TScriptResult;
begin
  AVars.Integers['Result'] := BrushSpacing;
  result := srOk;
end;

function TToolManager.ScriptGetDeformationGridMode(AVars: TVariableSet): TScriptResult;
begin
  result := srOk;
  case DeformationGridMode of
  gmDeform: AVars.Strings['Result'] := 'Deform';
  gmMovePointWithoutDeformation: AVars.Strings['Result'] := 'MovePointWithoutDeformation';
  else result := srException;
  end;
end;

function TToolManager.ScriptGetDeformationGridSize(AVars: TVariableSet): TScriptResult;
begin
  result := srOk;
  with DeformationGridSize do
    AVars.Points2D['Result'] := PointF(cx,cy);
end;

function TToolManager.ScriptGetEraserAlpha(AVars: TVariableSet): TScriptResult;
begin
  AVars.Integers['Result'] := EraserAlpha;
  result := srOk;
end;

function TToolManager.ScriptGetEraserMode(AVars: TVariableSet): TScriptResult;
begin
  result := srOk;
  case EraserMode of
  emEraseAlpha: AVars.Strings['Result'] := 'EraseAlpha';
  emSharpen: AVars.Strings['Result'] := 'Sharpen';
  emSoften: AVars.Strings['Result'] := 'Soften';
  emLighten: AVars.Strings['Result'] := 'Lighten';
  emDarken: AVars.Strings['Result'] := 'Darken';
  else result := srException;
  end;
end;

function TToolManager.ScriptGetFloodFillOptions(AVars: TVariableSet): TScriptResult;
var
  optionsVar: TScriptVariableReference;
  option: TFloodFillOption;
begin
  optionsVar := AVars.AddStringList('Result');
  for option := low(TFloodFillOption) to high(TFloodFillOption) do
    if option in FloodFillOptions then
    case option of
    ffProgressive: AVars.AppendString(optionsVar, 'Progressive');
    ffFillAll: Avars.AppendString(optionsVar, 'FillAll');
    end;
  result := srOk;
end;

function TToolManager.ScriptGetFontName(AVars: TVariableSet): TScriptResult;
begin
  AVars.Strings['Name'] := TextFontName;
  result := srOk;
end;

function TToolManager.ScriptGetFontSize(AVars: TVariableSet): TScriptResult;
begin
  AVars.Floats['Result'] := TextFontSize;
  result := srOk;
end;

function TToolManager.ScriptGetFontStyle(AVars: TVariableSet): TScriptResult;
var
  styles: TScriptVariableReference;
  style: TFontStyle;
begin
  styles := AVars.AddStringList('Result');
  for style := low(TFontStyle) to high(TFontStyle) do
    if style in TextFontStyle then
    case style of
    fsBold: AVars.AppendString(styles, 'Bold');
    fsItalic: Avars.AppendString(styles, 'Italic');
    fsUnderline: Avars.AppendString(styles, 'Underline');
    fsStrikeOut: Avars.AppendString(styles, 'StrikeOut');
    end;
  result := srOk;
end;

function TToolManager.ScriptGetGradientInterpolation(AVars: TVariableSet; AFill: TVectorialFill): TScriptResult;
begin
  result := srOk;
  if AFill.FillType <> vftGradient then result := srException else
  AVars.Strings['Result'] := GradientInterpolationToStr(AFill.Gradient.ColorInterpolation);
end;

function TToolManager.ScriptGetGradientRepetition(AVars: TVariableSet; AFill: TVectorialFill): TScriptResult;
begin
  result := srOk;
  if AFill.FillType <> vftGradient then result := srException else
  AVars.Strings['Result'] := GradientRepetitionToStr(AFill.Gradient.Repetition);
end;

function TToolManager.ScriptGetGradientType(AVars: TVariableSet; AFill: TVectorialFill): TScriptResult;
begin
  result := srOk;
  if AFill.FillType <> vftGradient then result := srException else
  AVars.Strings['Result'] := GradientTypeStr[AFill.Gradient.GradientType];
end;

function TToolManager.ScriptGetGradientColors(AVars: TVariableSet;
  AFill: TVectorialFill): TScriptResult;
var
  colors: TScriptVariableReference;
begin
  result := srOk;
  if AFill.FillType <> vftGradient then result := srException else
  begin
    colors := AVars.AddPixelList('Result');
    TVariableSet.AppendPixel(colors, AFill.Gradient.StartColor);
    TVariableSet.AppendPixel(colors, AFill.Gradient.EndColor);
  end;
end;

function TToolManager.ScriptGetBackGradientInterpolation(AVars: TVariableSet): TScriptResult;
begin
  result := ScriptGetGradientInterpolation(AVars, FBackFill);
end;

function TToolManager.ScriptGetBackGradientRepetition(AVars: TVariableSet): TScriptResult;
begin
  result := ScriptGetGradientRepetition(AVars, FBackFill);
end;

function TToolManager.ScriptGetBackGradientType(AVars: TVariableSet): TScriptResult;
begin
  result := ScriptGetGradientType(AVars, FBackFill);
end;

function TToolManager.ScriptGetBackGradientColors(AVars: TVariableSet): TScriptResult;
begin
  result := ScriptGetGradientColors(AVars, FBackFill);
end;

function TToolManager.ScriptGetForeGradientInterpolation(AVars: TVariableSet): TScriptResult;
begin
  result := ScriptGetGradientInterpolation(AVars, FForeFill);
end;

function TToolManager.ScriptGetForeGradientRepetition(AVars: TVariableSet): TScriptResult;
begin
  result := ScriptGetGradientRepetition(AVars, FForeFill);
end;

function TToolManager.ScriptGetForeGradientType(AVars: TVariableSet): TScriptResult;
begin
  result := ScriptGetGradientType(AVars, FForeFill);
end;

function TToolManager.ScriptGetForeGradientColors(AVars: TVariableSet): TScriptResult;
begin
  result := ScriptGetGradientColors(AVars, FForeFill);
end;

function TToolManager.ScriptGetOutlineGradientInterpolation(AVars: TVariableSet): TScriptResult;
begin
  result := ScriptGetGradientInterpolation(AVars, FOutlineFill);
end;

function TToolManager.ScriptGetOutlineGradientRepetition(AVars: TVariableSet): TScriptResult;
begin
  result := ScriptGetGradientRepetition(AVars, FOutlineFill);
end;

function TToolManager.ScriptGetOutlineGradientType(AVars: TVariableSet): TScriptResult;
begin
  result := ScriptGetGradientType(AVars, FOutlineFill);
end;

function TToolManager.ScriptGetOutlineGradientColors(AVars: TVariableSet): TScriptResult;
begin
  result := ScriptGetGradientColors(AVars, FOutlineFill);
end;

function TToolManager.ScriptGetTextureRepetition(AVars: TVariableSet;
  AFill: TVectorialFill): TScriptResult;
begin
  if AFill.FillType <> vftTexture then exit(srException);
  result := srOk;
  case AFill.TextureRepetition of
  trNone: AVars.Strings['Result'] := 'None';
  trRepeatX: AVars.Strings['Result'] := 'RepeatX';
  trRepeatY: AVars.Strings['Result'] := 'RepeatY';
  trRepeatBoth: AVars.Strings['Result'] := 'RepeatBoth';
  else
    result := srException;
  end;
end;

function TToolManager.ScriptGetTextureOpacity(AVars: TVariableSet;
  AFill: TVectorialFill): TScriptResult;
begin
  if AFill.FillType <> vftTexture then exit(srException);
  AVars.Integers['Result'] := AFill.TextureOpacity;
  result := srOk;
end;

function TToolManager.ScriptGetBackTextureRepetition(AVars: TVariableSet): TScriptResult;
begin
  result := ScriptGetTextureRepetition(AVars, BackFill);
end;

function TToolManager.ScriptGetBackTextureOpacity(AVars: TVariableSet): TScriptResult;
begin
  result := ScriptGetTextureOpacity(AVars, BackFill);
end;

function TToolManager.ScriptGetForeTextureRepetition(AVars: TVariableSet): TScriptResult;
begin
  result := ScriptGetTextureRepetition(AVars, ForeFill);
end;

function TToolManager.ScriptGetForeTextureOpacity(AVars: TVariableSet): TScriptResult;
begin
  result := ScriptGetTextureOpacity(AVars, ForeFill);
end;

function TToolManager.ScriptGetOutlineTextureRepetition(AVars: TVariableSet): TScriptResult;
begin
  result := ScriptGetTextureRepetition(AVars, OutlineFill);
end;

function TToolManager.ScriptGetOutlineTextureOpacity(AVars: TVariableSet): TScriptResult;
begin
  result := ScriptGetTextureOpacity(AVars, OutlineFill);
end;

function TToolManager.ScriptGetJoinStyle(AVars: TVariableSet): TScriptResult;
begin
  result := srOk;
  case JoinStyle of
  pjsBevel: AVars.Strings['Result'] := 'Bevel';
  pjsRound: AVars.Strings['Result'] := 'Round';
  pjsMiter: AVars.Strings['Result'] := 'Miter';
  else result := srException;
  end;
end;

function TToolManager.ScriptGetLightPosition(AVars: TVariableSet): TScriptResult;
begin
  AVars.Points2D['Result'] := LightPosition;
  result := srOk;
end;

function TToolManager.ScriptGetLineCap(AVars: TVariableSet): TScriptResult;
begin
  case LineCap of
  pecSquare: AVars.Strings['Result'] := 'Square';
  pecRound: AVars.Strings['Result'] := 'Round';
  pecFlat: AVars.Strings['Result'] := 'Flat';
  else exit(srException);
  end;
  result := srOk;
end;

function TToolManager.ScriptGetForeColor(AVars: TVariableSet): TScriptResult;
begin
  AVars.Pixels['Result'] := ForeColor;
  result := srOk;
end;

function TToolManager.ScriptGetPenStyle(AVars: TVariableSet): TScriptResult;
begin
  result := srOk;
  case PenStyle of
  psSolid: AVars.Strings['Result'] := 'Solid';
  psDash: AVars.Strings['Result'] := 'Dash';
  psDot: AVars.Strings['Result'] := 'Dot';
  psDashDot: AVars.Strings['Result'] := 'DashDot';
  psDashDotDot: AVars.Strings['Result'] := 'DashDotDot';
  else result := srException;
  end;
end;

function TToolManager.ScriptGetPenWidth(AVars: TVariableSet): TScriptResult;
begin
  AVars.Floats['Result'] := PenWidth;
  result := srOk;
end;

function TToolManager.ScriptGetPerspectiveOptions(AVars: TVariableSet): TScriptResult;
var
  optionsVar: TScriptVariableReference;
  option: TPerspectiveOption;
begin
  optionsVar := AVars.AddStringList('Result');
  for option := low(TPerspectiveOption) to high(TPerspectiveOption) do
    if option in PerspectiveOptions then
    case option of
    poRepeat: Avars.AppendString(optionsVar, 'Repeat');
    poTwoPlanes: Avars.AppendString(optionsVar, 'TwoPlanes');
    end;
  result := srOk;
end;

function TToolManager.ScriptGetPhongShapeAltitude(AVars: TVariableSet): TScriptResult;
begin
  result := srOk;
  AVars.Integers['Result'] := PhongShapeAltitude;
end;

function TToolManager.ScriptGetPhongShapeBorderSize(AVars: TVariableSet): TScriptResult;
begin
  result := srOk;
  AVars.Integers['Result'] := PhongShapeBorderSize;
end;

function TToolManager.ScriptGetPhongShapeKind(AVars: TVariableSet): TScriptResult;
begin
  result := srOk;
  case PhongShapeKind of
  pskRectangle: AVars.Strings['Result'] := 'Rectangle';
  pskRoundRectangle: AVars.Strings['Result'] := 'RoundRectangle';
  pskHalfSphere: AVars.Strings['Result'] := 'HalfSphere';
  pskConeTop: AVars.Strings['Result'] := 'ConeTop';
  pskConeSide: AVars.Strings['Result'] := 'ConeSide';
  pskHorizCylinder: AVars.Strings['Result'] := 'HorizCylinder';
  pskVertCylinder: AVars.Strings['Result'] := 'VertCylinder';
  else result := srException;
  end;
end;

function TToolManager.ScriptGetShapeOptions(AVars: TVariableSet): TScriptResult;
var
  options: TScriptVariableReference;
  opt: TShapeOption;
begin
  options := AVars.AddStringList('Result');
  for opt := low(TShapeOption) to high(TShapeOption) do
    if opt in ShapeOptions then
    case opt of
    toDrawShape: Avars.AppendString(options, 'DrawShape');
    toFillShape: Avars.AppendString(options, 'FillShape');
    toCloseShape: Avars.AppendString(options, 'CloseShape');
    end;
  result := srOk;
end;

function TToolManager.ScriptGetShapeRatio(AVars: TVariableSet): TScriptResult;
begin
  AVars.Floats['Result'] := ShapeRatio;
  result := srOk;
end;

function TToolManager.ScriptGetSplineStyle(AVars: TVariableSet): TScriptResult;
var
  s: String;
begin
  case SplineStyle of
    ssInside: s := 'Inside';
    ssInsideWithEnds: s := 'InsideWithEnds';
    ssCrossing: s := 'Crossing';
    ssCrossingWithEnds: s := 'CrossingWithEnds';
    ssOutside: s := 'Outside';
    ssRoundOutside: s := 'RoundOutside';
    ssVertexToSide: s := 'VertexToSide';
    ssEasyBezier: s := 'EasyBezier';
  else
    exit(srException);
  end;
  AVars.Strings['Result'] := s;
  result := srOk;
end;

function TToolManager.ScriptGetTextAlign(AVars: TVariableSet): TScriptResult;
begin
  case TextAlign of
  taLeftJustify: AVars.Strings['Result'] := 'Left';
  taCenter: AVars.Strings['Result'] := 'Center';
  taRightJustify: AVars.Strings['Result'] := 'Right';
  else exit(srException);
  end;
  result := srOk;
end;

function TToolManager.ScriptGetTextVerticalAlign(AVars: TVariableSet): TScriptResult;
begin
  case TextVerticalAlign of
  tlTop: AVars.Strings['Result'] := 'Top';
  tlCenter: AVars.Strings['Result'] := 'Middle';
  tlBottom: AVars.Strings['Result'] := 'Bottom';
  else exit(srException);
  end;
  result := srOk;
end;

function TToolManager.ScriptGetTextBidiMode(AVars: TVariableSet): TScriptResult;
begin
  case TextBidiMode of
  fbmAuto: AVars.Strings['Result'] := 'BidiAuto';
  fbmLeftToRight: AVars.Strings['Result'] := 'LeftToRight';
  fbmRightToLeft: AVars.Strings['Result'] := 'RightToLeft';
  else exit(srException);
  end;
  result := srOk;
end;

function TToolManager.ScriptGetTextOutline(AVars: TVariableSet): TScriptResult;
begin
  if TextOutline then
    AVars.Floats['Result'] := TextOutlineWidth
  else
    AVars.Floats['Result'] := 0;
  result := srOk;
end;

function TToolManager.ScriptGetTextPhong(AVars: TVariableSet): TScriptResult;
begin
  AVars.Booleans['Result'] := TextPhong;
  result := srOk;
end;

function TToolManager.ScriptGetTolerance(AVars: TVariableSet): TScriptResult;
begin
  AVars.Integers['Result'] := Tolerance;
  result := srOk;
end;

function TToolManager.ScriptSetAliasing(AVars: TVariableSet): TScriptResult;
begin
  if AVars.Booleans['Enabled'] then
    ShapeOptions:= ShapeOptions + [toAliasing]
  else
    ShapeOptions:= ShapeOptions - [toAliasing];
  result := srOk;
end;

function TToolManager.ScriptSetArrowEnd(AVars: TVariableSet): TScriptResult;
var ak: TArrowKind;
  kindStr: String;
begin
  kindStr := PascalToCSSCase(AVars.Strings['Kind']);
  ak := StrToArrowKind(kindStr);
  if (ak = akNone) and (kindStr <> ArrowKindToStr[akNone]) then
    exit(srInvalidParameters);
  ArrowEnd := ak;
  result := srOk;
end;

function TToolManager.ScriptSetArrowSize(AVars: TVariableSet): TScriptResult;
var
  s: TPointF;
begin
  s := AVars.Points2D['Size'];
  if isEmptyPointF(s) then exit(srInvalidParameters);
  ArrowSize := s;
  result := srOk;
end;

function TToolManager.ScriptSetArrowStart(AVars: TVariableSet): TScriptResult;
var ak: TArrowKind;
  kindStr: String;
begin
  kindStr := PascalToCSSCase(AVars.Strings['Kind']);
  ak := StrToArrowKind(kindStr);
  if (ak = akNone) and (kindStr <> ArrowKindToStr[akNone]) then
    exit(srInvalidParameters);
  ArrowStart := ak;
  result := srOk;
end;

function TToolManager.ScriptSetBackColor(AVars: TVariableSet): TScriptResult;
begin
  BackColor := AVars.Pixels['Color'];
  result := srOk;
end;

function TToolManager.ScriptSetOutlineColor(AVars: TVariableSet): TScriptResult;
begin
  OutlineColor := AVars.Pixels['Color'];
  result := srOk;
end;

function TToolManager.ScriptSetBrushIndex(AVars: TVariableSet): TScriptResult;
var
  index: Int64;
begin
  index := AVars.Integers['Index'];
  if (index < 0) or (index >= BrushCount) then exit(srException);
  BrushIndex:= index;
  result := srOk;
end;

function TToolManager.ScriptSetBrushSpacing(AVars: TVariableSet): TScriptResult;
begin
  BrushSpacing := AVars.Integers['Spacing'];
  result := srOk;
end;

function TToolManager.ScriptSetDeformationGridMode(AVars: TVariableSet): TScriptResult;
begin
  result := srOk;
  case AVars.Strings['Mode'] of
  'Deform': DeformationGridMode := gmDeform;
  'MovePointWithoutDeformation': DeformationGridMode := gmMovePointWithoutDeformation;
  else result := srInvalidParameters;
  end;
end;

function TToolManager.ScriptSetDeformationGridSize(AVars: TVariableSet): TScriptResult;
var
  s: TPointF;
begin
  s := AVars.Points2D['Size'];
  if s.x < MinDeformationGridSize then exit(srInvalidParameters);
  if s.y < MinDeformationGridSize then exit(srInvalidParameters);
  DeformationGridSize := Size(round(s.x),round(s.y));
  result := srOk;
end;

function TToolManager.ScriptSetEraserAlpha(AVars: TVariableSet): TScriptResult;
var
  alpha: Int64;
begin
  alpha := AVars.Integers['Alpha'];
  if alpha < 0 then alpha := 0;
  if alpha > 255 then alpha := 255;
  EraserAlpha:= alpha;
  result := srOk;
end;

function TToolManager.ScriptSetEraserMode(AVars: TVariableSet): TScriptResult;
begin
  result := srOk;
  case AVars.Strings['Mode'] of
  'EraseAlpha': EraserMode:= emEraseAlpha;
  'Soften': EraserMode := emSoften;
  else result := srInvalidParameters;
  end;
end;

function TToolManager.ScriptSetFloodFillOptions(AVars: TVariableSet): TScriptResult;
var optionsSet: TFloodFillOptions;
  optionsVar: TScriptVariableReference;
  i: Integer;
  optionStr: string;
begin
  optionsSet := [];
  optionsVar := AVars.GetVariable('Options');
  for i := 0 to AVars.GetListCount(optionsVar)-1 do
  begin
    optionStr := AVars.GetStringAt(optionsVar, i);
    case optionStr of
    'Progressive': include(optionsSet, ffProgressive);
    'FillAll': include(optionsSet, ffFillAll);
    else exit(srInvalidParameters);
    end;
  end;
  FloodFillOptions:= optionsSet;
  result := srOk;
end;

function TToolManager.ScriptSetFontName(AVars: TVariableSet): TScriptResult;
begin
  SetTextFont(AVars.Strings['Name'], TextFontSize, TextFontStyle);
  result := srOk;
end;

function TToolManager.ScriptSetFontSize(AVars: TVariableSet): TScriptResult;
begin
  SetTextFont(TextFontName, AVars.Floats['Size'], TextFontStyle);
  result := srOk;
end;

function TToolManager.ScriptSetFontStyle(AVars: TVariableSet): TScriptResult;
var style: TFontStyles;
  styles: TScriptVariableReference;
  i: Integer;
  styleStr: string;
begin
  style := [];
  styles := AVars.GetVariable('Style');
  for i := 0 to AVars.GetListCount(styles)-1 do
  begin
    styleStr := AVars.GetStringAt(styles, i);
    case styleStr of
    'Bold': include(style, fsBold);
    'Italic': include(style, fsItalic);
    'Underline': include(style, fsUnderline);
    'StrikeOut': include(style, fsStrikeOut);
    else exit(srInvalidParameters);
    end;
  end;
  SetTextFont(TextFontName, TextFontSize, style);
  result := srOk;
end;

function TToolManager.ScriptSetGradientInterpolation(AVars: TVariableSet; AFill: TVectorialFill): TScriptResult;
var
  ci: TBGRAColorInterpolation;
begin
  if AFill.FillType <> vftGradient then exit(srException);
  result := srOk;
  ci := StrToGradientInterpolation(AVars.Strings['Interpolation']);
  if GradientInterpolationToStr(ci) <> AVars.Strings['Interpolation'] then
    result := srInvalidParameters
  else AFill.Gradient.ColorInterpolation:= ci;
end;

function TToolManager.ScriptSetGradientRepetition(AVars: TVariableSet; AFill: TVectorialFill): TScriptResult;
var
  gr: TBGRAGradientRepetition;
begin
  if AFill.FillType <> vftGradient then exit(srException);
  result := srOk;
  gr := StrToGradientRepetition(AVars.Strings['Repetition']);
  if GradientRepetitionToStr(gr) <> AVars.Strings['Repetition'] then
    result := srInvalidParameters
  else AFill.Gradient.Repetition:= gr;
end;

function TToolManager.ScriptSetGradientType(AVars: TVariableSet; AFill: TVectorialFill): TScriptResult;
var
  gt: TGradientType;
  b: TAffineBox;
  lastGrad: TBGRALayerGradientOriginal;
begin
  result := srOk;
  gt := StrToGradientType(AVars.Strings['GradientType']);
  if GradientTypeStr[gt] <> AVars.Strings['GradientType'] then
    exit(srInvalidParameters);
  if AFill.FillType = vftGradient then
    AFill.Gradient.GradientType:= gt
  else
  begin
    if AFill = BackFill then lastGrad := FBackLastGradient
    else if AFill = OutlineFill then lastGrad := FOutlineLastGradient
    else lastGrad := FForeLastGradient;

    lastGrad.GradientType:= gt;
    b := SuggestGradientBox;
    if gt = gtLinear then lastGrad.Origin := b.TopLeft else
      lastGrad.Origin := (b.TopLeft+b.BottomRight)*0.5;
    lastGrad.XAxis := b.BottomRight;
    lastGrad.YAxis := EmptyPointF;
    lastGrad.FocalPoint := EmptyPointF;
    lastGrad.Radius := 1;
    lastGrad.FocalRadius := 0;
    AFill.SetGradient(lastGrad, False);
  end;
end;

function TToolManager.ScriptSetGradientColors(AVars: TVariableSet;
  AFill: TVectorialFill): TScriptResult;
var
  colors: TScriptVariableReference;
  lastGrad: TBGRALayerGradientOriginal;
  b: TAffineBox;
begin
  result := srOk;
  colors := AVars.GetVariable('Colors');
  if TVariableSet.GetListCount(colors) <> 2 then
    exit(srInvalidParameters);

  if AFill.FillType = vftGradient then
    AFill.Gradient.SetColors(TVariableSet.GetPixelAt(colors, 0), TVariableSet.GetPixelAt(colors, 1))
  else
  begin
    if AFill = BackFill then lastGrad := FBackLastGradient
    else if AFill = OutlineFill then lastGrad := FOutlineLastGradient
    else lastGrad := FForeLastGradient;

    b := SuggestGradientBox;
    if lastGrad.GradientType = gtLinear then lastGrad.Origin := b.TopLeft else
      lastGrad.Origin := (b.TopLeft+b.BottomRight)*0.5;
    lastGrad.XAxis := b.BottomRight;
    lastGrad.YAxis := EmptyPointF;
    lastGrad.FocalPoint := EmptyPointF;
    lastGrad.Radius := 1;
    lastGrad.FocalRadius := 0;
    lastGrad.SetColors(TVariableSet.GetPixelAt(colors, 0), TVariableSet.GetPixelAt(colors, 1));
    AFill.SetGradient(lastGrad, False);
  end;
end;

function TToolManager.ScriptSetBackGradientInterpolation(AVars: TVariableSet): TScriptResult;
begin
  result := ScriptSetGradientInterpolation(AVars, FBackFill);
end;

function TToolManager.ScriptSetBackGradientRepetition(AVars: TVariableSet): TScriptResult;
begin
  result := ScriptSetGradientRepetition(AVars, FBackFill);
end;

function TToolManager.ScriptSetBackGradientType(AVars: TVariableSet): TScriptResult;
begin
  result := ScriptSetGradientType(AVars, FBackFill);
end;

function TToolManager.ScriptSetBackGradientColors(AVars: TVariableSet): TScriptResult;
begin
  result := ScriptSetGradientColors(AVars, FBackFill);
end;

function TToolManager.ScriptSetOutlineGradientInterpolation(AVars: TVariableSet): TScriptResult;
begin
  result := ScriptSetGradientInterpolation(AVars, FOutlineFill);
end;

function TToolManager.ScriptSetOutlineGradientRepetition(AVars: TVariableSet): TScriptResult;
begin
  result := ScriptSetGradientRepetition(AVars, FOutlineFill);
end;

function TToolManager.ScriptSetOutlineGradientType(AVars: TVariableSet): TScriptResult;
begin
  result := ScriptSetGradientType(AVars, FOutlineFill);
end;

function TToolManager.ScriptSetOutlineGradientColors(AVars: TVariableSet): TScriptResult;
begin
  result := ScriptSetGradientColors(AVars, FOutlineFill);
end;

function TToolManager.ScriptSetForeGradientInterpolation(AVars: TVariableSet): TScriptResult;
begin
  result := ScriptSetGradientInterpolation(AVars, FForeFill);
end;

function TToolManager.ScriptSetForeGradientRepetition(AVars: TVariableSet): TScriptResult;
begin
  result := ScriptSetGradientRepetition(AVars, FForeFill);
end;

function TToolManager.ScriptSetForeGradientType(AVars: TVariableSet): TScriptResult;
begin
  result := ScriptSetGradientType(AVars, FForeFill);
end;

function TToolManager.ScriptSetForeGradientColors(AVars: TVariableSet): TScriptResult;
begin
  result := ScriptSetGradientColors(AVars, FForeFill);
end;

function TToolManager.ScriptSetTexture(AVars: TVariableSet;
  AFill: TVectorialFill): TScriptResult;
var
  fileName: String;
  flatImg: TBGRABitmap;
begin
  fileName := trim(AVars.Strings['FileName']);
  if fileName='' then exit(srInvalidParameters);
  flatImg := LoadFlatImageUTF8(fileName).bmp;
  if flatImg = nil then exit(srException);
  try
    if AFill.FillType <> vftTexture then
      AFill.SetTexture(flatImg, AffineMatrixIdentity)
    else
      AFill.SetTexture(flatImg, AffineMatrixIdentity, AFill.TextureOpacity, AFill.TextureRepetition);
    result := srOk;
  finally
    flatImg.FreeReference;
  end;
end;

function TToolManager.ScriptSetTextureRepetition(AVars: TVariableSet;
  AFill: TVectorialFill): TScriptResult;
begin
  if AFill.FillType <> vftTexture then exit(srException);
  case AVars.Strings['Repetition'] of
  'None': AFill.TextureRepetition:= trNone;
  'RepeatX': AFill.TextureRepetition:= trRepeatX;
  'RepeatY': AFill.TextureRepetition:= trRepeatY;
  'RepeatBoth': AFill.TextureRepetition:= trRepeatBoth;
  else exit(srInvalidParameters);
  end;
  result := srOk;
end;

function TToolManager.ScriptSetTextureOpacity(AVars: TVariableSet;
  AFill: TVectorialFill): TScriptResult;
begin
  if AFill.FillType <> vftTexture then exit(srException);
  AFill.TextureOpacity := min(255, max(0, AVars.Integers['Opacity']));
  result := srOk;
end;

function TToolManager.ScriptSetBackTexture(AVars: TVariableSet): TScriptResult;
begin
  result := ScriptSetTexture(AVars, BackFill);
end;

function TToolManager.ScriptSetBackTextureRepetition(AVars: TVariableSet): TScriptResult;
begin
  result := ScriptSetTextureRepetition(AVars, BackFill);
end;

function TToolManager.ScriptSetBackTextureOpacity(AVars: TVariableSet): TScriptResult;
begin
  result := ScriptSetTextureOpacity(AVars, BackFill);
end;

function TToolManager.ScriptSetForeTexture(AVars: TVariableSet): TScriptResult;
begin
  result := ScriptSetTexture(AVars, ForeFill);
end;

function TToolManager.ScriptSetForeTextureRepetition(AVars: TVariableSet): TScriptResult;
begin
  result := ScriptSetTextureRepetition(AVars, ForeFill);
end;

function TToolManager.ScriptSetForeTextureOpacity(AVars: TVariableSet): TScriptResult;
begin
  result := ScriptSetTextureOpacity(AVars, ForeFill);
end;

function TToolManager.ScriptSetOutlineTexture(AVars: TVariableSet): TScriptResult;
begin
  result := ScriptSetTexture(AVars, OutlineFill);
end;

function TToolManager.ScriptSetOutlineTextureRepetition(AVars: TVariableSet): TScriptResult;
begin
  result := ScriptSetTextureRepetition(AVars, OutlineFill);
end;

function TToolManager.ScriptSetOutlineTextureOpacity(AVars: TVariableSet): TScriptResult;
begin
  result := ScriptSetTextureOpacity(AVars, OutlineFill);
end;

function TToolManager.ScriptSetJoinStyle(AVars: TVariableSet): TScriptResult;
begin
  result := srOk;
  case AVars.Strings['Style'] of
  'Bevel': JoinStyle := pjsBevel;
  'Miter': JoinStyle := pjsMiter;
  'Round': JoinStyle := pjsRound;
  else result := srInvalidParameters;
  end;
end;

function TToolManager.ScriptSetLightPosition(AVars: TVariableSet): TScriptResult;
var
  ptF: TPointF;
begin
  ptF := AVars.Points2D['Position'];
  if IsEmptyPointF(ptF) then exit(srInvalidParameters);
  LightPosition := ptF;
  result := srOk;
end;

function TToolManager.ScriptSetLineCap(AVars: TVariableSet): TScriptResult;
var
  capStr: String;
begin
  capStr := AVars.Strings['Cap'];
  case capStr of
  'Round': LineCap := pecRound;
  'Square': LineCap := pecSquare;
  'Flat': LineCap := pecFlat;
  else exit(srInvalidParameters);
  end;
  result := srOk;
end;

function TToolManager.ScriptSetForeColor(AVars: TVariableSet): TScriptResult;
begin
  ForeColor := AVars.Pixels['Color'];
  ToolUpdate;
  result := srOk;
end;

function TToolManager.ScriptSetPenStyle(AVars: TVariableSet): TScriptResult;
begin
  result := srOk;
  case AVars.Strings['Style'] of
  'Solid': PenStyle := psSolid;
  'Dash': PenStyle := psDash;
  'Dot': PenStyle := psDot;
  'DashDot': PenStyle := psDashDot;
  'DashDotDot': PenStyle := psDashDotDot;
  else result := srInvalidParameters;
  end;
end;

function TToolManager.ScriptSetPenWidth(AVars: TVariableSet): TScriptResult;
begin
  PenWidth:= AVars.Floats['Width'];
  result := srOk;
end;

function TToolManager.ScriptSetPerspectiveOptions(AVars: TVariableSet): TScriptResult;
var optionsSet: TPerspectiveOptions;
  optionsVar: TScriptVariableReference;
  i: Integer;
  optionStr: string;
begin
  optionsSet := [];
  optionsVar := AVars.GetVariable('Options');
  for i := 0 to AVars.GetListCount(optionsVar)-1 do
  begin
    optionStr := AVars.GetStringAt(optionsVar, i);
    case optionStr of
    'Repeat': include(optionsSet, poRepeat);
    'TwoPlanes': include(optionsSet, poTwoPlanes);
    else exit(srInvalidParameters);
    end;
  end;
  PerspectiveOptions := optionsSet;
  result := srOk;
end;

function TToolManager.ScriptSetPhongShapeAltitude(AVars: TVariableSet): TScriptResult;
begin
  if (AVars.Floats['Size'] < MinPhongShapeAltitude) or
     (AVars.Floats['Size'] > MaxPhongShapeAltitude) then exit(srInvalidParameters);
  result := srOk;
  PhongShapeAltitude := AVars.Integers['Altitude'];
end;

function TToolManager.ScriptSetPhongShapeBorderSize(AVars: TVariableSet): TScriptResult;
begin
  if (AVars.Floats['Size'] < MinPhongBorderSize) or
     (AVars.Floats['Size'] > MaxPhongBorderSize) then exit(srInvalidParameters);
  result := srOk;
  PhongShapeBorderSize := AVars.Integers['Size'];
end;

function TToolManager.ScriptSetPhongShapeKind(AVars: TVariableSet): TScriptResult;
begin
  result := srOk;
  case AVars.Strings['Kind'] of
  'Rectangle': PhongShapeKind := pskRectangle;
  'RoundRectangle': PhongShapeKind := pskRoundRectangle;
  'HalfSphere': PhongShapeKind := pskHalfSphere;
  'ConeTop': PhongShapeKind := pskConeTop;
  'ConeSide': PhongShapeKind := pskConeSide;
  'HorizCylinder': PhongShapeKind := pskHorizCylinder;
  'VertCylinder': PhongShapeKind := pskVertCylinder;
  else result := srInvalidParameters;
  end;
end;

function TToolManager.ScriptSetShapeOptions(AVars: TVariableSet): TScriptResult;
var so: TShapeOptions;
  options: TScriptVariableReference;
  i: Integer;
  opt: String;
begin
  so := [];
  if toAliasing in ShapeOptions then include(so, toAliasing);
  options := AVars.GetVariable('Options');
  for i := 0 to AVars.GetListCount(options)-1 do
  begin
    opt := AVars.GetStringAt(options, i);
    case opt of
    'DrawShape': include(so, toDrawShape);
    'FillShape': include(so, toFillShape);
    'CloseShape': include(so, toCloseShape);
    else exit(srInvalidParameters);
    end;
  end;
  if [toDrawShape,toFillShape]*so = [] then
    so := so + [toDrawShape,toFillShape]*ShapeOptions;
  ShapeOptions := so;
  result := srOk;
end;

function TToolManager.ScriptSetShapeRatio(AVars: TVariableSet): TScriptResult;
var
  ratio: single;
begin
  ratio := AVars.Floats['Ratio'];
  if ratio <= 0 then result := srException else
  begin
    ShapeRatio := ratio;
    result := srOk;
  end;
end;

function TToolManager.ScriptSetSplineStyle(AVars: TVariableSet): TScriptResult;
var
  s: TSplineStyle;
begin
  case AVars.Strings['Style'] of
    'Inside': s := ssInside;
    'InsideWithEnds': s := ssInsideWithEnds;
    'Crossing': s := ssCrossing;
    'CrossingWithEnds': s := ssCrossingWithEnds;
    'Outside': s := ssOutside;
    'RoundOutside': s := ssRoundOutside;
    'VertexToSide': s := ssVertexToSide;
    'EasyBezier': s := ssEasyBezier;
  else
    exit(srInvalidParameters);
  end;
  SplineStyle := s;
  result := srOk;
end;

function TToolManager.ScriptSetTextAlign(AVars: TVariableSet): TScriptResult;
begin
  case AVars.Strings['Align'] of
  'Left': TextAlign:= taLeftJustify;
  'Center': TextAlign:= taCenter;
  'Right': TextAlign:= taRightJustify;
  else exit(srInvalidParameters);
  end;
  result := srOk;
end;

function TToolManager.ScriptSetTextVerticalAlign(AVars: TVariableSet): TScriptResult;
begin
  case AVars.Strings['Align'] of
  'Top': TextVerticalAlign:= tlTop;
  'Middle': TextVerticalAlign:= tlCenter;
  'Bottom': TextVerticalAlign:= tlBottom;
  else exit(srInvalidParameters);
  end;
  result := srOk;
end;

function TToolManager.ScriptSetTextBidiMode(AVars: TVariableSet): TScriptResult;
begin
  case AVars.Strings['BidiMode'] of
  'BidiAuto': TextBidiMode:= fbmAuto;
  'LeftToRight': TextBidiMode:= fbmLeftToRight;
  'RightToLeft': TextBidiMode:= fbmRightToLeft;
  else exit(srInvalidParameters);
  end;
  result := srOk;
end;

function TToolManager.ScriptSetTextOutline(AVars: TVariableSet): TScriptResult;
begin
  if AVars.IsDefined('Width') and (AVars.Floats['Width'] > 0) then
    SetTextOutline(true, AVars.Floats['Width'])
  else
    SetTextOutline(false, TextOutlineWidth);
  result := srOk;
end;

function TToolManager.ScriptSetTextPhong(AVars: TVariableSet): TScriptResult;
begin
  TextPhong:= AVars.Booleans['Enabled'];
  result := srOk;
end;

function TToolManager.ScriptSetTolerance(AVars: TVariableSet): TScriptResult;
var
  alpha: Int64;
begin
  alpha := AVars.Integers['Tolerance'];
  if alpha < 0 then alpha := 0;
  if alpha > 255 then alpha := 255;
  Tolerance:= alpha;
  result := srOk;
end;

procedure TToolManager.SetBrushIndex(AValue: integer);
begin
  if FBrushIndex=AValue then Exit;
  FBrushIndex:=AValue;
  ToolUpdate;
  if Assigned(FOnBrushChanged) then FOnBrushChanged(self);
end;

procedure TToolManager.SetBrushSpacing(AValue: integer);
begin
  if AValue < 0 then AValue := 0;
  if AValue > MaxBrushSpacing then AValue := MaxBrushSpacing;
  if FBrushSpacing=AValue then Exit;
  FBrushSpacing:=AValue;
  ToolUpdate;
  if Assigned(FOnBrushChanged) then FOnBrushChanged(self);
end;

constructor TToolManager.Create(AImage: TLazPaintImage; AConfigProvider: IConfigProvider;
  ABitmapToVirtualScreen: TBitmapToVirtualScreenFunction;
  ABlackAndWhite : boolean; AScriptContext: TScriptContext);
begin
  FImage:= AImage;
  CanvasScale := 1;
  PenWidthVisible := false;
  BitmapToVirtualScreen := ABitmapToVirtualScreen;
  FShouldExitTool:= false;
  FConfigProvider := AConfigProvider;
  FBlackAndWhite := ABlackAndWhite;
  FScriptContext := AScriptContext;
  RegisterScriptFunctions(True);

  FForeFill := TVectorialFill.Create;
  FForeFill.TransparentMode := tmAlphaZeroOnly;
  FForeFill.SolidColor := BGRABlack;
  FForeFill.OnChange:= @FillChange;
  FForeLastGradient:= TBGRALayerGradientOriginal.Create;
  FForeLastGradient.ColorInterpolation:= ciLinearRGB;

  FBackFill := TVectorialFill.Create;
  FBackFill.TransparentMode := tmAlphaZeroOnly;
  FBackFill.SolidColor := CSSSkyBlue;
  FBackFill.OnChange:= @FillChange;
  FBackLastGradient:= TBGRALayerGradientOriginal.Create;
  FBackLastGradient.ColorInterpolation:= ciLinearRGB;

  FOutlineFill := TVectorialFill.Create;
  FOutlineFill.TransparentMode := tmAlphaZeroOnly;
  FOutlineFill.SolidColor := CSSRed;
  FOutlineFill.OnChange:= @FillChange;
  FOutlineLastGradient:= TBGRALayerGradientOriginal.Create;
  FOutlineLastGradient.ColorInterpolation:= ciLinearRGB;

  FNormalPenWidth := 5;
  FEraserWidth := 10;
  FEraserAlpha := 255;
  FEraserMode := emEraseAlpha;
  ReloadBrushes;
  FBrushSpacing := 1;
  FShapeOptions := [toDrawShape, toFillShape, toCloseShape];
  FPenStyle := psSolid;
  FLineCap := pecRound;
  FJoinStyle := pjsRound;
  FArrowStart := akNone;
  FArrowEnd := akNone;
  FArrowSize := PointF(2,2);
  FSplineStyle := ssEasyBezier;
  FFloodFillOptions := [ffProgressive];
  FTolerance := 64;
  FTextOutline := False;
  FTextOutlineWidth := 2;
  FTextShadow := false;
  FTextFontSize := 10;
  FTextFontName := TTextShape.DefaultFontName;
  FTextFontStyle:= [];
  FTextAlign := taLeftJustify;
  FTextVerticalAlign := tlTop;
  FTextBidiMode := fbmAuto;
  FTextPhong := False;
  FTextShadowBlurRadius := 4;
  FTextShadowOffset := Point(5,5);
  FLightPosition := PointF(0,0);
  FLightAltitude := 100;
  FPhongShapeKind := pskRectangle;
  FPhongShapeAltitude := 50;
  FPhongShapeBorderSize := 20;
  FPerspectiveOptions:= [];
  FDeformationGridNbX := 5;
  FDeformationGridNbY := 5;
  FDeformationGridMode := gmDeform;

  PenWidthControls := TList.Create;
  AliasingControls := TList.Create;
  ShapeControls := TList.Create;
  PenStyleControls := TList.Create;
  CloseShapeControls := TList.Create;
  LineCapControls := TList.Create;
  JoinStyleControls := TList.Create;
  SplineStyleControls := TList.Create;
  EraserControls := TList.Create;
  ToleranceControls := TList.Create;
  DeformationControls := TList.Create;
  TextControls := TList.Create;
  TextShadowControls := TList.Create;
  PhongControls := TList.Create;
  AltitudeControls := TList.Create;
  PerspectiveControls := TList.Create;
  FillControls := TList.Create;
  OutlineFillControls := TList.Create;
  BrushControls := TList.Create;
  RatioControls := TList.Create;
  DonateControls := TList.Create;

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
  AliasingControls.Free;
  ShapeControls.Free;
  PenStyleControls.Free;
  CloseShapeControls.Free;
  LineCapControls.Free;
  JoinStyleControls.Free;
  SplineStyleControls.Free;
  EraserControls.Free;
  ToleranceControls.Free;
  DeformationControls.Free;
  TextControls.Free;
  TextShadowControls.Free;
  PhongControls.Free;
  AltitudeControls.Free;
  PerspectiveControls.Free;
  FillControls.Free;
  OutlineFillControls.Free;
  BrushControls.Free;
  RatioControls.Free;
  DonateControls.Free;

  for i := 0 to BrushCount do
    BrushAt[i].Free;
  FBrushInfoList.Free;

  FForeFill.Free;
  FBackFill.Free;
  FOutlineFill.Free;
  FForeLastGradient.Free;
  FBackLastGradient.Free;
  FOutlineLastGradient.Free;

  RegisterScriptFunctions(False);
  inherited Destroy;
end;

procedure TToolManager.LoadFromConfig;
var
  Config: TLazPaintConfig;
  opt: TShapeOptions;
begin
  if Assigned(FConfigProvider) then
    Config := FConfigProvider.GetConfig
  else
    exit;
  ForeColor := Config.DefaultToolForeColor;
  BackColor := Config.DefaultToolBackColor;
  OutlineColor := Config.DefaultToolOutlineColor;
  AssignGradientFromConfigStr(FForeLastGradient, Config.DefaultToolForeGradient);
  AssignGradientFromConfigStr(FBackLastGradient, Config.DefaultToolBackGradient);
  AssignGradientFromConfigStr(FOutlineLastGradient, Config.DefaultToolOutlineGradient);
  FNormalPenWidth := Config.DefaultToolPenWidth;
  FEraserWidth := Config.DefaultToolEraserWidth;
  if Assigned(FOnPenWidthChanged) then FOnPenWidthChanged(self);
  ReloadBrushes;
  opt := [];
  if Config.DefaultToolOptionDrawShape then include(opt, toDrawShape);
  if Config.DefaultToolOptionFillShape then include(opt, toFillShape);
  if Config.DefaultToolOptionCloseShape then include(opt, toCloseShape);
  ShapeOptions:= opt;
  Tolerance := Config.DefaultToolTolerance;

  //TextShadow := Config.DefaultToolTextShadow;
  SetTextOutline(Config.DefaultToolTextOutline, Config.DefaultToolTextOutlineWidth);
  TextPhong := Config.DefaultToolTextPhong;
  with Config.DefaultToolTextFont do
    SetTextFont(Name, Size, Style);
  TextShadowBlurRadius := Config.DefaultToolTextBlur;
  TextShadowOffset := Config.DefaultToolTextShadowOffset;

  LightPosition := Config.DefaultToolLightPosition;
  LightAltitude := Config.DefaultToolLightAltitude;
  PhongShapeAltitude := Config.DefaultToolShapeAltitude;
  PhongShapeBorderSize := Config.DefaultToolShapeBorderSize;
  PhongShapeKind := Config.DefaultToolShapeType;
end;

procedure TToolManager.SaveToConfig;
var
  Config: TLazPaintConfig;
begin
  if Assigned(FConfigProvider) then
    Config := FConfigProvider.GetConfig
  else
    exit;
  if ForeFill.FillType = vftSolid then Config.SetDefaultToolForeColor(ForeColor);
  if BackFill.FillType = vftSolid then Config.SetDefaultToolBackColor(BackColor);
  if OutlineFill.FillType = vftSolid then Config.SetDefaultToolOutlineColor(OutlineColor);
  Config.SetDefaultToolForeGradient(GradientToConfigStr(FForeLastGradient));
  Config.SetDefaultToolBackGradient(GradientToConfigStr(FBackLastGradient));
  Config.SetDefaultToolOutlineGradient(GradientToConfigStr(FOutlineLastGradient));
  Config.SetDefaultToolPenWidth(FNormalPenWidth);
  Config.SetDefaultToolEraserWidth(FEraserWidth);
  Config.SetDefaultToolOptionDrawShape(toDrawShape in ShapeOptions);
  Config.SetDefaultToolOptionFillShape(toFillShape in ShapeOptions);
  Config.SetDefaultToolOptionCloseShape(toCloseShape in ShapeOptions);
  Config.SetDefaultToolTolerance(Tolerance);

  Config.SetDefaultToolTextFont(FTextFontName, FTextFontSize, FTextFontStyle);
  Config.SetDefaultToolTextShadow(TextShadow);
  Config.SetDefaultToolTextOutline(TextOutline);
  Config.SetDefaultToolTextOutlineWidth(TextOutlineWidth);
  Config.SetDefaultToolTextBlur(TextShadowBlurRadius);
  Config.SetDefaultToolTextShadowOffset(TextShadowOffset);
  Config.SetDefaultToolTextPhong(TextPhong);

  Config.SetDefaultToolLightPosition(LightPosition);
  Config.SetDefaultToolLightAltitude(LightAltitude);
  Config.SetDefaultToolShapeBorderSize(PhongShapeBorderSize);
  Config.SetDefaultToolShapeAltitude(PhongShapeAltitude);
  Config.SetDefaultToolShapeType(PhongShapeKind);
end;

procedure TToolManager.ReloadBrushes;
var
  i: Integer;
  bi: TLazPaintBrush;
begin
  If Assigned(FBrushInfoList) then
  begin
    for i := 0 to FBrushInfoList.Count-1 do
      TObject(FBrushInfoList[i]).Free;
    FBrushInfoList.Clear;
  end else
    FBrushInfoList := TList.Create;
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
      FBrushInfoList.Add(bi);
    end;
  end;
  if FBrushInfoList.Count = 0 then
  begin
    FBrushInfoList.Add(TLazPaintBrush.Create(0,True));
    FBrushInfoList.Add(TLazPaintBrush.CreateFromStream64('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'));
  end;
  FBrushInfoListChanged := false;
end;

procedure TToolManager.SaveBrushes;
var
  i: Integer;
  infos: TStringList;
begin
  if Assigned(FConfigProvider) and FBrushInfoListChanged then
  begin
    infos := TStringList.Create;
    try
      for i := 0 to BrushCount-1 do
        infos.Add(BrushAt[i].AsString);
      FConfigProvider.GetConfig.SetBrushes(infos);
    except
    end;
    infos.Free;
  end;
  FBrushInfoListChanged := false;
end;

function TToolManager.ApplyPressure(AColor: TBGRAPixel): TBGRAPixel;
var alpha: integer;
begin
  alpha := round(AColor.alpha*FToolPressure);
  if alpha <= 0 then
    result := BGRAPixelTransparent
  else if alpha >= 255 then
    result := AColor
  else
  begin
    result := AColor;
    result.alpha := alpha;
  end;
end;

function TToolManager.ApplyPressure(AOpacity: byte): byte;
begin
  result := round(AOpacity*FToolPressure);
end;

procedure TToolManager.SetPressure(APressure: single);
begin
  if APressure <= 0 then
    FToolPressure := 0
  else if APressure >= 1 then
    FToolPressure := 1
  else
    FToolPressure:= APressure;
end;

function TToolManager.GetPressureB: Byte;
begin
  result := round(FToolPressure*255);
end;

procedure TToolManager.StepPenSize(ADecrease: boolean);
  function SizeDelta: single;
  var v: single;
  begin
    v := PenWidth;
    if ADecrease then v := v - 0.1;
    if v < 10 then result := 1 else
    if v < 20 then result := 2 else
    if v < 50 then result := 5 else
    if v < 100 then result := 10 else
    if v < 200 then result := 20 else
    if v < 500 then result := 50 else
      result := 100;
  end;
begin
  if ADecrease then
    PenWidth := PenWidth - SizeDelta
    else PenWidth := PenWidth + SizeDelta;
end;

procedure TToolManager.InternalSetCurrentToolType(tool: TPaintToolType);
begin
  if (tool <> FCurrentToolType) or (FCurrentTool=nil) then
  begin
    FreeAndNil(FCurrentTool);
    if PaintTools[tool] <> nil then
      FCurrentTool := PaintTools[tool].Create(self)
    else
      FCurrentTool := nil;

    FCurrentToolType:= tool;

    if not IsSelectingTool then
      Image.ReleaseEmptySelection;

    UpdateContextualToolbars;
    If Assigned(FOnToolChangedHandler) then
      FOnToolChangedHandler(self, FCurrentToolType);
    If Assigned(FOnToolRenderChanged) then
      FOnToolRenderChanged(self);
  end;
  FShouldExitTool:= false;
end;

function TToolManager.UpdateContextualToolbars: boolean;
var
  contextualToolbars: TContextualToolbars;
  hasPen: Boolean;

  procedure OrResult(AValue: boolean);
  begin
    if AValue then result := true;
  end;

begin
  result := false;
  contextualToolbars := GetContextualToolbars;
  if Assigned(FCurrentTool) then
    hasPen := FCurrentTool.HasPen
    else hasPen := false;

  if (ctBackFill in contextualToolbars) and not (ctPenFill in contextualToolbars) then
    OrResult(SetControlsVisible(FillControls, True, 'Panel_BackFill')) else
  if (ctPenFill in contextualToolbars) and not (ctBackFill in contextualToolbars) then
    OrResult(SetControlsVisible(FillControls, True, 'Panel_PenFill'))
  else
    OrResult(SetControlsVisible(FillControls, (ctPenFill in contextualToolbars) and (ctBackFill in contextualToolbars)));

  OrResult(SetControlsVisible(BrushControls, ctBrush in contextualToolbars));
  OrResult(SetControlsVisible(ShapeControls, ctShape in contextualToolbars));
  PenWidthVisible := (ctPenWidth in contextualToolbars) and hasPen;
  OrResult(SetControlsVisible(PenWidthControls, PenWidthVisible));
  OrResult(SetControlsVisible(JoinStyleControls, (ctJoinStyle in contextualToolbars) and hasPen));
  OrResult(SetControlsVisible(PenStyleControls, (ctPenStyle in contextualToolbars) and hasPen));
  OrResult(SetControlsVisible(CloseShapeControls, ctCloseShape in contextualToolbars));
  OrResult(SetControlsVisible(LineCapControls, ToolHasLineCap));
  OrResult(SetControlsVisible(AliasingControls, ctAliasing in contextualToolbars));
  OrResult(SetControlsVisible(SplineStyleControls, ctSplineStyle in contextualToolbars));
  OrResult(SetControlsVisible(EraserControls, ctEraserOption in contextualToolbars));
  OrResult(SetControlsVisible(ToleranceControls, ctTolerance in contextualToolbars));
  OrResult(SetControlsVisible(DeformationControls, ctDeformation in contextualToolbars));
  if (ctText in contextualToolbars) and not (ctOutlineWidth in contextualToolbars) then
    OrResult(SetControlsVisible(TextControls, True, 'Panel_Text')) else
  if (ctOutlineWidth in contextualToolbars) and not (ctText in contextualToolbars) then
    OrResult(SetControlsVisible(TextControls, True, 'Panel_TextOutline'))
  else
    OrResult(SetControlsVisible(TextControls, (ctText in contextualToolbars) and (ctOutlineWidth in contextualToolbars)));
  OrResult(SetControlsVisible(OutlineFillControls, ctOutlineFill in contextualToolbars));
  OrResult(SetControlsVisible(TextShadowControls, ctTextShadow in contextualToolbars));
  OrResult(SetControlsVisible(PhongControls, ctPhong in contextualToolbars));
  OrResult(SetControlsVisible(AltitudeControls, ctAltitude in contextualToolbars));
  OrResult(SetControlsVisible(PerspectiveControls, ctPerspective in contextualToolbars));
  OrResult(SetControlsVisible(RatioControls, ctRatio in contextualToolbars));
  OrResult(SetControlsVisible(DonateControls, FCurrentToolType = ptHand));

  if result and Assigned(FOnToolbarChanged) then FOnToolbarChanged(self);
end;

function TToolManager.GetContextualToolbars: TContextualToolbars;
begin
  if Assigned(FCurrentTool) then
    result := FCurrentTool.GetContextualToolbars
  else
    result := [ctPenFill, ctBackFill];
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

procedure TToolManager.RegisterScriptFunctions(ARegister: boolean);
begin
  if not Assigned(FScriptContext) then exit;
  FScriptContext.RegisterScriptFunction('ToolSetForeColor', @ScriptSetForeColor, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetForeColor', @ScriptGetForeColor, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetBackColor', @ScriptSetBackColor, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetBackColor', @ScriptGetBackColor, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetOutlineColor', @ScriptSetOutlineColor, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetOutlineColor', @ScriptGetOutlineColor, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetEraserMode', @ScriptSetEraserMode, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetEraserMode', @ScriptGetEraserMode, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetEraserAlpha', @ScriptSetEraserAlpha, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetEraserAlpha', @ScriptGetEraserAlpha, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetPenWidth', @ScriptSetPenWidth, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetPenWidth', @ScriptGetPenWidth, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetPenStyle', @ScriptSetPenStyle, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetPenStyle', @ScriptGetPenStyle, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetJoinStyle', @ScriptSetJoinStyle, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetJoinStyle', @ScriptGetJoinStyle, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetShapeOptions', @ScriptSetShapeOptions, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetShapeOptions', @ScriptGetShapeOptions, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetAliasing', @ScriptSetAliasing, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetAliasing', @ScriptGetAliasing, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetShapeRatio', @ScriptSetShapeRatio, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetShapeRatio', @ScriptGetShapeRatio, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetBrushIndex', @ScriptSetBrushIndex, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetBrushIndex', @ScriptGetBrushIndex, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetBrushCount', @ScriptGetBrushCount, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetBrushSpacing', @ScriptSetBrushSpacing, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetBrushSpacing', @ScriptGetBrushSpacing, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetFontName', @ScriptSetFontName, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetFontName', @ScriptGetFontName, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetFontSize', @ScriptSetFontSize, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetFontSize', @ScriptGetFontSize, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetFontStyle', @ScriptSetFontStyle, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetFontStyle', @ScriptGetFontStyle, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetTextAlign', @ScriptSetTextAlign, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetTextAlign', @ScriptGetTextAlign, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetTextVerticalAlign', @ScriptSetTextVerticalAlign, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetTextVerticalAlign', @ScriptGetTextVerticalAlign, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetTextBidiMode', @ScriptSetTextBidiMode, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetTextBidiMode', @ScriptGetTextBidiMode, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetTextOutline', @ScriptSetTextOutline, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetTextOutline', @ScriptGetTextOutline, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetTextPhong', @ScriptSetTextPhong, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetTextPhong', @ScriptGetTextPhong, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetLightPosition', @ScriptSetLightPosition, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetLightPosition', @ScriptGetLightPosition, ARegister);
{  FScriptContext.RegisterScriptFunction('ToolSetLightAltitude', @ScriptSetLightAltitude, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetLightAltitude', @ScriptGetLightAltitude, ARegister);}
  FScriptContext.RegisterScriptFunction('ToolSetLineCap', @ScriptSetLineCap, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetLineCap', @ScriptGetLineCap, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetArrowStart', @ScriptSetArrowStart, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetArrowStart', @ScriptGetArrowStart, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetArrowEnd', @ScriptSetArrowEnd, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetArrowEnd', @ScriptGetArrowEnd, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetArrowSize', @ScriptSetArrowSize, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetArrowSize', @ScriptGetArrowSize, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetSplineStyle', @ScriptSetSplineStyle, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetSplineStyle', @ScriptGetSplineStyle, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetForeGradientType', @ScriptSetForeGradientType, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetForeGradientType', @ScriptGetForeGradientType, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetForeGradientRepetition', @ScriptSetForeGradientRepetition, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetForeGradientRepetition', @ScriptGetForeGradientRepetition, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetForeGradientInterpolation', @ScriptSetForeGradientInterpolation, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetForeGradientInterpolation', @ScriptGetForeGradientInterpolation, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetForeGradientColors', @ScriptSetForeGradientColors, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetForeGradientColors', @ScriptGetForeGradientColors, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetBackGradientType', @ScriptSetBackGradientType, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetBackGradientType', @ScriptGetBackGradientType, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetBackGradientRepetition', @ScriptSetBackGradientRepetition, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetBackGradientRepetition', @ScriptGetBackGradientRepetition, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetBackGradientInterpolation', @ScriptSetBackGradientInterpolation, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetBackGradientInterpolation', @ScriptGetBackGradientInterpolation, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetBackGradientColors', @ScriptSetBackGradientColors, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetBackGradientColors', @ScriptGetBackGradientColors, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetOutlineGradientType', @ScriptSetOutlineGradientType, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetOutlineGradientType', @ScriptGetOutlineGradientType, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetOutlineGradientRepetition', @ScriptSetOutlineGradientRepetition, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetOutlineGradientRepetition', @ScriptGetOutlineGradientRepetition, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetOutlineGradientInterpolation', @ScriptSetOutlineGradientInterpolation, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetOutlineGradientInterpolation', @ScriptGetOutlineGradientInterpolation, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetOutlineGradientColors', @ScriptSetOutlineGradientColors, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetOutlineGradientColors', @ScriptGetOutlineGradientColors, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetForeTexture', @ScriptSetForeTexture, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetForeTextureRepetition', @ScriptSetForeTextureRepetition, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetForeTextureRepetition', @ScriptGetForeTextureRepetition, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetForeTextureOpacity', @ScriptSetForeTextureOpacity, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetForeTextureOpacity', @ScriptGetForeTextureOpacity, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetBackTexture', @ScriptSetBackTexture, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetBackTextureRepetition', @ScriptSetBackTextureRepetition, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetBackTextureRepetition', @ScriptGetBackTextureRepetition, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetBackTextureOpacity', @ScriptSetBackTextureOpacity, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetBackTextureOpacity', @ScriptGetBackTextureOpacity, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetOutlineTexture', @ScriptSetOutlineTexture, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetOutlineTextureRepetition', @ScriptSetOutlineTextureRepetition, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetOutlineTextureRepetition', @ScriptGetOutlineTextureRepetition, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetOutlineTextureOpacity', @ScriptSetOutlineTextureOpacity, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetOutlineTextureOpacity', @ScriptGetOutlineTextureOpacity, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetPhongShapeAltitude', @ScriptSetPhongShapeAltitude, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetPhongShapeAltitude', @ScriptGetPhongShapeAltitude, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetPhongShapeBorderSize', @ScriptSetPhongShapeBorderSize, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetPhongShapeBorderSize', @ScriptGetPhongShapeBorderSize, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetPhongShapeKind', @ScriptSetPhongShapeKind, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetPhongShapeKind', @ScriptGetPhongShapeKind, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetDeformationGridSize', @ScriptSetDeformationGridSize, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetDeformationGridSize', @ScriptGetDeformationGridSize, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetDeformationGridMode', @ScriptSetDeformationGridMode, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetDeformationGridMode', @ScriptGetDeformationGridMode, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetTolerance', @ScriptSetTolerance, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetTolerance', @ScriptGetTolerance, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetFloodFillOptions', @ScriptSetFloodFillOptions, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetFloodFillOptions', @ScriptGetFloodFillOptions, ARegister);
  FScriptContext.RegisterScriptFunction('ToolSetPerspectiveOptions', @ScriptSetPerspectiveOptions, ARegister);
  FScriptContext.RegisterScriptFunction('ToolGetPerspectiveOptions', @ScriptGetPerspectiveOptions, ARegister);
end;

procedure TToolManager.ToolWakeUp;
begin
  if FSleepingTool <> nil then
  begin
    FreeAndNil(FCurrentTool);
    FCurrentTool := FSleepingTool;
    FSleepingTool := nil;
    FCurrentToolType := FSleepingToolType;
    UpdateContextualToolbars;
    If Assigned(FOnToolChangedHandler) then
      FOnToolChangedHandler(self, FCurrentToolType);
    If Assigned(FOnToolRenderChanged) then
      FOnToolRenderChanged(self);
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

procedure TToolManager.SetDeformationGridSize(ASize: TSize);
begin
  if ASize.cx < 3 then ASize.cx := 3;
  if ASize.cy < 3 then ASize.cy := 3;
  if (ASize.cx <> DeformationGridNbX) or (ASize.cy <> DeformationGridNbY) then
  begin
    FDeformationGridNbX := ASize.cx;
    FDeformationGridNbY := ASize.cy;
    if ToolUpdate then
      Image.OnImageChanged.NotifyObservers;
    if Assigned(FOnDeformationGridChanged) then FOnDeformationGridChanged(self);
  end;
end;

function TToolManager.SwapToolColors: boolean;
var
  tmpFill: TVectorialFill;
begin
  result := false;
  if FInSwapFill then exit;
  if FForeFill.Equals(FBackFill) then exit;
  FInSwapFill:= true;
  tmpFill := FForeFill.Duplicate;
  FForeFill.Assign(FBackFill);
  FBackFill.Assign(tmpFill);
  tmpFill.Free;
  if FForeFill.FillType = vftGradient then
  begin
    FForeLastGradient.Free;
    FForeLastGradient := FForeFill.Gradient.Duplicate as TBGRALayerGradientOriginal;
  end;
  if FBackFill.FillType = vftGradient then
  begin
    FBackLastGradient.Free;
    FBackLastGradient := FBackFill.Gradient.Duplicate as TBGRALayerGradientOriginal;
  end;
  if Assigned(FOnFillChanged) then FOnFillChanged(self);
  FInSwapFill:= false;
  result := true;
end;

procedure TToolManager.NeedBackGradient;
var
  tempFill: TVectorialFill;
begin
  if BackFill.FillType <> vftGradient then
  begin
    tempFill := TVectorialFill.Create;
    tempFill.SetGradient(FBackLastGradient, False);
    tempFill.FitGeometry(SuggestGradientBox);
    BackFill.Assign(tempFill);
    tempFill.Free;
  end;
end;

procedure TToolManager.NeedForeGradient;
var
  tempFill: TVectorialFill;
begin
  if ForeFill.FillType <> vftGradient then
  begin
    tempFill := TVectorialFill.Create;
    tempFill.SetGradient(FForeLastGradient, False);
    tempFill.FitGeometry(SuggestGradientBox);
    ForeFill.Assign(tempFill);
    tempFill.Free;
  end;
end;

procedure TToolManager.NeedOutlineGradient;
var
  tempFill: TVectorialFill;
begin
  if OutlineFill.FillType <> vftGradient then
  begin
    tempFill := TVectorialFill.Create;
    tempFill.SetGradient(FOutlineLastGradient, False);
    tempFill.FitGeometry(SuggestGradientBox);
    OutlineFill.Assign(tempFill);
    tempFill.Free;
  end;
end;

procedure TToolManager.AddBrush(brush: TLazPaintBrush);
begin
  FBrushIndex := FBrushInfoList.Add(brush);
  FBrushInfoListChanged := true;
  if Assigned(FOnBrushListChanged) then FOnBrushListChanged(self);
end;

procedure TToolManager.RemoveBrushAt(index: integer);
begin
  if Assigned(FBrushInfoList) then
  begin
    if (index >= 1) and (index < BrushCount) then
    begin
      BrushAt[index].Free;
      FBrushInfoList.Delete(index);
      if index < FBrushIndex then dec(FBrushIndex)
      else if index = FBrushIndex then
        begin
          if FBrushIndex >= BrushCount then
            dec(FBrushIndex);
        end;
      FBrushInfoListChanged := true;
      if Assigned(FOnBrushListChanged) then FOnBrushListChanged(self);
    end;
  end;
end;

procedure TToolManager.SetTextFont(AName: string; ASize: single;
  AStyle: TFontStyles);
begin
  if AName = '' then AName := FTextFontName;
  if (FTextFontName <> AName) or
    (FTextFontSize <> ASize) or
    (FTextFontStyle <> AStyle) then
  begin
    FTextFontName := AName;
    if ASize >= 0 then FTextFontSize := ASize;
    FTextFontStyle := AStyle;
    ToolUpdate;
    if Assigned(FOnTextFontChanged) then FOnTextFontChanged(self);
  end;
end;

procedure TToolManager.SetTextFont(AFont: TFont);
begin
  SetTextFont(AFont.Name, AFont.Size, AFont.Style);
end;

procedure TToolManager.SetTextOutline(AEnabled: boolean; AWidth: single);
begin
  if (FTextOutline <> AEnabled) or
    (FTextOutlineWidth <> AWidth) then
  begin
    FTextOutlineWidth := AWidth;
    FTextOutline := AEnabled;
    ToolUpdate;
    if Assigned(FOnTextOutlineChanged) then FOnTextOutlineChanged(self);
  end;
end;

function TToolManager.ToolDown(X, Y: single; ARightBtn: boolean;
  APressure: single): boolean;
var changed: TRect;
begin
  if FInTool then exit(false);
  FInTool := true;
  try
    SetPressure(APressure);
    if ToolCanBeUsed and Assigned(CurrentTool) then
      changed := CurrentTool.ToolDown(X,Y,ARightBtn)
    else
      changed := EmptyRect;
    result := not IsRectEmpty(changed);
    if IsOnlyRenderChange(changed) then changed := EmptyRect;

    if CheckExitTool then result := true;
    if result then NotifyImageOrSelectionChanged(CurrentTool.LastToolDrawingLayer, changed);
  finally
    FInTool := false;
  end;
end;

function TToolManager.ToolMove(X, Y: single; APressure: single): boolean;
var changed: TRect;
begin
  if FInTool then exit(false);
  FInTool := true;
  try
    SetPressure(APressure);
    if ToolCanBeUsed and Assigned(CurrentTool) then
      changed := CurrentTool.ToolMove(X,Y)
    else
      changed := EmptyRect;
    result := not IsRectEmpty(changed);
    if IsOnlyRenderChange(changed) then changed := EmptyRect;

    if CheckExitTool then result := true;
    if result then NotifyImageOrSelectionChanged(CurrentTool.LastToolDrawingLayer, changed);
  finally
    FInTool := false;
  end;
end;

function TToolManager.ToolKeyDown(var key: Word): boolean;
var changed: TRect;
begin
  if FInTool then exit(false);
  FInTool := true;
  try
    if ToolCanBeUsed and Assigned(CurrentTool) then
      changed := CurrentTool.ToolKeyDown(key)
    else
      changed := EmptyRect;
    result := not IsRectEmpty(changed);
    if IsOnlyRenderChange(changed) then changed := EmptyRect;

    if CheckExitTool then result := true;
    if result then NotifyImageOrSelectionChanged(CurrentTool.LastToolDrawingLayer, changed);
  finally
    FInTool := false;
  end;
end;

function TToolManager.ToolKeyUp(var key: Word): boolean;
var changed: TRect;
begin
  if FInTool then exit(false);
  FInTool := true;
  try
    if ToolCanBeUsed and Assigned(CurrentTool) then
      changed := CurrentTool.ToolKeyUp(key)
    else
      changed := EmptyRect;
    result := not IsRectEmpty(changed);
    if IsOnlyRenderChange(changed) then changed := EmptyRect;

    if CheckExitTool then result := true;
    if result then NotifyImageOrSelectionChanged(CurrentTool.LastToolDrawingLayer, changed);
  finally
    FInTool := false;
  end;
end;

function TToolManager.ToolKeyPress(var key: TUTF8Char): boolean;
var changed: TRect;
begin
  if FInTool then exit(false);
  FInTool := true;
  try
    if ToolCanBeUsed and Assigned(CurrentTool) then
      changed := CurrentTool.ToolKeyPress(key)
    else
      changed := EmptyRect;
    result := not IsRectEmpty(changed);
    if IsOnlyRenderChange(changed) then changed := EmptyRect;

    if CheckExitTool then result := true;
    if result then NotifyImageOrSelectionChanged(CurrentTool.LastToolDrawingLayer, changed);
  finally
    FInTool := false;
  end;
end;

function TToolManager.ToolCommand(ACommand: TToolCommand): boolean;
begin
  if FInTool then exit(false);
  FInTool := true;
  try
    if Assigned(FCurrentTool) then
    begin
      result := FCurrentTool.ToolCommand(ACommand);
      CheckExitTool;
    end
    else
      result := false;
  finally
    FInTool := false;
  end;
end;

function TToolManager.ToolProvideCommand(ACommand: TToolCommand): boolean;
begin
  if Assigned(FCurrentTool) then
    result := FCurrentTool.ToolProvideCommand(ACommand)
  else
    result := false;
end;

function TToolManager.ToolUp: boolean;
var changed: TRect;
begin
  if FInTool then exit(false);
  FInTool := true;
  try
    if ToolCanBeUsed and Assigned(CurrentTool) then
      changed := CurrentTool.ToolUp
    else
      changed := EmptyRect;
    result := not IsRectEmpty(changed);
    if IsOnlyRenderChange(changed) then changed := EmptyRect;

    if CheckExitTool then result := true;
    if result then NotifyImageOrSelectionChanged(CurrentTool.LastToolDrawingLayer, changed);
  finally
    FInTool := false;
  end;
end;

procedure TToolManager.ToolCloseDontReopen;
begin
  if CurrentTool <> nil then
  begin
    if FInTool then raise exception.Create('Cannot close active tool');
    FreeAndNil(FCurrentTool);
  end;
end;

procedure TToolManager.ToolCloseAndReopenImmediatly;
begin
  if CurrentTool <> nil then
  begin
    if FInTool then raise exception.Create('Cannot close active tool');
    FInTool := true;
    try
      FreeAndNil(FCurrentTool);
    finally
      FInTool := false;
    end;
    ToolOpen;
  end;
end;

procedure TToolManager.ToolOpen;
begin
  if (FCurrentTool = nil) and (PaintTools[FCurrentToolType] <> nil) then
  begin
    if FInTool then raise exception.Create('Internal error');
    FInTool := true;
    try
      FCurrentTool := PaintTools[FCurrentToolType].Create(self);
      UpdateContextualToolbars;
      If Assigned(FOnToolRenderChanged) then
        FOnToolRenderChanged(self);
    finally
      FInTool := false;
    end;
  end;
end;

function TToolManager.ToolUpdate: boolean;
var changed: TRect;
begin
  if FInTool then exit(false);
  FInTool := true;
  FInToolUpdate := true;
  try
    if ToolCanBeUsed and Assigned(CurrentTool) then
      changed := CurrentTool.ToolUpdate
    else
      changed := EmptyRect;
    result := not IsRectEmpty(changed);
    if IsOnlyRenderChange(changed) then changed := EmptyRect;

    if CheckExitTool then result := true;
    if result then NotifyImageOrSelectionChanged(CurrentTool.LastToolDrawingLayer, changed);
  finally
    FInTool := false;
    FInToolUpdate := false;
  end;
end;

function TToolManager.ToolUpdateNeeded: boolean;
begin
  if ToolCanBeUsed and Assigned(CurrentTool) then
    result := CurrentTool.ToolUpdateNeeded
  else
    result := false;
  if CheckExitTool then
    result := true;
end;

procedure TToolManager.ToolPopup(AMessage: TToolPopupMessage; AKey: Word = 0; AAlways: boolean = false);
begin
  if Assigned(FOnPopupToolHandler) then
    FOnPopupToolHandler(self, AMessage, AKey, AAlways);
end;

function TToolManager.IsSelectingTool: boolean;
begin
  if CurrentTool <> nil then
    result := CurrentTool.IsSelectingTool
  else
    result := false;
end;

function TToolManager.DisplayFilledSelection: boolean;
begin
  result := IsSelectingTool or (FCurrentToolType = ptEditShape);
end;

function TToolManager.IsForeEditGradTexPoints: boolean;
begin
  if Assigned(CurrentTool) then result := CurrentTool.IsForeEditGradTexPoints
  else result := false;
end;

function TToolManager.IsBackEditGradTexPoints: boolean;
begin
  if Assigned(CurrentTool) then result := CurrentTool.IsBackEditGradTexPoints
  else result := false;
end;

function TToolManager.IsOutlineEditGradTexPoints: boolean;
begin
  if Assigned(CurrentTool) then result := CurrentTool.IsOutlineEditGradTexPoints
  else result := false;
end;

procedure TToolManager.QueryExitTool;
begin
  FShouldExitTool:= true;
end;

procedure TToolManager.QueryColorTarget(ATarget: TVectorialFill);
begin
  if Assigned(OnQueryColorTarget) then
    OnQueryColorTarget(self, ATarget);
end;

function TToolManager.RenderTool(formBitmap: TBGRABitmap): TRect;
begin
  if ToolCanBeUsed and Assigned(CurrentTool) and not FInTool then
  begin
    FInTool := true;
    try
      result := CurrentTool.Render(formBitmap,formBitmap.Width,formBitmap.Height, @InternalBitmapToVirtualScreen);
    finally
      FInTool := false;
    end;
  end else
    result := EmptyRect;
end;

function TToolManager.GetRenderBounds(VirtualScreenWidth, VirtualScreenHeight: integer): TRect;
begin
  if ToolCanBeUsed and Assigned(CurrentTool) and not CurrentTool.Validating and not CurrentTool.Canceling then
    result := CurrentTool.Render(nil,VirtualScreenWidth,VirtualScreenHeight, @InternalBitmapToVirtualScreen)
  else
    result := EmptyRect;
end;

function TToolManager.SuggestGradientBox: TAffineBox;
begin
  if Assigned(CurrentTool) then
    result := CurrentTool.SuggestGradientBox
  else
    result := TAffineBox.AffineBox(RectF(PointF(0,0),PointF(Image.Width,Image.Height)));
end;

function TToolManager.GetDeformationGridSize: TSize;
begin
  result := Size(DeformationGridNbX, DeformationGridNbY);
end;

function TToolManager.ToolDown(ACoord: TPointF; ARightBtn: boolean;
  APressure: single): boolean;
begin
  result := ToolDown(ACoord.x,ACoord.y,ARightBtn,APressure)
end;

function TToolManager.ToolMove(ACoord: TPointF; APressure: single): boolean;
begin
  result := ToolMove(ACoord.x,ACoord.y,APressure)
end;

initialization
  fillchar({%H-}PaintTools,sizeof(PaintTools),0);

end.

