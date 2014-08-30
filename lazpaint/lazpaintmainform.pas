unit LazpaintMainForm;

{$mode objfpc}{$H+}

interface

{$IFDEF DARWIN}
  {$DEFINE USEPAINTBOXPICTURE}
{$ENDIF}

{$IFNDEF DARWIN}
  {$DEFINE USE_IMAGE_BROWSER}
{$ENDIF}

uses
  Classes, LMessages, SysUtils, FileUtil, LResources, Forms,
  Controls, Graphics, Dialogs, Menus, ExtDlgs, ComCtrls, ActnList, StdCtrls,
  ExtCtrls, Buttons, types, LCLType, BGRAImageList, BGRAVirtualScreen,

  BGRABitmap, BGRABitmapTypes, BGRALayers,

  LazPaintType, UTool, UImage, UImageAction, ULayerAction, UZoom,
  UImageObservation, UConfig, UScaleDPI, UResourceStrings,
  UMenu, uscripting, {$IFDEF USE_IMAGE_BROWSER}ubrowseimages, {$ENDIF} UToolPolygon, UBarUpDown;

const
  MinPenWidthValue = 10;

type
  { TFMain }

  TFMain = class(TForm)
    SavePictureDialog1: TSaveDialog;
    vsGridNbX: TBGRAVirtualScreen;
    vsGridNbY: TBGRAVirtualScreen;
    vsPhongBorderSize: TBGRAVirtualScreen;
    vsShapeAltitude: TBGRAVirtualScreen;
    vsTextShadowX: TBGRAVirtualScreen;
    vsTextOutlineWidth: TBGRAVirtualScreen;
    vsTextShadowY: TBGRAVirtualScreen;
    vsTextSize: TBGRAVirtualScreen;
    vsTextBlur: TBGRAVirtualScreen;
    vsTolerance: TBGRAVirtualScreen;
    vsTextureOpacity: TBGRAVirtualScreen;
    vsPenOpacity: TBGRAVirtualScreen;
    FileSaveAsInSameFolder: TAction;
    FilePrint: TAction;
    FilterNoise: TAction;
    ColorPosterize: TAction;
    ColorCurves: TAction;
    FilterComplementaryColor: TAction;
    FilterFunction: TAction;
    FilterPhong: TAction;
    FileReload: TAction;
    MainMenu1: TMainMenu;
    MenuFile: TMenuItem;
    ItemViewImageList: TMenuItem;
    ViewImageList: TAction;
    MenuRecentFiles: TMenuItem;
    ItemDonate: TMenuItem;
    ItemLanguage: TMenuItem;
    ItemQuitSeparator: TMenuItem;
    ItemQuit: TMenuItem;
    MenuEdit: TMenuItem;
    MenuSelect: TMenuItem;
    MenuView: TMenuItem;
    ItemViewToolbox: TMenuItem;
    ItemViewColors: TMenuItem;
    ItemViewGrid: TMenuItem;
    ItemViewLayerStack: TMenuItem;
    MenuImage: TMenuItem;
    MenuHorizFlipSub: TMenuItem;
    ItemHorizFlipPicture: TMenuItem;
    ItemHorizFlipLayer: TMenuItem;
    ItemHorizFlipSelection: TMenuItem;
    MenuVertFlipSub: TMenuItem;
    ItemVertFlipLayer: TMenuItem;
    ItemVertFlipSelection: TMenuItem;
    ItemVertFlipPicture: TMenuItem;
    ItemSeparatorTransparency: TMenuItem;
    MenuRemoveTransparency: TMenuItem;
    MenuColors: TMenuItem;
    MenuTool: TMenuItem;
    MenuFilter: TMenuItem;
    MenuRadialBlur: TMenuItem;
    MenuRender: TMenuItem;
    MenuHelp: TMenuItem;
    Open3DObjectDialog: TOpenDialog;
    Panel_PerspectiveOption: TPanel;
    Perspective_Repeat: TToolButton;
    Perspective_TwoPlanes: TToolButton;
    ToolBar18: TToolBar;
    ToolBar19: TToolBar;
    ToolBar20: TToolBar;
    Tool_CurveMovePoint: TToolButton;
    Tool_CurveModeAuto: TToolButton;
    ToolLayerMapping: TAction;
    ImageFlatten: TAction;
    LayerRotate: TAction;
    LayerMove: TAction;
    LayerFromFile: TAction;
    LayerMergeOver: TAction;
    LayerDuplicate: TAction;
    LayerRemoveCurrent: TAction;
    LayerAddNew: TAction;
    LayerVerticalFlip: TAction;
    LayerHorizontalFlip: TAction;
    ImageCropLayer: TAction;
    Tool_CurveModeAngle: TToolButton;
    Tool_EraseAlpha: TToolButton;
    Tool_EraseBlur: TToolButton;
    Tool_CurveModeCurve: TToolButton;
    ViewLayerStackButton: TAction;
    ViewLayerStack: TAction;
    FileImport3D: TAction;
    ToolTextureMapping: TAction;
    EditSelection: TAction;
    FilterClearTypeInverse: TAction;
    FilterClearType: TAction;
    ToolPhong: TAction;
    FileLoadSelection: TAction;
    FileSaveSelectionAs: TAction;
    ToolText: TAction;
    FilterPixelate: TAction;
    EditPasteAsNew: TAction;
    ViewGrid: TAction;
    EmbeddedCancel: TAction;
    EmbeddedValidate: TAction;
    RenderWoodVertical: TAction;
    FilterTwirl: TAction;
    ToolDeformation: TAction;
    RenderWater: TAction;
    RenderMarble: TAction;
    RenderRoundStone: TAction;
    RenderStone: TAction;
    RenderSnowPrint: TAction;
    RenderCamouflage: TAction;
    RenderMetalFloor: TAction;
    RenderCyclicPerlinNoise: TAction;
    RenderWood: TAction;
    RenderPlastik: TAction;
    ToolNoTexture: TAction;
    ToolLoadTexture: TAction;
    RenderPerlinNoise: TAction;
    FilterBlurFast: TAction;
    FilterPlane: TAction;
    ToolMagicWand: TAction;
    ImageRepeat: TAction;
    RenderCustomWater: TAction;
    HelpAbout: TAction;
    HelpIndex: TAction;
    ColorColorize: TAction;
    ColorShiftColors: TAction;
    FilterLinearNegative: TAction;
    ColorLightness: TAction;
    ColorIntensity: TAction;
    RenderClouds: TAction;
    ImageRotateCCW: TAction;
    ImageRotateCW: TAction;
    ImageChangeCanvasSize: TAction;
    ImageFillBackground: TAction;
    ImageClearAlpha: TAction;
    EditSelectionFit: TAction;
    ImageCrop: TAction;
    FilterCylinder: TAction;
    FilterSphere: TAction;
    ViewColors: TAction;
    ViewToolbox: TAction;
    EditSelectAll: TAction;
    FilterContour: TAction;
    FilterBlurCustom: TAction;
    FilterGrayscale: TAction;
    ToolRotateSelection: TAction;
    EditDeleteSelection: TAction;
    EditCut: TAction;
    EditPaste: TAction;
    EditCopy: TAction;
    ToolMoveSelection: TAction;
    ToolSelectPen: TAction;
    EditInvertSelection: TAction;
    EditDeselect: TAction;
    ToolSelectSpline: TAction;
    ToolSelectPoly: TAction;
    ToolSelectEllipse: TAction;
    ToolSelectRect: TAction;
    FilterNegative: TAction;
    FilterNormalize: TAction;
    FilterEmboss: TAction;
    FilterBlurMotion: TAction;
    FilterBlurPrecise: TAction;
    FilterBlurDisk: TAction;
    FilterBlurCorona: TAction;
    FilterBlurRadial: TAction;
    ViewZoomFit: TAction;
    ViewZoomOriginal: TAction;
    FilterSharpen: TAction;
    FilterSmooth: TAction;
    ImageSmartZoom3: TAction;
    FilterMedian: TAction;
    ImageResample: TAction;
    EditRedo: TAction;
    EditUndo: TAction;
    ImageHorizontalFlip: TAction;
    ImageVerticalFlip: TAction;
    ToolGradient: TAction;
    ToolFloodfill: TAction;
    ToolSpline: TAction;
    ToolEraser: TAction;
    ToolPolygon: TAction;
    ToolColorPicker: TAction;
    ToolEllipse: TAction;
    ToolRect: TAction;
    ToolPen: TAction;
    ToolHand: TAction;
    ViewZoomOut: TAction;
    ViewZoomIn: TAction;
    FileQuit: TAction;
    FileSaveAs: TAction;
    FileSave: TAction;
    FileOpen: TAction;
    FileNew: TAction;
    Edit_Zoom: TEdit;
    OpenTextureDialog: TOpenDialog;
    OpenPictureDialog1: TOpenDialog;
    Panel_TextOutline: TPanel;
    Timer1: TTimer;
    Label_OutlineWidth: TLabel;
    ToolBar17: TToolBar;
    Tool_TextAlignLeft: TToolButton;
    Tool_TextAlignCenter: TToolButton;
    Tool_TextAlignRight: TToolButton;
    Tool_TextOutline: TToolButton;
    ToolButton_ViewLayerStack: TToolButton;
    Label_Altitude: TLabel;
    Label_PhongBorder: TLabel;
    Label_CurrentDiff: TLabel;
    Label_Shape: TLabel;
    Panel_PhongShape: TPanel;
    Panel_Altitude: TPanel;
    ToolBar16: TToolBar;
    Tool_PhongShapeVerticalCone: TToolButton;
    Tool_PhongShapeHorizontalCylinder: TToolButton;
    Tool_PhongShapeVerticalCylinder: TToolButton;
    Tool_TextPhong: TToolButton;
    Tool_PhongShapeSphere: TToolButton;
    Tool_PhongShapeRectangle: TToolButton;
    Tool_PhongShapeCone: TToolButton;
    Tool_PhongShapeRoundRect: TToolButton;
    FontDialog1: TFontDialog;
    LoadSelectionDialog: TOpenPictureDialog;
    SaveSelectionDialog: TSavePictureDialog;
    Label_TextBlur: TLabel;
    Label_ShadowOffset: TLabel;
    Label_Text: TLabel;
    Panel_Text: TPanel;
    ToolBar15: TToolBar;
    Tool_TextFont: TToolButton;
    Tool_TextShadow: TToolButton;
    Combo_SplineStyle: TComboBox;
    PaintBox_Picture: TPaintBox;
    PaintBox_PenPreview: TPaintBox;
    Panel_Embedded: TPanel;
    Panel_PenWidthPreview: TPanel;
    Panel_SplineStyle: TPanel;
    Label_Curve: TLabel;
    Panel_Grid: TPanel;
    Label_Grid: TLabel;
    Label_GridX: TLabel;
    TimerHidePenPreview: TTimer;
    ToolBar13: TToolBar;
    Tool_CloseShape: TToolButton;
    ToolBar14: TToolBar;
    ToolButton19: TToolButton;
    ToolButton20: TToolButton;
    ToolButton3: TToolButton;
    Tool_GridMoveWithoutDeformation: TToolButton;
    Image_CurrentTexture: TImage;
    Panel_Texture: TPanel;
    Panel_PenStyle: TPanel;
    Panel_LineCap: TPanel;
    Panel_JoinStyle: TPanel;
    ToolBar11: TToolBar;
    ToolBar12: TToolBar;
    ToolBar4: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    Tool_PenDashDotDot: TToolButton;
    Tool_PenDot: TToolButton;
    Tool_JoinBevel: TToolButton;
    Tool_PenSolid: TToolButton;
    Tool_PenDashDot: TToolButton;
    Tool_JoinRound: TToolButton;
    Tool_CapSquare: TToolButton;
    Tool_CapFlat: TToolButton;
    Tool_CapRound: TToolButton;
    ToolBar10: TToolBar;
    Tool_SinGradient: TToolButton;
    Tool_JoinMiter: TToolButton;
    Tool_PenDash: TToolButton;
    Label_Coordinates: TLabel;
    Panel_Coordinates: TPanel;
    Image_SwapColors: TImage;
    Label_Eraser: TLabel;
    Image_CurrentTool: TImage;
    Label_Pen: TLabel;
    Label_Back: TLabel;
    Label_CurrentZoom: TLabel;
    Panel_Undo: TPanel;
    Panel_CopyPaste: TPanel;
    Panel_ToolbarBackground: TPanel;
    Panel_File: TPanel;
    Panel_GradientType: TPanel;
    Panel_Tool: TPanel;
    Panel_Zoom: TPanel;
    Panel_Color: TPanel;
    Panel_PenWidth: TPanel;
    Panel_Eraser: TPanel;
    Panel_Tolerance: TPanel;
    Panel_ShapeOption: TPanel;
    Label_PenWidth: TLabel;
    Tool_DrawShapeBorder: TToolButton;
    Tool_FillShape: TToolButton;
    Shape_BackColor: TShape;
    Shape_PenColor: TShape;
    Label_Tolerance: TLabel;
    ToolBar2: TToolBar;
    ToolBar3: TToolBar;
    ToolBar5: TToolBar;
    ToolBar6: TToolBar;
    ToolBar7: TToolBar;
    ToolBar8: TToolBar;
    ToolBar9: TToolBar;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    ToolButton23: TToolButton;
    ToolButton24: TToolButton;
    ToolButton29: TToolButton;
    ToolButton30: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    Tool_DiamondGradient: TToolButton;
    Tool_LinearGradient: TToolButton;
    Tool_ProgressiveFloodfill: TToolButton;
    Tool_RadialGradient: TToolButton;
    Tool_ReflectedGradient: TToolButton;
    ToolButton_ZoomOriginal: TToolButton;
    ColorDialog1: TColorDialog;
    ActionList1: TActionList;
    ImageList1: TBGRAImageList;
    vsBackOpacity: TBGRAVirtualScreen;
    vsEraserOpacity: TBGRAVirtualScreen;
    vsPenWidth: TBGRAVirtualScreen;
    procedure EditCopyExecute(Sender: TObject);
    procedure EditCopyUpdate(Sender: TObject);
    procedure EditCutExecute(Sender: TObject);
    procedure EditCutUpdate(Sender: TObject);
    procedure EditDeleteSelectionUpdate(Sender: TObject);
    procedure EditPasteExecute(Sender: TObject);
    procedure EditSelectionUpdate(Sender: TObject);
    procedure FileImport3DUpdate(Sender: TObject);
    procedure FilePrintExecute(Sender: TObject);
    procedure FileSaveAsInSameFolderExecute(Sender: TObject);
    procedure FileSaveAsInSameFolderUpdate(Sender: TObject);
    procedure FormMouseLeave(Sender: TObject);
    procedure PaintBox_PenPreviewMouseDown(Sender: TObject;
      {%H-}Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure SpinEdit_PhongBorderSizeChange(Sender: TObject);
    procedure Combo_SplineStyleChange(Sender: TObject);
    procedure EditDeselectUpdate(Sender: TObject);
    procedure EditPasteUpdate(Sender: TObject);
    procedure EditRedoUpdate(Sender: TObject);
    procedure EditSelectionExecute(Sender: TObject);
    procedure EditUndoUpdate(Sender: TObject);
    procedure EmbeddedCancelExecute(Sender: TObject);
    procedure EmbeddedValidateExecute(Sender: TObject);
    procedure FileImport3DExecute(Sender: TObject);
    procedure FileReloadUpdate(Sender: TObject);
    procedure FileSaveSelectionAsUpdate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormMouseEnter(Sender: TObject);
    procedure FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure ImageCropLayerUpdate(Sender: TObject);
    procedure ImageFlattenExecute(Sender: TObject);
    procedure ImageFlattenUpdate(Sender: TObject);
    procedure LayerAddNewUpdate(Sender: TObject);
    procedure LayerDuplicateUpdate(Sender: TObject);
    procedure LayerFromFileExecute(Sender: TObject);
    procedure LayerMergeOverUpdate(Sender: TObject);
    procedure LayerMoveExecute(Sender: TObject);
    procedure LayerMoveUpdate(Sender: TObject);
    procedure LayerRemoveCurrentUpdate(Sender: TObject);
    procedure LayerRotateExecute(Sender: TObject);
    procedure LayerRotateUpdate(Sender: TObject);
    procedure ItemDonateClick(Sender: TObject);
    procedure ItemHorizFlipLayerClick(Sender: TObject);
    procedure ItemHorizFlipPictureClick(Sender: TObject);
    procedure ItemHorizFlipSelectionClick(Sender: TObject);
    procedure MenuImageClick(Sender: TObject);
    procedure ItemVertFlipLayerClick(Sender: TObject);
    procedure ItemVertFlipPictureClick(Sender: TObject);
    procedure ItemVertFlipSelectionClick(Sender: TObject);
    procedure PaintBox_PictureMouseEnter(Sender: TObject);
    procedure Perspective_RepeatClick(Sender: TObject);
    procedure Perspective_TwoPlanesClick(Sender: TObject);
    procedure SpinEdit_ShapeAltitudeChange(Sender: TObject);
    procedure SpinEdit_TextSizeChange(Sender: TObject);
    procedure SpinEdit_TextureOpacityChange(Sender: TObject);
    procedure SpinEdit_TextBlurChange(Sender: TObject);
    procedure GridNb_SpinEditChange(Sender: TObject);
    procedure Image_CurrentTextureMouseDown(Sender: TObject;
      {%H-}Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure PaintBox_PenPreviewPaint(Sender: TObject);
    procedure PaintBox_PictureMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox_PictureMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBox_PictureMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox_PictureMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure PaintBox_PicturePaint(Sender: TObject);
    procedure Panel_PenWidthMouseMove(Sender: TObject; {%H-}Shift: TShiftState; {%H-}X,
      {%H-}Y: Integer);
    procedure Panel_ToolbarBackgroundMouseMove(Sender: TObject;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure SpinEdit_PenWidthMouseMove(Sender: TObject; {%H-}Shift: TShiftState;
      {%H-}X, {%H-}Y: Integer);
    procedure RenderAnyExecute(Sender: TObject);
    procedure FilterAnyExecute(Sender: TObject);
    procedure ToolAnyExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormHide(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; {%H-}Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure HelpAboutExecute(Sender: TObject);
    procedure HelpIndexExecute(Sender: TObject);
    procedure ImageChangeCanvasSizeExecute(Sender: TObject);
    procedure ImageCropUpdate(Sender: TObject);
    procedure ImageRepeatExecute(Sender: TObject);
    procedure MenuFileClick(Sender: TObject);
    procedure RecentFileClick(Sender: TObject);
    procedure LanguageClick(Sender: TObject);
    procedure SpinEdit_TextOutlineWidthChange(Sender: TObject);
    procedure SpinEdit_TextShadowXChange(Sender: TObject);
    procedure SpinEdit_TextShadowYChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TimerHidePenPreviewTimer(Sender: TObject);
    procedure ToolButton_ViewLayerStackClick(Sender: TObject);
    procedure ToolRotateSelectionUpdate(Sender: TObject);
    procedure Tool_CurveModeAngleClick(Sender: TObject);
    procedure Tool_CurveModeAutoClick(Sender: TObject);
    procedure Tool_CurveModeCurveClick(Sender: TObject);
    procedure Tool_CurveMovePointClick(Sender: TObject);
    procedure Tool_EraseOptionClick(Sender: TObject);
    procedure Tool_PhongShapeHorizontalCylinderClick(Sender: TObject);
    procedure Tool_PhongShapeVerticalConeClick(Sender: TObject);
    procedure Tool_PhongShapeVerticalCylinderClick(Sender: TObject);
    procedure ToolLayerMappingUpdate(Sender: TObject);
    procedure Tool_TextAlignClick(Sender: TObject);
    procedure Tool_TextPhongClick(Sender: TObject);
    procedure Tool_GridMoveWithoutDeformationClick(Sender: TObject);
    procedure Tool_PhongShapeConeClick(Sender: TObject);
    procedure Tool_PhongShapeRectangleClick(Sender: TObject);
    procedure Tool_PhongShapeRoundRectClick(Sender: TObject);
    procedure Tool_PhongShapeSphereClick(Sender: TObject);
    procedure Tool_SinGradientClick(Sender: TObject);
    procedure ToolLoadTextureExecute(Sender: TObject);
    procedure ToolNoTextureExecute(Sender: TObject);
    procedure ToolNoTextureUpdate(Sender: TObject);
    procedure Tool_CapFlatClick(Sender: TObject);
    procedure Tool_CapRoundClick(Sender: TObject);
    procedure Tool_CapSquareClick(Sender: TObject);
    procedure Tool_JoinBevelClick(Sender: TObject);
    procedure Tool_JoinRoundClick(Sender: TObject);
    procedure Tool_JoinMiterClick(Sender: TObject);
    procedure Tool_PenDashClick(Sender: TObject);
    procedure Tool_PenDashDotClick(Sender: TObject);
    procedure Tool_PenDashDotDotClick(Sender: TObject);
    procedure Tool_PenDotClick(Sender: TObject);
    procedure Tool_PenSolidClick(Sender: TObject);
    procedure Tool_TextFontClick(Sender: TObject);
    procedure Tool_TextOutlineClick(Sender: TObject);
    procedure Tool_TextShadowClick(Sender: TObject);
    procedure ViewColorsExecute(Sender: TObject);
    procedure ViewColorsUpdate(Sender: TObject);
    procedure ViewGridUpdate(Sender: TObject);
    procedure ViewImageListExecute(Sender: TObject);
    procedure ViewLayerStackButtonUpdate(Sender: TObject);
    procedure ViewLayerStackExecute(Sender: TObject);
    procedure ViewLayerStackUpdate(Sender: TObject);
    procedure ViewToolboxUpdate(Sender: TObject);
    procedure ViewImagelistUpdate(Sender: TObject);
    procedure SpinEdit_EraserChange(Sender: TObject);
    procedure ScriptExecute(Sender: TObject);
    procedure FileQuitExecute(Sender: TObject);
    procedure FileSaveUpdate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure ImageActionExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Image_SwapColorsMouseDown(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure SpinEdit_PenWidthChange(Sender: TObject);
    procedure Tool_CloseShapeClick(Sender: TObject);
    procedure Shape_BackColorMouseDown(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure Shape_PenColorMouseDown(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure SpinEdit_BackOpacityChange(Sender: TObject);
    procedure SpinEdit_PenOpacityChange(Sender: TObject);
    procedure SpinEdit_ToleranceChange(Sender: TObject);
    procedure Tool_DiamondGradientClick(Sender: TObject);
    procedure Tool_LinearGradientClick(Sender: TObject);
    procedure Tool_ProgressiveFloodfillClick(Sender: TObject);
    procedure Tool_RadialGradientClick(Sender: TObject);
    procedure Tool_ReflectedGradientClick(Sender: TObject);
    procedure ToolDrawShapeExecute(Sender: TObject);
    procedure ToolFillShapeExecute(Sender: TObject);
    procedure ToolMoveSelectionUpdate(Sender: TObject);
    procedure ViewToolboxExecute(Sender: TObject);
    procedure vsTextureOpacityClick(Sender: TObject);
    procedure SpinEdit_PenWidthExit(Sender: TObject);
    procedure SpinEdit_GridNbExit(Sender: TObject);
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;

  private
    { private declarations }
    SpinEdit_PenOpacity, SpinEdit_BackOpacity, SpinEdit_TextureOpacity,
    SpinEdit_Eraser,SpinEdit_Tolerance,SpinEdit_PenWidth: TBarUpDown;

    SpinEdit_TextShadowX,
    SpinEdit_TextOutlineWidth,
    SpinEdit_TextShadowY,
    SpinEdit_TextSize,
    SpinEdit_TextBlur: TBarUpDown;

    SpinEdit_GridNbX,SpinEdit_GridNbY: TBarUpDown;

    SpinEdit_PhongBorderSize, SpinEdit_ShapeAltitude: TBarUpDown;

    FLastWidth,FLastHeight: integer;
    {$IFDEF USE_IMAGE_BROWSER}
    FBrowseImages: TFBrowseImages;
    {$ENDIF}
    FInTextFont: boolean;
    FMenus: TMainFormMenu;
    FInPenWidthChange: boolean;
    FOnlineUpdater: TLazPaintCustomOnlineUpdater;
    initialized: boolean;
    shouldArrangeOnResize: boolean;
    btnLeftDown, btnRightDown : boolean;
    spacePressed: boolean;
    FormMouseMovePos: TPoint;
    InFormMouseMove: boolean;
    InFormPaint: boolean;
    FirstPaint: boolean;
    CanCompressOrUpdateStack: boolean;
    FShowSelectionNormal: boolean;
    FLazPaintInstance: TLazPaintCustomInstance;
    Config: TLazPaintConfig;
    ToolManager: TToolManager;
    Image: TLazPaintImage;
    FImageActions: TImageActions;
    StartDirectory: string;
    previousToolImg: integer;
    InShowNoPicture: boolean;
    FTopMostInfo: TTopMostInfo;
    DelayedPaintPicture: boolean;
    FCoordinatesCaption: string;
    FCoordinatesCaptionCount: NativeInt;
    FLastPictureParameters: record
       defined: boolean;
       actualZoomFactorX,actualZoomFactorY: double;
       pictureArea: TRect;
       imageOffset: TPoint;
       imageWidth,imageHeight: integer;
    end;

    selectionHighlightInfo : record
       formArea: TRect;
       ImageOffset: TPoint;
       zoomFactorX,zoomFactorY: single;
       selectionHighlight: TBGRABitmap;
       selecting : boolean;
       selectionRotateAngle: single;
       selectionRotateCenter: TPointF;
       selectionOffset: TPoint;
       partialSelectionHighlight: boolean;
       selectionHighlightOffset: TPoint;
    end;

    procedure UpdatePanelTextWidth;
    function GetCurrentTool: TPaintToolType;
    function GetVSCursorPosition: TVSCursorPosition;
    function GetZoomFactor: single;
    procedure SwitchColors;
    procedure Init;
    function TextSpinEditFocused: boolean;
    procedure OnLatestVersionUpdate(ANewVersion: string);
    procedure OnToolChanged({%H-}sender: TToolManager; {%H-}ANewTool: TPaintToolType);
    procedure OnQueryExitToolHandler({%H-}sender: TLazPaintImage);
    procedure OnZoomChanged({%H-}sender: TZoom; {%H-}ANewZoom: single);
    procedure SetLazPaintInstance(const AValue: TLazPaintCustomInstance);
    procedure SetShowSelectionNormal(const AValue: boolean);
    function RetrieveSelectionHighlight(pFormArea: TRect; pImageOffset: TPoint; pSelectionRotateAngle: single;
        pSelectionRotateCenter: TPointF;pZoomFactorX, pZoomFactorY: single; selecting: boolean; pSelectionOffset: TPoint;
        out pPartialSelectionHighlight: boolean; out pSelectionHighlightOffset: TPoint): TBGRABitmap;
    procedure StoreSelectionHighlight(pFormArea: TRect; pImageOffset: TPoint; pSelectionRotateAngle: single;
        pSelectionRotateCenter: TPointF;pZoomFactorX, pZoomFactorY: single; selecting: boolean; pSelectionOffset: TPoint;
        pSelectionHighlight: TBGRABitmap; pPartialSelectionHighlight: boolean; pSelectionHighlightOffset: TPoint);
    procedure ForgetSelectionHightlight;
    procedure ToggleToolwindowsVisible;
    procedure UpdateToolImage;
    procedure NoTextureIcon;
    procedure ToggleGridVisible;
    procedure ToggleToolboxVisible;
    procedure ToggleImageListVisible;
    procedure ToggleColorsVisible;
    procedure ToggleLayersVisible;
    function UpdateCursor(X,Y: integer): boolean;
    procedure ShowColorDialogForPen;
    procedure ShowColorDialogForBack;
    procedure ShowPenPreview(ShouldRepaint: boolean= False);
    procedure HidePenPreview(TimeMs: Integer = 300);
    procedure PaintPictureImplementation; //do not call directly
    procedure PaintVirtualScreenImplementation; //do not call directly
    procedure PaintBlueAreaImplementation;
    procedure PaintBlueAreaOnly;
    procedure OnPaintHandler;
    procedure OnImageChangedHandler({%H-}AEvent: TLazPaintImageObservationEvent);
    procedure InvalidatePicture(AInvalidateAll: boolean);
    procedure UpdateTextureIcon;
    procedure LabelAutosize(ALabel: TLabel);
    procedure AskMergeSelection(ACaption: string);
    procedure ReleaseMouseButtons(Shift: TShiftState);
    procedure UpdatePanelPhongShape;
    procedure UpdateCurveMode;
    function ShowOpenTextureDialog: boolean;
    procedure ShowNoPicture;
    function GetRenderUpdateRect(AIncludeLastToolState: boolean): TRect;
    procedure SetCurveMode(AMode: TToolSplineMode);
    procedure IncreasePenSize;
    procedure DecreasePenSize;
    function PenSizeDelta(direction: integer): integer;
    procedure UpdateWindowCaption;

    procedure RegisterScripts(ARegister: Boolean);
    function ScriptFileNew(AVars: TVariableSet): TScriptResult;
    function ScriptFileOpen(AVars: TVariableSet): TScriptResult;
    function ScriptFileSaveAs(AVars: TVariableSet): TScriptResult;
    function ScriptFileSave({%H-}AVars: TVariableSet): TScriptResult;
    function ScriptFileReload({%H-}AVars: TVariableSet): TScriptResult;
    function ScriptFileLoadSelection(AVars: TVariableSet): TScriptResult;
    function ScriptFileSaveSelectionAs(AVars: TVariableSet): TScriptResult;
    function ScriptEditPasteAsNew({%H-}AVars: TVariableSet): TScriptResult;
    function ScriptImageResample(AParams: TVariableSet): TScriptResult;
    function ScriptFilter(AVars: TVariableSet): TScriptResult;
    function ScriptColorCurves(AVars: TVariableSet): TScriptResult;
    function ScriptColorPosterize(AVars: TVariableSet): TScriptResult;
    function ScriptColorColorize(AVars: TVariableSet): TScriptResult;
    function ScriptColorShiftColors(AVars: TVariableSet): TScriptResult;
    function ScriptColorIntensity(AVars: TVariableSet): TScriptResult;
    function ScriptColorLightness(AVars: TVariableSet): TScriptResult;
    function ScriptChooseTool(AVars: TVariableSet): TScriptResult;
    function ScriptViewZoomIn({%H-}AVars: TVariableSet): TScriptResult;
    function ScriptViewZoomOut({%H-}AVars: TVariableSet): TScriptResult;
    function ScriptViewZoomOriginal({%H-}AVars: TVariableSet): TScriptResult;
    function ScriptViewZoomFit({%H-}AVars: TVariableSet): TScriptResult;
    function ScriptViewGrid(AVars: TVariableSet): TScriptResult;
    function GetScriptContext: TScriptContext;
    procedure CallScriptFunction(AName:string); overload;
    procedure CallScriptFunction(AParams:TVariableSet); overload;
    procedure ZoomFitIfTooBig;
    property Scripting: TScriptContext read GetScriptContext;

  public
    { public declarations }
    pictureOrigin, pictureOffset, pictureViewSize, pictureActualSize: TPoint;
    FormBackgroundColor: TColor;
    virtualScreen : TBGRABitmap;
    virtualScreenPenCursor: boolean;
    virtualScreenPenCursorPos,virtualScreenPenCursorPosBefore: TVSCursorPosition;
    QueryPaintVirtualScreen: boolean;
    StackNeedUpdate: boolean;
    Zoom: TZoom;

    procedure PaintPictureNow;
    procedure PaintVirtualScreenCursor;
    function TryOpenFileUTF8(filenameUTF8: string; AddToRecent: Boolean=True): Boolean;
    function FormToBitmap(X,Y: Integer): TPointF;
    function FormToBitmap(pt: TPoint): TPointF;
    function BitmapToForm(X,Y: Single): TPointF;
    function BitmapToForm(pt: TPointF): TPointF;
    function BitmapToVirtualScreen(X,Y: Single): TPointF;
    function BitmapToVirtualScreen(ptF: TPointF): TPointF;
    function PictureCanvas: TCanvas;
    function PictureCanvasOfs: TPoint;
    procedure UpdateLineCapBar;
    procedure UpdateToolbar;
    function ChooseTool(Tool : TPaintToolType): boolean;
    procedure ArrangeToolbars;
    function GetPictureArea: TRect;
    procedure PictureSelectionChanged({%H-}sender: TLazPaintImage; AOffsetOnly: boolean);
    procedure SetCurrentFilenameUTF8(path: string);
    function GetCurrentFilenameUTF8: string;
    property LazPaintInstance: TLazPaintCustomInstance read FLazPaintInstance write SetLazPaintInstance;
    procedure UpdateEditPicture(ADelayed: boolean = false);
    property CurrentTool: TPaintToolType read GetCurrentTool;
    property ShowSelectionNormal: boolean read FShowSelectionNormal write SetShowSelectionNormal;
    property ZoomFactor: single read GetZoomFactor;
  end;

implementation

uses LCLIntf, LCLProc, ugraph, math, umac, uclipboard, ucursors,
   ufilters, ULoadImage, ULoading, UFileExtensions;

const PenWidthFactor = 10;

{ TFMain }

procedure TFMain.FormCreate(Sender: TObject);
begin
  initialized := false;

  SpinEdit_PenOpacity := TBarUpDown.Create(vsPenOpacity,0,255,255);
  SpinEdit_PenOpacity.Increment := 15;
  SpinEdit_PenOpacity.OnChange := @SpinEdit_PenOpacityChange;

  SpinEdit_BackOpacity := TBarUpDown.Create(vsBackOpacity,0,255,255);
  SpinEdit_BackOpacity.Increment := 15;
  SpinEdit_BackOpacity.OnChange := @SpinEdit_BackOpacityChange;

  SpinEdit_TextureOpacity := TBarUpDown.Create(vsTextureOpacity,0,255,255);
  SpinEdit_TextureOpacity.Increment := 15;
  SpinEdit_TextureOpacity.OnChange := @SpinEdit_TextureOpacityChange;

  SpinEdit_Eraser := TBarUpDown.Create(vsEraserOpacity,0,255,255);
  SpinEdit_Eraser.Increment := 15;
  SpinEdit_Eraser.OnChange := @SpinEdit_EraserChange;

  SpinEdit_Tolerance := TBarUpDown.Create(vsTolerance,0,255,128);
  SpinEdit_Tolerance.Increment := 5;
  SpinEdit_Tolerance.OnChange := @SpinEdit_ToleranceChange;

  SpinEdit_PenWidth := TBarUpDown.Create(vsPenWidth,1,9999,10);
  SpinEdit_PenWidth.OnChange := @SpinEdit_PenWidthChange;
  SpinEdit_PenWidth.OnMouseMove := @SpinEdit_PenWidthMouseMove;
  SpinEdit_PenWidth.OnExit:= @SpinEdit_PenWidthExit;
  SpinEdit_PenWidth.BarExponent := 3;

  SpinEdit_TextShadowX := TBarUpDown.Create(vsTextShadowX,-100,100,0);
  SpinEdit_TextShadowX.Increment := 1;
  SpinEdit_TextShadowX.OnChange := @SpinEdit_TextShadowXChange;

  SpinEdit_TextShadowY := TBarUpDown.Create(vsTextShadowY,-100,100,0);
  SpinEdit_TextShadowY.Increment := 1;
  SpinEdit_TextShadowY.OnChange := @SpinEdit_TextShadowYChange;

  SpinEdit_TextOutlineWidth := TBarUpDown.Create(vsTextOutlineWidth,1,100,3);
  SpinEdit_TextOutlineWidth.Increment := 1;
  SpinEdit_TextOutlineWidth.OnChange := @SpinEdit_TextOutlineWidthChange;

  SpinEdit_TextSize := TBarUpDown.Create(vsTextSize,1,999,12);
  SpinEdit_TextSize.Increment := 5;
  SpinEdit_TextSize.OnChange := @SpinEdit_TextSizeChange;
  SpinEdit_TextSize.BarExponent:= 3;

  SpinEdit_TextBlur := TBarUpDown.Create(vsTextBlur,1,100,3);
  SpinEdit_TextBlur.Increment := 1;
  SpinEdit_TextBlur.OnChange := @SpinEdit_TextBlurChange;

  SpinEdit_GridNbX := TBarUpDown.Create(vsGridNbX,1,100,10);
  SpinEdit_GridNbX.Increment := 1;
  SpinEdit_GridNbX.OnChange := @GridNb_SpinEditChange;
  SpinEdit_GridNbX.OnExit := @SpinEdit_GridNbExit;

  SpinEdit_GridNbY := TBarUpDown.Create(vsGridNbY,1,100,10);
  SpinEdit_GridNbY.Increment := 1;
  SpinEdit_GridNbY.OnChange := @GridNb_SpinEditChange;
  SpinEdit_GridNbY.OnExit := @SpinEdit_GridNbExit;

  SpinEdit_PhongBorderSize := TBarUpDown.Create(vsPhongBorderSize,1,100,10);
  SpinEdit_PhongBorderSize.Increment := 3;
  SpinEdit_PhongBorderSize.OnChange := @SpinEdit_PhongBorderSizeChange;

  SpinEdit_ShapeAltitude := TBarUpDown.Create(vsShapeAltitude,1,100,10);
  SpinEdit_ShapeAltitude.Increment := 1;
  SpinEdit_ShapeAltitude.OnChange := @SpinEdit_ShapeAltitudeChange;

  //layout init
  ScaleDPI(Self,OriginalDPI);
  ScaleImageList(ImageList1,OriginalDPI);
  Zoom := TZoom.Create(Label_CurrentZoom,Edit_Zoom);
  Zoom.OnZoomChanged:= @OnZoomChanged;
  pictureOrigin := Point(0,0);
  virtualScreen := nil;
  previousToolImg:= -1;

  {$IFDEF USEPAINTBOXPICTURE}
  PaintBox_Picture.Left := 0;
  PaintBox_Picture.Width := ClientWidth;
  PaintBox_Picture.Visible := True;
  {$ENDIF}

  {$IFDEF WINDOWS}
    StartDirectory := SysToUTF8(ExtractFilePath(Application.ExeName));
  {$ELSE}
    StartDirectory := GetCurrentDirUTF8;
  {$ENDIF}

  //use background color
  FormBackgroundColor := OutsideColor;
  self.Color := clBtnFace; //toolbar color inherited on mac

  //mouse status
  btnLeftDown := false;
  btnRightDown := false;

  //recursive calls
  InFormMouseMove:= false;
  InFormPaint := false;

  //mac interface
  CheckQuitMenu(ItemQuit,ItemQuitSeparator);
  CheckActions(ActionList1);

  Label_Coordinates.Caption := '';
  FCoordinatesCaption:= '';
  FCoordinatesCaptionCount := 0;
  LabelAutosize(Label_Pen);
  LabelAutosize(Label_Back);
  LabelAutosize(Label_PenWidth);
  LabelAutosize(Label_Eraser);
  LabelAutosize(Label_Tolerance);
  LabelAutosize(Label_Grid);
  LabelAutosize(Label_Curve);
  LabelAutosize(Label_Text);
  LabelAutosize(Label_TextBlur);
  LabelAutosize(Label_ShadowOffset);
  LabelAutosize(Label_Shape);
  LabelAutosize(Label_PhongBorder);
  LabelAutosize(Label_Altitude);
  LabelAutosize(Label_OutlineWidth);
  Image_SwapColors.Hint := Image_SwapColors.Hint + ' (X)';
  NoTextureIcon;

  MenuHorizFlipSub.ImageIndex := ImageHorizontalFlip.ImageIndex;
  MenuVertFlipSub.ImageIndex := ImageVerticalFlip.ImageIndex;

  FMenus := TMainFormMenu.Create(ActionList1);
  FMenus.PredefinedMainMenus([MenuFile,MenuEdit,MenuSelect,MenuView, MenuImage,MenuRemoveTransparency,
    MenuColors,MenuTool, MenuFilter,MenuRadialBlur, MenuRender,MenuHelp]);
  FMenus.Toolbars([Panel_Embedded,Panel_File,Panel_Zoom,Panel_Undo,Panel_CopyPaste,Panel_Coordinates,
    Panel_Tool,Panel_Color,Panel_Texture,Panel_Grid,Panel_PenWidth,Panel_ShapeOption,Panel_LineCap,Panel_JoinStyle,
    Panel_PenStyle,Panel_SplineStyle,Panel_Eraser,Panel_Tolerance,Panel_GradientType,Panel_Text,Panel_TextOutline,
    Panel_PhongShape,Panel_Altitude,Panel_PerspectiveOption],Panel_ToolbarBackground);
  FMenus.Apply;

  Tool_CurveModeAuto.Hint := Tool_CurveModeAuto.Hint + ' (A)';
  Tool_CurveModeAngle.Hint := Tool_CurveModeAngle.Hint + ' (X)';
  Tool_CurveModeCurve.Hint := Tool_CurveModeCurve.Hint + ' (S)';
  Tool_CurveMovePoint.Hint := Tool_CurveMovePoint.Hint + ' (Z)';

  OpenTextureDialog.Filter := GetExtensionFilter([eoReadable]);
  OpenPictureDialog1.Filter := GetExtensionFilter([eoReadable]);
  LoadSelectionDialog.Filter := GetExtensionFilter([eoReadable]);
  SavePictureDialog1.Filter := GetExtensionFilter([eoWritable]);
  SaveSelectionDialog.Filter := GetExtensionFilter([eoWritable]);

  initialized := true;
  FirstPaint := true;
end;

procedure TFMain.FormDestroy(Sender: TObject);
begin
  RegisterScripts(False);
  FreeAndNil(SpinEdit_PenOpacity);
  FreeAndNil(SpinEdit_BackOpacity);
  FreeAndNil(SpinEdit_TextureOpacity);
  FreeAndNil(SpinEdit_Eraser);
  FreeAndNil(SpinEdit_Tolerance);
  FreeAndNil(SpinEdit_PenWidth);

  FreeAndNil(SpinEdit_TextShadowX);
  FreeAndNil(SpinEdit_TextOutlineWidth);
  FreeAndNil(SpinEdit_TextShadowY);
  FreeAndNil(SpinEdit_TextSize);
  FreeAndNil(SpinEdit_TextBlur);

  FreeAndNil(SpinEdit_GridNbX);
  FreeAndNil(SpinEdit_GridNbY);

  FreeAndNil(SpinEdit_PhongBorderSize);
  FreeAndNil(SpinEdit_ShapeAltitude);

  FreeAndNil(FImageActions);

  if ToolManager.OnToolChanged = @OnToolChanged then
    ToolManager.OnToolChanged := nil;
  ForgetSelectionHightlight;
  FreeAndNil(Zoom);
  FreeAndNil(virtualScreen);
  FreeAndNil(FOnlineUpdater);
  FreeAndNil(FMenus);
end;

procedure TFMain.SetLazPaintInstance(const AValue: TLazPaintCustomInstance);
begin
  if (FLazPaintInstance = nil) and (AValue <> nil) then
  begin
    FLazPaintInstance := AValue;
    Init;
  end;
end;

procedure TFMain.Init;
begin
  initialized := false;
  Config := LazPaintInstance.Config;
  if Config.Default3dObjectDirectory = '' then
    Config.SetDefault3dObjectDirectory(StartDirectory);

  ToolManager := LazPaintInstance.ToolManager;
  Image := LazPaintInstance.Image;
  FImageActions := TImageActions.Create(LazPaintInstance);
  Panel_Embedded.Visible := LazPaintInstance.Embedded;
  Panel_File.Visible := not LazPaintInstance.Embedded;
  LazPaintInstance.EmbeddedResult := mrNone;

  Image.OnSelectionChanged := @PictureSelectionChanged;
  SetCurrentFilenameUTF8('');

  ToolManager.PenWidthControls.Add(Panel_PenWidth);
  ToolManager.ShapeControls.Add(Panel_ShapeOption);
  ToolManager.ShapeControls.Add(Panel_PenStyle);
  ToolManager.JoinStyleControls.Add(Panel_JoinStyle);
  ToolManager.SplineStyleControls.Add(Panel_SplineStyle);
  ToolManager.LineCapControls.Add(Panel_LineCap);
  ToolManager.EraserControls.Add(Panel_Eraser);
  ToolManager.ToleranceControls.Add(Panel_Tolerance);
  ToolManager.GradientControls.Add(Panel_GradientType);
  ToolManager.DeformationControls.Add(Panel_Grid);
  ToolManager.TextControls.Add(Panel_Text);
  ToolManager.TextControls.Add(Panel_TextOutline);
  ToolManager.PhongControls.Add(Panel_PhongShape);
  ToolManager.AltitudeControls.Add(Panel_Altitude);
  ToolManager.PerspectiveControls.Add(Panel_PerspectiveOption);
  ToolManager.PenColorOrTexControls.Add(Panel_Color);
  ToolManager.PenColorOrTexControls.Add(Panel_Texture);

  ToolManager.SetCurrentToolType(ptHand);
  ToolManager.OnToolChanged := @OnToolChanged;
  UpdateToolImage;

  FImageActions.SetCurrentBitmap(TBGRABitmap.Create(Config.DefaultImageWidth,Config.DefaultImageHeight,BGRAPixelTransparent), false);
  image.ClearUndo;
  image.SetSavedFlag;

  //toolbar init
  Shape_PenColor.Brush.Color := BGRAToColor(ToolManager.ToolForeColor);
  SpinEdit_PenOpacity.Value := ToolManager.ToolForeColor.alpha;
  Shape_BackColor.Brush.Color := BGRAToColor(ToolManager.ToolBackColor);
  SpinEdit_BackOpacity.Value := ToolManager.ToolBackColor.alpha;
  SpinEdit_PenWidth.Value := Round(ToolManager.ToolPenWidth*PenWidthFactor);
  Tool_DrawShapeBorder.Down := ToolManager.ToolOptionDrawShape;
  Tool_FillShape.Down := ToolManager.ToolOptionFillShape;
  Tool_CloseShape.Down := ToolManager.ToolOptionCloseShape;
  SpinEdit_Eraser.Value := ToolManager.ToolEraserAlpha;
  SpinEdit_Tolerance.Value := ToolManager.ToolTolerance;
  Tool_ProgressiveFloodfill.Down := ToolManager.ToolFloodFillOptionProgressive;
  Tool_SinGradient.Down := ToolManager.ToolGradientSine;
  Tool_GridMoveWithoutDeformation.Down := ToolManager.ToolDeformationGridMoveWithoutDeformation;
  SpinEdit_GridNbX.Value := ToolManager.ToolDeformationGridNbX-1;
  SpinEdit_GridNbY.Value := ToolManager.ToolDeformationGridNbY-1;
  if ToolManager.ToolSplineEasyBezier then
    Combo_SplineStyle.ItemIndex := Combo_SplineStyle.Items.IndexOf('Easy Bézier')
  else
    Combo_SplineStyle.ItemIndex:= ord(ToolManager.ToolSplineStyle);
  UpdateCurveMode;
  Tool_TextOutline.Down := ToolManager.ToolTextOutline;
  SpinEdit_TextOutlineWidth.Value := ToolManager.ToolTextOutlineWidth;
  Tool_TextShadow.Down := ToolManager.ToolTextShadow;
  Tool_TextPhong.Down := ToolManager.ToolTextPhong;
  FontDialog1.Font := ToolManager.ToolTextFont;
  SpinEdit_TextBlur.Value := ToolManager.ToolTextBlur;
  SpinEdit_TextSize.Value := ToolManager.ToolTextFont.Size;
  SpinEdit_TextShadowX.Value := ToolManager.ToolTextShadowOffset.X;
  SpinEdit_TextShadowY.Value := ToolManager.ToolTextShadowOffset.Y;
  SpinEdit_TextureOpacity.Value := ToolManager.ToolTextureOpacity;
  if ToolManager.ToolShapeType = 'RoundRectangle' then
    Tool_PhongShapeRoundRect.Down := true;
  if ToolManager.ToolShapeType = 'Sphere' then
    Tool_PhongShapeSphere.Down := true;
  if ToolManager.ToolShapeType = 'Cone' then
    Tool_PhongShapeCone.Down := true;
  if ToolManager.ToolShapeType = 'VerticalCone' then
    Tool_PhongShapeVerticalCone.Down := true;
  if ToolManager.ToolShapeType = 'HorizontalCylinder' then
    Tool_PhongShapeHorizontalCylinder.Down := true;
  if ToolManager.ToolShapeType = 'VerticalCylinder' then
    Tool_PhongShapeVerticalCylinder.Down := true;
  UpdatePanelPhongShape;
  SpinEdit_ShapeAltitude.Value := ToolManager.ToolShapeAltitude;
  SpinEdit_ShapeAltitudeChange(nil);
  SpinEdit_PhongBorderSize.Value := ToolManager.ToolShapeBorderSize;
  UpdateLineCapBar;

  ViewGrid.Checked := LazPaintInstance.GridVisible;
  ItemViewGrid.Checked:= LazPaintInstance.GridVisible;
  ColorColorize.Visible := not LazPaintInstance.BlackAndWhite;
  ColorShiftColors.Visible := not LazPaintInstance.BlackAndWhite;
  ColorIntensity.Visible := not LazPaintInstance.BlackAndWhite;
  FilterGrayscale.Visible := not LazPaintInstance.BlackAndWhite;
  FilterClearType.Visible := not LazPaintInstance.BlackAndWhite;
  FilterClearTypeInverse.Visible := not LazPaintInstance.BlackAndWhite;

  if not LazPaintInstance.Embedded then
  begin
    FOnlineUpdater := LazPaintInstance.GetOnlineUpdater;
    If Assigned(FOnlineUpdater) then
      FOnlineUpdater.OnLatestVersionUpdate := @OnLatestVersionUpdate;
  end;
  initialized := true;

  RegisterScripts(True)
end;

function TFMain.TextSpinEditFocused: boolean;
begin
  result := SpinEdit_TextSize.Focused or SpinEdit_TextBlur.Focused or SpinEdit_TextShadowX.Focused or
           SpinEdit_TextShadowY.Focused or SpinEdit_ShapeAltitude.Focused or
           SpinEdit_TextOutlineWidth.Focused;
end;

procedure TFMain.OnLatestVersionUpdate(ANewVersion: string);
begin
  if ANewVersion <> LazPaintCurrentVersionOnly then
    LazPaintInstance.ShowMessage(rsLazPaint, rsLatestVersion + ' ' + ANewVersion);
end;

procedure TFMain.RegisterScripts(ARegister: Boolean);
begin
  Scripting.RegisterScriptFunction('FileNew',@ScriptFileNew,ARegister);
  Scripting.RegisterScriptFunction('FileOpen',@ScriptFileOpen,ARegister);
  Scripting.RegisterScriptFunction('FileSaveAs',@ScriptFileSaveAs,ARegister);
  Scripting.RegisterScriptFunction('FileSave',@ScriptFileSave,ARegister);
  Scripting.RegisterScriptFunction('FileReload',@ScriptFileReload,ARegister);
  Scripting.RegisterScriptFunction('FileLoadSelection',@ScriptFileLoadSelection,ARegister);
  Scripting.RegisterScriptFunction('FileSaveSelectionAs',@ScriptFileSaveSelectionAs,ARegister);
  Scripting.RegisterScriptFunction('EditPasteAsNew',@ScriptEditPasteAsNew,ARegister);
  Scripting.RegisterScriptFunction('Filter',@ScriptFilter,ARegister);
  Scripting.RegisterScriptFunction('ColorCurves',@ScriptColorCurves,ARegister);
  Scripting.RegisterScriptFunction('ColorColorize',@ScriptColorColorize,ARegister);
  Scripting.RegisterScriptFunction('ColorPosterize',@ScriptColorPosterize,ARegister);
  Scripting.RegisterScriptFunction('ColorShiftColors',@ScriptColorShiftColors,ARegister);
  Scripting.RegisterScriptFunction('ColorIntensity',@ScriptColorIntensity,ARegister);
  Scripting.RegisterScriptFunction('ColorLightness',@ScriptColorLightness,ARegister);
  Scripting.RegisterScriptFunction('ImageResample',@ScriptImageResample,ARegister);
  Scripting.RegisterScriptFunction('ChooseTool',@ScriptChooseTool,ARegister);
  Scripting.RegisterScriptFunction('ViewZoomIn',@ScriptViewZoomIn,ARegister);
  Scripting.RegisterScriptFunction('ViewZoomOut',@ScriptViewZoomOut,ARegister);
  Scripting.RegisterScriptFunction('ViewZoomOriginal',@ScriptViewZoomOriginal,ARegister);
  Scripting.RegisterScriptFunction('ViewZoomFit',@ScriptViewZoomFit,ARegister);
  Scripting.RegisterScriptFunction('ViewGrid',@ScriptViewGrid,ARegister);
end;

procedure TFMain.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ReleaseMouseButtons(Shift);
  if not (Button in[mbLeft,mbRight]) then exit;
  CanCompressOrUpdateStack := false;
  Image.OnImageChanged.DelayedStackUpdate := True;

  if btnLeftDown or btnRightDown then exit;
  if Button = mbLeft then
    btnLeftDown := true else
  if Button = mbRight then
    btnRightDown := true
      else exit;

  if ToolManager.ToolDown(FormToBitmap(X,Y),btnRightDown) then
      PaintPictureNow;
  UpdateToolbar;
end;

procedure TFMain.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var BmpPos: TPointF;
    updateForVSCursor: boolean;
    virtualScreenPenCursorBefore: boolean;

//var tickstart:DWord;
begin
  //tickstart := GetTickCount;

  ReleaseMouseButtons(Shift);
  HidePenPreview;
  if LazPaintInstance.TopMostHasFocus then
  begin
    if LazPaintInstance.TopMostOkToUnfocus then
      self.SetFocus
    else
      exit;
  end;
  if (CurrentTool = ptText) and TextSpinEditFocused then SpinEdit_PenOpacity.SetFocus;

  FormMouseMovePos := Point(X,Y);
  if InFormMouseMove then exit;
  InFormMouseMove := True;
  //Application.ProcessMessages; //empty message stack

  BmpPos := FormToBitmap(FormMouseMovePos);
  FCoordinatesCaption := IntToStr(round(BmpPos.X))+','+IntToStr(round(BmpPos.Y));
  Inc(FCoordinatesCaptionCount);
  if FCoordinatesCaptionCount > 8 then
  begin
    FCoordinatesCaptionCount := 0;
    Label_Coordinates.caption := FCoordinatesCaption;
    Label_Coordinates.Update;
  end;
  updateForVSCursor:= false;
  if ToolManager.ToolMove(BmpPos) then
  begin
    virtualScreenPenCursorPosBefore := virtualScreenPenCursorPos;
    virtualScreenPenCursorPos := GetVSCursorPosition;
    PaintPictureNow;
    virtualScreenPenCursorPosBefore.bounds := EmptyRect;
    ToolManager.ToolMoveAfter(FormToBitmap(FormMouseMovePos)); //new BmpPos after repaint
  end else
    updateForVSCursor := true;
  UpdateToolbar;

  virtualScreenPenCursorBefore := virtualScreenPenCursor;
  if updateForVSCursor then
  begin
    UpdateCursor(X,Y);
    if (virtualScreenPenCursor or virtualScreenPenCursorBefore) then
      PaintVirtualScreenCursor;
  end;

  if ToolManager.ToolSleeping and not spacePressed and not btnLeftDown and not btnRightDown then
    ToolManager.ToolWakeUp;

  InFormMouseMove := False;
  //Canvas.TextOut(0,FMenus.ToolbarsHeight,inttostr(GetTickCount-tickstart)+'     ');
end;

procedure TFMain.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var redraw: boolean;
begin
  redraw := ToolManager.ToolMove(FormToBitmap(X,Y));
  if (btnLeftDown and (Button = mbLeft)) or (btnRightDown and (Button=mbRight)) then
  begin
    if ToolManager.ToolUp then redraw := true;
    btnLeftDown := false;
    btnRightDown := false;
  end;
  if redraw then PaintPictureNow;
  UpdateToolbar;
  ReleaseMouseButtons(Shift);

  if ToolManager.ToolSleeping and not spacePressed and not btnLeftDown and not btnRightDown then
    ToolManager.ToolWakeUp;
end;

function TFMain.ScriptFileOpen(AVars: TVariableSet): TScriptResult;
var vFilename: TScriptVariableReference;
    topInfo: TTopMostInfo;
    {$IFDEF USE_IMAGE_BROWSER}
    i: integer;
    {$ENDIF}
    cancelled: boolean;
    chosenFiles: array of string;
begin
  topInfo.defined:= false;
  if Image.IsFileModified then
  begin
    topInfo := FLazPaintInstance.HideTopmost;
    case LazPaintInstance.SaveQuestion(rsOpen) of
    IDYES: begin
             result := Scripting.CallScriptFunction('FileSave');
             if result <> srOk then
             begin
               FLazPaintInstance.ShowTopmost(topInfo);
               exit;
             end;
           end;
    IDCANCEL: begin
                FLazPaintInstance.ShowTopmost(topInfo);
                result := srCancelledByUser;
                exit;
              end;
    end;
  end;
  vFileName := AVars.GetVariable('FileName');
  if AVars.IsReferenceDefined(vFileName) then
  begin
    FLazPaintInstance.ShowTopmost(topInfo);
    if TryOpenFileUTF8(AVars.GetString(vFilename)) then
      result := srOk
    else
      result := srException;
  end
  else
  begin
    {$IFDEF USE_IMAGE_BROWSER}
    if not assigned(FBrowseImages) then
    begin
      FBrowseImages := TFBrowseImages.Create(self);
      FBrowseImages.LazPaintInstance := LazPaintInstance;
    end;
    {$ENDIF}
    try
      if not topInfo.defined then topInfo := FLazPaintInstance.HideTopmost;
      {$IFDEF USE_IMAGE_BROWSER}
      self.Hide;
      FBrowseImages.ListBox_RecentDirs.Clear;
      for i := 0 to Config.RecentDirectoriesCount-1 do
        FBrowseImages.ListBox_RecentDirs.Items.Add(Config.RecentDirectory[i]);
      if FBrowseImages.ShowModal = mrOK then
      begin
        setlength(chosenFiles, FBrowseImages.SelectedFileCount);
        for i := 0 to high(chosenFiles) do
          chosenFiles[i] := FBrowseImages.SelectedFile[i];
        cancelled := false
      end
      else
      begin
        chosenFiles := nil;
        cancelled := true;
      end;
      {$ELSE}
      if OpenPictureDialog1.Execute then
      begin
        chosenFile:= OpenPictureDialog1.FileName;
        cancelled := false;
      end
      else
      begin
        chosenFile:= '';
        cancelled:= true;
      end;
      {$ENDIF}
      if not cancelled then
      begin
        if length(chosenFiles) = 1 then
        begin
          if TryOpenFileUTF8(chosenFiles[0]) then
          begin
            result := srOk;
            if Assigned(Scripting.RecordingFunctionParameters) then
               Scripting.RecordingFunctionParameters.AddString('FileName',chosenFiles[0]);
          end
          else
            result := srException;
        end else
        begin
          result := srOk;
          FormDropFiles(self, chosenFiles);
        end;
      end
      else
        result := srCancelledByUser;
    finally
      self.Show;
      FLazPaintInstance.ShowTopmost(topInfo);
    end;
  end;
end;

procedure TFMain.FileQuitExecute(Sender: TObject);
begin
  Close;
end;

function TFMain.ScriptFileSaveAs(AVars: TVariableSet): TScriptResult;
var filename: string;
    vFileName: TScriptVariableReference;
    topMost: TTopMostInfo;
begin
  AskMergeSelection(rsSave);
  filename := ExtractFileName(GetCurrentFilenameUTF8);
  vFileName := AVars.GetVariable('FileName');
  if AVars.IsReferenceDefined(vFileName) then filename := AVars.GetString(vFileName);
  if filename = '' then filename := rsNoName;
  if SavePictureDialog1.FilterIndex > 1 then
    filename := ApplySelectedFilterExtension(filename,SavePictureDialog1.Filter,SavePictureDialog1.FilterIndex);
  if not Image.AbleToSaveAsUTF8(filename) then
  begin
    SavePictureDialog1.FilterIndex := 1;
    filename := ChangeFileExt(Filename,'');
  end;
  SavePictureDialog1.FileName := filename;
  topMost := LazPaintInstance.HideTopmost;
  if SavePictureDialog1.Execute then
  begin
    filename := SavePictureDialog1.FileName;
    if not Image.AbleToSaveAsUTF8(filename) then
    begin
      LazPaintInstance.ShowError(rsSave, rsFileExtensionNotSupported);
      result := srException;
    end else
    begin
      try
        Image.SaveToFileUTF8(filename);
        Config.AddRecentFile(filename);
        Config.AddRecentDirectory(ExtractFilePath(filename));
        SetCurrentFilenameUTF8(filename);
        result := srOk;
        if Assigned(Scripting.RecordingFunctionParameters) then
           Scripting.RecordingFunctionParameters.AddString('FileName',filename);
      except
        on ex: Exception do
        begin
          LazPaintInstance.ShowError('FileSaveAs',ex.Message);
          result := srException;
        end;
      end;
    end;
  end else
    result := srCancelledByUser;
  LazPaintInstance.ShowTopmost(topMost);
end;

function TFMain.ScriptFileSave(AVars: TVariableSet): TScriptResult;
begin
  if (GetCurrentFilenameUTF8 = '') or not Image.AbleToSaveAsUTF8(GetCurrentFilenameUTF8) then
    result := Scripting.CallScriptFunction('FileSaveAs', True) else
    begin
      AskMergeSelection(rsSave);
      try
        Image.SaveToFileUTF8(GetCurrentFilenameUTF8);
        result := srOk;
      except
        on ex: Exception do
        begin
          LazPaintInstance.ShowError('FileSave',ex.Message);
          result := srException;
        end;
      end;
    end;
end;

procedure TFMain.FileSaveUpdate(Sender: TObject);
begin
   FileSave.Enabled := image.IsFileModified;
end;

procedure TFMain.FilterAnyExecute(Sender: TObject);
var filterName: string;
    params: TVariableSet;
begin
  if Sender is TAction then
  begin
    filterName := (Sender as TAction).Name;
    if (length(filterName) >= 7) and (copy(filterName,1,6) = 'Filter') and
        (filterName[7] = upcase(filterName[7])) then
          delete(filterName,1,6);
    params := TVariableSet.Create('Filter');
    params.AddString('Name', filterName);
    CallScriptFunction(params);
    params.Free;
  end;
end;

procedure TFMain.RenderAnyExecute(Sender: TObject);
var filterName: string;
    params: TVariableSet;
begin
  if Sender is TAction then
  begin
    filterName := (Sender as TAction).Name;
    if (length(filterName) >= 7) and (copy(filterName,1,6) = 'Render') and
        (filterName[7] = upcase(filterName[7])) then
          delete(filterName,1,6);
    params := TVariableSet.Create('Filter');
    params.AddString('Name', filterName);
    CallScriptFunction(params);
    params.Free;
  end;
end;

procedure TFMain.ToolAnyExecute(Sender: TObject);
var toolName: string;
    params: TVariableSet;
begin
  if Sender is TAction then
  begin
    toolName := (Sender as TAction).Name;
    if (length(toolName) >= 5) and (copy(toolName,1,4) = 'Tool') and
        (toolName[5] = upcase(toolName[5])) then
          delete(toolName,1,4);
    params := TVariableSet.Create('ChooseTool');
    params.AddString('Name', toolName);
    CallScriptFunction(params);
    params.Free;
  end;
end;

procedure TFMain.SetCurrentFilenameUTF8(path: string);
begin
  image.currentFilenameUTF8 := path;
  UpdateWindowCaption;
end;

function TFMain.GetCurrentFilenameUTF8: string;
begin
  result := image.currentFilenameUTF8;
end;

procedure TFMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  try
    if Zoom.EditingZoom then exit;
    if not ((CurrentTool = ptText) and TextSpinEditFocused and (Key = VK_BACK)) and ToolManager.ToolKeyDown(Key) then
    begin
      DelayedPaintPicture := True;
    end else
    if Key = VK_F6 then
    begin
      ToggleToolwindowsVisible;
      Key := 0;
    end else
    If (Key = VK_SPACE) and not spacePressed then
    begin
      spacePressed:= true;
      Key := 0;
      if not btnLeftDown and not btnRightDown then ToolManager.ToolSleep;
    end else
    if LazPaintInstance.ImageListWindowVisible then
      LazPaintInstance.ImageListWindowVisibleKeyDown(Key,Shift);
    If Key = 0 then UpdateToolbar;
  except
    on ex:exception do
      LazPaintInstance.ShowError('KeyDown',ex.Message);
  end;
end;

procedure TFMain.FormResize(Sender: TObject);
begin
  if shouldArrangeOnResize then ArrangeToolbars;
end;

procedure TFMain.ImageActionExecute(Sender: TObject);
var actionName: string;
begin
  if Sender is TAction then
  begin
    actionName:= (Sender as TAction).Name;
    if (actionName = 'ImageHorizontalFlip') and not image.SelectionEmpty then actionName := 'SelectionHorizontalFlip' else
    if (actionName = 'ImageVerticalFlip') and not image.SelectionEmpty  then actionName := 'SelectionVerticalFlip';
    CallScriptFunction(actionName);
  end;
end;

procedure TFMain.EditUndoUpdate(Sender: TObject);
begin
  EditUndo.Enabled := image.CanUndo;
end;

procedure TFMain.EmbeddedCancelExecute(Sender: TObject);
begin
  LazPaintInstance.EmbeddedResult := mrCancel;
  Close;
end;

procedure TFMain.EmbeddedValidateExecute(Sender: TObject);
begin
  LazPaintInstance.EmbeddedResult := mrOK;
  Close;
end;

procedure TFMain.FileImport3DExecute(Sender: TObject);
var topmostInfo: TTopMostInfo;
begin
  topmostInfo:= LazPaintInstance.HideTopmost;
  Open3DObjectDialog.InitialDir := Config.Default3dObjectDirectory;
  if Open3DObjectDialog.Execute then
  begin
    FImageActions.Import3DObject(Open3DObjectDialog.FileName);
    Config.SetDefault3dObjectDirectory(ExtractFilePath(Open3DObjectDialog.FileName));
  end;
  LazPaintInstance.ShowTopmost(topmostInfo);
end;

function TFMain.ScriptFileLoadSelection(AVars: TVariableSet): TScriptResult;
var selectionFileName: string;
    vFileName: TScriptVariableReference;
begin
  vFileName := AVars.GetVariable('FileName');
  if AVars.IsReferenceDefined(vFileName) then
    selectionFileName := AVars.GetString(vFileName)
  else
  begin
    if LoadSelectionDialog.Execute then
      selectionFileName := LoadSelectionDialog.Filename
    else
    begin
      result := srCancelledByUser;
      exit;
    end;
  end;
  if FImageActions.LoadSelection(selectionFileName) then
  begin
    SaveSelectionDialog.GetFilterExt;
    SaveSelectionDialog.Filename := selectionFileName;
    if Assigned(Scripting.RecordingFunctionParameters) then
       Scripting.RecordingFunctionParameters.AddString('FileName',selectionFileName);
    result := srOk;
  end
  else result := srException;
end;

function TFMain.ScriptFileReload(AVars: TVariableSet): TScriptResult;
var topmostInfo: TTopMostInfo; res: integer;
begin
  if (GetCurrentFilenameUTF8 = '') then
  begin
    result := srOk;
    exit;
  end;
  if Image.IsFileModified then
  begin
    topmostInfo := LazPaintInstance.HideTopmost;
    res := MessageDlg(rsReload,rsReloadChanged,mtWarning,mbYesNo,0);
    LazPaintInstance.ShowTopmost(topmostInfo);
    if res <> IDYES then
    begin
      result := srCancelledByUser;
      exit;
    end;
  end;
  if TryOpenFileUTF8(GetCurrentFilenameUTF8) then
    result := srOk
  else
    result := srException;
end;

procedure TFMain.FileReloadUpdate(Sender: TObject);
begin
  FileReload.Enabled := (GetCurrentFilenameUTF8 <> '');
end;

function TFMain.ScriptFileSaveSelectionAs(AVars: TVariableSet): TScriptResult;
var previousFilename, filename: string;
    vFileName: TScriptVariableReference;
begin
  if Image.SelectionEmpty then
  begin
    result := srOk;
    exit;
  end;
  previousFilename:= SaveSelectionDialog.FileName;
  vFileName := AVars.GetVariable('FileName');
  if AVars.IsReferenceDefined(vFileName) then
    SaveSelectionDialog.FileName:= AVars.GetString(vFileName);
  if pos(PathDelim,SaveSelectionDialog.FileName)<>0 then
  begin
    filename := SaveSelectionDialog.FileName;
    SaveSelectionDialog.FileName := ExtractFileName(filename);
    SaveSelectionDialog.InitialDir := ExtractFilePath(filename);
  end;
  if SaveSelectionDialog.Execute then
  begin
    filename := SaveSelectionDialog.FileName;
    if not Image.AbleToSaveSelectionAsUTF8(filename) then
    begin
      LazPaintInstance.ShowError(rsSave,rsFileExtensionNotSupported);
      result := srException;
    end else
    begin
      try
        Image.SaveSelectionToFileUTF8(filename);
        result := srOk;
        if Assigned(Scripting.RecordingFunctionParameters) then
           Scripting.RecordingFunctionParameters.AddString('FileName',filename);
      except
        on ex: Exception do
        begin
          LazPaintInstance.ShowError('FileSaveSelectionAs',ex.Message);
          result := srException;
        end;
      end;
    end;
  end else
  begin
    result := srCancelledByUser;
    SaveSelectionDialog.FileName := previousFilename;
  end;
end;

function TFMain.ScriptFilter(AVars: TVariableSet): TScriptResult;
var filter: TPictureFilter;
begin
  filter := StrToPictureFilter(AVars.GetString(AVars.GetVariable('Name')));
  if filter = pfNone then
     result := srInvalidParameters
  else
  begin
    if ExecuteFilter(LazPaintInstance, filter, AVars) then
      result := srOk
    else
      result := srException;
  end;
end;

function TFMain.ScriptColorCurves(AVars: TVariableSet): TScriptResult;
begin
  if Assigned(Scripting.RecordingFunctionParameters) then AVars := Scripting.RecordingFunctionParameters;
  if not image.CheckCurrentLayerVisible then
  begin result := srException; exit; end;
  if LazPaintInstance.ShowColorCurvesDlg(AVars) then
    result := srOk else result := srCancelledByUser;
end;

function TFMain.ScriptColorPosterize(AVars: TVariableSet): TScriptResult;
begin
  if Assigned(Scripting.RecordingFunctionParameters) then AVars := Scripting.RecordingFunctionParameters;
  if not image.CheckCurrentLayerVisible then
  begin result := srException; exit; end;
  if LazPaintInstance.ShowPosterizeDlg(AVars) then
    result := srOk else result := srCancelledByUser;
end;

procedure TFMain.FileSaveSelectionAsUpdate(Sender: TObject);
begin
  FileSaveSelectionAs.Enabled := not Image.SelectionEmpty;
end;

procedure TFMain.FormDropFiles(Sender: TObject; const FileNames: array of String);
var
  i,tx,ty: integer;
  Errors: String='';
  loadedLayers: array of record
     bmp: TBGRABitmap;
     filename: string;
  end;
  topmost: TTopMostInfo;
begin
  if Length(FileNames)<1 then exit;
  if Length(FileNames)= 1
     then TryOpenFileUTF8(FileNames[0])
  else
     begin
       //Button values higher than 10 are used, in order to avoid Delphis icons on the buttons.
       case QuestionDlg (rsOpenMultipleImageFiles, rsMoreThanOneFile, mtConfirmation, [mrLast+1,rsOpenFilesAsLayers,mrLast+2,rsAddToImageList,mrLast+3,rsOpenFirstFileOnly,mrCancel,rsCancel ],'') of
       mrLast+1: begin  //OpenFilesAsLayers
            if Image.IsFileModified then
               case LazPaintInstance.SaveQuestion(rsOpen) of
                IDYES: Scripting.CallScriptFunction('FileSave');
                IDCANCEL: exit;
              end; //case
              //When files are open this way, they get the name of the first of them.
              //They shall have no name, in order to prevent saving with Save (it then gets redirected to Save As)
              setlength(loadedLayers,length(FileNames));
              tx := 0;
              ty := 0;
              for i := 0 to high(FileNames) do
              begin
                try
                  MessagePopupForever(rsLoading + ' ' + inttostr(i+1) + '/' + inttostr(length(FileNames)));
                  LazPaintInstance.UpdateWindows;
                  loadedLayers[i].bmp := LoadFlatImageUTF8(Filenames[i], loadedLayers[i].filename, '');
                  if loadedLayers[i].bmp.Width > tx then tx := loadedLayers[i].bmp.Width;
                  if loadedLayers[i].bmp.Height > ty then ty := loadedLayers[i].bmp.Height;
                  MessagePopupHide;
                except on ex:exception do
                  //begin
                    Errors:= Errors + StringReplace(rsErrorOnOpeningFile, '%1', FileNames[i], [])+ ' ('+ ex.Message + ')'+ LineEnding;
                  //end;
                  end; //try except
              end; //for
              MessagePopupForever(rsOpening+'...');
              LazPaintInstance.UpdateWindows;
              try
                if (tx > 0) and (ty > 0) then
                begin
                  SetCurrentFilenameUTF8('');
                  Image.Assign(TBGRABitmap.Create(tx,ty),true,false);
                  ZoomFitIfTooBig;
                  for i := 0 to high(loadedLayers) do
                  begin
                    FImageActions.AddLayerFromBitmap(loadedLayers[i].bmp,ExtractFileName(loadedLayers[i].filename));
                    loadedLayers[i].bmp := nil;
                  end;
                end;
              except on ex:exception do
                begin
                  MessagePopupHide;
                  topmost := LazPaintInstance.HideTopmost;
                  LazPaintInstance.ShowError(rsOpenMultipleImageFiles,ex.Message);
                  LazPaintInstance.ShowTopmost(topmost);
                end;
              end;
              MessagePopupHide;
              if Length(Errors)>0 then
              begin
                topmost := LazPaintInstance.HideTopmost;
                QuestionDlg (rsError,rsFollowingErrorsOccured+ LineEnding+ Errors, mtError,[11,rsOkay],'');
                LazPaintInstance.ShowTopmost(topmost);
              end;
              for i := 0 to high(loadedLayers) do
                FreeAndNil(loadedLayers[i].bmp);
          end;  //OpenFilesAsLayers
       mrLast+2: begin
             if not LazPaintInstance.ImageListWindowVisible then
                LazPaintInstance.ImageListWindowVisible := true;
             LazPaintInstance.UpdateWindows;
             LazPaintInstance.AddToImageList(FileNames);
           end;
       mrLast+3: begin
           if Image.IsFileModified then
             case LazPaintInstance.SaveQuestion(rsOpen) of
                IDYES: Scripting.CallScriptFunction('FileSave');
                IDCANCEL: exit;
             end; //case
             TryOpenFileUTF8(FileNames[0]);
           end;
       end; //case
     end; //else
end;

procedure TFMain.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if ToolManager.ToolKeyUp(Key) then
  begin
    DelayedPaintPicture := True;
  end else
  If Key = VK_SPACE then
  begin
    spacePressed:= false;
    if ToolManager.ToolSleeping and not spacePressed and not btnRightDown and not btnLeftDown then
      ToolManager.ToolWakeUp;
    Key := 0;
  end;
end;

procedure TFMain.FormMouseEnter(Sender: TObject);
begin
  Image.PrepareForRendering;
end;

procedure TFMain.FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
var selectedTool: TPaintToolType;
  toolProcessKey: boolean;
begin
  try
    if Zoom.EditingZoom then exit;
    if UTF8Key = '+' then
    begin
       ViewZoomIn.Execute;
       UTF8Key := '';
    end else
    if UTF8Key = '-' then
    begin
       ViewZoomOut.Execute;
       UTF8Key := '';
    end else
    begin
      toolProcessKey:= true;
      if (CurrentTool = ptText) and ((UTF8Key = #8) or ((length(UTF8Key)=1) and (UTF8Key[1] in['0'..'9']))) then
      begin
        if TextSpinEditFocused then
           toolProcessKey:= false;
      end;
      if toolProcessKey and ToolManager.ToolKeyPress(UTF8Key) then
      begin
        DelayedPaintPicture := true;
      end else
      if UTF8Key <> '' then
      begin
        if UTF8UpperCase(UTF8Key) = 'X' then
        begin
          SwitchColors;
          UTF8Key:= '';
        end else
        begin
          selectedTool := ToolManager.GetCurrentToolType;
          FMenus.CycleTool(selectedTool, UTF8Key);
          if selectedTool <> ToolManager.GetCurrentToolType then ChooseTool(selectedTool);
        end;
      end;
    end;
  except
    on ex:exception do
      LazPaintInstance.ShowError('KeyPress',ex.Message);
  end;
end;

procedure TFMain.ImageCropLayerUpdate(Sender: TObject);
begin
  ImageCropLayer.Enabled := not image.SelectionEmpty;
  ImageCropLayer.Visible := (image.NbLayers > 1);
end;

procedure TFMain.ImageFlattenExecute(Sender: TObject);
begin
  if ToolManager.ToolSleeping then exit;
  ChooseTool(ptHand);
  image.Flatten;
end;

procedure TFMain.ImageFlattenUpdate(Sender: TObject);
begin
  ImageFlatten.Enabled := Image.NbLayers > 1;
end;

procedure TFMain.LayerAddNewUpdate(Sender: TObject);
begin
  LayerAddNew.Enabled := LazPaintInstance.Image.NbLayers < MaxLayersToAdd;
end;

procedure TFMain.LayerDuplicateUpdate(Sender: TObject);
begin
  LayerDuplicate.Enabled := LazPaintInstance.Image.NbLayers < MaxLayersToAdd;
end;

procedure TFMain.LayerFromFileExecute(Sender: TObject);
var i: integer;
    topmostInfo: TTopMostInfo;
    layerLoaded:boolean;
begin
  if not image.SelectionLayerIsEmpty then
  begin
    LazPaintInstance.ShowMessage(rsLazPaint,rsMustReleaseSelection);
    exit;
  end;
  topmostInfo := LazPaintInstance.HideTopmost;
  OpenPictureDialog1.Options := OpenPictureDialog1.Options + [ofAllowMultiSelect];
  layerLoaded := false;
  if OpenPictureDialog1.Execute then
  begin
    for i := 0 to OpenPictureDialog1.Files.Count-1 do
      begin
        if FImageActions.TryAddLayerFromFile(OpenPictureDialog1.Files[i]) then
          layerLoaded := true;
      end;
  end;
  LazPaintInstance.ShowTopmost(topmostInfo);
  if layerLoaded and not LazPaintInstance.LayerWindowVisible then
    LazPaintInstance.LayerWindowVisible := true;
end;

procedure TFMain.LayerMergeOverUpdate(Sender: TObject);
begin
  LayerMergeOver.Enabled := (image.currentImageLayerIndex > 0) and Image.CurrentLayerVisible;
end;

procedure TFMain.LayerMoveExecute(Sender: TObject);
begin
  ChooseTool(ptMoveLayer);
end;

procedure TFMain.LayerMoveUpdate(Sender: TObject);
begin
  LayerMove.Enabled := Image.CurrentLayerVisible and Image.SelectionEmpty;
end;

procedure TFMain.LayerRemoveCurrentUpdate(Sender: TObject);
begin
  LayerRemoveCurrent.Enabled := LazPaintInstance.Image.NbLayers > 1;
end;

procedure TFMain.LayerRotateExecute(Sender: TObject);
begin
  ChooseTool(ptRotateLayer);
end;

procedure TFMain.LayerRotateUpdate(Sender: TObject);
begin
  LayerRotate.Enabled := Image.CurrentLayerVisible and Image.SelectionEmpty;
end;

procedure TFMain.ItemDonateClick(Sender: TObject);
begin
  LazPaintInstance.Donate;
end;

procedure TFMain.ItemHorizFlipLayerClick(Sender: TObject);
begin
  CallScriptFunction('LayerHorizontalFlip');
end;

procedure TFMain.ItemHorizFlipPictureClick(Sender: TObject);
begin
  CallScriptFunction('ImageHorizontalFlip');
end;

procedure TFMain.ItemHorizFlipSelectionClick(Sender: TObject);
begin
  CallScriptFunction('SelectionHorizontalFlip');
end;

procedure TFMain.MenuImageClick(Sender: TObject);
begin
  ItemHorizFlipLayer.Visible := (image.NbLayers > 1) and not LazPaintInstance.LayerWindowVisible;
  ItemHorizFlipSelection.Visible := not image.SelectionEmpty;
  ImageHorizontalFlip.Visible := not ItemHorizFlipLayer.Visible and not ItemHorizFlipSelection.Visible;
  MenuHorizFlipSub.Visible := not ImageHorizontalFlip.Visible;
  ItemVertFlipLayer.Visible := (image.NbLayers > 1) and not LazPaintInstance.LayerWindowVisible;
  ItemVertFlipSelection.Visible := not image.SelectionEmpty;
  ImageVerticalFlip.Visible := not ItemVertFlipLayer.Visible and not ItemVertFlipSelection.Visible;
  MenuVertFlipSub.Visible := not ImageVerticalFlip.Visible;
end;

procedure TFMain.ItemVertFlipLayerClick(Sender: TObject);
begin
  CallScriptFunction('LayerVerticalFlip');
end;

procedure TFMain.ItemVertFlipPictureClick(Sender: TObject);
begin
  CallScriptFunction('ImageVerticalFlip');
end;

procedure TFMain.ItemVertFlipSelectionClick(Sender: TObject);
begin
  CallScriptFunction('SelectionVerticalFlip');
end;

procedure TFMain.PaintBox_PictureMouseEnter(Sender: TObject);
begin
  FormMouseEnter(Sender);
end;

procedure TFMain.Perspective_RepeatClick(Sender: TObject);
begin
  if initialized then
  begin
    ToolManager.ToolPerspectiveRepeat := Perspective_Repeat.Down;
    UpdateEditPicture;
  end;
end;

procedure TFMain.Perspective_TwoPlanesClick(Sender: TObject);
begin
  if initialized then
  begin
    ToolManager.ToolPerspectiveTwoPlanes := Perspective_TwoPlanes.Down;
    UpdateEditPicture;
  end;
end;

procedure TFMain.SpinEdit_ShapeAltitudeChange(Sender: TObject);
begin
  if SpinEdit_ShapeAltitude.Value < 6 then
    SpinEdit_ShapeAltitude.Increment := 1
  else if SpinEdit_ShapeAltitude.Value < 25 then
    SpinEdit_ShapeAltitude.Increment := 3
  else
    SpinEdit_ShapeAltitude.Increment := 5;
  if initialized then
  begin
    if ToolManager.ToolShapeAltitude = SpinEdit_ShapeAltitude.Value then exit;
    ToolManager.ToolShapeAltitude := SpinEdit_ShapeAltitude.Value;
    UpdateEditPicture;
  end;
end;

procedure TFMain.SpinEdit_TextSizeChange(Sender: TObject);
begin
  if initialized and not FInTextFont then
  begin
    if ToolManager.ToolTextFont.Size = SpinEdit_TextSize.Value then exit;
    ToolManager.ToolTextFont.Size := SpinEdit_TextSize.Value;
    UpdateEditPicture(True);
  end;
end;

procedure TFMain.SpinEdit_TextureOpacityChange(Sender: TObject);
begin
  if initialized then
  begin
    if ToolManager.ToolTextureOpacity = SpinEdit_TextureOpacity.Value then exit;
    ToolManager.ToolTextureOpacity := SpinEdit_TextureOpacity.Value;
    UpdateEditPicture(True);
  end;
end;

procedure TFMain.SpinEdit_TextBlurChange(Sender: TObject);
begin
  if initialized then
  begin
    if ToolManager.ToolTextBlur = SpinEdit_TextBlur.Value then exit;
    ToolManager.ToolTextBlur := SpinEdit_TextBlur.Value;
    UpdateEditPicture(True);
  end;
end;

procedure TFMain.GridNb_SpinEditChange(Sender: TObject);
begin
  if not initialized then exit;
  if ToolManager.SetToolDeformationGridSize(SpinEdit_GridNbX.Value+1,SpinEdit_GridNbY.Value+1) then
    image.OnImageChanged.NotifyObservers;
end;

procedure TFMain.Image_CurrentTextureMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  LazPaintInstance.EditTexture;
  UpdateTextureIcon;
  UpdateEditPicture;
end;

procedure TFMain.PaintBox_PenPreviewPaint(Sender: TObject);
var bmp: TBGRABitmap;
    x,t: integer;
    m: single;
begin
  bmp := TBGRABitmap.Create(PaintBox_PenPreview.Width,PaintBox_PenPreview.Height,ColorToBGRA(ColorToRGB(clBtnFace)));
  bmp.FillEllipseAntialias(round(PaintBox_PenPreview.Width/2),round(PaintBox_PenPreview.Height/2),
    ToolManager.ToolPenWidth/2*ZoomFactor,ToolManager.ToolPenWidth/2*ZoomFactor,ColorToBGRA(ColorToRGB(clBtnText)));
  t := ImageList1.Width;
  m := t/10;
  x := 0;
  bmp.EllipseAntialias(x+(t-1)/2,(t-1)/2,t/2-m,t/2-m,BGRABlack,m,BGRAWhite);
  bmp.DrawLineAntialias(x+m*2.6,(t-1)/2,x+t-1-m*2.6,(t-1)/2,BGRABlack,m);
  x := bmp.Width-t;
  bmp.EllipseAntialias(x+(t-1)/2,(t-1)/2,t/2-m,t/2-m,BGRABlack,m,BGRAWhite);
  bmp.DrawLineAntialias(x+(t-1)/2,m*2.6,x+(t-1)/2,t-1-m*2.6,BGRABlack,m);
  bmp.DrawLineAntialias(x+m*2.6,(t-1)/2,x+t-1-m*2.6,(t-1)/2,BGRABlack,m);

  bmp.Draw(PaintBox_PenPreview.Canvas,0,0);
  bmp.Free;
end;

procedure TFMain.PaintBox_PictureMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FormMouseDown(Sender,Button,Shift,X,Y+PaintBox_Picture.Top);
end;

procedure TFMain.PaintBox_PictureMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  FormMouseMove(Sender,Shift,X,Y+PaintBox_Picture.Top);
end;

procedure TFMain.PaintBox_PictureMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FormMouseUp(Sender,Button,Shift,X,Y+PaintBox_Picture.Top);
end;

procedure TFMain.PaintBox_PictureMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  FormMouseWheel(Sender,Shift,WheelDelta,Point(MousePos.X,MousePos.Y+PaintBox_Picture.Top),Handled);
end;

procedure TFMain.PaintBox_PicturePaint(Sender: TObject);
begin
  {$IFDEF USEPAINTBOXPICTURE}
    OnPaintHandler;
  {$ENDIF}
end;

procedure TFMain.Panel_PenWidthMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  ShowPenPreview;
end;

procedure TFMain.Panel_ToolbarBackgroundMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  HidePenPreview();
end;

procedure TFMain.SpinEdit_PenWidthMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  ShowPenPreview(False);
end;

procedure TFMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var topmostInfo: TTopMostInfo;
begin
  if ToolManager.ToolSleeping then
  begin
    CanClose := false;
    exit;
  end;
  if CurrentTool in [ptDeformation,ptLayerMapping,ptTextureMapping] then
    ChooseTool(ptHand)
  else
    if not Image.CheckNoAction then
    begin
      CanClose := false;
      exit;
    end;
  if not LazPaintInstance.Embedded and image.IsFileModified and not image.Empty then
  begin
    topmostInfo:= LazPaintInstance.HideTopmost;

    case LazPaintInstance.SaveQuestion(rsExitRequest) of
    IDYES: FileSave.Execute;
    IDNO: ;
    IDCANCEL: begin
                CanClose := false;
                LazPaintInstance.RestartQuery := false;
                LazPaintInstance.ShowTopmost(topmostInfo);
              end;
    end;
  end else
  if LazPaintInstance.Embedded and (LazPaintInstance.EmbeddedResult = mrNone) and
    ((LazPaintInstance.EmbeddedImageBackup <> nil) or not image.Empty) and (not image.FlatImageEquals(LazPaintInstance.EmbeddedImageBackup)) then
  begin
    topmostInfo := LazPaintInstance.HideTopmost;
    case MessageDlg(rsCloseRequest,rsKeepChanges,mtConfirmation,mbYesNoCancel,0) of
    IDYES: LazPaintInstance.EmbeddedResult := mrOk;
    IDNO: LazPaintInstance.EmbeddedResult := mrCancel;
    IDCANCEL: begin
                CanClose := false;
                LazPaintInstance.RestartQuery := false;
                LazPaintInstance.ShowTopmost(topmostInfo);
              end;
    end;
  end;
end;

procedure TFMain.FormHide(Sender: TObject);
var r: TRect;
begin
  shouldArrangeOnResize := false;
  FTopMostInfo := LazPaintInstance.HideTopmost;
  if WindowState = wsMaximized then
    Config.SetDefaultMainWindowMaximized(true) else
    begin
      r.left := Left;
      r.top := Top;
      r.right := r.left+ClientWidth;
      r.Bottom := r.top+ClientHeight;
      Config.SetDefaultMainWindowPosition(r);
    end;
  Image.OnImageChanged.RemoveObserver(@OnImageChangedHandler);
  Image.OnQueryExitToolHandler := nil;
  Image.Zoom := nil;
end;

procedure TFMain.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  Zoom.SetPosition(FormToBitmap(MousePos.X,MousePos.Y), MousePos);
  if WheelDelta > 0 then Zoom.ZoomIn else
  if WheelDelta < 0 then Zoom.ZoomOut;
  Zoom.ClearPosition;
  Handled := True;
end;

procedure TFMain.HelpAboutExecute(Sender: TObject);
begin
  LazPaintInstance.ShowAboutDlg;
end;

procedure TFMain.HelpIndexExecute(Sender: TObject);
begin
  OpenURL('http://wiki.lazarus.freepascal.org/LazPaint');
end;

procedure TFMain.ImageChangeCanvasSizeExecute(Sender: TObject);
begin
  LazPaintInstance.ShowCanvasSizeDlg;
end;

procedure TFMain.ImageCropUpdate(Sender: TObject);
begin
  ImageCrop.Enabled := not image.SelectionEmpty;
end;

procedure TFMain.ImageRepeatExecute(Sender: TObject);
begin
  LazPaintInstance.ShowRepeatImageDlg;
end;

procedure TFMain.MenuFileClick(Sender: TObject);

  procedure EmptyMenu(AMenu: TMenuItem);
  var
    i: integer;
    item: TMenuItem;
  begin
    for i := AMenu.Count-1 downto 0 do
    begin
      item := AMenu.Items[i];
      AMenu.Delete(i);
      item.Free;
    end;
  end;

var
  i: integer;
  item: TMenuItem;
  currentLanguage: string;

begin
     EmptyMenu(MenuRecentFiles);
     for i := 0 to Config.RecentFilesCount-1 do
     if Config.RecentFile[i]<>GetCurrentFilenameUTF8 then
     begin
       item := NewItem(Config.RecentFile[i],0,false,true,@RecentFileClick,0,'');
       MenuRecentFiles.Add(item);
     end;
     MenuRecentFiles.Enabled := MenuRecentFiles.Count <> 0;

     EmptyMenu(ItemLanguage);
     currentLanguage := Config.DefaultLangage;
     for i := 0 to Config.Languages.Count-1 do
     begin
       item := NewItem(Config.Languages[i],0,false,true,@LanguageClick,0,'');
       if currentLanguage = item.Caption then
         item.Checked := true;
       ItemLanguage.Add(item);
     end;
     ItemLanguage.Enabled := ItemLanguage.Count <> 0;
end;

procedure TFMain.RecentFileClick(Sender: TObject);
var openParams: TVariableSet;
begin
  if Sender is TMenuItem then
  begin
    openParams := TVariableSet.Create('FileOpen');
    openParams.AddString('FileName',(sender as TMenuItem).Caption);
    Scripting.CallScriptFunction(openParams);
    openParams.Free;
  end;
end;

procedure TFMain.LanguageClick(Sender: TObject);
var language: string;
begin
  if Sender is TMenuItem then
  begin
    language := (Sender as TMenuItem).Caption;
    Config.SetDefaultLangage(language);
    LazPaintInstance.RestartQuery := True;
    Close;
  end;
end;

procedure TFMain.SpinEdit_TextOutlineWidthChange(Sender: TObject);
begin
  if initialized then
  begin
     if ToolManager.ToolTextOutlineWidth <> SpinEdit_TextOutlineWidth.Value then
     begin
       ToolManager.ToolTextOutlineWidth:= SpinEdit_TextOutlineWidth.Value;
       UpdateEditPicture;
     end;
  end;
end;

procedure TFMain.SpinEdit_TextShadowXChange(Sender: TObject);
begin
  if initialized then
  begin
    if ToolManager.ToolTextShadowOffset.X = SpinEdit_TextShadowX.Value then exit;
    ToolManager.ToolTextShadowOffset.X := SpinEdit_TextShadowX.Value;
    UpdateEditPicture(True);
  end;
end;

procedure TFMain.SpinEdit_TextShadowYChange(Sender: TObject);
begin
  if initialized then
  begin
    if ToolManager.ToolTextShadowOffset.Y = SpinEdit_TextShadowY.Value then exit;
    ToolManager.ToolTextShadowOffset.Y := SpinEdit_TextShadowY.Value;
    UpdateEditPicture(True);
  end;
end;

procedure TFMain.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := false;
  EditUndo.Update;
  EditRedo.Update;
  if FCoordinatesCaptionCount > 0 then
  begin
    Label_Coordinates.Caption := FCoordinatesCaption;
    Label_Coordinates.Update;
    FCoordinatesCaptionCount := 0;
  end;
  if CanCompressOrUpdateStack and StackNeedUpdate then
  begin
    LazPaintInstance.NotifyStackChange;
    StackNeedUpdate := false;
  end else
  begin
    if CanCompressOrUpdateStack then image.CompressUndo;
  end;
  if DelayedPaintPicture or ToolManager.ToolUpdateNeeded then
  begin
    if ToolManager.ToolUpdateNeeded then ToolManager.ToolUpdate;
    PaintPictureNow;
  end;
  Timer1.Enabled := true;
end;

procedure TFMain.TimerHidePenPreviewTimer(Sender: TObject);
begin
  Panel_PenWidthPreview.Visible := False;
  TimerHidePenPreview.Enabled := false;
end;

procedure TFMain.ToolButton_ViewLayerStackClick(Sender: TObject);
begin
  ToggleLayersVisible;
end;

procedure TFMain.ToolRotateSelectionUpdate(Sender: TObject);
begin
  ToolRotateSelection.Enabled := not image.SelectionEmpty;
end;

procedure TFMain.Tool_CurveModeAngleClick(Sender: TObject);
begin
  SetCurveMode(tsmCurveModeAngle);
end;

procedure TFMain.Tool_CurveModeAutoClick(Sender: TObject);
begin
  SetCurveMode(tsmCurveModeAuto);
end;

procedure TFMain.Tool_CurveModeCurveClick(Sender: TObject);
begin
  SetCurveMode(tsmCurveModeSpline);
end;

procedure TFMain.Tool_CurveMovePointClick(Sender: TObject);
begin
  SetCurveMode(tsmMovePoint);
end;

procedure TFMain.Tool_EraseOptionClick(Sender: TObject);
begin
  if Tool_EraseAlpha.Down then ToolManager.ToolEraserMode:= emEraseAlpha else
  if Tool_EraseBlur.Down then ToolManager.ToolEraserMode:= emSoften;
end;

procedure TFMain.Tool_PhongShapeHorizontalCylinderClick(Sender: TObject);
begin
  if Tool_PhongShapeHorizontalCylinder.Down then
  begin
    if initialized then ToolManager.ToolShapeType := 'HorizontalCylinder';
    UpdatePanelPhongShape;
  end
end;

procedure TFMain.Tool_PhongShapeVerticalConeClick(Sender: TObject);
begin
  if Tool_PhongShapeVerticalCone.Down then
  begin
    if initialized then ToolManager.ToolShapeType := 'VerticalCone';
    UpdatePanelPhongShape;
  end
end;

procedure TFMain.Tool_PhongShapeVerticalCylinderClick(Sender: TObject);
begin
  if Tool_PhongShapeVerticalCylinder.Down then
  begin
    if initialized then ToolManager.ToolShapeType := 'VerticalCylinder';
    UpdatePanelPhongShape;
  end
end;

procedure TFMain.ToolLayerMappingUpdate(Sender: TObject);
begin
  ToolLayerMapping.Enabled := Image.CurrentLayerVisible and Image.SelectionEmpty;
end;

procedure TFMain.Tool_TextAlignClick(Sender: TObject);
var newAlign: TAlignment;
begin
  if initialized then
  begin
    newAlign := taLeftJustify;
    if Tool_TextAlignCenter.Down then newAlign:= taCenter;
    if Tool_TextAlignRight.Down then newAlign := taRightJustify;
    if newAlign <> ToolManager.ToolTextAlign then
    begin
      ToolManager.ToolTextAlign := newAlign;
      UpdateEditPicture;
    end;
  end;
end;

procedure TFMain.Tool_TextPhongClick(Sender: TObject);
begin
  if initialized then
  begin
     if ToolManager.ToolTextPhong <> Tool_TextPhong.Down then
     begin
       ToolManager.ToolTextPhong:= Tool_TextPhong.Down;
       Panel_Altitude.Visible := ToolManager.ToolTextPhong;
       ArrangeToolbars;
       UpdateToolbar;
       UpdateEditPicture;
     end;
  end;
end;

procedure TFMain.Tool_GridMoveWithoutDeformationClick(Sender: TObject);
begin
    if initialized then
      ToolManager.ToolDeformationGridMoveWithoutDeformation :=
        Tool_GridMoveWithoutDeformation.Down;
end;

procedure TFMain.Tool_PhongShapeConeClick(Sender: TObject);
begin
    if Tool_PhongShapeCone.Down then
    begin
      if initialized then ToolManager.ToolShapeType := 'Cone';
      UpdatePanelPhongShape;
    end;
end;

procedure TFMain.Tool_PhongShapeRectangleClick(Sender: TObject);
begin
    if Tool_PhongShapeRectangle.Down then
    begin
      if initialized then ToolManager.ToolShapeType := 'Rectangle';
      UpdatePanelPhongShape;
    end;
end;

procedure TFMain.Tool_PhongShapeRoundRectClick(Sender: TObject);
begin
    if Tool_PhongShapeRoundRect.Down then
    begin
      if initialized then ToolManager.ToolShapeType := 'RoundRectangle';
      UpdatePanelPhongShape;
    end;
end;

procedure TFMain.Tool_PhongShapeSphereClick(Sender: TObject);
begin
    if Tool_PhongShapeSphere.Down then
    begin
      if initialized then ToolManager.ToolShapeType := 'Sphere';
      UpdatePanelPhongShape;
    end;
end;

procedure TFMain.Tool_SinGradientClick(Sender: TObject);
begin
  if initialized then
  begin
    ToolManager.ToolGradientSine := Tool_SinGradient.Down;
    UpdateEditPicture;
  end;
end;

procedure TFMain.ToolLoadTextureExecute(Sender: TObject);
begin
  ShowOpenTextureDialog;
end;

procedure TFMain.ToolNoTextureExecute(Sender: TObject);
begin
  try
    ToolManager.SetToolTexture(nil);

    UpdateTextureIcon;
    UpdateEditPicture;

  except
    on ex:Exception do
      LazPaintInstance.ShowError('ToolNoTextureExecute',ex.Message);
  end;
end;

procedure TFMain.ToolNoTextureUpdate(Sender: TObject);
begin
  ToolNoTexture.Enabled := ToolManager.GetToolTexture <> nil;
end;

procedure TFMain.Tool_CapFlatClick(Sender: TObject);
begin
  if Tool_CapFlat.Down then
  begin
    ToolManager.ToolLineCap := pecFlat;
    UpdateEditPicture;
  end;
end;

procedure TFMain.Tool_CapRoundClick(Sender: TObject);
begin
  if Tool_CapRound.Down then
  begin
    ToolManager.ToolLineCap := pecRound;
    UpdateEditPicture;
  end;
end;

procedure TFMain.Tool_CapSquareClick(Sender: TObject);
begin
  if Tool_CapSquare.Down then
  begin
    ToolManager.ToolLineCap := pecSquare;
    UpdateEditPicture;
  end;
end;

procedure TFMain.Tool_JoinBevelClick(Sender: TObject);
begin
  if Tool_JoinBevel.Down then
  begin
    ToolManager.ToolJoinStyle := pjsBevel;
    UpdateEditPicture;
  end;
end;

procedure TFMain.Tool_JoinRoundClick(Sender: TObject);
begin
  if Tool_JoinRound.Down then
  begin
    ToolManager.ToolJoinStyle := pjsRound;
    UpdateEditPicture;
  end;
end;

procedure TFMain.Tool_JoinMiterClick(Sender: TObject);
begin
  if Tool_JoinMiter.Down then
  begin
    ToolManager.ToolJoinStyle := pjsMiter;
    UpdateEditPicture;
  end;
end;

procedure TFMain.Tool_PenDashClick(Sender: TObject);
begin
  if Tool_PenDash.Down then
  begin
    ToolManager.ToolPenStyle := psDash;
    UpdateEditPicture;
  end;
end;

procedure TFMain.Tool_PenDashDotClick(Sender: TObject);
begin
  if Tool_PenDashDot.Down then
  begin
    ToolManager.ToolPenStyle := psDashDot;
    UpdateEditPicture;
  end;
end;

procedure TFMain.Tool_PenDashDotDotClick(Sender: TObject);
begin
  if Tool_PenDashDotDot.Down then
  begin
    ToolManager.ToolPenStyle := psDashDotDot;
    UpdateEditPicture;
  end;
end;

procedure TFMain.Tool_PenDotClick(Sender: TObject);
begin
  if Tool_PenDot.Down then
  begin
    ToolManager.ToolPenStyle := psDot;
    UpdateEditPicture;
  end;
end;

procedure TFMain.Tool_PenSolidClick(Sender: TObject);
begin
  if Tool_PenSolid.Down then
  begin
    ToolManager.ToolPenStyle := psSolid;
    UpdateEditPicture;
  end;
end;

procedure TFMain.Tool_TextFontClick(Sender: TObject);
var topmostInfo: TTopMostInfo;
begin
  FInTextFont := true;
  FontDialog1.Font.Assign(ToolManager.ToolTextFont);
  FontDialog1.Font.Color := BGRAToColor(ToolManager.ToolForeColor);
  topmostInfo := LazPaintInstance.HideTopmost;
  if FontDialog1.Execute then
  begin
    ToolManager.ToolTextFont.Assign(FontDialog1.Font);
    SpinEdit_TextSize.Value := FontDialog1.Font.Size;
    if FontDialog1.Font.Color <> BGRAToColor(ToolManager.ToolForeColor) then
    begin
      ToolManager.ToolForeColor := ColorToBGRA(FontDialog1.Font.Color,ToolManager.ToolForeColor.alpha);
      Shape_PenColor.Brush.Color := BGRAToColor(ToolManager.ToolForeColor);
      LazPaintInstance.ColorToFChooseColor;
    end;
    UpdateEditPicture;
  end;
  LazPaintInstance.ShowTopmost(topmostInfo);
  FInTextFont := false;
end;

procedure TFMain.Tool_TextOutlineClick(Sender: TObject);
begin
  if initialized then
  begin
     if ToolManager.ToolTextOutline <> Tool_TextOutline.Down then
     begin
       ToolManager.ToolTextOutline:= Tool_TextOutline.Down;
       UpdateEditPicture;
     end;
  end;
end;

procedure TFMain.Tool_TextShadowClick(Sender: TObject);
begin
  if initialized then
  begin
     if ToolManager.ToolTextShadow <> Tool_TextShadow.Down then
     begin
       ToolManager.ToolTextShadow:= Tool_TextShadow.Down;

       UpdatePanelTextWidth;
       ArrangeToolbars;
       UpdateToolbar;

       UpdateEditPicture;
     end;
  end;
end;

procedure TFMain.ViewColorsExecute(Sender: TObject);
begin
  ToggleColorsVisible;
end;

procedure TFMain.ViewColorsUpdate(Sender: TObject);
begin
  ViewColors.Checked := LazPaintInstance.ChooseColorVisible;
  ItemViewColors.Checked := ViewColors.Checked;
end;

procedure TFMain.ViewGridUpdate(Sender: TObject);
begin
  ViewGrid.Checked:= LazPaintInstance.GridVisible;
end;

procedure TFMain.ViewImageListExecute(Sender: TObject);
begin
  ToggleImageListVisible;
end;

procedure TFMain.ViewToolboxExecute(Sender: TObject);
begin
  ToggleToolboxVisible;
end;

procedure TFMain.vsTextureOpacityClick(Sender: TObject);
begin

end;

procedure TFMain.SpinEdit_PenWidthExit(Sender: TObject);
begin
  if SpinEdit_PenWidth.Value < MinPenWidthValue then SpinEdit_PenWidth.Value := MinPenWidthValue;
end;

procedure TFMain.SpinEdit_GridNbExit(Sender: TObject);
begin
  if SpinEdit_GridNbX.Value < 2 then SpinEdit_GridNbX.Value := 2;
  if SpinEdit_GridNbY.Value < 2 then SpinEdit_GridNbY.Value := 2;
end;

procedure TFMain.ViewLayerStackButtonUpdate(Sender: TObject);
begin
  ViewLayerStackButton.Checked := LazPaintInstance.LayerWindowVisible;
end;

procedure TFMain.ViewLayerStackExecute(Sender: TObject);
begin
  ToggleLayersVisible;
end;

procedure TFMain.ViewLayerStackUpdate(Sender: TObject);
begin
  ViewLayerStack.Checked := LazPaintInstance.LayerWindowVisible;
end;

procedure TFMain.ViewToolboxUpdate(Sender: TObject);
begin
  ViewToolBox.Checked := LazPaintInstance.ToolboxVisible;
end;

procedure TFMain.ViewImagelistUpdate(Sender: TObject);
begin
  ViewImageList.Checked := LazPaintInstance.ImageListWindowVisible;
end;

function TFMain.ScriptEditPasteAsNew(AVars: TVariableSet): TScriptResult;
var bmp: TBGRABitmap;
begin
  if ToolManager.ToolSleeping then
  begin
    result := srException;
    exit;
  end;
  bmp := GetBitmapFromClipboard;
  if bmp <> nil then
  begin
    if bmp.NbPixels > 0 then
    begin
      ChooseTool(ptHand);
      if Image.IsFileModified then
      begin
        case LazPaintInstance.SaveQuestion(rsNewImage) of
        IDYES: scripting.CallScriptFunction('FileSave');
        IDCANCEL: begin
            bmp.Free;
            result := srCancelledByUser;
            exit;
          end;
        end;
      end;
      image.Assign(bmp,true,false);
      SetCurrentFilenameUTF8('');
      image.SetSavedFlag;
      result := srOk;
    end
     else
     begin
       bmp.Free;
       result := srException;
     end;
  end else
    result := srException;
end;

function TFMain.ChooseTool(Tool: TPaintToolType): boolean;
var params: TVariableSet;
begin
  params := TVariableSet.Create('ChooseTool');
  params.AddString('Name', PaintToolTypeStr[Tool]);
  result := Scripting.CallScriptFunction(params) = srOk;
  params.Free;
end;

procedure TFMain.ArrangeToolbars;
var previousHeight: integer;
begin
   previousHeight := FMenus.ToolbarsHeight;
   FMenus.ArrangeToolbars(ClientWidth);
   if previousHeight <> FMenus.ToolbarsHeight then
   begin
     {$IFDEF USEPAINTBOXPICTURE}
     PaintBox_Picture.Top := FMenus.ToolbarsHeight;
     PaintBox_Picture.Height := ClientHeight-FMenus.ToolbarsHeight;
     {$ENDIF}
     InvalidatePicture(True);
   end;
   FMenus.RepaintToolbar;
end;

procedure TFMain.UpdateToolImage;
var img: integer; ToolBmp, IconBmp: TBitmap; IconBGRA, Blur, ToolBGRA: TBGRABitmap;
begin
  Case CurrentTool of
  ptHand: img := ToolHand.ImageIndex;
  ptPen: img := ToolPen.ImageIndex;
  ptColorPicker: img := ToolColorPicker.ImageIndex;
  ptEraser: img := ToolEraser.ImageIndex;
  ptRect: img := ToolRect.ImageIndex;
  ptEllipse: img := ToolEllipse.ImageIndex;
  ptPolygon: img := ToolPolygon.ImageIndex;
  ptSpline: img := ToolSpline.ImageIndex;
  ptFloodFill: img := ToolFloodfill.ImageIndex;
  ptGradient: img := ToolGradient.ImageIndex;
  ptSelectRect: img := ToolSelectRect.ImageIndex;
  ptSelectEllipse: img := ToolSelectEllipse.ImageIndex;
  ptSelectPoly: img := ToolSelectPoly.ImageIndex;
  ptSelectSpline: img := ToolSelectSpline.ImageIndex;
  ptSelectPen: img := ToolSelectPen.ImageIndex;
  ptMoveSelection: img := ToolMoveSelection.ImageIndex;
  ptRotateSelection: img := ToolRotateSelection.ImageIndex;
  ptMagicWand: img := ToolMagicWand.ImageIndex;
  ptText: img := ToolText.ImageIndex;
  ptDeformation: img := ToolDeformation.ImageIndex;
  ptTextureMapping: img := ToolTextureMapping.ImageIndex;
  ptLayerMapping: img := ToolLayerMapping.ImageIndex;
  ptPhong: img := ToolPhong.ImageIndex;
  ptMoveLayer: img := LayerMove.ImageIndex;
  else img := -1;
  end;

  if img <> previousToolImg then
  begin
    if img <> -1 then
    begin
      IconBGRA := TBGRABitmap.Create(Image_CurrentTool.Width,Image_CurrentTool.Height);

      ToolBmp := TBitmap.Create;
      ImageList1.GetBitmap(img,ToolBmp);
      ToolBGRA := TBGRABitmap.Create(ToolBmp);
      FreeAndNil(ToolBmp);

      IconBGRA.PutImage((IconBGRA.Width-ToolBGRA.Width) div 2, (IconBGRA.Height-ToolBGRA.Height) div 2,ToolBGRA,dmSet);
      Blur := IconBGRA.FilterBlurRadial(3,rbNormal) as TBGRABitmap;
      IconBGRA.PutImage(0,0,Blur,dmSet);
      FreeAndNil(Blur);
      IconBGRA.PutImage((IconBGRA.Width-ToolBGRA.Width) div 2, (IconBGRA.Height-ToolBGRA.Height) div 2,ToolBGRA,dmDrawWithTransparency);
      FreeAndNil(ToolBGRA);

      IconBmp := IconBGRA.MakeBitmapCopy(ColorToRGB(clBtnFace));
      FreeAndNil(IconBGRA);
      Image_CurrentTool.Picture.Assign(IconBmp);
      IconBmp.Free;
    end else
      Image_CurrentTool.Picture.Clear;
    previousToolImg := img;
  end;
  Panel_Texture.Enabled := (CurrentTool <> ptTextureMapping);
end;

procedure TFMain.NoTextureIcon;
var lIcon: TBitmap;
begin
  Image_CurrentTexture.Picture.Clear;
  lIcon := TBitmap.Create;
  ImageList1.GetBitmap(56,lIcon);
  Image_CurrentTexture.Picture.Assign(lIcon);
  lIcon.Free;
end;

procedure TFMain.ToggleGridVisible;
begin
  LazPaintInstance.GridVisible := not LazPaintInstance.GridVisible;
end;

procedure TFMain.ToggleToolboxVisible;
begin
  LazPaintInstance.ToolboxVisible := not LazPaintInstance.ToolboxVisible;
end;

procedure TFMain.ToggleImageListVisible;
begin
  LazPaintInstance.ImageListWindowVisible := not LazPaintInstance.ImageListWindowVisible;
end;

procedure TFMain.ToggleColorsVisible;
begin
  LazPaintInstance.ChooseColorVisible := not LazPaintInstance.ChooseColorVisible;
end;

procedure TFMain.ToggleLayersVisible;
begin
  LazPaintInstance.LayerWindowVisible := not LazPaintInstance.LayerWindowVisible;
end;

procedure TFMain.ToggleToolwindowsVisible;
var wantedState: boolean;
begin
  if LazPaintInstance.ToolboxVisible or LazPaintInstance.ChooseColorVisible or LazPaintInstance.LayerWindowVisible then
    wantedState := false
  else
    wantedState := true;
  if LazPaintInstance.ToolboxVisible <> wantedState then ToggleToolboxVisible;
  if LazPaintInstance.ChooseColorVisible <> wantedState then ToggleColorsVisible;
  if LazPaintInstance.LayerWindowVisible <> wantedState then ToggleLayersVisible;
end;

function TFMain.ScriptColorIntensity(AVars: TVariableSet): TScriptResult;
begin
  if Assigned(Scripting.RecordingFunctionParameters) then AVars := Scripting.RecordingFunctionParameters;
  if not image.CheckCurrentLayerVisible then
  begin result := srException; exit; end;
  if LazPaintInstance.ShowColorIntensityDlg(AVars) then
    result := srOk else result := srCancelledByUser;
end;

function TFMain.ScriptColorShiftColors(AVars: TVariableSet): TScriptResult;
begin
  if Assigned(Scripting.RecordingFunctionParameters) then AVars := Scripting.RecordingFunctionParameters;
  if not image.CheckCurrentLayerVisible then
  begin result := srException; exit; end;
  if LazPaintInstance.ShowShiftColorsDlg(AVars) then
    result := srOk else result := srCancelledByUser;
end;

function TFMain.ScriptColorColorize(AVars: TVariableSet): TScriptResult;
begin
  if Assigned(Scripting.RecordingFunctionParameters) then AVars := Scripting.RecordingFunctionParameters;
  if not image.CheckCurrentLayerVisible then
  begin result := srException; exit; end;
  if LazPaintInstance.ShowColorizeDlg(AVars) then
    result := srOk else result := srCancelledByUser;
end;

function TFMain.ScriptColorLightness(AVars: TVariableSet): TScriptResult;
begin
  if Assigned(Scripting.RecordingFunctionParameters) then AVars := Scripting.RecordingFunctionParameters;
  if not image.CheckCurrentLayerVisible then
  begin result := srException; exit; end;
  if LazPaintInstance.ShowColorLightnessDlg(AVars) then
    result := srOk else result := srCancelledByUser;
end;

function TFMain.ScriptChooseTool(AVars: TVariableSet): TScriptResult;
var toolName: string;
  Tool: TPaintToolType;
  LayerAction: TLayerAction;
  topmostInfo: TTopMostInfo;
  res: integer;
begin
  if ToolManager.ToolSleeping then exit;
  toolName := AVars.Strings['Name'];
  Tool := StrToPaintToolType(toolName);
  if CompareText(PaintToolTypeStr[Tool],toolName)=0 then
  begin
    result := srOk;
    if Tool = ToolManager.GetCurrentToolType then
    begin
      ToolManager.ToolOpen;
      exit;
    end;
    try
      ToolManager.ToolCloseDontReopen;
      if self.Visible then
      begin
        case Tool of
          ptTextureMapping:
            if (ToolManager.GetToolTexture = nil) or ToolManager.GetToolTexture.Empty then
            begin
              if not ShowOpenTextureDialog then
              begin
                Tool := ptHand;
                result := srCancelledByUser;
              end
              else
              if (ToolManager.GetToolTexture = nil) or ToolManager.GetToolTexture.Empty then
              begin
                Tool := ptHand;
                result := srException;
              end;
            end;
          ptLayerMapping:
          begin
            EditDeselect.Execute;
            if image.SelectedLayerEmpty then
            begin
              MessagePopup(rsEmptyLayer,2000);
              Tool := ptHand;
              result := srException;
            end;
          end;
          ptMoveLayer:
          begin
            if image.SelectedLayerEquals(image.SelectedLayerPixel[0,0]) then
            begin
              LazPaintInstance.ShowMessage(rsLazPaint, rsEmptyLayer);
              Tool := ptHand;
              result := srException;
            end;
          end;
          ptDeformation:
          begin
            if (image.SelectionEmpty and image.SelectedLayerEquals(image.SelectedLayerPixel[0,0])) or
               (not image.SelectionEmpty and image.SelectionLayerIsEmpty) then
            begin
              LazPaintInstance.ShowMessage(rsLazPaint, rsNothingToBeDeformed);
              Tool := ptHand;
              result := srException;
            end;
          end;
          ptMoveSelection,ptRotateSelection:
          begin
            if not ToolManager.SetCurrentToolType(Tool) then
            begin
              result := srException;
              exit;
            end;
            if image.CurrentLayerVisible and not image.SelectionEmpty and image.SelectionLayerIsEmpty and not image.SelectedLayerEmpty then
            begin
              topmostInfo := LazPaintInstance.HideTopmost;
              res := MessageDlg(rsMovingOrRotatingSelection,rsRetrieveSelectedArea,mtConfirmation,[mbYes,mbNo],0);
              LazPaintInstance.ShowTopmost(topmostInfo);
              case res of
                mrYes: begin
                  LayerAction := nil;
                  try
                    LayerAction := TLayerAction.Create(Image);
                    if LayerAction.RetrieveSelectionIfLayerEmpty(True) then
                    begin
                      ComputeSelectionMask(LayerAction.GetOrCreateSelectionLayer,LayerAction.CurrentSelection,Image.SelectionBounds[False]);
                      Image.SelectionMayChange(Image.SelectionBounds[False]);
                      LayerAction.Validate;
                    end;
                    if image.SelectionLayerIsEmpty then MessagePopup(rsNothingToBeRetrieved,2000);
                  except on ex:exception do LazPaintInstance.ShowError(rsMovingOrRotatingSelection,ex.Message);
                  end;
                  LayerAction.Free;
                end;
              end;
            end;
          end;
        end;
      end;
      ToolManager.SetCurrentToolType(Tool);
    except
      on ex:Exception do
      begin
        LazPaintInstance.ShowError('ChooseTool',ex.Message);
        result := srException;
      end;
    end;
  end else
    result := srInvalidParameters;
end;

function TFMain.ScriptViewZoomIn(AVars: TVariableSet): TScriptResult;
begin
  Zoom.ZoomIn;
  result := srOk;
end;

function TFMain.ScriptViewZoomOut(AVars: TVariableSet): TScriptResult;
begin
  Zoom.ZoomOut;
  result := srOk;
end;

function TFMain.ScriptViewZoomOriginal(AVars: TVariableSet): TScriptResult;
begin
  Zoom.ZoomOriginal;
  result := srOk;
end;

function TFMain.ScriptViewZoomFit(AVars: TVariableSet): TScriptResult;
begin
  Zoom.ZoomFit(Image.Width,Image.Height,GetPictureArea);
  result := srOk;
end;

function TFMain.ScriptViewGrid(AVars: TVariableSet): TScriptResult;
begin
  if AVars.IsDefined('Visible') then
  begin
    if LazPaintInstance.GridVisible <> AVars.Booleans['Visible'] then
      ToggleGridVisible;
  end else
    ToggleGridVisible;
  result := srOk;
end;

procedure TFMain.SpinEdit_PhongBorderSizeChange(Sender: TObject);
begin
  if initialized then
  begin
    if ToolManager.ToolShapeBorderSize = SpinEdit_PhongBorderSize.Value then exit;
    ToolManager.ToolShapeBorderSize := SpinEdit_PhongBorderSize.Value;
    UpdateEditPicture;
  end;
end;

procedure TFMain.EditSelectionUpdate(Sender: TObject);
begin
  EditSelection.Enabled := not Scripting.Recording;
end;

procedure TFMain.EditCopyExecute(Sender: TObject);
begin
  if not ToolManager.ToolCopy then
    Scripting.CallScriptFunction('EditCopy');
end;

procedure TFMain.EditCopyUpdate(Sender: TObject);
begin
  EditCopy.Enabled := ToolManager.ToolProvideCopy or not image.SelectionEmpty;
end;

procedure TFMain.EditCutExecute(Sender: TObject);
begin
  if not ToolManager.ToolCut then
    Scripting.CallScriptFunction('EditCut');
end;

procedure TFMain.EditCutUpdate(Sender: TObject);
begin
  EditCut.Enabled := ToolManager.ToolProvideCut or not image.SelectionEmpty;
end;

procedure TFMain.EditDeleteSelectionUpdate(Sender: TObject);
begin
  EditDeleteSelection.Enabled := not image.SelectionEmpty;
end;

procedure TFMain.EditPasteExecute(Sender: TObject);
begin
  if not ToolManager.ToolPaste then
    Scripting.CallScriptFunction('EditPaste');
end;

procedure TFMain.FileImport3DUpdate(Sender: TObject);
begin
  FileImport3d.Enabled := not Scripting.Recording
end;

procedure TFMain.FilePrintExecute(Sender: TObject);
begin
  LazPaintInstance.ShowPrintDlg;
end;

procedure TFMain.FileSaveAsInSameFolderExecute(Sender: TObject);
var dir: string;
begin
  dir := ExtractFilePath(GetCurrentFilenameUTF8);
  if dir <> '' then SavePictureDialog1.InitialDir := dir;
  Scripting.CallScriptFunction('FileSaveAs');
end;

procedure TFMain.FileSaveAsInSameFolderUpdate(Sender: TObject);
begin
  FileSaveAsInSameFolder.Enabled := ExtractFilePath(GetCurrentFilenameUTF8)<>'';
end;

procedure TFMain.FormMouseLeave(Sender: TObject);
begin
  Cursor := crDefault;
end;

procedure TFMain.PaintBox_PenPreviewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var t: integer;
begin
  t := ImageList1.Width;
  if (X < t) and (Y < t) then DecreasePenSize else
  if (X >= PaintBox_PenPreview.Width-t) and (Y < t) then IncreasePenSize;
end;

procedure TFMain.Combo_SplineStyleChange(Sender: TObject);
var v: string;
begin
  if initialized then
  begin
    v := Combo_SplineStyle.Text;
    ToolManager.ToolSplineEasyBezier := false;
    if v = 'Inside' then ToolManager.ToolSplineStyle := ssInside else
    if v = 'Inside + ends' then ToolManager.ToolSplineStyle := ssInsideWithEnds else
    if v = 'Crossing' then ToolManager.ToolSplineStyle := ssCrossing else
    if v = 'Crossing + ends' then ToolManager.ToolSplineStyle := ssCrossingWithEnds else
    if v = 'Outside' then ToolManager.ToolSplineStyle := ssOutside else
    if v = 'Round outside' then ToolManager.ToolSplineStyle:= ssRoundOutside else
    if v = 'Vertex to side' then ToolManager.ToolSplineStyle:= ssVertexToSide else
    if v = 'Easy Bézier' then ToolManager.ToolSplineEasyBezier := true;
    UpdateEditPicture(True);
    UpdateCurveMode;
  end;
end;

procedure TFMain.EditPasteUpdate(Sender: TObject);
begin
  EditPaste.Enabled := ToolManager.ToolProvidePaste or Image.CurrentLayerVisible;
end;

procedure TFMain.EditDeselectUpdate(Sender: TObject);
begin
  EditDeselect.Enabled := not image.SelectionEmpty;
end;

procedure TFMain.EditRedoUpdate(Sender: TObject);
begin
  EditRedo.Enabled := image.CanRedo;
end;

procedure TFMain.EditSelectionExecute(Sender: TObject);
begin
  LazPaintInstance.EditSelection;
end;

procedure TFMain.SpinEdit_EraserChange(Sender: TObject);
begin
    if initialized then
    begin
      if ToolManager.ToolEraserAlpha = SpinEdit_Eraser.value then exit;
      ToolManager.ToolEraserAlpha := SpinEdit_Eraser.Value;
    end;
end;

procedure TFMain.ScriptExecute(Sender: TObject);
var actionName: string;
begin
  if Sender is TAction then
  begin
    actionName := (Sender as TAction).Name;
    if (actionName = 'ImageHorizontalFlip') and not image.SelectionEmpty then actionName := 'SelectionHorizontalFlip' else
    if (actionName = 'ImageVerticalFlip') and not image.SelectionEmpty  then actionName := 'SelectionVerticalFlip';
    CallScriptFunction(actionName);
  end;
end;

procedure TFMain.ShowColorDialogForPen;
begin
  ColorDialog1.Color := BGRAToColor(ToolManager.ToolForeColor);
  if ColorDialog1.Execute then
  begin
       ToolManager.ToolForeColor := ColorToBGRA(ColorDialog1.Color,ToolManager.ToolForeColor.alpha);
       Shape_PenColor.Brush.Color := BGRAToColor(ToolManager.ToolForeColor);
       LazPaintInstance.ColorToFChooseColor;
       UpdateEditPicture;
  end;
end;

procedure TFMain.ShowColorDialogForBack;
begin
  ColorDialog1.Color := BGRAToColor(ToolManager.ToolBackColor);
  if ColorDialog1.Execute then
  begin
       ToolManager.ToolBackColor := ColorToBGRA(ColorDialog1.Color,ToolManager.ToolBackColor.alpha);
       Shape_BackColor.Brush.Color := BGRAToColor(ToolManager.ToolBackColor);
       LazPaintInstance.ColorToFChooseColor;
       UpdateEditPicture;
  end;
end;

procedure TFMain.ShowPenPreview(ShouldRepaint: boolean);
begin
  if not Panel_PenWidthPreview.Visible then
  begin
    Panel_PenWidthPreview.Left := Panel_PenWidth.Left;
    Panel_PenWidthPreview.Top := Panel_PenWidth.Top+Panel_PenWidth.Height;
    Panel_PenWidthPreview.Visible := True;
  end else
    if ShouldRepaint then
      PaintBox_PenPreview.Repaint;
  TimerHidePenPreview.Enabled := false;
  HidePenPreview(3000);
end;

procedure TFMain.HidePenPreview(TimeMs: Integer);
begin
  if Panel_PenWidthPreview.Visible then
  begin
    TimerHidePenPreview.Interval := TimeMs;
    TimerHidePenPreview.Enabled := true;
  end;
end;

procedure TFMain.UpdateTextureIcon;
var
  IconBGRA: TBGRABitmap;
  IconBmp: TBitmap;
begin
  if ToolManager.GetToolTexture = nil then
  begin
    NoTextureIcon;
    exit;
  end;
  IconBGRA := ToolManager.GetToolTexture.Resample(Image_CurrentTexture.Width,Image_CurrentTexture.Height) as TBGRABitmap;
  IconBmp := IconBGRA.MakeBitmapCopy(ColorToRGB(clBtnFace));
  FreeAndNil(IconBGRA);
  Image_CurrentTexture.Picture.Assign(IconBmp);
  IconBmp.Free;
end;

procedure TFMain.LabelAutosize(ALabel: TLabel);
var
  NewSize,Delta: integer;
  Container: TWinControl;
  ANext: TControl;
  I: Integer;
begin
  NewSize := ALabel.Canvas.TextWidth(ALabel.Caption+' ');
  if ALabel.Width <> NewSize then
  begin
    Delta := NewSize-ALabel.Width;
    Container := ALabel.Parent;
    Container.Width := Container.Width+Delta;
    For I := 0 to Container.ControlCount-1 do
    begin
      ANext := Container.Controls[I];
      if ANext.Left > ALabel.Left then
        ANext.Left := ANext.Left+Delta;
    end;
    ALabel.Width := NewSize;
  end;
end;

procedure TFMain.AskMergeSelection(ACaption: string);
var topmostInfo: TTopMostInfo; res: integer;
begin
  if not image.SelectionEmpty and not image.SelectionLayerIsEmpty then
  begin
    topmostInfo:= LazPaintInstance.HideTopmost;
    res := MessageDlg(ACaption,rsMergeSelection,mtConfirmation,[mbYes,mbNo],0);
    LazPaintInstance.ShowTopmost(topmostInfo);
    case res of
      mrYes: EditDeselect.Execute;
    end;
  end;
end;

procedure TFMain.ReleaseMouseButtons(Shift: TShiftState);
begin
  if not (ssLeft in Shift) and btnLeftDown then
  begin
    btnLeftDown := false;
    if ToolManager.ToolUp then PaintPictureNow;
  end;
  if not (ssRight in Shift) and btnRightDown then
  begin
    btnRightDown := false;
    if ToolManager.ToolUp then PaintPictureNow;
  end;
  if not btnLeftDown and not btnRightDown then
  begin
    CanCompressOrUpdateStack := true;
    Image.OnImageChanged.DelayedStackUpdate := False;
  end;
end;

procedure TFMain.UpdatePanelPhongShape;
begin
  SpinEdit_PhongBorderSize.Enabled := (ToolManager.ToolShapeType = 'Rectangle') or
        (ToolManager.ToolShapeType = 'RoundRectangle');
end;

procedure TFMain.UpdateCurveMode;
var
  cm: TToolSplineMode;
  splineTool: TToolGenericSpline;
begin
  if (ToolManager.CurrentTool <> nil) and (ToolManager.CurrentTool is TToolGenericSpline) then
  begin
    splineTool := ToolManager.CurrentTool as TToolGenericSpline;
    Tool_CurveMovePoint.Enabled := not splineTool.IsHandDrawing and not splineTool.IsIdle;
    cm := splineTool.CurrentMode;
    if Tool_CurveMovePoint.Down <> (cm = tsmMovePoint) then
      Tool_CurveMovePoint.Down := (cm = tsmMovePoint);
    if Tool_CurveModeAuto.Down <> (cm = tsmCurveModeAuto) then
      Tool_CurveModeAuto.Down := (cm = tsmCurveModeAuto);
    if Tool_CurveModeAngle.Down <> (cm = tsmCurveModeAngle) then
      Tool_CurveModeAngle.Down := (cm = tsmCurveModeAngle);
    if Tool_CurveModeCurve.Down <> (cm = tsmCurveModeSpline) then
      Tool_CurveModeCurve.Down := (cm = tsmCurveModeSpline);
  end;
end;

function TFMain.ShowOpenTextureDialog: boolean;
var newTex: TBGRABitmap;
  finalFilename: string;
  topMostInfo: TTopMostInfo;
begin
  result := false;
  topMostInfo := LazPaintInstance.HideTopmost;
  try
    OpenTextureDialog.InitialDir := Config.DefaultTextureDirectory;
    if OpenTextureDialog.Execute then
    begin
      try
        newTex := LoadFlatImageUTF8(OpenTextureDialog.FileName, finalFilename, '');
        if LazPaintInstance.BlackAndWhite then
          newTex.InplaceGrayscale;
        ToolManager.SetToolTexture(newTex);
        result := true;
        UpdateTextureIcon;
        UpdateEditPicture;
        Config.SetDefaultTextureDirectory(ExtractFilePath(OpenTextureDialog.FileName));
      except
        on ex:Exception do
          LazPaintInstance.ShowError(rsOpen,ex.Message);
      end;
    end;
  except
    on ex:Exception do
      LazPaintInstance.ShowError('ShowOpenTextureDialog',ex.Message);
  end;
  LazPaintInstance.ShowTopmost(topMostInfo);
end;

procedure TFMain.ShowNoPicture;
begin
  InShowNoPicture := true;
  PaintPictureNow;
  InShowNoPicture:= false;
end;

function TFMain.GetRenderUpdateRect(AIncludeLastToolState: boolean): TRect;
const displayMargin = 1;
begin
  result := Image.RenderUpdateRectInPicCoord;
  if not IsRectEmpty(result) then
  begin
    result := rect(floor((-PictureOffset.X+result.Left)*FLastPictureParameters.actualZoomFactorX)-displayMargin,
    floor((-PictureOffset.Y+result.Top)*FLastPictureParameters.actualZoomFactorY)-displayMargin,
     ceil((-PictureOffset.X+result.Right)*FLastPictureParameters.actualZoomFactorX)+displayMargin,
     ceil((-PictureOffset.Y+result.Bottom)*FLastPictureParameters.actualZoomFactorY)+displayMargin);
  end;
  result := RectUnion(result, Image.RenderUpdateRectInVSCoord);
  if AIncludeLastToolState and Assigned(virtualScreen) then
    result := RectUnion(result, ToolManager.GetRenderBounds(virtualScreen.Width,virtualScreen.Height));
end;

procedure TFMain.SetCurveMode(AMode: TToolSplineMode);
begin
  if (ToolManager.CurrentTool <> nil) and
     (ToolManager.CurrentTool is TToolGenericSpline) then
  begin
    (ToolManager.CurrentTool as TToolGenericSpline).CurrentMode := AMode;
    UpdateCurveMode;
  end;
end;

procedure TFMain.IncreasePenSize;
begin
  SpinEdit_PenWidth.Value := max(SpinEdit_PenWidth.Value+PenSizeDelta(1),MinPenWidthValue);
end;

procedure TFMain.DecreasePenSize;
begin
  SpinEdit_PenWidth.Value := max(SpinEdit_PenWidth.Value-PenSizeDelta(-1),MinPenWidthValue);
end;

function TFMain.PenSizeDelta(direction: integer): integer;
var v: integer;
begin
  v := SpinEdit_PenWidth.Value;
  if direction < 0 then dec(v);
  if v < 100 then result := 10 else
  if v < 200 then result := 20 else
  if v < 500 then result := 50 else
  if v < 1000 then result := 100 else
  if v < 2000 then result := 200 else
  if v < 5000 then result := 500 else
    result := 1000;
end;

procedure TFMain.UpdateWindowCaption;
begin
  if image.currentFilenameUTF8 = '' then
    self.Caption := inttostr(Image.Width)+'x'+inttostr(Image.Height) + ' - ' + LazPaintInstance.Title
  else
    self.Caption := inttostr(Image.Width)+'x'+inttostr(Image.Height) + ' - ' + image.currentFilenameUTF8;
end;

function TFMain.ScriptFileNew(AVars: TVariableSet): TScriptResult;
var
  bitmapRepl: TBGRABitmap;
  vW,vH: TScriptVariableReference;
  w,h: NativeInt;
  whDefined: boolean;
begin
  if ToolManager.ToolSleeping then
  begin
    result := srException;
    exit;
  end;
  vW := AVars.GetVariable('Width');
  vH := AVars.GetVariable('Height');
  whDefined := AVars.IsReferenceDefined(vW) and AVars.IsReferenceDefined(vH);
  if whDefined then
  begin
    w := AVars.GetInteger(vW);
    h := AVars.GetInteger(vH);
    if (w < 1) or (w > MaxImageWidth) or (h < 1) or (h > MaxImageHeight) then
    begin
      result := srInvalidParameters;
      exit;
    end;
  end else
  if AVars.IsReferenceDefined(vW) or AVars.IsReferenceDefined(vH) then //partial parameters
  begin
    result := srInvalidParameters;
    exit;
  end;
  if Image.IsFileModified then
  begin
    case LazPaintInstance.SaveQuestion(rsNewImage) of
    IDYES: FileSave.Execute;
    IDCANCEL: begin
        result := srCancelledByUser;
        exit;
      end;
    end;
  end;
  if whDefined then
    bitmapRepl := LazPaintInstance.MakeNewBitmapReplacement(w,h)
  else
  begin
    if not LazPaintInstance.ShowNewImageDlg(bitmapRepl) then
    begin
      result := srCancelledByUser;
      exit;
    end else
    if Assigned(Scripting.RecordingFunctionParameters) then
    begin
      Scripting.RecordingFunctionParameters.AddInteger('Width', bitmapRepl.Width);
      Scripting.RecordingFunctionParameters.AddInteger('Height', bitmapRepl.Height);
    end;
  end;
  ChooseTool(ptHand);
  image.Assign(bitmapRepl, True, False);
  SetCurrentFilenameUTF8('');
  image.SetSavedFlag;
  result := srOk;
end;

function TFMain.GetCurrentTool: TPaintToolType;
begin
  result := ToolManager.GetCurrentToolType;
end;

procedure TFMain.SwitchColors;
var temp: TBGRAPixel;
begin
  temp := ToolManager.ToolForeColor;
  ToolManager.ToolForeColor := ToolManager.ToolBackColor;
  ToolManager.ToolBackColor := temp;
  UpdateToolbar;
  UpdateEditPicture;
end;

procedure TFMain.OnToolChanged(sender: TToolManager; ANewTool: TPaintToolType);
begin
  if self.Visible then
  begin
    UpdatePanelTextWidth;
    ArrangeToolbars;
    PaintBox_PenPreview.Invalidate;
    Image.OnImageChanged.NotifyObservers;
    UpdateToolImage;
    UpdateToolBar;
  end;
end;

procedure TFMain.OnQueryExitToolHandler(sender: TLazPaintImage);
begin
  if ToolManager.ToolSleeping then exit;
  ChooseTool(ptHand);
end;

procedure TFMain.ZoomFitIfTooBig;
begin
  with GetPictureArea do
    if (image.Width*ZoomFactor > right-left) or (image.Height*ZoomFactor > bottom-top) then
      ViewZoomFit.Execute;
end;

function TFMain.TryOpenFileUTF8(filenameUTF8: string; AddToRecent: Boolean=True): Boolean;
var
  newPicture: TBGRABitmap;
  finalFilenameUTF8: string;
  format: TBGRAImageFormat;

  procedure StartImport;
  begin
    ToolManager.ToolCloseDontReopen;
    if CurrentTool in [ptDeformation,ptRotateSelection,ptMoveSelection,ptTextureMapping,ptLayerMapping] then
      ChooseTool(ptHand);
  end;
  procedure EndImport;
  begin
    if AddToRecent then
    begin
      Config.AddRecentFile(filenameUTF8);
      Config.AddRecentDirectory(ExtractFilePath(filenameUTF8));
    end;
    SetCurrentFilenameUTF8(finalFilenameUTF8);
    image.ClearUndo;
    image.SetSavedFlag;
    ToolManager.ToolOpen;
    ZoomFitIfTooBig;
    result := true;
  end;

begin
  result := false;
  if filenameUTF8 = '' then exit;
  if ToolManager.ToolSleeping then exit;
  if not AbleToLoadUTF8(filenameUTF8) then
  begin
    LazPaintInstance.ShowMessage(rsOpen,rsFileFormatNotRecognized);
    exit;
  end;
  ShowNoPicture;
  Image.OnImageChanged.NotifyObservers;
  finalFilenameUTF8 := filenameUTF8;
  try
    format := DetectFileFormat(filenameUTF8);
    if format in[ifIco,ifGif] then
    begin
      newPicture := LoadFlatImageUTF8(FilenameUTF8, finalFilenameUTF8, '.lzp');
    end else
    begin
      StartImport;
      image.LoadFromFileUTF8(filenameUTF8);
      EndImport;
      newPicture := nil;
    end;
    if (newPicture <> nil) and (newPicture.Width > 0) and (newPicture.Height > 0) then
    begin
      StartImport;
      with ComputeAcceptableImageSize(newPicture.Width,newPicture.Height) do
        if (cx < newPicture.Width) or (cy < newPicture.Height) then
          BGRAReplace(newPicture, newPicture.Resample(cx,cy,rmFineResample));
      FImageActions.SetCurrentBitmap(newPicture, False);
      EndImport;
    end else newPicture.Free;
  except
    on ex: Exception do
    begin
      ToolManager.ToolOpen;
      Image.OnImageChanged.NotifyObservers;
      LazPaintInstance.ShowError(rsOpen,ex.Message);
    end;
  end;
end;

procedure TFMain.FormShow(Sender: TObject);
var r: TRect;
begin
  LazPaintInstance.ColorToFChooseColor;
  LazPaintInstance.ShowTopmost(FTopMostInfo);
  if Position = poDefault then
  begin
    if Config.DefaultMainWindowMaximized then
      WindowState := wsMaximized else
    begin
      r := Config.DefaultMainWindowPosition;
      if (r.right > r.left) and (r.bottom > r.top) then
      begin
        Position := poDesigned;
        Left := r.Left;
        Top := r.Top;
        ClientWidth := r.right-r.left;
        ClientHeight := r.bottom-r.top
      end;
    end;
  end;

  ArrangeToolbars;
  UpdateToolBar;
  shouldArrangeOnResize := true;
  Image.OnImageChanged.AddObserver(@OnImageChangedHandler);
  Image.OnQueryExitToolHandler := @OnQueryExitToolHandler;
  Image.Zoom := Zoom;
  UpdateWindowCaption;
end;

function TFMain.ScriptImageResample(AParams: TVariableSet): TScriptResult;
var w,h: NativeInt;
    f: TResampleFilter;
begin
  if Assigned(Scripting.RecordingFunctionParameters) then AParams := Scripting.RecordingFunctionParameters;
  if AParams.IsDefined('Width') and AParams.IsDefined('Height') and AParams.IsDefined('Quality') then
  begin
    w := AParams.Integers['Width'];
    h := AParams.Integers['Height'];
    f := StrToResampleFilter(AParams.Strings['Quality']);
    if (CompareText(AParams.Strings['Quality'],ResampleFilterStr[f])<>0) or
     (w < 1) or (w > MaxImageWidth) or (h < 1) or (h > MaxImageHeight) then
      result := srInvalidParameters
    else
    try
      LazPaintInstance.Image.Resample(w,h,f);
      result := srOk;
    except
      on ex:exception do
        result := srException;
    end;
  end else
    if LazPaintInstance.ShowResampleDialog(AParams) then
      result := srOk
    else
      result := srCancelledByUser;
end;

procedure TFMain.Image_SwapColorsMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  SwitchColors;
end;

procedure TFMain.SpinEdit_PenWidthChange(Sender: TObject);
begin
  if initialized and not FInPenWidthChange then
  begin
    if round(ToolManager.ToolPenWidth*PenWidthFactor) = max(SpinEdit_PenWidth.Value,MinPenWidthValue) then exit;
    FInPenWidthChange:= true;
    ToolManager.ToolPenWidth := max(SpinEdit_PenWidth.Value,MinPenWidthValue)/PenWidthFactor;
    ShowPenPreview(True);
    UpdateEditPicture;
    FInPenWidthChange:= false;
  end;
end;

procedure TFMain.Tool_CloseShapeClick(Sender: TObject);
begin
  if initialized then
  begin
     if ToolManager.ToolOptionCloseShape <> Tool_CloseShape.Down then
     begin
       ToolManager.ToolOptionCloseShape:= Tool_CloseShape.Down;
       UpdateLineCapBar;
       ArrangeToolbars;
       UpdateEditPicture;
     end;
  end;
end;

procedure TFMain.Shape_BackColorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if LazPaintInstance.ChooseColorVisible and (Button = mbLeft) then
    LazPaintInstance.ChooseColorTarget := ctBackColor
  else
    ShowColorDialogForBack;
end;

procedure TFMain.Shape_PenColorMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if LazPaintInstance.ChooseColorVisible and (Button = mbLeft) then
    LazPaintInstance.ChooseColorTarget := ctForeColor
  else
    ShowColorDialogForPen;
end;

procedure TFMain.SpinEdit_BackOpacityChange(Sender: TObject);
begin
  if initialized then
  begin
    if ToolManager.ToolBackColor.alpha = SpinEdit_BackOpacity.value then exit;
    with ToolManager.ToolBackColor do
      ToolManager.ToolBackColor := BGRA(red,green,blue,SpinEdit_BackOpacity.value);
    LazPaintInstance.ColorToFChooseColor;
    UpdateEditPicture;
  end;
end;

procedure TFMain.SpinEdit_PenOpacityChange(Sender: TObject);
begin
  if initialized then
  begin
    if ToolManager.ToolForeColor.alpha = SpinEdit_PenOpacity.value then exit;
    with ToolManager.ToolForeColor do
      ToolManager.ToolForeColor := BGRA(red,green,blue,SpinEdit_PenOpacity.value);
    LazPaintInstance.ColorToFChooseColor;
    UpdateEditPicture;
  end;
end;

procedure TFMain.SpinEdit_ToleranceChange(Sender: TObject);
begin
  if initialized then
  begin
    if ToolManager.ToolTolerance = SpinEdit_Tolerance.Value then exit;
    ToolManager.ToolTolerance := SpinEdit_Tolerance.Value;
  end;
end;

procedure TFMain.Tool_DiamondGradientClick(Sender: TObject);
begin
     if Tool_DiamondGradient.Down then
     begin
       ToolManager.ToolGradientType := gtDiamond;
       UpdateEditPicture;
     end;
end;

procedure TFMain.Tool_LinearGradientClick(Sender: TObject);
begin
     if Tool_LinearGradient.Down then
     begin
       ToolManager.ToolGradientType := gtLinear;
       UpdateEditPicture;
     end;
end;

procedure TFMain.Tool_ProgressiveFloodfillClick(Sender: TObject);
begin
     if initialized then
       ToolManager.ToolFloodFillOptionProgressive := Tool_ProgressiveFloodfill.Down;
end;

procedure TFMain.Tool_RadialGradientClick(Sender: TObject);
begin
     if Tool_RadialGradient.Down then
     begin
       ToolManager.ToolGradientType := gtRadial;
       UpdateEditPicture;
     end;
end;

procedure TFMain.Tool_ReflectedGradientClick(Sender: TObject);
begin
     if Tool_ReflectedGradient.Down then
     begin
       ToolManager.ToolGradientType := gtReflected;
       UpdateEditPicture;
     end;
end;

procedure TFMain.ToolDrawShapeExecute(Sender: TObject);
begin
  if initialized then
  begin
     if not Tool_DrawShapeBorder.Down and not Tool_FillShape.Down then
        Tool_DrawShapeBorder.Down := true;
     ToolManager.ToolOptionDrawShape:= Tool_DrawShapeBorder.Down;
     UpdateEditPicture;
  end;
end;

procedure TFMain.ToolFillShapeExecute(Sender: TObject);
begin
  if initialized then
  begin
     if not Tool_DrawShapeBorder.Down and not Tool_FillShape.Down then
        Tool_FillShape.Down := true;
     ToolManager.ToolOptionFillShape:= Tool_FillShape.Down;
     UpdateEditPicture;
  end;
end;

procedure TFMain.UpdateLineCapBar;
begin
  if ToolManager.ToolOptionCloseShape then begin
    Panel_LineCap.Width := DoScaleX(27,OriginalDPI);
    with ToolBar10 do begin
      Tool_CapFlat.Visible:=False;
      Tool_CapRound.Visible:=False;
      Tool_CapSquare.Visible:=False;
    end;
  end
  else begin
    Panel_LineCap.Width := DoScaleX(97,OriginalDPI);
    with ToolBar10 do begin
      Tool_CapFlat.Visible:=True;
      Tool_CapRound.Visible:=True;
      Tool_CapSquare.Visible:=True;
    end;
  end;
end;

procedure TFMain.UpdateToolbar;
var maxGridNbX,maxGridNbY: integer;
    colorChange: boolean;
begin
  colorChange:= false;
  if Shape_PenColor.Brush.Color <> BGRAToColor(ToolManager.ToolForeColor) then
  begin
    Shape_PenColor.Brush.Color := BGRAToColor(ToolManager.ToolForeColor);
    colorChange:= true;
  end;
  if SpinEdit_PenOpacity.Value <> ToolManager.ToolForeColor.alpha then
  begin
    SpinEdit_PenOpacity.Value := ToolManager.ToolForeColor.alpha;
    colorChange:= true;
  end;

  if not FInPenWidthChange then
  begin
    if SpinEdit_PenWidth.Value<> round(ToolManager.ToolPenWidth*PenWidthFactor) then
    begin
      FInPenWidthChange:= true;
      SpinEdit_PenWidth.Value := round(ToolManager.ToolPenWidth*PenWidthFactor);
      FInPenWidthChange:= false;
    end;
  end;

  if Shape_BackColor.Brush.Color <> BGRAToColor(ToolManager.ToolBackColor) then
  begin
    Shape_BackColor.Brush.Color := BGRAToColor(ToolManager.ToolBackColor);
    colorChange:= true;
  end;
  if SpinEdit_BackOpacity.Value <> ToolManager.ToolBackColor.alpha then
  begin
    SpinEdit_BackOpacity.Value := ToolManager.ToolBackColor.alpha;
    colorChange:= true;
  end;

  if colorChange then
  begin
    Label_CurrentDiff.Caption := inttostr(round(BGRAWordDiff(ToolManager.ToolForeColor,ToolManager.ToolBackColor)/65535*100))+'%';
    Label_CurrentDiff.Update;
  end;

  maxGridNbX := Max(2,Min(image.Width div 2,50));
  maxGridNbY := Max(2,Min(image.Height div 2,50));
  if SpinEdit_GridNbX.MaxValue <> maxGridNbX then
    SpinEdit_GridNbX.MaxValue := maxGridNbX;
  if SpinEdit_GridNbY.MaxValue <> maxGridNbY then
    SpinEdit_GridNbY.MaxValue := maxGridNbY;

  if SpinEdit_TextOutlineWidth.Enabled <> not ToolManager.ToolTextPhong then
    SpinEdit_TextOutlineWidth.Enabled := not ToolManager.ToolTextPhong;

  LazPaintInstance.ColorToFChooseColor;

  UpdateCurveMode;
end;

procedure TFMain.ToolMoveSelectionUpdate(Sender: TObject);
begin
  ToolMoveSelection.Enabled := not image.SelectionEmpty;
end;

{****************************** Picture ************************}

procedure TFMain.OnPaintHandler;
var r: TRect;
begin
   if InShowNoPicture then
   begin
     PaintBlueAreaOnly;
     exit;
   end;
   if FirstPaint then
   begin
     r := Config.DefaultToolboxWindowPosition;
     if (r.right > r.left) and (r.bottom > r.top) then
       LazPaintInstance.MoveToolboxTo(r.left,r.Top)
     else
       LazPaintInstance.MoveToolboxTo(self.Left,self.Top+self.Height-LazPaintInstance.ToolBoxHeight);
     if Config.DefaultToolboxWindowVisible then ToggleToolboxVisible;

     r := Config.DefaultColorWindowPosition;
     if (r.right > r.left) and (r.bottom > r.top) then
       LazPaintInstance.MoveChooseColorTo(r.left,r.top)
     else
       LazPaintInstance.MoveChooseColorTo(self.Left+self.Width-LazPaintInstance.ChooseColorWidth,
                          self.Top+self.Height-LazPaintInstance.ChooseColorHeight);
     if Config.DefaultColorWindowVisible then ToggleColorsVisible;

     r := Config.DefaultLayerWindowPosition;
     if (r.right > r.left) and (r.bottom > r.top) then
     begin
       LazPaintInstance.MoveLayerWindowTo(r.left,r.top);
       LazPaintInstance.LayerWindowWidth := r.right-r.left;
       LazPaintInstance.LayerWindowHeight := r.Bottom-r.Top;
     end
     else
       LazPaintInstance.MoveLayerWindowTo(self.Left+self.Width-LazPaintInstance.LayerWindowWidth,
                          self.Top+self.Height-LazPaintInstance.ChooseColorHeight-LazPaintInstance.LayerWindowHeight);
     if Config.DefaultLayerWindowVisible then ToggleLayersVisible;

     r := Config.DefaultImageListPosition;
     if (r.right > r.left) and (r.bottom > r.top) then
     begin
       LazPaintInstance.MoveImageListWindowTo(r.left,r.top);
       LazPaintInstance.ImageListWindowWidth:= r.right-r.left;
       LazPaintInstance.ImageListWindowHeight:= r.Bottom-r.Top;
     end
     else
       LazPaintInstance.MoveImageListWindowTo(self.Left+self.Width-LazPaintInstance.ImageListWindowWidth,
                          self.Top+self.Height-LazPaintInstance.ChooseColorHeight-LazPaintInstance.ImageListWindowHeight);
     if Config.DefaultImagelistWindowVisible then ToggleImageListVisible;

     FirstPaint := false;
   end;
   if InFormPaint then exit;
   InFormPaint := true;
   if QueryPaintVirtualScreen and (FLastPictureParameters.defined and
     IsRectEmpty(GetRenderUpdateRect(False))) then
     PaintVirtualScreenImplementation
   else
     PaintPictureImplementation;
   PaintBlueAreaImplementation;

   InFormPaint := false;
end;

procedure TFMain.OnImageChangedHandler(AEvent: TLazPaintImageObservationEvent);
begin
  InvalidatePicture(False);
  if (image.Width <> FLastWidth) or (image.Height <> FLastHeight) then
  begin
    FLastWidth:= image.Width;
    FLastHeight:= image.Height;
    UpdateWindowCaption;
  end;
end;

procedure TFMain.InvalidatePicture(AInvalidateAll: boolean);
{$IFNDEF USEPAINTBOXPICTURE}
var area: TRect;
    DrawOfs: TPoint;
    curPicArea: TRect;
{$ENDIF}
begin
  {$IFDEF USEPAINTBOXPICTURE}
     PaintBox_Picture.Invalidate;
  {$ELSE}
    curPicArea := GetPictureArea;
    if not InShowNoPicture and not AInvalidateAll and FLastPictureParameters.defined and (FLastPictureParameters.actualZoomFactorX > 0) and (FLastPictureParameters.actualZoomFactorY > 0) and
      (FLastPictureParameters.imageWidth = image.Width) and (FLastPictureParameters.imageHeight = image.Height) and
      (FLastPictureParameters.imageOffset.x = Image.ImageOffset.x) and (FLastPictureParameters.imageOffset.y = Image.ImageOffset.y) and
      (FLastPictureParameters.pictureArea.Left = curPicArea.Left) and (FLastPictureParameters.pictureArea.Top = curPicArea.Top) and
      (FLastPictureParameters.pictureArea.Right = curPicArea.Right) and (FLastPictureParameters.pictureArea.Bottom = curPicArea.Bottom) then
    begin
      DrawOfs := PictureCanvasOfs;
      Inc(DrawOfs.X,pictureOrigin.X);
      inc(DrawOfs.Y,pictureOrigin.Y);
      area := GetRenderUpdateRect(True);
      area := RectUnion(area,virtualScreenPenCursorPosBefore.bounds);
      area := RectUnion(area,virtualScreenPenCursorPos.bounds);
      OffsetRect(area, DrawOfs.x,DrawOfs.y);
      InvalidateRect(Handle,@area,False);
    end
    else
    begin
      FLastPictureParameters.defined:=false;
      area:= GetPictureArea;
      InvalidateRect(Handle,@area,False);
    end;
  {$ENDIF}
end;

procedure TFMain.UpdateEditPicture(ADelayed: boolean = false);
begin
  if ToolManager.ToolUpdate then
  begin
    if ADelayed then DelayedPaintPicture := True
    else
      PaintPictureNow;
  end;
end;

procedure TFMain.OnZoomChanged(sender: TZoom; ANewZoom: single);
Var
  NewOfs: TPointF;
  pa: TRect;
begin
  if sender.PositionDefined then
  begin
    pa := GetPictureArea;
    PictureActualSize := point(image.Width,image.Height);
    pictureViewSize := point(round(PictureActualSize.X*Zoom.Factor),round(PictureActualSize.Y*Zoom.Factor));
    PictureOrigin := Point((pa.Left+pa.Right-PictureViewSize.X) div 2+round(image.ImageOffset.X*Zoom.Factor),
        (pa.Top+pa.Bottom-PictureViewSize.Y) div 2+round(image.ImageOffset.Y*Zoom.Factor));
    PictureOffset := Point(0,0);
    NewOfs := FormToBitmap(sender.MousePosition.X,sender.MousePosition.Y);
    image.ImageOffset:= point(image.ImageOffset.X+round(NewOfs.X-sender.BitmapPosition.X),
         image.ImageOffset.Y+round(NewOfs.Y-sender.BitmapPosition.Y));
  end;
  FLastPictureParameters.defined := false;
  UpdateToolbar;
  PaintPictureNow;
end;

procedure TFMain.PaintPictureNow;
begin
  if not visible then exit;
  StackNeedUpdate := true;
  Image.OnImageChanged.NotifyObservers;
  {$IFDEF USEPAINTBOXPICTURE}
    PaintBox_Picture.Update;
  {$ELSE}
    self.Update;
  {$ENDIF}
end;

function TFMain.RetrieveSelectionHighlight(pFormArea: TRect;
  pImageOffset: TPoint; pSelectionRotateAngle: single; pSelectionRotateCenter: TPointF;
   pZoomFactorX, pZoomFactorY: single; selecting: boolean; pSelectionOffset: TPoint;
  out pPartialSelectionHighlight: boolean; out pSelectionHighlightOffset: TPoint): TBGRABitmap;
begin
  if (selectionHighlightInfo.zoomFactorX = pZoomFactorX) and
     (selectionHighlightInfo.zoomFactorY = pZoomFactorY) and
     (not selectionHighlightInfo.partialSelectionHighlight or
     ((selectionHighlightInfo.formArea.Left = pFormArea.Left) and
     (selectionHighlightInfo.formArea.Right = pFormArea.Right) and
     (selectionHighlightInfo.formArea.Top = pFormArea.Top) and
     (selectionHighlightInfo.formArea.Bottom = pFormArea.Bottom) and
     (selectionHighlightInfo.SelectionOffset.X = pSelectionOffset.X) and
     (selectionHighlightInfo.SelectionOffset.Y = pSelectionOffset.Y) and
     (selectionHighlightInfo.ImageOffset.X = pImageOffset.X) and
     (selectionHighlightInfo.ImageOffset.Y = pImageOffset.Y))) and
     (selectionHighlightInfo.selecting = selecting) and
     (selectionHighlightInfo.selectionRotateAngle = pSelectionRotateAngle) and
     ((selectionHighlightInfo.selectionRotateAngle = 0) or (selectionHighlightInfo.selectionRotateCenter = pSelectionRotateCenter)) then
     begin
       result := selectionHighlightInfo.selectionHighlight;
       pPartialSelectionHighlight := selectionHighlightInfo.partialSelectionHighlight;
       pSelectionHighlightOffset := selectionHighlightInfo.selectionHighlightOffset;
     end else
     begin
       result := nil;
       pPartialSelectionHighlight := false;
       pSelectionHighlightOffset := Point(0,0);
     end;
end;

procedure TFMain.StoreSelectionHighlight(pFormArea: TRect;
  pImageOffset: TPoint; pSelectionRotateAngle: single; pSelectionRotateCenter: TPointF; pZoomFactorX,pZoomFactorY: single;
  selecting: boolean; pSelectionOffset: TPoint; pSelectionHighlight: TBGRABitmap;
  pPartialSelectionHighlight: boolean; pSelectionHighlightOffset: TPoint);
begin
   ForgetSelectionHightlight;
   selectionHighlightInfo.formArea.Left := pFormArea.Left;
   selectionHighlightInfo.formArea.Right := pFormArea.Right;
   selectionHighlightInfo.formArea.Top := pFormArea.Top;
   selectionHighlightInfo.formArea.Bottom := pFormArea.Bottom;
   selectionHighlightInfo.ImageOffset.X := pImageOffset.X;
   selectionHighlightInfo.ImageOffset.Y := pImageOffset.Y;
   selectionHighlightInfo.selectionOffset := pSelectionOffset;
   selectionHighlightInfo.selectionRotateAngle := pSelectionRotateAngle;
   selectionHighlightInfo.selectionRotateCenter := pSelectionRotateCenter;
   selectionHighlightInfo.zoomFactorX := pZoomFactorX;
   selectionHighlightInfo.zoomFactorY := pZoomFactorY;
   selectionHighlightInfo.selecting := selecting;
   selectionHighlightInfo.selectionHighlight := pSelectionHighlight;
   selectionHighlightInfo.partialSelectionHighlight := pPartialSelectionHighlight;
   selectionHighlightInfo.selectionHighlightOffset := pSelectionHighlightOffset;
end;

procedure TFMain.ForgetSelectionHightlight;
begin
   if selectionHighlightInfo.selectionHighlight <> nil then
     FreeAndNil(selectionHighlightInfo.selectionHighlight);
end;

procedure TFMain.FormPaint(Sender: TObject);
begin
  {$IFNDEF USEPAINTBOXPICTURE}
  OnPaintHandler;
  {$ENDIF}
end;

function TFMain.GetPictureArea: TRect;
begin
  result := Rect(0,FMenus.ToolbarsHeight,ClientWidth,ClientHeight);
end;

procedure TFMain.PictureSelectionChanged(sender: TLazPaintImage; AOffsetOnly: boolean);
begin
  if not AOffsetOnly or selectionHighlightInfo.partialSelectionHighlight then ForgetSelectionHightlight;
end;

procedure TFMain.SetShowSelectionNormal(const AValue: boolean);
begin
  if FShowSelectionNormal=AValue then exit;
  FShowSelectionNormal:=AValue;
  ForgetSelectionHightlight;
end;

function TFMain.FormToBitmap(X, Y: Integer): TPointF;
begin
  if PictureViewSize.X = 0 then
    result.X := 0 else
     result.x := (x+0.5-pictureOrigin.X)/PictureViewSize.X*PictureActualSize.X+PictureOffset.X-0.5;
  if PictureViewSize.Y = 0 then
    result.Y := 0 else
     result.y := (y+0.5-pictureOrigin.Y)/PictureViewSize.Y*PictureActualSize.Y+PictureOffset.Y-0.5;
end;

function TFMain.FormToBitmap(pt: TPoint): TPointF;
begin
  result := FormToBitmap(pt.X,pt.Y);
end;

function TFMain.BitmapToForm(X, Y: Single): TPointF;
begin
  if PictureActualSize.X = 0 then
    result.X := 0 else
     result.X := (X+0.5-PictureOffset.X)/PictureActualSize.X*PictureViewSize.X+PictureOrigin.X;
  if PictureActualSize.Y = 0 then
    result.Y := 0 else
     result.Y := (Y+0.5-PictureOffset.Y)/PictureActualSize.Y*PictureViewSize.Y+PictureOrigin.Y;
end;

function TFMain.BitmapToForm(pt: TPointF): TPointF;
begin
  result := BitmapToForm(pt.x,pt.y);
end;

function TFMain.BitmapToVirtualScreen(X, Y: Single): TPointF;
begin
  result := BitmapToForm(X,Y) - PointF(pictureOrigin.X,pictureOrigin.Y);
end;

function TFMain.BitmapToVirtualScreen(ptF: TPointF): TPointF;
begin
  result := BitmapToVirtualScreen(ptF.X,ptF.Y);
end;

procedure TFMain.PaintVirtualScreenCursor;
{$IFNDEF USEPAINTBOXPICTURE}
var area: TRect;
    DrawOfs: TPoint;
{$ENDIF}
begin
  QueryPaintVirtualScreen := True;
  {$IFDEF USEPAINTBOXPICTURE}
     PaintBox_Picture.Invalidate;
  {$ELSE}
    DrawOfs := PictureCanvasOfs;
    Inc(DrawOfs.X,pictureOrigin.X);
    inc(DrawOfs.Y,pictureOrigin.Y);

    area := virtualScreenPenCursorPos.bounds;
    virtualScreenPenCursorPos := GetVSCursorPosition;
    area := RectUnion(area, virtualScreenPenCursorPos.bounds);
    OffsetRect(area, DrawOfs.x,DrawOfs.y);
    InvalidateRect(Handle,@area,False);
  {$ENDIF}
  {$IFDEF USEPAINTBOXPICTURE}
    PaintBox_Picture.Update;
  {$ELSE}
    self.Update;
  {$ENDIF}
  QueryPaintVirtualScreen := False;
end;

{$hints off}
procedure TFMain.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
   //  block Erasing background
   //  inherited EraseBackground(DC);
end;

procedure TFMain.UpdatePanelTextWidth;
var showElem: boolean;
begin
  if ToolManager.ToolTextShadow then
  begin
    Panel_Text.Width := SpinEdit_TextShadowY.Left+SpinEdit_TextShadowY.Width+6;
    showElem := true;
  end
  else
  begin
    Panel_Text.Width := Toolbar15.Left + Toolbar15.Width + 6;
    showElem:= false;
  end;
  Label_TextBlur.Visible := showElem;
  SpinEdit_TextBlur.Visible := showElem;
  Label_ShadowOffset.Visible := showElem;
  SpinEdit_TextShadowX.Visible := showElem;
  SpinEdit_TextShadowY.Visible := showElem;
end;

function TFMain.GetScriptContext: TScriptContext;
begin
  result := LazPaintInstance.ScriptContext;
end;

procedure TFMain.CallScriptFunction(AName: string);
begin
  case Scripting.CallScriptFunction(AName) of
    srFunctionNotDefined: LazPaintInstance.ShowMessage(rsScript,StringReplace(rsFunctionNotDefined,'%1',AName,[]));
  end;
end;

procedure TFMain.CallScriptFunction(AParams: TVariableSet);
begin
  case Scripting.CallScriptFunction(AParams) of
    srFunctionNotDefined: LazPaintInstance.ShowMessage(rsScript,StringReplace(rsFunctionNotDefined,'%1',AParams.FunctionName,[]));
  end;
end;

{$hints on}

function TFMain.UpdateCursor(X,Y: integer): boolean;
var virtualScreenPenCursorBefore: boolean;
    wantedCursor: TCursor;

  function UseVSPenCursor: boolean;
  begin
    if (ToolManager.ToolPenWidth * Zoom.Factor > 6) and (X >= pictureOrigin.X) and (Y >= pictureOrigin.Y) and
                (X < pictureOrigin.X+pictureViewSize.X) and
                (Y < pictureOrigin.Y+pictureViewSize.Y) then
    begin
      virtualScreenPenCursor := True;
      {$IFNDEF DARWIN}   //on mac, cursor gets also invisible in tool windows
      wantedCursor := crNone;
      {$ENDIF}
      result := true;
    end else
      result := false;
  end;

begin
  result := false;
  virtualScreenPenCursorBefore := virtualScreenPenCursor;
  virtualScreenPenCursor := false;
  wantedCursor := ToolManager.Cursor;
  if CurrentTool in[ptPen,ptEraser] then UseVSPenCursor;
  {$IFNDEF USEPAINTBOXPICTURE}
  if not PtInRect(GetPictureArea, Point(X,Y)) then
    wantedCursor:= crDefault;
  if cursor <> wantedCursor then Cursor := wantedCursor;
  {$ENDIF}
  if PaintBox_Picture.Cursor <> wantedCursor then PaintBox_Picture.Cursor := wantedCursor;
  result := (virtualScreenPenCursorBefore <> virtualScreenPenCursor);
end;

function TFMain.GetVSCursorPosition: TVSCursorPosition;
const margin = 2;
var
  orig,tl,br: TPointF;
begin
  orig := PointF(pictureOrigin.X,pictureOrigin.Y);
  with ToolManager do
  begin
    result.c := self.BitmapToForm(ToolCurrentCursorPos) - orig;
    tl := self.BitmapToForm(ToolCurrentCursorPos.X-ToolPenWidth/2,ToolCurrentCursorPos.Y-ToolPenWidth/2) - orig;
    br := self.BitmapToForm(ToolCurrentCursorPos.X+ToolPenWidth/2,ToolCurrentCursorPos.Y+ToolPenWidth/2) - orig;
  end;
  result.rx := (br.x-tl.x)/2-0.5;
  result.ry := (br.y-tl.y)/2-0.5;
  if virtualScreenPenCursor then
  begin
    result.bounds.left := floor(tl.x)-margin;
    result.bounds.top := floor(tl.y)-margin;
    result.bounds.right := ceil(br.x)+1+2*margin;
    result.bounds.bottom := ceil(br.y)+1+2*margin;
  end else
    result.bounds := EmptyRect;
end;

function TFMain.GetZoomFactor: single;
begin
  if Assigned(Zoom) then result := zoom.Factor else result := 1;
end;

procedure TFMain.PaintVirtualScreenImplementation;
var cursorBack: TBGRABitmap;
    DestCanvas: TCanvas;
    DrawOfs: TPoint;
    cursorContourF: array of TPointF;
    rectBack: TRect;
    cursorPos: TVSCursorPosition;
begin
  if virtualscreen = nil then exit;
  DelayedPaintPicture := false;
  DestCanvas := PictureCanvas;
  DrawOfs := PictureCanvasOfs;
  Inc(DrawOfs.X,pictureOrigin.X);
  inc(DrawOfs.Y,pictureOrigin.Y);

  cursorPos := virtualScreenPenCursorPos;
  if virtualScreenPenCursor and not IsRectEmpty(cursorPos.bounds) then
  begin
    rectBack := cursorPos.bounds;
    IntersectRect(rectBack,rectBack,rect(0,0,virtualScreen.Width,virtualScreen.Height));
    if not IsRectEmpty(rectBack) then
    begin
      cursorBack := virtualScreen.GetPart(rectBack) as TBGRABitmap;

      cursorContourF := virtualscreen.ComputeEllipseContour(cursorPos.c.x,cursorPos.c.y,cursorPos.rx,cursorPos.ry);
      virtualscreen.PenStyle := psSolid;
      virtualscreen.DrawPolygonAntialias(cursorcontourF,BGRA(0,0,0,192),3);
      virtualscreen.DrawPolygonAntialias(cursorcontourF,BGRA(255,255,255,255),1);
      virtualscreen.Draw(DestCanvas, DrawOfs.X, DrawOfs.Y, True);

      virtualScreen.PutImage(rectBack.left,rectBack.Top,cursorBack,dmSet);
      cursorBack.Free;
    end else
      virtualscreen.Draw(DestCanvas, DrawOfs.X, DrawOfs.Y, True);
  end else
    virtualscreen.Draw(DestCanvas, DrawOfs.X, DrawOfs.Y, True);

  if (image.Width = 0) or (image.Height = 0) then
    Zoom.MinFactor := 1
  else
    Zoom.MinFactor := max(8/image.Width, 8/image.Height);
  with GetPictureArea do
    Zoom.MaxFactor := max(1,min((right-left)/8,(bottom-top)/8));
end;

procedure TFMain.PaintPictureImplementation;
var //layout
    formArea: TRect;
    visualLeft, visualTop: integer;
    visualWidth, visualHeight: integer;
    maxOffset, minOffset: TPoint; temp: integer;

    //partial drawing
    topLeftCorner,bottomRightCorner: TPoint;
    topLeftCornerF,bottomRightCornerF: TPointF;
    imageClipped: boolean;
    renderRect: TRect;

  procedure DrawSelectionHighlight;
  var
    shownSelection, resampledSelection, selectionHighlight: TBGRABitmap;
    partialSelectionHighlight,okForCompleteSelection: boolean;
    highlightOffset: TPoint;
    ofs: TPoint;
    selectionBounds: TRect;
  begin
    selectionHighlight := RetrieveSelectionHighlight(formArea,image.ImageOffset,image.GetSelectionRotateAngle,
         image.GetSelectionRotateCenter,
         FLastPictureParameters.actualZoomFactorX, FLastPictureParameters.actualZoomFactorY,ToolManager.IsSelectingTool, image.GetSelectionOffset,
         partialSelectionHighlight,highlightOffset);

    //if selection highlight has not been computed yet, then compute shown selection
    selectionBounds := EmptyRect;
    shownSelection := nil;
    if selectionHighlight = nil then
    begin
      if not image.SelectionNil then
      begin
        if image.GetSelectionRotateAngle <> 0 then
        begin
          if image.SelectionLayerIsEmpty then
          begin
            shownSelection := image.ComputeRotatedSelection;
            if Assigned(shownSelection) then
              selectionBounds := shownSelection.GetImageBounds(cRed);
          end;
        end else
          if not (image.SelectionEmptyComputed and image.SelectionEmpty) then
          begin
            shownSelection := image.SelectionReadonly;
            selectionBounds := image.SelectionBounds[False];
          end;
      end;
    end;

    //compute selection highlight if needed
    if (selectionHighlight = nil) and (shownSelection <> nil) and
      not IsRectEmpty(selectionBounds) then
    begin
      InflateRect(selectionBounds,1,1);
      okForCompleteSelection := false;
      if imageClipped then
      begin
        if ((selectionBounds.right-selectionBounds.left)*FLastPictureParameters.actualZoomFactorX <= visualWidth) and
            ((selectionBounds.bottom-selectionBounds.top)*FLastPictureParameters.actualZoomFactorY <= visualHeight) then
              okForCompleteSelection:= true;
      end;
      if imageClipped and not okForCompleteSelection then
      begin
        resampledSelection := TBGRABitmap.Create(visualWidth,visualHeight,BGRABlack);
        ofs := Point(-PictureOffset.X+image.GetSelectionOffset.X,-PictureOffset.Y+image.GetSelectionOffset.Y);
        partialSelectionHighlight:= true;
        resampledSelection.StretchPutImage(rect(round(ofs.x*FLastPictureParameters.actualZoomFactorX),
           round(ofs.y*FLastPictureParameters.actualZoomFactorY),
           round((ofs.x+Image.Width)*FLastPictureParameters.actualZoomFactorX),
           round((ofs.y+Image.Height)*FLastPictureParameters.actualZoomFactorY)),shownSelection,dmSet);
        highlightOffset := Point(0,0);
        selectionHighlight := resampledSelection.FilterEmbossHighlight(
               ToolManager.IsSelectingTool and not ShowSelectionNormal, BGRABlack, highlightOffset) as TBGRABitmap;
        StoreSelectionHighlight(formArea,image.ImageOffset,image.GetSelectionRotateAngle,image.GetSelectionRotateCenter,
          FLastPictureParameters.actualZoomFactorX, FLastPictureParameters.actualZoomFactorY,
          ToolManager.IsSelectingTool,image.GetSelectionOffset,selectionHighlight,partialSelectionHighlight,highlightOffset);
        FreeAndNil(resampledSelection);
      end else
      begin
        partialSelectionHighlight:= false;
        highlightOffset := Point(0,0);
        if (FLastPictureParameters.actualZoomFactorX <> 1) or (FLastPictureParameters.actualZoomFactorY <> 1) then
        begin
          highlightOffset := Point(round(selectionBounds.Left*FLastPictureParameters.actualZoomFactorX),
            round(selectionBounds.Top*FLastPictureParameters.actualZoomFactorY));
          resampledSelection := TBGRABitmap.Create(round((selectionBounds.Right-selectionBounds.Left)*FLastPictureParameters.actualZoomFactorX),
               round((selectionBounds.Bottom-selectionBounds.Top)*FLastPictureParameters.actualZoomFactorY));
          resampledSelection.StretchPutImage(rect(-highlightOffset.X,-highlightOffset.Y,
             round((-selectionBounds.Left+shownSelection.Width)*FLastPictureParameters.actualZoomFactorX),
             round((-selectionBounds.Top+shownSelection.Height)*FLastPictureParameters.actualZoomFactorY)),shownSelection,dmSet);
          selectionHighlight := resampledSelection.FilterEmbossHighlight(
                 ToolManager.IsSelectingTool and not ShowSelectionNormal, BGRABlack, highlightOffset) as TBGRABitmap;
          FreeAndNil(resampledSelection);
        end else
          selectionHighlight := shownSelection.FilterEmbossHighlight(
                 ToolManager.IsSelectingTool and not ShowSelectionNormal, BGRABlack, highlightOffset) as TBGRABitmap;
        StoreSelectionHighlight(formArea,image.ImageOffset,image.GetSelectionRotateAngle,image.GetSelectionRotateCenter,
          FLastPictureParameters.actualZoomFactorX, FLastPictureParameters.actualZoomFactorY,
          ToolManager.IsSelectingTool,image.GetSelectionOffset,selectionHighlight,partialSelectionHighlight,highlightOffset);
      end;
    end;

    if selectionHighlight <> nil then
    begin
      //draw previously stored highlight
      if partialSelectionHighlight then
        virtualScreen.PutImage(highlightOffset.X,highlightOffset.y,selectionHighlight,dmFastBlend)
      else
        virtualScreen.PutImage(highlightOffset.X+integer(round((-PictureOffset.x+image.GetSelectionOffset.X)*zoom.Factor)),
          highlightOffset.Y+integer(round((-PictureOffset.Y+image.GetSelectionOffset.Y)*zoom.Factor)),selectionHighlight,dmFastBlend);
    end;
    if shownSelection <> image.SelectionReadonly then shownSelection.Free;
  end;

begin
  formArea := GetPictureArea;
  if (formArea.Right <= formArea.Left) or (formArea.Bottom <= formArea.Top)
    then exit;

  if not InFormPaint then
  begin
     Repaint;
     exit;
  end;

  visualWidth := round(image.Width*Zoom.Factor);
  if visualWidth = 0 then visualWidth := 1;
  visualHeight := round(image.Height*Zoom.Factor);
  if visualHeight = 0 then visualHeight := 1;
  visualLeft := (formArea.Left+formArea.Right-visualWidth) div 2;
  visualTop := (formArea.Top+formArea.Bottom-visualHeight) div 2;

  maxOffset := point(floor((formArea.Right-(visualLeft+visualWidth))/Zoom.Factor),
       floor((formArea.Bottom-(visualTop+visualHeight))/Zoom.Factor));
  minOffset := point(ceil((formArea.Left-visualLeft)/Zoom.Factor),
               ceil((formArea.Top-visualTop)/Zoom.Factor));
  if maxOffset.X < minOffset.X then
  begin
    temp := maxOffset.X;
    maxOffset.X := minOffset.X;
    minOffset.X := temp;
  end;
  if maxOffset.Y < minOffset.Y then
  begin
    temp := maxOffset.Y;
    maxOffset.Y := minOffset.Y;
    minOffset.Y := temp;
  end;
  if image.ImageOffset.X < minOffset.X then image.ImageOffset.X := minOffset.X else
  if image.ImageOffset.X > maxOffset.X then image.ImageOffset.X := maxOffset.X;

  if image.ImageOffset.Y < minOffset.Y then image.ImageOffset.Y := minOffset.Y else
  if image.ImageOffset.Y > maxOffset.Y then image.ImageOffset.Y := maxOffset.Y;

  if image.width <> 0 then
    visualLeft += round(image.ImageOffset.X*visualWidth/image.Width);
  if image.height <> 0 then
    visualTop += round(image.ImageOffset.Y*visualHeight/image.Height);

  if (visualLeft < formArea.left) or (visualTop < formArea.Top) or
    (visualLeft+visualWidth > formArea.Right) or (visualTop+visualHeight > formArea.Bottom) then
  begin
    imageClipped:= true;
    TopLeftCornerF := PointF((formArea.Left-visualLeft)/visualWidth*image.Width,
       (formArea.Top-visualTop)/visualHeight*image.Height);
    BottomRightCornerF := PointF((formArea.Right-visualLeft)/visualWidth*image.Width,
       (formArea.Bottom-visualTop)/visualHeight*image.Height);
    TopLeftCorner := Point(floor(topLeftCornerF.X),floor(topLeftCornerF.Y));
    bottomRightCorner := Point(ceil(bottomRightCornerF.X),ceil(bottomRightCornerF.Y));

    TopLeftCorner.x := max(0, TopLeftCorner.x);
    TopLeftCorner.y := max(0, TopLeftCorner.y);
    BottomRightCorner.x := min(image.Width, BottomRightCorner.x);
    BottomRightCorner.y := min(image.Height, BottomRightCorner.y);

    PictureActualSize := Point(BottomRightCorner.x-TopLeftCorner.x, BottomRightCorner.Y-TopLeftCorner.Y);
    PictureOffset := Point(TopLeftCorner.x,TopLeftCorner.Y);

    visualLeft := visualLeft + round(TopLeftCorner.x/image.width*visualWidth);
    visualTop := visualTop + round(TopLeftCorner.y/image.height*visualHeight);
    visualWidth := round(PictureActualSize.X*Zoom.Factor);
    if visualWidth = 0 then visualWidth := 1;
    visualHeight := round(PictureActualSize.Y*Zoom.Factor);
    if visualHeight = 0 then visualHeight := 1;
  end else
  begin
    imageClipped := false;
    PictureActualSize.X := image.width;
    PictureActualSize.Y := image.Height;
    PictureOffset := Point(0,0);
  end;
  if pictureActualSize.X = 0 then
    FLastPictureParameters.actualZoomFactorX:= 1
  else
    FLastPictureParameters.actualZoomFactorX:= visualWidth/PictureActualSize.X;
  if pictureActualSize.Y = 0 then
    FLastPictureParameters.actualZoomFactorY:= 1
  else
    FLastPictureParameters.actualZoomFactorY:= visualHeight/PictureActualSize.Y;
  FLastPictureParameters.pictureArea := GetPictureArea;
  FLastPictureParameters.imageOffset := Image.ImageOffset;
  FLastPictureParameters.imageWidth := Image.Width;
  FLastPictureParameters.imageHeight := Image.Height;

  //create or resize virtual screen if needed
  if (virtualscreen = nil) or (virtualScreen.Width <> visualWidth) or (virtualScreen.Height <> visualHeight) then
  begin
    FreeAndNil(virtualScreen);
    virtualScreen := TBGRABitmap.Create(visualWidth, visualHeight);
  end else
    if FLastPictureParameters.defined then
      virtualScreen.ClipRect := GetRenderUpdateRect(False);
  Image.ResetRenderUpdateRect;
  FLastPictureParameters.defined := true;

  renderRect := rect(round(-PictureOffset.X*FLastPictureParameters.actualZoomFactorX),round(-PictureOffset.Y*FLastPictureParameters.actualZoomFactorY),
     round((-PictureOffset.X+Image.Width)*FLastPictureParameters.actualZoomFactorX),
     round((-PictureOffset.Y+Image.Height)*FLastPictureParameters.actualZoomFactorY));
  DrawCheckers(virtualScreen, renderRect);

  //draw image (with merged selection)
  virtualScreen.StretchPutImage(renderRect,Image.RenderedImage,dmDrawWithTransparency);
  if (Zoom.Factor > MinZoomForGrid) and LazPaintInstance.GridVisible then
    DrawGrid(virtualScreen,FLastPictureParameters.actualZoomFactorX,FLastPictureParameters.actualZoomFactorY);

  DrawSelectionHighlight;
  virtualScreen.NoClip;

  //define picture view
  pictureOrigin := Point(visualLeft,visualTop);
  pictureViewSize := Point(visualWidth,visualHeight);

  //show tools info
  ToolManager.RenderTool(virtualScreen);

  PaintVirtualScreenImplementation;
end;

procedure TFMain.PaintBlueAreaImplementation;
var
  DrawOfs: TPoint;
  formArea: TRect;
begin
  formArea := GetPictureArea;
  if (formArea.Right <= formArea.Left) or (formArea.Bottom <= formArea.Top)
      then exit;
  with PictureCanvas do
  begin
    Brush.Color := FormBackgroundColor;
    DrawOfs := PictureCanvasOfs;
    if pictureOrigin.X > formArea.Left then
      FillRect(formArea.Left+DrawOfs.X,formArea.Top+DrawOfs.Y,pictureOrigin.X+DrawOfs.X,formArea.Bottom+DrawOfs.Y);
    if pictureOrigin.Y > formArea.Top then
      FillRect(formArea.Left+DrawOfs.X,formArea.Top+DrawOfs.Y,formArea.Right+DrawOfs.X,pictureOrigin.Y+DrawOfs.Y);
    if pictureOrigin.X+pictureViewSize.x < formArea.Right then
      FillRect(pictureOrigin.X+pictureViewSize.x+DrawOfs.X,formArea.Top+DrawOfs.Y,formArea.Right+DrawOfs.X,formArea.Bottom+DrawOfs.Y);
    if pictureOrigin.Y+pictureViewSize.y < formArea.Bottom then
      FillRect(formArea.Left+DrawOfs.X,pictureOrigin.Y+pictureViewSize.y+DrawOfs.Y,formArea.Right+DrawOfs.X,formArea.Bottom+DrawOfs.Y);
  end;
end;

procedure TFMain.PaintBlueAreaOnly;
var
  formArea: TRect;
  DrawOfs: TPoint;
begin
  formArea := GetPictureArea;
  if (formArea.Right <= formArea.Left) or (formArea.Bottom <= formArea.Top)
      then exit;
  with PictureCanvas do
  begin
    Brush.Color := FormBackgroundColor;
    DrawOfs := PictureCanvasOfs;
    FillRect(formArea.Left+DrawOfs.X,formArea.Top+DrawOfs.Y,formArea.Right+DrawOfs.X,formArea.Bottom+DrawOfs.Y);
  end;
  FLastPictureParameters.defined := false;
end;

function TFMain.PictureCanvas: TCanvas;
begin
  {$IFDEF USEPAINTBOXPICTURE}
  result := PaintBox_Picture.Canvas;
  {$ELSE}
  result := Canvas;
  {$ENDIF}
end;

function TFMain.PictureCanvasOfs: TPoint;
begin
  {$IFDEF USEPAINTBOXPICTURE}
  result := Point(0,-PaintBox_Picture.Top);
  {$ELSE}
  result := Point(0,0);
  {$ENDIF}
end;

{$R *.lfm}

end.