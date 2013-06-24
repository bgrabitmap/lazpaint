unit LazpaintMainForm;

{$mode objfpc}{$H+}

interface

{$IFDEF DARWIN}
  {$DEFINE USEPAINTBOXPICTURE}
{$ENDIF}

uses
  Classes, LMessages, SysUtils, FileUtil, LResources, Forms, Controls,
  Graphics, Dialogs, Menus, ExtDlgs, ComCtrls, ActnList, StdCtrls, ExtCtrls,
  Spin, Buttons, types, LCLType,

  BGRABitmap, BGRABitmapTypes, BGRALayers,

  LazPaintType, UTool, UImage, UImageAction, UZoom,
  UImageObservation, UConfig, UScaleDPI, UOnline, UResourceStrings;

type
  { TFMain }

  TFMain = class(TForm)
    FilterFunction: TAction;
    FilterPhong: TAction;
    FileReload: TAction;
    MainMenu1: TMainMenu;
    MenuFile: TMenuItem;
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
    Panel_PerspectiveOption: TPanel;
    Perspective_Repeat: TToolButton;
    Perspective_TwoPlanes: TToolButton;
    SpinEdit_TextureOpacity: TSpinEdit;
    ToolBar18: TToolBar;
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
    SpinEdit_TextOutlineWidth: TSpinEdit;
    ToolBar17: TToolBar;
    Tool_TextAlignLeft: TToolButton;
    Tool_TextAlignCenter: TToolButton;
    Tool_TextAlignRight: TToolButton;
    Tool_TextOutline: TToolButton;
    Open3DObjectDialog: TOpenPictureDialog;
    ToolButton_ViewLayerStack: TToolButton;
    Label_Altitude: TLabel;
    Label_PhongBorder: TLabel;
    Label_CurrentDiff: TLabel;
    Label_Shape: TLabel;
    Panel_PhongShape: TPanel;
    Panel_Altitude: TPanel;
    SpinEdit_PhongBorderSize: TSpinEdit;
    SpinEdit_ShapeAltitude: TSpinEdit;
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
    SpinEdit_TextBlur: TSpinEdit;
    SpinEdit_TextShadowX: TSpinEdit;
    SpinEdit_TextShadowY: TSpinEdit;
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
    SpinEdit_GridNbY: TSpinEdit;
    Panel_Grid: TPanel;
    Label_Grid: TLabel;
    SpinEdit_GridNbX: TSpinEdit;
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
    SpinEdit_Eraser: TSpinEdit;
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
    SpinEdit_PenWidth: TSpinEdit;
    Tool_DrawShapeBorder: TToolButton;
    Tool_FillShape: TToolButton;
    Shape_BackColor: TShape;
    Shape_PenColor: TShape;
    SpinEdit_BackOpacity: TSpinEdit;
    SpinEdit_PenOpacity: TSpinEdit;
    Label_Tolerance: TLabel;
    SpinEdit_Tolerance: TSpinEdit;
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
    ImageList1: TImageList;
    SavePictureDialog1: TSavePictureDialog;
    procedure SpinEdit_PhongBorderSizeChange(Sender: TObject);
    procedure ColorColorizeExecute(Sender: TObject);
    procedure ColorShiftColorsExecute(Sender: TObject);
    procedure ColorIntensityExecute(Sender: TObject);
    procedure ColorLightnessExecute(Sender: TObject);
    procedure Combo_SplineStyleChange(Sender: TObject);
    procedure EditCopyExecute(Sender: TObject);
    procedure EditCutExecute(Sender: TObject);
    procedure EditDeleteSelectionExecute(Sender: TObject);
    procedure EditDeselectExecute(Sender: TObject);
    procedure EditDeselectUpdate(Sender: TObject);
    procedure EditInvertSelectionExecute(Sender: TObject);
    procedure EditPasteAsNewExecute(Sender: TObject);
    procedure EditPasteExecute(Sender: TObject);
    procedure EditPasteUpdate(Sender: TObject);
    procedure EditRedoExecute(Sender: TObject);
    procedure EditRedoUpdate(Sender: TObject);
    procedure EditSelectAllExecute(Sender: TObject);
    procedure EditSelectionExecute(Sender: TObject);
    procedure EditSelectionFitExecute(Sender: TObject);
    procedure EditUndoExecute(Sender: TObject);
    procedure EditUndoUpdate(Sender: TObject);
    procedure EmbeddedCancelExecute(Sender: TObject);
    procedure EmbeddedValidateExecute(Sender: TObject);
    procedure FileImport3DExecute(Sender: TObject);
    procedure FileLoadSelectionExecute(Sender: TObject);
    procedure FileReloadExecute(Sender: TObject);
    procedure FileReloadUpdate(Sender: TObject);
    procedure FileSaveSelectionAsExecute(Sender: TObject);
    procedure FileSaveSelectionAsUpdate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormMouseEnter(Sender: TObject);
    procedure FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure ImageCropLayerExecute(Sender: TObject);
    procedure ImageCropLayerUpdate(Sender: TObject);
    procedure ImageFlattenExecute(Sender: TObject);
    procedure ImageFlattenUpdate(Sender: TObject);
    procedure LayerAddNewExecute(Sender: TObject);
    procedure LayerAddNewUpdate(Sender: TObject);
    procedure LayerDuplicateExecute(Sender: TObject);
    procedure LayerDuplicateUpdate(Sender: TObject);
    procedure LayerFromFileExecute(Sender: TObject);
    procedure LayerHorizontalFlipExecute(Sender: TObject);
    procedure LayerMergeOverExecute(Sender: TObject);
    procedure LayerMergeOverUpdate(Sender: TObject);
    procedure LayerMoveExecute(Sender: TObject);
    procedure LayerMoveUpdate(Sender: TObject);
    procedure LayerRemoveCurrentExecute(Sender: TObject);
    procedure LayerRemoveCurrentUpdate(Sender: TObject);
    procedure LayerRotateExecute(Sender: TObject);
    procedure LayerRotateUpdate(Sender: TObject);
    procedure LayerVerticalFlipExecute(Sender: TObject);
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
    procedure ImageClearAlphaExecute(Sender: TObject);
    procedure ImageCropExecute(Sender: TObject);
    procedure ImageCropUpdate(Sender: TObject);
    procedure ImageFillBackgroundExecute(Sender: TObject);
    procedure ImageRepeatExecute(Sender: TObject);
    procedure ImageRotateCCWExecute(Sender: TObject);
    procedure ImageRotateCWExecute(Sender: TObject);
    procedure MenuFileClick(Sender: TObject);
    procedure RecentFileClick(Sender: TObject);
    procedure LanguageClick(Sender: TObject);
    procedure SpinEdit_TextOutlineWidthChange(Sender: TObject);
    procedure SpinEdit_TextShadowXChange(Sender: TObject);
    procedure SpinEdit_TextShadowYChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TimerHidePenPreviewTimer(Sender: TObject);
    procedure ToolButton_ViewLayerStackClick(Sender: TObject);
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
    procedure ViewGridExecute(Sender: TObject);
    procedure ViewLayerStackButtonUpdate(Sender: TObject);
    procedure ViewLayerStackExecute(Sender: TObject);
    procedure ViewLayerStackUpdate(Sender: TObject);
    procedure ViewToolboxUpdate(Sender: TObject);
    procedure ViewZoomFitExecute(Sender: TObject);
    procedure ViewZoomInExecute(Sender: TObject);
    procedure ViewZoomOriginalExecute(Sender: TObject);
    procedure ViewZoomOutExecute(Sender: TObject);
    procedure SpinEdit_EraserChange(Sender: TObject);
    procedure FileNewExecute(Sender: TObject);
    procedure FileOpenExecute(Sender: TObject);
    procedure FileQuitExecute(Sender: TObject);
    procedure FileSaveAsExecute(Sender: TObject);
    procedure FileSaveExecute(Sender: TObject);
    procedure FileSaveUpdate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure ImageSmartZoom3Execute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ImageHorizontalFlipExecute(Sender: TObject);
    procedure ImageResampleExecute(Sender: TObject);
    procedure ImageVerticalFlipExecute(Sender: TObject);
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
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;

  private
    { private declarations }
    Zoom: TZoom;
    FOnlineUpdater: TLazPaintOnlineUpdater;

    initialized: boolean;
    shouldArrangeOnResize: boolean;
    toolbars: TList;
    ToolbarsHeight : integer;
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
    DelayedPaintPicture: boolean;
    previousToolImg: integer;
    InShowNoPicture: boolean;

    selectionHighlightInfo : record
       formArea: TRect;
       ImageOffset: TPoint;
       zoomFactor: single;
       selectionHighlight: TBGRABitmap;
       selecting : boolean;
       selectionRotateAngle: single;
       selectionRotateCenter: TPointF;
       highlightOffset: TPoint;
    end;

    procedure AddMenus(AMenu: TMenuItem; AActionsCommaText: string; AIndex: integer = -1);
    function GetCurrentTool: TPaintToolType;
    procedure SwitchColors;
    procedure Init;
    procedure OnToolChanged({%H-}sender: TToolManager; {%H-}ANewTool: TPaintToolType);
    procedure OnQueryExitToolHandler({%H-}sender: TLazPaintImage);
    procedure OnZoomChanged({%H-}sender: TZoom; {%H-}ANewZoom: single);
    procedure SetLazPaintInstance(const AValue: TLazPaintCustomInstance);
    procedure SetShowSelectionNormal(const AValue: boolean);
    function RetrieveSelectionHighlight(pFormArea: TRect; pImageOffset: TPoint; pSelectionRotateAngle: single; pSelectionRotateCenter: TPointF;pZoomFactor: single; selecting: boolean; out pHighlightOffset: TPoint): TBGRABitmap;
    procedure StoreSelectionHighlight(pFormArea: TRect; pImageOffset: TPoint; pSelectionRotateAngle: single; pSelectionRotateCenter: TPointF;pZoomFactor: single; selecting: boolean; pSelectionHighlight: TBGRABitmap; pHighlightOffset: TPoint);
    procedure ForgetSelectionHightlight;
    procedure ToggleToolwindowsVisible;
    procedure UpdateToolImage;
    procedure NoTextureIcon;
    procedure ToggleGridVisible;
    procedure ToggleToolboxVisible;
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
    procedure UpdateTextureIcon;
    procedure LabelAutosize(ALabel: TLabel);
    procedure AskMergeSelection(ACaption: string);
    procedure ReleaseMouseButtons(Shift: TShiftState);
    procedure UpdatePanelPhongShape;
    function ShowOpenTextureDialog: boolean;
    procedure ShowNoPicture;
    function SaveQuestion(ATitle: string): integer;
    function DoLayerMove: boolean;

  public
    { public declarations }
    pictureOrigin, pictureOffset, pictureViewSize, pictureActualSize: TPoint;
    FormBackgroundColor: TColor;
    virtualScreen : TBGRABitmap;
    virtualScreenPenCursor: boolean;
    QueryPaintVirtualScreen: boolean;
    StackNeedUpdate: boolean;

    procedure PaintPictureNow;
    procedure PaintVirtualScreen;
    function TryOpenFile(filename: string): boolean;
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
    procedure ChooseTool(Tool : TPaintToolType);
    procedure ArrangeToolbars;
    function GetPictureArea: TRect;
    procedure PictureSelectionChanged({%H-}sender: TLazPaintImage; AOffsetOnly: boolean);
    procedure SetCurrentFilename(path: string);
    function GetCurrentFilename: string;
    property LazPaintInstance: TLazPaintCustomInstance read FLazPaintInstance write SetLazPaintInstance;
    procedure UpdateEditPicture(ADelayed: boolean = false);
    property CurrentTool: TPaintToolType read GetCurrentTool;
    property ShowSelectionNormal: boolean read FShowSelectionNormal write SetShowSelectionNormal;
  end;

implementation

uses LCLIntf, LCLProc, ugraph, math, umac, uclipboard, ucursors, uobject3D,
   ufilters, ULoadImage;

const PenWidthFactor = 10;

function ApplyShortcutStr(ACaption, AShortcut: string): string;
var idxPar: integer;
begin
  idxPar := Pos('(',ACaption);
  if idxPar <> 0 then
    result := Trim(copy(ACaption,1,idxPar-1))
  else
    result := Trim(ACaption);
  result := result+' ('+AShortcut+')';
end;

procedure ShortcutStr(AAction: TAction; AShortcut: string);
begin
  if AAction.Caption = '' then
    AAction.Caption := ApplyShortcutStr(AAction.Hint, AShortcut)
  else
    AAction.Caption := ApplyShortcutStr(AAction.Caption, AShortcut);
end;

{ TFMain }

procedure TFMain.FormCreate(Sender: TObject);
var i,j: integer;
begin
  initialized := false;

  //layout init
  ScaleDPI(Self,OriginalDPI);
  ScaleImageList(ImageList1,OriginalDPI);
  Zoom := TZoom.Create(Label_CurrentZoom,Edit_Zoom);
  Zoom.OnZoomChanged:= @OnZoomChanged;
  pictureOrigin := Point(0,0);
  ToolbarsHeight := 0;
  virtualScreen := nil;
  previousToolImg:= -1;

  {$IFDEF USEPAINTBOXPICTURE}
  PaintBox_Picture.Left := 0;
  PaintBox_Picture.Width := ClientWidth;
  PaintBox_Picture.Visible := True;
  {$ENDIF}

  {$IFDEF WINDOWS}
    StartDirectory := ExtractFilePath(Application.ExeName);
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
  CheckSpinEdit(SpinEdit_BackOpacity);
  CheckSpinEdit(SpinEdit_PenOpacity);
  CheckSpinEdit(SpinEdit_Tolerance);
  CheckSpinEdit(SpinEdit_Eraser);
  CheckSpinEdit(SpinEdit_PenWidth);
  CheckSpinEdit(SpinEdit_GridNbX);
  CheckSpinEdit(SpinEdit_GridNbY);
  CheckSpinEdit(SpinEdit_TextBlur);
  CheckSpinEdit(SpinEdit_TextShadowX);
  CheckSpinEdit(SpinEdit_TextShadowY);
  CheckSpinEdit(SpinEdit_ShapeAltitude);
  CheckSpinEdit(SpinEdit_PhongBorderSize);
  CheckSpinEdit(SpinEdit_TextureOpacity);

  Label_Coordinates.Caption := '';

  toolbars := TList.Create;
  toolbars.Add(Panel_Embedded);
  toolbars.Add(Panel_File);
  toolbars.Add(Panel_Zoom);
  toolbars.Add(Panel_Undo);
  toolbars.Add(Panel_CopyPaste);
  toolbars.Add(Panel_Coordinates);
  toolbars.Add(Panel_Tool);
  toolbars.Add(Panel_Color);
  toolbars.Add(Panel_Texture);
  toolbars.Add(Panel_Grid);
  toolbars.Add(Panel_PenWidth);
  toolbars.Add(Panel_ShapeOption);
  toolbars.Add(Panel_LineCap);
  toolbars.Add(Panel_JoinStyle);
  toolbars.Add(Panel_PenStyle);
  toolbars.Add(Panel_SplineStyle);
  toolbars.Add(Panel_Eraser);
  toolbars.Add(Panel_Tolerance);
  toolbars.Add(Panel_GradientType);
  toolbars.Add(Panel_Text);
  toolbars.Add(Panel_TextOutline);
  toolbars.Add(Panel_PhongShape);
  toolbars.Add(Panel_Altitude);
  toolbars.Add(Panel_PerspectiveOption);

  for i := 0 to toolbars.Count-1 do
  with TObject(toolbars[i]) as TPanel do
  begin
    Top := 0;
    Height := DoScaleY(26,OriginalDPI);
    Left := -Width;
    Color := clBtnFace;
    for j := 0 to ControlCount-1 do
      if Controls[j] is TToolBar then
        Controls[j].Color := clBtnFace;
  end;

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

  CheckActions(ActionList1);
  ShortcutStr(ToolHand,'H');
  ShortcutStr(ToolPen,'P');
  ShortcutStr(ToolColorPicker,'I');
  ShortcutStr(ToolEraser,'E');
  ShortcutStr(ToolRect,'U');
  ShortcutStr(ToolEllipse,'U''');
  ShortcutStr(ToolPolygon,'D');
  ShortcutStr(ToolSpline,'D''');
  ShortcutStr(ToolFloodfill,'G');
  ShortcutStr(ToolGradient,'G''');
  ShortcutStr(ToolPhong,'G"');
  ShortcutStr(ToolText,'T');
  ShortcutStr(ToolSelectRect,'M');
  ShortcutStr(ToolSelectEllipse,'M''');
  ShortcutStr(ToolSelectPoly,'A');
  ShortcutStr(ToolSelectSpline,'A''');
  ShortcutStr(ToolMoveSelection,'V');
  ShortcutStr(ToolRotateSelection,'V''');
  ShortcutStr(ToolSelectPen,'P');
  ShortcutStr(ToolMagicWand,'W');
  ShortcutStr(ViewZoomIn,'+');
  ShortcutStr(ViewZoomOut,'-');

  for i := 0 to ActionList1.ActionCount-1 do
    with ActionList1.Actions[i] as TAction do
      if (Caption = '') and (Hint <> '') then Caption := Hint;

  AddMenus(MenuFile,   'FileReload,-,FileSave,FileSaveAs,-,FileImport3D,-', 1);
  AddMenus(MenuFile,   'FileNew,FileOpen', 0);
  AddMenus(MenuEdit,   'EditUndo,EditRedo,-,EditCut,EditCopy,EditPaste,EditPasteAsNew,EditDeleteSelection,-,EditSelectAll,EditInvertSelection,EditSelectionFit,EditDeselect');
  AddMenus(MenuSelect, 'EditSelection,FileLoadSelection,FileSaveSelectionAs,-,ToolSelectRect,ToolSelectEllipse,ToolSelectPoly,ToolSelectSpline,-,ToolMoveSelection,ToolRotateSelection,-,ToolSelectPen,ToolMagicWand');
  AddMenus(MenuView,   'ViewZoomOriginal,ViewZoomIn,ViewZoomOut,ViewZoomFit,-', 0);
  AddMenus(MenuImage,  'ImageCrop,ImageCropLayer,ImageFlatten,-,ImageHorizontalFlip,ImageVerticalFlip',0);
  MenuHorizFlipSub.ImageIndex := ImageHorizontalFlip.ImageIndex;
  MenuVertFlipSub.ImageIndex := ImageVerticalFlip.ImageIndex;
  AddMenus(MenuImage,  'ImageChangeCanvasSize,ImageRepeat,-,ImageResample,ImageSmartZoom3,ImageRotateCW,ImageRotateCCW');
  AddMenus(MenuRemoveTransparency, 'ImageClearAlpha,ImageFillBackground');
  AddMenus(MenuFilter, 'FilterBlurMotion,FilterBlurCustom,FilterPixelate,-,FilterSharpen,FilterSmooth,FilterMedian,FilterClearType,FilterClearTypeInverse,FilterFunction,-,FilterContour,FilterEmboss,FilterPhong,-,FilterSphere,FilterTwirl,FilterCylinder');
  AddMenus(MenuRadialBlur,  'FilterBlurFast,FilterBlurRadial,FilterBlurCorona,FilterBlurDisk');
  AddMenus(MenuColors, 'ColorColorize,ColorShiftColors,ColorIntensity,ColorLightness,-,FilterNormalize,FilterNegative,FilterLinearNegative,FilterGrayscale');
  AddMenus(MenuTool,   'ToolHand,-,ToolPen,ToolColorPicker,ToolEraser,-,ToolRect,ToolEllipse,ToolPolygon,ToolSpline,-,ToolFloodFill,ToolGradient,ToolPhong,-,ToolText,ToolDeformation,ToolTextureMapping');
  AddMenus(MenuRender, 'RenderPerlinNoise,RenderCyclicPerlinNoise,-,RenderWater,RenderCustomWater,RenderSnowPrint,RenderWood,RenderWoodVertical,RenderMetalFloor,RenderPlastik,RenderStone,RenderRoundStone,RenderMarble,RenderCamouflage,-,RenderClouds');
  AddMenus(MenuHelp,   'HelpIndex,-,HelpAbout');

  initialized := true;
  FirstPaint := true;
end;

procedure TFMain.AddMenus(AMenu: TMenuItem; AActionsCommaText: string; AIndex: integer = -1);
var actions: TStringList;
  foundAction: TBasicAction;
  item: TMenuItem;
  i: integer;
begin
  actions := TStringList.Create;
  actions.CommaText := AActionsCommaText;
  for i := 0 to actions.Count-1 do
  begin
    item := TMenuItem.Create(nil);
    if trim(actions[i]) = '-' then
      item.Caption := cLineCaption
    else
    begin
      foundAction := ActionList1.ActionByName(trim(actions[i]));
      if foundAction <> nil then
        item.Action := foundAction
      else
        item.Caption := trim(actions[i])+'?';
    end;
    if AIndex = -1 then
      AMenu.Add(item)
    else
    begin
      AMenu.Insert(AIndex,item);
      inc(AIndex);
    end;
  end;
  actions.Free;
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
  ToolManager := LazPaintInstance.ToolManager;
  Image := LazPaintInstance.Image;
  FImageActions := TImageActions.Create(LazPaintInstance);
  Panel_Embedded.Visible := LazPaintInstance.Embedded;
  Panel_File.Visible := not LazPaintInstance.Embedded;
  LazPaintInstance.EmbeddedResult := mrNone;

  Image.OnSelectionChanged := @PictureSelectionChanged;
  SetCurrentFilename('');

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

  ToolManager.SetCurrentToolType(ptHand);
  ToolManager.OnToolChanged := @OnToolChanged;

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
  Tool_TextOutline.Down := ToolManager.ToolTextOutline;
  SpinEdit_TextOutlineWidth.Value := ToolManager.ToolTextOutlineWidth;
  Tool_TextShadow.Down := ToolManager.ToolTextShadow;
  Tool_TextPhong.Down := ToolManager.ToolTextPhong;
  FontDialog1.Font := ToolManager.ToolTextFont;
  SpinEdit_TextBlur.Value := ToolManager.ToolTextBlur;
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

  initialized := true;
  FOnlineUpdater := TLazPaintOnlineUpdater.Create(Config);
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
  begin
    if ToolManager.ToolUpdateNeeded then
      DelayedPaintPicture:= True
    else
      PaintPictureNow;
  end;
  UpdateToolbar;
end;

procedure TFMain.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var BmpPos: TPointF;
    updateForVSCursor: boolean;
    virtualScreenPenCursorBefore: boolean;
begin
  ReleaseMouseButtons(Shift);
  HidePenPreview;
  if LazPaintInstance.TopMostHasFocus then self.SetFocus;

  FormMouseMovePos := Point(X,Y);
  if InFormMouseMove then exit;
  InFormMouseMove := True;
  Application.ProcessMessages; //empty message stack

  BmpPos := FormToBitmap(FormMouseMovePos);
  Label_Coordinates.Caption := IntToStr(round(BmpPos.X))+','+IntToStr(round(BmpPos.Y));
  updateForVSCursor:= false;
  if ToolManager.ToolMove(BmpPos) then
  begin
    PaintPictureNow;
    ToolManager.ToolMoveAfter(FormToBitmap(FormMouseMovePos)); //new BmpPos after repaint
  end else
    updateForVSCursor := true;
  UpdateToolbar;

  virtualScreenPenCursorBefore := virtualScreenPenCursor;
  UpdateCursor(X,Y);
  if updateForVSCursor and (virtualScreenPenCursor or virtualScreenPenCursorBefore) then
    PaintVirtualScreen;

  if ToolManager.ToolSleeping and not spacePressed and not btnLeftDown and not btnRightDown then
    ToolManager.ToolWakeUp;

  InFormMouseMove := False;
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
    if UpdateCursor(X,Y) then redraw := true;
  end;
  if redraw then PaintPictureNow;
  UpdateToolbar;
  ReleaseMouseButtons(Shift);

  if ToolManager.ToolSleeping and not spacePressed and not btnLeftDown and not btnRightDown then
    ToolManager.ToolWakeUp;
end;

procedure TFMain.FileOpenExecute(Sender: TObject);
begin
  LazPaintInstance.HideTopmost;
  if Image.IsFileModified then
  begin
    case SaveQuestion(rsOpen) of
    IDYES: FileSave.Execute;
    IDCANCEL: begin
        LazPaintInstance.ShowTopmost;
        exit;
      end;
    end;
    LazPaintInstance.HideTopmost;
  end;
  OpenPictureDialog1.Options := OpenPictureDialog1.Options - [ofAllowMultiSelect];
  if OpenPictureDialog1.Execute then
    TryOpenFile(OpenPictureDialog1.FileName);
  LazPaintInstance.ShowTopmost;
end;

procedure TFMain.FileQuitExecute(Sender: TObject);
begin
  Close;
end;

procedure TFMain.FileSaveAsExecute(Sender: TObject);
var filename: string;
begin
  AskMergeSelection(rsSave);
  filename := ExtractFileName(GetCurrentFilename);
  if filename = '' then filename := rsNoName;
  if not Image.AbleToSaveAsSys(UTF8ToSys(filename)) then filename := ChangeFileExt(Filename,'.png');
  SavePictureDialog1.FileName := filename;
  LazPaintInstance.HideTopmost;
  if SavePictureDialog1.Execute then
  begin
    filename := SavePictureDialog1.FileName;
    if not Image.AbleToSaveAsSys(UTF8ToSys(filename)) then
      ShowMessage(rsFileExtensionNotSupported) else
    begin
      try
        Image.SaveToFileSys(UTF8ToSys( filename ));
        SetCurrentFilename(filename);
      except
        on ex: Exception do
          MessageDlg(ex.Message,mtError,[mbOk],0);
      end;
    end;
  end;
  LazPaintInstance.ShowTopmost;
end;

procedure TFMain.FileSaveExecute(Sender: TObject);
begin
  if (GetCurrentFilename = '') or not Image.AbleToSaveAsSys(UTF8ToSys(GetCurrentFilename)) then
    FileSaveAs.Execute else
    begin
      AskMergeSelection(rsSave);
      try
        Image.SaveToFileSys( UTF8ToSys( GetCurrentFilename ));
      except
        on ex: Exception do
          MessageDlg(ex.Message,mtError,[mbOk],0);
      end;
    end;
end;

procedure TFMain.FileSaveUpdate(Sender: TObject);
begin
   FileSave.Enabled := image.IsFileModified;
end;

procedure TFMain.FilterAnyExecute(Sender: TObject);
var filterName: string;
begin
  if Sender is TAction then
  begin
    filterName := (Sender as TAction).Name;
    if (length(filterName) >= 7) and (copy(filterName,1,6) = 'Filter') and
        (filterName[7] = upcase(filterName[7])) then
          delete(filterName,1,6);
    ExecuteFilter(LazPaintInstance, StrToPictureFilter(filterName));
  end;
end;

procedure TFMain.RenderAnyExecute(Sender: TObject);
var filterName: string;
begin
  if Sender is TAction then
  begin
    filterName := (Sender as TAction).Name;
    if (length(filterName) >= 7) and (copy(filterName,1,6) = 'Render') and
        (filterName[7] = upcase(filterName[7])) then
          delete(filterName,1,6);
    ExecuteFilter(LazPaintInstance, StrToPictureFilter(filterName));
  end;
end;

procedure TFMain.ToolAnyExecute(Sender: TObject);
var toolName: string;
begin
  if Sender is TAction then
  begin
    toolName := (Sender as TAction).Name;
    if (length(toolName) >= 5) and (copy(toolName,1,4) = 'Tool') and
        (toolName[5] = upcase(toolName[5])) then
          delete(toolName,1,4);
    ChooseTool(StrToPaintToolType(toolName));
  end;
end;

procedure TFMain.SetCurrentFilename(path: string);
begin
  image.CurrentFilename := path;
  if path = '' then
    self.Caption := LazPaintInstance.Title
  else
    self.Caption := LazPaintInstance.Title + ' - ' + path;
end;

function TFMain.GetCurrentFilename: string;
begin
  result := image.CurrentFilename;
end;

procedure TFMain.FormDestroy(Sender: TObject);
begin
  if ToolManager.OnToolChanged = @OnToolChanged then
    ToolManager.OnToolChanged := nil;
  ForgetSelectionHightlight;
  FreeAndNil(Zoom);
  FreeAndNil(virtualScreen);
  FreeAndNil(toolbars);
  FreeAndNil(FImageActions);
  FreeAndNil(FOnlineUpdater);
end;

procedure TFMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  if Zoom.EditingZoom then exit;
  if Key = VK_ADD then
  begin
     ViewZoomIn.Execute;
     Key := 0;
  end else
  if Key = VK_SUBTRACT then
  begin
     ViewZoomOut.Execute;
     Key := 0;
  end else
  if ToolManager.ToolKeyDown(Key) then
  begin
    DelayedPaintPicture := True;
    Key := 0;
  end else
  if Key = VK_F6 then
  begin
    ToggleToolwindowsVisible;
    Key := 0;
  end else
  If (Key = VK_SPACE) and not spacePressed then
  begin
    spacePressed:= true;
    if not btnLeftDown and not btnRightDown then ToolManager.ToolSleep;
  end;
end;

procedure TFMain.FormResize(Sender: TObject);
begin
  if shouldArrangeOnResize then
    ArrangeToolbars;
end;

procedure TFMain.ImageSmartZoom3Execute(Sender: TObject);
begin
  FImageActions.SmartZoom3;
end;

procedure TFMain.ViewZoomInExecute(Sender: TObject);
begin
  Zoom.ZoomIn;
end;

procedure TFMain.ViewZoomOriginalExecute(Sender: TObject);
begin
  Zoom.ZoomOriginal;
end;

procedure TFMain.EditUndoExecute(Sender: TObject);
begin
  FImageActions.Undo;
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
begin
  LazPaintInstance.HideTopmost;
  if Open3DObjectDialog.Execute then
    FImageActions.Import3DObject(UTF8ToSys(Open3DObjectDialog.FileName));
  LazPaintInstance.ShowTopmost;
end;

procedure TFMain.FileLoadSelectionExecute(Sender: TObject);
begin
  LazPaintInstance.HideTopmost;
  if LoadSelectionDialog.Execute then
    if FImageActions.LoadSelection(UTF8ToSys(LoadSelectionDialog.Filename)) then
      SaveSelectionDialog.Filename := LoadSelectionDialog.Filename;
  LazPaintInstance.ShowTopmost;
end;

procedure TFMain.FileReloadExecute(Sender: TObject);
begin
  if Image.IsFileModified then
  begin
    LazPaintInstance.HideTopmost;
    case MessageDlg(rsReload,rsReloadChanged,mtWarning,mbYesNo,0) of
    IDYES: begin
             TryOpenFile(GetCurrentFilename);
           end;
    end;
    LazPaintInstance.ShowTopmost;
  end else
    TryOpenFile(GetCurrentFilename);
end;

procedure TFMain.FileReloadUpdate(Sender: TObject);
begin
  FileReload.Enabled := (GetCurrentFilename <> '');
end;

procedure TFMain.FileSaveSelectionAsExecute(Sender: TObject);
var filename: string;
begin
  if Image.SelectionEmpty then exit;
  LazPaintInstance.HideTopmost;
  if SaveSelectionDialog.Execute then
  begin
    filename := SaveSelectionDialog.FileName;
    if not Image.AbleToSaveSelectionAsSys(UTF8ToSys(filename)) then
      ShowMessage(rsFileExtensionNotSupported) else
    begin
      try
        Image.SaveSelectionToFile(UTF8ToSys( filename ));
      except
        on ex: Exception do
          MessageDlg(ex.Message,mtError,[mbOk],0);
      end;
    end;
  end;
  LazPaintInstance.ShowTopmost;
end;

procedure TFMain.FileSaveSelectionAsUpdate(Sender: TObject);
begin
  FileSaveSelectionAs.Enabled := not Image.SelectionEmpty;
end;

procedure TFMain.FormDropFiles(Sender: TObject; const FileNames: array of String);
var i: integer;
begin
  if length(FileNames) > 0 then
  begin
    TryOpenFile(FileNames[0]);
    for i := 1 to high(FileNames) do
      FImageActions.TryAddLayerFromFile(FileNames[i]);
  end;
end;

procedure TFMain.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if ToolManager.ToolKeyUp(Key) then
  begin
    DelayedPaintPicture := True;
    Key := VK_UNKNOWN;
  end else
  If Key = VK_SPACE then
  begin
    spacePressed:= false;
    if ToolManager.ToolSleeping and not spacePressed and not btnRightDown and not btnLeftDown then
      ToolManager.ToolWakeUp;
  end;
end;

procedure TFMain.FormMouseEnter(Sender: TObject);
begin
  Image.PrepareForRendering;
end;

procedure TFMain.FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  if Zoom.EditingZoom then exit;
  if ToolManager.ToolKeyPress(UTF8Key) then
  begin
    UTF8Key := '';
    if ToolManager.ToolUpdateNeeded then
      DelayedPaintPicture := true
    else
      PaintPictureNow;
  end else
  begin
    case UTF8Key of
    'P','p': if (CurrentTool = ptPen) or (ToolManager.IsSelectingTool and (CurrentTool <> ptSelectPen)) then ChooseTool(ptSelectPen)
                else ChooseTool(ptPen);
    'M','m': if CurrentTool = ptSelectRect then ChooseTool(ptSelectEllipse)
                else ChooseTool(ptSelectRect);
    'I','i': ChooseTool(ptColorPicker);
    'W','w': ChooseTool(ptMagicWand);
    'V','v': if CurrentTool = ptMoveSelection then ChooseTool(ptRotateSelection)
               else ChooseTool(ptMoveSelection);
    'E','e': ChooseTool(ptEraser);
    'T','t': ChooseTool(ptText);
    'G','g': if CurrentTool = ptFloodFill then ChooseTool(ptGradient) else
             if CurrentTool = ptGradient then ChooseTool(ptPhong) else
              ChooseTool(ptFloodFill);
    'U','u': if CurrentTool = ptRect then ChooseTool(ptEllipse) else
                ChooseTool(ptRect);
    'X','x': SwitchColors;
    'H','h': ChooseTool(ptHand);
    'A','a': If CurrentTool = ptSelectPoly then ChooseTool(ptSelectSpline)
               else ChooseTool(ptSelectPoly);
    'D','d': if CurrentTool = ptPolygon then ChooseTool(ptSpline)
              else ChooseTool(ptPolygon);
    end;
  end;
end;

procedure TFMain.ImageCropLayerExecute(Sender: TObject);
begin
  FImageActions.CropToSelectionAndLayer;
end;

procedure TFMain.ImageCropLayerUpdate(Sender: TObject);
begin
  ImageCropLayer.Enabled := not image.SelectionEmpty;
  ImageCropLayer.Visible := (image.NbLayers > 1);
end;

procedure TFMain.ImageFlattenExecute(Sender: TObject);
begin
  ChooseTool(ptHand);
  image.Flatten;
end;

procedure TFMain.ImageFlattenUpdate(Sender: TObject);
begin
  ImageFlatten.Enabled := Image.NbLayers > 1;
end;

procedure TFMain.LayerAddNewExecute(Sender: TObject);
begin
  LazPaintInstance.NewLayer;
end;

procedure TFMain.LayerAddNewUpdate(Sender: TObject);
begin
  LayerAddNew.Enabled := LazPaintInstance.Image.NbLayers < MaxLayersToAdd;
end;

procedure TFMain.LayerDuplicateExecute(Sender: TObject);
begin
  LazPaintInstance.DuplicateLayer;
end;

procedure TFMain.LayerDuplicateUpdate(Sender: TObject);
begin
  LayerDuplicate.Enabled := LazPaintInstance.Image.NbLayers < MaxLayersToAdd;
end;

procedure TFMain.LayerFromFileExecute(Sender: TObject);
var i: integer;
begin
  if not image.SelectionLayerIsEmpty then
  begin
    Messagedlg(rsMustReleaseSelection,mtInformation,[mbOK],0);
    exit;
  end;
  LazPaintInstance.HideTopmost;
  OpenPictureDialog1.Options := OpenPictureDialog1.Options + [ofAllowMultiSelect];
  if OpenPictureDialog1.Execute then
  begin
    for i := 0 to OpenPictureDialog1.Files.Count-1 do
      FImageActions.TryAddLayerFromFile(OpenPictureDialog1.Files[i]);
  end;
  LazPaintInstance.ShowTopmost;
end;

procedure TFMain.LayerMergeOverExecute(Sender: TObject);
begin
  LazPaintInstance.MergeLayerOver;
end;

procedure TFMain.LayerMergeOverUpdate(Sender: TObject);
begin
  LayerMergeOver.Enabled := (image.currentImageLayerIndex > 0) and Image.CurrentLayerVisible;
end;

procedure TFMain.LayerMoveExecute(Sender: TObject);
begin
  if DoLayerMove then
    ChooseTool(ptMoveSelection);
end;

procedure TFMain.LayerMoveUpdate(Sender: TObject);
begin
  LayerMove.Enabled := Image.CurrentLayerVisible;
end;

procedure TFMain.LayerRemoveCurrentExecute(Sender: TObject);
begin
  LazPaintInstance.RemoveLayer;
end;

procedure TFMain.LayerRemoveCurrentUpdate(Sender: TObject);
begin
  LayerRemoveCurrent.Enabled := LazPaintInstance.Image.NbLayers > 1;
end;

procedure TFMain.LayerRotateExecute(Sender: TObject);
begin
  if DoLayerMove then ChooseTool(ptRotateSelection);
end;

procedure TFMain.LayerRotateUpdate(Sender: TObject);
begin
  LayerRotate.Enabled := Image.CurrentLayerVisible;
end;

procedure TFMain.ItemDonateClick(Sender: TObject);
begin
  OpenURL('http://sourceforge.net/donate/index.php?group_id=404555');
end;

procedure TFMain.ImageHorizontalFlipExecute(Sender: TObject);
begin
  FImageActions.HorizontalFlip(foAuto);
end;

procedure TFMain.LayerHorizontalFlipExecute(Sender: TObject);
begin
  FImageActions.HorizontalFlip(foCurrentLayer);
end;

procedure TFMain.ItemHorizFlipLayerClick(Sender: TObject);
begin
  FImageActions.HorizontalFlip(foCurrentLayer);
end;

procedure TFMain.ItemHorizFlipPictureClick(Sender: TObject);
begin
  FImageActions.HorizontalFlip(foWholePicture);
end;

procedure TFMain.ItemHorizFlipSelectionClick(Sender: TObject);
begin
  FImageActions.HorizontalFlip(foSelection);
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

procedure TFMain.ImageVerticalFlipExecute(Sender: TObject);
begin
  FImageActions.VerticalFlip(foAuto);
end;

procedure TFMain.LayerVerticalFlipExecute(Sender: TObject);
begin
  FImageActions.VerticalFlip(foCurrentLayer);
end;

procedure TFMain.ItemVertFlipLayerClick(Sender: TObject);
begin
  FImageActions.VerticalFlip(foCurrentLayer);
end;

procedure TFMain.ItemVertFlipPictureClick(Sender: TObject);
begin
  FImageActions.VerticalFlip(foWholePicture);
end;

procedure TFMain.ItemVertFlipSelectionClick(Sender: TObject);
begin
  FImageActions.VerticalFlip(foSelection);
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
begin
  bmp := TBGRABitmap.Create(PaintBox_PenPreview.Width,PaintBox_PenPreview.Height,ColorToBGRA(ColorToRGB(clBtnFace)));
  bmp.FillEllipseAntialias(round(PaintBox_PenPreview.Width/2),round(PaintBox_PenPreview.Height/2),
    ToolManager.ToolPenWidth/2,ToolManager.ToolPenWidth/2,ColorToBGRA(ColorToRGB(clBtnText)));
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
begin
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
    LazPaintInstance.HideTopmost;

    case SaveQuestion(rsExitRequest) of
    IDYES: FileSave.Execute;
    IDNO: ;
    IDCANCEL: begin
                CanClose := false;
                LazPaintInstance.RestartQuery := false;
                LazPaintInstance.ShowTopmost;
              end;
    end;
  end else
  if LazPaintInstance.Embedded and (LazPaintInstance.EmbeddedResult = mrNone) and
    ((LazPaintInstance.EmbeddedImageBackup <> nil) or not image.Empty) and (not image.FlatImageEquals(LazPaintInstance.EmbeddedImageBackup)) then
  begin
    LazPaintInstance.HideTopmost;
    case MessageDlg(rsCloseRequest,rsKeepChanges,mtConfirmation,mbYesNoCancel,0) of
    IDYES: LazPaintInstance.EmbeddedResult := mrOk;
    IDNO: LazPaintInstance.EmbeddedResult := mrCancel;
    IDCANCEL: begin
                CanClose := false;
                LazPaintInstance.RestartQuery := false;
                LazPaintInstance.ShowTopmost;
              end;
    end;
  end;
end;

procedure TFMain.FormHide(Sender: TObject);
var r: TRect;
begin
  shouldArrangeOnResize := false;
  LazPaintInstance.HideTopmost;
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

procedure TFMain.ImageClearAlphaExecute(Sender: TObject);
begin
  FImageActions.ClearAlpha;
end;

procedure TFMain.ImageCropExecute(Sender: TObject);
begin
  FImageActions.CropToSelection;
end;

procedure TFMain.ImageCropUpdate(Sender: TObject);
begin
  ImageCrop.Enabled := not image.SelectionEmpty;
end;

procedure TFMain.ImageFillBackgroundExecute(Sender: TObject);
begin
  FImageActions.FillBackground;
end;

procedure TFMain.ImageRepeatExecute(Sender: TObject);
begin
  LazPaintInstance.ShowRepeatImageDlg;
end;

procedure TFMain.ImageRotateCCWExecute(Sender: TObject);
begin
  FImageActions.RotateCCW;
end;

procedure TFMain.ImageRotateCWExecute(Sender: TObject);
begin
  FImageActions.RotateCW;
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
     if Config.RecentFile[i]<>GetCurrentFilename then
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
begin
  if Sender is TMenuItem then
  begin
    LazPaintInstance.HideTopmost;
    if Image.IsFileModified then
    begin
      case SaveQuestion(rsOpen) of
      IDYES: FileSave.Execute;
      IDCANCEL: begin
          LazPaintInstance.ShowTopmost;
          exit;
        end;
      end;
      LazPaintInstance.HideTopmost;
    end;
    TryOpenFile((sender as TMenuItem).Caption);
    LazPaintInstance.ShowTopmost;
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
  if CanCompressOrUpdateStack and StackNeedUpdate then
  begin
    LazPaintInstance.NotifyStackChange;
    StackNeedUpdate := false;
  end else
  begin
    if CanCompressOrUpdateStack then image.CompressUndo;
  end;
  if DelayedPaintPicture then
  begin
    DelayedPaintPicture := false;
    if ToolManager.ToolUpdateNeeded then
      ToolManager.ToolUpdate;
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
  ToolLayerMapping.Enabled := Image.CurrentLayerVisible;
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
      ShowMessage('ToolNoTextureExecute: '+ex.Message);
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
begin
  FontDialog1.Font.Assign(ToolManager.ToolTextFont);
  FontDialog1.Font.Color := BGRAToColor(ToolManager.ToolForeColor);
  LazPaintInstance.HideTopmost;
  if FontDialog1.Execute then
  begin
    ToolManager.ToolTextFont.Assign(FontDialog1.Font);
    if FontDialog1.Font.Color <> BGRAToColor(ToolManager.ToolForeColor) then
    begin
      ToolManager.ToolForeColor := ColorToBGRA(FontDialog1.Font.Color,ToolManager.ToolForeColor.alpha);
      Shape_PenColor.Brush.Color := BGRAToColor(ToolManager.ToolForeColor);
      LazPaintInstance.ColorToFChooseColor;
    end;
    UpdateEditPicture;
  end;
  LazPaintInstance.ShowTopmost;
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

procedure TFMain.ViewToolboxExecute(Sender: TObject);
begin
  ToggleToolboxVisible;
end;

procedure TFMain.ViewGridExecute(Sender: TObject);
begin
  ToggleGridVisible;
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

procedure TFMain.ViewZoomFitExecute(Sender: TObject);
begin
  Zoom.ZoomFit(Image.Width,Image.Height,GetPictureArea);
end;

procedure TFMain.EditRedoExecute(Sender: TObject);
begin
  FImageActions.Redo;
end;

procedure TFMain.EditInvertSelectionExecute(Sender: TObject);
begin
  FImageActions.InvertSelection;
end;

procedure TFMain.EditPasteAsNewExecute(Sender: TObject);
var bmp: TBGRABitmap;
begin
  bmp := GetBitmapFromClipboard;
  if bmp <> nil then
  begin
    if bmp.NbPixels > 0 then
    begin
      ChooseTool(ptHand);
      if Image.IsFileModified then
      begin
        LazPaintInstance.HideTopmost;
        case SaveQuestion(rsNewImage) of
        IDYES: FileSave.Execute;
        IDCANCEL: begin
            LazPaintInstance.ShowTopmost;
            bmp.Free;
            exit;
          end;
        end;
        LazPaintInstance.ShowTopmost;
      end;
      image.Assign(bmp,true,false);
      SetCurrentFilename('');
      image.SetSavedFlag;
    end
     else
       bmp.Free;
  end;
end;

procedure TFMain.EditDeselectExecute(Sender: TObject);
begin
  FImageActions.Deselect;
end;

procedure TFMain.ChooseTool(Tool: TPaintToolType);
var LayerAction: TLayerAction;
begin
  try
    ToolManager.ToolCloseDontReopen;
    if self.Visible then
    begin
      case Tool of
        ptTextureMapping:
          if (ToolManager.GetToolTexture = nil) or ToolManager.GetToolTexture.Empty then
          begin
            if not ShowOpenTextureDialog then
              Tool := ptHand
            else
            if (ToolManager.GetToolTexture = nil) or ToolManager.GetToolTexture.Empty then
            begin
              Tool := ptHand;
            end;
          end;
        ptLayerMapping:
        begin
          EditDeselect.Execute;
          if image.SelectedLayerEmpty then
          begin
            MessageDlg(rsEmptyLayer,mtInformation,[mbOK],0);
            Tool := ptHand;
          end;
        end;
        ptDeformation:
        begin
          if image.SelectedLayerEquals(image.SelectedLayerPixel[0,0]) then
          begin
            LazPaintInstance.HideTopmost;
            ShowMessage(rsNothingToBeDeformed);
            LazPaintInstance.ShowTopmost;
            Tool := ptHand;
          end;
        end;
        ptMoveSelection,ptRotateSelection:
        begin
          if image.CurrentLayerVisible and not image.SelectionEmpty and image.SelectionLayerIsEmpty and not image.SelectedLayerEmpty then
          begin
            LazPaintInstance.HideTopmost;
            case MessageDlg(rsMovingOrRotatingSelection,rsRetrieveSelectedArea,mtConfirmation,[mbYes,mbNo],0) of
              mrYes: begin
                  LayerAction := TLayerAction.Create(Image);
                  LayerAction.RetrieveSelectionIfLayerEmpty(True);
                  LayerAction.Validate;
                  LayerAction.Free;
                  if image.SelectionLayerIsEmpty then
                    ShowMessage(rsNothingToBeRetrieved);
              end;
            end;
            LazPaintInstance.ShowTopmost;
          end;
        end;
      end;
    end;
    ToolManager.SetCurrentToolType(Tool);
  except
    on ex:Exception do
      ShowMessage('ChooseTool: '+ex.Message);
  end;
end;

procedure TFMain.ArrangeToolbars;
var i,curx,cury,maxh: integer; tb: TControl;
begin
     curx := 0;
     cury := 0;
     maxh := 0;
     for i := 0 to toolbars.Count-1 do
     begin
       tb := TObject(toolbars[i]) as TControl;
       if tb.Visible then
       begin
         if curx+tb.Width > ClientWidth then
         begin
           curx := 0;
           cury += maxh;
           maxh := 0;
         end;
         tb.Left := curx;
         tb.Top := cury;
         inc(curx, tb.Width);
         if tb.Height > maxh then maxh := tb.Height;
       end;
     end;
     if curx <> 0 then ToolbarsHeight := cury+maxh else ToolbarsHeight := cury;
     if toolbarsHeight = 0 then
     begin
       Panel_ToolbarBackground.Visible := false;
       {$IFDEF USEPAINTBOXPICTURE}
       PaintBox_Picture.Top := 0;
       PaintBox_Picture.Height := ClientHeight;
       {$ENDIF}
     end else
     begin
       Panel_ToolbarBackground.Top := 0;
       Panel_ToolbarBackground.Left := 0;
       Panel_ToolbarBackground.width := Clientwidth;
       Panel_ToolbarBackground.Height := ToolbarsHeight;
       Panel_ToolbarBackground.Visible := true;
       {$IFDEF USEPAINTBOXPICTURE}
       PaintBox_Picture.Top := ToolbarsHeight;
       PaintBox_Picture.Height := ClientHeight-ToolbarsHeight;
       {$ENDIF}
     end;
     for i := 0 to toolbars.Count-1 do
       (TObject(toolbars[i]) as TControl).Repaint;
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
  ViewGrid.Checked := not ViewGrid.Checked;
  ItemViewGrid.Checked := ViewGrid.Checked;
  LazPaintInstance.GridVisible := ViewGrid.Checked;
end;

procedure TFMain.ToggleToolboxVisible;
begin
  ViewToolBox.Checked := not ViewToolBox.Checked;
  ItemViewToolbox.Checked := ViewToolBox.Checked;
  LazPaintInstance.ToolboxVisible := ViewToolBox.Checked;
end;

procedure TFMain.ToggleColorsVisible;
begin
  ViewColors.Checked := not ViewColors.Checked;
  ItemViewColors.Checked := ViewColors.Checked;
  LazPaintInstance.ChooseColorVisible := ViewColors.Checked;
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

procedure TFMain.EditCopyExecute(Sender: TObject);
begin
  FImageActions.CopySelection;
end;

procedure TFMain.ColorIntensityExecute(Sender: TObject);
begin
  if not image.CheckCurrentLayerVisible then exit;
  LazPaintInstance.ShowColorIntensityDlg;
end;

procedure TFMain.ColorShiftColorsExecute(Sender: TObject);
begin
  if not image.CheckCurrentLayerVisible then exit;
  LazPaintInstance.ShowShiftColorsDlg;
end;

procedure TFMain.ColorColorizeExecute(Sender: TObject);
begin
  if not image.CheckCurrentLayerVisible then exit;
  LazPaintInstance.ShowColorizeDlg;
end;

procedure TFMain.ColorLightnessExecute(Sender: TObject);
begin
  if not image.CheckCurrentLayerVisible then exit;
  LazPaintInstance.ShowColorLightnessDlg;
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
  end;
end;

procedure TFMain.EditCutExecute(Sender: TObject);
begin
  FImageActions.CutSelection;
end;

procedure TFMain.EditDeleteSelectionExecute(Sender: TObject);
begin
  FImageActions.DeleteSelection;
end;

procedure TFMain.EditPasteExecute(Sender: TObject);
begin
  FImageActions.Paste;
end;

procedure TFMain.EditPasteUpdate(Sender: TObject);
begin
  EditPaste.Enabled := Image.CurrentLayerVisible;
end;

procedure TFMain.EditDeselectUpdate(Sender: TObject);
begin
  EditDeselect.Enabled := not image.SelectionEmpty;
  EditDeleteSelection.Enabled := EditDeselect.Enabled;
  EditCopy.Enabled := EditDeselect.Enabled;
  EditCut.Enabled := EditDeselect.Enabled;
  ToolRotateSelection.Enabled := EditDeselect.Enabled;
  ToolMoveSelection.Enabled := EditDeselect.Enabled;
end;

procedure TFMain.EditRedoUpdate(Sender: TObject);
begin
  EditRedo.Enabled := image.CanRedo;
end;

procedure TFMain.EditSelectAllExecute(Sender: TObject);
begin
  FImageActions.SelectAll;
end;

procedure TFMain.EditSelectionExecute(Sender: TObject);
begin
  LazPaintInstance.EditSelection;
end;

procedure TFMain.EditSelectionFitExecute(Sender: TObject);
begin
  FImageActions.SelectionFit;
end;

procedure TFMain.ViewZoomOutExecute(Sender: TObject);
begin
  Zoom.ZoomOut;
end;

procedure TFMain.SpinEdit_EraserChange(Sender: TObject);
begin
    if initialized then
    begin
      if ToolManager.ToolEraserAlpha = SpinEdit_Eraser.value then exit;
      ToolManager.ToolEraserAlpha := SpinEdit_Eraser.Value;
    end;
end;

procedure TFMain.FileNewExecute(Sender: TObject);
var
  bitmapRepl: TBGRABitmap;
begin
  LazPaintInstance.HideTopmost;
  if Image.IsFileModified then
  begin
    case SaveQuestion(rsNewImage) of
    IDYES: FileSave.Execute;
    IDCANCEL: begin
                LazPaintInstance.ShowTopmost;
                exit;
              end;
    end;
    LazPaintInstance.ShowTopmost;
  end;
  if LazPaintInstance.ShowNewImageDlg(bitmapRepl) then
  begin
    ChooseTool(ptHand);
    image.Assign(bitmapRepl, True, False);
    SetCurrentFilename('');
    image.SetSavedFlag;
  end;
  LazPaintInstance.ShowTopmost;
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
begin
  if not image.SelectionEmpty and not image.SelectionLayerIsEmpty then
  begin
    case MessageDlg(ACaption,rsMergeSelection,mtConfirmation,[mbYes,mbNo],0) of
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

function TFMain.ShowOpenTextureDialog: boolean;
var newTex: TBGRABitmap;
  finalFilename: string;
begin
  result := false;
  try
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
      except
        on ex:Exception do
          ShowMessage(ex.Message);
      end;
    end;
  except
    on ex:Exception do
      ShowMessage('ToolLoadTextureExecute: '+ex.Message);
  end;
end;

procedure TFMain.ShowNoPicture;
begin
  InShowNoPicture := true;
  PaintPictureNow;
  InShowNoPicture:= false;
end;

function TFMain.SaveQuestion(ATitle: string): integer;
begin
  result := MessageDlg(ATitle,rsSaveChanges,mtWarning,mbYesNoCancel,0);
end;

function TFMain.DoLayerMove: boolean;
var LayerAction: TLayerAction;
    LayerBounds: TRect;
begin
  result := false;
  ChooseTool(ptHand);

  LayerAction := TLayerAction.Create(Image);
  if not Image.SelectionEmpty then
    LayerAction.ReleaseSelection;
  if not Image.SelectedLayerEmpty then
  begin
    LayerAction.QuerySelection;
    LayerBounds := Image.SelectedImageLayerReadOnly.GetImageBounds;
    LayerAction.CurrentSelection.FillRect(LayerBounds,BGRAWhite,dmSet);
    LayerAction.RetrieveSelection;
    LayerAction.SelectedImageLayer.Fill(BGRAPixelTransparent);
    Image.ImageMayChangeCompletely;
    LayerAction.Validate;
    LayerAction.Free;
    result := true;
  end else
  begin
    LayerAction.Free;
    MessageDlg(rsEmptyLayer,mtInformation,[mbOK],0);
  end;
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
    ArrangeToolbars;
    Image.OnImageChanged.NotifyObservers;
    UpdateToolBar;
  end;
end;

procedure TFMain.OnQueryExitToolHandler(sender: TLazPaintImage);
begin
  ChooseTool(ptHand);
end;

function TFMain.TryOpenFile(filename: string): boolean;
var
  newPicture: TBGRABitmap;
  finalFilename,ext: string;

  procedure StartImport;
  begin
    ToolManager.ToolCloseDontReopen;
    if CurrentTool in [ptDeformation,ptRotateSelection,ptMoveSelection,ptTextureMapping,ptLayerMapping] then
      ChooseTool(ptHand);
    Config.AddRecentFile(filename);
    SetCurrentFilename(finalFilename);
  end;
  procedure EndImport;
  begin
    image.ClearUndo;
    image.SetSavedFlag;
    ToolManager.ToolOpen;
    result := true;
  end;

begin
  result := false;
  if filename = '' then exit;
  ShowNoPicture;
  if not AbleToLoadUTF8(filename) then
  begin
    ShowMessage(rsFileExtensionNotSupported);
    exit;
  end;
  finalFilename := filename;
  try
    ext := UTF8LowerCase(ExtractFileExt(Filename));
    if (ext='.ico') or (ext='.gif') then
    begin
      newPicture := LoadFlatImageUTF8(Filename, finalFilename, '.png');
    end else
    begin
      StartImport;
      image.LoadFromFileSys(UTF8ToSys(filename));
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
      MessageDlg(ex.Message,mtError,[mbOk],0);
    end;
  end;
end;

procedure TFMain.FormShow(Sender: TObject);
var r: TRect;
begin
  //strange position fix
  SpinEdit_PenOpacity.Top := SpinEdit_PenWidth.Top;
  SpinEdit_BackOpacity.Top := SpinEdit_PenWidth.Top;

  LazPaintInstance.ColorToFChooseColor;
  LazPaintInstance.ShowTopmost;
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
end;

procedure TFMain.ImageResampleExecute(Sender: TObject);
begin
  LazPaintInstance.ShowResampleDialog;
end;

procedure TFMain.Image_SwapColorsMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  SwitchColors;
end;

procedure TFMain.SpinEdit_PenWidthChange(Sender: TObject);
begin
  if initialized then
  begin
    if ToolManager.ToolPenWidth = SpinEdit_PenWidth.Value/PenWidthFactor then exit;
    ToolManager.ToolPenWidth := SpinEdit_PenWidth.Value/PenWidthFactor;
    ShowPenPreview(True);
    UpdateEditPicture;
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
begin
  if Shape_PenColor.Brush.Color <> BGRAToColor(ToolManager.ToolForeColor) then
    Shape_PenColor.Brush.Color := BGRAToColor(ToolManager.ToolForeColor);
  if SpinEdit_PenOpacity.Value <> ToolManager.ToolForeColor.alpha then
    SpinEdit_PenOpacity.Value := ToolManager.ToolForeColor.alpha;

  if Shape_BackColor.Brush.Color <> BGRAToColor(ToolManager.ToolBackColor) then
    Shape_BackColor.Brush.Color := BGRAToColor(ToolManager.ToolBackColor);
  if SpinEdit_BackOpacity.Value <> ToolManager.ToolBackColor.alpha then
    SpinEdit_BackOpacity.Value := ToolManager.ToolBackColor.alpha;

  Label_CurrentDiff.Caption := inttostr(round(BGRAWordDiff(ToolManager.ToolForeColor,ToolManager.ToolBackColor)/65535*100))+'%';

  maxGridNbX := Max(2,Min(image.Width div 2,15));
  maxGridNbY := Max(2,Min(image.Height div 2,15));
  if SpinEdit_GridNbX.MaxValue <> maxGridNbX then
    SpinEdit_GridNbX.MaxValue := maxGridNbX;
  if SpinEdit_GridNbY.MaxValue <> maxGridNbY then
    SpinEdit_GridNbY.MaxValue := maxGridNbY;

  SpinEdit_TextOutlineWidth.Enabled := not ToolManager.ToolTextPhong;

  LazPaintInstance.ColorToFChooseColor;
  UpdateToolImage;
end;

procedure TFMain.ToolMoveSelectionUpdate(Sender: TObject);
begin
  //see EditDeselect
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
       LazPaintInstance.MoveLayerWindowTo(r.left,r.top)
     else
       LazPaintInstance.MoveLayerWindowTo(self.Left+self.Width-LazPaintInstance.LayerWindowWidth,
                          self.Top+self.Height-LazPaintInstance.ChooseColorHeight-LazPaintInstance.LayerWindowHeight);
     if Config.DefaultLayerWindowVisible then ToggleLayersVisible;

     FirstPaint := false;
   end;
   if InFormPaint then exit;
   InFormPaint := true;
   if QueryPaintVirtualScreen then
     PaintVirtualScreenImplementation
   else
     PaintPictureImplementation;
   PaintBlueAreaImplementation;

   InFormPaint := false;
end;

procedure TFMain.OnImageChangedHandler(AEvent: TLazPaintImageObservationEvent);
begin
  {$IFDEF USEPAINTBOXPICTURE}
     PaintBox_Picture.Invalidate;
  {$ELSE}
    Invalidate;
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
  UpdateToolbar;
  PaintPictureNow;
end;

procedure TFMain.PaintPictureNow;
begin
  if not visible then exit;
  DelayedPaintPicture := False;
  StackNeedUpdate := true;
  Image.OnImageChanged.NotifyObservers;
  {$IFDEF USEPAINTBOXPICTURE}
    PaintBox_Picture.Update;
  {$ELSE}
    self.Update;
  {$ENDIF}
end;

function TFMain.RetrieveSelectionHighlight(pFormArea: TRect;
  pImageOffset: TPoint; pSelectionRotateAngle: single; pSelectionRotateCenter: TPointF; pZoomFactor: single; selecting: boolean; out pHighlightOffset: TPoint): TBGRABitmap;
begin
  if (selectionHighlightInfo.zoomFactor = pZoomFactor) and
     (((selectionHighlightInfo.zoomFactor = 1) and (selectionHighlightInfo.selectionRotateAngle=0)) or
     ((selectionHighlightInfo.formArea.Left = pFormArea.Left) and
     (selectionHighlightInfo.formArea.Right = pFormArea.Right) and
     (selectionHighlightInfo.formArea.Top = pFormArea.Top) and
     (selectionHighlightInfo.formArea.Bottom = pFormArea.Bottom) and
     (selectionHighlightInfo.ImageOffset.X = pImageOffset.X) and
     (selectionHighlightInfo.ImageOffset.Y = pImageOffset.Y))) and
     (selectionHighlightInfo.selecting = selecting) and
     (selectionHighlightInfo.selectionRotateAngle = pSelectionRotateAngle) and
     ((selectionHighlightInfo.selectionRotateAngle = 0) or (selectionHighlightInfo.selectionRotateCenter = pSelectionRotateCenter)) then
     begin
       result := selectionHighlightInfo.selectionHighlight;
       pHighlightOffset := selectionHighlightInfo.highlightOffset;
     end else
     begin
       result := nil;
       pHighlightOffset := point(0,0);
     end;
end;

procedure TFMain.StoreSelectionHighlight(pFormArea: TRect;
  pImageOffset: TPoint; pSelectionRotateAngle: single; pSelectionRotateCenter: TPointF; pZoomFactor: single; selecting: boolean; pSelectionHighlight: TBGRABitmap; pHighlightOffset : TPoint);
begin
   ForgetSelectionHightlight;
   selectionHighlightInfo.formArea.Left := pFormArea.Left;
   selectionHighlightInfo.formArea.Right := pFormArea.Right;
   selectionHighlightInfo.formArea.Top := pFormArea.Top;
   selectionHighlightInfo.formArea.Bottom := pFormArea.Bottom;
   selectionHighlightInfo.ImageOffset.X := pImageOffset.X;
   selectionHighlightInfo.ImageOffset.Y := pImageOffset.Y;
   selectionHighlightInfo.selectionRotateAngle := pSelectionRotateAngle;
   selectionHighlightInfo.selectionRotateCenter := pSelectionRotateCenter;
   selectionHighlightInfo.zoomFactor := pZoomFactor;
   selectionHighlightInfo.selecting := selecting;
   selectionHighlightInfo.selectionHighlight := pSelectionHighlight;
   selectionHighlightInfo.highlightOffset := pHighlightOffset;
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
  result := Rect(0,ToolbarsHeight,ClientWidth,ClientHeight);
end;

procedure TFMain.PictureSelectionChanged(sender: TLazPaintImage; AOffsetOnly: boolean);
begin
  if not AOffsetOnly or (Zoom.Factor <> 1) then ForgetSelectionHightlight;
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

procedure TFMain.PaintVirtualScreen;
begin
  {$IFDEF USEPAINTBOXPICTURE}
  QueryPaintVirtualScreen := True;
  PaintBox_Picture.Repaint;
  QueryPaintVirtualScreen := False;
  {$ELSE}
  PaintVirtualScreenImplementation;
  {$ENDIF}
end;

{$hints off}
procedure TFMain.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
   //  block Erasing background
   //  inherited EraseBackground(DC);
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
  if cursor <> wantedCursor then Cursor := wantedCursor;
  if PaintBox_Picture.Cursor <> wantedCursor then PaintBox_Picture.Cursor := wantedCursor;
  result := (virtualScreenPenCursorBefore <> virtualScreenPenCursor);
end;

procedure TFMain.PaintVirtualScreenImplementation;
const margin = 2;
var cursorBack: TBGRABitmap;
    rx,ry: single;
    c,tl,br: TPointF;
    x0,y0,tx,ty: integer;
    orig: TPointF;
    DestCanvas: TCanvas;
    DrawOfs: TPoint;
    cursorContourF: array of TPointF;
    rectBack: TRect;
begin
  if virtualscreen = nil then exit;
  DestCanvas := PictureCanvas;
  DrawOfs := PictureCanvasOfs;
  if virtualScreenPenCursor then
  begin
    orig := PointF(pictureOrigin.X,pictureOrigin.Y);
    with ToolManager do
    begin
      c := self.BitmapToForm(ToolCurrentCursorPos) - orig;
      tl := self.BitmapToForm(ToolCurrentCursorPos.X-ToolPenWidth/2,ToolCurrentCursorPos.Y-ToolPenWidth/2) - orig;
      br := self.BitmapToForm(ToolCurrentCursorPos.X+ToolPenWidth/2,ToolCurrentCursorPos.Y+ToolPenWidth/2) - orig;
    end;
    rx := (br.x-tl.x)/2-0.5;
    ry := (br.y-tl.y)/2-0.5;
    x0 := floor(tl.x)-margin;
    y0 := floor(tl.y)-margin;
    tx := ceil(br.x)-x0+1+2*margin;
    ty := ceil(br.y)-y0+1+2*margin;
    rectBack := rect(x0,y0,x0+tx,y0+ty);
    IntersectRect(rectBack,rectBack,rect(0,0,virtualScreen.Width,virtualScreen.Height));
    if not IsRectEmpty(rectBack) then
    begin
      cursorBack := virtualScreen.GetPart(rectBack) as TBGRABitmap;

      cursorContourF := virtualscreen.ComputeEllipseContour(c.x,c.y,rx,ry);
      virtualscreen.PenStyle := psSolid;
      virtualscreen.DrawPolygonAntialias(cursorcontourF,BGRA(0,0,0,192),3);
      virtualscreen.DrawPolygonAntialias(cursorcontourF,BGRA(255,255,255,255),1);
      virtualscreen.Draw(DestCanvas, pictureOrigin.X+DrawOfs.X, pictureOrigin.Y+DrawOfs.Y, True);

      virtualScreen.PutImage(rectBack.left,rectBack.Top,cursorBack,dmSet);
      cursorBack.Free;
    end else
      virtualscreen.Draw(DestCanvas, pictureOrigin.X+DrawOfs.X, pictureOrigin.Y+DrawOfs.Y, True);
  end else
    virtualscreen.Draw(DestCanvas, pictureOrigin.X+DrawOfs.X, pictureOrigin.Y+DrawOfs.Y, True);
  ToolManager.ShownZoom := Zoom.Factor;
  if (image.Width = 0) or (image.Height = 0) then
    Zoom.MinFactor := 1
  else
    Zoom.MinFactor := max(8/image.Width, 8/image.Height);
  with GetPictureArea do
    Zoom.MaxFactor := max(1,min((right-left)/8,(bottom-top)/8));
end;

procedure TFMain.PaintPictureImplementation;
const StretchMode = rmSimpleStretch;
var //layout
    formArea: TRect;
    visualLeft, visualTop: integer;
    visualWidth, visualHeight: integer;
    maxOffset, minOffset: TPoint; temp: integer;

    //partial drawing
    partialImage, partialSelection: TBGRABitmap;
    topLeftCorner,bottomRightCorner: TPoint;
    topLeftCornerF,bottomRightCornerF: TPointF;

    //resampled image
    shownSelection, resampled, ofsSelection, resampledSelection, selectionHighlight: TBGRABitmap;
    highlightOffset: TPoint;

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

  selectionHighlight := RetrieveSelectionHighlight(formArea,image.ImageOffset,image.GetSelectionRotateAngle,
       image.GetSelectionRotateCenter,
       Zoom.Factor,ToolManager.IsSelectingTool, highlightOffset);

  //if selection highlight has not been computed yet, then compute shown selection
  if selectionHighlight = nil then
  begin
    if image.SelectionNil then
      shownSelection := nil else
    begin
      if image.GetSelectionRotateAngle <> 0 then
      begin
        if image.SelectionLayerIsEmpty then
          shownSelection := image.ComputeRotatedSelection else
            shownSelection := nil;
      end else
        shownSelection := image.SelectionReadonly;
    end;
  end else
    shownSelection := nil;

  visualLeft += round(image.ImageOffset.X*visualWidth/image.Width);
  visualTop += round(image.ImageOffset.Y*visualHeight/image.Height);

  if (visualLeft < formArea.left) or (visualTop < formArea.Top) or
    (visualLeft+visualWidth > formArea.Right) or (visualTop+visualHeight > formArea.Bottom) then
  begin
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

    if Zoom.Factor <> 1 then
    begin
      partialImage := TBGRABitmap.Create(PictureActualSize.X,PictureActualSize.Y);
      image.Draw(partialImage, -PictureOffset.x, -PictureOffset.Y);

      if shownSelection <> nil then
      begin
        partialSelection := TBGRABitmap.Create(PictureActualSize.X,PictureActualSize.Y);
        partialSelection.PutImage(-PictureOffset.X+image.GetSelectionOffset.X,
                                  -PictureOffset.Y+image.GetSelectionOffset.Y, shownSelection, dmSet);
      end else
       partialSelection := nil;
    end else
    begin
      partialSelection := nil;
      partialImage := nil;
    end;

    visualLeft := visualLeft + round(TopLeftCorner.x/image.width*visualWidth);
    visualTop := visualTop + round(TopLeftCorner.y/image.height*visualHeight);
    visualWidth := round(PictureActualSize.X*Zoom.Factor);
    if visualWidth = 0 then visualWidth := 1;
    visualHeight := round(PictureActualSize.Y*Zoom.Factor);
    if visualHeight = 0 then visualHeight := 1;

    if partialImage <> nil then
    begin
      if (visualWidth = partialImage.Width) and (visualHeight = partialImage.Height) then
      begin
        resampled := partialImage;
        resampledSelection := partialSelection;
      end else
      begin
        resampled := DoResample(partialImage,visualWidth,visualHeight, StretchMode);
        if (Zoom.Factor > 4) and LazPaintInstance.GridVisible then DrawGrid(resampled,visualWidth/partialImage.width,visualHeight/partialImage.Height);
        if partialSelection <> nil then
          resampledSelection := DoResample(partialSelection,visualWidth,visualHeight, StretchMode) else
            resampledSelection := nil;
        partialImage.Free;
        partialSelection.Free;
      end;
    end else
    begin
      resampled := nil;
      resampledSelection := partialSelection;
    end;
  end else
  begin
    if (visualWidth <> Image.Width) or (visualHeight <> Image.Height) then
    begin
      resampled := DoResample(Image.RenderedImage,visualWidth, visualHeight, StretchMode) as TBGRABitmap;
    end else
      resampled := nil;

    if (Zoom.Factor > 4) and LazPaintInstance.GridVisible then DrawGrid(resampled,visualWidth/image.width,visualHeight/image.Height);
    if (shownSelection <> nil) and (selectionHighlight = nil) and (Zoom.Factor <> 1) then
    begin
       if (image.GetSelectionOffset.X <> 0) or (image.GetSelectionOffset.Y <> 0) then
       begin
         ofsSelection := TBGRABitmap.Create(image.Width,
                      image.Height, BGRABlack);
         ofsSelection.PutImage(image.GetSelectionOffset.X,
                  image.GetSelectionOffset.Y, shownSelection, dmSet);
         resampledSelection := DoResample(ofsSelection,visualWidth, visualHeight, StretchMode);
         FreeAndNil(ofsSelection);
       end else
         resampledSelection := DoResample(shownSelection,visualWidth, visualHeight, StretchMode);
    end else
         resampledSelection := nil;
    PictureActualSize.X := image.width;
    PictureActualSize.Y := image.Height;
    PictureOffset := Point(0,0);
  end;

  //create or resize virtual screen if needed
  if (virtualscreen = nil) or (virtualScreen.Width <> visualWidth) or (virtualScreen.Height <> visualHeight) then
  begin
    FreeAndNil(virtualScreen);
    virtualScreen := TBGRABitmap.Create(visualWidth, visualHeight);
  end;
  DrawCheckers(virtualScreen);

  //draw image (with merged selection)
  if resampled <> nil then
    virtualScreen.PutImage(0,0,resampled,dmDrawWithTransparency)
  else
    Image.Draw(virtualScreen,-PictureOffset.x, -PictureOffset.Y);

  //compute selection highlight if needed
  if selectionHighlight = nil then
  begin
     if resampledSelection <> nil then
     begin
       highlightOffset := point(0,0);
       selectionHighlight := resampledSelection.FilterEmbossHighlight(
              ToolManager.IsSelectingTool and not ShowSelectionNormal, BGRABlack, highlightOffset) as TBGRABitmap;
       StoreSelectionHighlight(formArea,image.ImageOffset,image.GetSelectionRotateAngle,image.GetSelectionRotateCenter, Zoom.Factor,
         ToolManager.IsSelectingTool,selectionHighlight,highlightOffset);
     end else if shownSelection <> nil then
     begin
       highlightOffset := point(0,0);
       selectionHighlight := shownSelection.FilterEmbossHighlight(
              ToolManager.IsSelectingTool and not ShowSelectionNormal, BGRABlack, highlightOffset) as TBGRABitmap;
       StoreSelectionHighlight(formArea,image.ImageOffset,image.GetSelectionRotateAngle,image.GetSelectionRotateCenter, Zoom.Factor,
         ToolManager.IsSelectingTool,selectionHighlight,highlightOffset);
     end;
  end;
  FreeAndNil(resampledSelection);

  if selectionHighlight <> nil then
  begin
    //draw previously stored highlight
    if resampled <> nil then
      virtualScreen.PutImage(highlightOffset.X,highlightOffset.Y,selectionHighlight,dmFastBlend)
    else
      virtualScreen.PutImage(highlightOffset.X-PictureOffset.x+image.GetSelectionOffset.X,
        highlightOffset.Y-PictureOffset.Y+image.GetSelectionOffset.Y,selectionHighlight,dmFastBlend);
  end;
  FreeAndNil(resampled);

  //define picture view
  pictureOrigin := Point(visualLeft,visualTop);
  pictureViewSize := Point(visualWidth,visualHeight);

  //show tools info
  ToolManager.RenderTool(virtualScreen);
  PaintVirtualScreenImplementation;
  if shownSelection <> image.SelectionReadonly then shownSelection.Free;
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

initialization
  {$I lazpaintmainform.lrs}

end.
