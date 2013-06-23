unit LazpaintMainForm;

{$mode objfpc}{$H+}

interface

{$IFDEF DARWIN}
  {$DEFINE USEPAINTBOXPICTURE}
{$ENDIF}

uses
  Classes, LMessages, SysUtils, FileUtil, LResources, Forms, Controls,
  Graphics, Dialogs, Menus, ExtDlgs, ComCtrls, ActnList, StdCtrls, ExtCtrls,
  Spin, Buttons, process,

  BGRABitmap, BGRABitmapTypes, BGRALayers, LazPaintType, utool, uimage,
  uconfig, types, uscaledpi, uresourcestrings, LCLType;

type
  { TFMain }

  TFMain = class(TForm)
    ImageCropLayer: TAction;
    MenuItem130: TMenuItem;
    MenuHorizFlipSub: TMenuItem;
    MenuHorizFlipPicture: TMenuItem;
    MenuHorizFlipLayer: TMenuItem;
    MenuHorizFlipSelection: TMenuItem;
    MenuItem26: TMenuItem;
    MenuVertFlipLayer: TMenuItem;
    MenuVertFlipSelection: TMenuItem;
    MenuVertFlipSub: TMenuItem;
    MenuVertFlipPicture: TMenuItem;
    OpenTextureDialog: TOpenDialog;
    OpenPictureDialog1: TOpenDialog;
    ViewLayerStackButton: TAction;
    MenuItem_ViewLayerStack: TMenuItem;
    ViewLayerStack: TAction;
    FileImport3D: TAction;
    MenuItem127: TMenuItem;
    MenuItem128: TMenuItem;
    MenuItem129: TMenuItem;
    Open3DObjectDialog: TOpenPictureDialog;
    ToolButton_ViewLayerStack: TToolButton;
    ToolTextureMapping: TAction;
    EditSelection: TAction;
    FilterClearTypeInverse: TAction;
    FilterClearType: TAction;
    Label_Altitude: TLabel;
    Label_Border: TLabel;
    Label_CurrentDiff: TLabel;
    Label_Shape: TLabel;
    MenuItem103: TMenuItem;
    MenuItem104: TMenuItem;
    MenuItem121: TMenuItem;
    MenuItem122: TMenuItem;
    MenuItem123: TMenuItem;
    MenuItem124: TMenuItem;
    MenuItem125: TMenuItem;
    MenuDonate: TMenuItem;
    MenuItem126: TMenuItem;
    Menu_Language: TMenuItem;
    MnuCheckUpdate: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem80: TMenuItem;
    Panel_PhongShape: TPanel;
    Panel_Altitude: TPanel;
    BorderSize_SpinEdit: TSpinEdit;
    ShapeAltitude_SpinEdit: TSpinEdit;
    ToolBar16: TToolBar;
    ToolButton_ShapeVerticalCone: TToolButton;
    ToolButton_ShapeHorizontalCylinder: TToolButton;
    ToolButton_ShapeVerticalCylinder: TToolButton;
    Tool_TextPhong: TToolButton;
    ToolButton_ShapeSphere: TToolButton;
    ToolButton_ShapeRectangle: TToolButton;
    ToolButton_ShapeCone: TToolButton;
    ToolButton_ShapeRoundRect: TToolButton;
    ToolPhong: TAction;
    FileLoadSelection: TAction;
    FileSaveSelectionAs: TAction;
    FontDialog1: TFontDialog;
    MenuItem118: TMenuItem;
    MenuItem119: TMenuItem;
    MenuItem120: TMenuItem;
    LoadSelectionDialog: TOpenPictureDialog;
    SaveSelectionDialog: TSavePictureDialog;
    TextBlur_SpinEdit: TSpinEdit;
    TextShadowX_SpinEdit: TSpinEdit;
    TextShadowY_SpinEdit: TSpinEdit;
    Label_TextBlur: TLabel;
    Label_ShadowOffset: TLabel;
    MenuItem116: TMenuItem;
    MenuItem117: TMenuItem;
    MenuItem91: TMenuItem;
    ToolText: TAction;
    FilterPixelate: TAction;
    EditPasteAsNew: TAction;
    Label_Text: TLabel;
    MenuItem86: TMenuItem;
    MenuItem90: TMenuItem;
    MenuItem_ViewGrid: TMenuItem;
    Panel_Text: TPanel;
    ToolBar15: TToolBar;
    Tool_TextFont: TToolButton;
    Tool_TextOutline: TToolButton;
    Tool_TextShadow: TToolButton;
    ViewGrid: TAction;
    EmbeddedCancel: TAction;
    EmbeddedValidate: TAction;
    Combo_SplineStyle: TComboBox;
    MenuItem115: TMenuItem;
    PaintBox_Picture: TPaintBox;
    PaintBox_PenPreview: TPaintBox;
    Panel_Embedded: TPanel;
    Panel_PenWidthPreview: TPanel;
    Panel_SplineStyle: TPanel;
    Label_Curve: TLabel;
    RenderWoodVertical: TAction;
    FilterTwirl: TAction;
    GridNbY_SpinEdit: TSpinEdit;
    MenuItem114: TMenuItem;
    Panel_Grid: TPanel;
    Label_Grid: TLabel;
    GridNbX_SpinEdit: TSpinEdit;
    Label_x: TLabel;
    TimerHidePenPreview: TTimer;
    ToolBar13: TToolBar;
    Shapes_CloseShape: TToolButton;
    ToolBar14: TToolBar;
    ToolButton19: TToolButton;
    ToolButton20: TToolButton;
    ToolButton3: TToolButton;
    ToolButton_GridMoveWithoutDeformation: TToolButton;
    ToolDeformation: TAction;
    MenuItem113: TMenuItem;
    RenderWaterAuto: TAction;
    MenuItem101: TMenuItem;
    MenuItem105: TMenuItem;
    MenuItem106: TMenuItem;
    MenuItem107: TMenuItem;
    MenuItem108: TMenuItem;
    MenuItem109: TMenuItem;
    MenuItem110: TMenuItem;
    MenuItem111: TMenuItem;
    MenuItem112: TMenuItem;
    RenderMarble: TAction;
    RenderRoundStone: TAction;
    RenderStone: TAction;
    RenderSnowPrint: TAction;
    RenderCamouflage: TAction;
    RenderMetalFloor: TAction;
    RenderPerlinNoiseCyclic: TAction;
    RenderWood: TAction;
    RenderPlastik: TAction;
    MenuItem1: TMenuItem;
    MenuItem102: TMenuItem;
    MenuItem99: TMenuItem;
    ToolNoTexture: TAction;
    ToolLoadTexture: TAction;
    RenderPerlinNoise: TAction;
    FilterFastBlur: TAction;
    FilterPlane: TAction;
    IdleTimer1: TIdleTimer;
    Image_CurrentTexture: TImage;
    MenuItem100: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem38: TMenuItem;
    MenuItem59: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    MenuItem93: TMenuItem;
    MenuItem94: TMenuItem;
    MenuItem95: TMenuItem;
    MenuItem96: TMenuItem;
    MenuItem97: TMenuItem;
    MenuItem98: TMenuItem;
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
    ToolMagicWand: TAction;
    ImageRepeat: TAction;
    RenderCustomWater: TAction;
    HelpAbout: TAction;
    HelpIndex: TAction;
    ColorColorize: TAction;
    ColorShiftColor: TAction;
    FilterNegativeLinear: TAction;
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
    MenuItem39: TMenuItem;
    MenuItem48: TMenuItem;
    MenuItem49: TMenuItem;
    MenuItem51: TMenuItem;
    MenuItem60: TMenuItem;
    MenuItem70: TMenuItem;
    MenuItem71: TMenuItem;
    MenuItem72: TMenuItem;
    MenuItem73: TMenuItem;
    MenuItem74: TMenuItem;
    MenuItem75: TMenuItem;
    MenuItem76: TMenuItem;
    MenuItem77: TMenuItem;
    MenuItem78: TMenuItem;
    MenuItem79: TMenuItem;
    MenuItem81: TMenuItem;
    MenuItem82: TMenuItem;
    MenuItem83: TMenuItem;
    MenuItem84: TMenuItem;
    MenuItem85: TMenuItem;
    MnuHelp: TMenuItem;
    MenuItem87: TMenuItem;
    MenuItem88: TMenuItem;
    MenuItem89: TMenuItem;
    MnuIndex: TMenuItem;
    MnuAbout: TMenuItem;
    MenuItem92: TMenuItem;
    Menu_RecentFiles: TMenuItem;
    MenuItem_ViewColors: TMenuItem;
    ToolButton_SinGradient: TToolButton;
    Tool_JoinMiter: TToolButton;
    Tool_PenDash: TToolButton;
    ViewColors: TAction;
    Label_Coordinates: TLabel;
    Panel_Coordinates: TPanel;
    ViewToolbox: TAction;
    EditSelectAll: TAction;
    FilterContour: TAction;
    FilterCustomBlur: TAction;
    FilterGrayscale: TAction;
    Image_Swap: TImage;
    MenuItem61: TMenuItem;
    MenuItem62: TMenuItem;
    MenuItem63: TMenuItem;
    MenuItem64: TMenuItem;
    MenuItem65: TMenuItem;
    MenuItem66: TMenuItem;
    MenuItem67: TMenuItem;
    MenuItem68: TMenuItem;
    MenuItem69: TMenuItem;
    MenuItem_ViewToolbox: TMenuItem;
    ToolRotateSelection: TAction;
    EditDeleteSelection: TAction;
    EditCut: TAction;
    EditPaste: TAction;
    EditCopy: TAction;
    Eraser_Label: TLabel;
    Eraser_SpinEdit: TSpinEdit;
    Image_CurrentTool: TImage;
    Label_Pen: TLabel;
    Label_Back: TLabel;
    Label_CurrentZoom: TLabel;
    MenuItem36: TMenuItem;
    MenuItem53: TMenuItem;
    MenuItem54: TMenuItem;
    MenuItem55: TMenuItem;
    MenuItem56: TMenuItem;
    MenuItem57: TMenuItem;
    MenuItem58: TMenuItem;
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
    PenWidth_Label: TLabel;
    PenWidth_SpinEdit: TSpinEdit;
    Shapes_DrawBorder: TToolButton;
    Shapes_FillShape: TToolButton;
    Shape_BackColor: TShape;
    Shape_PenColor: TShape;
    SpinEdit_BackOpacity: TSpinEdit;
    SpinEdit_PenOpacity: TSpinEdit;
    Tolerance_Label: TLabel;
    Tolerance_SpinEdit: TSpinEdit;
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
    ToolButton_DiamondGradient: TToolButton;
    ToolButton_LinearGradient: TToolButton;
    ToolButton_ProgressiveFloodfill: TToolButton;
    ToolButton_RadialGradient: TToolButton;
    ToolButton_ReflectedGradient: TToolButton;
    ToolButton_ZoomOriginal: TToolButton;
    ToolMoveSelection: TAction;
    ToolSelectionPen: TAction;
    EditInvertSelection: TAction;
    EditDeselect: TAction;
    MenuItem50: TMenuItem;
    MenuItem52: TMenuItem;
    ToolSelectCurve: TAction;
    ToolSelectPoly: TAction;
    ToolSelectEllipse: TAction;
    ToolSelectRect: TAction;
    FilterNegative: TAction;
    FilterNormalize: TAction;
    FilterEmboss: TAction;
    FilterMotionBlur: TAction;
    FilterPreciseBlur: TAction;
    FilterDiskBlur: TAction;
    FilterCoronaBlur: TAction;
    FilterRadialBlur: TAction;
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
    MenuItem25: TMenuItem;
    MenuHorizFlip: TMenuItem;
    MenuVertFlip: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem37: TMenuItem;
    MenuItem40: TMenuItem;
    MenuItem41: TMenuItem;
    MenuItem42: TMenuItem;
    MenuItem43: TMenuItem;
    MenuItem44: TMenuItem;
    MenuItem45: TMenuItem;
    MenuItem46: TMenuItem;
    MenuItem47: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    ToolGradient: TAction;
    ToolFloodfill: TAction;
    MenuItem21: TMenuItem;
    ToolSpline: TAction;
    MenuItem16: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    Shapes_Separator1: TToolButton;
    ToolEraser: TAction;
    MenuItem15: TMenuItem;
    ToolPolygon: TAction;
    MenuItem14: TMenuItem;
    ToolColorPicker: TAction;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    ToolEllipse: TAction;
    ToolRect: TAction;
    MenuItem10: TMenuItem;
    MenuItem13: TMenuItem;
    ToolPen: TAction;
    ToolHand: TAction;
    ViewZoomOut: TAction;
    ViewZoomIn: TAction;
    FileQuit: TAction;
    FileSaveAs: TAction;
    FileSave: TAction;
    FileOpen: TAction;
    FileNew: TAction;
    ColorDialog1: TColorDialog;
    MainMenu1: TMainMenu;
    MenuFile: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem_QuitSeparator: TMenuItem;
    MenuItem_Quit: TMenuItem;
    ActionList1: TActionList;
    ImageList1: TImageList;
    SavePictureDialog1: TSavePictureDialog;
    procedure BorderSize_SpinEditChange(Sender: TObject);
    procedure ColorColorizeExecute(Sender: TObject);
    procedure ColorShiftColorExecute(Sender: TObject);
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
    procedure FileSaveSelectionAsExecute(Sender: TObject);
    procedure FileSaveSelectionAsUpdate(Sender: TObject);
    procedure FilterClearTypeExecute(Sender: TObject);
    procedure FilterClearTypeInverseExecute(Sender: TObject);
    procedure FilterPixelateExecute(Sender: TObject);
    procedure FilterTwirlExecute(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure ImageCropLayerExecute(Sender: TObject);
    procedure ImageCropLayerUpdate(Sender: TObject);
    procedure MenuDonateClick(Sender: TObject);
    procedure MenuHorizFlipLayerClick(Sender: TObject);
    procedure MenuHorizFlipPictureClick(Sender: TObject);
    procedure MenuHorizFlipSelectionClick(Sender: TObject);
    procedure MenuItem25Click(Sender: TObject);
    procedure MenuVertFlipLayerClick(Sender: TObject);
    procedure MenuVertFlipPictureClick(Sender: TObject);
    procedure MenuVertFlipSelectionClick(Sender: TObject);
    procedure ShapeAltitude_SpinEditChange(Sender: TObject);
    procedure TextBlur_SpinEditChange(Sender: TObject);
    procedure GridNb_SpinEditChange(Sender: TObject);
    procedure Image_CurrentTextureMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MnuCheckUpdateClick(Sender: TObject);
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
    procedure Panel_PenWidthMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Panel_ToolbarBackgroundMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure PenWidth_SpinEditMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure RenderCamouflageExecute(Sender: TObject);
    procedure RenderCloudsExecute(Sender: TObject);
    procedure FilterCylinderExecute(Sender: TObject);
    procedure FilterFastBlurExecute(Sender: TObject);
    procedure FilterNegativeLinearExecute(Sender: TObject);
    procedure RenderMarbleExecute(Sender: TObject);
    procedure RenderMetalFloorExecute(Sender: TObject);
    procedure RenderPerlinNoiseCyclicExecute(Sender: TObject);
    procedure RenderPerlinNoiseExecute(Sender: TObject);
    procedure FilterPlaneExecute(Sender: TObject);
    procedure FilterSphereExecute(Sender: TObject);
    procedure RenderPlastikExecute(Sender: TObject);
    procedure RenderRoundStoneExecute(Sender: TObject);
    procedure RenderSnowPrintExecute(Sender: TObject);
    procedure RenderStoneExecute(Sender: TObject);
    procedure RenderCustomWaterExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormHide(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure HelpAboutExecute(Sender: TObject);
    procedure HelpIndexExecute(Sender: TObject);
    procedure IdleTimer1Timer(Sender: TObject);
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
    procedure RenderWaterAutoExecute(Sender: TObject);
    procedure RenderWoodExecute(Sender: TObject);
    procedure RenderWoodVerticalExecute(Sender: TObject);
    procedure TextShadowX_SpinEditChange(Sender: TObject);
    procedure TextShadowY_SpinEditChange(Sender: TObject);
    procedure TimerHidePenPreviewTimer(Sender: TObject);
    procedure ToolButton_ViewLayerStackClick(Sender: TObject);
    procedure ToolButton_ShapeHorizontalCylinderClick(Sender: TObject);
    procedure ToolButton_ShapeVerticalConeClick(Sender: TObject);
    procedure ToolButton_ShapeVerticalCylinderClick(Sender: TObject);
    procedure ToolTextureMappingExecute(Sender: TObject);
    procedure Tool_TextPhongClick(Sender: TObject);
    procedure ToolButton_GridMoveWithoutDeformationClick(Sender: TObject);
    procedure ToolButton_ShapeConeClick(Sender: TObject);
    procedure ToolButton_ShapeRectangleClick(Sender: TObject);
    procedure ToolButton_ShapeRoundRectClick(Sender: TObject);
    procedure ToolButton_ShapeSphereClick(Sender: TObject);
    procedure ToolButton_SinGradientClick(Sender: TObject);
    procedure ToolDeformationExecute(Sender: TObject);
    procedure ToolLoadTextureExecute(Sender: TObject);
    procedure ToolMagicWandExecute(Sender: TObject);
    procedure ToolNoTextureExecute(Sender: TObject);
    procedure ToolNoTextureUpdate(Sender: TObject);
    procedure ToolPhongExecute(Sender: TObject);
    procedure ToolTextExecute(Sender: TObject);
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
    procedure Eraser_SpinEditChange(Sender: TObject);
    procedure FileNewExecute(Sender: TObject);
    procedure FileOpenExecute(Sender: TObject);
    procedure FileQuitExecute(Sender: TObject);
    procedure FileSaveAsExecute(Sender: TObject);
    procedure FileSaveExecute(Sender: TObject);
    procedure FileSaveUpdate(Sender: TObject);
    procedure FilterPreciseBlurExecute(Sender: TObject);
    procedure FilterContourExecute(Sender: TObject);
    procedure FilterCoronaBlurExecute(Sender: TObject);
    procedure FilterCustomBlurExecute(Sender: TObject);
    procedure FilterDiskBlurExecute(Sender: TObject);
    procedure FilterEmbossExecute(Sender: TObject);
    procedure FilterGrayscaleExecute(Sender: TObject);
    procedure FilterMedianExecute(Sender: TObject);
    procedure FilterMotionBlurExecute(Sender: TObject);
    procedure FilterNegativeExecute(Sender: TObject);
    procedure FilterNormalizeExecute(Sender: TObject);
    procedure FilterRadialBlurExecute(Sender: TObject);
    procedure FilterSharpenExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure ImageSmartZoom3Execute(Sender: TObject);
    procedure FilterSmoothExecute(Sender: TObject);
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
    procedure Image_SwapMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PenWidth_SpinEditChange(Sender: TObject);
    procedure Shapes_CloseShapeClick(Sender: TObject);
    procedure Shape_BackColorMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Shape_PenColorMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SpinEdit_BackOpacityChange(Sender: TObject);
    procedure SpinEdit_PenOpacityChange(Sender: TObject);
    procedure Tolerance_SpinEditChange(Sender: TObject);
    procedure ToolButton_DiamondGradientClick(Sender: TObject);
    procedure ToolButton_LinearGradientClick(Sender: TObject);
    procedure ToolButton_ProgressiveFloodfillClick(Sender: TObject);
    procedure ToolButton_RadialGradientClick(Sender: TObject);
    procedure ToolButton_ReflectedGradientClick(Sender: TObject);
    procedure ToolColorPickerExecute(Sender: TObject);
    procedure ToolDrawShapeExecute(Sender: TObject);
    procedure ToolEllipseExecute(Sender: TObject);
    procedure ToolEraserExecute(Sender: TObject);
    procedure ToolFillShapeExecute(Sender: TObject);
    procedure ToolFloodfillExecute(Sender: TObject);
    procedure ToolGradientExecute(Sender: TObject);
    procedure ToolHandExecute(Sender: TObject);
    procedure ToolMoveSelectionExecute(Sender: TObject);
    procedure ToolMoveSelectionUpdate(Sender: TObject);
    procedure ToolPenExecute(Sender: TObject);
    procedure ToolPolygonExecute(Sender: TObject);
    procedure ToolRectExecute(Sender: TObject);
    procedure ToolRotateSelectionExecute(Sender: TObject);
    procedure ToolSelectCurveExecute(Sender: TObject);
    procedure ToolSelectEllipseExecute(Sender: TObject);
    procedure ToolSelectionPenExecute(Sender: TObject);
    procedure ToolSelectPolyExecute(Sender: TObject);
    procedure ToolSelectRectExecute(Sender: TObject);
    procedure ToolSplineExecute(Sender: TObject);
    procedure ViewToolboxExecute(Sender: TObject);
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;

  private
    { private declarations }
    initialized: boolean;
    toolbars: TList;
    ToolbarsHeight : integer;
    btnLeftDown, btnRightDown : boolean;
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
    SelectionEditConfig,TextureEditConfig: TStream;
    StartDirectory: string;
    DelayedPaintPicture: boolean;
    previousToolImg: integer;

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

    function GetUpdateCheckerFilename: string;
    procedure Init;
    procedure SetLazPaintInstance(const AValue: TLazPaintCustomInstance);
    procedure SetShowSelectionNormal(const AValue: boolean);
    function RetrieveSelectionHighlight(pFormArea: TRect; pImageOffset: TPoint; pSelectionRotateAngle: single; pSelectionRotateCenter: TPointF;pZoomFactor: single; selecting: boolean; out pHighlightOffset: TPoint): TBGRABitmap;
    procedure StoreSelectionHighlight(pFormArea: TRect; pImageOffset: TPoint; pSelectionRotateAngle: single; pSelectionRotateCenter: TPointF;pZoomFactor: single; selecting: boolean; pSelectionHighlight: TBGRABitmap; pHighlightOffset: TPoint);
    procedure ForgetSelectionHightlight;
    procedure DoCopySelection;
    procedure UpdateToolImage;
    procedure NoTextureIcon;
    procedure ToggleGridVisible;
    procedure ToggleToolboxVisible;
    procedure ToggleColorsVisible;
    procedure ToggleLayersVisible;
    procedure ShowColorDialogForPen;
    procedure ShowColorDialogForBack;
    procedure ShowPenPreview(ShouldRepaint: boolean= False);
    procedure HidePenPreview(TimeMs: Integer = 300);
    function TryZoomIn: boolean;
    function TryZoomOut: boolean;
    function AbleToLoad(filename: string): boolean;
    function AbleToSaveAs(filename: string): boolean;
    property showSelectionNormal: boolean read FShowSelectionNormal write SetShowSelectionNormal;
    procedure PaintPictureImplementation; //do not call directly
    procedure PaintVirtualScreenImplementation; //do not call directly
    procedure PaintBlueAreaImplementation;
    procedure OnPaintHandler;
    procedure UpdateTextureIcon;
    procedure LabelAutosize(ALabel: TLabel);
    procedure AskMergeSelection(ACaption: string);
    procedure ReleaseMouseButtons(Shift: TShiftState);
    procedure UpdatePanelPhongShape;
    procedure SelectionInstanceOnRun(AInstance : TLazPaintCustomInstance);
    function ShowOpenTextureDialog: boolean;
    procedure CropToSelectionAndLayer;
    procedure CropToSelection;

  public
    { public declarations }
    zoomFactor: single;
    pictureOrigin, pictureOffset, pictureViewSize, pictureActualSize: TPoint;
    FormBackgroundColor: TColor;
    virtualScreen : TBGRABitmap;
    virtualScreenPenCursor: boolean;
    QueryPaintVirtualScreen: boolean;
    StackNeedUpdate: boolean;

//    procedure DoReplaceBitmap(var replacement: TBitmapReplacement; DoSaveUndo: boolean = true; DoPaintPicture: boolean = true);
    procedure PaintPicture;
    procedure PaintVirtualScreen;
    function TryOpenFile(filename: string): boolean;
    procedure SetCurrentBitmap(bmp: TBGRABitmap; DoRepaint: boolean = true);
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
    procedure PictureSelectionChanged(sender: TLazPaintImage; AOffsetOnly: boolean);
    function ExecuteFilter(filter: TPictureFilter; skipDialog: boolean = false): boolean;
    procedure SetCurrentFilename(path: string);
    function GetCurrentFilename: string;
    property LazPaintInstance: TLazPaintCustomInstance read FLazPaintInstance write SetLazPaintInstance;
    property UpdateCheckerFilename: string read GetUpdateCheckerFilename;
    procedure UpdateEditPicture(ADelayed: boolean = false);
  end;

implementation

uses LCLIntf, LCLProc, ugraph, BGRAGradients, math, umac, uclipboard, ucursors, uobject3D;

const PenWidthFactor = 10;

{ TFMain }

procedure TFMain.FormCreate(Sender: TObject);
var i,j: integer;
begin
  initialized := false;

  //layout init
  ScaleDPI(Self,OriginalDPI);
  ScaleImageList(ImageList1,OriginalDPI);
  zoomFactor := 1;
  pictureOrigin := Point(0,0);
  ToolbarsHeight := 0;
  virtualScreen := nil;
  TextureEditConfig := nil;
  SelectionEditConfig := nil;
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

  //Update menu item
  if FileExistsUTF8(UpdateCheckerFilename) then
  begin
    MnuCheckUpdate.Visible:= true;
  end;

  //use background color
  FormBackgroundColor := self.Color;
  self.Color := clBtnFace; //toolbar color inherited on mac

  //mouse status
  btnLeftDown := false;
  btnRightDown := false;

  //recursive calls
  InFormMouseMove:= false;
  InFormPaint := false;

  //mac interface
  CheckQuitMenu(MenuItem_Quit,MenuItem_QuitSeparator);
  CheckSpinEdit(SpinEdit_BackOpacity);
  CheckSpinEdit(SpinEdit_PenOpacity);
  CheckSpinEdit(Tolerance_SpinEdit);
  CheckSpinEdit(Eraser_SpinEdit);
  CheckSpinEdit(PenWidth_SpinEdit);
  CheckSpinEdit(GridNbX_SpinEdit);
  CheckSpinEdit(GridNbY_SpinEdit);
  CheckSpinEdit(TextBlur_SpinEdit);
  CheckSpinEdit(TextShadowX_SpinEdit);
  CheckSpinEdit(TextShadowY_SpinEdit);
  CheckSpinEdit(ShapeAltitude_SpinEdit);
  CheckSpinEdit(BorderSize_SpinEdit);

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
  toolbars.Add(Panel_PhongShape);
  toolbars.Add(Panel_Altitude);

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
  LabelAutosize(PenWidth_Label);
  LabelAutosize(Eraser_Label);
  LabelAutosize(Tolerance_Label);
  LabelAutosize(Label_Grid);
  LabelAutosize(Label_Curve);
  LabelAutosize(Label_Text);
  LabelAutosize(Label_TextBlur);
  LabelAutosize(Label_ShadowOffset);
  LabelAutosize(Label_Shape);
  LabelAutosize(Label_Border);
  LabelAutosize(Label_Altitude);

  NoTextureIcon;

  CheckActions(ActionList1);

  initialized := true;
  FirstPaint := true;
end;

procedure TFMain.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ReleaseMouseButtons(Shift);
  CanCompressOrUpdateStack := false;

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
      PaintPicture;
  end;
  UpdateToolbar;
end;

procedure TFMain.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var wantedCursor: TCursor; BmpPos: TPointF;
    virtualScreenPenCursorBefore: boolean;

  function UseVSPenCursor: boolean;
  begin
    if (ToolManager.ToolPenWidth * zoomFactor > 6) and (X >= pictureOrigin.X) and (Y >= pictureOrigin.Y) and
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
  ReleaseMouseButtons(Shift);
  HidePenPreview;
  if LazPaintInstance.TopMostHasFocus then self.SetFocus;

  FormMouseMovePos := Point(X,Y);
  if InFormMouseMove then exit;
  InFormMouseMove := True;
  Application.ProcessMessages; //empty message stack

  virtualScreenPenCursorBefore := virtualScreenPenCursor;
  virtualScreenPenCursor := false;
  case ToolManager.GetCurrentToolType of
  ptHand, ptMoveSelection: wantedCursor := crSizeAll;
  ptRotateSelection: wantedCursor := crCustomRotate;
  ptPen: begin
           wantedCursor := crCustomCrosshair;
           UseVSPenCursor;
        end;
  ptRect,ptEllipse,ptSelectRect,ptSelectEllipse: wantedCursor := crCustomCrosshair;
  ptColorPicker: wantedCursor := crCustomColorPicker;
  ptFloodFill: wantedCursor := crCustomFloodfill;
  ptSelectPen: wantedCursor := crHandPoint;
  ptEraser: begin
              wantedCursor := crDefault;
              UseVSPenCursor;
           end;
  else wantedCursor := crDefault;
  end;
  if cursor <> wantedCursor then Cursor := wantedCursor;
  if PaintBox_Picture.Cursor <> wantedCursor then PaintBox_Picture.Cursor := wantedCursor;

  BmpPos := FormToBitmap(FormMouseMovePos);
  Label_Coordinates.Caption := IntToStr(round(BmpPos.X))+','+IntToStr(round(BmpPos.Y));
  if ToolManager.ToolMove(BmpPos) then
  begin
    PaintPicture;
    ToolManager.ToolMoveAfter(FormToBitmap(FormMouseMovePos)); //new BmpPos after repaint
  end else
    if virtualScreenPenCursor or virtualScreenPenCursorBefore then
      PaintVirtualScreen;
  UpdateToolbar;

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
  end;
  if redraw then PaintPicture;
  UpdateToolbar;
  ReleaseMouseButtons(Shift);
end;

procedure TFMain.FileOpenExecute(Sender: TObject);
begin
  LazPaintInstance.HideTopmost;
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
  if not AbleToSaveAs(filename) then filename := ChangeFileExt(Filename,'.png');
  SavePictureDialog1.FileName := filename;
  LazPaintInstance.HideTopmost;
  if SavePictureDialog1.Execute then
  begin
    filename := SavePictureDialog1.FileName;
    if not AbleToSaveAs(filename) then
      ShowMessage(rsFileExtensionNotSupported) else
    begin
      try
        Image.SaveToFile(UTF8ToSys( filename ));
        SetCurrentFilename(filename);
        image.SetSavedFlag;
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
  if (GetCurrentFilename = '') or not AbleToSaveAs(GetCurrentFilename) then
    FileSaveAs.Execute else
    begin
      AskMergeSelection(rsSave);
      try
        Image.SaveToFile( UTF8ToSys( GetCurrentFilename ));
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

procedure TFMain.FilterPreciseBlurExecute(Sender: TObject);
begin
  ExecuteFilter(pfBlurPrecise);
end;

procedure TFMain.FilterContourExecute(Sender: TObject);
begin
  ExecuteFilter(pfContour);
end;

procedure TFMain.FilterCoronaBlurExecute(Sender: TObject);
begin
  ExecuteFilter(pfBlurCorona);
end;

procedure TFMain.FilterCustomBlurExecute(Sender: TObject);
begin
  ExecuteFilter(pfBlurCustom);
end;

procedure TFMain.FilterDiskBlurExecute(Sender: TObject);
begin
  ExecuteFilter(pfBlurDisk);
end;

procedure TFMain.FilterEmbossExecute(Sender: TObject);
begin
  ExecuteFilter(pfEmboss);
end;

procedure TFMain.FilterGrayscaleExecute(Sender: TObject);
begin
  ExecuteFilter(pfGrayscale);
end;

procedure TFMain.FilterMedianExecute(Sender: TObject);
begin
  ExecuteFilter(pfMedian);
end;

procedure TFMain.FilterMotionBlurExecute(Sender: TObject);
begin
  ExecuteFilter(pfBlurMotion);
end;

procedure TFMain.FilterNegativeExecute(Sender: TObject);
begin
  ExecuteFilter(pfNegative);
end;

procedure TFMain.FilterNormalizeExecute(Sender: TObject);
begin
  ExecuteFilter(pfNormalize);
end;

procedure TFMain.FilterRadialBlurExecute(Sender: TObject);
begin
  ExecuteFilter(pfBlurRadial);
end;

function TFMain.ExecuteFilter(filter: TPictureFilter; skipDialog: boolean = false): boolean;
var
    layer, filteredLayer: TBGRABitmap;
    blurType: TRadialBlurType;
    blurMask,blurMaskCopy,temp: TBGRABitmap;
    shouldSaveUndo, workingOnSelectionLayer: boolean;
begin
  result := false;
  Cursor := crHourGlass;
  shouldSaveUndo := false;
  try
    ToolManager.ToolClose;
    if image.SelectionEmpty then
    begin
      layer := Image.currentImageLayer;
      workingOnSelectionLayer := false;
    end
    else
    begin
      if image.SelectionLayerIsEmpty then
      begin
        image.RetrieveSelectionIfLayerEmpty;
        shouldSaveUndo := true;
      end;
      layer := image.GetOrCreateSelectionLayer;
      workingOnSelectionLayer := true;
    end;

    filteredLayer := nil;
    case filter of
    pfSharpen: filteredLayer := layer.FilterSharpen as TBGRABitmap;
    pfSmooth: filteredLayer := layer.FilterSmooth as TBGRABitmap;
    pfClearTypeInverse: filteredLayer := ClearTypeInverseFilter(layer) as TBGRABitmap;
    pfClearType: filteredLayer := ClearTypeFilter(layer) as TBGRABitmap;
    pfSphere: filteredLayer := layer.FilterSphere as TBGRABitmap;
    pfPlane: filteredLayer := layer.FilterPlane as TBGRABitmap;
    pfCylinder: filteredLayer := layer.FilterCylinder as TBGRABitmap;
    pfNormalize: filteredLayer := layer.FilterNormalize as TBGRABitmap;
    pfMedian: filteredLayer := layer.FilterMedian(moLowSmooth) as TBGRABitmap;
    pfNegative:
      begin
        filteredLayer := layer.Duplicate as TBGRABitmap;
        filteredLayer.Negative;
      end;
    pfLinearNegative:
      begin
        filteredLayer := layer.Duplicate as TBGRABitmap;
        filteredLayer.LinearNegative;
      end;
    pfBlurPrecise, pfBlurRadial, pfBlurCorona, pfBlurDisk, pfBlurFast:
      begin
        LazPaintInstance.HideTopmost;
        case filter of
          pfBlurPrecise: blurType := rbPrecise;
          pfBlurRadial: blurType := rbNormal;
          pfBlurCorona: blurType := rbCorona;
          pfBlurDisk: blurType := rbDisk;
          pfBlurFast: blurType := rbFast;
        end;
        if skipDialog then
          filteredLayer := layer.FilterBlurRadial(Config.DefaultBlurRadius,blurType) as TBGRABitmap
        else
          LazPaintInstance.ShowRadialBlurDlg(layer,filteredLayer,blurType);
        LazPaintInstance.ShowTopmost;
      end;
    pfBlurMotion:
      begin
        LazPaintInstance.HideTopmost;
        if skipDialog then
          filteredLayer := layer.FilterBlurMotion(Config.DefaultBlurMotionDistance,Config.DefaultBlurMotionAngle,Config.DefaultBlurMotionOriented) as TBGRABitmap
        else
          LazPaintInstance.ShowMotionBlurDlg(layer,filteredLayer);
        LazPaintInstance.ShowTopmost;
      end;
    pfBlurCustom:
      begin
        LazPaintInstance.HideTopmost;
        if skipDialog and (Config.DefaultCustomBlurMask <> '') then
        begin
          try
            blurMask := TBGRABitmap.Create(Config.DefaultCustomBlurMask);
            blurMaskCopy := blurMask.FilterGrayscale as TBGRABitmap;
            blurMask.Fill(BGRABlack);
            blurMask.PutImage(0,0,blurMaskCopy,dmDrawWithTransparency);
            blurMaskCopy.Free;
            filteredLayer := layer.FilterCustomBlur(blurMask) as TBGRABitmap;
            blurMask.Free;
          except
            on ex: exception do
            begin
              LazPaintInstance.ShowCustomBlurDlg(layer,filteredLayer);
            end;
          end;
        end
        else
          LazPaintInstance.ShowCustomBlurDlg(layer,filteredLayer);
        LazPaintInstance.ShowTopmost;
      end;
    pfEmboss:
      begin
        LazPaintInstance.HideTopmost;
        if skipDialog then
          filteredLayer := layer.FilterEmboss(Config.DefaultEmbossAngle) as TBGRABitmap
        else
          LazPaintInstance.ShowEmbossDlg(layer,filteredLayer);
        LazPaintInstance.ShowTopmost;
      end;
    pfPixelate:
      begin
        LazPaintInstance.HideTopmost;
        if skipDialog then
          filteredLayer := DoPixelate(layer,Config.DefaultPixelateSize,config.DefaultPixelateQuality)
        else
          LazPaintInstance.ShowPixelateDlg(layer,filteredLayer);
        LazPaintInstance.ShowTopmost;
      end;
    pfTwirl:
      begin
        LazPaintInstance.HideTopmost;
        if skipDialog then
          filteredLayer := layer.FilterTwirl( Point(layer.Width div 2,layer.Height div 2), Config.DefaultTwirlRadius, Config.DefaultTwirlTurn ) as TBGRABitmap
        else
          LazPaintInstance.ShowTwirlDlg(layer,filteredLayer);
        LazPaintInstance.ShowTopmost;
      end;
    pfContour: filteredLayer := layer.FilterContour as TBGRABitmap;
    pfGrayscale: filteredLayer := layer.FilterGrayscale as TBGRABitmap;
    pfPerlinNoise: filteredLayer := CreatePerlinNoiseMap(layer.Width,layer.Height,layer.Width/256,layer.Height/256,1,rfBestQuality);
    pfCyclicPerlinNoise: filteredLayer := CreateCyclicPerlinNoiseMap(layer.Width,layer.Height,layer.Width/256,layer.Height/256,1,rfBestQuality);
    pfClouds:
      begin
        filteredLayer := layer.Duplicate as TBGRABitmap;
        RenderCloudsOn(filteredLayer,ToolManager.ToolForeColor);
      end;
    pfCustomWater:
    begin
      filteredLayer := layer.Duplicate as TBGRABitmap;
      RenderWaterOn(filteredLayer,ToolManager.ToolForeColor,ToolManager.ToolBackColor);
    end;
    pfWater: filteredLayer := CreateWaterTexture(layer.Width,layer.Height);
    pfWood: filteredLayer := CreateWoodTexture(layer.Width,layer.Height);
    pfWoodVertical: filteredLayer := CreateVerticalWoodTexture(layer.Width,layer.Height);
    pfPlastik: filteredLayer := CreatePlastikTexture(layer.Width,layer.Height);
    pfMetalFloor: begin
                     temp := CreateMetalFloorTexture(100);
                     filteredLayer := temp.GetPart(rect(0,0,layer.Width,layer.Height)) as TBGRABitmap;
                     temp.Free;
                  end;
    pfCamouflage: filteredLayer := CreateCamouflageTexture(layer.Width,layer.Height);
    pfSnowPrint: filteredLayer := CreateSnowPrintTexture(layer.Width,layer.Height);
    pfStone: filteredLayer := CreateStoneTexture(layer.Width,layer.Height);
    pfRoundStone: filteredLayer := CreateRoundStoneTexture(layer.Width,layer.Height);
    pfMarble: filteredLayer := CreateMarbleTexture(layer.Width,layer.Height);
    end;

    if LazPaintInstance.BlackAndWhite then
      BGRAReplace(filteredLayer, filteredLayer.FilterGrayscale);

    if filteredLayer <> nil then
    begin
      if workingOnSelectionLayer then
        image.SetSelectionLayer(filteredLayer, true)
      else
        image.ReplaceCurrentLayer(filteredLayer, true);

      image.SaveLayerOrSelectionUndo;
      PaintPicture;
      result:= true;
    end
     else
       if shouldSaveUndo then image.SaveLayerOrSelectionUndo;
  except
    on ex: Exception do
      ShowMessage('ExecuteFilter: '+ex.Message);
  end;
  Cursor := crDefault;
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

procedure TFMain.FilterSharpenExecute(Sender: TObject);
begin
  ExecuteFilter(pfSharpen);
end;

procedure TFMain.FormDestroy(Sender: TObject);
begin
  TextureEditConfig.Free;
  SelectionEditConfig.Free;
  virtualScreen.Free;
  toolbars.Free;
  ForgetSelectionHightlight;
end;

procedure TFMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  if Key = VK_ADD then
  begin
     ViewZoomInExecute(Sender);
     Key := VK_UNKNOWN;
  end else
  if Key = VK_SUBTRACT then
  begin
     ViewZoomOutExecute(Sender);
     Key := VK_UNKNOWN;
  end else
  if ToolManager.ToolKeyDown(Key) then
  begin
    DelayedPaintPicture := True;
    Key := VK_UNKNOWN;
  end;
end;

procedure TFMain.FormResize(Sender: TObject);
begin
  ArrangeToolbars;
end;

procedure TFMain.ImageSmartZoom3Execute(Sender: TObject);
begin
  try
    LazPaintInstance.DoSmartZoom3;
    image.SaveImageUndo;
  except
    on ex:Exception do
      ShowMessage('ImageSmartZoom3Execute: '+ex.Message);
  end;
end;

procedure TFMain.FilterSmoothExecute(Sender: TObject);
begin
  ExecuteFilter(pfSmooth);
end;

procedure TFMain.ViewZoomInExecute(Sender: TObject);
begin
  if TryZoomIn then
    PaintPicture;
end;

procedure TFMain.ViewZoomOriginalExecute(Sender: TObject);
begin
  if zoomFactor <> 1 then
  begin
    zoomFactor := 1;
    PaintPicture;
    UpdateToolbar;
  end;
end;

procedure TFMain.EditUndoExecute(Sender: TObject);
begin
  try
    if ToolManager.GetCurrentToolType = ptTextureMapping then
      ChooseTool(ptHand);
    if image.CanUndo then
    begin
      CanCompressOrUpdateStack := false;
      ToolManager.ToolClose(False);
      image.Undo;
      ToolManager.ToolOpen;
      PaintPicture;
    end;
  except
    on ex:Exception do
      ShowMessage('EditUndoExecute: '+ex.Message);
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
var image3D: TBGRABitmap;
begin
  LazPaintInstance.HideTopmost;
  if Open3DObjectDialog.Execute then
  begin
    try
      image3D := ShowObject3DDlg(LazPaintInstance, Open3DObjectDialog.FileName, Image.Width, Image.Height);
      if image3D <> nil then
      begin
        if image3D.NbPixels <> 0 then
        begin
          image.ReleaseSelection;
          ChooseTool(ptMoveSelection);
          image.QuerySelection;
          image.GetOrCreateSelectionLayer.PutImage((image.Width - image3D.Width) div 2,
             (image.Height - image3D.Height) div 2,image3D,dmFastBlend);
          image.currentSelection.FillRect((image.Width - image3D.Width) div 2,
             (image.Height - image3D.Height) div 2,
             (image.Width - image3D.Width) div 2+image3D.Width,
             (image.Height - image3D.Height) div 2+image3D.Height,BGRAWhite,dmSet);
          image.SelectionMayChange;
          image.SelectionOffset.X := - image.ImageOffset.X;
          image.SelectionOffset.Y := - image.ImageOffset.Y;
          image.SaveLayerOrSelectionUndo;
          PaintPicture;
        end;
        image3D.Free;
      end;
    except
      on ex:Exception do
        ShowMessage('EditPasteExecute: '+ex.Message);
    end;
  end;
  LazPaintInstance.ShowTopmost;
end;

procedure TFMain.FileLoadSelectionExecute(Sender: TObject);
var
  newSelection: TBGRABitmap;
begin
  LazPaintInstance.HideTopmost;
  if LoadSelectionDialog.Execute then
  begin
    try
      newSelection := TBGRABitmap.Create(UTF8ToSys(LoadSelectionDialog.FileName));
      BGRAReplace(newSelection,newSelection.FilterGrayscale);
      ToolManager.ToolClose;
      if not (ToolManager.GetCurrentToolType in[ptMoveSelection,ptRotateSelection]) then
        ChooseTool(ptMoveSelection);
      image.QuerySelection;
      Image.currentSelection.Fill(BGRABlack);
      Image.currentSelection.PutImage(0,0,newSelection,dmSet);
      newSelection.Free;
      image.SelectionMayChange;
      image.SaveLayerOrSelectionUndo;
      PaintPicture;
      SaveSelectionDialog.Filename := LoadSelectionDialog.Filename;
    except
      on ex: exception do
      begin

      end;
    end;
  end;
  LazPaintInstance.ShowTopmost;
end;

procedure TFMain.FileSaveSelectionAsExecute(Sender: TObject);
var filename: string;
begin
  if Image.SelectionEmpty then exit;
  LazPaintInstance.HideTopmost;
  if SaveSelectionDialog.Execute then
  begin
    filename := SaveSelectionDialog.FileName;
    if not AbleToSaveAs(filename) then
      ShowMessage(rsFileExtensionNotSupported) else
    begin
      try
        Image.currentSelection.SaveToFile(UTF8ToSys( filename ));
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

procedure TFMain.FilterClearTypeExecute(Sender: TObject);
begin
  ExecuteFilter(pfClearType);
end;

procedure TFMain.FilterClearTypeInverseExecute(Sender: TObject);
begin
  ExecuteFilter(pfClearTypeInverse);
end;

procedure TFMain.FilterPixelateExecute(Sender: TObject);
begin
  ExecuteFilter(pfPixelate);
end;

procedure TFMain.FilterTwirlExecute(Sender: TObject);
begin
  ExecuteFilter(pfTwirl);
end;

procedure TFMain.FormDropFiles(Sender: TObject; const FileNames: array of String
  );
begin
  TryOpenFile(FileNames[0]);
end;

procedure TFMain.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if ToolManager.ToolKeyUp(Key) then
  begin
    DelayedPaintPicture := True;
    Key := VK_UNKNOWN;
  end;
end;

procedure TFMain.FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  if ToolManager.ToolKeyPress(UTF8Key) then
  begin
    UTF8Key := '';
    if ToolManager.ToolUpdateNeeded then
      DelayedPaintPicture := true
    else
      PaintPicture;
  end;
end;

procedure TFMain.ImageCropLayerExecute(Sender: TObject);
begin
  CropToSelectionAndLayer;
end;

procedure TFMain.ImageCropLayerUpdate(Sender: TObject);
begin
  ImageCropLayer.Enabled := not image.SelectionEmpty;
  ImageCropLayer.Visible := (image.NbLayers > 1);
end;

procedure TFMain.MenuDonateClick(Sender: TObject);
begin
  OpenURL('http://sourceforge.net/donate/index.php?group_id=404555');
end;

procedure TFMain.MenuHorizFlipLayerClick(Sender: TObject);
begin
  try
    LazPaintInstance.DoHorizontalFlip(foCurrentLayer);
    image.SaveLayerOrSelectionUndo;
  except
    on ex:Exception do
      ShowMessage('ImageHorizontalFlipExecute: '+ex.Message);
  end;
end;

procedure TFMain.MenuHorizFlipPictureClick(Sender: TObject);
begin
  try
    LazPaintInstance.DoHorizontalFlip(foWholePicture);
    image.SaveLayerOrSelectionUndo;
  except
    on ex:Exception do
      ShowMessage('ImageHorizontalFlipExecute: '+ex.Message);
  end;
end;

procedure TFMain.MenuHorizFlipSelectionClick(Sender: TObject);
begin
  try
    LazPaintInstance.DoHorizontalFlip(foSelection);
    image.SaveLayerOrSelectionUndo;
  except
    on ex:Exception do
      ShowMessage('ImageHorizontalFlipExecute: '+ex.Message);
  end;
end;

procedure TFMain.MenuItem25Click(Sender: TObject);
begin
  MenuHorizFlipLayer.Visible := image.NbLayers > 1;
  MenuHorizFlipSelection.Visible := not image.SelectionEmpty;
  MenuHorizFlip.Visible := not MenuHorizFlipLayer.Visible and not MenuHorizFlipSelection.Visible;
  MenuHorizFlipSub.Visible := not MenuHorizFlip.Visible;
  MenuVertFlipLayer.Visible := image.NbLayers > 1;
  MenuVertFlipSelection.Visible := not image.SelectionEmpty;
  MenuVertFlip.Visible := not MenuVertFlipLayer.Visible and not MenuVertFlipSelection.Visible;
  MenuVertFlipSub.Visible := not MenuVertFlip.Visible;
end;

procedure TFMain.MenuVertFlipLayerClick(Sender: TObject);
begin
  try
    LazPaintInstance.DoVerticalFlip(foCurrentLayer);
    image.SaveLayerOrSelectionUndo;
  except
    on ex:Exception do
      ShowMessage('ImageHorizontalFlipExecute: '+ex.Message);
  end;
end;

procedure TFMain.MenuVertFlipPictureClick(Sender: TObject);
begin
  try
    LazPaintInstance.DoVerticalFlip(foWholePicture);
    image.SaveLayerOrSelectionUndo;
  except
    on ex:Exception do
      ShowMessage('ImageHorizontalFlipExecute: '+ex.Message);
  end;
end;

procedure TFMain.MenuVertFlipSelectionClick(Sender: TObject);
begin
  try
    LazPaintInstance.DoVerticalFlip(foSelection);
    image.SaveLayerOrSelectionUndo;
  except
    on ex:Exception do
      ShowMessage('ImageHorizontalFlipExecute: '+ex.Message);
  end;
end;

procedure TFMain.ShapeAltitude_SpinEditChange(Sender: TObject);
begin
  if initialized then
  begin
    ToolManager.ToolShapeAltitude := ShapeAltitude_SpinEdit.Value;
    UpdateEditPicture;
  end;
end;

procedure TFMain.TextBlur_SpinEditChange(Sender: TObject);
begin
  if initialized then
  begin
    ToolManager.ToolTextBlur := TextBlur_SpinEdit.Value;
    UpdateEditPicture(True);
  end;
end;

procedure TFMain.GridNb_SpinEditChange(Sender: TObject);
begin
  if not initialized then exit;
  ToolManager.SetToolDeformationGridSize(GridNbX_SpinEdit.Value+1,GridNbY_SpinEdit.Value+1);
  PaintPicture;
end;

procedure TFMain.Image_CurrentTextureMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  LazPaintInstance.HideTopmost;
  try
    if TextureEditConfig = nil then
      TextureEditConfig := TStringStream.Create('[General]'+LineEnding+
        'DefaultImageWidth=256'+LineEnding+
        'DefaultImageHeight=256'+LineEnding);
    LazPaintInstance.EditBitmap(ToolManager.ToolTexture,TextureEditConfig,rsEditTexture,nil,nil,LazPaintInstance.BlackAndWhite);
    UpdateTextureIcon;
    UpdateEditPicture;
  except
    on ex: Exception do
      ShowMessage(ex.Message);
  end;
  LazPaintInstance.ShowTopmost;
end;

procedure TFMain.MnuCheckUpdateClick(Sender: TObject);
var
  AProcess: TProcess;
begin
  AProcess := TProcess.Create(nil);
  AProcess.CommandLine := UpdateCheckerFilename+' ' + LazPaintCurrentVersion;
  AProcess.Execute;
  AProcess.Free;
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
  OnPaintHandler;
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

procedure TFMain.PenWidth_SpinEditMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  ShowPenPreview(False);
end;

procedure TFMain.RenderCamouflageExecute(Sender: TObject);
begin
  ExecuteFilter(pfCamouflage);
end;

procedure TFMain.RenderCloudsExecute(Sender: TObject);
begin
  ExecuteFilter(pfClouds);
end;

procedure TFMain.FilterCylinderExecute(Sender: TObject);
begin
  ExecuteFilter(pfCylinder);
end;

procedure TFMain.FilterFastBlurExecute(Sender: TObject);
begin
  ExecuteFilter(pfBlurFast);
end;

procedure TFMain.FilterNegativeLinearExecute(Sender: TObject);
begin
  ExecuteFilter(pfLinearNegative);
end;

procedure TFMain.RenderMarbleExecute(Sender: TObject);
begin
  ExecuteFilter(pfMarble);
end;

procedure TFMain.RenderMetalFloorExecute(Sender: TObject);
begin
  ExecuteFilter(pfMetalFloor);
end;

procedure TFMain.RenderPerlinNoiseCyclicExecute(Sender: TObject);
begin
  ExecuteFilter(pfCyclicPerlinNoise);
end;

procedure TFMain.RenderPerlinNoiseExecute(Sender: TObject);
begin
  ExecuteFilter(pfPerlinNoise);
end;

procedure TFMain.FilterPlaneExecute(Sender: TObject);
begin
  ExecuteFilter(pfPlane);
end;

procedure TFMain.FilterSphereExecute(Sender: TObject);
begin
  ExecuteFilter(pfSphere);
end;

procedure TFMain.RenderPlastikExecute(Sender: TObject);
begin
  ExecuteFilter(pfPlastik);
end;

procedure TFMain.RenderRoundStoneExecute(Sender: TObject);
begin
  ExecuteFilter(pfRoundStone);
end;

procedure TFMain.RenderSnowPrintExecute(Sender: TObject);
begin
  ExecuteFilter(pfSnowPrint);
end;

procedure TFMain.RenderStoneExecute(Sender: TObject);
begin
  ExecuteFilter(pfStone);
end;

procedure TFMain.RenderCustomWaterExecute(Sender: TObject);
begin
  ExecuteFilter(pfCustomWater);
end;

procedure TFMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if not LazPaintInstance.Embedded and not image.IsFileModified and not image.Empty then
  begin
    LazPaintInstance.HideTopmost;
    case MessageDlg(rsExitRequest,rsSaveChanges,mtWarning,mbYesNoCancel,0) of
    IDYES: FileSaveExecute(Sender);
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
end;

procedure TFMain.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
Var
    SaveOfs,NewOfs: TPointF;
    zoomChange: boolean;
    pa: TRect;
begin
  SaveOfs := FormToBitmap(MousePos.X,MousePos.Y);
  zoomChange := false;
  if WheelDelta > 0 then zoomChange := TryZoomIn else
  if WheelDelta < 0 then zoomChange := TryZoomOut;
  if zoomChange then
  begin
    //approx image position
    pa := GetPictureArea;
    PictureActualSize := point(image.Width,image.Height);
    pictureViewSize := point(round(PictureActualSize.X*zoomFactor),round(PictureActualSize.Y*zoomFactor));
    PictureOrigin := Point((pa.Left+pa.Right-PictureViewSize.X) div 2+round(image.ImageOffset.X*zoomFactor),
        (pa.Top+pa.Bottom-PictureViewSize.Y) div 2+round(image.ImageOffset.Y*zoomFactor));
    PictureOffset := Point(0,0);
    NewOfs := FormToBitmap(MousePos.X,MousePos.Y);
    image.ImageOffset:= point(image.ImageOffset.X+round(NewOfs.X-SaveOfs.X),
         image.ImageOffset.Y+round(NewOfs.Y-SaveOfs.Y));
    PaintPicture;
  end;
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

procedure TFMain.IdleTimer1Timer(Sender: TObject);
begin
  IdleTimer1.Enabled := false;
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
    PaintPicture;
  end;
  IdleTimer1.Enabled := true;
  //Caption := inttostr(UndoSize div 1048576)+' Mo';
end;

procedure TFMain.ImageChangeCanvasSizeExecute(Sender: TObject);
begin
  LazPaintInstance.ShowCanvasSizeDlg;
end;

procedure TFMain.ImageClearAlphaExecute(Sender: TObject);
var c: TBGRAPixel;
    n: integer;
    p: PBGRAPixel;
begin
  try
    ToolManager.ToolClose;
    c := ToolManager.ToolBackColor;
    c.alpha := 255;
    image.currentImageLayer.ReplaceColor(BGRAPixelTransparent,c);
    p := image.currentImageLayer.Data;
    for n := image.currentImageLayer.NbPixels-1 downto 0 do
    begin
       p^.alpha := 255;
       inc(p);
    end;
    image.currentImageLayer.InvalidateBitmap;
    Image.ImageMayChangeCompletely;
    image.SaveLayerOrSelectionUndo;
    PaintPicture;
  except
    on ex:Exception do
      ShowMessage('ImageClearAlphaExecute: '+ex.Message);
  end;
end;

procedure TFMain.ImageCropExecute(Sender: TObject);
begin
  CropToSelection;
end;

procedure TFMain.ImageCropUpdate(Sender: TObject);
begin
    ImageCrop.Enabled := not image.SelectionEmpty;
end;

procedure TFMain.ImageFillBackgroundExecute(Sender: TObject);
var tempBmp: TBGRABitmap;
    c: TBGRAPixel;
begin
  try
    ToolManager.ToolClose;
    c := ToolManager.ToolBackColor;
    c.alpha := 255;
    tempBmp := TBGRABitmap.Create(image.Width,image.Height,c);
    tempBmp.PutImage(0,0,image.currentImageLayer,dmDrawWithTransparency);
    image.ReplaceCurrentLayer(tempBmp, True);
    image.SaveLayerOrSelectionUndo;
    PaintPicture;
  except
    on ex:Exception do
      ShowMessage('ImageFillBackgroundExecute: '+ex.Message);
  end;
end;

procedure TFMain.ImageRepeatExecute(Sender: TObject);
begin
  LazPaintInstance.ShowRepeatImageDlg;
end;

procedure TFMain.ImageRotateCCWExecute(Sender: TObject);
begin
  try
    LazPaintInstance.DoRotateCCW;
    Image.SaveImageUndo;
  except
    on ex:Exception do
      ShowMessage('ImageRotateCCWExecute: '+ex.Message);
  end;
end;

procedure TFMain.ImageRotateCWExecute(Sender: TObject);
begin
  try
    LazPaintInstance.DoRotateCW;
    Image.SaveImageUndo;
  except
    on ex:Exception do
      ShowMessage('ImageRotateCWExecute: '+ex.Message);
  end;
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
     EmptyMenu(Menu_RecentFiles);
     for i := 0 to Config.RecentFilesCount-1 do
     if Config.RecentFile[i]<>GetCurrentFilename then
     begin
       item := NewItem(Config.RecentFile[i],0,false,true,@RecentFileClick,0,'');
       Menu_RecentFiles.Add(item);
     end;
     Menu_RecentFiles.Enabled := Menu_RecentFiles.Count <> 0;

     EmptyMenu(Menu_Language);
     currentLanguage := Config.DefaultLangage;
     for i := 0 to Config.Languages.Count-1 do
     begin
       item := NewItem(Config.Languages[i],0,false,true,@LanguageClick,0,'');
       if currentLanguage = item.Caption then
         item.Checked := true;
       Menu_Language.Add(item);
     end;
     Menu_Language.Enabled := Menu_Language.Count <> 0;
end;

procedure TFMain.RecentFileClick(Sender: TObject);
begin
    if Sender is TMenuItem then
      TryOpenFile((sender as TMenuItem).Caption);
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

procedure TFMain.RenderWaterAutoExecute(Sender: TObject);
begin
  ExecuteFilter(pfWater);
end;

procedure TFMain.RenderWoodExecute(Sender: TObject);
begin
  ExecuteFilter(pfWood);
end;

procedure TFMain.RenderWoodVerticalExecute(Sender: TObject);
begin
  ExecuteFilter(pfWoodVertical);
end;

procedure TFMain.TextShadowX_SpinEditChange(Sender: TObject);
begin
  if initialized then
  begin
    ToolManager.ToolTextShadowOffset.X := TextShadowX_SpinEdit.Value;
    UpdateEditPicture(True);
  end;
end;

procedure TFMain.TextShadowY_SpinEditChange(Sender: TObject);
begin
  if initialized then
  begin
    ToolManager.ToolTextShadowOffset.Y := TextShadowY_SpinEdit.Value;
    UpdateEditPicture(True);
  end;
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

procedure TFMain.ToolButton_ShapeHorizontalCylinderClick(Sender: TObject);
begin
  if ToolButton_ShapeHorizontalCylinder.Down then
  begin
    if initialized then ToolManager.ToolShapeType := 'HorizontalCylinder';
    UpdatePanelPhongShape;
  end
end;

procedure TFMain.ToolButton_ShapeVerticalConeClick(Sender: TObject);
begin
  if ToolButton_ShapeVerticalCone.Down then
  begin
    if initialized then ToolManager.ToolShapeType := 'VerticalCone';
    UpdatePanelPhongShape;
  end
end;

procedure TFMain.ToolButton_ShapeVerticalCylinderClick(Sender: TObject);
begin
  if ToolButton_ShapeVerticalCylinder.Down then
  begin
    if initialized then ToolManager.ToolShapeType := 'VerticalCylinder';
    UpdatePanelPhongShape;
  end
end;

procedure TFMain.ToolTextureMappingExecute(Sender: TObject);
begin
  ChooseTool(ptTextureMapping);
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
       UpdateEditPicture;
     end;
  end;
end;

procedure TFMain.ToolButton_GridMoveWithoutDeformationClick(Sender: TObject);
begin
    if initialized then
      ToolManager.ToolDeformationGridMoveWithoutDeformation :=
        ToolButton_GridMoveWithoutDeformation.Down;
end;

procedure TFMain.ToolButton_ShapeConeClick(Sender: TObject);
begin
    if ToolButton_ShapeCone.Down then
    begin
      if initialized then ToolManager.ToolShapeType := 'Cone';
      UpdatePanelPhongShape;
    end;
end;

procedure TFMain.ToolButton_ShapeRectangleClick(Sender: TObject);
begin
    if ToolButton_ShapeRectangle.Down then
    begin
      if initialized then ToolManager.ToolShapeType := 'Rectangle';
      UpdatePanelPhongShape;
    end;
end;

procedure TFMain.ToolButton_ShapeRoundRectClick(Sender: TObject);
begin
    if ToolButton_ShapeRoundRect.Down then
    begin
      if initialized then ToolManager.ToolShapeType := 'RoundRectangle';
      UpdatePanelPhongShape;
    end;
end;

procedure TFMain.ToolButton_ShapeSphereClick(Sender: TObject);
begin
    if ToolButton_ShapeSphere.Down then
    begin
      if initialized then ToolManager.ToolShapeType := 'Sphere';
      UpdatePanelPhongShape;
    end;
end;

procedure TFMain.ToolButton_SinGradientClick(Sender: TObject);
begin
  if initialized then
  begin
    ToolManager.ToolGradientSine := ToolButton_SinGradient.Down;
    UpdateEditPicture;
  end;
end;

procedure TFMain.ToolDeformationExecute(Sender: TObject);
begin
  ChooseTool(ptDeformation);
end;

procedure TFMain.ToolLoadTextureExecute(Sender: TObject);
begin
  ShowOpenTextureDialog;
end;

procedure TFMain.ToolMagicWandExecute(Sender: TObject);
begin
  ChooseTool(ptMagicWand);
end;

procedure TFMain.ToolNoTextureExecute(Sender: TObject);
begin
  try
    ToolManager.ToolTexture.Free;
    ToolManager.ToolTexture := nil;

    UpdateTextureIcon;
    UpdateEditPicture;

  except
    on ex:Exception do
      ShowMessage('ToolNoTextureExecute: '+ex.Message);
  end;
end;

procedure TFMain.ToolNoTextureUpdate(Sender: TObject);
begin
  ToolNoTexture.Enabled := ToolManager.ToolTexture <> nil;
end;

procedure TFMain.ToolPhongExecute(Sender: TObject);
begin
  ChooseTool(ptPhong);
end;

procedure TFMain.ToolTextExecute(Sender: TObject);
begin
  ChooseTool(ptText);
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
  MenuItem_ViewColors.Checked := ViewColors.Checked;
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
var zx,zy: single;
    pa: TRect;
begin
  try
    pa := GetPictureArea;
    zx := (pa.right-pa.left)/image.Width;
    zy := (pa.bottom-pa.top)/image.height;
    zoomFactor := exp(floor(ln(min(zx,zy))/ln(2))*ln(2));
    PaintPicture;
    UpdateToolbar;
  except
    on ex:Exception do
      ShowMessage('ViewZoomFitExecute: '+ex.Message);
  end;
end;

procedure TFMain.EditRedoExecute(Sender: TObject);
begin
  try
    CanCompressOrUpdateStack := false;
    Image.SelectionOffset := Point(0,0);
    Image.SelectionRotateAngle := 0;
    ToolManager.ToolClose;
    image.Redo;
    PaintPicture;
    CanCompressOrUpdateStack := true;
  except
    on ex:Exception do
      ShowMessage('EditRedoExecute: '+ex.Message);
  end;
end;

procedure TFMain.EditInvertSelectionExecute(Sender: TObject);
begin
  try
    if not ToolManager.IsSelectingTool then ChooseTool(ptSelectRect);
    image.QuerySelection;
    image.currentSelection.Negative;
    image.SelectionMayChange;
    image.SaveLayerOrSelectionUndo;
    PaintPicture;
  except
    on ex:Exception do
      ShowMessage('EditInvertSelectionExecute: '+ex.Message);
  end;
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
      image.Assign(bmp,true);
      SetCurrentFilename('');
      image.SaveImageUndo;
      image.SetSavedFlag;
      PaintPicture;
    end
     else
       bmp.Free;
  end;
end;

procedure TFMain.EditDeselectExecute(Sender: TObject);
begin
  try
    if not image.SelectionEmpty then
    begin
      if (ToolManager.GetCurrentToolType in[ptRotateSelection,ptMoveSelection]) then
      begin
        image.ReleaseSelection;
        ToolManager.ToolClose;
        ChooseTool(ptHand);
      end else
      begin
        ToolManager.ToolClose;
        image.ReleaseSelection;
      end;
      image.SaveLayerOrSelectionUndo;
      PaintPicture;
    end;
  except
    on ex:Exception do
      ShowMessage('EditDeselectExecute: '+ex.Message);
  end;
end;

procedure TFMain.DoCopySelection;
var layer, partial, backupSelectionLayer : TBGRABitmap; r: TRect;
begin
  if image.SelectionEmpty then exit;

  if image.GetSelectionLayerIfExists <> nil then
    backupSelectionLayer := image.GetSelectionLayerIfExists.Duplicate as TBGRABitmap else
      backupSelectionLayer := nil;
  image.ApplySelectionMask;
  image.RetrieveSelectionIfLayerEmpty;
  layer := image.GetOrCreateSelectionLayer;
  r := layer.GetImageBounds;
  if (r.right > r.left) and (r.bottom > r.top) then
  begin
    partial := TBGRABitmap.Create(r.Right-r.left,r.Bottom-r.top);
    partial.PutImage(-r.left,-r.Top,layer,dmSet);
    CopyToClipboard(partial);
    partial.Free;
  end;
  image.SetSelectionLayer(backupSelectionLayer,True);
end;

procedure TFMain.ChooseTool(Tool: TPaintToolType);
begin
  try
    if self.Visible then
    begin
      case Tool of
        ptTextureMapping:
        if (ToolManager.ToolTexture = nil) or ToolManager.ToolTexture.Empty then
        begin
          if not ShowOpenTextureDialog then exit;
          if (ToolManager.ToolTexture = nil) or ToolManager.ToolTexture.Empty then exit;
        end;
        ptDeformation:
        begin
          if image.currentImageLayer.Equals(image.currentImageLayer.GetPixel(0,0)) then
          begin
            LazPaintInstance.HideTopmost;
            ShowMessage(rsNothingToBeDeformed);
            LazPaintInstance.ShowTopmost;
            exit;
          end;
        end;
        ptMoveSelection,ptRotateSelection:
        begin
          if not image.SelectionEmpty and image.SelectionLayerIsEmpty and not image.currentImageLayer.Empty then
          begin
            LazPaintInstance.HideTopmost;
            case MessageDlg(rsMovingOrRotatingSelection,rsRetrieveSelectedArea,mtConfirmation,[mbYes,mbNo],0) of
              mrYes: begin
                  ToolManager.ToolClose;
                  image.RetrieveSelectionIfLayerEmpty(True);
                  Image.SaveLayerOrSelectionUndo;
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
    if self.Visible then
    begin
      ArrangeToolbars;
      PaintPicture;
      UpdateToolBar;
    end;
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
  Case ToolManager.GetCurrentToolType of
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
  ptSelectSpline: img := ToolSelectCurve.ImageIndex;
  ptSelectPen: img := ToolSelectionPen.ImageIndex;
  ptMoveSelection: img := ToolMoveSelection.ImageIndex;
  ptRotateSelection: img := ToolRotateSelection.ImageIndex;
  ptMagicWand: img := ToolMagicWand.ImageIndex;
  ptText: img := ToolText.ImageIndex;
  ptDeformation: img := ToolDeformation.ImageIndex;
  ptTextureMapping: img := ToolTextureMapping.ImageIndex;
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
  MenuItem_ViewGrid.Checked := ViewGrid.Checked;
  LazPaintInstance.GridVisible := ViewGrid.Checked;
end;

procedure TFMain.ToggleToolboxVisible;
begin
  ViewToolBox.Checked := not ViewToolBox.Checked;
  MenuItem_ViewToolbox.Checked := ViewToolBox.Checked;
  LazPaintInstance.ToolboxVisible := ViewToolBox.Checked;
end;

procedure TFMain.ToggleColorsVisible;
begin
  ViewColors.Checked := not ViewColors.Checked;
  MenuItem_ViewColors.Checked := ViewColors.Checked;
  LazPaintInstance.ChooseColorVisible := ViewColors.Checked;
end;

procedure TFMain.ToggleLayersVisible;
begin
  LazPaintInstance.LayerWindowVisible := not LazPaintInstance.LayerWindowVisible;
end;

procedure TFMain.EditCopyExecute(Sender: TObject);
begin
  try
    ToolManager.ToolClose;
    DoCopySelection;
  except
    on ex:Exception do
      ShowMessage('EditCopyExecute: '+ex.Message);
  end;
end;

procedure TFMain.ColorIntensityExecute(Sender: TObject);
begin
  LazPaintInstance.HideTopMost;
  try
    ToolManager.ToolClose;
    if not image.SelectionLayerIsEmpty then
    begin
      ShowSelectionNormal := True;
      LazPaintInstance.ShowColorIntensityDlg(image.GetSelectionLayerIfExists);
      ShowSelectionNormal := false;
    end else
      LazPaintInstance.ShowColorIntensityDlg(image.currentImageLayer);
  except
    on ex:Exception do
      ShowMessage('ColorIntensityExecute: '+ex.Message);
  end;
  LazPaintInstance.ShowTopMost;
end;

procedure TFMain.ColorShiftColorExecute(Sender: TObject);
begin
  LazPaintInstance.HideTopMost;

  try
    ToolManager.ToolClose;
    if not image.SelectionLayerIsEmpty then
    begin
      ShowSelectionNormal := True;
      LazPaintInstance.ShowShiftColorsDlg(image.GetSelectionLayerIfExists);
      ShowSelectionNormal := false;
    end else
      LazPaintInstance.ShowShiftColorsDlg(image.currentImageLayer);
  except
    on ex:Exception do
      ShowMessage('ColorShiftColorExecute: '+ex.Message);
  end;

  LazPaintInstance.ShowTopMost;
end;

procedure TFMain.ColorColorizeExecute(Sender: TObject);
begin
  LazPaintInstance.HideTopMost;

  try
    ToolManager.ToolClose;
    if not image.SelectionLayerIsEmpty then
    begin
      ShowSelectionNormal := True;
      LazPaintInstance.ShowColorizeDlg(image.GetSelectionLayerIfExists);
      ShowSelectionNormal := false;
    end else
      LazPaintInstance.ShowColorizeDlg(image.currentImageLayer);
  except
    on ex:Exception do
      ShowMessage('ColorColorizeExecute: '+ex.Message);
  end;

  LazPaintInstance.ShowTopMost;
end;

procedure TFMain.BorderSize_SpinEditChange(Sender: TObject);
begin
  if initialized then
    ToolManager.ToolShapeBorderSize := BorderSize_SpinEdit.Value;
end;

procedure TFMain.ColorLightnessExecute(Sender: TObject);
begin
  LazPaintInstance.HideTopMost;
  try
    ToolManager.ToolClose;
    if not image.SelectionLayerIsEmpty then
    begin
      ShowSelectionNormal := True;
      LazPaintInstance.ShowColorLightnessDlg(image.GetSelectionLayerIfExists);
      ShowSelectionNormal := false;
    end else
      LazPaintInstance.ShowColorLightnessDlg(image.currentImageLayer);
  except
    on ex:Exception do
      ShowMessage('ColorLightnessExecute: '+ex.Message);
  end;
  LazPaintInstance.ShowTopMost;
end;

procedure TFMain.Combo_SplineStyleChange(Sender: TObject);
var v: string;
begin
  if initialized then
  begin
    v := Combo_SplineStyle.Text;
    if v = 'Inside' then ToolManager.ToolSplineStyle := ssInside else
    if v = 'Inside + ends' then ToolManager.ToolSplineStyle := ssInsideWithEnds else
    if v = 'Crossing' then ToolManager.ToolSplineStyle := ssCrossing else
    if v = 'Crossing + ends' then ToolManager.ToolSplineStyle := ssCrossingWithEnds else
    if v = 'Outside' then ToolManager.ToolSplineStyle := ssOutside else
    if v = 'Round outside' then ToolManager.ToolSplineStyle:= ssRoundOutside else
    if v = 'Vertex to side' then ToolManager.ToolSplineStyle:= ssVertexToSide;
    PaintPicture;
  end;
end;

procedure TFMain.EditCutExecute(Sender: TObject);
begin
  try
    ToolManager.ToolClose;
    if image.SelectionEmpty then exit;
    DoCopySelection;
    if (image.GetSelectionLayerIfExists = nil) or (image.GetSelectionLayerIfExists.Empty) then
      image.EraseSelectionInBitmap;
    image.RemoveSelection;
    image.SaveLayerOrSelectionUndo;
    if (ToolManager.GetCurrentToolType = ptRotateSelection) or
       (ToolManager.GetCurrentToolType = ptMoveSelection) then
      ChooseTool(ptHand);
    PaintPicture;
  except
    on ex:Exception do
      ShowMessage('EditCutExecute: '+ex.Message);
  end;
end;

procedure TFMain.EditDeleteSelectionExecute(Sender: TObject);
begin
  try
    ToolManager.ToolClose;
    if image.SelectionEmpty then exit;
    image.RetrieveSelectionIfLayerEmpty(true);
    image.RemoveSelection;
    image.SaveLayerOrSelectionUndo;
    if (ToolManager.GetCurrentToolType = ptRotateSelection) or
       (ToolManager.GetCurrentToolType = ptMoveSelection) then
      ChooseTool(ptHand);
    PaintPicture;
  except
    on ex:Exception do
      ShowMessage('EditDeleteSelectionExecute: '+ex.Message);
  end;
end;

procedure TFMain.EditPasteExecute(Sender: TObject);
var partial: TBGRABitmap;
begin
  try
    partial := GetBitmapFromClipboard;
    if partial<>nil then
    begin
      if partial.NbPixels <> 0 then
      begin
        image.ReleaseSelection;
        ChooseTool(ptMoveSelection);
        image.QuerySelection;
        image.GetOrCreateSelectionLayer.PutImage((image.Width - partial.Width) div 2,
           (image.Height - partial.Height) div 2,partial,dmFastBlend);
        ComputeSelectionMask(image.GetOrCreateSelectionLayer,image.currentSelection);
        image.SelectionMayChange;
        image.SelectionOffset.X := - image.ImageOffset.X;
        image.SelectionOffset.Y := - image.ImageOffset.Y;
        image.SaveLayerOrSelectionUndo;
        PaintPicture;
      end;
      partial.Free;
    end;
  except
    on ex:Exception do
      ShowMessage('EditPasteExecute: '+ex.Message);
  end;
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
  try
    if not ToolManager.IsSelectingTool then ChooseTool(ptSelectRect);
    image.QuerySelection;
    image.currentSelection.Fill(BGRAWhite);
    image.SelectionMayChange;
    image.SaveLayerOrSelectionUndo;
    PaintPicture;
  except
    on ex:Exception do
      ShowMessage('EditSelectAllExecute: '+ex.Message);
  end;
end;

procedure TFMain.EditSelectionExecute(Sender: TObject);
var lSelection,lTemp: TBGRABitmap;
begin
  LazPaintInstance.HideTopmost;
  try

    ToolManager.ToolClose;
    if SelectionEditConfig = nil then
      SelectionEditConfig := TStringStream.Create('[Tool]'+LineEnding+
        'ForeColor=FFFFFFFF'+LineEnding+
        'BackColor=000000FF');
    Image.QuerySelection;
    lSelection:= Image.currentSelection.Duplicate as TBGRABitmap;
    lSelection.ConvertFromLinearRGB;
    LazPaintInstance.EditBitmap(lSelection,SelectionEditConfig,rsEditSelection,@SelectionInstanceOnRun,nil,True);
    BGRAReplace(lSelection,lSelection.FilterGrayscale);
    lTemp := TBGRABitmap.Create(lSelection.Width,lSelection.Height,BGRABlack);
    lTemp.PutImage(0,0,lSelection,dmDrawWithTransparency);
    lSelection.Free;
    lSelection := lTemp;
    lTemp := nil;
    lSelection.ConvertToLinearRGB;
    Image.currentSelection := lSelection;
    Image.SaveLayerOrSelectionUndo;
    PaintPicture;
  except
    on ex: Exception do
      ShowMessage(ex.Message);
  end;
  LazPaintInstance.ShowTopmost;
end;

procedure TFMain.EditSelectionFitExecute(Sender: TObject);
begin
  try
    ToolManager.ToolClose;
    image.ApplySelectionMask;
    if image.SelectionEmpty then
    begin
      image.QuerySelection;
      image.currentSelection.Fill(BGRAWhite);
    end;
    image.RetrieveSelectionIfLayerEmpty(True);
    ComputeSelectionMask(image.GetOrCreateSelectionLayer,image.currentSelection);
    if image.SelectionEmpty then
    begin
      if (ToolManager.GetCurrentToolType = ptRotateSelection) or
         (ToolManager.GetCurrentToolType  = ptMoveSelection) then
        ChooseTool(ptHand);
    end else
      if not ToolManager.IsSelectingTool then ChooseTool(ptMoveSelection);
    image.SelectionMayChange;
    PaintPicture;
  except
    on ex:Exception do
      ShowMessage('EditSelectionFitExecute: '+ex.Message);
  end;
end;

procedure TFMain.ViewZoomOutExecute(Sender: TObject);
begin
  if TryZoomOut then
    PaintPicture;
end;

procedure TFMain.Eraser_SpinEditChange(Sender: TObject);
begin
    if initialized then
      ToolManager.ToolEraserAlpha := Eraser_SpinEdit.Value;
end;

procedure TFMain.FileNewExecute(Sender: TObject);
var
  bitmapRepl: TBGRABitmap;
begin
  LazPaintInstance.HideTopmost;
  if LazPaintInstance.ShowNewImageDlg(bitmapRepl) then
  begin
    ChooseTool(ptHand);
    image.Assign(bitmapRepl, True);
    SetCurrentFilename('');
    image.SaveImageUndo;
    image.SetSavedFlag;
    PaintPicture;
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

function TFMain.TryZoomIn: boolean;
var pa: TRect;
begin
  try
    pa := GetPictureArea;
    if (zoomFactor < (pa.right-pa.left)/8) and (zoomFactor < (pa.bottom-pa.top)/8) then
    begin
      zoomFactor *= 2;
      UpdateToolbar;
      result := true;
    end else
      result := false;
  except
    on ex:Exception do
    begin
      ShowMessage('TryZoomIn: '+ex.Message);
      result := false;
    end;
  end;
end;

function TFMain.TryZoomOut: boolean;
begin
  try
    if (zoomFactor > 8/image.Width) and
       (zoomFactor > 8/image.Height) then
    begin
      zoomFactor /= 2;
      UpdateToolbar;
      Result := true;
    end else
      Result := false;
  except
    on ex:Exception do
    begin
      ShowMessage('TryZoomOut: '+ex.Message);
      result := false;
    end;
  end;
end;

function TFMain.AbleToLoad(filename: string): boolean;
var ext: string;
begin
  ext := UTF8LowerCase(ExtractFileExt(Filename));
  if (ext='.bmp') or (ext='.jpg') or (ext='.jpeg')
    or (ext='.png') or (ext='.pcx') or
    (ext='.gif') or (ext='.ico') or (ext='.pdn') or
    (ext='.lzp') or (ext='.ora') then
    result := true else
      result := false;
end;

function TFMain.AbleToSaveAs(filename: string): boolean;
var ext: string;
begin
  ext := UTF8LowerCase(ExtractFileExt(Filename));
  if (ext='.bmp') or (ext='.jpg') or (ext='.jpeg')
    or (ext='.png') or (ext='.pcx') or (ext='.xpm') or
    (ext='.lzp') then
    result := true else
      result := false;
end;

procedure TFMain.OnPaintHandler;
var r: TRect;
begin
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

procedure TFMain.UpdateEditPicture(ADelayed: boolean = false);
begin
  if ToolManager.ToolUpdate then
  begin
    if ADelayed then DelayedPaintPicture := True
    else
      PaintPicture;
  end;
end;

procedure TFMain.UpdateTextureIcon;
var
  IconBGRA: TBGRABitmap;
  IconBmp: TBitmap;
begin
  if ToolManager.ToolTexture = nil then
  begin
    NoTextureIcon;
    exit;
  end;
  IconBGRA := ToolManager.ToolTexture.Resample(Image_CurrentTexture.Width,Image_CurrentTexture.Height) as TBGRABitmap;
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
      mrYes: EditDeselectExecute(self);
    end;
  end;
end;

procedure TFMain.ReleaseMouseButtons(Shift: TShiftState);
begin
  if not (ssLeft in Shift) and btnLeftDown then
  begin
    btnLeftDown := false;
    if ToolManager.ToolUp then PaintPicture;
  end;
  if not (ssRight in Shift) and btnRightDown then
  begin
    btnRightDown := false;
    if ToolManager.ToolUp then PaintPicture;
  end;
  if not btnLeftDown and not btnRightDown then CanCompressOrUpdateStack := true;
end;

procedure TFMain.UpdatePanelPhongShape;
begin
  BorderSize_SpinEdit.Enabled := (ToolManager.ToolShapeType = 'Rectangle') or
        (ToolManager.ToolShapeType = 'RoundRectangle');
end;

procedure TFMain.SelectionInstanceOnRun(AInstance: TLazPaintCustomInstance);
begin
  AInstance.Config.SetDefaultImageWidth(Image.Width);
  AInstance.Config.SetDefaultImageHeight(Image.Height);
end;

function TFMain.ShowOpenTextureDialog: boolean;
var newTex: TBGRABitmap;
begin
  result := false;
  try
    if OpenTextureDialog.Execute then
    begin
      try
        newTex := TBGRABitmap.Create(UTF8ToSys( OpenTextureDialog.FileName ));
        if LazPaintInstance.BlackAndWhite then
          BGRAReplace(newTex, newTex.FilterGrayscale);
        ToolManager.ToolTexture.Free;
        ToolManager.ToolTexture := newTex;
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

procedure TFMain.CropToSelectionAndLayer;
var layer,partial: TBGRABitmap; r,rSel: TRect;
begin
  try
    ToolManager.ToolClose;
    if image.SelectionEmpty then
    begin
      ShowMessage(rsEmptySelection);
      exit;
    end;
    if (ToolManager.GetCurrentToolType in[ptRotateSelection,ptMoveSelection]) then
      ChooseTool(ptHand);
    image.ApplySelectionMask;
    image.RetrieveSelectionIfLayerEmpty;
    layer := image.GetOrCreateSelectionLayer;
    r := layer.GetImageBounds;
    if (r.right > r.left) and (r.bottom > r.top) then
    begin
      rSel := image.currentSelection.GetImageBounds(cGreen);
      if (rSel.left <> r.left) or (rSel.Top <> r.top) or
        (rSel.Right <> r.right) or (rSel.bottom <> r.bottom) then
      begin
        case MessageDlg(rsCrop,rsKeepEmptySpace,mtConfirmation,mbYesNo,0) of
        mrYes: begin
                r := rSel
               end;
        end;
      end;
      partial := TBGRABitmap.Create(r.Right-r.left,r.Bottom-r.top);
      partial.PutImage(-r.left,-r.Top,layer,dmSet);
      image.RemoveSelection;
      SetCurrentBitmap(partial,true);
    end else PaintPicture;
    image.SaveImageUndo;
  except
    on ex:Exception do
      ShowMessage('CropToSelectionAndLayer: '+ex.Message);
  end;
end;

procedure TFMain.CropToSelection;
var cropped: TBGRALayeredBitmap;
    r: TRect;
    i,selectedLayer: integer;
    sel: TBGRABitmap;
begin
  if image.NbLayers = 1 then
  begin
    CropToSelectionAndLayer;
    exit;
  end;
  try
    ToolManager.ToolClose;
    if image.SelectionEmpty then
    begin
      ShowMessage(rsEmptySelection);
      exit;
    end;
    r := image.currentSelection.GetImageBounds(cGreen);
    if (r.right > r.left) and (r.bottom > r.top) then
    begin
      cropped := image.currentLayeredBitmap.Duplicate;
      selectedLayer := image.currentImageLayerIndex;
      for i := 0 to cropped.NbLayers-1 do
      begin
        cropped.LayerBitmap[i].ApplyMask(image.currentSelection);
        with cropped.LayerOffset[i] do
          cropped.LayerOffset[i] := point(x-r.left,y-r.top);
      end;
      cropped.SetSize(r.right-r.left,r.Bottom-r.top);
      sel := image.currentSelection.GetPart(r) as TBGRABitmap;
      image.RemoveSelection;
      image.Assign(cropped,true);
      image.currentSelection := sel;
      image.currentImageLayerIndex := selectedLayer;
      PaintPicture;
    end;
  except
    on ex:Exception do
      ShowMessage('CropToSelection: '+ex.Message);
  end;
end;

procedure TFMain.Init;
begin
  initialized := false;
  Config := LazPaintInstance.Config;
  ToolManager := LazPaintInstance.ToolManager;
  Image := LazPaintInstance.Image;
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
  ToolManager.PhongControls.Add(Panel_PhongShape);
  ToolManager.AltitudeControls.Add(Panel_Altitude);

  ToolManager.SetCurrentToolType(ptHand);

  SetCurrentBitmap(TBGRABitmap.Create(Config.DefaultImageWidth,Config.DefaultImageHeight,BGRAPixelTransparent), false);
  image.SaveImageUndo;
  image.SetSavedFlag;

  //toolbar init
  Shape_PenColor.Brush.Color := BGRAToColor(ToolManager.ToolForeColor);
  SpinEdit_PenOpacity.Value := ToolManager.ToolForeColor.alpha;
  Shape_BackColor.Brush.Color := BGRAToColor(ToolManager.ToolBackColor);
  SpinEdit_BackOpacity.Value := ToolManager.ToolBackColor.alpha;
  PenWidth_SpinEdit.Value := Round(ToolManager.ToolPenWidth*PenWidthFactor);
  Shapes_DrawBorder.Down := ToolManager.ToolOptionDrawShape;
  Shapes_FillShape.Down := ToolManager.ToolOptionFillShape;
  Shapes_CloseShape.Down := ToolManager.ToolOptionCloseShape;
  Eraser_SpinEdit.Value := ToolManager.ToolEraserAlpha;
  Tolerance_SpinEdit.Value := ToolManager.ToolTolerance;
  ToolButton_ProgressiveFloodfill.Down := ToolManager.ToolFloodFillOptionProgressive;
  ToolButton_SinGradient.Down := ToolManager.ToolGradientSine;
  ToolButton_GridMoveWithoutDeformation.Down := ToolManager.ToolDeformationGridMoveWithoutDeformation;
  GridNbX_SpinEdit.Value := ToolManager.ToolDeformationGridNbX-1;
  GridNbY_SpinEdit.Value := ToolManager.ToolDeformationGridNbY-1;
  Combo_SplineStyle.ItemIndex:= ord(ToolManager.ToolSplineStyle);
  Tool_TextOutline.Down := ToolManager.ToolTextOutline;
  Tool_TextShadow.Down := ToolManager.ToolTextShadow;
  Tool_TextPhong.Down := ToolManager.ToolTextPhong;
  FontDialog1.Font := ToolManager.ToolTextFont;
  TextBlur_SpinEdit.Value := ToolManager.ToolTextBlur;
  TextShadowX_SpinEdit.Value := ToolManager.ToolTextShadowOffset.X;
  TextShadowY_SpinEdit.Value := ToolManager.ToolTextShadowOffset.Y;
  if ToolManager.ToolShapeType = 'RoundRectangle' then
    ToolButton_ShapeRoundRect.Down := true;
  if ToolManager.ToolShapeType = 'Sphere' then
    ToolButton_ShapeSphere.Down := true;
  if ToolManager.ToolShapeType = 'Cone' then
    ToolButton_ShapeCone.Down := true;
  if ToolManager.ToolShapeType = 'VerticalCone' then
    ToolButton_ShapeVerticalCone.Down := true;
  if ToolManager.ToolShapeType = 'HorizontalCylinder' then
    ToolButton_ShapeHorizontalCylinder.Down := true;
  if ToolManager.ToolShapeType = 'VerticalCylinder' then
    ToolButton_ShapeVerticalCylinder.Down := true;
  UpdatePanelPhongShape;
  ShapeAltitude_SpinEdit.Value := ToolManager.ToolShapeAltitude;
  BorderSize_SpinEdit.Value := ToolManager.ToolShapeBorderSize;
  UpdateLineCapBar;

  ViewGrid.Checked := LazPaintInstance.GridVisible;
  MenuItem_ViewGrid.Checked:= LazPaintInstance.GridVisible;
  MenuItem87.Visible := not LazPaintInstance.BlackAndWhite;
  MenuItem88.Visible := not LazPaintInstance.BlackAndWhite;
  MenuItem89.Visible := not LazPaintInstance.BlackAndWhite;
  MenuItem82.Visible := not LazPaintInstance.BlackAndWhite;
  MenuItem84.Visible := not LazPaintInstance.BlackAndWhite;
  MenuItem123.Visible := not LazPaintInstance.BlackAndWhite;
  MenuItem124.Visible := not LazPaintInstance.BlackAndWhite;

  initialized := true;
end;

function TFMain.GetUpdateCheckerFilename: string;
begin
  {$IFDEF WINDOWS}
  result := CreateAbsolutePath('update_checker.exe',StartDirectory);
  {$ELSE}
  result := CreateAbsolutePath('update_checker',StartDirectory);
  {$ENDIF}
end;

procedure TFMain.PaintPicture;
begin
  if not visible then exit;
  DelayedPaintPicture := False;
  StackNeedUpdate := true;
  {$IFDEF USEPAINTBOXPICTURE}
  PaintBox_Picture.Repaint;
  {$ELSE}
  PaintPictureImplementation;
  {$ENDIF}
end;

function TFMain.TryOpenFile(filename: string): boolean;
var
  newPicture: TBGRABitmap;
  multi: ArrayOfBGRABitmap;
  finalFilename,ext: string;

  procedure StartImport;
  begin
    ToolManager.ToolClose;
    if ToolManager.GetCurrentToolType in [ptDeformation,ptRotateSelection,ptMoveSelection,ptTextureMapping] then
      ChooseTool(ptHand);
    Config.AddRecentFile(filename);
    SetCurrentFilename(finalFilename);
    image.ReleaseSelection;
  end;
  procedure EndImport;
  begin
    image.SaveImageUndo;
    image.SetSavedFlag;
    result := true;
  end;

begin
  result := false;
  if not AbleToLoad(filename) then
  begin
    ShowMessage(rsFileExtensionNotSupported);
    exit;
  end;
  finalFilename := filename;
  try
    ext := UTF8LowerCase(ExtractFileExt(Filename));
    if ext='.ico' then
    begin
      multi := LoadIco(filename);
      newPicture := LazPaintInstance.ChooseImage(multi);
      FreeMultiImage(multi);
      if newPicture <> nil then
        finalFilename += '.'+newPicture.Caption+'.png';
    end else
    if ext='.gif' then
    begin
      multi := LoadGif(filename);
      newPicture := LazPaintInstance.ChooseImage(multi);
      FreeMultiImage(multi);
      if newPicture <> nil then
        finalFilename += '.'+newPicture.Caption+'.png';
    end else
    begin
      StartImport;
      image.LoadFromFile(UTF8ToSys(filename));
      EndImport;
      newPicture := nil;
    end;
    if (newPicture <> nil) and (newPicture.Width > 0) and (newPicture.Height > 0) then
    begin
      StartImport;
      SetCurrentBitmap(newPicture);
      EndImport;
    end else newPicture.Free;
  except
    on ex: Exception do
      MessageDlg(ex.Message,mtError,[mbOk],0);
  end;
  if self.Visible then PaintPicture;
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

procedure TFMain.FormShow(Sender: TObject);
var r: TRect;
begin
  //strange position fix
  SpinEdit_PenOpacity.Top := PenWidth_SpinEdit.Top;
  SpinEdit_BackOpacity.Top := PenWidth_SpinEdit.Top;

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
  Image.SaveImageUndo;

  ArrangeToolbars;
  UpdateToolBar;
end;

procedure TFMain.ImageHorizontalFlipExecute(Sender: TObject);
begin
  try
    LazPaintInstance.DoHorizontalFlip(foAuto);
    image.SaveLayerOrSelectionUndo;
  except
    on ex:Exception do
      ShowMessage('ImageHorizontalFlipExecute: '+ex.Message);
  end;
end;

procedure TFMain.ImageResampleExecute(Sender: TObject);
begin
  LazPaintInstance.HideTopmost;
  if LazPaintInstance.ShowResampleDialog then PaintPicture;
  LazPaintInstance.ShowTopmost;
end;

procedure TFMain.ImageVerticalFlipExecute(Sender: TObject);
begin
  try
    LazPaintInstance.DoVerticalFlip(foAuto);
    image.SaveLayerOrSelectionUndo;
  except
    on ex:Exception do
      ShowMessage('ImageVerticalFlipExecute: '+ex.Message);
  end;
end;

procedure TFMain.Image_SwapMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var temp: TBGRAPixel;
begin
  temp := ToolManager.ToolForeColor;
  ToolManager.ToolForeColor := ToolManager.ToolBackColor;
  ToolManager.ToolBackColor := temp;
  UpdateToolbar;
  UpdateEditPicture;
end;

procedure TFMain.PenWidth_SpinEditChange(Sender: TObject);
begin
  if initialized then
  begin
    ToolManager.ToolPenWidth := PenWidth_SpinEdit.Value/PenWidthFactor;
    ShowPenPreview(True);
  end;
end;

procedure TFMain.Shapes_CloseShapeClick(Sender: TObject);
begin
  if initialized then
  begin
     if ToolManager.ToolOptionCloseShape <> Shapes_CloseShape.Down then
     begin
       ToolManager.ToolOptionCloseShape:= Shapes_CloseShape.Down;
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
    with ToolManager.ToolForeColor do
      ToolManager.ToolForeColor := BGRA(red,green,blue,SpinEdit_PenOpacity.value);
    LazPaintInstance.ColorToFChooseColor;
    UpdateEditPicture;
  end;
end;

procedure TFMain.Tolerance_SpinEditChange(Sender: TObject);
begin
  if initialized then
    ToolManager.ToolTolerance := Tolerance_SpinEdit.Value;
end;

procedure TFMain.ToolButton_DiamondGradientClick(Sender: TObject);
begin
     if ToolButton_DiamondGradient.Down then
     begin
       ToolManager.ToolGradientType := gtDiamond;
       UpdateEditPicture;
     end;
end;

procedure TFMain.ToolButton_LinearGradientClick(Sender: TObject);
begin
     if ToolButton_LinearGradient.Down then
     begin
       ToolManager.ToolGradientType := gtLinear;
       UpdateEditPicture;
     end;
end;

procedure TFMain.ToolButton_ProgressiveFloodfillClick(Sender: TObject);
begin
     if initialized then
       ToolManager.ToolFloodFillOptionProgressive := ToolButton_ProgressiveFloodfill.Down;
end;

procedure TFMain.ToolButton_RadialGradientClick(Sender: TObject);
begin
     if ToolButton_RadialGradient.Down then
     begin
       ToolManager.ToolGradientType := gtRadial;
       UpdateEditPicture;
     end;
end;

procedure TFMain.ToolButton_ReflectedGradientClick(Sender: TObject);
begin
     if ToolButton_ReflectedGradient.Down then
     begin
       ToolManager.ToolGradientType := gtReflected;
       UpdateEditPicture;
     end;
end;

procedure TFMain.ToolColorPickerExecute(Sender: TObject);
begin
  ChooseTool(ptColorPicker);
end;

procedure TFMain.ToolDrawShapeExecute(Sender: TObject);
begin
  if initialized then
  begin
     if not Shapes_DrawBorder.Down and not Shapes_FillShape.Down then
        Shapes_DrawBorder.Down := true;
     ToolManager.ToolOptionDrawShape:= Shapes_DrawBorder.Down;
     UpdateEditPicture;
  end;
end;

procedure TFMain.ToolEllipseExecute(Sender: TObject);
begin
  ChooseTool(ptEllipse);
end;

procedure TFMain.ToolEraserExecute(Sender: TObject);
begin
  ChooseTool(ptEraser);
end;

procedure TFMain.ToolFillShapeExecute(Sender: TObject);
begin
  if initialized then
  begin
     if not Shapes_DrawBorder.Down and not Shapes_FillShape.Down then
        Shapes_FillShape.Down := true;
     ToolManager.ToolOptionFillShape:= Shapes_FillShape.Down;
     UpdateEditPicture;
  end;
end;

procedure TFMain.ToolFloodfillExecute(Sender: TObject);
begin
  ChooseTool(ptFloodfill);
end;

procedure TFMain.ToolGradientExecute(Sender: TObject);
begin
  ChooseTool(ptGradient);
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

  visualWidth := round(image.Width*zoomFactor);
  if visualWidth = 0 then visualWidth := 1;
  visualHeight := round(image.Height*zoomFactor);
  if visualHeight = 0 then visualHeight := 1;
  visualLeft := (formArea.Left+formArea.Right-visualWidth) div 2;
  visualTop := (formArea.Top+formArea.Bottom-visualHeight) div 2;

  maxOffset := point(floor((formArea.Right-(visualLeft+visualWidth))/zoomFactor)-1,
       floor((formArea.Bottom-(visualTop+visualHeight))/zoomFactor)-1);
  minOffset := point(ceil((formArea.Left-visualLeft)/zoomFactor)+1,
               ceil((formArea.Top-visualTop)/zoomFactor)+1);
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

  selectionHighlight := RetrieveSelectionHighlight(formArea,image.ImageOffset,image.SelectionRotateAngle,
       image.SelectionRotateCenter,
       zoomFactor,ToolManager.IsSelectingTool, highlightOffset);

  //if selection highlight has not been computed yet, then compute shown selection
  if selectionHighlight = nil then
  begin
    if image.currentSelection = nil then
      shownSelection := nil else
    begin
      if image.SelectionRotateAngle <> 0 then
      begin
        if (image.currentSelection <> nil) and image.SelectionLayerIsEmpty then
          shownSelection := image.ComputeRotatedSelection else
            shownSelection := nil;
      end else
        shownSelection := image.currentSelection;
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

    if zoomFactor <> 1 then
    begin
      partialImage := TBGRABitmap.Create(PictureActualSize.X,PictureActualSize.Y);
      image.Draw(partialImage, -PictureOffset.x, -PictureOffset.Y);

      if shownSelection <> nil then
      begin
        partialSelection := TBGRABitmap.Create(PictureActualSize.X,PictureActualSize.Y);
        partialSelection.PutImage(-PictureOffset.X+image.SelectionOffset.X,
                                  -PictureOffset.Y+image.SelectionOffset.Y, shownSelection, dmSet);
      end else
       partialSelection := nil;
    end else
    begin
      partialSelection := nil;
      partialImage := nil;
    end;

    visualLeft := visualLeft + round(TopLeftCorner.x/image.width*visualWidth);
    visualTop := visualTop + round(TopLeftCorner.y/image.height*visualHeight);
    visualWidth := round(PictureActualSize.X*zoomFactor);
    if visualWidth = 0 then visualWidth := 1;
    visualHeight := round(PictureActualSize.Y*zoomFactor);
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
        if (zoomFactor > 4) and LazPaintInstance.GridVisible then DrawGrid(resampled,visualWidth/partialImage.width,visualHeight/partialImage.Height);
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

    if (zoomFactor > 4) and LazPaintInstance.GridVisible then DrawGrid(resampled,visualWidth/image.width,visualHeight/image.Height);
    if (shownSelection <> nil) and (selectionHighlight = nil) and (zoomFactor <> 1) then
    begin
       if (image.SelectionOffset.X <> 0) or (image.SelectionOffset.Y <> 0) then
       begin
         ofsSelection := TBGRABitmap.Create(image.Width,
                      image.Height, BGRABlack);
         ofsSelection.PutImage(image.SelectionOffset.X,
                  image.SelectionOffset.Y, shownSelection, dmSet);
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
  FreeAndNil(resampled);

  //compute selection highlight if needed
  if selectionHighlight = nil then
  begin
     if resampledSelection <> nil then
     begin
       highlightOffset := point(0,0);
       selectionHighlight := resampledSelection.FilterEmbossHighlight(
              ToolManager.IsSelectingTool and not ShowSelectionNormal, BGRABlack, highlightOffset) as TBGRABitmap;
       StoreSelectionHighlight(formArea,image.ImageOffset,image.SelectionRotateAngle,image.SelectionRotateCenter, zoomFactor,
         ToolManager.IsSelectingTool,selectionHighlight,highlightOffset);
     end else if shownSelection <> nil then
     begin
       highlightOffset := point(0,0);
       selectionHighlight := shownSelection.FilterEmbossHighlight(
              ToolManager.IsSelectingTool and not ShowSelectionNormal, BGRABlack, highlightOffset) as TBGRABitmap;
       StoreSelectionHighlight(formArea,image.ImageOffset,image.SelectionRotateAngle,image.SelectionRotateCenter, zoomFactor,
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
      virtualScreen.PutImage(highlightOffset.X-PictureOffset.x+image.SelectionOffset.X,
        highlightOffset.Y-PictureOffset.Y+image.SelectionOffset.Y,selectionHighlight,dmFastBlend);
  end;


  //define picture view
  pictureOrigin := Point(visualLeft,visualTop);
  pictureViewSize := Point(visualWidth,visualHeight);

  //show tools info
  ToolManager.RenderTool(virtualScreen);
  PaintVirtualScreenImplementation;
  if shownSelection <> image.currentSelection then shownSelection.Free;
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

procedure TFMain.PaintVirtualScreenImplementation;
var cursorBack: TBGRABitmap;
    rx,ry: single;
    c,tl,br: TPointF;
    x0,y0,tx,ty: integer;
    orig: TPointF;
    DestCanvas: TCanvas;
    DrawOfs: TPoint;
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
    x0 := floor(tl.x);
    y0 := floor(tl.y);
    tx := ceil(br.x)-x0+1;
    ty := ceil(br.y)-y0+1;
    cursorBack := TBGRABitmap.Create(tx,ty);
    cursorBack.PutImage(-x0,-y0,virtualScreen,dmSet);

    virtualscreen.EllipseAntialias(c.x,c.y,rx,ry,BGRAWhite,1);
    virtualscreen.PenStyle := psDash;
    virtualscreen.DrawPolygonAntialias(virtualscreen.ComputeEllipseContour(c.x,c.y,rx,ry),BGRABlack,1);
    virtualscreen.Draw(DestCanvas, pictureOrigin.X+DrawOfs.X, pictureOrigin.Y+DrawOfs.Y, True);

    virtualScreen.PutImage(x0,y0,cursorBack,dmSet);
    cursorBack.Free;
  end else
    virtualscreen.Draw(DestCanvas, pictureOrigin.X+DrawOfs.X, pictureOrigin.Y+DrawOfs.Y, True);
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

  if zoomFactor < 3 then
    Label_CurrentZoom.Caption := inttostr(round(zoomFactor*100))+'%' else
     Label_CurrentZoom.Caption := 'x'+FloatToStr(zoomFactor);

  maxGridNbX := Max(2,Min(image.Width div 2,15));
  maxGridNbY := Max(2,Min(image.Height div 2,15));
  if GridNbX_SpinEdit.MaxValue <> maxGridNbX then
    GridNbX_SpinEdit.MaxValue := maxGridNbX;
  if GridNbY_SpinEdit.MaxValue <> maxGridNbY then
    GridNbY_SpinEdit.MaxValue := maxGridNbY;

  LazPaintInstance.ColorToFChooseColor;
  UpdateToolImage;
end;

function TFMain.GetPictureArea: TRect;
begin
  result := Rect(0,ToolbarsHeight,ClientWidth,ClientHeight);
end;

procedure TFMain.PictureSelectionChanged(sender: TLazPaintImage; AOffsetOnly: boolean);
begin
  if not AOffsetOnly or (ZoomFactor <> 1) then ForgetSelectionHightlight;
end;

procedure TFMain.ToolHandExecute(Sender: TObject);
begin
  ChooseTool(ptHand);
end;

procedure TFMain.ToolMoveSelectionExecute(Sender: TObject);
begin
  ChooseTool(ptMoveSelection);
end;

procedure TFMain.ToolMoveSelectionUpdate(Sender: TObject);
begin
  //see EditDeselect
end;

procedure TFMain.ToolPenExecute(Sender: TObject);
begin
  ChooseTool(ptPen);
end;

procedure TFMain.ToolPolygonExecute(Sender: TObject);
begin
  ChooseTool(ptPolygon);
end;

procedure TFMain.ToolRectExecute(Sender: TObject);
begin
  ChooseTool(ptRect);
end;

procedure TFMain.ToolRotateSelectionExecute(Sender: TObject);
begin
  ChooseTool(ptRotateSelection);
end;

procedure TFMain.ToolSelectCurveExecute(Sender: TObject);
begin
  ChooseTool(ptSelectSpline);
end;

procedure TFMain.ToolSelectEllipseExecute(Sender: TObject);
begin
  ChooseTool(ptSelectEllipse);
end;

procedure TFMain.ToolSelectionPenExecute(Sender: TObject);
begin
  ChooseTool(ptSelectPen);
end;

procedure TFMain.ToolSelectPolyExecute(Sender: TObject);
begin
  ChooseTool(ptSelectPoly);
end;

procedure TFMain.ToolSelectRectExecute(Sender: TObject);
begin
  ChooseTool(ptSelectRect);
end;

procedure TFMain.ToolSplineExecute(Sender: TObject);
begin
  ChooseTool(ptSpline);
end;

procedure TFMain.ViewToolboxExecute(Sender: TObject);
begin
  ToggleToolboxVisible;
end;

{$hints off}
procedure TFMain.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
   //  block Erasing background
   //  inherited EraseBackground(DC);
end;
{$hints on}

procedure TFMain.SetShowSelectionNormal(const AValue: boolean);
begin
  if FShowSelectionNormal=AValue then exit;
  FShowSelectionNormal:=AValue;
  ForgetSelectionHightlight;
end;

procedure TFMain.SetLazPaintInstance(const AValue: TLazPaintCustomInstance);
begin
  FLazPaintInstance := AValue;
  Init;
end;

procedure TFMain.SetCurrentBitmap(bmp: TBGRABitmap; DoRepaint: boolean = true);
begin
  image.RemoveSelection;
  image.Assign(bmp,True);
  if DoRepaint then PaintPicture;
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

initialization
  {$I lazpaintmainform.lrs}

end.
