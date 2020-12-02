// SPDX-License-Identifier: GPL-3.0-only
unit LazpaintMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, LMessages, SysUtils, LazFileUtils, LResources, Forms, Controls,
  Graphics, Dialogs, Menus, ExtDlgs, ComCtrls, ActnList, StdCtrls, ExtCtrls,
  Buttons, types, LCLType, BGRAImageList, BCTrackbarUpdown, BCComboBox, BCButton,

  BGRABitmap, BGRABitmapTypes, BGRALayers, BGRASVGOriginal, BGRAGradientScanner, BGRAGradientOriginal,

  LazPaintType, UMainFormLayout, UTool, UImage, UImageAction, UZoom,
  UImageObservation, UConfig, LCScaleDPI, UResourceStrings, UMenu, uscripting,
  ubrowseimages, UToolPolygon, UToolVectorial, LCVectorRectShapes,
  LCVectorialFillControl, LCVectorialFill,

  udarktheme, UScriptType;

type
  { TFMain }

  TFMain = class(TForm)
    LayerExport: TAction;
    FileExport: TAction;
    ExportPictureDialog: TSaveDialog;
    Label_Donate: TLabel;
    MenuScript: TMenuItem;
    Panel_OutlineFill: TPanel;
    Panel_Donate: TPanel;
    ToolButton_Donate: TToolButton;
    ToolBar25: TToolBar;
    ToolOpenedCurve: TAction;
    ToolPolyline: TAction;
    FileRunScript: TAction;
    EditShapeToCurve: TAction;
    EditShapeAlignBottom: TAction;
    EditShapeCenterVertically: TAction;
    EditShapeAlignTop: TAction;
    EditShapeAlignRight: TAction;
    EditShapeCenterHorizontally: TAction;
    EditShapeAlignLeft: TAction;
    ComboBox_PenStyle: TBCComboBox;
    EditMoveToBack: TAction;
    EditMoveDown: TAction;
    EditMoveToFront: TAction;
    EditMoveUp: TAction;
    Image_SwapColors: TImage;
    Label_Pen: TLabel;
    Label_Back: TLabel;
    Label_ShadowOffset: TLabel;
    Label_TextBlur: TLabel;
    TimerHideFill: TTimer;
    TimerArrange: TTimer;
    Perspective_Repeat: TToolButton;
    Tool_EraseSharpen: TToolButton;
    Tool_EraseLighten: TToolButton;
    Tool_EraseDarken: TToolButton;
    VectorialFill_Outline: TLCVectorialFillControl;
    VectorialFill_Pen: TLCVectorialFillControl;
    VectorialFill_Back: TLCVectorialFillControl;
    Panel_BackFill: TPanel;
    Panel_SwapColor: TPanel;
    Panel_PenFill: TPanel;
    Panel_TextShadow: TPanel;
    Panel_CloseShape: TPanel;
    SpinEdit_TextBlur: TBCTrackbarUpdown;
    SpinEdit_TextShadowX: TBCTrackbarUpdown;
    SpinEdit_TextShadowY: TBCTrackbarUpdown;
    ToolBar23: TToolBar;
    ToolBar24: TToolBar;
    ToolEditShape: TAction;
    ComboBox_ArrowStart: TBCComboBox;
    ComboBox_ArrowEnd: TBCComboBox;
    ComboBox_BrushSelect: TBCComboBox;
    Combo_SplineStyle: TBCComboBox;
    FilterWaveDisplacement: TAction;
    SpinEdit_Eraser: TBCTrackbarUpdown;
    SpinEdit_GridNbX: TBCTrackbarUpdown;
    SpinEdit_GridNbY: TBCTrackbarUpdown;
    SpinEdit_PenWidth: TBCTrackbarUpdown;
    SpinEdit_ArrowSizeX: TBCTrackbarUpdown;
    SpinEdit_ArrowSizeY: TBCTrackbarUpdown;
    SpinEdit_Tolerance: TBCTrackbarUpdown;
    SpinEdit_BrushSpacing: TBCTrackbarUpdown;
    SpinEdit_ShapeAltitude: TBCTrackbarUpdown;
    SpinEdit_TextOutlineWidth: TBCTrackbarUpdown;
    SpinEdit_PhongBorderSize: TBCTrackbarUpdown;
    SpinEdit_TextSize: TBCTrackbarUpdown;
    Tool_CloseShape: TToolButton;
    Tool_TextShadow: TToolButton;
    ViewDarkTheme: TAction;
    MenuFileToolbar: TMenuItem;
    ViewWorkspaceColor: TAction;
    LayerRasterize: TAction;
    FileRememberSaveFormat: TAction;
    SelectionVerticalFlip: TAction;
    SelectionHorizontalFlip: TAction;
    LayerZoom: TAction;
    ImageSwapRedBlue: TAction;
    ImageLinearNegative: TAction;
    ImageNegative: TAction;
    ForgetDialogAnswers: TAction;
    FileChooseEntry: TAction;
    Panel_Aliasing: TPanel;
    ToolBar22: TToolBar;
    ToolButton8: TToolButton;
    ToolHotSpot: TAction;
    Combo_Ratio: TComboBox;
    FileUseImageBrowser: TAction;
    ItemUseImageBrowser: TMenuItem;
    Label_Ratio: TLabel;
    Panel_Ratio: TPanel;
    Tool_Aliasing: TToolButton;
    ViewPalette: TAction;
    ViewStatusBar: TAction;
    ImageList48: TBGRAImageList;
    MenuShowPalette: TMenuItem;
    ToolClone: TAction;
    FilterRain: TAction;
    BrushRemoveCurrent: TAction;
    BrushLoadFromFile: TAction;
    BrushCreateGeometric: TAction;
    Label_Brush: TLabel;
    Label_Spacing: TLabel;
    MenuDockToolboxLeft: TMenuItem;
    MenuDockToolboxRight: TMenuItem;
    MenuUndockToolbox: TMenuItem;
    MenuZoomToolbar: TMenuItem;
    MenuCopyPasteToolbar: TMenuItem;
    MenuCoordinatesToolbar: TMenuItem;
    MenuUndoRedoToolbar: TMenuItem;
    OpenBrushDialog: TOpenDialog;
    Panel_Brush: TPanel;
    PopupToolbox: TPopupMenu;
    PopupToolbar: TPopupMenu;
    ToolBar21: TToolBar;
    ToolBrush: TAction;
    ToolButton21: TToolButton;
    ToolButton22: TToolButton;
    ToolButton4: TToolButton;
    ToolButton7: TToolButton;
    ToolChangeDocking: TAction;
    FilterBlurBox: TAction;
    EditPasteAsNewLayer: TAction;
    ItemDockLayersAndColors: TMenuItem;
    ItemFullscreen: TMenuItem;
    ItemViewDockToolbox: TMenuItem;
    SaveSelectionDialog: TSaveDialog;
    SavePictureDialog1: TSaveDialog;
    TimerLoadToolWin: TTimer;
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
    ViewImageList: TAction;
    MenuRecentFiles: TMenuItem;
    ItemDonate: TMenuItem;
    MenuLanguage: TMenuItem;
    ItemQuitSeparator: TMenuItem;
    ItemQuit: TMenuItem;
    MenuEdit: TMenuItem;
    MenuSelect: TMenuItem;
    MenuView: TMenuItem;
    MenuImage: TMenuItem;
    MenuRemoveTransparency: TMenuItem;
    MenuColors: TMenuItem;
    MenuTool: TMenuItem;
    MenuFilter: TMenuItem;
    MenuRadialBlur: TMenuItem;
    MenuRender: TMenuItem;
    MenuHelp: TMenuItem;
    Open3DObjectDialog: TOpenDialog;
    Panel_PerspectiveOption: TPanel;
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
    ImageRotate180: TAction;
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
    TimerUpdate: TTimer;
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
    Label_Text: TLabel;
    Panel_Text: TPanel;
    ToolBar15: TToolBar;
    Tool_TextFont: TToolButton;
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
    ToolBar14: TToolBar;
    ToolButton19: TToolButton;
    ToolButton20: TToolButton;
    ToolButton3: TToolButton;
    Tool_GridMoveWithoutDeformation: TToolButton;
    Panel_PenStyle: TPanel;
    Panel_LineCap: TPanel;
    Panel_JoinStyle: TPanel;
    ToolBar11: TToolBar;
    Tool_JoinBevel: TToolButton;
    Tool_JoinRound: TToolButton;
    Tool_CapSquare: TToolButton;
    Tool_CapFlat: TToolButton;
    Tool_CapRound: TToolButton;
    ToolBar10: TToolBar;
    Tool_JoinMiter: TToolButton;
    Label_Coordinates: TLabel;
    Panel_Coordinates: TPanel;
    Label_Eraser: TLabel;
    Image_CurrentTool: TImage;
    Label_CurrentZoom: TLabel;
    Panel_Undo: TPanel;
    Panel_CopyPaste: TPanel;
    Panel_ToolbarBackground: TPanel;
    Panel_File: TPanel;
    Panel_Tool: TPanel;
    Panel_Zoom: TPanel;
    Panel_ColorDiff: TPanel;
    Panel_PenWidth: TPanel;
    Panel_Eraser: TPanel;
    Panel_Tolerance: TPanel;
    Panel_ShapeOption: TPanel;
    Label_PenWidth: TLabel;
    Tool_DrawShapeBorder: TToolButton;
    Tool_FillShape: TToolButton;
    Label_Tolerance: TLabel;
    ToolBar2: TToolBar;
    ToolBar3: TToolBar;
    ToolBar5: TToolBar;
    ToolBar6: TToolBar;
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
    Tool_ProgressiveFloodfill: TToolButton;
    ToolButton_ZoomOriginal: TToolButton;
    ColorDialog1: TColorDialog;
    ActionList1: TActionList;
    ImageList16: TBGRAImageList;
    procedure BrushCreateGeometricExecute(Sender: TObject);
    procedure BrushCreateGeometricUpdate(Sender: TObject);
    procedure BrushLoadFromFileExecute(Sender: TObject);
    procedure BrushLoadFromFileUpdate(Sender: TObject);
    procedure BrushRemoveCurrentExecute(Sender: TObject);
    procedure BrushRemoveCurrentUpdate(Sender: TObject);
    procedure ComboBox_ArrowEndChange(Sender: TObject);
    procedure ComboBox_ArrowEndDrawItem({%H-}Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure ComboBox_ArrowEndDrawSelectedItem(Sender: TObject; const ABGRA: TBGRABitmap;
        AState: TBCButtonState; ARect: TRect);
    procedure ComboBox_ArrowStartChange(Sender: TObject);
    procedure ComboBox_ArrowStartDrawItem({%H-}Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure ComboBox_ArrowStartDrawSelectedItem(Sender: TObject; const ABGRA: TBGRABitmap;
        AState: TBCButtonState; ARect: TRect);
    procedure ComboBox_BrushSelectChange(Sender: TObject);
    procedure ComboBox_BrushSelectDrawItem({%H-}Control: TWinControl;
      Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure ComboBox_BrushSelectDrawSelectedItem(Sender: TObject; const ABGRA: TBGRABitmap;
        AState: TBCButtonState; ARect: TRect);
    procedure Combo_RatioChange(Sender: TObject);
    procedure EditCopyExecute(Sender: TObject);
    procedure EditCopyUpdate(Sender: TObject);
    procedure EditCutExecute(Sender: TObject);
    procedure EditCutUpdate(Sender: TObject);
    procedure EditDeleteSelectionExecute(Sender: TObject);
    procedure EditDeleteSelectionUpdate(Sender: TObject);
    procedure EditMoveDownExecute(Sender: TObject);
    procedure EditMoveDownUpdate(Sender: TObject);
    procedure EditMoveToBackExecute(Sender: TObject);
    procedure EditMoveToBackUpdate(Sender: TObject);
    procedure EditMoveToFrontExecute(Sender: TObject);
    procedure EditMoveToFrontUpdate(Sender: TObject);
    procedure EditMoveUpExecute(Sender: TObject);
    procedure EditMoveUpUpdate(Sender: TObject);
    procedure EditPasteExecute(Sender: TObject);
    procedure EditSelectionFitUpdate(Sender: TObject);
    procedure EditSelectionUpdate(Sender: TObject);
    procedure EditShapeAlignBottomExecute(Sender: TObject);
    procedure EditShapeAlignBottomUpdate(Sender: TObject);
    procedure EditShapeAlignLeftExecute(Sender: TObject);
    procedure EditShapeAlignLeftUpdate(Sender: TObject);
    procedure EditShapeAlignRightExecute(Sender: TObject);
    procedure EditShapeAlignRightUpdate(Sender: TObject);
    procedure EditShapeAlignTopExecute(Sender: TObject);
    procedure EditShapeAlignTopUpdate(Sender: TObject);
    procedure EditShapeCenterHorizontallyExecute(Sender: TObject);
    procedure EditShapeCenterHorizontallyUpdate(Sender: TObject);
    procedure EditShapeCenterVerticallyExecute(Sender: TObject);
    procedure EditShapeCenterVerticallyUpdate(Sender: TObject);
    procedure EditShapeToCurveExecute(Sender: TObject);
    procedure EditShapeToCurveUpdate(Sender: TObject);
    procedure FileChooseEntryExecute(Sender: TObject);
    procedure FileChooseEntryUpdate(Sender: TObject);
    procedure FileExportExecute(Sender: TObject);
    procedure FileImport3DUpdate(Sender: TObject);
    procedure FilePrintExecute(Sender: TObject);
    procedure FileRememberSaveFormatExecute(Sender: TObject);
    procedure FileRunScriptExecute(Sender: TObject);
    procedure FileSaveAsInSameFolderExecute(Sender: TObject);
    procedure FileSaveAsInSameFolderUpdate(Sender: TObject);
    procedure FileUseImageBrowserExecute(Sender: TObject);
    procedure FileUseImageBrowserUpdate(Sender: TObject);
    procedure ForgetDialogAnswersExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormWindowStateChange(Sender: TObject);
    procedure ItemDockLayersAndColorsClick(Sender: TObject);
    procedure ItemFullscreenClick(Sender: TObject);
    procedure ItemViewDockToolboxClick(Sender: TObject);
    procedure LayerExportExecute(Sender: TObject);
    procedure LayerRasterizeUpdate(Sender: TObject);
    procedure LayerZoomExecute(Sender: TObject);
    procedure LayerZoomUpdate(Sender: TObject);
    procedure MenuCoordinatesToolbarClick(Sender: TObject);
    procedure MenuCopyPasteToolbarClick(Sender: TObject);
    procedure MenuDockToolboxLeftClick(Sender: TObject);
    procedure MenuDockToolboxRightClick(Sender: TObject);
    procedure MenuFileToolbarClick(Sender: TObject);
    procedure MenuShowPaletteClick(Sender: TObject);
    procedure MenuUndockToolboxClick(Sender: TObject);
    procedure MenuUndoRedoToolbarClick(Sender: TObject);
    procedure MenuViewClick(Sender: TObject);
    procedure MenuZoomToolbarClick(Sender: TObject);
    procedure PaintBox_PenPreviewMouseDown(Sender: TObject;
      {%H-}Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure PopupToolbarPopup(Sender: TObject);
    procedure PopupToolboxPopup(Sender: TObject);
    procedure SelectionHorizontalFlipUpdate(Sender: TObject);
    procedure SelectionVerticalFlipUpdate(Sender: TObject);
    procedure SpinEdit_PhongBorderSizeChange(Sender: TObject; AByUser: boolean);
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
    procedure Perspective_TwoPlanesClick(Sender: TObject);
    procedure SpinEdit_ShapeAltitudeChange(Sender: TObject; AByUser: boolean);
    procedure SpinEdit_BrushSpacingChange(Sender: TObject; AByUser: boolean);
    procedure SpinEdit_TextSizeChange(Sender: TObject; AByUser: boolean);
    procedure SpinEdit_TextBlurChange(Sender: TObject; AByUser: boolean);
    procedure GridNb_SpinEditChange(Sender: TObject; AByUser: boolean);
    procedure TimerArrangeTimer(Sender: TObject);
    procedure TimerHideFillTimer(Sender: TObject);
    procedure ToolButton_DonateClick(Sender: TObject);
    procedure VectorialFill_TextureClick(Sender: TObject);
    procedure PaintBox_PenPreviewPaint(Sender: TObject);
    procedure Panel_PenWidthMouseMove(Sender: TObject; {%H-}Shift: TShiftState; {%H-}X,
      {%H-}Y: Integer);
    procedure Panel_ToolbarBackgroundMouseMove(Sender: TObject;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure SpinEdit_PenWidthMouseMove(Sender: TObject; {%H-}Shift: TShiftState;
      {%H-}X, {%H-}Y: Integer);
    procedure RenderAnyExecute(Sender: TObject);
    procedure FilterAnyExecute(Sender: TObject);
    procedure TimerLoadToolWinTimer(Sender: TObject);
    procedure ToolAnyExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormHide(Sender: TObject);
    procedure HelpAboutExecute(Sender: TObject);
    procedure HelpIndexExecute(Sender: TObject);
    procedure ImageChangeCanvasSizeExecute(Sender: TObject);
    procedure ImageCropUpdate(Sender: TObject);
    procedure ImageRepeatExecute(Sender: TObject);
    procedure MenuFileClick(Sender: TObject);
    procedure RecentFileClick(Sender: TObject);
    procedure LanguageClick(Sender: TObject);
    procedure SpinEdit_TextOutlineWidthChange(Sender: TObject; AByUser: boolean);
    procedure SpinEdit_TextShadowXChange(Sender: TObject; AByUser: boolean);
    procedure SpinEdit_TextShadowYChange(Sender: TObject; AByUser: boolean);
    procedure TimerUpdateTimer(Sender: TObject);
    procedure TimerHidePenPreviewTimer(Sender: TObject);
    procedure ToolChangeDockingExecute(Sender: TObject);
    procedure ToolHotSpotUpdate(Sender: TObject);
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
    procedure Tool_CapFlatClick(Sender: TObject);
    procedure Tool_CapRoundClick(Sender: TObject);
    procedure Tool_CapSquareClick(Sender: TObject);
    procedure Tool_JoinBevelClick(Sender: TObject);
    procedure Tool_JoinRoundClick(Sender: TObject);
    procedure Tool_JoinMiterClick(Sender: TObject);
    procedure Tool_TextFontClick(Sender: TObject);
    procedure Tool_TextOutlineClick(Sender: TObject);
    procedure Tool_TextShadowClick(Sender: TObject);
    procedure ViewColorsExecute(Sender: TObject);
    procedure ViewColorsUpdate(Sender: TObject);
    procedure ViewDarkThemeExecute(Sender: TObject);
    procedure ViewDarkThemeUpdate(Sender: TObject);
    procedure ViewGridUpdate(Sender: TObject);
    procedure ViewImageListExecute(Sender: TObject);
    procedure ViewLayerStackButtonUpdate(Sender: TObject);
    procedure ViewLayerStackExecute(Sender: TObject);
    procedure ViewLayerStackUpdate(Sender: TObject);
    procedure ViewPaletteExecute(Sender: TObject);
    procedure ViewPaletteUpdate(Sender: TObject);
    procedure ViewStatusBarExecute(Sender: TObject);
    procedure ViewStatusBarUpdate(Sender: TObject);
    procedure ViewToolboxUpdate(Sender: TObject);
    procedure ViewImagelistUpdate(Sender: TObject);
    procedure SpinEdit_EraserChange(Sender: TObject; AByUser: boolean);
    procedure ScriptExecute(Sender: TObject);
    procedure FileQuitExecute(Sender: TObject);
    procedure FileSaveUpdate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure ImageActionExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Image_SwapColorsMouseDown(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure SpinEdit_PenWidthChange(Sender: TObject; AByUser: boolean);
    procedure Tool_CloseShapeClick(Sender: TObject);
    procedure VectorialFill_ChooseColor({%H-}ASender: TObject; AButton: TMouseButton;
              AColorIndex: integer; var {%H-}AColorValue: TBGRAPixel; out AHandled: boolean);
    procedure SpinEdit_ArrowSizeChange(Sender: TObject; AByUser: boolean);
    procedure SpinEdit_ToleranceChange(Sender: TObject; AByUser: boolean);
    procedure Tool_ProgressiveFloodfillClick(Sender: TObject);
    procedure Tool_AliasingClick(Sender: TObject);
    procedure Tool_DrawShapeBorderClick(Sender: TObject);
    procedure Tool_FillShapeClick(Sender: TObject);
    procedure ToolMoveSelectionUpdate(Sender: TObject);
    procedure ViewToolboxExecute(Sender: TObject);
    procedure SpinEdit_PenWidthExit(Sender: TObject);
    procedure SpinEdit_GridNbExit(Sender: TObject);
    procedure ViewWorkspaceColorExecute(Sender: TObject);
    procedure WMEraseBkgnd(var {%H-}Message: TLMEraseBkgnd); message LM_ERASEBKGND;

  private
    procedure ComboBox_PenStyleChange(Sender: TObject);
    procedure ComboBox_PenStyleDrawItem({%H-}Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure ComboBox_PenStyleDrawSelectedItem(Sender: TObject;
      const ABGRA: TBGRABitmap; AState: TBCButtonState; ARect: TRect);
    function GetImage: TLazPaintImage;
    procedure ManagerBrushChanged(Sender: TObject);
    procedure ManagerBrushListChanged(Sender: TObject);
    procedure ManagerDeformationGridSizeChanged(Sender: TObject);
    procedure ManagerEraserChanged(Sender: TObject);
    procedure ManagerFloodFillOptionChanged(Sender: TObject);
    procedure ManagerJoinStyleChanged(Sender: TObject);
    procedure ManagerLineCapChanged(Sender: TObject);
    procedure ManagerPerspectiveOptionChanged(Sender: TObject);
    procedure ManagerPhongShapeChanged(Sender: TObject);
    procedure ManagerPenStyleChanged(Sender: TObject);
    procedure ManagerPenWidthChanged(Sender: TObject);
    procedure ManagerShapeRatioChanged(Sender: TObject);
    procedure ManagerSplineStyleChanged(Sender: TObject);
    procedure ManagerTextAlignChanged(Sender: TObject);
    procedure ManagerTextFontChanged(Sender: TObject);
    procedure ManagerTextOutlineChanged(Sender: TObject);
    procedure ManagerTextPhongChanged(Sender: TObject);
    procedure ManagerTextShadowChanged(Sender: TObject);
    procedure ManagerShapeOptionChanged(Sender: TObject);
    procedure ManagerToleranceChanged(Sender: TObject);
    procedure ManagerToolbarChanged(Sender: TObject);
    procedure Perspective_RepeatClick(Sender: TObject);
    procedure PictureMouseBefore(Sender: TObject; AShift: TShiftState);
    procedure PictureMouseMove(Sender: TObject; APosition: TPointF);
    procedure PictureOnPaint(Sender: TObject);
    procedure PictureToolbarUpdate(Sender: TObject);
    function ScriptShowColorDialog(AVars: TVariableSet): TScriptResult;
    procedure ThemeChanged(Sender: TObject);
    procedure VectorialFill_Change(Sender: TObject);
    procedure vectorialFill_ClickLabel(Sender: TObject);
    procedure VectorialFill_TypeChange(Sender: TObject);
    procedure VectorialFill_Resize(Sender: TObject);
    procedure VectorialFill_EditGradTexPoints(Sender: TObject);
    procedure VectorialFill_AdjustToShape(Sender: TObject);
    procedure VectorialFill_ShowBackFill(Sender: TObject; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure VectorialFill_ShowPenFill(Sender: TObject; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure VectorialFill_ShowOutlineFill(Sender: TObject; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
  private
    { private declarations }
    FLayout: TMainFormLayout;

    FLastWidth,FLastHeight,FLastBPP,FLastFrameIndex: integer;
    {$IFDEF LINUX}
    FTopMostHiddenMinimised: TTopMostInfo;
    {$ENDIF}
    FBrowseImages: TFBrowseImages;
    FBrowseSelections: TFBrowseImages;
    FBrowseTextures: TFBrowseImages;
    FBrowseBrushes: TFBrowseImages;
    FSaveImage: TFBrowseImages;
    FSaveSelection: TFBrowseImages;
    FLoadInitialDir, FSaveInitialDir, FExportInitialDir: string;
    FSaveSelectionInitialFilename: string;
    FInFillChange, FInPenWidthChange, FInBrush, FInShapeRatio, FInEraserOption,
    FInSplineStyleChange, FInFloodfillOption, FInTolerance,
    FInTextFont, FInTextAlign, FInTextShadow,
    FInPerspective, FInGridNb: Boolean;
    FOnlineUpdater: TLazPaintCustomOnlineUpdater;
    FInitialized: boolean;
    FShouldArrange: boolean;
    spacePressed, altPressed, snapPressed, shiftPressed: boolean;
    FirstPaint, LoadToolWindow: boolean;
    FShowSelectionNormal: boolean;
    FLazPaintInstance: TLazPaintCustomInstance;
    Config: TLazPaintConfig;
    StartDirectory: string;
    previousToolImg: integer;
    currentToolLabel: string;
    FTopMostInfo: TTopMostInfo;
    Panel_LineCap_FullSize: integer;
    FCoordinatesCaption: string;
    FCoordinatesCaptionCount: NativeInt;
    FToolbarElementsInitDone: boolean;
    FPenPlusMinus: integer;
    FLastDonateClick: TDateTime;
    FInHideFill: boolean;

    function GetDarkTheme: boolean;
    function GetImageAction: TImageActions;
    function GetUpdatingPopup: boolean;
    function GetUseImageBrowser: boolean;
    procedure SetDarkTheme(AValue: boolean);
    procedure SetUpdatingPopup(AValue: boolean);
    procedure UpdateStatusText;
    procedure CreateToolbarElements;
    function GetCurrentToolAction: TAction;
    procedure RegisterToolbarElements;
    procedure InitToolbarElements;
    procedure UpdateChooseColors;
    procedure UpdateAllowedFillTypes;
    procedure UpdateToolOptions;
    procedure UpdateEraserToolbar;
    procedure UpdatePenStyleToolbar;
    procedure UpdateJoinStyleToolbar;
    procedure UpdateShapeRatio;
    procedure UpdateTextFontToolbar(AForce: boolean);
    procedure UpdateTextAlign;
    procedure UpdateTextOutlineToolbar;
    procedure UpdateTextPhongToolbar;
    procedure UpdateTextShadowToolbar;
    procedure UpdateLineCapToolbar;
    procedure UpdateSplineStyleToolbar;
    procedure UpdatePenWidthToolbar;
    procedure UpdatePhongToolbar;
    procedure UpdateToleranceToolbar;
    procedure UpdateDeformationGridToolbar;
    procedure UpdateFloodfillToolbar;
    procedure UpdatePerspectiveToolbar;
    function ShowOpenBrushDialog: boolean;
    function TextSpinEditFocused: boolean;
    procedure UpdateBrush;
    procedure UpdateBrushList;
    function CatchToolKeyDown(var AKey: Word): boolean;
    function CatchToolKeyUp(var AKey: Word): boolean;
    function CatchToolKeyPress(var AKey: TUTF8Char): boolean;

    procedure CreateMenuAndToolbar;
    function GetToolManager: TToolManager;
    function GetCurrentTool: TPaintToolType;
    procedure SwitchColors;
    function EditingColors: boolean;
    procedure Init;
    procedure OnLatestVersionUpdate(ANewVersion: string);
    procedure ManagerToolChanged({%H-}sender: TToolManager; {%H-}ANewTool: TPaintToolType);
    procedure OnQueryExitToolHandler({%H-}sender: TLazPaintImage);
    procedure SetLazPaintInstance(const AValue: TLazPaintCustomInstance);
    procedure SetShowSelectionNormal(const AValue: boolean);
    procedure ToggleToolwindowsVisible;
    procedure UpdateTextSizeIncrement;
    procedure UpdateToolImage(AForceUpdate: boolean = false);
    procedure ToggleGridVisible;
    procedure ToggleToolboxVisible;
    procedure ToggleImageListVisible;
    procedure ToggleColorsVisible;
    procedure ToggleLayersVisible;
    function ShowColorDialogFor(ATarget: TColorTarget): boolean;
    procedure ShowPenPreview(ShouldRepaint: boolean= False);
    procedure HidePenPreview(ATimeMs: Integer = 300; AClearTime: boolean = false);
    procedure ShowFill(AFillControl: TLCVectorialFillControl; APanel: TPanel);
    procedure HideFill(ATimeMs: Integer = 300; AClearTime: boolean = false);
    procedure OnImageChangedHandler({%H-}AEvent: TLazPaintImageObservationEvent);
    procedure OnImageRenderChanged({%H-}Sender: TObject);
    procedure LabelAutosize(ALabel: TLabel; ATargetDPI: integer);
    procedure AskMergeSelection(ACaption: string);
    procedure UpdateSpecialKeys({%H-}Shift: TShiftState);
    procedure UpdateCurveModeToolbar;
    function ShowOpenTextureDialog(ATargetFill: TVectorialFill): boolean;
    procedure ShowNoPicture;
    procedure SetCurveMode(AMode: TToolSplineMode);
    procedure UpdatePenWidthFromSpinEdit;
    procedure UpdateWindowCaption;
    procedure ImageCurrentFilenameChanged({%H-}sender: TLazPaintImage);

    procedure RegisterScripts(ARegister: Boolean);
    function ScriptFileOpen(AVars: TVariableSet): TScriptResult;
    function ScriptFileSaveAs(AVars: TVariableSet): TScriptResult;
    function ScriptFileSave({%H-}AVars: TVariableSet): TScriptResult;
    function ScriptFileNewEntry(AVars: TVariableSet): TScriptResult;
    function ScriptFileChooseEntry(AVars: TVariableSet): TScriptResult;
    function ScriptFileGetFilename(AVars: TVariableSet): TScriptResult;
    function ScriptFileReload({%H-}AVars: TVariableSet): TScriptResult;
    function ScriptFileLoadSelection(AVars: TVariableSet): TScriptResult;
    function ScriptFileSaveSelectionAs(AVars: TVariableSet): TScriptResult;
    function ScriptEditPasteAsNew({%H-}AVars: TVariableSet): TScriptResult;
    function ScriptFilter(AVars: TVariableSet): TScriptResult;
    function ScriptChooseTool(AVars: TVariableSet): TScriptResult;
    function ScriptToolMouse(AVars: TVariableSet): TScriptResult;
    function ScriptToolKeys(AVars: TVariableSet): TScriptResult;
    function ScriptToolWrite(AVars: TVariableSet): TScriptResult;
    function ScriptViewZoomIn({%H-}AVars: TVariableSet): TScriptResult;
    function ScriptViewZoomOut({%H-}AVars: TVariableSet): TScriptResult;
    function ScriptViewZoomGet({%H-}AVars: TVariableSet): TScriptResult;
    function ScriptViewZoomSet({%H-}AVars: TVariableSet): TScriptResult;
    function ScriptViewZoomOriginal({%H-}AVars: TVariableSet): TScriptResult;
    function ScriptViewZoomFit({%H-}AVars: TVariableSet): TScriptResult;
    function ScriptViewGrid(AVars: TVariableSet): TScriptResult;
    function ScriptViewGridGet(AVars: TVariableSet): TScriptResult;
    function GetScriptContext: TScriptContext;
    procedure CallScriptFunction(AName:string); overload;
    procedure CallScriptFunction(AParams:TVariableSet); overload;
    procedure ZoomFitIfTooBig;
    property Scripting: TScriptContext read GetScriptContext;
    property Image: TLazPaintImage read GetImage;

  public
    { public declarations }
    Zoom: TZoom;

    procedure PaintPictureNow;
    procedure PaintPictureLater;
    procedure NotifyImagePaint;
    procedure InvalidatePicture(AInvalidateAll: boolean = true);
    function TryOpenFileUTF8(filenameUTF8: string; AddToRecent: Boolean=True;
      ALoadedImage: PImageEntry = nil; ASkipDialogIfSingleImage: boolean = false;
      AAllowDuplicate: boolean = false; AEntryToLoad: integer = -1): Boolean;
    procedure UpdateLineCapBar;
    procedure UpdateFillToolbar(AUpdateColorDiff: boolean);
    procedure UpdateToolbar;
    procedure AddDockedControl(AControl: TControl);
    procedure RemoveDockedControl(AControl: TControl);
    procedure QueryArrange;
    function ChooseTool(Tool : TPaintToolType): boolean;
    procedure PictureSelectedLayerIndexChanged({%H-}sender: TLazPaintImage);
    procedure PictureSelectedLayerIndexChanging({%H-}sender: TLazPaintImage);
    property LazPaintInstance: TLazPaintCustomInstance read FLazPaintInstance write SetLazPaintInstance;
    procedure UpdateEditPicture(ADelayed: boolean = false);
    property CurrentTool: TPaintToolType read GetCurrentTool;
    property CurrentToolAction: TAction read GetCurrentToolAction;
    property ShowSelectionNormal: boolean read FShowSelectionNormal write SetShowSelectionNormal;
    property ToolManager: TToolManager read GetToolManager;
    property Layout: TMainFormLayout read FLayout;
    property UseImageBrowser: boolean read GetUseImageBrowser;
    property DarkTheme: boolean read GetDarkTheme write SetDarkTheme;
    property Initialized: boolean read FInitialized;
    property UpdatingPopup: boolean read GetUpdatingPopup write SetUpdatingPopup;
    property ImageAction: TImageActions read GetImageAction;
  end;

implementation

uses LCLIntf, BGRAUTF8, ugraph, math, umac, uclipboard, ucursors,
   ufilters, ULoadImage, ULoading, UFileExtensions, UBrushType,
   ugeometricbrush, UPreviewDialog, UQuestion, BGRALayerOriginal,
   BGRATransform, LCVectorPolyShapes, URaw, UFileSystem,
   UTranslation, UPython, BCTypes;

const PenWidthFactor = 10;

{ TFMain }

{$i maintoolbar.inc}

procedure TFMain.FormCreate(Sender: TObject);
begin
  FInitialized := false;

  Zoom := TZoom.Create(Label_CurrentZoom, Edit_Zoom);
  FLayout := TMainFormLayout.Create(self, Zoom);
  FLayout.OnPaintPicture:=@PictureOnPaint;
  FLayout.OnToolbarUpdate:=@PictureToolbarUpdate;
  FLayout.OnPictureMouseMove:=@PictureMouseMove;
  FLayout.OnPictureMouseBefore:=@PictureMouseBefore;
  FInHideFill := false;

  //ScaleControl(Self,OriginalDPI);
  self.Color := clBtnFace; //toolbar color inherited on mac

  //mac interface
  CheckActions(ActionList1);
  CheckQuitMenu(ItemQuit,ItemQuitSeparator);

  {$IFDEF WINDOWS}
  StartDirectory := SysToUTF8(ExtractFilePath(Application.ExeName));
  {$ELSE}
  StartDirectory := GetCurrentDirUTF8;
  {$ENDIF}

  OpenPictureDialog1.Filter := GetExtensionFilter([eoReadable]);
  OpenTextureDialog.Filter := OpenPictureDialog1.Filter;
  LoadSelectionDialog.Filter := OpenPictureDialog1.Filter;
  OpenBrushDialog.Filter := OpenPictureDialog1.Filter;
  SavePictureDialog1.Filter := GetExtensionFilter([eoWritable]);
  ExportPictureDialog.Filter := SavePictureDialog1.Filter;
  SaveSelectionDialog.Filter := SavePictureDialog1.Filter;

  previousToolImg:= -1;

  spacePressed:= false;
  altPressed:= false;
  snapPressed:= false;
  shiftPressed:= false;

  {$IFDEF LINUX}
  ComboBox_BrushSelect.Top := ComboBox_BrushSelect.Top - 2;
  ComboBox_BrushSelect.Font.Height := -10;
  ComboBox_BrushSelect.Font.Height := -( 10 + ((Panel_Brush.ClientHeight-2)-ComboBox_BrushSelect.Height) );

  ComboBox_ArrowStart.Top := ComboBox_ArrowStart.Top - 2;
  ComboBox_ArrowStart.Font.Height := ComboBox_BrushSelect.Font.Height;
  ComboBox_ArrowEnd.Top := ComboBox_ArrowEnd.Top - 2;
  ComboBox_ArrowEnd.Font.Height := ComboBox_BrushSelect.Font.Height;
  {$ENDIF}

  FInitialized := true;
  FirstPaint := true;
end;

procedure TFMain.CreateMenuAndToolbar;
begin
  CreateToolbarElements;
  FLayout.DarkTheme := Config.GetDarkTheme;
  DarkThemeInstance.Apply(Panel_PenWidthPreview, Config.GetDarkTheme);
end;

function TFMain.GetToolManager: TToolManager;
begin
  result := FLazPaintInstance.ToolManager;
end;

procedure TFMain.FormDestroy(Sender: TObject);
begin
  if Assigned(FLazPaintInstance) then
    FLazPaintInstance.RegisterThemeListener(@ThemeChanged, false);

  if Assigned(Image) then
  begin
    Image.OnSelectionChanged := nil;
    Image.OnSelectedLayerIndexChanged:= nil;
    Image.OnSelectedLayerIndexChanging:= nil;
  end;
  FLayout.ToolBoxPopup := nil;
  RegisterScripts(False);

  If Assigned(ToolManager) then
  begin
    if ToolManager.OnToolChanged = @ManagerToolChanged then ToolManager.OnToolChanged := nil;
    if ToolManager.OnToolbarChanged = @ManagerToolbarChanged then ToolManager.OnToolbarChanged := nil;
    if ToolManager.OnEraserChanged = @ManagerEraserChanged then ToolManager.OnEraserChanged := nil;
    if ToolManager.OnPenWidthChanged = @ManagerPenWidthChanged then ToolManager.OnPenWidthChanged := nil;
    if ToolManager.OnBrushChanged = @ManagerBrushChanged then ToolManager.OnBrushChanged := nil;
    if ToolManager.OnBrushListChanged = @ManagerBrushListChanged then ToolManager.OnBrushListChanged := nil;
    if ToolManager.OnPenStyleChanged = @ManagerPenStyleChanged then ToolManager.OnPenStyleChanged := nil;
    if ToolManager.OnJoinStyleChanged = @ManagerJoinStyleChanged then ToolManager.OnJoinStyleChanged := nil;
    if ToolManager.OnShapeOptionChanged = @ManagerShapeOptionChanged then ToolManager.OnShapeOptionChanged := nil;
    if ToolManager.OnShapeRatioChanged = @ManagerShapeRatioChanged then ToolManager.OnShapeRatioChanged := nil;
    if ToolManager.OnTextFontChanged = @ManagerTextFontChanged then ToolManager.OnTextFontChanged := nil;
    if ToolManager.OnTextAlignChanged = @ManagerTextAlignChanged then ToolManager.OnTextAlignChanged := nil;
    if ToolManager.OnTextOutlineChanged = @ManagerTextOutlineChanged then ToolManager.OnTextOutlineChanged := nil;
    if ToolManager.OnTextPhongChanged = @ManagerTextPhongChanged then ToolManager.OnTextPhongChanged := nil;
    if ToolManager.OnTextShadowChanged = @ManagerTextShadowChanged then ToolManager.OnTextShadowChanged := nil;
    if ToolManager.OnLineCapChanged = @ManagerLineCapChanged then ToolManager.OnLineCapChanged := nil;
    if ToolManager.OnSplineStyleChanged = @ManagerSplineStyleChanged then ToolManager.OnSplineStyleChanged := nil;
    if ToolManager.OnPhongShapeChanged = @ManagerPhongShapeChanged then ToolManager.OnPhongShapeChanged := nil;
    if ToolManager.OnToleranceChanged = @ManagerToleranceChanged then ToolManager.OnToleranceChanged := nil;
    if ToolManager.OnDeformationGridChanged = @ManagerDeformationGridSizeChanged then ToolManager.OnDeformationGridChanged := nil;
    if ToolManager.OnFloodFillOptionChanged = @ManagerFloodFillOptionChanged then ToolManager.OnFloodFillOptionChanged := nil;
    if ToolManager.OnPerspectiveOptionChanged = @ManagerPerspectiveOptionChanged then ToolManager.OnPerspectiveOptionChanged := nil;
  end;
  FreeAndNil(FOnlineUpdater);

  FreeAndNil(FBrowseSelections);
  FreeAndNil(FBrowseImages);
  FreeAndNil(FBrowseTextures);
  FreeAndNil(FBrowseBrushes);
  if Config.DefaultRememberSaveFormat then
  begin
    if Assigned(FSaveImage) and Config.DefaultRememberSaveFormat and Config.DefaultUseImageBrowser then
      Config.SetSaveExtensions(FSaveImage.DefaultExtensions)
    else
      Config.SetSaveExtensions(GetExtensionFilterByIndex([eoWritable], SavePictureDialog1.FilterIndex));
  end;
  FreeAndNil(FSaveImage);
  FreeAndNil(FSaveSelection);

  FreeAndNil(FLayout);
  FreeAndNil(Zoom);
end;

procedure TFMain.SetLazPaintInstance(const AValue: TLazPaintCustomInstance);
begin
  if (FLazPaintInstance = nil) and (AValue <> nil) then
  begin
    FLazPaintInstance := AValue;
    FLayout.LazPaintInstance := AValue;
    Init;
    FLazPaintInstance.RegisterThemeListener(@ThemeChanged, true);
  end;
end;

procedure TFMain.Init;
begin
  FInitialized := false;
  Config := LazPaintInstance.Config;
  CreateMenuAndToolbar;

  if Config.Default3dObjectDirectory = '' then
    Config.SetDefault3dObjectDirectory(StartDirectory);

  MainMenu1.Images := LazPaintInstance.Icons[DoScaleX(20,OriginalDPI)];
  if Config.DefaultRememberStartupTargetDirectory then
    FSaveInitialDir := Config.DefaultStartupTargetDirectory;
  if Config.DefaultRememberStartupExportDirectory then
    FExportInitialDir := Config.DefaultStartupExportDirectory;
  if Config.DefaultRememberStartupSourceDirectory then
    FLoadInitialDir := Config.DefaultStartupSourceDirectory;
  FileRememberSaveFormat.Checked:= Config.DefaultRememberSaveFormat;

  if Config.DefaultRememberSaveFormat then
  begin
    SavePictureDialog1.FilterIndex := GetExtensionFilterIndex([eoWritable], Config.DefaultSaveExtensions);
    ExportPictureDialog.FilterIndex:= SavePictureDialog1.FilterIndex;
  end else
  begin
    SavePictureDialog1.FilterIndex := 1;
    ExportPictureDialog.FilterIndex:= 1;
  end;

  LazPaintInstance.EmbeddedResult := mrNone;

  Image.OnSelectedLayerIndexChanged:= @PictureSelectedLayerIndexChanged;
  Image.OnSelectedLayerIndexChanging:= @PictureSelectedLayerIndexChanging;

  RegisterToolbarElements;

  ToolManager.SetCurrentToolType(ptHand);
  ToolManager.OnToolChanged  :=  @ManagerToolChanged;
  ToolManager.OnToolbarChanged:=@ManagerToolbarChanged;
  ToolManager.OnEraserChanged:=@ManagerEraserChanged;
  ToolManager.OnPenWidthChanged:= @ManagerPenWidthChanged;
  ToolManager.OnBrushChanged:=@ManagerBrushChanged;
  ToolManager.OnBrushListChanged:=@ManagerBrushListChanged;
  ToolManager.OnPenStyleChanged:= @ManagerPenStyleChanged;
  ToolManager.OnJoinStyleChanged:= @ManagerJoinStyleChanged;
  ToolManager.OnShapeOptionChanged:=@ManagerShapeOptionChanged;
  ToolManager.OnShapeRatioChanged:=@ManagerShapeRatioChanged;
  ToolManager.OnTextFontChanged := @ManagerTextFontChanged;
  ToolManager.OnTextAlignChanged:=@ManagerTextAlignChanged;
  ToolManager.OnTextOutlineChanged:=@ManagerTextOutlineChanged;
  ToolManager.OnTextPhongChanged:=@ManagerTextPhongChanged;
  ToolManager.OnTextShadowChanged:=@ManagerTextShadowChanged;
  ToolManager.OnLineCapChanged := @ManagerLineCapChanged;
  ToolManager.OnSplineStyleChanged:=@ManagerSplineStyleChanged;
  ToolManager.OnPhongShapeChanged:=@ManagerPhongShapeChanged;
  ToolManager.OnToleranceChanged:=@ManagerToleranceChanged;
  ToolManager.OnDeformationGridChanged:=@ManagerDeformationGridSizeChanged;
  ToolManager.OnFloodFillOptionChanged:=@ManagerFloodFillOptionChanged;
  ToolManager.OnPerspectiveOptionChanged:=@ManagerPerspectiveOptionChanged;

  InitToolbarElements;

  Image.CurrentFilenameUTF8 := '';
  ImageAction.SetCurrentBitmap(TBGRABitmap.Create(Config.DefaultImageWidth,Config.DefaultImageHeight,Config.DefaultImageBackgroundColor), false);
  image.ClearUndo;
  image.SetSavedFlag(0, -1, 0);

  ViewGrid.Checked := LazPaintInstance.GridVisible;
  ColorCurves.Visible := not LazPaintInstance.BlackAndWhite;
  ColorColorize.Visible := not LazPaintInstance.BlackAndWhite;
  ColorShiftColors.Visible := not LazPaintInstance.BlackAndWhite;
  FilterComplementaryColor.Visible := not LazPaintInstance.BlackAndWhite;
  ColorIntensity.Visible := not LazPaintInstance.BlackAndWhite;
  FilterGrayscale.Visible := not LazPaintInstance.BlackAndWhite;
  FilterClearType.Visible := not LazPaintInstance.BlackAndWhite;
  FilterClearTypeInverse.Visible := not LazPaintInstance.BlackAndWhite;
  Panel_Embedded.Visible := LazPaintInstance.Embedded;
  Panel_File.Visible := Config.DefaultFileToolbarVisible and not LazPaintInstance.Embedded;
  MenuFileToolbar.Visible:= not LazPaintInstance.Embedded;
  Panel_Zoom.Visible := Config.DefaultZoomToolbarVisible;
  Panel_Undo.Visible := Config.DefaultUndoRedoToolbarVisible;
  Panel_CopyPaste.Visible := Config.DefaultCopyPasteToolbarVisible;
  Panel_Coordinates.Visible := Config.DefaultCoordinatesToolbarVisible;
  FLayout.ToolBoxPopup := PopupToolbox;

  if not LazPaintInstance.Embedded then
  begin
    FOnlineUpdater := LazPaintInstance.GetOnlineUpdater;
    If Assigned(FOnlineUpdater) then
      FOnlineUpdater.OnLatestVersionUpdate := @OnLatestVersionUpdate;
  end;
  if Config.DefaultToolboxWindowVisible and (FLayout.DefaultToolboxDocking <> twWindow) then
    FLayout.ToolBoxVisible := true;

  RegisterScripts(True);

  Image.OnImageChanged.AddObserver(@OnImageChangedHandler);
  Image.OnImageRenderChanged := @OnImageRenderChanged;
  Image.OnQueryExitToolHandler := @OnQueryExitToolHandler;
  Image.Zoom := Zoom;
  UpdateWindowCaption;
  Image.OnCurrentFilenameChanged := @ImageCurrentFilenameChanged;
  FInitialized := true;
end;

procedure TFMain.FormShow(Sender: TObject);
var
  m: TMainFormMenu;
  startFillControlWidth: LongInt;
  iconSize: Integer;
  toolbarDPI: integer;
begin
  if FLayout.Menu = nil then
  begin
    m := TMainFormMenu.Create(LazPaintInstance, ActionList1);
    m.DarkTheme := Config.GetDarkTheme;
    m.PredefinedMainMenus([MenuFile,MenuEdit,MenuSelect,MenuView, MenuImage,MenuRemoveTransparency,
      MenuColors,MenuTool, MenuFilter,MenuRadialBlur, MenuRender,MenuScript,MenuHelp]);
    m.Toolbars([Panel_Embedded,Panel_File,Panel_Zoom,Panel_Undo,Panel_CopyPaste,Panel_Coordinates,
      Panel_Tool,Panel_PenFill,Panel_SwapColor,Panel_BackFill,Panel_ColorDiff,Panel_Grid,
      Panel_ShapeOption,Panel_PenWidth,Panel_PenStyle,Panel_JoinStyle,
      Panel_CloseShape,Panel_LineCap,Panel_Aliasing,
      Panel_SplineStyle,Panel_Eraser,Panel_Tolerance,Panel_Text,Panel_Altitude,Panel_TextShadow,Panel_TextOutline,
      Panel_OutlineFill,Panel_PhongShape,Panel_PerspectiveOption,Panel_Brush,Panel_Ratio,Panel_Donate],Panel_ToolbarBackground);
    iconSize := round(Config.DefaultIconSize(ScaleY(16, 96)) * 16 / 20);
    if iconSize < 16 then iconSize := 16;
    toolbarDPI := round(96*iconSize/16);
    m.ScaleToolbars(toolbarDPI);
    ScaleControl(Panel_PenWidthPreview, OriginalDPI, toolbarDPI, toolbarDPI);
    LabelAutosize(Label_Pen, toolbarDPI);
    LabelAutosize(Label_Back, toolbarDPI);
    LabelAutosize(Label_PenWidth, toolbarDPI);
    LabelAutosize(Label_Eraser, toolbarDPI);
    LabelAutosize(Label_Tolerance, toolbarDPI);
    LabelAutosize(Label_Grid, toolbarDPI);
    LabelAutosize(Label_Curve, toolbarDPI);
    LabelAutosize(Label_Text, toolbarDPI);
    LabelAutosize(Label_TextBlur, toolbarDPI);
    LabelAutosize(Label_ShadowOffset, toolbarDPI);
    LabelAutosize(Label_Shape, toolbarDPI);
    LabelAutosize(Label_PhongBorder, toolbarDPI);
    LabelAutosize(Label_Altitude, toolbarDPI);
    LabelAutosize(Label_OutlineWidth, toolbarDPI);
    LabelAutosize(Label_Brush, toolbarDPI);
    LabelAutosize(Label_Spacing, toolbarDPI);
    LabelAutosize(Label_Ratio, toolbarDPI);
    LabelAutosize(Label_Donate, toolbarDPI);
    m.ImageList := LazPaintInstance.Icons[iconSize];
    m.Apply;
    FLayout.Menu := m;

    Layout.DockedToolBoxAddButton(ToolChangeDocking);

    Layout.DockedToolBoxAddButton(ToolHand);
    Layout.DockedToolBoxAddButton(ToolColorPicker);
    Layout.DockedToolBoxAddButton(ToolPen);
    Layout.DockedToolBoxAddGroup([ToolBrush, ToolClone]);
    Layout.DockedToolBoxAddButton(ToolEraser);
    Layout.DockedToolBoxAddGroup([ToolFloodfill, ToolGradient]);

    Layout.DockedToolBoxAddButton(ToolEditShape);
    Layout.DockedToolBoxAddGroup([ToolRect, ToolEllipse, ToolPhong]);
    Layout.DockedToolBoxAddGroup([ToolPolyline, ToolOpenedCurve]);
    Layout.DockedToolBoxAddGroup([ToolPolygon, ToolSpline]);
    Layout.DockedToolBoxAddButton(ToolText);

    Layout.DockedToolBoxAddButton(ToolDeformation);
    Layout.DockedToolBoxAddButton(ToolTextureMapping);

    Layout.DockedToolBoxAddGroup([ToolSelectRect, ToolSelectEllipse]);
    Layout.DockedToolBoxAddGroup([ToolSelectPoly, ToolSelectSpline]);

    Layout.DockedToolBoxAddButton(ToolSelectPen);
    Layout.DockedToolBoxAddButton(ToolMagicWand);

    Layout.DockedToolBoxAddGroup([ToolMoveSelection, ToolRotateSelection]);
    Layout.DockedToolBoxAddButton(EditDeselect);

    Layout.DockedToolBoxSetImages(LazPaintInstance.Icons[Config.DefaultIconSize(DoScaleX(20,OriginalDPI))]);
  end;

  LazPaintInstance.ColorToFChooseColor;
  LazPaintInstance.ShowTopmost(FTopMostInfo);
  if Position = poDefault then LazPaintInstance.RestoreMainWindowPosition;

  startFillControlWidth := VectorialFill_Pen.PreferredSize.Width;
  VectorialFill_Pen.Width := startFillControlWidth;
  VectorialFill_Back.Width := startFillControlWidth;
  VectorialFill_Outline.Width := startFillControlWidth;
  ToolManager.UpdateContextualToolbars;
  UpdateToolImage;
  UpdateToolBar;
  FShouldArrange := true;
  QueryArrange;
end;

procedure TFMain.OnLatestVersionUpdate(ANewVersion: string);
begin
  if ANewVersion <> LazPaintVersionStr then
    LazPaintInstance.ShowMessage(rsLazPaint, rsLatestVersion + ' ' + ANewVersion);
end;

procedure TFMain.RegisterScripts(ARegister: Boolean);
begin
  Scripting.RegisterScriptFunction('FileOpen',@ScriptFileOpen,ARegister);
  Scripting.RegisterScriptFunction('FileSaveAs',@ScriptFileSaveAs,ARegister);
  Scripting.RegisterScriptFunction('FileSave',@ScriptFileSave,ARegister);
  Scripting.RegisterScriptFunction('GetFileName',@ScriptFileGetFilename,ARegister);
  Scripting.RegisterScriptFunction('FileReload',@ScriptFileReload,ARegister);
  Scripting.RegisterScriptFunction('FileChooseEntry',@ScriptFileChooseEntry,ARegister);
  Scripting.RegisterScriptFunction('FileNewEntry',@ScriptFileNewEntry,ARegister);
  Scripting.RegisterScriptFunction('FileLoadSelection',@ScriptFileLoadSelection,ARegister);
  Scripting.RegisterScriptFunction('FileSaveSelectionAs',@ScriptFileSaveSelectionAs,ARegister);
  Scripting.RegisterScriptFunction('EditPasteAsNew',@ScriptEditPasteAsNew,ARegister);
  Scripting.RegisterScriptFunction('Filter',@ScriptFilter,ARegister);
  Scripting.RegisterScriptFunction('ChooseTool',@ScriptChooseTool,ARegister);
  Scripting.RegisterScriptFunction('ToolMouse',@ScriptToolMouse,ARegister);
  Scripting.RegisterScriptFunction('ToolKeys',@ScriptToolKeys,ARegister);
  Scripting.RegisterScriptFunction('ToolWrite',@ScriptToolWrite,ARegister);
  Scripting.RegisterScriptFunction('ViewZoomIn',@ScriptViewZoomIn,ARegister);
  Scripting.RegisterScriptFunction('ViewZoomOut',@ScriptViewZoomOut,ARegister);
  Scripting.RegisterScriptFunction('ViewZoomGet',@ScriptViewZoomGet,ARegister);
  Scripting.RegisterScriptFunction('ViewZoomSet',@ScriptViewZoomSet,ARegister);
  Scripting.RegisterScriptFunction('ViewZoomOriginal',@ScriptViewZoomOriginal,ARegister);
  Scripting.RegisterScriptFunction('ViewZoomFit',@ScriptViewZoomFit,ARegister);
  Scripting.RegisterScriptFunction('ViewGrid',@ScriptViewGrid,ARegister);
  Scripting.RegisterScriptFunction('ViewGridGet',@ScriptViewGridGet,ARegister);
  Scripting.RegisterScriptFunction('ShowColorDialog',@ScriptShowColorDialog,ARegister);
end;

function TFMain.ScriptFileOpen(AVars: TVariableSet): TScriptResult;
var vFilename: TScriptVariableReference;
    topInfo: TTopMostInfo;
    i: integer;
    mr: TModalResult;
    cancelled: boolean;
    chosenFiles: array of string;
    loadedImage: TImageEntry;
begin
  loadedImage := TImageEntry.Empty;
  try
    topInfo.defined:= false;
    if Image.IsFileModified and not AVars.Booleans['IgnoreModified'] then
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
      if TryOpenFileUTF8(AVars.GetString(vFilename), true, nil,
           false, false) then
        result := srOk
      else
        result := srException;
    end
    else
    begin
      if UseImageBrowser then
      begin
        if not assigned(FBrowseImages) then
        begin
          FBrowseImages := TFBrowseImages.Create(self);
          FBrowseImages.LazPaintInstance := LazPaintInstance;
          FBrowseImages.Filter:= OpenPictureDialog1.Filter;
          FBrowseImages.ShowRememberStartupDirectory := true;
        end;
      end;
      try
        if not topInfo.defined then topInfo := FLazPaintInstance.HideTopmost;
        if UseImageBrowser then
        begin
          self.Hide;
          FBrowseImages.InitialDirectory:= FLoadInitialDir;
          FBrowseImages.RememberStartDirectory:= FLazPaintInstance.Config.DefaultRememberStartupSourceDirectory;
          FBrowseImages.AllowMultiSelect:= true;
          FBrowseImages.FilterIndex := OpenPictureDialog1.FilterIndex;
          mr := FBrowseImages.ShowModal;
          LazPaintInstance.Config.SetRememberStartupSourceDirectory(FBrowseImages.RememberStartDirectory);
          if mr = mrOK then
          begin
            OpenPictureDialog1.FilterIndex := FBrowseImages.FilterIndex;
            setlength(chosenFiles, FBrowseImages.SelectedFileCount);
            for i := 0 to high(chosenFiles) do
              chosenFiles[i] := FBrowseImages.SelectedFile[i];
            loadedImage := FBrowseImages.GetChosenImage;
            cancelled := false
          end
          else
          begin
            chosenFiles := nil;
            cancelled := true;
          end;
          FBrowseImages.AllowMultiSelect:= false;
        end else
        begin
          OpenPictureDialog1.InitialDir:= FLoadInitialDir;
          OpenPictureDialog1.Options := OpenPictureDialog1.Options + [ofAllowMultiSelect];
          if OpenPictureDialog1.Execute then
          begin
            setlength(chosenFiles,1);
            chosenFiles[0]:= OpenPictureDialog1.FileName;
            cancelled := false;
          end
          else
          begin
            chosenFiles:= nil;
            cancelled:= true;
          end;
          OpenPictureDialog1.Options := OpenPictureDialog1.Options - [ofAllowMultiSelect];
        end;
        if not cancelled then
        begin
          if length(chosenFiles) = 1 then
          begin
            if TryOpenFileUTF8(chosenFiles[0],true,@loadedImage,true) then
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
          FLoadInitialDir:= ExtractFilePath(chosenFiles[0]);
          if Config.DefaultRememberStartupSourceDirectory then
            Config.SetStartupSourceDirectory(FLoadInitialDir);
        end
        else
          result := srCancelledByUser;
      finally
        self.Show;
        FLazPaintInstance.ShowTopmost(topInfo);
      end;
    end;
  except
    on ex: Exception do
    begin
      LazPaintInstance.ShowError(RemoveTrail(FileOpen.Caption),ex.Message);
      result := srException;
    end;
  end;
  loadedImage.FreeAndNil;
end;

procedure TFMain.FileQuitExecute(Sender: TObject);
begin
  Close;
end;

function TFMain.ScriptFileSaveAs(AVars: TVariableSet): TScriptResult;
var
  dialogTitle: string;

  function DoSaveAs(filename: string; AExport: boolean): TScriptResult;
  var saved: boolean;
  begin
    if not Image.AbleToSaveAsUTF8(filename) then
    begin
      LazPaintInstance.ShowError(dialogTitle, rsFileExtensionNotSupported);
      result := srException;
    end else
    if not Image.CheckNoAction then result := srException else
    begin
      try
        saved := false;
        if (Image.currentFilenameUTF8 <> '') and
          ( ((SuggestImageFormat(Image.currentFilenameUTF8) in [ifIco,ifCur])
           and (SuggestImageFormat(filename) in [ifIco,ifCur])) or
           ((SuggestImageFormat(Image.currentFilenameUTF8) = ifTiff)
           and (SuggestImageFormat(filename) = ifTiff)) ) then
        begin
           Image.UpdateMultiImage(filename, AExport);
           saved := true;
        end
        else
        begin
          if not LazPaintInstance.ShowSaveOptionDlg(nil, filename,
                   AVars.Booleans['SkipOptions'], AExport) then
            result := srCancelledByUser
          else
            saved := true;
        end;

        if saved then
        begin
          if AExport then
          begin
            FExportInitialDir := extractFilePath(filename);
            if Config.DefaultRememberStartupExportDirectory then
              Config.SetStartupExportDirectory(FExportInitialDir);
          end else
          begin
            Config.AddRecentFile(filename);
            Config.AddRecentDirectory(ExtractFilePath(filename));
            FSaveInitialDir := extractFilePath(filename);
            if Config.DefaultRememberStartupTargetDirectory then
              Config.SetStartupTargetDirectory(FSaveInitialDir);
            Image.CurrentFilenameUTF8 := filename;
          end;
          AVars.Strings['Result'] := filename;
          result := srOk;
          if Assigned(Scripting.RecordingFunctionParameters) then
          begin
            Scripting.RecordingFunctionParameters.AddString('FileName',filename);
            Scripting.RecordingFunctionParameters.AddBoolean('Validate',True);
          end;
        end;
      except
        on ex: Exception do
        begin
          LazPaintInstance.ShowError(dialogTitle, ex.Message);
          result := srException;
        end;
      end;
    end;
  end;

var filename: string;
    vFileName: TScriptVariableReference;
    topMost: TTopMostInfo;
    defaultExt: string;
    initialDir: string;
    saveDlg: TSaveDialog;
    mr: TModalResult;
begin
  if AVars.Booleans['Export'] then
    saveDlg := ExportPictureDialog
  else
    saveDlg := SavePictureDialog1;
  dialogTitle := RemoveTrail(saveDlg.Title);
  AskMergeSelection(dialogTitle);
  filename := ExtractFileName(Image.CurrentFilenameUTF8);
  vFileName := AVars.GetVariable('FileName');
  if AVars.IsReferenceDefined(vFileName) then
  begin
    filename := AVars.GetString(vFileName);
    initialDir := ExtractFilePath(filename);
    {$WARNINGS OFF}
    if PathDelim <> '\' then initialDir := StringReplace(initialDir, '\', PathDelim, [rfReplaceAll]);
    if PathDelim <> '/' then initialDir := StringReplace(initialDir, '/', PathDelim, [rfReplaceAll]);
    {$WARNINGS ON}
    filename := ExtractFileName(filename);
  end else initialDir:= '';
  if filename = '' then filename := rsNoName;
  if initialDir = '' then
  begin
    if AVars.Booleans['Export'] then
      initialDir := FExportInitialDir
    else
      initialDir:= FSaveInitialDir;
  end;
  if saveDlg.FilterIndex > 1 then
    filename := ApplySelectedFilterExtension(filename,saveDlg.Filter,saveDlg.FilterIndex);
  if not Image.AbleToSaveAsUTF8(filename) then
  begin
    saveDlg.FilterIndex := 1;
    filename := ChangeFileExt(Filename,'');
  end;

  case SuggestImageFormat(Image.CurrentFilenameUTF8) of
  ifCur: defaultExt := '.cur';
  ifIco: defaultExt := '.ico';
  else
    begin
      if Image.NbLayers > 1 then defaultExt := '.lzp' else
        defaultExt := '.png';
    end;
  end;

  if AVars.Booleans['Validate'] and (initialDir <> '') then
  begin
    if not AVars.Booleans['Overwrite'] and (FileManager.FileExists(initialDir+filename)) then
    begin
      if QuestionDlg(rsSave, rsOverwriteFile, mtConfirmation,
          [mrOk, rsOkay, mrCancel, rsCancel],0) <> mrOk then
             exit(srCancelledByUser);
    end;
    result := DoSaveAs(initialDir+filename, AVars.Booleans['Export']);
  end else
  begin
    topMost := LazPaintInstance.HideTopmost;
    if UseImageBrowser then
    begin
      if not assigned(FSaveImage) then
      begin
        FSaveImage := TFBrowseImages.Create(self);
        FSaveImage.LazPaintInstance := LazPaintInstance;
        FSaveImage.IsSaveDialog := true;
        FSaveImage.ShowRememberStartupDirectory:= true;
        if Config.DefaultRememberSaveFormat then
          FSaveImage.DefaultExtensions:= Config.DefaultSaveExtensions;
      end;
      FSaveImage.Filter := saveDlg.Filter;
      FSaveImage.Caption := saveDlg.Title;
      FSaveImage.InitialFilename := filename;
      FSaveImage.DefaultExtension := defaultExt;
      FSaveImage.InitialDirectory:= initialDir;
      if AVars.Booleans['Export'] then
        FSaveImage.RememberStartDirectory:= FLazPaintInstance.Config.DefaultRememberStartupExportDirectory
        else FSaveImage.RememberStartDirectory:= FLazPaintInstance.Config.DefaultRememberStartupTargetDirectory;
      FSaveImage.FilterIndex := saveDlg.FilterIndex;
      mr := FSaveImage.ShowModal;
      if AVars.Booleans['Export'] then
        LazPaintInstance.Config.SetRememberStartupExportDirectory(FSaveImage.RememberStartDirectory)
        else LazPaintInstance.Config.SetRememberStartupTargetDirectory(FSaveImage.RememberStartDirectory);
      if mr = mrOK then
      begin
        saveDlg.FilterIndex := FSaveImage.FilterIndex;
        result := DoSaveAs(FSaveImage.FileName, AVars.Booleans['Export']);
      end
      else
        result := srCancelledByUser;
    end else
    begin
      saveDlg.FileName := filename;
      saveDlg.DefaultExt := defaultExt;
      saveDlg.InitialDir:= initialDir;
      if saveDlg.Execute then
      begin
        result := DoSaveAs(saveDlg.FileName, AVars.Booleans['Export']);
      end else
        result := srCancelledByUser;
    end;
    LazPaintInstance.ShowTopmost(topMost);
  end;
end;

function TFMain.ScriptFileSave(AVars: TVariableSet): TScriptResult;
begin
  if (Image.CurrentFilenameUTF8 = '') or not Image.AbleToSaveAsUTF8(Image.CurrentFilenameUTF8) then
    result := Scripting.CallScriptFunction('FileSaveAs', True) else
    begin
      AskMergeSelection(rsSave);
      try
        if not Image.CheckNoAction then result := srException else
        if SuggestImageFormat(Image.currentFilenameUTF8) in [ifIco,ifCur,ifTiff,ifGif] then
        begin
           LazPaintInstance.StartSavingImage(Image.currentFilenameUTF8);
           Image.UpdateMultiImage;
           LazPaintInstance.EndSavingImage;
           AVars.Strings['Result'] := Image.currentFilenameUTF8;
           result := srOk;
        end
        else
        begin
          if LazPaintInstance.ShowSaveOptionDlg(nil, Image.currentFilenameUTF8, AVars.Booleans['SkipOptions'], False) then
          begin
            AVars.Strings['Result'] := Image.currentFilenameUTF8;
            result := srOk;
          end
          else
            result := srCancelledByUser;
        end;

      except
        on ex: Exception do
        begin
          LazPaintInstance.ShowError(rsSave,ex.Message);
          result := srException;
        end;
      end;
    end;
end;

function TFMain.ScriptFileNewEntry(AVars: TVariableSet): TScriptResult;
var w,h: integer;
  topInfo: TTopMostInfo;
  backColor: TBGRAPixel;
begin
  if (Image.currentFilenameUTF8='') or not Image.CanHaveFrames then exit(srException);
  topInfo.defined:= false;
  if Image.IsFileModified and not AVars.Booleans['IgnoreModified'] then
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
  FLazPaintInstance.ShowTopmost(topInfo);

  if Image.CanDuplicateFrame then
  begin
    w := Image.Width;
    h := Image.Height;
  end else
  begin
    w := 0;
    h := 0;
  end;
  if AVars.IsDefined('Width') then w := AVars.Integers['Width'];
  if AVars.IsDefined('Height') then h := AVars.Integers['Height'];
  if (h <= 0) or (w <= 0) then exit(srInvalidParameters);
  if Image.IsGif and ((w <> Image.Width) or (h <> Image.Height)) then exit(srInvalidParameters);
  backColor := AVars.Pixels['BackColor'];

  Image.BPP:= 0;
  Image.FrameIndex:= -1;
  Image.Assign(TBGRABitmap.Create(w,h,backColor), true, false);
  result := srOk;
end;

function TFMain.ScriptFileChooseEntry(AVars: TVariableSet): TScriptResult;
var
  topInfo: TTopMostInfo;
  entryToLoad: integer;
  vEntryIndex: TScriptVariableReference;
begin
  if (Image.currentFilenameUTF8='') or not Image.CanHaveFrames then exit(srException);
  topInfo.defined:= false;
  if Image.IsFileModified and not AVars.Booleans['IgnoreModified'] then
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
  FLazPaintInstance.ShowTopmost(topInfo);
  vEntryIndex := AVars.GetVariable('EntryIndex');
  if AVars.IsReferenceDefined(vEntryIndex) then
  begin
    entryToLoad := AVars.GetInteger(vEntryIndex)-1;
    if entryToLoad < 0 then entryToLoad := -1;
  end
  else entryToLoad := -1;
  if TryOpenFileUTF8(Image.currentFilenameUTF8, false, nil,
       true, Image.CanDuplicateFrame, entryToLoad) then
  begin
    AVars.Integers['Result'] := Image.FrameIndex;
    result := srOk;
  end
  else
    result := srException;
end;

function TFMain.ScriptFileGetFilename(AVars: TVariableSet): TScriptResult;
begin
  if Image.currentFilenameUTF8='' then
    AVars.Remove('Result')
  else
    AVars.Strings['Result'] := Image.currentFilenameUTF8;
  result := srOk;
end;

procedure TFMain.FileSaveUpdate(Sender: TObject);
begin
   FileSave.Enabled := image.IsFileModified;
end;

procedure TFMain.FilterAnyExecute(Sender: TObject);
var filterName: string;
    params: TVariableSet;
begin
  if Assigned(LazPaintInstance) then LazPaintInstance.ExitColorEditor;
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

procedure TFMain.TimerLoadToolWinTimer(Sender: TObject);
var r: TRect;
begin
  if LoadToolWindow then
  begin
    LoadToolWindow := false;
    //show in descending order of size
    r := Config.DefaultImageListPosition;
    if (r.right > r.left) and (r.bottom > r.top) then
    begin
      LazPaintInstance.MoveImageListWindowTo(r.left,r.top);
      LazPaintInstance.ImageListWindowWidth:= r.Width;
      LazPaintInstance.ImageListWindowHeight:= r.Height;
    end
    else
      LazPaintInstance.MoveImageListWindowTo(self.Left+self.Width-LazPaintInstance.ImageListWindowWidth,
                         self.Top+self.Height-LazPaintInstance.ChooseColorHeight-LazPaintInstance.ImageListWindowHeight);
    LazPaintInstance.ImageListWindowVisible := Config.DefaultImagelistWindowVisible;

    r := Config.DefaultLayerWindowPosition;
    if (r.right > r.left) and (r.bottom > r.top) then
    begin
      LazPaintInstance.MoveLayerWindowTo(r.left,r.top);
      LazPaintInstance.LayerWindowWidth := r.Width;
      LazPaintInstance.LayerWindowHeight := r.Height;
    end
    else
      LazPaintInstance.MoveLayerWindowTo(self.Left+self.Width-LazPaintInstance.LayerWindowWidth,
                         self.Top+self.Height-LazPaintInstance.ChooseColorHeight-LazPaintInstance.LayerWindowHeight);
    LazPaintInstance.LayerWindowVisible:= Config.DefaultLayerWindowVisible;

    r := Config.DefaultColorWindowPosition;
    if (r.right > r.left) and (r.bottom > r.top) then
    begin
      LazPaintInstance.MoveChooseColorTo(r.left,r.top);
      LazPaintInstance.ChooseColorWidth := r.Width;
      LazPaintInstance.ChooseColorHeight := r.Height;
    end
    else
      LazPaintInstance.MoveChooseColorTo(self.Left+self.Width-LazPaintInstance.ChooseColorWidth,
                         self.Top+self.Height-LazPaintInstance.ChooseColorHeight);
    LazPaintInstance.ChooseColorVisible:= Config.DefaultColorWindowVisible;

    r := Config.DefaultToolboxWindowPosition;
    if (r.right > r.left) and (r.bottom > r.top) then
      LazPaintInstance.MoveToolboxTo(r.left,r.Top)
    else
      LazPaintInstance.MoveToolboxTo(self.Left,self.Top+self.Height-LazPaintInstance.ToolBoxHeight);
    LazPaintInstance.ToolboxVisible := Config.DefaultToolboxWindowVisible;
  end;
  TimerLoadToolWin.Enabled := false;
end;

procedure TFMain.RenderAnyExecute(Sender: TObject);
var filterName: string;
    params: TVariableSet;
begin
  if Assigned(LazPaintInstance) then LazPaintInstance.ExitColorEditor;
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
    params.AddBoolean('FromGUI', true);
    CallScriptFunction(params);
    params.Free;
  end;
end;

procedure TFMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  try
    if Key = VK_MENU then altPressed:= true
    else if (Key = VK_SNAP) or (Key = VK_SNAP2) then snapPressed:= true
    else if Key = VK_SHIFT then shiftPressed:= true;
    if Zoom.EditingZoom or EditingColors then exit;
    if not ((CurrentTool = ptText) and TextSpinEditFocused and (Key = VK_BACK)) and CatchToolKeyDown(Key) then
    begin
      PaintPictureLater;
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
      if not ToolManager.ToolSleeping and ([ssLeft,ssRight] * FLayout.MouseButtonState = []) then ToolManager.ToolSleep;
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
  QueryArrange;
end;

procedure TFMain.ImageActionExecute(Sender: TObject);
var actionName: string;
begin
  if Sender is TAction then
  begin
    actionName:= (Sender as TAction).Name;
    CallScriptFunction(actionName);
  end;
end;

procedure TFMain.EditUndoUpdate(Sender: TObject);
begin
  EditUndo.Enabled := image.CanUndo or ToolManager.ToolProvideCommand(tcFinish);
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
  dir3d: String;
begin
  topmostInfo:= LazPaintInstance.HideTopmost;
  Open3DObjectDialog.InitialDir := Config.Default3dObjectDirectory;
  if Open3DObjectDialog.InitialDir = '' then
  begin
    dir3d := GetResourcePath('models');
    if DirectoryExistsUTF8(dir3d) then
      Open3DObjectDialog.InitialDir := dir3d;
  end;
  if Open3DObjectDialog.Execute then
  begin
    ImageAction.Import3DObject(Open3DObjectDialog.FileName);
    Config.SetDefault3dObjectDirectory(ExtractFilePath(Open3DObjectDialog.FileName));
  end;
  LazPaintInstance.ShowTopmost(topmostInfo);
end;

function TFMain.ScriptFileLoadSelection(AVars: TVariableSet): TScriptResult;
var selectionFileName: string;
    vFileName: TScriptVariableReference;
    topmost: TTopMostInfo;
    loadedImage: TImageEntry;
begin
  loadedImage := TImageEntry.Empty;
  vFileName := AVars.GetVariable('FileName');
  if AVars.IsReferenceDefined(vFileName) then
    selectionFileName := AVars.GetString(vFileName)
  else
  begin
    topmost := LazPaintInstance.HideTopmost;
    if UseImageBrowser then
    begin
      if not assigned(FBrowseSelections) then
      begin
        FBrowseSelections := TFBrowseImages.Create(self);
        FBrowseSelections.LazPaintInstance := LazPaintInstance;
        FBrowseSelections.Filter := LoadSelectionDialog.Filter;
        FBrowseSelections.AllowMultiSelect := false;
        FBrowseSelections.Caption := LoadSelectionDialog.Title;
      end;
      FBrowseSelections.FilterIndex:= LoadSelectionDialog.FilterIndex;
      if FBrowseSelections.ShowModal = mrOK then
      begin
        LoadSelectionDialog.FilterIndex := FBrowseSelections.FilterIndex;
        LazPaintInstance.ShowTopmost(topmost);
        selectionFileName := FBrowseSelections.Filename;
        loadedImage := FBrowseSelections.GetChosenImage;
        Config.AddRecentDirectory(ExtractFilePath(selectionFileName));
      end else
      begin
        result := srCancelledByUser;
        LazPaintInstance.ShowTopmost(topmost);
        exit;
      end;
    end else
    begin
      if LoadSelectionDialog.Execute then
      begin
        LazPaintInstance.ShowTopmost(topmost);
        selectionFileName := LoadSelectionDialog.Filename
      end
      else
      begin
        result := srCancelledByUser;
        LazPaintInstance.ShowTopmost(topmost);
        exit;
      end;
    end;
  end;
  if ImageAction.LoadSelection(selectionFileName, @loadedImage) then
  begin
    FSaveSelectionInitialFilename := selectionFileName;
    if Assigned(Scripting.RecordingFunctionParameters) then
       Scripting.RecordingFunctionParameters.AddString('FileName',selectionFileName);
    result := srOk;
  end
  else result := srException;
  loadedImage.FreeAndNil;
end;

function TFMain.ScriptFileReload(AVars: TVariableSet): TScriptResult;
var topmostInfo: TTopMostInfo; res: integer;
begin
  if (Image.CurrentFilenameUTF8 = '') then
  begin
    result := srOk;
    exit;
  end;
  if Image.IsFileModified and not AVars.Booleans['IgnoreModified'] then
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
  if TryOpenFileUTF8(Image.CurrentFilenameUTF8,false,nil,
                     true,false,Image.FrameIndex) then
    result := srOk
  else
    result := srException;
end;

procedure TFMain.FileReloadUpdate(Sender: TObject);
begin
  FileReload.Enabled := (Image.CurrentFilenameUTF8 <> '');
end;

function TFMain.ScriptFileSaveSelectionAs(AVars: TVariableSet): TScriptResult;
var filename, initialDir: string;
    vFileName: TScriptVariableReference;
begin
  if Image.SelectionMaskEmpty then
  begin
    result := srException;
    exit;
  end;
  filename := '';
  vFileName := AVars.GetVariable('FileName');
  if AVars.IsReferenceDefined(vFileName) then
  begin
    filename:= AVars.GetString(vFileName);
    {$WARNINGS OFF}
    if PathDelim <> '\' then filename := StringReplace(filename, '\', PathDelim, [rfReplaceAll]);
    if PathDelim <> '/' then filename := StringReplace(filename, '/', PathDelim, [rfReplaceAll]);
    {$WARNINGS ON}
    if ExtractFilePath(filename)<>'' then fileName := ExpandFileName(filename);
  end;
  if filename = '' then filename := FSaveSelectionInitialFilename;
  initialDir := ExtractFilePath(filename);
  if AVars.Booleans['Validate'] and (filename <> '') and (initialDir <> '') then
  begin
    //skip
  end else
  if UseImageBrowser then
  begin
    if not assigned(FSaveSelection) then
    begin
      FSaveSelection := TFBrowseImages.Create(self);
      FSaveSelection.LazPaintInstance := LazPaintInstance;
      FSaveSelection.IsSaveDialog := true;
      FSaveSelection.Filter := SaveSelectionDialog.Filter;
      FSaveSelection.Caption := SaveSelectionDialog.Title;
      FSaveSelection.DefaultExtension := SaveSelectionDialog.DefaultExt;
    end;
    if initialDir<>'' then FSaveSelection.InitialDirectory := initialDir;
    FSaveSelection.InitialFilename := ExtractFileName(filename);
    FSaveSelection.FilterIndex := SaveSelectionDialog.FilterIndex;
    if (FSaveSelection.ShowModal = mrOk) and (FSaveSelection.Filename <> '') then
    begin
      SaveSelectionDialog.FilterIndex := FSaveSelection.FilterIndex;
      filename := FSaveSelection.Filename;
    end
    else
      filename := '';
  end else
  begin
    SaveSelectionDialog.FileName:= filename;
    if pos(PathDelim,SaveSelectionDialog.FileName)<>0 then
    begin
      filename := SaveSelectionDialog.FileName;
      SaveSelectionDialog.FileName := ExtractFileName(filename);
      if initialDir<>'' then SaveSelectionDialog.InitialDir := initialDir;
    end;
    if SaveSelectionDialog.Execute then
      filename := SaveSelectionDialog.FileName
    else
      filename := '';
  end;
  if filename <> '' then
  begin
    if not Image.AbleToSaveSelectionAsUTF8(filename) then
    begin
      LazPaintInstance.ShowError(rsSave,rsFileExtensionNotSupported);
      result := srException;
    end else
    begin
      try
        Image.SaveSelectionMaskToFileUTF8(filename);
        FSaveSelectionInitialFilename := filename;
        result := srOk;
        if Assigned(Scripting.RecordingFunctionParameters) then
        begin
          Scripting.RecordingFunctionParameters.AddString('FileName',filename);
          Scripting.RecordingFunctionParameters.AddBoolean('Validate',True);
        end;
        AVars.Strings['Result'] := filename;
      except
        on ex: Exception do
        begin
          LazPaintInstance.ShowError(RemoveTrail(FileSaveSelectionAs.Caption),ex.Message);
          result := srException;
        end;
      end;
    end;
  end else
    result := srCancelledByUser;
end;

function TFMain.ScriptFilter(AVars: TVariableSet): TScriptResult;
var filter: TPictureFilter;
  a: TContainedAction;
  defaultCaption: String;
begin
  filter := StrToPictureFilter(AVars.Strings['Name']);
  if filter = pfNone then
     result := srInvalidParameters
  else
  begin
    defaultCaption := '';

    a := ActionList1.ActionByName('Filter'+AVars.Strings['Name']);
    if Assigned(a) then
      defaultCaption := RemoveTrail((a as TAction).Caption)
    else
      defaultCaption := AVars.Strings['Name'];

    result := ExecuteFilter(LazPaintInstance, filter, AVars, false, defaultCaption);
  end;
end;

procedure TFMain.FileSaveSelectionAsUpdate(Sender: TObject);
begin
  FileSaveSelectionAs.Enabled := not Image.SelectionMaskEmpty;
end;

procedure TFMain.FormDropFiles(Sender: TObject; const FileNames: array of String);
var
  i,tx,ty: integer;
  Errors: String='';
  loadedLayers: array of record
     bmp: TBGRABitmap;
     orig: TBGRALayerCustomOriginal;
     filename: string;
  end;
  topmost: TTopMostInfo;
  choice: TModalResult;
  svgOrig: TBGRALayerSVGOriginal;
begin
  if Length(FileNames)<1 then exit;
  if Length(FileNames)= 1
     then TryOpenFileUTF8(FileNames[0])
  else
     begin
       {$IFNDEF LINUX}
       //Button values higher than 10 are used, in order to avoid Delphis icons on the buttons.
       choice := QuestionDlg (rsOpenMultipleImageFiles, rsMoreThanOneFile, mtConfirmation, [mrLast+1,rsOpenFilesAsLayers,mrLast+2,rsAddToImageList,mrLast+3,rsOpenFirstFileOnly,mrCancel,rsCancel ],'');
       {$ELSE}
       choice := mrLast+1;
       {$ENDIF}
       case choice of
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
                  loadedLayers[i].filename := Filenames[i];
                  case DetectFileFormat(Filenames[i]) of
                   ifSvg:
                     begin
                       svgOrig := LoadSVGOriginalUTF8(Filenames[i], Screen.Width, Screen.Height,
                         Screen.PixelsPerInch / 96 * CanvasScale);
                       loadedLayers[i].orig := svgOrig;
                       tx := max(tx, floor(svgOrig.Width + 0.95));
                       ty := max(ty, floor(svgOrig.Height + 0.95));
                     end
                   else
                     begin
                       loadedLayers[i].bmp := LoadFlatImageUTF8(Filenames[i]).bmp;
                       if loadedLayers[i].bmp.Width > tx then tx := loadedLayers[i].bmp.Width;
                       if loadedLayers[i].bmp.Height > ty then ty := loadedLayers[i].bmp.Height;
                     end;
                  end;
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
                  Image.CurrentFilenameUTF8 := '';
                  Image.Assign(TBGRABitmap.Create(tx,ty),true,false);
                  ZoomFitIfTooBig;
                  for i := 0 to high(loadedLayers) do
                  if Assigned(loadedLayers[i].bmp) then
                  begin
                    ImageAction.AddLayerFromBitmap(loadedLayers[i].bmp,ExtractFileName(loadedLayers[i].filename));
                    loadedLayers[i].bmp := nil;
                  end else
                  begin
                    ImageAction.AddLayerFromOriginal(loadedLayers[i].orig,ExtractFileName(loadedLayers[i].filename));
                    loadedLayers[i].orig := nil;
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
                QuestionDlg (rsError,rsFollowingErrorsOccurred+ LineEnding+ Errors, mtError,[11,rsOkay],'');
                LazPaintInstance.ShowTopmost(topmost);
              end;
              for i := 0 to high(loadedLayers) do
              begin
                FreeAndNil(loadedLayers[i].bmp);
                FreeAndNil(loadedLayers[i].orig);
              end;
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
  if Key = VK_MENU then altPressed:= false
  else if (Key = VK_SNAP) or (Key = VK_SNAP2) then snapPressed:= false
  else if Key = VK_SHIFT then shiftPressed:= false;
  if CatchToolKeyUp(Key) then
  begin
    PaintPictureLater;
  end else
  If Key = VK_SPACE then
  begin
    spacePressed:= false;
    if ToolManager.ToolSleeping and not spacePressed and
       ([ssLeft,ssRight] * FLayout.MouseButtonState = []) then
      ToolManager.ToolWakeUp;
    Key := 0;
  end;
end;

procedure TFMain.FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
var selectedTool: TPaintToolType;
  toolProcessKey: boolean;
begin
  try
    if Zoom.EditingZoom or EditingColors then exit;
    toolProcessKey:= true;
    if (CurrentTool in[ptText,ptEditShape]) and ((UTF8Key = #8) or ((length(UTF8Key)=1) and (UTF8Key[1] in['-','0'..'9']))) then
    begin
      if TextSpinEditFocused then
         toolProcessKey:= false;
    end;
    if toolProcessKey and CatchToolKeyPress(UTF8Key) then
    begin
      PaintPictureLater;
      UpdateToolbar;
    end else
    if UTF8Key <> '' then
    begin
      if UTF8UpperCase(UTF8Key) = 'X' then
      begin
        SwitchColors;
        UTF8Key:= '';
      end else
      if toolProcessKey and (UTF8Key = '+') then
      begin
         ViewZoomIn.Execute;
         UTF8Key := '';
      end else
      if toolProcessKey and (UTF8Key = '-') then
      begin
         ViewZoomOut.Execute;
         UTF8Key := '';
      end else
      if toolProcessKey and (UTF8Key = '*') then
      begin
         ViewZoomOriginal.Execute;
         UTF8Key := '';
      end else
      if toolProcessKey and (UTF8Key = '/') then
      begin
         ViewZoomFit.Execute;
         UTF8Key := '';
      end else
      begin
        selectedTool := ToolManager.GetCurrentToolType;
        FLayout.Menu.CycleTool(selectedTool, UTF8Key);
        if selectedTool <> ToolManager.GetCurrentToolType then ChooseTool(selectedTool);
      end;
    end;
  except
    on ex:exception do
      LazPaintInstance.ShowError('KeyPress',ex.Message);
  end;
end;

procedure TFMain.FormWindowStateChange(Sender: TObject);
begin
  {$IFDEF LINUX}
  if not FTopMostHiddenMinimised.defined and
    (self.WindowState = wsMinimized) then
  begin
    FTopMostHiddenMinimised := LazPaintInstance.HideTopmost;
  end else
  if FTopMostHiddenMinimised.defined and not (self.WindowState = wsMinimized) then
  begin
    LazPaintInstance.ShowTopmost(FTopMostHiddenMinimised);
    FTopMostHiddenMinimised.defined := false;
  end;
  {$ENDIF}
end;

procedure TFMain.ImageCropLayerUpdate(Sender: TObject);
begin
  ImageCropLayer.Enabled := not image.SelectionMaskEmpty;
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
    mr : TModalResult;
    topmostInfo: TTopMostInfo;
    layerLoaded:boolean;
    chosenFiles: array of string;
    loadedImage: TBGRABitmap;
begin
  if not image.SelectionLayerIsEmpty then
  begin
    LazPaintInstance.ShowMessage(rsLazPaint,rsMustReleaseSelection);
    exit;
  end;
  topmostInfo := LazPaintInstance.HideTopmost;
  chosenFiles := nil;
  loadedImage := nil;
  if UseImageBrowser then
  begin
    if not assigned(FBrowseImages) then
    begin
      FBrowseImages := TFBrowseImages.Create(self);
      FBrowseImages.LazPaintInstance := LazPaintInstance;
      FBrowseImages.Filter := OpenPictureDialog1.Filter;
      FBrowseImages.ShowRememberStartupDirectory := true;
    end;
    self.Hide;
    FBrowseImages.InitialDirectory:= FLoadInitialDir;
    FBrowseImages.RememberStartDirectory:= FLazPaintInstance.Config.DefaultRememberStartupSourceDirectory;
    FBrowseImages.AllowMultiSelect := true;
    FBrowseImages.OpenLayerIcon := true;
    FBrowseImages.FilterIndex:= OpenPictureDialog1.FilterIndex;
    try
      mr := FBrowseImages.ShowModal;
      LazPaintInstance.Config.SetRememberStartupSourceDirectory(FBrowseImages.RememberStartDirectory);
      if mr = mrOK then
      begin
        OpenPictureDialog1.FilterIndex := FBrowseImages.FilterIndex;
        setlength(chosenFiles, FBrowseImages.SelectedFileCount);
        for i := 0 to high(chosenFiles) do
          chosenFiles[i] := FBrowseImages.SelectedFile[i];
        loadedImage := FBrowseImages.GetChosenImage.bmp;
      end;
    except
      on ex: Exception do
      begin
        LazPaintInstance.ShowError(RemoveTrail(LayerFromFile.Hint),ex.Message);
      end;
    end;
    FBrowseImages.OpenLayerIcon := false;
    FBrowseImages.AllowMultiSelect := false;
    self.Show;
  end else
  begin
    OpenPictureDialog1.InitialDir:= FLoadInitialDir;
    OpenPictureDialog1.Options := OpenPictureDialog1.Options + [ofAllowMultiSelect];
    layerLoaded := false;
    if OpenPictureDialog1.Execute then
    begin
      setlength(chosenFiles, OpenPictureDialog1.Files.Count);
      for i := 0 to OpenPictureDialog1.Files.Count-1 do
        chosenFiles[i] := OpenPictureDialog1.Files[i];
    end;
    OpenPictureDialog1.Options := OpenPictureDialog1.Options - [ofAllowMultiSelect];
  end;
  if chosenFiles <> nil then
  begin
    if Assigned(loadedImage) and (length(chosenFiles)=1) then
    begin
      layerLoaded := length(ImageAction.TryAddLayerFromFile(chosenFiles[0], loadedImage)) > 0;
    end else
    begin
      FreeAndNil(loadedImage);
      for i := 0 to high(chosenFiles) do
        begin
          if length(ImageAction.TryAddLayerFromFile(chosenFiles[i])) > 0 then
            layerLoaded := true;
        end;
    end;
    FLoadInitialDir:= ExtractFilePath(chosenFiles[0]);
    if Config.DefaultRememberStartupSourceDirectory then
      Config.SetStartupSourceDirectory(FLoadInitialDir);
  end;

  LazPaintInstance.ShowTopmost(topmostInfo);
  if layerLoaded and not LazPaintInstance.LayerWindowVisible then
    LazPaintInstance.LayerWindowVisible := true;
end;

procedure TFMain.LayerMergeOverUpdate(Sender: TObject);
begin
  LayerMergeOver.Enabled := (image.CurrentLayerIndex > 0) and Image.CurrentLayerVisible;
end;

procedure TFMain.LayerMoveExecute(Sender: TObject);
begin
  ChooseTool(ptMoveLayer);
end;

procedure TFMain.LayerMoveUpdate(Sender: TObject);
begin
  LayerMove.Enabled := Image.CurrentLayerVisible and Image.SelectionMaskEmpty;
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
  LayerRotate.Enabled := Image.CurrentLayerVisible and Image.SelectionMaskEmpty;
end;

procedure TFMain.ItemDonateClick(Sender: TObject);
begin
  LazPaintInstance.Donate;
end;

procedure TFMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var topmostInfo: TTopMostInfo;
begin
  if ToolManager.ToolSleeping then
  begin
    CanClose := false;
    exit;
  end;
  if CurrentTool in [ptDeformation,ptLayerMapping] then
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
                LazPaintInstance.CancelRestart;
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
                LazPaintInstance.CancelRestart;
                LazPaintInstance.ShowTopmost(topmostInfo);
              end;
    end;
  end;
end;

procedure TFMain.FormHide(Sender: TObject);
begin
  FShouldArrange := false;
  FTopMostInfo := LazPaintInstance.HideTopmost;
  LazPaintInstance.SaveMainWindowPosition;
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
  ImageCrop.Enabled := not image.SelectionMaskEmpty;
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
     if Config.RecentFile[i]<>Image.CurrentFilenameUTF8 then
     begin
       item := NewItem(Config.RecentFile[i],0,false,true,@RecentFileClick,0,'');
       MenuRecentFiles.Add(item);
     end;
     MenuRecentFiles.Enabled := MenuRecentFiles.Count <> 0;

     EmptyMenu(MenuLanguage);
     currentLanguage := Config.DefaultLangage;
     for i := 0 to Config.Languages.Count-1 do
     begin
       item := NewItem(Config.Languages[i],0,false,true,@LanguageClick,0,'');
       if currentLanguage = item.Caption then
         item.Checked := true;
       MenuLanguage.Add(item);
     end;
     MenuLanguage.Enabled := MenuLanguage.Count <> 0;
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
    if Config.DefaultLangage <> language then
    begin
      Config.SetDefaultLangage(language);
      LazPaintInstance.Restart;
    end;
  end;
end;

procedure TFMain.TimerUpdateTimer(Sender: TObject);
begin
  TimerUpdate.Enabled := false;
  if ToolManager.ToolSleeping and not spacePressed and
     ([ssLeft,ssRight,ssMiddle] * FLayout.MouseButtonState = []) then
    ToolManager.ToolWakeUp;
  EditUndo.Update;
  EditRedo.Update;
  UpdateStatusText;
  if FCoordinatesCaptionCount > 0 then
  begin
    Label_Coordinates.Caption := FCoordinatesCaption;
    Label_Coordinates.Update;
    FCoordinatesCaptionCount := 0;
  end;
  FLayout.CheckDelayedUpdate;
  if Assigned(LazPaintInstance) then
  begin
    if DarkThemeInstance.HasSystemDarkThemeChanged then
    begin
      if (Config.GetDarkThemePreference = dtpAuto) and (LazPaintInstance.DarkTheme <> DarkThemeInstance.IsSystemDarkTheme) then
        LazPaintInstance.DarkTheme := DarkThemeInstance.IsSystemDarkTheme
        else LazPaintInstance.NotifyThemeChanged;
    end;
  end;
  TimerUpdate.Enabled := true;
end;

procedure TFMain.ToolRotateSelectionUpdate(Sender: TObject);
begin
  ToolRotateSelection.Enabled := not image.SelectionMaskEmpty;
end;

procedure TFMain.ToolLayerMappingUpdate(Sender: TObject);
begin
  ToolLayerMapping.Enabled := Image.CurrentLayerVisible and Image.SelectionMaskEmpty;
end;

procedure TFMain.ViewColorsExecute(Sender: TObject);
begin
  ToggleColorsVisible;
end;

procedure TFMain.ViewColorsUpdate(Sender: TObject);
begin
  ViewColors.Checked := LazPaintInstance.ChooseColorVisible;
end;

procedure TFMain.ViewDarkThemeExecute(Sender: TObject);
var
  newDarkTheme: Boolean;
begin
  newDarkTheme := not LazPaintInstance.DarkTheme;
  if newDarkTheme = DarkThemeInstance.IsSystemDarkTheme then
    Config.SetDarkThemePreference(dtpAuto) else
    begin
      if newDarkTheme then
        Config.SetDarkThemePreference(dtpDark)
        else Config.SetDarkThemePreference(dtpAuto);
    end;
  LazPaintInstance.DarkTheme := newDarkTheme;
end;

procedure TFMain.ViewDarkThemeUpdate(Sender: TObject);
begin
  ViewDarkTheme.Checked := LazPaintInstance.DarkTheme;
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

procedure TFMain.ViewPaletteExecute(Sender: TObject);
begin
  Layout.PaletteVisible := not Layout.PaletteVisible;
end;

procedure TFMain.ViewPaletteUpdate(Sender: TObject);
begin
  ViewPalette.Checked := Layout.PaletteVisible;
end;

procedure TFMain.ViewStatusBarExecute(Sender: TObject);
begin
  Layout.StatusBarVisible := not Layout.StatusBarVisible;
end;

procedure TFMain.ViewStatusBarUpdate(Sender: TObject);
begin
  ViewStatusBar.Checked := Layout.StatusBarVisible;
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
  Cursor := crHourGlass;
  try
    bmp := GetBitmapFromClipboard;
    Cursor := crDefault;
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
        Image.CurrentFilenameUTF8 := '';
        image.SetSavedFlag(0, -1, 0);
        result := srOk;
      end
       else
       begin
         bmp.Free;
         result := srException;
       end;
    end else
      result := srException;
  except on ex:exception do
    begin
      Cursor := crDefault;
      LazPaintInstance.ShowError(RemoveTrail(EditPasteAsNew.Caption), ex.Message);
      result := srException;
    end;
  end;
end;

function TFMain.ChooseTool(Tool: TPaintToolType): boolean;
var params: TVariableSet;
begin
  params := TVariableSet.Create('ChooseTool');
  params.AddString('Name', PaintToolTypeStr[Tool]);
  params.AddBoolean('FromGUI', true);
  result := Scripting.CallScriptFunction(params) = srOk;
  params.Free;
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

function TFMain.ScriptChooseTool(AVars: TVariableSet): TScriptResult;
var toolName: string;
  Tool, prevTool: TPaintToolType;
  topmostInfo: TTopMostInfo;
  useSelection: boolean;
  newTexture: TBGRABitmap;
  res: TQuestionResult;
  texMapBounds: TRect;
begin
  if ToolManager.ToolSleeping then exit;
  LazPaintInstance.ExitColorEditor;
  texMapBounds := EmptyRect;
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
    prevTool := ToolManager.GetCurrentToolType;
    try
      if not ((Tool in [ptMoveSelection,ptRotateSelection]) and
        (CurrentTool in [ptMoveSelection,ptRotateSelection])) then
        ToolManager.ToolCloseDontReopen;
      if self.Visible then
      begin
        case Tool of
          ptTextureMapping:
          begin
            useSelection:= false;
            newTexture := nil;
            if not image.SelectionMaskEmpty and (not image.SelectionLayerIsEmpty
              or not image.CurrentLayerEmpty) then
            begin
              topmostInfo := LazPaintInstance.HideTopmost;
              if Config.DefaultTransformSelectionAnswer <> mrNone then
                res := QuestionResult(Config.DefaultTransformSelectionAnswer)
              else
              begin
                res := ShowQuestionDialog(rsTextureMapping,rsTransformSelectionContent,[mbYes,mbNo],true);
                if res.Remember and (res.ButtonResult in [mrYes,mrNo]) then
                  Config.SetDefaultTransformSelectionAnswer(res.ButtonResult);
              end;
              LazPaintInstance.ShowTopmost(topmostInfo);
              case res.ButtonResult of
                mrYes: begin
                  useSelection:= true;
                  if image.SelectionLayerIsEmpty then
                    GetScriptContext.CallScriptFunction('EditSelectionFit', false);
                  if image.SelectionLayerReadonly <> nil then
                  begin
                    newTexture := image.SelectionLayerReadonly.Duplicate as TBGRABitmap;
                    newTexture.ApplyMask(image.SelectionMaskReadonly, image.SelectionLayerBounds);
                    if newTexture.Empty then
                    begin
                      newTexture.Free;
                      MessagePopup(rsNothingToBeRetrieved,2000);
                    end
                    else
                    begin
                      ImageAction.RemoveSelection;
                      texMapBounds := newTexture.GetImageBounds;
                      BGRAReplace(newTexture, newTexture.GetPart(texMapBounds));
                      ToolManager.BackFill.SetTexture(newTexture, AffineMatrixIdentity,
                        ToolManager.BackFill.TextureOpacity, ToolManager.BackFill.TextureRepetition);
                      newTexture.FreeReference;
                    end;
                  end;
                end;
              end;
            end;
            if (ToolManager.BackFill.Texture = nil) or
               ToolManager.BackFill.Texture.Empty then
            begin
              if useSelection then
              begin
                Tool := ptHand;
                result := srException;
              end else
              if not ShowOpenTextureDialog(ToolManager.BackFill) then
              begin
                Tool := ptHand;
                result := srCancelledByUser;
              end
              else
              if (ToolManager.BackFill.Texture = nil) or
                ToolManager.BackFill.Texture.Empty then
              begin
                Tool := ptHand;
                result := srException;
              end;
            end;
          end;
          ptLayerMapping:
          begin
            EditDeselect.Execute;
            if image.CurrentLayerEmpty then
            begin
              MessagePopup(rsEmptyLayer,2000);
              Tool := ptHand;
              result := srException;
            end;
          end;
          ptMoveLayer, ptRotateLayer, ptZoomLayer:
          begin
            if image.LayerOriginalDefined[image.CurrentLayerIndex] and
               image.LayerOriginalKnown[image.CurrentLayerIndex] and
               (image.LayerOriginal[image.CurrentLayerIndex]=nil) then
            begin
              Tool := ptHand;
              result := srException;
            end;

            if image.CurrentLayerEquals(BGRAPixelTransparent) and not
              (image.LayerOriginalDefined[image.CurrentLayerIndex] and
               image.LayerOriginalKnown[image.CurrentLayerIndex] and
               not image.LayerOriginal[image.CurrentLayerIndex].GetRenderBounds(
                 rect(-maxLongInt div 2,-maxLongInt div 2,maxLongInt div 2,maxLongInt div 2),
                 AffineMatrixIdentity).IsEmpty) then
            begin
              MessagePopup(rsEmptyLayer, 4000);
              Tool := ptHand;
              result := srException;
            end;
          end;
          ptDeformation:
          begin
            if (image.SelectionMaskEmpty and image.CurrentLayerEquals(image.CurrentLayerPixel[0,0])) or
               (not image.SelectionMaskEmpty and image.SelectionLayerIsEmpty) then
            begin
              LazPaintInstance.ShowMessage(rsLazPaint, rsNothingToBeDeformed);
              Tool := ptHand;
              result := srException;
            end;
          end;
          ptMoveSelection,ptRotateSelection,ptEditShape:
          begin
            if not ToolManager.SetCurrentToolType(Tool) then
            begin
              result := srException;
              exit;
            end;
            if AVars.Booleans['FromGUI'] and
               not (prevTool in [ptMoveSelection,ptRotateSelection,ptEditShape]) then
            begin
              if image.CurrentLayerVisible and not image.SelectionMaskEmpty and image.SelectionLayerIsEmpty and not image.CurrentLayerEmpty then
              begin
                topmostInfo := LazPaintInstance.HideTopmost;
                if Config.DefaultRetrieveSelectionAnswer <> mrNone then
                  res := QuestionResult(Config.DefaultRetrieveSelectionAnswer)
                else
                begin
                  res := ShowQuestionDialog(rsMovingOrRotatingSelection,rsRetrieveSelectedArea,[mbYes,mbNo],true);
                  if res.Remember and (res.ButtonResult in [mrYes,mrNo]) then
                    Config.SetDefaultRetrieveSelectionAnswer(res.ButtonResult);
                end;
                LazPaintInstance.ShowTopmost(topmostInfo);
                case res.ButtonResult of
                  mrYes:
                    begin
                      ToolManager.ToolCloseDontReopen;
                      ImageAction.RetrieveSelection;
                      ToolManager.ToolOpen;
                    end;
                end;
              end;
            end;
          end;
        end;
      end;
      ToolManager.SetCurrentToolType(Tool);
      if not texMapBounds.IsEmpty then
      begin
        ToolManager.PerspectiveOptions:= [];
        ToolManager.ToolDown(texMapBounds.Left-0.5, texMapBounds.Top-0.5, false, 1);
        ToolManager.ToolMove(texMapBounds.Right-0.5, texMapBounds.Bottom-0.5, 1);
        ToolManager.ToolUp;
      end;
      FLayout.FillSelectionHighlight := ToolManager.DisplayFilledSelection and not FShowSelectionNormal;
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

function TFMain.ScriptToolMouse(AVars: TVariableSet): TScriptResult;
var
  state, xy, pressure: TScriptVariableReference;
  nb, i: integer;
  ptF: TPointF;
  shiftState: TShiftState;
  needUpdate: Boolean;
  p: Double;
  keyCode: word;
begin
  state := AVars.GetVariable('State');
  xy := AVars.GetVariable('Coords');
  pressure := AVars.GetVariable('Pressure');
  if not TVariableSet.IsReferenceDefined(xy) or
     not TVariableSet.IsReferenceDefined(pressure) then
       exit(srInvalidParameters);
  nb := min(AVars.GetListCount(xy), AVars.GetListCount(pressure));
  if nb < 1 then
    exit(srInvalidParameters);
  shiftState := [];
  needUpdate := false;
  if TVariableSet.IsReferenceDefined(state) then
  begin
    for i := 0 to AVars.GetListCount(state)-1 do
      case AVars.GetStringAt(state, i) of
        'Left': include(shiftState, ssLeft);
        'Right': include(shiftState, ssRight);
        'Shift': include(shiftState, ssShift);
        'Ctrl': include(shiftState, ssSnap);
        'Alt': include(shiftState, ssAlt);
      end;
    if (ssLeft in shiftState) and (ssRight in ShiftState) then
      exit(srInvalidParameters);

    for i := 0 to AVars.GetListCount(state)-1 do
      case AVars.GetStringAt(state, i) of
        'Shift': begin keyCode := VK_SHIFT; if ToolManager.ToolKeyDown(keyCode) then needUpdate := true; end;
        'Ctrl': begin keyCode := VK_SNAP; if ToolManager.ToolKeyDown(keyCode) then needUpdate := true; end;
        'Alt': begin keyCode := VK_MENU; if ToolManager.ToolKeyDown(keyCode) then needUpdate := true; end;
      end;
  end;

  for i := 0 to nb-1 do
  begin
    ptF := AVars.GetPoint2DAt(xy, i);
    p := AVars.GetFloatAt(pressure, i);
    if ToolManager.ToolMove(ptF,p) then needUpdate := true;
    if (ssLeft in ShiftState) or (ssRight in shiftState) then
    begin
      if i = 0 then
        if ToolManager.ToolDown(ptF, ssRight in shiftState, p) then
          needUpdate := true;
      if i = nb-1 then
        if ToolManager.ToolUp then
          needUpdate := true;
    end;
  end;

  if TVariableSet.IsReferenceDefined(state) then
    for i := 0 to AVars.GetListCount(state)-1 do
      case AVars.GetStringAt(state, i) of
        'Shift': begin keyCode := VK_SHIFT; if ToolManager.ToolKeyUp(keyCode) then needUpdate := true; end;
        'Ctrl': begin keyCode := VK_SNAP; if ToolManager.ToolKeyUp(keyCode) then needUpdate := true; end;
        'Alt': begin keyCode := VK_MENU; if ToolManager.ToolKeyUp(keyCode) then needUpdate := true; end;
      end;

  if needUpdate then
  begin
    PaintPictureNow;
    UpdateToolbar;
  end;
  result := srOk;
end;

function TFMain.ScriptToolKeys(AVars: TVariableSet): TScriptResult;
var
  state, keys: TScriptVariableReference;
  i: Integer;
  needUpdate: Boolean;
  sk, foundSk: TSpecialKey;
  keyStr: String;
  keyCode: Word;
  shiftState: TShiftState;
  utf8Char: TUTF8Char;
begin
  state := AVars.GetVariable('State');
  keys := AVars.GetVariable('Keys');
  if not TVariableSet.IsReferenceDefined(keys) then
       exit(srInvalidParameters);

  result := srOk;
  needUpdate := false;

  shiftState := [];
  if TVariableSet.IsReferenceDefined(state) then
    for i := 0 to AVars.GetListCount(state)-1 do
      case AVars.GetStringAt(state, i) of
        'Shift': begin include(shiftState, ssShift); keyCode := VK_SHIFT; if ToolManager.ToolKeyDown(keyCode) then needUpdate := true; end;
        'Ctrl': begin include(shiftState, ssSnap); keyCode := VK_SNAP; if ToolManager.ToolKeyDown(keyCode) then needUpdate := true; end;
        'Alt': begin include(shiftState, ssAlt); keyCode := VK_MENU; if ToolManager.ToolKeyDown(keyCode) then needUpdate := true; end;
      end;
  for i := 0 to AVars.GetListCount(keys)-1 do
  begin
    keyStr := AVars.GetStringAt(keys, i);
    foundSk := skUnknown;
    for sk := low(TSpecialKey) to high(TSpecialKey) do
      if SpecialKeyStr[sk] = keyStr then
      begin
        foundSk := sk;
        break;
      end;
    if foundSk in[skUnknown, skShift,skCtrl,skAlt] then
    begin
      result := srInvalidParameters;
      break;
    end;
    keyCode := SpecialKeyToLCL[foundSk];
    if ToolManager.ToolKeyDown(keyCode) then needUpdate := true;
    if (keyCode <> 0) then
    begin
      utf8Char:= #0;
      if foundSk in [skNum0..skNum9] then utf8Char := chr(ord(foundSk)-ord(skNum0)+ord('0'))
      else if foundSk in [sk0..sk9] then utf8Char := chr(ord(foundSk)-ord(sk0)+ord('0'))
      else if foundSk in [skA..skZ] then
      begin
        if [ssShift,ssSnap,ssAlt]*shiftState = [] then utf8Char := chr(ord(foundSk)-ord(skA)+ord('a'))
        else if [ssShift,ssSnap,ssAlt]*shiftState = [sSShift] then utf8Char := chr(ord(foundSk)-ord(skA)+ord('A'));
      end
      else if (foundSk = skTab) and ([ssSnap,ssAlt]*shiftState = []) then utf8Char := #9
      else if (foundSk = skBackspace) and ([ssSnap,ssAlt]*shiftState = []) then utf8Char := #8;
      if (utf8Char <> #0) and ToolManager.ToolKeyPress(utf8Char) then needUpdate := true;
    end;
    keyCode := SpecialKeyToLCL[foundSk];
    if ToolManager.ToolKeyUp(keyCode) then needUpdate := true;
  end;
  if TVariableSet.IsReferenceDefined(state) then
    for i := 0 to AVars.GetListCount(state)-1 do
      case AVars.GetStringAt(state, i) of
        'Shift': begin keyCode := VK_SHIFT; if ToolManager.ToolKeyUp(keyCode) then needUpdate := true; end;
        'Ctrl': begin keyCode := VK_SNAP; if ToolManager.ToolKeyUp(keyCode) then needUpdate := true; end;
        'Alt': begin keyCode := VK_MENU; if ToolManager.ToolKeyUp(keyCode) then needUpdate := true; end;
      end;

  if needUpdate then
  begin
    PaintPictureNow;
    UpdateToolbar;
  end;
end;

function TFMain.ScriptToolWrite(AVars: TVariableSet): TScriptResult;
var
  txt: String;
  utf8char: TUTF8Char;
  p, pEnd: PChar;
  needUpdate: Boolean;
  utf8len: integer;
  keyCode,keyParam: word;
begin
  result := srOk;
  txt := AVars.Strings['Text'];
  if txt <> '' then
  begin
    needUpdate := false;
    p := @txt[1];
    pEnd := @txt[length(txt)];
    while p <= pEnd do
    begin
      utf8len := min(UTF8CharacterLength(p), (pEnd-p)+1);
      if utf8len = 0 then break;
      setlength(utf8char, utf8len);
      move(p^, utf8char[1], utf8len);
      if utf8char = #10 then keyCode := VK_RETURN
      else if utf8char = #8 then keyCode := VK_BACK
      else if utf8char = #9 then keyCode := VK_TAB
      else keyCode := 0;

      if keyCode<>0 then
      begin
        keyParam := keyCode;
        if ToolManager.ToolKeyDown(keyParam) then needUpdate:= true;
      end else keyParam := VK_UNDEFINED;
      if keyParam <> 0 then if ToolManager.ToolKeyPress(utf8char) then needUpdate:= true;
      if keyCode<>0 then
      begin
        keyParam := keyCode;
        if ToolManager.ToolKeyUp(keyParam) then needUpdate:= true;
      end;
      inc(p, utf8len);
    end;
    if needUpdate then
    begin
      PaintPictureNow;
      UpdateToolbar;
    end;
  end;
end;

function TFMain.ScriptViewZoomIn(AVars: TVariableSet): TScriptResult;
begin
  Zoom.ZoomIn(AVars.Booleans['Fine']);
  result := srOk;
end;

function TFMain.ScriptViewZoomOut(AVars: TVariableSet): TScriptResult;
begin
  Zoom.ZoomOut(AVars.Booleans['Fine']);
  result := srOk;
end;

function TFMain.ScriptViewZoomGet(AVars: TVariableSet): TScriptResult;
begin
  AVars.Floats['Result'] := Zoom.Factor;
  result := srOk;
end;

function TFMain.ScriptViewZoomSet(AVars: TVariableSet): TScriptResult;
begin
  Zoom.Factor := AVars.Floats['Factor'];
  result := srOk;
end;

function TFMain.ScriptViewZoomOriginal(AVars: TVariableSet): TScriptResult;
begin
  AVars.Floats['Factor'] := 1;
  result := GetScriptContext.CallScriptFunction('ViewZoomSet', true, AVars);
end;

function TFMain.ScriptViewZoomFit(AVars: TVariableSet): TScriptResult;
begin
  Image.ZoomFit;
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

function TFMain.ScriptViewGridGet(AVars: TVariableSet): TScriptResult;
begin
  AVars.Booleans['Result'] := LazPaintInstance.GridVisible;
  result := srOk;
end;

function TFMain.ScriptShowColorDialog(AVars: TVariableSet): TScriptResult;
begin
  if AVars.IsDefined('Color') then
    ColorDialog1.Color := AVars.Pixels['Color'].ToColor;
  if ColorDialog1.Execute then
    AVars.Pixels['Result'] := ColorToBGRA(ColorDialog1.Color)
  else
    AVars.Remove('Result');
  result := srOk;
end;

procedure TFMain.ThemeChanged(Sender: TObject);
begin
  DarkTheme := LazPaintInstance.DarkTheme;
end;

procedure TFMain.BrushCreateGeometricExecute(Sender: TObject);
var b: TLazPaintBrush;
begin
  b := ShowGeometricBrushDialog(LazPaintInstance);
  if Assigned(b) then ToolManager.AddBrush(b);
end;

procedure TFMain.BrushCreateGeometricUpdate(Sender: TObject);
begin
  BrushCreateGeometric.Enabled := ToolManager.BrushCount < 9;
end;

procedure TFMain.EditSelectionUpdate(Sender: TObject);
begin
  EditSelection.Enabled := not Scripting.Recording;
end;

procedure TFMain.EditShapeAlignBottomExecute(Sender: TObject);
begin
  ToolManager.ToolCommand(tcAlignBottom);
end;

procedure TFMain.EditShapeAlignBottomUpdate(Sender: TObject);
begin
  EditShapeAlignBottom.Enabled := ToolManager.ToolProvideCommand(tcAlignBottom);
end;

procedure TFMain.EditShapeAlignLeftExecute(Sender: TObject);
begin
  ToolManager.ToolCommand(tcAlignLeft);
end;

procedure TFMain.EditShapeAlignLeftUpdate(Sender: TObject);
begin
  EditShapeAlignLeft.Enabled := ToolManager.ToolProvideCommand(tcAlignLeft);
end;

procedure TFMain.EditShapeAlignRightExecute(Sender: TObject);
begin
  ToolManager.ToolCommand(tcAlignRight);
end;

procedure TFMain.EditShapeAlignRightUpdate(Sender: TObject);
begin
  EditShapeAlignRight.Enabled := ToolManager.ToolProvideCommand(tcAlignRight);
end;

procedure TFMain.EditShapeAlignTopExecute(Sender: TObject);
begin
  ToolManager.ToolCommand(tcAlignTop);
end;

procedure TFMain.EditShapeAlignTopUpdate(Sender: TObject);
begin
  EditShapeAlignTop.Enabled := ToolManager.ToolProvideCommand(tcAlignTop);
end;

procedure TFMain.EditShapeCenterHorizontallyExecute(Sender: TObject);
begin
  ToolManager.ToolCommand(tcCenterHorizontally);
end;

procedure TFMain.EditShapeCenterHorizontallyUpdate(Sender: TObject);
begin
  EditShapeCenterHorizontally.Enabled := ToolManager.ToolProvideCommand(tcCenterHorizontally);
end;

procedure TFMain.EditShapeCenterVerticallyExecute(Sender: TObject);
begin
  ToolManager.ToolCommand(tcCenterVertically);
end;

procedure TFMain.EditShapeCenterVerticallyUpdate(Sender: TObject);
begin
  EditShapeCenterVertically.Enabled := ToolManager.ToolProvideCommand(tcCenterVertically);
end;

procedure TFMain.EditShapeToCurveExecute(Sender: TObject);
begin
  if ToolManager.CurrentTool is TVectorialTool then ChooseTool(ptEditShape);
  ToolManager.ToolCommand(tcShapeToSpline);
end;

procedure TFMain.EditShapeToCurveUpdate(Sender: TObject);
begin
  EditShapeToCurve.Enabled := ToolManager.ToolProvideCommand(tcShapeToSpline);
end;

procedure TFMain.FileChooseEntryExecute(Sender: TObject);
var
  openParams: TVariableSet;
begin
  openParams := TVariableSet.Create('FileChooseEntry');
  Scripting.CallScriptFunction(openParams);
  openParams.Free;
end;

procedure TFMain.FileChooseEntryUpdate(Sender: TObject);
begin
  FileChooseEntry.Enabled := Image.IsIconCursor or Image.IsTiff or Image.IsGif;
end;

procedure TFMain.FileExportExecute(Sender: TObject);
var
  params: TVariableSet;
begin
  params := TVariableSet.Create('FileSaveAs');
  params.Booleans['Export'] := true;
  Scripting.CallScriptFunction(params);
  params.Free;
end;

procedure TFMain.EditCopyExecute(Sender: TObject);
begin
  if not ToolManager.ToolCommand(tcCopy) then
    Scripting.CallScriptFunction('EditCopy');
end;

procedure TFMain.EditCopyUpdate(Sender: TObject);
begin
  EditCopy.Enabled := ToolManager.ToolProvideCommand(tcCopy) or not image.SelectionMaskEmpty;
end;

procedure TFMain.EditCutExecute(Sender: TObject);
begin
  if not ToolManager.ToolCommand(tcCut) then
    Scripting.CallScriptFunction('EditCut');
end;

procedure TFMain.EditCutUpdate(Sender: TObject);
begin
  EditCut.Enabled := ToolManager.ToolProvideCommand(tcCut) or
    (not image.SelectionMaskEmpty and
       (not image.SelectionLayerIsEmpty or image.CurrentLayerVisible));
end;

procedure TFMain.EditDeleteSelectionExecute(Sender: TObject);
begin
  if not ToolManager.ToolCommand(tcDelete) then
    Scripting.CallScriptFunction('EditDeleteSelection');
end;

procedure TFMain.EditDeleteSelectionUpdate(Sender: TObject);
begin
  EditDeleteSelection.Enabled := ToolManager.ToolProvideCommand(tcDelete) or not image.SelectionMaskEmpty;
end;

procedure TFMain.EditMoveDownExecute(Sender: TObject);
begin
  if ToolManager.CurrentTool is TVectorialTool then ChooseTool(ptEditShape);
  ToolManager.ToolCommand(tcMoveDown);
end;

procedure TFMain.EditMoveDownUpdate(Sender: TObject);
begin
  EditMoveDown.Enabled := ToolManager.ToolProvideCommand(tcMoveDown);
end;

procedure TFMain.EditMoveToBackExecute(Sender: TObject);
begin
  if ToolManager.CurrentTool is TVectorialTool then ChooseTool(ptEditShape);
  ToolManager.ToolCommand(tcMoveToBack);
end;

procedure TFMain.EditMoveToBackUpdate(Sender: TObject);
begin
  EditMoveToBack.Enabled := ToolManager.ToolProvideCommand(tcMoveToBack);
end;

procedure TFMain.EditMoveToFrontExecute(Sender: TObject);
begin
  ToolManager.ToolCommand(tcMoveToFront);
end;

procedure TFMain.EditMoveToFrontUpdate(Sender: TObject);
begin
  EditMoveToFront.Enabled := ToolManager.ToolProvideCommand(tcMoveToFront);
end;

procedure TFMain.EditMoveUpExecute(Sender: TObject);
begin
  ToolManager.ToolCommand(tcMoveUp);
end;

procedure TFMain.EditMoveUpUpdate(Sender: TObject);
begin
  EditMoveUp.Enabled := ToolManager.ToolProvideCommand(tcMoveUp);
end;

procedure TFMain.EditPasteExecute(Sender: TObject);
begin
  if not ToolManager.ToolCommand(tcPaste) then
    Scripting.CallScriptFunction('EditPaste');
end;

procedure TFMain.EditSelectionFitUpdate(Sender: TObject);
begin
  EditSelectionFit.Enabled:= Image.CurrentLayerVisible;
end;

procedure TFMain.FileImport3DUpdate(Sender: TObject);
begin
  FileImport3d.Enabled := not Scripting.Recording
end;

procedure TFMain.FilePrintExecute(Sender: TObject);
begin
  LazPaintInstance.ShowPrintDlg;
end;

procedure TFMain.FileRememberSaveFormatExecute(Sender: TObject);
begin
  FileRememberSaveFormat.Checked := not FileRememberSaveFormat.Checked;
  Config.SetRememberSaveFormat(FileRememberSaveFormat.Checked);
end;

procedure TFMain.FileRunScriptExecute(Sender: TObject);
var
  dlg: TOpenDialog;
  tmi: TTopMostInfo;
  title: String;
  posSub: Integer;
begin
  tmi := LazPaintInstance.HideTopmost;
  try
    dlg := TOpenDialog.Create(nil);
    try
      dlg.Title := FileRunScript.Caption;
      dlg.InitialDir:= Config.DefaultScriptDirectory;
      dlg.DefaultExt:= 'py';
      dlg.Filter:= 'Python (*.py)|*.py';
      if dlg.Execute then
      begin
        Config.SetDefaultScriptDirectory(ExtractFilePath(dlg.FileName));
        title := GetScriptTitle(dlg.FileName);
        posSub := title.LastIndexOf('>');
        if posSub <> -1 then title := title.Substring(posSub+1);
        LazPaintInstance.RunScript(dlg.FileName, title);
      end;
    except
      on ex:exception do
        LazPaintInstance.ShowError(RemoveTrail(FileRunScript.Caption), ex.Message);
    end;
    dlg.Free;
  finally
    LazPaintInstance.ShowTopmost(tmi);
  end;
end;

procedure TFMain.FileSaveAsInSameFolderExecute(Sender: TObject);
var dir: string;
begin
  dir := ExtractFilePath(Image.CurrentFilenameUTF8);
  if dir <> '' then FSaveInitialDir := dir;
  Scripting.CallScriptFunction('FileSaveAs');
end;

procedure TFMain.FileSaveAsInSameFolderUpdate(Sender: TObject);
begin
  FileSaveAsInSameFolder.Enabled := ExtractFilePath(Image.CurrentFilenameUTF8)<>'';
end;

procedure TFMain.FileUseImageBrowserExecute(Sender: TObject);
begin
  Config.SetDefaultUseImageBrowser(not UseImageBrowser);
end;

procedure TFMain.FileUseImageBrowserUpdate(Sender: TObject);
begin
  FileUseImageBrowser.Checked := UseImageBrowser;
end;

procedure TFMain.ForgetDialogAnswersExecute(Sender: TObject);
var
  m: TToolPopupMessage;
begin
  Config.SetDefaultTransformSelectionAnswer(0);
  Config.SetDefaultRetrieveSelectionAnswer(0);
  for m := low(TToolPopupMessage) to high(TToolPopupMessage) do
    Config.SetToolPopupMessageShownCount(ord(m), 0);
end;

procedure TFMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Image.OnCurrentFilenameChanged := nil;
  Image.OnImageChanged.RemoveObserver(@OnImageChangedHandler);
  Image.OnQueryExitToolHandler := nil;
  Image.Zoom := nil;
end;

procedure TFMain.ItemDockLayersAndColorsClick(Sender: TObject);
begin
  LazPaintInstance.DockLayersAndColors := not LazPaintInstance.DockLayersAndColors;
end;

procedure TFMain.ItemFullscreenClick(Sender: TObject);
begin
  LazPaintInstance.Fullscreen := not LazPaintInstance.Fullscreen;
end;

procedure TFMain.ItemViewDockToolboxClick(Sender: TObject);
begin
  if Layout.ToolBoxDocking in[twWindow,twNone] then
    Layout.ToolBoxDocking := twLeft
  else
    Layout.ToolBoxDocking := twWindow;
end;

procedure TFMain.LayerExportExecute(Sender: TObject);
var
  topMost: TTopMostInfo;
  saveDlg: TSaveDialog;
  layerIdx, mr: Integer;
  defaultExt, filename: String;

  procedure DoExportLayer(AFilename: string);
  var
    vars: TVariableSet;
  begin
    FExportInitialDir := extractFilePath(AFilename);
    if Config.DefaultRememberStartupExportDirectory then
      Config.SetStartupExportDirectory(FExportInitialDir);

    vars := TVariableSet.Create('LayerSaveAs');
    vars.Strings['FileName'] := AFilename;
    CallScriptFunction(vars);
    vars.Free;
  end;

begin
  layerIdx := Image.CurrentLayerIndex;
  if not Image.LayerOriginalDefined[layerIdx] then
    defaultExt := '.png' else
  if Image.LayerOriginalClass[layerIdx] = TBGRALayerSVGOriginal then
    defaultExt := '.svg'
  else
    defaultExt := '.lzp';
  filename := FileManager.GetValidFilename(Image.LayerName[layerIdx]) + defaultExt;
  topMost := LazPaintInstance.HideTopmost;
  saveDlg := ExportPictureDialog;
  if UseImageBrowser then
  begin
    if not assigned(FSaveImage) then
    begin
      FSaveImage := TFBrowseImages.Create(self);
      FSaveImage.LazPaintInstance := LazPaintInstance;
      FSaveImage.IsSaveDialog := true;
      FSaveImage.ShowRememberStartupDirectory:= true;
      if Config.DefaultRememberSaveFormat then
        FSaveImage.DefaultExtensions:= Config.DefaultSaveExtensions;
    end;
    FSaveImage.Filter := saveDlg.Filter;
    FSaveImage.Caption := saveDlg.Title;
    FSaveImage.InitialFilename := filename;
    FSaveImage.DefaultExtension := defaultExt;
    FSaveImage.InitialDirectory:= FExportInitialDir;
    FSaveImage.RememberStartDirectory:= FLazPaintInstance.Config.DefaultRememberStartupExportDirectory;
    FSaveImage.FilterIndex := saveDlg.FilterIndex;
    mr := FSaveImage.ShowModal;
    LazPaintInstance.Config.SetRememberStartupExportDirectory(FSaveImage.RememberStartDirectory);
    if mr = mrOK then
    begin
      saveDlg.FilterIndex := FSaveImage.FilterIndex;
      DoExportLayer(FSaveImage.FileName);
    end;
  end else
  begin
    saveDlg.FileName := filename;
    saveDlg.DefaultExt := defaultExt;
    saveDlg.InitialDir:= FExportInitialDir;
    if saveDlg.Execute then
    begin
      DoExportLayer(saveDlg.FileName);
    end;
  end;
  LazPaintInstance.ShowTopmost(topMost);
end;

procedure TFMain.LayerRasterizeUpdate(Sender: TObject);
begin
  LayerRasterize.Enabled := Image.LayerOriginalDefined[Image.CurrentLayerIndex];
end;

procedure TFMain.LayerZoomExecute(Sender: TObject);
begin
  ChooseTool(ptZoomLayer);
end;

procedure TFMain.LayerZoomUpdate(Sender: TObject);
begin
  LayerZoom.Enabled := Image.CurrentLayerVisible and Image.SelectionMaskEmpty;
end;

procedure TFMain.MenuCoordinatesToolbarClick(Sender: TObject);
begin
  Panel_Coordinates.Visible := not Panel_Coordinates.Visible;
  Config.SetDefaultCoordinatesToolbarVisible(Panel_Coordinates.Visible);
  QueryArrange;
end;

procedure TFMain.MenuCopyPasteToolbarClick(Sender: TObject);
begin
  Panel_CopyPaste.Visible := not Panel_CopyPaste.Visible;
  Config.SetDefaultCopyPasteToolbarVisible(Panel_CopyPaste.Visible);
  QueryArrange;
end;

procedure TFMain.MenuDockToolboxLeftClick(Sender: TObject);
begin
  Layout.ToolBoxDocking := twLeft;
end;

procedure TFMain.MenuDockToolboxRightClick(Sender: TObject);
begin
  Layout.ToolBoxDocking := twRight;
end;

procedure TFMain.MenuFileToolbarClick(Sender: TObject);
begin
  Config.SetDefaultFileToolbarVisible(not Config.DefaultFileToolbarVisible);
  Panel_File.Visible := Config.DefaultFileToolbarVisible and not LazPaintInstance.Embedded;
  QueryArrange;
end;

procedure TFMain.MenuShowPaletteClick(Sender: TObject);
begin
  Layout.PaletteVisible := not Layout.PaletteVisible;
end;

procedure TFMain.MenuUndockToolboxClick(Sender: TObject);
begin
  Layout.ToolBoxDocking := twWindow;
end;

procedure TFMain.MenuUndoRedoToolbarClick(Sender: TObject);
begin
  Panel_Undo.Visible := not Panel_Undo.Visible;
  Config.SetDefaultUndoRedoToolbarVisible(Panel_Undo.Visible);
  QueryArrange;
end;

procedure TFMain.MenuViewClick(Sender: TObject);
begin
  ItemViewDockToolbox.Checked := (Layout.ToolBoxDocking <> twWindow) and (Layout.ToolBoxDocking <> twNone);
  ItemDockLayersAndColors.Checked := LazPaintInstance.DockLayersAndColors;
  ItemFullscreen.Checked := LazPaintInstance.Fullscreen;
  {$IF defined(LINUX) or defined(DARWIN)}
  ItemFullscreen.Visible := false;
  {$ENDIF}
end;

procedure TFMain.MenuZoomToolbarClick(Sender: TObject);
begin
  Panel_Zoom.Visible := not Panel_Zoom.Visible;
  Config.SetDefaultZoomToolbarVisible(Panel_Zoom.Visible);
  QueryArrange;
end;

procedure TFMain.EditPasteUpdate(Sender: TObject);
begin
  if Assigned(LazPaintInstance) and LazPaintInstance.ColorEditorActive then
    EditPaste.Enabled := false
    else EditPaste.Enabled := ToolManager.ToolProvideCommand(tcPaste) or Image.CurrentLayerVisible;
end;

procedure TFMain.EditDeselectUpdate(Sender: TObject);
begin
  EditDeselect.Enabled := not image.SelectionMaskEmpty;
  if image.SelectionMaskEmpty then FSaveSelectionInitialFilename := '';
end;

procedure TFMain.EditRedoUpdate(Sender: TObject);
begin
  EditRedo.Enabled := image.CanRedo;
end;

procedure TFMain.EditSelectionExecute(Sender: TObject);
begin
  LazPaintInstance.EditSelection;
end;

procedure TFMain.ScriptExecute(Sender: TObject);
var actionName: string;
begin
  if Assigned(LazPaintInstance) then
    LazPaintInstance.ExitColorEditor;
  if Sender is TAction then
  begin
    actionName := (Sender as TAction).Name;
    CallScriptFunction(actionName);
  end;
end;

procedure TFMain.AskMergeSelection(ACaption: string);
var topmostInfo: TTopMostInfo; res: integer;
begin
  if not image.SelectionMaskEmpty and not image.SelectionLayerIsEmpty then
  begin
    topmostInfo:= LazPaintInstance.HideTopmost;
    res := MessageDlg(ACaption,rsMergeSelection,mtConfirmation,[mbYes,mbNo],0);
    LazPaintInstance.ShowTopmost(topmostInfo);
    case res of
      mrYes: EditDeselect.Execute;
    end;
  end;
end;

procedure TFMain.UpdateSpecialKeys(Shift: TShiftState);
  procedure UpdateKey(AShift: TShiftStateEnum; ACode: Word; var APressed: boolean);
  begin
    if (AShift in Shift) and not APressed then
    begin
      if ToolManager.ToolKeyDown(ACode) then PaintPictureNow;
      APressed:= true;
    end else
    if not (AShift in Shift) and APressed then
    begin
      if ToolManager.ToolKeyUp(ACode) then PaintPictureNow;
      APressed:= false;
    end;
  end;
begin
  {$IFDEF DARWIN}
  UpdateKey(ssSnap, VK_SNAP, snapPressed);
  UpdateKey(ssAlt, VK_MENU, altPressed);
  UpdateKey(ssShift, VK_SHIFT, shiftPressed);
  {$ENDIF}
end;

function TFMain.ShowOpenTextureDialog(ATargetFill: TVectorialFill): boolean;
var newTex: TBGRABitmap;
  texFilename: string;
  topMostInfo: TTopMostInfo;
begin
  result := false;
  newTex := nil;
  topMostInfo := LazPaintInstance.HideTopmost;
  try
    texFilename := '';
    if UseImageBrowser then
    begin
      if not assigned(FBrowseTextures) then
      begin
        FBrowseTextures := TFBrowseImages.Create(self);
        FBrowseTextures.LazPaintInstance := LazPaintInstance;
        FBrowseTextures.Filter := OpenTextureDialog.Filter;
        FBrowseTextures.AllowMultiSelect := false;
        FBrowseTextures.Caption := OpenTextureDialog.Title;
      end;
      self.Hide;
      try
        FBrowseTextures.InitialDirectory := Config.DefaultTextureDirectory;
        FBrowseTextures.FilterIndex:= OpenTextureDialog.FilterIndex;
        if FBrowseTextures.ShowModal = mrOK then
        begin
          OpenTextureDialog.FilterIndex := FBrowseTextures.FilterIndex;
          texFilename := FBrowseTextures.Filename;
          newTex := FBrowseTextures.GetChosenImage.bmp;
        end;
      finally
        self.Show;
      end;
    end else
    begin
      OpenTextureDialog.InitialDir := Config.DefaultTextureDirectory;
      if OpenTextureDialog.Execute then
        texFilename:= OpenTextureDialog.FileName;
    end;
    if texFilename <> '' then
    begin
      LazPaintInstance.StartLoadingImage(texFilename);
      try
        if not Assigned(newTex) then
          newTex := LoadFlatImageUTF8(texFilename).bmp;
        if LazPaintInstance.BlackAndWhite then
          newTex.InplaceGrayscale;
        ATargetFill.SetTexture(newTex, AffineMatrixIdentity,
          ATargetFill.TextureOpacity, ATargetFill.TextureRepetition);
        newTex.FreeReference;
        newTex := nil;
        result := true;
        Config.SetDefaultTextureDirectory(ExtractFilePath(texFilename));
      except
        on ex:Exception do
        begin
          LazPaintInstance.EndLoadingImage;
          LazPaintInstance.ShowError(rsOpen,ex.Message);
        end;
      end;
      LazPaintInstance.EndLoadingImage;
    end;
  except
    on ex:Exception do
      LazPaintInstance.ShowError(rsOpen,ex.Message);
  end;
  FreeAndNil(newTex);
  LazPaintInstance.ShowTopmost(topMostInfo);
end;

function TFMain.ShowOpenBrushDialog: boolean;
var newBrushBmp: TBGRABitmap;
  newBrush: TLazPaintBrush;
  brushFilename: string;
  topMostInfo: TTopMostInfo;
begin
  result := false;
  topMostInfo := LazPaintInstance.HideTopmost;
  newBrushBmp := nil;
  try
    brushFilename := '';
    if UseImageBrowser then
    begin
      if not assigned(FBrowseBrushes) then
      begin
        FBrowseBrushes := TFBrowseImages.Create(self);
        FBrowseBrushes.LazPaintInstance := LazPaintInstance;
        FBrowseBrushes.Filter := OpenBrushDialog.Filter;
        FBrowseBrushes.AllowMultiSelect := false;
        FBrowseBrushes.Caption := OpenBrushDialog.Title;
      end;
      FBrowseBrushes.InitialDirectory := Config.DefaultBrushDirectory;
      FBrowseBrushes.FilterIndex:= OpenBrushDialog.FilterIndex;
      if FBrowseBrushes.ShowModal = mrOK then
      begin
        OpenBrushDialog.FilterIndex := FBrowseBrushes.FilterIndex;
        brushFilename := FBrowseBrushes.Filename;
        newBrushBmp := FBrowseBrushes.GetChosenImage.bmp;
      end;
    end else
    begin
      OpenBrushDialog.InitialDir := Config.DefaultTextureDirectory;
      if OpenBrushDialog.Execute then
        brushFilename:= OpenBrushDialog.FileName;
    end;
    if brushFilename <> '' then
    begin
      LazPaintInstance.StartLoadingImage(brushFilename);
      try
        if not assigned(newBrushBmp) then
          newBrushBmp := LoadFlatImageUTF8(brushFilename).bmp;
        newBrush := TLazPaintBrush.Create;
        newBrush.AssignBrushImage(newBrushBmp);
        FreeAndNil(newBrushBmp);
        ToolManager.AddBrush(newBrush);
        result := true;
        Config.SetDefaultBrushDirectory(ExtractFilePath(brushFilename));
      except
        on ex:Exception do
        begin
          LazPaintInstance.EndLoadingImage;
          LazPaintInstance.ShowError(rsOpen,ex.Message);
        end;
      end;
      LazPaintInstance.EndLoadingImage;
    end;
  except
    on ex:Exception do
      LazPaintInstance.ShowError(rsOpen,ex.Message);
  end;
  FreeAndNil(newBrushBmp);
  LazPaintInstance.ShowTopmost(topMostInfo);
end;

procedure TFMain.ShowNoPicture;
begin
  if Assigned(FLayout) then
    FLayout.ShowNoPicture;
end;

procedure TFMain.UpdateWindowCaption;
var bppStr: string;
begin
  if Image.IsTiff or Image.IsGif then
  begin
    if Image.FrameIndex = TImageEntry.NewFrameIndex then
      bppStr := ' '+rsNewImage
    else
      bppStr := ' #'+inttostr(Image.FrameIndex+1);
  end else
    if Image.bpp = 0 then
      bppStr := ''
    else
      bppStr := ' '+inttostr(Image.bpp)+'bit';

  if Image.CurrentFilenameUTF8 = '' then
    self.Caption := inttostr(Image.Width)+'x'+inttostr(Image.Height) + bppStr + ' - ' + LazPaintInstance.Title
  else
    self.Caption := inttostr(Image.Width)+'x'+inttostr(Image.Height) + bppStr + ' - ' + image.CurrentFilenameUTF8;
end;

procedure TFMain.ImageCurrentFilenameChanged(sender: TLazPaintImage);
begin
  UpdateWindowCaption;
end;

procedure TFMain.AddDockedControl(AControl: TControl);
begin
  if Assigned(FLayout) then
  begin
    FLayout.AddDockedControl(AControl);
    QueryArrange;
  end;
end;

procedure TFMain.RemoveDockedControl(AControl: TControl);
begin
  if Assigned(FLayout) then
  begin
    FLayout.RemoveDockedControl(AControl);
    QueryArrange;
  end;
end;

function TFMain.GetCurrentTool: TPaintToolType;
begin
  result := ToolManager.GetCurrentToolType;
end;

procedure TFMain.ManagerToolChanged(sender: TToolManager; ANewTool: TPaintToolType);
begin
  if self.Visible then
  begin
    PaintBox_PenPreview.Invalidate;
    Image.OnImageChanged.NotifyObservers;
    UpdateToolImage;
    UpdatePenWidthToolbar;
    UpdateCurveModeToolbar;
  end;
end;

procedure TFMain.OnQueryExitToolHandler(sender: TLazPaintImage);
begin
  if ToolManager.ToolSleeping then exit;
  ChooseTool(ptHand);
end;

procedure TFMain.ZoomFitIfTooBig;
begin
  if Assigned(Zoom) then
  begin
    with FLayout.WorkArea do
      if (image.Width*Zoom.Factor > right-left) or (image.Height*Zoom.Factor > bottom-top) then
        ViewZoomFit.Execute;
  end;
end;

function TFMain.TryOpenFileUTF8(filenameUTF8: string; AddToRecent: Boolean;
     ALoadedImage: PImageEntry; ASkipDialogIfSingleImage: boolean;
     AAllowDuplicate: boolean; AEntryToLoad: integer): Boolean;
var
  picture: TImageEntry;
  format: TBGRAImageFormat;
  dupIndex: Integer;

  procedure StartImport(AFilename: string);
  begin
    LazPaintInstance.StartLoadingImage(AFilename);
    ToolManager.ToolCloseDontReopen;
    if (CurrentTool in [ptDeformation,ptRotateSelection,ptMoveSelection,ptLayerMapping])
     or ((CurrentTool = ptHotSpot) and (format <> ifCur)) then
      ChooseTool(ptHand);
    ShowNoPicture;
    Image.OnImageChanged.NotifyObservers;
  end;
  procedure EndImport(ABPP: integer = 0; AFrameIndex: integer = 0; AFrameCount: integer = 1; AIsNewFrame: boolean = false);
  begin
    LazPaintInstance.EndLoadingImage;
    if AddToRecent then
    begin
      Config.AddRecentFile(filenameUTF8);
      Config.AddRecentDirectory(ExtractFilePath(filenameUTF8));
    end;
    Image.CurrentFilenameUTF8 := filenameUTF8;
    image.ClearUndo;
    if AIsNewFrame then
    begin
      Image.BPP:= ABPP;
      Image.FrameIndex:= AFrameIndex;
      Image.FrameCount:= AFrameCount;
    end else
      image.SetSavedFlag(ABPP, AFrameIndex, AFrameCount, True);
    ToolManager.ToolOpen;
    ZoomFitIfTooBig;
    ToolHotSpotUpdate(nil);
    result := true;
  end;
  procedure ImportPicture(AIsNewFrame: boolean = false);
  begin
    if (picture.bmp <> nil) and (picture.bmp.Width > 0) and (picture.bmp.Height > 0) then
    begin
      if AIsNewFrame then
        StartImport('<'+rsNewImage+'>')
      else
        StartImport(filenameUTF8);
      with ComputeAcceptableImageSize(picture.bmp.Width,picture.bmp.Height) do
        if (cx < picture.bmp.Width) or (cy < picture.bmp.Height) then
        begin
          MessagePopupForever(rsResamplingImage);
          LazPaintInstance.UpdateWindows;
          BGRAReplace(picture.bmp, picture.bmp.Resample(cx,cy,rmFineResample));
          MessagePopupHide
        end;
      image.Assign(picture.bmp,True, false);
      picture.bmp := nil;
      EndImport(picture.bpp, picture.frameIndex, picture.frameCount, AIsNewFrame);
    end else FreeAndNil(picture.bmp);
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
  picture := TImageEntry.Empty;
  try
    format := Image.DetectImageFormat(filenameUTF8);
    if (format <> ifSvg) and Assigned(ALoadedImage) and Assigned(ALoadedImage^.bmp) then
    begin
      picture := ALoadedImage^;
      ALoadedImage^.bmp := nil;
      ImportPicture;
    end
    else
    if IsRawFilename(filenameUTF8) then
    begin
      picture.bmp := GetRawFileImage(filenameUTF8);
      picture.bpp := 0;
      picture.frameIndex:= 0;
      picture.frameCount:= 1;
      ImportPicture;
    end else
    if format in[ifIco,ifCur] then
    begin
      picture := ShowPreviewDialog(LazPaintInstance, FilenameUTF8, rsIconOrCursor, ASkipDialogIfSingleImage);
      ImportPicture;
    end
    else
    if format in[ifIco,ifGif,ifTiff] then
    begin
      if AEntryToLoad <> -1 then
        picture := LoadFlatImageUTF8(FilenameUTF8, AEntryToLoad) else
      begin
        if (format in[ifGif,ifTiff]) and AAllowDuplicate and (Image.FrameIndex <> -1) then
          dupIndex := Image.FrameIndex else dupIndex := -1;
        picture := ShowPreviewDialog(LazPaintInstance, FilenameUTF8,
          GetImageFormatName(format),ASkipDialogIfSingleImage, dupIndex);
      end;
      if picture.isDuplicate then
      begin
        picture.FreeAndNil;
        Image.FrameIndex:= picture.frameIndex;
        Image.FrameCount:= picture.frameCount;
        Image.OnImageChanged.NotifyObservers;
      end
      else
        ImportPicture(picture.frameIndex = picture.NewFrameIndex);
    end
    else
    begin
      StartImport(filenameUTF8);
      image.LoadFromFileUTF8(filenameUTF8);
      EndImport;
    end;

  except
    on ex: Exception do
    begin
      picture.FreeAndNil;
      ToolManager.ToolOpen;
      Image.OnImageChanged.NotifyObservers;
      LazPaintInstance.ShowError(rsOpen,ex.Message);
    end;
  end;
end;

procedure TFMain.ToolMoveSelectionUpdate(Sender: TObject);
begin
  ToolMoveSelection.Enabled := not image.SelectionMaskEmpty;
end;

{****************************** Picture ************************}

procedure TFMain.OnImageChangedHandler(AEvent: TLazPaintImageObservationEvent);
begin
  if (image.Width <> FLastWidth) or (image.Height <> FLastHeight)
   or (image.BPP <> FLastBPP) or (image.FrameIndex <> FLastFrameIndex) then
  begin
    FLastWidth:= image.Width;
    FLastHeight:= image.Height;
    FLastBPP := image.BPP;
    FLastFrameIndex:= image.FrameIndex;
    UpdateWindowCaption;
  end;
  if not image.CurrentLayerVisible and not ToolManager.ToolCanBeUsed then
  begin
    ChooseTool(ptHand);
    MessagePopup(rsToolOnInvisibleLayer,5000);
  end;
end;

procedure TFMain.OnImageRenderChanged(Sender: TObject);
begin
  InvalidatePicture(false);
end;

procedure TFMain.UpdateEditPicture(ADelayed: boolean = false);
begin
  if ToolManager.ToolUpdate then
  begin
    if ADelayed then PaintPictureLater
    else
      PaintPictureNow;
  end;
end;

procedure TFMain.PaintPictureNow;
begin
  if not visible then exit;
  if Assigned(LazPaintInstance) then LazPaintInstance.UpdateStackOnTimer := true;
  if Assigned(FLayout) then FLayout.PaintPictureNow;
end;

procedure TFMain.PaintPictureLater;
begin
  if Assigned(FLayout) then FLayout.DelayedPaintPicture := True;
end;

procedure TFMain.NotifyImagePaint;
begin
  if Assigned(FLayout) then
    FLayout.DelayedPaintPicture:= false;
end;

procedure TFMain.PictureMouseBefore(Sender: TObject; AShift: TShiftState);
begin
  UpdateSpecialKeys(AShift);
end;

procedure TFMain.PictureMouseMove(Sender: TObject; APosition: TPointF);
begin
  HidePenPreview;
  HideFill;

  FCoordinatesCaption := IntToStr(round(APosition.X))+','+IntToStr(round(APosition.Y));
  Inc(FCoordinatesCaptionCount);
  if FCoordinatesCaptionCount > 8 then
  begin
    FCoordinatesCaptionCount := 0;
    Label_Coordinates.caption := FCoordinatesCaption;
    Label_Coordinates.Update;
    UpdateStatusText;
  end;

  if LazPaintInstance.TopMostHasFocus then
  begin
    if LazPaintInstance.TopMostOkToUnfocus then
    begin
      if ToolWindowStyle = fsStayOnTop then
        SafeSetFocus(self);
    end
    else
      exit;
  end;
  if (CurrentTool in[ptText,ptEditShape]) and TextSpinEditFocused then Layout.FocusImage;
end;

procedure TFMain.PictureOnPaint(Sender: TObject);
var
  ac: TWinControl;
begin
  if FirstPaint then
  begin
    LoadToolwindow := True;
    TimerLoadToolWin.Enabled := true;
    FirstPaint := false;
  end;
  ac := ActiveControl;
  if ac is TBCTrackbarUpdown then
    TBCTrackbarUpdown(ac).DelayTimer;
end;

procedure TFMain.PictureToolbarUpdate(Sender: TObject);
begin
  UpdateToolbar;
end;

procedure TFMain.PictureSelectedLayerIndexChanged(sender: TLazPaintImage);
begin
  if not image.CurrentLayerVisible and not ToolManager.ToolCanBeUsed then
    ChooseTool(ptHand)
  else
  begin
    ToolManager.ToolOpen;
    ToolManager.UpdateContextualToolbars;
  end;
end;

procedure TFMain.PictureSelectedLayerIndexChanging(sender: TLazPaintImage);
begin
  ToolManager.ToolCloseDontReopen;
end;

procedure TFMain.SetShowSelectionNormal(const AValue: boolean);
begin
  FShowSelectionNormal := AValue;
  if Assigned(FLayout) then
    FLayout.FillSelectionHighlight := ToolManager.DisplayFilledSelection and not FShowSelectionNormal;
end;

procedure TFMain.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
   //  block Erasing background
   //  inherited EraseBackground(DC);
end;

function TFMain.GetImage: TLazPaintImage;
begin
  result := LazPaintInstance.Image;
end;

procedure TFMain.ManagerBrushChanged(Sender: TObject);
begin
  UpdateBrush;
end;

procedure TFMain.ManagerBrushListChanged(Sender: TObject);
begin
  UpdateBrushList;
end;

procedure TFMain.ManagerDeformationGridSizeChanged(Sender: TObject);
begin
  UpdateDeformationGridToolbar;
end;

procedure TFMain.ManagerEraserChanged(Sender: TObject);
begin
  UpdateEraserToolbar;
end;

procedure TFMain.ManagerFloodFillOptionChanged(Sender: TObject);
begin
  UpdateFloodFillToolbar;
end;

procedure TFMain.ManagerJoinStyleChanged(Sender: TObject);
begin
  UpdateJoinStyleToolbar;
end;

procedure TFMain.ManagerLineCapChanged(Sender: TObject);
begin
  UpdateLineCapToolbar;
end;

procedure TFMain.ManagerPerspectiveOptionChanged(Sender: TObject);
begin
  UpdatePerspectiveToolbar;
end;

procedure TFMain.ManagerPhongShapeChanged(Sender: TObject);
begin
  UpdatePhongToolbar;
end;

procedure TFMain.ManagerPenStyleChanged(Sender: TObject);
begin
  UpdatePenStyleToolbar;
end;

procedure TFMain.ManagerPenWidthChanged(Sender: TObject);
begin
  UpdatePenWidthToolbar;
end;

procedure TFMain.ManagerShapeRatioChanged(Sender: TObject);
begin
  UpdateShapeRatio;
end;

procedure TFMain.ManagerSplineStyleChanged(Sender: TObject);
begin
  UpdateSplineStyleToolbar;
end;

procedure TFMain.ManagerTextAlignChanged(Sender: TObject);
begin
  UpdateTextAlign;
end;

procedure TFMain.ManagerTextFontChanged(Sender: TObject);
begin
  UpdateTextFontToolbar(False);
end;

procedure TFMain.ManagerTextOutlineChanged(Sender: TObject);
begin
  UpdateTextOutlineToolbar;
end;

procedure TFMain.ManagerTextPhongChanged(Sender: TObject);
begin
  UpdateTextPhongToolbar;
end;

procedure TFMain.ManagerTextShadowChanged(Sender: TObject);
begin
  UpdateTextShadowToolbar;
end;

procedure TFMain.ManagerShapeOptionChanged(Sender: TObject);
begin
  UpdateToolOptions;
  ToolManager.UpdateContextualToolbars;
end;

procedure TFMain.ManagerToleranceChanged(Sender: TObject);
begin
  UpdateToleranceToolbar;
end;

procedure TFMain.ManagerToolbarChanged(Sender: TObject);
begin
  QueryArrange;
end;

procedure TFMain.UpdateStatusText;
var s: string;
begin
  if ToolManager.CurrentTool <> nil then
  begin
    s := ToolManager.CurrentTool.StatusText;
    if s = '' then s := currentToolLabel;
  end
  else s := '';
  Layout.StatusText := s;
end;

procedure TFMain.InvalidatePicture(AInvalidateAll: boolean = true);
begin
  if Assigned(FLayout) then
    FLayout.InvalidatePicture(AInvalidateAll);
end;

function TFMain.GetUseImageBrowser: boolean;
begin
  result := Config.DefaultUseImageBrowser;
end;

procedure TFMain.SetDarkTheme(AValue: boolean);
begin
  if Layout.DarkTheme<>AValue then
  begin
    Layout.DarkTheme := AValue;
    DarkThemeInstance.Apply(Panel_PenWidthPreview, AValue);
    Invalidate;
    UpdateToolImage(true);
  end;
end;

procedure TFMain.SetUpdatingPopup(AValue: boolean);
begin
  if Assigned(FLayout) then
    FLayout.UpdatingPopup := AValue;
end;

function TFMain.GetDarkTheme: boolean;
begin
  if Assigned(FLayout) then result := FLayout.DarkTheme
  else result := false;
end;

function TFMain.GetImageAction: TImageActions;
begin
  result := TImageActions(LazPaintInstance.ImageAction);
end;

function TFMain.GetUpdatingPopup: boolean;
begin
  if Assigned(FLayout) then
    result := FLayout.UpdatingPopup
    else result := false;
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

{$R *.lfm}

end.
