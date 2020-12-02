// SPDX-License-Identifier: GPL-3.0-only
unit LazpaintInstance;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazPaintType, BGRABitmap, BGRABitmapTypes, BGRALayers, LCVectorialFill,
  Menus, Forms, Controls, fgl,

  LazPaintMainForm, UMainFormLayout,

  UToolbox, UChooseColor, ULayerstack, UCanvassize,
  UColorintensity, UShiftColors, UColorize, uadjustcurves,
  UCustomblur, uimagelist,

  ULoading, UImage, UImageAction, UTool, uconfig, IniFiles, UResourceStrings, UScripting,
  UScriptType;

const
  MaxToolPopupShowCount = 3;

type
  TImageListList = specialize TFPGObjectList<TImageList>;

  TListeners = specialize TFPGList<TNotifyEvent>;

  { TLazPaintInstance }

  TLazPaintInstance = class(TLazPaintCustomInstance)
  private
    FScriptName: String;
    FThemeListeners: TListeners;

    procedure ChooseColorHide(Sender: TObject);
    function GetFormAdjustCurves: TFAdjustCurves;
    function GetFormCanvasSize: TFCanvasSize;
    function GetFormColorIntensity: TFColorIntensity;
    function GetFormColorize: TFColorize;
    function GetFormCustomBlur: TFCustomBlur;
    function GetFormShiftColors: TFShiftColors;
    function GetInitialized: boolean;
    function GetMainFormVisible: boolean;
    procedure LayerStackHide(Sender: TObject);
    procedure OnImageActionProgress({%H-}ASender: TObject; AProgressPercent: integer);
    procedure OnLayeredBitmapLoadStartHandler(AFilenameUTF8: string);
    procedure OnLayeredBitmapLoadProgressHandler(APercentage: integer);
    procedure OnLayeredBitmapLoadedHandler;
    procedure OnLayeredBitmapSavedHandler();
    procedure OnLayeredBitmapSaveProgressHandler(APercentage: integer);
    procedure OnLayeredBitmapSaveStartHandler(AFilenameUTF8: string);
    procedure OnSizeChanged(Sender: TObject);
    procedure RegisterScripts(ARegister: Boolean);
    function ScriptColorColorize(AVars: TVariableSet): TScriptResult;
    function ScriptColorCurves(AVars: TVariableSet): TScriptResult;
    function ScriptColorIntensity(AVars: TVariableSet): TScriptResult;
    function ScriptColorLightness(AVars: TVariableSet): TScriptResult;
    function ScriptColorPosterize(AVars: TVariableSet): TScriptResult;
    function ScriptColorShiftColors(AVars: TVariableSet): TScriptResult;
    function ScriptFileGetTemporaryName(AVars: TVariableSet): TScriptResult;
    function ScriptFileNew(AVars: TVariableSet): TScriptResult;
    function ScriptGetName(AVars: TVariableSet): TScriptResult;
    function ScriptImageCanvasSize(AVars: TVariableSet): TScriptResult;
    function ScriptImageRepeat(AVars: TVariableSet): TScriptResult;
    function ScriptImageResample(AParams: TVariableSet): TScriptResult;
    function ScriptLazPaintGetVersion(AVars: TVariableSet): TScriptResult;
    function ScriptShowDirectoryDialog(AVars: TVariableSet): TScriptResult;
    function ScriptTranslateGetLanguage(AVars: TVariableSet): TScriptResult;
    function ScriptTranslateText(AVars: TVariableSet): TScriptResult;
    procedure SelectionInstanceOnRun(AInstance: TLazPaintCustomInstance);
    procedure ToolFillChanged(Sender: TObject);
    procedure PythonScriptCommand({%H-}ASender: TObject; ACommand, AParam: UTF8String; out
      AResult: UTF8String);
    procedure PythonBusy({%H-}Sender: TObject);
    function ScriptShowMessage(AVars: TVariableSet): TScriptResult;
    function ScriptInputBox(AVars: TVariableSet): TScriptResult;
    procedure ToolQueryColorTarget(sender: TToolManager; ATarget: TVectorialFill);

  protected
    InColorFromFChooseColor: boolean;
    FMain: TFMain;
    FFormToolbox: TFToolbox;
    FFormToolboxInitialPopup: TPopupMenu;
    FFormToolboxInitialPosition: TPoint;
    FImageList: TFImageList;
    FChooseColor: TFChooseColor;
    FLayerStack: TFLayerStack;
    FFormCanvasSize: TFCanvasSize;
    FFormColorIntensity: TFColorIntensity;
    FFormShiftColors: TFShiftColors;
    FFormColorize: TFColorize;
    FFormAdjustCurves: TFAdjustCurves;
    FFormCustomBlur: TFCustomBlur;
    FLoadingLayers: TFLoading;
    FTopMostInfo: TTopMostInfo;
    FGridVisible: boolean;
    FConfig: TLazPaintConfig;
    FImage: TLazPaintImage;
    FImageAction: TImageActions;
    FToolManager : TToolManager;
    FEmbedded: boolean;
    FDestroying: boolean;
    FSelectionEditConfig: TStream;
    FTextureEditConfig: TStream;
    FScriptContext: TScriptContext;
    FInFormsNeeded: boolean;
    FLayerControlVisible, FChooseColorControlVisible: boolean;
    FDockLayersAndColors, FFullscreen: boolean;
    FPrevDockArea: TRect;
    FInSetToolboxVisible: boolean;
    FToolBoxPositionDefined,
    FChooseColorPositionDefined,
    FLayerStackPositionDefined,
    FImageListPositionDefined : boolean;
    FCustomImageList: TImageListList;
    FLoadingFilename, FSavingFilename: string;
    FInRunScript: boolean;
    FScriptTempFileNames: TStringList;
    FInCommandLine: boolean;
    FUpdateStackOnTimer: boolean;

    function GetIcons(ASize: integer): TImageList; override;
    function GetToolBoxWindowPopup: TPopupMenu; override;
    procedure SetToolBoxWindowPopup(AValue: TPopupMenu); override;
    function GetFullscreen: boolean; override;
    procedure SetFullscreen(AValue: boolean); override;
    function GetToolWindowVisible(AWindow: TForm; ADockedVisible: boolean = false): boolean;
    function GetDockLayersAndColors: boolean; override;
    procedure SetDockLayersAndColors(AValue: boolean); override;
    function GetScriptContext: TScriptContext; override;
    function GetShowSelectionNormal: boolean; override;
    procedure SetShowSelectionNormal(AValue: boolean); override;
    function GetEmbedded: boolean; override;
    function GetGridVisible: boolean; override;
    procedure SetGridVisible(const AValue: boolean); override;
    function GetChooseColorVisible: boolean; override;
    function GetToolboxVisible: boolean; override;
    function GetImageListWindowVisible: boolean; override;
    procedure SetChooseColorVisible(const AValue: boolean); override;
    procedure SetToolBoxVisible(const AValue: boolean); override;
    procedure SetImageListWindowVisible(const AValue: boolean); override;
    function GetChooseColorHeight: integer; override;
    function GetChooseColorWidth: integer; override;
    procedure SetChooseColorHeight(AValue: integer); override;
    procedure SetChooseColorWidth(AValue: integer); override;
    function GetToolboxHeight: integer; override;
    function GetToolboxWidth: integer; override;
    function GetTopMostHasFocus: boolean; override;
    function GetTopMostVisible: boolean; override;
    function GetTopMostOkToUnfocus: boolean; override;
    function GetChooseColorTarget: TColorTarget; override;
    procedure SetChooseColorTarget(const AValue: TColorTarget); override;
    function GetConfig: TLazPaintConfig; override;
    function GetImage: TLazPaintImage; override;
    function GetImageAction: TImageActions; override;
    function GetToolManager: TToolManager; override;
    function GetUpdateStackOnTimer: boolean; override;
    procedure SetUpdateStackOnTimer(AValue: boolean); override;
    procedure CreateLayerStack;
    procedure CreateToolBox;
    procedure FormsNeeded;
    procedure Init(AEmbedded: boolean);
    procedure SetBlackAndWhite(AValue: boolean); override;
    procedure OnStackChanged({%H-}sender: TLazPaintImage; AScrollIntoView: boolean);
    procedure OnToolPopup({%H-}sender: TToolManager; AMessage: TToolPopupMessage; AKey: Word;
      AAlways: boolean);

    function GetImageListWindowHeight: integer; override;
    function GetImageListWindowWidth: integer; override;
    procedure SetImageListWindowHeight(AValue: integer); override;
    procedure SetImageListWindowWidth(AValue: integer); override;
    function GetLayerWindowHeight: integer; override;
    function GetLayerWindowWidth: integer; override;
    procedure SetLayerWindowHeight(AValue: integer); override;
    procedure SetLayerWindowWidth(AValue: integer); override;
    function GetLayerWindowVisible: boolean; override;
    procedure SetLayerWindowVisible(AValue: boolean); override;
    procedure OnFunctionException(AFunctionName: string; AException: Exception);
    function GetMainFormBounds: TRect; override;
    procedure EditSelectionHandler(var AImage: TBGRABitmap);
    function GetZoomFactor: single; override;
    procedure UpdateLayerControlVisibility;
    procedure UpdateChooseColorControlVisibility;
    property FormCanvasSize: TFCanvasSize read GetFormCanvasSize;
    property FormColorIntensity: TFColorIntensity read GetFormColorIntensity;
    property FormShiftColors: TFShiftColors read GetFormShiftColors;
    property FormColorize: TFColorize read GetFormColorize;
    property FormAdjustCurves: TFAdjustCurves read GetFormAdjustCurves;
    property FormCustomBlur: TFCustomBlur read GetFormCustomBlur;

  public
    constructor Create; override;
    constructor Create(AEmbedded: boolean); override;
    procedure RegisterThemeListener(AHandler: TNotifyEvent; ARegister: boolean); override;
    procedure NotifyThemeChanged; override;
    procedure StartLoadingImage(AFilename: string); override;
    procedure EndLoadingImage; override;
    procedure StartSavingImage(AFilename: string); override;
    procedure EndSavingImage; override;
    procedure ReportActionProgress(AProgressPercent: integer); override;
    procedure Donate; override;
    procedure SaveMainWindowPosition; override;
    procedure RestoreMainWindowPosition; override;
    procedure UseConfig(ini: TInifile); override;
    procedure AssignBitmap(bmp: TBGRABitmap); override;
    procedure EditBitmap(var bmp: TBGRABitmap; ConfigStream: TStream = nil; ATitle: String = ''; AOnRun: TLazPaintInstanceEvent = nil; AOnExit: TLazPaintInstanceEvent = nil; ABlackAndWhite: boolean = false); override;
    procedure EditSelection; override;
    function EditTexture(ASource: TBGRABitmap): TBGRABitmap; override;
    function ProcessCommandLine: boolean; override;
    function ProcessCommands(commands: TStringList): boolean; override;
    procedure ChangeIconSize(size: integer); override;
    procedure Show; override;
    function Hide: boolean; override;
    procedure Run; override;
    procedure Restart; override;
    procedure CancelRestart; override;
    destructor Destroy; override;
    procedure NotifyImageChange(RepaintNow: boolean; ARect: TRect); override;
    procedure NotifyImageChangeCompletely(RepaintNow: boolean); override;
    procedure NotifyImagePaint; override;
    function TryOpenFileUTF8(filename: string; skipDialogIfSingleImage: boolean = false): boolean; override;
    function ExecuteFilter(filter: TPictureFilter; skipDialog: boolean = false): TScriptResult; override;
    function RunScript(AFilename: string; ACaption: string = ''): boolean; override;
    procedure AdjustChooseColorHeight; override;
    procedure ColorFromFChooseColor; override;
    procedure ColorToFChooseColor; override;
    procedure ExitColorEditor; override;
    function ColorEditorActive: boolean; override;
    function ShowSaveOptionDlg({%H-}AParameters: TVariableSet; AOutputFilenameUTF8: string;
      ASkipOptions: boolean; AExport: boolean): boolean; override;
    function ShowColorIntensityDlg(AParameters: TVariableSet): TScriptResult; override;
    function ShowColorLightnessDlg(AParameters: TVariableSet): TScriptResult; override;
    function ShowShiftColorsDlg(AParameters: TVariableSet): TScriptResult; override;
    function ShowColorizeDlg(AParameters: TVariableSet): TScriptResult; override;
    function ShowColorCurvesDlg(AParameters: TVariableSet): TScriptResult; override;
    function ShowRadialBlurDlg(AFilterConnector: TObject; blurType:TRadialBlurType; ACaption: string = ''): TScriptResult; override;
    function ShowMotionBlurDlg(AFilterConnector: TObject): TScriptResult; override;
    function ShowCustomBlurDlg(AFilterConnector: TObject): TScriptResult; override;
    function ShowEmbossDlg(AFilterConnector: TObject): TScriptResult; override;
    function ShowRainDlg(AFilterConnector: TObject): TScriptResult; override;
    function ShowPixelateDlg(AFilterConnector: TObject): TScriptResult; override;
    function ShowNoiseFilterDlg(AFilterConnector: TObject): TScriptResult; override;
    function ShowTwirlDlg(AFilterConnector: TObject): TScriptResult; override;
    function ShowWaveDisplacementDlg(AFilterConnector: TObject): TScriptResult; override;
    function ShowPhongFilterDlg(AFilterConnector: TObject): TScriptResult; override;
    function ShowFunctionFilterDlg(AFilterConnector: TObject): TScriptResult; override;
    function ShowSharpenDlg(AFilterConnector: TObject): TScriptResult; override;
    function ShowPosterizeDlg(AParameters: TVariableSet): TScriptResult; override;
    procedure ShowPrintDlg; override;
    function HideTopmost: TTopMostInfo; override;
    procedure ShowTopmost(AInfo: TTopMostInfo); override;
    procedure UpdateWindows; override;
    procedure Wait(ACheckActive: TCheckFunction; ADelayMs: integer); override;
    procedure ShowCanvasSizeDlg; override;
    procedure ShowRepeatImageDlg; override;
    procedure MoveToolboxTo(X,Y: integer); override;
    procedure MoveChooseColorTo(X,Y: integer); override;
    procedure MoveLayerWindowTo(X,Y: integer); override;
    procedure MoveImageListWindowTo(X,Y: integer); override;
    procedure ImageListWindowVisibleKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure ShowAboutDlg; override;
    function ShowNewImageDlg(out bitmap: TBGRABitmap):boolean; override;
    function ShowResampleDialog(AParameters: TVariableSet):boolean; override;
    property MainFormVisible: boolean read GetMainFormVisible;
    procedure NotifyStackChange; override;
    procedure ScrollLayerStackOnItem(AIndex: integer; ADelayedUpdate: boolean = true); override;
    procedure InvalidateLayerStack; override;
    procedure UpdateLayerStackOnTimer; override;
    function MakeNewBitmapReplacement(AWidth, AHeight: integer; AColor: TBGRAPixel): TBGRABitmap; override;
    procedure ChooseTool(Tool : TPaintToolType); override;
    function OpenImage (FileName: string; AddToRecent: Boolean= True): boolean; override;
    procedure AddToImageList(const FileNames: array of String); override;
    procedure UpdateToolbar; override;
    procedure UpdateEditPicture(ADelayed: boolean); override;
    procedure AddColorToPalette(AColor: TBGRAPixel); override;
    procedure RemoveColorFromPalette(AColor: TBGRAPixel); override;
    property Initialized: boolean read GetInitialized;
  end;

implementation

uses LCLType, Types, Dialogs, FileUtil, StdCtrls, LCLIntf, BGRAUTF8, UTranslation,

     URadialBlur, UMotionBlur, UEmboss, UTwirl, UWaveDisplacement,
     unewimage, uresample, UPixelate, unoisefilter, ufilters,
     USharpen, uposterize, UPhongFilter, UFilterFunction,
     uprint, USaveOption, UFormRain,

     ugraph, LCScaleDPI, ucommandline, uabout, UPython, BGRAGraphics;

{ TLazPaintInstance }

{$i lazpaintdialogs.inc}

constructor TLazPaintInstance.Create;
begin
  Init(False);
end;

constructor TLazPaintInstance.Create(AEmbedded: boolean);
begin
  Init(AEmbedded);
end;

procedure TLazPaintInstance.RegisterThemeListener(AHandler: TNotifyEvent;
  ARegister: boolean);
begin
  if ARegister then
  begin
    if FThemeListeners.IndexOf(AHandler) = -1 then
      FThemeListeners.Add(AHandler);
  end else
  begin
    FThemeListeners.Remove(AHandler);
  end;
end;

procedure TLazPaintInstance.NotifyThemeChanged;
var
  i: Integer;
begin
  for i := 0 to FThemeListeners.Count-1 do
    FThemeListeners[i](self);
end;

procedure TLazPaintInstance.StartLoadingImage(AFilename: string);
begin
  FLoadingFilename:= AFilename;
  if not FInCommandLine then
    BGRALayers.RegisterLoadingHandler(@OnLayeredBitmapLoadStartHandler,@OnLayeredBitmapLoadProgressHandler,@OnLayeredBitmapLoadedHandler);
end;

procedure TLazPaintInstance.EndLoadingImage;
begin
  BGRALayers.UnregisterLoadingHandler(@OnLayeredBitmapLoadStartHandler,@OnLayeredBitmapLoadProgressHandler,@OnLayeredBitmapLoadedHandler);
  FreeAndNil(FLoadingLayers);
  FLoadingFilename:= '';
end;

procedure TLazPaintInstance.StartSavingImage(AFilename: string);
begin
  Screen.Cursor := crHourGlass;
  UpdateWindows;
  FSavingFilename:= AFilename;
  if not FInCommandLine then
    BGRALayers.RegisterSavingHandler(@OnLayeredBitmapSaveStartHandler,@OnLayeredBitmapSaveProgressHandler,@OnLayeredBitmapSavedHandler);
end;

procedure TLazPaintInstance.EndSavingImage;
begin
  BGRALayers.UnregisterSavingHandler(@OnLayeredBitmapSaveStartHandler,@OnLayeredBitmapSaveProgressHandler,@OnLayeredBitmapSavedHandler);
  FreeAndNil(FLoadingLayers);
  FSavingFilename:= '';
  Screen.Cursor := crDefault;
  UpdateWindows;
end;

procedure TLazPaintInstance.ReportActionProgress(AProgressPercent: integer);
var
  delay: Integer;
begin
  if AProgressPercent < 100 then delay := 10000 else delay := 1000;
  if Assigned(FMain) then FMain.UpdatingPopup:= true;
  try
    MessagePopup(rsActionInProgress+'... '+inttostr(AProgressPercent)+'%', delay);
    UpdateWindows;
  finally
    if Assigned(FMain) then FMain.UpdatingPopup:= false;
  end;
end;

procedure TLazPaintInstance.Donate;
begin
  OpenURL('http://sourceforge.net/donate/index.php?group_id=404555');
end;

procedure TLazPaintInstance.SaveMainWindowPosition;
var r:TRect;
begin
  if FMain.WindowState = wsMinimized then exit;
  if FMain.WindowState = wsMaximized then
    Config.SetDefaultMainWindowMaximized(true) else
  If FMain.WindowState = wsNormal then
    begin
      r.left := FMain.Left;
      r.top := FMain.Top;
      r.right := r.left+FMain.ClientWidth;
      r.Bottom := r.top+FMain.ClientHeight;
      Config.SetDefaultMainWindowPosition(r);
    end;
end;

procedure TLazPaintInstance.RestoreMainWindowPosition;
var r:TRect;
begin
  if not MainFormVisible then exit;
  if Config.DefaultMainWindowMaximized then
    FMain.WindowState := wsMaximized else
  begin
    r := Config.DefaultMainWindowPosition;
    if (r.right > r.left) and (r.bottom > r.top) then
    begin
      FMain.Position := poDesigned;
      FMain.Left := r.Left;
      FMain.Top := r.Top;
      FMain.ClientWidth := r.right-r.left;
      FMain.ClientHeight := r.bottom-r.top
    end;
  end;
end;

procedure TLazPaintInstance.RegisterScripts(ARegister: Boolean);
begin
  if not Assigned(ScriptContext) then exit;
  ScriptContext.RegisterScriptFunction('FileGetTemporaryName', @ScriptFileGetTemporaryName,ARegister);
  ScriptContext.RegisterScriptFunction('FileNew',@ScriptFileNew,ARegister);
  ScriptContext.RegisterScriptFunction('ImageResample',@ScriptImageResample,ARegister);
  ScriptContext.RegisterScriptFunction('ImageCanvasSize',@ScriptImageCanvasSize,ARegister);
  ScriptContext.RegisterScriptFunction('ImageRepeat',@ScriptImageRepeat,ARegister);
  ScriptContext.RegisterScriptFunction('ColorCurves',@ScriptColorCurves,ARegister);
  ScriptContext.RegisterScriptFunction('ColorPosterize',@ScriptColorPosterize,ARegister);
  ScriptContext.RegisterScriptFunction('ColorColorize',@ScriptColorColorize,ARegister);
  ScriptContext.RegisterScriptFunction('ColorLightness',@ScriptColorLightness,ARegister);
  ScriptContext.RegisterScriptFunction('ColorShiftColors',@ScriptColorShiftColors,ARegister);
  ScriptContext.RegisterScriptFunction('ColorIntensity',@ScriptColorIntensity,ARegister);
  ScriptContext.RegisterScriptFunction('ShowMessage',@ScriptShowMessage,ARegister);
  ScriptContext.RegisterScriptFunction('ShowDirectoryDialog',@ScriptShowDirectoryDialog,ARegister);
  ScriptContext.RegisterScriptFunction('InputBox',@ScriptInputBox,ARegister);
  ScriptContext.RegisterScriptFunction('LazPaintGetVersion',@ScriptLazPaintGetVersion,ARegister);
  ScriptContext.RegisterScriptFunction('TranslateText',@ScriptTranslateText,ARegister);
  ScriptContext.RegisterScriptFunction('TranslateGetLanguage',@ScriptTranslateGetLanguage,ARegister);
  ScriptContext.RegisterScriptFunction('ScriptGetName',@ScriptGetName,ARegister);
end;

function TLazPaintInstance.ScriptFileGetTemporaryName(AVars: TVariableSet): TScriptResult;
var
  name: String;
  t: file;
begin
  if FInRunScript and Assigned(FScriptTempFileNames) then
  begin
    try
      name := GetTempFileName;
      assignfile(t, name);
      rewrite(t);
      closefile(t);
      AVars.Strings['Result'] := name;
      FScriptTempFileNames.Add(name);
      result := srOk;
    except
      on ex: exception do
      begin
        ShowError(rsScript, ex.Message);
        result := srException;
      end;
    end;
  end else
    result := srException;
end;

procedure TLazPaintInstance.Init(AEmbedded: boolean);
begin
  Title := 'LazPaint ' + LazPaintCurrentVersion;
  FThemeListeners := TListeners.Create;
  FCustomImageList := TImageListList.Create;
  FTopMostInfo.choosecolorHidden := 0;
  FTopMostInfo.layerstackHidden := 0;
  FTopMostInfo.toolboxHidden := 0;
  FTopMostInfo.imagelistHidden := 0;
  FEmbedded:= AEmbedded;
  FScriptContext := TScriptContext.Create;
  FScriptContext.OnFunctionException:= @OnFunctionException;
  FFormToolboxInitialPopup := nil;
  FFormToolboxInitialPosition := Point(0,0);

  RegisterScripts(True);

  InColorFromFChooseColor := false;
  FImage := TLazPaintImage.Create(self);
  FImage.OnStackChanged:= @OnStackChanged;
  FImage.OnException := @OnFunctionException;
  FImage.OnActionProgress:=@OnImageActionProgress;
  FImage.OnSizeChanged:=@OnSizeChanged;
  FToolManager := TToolManager.Create(FImage, self, nil, BlackAndWhite, FScriptContext);
  UseConfig(TIniFile.Create(''));
  FToolManager.OnPopup := @OnToolPopup;
  FToolManager.OnFillChanged:= @ToolFillChanged;
  FToolManager.OnQueryColorTarget:=@ToolQueryColorTarget;
  FSelectionEditConfig := nil;
  FTextureEditConfig := nil;
  FImageAction := TImageActions.Create(self);
end;

procedure TLazPaintInstance.FormsNeeded;
begin
  if (FMain <> nil) or FInFormsNeeded then exit;

  FInFormsNeeded := true;
  Application.CreateForm(TFMain, FMain);
  FMain.LazPaintInstance := self;

  CreateLayerStack;

  Application.CreateForm(TFImageList, FImageList);
  FImageList.LazPaintInstance := self;

  TFChooseColor_CustomDPI := (Config.DefaultIconSize(DoScaleX(16,OriginalDPI))*96+8) div 16;
  Application.CreateForm(TFChooseColor, FChooseColor);
  FChooseColor.LazPaintInstance := self;
  FChooseColor.DarkTheme:= Config.GetDarkTheme;
  FChooseColor.OnHide:=@ChooseColorHide;

  FInFormsNeeded := false;
end;

procedure TLazPaintInstance.UseConfig(ini: TInifile);
begin
  FreeAndNil(FConfig);
  FConfig := TLazPaintConfig.Create(ini,LazPaintVersionStr);
  ToolManager.LoadFromConfig;
  FGridVisible := Config.DefaultGridVisible;
  FDockLayersAndColors:= Config.DefaultDockLayersAndColors;
end;

function TLazPaintInstance.GetConfig: TLazPaintConfig;
begin
  result := FConfig;
end;

function TLazPaintInstance.GetImage: TLazPaintImage;
begin
  Result:= FImage;
end;

function TLazPaintInstance.GetImageAction: TImageActions;
begin
  result := FImageAction;
end;

function TLazPaintInstance.GetToolManager: TToolManager;
begin
  Result:= FToolManager;
end;

function TLazPaintInstance.GetUpdateStackOnTimer: boolean;
begin
  result := FUpdateStackOnTimer;
end;

procedure TLazPaintInstance.SetUpdateStackOnTimer(AValue: boolean);
begin
  FUpdateStackOnTimer := AValue;
end;

procedure TLazPaintInstance.CreateLayerStack;
var
  defaultZoom: Single;
begin
  if Assigned(FLayerStack) then exit;
  TFLayerStack_CustomDPI := (Config.DefaultIconSize(DoScaleX(16,OriginalDPI))*96+8) div 16;
  Application.CreateForm(TFLayerStack,FLayerStack);
  FLayerStack.OnHide:=@LayerStackHide;
  FLayerStack.LazPaintInstance := self;
  FLayerStack.DarkTheme:= Config.GetDarkTheme;
  defaultZoom := Config.DefaultLayerStackZoom;
  if defaultZoom <> EmptySingle then
    FLayerStack.ZoomFactor:= defaultZoom;

  FLayerStack.AddButton(FMain.LayerRemoveCurrent);
  FLayerStack.AddButton(FMain.LayerAddNew);
  FLayerStack.AddButton(FMain.LayerFromFile);
  FLayerStack.AddButton(FMain.LayerDuplicate);
  FLayerStack.AddButton(FMain.LayerMergeOver);
  FLayerStack.AddButton(FMain.LayerRasterize);
  FLayerStack.AddButton(FMain.LayerMove);
  FLayerStack.AddButton(FMain.LayerRotate);
  FLayerStack.AddButton(FMain.LayerZoom);
  FLayerStack.AddButton(FMain.LayerHorizontalFlip);
  FLayerStack.AddButton(FMain.LayerVerticalFlip);
  FLayerStack.AddButton(FMain.ToolLayerMapping);

  FLayerStack.AddLayerMenu(FMain.LayerDuplicate);
  FLayerStack.AddLayerMenu(FMain.LayerRemoveCurrent);
  FLayerStack.AddLayerMenu(FMain.LayerRasterize);
  FLayerStack.AddLayerMenu(FMain.LayerExport);
end;

procedure TLazPaintInstance.CreateToolBox;
begin
  if Assigned(FFormToolbox) or not Assigned(FMain) then exit;
  Application.CreateForm(TFToolbox, FFormToolbox);
  FFormToolbox.LazPaintInstance := self;
  FFormToolbox.DarkTheme := Config.GetDarkTheme;

  //needed to attach to the right instance of FMain
  FFormToolbox.AddButton(FFormToolbox.Toolbar1, FMain.ToolHand);
  FFormToolbox.AddButton(FFormToolbox.Toolbar1, FMain.ToolColorPicker);
  FFormToolbox.AddButton(FFormToolbox.Toolbar1, FMain.ToolPen);
  FFormToolbox.AddButton(FFormToolbox.Toolbar1, FMain.ToolBrush);
  FFormToolbox.AddButton(FFormToolbox.Toolbar1, FMain.ToolEraser);
  FFormToolbox.AddButton(FFormToolbox.Toolbar1, FMain.ToolFloodfill);
  FFormToolbox.AddButton(FFormToolbox.Toolbar1, FMain.ToolClone);

  FFormToolbox.AddButton(FFormToolbox.Toolbar2, FMain.ToolRect);
  FFormToolbox.AddButton(FFormToolbox.Toolbar2, FMain.ToolEllipse);
  FFormToolbox.AddButton(FFormToolbox.Toolbar2, FMain.ToolPolygon);
  FFormToolbox.AddButton(FFormToolbox.Toolbar2, FMain.ToolSpline);
  FFormToolbox.AddButton(FFormToolbox.Toolbar2, FMain.ToolGradient);
  FFormToolbox.AddButton(FFormToolbox.Toolbar2, FMain.ToolPhong);
  FFormToolbox.AddButton(FFormToolbox.Toolbar2, FMain.ToolText);

  FFormToolbox.AddButton(FFormToolbox.Toolbar3, FMain.ToolEditShape);
  FFormToolbox.AddButton(FFormToolbox.Toolbar3, FMain.ToolDeformation);
  FFormToolbox.AddButton(FFormToolbox.Toolbar3, FMain.ToolTextureMapping);
  FFormToolbox.AddButton(FFormToolbox.Toolbar3, FMain.EditSelectAll);
  FFormToolbox.AddButton(FFormToolbox.Toolbar3, FMain.ToolMoveSelection);
  FFormToolbox.AddButton(FFormToolbox.Toolbar3, FMain.ToolRotateSelection);
  FFormToolbox.AddButton(FFormToolbox.Toolbar3, FMain.EditDeselect);

  FFormToolbox.AddButton(FFormToolbox.Toolbar4, FMain.ToolSelectRect);
  FFormToolbox.AddButton(FFormToolbox.Toolbar4, FMain.ToolSelectEllipse);
  FFormToolbox.AddButton(FFormToolbox.Toolbar4, FMain.ToolSelectPoly);
  FFormToolbox.AddButton(FFormToolbox.Toolbar4, FMain.ToolSelectSpline);
  FFormToolbox.AddButton(FFormToolbox.Toolbar4, FMain.ToolSelectPen);
  FFormToolbox.AddButton(FFormToolbox.Toolbar4, FMain.ToolMagicWand);
  FFormToolbox.AddButton(FFormToolbox.Toolbar4, FMain.ToolHotSpot);

  FFormToolbox.SetImages(Icons[Config.DefaultIconSize(DoScaleX(20,OriginalDPI))]);

  FFormToolbox.PopupMenu := FFormToolboxInitialPopup;

  if FToolBoxPositionDefined then
  begin
    FFormToolbox.Left := FFormToolboxInitialPosition.X;
    FFormToolbox.Top := FFormToolboxInitialPosition.Y;
  end;
end;

procedure TLazPaintInstance.SetBlackAndWhite(AValue: boolean);
begin
  inherited SetBlackAndWhite(AValue);
  if FToolManager <> nil then FToolManager.BlackAndWhite := AValue;
end;

procedure TLazPaintInstance.OnStackChanged(sender: TLazPaintImage;
  AScrollIntoView: boolean);
begin
  if (FLayerStack <> nil) then
    FLayerStack.InvalidateStack(AScrollIntoView);
end;

procedure TLazPaintInstance.OnToolPopup(sender: TToolManager; AMessage: TToolPopupMessage; AKey: Word;
  AAlways: boolean);
var messageStr: string;
    idx: integer;
begin
  if FInCommandLine then exit;
  if Assigned(Config) and not AAlways then
  begin
    idx := ord(AMessage);
    if Config.ToolPopupMessageShownCount(idx) < MaxToolPopupShowCount then
      Config.SetToolPopupMessageShownCount(idx, Config.ToolPopupMessageShownCount(idx)+1)
    else
      exit;
  end;
  messageStr := ToolPopupMessageToStr(AMessage, AKey);
  if messageStr <> '' then
    MessagePopup(messageStr,4000);
end;

function TLazPaintInstance.GetImageListWindowHeight: integer;
begin
  if FImageList <> nil then
    result := FImageList.Height
  else
    result := 0;
end;

function TLazPaintInstance.GetImageListWindowWidth: integer;
begin
  if FImageList <> nil then
    result := FImageList.Width
  else
    result := 0;
end;

procedure TLazPaintInstance.SetImageListWindowHeight(AValue: integer);
begin
  if FImageList <> nil then
    FImageList.Height := AValue;
end;

procedure TLazPaintInstance.SetImageListWindowWidth(AValue: integer);
begin
  if FImageList <> nil then
    FImageList.Width := AValue;
end;


function TLazPaintInstance.GetLayerWindowHeight: integer;
begin
  if FLayerStack <> nil then
    result := FLayerStack.Height
  else
    result := 0;
end;

function TLazPaintInstance.GetLayerWindowWidth: integer;
begin
  if FLayerStack <> nil then
    result := FLayerStack.Width
  else
    result := 0;
end;

procedure TLazPaintInstance.SetLayerWindowHeight(AValue: integer);
begin
  if FLayerStack <> nil then
  begin
    FLayerStack.Height := AValue;
    FLayerStack.LayerStackControl.Height := AValue;
  end;
end;

procedure TLazPaintInstance.SetLayerWindowWidth(AValue: integer);
begin
  if FLayerStack <> nil then
  begin
    FLayerStack.Width := AValue;
    FLayerStack.LayerStackControl.Width := AValue;
  end;
end;

function TLazPaintInstance.GetMainFormVisible: boolean;
begin
  if FMain <> nil then
    result := FMain.Visible
  else
    result := false;
end;

procedure TLazPaintInstance.LayerStackHide(Sender: TObject);
begin
  if not DockLayersAndColors then
    FLayerControlVisible:= false;
end;

procedure TLazPaintInstance.OnImageActionProgress(ASender: TObject;
  AProgressPercent: integer);
begin
  ReportActionProgress(AProgressPercent);
end;

function TLazPaintInstance.GetInitialized: boolean;
begin
  result := Assigned(FMain) and FMain.Initialized;
end;

function TLazPaintInstance.GetFormCanvasSize: TFCanvasSize;
begin
  if FFormCanvasSize = nil then
  begin
    Application.CreateForm(TFCanvasSize, FFormCanvasSize);
    FFormCanvasSize.LazPaintInstance := self;
  end;
  result := FFormCanvasSize;
end;

function TLazPaintInstance.GetFormAdjustCurves: TFAdjustCurves;
begin
  if FFormAdjustCurves = nil then
    Application.CreateForm(TFAdjustCurves, FFormAdjustCurves);
  result := FFormAdjustCurves;
end;

procedure TLazPaintInstance.ChooseColorHide(Sender: TObject);
begin
  if not DockLayersAndColors then
    FChooseColorControlVisible:= false;
end;

function TLazPaintInstance.GetFormColorIntensity: TFColorIntensity;
begin
  if FFormColorIntensity = nil then
    Application.CreateForm(TFColorIntensity, FFormColorIntensity);
  result := FFormColorIntensity;
end;

function TLazPaintInstance.GetFormColorize: TFColorize;
begin
  if FFormColorize = nil then
    Application.CreateForm(TFColorize, FFormColorize);
  result := FFormColorize;
end;

function TLazPaintInstance.GetFormCustomBlur: TFCustomBlur;
begin
  if FFormCustomBlur = nil then
  begin
    Application.CreateForm(TFCustomBlur, FFormCustomBlur);
    FFormCustomBlur.LazPaintInstance := self;
  end;
  result := FFormCustomBlur;
end;

function TLazPaintInstance.GetFormShiftColors: TFShiftColors;
begin
  if FFormShiftColors = nil then
    Application.CreateForm(TFShiftColors, FFormShiftColors);
  result := FFormShiftColors;
end;

procedure TLazPaintInstance.PythonScriptCommand(ASender: TObject; ACommand,
  AParam: UTF8String; out AResult: UTF8String);
var
  params: TVariableSet;
  err: TInterpretationErrors;
  scriptErr: TScriptResult;
  vRes: TScriptVariableReference;
  i: Integer;
begin
  AResult := 'None';
  if Assigned(FScriptContext) then
  begin
    params := TVariableSet.Create(ACommand);
    AParam := trim(AParam);
    if length(AParam)>0 then
    begin
      if AParam[1] = '{' then
      begin
        delete(AParam,1,1);
        if (length(AParam)>0) and (AParam[length(AParam)] = '}') then
          delete(AParam, length(AParam), 1);
        err := params.LoadFromVariablesAsString(AParam);
        if err <> [] then
          raise exception.Create('Error in parameter format: '+InterpretationErrorsToStr(err));
      end else
        raise exception.Create('Error in parameter format: dictionary not found');
    end;
    try
      scriptErr := FScriptContext.CallScriptFunction(params);
      if scriptErr = srOk then
      begin
        vRes := params.GetVariable('Result');
        if params.IsReferenceDefined(vRes) then
        begin
          case vRes.variableType of
          svtFloat, svtInteger, svtPoint, svtBoolean: AResult := params.GetString(vRes);
          svtString: AResult := ScriptQuote(params.GetString(vRes));
          svtPixel: AResult := '"'+BGRAToStr(params.GetPixel(vRes), nil,0,true)+'"';
          svtFloatList, svtIntList, svtPointList, svtBoolList, svtStrList: AResult := params.GetString(vRes);
          svtPixList: begin
              AResult := '[';
              for i := 0 to TVariableSet.GetListCount(vRes)-1 do
              begin
                if i > 0 then AResult += ', ';
                AResult += '"'+BGRAToStr(params.GetPixelAt(vRes, i), nil,0,true)+'"'
              end;
              AResult += ']';
            end;
          svtSubset: AResult := '{'+params.GetSubset(vRes).VariablesAsString+'}';
          end;
        end;
      end else
        raise exception.Create(ScriptResultToStr(scriptErr, ACommand)+' ('+ACommand+')');
    finally
      params.Free;
    end;
  end;
end;

function TLazPaintInstance.ScriptShowMessage(AVars: TVariableSet): TScriptResult;
begin
  ShowMessage(ExtractFileName(FScriptName), AVars.Strings['Message']);
  result := srOk;
end;

function TLazPaintInstance.ScriptInputBox(AVars: TVariableSet): TScriptResult;
var
  str: String;
begin
  str := AVars.Strings['Default'];
  if InputQuery(ExtractFileName(FScriptName), AVars.Strings['Prompt'], str) then
  begin
    AVars.Strings['Result'] := str;
    result := srOk;
  end else
    result := srCancelledByUser;
end;

procedure TLazPaintInstance.ToolQueryColorTarget(sender: TToolManager;
  ATarget: TVectorialFill);
begin
  if ATarget = ToolManager.ForeFill then
  begin
    if ToolManager.ForeFill.FillType = vftGradient then
      ChooseColorTarget := ctForeColorStartGrad
      else ChooseColorTarget := ctForeColorSolid;
  end else
  if ATarget = ToolManager.BackFill then
  begin
    if ToolManager.BackFill.FillType = vftGradient then
      ChooseColorTarget := ctBackColorStartGrad
      else ChooseColorTarget := ctBackColorSolid;
  end else
  if ATarget = ToolManager.OutlineFill then
  begin
    if ToolManager.OutlineFill.FillType = vftGradient then
      ChooseColorTarget := ctOutlineColorStartGrad
      else ChooseColorTarget := ctOutlineColorSolid;
  end;
end;

procedure TLazPaintInstance.OnLayeredBitmapLoadStartHandler(AFilenameUTF8: string);
begin
  if FLoadingLayers = nil then FLoadingLayers := TFLoading.Create(nil);
  if (AFilenameUTF8 = '<Stream>') and (FLoadingFilename <> '') then AFilenameUTF8 := FLoadingFilename;
  if Assigned(FMain) then FMain.UpdatingPopup:= true;
  try
    FLoadingLayers.ShowMessage(rsOpening+' ' +AFilenameUTF8+'...');
    UpdateWindows;
  finally
    if Assigned(FMain) then FMain.UpdatingPopup:= false;
  end;
end;

procedure TLazPaintInstance.OnLayeredBitmapLoadProgressHandler(
  APercentage: integer);
begin
  if FLoadingLayers <> nil then
  begin
    if Assigned(FMain) then FMain.UpdatingPopup:= true;
    try
      FLoadingLayers.ShowMessage(rsLoading+' (' +inttostr(APercentage)+'%)');
      UpdateWindows;
    finally
      if Assigned(FMain) then FMain.UpdatingPopup:= false;
    end;
  end;
end;

procedure TLazPaintInstance.OnLayeredBitmapLoadedHandler;
begin
  if FLoadingLayers <> nil then
  begin
    if Assigned(FMain) then FMain.UpdatingPopup:= true;
    try
      FreeAndNil(FLoadingLayers);
      UpdateWindows;
    finally
      if Assigned(FMain) then FMain.UpdatingPopup:= false;
    end;
  end;
end;

procedure TLazPaintInstance.OnLayeredBitmapSavedHandler();
begin
  if FLoadingLayers <> nil then
  begin
    if Assigned(FMain) then FMain.UpdatingPopup:= true;
    try
      FreeAndNil(FLoadingLayers);
      UpdateWindows;
    finally
      if Assigned(FMain) then FMain.UpdatingPopup:= false;
    end;
  end;
end;

procedure TLazPaintInstance.OnLayeredBitmapSaveProgressHandler(
  APercentage: integer);
begin
  if FLoadingLayers <> nil then
  begin
    if Assigned(FMain) then FMain.UpdatingPopup:= true;
    try
      FLoadingLayers.ShowMessage(rsSave+' (' +inttostr(APercentage)+'%)');
      UpdateWindows;
    finally
      if Assigned(FMain) then FMain.UpdatingPopup:= false;
    end;
  end;
end;

procedure TLazPaintInstance.OnLayeredBitmapSaveStartHandler(
  AFilenameUTF8: string);
begin
  if FLoadingLayers = nil then FLoadingLayers := TFLoading.Create(nil);
  if (AFilenameUTF8 = '<Stream>') and (FSavingFilename <> '') then AFilenameUTF8 := FSavingFilename;
  if Assigned(FMain) then FMain.UpdatingPopup:= true;
  try
    FLoadingLayers.ShowMessage(rsSave+' ' +AFilenameUTF8+'...');
    UpdateWindows;
  finally
    if Assigned(FMain) then FMain.UpdatingPopup:= false;
  end;
end;

procedure TLazPaintInstance.OnSizeChanged(Sender: TObject);
begin
  if FMain <> nil then FMain.Layout.InvalidatePicture(true);
end;

procedure TLazPaintInstance.PythonBusy(Sender: TObject);
begin
  Application.ProcessMessages;
end;

function TLazPaintInstance.GetShowSelectionNormal: boolean;
begin
  if FMain <> nil then result := fmain.ShowSelectionNormal
    else result := true;
end;

procedure TLazPaintInstance.SetShowSelectionNormal(AValue: boolean);
begin
  if FMain <> nil then fmain.ShowSelectionNormal:= AValue;
end;

function TLazPaintInstance.GetEmbedded: boolean;
begin
  Result:=FEmbedded;
end;

function TLazPaintInstance.GetLayerWindowVisible: boolean;
begin
  result := GetToolWindowVisible(FLayerStack, FLayerControlVisible);
end;

procedure TLazPaintInstance.SetLayerWindowVisible(AValue: boolean);
begin
  FLayerControlVisible := AValue;
  UpdateLayerControlVisibility;
end;

procedure TLazPaintInstance.OnFunctionException(AFunctionName: string;
  AException: Exception);
begin
  ShowError(AFunctionName,AException.Message);
end;

function TLazPaintInstance.GetMainFormBounds: TRect;
var workarea: TRect;
begin
  workarea := rect(Screen.WorkAreaLeft,Screen.WorkAreaTop,
    Screen.WorkAreaLeft+Screen.WorkAreaWidth,
    Screen.WorkAreaTop+Screen.WorkAreaHeight);
  result := workarea;
  if Assigned(FMain) then
  begin
    if not IntersectRect(result, workarea, FMain.BoundsRect) then
      result := workarea;
  end;
end;

procedure TLazPaintInstance.EditSelectionHandler(var AImage: TBGRABitmap);
begin
  if FSelectionEditConfig = nil then
    FSelectionEditConfig := TStringStream.Create('[Tool]'+LineEnding+
      'ForeColor=FFFFFFFF'+LineEnding+
      'BackColor=000000FF'+LineEnding+
      '[Window]'+LineEnding+
      'LayerWindowVisible=False'+LineEnding+
      'DockLayersAndColors='+BoolToStr(DockLayersAndColors, 'True', 'False')+LineEnding+
      '[General]'+LineEnding+
      'DarkTheme='+BoolToStr(DarkTheme, 'True', 'False')+LineEnding);
  EditBitmap(AImage, FSelectionEditConfig, rsEditSelection, @SelectionInstanceOnRun, nil, True);
end;

function TLazPaintInstance.GetZoomFactor: single;
begin
  if Assigned(FMain) and Assigned(FMain.Zoom) then
    Result:=FMain.Zoom.Factor else
      result := inherited GetZoomFactor;
end;

procedure TLazPaintInstance.UpdateLayerControlVisibility;
begin
  if FLayerStack <> nil then
  begin
    if DockLayersAndColors then
      FLayerStack.Visible := false
    else
    begin
      if FLayerStack.Visible and FLayerControlVisible then
        FLayerStack.BringToFront
        else FLayerStack.Visible := FLayerControlVisible;
    end;
  end;
  if FMain <> nil then
  begin
    if (FLayerControlVisible and DockLayersAndColors) and (FLayerStack.LayerStackControl.Parent = FLayerStack) then
    begin
      FLayerStack.LayerStackControl.Parent := nil;
      FLayerStack.LayerStackControl.Align := alNone;
      FLayerStack.LayerStackControl.Width := FLayerStack.ClientWidth;
      FLayerStack.LayerStackControl.Height := FLayerStack.ClientHeight;
      FMain.AddDockedControl(FLayerStack.LayerStackControl);
    end else
    if not (FLayerControlVisible and DockLayersAndColors) and (FLayerStack.LayerStackControl.Parent <> FLayerStack) then
    begin
      FMain.RemoveDockedControl(FLayerStack.LayerStackControl);
      FLayerStack.LayerStackControl.Align := alClient;
      FLayerStack.LayerStackControl.Parent := FLayerStack;
    end;
  end;
end;

procedure TLazPaintInstance.UpdateChooseColorControlVisibility;
begin
  if FChooseColor <> nil then
  begin
    if DockLayersAndColors then
      FChooseColor.Visible := false
    else
    begin
      if FChooseColor.Visible and FChooseColorControlVisible then
        FChooseColor.BringToFront
        else FChooseColor.Visible := FChooseColorControlVisible;
    end;
  end;
  if FMain <> nil then
  begin
    if (FChooseColorControlVisible and DockLayersAndColors) and (FChooseColor.ChooseColorControl.Parent = FChooseColor) then
    begin
      FChooseColor.ChooseColorControl.Parent := nil;
      FChooseColor.ChooseColorControl.Align := alNone;
      FChooseColor.ChooseColorControl.Width := FChooseColor.ClientWidth;
      FChooseColor.ChooseColorControl.Height := FChooseColor.ClientHeight;
      FMain.AddDockedControl(FChooseColor.ChooseColorControl);
    end else
    if not (FChooseColorControlVisible and DockLayersAndColors) and (FChooseColor.ChooseColorControl.Parent <> FChooseColor) then
    begin
      FMain.RemoveDockedControl(FChooseColor.ChooseColorControl);
      FChooseColor.ChooseColorControl.Align := alClient;
      FChooseColor.ChooseColorControl.Parent := FChooseColor;
    end;
  end;
end;

function TLazPaintInstance.GetGridVisible: boolean;
begin
  Result:= FGridVisible;
end;

procedure TLazPaintInstance.SetGridVisible(const AValue: boolean);
begin
  FGridVisible := AValue;
  Image.RenderMayChange(rect(0,0,Image.Width,Image.Height),True);
end;

function TLazPaintInstance.GetChooseColorVisible: boolean;
begin
  result := GetToolWindowVisible(FChooseColor, FChooseColorControlVisible);
end;

function TLazPaintInstance.GetToolboxVisible: boolean;
begin
  Result:= GetToolWindowVisible(FFormToolbox) or
           ((FMain <> nil) and not (FMain.Layout.ToolBoxDocking in [twNone,twWindow]));
end;

function TLazPaintInstance.GetImageListWindowVisible: boolean;
begin
  result := GetToolWindowVisible(FImageList);
end;

procedure TLazPaintInstance.SetChooseColorVisible(const AValue: boolean);
begin
  FChooseColorControlVisible:= AValue;
  UpdateChooseColorControlVisibility;
end;

procedure TLazPaintInstance.SetToolBoxVisible(const AValue: boolean);
var winVisible: boolean;
begin
  if FInSetToolboxVisible then exit;
  FInSetToolboxVisible := true;
  if Assigned(FMain) then
  begin
    FMain.Layout.ToolBoxVisible := AValue;
    winVisible := (FMain.Layout.ToolBoxDocking = twWindow);
  end else
    winVisible := AValue;

  if winVisible and not Assigned(FFormToolbox) then CreateToolBox;
  if Assigned(FFormToolbox) then FFormToolbox.Visible := winVisible;

  FInSetToolboxVisible := false;
end;

procedure TLazPaintInstance.SetImageListWindowVisible(const AValue: boolean);
begin
  if FImageList <> nil then
    FImageList.Visible := AValue;
end;

function TLazPaintInstance.GetChooseColorHeight: integer;
begin
  Result:= FChooseColor.Height;
end;

function TLazPaintInstance.GetChooseColorWidth: integer;
begin
  Result:= FChooseColor.Width;
end;

procedure TLazPaintInstance.SetChooseColorHeight(AValue: integer);
begin
  if FChooseColor <> nil then
  begin
    FChooseColor.Height := AValue;
    FChooseColor.ChooseColorControl.Height := AValue;
  end;
end;

procedure TLazPaintInstance.SetChooseColorWidth(AValue: integer);
begin
  if FChooseColor <> nil then
  begin
    FChooseColor.Width := AValue;
    FChooseColor.ChooseColorControl.Width := AValue;
  end;
end;

procedure TLazPaintInstance.AssignBitmap(bmp: TBGRABitmap);
begin
  if Assigned(FImageAction) then
    FImageAction.SetCurrentBitmap(bmp.Duplicate as TBGRABitmap, False);
end;

procedure TLazPaintInstance.EditBitmap(var bmp: TBGRABitmap; ConfigStream: TStream; ATitle: String; AOnRun: TLazPaintInstanceEvent; AOnExit: TLazPaintInstanceEvent; ABlackAndWhite: boolean);
var
  subLaz: TLazPaintInstance;
  ini : TIniFile;
  topmostInfo: TTopMostInfo;

begin
  try
    subLaz := TLazPaintInstance.Create(True);
  except
    on ex:Exception do
    begin
      ShowError('EditBitmap',ex.Message);
      exit;
    end;
  end;
  subLaz.BlackAndWhite := ABlackAndWhite;
  if ATitle <> '' then subLaz.Title := ATitle;
  if FMain <> nil then FMain.Enabled := false;
  topmostInfo:= HideTopmost;
  try
    if ConfigStream <> nil then
    begin
      ConfigStream.Position := 0;
      ini := TInifile.Create(ConfigStream);
      ini.CacheUpdates := True;
      subLaz.UseConfig(ini);
    end;
    subLaz.FormsNeeded;
    if bmp <> nil then subLaz.AssignBitmap(bmp);
    subLaz.AboutText := AboutText;
    subLaz.EmbeddedImageBackup := bmp;
    if AOnRun <> nil then
      AOnRun(subLaz);
    subLaz.Run;
    if AOnExit <> nil then
      AOnExit(subLaz);
    if subLaz.EmbeddedResult = mrOk then
    begin
      FreeAndNil(bmp);
      bmp := subLaz.Image.RenderedImage.Duplicate as TBGRABitmap;
    end;
    if ConfigStream <> nil then
    begin
      ConfigStream.Position := 0;
      ConfigStream.Size := 0;
    end;
  except
    on ex:Exception do
      ShowError('EditBitmap',ex.Message);
  end;
  ShowTopmost(topmostInfo);
  if FMain <> nil then FMain.Enabled := true;
  subLaz.Free;
end;

procedure TLazPaintInstance.EditSelection;
begin
  try
    TImageActions(ImageAction).EditSelection(@EditSelectionHandler);
  except
    on ex: Exception do
      ShowError('EditSelection',ex.Message);
  end;
end;

function TLazPaintInstance.EditTexture(ASource: TBGRABitmap): TBGRABitmap;
begin
  try
    if FTextureEditConfig = nil then
      FTextureEditConfig := TStringStream.Create('[General]'+LineEnding+
        'DefaultImageWidth=256'+LineEnding+
        'DefaultImageHeight=256'+LineEnding);
    result := ASource.Duplicate as TBGRABitmap;
    try
      EditBitmap(result,FTextureEditConfig,rsEditTexture,nil,nil,BlackAndWhite);
    finally
      if result.Equals(ASource) then FreeAndNil(result);
    end;
  except
    on ex: Exception do
      ShowError('EditTexture',ex.Message);
  end;
end;

procedure TLazPaintInstance.SelectionInstanceOnRun(AInstance: TLazPaintCustomInstance);
begin
  AInstance.Config.SetDefaultImageWidth(Image.Width);
  AInstance.Config.SetDefaultImageHeight(Image.Height);
end;

procedure TLazPaintInstance.ToolFillChanged(Sender: TObject);
begin
  ColorToFChooseColor;
  if Assigned(FMain) then FMain.UpdateFillToolbar(false);
end;

function TLazPaintInstance.GetIcons(ASize: integer): TImageList;

  function GetUnscaledIcons(ASize: integer): TImageList;
  begin
    if ASize < 24 then
    begin;
      if ASize = 16 then
      begin
        result := TImageList.Create(nil);
        result.Assign(FMain.ImageList16);
      end
      else
      begin
        result := TImageList.Create(nil);
        ScaleImageList(FMain.ImageList16, ASize,ASize, result);
      end;
    end
    else
    begin
      if ASize = 48 then
      begin
        result := TImageList.Create(nil);
        result.Assign(FMain.ImageList48);
      end
      else
      begin
        result := TImageList.Create(nil);
        ScaleImageList(FMain.ImageList48, ASize,ASize, result);
      end;
    end;
  end;

var
  i: Integer;
  {$IFDEF DARWIN}
  retina, unscaled: TImageList;
  bmpUnscaled, bmpRetina: TBitmap;
  {$ENDIF}

begin
  if Assigned(FMain) then
  begin
    for i := 0 to FCustomImageList.Count-1 do
      if FCustomImageList[i].Height = ASize then
        exit(FCustomImageList[i]);

    {$IFDEF DARWIN}
    unscaled := GetUnscaledIcons(ASize);
    retina := GetUnscaledIcons(ASize*2);
    bmpUnscaled := TBitmap.Create;
    bmpRetina := TBitmap.Create;

    result := TImageList.Create(nil);
    result.Width := ASize;
    result.Height := ASize;
    result.Scaled := true;
    result.RegisterResolutions([ASize, ASize*2]);
    for i := 0 to unscaled.Count-1 do
    begin
      unscaled.GetBitmap(i, bmpUnscaled);
      retina.GetBitmap(i, bmpRetina);
      result.AddMultipleResolutions([bmpUnscaled, bmpRetina]);
    end;

    bmpUnscaled.Free;
    bmpRetina.Free;
    unscaled.Free;
    retina.Free;
    {$ELSE}
    if ASize = 16 then
    begin
      result := FMain.ImageList16;
      exit
    end else
    if ASize = 48 then
    begin
      result := FMain.ImageList48;
      exit;
    end else
      result := GetUnscaledIcons(ASize);
    {$ENDIF}
    FCustomImageList.Add(result);
  end else
    result := nil;
end;

function TLazPaintInstance.GetToolBoxWindowPopup: TPopupMenu;
begin
  if Assigned(FFormToolbox) then
    result := FFormToolbox.PopupMenu
  else
    result := FFormToolboxInitialPopup;
end;

procedure TLazPaintInstance.SetToolBoxWindowPopup(AValue: TPopupMenu);
begin
  if Assigned(FFormToolbox) then
    FFormToolbox.PopupMenu := AValue
  else
    FFormToolboxInitialPopup := AValue;
end;

function TLazPaintInstance.GetFullscreen: boolean;
begin
  result := FFullscreen;
end;

procedure TLazPaintInstance.SetFullscreen(AValue: boolean);
begin
  if (AValue = FFullscreen) or not MainFormVisible or (FMain.WindowState = wsMinimized) then exit;
  FFullscreen := AValue;
  if AValue then
  begin
    SaveMainWindowPosition;
    FMain.BorderStyle:= bsNone;
    FMain.WindowState:= wsFullScreen;
  end else
  begin
    FMain.BorderStyle := bsSizeable;
    FMain.WindowState:= wsNormal;
    RestoreMainWindowPosition;
  end;
end;

function TLazPaintInstance.GetToolWindowVisible(AWindow: TForm; ADockedVisible: boolean = false): boolean;
begin
  if Assigned(AWindow) and AWindow.Visible then
  begin
    result := not ((AWindow.FormStyle <> fsStayOnTop) and (AWindow.BorderStyle <> bsDialog) and
                   Assigned(FMain) and FMain.Active and
                   FMain.BoundsRect.Contains(AWindow.BoundsRect));
  end else
    result := ADockedVisible;
end;

function TLazPaintInstance.GetDockLayersAndColors: boolean;
begin
  result := FDockLayersAndColors;
end;

procedure TLazPaintInstance.SetDockLayersAndColors(AValue: boolean);
begin
  if FDockLayersAndColors= AValue then exit;
  FDockLayersAndColors:= AValue;
  UpdateLayerControlVisibility;
  UpdateChooseColorControlVisibility;
  if Assigned(FMain) and FMain.Visible then FMain.QueryArrange;
end;

function TLazPaintInstance.GetScriptContext: TScriptContext;
begin
  result := FScriptContext;
end;

function TLazPaintInstance.ProcessCommandLine: boolean;
var commands: TStringList;
    error, saved, quitQuery: boolean;
    i: Integer;
begin
  if paramCount = 0 then
  begin
    result := false;
    exit;
  end;

  FormsNeeded;
  FInCommandLine := true;
  commands := TStringList.Create;
  try
    for i := 1 to paramCount do
      commands.Add(ParamStrUtf8(i));
    ucommandline.ProcessCommands(self, commands, error, saved, quitQuery);
  finally
    commands.free;
    FInCommandLine := false;
  end;
  result := error or saved or quitQuery;
end;

function TLazPaintInstance.ProcessCommands(commands: TStringList): boolean;
var saved, quitQuery: boolean;
begin
  quitQuery := false;
  if paramCount = 0 then
  begin
    result := true;
    exit;
  end;
  FormsNeeded;
  ucommandline.ProcessCommands(self, commands, result, saved, quitQuery);
end;

procedure TLazPaintInstance.ChangeIconSize(size: integer);
begin
  if Config.DefaultIconSize(0)<>size then
  begin
    Config.SetDefaultIconSize(size);
    Restart;
  end;
end;

procedure TLazPaintInstance.Show;
begin
  EmbeddedResult := mrNone;
  FormsNeeded;
  FMain.Show;
end;

function TLazPaintInstance.Hide: boolean;
begin
  if MainFormVisible then
  begin
    FMain.Hide;
    result := true;
  end
  else result := false;
end;

procedure TLazPaintInstance.Run;
begin
  if not MainFormVisible then Show;
  repeat
    application.ProcessMessages;
    Sleep(10);
  until not MainFormVisible;
end;

procedure TLazPaintInstance.Restart;
begin
  if FMain <> nil then
  begin
    FRestartQuery := true;
    FMain.Close;
  end;
end;

procedure TLazPaintInstance.CancelRestart;
begin
  FRestartQuery := false;
end;

destructor TLazPaintInstance.Destroy;
begin
  FreeAndNil(FImageAction);
  RegisterScripts(False);

  FDestroying := true;

  Config.SetDefaultDockLayersAndColors(FDockLayersAndColors);
  Config.SetDefaultGridVisible(FGridVisible);
  if (FChooseColor <> nil) and FChooseColorPositionDefined then
  begin
    Config.SetDefaultColorWindowVisible(ChooseColorVisible or (FTopMostInfo.choosecolorHidden > 0));
    Config.SetDefaultColorWindowPosition(FChooseColor.BoundsRect);
  end;
  if (FLayerStack <> nil) and FLayerStackPositionDefined then
  begin
    Config.SetDefaultLayerWindowVisible(LayerWindowVisible or (FTopMostInfo.layerstackHidden > 0));
    Config.SetDefaultLayerWindowPosition(FLayerStack.BoundsRect);
    Config.SetDefaultLayerStackZoom(FLayerStack.ZoomFactor);
  end;
  if (FImageList <> nil) and FImageListPositionDefined then
  begin
    Config.SetDefaultImagelistWindowVisible (ImageListWindowVisible or (FTopMostInfo.imagelistHidden > 0));
    Config.SetDefaultImagelistWindowPosition(FImageList.BoundsRect);
  end;
  if (FFormToolbox <> nil) and FToolBoxPositionDefined then
  begin
    Config.SetDefaultToolboxWindowVisible(ToolboxVisible or (FTopMostInfo.toolboxHidden > 0));
    Config.SetDefaultToolboxWindowPosition(FFormToolbox.BoundsRect);
  end;
  ToolManager.SaveToConfig;

  BGRALayers.UnregisterLoadingHandler(@OnLayeredBitmapLoadStartHandler,@OnLayeredBitmapLoadProgressHandler,@OnLayeredBitmapLoadedHandler);
  BGRALayers.UnregisterLoadingHandler(@OnLayeredBitmapSaveStartHandler,@OnLayeredBitmapSaveProgressHandler,@OnLayeredBitmapSavedHandler);
  if FLoadingLayers <> nil then FreeAndNil(FLoadingLayers);

  FreeAndNil(FLayerStack);
  FreeAndNil(FFormCustomBlur);
  FreeAndNil(FFormColorize);
  FreeAndNil(FFormAdjustCurves);
  FreeAndNil(FFormShiftColors);
  FreeAndNil(FFormColorIntensity);
  FreeAndNil(FFormCanvasSize);
  FreeAndNil(FChooseColor);
  FreeAndNil(FFormToolbox);
  FreeAndNil(FToolManager);
  FreeAndNil(FMain);
  FreeAndNil(FImage);
  FreeAndNil(FConfig);
  FreeAndNil(FSelectionEditConfig);
  FreeAndNil(FTextureEditConfig);
  //MessageDlg(FScriptContext.RecordedScript,mtInformation,[mbOk],0);
  FreeAndNil(FScriptContext);
  FreeAndNil(FImageList);
  FreeAndNil(FCustomImageList);
  FreeAndNil(FThemeListeners);
  inherited Destroy;
end;

function TLazPaintInstance.HideTopmost: TTopMostInfo;
begin
  result.defined:= false;
  if FDestroying then exit;
  ExitColorEditor;

  if (FFormToolbox <> nil) and FFormToolbox.Visible then
  begin
    FFormToolbox.Hide;
    result.toolboxHidden := 1;
  end else
    result.toolboxHidden := 0;
  if (FChooseColor <> nil) and FChooseColor.Visible then
  begin
    FChooseColor.Hide;
    result.choosecolorHidden := 1;
  end else
    result.choosecolorHidden := 0;
  if (FLayerStack <> nil) and FLayerStack.Visible then
  begin
    FLayerStack.Hide;
    result.layerstackHidden := 1;
  end else
    result.layerstackHidden := 0;
  if (FImageList <> nil) and FImageList.Visible then
  begin
    FImageList.Hide;
    result.imagelistHidden := 1;
  end else
     result.imagelistHidden := 0;
  Inc(FTopMostInfo.toolboxHidden, result.toolboxHidden);
  Inc(FTopMostInfo.choosecolorHidden, result.choosecolorHidden);
  Inc(FTopMostInfo.layerstackHidden, result.layerstackHidden);
  Inc(FTopMostInfo.imagelistHidden, result.imagelistHidden);
  result.defined:= true;
end;

procedure TLazPaintInstance.ShowTopmost(AInfo: TTopMostInfo);
begin
  if FDestroying or not AInfo.defined then exit;

  if assigned(FImageList) and (AInfo.imagelistHidden > 0) then
  begin
    FImageList.Show;
    dec(FTopMostInfo.imagelistHidden);
  end;
  if Assigned(FLayerStack) and (AInfo.layerstackHidden > 0) then
  begin
    FLayerStack.Show;
    FLayerStack.BringToFront;
    FLayerStack.InvalidateStack(False);
    dec(FTopMostInfo.layerstackHidden);
  end;
  if Assigned(FChooseColor) and (AInfo.choosecolorHidden > 0) then
  begin
    FChooseColor.Show;
    FChooseColor.BringToFront;
    dec(FTopMostInfo.choosecolorHidden);
  end;
  if Assigned(FFormToolbox) and (AInfo.toolboxHidden > 0) then
  begin
    FFormToolbox.Show;
    FFormToolbox.BringToFront;
    dec(FTopMostInfo.toolboxHidden);
  end;
end;

procedure TLazPaintInstance.UpdateWindows;
begin
  if Assigned(FMain) then FMain.Enabled:= false;
  if Assigned(FFormToolbox) then FFormToolbox.Enabled:= false;
  if Assigned(FChooseColor) then FChooseColor.Enabled:= false;
  if Assigned(FLayerStack) then FLayerStack.Enabled:= false;
  if Assigned(FImageList) then FImageList.Enabled:= false;
  Application.ProcessMessages;
  if Assigned(FMain) then FMain.Enabled:= true;
  if Assigned(FFormToolbox) then FFormToolbox.Enabled:= true;
  if Assigned(FChooseColor) then FChooseColor.Enabled:= true;
  if Assigned(FLayerStack) then FLayerStack.Enabled:= true;
  if Assigned(FImageList) then FImageList.Enabled:= true;
end;

procedure TLazPaintInstance.Wait(ACheckActive: TCheckFunction; ADelayMs: integer);
var
  tmi: TTopMostInfo;
  wasEnabled: Boolean;
begin
  tmi := HideTopmost;
  if Assigned(FMain) then
  begin
    wasEnabled := FMain.Enabled;
    FMain.Enabled:= false;
  end
  else wasEnabled := false;
  try
    repeat
      Application.ProcessMessages;
      sleep(ADelayMs);
    until not ACheckActive();
  finally
    if Assigned(FMain) then
      FMain.Enabled := wasEnabled;
    ShowTopmost(tmi);
  end;
end;

procedure TLazPaintInstance.NotifyImageChange(RepaintNow: boolean; ARect: TRect);
begin
  FormsNeeded;
  Image.ImageMayChange(ARect);
  Image.OnImageChanged.NotifyObservers;
  If RepaintNow then FMain.Update;
end;

procedure TLazPaintInstance.NotifyImageChangeCompletely(RepaintNow: boolean);
begin
  FormsNeeded;
  Image.ImageMayChangeCompletely;
  If RepaintNow then FMain.Update;
end;

procedure TLazPaintInstance.NotifyImagePaint;
begin
  if Assigned(FMain) then
    FMain.NotifyImagePaint;
end;

function TLazPaintInstance.TryOpenFileUTF8(filename: string; skipDialogIfSingleImage: boolean): boolean;
begin
  FormsNeeded;
  result := FMain.TryOpenFileUTF8(filename, true, nil, skipDialogIfSingleImage);
end;

function TLazPaintInstance.ExecuteFilter(filter: TPictureFilter;
  skipDialog: boolean): TScriptResult;
var vars: TVariableSet;
begin
  if filter = pfNone then
  begin
    result := srInvalidParameters;
    exit;
  end;
  vars := TVariableSet.Create('Filter');
  vars.AddString('Name',PictureFilterStr[filter]);
  Result:= UFilters.ExecuteFilter(self, filter, vars, skipDialog);
  vars.Free;
end;

function TLazPaintInstance.RunScript(AFilename: string; ACaption: string): boolean;
var
  p: TPythonScript;
  fError: TForm;
  memo: TMemo;
  doFound, somethingDone: boolean;
  tmi: TTopMostInfo;
  i: Integer;
begin
  if FInRunScript then exit;
  p := nil;

  if ToolManager.TextShadow then
  begin
    //text shadow will be replaced in the future so do not allow it
    if ToolManager.ToolProvideCommand(tcFinish) then
      ToolManager.ToolCommand(tcFinish);
    ToolManager.TextShadow := false;
  end;

  tmi := HideTopmost;
  if Assigned(FMain) then FMain.Enabled:= false;
  FInRunScript := true;
  try
    FScriptTempFileNames := TStringList.Create;
    p := TPythonScript.Create;
    if Trim(ACaption).Length > 0 then
      FScriptName:= Trim(ACaption)
      else FScriptName := AFilename;
    p.OnCommand:=@PythonScriptCommand;
    p.OnBusy := @PythonBusy;
    p.Run(AFilename);
    if p.ErrorText<>'' then
    begin
      fError := TForm.Create(nil);
      try
        fError.Caption := ChangeFileExt(ExtractFileName(AFilename),'');
        fError.Position:= poDesktopCenter;
        fError.Width := Screen.Width*3 div 4;
        fError.Height := Screen.Height*3 div 4;
        memo := TMemo.Create(fError);
        memo.Align:= alClient;
        memo.Parent := fError;
        memo.ScrollBars := ssVertical;
        memo.Font.Name:= 'monospace';
        memo.Text := p.ErrorText;
        fError.ShowModal;
      finally
        fError.Free;
      end;
      result := false;
    end else
      result := true;
  except
    on ex:exception do
    begin
      ShowError(ChangeFileExt(ExtractFileName(AFilename),''), ex.Message);
      result := false;
    end;
  end;
  FInRunScript := false;
  FScriptName := '';
  try
    for i := 0 to FScriptTempFileNames.Count-1 do
      if FileExistsUTF8(FScriptTempFileNames[i]) then
        DeleteFileUTF8(FScriptTempFileNames[i]);
  except
    on ex:exception do
    begin
      ShowError(ChangeFileExt(ExtractFileName(AFilename),''), ex.Message);
      result := false;
    end;
  end;
  FScriptTempFileNames.Free;
  p.Free;
  if Assigned(FMain) then FMain.Enabled:= true;
  ShowTopmost(tmi);
  //ensure we are out of any do group
  repeat
    Image.DoEnd(doFound, somethingDone);
  until not doFound;
end;

procedure TLazPaintInstance.AdjustChooseColorHeight;
begin
  if Assigned(FChooseColor) then
    FChooseColor.AdjustControlHeight;
end;

procedure TLazPaintInstance.ColorFromFChooseColor;
begin
  FormsNeeded;
  if InColorFromFChooseColor then exit;
  InColorFromFChooseColor := True;
  SetColor(FChooseColor.ColorTarget, FChooseColor.GetCurrentColor);
  InColorFromFChooseColor := false;
end;

procedure TLazPaintInstance.ColorToFChooseColor;
begin
  if not Assigned(FChooseColor) or InColorFromFChooseColor then exit;
  FChooseColor.SetCurrentColor(GetColor(FChooseColor.ColorTarget));
end;

procedure TLazPaintInstance.ExitColorEditor;
begin
  if Assigned(FChooseColor) then FChooseColor.HideEditor;
end;

function TLazPaintInstance.ColorEditorActive: boolean;
begin
  if Assigned(FChooseColor) then
    result := FChooseColor.EditorVisible
    else result := false;
end;

function TLazPaintInstance.ShowSaveOptionDlg(AParameters: TVariableSet;
  AOutputFilenameUTF8: string; ASkipOptions: boolean; AExport: boolean): boolean;
begin
  result := USaveOption.ShowSaveOptionDialog(self, AOutputFilenameUTF8, ASkipOptions, AExport);
end;

procedure TLazPaintInstance.MoveToolboxTo(X, Y: integer);
begin
  if Assigned(FFormToolbox) then
  begin
    FFormToolbox.Left := X;
    FFormToolbox.Top := Y;
  end else
    FFormToolboxInitialPosition := Point(X, Y);
  FToolBoxPositionDefined := true;
end;

procedure TLazPaintInstance.MoveChooseColorTo(X, Y: integer);
begin
  FormsNeeded;
  FChooseColor.Left := X;
  FChooseColor.Top := Y;
  FChooseColorPositionDefined := true;
end;

procedure TLazPaintInstance.MoveLayerWindowTo(X, Y: integer);
begin
  if FLayerStack <> nil then
  begin
    FLayerStack.Left := X;
    FLayerStack.Top := Y;
    if IsRectEmpty(Config.DefaultLayerWindowPosition) then
      Config.SetDefaultLayerWindowPosition(FLayerStack.BoundsRect);
  end;
  FLayerStackPositionDefined := true;
end;

procedure TLazPaintInstance.MoveImageListWindowTo(X, Y: integer);
begin
  FormsNeeded;
  FImageList.Left := X;
  FImageList.Top := Y;
  FImageListPositionDefined := true;
end;

procedure TLazPaintInstance.ImageListWindowVisibleKeyDown(var Key: Word;
  Shift: TShiftState);
begin
  if FImageList <> nil then
    FImageList.FormKeyDown(nil,Key,Shift);
end;

procedure TLazPaintInstance.ShowAboutDlg;
var tmi: TTopMostInfo;
begin
  tmi := HideTopmost;
  uabout.ShowAboutDlg(self,AboutText);
  ShowTopmost(tmi);
end;

procedure TLazPaintInstance.NotifyStackChange;
begin
  OnStackChanged(image,False);
end;

procedure TLazPaintInstance.ScrollLayerStackOnItem(AIndex: integer; ADelayedUpdate: boolean);
begin
  if FLayerStack<> nil then
  begin
    if not Assigned(FMain) then ADelayedUpdate:= false;
    FLayerStack.ScrollToItem(AIndex, not ADelayedUpdate);
    if ADelayedUpdate then UpdateStackOnTimer := true;
  end;
end;

procedure TLazPaintInstance.InvalidateLayerStack;
begin
  if FLayerStack<> nil then
    FLayerStack.InvalidateStack(false);
end;

procedure TLazPaintInstance.UpdateLayerStackOnTimer;
begin
  UpdateStackOnTimer := true;
end;

function TLazPaintInstance.MakeNewBitmapReplacement(AWidth, AHeight: integer; AColor: TBGRAPixel): TBGRABitmap;
begin
  result := TBGRABitmap.Create(AWidth,AHeight, AColor);
end;

procedure TLazPaintInstance.ChooseTool(Tool: TPaintToolType);
begin
  FormsNeeded;
  if Assigned(FMain) then FMain.ChooseTool(Tool);
end;

function TLazPaintInstance.GetToolboxHeight: integer;
begin
  if Assigned(FFormToolbox) then
    Result:= FFormToolbox.Height
  else
  begin
    Result := DoScaleY(99, OriginalDPI);
    if Assigned(FMain) then
      Inc(result, FMain.Height-FMain.ClientHeight);
  end;
end;

function TLazPaintInstance.GetToolboxWidth: integer;
begin
  if Assigned(FFormToolbox) then
    Result:= FFormToolbox.Width else
  begin
    Result := DoScaleX(143, OriginalDPI);
    if Assigned(FMain) then
      Inc(result, FMain.Width-FMain.ClientWidth);
  end;
end;

function TLazPaintInstance.GetTopMostHasFocus: boolean;
begin
  if FDestroying then
  begin
    result := false;
    exit;
  end;

  result := false;
  if (FFormToolbox <> nil) and FFormToolbox.Visible and FFormToolbox.Active then
    result := true;
  if (FChooseColor <> nil) and FChooseColor.Visible and FChooseColor.Active then
    result := true;
  if (FLayerStack <> nil) and FLayerStack.Visible and FLayerStack.Active then
    result := true;
  if (FImageList <> nil) and FImageList.Visible and FImageList.Active then
    result := true;
end;

function TLazPaintInstance.GetTopMostVisible: boolean;
begin
  if FDestroying then
  begin
    result := false;
    exit;
  end;
  FormsNeeded;
  result := (Assigned(FFormToolbox) and FFormToolbox.Visible) or
            (Assigned(FChooseColor) and FChooseColor.Visible) or
            (Assigned(FLayerStack) and FLayerStack.Visible) or
            (Assigned(FImageList) and FImageList.Visible);
end;

function TLazPaintInstance.GetTopMostOkToUnfocus: boolean;
begin
  if FChooseColor.Active and FChooseColor.EditorVisible then
    result := false
  else
    result := true;
end;

function TLazPaintInstance.GetChooseColorTarget: TColorTarget;
begin
  if Assigned(FChooseColor) then
    Result:= FChooseColor.ColorTarget
  else
    result := ctForeColorSolid;
end;

procedure TLazPaintInstance.SetChooseColorTarget(const AValue: TColorTarget);
begin
  if not Assigned(FChooseColor) then exit;
  FChooseColor.ColorTarget:= AValue;
  if Assigned(FMain) then
  begin
    FMain.VectorialFill_Pen.IsTarget := AValue in [ctForeColorSolid..ctForeColorEndGrad];
    FMain.VectorialFill_Back.IsTarget := AValue in [ctBackColorSolid..ctBackColorEndGrad];
    FMain.VectorialFill_Outline.IsTarget := AValue in [ctOutlineColorSolid..ctOutlineColorEndGrad];
  end;
  ColorToFChooseColor;
end;

function TLazPaintInstance.OpenImage (FileName: string; AddToRecent: Boolean=True): boolean;
begin
  FormsNeeded;
  Result:= FMain.TryOpenFileUTF8(FileName, AddToRecent);
end;

procedure TLazPaintInstance.AddToImageList(const FileNames: array of String);
begin
  if FImageList <> nil then
    FImageList.AddFiles (FileNames, true);
end;

procedure TLazPaintInstance.UpdateToolbar;
begin
  if Assigned(FMain) then FMain.UpdateToolbar;
end;

procedure TLazPaintInstance.UpdateEditPicture(ADelayed: boolean);
begin
  if Assigned(FMain) then FMain.UpdateEditPicture(ADelayed);
end;

procedure TLazPaintInstance.AddColorToPalette(AColor: TBGRAPixel);
begin
  if Assigned(FMain) then FMain.Layout.AddColorToPalette(AColor);
end;

procedure TLazPaintInstance.RemoveColorFromPalette(AColor: TBGRAPixel);
begin
  if Assigned(FMain) then FMain.Layout.RemoveColorFromPalette(AColor);
end;

end.

