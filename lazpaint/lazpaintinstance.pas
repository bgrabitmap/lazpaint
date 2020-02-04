unit LazpaintInstance;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazPaintType, BGRABitmap, BGRABitmapTypes, BGRALayers,
  Menus, Controls, fgl,

  LazPaintMainForm, UMainFormLayout,

  utoolbox, uchoosecolor, ulayerstack, UCanvassize,
  ucolorintensity, ushiftcolors, ucolorize, uadjustcurves,
  ucustomblur, uimagelist,

  ULoading, UImage, UTool, uconfig, IniFiles, uresourcestrings, uscripting,
  UScriptType;

const
  MaxToolPopupShowCount = 3;

type
  TImageListList = specialize TFPGObjectList<TImageList>;

  { TLazPaintInstance }

  TLazPaintInstance = class(TLazPaintCustomInstance)
  private
    FScriptName: String;

    function GetInitialized: boolean;
    function GetMainFormVisible: boolean;
    procedure OnLayeredBitmapLoadStartHandler(AFilenameUTF8: string);
    procedure OnLayeredBitmapLoadProgressHandler(APercentage: integer);
    procedure OnLayeredBitmapLoadedHandler;
    procedure RegisterScripts(ARegister: Boolean);
    function ScriptColorColorize(AVars: TVariableSet): TScriptResult;
    function ScriptColorCurves(AVars: TVariableSet): TScriptResult;
    function ScriptColorIntensity(AVars: TVariableSet): TScriptResult;
    function ScriptColorLightness(AVars: TVariableSet): TScriptResult;
    function ScriptColorPosterize(AVars: TVariableSet): TScriptResult;
    function ScriptColorShiftColors(AVars: TVariableSet): TScriptResult;
    function ScriptFileNew(AVars: TVariableSet): TScriptResult;
    function ScriptImageCanvasSize(AVars: TVariableSet): TScriptResult;
    function ScriptImageRepeat(AVars: TVariableSet): TScriptResult;
    function ScriptImageResample(AParams: TVariableSet): TScriptResult;
    procedure SelectionInstanceOnRun(AInstance: TLazPaintCustomInstance);
    procedure ToolFillChanged(Sender: TObject);
    procedure PythonScriptCommand({%H-}ASender: TObject; ACommand, AParam: UTF8String; out
      AResult: UTF8String);
    procedure PythonBusy({%H-}Sender: TObject);
    function ScriptShowMessage(AVars: TVariableSet): TScriptResult;
    function ScriptInputBox(AVars: TVariableSet): TScriptResult;

  protected
    InColorFromFChooseColor: boolean;
    FMain: TFMain;
    FToolbox: TFToolbox;
    FImageList: TFImageList;
    FChooseColor: TFChooseColor;
    FLayerStack: TFLayerStack;
    FCanvasSize: TFCanvasSize;
    FColorIntensity: TFColorIntensity;
    FShiftColors: TFShiftColors;
    FColorize: TFColorize;
    FColorCurves: TFAdjustCurves;
    FCustomBlur: TFCustomBlur;
    FLoadingLayers: TFLoading;
    FTopMostInfo: TTopMostInfo;
    FGridVisible: boolean;
    FConfig: TLazPaintConfig;
    FImage: TLazPaintImage;
    FToolManager : TToolManager;
    FEmbedded: boolean;
    FDestroying: boolean;
    FSelectionEditConfig: TStream;
    FTextureEditConfig: TStream;
    FScriptContext: TScriptContext;
    FInFormsNeeded: boolean;
    FDockLayersAndColors, FFullscreen: boolean;
    FPrevDockArea: TRect;
    FInSetToolboxVisible: boolean;
    FToolBoxPositionDefined,
    FChooseColorPositionDefined,
    FLayerStackPositionDefined,
    FImageListPositionDefined : boolean;
    FCustomImageList: TImageListList;

    function GetIcons(ASize: integer): TImageList; override;
    function GetToolBoxWindowPopup: TPopupMenu; override;
    procedure SetToolBoxWindowPopup(AValue: TPopupMenu); override;
    function GetFullscreen: boolean; override;
    procedure SetFullscreen(AValue: boolean); override;
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
    function GetToolboxHeight: integer; override;
    function GetToolboxWidth: integer; override;
    function GetTopMostHasFocus: boolean; override;
    function GetTopMostVisible: boolean; override;
    function GetTopMostOkToUnfocus: boolean; override;
    function GetChooseColorTarget: TColorTarget; override;
    procedure SetChooseColorTarget(const AValue: TColorTarget); override;
    function GetConfig: TLazPaintConfig; override;
    function GetImage: TLazPaintImage; override;
    function GetToolManager: TToolManager; override;
    procedure CreateLayerStack;
    procedure CreateToolBox;
    procedure FormsNeeded;
    procedure Init(AEmbedded: boolean);
    procedure SetBlackAndWhite(AValue: boolean); override;
    procedure OnStackChanged({%H-}sender: TLazPaintImage; AScrollIntoView: boolean);
    procedure OnToolPopup({%H-}sender: TToolManager; AMessage: TToolPopupMessage; AKey: Word);

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
    procedure ApplyTheme(ADarkTheme: boolean); override;

  public
    constructor Create; override;
    constructor Create(AEmbedded: boolean); override;
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
    procedure Hide; override;
    procedure Run; override;
    procedure Restart; override;
    procedure CancelRestart; override;
    destructor Destroy; override;
    procedure NotifyImageChange(RepaintNow: boolean; ARect: TRect); override;
    procedure NotifyImageChangeCompletely(RepaintNow: boolean); override;
    function TryOpenFileUTF8(filename: string; skipDialogIfSingleImage: boolean = false): boolean; override;
    function ExecuteFilter(filter: TPictureFilter; skipDialog: boolean = false): TScriptResult; override;
    function RunScript(AFilename: string): boolean; override;
    procedure ColorFromFChooseColor; override;
    procedure ColorToFChooseColor; override;
    function ShowSaveOptionDlg({%H-}AParameters: TVariableSet; AOutputFilenameUTF8: string; ASkipOptions: boolean): boolean; override;
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
    procedure ApplyDocking; override;
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
    procedure ScrollLayerStackOnItem(AIndex: integer); override;
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

uses LCLType, Types, Forms, Dialogs, FileUtil, StdCtrls, LCLIntf, Math,

     URadialBlur, UMotionBlur, UEmboss, UTwirl, UWaveDisplacement,
     unewimage, uresample, UPixelate, unoisefilter, ufilters,
     UImageAction, USharpen, uposterize, UPhongFilter, UFilterFunction,
     uprint, USaveOption, UFormRain,

     ugraph, LCScaleDPI, ucommandline, uabout, UPython, UVolatileScrollBar;

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
  ScriptContext.RegisterScriptFunction('InputBox',@ScriptInputBox,ARegister);
end;

procedure TLazPaintInstance.Init(AEmbedded: boolean);
begin
  TVolatileScrollBar.InitDPI;

  Title := 'LazPaint ' + LazPaintCurrentVersion;
  FCustomImageList := TImageListList.Create;
  FTopMostInfo.choosecolorHidden := 0;
  FTopMostInfo.layerstackHidden := 0;
  FTopMostInfo.toolboxHidden := 0;
  FTopMostInfo.imagelistHidden := 0;
  FEmbedded:= AEmbedded;
  FScriptContext := TScriptContext.Create;
  FScriptContext.OnFunctionException:= @OnFunctionException;

  RegisterScripts(True);

  InColorFromFChooseColor := false;
  FImage := TLazPaintImage.Create;
  FImage.OnStackChanged:= @OnStackChanged;
  FImage.OnException := @OnFunctionException;
  FToolManager := TToolManager.Create(FImage, self, nil, BlackAndWhite, FScriptContext);
  UseConfig(TIniFile.Create(''));
  FToolManager.OnPopup := @OnToolPopup;
  FToolManager.OnFillChanged:= @ToolFillChanged;
  FSelectionEditConfig := nil;
  FTextureEditConfig := nil;

  if not AEmbedded then
    BGRALayers.RegisterLoadingHandler(@OnLayeredBitmapLoadStartHandler,@OnLayeredBitmapLoadProgressHandler,@OnLayeredBitmapLoadedHandler)
  else
    FLoadingLayers := nil;
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

  CreateToolBox;

  Application.CreateForm(TFCanvasSize, FCanvasSize);
  FCanvasSize.LazPaintInstance := self;
  Application.CreateForm(TFColorIntensity, FColorIntensity);
  Application.CreateForm(TFShiftColors, FShiftColors);
  Application.CreateForm(TFColorize, FColorize);
  Application.CreateForm(TFAdjustCurves, FColorCurves);
  Application.CreateForm(TFCustomBlur, FCustomBlur);
  FCustomBlur.LazPaintInstance := self;


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

function TLazPaintInstance.GetToolManager: TToolManager;
begin
  Result:= FToolManager;
end;

procedure TLazPaintInstance.CreateLayerStack;
begin
  if Assigned(FLayerStack) then exit;
  TFLayerStack_CustomDPI := (Config.DefaultIconSize(DoScaleX(16,OriginalDPI))*96+8) div 16;
  Application.CreateForm(TFLayerStack,FLayerStack);
  FLayerStack.LazPaintInstance := self;
  FLayerStack.DarkTheme:= Config.GetDarkTheme;

  FLayerStack.AddButton(FMain.LayerAddNew);
  FLayerStack.AddButton(FMain.LayerFromFile);
  FLayerStack.AddButton(FMain.LayerDuplicate);
  FLayerStack.AddButton(FMain.LayerMergeOver);
  FLayerStack.AddButton(FMain.LayerRasterize);
  FLayerStack.AddSeparator;
  FLayerStack.AddButton(FMain.LayerMove);
  FLayerStack.AddButton(FMain.LayerRotate);
  FLayerStack.AddButton(FMain.LayerZoom);
  FLayerStack.AddButton(FMain.LayerHorizontalFlip);
  FLayerStack.AddButton(FMain.LayerVerticalFlip);
  FLayerStack.AddButton(FMain.ToolLayerMapping);
end;

procedure TLazPaintInstance.CreateToolBox;
begin
  if Assigned(FToolBox) or not Assigned(FMain) then exit;
  Application.CreateForm(TFToolbox, FToolbox);
  FToolbox.LazPaintInstance := self;
  FToolbox.DarkTheme := Config.GetDarkTheme;

  //needed to attach to the right instance of FMain
  FToolbox.AddButton(FToolbox.Toolbar1, FMain.ToolHand);
  FToolbox.AddButton(FToolbox.Toolbar1, FMain.ToolColorPicker);
  FToolbox.AddButton(FToolbox.Toolbar1, FMain.ToolPen);
  FToolbox.AddButton(FToolbox.Toolbar1, FMain.ToolBrush);
  FToolbox.AddButton(FToolbox.Toolbar1, FMain.ToolEraser);
  FToolbox.AddButton(FToolbox.Toolbar1, FMain.ToolFloodfill);
  FToolbox.AddButton(FToolbox.Toolbar1, FMain.ToolClone);

  FToolbox.AddButton(FToolbox.Toolbar2, FMain.ToolRect);
  FToolbox.AddButton(FToolbox.Toolbar2, FMain.ToolEllipse);
  FToolbox.AddButton(FToolbox.Toolbar2, FMain.ToolPolygon);
  FToolbox.AddButton(FToolbox.Toolbar2, FMain.ToolSpline);
  FToolbox.AddButton(FToolbox.Toolbar2, FMain.ToolGradient);
  FToolbox.AddButton(FToolbox.Toolbar2, FMain.ToolPhong);
  FToolbox.AddButton(FToolbox.Toolbar2, FMain.ToolText);

  FToolbox.AddButton(FToolbox.Toolbar3, FMain.ToolEditShape);
  FToolbox.AddButton(FToolbox.Toolbar3, FMain.ToolDeformation);
  FToolbox.AddButton(FToolbox.Toolbar3, FMain.ToolTextureMapping);
  FToolbox.AddButton(FToolbox.Toolbar3, FMain.EditSelectAll);
  FToolbox.AddButton(FToolbox.Toolbar3, FMain.ToolMoveSelection);
  FToolbox.AddButton(FToolbox.Toolbar3, FMain.ToolRotateSelection);
  FToolbox.AddButton(FToolbox.Toolbar3, FMain.EditDeselect);

  FToolbox.AddButton(FToolbox.Toolbar4, FMain.ToolSelectRect);
  FToolbox.AddButton(FToolbox.Toolbar4, FMain.ToolSelectEllipse);
  FToolbox.AddButton(FToolbox.Toolbar4, FMain.ToolSelectPoly);
  FToolbox.AddButton(FToolbox.Toolbar4, FMain.ToolSelectSpline);
  FToolbox.AddButton(FToolbox.Toolbar4, FMain.ToolSelectPen);
  FToolbox.AddButton(FToolbox.Toolbar4, FMain.ToolMagicWand);
  FToolbox.AddButton(FToolbox.Toolbar4, FMain.ToolHotSpot);

  FToolBox.SetImages(Icons[Config.DefaultIconSize(DoScaleX(20,OriginalDPI))]);

  FMain.Layout.DockedToolBoxAddButton(FMain.ToolChangeDocking);

  FMain.Layout.DockedToolBoxAddButton(FMain.ToolHand);
  FMain.Layout.DockedToolBoxAddButton(FMain.ToolColorPicker);
  FMain.Layout.DockedToolBoxAddButton(FMain.ToolPen);
  FMain.Layout.DockedToolBoxAddButton(FMain.ToolBrush);
  FMain.Layout.DockedToolBoxAddButton(FMain.ToolEraser);
  FMain.Layout.DockedToolBoxAddButton(FMain.ToolFloodfill);
  FMain.Layout.DockedToolBoxAddButton(FMain.ToolClone);

  FMain.Layout.DockedToolBoxAddButton(FMain.ToolEditShape);
  FMain.Layout.DockedToolBoxAddButton(FMain.ToolRect);
  FMain.Layout.DockedToolBoxAddButton(FMain.ToolEllipse);
  FMain.Layout.DockedToolBoxAddButton(FMain.ToolPolygon);
  FMain.Layout.DockedToolBoxAddButton(FMain.ToolSpline);
  FMain.Layout.DockedToolBoxAddButton(FMain.ToolGradient);
  FMain.Layout.DockedToolBoxAddButton(FMain.ToolPhong);
  FMain.Layout.DockedToolBoxAddButton(FMain.ToolText);

  FMain.Layout.DockedToolBoxAddButton(FMain.ToolDeformation);
  FMain.Layout.DockedToolBoxAddButton(FMain.ToolTextureMapping);

  FMain.Layout.DockedToolBoxAddButton(FMain.ToolSelectRect);
  FMain.Layout.DockedToolBoxAddButton(FMain.ToolSelectEllipse);
  FMain.Layout.DockedToolBoxAddButton(FMain.ToolSelectPoly);
  FMain.Layout.DockedToolBoxAddButton(FMain.ToolSelectSpline);

  FMain.Layout.DockedToolBoxAddButton(FMain.ToolSelectPen);
  FMain.Layout.DockedToolBoxAddButton(FMain.ToolMagicWand);

  FMain.Layout.DockedToolBoxAddButton(FMain.ToolMoveSelection);
  FMain.Layout.DockedToolBoxAddButton(FMain.ToolRotateSelection);
  FMain.Layout.DockedToolBoxAddButton(FMain.EditDeselect);

  FMain.Layout.DockedToolBoxSetImages(Icons[Config.DefaultIconSize(DoScaleX(20,OriginalDPI))]);
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

procedure TLazPaintInstance.OnToolPopup(sender: TToolManager; AMessage: TToolPopupMessage; AKey: Word);
var messageStr: string;
    idx: integer;
begin
  if Assigned(Config) then
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
    FLayerStack.Height := AValue;
end;

procedure TLazPaintInstance.SetLayerWindowWidth(AValue: integer);
begin
  if FLayerStack <> nil then
    FLayerStack.Width := AValue;
end;

function TLazPaintInstance.GetMainFormVisible: boolean;
begin
  if FMain <> nil then
    result := FMain.Visible
  else
    result := false;
end;

function TLazPaintInstance.GetInitialized: boolean;
begin
  result := Assigned(FMain) and FMain.Initialized;
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

procedure TLazPaintInstance.OnLayeredBitmapLoadStartHandler(AFilenameUTF8: string);
begin
  if FLoadingLayers = nil then
    FLoadingLayers := TFLoading.Create(nil);
  FLoadingLayers.ShowMessage(rsOpening+' ' +AFilenameUTF8+'...');
  UpdateWindows;
end;

procedure TLazPaintInstance.OnLayeredBitmapLoadProgressHandler(
  APercentage: integer);
begin
  if FLoadingLayers <> nil then
  begin
    FLoadingLayers.ShowMessage(rsLoading+' (' +inttostr(APercentage)+'%)');
    UpdateWindows;
  end;
end;

procedure TLazPaintInstance.OnLayeredBitmapLoadedHandler;
begin
  if FLoadingLayers <> nil then
  begin
    FreeAndNil(FLoadingLayers);
    UpdateWindows;
  end;
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
  if FLayerStack <> nil then
    result := FLayerStack.Visible
  else
    result := false;
end;

procedure TLazPaintInstance.SetLayerWindowVisible(AValue: boolean);
begin
  if FLayerStack <> nil then
    FLayerStack.Visible := AValue;
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
      '[Window]'+LineEnding+'LayerWindowVisible=False');
  EditBitmap(AImage,FSelectionEditConfig,rsEditSelection,@SelectionInstanceOnRun,nil,True);
end;

function TLazPaintInstance.GetZoomFactor: single;
begin
  if Assigned(FMain) and Assigned(FMain.Zoom) then
    Result:=FMain.Zoom.Factor else
      result := inherited GetZoomFactor;
end;

procedure TLazPaintInstance.ApplyTheme(ADarkTheme: boolean);
begin
  if Assigned(FChooseColor) then FChooseColor.DarkTheme := ADarkTheme;
  if Assigned(FLayerStack) then FLayerStack.DarkTheme := ADarkTheme;
  if Assigned(FMain) then FMain.DarkTheme := ADarkTheme;
  if Assigned(FToolbox) then FToolbox.DarkTheme:= ADarkTheme;
end;

function TLazPaintInstance.GetGridVisible: boolean;
begin
  Result:= FGridVisible;
end;

procedure TLazPaintInstance.SetGridVisible(const AValue: boolean);
begin
  FGridVisible := AValue;
  Image.RenderMayChange(rect(0,0,Image.Width,Image.Height),True);
  NotifyImageChange(False,EmptyRect);
end;

function TLazPaintInstance.GetChooseColorVisible: boolean;
begin
  result := FChooseColor.Visible;
end;

function TLazPaintInstance.GetToolboxVisible: boolean;
begin
  if FToolbox <> nil then
    Result:= FToolbox.Visible or ((FMain <> nil) and not (FMain.Layout.ToolBoxDocking in [twNone,twWindow]))
  else
    Result := false;
end;

function TLazPaintInstance.GetImageListWindowVisible: boolean;
begin
  if FImageList <> nil then
    Result:= FImageList.Visible
  else
    Result := false;
end;

procedure TLazPaintInstance.SetChooseColorVisible(const AValue: boolean);
begin
  if FChooseColor <> nil then
    FChooseColor.Visible := AValue;
end;

procedure TLazPaintInstance.SetToolBoxVisible(const AValue: boolean);
begin
  if FInSetToolboxVisible then exit;
  if FToolbox <> nil then
  begin
    FInSetToolboxVisible := true;
    if Assigned(FMain) then
    begin
      FMain.Layout.ToolBoxVisible := AValue;
      FToolbox.Visible := (FMain.Layout.ToolBoxDocking = twWindow);
    end else
      FToolbox.Visible := AValue;
    FInSetToolboxVisible := false;
  end;
end;

procedure TLazPaintInstance.SetImageListWindowVisible(const AValue: boolean);
begin
  if FImageList <> nil then
    FImageList.Visible := AValue;
end;

function TLazPaintInstance.GetChooseColorHeight: integer;
begin
  FChooseColor.UpdateLayout;
  Result:= FChooseColor.Height;
end;

function TLazPaintInstance.GetChooseColorWidth: integer;
begin
  FChooseColor.UpdateLayout;
  Result:= FChooseColor.Width;
end;

procedure TLazPaintInstance.AssignBitmap(bmp: TBGRABitmap);
var imageActions: TimageActions;
begin
  imageActions := TImageActions.Create(self);
  imageActions.SetCurrentBitmap(bmp.Duplicate as TBGRABitmap, False);
  imageActions.Free;
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
var imageActions: TimageActions;
begin
  imageActions := TImageActions.Create(self);
  try
    imageActions.EditSelection(@EditSelectionHandler);
  except
    on ex: Exception do
      ShowError('EditSelection',ex.Message);
  end;
  imageActions.Free;
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
var
  i: Integer;
begin
  if Assigned(FMain) then
  begin
    for i := 0 to FCustomImageList.Count-1 do
      if FCustomImageList[i].Height = ASize then
        exit(FCustomImageList[i]);

    if ASize < 24 then
    begin;
      if ASize = 16 then
        result := FMain.ImageList16
      else
      begin
        result := TImageList.Create(nil);
        ScaleImageList(FMain.ImageList16, ASize,ASize, result);
        FCustomImageList.Add(result);
      end;
    end
    else
    begin
      if ASize = 48 then
        result := FMain.ImageList48
      else
      begin
        result := TImageList.Create(nil);
        ScaleImageList(FMain.ImageList48, ASize,ASize, result);
        FCustomImageList.Add(result);
      end;
    end;
  end else
    result := nil;
end;

function TLazPaintInstance.GetToolBoxWindowPopup: TPopupMenu;
begin
  if Assigned(FToolbox) then
    result := FToolbox.PopupMenu
  else
    result := nil;
end;

procedure TLazPaintInstance.SetToolBoxWindowPopup(AValue: TPopupMenu);
begin
  CreateToolBox;
  if Assigned(FToolbox) then
    FToolbox.PopupMenu := AValue;
end;

function TLazPaintInstance.GetFullscreen: boolean;
begin
  result := FFullscreen;
end;

procedure TLazPaintInstance.SetFullscreen(AValue: boolean);
begin
  if (AValue = FFullscreen) or not MainFormVisible or (FMain.WindowState = wsMinimized) then exit;
  if AValue then DockLayersAndColors:= false;
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

function TLazPaintInstance.GetDockLayersAndColors: boolean;
begin
  result := FDockLayersAndColors;
end;

procedure TLazPaintInstance.SetDockLayersAndColors(AValue: boolean);
begin
  if FDockLayersAndColors= AValue then exit;

  if AValue = false then
  begin
    if FChooseColor <> nil then
      with Config.DefaultColorWindowPosition do
        if (Right > Left) and (Bottom > Top) then
          SetWindowTopLeftCorner(FChooseColor, Left,Top);
    if FLayerStack <> nil then
      with Config.DefaultLayerWindowPosition do
        if (Right > Left) and (Bottom > Top) then
        FLayerStack.SetBounds(Left,Top,Right-Left,Bottom-Top);
  end else
  begin
    Fullscreen:= false;
    if FChooseColor <> nil then
      Config.SetDefaultColorWindowPosition(FChooseColor.BoundsRect);
    if FLayerStack <> nil then
      Config.SetDefaultLayerWindowPosition(FLayerStack.BoundsRect);
  end;
  FDockLayersAndColors:= AValue;
end;

function TLazPaintInstance.GetScriptContext: TScriptContext;
begin
  result := FScriptContext;
end;

function TLazPaintInstance.ProcessCommandLine: boolean;
var commands: TStringList;
    error,saved: boolean;
    i: Integer;
begin
  if paramCount = 0 then
  begin
    result := false;
    exit;
  end;

  FormsNeeded;
  commands := TStringList.Create;
  for i := 1 to paramCount do
    commands.Add( ParamStrUtf8(i));
  ucommandline.ProcessCommands(self,commands,error,saved);
  commands.free;
  result := error or saved;
end;

function TLazPaintInstance.ProcessCommands(commands: TStringList): boolean;
var saved: boolean;
begin
  if paramCount = 0 then
  begin
    result := true;
    exit;
  end;
  FormsNeeded;
  ucommandline.ProcessCommands(self,commands,result,saved);
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

procedure TLazPaintInstance.Hide;
begin
  if MainFormVisible then FMain.Hide;
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
  RegisterScripts(False);

  FDestroying := true;

  Config.SetDefaultDockLayersAndColors(FDockLayersAndColors);
  Config.SetDefaultGridVisible(FGridVisible);
  if (FChooseColor <> nil) and FChooseColorPositionDefined then
  begin
    Config.SetDefaultColorWindowVisible(ChooseColorVisible or (FTopMostInfo.choosecolorHidden > 0));
    if not FDockLayersAndColors then Config.SetDefaultColorWindowPosition(FChooseColor.BoundsRect);
  end;
  if (FLayerStack <> nil) and FLayerStackPositionDefined then
  begin
    Config.SetDefaultLayerWindowVisible(LayerWindowVisible or (FTopMostInfo.layerstackHidden > 0));
    if not FDockLayersAndColors then Config.SetDefaultLayerWindowPosition(FLayerStack.BoundsRect);
  end;
  if (FImageList <> nil) and FImageListPositionDefined then
  begin
    Config.SetDefaultImagelistWindowVisible (ImageListWindowVisible or (FTopMostInfo.imagelistHidden > 0));
    Config.SetDefaultImagelistWindowPosition(FImageList.BoundsRect);
  end;
  if (FToolbox <> nil) and FToolBoxPositionDefined then
  begin
    Config.SetDefaultToolboxWindowVisible(ToolboxVisible or (FTopMostInfo.toolboxHidden > 0));
    Config.SetDefaultToolboxWindowPosition(FToolBox.BoundsRect);
  end;
  ToolManager.SaveToConfig;

  if FLoadingLayers <> nil then
  begin
    FreeAndNil(FLoadingLayers);
    BGRALayers.UnregisterLoadingHandler(@OnLayeredBitmapLoadStartHandler,@OnLayeredBitmapLoadProgressHandler,@OnLayeredBitmapLoadedHandler);
  end;
  FreeAndNil(FLayerStack);
  FreeAndNil(FCustomBlur);
  FreeAndNil(FColorize);
  FreeAndNil(FColorCurves);
  FreeAndNil(FShiftColors);
  FreeAndNil(FColorIntensity);
  FreeAndNil(FCanvasSize);
  FreeAndNil(FChooseColor);
  FreeAndNil(FToolbox);
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
  inherited Destroy;
end;

function TLazPaintInstance.HideTopmost: TTopMostInfo;
begin
  result.defined:= false;
  if FDestroying then exit;

  if (FToolBox <> nil) and FToolBox.Visible then
  begin
    FToolbox.Hide;
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
    FLayerStack.InvalidateStack(False);
    dec(FTopMostInfo.layerstackHidden);
  end;
  if Assigned(FChooseColor) and (AInfo.choosecolorHidden > 0) then
  begin
    FChooseColor.Show;
    dec(FTopMostInfo.choosecolorHidden);
  end;
  if Assigned(FToolbox) and (AInfo.toolboxHidden > 0) then
  begin
    FToolbox.Show;
    dec(FTopMostInfo.toolboxHidden);
  end;
end;

procedure TLazPaintInstance.UpdateWindows;
begin
  {$IFDEF LINUX}
  if Assigned(FMain) then FMain.Enabled:= false;
  if Assigned(FToolbox) then FToolbox.Enabled:= false;
  if Assigned(FChooseColor) then FChooseColor.Enabled:= false;
  if Assigned(FLayerStack) then FLayerStack.Enabled:= false;
  if Assigned(FImageList) then FImageList.Enabled:= false;
  Application.ProcessMessages;
  if Assigned(FMain) then FMain.Enabled:= true;
  if Assigned(FToolbox) then FToolbox.Enabled:= true;
  if Assigned(FChooseColor) then FChooseColor.Enabled:= true;
  if Assigned(FLayerStack) then FLayerStack.Enabled:= true;
  if Assigned(FImageList) then FImageList.Enabled:= true;
  {$ELSE}
  if Assigned(FMain) then FMain.Update;
  if Assigned(FToolbox) then FToolbox.Update;
  if Assigned(FChooseColor) then FChooseColor.Update;
  if Assigned(FLayerStack) then FLayerStack.Update;
  if Assigned(FImageList) then FImageList.Update;
  {$ENDIF}
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

function TLazPaintInstance.RunScript(AFilename: string): boolean;
var
  p: TPythonScript;
  fError: TForm;
  memo: TMemo;
  doFound, somethingDone: boolean;
  tmi: TTopMostInfo;
begin
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
  try
    p := TPythonScript.Create;
    FScriptName := AFilename;
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
  p.Free;
  if Assigned(FMain) then FMain.Enabled:= true;
  ShowTopmost(tmi);
  //ensure we are out of any do group
  repeat
    Image.DoEnd(doFound, somethingDone);
  until not doFound;
end;

procedure TLazPaintInstance.ColorFromFChooseColor;
begin
  FormsNeeded;
  if InColorFromFChooseColor then exit;
  InColorFromFChooseColor := True;
  SetColor(FChooseColor.colorTarget, FChooseColor.GetCurrentColor);
  InColorFromFChooseColor := false;
end;

procedure TLazPaintInstance.ColorToFChooseColor;
begin
  if not Assigned(FChooseColor) or InColorFromFChooseColor then exit;
  FChooseColor.SetCurrentColor(GetColor(FChooseColor.colorTarget));
end;

function TLazPaintInstance.ShowSaveOptionDlg(AParameters: TVariableSet;
  AOutputFilenameUTF8: string; ASkipOptions: boolean): boolean;
begin
  result := USaveOption.ShowSaveOptionDialog(self,AOutputFilenameUTF8,ASkipOptions);
end;

procedure TLazPaintInstance.MoveToolboxTo(X, Y: integer);
begin
  FormsNeeded;
  FToolbox.Left := X;
  FToolbox.Top := Y;
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

procedure TLazPaintInstance.ScrollLayerStackOnItem(AIndex: integer);
begin
  if FLayerStack<> nil then
  begin
    FLayerStack.SetLayerStackScrollPosOnItem(AIndex);
    if FMain <> nil then
    begin
      FMain.UpdateStackOnTimer := true;
    end else
      NotifyStackChange;
  end;
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
  Result:= FToolbox.Height;
end;

function TLazPaintInstance.GetToolboxWidth: integer;
begin
  Result:= FToolbox.Width;
end;

function TLazPaintInstance.GetTopMostHasFocus: boolean;
begin
  if FDestroying then
  begin
    result := false;
    exit;
  end;

  result := false;
  if (FToolBox <> nil) and FToolBox.Visible and FToolBox.Active then
    result := true;
  if (FChooseColor <> nil) and FChooseColor.Visible and FChooseColor.Active then
    result := true;
  if (FLayerStack <> nil) and FLayerStack.Visible and FLayerStack.Active then
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
  result := FToolBox.Visible or FChooseColor.Visible or FLayerStack.Visible;
end;

function TLazPaintInstance.GetTopMostOkToUnfocus: boolean;
begin
  if FChooseColor.Active and FChooseColor.EColor.Visible then
    result := false
  else
    result := true;
end;

function TLazPaintInstance.GetChooseColorTarget: TColorTarget;
begin
  if Assigned(FChooseColor) then
    Result:= FChooseColor.colorTarget
  else
    result := ctForeColorSolid;
end;

procedure TLazPaintInstance.SetChooseColorTarget(const AValue: TColorTarget);
begin
  if not Assigned(FChooseColor) then exit;
  FChooseColor.colorTarget:= AValue;
  ColorToFChooseColor;
end;

procedure TLazPaintInstance.ApplyDocking;
var xDest,yDest: integer;
    w,h: integer;
    pos: TPoint;
    r: TRect;
begin
  if FDockLayersAndColors and MainFormVisible then
  begin
    if (FMain.WindowState = wsMaximized) and (LayerWindowVisible or ChooseColorVisible) then
    begin
      w := GetWindowFullWidth(FMain) - WindowBorderWidth(FMain)*2;
      h := GetWindowFullHeight(FMain) - WindowBorderTopHeight(FMain,False) - WindowBorderBottomHeight(FMain);
      pos := GetWindowTopLeftCorner(FMain);
      pos.x += WindowBorderWidth(FMain) - WindowOutermostBorderWidth;
      pos.y += WindowBorderTopHeight(FMain,False) - WindowOutermostBorderHeight;
      w += WindowOutermostBorderWidth*2;
      h += WindowOutermostBorderHeight*2;
      if ChooseColorVisible then w -= GetWindowFullWidth(FChooseColor) - WindowOutermostBorderWidth
      else if LayerWindowVisible then w -= GetWindowFullWidth(FLayerStack) - WindowOutermostBorderWidth;
      FMain.WindowState := wsNormal;
      SetWindowTopLeftCorner(FMain,pos.x,pos.y);
      SetWindowFullSize(FMain,w,h);
    end;
    xDest := FMain.Left+GetWindowFullWidth(FMain)-WindowOutermostBorderWidth;
    yDest := FMain.Top;

    r.left := FMain.Left+GetWindowFullWidth(FMain);
    r.top := FMain.Top;
    r.right := r.left;
    r.bottom := r.top+GetWindowFullHeight(FMain);
    if ChooseColorVisible then r.right := r.Left+GetWindowFullWidth(FChooseColor)-WindowOutermostBorderHeight;
    if LayerWindowVisible then r.right := max(r.right, r.Left+GetWindowFullWidth(FLayerStack)-WindowOutermostBorderHeight);

    h := GetWindowFullHeight(FMain);
    if ChooseColorVisible then dec(h, GetWindowFullHeight(FChooseColor));

    if LayerWindowVisible then
    begin
      FLayerStack.CompletelyResizeable := false;
      SetWindowTopLeftCorner(FLayerStack, xDest,yDest);
      SetWindowFullHeight(FLayerStack, h);
      yDest += GetWindowFullHeight(FLayerStack)-WindowOutermostBorderHeight;
      FLayerStack.FormStyle := fsNormal;
    end;

    if ChooseColorVisible then
    begin
      if LayerWindowVisible then
        SetWindowFullWidth(FLayerStack, GetWindowFullWidth(FChooseColor));
      SetWindowTopLeftCorner(FChooseColor, xDest,yDest);
      yDest += GetWindowFullHeight(FChooseColor)-WindowOutermostBorderHeight;
      FChooseColor.FormStyle := fsNormal;
    end;

    FPrevDockArea := r;
  end else
  begin
    if LayerWindowVisible then
    begin
      FLayerStack.CompletelyResizeable := true;
      FLayerStack.FormStyle := fsStayOnTop;
    end;
    if FChooseColor <> nil then FChooseColor.FormStyle := fsStayOnTop;
  end;
end;

function TLazPaintInstance.OpenImage (FileName: string; AddToRecent: Boolean=True): boolean;
begin
  FormsNeeded;
  Result:= FMain.TryOpenFileUTF8(FileName, AddToRecent);
end;

procedure TLazPaintInstance.AddToImageList(const FileNames: array of String);
begin
  if FImageList <> nil then
    FImageList.AddFiles (FileNames);
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

