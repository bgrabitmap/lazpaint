unit LazpaintInstance;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazPaintType, BGRABitmap, BGRABitmapTypes, BGRALayers,

  LazPaintMainForm,

  utoolbox, uchoosecolor, ulayerstack, ucanvassize,
  ucolorintensity, ushiftcolors, ucolorize, uadjustcurves,
  ucustomblur, uimagelist,

  ULoading, UImage, UTool, uconfig, IniFiles, uresourcestrings, uscripting;

const
  MaxToolPopupShowCount = 2;

type

  { TLazPaintInstance }

  TLazPaintInstance = class(TLazPaintCustomInstance)
  private
    function GetMainFormVisible: boolean;
    procedure OnLayeredBitmapLoadStartHandler(AFilenameUTF8: string);
    procedure OnLayeredBitmapLoadProgressHandler(APercentage: integer);
    procedure OnLayeredBitmapLoadedHandler;
    procedure SelectionInstanceOnRun(AInstance: TLazPaintCustomInstance);

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
    procedure FormsNeeded;
    procedure Init(AEmbedded: boolean);
    procedure SetBlackAndWhite(AValue: boolean); override;
    procedure OnStackChanged({%H-}sender: TLazPaintImage; AScrollIntoView: boolean);
    procedure OnToolPopup({%H-}sender: TToolManager; AMessage: TToolPopupMessage);

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

  public
    constructor Create; override;
    constructor Create(AEmbedded: boolean); override;
    procedure Donate; override;
    procedure UseConfig(ini: TInifile); override;
    procedure AssignBitmap(bmp: TBGRABitmap); override;
    procedure EditBitmap(var bmp: TBGRABitmap; ConfigStream: TStream = nil; ATitle: String = ''; AOnRun: TLazPaintInstanceEvent = nil; AOnExit: TLazPaintInstanceEvent = nil; ABlackAndWhite: boolean = false); override;
    procedure EditSelection; override;
    procedure EditTexture; override;
    function ProcessCommandLine: boolean; override;
    function ProcessCommands(commands: TStringList): boolean; override;
    procedure Show; override;
    procedure Hide; override;
    procedure Run; override;
    destructor Destroy; override;
    procedure NotifyImageChange(RepaintNow: boolean; ARect: TRect); override;
    procedure NotifyImageChangeCompletely(RepaintNow: boolean); override;
    function TryOpenFileUTF8(filename: string): boolean; override;
    function ExecuteFilter(filter: TPictureFilter; skipDialog: boolean = false): boolean; override;
    procedure ColorFromFChooseColor; override;
    procedure ColorToFChooseColor; override;
    function ShowColorIntensityDlg(AParameters: TVariableSet): boolean; override;
    function ShowColorLightnessDlg(AParameters: TVariableSet): boolean; override;
    function ShowShiftColorsDlg(AParameters: TVariableSet): boolean; override;
    function ShowColorizeDlg(AParameters: TVariableSet): boolean; override;
    function ShowColorCurvesDlg(AParameters: TVariableSet): boolean; override;
    function ShowRadialBlurDlg(AFilterConnector: TObject;blurType:TRadialBlurType):boolean; override;
    function ShowMotionBlurDlg(AFilterConnector: TObject):boolean; override;
    function ShowCustomBlurDlg(AFilterConnector: TObject):boolean; override;
    function ShowEmbossDlg(AFilterConnector: TObject):boolean; override;
    function ShowPixelateDlg(AFilterConnector: TObject):boolean; override;
    function ShowNoiseFilterDlg(AFilterConnector: TObject):boolean; override;
    function ShowTwirlDlg(AFilterConnector: TObject):boolean; override;
    function ShowPhongFilterDlg(AFilterConnector: TObject): boolean; override;
    function ShowFunctionFilterDlg(AFilterConnector: TObject): boolean; override;
    function ShowSharpenDlg(AFilterConnector: TObject):boolean; override;
    function ShowPosterizeDlg(AParameters: TVariableSet):boolean; override;
    procedure ShowPrintDlg; override;
    function HideTopmost: TTopMostInfo; override;
    procedure ShowTopmost(AInfo: TTopMostInfo); override;
    procedure UpdateWindows;  override;
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
    function MakeNewBitmapReplacement(AWidth, AHeight: integer): TBGRABitmap; override;
    procedure ChooseTool(Tool : TPaintToolType); override;
    function OpenImage (FileName: string; AddToRecent: Boolean= True): boolean; override;
    procedure ZoomFit; override;
    procedure AddToImageList(const FileNames: array of String); override;
  end;

implementation

uses Types, Forms, Dialogs, Controls, FileUtil, LCLIntf,

     uradialblur, umotionblur, uemboss, utwirl,
     unewimage, uresample, upixelate, unoisefilter, ufilters,
     UImageAction, USharpen, uposterize, UPhongFilter, UFilterFunction,
     uprint,

     ugraph, ucommandline, uabout;

{ TLazPaintInstance }

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

procedure TLazPaintInstance.Init(AEmbedded: boolean);
begin
  Title := 'LazPaint ' + LazPaintCurrentVersion;
  FTopMostInfo.choosecolorHidden := 0;
  FTopMostInfo.layerstackHidden := 0;
  FTopMostInfo.toolboxHidden := 0;
  FTopMostInfo.imagelistHidden := 0;
  FEmbedded:= AEmbedded;
  FScriptContext := TScriptContext.Create;
  FScriptContext.OnFunctionException:= @OnFunctionException;
  //FScriptContext.Recording := true;

  InColorFromFChooseColor := false;
  FImage := TLazPaintImage.Create;
  FImage.OnStackChanged:= @OnStackChanged;
  FImage.OnException := @OnFunctionException;
  FToolManager := TToolManager.Create(FImage, nil, BlackAndWhite);
  FToolManager.OnPopup := @OnToolPopup;
  UseConfig(TIniFile.Create(''));
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
  ToolManager.BitmapToVirtualScreen := @FMain.BitmapToVirtualScreen;

  Application.CreateForm(TFToolbox, FToolbox);
  FToolbox.LazPaintInstance := self;

  //needed to attach to the right instance of FMain
  FToolbox.AddButton(FToolbox.Toolbar1, FMain.ToolHand);
  FToolbox.AddButton(FToolbox.Toolbar1, FMain.ToolPen);
  FToolbox.AddButton(FToolbox.Toolbar1, FMain.ToolColorPicker);
  FToolbox.AddButton(FToolbox.Toolbar1, FMain.ToolEraser);
  FToolbox.AddButton(FToolbox.Toolbar1, FMain.ToolFloodfill);
  FToolbox.AddButton(FToolbox.Toolbar1, FMain.ToolGradient);

  FToolbox.AddButton(FToolbox.Toolbar2, FMain.ToolRect);
  FToolbox.AddButton(FToolbox.Toolbar2, FMain.ToolEllipse);
  FToolbox.AddButton(FToolbox.Toolbar2, FMain.ToolPolygon);
  FToolbox.AddButton(FToolbox.Toolbar2, FMain.ToolSpline);
  FToolbox.AddButton(FToolbox.Toolbar2, FMain.ToolText);
  FToolbox.AddButton(FToolbox.Toolbar2, FMain.ToolPhong);

  FToolbox.AddButton(FToolbox.Toolbar3, FMain.ToolSelectRect);
  FToolbox.AddButton(FToolbox.Toolbar3, FMain.ToolSelectEllipse);
  FToolbox.AddButton(FToolbox.Toolbar3, FMain.ToolSelectPoly);
  FToolbox.AddButton(FToolbox.Toolbar3, FMain.ToolSelectSpline);
  FToolbox.AddButton(FToolbox.Toolbar3, FMain.ToolDeformation);
  FToolbox.AddButton(FToolbox.Toolbar3, FMain.ToolTextureMapping);

  FToolbox.AddButton(FToolbox.Toolbar4, FMain.ToolSelectPen);
  FToolbox.AddButton(FToolbox.Toolbar4, FMain.ToolMoveSelection);
  FToolbox.AddButton(FToolbox.Toolbar4, FMain.ToolRotateSelection);
  FToolbox.AddButton(FToolbox.Toolbar4, FMain.ToolMagicWand);
  FToolbox.AddButton(FToolbox.Toolbar4, FMain.EditDeselect);

  Application.CreateForm(TFLayerStack,FLayerStack);
  FLayerStack.LazPaintInstance := self;

  FLayerStack.AddButton(FMain.LayerAddNew);
  FLayerStack.AddButton(FMain.LayerFromFile);
  FLayerStack.AddButton(FMain.LayerDuplicate);
  FLayerStack.AddButton(FMain.LayerMergeOver);
  FLayerStack.AddButton(FMain.LayerRemoveCurrent);
  FLayerStack.AddSeparator;
  FLayerStack.AddButton(FMain.LayerMove);
  FLayerStack.AddButton(FMain.LayerRotate);
  FLayerStack.AddButton(FMain.ToolLayerMapping);
  FLayerStack.AddButton(FMain.LayerHorizontalFlip);
  FLayerStack.AddButton(FMain.LayerVerticalFlip);

  Application.CreateForm(TFChooseColor, FChooseColor);
  FChooseColor.LazPaintInstance := self;
  Application.CreateForm(TFCanvasSize, FCanvasSize);
  FCanvasSize.LazPaintInstance := self;
  Application.CreateForm(TFColorIntensity, FColorIntensity);
  Application.CreateForm(TFShiftColors, FShiftColors);
  Application.CreateForm(TFColorize, FColorize);
  Application.CreateForm(TFAdjustCurves, FColorCurves);
  Application.CreateForm(TFCustomBlur, FCustomBlur);
  FCustomBlur.LazPaintInstance := self;

  Application.CreateForm(TFImageList, FImageList);
  FImageList.LazPaintInstance := self;

  FInFormsNeeded := false;
end;

procedure TLazPaintInstance.UseConfig(ini: TInifile);
begin
  FreeAndNil(FConfig);
  FConfig := TLazPaintConfig.Create(ini,LazPaintCurrentVersionOnly);

  ToolManager.ToolForeColor := Config.DefaultToolForeColor;
  ToolManager.ToolBackColor := Config.DefaultToolBackColor;
  ToolManager.ToolNormalPenWidth := Config.DefaultToolPenWidth;
  ToolManager.ToolEraserWidth := Config.DefaultToolEraserWidth;
  ToolManager.ToolOptionDrawShape := Config.DefaultToolOptionDrawShape;
  ToolManager.ToolOptionFillShape := Config.DefaultToolOptionFillShape;
  ToolManager.ToolOptionCloseShape := Config.DefaultToolOptionCloseShape;
  ToolManager.ToolTolerance := Config.DefaultToolTolerance;
  ToolManager.ToolTextShadow := Config.DefaultToolTextShadow;
  ToolManager.ToolTextOutline := Config.DefaultToolTextOutline;
  ToolManager.ToolTextPhong := Config.DefaultToolTextPhong;
  ToolManager.ToolTextFont.Assign(Config.DefaultToolTextFont);
  ToolManager.ToolTextBlur := Config.DefaultToolTextBlur;
  ToolManager.ToolTextShadowOffset := Config.DefaultToolTextShadowOffset;
  ToolManager.ToolLightPosition := Config.DefaultToolLightPosition;
  ToolManager.ToolLightAltitude := Config.DefaultToolLightAltitude;
  ToolManager.ToolShapeAltitude := Config.DefaultToolShapeAltitude;
  ToolManager.ToolShapeBorderSize := Config.DefaultToolShapeBorderSize;
  ToolManager.ToolShapeType := Config.DefaultToolShapeType;
  FGridVisible := Config.DefaultGridVisible;
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

procedure TLazPaintInstance.OnToolPopup(sender: TToolManager; AMessage: TToolPopupMessage);
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
  messageStr := ToolPopupMessageToStr(AMessage);
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
  if Assigned(FMain) then
    Result:=FMain.ZoomFactor else
      result := inherited GetZoomFactor;
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
    Result:= FToolbox.Visible
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
  if FToolbox <> nil then
    FToolbox.Visible := AValue;
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

procedure TLazPaintInstance.EditTexture;
var tex: TBGRABitmap;
begin
  try
    if FTextureEditConfig = nil then
      FTextureEditConfig := TStringStream.Create('[General]'+LineEnding+
        'DefaultImageWidth=256'+LineEnding+
        'DefaultImageHeight=256'+LineEnding);
    tex := ToolManager.BorrowToolTexture;
    try
      EditBitmap(tex,FTextureEditConfig,rsEditTexture,nil,nil,BlackAndWhite);
    finally
      ToolManager.SetToolTexture(tex);
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

destructor TLazPaintInstance.Destroy;
begin
  FDestroying := true;

  Config.SetDefaultGridVisible(FGridVisible);
  if FToolbox <> nil then
    Config.SetDefaultToolboxWindowVisible(FToolbox.Visible or (FTopMostInfo.toolboxHidden > 0));
  if FChooseColor <> nil then
    Config.SetDefaultColorWindowVisible(FChooseColor.Visible or (FTopMostInfo.choosecolorHidden > 0));
  if FLayerStack <> nil then
    Config.SetDefaultLayerWindowVisible(FLayerStack.Visible or (FTopMostInfo.layerstackHidden > 0));
  if FImageList <> nil then
    Config.SetDefaultImagelistWindowVisible (FImageList.Visible or (FTopMostInfo.imagelistHidden > 0));
  if FToolbox <> nil then
    Config.SetDefaultToolboxWindowPosition(FToolBox.BoundsRect);
  if FChooseColor <> nil then
    Config.SetDefaultColorWindowPosition(FChooseColor.BoundsRect);
  if FLayerStack <> nil then
    Config.SetDefaultLayerWindowPosition(FLayerStack.BoundsRect);
  if FImageList <> nil then
     Config.SetDefaultImagelistWindowPosition(FImageList.BoundsRect);
  Config.SetDefaultToolForeColor(ToolManager.ToolForeColor);
  Config.SetDefaultToolBackColor(ToolManager.ToolBackColor);
  Config.SetDefaultToolPenWidth(ToolManager.ToolNormalPenWidth);
  Config.SetDefaultToolEraserWidth(ToolManager.ToolEraserWidth);
  Config.SetDefaultToolOptionDrawShape(ToolManager.ToolOptionDrawShape);
  Config.SetDefaultToolOptionFillShape(ToolManager.ToolOptionFillShape);
  Config.SetDefaultToolOptionCloseShape(ToolManager.ToolOptionCloseShape);
  Config.SetDefaultToolTolerance(ToolManager.ToolTolerance);

  Config.SetDefaultToolTextFont(ToolManager.ToolTextFont);
  Config.SetDefaultToolTextShadow(ToolManager.ToolTextShadow);
  Config.SetDefaultToolTextOutline(ToolManager.ToolTextOutline);
  Config.SetDefaultToolTextBlur(ToolManager.ToolTextBlur);
  Config.SetDefaultToolTextShadowOffset(ToolManager.ToolTextShadowOffset);
  Config.SetDefaultToolTextPhong(ToolManager.ToolTextPhong);

  Config.SetDefaultToolLightPosition(ToolManager.ToolLightPosition);
  Config.SetDefaultToolLightAltitude(ToolManager.ToolLightAltitude);
  Config.SetDefaultToolShapeBorderSize(ToolManager.ToolShapeBorderSize);
  Config.SetDefaultToolShapeAltitude(ToolManager.ToolShapeAltitude);
  Config.SetDefaultToolShapeType(ToolManager.ToolShapeType);

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

  if Assigned(FToolbox) and (AInfo.toolboxHidden > 0) then
  begin
    FToolbox.Show;
    dec(FTopMostInfo.toolboxHidden);
  end;
  if Assigned(FChooseColor) and (AInfo.choosecolorHidden > 0) then
  begin
    FChooseColor.Show;
    dec(FTopMostInfo.choosecolorHidden);
  end;
  if Assigned(FLayerStack) and (AInfo.layerstackHidden > 0) then
  begin
    FLayerStack.Show;
    dec(FTopMostInfo.layerstackHidden);
  end;
  if assigned(FImageList) and (AInfo.imagelistHidden > 0) then
  begin
    FImageList.Show;
    dec(FTopMostInfo.imagelistHidden);
  end;
end;

procedure TLazPaintInstance.UpdateWindows;
begin
  if Assigned(FMain) then FMain.Update;
  if Assigned(FToolbox) then FToolbox.Update;
  if Assigned(FChooseColor) then FChooseColor.Update;
  if Assigned(FLayerStack) then FLayerStack.Update;
  if Assigned(FImageList) then FImageList.Update;
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

function TLazPaintInstance.TryOpenFileUTF8(filename: string): boolean;
begin
  FormsNeeded;
  result := FMain.TryOpenFileUTF8(filename);
end;

function TLazPaintInstance.ExecuteFilter(filter: TPictureFilter;
  skipDialog: boolean): boolean;
var vars: TVariableSet;
begin
  if filter = pfNone then exit;
  vars := TVariableSet.Create('Filter');
  vars.AddString('Name',PictureFilterStr[filter]);
  Result:= UFilters.ExecuteFilter(self, filter, vars, skipDialog);
  vars.Free;
end;

procedure TLazPaintInstance.ColorFromFChooseColor;
begin
  FormsNeeded;
  if InColorFromFChooseColor then exit;
  InColorFromFChooseColor := True;
  if FChooseColor.colorTarget = ctForeColor then
    ToolManager.ToolForeColor := FChooseColor.GetCurrentColor else
  if FChooseColor.colorTarget = ctBackColor then
    ToolManager.ToolBackColor := FChooseColor.GetCurrentColor;
  FMain.UpdateToolbar;
  FMain.UpdateEditPicture;
  InColorFromFChooseColor := false;
end;

procedure TLazPaintInstance.ColorToFChooseColor;
begin
  if not Assigned(FChooseColor) or InColorFromFChooseColor then exit;
  if FChooseColor.colorTarget = ctForeColor then
    FChooseColor.SetCurrentColor(ToolManager.ToolForeColor) else
  if FChooseColor.colorTarget = ctBackColor then
    FChooseColor.SetCurrentColor(ToolManager.ToolBackColor);
end;

procedure TLazPaintInstance.ShowCanvasSizeDlg;
var topmostInfo: TTopMostInfo;
begin
  FormsNeeded;
  topmostInfo := HideTopmost;
  try
    FCanvasSize.repeatImage := False;
    if FCanvasSize.ShowModal = mrOk then
      Image.Assign(FCanvasSize.canvasSizeResult, true, True);
  except
    on ex:Exception do
      ShowError('ShowCanvasSizeDlg',ex.Message);
  end;
  ShowTopmost(topmostInfo);
end;

procedure TLazPaintInstance.ShowRepeatImageDlg;
var topmostInfo: TTopMostInfo;
begin
  FormsNeeded;
  topmostInfo := HideTopmost;
  try
    FCanvasSize.repeatImage := True;
    if FCanvasSize.ShowModal = mrOk then
      image.Assign(FCanvasSize.canvasSizeResult,true,True);
  except
    on ex:Exception do
      ShowError('ShowRepeatImageDlg',ex.Message);
  end;
  ShowTopmost(topmostInfo);
end;

procedure TLazPaintInstance.MoveToolboxTo(X, Y: integer);
begin
  FormsNeeded;
  FToolbox.Left := X;
  FToolbox.Top := Y;
end;

procedure TLazPaintInstance.MoveChooseColorTo(X, Y: integer);
begin
  FormsNeeded;
  FChooseColor.Left := X;
  FChooseColor.Top := Y;
end;

procedure TLazPaintInstance.MoveLayerWindowTo(X, Y: integer);
begin
  if FLayerStack <> nil then
  begin
    FLayerStack.Left := X;
    FLayerStack.Top := Y;
  end;
end;

procedure TLazPaintInstance.MoveImageListWindowTo(X, Y: integer);
begin
  FormsNeeded;
  FImageList.Left := X;
  FImageList.Top := Y;
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

function TLazPaintInstance.ShowNewImageDlg(out bitmap: TBGRABitmap
  ): boolean;
var tx,ty: integer;
begin
  FormsNeeded;

  Result:= unewimage.ShowNewImageDlg(self,tx,ty);
  if result then
    bitmap := MakeNewBitmapReplacement(tx,ty)
  else
    bitmap := nil;
end;

function TLazPaintInstance.ShowResampleDialog(AParameters: TVariableSet): boolean;
begin
  FormsNeeded;
  Result:= uresample.ShowResampleDialog(self,AParameters);
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
      FMain.StackNeedUpdate := true;
    end else
      NotifyStackChange;
  end;
end;

function TLazPaintInstance.MakeNewBitmapReplacement(AWidth, AHeight: integer): TBGRABitmap;
begin
  result := TBGRABitmap.Create(AWidth,AHeight);
end;

procedure TLazPaintInstance.ChooseTool(Tool: TPaintToolType);
begin
  FormsNeeded;
  FMain.ChooseTool(Tool);
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
  if FDestroying then exit;

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
  Result:= FChooseColor.colorTarget;
end;

procedure TLazPaintInstance.SetChooseColorTarget(const AValue: TColorTarget);
begin
  FChooseColor.colorTarget:= AValue;
  ColorToFChooseColor;
end;

function TLazPaintInstance.ShowColorIntensityDlg(AParameters: TVariableSet
  ): boolean;
var oldSelectionNormal: boolean;
begin
  result := false;
  FormsNeeded;
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  try
    result := FColorIntensity.ShowModal(self,ciIntensity,AParameters) = mrOK;
  except
    on ex:Exception do
      ShowError('ShowColorIntensityDlg',ex.Message);
  end;
  ShowSelectionNormal := oldSelectionNormal;
end;

function TLazPaintInstance.ShowColorLightnessDlg(AParameters: TVariableSet
  ): boolean;
var oldSelectionNormal: boolean;
begin
  result := false;
  FormsNeeded;
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  try
    result := FColorIntensity.ShowModal(self,ciLightness,AParameters) = mrOk;
  except
    on ex:Exception do
      ShowError('ShowColorLightnessDlg',ex.Message);
  end;
  ShowSelectionNormal := oldSelectionNormal;
end;

function TLazPaintInstance.ShowShiftColorsDlg(AParameters: TVariableSet
  ): boolean;
var oldSelectionNormal: boolean;
begin
  result := false;
  FormsNeeded;
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  try
    result := FShiftColors.ShowModal(self,AParameters) = mrOk;
  except
    on ex:Exception do
      ShowError('ShowShiftColorsDlg',ex.Message);
  end;
  ShowSelectionNormal := oldSelectionNormal;
end;

function TLazPaintInstance.ShowColorizeDlg(AParameters: TVariableSet): boolean;
var oldSelectionNormal: boolean;
begin
  result := false;
  FormsNeeded;
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  try
    result := FColorize.ShowModal(self,AParameters) = mrOk;
  except
    on ex:Exception do
      ShowError('ShowColorizeDlg',ex.Message);
  end;
  ShowSelectionNormal := oldSelectionNormal;
end;

function TLazPaintInstance.ShowColorCurvesDlg(AParameters: TVariableSet
  ): boolean;
var oldSelectionNormal: boolean;
begin
  result := false;
  FormsNeeded;
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  try
    result := FColorCurves.ShowModal(self,AParameters) = mrOk;
  except
    on ex:Exception do
      ShowError('ShowColorCurvesDlg',ex.Message);
  end;
  ShowSelectionNormal := oldSelectionNormal;
end;

function TLazPaintInstance.ShowRadialBlurDlg(AFilterConnector: TObject; blurType: TRadialBlurType): boolean;
var oldSelectionNormal: boolean;
    top: TTopMostInfo;
begin
  top := self.HideTopmost;
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  result := uradialblur.ShowRadialBlurDlg(AFilterConnector,blurType);
  ShowSelectionNormal := oldSelectionNormal;
  self.ShowTopmost(top);
end;

function TLazPaintInstance.ShowMotionBlurDlg(AFilterConnector: TObject):boolean;
var oldSelectionNormal: boolean;
    top: TTopMostInfo;
begin
  top := self.HideTopmost;
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  result := umotionblur.ShowMotionBlurDlg(AFilterConnector);
  ShowSelectionNormal := oldSelectionNormal;
  self.ShowTopmost(top);
end;

function TLazPaintInstance.ShowCustomBlurDlg(AFilterConnector: TObject):boolean;
var oldSelectionNormal: boolean;
    top: TTopMostInfo;
begin
  top := self.HideTopmost;
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  result := FCustomBlur.ShowDlg(AFilterConnector);
  ShowSelectionNormal := oldSelectionNormal;
  self.ShowTopmost(top);
end;

function TLazPaintInstance.ShowEmbossDlg(AFilterConnector: TObject): boolean;
var oldSelectionNormal: boolean;
    top: TTopMostInfo;
begin
  top := self.HideTopmost;
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  result := uemboss.ShowEmbossDlg(AFilterConnector);
  ShowSelectionNormal := oldSelectionNormal;
  self.ShowTopmost(top);
end;

function TLazPaintInstance.ShowPixelateDlg(AFilterConnector: TObject): boolean;
var oldSelectionNormal: boolean;
    top: TTopMostInfo;
begin
  top := self.HideTopmost;
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  result := upixelate.ShowPixelateDlg(AFilterConnector);
  ShowSelectionNormal := oldSelectionNormal;
  self.ShowTopmost(top);
end;

function TLazPaintInstance.ShowNoiseFilterDlg(AFilterConnector: TObject
  ): boolean;
var oldSelectionNormal: boolean;
    top: TTopMostInfo;
begin
  top := self.HideTopmost;
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  result := unoisefilter.ShowNoiseFilterDlg(AFilterConnector);
  ShowSelectionNormal := oldSelectionNormal;
  self.ShowTopmost(top);
end;

function TLazPaintInstance.ShowTwirlDlg(AFilterConnector: TObject): boolean;
var oldSelectionNormal: boolean;
    top: TTopMostInfo;
begin
  top := self.HideTopmost;
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  result := utwirl.ShowTwirlDlg(AFilterConnector);
  ShowSelectionNormal := oldSelectionNormal;
  self.ShowTopmost(top);
end;

function TLazPaintInstance.ShowPhongFilterDlg(AFilterConnector: TObject): boolean;
var oldSelectionNormal: boolean;
    top: TTopMostInfo;
begin
  top := self.HideTopmost;
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  result := UPhongFilter.ShowPhongFilterDlg(AFilterConnector);
  ShowSelectionNormal := oldSelectionNormal;
  self.ShowTopmost(top);
end;

function TLazPaintInstance.ShowFunctionFilterDlg(AFilterConnector: TObject
  ): boolean;
var oldSelectionNormal: boolean;
    top: TTopMostInfo;
begin
  top := self.HideTopmost;
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  result := UFilterFunction.ShowFilterFunctionDlg(AFilterConnector);
  ShowSelectionNormal := oldSelectionNormal;
  self.ShowTopmost(top);
end;

function TLazPaintInstance.ShowSharpenDlg(AFilterConnector: TObject): boolean;
var oldSelectionNormal: boolean;
    top: TTopMostInfo;
begin
  top := self.HideTopmost;
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  result := USharpen.ShowSharpenDlg(AFilterConnector,smSharpen);
  ShowSelectionNormal := oldSelectionNormal;
  self.ShowTopmost(top);
end;

function TLazPaintInstance.ShowPosterizeDlg(AParameters: TVariableSet): boolean;
var oldSelectionNormal: boolean;
    top: TTopMostInfo;
begin
  top := self.HideTopmost;
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  result := uposterize.ShowPosterizeDlg(self, AParameters);
  ShowSelectionNormal := oldSelectionNormal;
  self.ShowTopmost(top);
end;

procedure TLazPaintInstance.ShowPrintDlg;
var f: TFPrint;
    wasVisible: boolean;
begin
  wasVisible := false;
  if (FMain <> nil) and FMain.Visible then
  begin
    wasVisible := true;
    FMain.Hide;
  end;
  f := TFPrint.Create(nil);
  f.Instance := self;
  f.ShowModal;
  f.Free;
  if (FMain <> nil) and wasVisible then FMain.Show;
end;

function TLazPaintInstance.OpenImage (FileName: string; AddToRecent: Boolean=True): boolean;
begin
  FormsNeeded;
  Result:= FMain.TryOpenFileUTF8(FileName, AddToRecent);
end;

procedure TLazPaintInstance.ZoomFit;
begin
    FMain.Zoom.ZoomFit(Image.Width,Image.Height,FMain.GetPictureArea);
end;

procedure TLazPaintInstance.AddToImageList(const FileNames: array of String);
begin
  if FImageList <> nil then
    FImageList.AddFiles (FileNames);
end;

end.
