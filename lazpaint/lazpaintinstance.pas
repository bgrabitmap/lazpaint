unit LazpaintInstance;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazPaintType, BGRABitmap, BGRABitmapTypes, BGRALayers,

  LazPaintMainForm,

  utoolbox, uchoosecolor, ulayerstack, ucanvassize,
  ucolorintensity, ushiftcolors, ucolorize,
  ucustomblur, umultiimage,

  ULoading, uimage, utool, uconfig, IniFiles, uresourcestrings;

const
  MaxToolPopupShowCount = 2;

type

  { TLazPaintInstance }

  TLazPaintInstance = class(TLazPaintCustomInstance)
  private
    function GetMainFormVisible: boolean;
    procedure OnLayeredBitmapLoadStartHandler(AFilename: string);
    procedure OnLayeredBitmapLoadProgressHandler(APercentage: integer);
    procedure OnLayeredBitmapLoadedHandler;
    procedure SelectionInstanceOnRun(AInstance: TLazPaintCustomInstance);

  protected
    InColorFromFChooseColor: boolean;
    FMain: TFMain;
    FToolbox: TFToolbox;
    FChooseColor: TFChooseColor;
    FLayerStack: TFLayerStack;
    FCanvasSize: TFCanvasSize;
    FColorIntensity: TFColorIntensity;
    FShiftColors: TFShiftColors;
    FColorize: TFColorize;
    FCustomBlur: TFCustomBlur;
    FLoading: TFLoading;
    toolboxHidden, choosecolorHidden, layerstackHidden: boolean;
    FGridVisible: boolean;
    FConfig: TLazPaintConfig;
    FImage: TLazPaintImage;
    FToolManager : TToolManager;
    FEmbedded: boolean;
    FDestroying: boolean;
    FSelectionEditConfig: TStream;
    FTextureEditConfig: TStream;

    function GetShowSelectionNormal: boolean; override;
    procedure SetShowSelectionNormal(AValue: boolean); override;
    function GetEmbedded: boolean; override;
    function GetGridVisible: boolean; override;
    procedure SetGridVisible(const AValue: boolean); override;
    function GetChooseColorVisible: boolean; override;
    function GetToolboxVisible: boolean; override;
    procedure SetChooseColorVisible(const AValue: boolean); override;
    procedure SetToolboxVisible(const AValue: boolean); override;
    function GetChooseColorHeight: integer; override;
    function GetChooseColorWidth: integer; override;
    function GetToolboxHeight: integer; override;
    function GetToolboxWidth: integer; override;
    function GetTopMostHasFocus: boolean; override;
    function GetTopMostVisible: boolean; override;
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

    function GetLayerWindowHeight: integer; override;
    function GetLayerWindowWidth: integer; override;
    function GetLayerWindowVisible: boolean; override;
    procedure SetLayerWindowVisible(AValue: boolean); override;

  public
    constructor Create; override;
    constructor Create(AEmbedded: boolean); override;
    procedure UseConfig(ini: TInifile); override;
    procedure AssignBitmap(bmp: TBGRABitmap); override;
    procedure EditBitmap(var bmp: TBGRABitmap; ConfigStream: TStream = nil; ATitle: String = ''; AOnRun: TLazPaintInstanceEvent = nil; AOnExit: TLazPaintInstanceEvent = nil; ABlackAndWhite: boolean = false); override;
    procedure EditSelection; override;
    procedure EditTexture; override;
    function ProcessCommandLine: boolean; override;
    function ProcessCommands(commands: TStringList): boolean; override;
    procedure Show; override;
    procedure Run; override;
    destructor Destroy; override;
    procedure NotifyImageChange(RepaintNow: boolean; ARect: TRect); override;
    procedure NotifyImageChangeCompletely(RepaintNow: boolean); override;
    function TryOpenFile(filename: string): boolean; override;
    function ExecuteFilter(filter: TPictureFilter; skipDialog: boolean = false): boolean; override;
    procedure ColorFromFChooseColor; override;
    procedure ColorToFChooseColor; override;
    procedure ShowColorIntensityDlg; override;
    procedure ShowColorLightnessDlg; override;
    procedure ShowShiftColorsDlg; override;
    procedure ShowColorizeDlg; override;
    function ShowRadialBlurDlg(AFilterConnector: TObject;blurType:TRadialBlurType):boolean; override;
    function ShowMotionBlurDlg(AFilterConnector: TObject):boolean; override;
    function ShowCustomBlurDlg(AFilterConnector: TObject):boolean; override;
    function ShowEmbossDlg(AFilterConnector: TObject):boolean; override;
    function ShowPixelateDlg(AFilterConnector: TObject):boolean; override;
    function ShowTwirlDlg(AFilterConnector: TObject):boolean; override;
    function ShowPhongFilterDlg(AFilterConnector: TObject): boolean; override;
    function ShowFunctionFilterDlg(AFilterConnector: TObject): boolean; override;
    function ShowSharpenDlg(AFilterConnector: TObject):boolean; override;
    procedure HideTopmost; override;
    procedure ShowTopmost; override;
    procedure ShowCanvasSizeDlg; override;
    procedure ShowRepeatImageDlg; override;
    procedure MoveToolboxTo(X,Y: integer); override;
    procedure MoveChooseColorTo(X,Y: integer); override;
    procedure MoveLayerWindowTo(X,Y: integer); override;
    procedure ShowAboutDlg; override;
    function ShowNewImageDlg(out bitmap: TBGRABitmap):boolean; override;
    function ShowResampleDialog:boolean; override;
    property MainFormVisible: boolean read GetMainFormVisible;
    procedure NotifyStackChange; override;

    procedure NewLayer; override;
    procedure NewLayer(ALayer: TBGRABitmap; AName: string); override;
    procedure DuplicateLayer; override;
    procedure RemoveLayer; override;
    procedure MergeLayerOver; override;
    function MakeNewBitmapReplacement(AWidth, AHeight: integer): TBGRABitmap; override;
    procedure ChooseTool(Tool : TPaintToolType); override;

  end;

implementation

uses Forms, Dialogs, Controls, FileUtil,

     uradialblur, umotionblur, uemboss, utwirl,
     unewimage, uresample, upixelate, ufilters,
     UImageAction, USharpen, UPhongFilter, UFilterFunction,

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

procedure TLazPaintInstance.Init(AEmbedded: boolean);
begin
  Title := 'LazPaint ' + LazPaintCurrentVersion;
  toolboxHidden := false;
  choosecolorHidden := false;
  layerstackHidden := false;
  FEmbedded:= AEmbedded;

  InColorFromFChooseColor := false;
  FImage := TLazPaintImage.Create;
  FImage.OnStackChanged:= @OnStackChanged;
  FToolManager := TToolManager.Create(FImage, nil, BlackAndWhite);
  FToolManager.OnPopup := @OnToolPopup;
  UseConfig(TIniFile.Create(''));
  FSelectionEditConfig := nil;
  FTextureEditConfig := nil;

  if not AEmbedded then
    BGRALayers.RegisterLoadingHandler(@OnLayeredBitmapLoadStartHandler,@OnLayeredBitmapLoadProgressHandler,@OnLayeredBitmapLoadedHandler)
  else
    FLoading := nil;
end;

procedure TLazPaintInstance.FormsNeeded;
begin
  if FMain <> nil then exit;

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
  FLayerStack.AddButton(FMain.LayerMove);
  FLayerStack.AddButton(FMain.LayerRotate);
  FLayerStack.AddButton(FMain.ToolLayerMapping);
  FLayerStack.AddButton(FMain.LayerHorizontalFlip);
  FLayerStack.AddButton(FMain.LayerVerticalFlip);
  FLayerStack.AddButton(FMain.LayerDuplicate);
  FLayerStack.AddButton(FMain.LayerFromFile);
  FLayerStack.AddButton(FMain.LayerMergeOver);
  FLayerStack.AddButton(FMain.LayerRemoveCurrent);

  Application.CreateForm(TFChooseColor, FChooseColor);
  FChooseColor.LazPaintInstance := self;
  Application.CreateForm(TFCanvasSize, FCanvasSize);
  FCanvasSize.LazPaintInstance := self;
  Application.CreateForm(TFColorIntensity, FColorIntensity);
  Application.CreateForm(TFShiftColors, FShiftColors);
  Application.CreateForm(TFColorize, FColorize);
  Application.CreateForm(TFCustomBlur, FCustomBlur);
  FCustomBlur.LazPaintInstance := self;
end;

procedure TLazPaintInstance.UseConfig(ini: TInifile);
begin
  FreeAndNil(FConfig);
  FConfig := TLazPaintConfig.Create(ini);

  ToolManager.ToolForeColor := Config.DefaultToolForeColor;
  ToolManager.ToolBackColor := Config.DefaultToolBackColor;
  ToolManager.ToolPenWidth := Config.DefaultToolPenWidth;
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
    MessagePopup(messageStr,2000);
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

function TLazPaintInstance.GetMainFormVisible: boolean;
begin
  if FMain <> nil then
    result := FMain.Visible
  else
    result := false;
end;

procedure TLazPaintInstance.OnLayeredBitmapLoadStartHandler(AFilename: string);
begin
  if FLoading = nil then
  begin
    FLoading := TFLoading.Create(nil);
    FLoading.ShowMessage(rsOpening+' ' +AFilename+'...');
  end;
end;

procedure TLazPaintInstance.OnLayeredBitmapLoadProgressHandler(
  APercentage: integer);
begin
  if FLoading <> nil then
    FLoading.ShowMessage(rsLoading+' (' +inttostr(APercentage)+'%)');
end;

procedure TLazPaintInstance.OnLayeredBitmapLoadedHandler;
begin
  if FLoading <> nil then
  begin
    FreeAndNil(FLoading);
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

function TLazPaintInstance.GetGridVisible: boolean;
begin
  Result:= FGridVisible;
end;

procedure TLazPaintInstance.SetGridVisible(const AValue: boolean);
begin
  FGridVisible := AValue;
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

procedure TLazPaintInstance.SetChooseColorVisible(const AValue: boolean);
begin
  if FChooseColor <> nil then
    FChooseColor.Visible := AValue;
end;

procedure TLazPaintInstance.SetToolboxVisible(const AValue: boolean);
begin
  if FToolbox <> nil then
    FToolbox.Visible := AValue;
end;

function TLazPaintInstance.GetChooseColorHeight: integer;
begin
  Result:= FChooseColor.Height;
end;

function TLazPaintInstance.GetChooseColorWidth: integer;
begin
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
begin
  subLaz := TLazPaintInstance.Create(True);
  subLaz.BlackAndWhite := ABlackAndWhite;
  if ATitle <> '' then subLaz.Title := ATitle;
  if FMain <> nil then FMain.Enabled := false;
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
      ShowMessage(ex.Message);
  end;
  if FMain <> nil then FMain.Enabled := true;
  subLaz.Free;
end;

procedure TLazPaintInstance.EditSelection;
var lSelection,lTemp: TBGRABitmap;
    LayerAction: TLayerAction;
begin
  if not image.CheckNoAction then exit;
  HideTopmost;
  try
    if FSelectionEditConfig = nil then
      FSelectionEditConfig := TStringStream.Create('[Tool]'+LineEnding+
        'ForeColor=FFFFFFFF'+LineEnding+
        'BackColor=000000FF');
    LayerAction := TLayerAction.Create(Image);
    LayerAction.QuerySelection;
    lSelection:= LayerAction.currentSelection.Duplicate as TBGRABitmap;
    lSelection.LinearAntialiasing := False;
    lSelection.ConvertFromLinearRGB;
    EditBitmap(lSelection,FSelectionEditConfig,rsEditSelection,@SelectionInstanceOnRun,nil,True);
    lSelection.InplaceGrayscale;
    lTemp := TBGRABitmap.Create(lSelection.Width,lSelection.Height,BGRABlack);
    lTemp.PutImage(0,0,lSelection,dmDrawWithTransparency);
    lSelection.Free;
    lSelection := lTemp;
    lTemp := nil;
    lSelection.ConvertToLinearRGB;
    lSelection.LinearAntialiasing := True;
    LayerAction.ReplaceCurrentSelection(lSelection);
    LayerAction.Validate;
    LayerAction.Free;
    Image.ImageMayChangeCompletely;
  except
    on ex: Exception do
      ShowMessage(ex.Message);
  end;
  ShowTopmost;
end;

procedure TLazPaintInstance.EditTexture;
var tex: TBGRABitmap;
begin
  HideTopmost;
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
      ShowMessage(ex.Message);
  end;
  ShowTopmost;
end;

procedure TLazPaintInstance.SelectionInstanceOnRun(AInstance: TLazPaintCustomInstance);
begin
  AInstance.Config.SetDefaultImageWidth(Image.Width);
  AInstance.Config.SetDefaultImageHeight(Image.Height);
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
    commands.Add( ConvertToUTF8IfNeeded(ParamStr(i)));
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
    Config.SetDefaultToolboxWindowVisible(FToolbox.Visible or toolboxHidden);
  if FChooseColor <> nil then
    Config.SetDefaultColorWindowVisible(FChooseColor.Visible or choosecolorHidden);
  if FLayerStack <> nil then
    Config.SetDefaultLayerWindowVisible(FLayerStack.Visible or layerstackHidden);
  if FToolbox <> nil then
    Config.SetDefaultToolboxWindowPosition(FToolBox.BoundsRect);
  if FChooseColor <> nil then
    Config.SetDefaultColorWindowPosition(FChooseColor.BoundsRect);
  if FLayerStack <> nil then
    Config.SetDefaultLayerWindowPosition(FLayerStack.BoundsRect);

  Config.SetDefaultToolForeColor(ToolManager.ToolForeColor);
  Config.SetDefaultToolBackColor(ToolManager.ToolBackColor);
  Config.SetDefaultToolPenWidth(ToolManager.ToolPenWidth);
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

  if FLoading <> nil then
  begin
    FreeAndNil(FLoading);
    BGRALayers.UnregisterLoadingHandler(@OnLayeredBitmapLoadStartHandler,@OnLayeredBitmapLoadProgressHandler,@OnLayeredBitmapLoadedHandler);
  end;
  FreeAndNil(FLayerStack);
  FreeAndNil(FCustomBlur);
  FreeAndNil(FColorize);
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
  inherited Destroy;
end;

procedure TLazPaintInstance.HideTopmost;
begin
  if FDestroying then exit;

  FormsNeeded;
  if (FToolBox <> nil) and FToolBox.Visible then
  begin
    FToolbox.Hide;
    toolboxHidden := true;
  end;
  if (FChooseColor <> nil) and FChooseColor.Visible then
  begin
    FChooseColor.Hide;
    choosecolorHidden := true;
  end;
  if (FLayerStack <> nil) and FLayerStack.Visible then
  begin
    FLayerStack.Hide;
    layerstackHidden := true;
  end;
end;

procedure TLazPaintInstance.ShowTopmost;
begin
  if FDestroying then exit;

  FormsNeeded;
  if toolboxHidden then
  begin
    FToolbox.Show;
    toolboxHidden := false;
  end;
  if choosecolorHidden then
  begin
    FChooseColor.Show;
    choosecolorHidden := false;
  end;
  if layerstackHidden then
  begin
    FLayerStack.Show;
    layerstackHidden := false;
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

function TLazPaintInstance.TryOpenFile(filename: string): boolean;
begin
  FormsNeeded;
  result := FMain.TryOpenFile(filename);
end;

function TLazPaintInstance.ExecuteFilter(filter: TPictureFilter;
  skipDialog: boolean): boolean;
begin
  Result:= UFilters.ExecuteFilter(self, filter, skipDialog);
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
  FormsNeeded;
  if InColorFromFChooseColor then exit;
  if FChooseColor.colorTarget = ctForeColor then
    FChooseColor.SetCurrentColor(ToolManager.ToolForeColor) else
  if FChooseColor.colorTarget = ctBackColor then
    FChooseColor.SetCurrentColor(ToolManager.ToolBackColor);
end;

procedure TLazPaintInstance.ShowCanvasSizeDlg;
begin
  FormsNeeded;
  HideTopmost;
  try
    FCanvasSize.repeatImage := False;
    if FCanvasSize.ShowModal = mrOk then
      Image.Assign(FCanvasSize.canvasSizeResult, true, True);
  except
    on ex:Exception do
      ShowMessage('ShowCanvasSizeDlg: '+ex.Message);
  end;
  ShowTopmost;
end;

procedure TLazPaintInstance.ShowRepeatImageDlg;
begin
  FormsNeeded;
  HideTopmost;
  try
    FCanvasSize.repeatImage := True;
    if FCanvasSize.ShowModal = mrOk then
      image.Assign(FCanvasSize.canvasSizeResult,true,True);
  except
    on ex:Exception do
      ShowMessage('ShowRepeatImageDlg: '+ex.Message);
  end;
  ShowTopmost;
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

procedure TLazPaintInstance.ShowAboutDlg;
begin
  uabout.ShowAboutDlg(AboutText);
end;

function TLazPaintInstance.ShowNewImageDlg(out bitmap: TBGRABitmap
  ): boolean;
begin
  FormsNeeded;
  Result:= unewimage.ShowNewImageDlg(self,bitmap);
end;

function TLazPaintInstance.ShowResampleDialog: boolean;
begin
  FormsNeeded;
  HideTopmost;
  Result:= uresample.ShowResampleDialog(self);
  ShowTopmost;
end;

procedure TLazPaintInstance.NotifyStackChange;
begin
  OnStackChanged(image,False);
end;

procedure TLazPaintInstance.NewLayer;
begin
  if not image.SelectionLayerIsEmpty then
    if MessageDlg(rsTransferSelectionToOtherLayer,mtConfirmation,[mbOk,mbCancel],0) <> mrOk then
      exit;
  if image.NbLayers < MaxLayersToAdd then
  begin
    Image.AddNewLayer;
    if FLayerStack <> nil then FLayerStack.SetLayerStackScrollPos(0,0);
  end;
end;

procedure TLazPaintInstance.NewLayer(ALayer: TBGRABitmap; AName: string);
begin
  if image.NbLayers < MaxLayersToAdd then
  begin
    Image.AddNewLayer(ALayer, AName);
    if FLayerStack <> nil then FLayerStack.SetLayerStackScrollPos(0,0);
  end;
end;

procedure TLazPaintInstance.DuplicateLayer;
begin
  if image.NbLayers < MaxLayersToAdd then
  begin
    Image.DuplicateLayer;
    if FLayerStack <> nil then FLayerStack.SetLayerStackScrollPosOnItem(Image.currentImageLayerIndex);
  end;
end;

procedure TLazPaintInstance.MergeLayerOver;
begin
  if (Image.currentImageLayerIndex <> -1) and (image.NbLayers > 1) then
  begin
    Image.MergeLayerOver;
    if FLayerStack <> nil then FLayerStack.SetLayerStackScrollPosOnItem(Image.currentImageLayerIndex);
  end;
end;

procedure TLazPaintInstance.RemoveLayer;
var idx: integer;
begin
  if (Image.currentImageLayerIndex <> -1) and (Image.NbLayers > 1) then
  begin
    idx := Image.currentImageLayerIndex;
    Image.RemoveLayer;
    if FLayerStack <> nil then FLayerStack.SetLayerStackScrollPosOnItem(Idx);
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

function TLazPaintInstance.GetChooseColorTarget: TColorTarget;
begin
  Result:= FChooseColor.colorTarget;
end;

procedure TLazPaintInstance.SetChooseColorTarget(const AValue: TColorTarget);
begin
  FChooseColor.colorTarget:= AValue;
  ColorToFChooseColor;
end;

procedure TLazPaintInstance.ShowColorIntensityDlg;
var oldSelectionNormal: boolean;
begin
  FormsNeeded;
  HideTopMost;
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  try
    FColorIntensity.ShowModal(self,ciIntensity);
  except
    on ex:Exception do
      ShowMessage('ShowColorIntensityDlg: '+ex.Message);
  end;
  ShowSelectionNormal := oldSelectionNormal;
  ShowTopMost;
end;

procedure TLazPaintInstance.ShowColorLightnessDlg;
var oldSelectionNormal: boolean;
begin
  FormsNeeded;
  HideTopMost;
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  try
    FColorIntensity.ShowModal(self,ciLightness);
  except
    on ex:Exception do
      ShowMessage('ShowColorLightnessDlg: '+ex.Message);
  end;
  ShowSelectionNormal := oldSelectionNormal;
  ShowTopMost;
end;

procedure TLazPaintInstance.ShowShiftColorsDlg;
var oldSelectionNormal: boolean;
begin
  FormsNeeded;
  HideTopMost;
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  try
    FShiftColors.ShowModal(self);
  except
    on ex:Exception do
      ShowMessage('ShowShiftColorsDlg: '+ex.Message);
  end;
  ShowSelectionNormal := oldSelectionNormal;
  ShowTopMost;
end;

procedure TLazPaintInstance.ShowColorizeDlg;
var oldSelectionNormal: boolean;
begin
  FormsNeeded;
  HideTopMost;
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  try
    FColorize.ShowModal(self);
  except
    on ex:Exception do
      ShowMessage('ShowColorizeDlg: '+ex.Message);
  end;
  ShowSelectionNormal := oldSelectionNormal;
  ShowTopMost;
end;

function TLazPaintInstance.ShowRadialBlurDlg(AFilterConnector: TObject; blurType: TRadialBlurType): boolean;
var oldSelectionNormal: boolean;
begin
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  result := uradialblur.ShowRadialBlurDlg(AFilterConnector,blurType);
  ShowSelectionNormal := oldSelectionNormal;
end;

function TLazPaintInstance.ShowMotionBlurDlg(AFilterConnector: TObject):boolean;
var oldSelectionNormal: boolean;
begin
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  result := umotionblur.ShowMotionBlurDlg(AFilterConnector);
  ShowSelectionNormal := oldSelectionNormal;
end;

function TLazPaintInstance.ShowCustomBlurDlg(AFilterConnector: TObject):boolean;
var oldSelectionNormal: boolean;
begin
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  result := FCustomBlur.ShowDlg(AFilterConnector);
  ShowSelectionNormal := oldSelectionNormal;
end;

function TLazPaintInstance.ShowEmbossDlg(AFilterConnector: TObject): boolean;
var oldSelectionNormal: boolean;
begin
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  result := uemboss.ShowEmbossDlg(AFilterConnector);
  ShowSelectionNormal := oldSelectionNormal;
end;

function TLazPaintInstance.ShowPixelateDlg(AFilterConnector: TObject): boolean;
var oldSelectionNormal: boolean;
begin
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  result := upixelate.ShowPixelateDlg(AFilterConnector);
  ShowSelectionNormal := oldSelectionNormal;
end;

function TLazPaintInstance.ShowTwirlDlg(AFilterConnector: TObject): boolean;
var oldSelectionNormal: boolean;
begin
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  result := utwirl.ShowTwirlDlg(AFilterConnector);
  ShowSelectionNormal := oldSelectionNormal;
end;

function TLazPaintInstance.ShowPhongFilterDlg(AFilterConnector: TObject): boolean;
var oldSelectionNormal: boolean;
begin
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  result := UPhongFilter.ShowPhongFilterDlg(AFilterConnector);
  ShowSelectionNormal := oldSelectionNormal;
end;

function TLazPaintInstance.ShowFunctionFilterDlg(AFilterConnector: TObject
  ): boolean;
var oldSelectionNormal: boolean;
begin
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  result := UFilterFunction.ShowFilterFunctionDlg(AFilterConnector);
  ShowSelectionNormal := oldSelectionNormal;
end;

function TLazPaintInstance.ShowSharpenDlg(AFilterConnector: TObject): boolean;
var oldSelectionNormal: boolean;
begin
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  result := USharpen.ShowSharpenDlg(AFilterConnector,smSharpen);
  ShowSelectionNormal := oldSelectionNormal;
end;

end.

