unit LazpaintInstance;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazPaintType, BGRABitmap, BGRABitmapTypes, BGRALayers,

  LazPaintMainForm,

  utoolbox, uchoosecolor, ulayerstack, ucanvassize,
  ucolorintensity, ushiftcolors, ucolorize,
  ucustomblur, umultiimage,

  uimage, utool, uconfig, IniFiles, uresourcestrings;

type

  { TLazPaintInstance }

  TLazPaintInstance = class(TLazPaintCustomInstance)
  private
    function GetMainFormVisible: boolean;
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
    FMultiImage: TFMultiImage;
    toolboxHidden, choosecolorHidden, layerstackHidden: boolean;
    FGridVisible: boolean;
    FConfig: TLazPaintConfig;
    FImage: TLazPaintImage;
    FToolManager : TToolManager;
    FEmbedded: boolean;
    FDestroying: boolean;

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
    function GetChooseColorTarget: TColorTarget; override;
    procedure SetChooseColorTarget(const AValue: TColorTarget); override;
    function GetConfig: TLazPaintConfig; override;
    function GetImage: TLazPaintImage; override;
    function GetToolManager: TToolManager; override;
    procedure FormsNeeded;
    procedure Init(AEmbedded: boolean);
    procedure SetBlackAndWhite(AValue: boolean); override;
    procedure OnStackChanged({%H-}sender: TLazPaintImage; AScrollIntoView: boolean);

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
    procedure ShowColorIntensityDlg(activeLayer: TBGRABitmap); override;
    procedure ShowColorLightnessDlg(activeLayer: TBGRABitmap); override;
    procedure ShowShiftColorsDlg(activeLayer: TBGRABitmap); override;
    procedure ShowColorizeDlg(activeLayer: TBGRABitmap); override;
    function ShowRadialBlurDlg(layer:TBGRABitmap; out filteredLayer: TBGRABitmap;blurType:TRadialBlurType):boolean; override;
    function ShowMotionBlurDlg(layer:TBGRABitmap; out filteredLayer: TBGRABitmap):boolean; override;
    function ShowCustomBlurDlg(layer:TBGRABitmap; out filteredLayer: TBGRABitmap): boolean; override;
    function ShowEmbossDlg(layer:TBGRABitmap; out filteredLayer: TBGRABitmap):boolean; override;
    function ShowPixelateDlg(layer:TBGRABitmap; out filteredLayer: TBGRABitmap):boolean; override;
    function ShowTwirlDlg(layer:TBGRABitmap; out filteredLayer: TBGRABitmap):boolean; override;
    procedure HideTopmost; override;
    procedure ShowTopmost; override;
    procedure ShowCanvasSizeDlg; override;
    procedure ShowRepeatImageDlg; override;
    procedure MoveToolboxTo(X,Y: integer); override;
    procedure MoveChooseColorTo(X,Y: integer); override;
    procedure MoveLayerWindowTo(X,Y: integer); override;
    function ChooseImage(images: ArrayOfBGRABitmap): TBGRABitmap; override;
    procedure ShowAboutDlg; override;
    function ShowNewImageDlg(out bitmap: TBGRABitmap):boolean; override;
    function ShowResampleDialog:boolean; override;
    property MainFormVisible: boolean read GetMainFormVisible;
    procedure NotifyStackChange; override;

    procedure DoHorizontalFlip(AOption: TFlipOption); override;
    procedure DoVerticalFlip(AOption: TFlipOption); override;
    procedure DoRotateCW; override;
    procedure DoRotateCCW; override;
    procedure DoSmartZoom3; override;
    function MakeNewBitmapReplacement(AWidth, AHeight: integer): TBGRABitmap; override;
    procedure ChooseTool(Tool : TPaintToolType); override;
    procedure PictureSelectionChanged; override;

  end;

implementation

uses Forms, Dialogs, Controls, FileUtil,

     uradialblur, umotionblur, uemboss, utwirl,
     unewimage, uresample, upixelate,

     ugraph, ucommandline, uabout;

{ TLazPaintInstance }

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

procedure TLazPaintInstance.FormsNeeded;
begin
  if FMain <> nil then exit;

  Application.CreateForm(TFMain, FMain);
  FMain.LazPaintInstance := self;
  ToolManager.BitmapToVirtualScreen := @FMain.BitmapToVirtualScreen;
  Application.CreateForm(TFToolbox, FToolbox);
  FToolbox.LazPaintInstance := self;
  Application.CreateForm(TFLayerStack,FLayerStack);
  FLayerStack.LazPaintInstance := self;

  //needed to attach to the right instance of FMain
  FToolbox.ToolButton_ColorPicker.Action := FMain.ToolColorPicker;
  FToolbox.ToolButton_Deformation.Action := FMain.ToolDeformation;
  FToolbox.ToolButton_EditDeselect.Action := FMain.EditDeselect;
  FToolbox.ToolButton_Ellipse.Action := FMain.ToolEllipse;
  FToolbox.ToolButton_Eraser.Action := FMain.ToolEraser;
  FToolbox.ToolButton_Floodfill.Action := FMain.ToolFloodfill;
  FToolbox.ToolButton_Gradient.Action := FMain.ToolGradient;
  FToolbox.ToolButton_Hand.Action := FMain.ToolHand;
  FToolbox.ToolButton_MagicWand.Action := FMain.ToolMagicWand;
  FToolbox.ToolButton_MoveSelection.Action := FMain.ToolMoveSelection;
  FToolbox.ToolButton_Pen.Action := FMain.ToolPen;
  FToolbox.ToolButton_Polygon.Action := FMain.ToolPolygon;
  FToolbox.ToolButton_Rect.Action := FMain.ToolRect;
  FToolbox.ToolButton_RotateSelection.Action := FMain.ToolRotateSelection;
  FToolbox.ToolButton_SelectCurve.Action := FMain.ToolSelectCurve;
  FToolbox.ToolButton_SelectEllipse.Action := FMain.ToolSelectEllipse;
  FToolbox.ToolButton_SelectionPen.Action := FMain.ToolSelectionPen;
  FToolbox.ToolButton_SelectPoly.Action := FMain.ToolSelectPoly;
  FToolbox.ToolButton_SelectRect.Action := FMain.ToolSelectRect;
  FToolbox.ToolButton_Spline.Action := FMain.ToolSpline;
  FToolbox.ToolButton_Text.Action := FMain.ToolText;
  FToolbox.ToolButton_Phong.Action := FMain.ToolPhong;
  FToolbox.ToolButton_TextureMapping.Action := FMain.ToolTextureMapping;

  Application.CreateForm(TFChooseColor, FChooseColor);
  FChooseColor.LazPaintInstance := self;
  Application.CreateForm(TFCanvasSize, FCanvasSize);
  FCanvasSize.LazPaintInstance := self;
  Application.CreateForm(TFColorIntensity, FColorIntensity);
  FColorIntensity.LazPaintInstance := self;
  Application.CreateForm(TFShiftColors, FShiftColors);
  FShiftColors.LazPaintInstance := self;
  Application.CreateForm(TFColorize, FColorize);
  FColorize.LazPaintInstance := self;
  Application.CreateForm(TFCustomBlur, FCustomBlur);
  FCustomBlur.LazPaintInstance := self;
  Application.CreateForm(TFMultiImage, FMultiImage);
  FMultiImage.LazPaintInstance := self;
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
  UseConfig(TIniFile.Create(''));
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

constructor TLazPaintInstance.Create;
begin
  Init(False);
end;

function TLazPaintInstance.GetMainFormVisible: boolean;
begin
  if FMain <> nil then
    result := FMain.Visible
  else
    result := false;
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

constructor TLazPaintInstance.Create(AEmbedded: boolean);
begin
  Init(AEmbedded);
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

procedure TLazPaintInstance.AssignBitmap(bmp: TBGRABitmap);
begin
  FormsNeeded;
  FMain.SetCurrentBitmap(bmp.Duplicate as TBGRABitmap);
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

  FreeAndNil(FMultiImage);
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
  If RepaintNow then
    FMain.PaintPicture
  else
  begin
    FMain.Invalidate;
    FMain.PaintBox_Picture.Invalidate;
  end;
end;

procedure TLazPaintInstance.NotifyImageChangeCompletely(RepaintNow: boolean);
begin
  FormsNeeded;
  Image.ImageMayChangeCompletely;
  If RepaintNow then
    FMain.PaintPicture
  else
  begin
    FMain.Invalidate;
    FMain.PaintBox_Picture.Invalidate;
  end;
end;

function TLazPaintInstance.TryOpenFile(filename: string): boolean;
begin
  FormsNeeded;
  result := FMain.TryOpenFile(filename);
end;

function TLazPaintInstance.ExecuteFilter(filter: TPictureFilter;
  skipDialog: boolean): boolean;
begin
  FormsNeeded;
  Result:= FMain.ExecuteFilter(filter, skipDialog);
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
    begin
      Image.Assign(FCanvasSize.canvasSizeResult, true);
      FMain.PaintPicture;
    end;
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
    begin
      image.Assign(FCanvasSize.canvasSizeResult,true);
      FMain.PaintPicture;
    end;
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

function TLazPaintInstance.ChooseImage(images: ArrayOfBGRABitmap): TBGRABitmap;
begin
  FormsNeeded;
  Result:= FMultiImage.ShowAndChoose(images);
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
  Result:= uresample.ShowResampleDialog(self);
end;

procedure TLazPaintInstance.NotifyStackChange;
begin
  OnStackChanged(image,False);
end;

procedure TLazPaintInstance.DoHorizontalFlip(AOption: TFlipOption);
var center: TPointF;
begin
  ToolManager.ToolClose;
  if ((AOption = foAuto) and image.SelectionEmpty) or (AOption = foCurrentLayer) then
  begin
    image.ReleaseSelection;
    image.currentImageLayer.HorizontalFlip;
    Image.ImageMayChangeCompletely;
  end else
  if (AOption = foAuto) or (AOption = foSelection) then
  begin
    if not image.SelectionEmpty then
    begin
      ChooseTool(ptMoveSelection);
      center := GetSelectionCenter(image.currentSelection);
      image.currentSelection.HorizontalFlip;
      image.SelectionOffset := point(round((center.X-(image.Width/2-0.5))*2),0);
      PictureSelectionChanged;
      image.GetDrawingLayer.HorizontalFlip;
      Image.ImageMayChangeCompletely;
    end else
      exit;
  end else
  if (AOption = foWholePicture) then
  begin
    image.currentLayeredBitmap.HorizontalFlip;
    Image.ImageMayChangeCompletely;
  end;
  if FMain <> nil then FMain.PaintPicture;
end;

procedure TLazPaintInstance.DoVerticalFlip(AOption: TFlipOption);
var center: TPointF;
begin
  ToolManager.ToolClose;
  if ((AOption = foAuto) and image.SelectionEmpty) or (AOption = foCurrentLayer) then
  begin
    image.ReleaseSelection;
    image.currentImageLayer.VerticalFlip;
    Image.ImageMayChangeCompletely;
  end else
  if (AOption = foAuto) or (AOption = foSelection) then
  begin
    if not image.SelectionEmpty then
    begin
      ChooseTool(ptMoveSelection);
      center := GetSelectionCenter(image.currentSelection);
      image.currentSelection.VerticalFlip;
      image.SelectionOffset := point(0,round((center.Y-(image.Height/2-0.5))*2));
      PictureSelectionChanged;
      image.GetDrawingLayer.VerticalFlip;
      Image.ImageMayChangeCompletely;
    end else
      exit;
  end else
  if (AOption = foWholePicture) then
  begin
    image.currentLayeredBitmap.VerticalFlip;
    Image.ImageMayChangeCompletely;
  end;
  if (FMain <> nil) and FMain.Visible then FMain.PaintPicture;
end;

procedure TLazPaintInstance.DoRotateCW;
begin
  image.currentLayeredBitmap.RotateCW;
  if image.currentSelection <> nil then
    image.currentSelection := image.currentSelection.RotateCW as TBGRABitmap;
  if image.GetSelectionLayerIfExists <> nil then
    image.SetSelectionLayer(image.GetSelectionLayerIfExists.RotateCW as TBGRABitmap,True);
  image.ImageMayChangeCompletely;
  if FMain <> nil then FMain.PaintPicture;
end;

procedure TLazPaintInstance.DoRotateCCW;
begin
  image.currentLayeredBitmap.RotateCCW;
  if image.currentSelection <> nil then
    image.currentSelection := image.currentSelection.RotateCW as TBGRABitmap;
  if image.GetSelectionLayerIfExists <> nil then
    image.SetSelectionLayer(image.GetSelectionLayerIfExists.RotateCW as TBGRABitmap,True);
  image.ImageMayChangeCompletely;
  if FMain <> nil then FMain.PaintPicture;
end;

procedure TLazPaintInstance.DoSmartZoom3;
begin
  ToolManager.ToolClose;
  image.ApplySmartZoom3;
  if FMain <> nil then FMain.PaintPicture;
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

procedure TLazPaintInstance.PictureSelectionChanged;
begin
  FormsNeeded;
  image.SelectionMayChange;
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

function TLazPaintInstance.GetChooseColorTarget: TColorTarget;
begin
  Result:= FChooseColor.colorTarget;
end;

procedure TLazPaintInstance.SetChooseColorTarget(const AValue: TColorTarget);
begin
  FChooseColor.colorTarget:= AValue;
  ColorToFChooseColor;
end;

procedure TLazPaintInstance.ShowColorIntensityDlg(activeLayer: TBGRABitmap);
begin
  FormsNeeded;
  if activeLayer = nil then
  begin
    ShowMessage(rsNoActiveLayer);
    exit;
  end;
  try
    FColorIntensity.mode := ciIntensity;
    FColorIntensity.activeLayer := activeLayer;
    FColorIntensity.ShowModal;
  except
    on ex:Exception do
      ShowMessage('ShowColorIntensityDlg: '+ex.Message);
  end;
end;

procedure TLazPaintInstance.ShowColorLightnessDlg(activeLayer: TBGRABitmap);
begin
  FormsNeeded;
  if activeLayer = nil then
  begin
    ShowMessage(rsNoActiveLayer);
    exit;
  end;
  try
    FColorIntensity.mode := ciLightness;
    FColorIntensity.activeLayer := activeLayer;
    FColorIntensity.ShowModal;
  except
    on ex:Exception do
      ShowMessage('ShowColorLightnessDlg: '+ex.Message);
  end;
end;

procedure TLazPaintInstance.ShowShiftColorsDlg(activeLayer: TBGRABitmap);
begin
  FormsNeeded;
  if activeLayer = nil then
  begin
    ShowMessage(rsNoActiveLayer);
    exit;
  end;
  try
    FShiftColors.activeLayer := activeLayer;
    FShiftColors.ShowModal;
  except
    on ex:Exception do
      ShowMessage('ShowShiftColorsDlg: '+ex.Message);
  end;
end;

procedure TLazPaintInstance.ShowColorizeDlg(activeLayer: TBGRABitmap);
begin
  FormsNeeded;
  if activeLayer = nil then
  begin
    ShowMessage(rsNoActiveLayer);
    exit;
  end;
  try
    FColorize.activeLayer := activeLayer;
    FColorize.ShowModal;
  except
    on ex:Exception do
      ShowMessage('ShowColorizeDlg: '+ex.Message);
  end;
end;

function TLazPaintInstance.ShowRadialBlurDlg(layer: TBGRABitmap; out
  filteredLayer: TBGRABitmap; blurType: TRadialBlurType): boolean;
begin
  FormsNeeded;
  result := uradialblur.ShowRadialBlurDlg(self,layer,filteredLayer,blurType);
end;

function TLazPaintInstance.ShowMotionBlurDlg(layer: TBGRABitmap; out
  filteredLayer: TBGRABitmap): boolean;
begin
  FormsNeeded;
  result := umotionblur.ShowMotionBlurDlg(self,layer,filteredLayer);
end;

function TLazPaintInstance.ShowCustomBlurDlg(layer: TBGRABitmap; out
  filteredLayer: TBGRABitmap): boolean;
begin
  FormsNeeded;
  try
    result := FCustomBlur.ShowDlg(layer,filteredLayer);
  except
    filteredLayer := nil;
    result := false;
  end;
end;

function TLazPaintInstance.ShowEmbossDlg(layer: TBGRABitmap; out
  filteredLayer: TBGRABitmap): boolean;
begin
  FormsNeeded;
  result := uemboss.ShowEmbossDlg(self,layer,filteredLayer);
end;

function TLazPaintInstance.ShowPixelateDlg(layer: TBGRABitmap; out
  filteredLayer: TBGRABitmap): boolean;
begin
  FormsNeeded;
  result := upixelate.ShowPixelateDlg(self,layer,filteredLayer);
end;

function TLazPaintInstance.ShowTwirlDlg(layer: TBGRABitmap; out
  filteredLayer: TBGRABitmap): boolean;
begin
  FormsNeeded;
  result := utwirl.ShowTwirlDlg(self,layer,filteredLayer);
end;

end.

