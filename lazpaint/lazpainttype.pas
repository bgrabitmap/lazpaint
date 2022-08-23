// SPDX-License-Identifier: GPL-3.0-only
unit LazPaintType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Inifiles, BGRABitmap, BGRABitmapTypes, UConfig, UImage, UTool, Forms, BGRALayers, Graphics, Menus,
  UScripting, Dialogs, Controls
  {$IFDEF LINUX}, InterfaceBase{$ENDIF};

const
  LazPaintVersion = 7020200;

  function LazPaintVersionStr: string;

  {

  Improvements accepted:
  ----------------------
  Mac:
  - combobox dropdown rect without scrollbar
  Scripting
  Color picker
  - From final image
  - With radius
  Translation of curve modes (in dropdown)
  Lasso
  Utiliser les touches de direction
  Mettre a jour le curseur quand on change d'outil (notamment avec Espace)

  Possible improvements:
  ----------------------
  Integrate tools in window (partly done)
  Hue/color blend mode
  Acquisition (Twain)

  Format:
  - TIM image format
  - load/save image DPI
  - load/save RAW

  Filters:
  - filtre de vagues en translation
  - filtre pontillisme
  - G'MIC filters
  - surface blur
  - smart zoom using vectorization

  Tools:
  - 3D text
  - erase tool in empty selection
  - aliased selection
  - antialiased magic wand

  Known bugs:
  - Puppy linx: title bar disappears sometimes
  }

const
  OriginalDPI = 96;
  ToolWindowFixedSize = {$IFDEF LINUX}bsDialog{$ELSE}bsToolWindow{$ENDIF};
  ToolWindowSizeable = {$IFDEF LINUX}bsSizeable{$ELSE}bsSizeToolWin{$ENDIF};
  ToolWindowStyle = {$IF defined(LINUX) and defined(LCLqt5)}fsNormal{$ELSE}fsStayOnTop{$ENDIF};

  function LazPaintCurrentVersion : String;

type
  TPictureFilter = (pfNone,
                    pfBlurPrecise, pfBlurRadial, pfBlurFast, pfBlurBox, pfBlurCorona, pfBlurDisk, pfBlurMotion, pfBlurCustom,
                    pfSharpen, pfSmooth, pfMedian, pfNoise, pfPixelate, pfClearType, pfClearTypeInverse, pfFunction,
                    pfEmboss, pfPhong, pfContour, pfGrayscale, pfNegative, pfLinearNegative, pfComplementaryColor, pfNormalize,
                    pfSphere, pfTwirl, pfWaveDisplacement, pfCylinder, pfPlane,
                    pfPerlinNoise,pfCyclicPerlinNoise,pfClouds,pfCustomWater,pfWater,pfRain,pfWood,pfWoodVertical,pfPlastik,pfMetalFloor,pfCamouflage,
                    pfSnowPrint,pfStone,pfRoundStone,pfMarble);

const
  PictureFilterStr : array[TPictureFilter] of string =
                   ('None',
                    'BlurPrecise', 'BlurRadial', 'BlurFast', 'BlurBox', 'BlurCorona', 'BlurDisk', 'BlurMotion', 'BlurCustom',
                    'Sharpen', 'Smooth', 'Median', 'Noise', 'Pixelate', 'ClearType', 'ClearTypeInverse', 'Function',
                    'Emboss', 'Phong', 'Contour', 'Grayscale', 'Negative', 'LinearNegative', 'ComplementaryColor', 'Normalize',
                    'Sphere', 'Twirl', 'WaveDisplacement', 'Cylinder', 'Plane',
                    'PerlinNoise','CyclicPerlinNoise','Clouds','CustomWater','Water','Rain','Wood','WoodVertical','Plastik','MetalFloor','Camouflage',
                    'SnowPrint','Stone','RoundStone','Marble');

  IsColoredFilter: array[TPictureFilter] of boolean =
                   (false,
                    false, false, false, false, false, false, false, false,
                    false, false, false, false, false, true, true, true,
                    false, true, false, false, false, false, false, false,
                    false, false, false, false, false,
                    false,false,true,true,true,true,true,true,true,true,true,
                    true,true,true,true);

const
  MinZoomForGrid = 4;

type
  TVSCursorPosition = record
     bounds: TRect;
     c: TPointF;
     rx,ry: single;
  end;
  ArrayOfLayerId = array of integer;

const
  OnlyRenderChange : TRect = (left:-32768;top:-32768;right:0;bottom:0);

function IsOnlyRenderChange(const ARect:TRect): boolean;

type
    ArrayOfBGRABitmap = array of TBGRABitmap;
    TColorTarget = (ctForeColorSolid, ctForeColorStartGrad, ctForeColorEndGrad,
                    ctBackColorSolid, ctBackColorStartGrad, ctBackColorEndGrad,
                    ctOutlineColorSolid, ctOutlineColorStartGrad, ctOutlineColorEndGrad);
    TFlipOption = (foAuto, foWholePicture, foSelection, foCurrentLayer);

    PImageEntry = ^TImageEntry;

    { TImageEntry }

    TImageEntry = object
      bmp: TBGRABitmap;
      bpp: integer;
      frameIndex, frameCount: integer;
      isDuplicate: boolean;
      class function Empty: TImageEntry; static;
      class function NewFrameIndex: integer; static;
      procedure FreeAndNil;
      procedure Release;
    end;
    ArrayOfImageEntry = array of TImageEntry;

type
    TLatestVersionUpdateHandler = procedure(ANewVersion: string) of object;
    TLazPaintCustomOnlineUpdater = class
       OnLatestVersionUpdate: TLatestVersionUpdateHandler;
    end;

type
  TLazPaintCustomInstance = class;
  TLazPaintInstanceEvent = procedure(AInstance : TLazPaintCustomInstance) of object;
  TTopMostInfo = record
     defined: boolean;
     toolboxHidden, choosecolorHidden, layerstackHidden, imagelistHidden: NativeInt;
  end;
  TCheckFunction = function: boolean of object;

  { TLazPaintCustomInstance }

  TLazPaintCustomInstance = class(TInterfacedObject,IConfigProvider)
  private
    FBlackAndWhite: boolean;
    function GetDarkTheme: boolean;
    procedure SetDarkTheme(AValue: boolean);
  protected
    FRestartQuery: boolean;
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
    function _AddRef: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
    function _Release: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};

    function GetIcons(ASize: integer): TImageList; virtual; abstract;
    function GetToolBoxWindowPopup: TPopupMenu; virtual; abstract;
    procedure SetToolBoxWindowPopup(AValue: TPopupMenu); virtual; abstract;
    function GetFullscreen: boolean; virtual; abstract;
    procedure SetFullscreen(AValue: boolean); virtual; abstract;
    function GetDockLayersAndColors: boolean; virtual; abstract;
    procedure SetDockLayersAndColors(AValue: boolean); virtual; abstract;
    function GetScriptContext: TScriptContext; virtual; abstract;
    function GetShowSelectionNormal: boolean; virtual; abstract;
    procedure SetShowSelectionNormal(AValue: boolean); virtual; abstract;
    function GetGridVisible: boolean; virtual; abstract;
    procedure SetGridVisible(const AValue: boolean); virtual; abstract;
    function GetEmbedded: boolean; virtual; abstract;
    function GetTopMostHasFocus: boolean; virtual; abstract;
    function GetTopMostVisible: boolean; virtual; abstract;
    function GetTopMostOkToUnfocus: boolean; virtual; abstract;

    function GetConfig: TLazPaintConfig; virtual; abstract;
    function GetImage: TLazPaintImage; virtual; abstract;
    function GetImageAction: TObject; virtual; abstract;
    function GetToolManager: TToolManager; virtual; abstract;
    procedure SetBlackAndWhite(AValue: boolean); virtual;
    function GetZoomFactor: single; virtual;

    function GetUpdateStackOnTimer: boolean; virtual; abstract;
    procedure SetUpdateStackOnTimer(AValue: boolean); virtual; abstract;

    function GetChooseColorHeight: integer; virtual; abstract;
    function GetChooseColorWidth: integer; virtual; abstract;
    procedure SetChooseColorHeight(AValue: integer); virtual; abstract;
    procedure SetChooseColorWidth(AValue: integer); virtual; abstract;
    function GetChooseColorVisible: boolean; virtual; abstract;
    procedure SetChooseColorVisible(const AValue: boolean); virtual; abstract;
    function GetChooseColorTarget: TColorTarget; virtual; abstract;
    procedure SetChooseColorTarget(const AValue: TColorTarget); virtual; abstract;

    function GetToolboxVisible: boolean; virtual; abstract;
    procedure SetToolboxVisible(const AValue: boolean); virtual; abstract;
    function GetToolboxHeight: integer; virtual; abstract;
    function GetToolboxWidth: integer; virtual; abstract;

    function GetImageListWindowVisible: boolean; virtual; abstract;
    procedure SetImageListWindowVisible(const AValue: boolean); virtual; abstract;
    function GetImageListWindowHeight: integer; virtual; abstract;
    function GetImageListWindowWidth: integer; virtual; abstract;
    procedure SetImageListWindowHeight(AValue: integer); virtual; abstract;
    procedure SetImageListWindowWidth(AValue: integer); virtual; abstract;

    function GetLayerWindowVisible: boolean; virtual; abstract;
    procedure SetLayerWindowVisible(AValue: boolean); virtual; abstract;
    function GetLayerWindowHeight: integer; virtual; abstract;
    function GetLayerWindowWidth: integer; virtual; abstract;
    procedure SetLayerWindowHeight(AValue: integer); virtual; abstract;
    procedure SetLayerWindowWidth(AValue: integer); virtual; abstract;

    function GetMainFormBounds: TRect; virtual; abstract;
  public
    Title: string;
    EmbeddedResult: TModalResult;
    EmbeddedImageBackup: TBGRABitmap;

    constructor Create; virtual; abstract;
    constructor Create(AEmbedded: boolean); virtual; abstract;
    procedure RegisterThemeListener(AHandler: TNotifyEvent; ARegister: boolean); virtual; abstract;
    procedure NotifyThemeChanged; virtual; abstract;
    procedure StartLoadingImage(AFilename: string); virtual; abstract;
    procedure EndLoadingImage; virtual; abstract;
    procedure StartSavingImage(AFilename: string); virtual; abstract;
    procedure EndSavingImage; virtual; abstract;
    procedure ReportActionProgress(AProgressPercent: integer); virtual; abstract;
    procedure SaveMainWindowPosition; virtual; abstract;
    procedure RestoreMainWindowPosition; virtual; abstract;
    procedure Donate; virtual; abstract;
    procedure UseConfig(ini: TInifile); virtual; abstract;
    procedure AssignBitmap(bmp: TBGRABitmap); virtual; abstract; overload;
    procedure AssignBitmap(bmp: TBGRALayeredBitmap); virtual; abstract; overload;
    function EditBitmap(var bmp: TBGRABitmap; ConfigStream: TStream = nil;
      ATitle: String = ''; AOnRun: TLazPaintInstanceEvent = nil;
      AOnExit: TLazPaintInstanceEvent = nil;
      ABlackAndWhite : boolean = false): boolean; virtual; abstract;
    function EditBitmap(var bmp: TBGRALayeredBitmap;
      ConfigStream: TStream = nil; ATitle: String = '';
      AOnRun: TLazPaintInstanceEvent = nil;
      AOnExit: TLazPaintInstanceEvent = nil;
      ABlackAndWhite : boolean = false): boolean; virtual; abstract;
    function EditTexture(ASource: TBGRABitmap): TBGRABitmap; virtual; abstract;
    procedure EditSelection; virtual; abstract;
    function ProcessCommandLine: boolean; virtual; abstract;
    function ProcessCommands(commands: TStringList): boolean; virtual; abstract;
    procedure ChangeIconSize(size: integer); virtual; abstract;
    procedure Show; virtual; abstract;
    function Hide: boolean; virtual; abstract;
    procedure Run; virtual; abstract;
    function Restart: boolean; virtual; abstract;
    procedure CancelRestart; virtual; abstract;
    procedure NotifyImageChange(RepaintNow: boolean; ARect: TRect); virtual; abstract;
    procedure NotifyImageChangeCompletely(RepaintNow: boolean); virtual; abstract;
    procedure NotifyImagePaint; virtual; abstract;
    procedure NotifyStackChange; virtual; abstract;
    function TryOpenFileUTF8(filename: string; skipDialogIfSingleImage: boolean = false): boolean; virtual; abstract;
    function ExecuteFilter(filter: TPictureFilter; skipDialog: boolean = false): TScriptResult; virtual; abstract;
    function RunScript(AFilename: string; ACaption: string = ''): boolean; virtual; abstract;
    procedure AdjustChooseColorHeight; virtual; abstract;
    procedure ColorFromFChooseColor; virtual; abstract;
    procedure ColorToFChooseColor; virtual; abstract;
    procedure ExitColorEditor; virtual; abstract;
    function ColorEditorActive: boolean; virtual; abstract;
    function GetColor(ATarget: TColorTarget): TBGRAPixel;
    procedure SetColor(ATarget: TColorTarget; AColor: TBGRAPixel);
    function ShowSaveOptionDlg(AParameters: TVariableSet; AOutputFilenameUTF8: string;
                               ASkipOptions: boolean; AExport: boolean): boolean; virtual; abstract;
    function ShowColorIntensityDlg(AParameters: TVariableSet): TScriptResult; virtual; abstract;
    function ShowColorLightnessDlg(AParameters: TVariableSet): TScriptResult; virtual; abstract;
    function ShowShiftColorsDlg(AParameters: TVariableSet): TScriptResult; virtual; abstract;
    function ShowColorizeDlg(AParameters: TVariableSet): TScriptResult; virtual; abstract;
    function ShowColorCurvesDlg(AParameters: TVariableSet): TScriptResult; virtual; abstract;
    function ShowRadialBlurDlg(AFilterConnector: TObject; blurType:TRadialBlurType; ACaption: string = ''): TScriptResult; virtual; abstract;
    function ShowMotionBlurDlg(AFilterConnector: TObject): TScriptResult; virtual; abstract;
    function ShowCustomBlurDlg(AFilterConnector: TObject): TScriptResult; virtual; abstract;
    function ShowEmbossDlg(AFilterConnector: TObject): TScriptResult; virtual; abstract;
    function ShowRainDlg(AFilterConnector: TObject): TScriptResult; virtual; abstract;
    function ShowPixelateDlg(AFilterConnector: TObject): TScriptResult; virtual; abstract;
    function ShowNoiseFilterDlg(AFilterConnector: TObject): TScriptResult; virtual; abstract;
    function ShowTwirlDlg(AFilterConnector: TObject): TScriptResult; virtual; abstract;
    function ShowWaveDisplacementDlg(AFilterConnector: TObject): TScriptResult; virtual; abstract;
    function ShowPhongFilterDlg(AFilterConnector: TObject): TScriptResult; virtual; abstract;
    function ShowFunctionFilterDlg(AFilterConnector: TObject): TScriptResult; virtual; abstract;
    function ShowSharpenDlg(AFilterConnector: TObject): TScriptResult; virtual; abstract;
    function ShowPosterizeDlg(AParameters: TVariableSet): TScriptResult; virtual; abstract;
    procedure ShowPrintDlg; virtual; abstract;
    function OpenImage (FileName: string; AddToRecent: Boolean= True): boolean; virtual; abstract;
    procedure AddToImageList(const FileNames: array of String); virtual; abstract;
    procedure UpdateToolbar; virtual; abstract;
    procedure UpdateEditPicture(ADelayed: boolean); virtual; abstract;
    function HideTopmost: TTopMostInfo; virtual; abstract;
    procedure ShowTopmost(AInfo: TTopMostInfo); virtual; abstract;
    procedure ShowCanvasSizeDlg; virtual; abstract;
    procedure ShowRepeatImageDlg; virtual; abstract;
    procedure ShowAboutDlg; virtual; abstract;
    procedure ShowMessage(ACaption: string; AMessage: string; ADlgType: TMsgDlgType = mtInformation);
    procedure ShowError(ACaption: string; AMessage: string);
    function SaveQuestion(ATitle: string): integer;
    function ShowNewImageDlg(out bitmap: TBGRABitmap):boolean; virtual; abstract;
    function ShowResampleDialog(AParameters: TVariableSet):boolean; virtual; abstract;
    procedure UpdateWindows; virtual; abstract;
    procedure Wait(ACheckActive: TCheckFunction; ADelayMs: integer); virtual; abstract;
    procedure AddColorToPalette(AColor: TBGRAPixel); virtual; abstract;
    procedure RemoveColorFromPalette(AColor: TBGRAPixel); virtual; abstract;

    property BlackAndWhite: boolean read FBlackAndWhite write SetBlackAndWhite;

    procedure ScrollLayerStackOnItem(AIndex: integer; ADelayedUpdate: boolean = true); virtual; abstract;
    procedure InvalidateLayerStack; virtual; abstract;
    procedure UpdateLayerStackOnTimer; virtual; abstract;
    function MakeNewBitmapReplacement(AWidth, AHeight: integer; AColor: TBGRAPixel): TBGRABitmap; virtual; abstract;
    procedure ChooseTool(Tool : TPaintToolType; AAsFromGui: boolean); virtual; abstract;
    function GetOnlineUpdater: TLazPaintCustomOnlineUpdater; virtual;

    property GridVisible: boolean read GetGridVisible write SetGridVisible;

    procedure MoveToolboxTo(X,Y: integer); virtual; abstract;
    property ToolboxVisible: boolean read GetToolboxVisible write SetToolboxVisible;
    property ToolboxWidth: integer read GetToolboxWidth;
    property ToolboxHeight: integer read GetToolboxHeight;
    property ToolboxWindowPopup: TPopupMenu read GetToolBoxWindowPopup write SetToolBoxWindowPopup;

    procedure MoveChooseColorTo(X,Y: integer); virtual; abstract;
    property ChooseColorVisible: boolean read GetChooseColorVisible write SetChooseColorVisible;
    property ChooseColorWidth: integer read GetChooseColorWidth write SetChooseColorWidth;
    property ChooseColorHeight: integer read GetChooseColorHeight write SetChooseColorHeight;

    procedure MoveLayerWindowTo(X,Y: integer); virtual; abstract;
    property LayerWindowWidth: integer read GetLayerWindowWidth write SetLayerWindowWidth;
    property LayerWindowHeight: integer read GetLayerWindowHeight write SetLayerWindowHeight;
    property LayerWindowVisible: boolean read GetLayerWindowVisible write SetLayerWindowVisible;

    procedure ImageListWindowVisibleKeyDown(var Key: Word; Shift: TShiftState); virtual; abstract;
    procedure MoveImageListWindowTo(X,Y: integer); virtual; abstract;
    property ImageListWindowWidth: integer read GetImageListWindowWidth write SetImageListWindowWidth;
    property ImageListWindowHeight: integer read GetImageListWindowHeight write SetImageListWindowHeight;
    property ImageListWindowVisible: boolean read GetImageListWindowVisible write SetImageListWindowVisible;

    property ChooseColorTarget: TColorTarget read GetChooseColorTarget write setChooseColorTarget;
    property Config: TLazPaintConfig read GetConfig;
    property Image: TLazPaintImage read GetImage;
    property ImageAction: TObject read GetImageAction;
    property ZoomFactor: single read GetZoomFactor;
    property ToolManager: TToolManager read GetToolManager;
    property Embedded: boolean read GetEmbedded;
    property TopMostHasFocus: boolean read GetTopMostHasFocus;
    property TopMostOkToUnfocus: boolean read GetTopMostOkToUnfocus;
    property TopMostVisible: boolean read GetTopMostVisible;

    property ShowSelectionNormal: boolean read GetShowSelectionNormal write SetShowSelectionNormal;
    property ScriptContext: TScriptContext read GetScriptContext;
    property MainFormBounds: TRect read GetMainFormBounds;
    property DockLayersAndColors: boolean read GetDockLayersAndColors write SetDockLayersAndColors;
    property Fullscreen: boolean read GetFullscreen write SetFullscreen;
    property RestartQuery: boolean read FRestartQuery;
    property DarkTheme: boolean read GetDarkTheme write SetDarkTheme;
    property UpdateStackOnTimer: boolean read GetUpdateStackOnTimer write SetUpdateStackOnTimer;

    property Icons[ASize: integer]: TImageList read GetIcons;
  end;

function StrToPictureFilter(const s: ansistring): TPictureFilter;
procedure SafeSetFocus(AControl: TWinControl);
function WindowBorderWidth(AForm: TForm): integer;
function WindowBorderTopHeight(AForm: TForm; {%H-}AIncludeTitle: boolean): integer;
function WindowBorderBottomHeight(AForm: TForm): integer;
function WindowOutermostBorderWidth: integer;
function WindowOutermostBorderHeight: integer;
function GetWindowFullWidth(AForm: TForm): integer;
procedure SetWindowFullWidth(AForm: TForm; AWidth: integer);
function GetWindowFullHeight(AForm: TForm): integer;
procedure SetWindowFullHeight(AForm: TForm; AHeight: integer);
procedure SetWindowFullSize(AForm: TForm; AWidth,AHeight: integer);
procedure SetWindowTopLeftCorner(AForm: TForm; X,Y: integer);
function GetWindowTopLeftCorner(AForm: TForm): TPoint;
function PascalToCSSCase(AIdentifier: string): string;
function CSSToPascalCase(AIdentifier: string): string;

implementation

uses LCLType, BGRAUTF8, LCLIntf, FileUtil, UResourceStrings, LCVectorialFill;

function LazPaintVersionStr: string;
var numbers: TStringList;
  i,remaining: cardinal;
begin
  numbers := TStringList.Create;
  remaining := LazPaintVersion;
  for i := 1 to 4 do
  begin
    numbers.Insert(0, IntToStr(remaining mod 100));
    remaining := remaining div 100;
  end;
  while (numbers.Count > 1) and (numbers[numbers.Count-1]='0') do
    numbers.Delete(numbers.Count-1);
  numbers.Delimiter:= '.';
  result := numbers.DelimitedText;
  numbers.Free;
end;

function LazPaintCurrentVersion: String;
const
{$IFDEF CPU64}
  LazPaintProcessorInfo = ' (64-bit)';
{$ELSE}
  LazPaintProcessorInfo = ' (32-bit)';
{$ENDIF}
begin
  result := LazPaintVersionStr {$IFDEF DEBUG} + ' Beta'{$ENDIF} + LazPaintProcessorInfo;
end;

function IsOnlyRenderChange(const ARect: TRect): boolean;
begin
  result := (ARect.left = OnlyRenderChange.left) and
    (ARect.top  = OnlyRenderChange.top) and
    (ARect.right = OnlyRenderChange.right) and
    (ARect.bottom = OnlyRenderChange.bottom);
end;

function StrToPictureFilter(const s: ansistring): TPictureFilter;
var pf: TPictureFilter;
    ls: ansistring;
begin
  result := pfNone;
  ls:= UTF8LowerCase(s);
  for pf := low(TPictureFilter) to high(TPictureFilter) do
    if ls = UTF8LowerCase(PictureFilterStr[pf]) then
    begin
      result := pf;
      break;
    end;
end;

procedure SafeSetFocus(AControl: TWinControl);
begin
  try
    AControl.SetFocus;
  except
  end;
end;

function WindowBorderWidth(AForm: TForm): integer;
begin
  If AForm.BorderStyle = bsNone then
  begin
    result := 0;
    exit;
  end;
  {$IFDEF LINUX}
  result := (GetWindowFullWidth(AForm)-AForm.ClientWidth) div 2;
  {$ELSE}
  if AForm.BorderStyle in[bsSizeable,bsSizeToolWin] then
    result := GetSystemMetrics(SM_CXSIZEFRAME)
  else if AForm.BorderStyle in[bsSingle,bsDialog,bsToolWindow] then
    result := GetSystemMetrics(SM_CXFIXEDFRAME)
  else
    result := 0;
  {$ENDIF}
end;

function WindowBorderTopHeight(AForm: TForm; AIncludeTitle: boolean): integer;
begin
  if AForm.BorderStyle = bsNone then
  begin
    result := 0;
    exit;
  end;
  {$IFDEF LINUX}
  result := GetWindowFullHeight(AForm) - AForm.ClientHeight - WindowBorderBottomHeight(AForm);
  {$ELSE}
  result := WindowBorderBottomHeight(AForm);
  if AIncludeTitle and (AForm.BorderStyle <> bsNone) then
  begin
    if AForm.BorderStyle in[bsToolWindow,bsSizeToolWin] then
    begin
      result += GetSystemMetrics(SM_CYSMCAPTION)
    end
    else result += GetSystemMetrics(SM_CYCAPTION);
  end;
  {$ENDIF}
end;

function WindowBorderBottomHeight(AForm: TForm): integer;
begin
  {$IFDEF LINUX}
  result := WindowBorderWidth(AForm);
  {$ELSE}
  if AForm.BorderStyle in[bsSizeable,bsSizeToolWin] then
    result := GetSystemMetrics(SM_CYSIZEFRAME)
  else if AForm.BorderStyle in[bsSingle,bsDialog,bsToolWindow] then
    result := GetSystemMetrics(SM_CYFIXEDFRAME)
  else
    result := 0;
  {$ENDIF}
end;

function WindowOutermostBorderWidth: integer;
begin
  result := {$IFDEF LINUX}1{$ELSE}GetSystemMetrics(SM_CXBORDER){$ENDIF};
end;

function WindowOutermostBorderHeight: integer;
begin
  result := {$IFDEF LINUX}1{$ELSE}GetSystemMetrics(SM_CYBORDER){$ENDIF};
end;

function GetWindowFullWidth(AForm: TForm): integer;
{$IFDEF LINUX}var r: TRect;{$ENDIF}
begin
  {$IFDEF LINUX}
  if AForm.BorderStyle <> bsNone then
  begin
    r := rect(0,0,AForm.Width,AForm.Height);
    WidgetSet.GetWindowRect(AForm.Handle, r);
    result := r.right-r.left;
  end else
  {$ENDIF}
  result := AForm.Width + WindowBorderWidth(AForm)*2;
end;

procedure SetWindowFullWidth(AForm: TForm; AWidth: integer);
begin
  AForm.Width := AWidth - WindowBorderWidth(AForm)*2;
end;

function GetWindowFullHeight(AForm: TForm): integer;
{$IFDEF LINUX}var r: TRect;{$ENDIF}
begin
  {$IFDEF LINUX}
  if AForm.BorderStyle <> bsNone then
  begin
    r := rect(0,0,AForm.Width,AForm.Height);
    WidgetSet.GetWindowRect(AForm.Handle, r);
    result := r.bottom-r.top;
  end else
  {$ENDIF}
  result := AForm.Height + WindowBorderTopHeight(AForm,True) + WindowBorderBottomHeight(AForm);
end;

procedure SetWindowFullHeight(AForm: TForm; AHeight: integer);
begin
  AForm.Height := AHeight - WindowBorderTopHeight(AForm,True) - WindowBorderBottomHeight(AForm);
end;

procedure SetWindowFullSize(AForm: TForm; AWidth, AHeight: integer);
begin
  AForm.SetBounds(AForm.Left,AForm.Top, AWidth - WindowBorderWidth(AForm)*2,
      AHeight - WindowBorderTopHeight(AForm,True) - WindowBorderBottomHeight(AForm));
end;

procedure SetWindowTopLeftCorner(AForm: TForm; X, Y: integer);
begin
  AForm.SetBounds(X,Y,AForm.Width,AForm.Height);
end;

function GetWindowTopLeftCorner(AForm: TForm): TPoint;
begin
  result := Point(AForm.Left,AForm.Top);
end;

function PascalToCSSCase(AIdentifier: string): string;
var
  i: Integer;
begin
  result := AIdentifier;
  for i := length(result) downto 1 do
    if result[i] <> lowercase(result[i]) then
    begin
      result[i] := lowercase(result[i]);
      if i > 1 then Insert('-', result, i);
    end;
end;

function CSSToPascalCase(AIdentifier: string): string;
var
  i: Integer;
begin
  result := AIdentifier;
  for i := length(result) downto 1 do
  begin
    if (i = 1) or (result[i-1] = '-') then
      result[i] := upcase(result[i]) else
    if result[i] = '-' then delete(result, i, 1);
  end;
end;

{ TImageEntry }

class function TImageEntry.Empty: TImageEntry;
begin
  result.bmp := nil;
  result.bpp := 0;
  result.frameIndex := -1;
  result.frameCount := 0;
  result.isDuplicate:= false;
end;

class function TImageEntry.NewFrameIndex: integer;
begin
  result := -1;
end;

procedure TImageEntry.FreeAndNil;
begin
  SysUtils.FreeAndNil(bmp);
  bpp := 0;
end;

procedure TImageEntry.Release;
begin
  bmp := nil;
  bpp := 0;
end;

function TLazPaintCustomInstance.GetDarkTheme: boolean;
begin
  if Assigned(Config) then
    result := Config.GetDarkTheme
  else
    result := false;
end;

procedure TLazPaintCustomInstance.SetDarkTheme(AValue: boolean);
begin
  if Assigned(Config) then
  begin
    if AValue <> Config.GetDarkTheme then
    begin
      Config.SetDarkTheme(AValue);
      NotifyThemeChanged;
    end;
  end;
end;

{ Interface gateway }
function TLazPaintCustomInstance.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
begin
  if GetInterface(iid, obj) then
    Result := S_OK
  else
    Result := longint(E_NOINTERFACE);
end;

{ There is no automatic reference counting, but it is compulsory to define these functions }
function TLazPaintCustomInstance._AddRef: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
begin
  result := 0;
end;

function TLazPaintCustomInstance._Release: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
begin
  result := 0;
end;

function TLazPaintCustomInstance.SaveQuestion(ATitle: string): integer;
var top: TTopMostInfo;
begin
  top := HideTopmost;
  result := QuestionDlg (ATitle,rsSaveChanges,mtWarning,[mrYes,rsYes,mrNo,rsNo,mrCancel,rsCancel],'');
  ShowTopmost(top);
end;

function TLazPaintCustomInstance.GetOnlineUpdater: TLazPaintCustomOnlineUpdater;
begin
  result := nil;
end;

function TLazPaintCustomInstance.GetZoomFactor: single;
begin
  result := 1;
end;

function TLazPaintCustomInstance.GetColor(ATarget: TColorTarget): TBGRAPixel;
  function GetStartColor(AFill: TVectorialFill): TBGRAPixel;
  begin
    if AFill.FillType = vftGradient then
      result := AFill.Gradient.StartColor
      else result := AFill.AverageColor;
  end;
  function GetEndColor(AFill: TVectorialFill): TBGRAPixel;
  begin
    if AFill.FillType = vftGradient then
      result := AFill.Gradient.EndColor
      else result := AFill.AverageColor;
  end;

begin
  case ATarget of
    ctForeColorSolid: result := ToolManager.ForeFill.AverageColor;
    ctForeColorStartGrad: result := GetStartColor(ToolManager.ForeFill);
    ctForeColorEndGrad: result := GetEndColor(ToolManager.ForeFill);
    ctBackColorSolid: result := ToolManager.BackFill.AverageColor;
    ctBackColorStartGrad: result := GetStartColor(ToolManager.BackFill);
    ctBackColorEndGrad: result := GetEndColor(ToolManager.BackFill);
    ctOutlineColorSolid: result := ToolManager.OutlineFill.AverageColor;
    ctOutlineColorStartGrad: result := GetStartColor(ToolManager.OutlineFill);
    ctOutlineColorEndGrad: result := GetEndColor(ToolManager.OutlineFill);
  else
    result := BGRAPixelTransparent;
  end;
  if BlackAndWhite then result := BGRAToGrayscale(result);
end;

procedure TLazPaintCustomInstance.SetColor(ATarget: TColorTarget;
  AColor: TBGRAPixel);
begin
  if BlackAndWhite then AColor := BGRAToGrayscale(AColor);
  case ATarget of
    ctForeColorSolid: if ToolManager.ForeFill.FillType = vftSolid then
                        ToolManager.ForeColor := AColor;
    ctForeColorStartGrad: if ToolManager.ForeFill.FillType = vftGradient then
                            ToolManager.ForeFill.Gradient.StartColor := AColor;
    ctForeColorEndGrad: if ToolManager.ForeFill.FillType = vftGradient then
                          ToolManager.ForeFill.Gradient.EndColor := AColor;
    ctBackColorSolid: if ToolManager.BackFill.FillType = vftSolid then
                        ToolManager.BackColor := AColor;
    ctBackColorStartGrad: if ToolManager.BackFill.FillType = vftGradient then
                            ToolManager.BackFill.Gradient.StartColor := AColor;
    ctBackColorEndGrad: if ToolManager.BackFill.FillType = vftGradient then
                          ToolManager.BackFill.Gradient.EndColor := AColor;
    ctOutlineColorSolid: if ToolManager.OutlineFill.FillType = vftSolid then
                        ToolManager.OutlineColor := AColor;
    ctOutlineColorStartGrad: if ToolManager.OutlineFill.FillType = vftGradient then
                            ToolManager.OutlineFill.Gradient.StartColor := AColor;
    ctOutlineColorEndGrad: if ToolManager.OutlineFill.FillType = vftGradient then
                          ToolManager.OutlineFill.Gradient.EndColor := AColor;
  end;
end;

procedure TLazPaintCustomInstance.SetBlackAndWhite(AValue: boolean);
begin
  if FBlackAndWhite=AValue then Exit;
  FBlackAndWhite:=AValue;
end;

procedure TLazPaintCustomInstance.ShowMessage(ACaption: string; AMessage: string; ADlgType: TMsgDlgType = mtInformation);
var top: TTopMostInfo;
  elems: TStringList;
  res: TModalResult;
begin
  top := HideTopmost;
  elems := TStringList.Create;
  elems.Delimiter:= #9;
  elems.StrictDelimiter:= true;
  elems.DelimitedText:= AMessage;
  if (elems.Count = 3) and (elems[1] = rsDownload) then
  begin
    res := QuestionDlg(ACaption,elems[0],ADlgType,[mrOk,rsDownload,mrCancel,rsCancel],'');
    if res = mrOk then OpenURL(elems[2]);
  end else
    QuestionDlg(ACaption,AMessage,ADlgType,[mrOk,rsOkay],'');
  elems.Free;
  ShowTopmost(top);
end;

procedure TLazPaintCustomInstance.ShowError(ACaption: string; AMessage: string);
begin
  ShowMessage(ACaption,AMessage,mtError);
end;

end.

