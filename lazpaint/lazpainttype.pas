unit LazPaintType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Inifiles, BGRABitmap, BGRABitmapTypes, uconfig, uimage, utool, Forms, BGRALayers, Graphics, uscripting, Dialogs, Controls;

const
  //Version Number (to increment at each release)
  LazPaintCurrentVersionOnly = '6.2';

  { to do :

  filtre de vagues concentriques
  filtre de vagues en translation
  filtre pontillisme
  G'MIC filters

  scripting

  integrate tools in window
  selection redraw optimization

  3D text

  linear gradient  
  hue/color blend mode

  keyboard zoom with mousepos
  
  box blur
  surface blur
  smart zoom using vectorization
  facelets
  
  erase tool in empty selection
  aliased selection
  antialiased magic wand

  }

const
  {$IFDEF CPU64}
    LazPaintProcessorInfo = ' (64-bit)';
  {$ELSE}
    LazPaintProcessorInfo = ' (32-bit)';
  {$ENDIF}
  LazPaintCurrentVersion : String=LazPaintCurrentVersionOnly + LazPaintProcessorInfo;
  OriginalDPI = 96;

type
  TPictureFilter = (pfNone,
                    pfBlurPrecise, pfBlurRadial, pfBlurFast, pfBlurCorona, pfBlurDisk, pfBlurMotion, pfBlurCustom,
                    pfSharpen, pfSmooth, pfMedian, pfNoise, pfPixelate, pfClearType, pfClearTypeInverse, pfFunction,
                    pfEmboss, pfPhong, pfContour, pfGrayscale, pfNegative, pfLinearNegative, pfComplementaryColor, pfNormalize,
                    pfSphere, pfTwirl, pfCylinder, pfPlane,
                    pfPerlinNoise,pfCyclicPerlinNoise,pfClouds,pfCustomWater,pfWater,pfWood,pfWoodVertical,pfPlastik,pfMetalFloor,pfCamouflage,
                    pfSnowPrint,pfStone,pfRoundStone,pfMarble);

const
  PictureFilterStr : array[TPictureFilter] of string =
                   ('None',
                    'BlurPrecise', 'BlurRadial', 'BlurFast', 'BlurCorona', 'BlurDisk', 'BlurMotion', 'BlurCustom',
                    'Sharpen', 'Smooth', 'Median', 'Noise', 'Pixelate', 'ClearType', 'ClearTypeInverse', 'Function',
                    'Emboss', 'Phong', 'Contour', 'Grayscale', 'Negative', 'LinearNegative', 'ComplementaryColor', 'Normalize',
                    'Sphere', 'Twirl', 'Cylinder', 'Plane',
                    'PerlinNoise','CyclicPerlinNoise','Clouds','CustomWater','Water','Wood','WoodVertical','Plastik','MetalFloor','Camouflage',
                    'SnowPrint','Stone','RoundStone','Marble');

  IsColoredFilter: array[TPictureFilter] of boolean =
                   (false,
                    false, false, false, false, false, false, false,
                    false, false, false, false, false, true, true, true,
                    false, true, false, false, false, false, false, false,
                    false, false, false, false,
                    false,false,true,true,true,true,true,true,true,true,
                    true,true,true,true);

const
  OutsideColor = TColor($00E8D1BB);
  MinZoomForGrid = 4;

type
  TVSCursorPosition = record
     bounds: TRect;
     c: TPointF;
     rx,ry: single;
  end;

const
  OnlyRenderChange : TRect = (left:-32768;top:-32768;right:0;bottom:0);

function IsOnlyRenderChange(const ARect:TRect): boolean;

type
    ArrayOfBGRABitmap = array of TBGRABitmap;
    TColorTarget = (ctForeColor, ctBackColor);
    TFlipOption = (foAuto, foWholePicture, foSelection, foCurrentLayer);


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

  { TLazPaintCustomInstance }

  TLazPaintCustomInstance = class
  private
    FBlackAndWhite: boolean;
  protected
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
    function GetToolManager: TToolManager; virtual; abstract;
    procedure SetBlackAndWhite(AValue: boolean); virtual;
    function GetZoomFactor: single; virtual;

    function GetChooseColorHeight: integer; virtual; abstract;
    function GetChooseColorWidth: integer; virtual; abstract;
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
    Title,AboutText: string;
    EmbeddedResult: TModalResult;
    EmbeddedImageBackup: TBGRABitmap;
    RestartQuery: boolean;

    constructor Create; virtual; abstract;
    constructor Create(AEmbedded: boolean); virtual; abstract;
    procedure Donate; virtual; abstract;
    procedure UseConfig(ini: TInifile); virtual; abstract;
    procedure AssignBitmap(bmp: TBGRABitmap); virtual; abstract;
    procedure EditBitmap(var bmp: TBGRABitmap; ConfigStream: TStream = nil; ATitle: String = ''; AOnRun: TLazPaintInstanceEvent = nil; AOnExit: TLazPaintInstanceEvent = nil; ABlackAndWhite : boolean = false); virtual; abstract;
    procedure EditTexture; virtual; abstract;
    procedure EditSelection; virtual; abstract;
    function ProcessCommandLine: boolean; virtual; abstract;
    function ProcessCommands(commands: TStringList): boolean; virtual; abstract;
    procedure Show; virtual; abstract;
    procedure Hide; virtual; abstract;
    procedure Run; virtual; abstract;
    procedure NotifyImageChange(RepaintNow: boolean; ARect: TRect); virtual; abstract;
    procedure NotifyImageChangeCompletely(RepaintNow: boolean); virtual; abstract;
    procedure NotifyStackChange; virtual; abstract;
    function TryOpenFileUTF8(filename: string): boolean; virtual; abstract;
    function ExecuteFilter(filter: TPictureFilter; skipDialog: boolean = false): boolean; virtual; abstract;
    procedure ColorFromFChooseColor; virtual; abstract;
    procedure ColorToFChooseColor; virtual; abstract;
    function ShowColorIntensityDlg(AParameters: TVariableSet): boolean; virtual; abstract;
    function ShowColorLightnessDlg(AParameters: TVariableSet): boolean; virtual; abstract;
    function ShowShiftColorsDlg(AParameters: TVariableSet): boolean; virtual; abstract;
    function ShowColorizeDlg(AParameters: TVariableSet): boolean; virtual; abstract;
    function ShowColorCurvesDlg(AParameters: TVariableSet): boolean; virtual; abstract;
    function ShowRadialBlurDlg(AFilterConnector: TObject;blurType:TRadialBlurType):boolean; virtual; abstract;
    function ShowMotionBlurDlg(AFilterConnector: TObject):boolean; virtual; abstract;
    function ShowCustomBlurDlg(AFilterConnector: TObject):boolean; virtual; abstract;
    function ShowEmbossDlg(AFilterConnector: TObject):boolean; virtual; abstract;
    function ShowPixelateDlg(AFilterConnector: TObject):boolean; virtual; abstract;
    function ShowNoiseFilterDlg(AFilterConnector: TObject):boolean; virtual; abstract;
    function ShowTwirlDlg(AFilterConnector: TObject):boolean; virtual; abstract;
    function ShowPhongFilterDlg(AFilterConnector: TObject): boolean; virtual; abstract;
    function ShowFunctionFilterDlg(AFilterConnector: TObject): boolean; virtual; abstract;
    function ShowSharpenDlg(AFilterConnector: TObject):boolean; virtual; abstract;
    function ShowPosterizeDlg(AParameters: TVariableSet):boolean; virtual; abstract;
    procedure ShowPrintDlg; virtual; abstract;
    function OpenImage (FileName: string; AddToRecent: Boolean= True): boolean; virtual; abstract;
    procedure ZoomFit; virtual; abstract;
    procedure AddToImageList(const FileNames: array of String); virtual; abstract;
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

    property BlackAndWhite: boolean read FBlackAndWhite write SetBlackAndWhite;

    procedure ScrollLayerStackOnItem(AIndex: integer); virtual; abstract;
    function MakeNewBitmapReplacement(AWidth, AHeight: integer): TBGRABitmap; virtual; abstract;
    procedure ChooseTool(Tool : TPaintToolType); virtual; abstract;
    function GetOnlineUpdater: TLazPaintCustomOnlineUpdater; virtual;

    property GridVisible: boolean read GetGridVisible write SetGridVisible;

    procedure MoveToolboxTo(X,Y: integer); virtual; abstract;
    property ToolboxVisible: boolean read GetToolboxVisible write SetToolboxVisible;
    property ToolboxWidth: integer read GetToolboxWidth;
    property ToolboxHeight: integer read GetToolboxHeight;

    procedure MoveChooseColorTo(X,Y: integer); virtual; abstract;
    property ChooseColorVisible: boolean read GetChooseColorVisible write SetChooseColorVisible;
    property ChooseColorWidth: integer read GetChooseColorWidth;
    property ChooseColorHeight: integer read GetChooseColorHeight;

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
    property ZoomFactor: single read GetZoomFactor;
    property ToolManager: TToolManager read GetToolManager;
    property Embedded: boolean read GetEmbedded;
    property TopMostHasFocus: boolean read GetTopMostHasFocus;
    property TopMostOkToUnfocus: boolean read GetTopMostOkToUnfocus;
    property TopMostVisible: boolean read GetTopMostVisible;

    property ShowSelectionNormal: boolean read GetShowSelectionNormal write SetShowSelectionNormal;
    property ScriptContext: TScriptContext read GetScriptContext;
    property MainFormBounds: TRect read GetMainFormBounds;
  end;

function StrToPictureFilter(const s: ansistring): TPictureFilter;
function ConvertToUTF8IfNeeded(const s: ansistring): ansistring;
procedure SafeSetFocus(AControl: TWinControl);

implementation

uses LCLProc, FileUtil, UResourceStrings;

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

function ConvertToUTF8IfNeeded(const s: ansistring): ansistring;
begin
  if FindInvalidUTF8Character(pchar(s),length(s)) = -1 then
    result := s
  else
    result := SysToUtf8(s);
end;

procedure SafeSetFocus(AControl: TWinControl);
begin
  try
    AControl.SetFocus;
  except
  end;
end;

{ TLazPaintCustomInstance }

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

procedure TLazPaintCustomInstance.SetBlackAndWhite(AValue: boolean);
begin
  if FBlackAndWhite=AValue then Exit;
  FBlackAndWhite:=AValue;
end;

procedure TLazPaintCustomInstance.ShowMessage(ACaption: string; AMessage: string; ADlgType: TMsgDlgType = mtInformation);
var top: TTopMostInfo;
begin
  top := HideTopmost;
  QuestionDlg(ACaption,AMessage,ADlgType,[mrOk,rsOkay],'');
  ShowTopmost(top);
end;

procedure TLazPaintCustomInstance.ShowError(ACaption: string; AMessage: string);
begin
  ShowMessage(ACaption,AMessage,mtError);
end;

end.
