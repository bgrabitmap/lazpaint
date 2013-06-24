unit LazPaintType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Inifiles, BGRABitmap, BGRABitmapTypes, uconfig, uimage, utool, Forms, BGRALayers, Graphics;

const
  //Version Number (to increment at each release)
  LazPaintCurrentVersionOnly = '5.6';

  { to do :

  load selection filename with additional slash
  filtre de vagues concentriques
  filtre de vagues en translation
  posterize
  G'MIC filters

  scripting

  integrate tools in window
  selection redraw optimization
  image bounds optimisation

  3D text

  linear gradient  
  hue curve
  hue/color blend mode

  keyboard zoom with mousepos
  
  noise filter
  box blur
  surface blur
  smart zoom using vectorization
  facelets
  
  erase tool in empty selection
  aliased selection
  antialiased magic wand

  import 3D object -> new layer
  texture for 3D objects

  layers shortcut
  layer solo
  inversible layer action undo optimisation

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
                    pfSharpen, pfSmooth, pfMedian, pfPixelate, pfClearType, pfClearTypeInverse, pfFunction,
                    pfEmboss, pfPhong, pfContour, pfGrayscale, pfNegative, pfLinearNegative, pfNormalize,
                    pfSphere, pfTwirl, pfCylinder, pfPlane,
                    pfPerlinNoise,pfCyclicPerlinNoise,pfClouds,pfCustomWater,pfWater,pfWood,pfWoodVertical,pfPlastik,pfMetalFloor,pfCamouflage,
                    pfSnowPrint,pfStone,pfRoundStone,pfMarble);

const
  PictureFilterStr : array[TPictureFilter] of string =
                   ('None',
                    'BlurPrecise', 'BlurRadial', 'BlurFast', 'BlurCorona', 'BlurDisk', 'BlurMotion', 'BlurCustom',
                    'Sharpen', 'Smooth', 'Median', 'Pixelate', 'ClearType', 'ClearTypeInverse', 'Function',
                    'Emboss', 'Phong', 'Contour', 'Grayscale', 'Negative', 'LinearNegative', 'Normalize',
                    'Sphere', 'Twirl', 'Cylinder', 'Plane',
                    'PerlinNoise','CyclicPerlinNoise','Clouds','CustomWater','Water','Wood','WoodVertical','Plastik','MetalFloor','Camouflage',
                    'SnowPrint','Stone','RoundStone','Marble');

  IsColoredFilter: array[TPictureFilter] of boolean =
                   (false,
                    false, false, false, false, false, false, false,
                    false, false, false, false, true, true, true,
                    false, true, false, false, false, false, false,
                    false, false, false, false,
                    false,false,true,true,true,true,true,true,true,true,
                    true,true,true,true);

const
  OutsideColor = TColor($00E8D1BB);

type
    ArrayOfBGRABitmap = array of TBGRABitmap;
    TColorTarget = (ctForeColor, ctBackColor);
    TFlipOption = (foAuto, foWholePicture, foSelection, foCurrentLayer);

type
  TLazPaintCustomInstance = class;
  TLazPaintInstanceEvent = procedure(AInstance : TLazPaintCustomInstance) of object;

  { TLazPaintCustomInstance }

  TLazPaintCustomInstance = class
  private
    FBlackAndWhite: boolean;
  protected
    function GetShowSelectionNormal: boolean; virtual; abstract;
    procedure SetShowSelectionNormal(AValue: boolean); virtual; abstract;
    function GetGridVisible: boolean; virtual; abstract;
    procedure SetGridVisible(const AValue: boolean); virtual; abstract;
    function GetEmbedded: boolean; virtual; abstract;
    function GetTopMostHasFocus: boolean; virtual; abstract;
    function GetTopMostVisible: boolean; virtual; abstract;

    function GetConfig: TLazPaintConfig; virtual; abstract;
    function GetImage: TLazPaintImage; virtual; abstract;
    function GetToolManager: TToolManager; virtual; abstract;
    procedure SetBlackAndWhite(AValue: boolean); virtual;

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

    function GetLayerWindowVisible: boolean; virtual; abstract;
    procedure SetLayerWindowVisible(AValue: boolean); virtual; abstract;
    function GetLayerWindowHeight: integer; virtual; abstract;
    function GetLayerWindowWidth: integer; virtual; abstract;
  public
    Title,AboutText: string;
    EmbeddedResult: TModalResult;
    EmbeddedImageBackup: TBGRABitmap;
    RestartQuery: boolean;

    constructor Create; virtual; abstract;
    constructor Create(AEmbedded: boolean); virtual; abstract;
    procedure UseConfig(ini: TInifile); virtual; abstract;
    procedure AssignBitmap(bmp: TBGRABitmap); virtual; abstract;
    procedure EditBitmap(var bmp: TBGRABitmap; ConfigStream: TStream = nil; ATitle: String = ''; AOnRun: TLazPaintInstanceEvent = nil; AOnExit: TLazPaintInstanceEvent = nil; ABlackAndWhite : boolean = false); virtual; abstract;
    procedure EditTexture; virtual; abstract;
    procedure EditSelection; virtual; abstract;
    function ProcessCommandLine: boolean; virtual; abstract;
    function ProcessCommands(commands: TStringList): boolean; virtual; abstract;
    procedure Show; virtual; abstract;
    procedure Run; virtual; abstract;
    procedure NotifyImageChange(RepaintNow: boolean; ARect: TRect); virtual; abstract;
    procedure NotifyImageChangeCompletely(RepaintNow: boolean); virtual; abstract;
    procedure NotifyStackChange; virtual; abstract;
    function TryOpenFile(filename: string): boolean; virtual; abstract;
    function ExecuteFilter(filter: TPictureFilter; skipDialog: boolean = false): boolean; virtual; abstract;
    procedure ColorFromFChooseColor; virtual; abstract;
    procedure ColorToFChooseColor; virtual; abstract;
    procedure ShowColorIntensityDlg; virtual; abstract;
    procedure ShowColorLightnessDlg; virtual; abstract;
    procedure ShowShiftColorsDlg; virtual; abstract;
    procedure ShowColorizeDlg; virtual; abstract;
    function ShowRadialBlurDlg(AFilterConnector: TObject;blurType:TRadialBlurType):boolean; virtual; abstract;
    function ShowMotionBlurDlg(AFilterConnector: TObject):boolean; virtual; abstract;
    function ShowCustomBlurDlg(AFilterConnector: TObject):boolean; virtual; abstract;
    function ShowEmbossDlg(AFilterConnector: TObject):boolean; virtual; abstract;
    function ShowPixelateDlg(AFilterConnector: TObject):boolean; virtual; abstract;
    function ShowTwirlDlg(AFilterConnector: TObject):boolean; virtual; abstract;
    function ShowPhongFilterDlg(AFilterConnector: TObject): boolean; virtual; abstract;
    function ShowFunctionFilterDlg(AFilterConnector: TObject): boolean; virtual; abstract;
    function ShowSharpenDlg(AFilterConnector: TObject):boolean; virtual; abstract;
    procedure HideTopmost; virtual; abstract;
    procedure ShowTopmost; virtual; abstract;
    procedure ShowCanvasSizeDlg; virtual; abstract;
    procedure ShowRepeatImageDlg; virtual; abstract;
    procedure ShowAboutDlg; virtual; abstract;
    function ShowNewImageDlg(out bitmap: TBGRABitmap):boolean; virtual; abstract;
    function ShowResampleDialog:boolean; virtual; abstract;

    property BlackAndWhite: boolean read FBlackAndWhite write SetBlackAndWhite;

    procedure NewLayer; virtual; abstract;
    procedure NewLayer(ALayer: TBGRABitmap; AName: string); virtual; abstract;
    procedure DuplicateLayer; virtual; abstract;
    procedure RemoveLayer; virtual; abstract;
    procedure MergeLayerOver; virtual; abstract;
    function MakeNewBitmapReplacement(AWidth, AHeight: integer): TBGRABitmap; virtual; abstract;
    procedure ChooseTool(Tool : TPaintToolType); virtual; abstract;

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
    property LayerWindowWidth: integer read GetLayerWindowWidth;
    property LayerWindowHeight: integer read GetLayerWindowHeight;
    property LayerWindowVisible: boolean read GetLayerWindowVisible write SetLayerWindowVisible;

    property ChooseColorTarget: TColorTarget read GetChooseColorTarget write setChooseColorTarget;
    property Config: TLazPaintConfig read GetConfig;
    property Image: TLazPaintImage read GetImage;
    property ToolManager: TToolManager read GetToolManager;
    property Embedded: boolean read GetEmbedded;
    property TopMostHasFocus: boolean read GetTopMostHasFocus;
    property TopMostVisible: boolean read GetTopMostVisible;

    property ShowSelectionNormal: boolean read GetShowSelectionNormal write SetShowSelectionNormal;
  end;

function StrToPictureFilter(const s: ansistring): TPictureFilter;
function ConvertToUTF8IfNeeded(const s: ansistring): ansistring;

implementation

uses LCLProc, FileUtil;

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

{ TLazPaintCustomInstance }

procedure TLazPaintCustomInstance.SetBlackAndWhite(AValue: boolean);
begin
  if FBlackAndWhite=AValue then Exit;
  FBlackAndWhite:=AValue;
end;

end.

