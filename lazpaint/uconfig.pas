unit UConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, BGRABitmapTypes, Graphics, LCLType, uscripting;

type
  TLazPaintConfig = class;

  IConfigProvider = interface
    function GetConfig: TLazPaintConfig;
  end;

  { TLazPaintConfig }

  TLazPaintConfig = class
  private
    iniOptions: TIniFile;
    recentFiles, recentDirs: TStringList;
    tempFont: TFont;
    colorizePresets: TList;
    FVersion: string;
    function GetBrushCount: integer;
    function GetBrushInfo(Index: integer): string;
    function GetColorizePreset(Index: Integer): TVariableSet;
    function GetColorizePresetCount: integer;
    function GetRecentDirectoriesCount: integer;
    function GetRecentDirectory(Index: Integer): string;
    function GetRecentFile(Index: Integer): string;
    function GetRecentFilesCount: integer;

  public
    Languages: TStringList;
    constructor Create(ini: TIniFile; AVersion: string);
    destructor Destroy; override;

    procedure InitRecentFiles;
    procedure AddRecentFile(filename: string);
    procedure AddRecentDirectory(dirname: string);
    procedure FinalizeRecentFiles;
    procedure SetBrushes(ABrushes: TStringList);
    function DefaultJpegQuality: integer;
    procedure SetDefaultJpegQuality(value: integer);
    function DefaultSaveOptionDialogMaximized: boolean;
    procedure SetDefaultSaveOptionDialogMaximized(value: boolean);

    procedure InitColorizePresets;
    function AddColorizePreset(AName: string; AHue, ASaturation: double; ACorrection: Boolean): integer;
    function AddColorizePreset(AParams: TVariableSet): integer;
    procedure RemoveColorizePreset(AIndex: integer);
    function IndexOfColorizePreset(AName: string): integer;
    procedure FinalizeColorizePresets;

    function DefaultLangage: string;
    class function ClassGetDefaultLangage(AIni: TIniFile): string;
    procedure SetDefaultLangage(value: string);

    function GetLastUpdateCheck: TDateTime;
    procedure SetLastUpdateCheck(value: TDateTime);

    function LatestVersion: string;
    procedure SetLatestVersion(value: string);
    procedure GetUpdatedLanguages(AList: TStringList);
    procedure SetUpdatedLanguages(AList: TStringList);
    class procedure ClassGetUpdatedLanguages(AList: TStringList; AIni: TIniFile; AVersion: string);
    class procedure ClassSetUpdatedLanguages(AList: TStringList; AIni: TIniFile; AVersion: string);
    procedure AddUpdatedLanguage(ALang: string);

    function Default3dObjectDirectory: string;
    procedure SetDefault3dObjectDirectory(value: string);
    function DefaultTextureDirectory: string;
    procedure SetDefaultTextureDirectory(value: string);
    function DefaultBrushDirectory: string;
    procedure SetDefaultBrushDirectory(value: string);
    function DefaultPaletteDirectory: string;
    procedure SetDefaultPaletteDirectory(value: string);

    function DefaultIconSize(defaultValue: integer): integer;
    procedure SetDefaultIconSize(value: integer);

    //new image config
    function DefaultImageWidth: integer;
    function DefaultImageHeight: integer;
    procedure SetDefaultImageWidth(value: integer);
    procedure SetDefaultImageHeight(value: integer);

    //resample config
    function DefaultResampleKeepAspectRatio: boolean;
    function DefaultResampleQuality: integer;
    procedure SetDefaultResampleKeepAspectRatio(value: boolean);
    procedure SetDefaultResampleQuality(value: integer);

    //window
    function DefaultScreenSize: TRect;
    function DefaultDockLayersAndColors: boolean;
    procedure SetDefaultDockLayersAndColors(value: boolean);
    function ScreenSizeChanged: boolean;
    procedure SetDefaultScreenSize(value: TRect);
    function DefaultMainWindowMaximized: boolean;
    procedure SetDefaultMainWindowMaximized(value: boolean);
    function DefaultMainWindowPosition: TRect;
    procedure SetDefaultMainWindowPosition(value: TRect);
    function DefaultColorWindowPosition: TRect;
    procedure SetDefaultColorWindowPosition(value: TRect);
    function DefaultLayerWindowPosition: TRect;
    procedure SetDefaultLayerWindowPosition(value: TRect);
    function DefaultToolboxWindowPosition: TRect;
    procedure SetDefaultToolboxWindowPosition(value: TRect);
    function DefaultToolboxDocking: string;
    procedure SetDefaultToolboxDocking(value: string);
    function DefaultImageListPosition: TRect;
    procedure SetDefaultImagelistWindowPosition(value: TRect);
    function DefaultBrowseWindowMaximized: boolean;
    procedure SetDefaultBrowseWindowMaximized(value: boolean);
    function DefaultBrowseWindowPosition: TRect;
    procedure SetDefaultBrowseWindowPosition(value: TRect);

    function DefaultPaletteToolbarVisible: boolean;
    procedure SetDefaultPaletteToolbarVisible(value: boolean);
    function DefaultColorWindowVisible: boolean;
    procedure SetDefaultColorWindowVisible(value: boolean);
    function DefaultLayerWindowVisible: boolean;
    procedure SetDefaultLayerWindowVisible(value: boolean);
    function DefaultToolboxWindowVisible: boolean;
    procedure SetDefaultToolboxWindowVisible(value: boolean);
    function DefaultImagelistWindowVisible: boolean;
    procedure SetDefaultImagelistWindowVisible(value: boolean);
    function DefaultGridVisible: boolean;
    procedure SetDefaultGridVisible(value: boolean);
    function DefaultZoomToolbarVisible: boolean;
    procedure SetDefaultZoomToolbarVisible(value: boolean);
    function DefaultUndoRedoToolbarVisible: boolean;
    procedure SetDefaultUndoRedoToolbarVisible(value: boolean);
    function DefaultCopyPasteToolbarVisible: boolean;
    procedure SetDefaultCopyPasteToolbarVisible(value: boolean);
    function DefaultCoordinatesToolbarVisible: boolean;
    procedure SetDefaultCoordinatesToolbarVisible(value: boolean);

    function GetStatusBarVisible: boolean;
    procedure SetStatusBarVisible(value: boolean);
    function DefaultUseImageBrowser: boolean;
    procedure SetDefaultUseImageBrowser(value: boolean);

    //tools
    function DefaultToolForeColor: TBGRAPixel;
    function DefaultToolBackColor: TBGRAPixel;
    procedure SetDefaultToolForeColor(value: TBGRAPixel);
    procedure SetDefaultToolBackColor(value: TBGRAPixel);
    function DefaultToolPenWidth: single;
    procedure SetDefaultToolPenWidth(value: single);
    function DefaultToolEraserWidth: single;
    procedure SetDefaultToolEraserWidth(value: single);
    function DefaultToolOptionDrawShape: boolean;
    procedure SetDefaultToolOptionDrawShape(value: boolean);
    function DefaultToolOptionFillShape: boolean;
    procedure SetDefaultToolOptionFillShape(value: boolean);
    function DefaultToolOptionCloseShape: boolean;
    procedure SetDefaultToolOptionCloseShape(value: boolean);
    function DefaultToolTolerance: integer;
    procedure SetDefaultToolTolerance(value: integer);
    function DefaultToolTextOutline: boolean;
    procedure SetDefaultToolTextOutline(value: boolean);
    function DefaultToolTextShadow: boolean;
    procedure SetDefaultToolTextShadow(value: boolean);
    function DefaultToolTextFont: TFont;
    procedure SetDefaultToolTextFont(value: TFont);
    function DefaultToolTextBlur: integer;
    procedure SetDefaultToolTextBlur(value: integer);
    function DefaultToolTextShadowOffsetX: integer;
    procedure SetDefaultToolTextShadowOffsetX(value: integer);
    function DefaultToolTextShadowOffsetY: integer;
    procedure SetDefaultToolTextShadowOffsetY(value: integer);
    procedure SetDefaultToolTextShadowOffset(value: TPoint);
    function DefaultToolTextShadowOffset: TPoint;
    function DefaultToolTextPhong: boolean;
    procedure SetDefaultToolTextPhong(value: boolean);

    function ToolPopupMessageShownCount(index: integer): integer;
    procedure SetToolPopupMessageShownCount(index: integer; AValue: integer);

    function DefaultToolLightPositionX: integer;
    procedure SetDefaultToolLightPositionX(value: integer);
    function DefaultToolLightPositionY: integer;
    procedure SetDefaultToolLightPositionY(value: integer);
    procedure SetDefaultToolLightPosition(value: TPoint);
    function DefaultToolLightPosition: TPoint;
    function DefaultToolLightAltitude: integer;
    procedure SetDefaultToolLightAltitude(value: integer);
    function DefaultToolShapeAltitude: integer;
    procedure SetDefaultToolShapeAltitude(value: integer);
    function DefaultToolShapeBorderSize: integer;
    procedure SetDefaultToolShapeBorderSize(value: integer);
    function DefaultToolShapeType: string;
    procedure SetDefaultToolShapeType(value: string);

    //radial blur config
    function DefaultBlurRadius: single;
    procedure SetDefaultBlurRadius(value: single);
    function DefaultPixelateSize: integer;
    procedure SetDefaultPixelateSize(value: integer);
    function DefaultPixelateQuality: string;
    procedure SetDefaultPixelateQuality(value: string);
    function DefaultSharpenAmount: single;
    procedure SetDefaultSharpenAmount(value: single);

    //motion blur config
    function DefaultBlurMotionAngle: double;
    function DefaultBlurMotionDistance: single;
    function DefaultBlurMotionOriented: boolean;
    procedure SetDefaultBlurMotionAngle(value: double);
    procedure SetDefaultBlurMotionDistance(value: single);
    procedure SetDefaultBlurMotionOriented(value: boolean);

    //custom blur config
    function DefaultCustomBlurMaskUTF8: string;
    procedure SetDefaultCustomBlurMaskUTF8(value: string);

    //emboss config
    function DefaultEmbossAngle: double;
    procedure SetDefaultEmbossAngle(value: double);

    //rain config
    function DefaultRainWind: double;
    procedure SetDefaultRainWind(value: double);
    function DefaultRainQuantity: double;
    procedure SetDefaultRainQuantity(value: double);

    //twirl config
    function DefaultTwirlRadius: double;
    procedure SetDefaultTwirlRadius(value: double);
    function DefaultTwirlTurn: double;
    procedure SetDefaultTwirlTurn(value: double);

    //phong filter
    function DefaultPhongFilterAltitude: integer;
    procedure SetDefaultPhongFilterAltitude(value: integer);

    //color config
    function DefaultPosterizeLevels: integer;
    procedure SetDefaultPosterizeLevels(value: integer);
    function DefaultPosterizeByLightness: boolean;
    procedure SetDefaultPosterizeByLightness(value: boolean);
    function DefaultUseGSBA: boolean;
    procedure SetDefaultUseGSBA(value: boolean);

    property RecentFilesCount : integer read GetRecentFilesCount;
    property RecentFile[Index: Integer]: string read GetRecentFile;
    property RecentDirectoriesCount : integer read GetRecentDirectoriesCount;
    property RecentDirectory[Index: Integer]: string read GetRecentDirectory;

    property ColorizePresetCount: integer read GetColorizePresetCount;
    property ColorizePreset[Index: Integer]: TVariableSet read GetColorizePreset;

    property BrushCount: integer read GetBrushCount;
    property BrushInfo[Index: integer]: string read GetBrushInfo;

    //ImageList
    function ImageListLastFolder: string;
    procedure SetImageListLastFolder(value: string);
    function ImageListAutoZoom: Boolean;
    procedure SetImageListAutoZoom(value: Boolean);
    function ImageListAutoUncheck: Boolean;
    procedure SetImageListAutoUncheck(value: Boolean);
    function ImageListAutoUncheckMode: Integer;
    procedure SetImageListAutoUncheckMode(value: integer);
  end;

function GetActualConfig: TIniFile;
function DefaultPicturesDirectory: string;

var
  ActualConfigDirUTF8 : string;
  MenuDefinitionKeys,MenuDefinitionValues: TStringList;

implementation

uses forms, uparse, LCLProc, FileUtil;

const maxRecentFiles = 10;
      maxRecentDirectories = 10;

//returns the config file to use
function GetActualConfig: TIniFile;
var
  PortableConfig: TIniFile;
  AppDirSys: string;
  PortableConfigFilenameSys: string;
  ActualConfigFilenameSys: string;
  {$IFDEF DARWIN}
  ConfigPath: string;
  {$ENDIF}
  i: integer;
begin
  ActualConfigFilenameSys := '';

  //check if a config file path is defined
  AppDirSys := ExtractFilePath(Application.ExeName);
  PortableConfigFilenameSys := AppDirSys+'lazpaint.ini';
  If FileExists(PortableConfigFilenameSys) then
  begin
    PortableConfig := TIniFile.Create(PortableConfigFilenameSys);
    ActualConfigFilenameSys:= PortableConfig.ReadString('General','ConfigFile','');
    if ActualConfigFilenameSys <> '' then
      ActualConfigFilenameSys:= ExpandFileName(AppDirSys+ActualConfigFilenameSys);
    MenuDefinitionKeys.Clear;
    MenuDefinitionValues.Clear;
    PortableConfig.ReadSection('Menu',MenuDefinitionKeys);
    for i := 0 to MenuDefinitionKeys.Count-1 do
      MenuDefinitionValues.Add(PortableConfig.ReadString('Menu',MenuDefinitionKeys[i],''));
    PortableConfig.Free;
  end;

  //otherwise, use default path
  if ActualConfigFilenameSys = '' then
  begin
    CreateDir(GetAppConfigDir(False));
    ActualConfigFilenameSys := GetAppConfigFile(False,False);
  end;

  {$IFDEF DARWIN}
  ConfigPath := ExtractFilePath(ActualConfigFilenameSys);
  CreateDir(ConfigPath);
  {$ENDIF}
  result := TIniFile.Create(ActualConfigFilenameSys,True);
  ActualConfigDirUTF8 := SysToUTF8(ExtractFilePath(ActualConfigFilenameSys));
end;

function DefaultPicturesDirectory: string;
begin
  DefaultPicturesDirectory := SysToUTF8(GetUserDir);
  if DirectoryExistsUTF8(AppendPathDelim(DefaultPicturesDirectory)+'Pictures') then
    DefaultPicturesDirectory := AppendPathDelim(DefaultPicturesDirectory)+'Pictures';
end;

{ TLazPaintConfig }

function TLazPaintConfig.DefaultImageWidth: integer;
begin
  result := iniOptions.ReadInteger('General','DefaultImageWidth',640);
end;

function TLazPaintConfig.DefaultImageHeight: integer;
begin
  result := iniOptions.ReadInteger('General','DefaultImageHeight',480);
end;

procedure TLazPaintConfig.SetDefaultImageWidth(value: integer);
begin
  iniOptions.WriteInteger('General','DefaultImageWidth',value);
end;

procedure TLazPaintConfig.SetDefaultImageHeight(value: integer);
begin
  iniOptions.WriteInteger('General','DefaultImageHeight',value);
end;

function TLazPaintConfig.DefaultResampleKeepAspectRatio: boolean;
begin
  result := iniOptions.ReadBool('General','ResampleKeepAspectRatio',true);
end;

function TLazPaintConfig.DefaultResampleQuality: integer;
begin
  result := iniOptions.ReadInteger('General','ResampleQuality',2);
end;

procedure TLazPaintConfig.SetDefaultResampleKeepAspectRatio(value: boolean);
begin
  iniOptions.WriteBool('General','ResampleKeepAspectRatio',value);
end;

procedure TLazPaintConfig.SetDefaultResampleQuality(value: integer);
begin
  iniOptions.WriteInteger('General','ResampleQuality',value);
end;

function TLazPaintConfig.DefaultScreenSize: TRect;
begin
 result := StrToRect(iniOptions.ReadString('Window','ScreenSize',''));
end;

function TLazPaintConfig.DefaultDockLayersAndColors: boolean;
begin
  result := iniOptions.ReadBool('Window','DockLayersAndColors',false);
end;

procedure TLazPaintConfig.SetDefaultDockLayersAndColors(value: boolean);
begin
  iniOptions.WriteBool('Window','DockLayersAndColors',value);
end;

procedure TLazPaintConfig.SetDefaultScreenSize(value: TRect);
begin
 iniOptions.WriteString('Window','ScreenSize',RectToStr(value));
end;

function TLazPaintConfig.DefaultMainWindowMaximized: boolean;
begin
  result := iniOptions.ReadBool('Window','MainWindowMaximized',false);
end;

procedure TLazPaintConfig.SetDefaultMainWindowMaximized(value: boolean);
begin
  iniOptions.WriteBool('Window','MainWindowMaximized',value);
end;

function TLazPaintConfig.DefaultMainWindowPosition: TRect;
begin
  result := StrToRect(iniOptions.ReadString('Window','MainWindowPosition',''));
end;

procedure TLazPaintConfig.SetDefaultMainWindowPosition(value: TRect);
begin
  iniOptions.WriteString('Window','MainWindowPosition',RectToStr(value));
  SetDefaultMainWindowMaximized(False);
end;

function TLazPaintConfig.DefaultColorWindowPosition: TRect;
begin
  result := StrToRect(iniOptions.ReadString('Window','ColorWindowPosition',''));
end;

procedure TLazPaintConfig.SetDefaultColorWindowPosition(value: TRect);
begin
  iniOptions.WriteString('Window','ColorWindowPosition',RectToStr(value));
end;

function TLazPaintConfig.DefaultLayerWindowPosition: TRect;
begin
  result := StrToRect(iniOptions.ReadString('Window','LayerWindowPosition',''));
end;

procedure TLazPaintConfig.SetDefaultLayerWindowPosition(value: TRect);
begin
  iniOptions.WriteString('Window','LayerWindowPosition',RectToStr(value));
end;

function TLazPaintConfig.DefaultToolboxWindowPosition: TRect;
begin
  result := StrToRect(iniOptions.ReadString('Window','ToolboxWindowPosition',''));
end;

procedure TLazPaintConfig.SetDefaultToolboxWindowPosition(value: TRect);
begin
  iniOptions.WriteString('Window','ToolboxWindowPosition',RectToStr(value));
end;

function TLazPaintConfig.DefaultToolboxDocking: string;
begin
  result := iniOptions.ReadString('Window','ToolBoxDocking','Left');
end;

procedure TLazPaintConfig.SetDefaultToolboxDocking(value: string);
begin
  iniOptions.WriteString('Window','ToolBoxDocking',value);
end;

function TLazPaintConfig.DefaultImageListPosition: TRect;
begin
  result := StrToRect(iniOptions.ReadString('Window','ImagelistWindowPosition',''));
end;

procedure TLazPaintConfig.SetDefaultImagelistWindowPosition(value: TRect);
begin
  iniOptions.WriteString('Window','ImagelistWindowPosition',RectToStr(value));
end;

function TLazPaintConfig.DefaultBrowseWindowMaximized: boolean;
begin
  result := iniOptions.ReadBool('Window','BrowseWindowMaximized',false);
end;

procedure TLazPaintConfig.SetDefaultBrowseWindowMaximized(value: boolean);
begin
  iniOptions.WriteBool('Window','BrowseWindowMaximized',value);
end;

function TLazPaintConfig.DefaultBrowseWindowPosition: TRect;
begin
  result := StrToRect(iniOptions.ReadString('Window','BrowseWindowPosition',''));
end;

procedure TLazPaintConfig.SetDefaultBrowseWindowPosition(value: TRect);
begin
  iniOptions.WriteString('Window','BrowseWindowPosition',RectToStr(value));
end;

function TLazPaintConfig.DefaultPaletteToolbarVisible: boolean;
begin
  result := iniOptions.ReadBool('Toolbar','PaletteToolbar',true);
end;

procedure TLazPaintConfig.SetDefaultPaletteToolbarVisible(value: boolean);
begin
  iniOptions.WriteBool('Toolbar','PaletteToolbar',value);
end;

function TLazPaintConfig.DefaultColorWindowVisible: boolean;
begin
  result := iniOptions.ReadBool('Window','ColorWindowVisible',true);
end;

procedure TLazPaintConfig.SetDefaultColorWindowVisible(value: boolean);
begin
  iniOptions.WriteBool('Window','ColorWindowVisible',value);
end;

function TLazPaintConfig.DefaultLayerWindowVisible: boolean;
begin
  result := iniOptions.ReadBool('Window','LayerWindowVisible',true);
end;

procedure TLazPaintConfig.SetDefaultLayerWindowVisible(value: boolean);
begin
  iniOptions.WriteBool('Window','LayerWindowVisible',value);
end;

function TLazPaintConfig.DefaultToolboxWindowVisible: boolean;
begin
  result := iniOptions.ReadBool('Window','ToolboxWindowVisible',true);
end;

function TLazPaintConfig.DefaultImagelistWindowVisible: boolean;
begin
  result := iniOptions.ReadBool('Window','ImagelistWindowVisible',false);
end;

procedure TLazPaintConfig.SetDefaultToolboxWindowVisible(value: boolean);
begin
  iniOptions.WriteBool('Window','ToolboxWindowVisible',value);
end;

procedure TLazPaintConfig.SetDefaultImagelistWindowVisible(value: boolean);
begin
  iniOptions.WriteBool('Window','ImagelistWindowVisible',value);
end;

function TLazPaintConfig.DefaultGridVisible: boolean;
begin
  result := iniOptions.ReadBool('General','GridVisible',true);
end;

procedure TLazPaintConfig.SetDefaultGridVisible(value: boolean);
begin
  iniOptions.WriteBool('General','GridVisible',value);
end;

function TLazPaintConfig.DefaultZoomToolbarVisible: boolean;
begin
  result := iniOptions.ReadBool('Toolbar','ZoomToolbar',true);
end;

procedure TLazPaintConfig.SetDefaultZoomToolbarVisible(value: boolean);
begin
  iniOptions.WriteBool('Toolbar','ZoomToolbar',value);
end;

function TLazPaintConfig.DefaultUndoRedoToolbarVisible: boolean;
begin
  result := iniOptions.ReadBool('Toolbar','UndoRedoToolbar',false);
end;

procedure TLazPaintConfig.SetDefaultUndoRedoToolbarVisible(value: boolean);
begin
  iniOptions.WriteBool('Toolbar','UndoRedoToolbar',value);
end;

function TLazPaintConfig.DefaultCopyPasteToolbarVisible: boolean;
begin
  result := iniOptions.ReadBool('Toolbar','CopyPasteToolbar',false);
end;

procedure TLazPaintConfig.SetDefaultCopyPasteToolbarVisible(value: boolean);
begin
  iniOptions.WriteBool('Toolbar','CopyPasteToolbar',value);
end;

function TLazPaintConfig.DefaultCoordinatesToolbarVisible: boolean;
begin
  result := iniOptions.ReadBool('Toolbar','CoordinatesToolbar',true);
end;

procedure TLazPaintConfig.SetDefaultCoordinatesToolbarVisible(value: boolean);
begin
  iniOptions.WriteBool('Toolbar','CoordinatesToolbar',value);
end;

function TLazPaintConfig.GetStatusBarVisible: boolean;
begin
  result := iniOptions.ReadBool('Toolbar','StatusBar',true);
end;

procedure TLazPaintConfig.SetStatusBarVisible(value: boolean);
begin
  iniOptions.WriteBool('Toolbar','StatusBar',value);
end;

function TLazPaintConfig.DefaultUseImageBrowser: boolean;
begin
  result := iniOptions.ReadBool('General','UseImageBrowser',{$IFDEF DARWIN}false{$ELSE}true{$ENDIF});
end;

procedure TLazPaintConfig.SetDefaultUseImageBrowser(value: boolean);
begin
  iniOptions.WriteBool('General','UseImageBrowser',value);
end;

function TLazPaintConfig.DefaultToolForeColor: TBGRAPixel;
begin
  result := StrToBGRA(iniOptions.ReadString('Tool','ForeColor','00000080'));
end;

function TLazPaintConfig.DefaultToolBackColor: TBGRAPixel;
begin
  result := StrToBGRA(iniOptions.ReadString('Tool','BackColor','0080FFC0'));
end;

procedure TLazPaintConfig.SetDefaultToolForeColor(value: TBGRAPixel);
begin
  iniOptions.WriteString('Tool','ForeColor',BGRAToStr(value));
end;

procedure TLazPaintConfig.SetDefaultToolBackColor(value: TBGRAPixel);
begin
  iniOptions.WriteString('Tool','BackColor',BGRAToStr(value));
end;

function TLazPaintConfig.DefaultToolPenWidth: single;
begin
  result := iniOptions.ReadFloat('Tool','PenWidth',5);
end;

procedure TLazPaintConfig.SetDefaultToolPenWidth(value: single);
begin
  iniOptions.WriteFloat('Tool','PenWidth',value);
end;

function TLazPaintConfig.DefaultToolEraserWidth: single;
begin
  result := iniOptions.ReadFloat('Tool','EraserWidth',10);
end;

procedure TLazPaintConfig.SetDefaultToolEraserWidth(value: single);
begin
  iniOptions.WriteFloat('Tool','EraserWidth',value);
end;

function TLazPaintConfig.DefaultToolOptionDrawShape: boolean;
begin
  result := iniOptions.ReadBool('Tool','DrawShape',true);
end;

procedure TLazPaintConfig.SetDefaultToolOptionDrawShape(value: boolean);
begin
  iniOptions.WriteBool('Tool','DrawShape',value);
end;

function TLazPaintConfig.DefaultToolOptionFillShape: boolean;
begin
  result := iniOptions.ReadBool('Tool','FillShape',true);
end;

procedure TLazPaintConfig.SetDefaultToolOptionFillShape(value: boolean);
begin
  iniOptions.WriteBool('Tool','FillShape',value);
end;

function TLazPaintConfig.DefaultToolOptionCloseShape: boolean;
begin
  result := iniOptions.ReadBool('Tool','CloseShape',true);
end;

procedure TLazPaintConfig.SetDefaultToolOptionCloseShape(value: boolean);
begin
  iniOptions.WriteBool('Tool','CloseShape',value);
end;

function TLazPaintConfig.DefaultToolTolerance: integer;
begin
  result := iniOptions.ReadInteger('Tool','Tolerance',64);
end;

procedure TLazPaintConfig.SetDefaultToolTolerance(value: integer);
begin
  iniOptions.WriteInteger('Tool','Tolerance',value);
end;

function TLazPaintConfig.DefaultToolTextOutline: boolean;
begin
  result := iniOptions.ReadBool('Tool','TextOutline',false);
end;

procedure TLazPaintConfig.SetDefaultToolTextOutline(value: boolean);
begin
  iniOptions.WriteBool('Tool','TextOutline',value);
end;

function TLazPaintConfig.DefaultToolTextShadow: boolean;
begin
  result := iniOptions.ReadBool('Tool','TextShadow',false);
end;

procedure TLazPaintConfig.SetDefaultToolTextShadow(value: boolean);
begin
  iniOptions.WriteBool('Tool','TextShadow',value);
end;

function TLazPaintConfig.DefaultToolTextFont: TFont;
var fontStyle: TFontStyles;
begin
  tempFont.Name := iniOptions.ReadString('Tool','TextFontName','Arial');
  tempFont.Height := iniOptions.ReadInteger('Tool','TextFontHeight',-13);
  tempFont.CharSet := iniOptions.ReadInteger('Tool','TextFontCharSet',ANSI_CHARSET);
  fontStyle := [];
  if iniOptions.ReadBool('Tool','TextFontBold',False) then fontStyle += [fsBold];
  if iniOptions.ReadBool('Tool','TextFontItalic',False) then fontStyle += [fsItalic];
  if iniOptions.ReadBool('Tool','TextFontStrikeOut',False) then fontStyle += [fsStrikeOut];
  if iniOptions.ReadBool('Tool','TextFontUnderline',False) then fontStyle += [fsUnderline];
  tempFont.Style := fontStyle;
  result := tempFont;
end;

procedure TLazPaintConfig.SetDefaultToolTextFont(value: TFont);
begin
  tempFont.Assign(value);

  iniOptions.WriteString('Tool','TextFontName',tempFont.Name);
  iniOptions.WriteInteger('Tool','TextFontHeight',tempFont.Height);
  iniOptions.WriteInteger('Tool','TextFontCharSet',tempFont.CharSet);
  iniOptions.WriteBool('Tool','TextFontBold',fsBold in tempFont.Style);
  iniOptions.WriteBool('Tool','TextFontItalic',fsItalic in tempFont.Style);
  iniOptions.WriteBool('Tool','TextFontStrikeOut',fsStrikeOut in tempFont.Style);
  iniOptions.WriteBool('Tool','TextFontUnderline',fsUnderline in tempFont.Style);
end;

function TLazPaintConfig.DefaultToolTextBlur: integer;
begin
  result := iniOptions.ReadInteger('Tool','TextBlur',4);
end;

procedure TLazPaintConfig.SetDefaultToolTextBlur(value: integer);
begin
  iniOptions.WriteInteger('Tool','TextBlur',value);
end;

function TLazPaintConfig.DefaultToolTextShadowOffsetX: integer;
begin
  result := iniOptions.ReadInteger('Tool','TextShadowX',5);
end;

procedure TLazPaintConfig.SetDefaultToolTextShadowOffsetX(value: integer);
begin
  iniOptions.WriteInteger('Tool','TextShadowX',value);
end;

function TLazPaintConfig.DefaultToolTextShadowOffsetY: integer;
begin
  result := iniOptions.ReadInteger('Tool','TextShadowY',5);
end;

procedure TLazPaintConfig.SetDefaultToolTextShadowOffsetY(value: integer);
begin
  iniOptions.WriteInteger('Tool','TextShadowY',value);
end;

procedure TLazPaintConfig.SetDefaultToolTextShadowOffset(value: TPoint);
begin
  SetDefaultToolTextShadowOffsetX(value.X);
  SetDefaultToolTextShadowOffsetY(value.Y);
end;

function TLazPaintConfig.DefaultToolTextShadowOffset: TPoint;
begin
  result := Point(DefaultToolTextShadowOffsetX,DefaultToolTextShadowOffsetY);
end;

function TLazPaintConfig.DefaultToolTextPhong: boolean;
begin
  result := iniOptions.ReadBool('Tool','TextPhong',false);
end;

procedure TLazPaintConfig.SetDefaultToolTextPhong(value: boolean);
begin
  iniOptions.WriteBool('Tool','TextPhong',value);
end;

function TLazPaintConfig.ToolPopupMessageShownCount(index: integer): integer;
begin
  result := iniOptions.ReadInteger('Popup','ToolPopupMessage' + inttostr(index),0);
end;

procedure TLazPaintConfig.SetToolPopupMessageShownCount(index: integer;
  AValue: integer);
begin
  iniOptions.WriteInteger('Popup','ToolPopupMessage' + inttostr(index), avalue);
end;

function TLazPaintConfig.DefaultToolLightPositionX: integer;
begin
  result := iniOptions.ReadInteger('Tool','LightPositionX',0);
end;

procedure TLazPaintConfig.SetDefaultToolLightPositionX(value: integer);
begin
  iniOptions.WriteInteger('Tool','LightPositionX',value);
end;

function TLazPaintConfig.DefaultToolLightPositionY: integer;
begin
  result := iniOptions.ReadInteger('Tool','LightPositionY',0);
end;

procedure TLazPaintConfig.SetDefaultToolLightPositionY(value: integer);
begin
  iniOptions.WriteInteger('Tool','LightPositionY',value);
end;

procedure TLazPaintConfig.SetDefaultToolLightPosition(value: TPoint);
begin
  SetDefaultToolLightPositionX(value.X);
  SetDefaultToolLightPositionY(value.Y);
end;

function TLazPaintConfig.DefaultToolLightPosition: TPoint;
begin
  result := Point(DefaultToolLightPositionX,DefaultToolLightPositionY);
end;

function TLazPaintConfig.DefaultToolLightAltitude: integer;
begin
  result := iniOptions.ReadInteger('Filter','LightPositionZ',100);
end;

procedure TLazPaintConfig.SetDefaultToolLightAltitude(value: integer);
begin
  iniOptions.WriteInteger('Filter','LightPositionZ',value);
end;

function TLazPaintConfig.DefaultToolShapeAltitude: integer;
begin
  result := iniOptions.ReadInteger('Filter','ShapeAltitude',50);
end;

procedure TLazPaintConfig.SetDefaultToolShapeAltitude(value: integer);
begin
  iniOptions.WriteInteger('Filter','ShapeAltitude',value);
end;

function TLazPaintConfig.DefaultToolShapeBorderSize: integer;
begin
  result := iniOptions.ReadInteger('Filter','ShapeBorderSize',20);
end;

procedure TLazPaintConfig.SetDefaultToolShapeBorderSize(value: integer);
begin
  iniOptions.WriteInteger('Filter','ShapeBorderSize',value);
end;

function TLazPaintConfig.DefaultToolShapeType: string;
begin
  result := iniOptions.ReadString('Filter','ShapeType','Rectangle');
end;

procedure TLazPaintConfig.SetDefaultToolShapeType(value: string);
begin
  iniOptions.WriteString('Filter','ShapeType',value);
end;

function TLazPaintConfig.DefaultBlurRadius: single;
begin
  result := iniOptions.ReadFloat('Filter','BlurRadius',5);
end;

procedure TLazPaintConfig.SetDefaultBlurRadius(value: single);
begin
  iniOptions.WriteFloat('Filter','BlurRadius',value);
end;

function TLazPaintConfig.DefaultPixelateSize: integer;
begin
  result := iniOptions.ReadInteger('Filter','PixelateSize',5);
end;

procedure TLazPaintConfig.SetDefaultPixelateSize(value: integer);
begin
  iniOptions.WriteInteger('Filter','PixelateSize',value);
end;

function TLazPaintConfig.DefaultPixelateQuality: string;
begin
  result := iniOptions.ReadString('Filter','PixelateQuality','Linear');
end;

procedure TLazPaintConfig.SetDefaultPixelateQuality(value: string);
begin
  iniOptions.WriteString('Filter','PixelateQuality',value);
end;

function TLazPaintConfig.DefaultSharpenAmount: single;
begin
  result := iniOptions.ReadFloat('Filter','SharpenAmount',1);
end;

procedure TLazPaintConfig.SetDefaultSharpenAmount(value: single);
begin
  iniOptions.WriteFloat('Filter','SharpenAmount',value);
end;

function TLazPaintConfig.DefaultBlurMotionAngle: double;
begin
  result := iniOptions.ReadFloat('Filter','MotionBlurAngle',0);
end;

function TLazPaintConfig.DefaultBlurMotionDistance: single;
begin
  result := iniOptions.ReadFloat('Filter','MotionBlurDistance',5);
end;

function TLazPaintConfig.DefaultBlurMotionOriented: boolean;
begin
  result := iniOptions.ReadBool('Filter','MotionBlurOriented',true);
end;

procedure TLazPaintConfig.SetDefaultBlurMotionAngle(value: double);
begin
  iniOptions.WriteFloat('Filter','MotionBlurAngle',value);
end;

procedure TLazPaintConfig.SetDefaultBlurMotionDistance(value: single);
begin
  iniOptions.WriteFloat('Filter','MotionBlurDistance',value);
end;

procedure TLazPaintConfig.SetDefaultBlurMotionOriented(value: boolean);
begin
  iniOptions.WriteBool('Filter','MotionBlurOriented',value);
end;

function TLazPaintConfig.DefaultCustomBlurMaskUTF8: string;
begin
  result := iniOptions.ReadString('Filter','CustomBlurMask','');
end;

procedure TLazPaintConfig.SetDefaultCustomBlurMaskUTF8(value: string);
begin
  iniOptions.WriteString('Filter','CustomBlurMask',value);
end;

function TLazPaintConfig.DefaultEmbossAngle: double;
begin
  result := iniOptions.ReadFloat('Filter','EmbossAngle',45);
end;

procedure TLazPaintConfig.SetDefaultEmbossAngle(value: double);
begin
  iniOptions.WriteFloat('Filter','EmbossAngle',value);
end;

function TLazPaintConfig.DefaultRainWind: double;
begin
  result := iniOptions.ReadFloat('Filter','RainWind',-0.5);
end;

procedure TLazPaintConfig.SetDefaultRainWind(value: double);
begin
  iniOptions.WriteFloat('Filter','RainWind',value);
end;

function TLazPaintConfig.DefaultRainQuantity: double;
begin
  result := iniOptions.ReadFloat('Filter','RainQuantity',0.5);
end;

procedure TLazPaintConfig.SetDefaultRainQuantity(value: double);
begin
  iniOptions.WriteFloat('Filter','RainQuantity',value);
end;

function TLazPaintConfig.DefaultTwirlRadius: double;
begin
  result := iniOptions.ReadFloat('Filter','TwirlRadius',100);
end;

procedure TLazPaintConfig.SetDefaultTwirlRadius(value: double);
begin
  iniOptions.WriteFloat('Filter','TwirlRadius',value);
end;

function TLazPaintConfig.DefaultTwirlTurn: double;
begin
  result := iniOptions.ReadFloat('Filter','TwirlTurn',1);
end;

procedure TLazPaintConfig.SetDefaultTwirlTurn(value: double);
begin
  iniOptions.WriteFloat('Filter','TwirlTurn',value);
end;

function TLazPaintConfig.DefaultPhongFilterAltitude: integer;
begin
  result := iniOptions.ReadInteger('Filter','MapAltitude',10);
end;

procedure TLazPaintConfig.SetDefaultPhongFilterAltitude(value: integer);
begin
  iniOptions.WriteInteger('Filter','MapAltitude',value);
end;

function TLazPaintConfig.DefaultPosterizeLevels: integer;
begin
  result := iniOptions.ReadInteger('Filter','PosterizeLevels',4);
end;

procedure TLazPaintConfig.SetDefaultPosterizeLevels(value: integer);
begin
  iniOptions.WriteInteger('Filter','PosterizeLevels',value);
end;

function TLazPaintConfig.DefaultPosterizeByLightness: boolean;
begin
  result := iniOptions.ReadBool('Filter','PosterizeByLightness',True);
end;

procedure TLazPaintConfig.SetDefaultPosterizeByLightness(value: boolean);
begin
  iniOptions.WriteBool('Filter','PosterizeByLightness',value);
end;

function TLazPaintConfig.DefaultUseGSBA: boolean;
begin
  result := iniOptions.ReadBool('Filter','UseGSBA',True);
end;

procedure TLazPaintConfig.SetDefaultUseGSBA(value: boolean);
begin
  iniOptions.WriteBool('Filter','UseGSBA',value);
end;

procedure TLazPaintConfig.InitRecentFiles;
var i: integer;
begin
  recentFiles := TStringList.Create;
  iniOptions.ReadSection('RecentFiles',recentFiles);
  for i := 0 to recentFiles.Count-1 do
    recentFiles[i] := iniOptions.ReadString('RecentFiles',recentFiles[i],'');
  for i := recentFiles.Count-1 downto 0 do
    if not FileExistsUTF8(recentFiles[i]) then recentFiles.Delete(i);

  recentDirs := TStringList.Create;
  iniOptions.ReadSection('RecentDirectories',recentDirs);
  for i := 0 to recentDirs.Count-1 do
    recentDirs[i] := iniOptions.ReadString('RecentDirectories',recentDirs[i],'');
  for i := recentDirs.Count-1 downto 0 do
    if not DirectoryExistsUTF8(recentDirs[i]) then recentDirs.Delete(i);
end;

procedure TLazPaintConfig.FinalizeRecentFiles;
var i: integer;
begin
  iniOptions.EraseSection('RecentFiles');
  for i := 0 to recentFiles.Count-1 do
    iniOptions.WriteString('RecentFiles','File'+inttostr(I+1),recentFiles[i]);
  recentFiles.Free;

  iniOptions.EraseSection('RecentDirectories');
  for i := 0 to recentDirs.Count-1 do
    iniOptions.WriteString('RecentDirectories','Dir'+inttostr(I+1),recentDirs[i]);
  recentDirs.Free;
end;

procedure TLazPaintConfig.SetBrushes(ABrushes: TStringList);
var
  i: Integer;
begin
  iniOptions.EraseSection('Brushes');
  iniOptions.WriteInteger('Brushes','Count',ABrushes.Count);
  for i := 0 to ABrushes.Count-1 do
    iniOptions.WriteString('Brushes','Brush'+inttostr(I+1),ABrushes[i]);
end;

function TLazPaintConfig.DefaultJpegQuality: integer;
begin
  result := iniOptions.ReadInteger('General','JpegQuality',100);
end;

procedure TLazPaintConfig.SetDefaultJpegQuality(value: integer);
begin
  iniOptions.WriteInteger('General','JpegQuality',value);
end;

function TLazPaintConfig.DefaultSaveOptionDialogMaximized: boolean;
begin
  result := iniOptions.ReadBool('Window','SaveOptionDialogMaximized',false);
end;

procedure TLazPaintConfig.SetDefaultSaveOptionDialogMaximized(value: boolean);
begin
  iniOptions.WriteBool('Window','SaveOptionDialogMaximized',value);
end;

procedure TLazPaintConfig.InitColorizePresets;
var presetsStr: TStringList;
  i: integer;
begin
  presetsStr := TStringList.Create;
  iniOptions.ReadSection('Colorize',presetsStr);
  colorizePresets := TList.Create;
  for i := 0 to presetsStr.Count-1 do
   begin
     AddColorizePreset(TVariableSet.Create('ColorColorize',iniOptions.ReadString('Colorize',presetsStr[i],'')));
   end;
  presetsStr.free;
  if ColorizePresetCount = 0 then
  begin
    AddColorizePreset('Antartic',211,0.660,True);
    AddColorizePreset('Mars',16.5,0.902,True);
    AddColorizePreset('Purple',291,0.634,False);
    AddColorizePreset('Sepia',31.0,0.119,False);
  end;
end;

function TLazPaintConfig.AddColorizePreset(AName: string; AHue,
  ASaturation: double; ACorrection: Boolean): integer;
var
  params: TVariableSet;
begin
  params := TVariableSet.Create('ColorColorize');
  params.AddString('Name',AName);
  params.AddFloat('Hue', AHue);
  params.AddFloat('Saturation',ASaturation);
  params.AddBoolean('Correction',ACorrection);
  result := AddColorizePreset(params);
end;

function TLazPaintConfig.AddColorizePreset(AParams: TVariableSet): integer;
var i: integer;
  s: string;
begin
  s := AParams.Strings['Name'];
  for i := ColorizePresetCount-1 downto 0 do
    if CompareText(colorizePreset[i].Strings['Name'],s) = 0 then
      RemoveColorizePreset(i);
  result := colorizePresets.Add(AParams);
end;

procedure TLazPaintConfig.RemoveColorizePreset(AIndex: integer);
begin
  if (AIndex >= 0) and (AIndex < ColorizePresetCount) then
  begin
    ColorizePreset[AIndex].Free;
    colorizePresets.Delete(AIndex);
  end;
end;

function TLazPaintConfig.IndexOfColorizePreset(AName: string): integer;
var i: integer;
begin
  for i := ColorizePresetCount-1 downto 0 do
    if CompareText(colorizePreset[i].Strings['Name'],AName) = 0 then
    begin
      result := i;
      exit;
    end;
  result := -1;
end;

procedure TLazPaintConfig.FinalizeColorizePresets;
var i: integer;
begin
  iniOptions.EraseSection('Colorize');
  for i := 0 to ColorizePresetCount-1 do
    iniOptions.WriteString('Colorize','Preset'+inttostr(I+1),ColorizePreset[i].VariablesAsString);

  for i := 0 to ColorizePresetCount-1 do
    ColorizePreset[i].Free;
  colorizePresets.Free;
end;

function TLazPaintConfig.DefaultLangage: string;
begin
  result := ClassGetDefaultLangage(iniOptions);
end;

class function TLazPaintConfig.ClassGetDefaultLangage(AIni: TIniFile): string;
begin
  result := AIni.ReadString('General','Language','');
  if result = '' then result := 'auto';
end;

procedure TLazPaintConfig.SetDefaultLangage(value: string);
begin
  if value = 'auto' then value := '';
  iniOptions.WriteString('General','Language',value);
end;

function TLazPaintConfig.GetLastUpdateCheck: TDateTime;
begin
  result := iniOptions.ReadInt64('General','LastUpdateCheck',0);
end;

procedure TLazPaintConfig.SetLastUpdateCheck(value: TDateTime);
begin
 iniOptions.WriteInt64('General','LastUpdateCheck',round(value));
end;

function TLazPaintConfig.LatestVersion: string;
begin
  result := iniOptions.ReadString('General','LatestOnlineVersion','');
end;

procedure TLazPaintConfig.SetLatestVersion(value: string);
begin
  iniOptions.WriteString('General','LatestOnlineVersion',value);
end;

procedure TLazPaintConfig.GetUpdatedLanguages(AList: TStringList);
begin
  ClassGetUpdatedLanguages(AList,iniOptions,FVersion);
end;

procedure TLazPaintConfig.SetUpdatedLanguages(AList: TStringList);
begin
  ClassSetUpdatedLanguages(AList,iniOptions,FVersion);
end;

class procedure TLazPaintConfig.ClassGetUpdatedLanguages(AList: TStringList;
  AIni: TIniFile; AVersion: string);
begin
  AList.CommaText := AIni.ReadString('General','UpdatedLanguages'+AVersion,'');
end;

class procedure TLazPaintConfig.ClassSetUpdatedLanguages(AList: TStringList;
  AIni: TIniFile; AVersion: string);
begin
  AIni.WriteString('General','UpdatedLanguages'+AVersion,AList.CommaText)
end;

procedure TLazPaintConfig.AddUpdatedLanguage(ALang: string);
var list: TStringList;
begin
  list := TStringList.Create;
  GetUpdatedLanguages(list);
  if list.IndexOf(ALang)=-1 then
  begin
    list.Add(ALang);
    SetUpdatedLanguages(list);
  end;
  list.Free;
end;

function TLazPaintConfig.Default3dObjectDirectory: string;
begin
  result := iniOptions.ReadString('General','3dObjectDirectory','');
end;

procedure TLazPaintConfig.SetDefault3dObjectDirectory(value: string);
begin
  iniOptions.WriteString('General','3dObjectDirectory',ChompPathDelim(value))
end;

function TLazPaintConfig.DefaultTextureDirectory: string;
begin
  result := iniOptions.ReadString('General','TextureDirectory',DefaultPicturesDirectory);
end;

procedure TLazPaintConfig.SetDefaultTextureDirectory(value: string);
begin
  iniOptions.WriteString('General','TextureDirectory',ChompPathDelim(value))
end;

function TLazPaintConfig.DefaultBrushDirectory: string;
begin
  result := iniOptions.ReadString('General','BrushDirectory',DefaultPicturesDirectory);
end;

procedure TLazPaintConfig.SetDefaultBrushDirectory(value: string);
begin
  iniOptions.WriteString('General','BrushDirectory',ChompPathDelim(value))
end;

function TLazPaintConfig.DefaultPaletteDirectory: string;
begin
  result := iniOptions.ReadString('General','PaletteDirectory','');
end;

procedure TLazPaintConfig.SetDefaultPaletteDirectory(value: string);
begin
  iniOptions.WriteString('General','PaletteDirectory',ChompPathDelim(value))
end;

function TLazPaintConfig.DefaultIconSize(defaultValue: integer): integer;
begin
  result := iniOptions.ReadInteger('General','DefaultIconSize',0);
  if result = 0 then result := defaultValue;
end;

procedure TLazPaintConfig.SetDefaultIconSize(value: integer);
begin
  iniOptions.WriteInteger('General','DefaultIconSize',value);
end;

function TLazPaintConfig.ScreenSizeChanged: boolean;
var currentScreenSize,previousScreenSize: TRect;
begin
  currentScreenSize := rect(0,0,screen.Width,screen.Height);
  previousScreenSize := DefaultScreenSize;
  if not CompareRect(@previousScreenSize,@currentScreenSize.left) then
  begin
    SetDefaultScreenSize(currentScreenSize);
    result := true;
  end else
    result := false;
end;

procedure TLazPaintConfig.AddRecentFile(filename: string);
var idx : integer;
begin
  idx := recentFiles.IndexOf(filename);
  if idx <> -1 then
    recentFiles.Delete(idx);
  recentFiles.Insert(0,filename);
  while recentFiles.count > maxRecentFiles do
    recentFiles.Delete(recentFiles.count-1);
end;

procedure TLazPaintConfig.AddRecentDirectory(dirname: string);
var idx : integer;
begin
  dirname := ChompPathDelim(dirname);
  idx := recentDirs.IndexOf(dirname);
  if idx <> -1 then
    recentDirs.Delete(idx);
  recentDirs.Insert(0,dirname);
  while recentDirs.count > maxRecentDirectories do
    recentDirs.Delete(recentDirs.count-1);
end;

function TLazPaintConfig.GetRecentFilesCount: integer;
begin
  result := recentFiles.Count;
end;

function TLazPaintConfig.GetColorizePreset(Index: Integer): TVariableSet;
begin
  result := TVariableSet(colorizePresets[Index]);
end;

function TLazPaintConfig.GetBrushCount: integer;
begin
  result := iniOptions.ReadInteger('Brushes','Count',0);
end;

function TLazPaintConfig.GetBrushInfo(Index: integer): string;
begin
  result := iniOptions.ReadString('Brushes','Brush'+IntToStr(index+1),'');
end;

function TLazPaintConfig.GetColorizePresetCount: integer;
begin
  result := colorizePresets.Count;
end;

function TLazPaintConfig.GetRecentDirectoriesCount: integer;
begin
  result := recentDirs.Count;
end;

function TLazPaintConfig.GetRecentDirectory(Index: Integer): string;
begin
  result := recentDirs[Index];
end;

function TLazPaintConfig.GetRecentFile(Index: Integer): string;
begin
  result := recentFiles[Index];
end;

function TLazPaintConfig.ImageListLastFolder: string;
begin
  result := iniOptions.ReadString('ImageList','LastFolder','');
end;

procedure TLazPaintConfig.SetImageListLastFolder(value: string);
begin
  iniOptions.WriteString('ImageList','LastFolder',ChompPathDelim(value));
end;

function TLazPaintConfig.ImageListAutoZoom: Boolean;
begin
  result := iniOptions.ReadBool('ImageList','AutoZoom',True);
end;

procedure TLazPaintConfig.SetImageListAutoZoom(value: Boolean);
begin
  iniOptions.WriteBool('ImageList','AutoZoom',Value);
end;

function TLazPaintConfig.ImageListAutoUncheck: Boolean;
begin
  result := iniOptions.ReadBool('ImageList','AutoUncheck',True);
end;

procedure TLazPaintConfig.SetImageListAutoUncheck(value: Boolean);
begin
  iniOptions.WriteBool('ImageList','AutoUncheck',Value);
end;

function TLazPaintConfig.ImageListAutoUncheckMode: Integer;
begin
  result := iniOptions.ReadInteger('ImageList','AutoUncheckMode',0);
end;

procedure TLazPaintConfig.SetImageListAutoUncheckMode(value: integer);
begin
  iniOptions.WriteInteger('ImageList','AutoUncheckMode',value);
end;


constructor TLazPaintConfig.Create(ini: TIniFile; AVersion: string);
begin
  FVersion:= AVersion;
  iniOptions := ini;
  InitRecentFiles;

  if ScreenSizeChanged then
  begin
    SetDefaultMainWindowPosition(EmptyRect);
    SetDefaultColorWindowPosition(EmptyRect);
    SetDefaultToolboxWindowPosition(EmptyRect);
    SetDefaultLayerWindowPosition(EmptyRect);
    SetDefaultImagelistWindowPosition(EmptyRect);
    SetDefaultBrowseWindowPosition(EmptyRect);
  end;

  tempFont := TFont.Create;
  Languages := TStringList.Create;

  InitColorizePresets;
end;

destructor TLazPaintConfig.Destroy;
begin
  FinalizeRecentFiles;
  FinalizeColorizePresets;
  iniOptions.Free;
  tempFont.Free;
  Languages.Free;
end;

initialization

  MenuDefinitionKeys := TStringList.Create;
  MenuDefinitionValues := TStringList.Create;

finalization

  MenuDefinitionKeys.Free;
  MenuDefinitionValues.Free;

end.
