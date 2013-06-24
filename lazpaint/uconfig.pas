unit UConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, BGRABitmapTypes, Graphics, LCLType;

type
  { TLazPaintConfig }

  TLazPaintConfig = class
  private
    iniOptions: TIniFile;
    recentFiles: TStringList;
    tempFont: TFont;
    function GetRecentFile(Index: Integer): string;
    function GetRecentFilesCount: integer;

  public
    Languages: TStringList;
    constructor Create(ini: TIniFile);
    destructor Destroy; override;

    procedure InitRecentFiles;
    procedure AddRecentFile(filename: string);
    procedure FinalizeRecentFiles;

    function DefaultLangage: string;
    procedure SetDefaultLangage(value: string);

    function LatestVersion: string;
    procedure SetLatestVersion(value: string);
    procedure GetUpdatedLanguages(AList: TStringList);
    procedure SetUpdatedLanguages(AList: TStringList);
    procedure AddUpdatedLanguage(ALang: string);

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
    function DefaultColorWindowVisible: boolean;
    procedure SetDefaultColorWindowVisible(value: boolean);
    function DefaultLayerWindowVisible: boolean;
    procedure SetDefaultLayerWindowVisible(value: boolean);
    function DefaultToolboxWindowVisible: boolean;
    procedure SetDefaultToolboxWindowVisible(value: boolean);
    function DefaultGridVisible: boolean;
    procedure SetDefaultGridVisible(value: boolean);

    //tools
    function DefaultToolForeColor: TBGRAPixel;
    function DefaultToolBackColor: TBGRAPixel;
    procedure SetDefaultToolForeColor(value: TBGRAPixel);
    procedure SetDefaultToolBackColor(value: TBGRAPixel);
    function DefaultToolPenWidth: single;
    procedure SetDefaultToolPenWidth(value: single);
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
    function DefaultBlurRadius: integer;
    procedure SetDefaultBlurRadius(value: integer);
    function DefaultPixelateSize: integer;
    procedure SetDefaultPixelateSize(value: integer);
    function DefaultPixelateQuality: string;
    procedure SetDefaultPixelateQuality(value: string);
    function DefaultSharpenAmount: single;
    procedure SetDefaultSharpenAmount(value: single);

    //motion blur config
    function DefaultBlurMotionAngle: double;
    function DefaultBlurMotionDistance: integer;
    function DefaultBlurMotionOriented: boolean;
    procedure SetDefaultBlurMotionAngle(value: double);
    procedure SetDefaultBlurMotionDistance(value: integer);
    procedure SetDefaultBlurMotionOriented(value: boolean);

    //custom blur config
    function DefaultCustomBlurMask: string;
    procedure SetDefaultCustomBlurMask(value: string);

    //emboss config
    function DefaultEmbossAngle: double;
    procedure SetDefaultEmbossAngle(value: double);

    //twirl config
    function DefaultTwirlRadius: double;
    procedure SetDefaultTwirlRadius(value: double);
    function DefaultTwirlTurn: double;
    procedure SetDefaultTwirlTurn(value: double);

    //phong filter
    function DefaultPhongFilterAltitude: integer;
    procedure SetDefaultPhongFilterAltitude(value: integer);

    //color config
    function DefaultUseGSBA: boolean;
    procedure SetDefaultUseGSBA(value: boolean);

    property RecentFilesCount : integer read GetRecentFilesCount;
    property RecentFile[Index: Integer]: string read GetRecentFile;
  end;

function GetActualConfig: TIniFile;

var ActualConfigDir : string;

implementation

uses forms, uparse, LCLProc;

const maxRecentFiles = 10;

//returns the config file to use
function GetActualConfig: TIniFile;
var
  PortableConfig: TIniFile;
  PortableConfigFilename: string;
  ActualConfigFilename: string;
  {$IFDEF DARWIN}
  ConfigPath: string;
  {$ENDIF}
begin
  ActualConfigFilename := '';

  //check if a config file path is defined
  PortableConfigFilename := ExtractFilePath(Application.ExeName)+'lazpaint.ini';
  If FileExists(PortableConfigFilename) then
  begin
    PortableConfig := TIniFile.Create(PortableConfigFilename);
    ActualConfigFilename:= PortableConfig.ReadString('General','ConfigFile','');
    if ActualConfigFilename <> '' then
      ActualConfigFilename:= ExpandFileName(ExtractFilePath(Application.ExeName)+ActualConfigFilename);
    PortableConfig.Free;
  end;

  //otherwise, use default path
  if ActualConfigFilename = '' then
  begin
    CreateDir(GetAppConfigDir(False));
    ActualConfigFilename := GetAppConfigFile(False,False);
  end;

  {$IFDEF DARWIN}
  ConfigPath := ExtractFilePath(ActualConfigFilename);
  CreateDir(ConfigPath);
  {$ENDIF}
  result := TIniFile.Create(ActualConfigFilename,True);
  ActualConfigDir := ExtractFilePath(ActualConfigFilename);
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

procedure TLazPaintConfig.SetDefaultToolboxWindowVisible(value: boolean);
begin
  iniOptions.WriteBool('Window','ToolboxWindowVisible',value);
end;

function TLazPaintConfig.DefaultGridVisible: boolean;
begin
  result := iniOptions.ReadBool('General','GridVisible',true);
end;

procedure TLazPaintConfig.SetDefaultGridVisible(value: boolean);
begin
  iniOptions.WriteBool('General','GridVisible',value);
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

function TLazPaintConfig.DefaultBlurRadius: integer;
begin
  result := iniOptions.ReadInteger('Filter','BlurRadius',5);
end;

procedure TLazPaintConfig.SetDefaultBlurRadius(value: integer);
begin
  iniOptions.WriteInteger('Filter','BlurRadius',value);
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

function TLazPaintConfig.DefaultBlurMotionDistance: integer;
begin
  result := iniOptions.ReadInteger('Filter','MotionBlurDistance',5);
end;

function TLazPaintConfig.DefaultBlurMotionOriented: boolean;
begin
  result := iniOptions.ReadBool('Filter','MotionBlurOriented',true);
end;

procedure TLazPaintConfig.SetDefaultBlurMotionAngle(value: double);
begin
  iniOptions.WriteFloat('Filter','MotionBlurAngle',value);
end;

procedure TLazPaintConfig.SetDefaultBlurMotionDistance(value: integer);
begin
  iniOptions.WriteInteger('Filter','MotionBlurDistance',value);
end;

procedure TLazPaintConfig.SetDefaultBlurMotionOriented(value: boolean);
begin
  iniOptions.WriteBool('Filter','MotionBlurOriented',value);
end;

function TLazPaintConfig.DefaultCustomBlurMask: string;
begin
  result := iniOptions.ReadString('Filter','CustomBlurMask','');
end;

procedure TLazPaintConfig.SetDefaultCustomBlurMask(value: string);
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
   if not FileExists(recentFiles[i]) then recentFiles.Delete(i);
end;

procedure TLazPaintConfig.FinalizeRecentFiles;
var i: integer;
begin
  iniOptions.EraseSection('RecentFiles');
  for i := 0 to recentFiles.Count-1 do
    iniOptions.WriteString('RecentFiles','File'+inttostr(I+1),recentFiles[i]);
  recentFiles.Free;
end;

function TLazPaintConfig.DefaultLangage: string;
begin
  result := iniOptions.ReadString('General','Language','');
  if result = '' then result := 'auto';
end;

procedure TLazPaintConfig.SetDefaultLangage(value: string);
begin
  if value = 'auto' then value := '';
  iniOptions.WriteString('General','Language',value);
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
  AList.CommaText := iniOptions.ReadString('General','UpdatedLanguages','');
end;

procedure TLazPaintConfig.SetUpdatedLanguages(AList: TStringList);
begin
  iniOptions.WriteString('General','UpdatedLanguages',AList.CommaText)
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

function TLazPaintConfig.GetRecentFilesCount: integer;
begin
  result := recentFiles.Count;
end;

function TLazPaintConfig.GetRecentFile(Index: Integer): string;
begin
  result := recentFiles[Index];
end;

constructor TLazPaintConfig.Create(ini: TIniFile);
begin
  iniOptions := ini;
  InitRecentFiles;

  if ScreenSizeChanged then
  begin
    SetDefaultMainWindowPosition(EmptyRect);
    SetDefaultColorWindowPosition(EmptyRect);
    SetDefaultToolboxWindowPosition(EmptyRect);
    SetDefaultLayerWindowPosition(EmptyRect);
  end;

  tempFont := TFont.Create;
  Languages := TStringList.Create;
end;

destructor TLazPaintConfig.Destroy;
begin
  FinalizeRecentFiles;
  iniOptions.Free;
  tempFont.Free;
  Languages.Free;
end;

end.
