program lazpaint;

{$mode objfpc}{$H+}

{$DEFINE UseCThreads}

uses
  {$IFDEF UNIX}cwstring, {$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,

  process, Forms, FileUtil, SysUtils, Inifiles, BGRAUTF8, LazFileUtils, printer4lazarus, //packages

  LazPaintType, LazpaintInstance, LazpaintMainForm, UConfig, UOnline,

  UToolbox, UChooseColor, ULayerstack,  //tool windows
  UMac, LCScaleDPI, UVolatileScrollBar, UCursors, UTranslation, //interface

  UGraph, UImage, UStateType, UImageState, UClipboard,
  UCommandline, UParse, UZoom, UResourceStrings, UImageObservation,
  UFilterConnector, UFilters, UImageAction, ULoadImage, UImageDiff,
  UFilterThread,

  //forms
  UNewimage, UMultiImage, UBrowseImages, UBlendOp, UCanvassize, UResample, UObject3D,
  URadialBlur, UMotionBlur, UCustomblur, UEmboss, UTwirl, UWaveDisplacement, UPixelate,
  UColorintensity, UShiftColors, UColorize, USharpen,
  UPhongFilter, UFilterFunction,
  UAbout, ULoading,

  //tools
  UTool, UToolVectorial, UToolDeformationGrid, UToolSelect, UToolPolygon, UToolFloodFill, UToolBasic,
  UToolPhong, UToolText, UToolBrush, UToolIcon, UToolLayer,

  UScripting, UMenu, UColorFilters, uadjustcurves,
  UScriptType, ULayerAction, UImageType, uposterize, UMySLV,
  unoisefilter, uprint, uimagelist, UFileExtensions, UFileSystem,
  UMainFormLayout, USaveOption, UBrushType, ugeometricbrush,
  URainType, UFormRain, UPaletteToolbar, uselectionhighlight,
  UImagePreview, UPreviewDialog, UQuestion, UTiff, UImageView,
  UDarkTheme, URaw, UProcessAuto, UPython, UImageBackup, ULayerStackInterface,
  UChooseColorInterface;

//sometimes LResources disappear in the uses clause

procedure RestartApplication;
var p: TProcess;
  exe: string;
begin
  p := TProcess.Create(nil);
  exe := Application.ExeName;
  p.Executable := exe;
  p.Options := [];
  p.Execute;
  p.Free;
end;

type

  { TMyLazPaintInstance }

  TMyLazPaintInstance = class(TLazPaintInstance)
    FMyOnlineUpdater: TLazPaintOnlineUpdater;
    function GetOnlineUpdater: TLazPaintCustomOnlineUpdater; override;
    constructor Create; override;
    constructor Create(AEmbedded: boolean); override;
    destructor Destroy; override;
  private
    procedure ApplicationException(Sender: TObject; E: Exception);
  end;

var
  LazpaintApplication: TMyLazPaintInstance;
  ActualConfig: TIniFile;
  RestartQuery: boolean;

{ TMyLazPaintInstance }

function TMyLazPaintInstance.GetOnlineUpdater: TLazPaintCustomOnlineUpdater;
var lastCheck: TDateTime;
begin
  try
    lastCheck := Config.GetLastUpdateCheck;
    if (lastCheck = 0) or (Now-lastCheck > 30) then
    begin
      if not Assigned(FMyOnlineUpdater) then
      begin
        FMyOnlineUpdater := TLazPaintOnlineUpdater.Create(Config);
        Config.SetLastUpdateCheck(Now);
      end;
    end;
    Result:= FMyOnlineUpdater;
  except
    on ex:exception do
      result := nil;
  end;
end;

constructor TMyLazPaintInstance.Create;
begin
  inherited Create;
  Application.OnException:=@ApplicationException;
end;

constructor TMyLazPaintInstance.Create(AEmbedded: boolean);
begin
  inherited Create(AEmbedded);
  Application.OnException:=@ApplicationException;
end;

destructor TMyLazPaintInstance.Destroy;
begin
  if Application.OnException = @ApplicationException then
    Application.OnException:= nil;
  inherited Destroy;
end;

procedure TMyLazPaintInstance.ApplicationException(Sender: TObject; E: Exception);
var
  I: Integer;
  Frames: PPointer;
  Report: string;
begin
  if Initialized then
    Report := 'Unhandled exception!' + LineEnding
  else
    Report := 'Error initializing application!' + LineEnding;
  Report += LineEnding;
  if E <> nil then
  begin
    if E.ClassName <> 'Exception' then
      Report += 'Exception class: ' + E.ClassName + LineEnding;
    Report += 'Message: ' + E.Message + LineEnding;
  end;
  Report += 'Stacktrace:' + LineEnding;
  Report := Report + '  ' + BackTraceStrFunc(ExceptAddr) + LineEnding;
  Frames := ExceptFrames;
  for I := 0 to ExceptFrameCount - 1 do
    Report := Report + '  ' + BackTraceStrFunc(Frames[I]) + LineEnding;
  Report += LineEnding;
  if Initialized then
    Report += 'It is recommanded to save a backup and restart the application.'
  else
    Report += 'Application will now close.';
  ShowError(rsLazPaint, Report);
  if not Initialized then
    Halt; // End of program execution
end;

{$R *.res}

{$IFDEF DARWIN}{$IFDEF DEBUG}
var
  OldOutput: TextFile;

  procedure InitOutput;
  begin
    OldOutput := Output;
    AssignFile(Output, '/dev/ttys000');
    Append(Output);
    Writeln;
    Writeln('Debug started');
  end;

  procedure DoneOutput;
  begin
    Writeln('Debug ended');
    CloseFile(Output);
    Output := OldOutput;
  end;
{$ENDIF}{$ENDIF}

begin
  {$IFDEF DARWIN}{$IFDEF DEBUG}InitOutput;{$ENDIF}{$ENDIF}

  ActualConfig := GetActualConfig;
  TranslateLazPaint(ActualConfig);

  Application.Title:='LazPaint';
  Application.Initialize;
  UGraph.NicePointMaxRadius:= DoScaleX(UGraph.NicePointMaxRadius, OriginalDPI);
  UGraph.FrameDashLength:= DoScaleX(UGraph.FrameDashLength, OriginalDPI);

  LazpaintApplication := TMyLazPaintInstance.Create;
  LazpaintApplication.UseConfig(ActualConfig);
  FillLanguageList(LazpaintApplication.Config);

  {$IFDEF WINDOWS}
    LazpaintApplication.AboutText := ReadFileToString(SysToUTF8(ExtractFilePath(Application.ExeName))+'readme.txt');
  {$ELSE}
    LazpaintApplication.AboutText := ReadFileToString('readme.txt');
  {$ENDIF}

  if not LazpaintApplication.ProcessCommandLine then
  begin
    LazpaintApplication.Show;
    Application.Run;
  end;
  LazpaintApplication.Hide;
  Application.ProcessMessages;

  RestartQuery := LazpaintApplication.RestartQuery;
  LazpaintApplication.Free;
  if RestartQuery then RestartApplication;

  {$IFDEF DARWIN}{$IFDEF DEBUG}DoneOutput;{$ENDIF}{$ENDIF}
end.

