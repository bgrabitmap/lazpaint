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
  UChooseColorInterface, UIconCache;

//sometimes LResources disappear in the uses clause

procedure RestartApplication;
var p: TProcess;
  exe: string;
begin
  p := TProcess.Create(nil);
  exe := Application.ExeName;
  p.Executable := exe;
  if CustomScriptDirectory <> '' then
  begin
    p.Parameters.Add('-scriptbasedir');
    p.Parameters.Add(CustomScriptDirectory);
  end;
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
  {$IFDEF DEBUG}
  I: Integer;
  Frames: PPointer;
  {$ENDIF}
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
  {$IFDEF DEBUG}
  Report += 'Stacktrace:' + LineEnding;
  Report := Report + '  ' + BackTraceStrFunc(ExceptAddr) + LineEnding;
  Frames := ExceptFrames;
  for I := 0 to ExceptFrameCount - 1 do
    Report := Report + '  ' + BackTraceStrFunc(Frames[I]) + LineEnding;
  {$ELSE}
  Report += 'To get more information, use the debug version.' + LineEnding;
  {$ENDIF}
  Report += LineEnding;
  if Initialized then
    Report += 'It is recommended to save a backup of your image and restart the application.'
  else
    Report += 'Application will now close.';
  ShowError(rsLazPaint, Report);
  if not Initialized then
    Halt; // End of program execution
end;

{$R *.res}

{$IFDEF DARWIN}{$IFDEF DEBUG}
const
  ConsoleOutputFile = '/dev/ttys000';
var
  OldOutput: TextFile;
  HasTerminalOutput: boolean;

  procedure InitOutput;
  var TerminalOutput: TextFile;
  begin
    //on MacOS, you need to open the terminal before running LazPaint to get debug information
    OldOutput := Output;
    AssignFile(TerminalOutput, ConsoleOutputFile);
    try
      Append(TerminalOutput);
      Output := TerminalOutput;
      HasTerminalOutput := true;
      Writeln;
      Writeln('Debug started');
    except
      HasTerminalOutput := false;
    end;
  end;

  procedure DoneOutput;
  begin
    if HasTerminalOutput then
    begin
      Writeln('Debug ended');
      CloseFile(Output);
      Output := OldOutput;
      HasTerminalOutput := false;
      SetHeapTraceOutput(ConsoleOutputFile);
    end;
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

