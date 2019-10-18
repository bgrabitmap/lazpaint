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
  UDarkTheme;

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

