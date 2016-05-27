program lazpaint;

{$mode objfpc}{$H+}

{$DEFINE UseCThreads}

uses
  {$IFDEF UNIX}cwstring, {$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,

  process, Forms, SysUtils, Inifiles, FileUtil, printer4lazarus, //packages

  LazPaintType, LazpaintInstance, LazpaintMainForm, UConfig, UOnline,

  UToolbox, UChooseColor, ULayerstack,  //tool windows
  UMac, UScaleDPI, UVolatileScrollBar, UCursors, UTranslation, //interface

  UGraph, UTool, UImage, UStateType, UImageState, UClipboard,
  UCommandline, UParse, UZoom, UResourceStrings, UImageObservation,
  UFilterConnector, UFilters, UImageAction, ULoadImage, UImageDiff,
  UFilterThread,

  //forms
  UNewimage, UMultiImage, UBrowseImages, UBlendOp, UCanvassize, UResample, UObject3D,
  URadialBlur, UMotionBlur, UCustomblur, UEmboss, UTwirl, UPixelate,
  UColorintensity, UShiftColors, UColorize, USharpen,
  UPhongFilter, UFilterFunction,
  UAbout, ULoading,

  //tools
  UToolDeformationGrid, UToolSelect, UToolPolygon, UToolFloodFill, UToolBasic,
  UToolPhong, UToolText, UScripting, UMenu, UColorFilters, uadjustcurves,
  UScriptType, ULayerAction, UImageType, uposterize, UMySLV, UToolLayer,
  unoisefilter, uprint, uimagelist, UBarUpDown, UFileExtensions, UFileSystem, UToolBrush, UMainFormLayout, USaveOption, UBrushType, 
  ugeometricbrush, URainType, UFormRain, UPaletteToolbar;

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

begin
  ActualConfig := GetActualConfig;
  TranslateLazPaint(ActualConfig);

  Application.Title := 'LazPaint';
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
end.

