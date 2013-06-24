program lazpaint;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cwstring, {$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,

  Forms, SysUtils, Inifiles, FileUtil,
  BGRABitmapPack, lnetvisual, //packages

  LazPaintType, LazpaintInstance, LazpaintMainForm, UConfig, UOnline,

  UToolbox, UChooseColor, ULayerstack,  //tool windows
  UMac, UScaleDPI, UVolatileScrollBar, UCursors, UTranslation, //interface

  UGraph, UTool, UImage, UStateType, UImageState, UClipboard,
  UCommandline, UParse, UZoom, UResourceStrings, UImageObservation,
  UFilterConnector, UFilters, UImageAction, ULoadImage, UImageDiff,
  UFilterThread,

  //forms
  UNewimage, UMultiImage, UBlendOp, UCanvassize, UResample, UObject3D,
  URadialBlur, UMotionBlur, UCustomblur, UEmboss, UTwirl, UPixelate,
  UColorintensity, UShiftColors, UColorize, USharpen,
  UPhongFilter, UFilterFunction,
  UAbout, ULoading,

  //tools
  UToolDeformationGrid, UToolSelect, UToolPolygon,
  UToolFloodFill, UToolBasic, UToolPhong, UToolText;

//sometimes LResources disappear in the uses clause

procedure RestartApplication;
begin
  SysUtils.ExecuteProcess(Application.ExeName, '', []);
end;

var
  LazpaintApplication: TLazPaintInstance;
  ActualConfig: TIniFile;
  RestartQuery: boolean;

{$R *.res}

begin
  ActualConfig := GetActualConfig;
  TranslateLazPaint(ActualConfig);

  Application.Title := 'LazPaint';
  Application.Initialize;

  LazpaintApplication := TLazPaintInstance.Create;
  LazpaintApplication.UseConfig(ActualConfig);
  FillLanguageList(LazpaintApplication.Config);

  {$IFDEF WINDOWS}
    LazpaintApplication.AboutText := ReadFileToString(ExtractFilePath(Application.ExeName)+'readme.txt');
  {$ELSE}
    LazpaintApplication.AboutText := ReadFileToString('readme.txt');
  {$ENDIF}

  if not LazpaintApplication.ProcessCommandLine then
  begin
    LazpaintApplication.Show;
    Application.Run;
  end;

  RestartQuery := LazpaintApplication.RestartQuery;
  LazpaintApplication.Free;
  if RestartQuery then RestartApplication;
end.

