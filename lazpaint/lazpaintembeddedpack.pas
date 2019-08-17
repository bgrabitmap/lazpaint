{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazPaintEmbeddedPack;

{$warn 5023 off : no warning about unused units}
interface

uses
  LazpaintInstance, LazpaintMainForm, LazPaintType, UAbout, UCanvassize, 
  UChooseColor, UClipboard, UColorintensity, UColorize, UCommandline, UConfig, 
  UCursors, UCustomblur, UEmboss, UGraph, UImage, UMac, UMotionBlur, 
  UMultiImage, UNewimage, UParse, URadialBlur, UResample, UShiftColors, UTool, 
  UToolbox, UTwirl, UImageState, UPixelate, UResourceStrings, UToolBasic, 
  UToolDeformationGrid, UToolFloodFill, UToolPolygon, UToolSelect, UToolText, 
  UBlendOp, UImageAction, UImageDiff, UImageObservation, ULayerstack, 
  ULoadImage, ULoading, UMenu, UObject3D, UPhongFilter, UScripting, USharpen, 
  UToolPhong, UTranslation, UVolatileScrollBar, UZoom, laztablet, 
  ugeometricbrush, UFilterFunction, UFormRain, unoisefilter, uposterize, 
  uadjustcurves, UToolBrush, UToolIcon, UToolLayer, UImageType, ULayerAction, 
  UStateType, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('LazPaintEmbeddedPack', @Register);
end.
