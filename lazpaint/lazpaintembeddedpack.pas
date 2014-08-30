{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazPaintEmbeddedPack;

interface

uses
  LazpaintInstance, LazpaintMainForm, LazPaintType, UAbout, UCanvassize, 
  UChooseColor, UClipboard, UColorintensity, UColorize, UCommandline, UConfig, 
  UCursors, UCustomblur, UEmboss, UGraph, UImage, UMac, UMotionBlur, 
  UMultiImage, UNewimage, UParse, URadialBlur, UResample, UShiftColors, UTool, 
  UToolbox, UTwirl, UImageState, UPixelate, UResourceStrings, UScaleDPI, 
  UToolBasic, UToolDeformationGrid, UToolFloodFill, UToolPolygon, UToolSelect, 
  UToolText, UBlendOp, UImageAction, UImageDiff, UImageObservation, 
  ULayerstack, ULoadImage, ULoading, UMenu, UObject3D, UPhongFilter, 
  uscripting, USharpen, UToolPhong, UTranslation, UVolatileScrollBar, UZoom, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('LazPaintEmbeddedPack', @Register);
end.
