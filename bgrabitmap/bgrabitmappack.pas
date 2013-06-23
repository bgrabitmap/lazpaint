{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit BGRABitmapPack;

interface

uses
  BGRAAnimatedGif, BGRABitmap, BGRABitmapTypes, BGRABlend, 
  BGRACompressableBitmap, BGRADefaultBitmap, BGRADNetDeserial, BGRAFilters, 
  BGRAPaintNet, BGRAPolygon, BGRAResample, BGRAPen, BGRATransform, 
  BGRAGradientScanner, BGRAText, BGRAPolygonAliased, BGRACanvas, BGRAFillInfo, 
  BGRAPath, BGRACanvas2D, BGRAScene3D, BGRATextFX, BGRAPhongTypes, BGRALayers, 
  BGRASSE, BGRAMatrix3D, BGRAColorInt, BGRACoordPool3D, BGRAOpenRaster, 
  BGRAFreeType, BGRAGradients, BGRASliceScaling, BGRAVectorize, 
  BGRATypewriter, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('BGRABitmapPack', @Register);
end.
