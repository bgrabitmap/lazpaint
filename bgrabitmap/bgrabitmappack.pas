{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit BGRABitmapPack;

interface

uses
  BGRAAnimatedGif, BGRABitmap, BGRABitmapTypes, BGRABlend, BGRACanvas, 
  BGRACanvas2D, BGRAColorInt, BGRACompressableBitmap, BGRACoordPool3D, 
  BGRADefaultBitmap, BGRADNetDeserial, BGRAFillInfo, BGRAFilters, 
  BGRAFreeType, BGRAGradients, BGRAGradientScanner, BGRALayers, BGRAMatrix3D, 
  BGRAOpenRaster, BGRAPaintNet, BGRAPath, BGRAPen, BGRAPhongTypes, 
  BGRAPolygon, BGRAPolygonAliased, BGRAResample, BGRAScene3D, 
  BGRASliceScaling, BGRASSE, BGRAStreamLayers, BGRAText, BGRATextFX, 
  BGRATransform, BGRATypewriter, BGRAVectorize, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('BGRABitmapPack', @Register);
end.
