{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazpaintcontrols;

interface

uses
  LCToolbars, LCVectorialFill, LCVectorialFillInterface, LCVectorOriginal, 
  LCVectorPolyShapes, LCVectorRectShapes, LCVectorialFillControl, 
  LCVectorShapes, LCVectorTextShapes, LCScaleDPI, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('LCVectorialFillControl', @LCVectorialFillControl.Register);
end;

initialization
  RegisterPackage('lazpaintcontrols', @Register);
end.
