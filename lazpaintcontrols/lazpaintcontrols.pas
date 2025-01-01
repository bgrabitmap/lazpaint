{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazpaintcontrols;

{$warn 5023 off : no warning about unused units}
interface

uses
  LCToolbars, LCVectorialFill, LCVectorialFillInterface, LCVectorOriginal, 
  LCVectorPolyShapes, LCVectorRectShapes, LCVectorialFillControl, 
  LCVectorShapes, LCVectorTextShapes, LCScaleDPI, LCVectorClipboard, 
  LCResourceString, LCVectorMultishape, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('LCVectorialFillControl', @LCVectorialFillControl.Register);
end;

initialization
  RegisterPackage('lazpaintcontrols', @Register);
end.
