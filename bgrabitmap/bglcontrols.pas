{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit BGLControls;

interface

uses
  BGLVirtualScreen, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('BGLVirtualScreen', @BGLVirtualScreen.Register);
end;

initialization
  RegisterPackage('BGLControls', @Register);
end.
