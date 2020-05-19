// SPDX-License-Identifier: GPL-3.0-only
unit LCVectorShapes;

{$mode objfpc}{$H+}

interface

uses
  LCVectorRectShapes, LCVectorPolyShapes, LCVectorTextShapes;

implementation

procedure RegisterShapes;
begin
  //done in used units
end;

initialization

  RegisterShapes;

end.

