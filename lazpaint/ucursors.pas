// SPDX-License-Identifier: GPL-3.0-only
unit UCursors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

var
   crCustomColorPicker : integer = 1;
   crCustomCrosshair : integer = 2;
   crCustomFloodfill : integer = 3;
   crCustomRotate : integer = 4;

implementation

uses Graphics, Forms, LResources, Controls, LCLType;

var useCustomCursors: boolean = true;

initialization

  {$I tools\cursors\paintcursors.lrs}

  if useCustomCursors then
  begin
    screen.Cursors[crCustomColorPicker] := LoadCursorFromLazarusResource('colorpicker');
    screen.Cursors[crCustomCrossHair] := LoadCursorFromLazarusResource('crosshair');
    screen.Cursors[crCustomFloodfill] := LoadCursorFromLazarusResource('floodfill');
    screen.Cursors[crCustomRotate] := LoadCursorFromLazarusResource('rotate');
  end else
  begin
    crCustomCrossHair := crCross;
    crCustomColorPicker := crHandPoint;
    crCustomFloodfill := crDefault;
    crCustomRotate := crSizeAll;
  end;
end.

