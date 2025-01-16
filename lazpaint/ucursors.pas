// SPDX-License-Identifier: GPL-3.0-only
unit UCursors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function crCustomColorPicker : integer;
function crCustomCrosshair : integer;
function crCustomFloodfill : integer;
function crCustomRotate : integer;

implementation

uses Graphics, Forms, LResources, Controls, LCLType,
  LCScaleDPI, BGRABitmap, BGRABitmapTypes, BGRAIconCursor;

var
  useCustomCursors: boolean = true;
  customCursorCount: integer;
  crCustomColorPickerValue,
  crCustomCrosshairValue,
  crCustomFloodfillValue,
  crCustomRotateValue: integer;

procedure GetCursorScaledSize(ACursor: TBGRAIconCursor; out AWidth, AHeight: integer);
var minWidth, minIndex, i: integer;
begin
  minIndex := -1;
  minWidth := MaxInt;
  for i := 0 to ACursor.Count - 1 do
  begin
    if ACursor.Width[i] < minWidth then
    begin
      minWidth := ACursor.Width[i];
      minIndex := i;
    end;
  end;
  if minIndex = -1 then
  begin
    AWidth := 0;
    AHeight := 0;
  end else
  begin
    AWidth := DoScaleX(minWidth, 96);
    AHeight := DoScaleY(ACursor.Height[minIndex], 96);
  end;
end;

function GetCursor(var AIndex: integer; AName: string; ADefault: integer): integer;
var
  icon, resampledIcon: TBGRAIconCursor;
  bmpIcon, resampledBmpIcon: TBGRABitmap;
  str: TStream;
  cursorImage: TCursorImage;
  wantedWidth, wantedHeight, bmpIndex: integer;
  scaledHotspot: TPoint;
begin
  if not useCustomCursors then exit(ADefault);
  if AIndex = 0 then
  begin
    AIndex := customCursorCount+1;
    if (ScreenInfo.PixelsPerInchX > 120) or
      (ScreenInfo.PixelsPerInchY > 120) then
    begin
      icon := TBGRAIconCursor.Create;
      try
        icon.LoadFromResource(AName);
        GetCursorScaledSize(icon, wantedWidth, wantedHeight);
        bmpIndex := icon.GetBestFitIndex(wantedWidth, wantedHeight);
        if (wantedWidth - icon.Width[bmpIndex]) / icon.Width[bmpIndex] <= 1.3 then
        begin
          screen.Cursors[AIndex] := LoadCursorFromLazarusResource(AName);
        end else
        begin
          bmpIcon := icon.GetBitmap(bmpIndex) as TBGRABitmap;
          try
            bmpIcon.ResampleFilter:= rfLanczos3;
            resampledBmpIcon := bmpIcon.Resample(wantedWidth, wantedHeight);
            try
              resampledIcon := TBGRAIconCursor.Create;
              try
                resampledIcon.FileType:= ifCur;
                resampledIcon.Add(resampledBmpIcon, icon.BitDepth[bmpIndex]);
                scaledHotspot :=
                  Point(round(bmpIcon.HotSpot.X * wantedWidth / bmpIcon.Width),
                  round(bmpIcon.HotSpot.Y * wantedHeight / bmpIcon.Height));
                resampledIcon.HotSpot[0] := scaledHotspot;
                str := TMemoryStream.Create;
                try
                  resampledIcon.SaveToStream(str);
                  str.Position:= 0;
                  cursorImage := TCursorImage.Create;
                  try
                    cursorImage.LoadFromStream(str);
                    screen.Cursors[AIndex] := cursorImage.ReleaseHandle;
                  finally
                    cursorImage.Free;
                  end;
                finally
                  str.Free;
                end;
              finally
                resampledIcon.Free;
              end;
            finally
              resampledBmpIcon.Free;
            end;
          finally
            bmpIcon.Free;
          end;
        end;
      finally
        icon.Free;
      end;
    end else
    begin
      screen.Cursors[AIndex] := LoadCursorFromLazarusResource(AName);
    end;
    inc(customCursorCount);
  end;
  result := AIndex;
end;

function crCustomColorPicker: integer;
begin
  result := GetCursor(crCustomColorPickerValue, 'colorpicker', crHandPoint);
end;

function crCustomCrosshair: integer;
begin
  result := GetCursor(crCustomCrosshairValue, 'crosshair', crCross);
end;

function crCustomFloodfill: integer;
begin
  result := GetCursor(crCustomFloodfillValue, 'floodfill', crDefault);
end;

function crCustomRotate: integer;
begin
  result := GetCursor(crCustomRotateValue, 'rotate', crSizeAll);
end;

initialization

  {$I tools\cursors\paintcursors.lrs}

end.

