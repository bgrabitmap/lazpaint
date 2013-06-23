unit BGRAPhongTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmapTypes;

type TCustomPhongShading = class

   LightPosition : TPoint;

   { Render the specified map on the destination bitmap with one solid color. Map altitude
     indicate the global height of the map. }
   procedure Draw(dest: TBGRACustomBitmap; map: TBGRACustomBitmap; mapAltitude: integer; ofsX,ofsY: integer;
                  Color : TBGRAPixel); virtual; abstract;

   { Render with a color map of the same size as the height map. Map altitude
     indicate the global height of the map. }
   procedure Draw(dest: TBGRACustomBitmap; map: TBGRACustomBitmap; mapAltitude: integer; ofsX,ofsY: integer;
                  ColorMap : TBGRACustomBitmap); virtual; abstract;

   { Render with a scanner. Map altitude
     indicate the global height of the map. }
   procedure DrawScan(dest: TBGRACustomBitmap; map: TBGRACustomBitmap; mapAltitude: integer; ofsX,ofsY: integer;
                  ColorScan : IBGRAScanner); virtual; abstract;

 end;

implementation

end.

