unit laztablet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  {$IFDEF WINDOWS}
    laztabletwin
  {$ELSE}
    laztabletother
  {$ENDIF};

type
  TLazTablet = class(TCustomLazTablet)
  public
    property Coords;
  published
    property Min;
    property Max;
    property Pressure;
    property Entering;
    property Present;
  end;

implementation

end.

