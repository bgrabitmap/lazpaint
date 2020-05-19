unit laztabletother;

{$mode objfpc}{$H+}

{********************************************}
{  This unit is a part of TTablet component  }
{  Copyright (C) 2001-2002 Mattias Andersson }
{  See COPYING.txt for license information   }
{  Last modified: 2003-10-25; version 1.24   }
{********************************************}
{  Modified by Nelson Chu in 2013 to work    }
{  with Lazarus.                             }
{********************************************}

interface

uses
  Classes, SysUtils;

type

  { TCustomLazTablet }

  TCustomLazTablet = class(TComponent)
  private
    FCoords: TPoint;
    FEntering: boolean;
    FMax: integer;
    FMin: integer;
    FPresent: boolean;
    FPressure: integer;
  public
    constructor Create(AOwner: TComponent); override;
  public
    property Coords: TPoint read FCoords;
  published
    property Min: integer read FMin;
    property Max: integer read FMax;
    property Pressure: integer read FPressure;
    property Entering: boolean read FEntering;
    property Present: boolean read FPresent;
  end;

implementation

{ TCustomLazTablet }

constructor TCustomLazTablet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPresent := False;
  FMin := 0;
  FMax := 0;
  FPressure := 0;
  FEntering := False;
  FCoords := Point(0, 0);
end;

end.

