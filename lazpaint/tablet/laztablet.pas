unit laztablet;

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

