unit utest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type
  TTest = class
  public
    Name: string;
    procedure OnPaint(Canvas: TCanvas; Width,Height: Integer); virtual; abstract;
    procedure OnTimer(Canvas: TCanvas; Width,Height: Integer; ElapsedSec: Double); virtual; abstract;
  end;

implementation

end.

