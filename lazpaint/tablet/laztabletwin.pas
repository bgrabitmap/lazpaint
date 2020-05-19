unit laztabletwin;

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
  Classes, SysUtils, Tablet, WintabConsts;

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
    FTablet: TTablet;
  protected
    procedure TabletPacket(Sender: TObject; {%H-}PacketNumber, {%H-}ContextHandle: HCtx;
      pPacket: Pointer);
    procedure TabletProximity(Sender: TObject; {%H-}ContextHandle: HCtx;
      Entering: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    property Coords: TPoint read FCoords;
  published
    property Min: integer read FMin;
    property Max: integer read FMax;
    property Pressure: integer read FPressure;
    property Entering: boolean read FEntering;
    property Present: boolean read FPresent;
  end;

  TMyPacket = record
    Cursor: integer;
    Buttons: integer;
    Coords: TPoint;
    z: longint;
    Pressure: integer;
    Orientation: TOrientation;
    //    Rotation: TRotation;
  end;
  PMyPacket = ^TMyPacket;

const
  lcPktData = PK_CURSOR or PK_BUTTONS or PK_X or PK_Y or PK_Z or PK_NORMAL_PRESSURE or
    //  PK_ROTATION;
    PK_ORIENTATION;
  lcOptions = CXO_SYSTEM or CXO_MESSAGES or CXO_CSRMESSAGES;
  lcBtnMask = SBN_LCLICK or SBN_RCLICK or SBN_LDBLCLICK or SBN_RDBLCLICK;

implementation

{ TCustomLazTablet }

procedure TCustomLazTablet.TabletPacket(Sender: TObject;
  PacketNumber, ContextHandle: HCtx; pPacket: Pointer);
begin
  with TMyPacket(pPacket^) do
  begin
    FPressure := Pressure;
    FCoords := Coords;
  end;
end;

procedure TCustomLazTablet.TabletProximity(Sender: TObject;
  ContextHandle: HCtx; Entering: boolean);
begin
  FEntering := Entering;
end;

constructor TCustomLazTablet.Create(AOwner: TComponent);
var
  AContext: TLogContext;
begin
  inherited Create(AOwner);
  FTablet := nil;
  FTablet := TTablet.Create(Self);

  FTablet.OnPacket := @TabletPacket;
  FTablet.OnProximity := @TabletProximity;

  with FTablet, AContext do
  begin
    FPresent := Present;
    if not Present then
    begin
      FMin := 0;
      FMax := 0;
      FPressure := 0;
      FEntering := False;
      FCoords := Point(0, 0);
      Exit;
    end;
    DefaultContext({%H-}AContext);
    Options := Options or lcOptions;
    PktData := lcPktData;
    BtnDnMask := lcBtnMask;
    BtnUpMask := BtnDnMask;
    MsgBase := WT_DEFBASE;
    PacketSize := SizeOf(TMyPacket);
    Enabled := True;
    Open(AContext);

    with FTablet.Pressure do
    begin
      FMin := Min;
      FMax := Max;
    end;
  end;
end;

destructor TCustomLazTablet.Destroy;
begin
  if Assigned(FTablet) then FTablet.Close;
  inherited Destroy;
end;

end.

