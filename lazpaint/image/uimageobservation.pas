// SPDX-License-Identifier: GPL-3.0-only
unit UImageObservation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TLazPaintImageObservationEvent = record
    Sender: TObject;
    DelayedStackUpdate: boolean;
  end;

  TLazPaintImageObservationHandler = procedure(AEvent: TLazPaintImageObservationEvent) of object;

  { TLazPaintImageObservable }

  TLazPaintImageObservable = class
  private
    FObservers: array of TLazPaintImageObservationHandler;
    FObserverCount: integer;
    FObservable: TObject;
  public
    DelayedStackUpdate: boolean;
    constructor Create(AObservable: TObject);
    destructor Destroy; override;
    procedure AddObserver(AObservationHandler: TLazPaintImageObservationHandler);
    procedure RemoveObserver(AObservationHandler: TLazPaintImageObservationHandler);
    procedure NotifyObservers;
  end;

implementation

{ TLazPaintImageObservable }

constructor TLazPaintImageObservable.Create(AObservable: TObject);
begin
  FObserverCount := 0;
  FObservable:= AObservable;
end;

destructor TLazPaintImageObservable.Destroy;
begin
  inherited Destroy;
end;

procedure TLazPaintImageObservable.AddObserver(
  AObservationHandler: TLazPaintImageObservationHandler);
begin
  if length(FObservers) = FObserverCount then
    setLength(FObservers,length(FObservers)*2+1);
  FObservers[FObserverCount] := AObservationHandler;
  inc(FObserverCount);
end;

procedure TLazPaintImageObservable.RemoveObserver(
  AObservationHandler: TLazPaintImageObservationHandler);
var
  i,j: Integer;
begin
  for i := 0 to FObserverCount-1 do
    if FObservers[i] = AObservationHandler then
     begin
       for j := i to FObserverCount-2 do
         FObservers[j] := FObservers[j+1];
       dec(FObserverCount);
       exit;
     end;
end;

procedure TLazPaintImageObservable.NotifyObservers;
var
  i: Integer;
  event: TLazPaintImageObservationEvent;
begin
  event.Sender := FObservable;
  event.DelayedStackUpdate := DelayedStackUpdate;
  for i := 0 to FObserverCount-1 do
    FObservers[i](event);
end;

end.

