unit UFilterThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRAFilters, UFilterConnector;

type
  TFilterThreadOnDoneHandler = procedure(ASender: TThread; AFilteredLayer: TBGRABitmap) of object;

  { TFilterThread }

  TFilterThread = class(TThread)
  strict private
    FConnector: TFilterConnector;
    FOnDone: TFilterThreadOnDoneHandler;
    FTask: TFilterTask;
    FFilteredLayer: TBGRABitmap;
  protected
    procedure SynchronizedOnDone;
    procedure CallOnDone;
    function CheckShouldStop: boolean;
    function CreateFilterTask: TFilterTask; virtual; abstract;
  public
    constructor Create(AConnector: TFilterConnector; ASuspended: boolean);
    procedure Execute; override;
    destructor Destroy; override;
    property OnFilterDone: TFilterThreadOnDoneHandler read FOnDone write FOnDone;
    property FilteredLayer: TBGRABitmap read FFilteredLayer;
    property FilterConnector: TFilterConnector read FConnector;
  end;

implementation

{ TFilterThread }

procedure TFilterThread.SynchronizedOnDone;
begin
  if Assigned(FOnDone) then FOnDone(self, FFilteredLayer);
end;

procedure TFilterThread.CallOnDone;
begin
  Synchronize(@SynchronizedOnDone);
end;

function TFilterThread.CheckShouldStop: boolean;
begin
  result := Terminated;
end;

constructor TFilterThread.Create(AConnector: TFilterConnector;
  ASuspended: boolean);
begin
  inherited Create(True);
  FConnector := AConnector;
  FreeOnTerminate := True;
  FFilteredLayer := nil;
  if not ASuspended then Start;
end;

procedure TFilterThread.Execute;
begin
  FreeAndNil(FFilteredLayer);
  FFilteredLayer := FConnector.BackupLayer.Duplicate() as TBGRABitmap;
  FTask := CreateFilterTask;
  FTask.Destination := FFilteredLayer;
  FTask.CheckShouldStop := @CheckShouldStop;
  try
    FTask.Execute;
    if not Terminated then CallOnDone;
  finally
    FreeAndNil(FTask);
  end;
end;

destructor TFilterThread.Destroy;
begin
  FreeAndNil(FFilteredLayer);
  inherited Destroy;
end;


end.

