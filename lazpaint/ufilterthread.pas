unit UFilterThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRAFilters, UFilterConnector;

type
  TFilterThread = class;

  TThreadManagerEvent = (tmeStartingNewTask, tmeCompletedTask, tmeAbortedTask);
  TThreadManagerEventHandler = procedure(ASender:TObject; AEvent: TThreadManagerEvent) of object;

  { TFilterThreadManager }

  TFilterThreadManager = class
  private
    FThread: TFilterThread;
    FFilterConnector: TFilterConnector;
    FQuitting,FCancellingPreview: boolean;
    FNextTask: TFilterTask;
    FOnEvent: TThreadManagerEventHandler;
    FLastUpdatedY: integer;
    function GetReadyToClose: boolean;
    procedure StartNextTask;
    procedure RaiseEvent(AEvent: TThreadManagerEvent);
  protected
    procedure OnFilterDone({%H-}ASender: TThread; {%H-}AFilteredLayer: TBGRABitmap);
    procedure OnFilterTerminate({%H-}ASender: TObject);
  public
    constructor Create(AFilterConnector: TFilterConnector);
    destructor Destroy; override;
    procedure WantPreview(ATask: TFilterTask);
    procedure Quit;
    procedure RegularCheck;
    property Quitting: boolean read FQuitting;
    property ReadyToClose: boolean read GetReadyToClose;
    property CancellingPreview: boolean read FCancellingPreview;
    property OnEvent: TThreadManagerEventHandler read FOnEvent write FOnEvent;
  end;

  TFilterThreadOnDoneHandler = procedure(ASender: TThread; AFilteredLayer: TBGRABitmap) of object;

  { TFilterThread }

  TFilterThread = class(TThread)
  strict private
    FConnector: TFilterConnector;
    FOnDone: TFilterThreadOnDoneHandler;
    FTask: TFilterTask;
    FFilteredLayer: TBGRABitmap;
    FCurrentY: integer;
  protected
    procedure SynchronizedOnDone;
    procedure CallOnDone;
    function CheckShouldStop(ACurrentY: integer): boolean;
    function CreateFilterTask: TFilterTask; virtual; abstract;
  public
    constructor Create(AConnector: TFilterConnector; ASuspended: boolean);
    procedure Execute; override;
    destructor Destroy; override;
    property OnFilterDone: TFilterThreadOnDoneHandler read FOnDone write FOnDone;
    property FilteredLayer: TBGRABitmap read FFilteredLayer;
    property FilterConnector: TFilterConnector read FConnector;
    property CurrentY: integer read FCurrentY;
  end;

  { TSingleTaskFilterThread }

  TSingleTaskFilterThread = class(TFilterThread)
  private
    FTask: TFilterTask;
  protected
    function CreateFilterTask: TFilterTask; override;
  public
    constructor Create(AFilterConnector: TFilterConnector; ATask: TFilterTask; ASuspended: boolean);
    destructor Destroy; override;
  end;

implementation

{ TSingleTaskFilterThread }

function TSingleTaskFilterThread.CreateFilterTask: TFilterTask;
begin
  result := FTask;
  FTask := nil;
end;

constructor TSingleTaskFilterThread.Create(AFilterConnector: TFilterConnector;
  ATask: TFilterTask; ASuspended: boolean);
begin
  FTask := ATask;
  inherited Create(AFilterConnector,ASuspended);
end;

destructor TSingleTaskFilterThread.Destroy;
begin
  FTask.Free;
  inherited Destroy;
end;

{ TFilterThreadManager }

procedure TFilterThreadManager.StartNextTask;
begin
  if not Assigned(FNextTask) then exit;
  FThread := TSingleTaskFilterThread.Create(FFilterConnector, FNextTask, True);
  FNextTask := nil;
  FThread.OnTerminate:= @OnFilterTerminate;
  FThread.OnFilterDone := @OnFilterDone;
  FThread.Start;
  FLastUpdatedY:= 0;
  RaiseEvent(tmeStartingNewTask);
end;

function TFilterThreadManager.GetReadyToClose: boolean;
begin
  result := FQuitting and not FCancellingPreview;
end;

procedure TFilterThreadManager.RaiseEvent(AEvent: TThreadManagerEvent);
begin
  if Assigned(FOnEvent) then FOnEvent(self,AEvent);
end;

procedure TFilterThreadManager.OnFilterDone(ASender: TThread;
  AFilteredLayer: TBGRABitmap);
var changedBounds: TRect;
begin
  if FLastUpdatedY < FFilterConnector.WorkArea.Bottom then
  begin
    changedBounds := rect(FFilterConnector.WorkArea.Left,FLastUpdatedY,FFilterConnector.WorkArea.Right,FFilterConnector.WorkArea.Bottom);
    If Assigned(AFilteredLayer) then
      FFilterConnector.PutImage(AFilteredLayer,changedBounds,False,False)
    else
      FFilterConnector.InvalidateActiveLayer(changedBounds);
    FLastUpdatedY := FFilterConnector.WorkArea.Bottom;
  end;
  FThread := nil; //it will free itself, set it now to nil so that it cannot be cancelled
  RaiseEvent(tmeCompletedTask);
end;

procedure TFilterThreadManager.OnFilterTerminate(ASender: TObject);
begin
  FThread := nil; //it will free itself
  if FCancellingPreview then
  begin
    if Quitting or not Assigned(FNextTask) then FFilterConnector.RestoreBackup;
    FCancellingPreview := false;
    RaiseEvent(tmeAbortedTask);
  end;
  if not Quitting then StartNextTask;
end;

constructor TFilterThreadManager.Create(AFilterConnector: TFilterConnector);
begin
  FFilterConnector := AFilterConnector;
end;

destructor TFilterThreadManager.Destroy;
begin
  if Assigned(FThread) then
    raise exception.Create('Current task is not terminated');
  inherited Destroy;
end;

procedure TFilterThreadManager.WantPreview(ATask: TFilterTask);
begin
  if FQuitting then
  begin
    FreeAndNil(ATask);
    exit;
  end;
  FreeAndNil(FNextTask);
  FNextTask := ATask;
  if Assigned(FThread) then
  begin
    FCancellingPreview:= true;
    FThread.Terminate;
  end else
    StartNextTask;
end;

procedure TFilterThreadManager.Quit;
begin
  FQuitting:= true;
  if Assigned(FThread) then
  begin
    FCancellingPreview:= true;
    FThread.Terminate;
  end;
  FreeAndNil(FNextTask);
end;

procedure TFilterThreadManager.RegularCheck;
var filteredLayer: TBGRABitmap;
  currentY: integer;
  changedBounds: TRect;
begin
  if Assigned(FThread) and not FQuitting and not FCancellingPreview then
  begin
    filteredLayer := (FThread as TFilterThread).FilteredLayer;
    currentY := FThread.CurrentY;
    if currentY >= FLastUpdatedY then
    begin
      changedBounds := rect(FFilterConnector.WorkArea.Left,FLastUpdatedY,FFilterConnector.WorkArea.Right,currentY);
      if (currentY < FFilterConnector.WorkArea.Bottom) and (currentY=FLastUpdatedY) then currentY+=1;
      if filteredLayer <> nil then
        FFilterConnector.PutImage(filteredLayer,changedBounds,False,False)
      else
        FFilterConnector.InvalidateActiveLayer(changedBounds);
    end;
    FLastUpdatedY := currentY;
  end else
  if Assigned(FNextTask) then
  begin
    if not FCancellingPreview then StartNextTask;
  end;
end;

{ TFilterThread }

procedure TFilterThread.SynchronizedOnDone;
begin
  if Assigned(FOnDone) then FOnDone(self, FFilteredLayer);
end;

procedure TFilterThread.CallOnDone;
begin
  Synchronize(@SynchronizedOnDone);
end;

function TFilterThread.CheckShouldStop(ACurrentY: integer): boolean;
begin
  FCurrentY:= ACurrentY;
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
  FCurrentY:= 0;
  FreeAndNil(FFilteredLayer);
  FTask := CreateFilterTask;
  If FTask.Destination = nil then
  begin
    FFilteredLayer := FConnector.BackupLayer.Duplicate() as TBGRABitmap;
    FTask.Destination := FFilteredLayer;
  end;
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

