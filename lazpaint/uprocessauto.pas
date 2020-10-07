// SPDX-License-Identifier: GPL-3.0-only
unit UProcessAuto;

{$mode objfpc}{$H+}
{ This unit allows to receive line by line the output of a process
  and to send lines to its input in response.

  Note:
  - the process will freeze if it expects an input that is not provided.
  - if the process draw only part of a line, like with a progress bar, this
    won't be received in the events.
}

interface

uses
  Classes, SysUtils;

type
  TReceiveLineEvent = procedure(ALine: RawByteString) of object;
  TSendLineMethod = procedure(const ALine: RawByteString) of object;
  TBusyEvent = procedure(var ASleep: boolean) of object;

var
  AutomationEnvironment: TStringList;

procedure RunProcessAutomation(AExecutable: string; AParameters: array of string;
  out ASendLine: TSendLineMethod;
  AOnReceiveOutput: TReceiveLineEvent;
  AOnReceiveError: TReceiveLineEvent;
  AOnBusy: TBusyEvent);

implementation

uses process, Pipes, math;

const
  LineEndingStr: string = LineEnding;

type
  { TAutomatedProcess }

  TAutomatedProcess = class(TProcess)
    constructor Create(AOwner: TComponent); override;
    procedure SendLine(const ALine: RawByteString);
  end;

procedure RunProcessAutomation(AExecutable: string; AParameters: array of string;
  out ASendLine: TSendLineMethod;
  AOnReceiveOutput: TReceiveLineEvent;
  AOnReceiveError: TReceiveLineEvent;
  AOnBusy: TBusyEvent);

type
  TReceiveBuffer = record
    Data: RawByteString;
    Length: integer;
    OnReceive: TReceiveLineEvent;
  end;

  procedure InitBuffer(out Buffer: TReceiveBuffer; ASize: integer; AOnReceive: TReceiveLineEvent);
  begin
    setlength(Buffer.Data, ASize);
    Buffer.Length:= 0;
    Buffer.OnReceive:= AOnReceive;
  end;

  procedure ParseBuffer(var Buffer: TReceiveBuffer);
  var
    startIdx,idx, count: integer;
    line: RawByteString;
  begin
    startIdx := 1;
    idx := startIdx;
    while idx <= Buffer.Length do
    begin
      //find LineEnding
      if (Buffer.Data[idx] = LineEndingStr[1]) and
         (idx+length(LineEndingStr)-1 <= Buffer.Length) and
         (copy(Buffer.Data,idx,length(LineEndingStr)) = LineEndingStr) then
      begin
        line := copy(Buffer.Data, startIdx, idx-startIdx);
        Buffer.OnReceive(line);
        inc(idx, length(LineEndingStr));
        startIdx := idx;
        continue;
      end;
      inc(idx);
    end;
    if startIdx > 1 then
    begin
      count := Buffer.Length-startIdx+1;
      if count > 0 then
        move(Buffer.Data[startIdx], Buffer.Data[1], Buffer.Length-startIdx+1);
      dec(Buffer.Length, startIdx-1);
    end;
  end;

  function Receive(AInput: TInputPipeStream; var Buffer: TReceiveBuffer): boolean;
  var
    receivedCount: integer;
  begin
    receivedCount := AInput.NumBytesAvailable;
    if receivedCount > 0 then
    begin
      if Buffer.Length+receivedCount > length(Buffer.Data) then
        setlength(Buffer.Data, max(length(Buffer.Data)*2, Buffer.Length+receivedCount));
      AInput.Read(Buffer.Data[Buffer.Length+1], receivedCount);
      inc(Buffer.Length, receivedCount);
      ParseBuffer(Buffer);
      result := true;
    end else
      result := false;
  end;

var
  p: TAutomatedProcess;
  Output, Error: TReceiveBuffer;
  i: integer;
  shouldSleep: Boolean;
begin
  p := TAutomatedProcess.Create(nil);
  ASendLine := @p.SendLine;
  try
    for i := 1 to GetEnvironmentVariableCount do
      p.Environment.Add(GetEnvironmentString(I));
    for i := 0 to AutomationEnvironment.Count-1 do
      p.Environment.Values[AutomationEnvironment.Names[i]] := AutomationEnvironment.Values[AutomationEnvironment.Names[i]];
    p.Executable:= AExecutable;
    for i := 0 to high(AParameters) do
      p.Parameters.Add(AParameters[i]);
    p.Execute;
    InitBuffer(Output, p.PipeBufferSize, AOnReceiveOutput);
    InitBuffer(Error, p.PipeBufferSize, AOnReceiveError);
    while p.Running do
    begin
      if not Receive(p.Output, Output) and
         not Receive(p.Stderr, Error) then
      begin
        shouldSleep := true;
        AOnBusy(shouldSleep);
        if shouldSleep then sleep(15);
      end;
    end;
    Receive(p.Output, Output);
    Receive(p.Stderr, Error);
  finally
    p.Free;
  end;
end;

{ TAutomatedProcess }

constructor TAutomatedProcess.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Options:= [poNoConsole,poUsePipes];
  PipeBufferSize := 65536;
end;

procedure TAutomatedProcess.SendLine(const ALine: RawByteString);
begin
  if length(ALine)>0 then
    Input.Write(ALine[1],length(ALine));
  Input.Write(LineEndingStr[1],length(LineEndingStr));
end;

initialization

  AutomationEnvironment := TStringList.Create;

finalization

  AutomationEnvironment.Free;

end.

