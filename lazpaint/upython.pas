unit UPython;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UProcessAuto;

const
  DefaultPythonBin = {$IFDEF WINDOWS}'py'{$ELSE}'python3'{$ENDIF};

type
  TReceiveLineEvent = procedure(ASender: TObject; ALine: UTF8String) of object;
  TCommandEvent = procedure(ASender: TObject; ACommand, AParam: UTF8String; out AResult: UTF8String) of object;

  { TPythonScript }

  TPythonScript = class
  private
    FPythonBin: string;
    FPythonVersion: string;
    FLinePrefix: RawByteString;
    FOnCommand: TCommandEvent;
    FOnError: TReceiveLineEvent;
    FOnOutputLine: TReceiveLineEvent;
    FPythonSend: TSendLineMethod;
    FErrorText: UTF8String;
    FFirstOutput: boolean;
    function GetPythonVersionMajor: integer;
    procedure PythonError(ALine: RawByteString);
    procedure PythonOutput(ALine: RawByteString);
  public
    constructor Create(APythonBin: string = DefaultPythonBin);
    procedure Run(AScriptFilename: UTF8String; APythonVersion: integer = 3);
    property OnOutputLine: TReceiveLineEvent read FOnOutputLine write FOnOutputLine;
    property OnError: TReceiveLineEvent read FOnError write FOnError;
    property OnCommand: TCommandEvent read FOnCommand write FOnCommand;
    property PythonVersion: string read FPythonVersion;
    property PythonVersionMajor: integer read GetPythonVersionMajor;
    property ErrorText: UTF8String read FErrorText;
  end;

function GetPythonVersion(APythonBin: string = DefaultPythonBin): string;

implementation

uses process;

function GetPythonVersion(APythonBin: string = DefaultPythonBin): string;
const PythonVersionPrefix = 'Python ';
var versionStr: string;
begin
  RunCommand(APythonBin, ['-V'], versionStr, [poStderrToOutPut]);
  if versionStr.StartsWith(PythonVersionPrefix) then
    result := trim(copy(versionStr,length(PythonVersionPrefix)+1,
           length(versionStr)-length(PythonVersionPrefix)))
  else
    result := '?';
end;

{ TPythonScript }

procedure TPythonScript.PythonOutput(ALine: RawByteString);
var
  idxParam, cmdPos: SizeInt;
  command, param, finalLine: RawByteString;
  commandRes: UTF8String;
  i, curDisplayPos, maxDisplayLen: Integer;
  displayedLine: RawByteString;
begin
  if FFirstOutput then
  begin
    if ALine <> 'LazPaint script'#9 then
      raise exception.Create('This is not a LazPaint script')
    else
    begin
      FFirstOutput:= false;
      if Assigned(FPythonSend) then
        FPythonSend(chr(27)+'LazPaint')
      else
        raise exception.Create('Send callback not defined');
    end;
  end;

  cmdPos := pos(#27, ALine);
  if (cmdPos > 0) then
  begin
    FLinePrefix += copy(ALine, 1, cmdPos-1);
    delete(ALine, 1, cmdPos-1);

    idxParam := Pos(#29, ALine);
    param := '';
    if idxParam = 0 then
      command := copy(ALine,2,length(ALine)-1)
    else
    begin
      command := copy(ALine,2,idxParam-2);
      param := copy(ALine,idxParam+1,length(ALine)-(idxParam+1)+1);
    end;
    if command<>'' then
    begin
      if command[length(command)] = '?' then
      begin
        delete(command, length(command), 1);
        if Assigned(FOnCommand) then
          FOnCommand(self, command, param, commandRes)
        else
          commandRes := '';
        if Assigned(FPythonSend) then
          FPythonSend(commandRes);
      end else
      begin
        if Assigned(FOnCommand) then
          FOnCommand(self, command, param, commandRes);
      end;
    end;

  end else
  begin
    if Assigned(FOnOutputLine) then
    begin
      finalLine := FLinePrefix+ALine;
      setlength(displayedLine, 80);
      curDisplayPos := 1;
      maxDisplayLen := 0;
      for i := 1 to length(finalLine) do
      begin
        if finalLine[i] = #13 then curDisplayPos := 1 else
        if finalLine[i] = #8 then
        begin
          if curDisplayPos > 1 then dec(curDisplayPos);
        end else
        begin
          if curDisplayPos > length(displayedLine) then
            setlength(displayedLine, length(displayedLine)*2);
          displayedLine[curDisplayPos] := finalLine[i];
          if curDisplayPos > maxDisplayLen then
            maxDisplayLen := curDisplayPos;
          inc(curDisplayPos);
        end;
      end;
      setlength(displayedLine, maxDisplayLen);
      FOnOutputLine(self, displayedLine);
    end;
    FLinePrefix := '';
  end;
end;

constructor TPythonScript.Create(APythonBin: string);
begin
  FPythonBin := APythonBin;
  FPythonVersion:= GetPythonVersion(FPythonBin);
end;

procedure TPythonScript.PythonError(ALine: RawByteString);
begin
  if Assigned(FOnError) then
    FOnError(self, ALine)
  else
    FErrorText += ALine+LineEnding;
end;

function TPythonScript.GetPythonVersionMajor: integer;
var
  posDot: SizeInt;
  errPos: integer;
begin
  posDot := pos('.',PythonVersion);
  val(copy(PythonVersion,1,posDot-1), result, errPos);
end;

procedure TPythonScript.Run(AScriptFilename: UTF8String;
  APythonVersion: integer);
begin
  FLinePrefix := '';
  if PythonVersionMajor <> APythonVersion then
    raise exception.Create('Expected python version is '+inttostr(APythonVersion)+' but '+inttostr(PythonVersionMajor)+' found.');
  FFirstOutput:= true;
  RunProcessAutomation(FPythonBin, ['-u', AScriptFilename], FPythonSend, @PythonOutput, @PythonError);
  FPythonSend := nil;
end;

end.

