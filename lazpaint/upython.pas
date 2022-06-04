// SPDX-License-Identifier: GPL-3.0-only
unit UPython;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UProcessAuto;

const
  DefaultPythonBin = {$IFDEF WINDOWS}'pyw'{$ELSE}'python3'{$ENDIF};

type
  TReceiveLineEvent = procedure(ASender: TObject; ALine: UTF8String) of object;
  TCommandEvent = procedure(ASender: TObject; ACommand, AParam: UTF8String; out AResult: UTF8String) of object;

  { TPythonScript }

  TPythonScript = class
  private
    FOnBusy: TNotifyEvent;
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
    procedure PythonBusy(var {%H-}ASleep: boolean);
  public
    constructor Create(APythonBin: string = DefaultPythonBin);
    procedure Run(AScriptFilename: UTF8String; APythonVersion: integer = 3);
    class function DefaultScriptDirectory: string;
    property OnOutputLine: TReceiveLineEvent read FOnOutputLine write FOnOutputLine;
    property OnError: TReceiveLineEvent read FOnError write FOnError;
    property OnCommand: TCommandEvent read FOnCommand write FOnCommand;
    property OnBusy: TNotifyEvent read FOnBusy write FOnBusy;
    property PythonVersion: string read FPythonVersion;
    property PythonVersionMajor: integer read GetPythonVersionMajor;
    property ErrorText: UTF8String read FErrorText;
  end;

function GetPythonVersion(APythonBin: string = DefaultPythonBin): string;
function GetScriptTitle(AFilename: string): string;

var
  CustomScriptDirectory: string;

implementation

uses process, UResourceStrings, Forms, UTranslation;

var
  PythonVersionCache: record
    Bin: string;
    Version: string;
  end;

function GetPythonVersion(APythonBin: string = DefaultPythonBin): string;
const PythonVersionPrefix = 'Python ';
var versionStr: string;
begin
  if (PythonVersionCache.Bin <> APythonBin) or (PythonVersionCache.Version = '?') then
  begin
    RunCommand(APythonBin, ['-V'], versionStr, []);
    PythonVersionCache.Bin := APythonBin;
    if versionStr.StartsWith(PythonVersionPrefix) then
      PythonVersionCache.Version := trim(copy(versionStr,length(PythonVersionPrefix)+1,
             length(versionStr)-length(PythonVersionPrefix)))
    else
      PythonVersionCache.Version := '?';
  end;
  result := PythonVersionCache.Version;
end;

function GetScriptTitle(AFilename: string): string;
var t: textfile;
  header: string;
  matchLang: boolean;

  procedure RetrieveTitle(AText: string; ADefault: boolean; var title: string; out ALangMatch: boolean);
  var
    posCloseBracket: SizeInt;
    lang: String;
  begin
    If AText.StartsWith('#') then
      Delete(AText, 1,1);
    AText := AText.Trim;
    ALangMatch := false;
    if AText.StartsWith('(') then
    begin
      posCloseBracket := pos(')', AText);
      if posCloseBracket > 0 then
      begin
        lang := copy(AText, 2, posCloseBracket-2);
        delete(AText, 1, posCloseBracket);
        AText := AText.Trim;
        if lang = ActiveLanguage then
          ALangMatch:= true;
      end;
    end else
    begin
      if not ADefault then exit;
      if ActiveLanguage = DesignLanguage then ALangMatch:= true;
    end;
    if ALangMatch or ADefault then
    begin
      title := AText;
      title := StringReplace(title, ' >', '>', [rfReplaceAll]);
      title := StringReplace(title, '> ', '>', [rfReplaceAll]);
    end;
  end;

  procedure TranslateWithPoFile(var title: string);
  var elements: TStringList;
    i: integer;
    u: string;
  begin
    elements := TStringList.Create;
    try
      elements.Delimiter := '>';
      elements.QuoteChar := #0;
      elements.DelimitedText := StringReplace(title, ' ', #160, [rfReplaceAll]);
      for i := 0 to elements.Count-1 do
      begin
        u := Trim(StringReplace(elements[i], #160, ' ', [rfReplaceAll]));
        elements[i] := DoTranslate('', u);
      end;
    finally
      title := elements.DelimitedText;
      elements.free;
    end;
  end;

begin
  result := '';
  assignFile(t, AFilename);
  reset(t);
  try
    readln(t, header);
    if header.StartsWith('#') then
    begin
      RetrieveTitle(header, true, result, matchLang);
      while not matchLang do
      begin
        readln(t, header);
        if header.StartsWith('#') then
        begin
          RetrieveTitle(header, false, result, matchLang);
        end else break;
      end;
      if not matchLang then
         TranslateWithPoFile(result);
    end;
  finally
    closefile(t);
  end;
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
      displayedLine := '';
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

procedure TPythonScript.PythonBusy(var ASleep: boolean);
begin
  if Assigned(FOnBusy) then FOnBusy(self);
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
  {%H-}errPos: integer;
begin
  posDot := pos('.',PythonVersion);
  if posDot = 0 then
    result := 0
  else
    val(copy(PythonVersion,1,posDot-1), result, errPos);
end;

procedure TPythonScript.Run(AScriptFilename: UTF8String;
  APythonVersion: integer);
begin
  FLinePrefix := '';
  if PythonVersionMajor <> APythonVersion then
    raise exception.Create(
      StringReplace( StringReplace(rsPythonUnexpectedVersion,
        '%1',inttostr(APythonVersion),[]),
        '%2',inttostr(PythonVersionMajor),[]) + #9 + rsDownload + #9 + 'https://www.python.org');
  FFirstOutput:= true;
  AutomationEnvironment.Values['PYTHONPATH'] := DefaultScriptDirectory;
  try
    RunProcessAutomation(FPythonBin, ['-u', AScriptFilename], FPythonSend, @PythonOutput, @PythonError, @PythonBusy);
  finally
    AutomationEnvironment.Clear;
  end;
  FPythonSend := nil;
end;

class function TPythonScript.DefaultScriptDirectory: string;
begin
  if CustomScriptDirectory<>'' then
    result := CustomScriptDirectory
  else
    result := GetResourcePath('scripts');
end;

end.

