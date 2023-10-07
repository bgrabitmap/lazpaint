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
  TWarningEvent = procedure(ASender: TObject; AMessage: UTF8String; out AProceed: boolean) of object;

  { TPythonScript }

  TPythonScript = class
  private
    FPythonBin: string;
    FPythonVersion: string;
    FLinePrefix: RawByteString;
    FOnCommand: TCommandEvent;
    FOnError: TReceiveLineEvent;
    FOnBusy: TNotifyEvent;
    FOnWarning: TWarningEvent;
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
    property OnWarning: TWarningEvent read FOnWarning write FOnWarning;
    property PythonVersion: string read FPythonVersion;
    property PythonVersionMajor: integer read GetPythonVersionMajor;
    property ErrorText: UTF8String read FErrorText;
  end;

function GetPythonVersion(APythonBin: string = DefaultPythonBin): string;
function GetScriptTitle(AFilename: string): string;
function CheckPythonScriptSafe(AFilename: string; out AUnsafeModules: TStringList): boolean;

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

function CheckPythonScriptSafe(AFilename: string; out AUnsafeModules: TStringList): boolean;
  function binarySearch(x: string; a: array of string): integer;
  var  L, R, M: integer;  // left, right, middle
  begin
    if Length(a)=0 then Exit(-1);
    L := Low (a);
    R := High(a);
    while (L <= R) do begin
      M := (L + R) div 2;
      if (x = a[M]) then Exit(M);  // found x in a
      if (x > a[M])
      then L := Succ(M)
      else R := Pred(M);
    end;
    Exit(-1) // did not found x in a
  end;

  function idOk(AId: string; var isImport: integer): boolean;
  const forbidden: array[0..6] of string =
  ('__import__',
   'compile',
   'eval',
   'exec',
   'getattr',
   'globals',
   'locals');
  begin
    if AId = 'import' then inc(isImport);
    exit(binarySearch(AId, forbidden) = -1);
  end;

  const StartIdentifier = ['A'..'Z','a'..'z','_'];
  const ContinueIdentifier = ['A'..'Z','a'..'z','_','0'..'9'];
  const WhiteSpace = [' ', #9];

  function importOk(const s: string; isImport: integer; previousBackslash: boolean): boolean;
  const forbiddenModules: array[0..23] of string =
  ('ast',
   'builtins',
   'code',
   'codecs',
   'ctypes',
   'ftplib',
   'gc',
   'io',
   'multiprocessing',
   'os',
   'pathlib',
   'poplib',
   'pty',
   'runpy',
   'shutil',
   'smtplib',
   'socket',
   'subprocess',
   'sys',
   'telnetlib',
   'tempfile',
   'threading',
   'wsgiref',
   'xmlrpc');

  const safeModules: array[0..10] of string =
  ('PIL',
   'calendar',
   'datetime',
   'decimal',
   'fractions',
   'lazpaint',
   'math',
   'platform',
   'statistics',
   'time',
   'tkinter');

  procedure SkipSpaces(var idx: integer);
  begin
    while (idx <= length(s)) and (s[idx] in WhiteSpace) do inc(idx);
  end;

  function GetId(var idx: integer): string;
  var idxEnd: integer;
  begin
    if (idx > length(s)) or not (s[idx] in StartIdentifier) then exit('');
    idxEnd := idx+1;
    while (idxEnd <= length(s)) and (s[idxEnd] in ContinueIdentifier) do inc(idxEnd);
    result := copy(s, idx, idxEnd-idx);
    idx := idxEnd;
  end;

  var idx: integer;
    importAfter: boolean;
    moduleName, subId: string;
  begin
    if isImport <> 1 then exit(false); // syntax error

    if s.StartsWith('from ') then
    begin
      idx := length('from ') + 1;
      importAfter := true;
    end else
    if s.StartsWith('import ') then
    begin
      if previousBackslash then exit(false); // could be an exploit
      idx := length('import ') + 1;
      importAfter := false;
    end
    else
      exit(false); // syntax error

    SkipSpaces(idx);
    moduleName := GetId(idx);
    if moduleName = '' then exit(false); // syntax error
    // check if module is allowed
    if binarySearch(moduleName, forbiddenModules) <> -1 then exit(false);
    if binarySearch(moduleName, safeModules) = -1 then
    begin
      if AUnsafeModules = nil then
         AUnsafeModules := TStringList.Create;
      if AUnsafeModules.IndexOf(moduleName) = -1 then
        AUnsafeModules.Add(moduleName);
    end;

    SkipSpaces(idx);
    // submodule
    while (idx <= length(s)) and (s[idx] = '.') do
    begin
      inc(idx);
      SkipSpaces(idx);
      subId := GetId(idx);
      if subId = '' then exit(false); // syntax error
      SkipSpaces(idx);
    end;

    if importAfter then
    begin
      subId := GetId(idx);
      if subId <> 'import' then exit(false); // syntax error
    end else
    begin
      if (idx > length(s)) or (s[idx] = '#') then exit(true);

      subId := GetId(idx);
      if subId = 'as' then
      begin
        SkipSpaces(idx);
        subId := GetId(idx);
        if subId = '' then exit(false); // syntax error

        if (idx <= length(s)) and (s[idx] <> '#') then // expect end of line
          exit(false); // syntax error
      end;
    end;

    exit(true);
  end;

  function lineOk(const s: string; previousBackslash: boolean): boolean;
  var
    startId, i: integer;
    isImport: integer;
  begin
    startId := -1;
    isImport := 0;

    for i := 1 to length(s) do
    begin
      // check identifier boundaries
      if (startId = -1) and (s[i] in StartIdentifier) then
      begin
        startId := i;
      end else
      if (startId <> -1) and not (s[i] in ContinueIdentifier) then
      begin
        if not idOk(copy(s, startId, i-startId), isImport) then exit(false);
        startId := -1;
      end;
    end;
    if (startId <> -1) and not idOk(copy(s, startId, length(s)-startId+1), isImport) then
      exit(false);

    if (isImport > 0) and not importOk(s, isImport, previousBackslash) then exit(false);

    exit(true);
  end;

var
  t: textfile;
  s: string;
  previousBackslash: boolean;
begin
  AUnsafeModules := nil;
  assignFile(t, AFilename);
  reset(t);
  previousBackslash := false;
  while not eof(t) do
  begin
    readln(t, s);
    s := trim(s);
    if not lineOk(s, previousBackslash) then exit(false);
    previousBackslash := s.EndsWith('\');
  end;
  closefile(t);
  exit(true);
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
var
  unsafeModules: TStringList;
  proceed: boolean;
begin
  if not CheckPythonScriptSafe(AScriptFilename, unsafeModules) then
  begin
    unsafeModules.Free;
    raise exception.Create('The script file does not seem to be safe');
  end;
  if Assigned(unsafeModules) then
  begin
    proceed := true;
    if Assigned(OnWarning) then
    begin
      OnWarning(self, 'Are you sure you would like to run this script? ' +
        'The following modules used by this script may be unsafe: '+
        unsafeModules.CommaText, proceed);
    end;
    unsafeModules.Free;
    if not proceed then exit;
  end;
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

