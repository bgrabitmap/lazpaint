// SPDX-License-Identifier: GPL-3.0-only
unit UPython;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UProcessAuto;

const
  DefaultPythonBin = {$IFDEF WINDOWS}'pyw'{$ELSE}'python3'{$ENDIF};
  {$IFDEF DARWIN}
  UserPythonBin = '/usr/local/bin/python3';
  {$ENDIF}

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
    function CheckScriptAndDependencySafe(AFilename: UTF8String; APythonVersion: integer): boolean;
  public
    constructor Create(APythonBin: string = DefaultPythonBin);
    function Run(AScriptFilename: UTF8String; APythonVersion: integer = 3): boolean;
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
function CheckPythonScriptSafe(AFilename: string; out ASafeModules, AUnsafeModules: TStringList): boolean;

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

function CheckPythonScriptSafe(AFilename: string; out ASafeModules, AUnsafeModules: TStringList): boolean;
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

  function idOk(AId: string; var importCount: integer): boolean;
  const forbidden: array[0..6] of string =
  ('__import__',
   'compile',
   'eval',
   'exec',
   'getattr',
   'globals',
   'locals');
  begin
    if AId = 'import' then inc(importCount);
    exit(binarySearch(AId, forbidden) = -1);
  end;

  const StartIdentifier = ['A'..'Z','a'..'z','_'];
  const ContinueIdentifier = ['A'..'Z','a'..'z','_','0'..'9'];
  const WhiteSpace = [' ', #9];

  function importOk(const s: string; importCount: integer; previousBackslash: boolean): boolean;
  const ForbiddenModules: array[0..22] of string =
  ('builtins',          // Provides direct access to all built-in identifiers of Python.
   'code',              // Facilities to implement interactive Python interpreters.
   'codecs',            // Core support for encoding and decoding text and binary data.
   'ctypes',            // Create and manipulate C-compatible data types in Python, and call functions in dynamic link libraries/shared libraries.
   'ftplib',            // Interface to the FTP protocol.
   'gc',                // Interface to the garbage collection facility for reference cycles.
   'io',                // Core tools for working with streams (core I/O operations).
   'multiprocessing',   // Process-based parallelism.
   'os',                // Interface to the operating system, including file and process operations.
   'pathlib',           // Object-oriented filesystem paths.
   'poplib',            // Client-side support for the POP3 protocol.
   'pty',               // Operations for handling the pseudo-terminal concept.
   'runpy',             // Locating and running Python programs using various modes of the `__main__` module.
   'shutil',            // High-level file operations, including copying and deletion.
   'smtplib',           // Client-side objects for the SMTP and ESMTP protocols.
   'socket',            // Low-level networking operations.
   'subprocess',        // Spawn additional processes, connect to their input/output/error pipes, and obtain their return codes.
   'sys',               // Access and set variables used or maintained by the Python interpreter.
   'telnetlib',         // Client-side support for the Telnet protocol.
   'tempfile',          // Generate temporary files and directories.
   'threading',         // Higher-level threading interfaces on top of the lower-level `_thread` module.
   'wsgiref',           // WSGI utility functions and reference implementation.
   'xmlrpc'             // XML-RPC server and client modules.
  );

  const SafeModules: array[0..26] of string =
  ('PIL',           // Python Imaging Library, for image processing.
   'array',         // Basic mutable array operations.
   'ast',           // Abstract Syntax Trees
   'bisect',        // Algorithms for manipulating sorted lists.
   'calendar',      // Functions for working with calendars and dates.
   'collections',   // Container datatypes like namedtuples and defaultdict.
   'colorsys',      // Color system conversions.
   'copy',          // Shallow and deep copy operations.
   'csv',           // Reading and writing CSV files.
   'datetime',      // Basic date and time types.
   'decimal',       // Fixed and floating point arithmetic using decimal notation.
   'enum',          // Enumerations in Python.
   'fractions',     // Rational numbers.
   'functools',     // Higher-order functions and operations on callable objects.
   'hashlib',       // Secure hash and message digest algorithms.
   'itertools',     // Functions for creating iterators for efficient looping.
   'json',          // Encoding and decoding JSON format.
   'lazpaint',
   'math',          // Mathematical functions.
   'platform',      // Access to platform-specific attributes and functions.
   'queue',         // A multi-producer, multi-consumer queue.
   'random',        // Generate pseudo-random numbers.
   'statistics',    // Mathematical statistics functions.
   'string',        // Common string operations.
   'time',          // Time-related functions.
   'tkinter',       // Standard GUI library for Python.
   'uuid');         // UUID objects

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

  function SkipAs(var idx: integer): boolean;
  var
    subId: String;
  begin
    SkipSpaces(idx);
    if (idx > length(s)) or (s[idx] = '#') then exit(true);
    subId := GetId(idx);
    if subId = 'as' then
    begin
      SkipSpaces(idx);
      subId := GetId(idx);
      if subId = '' then exit(false); // syntax error
    end;
    exit(true);
  end;

  function ParseModuleName(var idx: integer; out AModuleName: string; out AIsSafe: boolean): boolean;
  var
    subId: String;
  begin
    SkipSpaces(idx);
    AIsSafe := false;
    AModuleName := GetId(idx);
    if AModuleName = '' then exit(false); // syntax error
    // check if module is allowed
    if binarySearch(AModuleName, ForbiddenModules) <> -1 then exit(false);
    AIsSafe := binarySearch(AModuleName, SafeModules) <> -1;
    SkipSpaces(idx);
    // submodule
    while (idx <= length(s)) and (s[idx] = '.') do
    begin
      inc(idx);
      SkipSpaces(idx);
      subId := GetId(idx);
      if subId = '' then exit(false); // syntax error
      AModuleName += '.' + subId;
      SkipSpaces(idx);
    end;
    exit(true);
  end;

  procedure AddModule(AModuleName: string; AIsSafe: boolean);
  begin
    if not AIsSafe then
    begin
      if AUnsafeModules = nil then
         AUnsafeModules := TStringList.Create;
      if AUnsafeModules.IndexOf(AModuleName) = -1 then
        AUnsafeModules.Add(AModuleName);
    end else
    begin
      if ASafeModules = nil then
         ASafeModules := TStringList.Create;
      if ASafeModules.IndexOf(AModuleName) = -1 then
        ASafeModules.Add(AModuleName);
    end;
  end;

  var idx: integer;
    fromClause: boolean;
    moduleName, subId: string;
    isSafe: boolean;
  begin
    if importCount <> 1 then exit(false); // syntax error

    if s.StartsWith('from ') then
    begin
      idx := length('from ') + 1;
      fromClause := true;
    end else
    if s.StartsWith('import ') then
    begin
      if previousBackslash then exit(false); // could be an exploit
      idx := length('import ') + 1;
      fromClause := false;
    end
    else
      exit(false); // syntax error

    if not ParseModuleName(idx, moduleName, isSafe) then exit(false);

    if fromClause then
    begin
      subId := GetId(idx);
      if subId <> 'import' then exit(false); // syntax error
      repeat
        SkipSpaces(idx);
        subId := GetId(idx);
        if subId = '' then exit(false); // syntax error
        AddModule(moduleName+'.'+subId, isSafe);
        if not SkipAs(idx) then exit(false);
        SkipSpaces(idx);
        if (idx <= length(s)) and (s[idx] = ',') then inc(idx)
        else break;
      until false;
    end else
    begin
      repeat
        AddModule(moduleName, isSafe);
        if not SkipAs(idx) then exit(false);
        SkipSpaces(idx);
        if (idx <= length(s)) and (s[idx] = ',') then
        begin
          inc(idx);
          if not ParseModuleName(idx, moduleName, isSafe) then exit(false);
        end
        else break;
      until false;
    end;
    if (idx <= length(s)) and (s[idx] <> '#') then // expect end of line
      exit(false); // syntax error

    exit(true);
  end;

  function lineOk(const s: string; previousBackslash: boolean): boolean;
  var
    startId, i: integer;
    importCount: integer;
  begin
    startId := -1;
    importCount := 0;

    for i := 1 to length(s) do
    begin
      // check identifier boundaries
      if (startId = -1) and (s[i] in StartIdentifier) then
      begin
        startId := i;
      end else
      if (startId <> -1) and not (s[i] in ContinueIdentifier) then
      begin
        if not idOk(copy(s, startId, i-startId), importCount) then exit(false);
        startId := -1;
      end;
    end;
    if (startId <> -1) and not idOk(copy(s, startId, length(s)-startId+1), importCount) then
      exit(false);

    if (importCount > 0) and not importOk(s, importCount, previousBackslash) then exit(false);

    exit(true);
  end;

var
  t: textfile;
  s: string;
  previousBackslash: boolean;
begin
  ASafeModules := nil;
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

function TPythonScript.CheckScriptAndDependencySafe(AFilename: UTF8String; APythonVersion: integer): boolean;
var
  filesToCheck: TStringList;

  procedure AddModuleToCheck(AModuleName: UTF8String; ABasePath: UTF8String);
  var fullPath, moduleFilename: string;
  begin
    fullPath := ConcatPaths([ABasePath, StringReplace(AModuleName, '.', PathDelim, [rfReplaceAll])]);
    moduleFilename := fullPath+'.py';
    if (filesToCheck.IndexOf(moduleFilename) = -1) and FileExists(moduleFilename) then
      filesToCheck.Add(moduleFilename) else
    begin
      moduleFilename := fullPath+'\__init__.py';
      if (filesToCheck.IndexOf(moduleFilename) = -1) and FileExists(moduleFilename) then
        filesToCheck.Add(moduleFilename);
      end;
  end;

var
  safeModules, unsafeModules, allUnsafeModules: TStringList;
  proceed: boolean;
  curFile, i: integer;
  curPath: string;

begin
  allUnsafeModules := TStringList.Create;
  allUnsafeModules.Sorted := true;
  allUnsafeModules.Duplicates:= dupIgnore;
  filesToCheck := TStringList.Create;
  filesToCheck.Add(AFilename);
  curFile := 0;
  curPath := ExtractFilePath(AFilename);
  while curFile < filesToCheck.Count do
  begin
    if not CheckPythonScriptSafe(filesToCheck[curFile], safeModules, unsafeModules) then
    begin
      safeModules.Free;
      unsafeModules.Free;
      raise exception.Create('The script file does not seem to be safe: ' +
                             filesToCheck[curFile]);
    end;
    if Assigned(unsafeModules) then
    begin
      for i := 0 to unsafeModules.Count-1 do
      begin
        AddModuleToCheck(unsafeModules[i], curPath);
        allUnsafeModules.Add(unsafeModules[i]);
      end;
    end;
    if Assigned(safeModules) then
    begin
      for i := 0 to safeModules.Count-1 do
        AddModuleToCheck(safeModules[i], curPath);
    end;
    safeModules.Free;
    unsafeModules.Free;
    inc(curFile);
  end;
  filesToCheck.Free;

  if allUnsafeModules.Count > 0 then
  begin
    proceed := true;
    if Assigned(OnWarning) then
    begin
      OnWarning(self, 'Are you sure you would like to run this script? ' +
        'The following modules used by this script may be unsafe: '+
        allUnsafeModules.CommaText, proceed);
    end;
    allUnsafeModules.Free;
    if not proceed then exit(false);
  end else
    allUnsafeModules.Free;

  if PythonVersionMajor <> APythonVersion then
    raise exception.Create(
      StringReplace( StringReplace(rsPythonUnexpectedVersion,
        '%1',inttostr(APythonVersion),[]),
        '%2',inttostr(PythonVersionMajor),[]) + #9 + rsDownload + #9 + 'https://www.python.org');
  exit(true);
end;

constructor TPythonScript.Create(APythonBin: string);
begin
  FPythonBin := APythonBin;
  {$IFDEF DARWIN}
  if (FPythonBin = 'python3') and FileExists(UserPythonBin) then
    FPythonBin:= UserPythonBin;
  {$ENDIF}
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

function TPythonScript.Run(AScriptFilename: UTF8String;
  APythonVersion: integer): boolean;
var exitCode: integer;
begin
  result := false;
  if not CheckScriptAndDependencySafe(AScriptFilename, APythonVersion) then exit;
  FLinePrefix := '';
  FFirstOutput:= true;
  AutomationEnvironment.Values['PYTHONPATH'] := DefaultScriptDirectory;
  AutomationEnvironment.Values['PYTHONIOENCODING'] := 'utf-8';
  try
    exitCode := RunProcessAutomation(FPythonBin, ['-u', AScriptFilename], FPythonSend, @PythonOutput, @PythonError, @PythonBusy);
  finally
    AutomationEnvironment.Clear;
  end;
  FPythonSend := nil;
  result := exitCode = 0;
end;

class function TPythonScript.DefaultScriptDirectory: string;
begin
  if CustomScriptDirectory<>'' then
    result := CustomScriptDirectory
  else
    result := GetResourcePath('scripts');
end;

end.

