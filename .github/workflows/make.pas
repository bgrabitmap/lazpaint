program Make;
{$mode objfpc}{$H+}

uses
  Classes,
  SysUtils,
  StrUtils,
  FileUtil,
  Zipper,
  fphttpclient,
  RegExpr,
  openssl,
  opensslsockets,
  Process;

const
  Target: string = 'lazpaint';
  Dependencies: array of string = ('BGRAControls', 'BGRABitmap');

type
  Output = record
    Code: boolean;
    Output: ansistring;
  end;

  function CheckModules: Output;
  begin
    if FileExists('.gitmodules') then
      if RunCommand('git', ['submodule', 'update', '--init', '--recursive',
        '--force', '--remote'], Result.Output) then
        Writeln(stderr, #27'[33m', Result.Output, #27'[0m');
  end;

  function AddPackage(Path: string): Output;
  begin
    with TRegExpr.Create do
    begin
      Expression :=
        {$IFDEF MSWINDOWS}
        '(cocoa|x11|_template)'
      {$ELSE}
        '(cocoa|gdi|_template)'
      {$ENDIF}
      ;
      if not Exec(Path) and RunCommand('lazbuild', ['--add-package-link', Path],
        Result.Output) then
        Writeln(stderr, #27'[33m', 'added ', Path, #27'[0m');
      Free;
    end;
  end;

  function BuildProject(Path: string): Output;
  var
    Line: string;
  begin
    Write(stderr, #27'[33m', 'build from ', Path, #27'[0m');
    try
      Result.Code := RunCommand('lazbuild', ['--build-all', '--recursive',
        '--no-write-project', Path], Result.Output);
      if Result.Code then
        for Line in SplitString(Result.Output, LineEnding) do
        begin
          if ContainsStr(Line, 'Linking') then
          begin
            Result.Output := SplitString(Line, ' ')[2];
            Writeln(stderr, #27'[32m', ' to ', Result.Output, #27'[0m');
            break;
          end;
        end
      else
      begin
        ExitCode += 1;
        for Line in SplitString(Result.Output, LineEnding) do
          with TRegExpr.Create do
          begin
            Expression := '(Fatal|Error):';
            if Exec(Line) then
            begin
              WriteLn(stderr);
              Writeln(stderr, #27'[31m', Line, #27'[0m');
            end;
            Free;
          end;
      end;
    except
      on E: Exception do
        WriteLn(stderr, 'Error: ' + E.ClassName + #13#10 + E.Message);
    end;
  end;

  function RunTest(Path: string): Output;
  var
    Temp: string;
  begin
    Result := BuildProject(Path);
    Temp:= Result.Output;
    if Result.Code then
        try
          if not RunCommand(Temp, ['--all', '--format=plain', '--progress'], Result.Output) then
            ExitCode += 1;
          WriteLn(stderr, Result.Output);
        except
          on E: Exception do
            WriteLn(stderr, 'Error: ' + E.ClassName + #13#10 + E.Message);
        end;
  end;

  function AddOPM(Each: string): string;
  var
    TempFile, Url: string;
    Zip: TStream;
  begin
    Result :=
      {$IFDEF MSWINDOWS}
      GetEnvironmentVariable('APPDATA') + '\.lazarus\onlinepackagemanager\packages\'
      {$ELSE}
      GetEnvironmentVariable('HOME') + '/.lazarus/onlinepackagemanager/packages/'
      {$ENDIF}
      + Each;
    TempFile := GetTempFileName;
    Url := 'https://packages.lazarus-ide.org/' + Each + '.zip';
    if not DirectoryExists(Result) then
    begin
      Zip := TFileStream.Create(TempFile, fmCreate or fmOpenWrite);
      with TFPHttpClient.Create(nil) do
      begin
        try
          AddHeader('User-Agent', 'Mozilla/5.0 (compatible; fpweb)');
          AllowRedirect := True;
          Get(Url, Zip);
          WriteLn(stderr, 'Download from ', Url, ' to ', TempFile);
        finally
          Free;
        end;
      end;
      Zip.Free;
      CreateDir(Result);
      with TUnZipper.Create do
      begin
        try
          FileName := TempFile;
          OutputPath := Result;
          Examine;
          UnZipAllFiles;
          WriteLn(stderr, 'Unzip from ', TempFile, ' to ', Result);
        finally
          Free;
        end;
      end;
      DeleteFile(TempFile);
    end;
  end;

  function LintPython(Path: string): Output;
  begin
    WriteLn(stderr, #27'[33m', 'Linting Python file: ', Path, #27'[0m');
    if not RunCommand('python3', ['-m', 'pylint', Path], Result.Output) then
      begin
        Writeln(stderr, #27'[31m', Result.Output, #27'[0m');
        ExitCode += 1;
      end
  end;

  function LintC(Path: string): Output;
  begin
    WriteLn(stderr, #27'[33m', 'Linting C file: ', Path, #27'[0m');
    if not RunCommand('cppcheck', ['--language=c', '--enable=warning,style', '--template=gcc', Path], Result.Output) then
      begin
        Writeln(stderr, #27'[31m', Result.Output, #27'[0m');
        ExitCode += 1;
      end
  end;

  function LintShell(Path: string): Output;
  begin
    WriteLn(stderr, #27'[33m', 'Linting Shell file: ', Path, #27'[0m');
    if not RunCommand('shellcheck', ['--external-sources', Path], Result.Output) then
      begin
        Writeln(stderr, #27'[31m', Result.Output, #27'[0m');
        ExitCode += 1;
      end
  end;

  procedure Main;
  var
    Each, Item: string;
    List: TStringList;
  begin
    CheckModules;
    InitSSLInterface;
    for Each in Dependencies do
    begin
      List := FindAllFiles(AddOPM(Each), '*.lpk', True);
      try
        for Item in List do
          AddPackage(Item);
      finally
        List.Free;
      end;
    end;
    List := FindAllFiles(GetCurrentDir, '*.lpk', True);
    try
      for Each in List do
        AddPackage(Each);
    finally
      List.Free;
    end;
    List := FindAllFiles(Target, '*.lpi', True);
    try
      for Each in List do
        if ContainsStr(ReadFileToString(ReplaceStr(Each, '.lpi', '.lpr')),
          'consoletestrunner') then
          RunTest(Each)
        else
          BuildProject(Each);
    finally
      List.Free;
    end;
    {$IFDEF LINUX}
    List := FindAllFiles(GetCurrentDir, '*.py', True);
    try
      for Each in List do
        LintPython(Each);
    finally
      List.Free;
    end;
    List := FindAllFiles(GetCurrentDir, '*.c', True);
    try
      for Each in List do
        LintC(Each);
    finally
      List.Free;
    end;
    List := FindAllFiles(GetCurrentDir, '*.sh', True);
    try
      for Each in List do
        LintShell(Each);
    finally
      List.Free;
    end;
    {$ENDIF}
    WriteLn(stderr);
    if ExitCode <> 0 then
      WriteLn(stderr, #27'[31m', 'Errors: ', ExitCode, #27'[0m')
    else
      WriteLn(stderr, #27'[32m', 'Errors: ', ExitCode, #27'[0m');
  end;

begin
  Main;
end.
