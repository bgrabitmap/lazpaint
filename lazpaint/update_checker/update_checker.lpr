program update_checker;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, Forms, uupdate, lnetvisual, uscaledpi,
  DefaultTranslator, Translations, LCLProc, LResources, SysUtils, FileUtil;

{$R *.res}

procedure TranslateUpdateChecker;
var
  Lang,FallbackLang,POFile: String;
begin
  Lang:='';
  FallbackLang:='';
  LCLGetLanguageIDs(Lang,FallbackLang);

  {$IFDEF WINDOWS}
    POFile:=ExtractFilePath(Application.ExeName)+'i18n/update_checker.'+FallbackLang+'.po';
  {$ELSE}
    POFile:='i18n/update_checker.'+FallbackLang+'.po';
  {$ENDIF}

  if FileExistsUTF8(POFile) then
  begin
    LRSTranslator:=TPoTranslator.Create(POFile);
    TranslateResourceStrings(POFile);
  end;
end;

begin
  TranslateUpdateChecker;
  Application.Title:='Update Checker';
  Application.Initialize;
  Application.CreateForm(TFrmUpdate, FrmUpdate);
  HighDPI(96);
  Application.Run;
end.

