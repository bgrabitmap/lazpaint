unit UTranslation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UConfig, IniFiles;

{*************** Language ****************}
const
  DesignLanguage = 'en';
var
  ActiveLanguage: string;

function FallbackLanguage: string;

{*************** Language files ************}
function LanguagePath: string;
function LazPaintLanguageFile(ALanguage: string): string;

{*************** Translation ***************}
procedure FillLanguageList(AConfig: TLazPaintConfig);
procedure TranslateLazPaint(AConfig: TIniFile);


implementation

uses Forms, FileUtil, LCLProc, LResources, DefaultTranslator, Translations;

function LanguagePath: string;
{$IFDEF DARWIN}
const macLangDir = '..'+PathDelim+'..'+PathDelim+'Resources'+PathDelim+'i18n';
{$ENDIF}
begin
  {$IFDEF WINDOWS}
    result:=ExtractFilePath(Application.ExeName)+'i18n'+PathDelim;
  {$ELSE}
    {$IFDEF DARWIN}
    if DirectoryExists(macLangDir) then
      result := macLangDir+PathDelim
    else
    {$ENDIF}
    result:='i18n'+PathDelim;
  {$ENDIF}
end;

function LazPaintLanguageFile(ALanguage: string): string;
begin
  result := 'lazpaint.'+ALanguage+'.po';
end;

function FallbackLanguage: string;
var Lang,FallbackLang: string;
begin
  Lang:='';
  FallbackLang:='';
  LCLGetLanguageIDs(Lang,FallbackLang);
  result := FallbackLang;
end;

//translate program
procedure TranslateLazPaint(AConfig: TIniFile);
var
  POFile: String;
  Language: string;
begin
  Language := AConfig.ReadString('General','Language','');
  if Language = '' then
    Language := FallBackLanguage;
  if Language = 'en' then exit;

  POFile:=ActualConfigDir+LazPaintLanguageFile(Language); //updated file
  if not FileExistsUTF8(POFile) then
    POFile:=LanguagePath+LazPaintLanguageFile(Language); //default file
  if FileExistsUTF8(POFile) then
  begin
    LRSTranslator:=TPoTranslator.Create(POFile);
    TranslateResourceStrings(POFile);
    ActiveLanguage:= Language;
  end;

  POFile:=LanguagePath+'lclstrconsts.'+Language+'.po';
  if FileExistsUTF8(POFile) then
    Translations.TranslateUnitResourceStrings('LCLStrConsts',POFile);
end;

//fill language list for configuration
procedure FillLanguageList(AConfig: TLazPaintConfig);
var
  dir : TSearchRec;
  idxDot: integer;
  language: string;

  i: integer;
  updatedLanguages: TStringList;
begin
  AConfig.Languages.Add('en');
  if FindFirst(LanguagePath+'*.po',faAnyFile,dir) = 0 then
  repeat
    if (dir.Attr and (faDirectory or faVolumeId) = 0) and
      (copy(dir.name,1,9)='lazpaint.') then
    begin
      language := copy(dir.name,10,length(dir.name)-10+1);
      idxDot := pos('.',language);
      if idxDot <> 0 then
      begin
        language := copy(language,1,idxDot-1);
        AConfig.Languages.Add(language);
      end;
    end;
  until FindNext(dir)<>0;
  FindClose(dir);

  updatedLanguages := TStringList.Create;
  AConfig.GetUpdatedLanguages(updatedLanguages);
  for i := 0 to updatedLanguages.Count-1 do
  begin
    if AConfig.Languages.IndexOf(updatedLanguages[i]) = -1 then
      AConfig.Languages.Add(updatedLanguages[i]);
  end;
  updatedLanguages.Free;

  AConfig.Languages.Sort;
  AConfig.Languages.Insert(0,'auto');
end;

initialization

  ActiveLanguage := DesignLanguage;

end.

