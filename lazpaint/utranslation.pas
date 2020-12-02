// SPDX-License-Identifier: GPL-3.0-only
unit UTranslation;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef Darwin}
  MacOSAll,
 {$endif}
  Classes, SysUtils, UConfig, IniFiles;

{*************** Language ****************}
const
  DesignLanguage = 'en';
  {$ifdef Darwin}
  BundleResourcesDirectory = '/Contents/Resources/';
  {$endif}
var
  ActiveLanguage: string;

function FallbackLanguage: string;

{*************** Language files ************}
function LanguagePathUTF8: string;
function LazPaintLanguageFile(ALanguage: string): string;

{*************** Translation ***************}
procedure FillLanguageList(AConfig: TLazPaintConfig);
procedure TranslateLazPaint(AConfig: TIniFile);
function GetResourcePath(AResource: string): string;
function DoTranslate(AId, AText: string): string;
function AppendQuestionMark(AText: string): string;

implementation

uses Forms, LCLProc, LazUTF8, BGRAUTF8, LCLTranslator, LResources, Translations,
  LazPaintType, LCVectorOriginal;

var
  FMainPoFile: TPOFile;

{$ifdef Darwin}
function GetDarwinResourcesPath: string;
var
  pathRef: CFURLRef;
  pathCFStr: CFStringRef;
  pathStr: shortstring;

begin
  pathRef := CFBundleCopyBundleURL(CFBundleGetMainBundle());
  pathCFStr := CFURLCopyFileSystemPath(pathRef, kCFURLPOSIXPathStyle);
  CFStringGetPascalString(pathCFStr, @pathStr, 255, CFStringGetSystemEncoding());
  CFRelease(pathRef);
  CFRelease(pathCFStr);

  Result := pathStr + BundleResourcesDirectory;
end;
{$endif}

{$ifdef Linux}
function GetLinuxResourcesPath: string;
var
  binPath: String;
begin
  binPath := ExtractFilePath(Application.ExeName);
  result := ExpandFileName(binPath+'..'+PathDelim+'share'+PathDelim+'lazpaint'+PathDelim);
end;
{$endif}

function GetResourcePath(AResource: string): string;
begin
  {$IFDEF WINDOWS}
    result:=SysToUTF8(ExtractFilePath(Application.ExeName))+AResource+PathDelim;
  {$ELSE}
    {$IFDEF DARWIN}
    if DirectoryExists(GetDarwinResourcesPath+AResource) then
      result := GetDarwinResourcesPath+AResource+PathDelim
    else
    {$ELSE}
      {$IFDEF LINUX}
      if DirectoryExists(GetLinuxResourcesPath+AResource) then
        result := GetLinuxResourcesPath+AResource+PathDelim
      else
      {$ENDIF}
    {$ENDIF}
    result:=ExtractFilePath(Application.ExeName)+AResource+PathDelim;
  {$ENDIF}
end;

function DoTranslate(AId, AText: string): string;
var
  item: TPOFileItem;
begin
  if Assigned(FMainPoFile) then
  begin
    item := FMainPoFile.FindPoItem(AId);
    if (AId <> '') and Assigned(item) then
      exit(item.Translation);

    item := FMainPoFile.OriginalToItem(AText);
    if Assigned(item) then exit(item.Translation);

    item := FMainPoFile.OriginalToItem(AText+':');
    if item = nil then item := FMainPoFile.OriginalToItem(AText+' :');
    if Assigned(item) then
    begin
      result := item.Translation.TrimRight;
      if result.EndsWith(':') then
      begin
        delete(result, length(result), 1);
        result := result.TrimRight;
      end;
      exit;
    end;

    item := FMainPoFile.OriginalToItem(AText+'...');
    if Assigned(item) then
    begin
      result := item.Translation.TrimRight;
      if result.EndsWith('...') then delete(result, length(result)-2, 3);
      exit;
    end;
  end;
  result := AText;
end;

function AppendQuestionMark(AText: string): string;
begin
  if ActiveLanguage = 'es' then
    result := '¿'+AText+'?'
  else if ActiveLanguage = 'ar' then
    result := AText+'؟'
  else if (ActiveLanguage = 'fr') or (ActiveLanguage = 'kab') then
    result := Atext+' ?'
  else
    result := AText+'?';
end;

function LanguagePathUTF8: string;
begin
  result := GetResourcePath('i18n');
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
  LazGetLanguageIDs(Lang,FallbackLang);
  result := FallbackLang;
end;

//translate program
procedure TranslateLazPaint(AConfig: TIniFile);
var
  POFile: String;
  Language: string;
  UpdatedLanguages: TStringList;
begin
  Language := TLazPaintConfig.ClassGetDefaultLangage(AConfig);
  if Language = 'auto' then
    Language := FallBackLanguage;
  if Language = 'en' then exit;

  UpdatedLanguages := TStringList.Create;
  TLazPaintConfig.ClassGetUpdatedLanguages(UpdatedLanguages,AConfig,LazPaintVersionStr);
  if UpdatedLanguages.IndexOf(Language)<>-1 then
    POFile:=ActualConfigDirUTF8+LazPaintLanguageFile(Language) //updated file
  else
    POFile:='';
  if (POFile='') or not FileExistsUTF8(POFile) then
    POFile:=LanguagePathUTF8+LazPaintLanguageFile(Language); //default file
  UpdatedLanguages.Free;

  if FileExistsUTF8(POFile) then
  begin
    FMainPoFile := TPOFile.Create(POFile, true);
    LRSTranslator:=TPoTranslator.Create(FMainPoFile);
    TranslateResourceStrings(FMainPoFile);
    ActiveLanguage:= Language;
  end;

  POFile:=LanguagePathUTF8+'lclstrconsts.'+Language+'.po';
  if FileExistsUTF8(POFile) then
    Translations.TranslateUnitResourceStrings('LCLStrConsts',POFile);

  POFile:=LanguagePathUTF8+'lcresourcestring.'+Language+'.po';
  if FileExistsUTF8(POFile) then
    Translations.TranslateUnitResourceStrings('LCResourceString',POFile);
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
  if FindFirstUTF8(LanguagePathUTF8+'*.po',faAnyFile,dir) = 0 then
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
  until FindNextUTF8(dir)<>0;
  FindCloseUTF8(dir);

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

