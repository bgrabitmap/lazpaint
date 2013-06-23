program lazpaint;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cwstring, {$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,

  Forms, FileUtil, SysUtils, Inifiles, bgrabitmappack,

  LazPaintType, LazpaintInstance, LazpaintMainForm, uconfig,

  utoolbox, uchoosecolor,

  ugraph, utool, uimage, ustatehandler, uimagestate, uclipboard, umac, ucursors, ucommandline,

  uresample, unewimage, umultiimage, uradialblur, umotionblur, uemboss, ucustomblur,
  ucanvassize, ucolorintensity, ushiftcolors, ucolorize, utwirl, upixelate,

  uabout, uparse, utooldeformationgrid, utoolselect, utoolpolygon, 
  utoolfloodfill, utoolbasic, utoolphong, utooltext,

  DefaultTranslator, Translations, LCLProc, uresourcestrings, LResources,
  uobject3D, ulayerstack, UVolatileScrollBar, ublendop, uscaledpi;

function LanguagePath: string;
begin
  {$IFDEF WINDOWS}
    result:=ExtractFilePath(Application.ExeName)+'i18n'+PathDelim;
  {$ELSE}
    result:='i18n'+PathDelim;
  {$ENDIF}
end;

//translate program
procedure TranslateLazPaint(Language: string);
var
  Lang,FallbackLang,POFile: String;
begin
  if Language = '' then
  begin
    Lang:='';
    FallbackLang:='';
    LCLGetLanguageIDs(Lang,FallbackLang);
    Language := FallbackLang;
  end;
  if Language = 'en' then exit;

  POFile:=LanguagePath+'lazpaint.'+Language+'.po';
  if FileExistsUTF8(POFile) then
  begin
    LRSTranslator:=TPoTranslator.Create(POFile);
    TranslateResourceStrings(POFile);
  end;

  POFile:=LanguagePath+'lclstrconsts.'+Language+'.po';
  if FileExistsUTF8(POFile) then
    Translations.TranslateUnitResourceStrings('LCLStrConsts',POFile);
end;

//fill language list for configuration
procedure FillLanguageList(Config: TLazPaintConfig);
var
  dir : TSearchRec;
  idxDot: integer;
  language: string;
begin
  Config.Languages.Add('en');
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
        Config.Languages.Add(language);
      end;
    end;
  until FindNext(dir)<>0;
  FindClose(dir);
  Config.Languages.Sort;
  Config.Languages.Insert(0,'auto');
end;

//returns the config file to use
function GetActualConfig: TIniFile;
var
  PortableConfig: TIniFile;
  PortableConfigFilename: string;
  ActualConfigFilename: string;

begin
  ActualConfigFilename := '';

  //check if a config file path is defined
  PortableConfigFilename := ExtractFilePath(Application.ExeName)+'lazpaint.ini';
  If FileExists(PortableConfigFilename) then
  begin
    PortableConfig := TIniFile.Create(PortableConfigFilename);
    ActualConfigFilename:= PortableConfig.ReadString('General','ConfigFile','');
    if ActualConfigFilename <> '' then
      ActualConfigFilename:= ExpandFileName(ExtractFilePath(Application.ExeName)+ActualConfigFilename);
    PortableConfig.Free;
  end;

  //otherwise, use default path
  if ActualConfigFilename = '' then
  begin
    CreateDir(GetAppConfigDir(False));
    ActualConfigFilename := GetAppConfigFile(False,False);
  end;

  result := TIniFile.Create(ActualConfigFilename,True);
end;

procedure RestartApplication;
begin
  SysUtils.ExecuteProcess(Application.ExeName, '', []);
end;

var
  LazpaintApplication: TLazPaintInstance;
  ActualConfig: TIniFile;
  RestartQuery: boolean;

{$R *.res}

begin
  {// debug files
  if FileExists('_heap.txt') then
    DeleteFile('_heap.txt');
  SetHeapTraceOutput('_heap.txt'); }

  ActualConfig := GetActualConfig;
  TranslateLazPaint(ActualConfig.ReadString('General','Language',''));

  Application.Title := 'LazPaint';
  Application.Initialize;

  LazpaintApplication := TLazPaintInstance.Create;
  LazpaintApplication.UseConfig(ActualConfig);
  FillLanguageList(LazpaintApplication.Config);

  {$IFDEF WINDOWS}
    LazpaintApplication.AboutText := ReadFileToString(ExtractFilePath(Application.ExeName)+'readme.txt');
  {$ELSE}
    LazpaintApplication.AboutText := ReadFileToString('readme.txt');
  {$ENDIF}

  if not LazpaintApplication.ProcessCommandLine then
  begin
    LazpaintApplication.Show;
    Application.Run;
  end;

  RestartQuery := LazpaintApplication.RestartQuery;
  LazpaintApplication.Free;
  if RestartQuery then RestartApplication;
end.

