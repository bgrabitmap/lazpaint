unit UOnline;

{$mode objfpc}{$H+}

{$IFNDEF DARWIN}
  {$DEFINE LAZPAINT_USE_LNET}
{$ENDIF}

interface

uses
  {$IFDEF LAZPAINT_USE_LNET}lNetComponents, lhttp,{$ENDIF}
  Classes, SysUtils,
  UConfig;

type

  { TLazPaintOnlineUpdater }

  TLazPaintOnlineUpdater = class
  private
    class var FOnlineVersionQueryDone: boolean;
    FConfig: TLazPaintConfig;
    FLanguagesAvailableOnline: TStringList;
    FHTTPBuffer: string;
    FUpdaterState: (usReady, usGettingVersion, usDownloadingLanguage, usDisabled);
    FDownloadedLanguageFile, FDownloadedLanguage: string;

    {$IFDEF LAZPAINT_USE_LNET}
    LHTTPClient: TLHTTPClientComponent;
    procedure LHTTPClientDoneInput(ASocket: TLHTTPClientSocket);
    function LHTTPClientInput({%H-}ASocket: TLHTTPClientSocket;
      ABuffer: pchar; ASize: integer): integer;
    {$ENDIF}

    function GetURL(AURL: string): boolean;
    procedure ReceiveHTTPBuffer;
    procedure ParseVersionInfo;
    procedure CheckDownloadLanguages;
    procedure DoOnlineVersionQuery;
    procedure SaveLanguageFile;
  public
    constructor Create(AConfig: TLazPaintConfig);
    destructor Destroy; override;
  end;

implementation

uses {$IFDEF LAZPAINT_USE_LNET}lHTTPUtil,{$ENDIF}
    FileUtil, Dialogs,
    LazPaintType, UResourceStrings, UTranslation;

const OnlineResourcesURL = 'http://lazpaint.sourceforge.net/';

function GetNumericPart(s: string): string;
var
  i: integer;
begin
  Result := '';
  for i := 1 to length(s) - 1 do
    if ((char(s[i]) in ['0'..'9']) or (char(s[i]) = '.')) then
      Result := Result + char(s[i])
    else
      exit;
end;

procedure TLazPaintOnlineUpdater.DoOnlineVersionQuery;
begin
  if FUpdaterState <> usReady then exit;

  if GetURL(OnlineResourcesURL+'latest.txt') then
  begin
    FUpdaterState := usGettingVersion;
    FOnlineVersionQueryDone := true;
  end;
end;

procedure TLazPaintOnlineUpdater.SaveLanguageFile;
var
  stream: TFileStream;
begin
  if (FUpdaterState = usDownloadingLanguage) then
  begin
    if (FHTTPBuffer <> '') and (FDownloadedLanguageFile <> '') and
     (copy(FHTTPBuffer,1,8)='msgid ""') then
    begin
      stream := TFileStream.Create(UTF8ToSys(ActualConfigDir+FDownloadedLanguageFile),fmOpenWrite or fmCreate);
      stream.Write(FHTTPBuffer[1],length(FHTTPBuffer));
      stream.Free;
      FConfig.AddUpdatedLanguage(FDownloadedLanguage);
      FUpdaterState:= usReady;
    end else
     FUpdaterState:= usDisabled;
  end;
end;

procedure TLazPaintOnlineUpdater.ParseVersionInfo;
var latestVersion,key,value: string;
  idxEq: integer;
  onlineInfo: TStringList;
  i: integer;
begin
  FUpdaterState:= usReady;
  latestVersion := GetNumericPart(FHTTPBuffer);
  if latestVersion <> FConfig.LatestVersion then
  begin
    Fconfig.SetLatestVersion(latestVersion);
    if latestVersion <> LazPaintCurrentVersionOnly then
      MessageDlg(rsLatestVersion + ' ' + latestVersion,mtInformation,[mbOK],0);
  end;
  if latestVersion = LazPaintCurrentVersionOnly then
  begin
    onlineInfo := TStringList.Create;
    onlineInfo.SetText(@FHTTPBuffer[1]);
    for i := 0 to onlineInfo.Count-1 do
    begin
      idxEq := pos('=',onlineInfo[i]);
      if idxEq = 0 then
      begin
        key := onlineinfo[i];
        value := '';
      end else
      begin
        key := trim(copy(onlineInfo[i],1,idxEq-1));
        value := trim(copy(onlineInfo[i],idxEq+1,length(onlineInfo[i])-idxEq));
      end;
      if CompareText(key, 'lang_update') = 0 then
      begin
        if FLanguagesAvailableOnline = nil then
          FLanguagesAvailableOnline := TStringList.Create;
        FLanguagesAvailableOnline.CommaText := value;
      end;
    end;
    onlineInfo.Free;
  end;
end;

procedure TLazPaintOnlineUpdater.CheckDownloadLanguages;
var
  i: integer;
  languagesAlreadyUpdated: TStringList;
begin
  if FUpdaterState <> usReady then exit;

  if (FLanguagesAvailableOnline <> nil) and (FLanguagesAvailableOnline.Count > 0) then
  begin
    languagesAlreadyUpdated := TStringList.Create;
    FConfig.GetUpdatedLanguages(languagesAlreadyUpdated);
    for i := 0 to FLanguagesAvailableOnline.Count-1 do
      if languagesAlreadyUpdated.IndexOf(FLanguagesAvailableOnline[i]) = -1 then
      begin
        FDownloadedLanguage := FLanguagesAvailableOnline[i];
        FDownloadedLanguageFile := LazPaintLanguageFile(FDownloadedLanguage);
        if GetURL(OnlineResourcesURL+'i18n/'+FDownloadedLanguageFile) then
          FUpdaterState:= usDownloadingLanguage;
        break;
      end;
    languagesAlreadyUpdated.Free;
  end;
end;

function TLazPaintOnlineUpdater.GetURL(AURL: string): boolean;
{$IFDEF LAZPAINT_USE_LNET}
var
  aHost, aURI: string;
  aPort: word;
{$ENDIF}
begin
  {$IFDEF LAZPAINT_USE_LNET}
  try
    if LHTTPClient = nil then
    begin
      LHTTPClient := TLHTTPClientComponent.Create(nil);
      LHTTPClient.OnDoneInput := @LHTTPClientDoneInput;
      LHTTPClient.OnInput:= @LHTTPClientInput;
    end;
    FHTTPBuffer := '';
    LHTTPClient.Method := hmGet;
    DecomposeURL(AURL, aHost, aURI, aPort);
    LHTTPClient.Host := aHost;
    LHTTPClient.URI := aURI;
    LHTTPClient.Port := aPort;
    LHTTPClient.SendRequest;
    result := true;
  except
    on ex: exception do
    begin
      result := false;
    end;
  end;
  {$ELSE}
    //no connection available
    result := false;
  {$ENDIF}
end;

procedure TLazPaintOnlineUpdater.ReceiveHTTPBuffer;
begin
  case FUpdaterState of
  usGettingVersion:
  begin
    if LHTTPClient.Response.Status <> hsOK then
      FUpdaterState:= usDisabled
    else
      ParseVersionInfo;
  end;
  usDownloadingLanguage:
  begin
    if LHTTPClient.Response.Status <> hsOK then
      FUpdaterState:= usDisabled
    else
      SaveLanguageFile;
  end;
  end;
  CheckDownloadLanguages;
end;

constructor TLazPaintOnlineUpdater.Create(AConfig: TLazPaintConfig);
begin
  FConfig := AConfig;
  FLanguagesAvailableOnline := nil;
  {$IFDEF LAZPAINT_USE_LNET}
  if not FOnlineVersionQueryDone then
  begin
    FUpdaterState:= usReady;
    DoOnlineVersionQuery
  end
  else
    FUpdaterState:= usDisabled;
  {$ELSE}
  FUpdaterState := usDisabled;
  {$ENDIF}
end;

destructor TLazPaintOnlineUpdater.Destroy;
begin
  {$IFDEF LAZPAINT_USE_LNET}
  if LHTTPClient <> nil then
  begin
    LHTTPClient.Disconnect(True);
    FreeAndNil(LHTTPClient);
  end;
  {$ENDIF}
  FreeAndNil(FLanguagesAvailableOnline);
  inherited Destroy;
end;

{$IFDEF LAZPAINT_USE_LNET}
procedure TLazPaintOnlineUpdater.LHTTPClientDoneInput(ASocket: TLHTTPClientSocket);
begin
  aSocket.Disconnect;
  ReceiveHTTPBuffer;
end;

function TLazPaintOnlineUpdater.LHTTPClientInput(ASocket: TLHTTPClientSocket;
  ABuffer: pchar; ASize: integer): integer;
var
  oldLength: dword;
begin
  oldLength := Length(FHTTPBuffer);
  setlength(FHTTPBuffer, oldLength + dword(ASize));
  move(ABuffer^, FHTTPBuffer[oldLength + 1], ASize);
  Result := aSize; // tell the http buffer we read it all
end;
{$ENDIF}

end.

