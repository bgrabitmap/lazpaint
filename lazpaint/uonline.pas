// SPDX-License-Identifier: GPL-3.0-only
unit UOnline;

{$mode objfpc}{$H+}
{$if defined(DARWIN) and defined(LCLcocoa)}
  {$DEFINE USE_NS_URL_REQUEST}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  UConfig, LazPaintType,
  {$IFDEF USE_NS_URL_REQUEST}ns_url_request{$ELSE}fphttpclient, opensslsockets{$ENDIF};

type
  { THttpGetThread }

  THttpGetThread = class(TThread)
  private
    FOnError: TNotifyEvent;
    FOnSuccess: TNotifyEvent;
    FUrl, FError: string;
    FBuffer: string;
    procedure NotifySuccess;
    procedure NotifyError;
  public
    constructor Create(AUrl: string);
    procedure Execute; override;
    property OnSuccess: TNotifyEvent read FOnSuccess write FOnSuccess;
    property OnError: TNotifyEvent read FOnError write FOnError;
    property Buffer: string read FBuffer;
  end;

  { TLazPaintOnlineUpdater }

  TLazPaintOnlineUpdater = class(TLazPaintCustomOnlineUpdater)
  private
    class var FOnlineVersionQueryDone: boolean;
    FConfig: TLazPaintConfig;
    FLanguagesAvailableOnline: TStringList;
    FHTTPBuffer: string;
    FUpdaterState: (usReady, usGettingVersion, usDownloadingLanguage, usDisabled);
    FDownloadedLanguageFile, FDownloadedLanguage: string;
    FThread: THttpGetThread;

    function GetURL({%H-}AURL: string): boolean;
    procedure ParseVersionInfo;
    procedure CheckDownloadLanguages;
    procedure DoOnlineVersionQuery;
    procedure SaveLanguageFile;
    procedure OnThreadSuccess(Sender: TObject);
    procedure OnThreadError(Sender: TObject);
    procedure OnThreadTerminate(Sender: TObject);
  public
    constructor Create(AConfig: TLazPaintConfig);
    destructor Destroy; override;
  end;

procedure MyHttpGet(AURL: string; ADestStream: TStream);

implementation

uses LazFileUtils, Dialogs,
    UTranslation, UFileSystem,
    base64;

const OnlineResourcesURL = 'https://gitcdn.link/repo/bgrabitmap/lazpaint/master/lazpaint/release/stable/';

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

procedure MyHttpGet(AURL: string; ADestStream: TStream);
var
  posDelim: SizeInt;
  stream64: TStringStream;
  decoder: TBase64DecodingStream;
{$IFNDEF USE_NS_URL_REQUEST}
  client: TFPHTTPClient;
{$ENDIF}
begin
  if AURL.StartsWith('data:') then
  begin
    posDelim := pos(';', AURL);
    if posDelim > 0 then
    begin
      if copy(AURL, posDelim+1, 7) = 'base64,' then
      begin
        stream64 := TStringStream.Create(AURL);
        try
          stream64.Position:= posDelim+7;
          decoder := TBase64DecodingStream.Create(stream64, bdmMIME);
          try
            ADestStream.CopyFrom(decoder, decoder.Size);
            exit;
          finally
            decoder.Free;
          end;
        finally
          stream64.Free;
        end;
      end;
    end;
    raise exception.Create('Invalid data URL format');
  end else
  {$IFDEF USE_NS_URL_REQUEST}
  begin
    TNSHTTPSendAndReceive.SimpleGet(AURL, ADestStream);
  end;
  {$ELSE}
  begin
    client := TFPHTTPClient.Create(nil);
    try
      client.KeepConnection:= false;
      client.AllowRedirect:= true;
      client.Get(AURL, ADestStream);
    finally
      client.Free;
    end;
  end;
  {$ENDIF}
end;

{ THttpGetThread }

procedure THttpGetThread.NotifySuccess;
begin
  if Assigned(OnSuccess) then OnSuccess(self);
end;

procedure THttpGetThread.NotifyError;
begin
  if Assigned(OnError) then OnError(self);
end;

constructor THttpGetThread.Create(AUrl: string);
begin
  inherited Create(True);
  FUrl:= AUrl;
  FreeOnTerminate := true;
end;

procedure THttpGetThread.Execute;
var stream: TMemoryStream;
begin
  stream := TMemoryStream.Create;
  try
    MyHttpGet(FUrl, stream);
    setlength(FBuffer, stream.Size);
    stream.Position:= 0;
    stream.Read(FBuffer[1], length(FBuffer));
    Synchronize(@NotifySuccess);
  except
    on ex:exception do
    begin
      FError := ex.Message;
      Synchronize(@NotifyError);
    end;
  end;
  stream.Free;
end;

procedure TLazPaintOnlineUpdater.DoOnlineVersionQuery;
begin
  if FUpdaterState <> usReady then exit;

  FUpdaterState := usGettingVersion;
  if GetURL(OnlineResourcesURL+'latest.txt') then
    FOnlineVersionQueryDone := true
  else
    FUpdaterState := usReady;
end;

procedure TLazPaintOnlineUpdater.SaveLanguageFile;
var
  stream: TStream;
begin
  if (FUpdaterState = usDownloadingLanguage) then
  begin
    if (FHTTPBuffer <> '') and (FDownloadedLanguageFile <> '') and
     (copy(FHTTPBuffer,1,8)='msgid ""') then
    begin
      try
        if FileManager.FileExists(ActualConfigDirUTF8+FDownloadedLanguageFile) then
          FileManager.DeleteFile(ActualConfigDirUTF8+FDownloadedLanguageFile);
        stream := FileManager.CreateFileStream(ActualConfigDirUTF8+FDownloadedLanguageFile,fmCreate);
        try
          stream.Write(FHTTPBuffer[1],length(FHTTPBuffer));
        finally
          stream.Free;
        end;
      except

      end;
      //even if there was an error, we consider that it has been downloaded to avoid downloading it again
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
    If Assigned(OnLatestVersionUpdate) then
       OnLatestVersionUpdate(latestVersion);
  end;
  if latestVersion = LazPaintVersionStr then
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
begin
  if not Assigned(FThread) then
  begin
    FThread := THttpGetThread.Create(AUrl);
    FThread.OnSuccess := @OnThreadSuccess;
    FThread.OnError := @OnThreadError;
    FThread.OnTerminate := @OnThreadTerminate;
    FThread.Suspended := false;
    result := true;
  end else
    result := false;
end;

procedure TLazPaintOnlineUpdater.OnThreadSuccess(Sender: TObject);
begin
  FHTTPBuffer := FThread.Buffer;
end;

procedure TLazPaintOnlineUpdater.OnThreadError(Sender: TObject);
begin
  FUpdaterState:= usDisabled;
end;

procedure TLazPaintOnlineUpdater.OnThreadTerminate(Sender: TObject);
begin
  FThread := nil;
  case FUpdaterState of
  usGettingVersion:
  begin
    if FHTTPBuffer = '' then
      FUpdaterState:= usDisabled
    else
      ParseVersionInfo;
  end;
  usDownloadingLanguage:
  begin
    if FHTTPBuffer = '' then
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
  if not FOnlineVersionQueryDone then
  begin
    FUpdaterState:= usReady;
    DoOnlineVersionQuery
  end
  else
    FUpdaterState:= usDisabled;
end;

destructor TLazPaintOnlineUpdater.Destroy;
begin
  FreeAndNil(FThread);
  FreeAndNil(FLanguagesAvailableOnline);
  inherited Destroy;
end;

end.

