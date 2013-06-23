unit uupdate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, lNetComponents, lhttp, lHTTPUtil, lNet;

type

  { TFrmUpdate }

  TFrmUpdate = class(TForm)
    BtnGet: TButton;
    Label1: TLabel;
    Label2: TLabel;
    LblCurrent: TLabel;
    LblAvailable: TLabel;
    LblVersion: TLabel;
    HTTPClient: TLHTTPClientComponent;
    Shape1: TShape;
    Shape2: TShape;
    procedure BtnGetClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HTTPClientDoneInput({%H-}aSocket: TLHTTPClientSocket);
    procedure HTTPClientError(const {%H-}msg: string; {%H-}aSocket: TLSocket);
    function HTTPClientInput({%H-}aSocket: TLHTTPClientSocket; ABuffer: PChar;
      ASize: integer): integer;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FrmUpdate: TFrmUpdate;
  HTTPBuffer: string;

resourcestring
  rsUCKCannotWorkAlone='Update Checker cannot work alone!';
  rsNewVersionAvailable='New version available';
  rsHowDidYouGetThisVersion='How did you get this version?';
  rsErrorWhileCheckingVersion='Error while checking version';

implementation

uses LazHelpHTML, UTF8Process;

{$R *.lfm}

{ TFrmUpdate }

procedure OpenURL(URL: string);
var
  v: THTMLBrowserHelpViewer;
  BrowserPath, BrowserParams: string;
  p: longint;
  BrowserProcess: TProcessUTF8;
begin
  v := THTMLBrowserHelpViewer.Create(nil);
  try
    {$HINTS OFF}
    v.FindDefaultBrowser(BrowserPath, BrowserParams);
    {$HINTS ON}

    p := System.Pos('%s', BrowserParams);
    System.Delete(BrowserParams, p, 2);
    System.Insert(URL, BrowserParams, p);

    // start browser
    BrowserProcess := TProcessUTF8.Create(nil);
    try
      BrowserProcess.CommandLine := BrowserPath + ' ' + BrowserParams;
      BrowserProcess.Execute;
    finally
      BrowserProcess.Free;
    end;
  finally
    v.Free;
  end;
end;

procedure TFrmUpdate.BtnGetClick(Sender: TObject);
begin
  //open url sourceforge lazpaint files
  OpenURL('https://sourceforge.net/projects/lazpaint/files/');
end;

procedure TFrmUpdate.FormCreate(Sender: TObject);
var
  URL, aHost, aURI: string;
  aPort: word;
begin
  //Load current version in first parameter
  if Application.ParamCount >= 1 then
  begin
    LblCurrent.Caption := Application.Params[1];
    //we got the version, let's check for update :)
    HTTPClient.Method := hmGet;
    URL := 'http://lazpaint.sourceforge.net/latest.txt';
    DecomposeURL(URL, aHost, aURI, aPort);
    HTTPClient.Host := aHost;
    HTTPClient.URI := aURI;
    HTTPClient.Port := aPort;
    HTTPClient.SendRequest;
  end
  else
  begin
    MessageDlg(rsUCKCannotWorkAlone, mtError, [mbOK], 0);
    Application.Terminate;
  end;
end;

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

function IsNumeric(s: string): boolean;
var
  i: integer;
begin
  Result := (length(s) > 0);
  for i := 1 to length(s) - 1 do
    if not ((char(s[i]) in ['0'..'9']) or (char(s[i]) = '.')) then
    begin
      Result := False;
      exit;
    end;
end;

procedure TFrmUpdate.HTTPClientDoneInput(ASocket: TLHTTPClientSocket);
begin
  aSocket.Disconnect;

  HTTPBuffer := GetNumericPart(HTTPBuffer);
  if IsNumeric(HTTPBuffer) then
    LblAvailable.Caption := HTTPBuffer;

  if (LblAvailable.Caption > LblCurrent.Caption) and (LblAvailable.Caption <> '-') then
  begin
    BtnGet.Enabled := True;
    LblVersion.Font.Color := clRed;
    LblVersion.Caption := rsNewVersionAvailable;
  end;
  if (LblAvailable.Caption < LblCurrent.Caption) and (LblAvailable.Caption <> '-') then
  begin
    LblVersion.Font.Color := clRed;
    LblVersion.Caption := rsHowDidYouGetThisVersion;
  end;
  LblVersion.Visible := True;
end;

procedure TFrmUpdate.HTTPClientError(const msg: string; aSocket: TLSocket);
begin
  LblVersion.Font.Color := clRed;
  LblVersion.Caption := rsErrorWhileCheckingVersion;
  LblVersion.Visible := True;
end;

function TFrmUpdate.HTTPClientInput(ASocket: TLHTTPClientSocket;
  ABuffer: PChar; ASize: integer): integer;
var
  oldLength: dword;
begin
  oldLength := Length(HTTPBuffer);
  setlength(HTTPBuffer, oldLength + dword(ASize));
  move(ABuffer^, HTTPBuffer[oldLength + 1], ASize);

  Result := aSize; // tell the http buffer we read it all
end;

end.

