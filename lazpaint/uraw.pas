// SPDX-License-Identifier: GPL-3.0-only
unit URaw;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPimage, BGRABitmap, BGRABitmapTypes, Controls;

type
  TRawExtension = record
    ext: string;
    brand: string;
  end;

const
  RawFileExtensions: array[0..28] of TRawExtension =
    ((ext:'3fr'; brand:'Hasselblad'),
     (ext:'ari'; brand:'Arri_Alexa'),
     (ext:'arw;srf;sr2'; brand:'Sony'),
     (ext:'bay'; brand:'Casio'),
     (ext:'braw'; brand:'Blackmagic Design'),
     (ext:'cri'; brand:'Cintel'),
     (ext:'crw;cr2;cr3'; brand:'Canon'),
     (ext:'cap;iiq;eip'; brand:'Phase_One'),
     (ext:'dcs;dcr;drf;k25;kdc'; brand:'Kodak'),
     (ext:'dng'; brand:'Adobe'),
     (ext:'erf'; brand:'Epson'),
     (ext:'fff'; brand:'Imacon/Hasselblad raw'),
     (ext:'gpr'; brand:'GoPro'),
     (ext:'mef'; brand:'Mamiya'),
     (ext:'mdc'; brand:'Minolta, Agfa'),
     (ext:'mos'; brand:'Leaf'),
     (ext:'mrw'; brand:'Minolta, Konica Minolta'),
     (ext:'nef;nrw'; brand:'Nikon'),
     (ext:'orf'; brand:'Olympus'),
     (ext:'pef;ptx'; brand:'Pentax'),
     (ext:'pxn'; brand:'Logitech'),
     (ext:'R3D'; brand:'RED Digital Cinema'),
     (ext:'raf'; brand:'Fuji'),
     (ext:'raw'; brand:'Panasonic/Leica'),
     (ext:'rw2'; brand:'Panasonic'),
     (ext:'rwl;dng'; brand:'Leica'),
     (ext:'rwz'; brand:'Rawzor'),
     (ext:'srw'; brand:'Samsung'),
     (ext:'x3f'; brand:'Sigma'));

var
  AllRawExtensions: string;

function GetRawStreamThumbnail(AStream: TStream; AWidth,AHeight: integer;
  ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap= nil): TBGRABitmap;
function GetRawStreamImage(AStream: TStream): TBGRABitmap;
function IsRawFilename(AFilename: string): boolean;
function GetRawFileImage(AFilename: string): TBGRABitmap;

implementation

uses process, BGRAThumbnail, UResourceStrings, UFileSystem, Forms, LazFileUtils;

var
  RawCriticalSection: TRTLCriticalSection;

function GetAllRawExtensions: string;
var
  i: Integer;
begin
  result := '';
  for i := low(RawFileExtensions) to high(RawFileExtensions) do
  begin
    if result <> '' then result += ';';
    result += RawFileExtensions[i].ext;
  end;
end;

procedure RunDCRaw(AOptions: array of string;
  AInputStream, AOutputStream: TStream);
var
  tempName,tempOutName: String;
  s: TFileStream;
  p: TProcess;
  available: DWord;
  i: Integer;
  consoleOut, tiffOut: boolean;
begin
  tempName := '';
  p := nil;
  try

    EnterCriticalsection(RawCriticalSection);
    try
      tempName := GetTempFileName;
      s := TFileStream.Create(tempName, fmCreate);
      try
        s.CopyFrom(AInputStream, AInputStream.Size);
      finally
        s.Free;
      end;
    finally
      LeaveCriticalsection(RawCriticalSection);
    end;

    p := TProcess.Create(nil);
    try
      p.Options:= p.Options+[poStderrToOutPut, poNoConsole];
      {$IFDEF WINDOWS}
      p.CurrentDirectory:= ExtractFilePath(Application.ExeName);
      p.Executable:= 'dcraw.exe';
      if not FileExistsUTF8(p.CurrentDirectory+p.Executable) then
        raise exception.Create('Cannot find DCRaw binary');
      {$ELSE}
      p.Executable:= 'dcraw';
      {$ENDIF}

      consoleOut := false;
      tiffOut := false;
      for i := 0 to High(AOptions) do
      begin
        p.Parameters.Add(AOptions[i]);
        if AOptions[i] = '-c' then consoleOut := true;
        if AOptions[i] = '-T' then tiffOut := true;
      end;
      p.Parameters.Add(tempName);

      if consoleOut then
      begin
        p.Options:= p.Options+[poUsePipes];
        p.PipeBufferSize:= 524288;
        p.Execute;
        while p.Running do
        begin
          available:=P.Output.NumBytesAvailable;
          if available > 0 then
            AOutputStream.CopyFrom(P.Output, available)
          else
            sleep(30);
        end;
        available:=P.Output.NumBytesAvailable;
        if available > 0 then
          AOutputStream.CopyFrom(P.Output, available);
      end else
      begin
        if tiffOut then
          tempOutName := ChangeFileExt(tempName, '.tiff')
        else
          tempOutName := ChangeFileExt(tempName, '.ppm');
        p.Execute;
        try
          p.WaitOnExit;
          if not FileExists(tempOutName) then
            raise exception.Create(rsErrorDecodingRaw);
          s := TFileStream.Create(tempOutName, fmOpenRead);
          try
            AOutputStream.CopyFrom(s, s.Size);
          finally
            s.Free;
          end;
        finally
          if FileExists(tempOutName) then DeleteFile(tempOutName);
        end;
      end;
    finally
      FreeAndNil(p);
    end;
  finally
    if FileExists(tempName) then DeleteFile(tempName);
  end;
end;

function GetRawStreamThumbnail(AStream: TStream; AWidth, AHeight: integer;
  ABackColor: TBGRAPixel; ACheckers: boolean; ADest: TBGRABitmap): TBGRABitmap;
var
  thumbData: TMemoryStream;
begin
  result := nil;
  thumbData := TMemoryStream.Create;
  try
    RunDCRaw(['-c','-e'],AStream,thumbData);
    thumbData.Position:= 0;
    result := GetStreamThumbnail(thumbData, AWidth,AHeight, ABackColor,ACheckers,'',ADest);
  finally
    thumbData.Free;
  end;
end;

function GetRawStreamImage(AStream: TStream): TBGRABitmap;
var
  imageData: TMemoryStream;
  prevCursor: TCursor;
begin
  prevCursor := Screen.Cursor;
  Screen.Cursor:= crHourGlass;
  result := nil;
  imageData := TMemoryStream.Create;
  try
    RunDCRaw(['-T'],AStream,imageData);
    imageData.Position:= 0;
    result := TBGRABitmap.Create(imageData);
  finally
    imageData.Free;
    Screen.Cursor:= prevCursor;
  end;
end;

function IsRawFilename(AFilename: string): boolean;
var
  ext: String;
begin
  ext := LowerCase(ExtractFileExt(AFilename));
  delete(ext,1,1);
  result := Pos(';'+ext+';',';'+AllRawExtensions+';') <> 0;
end;

function GetRawFileImage(AFilename: string): TBGRABitmap;
var
  s: TStream;
begin
  s := FileManager.CreateFileStream(AFilename, fmOpenRead);
  result := nil;
  try
    result := GetRawStreamImage(s);
  finally
    s.Free;
  end;
end;

initialization

  AllRawExtensions := GetAllRawExtensions;
  InitCriticalSection(RawCriticalSection);

finalization

  DoneCriticalsection(RawCriticalSection);

end.

