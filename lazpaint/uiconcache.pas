// SPDX-License-Identifier: GPL-3.0-only
unit UIconCache;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap;

procedure AddToCache(AFilenames: array of string; ALastModifications: array of TDateTime;
          AIconSize: integer);
function GetCachedIcon(AFilename: string; ALastModification: TDateTime; AInvalidIcon: TBGRABitmap): TBGRABitmap;
procedure StopCaching(AWait: boolean = false);
function IsCacheBusy: boolean;

implementation

uses URaw, BGRAThumbnail, UFileSystem, BGRAReadLzp, BGRABitmapTypes, BGRAWriteLzp;

const
  MaxIconCacheCount = 512;

type

  { TIconCacheThread }

  TIconCacheThread = class(TThread)
  private
    FFilenames: array of string;
    FLastModifications: array of TDateTime;
    FIconSize: integer;
    FSyncStream: TStream;
    FSyncExtension: string;
    FSyncBitmap: TBGRABitmap;
    FSyncResult: boolean;
    procedure GetStreamThumbnailSync;
  public
    constructor Create(AFilenames: array of string;
      ALastModifications: array of TDateTime; AIconSize: integer);
    procedure Execute; override;
  end;

var
  IconCache: TStringList;
  IconCacheInvalid: TStringList;
  CacheThread: TIconCacheThread;

procedure AddToCache(AFilenames: array of string;
  ALastModifications: array of TDateTime; AIconSize: integer);
begin
  if IsCacheBusy then
    raise exception.Create('Cache is busy');
  FreeAndNil(CacheThread);
  CacheThread := TIconCacheThread.Create(AFilenames, ALastModifications, AIconSize);
end;

function GetCachedIcon(AFilename: string; ALastModification: TDateTime; AInvalidIcon: TBGRABitmap): TBGRABitmap;
var
  cacheName, dummyCaption: String;
  cacheIndex: Integer;
begin
  if IsCacheBusy then exit(nil);
  cacheName := AFilename+':'+FloatToStr(ALastModification);
  cacheIndex := IconCache.IndexOf(cacheName);
  if cacheIndex <> -1 then
  begin
    TStream(IconCache.Objects[cacheIndex]).Position:= 0;
    result := TBGRABitmap.Create;
    TBGRAReaderLazPaint.LoadRLEImage(TStream(IconCache.Objects[cacheIndex]), result, dummyCaption);
    exit;
  end else
  if IconCacheInvalid.IndexOf(cacheName) <> -1 then
    exit(AInvalidIcon)
  else
    exit(nil);
end;

procedure StopCaching(AWait: boolean);
begin
  if Assigned(CacheThread) then
  begin
    CacheThread.Terminate;
    if AWait then CacheThread.WaitFor;
  end;
end;

function IsCacheBusy: boolean;
begin
  result := Assigned(CacheThread) and not CacheThread.Finished;
end;

{ TIconCacheThread }

procedure TIconCacheThread.GetStreamThumbnailSync;
begin
  FSyncResult := GetStreamThumbnail(FSyncStream, FIconSize, FIconSize,
    BGRAPixelTransparent, True, FSyncExtension, FSyncBitmap) <> nil;
end;

constructor TIconCacheThread.Create(AFilenames: array of string;
  ALastModifications: array of TDateTime; AIconSize: integer);
var
  i: Integer;
begin
  if length(AFilenames)<>length(ALastModifications) then
    raise exception.Create('Array size mismatch');
  setlength(FFilenames, length(AFilenames));
  setlength(FLastModifications, length(FFilenames));
  for i := 0 to high(FFilenames) do
  begin
    FFilenames[i] := AFilenames[i];
    FLastModifications[i] := ALastModifications[i];
  end;
  FIconSize := AIconSize;

  inherited Create(False);
end;

procedure TIconCacheThread.Execute;
var
  i, cacheIndex: Integer;
  cacheName: String;
  bmpIcon: TBGRABitmap;
  found: Boolean;
  s: TStream;
  mem: TMemoryStream;
  endTime: TDateTime;
begin
  bmpIcon := TBGRABitmap.Create;
  endTime := Now + 150/MSecsPerDay;
  for i := 0 to high(FFilenames) do
  begin
    if Terminated or (Now > endTime) then break;
    cacheName := FFilenames[i] + ':' + FloatToStr(FLastModifications[i]);
    cacheIndex := IconCache.IndexOf(cacheName);
    if cacheIndex <> -1 then Continue;
    try
      s := FileManager.CreateFileStream(FFilenames[i], fmOpenRead or fmShareDenyWrite);
      try
        if IsRawFilename(FFilenames[i]) then
        begin
          found := GetRawStreamThumbnail(s, FIconSize, FIconSize, BGRAPixelTransparent,
                                         True, bmpIcon) <> nil;
        end else
        begin
          if DetectFileFormat(s) = ifSvg then
          begin
            FSyncStream := s;
            FSyncExtension := ExtractFileExt(FFilenames[i]);
            FSyncBitmap := bmpIcon;
            Synchronize(@GetStreamThumbnailSync);
            found := FSyncResult;
          end else
            found := GetStreamThumbnail(s, FIconSize, FIconSize, BGRAPixelTransparent,
                                           True, ExtractFileExt(FFilenames[i]), bmpIcon) <> nil;
        end;
      finally
        s.Free;
      end;
    except
      found := false;
    end;
    if found then
    begin
      if IconCache.Count >= MaxIconCacheCount then IconCache.Delete(0);
      mem := TMemoryStream.Create;
      TBGRAWriterLazPaint.WriteRLEImage(mem, bmpIcon);
      IconCache.AddObject(cacheName, mem); //mem owned by IconCache
    end else
      IconCacheInvalid.Add(cacheName);
  end;
  bmpIcon.Free;
end;


initialization

  IconCache := TStringList.Create;
  IconCache.CaseSensitive := true;
  IconCache.OwnsObjects := true;
  IconCacheInvalid := TStringList.Create;
  IconCacheInvalid.CaseSensitive := true;

finalization

  StopCaching(true);
  CacheThread.Free;
  IconCacheInvalid.Free;
  IconCache.Free;

end.
