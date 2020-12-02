// SPDX-License-Identifier: GPL-3.0-only
unit UFileSystem;

{$mode objfpc}{$H+}{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils, UResourceStrings, LazUTF8, Forms, BGRAMultiFileType, ShellCtrls, fgl;

type
  TDeleteConfirmationFunction = function(AForm: TForm; const AFiles: array of string; AContained: boolean): boolean of object;

const
  //Windows file systems
  fsFAT  = 'FAT';      // FAT16 or FAT32
  fsNTFS = 'NTFS';     // Windows NT
  fsExFAT = 'exFAT';   // Extended FAT for flash drives

  //Removable disks file system
  fsCDFS = 'ISO 9660'; // CD-ROM
  fsUDF  = 'UDF';      // Unversal Disk Format

  //Common Linux file systems
  fsMinix = 'minix';   // Minix file system
  fsExt2 = 'ext2';     // Linux ext2
  fsExt3 = 'ext3';     // Linux ext3
  fsExt4 = 'ext4';     // Linux ext4

  //more Linux file systems
  fsSysV = 'sysv';     // Unix System V
  fsXFS = 'XFS';       //IRIX
  fsJFS = 'JFS';       //AIX journaled file system
  fsXiaFS = 'xiafs';   //extension of Minix file system
  fsReiserFS = 'Reiserfs';  //ReiserFS

  //Apple file systems
  fsAPFS = 'APFS';
  fsHFS = 'HFS';

  //Misc
  fsHPFS = 'HPFS';     // OS/2 'High Performance File System'
  fsNWFS = 'NWFS';     // Novel NetWare File System

  //Network protocols
  fsNFS = 'nfs';  // Network File System
  fsSMB = 'smb';  // Server Message Block
  rsNCPFS = 'ncpfs'; // Novel Client

type
  TFileSystemInfo = record
    name: string;
    path: string;
    device: string;
    fileSystem: string;
    longFilenames,
    caseSensitive,
    readonly: boolean;
  end;

  TFileSystemArray = array of TFileSystemInfo;

  { TFileInfo }

  TFileInfo = record
    Filename: string;
    LastModification: TDateTime;
    Size: int64;
    IsDirectory: boolean;
    class operator =(const fi1,fi2: TFileInfo): boolean;
  end;

  TFileInfoList = specialize TFPGList<TFileInfo>;

  { TFileManager }

  TFileManager = class
    function RemovePathTrail(ADir: string): string;
    procedure RemoveLastPathElement(var ADir: string; out ALastElement: string);
    function GetFileSystems: TFileSystemArray;
    function CanGetFileSystems: boolean;
    function MoveToTrash(AForm: TForm; const AFilenamesUTF8: array of string; AConfirmationCallback: TDeleteConfirmationFunction): boolean;
    function CreateFileStream(AFilenameUTF8: string; AMode: Word): TStream; overload;
    procedure CancelStreamAndFree(AStream: TStream);
    destructor Destroy; override;
    procedure GetDirectoryElements(const ABaseDir: string;
          AMask: string; AObjectTypes: TObjectTypes;
          AResult: TFileInfoList; AFileSortType: TFileSortType = fstNone);
    function IsDirectory(APathUTF8: string): boolean;
    function IsDirectoryEmpty(APathUTF8: string): boolean;
    procedure CreateDirectory(APathUTF8: string);
    function DeleteDirectory(APathUTF8: string): boolean;
    function FileExists(AFilenameUTF8: string): boolean;
    procedure DeleteFile(AFilenameUTF8: string);
    function GetValidFilename(ASuggested: string): string;
  end;

var
  FileManager: TFileManager;

implementation

uses BGRAUTF8, BGRAWinResource, BGRALazResource, LazFileUtils, Dialogs
{$IFDEF WINDOWS}, Windows{$ENDIF}
{$IFDEF LINUX}, Process{$ENDIF}
{$IFDEF DARWIN}, Process{$ENDIF};

type
  TExtendedFilename = record
    Filename: string;
    SubFilename: string;
  end;

procedure LinuxBundleToFileSystem(ABundle: string; out AFilesystem: string;
      out ALongFilenames: boolean; out ACaseSensitive: boolean);
begin
  if (ABundle = 'ntfs') or (ABundle = 'fuseblk') then AFilesystem := fsNTFS else
  if (ABundle = 'msdos') or (ABundle = 'umsdos') or (ABundle='vfat') then AFilesystem := fsFAT else
  if ABundle = 'iso9660' then AFilesystem:= fsCDFS else
  if ABundle = 'hpfs' then AFilesystem := fsHPFS else
  if ABundle = 'udf' then AFilesystem := fsUDF else
  if ABundle = 'ncp' then AFilesystem := fsNWFS else
  if ABundle = 'apfs' then AFilesystem := fsAPFS else
  if ABundle = 'hfs' then AFilesystem := fsHFS else
  if ABundle = 'exfat' then AFilesystem := fsExFAT else
    AFilesystem := ABundle;
  ALongFilenames := (ABundle <> 'minix') and (ABundle <> 'msdos');
  ACaseSensitive := (ABundle <> 'msdos') and (ABundle <> 'umsdos') and (ABundle <> 'vfat')
                and (ABundle <> 'exfat');
end;

{$IFDEF LINUX}
const LinuxFileSystems: array[0..21] of string =
 ('minix', 'ext2', 'ext3', 'ext4',
  'sysv', 'XFS', 'JFS', 'xiafs', 'Reiserfs',
  {FAT} 'msdos', 'umsdos', 'vfat', 'exfat', {NTFS} 'ntfs', 'fuseblk',
  {CDFS} 'iso9660', {UDF} 'udf',
  {HPFS} 'hpfs', {NWFS} 'ncp',
  'nfs', 'smb', 'ncpfs');

function ReadBooleanFromFile(AFilename: string): boolean;
var t: textfile;
  s: string;
begin
  assignfile(t, AFilename);
  reset(t);
  readln(t,s);
  closefile(t);
  result := trim(s)='1';
end;

function UnespacePath(APath: string): string;
var
  i, charCode: Integer;
begin
  result := APath;
  for i := length(result)-3 downto 1 do
    if (result[i]='\') and (result[i+1] in['0','1']) and
      (result[i+2] in ['0'..'9']) and (result[i+3] in ['0'..'9']) then
    begin
      charCode := (ord(result[i+3])-ord('0'))+
                  (ord(result[i+2])-ord('0'))*8+
                  (ord(result[i+1])-ord('0'))*64;
      delete(result,i+1,3);
      result[i] := chr(charCode);
    end;
end;

function GetLinuxFileSystems(AMountsFile: string): TFileSystemArray;
var mtab: TextFile;
  desc: string;
  parsedDesc: TStringList;
  lFileSystem, removableInfo, lPath: string;
  i: integer;
  found, isRemovable: boolean;
begin
  result := nil;
  parsedDesc := TStringList.Create;
  try
    AssignFile(mtab,AMountsFile);
    Reset(mtab);
    try
      while not Eof(mtab) do
      begin
        ReadLn(mtab,desc);
        parsedDesc.Delimiter := ' ';
        parsedDesc.DelimitedText := desc;
        if parsedDesc.Count >= 4 then
        begin
          lFileSystem:= parsedDesc[2];
          lPath := parsedDesc[1];
          found := false;
          for i := low(LinuxFileSystems) to high(LinuxFileSystems) do
            if LinuxFileSystems[i] = lFileSystem then
            begin
              found := true;
              break;
            end;
          if found and not lPath.StartsWith('/boot/') then
          begin
            setlength(result, length(result)+1);
            with result[high(result)] do
            begin
              fileSystem := lFileSystem;
              path := UnespacePath(parsedDesc[1]);
              device := parsedDesc[0];
              readonly:= (copy(parsedDesc[3],1,3) <> 'rw,') and (parsedDesc[3]<>'rw');

              //detecting device type
              if copy(device,1,5)='/dev/' then delete(device,1,5);
              if (copy(device,1,5) = 'disk/') or (copy(device,1,4) = 'dsk/') or
                (copy(device,1,2) = 'hd') then
              begin
                if fileSystem = 'iso9660' then
                  device := rsCdRom
                else
                  device := rsFixedDrive;
              end
              else
              if copy(device,1,2) = 'fd' then
                device := rsRemovableDrive
              else if copy(device,1,2) = 'sd' then
              begin
                removableInfo := '/sys/block/'+copy(device,1,3)+'/removable';
                if FileExists(removableInfo) then
                  isRemovable := ReadBooleanFromFile(removableInfo)
                else
                  isRemovable := false;
                if isRemovable then
                  device := rsRemovableDrive
                else
                  device := rsFixedDrive;
              end
              else if copy(device,1,3) = 'scd' then
                device := rsCdRom;
              if (fileSystem = 'nfs') or (fileSystem = 'smb') or (fileSystem = 'ncpfs') then device := rsNetworkDrive;

              //retrieving volume name
              if path = '/' then
                name := rsFileSystem
              else
                name := ExtractFileName(path);

              //formatting file system
              LinuxBundleToFileSystem(fileSystem, fileSystem, longFilenames, caseSensitive);
            end;
          end;
        end;
      end;
    finally
      CloseFile(mtab);
    end;
  except

  end;
  parsedDesc.Free;
end;
{$ENDIF}

{$IFDEF WINDOWS}
function GetWindowsFileSystems: TFileSystemArray;
var
  Drive: Char;
  DrivePath: widestring;
  lDevice: string;
  volumeName,fileSystemName: packed array[1..MAX_PATH+1] of WideChar;
  maxFilenameLength,fileSystemFlags: DWord;
begin
  result := nil;
  for Drive := 'A' to 'Z' do
  begin
    DrivePath := WideString(Drive + ':\');
    lDevice := '';
    case GetDriveTypeW(PWideChar(DrivePath)) of
     DRIVE_REMOVABLE: lDevice := rsRemovableDrive;
     DRIVE_FIXED:     lDevice := rsFixedDrive;
     DRIVE_REMOTE:    lDevice := rsNetworkDrive;
     DRIVE_CDROM:     lDevice := rsCdRom;
     DRIVE_RAMDISK:   lDevice := rsRamDisk;
    end;
    if lDevice <> '' then
    begin
      volumeName := '';
      fileSystemName := '';
      if GetVolumeInformationW(PWideChar(DrivePath), @volumeName, high(volumeName),
          nil, {%H-}maxFilenameLength, {%H-}fileSystemFlags, @fileSystemName, high(fileSystemName)) then
      begin
        setlength(result, length(result)+1);
        with result[high(result)] do
        begin
          device := lDevice;
          path := UTF8Encode(DrivePath);
          name := UTF16ToUTF8(PWideChar(@volumeName));
          fileSystem := UTF16ToUTF8(PWideChar(@fileSystemName));
          longFilenames:= maxFilenameLength >= 128;
          caseSensitive:= ((fileSystemFlags and $00000001) <> 0);
          readonly:= (fileSystemFlags and $00080000) <> 0;

          //formatting file system
          if (fileSystem = 'FAT') or (fileSystem = 'FAT32') then fileSystem := fsFAT else
          if fileSystem = 'CDFS' then fileSystem:= fsCDFS else
          if fileSystem = 'HPFS' then fileSystem:= fsHPFS else
          if fileSystem = 'UDF' then fileSystem := fsUDF else
          if fileSystem = 'NWFS' then fileSystem := fsNWFS;
        end;
      end;
    end;
  end;

end;
{$ENDIF}

{$IFDEF DARWIN}
var
  darwinFilesystemsDate: TDateTime;
  darwinFilesystemsCached: TFileSystemArray;

function GetDarwinFileSystems: TFileSystemArray;
  procedure FindDevices;
  var
    runResult, headers, curLine, fs, mountPath: string;
    lines: TStringList;
    blocksPos, mountedPos, i, endFS: integer;
    count: integer;
  begin
    if not RunCommand('df',['-P'],runResult) then exit;
    lines := TStringList.Create;
    lines.Text:= runResult;
    headers := lines[0];
    blocksPos := pos('-blocks', headers);
    mountedPos := pos('Mounted on', headers);
    if (blocksPos <> 0) and (mountedPos <> 0) then
    begin
      inc(blocksPos, 5);
      count := 0;
      setlength(result, lines.Count-1);
      for i := 1 to lines.Count-1 do
      begin
        curLine := lines[i];
        endFS := blocksPos;
        if endFS > length(curLine) then continue;
        while (endFS > 1) and (curLine[endFS] in['0'..'9']) do dec(endFS);
        while (endFS > 1) and (curLine[endFS] in[#0..#32]) do dec(endFS);
        fs := copy(curLine,1,endFS);
        if fs.StartsWith('/dev/') then
        begin
          mountPath := copy(curLine, mountedPos, length(curLine)-mountedPos+1);
          if (mountPath <> '/var/vm') and (mountPath <> '/private/var/vm') then
          begin
            result[count].path := mountPath;
            inc(count);
          end;
        end;
      end;
      setlength(result, count);
    end;
    lines.Free;
  end;

  procedure FetchDiskInfo(var fsi: TFileSystemInfo);
  var
    runResult, curLine, key, value: string;
    lines: TStringList;
    posColon: SizeInt;
    i: Integer;
  begin
    if not RunCommand('diskutil',['info',fsi.path],runResult) then exit;
    lines := TStringList.Create;
    lines.Text:= runResult;
    fsi.name:= '';
    fsi.device := '?';
    fsi.fileSystem := '?';
    fsi.longFilenames:= true;
    for i := 0 to lines.Count-1 do
    begin
      curLine := lines[i];
      posColon := pos(':',curLine);
      if posColon <> 0 then
      begin
        key := copy(curLine,1,posColon-1).TrimLeft;
        value := copy(curLine,posColon+1,length(curLine)-posColon).Trim;
        if key = 'Optical Drive Type' then fsi.device := rsCdRom else
        if (key = 'Removable Media') and (fsi.device = '?') then
        begin
          if value = 'Fixed' then fsi.device := rsFixedDrive
          else fsi.device := rsRemovableDrive;
        end else
        if key = 'Type (Bundle)' then
        begin
          LinuxBundleToFileSystem(value, fsi.fileSystem, fsi.longFilenames, fsi.caseSensitive);
        end else
        if key = 'Volume Name' then fsi.name:= value else
        if key = 'Read-Only Volume' then fsi.readonly:= value='Yes';
      end;
    end;
    lines.Free;
  end;

var i: integer;
begin
  if (darwinFilesystemsDate <> 0) and (Now < darwinFilesystemsDate + (10/(60*60*24))) then
    exit(darwinFilesystemsCached);
  result := nil;
  FindDevices;
  for i := 0 to high(result) do
    FetchDiskInfo(result[i]);
  darwinFilesystemsCached := result;
  darwinFilesystemsDate:= Now;
end;
{$ENDIF}

function TFileManager.RemovePathTrail(ADir: string): string;
begin
  if (length(ADir)>=1) and (ADir[length(ADir)]=PathDelim) then
  begin
    if (length(ADir)>=2) and (ADir[length(ADir)-1]=PathDelim) then
      result := copy(ADir,1,length(ADir)-2)
    else
      result := copy(ADir,1,length(ADir)-1);
  end
  else
    result := ADir;
end;

procedure TFileManager.RemoveLastPathElement(var ADir: string; out ALastElement: string);
var
  idx, idxEnd: Integer;
begin
  ADir := RemovePathTrail(ADir);
  idx := length(ADir);
  idxEnd := idx;
  while (idx >= 1) and (ADir[idx] <> PathDelim) do dec(idx);
  ALastElement:= copy(ADir,idx+1,idxEnd-idx);
  ADir := copy(ADir,1,idx);
end;

function TFileManager.GetFileSystems: TFileSystemArray;
begin
  {$IFDEF LINUX}
  result := GetLinuxFileSystems('/proc/mounts');
  if result = nil then result := GetLinuxFileSystems('/etc/mtab');
  {$ELSE}
    {$IFDEF WINDOWS}
    result := GetWindowsFileSystems;
    {$ELSE}
      {$IFDEF DARWIN}
      result := GetDarwinFileSystems;
      {$ELSE}
      result := nil;
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
end;

function TFileManager.CanGetFileSystems: boolean;
begin
  {$IFDEF DARWIN}
  result := true;
  {$ELSE}
  result := length(GetFileSystems)>0;
  {$ENDIF}
end;

{$IFDEF WINDOWS}
type
  {$PUSH}{$PACKRECORDS C}
  SHFILEOPSTRUCTW = record
     hwnd : HWND;
     wFunc : UINT;
     pFrom : LPCWSTR;
     pTo : LPCWSTR;
     fFlags : FILEOP_FLAGS;
     fAnyOperationsAborted : WINBOOL;
     hNameMappings : LPVOID;
     lpszProgressTitle : LPCWSTR;
  end;
  {$POP}
function SHFileOperationW(Var para1: SHFILEOPSTRUCTW):longint; stdcall; external 'shell32' name 'SHFileOperationW';

function MoveToTrashOnWindows(AForm: TForm; const AFilenamesUTF8: array of string; {%H-}AConfirmationCallback: TDeleteConfirmationFunction): boolean;
const FOF_ALLOWUNDO = $40;
      FO_DELETE = 3;
var
  struct: SHFILEOPSTRUCTW;
  errorCode: longint;
  filenamesW: unicodestring;
  i: Integer;
begin
  filenamesW := '';
  for i := 0 to high(AFilenamesUTF8) do
    filenamesW += UTF8ToUTF16(AFilenamesUTF8[i]) + #0; //this is a list of filenames, it is double terminated
  struct.hwnd := AForm.Handle;
  struct.wFunc := FO_DELETE;
  struct.pFrom := PWideChar(filenamesW);
  struct.pTo := nil;
  struct.fFlags := FOF_ALLOWUNDO;
  struct.lpszProgressTitle := nil;

  struct.fAnyOperationsAborted:= false;
  struct.hNameMappings := nil;
  errorCode := SHFileOperationW(struct);
  if errorCode = 0 then
    result := not struct.fAnyOperationsAborted
  else
    result := false;
end;
{$ENDIF}

{$IFDEF LINUX}
function MoveToTrashOnLinux(AForm: TForm; const AFilenamesUTF8: array of string; AConfirmationCallback: TDeleteConfirmationFunction): boolean;
const gvfsTrash = '/usr/bin/gvfs-trash';
      trashPut = '/usr/bin/trash-put';

  function DoTrash(prog: string): boolean;
  var p: TProcess;
    i: integer;
  begin
    result := false;
    if Assigned(AConfirmationCallback) then
    begin
      if not AConfirmationCallback(AForm, AFilenamesUTF8, False) then exit;
    end;
    try
      p := TProcess.Create(nil);
      p.Executable := prog;
      for i := 0 to high(AFilenamesUTF8) do
        p.Parameters.Add(AFilenamesUTF8[i]);
      p.Options := [poWaitOnExit];
      p.Execute;
      p.Free;
      result := true;
      for i := 0 to high(AFilenamesUTF8) do
        if FileExists(AFilenamesUTF8[i]) then result := false;
    except

    end;
  end;

begin
  if FileExists(gvfsTrash) then result := DoTrash(gvfsTrash)
  else if FileExists(trashPut) then result := DoTrash(trashPut)
  else
    result := false;
end;
{$ENDIF}

{$IFDEF DARWIN}
function RunAppleScriptLine(AScriptLine: string): boolean;
var
  p: TProcess;
begin
  p := nil;
  try
    p := TProcess.Create(nil);
    p.Executable := 'osascript';
    p.Parameters.Add('-e');
    p.Parameters.Add(AScriptLine);
    p.Options := [poWaitOnExit];
    p.Execute;
    result := true;
  except
    result := false;
  end;
  p.Free;
end;

function AppleScriptEscape(AText: string): string;
begin
  result := StringReplace(AText, '\', '\\', [rfReplaceAll]);
  result := StringReplace(result, '"', '\"', [rfReplaceAll]);
end;

function MoveToTrashOnMacOS(AForm: TForm; const AFilenamesUTF8: array of string; AConfirmationCallback: TDeleteConfirmationFunction): boolean;
var
  appleScript: String;
  i: Integer;
begin
  if length(AFilenamesUTF8) = 0 then exit(true);
  if Assigned(AConfirmationCallback) then
  begin
    if not AConfirmationCallback(AForm, AFilenamesUTF8, False) then exit(false);
  end;
  appleScript := 'tell application "Finder" to delete {';
  for i := 0 to high(AFilenamesUTF8) do
  begin
    if i > 0 then appleScript += ', ';
    appleScript += 'POSIX file "' + AppleScriptEscape(AFilenamesUTF8[i]) + '"';
  end;
  appleScript += '}';
  if not RunAppleScriptLine(appleScript) then exit(false);
  result := true;
  for i := 0 to high(AFilenamesUTF8) do
    if FileExists(AFilenamesUTF8[i]) then result := false;
end;
{$ENDIF}

function IsMultiFileContainerName(AFilenameUTF8: string): boolean;
var
  ext: String;
begin
  ext := UTF8LowerCase(ExtractFileExt(AFilenameUTF8));
  result := ((ext = '.lrs') or (ext = '.res'));
end;

function IsMultiFileContainer(AFilenameUTF8: string): boolean;
begin
  result := IsMultiFileContainerName(AFilenameUTF8) and FileExistsUTF8(AFilenameUTF8);
end;

function ParseExtendedFilename(AFilenameUTF8: string): TExtendedFilename;
var p: string;
begin
  p := ExcludeTrailingPathDelimiter(ExtractFilePath(AFilenameUTF8));
  if IsMultiFileContainer(p) then
  begin
    result.Filename:= p;
    result.SubFilename := ExtractFileName(AFilenameUTF8);
  end else
  begin //regular file
    result.Filename:= AFilenameUTF8;
    result.SubFilename := '';
  end;
end;

function TFileManager.MoveToTrash(AForm: TForm; const AFilenamesUTF8: array of string; AConfirmationCallback: TDeleteConfirmationFunction): boolean;
var
  i: integer;
  realFiles, containedFiles: array of string;
  nbRealFiles, nbContainedFiles: integer;
begin
  if length(AFilenamesUTF8) = 0 then
  begin
    result := true;
    exit;
  end;
  nbRealFiles:= 0;
  setlength(realFiles, length(AFilenamesUTF8));
  nbContainedFiles:= 0;
  setlength(containedFiles, length(AFilenamesUTF8));
  for i := 0 to high(AFilenamesUTF8) do
    if ParseExtendedFilename(AFilenamesUTF8[i]).SubFilename = '' then
    begin
      realFiles[nbRealFiles] := AFilenamesUTF8[i];
      inc(nbRealFiles);
    end else
    begin
      containedFiles[nbContainedFiles] := AFilenamesUTF8[i];
      inc(nbContainedFiles);
    end;
  setlength(realFiles, nbRealFiles);
  setlength(containedFiles, nbContainedFiles);
  if nbContainedFiles > 0 then
  begin
    if not AConfirmationCallback(AForm, containedFiles, True) then exit(false);
    try
      for i := 0 to high(containedFiles) do
        DeleteFile(containedFiles[i]);
    except on ex: exception do
      begin
        ShowMessage(ex.Message);
        exit(false);
      end;
    end;
  end;
  if nbRealFiles > 0 then
  begin
    {$IFDEF LINUX}
    result := MoveToTrashOnLinux(AForm, realFiles, AConfirmationCallback);
    {$ELSE}
      {$IFDEF WINDOWS}
      result := MoveToTrashOnWindows(AForm, realFiles, AConfirmationCallback);
      {$ELSE}
        {$IFDEF DARWIN}
        result := MoveToTrashOnMacOS(AForm, realFiles, AConfirmationCallback);
        {$ELSE}
        result := false;
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  end;
end;


type

  { TStreamOverwriter }

  TStreamOverwriter = class(TFileStreamUTF8)
  protected
    FTempFilename: string;
    FFinalFilename: string;
  public
    constructor Create(AFilename: string);
    procedure Cancel;
    destructor Destroy; override;
  end;

  TOnDestroyStreamInsideMultifile = procedure(ASender: TObject);

  { TStreamInsideMultifile }

  TStreamInsideMultifile = class(TMemoryStream)
  private
    FOnDestroy: TOnDestroyStreamInsideMultifile;
    procedure SetContainer(AValue: TMultiFileContainer);
    procedure SetOnDestroy(AValue: TOnDestroyStreamInsideMultifile);
  protected
    FContainer: TMultiFileContainer;
    FName: string;
    FExtension: string;
  public
    constructor Create(AContainer: TMultiFileContainer; AName: string; AExtension: string);
    destructor Destroy; override;
    property Container: TMultiFileContainer read FContainer write SetContainer;
    property OnDestroy: TOnDestroyStreamInsideMultifile read FOnDestroy write SetOnDestroy;
    property Name: string read FName;
    property Extension: string read FExtension;
  end;

var
  CurrentMultiFile: TMultiFileContainer;
  CurrentMultiFileName: string;
  CurrentMultiFileAge: Longint;
  CurrentStreams : array of TStreamInsideMultifile;

function LoadMultiFile(AFilenameUTF8: string): TMultiFileContainer;
begin
  case UTF8LowerCase(ExtractFileExt(AFilenameUTF8)) of
   '.res': result := TWinResourceContainer.Create(AFilenameUTF8);
   '.lrs': result := TLazResourceContainer.Create(AFilenameUTF8);
  else
    raise exception.Create('Unknown container type');
  end;
end;

procedure SetCurrentMultiFile(AFilenameUTF8: string);
var newMulti: TMultiFileContainer;
  i: Integer;
begin
  if {$IFDEF WINDOWS}UTF8CompareText(AFilenameUTF8, CurrentMultiFileName)=0{$ELSE}
     AFilenameUTF8 = CurrentMultiFileName{$ENDIF} then
  begin
    try
      if FileAgeUTF8(AFilenameUTF8) = CurrentMultiFileAge then exit;
    except
    end;
  end;

  if length(CurrentStreams)> 0 then
  begin
    MessageDlg(rsFileSystem, 'Some streams were still open when switching multifile container', mtWarning, [mbOk], 0);
    for i := 0 to high(CurrentStreams) do
      CurrentStreams[i].Container := nil;
    CurrentStreams := nil;
  end;

  newMulti := LoadMultiFile(AFilenameUTF8);
  FreeAndNil(CurrentMultiFile);
  CurrentMultiFile := newMulti;
  CurrentMultiFileName := AFilenameUTF8;
  CurrentMultiFileAge := FileAgeUTF8(AFilenameUTF8);
end;

procedure HandleStreamDestruction(ASender: TObject);
var i, j: integer;
begin
  for i := 0 to High(CurrentStreams) do
  if CurrentStreams[i] = ASender then
  begin
    for j := i to High(CurrentStreams)-1 do
      CurrentStreams[j] := CurrentStreams[j+1];
    setlength(CurrentStreams, length(CurrentStreams)-1);
    if Assigned(CurrentMultiFile) then
    begin
      try
        SetCurrentMultiFile(CurrentMultiFileName);
        CurrentMultiFile.Add(TStreamInsideMultifile(ASender).Name,TStreamInsideMultifile(ASender).Extension,TStreamInsideMultifile(ASender),true,false);
        CurrentMultiFile.SaveToFile(CurrentMultiFileName);
        CurrentMultiFileAge := FileAgeUTF8(CurrentMultiFileName);
      except
        MessageDlg(rsFileSystem, rsFileNotSaved, mtWarning, [mbOk], 0);
      end;
    end;
    exit;
  end;
end;

function InternalCreateFileStream(AExtendedFilename: TExtendedFilename; AMode: Word): TStream;
var
  name,ext: string;
  index: Integer;
begin
  if AExtendedFilename.SubFilename = '' then
  begin
    if ((AMode and not $00F0) = fmCreate) and (FileExistsUTF8(AExtendedFilename.Filename)) then
      result := TStreamOverwriter.Create(AExtendedFilename.Filename)
    else
      result := TFileStreamUTF8.Create(AExtendedFilename.Filename, AMode);
  end
  else
  begin
    SetCurrentMultiFile(AExtendedFilename.Filename);
    name := ChangeFileExt(AExtendedFilename.SubFilename,'');
    ext := ExtractFileExt(AExtendedFilename.SubFilename);
    if (length(ext)>0) and (ext[1]='.') then Delete(ext,1,1);
    index := CurrentMultiFile.IndexOf(name,ext);
    if (AMode and not $00F0) = fmOpenRead then
    begin
      if (index = -1) then
        raise exception.Create('File not found in container');
      result := TMemoryStream.Create;
      CurrentMultiFile.Entry[index].CopyTo(result);
      result.Position:= 0;
    end else
    if (AMode and not $00F0) = fmCreate then
    begin
      result := TStreamInsideMultifile.Create(CurrentMultiFile, name,ext);
      TStreamInsideMultifile(result).OnDestroy := @HandleStreamDestruction;
      setlength(CurrentStreams, length(CurrentStreams)+1);
      CurrentStreams[high(CurrentStreams)] := TStreamInsideMultifile(result);
    end else
    if (AMode and not $00F0) = fmOpenReadWrite then
    begin
      result := TStreamInsideMultifile.Create(CurrentMultiFile, name,ext);
      TStreamInsideMultifile(result).OnDestroy := @HandleStreamDestruction;
      CurrentMultiFile.Entry[index].CopyTo(result);
      result.Position:= 0;

      setlength(CurrentStreams, length(CurrentStreams)+1);
      CurrentStreams[high(CurrentStreams)] := TStreamInsideMultifile(result);
    end else
      raise exception.Create('Access mode not supported');
  end;
end;

{ TStreamOverwriter }

constructor TStreamOverwriter.Create(AFilename: string);
begin
  FTempFilename:= SysUtils.GetTempFileName(ExtractFilePath(AFilename), '');
  FFinalFilename := AFilename;
  inherited Create(FTempFilename, fmCreate);
end;

procedure TStreamOverwriter.Cancel;
begin
  FFinalFilename:= '';
end;

destructor TStreamOverwriter.Destroy;
begin
  inherited Destroy;
  if FFinalFilename <> '' then
  begin
    if FileExistsUTF8(FFinalFilename) then DeleteFileUTF8(FFinalFilename);
    RenameFileUTF8(FTempFilename, FFinalFilename);
  end else
    DeleteFileUTF8(FTempFilename);
end;

function TFileManager.CreateFileStream(AFilenameUTF8: string; AMode: Word): TStream;
begin
  result := InternalCreateFileStream(ParseExtendedFilename(AFilenameUTF8), AMode);
end;

procedure TFileManager.CancelStreamAndFree(AStream: TStream);
var
  i, j: Integer;
begin
  if AStream is TStreamInsideMultifile then
    with TStreamInsideMultifile(AStream) do
    begin
      OnDestroy:= nil;

      for i := 0 to High(CurrentStreams) do
        if CurrentStreams[i] = AStream then
        begin
          for j := i to High(CurrentStreams)-1 do
            CurrentStreams[j] := CurrentStreams[j+1];
          setlength(CurrentStreams, length(CurrentStreams)-1);
          break;
        end;
    end else
  if AStream is TStreamOverwriter then
    with TStreamOverwriter(AStream) do
    begin
      Cancel;
    end;
  AStream.Free;
end;

destructor TFileManager.Destroy;
begin
  FreeAndNil(CurrentMultiFile);
  CurrentMultiFileName := '';
  inherited Destroy;
end;

function CompareFileInfoAlphabetically(const fi1, fi2: TFileInfo): integer;
begin
  result := UTF8CompareText(fi1.Filename, fi2.Filename);
end;

function CompareFileInfoFoldersFirst(const fi1, fi2: TFileInfo): integer;
begin
  if fi1.IsDirectory then
  begin
    if fi2.IsDirectory then
      result := UTF8CompareText(fi1.Filename, fi2.Filename)
    else
      result := 1;
  end else
  begin
    if not fi2.IsDirectory then
      result := UTF8CompareText(fi1.Filename, fi2.Filename)
    else
      result := -1;
  end;
end;

{ TStreamInsideMultifile }

procedure TStreamInsideMultifile.SetContainer(AValue: TMultiFileContainer);
begin
  if FContainer=AValue then Exit;
  FContainer:=AValue;
end;

procedure TStreamInsideMultifile.SetOnDestroy(
  AValue: TOnDestroyStreamInsideMultifile);
begin
  if FOnDestroy=AValue then Exit;
  FOnDestroy:=AValue;
end;

constructor TStreamInsideMultifile.Create(AContainer: TMultiFileContainer;
  AName: string; AExtension: string);
begin
  FContainer := AContainer;
  FName := AName;
  FExtension:= AExtension;
end;

destructor TStreamInsideMultifile.Destroy;
begin
  if Assigned(FOnDestroy) then
    FOnDestroy(self);
  inherited Destroy;
end;

{ TFileInfo }

class operator TFileInfo.=(const fi1, fi2: TFileInfo): boolean;
begin
  result := fi1.Filename = fi2.Filename;
end;

function MaskAccepts(const AMask, AName, AExt: string): boolean;
var
  maskStart,maskEnd,maskDot: integer;
  currentNameMask,currentExtMask: string;

function NextMask: boolean;
begin
  maskStart := maskEnd;
  while (maskStart < length(AMask)) and (AMask[maskStart] in[';',' ']) do inc(maskStart);
  maskEnd := maskStart;
  while (maskEnd < length(AMask)) and not (AMask[maskEnd] in[';',' ']) do inc(maskEnd);
  if maskEnd > maskStart then
  begin
    maskDot := maskStart;
    while (maskDot < maskEnd) and (AMask[maskDot] <> '.') do inc(maskDot);
    currentNameMask := copy(AMask,maskStart,maskDot-maskStart);
    if maskDot < maskEnd then
      currentExtMask := copy(AMask,maskDot+1,maskEnd-(maskDot+1))
    else
      currentExtMask := '';
    result := true;
  end else
    result := false;
end;

begin
  maskStart := 1;
  maskEnd := 1;
  maskDot := 1;
  currentNameMask:= '';
  currentExtMask := '';
  if not NextMask then
    result := true
  else
  begin
    repeat
      if ((currentNameMask = '*') or (currentNameMask = AName)) and
         ((currentExtMask = '*') or (currentExtMask = AExt)) then
      begin
        result := true;
        exit;
      end;
    until not NextMask;
    result := false;
  end;
end;

procedure TFileManager.GetDirectoryElements(const ABaseDir: string; AMask: string;
  AObjectTypes: TObjectTypes; AResult: TFileInfoList; AFileSortType: TFileSortType);
var p: string;
  temp: TStringList;
  fi: TFileInfo;
  fullname: string;
  age: LongInt;
  i: Integer;
  entry: TMultiFileEntry;
begin
  if AMask = '' then AMask := '*';
  p := ExcludeTrailingPathDelimiter(ABaseDir);
  if IsMultiFileContainer(p) then
  begin
    try
      if otNonFolders in AObjectTypes then
      begin
        SetCurrentMultiFile(p);

        age := FileAgeUTF8(p);
        try
          fi.LastModification := FileDateToDateTime(age);
        except
          fi.LastModification:= Now;
        end;

        for i := 0 to CurrentMultiFile.Count-1 do
        begin
          entry := CurrentMultiFile.Entry[i];
          if entry is TCustomResourceEntry then
          begin
            if TCustomResourceEntry(entry).LanguageId <> 0 then continue;
          end;
          if MaskAccepts(AMask, entry.Name, entry.Extension) then
          begin
            fi.IsDirectory := false;
            fi.Filename := entry.Name+'.'+entry.Extension;
            fi.Size := entry.FileSize;
            AResult.Add(fi)
          end;

        end;
      end;
    except
    end;
  end else
  begin
    temp := TStringList.Create;
    temp.OwnsObjects := true;
    TCustomShellTreeView.GetFilesInDir(ABaseDir,AMask,AObjectTypes,temp,fstNone);
    for i := 0 to temp.Count-1 do
    begin
      fullname := IncludeTrailingPathDelimiter(ABaseDir)+temp[i];
      if IsMultiFileContainer(fullname) then continue;

      if AObjectTypes = [otFolders] then
        fi.IsDirectory := true
      else if not (otFolders in AObjectTypes) then
        fi.IsDirectory := false
      else
        fi.IsDirectory := DirectoryExistsUTF8(fullname);

      fi.Filename:= temp[i];

      age := FileAgeUTF8(fullname);
      if age = -1 then
        fi.LastModification:= 0
      else
      begin
        try
          fi.LastModification := FileDateToDateTime(age);
        except
          fi.LastModification:= 0;
        end;
      end;
      fi.Size := FileSizeUtf8(fullname);
      AResult.Add(fi);
    end;
    if otFolders in AObjectTypes then
    begin
      temp.Clear;
      TCustomShellTreeView.GetFilesInDir(ABaseDir,'*.res;*.Res;*.RES;*.lrs;*.Lrs;*.LRS',[otNonFolders],temp,fstNone);
      for i := 0 to temp.Count-1 do
      begin
        fullname := IncludeTrailingPathDelimiter(ABaseDir)+temp[i];
        fi.IsDirectory := true;
        fi.Filename:= temp[i];
        age := FileAgeUTF8(fullname);
        if age = -1 then
          fi.LastModification:= 0
        else
        begin
          try
            fi.LastModification := FileDateToDateTime(age);
          except
            fi.LastModification:= 0;
          end;
        end;
        fi.Size := FileSizeUtf8(fullname);
        AResult.Add(fi);
      end;
    end;
    temp.Free;
  end;
  case AFileSortType of
  fstAlphabet: AResult.Sort(@CompareFileInfoAlphabetically);
  fstFoldersFirst: AResult.Sort(@CompareFileInfoFoldersFirst);
  end;
end;

function TFileManager.IsDirectory(APathUTF8: string): boolean;
begin
  result := IsMultiFileContainer(RemovePathTrail(APathUTF8)) or DirectoryExistsUTF8(APathUTF8);
end;

function TFileManager.IsDirectoryEmpty(APathUTF8: string): boolean;
var searchRec: TSearchRec;
begin
  if FindFirstUTF8(AppendPathDelim(APathUTF8) + '*.*', faAnyFile, searchRec) = 0 then
  repeat
    if (searchRec.Name <> '.') and (searchRec.Name <> '..') then
    begin
      result := false;
      FindCloseUTF8(searchRec);
      exit;
    end;
  until FindNextUTF8(searchRec)<>0;
  FindCloseUTF8(searchRec);
  result := true;
end;

procedure TFileManager.CreateDirectory(APathUTF8: string);
var
  str: TStream;
begin
  if not IsMultiFileContainerName(APathUTF8) then
    CreateDirUTF8(APathUTF8)
  else
  begin
    str := CreateFileStream(APathUTF8, fmCreate);
    str.Free;
  end;
end;

function TFileManager.DeleteDirectory(APathUTF8: string): boolean;
begin
  result := RemoveDirUTF8(APathUTF8);
end;

function TFileManager.FileExists(AFilenameUTF8: string): boolean;
var exFilename: TExtendedFilename;
  ext: string;
begin
  exFilename := ParseExtendedFilename(AFilenameUTF8);
  if exFilename.SubFilename = '' then
    result := FileExistsUTF8(exFilename.Filename)
  else
  begin
    SetCurrentMultiFile(exFilename.Filename);
    ext := ExtractFileExt(exFilename.SubFilename);
    if (length(ext)>0) and (ext[1]='.') then delete(ext,1,1);
    result := CurrentMultiFile.IndexOf(ChangeFileExt(exFilename.SubFilename,''),ext)<>-1;
  end;
end;

procedure TFileManager.DeleteFile(AFilenameUTF8: string);
var exFilename: TExtendedFilename;
  ext: string;
  index: integer;
begin
  exFilename := ParseExtendedFilename(AFilenameUTF8);
  if exFilename.SubFilename = '' then
    DeleteFileUTF8(exFilename.Filename)
  else
  begin
    SetCurrentMultiFile(exFilename.Filename);
    ext := ExtractFileExt(exFilename.SubFilename);
    if (length(ext)>0) and (ext[1]='.') then delete(ext,1,1);
    index := CurrentMultiFile.IndexOf(ChangeFileExt(exFilename.SubFilename,''),ext);
    if index <> -1 then
    begin
      CurrentMultiFile.Delete(index);
      CurrentMultiFile.SaveToFile(CurrentMultiFileName);
      CurrentMultiFileAge:= FileAgeUTF8(CurrentMultiFileName);
    end;
  end;
end;

function TFileManager.GetValidFilename(ASuggested: string): string;
var
  i: Integer;
begin
  result := ASuggested;
  for i := 1 to length(result) do
    case result[i] of
    '/','\',':','|': result[i] := '-';
    '?','%','*': result[i] := '_';
    '"': result[i] := '''';
    '<': result[i] := '(';
    '>': result[i] := ')';
    end;
end;

initialization

  FileManager := TFileManager.Create;

finalization

  FileManager.Free;

end.
