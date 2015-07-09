unit UFileSystem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UResourceStrings, LazUTF8, Forms;

type
  TDeleteConfirmationFunction = function(AForm: TForm; const AFiles: array of string): boolean of object;

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
  rsXFS = 'XFS';       //IRIX
  rsJFS = 'JFS';       //AIX journaled file system
  rsXiaFS = 'xiafs';   //extension of Minix file system
  fsReiserFS = 'Reiserfs';  //ReiserFS

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

function GetFileSystems: TFileSystemArray;
function MoveToTrash(AForm: TForm; const AFilenamesUTF8: array of string; AConfirmationCallback: TDeleteConfirmationFunction): boolean;

implementation

{$IFDEF WINDOWS}
uses Windows;
{$ENDIF}

{$IFDEF LINUX}
uses process;
{$ENDIF}

{$IFDEF LINUX}
const LinuxFileSystems: array[0..20] of string =
 ('minix', 'ext2', 'ext3', 'ext4',
  'sysv', 'XFS', 'JFS', 'xiafs', 'Reiserfs',
  {FAT} 'msdos', 'umsdos', 'vfat', {NTFS} 'ntfs', 'fuseblk',
  {CDFS} 'iso9660', {UDF} 'udf',
  {HPFS} 'hpfs', {NWFS} 'ncp',
  'nfs', 'smb', 'ncpfs');

function GetLinuxFileSystems(AMountsFile: string): TFileSystemArray;
var mtab: TextFile;
  desc: string;
  parsedDesc: TStringList;
  lFileSystem: string;
  i: integer;
  found: boolean;
begin
  result := nil;
  parsedDesc := TStringList.Create;
  try
    AssignFile(mtab,AMountsFile);
    try
      Reset(mtab);
      while not Eof(mtab) do
      begin
        ReadLn(mtab,desc);
        parsedDesc.Delimiter := ' ';
        parsedDesc.DelimitedText := desc;
        if parsedDesc.Count >= 4 then
        begin
          lFileSystem:= parsedDesc[2];
          found := false;
          for i := low(LinuxFileSystems) to high(LinuxFileSystems) do
            if LinuxFileSystems[i] = lFileSystem then
            begin
              found := true;
              break;
            end;
          if found then
          begin
            setlength(result, length(result)+1);
            with result[high(result)] do
            begin
              fileSystem := lFileSystem;
              path := parsedDesc[1];
              device := parsedDesc[0];
              longFilenames := (fileSystem <> 'minix') and
                (fileSystem <> 'msdos');
              caseSensitive := (fileSystem <> 'msdos') and
                (fileSystem <> 'umsdos') and (fileSystem <> 'vfat');
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
              if (copy(device,1,2) = 'fd') or (copy(device,1,2) = 'sd') then
                device := rsRemovableDrive
              else if copy(device,1,3) = 'scd' then
                device := rsCdRom;
              if (fileSystem = 'nfs') or (fileSystem = 'smb') or (fileSystem = 'ncpfs') then device := rsNetworkDrive;

              //retrieving volume name
              if path = '/' then
                name := rsFileSystem
              else
                name := ExtractFileName(path);

              //formatting file system
              if (fileSystem = 'ntfs') or (fileSystem = 'fuseblk') then fileSystem := fsNTFS else
              if (fileSystem = 'msdos') or (fileSystem = 'umsdos') or (fileSystem='vfat') then fileSystem := fsFAT else
              if fileSystem = 'iso9660' then fileSystem:= fsCDFS else
              if fileSystem = 'hpfs' then fileSystem := fsHPFS else
              if fileSystem = 'udf' then fileSystem := fsUDF else
              if fileSystem = 'ncp' then fileSystem := fsNWFS;
            end;
          end;
        end;
      end;
    except
      CloseFile(mtab);
    end;
  except
    parsedDesc.Free;
  end;
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
    DrivePath := Drive + ':\';
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
          path := DrivePath;
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

function GetFileSystems: TFileSystemArray;
begin
  {$IFDEF LINUX}
  result := GetLinuxFileSystems('/proc/mounts');
  if result = nil then result := GetLinuxFileSystems('/etc/mtab');
  {$ELSE}
    {$IFDEF WINDOWS}
    result := GetWindowsFileSystems;
    {$ELSE}
    result := nil;
    {$ENDIF}
  {$ENDIF}
end;

{$IFDEF WINDOWS}
type
  SHFILEOPSTRUCTW = packed record
     hwnd : HWND;
     wFunc : UINT;
     pFrom : LPCWSTR;
     pTo : LPCWSTR;
     fFlags : FILEOP_FLAGS;
     fAnyOperationsAborted : WINBOOL;
     hNameMappings : LPVOID;
     lpszProgressTitle : LPCWSTR;
  end;
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
  {if not AConfirmationCallback(AForm, AFilenamesUTF8) then
  begin
    result := false;
    exit;
  end;}
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
    if not AConfirmationCallback(AForm, AFilenamesUTF8) then exit;
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

function MoveToTrash(AForm: TForm; const AFilenamesUTF8: array of string; AConfirmationCallback: TDeleteConfirmationFunction): boolean;
begin
  if length(AFilenamesUTF8) = 0 then
  begin
    result := true;
    exit;
  end;
  {$IFDEF LINUX}
  result := MoveToTrashOnLinux(AForm, AFilenamesUTF8, AConfirmationCallback);
  {$ELSE}
    {$IFDEF WINDOWS}
    result := MoveToTrashOnWindows(AForm, AFilenamesUTF8, AConfirmationCallback);
    {$ELSE}
    result := false;
    {$ENDIF}
  {$ENDIF}
end;

end.
