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
    function MoveToTrash(AForm: TForm; const AFilenamesUTF8: array of string; AConfirmationCallback: TDeleteConfirmationFunction): boolean;
    function CreateFileStream(AFilenameUTF8: string; AMode: Word): TStream; overload;
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
  end;

var
  FileManager: TFileManager;

implementation

uses BGRAUTF8, BGRAWinResource, BGRALazResource, LazFileUtils, Dialogs
{$IFDEF WINDOWS}, Windows{$ENDIF}
{$IFDEF LINUX}, BGRAUTF8{$ENDIF}
;

type
  TExtendedFilename = record
    Filename: string;
    SubFilename: string;
  end;

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
    result := nil;
    {$ENDIF}
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
    if not AConfirmationCallback(AForm, containedFiles, True) then exit;
    for i := 0 to high(containedFiles) do
      DeleteFile(containedFiles[i]);
  end;
  if nbRealFiles > 0 then
  begin
    {$IFDEF LINUX}
    result := MoveToTrashOnLinux(AForm, realFiles, AConfirmationCallback);
    {$ELSE}
      {$IFDEF WINDOWS}
      result := MoveToTrashOnWindows(AForm, realFiles, AConfirmationCallback);
      {$ELSE}
      result := false;
      {$ENDIF}
    {$ENDIF}
  end;
end;


type
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
    result := TFileStreamUTF8.Create(AExtendedFilename.Filename, AMode)
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

function TFileManager.CreateFileStream(AFilenameUTF8: string; AMode: Word): TStream;
begin
  result := InternalCreateFileStream(ParseExtendedFilename(AFilenameUTF8), AMode);
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

initialization

  FileManager := TFileManager.Create;

finalization

  FileManager.Free;

end.
