// imported from private implementation of ShellCtrls

{ TFileItem : internal helper class used for temporarily storing info in an internal TStrings component}
type
  { TFileItem }
  TFileItem = class(TObject)
  private
    FFileInfo: TSearchRec;
    FBasePath: String;
  public
    //more data to sort by size, date... etc
    isFolder: Boolean;
    constructor Create(const DirInfo: TSearchRec; ABasePath: String);
    property FileInfo: TSearchRec read FFileInfo write FFileInfo;
  end;

constructor TFileItem.Create(const DirInfo:TSearchRec; ABasePath: String);
begin
  FFileInfo := DirInfo;
  FBasePath:= ABasePath;
  isFolder:=DirInfo.Attr and FaDirectory > 0;
end;

function FilesSortAlphabet(p1, p2: Pointer): Integer;
var
  f1, f2: TFileItem;
begin
  f1:=TFileItem(p1);
  f2:=TFileItem(p2);
  Result:=CompareText(f1.FileInfo.Name, f2.FileInfo.Name);
end;

function FilesSortFoldersFirst(p1,p2: Pointer): Integer;
var
  f1, f2: TFileItem;
begin
  f1:=TFileItem(p1);
  f2:=TFileItem(p2);
  if f1.isFolder=f2.isFolder then
    Result:=FilesSortAlphabet(p1,p2)
  else begin
    if f1.isFolder then Result:=-1
    else Result:=1;
  end;

end;

procedure GetFilesInDir(const ABaseDir: string; AMask: string;
  AObjectTypes: TObjectTypes; AResult: TStrings; AFileSortType: TFileSortType;
  ACaseSensitivity: TMaskCaseSensitivity = mcsPlatformDefault);
var
  DirInfo: TSearchRec;
  FindResult, i: Integer;
  IsDirectory, IsValidDirectory, IsHidden, AddFile, UseMaskList: Boolean;
  SearchStr, ShortFilename: string;
  MaskList: TMaskList = nil;
  Files: TList;
  FileItem: TFileItem;
  MaskOptions: TMaskOptions;
  {$if defined(windows) and not defined(wince)}
  ErrMode : LongWord;
  {$endif}
begin
  {$if defined(windows) and not defined(wince)}
  // disables the error dialog, while enumerating not-available drives
  // for example listing A: path, without diskette present.
  // WARNING: Since Application.ProcessMessages is called, it might effect some operations!
  ErrMode:=SetErrorMode(SEM_FAILCRITICALERRORS or SEM_NOALIGNMENTFAULTEXCEPT or SEM_NOGPFAULTERRORBOX or SEM_NOOPENFILEERRORBOX);
  try
  {$endif}
    while (Length(AMask) > 0) and (AMask[Length(AMask)] = ';') do
      Delete(AMask, Length(AMask), 1);
    if Trim(AMask) = '' then
      AMask := AllFilesMask;
    //Use a TMaksList if more than 1 mask is specified or if MaskCaseSensitivity differs from the platform default behaviour
    UseMaskList := (Pos(';', AMask) > 0) or
                   {$ifdef NotLiteralFilenames}
                   (ACaseSensitivity = mcsCaseSensitive)
                   {$else}
                   (ACaseSensitivity = mcsCaseInsensitive)
                   {$endif}
                   ;
    if UseMaskList then
    begin
      //Disable the use of sets in the masklist.
      //this behaviour would be incompatible with the situation if no MaskList was used
      //and it would break backwards compatibilty and could raise unexpected EConvertError where it did not in the past.
      //If you need sets in the MaskList, use the OnAddItem event for that. (BB)
      MaskOptions := [moDisableSets];
      {$ifdef NotLiteralFilenames}
      if (ACaseSensitivity = mcsCaseSensitive) then
        MaskOptions := [moDisableSets, moCaseSensitive];
      {$else}
      if (ACaseSensitivity <> mcsCaseInsensitive) then
        MaskOptions := [moDisableSets, moCaseSensitive];
      {$endif}
      MaskList := TMaskList.Create(AMask, ';', MaskOptions);  //False by default
    end;

    try
      if AFileSortType = fstNone then
        Files:=nil
      else
        Files := TList.Create;

      i := 0;
      if UseMaskList then
        SearchStr := IncludeTrailingPathDelimiter(ABaseDir) + AllFilesMask
      else
        SearchStr := IncludeTrailingPathDelimiter(ABaseDir) + AMask; //single mask, let FindFirst/FindNext handle matching

      FindResult := FindFirstUTF8(SearchStr, faAnyFile, DirInfo);
      while (FindResult = 0) do
      begin
        ShortFilename := DirInfo.Name;
        IsValidDirectory := (ShortFilename <> '.') and (ShortFilename <> '..');
        //no need to call MaskListMatches (which loops through all masks) if ShortFileName is '.' or '..' since we never process this
        if ((not UseMaskList) or MaskList.Matches(DirInfo.Name)) and IsValidDirectory  then
        begin
          inc(i);
          if i = 100 then
          begin
            Application.ProcessMessages;
            i := 0;
          end;
          IsDirectory := (DirInfo.Attr and FaDirectory = FaDirectory);
          IsHidden := (DirInfo.Attr and faHidden{%H-} = faHidden{%H-});

          // First check if we show hidden files
          if IsHidden then
            AddFile := (otHidden in AObjectTypes)
          else
            AddFile := True;

          // If it is a directory, check if it is a valid one
          if IsDirectory then
            AddFile := AddFile and ((otFolders in AObjectTypes) and IsValidDirectory)
          else
            AddFile := AddFile and (otNonFolders in AObjectTypes);

          // AddFile identifies if the file is valid or not
          if AddFile then
          begin
            if Assigned(Files) then
              Files.Add(TFileItem.Create(DirInfo, ABaseDir))
            else
              AResult.AddObject(ShortFilename, TFileItem.Create(DirInfo, ABaseDir));
          end;
        end;// Filename matches the mask
        FindResult := FindNextUTF8(DirInfo);
      end; //FindResult = 0

      FindCloseUTF8(DirInfo);
    finally
      MaskList.Free;
    end;

    if Assigned(Files) then
    begin
      case AFileSortType of
        fstAlphabet:     Files.Sort(@FilesSortAlphabet);
        fstFoldersFirst: Files.Sort(@FilesSortFoldersFirst);
      end;

      for i:=0 to Files.Count-1 do
      begin
        FileItem:=TFileItem(Files[i]);
        AResult.AddObject(FileItem.FileInfo.Name, FileItem);
      end;
      //don't free the TFileItems here, they will freed by the calling routine
      Files.Free;
    end;

  {$if defined(windows) and not defined(wince)}
  finally
     SetErrorMode(ErrMode);
  end;
  {$endif}
end;


