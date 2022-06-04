// SPDX-License-Identifier: GPL-3.0-only
unit ubrowseimages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  Buttons, StdCtrls, BGRAVirtualScreen, BCComboBox, BGRABitmap, BGRABitmapTypes,
  BGRAAnimatedGif, UMySLV, LazPaintType, Masks, LCLType, UFileSystem,
  UImagePreview;

type

  { TFBrowseImages }

   TFBrowseImages = class(TForm)
    ComboBox_FileExtension: TBCComboBox;
    CheckBox_UseDirectoryOnStartup: TCheckBox;
    DirectoryEdit1: TEdit;
    ToolButton_CreateFolderOrContainer: TToolButton;
    Tool_SelectDrive: TToolButton;
    ToolButtonSeparator: TToolButton;
    ToolButton_OpenSelectedFiles: TToolButton;
    vsList: TBGRAVirtualScreen;
    Edit_Filename: TEdit;
    ListBox_RecentDirs: TListBox;
    Panel3: TPanel;
    ToolBar1: TToolBar;
    ToolButton_GoUp: TToolButton;
    ToolButton_ViewBigIcon: TToolButton;
    ToolButton_ViewDetails: TToolButton;
    vsPreview: TBGRAVirtualScreen;
    ImageListToolbar: TImageList;
    ImageList128: TImageList;
    Label_Status: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    Timer1: TTimer;
    procedure ComboBox_FileExtensionChange(Sender: TObject);
    procedure DirectoryEdit1Change(Sender: TObject);
    procedure Edit_FilenameChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var {%H-}CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure ListBox_RecentDirsClick(Sender: TObject);
    procedure ShellListView1DblClick(Sender: TObject);
    procedure ShellListView1SelectItem(Sender: TObject; Item: integer;
      {%H-}Selected: Boolean);
    procedure ShellListView1OnSort(Sender: TObject);
    procedure ShellListView1OnFormatType(Sender: Tobject; var AType:string);
    procedure Timer1Timer(Sender: TObject);
    procedure ToolButton_CreateFolderOrContainerClick(Sender: TObject);
    procedure ToolButton_OpenSelectedFilesClick(Sender: TObject);
    procedure ToolButton_ViewDetailsClick(Sender: TObject);
    procedure ToolButton_GoUpClick(Sender: TObject);
    procedure ToolButton_ViewBigIconClick(Sender: TObject);
    procedure Tool_SelectDriveClick(Sender: TObject);
    function OnDeleteConfirmation({%H-}AForm:TForm; const AFiles: array of string; AContained: boolean): boolean;
  private
    function GetCurrentDirectory: string;
    function GetCurrentFullname: string;
    procedure SetCurrentDirectory(AValue: string);
    function AdaptExtension: boolean;
    procedure ShellListView1SelectionChanged(Sender: TObject);
    procedure ThemeChanged(Sender: TObject);
  private
    FLazPaintInstance: TLazPaintCustomInstance;
    FDefaultExtension: string;
    { private declarations }
    FFileExtensionFilter: string;
    FFileExtensions: array of string;
    FDefaultExtensions: string;
    FOpenButtonHint: string;
    FIsSaveDialog: boolean;
    FOverwritePrompt: boolean;
    ShellListView1: TLCShellListView;
    FUpdateListBounds: boolean;
    FInFormShow: boolean;
    FChosenImage: TImageEntry;
    FPreview: TImagePreview;
    FComputeIconCurrentItem: integer;
    FCacheComputeIconIndexes: array of integer;
    FPreviewFilename: string;
    FInShowPreview,FInHidePreview: boolean;
    FSavedDetailsViewWidth: integer;
    FLastDirectory: string;
    FFileSystems: TFileSystemArray;
    FFilename, FInitialFilename: string;
    FBmpIcon: TBGRABitmap;
    FLastBigIcon: boolean;
    FImageFileNotChecked, FImageFileUnkown, FImageFolder,
    FImageHardDrive, FImageCdRom, FImageUsbStick, FImageRamDrive, FImageNetworkDrive: TBGRABitmap;
    InFilenameChange: boolean;
    FSelectedFiles: array of string;
    FCreateFolderOrContainerCaption: string;
    function GetCurrentExtensionFilter: string;
    function GetFilterIndex: integer;
    function GetInitialFilename: string;
    function GetOpenLayerIcon: boolean;
    function GetRememberStartDirectory: boolean;
    procedure SetDefaultExtensions(AValue: string);
    procedure SetFileExtensionFilter(AValue: string);
    procedure SetFilterIndex(AValue: integer);
    procedure SetInitialFilename(AValue: string);
    procedure SetLazPaintInstance(AValue: TLazPaintCustomInstance);
    procedure SetOpenLayerIcon(AValue: boolean);
    procedure SetRememberStartDirectory(AValue: boolean);
    procedure UpdateToolButtonOpen;
    function GetAllowMultiSelect: boolean;
    function GetSelectedFile(AIndex: integer): string;
    function GetSelectedFileCount: integer;
    procedure ResetDirectory(AFocus: boolean; AForceReload: boolean = false);
    procedure ClearThumbnails;
    procedure SetAllowMultiSelect(AValue: boolean);
    procedure SetIsSaveDialog(AValue: boolean);
    procedure StartThumbnails;
    procedure SelectCurrentDir;
    procedure UpdatePreview(AFilename:string); overload;
    procedure UpdatePreview; overload;
    procedure UpdateTheme;
    procedure ShowPreview;
    procedure HidePreview;
    procedure UpdateConstraints;
    procedure ViewDetails;
    procedure ViewBigIcons;
    procedure ValidateFileOrDir;
    procedure GoDirUp;
    procedure InitComboExt;
    procedure SetShellMask;
    procedure DeleteSelectedFiles;
    procedure SelectFile(AName: string);
    procedure SelectDefaultExtensions;
    procedure PreviewValidate({%H-}ASender: TObject);
    property CurrentFullname: string read GetCurrentFullname;
    property CurrentDirectory: string read GetCurrentDirectory write SetCurrentDirectory;
  public
    { public declarations }
    ShowRememberStartupDirectory: boolean;
    function GetChosenImage: TImageEntry;
    procedure FreeChosenImage;
    function ShowModal: Integer; override;
    property LazPaintInstance: TLazPaintCustomInstance read FLazPaintInstance write SetLazPaintInstance;
    property Filename: string read FFilename;
    property SelectedFileCount: integer read GetSelectedFileCount;
    property SelectedFile[AIndex:integer]: string read GetSelectedFile;
    property AllowMultiSelect: boolean read GetAllowMultiSelect write SetAllowMultiSelect;
    property InitialDirectory: string read FLastDirectory write FLastDirectory;
    property IsSaveDialog: boolean read FIsSaveDialog write SetIsSaveDialog;
    property OverwritePrompt: boolean read FOverwritePrompt write FOverwritePrompt;
    property DefaultExtension: string read FDefaultExtension write FDefaultExtension;
    property DefaultExtensions: string read FDefaultExtensions write SetDefaultExtensions;
    property InitialFilename: string read GetInitialFilename write SetInitialFilename;
    property CurrentExtensionFilter: string read GetCurrentExtensionFilter;
    property Filter: string read FFileExtensionFilter write SetFileExtensionFilter;
    property FilterIndex: integer read GetFilterIndex write SetFilterIndex;
    property OpenLayerIcon: boolean read GetOpenLayerIcon write SetOpenLayerIcon;
    property RememberStartDirectory: boolean read GetRememberStartDirectory write SetRememberStartDirectory;
  end;

var
  FBrowseImages: TFBrowseImages;

implementation

{$R *.lfm}

uses BGRAThumbnail, BGRAPaintNet, BGRAOpenRaster, BGRAReadLzp,
    BGRAWriteLzp, FPimage,
    Types, UResourceStrings,
    UConfig, bgrareadjpeg, FPReadJPEG,
    UFileExtensions, BGRAUTF8, LazFileUtils,
    UGraph, URaw, UDarkTheme, ShellCtrls,
    UIconCache, LCScaleDPI;

{ TFBrowseImages }

procedure TFBrowseImages.DirectoryEdit1Change(Sender: TObject);
begin
  ResetDirectory(False);
end;

procedure TFBrowseImages.Edit_FilenameChange(Sender: TObject);
var i: integer;
  txt: string;
  first: boolean;
begin
  if InFilenameChange then exit;
  InFilenameChange := true;
  txt := trim(Edit_Filename.Text);
  {$IFDEF WINDOWS}
  if (length(txt) >= 3) and (upcase(txt[1]) in ['A'..'Z']) and (txt[2]=':') and (txt[3]='\') then
  {$ELSE}
  if (length(txt) >= 1) and (txt[1] = PathDelim) then
  {$ENDIF}
  begin
    DirectoryEdit1.Text := ExtractFilePath(txt);
    txt := ExtractFileName(txt);
    Edit_Filename.Text := txt;
  end else
  if pos(PathDelim, txt) > 1 then
  begin
    DirectoryEdit1.Text := ConcatPaths([DirectoryEdit1.Text,ExtractFilePath(txt)]);
    txt := ExtractFileName(txt);
    Edit_Filename.Text := txt;
  end;
  ShellListView1.DeselectAll;
  UpdatePreview('');
  first := true;
  for i := 0 to ShellListView1.ItemCount-1 do
    if UTF8CompareText(ShellListView1.ItemName[i],txt) = 0 then
    begin
      if first then
      begin
        ShellListView1.SelectedIndex := i;
        ShellListView1.MakeItemVisible(i);
        ShellListView1SelectItem(nil, i, true);
        if not ShellListView1.ItemIsFolder[i] then
          UpdatePreview(ShellListView1.ItemFullName[i]);
      end;
      ShellListView1.ItemSelected[i] := true;
      first := false;
    end;
  if first then
  begin
    for i := 0 to ShellListView1.ItemCount-1 do
      if UTF8CompareText(ChangeFileExt(ShellListView1.ItemName[i],''),txt) = 0 then
      begin
        if first then
        begin
          ShellListView1.SelectedIndex := i;
          ShellListView1.MakeItemVisible(i);
          ShellListView1SelectItem(nil, i, true);
          UpdatePreview(ShellListView1.ItemFullName[i]);
        end;
        ShellListView1.ItemSelected[i] := true;
        first := false;
      end;
  end;
  if IsSaveDialog then UpdateToolButtonOpen;
  InFilenameChange := false;
end;

procedure TFBrowseImages.ComboBox_FileExtensionChange(Sender: TObject);
begin
  ClearThumbnails;
  SetShellMask;
  StartThumbnails;
  if IsSaveDialog then
  begin
    if not AdaptExtension then
      Edit_FilenameChange(nil);
  end else
    Edit_Filename.Text := '';
end;

procedure TFBrowseImages.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var r:TRect;
begin
  LazPaintInstance.Config.SetDefaultBrowseWindowMaximized(self.WindowState = wsMaximized);
  if self.WindowState = wsNormal then
  begin
    r.left := Left;
    r.top := Top;
    r.right := r.left+ClientWidth;
    r.Bottom := r.top+ClientHeight;
    LazPaintInstance.Config.SetDefaultBrowseWindowPosition(r);
  end
  else
    LazPaintInstance.Config.SetDefaultBrowseWindowPosition(EmptyRect);
end;

procedure TFBrowseImages.FormCreate(Sender: TObject);
var bmp : TBitmap;
begin
  FLastDirectory := '';
  FOverwritePrompt:= true;
  FOpenButtonHint:= ToolButton_OpenSelectedFiles.Hint;

  FPreview := TImagePreview.Create(vsPreview, Label_Status, true);
  FPreview.OnValidate:= @PreviewValidate;
  FChosenImage := TImageEntry.Empty;

  ScaleImageList(ImageListToolbar, DoScaleX(32, OriginalDPI),
    DoScaleY(32, OriginalDPI), ImageListToolbar);

  ScaleControl(Panel1, OriginalDPI, 0,0, true);
  ScaleControl(Panel2, OriginalDPI, 0,0, true);
  ScaleControl(Panel3, OriginalDPI, 0,0, true);

  UpdateTheme;

  vsList.BitmapAutoScale:= false;

  bmp := TBitmap.Create;
  ImageList128.GetBitmap(0,bmp);
  FImageFileNotChecked := TBGRABitmap.Create(bmp);
  ImageList128.GetBitmap(1,bmp);
  FImageFolder := TBGRABitmap.Create(bmp);
  ImageList128.GetBitmap(2,bmp);
  FImageFileUnkown := TBGRABitmap.Create(bmp);
  ImageList128.GetBitmap(3,bmp);
  FImageHardDrive := TBGRABitmap.Create(bmp);
  ImageList128.GetBitmap(4,bmp);
  FImageCdRom := TBGRABitmap.Create(bmp);
  ImageList128.GetBitmap(5,bmp);
  FImageUsbStick := TBGRABitmap.Create(bmp);
  ImageList128.GetBitmap(6,bmp);
  FImageRamDrive := TBGRABitmap.Create(bmp);
  ImageList128.GetBitmap(7,bmp);
  FImageNetworkDrive := TBGRABitmap.Create(bmp);
  bmp.Free;
  ImageList128.Clear;

  ShellListView1 := TLCShellListView.Create(vsList);
  SetShellMask;
  ShellListView1.OnDblClick := @ShellListView1DblClick;
  ShellListView1.OnSelectItem := @ShellListView1SelectItem;
  ShellListView1.OnSelectionChanged:=@ShellListView1SelectionChanged;
  ShellListView1.OnSort := @ShellListView1OnSort;
  ShellListView1.OnFormatType := @ShellListView1OnFormatType;
  ShellListView1.SelectAllAction := [otNonFolders];

  BGRAPaintNet.RegisterPaintNetFormat;
  BGRAOpenRaster.RegisterOpenRasterFormat;

  Tool_SelectDrive.Visible := FileManager.CanGetFileSystems;
  Toolbar1.AutoSize := true;

  FCreateFolderOrContainerCaption := ToolButton_CreateFolderOrContainer.Hint;
  ToolButton_CreateFolderOrContainer.Hint := ToolButton_CreateFolderOrContainer.Hint + '...';
  ComboBox_FileExtension.TabStop := true;
end;

procedure TFBrowseImages.FormDestroy(Sender: TObject);
begin
  if Assigned(FLazPaintInstance) then
    FLazPaintInstance.RegisterThemeListener(@ThemeChanged, false);
  ShellListView1.VirtualScreenFreed;
  FreeAndNil(ShellListView1);
  FreeAndNil(FChosenImage.bmp);
  FreeAndNil(FPreview);
  FreeAndNil(FBmpIcon);
  FreeAndNil(FImageFileNotChecked);
  FreeAndNil(FImageFileUnkown);
  FreeAndNil(FImageFolder);
  FreeAndNil(FImageHardDrive);
  FreeAndNil(FImageCdRom);
  FreeAndNil(FImageUsbStick);
  FreeAndNil(FImageRamDrive);
  FreeAndNil(FImageNetworkDrive);
end;

procedure TFBrowseImages.FormHide(Sender: TObject);
begin
  FCacheComputeIconIndexes := nil;
  StopCaching(true);
  BGRAThumbnail.CheckersScale:= 1;

  FLastBigIcon := (ShellListView1.ViewStyle = vsIcon);
  if not IsSaveDialog then FFilename:= FPreviewFilename;
  Timer1.Enabled := false;
  vsList.Anchors := [akLeft,akTop,akRight,akBottom];
  FLastDirectory := CurrentDirectory;
  CurrentDirectory := '';
  UpdatePreview('');
end;

procedure TFBrowseImages.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    if Edit_Filename.Focused and (trim(Edit_Filename.Text)='..') then
      GoDirUp
    else
    begin
      ValidateFileOrDir;
      ShellListView1.SetFocus;
    end;
    Key := 0;
  end else
  if (Key = VK_BACK) and not Edit_Filename.Focused and not DirectoryEdit1.Focused then
  begin
    GoDirUp;
    Key := 0;
  end else
  if (KEY = VK_DELETE) and not Edit_Filename.Focused and not DirectoryEdit1.Focused then
  begin
    DeleteSelectedFiles;
    Key := 0;
  end else
  if (KEY = VK_F5) then
  begin
    ResetDirectory(false, true);
    SelectFile(Edit_Filename.Text);
    Key := 0;
  end else
  if (KEY = VK_ESCAPE) then
  begin
    Close;
    Key := 0;
  end;
end;

procedure TFBrowseImages.FormResize(Sender: TObject);
begin
  UpdateConstraints;
end;

procedure TFBrowseImages.FormShow(Sender: TObject);
var r:TRect; i, delta: integer;
begin
  if FInFormShow then exit;
  FInFormShow:= true;

  delta := DirectoryEdit1.Left - (Toolbar1.Left + Toolbar1.Width
    + DoScaleX(4, OriginalDPI));
  DirectoryEdit1.Left := DirectoryEdit1.Left-delta;
  DirectoryEdit1.Width := DirectoryEdit1.Width+delta;

  BGRAThumbnail.CheckersScale:= GetCanvasScaleFactor;
  ShellListView1.FontHeight:= ScaleY(round(13*GetCanvasScaleFactor),OriginalDPI);
  ShellListView1.SmallIconSize := round(ScaleX(round(64*GetCanvasScaleFactor),OriginalDPI)/16)*16;
  if ShellListView1.SmallIconSize > 128 then
    ShellListView1.SmallIconSize := 128;
  ShellListView1.LargeIconSize:= ShellListView1.SmallIconSize*2;
  if ShellListView1.LargeIconSize > 192 then
    ShellListView1.LargeIconSize := 192;
  ShellListView1.DetailIconSize:= ShellListView1.SmallIconSize;

  ListBox_RecentDirs.Clear;
  for i := 0 to LazPaintInstance.Config.RecentDirectoriesCount-1 do
    ListBox_RecentDirs.Items.Add(LazPaintInstance.Config.RecentDirectory[i]);
  InFilenameChange := true;
  if not IsSaveDialog then Edit_Filename.Text := ''
  else Edit_Filename.Text := InitialFilename;
  if Filter = '' then //default filter
  begin
    if IsSaveDialog then
      Filter := GetExtensionFilter([eoWritable])
    else
      Filter := GetExtensionFilter([eoReadable]);
  end;
  FFilename := '';
  FSelectedFiles := nil;
  InFilenameChange := false;
  if Assigned(LazPaintInstance) then
  begin
    if LazPaintInstance.Config.DefaultBrowseWindowMaximized then self.WindowState := wsMaximized
      else
    begin
      self.WindowState := wsNormal;
      r := LazPaintInstance.Config.DefaultBrowseWindowPosition;
      if (r.right > r.left) and (r.bottom > r.top) then
      begin
        self.Position := poDesigned;
        self.Left := r.Left;
        self.Top := r.Top;
        self.ClientWidth := r.right-r.left;
        self.ClientHeight := r.bottom-r.top
      end;
    end;
  end;
  if FLastBigIcon then ViewBigIcons;
  if (FLastDirectory = '') or not FileManager.IsDirectory(FLastDirectory) then
    CurrentDirectory := DefaultPicturesDirectory
  else
    CurrentDirectory := FLastDirectory;
  Timer1.Enabled := true;
  vsList.Anchors := [akLeft,akTop];
  ShellListView1.SetFocus;
  FreeAndNil(FChosenImage.bmp);
  UpdatePreview;
  UpdateToolButtonOpen;
  if IsSaveDialog then
  begin
    If not AdaptExtension then
      Edit_FilenameChange(nil);
  end;
  FInFormShow:= false;
end;

procedure TFBrowseImages.FormUTF8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
begin
  if (UTF8Key <> '') and (Utf8Key[1] > #32) and not Edit_Filename.Focused and
    not DirectoryEdit1.Focused then
  begin
    SafeSetFocus(Edit_Filename);
    Edit_Filename.Text := UTF8Key;
    Edit_Filename.SelLength:= 0;
    Edit_Filename.SelStart := UTF8Length(UTF8Key);
    UTF8Key := #0;
  end;
end;

procedure TFBrowseImages.ListBox_RecentDirsClick(Sender: TObject);
begin
  if ListBox_RecentDirs.ItemIndex <> -1 then
  begin
    if ChompPathDelim(CurrentDirectory) <> ChompPathDelim(ListBox_RecentDirs.Items[ListBox_RecentDirs.ItemIndex]) then
      CurrentDirectory := AppendPathDelim(ListBox_RecentDirs.Items[ListBox_RecentDirs.ItemIndex]);
  end;
end;

procedure TFBrowseImages.ShellListView1DblClick(Sender: TObject);
begin
  ValidateFileOrDir;
end;

procedure TFBrowseImages.ShellListView1SelectItem(Sender: TObject; Item: integer;
  Selected: Boolean);
var wasTimer: boolean;
begin
  wasTimer := Timer1.Enabled;
  Timer1.Enabled := false;

  if not InFilenameChange and Selected then
  begin
    InFilenameChange := true;
    Edit_Filename.Text := ShellListView1.ItemName[Item];
    InFilenameChange := false;
  end;

  if Selected and not ShellListView1.ItemIsFolder[Item] then
    UpdatePreview(ShellListView1.ItemFullName[Item])
  else
    if FPreviewFilename = ShellListView1.ItemFullName[Item] then
      UpdatePreview('');

  Timer1.Enabled := wasTimer;
end;

procedure TFBrowseImages.ShellListView1OnSort(Sender: TObject);
begin
  FComputeIconCurrentItem := 0;
  FCacheComputeIconIndexes := nil;
  StopCaching;
end;

procedure TFBrowseImages.ShellListView1OnFormatType(Sender: Tobject;
  var AType: string);
var format: TBGRAImageFormat;
begin
  if AType = 'Folder' then AType := rsFolder else
  begin
    format := SuggestImageFormat(AType);
    if IsRawFilename('noname'+AType) then AType := 'Raw'
    else if format = ifPortableAnyMap then AType := UTF8UpperCase(copy(AType,2,length(AType)-1))
    else if format = ifPng then AType := 'PNG' //too long to write explicitely
    else if format = ifGIF then AType := 'GIF' //do not know if animated or not
    else if format = ifIco then AType := 'Icon'
    else if format = ifCur then AType := 'Cursor'
    else if format = ifSvg then AType := 'SVG'  //too long to write explicitely
    else AType := GetImageFormatName(format);
  end;
end;

procedure TFBrowseImages.Timer1Timer(Sender: TObject);
const MaxCacheComputeCount = 10;
var
  bmpIcon: TBGRABitmap;
  iconRect, shellRect:TRect;
  i,j,cacheComputeCount: Integer;
  newFilenames: array of string;
  newLastModifications: array of TDateTime;
begin
  Timer1.Enabled:= false;
  if FPreview.Filename <> FPreviewFilename then
    UpdatePreview
  else
    FPreview.HandleTimer;

  if not IsCacheBusy and (length(FCacheComputeIconIndexes) > 0) then
  begin
    //retrieve computed icons
    for i := 0 to high(FCacheComputeIconIndexes) do
    begin
      j := FCacheComputeIconIndexes[i];
      if ShellListView1.GetItemImage(j) = FImageFileNotChecked then
      begin
        bmpIcon := GetCachedIcon(ShellListView1.ItemFullName[j],
                                 ShellListView1.ItemLastModification[j],
                                 FImageFileUnkown);
        if Assigned(bmpIcon) then
          ShellListView1.SetItemImage(j, bmpIcon, bmpIcon <> FImageFileUnkown)
        else
          if j <  FComputeIconCurrentItem then
            FComputeIconCurrentItem := j;
      end;
    end;
    FCacheComputeIconIndexes := nil;
  end;

  if not IsCacheBusy and (FComputeIconCurrentItem < ShellListView1.ItemCount) then
  begin
    //queue icons to compute
    setlength(FCacheComputeIconIndexes, MaxCacheComputeCount);
    cacheComputeCount := 0;

    //compute icons for visible items
    shellRect := rect(0,0,ShellListView1.Width,ShellListView1.Height);
    for i := FComputeIconCurrentItem to ShellListView1.ItemCount-1 do
    if ShellListView1.GetItemImage(i) = FImageFileNotChecked then
    begin
      iconRect := ShellListView1.ItemDisplayRect[i];
      if IntersectRect(iconRect, iconRect, shellRect) then
      begin
        FCacheComputeIconIndexes[cacheComputeCount] := i;
        inc(cacheComputeCount);
        if cacheComputeCount = MaxCacheComputeCount then break;
      end;
    end;

    //compute icons in current display order
    while (FComputeIconCurrentItem < ShellListView1.ItemCount-1)
      and (cacheComputeCount < MaxCacheComputeCount) do
    begin
      if ShellListView1.GetItemImage(FComputeIconCurrentItem) = FImageFileNotChecked then
      begin
        FCacheComputeIconIndexes[cacheComputeCount] := FComputeIconCurrentItem;
        inc(cacheComputeCount);
      end;
      inc(FComputeIconCurrentItem);
    end;

    setlength(FCacheComputeIconIndexes, cacheComputeCount);
    newFilenames := nil;
    setlength(newFilenames, cacheComputeCount);
    newLastModifications := nil;
    setlength(newLastModifications, cacheComputeCount);
    for i := 0 to cacheComputeCount-1 do
    begin
      j := FCacheComputeIconIndexes[i];
      newFilenames[i] := ShellListView1.ItemFullName[j];
      newLastModifications[i] := ShellListView1.ItemLastModification[j];
    end;
    AddToCache(newFilenames, newLastModifications, ShellListView1.LargeIconSize);
  end;
  if FUpdateListBounds then
  begin
    vsList.SetBounds(vsList.Left, vsList.Top, Panel2.Width, Panel2.Height-Panel3.Height);
    FUpdateListBounds := false;
  end;
  ShellListView1.Update;
  Timer1.Enabled:= true;
end;

procedure TFBrowseImages.ToolButton_CreateFolderOrContainerClick(Sender: TObject);
var
  newName: String;
  newFullname: string;
begin
  if pos(PathDelim, CurrentDirectory) = 0 then exit;
  newName := InputBox(FCreateFolderOrContainerCaption, rsEnterFolderOrContainerName, '');
  if newName = '' then exit;
  if (pos(':',newName) <> 0) or (pos('\',newName) <> 0) then
    MessageDlg(rsInvalidName, mtError, [mbOK], 0) else
  begin
    newFullname := ChompPathDelim(CurrentDirectory)+PathDelim+newName;
    if FileManager.IsDirectory(newFullname) then
      MessageDlg(rsFolderOrContainerAlreadyExists, mtInformation, [mbOK], 0)
    else
    begin
      if FileManager.FileExists(newFullname) then
      begin
        if MessageDlg(rsOverwriteFile, mtConfirmation, [mbYes,mbNo], 0) = mrYes then
        begin
          try
            FileManager.DeleteFile(newFullname);
          except
            on ex:exception do
            begin
              MessageDlg(ex.Message, mtError, [mbOk], 0);
              exit;
            end;
          end;
        end
        else exit;
      end;
      try
        FileManager.CreateDirectory(newFullname);
      except
        on ex:exception do
          MessageDlg(ex.Message, mtError, [mbOk], 0);
      end;
      ResetDirectory(True,True);
      SelectFile(newName);
    end;
  end;

end;

procedure TFBrowseImages.ToolButton_OpenSelectedFilesClick(Sender: TObject);
begin
  ValidateFileOrDir;
  ShellListView1.SetFocus;
end;

procedure TFBrowseImages.ToolButton_ViewDetailsClick(Sender: TObject);
begin
  ViewDetails;
end;

procedure TFBrowseImages.ToolButton_GoUpClick(Sender: TObject);
begin
  GoDirUp;
end;

procedure TFBrowseImages.ToolButton_ViewBigIconClick(Sender: TObject);
begin
  ViewBigIcons;
end;

procedure TFBrowseImages.Tool_SelectDriveClick(Sender: TObject);
begin
  CurrentDirectory := ':';
end;

function TFBrowseImages.OnDeleteConfirmation(AForm: TForm;
  const AFiles: array of string; AContained: boolean): boolean;
begin
  if AContained then
  begin
    if length(AFiles)=1 then
      result := QuestionDlg(rsDeleteFile,rsConfirmDeleteFromContainer,mtConfirmation,[mrOK,rsOkay,mrCancel,rsCancel],0)=mrOk else
    if length(AFiles)>1 then
      result := QuestionDlg(rsDeleteFile,StringReplace(rsConfirmDeleteMultipleFromContainer,'%1',IntToStr(length(AFiles)),[]),mtConfirmation,[mrOK,rsOkay,mrCancel,rsCancel],0)=mrOk
  end else
  if length(AFiles)=1 then
    result := QuestionDlg(rsDeleteFile,rsConfirmMoveToTrash,mtConfirmation,[mrOK,rsOkay,mrCancel,rsCancel],0)=mrOk else
  if length(AFiles)>1 then
    result := QuestionDlg(rsDeleteFile,StringReplace(rsConfirmMoveMultipleToTrash,'%1',IntToStr(length(AFiles)),[]),mtConfirmation,[mrOK,rsOkay,mrCancel,rsCancel],0)=mrOk
  else
    result := true;
end;

function TFBrowseImages.GetCurrentDirectory: string;
begin
  result := DirectoryEdit1.Text;
end;

function TFBrowseImages.GetCurrentFullname: string;
begin
  result := IncludeTrailingPathDelimiter(trim(CurrentDirectory))+Edit_Filename.Text;
end;

procedure TFBrowseImages.SetCurrentDirectory(AValue: string);
begin
  DirectoryEdit1.Text := AValue;
  ResetDirectory(False);
end;

function TFBrowseImages.AdaptExtension: boolean;
begin
  If (Trim(Edit_Filename.Text) <> '') and (ComboBox_FileExtension.ItemIndex > 0) and not
    FileManager.IsDirectory(CurrentFullname) then
  begin
    Edit_Filename.Text := ApplySelectedFilterExtension(Edit_Filename.Text, '?|'+CurrentExtensionFilter,1);
    result := true;
  end
  else
    result := false;
end;

procedure TFBrowseImages.ShellListView1SelectionChanged(Sender: TObject);
begin
  UpdateToolButtonOpen;
end;

procedure TFBrowseImages.ThemeChanged(Sender: TObject);
begin
  UpdateTheme;
end;

procedure TFBrowseImages.UpdateToolButtonOpen;
var chosenFilename: string;
begin
  chosenFilename := Trim(Edit_Filename.Text);
  ToolButton_OpenSelectedFiles.Enabled := (ShellListView1.SelectedCount> 0) or (IsSaveDialog and (chosenFilename<>''));
end;

function TFBrowseImages.GetInitialFilename: string;
begin
  result := FInitialFilename;
end;

function TFBrowseImages.GetOpenLayerIcon: boolean;
begin
  result := ToolButton_OpenSelectedFiles.ImageIndex = 7;
end;

function TFBrowseImages.GetRememberStartDirectory: boolean;
begin
  result := CheckBox_UseDirectoryOnStartup.Checked;
end;

procedure TFBrowseImages.SetDefaultExtensions(AValue: string);
begin
  if FDefaultExtensions=AValue then Exit;
  FDefaultExtensions:=AValue;
  SelectDefaultExtensions;
end;

procedure TFBrowseImages.SetFileExtensionFilter(AValue: string);
begin
  if FFileExtensionFilter=AValue then Exit;
  FFileExtensionFilter:=AValue;
  InitComboExt;
end;

procedure TFBrowseImages.SetFilterIndex(AValue: integer);
begin
  if AValue < 1 then AValue := 1;
  if AValue > ComboBox_FileExtension.Items.Count then
    AValue := ComboBox_FileExtension.Items.Count;
  if AValue <> ComboBox_FileExtension.ItemIndex then
  begin
    ComboBox_FileExtension.ItemIndex := AValue-1;
    if Visible then ComboBox_FileExtensionChange(nil);
  end;
end;

function TFBrowseImages.GetCurrentExtensionFilter: string;
begin
  if (ComboBox_FileExtension.ItemIndex >= 0) and (ComboBox_FileExtension.ItemIndex < length(FFileExtensions)) then
    result := FFileExtensions[ComboBox_FileExtension.ItemIndex]
  else
    result := '*.*';
end;

function TFBrowseImages.GetFilterIndex: integer;
begin
  result := ComboBox_FileExtension.ItemIndex+1;
end;

procedure TFBrowseImages.SetInitialFilename(AValue: string);
begin
  FInitialFilename := Trim(ExtractFileName(AValue));
end;

procedure TFBrowseImages.SetLazPaintInstance(AValue: TLazPaintCustomInstance);
begin
  if FLazPaintInstance=AValue then Exit;
  if Assigned(FLazPaintInstance) then
    FLazPaintInstance.RegisterThemeListener(@ThemeChanged, false);
  FLazPaintInstance:=AValue;
  if Assigned(FLazPaintInstance) then
    FLazPaintInstance.RegisterThemeListener(@ThemeChanged, true);
  if Assigned(FPreview) then
    FPreview.LazPaintInstance := AValue;
end;

procedure TFBrowseImages.SetOpenLayerIcon(AValue: boolean);
begin
  if AValue then
    ToolButton_OpenSelectedFiles.ImageIndex := 7
  else
    ToolButton_OpenSelectedFiles.ImageIndex := 5;
end;

procedure TFBrowseImages.SetRememberStartDirectory(AValue: boolean);
begin
  CheckBox_UseDirectoryOnStartup.Checked := AValue;
end;

procedure TFBrowseImages.ResetDirectory(AFocus: boolean; AForceReload: boolean);
var newDir: string;
begin
  newDir := DirectoryEdit1.Text;
  if Assigned(ShellListView1) and ((newDir <> ShellListView1.Root) or AForceReload) then
  begin
    ClearThumbnails;
    if newDir = ShellListView1.Root then
      ShellListView1.Reload
    else
      ShellListView1.Root := newDir;
    SetShellMask;
    StartThumbnails;
    if AFocus then ShellListView1.SetFocus;
    if ShellListView1.ItemCount <> 0 then
    begin
      ShellListView1.MakeItemVisible(0);
    end;
    SelectCurrentDir;
    ToolButton_CreateFolderOrContainer.Enabled := pos(PathDelim, newDir) <> 0;
  end;
end;

function TFBrowseImages.GetSelectedFile(AIndex: integer): string;
begin
  if (AIndex < 0) or (AIndex >= length(FSelectedFiles)) then
    result := ''
  else
    result := FSelectedFiles[AIndex];
end;

function TFBrowseImages.GetAllowMultiSelect: boolean;
begin
  result := ShellListView1.AllowMultiSelect;
end;

function TFBrowseImages.GetSelectedFileCount: integer;
begin
  result := length(FSelectedFiles);
end;

procedure TFBrowseImages.ClearThumbnails;
var I: integer;
begin
  ShellListView1.BeginUpdate;
  for I := 0 to ShellListView1.ItemCount-1 do
    ShellListView1.SetItemImage(i,FImageFileNotChecked,false);
  ShellListView1.EndUpdate;
end;

procedure TFBrowseImages.SetAllowMultiSelect(AValue: boolean);
begin
  ShellListView1.AllowMultiSelect := AValue;
end;

procedure TFBrowseImages.SetIsSaveDialog(AValue: boolean);
begin
  if FIsSaveDialog=AValue then Exit;
  FIsSaveDialog:=AValue;
  if AValue then
  begin
    ToolButton_OpenSelectedFiles.ImageIndex := 4;
    ToolButton_OpenSelectedFiles.Hint := rsSaveAsButtonHint;
    AllowMultiSelect:= false;
  end else
  begin
    ToolButton_OpenSelectedFiles.ImageIndex := 5;
    ToolButton_OpenSelectedFiles.Hint := FOpenButtonHint;
  end;
end;

procedure TFBrowseImages.StartThumbnails;
var I: integer;
  t: string;
begin
  for I := 0 to ShellListView1.ItemCount-1 do
    begin
      if ShellListView1.ItemIsFolder[I] then
      begin
        t := ShellListView1.itemDevice[I];
        if t = rsFixedDrive then
          ShellListView1.SetItemImage(i,FImageHardDrive,false)
        else if t = rsCdRom then
          ShellListView1.SetItemImage(i,FImageCdRom,false)
        else if t = rsRemovableDrive then
          ShellListView1.SetItemImage(i,FImageUsbStick,false)
        else if t = rsNetworkDrive then
          ShellListView1.SetItemImage(i,FImageNetworkDrive,false)
        else if t = rsRamDisk then
          ShellListView1.SetItemImage(i,FImageRamDrive,false)
        else
          ShellListView1.SetItemImage(i,FImageFolder,false);
      end
      else
        ShellListView1.SetItemImage(i,FImageFileNotChecked,false);
    end;
  FComputeIconCurrentItem := 0;
  FCacheComputeIconIndexes := nil;
  StopCaching;
end;

procedure TFBrowseImages.SelectCurrentDir;
var I: integer;
begin
  ListBox_RecentDirs.ItemIndex := -1;
  for I := 0 to ListBox_RecentDirs.Count-1 do
    if ChompPathDelim(ListBox_RecentDirs.Items[i]) = ChompPathDelim(CurrentDirectory) then
    begin
      ListBox_RecentDirs.ItemIndex:= I;
      break;
    end;
end;

procedure TFBrowseImages.UpdatePreview(AFilename: string);
begin
  if IsSaveDialog then
    FPreviewFilename := ''
  else
    FPreviewFilename := AFilename;
end;

procedure TFBrowseImages.UpdatePreview;
begin
  if (FPreviewFilename = '') or not Panel1.Visible then
  begin
    FPreview.Filename:= '';
    vsPreview.Visible := false;
    Label_Status.Caption := rsRecentDirectories;
    ListBox_RecentDirs.Visible := true;
    CheckBox_UseDirectoryOnStartup.Left := Label_Status.Left+Label_Status.Width + 10;
    CheckBox_UseDirectoryOnStartup.Visible := ShowRememberStartupDirectory;
    SelectCurrentDir;
  end else
  begin
    ListBox_RecentDirs.Visible := false;
    CheckBox_UseDirectoryOnStartup.Visible := false;
    vsPreview.Visible := true;
    FPreview.Filename:= FPreviewFilename;
  end;
end;

procedure TFBrowseImages.UpdateTheme;
begin
  DarkThemeInstance.Apply(ComboBox_FileExtension, DarkThemeInstance.IsSystemDarkTheme, 0.40);
end;

procedure TFBrowseImages.ShowPreview;
begin
  if FInShowPreview or Panel1.Visible then exit;
  FInShowPreview := true;
  Panel1.Visible := true;
  UpdateConstraints;
  Panel2.Align := alLeft;
  Panel2.Width := FSavedDetailsViewWidth;
  Splitter1.Visible := true;
  UpdatePreview;
  FInShowPreview := false;
end;

procedure TFBrowseImages.HidePreview;
begin
  if FInHidePreview or not Panel1.Visible then exit;
  FInHidePreview:= true;
  FSavedDetailsViewWidth := Panel2.Width;
  Panel1.Visible := false;
  UpdateConstraints;
  Splitter1.Visible := false;
  Panel2.Width := ClientWidth;
  Panel2.Align := alClient;
  FInHidePreview:= false;
end;

procedure TFBrowseImages.UpdateConstraints;
begin
  if Panel1.Visible then
    Panel2.Constraints.MaxWidth := ClientWidth-Splitter1.Width-64
  else
    Panel2.Constraints.MaxWidth := 0;
  FUpdateListBounds := true;
end;

procedure TFBrowseImages.ViewDetails;
begin
  ShellListView1.BeginUpdate;
  ShellListView1.ViewStyle := vsReport;
  ShowPreview;
  ShellListView1.EndUpdate;
end;

procedure TFBrowseImages.ViewBigIcons;
begin
  ShellListView1.BeginUpdate;
  HidePreview;
  ShellListView1.ViewStyle := vsIcon;
  ShellListView1.EndUpdate;
end;

procedure TFBrowseImages.ValidateFileOrDir;
var fullName: string;
  i,selCount: integer;
begin
  selCount := 0;
  for i := 0 to ShellListView1.ItemCount-1 do
    if ShellListView1.ItemSelected[i] and not ShellListView1.ItemIsFolder[i] then inc(selCount);

  if (selCount > 0) or (ShellListView1.SelectedIndex <> -1) then
  begin
    if ShellListView1.SelectedIndex <> -1 then
      fullName := ShellListView1.ItemFullName[ShellListView1.SelectedIndex]
    else
      fullName := '';

    if (ShellListView1.SelectedIndex <> -1) and ShellListView1.ItemIsFolder[ShellListView1.SelectedIndex] then
    begin
      CurrentDirectory := fullName;
      InFilenameChange := true;
      if IsSaveDialog then
      begin
        Edit_Filename.text := InitialFilename;
        AdaptExtension;
      end
      else
        Edit_Filename.text := '';
      InFilenameChange := false;
      Edit_FilenameChange(nil);
      ShellListView1.SetFocus;
    end
    else
    if selCount > 0 then
    begin
      if IsSaveDialog and OverwritePrompt then
      begin
        if QuestionDlg(rsSave, rsOverwriteFile, mtConfirmation, [mrOk, rsOkay, mrCancel, rsCancel],0) <> mrOk then exit;
      end;
      setlength(FSelectedFiles,selCount);
      selCount := 0;
      for i := 0 to ShellListView1.ItemCount-1 do
        if ShellListView1.ItemSelected[i] and not ShellListView1.ItemIsFolder[i] then
        begin
          FSelectedFiles[selCount] := ShellListView1.ItemFullName[i];
          inc(selCount);
        end;
      if IsSaveDialog and (selCount > 0) then FFilename := FSelectedFiles[0];
      UpdatePreview(fullName);

      //if we are opening one image and its preview contains all data
      //then we can provide the loaded image / frame
      if not IsSaveDialog and (selCount = 1)
         and (FPreview.Filename = FPreviewFilename)
         and not FPreview.PreviewDataLoss then
      begin
        FChosenImage := FPreview.GetPreviewBitmap;
        if FChosenImage.bmp = nil then exit;
      end;

      if ComboBox_FileExtension.ItemIndex <> -1 then
        FDefaultExtensions := FFileExtensions[ComboBox_FileExtension.ItemIndex];
      ModalResult:= mrOk;
    end;
  end else
    if IsSaveDialog and (Trim(Edit_Filename.Text)<>'') and (CurrentDirectory <> ':') and
      FileManager.IsDirectory(trim(CurrentDirectory)) then
    begin
      FFilename:= CurrentFullname;
      if (ExtractFileExt(FFilename)='') then
      begin
        if AdaptExtension then
          FFilename:= CurrentFullname
        else
          if DefaultExtension <> '' then
            FFilename += DefaultExtension;
      end;
      if not FileManager.IsValidFileName(ExtractFileName(FFilename)) and IsSaveDialog then
      begin
        ShowMessage(rsInvalidName);
        exit;
      end;
      if FileManager.FileExists(FFilename) and IsSaveDialog and OverwritePrompt then
      begin
        if QuestionDlg(rsSave, rsOverwriteFile, mtConfirmation, [mrOk, rsOkay, mrCancel, rsCancel],0) <> mrOk then exit;
      end;
      setlength(FSelectedFiles,1);
      FSelectedFiles[0] := FFilename;
      if ComboBox_FileExtension.ItemIndex <> -1 then
        FDefaultExtensions := FFileExtensions[ComboBox_FileExtension.ItemIndex];
      ModalResult:= mrOk;
    end;
end;

procedure TFBrowseImages.GoDirUp;
var dir: string;
  itemToSelect: string;
begin
  dir := CurrentDirectory;
  FileManager.RemoveLastPathElement(dir, itemToSelect);
  if dir = '' then
  begin
    FFileSystems:= FileManager.GetFileSystems;
    if length(FFileSystems)>0 then CurrentDirectory := ':';
    itemToSelect := '';
  end else
    CurrentDirectory := dir;
  ShellListView1.SetFocus;
  UpdatePreview('');
  InFilenameChange := true;
  Edit_Filename.text := '';
  InFilenameChange := false;
  SelectFile(itemToSelect);
end;

procedure TFBrowseImages.InitComboExt;
var
  parsedExt: TStringList;
  i: integer;
begin
  parsedExt := TParseStringList.Create(FFileExtensionFilter,'|');
  setlength(FFileExtensions, parsedExt.Count div 2);
  ComboBox_FileExtension.Clear;
  for i := 0 to high(FFileExtensions) do
  begin
    FFileExtensions[i] := parsedExt[i*2+1];
    ComboBox_FileExtension.Items.Add(parsedExt[i*2]);
  end;
  parsedExt.Free;

  if ComboBox_FileExtension.Items.Count > 0 then
  begin
    ComboBox_FileExtension.ItemIndex := 0;
    SelectDefaultExtensions;
  end;
end;

procedure TFBrowseImages.SetShellMask;
begin
  if ComboBox_FileExtension.ItemIndex >= 0 then
    ShellListView1.Mask := CurrentExtensionFilter;
end;

procedure TFBrowseImages.DeleteSelectedFiles;
var filesToDelete: array of string;
  i,deleteCount: integer;
begin
  deleteCount := 0;
  for i := 0 to ShellListView1.ItemCount-1 do
    if ShellListView1.ItemSelected[i] then
    begin
      if FileManager.FileExists(ShellListView1.ItemFullName[i]) then
        inc(deleteCount)
      else
      begin
        if ShellListView1.ItemFullName[i] = FPreviewFilename then UpdatePreview('');
        ShellListView1.ItemSelected[i] := false;
        if FileManager.IsDirectory(ShellListView1.ItemFullName[i]) then
        begin
          if FileManager.IsDirectoryEmpty(ShellListView1.ItemFullName[i]) then
          begin
            try
              FileManager.DeleteDirectory(ShellListView1.ItemFullName[i]);
              ShellListView1.RemoveItemFromList(i);
            except on ex:Exception do
              MessageDlg(rsDeleteFile, ex.Message, mtError, [mbOk], 0);
            end;
          end else
            MessageDlg(rsDeleteFile, rsDirectoryNotEmpty, mtError, [mbOk], 0);
        end;
      end;
    end;

  filesToDelete := nil;
  setlength(filesToDelete, deleteCount);
  deleteCount := 0;
  for i := 0 to ShellListView1.ItemCount-1 do
    if ShellListView1.ItemSelected[i] then
    begin
      filesToDelete[deleteCount] := ShellListView1.ItemFullName[i];
      inc(deleteCount);
    end;

  if deleteCount > 0 then
  begin
    self.Enabled := false;
    FileManager.MoveToTrash(self, filesToDelete, @OnDeleteConfirmation);
    self.Enabled := true;

    for i := ShellListView1.ItemCount-1 downto 0 do
      if ShellListView1.ItemSelected[i] then
      begin
        if not FileManager.FileExists(ShellListView1.ItemFullName[i]) then
        begin
          if ShellListView1.ItemFullName[i] = FPreviewFilename then
            UpdatePreview('');
          ShellListView1.RemoveItemFromList(i);
        end;
      end;
  end;
end;

procedure TFBrowseImages.SelectFile(AName: string);
var
  idx: Integer;
begin
  idx := ShellListView1.IndexByName(AName, {$IFNDEF WINDOWS}True{$ELSE}False{$ENDIF});
  if (idx <> -1) then
  begin
    ShellListView1.SelectedIndex := idx;
    ShellListView1.MakeItemVisible(idx);
    InFilenameChange := true;
    Edit_Filename.text := ShellListView1.ItemName[idx];
    InFilenameChange := false;
  end;
end;

procedure TFBrowseImages.SelectDefaultExtensions;
var
  i: Integer;
begin
  if FDefaultExtensions <> '' then
  begin
    for i := 0 to high(FFileExtensions) do
      if FFileExtensions[i] = FDefaultExtensions then
      begin
        ComboBox_FileExtension.ItemIndex := i;
        break;
      end;
  end;
end;

procedure TFBrowseImages.PreviewValidate(ASender: TObject);
begin
  ValidateFileOrDir;
end;

function TFBrowseImages.GetChosenImage: TImageEntry;
begin
  result := FChosenImage;
  FChosenImage := TImageEntry.Empty;
end;

procedure TFBrowseImages.FreeChosenImage;
begin
  FreeAndNil(FChosenImage.bmp);
end;

function TFBrowseImages.ShowModal: Integer;
var
  mainHidden: Boolean;
begin
  mainHidden := FLazPaintInstance.Hide;
  try
    {$IFDEF LCLqt5}
    Show;
    ModalResult := mrNone;
    repeat
      Application.ProcessMessages;
      Sleep(50);
    until (ModalResult <> mrNone) or not Visible;
    if Visible then Hide;
    if ModalResult = mrNone then
      result := mrAbort
      else result := ModalResult;
    {$ELSE}
    Result:=inherited ShowModal;
    {$ENDIf}
  finally
    if mainHidden then FLazPaintInstance.Show;
  end;
end;

end.

