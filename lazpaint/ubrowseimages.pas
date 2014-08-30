unit ubrowseimages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  EditBtn, ComCtrls, ExtCtrls, Buttons, StdCtrls, BGRAVirtualScreen, BGRABitmap,
  BGRABitmapTypes, BGRAAnimatedGif, UMySLV, LazPaintType, LCLType;

const
  MaxIconCacheCount = 512;

type

  { TFBrowseImages }

  TFBrowseImages = class(TForm)
    vsList: TBGRAVirtualScreen;
    ComboBox_FileExtension: TComboBox;
    DirectoryEdit1: TDirectoryEdit;
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
    Label_Size: TLabel;
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
    procedure ToolButton_ViewDetailsClick(Sender: TObject);
    procedure ToolButton_GoUpClick(Sender: TObject);
    procedure ToolButton_ViewBigIconClick(Sender: TObject);
    procedure vsPreviewRedraw(Sender: TObject; Bitmap: TBGRABitmap);
  private
    { private declarations }
    FFileExtensions: array of string;
    ShellListView1: TMyShellListView;
    ImageSizeCaption: string;
    FInFormShow: boolean;
    FCurrentImage: TBGRABitmap;
    FCurrentAnimatedGif: TBGRAAnimatedGif;
    FCurrentImageNbLayers: integer;
    FComputeIconCurrentItem: integer;
    FPreviewFilename, FDisplayedPreviewFilename: string;
    FInShowPreview,FInHidePreview: boolean;
    FSavedDetailsViewWidth: integer;
    FIconCache: TStringList;
    FLastDirectory: string;
    FFilename: string;
    FBmpIcon: TBGRABitmap;
    FLastBigIcon: boolean;
    FImageFileNotChecked, FImageFileUnkown, FImageFolder: TBGRABitmap;
    InFilenameChange: boolean;
    FSelectedFiles: array of string;
    function GetSelectedFile(AIndex: integer): string;
    function GetSelectedFileCount: integer;
    procedure ResetDirectory;
    procedure ClearThumbnails;
    procedure StartThumbnails;
    procedure SelectCurrentDir;
    procedure UpdatePreview(AFilename:string); overload;
    procedure UpdatePreview; overload;
    procedure ShowPreview;
    procedure HidePreview;
    procedure UpdateConstraints;
    procedure ViewDetails;
    procedure ViewBigIcons;
    procedure ValidateFileOrDir;
    procedure GoDirUp;
    procedure InitComboExt;
    procedure SetShellMask;
  public
    { public declarations }
    LazPaintInstance: TLazPaintCustomInstance;
    property Filename: string read FFilename;
    property SelectedFileCount: integer read GetSelectedFileCount;
    property SelectedFile[AIndex:integer]: string read GetSelectedFile;
  end;

var
  FBrowseImages: TFBrowseImages;

implementation

{$R *.lfm}

uses BGRAThumbnail, BGRAPaintNet, BGRAOpenRaster, BGRAReadLzp,
    BGRAWriteLzp, FPimage,
    Types, UResourceStrings,
    UConfig, bgrareadjpeg, FPReadJPEG,
    UFileExtensions, Masks, LazUTF8;

{ TFBrowseImages }

procedure TFBrowseImages.DirectoryEdit1Change(Sender: TObject);
var
  SelStart,SelLength: integer;
begin
  SelStart:=DirectoryEdit1.SelStart;
  SelLength:=DirectoryEdit1.SelLength;
  ResetDirectory;
  Self.ActiveControl:= DirectoryEdit1;
  DirectoryEdit1.SelStart:=SelStart;
  DirectoryEdit1.SelLength:=SelLength;
end;

procedure TFBrowseImages.Edit_FilenameChange(Sender: TObject);
var i: integer;
  txt: string;
  first: boolean;
begin
  if InFilenameChange then exit;
  InFilenameChange := true;
  ShellListView1.DeselectAll;
  UpdatePreview('');
  first := true;
  txt := trim(Edit_Filename.Text);
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
  InFilenameChange := false;
end;

procedure TFBrowseImages.ComboBox_FileExtensionChange(Sender: TObject);
begin
  ClearThumbnails;
  SetShellMask;
  StartThumbnails;
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
  FLastDirectory := DefaultPicturesDirectory;

  InitComboExt;

  bmp := TBitmap.Create;
  ImageList128.GetBitmap(0,bmp);
  FImageFileNotChecked := TBGRABitmap.Create(bmp);
  ImageList128.GetBitmap(1,bmp);
  FImageFolder := TBGRABitmap.Create(bmp);
  ImageList128.GetBitmap(2,bmp);
  FImageFileUnkown := TBGRABitmap.Create(bmp);
  bmp.Free;

  FIconCache := TStringList.Create;
  FIconCache.CaseSensitive := true;
  FIconCache.OwnsObjects := true;
  ShellListView1 := TMyShellListView.Create(vsList);
  SetShellMask;
  ShellListView1.OnDblClick := @ShellListView1DblClick;
  ShellListView1.OnSelectItem := @ShellListView1SelectItem;
  ShellListView1.OnSort := @ShellListView1OnSort;
  ShellListView1.OnFormatType := @ShellListView1OnFormatType;

  ImageSizeCaption := Label_Size.Caption;
  Label_Size.Caption := '';
  BGRAPaintNet.RegisterPaintNetFormat;
  BGRAOpenRaster.RegisterOpenRasterFormat;
end;

procedure TFBrowseImages.FormDestroy(Sender: TObject);
begin
  FreeAndNil(ShellListView1);

  FreeAndNil(FCurrentImage);
  FreeAndNil(FBmpIcon);
  FreeAndNil(FImageFileNotChecked);
  FreeAndNil(FImageFileUnkown);
  FreeAndNil(FImageFolder);
  FIconCache.Free;
end;

procedure TFBrowseImages.FormHide(Sender: TObject);
begin
  FLastBigIcon := (ShellListView1.ViewStyle = vsIcon);
  FFilename:= FPreviewFilename;
  Timer1.Enabled := false;
  FLastDirectory := DirectoryEdit1.Text;
  DirectoryEdit1.Text := '';
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
  if (Key = VK_BACK) and not Edit_Filename.Focused then
  begin
    GoDirUp;
    Key := 0;
  end;
end;

procedure TFBrowseImages.FormResize(Sender: TObject);
begin
  UpdateConstraints;
end;

procedure TFBrowseImages.FormShow(Sender: TObject);
var r:TRect;
begin
  if FInFormShow then exit;
  FInFormShow:= true;
  InFilenameChange := true;
  Edit_Filename.Text := '';
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
  if DirectoryEdit1.Text = '' then
    DirectoryEdit1.Text := FLastDirectory;
  Timer1.Enabled := true;
  ShellListView1.SetFocus;
  UpdatePreview;
  FInFormShow:= false;
end;

procedure TFBrowseImages.FormUTF8KeyPress(Sender: TObject;
  var UTF8Key: LCLType.TUTF8Char);
begin
  if (UTF8Key <> '') and (Utf8Key[1] > #32) and not Edit_Filename.Focused then
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
    if ChompPathDelim(DirectoryEdit1.Directory) <> ChompPathDelim(ListBox_RecentDirs.Items[ListBox_RecentDirs.ItemIndex]) then
      DirectoryEdit1.Directory := AppendPathDelim(ListBox_RecentDirs.Items[ListBox_RecentDirs.ItemIndex]);
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

  if not InFilenameChange and ShellListView1.ItemSelected[Item] then
  begin
    InFilenameChange := true;
    Edit_Filename.Text := ShellListView1.ItemName[Item];
    InFilenameChange := false;
  end;

  if ShellListView1.ItemSelected[Item] and not ShellListView1.ItemIsFolder[Item] then
    UpdatePreview(ShellListView1.ItemFullName[Item])
  else
    if FPreviewFilename = ShellListView1.ItemFullName[Item] then
      UpdatePreview('');

  Timer1.Enabled := wasTimer;
end;

procedure TFBrowseImages.ShellListView1OnSort(Sender: TObject);
begin
  FComputeIconCurrentItem := 0;
end;

procedure TFBrowseImages.ShellListView1OnFormatType(Sender: Tobject;
  var AType: string);
var format: TBGRAImageFormat;
begin
  if AType = 'Folder' then AType := rsFolder else
  begin
    format := SuggestImageFormat(AType);
    if format = ifPng then AType := 'PNG' //too long to write explicitely
    else if format = ifGIF then AType := 'GIF' //do not know if animated or not
    else AType := GetImageFormatName(format);
  end;
end;

procedure TFBrowseImages.Timer1Timer(Sender: TObject);
var i: integer;
  iconRect,shellRect:TRect;
  endDate: TDateTime;

  function DetermineIcon(i: integer): boolean;
  var itemPath,cacheName,dummyCaption: string;
    cacheIndex: integer;
    found: boolean;
    mem: TMemoryStream;
  begin
    result := false;
    if ShellListView1.GetItemImage(i) = FImageFileNotChecked then
    begin
      if ShellListView1.ItemIsFolder[i] then
        ShellListView1.SetItemImage(i,FImageFolder,false)
      else
      begin
        itemPath := ShellListView1.ItemFullName[i];
        cacheName := itemPath+':'+Inttostr(FileAgeUTF8(itemPath));
        cacheIndex := FIconCache.IndexOf(cacheName);
        if not Assigned(FBmpIcon) then FBmpIcon := TBGRABitmap.Create;
        if cacheIndex <> -1 then
        begin
          TStream(FIconCache.Objects[cacheIndex]).Position:= 0;
          TBGRAReaderLazPaint.LoadRLEImage(TStream(FIconCache.Objects[cacheIndex]),FBmpIcon,dummyCaption);
          found := true;
        end
        else
        begin
          found := GetFileThumbnail(itemPath,ShellListView1.LargeIconSize,ShellListView1.LargeIconSize, BGRAPixelTransparent, True, FBmpIcon) <> nil;
          if found then
          begin
            if FIconCache.Count >= MaxIconCacheCount then FIconCache.Delete(0);
            mem := TMemoryStream.Create;
            TBGRAWriterLazPaint.WriteRLEImage(mem,FBmpIcon);
            FIconCache.AddObject(cacheName,mem);
          end;
        end;
        if found then
        begin
          ShellListView1.SetItemImage(i,FBmpIcon.Duplicate as TBGRABitmap,True);
        end else
          ShellListView1.SetItemImage(i,FImageFileUnkown,False);
      end;
      result := true;
    end;
  end;

var someIconDone: boolean;

begin
  Timer1.Enabled:= false;
  EndDate := Now + 50 / MSecsPerDay;
  if assigned(FCurrentAnimatedGif) and (FCurrentAnimatedGif.TimeUntilNextImageMs <= 0) then
    vsPreview.RedrawBitmap;
  if FDisplayedPreviewFilename <> FPreviewFilename then UpdatePreview;
  if FComputeIconCurrentItem < ShellListView1.ItemCount then
  begin
    shellRect := rect(0,0,ShellListView1.Width,ShellListView1.Height);
    someIconDone := false;
    for i := FComputeIconCurrentItem to ShellListView1.ItemCount-1 do
    if ShellListView1.GetItemImage(i) = FImageFileNotChecked then
    If Now >= EndDate then break else
    begin
      iconRect := ShellListView1.ItemDisplayRect[i];
      if IntersectRect(iconRect,iconRect,shellRect) then
        if DetermineIcon(i) then someIconDone := true;
    end;
    if not someIconDone then EndDate := Now + 50 / MSecsPerDay;
    for i := FComputeIconCurrentItem to ShellListView1.ItemCount-1 do
    If Now >= EndDate then break else
    begin
      FComputeIconCurrentItem := i+1;
      DetermineIcon(i);
    end;
  end;
  ShellListView1.Update;
  Timer1.Enabled:= true;
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

procedure TFBrowseImages.vsPreviewRedraw(Sender: TObject; Bitmap: TBGRABitmap);
var x,y,w,h: integer;
  ofs: integer;
begin
  if (Bitmap.Width = 0) or (Bitmap.Height = 0) then exit;
  if Assigned(FCurrentImage) or Assigned(FCurrentAnimatedGif) then
  begin
    if Assigned(FCurrentImage) then
    begin
      w := FCurrentImage.Width;
      h := FCurrentImage.Height;
    end else
    begin
      w := FCurrentAnimatedGif.Width;
      h := FCurrentAnimatedGif.Height;
    end;
    if w > bitmap.Width then
    begin
      h := round(h/w*bitmap.Width);
      w := bitmap.Width;
    end;
    if h > bitmap.Height then
    begin
      w := round(w/h*bitmap.Height);
      h := bitmap.Height;
    end;
    x := (bitmap.Width-w) div 2;
    y := (bitmap.Height-h) div 2;
    ofs := 4;
    if w < ofs then ofs := w;
    if h < ofs then ofs := h;
    bitmap.FillRect(rect(x+w,y+ofs,x+ofs+w,y+ofs+h), BGRA(0,0,0,128),dmDrawWithTransparency);
    bitmap.FillRect(rect(x+ofs,y+h,x+w,y+ofs+h), BGRA(0,0,0,128),dmDrawWithTransparency);
    DrawThumbnailCheckers(Bitmap, rect(x,y,x+w,y+h));
    if Assigned(FCurrentImage) then
      bitmap.StretchPutImage(rect(x,y,x+w,y+h), FCurrentImage, dmDrawWithTransparency)
    else
    begin
      bitmap.StretchPutImage(rect(x,y,x+w,y+h), FCurrentAnimatedGif.MemBitmap, dmDrawWithTransparency)

    end;
  end;
end;

procedure TFBrowseImages.ResetDirectory;
begin
  if DirectoryEdit1.Directory <> ShellListView1.Root then
  begin
    ClearThumbnails;
    ShellListView1.Root := DirectoryEdit1.Directory;
    StartThumbnails;
    ShellListView1.SetFocus;
    if ShellListView1.ItemCount <> 0 then
    begin
      ShellListView1.MakeItemVisible(0);
    end;
    SelectCurrentDir;
  end;
end;

function TFBrowseImages.GetSelectedFile(AIndex: integer): string;
begin
  if (AIndex < 0) or (AIndex >= length(FSelectedFiles)) then
    result := ''
  else
    result := FSelectedFiles[AIndex];
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

procedure TFBrowseImages.StartThumbnails;
var I: integer;
begin
  for I := 0 to ShellListView1.ItemCount-1 do
    begin
      if ShellListView1.ItemIsFolder[I] then
        ShellListView1.SetItemImage(i,FImageFolder,false)
      else
        ShellListView1.SetItemImage(i,FImageFileNotChecked,false);
    end;
  FComputeIconCurrentItem := 0;
end;

procedure TFBrowseImages.SelectCurrentDir;
var I: integer;
begin
  ListBox_RecentDirs.ItemIndex := -1;
  for I := 0 to ListBox_RecentDirs.Count-1 do
    if ChompPathDelim(ListBox_RecentDirs.Items[i]) = ChompPathDelim(DirectoryEdit1.Directory) then
    begin
      ListBox_RecentDirs.ItemIndex:= I;
      break;
    end;
end;

procedure TFBrowseImages.UpdatePreview(AFilename: string);
begin
  FPreviewFilename := AFilename;
end;

procedure TFBrowseImages.UpdatePreview;
var reader: TFPCustomImageReader;
  jpegReader: TBGRAReaderJpeg;
  format: TBGRAImageFormat;
begin
  FreeAndNil(FCurrentImage);
  FreeAndNil(FCurrentAnimatedGif);
  FCurrentImageNbLayers := 0;
  vsPreview.RedrawBitmap;
  FDisplayedPreviewFilename:= FPreviewFilename;
  if (FPreviewFilename = '') or not Panel1.Visible then
  begin
    Label_Size.Caption := rsRecentDirectories;
    ListBox_RecentDirs.Visible := true;
    SelectCurrentDir;
    exit;
  end;
  ListBox_RecentDirs.Visible := false;
  Label_Size.Caption := rsLoading+'...';
  Label_Size.Update;
  try
    format := DetectFileFormat(FPreviewFilename);
    if format = ifGif then
    begin
      try
        FCurrentAnimatedGif := TBGRAAnimatedGif.Create(FPreviewFilename);
      except
      end;
    end;
    if FCurrentAnimatedGif = nil then
    begin
      if format = ifJpeg then
      begin
        jpegReader := TBGRAReaderJpeg.Create;
        jpegReader.Performance := jpBestSpeed;
        jpegReader.MinWidth := Screen.Width;
        jpegReader.MinHeight := Screen.Height;
        reader := jpegReader;
      end else
        reader := CreateBGRAImageReader(format);
      try
        FCurrentImage := TBGRABitmap.Create;
        try
          FCurrentImage.LoadFromFileUTF8(FPreviewFilename,reader);
          FCurrentImageNbLayers := 1;
          if reader is TFPReaderOpenRaster then FCurrentImageNbLayers := TFPReaderOpenRaster(reader).NbLayers else
          if reader is TFPReaderPaintDotNet then FCurrentImageNbLayers := TFPReaderPaintDotNet(reader).NbLayers else
          if reader is TBGRAReaderLazPaint then FCurrentImageNbLayers := TBGRAReaderLazPaint(reader).NbLayers;
        except
          FreeAndNil(FCurrentImage);
        end;
      finally
        reader.Free;
      end;
    end;
  except
  end;
  if FCurrentImage <> nil then
  begin
    Label_Size.Caption := ImageSizeCaption + IntToStr(FCurrentImage.Width)+'x'+IntToStr(FCurrentImage.Height)+'x'+IntToStr(FCurrentImageNbLayers);
  end else
  if FCurrentAnimatedGif <> nil then
  begin
    Label_Size.Caption := ImageSizeCaption + IntToStr(FCurrentAnimatedGif.Width)+'x'+IntToStr(FCurrentAnimatedGif.Height)+'x'+IntToStr(FCurrentAnimatedGif.Count);
  end else
    Label_Size.Caption := '';
  vsPreview.RedrawBitmap;
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
  i,count: integer;
begin
  if ShellListView1.SelectedIndex <> -1 then
  begin
    fullName := ShellListView1.ItemFullName[ShellListView1.SelectedIndex];
    if ShellListView1.ItemIsFolder[ShellListView1.SelectedIndex] then
    begin
      DirectoryEdit1.Directory := fullName;
      InFilenameChange := true;
      Edit_Filename.text := '';
      InFilenameChange := false;
      ShellListView1.SetFocus;
    end
    else
    begin
      count := 0;
      for i := 0 to ShellListView1.ItemCount-1 do
        if ShellListView1.ItemSelected[i] then inc(count);
      setlength(FSelectedFiles,count);
      count := 0;
      for i := 0 to ShellListView1.ItemCount-1 do
        if ShellListView1.ItemSelected[i] then
        begin
          FSelectedFiles[count] := ShellListView1.ItemFullName[i];
          inc(count);
        end;
      UpdatePreview(fullName);
      ModalResult:= mrOk;
    end;
  end;
end;

procedure TFBrowseImages.GoDirUp;
var dir: string;
  idx: integer;
begin
  dir := DirectoryEdit1.Directory;
  idx := length(dir);
  if (idx > 1) and (dir[idx] = PathDelim) and (dir[idx-1] <> PathDelim) then dec(idx);
  while (idx >= 1) and (dir[idx] <> PathDelim) do dec(idx);
  if idx <> 0 then DirectoryEdit1.Directory := copy(dir,1,idx);
  ShellListView1.SetFocus;
  UpdatePreview('');
  InFilenameChange := true;
  Edit_Filename.text := '';
  InFilenameChange := false;
end;

procedure TFBrowseImages.InitComboExt;
var extFilter: string;
  parsedExt: TStringList;
  i: integer;
begin
  extFilter := GetExtensionFilter([eoReadable]);
  parsedExt := TParseStringList.Create(extFilter,'|');
  setlength(FFileExtensions, parsedExt.Count div 2);
  for i := 0 to parsedExt.Count div 2-1 do
  begin
    FFileExtensions[i] := parsedExt[i*2+1];
    ComboBox_FileExtension.Items.Add(parsedExt[i*2]);
  end;
  parsedExt.Free;
  if ComboBox_FileExtension.Items.Count > 0 then
    ComboBox_FileExtension.ItemIndex := 0;
end;

procedure TFBrowseImages.SetShellMask;
begin
  if (ComboBox_FileExtension.ItemIndex >= 0) and (ComboBox_FileExtension.ItemIndex < length(FFileExtensions)) then
    ShellListView1.Mask := FFileExtensions[ComboBox_FileExtension.ItemIndex];
end;

end.

