unit UMySLV;

{$mode objfpc}{$H+}

interface

uses
  Classes, Types, SysUtils, ComCtrls, ShellCtrls, Controls, Graphics, BGRABitmap,
  BGRAVirtualScreen, BGRABitmapTypes, UVolatileScrollBar;

type
  PMyShellListViewItemData = ^TMyShellListViewItemData;
  TMyShellListViewItemData = record
    initialIndex: integer;
    caption, filename, typeStr, sizeStr, dateOrDeviceStr: string;
    fileSize: int64;
    isFolder: boolean;
    modification: TDateTime;
    image: TBGRABitmap;
    imageOwned: boolean;
    displayRect: TRect;
    isSelected: boolean;
  end;
  TMyShellListViewData = array of TMyShellListViewItemData;

  TFormatTypeEvent = procedure(Sender: TObject; var AType: string) of object;
  TSelectItemEvent = procedure(Sender: TObject; Item: Integer; Selected: Boolean) of object;

  { TMyShellListView }

  TMyShellListView = class
  private
    FAllowMultiSelect: boolean;
    FOnDblClick: TNotifyEvent;
    FOnSelectItem: TSelectItemEvent;
    FSortColumn: integer;
    FVirtualScreen: TBGRAVirtualScreen;
    FMask: string;
    FObjectTypes: TObjectTypes;
    FOnFormatType: TFormatTypeEvent;
    FRoot: string;
    FData: TMyShellListViewData;
    FPreviousResize: TSize;
    FFitColumnNeeded: boolean;
    FViewStyle: TViewStyle;
    FIndexIcon,FIndexName, FIndexSize, FIndexType, FIndexDate: integer;
    FUpdateCount: integer;
    FColumns: array of record
      Name: string;
      Width: integer;
      Align: TAlignment;
      displayRect: TRect;
    end;
    FActualRowHeight,FIconsPerLine: integer;
    FSelectedIndex,FKeySelectionRangeStart: integer;
    FVScrollBar: TVolatileScrollBar;
    FVerticalScrollPos: integer;
    FWantedItemVisible: integer;
    FItemsPerPage: integer;
    { Setters and getters }
    function GetColumnCount: integer;
    function GetHeight: integer;
    function GetItemCaption(AIndex: integer): string;
    function GetItemCount: integer;
    function GetItemDevice(AIndex: integer): string;
    function GetItemIsFolder(AIndex: integer): boolean;
    function GetItemName(AIndex: integer): string;
    function GetItemSelected(AIndex: integer): boolean;
    function GetItemType(AIndex: integer): string;
    function GetSelectedCount: integer;
    function GetViewStyleFit: TViewStyle;
    function GetWidth: integer;
    procedure SetAllowMultiSelect(AValue: boolean);
    procedure SetItemSelected(AIndex: integer; AValue: boolean);
    procedure SetMask(const AValue: string);
    procedure SetOnDblClick(AValue: TNotifyEvent);
    procedure SetOnSelectItem(AValue: TSelectItemEvent);
    procedure SetRoot(const AValue: string);
    procedure SetSelectedIndex(AValue: integer);
    procedure SetSortColumn(AValue: integer);
    procedure SetViewStyleFit(AValue: TViewStyle);
    procedure SetDisplayRect(AIndex: integer; const ARect:TRect);
  protected
    FOnSort: TNotifyEvent;
    { Methods specific to Lazarus }
    procedure PopulateWithRoot();
    procedure Redraw(Sender: TObject; ABitmap: TBGRABitmap);
    procedure KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MouseDoubleClick(Sender: TObject);
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MouseMove(Sender: TObject; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure MouseWheel(Sender: TObject; {%H-}Shift: TShiftState;
         WheelDelta: Integer; {%H-}MousePos: TPoint; var Handled: Boolean);
    procedure CompareItem(Sender: TObject; Item1, Item2: PMyShellListViewItemData; {%H-}Data: Integer;
      var Compare: Integer);
    procedure ColumnClick(Sender: TObject; AColumn: integer);
    procedure DoFitColumns(ABitmap: TBGRABitmap; AClientWidth: integer);
    function AddColumn(AName: string; AWidth: integer; AAlign:TAlignment): integer;
    function GetItemCell(AIndex, AColumn: integer): string; virtual;
    procedure Clear;
    function GetItemFullName(AIndex: integer): string;
    function GetItemDisplayRect(AIndex: integer): TRect;
  public
    DetailIconSize, SmallIconSize, LargeIconSize, FontHeight, MinimumRowHeight: integer;
    IconPadding: integer;
    BytesCaption: string;
    { Basic methods }
    procedure Reload;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Invalidate;
    procedure Update;
    procedure MakeItemVisible(AIndex : integer);
    constructor Create(AVirtualScreen: TBGRAVirtualScreen);
    destructor Destroy; override;
    procedure SetItemImage(AIndex: integer; ABitmap: TBGRABitmap; AOwned: boolean);
    function GetItemImage(AIndex: integer): TBGRABitmap;
    procedure SetFocus;
    function GetItemAt(X,Y: Integer): integer;
    procedure DeselectAll;
    procedure Sort;
    procedure RemoveItemFromList(AIndex: integer);
    { Properties }
    property Mask: string read FMask write SetMask; // Can be used to conect to other controls
    property ObjectTypes: TObjectTypes read FObjectTypes write FObjectTypes;
    property Root: string read FRoot write SetRoot;
    property ViewStyle: TViewStyle read GetViewStyleFit write SetViewStyleFit;
    property OnDblClick: TNotifyEvent read FOnDblClick write SetOnDblClick;
    property OnSelectItem: TSelectItemEvent read FOnSelectItem write SetOnSelectItem;
    property SortColumn: integer read FSortColumn write SetSortColumn;
    property OnSort: TNotifyEvent read FOnSort write FOnSort;
    property OnFormatType: TFormatTypeEvent read FOnFormatType write FOnFormatType;
    property ColumnCount: integer read GetColumnCount;
    property ItemCount: integer read GetItemCount;
    property SelectedIndex: integer read FSelectedIndex write SetSelectedIndex;
    property Width: integer read GetWidth;
    property Height: integer read GetHeight;
    property ItemCaption[AIndex: integer]: string read GetItemCaption;
    property ItemFullName[AIndex: integer]: string read GetItemFullName;
    property ItemName[AIndex: integer]: string read GetItemName;
    property ItemDisplayRect[AIndex: integer]: TRect read GetItemDisplayRect;
    property ItemSelected[AIndex: integer]: boolean read GetItemSelected write SetItemSelected;
    property ItemIsFolder[AIndex: integer]: boolean read GetItemIsFolder;
    property ItemType[AIndex: integer]: string read GetItemType;
    property ItemDevice[AIndex: integer]: string read GetItemDevice;
    property AllowMultiSelect: boolean read FAllowMultiSelect write SetAllowMultiSelect;
    property SelectedCount: integer read GetSelectedCount;
  end;

function FileSizeToStr(ASize: int64; AByteCaption: string): string;

implementation

uses LCLType, FileUtil, UResourceStrings, LazPaintType, LazUTF8, Forms, Math,
  UFileSystem;

var
  SortTarget: TMyShellListView;

function FileSizeToStr(ASize: int64; AByteCaption: string): string;
begin
  if ASize < 1024 then
    result := IntToStr(ASize) + ' ' + AByteCaption
  else if ASize < 1024 * 1024 then
    result := FloatToStrF(ASize/1024, ffFixed, 5, 1) + ' kB'
  else
    result := FloatToStrF(ASize/(1024*1024), ffFixed, 5, 1) + ' MB';
end;

function MyShellListViewCompare(item1,item2: pointer): integer;
begin
  result := 0;
  if Assigned(SortTarget) then
    SortTarget.CompareItem(SortTarget,item1,item2,0,result);
end;

{ TMyShellListView }

procedure TMyShellListView.SetMask(const AValue: string);
begin
  if AValue <> FMask then
  begin
    FMask := AValue;
    PopulateWithRoot();
  end;
end;

procedure TMyShellListView.SetOnDblClick(AValue: TNotifyEvent);
begin
  if FOnDblClick=AValue then Exit;
  FOnDblClick:=AValue;
end;

procedure TMyShellListView.SetOnSelectItem(AValue: TSelectItemEvent);
begin
  if FOnSelectItem=AValue then Exit;
  FOnSelectItem:=AValue;
end;

function TMyShellListView.GetViewStyleFit: TViewStyle;
begin
  result := FViewStyle;
end;

function TMyShellListView.GetWidth: integer;
begin
  result := FVirtualScreen.Width;
end;

procedure TMyShellListView.SetAllowMultiSelect(AValue: boolean);
var idx: integer;
begin
  if FAllowMultiSelect=AValue then Exit;
  FAllowMultiSelect:=AValue;

  if not AValue then
  begin
    if SelectedCount > 1 then
    begin
      idx := SelectedIndex;
      DeselectAll;
      if idx <> -1 then ItemSelected[idx] := true;
    end;
  end;
end;

procedure TMyShellListView.SetItemSelected(AIndex: integer; AValue: boolean);
begin
  if (AIndex < 0) or (AIndex >= ItemCount) then exit
  else
  begin
    FData[AIndex].isSelected := AValue;
    FVirtualScreen.DiscardBitmap;
  end;
end;

function TMyShellListView.GetColumnCount: integer;
begin
  result := length(FColumns);
end;

function TMyShellListView.GetHeight: integer;
begin
  result := FVirtualScreen.Height;
end;

function TMyShellListView.GetItemCaption(AIndex: integer): string;
begin
  if (AIndex < 0) or (AIndex >= ItemCount) then
    result := ''
  else
    result := FData[AIndex].caption;
end;

function TMyShellListView.GetItemCount: integer;
begin
  result := length(FData);
end;

function TMyShellListView.GetItemDevice(AIndex: integer): string;
begin
  if (AIndex < 0) or (AIndex >= ItemCount) then
    result := ''
  else
    result := FData[AIndex].dateOrDeviceStr;
end;

function TMyShellListView.GetItemIsFolder(AIndex: integer): boolean;
begin
  if (AIndex < 0) or (AIndex >= ItemCount) then
    result := false
  else
    result := FData[AIndex].isFolder;
end;

function TMyShellListView.GetItemName(AIndex: integer): string;
begin
  if (AIndex < 0) or (AIndex >= ItemCount) then
    result := ''
  else
    result := FData[AIndex].filename;
end;

function TMyShellListView.GetItemSelected(AIndex: integer): boolean;
begin
  if (AIndex < 0) or (AIndex >= ItemCount) then
    result := false
  else
    result := FData[AIndex].isSelected;
end;

function TMyShellListView.GetItemType(AIndex: integer): string;
begin
  if (AIndex < 0) or (AIndex >= ItemCount) then
    result := ''
  else
    result := FData[AIndex].typeStr;
end;

function TMyShellListView.GetSelectedCount: integer;
var
  i: Integer;
begin
  result := 0;
  for i := 0 to ItemCount-1 do
    if ItemSelected[i] then inc(result);
end;

procedure TMyShellListView.SetRoot(const AValue: string);
begin
  if FRoot <> AValue then
  begin
    FRoot := AValue;
    PopulateWithRoot();
  end;
end;

procedure TMyShellListView.SetSelectedIndex(AValue: integer);
begin
  if (AValue < 0) or (AValue >= ItemCount) then AValue := -1;
  if FSelectedIndex=AValue then Exit;
  DeselectAll;
  FSelectedIndex:=AValue;
  ItemSelected[AValue] := true;
end;

procedure TMyShellListView.SetSortColumn(AValue: integer);
begin
  if FSortColumn=AValue then Exit;
  FSortColumn:=AValue;
end;

procedure TMyShellListView.SetViewStyleFit(AValue: TViewStyle);
begin
  if FViewStyle=AValue then Exit;
  FViewStyle := AValue;
  FFitColumnNeeded:= true;
  FreeAndNil(FVScrollBar);
end;

procedure TMyShellListView.SetDisplayRect(AIndex: integer; const ARect: TRect);
begin
  if (AIndex < 0) or (AIndex >= ItemCount) then exit;
  FData[AIndex].displayRect := ARect;
end;

procedure TMyShellListView.PopulateWithRoot;
var
  i,j: Integer;
  Dirs,Files: TStringList;
  CurFileName, CurFilePath, fileType: string;
  CurFileSize: Int64;
  dataIndex: integer;
  age: longint;

  function NewItem: integer;
  begin
    result := dataIndex;
    with FData[dataIndex] do
    begin
      initialIndex := dataIndex;
      caption := '';
      filename := '';
      typeStr := '';
      sizeStr := '';
      dateOrDeviceStr := '';
      fileSize:= 0;
      isFolder := false;
      modification := 0;
      image := nil;
      imageOwned := false;
      displayRect := EmptyRect;
      isSelected := false;
    end;
    inc(dataIndex);
  end;

var drives: TFileSystemArray;
begin
  BeginUpdate;
  Clear;

  // Check inputs
  if Trim(FRoot) = '' then
  begin
    EndUpdate;
    Exit;
  end;

  Files := TStringList.Create;
  Dirs := TStringList.Create;
  try
    if FRoot = ':' then
    begin
      if FIndexDate <> -1 then FColumns[FIndexDate].Name := rsStorageDevice;
      if FObjectTypes * [otFolders] = [] then
      begin
        FData := nil;
        dataIndex := 0;
      end else
      begin
        drives := GetFileSystems;
        setlength(FData, length(drives));
        dataIndex := 0;
        for i := 0 to high(drives) do
        with FData[NewItem] do
        begin
          isFolder := true;
          caption := Trim(drives[i].name);
          filename := drives[i].path;
          if filename <> PathDelim then
            filename := ExcludeTrailingPathDelimiter(filename);
          if caption = '' then caption := filename;
          dateOrDeviceStr := drives[i].device;
          typeStr := drives[i].fileSystem;
        end;
      end;
    end else
    begin
      if FIndexDate <> -1 then FColumns[FIndexDate].Name := rsFileDate;
      if FObjectTypes * [otFolders] <> [] then TCustomShellTreeView.GetFilesInDir(FRoot, '', FObjectTypes * [otFolders], Dirs, fstAlphabet);
      if FObjectTypes - [otFolders] <> [] then TCustomShellTreeView.GetFilesInDir(FRoot, FMask, FObjectTypes - [otFolders], Files, fstAlphabet);
      setlength(FData, Dirs.Count+Files.Count);
      dataIndex := 0;

      fileType := rsFolder;
      if Assigned(FOnFormatType) then FOnFormatType(self, fileType);
      for i := 0 to Dirs.Count - 1 do
      if (Dirs.Strings[i] <> '') and (Dirs.Strings[i][1] <> '.') then
      begin
        CurFileName := Dirs.Strings[i];
        CurFilePath := IncludeTrailingPathDelimiter(FRoot) + CurFileName;
        with FData[NewItem] do
        begin
          isFolder := true;
          filename := CurFileName;
          caption := CurFileName;
          typeStr := fileType;
        end;
      end;

      for i := 0 to Files.Count - 1 do
      begin
        j := NewItem;
        CurFileName := Files.Strings[i];
        CurFilePath := IncludeTrailingPathDelimiter(FRoot) + CurFileName;
        CurFileSize := FileSize(CurFilePath); // in Bytes
        FData[j].isFolder := false;
        FData[j].filename := CurFileName;
        FData[j].caption := ChangeFileExt(CurFileName,'');
        age := FileAgeUTF8(CurFilePath);
        try
          FData[j].modification := FileDateToDateTime(age);
        except
        end;
        FData[j].fileSize:= CurFileSize;

        // Second column - Size
        // The raw size in bytes is stored in the data part of the item
        FData[j].sizeStr := FileSizeToStr(CurFileSize, BytesCaption);
        // Third column - Type
        fileType := ExtractFileExt(CurFileName);
        if Assigned(FOnFormatType) then FOnFormatType(self, fileType);
        FData[j].typeStr := fileType;
        FData[j].dateOrDeviceStr := DateToStr(FData[j].modification);
      end;
    end;
  finally
    Files.Free;
    Dirs.Free;
    setlength(FData, dataIndex);
    FFitColumnNeeded:= true;
    DeselectAll;
    EndUpdate;
    if SortColumn <> -1 then Sort;
  end;
end;

procedure TMyShellListView.Redraw(Sender: TObject; ABitmap: TBGRABitmap);
var
  clientArea: TRect;
  textHeight,w,h: integer;
  maxScrollDetail, maxScrollIcons: integer;
  btnColor,btnTxtColor: TBGRAPixel;
  txtColor,selTxtColor,selBackColor: TBGRAPixel;
  actualIconSize,iconSizeWithPadding: integer;
  scrollBarVisible: boolean;
  totalIconVSize: integer;

  procedure DrawDetails;
  var col,x,y,row: integer;
    colPos: array of integer;
    curY : integer;
    maxScroll: integer;
    c: TBGRAPixel;
    r: TRect;
    txt:string;
  begin
    ABitmap.GradientFill(0,0,w,textHeight,  ApplyIntensityFast(btnColor,38000),ApplyIntensityFast(btnColor,26000),
                       gtLinear, PointF(0,0),PointF(0,textHeight), dmSet);

    if (FWantedItemVisible <> -1) and (FWantedItemVisible < ItemCount) then
    begin
      curY := -(FVerticalScrollPos*FActualRowHeight div 32)+(FWantedItemVisible*FActualRowHeight);
      if curY < 0 then FVerticalScrollPos:= FWantedItemVisible*32 else
        if textHeight+curY+FActualRowHeight > h then
        begin
          FVerticalScrollPos:= (FWantedItemVisible+1)*32 - (h-textHeight)*32 div FActualRowHeight;
          if FVerticalScrollPos < 0 then FVerticalScrollPos := 0;
        end;
      FWantedItemVisible:= -1;
      FreeAndNil(FVScrollBar);
    end;

    if scrollBarVisible and not Assigned(FVScrollBar) then
    begin
      maxScroll := ItemCount*32 - (h-textHeight)*32 div FActualRowHeight + 8;
      if maxScroll < 0 then maxScroll := 0;
      FVScrollBar := TVolatileScrollBar.Create(w-VolatileScrollBarSize,textHeight,
         VolatileScrollBarSize,h-textHeight,sbVertical,FVerticalScrollPos,0,maxScroll);
    end;
    if Assigned(FVScrollBar) then
    begin
      FVScrollBar.Draw(ABitmap);
      clientArea := rect(0,textHeight,w-VolatileScrollBarSize,h);
    end else
    begin
      clientArea := rect(0,textHeight,w,h);
      FVerticalScrollPos:= 0;
    end;
    FItemsPerPage:= Size(clientArea).cy div FActualRowHeight;

    setlength(colPos,ColumnCount+1);
    colPos[0] := 0;
    for col := 0 to ColumnCount-1 do
    begin
      colPos[col+1] := colPos[col] + FColumns[col].Width;
      r := rect(colPos[col],0,colPos[col+1],textHeight);
      FColumns[col].displayRect := r;
      txt := FColumns[col].Name;
      if col = SortColumn then
        if col = FIndexDate then txt += '▲'
          else txt += '▼';
      ABitmap.ClipRect := r;
      if ABitmap.TextSize(txt).cx > Size(r).cx then
        ABitmap.TextOut(r.left,r.top, txt, btnTxtColor, taLeftJustify)
      else
       case FColumns[col].Align of
        taCenter: ABitmap.TextOut((r.left+r.right) div 2,r.top, txt, btnTxtColor, FColumns[col].Align);
        taRightJustify: ABitmap.TextOut(r.right,r.top, txt, btnTxtColor, FColumns[col].Align);
        else ABitmap.TextOut(r.left,r.top, txt, btnTxtColor, FColumns[col].Align);
       end;
      ABitmap.NoClip;
    end;
    curY := textHeight-(FVerticalScrollPos mod 32)*FActualRowHeight div 32;
    for row := 0 to ItemCount-1 do SetDisplayRect(row,EmptyRect);
    row := FVerticalScrollPos div 32;
    while (curY < clientArea.Bottom) and (row < ItemCount) do
    begin
      r := rect(clientArea.Left,curY,clientArea.Right,curY+FActualRowHeight);
      if IntersectRect(r,r,clientArea) then
      begin
        SetDisplayRect(row,r);
        ABitmap.ClipRect := r;

        if ItemSelected[row] then
        begin
          ABitmap.FillRect(r, selBackColor, dmSet);
          c := selTxtColor;
        end else
          c := txtColor;

        if GetItemImage(row) <> nil then
        begin
          x := colPos[FIndexIcon]+(FColumns[FIndexIcon].Width-DetailIconSize) div 2;
          y := curY+(FActualRowHeight-DetailIconSize) div 2;
          ABitmap.StretchPutImage(RectWithSize(x,y,DetailIconSize,DetailIconSize), GetItemImage(row), dmDrawWithTransparency);
        end;
        for col := 0 to ColumnCount-1 do
          ABitmap.TextRect(rect(colPos[col],curY,colPos[col+1],curY+FActualRowHeight), ' '+GetItemCell(row,col)+' ', FColumns[col].Align, tlCenter, c);
        ABitmap.NoClip;
      end;
      inc(curY, FActualRowHeight);
      inc(row);
    end;
  end;

  procedure DrawIcons;
  var x,y,item,nx: integer;
    r: TRect;
    c: TBGRAPixel;
    maxScroll: integer;
  begin
    if (FWantedItemVisible <> -1) and (FWantedItemVisible < ItemCount) then
    begin
      y := -(FVerticalScrollPos*totalIconVSize div (32*FIconsPerLine))+(FWantedItemVisible div FIconsPerLine)*totalIconVSize;
      if y < 0 then FVerticalScrollPos:= (FWantedItemVisible div FIconsPerLine)*32*FIconsPerLine else
        if y+totalIconVSize > h then
        begin
          FVerticalScrollPos:= ((FWantedItemVisible div FIconsPerLine)+1)*32*FIconsPerLine - (h*32*FIconsPerLine div totalIconVSize);
          if FVerticalScrollPos < 0 then FVerticalScrollPos := 0;
        end;
      FWantedItemVisible:= -1;
      FreeAndNil(FVScrollBar);
    end;

    if scrollBarVisible and not Assigned(FVScrollBar) then
    begin
      maxScroll := ((ItemCount+FIconsPerLine-1) div FIconsPerLine)*32*FIconsPerLine - (h*32*FIconsPerLine div totalIconVSize) + 8*FIconsPerLine;
      if maxScroll < 0 then maxScroll := 0;
      FVScrollBar := TVolatileScrollBar.Create(w-VolatileScrollBarSize,0,
         VolatileScrollBarSize,h,sbVertical,FVerticalScrollPos,0,maxScroll);
    end;
    if Assigned(FVScrollBar) then
    begin
      FVScrollBar.Draw(ABitmap);
      clientArea := rect(0,0,w-VolatileScrollBarSize,h);
    end else
    begin
      clientArea := rect(0,0,w,h);
      FVerticalScrollPos := 0;
    end;
    FItemsPerPage:= (Size(clientArea).cy div totalIconVSize)*FIconsPerLine;
    for item := 0 to ItemCount-1 do SetDisplayRect(item,EmptyRect);
    item := (FVerticalScrollPos div (32*FIconsPerLine))*FIconsPerLine;
    x := clientArea.left;
    y := clientArea.top - FVerticalScrollPos*totalIconVSize div (32*FIconsPerLine) + (item div FIconsPerLine)*totalIconVSize;
    nx := 0;
    while item < ItemCount do
    begin
      r := RectWithSize(x,y,iconSizeWithPadding,totalIconVSize);
      if IntersectRect(r,r,clientArea) then
      begin
        ABitmap.ClipRect := r;
        SetDisplayRect(item,r);
        if ItemSelected[item] then
        begin
          ABitmap.FillRect(r, selBackColor, dmSet);
          c := selTxtColor;
        end else
          c := txtColor;

        if GetItemImage(item) <> nil then
        begin
          r := RectWithSize(x+IconPadding,y+IconPadding,actualIconSize,actualIconSize);
          ABitmap.StretchPutImage(r,GetItemImage(item),dmDrawWithTransparency);
        end;
        with ABitmap.TextSize(ItemCaption[item]) do
          begin
            if cx > iconSizeWithPadding then
              ABitmap.TextOut(x,y+IconPadding+actualIconSize,ItemCaption[item],c,taLeftJustify)
            else
              ABitmap.TextOut(x+(iconSizeWithPadding div 2),y+IconPadding+actualIconSize,ItemCaption[item],c,taCenter)
          end;

        ABitmap.NoClip;
      end;
      inc(x,iconSizeWithPadding);
      inc(nx);
      if nx >= FIconsPerLine then
      begin
        nx := 0;
        x := clientArea.Left;
        inc(y,totalIconVSize);
        if y >= clientArea.Bottom then break;
      end;
      inc(item);
    end;
  end;

var i: integer;

begin
  if SelectedIndex = -1 then FKeySelectionRangeStart := -1
  else if FKeySelectionRangeStart = -1 then FKeySelectionRangeStart:= SelectedIndex;
  for i := 0 to ColumnCount-1 do
    FColumns[i].displayRect := EmptyRect;
  w := ABitmap.Width;
  h := ABitmap.Height;
  FItemsPerPage:= 0;
  ABitmap.FontHeight := FontHeight;
  ABitmap.FontQuality := fqSystemClearType;
  FActualRowHeight:= MinimumRowHeight;
  textHeight := ABitmap.FontFullHeight+2;
  if textHeight > FActualRowHeight then FActualRowHeight:= textHeight;

  if (w <> FPreviousResize.cx) or (h <> FPreviousResize.cy) then
  begin
    FPreviousResize.cx := w;
    FPreviousResize.cy := h;
    FFitColumnNeeded:= true;

    FreeAndNil(FVScrollBar);
  end;
  if ViewStyle = vsReport then actualIconSize:= DetailIconSize
  else if ViewStyle = vsSmallIcon then actualIconSize := SmallIconSize
  else if ViewStyle = vsIcon then actualIconSize:= LargeIconSize
  else actualIconSize := FActualRowHeight-2;

  if actualIconSize+2 > FActualRowHeight then FActualRowHeight:= actualIconSize+2;
  iconSizeWithPadding := actualIconSize+IconPadding*2;
  FIconsPerLine:= (w-VolatileScrollBarSize) div iconSizeWithPadding;
  if FIconsPerLine < 1 then FIconsPerLine:= 1;
  totalIconVSize := iconSizeWithPadding+textHeight;

  maxScrollDetail := Max(0,ItemCount - ((h-textHeight) div FActualRowHeight));
  maxScrollIcons := ((ItemCount+FIconsPerLine-1) div FIconsPerLine)*FIconsPerLine
                - (h div totalIconVSize)*FIconsPerLine;

  scrollBarVisible:= ((ViewStyle = vsReport) and (maxScrollDetail > 0)) or
       ((ViewStyle in[vsSmallIcon,vsIcon]) and (maxScrollIcons > 0));

  if FFitColumnNeeded then
  begin
    if scrollBarVisible then
      DoFitColumns(ABitmap, w-VolatileScrollBarSize) else
      DoFitColumns(ABitmap, w);
    FFitColumnNeeded:= false;
  end;
  btnColor := ColorToBGRA(ColorToRGB(clBtnFace));
  btnTxtColor := ColorToBGRA(ColorToRGB(clBtnText));
  txtColor := ColorToBGRA(ColorToRGB(clWindowText));
  selTxtColor := ColorToBGRA(ColorToRGB(clHighlightText));
  selBackColor := ColorToBGRA(ColorToRGB(clHighlight));
  ABitmap.Fill(ColorToBGRA(ColorToRGB(clWindow)));
  if ViewStyle = vsReport then DrawDetails else
    DrawIcons;
end;

procedure TMyShellListView.KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);

  procedure KeySelectRange(curItem: integer);
  var i: integer;
  begin
    DeselectAll;
    if (ssShift in Shift) and (FKeySelectionRangeStart >= 0) and (FKeySelectionRangeStart < ItemCount) and
      FAllowMultiSelect then
      begin
        i := curItem;
        FSelectedIndex:= curItem;
        ItemSelected[i] := true;
        while i <> FKeySelectionRangeStart do
        begin
          if i< FKeySelectionRangeStart then inc(i) else dec(i);
          ItemSelected[i]:= true;
        end;
      end else
      begin
        FSelectedIndex:= curItem;
        ItemSelected[FSelectedIndex] := true;
      end;
    FVirtualScreen.DiscardBitmap;
    if Assigned(FOnSelectItem) then FOnSelectItem(self,curItem,true);
    MakeItemVisible(curItem);
  end;

begin
  if ItemCount = 0 then exit;

  if Key in [VK_DOWN,VK_RIGHT,VK_LEFT,VK_UP,VK_PRIOR,VK_NEXT] then
    begin
      if SelectedIndex = -1 then
      begin
        Key := 0;
        DeselectAll;
        FSelectedIndex:= 0;
        ItemSelected[0] := true;
        FVirtualScreen.DiscardBitmap;
        MakeItemVisible(0);
        if Assigned(FOnSelectItem) then FOnSelectItem(self,0,true);
        exit;
      end
    end;
  if Key = VK_HOME then
  begin
    Key := 0;
    KeySelectRange(0);
  end else
  if Key = VK_END then
  begin
    Key := 0;
    KeySelectRange(ItemCount-1);
  end else
  if ((Key = VK_DOWN) and (ViewStyle in [vsReport,vsList])) or
    ((Key = VK_RIGHT) and (ViewStyle in [vsIcon,vsSmallIcon])) then
  begin
    Key := 0;
    if SelectedIndex < ItemCount-1 then KeySelectRange(SelectedIndex+1);
  end else
  if ((Key = VK_UP) and (ViewStyle = vsReport)) or
    ((Key = VK_LEFT) and (ViewStyle = vsIcon)) then
  begin
    Key := 0;
    if SelectedIndex > 0 then KeySelectRange(SelectedIndex-1);
  end else
  if (Key = VK_DOWN) and (ViewStyle in [vsIcon,vsSmallIcon]) then
  begin
    Key := 0;
    if SelectedIndex < ItemCount-1 then KeySelectRange(Min(ItemCount-1,SelectedIndex+FIconsPerLine));
  end else
  if (Key = VK_UP) and (ViewStyle in [vsIcon,vsSmallIcon]) then
  begin
    Key := 0;
    if SelectedIndex > 0 then KeySelectRange(Max(0,SelectedIndex-FIconsPerLine));
  end else
  if (Key = VK_NEXT) and (FItemsPerPage <> 0) then
  begin
    Key := 0;
    if SelectedIndex < ItemCount-1 then KeySelectRange(Min(ItemCount-1,SelectedIndex+FItemsPerPage));
  end else
  if (Key = VK_PRIOR) and (FItemsPerPage <> 0) then
  begin
    Key := 0;
    if SelectedIndex > 0 then KeySelectRange(Max(0,SelectedIndex-FItemsPerPage));
  end;

end;

procedure TMyShellListView.MouseDoubleClick(Sender: TObject);
begin
  if (SelectedIndex <> -1) and Assigned(FOnDblClick) then
    FOnDblClick(self);
end;

procedure TMyShellListView.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var i,idx,prevItem:integer;
  prevItemWasSelected:boolean;
begin
  SetFocus;
  for i := 0 to ColumnCount-1 do
    if PtInRect(Point(x,y),FColumns[i].displayRect) then
    begin
      ColumnClick(self,i);
      exit;
    end;

  if Assigned(FVScrollBar) and (Button = mbLeft) then
    if FVScrollBar.MouseDown(X,Y) then
    begin
      FVerticalScrollPos:= FVScrollBar.Position;
      FVirtualScreen.DiscardBitmap;
      exit;
    end;
  idx := GetItemAt(X,Y);
  prevItem := FSelectedIndex;
  prevItemWasSelected := (prevItem <> -1) and ItemSelected[prevItem];
  if not (ssCtrl in Shift) or not FAllowMultiSelect then DeselectAll;
  if (ssShift in Shift) and (prevItem <> -1) and (idx <> -1) and FAllowMultiSelect then
  begin
    FSelectedIndex:= prevItem;
    ItemSelected[FSelectedIndex] := prevItemWasSelected;
    while idx <> FSelectedIndex do
    begin
      if FSelectedIndex > idx then dec(FSelectedIndex) else inc(FSelectedIndex);
      ItemSelected[FSelectedIndex] := not ItemSelected[FSelectedIndex];
    end;
    FVirtualScreen.DiscardBitmap;
  end else
  if idx <> -1 then
  begin
    ItemSelected[idx] := not ItemSelected[idx];
    if ItemSelected[idx] then
      FSelectedIndex := idx
    else
      FSelectedIndex:= -1;
    FVirtualScreen.DiscardBitmap;
  end;
  if (FSelectedIndex <> prevItem) then
  begin
    if prevItemWasSelected and not ((prevItem <> -1) and ItemSelected[prevItem]) then
      if Assigned(FOnSelectItem) then FOnSelectItem(self,prevItem,False);
    if (FSelectedIndex <> -1) and ItemSelected[FSelectedIndex] then
    begin
      if Assigned(FOnSelectItem) then FOnSelectItem(self,FSelectedIndex,true);
      MakeItemVisible(FSelectedIndex);
    end;
  end;
end;

procedure TMyShellListView.MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if Assigned(FVScrollBar) then
    if FVScrollBar.MouseMove(X,Y) then
    begin
      FVerticalScrollPos:= FVScrollBar.Position;
      FVirtualScreen.DiscardBitmap;
    end;
end;

procedure TMyShellListView.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FVScrollBar) and (Button = mbLeft) then
    if FVScrollBar.MouseUp(X,Y) then
    begin
      FVerticalScrollPos:= FVScrollBar.Position;
      FVirtualScreen.DiscardBitmap;
    end;
end;

procedure TMyShellListView.MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var Delta: integer;
begin
  if Assigned(FVScrollBar) then
  begin
    Delta := WheelDelta*32;
    if ViewStyle = vsIcon then
      Delta *= FIconsPerLine
    else
      Delta *= 2;
    FVerticalScrollPos -= Delta div 120;
    if FVerticalScrollPos > FVScrollBar.Maximum then FVerticalScrollPos:= FVScrollBar.Maximum;
    if FVerticalScrollPos < FVScrollBar.Minimum then FVerticalScrollPos:= FVScrollBar.Minimum;
    FreeAndNil(FVScrollBar);
    FVirtualScreen.DiscardBitmap;
    Handled := true;
  end;
end;

procedure TMyShellListView.CompareItem(Sender: TObject; Item1,
  Item2: PMyShellListViewItemData; Data: Integer; var Compare: Integer);
var diff: int64;
  diffDate: TDateTime;

  procedure CompareIndex;
  begin
    if item1^.initialIndex > item2^.initialIndex then
      compare := 1
    else
    if item1^.initialIndex < item2^.initialIndex then
      compare := -1
    else
      compare := 0;
  end;

  procedure CompareName;
  begin
    if not (Item1^.isFolder xor Item2^.isFolder) then
    begin
      compare := CompareText(Item1^.filename,Item2^.filename);
      if compare = 0 then CompareIndex;
    end
    else
    begin
      if Item1^.isFolder then Compare := -1
        else Compare := 1;
    end;
  end;

begin
  if SortColumn = FIndexName then CompareName else
  if SortColumn = FIndexSize then
  begin
    diff := Item1^.fileSize-Item2^.fileSize;
    if diff < 0 then compare := -1 else
    if diff > 0 then compare := 1 else
      CompareName;
  end else
  if SortColumn = FIndexType then
  begin
    if not (Item1^.isFolder xor Item2^.isFolder) then
    begin
      compare := UTF8CompareText(Item1^.typeStr,Item2^.typeStr);
      if compare = 0 then CompareName;
    end else
      CompareName;
  end else
  if SortColumn = FIndexDate then
  begin
    diffDate:= Item1^.modification-Item2^.modification;
    //sort dates descending
    if diffDate > 0 then compare := -1 else
    if diffDate < 0 then compare := 1 else
      CompareName;
  end else
   CompareIndex;
end;

procedure TMyShellListView.ColumnClick(Sender: TObject; AColumn: integer);
begin
  if SortColumn = AColumn then SortColumn := -1
  else SortColumn := AColumn;
  Sort;
  If Assigned(FOnSort) then FOnSort(Sender);
end;

procedure TMyShellListView.Reload;
begin
  PopulateWithRoot();
end;

procedure TMyShellListView.BeginUpdate;
begin
  inc(FUpdateCount);
end;

procedure TMyShellListView.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    dec(FUpdateCount);
    if FUpdateCount = 0 then FVirtualScreen.DiscardBitmap;
  end;
  if FSelectedIndex >= ItemCount then FSelectedIndex:= -1;
  FreeAndNil(FVScrollBar);
end;

procedure TMyShellListView.Invalidate;
begin
  FVirtualScreen.DiscardBitmap;
end;

procedure TMyShellListView.Update;
begin
  FVirtualScreen.Update;
end;

procedure TMyShellListView.MakeItemVisible(AIndex: integer);
begin
  FWantedItemVisible := AIndex;
  FVirtualScreen.DiscardBitmap;
end;

procedure TMyShellListView.DoFitColumns(ABitmap: TBGRABitmap; AClientWidth: integer);
var i,j,curSize,totalSize: integer;
  colSizes: array of integer;
  sizeA: integer;
  s: string;
begin
  if (ItemCount = 0) or (ColumnCount = 0) then exit;
  setlength(colSizes,ColumnCount);
  sizeA := ABitmap.TextSize('a').cx;
  for j := 0 to ColumnCount-1 do
    colSizes[j] := sizeA;
  colSizes[FIndexName] := sizeA*8;
  colSizes[FIndexIcon] := DetailIconSize+2;
  for j := 0 to ColumnCount-1 do
  begin
    s := FColumns[j].Name;
    if s <> '' then
    begin
      curSize := ABitmap.TextSize(' '+s+' ').cx;
      if curSize > colSizes[j] then colSizes[j] := curSize;
    end;
  end;
  for i := 0 to ItemCount-1 do
  begin
    for j := 0 to ColumnCount-1 do
    if j <> FIndexName then
    begin
      s := GetItemCell(i,j);
      if s <> '' then
      begin
        curSize := ABitmap.TextSize(' '+s+' ').cx;
        if curSize > colSizes[j] then colSizes[j] := curSize;
      end;
    end;
  end;
  BeginUpdate;
  for j := 0 to ColumnCount-1 do
    FColumns[j].Width := colSizes[j];
  totalSize := 0;
  for j := 0 to ColumnCount-1 do
    inc(totalSize, colSizes[j]);
  if totalSize < AClientWidth then inc(colSizes[FIndexName], AClientWidth-totalSize);
  FColumns[FIndexName].Width := colSizes[FIndexName];
  EndUpdate;
end;

function TMyShellListView.AddColumn(AName: string; AWidth: integer;
  AAlign: TAlignment): integer;
begin
  setlength(FColumns, length(FColumns)+1);
  with FColumns[high(FColumns)] do
  begin
    Name := AName;
    Width := AWidth;
    Align:= AAlign;
  end;
  result := high(FColumns);
end;

function TMyShellListView.GetItemCell(AIndex, AColumn: integer): string;
begin
  result := '';
  if (AIndex < 0) or (AIndex >= ItemCount) then exit;
  if AColumn = FIndexName then result := FData[AIndex].caption;
  if AColumn = FIndexSize then result := FData[AIndex].sizeStr;
  if AColumn = FIndexType then result := FData[AIndex].typeStr;
  if AColumn = FIndexDate then result := FData[AIndex].dateOrDeviceStr;
end;

procedure TMyShellListView.Clear;
var i: integer;
begin
  for I := 0 to ItemCount-1 do
    SetItemImage(I,nil,false);
  FData := nil;
  if FUpdateCount = 0 then FVirtualScreen.DiscardBitmap;
end;

constructor TMyShellListView.Create(AVirtualScreen: TBGRAVirtualScreen);
begin
  BytesCaption:= rsBytes;
  FVirtualScreen := AVirtualScreen;
  FVScrollBar := nil;
  FAllowMultiSelect:= true;
  FVirtualScreen.OnRedraw := @Redraw;
  FVirtualScreen.OnKeyDown := @KeyDown;
  FVirtualScreen.OnDblClick := @MouseDoubleClick;
  FVirtualScreen.OnMouseDown := @MouseDown;
  FVirtualScreen.OnMouseMove := @MouseMove;
  FVirtualScreen.OnMouseUp := @MouseUp;
  FVirtualScreen.OnMouseWheel:= @MouseWheel;
  FIndexIcon := AddColumn('',50,taCenter);
  FIndexName := AddColumn(rsFileName,200,taLeftJustify);
  FIndexSize := AddColumn(rsFileSize,80,taCenter);
  FIndexType := AddColumn(rsFileType,80,taCenter);
  FIndexDate := AddColumn(rsFileDate,80,taCenter);
  FViewStyle:= vsReport;
  FFitColumnNeeded:= true;
  FontHeight := ScaleY(13,OriginalDPI);
  FSelectedIndex:= -1;
  FVerticalScrollPos := 0;
  SmallIconSize := round(ScaleX(64,OriginalDPI)/32)*32;
  LargeIconSize:= SmallIconSize*2;
  DetailIconSize:= SmallIconSize;
  IconPadding := 8;
  FObjectTypes := [otFolders, otNonFolders];
  FSortColumn:= -1;
end;

destructor TMyShellListView.Destroy;
begin
  Clear;
  FVirtualScreen.OnRedraw := nil;
  FVirtualScreen.OnKeyDown := nil;
  FVirtualScreen.OnDblClick := nil;
  FVirtualScreen.OnMouseDown := nil;
  FVirtualScreen.OnMouseMove := nil;
  FVirtualScreen.OnMouseUp := nil;
  FVirtualScreen.OnMouseWheel := nil;
  FreeAndNil(FVScrollBar);
  inherited Destroy;
end;

procedure TMyShellListView.SetItemImage(AIndex: integer; ABitmap: TBGRABitmap;
  AOwned: boolean);
begin
  if (AIndex < 0) or (AIndex >= ItemCount) then exit;
  with FData[AIndex] do
  begin
    if imageOwned then FreeAndNil(image);
    image := ABitmap;
    imageOwned := AOwned and (ABitmap <> nil);
    FVirtualScreen.DiscardBitmap;
  end;
end;

function TMyShellListView.GetItemImage(AIndex: integer): TBGRABitmap;
begin
  if (AIndex < 0) or (AIndex >= ItemCount) then
    result := nil
  else
    Result := FData[AIndex].image;
end;

function TMyShellListView.GetItemFullName(AIndex: integer): string;
begin
  if (AIndex < 0) or (AIndex >= ItemCount) then
    result := ''
  else
    if FRoot = ':' then
      result := FData[AIndex].filename
    else
      Result := IncludeTrailingPathDelimiter(FRoot) + FData[AIndex].filename;
end;

procedure TMyShellListView.SetFocus;
begin
  SafeSetFocus(FVirtualScreen);
end;

function TMyShellListView.GetItemDisplayRect(AIndex: integer): TRect;
begin
  if (AIndex < 0) or (AIndex >= ItemCount) then
    result := EmptyRect
  else
    result := FData[AIndex].displayRect;
end;

procedure TMyShellListView.Sort;
var lst: TList;
  i: integer;
  sortedData: TMyShellListViewData;
begin
  lst := TList.Create;
  for i:= 0 to ItemCount-1 do
    lst.Add(@FData[i]);
  SortTarget := self;
  lst.Sort(@MyShellListViewCompare);
  setlength(sortedData,ItemCount);
  for i := 0 to lst.Count-1 do
    sortedData[i] := PMyShellListViewItemData(lst[i])^;
  FData := sortedData;
  lst.Free;

  if Assigned(FOnSort) then FOnSort(self);
  FVirtualScreen.DiscardBitmap;
end;

procedure TMyShellListView.RemoveItemFromList(AIndex: integer);
var i: integer;
begin
  if (AIndex < 0) or (AIndex >= ItemCount) then exit;
  SetItemImage(AIndex,nil,false);
  for i := AIndex to ItemCount-2 do
    FData[i] := FData[i+1];
  setlength(FData, ItemCount-1);
  FVirtualScreen.DiscardBitmap;
end;

function TMyShellListView.GetItemAt(X, Y: Integer): integer;
var i: integer;
  p : TPoint;
begin
  p := Point(X,Y);
  for i := 0 to ItemCount-1 do
    if PtInRect(FData[i].displayRect,p) then
    begin
      result := i;
      exit;
    end;
  result := -1;
end;

procedure TMyShellListView.DeselectAll;
var i:integer;
  discard: boolean;
begin
  discard:= false;
  for i := 0 to ItemCount-1 do
    if FData[i].isSelected then
    begin
      FData[i].isSelected := false;
      discard := true;
    end;
  FSelectedIndex := -1;
  if discard then FVirtualScreen.DiscardBitmap;
end;

end.

