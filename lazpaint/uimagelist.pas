unit uimagelist;

{$mode objfpc}{$H+}


interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  Grids, StdCtrls, Buttons, ComCtrls, ExtCtrls, Menus,
  LazPaintType, UResourceStrings, UConfig, BGRAImageList, ubrowseimages;

type
  String1D= array of string;

  { TFImageList }

  TFImageList = class(TForm)
    ImageList1: TBGRAImageList;
    lblStatus: TLabel;
    pmAutouncheckOnOpen: TMenuItem;
    pmAutouncheckOnSave: TMenuItem;
    pmUncheckNonExisting: TMenuItem;
    pmRemoveAll: TMenuItem;
    pmRemoveNonexisting: TMenuItem;
    pmRemoveUnchecked: TMenuItem;
    OpenDialog1: TOpenDialog;
    pnlButtonsSmallWindow: TPanel;
    pnlButtonsNormalWindow: TPanel;
    pmRemove: TPopupMenu;
    pmUncheck: TPopupMenu;
    pmAutouncheck: TPopupMenu;
    StringGrid1: TStringGrid;
    tbAutoZoomFit: TToolButton;
    tbOpenNextSW: TToolButton;
    tbNormalWindows: TToolButton;
    tbOpenPrevSW: TToolButton;
    tbUncheckDropDown: TToolButton;
    tbRemoveItem: TToolButton;
    tbOpenPrev: TToolButton;
    tbMiniWindow: TToolButton;
    tbSeparator2: TToolButton;
    tbSeparator3: TToolButton;
    tbAutoUncheck: TToolButton;
    tbUncheckAll: TToolButton;
    tbCheckAll: TToolButton;
    tbOpenNext: TToolButton;
    tbOpenImage: TToolButton;
    tbButtonsNormalWindows: TToolBar;
    tbAddFiles: TToolButton;
    tbSeparator0: TToolButton;
    tbButtonsSmallWindow: TToolBar;
    tbMoveDown: TToolButton;
    tbMoveUp: TToolButton;
    tbRemoveDropDown: TToolButton;
    tbAutoUncheckDropDown: TToolButton;
    procedure EnableButtons;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pmAutouncheckClose(Sender: TObject);
    procedure pmAutouncheckOnOpenClick(Sender: TObject);
    procedure pmAutouncheckOnSaveClick(Sender: TObject);
    procedure pmAutouncheckPopup(Sender: TObject);
    procedure pmRemoveAllClick(Sender: TObject);
    procedure pmRemoveClose(Sender: TObject);
    procedure pmRemoveNonexistingClick(Sender: TObject);
    procedure pmRemovePopup(Sender: TObject);
    procedure pmRemoveUncheckedClick(Sender: TObject);
    procedure pmUncheckClose(Sender: TObject);
    procedure pmUncheckNonExistingClick(Sender: TObject);
    procedure pmUncheckPopup(Sender: TObject);
    procedure StringGrid1SelectCell(Sender: TObject; {%H-}aCol, aRow: Integer; var {%H-}CanSelect: Boolean);
    procedure StringGrid1SetCheckboxState(Sender: TObject; {%H-}ACol, ARow: Integer; const Value: TCheckboxState);
    procedure tbAutoUncheckClick(Sender: TObject);
    procedure tbAutoZoomFitClick(Sender: TObject);
    procedure tbMoveDownClick(Sender: TObject);
    procedure tbMoveUpClick(Sender: TObject);
    procedure tbNormalWindowsClick(Sender: TObject);
    procedure tbOpenImageClick(Sender: TObject);
    procedure tbOpenNextClick(Sender: TObject);
    procedure tbOpenPrevClick(Sender: TObject);
    procedure tbRemoveItemClick(Sender: TObject);
    procedure tbMiniWindowClick(Sender: TObject);
    procedure tbUncheckAllClick(Sender: TObject);
    procedure tbCheckAllClick(Sender: TObject);
    procedure tbCleanListClick(Sender: TObject);
    procedure tbAddFilesClick(Sender: TObject);
    function RowChecked(Row: integer): Boolean;
    function CheckedExist (Verbose: Boolean=True): Boolean;
    function SelectedRow: integer;
    procedure NormalWindow (Normalsize: Boolean= True);
    function SaveModified: boolean;
    function OpenImage (FileName: string): boolean;
    function IsExtensionIsValid (FileName:string): boolean;
    procedure Renumber;
  private
    FBrowseImages: TFBrowseImages;
    WidthNormal: integer;
    HeightNormal: integer;
    WidthMinimal: integer;
    HeightMinimal: integer;
    ManualResize: Boolean;
//    ILConfig: TLazPaintConfig;
    procedure EnsureGridRectVisible(ARect: TGridRect);
  public
      LazPaintInstance: TLazPaintCustomInstance;
      procedure AddFiles (const FileNames: array of String);
  end;

var
  FImageList: TFImageList;
  colNumber: integer=0;
  ColShortFname: integer= 1;
  ColCB: integer= 2;
  ColLongFname: integer= 3;

implementation

{$R *.lfm}

uses LCLType, UFileExtensions;

{ TFImageList }

procedure SelectRow (aStringGrid: TStringGrid; Row: integer; aSetFocus: Boolean= True);
var
   GridRect: TGridRect;
begin
  GridRect:= aStringGrid.Selection;
  GridRect.Top:=Row;
  GridRect.Bottom:=Row;
   if Row> aStringGrid.RowCount-1 then Exit;
   aStringGrid.Row:=Row;
   aStringGrid.Selection:=GridRect;
  if aSetFocus then SafeSetFocus(aStringGrid);
end;

function StringExists (aStringGrid: TStringGrid; Column: integer ;  aStr: String): Boolean;
var
  i:integer;
begin
  Result:=False;
   if aStringGrid.RowCount< (aStringGrid.FixedRows+1) then exit;  //Line1 is header
   for i:= aStringGrid.FixedRows to aStringGrid.RowCount -1 do
     begin
        //TODO: Should I use case sensitive search? Maybe enable it for *nix only
        if CompareStr (LowerCase(aStringGrid.Cells[Column,i]), LowerCase (aStr))= 0 then
           begin
             Result:= True;
             break;
           end;
     end;
end;

function TFImageList.OpenImage (FileName: string): boolean;
begin
  Result:=False;
  if not FileExistsUTF8(FileName) then begin QuestionDlg (rsError, rsFileNotFound, mtError, [mrOk, rsOkay],''); exit; end;
  if not LazPaintInstance.OpenImage(FileName,False) then
     begin QuestionDlg (rsError, rsCannotOpenFile, mtError, [mrOk, rsOkay],''); Exit; end
     else Result:=True;
  if tbAutoZoomFit.Down then LazPaintInstance.Image.ZoomFit;
end;

procedure DeleteRow(aStringGrid: TStringGrid; ARow: integer);
var
  i, j: integer;
begin
  with aStringGrid do
  begin
    for i:=ARow to RowCount-2 do
      for j:=0 to ColCount-1 do
        Cells[j,i]:=Cells[j,i+1];
    RowCount:=RowCount-1;
  end;
end;

procedure TFImageList.NormalWindow (Normalsize: Boolean= True);
begin
  StringGrid1.Visible:=Normalsize;
  pnlButtonsNormalWindow.Visible:=Normalsize;
  pnlButtonsSmallWindow.Visible:=not Normalsize;
  if Normalsize = True then
    begin
       Self.Constraints.MinWidth:=WidthMinimal;
       Self.Constraints.MinHeight:=HeightMinimal;
       Self.Constraints.MaxWidth:=0;
       Self.Constraints.MaxHeight:=0;
       Self.Width:=WidthNormal;
       Self.Height:=HeightNormal;
    end
  else
    begin
      Self.Constraints.MinWidth:=80;
      Self.Constraints.MinHeight:=28;
      Self.Constraints.MaxWidth:=Self.Constraints.MinWidth;
      Self.Constraints.MaxHeight:=Self.Constraints.MinHeight;
      Self.Width:=80;
      Self.Height:=28;
    end;
end; //sub

function PerCent (WholePart: Double; Portion:Double): Double;
begin
     if WholePart <> 0 then
       Result:= (Portion/WholePart)*100
     else Result:=0;
end;

procedure TFImageList.EnableButtons;
var
  TF: Boolean;
  i: integer;
  TodoFiles: integer=0;
begin
  //first line contains headers
  TF:=(StringGrid1.RowCount>=2);
  tbRemoveItem.Enabled:=TF;
  tbCheckAll.Enabled:=TF;
  tbUncheckAll.Enabled:=TF;
  if TF= true then
    begin
    TF:= False;
    for i:=1 to StringGrid1.RowCount-1 do
     if RowChecked(i) then
        begin
          TF:=True;
        end;
  end;
  tbOpenPrev.Enabled:=TF;
  tbOpenNext.Enabled:=TF;
  tbOpenPrevSW.Enabled:=TF;
  tbOpenNextSW.Enabled:=TF;
  tbMoveDown.Enabled:=TF;
  tbMoveUp.Enabled:=TF;
  StringGrid1.Columns.Items[ColShortFname].Width:=StringGrid1.Width- StringGrid1.Columns.Items[colNumber].Width- StringGrid1.Columns.Items[ColCB].Width-5 - {vert scrollbar width, 0 if invisible} (StringGrid1.Width-StringGrid1.ClientWidth);
  tbOpenImage.Enabled:=RowChecked(SelectedRow);
  for i:= 1 to (StringGrid1.RowCount-1) do
     if (StringGrid1.Cells[ColCB,i]='1') then inc (TodoFiles);
  lblStatus.Caption:= StringReplace(rsTotalImages,'%1',IntToStr(StringGrid1.RowCount-1),[])+ ';  '+
     StringReplace(rsToDoImages,'%1',IntToStr(TodoFiles),[]) + '  '
    +'(' + IntToStr(trunc(percent(StringGrid1.RowCount-1,TodoFiles)))+ '%)';
end;

procedure TFImageList.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
   LazPaintInstance.ImageListWindowVisible:=False;
   CanClose:=False;
end;

procedure TFImageList.FormCreate(Sender: TObject);
begin
  WidthNormal:=500;
  HeightNormal:=360;
  WidthMinimal:=340;
  HeightMinimal:=200;
  ManualResize:=True;

  Self.Constraints.MinWidth:=WidthMinimal;
  Self.Constraints.MinHeight:=HeightMinimal;
  Self.Constraints.MaxWidth:=0;
  Self.Constraints.MaxHeight:=0;
  Self.Width:=WidthNormal;
  Self.Height:=HeightNormal;

  StringGrid1.Columns.Items[colNumber].Title.Column.Title.Caption:=rsNumber;
  StringGrid1.Columns.Items[ColShortFname].Title.Column.Title.Caption:=rsFilename;
  StringGrid1.Columns.Items[ColCB].Title.Column.Title.Caption:=rsToDo;
  StringGrid1.Columns.Items[ColCB].Title.Column.Title.Alignment := taCenter;
  StringGrid1.Columns.Items[ColLongFname].Title.Caption:='';
  StringGrid1.Columns.Items[ColCB].ButtonStyle:=cbsCheckboxColumn;
  StringGrid1.Columns.Items[colNumber].ReadOnly:=True;
  StringGrid1.Columns.Items[ColShortFname].ReadOnly:=True;
  StringGrid1.Columns.Items[ColCB].ReadOnly:=False;
  StringGrid1.Columns.Items[ColLongFname].ReadOnly:=True;
  StringGrid1.Columns.Items[colNumber].Width:=30;
  StringGrid1.Columns.Items[ColCB].Width:=80;
  StringGrid1.Columns.Items[ColShortFname].Width:=StringGrid1.Width- StringGrid1.Columns.Items[colNumber].Width- StringGrid1.Columns.Items[ColCB].Width-5;
  StringGrid1.Columns.Items[ColLongFname].Width:=0;
  StringGrid1.Columns.Items[ColLongFname].Visible:=False;
  OpenDialog1.Filter:= GetExtensionFilter([eoReadable]);
  lblStatus.Caption:='';
  lblStatus.AutoSize := true;
  EnableButtons;
end;

procedure TFImageList.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FBrowseImages);
end;

function TFImageList.IsExtensionIsValid (FileName: string): boolean;
begin
  result := IsExtensionReadable(Filename);
end;

procedure TFImageList.AddFiles (const FileNames: array of String);
var
  PrevRowCount, Row: integer;
  i: integer;
  shouldOpenFirst: boolean;
begin
  if Length(FileNames) > 0 then
  begin
       shouldOpenFirst := StringGrid1.RowCount-StringGrid1.FixedRows = 0;
       LazPaintInstance.Config.SetImageListLastFolder(ExtractFileDir(FileNames[0]));
       PrevRowCount:=StringGrid1.RowCount;
       Row:=PrevRowCount;
       StringGrid1.RowCount:= PrevRowCount+Length(FileNames);
       for i:= 0 to length(FileNames)- 1 do
         begin
           if (not StringExists (StringGrid1,ColLongFname,FileNames[i])) and (IsExtensionIsValid(FileNames[i]))then
             begin
               StringGrid1.Cells[colNumber,Row]:=IntToStr (Row);
               StringGrid1.Cells[ColShortFname,Row]:=ExtractFileName(FileNames[i]);
               StringGrid1.Cells[ColCB,Row]:='1';   //Checkbox is checked
               StringGrid1.Cells[ColLongFname,Row]:=FileNames[i];
               Inc(Row);
             end;  //if StringExists
         end;  //for i
       StringGrid1.RowCount:=Row;
       if shouldOpenFirst then
       begin
            tbOpenImageClick(nil);
            if tbAutoZoomFit.Down then LazPaintInstance.Image.ZoomFit;
       end;
  end; //if
  EnableButtons;
end;

procedure TFImageList.FormDropFiles(Sender: TObject;  const FileNames: array of String);
begin
   AddFiles(FileNames);
end;

procedure TFImageList.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if ssAlt in Shift then
    begin
      case key of
        VK_LEFT:  begin
                    tbOpenPrevClick(Sender);
                    key := 0;
                  end;
        VK_RIGHT: begin
                    tbOpenNextClick(Sender);
                    key := 0;
                  end;
        end;
    end;
end;

procedure TFImageList.FormResize(Sender: TObject);
begin
  if ManualResize then
  begin
    StringGrid1.Left := 0;
    StringGrid1.Top := 0;
    StringGrid1.Width:= ClientWidth;
    StringGrid1.Height:= ClientHeight - pnlButtonsNormalWindow.Height -lblStatus.Height - 4;
    StringGrid1.Columns.Items[ColShortFname].Width:=StringGrid1.Width- StringGrid1.Columns.Items[colNumber].Width- StringGrid1.Columns.Items[ColCB].Width-5 - {vert scrollbar width, 0 if invisible} (StringGrid1.Width-StringGrid1.ClientWidth);
    HeightNormal:=Self.Height;
    WidthNormal:=Self.Width;
  end;
  ManualResize:=True;
end;

procedure TFImageList.FormShow(Sender: TObject);
begin
  tbAutoUncheck.Down:= LazPaintInstance.Config.ImageListAutoUncheck;
  tbAutoZoomFit.Down:= LazPaintInstance.Config.ImageListAutoZoom;
  pmAutouncheckOnSave.Checked:=LazPaintInstance.Config.ImageListAutoUncheckMode=0;
  pmAutouncheckOnOpen.Checked:=LazPaintInstance.Config.ImageListAutoUncheckMode=1;
end;

procedure TFImageList.pmAutouncheckClose(Sender: TObject);
begin
  tbAutoUncheckDropDown.Down := false;
end;

procedure TFImageList.pmAutouncheckOnOpenClick(Sender: TObject);
begin
   LazPaintInstance.Config.SetImageListAutoUncheckMode(1);
   pmAutouncheckOnOpen.Checked := true;
   pmAutouncheckOnSave.Checked := false;
   tbAutoUncheck.Down:=True;
end;

procedure TFImageList.pmAutouncheckOnSaveClick(Sender: TObject);
begin
   LazPaintInstance.Config.SetImageListAutoUncheckMode(0);
   pmAutouncheckOnOpen.Checked := false;
   pmAutouncheckOnSave.Checked := true;
   tbAutoUncheck.Down:=True;
end;

procedure TFImageList.pmAutouncheckPopup(Sender: TObject);
begin
  tbAutoUncheckDropDown.Down := true;
end;

procedure TFImageList.pmRemoveAllClick(Sender: TObject);
begin
  StringGrid1.Clean;
  StringGrid1.RowCount:=1;
  EnableButtons;
end;

procedure TFImageList.pmRemoveClose(Sender: TObject);
begin
  tbRemoveDropDown.Down := false;
end;

procedure TFImageList.pmRemoveNonexistingClick(Sender: TObject);
var
  i:integer;
  needRenumber: boolean;
begin
  needRenumber := false;
  for i:=StringGrid1.RowCount-1 downto 1 do
   if not FileExistsUTF8(StringGrid1.Cells[ColLongFname,i])
      then begin DeleteRow(StringGrid1,i); needRenumber :=true; end;
  if needRenumber then Renumber;
  EnableButtons;
end;

procedure TFImageList.pmRemovePopup(Sender: TObject);
begin
  tbRemoveDropDown.Down := true;
end;

procedure TFImageList.Renumber;
var
  i:integer;
begin
  if StringGrid1.RowCount>1 then
     for i:=0 to StringGrid1.RowCount-1 do
        StringGrid1.Cells[colNumber,i]:=IntToStr(i);
end;

procedure TFImageList.EnsureGridRectVisible(ARect: TGridRect);
var VisibleRows, SelectedRows, MinTopRow, MaxTopRow: integer;
  dummyCol,LastVisibleRow: integer;
begin
  StringGrid1.Update;
  StringGrid1.MouseToCell(0,StringGrid1.ClientHeight-StringGrid1.DefaultRowHeight,{%H-}dummyCol,{%H-}LastVisibleRow);
  VisibleRows := LastVisibleRow-StringGrid1.TopRow+1;
  SelectedRows := ARect.Bottom-ARect.Top+1;
  MaxTopRow := ARect.Top;
  MinTopRow := ARect.Bottom - VisibleRows + 1;
  if MinTopRow < 0 then MinTopRow := 0;

  if StringGrid1.TopRow > MaxTopRow then StringGrid1.TopRow := MaxTopRow
  else if (SelectedRows <= VisibleRows) and (StringGrid1.TopRow < MinTopRow) then StringGrid1.TopRow := MinTopRow;
end;

procedure TFImageList.pmRemoveUncheckedClick(Sender: TObject);
var
  i:integer;
  needRenumber: boolean;
begin
  needRenumber := false;
  for i:=StringGrid1.RowCount-1 downto 1 do
   if not RowChecked(i)
      then begin DeleteRow(StringGrid1,i); needRenumber :=true; end;
  if needRenumber then Renumber;
  EnableButtons;
end;

procedure TFImageList.pmUncheckClose(Sender: TObject);
begin
  tbUncheckDropDown.Down := false;
end;

procedure TFImageList.pmUncheckNonExistingClick(Sender: TObject);
var
i: integer;
begin
  for i:= 1 to StringGrid1.RowCount-1 do
     if not FileExistsUTF8(StringGrid1.Cells[ColLongFname,i])
         then StringGrid1.Cells[ColCB,i]:='0';
  EnableButtons;
end;

procedure TFImageList.pmUncheckPopup(Sender: TObject);
begin
  tbUncheckDropDown.Down := true;
end;

procedure TFImageList.StringGrid1SelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
begin
    tbOpenImage.Enabled:=RowChecked(aRow);
end;

procedure TFImageList.StringGrid1SetCheckboxState(Sender: TObject; ACol,
  ARow: Integer; const Value: TCheckboxState);
begin
  if Value=cbChecked
    then begin StringGrid1.Cells[ColCB,ARow]:='1'; end
    else begin StringGrid1.Cells[ColCB,ARow]:='0'; end;
  EnableButtons;
end;

procedure TFImageList.tbAutoUncheckClick(Sender: TObject);
begin
  LazPaintInstance.Config.SetImageListAutoUncheck(tbAutoUncheck.Down);
end;

procedure TFImageList.tbAutoZoomFitClick(Sender: TObject);
begin
    LazPaintInstance.Config.SetImageListAutoZoom(tbAutoZoomFit.Down);
end;

procedure TFImageList.tbMoveDownClick(Sender: TObject);
var
   SelRect: TGridRect;
begin
   if (StringGrid1.RowCount>2) and (StringGrid1.Selection.Bottom< StringGrid1.RowCount-1) then
   begin
     SelRect:=StringGrid1.Selection;
     StringGrid1.MoveColRow(False,SelRect.Bottom+1,SelRect.Top);
     SelRect.Top += 1;
     SelRect.Bottom += 1;
     StringGrid1.Selection:=SelRect;
     EnsureGridRectVisible(SelRect);
   end;
end;

procedure TFImageList.tbMoveUpClick(Sender: TObject);
var
   SelRect: TGridRect;
begin
  if (StringGrid1.RowCount>2) and (StringGrid1.Selection.Top>1) then
  begin
    SelRect:=StringGrid1.Selection;
    StringGrid1.MoveColRow(False,SelRect.Top-1,SelRect.Bottom);
    SelRect.Top -= 1;
    SelRect.Bottom -= 1;
    StringGrid1.Selection:=SelRect;
    EnsureGridRectVisible(SelRect);
  end;
end;

procedure TFImageList.tbNormalWindowsClick(Sender: TObject);
begin
  ManualResize:=False;
  NormalWindow;
end;

function TFImageList.RowChecked(Row: integer): Boolean;
begin
   if StringGrid1.Cells[ColCB, Row]='1'
      then Result:=True
      else Result:=False;
end;

procedure TFImageList.tbOpenImageClick(Sender: TObject);
begin
   if SaveModified= True then
   begin
   if (tbAutoUncheck.Down and pmAutouncheckOnOpen.Checked) or (tbAutoUncheck.Down and pmAutouncheckOnSave.Checked and LazPaintInstance.Image.IsFileModifiedAndSaved) then
       begin StringGrid1.Cells[ColCB, SelectedRow]:='0'; EnableButtons; end;
     OpenImage (StringGrid1.Cells[ColLongFname,SelectedRow]);
   end;
end;

function TFImageList.SelectedRow: integer;
begin
  Result:=StringGrid1.Selection.Top;
end;

function TFImageList.CheckedExist (Verbose: Boolean= True) : Boolean;
var
  i: integer;
begin
  Result:=False;
  for i:=1 to StringGrid1.RowCount -1 do
      if RowChecked(i) then
        begin
          Result:=True;
          Break;
        end;
  if (Result = false) and (Verbose= True) then QuestionDlg (rsInformation, rsThereAreNoCheckedItems, mtInformation, [mrOk, rsOkay],'');
end;

function TFImageList.SaveModified: Boolean;
begin
  Result:=True;
  if LazPaintInstance.Image.IsFileModified=True then
    case QuestionDlg (rsFileNotSaved, rsSaveChanges, mtWarning, [mrYes,rsYes,mrNo,rsNoAndProceedToNext,mrCancel,rsCancel],'') of
       mrYes :
          begin
            if Length(LazPaintInstance.Image.currentFilenameUTF8)=0 //TODO: Should I use < something. For Windows length cannot be less than 4.
                then begin QuestionDlg (rsInformation, rsThereIsNoFileNameGivenForThisFileUseSaveAs, mtInformation, [mrOk, rsOkay],''); Exit; end;
             try
                LazPaintInstance.Image.SaveToFileUTF8(LazPaintInstance.Image.currentFilenameUTF8);
             Except
                on ex:exception do
                begin
                  LazPaintInstance.ShowError('FileSaveAs',ex.Message);
                  Result:=False;
                end;
             end;
          end;
        mrCancel: Result:=False;
        end;
end;

procedure TFImageList.tbOpenNextClick(Sender: TObject);
var
  i:integer;
begin
  if not CheckedExist then Exit;
  if SaveModified= False then exit;
  if (tbAutoUncheck.Down and pmAutouncheckOnSave.Checked and LazPaintInstance.Image.IsFileModifiedAndSaved) then
     begin StringGrid1.Cells[ColCB, SelectedRow]:='0'; EnableButtons; end;
  if SelectedRow<StringGrid1.RowCount-1
     then
       for i:= SelectedRow + 1 to StringGrid1.RowCount-1 do
         if RowChecked(i) then
           begin
              OpenImage(StringGrid1.Cells[ColLongFname,i]);
              SelectRow(StringGrid1,i, pnlButtonsNormalWindow.Visible);
              if (tbAutoUncheck.Down and pmAutouncheckOnOpen.Checked) then
              begin StringGrid1.Cells[ColCB, SelectedRow]:='0'; EnableButtons; end;
              Exit;
           end;
       for i:= 1 to SelectedRow-1 do
          if RowChecked(i) then
            begin
              OpenImage(StringGrid1.Cells[ColLongFname,i]);
              SelectRow(StringGrid1,i,pnlButtonsNormalWindow.Visible);
              if (tbAutoUncheck.Down and pmAutouncheckOnOpen.Checked) then
              begin StringGrid1.Cells[ColCB, SelectedRow]:='0'; EnableButtons; end;
              Exit;
            end;
end;

procedure TFImageList.tbOpenPrevClick(Sender: TObject);
var
    i:integer;
begin
  if not CheckedExist then Exit;
  if SaveModified= False then exit;
  if (tbAutoUncheck.Down and pmAutouncheckOnSave.Checked and LazPaintInstance.Image.IsFileModifiedAndSaved) then
     begin StringGrid1.Cells[ColCB, SelectedRow]:='0'; EnableButtons; end;
  if SelectedRow>1 then
     for i:= SelectedRow -1 downto 1 do
       if RowChecked(i) then
         begin
            OpenImage(StringGrid1.Cells[ColLongFname,i]);
            SelectRow(StringGrid1,i,pnlButtonsNormalWindow.Visible);
            if (tbAutoUncheck.Down and pmAutouncheckOnOpen.Checked) then
            begin StringGrid1.Cells[ColCB, SelectedRow]:='0'; EnableButtons; end;
            Exit;
         end; //if
  for i:= StringGrid1.RowCount-1 downto SelectedRow+1 do
    if RowChecked(i) then
      begin
        OpenImage(StringGrid1.Cells[ColLongFname,i]);
        SelectRow(StringGrid1,i,pnlButtonsNormalWindow.Visible);
        if (tbAutoUncheck.Down and pmAutouncheckOnOpen.Checked) then
        begin StringGrid1.Cells[ColCB, SelectedRow]:='0'; EnableButtons; end;
        Exit;
      end; //if
end;

procedure TFImageList.tbRemoveItemClick(Sender: TObject);
var
  i:integer;
  sTop: integer;
begin
  if (StringGrid1.Selection.Top= 0) and (StringGrid1.Selection.Bottom=0) then Exit;
  if StringGrid1.Selection.Top=0 then sTop:=1 else sTop:=StringGrid1.Selection.Top;
  for i:=sTop to  StringGrid1.Selection.Bottom do
    DeleteRow(StringGrid1, i);
  EnableButtons;
end;

procedure TFImageList.tbMiniWindowClick(Sender: TObject);
begin
  ManualResize:=False;
  NormalWindow(False);
end;

procedure TFImageList.tbUncheckAllClick(Sender: TObject);
var
  i: integer;
begin
    for i:= 1 to StringGrid1.RowCount-1 do
      StringGrid1.Cells[ColCB,i]:='0';
    EnableButtons;
end;

procedure TFImageList.tbCheckAllClick(Sender: TObject);
  var
  i: integer;
begin
  for i:= 1 to StringGrid1.RowCount-1 do
    StringGrid1.Cells[ColCB,i]:='1';
  EnableButtons;
end;

procedure TFImageList.tbCleanListClick(Sender: TObject);
begin
  StringGrid1.Clean;
  StringGrid1.RowCount:=1;
  EnableButtons;
end;

function StringsToStringArray(const aStrings: TStrings): String1D;
var
   i: integer;
   TempVar: array of String;
begin
  SetLength(TempVar, aStrings.Count);
  For i := 0 To aStrings.Count-1 Do
    TempVar[i] := aStrings[i];
  Result:= TempVar;
end;

procedure TFImageList.tbAddFilesClick(Sender: TObject);
var topMostInfo: TTopMostInfo;
   fileNames: array of string;
   i: integer;

begin
  topMostInfo := LazPaintInstance.HideTopmost;

  if LazPaintInstance.Config.DefaultUseImageBrowser then
  begin
    if not assigned(FBrowseImages) then
    begin
      FBrowseImages := TFBrowseImages.Create(self);
      FBrowseImages.LazPaintInstance := LazPaintInstance;
    end;

    if FBrowseImages.ShowModal = mrOK then
    begin
      setlength(fileNames,FBrowseImages.SelectedFileCount);
      for i := 0 to high(fileNames) do
        fileNames[i] := FBrowseImages.SelectedFile[i];
      AddFiles(Filenames);
    end;
  end else
  begin
    if Length(LazPaintInstance.Config.ImageListLastFolder)>0 then OpenDialog1.InitialDir:=LazPaintInstance.Config.ImageListLastFolder;
    if OpenDialog1.Execute= True then AddFiles(StringsToStringArray(OpenDialog1.Files));
  end;

  LazPaintInstance.ShowTopmost(topMostInfo);
end;


end.

