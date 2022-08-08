// SPDX-License-Identifier: GPL-3.0-only
unit uimagelist;

{$mode objfpc}{$H+}


interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  Grids, StdCtrls, Buttons, ComCtrls, ExtCtrls, Menus, UImageObservation,
  LazPaintType, UResourceStrings, UConfig, BGRAImageList, ubrowseimages,
  UScripting;

type
  String1D= array of string;

  { TFImageList }

  TFImageList = class(TForm)
    ImageList1: TBGRAImageList;
    lblStatus: TLabel;
    pmAutouncheckOnOpen: TMenuItem;
    pmAutouncheckOnSave: TMenuItem;
    pmUncheckNonexistent: TMenuItem;
    pmRemoveAll: TMenuItem;
    pmRemoveNonexistent: TMenuItem;
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
    procedure pmRemoveNonexistentClick(Sender: TObject);
    procedure pmRemovePopup(Sender: TObject);
    procedure pmRemoveUncheckedClick(Sender: TObject);
    procedure pmUncheckClose(Sender: TObject);
    procedure pmUncheckNonexistentClick(Sender: TObject);
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
    function GetRowChecked(Row: integer): Boolean;
    function CheckedExist (Verbose: Boolean=True): Boolean;
    function GetSelectedRow: integer;
    procedure NormalWindow (Normalsize: Boolean= True);
    function SaveModified: boolean;
    function OpenImage (FileName: string): boolean;
    function IsExtensionIsValid (FileName:string): boolean;
    procedure Renumber;
  private
    FLazPaintInstance: TLazPaintCustomInstance;
    FBrowseImages: TFBrowseImages;
    WidthNormal: integer;
    HeightNormal: integer;
    WidthMinimal: integer;
    HeightMinimal: integer;
    ManualResize: Boolean;
    FResizedImageList: TBGRAImageList;
//    ILConfig: TLazPaintConfig;
    procedure EnsureGridRectVisible(ARect: TGridRect);
    function GetFileCount: integer;
    function GetLongFileName(AIndex: integer): string;
    procedure ImageSaving({%H-}AEvent: TLazPaintImageObservationEvent);
    function ScriptAddFiles(AVars: TVariableSet): TScriptResult;
    function ScriptGetAutoUncheckMode(AVars: TVariableSet): TScriptResult;
    function ScriptGetAutoZoomFit(AVars: TVariableSet): TScriptResult;
    function ScriptGetFileChecked(AVars: TVariableSet): TScriptResult;
    function ScriptGetFileCount(AVars: TVariableSet): TScriptResult;
    function ScriptGetFileName(AVars: TVariableSet): TScriptResult;
    function ScriptGetSelectedIndex(AVars: TVariableSet): TScriptResult;
    function ScriptIndexOfFileName(AVars: TVariableSet): TScriptResult;
    function ScriptOpenFirst(AVars: TVariableSet): TScriptResult;
    function ScriptOpenNext(AVars: TVariableSet): TScriptResult;
    function ScriptOpenPrevious(AVars: TVariableSet): TScriptResult;
    function ScriptOpenSelected(AVars: TVariableSet): TScriptResult;
    function ScriptRemoveAll({%H-}AVars: TVariableSet): TScriptResult;
    function ScriptRemoveIndex(AVars: TVariableSet): TScriptResult;
    function ScriptRemoveNonExistent({%H-}AVars: TVariableSet): TScriptResult;
    function ScriptRemoveUnchecked({%H-}AVars: TVariableSet): TScriptResult;
    function ScriptSetAutoUncheckMode(AVars: TVariableSet): TScriptResult;
    function ScriptSetAutoZoomFit(AVars: TVariableSet): TScriptResult;
    function ScriptSetFileChecked(AVars: TVariableSet): TScriptResult;
    function ScriptSetSelectedIndex(AVars: TVariableSet): TScriptResult;
    function ScriptUncheckNonExistent({%H-}AVars: TVariableSet): TScriptResult;
    procedure SetLazPaintInstance(AValue: TLazPaintCustomInstance);
    procedure SetRowChecked(AIndex: integer; AValue: boolean);
    procedure RegisterScriptFunctions(ARegister: boolean);
    procedure CallScriptFunction(AName: string); overload;
    function CallScriptFunction(AVars: TVariableSet): TScriptResult; overload;
    procedure SetSelectedRow(AValue: integer);
  public
    function AddFiles (const FileNames: array of String; AAutoOpen: boolean): integer;
    property FileChecked[AIndex: integer]: boolean read GetRowChecked write SetRowChecked;
    property LongFileName[AIndex: integer]: string read GetLongFileName;
    property FileCount: integer read GetFileCount;
    property LazPaintInstance: TLazPaintCustomInstance read FLazPaintInstance write SetLazPaintInstance;
    property SelectedRow: integer read GetSelectedRow write SetSelectedRow;
  end;

var
  FImageList: TFImageList;
  colNumber: integer=0;
  ColShortFname: integer= 1;
  ColCB: integer= 2;
  ColLongFname: integer= 3;

implementation

{$R *.lfm}

uses LCLType, UFileExtensions, LazFileUtils, UFileSystem, LCScaleDPI;

{ TFImageList }

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
  if not FileManager.FileExists(FileName) then begin QuestionDlg (rsError, rsFileNotFound, mtError, [mrOk, rsOkay],''); exit; end;
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
  lblStatus.Visible := Normalsize;
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
      Self.Constraints.MinWidth:= DoScaleX(80, OriginalDPI);
      Self.Constraints.MinHeight:=DoScaleY(28, OriginalDPI);
      Self.Constraints.MaxWidth:=Self.Constraints.MinWidth;
      Self.Constraints.MaxHeight:=Self.Constraints.MinHeight;
      Self.Width:= DoScaleX(80, OriginalDPI);
      Self.Height:=DoScaleY(28, OriginalDPI);
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
  TF:= FileCount > 0;
  tbRemoveItem.Enabled:=TF;
  tbCheckAll.Enabled:=TF;
  tbUncheckAll.Enabled:=TF;
  StringGrid1.Enabled := TF;

  if TF then
  begin
    TF:= False;
    for i:=1 to FileCount do
      if FileChecked[i] then TF := true;
  end;
  tbOpenPrev.Enabled:=TF;
  tbOpenNext.Enabled:=TF;
  tbOpenPrevSW.Enabled:=TF;
  tbOpenNextSW.Enabled:=TF;
  tbMoveDown.Enabled:=TF;
  tbMoveUp.Enabled:=TF;
  StringGrid1.Columns.Items[ColShortFname].Width:=StringGrid1.Width- StringGrid1.Columns.Items[colNumber].Width- StringGrid1.Columns.Items[ColCB].Width-5 - {vert scrollbar width, 0 if invisible} (StringGrid1.Width-StringGrid1.ClientWidth);
  tbOpenImage.Enabled:= TF and FileChecked[SelectedRow];
  for i:= 1 to (FileCount) do
    if FileChecked[i] then inc(TodoFiles);
  lblStatus.Caption:= StringReplace(rsTotalImages,'%1',IntToStr(FileCount),[])+ ';  '+
     StringReplace(rsToDoImages,'%1',IntToStr(TodoFiles),[]) + '  '
    +'(' + IntToStr(trunc(percent(FileCount,TodoFiles)))+ '%)';
end;

procedure TFImageList.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
   LazPaintInstance.ImageListWindowVisible:=False;
   CanClose:=False;
end;

procedure TFImageList.FormCreate(Sender: TObject);
begin
  BorderStyle := ToolWindowSizeable;
  FormStyle := ToolWindowStyle;
  WidthNormal:= DoScaleX(500, OriginalDPI);
  HeightNormal:=DoScaleY(360, OriginalDPI);
  WidthMinimal:=DoScaleX(340, OriginalDPI);
  HeightMinimal:=DoScaleY(200, OriginalDPI);
  ManualResize:=True;

  Self.Constraints.MinWidth:=WidthMinimal;
  Self.Constraints.MinHeight:=HeightMinimal;
  Self.Constraints.MaxWidth:=0;
  Self.Constraints.MaxHeight:=0;
  Self.Width:=WidthNormal;
  Self.Height:=HeightNormal;

  if DoScaleX(ImageList1.Width, OriginalDPI) <> ImageList1.Width then
  begin
    FResizedImageList := TBGRAImageList.Create(self);
    ScaleImageList(ImageList1, DoScaleX(ImageList1.Width, OriginalDPI),
      DoScaleY(ImageList1.Height, OriginalDPI), FResizedImageList);
    tbButtonsNormalWindows.Images := FResizedImageList;
    tbButtonsSmallWindow.Images := FResizedImageList;
  end;
  tbButtonsNormalWindows.ButtonHeight := tbButtonsNormalWindows.Images.Height + DoScaleY(4, OriginalDPI);
  tbButtonsNormalWindows.Align:= alClient;
  tbButtonsSmallWindow.ButtonHeight := tbButtonsSmallWindow.Images.Height + DoScaleY(4, OriginalDPI);
  tbButtonsSmallWindow.Align:= alClient;
  pnlButtonsNormalWindow.Height := tbButtonsNormalWindows.ButtonHeight + DoScaleY(4, OriginalDPI);
  pnlButtonsNormalWindow.Align := alBottom;
  pnlButtonsSmallWindow.Height := tbButtonsSmallWindow.ButtonHeight + DoScaleY(4, OriginalDPI);
  pnlButtonsSmallWindow.Align := alBottom;

  lblStatus.Caption:='';
  lblStatus.AutoSize := true;
  lblStatus.Align := alBottom;

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
  StringGrid1.Columns.Items[colNumber].Width:= DoScaleX(30, OriginalDPI);
  StringGrid1.Columns.Items[ColCB].Width:= DoScaleX(80, OriginalDPI);
  StringGrid1.Columns.Items[ColShortFname].Width:=StringGrid1.Width- StringGrid1.Columns.Items[colNumber].Width- StringGrid1.Columns.Items[ColCB].Width-5;
  StringGrid1.Columns.Items[ColLongFname].Width:=0;
  StringGrid1.Columns.Items[ColLongFname].Visible:=False;
  StringGrid1.Align := alClient;

  OpenDialog1.Filter:= GetExtensionFilter([eoReadable]);
  EnableButtons;
end;

procedure TFImageList.FormDestroy(Sender: TObject);
begin
  if Assigned(LazPaintInstance.Image) then
  begin
    LazPaintInstance.Image.OnImageSaving.RemoveObserver(@ImageSaving);
    LazPaintInstance.Image.OnImageExport.RemoveObserver(@ImageSaving);
  end;
  FreeAndNil(FBrowseImages);
end;

function TFImageList.IsExtensionIsValid (FileName: string): boolean;
begin
  result := IsExtensionReadable(Filename);
end;

function TFImageList.AddFiles (const FileNames: array of String; AAutoOpen: boolean): integer;
var
  PrevRowCount, Row: integer;
  i: integer;
  shouldOpenFirst: boolean;
begin
  result := 0;
  if Length(FileNames) > 0 then
  begin
     shouldOpenFirst := AAutoOpen and (FileCount = 0);
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
             inc(result);
           end;  //if StringExists
       end;  //for i
     StringGrid1.RowCount:=Row;
     if shouldOpenFirst then
     begin
        tbOpenImageClick(nil);
        if tbAutoZoomFit.Down then LazPaintInstance.Image.ZoomFit;
     end;
     EnableButtons;
  end; //if
end;

procedure TFImageList.FormDropFiles(Sender: TObject;  const FileNames: array of String);
begin
   AddFiles(FileNames, true);
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
    StringGrid1.Columns.Items[ColShortFname].Width:=StringGrid1.Width- StringGrid1.Columns.Items[colNumber].Width- StringGrid1.Columns.Items[ColCB].Width-5 - {vert scrollbar width, 0 if invisible} (StringGrid1.Width-StringGrid1.ClientWidth);
    HeightNormal:=Self.Height;
    WidthNormal:=Self.Width;
  end;
  ManualResize:=True;
end;

procedure TFImageList.FormShow(Sender: TObject);
begin
  EnsureVisible(False);
  tbAutoUncheck.Down:= LazPaintInstance.Config.ImageListAutoUncheck;
  tbAutoZoomFit.Down:= LazPaintInstance.Config.ImageListAutoZoom;
  pmAutouncheckOnSave.Checked:=LazPaintInstance.Config.ImageListAutoUncheckMode=0;
  pmAutouncheckOnOpen.Checked:=LazPaintInstance.Config.ImageListAutoUncheckMode=1;
  if Assigned(LazPaintInstance.Image) then
  begin
    LazPaintInstance.Image.OnImageSaving.AddObserver(@ImageSaving);
    LazPaintInstance.Image.OnImageExport.AddObserver(@ImageSaving);
  end;
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
  CallScriptFunction('ImageListRemoveAll');
end;

procedure TFImageList.pmRemoveClose(Sender: TObject);
begin
  tbRemoveDropDown.Down := false;
end;

procedure TFImageList.pmRemoveNonexistentClick(Sender: TObject);
begin
  CallScriptFunction('ImageListRemoveNonexistent');
end;

procedure TFImageList.pmRemovePopup(Sender: TObject);
begin
  tbRemoveDropDown.Down := true;
end;

procedure TFImageList.Renumber;
var
  i:integer;
begin
  if FileCount > 0 then
     for i:= 1 to FileCount do
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

function TFImageList.GetFileCount: integer;
begin
  result := StringGrid1.RowCount - StringGrid1.FixedRows;
end;

function TFImageList.GetLongFileName(AIndex: integer): string;
begin
  result := StringGrid1.Cells[ColLongFname, AIndex];
end;

procedure TFImageList.ImageSaving(AEvent: TLazPaintImageObservationEvent);
var
  i: Integer;
begin
  if not pmAutouncheckOnSave.Checked then exit;
  for i:= 1 to FileCount do
   if LongFileName[i] = LazPaintInstance.Image.currentFilenameUTF8 then
     FileChecked[i] := false;
end;

function TFImageList.ScriptAddFiles(AVars: TVariableSet): TScriptResult;
var
  files: TScriptVariableReference;
  fileArray: array of string;
  i: Integer;
begin
  files := AVars.GetVariable('FileNames');
  fileArray := nil;
  setLength(fileArray, AVars.GetListCount(files));
  for i := 0 to high(fileArray) do
    fileArray[i] := AVars.GetStringAt(files, i);
  AVars.Integers['Result'] := AddFiles(fileArray, false);
  result := srOk;
end;

function TFImageList.ScriptGetAutoUncheckMode(AVars: TVariableSet): TScriptResult;
begin
  if tbAutoUncheck.Down then
  begin
    if pmAutouncheckOnOpen.Checked then
      AVars.Strings['Result'] := 'UncheckOnOpen'
    else
      AVars.Strings['Result'] := 'UncheckOnSave';
  end else
    AVars.Strings['Result'] := 'UncheckOff';
  result := srOk;
end;

function TFImageList.ScriptGetAutoZoomFit(AVars: TVariableSet): TScriptResult;
begin
  AVars.Booleans['Result'] := tbAutoZoomFit.Down;
  result := srOk;
end;

function TFImageList.ScriptGetFileChecked(AVars: TVariableSet): TScriptResult;
var
  idx: Int64;
begin
  if AVars.IsDefined('Index') then
  begin
    idx := AVars.Integers['Index'];
    if (idx < 1) or (idx > FileCount) then exit(srInvalidParameters);
    AVars.Booleans['Result'] := FileChecked[idx];
    result := srOk;
  end else
  begin
    if SelectedRow >= 1 then
    begin
      AVars.Booleans['Result'] := FileChecked[SelectedRow];
      result := srOk;
    end else
      result := srException;
  end;
end;

function TFImageList.ScriptGetFileCount(AVars: TVariableSet): TScriptResult;
begin
  AVars.Integers['Result'] := FileCount;
  result := srOk;
end;

function TFImageList.ScriptGetFileName(AVars: TVariableSet): TScriptResult;
var
  idx: Int64;
begin
  if AVars.IsDefined('Index') then
  begin
    idx := AVars.Integers['Index'];
    if (idx < 1) or (idx > FileCount) then exit(srInvalidParameters);
    AVars.Strings['Result'] := LongFileName[idx];
    result := srOk;
  end else
  begin
    if SelectedRow >= 1 then
    begin
      AVars.Strings['Result'] := LongFileName[SelectedRow];
      result := srOk;
    end else
      result := srException;
  end;
end;

function TFImageList.ScriptGetSelectedIndex(AVars: TVariableSet): TScriptResult;
begin
  AVars.Integers['Result'] := SelectedRow;
  result := srOk;
end;

function TFImageList.ScriptIndexOfFileName(AVars: TVariableSet): TScriptResult;
var
  fn: String;
  i: Integer;
begin
  fn := AVars.Strings['FileName'];
  for i := 1 to FileCount do
    if LongFileName[i] = fn then
    begin
      AVars.Integers['Result'] := i;
      exit(srOk);
    end;
  AVars.Remove('Result');
  result := srOk;
end;

function TFImageList.ScriptOpenFirst(AVars: TVariableSet): TScriptResult;
var
  i: Integer;
  subVars: TVariableSet;
begin
  for i := 1 to FileCount do
    if FileChecked[i] then
    begin
      SelectedRow := i;
      subVars := TVariableSet.Create('ImageListOpenSelected');
      subVars.Booleans['SkipSave'] := Avars.Booleans['SkipSave'];
      result := CallScriptFunction(subVars);
      AVars.Booleans['Result'] := true;
      subVars.Free;
      exit;
    end;
  AVars.Booleans['Result'] := false;
  result := srOk;
end;

function TFImageList.ScriptOpenNext(AVars: TVariableSet): TScriptResult;
var
  i:integer;
begin
  if not CheckedExist(not AVars.Booleans['Silent']) then
  begin
    AVars.Booleans['Result'] := false;
    exit(srOk);
  end;
  if not (AVars.Booleans['SkipSave'] or SaveModified) then exit(srCancelledByUser);
  if SelectedRow < FileCount then
    for i:= SelectedRow + 1 to FileCount do
      if FileChecked[i] then
        begin
          if not OpenImage(LongFileName[i]) then exit(srException);
          SelectedRow := i;
          if (tbAutoUncheck.Down and pmAutouncheckOnOpen.Checked) then
            FileChecked[SelectedRow]:= false;
          AVars.Booleans['Result'] := true;
          Exit(srOk);
        end;
  if AVars.Booleans['CanCycle'] then
    for i:= 1 to SelectedRow do
      if FileChecked[i] then
        begin
          if not OpenImage(LongFileName[i]) then exit(srException);
          SelectedRow := i;
          if (tbAutoUncheck.Down and pmAutouncheckOnOpen.Checked) then
            FileChecked[SelectedRow]:= false;
          AVars.Booleans['Result'] := true;
          Exit(srOk);
        end;
  AVars.Booleans['Result'] := false;
  exit(srOk);
end;

function TFImageList.ScriptOpenPrevious(AVars: TVariableSet): TScriptResult;
var
    i:integer;
begin
  if not CheckedExist(not AVars.Booleans['Silent']) then
  begin
    AVars.Booleans['Result'] := false;
    exit(srOk);
  end;
  if not (AVars.Booleans['SkipSave'] or SaveModified) then exit;
  if SelectedRow > 1 then
    for i:= SelectedRow -1 downto 1 do
      if FileChecked[i] then
        begin
          if not OpenImage(LongFileName[i]) then exit(srException);
          SelectedRow := i;
          if (tbAutoUncheck.Down and pmAutouncheckOnOpen.Checked) then
            FileChecked[SelectedRow]:= false;
          AVars.Booleans['Result'] := true;
          Exit(srOk);
        end; //if
  if AVars.Booleans['CanCycle'] then
    for i:= FileCount downto SelectedRow do
      if FileChecked[i] then
        begin
          if not OpenImage(LongFileName[i]) then exit(srException);
          SelectedRow := i;
          if (tbAutoUncheck.Down and pmAutouncheckOnOpen.Checked) then
            FileChecked[SelectedRow] := false;
          AVars.Booleans['Result'] := true;
          Exit(srOk);
        end; //if
  AVars.Booleans['Result'] := false;
  exit(srOk);
end;

function TFImageList.ScriptOpenSelected(AVars: TVariableSet): TScriptResult;
begin
   if AVars.Booleans['SkipSave'] or SaveModified then
   begin
     if tbAutoUncheck.Down and pmAutouncheckOnOpen.Checked then
       FileChecked[SelectedRow]:= false;
     if not OpenImage (LongFileName[SelectedRow]) then
       result := srException
     else
       result := srOk;
   end else
     result := srCancelledByUser;
end;

function TFImageList.ScriptRemoveAll(AVars: TVariableSet): TScriptResult;
begin
  StringGrid1.Clean;
  StringGrid1.RowCount:= StringGrid1.FixedRows;
  EnableButtons;
  result := srOk;
end;

function TFImageList.ScriptRemoveIndex(AVars: TVariableSet): TScriptResult;
var
  idx: Int64;
begin
  idx := AVars.Integers['Index'];
  if (idx < 1) or (idx > FileCount) then exit(srInvalidParameters);
  DeleteRow(StringGrid1, idx);
  EnableButtons;
  result := srOk;
end;

function TFImageList.ScriptRemoveNonExistent(AVars: TVariableSet): TScriptResult;
var
  i:integer;
  needRenumber: boolean;
begin
  needRenumber := false;
  for i:=FileCount downto 1 do
   if not FileManager.FileExists(LongFileName[i])
      then begin DeleteRow(StringGrid1,i); needRenumber :=true; end;
  if needRenumber then Renumber;
  EnableButtons;
  result := srOk;
end;

function TFImageList.ScriptRemoveUnchecked(AVars: TVariableSet): TScriptResult;
var
  i:integer;
  needRenumber: boolean;
begin
  needRenumber := false;
  for i:=FileCount downto 1 do
   if not FileChecked[i] then
      begin DeleteRow(StringGrid1,i); needRenumber :=true; end;
  if needRenumber then Renumber;
  EnableButtons;
  result := srOk;
end;

function TFImageList.ScriptSetAutoUncheckMode(AVars: TVariableSet): TScriptResult;
begin
  case AVars.Strings['Mode'] of
    'UncheckOnOpen': begin
      tbAutoUncheck.Down := true;
      pmAutouncheckOnSave.Checked := false;
      pmAutouncheckOnOpen.Checked := true;
      LazPaintInstance.Config.SetImageListAutoUncheckMode(1);
    end;
    'UncheckOnSave': begin
      tbAutoUncheck.Down := true;
      pmAutouncheckOnSave.Checked := true;
      pmAutouncheckOnOpen.Checked := false;
      LazPaintInstance.Config.SetImageListAutoUncheckMode(0);
    end;
    else
      tbAutoUncheck.Down := false;
  end;
  LazPaintInstance.Config.SetImageListAutoUncheck(true);
  result := srOk;
end;

function TFImageList.ScriptSetAutoZoomFit(AVars: TVariableSet): TScriptResult;
begin
  tbAutoZoomFit.Down := AVars.Booleans['Enabled'];
  LazPaintInstance.Config.SetImageListAutoZoom(AVars.Booleans['Enabled']);
  result := srOk;
end;

function TFImageList.ScriptSetFileChecked(AVars: TVariableSet): TScriptResult;
var
  idx: Int64;
begin
  if AVars.IsDefined('Index') then
  begin
    idx := AVars.Integers['Index'];
    if (idx < 1) or (idx > FileCount) then exit(srInvalidParameters);
    FileChecked[idx] := AVars.Booleans['Checked'];
    result := srOk;
  end else
  begin
    if SelectedRow >= 1 then
    begin
      FileChecked[SelectedRow] := AVars.Booleans['Checked'];
      result := srOk;
    end else
      result := srException;
  end;
end;

function TFImageList.ScriptSetSelectedIndex(AVars: TVariableSet): TScriptResult;
var
  idx: Int64;
begin
  idx := AVars.Integers['Index'];
  if (idx < 1) or (idx > FileCount) then exit(srInvalidParameters);
  SelectedRow := idx;
  result := srOk;
end;

function TFImageList.ScriptUncheckNonExistent(AVars: TVariableSet): TScriptResult;
var
  i: integer;
begin
  for i:= 1 to FileCount do
     if not FileManager.FileExists(LongFileName[i])
         then FileChecked[i] := false;
  EnableButtons;
  result := srOk;
end;

procedure TFImageList.SetLazPaintInstance(AValue: TLazPaintCustomInstance);
begin
  if FLazPaintInstance=AValue then Exit;
  RegisterScriptFunctions(False);
  FLazPaintInstance:=AValue;
  RegisterScriptFunctions(True);
end;

procedure TFImageList.SetRowChecked(AIndex: integer; AValue: boolean);
begin
  StringGrid1.Cells[ColCB,AIndex]:= BoolToStr(AValue, '1', '0');
  EnableButtons;
end;

procedure TFImageList.RegisterScriptFunctions(ARegister: boolean);
begin
  if LazPaintInstance = nil then exit;
  LazPaintInstance.ScriptContext.RegisterScriptFunction('ImageListGetFileCount', @ScriptGetFileCount, ARegister);
  LazPaintInstance.ScriptContext.RegisterScriptFunction('ImageListAddFiles', @ScriptAddFiles, ARegister);
  LazPaintInstance.ScriptContext.RegisterScriptFunction('ImageListRemoveIndex', @ScriptRemoveIndex, ARegister);
  LazPaintInstance.ScriptContext.RegisterScriptFunction('ImageListRemoveUnchecked', @ScriptRemoveUnchecked, ARegister);
  LazPaintInstance.ScriptContext.RegisterScriptFunction('ImageListRemoveNonExistent', @ScriptRemoveNonExistent, ARegister);
  LazPaintInstance.ScriptContext.RegisterScriptFunction('ImageListRemoveAll', @ScriptRemoveAll, ARegister);
  LazPaintInstance.ScriptContext.RegisterScriptFunction('ImageListUncheckNonExistent', @ScriptUncheckNonExistent, ARegister);
  LazPaintInstance.ScriptContext.RegisterScriptFunction('ImageListOpenFirst', @ScriptOpenFirst, ARegister);
  LazPaintInstance.ScriptContext.RegisterScriptFunction('ImageListOpenSelected', @ScriptOpenSelected, ARegister);
  LazPaintInstance.ScriptContext.RegisterScriptFunction('ImageListOpenNext', @ScriptOpenNext, ARegister);
  LazPaintInstance.ScriptContext.RegisterScriptFunction('ImageListOpenPrevious', @ScriptOpenPrevious, ARegister);
  LazPaintInstance.ScriptContext.RegisterScriptFunction('ImageListGetSelectedIndex', @ScriptGetSelectedIndex, ARegister);
  LazPaintInstance.ScriptContext.RegisterScriptFunction('ImageListSetSelectedIndex', @ScriptSetSelectedIndex, ARegister);
  LazPaintInstance.ScriptContext.RegisterScriptFunction('ImageListIndexOfFileName', @ScriptIndexOfFileName, ARegister);
  LazPaintInstance.ScriptContext.RegisterScriptFunction('ImageListGetFileName', @ScriptGetFileName, ARegister);
  LazPaintInstance.ScriptContext.RegisterScriptFunction('ImageListGetFileChecked', @ScriptGetFileChecked, ARegister);
  LazPaintInstance.ScriptContext.RegisterScriptFunction('ImageListSetFileChecked', @ScriptSetFileChecked, ARegister);
  LazPaintInstance.ScriptContext.RegisterScriptFunction('ImageListGetAutoUncheckMode', @ScriptGetAutoUncheckMode, ARegister);
  LazPaintInstance.ScriptContext.RegisterScriptFunction('ImageListSetAutoUncheckMode', @ScriptSetAutoUncheckMode, ARegister);
  LazPaintInstance.ScriptContext.RegisterScriptFunction('ImageListGetAutoZoomFit', @ScriptGetAutoZoomFit, ARegister);
  LazPaintInstance.ScriptContext.RegisterScriptFunction('ImageListSetAutoZoomFit', @ScriptSetAutoZoomFit, ARegister);
end;

procedure TFImageList.CallScriptFunction(AName: string);
begin
  case LazPaintInstance.ScriptContext.CallScriptFunction(AName) of
    srFunctionNotDefined: LazPaintInstance.ShowMessage(rsScript, StringReplace(rsFunctionNotDefined, '%1', AName, []));
  end;
end;

function TFImageList.CallScriptFunction(AVars: TVariableSet): TScriptResult;
begin
  result := LazPaintInstance.ScriptContext.CallScriptFunction(AVars);
  case result of
    srFunctionNotDefined: LazPaintInstance.ShowMessage(rsScript, StringReplace(rsFunctionNotDefined, '%1', AVars.FunctionName, []));
  end;
end;

procedure TFImageList.SetSelectedRow(AValue: integer);
var
  gr: TGridRect;
begin
  StringGrid1.Row := AValue;
  gr := StringGrid1.Selection;
  gr.Top := AValue;
  gr.Bottom:= AValue;
  StringGrid1.Selection := gr;
  if pnlButtonsNormalWindow.Visible and self.Visible then SafeSetFocus(StringGrid1);
end;

procedure TFImageList.pmRemoveUncheckedClick(Sender: TObject);
begin
  CallScriptFunction('ImageListRemoveUnchecked');
end;

procedure TFImageList.pmUncheckClose(Sender: TObject);
begin
  tbUncheckDropDown.Down := false;
end;

procedure TFImageList.pmUncheckNonexistentClick(Sender: TObject);
begin
  CallScriptFunction('ImageListUncheckNonexistent');
end;

procedure TFImageList.pmUncheckPopup(Sender: TObject);
begin
  tbUncheckDropDown.Down := true;
end;

procedure TFImageList.StringGrid1SelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
begin
  tbOpenImage.Enabled:= FileChecked[ARow];
end;

procedure TFImageList.StringGrid1SetCheckboxState(Sender: TObject; ACol,
  ARow: Integer; const Value: TCheckboxState);
begin
  FileChecked[ARow] := (Value=cbChecked);
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
   if (StringGrid1.RowCount>2) and (StringGrid1.Selection.Bottom< FileCount) then
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

function TFImageList.GetRowChecked(Row: integer): Boolean;
begin
   Result:= (StringGrid1.Cells[ColCB, Row] = '1')
end;

procedure TFImageList.tbOpenImageClick(Sender: TObject);
begin
  CallScriptFunction('ImageListOpenSelected');
end;

function TFImageList.GetSelectedRow: integer;
begin
  Result:=StringGrid1.Selection.Top;
end;

function TFImageList.CheckedExist (Verbose: Boolean= True) : Boolean;
var
  i: integer;
begin
  Result:=False;
  for i:=1 to StringGrid1.RowCount -1 do
    if FileChecked[i] then
      begin
        Result:=True;
        Break;
      end;
  if (Result = false) and (Verbose= True) then
    QuestionDlg (rsInformation, rsThereAreNoCheckedItems, mtInformation, [mrOk, rsOkay],'');
end;

function TFImageList.SaveModified: boolean;
begin
  Result:=True;
  if LazPaintInstance.Image.IsFileModified=True then
    case QuestionDlg (rsFileNotSaved, rsSaveChanges, mtWarning, [mrYes,rsYes,mrNo,rsNoAndProceedToNext,mrCancel,rsCancel],'') of
       mrYes :
          begin
            if Length(LazPaintInstance.Image.currentFilenameUTF8)=0 //TODO: Should I use < something. For Windows length cannot be less than 4.
            then begin QuestionDlg (rsInformation, rsThereIsNoFileNameGivenForThisFileUseSaveAs, mtInformation, [mrOk, rsOkay],''); Exit; end;
            LazPaintInstance.StartSavingImage(LazPaintInstance.Image.currentFilenameUTF8);
            try
              LazPaintInstance.Image.SaveToFileUTF8(LazPaintInstance.Image.currentFilenameUTF8);
            Except
              on ex:exception do
              begin
                LazPaintInstance.ShowError('FileSaveAs',ex.Message);
                Result:=False;
              end;
            end;
            LazPaintInstance.EndSavingImage;
          end;
        mrCancel: Result:=False;
        end;
end;

procedure TFImageList.tbOpenNextClick(Sender: TObject);
var
  vars: TVariableSet;
begin
  vars := TVariableSet.Create('ImageListOpenNext');
  vars.Booleans['CanCycle'] := true;
  CallScriptFunction(vars);
  vars.Free;
end;

procedure TFImageList.tbOpenPrevClick(Sender: TObject);
var
  vars: TVariableSet;
begin
  vars := TVariableSet.Create('ImageListOpenPrevious');
  vars.Booleans['CanCycle'] := true;
  CallScriptFunction(vars);
  vars.Free;
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
    for i:= 1 to FileCount do
      StringGrid1.Cells[ColCB,i]:= '0';
    EnableButtons;
end;

procedure TFImageList.tbCheckAllClick(Sender: TObject);
  var
  i: integer;
begin
  for i:= 1 to FileCount do
    StringGrid1.Cells[ColCB,i]:= '1';
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
  TempVar := nil;
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
      fileNames := nil;
      setlength(fileNames,FBrowseImages.SelectedFileCount);
      for i := 0 to high(fileNames) do
        fileNames[i] := FBrowseImages.SelectedFile[i];
      AddFiles(Filenames, true);
      FBrowseImages.FreeChosenImage;
    end;
  end else
  begin
    if Length(LazPaintInstance.Config.ImageListLastFolder)>0 then OpenDialog1.InitialDir:=LazPaintInstance.Config.ImageListLastFolder;
    if OpenDialog1.Execute= True then AddFiles(StringsToStringArray(OpenDialog1.Files), true);
  end;

  LazPaintInstance.ShowTopmost(topMostInfo);
end;


end.

