// SPDX-License-Identifier: GPL-3.0-only
unit UPreviewDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, BGRAVirtualScreen, UImagePreview, LazPaintType;

type

  { TFPreviewDialog }

  TFPreviewDialog = class(TForm)
    LStatus: TLabel;
    Panel1: TPanel;
    vsPreview: TBGRAVirtualScreen;
    procedure FormCloseQuery(Sender: TObject; var {%H-}CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FPreview: TImagePreview;
    function GetDuplicateSourceIndex: integer;
    function GetEntryCount: integer;
    function GetFilename: string;
    function GetLazPaintInstance: TLazPaintCustomInstance;
    procedure SetDuplicateSourceIndex(AValue: integer);
    procedure SetFilename(AValue: string);
    procedure PreviewValidate(Sender: TObject);
    procedure PreviewEscape(Sender: TObject);
    procedure SetLazPaintInstance(AValue: TLazPaintCustomInstance);
  public
    function GetPreviewBitmap: TImageEntry;
    property Filename: string read GetFilename write SetFilename;
    property LazPaintInstance: TLazPaintCustomInstance read GetLazPaintInstance write SetLazPaintInstance;
    property EntryCount: integer read GetEntryCount;
    property DuplicateSourceIndex: integer read GetDuplicateSourceIndex write SetDuplicateSourceIndex;
  end;

var
  FPreviewDialog: TFPreviewDialog;

function ShowPreviewDialog(AInstance: TLazPaintCustomInstance; AFilename: string; ATitle: string = '';
  ASkipIfSingleImage: boolean = false; ADuplicateSourceIndex: integer = -1): TImageEntry;

implementation

function ShowPreviewDialog(AInstance: TLazPaintCustomInstance; AFilename: string; ATitle: string;
  ASkipIfSingleImage: boolean; ADuplicateSourceIndex: integer): TImageEntry;
var f: TFPreviewDialog;
begin
  f := TFPreviewDialog.Create(nil);
  f.DuplicateSourceIndex := ADuplicateSourceIndex;
  f.LazPaintInstance := AInstance;
  if ATitle <> '' then f.Caption := ATitle;
  f.Filename:= AFilename;
  if ASkipIfSingleImage and (f.EntryCount = 1) then
  begin
    result := f.GetPreviewBitmap;
  end else
  begin
    if f.ShowModal = mrOk then
      result := f.GetPreviewBitmap
    else
      result := TImageEntry.Empty;
  end;
  f.Free;
end;

{ TFPreviewDialog }

procedure TFPreviewDialog.FormCreate(Sender: TObject);
begin
  FPreview := TImagePreview.Create(vsPreview, LStatus, false);
  FPreview.OnValidate:= @PreviewValidate;
  FPreview.OnEscape:= @PreviewEscape;
end;

procedure TFPreviewDialog.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var r:TRect;
begin
  LazPaintInstance.Config.SetDefaultPreviewDialogMaximized(self.WindowState = wsMaximized);
  if self.WindowState = wsNormal then
  begin
    r.left := Left;
    r.top := Top;
    r.right := r.left+ClientWidth;
    r.Bottom := r.top+ClientHeight;
    LazPaintInstance.Config.SetDefaultPreviewDialogPosition(r);
  end
  else
    LazPaintInstance.Config.SetDefaultPreviewDialogPosition(TRect.Empty);
end;

procedure TFPreviewDialog.FormDestroy(Sender: TObject);
begin
  FPreview.Free;
end;

procedure TFPreviewDialog.FormShow(Sender: TObject);
var
  r: TRect;
begin
  if Assigned(LazPaintInstance) then
  begin
    if LazPaintInstance.Config.DefaultPreviewDialogMaximized then self.WindowState := wsMaximized
      else
    begin
      self.WindowState := wsNormal;
      r := LazPaintInstance.Config.DefaultPreviewDialogPosition;
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
end;

function TFPreviewDialog.GetFilename: string;
begin
  result := FPreview.Filename;
end;

function TFPreviewDialog.GetEntryCount: integer;
begin
  if Assigned(FPreview) then
    result := FPreview.EntryCount
  else
    result := 0;
end;

function TFPreviewDialog.GetDuplicateSourceIndex: integer;
begin
  result := FPreview.DuplicateEntrySourceIndex;
end;

function TFPreviewDialog.GetLazPaintInstance: TLazPaintCustomInstance;
begin
  result := FPreview.LazPaintInstance;
end;

procedure TFPreviewDialog.SetDuplicateSourceIndex(AValue: integer);
begin
  FPreview.DuplicateEntrySourceIndex:= AValue;
end;

procedure TFPreviewDialog.SetFilename(AValue: string);
begin
  FPreview.Filename := AValue;
end;

procedure TFPreviewDialog.PreviewValidate(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TFPreviewDialog.PreviewEscape(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TFPreviewDialog.SetLazPaintInstance(AValue: TLazPaintCustomInstance);
begin
  FPreview.LazPaintInstance := AValue;
end;

function TFPreviewDialog.GetPreviewBitmap: TImageEntry;
begin
  result := FPreview.GetPreviewBitmap;
end;

{$R *.lfm}

end.

