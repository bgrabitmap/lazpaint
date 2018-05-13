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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FPreview: TImagePreview;
    function GetEntryCount: integer;
    function GetFilename: string;
    function GetLazPaintInstance: TLazPaintCustomInstance;
    procedure SetFilename(AValue: string);
    procedure PreviewValidate(Sender: TObject);
    procedure PreviewEscape(Sender: TObject);
    procedure SetLazPaintInstance(AValue: TLazPaintCustomInstance);
  public
    function GetPreviewBitmap: TImageEntry;
    property Filename: string read GetFilename write SetFilename;
    property LazPaintInstance: TLazPaintCustomInstance read GetLazPaintInstance write SetLazPaintInstance;
    property EntryCount: integer read GetEntryCount;
  end;

var
  FPreviewDialog: TFPreviewDialog;

function ShowPreviewDialog(AInstance: TLazPaintCustomInstance; AFilename: string; ATitle: string = '';
  ASkipIfSingleImage: boolean = false): TImageEntry;

implementation

function ShowPreviewDialog(AInstance: TLazPaintCustomInstance; AFilename: string; ATitle: string;
  ASkipIfSingleImage: boolean): TImageEntry;
var f: TFPreviewDialog;
begin
  f := TFPreviewDialog.Create(nil);
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

procedure TFPreviewDialog.FormDestroy(Sender: TObject);
begin
  FPreview.Free;
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

function TFPreviewDialog.GetLazPaintInstance: TLazPaintCustomInstance;
begin
  result := FPreview.LazPaintInstance;
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

initialization
  {$I upreviewdialog.lrs}

end.

