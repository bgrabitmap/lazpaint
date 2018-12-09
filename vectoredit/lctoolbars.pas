unit LCToolbars;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ComCtrls, Types, LResources;

function CreateToolBar(AImages: TImageList; AOwner: TComponent = nil): TToolbar;
function GetToolbarSize(AToolbar: TToolbar; APadding: integer = 1): TSize;
procedure SetToolbarImages(AToolbar: TToolbar; AImages: TImageList);
procedure EnableDisableToolButtons(AButtons: array of TToolButton; AEnabled: boolean);
procedure ShowAppendToolButtons(AButtons: array of TControl);
function AddToolbarCheckButton(AToolbar: TToolbar; ACaption: string; AImageIndex: integer;
          AOnClick: TNotifyEvent; ADown: boolean; AGrouped: boolean = true; ATag: PtrInt = 0): TToolButton;
function AddToolbarButton(AToolbar: TToolbar; ACaption: string; AImageIndex: integer;
          AOnClick: TNotifyEvent; ATag: PtrInt = 0): TToolButton;
procedure AddToolbarControl(AToolbar: TToolbar; AControl: TControl);
function GetResourceStream(AFilename: string): TLazarusResourceStream;
function GetResourceString(AFilename: string): string;
procedure LoadToolbarImage(AImages: TImageList; AIndex: integer; AFilename: string);

implementation

uses BGRALazPaint, BGRABitmap, BGRABitmapTypes, math;

function CreateToolBar(AImages: TImageList; AOwner: TComponent): TToolbar;
begin
  result := TToolBar.Create(AOwner);
  result.Align := alNone;
  result.Height := AImages.Height + 9;
  result.ShowHint:= true;
  result.ShowCaptions:= false;
  result.Images := AImages;
  result.ParentColor := false;
end;

function GetToolbarSize(AToolbar: TToolbar; APadding: integer = 1): TSize;
var
  i: Integer;
  r: TRect;
begin
  result := Size(APadding,APadding);
  for i := 0 to AToolbar.ControlCount-1 do
  if AToolbar.Controls[i].Visible then
  begin
    r := AToolbar.Controls[i].BoundsRect;
    if r.Right > result.cx then result.cx := r.Right;
    if r.Bottom > result.cy then result.cy := r.Bottom;
  end;
  result.cx += APadding;
  result.cy += APadding;
end;

procedure SetToolbarImages(AToolbar: TToolbar; AImages: TImageList);
begin
  AToolbar.Images := AImages;
  AToolbar.ButtonWidth:= AImages.Width+5;
  AToolbar.ButtonHeight:= AImages.Height+4;
end;

function GetResourceStream(AFilename: string): TLazarusResourceStream;
var
  ext: RawByteString;
begin
  ext := UpperCase(ExtractFileExt(AFilename));
  if (ext<>'') and (ext[1]='.') then Delete(ext,1,1);
  result := TLazarusResourceStream.Create(ChangeFileExt(AFilename,''), pchar(ext));
end;

function GetResourceString(AFilename: string): string;
var
  res: TLResource;
  ext: RawByteString;
begin
  ext := UpperCase(ExtractFileExt(AFilename));
  if (ext<>'') and (ext[1]='.') then Delete(ext,1,1);
  res := LazarusResources.Find(ChangeFileExt(AFilename,''), ext);
  if assigned(res) then result:= res.Value else result:= '';
end;

procedure LoadToolbarImage(AImages: TImageList; AIndex: integer; AFilename: string);
var
  iconImg: TBGRALazPaintImage;
  iconFlat: TBGRABitmap;
  res: TLazarusResourceStream;
  mem: TMemoryStream;
begin
  iconImg := TBGRALazPaintImage.Create;
  res := GetResourceStream(AFilename);
  mem:= TMemoryStream.Create;
  res.SaveToStream(mem);
  res.Free;
  mem.Position:= 0;
  iconImg.LoadFromStream(mem);
  iconImg.Resample(AImages.Width,AImages.Height,rmFineResample,rfBestQuality);
  iconFlat := TBGRABitmap.Create(iconImg.Width,iconImg.Height);
  iconImg.Draw(iconFlat,0,0);
  if AImages.Count < AIndex then
    AImages.Replace(AIndex, iconFlat.Bitmap,nil)
  else
    AImages.Add(iconFlat.Bitmap,nil);
  iconFlat.Free;
  iconImg.Free;
end;

function AddToolbarCheckButton(AToolbar: TToolbar; ACaption: string; AImageIndex: integer;
          AOnClick: TNotifyEvent; ADown: boolean; AGrouped: boolean = true; ATag: PtrInt = 0): TToolButton;
var
  btn: TToolButton;
begin
  btn := TToolButton.Create(AToolbar);
  btn.Style := tbsCheck;
  btn.Caption := ACaption;
  btn.Hint := ACaption;
  btn.ImageIndex := AImageIndex;
  btn.Down:= ADown;
  btn.Grouped := AGrouped;
  btn.OnClick:= AOnClick;
  btn.Left:= AToolbar.ButtonCount*AToolbar.ButtonWidth;
  btn.Parent := AToolbar;
  btn.Tag:= ATag;
  result := btn;
end;

function AddToolbarButton(AToolbar: TToolbar; ACaption: string;
  AImageIndex: integer; AOnClick: TNotifyEvent; ATag: PtrInt): TToolButton;
var
  btn: TToolButton;
begin
  btn := TToolButton.Create(AToolbar);
  btn.Style := tbsButton;
  btn.Caption := ACaption;
  btn.Hint := ACaption;
  btn.ImageIndex := AImageIndex;
  btn.OnClick:= AOnClick;
  btn.Left:= AToolbar.ControlCount*AToolbar.ButtonWidth;
  btn.Parent := AToolbar;
  btn.Tag:= ATag;
  result := btn;
end;

procedure AddToolbarControl(AToolbar: TToolbar; AControl: TControl);
begin
  AControl.Left := AToolbar.ControlCount*AToolbar.ButtonWidth;
  AToolbar.InsertControl(AControl);
end;

procedure EnableDisableToolButtons(AButtons: array of TToolButton; AEnabled: boolean);
var
  i: Integer;
begin
  for i := 0 to high(AButtons) do
    AButtons[i].Enabled:= AEnabled;
end;

procedure ShowAppendToolButtons(AButtons: array of TControl);
var btnCount,x,y, i: integer;
  toolbar: TToolBar;
begin
  if length(AButtons) = 0 then exit;
  toolbar := AButtons[0].Parent as TToolBar;
  x := 0;
  y := 0;
  btnCount := 0;
  for i := 0 to toolbar.ControlCount-1 do
    if toolbar.Controls[i].Visible then
    begin
      x := max(toolbar.Controls[i].Left+toolbar.Controls[i].Width,x);
      y := max(toolbar.Controls[i].Top+toolbar.Controls[i].Height,y);
      inc(btnCount);
    end;

  toolbar.BeginUpdate;
  x:= max(btnCount * toolbar.ButtonWidth,x);
  for i := 0 to high(AButtons) do
  begin
    AButtons[i].Left := x;
    AButtons[i].Visible:= true;
    x += toolbar.ButtonWidth;
  end;
  toolbar.EndUpdate;
end;

end.

