// SPDX-License-Identifier: GPL-3.0-only
unit LCToolbars;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ComCtrls, Types, LResources, StdCtrls, BCTrackbarUpdown;

function CreateToolBar(AImages: TImageList; AOwner: TComponent = nil): TToolbar;
procedure ReorderToolbarContent(AToolbar: TToolbar);
function GetToolbarSize(AToolbar: TToolbar; APadding: integer = 1): TSize;
procedure SetToolbarImages(AToolbar: TToolbar; AImages: TImageList; HorizPadding: integer = 5; VertPadding: integer = 4);
procedure EnableDisableToolButtons(AButtons: array of TToolButton; AEnabled: boolean);
procedure ShowAppendToolButtons(AButtons: array of TControl);
function AddToolbarLabel(AToolbar: TToolbar; ACaption: string; AExistingContainer: TCustomControl): TLabel;
function AddToolbarCheckButton(AToolbar: TToolbar; ACaption: string; AImageIndex: integer;
          AOnClick: TNotifyEvent; ADown: boolean; AGrouped: boolean = true; ATag: PtrInt = 0): TToolButton;
function AddToolbarButton(AToolbar: TToolbar; ACaption: string; AImageIndex: integer;
          AOnClick: TNotifyEvent; ATag: PtrInt = 0): TToolButton;
function AddToolbarUpDown(AToolbar: TToolbar; ACaption: string; AMin,AMax,AValue: Integer; AOnChange: TTrackBarUpDownChangeEvent): TBCTrackbarUpdown;
function AddToolbarTextBox(AToolbar: TToolbar; ACaption: string; AText: string; AOnChange: TNotifyEvent): TEdit;
procedure AddToolbarControl(AToolbar: TToolbar; AControl: TControl);
function GetResourceString(AFilename: string): string;
procedure LoadToolbarImage(AImages: TImageList; AIndex: integer; AFilename: string);

implementation

uses BGRALazPaint, Graphics, BGRABitmap, BGRABitmapTypes, math, Toolwin;

function CreateToolBar(AImages: TImageList; AOwner: TComponent): TToolbar;
begin
  result := TToolBar.Create(AOwner);
  result.Align := alNone;
  result.Height := AImages.Height+4;
  result.ShowHint:= true;
  result.ShowCaptions:= false;
  result.Images := AImages;
  result.ButtonWidth := AImages.Width+5;
  result.ButtonHeight := AImages.Height+4;
  result.ParentColor := false;
  result.EdgeBorders:= [];
  result.EdgeInner:= esNone;
  result.EdgeOuter:= esNone;
end;

procedure ReorderToolbarContent(AToolbar: TToolbar);
var
  i,x,y: Integer;
begin
  AToolbar.BeginUpdate;
  x := AToolbar.Indent;
  y := 0;
  for i := 0 to AToolbar.ControlCount-1 do
  begin
    with AToolbar.Controls[i] do
    begin
      if (x+Width > AToolbar.Width) and AToolbar.Wrapable then
      begin
        x := AToolbar.Indent;
        y += AToolbar.ButtonHeight;
      end;
      Left := x;
      Top := y;
      x += Width;
    end;
    if (AToolbar.Controls[i] is TToolButton) and
      TToolButton(AToolbar.Controls[i]).Wrap then
    begin
      x := AToolbar.Indent;
      y += AToolbar.ButtonHeight;
    end;
  end;
  AToolbar.EndUpdate;
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

procedure SetToolbarImages(AToolbar: TToolbar; AImages: TImageList; HorizPadding: integer; VertPadding: integer);
begin
  AToolbar.Images := AImages;
  AToolbar.ButtonWidth:= AImages.Width+HorizPadding;
  AToolbar.ButtonHeight:= AImages.Height+VertPadding;
end;

function GetResourceString(AFilename: string): string;
var
  strStream: TStringStream;
  resStream: TStream;
begin
  resStream := BGRAResource.GetResourceStream(AFilename);
  strStream := TStringStream.Create('');
  strStream.CopyFrom(resStream, resStream.Size);
  resStream.Free;
  result:= strStream.DataString;
  strStream.Free;
end;

procedure LoadToolbarImage(AImages: TImageList; AIndex: integer; AFilename: string);
var
  iconImg: TBGRALazPaintImage;
  iconFlat: array of TBGRABitmap;
  bmpArray: array of TCustomBitmap;
  i: Integer;
begin
  iconImg := TBGRALazPaintImage.Create;
  iconImg.LoadFromResource(AFilename);
  if AImages.ResolutionCount = 0 then
    AImages.RegisterResolutions([AImages.Width]);
  setlength(iconFlat, AImages.ResolutionCount);
  setlength(bmpArray, length(iconFlat));
  for i := 0 to high(iconFlat) do
  begin
    iconImg.Resample(AImages.ResolutionByIndex[i].Width,
                      AImages.ResolutionByIndex[i].Height,
                      rmFineResample,rfBestQuality);
    iconFlat[i] := TBGRABitmap.Create(iconImg.Width, iconImg.Height);
    iconImg.Draw(iconFlat[i],0,0);
    bmpArray[i] := iconFlat[i].Bitmap;
  end;
  iconImg.Free;
  if AImages.Count < AIndex then
  begin
    for i := 0 to high(iconFlat) do
      AImages.Replace(AIndex, bmpArray[i],nil, false);
  end
  else
    AImages.AddMultipleResolutions(bmpArray);
  for i := 0 to high(iconFlat) do
    iconFlat[i].Free;
end;

function AddToolbarLabel(AToolbar: TToolbar; ACaption: string;
  AExistingContainer: TCustomControl): TLabel;
var
  lbl: TLabel;
begin
  lbl := TLabel.Create(AToolbar);
  lbl.AutoSize:= false;
  lbl.Alignment:= taCenter;
  lbl.Layout := tlCenter;
  lbl.Caption := ACaption;
  lbl.Width := AExistingContainer.Canvas.TextWidth(lbl.Caption)+(AToolbar.ButtonHeight div 4);
  lbl.Height := AToolbar.ButtonHeight;
  AddToolbarControl(AToolbar, lbl);
  result := lbl;
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
  btn.Tag:= ATag;
  AddToolbarControl(AToolbar, btn);
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
  btn.Tag:= ATag;
  AddToolbarControl(AToolbar, btn);
  result := btn;
end;

function AddToolbarUpDown(AToolbar: TToolbar; ACaption: string; AMin,
  AMax, AValue: Integer; AOnChange: TTrackBarUpDownChangeEvent): TBCTrackbarUpdown;
begin
  result := TBCTrackbarUpdown.Create(AToolbar);
  result.Width := AToolbar.ButtonWidth*2;
  result.Height:= AToolbar.ButtonHeight;
  result.MinValue := AMin;
  result.MaxValue := AMax;
  result.Value := AValue;
  result.Hint := ACaption;
  result.ShowHint:= true;
  result.OnChange:= AOnChange;
  AddToolbarControl(AToolbar, result);
end;

function AddToolbarTextBox(AToolbar: TToolbar; ACaption: string; AText: string;
  AOnChange: TNotifyEvent): TEdit;
begin
  result := TEdit.Create(AToolbar);
  result.Width := AToolbar.ButtonWidth*5;
  result.Font.Height := round(AToolbar.Height*0.5);
  result.Hint := ACaption;
  result.ShowHint:= true;
  result.Text := AText;
  result.OnChange:= AOnChange;
  AddToolbarControl(AToolbar, result);
end;

procedure AddToolbarControl(AToolbar: TToolbar; AControl: TControl);
var
  x,y, i: Integer;
begin
  x := AToolbar.Indent;
  y := 0;
  for i := 0 to AToolbar.ControlCount-1 do
  begin
    if AToolbar.Controls[i] is TToolButton then
    begin
      inc(x, AToolbar.ButtonWidth);
      if TToolButton(AToolbar.Controls[i]).Wrap then
      begin
        x := 0;
        inc(y, AToolbar.ButtonHeight);
      end;
    end
    else inc(x, AToolbar.Controls[i].Width);
  end;
  AControl.Left := x;
  AControl.Top := y;
  AControl.Parent := AToolbar;
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

