// SPDX-License-Identifier: GPL-3.0-only
unit UMultiImage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, BGRABitmap, LazPaintType, LCScaleDPI, BGRABitmapTypes;

type

  { TFMultiImage }

  TFMultiImage = class(TForm)
    Button_Cancel: TButton;
    Button_OK: TButton;
    ImageList1: TImageList;
    ListView1: TListView;
    procedure Button_OKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListView1DblClick(Sender: TObject);
    procedure ListView1SelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    { private declarations }
    selectedIndex : integer;
  public
    { public declarations }
    function ShowAndChoose(images: ArrayOfImageEntry; AStretch: boolean; AFormat: TBGRAImageFormat): TImageEntry;
  end; 

implementation

uses umac, BGRAThumbnail;

{ TFMultiImage }

procedure TFMultiImage.FormCreate(Sender: TObject);
begin
  ScaleControl(Self,OriginalDPI);

  CheckOKCancelBtns(Button_OK,Button_Cancel);
end;

procedure TFMultiImage.ListView1DblClick(Sender: TObject);
begin
  if selectedIndex <> -1 then ModalResult := mrOK;
end;

procedure TFMultiImage.Button_OKClick(Sender: TObject);
begin
  ModalResult:= mrOK;
end;

procedure TFMultiImage.ListView1SelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if Selected then
  begin
    selectedIndex := Item.Index;
    Button_OK.Enabled := true;
  end else
  begin
    selectedIndex := -1;
    Button_OK.Enabled := false;
  end;
end;

function TFMultiImage.ShowAndChoose(images: ArrayOfImageEntry; AStretch: boolean; AFormat: TBGRAImageFormat): TImageEntry;
var i: integer; thumb : TBGRABitmap; mr: integer;
  x,y: integer; temp: TBGRABitmap;
begin
  result := TImageEntry.Empty;

  ListView1.Clear;
  ImageList1.Clear;
  ImageList1.Masked := false;
  thumb := TBGRABitmap.Create(ImageList1.Width,ImageList1.Height);
  for i := 0 to high(images) do
  begin
    if AStretch or (images[i].bmp.Width > thumb.width) or (images[i].bmp.Height > thumb.Height) then
    begin
      GetBitmapThumbnail(images[i].bmp,AFormat,thumb.Width,thumb.Height,BGRAPixelTransparent,True,thumb);
    end else
    begin
      thumb.FillTransparent;
      temp := GetBitmapThumbnail(images[i].bmp,AFormat,images[i].bmp.Width,images[i].bmp.Height,BGRAPixelTransparent,true);
      x := (thumb.width-images[i].bmp.Width) div 2;
      y := (thumb.Height-images[i].bmp.Height) div 2;
      thumb.PutImage(x,y, temp, dmSet);
      temp.Free;
    end;
    ImageList1.Add(thumb.Bitmap,nil);
  end;
  thumb.free;

  for i := 0 to high(images) do
    with ListView1.Items.Add do
    begin
      ImageIndex := i;
      Caption := images[i].bmp.Caption;
    end;
  Button_OK.Enabled := false;
  selectedIndex := -1;
  mr := ShowModal;
  if mr = mrOK then
  begin
    if (selectedIndex <> -1) and (selectedIndex <= length(images)) then
    begin
      result.bmp := images[selectedIndex].bmp.Duplicate(AFormat = ifCur,True) as TBGRABitmap;
      result.bpp := images[selectedIndex].bpp;
      result.frameIndex := images[selectedIndex].frameIndex;
      result.frameCount := images[selectedIndex].frameCount;
      result.isDuplicate:= images[selectedIndex].isDuplicate;
    end;
  end;
end;

{$R *.lfm}

end.

