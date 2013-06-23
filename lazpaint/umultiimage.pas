unit umultiimage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, ugraph, BGRABitmap, LazPaintType, uscaledpi;

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
    LazPaintInstance: TLazPaintCustomInstance;
    function ShowAndChoose(images: ArrayOfBGRABitmap): TBGRABitmap;
  end; 

implementation

uses umac;

{ TFMultiImage }

procedure TFMultiImage.FormCreate(Sender: TObject);
begin
  ScaleDPI(Self,OriginalDPI);

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

function TFMultiImage.ShowAndChoose(images: ArrayOfBGRABitmap): TBGRABitmap;
var i: integer; thumb : TBGRABitmap; mr: integer;
begin
  ListView1.Clear;
  ImageList1.Clear;
  ImageList1.Masked := false;
  for i := 0 to high(images) do
  begin
    thumb := MakeThumbnail(images[i],ImageList1.Width,ImageList1.Height);
    ImageList1.Add(thumb.Bitmap,nil);
    thumb.free;
  end;

  for i := 0 to high(images) do
    with ListView1.Items.Add do
    begin
      ImageIndex := i;
      Caption := images[i].Caption;
    end;
  Button_OK.Enabled := false;
  selectedIndex := -1;
  mr := ShowModal;
  if mr = mrOK then
  begin
    if (selectedIndex <> -1) and (selectedIndex <= length(images)) then
      result := images[selectedIndex].Duplicate as TBGRABitmap else
        result := nil;
  end else
    result := nil;
end;

initialization
  {$I umultiimage.lrs}

end.

