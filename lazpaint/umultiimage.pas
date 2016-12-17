unit UMultiImage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, BGRABitmap, LazPaintType, uscaledpi;

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
    function ShowAndChoose(images: ArrayOfBGRABitmap; AStretch: boolean): TBGRABitmap;
  end; 

implementation

uses umac, BGRAThumbnail, BGRABitmapTypes;

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

function TFMultiImage.ShowAndChoose(images: ArrayOfBGRABitmap; AStretch: boolean): TBGRABitmap;
var i: integer; thumb : TBGRABitmap; mr: integer;
  x,y: integer;
begin
  ListView1.Clear;
  ImageList1.Clear;
  ImageList1.Masked := false;
  thumb := TBGRABitmap.Create(ImageList1.Width,ImageList1.Height);
  for i := 0 to high(images) do
  begin
    if AStretch or (images[i].Width > thumb.width) or (images[i].Height > thumb.Height) then
    begin
      GetBitmapThumbnail(images[i],thumb.Width,thumb.Height,BGRAPixelTransparent,True,thumb);
    end else
    begin
      thumb.FillTransparent;
      x := (thumb.width-images[i].Width) div 2;
      y := (thumb.Height-images[i].Height) div 2;
      DrawThumbnailCheckers(thumb, rect(x,y,x+images[i].Width,y+images[i].Height));
      thumb.PutImage(x,y,images[i],dmDrawWithTransparency);
    end;
    ImageList1.Add(thumb.Bitmap,nil);
  end;
  thumb.free;

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
      result := images[selectedIndex].Duplicate(False,True) as TBGRABitmap else
        result := nil;
  end else
    result := nil;
end;

{$R *.lfm}

end.

