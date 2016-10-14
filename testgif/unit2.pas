unit unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  BGRABitmap, BGRABitmapTypes, BGRAAnimatedGif, BGRAVirtualScreen, BCTypes;

type

  { TForm2 }

  TForm2 = class(TForm)
    BGRAVirtualScreen1: TBGRAVirtualScreen;
    Timer1: TTimer;
    procedure BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    image: TBGRAAnimatedGif;
  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}

{ TForm2 }

procedure TForm2.FormCreate(Sender: TObject);
begin
  image := TBGRAAnimatedGif.Create('waterdrops.gif');
  BGRAVirtualScreen1.Width := image.Width;
  BGRAVirtualScreen1.Height := image.Height;
  ClientWidth := image.Width;
  ClientHeight := image.Height;
end;

procedure TForm2.BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  Bitmap.PutImage(0,0, image.MemBitmap, dmDrawWithTransparency);
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  image.Free;
end;

procedure TForm2.Timer1Timer(Sender: TObject);
begin
  if image.TimeUntilNextImageMs <= 0 then
    BGRAVirtualScreen1.DiscardBitmap;
end;

end.

