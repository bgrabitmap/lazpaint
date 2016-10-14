unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  BGRABitmap, BGRAAnimatedGif;

type

  { TForm1 }

  TForm1 = class(TForm)
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    image: TBGRAAnimatedGif;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  image := TBGRAAnimatedGif.Create('waterdrops.gif');
  ClientWidth := image.Width;
  ClientHeight := image.Height;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  image.Free;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  image.Show(Canvas, rect(0,0,image.Width,image.Height));
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  image.Update(Canvas, rect(0,0,image.Width,image.Height));
end;

end.

