// SPDX-License-Identifier: GPL-3.0-only
unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Image1: TImage;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

uses LazpaintInstance;

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var lazpaint: TLazPaintInstance;
begin
  lazpaint := TLazPaintInstance.Create;
  lazpaint.AboutText:= 'This is the normal application';
  lazpaint.Run;
  lazpaint.Free;
end;

procedure TForm1.Button2Click(Sender: TObject);
var lazpaint: TLazPaintInstance;
    bmp: TBitmap;
begin
  lazpaint := TLazPaintInstance.Create(True);
  lazpaint.AboutText:= 'This is an embedded application';
  lazpaint.Run;
  if lazpaint.EmbeddedResult = mrOK then
  begin
    bmp := lazpaint.Image.MakeBitmapCopy(ColorToRGB(clBtnFace));
    image1.Picture.assign(bmp);
    bmp.Free;
  end;
  lazpaint.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

end.

