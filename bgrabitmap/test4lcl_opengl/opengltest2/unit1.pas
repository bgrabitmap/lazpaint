unit Unit1;

{
-If you didnt saw opengltest1, go see it first then come back

-OpenGL is fast, so we want to test it a bit
-Here you will see that you can update your painting much fast without any particular cpu using or whatever
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  OpenGLContext, BGRABitmap, BGRABitmapTypes, BGRAOpenGL;

type

  { TForm1 }

  TForm1 = class(TForm)
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    TexBig,TexSmall: IBGLTexture;
    OpenGLControl: TOpenGLControl;
    DataLoaded: boolean;
    angle: single;
    GoBack: boolean;
    procedure Load;
    procedure OpenGLControlPaint(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  OpenGLControl := TOpenGLControl.Create(Self);
  with OpenGLControl do
  begin
    Align := alClient;
    Parent := Self;
    OnPaint := @OpenGLControlPaint;
    AutoResizeViewport := True;
  end;
  DataLoaded := False;
  GoBack := False;
  angle := 0;
  WindowState := wsMaximized;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if not GoBack then
  begin
    if angle = 360 then
      GoBack := True;
    angle += 1;
  end
  else
  begin
    if angle = 0 then
      GoBack := False;
    angle -= 1;
  end;
  OpenGLControl.DoOnPaint;
end;

procedure TForm1.Load;
var bmp, bmpSmall : TBGLBitmap;
begin
  if not DataLoaded then
  begin
    bmp := TBGLBitmap.Create(ExtractFilePath(Application.ExeName) + 'earth.png');
    bmp.ResampleFilter := rfBestQuality;
    bmpSmall := bmp.Resample(bmp.Width div 2,bmp.Height div 2) as TBGLBitmap;
    TexBig := bmp.MakeTextureAndFree;
    TexSmall := bmpSmall.MakeTextureAndFree;

    DataLoaded := True;
  end;
end;

procedure TForm1.OpenGLControlPaint(Sender: TObject);
var
  x, y, w: single;
  tex: IBGLTexture;
begin
  Load;
  BGLViewPort(OpenGLControl.Width, OpenGLControl.Height, BGRABlack);

  x := OpenGLControl.Width / 2;
  y := OpenGLControl.Height / 2;
  w := (angle / 360) * OpenGLControl.Width;
  if w < TexSmall.Width then
    tex := TexSmall
  else
    tex := TexBig;
  tex.StretchDrawAngle(x, y, w, w/tex.Width*tex.Height, angle, PointF(tex.Width/2,tex.Height/2), False);

  OpenGLControl.SwapBuffers;
end;

end.

