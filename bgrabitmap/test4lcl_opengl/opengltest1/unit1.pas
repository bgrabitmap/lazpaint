unit Unit1;
{
-For using OpenGl with BGRABitmap and LCl:
-Add two packages (take a look at project inspector!)
-Then you need a TOpenGLControl and do create stuff and setting events
-If you want OpenGl speed you should use a texture again and again, dont make it each time unless it is necceray so here we will load them once
-For making texture you can use TBGLBitmap or IBGLTexture
-TBGLBitmap is like a TBGRABitmap with OpenGl stuff and a texture property, you can draw, load and whatever here and then use its texture and if you dont need it later free it.
-IBGLTexture is an interface so you dont need to free it,just use it and note that it will use less memory
-After loading you need to draw background with BGLViewPort
-After that draw your textures with BGLCanvas. for more info just take a look at it's class
-In the end do SwapBuffers so all you paint goes to buffer and will draw

-For more info you should check other demos, classes, Wiki or ask on the forum
http://forum.lazarus.freepascal.org/index.php?board=46.0
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  OpenGLContext, BGRABitmap, BGRABitmapTypes, BGRAOpenGL;

type

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
  public
    bmp: TBGLBitmap;
    Tex: IBGLTexture;
    OpenGLControl: TOpenGLControl;
    DataLoaded: boolean;
    procedure Load;
    procedure OpenGLControlPaint(Sender: TObject);
    procedure OpenGLControlMouseMove(Sender: TObject; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: integer);
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
    OnMouseMove := @OpenGLControlMouseMove;
    //If you dont do it,you will have some problems
    AutoResizeViewport := True;
  end;
  DataLoaded := False;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  OpenGLControl.Free;
end;

procedure TForm1.Load;
var path: string;
begin
  if not DataLoaded then
  begin
    path := ExtractFilePath(Application.ExeName) + '..' + PathDelim + 'tux_game' + PathDelim + 'ground.png';
    //Do this :
    //Way1
    //Tex := BGLTexture(path);

    //Or this for more editing options
    //Way2
    bmp := TBGLBitmap.Create(path);
    bmp.Rectangle(0,0,bmp.Width,bmp.Height,BGRABlack,dmSet);
    Tex:=bmp.MakeTextureAndFree;

    //Or whatever you want!

    DataLoaded := True;
  end;
end;

procedure TForm1.OpenGLControlPaint(Sender: TObject);
var
  mousePos: TPoint;
begin
  { Load data }
  Load;
  { Draw Background }
  BGLViewPort(OpenGLControl.Width, OpenGLControl.Height, BGRAWhite);
  { Draw Texture }
  mousePos := ScreenToClient(Mouse.CursorPos);
  BGLCanvas.PutImage(mousePos.x - Tex.Width div 2, mousePos.y - Tex.Height div 2, Tex);
  { Update }
  OpenGLControl.SwapBuffers;
end;

procedure TForm1.OpenGLControlMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  OpenGLControl.DoOnPaint;
end;

end.


