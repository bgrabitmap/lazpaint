unit Unit1;

{
-So lets put things together and play with masks
-Just read the code, it should be clear
-As you can see this demo will uses more cpu because it create mask every time it paints but still it is very fast
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls, OpenGLContext, BGRABitmap, BGRABitmapTypes, BGRAOpenGL;

type

  { TForm1 }

  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    RadioGroup1: TRadioGroup;
    Timer1: TTimer;
    TrackBar1: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    OpenGLControl: TOpenGLControl;
  public
    Tex, Mask: IBGLTexture;
    rectMask: TRect;
    MainFont: IBGLFont;
    r: single;
    GoBack: boolean;
    procedure OpenGLControlPaint(Sender: TObject);
    procedure UpdateMask;
  end;

var
  Form1: TForm1;

implementation

uses Types,Math;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  OpenGLControl := TOpenGLControl.Create(Self);
  with OpenGLControl do
  begin
    Align := alClient;
    Parent := Panel2;
    OnPaint := @OpenGLControlPaint;
    AutoResizeViewport := True;
  end;
  RadioGroup1.ItemIndex := 0;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  //You can not make textures before form show
  Tex := BGLTexture('Background.jpg');
  MainFont := BGLFont('Arial',20);
  Timer1.Enabled := True;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if not GoBack then
  begin
    if r = 200 then
      GoBack := True;
    r += 1;
  end
  else
  begin
    if r = 50 then
      GoBack := False;
    r -= 1;
  end;
  OpenGLControl.Invalidate;
end;

procedure TForm1.OpenGLControlPaint(Sender: TObject);
begin
  //Dont forget this
  BGLViewPort(OpenGLControl.Width, OpenGLControl.Height, BGRABlack);

  case RadioGroup1.ItemIndex of
    0:
    begin
      //Draw just texture
      //StretchPutImage will resample image to prefered size
      BGLCanvas.StretchPutImage(0, 0, OpenGLControl.Width, OpenGLControl.Height, Tex);
    end;
    1:
    begin
      UpdateMask;
      //See how mask looks
      if Assigned(Mask) then
      begin
        Mask.BlendMode := obmNormal;
        BGLCanvas.PutImage(rectMask.Left, rectMask.Top, Mask);
      end;
    end;
    2:
    begin
      UpdateMask;
      //Now see them together
      if Assigned(Mask) then
      begin
        //draw only the part of the image that overlaps with the mask
        BGLCanvas.ClipRect := rectMask;
        BGLCanvas.StretchPutImage(0, 0, OpenGLControl.Width, OpenGLControl.Height, Tex);
        BGLCanvas.NoClip;

        //apply the mask
        Mask.BlendMode := obmMultiply;
        BGLCanvas.PutImage(rectMask.Left, rectMask.Top, Mask);
      end;

      //draw the whole picture without mask
      BGLCanvas.StretchPutImage(0, 0, OpenGLControl.Width, OpenGLControl.Height, Tex, 255-TrackBar1.Position);

      if Assigned(Mask) then
      begin
        //draw the mask
        Mask.BlendMode := obmAdd;
        BGLCanvas.PutImage(rectMask.Left, rectMask.Top, Mask, (255-TrackBar1.Position) div 4);
      end;
    end;
  end;

  MainFont.TextOut(0,0, inttostr(OpenGLControl.FrameDiffTimeInMSecs) + ' ms');

  //And dont forget this
  OpenGLControl.SwapBuffers;
end;

procedure TForm1.UpdateMask;
var rectEllipse: TRect;
    mousePos: TPoint;
    bmp: TBGLBitmap;
begin
  mousePos := Panel2.ScreenToControl(Mouse.CursorPos);

  //determine area of the ellipse
  rectEllipse := Rect(mousePos.x - ceil(r), mousePos.y - ceil(r),
                 mousePos.x + ceil(r) + 1, mousePos.y + ceil(r) + 1);
  rectMask := EmptyRect;
  if IntersectRect(rectMask, rect(0,0, BGLCanvas.Width, BGLCanvas.Height), rectEllipse) then
  begin
    //render the ellipse
    bmp := TBGLBitmap.Create(rectMask.Right-rectMask.Left, rectMask.Bottom-rectMask.Top, BGRABlack);
    bmp.FillEllipseAntialias(mousePos.x-rectMask.Left, mousePos.y-rectMask.Top, r, r, BGRAWhite);
    Mask := bmp.MakeTextureAndFree;
  end else
    Mask := nil;
end;

end.
