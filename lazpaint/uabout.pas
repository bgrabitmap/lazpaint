unit uabout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, uscaledpi, BGRATextFX, BGRAGradients;

type

  { TFAbout }

  TFAbout = class(TForm)
    Image_Title: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LabelUrl: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Memo1: TMemo;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LabelUrlClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    titleStartTime: TDateTime;
    frameNumber: integer;
    fx: TBGRATextEffect;
    shader: TPhongShading;
    angle: single;
    procedure RenderTitle;
  public
    { public declarations }
  end;

procedure ShowAboutDlg(AText: string);

implementation

uses LCLIntf, BGRABitmap, BGRABitmapTypes, LazPaintType;

procedure ShowAboutDlg(AText: string);
var
  About: TFAbout;
begin
  About := nil;
  try
    About:= TFAbout.create(nil);
    About.Memo1.Text := AText;
    About.ShowModal;
  except
    on ex:Exception do
      ShowMessage('ShowAboutDlg: '+ex.Message);
  end;
  About.Free;
end;

{ TFAbout }

procedure TFAbout.LabelUrlClick(Sender: TObject);
begin
  OpenURL(LabelUrl.Caption);
end;

procedure TFAbout.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := false;
  angle += 2*Pi/180;
  shader.LightPosition := Point(round(cos(angle)*Image_Title.Width),round(sin(angle)*Image_Title.Width));
  RenderTitle;
  Timer1.Enabled := true;
end;

procedure TFAbout.RenderTitle;
var bmp: TBGRABitmap;
begin
  inc(frameNumber);
  bmp := TBGRABitmap.Create(Image_Title.Width,Image_Title.Height);

  bmp.GradientFill(0,0,bmp.Width,bmp.Height,BGRA(0,128,255,255),BGRA(55,70,176,255),gtLinear,
    PointF(bmp.Width div 4,-bmp.Width div 4),PointF(bmp.Width*3 div 4,bmp.Width div 4),dmSet,False);
  fx.DrawShaded(bmp,bmp.Width div 2,0, shader, 3, BGRA(255,160,0), taCenter, false);

  bmp.Draw(Image_Title.Picture.Bitmap.Canvas,0,0,true);
  bmp.Free;
  Image_Title.Refresh;
end;

procedure TFAbout.FormShow(Sender: TObject);
begin
  titleStartTime := Now;
  RenderTitle;
  Timer1.Enabled := true;
end;

procedure TFAbout.FormHide(Sender: TObject);
begin
  Timer1.Enabled := false;
end;

procedure TFAbout.FormCreate(Sender: TObject);
var bmp: TBitmap;
    titlefont: TFont;
begin
  ScaleDPI(Self,OriginalDPI);

  Self.DoubleBuffered:=True;

  bmp := TBitmap.Create;
  bmp.Width := Image_Title.Width;
  bmp.Height := Image_Title.Height;
  bmp.PixelFormat := pf24bit;
  Image_Title.Picture.Bitmap := Bmp;
  bmp.Free;
  FrameNumber := 0;

  titlefont := TFont.Create;
  titlefont.Height := Image_Title.Height;
  titlefont.Name := 'Arial';
  titlefont.Style := [fsBold];
  fx := TBGRATextEffect.Create('LazPaint ' + LazPaintCurrentVersion,titlefont,True);
  titlefont.Free;
  shader := TPhongShading.Create;
end;

procedure TFAbout.FormDestroy(Sender: TObject);
begin
  fx.Free;
  shader.Free;
end;

initialization
  {$I uabout.lrs}

end.

