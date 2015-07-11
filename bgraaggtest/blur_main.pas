unit blur_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, BGRABitmap, BGRABitmapTypes, EpikTimer, LMessages;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label_RadiusValue: TLabel;
    Panel1: TPanel;
    Radio_Box: TRadioButton;
    Radio_Motion: TRadioButton;
    Radio_Fast: TRadioButton;
    Radio_Corona: TRadioButton;
    Radio_Disk: TRadioButton;
    Radio_Radial: TRadioButton;
    TrackBar_BlurRadius: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure Radio_Change(Sender: TObject);
    procedure TrackBar_BlurRadiusChange(Sender: TObject);
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
  private
    { private declarations }
  public
    { public declarations }
    image,shadowBase: TBGRABitmap;
    timer : TEpikTimer;
    movingShadow: boolean;
    movingOrigin,shadowOfs: TPoint;
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormPaint(Sender: TObject);
var bmp,ombre: TBGRABitmap;
    x,y,tx,ty: integer;
    blurType: TRadialBlurType;
    radius: single;
begin
  tx := clientWidth;
  ty := Panel1.Top;
  bmp := TBGRABitmap.Create(tx,ty,BGRAWhite);
  x := (tx-image.Width) div 2;
  y := (ty-image.Height) div 2;

  timer.Clear;
  timer.Start;
  if Radio_Motion.Checked then
  begin
    ombre := shadowBase.FilterBlurMotion(TrackBar_BlurRadius.Position,0,False) as TBGRABitmap;
  end else
  begin
    radius := TrackBar_BlurRadius.Position;
    if Radio_Box.Checked then
    begin
      blurType := rbBox;
      radius := radius/sqrt(2);
    end else
    if Radio_Fast.Checked then blurType := rbFast else
    if Radio_Corona.Checked then blurType := rbCorona else
    if Radio_Disk.Checked then blurType := rbDisk else
    if Radio_Radial.Checked then blurType := rbNormal;
    ombre := shadowBase.FilterBlurRadial(round(radius),blurType) as TBGRABitmap;
  end;
  timer.Stop;
  ombre.Rectangle(0,0,ombre.width,ombre.height,BGRA(0,0,0,128),dmDrawWithTransparency);
  bmp.PutImage(x-shadowOfs.x,y-shadowOfs.y,ombre,dmDrawWithTransparency);
  ombre.free;

  bmp.PutImage(x,y,image,dmDrawWithTransparency);
  bmp.TextOut(0,0,inttostr(round(timer.Elapsed*1000))+' ms',BGRABlack);
  bmp.Draw(Canvas,0,0);
  bmp.Free;
end;

procedure TForm1.Radio_Change(Sender: TObject);
begin
  Invalidate;
end;

procedure TForm1.TrackBar_BlurRadiusChange(Sender: TObject);
begin
  Label_RadiusValue.Caption := '= '+IntToStr(TrackBar_BlurRadius.Position);
  Invalidate;
end;

procedure TForm1.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  //
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  image := TBGRABitmap.Create(200,250);
  image.FontName := 'Times New Roman';
  image.FontHeight := 300;
  image.FontAntialias:= true;
  image.TextOut(100,-100,'a',BGRA(128,192,128,240),taCenter);
  shadowBase := TBGRABitmap.Create(200,200,BGRAWhite);
  image.CopyPropertiesTo(shadowBase);
  shadowBase.TextOut(100,-100,'a',BGRA(64,128,64),taCenter);
  Label_RadiusValue.Caption := '= '+IntToStr(TrackBar_BlurRadius.Position);
  timer := TEpikTimer.Create(Self);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  image.free;
  shadowBase.free;
end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    movingOrigin := Point(X,Y);
    movingShadow := true;
  end;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if movingShadow then
  begin
    shadowOfs.x += movingOrigin.X-X;
    shadowOfs.y += movingOrigin.Y-Y;
    movingOrigin := Point(X,Y);
    Invalidate;
  end;
end;

procedure TForm1.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    movingShadow:= false;
end;

end.

