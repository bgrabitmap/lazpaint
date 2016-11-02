unit blur_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, BGRABitmap, BGRABitmapTypes, EpikTimer, LMessages;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label_RadiusValueY: TLabel;
    Label_RadiusX: TLabel;
    Label2: TLabel;
    Label_RadiusY: TLabel;
    Label_RadiusValueX: TLabel;
    Panel1: TPanel;
    Radio_Box: TRadioButton;
    Radio_Motion: TRadioButton;
    Radio_Fast: TRadioButton;
    Radio_Corona: TRadioButton;
    Radio_Disk: TRadioButton;
    Radio_OrientedMotion: TRadioButton;
    Radio_Radial: TRadioButton;
    TrackBar_BlurRadiusX: TTrackBar;
    TrackBar_BlurRadiusY: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure Radio_Change(Sender: TObject);
    procedure TrackBar_BlurRadiusChange(Sender: TObject);
    procedure WMEraseBkgnd(var {%H-}Message: TLMEraseBkgnd); message LM_ERASEBKGND;
  private
    { private declarations }
    procedure UpdateLabelRadius;
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

function ComputeAngle(dx, dy: single): single;
begin
     if dy = 0 then
     begin
       if dx < 0 then result := 180 else result := 0;
     end else
     if dx = 0 then
     begin
       if dy < 0 then result := -90 else result := 90;
     end else
     begin
       result := ArcTan(dy/dx)*180/Pi;
       if dx < 0 then result += 180;
     end;
end;

{ TForm1 }

procedure TForm1.FormPaint(Sender: TObject);
var bmp,ombre: TBGRABitmap;
    x,y,tx,ty: integer;
    blurType: TRadialBlurType;
    radiusX,radiusY,len: single;
begin
  tx := clientWidth;
  ty := Panel1.Top;
  bmp := TBGRABitmap.Create(tx,ty,BGRAWhite);
  x := (tx-image.Width) div 2;
  y := (ty-image.Height) div 2;

  radiusX := TrackBar_BlurRadiusX.Position/10;
  radiusY := TrackBar_BlurRadiusY.Position/10;

  timer.Clear;
  timer.Start;
  if Radio_Motion.Checked or Radio_OrientedMotion.Checked then
  begin
    len := sqrt(sqr(radiusX)+sqr(radiusY));
    ombre := shadowBase.FilterBlurMotion(len*2,ComputeAngle(radiusX,radiusY),Radio_OrientedMotion.Checked) as TBGRABitmap;
  end else
  begin
    if Radio_Box.Checked then
    begin
      blurType := rbBox;
      ombre := shadowBase.FilterBlurRadial(radiusX,radiusY,blurType) as TBGRABitmap;
    end else
    begin
      if Radio_Fast.Checked then blurType := rbFast else
      if Radio_Corona.Checked then blurType := rbCorona else
      if Radio_Disk.Checked then blurType := rbDisk else
      if Radio_Radial.Checked then blurType := rbNormal;
      ombre := shadowBase.FilterBlurRadial(radiusX,radiusY,blurType) as TBGRABitmap;
    end;
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
  UpdateLabelRadius;
  Repaint;
end;

procedure TForm1.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  //
end;

procedure TForm1.UpdateLabelRadius;
begin
  Label_RadiusValueX.Caption := '= '+FloatToStrF(TrackBar_BlurRadiusX.Position/10,ffFixed,7,1);
  Label_RadiusValueY.Caption := '= '+FloatToStrF(TrackBar_BlurRadiusY.Position/10,ffFixed,7,1);
  Label_RadiusValueX.Update;
  Label_RadiusValueY.Update;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  image := TBGRABitmap.Create(160,200);
  image.FontName := 'Times New Roman';
  image.FontHeight := 300;
  image.FontAntialias:= true;
  image.TextOut(image.Width div 2,-100,'a',BGRA(128,192,128,240),taCenter);
  shadowBase := TBGRABitmap.Create(image.Width,image.Height,BGRAWhite);
  image.CopyPropertiesTo(shadowBase);
  shadowBase.TextOut(shadowBase.Width div 2,-100,'a',BGRA(64,128,64),taCenter);
  UpdateLabelRadius;
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

