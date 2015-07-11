unit distortions_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls, BGRABitmap, BGRABitmapTypes, BGRAGradientScanner,
  LMessages, EpikTimer;

type

  { TWaveDistortion }

  TWaveDistortion = class(TBGRACustomScanner)
  private
    FSource : IBGRAScanner;
    FCenter : TPointF;
    FRadius, FAmplitude: Single;
  public
    Delta: single;
    constructor Create(source : IBGRAScanner; center : TPointF; radius, amplitude: single);
    function ScanAt(X, Y: Single): TBGRAPixel; override;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    Timer1: TTimer;
    TrackBar_Angle: TTrackBar;
    TrackBar_Scale: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
  private
    { private declarations }
  public
    { public declarations }
    image: TBGRABitmap;
    grad: TBGRAGradientScanner;
    multigrad: TBGRAMultiGradient;
    delta: single;
    timer: TEpikTimer;
  end;

var
  Form1: TForm1; 

implementation

uses BGRATransform;

{ TWaveDistortion }

constructor TWaveDistortion.Create(source: IBGRAScanner; center : TPointF; radius, amplitude: single);
begin
  FSource := Source;
  FCenter := center;
  FRadius := radius;
  FAmplitude := amplitude;
  Delta := 0;
end;

function TWaveDistortion.ScanAt(X, Y: Single): TBGRAPixel;
var d: single;
    p,v: TPointF;
begin
  p := PointF(X,Y);
  v := p-FCenter;
  d := sqrt(v*v);
  if d <> 0 then v *= 1/d;
  p += v*sin(d*2*Pi/FRadius+Delta)*FAmplitude;
  result := FSource.ScanAt(p.X,p.Y);
end;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormPaint(Sender: TObject);
const ampl = 10;
var bmp: TBGRABitmap;
    tx,ty: integer;
    x,y,rx,ry,scale :single;

  procedure DrawEllipse(source: IBGRAScanner);
  var
    disto: TWaveDistortion;
    affine: TBGRAAffineScannerTransform;
  begin
    affine := TBGRAAffineScannerTransform.Create(source);
    affine.RotateDeg(TrackBar_Angle.Position);
    affine.Scale(scale,scale);
    affine.Translate(x,y);
    disto := TWaveDistortion.Create(affine,PointF(x,y),(rx+ry)/2*0.6,ampl);
    disto.Delta := Delta;
    bmp.FillEllipseAntialias(x,y,rx,ry,disto);
    disto.Free;
    affine.free;
  end;

begin
  timer.Clear;
  timer.start;
  tx := ClientWidth;
  ty := Panel1.Top;
  scale := TrackBar_Scale.Position/10;
  bmp := TBGRABitmap.Create(tx,ty, BGRAWhite);
  x := tx/4;
  y := ty/2;
  rx := tx/4*0.8;
  ry := ty/2*0.8;
  image.ScanOffset := Point(round(image.width/2),round(image.Height/2));
  DrawEllipse(image);

  x := 3*tx/4;
  y := ty/2;
  bmp.FillEllipseAntialias(x,y,rx,ry,BGRABlack);
  grad := TBGRAGradientScanner.Create(multigrad,gtRadial,PointF(0.4*rx/scale,-0.4*ry/scale),PointF(0.4*rx/scale+rx,-0.4*ry/scale),False);
  DrawEllipse(grad);
  grad.free;

  timer.Stop;
  bmp.TextOut(0,0,inttostr(round(timer.Elapsed*1000))+ ' ms',BGRABlack);
  bmp.Draw(Canvas,0,0);
  bmp.Free;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := false;
  Delta -= 10*Pi/180;
  Repaint;
  Timer1.Enabled := true;
end;

procedure TForm1.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  //
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  image := TBGRABitmap.Create('spheres.png');
  timer := TEpikTimer.Create(self);
  multigrad := TBGRAMultiGradient.Create([BGRAWhite,BGRA(255,235,96),BGRA(255,160,0),BGRA(140,0,0),BGRA(64,0,0),BGRA(160,64,0)],[0,0.2,0.4,0.8,0.9,1],True);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  image.free;
  multigrad.Free;
end;

end.

