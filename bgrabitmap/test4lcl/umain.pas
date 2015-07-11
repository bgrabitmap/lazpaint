unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs;

type

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormPaint(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

uses BGRABitmap, BGRABitmapTypes;

{$R *.lfm}

procedure DrawEllipseHello(bmp: TBGRABitmap);
var br: TBGRACustomBitmap;
begin
  bmp.Fill(BGRABlack);
  bmp.CustomPenStyle := BGRAPenStyle(2,1);
  bmp.FillEllipseLinearColorAntialias(bmp.Width/2,bmp.Height/2,bmp.Width/2-5,bmp.Height/2-5, BGRAPixelTransparent, BGRAWhite);
  bmp.EllipseAntialias(bmp.Width/2,bmp.Height/2,bmp.Width/2-5,bmp.Height/2-5,CSSRed,5);
  if bmp.Height div 10 < 10 then
    bmp.FontHeight := 10
  else
    bmp.FontHeight := bmp.Height div 10;
  with bmp.FontPixelMetric do
    bmp.TextOut(bmp.Width/2,bmp.Height/2 - (CapLine+Baseline)/2,'Hello world', BGRABlack, taCenter);
  bmp.Canvas.Pen.Color := clBlue;
  bmp.Canvas.MoveTo(0,0);
  bmp.Canvas.LineTo(bmp.Width,bmp.Height);
  br := bmp.CreateBrushTexture(bsDiagCross, CSSYellow,CSSRed);
  bmp.FillPieInRect(rect(10,10,100,100),0,3*Pi/2,br);
  bmp.TextOutAngle(50,50, -300, 'Test angle', CSSGreen, taLeftJustify);
  br.Free;
end;

{ TForm1 }

procedure TForm1.FormPaint(Sender: TObject);
var
  bmp: TBGRABitmap;
begin
  bmp := TBGRABitmap.Create(ClientWidth,ClientHeight);
  DrawEllipseHello(bmp);
  bmp.Draw(Canvas,0,0);
  bmp.Free;
end;


end.

