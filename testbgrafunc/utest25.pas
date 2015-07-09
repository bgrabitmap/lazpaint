unit utest25;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, BGRABitmap, BGRABitmapTypes, BGRACanvas, utest;

type
  { TTest25 }

  TTest25 = class(TTest)
  private
    pacman,virtualScreen: TBGRABitmap;
    alphaFactor: byte;
    time: single;
    pts: array of TPoint;
  public
    constructor Create;
    destructor Destroy; override;
    procedure OnPaint(Canvas: TCanvas; Left,Top,Width,Height: Integer); override;
    procedure OnTimer(Width,Height: Integer; ElapsedSec: Double); override;
    procedure DrawOnCanvas(Canvas: TBGRACanvas);
  end;

implementation

uses BGRAGradientScanner;

{ TTest25 }

constructor TTest25.Create;
begin
  inherited Create;
  Name := 'BGRACanvas test (with aliasing on the left, with anti-aliasing on the right)';
  virtualScreen := nil;
  pacman := TBGRABitmap.Create('..'+pathdelim+'img'+pathdelim+'pac_d1.bmp');
  pacman.ReplaceColor(pacman.GetPixel(0,0),BGRAPixelTransparent);
  time := 0;
end;

destructor TTest25.Destroy;
begin
  virtualScreen.Free;
  pacman.free;
  inherited Destroy;
end;

procedure TTest25.OnPaint(Canvas: TCanvas; Left,Top,Width, Height: Integer);
var
  i: Integer;
  tx: integer;
begin
  if time=0 then exit;

  tx := (Width+1) div 2;
  if (virtualscreen <> nil) and ((virtualscreen.width <> tx) or (virtualscreen.Height <> height)) then
    FreeAndNil(virtualScreen);

  if virtualscreen = nil then
  begin
    virtualscreen := TBGRABitmap.Create(tx,Height);
    //randseed := 1;
    setlength(pts, 20);
    for i := 0 to high(pts) do
      pts[i] := Point(Random(virtualScreen.Width),random(virtualScreen.Height));
  end;

  virtualScreen.CanvasBGRA.AntialiasingMode := amOff;
  virtualScreen.CanvasBGRA.Font.Quality := fqSystem;
  DrawOnCanvas(virtualScreen.CanvasBGRA);
  virtualscreen.Draw(Canvas,Left,Top,True);

  virtualScreen.CanvasBGRA.AntialiasingMode := amOn;
  virtualScreen.CanvasBGRA.Font.Quality := fqFineClearTypeRGB;
  DrawOnCanvas(virtualScreen.CanvasBGRA);
  virtualscreen.Draw(Canvas,Left+Width div 2,Top,True);
end;

procedure TTest25.OnTimer(Width, Height: Integer; ElapsedSec: Double);
begin
  time := time+ElapsedSec;
  alphaFactor := round((sin(time)+1)/2*255);
end;

procedure TTest25.DrawOnCanvas(Canvas: TBGRACanvas);
var i,k: integer;
    grad: TBGRAGradientScanner;
begin
  for k := 1 to 1 do
  begin
    grad := TBGRAGradientScanner.Create(BGRA(0,150,255),BGRA(0,0,0),gtLinear,PointF(0,0),PointF(0,Canvas.Height/40),True,True);
    With Canvas do
    begin
      Brush.style := bsSolid;
      Brush.Color := clWhite;
      Brush.Opacity := 255;
      FillRect(0,0,Width,Height);

      Font.Orientation := 0;
      Font.Color := clBlack;
      Font.Height := Height div 20;
      Font.Texture := grad;
      TextOut(Font.Height div 2,0,'Font rendering');

      Font.Orientation := -450;
      TextOut(Font.Height,Font.Height,'Text angle');

      pen.width := 2;
      pen.opacity := 255;
      Brush.Color := clGreen;
      brush.Style := bsSolid;
      pen.Style := psDash;
      Ellipse(pts[6].x,pts[6].y,pts[7].x,pts[7].y);

      pen.Style := psSolid;
      pen.width := 1;
      pen.opacity := 255;
      Brush.BGRAColor := BGRA(255,128,0);
      brush.style := bsFDiagonal;
      Ellipse(pts[14].x,pts[14].y,pts[15].x,pts[15].y);

      pen.style := psClear;
      pen.opacity := 64;
      Brush.Color := clBlue;
      Brush.Opacity := 128;
      brush.style := bsCross;
      Rectangle(pts[8].x,pts[8].y,pts[9].x,pts[9].y);

      pen.Style := psSolid;
      pen.width := 1;
      pen.opacity := 128;
      Brush.Color := clRed;
      Brush.Opacity := 64;
      brush.Style := bsSolid;
      RoundRect(pts[10].x,pts[10].y,pts[11].x,pts[11].y,abs(pts[11].x-pts[10].x) div 2,abs(pts[11].y-pts[10].y) div 2);

      brush.style := bsClear;
      pen.width := 2;
      RoundRect(pts[16].x,pts[16].y,pts[17].x,pts[17].y,abs(pts[17].x-pts[16].x) div 2,abs(pts[17].y-pts[16].y) div 2);

      pen.style := psClear;
      Brush.Color := clBlue;
      Brush.Opacity := 192;
      brush.style := bsSolid;
      //Polygon(pts);
      Pie(pts[12].x,pts[12].y,pts[13].x,pts[13].y,pts[6].x,pts[6].y,pts[7].x,pts[7].y);

      Pen.Color := clBlack;
      pen.style := psSolid;
      pen.opacity := 128;
      MoveTo(pts[0]);
      for i := 0 to 5 do
      begin
        Pen.Width := i+1;
        LineTo(pts[i+1]);
      end;

      Font.Texture := nil;
    end;
    Grad.Free;
  end;
end;

end.

