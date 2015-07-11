program test4nogui;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  BGRAGraphics,
  BGRABitmap,
  BGRABitmapTypes;

type

  { TTestBGRANoGui }

  TTestBGRANoGui = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

procedure DrawEllipseHello(bmp: TBGRABitmap);
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
end;

{ TTestBGRANoGui }

procedure TTestBGRANoGui.DoRun;
var
  bmp: TBGRABitmap;
begin
  TBGRABitmap.AddFreeTypeFontFolder(GetCurrentDir);

  bmp := TBGRABitmap.Create(800,600,BGRABlack);
  DrawEllipseHello(bmp);
  bmp.SaveToFile('test.png');
  bmp.free;

  Terminate;
end;

constructor TTestBGRANoGui.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TTestBGRANoGui.Destroy;
begin
  inherited Destroy;
end;

var
  Application: TTestBGRANoGui;

{$R *.res}

begin
  Application:=TTestBGRANoGui.Create(nil);
  Application.Title:='TestBGRANoGui';
  Application.Run;
  Application.Free;
end.

