program test4fpgui;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  fpg_base, fpg_main, fpg_form,
  BGRABitmapTypes, BGRABitmap;

type

  { TMainForm }

  TMainForm = class(TfpgForm)
    procedure   FormPaint(Sender: TObject);
  protected
    FBmp: TBGRABitmap;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   AfterCreate; override;
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

{ TMainForm }

procedure TMainForm.FormPaint(Sender: TObject);
begin
  FBmp.SetSize(Width,Height);
  DrawEllipseHello(FBmp);
  FBmp.Draw(Canvas,0,0);
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBmp := TBGRABitmap.Create;
end;

destructor TMainForm.Destroy;
begin
  FBmp.Free;
  inherited Destroy;
end;

procedure TMainForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(50, 50, 500, 400);
  WindowTitle := 'BGRABitmap and fpGUI';
  Hint := '';
  WindowPosition := wpOneThirdDown;
  OnPaint := @FormPaint;
  {@VFD_BODY_END: MainForm}
  {%endregion}
end;

procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;
  TBGRABitmap.AddFreeTypeFontFolder(GetCurrentDir);
  frm := TMainForm.Create(nil);
  frm.Show;
  fpgApplication.Run;
  frm.Free;
end;

{$R *.res}

begin
  MainProc;
end.


