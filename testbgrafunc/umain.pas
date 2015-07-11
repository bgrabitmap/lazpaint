unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, utest, EpikTimer;

type

  { TFMain }

  TFMain = class(TForm)
    Button_First: TButton;
    Button_Last: TButton;
    Button_Next: TButton;
    Button_Prev: TButton;
    Label_TestName: TLabel;
    Panel1: TPanel;
    Timer1: TTimer;
    procedure Button_LastClick(Sender: TObject);
    procedure Button_NextClick(Sender: TObject);
    procedure Button_FirstClick(Sender: TObject);
    procedure Button_PrevClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  public
    NumTest: integer;
    CurrentTest: TTest;
    stopwatch, timeMeasure: TEpikTimer;
    procedure SetNumTest(value: integer);
  end; 

var
  FMain: TFMain;

implementation

uses utest1, utest2, utest3, utest4, utest5, utest6, utest7,
     utest8, utest9, utest10, utest11, utest14, utest15, utest16, utest17,
     utest18, utest19, utest22, utest23, utest24, utest25, utest26,
     utest27, utest31, utest32, utest33, BGRAScene3D,
     BGRABitmapTypes, LCLIntf;

const
    TestCount = 33;

{ TFMain }

procedure TFMain.SetNumTest(value: integer);
begin
  FreeAndNil(CurrentTest);
  numTest := value;
  Button_Prev.Enabled := TestCount >=2;
  Button_Next.Enabled := TestCount >=2;
  case numTest of
  1: currentTest := TTest1.Create;
  2: currentTest := TTest2.Create;
  3: currentTest := TTest3.Create;
  4: currentTest := TTest4.Create;
  5: currentTest := TTest5.Create;
  6: currentTest := TTest6.Create;
  7: currentTest := TTest7.create;
  8: currentTest := TTest8.create;
  9: currentTest := TTest9.create;
  10: currentTest := TTest10.create;
  11: currentTest := TTest11.create('');
  12: currentTest := TTest11.create('Emboss');
  13: currentTest := TTest11.create('Contour');
  14: currentTest := TTest14.create;
  15: currentTest := TTest15.create;
  16: currentTest := TTest16.create;
  17: currentTest := TTest17.create;
  18: currentTest := TTest18.create;
  19: currentTest := TTest19.create(False,False);
  20: currentTest := TTest19.create(True,False);
  21: currentTest := TTest19.create(True,True);
  22: currentTest := TTest22.create;
  23: currentTest := TTest23.create;
  24: currentTest := TTest24.create;
  25: currentTest := TTest25.create;
  26: currentTest := TTest26.create;
  27: currentTest := TTest27.create(lnFace, liLowQuality);
  28: currentTest := TTest27.create(lnFaceVertexMix, liLowQuality);
  29: currentTest := TTest27.create(lnVertex, liLowQuality);
  30: currentTest := TTest27.create(lnVertex, liAlwaysHighQuality);
  31: currentTest := TTest31.create;
  32: currentTest := TTest32.create;
  33: currentTest := TTest33.create;
  else
    raise exception.Create('Test number unknown ('+IntToStr(numTest)+')');
  end;
  Label_TestName.Caption := 'Test n°'+inttostr(Value)+': '+currentTest.Name;
end;

procedure TFMain.FormCreate(Sender: TObject);
begin
  CurrentTest := nil;
  SetNumTest(1);
  stopwatch := TEpikTimer.Create(Application);
  timeMeasure := TEpikTimer.Create(Application);
  timeMeasure.Start;

  if TBGRAPixel_RGBAOrder then
    Caption := Caption + ' (RGBA 32-bit)'
  else
    Caption := Caption + ' (BGRA 32-bit)';
end;

procedure TFMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(CurrentTest);
end;

procedure TFMain.FormPaint(Sender: TObject);
var
  strTime: string;
  ptText: TPoint;
  drawElapsed: extended;
begin
  if CurrentTest <> nil then
  begin
    stopwatch.clear;
    stopwatch.Start;

    CurrentTest.OnPaint(self.Canvas,0,Panel1.Height,ClientWidth,ClientHeight-Panel1.Height);

    drawElapsed := stopwatch.Elapsed;

    if drawElapsed > 0.0001 then
    begin
      strTime := IntToStr(round(drawElapsed*1000))+ ' ms';
      strTime += ', ' + IntToStr(round(1/drawElapsed)) + ' FPS';
      ptText := Point(3,ClientHeight-25);
      with self.Canvas do
      begin
        Font.Height := 25;
        Font.Style := [fsBold];
        Brush.Style := bsClear;
        Font.Color := clBlack;
        TextOut(ptText.X-1,ptText.Y-1,strTime);
        TextOut(ptText.X+1,ptText.Y-1,strTime);
        TextOut(ptText.X+1,ptText.Y+1,strTime);
        TextOut(ptText.X-1,ptText.Y+1,strTime);
        Font.Color := clWhite;
        TextOut(ptText.X,ptText.Y,strTime);
      end;
    end;

  end;
end;

procedure TFMain.Timer1Timer(Sender: TObject);
var
  elapsed: Extended;
  r: TRect;
begin
  if CurrentTest <> nil then
  begin
    Timer1.Enabled := false;
    elapsed := timeMeasure.Elapsed;
    timeMeasure.clear;
    timeMeasure.Start;

    CurrentTest.OnTimer(ClientWidth,ClientHeight-Panel1.Height,elapsed);
    r := rect(0,Panel1.Height,ClientWidth,ClientHeight);
    InvalidateRect(self.Handle,@r,False);
    self.Update;
    Timer1.Enabled := true;
  end;
end;

procedure TFMain.Button_NextClick(Sender: TObject);
begin
  if NumTest < TestCount then
    SetNumTest(NumTest+1) else
      SetNumTest(1);
end;

procedure TFMain.Button_LastClick(Sender: TObject);
begin
  SetNumTest(TestCount);
end;

procedure TFMain.Button_FirstClick(Sender: TObject);
begin
  SetNumTest(1);
end;

procedure TFMain.Button_PrevClick(Sender: TObject);
begin
  if NumTest > 1 then
    SetNumTest(NumTest-1) else
      SetNumTest(TestCount);
end;

initialization
  {$I umain.lrs}

end.

