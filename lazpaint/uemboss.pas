unit uemboss;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, BGRABitmap, LazPaintType, uscaledpi;

type

  { TFEmboss }

  TFEmboss = class(TForm)
    Button_OK: TButton;
    Button_Cancel: TButton;
    Label3: TLabel;
    PaintBox1: TPaintBox;
    procedure Button_OKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1Paint(Sender: TObject);
  private
    { private declarations }
    InPaintBoxMouseMove: boolean;
    PaintBoxMouseMovePos: TPoint;
    angle: single;
    selectingAngle: boolean;
    backupSource: TBGRABitmap;
    procedure ComputeAngle(X,Y: integer);
    function ComputeFilteredLayer: TBGRABitmap;
    procedure PreviewNeeded;
  public
    { public declarations }
    LazPaintInstance: TLazPaintCustomInstance;
    sourceLayer, filteredLayer: TBGRABitmap;
  end;

function ShowEmbossDlg(Instance: TLazPaintCustomInstance; layer:TBGRABitmap; out filteredLayer: TBGRABitmap):boolean;

implementation

uses BGRABitmapTypes, math, ugraph, umac;

{ TFEmboss }

function ShowEmbossDlg(Instance: TLazPaintCustomInstance; layer:TBGRABitmap; out filteredLayer: TBGRABitmap):boolean;
var
  FEmboss: TFEmboss;
begin
  filteredLayer := nil;
  result := false;
  FEmboss:= TFEmboss.create(nil);
  FEmboss.LazPaintInstance := Instance;
  try
    FEmboss.sourceLayer := layer;
    result:= (FEmboss.showModal = mrOk);
    filteredLayer := FEmboss.filteredLayer;
  finally
    FEmboss.free;
  end;
end;

procedure TFEmboss.FormCreate(Sender: TObject);
begin
  ScaleDPI(Self,OriginalDPI);

  InPaintBoxMouseMove := false;
  CheckOKCancelBtns(Button_OK,Button_Cancel);
  filteredLayer := nil;
  backupSource := nil;
end;

procedure TFEmboss.FormDestroy(Sender: TObject);
begin
  if backupSource <> nil then
  begin
    sourceLayer.PutImage(0,0,backupSource,dmSet);
    FreeAndNil(backupSource);
    LazPaintInstance.NotifyImageChangeCompletely(False);
  end;
end;

procedure TFEmboss.FormShow(Sender: TObject);
begin
  angle := LazPaintInstance.Config.DefaultEmbossAngle;
  PreviewNeeded;
end;

procedure TFEmboss.Button_OKClick(Sender: TObject);
begin
  if sourceLayer <> nil then
  begin
    filteredLayer := ComputeFilteredLayer;
    LazPaintInstance.Config.SetDefaultEmbossAngle(angle);
    ModalResult := mrOK;
  end else
    ModalResult := mrCancel;
end;

procedure TFEmboss.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if button = mbLeft then selectingAngle := true;
  ComputeAngle(X,Y);
end;

procedure TFEmboss.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  PaintBoxMouseMovePos := Point(X,Y);
  if InPaintBoxMouseMove then Exit;
  InPaintBoxMouseMove := True;
  Application.ProcessMessages; //empty message stack
  if selectingAngle then ComputeAngle(PaintBoxMouseMovePos.X,PaintBoxMouseMovePos.Y);
  InPaintBoxMouseMove := False;
end;

procedure TFEmboss.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if button = mbLeft then selectingAngle := false;
end;

procedure TFEmboss.PaintBox1Paint(Sender: TObject);
var bmp: TBGRABitmap;
    x1,y1,x2,y2: single;
    dx,dy,t: single;
    c: TBGRAPixel;
begin
    bmp := TBGRABitmap.Create(PaintBox1.Width,PaintBox1.Height);
    bmp.Fill(ColorToRGB(clBtnFace));
    c := ColorToBGRA(ColorToRGB(clWindowText));
    t := min(PaintBox1.Width/2,PaintBox1.Height/2);
    dx := cos(angle*Pi/180);
    dy := sin(angle*Pi/180);
    x1 := PaintBox1.Width/2;
    y1 := PaintBox1.Height/2;
    x2 := x1+dx*(t-2);
    y2 := y1+dy*(t-2);
    bmp.FillEllipseAntialias(x1,y1,t-1,t-1,BGRA(c.red,c.green,c.blue,48));
    bmp.DrawLineAntialias(x1,y1,x2,y2,c,2,true);
    bmp.DrawLineAntialias(x2+dy*5-dx*5,y2-dx*5-dy*5,x2,y2,c,2,false);
    bmp.DrawLineAntialias(x2-dy*5-dx*5,y2+dx*5-dy*5,x2,y2,c,2,false);
    bmp.Draw(PaintBox1.Canvas,0,0,true);
    bmp.Free;
end;

procedure TFEmboss.ComputeAngle(X, Y: integer);
begin
  angle := ugraph.ComputeAngle(X-PaintBox1.Width/2,Y-PaintBox1.Height/2);
  PreviewNeeded;
  PaintBox1.Repaint;
end;

function TFEmboss.ComputeFilteredLayer: TBGRABitmap;
var usedSource: TBGRABitmap;
begin
  if backupSource <> nil then
    usedSource := backupSource
  else
    usedSource := sourceLayer;
  result := usedSource.FilterEmboss(angle) as TBGRABitmap;
end;

procedure TFEmboss.PreviewNeeded;
var temp: TBGRABitmap;
begin
  if sourceLayer = nil then exit;
  temp := ComputeFilteredLayer;
  if backupSource = nil then
    backupSource := sourceLayer.Duplicate as TBGRABitmap;
  sourceLayer.PutImage(0,0,temp,dmSet);
  temp.Free;
  LazPaintInstance.NotifyImageChangeCompletely(True);
end;

initialization
  {$I uemboss.lrs}

end.

