unit umotionblur;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Spin, ExtCtrls, BGRABitmap, LazPaintType, uscaledpi;

type

  { TFMotionBlur }

  TFMotionBlur = class(TForm)
    Button_OK: TButton;
    Button_Cancel: TButton;
    Checkbox_Oriented: TCheckBox;
    Label3: TLabel;
    PaintBox1: TPaintBox;
    SpinEdit_Distance: TSpinEdit;
    Timer1: TTimer;
    procedure Button_OKClick(Sender: TObject);
    procedure Checkbox_OrientedChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
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
    procedure SpinEdit_DistanceChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    angle: single;
    selectingAngle: boolean;
    InPaintBoxMouseMove: boolean;
    PaintBoxMouseMovePos: TPoint;
    backupSource: TBGRABitmap;
    filteredLayerComputed: boolean;
    procedure ComputeAngle(X,Y: integer);
    procedure ComputeFilteredLayer;
    procedure PreviewNeeded;
  public
    { public declarations }
    LazPaintInstance: TLazPaintCustomInstance;
    sourceLayer, filteredLayer: TBGRABitmap;
  end; 

function ShowMotionBlurDlg(Instance: TLazPaintCustomInstance; layer:TBGRABitmap; out filteredLayer: TBGRABitmap):boolean;

implementation

uses BGRABitmapTypes, math, ugraph, umac;

{ TFMotionBlur }

function ShowMotionBlurDlg(Instance: TLazPaintCustomInstance; layer:TBGRABitmap; out filteredLayer: TBGRABitmap):boolean;
var
  FMotionBlur: TFMotionBlur;
begin
  filteredLayer := nil;
  result := false;
  FMotionBlur:= TFMotionBlur.create(nil);
  FMotionBlur.LazPaintInstance := Instance;
  try
    FMotionBlur.sourceLayer := layer;
    result:= (FMotionBlur.showmodal = mrOk);
    filteredLayer := FMotionBlur.filteredLayer;
  finally
    FMotionBlur.free;
  end;
end;

procedure TFMotionBlur.PaintBox1Paint(Sender: TObject);
var bmp: TBGRABitmap;
    x0,y0,x1,y1,x2,y2: single;
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
    x0 := x1-dx*(t-2);
    y0 := y1-dy*(t-2);
    x2 := x1+dx*(t-2);
    y2 := y1+dy*(t-2);
    bmp.FillEllipseAntialias(x1,y1,t-1,t-1,BGRA(c.red,c.green,c.blue,48));
    if Checkbox_Oriented.Checked then
    begin
      bmp.DrawLineAntialias(x1,y1,x2,y2,c,2,true);
      bmp.DrawLineAntialias(x2+dy*5-dx*5,y2-dx*5-dy*5,x2,y2,c,2,false);
      bmp.DrawLineAntialias(x2-dy*5-dx*5,y2+dx*5-dy*5,x2,y2,c,2,false);
    end else
    begin
      bmp.DrawLineAntialias(x0,y0,x2,y2,c,2,true);
    end;
    bmp.Draw(PaintBox1.Canvas,0,0,true);
    bmp.Free;
end;

procedure TFMotionBlur.SpinEdit_DistanceChange(Sender: TObject);
begin
  PreviewNeeded;
end;

procedure TFMotionBlur.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := false;
  if (sourceLayer = nil) or filteredLayerComputed then exit;

  ComputeFilteredLayer;
  if backupSource = nil then
    backupSource := sourceLayer.Duplicate as TBGRABitmap;
  sourceLayer.PutImage(0,0,filteredLayer,dmSet);
  LazPaintInstance.NotifyImageChangeCompletely(True);
end;

procedure TFMotionBlur.ComputeAngle(X, Y: integer);
var oldangle: single;
begin
  oldangle := angle;
  angle := ugraph.ComputeAngle(X-PaintBox1.Width/2,Y-PaintBox1.Height/2);
  if angle <> oldangle then PreviewNeeded;
  PaintBox1.Repaint;
end;

procedure TFMotionBlur.ComputeFilteredLayer;
var usedSource: TBGRABitmap;
begin
  Cursor := crHourGlass;
  FreeAndNil(filteredLayer);
  if backupSource <> nil then
    usedSource := backupSource
  else
    usedSource := sourceLayer;
  filteredLayer := usedSource.FilterBlurMotion(SpinEdit_Distance.Value, angle, Checkbox_Oriented.Checked) as TBGRABitmap;
  cursor := crDefault;
  filteredLayerComputed := true;
end;

procedure TFMotionBlur.PreviewNeeded;
begin
  Timer1.Enabled := false;
  Timer1.Enabled := True;
  filteredLayerComputed := false;
end;

procedure TFMotionBlur.FormCreate(Sender: TObject);
begin
  ScaleDPI(Self,OriginalDPI);

  InPaintBoxMouseMove := false;
  CheckOKCancelBtns(Button_OK,Button_Cancel);
  CheckSpinEdit(SpinEdit_Distance);
  filteredLayer := nil;
  backupSource := nil;
  filteredLayerComputed := false;
end;

procedure TFMotionBlur.FormDestroy(Sender: TObject);
begin
  if backupSource <> nil then
  begin
    sourceLayer.PutImage(0,0,backupSource,dmSet);
    FreeAndNil(backupSource);
    LazPaintInstance.NotifyImageChangeCompletely(False);
  end;
end;

procedure TFMotionBlur.FormShow(Sender: TObject);
begin
  Checkbox_Oriented.Checked := LazPaintInstance.Config.DefaultBlurMotionOriented;
  SpinEdit_Distance.Value := LazPaintInstance.Config.DefaultBlurMotionDistance;
  angle := LazPaintInstance.Config.DefaultBlurMotionAngle;
  Timer1.Enabled := true;
end;

procedure TFMotionBlur.PaintBox1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if button = mbLeft then selectingAngle := true;
  ComputeAngle(X,Y);
end;

procedure TFMotionBlur.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  PaintBoxMouseMovePos := Point(X,Y);
  if InPaintBoxMouseMove then Exit;
  InPaintBoxMouseMove := True;
  Application.ProcessMessages; //empty message stack
  if selectingAngle then ComputeAngle(PaintBoxMouseMovePos.X,PaintBoxMouseMovePos.Y);
  InPaintBoxMouseMove := False;
end;

procedure TFMotionBlur.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if button = mbLeft then selectingAngle := false;
end;

procedure TFMotionBlur.Button_OKClick(Sender: TObject);
begin
    if sourceLayer <> nil then
    begin
      if not filteredLayerComputed then ComputeFilteredLayer;
      LazPaintInstance.Config.SetDefaultBlurMotionOriented(Checkbox_Oriented.Checked);
      LazPaintInstance.Config.SetDefaultBlurMotionDistance(SpinEdit_Distance.Value);
      LazPaintInstance.Config.SetDefaultBlurMotionAngle(angle);
      ModalResult := mrOK;
    end else
      ModalResult := mrCancel;
end;

procedure TFMotionBlur.Checkbox_OrientedChange(Sender: TObject);
begin
  PaintBox1.Repaint;
  PreviewNeeded;
end;

procedure TFMotionBlur.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  if ModalResult <> mrOk then
    FreeAndNil(filteredLayer);
end;

initialization
  {$I umotionblur.lrs}

end.

