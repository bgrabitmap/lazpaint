unit UMotionBlur;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Spin, ExtCtrls, BGRABitmap, LazPaintType, uscaledpi,
  UFilterConnector, UFilterThread;

type

  { TFMotionBlur }

  TFMotionBlur = class(TForm)
    Button_OK: TButton;
    Button_Cancel: TButton;
    Checkbox_Oriented: TCheckBox;
    SpinEdit_Distance: TFloatSpinEdit;
    Label_Distance: TLabel;
    PaintBox1: TPaintBox;
    Timer1: TTimer;
    procedure Button_OKClick(Sender: TObject);
    procedure Checkbox_OrientedChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseMove(Sender: TObject; {%H-}Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure PaintBox1Paint(Sender: TObject);
    procedure SpinEdit_DistanceChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    FFilterConnector: TFilterConnector;
    FThreadManager: TFilterThreadManager;
    angle: single;
    selectingAngle: boolean;
    InPaintBoxMouseMove: boolean;
    PaintBoxMouseMovePos: TPoint;
    FQuitQuery: boolean;
    procedure UpdateStep;
    procedure ComputeAngle(X,Y: integer);
    procedure PreviewNeeded;
    procedure OnTaskEvent({%H-}ASender: TObject; AEvent: TThreadManagerEvent);
  end;

function ShowMotionBlurDlg(AFilterConnector: TObject):boolean;

implementation

uses BGRABitmapTypes, math, ugraph, umac, BGRAFilters;

function ShowMotionBlurDlg(AFilterConnector: TObject):boolean;
var
  FMotionBlur: TFMotionBlur;
begin
  result := false;
  FMotionBlur:= TFMotionBlur.create(nil);
  FMotionBlur.FFilterConnector := AFilterConnector as TFilterConnector;
  FMotionBlur.FThreadManager := TFilterThreadManager.Create(FMotionBlur.FFilterConnector);
  FMotionBlur.FThreadManager.OnEvent:= @FMotionBlur.OnTaskEvent;
  try
    if FMotionBlur.FFilterConnector.ActiveLayer <> nil then
      result:= (FMotionBlur.showmodal = mrOk)
    else
      result := false;
  finally
    FMotionBlur.FThreadManager.free;
    FMotionBlur.free;
  end;
end;

{ TFMotionBlur }

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
  UpdateStep;
  PreviewNeeded;
end;

procedure TFMotionBlur.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled:= false;
  FThreadManager.RegularCheck;
  Timer1.Interval := 200;
  Timer1.Enabled:= true;
end;

procedure TFMotionBlur.UpdateStep;
var deci: integer;
begin
  deci := round(SpinEdit_Distance.Value*10) mod 10;
  if (SpinEdit_Distance.Value < 2) or ((deci <> 0) and (deci <> 5)) then
    SpinEdit_Distance.Increment := 0.1 else
  if (SpinEdit_Distance.Value < 8) or (deci<>0) then
    SpinEdit_Distance.Increment := 0.5 else
    SpinEdit_Distance.Increment := 1;
end;

procedure TFMotionBlur.ComputeAngle(X, Y: integer);
var oldangle: single;
begin
  oldangle := angle;
  angle := ugraph.ComputeAngle(X-PaintBox1.Width/2,Y-PaintBox1.Height/2);
  if angle <> oldangle then PreviewNeeded;
  PaintBox1.Repaint;
end;

procedure TFMotionBlur.PreviewNeeded;
begin
  FThreadManager.WantPreview(CreateMotionBlurTask(FFilterConnector.BackupLayer,
    FFilterConnector.WorkArea, SpinEdit_Distance.Value, angle,
    Checkbox_Oriented.Checked));
end;

procedure TFMotionBlur.OnTaskEvent(ASender: TObject; AEvent: TThreadManagerEvent
  );
begin
  case AEvent of
  tmeAbortedTask,tmeCompletedTask:
    begin
      Timer1.Enabled := false;
      if FThreadManager.ReadyToClose then
        Close
      else
        if AEvent = tmeCompletedTask then Button_OK.Enabled := true;
    end;
  tmeStartingNewTask:
    begin
      Timer1.Enabled := false;
      Timer1.Interval := 100;
      Timer1.Enabled := true;
      Button_OK.Enabled := false;
    end;
  end;
end;

procedure TFMotionBlur.FormCreate(Sender: TObject);
begin
  ScaleDPI(Self,OriginalDPI);

  InPaintBoxMouseMove := false;
  CheckOKCancelBtns(Button_OK,Button_Cancel);
  CheckFloatSpinEdit(SpinEdit_Distance);
end;

procedure TFMotionBlur.FormShow(Sender: TObject);
begin
  FQuitQuery := false;
  Checkbox_Oriented.Checked := FFilterConnector.LazPaintInstance.Config.DefaultBlurMotionOriented;
  SpinEdit_Distance.Value := FFilterConnector.LazPaintInstance.Config.DefaultBlurMotionDistance;
  UpdateStep;
  angle := FFilterConnector.LazPaintInstance.Config.DefaultBlurMotionAngle;
  PreviewNeeded;
  Left := FFilterConnector.LazPaintInstance.MainFormBounds.Left;
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
  if not FFilterConnector.ActionDone then
  begin
    FFilterConnector.ValidateAction;
    FFilterConnector.LazPaintInstance.Config.SetDefaultBlurMotionOriented(Checkbox_Oriented.Checked);
    FFilterConnector.LazPaintInstance.Config.SetDefaultBlurMotionDistance(SpinEdit_Distance.Value);
    FFilterConnector.LazPaintInstance.Config.SetDefaultBlurMotionAngle(angle);
  end;
  ModalResult := mrOK;
end;

procedure TFMotionBlur.Checkbox_OrientedChange(Sender: TObject);
begin
  PaintBox1.Repaint;
  PreviewNeeded;
end;

procedure TFMotionBlur.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  FThreadManager.Quit;
  CanClose := FThreadManager.ReadyToClose;
end;

{$R *.lfm}

end.

