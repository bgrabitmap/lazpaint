// SPDX-License-Identifier: GPL-3.0-only
unit UMotionBlur;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Spin, ExtCtrls, BGRABitmap, LazPaintType, LCScaleDPI,
  UFilterConnector, UFilterThread, UScripting;

type

  { TFMotionBlur }

  TFMotionBlur = class(TForm)
    Button_OK: TButton;
    Button_Cancel: TButton;
    Checkbox_Oriented: TCheckBox;
    Panel1: TPanel;
    Panel2: TPanel;
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
    FVars: TVariableSet;
    angle: single;
    selectingAngle: boolean;
    InPaintBoxMouseMove: boolean;
    PaintBoxMouseMovePos: TPoint;
    FQuitQuery: boolean;
    procedure UpdateStep;
    procedure ComputeAngle(X,Y: integer);
    procedure InitParams;
    procedure PreviewNeeded;
    procedure OnTaskEvent({%H-}ASender: TObject; AEvent: TThreadManagerEvent);
  end;

function ShowMotionBlurDlg(AFilterConnector: TObject): TScriptResult;

implementation

uses BGRABitmapTypes, math, ugraph, umac, BGRAFilters;

function ShowMotionBlurDlg(AFilterConnector: TObject): TScriptResult;
var
  FMotionBlur: TFMotionBlur;
begin
  FMotionBlur:= TFMotionBlur.create(nil);
  FMotionBlur.FFilterConnector := AFilterConnector as TFilterConnector;
  FMotionBlur.FThreadManager := TFilterThreadManager.Create(FMotionBlur.FFilterConnector);
  FMotionBlur.FThreadManager.OnEvent:= @FMotionBlur.OnTaskEvent;
  FMotionBlur.FVars := FMotionBlur.FFilterConnector.Parameters;
  try
    if FMotionBlur.FFilterConnector.ActiveLayer <> nil then
    begin
      if Assigned(FMotionBlur.FFilterConnector.Parameters) and
        FMotionBlur.FFilterConnector.Parameters.Booleans['Validate'] then
      begin
        FMotionBlur.InitParams;
        FMotionBlur.PreviewNeeded;
        FMotionBlur.FFilterConnector.LazPaintInstance.Wait(@FMotionBlur.FThreadManager.RegularCheck, 50);
        FMotionBlur.FFilterConnector.ValidateAction;
        result := srOk;
      end else
      begin
        if FMotionBlur.ShowModal = mrOk then result := srOk
        else result := srCancelledByUser;
      end;
    end
    else
      result := srException;
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
    scaling: Double;
begin
  scaling := GetCanvasScaleFactor;
  bmp := TBGRABitmap.Create(round(PaintBox1.Width*scaling),
    round(PaintBox1.Height*scaling));
  bmp.Fill(clForm);
  c := ColorToBGRA(ColorToRGB(clWindowText));
  t := min(bmp.Width/2,bmp.Height/2);
  dx := cos(angle*Pi/180);
  dy := sin(angle*Pi/180);
  x1 := bmp.Width/2;
  y1 := bmp.Height/2;
  x0 := x1-dx*(t-2);
  y0 := y1-dy*(t-2);
  x2 := x1+dx*(t-2);
  y2 := y1+dy*(t-2);
  bmp.FillEllipseAntialias(x1,y1,t-1,t-1,BGRA(c.red,c.green,c.blue,48));
  if Checkbox_Oriented.Checked then
  begin
    bmp.DrawLineAntialias(x1,y1,x2,y2,c,2*scaling,true);
    bmp.DrawLineAntialias(x2+dy*5*scaling-dx*5*scaling,
      y2-dx*5*scaling-dy*5*scaling,x2,y2,c,2*scaling,false);
    bmp.DrawLineAntialias(x2-dy*5*scaling-dx*5*scaling,
      y2+dx*5*scaling-dy*5*scaling,x2,y2,c,2*scaling,false);
  end else
  begin
    bmp.DrawLineAntialias(x0,y0,x2,y2,c,2*scaling,true);
  end;
  bmp.Draw(PaintBox1.Canvas, rect(0,0,PaintBox1.Width,PaintBox1.Height), true);
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

procedure TFMotionBlur.InitParams;
begin
  if Assigned(FVars) and (FVars.IsDefined('Oriented')) then
    Checkbox_Oriented.Checked := FVars.Booleans['Oriented']
  else
    Checkbox_Oriented.Checked := FFilterConnector.LazPaintInstance.Config.DefaultBlurMotionOriented;

  if Assigned(FVars) and (FVars.IsDefined('Distance')) then
    SpinEdit_Distance.Value := FVars.Floats['Distance']
  else
    SpinEdit_Distance.Value := FFilterConnector.LazPaintInstance.Config.DefaultBlurMotionDistance;

  UpdateStep;

  if Assigned(FVars) and (FVars.IsDefined('Angle')) then
    angle := FVars.Floats['Angle']
  else
    angle := FFilterConnector.LazPaintInstance.Config.DefaultBlurMotionAngle;
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
  ScaleControl(Self,OriginalDPI);

  InPaintBoxMouseMove := false;
  CheckOKCancelBtns(Button_OK{,Button_Cancel});
  CheckFloatSpinEdit(SpinEdit_Distance);
  SpinEdit_Distance.Constraints.MinWidth := DoScaleX(70, OriginalDPI);
end;

procedure TFMotionBlur.FormShow(Sender: TObject);
begin
  FQuitQuery := false;
  InitParams;
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
  if not (ssLeft in Shift) then selectingAngle:= false;
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

