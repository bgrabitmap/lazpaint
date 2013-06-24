unit UMotionBlur;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Spin, ExtCtrls, BGRABitmap, LazPaintType, uscaledpi, UFilterConnector;

type

  { TFMotionBlur }

  TFMotionBlur = class(TForm)
    Button_OK: TButton;
    Button_Cancel: TButton;
    Checkbox_Oriented: TCheckBox;
    Label_Distance: TLabel;
    PaintBox1: TPaintBox;
    SpinEdit_Distance: TSpinEdit;
    Timer1: TTimer;
    procedure Button_OKClick(Sender: TObject);
    procedure Checkbox_OrientedChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
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
    FThread: TThread;
    angle: single;
    selectingAngle: boolean;
    InPaintBoxMouseMove: boolean;
    PaintBoxMouseMovePos: TPoint;
    procedure ComputeAngle(X,Y: integer);
    procedure PreviewNeeded;
    procedure OnBlurDone({%H-}ASender: TThread; AFilteredLayer: TBGRABitmap);
    procedure ClearThread;
  end;

function ShowMotionBlurDlg(AFilterConnector: TObject):boolean;

implementation

uses BGRABitmapTypes, math, ugraph, umac, UFilterThread, BGRAFilters;

function ShowMotionBlurDlg(AFilterConnector: TObject):boolean;
var
  FMotionBlur: TFMotionBlur;
begin
  result := false;
  FMotionBlur:= TFMotionBlur.create(nil);
  FMotionBlur.FFilterConnector := AFilterConnector as TFilterConnector;
  try
    if FMotionBlur.FFilterConnector.ActiveLayer <> nil then
      result:= (FMotionBlur.showmodal = mrOk)
    else
      result := false;
  finally
    FMotionBlur.free;
  end;
end;


type
  { TMotionBlurThread }

  TMotionBlurThread = class(TFilterThread)
  protected
    FDistance, FAngle: single;
    FOriented: boolean;
    function CreateFilterTask: TFilterTask; override;
  public
    constructor Create(AConnector: TFilterConnector; ADistance, AAngle: single;
    AOriented: boolean; ASuspended: boolean);
  end;

{ TMotionBlurThread }

function TMotionBlurThread.CreateFilterTask: TFilterTask;
begin
  result := CreateMotionBlurTask(FilterConnector.BackupLayer,FilterConnector.WorkArea, FDistance, FAngle, FOriented)
end;

constructor TMotionBlurThread.Create(AConnector: TFilterConnector; ADistance, AAngle: single;
    AOriented: boolean; ASuspended: boolean);
begin
  inherited Create(AConnector, True);
  FDistance := ADistance;
  FAngle := AAngle;
  FOriented := AOriented;
  if not ASuspended then Start;
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
  PreviewNeeded;
end;

procedure TFMotionBlur.Timer1Timer(Sender: TObject);
var filteredLayer: TBGRABitmap;
begin
  if FThread <> nil then
  begin
    Timer1.Enabled:= false;
    filteredLayer := (FThread as TFilterThread).FilteredLayer;
    if filteredLayer <> nil then
      FFilterConnector.PutImage(filteredLayer,False);
    Timer1.Enabled:= true;
  end;
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
var blurThread: TFilterThread;
begin
  ClearThread;
  Button_OK.Enabled := false;
  blurThread := TMotionBlurThread.Create(FFilterConnector,SpinEdit_Distance.Value, angle, Checkbox_Oriented.Checked, True);
  blurThread.OnFilterDone:= @OnBlurDone;
  FThread := blurThread;
  FThread.Start;
end;

procedure TFMotionBlur.OnBlurDone(ASender: TThread; AFilteredLayer: TBGRABitmap
  );
begin
  FFilterConnector.PutImage(AFilteredLayer,False);
  Button_OK.Enabled := true;
  ClearThread;
end;

procedure TFMotionBlur.ClearThread;
begin
  if FThread <> nil then
  begin
    FThread.Terminate;
    FThread := nil; //let the thread free itself
  end;
end;

procedure TFMotionBlur.FormCreate(Sender: TObject);
begin
  ScaleDPI(Self,OriginalDPI);

  InPaintBoxMouseMove := false;
  CheckOKCancelBtns(Button_OK,Button_Cancel);
  CheckSpinEdit(SpinEdit_Distance);
end;

procedure TFMotionBlur.FormDestroy(Sender: TObject);
begin
  ClearThread;
end;

procedure TFMotionBlur.FormShow(Sender: TObject);
begin
  Checkbox_Oriented.Checked := FFilterConnector.LazPaintInstance.Config.DefaultBlurMotionOriented;
  SpinEdit_Distance.Value := FFilterConnector.LazPaintInstance.Config.DefaultBlurMotionDistance;
  angle := FFilterConnector.LazPaintInstance.Config.DefaultBlurMotionAngle;
  PreviewNeeded;
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
  FFilterConnector.ValidateAction;
  FFilterConnector.LazPaintInstance.Config.SetDefaultBlurMotionOriented(Checkbox_Oriented.Checked);
  FFilterConnector.LazPaintInstance.Config.SetDefaultBlurMotionDistance(SpinEdit_Distance.Value);
  FFilterConnector.LazPaintInstance.Config.SetDefaultBlurMotionAngle(angle);
  ModalResult := mrOK;
end;

procedure TFMotionBlur.Checkbox_OrientedChange(Sender: TObject);
begin
  PaintBox1.Repaint;
  PreviewNeeded;
end;

initialization
  {$I umotionblur.lrs}

end.

