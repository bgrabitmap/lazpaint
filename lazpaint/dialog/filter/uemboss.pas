// SPDX-License-Identifier: GPL-3.0-only
unit UEmboss;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, BGRABitmap, LazPaintType, LCScaleDPI,
  UFilterConnector, UScripting;

type

  { TFEmboss }

  TFEmboss = class(TForm)
    Button_OK: TButton;
    Button_Cancel: TButton;
    CheckBox_Transparent: TCheckBox;
    CheckBox_PreserveColors: TCheckBox;
    Label_Direction: TLabel;
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    Panel2: TPanel;
    TrackBar_Strength: TTrackBar;
    procedure Button_OKClick(Sender: TObject);
    procedure CheckBox_Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseMove(Sender: TObject; {%H-}Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure PaintBox1Paint(Sender: TObject);
    procedure TrackBar_StrengthChange(Sender: TObject);
  private
    { private declarations }
    InPaintBoxMouseMove: boolean;
    PaintBoxMouseMovePos: TPoint;
    angle: single;
    selectingAngle: boolean;
    FInitializing: boolean;
    procedure ComputeAngle(X,Y: integer);
    function ComputeFilteredLayer: TBGRABitmap;
    procedure PreviewNeeded;
    procedure InitParams;
  public
    FilterConnector: TFilterConnector;
    FVars: TVariableSet;
  end;

function ShowEmbossDlg(AFilterConnector: TObject): TScriptResult;

implementation

uses BGRABitmapTypes, math, ugraph, umac;

{ TFEmboss }

function ShowEmbossDlg(AFilterConnector: TObject): TScriptResult;
var
  FEmboss: TFEmboss;
begin
  FEmboss:= TFEmboss.create(nil);
  FEmboss.FilterConnector := AFilterConnector as TFilterConnector;
  FEmboss.FVars := FEmboss.FilterConnector.Parameters;
  try
    if FEmboss.FilterConnector.ActiveLayer <> nil then
    begin
      if Assigned(FEmboss.FilterConnector.Parameters) and
        FEmboss.FilterConnector.Parameters.Booleans['Validate'] then
      begin
        FEmboss.InitParams;
        FEmboss.PreviewNeeded;
        FEmboss.FilterConnector.ValidateAction;
        result := srOk;
      end else
      begin
        if FEmboss.showModal = mrOk then result := srOk
        else result := srCancelledByUser;
      end;
    end
    else
      result := srException;
  finally
    FEmboss.free;
  end;
end;

procedure TFEmboss.FormCreate(Sender: TObject);
begin
  ScaleControl(Self,OriginalDPI);

  InPaintBoxMouseMove := false;
  CheckOKCancelBtns(Button_OK{,Button_Cancel});
end;

procedure TFEmboss.FormShow(Sender: TObject);
begin
  InitParams;
  PreviewNeeded;
  Left := FilterConnector.LazPaintInstance.MainFormBounds.Left
end;

procedure TFEmboss.Button_OKClick(Sender: TObject);
begin
  FilterConnector.ValidateAction;
  FilterConnector.LazPaintInstance.Config.SetDefaultEmbossAngle(angle);
  ModalResult := mrOK;
end;

procedure TFEmboss.CheckBox_Change(Sender: TObject);
begin
  PreviewNeeded;
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
  ComputeAngle(PaintBoxMouseMovePos.X,PaintBoxMouseMovePos.Y);
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
    scaling: Double;
begin
  scaling := GetCanvasScaleFactor;
  bmp := TBGRABitmap.Create(round(PaintBox1.Width*scaling),round(PaintBox1.Height*scaling));
  bmp.Fill(clForm);
  c := ColorToBGRA(ColorToRGB(clWindowText));
  t := min(bmp.Width/2,bmp.Height/2);
  dx := cos(angle*Pi/180);
  dy := sin(angle*Pi/180);
  x1 := bmp.Width/2;
  y1 := bmp.Height/2;
  x2 := x1+dx*(t-2);
  y2 := y1+dy*(t-2);
  bmp.FillEllipseAntialias(x1,y1,t-1,t-1,BGRA(c.red,c.green,c.blue,48));
  bmp.DrawLineAntialias(x1,y1,x2,y2,c,2*scaling,true);
  bmp.DrawLineAntialias(x2+dy*5*scaling-dx*5*scaling,
    y2-dx*5*scaling-dy*5*scaling,x2,y2,c,2*scaling,false);
  bmp.DrawLineAntialias(x2-dy*5*scaling-dx*5*scaling,
    y2+dx*5*scaling-dy*5*scaling,x2,y2,c,2*scaling,false);
  bmp.Draw(PaintBox1.Canvas, rect(0,0,PaintBox1.Width,PaintBox1.Height), true);
  bmp.Free;
end;

procedure TFEmboss.TrackBar_StrengthChange(Sender: TObject);
begin
  PreviewNeeded;
  PaintBox1.Repaint;
end;

procedure TFEmboss.ComputeAngle(X, Y: integer);
begin
  if selectingAngle then
  begin
    angle := ugraph.ComputeAngle(X-PaintBox1.Width/2,Y-PaintBox1.Height/2);
    PreviewNeeded;
    PaintBox1.Repaint;
  end;
end;

function TFEmboss.ComputeFilteredLayer: TBGRABitmap;
var options: TEmbossOptions;
begin
  options := [];
  if CheckBox_Transparent.Checked then options += [eoTransparent];
  if CheckBox_PreserveColors.Checked then options += [eoPreserveHue];
  result := FilterConnector.BackupLayer.FilterEmboss(angle,FilterConnector.WorkArea,TrackBar_Strength.Position,options) as TBGRABitmap;
end;

procedure TFEmboss.PreviewNeeded;
begin
  FilterConnector.PutImage(ComputeFilteredLayer,False,True);
end;

procedure TFEmboss.InitParams;
begin
  FInitializing:= true;
  angle := FilterConnector.LazPaintInstance.Config.DefaultEmbossAngle;
  if Assigned(FVars) then
  begin
    if FVars.IsDefined('Angle') then angle := FVars.Floats['Angle'];
    if FVars.IsDefined('Transparent') then
      CheckBox_Transparent.Checked := FVars.Booleans['Transparent'];
    if FVars.IsDefined('PreserveColors') then
      CheckBox_PreserveColors.Checked := FVars.Booleans['PreserveColors'];
  end;
  FInitializing:= false;
end;

{$R *.lfm}

end.

