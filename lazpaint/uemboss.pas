unit UEmboss;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, BGRABitmap, LazPaintType, uscaledpi,
  ufilterconnector;

type

  { TFEmboss }

  TFEmboss = class(TForm)
    Button_OK: TButton;
    Button_Cancel: TButton;
    CheckBox_Transparent: TCheckBox;
    CheckBox_PreserveColors: TCheckBox;
    Label_Direction: TLabel;
    PaintBox1: TPaintBox;
    TrackBar_Strength: TTrackBar;
    procedure Button_OKClick(Sender: TObject);
    procedure CheckBox_Change(Sender: TObject);
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
    procedure TrackBar_StrengthChange(Sender: TObject);
  private
    { private declarations }
    InPaintBoxMouseMove: boolean;
    PaintBoxMouseMovePos: TPoint;
    angle: single;
    selectingAngle: boolean;
    procedure ComputeAngle(X,Y: integer);
    function ComputeFilteredLayer: TBGRABitmap;
    procedure PreviewNeeded;
  public
    FilterConnector: TFilterConnector;
  end;

function ShowEmbossDlg(AFilterConnector: TObject):boolean;

implementation

uses BGRABitmapTypes, math, ugraph, umac;

{ TFEmboss }

function ShowEmbossDlg(AFilterConnector: TObject):boolean;
var
  FEmboss: TFEmboss;
begin
  result := false;
  FEmboss:= TFEmboss.create(nil);
  FEmboss.FilterConnector := AFilterConnector as TFilterConnector;
  try
    if FEmboss.FilterConnector.ActiveLayer <> nil then
      result:= (FEmboss.showModal = mrOk)
    else
      result := false;
  finally
    FEmboss.free;
  end;
end;

procedure TFEmboss.FormCreate(Sender: TObject);
begin
  ScaleDPI(Self,OriginalDPI);

  InPaintBoxMouseMove := false;
  CheckOKCancelBtns(Button_OK,Button_Cancel);
end;

procedure TFEmboss.FormDestroy(Sender: TObject);
begin

end;

procedure TFEmboss.FormShow(Sender: TObject);
begin
  angle := FilterConnector.LazPaintInstance.Config.DefaultEmbossAngle;
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

{$R *.lfm}

end.

