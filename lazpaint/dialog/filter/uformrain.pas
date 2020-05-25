// SPDX-License-Identifier: GPL-3.0-only
unit UFormRain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, UFilterConnector, BGRABitmapTypes, BGRABitmap,
  URainType, UScripting;

type

  { TFRain }

  TFRain = class(TForm)
    Button_Cancel: TButton;
    Button_OK: TButton;
    Label_Wind: TLabel;
    Label_Quantity: TLabel;
    PaintBox_Wind: TPaintBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Timer1: TTimer;
    TrackBar_Quantity: TTrackBar;
    procedure Button_OKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PaintBox_WindMouseDown(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure PaintBox_WindMouseMove(Sender: TObject; {%H-}Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBox_WindMouseUp(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure PaintBox_WindPaint(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    wind: single;
    FRenderer: TRainRenderer;
    InPaintBoxMouseMove: boolean;
    PaintBoxMouseMovePos: TPoint;
    selectingWind: boolean;
    FVars: TVariableSet;
    function GetRainRenderer: TRainRenderer;
    function GetRainQuantity: single;
    procedure SetRainQuantity(AValue: single);
    procedure ComputeWind(X,{%H-}Y: integer);
    function ComputeFilteredLayer: TBGRABitmap;
    procedure PreviewNeeded;
    procedure InitParams;
    property RainQuantity: single read GetRainQuantity write SetRainQuantity;
    property RainRenderer: TRainRenderer read GetRainRenderer;
  public
    { public declarations }
    FilterConnector: TFilterConnector;
    renderTimeInS: Double;
  end;

function ShowRainDlg(AFilterConnector: TObject): TScriptResult;

implementation

uses LCScaleDPI, umac, LazPaintType;

function ShowRainDlg(AFilterConnector: TObject): TScriptResult;
var
  FRain: TFRain;
begin
  FRain := TFRain.create(nil);
  FRain.FilterConnector := AFilterConnector as TFilterConnector;
  FRain.FVars := FRain.FilterConnector.Parameters;
  try
    if FRain.FilterConnector.ActiveLayer <> nil then
    begin
      if Assigned(FRain.FVars) and FRain.FVars.Booleans['Validate'] then
      begin
        FRain.InitParams;
        FRain.PreviewNeeded;
        FRain.FilterConnector.ValidateAction;
        result := srOk;
      end else
      begin
        if FRain.showModal = mrOk then result := srOk
        else result := srCancelledByUser;
      end;
    end
    else
      result := srException;
  finally
    FRain.free;
  end;
end;

{ TFRain }

procedure TFRain.FormCreate(Sender: TObject);
begin
  ScaleControl(Self,OriginalDPI);
  CheckOKCancelBtns(Button_OK{,Button_Cancel});
end;

procedure TFRain.Button_OKClick(Sender: TObject);
begin
  Timer1.Enabled := false;
  FilterConnector.ValidateAction;
  FilterConnector.LazPaintInstance.Config.SetDefaultRainWind(wind);
  FilterConnector.LazPaintInstance.Config.SetDefaultRainQuantity(RainQuantity);
  ModalResult := mrOK;
end;

procedure TFRain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FRenderer);
end;

procedure TFRain.FormHide(Sender: TObject);
begin
  Timer1.Enabled := false;
end;

procedure TFRain.FormShow(Sender: TObject);
begin
  InitParams;
  PreviewNeeded;
  Left := FilterConnector.LazPaintInstance.MainFormBounds.Left;
  Timer1.Enabled := true;
end;

procedure TFRain.PaintBox_WindMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if button = mbLeft then selectingWind := true;
  ComputeWind(X,Y);
end;

procedure TFRain.PaintBox_WindMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  PaintBoxMouseMovePos := Point(X,Y);
  if InPaintBoxMouseMove then Exit;
  InPaintBoxMouseMove := True;
  Application.ProcessMessages; //empty message stack
  ComputeWind(PaintBoxMouseMovePos.X,PaintBoxMouseMovePos.Y);
  InPaintBoxMouseMove := False;
end;

procedure TFRain.PaintBox_WindMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if button = mbLeft then selectingWind := false;
end;

procedure TFRain.PaintBox_WindPaint(Sender: TObject);
var bmp: TBGRABitmap;
    x1,y1,x2,y2: single;
    angle,dx,dy,t: single;
    c: TBGRAPixel;
begin
    if wind = 0 then exit;
    bmp := TBGRABitmap.Create(PaintBox_Wind.Width,PaintBox_Wind.Height);
    bmp.Fill(clForm);
    c := ColorToBGRA(ColorToRGB(clWindowText));
    if wind < 0 then angle := 180 else angle := 0;
    t := PaintBox_Wind.Width/2*abs(wind);
    dx := cos(angle*Pi/180);
    dy := sin(angle*Pi/180);
    x1 := PaintBox_Wind.Width/2;
    y1 := PaintBox_Wind.Height/2;
    x2 := x1+dx*(t-2);
    y2 := y1+dy*(t-2);
    bmp.FillEllipseAntialias(x1,y1,t-1,t-1,BGRA(c.red,c.green,c.blue,48));
    bmp.DrawLineAntialias(x1,y1,x2,y2,c,2,true);
    bmp.DrawLineAntialias(x2+dy*5-dx*5,y2-dx*5-dy*5,x2,y2,c,2,false);
    bmp.DrawLineAntialias(x2-dy*5-dx*5,y2+dx*5-dy*5,x2,y2,c,2,false);
    bmp.Draw(PaintBox_Wind.Canvas,0,0,true);
    bmp.Free;
end;

procedure TFRain.Timer1Timer(Sender: TObject);
begin
  if selectingWind or (renderTimeInS > 0.5) then exit;
  Timer1.Enabled:= false;
  RainRenderer.RainElapse(0.1, RainQuantity, FilterConnector.BackupLayer.Width, FilterConnector.BackupLayer.Height);
  PreviewNeeded;
  Timer1.Enabled:= true;
end;

function TFRain.GetRainRenderer: TRainRenderer;
begin
  if FRenderer = nil then
  begin
     FRenderer := TRainRenderer.Create(wind,2);
     FRenderer.RainElapse(0, RainQuantity, FilterConnector.BackupLayer.Width, FilterConnector.BackupLayer.Height);
  end;
  result := FRenderer;
end;

function TFRain.GetRainQuantity: single;
begin
  result := TrackBar_Quantity.Position/TrackBar_Quantity.Max;
end;

procedure TFRain.SetRainQuantity(AValue: single);
begin
  TrackBar_Quantity.Position := round(AValue*TrackBar_Quantity.Max);
end;

procedure TFRain.ComputeWind(X, Y: integer);
var newWind : single;
begin
  if selectingWind then
  begin
    newWind := (x/(PaintBox_Wind.Width-1))*2-1;
    if newWind < -1 then newWind := -1
    else if newWind > 1 then newWind:= 1;
    if newWind <> wind then
    begin
      wind := newWind;
      FreeAndNil(FRenderer);
      PreviewNeeded;
      PaintBox_Wind.Repaint;
    end;
  end;
end;

function TFRain.ComputeFilteredLayer: TBGRABitmap;
begin
  renderTimeInS := Now;
  result := FilterConnector.BackupLayer.Duplicate as TBGRABitmap;
  RainRenderer.RenderRain(result);
  renderTimeInS:= (Now-renderTimeInS)*(60*60*24);
end;

procedure TFRain.PreviewNeeded;
begin
  FilterConnector.PutImage(ComputeFilteredLayer,False,True);
end;

procedure TFRain.InitParams;
begin
  if Assigned(FVars) and FVars.IsDefined('Wind') then
    wind := FVars.Floats['Wind']
  else wind := FilterConnector.LazPaintInstance.Config.DefaultRainWind;
  if Assigned(FVars) and FVars.IsDefined('Amount') then
    RainQuantity := FVars.Floats['Amount']
  else RainQuantity := FilterConnector.LazPaintInstance.Config.DefaultRainQuantity;
end;

{$R *.lfm}

end.

