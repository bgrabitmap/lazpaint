unit UWaveDisplacement;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin, UFilterConnector, BGRABitmap, BGRABitmapTypes, LazPaintType;

type

  { TFWaveDisplacement }

  TFWaveDisplacement = class(TForm)
    Button_Cancel: TButton;
    Button_OK: TButton;
    Label_Displacement: TLabel;
    Label_Phase: TLabel;
    Label_Wavelength: TLabel;
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    Panel2: TPanel;
    SpinEdit_Displacement: TSpinEdit;
    SpinEdit_Phase: TSpinEdit;
    SpinEdit_Wavelength: TSpinEdit;
    Timer1: TTimer;
    procedure Button_OKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBox1Paint(Sender: TObject);
    procedure SpinEdit_DisplacementChange(Sender: TObject);
    procedure SpinEdit_PhaseChange(Sender: TObject);
    procedure SpinEdit_WavelengthChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    FInitializing: boolean;
    FCenter: TPointF;
    procedure PreviewNeeded;
    function ComputeFilteredLayer: TBGRABitmap;
  public
    FilterConnector: TFilterConnector;
  end;

var
  FWaveDisplacement: TFWaveDisplacement;

function ShowWaveDisplacementDlg(AFilterConnector: TObject):boolean;

implementation

uses umac, ugraph, LCScaleDPI;

function ShowWaveDisplacementDlg(AFilterConnector: TObject):boolean;
var
  FWaveDisplacement: TFWaveDisplacement;
begin
  result := false;
  FWaveDisplacement:= TFWaveDisplacement.create(nil);
  FWaveDisplacement.FilterConnector := AFilterConnector as TFilterConnector;
  try
    if FWaveDisplacement.FilterConnector.ActiveLayer <> nil then
      result:= (FWaveDisplacement.showModal = mrOk)
    else
      result := false;
  finally
    FWaveDisplacement.free;
  end;
end;

{ TFWaveDisplacement }

procedure TFWaveDisplacement.FormCreate(Sender: TObject);
begin
  ScaleControl(Self,OriginalDPI);

  CheckSpinEdit(SpinEdit_Wavelength);
  CheckSpinEdit(SpinEdit_Displacement);
  CheckSpinEdit(SpinEdit_Phase);
  CheckOKCancelBtns(Button_OK{,Button_Cancel});

  FCenter := PointF(0.5,0.5);
end;

procedure TFWaveDisplacement.FormShow(Sender: TObject);
begin
  FInitializing:= true;
  SpinEdit_Wavelength.Value := round(FilterConnector.LazPaintInstance.Config.DefaultWaveDisplacementWavelength);
  SpinEdit_Displacement.Value := round(FilterConnector.LazPaintInstance.Config.DefaultWaveDisplacementAmount);
  SpinEdit_Phase.Value := round(FilterConnector.LazPaintInstance.Config.DefaultWaveDisplacementPhase);
  FInitializing := false;
  PreviewNeeded;
  Left := FilterConnector.LazPaintInstance.MainFormBounds.Left;
end;

procedure TFWaveDisplacement.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FCenter := PointF(X/PaintBox1.Width,Y/PaintBox1.Height);
  PaintBox1.Invalidate;
  PreviewNeeded;
end;

procedure TFWaveDisplacement.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if ssLeft in Shift then
  begin
    FCenter := PointF(X/PaintBox1.Width,Y/PaintBox1.Height);
    PaintBox1.Invalidate;
    PreviewNeeded;
  end;
end;

procedure TFWaveDisplacement.PaintBox1Paint(Sender: TObject);
var x,y: integer;
begin
  x := round(FCenter.X*PaintBox1.Width);
  y := round(FCenter.Y*PaintBox1.Height);
  PaintBox1.Canvas.Brush.Style := bsClear;
  PaintBox1.Canvas.Pen.Style := psSolid;
  PaintBox1.Canvas.Pen.Color := clWindowText;
  PaintBox1.Canvas.Rectangle(0,0,PaintBox1.Width,PaintBox1.Height);
  PaintBox1.Canvas.Pen.Color := clBlack;
  PaintBox1.Canvas.Brush.Style := bsSolid;
  PaintBox1.Canvas.Brush.Color := clWhite;
  PaintBox1.Canvas.Ellipse(x-3,y-3,x+4,y+4);
end;

procedure TFWaveDisplacement.SpinEdit_WavelengthChange(Sender: TObject);
begin
  if not FInitializing then PreviewNeeded;
end;

procedure TFWaveDisplacement.SpinEdit_DisplacementChange(Sender: TObject);
begin
  if not FInitializing then PreviewNeeded;
end;

procedure TFWaveDisplacement.SpinEdit_PhaseChange(Sender: TObject);
begin
  if not FInitializing then PreviewNeeded;
end;

procedure TFWaveDisplacement.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := false;
  FilterConnector.PutImage(ComputeFilteredLayer,False,true);
  Button_OK.Enabled := true;
end;

procedure TFWaveDisplacement.PreviewNeeded;
begin
  Timer1.Enabled := false;
  Timer1.Enabled := True;
  Button_OK.Enabled := false;
end;

function TFWaveDisplacement.ComputeFilteredLayer: TBGRABitmap;
begin
  result := ugraph.WaveDisplacementFilter(FilterConnector.BackupLayer,FilterConnector.WorkArea,
      PointF(FCenter.X*FilterConnector.ActiveLayer.Width,FCenter.Y*FilterConnector.ActiveLayer.Height),
      SpinEdit_Wavelength.Value,SpinEdit_Displacement.Value,SpinEdit_Phase.Value) as TBGRABitmap;
end;

procedure TFWaveDisplacement.Button_OKClick(Sender: TObject);
begin
  FilterConnector.ValidateAction;
  FilterConnector.LazPaintInstance.Config.SetDefaultWaveDisplacementWavelength(SpinEdit_Wavelength.Value);
  FilterConnector.LazPaintInstance.Config.SetDefaultWaveDisplacementAmount(SpinEdit_Displacement.Value);
  FilterConnector.LazPaintInstance.Config.SetDefaultWaveDisplacementPhase(SpinEdit_Phase.Value);
  ModalResult := mrOK;
end;

{$R *.lfm}

end.

