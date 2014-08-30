unit UTwirl;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Spin, ExtCtrls, BGRABitmap, LazPaintType, uscaledpi, ufilterconnector, BGRABitmapTypes;

type

  { TFTwirl }

  TFTwirl = class(TForm)
    Button_Cancel: TButton;
    Button_OK: TButton;
    Label_Radius: TLabel;
    Label_Angle: TLabel;
    PaintBox1: TPaintBox;
    SpinEdit_Angle: TSpinEdit;
    SpinEdit_Radius: TSpinEdit;
    Timer1: TTimer;
    procedure Button_OKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBox1Paint(Sender: TObject);
    procedure SpinEdit_AngleChange(Sender: TObject);
    procedure SpinEdit_RadiusChange(Sender: TObject);
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

function ShowTwirlDlg(AFilterConnector: TObject):boolean;

implementation

uses umac;

function ShowTwirlDlg(AFilterConnector: TObject):boolean;
var
  FTwirl: TFTwirl;
begin
  result := false;
  FTwirl:= TFTwirl.create(nil);
  FTwirl.FilterConnector := AFilterConnector as TFilterConnector;
  try
    if FTwirl.FilterConnector.ActiveLayer <> nil then
      result:= (FTwirl.showModal = mrOk)
    else
      result := false;
  finally
    FTwirl.free;
  end;
end;

{ TFTwirl }

procedure TFTwirl.FormCreate(Sender: TObject);
begin
  ScaleDPI(Self,OriginalDPI);

  CheckSpinEdit(SpinEdit_Radius);
  CheckSpinEdit(SpinEdit_Angle);
  CheckOKCancelBtns(Button_OK,Button_Cancel);

  FCenter := PointF(0.5,0.5);
end;

procedure TFTwirl.FormDestroy(Sender: TObject);
begin
end;

procedure TFTwirl.FormShow(Sender: TObject);
begin
  FInitializing:= true;
  SpinEdit_Radius.Value := round(FilterConnector.LazPaintInstance.Config.DefaultTwirlRadius);
  SpinEdit_Angle.Value := round(FilterConnector.LazPaintInstance.Config.DefaultTwirlTurn*360);
  FInitializing := false;
  PreviewNeeded;
  Left := FilterConnector.LazPaintInstance.MainFormBounds.Left
end;

procedure TFTwirl.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FCenter := PointF(X/PaintBox1.Width,Y/PaintBox1.Height);
  PaintBox1.Invalidate;
  PreviewNeeded;
end;

procedure TFTwirl.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if ssLeft in Shift then
  begin
    FCenter := PointF(X/PaintBox1.Width,Y/PaintBox1.Height);
    PaintBox1.Invalidate;
    PreviewNeeded;
  end;
end;

procedure TFTwirl.PaintBox1Paint(Sender: TObject);
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

procedure TFTwirl.SpinEdit_AngleChange(Sender: TObject);
begin
  if not FInitializing then PreviewNeeded;
end;

procedure TFTwirl.SpinEdit_RadiusChange(Sender: TObject);
begin
  if not FInitializing then PreviewNeeded;
end;

procedure TFTwirl.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := false;
  FilterConnector.PutImage(ComputeFilteredLayer,False,true);
  Button_OK.Enabled := true;
end;

procedure TFTwirl.PreviewNeeded;
begin
  Timer1.Enabled := false;
  Timer1.Enabled := True;
  Button_OK.Enabled := false;
end;

function TFTwirl.ComputeFilteredLayer: TBGRABitmap;
begin
  result := FilterConnector.BackupLayer.FilterTwirl(FilterConnector.WorkArea, Point(round(FCenter.X*FilterConnector.ActiveLayer.Width),round(FCenter.Y*FilterConnector.ActiveLayer.Height)),
      SpinEdit_Radius.Value,SpinEdit_Angle.Value/360) as TBGRABitmap;
end;

procedure TFTwirl.Button_OKClick(Sender: TObject);
begin
  FilterConnector.ValidateAction;
  FilterConnector.LazPaintInstance.Config.SetDefaultTwirlRadius(SpinEdit_Radius.Value);
  FilterConnector.LazPaintInstance.Config.SetDefaultTwirlTurn(SpinEdit_Angle.Value/360);
  ModalResult := mrOK;
end;

{$R *.lfm}

end.

