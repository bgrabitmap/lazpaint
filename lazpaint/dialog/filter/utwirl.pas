// SPDX-License-Identifier: GPL-3.0-only
unit UTwirl;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Spin, ExtCtrls, BGRABitmap, LazPaintType, LCScaleDPI,
  UFilterConnector, BGRABitmapTypes, UScripting;

type

  { TFTwirl }

  TFTwirl = class(TForm)
    Button_Cancel: TButton;
    Button_OK: TButton;
    CheckBox_Preview: TCheckBox;
    Label_Radius: TLabel;
    Label_Angle: TLabel;
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    Panel2: TPanel;
    SpinEdit_Angle: TSpinEdit;
    SpinEdit_Radius: TSpinEdit;
    Timer1: TTimer;
    procedure Button_OKClick(Sender: TObject);
    procedure CheckBox_PreviewChange(Sender: TObject);
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
    FComputedImage: TBGRABitmap;
    procedure InitParams;
    procedure PreviewNeeded;
    procedure DisplayComputedImage;
    function ComputeFilteredLayer: TBGRABitmap;
  public
    FilterConnector: TFilterConnector;
  end;

function ShowTwirlDlg(AFilterConnector: TObject): TScriptResult;

implementation

uses umac, UResourceStrings;

function ShowTwirlDlg(AFilterConnector: TObject): TScriptResult;
var
  FTwirl: TFTwirl;
begin
  FTwirl:= TFTwirl.create(nil);
  FTwirl.FilterConnector := AFilterConnector as TFilterConnector;
  try
    if FTwirl.FilterConnector.ActiveLayer <> nil then
    begin
      if Assigned(FTwirl.FilterConnector.Parameters) and
        FTwirl.FilterConnector.Parameters.Booleans['Validate'] then
      begin
        FTwirl.InitParams;
        FTwirl.FilterConnector.PutImage(FTwirl.ComputeFilteredLayer,False,true);
        FTwirl.FilterConnector.ValidateAction;
        result := srOk;
      end else
      begin
        if FTwirl.showModal = mrOk then result := srOk
        else result := srCancelledByUser;
      end;
    end
    else
      result := srException;
  finally
    FTwirl.free;
  end;
end;

{ TFTwirl }

procedure TFTwirl.FormCreate(Sender: TObject);
begin
  ScaleControl(Self,OriginalDPI);

  CheckSpinEdit(SpinEdit_Radius);
  CheckSpinEdit(SpinEdit_Angle);
  CheckOKCancelBtns(Button_OK{,Button_Cancel});
end;

procedure TFTwirl.FormDestroy(Sender: TObject);
begin
  if FComputedImage <> nil then FreeAndNil(FComputedImage);
end;

procedure TFTwirl.FormShow(Sender: TObject);
begin
  InitParams;
  PreviewNeeded;
  Left := FilterConnector.LazPaintInstance.MainFormBounds.Left;
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
  if FComputedImage <> nil then FComputedImage.Free;
  FComputedImage := ComputeFilteredLayer;
  if CheckBox_Preview.Checked then DisplayComputedImage;
  Button_OK.Enabled := true;
  CheckBox_Preview.Enabled := true;
end;

procedure TFTwirl.InitParams;
begin
  FInitializing:= true;
  SpinEdit_Radius.Value := round(FilterConnector.LazPaintInstance.Config.DefaultTwirlRadius);
  SpinEdit_Angle.Value := round(FilterConnector.LazPaintInstance.Config.DefaultTwirlTurn*360);
  FCenter := PointF(0.5,0.5);
  if Assigned(FilterConnector.Parameters) then
  with FilterConnector.Parameters do
  begin
    if IsDefined('Radius') then SpinEdit_Radius.Value := Floats['Radius'];
    if IsDefined('Angle') then SpinEdit_Angle.Value:= Floats['Angle'];
    if IsDefined('CenterPosPercent') then FCenter := Points2D['CenterPosPercent']*(1/100);
    if IsDefined('CenterXPercent') then FCenter.X := Floats['CenterXPercent']/100;
    if IsDefined('CenterYPercent') then FCenter.Y := Floats['CenterYPercent']/100;
  end;

  CheckBox_Preview.Checked := True;
  CheckBox_Preview.Caption := rsPreview;
  Button_OK.Caption := rsOk;
  Button_Cancel.Caption := rsCancel;
  FInitializing := false;
end;

procedure TFTwirl.PreviewNeeded;
begin
  Timer1.Enabled := false;
  Timer1.Enabled := True;
  Button_OK.Enabled := false;
  CheckBox_Preview.Enabled := false;
end;

procedure TFTwirl.DisplayComputedImage;
begin
  FilterConnector.PutImage(FComputedImage,False,False);
end;

function TFTwirl.ComputeFilteredLayer: TBGRABitmap;
begin
  result := FilterConnector.BackupLayer.FilterTwirl(FilterConnector.WorkArea, Point(round(FCenter.X*FilterConnector.ActiveLayer.Width),round(FCenter.Y*FilterConnector.ActiveLayer.Height)),
      SpinEdit_Radius.Value,SpinEdit_Angle.Value/360) as TBGRABitmap;
end;

procedure TFTwirl.Button_OKClick(Sender: TObject);
begin
  if not CheckBox_Preview.Checked then DisplayComputedImage;

  FilterConnector.ValidateAction;
  FilterConnector.LazPaintInstance.Config.SetDefaultTwirlRadius(SpinEdit_Radius.Value);
  FilterConnector.LazPaintInstance.Config.SetDefaultTwirlTurn(SpinEdit_Angle.Value/360);
  ModalResult := mrOK;
end;

procedure TFTwirl.CheckBox_PreviewChange(Sender: TObject);
begin
  if FInitializing then exit;
  if CheckBox_Preview.Checked then
    DisplayComputedImage
  else
    FilterConnector.RestoreBackup;
end;

{$R *.lfm}

end.

