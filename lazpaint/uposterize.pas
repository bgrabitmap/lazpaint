unit uposterize;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Spin, UFilterConnector, UScripting, LazPaintType;

type

  { TFPosterize }

  TFPosterize = class(TForm)
    Button_Cancel: TButton;
    Button_OK: TButton;
    CheckBox_ByLightness: TCheckBox;
    Label_Levels: TLabel;
    SpinEdit_Levels: TSpinEdit;
    procedure Button_OKClick(Sender: TObject);
    procedure CheckBox_ByLightnessChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpinEdit_LevelsChange(Sender: TObject);
  private
    FInitializing: boolean;
    FFilterConnector: TFilterConnector;
    procedure OnTryStopAction({%H-}sender: TFilterConnector);
    procedure PreviewNeeded;
    { private declarations }
  public
    { public declarations }
  end;

function ShowPosterizeDlg(AInstance: TLazPaintCustomInstance; AParameters: TVariableSet):boolean;

implementation

uses UScaleDPI, UMac, UColorFilters;

function ShowPosterizeDlg(AInstance: TLazPaintCustomInstance; AParameters: TVariableSet): boolean;
var FPosterize: TFPosterize;
  topmostInfo: TTopMostInfo;
begin
  FPosterize := TFPosterize.Create(nil);
  try
    FPosterize.FFilterConnector := TFilterConnector.Create(AInstance, AParameters);
    FPosterize.FFilterConnector.OnTryStopAction := @FPosterize.OnTryStopAction;
  except
    on ex: exception do
    begin
      AInstance.ShowError('ShowPosterizeDlg',ex.Message);
      result := false;
      exit;
    end;
  end;
  topmostInfo := AInstance.HideTopmost;
  try
    result := FPosterize.ShowModal = mrOK;
  finally
    AInstance.ShowTopmost(topmostInfo);
    FPosterize.FFilterConnector.OnTryStopAction := nil;
    FreeAndNil(FPosterize.FFilterConnector);
    FPosterize.Free;
  end;
end;

{ TFPosterize }

procedure TFPosterize.FormCreate(Sender: TObject);
begin
  ScaleDPI(Self,OriginalDPI);

  CheckOKCancelBtns(Button_OK,Button_Cancel);
  CheckSpinEdit(SpinEdit_Levels);
end;

procedure TFPosterize.Button_OKClick(Sender: TObject);
begin
  FFilterConnector.ValidateAction;
  FFilterConnector.LazPaintInstance.Config.SetDefaultPosterizeLevels(SpinEdit_Levels.Value);
  FFilterConnector.LazPaintInstance.Config.SetDefaultPosterizeByLightness(CheckBox_ByLightness.Checked);
  ModalResult := mrOK;
end;

procedure TFPosterize.CheckBox_ByLightnessChange(Sender: TObject);
begin
  if not FInitializing then PreviewNeeded;
end;

procedure TFPosterize.FormShow(Sender: TObject);
begin
  FInitializing := true;
  SpinEdit_Levels.Value := FFilterConnector.LazPaintInstance.Config.DefaultPosterizeLevels;
  CheckBox_ByLightness.Checked := FFilterConnector.LazPaintInstance.Config.DefaultPosterizeByLightness;
  FInitializing := false;
  PreviewNeeded;
  Top := FFilterConnector.LazPaintInstance.MainFormBounds.Top;
end;

procedure TFPosterize.SpinEdit_LevelsChange(Sender: TObject);
begin
  if not FInitializing then PreviewNeeded;
end;

procedure TFPosterize.OnTryStopAction(sender: TFilterConnector);
begin
  if self.visible then Close;
end;

procedure TFPosterize.PreviewNeeded;
var params:TVariableSet;
  levels: integer;

  procedure AddPosterize(AChannel :string);
  var
    XList,YList: TScriptVariableReference;
    i: integer;
  begin
    with params.AddSubset(AChannel) do
    begin
      Booleans['Posterize'] := true;
      XList := AddFloatList('X');
      YList := AddFloatList('Y');
      for i := 0 to levels-1 do
      begin
        AppendFloat(XList, i/levels);
        AppendFloat(YList, i/(levels-1));
      end;
    end;
  end;

begin
  levels := SpinEdit_Levels.Value;
  params := TVariableSet.Create('');
  if CheckBox_ByLightness.Checked then
    AddPosterize('Lightness') else
  begin
    AddPosterize('Red');
    AddPosterize('Green');
    AddPosterize('Blue');
  end;
  FilterAdjustCurves(FFilterConnector, params);
  params.Free;
end;

initialization
  {$I uposterize.lrs}

end.

