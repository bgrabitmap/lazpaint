unit UShiftColors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, Spin, BGRABitmap, LazPaintType, uscaledpi,
  ufilterconnector;

type

  { TFShiftColors }

  TFShiftColors = class(TForm)
    Button_Cancel: TButton;
    Button_OK: TButton;
    CheckBox_GSBA: TCheckBox;
    FloatSpinEdit_Hue: TFloatSpinEdit;
    FloatSpinEdit_Saturation: TFloatSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    TrackBar_Hue: TTrackBar;
    TrackBar_Saturation: TTrackBar;
    procedure Button_OKClick(Sender: TObject);
    procedure CheckBox_GSBAChange(Sender: TObject);
    procedure FloatSpinEdit_HueChange(Sender: TObject);
    procedure FloatSpinEdit_SaturationChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TrackBar_Change(Sender: TObject);
  private
    { private declarations }
    FInitialized: boolean;
    FFilterConnector: TFilterConnector;
    FUpdatingSpinEdit: boolean;
    procedure UpdateSpinEdit;
    function GetChosenHueShift: integer;
    procedure OnTryStopAction({%H-}sender: TFilterConnector);
    procedure SetChosenHueShift(AValue: integer);
  public
    { public declarations }
    function ShowModal: integer; override;
    function ShowModal(AInstance: TLazPaintCustomInstance): integer;
    procedure ApplyChosenShift;
    procedure ApplyShift(hueShift, satMul: integer; useGSBA: boolean);
    property ChosenHueShift: integer read GetChosenHueShift write SetChosenHueShift;
  end;

implementation

uses umac, BGRABitmapTypes, math, uresourcestrings;

{ TFShiftColors }

procedure TFShiftColors.FormCreate(Sender: TObject);
begin
  ScaleDPI(Self,OriginalDPI);

  CheckFloatSpinEdit(FloatSpinEdit_Saturation);
  CheckFloatSpinEdit(FloatSpinEdit_Hue);
  CheckOKCancelBtns(Button_OK,Button_Cancel);
  TrackBar_Hue.Min := 0;
  TrackBar_Hue.Max := TrackBar_Hue.Width and not 3;
  TrackBar_Hue.Position := TrackBar_Hue.Max div 2;
  TrackBar_Hue.Frequency := TrackBar_Hue.Max div 4;
  TrackBar_Saturation.Max := TrackBar_Saturation.Width and not 3;
  TrackBar_Saturation.Position := TrackBar_Saturation.Max div 2;
  TrackBar_Saturation.Frequency := TrackBar_Saturation.Max div 4;
  FInitialized := true;
  UpdateSpinEdit;
end;

procedure TFShiftColors.FormShow(Sender: TObject);
begin
  ApplyChosenShift;
end;

procedure TFShiftColors.TrackBar_Change(Sender: TObject);
begin
  if FInitialized then
  begin
    UpdateSpinEdit;
    ApplyChosenShift;
  end;
end;

procedure TFShiftColors.UpdateSpinEdit;
begin
  if FUpdatingSpinEdit then exit;
  FUpdatingSpinEdit:= true;
  FloatSpinEdit_Hue.Value := round(ChosenHueShift/65536*3600)/10;
  FloatSpinEdit_Saturation.Value := round((TrackBar_Saturation.Position/TrackBar_Saturation.Max*4-2)*1000)/1000;
  FloatSpinEdit_Hue.Update;
  FloatSpinEdit_Saturation.Update;
  FUpdatingSpinEdit:= false;
end;

procedure TFShiftColors.OnTryStopAction(sender: TFilterConnector);
begin
  if self.visible then Close;
end;

function TFShiftColors.GetChosenHueShift: integer;
begin
  result := round((TrackBar_Hue.Position/TrackBar_Hue.Max-0.5)*65536);
end;

procedure TFShiftColors.SetChosenHueShift(AValue: integer);
begin
  TrackBar_Hue.Position := round((AValue/65536+0.5)*TrackBar_Hue.Max);
end;

function TFShiftColors.ShowModal: integer;
begin
  if (FFilterConnector = nil) or (FFilterConnector.ActiveLayer = nil) then
  begin
    ShowMessage(rsNoActiveLayer);
    result := mrAbort
  end
  else
    Result:=inherited ShowModal;
end;

function TFShiftColors.ShowModal(AInstance: TLazPaintCustomInstance): integer;
begin
  try
    FFilterConnector := TFilterConnector.Create(AInstance);
    FFilterConnector.OnTryStopAction := @OnTryStopAction;
  except
    on ex: exception do
    begin
      MessageDlg(ex.Message,mtInformation,[mbOk],0);
      result := mrAbort;
      exit;
    end;
  end;
  try
    self.CheckBox_GSBA.Checked := AInstance.Config.DefaultUseGSBA;
    result := self.ShowModal;
    if result = mrOK then
      AInstance.Config.SetDefaultUseGSBA(self.CheckBox_GSBA.Checked);
  finally
    FFilterConnector.Free;
  end;
end;

procedure TFShiftColors.Button_OKClick(Sender: TObject);
begin
  ApplyChosenShift;
  FFilterConnector.ValidateAction;
end;

procedure TFShiftColors.CheckBox_GSBAChange(Sender: TObject);
begin
  if FInitialized and Visible then ApplyChosenShift;
end;

procedure TFShiftColors.FloatSpinEdit_HueChange(Sender: TObject);
begin
  FUpdatingSpinEdit := true;
  ChosenHueShift:= round(FloatSpinEdit_Hue.Value*65536/360);
  FUpdatingSpinEdit := false;
end;

procedure TFShiftColors.FloatSpinEdit_SaturationChange(Sender: TObject);
begin
  FUpdatingSpinEdit := true;
  TrackBar_Saturation.Position := round((FloatSpinEdit_Saturation.Value+2)/4*TrackBar_Saturation.Max);
  FUpdatingSpinEdit := false;
end;

procedure TFShiftColors.ApplyShift(hueShift, satMul: integer; useGSBA: boolean);
var n: integer;
    psrc,pdest: PBGRAPixel;
    hsl: THSLAPixel;

    pselect: PBGRAPixel;
    selection: TBGRABitmap;
    alpha: byte;
begin
    psrc := FFilterConnector.BackupLayer.Data;
    pdest := FFilterConnector.ActiveLayer.Data;

    selection := FFilterConnector.CurrentSelection;
    if selection = nil then
    begin
      alpha := 255;
      pselect := nil;
    end else pselect := selection.Data;

    if not useGSBA then
    begin
      for n := 0 to FFilterConnector.ActiveLayer.NbPixels-1 do
      begin
        if pselect <> nil then
        begin
          alpha := pselect^.green;
          inc(pselect);
        end;
        if alpha <> 0 then
        begin
          hsl := BGRAToHSLA(psrc^);
          hsl.hue := hsl.hue + hueShift;
          hsl.saturation := min(65535,max(0,hsl.saturation*satMul shr 12));
          pdest^ := HSLAToBGRA(hsl);
          if alpha <> 255 then
            pdest^ := MergeBGRAWithGammaCorrection(pdest^,alpha,psrc^,not alpha);
        end;
        inc(psrc);
        inc(pdest);
      end;
    end else
    begin
      for n := 0 to FFilterConnector.ActiveLayer.NbPixels-1 do
      begin
        if pselect <> nil then
        begin
          alpha := pselect^.green;
          inc(pselect);
        end;
        if alpha <> 0 then
        begin
          hsl := BGRAToGSBA(psrc^);
          hsl.hue := hsl.hue + hueShift;
          hsl.saturation := min(65535,max(0,hsl.saturation*satMul shr 12));
          pdest^ := GSBAToBGRA(hsl);
          if alpha <> 255 then
            pdest^ := MergeBGRAWithGammaCorrection(pdest^,alpha,psrc^,not alpha);
        end;
        inc(psrc);
        inc(pdest);
      end;
    end;
    FFilterConnector.InvalidateActiveLayer;
end;

procedure TFShiftColors.ApplyChosenShift;
begin
  ApplyShift(ChosenHueShift,round(exp((TrackBar_Saturation.Position/TrackBar_Saturation.Max-0.5)*4)*4096),CheckBox_GSBA.Checked);
end;

initialization
  {$I ushiftcolors.lrs}

end.

