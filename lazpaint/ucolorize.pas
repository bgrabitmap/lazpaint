unit UColorize;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, Spin, BGRABitmap, uscaledpi, lazpainttype,
  ufilterconnector;

type

  { TFColorize }

  TFColorize = class(TForm)
    Button_Cancel: TButton;
    Button_OK: TButton;
    CheckBox_GSBA: TCheckBox;
    FloatSpinEdit_Hue: TFloatSpinEdit;
    FloatSpinEdit_Saturation: TFloatSpinEdit;
    Label_Hue: TLabel;
    Label_Colorness: TLabel;
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
    procedure AdjustLabels(ALabel1, ALabel2: TLabel; ATrack1, ATrack2: TTrackBar
      );
    function GetChosenHue: Word;
    function GetChosenSaturation: Word;
    procedure OnTryStopAction({%H-}sender: TFilterConnector);
    procedure SetChosenHue(AValue: Word);
    procedure SetChosenSaturation(AValue: Word);
    procedure UpdateSpinEdit;
  public
    { public declarations }
    function ShowModal: integer; override;
    function ShowModal(AInstance: TLazPaintCustomInstance): integer;
    procedure ApplyChosenColor;
    procedure ApplyColor(hue, sat: integer; useGSBA: boolean);
    property ChosenHue: Word read GetChosenHue write SetChosenHue;
    property ChosenSaturation: Word read GetChosenSaturation write SetChosenSaturation;
  end;

implementation

uses umac, BGRABitmapTypes, uresourcestrings;

{ TFColorize }

procedure TFColorize.FormCreate(Sender: TObject);
begin
  ScaleDPI(Self,OriginalDPI);

  CheckOKCancelBtns(Button_OK,Button_Cancel);
  CheckFloatSpinEdit(FloatSpinEdit_Hue);
  CheckFloatSpinEdit(FloatSpinEdit_Saturation);
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

procedure TFColorize.Button_OKClick(Sender: TObject);
begin
  ApplyChosenColor;
  FFilterConnector.ValidateAction;
end;

procedure TFColorize.CheckBox_GSBAChange(Sender: TObject);
begin
  if FInitialized and Visible then ApplyChosenColor;
end;

procedure TFColorize.FloatSpinEdit_HueChange(Sender: TObject);
begin
  FUpdatingSpinEdit := true;
  ChosenHue:= round(FloatSpinEdit_Hue.Value*65536/360) and 65535;
  FUpdatingSpinEdit := false;
end;

procedure TFColorize.FloatSpinEdit_SaturationChange(Sender: TObject);
begin
  FUpdatingSpinEdit := true;
  ChosenSaturation:= round(FloatSpinEdit_Saturation.Value*65535);
  FUpdatingSpinEdit := false;
end;

procedure TFColorize.AdjustLabels(ALabel1, ALabel2: TLabel; ATrack1,
  ATrack2: TTrackBar);
var
  LabelMaxRight: integer;
begin
  LabelMaxRight:= ALabel1.BoundsRect.Right;
  if ALabel2.BoundsRect.Right > LabelMaxRight then
    LabelMaxRight := ALabel2.BoundsRect.Right;
  if (LabelMaxRight > ATrack1.Left) or (LabelMaxRight > ATrack2.Left) then
  begin
    ATrack1.SetBounds(LabelMaxRight,ATrack1.Top,ATrack1.BoundsRect.Right-LabelMaxRight,ATrack1.Height);
    ATrack2.SetBounds(LabelMaxRight,ATrack2.Top,ATrack2.BoundsRect.Right-LabelMaxRight,ATrack2.Height);
  end;
end;

procedure TFColorize.FormShow(Sender: TObject);
begin
  AdjustLabels(Label_Hue,Label_Colorness, TrackBar_Hue,TrackBar_Saturation);
  ApplyChosenColor;
end;

procedure TFColorize.TrackBar_Change(Sender: TObject);
begin
  if FInitialized then
  begin
    UpdateSpinEdit;
    ApplyChosenColor;
  end;
end;

procedure TFColorize.OnTryStopAction(sender: TFilterConnector);
begin
  if self.visible then Close;
end;

function TFColorize.GetChosenHue: Word;
begin
  result := round(TrackBar_Hue.Position/TrackBar_Hue.Max*65535);
end;

function TFColorize.GetChosenSaturation: Word;
begin
  result := round(TrackBar_Saturation.Position/TrackBar_Saturation.Max*65535);
end;

procedure TFColorize.SetChosenHue(AValue: Word);
begin
  TrackBar_Hue.Position := round(AValue/65535*TrackBar_Hue.Max);
end;

procedure TFColorize.SetChosenSaturation(AValue: Word);
begin
  TrackBar_Saturation.Position := round(AValue/65535*TrackBar_Saturation.Max);
end;

procedure TFColorize.UpdateSpinEdit;
begin
  if FUpdatingSpinEdit then exit;
  FUpdatingSpinEdit:= true;
  FloatSpinEdit_Hue.Value := round(ChosenHue/65536*3600)/10;
  FloatSpinEdit_Saturation.Value := round(ChosenSaturation/65535*1000)/1000;
  FloatSpinEdit_Hue.Update;
  FloatSpinEdit_Saturation.Update;
  FUpdatingSpinEdit:= false;
end;

function TFColorize.ShowModal: integer;
begin
  if (FFilterConnector = nil) or (FFilterConnector.ActiveLayer = nil) then
  begin
    ShowMessage(rsNoActiveLayer);
    result := mrAbort
  end
  else
    Result:=inherited ShowModal;
end;

function TFColorize.ShowModal(AInstance: TLazPaintCustomInstance): integer;
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

procedure TFColorize.ApplyChosenColor;
begin
  ApplyColor(ChosenHue,ChosenSaturation,CheckBox_GSBA.Checked);
end;

procedure TFColorize.ApplyColor(hue, sat: integer; useGSBA: boolean);
var n: integer;
    psrc,pdest: PBGRAPixel;
    hsl: THSLAPixel;
    ec: TExpandedPixel;

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

    for n := 0 to FFilterConnector.ActiveLayer.NbPixels-1 do
    begin
      if pselect <> nil then
      begin
        alpha := pselect^.green;
        inc(pselect);
      end;
      if alpha <> 0 then
      begin
        ec := GammaExpansion(psrc^);
        hsl.alpha := ec.alpha;
        hsl.lightness := GetLightness(ec);
        hsl.hue := hue;
        hsl.saturation := sat;
        if useGSBA then
          pdest^ := GSBAToBGRA(hsl)
        else
          pdest^ := HSLAToBGRA(hsl);
        if alpha <> 255 then
          pdest^ := MergeBGRAWithGammaCorrection(pdest^,alpha,psrc^,not alpha);
      end else
        pdest^ := psrc^;
      inc(psrc);
      inc(pdest);
    end;
    FFilterConnector.InvalidateActiveLayer;
end;

initialization
  {$I ucolorize.lrs}

end.

