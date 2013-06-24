unit UColorintensity;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, Spin, BGRABitmap, LazPaintType, uscaledpi,
  uresourcestrings, ufilterconnector;

type
  TColorIntensityMode = (ciIntensity, ciLightness);

  { TFColorIntensity }

  TFColorIntensity = class(TForm)
    Button_Cancel: TButton;
    Button_OK: TButton;
    FloatSpinEdit_Shift: TFloatSpinEdit;
    FloatSpinEdit_Factor: TFloatSpinEdit;
    Label_Multiply: TLabel;
    Label_Shift: TLabel;
    TrackBar_Multiply: TTrackBar;
    TrackBar_Shift: TTrackBar;
    procedure Button_OKClick(Sender: TObject);
    procedure FloatSpinEdit_FactorChange(Sender: TObject);
    procedure FloatSpinEdit_ShiftChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TrackBar_Change(Sender: TObject);
  private
    { private declarations }
    FInitialized: boolean;
    FFilterConnector: TFilterConnector;
    FMode: TColorIntensityMode;
    FUpdatingSpinEdit: boolean;
    procedure UpdateSpinEdit;
    function GetChosenFactor: single;
    function GetChosenShift: single;
    procedure OnTryStopAction({%H-}sender: TFilterConnector);
    procedure SetChosenFactor(AValue: single);
    procedure SetChosenShift(AValue: single);
    procedure AdjustLabels(ALabel1,ALabel2: TLabel; ATrack1,ATrack2:TTrackBar);
  public
    { public declarations }
    function ShowModal: integer; override;
    function ShowModal(AInstance: TLazPaintCustomInstance; AMode: TColorIntensityMode): integer;
    procedure ApplyIntensity(multiply, shift: single);
    procedure ApplyChosenIntensity;
    property ChosenFactor: single read GetChosenFactor write SetChosenFactor;
    property ChosenShift: single read GetChosenShift write SetChosenShift;

  end;

implementation

uses umac, BGRABitmapTypes;

{ TFColorIntensity }

procedure TFColorIntensity.FormCreate(Sender: TObject);
begin
  ScaleDPI(Self,OriginalDPI);

  CheckFloatSpinEdit(FloatSpinEdit_Factor);
  CheckFloatSpinEdit(FloatSpinEdit_Shift);
  CheckOKCancelBtns(Button_OK,Button_Cancel);
  TrackBar_Multiply.Min := 0;
  TrackBar_Multiply.Max := TrackBar_Multiply.Width and not 3;
  TrackBar_Multiply.Position := TrackBar_Multiply.Max div 2;
  TrackBar_Multiply.Frequency := TrackBar_Multiply.Max div 4;
  TrackBar_Shift.Max := TrackBar_Shift.Width and not 3;
  TrackBar_Shift.Position := TrackBar_Shift.Max div 2;
  TrackBar_Shift.Frequency := TrackBar_Shift.Max div 4;
  FMode := ciIntensity;
  FInitialized := true;
  UpdateSpinEdit;
end;

procedure TFColorIntensity.Button_OKClick(Sender: TObject);
begin
  ApplyChosenIntensity;
  FFilterConnector.ValidateAction;
end;

procedure TFColorIntensity.FloatSpinEdit_FactorChange(Sender: TObject);
begin
  FUpdatingSpinEdit := true;
  ChosenFactor := FloatSpinEdit_Factor.Value;
  FUpdatingSpinEdit := false;
end;

procedure TFColorIntensity.FloatSpinEdit_ShiftChange(Sender: TObject);
begin
  FUpdatingSpinEdit := true;
  ChosenShift := FloatSpinEdit_Shift.Value;
  FUpdatingSpinEdit := false;
end;

procedure TFColorIntensity.FormShow(Sender: TObject);
begin
  AdjustLabels(Label_Multiply,Label_Shift, TrackBar_Multiply,TrackBar_Shift);
  if FMode = ciIntensity then Caption := rsIntensity else
    Caption := rsLightness;
  ApplyChosenIntensity;
end;

procedure TFColorIntensity.TrackBar_Change(Sender: TObject);
begin
  if FInitialized then
  begin
    UpdateSpinEdit;
    ApplyChosenIntensity;
  end;
end;

procedure TFColorIntensity.UpdateSpinEdit;
begin
  if FUpdatingSpinEdit then exit;
  FUpdatingSpinEdit:= true;
  FloatSpinEdit_Shift.Value := round(ChosenShift*1000)/1000;
  FloatSpinEdit_Factor.Value := round(ChosenFactor*1000)/1000;
  FloatSpinEdit_Shift.Update;
  FloatSpinEdit_Factor.Update;
  FUpdatingSpinEdit:= false;
end;

procedure TFColorIntensity.OnTryStopAction(sender: TFilterConnector);
begin
  if self.visible then Close;
end;

function TFColorIntensity.GetChosenFactor: single;
begin
  result := (TrackBar_Multiply.Position/TrackBar_Multiply.Max-0.5)*4;
end;

function TFColorIntensity.GetChosenShift: single;
begin
  result := (TrackBar_Shift.Position/TrackBar_Shift.Max-0.5)*2;
end;

procedure TFColorIntensity.SetChosenFactor(AValue: single);
begin
  TrackBar_Multiply.Position := round((AValue/4+0.5)*TrackBar_Multiply.Max);
end;

procedure TFColorIntensity.SetChosenShift(AValue: single);
begin
  TrackBar_Shift.Position := round((AValue/2+0.5)*TrackBar_Shift.Max);
end;

procedure TFColorIntensity.AdjustLabels(ALabel1, ALabel2: TLabel; ATrack1,
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

function TFColorIntensity.ShowModal: integer;
begin
  if (FFilterConnector = nil) or (FFilterConnector.ActiveLayer = nil) then
  begin
    ShowMessage(rsNoActiveLayer);
    result := mrAbort
  end
  else
    Result:=inherited ShowModal;
end;

function TFColorIntensity.ShowModal(AInstance: TLazPaintCustomInstance;
  AMode: TColorIntensityMode): integer;
begin
  FMode := AMode;
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
    result := self.ShowModal;
  finally
    FFilterConnector.Free;
  end;
end;

procedure TFColorIntensity.ApplyIntensity(multiply, shift: single);
var n: integer;
    psrc,pdest: PBGRAPixel;
    ec: TExpandedPixel;
    newIntensity: single;

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

    if FMode = ciIntensity then
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
          ec := GammaExpansion(psrc^);
          newIntensity := (GetIntensity(ec)-32767.5)*multiply+shift+32767.5;
          if newIntensity < 0 then newIntensity := 0;
          if newIntensity > 65535 then newIntensity := 65535;
          pdest^ := GammaCompression(SetIntensity(ec,round(newIntensity)));
          if alpha <> 255 then
            pdest^ := MergeBGRAWithGammaCorrection(pdest^,alpha,psrc^,not alpha);
        end;
        inc(psrc);
        inc(pdest);
      end;
    end else //ciLightness
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
          ec := GammaExpansion(psrc^);
          newIntensity := (GetLightness(ec)-32767.5)*multiply+shift+32767.5;
          if newIntensity < 0 then newIntensity := 0;
          if newIntensity > 65535 then newIntensity := 65535;
          pdest^ := GammaCompression(SetLightness(ec,round(newIntensity)));
          if alpha <> 255 then
            pdest^ := MergeBGRAWithGammaCorrection(pdest^,alpha,psrc^,not alpha);
        end;
        inc(psrc);
        inc(pdest);
      end;
    end;
    FFilterConnector.InvalidateActiveLayer;
end;

procedure TFColorIntensity.ApplyChosenIntensity;
begin
  ApplyIntensity(exp(ChosenFactor),ChosenShift*65535);
end;

initialization
  {$I ucolorintensity.lrs}

end.

