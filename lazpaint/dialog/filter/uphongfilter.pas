// SPDX-License-Identifier: GPL-3.0-only
unit UPhongFilter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Spin, UFilterConnector, BGRABitmapTypes, BGRABitmap,
  UScripting;

type

  { TFPhongFilter }

  TFPhongFilter = class(TForm)
    Button_Cancel: TButton;
    Button_OK: TButton;
    GroupBox_Color: TGroupBox;
    GroupBox_Color1: TGroupBox;
    Label_LightPosition: TLabel;
    Label_Altitude: TLabel;
    PaintBox1: TPaintBox;
    Radio_MapLinearLightness: TRadioButton;
    Radio_MapSaturation: TRadioButton;
    Radio_UseKeep: TRadioButton;
    Radio_UseBackColor: TRadioButton;
    Radio_MapLightness: TRadioButton;
    Radio_UsePenColor: TRadioButton;
    Radio_MapAlpha: TRadioButton;
    Radio_UseTexture: TRadioButton;
    Radio_MapRed: TRadioButton;
    Radio_MapGreen: TRadioButton;
    Radio_MapBlue: TRadioButton;
    SpinEdit_Altitude: TSpinEdit;
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
    procedure Radio_MapChange(Sender: TObject);
    procedure Radio_UseChange(Sender: TObject);
    procedure SpinEdit_AltitudeChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    FInitializing: boolean;
    FCenter: TPointF;
    FHeightMap: TBGRABitmap;
    FWorkspaceColor: TColor;
    FTexture: TBGRACustomBitmap;
    function GetCurrentLightPos: TPointF;
    procedure InitParams;
    procedure PreviewNeeded;
    function ComputeFilteredLayer: TBGRABitmap;
  public
    FilterConnector: TFilterConnector;
    property CurrentLightPos: TPointF read GetCurrentLightPos;
  end;

function ShowPhongFilterDlg(AFilterConnector: TObject): TScriptResult;

implementation

uses LCScaleDPI, UMac, BGRAGradients, LazPaintType;

function ShowPhongFilterDlg(AFilterConnector: TObject): TScriptResult;
var
  FPhongFilter: TFPhongFilter;
begin
  FPhongFilter:= TFPhongFilter.create(nil);
  FPhongFilter.FilterConnector := AFilterConnector as TFilterConnector;
  FPhongFilter.FWorkspaceColor:= FPhongFilter.FilterConnector.LazPaintInstance.Config.GetWorkspaceColor;
  try
    if FPhongFilter.FilterConnector.ActiveLayer <> nil then
    begin
      if Assigned(FPhongFilter.FilterConnector.Parameters) and
        FPhongFilter.FilterConnector.Parameters.Booleans['Validate'] then
      begin
        FPhongFilter.InitParams;
        FPhongFilter.PreviewNeeded;
        FPhongFilter.FilterConnector.PutImage(FPhongFilter.ComputeFilteredLayer,true,true);
        FPhongFilter.FilterConnector.ValidateAction;
        result := srOk;
      end else
      begin
        if FPhongFilter.showModal = mrOk then result := srOk
        else result := srCancelledByUser;
      end;
    end
    else
      result := srException;
  finally
    FPhongFilter.free;
  end;
end;

{ TFPhongFilter }

procedure TFPhongFilter.Button_OKClick(Sender: TObject);
begin
  FilterConnector.ValidateAction;
  FilterConnector.LazPaintInstance.Config.SetDefaultPhongFilterAltitude(SpinEdit_Altitude.Value);
  FilterConnector.LazPaintInstance.ToolManager.LightPosition := CurrentLightPos;
  ModalResult := mrOK;
end;

procedure TFPhongFilter.FormCreate(Sender: TObject);
begin
  ScaleControl(Self,OriginalDPI);
  CheckOKCancelBtns(Button_OK,Button_Cancel);
  FCenter := PointF(0.5,0.5);
  FWorkspaceColor:= clAppWorkspace;
end;

procedure TFPhongFilter.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FHeightMap);
  if Assigned(FTexture) then FTexture.Free;
end;


procedure TFPhongFilter.FormShow(Sender: TObject);
begin
  InitParams;
  PreviewNeeded;
end;

procedure TFPhongFilter.PaintBox1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FCenter := PointF(X/PaintBox1.Width*2-0.5,Y/PaintBox1.Height*2-0.5);
  PaintBox1.Invalidate;
  PreviewNeeded;
end;

procedure TFPhongFilter.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if ssLeft in Shift then
  begin
    FCenter := PointF(X/PaintBox1.Width*2-0.5,Y/PaintBox1.Height*2-0.5);
    PaintBox1.Invalidate;
    PreviewNeeded;
  end;
end;

procedure TFPhongFilter.PaintBox1Paint(Sender: TObject);
var x,y: integer;
begin
  x := round((FCenter.X+0.5)*PaintBox1.Width/2);
  y := round((FCenter.Y+0.5)*PaintBox1.Height/2);
  PaintBox1.Canvas.Brush.Style := bsSolid;
  PaintBox1.Canvas.Brush.Color := FWorkspaceColor;
  PaintBox1.Canvas.Pen.Style := psSolid;
  PaintBox1.Canvas.Pen.Color := MergeBGRA(ColorToBGRA(clBlack),ColorToBGRA(FWorkspaceColor));
  PaintBox1.Canvas.Rectangle(0,0,PaintBox1.Width,PaintBox1.Height);
  PaintBox1.Canvas.Pen.Style := psDot;
  PaintBox1.Canvas.Pen.Color := clBlack;
  PaintBox1.Canvas.Brush.Style := bsSolid;
  PaintBox1.Canvas.Brush.Color := clWhite;
  PaintBox1.Canvas.Rectangle(PaintBox1.Width div 4,PaintBox1.Height div 4,PaintBox1.Width*3 div 4,PaintBox1.Height*3 div 4);
  PaintBox1.Canvas.Pen.Style := psSolid;
  PaintBox1.Canvas.Pen.Color := clBlack;
  PaintBox1.Canvas.Brush.Style := bsSolid;
  PaintBox1.Canvas.Brush.Color := clWhite;
  PaintBox1.Canvas.Ellipse(x-3,y-3,x+4,y+4);
end;

procedure TFPhongFilter.Radio_MapChange(Sender: TObject);
begin
  FreeAndNil(FHeightMap);
  if not FInitializing then PreviewNeeded;
end;

procedure TFPhongFilter.Radio_UseChange(Sender: TObject);
begin
  if not FInitializing then PreviewNeeded;
end;

procedure TFPhongFilter.SpinEdit_AltitudeChange(Sender: TObject);
begin
  if SpinEdit_Altitude.Value < 6 then
    SpinEdit_Altitude.Increment := 1
  else if SpinEdit_Altitude.Value < 25 then
    SpinEdit_Altitude.Increment := 3
  else
    SpinEdit_Altitude.Increment := 5;
  if not FInitializing then PreviewNeeded;
end;

procedure TFPhongFilter.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := false;
  FilterConnector.PutImage(ComputeFilteredLayer,True,true);
  Button_OK.Enabled := true;
end;

procedure TFPhongFilter.PreviewNeeded;
begin
  Timer1.Enabled := false;
  Timer1.Enabled := True;
  Button_OK.Enabled := false;
end;

function TFPhongFilter.GetCurrentLightPos: TPointF;
begin
  result := PointF(FCenter.X*FilterConnector.ActiveLayer.Width,
    FCenter.Y*FilterConnector.ActiveLayer.Height);
end;

procedure TFPhongFilter.InitParams;
var
  texOpacity: Byte;
begin
  FInitializing:= true;
  Radio_UseTexture.Enabled := (FilterConnector.LazPaintInstance.ToolManager.BackFill.Texture <> nil);
  if FTexture <> nil then
  begin
    FTexture.FreeReference;
    FTexture := nil;
  end;
  if Radio_UseTexture.Enabled then
  begin
    Radio_UseTexture.Checked := true;
    texOpacity := FilterConnector.LazPaintInstance.ToolManager.BackFill.TextureOpacity;
    if texOpacity <> 255 then
    begin
      FTexture := FilterConnector.LazPaintInstance.ToolManager.BackFill.Texture.Duplicate;
      FTexture.ApplyGlobalOpacity(texOpacity);
    end else
      FTexture := FilterConnector.LazPaintInstance.ToolManager.BackFill.Texture.NewReference;
  end
  else Radio_UsePenColor.Checked := true;
  SpinEdit_Altitude.Value := FilterConnector.LazPaintInstance.Config.DefaultPhongFilterAltitude;
  with FilterConnector.LazPaintInstance.ToolManager.LightPosition do
    FCenter := PointF(X/FilterConnector.LazPaintInstance.Image.Width,
        Y/FilterConnector.LazPaintInstance.Image.Height);
  if Assigned(FilterConnector.Parameters) then
  with FilterConnector.Parameters do
  begin
    if IsDefined('ColorSource') then
    case Strings['ColorSource'] of
    'Pen': Radio_UsePenColor.checked := true;
    'Back': Radio_UseBackColor.checked := true;
    'Layer': Radio_UseKeep.checked := true;
    end;
    if IsDefined('AltitudePercent') then
      SpinEdit_Altitude.Value := Integers['AltitudePercent'];
    if IsDefined('LightPosPercent') then
      FCenter := Points2D['LightPosPercent']*(1/100);
    if IsDefined('LightXPercent') then
      FCenter.x := Floats['LightXPercent']/100;
    if IsDefined('LightYPercent') then
      FCenter.y := Floats['LightYPercent']/100;
    if IsDefined('AltitudeSource') then
    case Strings['AltitudeSource'] of
    'Lightness': Radio_MapLightness.Checked:= true;
    'LinearLightness': Radio_MapLinearLightness.Checked:= true;
    'Saturation': Radio_MapSaturation.Checked:= true;
    'Alpha': Radio_MapAlpha.Checked:= true;
    'Red': Radio_MapRed.Checked:= true;
    'Green': Radio_MapGreen.Checked:= true;
    'Blue': Radio_MapBlue.Checked:= true;
    end;
  end;
  SpinEdit_AltitudeChange(nil);
  FInitializing := false;
end;

procedure ScanLineMapLightness(psrc,pdest: PBGRAPixel; count: integer);
const oneOver65535 = 1/65535;
begin
  while count > 0 do
  begin
    pdest^ := MapHeightToBGRA(GetLightness(GammaExpansion(psrc^))*oneOver65535,psrc^.alpha);
    inc(pdest);
    inc(psrc);
    dec(count);
  end;
end;

procedure ScanLineMapLinearLightness(psrc,pdest: PBGRAPixel; count: integer);
const oneOver255 = 1/255;
begin
  while count > 0 do
  begin
    pdest^ := MapHeightToBGRA((psrc^.red*0.299+psrc^.green*0.587+psrc^.blue*0.114)*oneOver255,psrc^.alpha);
    inc(pdest);
    inc(psrc);
    dec(count);
  end;
end;

procedure ScanLineMapAlpha(psrc,pdest: PBGRAPixel; count: integer);
begin
  while count > 0 do
  begin
    pdest^ := BGRA(psrc^.alpha,psrc^.alpha,psrc^.alpha,255);
    inc(pdest);
    inc(psrc);
    dec(count);
  end;
end;

procedure ScanLineMapBlue(psrc,pdest: PBGRAPixel; count: integer);
begin
  while count > 0 do
  begin
    pdest^ := BGRA(psrc^.blue,psrc^.blue,psrc^.blue,psrc^.alpha);
    inc(pdest);
    inc(psrc);
    dec(count);
  end;
end;

procedure ScanLineMapGreen(psrc,pdest: PBGRAPixel; count: integer);
begin
  while count > 0 do
  begin
    pdest^ := BGRA(psrc^.green,psrc^.green,psrc^.green,psrc^.alpha);
    inc(pdest);
    inc(psrc);
    dec(count);
  end;
end;

procedure ScanLineMapRed(psrc,pdest: PBGRAPixel; count: integer);
begin
  while count > 0 do
  begin
    pdest^ := BGRA(psrc^.red,psrc^.red,psrc^.red,psrc^.alpha);
    inc(pdest);
    inc(psrc);
    dec(count);
  end;
end;

procedure ScanLineMapSaturation(psrc,pdest: PBGRAPixel; count: integer);
const oneOver65535 = 1/65535;
begin
  while count > 0 do
  begin
    with BGRAToHSLA(psrc^) do
      pdest^ := MapHeightToBGRA(saturation*oneOver65535,psrc^.alpha);
    inc(pdest);
    inc(psrc);
    dec(count);
  end;
end;

function TFPhongFilter.ComputeFilteredLayer: TBGRABitmap;
var shader: TPhongShading;
  yb: integer;
  scanlineMapFunc: procedure(psrc,pdest: PBGRAPixel; count: integer);

begin
  result := TBGRABitmap.Create(FilterConnector.ActiveLayer.Width, FilterConnector.ActiveLayer.Height);
  shader := TPhongShading.Create;
  shader.AmbientFactor := 0.5;
  shader.NegativeDiffusionFactor := 0.15;
  shader.LightPositionF := CurrentLightPos;
  shader.LightPositionZ := FilterConnector.LazPaintInstance.ToolManager.LightAltitude;
  if FHeightMap = nil then
  begin
    if Radio_MapLightness.Checked then
      scanlineMapFunc := @ScanLineMapLightness
    else if Radio_MapLinearLightness.Checked then
      scanlineMapFunc := @ScanLineMapLinearLightness
    else if Radio_MapAlpha.Checked then
      scanlineMapFunc := @ScanLineMapAlpha
    else if Radio_MapBlue.Checked then
      scanlineMapFunc := @ScanLineMapBlue
    else if Radio_MapGreen.Checked then
      scanlineMapFunc := @ScanLineMapGreen
    else if Radio_MapRed.Checked then
      scanlineMapFunc := @ScanLineMapRed
    else if Radio_MapSaturation.Checked then
      scanlineMapFunc := @ScanLineMapSaturation
    else
      scanlineMapFunc := nil;

    if Assigned(scanlineMapFunc) then
    begin
      FHeightMap := TBGRABitmap.Create(FilterConnector.BackupLayer.Width,FilterConnector.BackupLayer.Height);
      for yb := FilterConnector.WorkArea.Top to FilterConnector.WorkArea.Bottom-1 do
        scanlineMapFunc(FilterConnector.BackupLayer.ScanLine[yb]+FilterConnector.WorkArea.Left,
          FHeightMap.ScanLine[yb]+FilterConnector.WorkArea.Left, FilterConnector.WorkArea.Right - FilterConnector.WorkArea.Left);
    end;
  end;
  if FHeightMap <> nil then
  begin
    if Radio_UseTexture.Checked then
      shader.DrawScan(result, FHeightMap, SpinEdit_Altitude.Value, 0, 0, FTexture)
    else if Radio_UsePenColor.Checked then
      shader.Draw(result, FHeightMap, SpinEdit_Altitude.Value,0,0,FilterConnector.LazPaintInstance.ToolManager.ForeColor)
    else if Radio_UseKeep.Checked then
      shader.Draw(result, FHeightMap, SpinEdit_Altitude.Value,0,0,FilterConnector.BackupLayer)
    else
      shader.Draw(result, FHeightMap, SpinEdit_Altitude.Value,0,0,FilterConnector.LazPaintInstance.ToolManager.BackColor);
  end;
  shader.Free;
end;

{$R *.lfm}

end.

