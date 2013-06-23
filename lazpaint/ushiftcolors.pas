unit ushiftcolors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, BGRABitmap, LazPaintType, uscaledpi;

type

  { TFShiftColors }

  TFShiftColors = class(TForm)
    Button_Cancel: TButton;
    Button_OK: TButton;
    CheckBox_GSBA: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    TrackBar_Hue: TTrackBar;
    TrackBar_Saturation: TTrackBar;
    procedure Button_OKClick(Sender: TObject);
    procedure CheckBox_GSBAChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TrackBar_Change(Sender: TObject);
  private
    { private declarations }
    initialized: boolean;
    backupImage: TBGRABitmap;
  public
    { public declarations }
    LazPaintInstance: TLazPaintCustomInstance;
    activeLayer: TBGRABitmap;
    procedure ApplyChosenShift;
    procedure ApplyDefinitely;
    procedure ApplyShift(hueShift, satMul: integer; useGSBA: boolean);
  end;

implementation

uses umac, BGRABitmapTypes, math;

{ TFShiftColors }

procedure TFShiftColors.FormCreate(Sender: TObject);
begin
  ScaleDPI(Self,OriginalDPI);

  CheckOKCancelBtns(Button_OK,Button_Cancel);
  TrackBar_Hue.Min := 0;
  TrackBar_Hue.Max := TrackBar_Hue.Width and not 3;
  TrackBar_Hue.Position := TrackBar_Hue.Max div 2;
  TrackBar_Hue.Frequency := TrackBar_Hue.Max div 4;
  TrackBar_Saturation.Max := TrackBar_Saturation.Width and not 3;
  TrackBar_Saturation.Position := TrackBar_Saturation.Max div 2;
  TrackBar_Saturation.Frequency := TrackBar_Saturation.Max div 4;
  initialized := true;
end;

procedure TFShiftColors.FormHide(Sender: TObject);
begin
  if backupImage <> nil then
  begin
    activeLayer.PutImage(0,0,backupImage,dmSet);
    LazPaintInstance.NotifyImageChangeCompletely(True);
    FreeAndNil(backupImage);
  end;
end;

procedure TFShiftColors.FormShow(Sender: TObject);
begin
  backupImage := activeLayer.Duplicate as TBGRABitmap;
  ApplyChosenShift;
end;

procedure TFShiftColors.TrackBar_Change(Sender: TObject);
begin
  if initialized then
    ApplyChosenShift;
end;

procedure TFShiftColors.Button_OKClick(Sender: TObject);
begin
  ApplyDefinitely;
  Close;
end;

procedure TFShiftColors.CheckBox_GSBAChange(Sender: TObject);
begin
  if initialized then
    ApplyChosenShift;
end;

procedure TFShiftColors.ApplyShift(hueShift, satMul: integer; useGSBA: boolean);
var n: integer;
    psrc,pdest: PBGRAPixel;
    hsl: THSLAPixel;
begin
    psrc := backupImage.Data;
    pdest := activeLayer.Data;

    if not useGSBA then
    begin
      for n := 0 to activeLayer.NbPixels-1 do
      begin
        hsl := BGRAToHSLA(psrc^);
        hsl.hue := hsl.hue + hueShift;
        hsl.saturation := min(65535,max(0,hsl.saturation*satMul shr 12));
        pdest^ := HSLAToBGRA(hsl);
        inc(psrc);
        inc(pdest);
      end;
    end else
    begin
      for n := 0 to activeLayer.NbPixels-1 do
      begin
        hsl := BGRAToGSBA(psrc^);
        hsl.hue := hsl.hue + hueShift;
        hsl.saturation := min(65535,max(0,hsl.saturation*satMul shr 12));
        pdest^ := GSBAToBGRA(hsl);
        inc(psrc);
        inc(pdest);
      end;
    end;
    activeLayer.InvalidateBitmap;
    LazPaintInstance.NotifyImageChangeCompletely(True);
end;

procedure TFShiftColors.ApplyChosenShift;
begin
  ApplyShift(round((TrackBar_Hue.Position/TrackBar_Hue.Max-0.5)*65536),round(exp((TrackBar_Saturation.Position/TrackBar_Saturation.Max-0.5)*4)*4096),
    CheckBox_GSBA.Checked);
end;

procedure TFShiftColors.ApplyDefinitely;
begin
  activeLayer.PutImage(0,0,backupImage,dmSet);
  ApplyChosenShift;
  FreeAndNil(backupImage);
  LazPaintInstance.Image.SaveLayerOrSelectionUndo;
end;

initialization
  {$I ushiftcolors.lrs}

end.

