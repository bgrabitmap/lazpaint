unit ucolorize;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, BGRABitmap, LazPaintType, uscaledpi;

type

  { TFColorize }

  TFColorize = class(TForm)
    Button_Cancel: TButton;
    Button_OK: TButton;
    Label1: TLabel;
    Label2: TLabel;
    TrackBar_Hue: TTrackBar;
    TrackBar_Saturation: TTrackBar;
    procedure Button_OKClick(Sender: TObject);
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
    procedure ApplyChosenColor;
    procedure ApplyDefinitely;
    procedure ApplyColor(hue, sat: integer);
  end; 

implementation

uses umac, BGRABitmapTypes;

{ TFColorize }

procedure TFColorize.FormCreate(Sender: TObject);
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

procedure TFColorize.Button_OKClick(Sender: TObject);
begin
  ApplyDefinitely;
  Close;
end;

procedure TFColorize.FormHide(Sender: TObject);
begin
  if backupImage <> nil then
  begin
    activeLayer.PutImage(0,0,backupImage,dmSet);
    LazPaintInstance.NotifyImageChangeCompletely(True);
    FreeAndNil(backupImage);
  end;
end;

procedure TFColorize.FormShow(Sender: TObject);
begin
  backupImage := activeLayer.Duplicate as TBGRABitmap;
  ApplyChosenColor;
end;

procedure TFColorize.TrackBar_Change(Sender: TObject);
begin
  if initialized then
    ApplyChosenColor;
end;

procedure TFColorize.ApplyChosenColor;
begin
  ApplyColor(round(TrackBar_Hue.Position/TrackBar_Hue.Max*65535),round(TrackBar_Saturation.Position/TrackBar_Saturation.Max*65535));
end;

procedure TFColorize.ApplyDefinitely;
begin
  activeLayer.PutImage(0,0,backupImage,dmSet);
  ApplyChosenColor;
  FreeAndNil(backupImage);
  LazPaintInstance.Image.SaveLayerOrSelectionUndo;
end;

procedure TFColorize.ApplyColor(hue, sat: integer);
var n: integer;
    psrc,pdest: PBGRAPixel;
    hsl: THSLAPixel;
    ec: TExpandedPixel;
begin
    psrc := backupImage.Data;
    pdest := activeLayer.Data;
    for n := 0 to activeLayer.NbPixels-1 do
    begin
      ec := GammaExpansion(psrc^);
      hsl.alpha := ec.alpha;
      hsl.lightness := GetLightness(ec);
      hsl.hue := hue;
      hsl.saturation := sat;
      pdest^ := HSLAToBGRA(hsl);
      inc(psrc);
      inc(pdest);
    end;
    activeLayer.InvalidateBitmap;
    LazPaintInstance.NotifyImageChangeCompletely(True);
end;

initialization
  {$I ucolorize.lrs}

end.

