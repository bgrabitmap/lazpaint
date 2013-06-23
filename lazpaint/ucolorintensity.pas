unit ucolorintensity;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, BGRABitmap, LazPaintType, uscaledpi, uresourcestrings;

type
  TColorIntensityMode = (ciIntensity, ciLightness);

  { TFColorIntensity }

  TFColorIntensity = class(TForm)
    Button_Cancel: TButton;
    Button_OK: TButton;
    Label1: TLabel;
    Label2: TLabel;
    TrackBar_Multiply: TTrackBar;
    TrackBar_Shift: TTrackBar;
    procedure Button_OKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TrackBar_Change(Sender: TObject);
  private
    { private declarations }
    backupImage: TBGRABitmap;
    initialized: boolean;
  public
    { public declarations }
    LazPaintInstance: TLazPaintCustomInstance;
    mode: TColorIntensityMode;
    activeLayer: TBGRABitmap;
    procedure ApplyIntensity(multiply, shift: single);
    procedure ApplyChosenIntensity;
    procedure ApplyDefinitely;

  end;

implementation

uses umac, BGRABitmapTypes;

{ TFColorIntensity }

procedure TFColorIntensity.FormCreate(Sender: TObject);
begin
  ScaleDPI(Self,OriginalDPI);

  CheckOKCancelBtns(Button_OK,Button_Cancel);
  TrackBar_Multiply.Min := 0;
  TrackBar_Multiply.Max := TrackBar_Multiply.Width and not 3;
  TrackBar_Multiply.Position := TrackBar_Multiply.Max div 2;
  TrackBar_Multiply.Frequency := TrackBar_Multiply.Max div 4;
  TrackBar_Shift.Max := TrackBar_Shift.Width and not 3;
  TrackBar_Shift.Position := TrackBar_Shift.Max div 2;
  TrackBar_Shift.Frequency := TrackBar_Shift.Max div 4;
  mode := ciIntensity;
  initialized := true;
end;

procedure TFColorIntensity.Button_OKClick(Sender: TObject);
begin
  ApplyDefinitely;
  Close;
end;

procedure TFColorIntensity.FormHide(Sender: TObject);
begin
  if backupImage <> nil then
  begin
    activeLayer.PutImage(0,0,backupImage,dmSet);
    LazPaintInstance.NotifyImageChangeCompletely(True);
    FreeAndNil(backupImage);
  end;
end;

procedure TFColorIntensity.FormShow(Sender: TObject);
begin
  backupImage := activeLayer.Duplicate as TBGRABitmap;
  if mode = ciIntensity then Caption := rsIntensity else
    Caption := rsLightness;
  ApplyChosenIntensity;
end;

procedure TFColorIntensity.TrackBar_Change(Sender: TObject);
begin
  if initialized then
    ApplyChosenIntensity;
end;

procedure TFColorIntensity.ApplyIntensity(multiply, shift: single);
var n: integer;
    psrc,pdest: PBGRAPixel;
    ec: TExpandedPixel;
    newIntensity: single;
begin
    psrc := backupImage.Data;
    pdest := activeLayer.Data;
    if mode = ciIntensity then
    begin
      for n := 0 to activeLayer.NbPixels-1 do
      begin
        ec := GammaExpansion(psrc^);
        newIntensity := (GetIntensity(ec)-32767.5)*multiply+shift+32767.5;
        if newIntensity < 0 then newIntensity := 0;
        if newIntensity > 65535 then newIntensity := 65535;
        pdest^ := GammaCompression(SetIntensity(ec,round(newIntensity)));
        inc(psrc);
        inc(pdest);
      end;
    end else //ciLightness
    begin
      for n := 0 to activeLayer.NbPixels-1 do
      begin
        ec := GammaExpansion(psrc^);
        newIntensity := (GetLightness(ec)-32767.5)*multiply+shift+32767.5;
        if newIntensity < 0 then newIntensity := 0;
        if newIntensity > 65535 then newIntensity := 65535;
        pdest^ := GammaCompression(SetLightness(ec,round(newIntensity)));
        inc(psrc);
        inc(pdest);
      end;
    end;
    activeLayer.InvalidateBitmap;
    LazPaintInstance.NotifyImageChangeCompletely(True);
end;

procedure TFColorIntensity.ApplyChosenIntensity;
begin
  ApplyIntensity(exp((TrackBar_Multiply.Position/TrackBar_Multiply.Max-0.5)*4),(TrackBar_Shift.Position/TrackBar_Shift.Max-0.5)*2*65535);
end;

procedure TFColorIntensity.ApplyDefinitely;
begin
  activeLayer.PutImage(0,0,backupImage,dmSet);
  ApplyChosenIntensity;
  LazPaintInstance.Image.SaveLayerOrSelectionUndo;
  FreeAndNil(backupImage);
end;

initialization
  {$I ucolorintensity.lrs}

end.

