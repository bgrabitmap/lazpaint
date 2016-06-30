unit BGRAReadIco;

{$mode objfpc}{$H+}
{$i bgrabitmap.inc}

interface

uses
  Classes, SysUtils, FPimage;

type

  { TBGRAReaderIco }

  TBGRAReaderIco = class(TFPCustomImageReader)
  protected
    procedure InternalRead({%H-}Str: TStream; {%H-}Img: TFPCustomImage); override;
    function InternalCheck(Str: TStream): boolean; override;
  public
    WantedWidth, WantedHeight : integer;
  end;

implementation

uses BGRABitmapTypes{$IFDEF BGRABITMAP_USE_LCL}, Graphics{$ENDIF};

{ TBGRAReaderIco }

procedure TBGRAReaderIco.InternalRead(Str: TStream; Img: TFPCustomImage);
{$IFDEF BGRABITMAP_USE_LCL}
var ico: TIcon; i,bestIdx: integer;
    height,width: word; format:TPixelFormat;
    bestHeight,bestWidth: integer; maxFormat: TPixelFormat;
    compWidth,compHeight: integer;
begin
  if WantedWidth > 0 then compWidth:= WantedWidth else compWidth:= 65536;
  if WantedHeight > 0 then compHeight:= WantedHeight else compHeight:= 65536;
  ico := TIcon.Create;
  try
    ico.LoadFromStream(Str);
    bestIdx := -1;
    bestHeight := 0;
    bestWidth := 0;
    maxFormat := pfDevice;
    for i := 0 to ico.Count-1 do
    begin
      ico.GetDescription(i,format,height,width);
      if (bestIdx = -1) or (abs(height-compHeight)+abs(width-compWidth) < abs(bestHeight-compHeight)+abs(bestWidth-compWidth)) or
      ((height = bestHeight) or (width = bestWidth) and (format > maxFormat)) then
      begin
        bestIdx := i;
        bestHeight := height;
        bestWidth := width;
        maxFormat := format;
      end;
    end;
    if (bestIdx = -1) or (bestWidth = 0) or (bestHeight = 0) then
      raise exception.Create('No adequate icon found') else
    begin
      ico.Current := bestIdx;
      (Img as TBGRACustomBitmap).Assign(ico);
    end;
  finally
    ico.free;
  end;
end;
{$ELSE}
begin
  raise exception.create('Not implemented');
end;
{$ENDIF}

function TBGRAReaderIco.InternalCheck(Str: TStream): boolean;
var {%H-}magic: packed array[0..5] of byte;
    oldPos: int64;
begin
  oldPos := str.Position;
  result := (str.Read({%H-}magic,sizeof(magic)) = sizeof(magic));
  str.Position:= oldPos;
  if result then
    result := (magic[0] = $00) and (magic[1] = $00) and (magic[2] in[$01,$02]) and (magic[3] = $00) and
             (magic[4] + (magic[5] shl 8) > 0);
end;

initialization

  DefaultBGRAImageReader[ifIco] := TBGRAReaderIco;

end.

