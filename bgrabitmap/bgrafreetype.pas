unit BGRAFreeType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, BGRABitmapTypes, EasyLazFreeType, FPimage;

type

  { TBGRAFreeTypeDrawer }

  TBGRAFreeTypeDrawer = class(TFreeTypeDrawer)
  private
    FMask: TBGRACustomBitmap;
    FColor: TBGRAPixel;
    procedure RenderDirectly(x, y, tx: integer; data: pointer);
    procedure RenderDirectlyClearType(x, y, tx: integer; data: pointer);
  public
    Destination: TBGRACustomBitmap;
    ClearTypeRGBOrder: boolean;
    constructor Create(ADestination: TBGRACustomBitmap);
    procedure DrawText(AText: string; AFont: TFreeTypeRenderableFont; x,y: single; AColor: TFPColor); override; overload;
    procedure DrawText(AText: string; AFont: TFreeTypeRenderableFont; x,y: single; AColor: TBGRAPixel); overload;
    procedure DrawText(AText: string; AFont: TFreeTypeRenderableFont; x,y: single; AColor: TBGRAPixel; AAlign: TFreeTypeAlignments); overload;
    destructor Destroy; override;
  end;

implementation

uses LCLType, BGRABlend, BGRAText;

{ TBGRAFreeTypeDrawer }

procedure TBGRAFreeTypeDrawer.RenderDirectly( x,y,tx: integer;
                          data: pointer );
var psrc: pbyte;
    pdest: PBGRAPixel;
    c: TBGRAPixel;
begin
  if Destination <> nil then
  begin
    //ensure rendering in bounds
    if (y < 0) or (y >= Destination.height) or (x < 0) or (x > Destination.width-tx) then exit;

    c := FColor;
    psrc := pbyte(data);
    pdest := Destination.ScanLine[y]+x;
    while tx > 0 do
    begin
      DrawPixelInlineWithAlphaCheck(pdest,c,psrc^);
      inc(psrc);
      inc(pdest);
      dec(tx);
    end;
  end;
end;

procedure TBGRAFreeTypeDrawer.RenderDirectlyClearType(x, y, tx: integer; data: pointer);
var xb: integer;
    psrc: pbyte;
    pdest: PBGRAPixel;
begin
  if Destination <> nil then
  begin
    tx := tx div 3;
    if tx=0 then exit;
    if (FMask <> nil) and (FMask.Width <> tx) then
      FMask.SetSize(tx,1)
    else if FMask = nil then FMask := BGRABitmapFactory.create(tx,1);

    pdest := FMask.Data;
    psrc := pbyte(data);
    pdest^.red := (psrc^ + psrc^ + (psrc+1)^) div 3;
    pdest^.green := (psrc^+ (psrc+1)^ + (psrc+2)^) div 3;
    if tx > 1 then
      pdest^.blue := ((psrc+1)^ + (psrc+2)^ + (psrc+3)^) div 3
    else
      pdest^.blue := ((psrc+1)^ + (psrc+2)^ + (psrc+2)^) div 3;
    inc(pdest);
    inc(psrc,3);
    for xb := 1 to tx-2 do
    begin
      pdest^.red := ((psrc-1)^+ psrc^ + (psrc+1)^) div 3;
      pdest^.green := (psrc^+ (psrc+1)^ + (psrc+2)^) div 3;
      pdest^.blue := ((psrc+1)^ + (psrc+2)^ + (psrc+3)^) div 3;
      inc(pdest);
      inc(psrc,3);
    end;
    if tx > 1 then
    begin
      pdest^.red := ((psrc-1)^+ psrc^ + (psrc+1)^) div 3;
      pdest^.green := (psrc^+ (psrc+1)^ + (psrc+2)^) div 3;
      pdest^.blue := ((psrc+1)^ + (psrc+2)^ + (psrc+2)^) div 3;
    end;
    BGRAFillClearTypeRGBMask(Destination,x div 3,y,FMask,FColor,nil,ClearTypeRGBOrder);
  end;
end;

constructor TBGRAFreeTypeDrawer.Create(ADestination: TBGRACustomBitmap);
begin
  Destination := ADestination;
  ClearTypeRGBOrder:= true;
end;

procedure TBGRAFreeTypeDrawer.DrawText(AText: string;
  AFont: TFreeTypeRenderableFont; x, y: single; AColor: TFPColor);
begin
  FColor := FPColorToBGRA(AColor);
  if AFont.ClearType then
    AFont.RenderText(AText, x, y, Destination.ClipRect, @RenderDirectlyClearType)
  else
    AFont.RenderText(AText, x, y, Destination.ClipRect, @RenderDirectly);
end;

procedure TBGRAFreeTypeDrawer.DrawText(AText: string;
  AFont: TFreeTypeRenderableFont; x, y: single; AColor: TBGRAPixel);
begin
  DrawText(AText, AFont, x,y, BGRAToFPColor(AColor));
end;

procedure TBGRAFreeTypeDrawer.DrawText(AText: string;
  AFont: TFreeTypeRenderableFont; x, y: single; AColor: TBGRAPixel;
  AAlign: TFreeTypeAlignments);
begin
  DrawText(AText, AFont, x,y, BGRAToFPColor(AColor), AAlign);
end;

destructor TBGRAFreeTypeDrawer.Destroy;
begin
  FMask.Free;
  inherited Destroy;
end;

end.
