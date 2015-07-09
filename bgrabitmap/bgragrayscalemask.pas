unit BGRAGrayscaleMask;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmapTypes;

type
  { TGrayscaleMask }

  TGrayscaleMask = class
  private
     FData: PByte;
     FWidth, FHeight, FNbPixels: Integer;
     function GetScanLine(Y: Integer): PByte;
     procedure Init(AWidth,AHeight: Integer);
  public
     constructor Create(AWidth,AHeight: Integer); overload;
     constructor Create(AWidth,AHeight: Integer; AValue: byte); overload;
     constructor Create(ABitmap: TBGRACustomBitmap; AChannel: TChannel); overload;
     constructor CreateDownSample(ABitmap: TBGRACustomBitmap; AWidth,AHeight: integer);
     procedure CopyFrom(ABitmap: TBGRACustomBitmap; AChannel: TChannel);
     procedure Draw(ABitmap: TBGRACustomBitmap; X,Y: Integer);
     procedure DrawAsAlpha(ABitmap: TBGRACustomBitmap; X,Y: Integer; const c: TBGRAPixel); overload;
     procedure DrawAsAlpha(ABitmap: TBGRACustomBitmap; X,Y: Integer; texture: IBGRAScanner); overload;
     destructor Destroy; override;
     function GetPixel(X,Y: integer): byte;
     procedure SetPixel(X,Y: integer; AValue: byte);
     property ScanLine[Y: Integer]: PByte read GetScanLine;
     property Data: PByte read FData;
     property Width: Integer read FWidth;
     property Height: Integer read FHeight;
     property NbPixels: Integer read FNbPixels;
  end;

procedure DownSamplePutImageGrayscale(sourceData: PByte; sourcePixelSize: NativeInt; sourceRowDelta: NativeInt; sourceWidth, sourceHeight: NativeInt; dest: TGrayscaleMask; ADestRect: TRect);
procedure DownSamplePutImageGrayscale(source: TBGRACustomBitmap; dest: TGrayscaleMask; ADestRect: TRect);

procedure BGRAFillClearTypeGrayscaleMask(dest: TBGRACustomBitmap; x,
  y: integer; xThird: integer; mask: TGrayscaleMask; color: TBGRAPixel;
  texture: IBGRAScanner; RGBOrder: boolean);

implementation

uses BGRABlend;

procedure BGRAFillClearTypeGrayscaleMask(dest: TBGRACustomBitmap; x,
  y: integer; xThird: integer; mask: TGrayscaleMask; color: TBGRAPixel;
  texture: IBGRAScanner; RGBOrder: boolean);
var delta: NativeInt;
begin
  delta := mask.Width;
  BGRABlend.BGRAFillClearTypeMaskPtr(dest,x,y,xThird,mask.ScanLine[0],1,delta,mask.Width,mask.Height,color,texture,RGBOrder);
end;

{ TGrayscaleMask }

function TGrayscaleMask.GetScanLine(Y: Integer): PByte;
begin
  if (y < 0) or (y >= FHeight) then
    raise ERangeError.Create('Scanline: out of bounds');
  result := FData + NativeInt(Y)*NativeInt(FWidth);
end;

procedure TGrayscaleMask.Init(AWidth, AHeight: Integer);
begin
  if FData <> nil then
  begin
    FreeMem(FData);
    FData := nil;
  end;
  FWidth := AWidth; if FWidth < 0 then FWidth:= 0;
  FHeight := AHeight; if FHeight < 0 then FHeight:= 0;
  FNbPixels:= FWidth*FHeight;
  if FNbPixels > 0 then getmem(FData, FNbPixels);
end;

procedure TGrayscaleMask.CopyFrom(ABitmap: TBGRACustomBitmap; AChannel: TChannel
  );
var psrc: PByte;
  pdest: PByte;
  x,y: integer;
  ofs: NativeInt;
begin
  Init(ABitmap.Width,ABitmap.Height);
  if FNbPixels > 0 then
  begin
    pdest := FData;
    Case AChannel of
      cAlpha: ofs := TBGRAPixel_AlphaByteOffset;
      cRed: ofs := TBGRAPixel_RedByteOffset;
      cGreen: ofs := TBGRAPixel_GreenByteOffset;
    else
      ofs := TBGRAPixel_BlueByteOffset;
    end;
    for y := 0 to FHeight-1 do
    begin
      psrc := PByte(ABitmap.ScanLine[y])+ofs;
      for x := FWidth-1 downto 0 do
      begin
        pdest^ := psrc^;
        inc(pdest);
        inc(psrc,sizeof(TBGRAPixel));
      end;
    end;
  end;
end;

constructor TGrayscaleMask.Create(AWidth, AHeight: Integer);
begin
  Init(AWidth,AHeight);
  if FNbPixels > 0 then FillChar(FData^, FNbPixels, 0);
end;

constructor TGrayscaleMask.Create(AWidth, AHeight: Integer; AValue: byte);
begin
  Init(AWidth,AHeight);
  if FNbPixels > 0 then FillChar(FData^, FNbPixels, AValue);
end;

constructor TGrayscaleMask.Create(ABitmap: TBGRACustomBitmap; AChannel: TChannel);
begin
  CopyFrom(ABitmap, AChannel);
end;

constructor TGrayscaleMask.CreateDownSample(ABitmap: TBGRACustomBitmap; AWidth,
  AHeight: integer);
begin
  if (AWidth = ABitmap.Width) and (AHeight = ABitmap.Height) then
    CopyFrom(ABitmap,cGreen)
  else
  begin
    if (ABitmap.Width < AWidth) or (ABitmap.Height < AHeight) then
      raise exception.Create('Original size smaller');
    Init(AWidth,AHeight);
    if FNbPixels > 0 then
      DownSamplePutImageGrayscale(ABitmap, self, rect(0,0,FWidth,FHeight));
  end;
end;

procedure TGrayscaleMask.Draw(ABitmap: TBGRACustomBitmap; X, Y: Integer);
var
  yb, minxb, minyb, maxxb, maxyb, ignoreleft, copycount,
  i, delta_source, delta_dest: integer;
  pdest: PBGRAPixel;
  psource: PByte;
  value: byte;
begin
  if not CheckPutImageBounds(x,y,FWidth,Fheight,minxb,minyb,maxxb,maxyb,ignoreleft,ABitmap.ClipRect) then exit;
  copycount := maxxb - minxb + 1;

  psource := ScanLine[minyb - y] + ignoreleft;
  delta_source := FWidth;

  pdest := ABitmap.Scanline[minyb] + minxb;
  if ABitmap.LineOrder = riloBottomToTop then
    delta_dest := -ABitmap.Width
  else
    delta_dest := ABitmap.Width;

  Dec(delta_source, copycount);
  Dec(delta_dest, copycount);
  for yb := minyb to maxyb do
  begin
    for i := copycount -1 downto 0 do
    begin
      value := psource^;
      pdest^ := BGRA(value,value,value,255);
      inc(psource);
      inc(pdest);
    end;
    Inc(psource, delta_source);
    Inc(pdest, delta_dest);
  end;
  ABitmap.InvalidateBitmap;
end;

procedure TGrayscaleMask.DrawAsAlpha(ABitmap: TBGRACustomBitmap; X, Y: Integer;
  const c: TBGRAPixel);
var
  yb, minxb, minyb, maxxb, maxyb, ignoreleft, copycount,
  i, delta_source, delta_dest: integer;
  pdest: PBGRAPixel;
  psource: PByte;
begin
  if not CheckPutImageBounds(x,y,FWidth,Fheight,minxb,minyb,maxxb,maxyb,ignoreleft,ABitmap.ClipRect) then exit;
  copycount := maxxb - minxb + 1;

  psource := ScanLine[minyb - y] + ignoreleft;
  delta_source := FWidth;

  pdest := ABitmap.Scanline[minyb] + minxb;
  if ABitmap.LineOrder = riloBottomToTop then
    delta_dest := -ABitmap.Width
  else
    delta_dest := ABitmap.Width;

  Dec(delta_source, copycount);
  Dec(delta_dest, copycount);
  for yb := minyb to maxyb do
  begin
    for i := copycount -1 downto 0 do
    begin
      DrawPixelInlineWithAlphaCheck(pdest,c,psource^);
      inc(psource);
      inc(pdest);
    end;
    Inc(psource, delta_source);
    Inc(pdest, delta_dest);
  end;
  ABitmap.InvalidateBitmap;
end;

procedure TGrayscaleMask.DrawAsAlpha(ABitmap: TBGRACustomBitmap; X, Y: Integer;
  texture: IBGRAScanner);
var
  yb, minxb, minyb, maxxb, maxyb, ignoreleft, copycount,
  i, delta_source, delta_dest: integer;
  pdest,ptex: PBGRAPixel;
  psource: PByte;
  memScan: PBGRAPixel;
begin
  if not CheckPutImageBounds(x,y,FWidth,Fheight,minxb,minyb,maxxb,maxyb,ignoreleft,ABitmap.ClipRect) then exit;
  copycount := maxxb - minxb + 1;
  if copycount <= 0 then exit;

  psource := ScanLine[minyb - y] + ignoreleft;
  delta_source := FWidth;

  pdest := ABitmap.Scanline[minyb] + minxb;
  if ABitmap.LineOrder = riloBottomToTop then
    delta_dest := -ABitmap.Width
  else
    delta_dest := ABitmap.Width;

  getmem(memscan, copycount*sizeof(TBGRAPixel));

  Dec(delta_source, copycount);
  Dec(delta_dest, copycount);
  for yb := minyb to maxyb do
  begin
    texture.ScanMoveTo(ignoreleft,yb-y);
    texture.ScanPutPixels(memscan,copycount,dmSet);
    ptex := memScan;
    for i := copycount -1 downto 0 do
    begin
      DrawPixelInlineWithAlphaCheck(pdest,ptex^,psource^);
      inc(psource);
      inc(pdest);
      inc(ptex);
    end;
    Inc(psource, delta_source);
    Inc(pdest, delta_dest);
  end;
  ABitmap.InvalidateBitmap;
  freemem(memscan);
end;

destructor TGrayscaleMask.Destroy;
begin
  if FData <> nil then
  begin
    freemem(FData);
    FData := nil;
  end;
  inherited Destroy;
end;

function TGrayscaleMask.GetPixel(X, Y: integer): byte;
begin
  if (x < 0) or (x >= FWidth) then
    raise ERangeError.Create('GetPixel: out of bounds');
  result := (ScanLine[Y]+X)^;
end;

procedure TGrayscaleMask.SetPixel(X, Y: integer; AValue: byte);
begin
  if (x < 0) or (x >= FWidth) then
    raise ERangeError.Create('SetPixel: out of bounds');
  (ScanLine[Y]+X)^ := AValue;
end;

procedure DownSamplePutImageGrayscale(sourceData: PByte;
  sourcePixelSize: NativeInt; sourceRowDelta: NativeInt; sourceWidth,
  sourceHeight: NativeInt; dest: TGrayscaleMask; ADestRect: TRect);
var
  x_dest,y_dest: integer;
  pdest: PByte;
  nbPix,sum: NativeUInt;
  prev_x_src,x_src,x_src_nb,xb: NativeInt;
  x_src_inc,x_src_acc,x_src_div,x_src_rest: NativeInt;
  prev_y_src,y_src,y_src_nb,yb: NativeInt;
  y_src_inc,y_src_acc,y_src_div,y_src_rest: NativeInt;
  psrc,psrc2,psrc3: PByte;
begin
  y_src_div := ADestRect.Bottom-ADestRect.Top;
  y_src_inc := sourceHeight div y_src_div;
  y_src_rest := sourceHeight mod y_src_div;
  x_src_div := ADestRect.Right-ADestRect.Left;
  x_src_inc := sourceWidth div x_src_div;
  x_src_rest := sourceWidth mod x_src_div;

  if (x_src_rest = 0) and (y_src_rest = 0) then
  begin
    x_src_nb := x_src_inc;
    y_src_nb := y_src_inc;
    nbPix := x_src_nb*y_src_nb;
    y_src := 0;
    for y_dest := ADestRect.Top to ADestRect.Bottom-1 do
    begin
      pdest := dest.ScanLine[y_dest]+ADestRect.Left;
      psrc := sourceData + y_src*sourceRowDelta;
      inc(y_src,y_src_inc);

      for x_dest := ADestRect.Right-ADestRect.Left-1 downto 0 do
      begin
        sum := 0;
        psrc2 := psrc;
        for xb := x_src_nb-1 downto 0 do
        begin
          psrc3 := psrc2;
          for yb := y_src_nb-1 downto 0 do
          begin
            inc(sum, psrc3^);
            inc(psrc3, sourceRowDelta);
          end;
          inc(psrc2, sourcePixelSize);
        end;
        pdest^ := sum div nbPix;

        psrc := psrc2;
        inc(pdest);
      end;
    end;
  end else
  begin
    y_src := 0;
    y_src_acc := 0;
    for y_dest := ADestRect.Top to ADestRect.Bottom-1 do
    begin
      pdest := dest.ScanLine[y_dest]+ADestRect.Left;
      psrc := sourceData + y_src*sourceRowDelta;

      prev_y_src := y_src;
      inc(y_src,y_src_inc);
      inc(y_src_acc,y_src_rest);
      if y_src_acc >= y_src_div then
      begin
        dec(y_src_acc,y_src_div);
        inc(y_src);
      end;
      y_src_nb := y_src-prev_y_src;

      x_src := 0;
      x_src_acc := 0;
      for x_dest := ADestRect.Right-ADestRect.Left-1 downto 0 do
      begin
        prev_x_src := x_src;
        inc(x_src,x_src_inc);
        inc(x_src_acc,x_src_rest);
        if x_src_acc >= x_src_div then
        begin
          dec(x_src_acc,x_src_div);
          inc(x_src);
        end;
        x_src_nb := x_src-prev_x_src;

        sum := 0;
        nbPix := 0;
        psrc2 := psrc;
        for xb := x_src_nb-1 downto 0 do
        begin
          psrc3 := psrc2;
          for yb := y_src_nb-1 downto 0 do
          begin
            inc(nbPix);
            inc(sum, psrc3^);
            inc(psrc3, sourceRowDelta);
          end;
          inc(psrc2, sourcePixelSize);
        end;
        pdest^ := sum div nbPix;

        psrc := psrc2;
        inc(pdest);
      end;
    end;
  end;
end;

procedure DownSamplePutImageGrayscale(source: TBGRACustomBitmap;
  dest: TGrayscaleMask; ADestRect: TRect);
var delta: NativeInt;
begin
  delta := source.Width*sizeof(TBGRAPixel);
  if source.LineOrder = riloBottomToTop then
    delta := -delta;
  DownSamplePutImageGrayscale(PByte(source.ScanLine[0])+1,sizeof(TBGRAPixel),delta,source.Width,source.Height,dest,ADestRect);
end;

end.

