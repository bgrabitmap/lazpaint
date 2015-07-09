unit BGRALCLBitmap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, GraphType, BGRABitmapTypes, BGRADefaultBitmap;

type
  { TBGRALCLBitmap }

  TBGRALCLBitmap = class(TBGRADefaultBitmap)
  protected
    function LoadFromRawImage(ARawImage: TRawImage; DefaultOpacity: byte;
      AlwaysReplaceAlpha: boolean = False; RaiseErrorOnInvalidPixelFormat: boolean = True): boolean; override;
    function CreateDefaultFontRenderer: TBGRACustomFontRenderer; override;
    procedure DoLoadFromBitmap; override;
    procedure RebuildBitmap; override;
    function CreatePtrBitmap(AWidth, AHeight: integer; AData: PBGRAPixel
      ): TBGRAPtrBitmap; override;
    procedure AssignRasterImage(ARaster: TRasterImage); virtual;
  public
    procedure Assign(Source: TPersistent); override;
    procedure DataDrawTransparent(ACanvas: TCanvas; Rect: TRect;
      AData: Pointer; ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer); override;
    procedure DataDrawOpaque(ACanvas: TCanvas; Rect: TRect; AData: Pointer;
      ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer); override;
    procedure GetImageFromCanvas(CanvasSource: TCanvas; x, y: integer); override;
    procedure LoadFromDevice({%H-}DC: System.THandle); override;
    procedure LoadFromDevice({%H-}DC: System.THandle; {%H-}ARect: TRect); override;
    procedure TakeScreenshotOfPrimaryMonitor; override;
    procedure TakeScreenshot({%H-}ARect: TRect); override;
  end;

  { TBGRALCLPtrBitmap }

  TBGRALCLPtrBitmap = class(TBGRAPtrBitmap)

    procedure RebuildBitmap; override;
    function CreatePtrBitmap(AWidth, AHeight: integer; AData: PBGRAPixel): TBGRAPtrBitmap; override;
    function CreateDefaultFontRenderer: TBGRACustomFontRenderer; override;
    function LoadFromRawImage(ARawImage: TRawImage; DefaultOpacity: byte;
      AlwaysReplaceAlpha: boolean=False; {%H-}RaiseErrorOnInvalidPixelFormat: boolean
      =True): boolean; override;
  public
    procedure GetImageFromCanvas(CanvasSource: TCanvas; x, y: integer); override;
    procedure DataDrawTransparent(ACanvas: TCanvas; Rect: TRect; AData: Pointer;
      ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer); override;
    procedure DataDrawOpaque(ACanvas: TCanvas; Rect: TRect; AData: Pointer;
      ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer); override;
  end;

implementation

uses BGRAText, LCLType, LCLIntf, FPimage;

{ Load raw image data. It must be 32bit or 24 bits per pixel}
function LoadFromRawImageImplementation(ADestination: TBGRADefaultBitmap; ARawImage: TRawImage;
  DefaultOpacity: byte; AlwaysReplaceAlpha: boolean; RaiseErrorOnInvalidPixelFormat: boolean): boolean;
var
  psource_byte, pdest_byte,
  psource_first, pdest_first: PByte;
  psource_delta, pdest_delta: integer;

  n: integer;
  mustSwapRedBlue, fromARGB: boolean;

  procedure CopyFrom24Bit(psrc: PByte; pdest: PBGRAPixel; count: integer);
  begin
    if mustSwapRedBlue then
    begin
      while count > 0 do
      begin
        PByte(pdest)^ := (psrc+2)^;
        (PByte(pdest)+1)^ := (psrc+1)^;
        (PByte(pdest)+2)^ := psrc^;
        pdest^.alpha := DefaultOpacity;
        inc(psrc,3);
        inc(pdest);
        dec(count);
      end;
    end else
    begin
      while count > 0 do
      begin
        PWord(pdest)^ := PWord(psrc)^;
        (PByte(pdest)+2)^ := (psrc+2)^;
        pdest^.alpha := DefaultOpacity;
        inc(psrc,3);
        inc(pdest);
        dec(count);
      end;
    end;
  end;

  procedure CopyAndSwapIfNecessary(psrc: PBGRAPixel; pdest: PBGRAPixel; count: integer);
  begin
    if fromARGB then
    begin
      while count > 0 do
      begin
        PDWord(pdest)^ := ((PByte(psrc)+3)^ shl TBGRAPixel_BlueShift) or
                          ((PByte(psrc)+2)^ shl TBGRAPixel_GreenShift) or
                          ((PByte(psrc)+1)^ shl TBGRAPixel_RedShift) or
                          (PByte(psrc)^ shl TBGRAPixel_AlphaShift);
        dec(count);
        inc(pdest);
        inc(psrc);
      end;
    end else
    if mustSwapRedBlue then
    begin
      while count > 0 do
      begin
        pdest^.red := psrc^.blue;
        pdest^.green := psrc^.green;
        pdest^.blue := psrc^.red;
        pdest^.alpha := psrc^.alpha;
        dec(count);
        inc(pdest);
        inc(psrc);
      end;
    end else
      move(psrc^,pdest^,count*sizeof(TBGRAPixel));
  end;

  procedure CopyAndSwapIfNecessaryAndSetAlpha(psrc: PBGRAPixel; pdest: PBGRAPixel; count: integer);
  begin
    if fromARGB then
    begin
      while count > 0 do
      begin
        PDWord(pdest)^ := ((PByte(psrc)+3)^ shl TBGRAPixel_BlueShift) or
                          ((PByte(psrc)+2)^ shl TBGRAPixel_GreenShift) or
                          ((PByte(psrc)+1)^ shl TBGRAPixel_RedShift) or
                          (DefaultOpacity shl TBGRAPixel_AlphaShift);
        inc(psrc);
        inc(pdest);
        dec(count);
      end;
    end else
    if mustSwapRedBlue then
    begin
      while count > 0 do
      begin
        pdest^.red := psrc^.blue;
        pdest^.green := psrc^.green;
        pdest^.blue := psrc^.red;
        pdest^.alpha := DefaultOpacity; //use default opacity
        inc(psrc);
        inc(pdest);
        dec(count);
      end;
    end else
    begin
      while count > 0 do
      begin
        PDWord(pdest)^ := PDWord(psrc)^;
        pdest^.alpha := DefaultOpacity; //use default opacity
        inc(psrc);
        inc(pdest);
        dec(count);
      end;
    end;
  end;

  procedure CopyAndSwapIfNecessaryAndReplaceAlpha(psrc: PBGRAPixel; pdest: PBGRAPixel; count: integer);
  const
    BGRA_ColorMask = not (255 shl TBGRAPixel_AlphaShift);
    ARGB_ColorMask = {$IFDEF ENDIAN_LITTLE}$FFFFFF00{$ELSE}$00FFFFFF{$ENDIF};

  var sourceval: Longword;
    alphaValue: NativeUint;
    OpacityOrMask : longword;
  begin
    OpacityOrMask := DefaultOpacity shl TBGRAPixel_AlphaShift;
    if fromARGB then
    begin
      while count > 0 do
      begin
        sourceval := plongword(psrc)^;
        alphaValue := {$IFDEF ENDIAN_LITTLE}sourceval and $ff{$ELSE}sourceval shr 24{$ENDIF};
        if (alphaValue = 0) and ((sourceval and ARGB_ColorMask) <> 0) then //if not black but transparent
        begin
          PDWord(pdest)^ := ((PByte(psrc)+3)^ shl TBGRAPixel_BlueShift) or
                            ((PByte(psrc)+2)^ shl TBGRAPixel_GreenShift) or
                            ((PByte(psrc)+1)^ shl TBGRAPixel_RedShift) or
                            OpacityOrMask;
        end else
        begin
          PDWord(pdest)^ := ((PByte(psrc)+3)^ shl TBGRAPixel_BlueShift) or
                            ((PByte(psrc)+2)^ shl TBGRAPixel_GreenShift) or
                            ((PByte(psrc)+1)^ shl TBGRAPixel_RedShift) or
                            (alphaValue shl TBGRAPixel_AlphaShift);
        end;
        dec(count);
        inc(pdest);
        inc(psrc);
      end;
    end else
    if mustSwapRedBlue then
    begin
      while count > 0 do
      begin
        sourceval := plongword(psrc)^;
        alphaValue:= (sourceVal shr TBGRAPixel_AlphaShift) and $ff;
        if (alphaValue = 0) and ((sourceval and BGRA_ColorMask) <> 0) then //if not black but transparent
        begin
          (PByte(pdest)+2)^ := PByte(psrc)^;
          (PByte(pdest)+1)^ := (PByte(psrc)+1)^;
          PByte(pdest)^ := (PByte(psrc)+2)^;
          pdest^.alpha := DefaultOpacity; //use default opacity
        end
        else
        begin
          (PByte(pdest)+2)^ := PByte(psrc)^;
          (PByte(pdest)+1)^ := (PByte(psrc)+1)^;
          PByte(pdest)^ := (PByte(psrc)+2)^;
          pdest^.alpha := alphaValue;
        end;
        dec(count);
        inc(pdest);
        inc(psrc);
      end;
    end else
    begin
      while count > 0 do
      begin
        sourceval := plongword(psrc)^;
        alphaValue:= (sourceVal shr TBGRAPixel_AlphaShift) and $ff;
        if (alphaValue = 0) and ((sourceval and BGRA_ColorMask) <> 0) then //if not black but transparent
          plongword(pdest)^ := (sourceval and BGRA_ColorMask) or OpacityOrMask //use default opacity
        else
          pdest^ := psrc^;
        dec(count);
        inc(pdest);
        inc(psrc);
      end;
    end;
  end;

begin
  if (ARawImage.Description.Width <> cardinal(ADestination.Width)) or
    (ARawImage.Description.Height <> cardinal(ADestination.Height)) then
    raise Exception.Create('Bitmap size is inconsistant');

  if (ADestination.Height=0) or (ADestination.Width=0) then
  begin
    result := true;
    exit;
  end;

  if ARawImage.Description.LineOrder = riloTopToBottom then
  begin
    psource_first := ARawImage.Data;
    psource_delta := ARawImage.Description.BytesPerLine;
  end else
  begin
    psource_first := ARawImage.Data + (ARawImage.Description.Height-1) * ARawImage.Description.BytesPerLine;
    psource_delta := -ARawImage.Description.BytesPerLine;
  end;

  //channels are in RGB order (alpha channel may follow)
  if ((ARawImage.Description.RedShift = 0) and
    (ARawImage.Description.BlueShift = 16) and
    (ARawImage.Description.ByteOrder = riboLSBFirst)) or
    ((ARawImage.Description.RedShift = 24) and
    (ARawImage.Description.BlueShift = 8) and
    (ARawImage.Description.ByteOrder = riboMSBFirst)) then
  begin
    mustSwapRedBlue:= not TBGRAPixel_RGBAOrder;
    fromARGB := false;
  end
  else
  begin
    //channels are in ARGB order
    if ((ARawImage.Description.RedShift = 8) and
      (ARawImage.Description.GreenShift = 16) and
      (ARawImage.Description.BlueShift = 24) and
      (ARawImage.Description.ByteOrder = riboLSBFirst)) or
      ((ARawImage.Description.RedShift = 16) and
      (ARawImage.Description.GreenShift = 8) and
      (ARawImage.Description.BlueShift = 0) and
      (ARawImage.Description.ByteOrder = riboMSBFirst)) then
      begin
        fromARGB := true;
        mustSwapRedBlue:= false;
      end
      else
      begin
        //assume channel are in BGR order (alpha channel may follow)
        fromARGB := false;
        mustSwapRedBlue:= TBGRAPixel_RGBAOrder;
      end;
  end;

  if ADestination.LineOrder = riloTopToBottom then
  begin
    pdest_first := PByte(ADestination.Data);
    pdest_delta := ADestination.Width*sizeof(TBGRAPixel);
  end else
  begin
    pdest_first := PByte(ADestination.Data) + (ADestination.Height-1)*ADestination.Width*sizeof(TBGRAPixel);
    pdest_delta := -ADestination.Width*sizeof(TBGRAPixel);
  end;

  { 32 bits per pixel }
  if (ARawImage.Description.BitsPerPixel = 32) and
    (ARawImage.DataSize >= longword(ADestination.NbPixels) * 4) then
  begin
    { If there is an alpha channel }
    if (ARawImage.Description.AlphaPrec = 8) and not AlwaysReplaceAlpha then
    begin
      if DefaultOpacity = 0 then
      begin
        if ARawImage.Description.LineOrder = ADestination.LineOrder then
          CopyAndSwapIfNecessary(PBGRAPixel(ARawImage.Data), ADestination.Data, ADestination.NbPixels) else
        begin
          psource_byte := psource_first;
          pdest_byte := pdest_first;
          for n := ADestination.Height-1 downto 0 do
          begin
            CopyAndSwapIfNecessary(PBGRAPixel(psource_byte), PBGRAPixel(pdest_byte), ADestination.Width);
            inc(psource_byte, psource_delta);
            inc(pdest_byte, pdest_delta);
          end;
        end;
      end
      else
      begin
        psource_byte := psource_first;
        pdest_byte := pdest_first;
        for n := ADestination.Height-1 downto 0 do
        begin
          CopyAndSwapIfNecessaryAndReplaceAlpha(PBGRAPixel(psource_byte), PBGRAPixel(pdest_byte), ADestination.Width);
          inc(psource_byte, psource_delta);
          inc(pdest_byte, pdest_delta);
        end;
      end;
    end
    else
    begin { If there isn't any alpha channel }
      psource_byte := psource_first;
      pdest_byte := pdest_first;
      for n := ADestination.Height-1 downto 0 do
      begin
        CopyAndSwapIfNecessaryAndSetAlpha(PBGRAPixel(psource_byte), PBGRAPixel(pdest_byte), ADestination.Width);
        inc(psource_byte, psource_delta);
        inc(pdest_byte, pdest_delta);
      end;
    end;
  end
  else
  { 24 bit per pixel }
  if (ARawImage.Description.BitsPerPixel = 24) then
  begin
    psource_byte := psource_first;
    pdest_byte := pdest_first;
    for n := ADestination.Height-1 downto 0 do
    begin
      CopyFrom24Bit(psource_byte, PBGRAPixel(pdest_byte), ADestination.Width);
      inc(psource_byte, psource_delta);
      inc(pdest_byte, pdest_delta);
    end;
  end
  else
  begin
    if RaiseErrorOnInvalidPixelFormat then
      raise Exception.Create('Invalid raw image format (' + IntToStr(
        ARawImage.Description.Depth) + ' found)') else
    begin
      result := false;
      exit;
    end;
  end;

  ADestination.InvalidateBitmap;
  result := true;
end;

{ Draw BGRA data to a canvas with transparency }
procedure DataDrawTransparentImplementation(ACanvas: TCanvas; Rect: TRect;
  AData: Pointer; ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer);
var
  Temp:     TBitmap;
  RawImage: TRawImage;
  BitmapHandle, MaskHandle: HBitmap;
begin
  RawImage.Init;
  RawImage.Description.Init_BPP32_B8G8R8A8_BIO_TTB(AWidth, AHeight);
  RawImage.Description.LineOrder := ALineOrder;
  RawImage.Data     := PByte(AData);
  RawImage.DataSize := AWidth * AHeight * sizeof(TBGRAPixel);
  if not RawImage_CreateBitmaps(RawImage, BitmapHandle, MaskHandle, False) then
    raise FPImageException.Create('Failed to create bitmap handle');
  Temp := TBitmap.Create;
  Temp.Handle := BitmapHandle;
  Temp.MaskHandle := MaskHandle;
  ACanvas.StretchDraw(Rect, Temp);
  Temp.Free;
end;

{ Draw BGRA data to a canvas without transparency }
procedure DataDrawOpaqueImplementation(ACanvas: TCanvas; Rect: TRect;
  AData: Pointer; ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer);
var
  Temp:      TBitmap;
  RawImage:  TRawImage;
  BitmapHandle, MaskHandle: HBitmap;
  TempData:  Pointer;
  x, y:      integer;
  PTempData: PByte;
  PSource:   PByte;
  ADataSize: integer;
  ALineEndMargin: integer;
  CreateResult: boolean;
  swapRedBlueWhenCopying: boolean;
  {$IFDEF DARWIN}
  TempShift: Byte;
  {$ENDIF}
begin
  if (AHeight = 0) or (AWidth = 0) then
    exit;

  //by default, we provide data in BGR order
  swapRedBlueWhenCopying := TBGRAPixel_RGBAOrder;

  RawImage.Init;
  RawImage.Description.Init_BPP24_B8G8R8_BIO_TTB(AWidth, AHeight);
{$IFDEF DARWIN}
  //on MacOS, we provide data in RGB order
  TempShift := RawImage.Description.RedShift;
  RawImage.Description.RedShift := RawImage.Description.BlueShift;
  RawImage.Description.BlueShift := TempShift;
  swapRedBlueWhenCopying := not swapRedBlueWhenCopying;
{$ENDIF}
  RawImage.Description.LineOrder := ALineOrder;
  RawImage.Description.LineEnd := rileDWordBoundary;

  ALineEndMargin := (4 - ((AWidth * 3) and 3)) and 3;
  ADataSize      := (AWidth * 3 + ALineEndMargin) * AHeight;

  if integer(RawImage.Description.BytesPerLine) <> AWidth * 3 + ALineEndMargin then
    raise FPImageException.Create('Line size is inconsistant');

  PSource   := AData;
  GetMem({%H-}TempData, ADataSize);
  PTempData := TempData;

  if swapRedBlueWhenCopying then
  begin
    for y := 0 to AHeight - 1 do
    begin
      for x := 0 to AWidth - 1 do
      begin
        PTempData^ := (PSource+2)^;
        (PTempData+1)^ := (PSource+1)^;
        (PTempData+2)^ := PSource^;
        inc(PTempData,3);
        inc(PSource,4);
      end;
      Inc(PTempData, ALineEndMargin);
    end;
  end else
  begin
    for y := 0 to AHeight - 1 do
    begin
      for x := 0 to AWidth - 1 do
      begin
        PWord(PTempData)^ := PWord(PSource)^;
        (PTempData+2)^ := (PSource+2)^;
        Inc(PTempData,3);
        Inc(PSource, 4);
      end;
      Inc(PTempData, ALineEndMargin);
    end;
  end;

  RawImage.Data     := PByte(TempData);
  RawImage.DataSize := ADataSize;

  CreateResult := RawImage_CreateBitmaps(RawImage, BitmapHandle, MaskHandle, False);
  FreeMem(TempData);

  if not CreateResult then
    raise FPImageException.Create('Failed to create bitmap handle');

  Temp := TBitmap.Create;
  Temp.Handle := BitmapHandle;
  Temp.MaskHandle := MaskHandle;
  ACanvas.StretchDraw(Rect, Temp);
  Temp.Free;
end;

procedure GetImageFromCanvasImplementation(ADestination: TBGRADefaultBitmap; CanvasSource: TCanvas; x, y: integer);
var
  bmp: TBitmap;
  subBmp: TBGRACustomBitmap;
  subRect: TRect;
  cw,ch: integer;
begin
  cw := CanvasSource.Width;
  ch := CanvasSource.Height;
  if (x < 0) or (y < 0) or (x+ADestination.Width > cw) or
    (y+ADestination.Height > ch) then
  begin
    ADestination.FillTransparent;
    if (x+ADestination.Width <= 0) or (y+ADestination.Height <= 0) or
      (x >= cw) or (y >= ch) then
      exit;

    if (x > 0) then subRect.Left := x else subRect.Left := 0;
    if (y > 0) then subRect.Top := y else subRect.Top := 0;
    if (x+ADestination.Width > cw) then subRect.Right := cw else
      subRect.Right := x+ADestination.Width;
    if (y+ADestination.Height > ch) then subRect.Bottom := ch else
      subRect.Bottom := y+ADestination.Height;

    subBmp := ADestination.NewBitmap(subRect.Right-subRect.Left,subRect.Bottom-subRect.Top);
    subBmp.GetImageFromCanvas(CanvasSource,subRect.Left,subRect.Top);
    ADestination.PutImage(subRect.Left-x,subRect.Top-y,subBmp,dmSet);
    subBmp.Free;
    exit;
  end;
  bmp := TBitmap.Create;
  bmp.PixelFormat := pf24bit;
  bmp.Width := ADestination.Width;
  bmp.Height := ADestination.Height;
  bmp.Canvas.CopyRect(Classes.rect(0, 0, ADestination.Width, ADestination.Height), CanvasSource,
    Classes.rect(x, y, x + ADestination.Width, y + ADestination.Height));
  LoadFromRawImageImplementation(ADestination, bmp.RawImage, 255, True, False);
  bmp.Free;
  ADestination.InvalidateBitmap;
end;

{ TBGRALCLPtrBitmap }

procedure TBGRALCLPtrBitmap.RebuildBitmap;
var
  RawImage: TRawImage;
  BitmapHandle, MaskHandle: HBitmap;
begin
  if FBitmap <> nil then
    FBitmap.Free;

  FBitmap := TBitmapTracker.Create(self);

  if (FWidth > 0) and (FHeight > 0) then
  begin
    RawImage.Init;
    if TBGRAPixel_RGBAOrder then
      RawImage.Description.Init_BPP32_R8G8B8A8_BIO_TTB(FWidth, FHeight)
    else
      RawImage.Description.Init_BPP32_B8G8R8A8_BIO_TTB(FWidth, FHeight);
    RawImage.Description.LineOrder := FLineOrder;
    RawImage.Data     := PByte(FData);
    RawImage.DataSize := FWidth * FHeight * sizeof(TBGRAPixel);
    if not RawImage_CreateBitmaps(RawImage, BitmapHandle, MaskHandle, False) then
      raise FPImageException.Create('Failed to create bitmap handle');
    FBitmap.Handle     := BitmapHandle;
    FBitmap.MaskHandle := MaskHandle;
  end;

  FBitmap.Canvas.AntialiasingMode := amOff;
  FBitmapModified := False;
end;

function TBGRALCLPtrBitmap.CreatePtrBitmap(AWidth, AHeight: integer;
  AData: PBGRAPixel): TBGRAPtrBitmap;
begin
  Result:= TBGRALCLPtrBitmap.Create(AWidth,AHeight,AData);
end;

function TBGRALCLPtrBitmap.CreateDefaultFontRenderer: TBGRACustomFontRenderer;
begin
  result := TLCLFontRenderer.Create;
end;

function TBGRALCLPtrBitmap.LoadFromRawImage(ARawImage: TRawImage;
  DefaultOpacity: byte; AlwaysReplaceAlpha: boolean;
  RaiseErrorOnInvalidPixelFormat: boolean): boolean;
begin
  DiscardBitmapChange;
  result := LoadFromRawImageImplementation(self,ARawImage,DefaultOpacity,AlwaysReplaceAlpha,RaiseErrorOnInvalidPixelFormat);
end;

procedure TBGRALCLPtrBitmap.GetImageFromCanvas(CanvasSource: TCanvas; x,
  y: integer);
begin
  DiscardBitmapChange;
  GetImageFromCanvasImplementation(self,CanvasSource,x,y);
end;

procedure TBGRALCLPtrBitmap.DataDrawTransparent(ACanvas: TCanvas; Rect: TRect;
  AData: Pointer; ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer);
begin
  DataDrawTransparentImplementation(ACanvas, Rect, AData, ALineOrder, AWidth, AHeight);
end;

procedure TBGRALCLPtrBitmap.DataDrawOpaque(ACanvas: TCanvas; Rect: TRect;
  AData: Pointer; ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer);
begin
  DataDrawOpaqueImplementation(ACanvas, Rect, AData, ALineOrder, AWidth, AHeight);
end;

function TBGRALCLBitmap.LoadFromRawImage(ARawImage: TRawImage;
  DefaultOpacity: byte; AlwaysReplaceAlpha: boolean;
  RaiseErrorOnInvalidPixelFormat: boolean): boolean;
begin
  DiscardBitmapChange;
  result := LoadFromRawImageImplementation(self,ARawImage,DefaultOpacity,AlwaysReplaceAlpha,RaiseErrorOnInvalidPixelFormat);
end;

function TBGRALCLBitmap.CreateDefaultFontRenderer: TBGRACustomFontRenderer;
begin
  result := TLCLFontRenderer.Create;
end;

procedure TBGRALCLBitmap.DoLoadFromBitmap;
begin
  if FBitmap <> nil then
    LoadFromRawImage(FBitmap.RawImage, FCanvasOpacity);
end;

procedure TBGRALCLBitmap.RebuildBitmap;
var
  RawImage: TRawImage;
  BitmapHandle, MaskHandle: HBitmap;
begin
  if FBitmap <> nil then
    FBitmap.Free;

  FBitmap := TBitmapTracker.Create(self);

  if (FWidth > 0) and (FHeight > 0) then
  begin
    RawImage.Init;
    if TBGRAPixel_RGBAOrder then
      RawImage.Description.Init_BPP32_R8G8B8A8_BIO_TTB(FWidth, FHeight)
    else
      RawImage.Description.Init_BPP32_B8G8R8A8_BIO_TTB(FWidth, FHeight);
    RawImage.Description.LineOrder := FLineOrder;
    RawImage.Data     := PByte(FData);
    RawImage.DataSize := FWidth * FHeight * sizeof(TBGRAPixel);
    if not RawImage_CreateBitmaps(RawImage, BitmapHandle, MaskHandle, False) then
      raise FPImageException.Create('Failed to create bitmap handle');
    FBitmap.Handle     := BitmapHandle;
    FBitmap.MaskHandle := MaskHandle;
  end;

  FBitmap.Canvas.AntialiasingMode := amOff;
  FBitmapModified := False;
end;

function TBGRALCLBitmap.CreatePtrBitmap(AWidth, AHeight: integer;
  AData: PBGRAPixel): TBGRAPtrBitmap;
begin
  Result:= TBGRALCLPtrBitmap.Create(AWidth, AHeight, AData);
end;

procedure TBGRALCLBitmap.Assign(Source: TPersistent);
begin
  if Source is TRasterImage then
  begin
    AssignRasterImage(TRasterImage(Source));
  end else
    inherited Assign(Source);
end;

procedure TBGRALCLBitmap.AssignRasterImage(ARaster: TRasterImage);
var TempBmp: TBitmap;
    ConvertOk: boolean;
begin
  DiscardBitmapChange;
  SetSize(ARaster.Width, ARaster.Height);
  if not LoadFromRawImage(ARaster.RawImage,0,False,False) then
  if ARaster is TBitmap then
  begin //try to convert
    TempBmp := TBitmap.Create;
    TempBmp.Width := ARaster.Width;
    TempBmp.Height := ARaster.Height;
    TempBmp.Canvas.Draw(0,0,ARaster);
    ConvertOk := LoadFromRawImage(TempBmp.RawImage,0,False,False);
    TempBmp.Free;
    if not ConvertOk then
      raise Exception.Create('Unable to convert image to 24 bit');
  end else
    raise Exception.Create('Unable to convert image to 24 bit');
  If Empty then AlphaFill(255); // if bitmap seems to be empty, assume
                                // it is an opaque bitmap without alpha channel
end;

procedure TBGRALCLBitmap.DataDrawTransparent(ACanvas: TCanvas; Rect: TRect;
  AData: Pointer; ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer);
begin
  DataDrawTransparentImplementation(ACanvas, Rect, AData, ALineOrder, AWidth, AHeight);
end;

procedure TBGRALCLBitmap.DataDrawOpaque(ACanvas: TCanvas; Rect: TRect;
  AData: Pointer; ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer);
begin
  DataDrawOpaqueImplementation(ACanvas, Rect, AData, ALineOrder, AWidth, AHeight);
end;

procedure TBGRALCLBitmap.GetImageFromCanvas(CanvasSource: TCanvas; x, y: integer
  );
begin
  DiscardBitmapChange;
  GetImageFromCanvasImplementation(self,CanvasSource,x,y);
end;

procedure TBGRALCLBitmap.LoadFromDevice(DC: System.THandle);
var
  rawImage: TRawImage;
  sourceSize: TPoint;
begin
  sourceSize := Point(0,0);
  GetDeviceSize(DC, sourceSize);
  if (sourceSize.x = 0) or (sourceSize.y = 0) then
  begin
    SetSize(0,0);
    exit;
  end;
  try
    if not RawImage_FromDevice(rawImage, DC, rect(0,0,sourceSize.x,sourceSize.y)) then
      raise Exception.Create('Cannot get raw image from device');
    SetSize(rawImage.Description.Width, rawImage.Description.Height);
    LoadFromRawImage(rawImage,255);
  finally
    rawImage.FreeData;
  end;
end;

procedure TBGRALCLBitmap.LoadFromDevice(DC: System.THandle; ARect: TRect);
var
  rawImage: TRawImage;
begin
  if (ARect.Right <= ARect.Left) or (ARect.Bottom <= ARect.Top) then
  begin
    SetSize(0,0);
    exit;
  end;
  try
    if not RawImage_FromDevice(rawImage, DC, ARect) then
      raise Exception.Create('Cannot get raw image from device');
    SetSize(rawImage.Description.Width, rawImage.Description.Height);
    LoadFromRawImage(rawImage,255);
  finally
    rawImage.FreeData;
  end;
end;

procedure TBGRALCLBitmap.TakeScreenshotOfPrimaryMonitor;
var primaryDC: THandle;
begin
  primaryDC := LCLIntf.GetDC(0);
  LoadFromDevice(primaryDC);
  LCLIntf.ReleaseDC(0, primaryDC);
end;

procedure TBGRALCLBitmap.TakeScreenshot(ARect: TRect);
var primaryDC: THandle;
begin
  primaryDC := LCLIntf.GetDC(0);
  LoadFromDevice(primaryDC, ARect);
  LCLIntf.ReleaseDC(0, primaryDC);
end;

end.

