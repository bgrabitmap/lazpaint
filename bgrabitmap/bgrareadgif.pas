{ This unit provides some optimisations of TFPReaderGif: decompression algorithm and direct pixel access of TBGRABitmap.
  Note: to read an animation use TBGRAAnimatedGif instead. }

unit BGRAReadGif;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPimage, FPReadGif;

type
  PGifRGB = ^TGifRGB;

  { TBGRAReaderGif }

  TBGRAReaderGif = class(TFPReaderGif)
  protected
    procedure ReadPaletteAtOnce(Stream: TStream; Size: integer);
    procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
    function ReadScanLine(Stream: TStream): boolean; override;
    function WriteScanLineBGRA(Img: TFPCustomImage): Boolean; virtual;
  end;

implementation

uses BGRABitmapTypes;

{ TBGRAReaderGif }

procedure TBGRAReaderGif.ReadPaletteAtOnce(Stream: TStream; Size: integer);
Var
  RGBEntries, RGBEntry : PGifRGB;
  I : Integer;
  c : TFPColor;
begin
  FPalette.count := 0;
  getmem(RGBEntries, sizeof(TGifRGB)*Size);
  Stream.Read(RGBEntries^, sizeof(TGifRGB)*Size);
  For I:=0 To Size-1 Do
  Begin
    RGBEntry := RGBEntries+I;
    With c do
    begin
      Red:=RGBEntry^.Red or (RGBEntry^.Red shl 8);
      Green:=RGBEntry^.Green or (RGBEntry^.Green shl 8);
      Blue:=RGBEntry^.Blue or (RGBEntry^.Blue shl 8);
      Alpha:=alphaOpaque;
    end;
    FPalette.Add(C);
  End;
  FreeMem(RGBEntries);
end;

procedure TBGRAReaderGif.InternalRead(Stream: TStream; Img: TFPCustomImage);
var
  Introducer:byte;
  ColorTableSize :Integer;
  ContProgress: Boolean;
begin
  FPalette:=nil;
  FScanLine:=nil;
  try
    ContProgress:=true;
    Progress(psStarting, 0, False, Rect(0,0,0,0), '', ContProgress);
    if not ContProgress then exit;

    FPalette := TFPPalette.Create(0);

    Stream.Position:=0;
    // header
    Stream.Read(FHeader,SizeOf(FHeader));
    Progress(psRunning, 0, False, Rect(0,0,0,0), '', ContProgress);
    if not ContProgress then exit;

    // Endian Fix Mantis 8541. Gif is always little endian
    {$IFDEF ENDIAN_BIG}
      with FHeader do
        begin
          ScreenWidth := LEtoN(ScreenWidth);
          ScreenHeight := LEtoN(ScreenHeight);
        end;
    {$ENDIF}
    // global palette
    if (FHeader.Packedbit and $80) <> 0 then
    begin
      ColorTableSize := FHeader.Packedbit and 7 + 1;
      ReadPaletteAtOnce(stream, 1 shl ColorTableSize);
    end;

    // skip extensions
    Repeat
      Introducer:=SkipBlock(Stream);
    until (Introducer = $2C) or (Introducer = $3B);

    // descriptor
    Stream.Read(FDescriptor, SizeOf(FDescriptor));
    {$IFDEF ENDIAN_BIG}
      with FDescriptor do
        begin
          Left := LEtoN(Left);
          Top := LEtoN(Top);
          Width := LEtoN(Width);
          Height := LEtoN(Height);
        end;
    {$ENDIF}
    // local palette
    if (FDescriptor.Packedbit and $80) <> 0 then
    begin
      ColorTableSize := FDescriptor.Packedbit and 7 + 1;
      ReadPaletteAtOnce(stream, 1 shl ColorTableSize);
    end;

    // parse header
    if not AnalyzeHeader then exit;

    // create image
    if Assigned(OnCreateImage) then
      OnCreateImage(Self,Img);
    Img.SetSize(FWidth,FHeight);

    // read pixels
    if not ReadScanLine(Stream) then exit;
    if Img is TBGRACustomBitmap then
    begin
      if not WriteScanLineBGRA(Img) then exit;
    end else
      if not WriteScanLine(Img) then exit;

    // ToDo: read further images
  finally
    FreeAndNil(FPalette);
    ReAllocMem(FScanLine,0);
  end;
  Progress(FPimage.psEnding, 100, false, Rect(0,0,FWidth,FHeight), '', ContProgress);
end;

function TBGRAReaderGif.ReadScanLine(Stream: TStream): Boolean;
var
  OldPos,
  UnpackedSize,
  PackedSize:longint;
  I: Integer;
  Data,
  Bits,
  Code: Cardinal;
  SourcePtr: PByte;
  InCode: Cardinal;

  CodeSize: Cardinal;
  CodeMask: Cardinal;
  FreeCode: Cardinal;
  OldCode: Cardinal;
  Prefix: array[0..4095] of Cardinal;
  Suffix,
  Stack: array [0..4095] of Byte;
  StackPointer, StackTop: PByte;
  StackSize: integer;
  DataComp,
  Target: PByte;
  {%H-}B,
  {%H-}FInitialCodeSize,
  FirstChar: Byte;
  ClearCode,
  EOICode: Word;
  ContProgress: Boolean;

begin
  DataComp:=nil;
  ContProgress:=true;
  try
    // read dictionary size
    Stream.read({%H-}FInitialCodeSize, 1);

    // search end of compressor table
    OldPos:=Stream.Position;
    PackedSize := 0;
    Repeat
      Stream.read({%H-}B, 1);
      if B > 0 then
      begin
        inc(PackedSize, B);
        Stream.Seek(B, soFromCurrent);
      end;
    until B = 0;

    Progress(psRunning, trunc(100.0 * (Stream.position / Stream.size)),
             False, Rect(0,0,0,0), '', ContProgress);
    if not ContProgress then exit(false);

    Getmem(DataComp, PackedSize);
    // read compressor table
    SourcePtr:=DataComp;
    Stream.Position:=OldPos;
    Repeat
      Stream.read(B, 1);
      if B > 0 then
      begin
         Stream.ReadBuffer(SourcePtr^, B);
         Inc(SourcePtr,B);
      end;
    until B = 0;

    Progress(psRunning, trunc(100.0 * (Stream.position / Stream.size)),
             False, Rect(0,0,0,0), '', ContProgress);
    if not ContProgress then exit(false);

    SourcePtr:=DataComp;
    Target := FScanLine;
    CodeSize := FInitialCodeSize + 1;
    ClearCode := 1 shl FInitialCodeSize;
    EOICode := ClearCode + 1;
    FreeCode := ClearCode + 2;
    OldCode := 4096;
    CodeMask := (1 shl CodeSize) - 1;
    UnpackedSize:=FWidth * FHeight;
    for I := 0 to ClearCode - 1 do
    begin
      Prefix[I] := 4096;
      Suffix[I] := I;
    end;
    StackTop := @Stack[high(Stack)];
    StackPointer := StackTop;
    FirstChar := 0;
    Data := 0;
    Bits := 0;
    // LZW decompression gif
    while (UnpackedSize > 0) and (PackedSize > 0) do
    begin
      Inc(Data, SourcePtr^ shl Bits);
      Inc(Bits, 8);
      while Bits >= CodeSize do
      begin
        Code := Data and CodeMask;
        Data := Data shr CodeSize;
        Dec(Bits, CodeSize);
        if Code = EOICode then Break;
        if Code = ClearCode then
        begin
          CodeSize := FInitialCodeSize + 1;
          CodeMask := (1 shl CodeSize) - 1;
          FreeCode := ClearCode + 2;
          OldCode := 4096;
          Continue;
        end;
        if Code > FreeCode then Break;
        if OldCode = 4096 then
        begin
          FirstChar := Suffix[Code];
          Target^ := FirstChar;
          Inc(Target);
          Dec(UnpackedSize);
          OldCode := Code;
          Continue;
        end;
        InCode := Code;
        if Code = FreeCode then
        begin
          StackPointer^ := FirstChar;
          dec(StackPointer);
          Code := OldCode;
        end;
        while Code > ClearCode do
        begin
          StackPointer^ := Suffix[Code];
          dec(StackPointer);
          Code := Prefix[Code];
        end;
        FirstChar := Suffix[Code];
        StackPointer^ := FirstChar;
        dec(StackPointer);
        Prefix[FreeCode] := OldCode;
        Suffix[FreeCode] := FirstChar;
        if (FreeCode = CodeMask) and
           (CodeSize < 12) then
        begin
          Inc(CodeSize);
          CodeMask := (1 shl CodeSize) - 1;
        end;
        if FreeCode < 4095 then Inc(FreeCode);
        OldCode := InCode;
        StackSize := StackTop-StackPointer;
        if StackSize > 0 then
        begin
          Move((StackPointer+1)^, Target^, StackSize);
          inc(Target, StackSize);
          StackPointer:= StackTop;
          dec(UnpackedSize, StackSize);
        end;
      end;
      Inc(SourcePtr);
      Dec(PackedSize);
    end;
    Progress(psRunning, trunc(100.0 * (Stream.position / Stream.size)),
             False, Rect(0,0,0,0), '', ContProgress);
    if not ContProgress then exit(false);
  finally
    if DataComp<>nil then
      FreeMem(DataComp);
  end;
  Result:=true;
end;

function TBGRAReaderGif.WriteScanLineBGRA(Img: TFPCustomImage): Boolean;
Var
  Row, Col,i : Integer;
  Pass, Every : byte;
  P : PByte;
  PBGRAPalette: PBGRAPixel;
  PDest: PBGRAPixel;
  function IsMultiple(NumberA, NumberB: Integer): Boolean;
  begin
    Result := (NumberA >= NumberB) and
              (NumberB > 0) and
              (NumberA mod NumberB = 0);
  end;
begin
  Result:=false;
  P:=FScanLine;
  getmem(PBGRAPalette, (FPalette.Count)*sizeof(TBGRAPixel));
  for i := 0 to FPalette.Count-1 do PBGRAPalette[i] := FPColorToBGRA(FPalette.Color[i]);
  If FInterlace then
  begin
    For Pass := 1 to 4 do
    begin
      Case Pass of
         1 : begin
               Row := 0;
               Every := 8;
             end;
         2 : begin
               Row := 4;
               Every := 8;
             end;
         3 : begin
               Row := 2;
               Every := 4;
             end;
         else{4}
             begin
               Row := 1;
               Every := 2;
             end;
        end;
      Repeat
        PDest := TBGRACustomBitmap(Img).ScanLine[Row];
        for Col:=Img.Width-1 downto 0 do
        begin
          PDest^ := PBGRAPalette[P^];
          Inc(P);
          Inc(PDest);
        end;
        Inc(Row, Every);
      until Row >= Img.Height;
    end;
  end
  else
  begin
    for Row:=0 to Img.Height-1 do
    begin
      PDest := TBGRACustomBitmap(Img).ScanLine[Row];
      for Col:=Img.Width-1 downto 0 do
      begin
        PDest^ := PBGRAPalette[P^];
        Inc(P);
        Inc(PDest);
      end;
    end;
  end;
  FreeMem(PBGRAPalette);
  Result:=true;
end;


initialization

  DefaultBGRAImageReader[ifGif] := TBGRAReaderGif;

end.
