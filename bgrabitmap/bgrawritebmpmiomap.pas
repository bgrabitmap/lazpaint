unit BGRAWriteBmpMioMap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPimage, BGRABitmapTypes, BGRAReadBmpMioMap;

type

  { TBGRAWriterBmpMioMap }

  TBGRAWriterBmpMioMap = class (TFPCustomImageWriter)
  protected
    FHeader: TMioHeader;
    FPalette: packed array of record
        ColorValue: Word;
        AlphaValue: Byte;
        Padding: Byte;
      end;
    FPaletteIndexes: packed array[0..65535] of NativeInt;
    FPaletteOffset: NativeInt;
    FPaletteAlpha: boolean;
    FChunks: array of TMemoryStream;
    FCurrentChunk: TMemoryStream;
    FMaxChunkSize: Word;
    function IndexOfColor(const AColor: TBGRAPixel): NativeInt;
    procedure InitHeader(Img: TFPCustomImage);
    procedure InitPalette;
    procedure InitChunks;
    procedure FlushChunk;
    procedure FreeChunks;
    procedure NeedChunk;
    procedure AppendToChunks(const Buffer; Count: integer);
    procedure BuildPaletteAndChunks(Img: TFPCustomImage);
    procedure WriteHeader(Str: TStream);
    procedure WritePalette(Str: TStream);
    procedure WriteChunks(Str: TStream);
    procedure ReadScanline(Img: TFPCustomImage; Y: integer; ADest: PBGRAPixel);
    procedure InternalWrite(Str: TStream; Img: TFPCustomImage); override;
  public
    constructor Create; override;
    property MaxChunkSize: Word read FMaxChunkSize write FMaxChunkSize;
  end;

implementation

{ TBGRAWriterBmpMioMap }

function TBGRAWriterBmpMioMap.IndexOfColor(const AColor: TBGRAPixel): NativeInt;
var searchedColorValue: Word;
  searchedAlphaValue: Byte;
  i,startSearch,endSearch: NativeInt;
begin
  searchedColorValue:= BGRAToMioMap(AColor);
  searchedAlphaValue:= AlphaToMioMap(AColor.alpha);
  if length(FPalette)>0 then
  begin
    with FPalette[0] do
    begin
      if (ColorValue = searchedColorValue) and
        (AlphaValue = searchedAlphaValue) then
      begin
        result := 0;
        exit;
      end;
    end;
  end;

  startSearch:= FPaletteOffset+1;
  endSearch:= FPaletteOffset+$FC;
  if endSearch >= FHeader.nbColors then
    endSearch:= FHeader.nbColors-1;
  for i := startSearch to endSearch do
  with FPalette[i] do
  begin
    if (ColorValue = searchedColorValue)
    and (AlphaValue = searchedAlphaValue) then
    begin
      result := i;
      exit;
    end;
  end;

  result := FPaletteIndexes[searchedColorValue];
  if (result <> -1) and (FPalette[result].AlphaValue <> searchedAlphaValue) then
    result := -1;

  if result = -1 then
  begin
    if fheader.nbColors = 65535 then
      raise exception.Create('Too many colors');
    result := fheader.nbColors;
    inc(FHeader.nbColors);
    if length(FPalette) <= result then
      setlength(FPalette, length(FPalette)*2 + 128);
    with FPalette[result] do
    begin
      ColorValue := searchedColorValue;
      AlphaValue := searchedAlphaValue;
    end;
    FPaletteIndexes[searchedColorValue] := result;
    if (searchedAlphaValue > 0) and (searchedAlphaValue < 32) then
      FPaletteAlpha := true;
  end;
end;

procedure TBGRAWriterBmpMioMap.InitHeader(Img: TFPCustomImage);
begin
  if (Img.Width > 65535) or (Img.Height > 65535) then
    raise exception.Create('Image too big to be saved in Bmp MioMap format');
  FHeader.magic := MioMapMagicValue;
  fheader.format:= 0;
  FHeader.width := Img.Width;
  FHeader.height := img.Height;
  FHeader.nbColors := 0;
  FHeader.nbChunks := 0;
end;

procedure TBGRAWriterBmpMioMap.InitPalette;
var i: NativeInt;
begin
  for i := 0 to high(FPaletteIndexes) do
    FPaletteIndexes[i] := -1;
  FPaletteOffset := 0;
  FPaletteAlpha := false;
  IndexOfColor(BGRAPixelTransparent); //define transparent color as zero
end;

procedure TBGRAWriterBmpMioMap.InitChunks;
begin
  FCurrentChunk := nil;
end;

procedure TBGRAWriterBmpMioMap.FlushChunk;
begin
  if FCurrentChunk <> nil then
  begin
    setlength(FChunks, length(FChunks)+1);
    FChunks[high(FChunks)] := FCurrentChunk;
    FCurrentChunk := nil;
    FHeader.nbChunks += 1;
  end;
end;

procedure TBGRAWriterBmpMioMap.FreeChunks;
var
  i: Integer;
begin
  FreeAndNil(FCurrentChunk);
  for i := 0 to high(FChunks) do
    FChunks[i].Free;
  FChunks := nil;
end;

procedure TBGRAWriterBmpMioMap.NeedChunk;
begin
  if FCurrentChunk = nil then
  begin
    if FHeader.nbChunks = 65535 then
      raise exception.Create('Too many chunks');
    FCurrentChunk := TMemoryStream.Create;
  end;
end;

procedure TBGRAWriterBmpMioMap.AppendToChunks(const Buffer; Count: integer);
begin
  if Count > 65535 then
    raise exception.Create('Buffer too big');
  NeedChunk;
  if FCurrentChunk.Size + Count > MaxChunkSize then
  begin
    FlushChunk;
    NeedChunk;
  end;
  FCurrentChunk.WriteBuffer(Buffer,Count);
end;

procedure TBGRAWriterBmpMioMap.BuildPaletteAndChunks(Img: TFPCustomImage);
var y,w: NativeInt;
  PData,PDataEnd: PBGRAPixel;
  p: PBGRAPixel;
  currentColorIndex,
  nextColorIndex,
  repCount: NativeInt;
  b: byte;
  changeOfsRec: packed record
      valFD: byte;
      valLo: byte;
      valHi: byte;
  end;
  repRec: packed record
      valFE: byte;
      relativeColorIndex: byte;
      count: byte;
  end;
  repZeroRec: packed record
        valFF: byte;
        count: byte;
    end;

begin
  w := Img.Width;
  getmem(PData, w*sizeof(TBGRAPixel));
  try
    PDataEnd := PData+w;
    for y := 0 to Img.Height-1 do
    begin
      ReadScanline(Img,Y,PData);
      p := PData;
      while p < PDataEnd do
      begin
        currentColorIndex:= IndexOfColor(p^);
        nextColorIndex := currentColorIndex;
        repCount:= 1;
        inc(p);
        while p < PDataEnd do
        begin
          nextColorIndex:= IndexOfColor(p^);
          if nextColorIndex = currentColorIndex then
          begin
            inc(p);
            inc(repCount);
            if repCount = 255 then break;
          end
          else
            break;
        end;
        if currentColorIndex = 0 then
        begin
          if repCount = 1 then
          begin
            b := 0;
            AppendToChunks(b,1);
          end else
          begin
            repZeroRec.valFF := $ff;
            repZeroRec.count := repCount;
            AppendToChunks(repZeroRec, sizeof(repZeroRec));
          end;
        end else
        begin
          if (currentColorIndex < FPaletteOffset+1)
            or (currentColorIndex > FPaletteOffset+$FC) then
          begin
            if (abs(nextColorIndex-currentColorIndex) < $FC) then
            begin
              FPaletteOffset := (nextColorIndex+currentColorIndex) div 2 - 126;
            end else
              FPaletteOffset := currentColorIndex-126;
            if FPaletteOffset < 0 then FPaletteOffset := 0;
            changeOfsRec.valFD := $fd;
            changeOfsRec.valLo := FPaletteOffset and 255;
            changeOfsRec.valHi := FPaletteOffset shr 8;
            AppendToChunks(changeOfsRec,sizeof(changeOfsRec));
          end;
          if (currentColorIndex < FPaletteOffset+1)
            or (currentColorIndex > FPaletteOffset+$FC) then
              raise exception.Create('Index out of range');
          if repCount = 1 then
          begin
            b := currentColorIndex-FPaletteOffset;
            AppendToChunks(b,1);
          end else
          if repCount = 2 then
          begin
            b := currentColorIndex-FPaletteOffset;
            AppendToChunks(b,1);
            AppendToChunks(b,1);
          end else
          begin
            repRec.valFE:= $FE;
            repRec.count := repCount;
            repRec.relativeColorIndex := currentColorIndex-FPaletteOffset;
            AppendToChunks(repRec, sizeof(repRec));
          end;
        end;
      end;
      FlushChunk;
    end;
  finally
    freemem(PData);
  end;
end;

procedure TBGRAWriterBmpMioMap.WriteChunks(Str: TStream);
var
  bigChunkDef: packed record
      val255: byte;
      valHi: byte;
      valLo: byte;
  end;
  i: NativeInt;
begin
  for i := 0 to high(FChunks) do
  begin
    if FChunks[i].Size > 254 then
    begin
      bigChunkDef.val255 := 255;
      bigChunkDef.valHi := FChunks[i].Size shr 8;
      bigChunkDef.valLo := FChunks[i].Size and 255;
      Str.WriteBuffer(bigChunkDef, sizeof(bigChunkDef));
    end else
      Str.WriteByte(FChunks[i].Size);
  end;
  for i := 0 to high(FChunks) do
  begin
    FChunks[i].Position := 0;
    if Str.CopyFrom(FChunks[i],FChunks[i].Size) <> FChunks[i].Size then
      raise exception.Create('Unable to write chunk');
  end;
end;

procedure TBGRAWriterBmpMioMap.WriteHeader(Str: TStream);
var header: TMioHeader;
begin
  if FPaletteAlpha then FHeader.format := 1;
  FlushChunk;

  header := FHeader;
  header.format:= NtoLE(header.format);
  header.width:= NtoLE(header.width);
  header.height:= NtoLE(header.height);
  header.nbColors:= NtoLE(header.nbColors);
  header.nbChunks:= NtoLE(header.nbChunks);
  Str.WriteBuffer(header, sizeof(header));
end;

procedure TBGRAWriterBmpMioMap.WritePalette(Str: TStream);
var
  colors: packed array of Word;
  alphas: packed array of byte;
  i: NativeInt;
begin
  setlength(Colors, FHeader.nbColors);
  for i := 0 to FHeader.nbColors-1 do
    colors[i] := NtoLE(FPalette[i].ColorValue);
  Str.WriteBuffer(colors[0], length(Colors)*sizeof(word));
  if FPaletteAlpha then
  begin
    setlength(alphas, FHeader.nbColors);
    for i := 0 to FHeader.nbColors-1 do
      alphas[i] := FPalette[i].AlphaValue;
    Str.WriteBuffer(alphas[0], length(alphas)*sizeof(byte));
  end;
end;

procedure TBGRAWriterBmpMioMap.ReadScanline(Img: TFPCustomImage; Y: integer;
  ADest: PBGRAPixel);
var
  i: NativeInt;
begin
  if Img is TBGRACustomBitmap then
    Move(TBGRACustomBitmap(Img).ScanLine[Y]^, ADest^, Img.Width*sizeof(TBGRAPixel))
  else
  begin
    for i := 0 to Img.Width-1 do
      (ADest+i)^ := FPColorToBGRA(Img.Colors[y,i]);
  end;
end;

procedure TBGRAWriterBmpMioMap.InternalWrite(Str: TStream; Img: TFPCustomImage);
begin
  try
    InitHeader(Img);
    InitPalette;
    InitChunks;
    BuildPaletteAndChunks(Img);
    WriteHeader(Str);
    WritePalette(Str);
    WriteChunks(Str);
  finally
    FreeChunks;
  end;
end;

constructor TBGRAWriterBmpMioMap.Create;
begin
  inherited Create;
  MaxChunkSize := 254;
end;

initialization

  DefaultBGRAImageWriter[ifBmpMioMap] := TBGRAWriterBmpMioMap;

end.

