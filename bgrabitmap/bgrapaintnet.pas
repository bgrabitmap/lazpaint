unit BGRAPaintNet;

{$mode objfpc}{$H+}

interface

{ This unit reads Paint.NET files. It needs BGRADNetDeserial to deserialize binary .Net objects.

  A Paint.NET image consists in three parts :
  - Xml header
  - Binary serialized information (contains layer information)
  - Compressed data (pixel data)

  The class TPaintDotNetFile do not read the Xml header. ComputeFlatImage builds the resulting image
  by using blending operations to merge layers.

  The unit registers a TFPCustomImageReader so that it can be read by any image reading function of FreePascal,
  and also registers a reader for BGRALayers }

uses
  Classes, SysUtils, BGRADNetDeserial, FPImage, BGRABitmapTypes, BGRABitmap, BGRALayers;

type

  { TPaintDotNetFile }

  TPaintDotNetFile = class(TBGRACustomLayeredBitmap)
  public
    procedure LoadFromFile(const filenameUTF8: string); override;
    procedure LoadFromStream(stream: TStream); override;
    procedure Clear; override;
    function ToString: ansistring; override;
    function GetLayerBitmapCopy(layer: integer): TBGRABitmap; override;
    constructor Create; override;
  protected
    procedure InternalLoadFromStream(stream: TStream);
    function GetWidth: integer; override;
    function GetHeight: integer; override;
    function GetNbLayers: integer; override;
    function GetBlendOperation(Layer: integer): TBlendOperation; override;
    function GetLayerVisible(layer: integer): boolean; override;
    function GetLayerOpacity(layer: integer): byte; override;
    function GetLayerName(layer: integer): string; override;
  private
    Content:   TDotNetDeserialization;
    Document:  TSerializedClass;
    Layers:    TSerializedClass;
    LayerData: array of TMemoryStream;
    function InternalGetLayer(num: integer): TSerializedClass;
    function InternalGetBlendOperation(layer: TSerializedClass): TBlendOperation;
    function InternalGetLayerName(layer: TSerializedClass): string;
    function InternalGetLayerVisible(layer: TSerializedClass): boolean;
    function InternalGetLayerOpacity(layer: TSerializedClass): byte;
    function LayerDataSize(numLayer: integer): int64;
    procedure LoadLayer(dest: TMemoryStream; src: TStream; uncompressedSize: int64);
  end;

  { TFPReaderPaintDotNet }

  TFPReaderPaintDotNet = class(TFPCustomImageReader)
    private
      FWidth,FHeight,FNbLayers: integer;
    protected
      function InternalCheck(Stream: TStream): boolean; override;
      procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
    public
      property Width: integer read FWidth;
      property Height: integer read FHeight;
      property NbLayers: integer read FNbLayers;
  end;

function IsPaintDotNetFile(filename: string): boolean;
function IsPaintDotNetFileUTF8(filenameUTF8: string): boolean;
function IsPaintDotNetStream(stream: TStream): boolean;
function LoadPaintDotNetFile(filename: string): TBGRABitmap;
function LoadPaintDotNetFileUTF8(filenameUTF8: string): TBGRABitmap;

procedure RegisterPaintNetFormat;

implementation

uses zstream, Math, graphtype, Graphics, lazutf8classes, FileUtil;

{$hints off}
function BEReadLongword(Stream: TStream): longword;
begin
  Stream.Read(Result, sizeof(Result));
  Result := BEtoN(Result);
end;

{$hints on}

{$hints off}
function BEReadLongint(Stream: TStream): longint;
begin
  Stream.Read(Result, sizeof(Result));
  Result := BEtoN(Result);
end;

function IsPaintDotNetFile(filename: string): boolean;
var
  stream: TFileStream;
begin
  Result := False;
  if FileExists(filename) then
  begin
    stream := TFileStream.Create(filename, fmOpenRead);
    Result := IsPaintDotNetStream(stream);
    stream.Free;
  end;
end;

function IsPaintDotNetFileUTF8(filenameUTF8: string): boolean;
var
  stream: TFileStreamUTF8;
begin
  Result := False;
  if FileExistsUTF8(filenameUTF8) then
  begin
    stream := TFileStreamUTF8.Create(filenameUTF8, fmOpenRead);
    Result := IsPaintDotNetStream(stream);
    stream.Free;
  end;
end;

function IsPaintDotNetStream(stream: TStream): boolean;
var
  header:  packed array[0..3] of char;
  SavePos: int64;
begin
  Result := False;
  try
    if stream.Position + 4 < Stream.Size then
    begin
      header  := #0#0#0#0;
      SavePos := stream.Position;
      stream.Read(header, 4);
      stream.Position := SavePos;
      if (header[0] = 'P') and (header[1] = 'D') and (header[2] = 'N') and
        (header[3] = '3') then
        Result := True;
    end;
  except
    on ex: Exception do ;
  end;
end;

function LoadPaintDotNetFile(filename: string): TBGRABitmap;
begin
  result := LoadPaintDotNetFileUTF8(SysToUTF8(filename));
end;

function LoadPaintDotNetFileUTF8(filenameUTF8: string): TBGRABitmap;
var
  pdn: TPaintDotNetFile;
begin
  pdn    := TPaintDotNetFile.Create;
  Result := nil;
  try
    pdn.LoadFromFile(filenameUTF8);
    Result := pdn.ComputeFlatImage;
    pdn.Free;
  except
    on ex: Exception do
    begin
      FreeAndNil(Result);
      pdn.Free;
      raise Exception.Create('Error while loading Paint.NET file. ' + ex.Message);
    end;
  end;
end;

function LoadPaintDotNetStream(stream: TStream): TBGRABitmap;
var
  pdn: TPaintDotNetFile;
begin
  pdn    := TPaintDotNetFile.Create;
  Result := nil;
  try
    pdn.LoadFromStream(stream);
    Result := pdn.ComputeFlatImage;
    pdn.Free;
  except
    on ex: Exception do
    begin
      FreeAndNil(Result);
      pdn.Free;
      raise Exception.Create('Error while loading Paint.NET stream. ' + ex.Message);
    end;
  end;
end;

{$hints on}

{ TFPReaderPaintDotNet }

function TFPReaderPaintDotNet.InternalCheck(Stream: TStream): boolean;
begin
  result := IsPaintDotNetStream(stream);
end;

procedure TFPReaderPaintDotNet.InternalRead(Stream: TStream; Img: TFPCustomImage);
var
  pdn: TPaintDotNetFile;
  flat: TBGRABitmap;
  x,y: integer;
begin
  FWidth := 0;
  FHeight:= 0;
  FNbLayers:= 0;
  pdn    := TPaintDotNetFile.Create;
  try
    pdn.LoadFromStream(Stream);
    flat := pdn.ComputeFlatImage;
    try
      FWidth:= pdn.Width;
      FHeight:= pdn.Height;
      FNbLayers:= pdn.NbLayers;

      if Img is TBGRACustomBitmap then
        TBGRACustomBitmap(Img).Assign(flat) else
      begin
        Img.SetSize(pdn.Width,pdn.Height);
        for y := 0 to pdn.Height-1 do
          for x := 0 to pdn.Width-1 do
            Img.Colors[x,y] := BGRAToFPColor(flat.GetPixel(x,y));
      end;
    finally
      flat.free;
    end;
    pdn.Free;
  except
    on ex: Exception do
    begin
      pdn.Free;
      raise Exception.Create('Error while loading Paint.NET file. ' + ex.Message);
    end;
  end;
end;

{ TPaintDotNetFile }

procedure TPaintDotNetFile.LoadFromFile(const filenameUTF8: string);
var
  stream: TFileStreamUTF8;
begin
  stream := TFileStreamUTF8.Create(filenameUTF8, fmOpenRead);
  OnLayeredBitmapLoadStart(filenameUTF8);
  try
    InternalLoadFromStream(stream);
  finally
    OnLayeredBitmapLoaded;
    stream.Free;
  end;
end;

procedure TPaintDotNetFile.LoadFromStream(stream: TStream);
begin
  OnLayeredBitmapLoadFromStreamStart;
  try
    InternalLoadFromStream(stream);
  finally
    OnLayeredBitmapLoaded;
  end;
end;

procedure TPaintDotNetFile.InternalLoadFromStream(stream: TStream);
var
  header: packed array[0..3] of char;
  XmlHeaderSize: integer;
  CompressionFormat: word;
  i:      integer;
begin
  Clear;
  header := #0#0#0#0;
  stream.Read(header, 4);
  if (header[0] <> 'P') or (header[1] <> 'D') or (header[2] <> 'N') or
    (header[3] <> '3') then
    raise Exception.Create('Invalid header');
  XmlHeaderSize := 0;
  stream.Read(XmlHeaderSize, 3);
  XmlheaderSize := LEtoN(XmlheaderSize);
  if Stream.Position + XmlHeaderSize > stream.Size then
    raise Exception.Create('Xml header size error');
  Stream.Position:= Stream.Position + XmlHeaderSize;
     {$hints off}
  stream.Read(CompressionFormat, sizeof(CompressionFormat));
     {$hints on}
  CompressionFormat := LEToN(CompressionFormat);
  Content := TDotNetDeserialization.Create;
  case Compressionformat of
    $0100: Content.LoadFromStream(Stream);
    $8b1f: raise Exception.Create('Serialized data decompression not handled');
    else
      raise Exception.Create('Unknown compression format (' +
        IntToStr(Compressionformat) + ')');
  end;
  Document := Content.FindClass('Document');
  if Document <> nil then
    Layers := Content.GetObjectField(Document, 'layers') as TSerializedClass;
  SetLength(LayerData, NbLayers);
  for i := 0 to NbLayers - 1 do
  begin
    OnLayeredBitmapLoadProgress((i+1)*100 div NbLayers);
    LayerData[i] := TMemoryStream.Create;
    LoadLayer(LayerData[i], Stream, LayerDataSize(i));
  end;
end;

function TPaintDotNetFile.ToString: ansistring;
var
  i, j, nbbytes: integer;
  b: byte;
begin
  Result := 'Paint.Net document' + LineEnding + LineEnding;
  Result += Content.ToString;
  for i := 0 to NbLayers - 1 do
  begin
    Result += LineEnding + 'Layer ' + IntToStr(i) + ' : ' + LayerName[i] + LineEnding;
    Result += '[ ';
    LayerData[i].Position := 0;
    if LayerData[i].Size > 256 then
      nbbytes := 256
    else
      nbbytes := LayerData[i].Size;
    for j := 0 to nbbytes - 1 do
    begin
        {$hints off}
      LayerData[i].Read(b, 1);
        {$hints on}
      Result += IntToHex(b, 2) + ' ';
    end;
    if LayerData[i].Size > nbbytes then
      Result += '...';
    Result   += ']' + lineending;
  end;
end;

constructor TPaintDotNetFile.Create;
begin
  inherited Create;
  Content   := nil;
  Document  := nil;
  Layers    := nil;
  LinearBlend := True;
  RegisterPaintNetFormat;
end;

procedure TPaintDotNetFile.Clear;
var
  i: integer;
begin
  FreeAndNil(content);
  document := nil;
  Layers   := nil;
  for i := 0 to high(LayerData) do
    LayerData[i].Free;
  setLength(LayerData, 0);
end;

function TPaintDotNetFile.GetWidth: integer;
begin
  if Document = nil then
    Result := 0
  else
    Result := StrToInt(Content.GetSimpleField(Document, 'width'));
end;

function TPaintDotNetFile.GetHeight: integer;
begin
  if Document = nil then
    Result := 0
  else
    Result := StrToInt(Content.GetSimpleField(Document, 'height'));
end;

function TPaintDotNetFile.GetNbLayers: integer;
begin
  if Layers = nil then
    Result := 0
  else
    Result := StrToInt(Content.GetSimpleField(Layers, '_size'));
end;

function TPaintDotNetFile.GetBlendOperation(Layer: integer): TBlendOperation;
begin
  Result := InternalGetBlendOperation(InternalGetLayer(layer));
end;

function TPaintDotNetFile.GetLayerVisible(layer: integer): boolean;
begin
  Result := InternalGetLayerVisible(InternalGetLayer(layer));
end;

function TPaintDotNetFile.GetLayerOpacity(layer: integer): byte;
begin
  Result := InternalGetLayerOpacity(InternalGetLayer(layer));
end;

function TPaintDotNetFile.GetLayerName(layer: integer): string;
begin
  Result := InternalGetLayerName(InternalGetLayer(layer));
end;

function TPaintDotNetFile.GetLayerBitmapCopy(layer: integer): TBGRABitmap;
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds');

  Result := TBGRABitmap.Create(Width, Height);
  if int64(Result.NbPixels) * 4 <> LayerData[layer].Size then
  begin
    Result.Free;
    raise Exception.Create('Inconsistent layer data size');
  end
  else
  begin
    layerData[layer].Position := 0;
    layerData[layer].Read(Result.Data^, LayerData[layer].Size);
    Result.InvalidateBitmap;

    if Result.LineOrder = riloBottomToTop then
      Result.VerticalFlip;
  end;
end;

function TPaintDotNetFile.InternalGetLayerName(layer: TSerializedClass): string;
var
  prop: TCustomSerializedObject;
begin
  if layer = nil then
    Result := ''
  else
  begin
    prop := Content.GetObjectField(layer, 'Layer+properties');
    if prop = nil then
      Result := ''
    else
    begin
      Result := Content.GetSimpleField(prop, 'name');
    end;
  end;
end;

function TPaintDotNetFile.LayerDataSize(numLayer: integer): int64;
var
  layer, surface, scan0: TCustomSerializedObject;
begin
  layer := InternalGetLayer(numLayer);
  if layer = nil then
    Result := 0
  else
  begin
    surface := Content.GetObjectField(layer, 'surface');
    if surface = nil then
      Result := 0
    else
    begin
      scan0  := Content.GetObjectField(surface, 'scan0');
      Result := StrToInt64(Content.GetSimpleField(scan0, 'length64'));
    end;
  end;
end;

procedure TPaintDotNetFile.LoadLayer(dest: TMemoryStream; src: TStream;
  uncompressedSize: int64);
var
  CompressionFlag: byte;
  maxChunkSize, decompressedChunkSize, compressedChunkSize: longword;
  chunks:   array of TMemoryStream;
  numChunk: integer;
  chunkCount, i: integer;
  decomp:   Tdecompressionstream;
  nextPos:  int64;

begin
  {$hints off}
  src.Read(CompressionFlag, 1);
  {$hints on}
  if CompressionFlag = 1 then
    dest.CopyFrom(src, uncompressedSize)
  else
  if CompressionFlag = 0 then
  begin
    maxChunkSize := BEReadLongword(src);
    if maxChunkSize < 4 then
      raise Exception.Create('Invalid max chunk size');
    chunkCount := (uncompressedSize + maxChunkSize - 1) div maxChunkSize;
    setlength(chunks, chunkCount);
    for i := 0 to ChunkCount - 1 do
    begin
      numChunk := BEReadLongint(src);
      if (numChunk < 0) or (numChunk >= chunkCount) then
        raise Exception.Create('Chunk number out of bounds');
      compressedChunkSize := BEReadLongword(src);
      nextPos := src.Position + compressedChunkSize;
      src.Position := src.Position + 10; //skip gzip header
      decompressedChunkSize :=
        min(maxChunkSize, uncompressedSize - int64(numChunk) * int64(maxChunkSize));
      decomp := Tdecompressionstream.Create(src, True);
      chunks[numChunk] := TMemoryStream.Create;
      chunks[numChunk].CopyFrom(decomp, decompressedChunkSize);
      FreeAndNil(decomp);
      src.Position := nextPos;
    end;
    for i := 0 to ChunkCount - 1 do
    begin
      chunks[i].Position := 0;
      dest.CopyFrom(chunks[i], chunks[i].size);
      chunks[i].Free;
    end;
    setlength(chunks, 0);
  end
  else
    raise Exception('Unknown compression flag (' + IntToStr(CompressionFlag) + ')');
end;

function TPaintDotNetFile.InternalGetLayer(num: integer): TSerializedClass;
var
  layerList: TCustomSerializedObject;
begin
  if Layers = nil then
    raise Exception.Create('No layers available')
  else
  if (num < 0) or (num >= NbLayers) then
    raise Exception.Create('Layer index out of bounds')
  else
  begin
    layerList := Content.GetObjectField(Layers, '_items');
    Result    := Content.GetObject(layerList.FieldAsString[num]) as TSerializedClass;
  end;
end;

function TPaintDotNetFile.InternalGetBlendOperation(layer: TSerializedClass): TBlendOperation;
var
  prop, blendOp: TCustomSerializedObject;
  blendName:     string;
begin
  if layer = nil then
    Result := boTransparent
  else
  begin
    prop := Content.GetObjectField(layer, 'properties');
    if prop = nil then
      Result := boTransparent
    else
    begin
      blendOp := Content.GetObjectField(prop, 'blendOp');
      if blendOp = nil then
        Result := boTransparent
      else
      begin
        blendName := blendOp.TypeAsString;
        if (pos('+', blendName) <> 0) then
          Delete(blendName, 1, pos('+', blendName));
        if copy(blendName, length(blendName) - length('BlendOp') +
          1, length('BlendOp')) = 'BlendOp' then
          Delete(blendName, length(blendName) - length('BlendOp') +
            1, length('BlendOp'));

        if blendName = 'Normal' then
          Result := boTransparent
        else
        if blendName = 'Multiply' then
          Result := boLinearMultiply
        else
        if blendName = 'Additive' then
          Result := boLinearAdd
        else
        if blendName = 'ColorBurn' then
          Result := boColorBurn
        else
        if blendName = 'ColorDodge' then
          Result := boColorDodge
        else
        if blendName = 'Reflect' then
          Result := boReflect
        else
        if blendName = 'Glow' then
          Result := boGlow
        else
        if blendName = 'Overlay' then
          Result := boOverlay
        else
        if blendName = 'Difference' then
          Result := boLinearDifference
        else
        if blendName = 'Negation' then
          Result := boLinearNegation
        else
        if blendName = 'Lighten' then
          Result := boLighten
        else
        if blendName = 'Darken' then
          Result := boDarken
        else
        if blendName = 'Screen' then
          Result := boScreen
        else
        if blendName = 'Xor' then
          Result := boXor
        else
          Result := boTransparent;
      end;
    end;
  end;
end;

function TPaintDotNetFile.InternalGetLayerVisible(layer: TSerializedClass): boolean;
var
  prop: TCustomSerializedObject;
begin
  if layer = nil then
    Result := False
  else
  begin
    prop := Content.GetObjectField(layer, 'Layer+properties');
    if prop = nil then
      Result := False
    else
    begin
      Result := (Content.GetSimpleField(prop, 'visible') = 'True');
    end;
  end;
end;

function TPaintDotNetFile.InternalGetLayerOpacity(layer: TSerializedClass): byte;
var
  prop: TCustomSerializedObject;
begin
  if layer = nil then
    Result := 0
  else
  begin
    prop := Content.GetObjectField(layer, 'Layer+properties');
    if prop = nil then
      Result := 0
    else
    begin
      Result := StrToInt(Content.GetSimpleField(prop, 'opacity'));
    end;
  end;
end;

var AlreadyRegistered: boolean;

procedure RegisterPaintNetFormat;
begin
  if AlreadyRegistered then exit;
  ImageHandlers.RegisterImageReader ('Paint.NET image', 'pdn', TFPReaderPaintDotNet);
  RegisterLayeredBitmapReader('pdn', TPaintDotNetFile);
  //TPicture.RegisterFileFormat('pdn', 'Paint.NET image', TPaintDotNetFile);
  DefaultBGRAImageReader[ifPaintDotNet] := TFPReaderPaintDotNet;
  AlreadyRegistered := true;
end;

end.
