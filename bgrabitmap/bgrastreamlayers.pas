unit BGRAStreamLayers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRALayers, BGRABitmap;

function CheckStreamForLayers(AStream: TStream): boolean;
function LoadLayersFromStream(AStream: TStream; out ASelectedLayerIndex: integer; ALoadLayerUniqueIds: boolean = false) : TBGRALayeredBitmap;
procedure SaveLayersToStream(AStream: TStream; ALayers: TBGRACustomLayeredBitmap; ASelectedLayerIndex: integer);
procedure SaveLayerBitmapToStream(AStream: TStream; ABitmap: TBGRABitmap; ACaption: string);
function LoadLayerBitmapFromStream(AStream: TStream) : TBGRABitmap;
procedure RegisterStreamLayers;

implementation

uses BGRABitmapTypes, BGRACompressableBitmap, zstream;

procedure SaveLayeredBitmapToStream(AStream: TStream; ALayers: TBGRACustomLayeredBitmap);
begin
  SaveLayersToStream(AStream,ALayers,-1);
end;

function LoadLayeredBitmapFromStream(AStream: TStream) : TBGRALayeredBitmap;
var selectedIndex: integer;
begin
  if not CheckStreamForLayers(AStream) then
    result := nil
  else
    result := LoadLayersFromStream(AStream,selectedIndex);
end;

const
  StreamHeader = 'TBGRALayeredBitmap'#26#0;
  StreamMaxLayerCount = 4096;
  StreamMaxHeaderSize = 256;

{$i winstream.inc}

function CheckStreamForLayers(AStream: TStream): boolean;
var
  OldPosition: Int64;
  HeaderFound: string;
begin
  result := false;
  OldPosition:= AStream.Position;
  try
    SetLength(HeaderFound, length(StreamHeader));
    SetLength(HeaderFound, AStream.Read(HeaderFound[1], length(HeaderFound)));
    if HeaderFound = StreamHeader then
      result := true;
  except
    on ex: exception do
    begin
      //nothing
    end;
  end;
  AStream.Position:= OldPosition;
end;

function LoadLayersFromStream(AStream: TStream; out ASelectedLayerIndex: integer; ALoadLayerUniqueIds: boolean = false): TBGRALayeredBitmap;
var
  OldPosition: Int64;
  HeaderFound: string;
  NbLayers: LongInt;
  HeaderSize, LayerHeaderSize: LongInt;
  LayerStackStartPosition, LayerHeaderPosition, LayerBitmapPosition, LayerEndPosition: Int64;
  LayerOption,StackOption: LongInt;
  Layer: TBGRABitmap;
  i,LayerIndex: integer;
  LayerName: string;
  LayerId: LongInt;
begin
  result := TBGRALayeredBitmap.Create;
  OldPosition:= AStream.Position;
  SetLength(HeaderFound, length(StreamHeader));
  try
    //format identifier
    SetLength(HeaderFound, AStream.Read(HeaderFound[1], length(HeaderFound)));
    if HeaderFound <> StreamHeader then
      raise exception.Create('Invalid header');

    //header size
    HeaderSize:= WinReadLongint(AStream);
    if (HeaderSize < 12) or (HeaderSize > StreamMaxHeaderSize) then
      raise exception.Create('Invalid header size');
    LayerStackStartPosition := AStream.Position + HeaderSize;

    NbLayers:= WinReadLongint(AStream);
    if (NbLayers < 0) or (NbLayers > StreamMaxLayerCount) then
      raise exception.Create('Invalid layer count');

    ASelectedLayerIndex:= WinReadLongint(AStream);
    if (ASelectedLayerIndex < -1) or (ASelectedLayerIndex >= NbLayers) then
      raise exception.Create('Selected layer out of bounds');

    StackOption := WinReadLongint(AStream);
    result.LinearBlend := (StackOption and 1) = 1;
    //end of header

    AStream.Position:= LayerStackStartPosition;
    for i := 0 to NbLayers-1 do
    begin
      LayerHeaderSize:= WinReadLongint(AStream);
      LayerHeaderPosition := AStream.Position;
      LayerBitmapPosition := LayerHeaderPosition + LayerHeaderSize;

      AStream.Position:= LayerBitmapPosition;
      Layer := LoadLayerBitmapFromStream(AStream);
      LayerName := Layer.Caption;
      LayerIndex := result.AddOwnedLayer(Layer);
      Layer := nil;
      LayerEndPosition := AStream.Position;

      AStream.Position := LayerHeaderPosition;
      if AStream.Position <= LayerBitmapPosition-4 then
      begin
        LayerOption := WinReadLongint(AStream);
        result.LayerVisible[LayerIndex] := (LayerOption and 1) = 1;
      end;
      if AStream.Position <= LayerBitmapPosition-4 then
        result.BlendOperation[LayerIndex]:= TBlendOperation(WinReadLongint(AStream));
      if AStream.Position <= LayerBitmapPosition-8 then
      begin
        result.LayerOffset[LayerIndex] := Point(WinReadLongint(AStream),WinReadLongint(AStream));
        if AStream.Position <= LayerBitmapPosition-4 then
        begin
          LayerId := WinReadLongint(AStream);
          if ALoadLayerUniqueIds then
            result.LayerUniqueId[LayerIndex] := LayerId;
        end;
        if AStream.Position <= LayerBitmapPosition-4 then
          result.LayerOpacity[LayerIndex] := WinReadLongint(AStream) shr 8;
      end;
      result.LayerName[LayerIndex] := LayerName;

      AStream.Position := LayerEndPosition;
    end;
  except
    on ex: Exception do
    begin
      AStream.Position := OldPosition;
      raise ex;
    end;
  end;
end;

procedure SaveLayersToStream(AStream: TStream; ALayers: TBGRACustomLayeredBitmap; ASelectedLayerIndex: integer);
var
  LayerOption,StackOption: longint;
  i: integer;
  LayerHeaderSizePosition,LayerHeaderPosition: int64;
  LayerBitmapPosition: int64;
  LayerHeaderSize: integer;
  bitmap: TBGRABitmap;
begin
  if (ASelectedLayerIndex < -1) or (ASelectedLayerIndex >= ALayers.NbLayers) then
    raise exception.Create('Selected layer out of bounds');
  AStream.Write(StreamHeader[1], length(StreamHeader));
  WinWriteLongint(AStream, 12); //header size
  WinWriteLongint(AStream, ALayers.NbLayers);
  WinWriteLongint(AStream, ASelectedLayerIndex);
  StackOption := 0;
  if ALayers.LinearBlend then StackOption := StackOption or 1;
  WinWriteLongint(AStream, StackOption);
  //end of header

  for i := 0 to ALayers.NbLayers-1 do
  begin
    LayerHeaderSizePosition:= AStream.Position;
    WinWriteLongint(AStream, 0); //header size not computed yet
    LayerHeaderPosition := AStream.Position;

    LayerOption := 0;
    if ALayers.LayerVisible[i] then LayerOption:= LayerOption or 1;
    WinWriteLongint(AStream, LayerOption);
    WinWriteLongint(AStream, Longint(ALayers.BlendOperation[i]));
    WinWriteLongint(AStream, ALayers.LayerOffset[i].x);
    WinWriteLongint(AStream, ALayers.LayerOffset[i].y);
    WinWriteLongint(AStream, ALayers.LayerUniqueId[i]);
    WinWriteLongint(AStream, integer(ALayers.LayerOpacity[i])*$101);
    LayerBitmapPosition:=AStream.Position;
    LayerHeaderSize := LayerBitmapPosition - LayerHeaderPosition;
    AStream.Position:= LayerHeaderSizePosition;
    WinWriteLongint(AStream, LayerHeaderSize);
    //end of layer header

    AStream.Position:= LayerBitmapPosition;
    bitmap := ALayers.GetLayerBitmapDirectly(i);
    if bitmap <> nil then
      SaveLayerBitmapToStream(AStream, bitmap, ALayers.LayerName[i])
    else
    begin
      bitmap := ALayers.GetLayerBitmapCopy(i);
      SaveLayerBitmapToStream(AStream, bitmap, ALayers.LayerName[i]);
      bitmap.free;
    end;
  end;
end;

procedure SaveLayerBitmapToStream(AStream: TStream; ABitmap: TBGRABitmap; ACaption: string);
var Compressed: TBGRACompressableBitmap;
begin
  Compressed := TBGRACompressableBitmap.Create(ABitmap);
  Compressed.Caption := ACaption;
  Compressed.CompressionLevel:= cldefault;
  Compressed.WriteToStream(AStream);
  Compressed.Free;
end;

function LoadLayerBitmapFromStream(AStream: TStream): TBGRABitmap;
var Compressed: TBGRACompressableBitmap;
begin
  Compressed := TBGRACompressableBitmap.Create;
  Compressed.ReadFromStream(AStream);
  result := Compressed.GetBitmap;
  Compressed.Free;
end;

procedure RegisterStreamLayers;
begin
  LayeredBitmapSaveToStreamProc := @SaveLayeredBitmapToStream;
  LayeredBitmapLoadFromStreamProc := @LoadLayeredBitmapFromStream;
end;

end.

