unit BGRAStreamLayers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRALayers, BGRABitmap, BGRALzpCommon;

function CheckStreamForLayers(AStream: TStream): boolean;
function LoadLayersFromStream(AStream: TStream; out ASelectedLayerIndex: integer; ALoadLayerUniqueIds: boolean = false) : TBGRALayeredBitmap;
procedure SaveLayersToStream(AStream: TStream; ALayers: TBGRACustomLayeredBitmap; ASelectedLayerIndex: integer; ACompression: TLzpCompression = lzpZStream);
procedure SaveLayerBitmapToStream(AStream: TStream; ABitmap: TBGRABitmap; ACaption: string; ACompression: TLzpCompression = lzpZStream);
function LoadLayerBitmapFromStream(AStream: TStream; ACompression: TLzpCompression = lzpZStream) : TBGRABitmap;
procedure RegisterStreamLayers;

implementation

uses BGRABitmapTypes, BGRACompressableBitmap, zstream, BGRAReadLzp, BGRAWriteLzp,
     BGRAUTF8;

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
  Compression: TLzpCompression;
  LayerVisible: boolean;
  LayerBlendOp: TBlendOperation;
  LayerOffset: TPoint;
  LayerOpacity: integer;
  LayerIdFound: boolean;
  LayerBitmapSize: integer;
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
    HeaderSize:= LEReadLongint(AStream);
    if (HeaderSize < 12) or (HeaderSize > StreamMaxHeaderSize) then
      raise exception.Create('Invalid header size');
    LayerStackStartPosition := AStream.Position + HeaderSize;

    NbLayers:= LEReadLongint(AStream);
    if (NbLayers < 0) or (NbLayers > StreamMaxLayerCount) then
      raise exception.Create('Invalid layer count');

    ASelectedLayerIndex:= LEReadLongint(AStream);
    if (ASelectedLayerIndex < -1) or (ASelectedLayerIndex >= NbLayers) then
      raise exception.Create('Selected layer out of bounds');

    StackOption := LEReadLongint(AStream);
    result.LinearBlend := (StackOption and 1) = 1;
    if (StackOption and 2) = 2 then Compression := lzpRLE else Compression:= lzpZStream;
    //end of header

    AStream.Position:= LayerStackStartPosition;
    for i := 0 to NbLayers-1 do
    begin
      LayerHeaderSize:= LEReadLongint(AStream);
      LayerHeaderPosition := AStream.Position;
      LayerBitmapPosition := LayerHeaderPosition + LayerHeaderSize;
      LayerEndPosition := -1;

      LayerVisible := true;
      LayerBlendOp := result.DefaultBlendingOperation;
      LayerOffset := Point(0,0);
      LayerId := 0;
      LayerIdFound := false;
      LayerOpacity := 255;

      if AStream.Position <= LayerBitmapPosition-4 then
      begin
        LayerOption := LEReadLongint(AStream);
        LayerVisible := (LayerOption and 1) = 1;
      end;
      if AStream.Position <= LayerBitmapPosition-4 then
        LayerBlendOp := TBlendOperation(LEReadLongint(AStream));

      if AStream.Position <= LayerBitmapPosition-8 then
      begin
        LayerOffset := Point(LEReadLongint(AStream),LEReadLongint(AStream));
        if AStream.Position <= LayerBitmapPosition-4 then
        begin
          LayerId := LEReadLongint(AStream);
          LayerIdFound := true;
        end;
        if AStream.Position <= LayerBitmapPosition-4 then
          LayerOpacity := LEReadLongint(AStream) shr 8;
      end;
      if AStream.Position <= LayerBitmapPosition-4 then
      begin
        LayerBitmapSize := LEReadLongint(AStream);
        LayerEndPosition:= LayerBitmapPosition+LayerBitmapSize;
      end;

      AStream.Position:= LayerBitmapPosition;
      Layer := LoadLayerBitmapFromStream(AStream, Compression);
      LayerName := Layer.Caption;
      LayerIndex := result.AddOwnedLayer(Layer);
      Layer := nil;

      result.LayerName[LayerIndex] := LayerName;
      result.LayerVisible[LayerIndex] := LayerVisible;
      result.BlendOperation[LayerIndex]:= LayerBlendOp;
      result.LayerOffset[LayerIndex] := LayerOffset;
      if ALoadLayerUniqueIds and LayerIdFound then
        result.LayerUniqueId[LayerIndex] := LayerId;
      result.LayerOpacity[LayerIndex] := LayerOpacity;

      if LayerEndPosition <> -1 then AStream.Position := LayerEndPosition;
    end;
  except
    on ex: Exception do
    begin
      AStream.Position := OldPosition;
      raise ex;
    end;
  end;
end;

procedure SaveLayersToStream(AStream: TStream; ALayers: TBGRACustomLayeredBitmap; ASelectedLayerIndex: integer; ACompression: TLzpCompression);
var
  LayerOption,StackOption: longint;
  i: integer;
  LayerHeaderSizePosition,LayerHeaderPosition: int64;
  LayerBitmapPosition,LayerBitmapSizePosition,BitmapSize: int64;
  LayerHeaderSize: integer;
  bitmap: TBGRABitmap;
begin
  if (ASelectedLayerIndex < -1) or (ASelectedLayerIndex >= ALayers.NbLayers) then
    raise exception.Create('Selected layer out of bounds');
  AStream.Write(StreamHeader[1], length(StreamHeader));
  LEWriteLongint(AStream, 12); //header size
  LEWriteLongint(AStream, ALayers.NbLayers);
  LEWriteLongint(AStream, ASelectedLayerIndex);
  StackOption := 0;
  if ALayers.LinearBlend then StackOption := StackOption or 1;
  if ACompression = lzpRLE then StackOption:= StackOption or 2;
  LEWriteLongint(AStream, StackOption);
  //end of header

  for i := 0 to ALayers.NbLayers-1 do
  begin
    LayerHeaderSizePosition:= AStream.Position;
    LEWriteLongint(AStream, 0); //header size not computed yet
    LayerHeaderPosition := AStream.Position;

    LayerOption := 0;
    if ALayers.LayerVisible[i] then LayerOption:= LayerOption or 1;
    LEWriteLongint(AStream, LayerOption);
    LEWriteLongint(AStream, Longint(ALayers.BlendOperation[i]));
    LEWriteLongint(AStream, ALayers.LayerOffset[i].x);
    LEWriteLongint(AStream, ALayers.LayerOffset[i].y);
    LEWriteLongint(AStream, ALayers.LayerUniqueId[i]);
    LEWriteLongint(AStream, integer(ALayers.LayerOpacity[i])*$101);
    LayerBitmapSizePosition:=AStream.Position;
    LEWriteLongint(AStream, 0);
    LayerBitmapPosition:=AStream.Position;
    LayerHeaderSize := LayerBitmapPosition - LayerHeaderPosition;
    AStream.Position:= LayerHeaderSizePosition;
    LEWriteLongint(AStream, LayerHeaderSize);
    //end of layer header

    AStream.Position:= LayerBitmapPosition;
    bitmap := ALayers.GetLayerBitmapDirectly(i);
    if bitmap <> nil then
      SaveLayerBitmapToStream(AStream, bitmap, ALayers.LayerName[i], ACompression)
    else
    begin
      bitmap := ALayers.GetLayerBitmapCopy(i);
      SaveLayerBitmapToStream(AStream, bitmap, ALayers.LayerName[i], ACompression);
      bitmap.free;
    end;
    BitmapSize := AStream.Position - LayerBitmapPosition;
    if BitmapSize > maxLongint then
      raise exception.Create('Image too big');
    AStream.Position:= LayerBitmapSizePosition;
    LEWriteLongint(AStream, BitmapSize);
    AStream.Position:= LayerBitmapPosition+BitmapSize;
  end;
end;

procedure SaveLayerBitmapToStream(AStream: TStream; ABitmap: TBGRABitmap; ACaption: string; ACompression: TLzpCompression);
var Compressed: TBGRACompressableBitmap;
begin
  if ACompression = lzpZStream then
  begin
    Compressed := TBGRACompressableBitmap.Create(ABitmap);
    Compressed.Caption := ACaption;
    Compressed.CompressionLevel:= cldefault;
    Compressed.WriteToStream(AStream);
    Compressed.Free;
  end else
    TBGRAWriterLazPaint.WriteRLEImage(AStream, ABitmap, ACaption);
end;

function LoadLayerBitmapFromStream(AStream: TStream; ACompression: TLzpCompression): TBGRABitmap;
var Compressed: TBGRACompressableBitmap;
  captionFound: string;
begin
  if ACompression = lzpZStream then
  begin
    Compressed := TBGRACompressableBitmap.Create;
    Compressed.ReadFromStream(AStream);
    result := Compressed.GetBitmap;
    Compressed.Free;
  end else
  begin
    result := TBGRABitmap.Create;
    TBGRAReaderLazPaint.LoadRLEImage(AStream, result, captionFound);
    result.Caption := captionFound;
  end;
end;

procedure RegisterStreamLayers;
begin
  LayeredBitmapSaveToStreamProc := @SaveLayeredBitmapToStream;
  LayeredBitmapLoadFromStreamProc := @LoadLayeredBitmapFromStream;
end;

end.

