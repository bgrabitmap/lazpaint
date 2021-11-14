// SPDX-License-Identifier: GPL-3.0-only
unit UImageBackup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes;

type
  TRowBackupArray = packed array of packed record
      Left, Right: Word;
      DataLen: integer;
      DataPos: Int64;
    end;

  { TImageBackup }

  TImageBackup = class
  private
    FRows: TRowBackupArray;
    FWidth,FHeight: integer;
    FBounds: TRect;
    FGrayscale: boolean;
    FData: TMemoryStream;
    procedure Init(ASource, ASource2: TBGRABitmap; AGrayscale: boolean; ASourceBounds: TRect);
  public
    constructor Create;
    constructor Create(ASource: TBGRABitmap; AGrayscale: boolean);
    constructor Create(ASource: TBGRABitmap; AGrayscale: boolean; ABounds: TRect);
    constructor Create(ASource, ASource2: TBGRABitmap; AGrayscale: boolean; ABounds: TRect);
    procedure Restore(ADest: TBGRABitmap; ARect: TRect; AXor: boolean);
    procedure SaveToStream(ADest: TStream);
    procedure LoadFromStream(ASource: TStream);
    destructor Destroy; override;
    function UsedMemory: int64;
    property Width: integer read FWidth;
    property Height: integer read FHeight;
    property Grayscale: boolean read FGrayscale;
    property Bounds: TRect read FBounds;
  end;

implementation

uses BGRALzpCommon, math;

{ TImageBackup }

procedure TImageBackup.Init(ASource, ASource2: TBGRABitmap; AGrayscale: boolean;
  ASourceBounds: TRect);
var
  tempRow: Pointer;
  pPix, pPix2: PBGRAPixel;
  sourceWidth, sourceWidth2: integer;

  procedure CopyChannelGrayscale(y, ARowWidth: integer);
  var
    pSrc, pTempRow: PByte;
    remain: LongInt;
  begin
    if Assigned(ASource) then
    begin
      pTempRow := PByte(tempRow);
      pSrc := ASource.GetPixelAddress(FRows[y].Left, y) + TBGRAPixel_GreenByteOffset;
      remain := ARowWidth;
      while remain > 0 do
      begin
        pTempRow^ := pSrc^;
        inc(pSrc, 4);
        inc(pTempRow);
        dec(remain);
      end;
    end else fillchar(tempRow^, ARowWidth, 0);
    if Assigned(ASource2) then
    begin
      pTempRow := PByte(tempRow);
      pSrc := ASource2.GetPixelAddress(FRows[y].Left, y) + TBGRAPixel_GreenByteOffset;
      remain := ARowWidth;
      while remain > 0 do
      begin
        pTempRow^ := pTempRow^ xor pSrc^;
        inc(pSrc, 4);
        inc(pTempRow);
        dec(remain);
      end;
    end;
  end;

  procedure CopyChannelsRGBA(y, ARowWidth: integer);
  var
    pSrc, pTempRow, pTempRow2, pTempRow3, pTempRow4: PByte;
    remain: LongInt;
  begin
    if Assigned(ASource) then
    begin
      pTempRow := PByte(tempRow);
      pTempRow2 := pTempRow + ARowWidth;
      pTempRow3 := pTempRow + (ARowWidth shl 1);
      pTempRow4 := pTempRow + ARowWidth*3;
      pSrc := ASource.GetPixelAddress(FRows[y].Left, y);
      remain := ARowWidth;
      while remain > 0 do
      begin
        pTempRow^ := pSrc^;
        pTempRow2^ := (pSrc+1)^;
        pTempRow3^ := (pSrc+2)^;
        pTempRow4^ := (pSrc+3)^;
        inc(pSrc, 4);
        inc(pTempRow);
        inc(pTempRow2);
        inc(pTempRow3);
        inc(pTempRow4);
        dec(remain);
      end;
    end else fillchar(tempRow^, ARowWidth*4, 0);
    if Assigned(ASource2) then
    begin
      pTempRow := PByte(tempRow);
      pTempRow2 := pTempRow + ARowWidth;
      pTempRow3 := pTempRow + (ARowWidth shl 1);
      pTempRow4 := pTempRow + ARowWidth*3;
      pSrc := ASource2.GetPixelAddress(FRows[y].Left, y);
      remain := ARowWidth;
      while remain > 0 do
      begin
        pTempRow^ := pTempRow^ xor pSrc^;
        pTempRow2^ := pTempRow2^ xor (pSrc+1)^;
        pTempRow3^ := pTempRow3^ xor (pSrc+2)^;
        pTempRow4^ := pTempRow4^ xor (pSrc+3)^;
        inc(pSrc, 4);
        inc(pTempRow);
        inc(pTempRow2);
        inc(pTempRow3);
        inc(pTempRow4);
        dec(remain);
      end;
    end;
  end;

  procedure EncodeChannels(y: integer);
  var
    rowWidth: Integer;
  begin
    rowWidth := FRows[y].Right - FRows[y].Left;
    if FGrayscale then
    begin
      CopyChannelGrayscale(y, rowWidth);
      EncodeLazRLE(tempRow^, rowWidth, FData);
    end
    else
    begin
      CopyChannelsRGBA(y, rowWidth);
      EncodeLazRLE(tempRow^, rowWidth*4, FData);
    end;
  end;

  function CheckPixel(x: integer): boolean; inline;
  var value: DWord;
  begin
    if FGrayscale then
    begin
      value := 0;
      if Assigned(pPix) and (x < sourceWidth) then value := (pPix+x)^.green;
      if Assigned(pPix2) and (x < SourceWidth2) then value := value xor (pPix2+x)^.green;
    end else
    begin
      value := 0;
      if Assigned(pPix) and (x < sourceWidth) then value := PDWord(pPix+x)^;
      if Assigned(pPix2) and (x < sourceWidth2) then value := value xor PDWord(pPix2+x)^;
    end;
    result := value <> 0;
  end;

  procedure IncludeBound(y: integer);
  begin
    if y < FBounds.Top then FBounds.Top := y;
    if y+1 > FBounds.Bottom then FBounds.Bottom := y+1;
    if FRows[y].Left < FBounds.Left then FBounds.Left := FRows[y].Left;
    if FRows[y].Right > FBounds.Right then FBounds.Right := FRows[y].Right;
  end;

var
  y,x,x2: LongInt;

begin
  FWidth := 0;
  FHeight := 0;
  if Assigned(ASource) then
  begin
    ASourceBounds.Intersect(rect(0,0,ASource.Width,ASource.Height));
    FWidth := ASource.Width;
    FHeight := ASource.Height;
    sourceWidth := ASource.Width;
  end else sourceWidth := 0;
  if Assigned(ASource2) then
  begin
    ASourceBounds.Intersect(rect(0,0,ASource2.Width,ASource2.Height));
    FWidth := max(FWidth, ASource2.Width);
    FHeight := max(FHeight, ASource2.Height);
    sourceWidth2 := ASource2.Width;
  end else sourceWidth2 := 0;
  FGrayscale := AGrayscale;
  FData := TMemoryStream.Create;
  FBounds := Rect(maxLongint, maxLongint, -maxLongint, -maxLongint);

  setlength(FRows, FHeight);
  if AGrayscale then
    Getmem(tempRow, ASourceBounds.Width)
    else Getmem(tempRow, ASourceBounds.Width*4);

  for y := ASourceBounds.Top to ASourceBounds.Bottom-1 do
  begin
    if Assigned(ASource) then pPix := ASource.ScanLine[y] else pPix := nil;
    if Assigned(ASource2) then pPix2 := ASource2.ScanLine[y] else pPix2 := nil;
    for x := ASourceBounds.Left to ASourceBounds.Right-1 do
    begin
      if CheckPixel(x) then //row start found
      begin
        FRows[y].Left:= x;
        FRows[y].Right:= x;
        FRows[y].DataPos:= FData.Position;
        for x2 := ASourceBounds.Right-1 downto x do
        begin
          if CheckPixel(x2) then //row end found
          begin
            FRows[y].Right := x2+1;
            IncludeBound(y);
            EncodeChannels(y);
            FRows[y].DataLen:= FData.Position-FRows[y].DataPos;
            break;
          end;
        end;
        break;
      end;
    end;
  end;
  FreeMem(tempRow);
  if (FBounds.Right < FBounds.Left) or (FBounds.Bottom < FBounds.Top) then
    FBounds := EmptyRect;
end;

constructor TImageBackup.Create;
begin
  FWidth:= 0;
  FHeight:= 0;
  FGrayscale:= false;
  FBounds:= EmptyRect;
  FData := nil;
end;

constructor TImageBackup.Create(ASource: TBGRABitmap; AGrayscale: boolean);
begin
  Init(ASource, nil, AGrayscale, rect(0, 0, ASource.Width, ASource.Height));
end;

constructor TImageBackup.Create(ASource: TBGRABitmap; AGrayscale: boolean;
  ABounds: TRect);
begin
  Init(ASource, nil, AGrayscale, ABounds);
end;

constructor TImageBackup.Create(ASource, ASource2: TBGRABitmap;
  AGrayscale: boolean; ABounds: TRect);
begin
  Init(ASource, ASource2, AGrayscale, ABounds);
end;

procedure TImageBackup.Restore(ADest: TBGRABitmap; ARect: TRect; AXor: boolean);
var
  tempRow: Pointer;

  procedure RestoreRow(y: integer; AInnerStart, AInnerEnd: integer);
  var
    rowWidth: integer;
    pData: PByte;
    remain: integer;

    procedure DecodeChannels;
    var
      dataWidth: integer;
      decoded: PtrInt;
    begin
      FData.Position := FRows[y].DataPos;
      if not FGrayscale then dataWidth := rowWidth*4 else dataWidth := rowWidth;
      decoded := DecodeLazRLE(FData, tempRow^, dataWidth, FRows[y].DataLen);
      if decoded < dataWidth then fillChar((PByte(tempRow)+decoded)^, 0, dataWidth - decoded);
      pData := PByte(tempRow) + AInnerStart;
      remain := AInnerEnd - AInnerStart;
    end;

  var
    pDest: PBGRAPixel;
    rowWidth3: Integer;
  begin
    if AInnerEnd <= AInnerStart then exit;
    rowWidth := FRows[y].Right - FRows[y].Left;
    rowWidth3 := rowWidth*3;
    DecodeChannels;
    pDest := PBGRAPixel(ADest.GetPixelAddress(FRows[y].Left + AInnerStart, y));
    if FGrayscale then
    begin
      if AXor then
      begin
        while remain > 0 do
        begin
          pDest^.green := pDest^.green xor pData^;
          pDest^.red := pDest^.green;
          pDest^.blue := pDest^.green;
          pDest^.alpha := 255;
          inc(pData);
          inc(pDest);
          dec(remain);
        end;
      end else
      begin
        while remain > 0 do
        begin
          pDest^.red := pData^;
          pDest^.green := pData^;
          pDest^.blue := pData^;
          pDest^.alpha := 255;
          inc(pData);
          inc(pDest);
          dec(remain);
        end;
      end;
    end else
    begin
      if AXor then
      begin
        while remain > 0 do
        begin
          PByte(pDest)^ := PByte(pDest)^ xor pData^;
          (PByte(pDest)+1)^ := (PByte(pDest)+1)^ xor (pData+rowWidth)^;
          (PByte(pDest)+2)^ := (PByte(pDest)+2)^ xor (pData+(rowWidth shl 1))^;
          (PByte(pDest)+3)^ := (PByte(pDest)+3)^ xor (pData+rowWidth3)^;
          inc(pData);
          inc(pDest);
          dec(remain);
        end;
      end else
      begin
        while remain > 0 do
        begin
          PByte(pDest)^ := pData^;
          (PByte(pDest)+1)^ := (pData+rowWidth)^;
          (PByte(pDest)+2)^ := (pData+(rowWidth shl 1))^;
          (PByte(pDest)+3)^ := (pData+rowWidth3)^;
          inc(pData);
          inc(pDest);
          dec(remain);
        end;
      end;
    end;
  end;

var
  emptyColor: TBGRAPixel;

  procedure SetEmpty(x, y, x2: integer);
  begin
    if not AXor then
      ADest.SetHorizLine(x, y, x2, emptyColor);
  end;

var rowInnerStart, y: integer;
begin
  ARect.Intersect(Bounds);
  ARect.Intersect(ADest.ClipRect);
  if ARect.IsEmpty then exit;

  if FGrayscale then
  begin
    GetMem(tempRow, FWidth);
    emptyColor := BGRABlack;
  end else
  begin
    GetMem(tempRow, FWidth*4);
    emptyColor := BGRAPixelTransparent;
  end;

  for y := ARect.Top to ARect.Bottom-1 do
  begin
    if FRows[y].Left >= ARect.Left then
    begin
      if FRows[y].Left >= ARect.Right then
      begin
        SetEmpty(ARect.Left, y, ARect.Right-1);
        Continue;
      end
      else
      begin
        if FRows[y].Left > ARect.Left then
          SetEmpty(ARect.Left, y, FRows[y].Left-1);
        rowInnerStart := 0;
      end;
    end else
    if FRows[y].Right <= ARect.Left then Continue else
      rowInnerStart := ARect.Left - FRows[y].Left;

    if FRows[y].Right >= ARect.Right then
      RestoreRow(y, rowInnerStart, ARect.Right - FRows[y].Left) else
    begin
      RestoreRow(y, rowInnerStart, FRows[y].Right - FRows[y].Left);
      SetEmpty(FRows[y].Right, y, ARect.Right-1);
    end;
  end;
  FreeMem(tempRow);
end;

procedure TImageBackup.SaveToStream(ADest: TStream);
var dataSize: int64;
  lenRows: Integer;
begin
  ADest.WriteBuffer(FWidth, sizeof(FWidth));
  ADest.WriteBuffer(FHeight, sizeof(FHeight));
  ADest.WriteBuffer(FBounds, sizeof(FBounds));
  ADest.WriteBuffer(FGrayscale, sizeof(FGrayscale));
  lenRows := length(FRows);
  ADest.WriteBuffer(lenRows, sizeof(lenRows));
  if lenRows > 0 then ADest.WriteBuffer(FRows[0], sizeof(FRows[0])*lenRows);
  dataSize := FData.Size;
  ADest.WriteBuffer(dataSize, sizeof(dataSize));
  FData.Position:= 0;
  ADest.CopyFrom(FData, dataSize);
end;

procedure TImageBackup.LoadFromStream(ASource: TStream);
var newWidth, newHeight: integer;
  newGrayscale: boolean;
  newBounds: TRect;
  newRows: TRowBackupArray;
  dataSize: int64;
  lenRows: Integer;
begin
  ASource.ReadBuffer({%H-}newWidth, sizeof(newWidth));
  ASource.ReadBuffer({%H-}newHeight, sizeof(newHeight));
  ASource.ReadBuffer({%H-}newBounds, sizeof(newBounds));
  ASource.ReadBuffer({%H-}newGrayscale, sizeof(newGrayscale));
  ASource.ReadBuffer({%H-}lenRows, sizeof(lenRows));
  newRows := nil;
  setlength(newRows, lenRows);
  if lenRows > 0 then ASource.ReadBuffer(newRows[0], sizeof(newRows[0])*length(newRows));
  ASource.ReadBuffer({%H-}dataSize, sizeof(dataSize));
  FreeAndNil(FData);
  FData := TMemoryStream.Create;
  FData.CopyFrom(ASource, dataSize);
  FWidth := newWidth;
  FHeight := newHeight;
  FBounds := newBounds;
  FGrayscale:= newGrayscale;
  FRows := newRows;
end;

destructor TImageBackup.Destroy;
begin
  FData.Free;
  inherited Destroy;
end;

function TImageBackup.UsedMemory: int64;
begin
  result := 0;
  if Assigned(FData) then result := FData.Size;
  if FRows <> nil then
    inc(result, length(FRows)*sizeof(FRows[0]));
end;

end.

