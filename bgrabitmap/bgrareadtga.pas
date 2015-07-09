{*****************************************************************************}
{
    This file is part of the Free Pascal's "Free Components Library".
    Copyright (c) 2003 by Mazen NEIFER of the Free Pascal development team

    Targa reader implementation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{*****************************************************************************}

{ - 22/11/2007 Modified by Laurent Jacques for support all format }

{$mode objfpc}
{$h+}

unit BGRAReadTGA;

interface

uses FPReadTGA, FPimage, Classes;

type
  { TBGRAReaderTarga }

  TBGRAReaderTarga = class (TFPReaderTarga)
  protected
    FBuffer: packed array of byte;
    FBufferPos, FBufferSize: integer;
    FBufferStream: TStream;
    procedure ReadScanLine({%H-}Row: Integer; Stream: TStream); override;
    procedure WriteScanLine(Row : Integer; Img : TFPCustomImage); override;
    procedure InitReadBuffer(AStream: TStream; ASize: integer);
    procedure CloseReadBuffer;
    function GetNextBufferByte: byte;
  end;

Implementation

uses BGRABitmapTypes, targacmn;

procedure TBGRAReaderTarga.ReadScanLine(Row: Integer; Stream: TStream);
Var
  P : PByte;
  B : Byte;
  I,J : Integer;
  PixelSizeInBytesMinus1: integer;

begin
  If Not Compressed then
    Stream.ReadBuffer(FScanLine^,FLineSize)
  else
  begin
    InitReadBuffer(Stream, 2048);
    P:=FScanLine;
    PixelSizeInBytesMinus1 := (BytesPerPixel shr 3)-1;
    For I:=0 to ToWord(Header.Width)-1 do
      begin
      If (FPixelCount>0) then
        Dec(FPixelCount)
      else
      begin
        Dec(FBlockCount);
        If (FBlockCount<0) then
          begin
          B := GetNextBufferByte;
          If (B and $80)<>0 then
            begin
            FPixelCount:=B and $7F;
            FblockCount:=0;
            end
          else
            FBlockCount:=B and $7F
          end;
        For J:=0 to PixelSizeInBytesMinus1 do
           FLastPixel[j] := GetNextBufferByte;
      end;
      For J:=0 to PixelSizeInBytesMinus1 do
        begin
        P[0]:=FLastPixel[j];
        Inc(P);
        end;
      end;
    CloseReadBuffer;
  end;
end;

Procedure TBGRAReaderTarga.WriteScanLine(Row : Integer; Img : TFPCustomImage);
Var
  Col : Integer;
  Value   : NativeUint;
  P   : PByte;
  PDest: PBGRAPixel;

begin
  P:=FScanLine;
  PDest := TBGRACustomBitmap(img).ScanLine[Row];
  Case Header.ImgType of
    TARGA_INDEXED_IMAGE
      : for Col:=Img.width-1 downto 0 do
        begin
         PDest^ := FPColorToBGRA(FPalette[P^]);
         Inc(PDest);
         Inc(P);
        end;
    TARGA_TRUECOLOR_IMAGE
      : if (BytesPerPixel = 32) and (AlphaBits = 8) then
           Move(P^,PDest^,Img.Width*sizeof(TBGRAPixel)) else
        if (BytesPerPixel = 24) then
        begin
          for Col:=Img.Width-1 downto 0 do
          begin
            PDest^ := BGRA((P+2)^,(P+1)^,P^);
            inc(Pdest);
            Inc(p,3);
          end;
        end
        else if (BytesPerPixel in[8,16]) then
        for Col:= Img.Width-1 to 0 do
          begin
            Value:=P[0];
            inc(P);
            Value:=value or (P[0] shl 8);
            PDest^ := BGRA(((value)shr 10) shl 3,((value)shr 5) shl 3,((value)) shl 3);
            Inc(PDest);
            Inc(P);
          end;
    TARGA_GRAY_IMAGE
      :  case BytesPerPixel of
           8 : for Col:=Img.width-1 downto 0 do
              begin
               PDest^ := FPColorToBGRA(FPalette[P^]);
               Inc(PDest);
               Inc(P);
              end;
          16 : for Col:=0 to Img.width-1 do
               begin
                 With PDest^ do
                 begin
                   blue:=FPalette[P^].blue shr 8;
                   green:=FPalette[P^].green shr 8;
                   red:=FPalette[P^].red shr 8;
                   Inc(P);
                   if alphaBits = 8 then alpha := P^ else
                     alpha:=255;
                   Inc(P);
                 end;
                 inc(PDest);
               end;
         end;
  end;
end;

procedure TBGRAReaderTarga.InitReadBuffer(AStream: TStream; ASize: integer);
begin
  setLength(FBuffer,ASize);
  FBufferSize := AStream.Read(FBuffer[0],ASize);
  FBufferPos := 0;
  FBufferStream := AStream;
end;

procedure TBGRAReaderTarga.CloseReadBuffer;
begin
  FBufferStream.Position:= FBufferStream.Position-FBufferSize+FBufferPos;
end;

function TBGRAReaderTarga.GetNextBufferByte: byte;
begin
  if FBufferPos < FBufferSize then
  begin
    result := FBuffer[FBufferPos];
    inc(FBufferPos);
  end else
  if FBufferSize = 0 then
    result := 0
  else
  begin
    FBufferSize := FBufferStream.Read(FBuffer[0],length(FBuffer));
    FBufferPos := 0;
    if FBufferPos < FBufferSize then
    begin
      result := FBuffer[FBufferPos];
      inc(FBufferPos);
    end else
      result := 0;
  end;
end;

initialization

  DefaultBGRAImageReader[ifTarga] := TBGRAReaderTarga;

end.
