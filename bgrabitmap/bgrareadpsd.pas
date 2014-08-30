{
    This original file was part of the Free Pascal run time library.
    Copyright (c) 2008 by the Free Pascal development team

    Psd reader for fpImage.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

    03/2014 changes by circular :
    - added MinifyHeight,WantedHeight and OutputHeight (useful for thumbnails)
}
unit BGRAReadPSD;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPimage, FPReadPSD;

type
  { TBGRAReaderPSD }

  TBGRAReaderPSD = class(TFPReaderPSD)
  private
    FCompressed: boolean;
  protected
    FScanLines      : array of PByte;
    FInputLine     : array of record
        StreamOffset: Int64;
        Size: PtrInt;
      end;
    FOutputHeight: integer;
    function ReadPalette(Stream: TStream): boolean;
    procedure AnalyzeHeader;
    procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
    function ReadScanLine(Stream: TStream; AInputSize: PtrInt; AChannel: integer): boolean; overload;
    procedure WriteScanLine(Img: TFPCustomImage; Row: integer); overload;
    function  InternalCheck(Stream: TStream) : boolean; override;
  public
    MinifyHeight,WantedHeight: integer;
    constructor Create; override;
    property Compressed: Boolean read FCompressed;
    property OutputHeight: integer read FOutputHeight;
  end;

implementation

uses BGRABitmapTypes;

function clamp(AValue, AMax: integer): integer;
begin
  if AValue < 0 then result := 0 else
  if AValue > AMax then result := AMax else
   result := AValue;;
end;

function CMYKtoRGB ( C : TFPColor):  TFPColor;
var r,g,b: integer;
begin
  r := $ffff - c.red + c.green div 10 + c.blue div 10 - c.alpha;
  g := $ffff + c.red div 10 - c.green + c.blue div 10 - c.alpha;
  b := $ffff + c.red div 10 + c.green div 10 - c.blue - c.alpha;
  result.red := clamp(r, 65535);
  result.green := clamp(g, 65535);
  result.blue := clamp(b, 65535);
  Result.alpha:=alphaOpaque;
end;

function fInv(t: single): single;
begin
  if t > 6/29 then result := t*t*t else
   result := 3*(6/29)*(6/29)*(t-4/29);
end;

function Csrgb(linear: single): single;
begin
  if linear <= 0.0031308 then
      result := 12.92*linear else
  result := (1+0.055)*exp(ln(linear)*(1/2.4)) - 0.055;
end;

function LabToRGB(L,a,b: single):TFPColor; overload;
var r,g,blue: single;
begin
  if a < 0 then
    r := L + a + 0.25*b
  else
    r := L + 0.75*a + 0.25*b;
  g := L - 0.25*a;
  blue := L - b;
  Result.red:= clamp(round((r)*65535),65535);
  Result.green:= clamp(round((g)*65535),65535);
  Result.blue:= clamp(round((blue)*65535),65535);
  result.alpha := 65535;
end;

function LabToRGB(const Lab:TLab):TFPColor; overload;
var L: single;
begin
  L := 1/255*Lab.L;
  result := LabToRGB(L,(Lab.a-128)/127,(Lab.b-128)/127);
end;

{ TBGRAReaderPSD }

function TBGRAReaderPSD.ReadPalette(Stream: TStream): boolean;
Var
  I : Integer;
  c : TFPColor;
  OldPos: Integer;
  BufSize:Longint;
  {%H-}PalBuf: array[0..767] of Byte;
  ContProgress: Boolean;
begin
  Result:=false;
  ThePalette.count := 0;
  OldPos := Stream.Position;
  BufSize:=0;
  Stream.Read(BufSize, SizeOf(BufSize));
  BufSize:=BEtoN(BufSize);
  Stream.Read({%H-}PalBuf, BufSize);
  ContProgress:=true;
  Progress(FPimage.psRunning, 0, False, Rect(0,0,0,0), '', ContProgress);
  if not ContProgress then exit;
  For I:=0 To BufSize div 3 Do
  Begin
    With c do
    begin
      Red:=PalBuf[I] shl 8;
      Green:=PalBuf[I+(BufSize div 3)] shl 8;
      Blue:=PalBuf[I+(BufSize div 3)* 2] shl 8;
      Alpha:=alphaOpaque;
    end;
    ThePalette.Add(C);
  End;
  Stream.Position := OldPos;
  Result:=true;
end;

procedure TBGRAReaderPSD.AnalyzeHeader;
var channel: integer;
begin
  With FHeader do
  begin
    Depth:=BEtoN(Depth);
    if (Signature <> '8BPS') then
      Raise Exception.Create('Unknown/Unsupported PSD image type');
    Channels:=BEtoN(Channels);
    if Channels > 4 then
      FBytesPerPixel:=Depth*4
    else
      FBytesPerPixel:=Depth*Channels;
    Mode:=BEtoN(Mode);
    FWidth:=BEtoN(Columns);
    FHeight:=BEtoN(Rows);
    FChannelCount:=Channels;
    FLineSize:=(PtrInt(FWidth)*Depth+7) div 8;
    setlength(FScanLines, FChannelCount);
    for channel := 0 to FChannelCount-1 do
      GetMem(FScanLines[channel],FLineSize);
  end;
end;

procedure TBGRAReaderPSD.InternalRead(Stream: TStream; Img: TFPCustomImage);
var
  H,HOutput,InputLineIndex,LenOfLineIndex,channel: Integer;
  LenOfLineFactor: PtrInt;
  BufSize:Cardinal;
  Encoding:word;
  ContProgress: Boolean;
  CurOffset: int64;
  PrevOutputRow, OutputRow, OutputRowAdd, OutputRowAcc, OutputRowAccAdd, OutputRowMod: integer;
begin
  FScanLines:=nil;
  FPalette:=nil;
  try
    Stream.Position:=0;
    ContProgress:=true;
    Progress(FPimage.psStarting, 0, False, Rect(0,0,0,0), '', ContProgress);
    if not ContProgress then exit;
    // read header
    Stream.Read(FHeader, SizeOf(FHeader));
    Progress(FPimage.psRunning, 0, False, Rect(0,0,0,0), '', ContProgress);
    if not ContProgress then exit;
    AnalyzeHeader;
    Case FHeader.Mode of
        0:begin  // Bitmap (monochrome)
            FPalette := TFPPalette.Create(0);
            CreateBWPalette;
          end;
        1, 8:begin // Gray-scale
            FPalette := TFPPalette.Create(0);
            CreateGrayPalette;
          end;
        2:begin // Indexed color (palette color)
            FPalette := TFPPalette.Create(0);
            if not ReadPalette(stream) then exit;
          end;
    end;

    if Assigned(OnCreateImage) then
      OnCreateImage(Self,Img);

    if (MinifyHeight > 0) and (FHeight > MinifyHeight) then
      FOutputHeight:= MinifyHeight
    else
      if WantedHeight > 0 then
        FOutputHeight:= WantedHeight
      else
        FOutputHeight:= FHeight;
    Img.SetSize(FWidth,FOutputHeight);

    //  color palette
    BufSize:=0;
    Stream.Read(BufSize, SizeOf(BufSize));
    BufSize:=BEtoN(BufSize);
    Stream.Seek(BufSize, soCurrent);
    //  color data block
    Stream.Read(BufSize, SizeOf(BufSize));
    BufSize:=BEtoN(BufSize);
    Stream.Read(FColorDataBlock, SizeOf(FColorDataBlock));
    Stream.Seek(BufSize-SizeOf(FColorDataBlock), soCurrent);
    //  mask
    Stream.Read(BufSize, SizeOf(BufSize));
    BufSize:=BEtoN(BufSize);
    Stream.Seek(BufSize, soCurrent);
    //  compression type
    Encoding:=0;
    Stream.Read(Encoding, SizeOf(Encoding));
    FCompressed:=BEtoN(Encoding) = 1;
    if BEtoN(Encoding)>1 then
      Raise Exception.Create('Unknown compression type');
    If FCompressed then
    begin
      SetLength(FLengthOfLine, FHeight * FChannelCount);
      Stream.ReadBuffer(FLengthOfLine[0], 2 * Length(FLengthOfLine));
      Progress(FPimage.psRunning, 0, False, Rect(0,0,0,0), '', ContProgress);
      if not ContProgress then exit;
      if not (FHeader.Mode in [0, 2]) then
        LenOfLineFactor := FHeader.Depth div 8
      else
        LenOfLineFactor := 1;
    end else
    begin
      FLengthOfLine := nil;
    end;

    setlength(FInputLine, FHeight * FChannelCount);
    CurOffset := Stream.Position;
    H := 0;
    channel := 0;
    InputLineIndex:= 0;
    for LenOfLineIndex := 0 to FHeight * FChannelCount-1 do
    begin
      FInputLine[InputLineIndex].StreamOffset := CurOffset;
      if FLengthOfLine <> nil then
        FInputLine[InputLineIndex].Size := BEtoN(FLengthOfLine[LenOfLineIndex])*LenOfLineFactor else
        FInputLine[InputLineIndex].Size := FLineSize;
      inc(CurOffset, FInputLine[InputLineIndex].Size);
      inc(H);
      Inc(InputLineIndex, FChannelCount);
      if H = FHeight then
      begin
        H := 0;
        Inc(channel);
        InputLineIndex:= channel;
      end;
    end;

    InputLineIndex := 0;
    PrevOutputRow := -1;
    OutputRow := 0;
    OutputRowAdd := FOutputHeight div FHeight;
    OutputRowMod:= FHeight;
    OutputRowAccAdd := FOutputHeight mod FHeight;
    OutputRowAcc:= 0;

    For H := 0 to FHeight - 1 do
    begin
      if OutputRow > PrevOutputRow then
      begin
        for channel := 0 to FChannelCount-1 do
        begin
            Stream.Position := FInputLine[InputLineIndex].StreamOffset;
            ReadScanLine(Stream, FInputLine[InputLineIndex].Size, channel);
            Inc(InputLineIndex);
        end;
        For HOutput:= PrevOutputRow+1 to OutputRow do WriteScanLine(Img, HOutput);
        Progress(FPimage.psRunning, round((H+1)*99.0 / (FHeight * FChannelCount)), False, Rect(0,0,0,0), '', ContProgress);
        if not ContProgress then exit;
      end else inc(InputLineIndex, FChannelCount);

      PrevOutputRow:= OutputRow;
      Inc(OutputRow, OutputRowAdd);
      Inc(OutputRowAcc, OutputRowAccAdd);
      if OutputRowAcc> OutputRowMod then
      begin
        dec(OutputRowAcc, OutputRowMod);
        inc(OutputRow);
      end;
    end;
    Progress(FPimage.psRunning, 100, False, Rect(0,0,0,0), '', ContProgress);
    if not ContProgress then exit;

   {$ifdef FPC_Debug_Image}
    WriteLn('TBGRAReaderPSD.InternalRead AAA1 ',Stream.position,' ',Stream.size);
    {$endif}
  finally
    FreeAndNil(FPalette);
    for channel := 0 to FChannelCount-1 do
      ReAllocMem(FScanLines[channel],0);
  end;
  Progress(FPimage.psEnding, 100, false, Rect(0,0,FWidth,FHeight), '', ContProgress);
end;

function TBGRAReaderPSD.ReadScanLine(Stream: TStream; AInputSize: PtrInt;
  AChannel: integer): boolean;
Var
  P : PByte;
  B : Byte;
  I, left : PtrInt;
  N : Shortint;
  Count:integer;
  buf, PBuf: PByte;
begin
  Result:=false;
  If Not Compressed then
    Stream.ReadBuffer(FScanLines[AChannel]^,FLineSize)
  else
    begin
      getmem(buf, AInputSize);
      if stream.Read(buf^,AInputSize) <> AInputSize then
      begin
        freemem(buf);
        result := false;
        exit;
      end;
      P:=FScanLines[AChannel];
      left := FLineSize;
      i:=AInputSize;
      PBuf := buf;
      repeat
        Count:=0;
        N:= PShortInt(PBuf)^;
        inc(PBuf);
        dec(i);
        If N = -128 then
        else
        if N < 0 then
        begin
           Count:=-N+1;
           if Count > left then Count := left;
           dec(left,Count);
           B:= PBuf^;
           Inc(PBuf);
           dec(i);
           fillchar(p^,count,B);
           inc(p,count);
        end
        else
        begin
           Count:=N+1;
           if Count > left then Count := left;
           dec(left,Count);
           Move(PBuf^, P^, Count);
           Inc(PBuf, Count);
           inc(p, count);
           dec(i, count);
        end;
      until (i <= 0) or (left <= 0);
      freemem(buf);
    end;
  Result:=true;
end;

function Value32To16(p: PDWord; gamma: single): Word;
var v: single;
begin
  v := (BEtoN(P^) - 1024000000)/40960000;
  if v <= 0 then result := 0 else
    if v >= 1 then result := 65535 else
      result := round(exp(ln(v)*gamma)*65535);
end;

procedure TBGRAReaderPSD.WriteScanLine(Img: TFPCustomImage; Row: integer);
Var
  Col : Integer;
  C   : TFPColor;
  P, P1, P2, P3   : PByte;
  Lab : TLab;
begin
  C.Alpha:=AlphaOpaque;
  P:=FScanLines[0];
  begin
    case FBytesPerPixel of
      1 : begin
             for Col:=0 to Img.Width-1 do
               if (P[col div 8] and (128 shr (col mod 8))) <> 0 then
                 Img.Colors[Col,Row]:=ThePalette[0]
  	       else
                 Img.Colors[Col,Row]:=ThePalette[1];
           end;
      8 : begin
             for Col:=0 to Img.Width-1 do
             begin
               Img.Colors[Col,Row]:=ThePalette[P[0]];
               inc(p);
             end;
          end;
      16 : if (FHeader.Mode = 1) or (FHeader.Mode = 8) then
           begin
             if FChannelCount = 1 then
               for Col:=0 to Img.Width-1 do
               begin
                 C.Red:=BEtoN(PWord(P)^);
                 C.green:=C.Red;
                 C.blue:=C.Red;
                 C.alpha:=65535;
                 Img[col, row] := C;
                 Inc(P,2);
               end else
             if FChannelCount = 2 then
             begin
               P1:=FScanLines[1];
               for Col:=0 to Img.Width-1 do
               begin
                 C.Red:=P^ shl 8 + P^;
                 C.green:=C.Red;
                 C.blue:=C.Red;
                 C.alpha:=p1^ shl 8 + P1^;
                 Img[col, row] := C;
                 Inc(P);
                 Inc(P1);
               end;
             end;
           end else
           begin
             for Col:=0 to Img.Width-1 do
             begin
               Img.Colors[Col,Row]:=ThePalette[BEtoN(PWord(P)^)];
               inc(p,2);
             end;
          end;
      24 : if FChannelCount>=3 then
           begin
           P1:=FScanLines[1];
           P2:=FScanLines[2];
           for Col:=0 to Img.Width-1 do
           begin
             if (FHeader.Mode =9) then
             begin
               Lab.L:=P[0];
               Lab.a:=P1[0];
               Lab.b:=P2[0];
               C:=LabToRGB(Lab);
             end
             else
              With C do
              begin
                Red:=P[0] or (P[0] shl 8);
                green:=P1[0] or (P1[0] shl 8);
                blue:=P2[0] or (P2[0] shl 8);
                alpha:=alphaOpaque;
              end;
              Inc(P);
              Inc(P1);
              Inc(P2);
              Img[col, row] := C;
           end;
          end;
      32 : if (FHeader.Mode = 1) or (FHeader.Mode = 8) then
           begin
             if FChannelCount = 1 then
               for Col:=0 to Img.Width-1 do
               begin
                 C.Red:=Value32To16(PDWord(P),1.3);
                 C.green:=C.Red;
                 C.blue:=C.Red;
                 C.alpha:=65535;
                 Img[col, row] := C;
                 Inc(P,4);
               end else
             if FChannelCount = 2 then
             begin
               P1:=FScanLines[1];
               for Col:=0 to Img.Width-1 do
               begin
                 C.Red:=BEtoN(PWord(P)^);
                 C.green:=C.Red;
                 C.blue:=C.Red;
                 C.alpha:=BEtoN(PWord(p1)^);
                 Img[col, row] := C;
                 Inc(P,2);
                 Inc(P1,2);
               end;
             end;
           end else
           if FChannelCount >= 4 then
           begin
           P1:=FScanLines[1];
           P2:=FScanLines[2];
           P3:=FScanLines[3];
           for Col:=0 to Img.Width-1 do
           begin
             if (FHeader.Mode =4) then
             begin
                 P^ := 255 - P^;
                 P1^ := 255 - P1^;
                 P2^ := 255 - P2^;
                 P3^ := 255 - P3^;
             end;
             C.Red:=P[0] or (P[0] shl 8);
             C.green:=P1[0] or (P1[0] shl 8);
             C.blue:=P2[0] or (P2[0] shl 8);
             C.alpha:=P3[0] or (P3[0] shl 8);
             if (FHeader.Mode =4) then  C:=CMYKtoRGB(C); // CMYK to RGB
             Img[col, row] := C;
             Inc(P);
             Inc(P1);
             Inc(P2);
             Inc(P3);
           end;
          end;
      48 :if FChannelCount = 3 then
          begin
           P1:=FScanLines[1];
           P2:=FScanLines[2];
           C.alpha:=alphaOpaque;
           for Col:=0 to Img.Width-1 do
           begin
              if (FHeader.Mode =9) then
                C := LabToRGB(BEtoN(PWord(P)^)/65535, (BEtoN(PWord(P1)^)-32768)/32767, (BEtoN(PWord(P2)^)-32768)/32767)
              else
              With C do
              begin
                Red:=BEtoN(PWord(P)^);
                green:=BEtoN(PWord(P1)^);
                blue:=BEtoN(PWord(P2)^);
              end;
              Inc(P,2);
              Inc(P1,2);
              Inc(P2,2);
              Img[col, row] := C;
           end;
          end;
      64 : if FChannelCount = 4 then
           begin
           P1:=FScanLines[1];
           P2:=FScanLines[2];
           P3:=FScanLines[3];
           for Col:=0 to Img.Width-1 do
           begin
             C.Red:=BEtoN(PWord(P)^);
             C.green:=BEtoN(PWord(P1)^);
             C.blue:=BEtoN(PWord(P2)^);
             C.alpha:=BEtoN(PWord(P3)^);
             if (FHeader.Mode =4) then
             begin
                 C.red:=$ffff-C.red;
                 C.green:=$ffff-C.green;
                 C.blue:=$ffff-C.blue;
                 C.alpha:=$ffff-C.alpha;
             end;
             if (FHeader.Mode =4) then  C:=CMYKtoRGB(C); // CMYK to RGB
             Img[col, row] := C;
             Inc(P,2);
             Inc(P1,2);
             Inc(P2,2);
             Inc(P3,2);
           end;
          end;
      96 :if FChannelCount = 3 then
          begin
           P1:=FScanLines[1];
           P2:=FScanLines[2];
           C.alpha:=alphaOpaque;
           for Col:=0 to Img.Width-1 do
           begin
              With C do
              begin
                Red:=Value32To16(PDWord(P),2.7);
                green:=Value32To16(PDWord(P1),2.7);
                blue:=Value32To16(PDWord(P2),2.7);
              end;
              Inc(P,4);
              Inc(P1,4);
              Inc(P2,4);
              Img[col, row] := C;
           end;
          end;
    end;
  end;
end;

function TBGRAReaderPSD.InternalCheck(Stream: TStream): boolean;
var
  OldPos: Int64;
begin
  try
    OldPos:=Stream.Position;
    Stream.Read(FHeader,SizeOf(FHeader));
    Result:=(FHeader.Signature = '8BPS');
    Stream.Position:=OldPos;
  except
    Result:=False;
  end;
end;

constructor TBGRAReaderPSD.Create;
begin
  inherited Create;
end;

initialization

  DefaultBGRAImageReader[ifPsd] := TBGRAReaderPSD;

end.
