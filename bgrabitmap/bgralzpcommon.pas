unit BGRALzpCommon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  LAZPAINT_COMPRESSION_MODE_ZSTREAM = 1;
  LAZPAINT_COMPRESSION_MODE_RLE = 2;
  LAZPAINT_COMPRESSION_MASK = 255;
  LAZPAINT_THUMBNAIL_PNG = 256;
  LAZPAINT_MAGIC_HEADER : array[0..7] of char = 'LazPaint';

  LazpaintChannelGreenFromRed  = 1;
  LazpaintChannelBlueFromRed   = 2;
  LazpaintChannelBlueFromGreen = 4;
  LazpaintChannelNoAlpha       = 8;
  LazpaintPalettedRGB          = 16;

  LazPaintThumbMaxWidth = 128;
  LazPaintThumbMaxHeight = 128;

type
  TLzpCompression = (lzpZStream, //slower and not necessarily better
                     lzpRLE);    //custom RLE for lzp files

  { TLazPaintImageHeader }

  TLazPaintImageHeader = packed record
    magic: packed array[0..7] of char;
    zero1, headerSize: DWord;
    width, height, nbLayers, previewOffset: DWord;
    zero2, compressionMode, reserved1, layersOffset: DWord;
  end;

procedure LazPaintImageHeader_SwapEndianIfNeeded(AHeader: TLazPaintImageHeader);

//routines to compress and uncompress byte-sized values (you need to
//separate the channels to obtain any compression)

procedure EncodeLazRLE(var sourceBuffer; size:PtrInt; ADest: TStream);
function DecodeLazRLE(ASource: TStream; var destBuffer; availableOutputSize: PtrInt; availableInputSize: int64 = -1): PtrInt;

implementation

uses bufstream;

const //flag to distinguish ranges of opcodes
      simpleRepetitionFlag = $00;    // $01..$3f: normal repetition
      packedRepetitionFlag = $40;    // $41..$5f: packed repetition
      repetitionOf0Flag    = $60;    // $60..$6f: repeat 1..16 zeros
      repetitionOf255Flag  = $70;    // $70..$7f: repeat 1..16 values of 255
      simpleDumpFlag       = $80;    // $81: dump of size as byte+64,   $82..$bf: simple dump (2..63)
      packedDumpFlag       = $c0;    // $c3..$df: packed dump (3..31)
      packedDumpFromLastFlag= $e0;   // $e2..$fe: packed dump from last packed dump value (2..30)

      //special opcodes
      wordRepetitionOpCode             = $00; //followed by word and then a value to repeat
      byteRepetitionOpCode             = $40; //followed by byte (add 64 to it to get the repetition count) and then a value to repeat
      previousWordSizeRepetitionOpCode = $80; //use the last value of opcode $00 so only followed by value to repeat
      previousByteSizeRepetitionOpCode = $c0; //use the last value of opcode $80 so only followed by value to repeat
      endOfStreamOpCode                = $e0; //end of RLE stream (not necessarily the end of the image)

      //for future use but must not be accepted in the input stream
      {%H-}reservedOpCode1 = $c1;
      {%H-}reservedOpCode2 = $c2;
      {%H-}reservedOpCode3 = $e1;

      {%H-}optionalOpCode = $ff;  //for future use but should be ignored if not recognized

      //numeric information
      maxNormalRepetition = 63;
      maxSmallRepCount    = 31 * 4;  // sets of four packed repetition
      maxDumpCount        = 255+32;

      maxRepetition = 65535;         // normal: 1..63, byte+64: 64..319, word-sized: 0..65535
      minSmallRep = 1;
      maxSmallRep = minSmallRep+3;


procedure EncodeLazRLE(var sourceBuffer; size:PtrInt; ADest: TStream);
var
  smallRepetitions: array[0..maxSmallRepCount-1] of record
    value: NativeInt;
    count: NativeInt; //minSmallRep..maxSmallRep
  end;
  smallRepetitionsCount, smallRepTotal: NativeInt;
  buf: TStream;
  previousWordSizeRepetition, previousByteSizeRepetition: NativeInt;
  lastPackedDumpValue: NativeInt;

  procedure OutputNormalRepetition(AValue,ACount: NativeInt);
  begin
    If (ACount < 1) or (ACount > maxNormalRepetition) then
      raise exception.Create('Invalid count');

    if (AValue = 0) and (ACount <= 16) then
      begin
        buf.WriteByte((ACount-1) or repetitionOf0Flag);
      end else
    if (AValue = 255) and (ACount <= 16) then
      begin
        buf.WriteByte((ACount-1) or repetitionOf255Flag);
      end else
      begin
        buf.WriteByte(ACount or simpleRepetitionFlag);
        buf.WriteByte(AValue);
      end;
  end;

  procedure FlushSmallRepetitions;
  var i,j: NativeInt;
    packedCount: NativeInt;
    smallOutput: NativeInt;
  begin
    if smallRepetitionsCount = 0 then exit;
    if smallRepetitionsCount >= 4 then
    begin
      smallOutput:= smallRepetitionsCount and not 3;
      buf.Writebyte(packedRepetitionFlag or (smallOutput shr 2));
      packedCount := 0;
      for i := 0 to smallOutput-1 do
      begin
        packedCount := packedCount + ((smallRepetitions[i].count-minSmallRep) shl ((i and 3) shl 1));
        if (i and 3) = 3 then
        begin
          buf.WriteByte(packedCount);
          for j := i-3 to i do
            buf.WriteByte(smallRepetitions[j].value);
          packedCount:= 0;
        end;
      end;
      for i := smallOutput to smallRepetitionsCount-1 do
        OutputNormalRepetition(smallRepetitions[i].value,smallRepetitions[i].count);
    end else
    begin
      for i := 0 to smallRepetitionsCount-1 do
        OutputNormalRepetition(smallRepetitions[i].value,smallRepetitions[i].count);
    end;
    smallRepetitionsCount := 0;
    smallRepTotal := 0;
  end;

  procedure OutputRepetition(AValue,ACount: NativeInt; AAccumulate: boolean = true);
  begin
    if AAccumulate and (ACount >= minSmallRep) and (ACount <= maxSmallRep) and (maxSmallRepCount>0) then
    begin
      if (smallRepetitionsCount> 0) and (smallRepetitions[smallRepetitionsCount-1].value = AValue) and
        (smallRepetitions[smallRepetitionsCount-1].count+ACount <= maxSmallRepCount) then
      begin
        inc(smallRepetitions[smallRepetitionsCount-1].count, ACount);
        exit;
      end;
      if smallRepetitionsCount = maxSmallRepCount then
        FlushSmallRepetitions;
      if smallRepetitionsCount and 3 = 0 then smallRepTotal := 0;
      smallRepetitions[smallRepetitionsCount].value := AValue;
      smallRepetitions[smallRepetitionsCount].count := ACount;
      inc(smallRepetitionsCount);
      inc(smallRepTotal, ACount);
    end else
    begin
      flushSmallRepetitions;
      if ACount <= maxNormalRepetition then
        OutputNormalRepetition(AValue,ACount) else
      begin
        if ACount = previousWordSizeRepetition then
        begin
          buf.WriteByte(previousWordSizeRepetitionOpCode);
          buf.WriteByte(AValue);
        end else
        if ACount = previousByteSizeRepetition then
        begin
          buf.WriteByte(previousByteSizeRepetitionOpCode);
          buf.WriteByte(AValue);
        end else
        if ACount <= 64+255 then
        begin
          buf.WriteByte(byteRepetitionOpCode);
          buf.WriteByte(ACount-64);
          buf.WriteByte(AValue);
          previousByteSizeRepetition := ACount;
        end else
        if ACount <= 65535 then
        begin
          buf.WriteByte(wordRepetitionOpCode);
          buf.WriteByte(ACount shr 8);
          buf.WriteByte(ACount and 255);
          buf.WriteByte(AValue);
          previousWordSizeRepetition := ACount;
        end else
          raise exception.Create('Invalid count');
      end;
    end;
  end;

  procedure DumpNoPack(P: PByte; ACount: NativeInt);
  begin
    if ACount = 0 then exit;
    if ACount = 1 then
    begin
      OutputNormalRepetition(p^,1);
      exit;
    end;
    If (ACount < 0) or (ACount > maxDumpCount) then
      raise exception.Create('Invalid count');

    if ACount > 63 then
    begin
      if ACount > 255+64 then
        raise exception.Create('Invalid count');
      buf.WriteByte($01 or simpleDumpFlag);
      buf.WriteByte(ACount-64);
    end else
      buf.WriteByte(ACount or simpleDumpFlag);

    buf.Write(p^, ACount);
  end;

  procedure DumpPacked(p : PByte; ACount: NativeInt);
  var diffLast: integer;
      packedValues: array[0..31] of NativeInt;
      nbPackedValues, idx: NativeInt;

  begin
    if ACount = 0 then exit else
    if ACount = 1 then
    begin
      OutputNormalRepetition(p^,1);
      exit;
    end else
    if ACount = 2 then
    begin
      DumpNoPack(p, ACount);
      exit;
    end;
    If (ACount < 3) or (ACount > maxDumpCount) then
      raise exception.Create('Invalid count');

    diffLast := p^ - lastPackedDumpValue;
    if (diffLast < -7) or (diffLast > 7) then
    begin
      if ACount > 31 then
      begin
        DumpPacked(p, 31);
        DumpPacked(p+31, ACount-31);
        exit;
      end;
      buf.WriteByte(ACount or packedDumpFlag);
      lastPackedDumpValue:= p^;
      buf.WriteByte(lastPackedDumpValue);
      dec(ACount);
      inc(p);
    end else
    if ACount > 30 then
    begin
      while ACount > 30 do
      begin
        DumpPacked(p, 30);
        inc(p,30);
        dec(ACount,30);
      end;
      DumpPacked(p, ACount);
      exit;
    end else
      buf.WriteByte(ACount or packedDumpFromLastFlag);

    nbPackedValues := 0;
    while ACount >0 do
    begin
      packedValues[nbPackedValues] := (p^ - lastPackedDumpValue + 8) and 15;
      inc(nbPackedValues);
      lastPackedDumpValue := p^;
      inc(p);
      dec(ACount);
    end;

    idx := 0;
    while idx < nbPackedValues do
    begin
      if idx+1 = nbPackedValues then
      begin
        buf.WriteByte(packedValues[idx] shl 4);
        break;
      end;
      buf.WriteByte((packedValues[idx] shl 4) + packedValues[idx+1]);
      inc(idx,2);
    end;
  end;

  procedure Dump(p: PByte; ACount: NativeInt);
  const smallestPackedDump = 5;
        smallestPackedDumpTail = 3;
  var
    diffVal,i: NativeInt;
    fitPackStart: NativeInt;
    p2: PByte;
  begin
    if ACount >= smallestPackedDump then
    begin
      p2 := p+1;
      fitPackStart := -1;
      for i := 1 to ACount-1 do
      begin
        diffVal := p2^ - (p2-1)^;
        if diffVal > 128 then dec(diffVal,256)
        else if diffVal < -128 then inc(diffVal,256);
        if (diffVal > 7) or (diffVal < -7) then
        begin
          if (fitPackStart <> -1) and
               ((i-fitPackStart+1 >= smallestPackedDump) or
                ((i-fitPackStart+1 >= smallestPackedDumpTail) and
                 (fitPackStart = 1) )) then
          begin
            DumpNoPack(p, fitPackStart-1);
            DumpPacked(p+(fitPackStart-1), i-fitPackStart+1);
            Dump(p+i, ACount-i);
            exit;
          end;
          fitPackStart := -1;
        end else
        if fitPackStart = -1 then fitPackStart := i;
        inc(p2);
      end;
      if (fitPackStart <> -1) and (ACount-fitPackStart+1 >= smallestPackedDumpTail) then
      begin
        DumpNoPack(p,fitPackStart-1);
        DumpPacked(p+(fitPackStart-1), ACount-fitPackStart+1);
        exit;
      end;
      ACount := ACount;
    end;
    DumpNoPack(p, ACount);
  end;

var
  psrc,psrcBefore: PByte;
  curValue: NativeInt;
  curCount: NativeInt;
begin
  if size = 0 then exit;
  psrc := @sourceBuffer;
  if psrc = nil then
    raise exception.Create('Source buffer not provided');
  buf := TWriteBufStream.Create(ADest,4096);
  curValue := psrc^;
  curCount := 1;
  inc(psrc);
  dec(size);
  smallRepetitionsCount := 0;
  smallRepTotal := 0;
  previousWordSizeRepetition := 0;
  previousByteSizeRepetition := 0;
  lastPackedDumpValue:= $80;
  while size > 0 do
  begin
    if (psrc^ = curValue) and (curCount < maxRepetition) then
    begin
      inc(curCount);
      dec(size);
      inc(psrc);
    end else
    if (curCount > 1) or (((smallRepetitionsCount and 3) <> 0) and (smallRepTotal >= 5)) then
    begin
      outputRepetition(curValue,curCount);
      curCount := 1;
      curValue := psrc^;
      dec(size);
      inc(psrc);
    end else //curCount = 1
    begin
      psrcBefore := psrc-1;
      inc(psrc);
      inc(curCount);
      dec(size);
      while (curCount < maxDumpCount) and (size>0) and
            ( (psrc^ <> (psrc-1)^) or ((size>1) and ((psrc+1)^ <> (psrc-1)^)) ) do //eat doubles too
      begin
        inc(psrc);
        inc(curCount);
        dec(size);
        if (curCount > 12) and ((psrc-1)^ = (psrc-2)^) and
          ((psrc-3)^ = (psrc-4)^) and ((psrc-5)^ = (psrc-6)^) then //three doubles
        begin
          dec(psrc,6);
          dec(curCount,6);
          inc(size,6);
          break;
        end;
      end;
      if (size > 1) and (psrc^ = (psrc-1)^) and ((psrc+1)^ = psrc^) then //repetition coming
      begin
        dec(psrc);
        dec(curCount);
        inc(size);
      end;
      FlushSmallRepetitions;
      if curCount = 1 then OutputRepetition(psrcBefore^,1) else
      if curCount > 1 then Dump(psrcBefore, curCount);
      if size > 0 then
      begin
        curValue := psrc^;
        curCount := 1;
        inc(psrc);
        dec(size);
      end else
      begin
        curCount := 0;
        break;
      end;
    end;
  end;
  if curCount > 0 then OutputRepetition(curValue,curCount);
  FlushSmallRepetitions;
  buf.WriteByte(endOfStreamOpCode);
  buf.Free;
end;

function DecodeLazRLE(ASource: TStream; var destBuffer; availableOutputSize: PtrInt; availableInputSize: int64 = -1): PtrInt;
const MaxBufferSize = 1024;
var
  opCode: NativeInt;
  pdest: PByte;
  lastRepeatWordSize, lastRepeatByteSize: NativeInt;
  lastPackedDumpValue: NativeInt;

  Buffer: packed array of byte;
  BufferPos, BufferSize: NativeInt;
  BufferStartLocation: Int64;

  procedure OutputOverflow(AWanted: PtrInt; AFunctionName: string);
  var position: int64;
  begin
    position := ASource.Position - BufferSize + BufferPos;
    raise exception.Create('Output buffer overflow. Current position is ' + IntToStr(result)+' out of '+ IntToStr(availableOutputSize)+
    ' and '+IntToStr(AWanted)+' is required by '+AFunctionName+'. ' +
    'The absolute input position is '+IntToStr(position)+' which is ' + inttostr(position-BufferStartLocation) + ' from start.');
  end;

  function ReduceAvailableInputSize(AWanted: PtrInt): PtrInt;
  begin
    if availableInputSize <> -1 then
    begin
      if AWanted>availableInputSize then
        result := availableInputSize
      else
        result := AWanted;
      dec(availableInputSize, result);
    end else
      result := AWanted;
  end;

  function GetNextBufferByte: byte;
  begin
    if BufferPos < BufferSize then
    begin
      result := Buffer[BufferPos];
      inc(BufferPos);
    end else
    if BufferSize = 0 then
      result := $e0
    else
    begin
      BufferSize := ASource.Read(Buffer[0],ReduceAvailableInputSize(length(Buffer)));
      BufferPos := 0;
      if BufferPos < BufferSize then
      begin
        result := Buffer[BufferPos];
        inc(BufferPos);
      end else
        result := $e0;
    end;
  end;

  procedure RepeatValue(AValue: NativeInt; ACount: NativeInt);
  begin
    if result+ACount > availableOutputSize then OutputOverflow(ACount,'RepeatValue');
    fillchar(pdest^, ACount, AValue);
    inc(pdest, ACount);
    inc(result, ACount);
  end;

  procedure PackedRepeatValues(ACount: NativeInt);
  var packedCount: NativeInt;
  begin
    while ACount > 0 do
    begin
      packedCount := GetNextBufferByte;
      RepeatValue(GetNextBufferByte, (packedCount and 3) + 1);
      packedCount:= packedCount shr 2;
      RepeatValue(GetNextBufferByte, (packedCount and 3) + 1);
      packedCount:= packedCount shr 2;
      RepeatValue(GetNextBufferByte, (packedCount and 3) + 1);
      packedCount:= packedCount shr 2;
      RepeatValue(GetNextBufferByte, (packedCount and 3) + 1);
      dec(ACount);
    end;
  end;

  procedure DumpValues(ACount: NativeInt);
  begin
    if result+ACount > availableOutputSize then OutputOverflow(ACount, 'DumpValues');
    inc(result, ACount);
    while ACount > 0 do
    begin
      pdest^ := GetNextBufferByte;
      inc(pdest);
      dec(ACount);
    end;
  end;

  procedure PackedDumpValues(ACount: NativeInt);
  var packedData: NativeInt;
  begin
    if result+ACount > availableOutputSize then OutputOverflow(ACount, 'PackedDumpValues');
    inc(result, ACount);
    while ACount > 0 do
    begin
      packedData := GetNextBufferByte;
      lastPackedDumpValue := (lastPackedDumpValue + (packedData shr 4) - 8) and 255;
      pdest^ := lastPackedDumpValue;
      if ACount >= 2 then
      begin
        lastPackedDumpValue := (lastPackedDumpValue + (packedData and 15) - 8) and 255;
        (pdest+1)^ := lastPackedDumpValue;
        inc(pdest,2);
        dec(ACount,2);
      end else
      begin
        inc(pdest);
        dec(ACount);
      end;
      lastPackedDumpValue:= (pdest-1)^;
    end;
  end;
begin
  BufferStartLocation:= ASource.Position;
  setLength(Buffer,MaxBufferSize);
  BufferSize := ASource.Read(Buffer[0],ReduceAvailableInputSize(length(Buffer)));
  BufferPos := 0;

  pdest := @destBuffer;
  result := 0;
  lastPackedDumpValue:= $80;
  try
    repeat
      opCode := GetNextBufferByte;
      case opCode of
      wordRepetitionOpCode: begin
          lastRepeatWordSize:= GetNextBufferByte shl 8;
          lastRepeatWordSize+= GetNextBufferByte;
          RepeatValue(GetNextBufferByte, lastRepeatWordSize);
        end;
      previousWordSizeRepetitionOpCode: RepeatValue(GetNextBufferByte, lastRepeatWordSize);
      byteRepetitionOpCode: begin
          lastRepeatByteSize:= GetNextBufferByte + 64;
          RepeatValue(GetNextBufferByte, lastRepeatByteSize);
        end;
      previousByteSizeRepetitionOpCode: RepeatValue(GetNextBufferByte, lastRepeatByteSize);

      $01..$3f: RepeatValue(GetNextBufferByte, opCode);
      $41..$5f: PackedRepeatValues(opCode - $40);
      $60..$6f: RepeatValue($00, opCode - $60 + 1);
      $70..$7f: RepeatValue($FF, opCode - $70 + 1);
      $81:      DumpValues(GetNextBufferByte+64);
      $82..$bf: DumpValues(opCode - $80);
      $c3..$df: begin
          lastPackedDumpValue := GetNextBufferByte;
          RepeatValue(lastPackedDumpValue, 1);
          PackedDumpValues(opCode - $c0 - 1);
        end;
      $e2..$fe: PackedDumpValues(opCode - $e0);

      reservedOpCode1, reservedOpCode2, reservedOpCode3: raise exception.Create('Unexpected opcode');
      endOfStreamOpCode, optionalOpCode: ;
      end;
    until opCode = endOfStreamOpCode;
  finally
    ASource.Position:= ASource.Position-BufferSize+BufferPos;
  end;
end;

{ TLazPaintImageHeader }

procedure LazPaintImageHeader_SwapEndianIfNeeded(AHeader: TLazPaintImageHeader);
begin
  with AHeader do
  begin
    headerSize := LEtoN(headerSize);
    width := LEtoN(width);
    height := LEtoN(height);
    nbLayers := LEtoN(nbLayers);
    height := LEtoN(height);
    previewOffset := LEtoN(previewOffset);
    compressionMode := LEtoN(compressionMode);
    layersOffset := LEtoN(layersOffset);
  end;
end;


end.
