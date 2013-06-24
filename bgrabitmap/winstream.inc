function WinReadLongint(Stream: TStream): longint;
begin
  Result := 0;
  stream.Read(Result, sizeof(Result));
  Result := LEtoN(Result);
end;

procedure WinWriteLongint(Stream: TStream; AValue: LongInt);
begin
  AValue := NtoLE(AValue);
  stream.Write(AValue, sizeof(AValue));
end;

function WinReadByte(Stream: TStream): byte;
begin
  Result := 0;
  stream.Read(Result, sizeof(Result));
end;

procedure WinWriteByte(Stream: TStream; AValue: Byte);
begin
  stream.Write(AValue, sizeof(AValue));
end;

function WinReadSingle(Stream: TStream): single;
var
  ResultAsDWord : longword absolute result;
begin
  ResultAsDWord := 0;
  stream.Read(ResultAsDWord, sizeof(Result));
  ResultAsDWord := LEtoN(ResultAsDWord);
end;

procedure WinWriteSingle(Stream: TStream; AValue: single);
var
  ValueAsDWord : longword absolute AValue;
begin
  ValueAsDWord := NtoLE(ValueAsDWord);
  stream.Write(ValueAsDWord, sizeof(AValue));
end;

procedure WinWritePointF(Stream: TStream; AValue: TPointF);
begin
  WinWriteSingle(Stream,AValue.x);
  WinWriteSingle(Stream,AValue.y);
end;

function WinReadPointF(Stream: TStream): TPointF;
begin
  result.x := WinReadSingle(Stream);
  result.y := WinReadSingle(Stream);
end;

