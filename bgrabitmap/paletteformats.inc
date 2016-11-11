var
  PaletteFormats : array of record
    formatIndex: TBGRAPaletteFormat;
    ext: string;
    description: string;
    reader: TPaletteReaderProc;
    writer: TPaletteWriterProc;
    checkFormat: TCheckPaletteFormatProc;
  end;

const
  GimpPaletteHeader : string = 'GIMP Palette';
  KOfficePaletteHeader : string = 'KDE RGB Palette';
  AdobeSwatchExchangeHeader : string = 'ASEF';
  JascPaletteHeader : string = 'JASC-PAL';
  PaintDotNetPaletteHeader : string = '; paint.net Palette File';
  PaintDotNetPaletteHeaderUTF8 : string = #$EF#$BB#$BF + '; paint.net Palette File';

procedure SaveToStreamAsPaintDotNet(APalette: TBGRAPalette; AStream: TStream);

  procedure WriteStr(s: string);
  begin
    AStream.WriteBuffer(s[1],length(S));
  end;
  procedure WriteStrLn(s: string);
  begin
    WriteStr(s+#$0D#$0A);
  end;

var
  i: Integer;

begin
  WriteStrLn(PaintDotNetPaletteHeaderUTF8);
  for i := 0 to APalette.Count-1 do
  with APalette.Color[i] do
    WriteStrLn(IntToHex(alpha,2)+IntToHex(red,2)+IntToHex(green,2)+IntToHex(blue,2));
end;

procedure SaveToStreamAsGimp(APalette: TBGRAPalette; AStream: TStream);
  procedure WriteStr(s: string);
  begin
    AStream.WriteBuffer(s[1],length(S));
  end;
  procedure WriteStrLn(s: string);
  begin
    WriteStr(s+#$0A);
  end;

  procedure WriteChannelValue(AValue: byte);
  var s: string;
  begin
    s := IntToStr(AValue);
    while length(s) < 3 do s := ' '+s;
    WriteStr(s);
  end;

var
  i: Integer;

begin
  WriteStrLn(GimpPaletteHeader);
  WriteStrLn('Name: Palette');
  WriteStrLn('Columns: 3');
  WriteStrLn('#');
  for i := 0 to APalette.Count-1 do
  with APalette.Color[i] do
  begin
    WriteChannelValue(red);
    WriteStr(' ');
    WriteChannelValue(green);
    WriteStr(' ');
    WriteChannelValue(blue);
    WriteStrLn(#$09+BGRAToStr(APalette.Color[i],CSSColors));
  end;
end;

procedure SaveToStreamAsAdobeSwatchExchange(APalette: TBGRAPalette; AStream: TStream);
  procedure WriteStr(s: string);
  begin
    AStream.WriteBuffer(s[1],length(S));
  end;
  procedure WriteInt32(AValue: Int32);
  begin
    AValue := NtoBE(AValue);
    AStream.WriteBuffer(AValue,sizeof(AValue));
  end;
  procedure WriteInt16(AValue: Int16);
  begin
    AValue := NtoBE(AValue);
    AStream.WriteBuffer(AValue,sizeof(AValue));
  end;
  procedure WriteSingle(AValue: Single);
  begin
    DWord(AValue) := BEtoN(DWord(AValue));
    AStream.WriteBuffer(AValue,sizeof(AValue));
  end;
  procedure WriteBlock(ABlockType: Int16; AContentLength: Int32);
  begin
    WriteInt16(ABlockType);
    WriteInt32(AContentLength);
  end;

  procedure WriteBlock(ABlockType: Int16; AName: string; AExtraContentLength: Int32);
  var contentLength: Int32;
    wideName: UnicodeString;
    nameBuf: array of byte;
    i: Integer;
  begin
    wideName := UTF8Decode(AName);
    setlength(nameBuf, (length(wideName)+1)*2);
    contentLength:= AExtraContentLength + 2 + length(nameBuf);
    WriteBlock(ABlockType, contentLength);
    WriteInt16(length(nameBuf) shr 1);
    for i := 1 to length(wideName) do
    begin
      nameBuf[((i-1) shl 1)] := ord(wideName[i]) shr 8;
      nameBuf[((i-1) shl 1)+1] := ord(wideName[i]) and 255;
    end;
    AStream.WriteBuffer(nameBuf[0],length(namebuf));
  end;

var
  i: Integer;

begin
  WriteStr(AdobeSwatchExchangeHeader+#$00#$01+#$00#$00);
  WriteInt32(APalette.Count+2); //number of blocks
  WriteBlock($1c0, 'Palette', 0); //group start
  for i := 0 to APalette.Count-1 do
  with APalette.Color[i] do
  begin
    WriteBlock(1, BGRAToStr(APalette.Color[i],CSSColors), 4+4*3+2);
    WriteStr('RGB ');
    WriteSingle(red/255);
    WriteSingle(green/255);
    WriteSingle(blue/255);
    WriteInt16(2); //normal
  end;
  WriteBlock($2c0, 0); //group end
end;

procedure SaveToStreamAsKOffice(APalette: TBGRAPalette; AStream: TStream);
  procedure WriteStr(s: string);
  begin
    AStream.WriteBuffer(s[1],length(S));
  end;
  procedure WriteStrLn(s: string);
  begin
    WriteStr(s+#$0A);
  end;

  procedure WriteChannelValue(AValue: byte);
  begin
    WriteStr(IntToStr(AValue));
  end;

var
  i: Integer;

begin
  WriteStrLn(KOfficePaletteHeader);
  for i := 0 to APalette.Count-1 do
  with APalette.Color[i] do
  begin
    WriteChannelValue(red);
    WriteStr(' ');
    WriteChannelValue(green);
    WriteStr(' ');
    WriteChannelValue(blue);
    WriteStrLn(#$09+BGRAToStr(APalette.Color[i],CSSColors));
  end;
end;

procedure SaveToStreamAsJasc(APalette: TBGRAPalette; AStream: TStream);
  procedure WriteStr(s: string);
  begin
    AStream.WriteBuffer(s[1],length(S));
  end;
  procedure WriteStrLn(s: string);
  begin
    WriteStr(s+#$0D#$0A);
  end;

var
  i: Integer;

begin
  WriteStrLn(JascPaletteHeader);
  WriteStrLn('0100');
  WriteStrLn(IntToStr(APalette.Count));
  for i := 0 to APalette.Count-1 do
  with APalette.Color[i] do
    WriteStrLn(IntToStr(red)+' '+IntToStr(green)+' '+IntToStr(blue));
end;

function LoadFromStreamAsPaintDotNet(APalette: TBGRAPalette; AStream: TStream): boolean;
var lines: TStringList;
  header,s: string;
  idxComment: integer;
  code: integer;
  hexArgb: int32;
  i: Integer;
begin
  result := false;
  lines := TStringList.Create;
  try
    lines.LoadFromStream(AStream);
    if lines.Count = 0 then
    begin
      lines.Free;
      exit;
    end;
    header := lines[0];
    if (header <> PaintDotNetPaletteHeader) and (header <> PaintDotNetPaletteHeaderUTF8) then
    begin
      lines.Free;
      exit;
    end;

    for i := 0 to lines.Count-1 do
    begin
      s := lines[i];
      idxComment := pos(';',s);
      if idxComment<>0 then s := copy(s,1,idxComment-1);
      s := trim(s);
      if length(s)> 0 then
      begin
        val('$'+s, hexArgb, code);
        if code = 0 then
          APalette.AddColor(BGRA((hexArgb shr 16) and 255,
                        (hexArgb shr 8) and 255,
                        hexArgb and 255,
                        (hexArgb shr 24) and 255));
      end;
    end;
    result := true;
  finally
    lines.Free;
  end;
end;

function LoadFromStreamAsGimp(APalette: TBGRAPalette; AStream: TStream): boolean;
var lines,line: TStringList;
  s: string;
  idxComment: integer;
  code: integer;
  c: TBGRAPixel;
  i: Integer;
begin
  result := false;
  lines := TStringList.Create;
  line := TStringList.Create;
  try
    lines.LoadFromStream(AStream);
    if (lines.Count < 3) or (lines[0] <> GimpPaletteHeader) or
      (copy(lines[1],1,6) <> 'Name: ') or
      (copy(lines[2],1,9) <> 'Columns: ') then
    begin
      lines.Free;
      line.Free;
      exit;
    end;
    for i := 3 to lines.Count-1 do
    begin
      s := lines[i];
      idxComment := pos('#',s);
      if idxComment<>0 then s := copy(s,1,idxComment-1);
      s := trim(s);
      if length(s)> 0 then
      begin
        line.CommaText := s;
        if line.Count >= 3 then
        begin
          c.alpha:= 255;
          val(line[0],c.red,code);
          if code <> 0 then continue;
          val(line[1],c.green,code);
          if code <> 0 then continue;
          val(line[2],c.blue,code);
          if code <> 0 then continue;
          APalette.AddColor(c);
        end;
      end;
    end;
    result := true;
  finally
    lines.Free;
    line.Free;
  end;
end;

function clamp(AValue, AMax: integer): integer;
begin
  if AValue < 0 then result := 0 else
  if AValue > AMax then result := AMax else
   result := AValue;;
end;

function LabToRGB(L,a,b: single): TBGRAPixel; overload;
var r,g,blue: single;
begin
  if a < 0 then
    r := L + a + 0.5*b
  else
    r := L + 0.75*a + 0.5*b;
  g := L - 0.5*a;
  blue := L - b;
  Result.red:= clamp(round((r)*255),255);
  Result.green:= clamp(round((g)*255),255);
  Result.blue:= clamp(round((blue)*255),255);
  result.alpha := 255;
end;

function LoadFromStreamAsAdobeSwatchExchange(APalette: TBGRAPalette; AStream: TStream): boolean;
  function ReadInt16: int16;
  begin
    {$PUSH}{$HINTS OFF}
    AStream.Read(result, sizeof(result));
    {$POP}
    result := BEtoN(result);
  end;
  function ReadInt32: int32;
  begin
    {$PUSH}{$HINTS OFF}
    AStream.Read(result, sizeof(result));
    {$POP}
    result := BEtoN(result);
  end;
  function ReadStr(ALength: integer): string;
  begin
    setlength(result, ALength);
    ALength := AStream.Read(result[1], ALength);
    setlength(result, ALength);
  end;
  function ReadSingle: single;
  begin
    {$PUSH}{$HINTS OFF}
    AStream.Read(Result, sizeof(result));
    {$POP}
    DWord(Result) := BEtoN(DWord(Result));
  end;
  function DblToByte(AValue: double): byte;
  begin
    if AValue < 0 then result := 0
    else if AValue > 1 then result := 255 else
      result := round(AValue*255);
  end;

var header: string;
  nbBlocks,blockSize: int32;
  blockType,nameLength: int16;
  nextPos: int64;
  colorFormat: string;
  colorF: TColorF;
  i: Integer;
begin
  result := false;
  header := ReadStr(length(AdobeSwatchExchangeHeader)+4);
  if header <> AdobeSwatchExchangeHeader+#$00#$01+#$00#$00 then exit;
  nbBlocks := ReadInt32;
  for i := 0 to nbBlocks-1 do
  begin
    blockType := ReadInt16;
    blockSize := ReadInt32;
    nextPos := AStream.Position + blockSize;
    if blockType = 1 then
    begin
      nameLength := ReadInt16;
      ReadStr(nameLength*2);
      colorFormat := ReadStr(4);
      if colorFormat = 'RGB ' then
      begin
        colorF[1] := ReadSingle;
        colorF[2] := ReadSingle;
        colorF[3] := ReadSingle;
        colorF[4] := 1;
        APalette.AddColor(BGRA(DblToByte(colorF[1]),DblToByte(colorF[2]),DblToByte(colorF[3])));
        ReadInt16; //ignore color type
      end else
      if colorFormat = 'CMYK' then
      begin
        colorF[1] := ReadSingle;
        colorF[2] := ReadSingle;
        colorF[3] := ReadSingle;
        colorF[4] := ReadSingle;

        APalette.AddColor(BGRA(DblToByte(1 - colorF[1] + ColorF[2]/10 + ColorF[3]/10 - ColorF[4]),
          DblToByte(1 - colorF[2] + ColorF[1]/10 + ColorF[3]/10 - ColorF[4]),
          DblToByte(1 - colorF[3] + ColorF[1]/10 + ColorF[2]/10 - ColorF[4])));
        ReadInt16; //ignore color type
      end else
      if colorFormat = 'LAB ' then
      begin
        colorF[1] := ReadSingle;
        colorF[2] := ReadSingle;
        colorF[3] := ReadSingle;
        colorF[4] := 1;

        APalette.AddColor(LabToRGB(colorF[1],colorF[2]/128,colorF[3]/128));
        ReadInt16; //ignore color type
      end else
      if colorFormat = 'GRAY' then
      begin
        colorF[1] := ReadSingle;
        colorF[2] := colorF[1];
        colorF[3] := colorF[1];
        colorF[4] := 1;
        APalette.AddColor(BGRA(DblToByte(colorF[1]),DblToByte(colorF[2]),DblToByte(colorF[3])));
        ReadInt16; //ignore color type
      end;
    end;
    if AStream.Position<>nextPos then
      AStream.Position:= nextPos;
  end;
  result := true;
end;

function LoadFromStreamAsKOffice(APalette: TBGRAPalette; AStream: TStream): boolean;
var lines,line: TStringList;
  s: string;
  idxComment: integer;
  code: integer;
  c: TBGRAPixel;
  i: Integer;
begin
  result := false;
  lines := TStringList.Create;
  line := TStringList.Create;
  try
    lines.LoadFromStream(AStream);
    if (lines.Count < 1) or (lines[0] <> KOfficePaletteHeader) then
    begin
      lines.Free;
      line.Free;
      exit;
    end;
    for i := 3 to lines.Count-1 do
    begin
      s := lines[i];
      idxComment := pos('#',s);
      if idxComment<>0 then s := copy(s,1,idxComment-1);
      s := trim(s);
      if length(s)> 0 then
      begin
        line.CommaText := s;
        if line.Count >= 3 then
        begin
          c.alpha:= 255;
          val(line[0],c.red,code);
          if code <> 0 then continue;
          val(line[1],c.green,code);
          if code <> 0 then continue;
          val(line[2],c.blue,code);
          if code <> 0 then continue;
          APalette.AddColor(c);
        end;
      end;
    end;
    result := true;
  finally
    lines.Free;
    line.Free;
  end;
end;

function LoadFromStreamAsJasc(APalette: TBGRAPalette; AStream: TStream): boolean;
var lines,line: TStringList;
  s: string;
  idxComment: integer;
  code: integer;
  c: TBGRAPixel;
  i: Integer;
begin
  result := false;
  lines := TStringList.Create;
  line := TStringList.Create;
  try
    lines.LoadFromStream(AStream);
    if (lines.Count < 2) or (lines[0] <> JascPaletteHeader) or
     (lines[1] <> '0100') then
    begin
      lines.Free;
      line.Free;
      exit;
    end;
    for i := 2 to lines.Count-1 do
    begin
      s := lines[i];
      idxComment := pos('#',s);
      if idxComment<>0 then s := copy(s,1,idxComment-1);
      s := trim(s);
      if length(s)> 0 then
      begin
        line.CommaText := s;
        if line.Count >= 3 then
        begin
          c.alpha:= 255;
          val(line[0],c.red,code);
          if code <> 0 then continue;
          val(line[1],c.green,code);
          if code <> 0 then continue;
          val(line[2],c.blue,code);
          if code <> 0 then continue;
          APalette.AddColor(c);
        end;
      end;
    end;
    result := true;
  finally
    lines.Free;
    line.Free;
  end;
end;

function CheckPaletteFormatAsJasc(ABuf256: string): boolean;
begin
  result := (copy(ABuf256,1,length(JascPaletteHeader)+1) = JascPaletteHeader+#$0A) or
    (copy(ABuf256,1,length(JascPaletteHeader)+2) = JascPaletteHeader+#$0D#$0A);
end;

function CheckPaletteFormatAsGimp(ABuf256: string): boolean;
begin
  result := (copy(ABuf256,1,length(GimpPaletteHeader)+1) = GimpPaletteHeader+#$0A) or
    (copy(ABuf256,1,length(GimpPaletteHeader)+2) = GimpPaletteHeader+#$0D#$0A);
end;

function CheckPaletteFormatAsKOffice(ABuf256: string): boolean;
begin
  result := (copy(ABuf256,1,length(KOfficePaletteHeader)+1) = KOfficePaletteHeader+#$0A) or
    (copy(ABuf256,1,length(KOfficePaletteHeader)+2) = KOfficePaletteHeader+#$0D#$0A);
end;

function CheckPaletteFormatAsPaintDotNet(ABuf256: string): boolean;
begin
  result := (copy(ABuf256,1,length(PaintDotNetPaletteHeader)+1) = PaintDotNetPaletteHeader+#$0A) or
    (copy(ABuf256,1,length(PaintDotNetPaletteHeader)+2) = PaintDotNetPaletteHeader+#$0D#$0A) or
    (copy(ABuf256,1,length(PaintDotNetPaletteHeaderUTF8)+1) = PaintDotNetPaletteHeaderUTF8+#$0A) or
    (copy(ABuf256,1,length(PaintDotNetPaletteHeaderUTF8)+2) = PaintDotNetPaletteHeaderUTF8+#$0D#$0A);
end;

function CheckPaletteFormatAsAdobeSwatchExchange(ABuf256: string): boolean;
begin
  result := copy(ABuf256,1,length(AdobeSwatchExchangeHeader)) = AdobeSwatchExchangeHeader;
end;


procedure RegisterDefaultPaletteFormats; forward;

procedure BGRARegisterPaletteFormat(AFormatIndex: TBGRAPaletteFormat; AExtension: string;
  ADescription: string; AReadProc: TPaletteReaderProc; AWriteProc: TPaletteWriterProc;
  ACheckFormatProc: TCheckPaletteFormatProc);
var
  i: Integer;
begin
  RegisterDefaultPaletteFormats;
  if AFormatIndex = palUnknown then
    raise Exception.Create('Invalid format index');
  for i := 0 to high(PaletteFormats) do
    if PaletteFormats[i].formatIndex = AFormatIndex then
    with PaletteFormats[i] do
    begin
      ext := AExtension;
      description := ADescription;
      reader := AReadProc;
      writer := AWriteProc;
      checkFormat := ACheckFormatProc;
      exit;
    end;
  setlength(PaletteFormats,length(PaletteFormats)+1);
  with PaletteFormats[high(PaletteFormats)] do
  begin
    formatIndex:= AFormatIndex;
    ext := AExtension;
    description := ADescription;
    reader := AReadProc;
    writer := AWriteProc;
    checkFormat := ACheckFormatProc;
  end;
end;

function BGRARegisteredPaletteFormatFilter(AAllSupportedDescription: string
  ): string;
var allExt: TStringList;
  allDesc: string;
  i: Integer;
begin
  result := '';
  RegisterDefaultPaletteFormats;
  allExt := TStringList.Create;
  allExt.CaseSensitive := false;
  for i := 0 to high(PaletteFormats) do
  with PaletteFormats[i] do
  begin
    if allExt.IndexOf(ext) = -1 then allExt.Add(ext);
    if length(result)>0 then result += '|';
    result += description + ' (*'+ext+')|*'+ext;
  end;
  if allExt.Count > 0 then
  begin
    allDesc := AAllSupportedDescription + ' (';
    for i := 0 to allExt.count-1 do
    begin
      if i > 0 then
        allDesc += '; ';
      allDesc += '*' + allExt[i];
    end;
    allDesc += ')';
    allDesc += '|';
    for i := 0 to allExt.count-1 do
    begin
      if i > 0 then
        allDesc += '; ';
      allDesc += '*' + allExt[i];
    end;
    result := allDesc + '|' + result;
  end;
  allExt.Free;
end;

var DefaultPaletteFormatsRegistered: boolean;

procedure RegisterDefaultPaletteFormats;
begin
  if DefaultPaletteFormatsRegistered then exit;
  DefaultPaletteFormatsRegistered := true;
  BGRARegisterPaletteFormat(palPaintDotNet, '.txt', 'Paint.NET',
                            @LoadFromStreamAsPaintDotNet, @SaveToStreamAsPaintDotNet,
                            @CheckPaletteFormatAsPaintDotNet);
  BGRARegisterPaletteFormat(palGimp, '.gpl', 'GIMP',
                            @LoadFromStreamAsGimp, @SaveToStreamAsGimp,
                            @CheckPaletteFormatAsGimp);
  BGRARegisterPaletteFormat(palAdobeSwatchExchange, '.ase', 'Adobe Swatch Exchange',
                            @LoadFromStreamAsAdobeSwatchExchange, @SaveToStreamAsAdobeSwatchExchange,
                            @CheckPaletteFormatAsAdobeSwatchExchange);
  BGRARegisterPaletteFormat(palKOffice, '.colors', 'KOffice',
                            @LoadFromStreamAsKOffice, @SaveToStreamAsKOffice,
                            @CheckPaletteFormatAsKOffice);
  BGRARegisterPaletteFormat(palJascPSP, '.pal', 'Jasc Paint Shop Pro',
                            @LoadFromStreamAsJasc, @SaveToStreamAsJasc,
                            @CheckPaletteFormatAsJasc);
end;

