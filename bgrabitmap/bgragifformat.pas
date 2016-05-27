unit BGRAGifFormat;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRAGraphics, BGRABitmap, BGRABitmapTypes,
  BGRAPalette;

type
  //what to do when finishing a frame and starting the next one
  TDisposeMode = (dmNone,        //undefined value
                  dmKeep,        //keep the changes done by the frame
                  dmErase,       //clear everything after the frame
                  dmRestore);    //restore to how it was before the frame

  //one image in the array
  TGifSubImage = record
    Image:    TBGRABitmap;       //image to draw at the beggining of the frame
    Position: TPoint;            //relative position of the image in the frame
    DelayMs:    integer;         //time in milliseconds to wait before going to next frame
    DisposeMode: TDisposeMode;   //what do do when going to next frame
    HasLocalPalette: boolean;    //the image has its own palette
  end;
  TGifSubImageArray = array of TGifSubImage;

  TGIFSignature = packed array[1..6] of char; //'GIF87a' or 'GIF89a'

  TGIFScreenDescriptor = packed record
    Width, Height: word;
    flags,                    //screen bit depth  = ((flags shr 4) and 7) + 1
                              //palette bit depth = (flags and 7) + 1
    BackgroundColorIndex,     //index of background color in global palette
    AspectRatio64 : byte;     //0 if not specified, otherwise aspect ratio is (AspectRatio64 + 15) / 64
  end;

  TGIFImageDescriptor = packed record
    x, y, Width, Height: word;
    flags: byte;
  end;

  TGIFImageDescriptorWithHeader = packed record
    ImageIntroducer: byte;
    Image: TGIFImageDescriptor;
  end;

  TGIFExtensionBlock = packed record
    FunctionCode: byte;
  end;

  TGIFGraphicControlExtension = packed record
    flags: byte;
    DelayHundredthSec: word;
    TransparentColorIndex: byte;
  end;

  TGIFGraphicControlExtensionWithHeader = packed record
    ExtensionIntroducer: byte;
    FunctionCode: byte;
    BlockSize: byte;
    GraphicControl: TGIFGraphicControlExtension;
    BlockTerminator: byte;
  end;

  TPackedRGBTriple = packed record
    r, g, b: byte;
  end;

  TGIFData = record
    Width, Height: integer;
    AspectRatio: single;
    BackgroundColor: TColor;
    Images: array of TGifSubImage;
  end;

  { EColorQuantizerMissing }

  EColorQuantizerMissing = class(Exception)
    constructor Create;
    constructor Create(AMessage: string);
  end;

const
  GIFScreenDescriptor_GlobalColorTableFlag = $80;    //global palette is present
  GIFScreenDescriptor_GlobalColorSortFlag  = $08;    //global palette colors are sorted by importance

  GIFImageIntroducer     = $2c;
  GIFExtensionIntroducer = $21;
  GIFBlockTerminator     = $00;

  GIFGraphicControlExtension_TransparentFlag = $01;  //transparent color index is provided
  GIFGraphicControlExtension_UserInputFlag = $02;    //wait for user input at this frame (ignored)
  GIFGraphicControlExtension_FunctionCode = $f9;
  GIFGraphicControlExtension_DisposeModeShift = 2;

  GIFImageDescriptor_LocalColorTableFlag = $80;      //local palette is present
  GIFImageDescriptor_InterlacedFlag = $40;           //image data is interlaced
  GIFImageDescriptor_LocalColorSortFlag = $20;       //local palette colors are sorted by importance

  GIFInterlacedStart: array[1..4] of longint = (0, 4, 2, 1);
  GIFInterlacedStep: array[1..4] of longint = (8, 8, 4, 2);

  GIFCodeTableSize = 4096;

function CeilLn2(AValue: Integer): integer;
function BGRAToPackedRgbTriple(color: TBGRAPixel): TPackedRGBTriple;
function PackedRgbTribleToBGRA(rgb: TPackedRGBTriple): TBGRAPixel;
function GIFLoadFromStream(stream: TStream): TGIFData;
procedure GIFSaveToStream(AData: TGifData; Stream: TStream; AQuantizerFactory: TBGRAColorQuantizerAny;
          ADitheringAlgorithm: TDitheringAlgorithm);
procedure GIFDecodeLZW(AStream: TStream; AImage: TBGRACustomBitmap;
          const APalette: ArrayOfTBGRAPixel; transcolorIndex: integer;
          interlaced: boolean);

//Encode an image supplied as an sequence of bytes, from left to right and top to bottom.
//Adapted from the work of Udo Schmal, http://www.gocher.me/FPWriteGIF
procedure GIFEncodeLZW(AStream: TStream; AImageData: PByte;
          AImageWidth, AImageHeight: integer; ABitDepth: integer);

implementation

function PackedRgbTribleToBGRA(rgb: TPackedRGBTriple): TBGRAPixel;
begin
  Result.red   := rgb.r;
  Result.green := rgb.g;
  Result.blue  := rgb.b;
  Result.alpha := 255;
end;

function BGRAToPackedRgbTriple(color: TBGRAPixel): TPackedRGBTriple;
begin
  result.r := color.red;
  result.g := color.green;
  result.b := color.blue;
end;

function CeilLn2(AValue: Integer): integer;
var comp: integer;
begin
  result := 0;
  comp := 1;
  while (comp < AValue) and (result < 30) do
  begin
    inc(result);
    comp := comp shl 1;
  end;
end;

procedure GIFDecodeLZW(AStream: TStream; AImage: TBGRACustomBitmap;
          const APalette: ArrayOfTBGRAPixel; transcolorIndex: integer;
          interlaced: boolean);
var
  xd, yd: longint;
type
  Pstr = ^Tstr;

  Tstr = record
    prefix: Pstr;
    suffix: longint;
  end;
  Pstrtab = ^Tstrtab;
  Tstrtab = array[0..GIFCodeTableSize-1] of Tstr;

var
  strtab:   Pstrtab;
  oldcode, curcode, clearcode, endcode: longint;
  codesize, codelen, codemask: longint;
  stridx:   longint;
  bitbuf, bitsinbuf: longint;
  bytbuf:   packed array[0..255] of byte;
  bytinbuf, bytbufidx: byte;
  endofsrc: boolean;
  xcnt, ycnt, ystep, pass: longint;
  pdest: PBGRAPixel;

  procedure InitStringTable;
  var
    i: longint;
  begin
    new(strtab);
    clearcode := 1 shl codesize;
    endcode   := clearcode + 1;
    stridx    := endcode + 1;
    codelen   := codesize + 1;
    codemask  := (1 shl codelen) - 1;
    for i := 0 to clearcode - 1 do
    begin
      strtab^[i].prefix := nil;
      strtab^[i].suffix := i;
    end;
    for i := clearcode to GIFCodeTableSize-1 do
    begin
      strtab^[i].prefix := nil;
      strtab^[i].suffix := 0;
    end;
  end;

  procedure ClearStringTable;
  var
    i: longint;
  begin
    clearcode := 1 shl codesize;
    endcode   := clearcode + 1;
    stridx    := endcode + 1;
    codelen   := codesize + 1;
    codemask  := (1 shl codelen) - 1;
    for i := clearcode to GIFCodeTableSize-1 do
    begin
      strtab^[i].prefix := nil;
      strtab^[i].suffix := 0;
    end;
  end;

  procedure DoneStringTable;
  begin
    dispose(strtab);
  end;

  function GetNextCode: longint;
  begin
    while (bitsinbuf < codelen) do
    begin
      if (bytinbuf = 0) then
      begin
        AStream.Read(bytinbuf, 1);
        if (bytinbuf = 0) then
          endofsrc := True;
        AStream.Read(bytbuf, bytinbuf);
        bytbufidx := 0;
      end;
      bitbuf := bitbuf or (longint(byte(bytbuf[bytbufidx])) shl bitsinbuf);
      Inc(bytbufidx);
      Dec(bytinbuf);
      Inc(bitsinbuf, 8);
    end;
    Result := bitbuf and codemask;
    bitbuf := bitbuf shr codelen;
    Dec(bitsinbuf, codelen);
  end;

  procedure AddStr2Tab(prefix: Pstr; suffix: longint);
  begin
    if stridx >= GIFCodeTableSize then exit;
    strtab^[stridx].prefix := prefix;
    strtab^[stridx].suffix := suffix;
    Inc(stridx);
    case stridx of
      0..1: codelen      := 1;
      2..3: codelen      := 2;
      4..7: codelen      := 3;
      8..15: codelen     := 4;
      16..31: codelen    := 5;
      32..63: codelen    := 6;
      64..127: codelen   := 7;
      128..255: codelen  := 8;
      256..511: codelen  := 9;
      512..1023: codelen := 10;
      1024..2047: codelen := 11;
      2048..4096: codelen := 12;
    end;
    codemask := (1 shl codelen) - 1;
  end;

  function Code2Str(code: longint): Pstr;
  begin
    Result := addr(strtab^[code]);
  end;

  procedure WriteStr(s: Pstr);
  var
    colorIndex: integer;
  begin
    if (s^.prefix <> nil) then
      WriteStr(s^.prefix);
    if (ycnt >= yd) then
    begin
      if interlaced then
      begin
        while (ycnt >= yd) and (pass < 5) do
        begin
          Inc(pass);
          ycnt  := GIFInterlacedStart[pass];
          ystep := GIFInterlacedStep[pass];
        end;
      end;
    end;

    colorIndex := s^.suffix;
    if xcnt = 0 then pdest := AImage.ScanLine[ycnt];

    if (colorIndex <> transcolorIndex) and (colorIndex >= 0) and
      (colorIndex < length(APalette)) then
      pdest^ := APalette[colorIndex];

    Inc(xcnt);
    inc(pdest);

    if (xcnt >= xd) then
    begin
      pdest := nil;
      xcnt := 0;
      Inc(ycnt, ystep);

      if not interlaced then
        if (ycnt >= yd) then
        begin
          Inc(pass);
        end;

    end;
  end;

  function firstchar(s: Pstr): byte;
  begin
    while (s^.prefix <> nil) do
      s    := s^.prefix;
    Result := s^.suffix;
  end;

begin
  endofsrc := False;
  xd   := AImage.Width;
  yd   := AImage.Height;
  xcnt := 0;
  pdest := nil;
  if interlaced then
  begin
    pass  := 1;
    ycnt  := GIFInterlacedStart[pass];
    ystep := GIFInterlacedStep[pass];
  end
  else
  begin
    pass  := 4;
    ycnt  := 0;
    ystep := 1;
  end;
  oldcode   := 0;
  bitbuf    := 0;
  bitsinbuf := 0;
  bytinbuf  := 0;
  bytbufidx := 0;
  codesize  := 0;
  AStream.Read(codesize, 1);
  InitStringTable;
  curcode := getnextcode;
  while (curcode <> endcode) and (pass < 5) and not endofsrc do
  begin
    if (curcode = clearcode) then
    begin
      ClearStringTable;
      repeat
        curcode := getnextcode;
      until (curcode <> clearcode);
      if (curcode = endcode) then
        break;
      WriteStr(code2str(curcode));
      oldcode := curcode;
    end
    else
    begin
      if (curcode < stridx) then
      begin
        WriteStr(Code2Str(curcode));
        AddStr2Tab(Code2Str(oldcode), firstchar(Code2Str(curcode)));
        oldcode := curcode;
      end
      else
      begin
        if (curcode > stridx) then
          break;
        AddStr2Tab(Code2Str(oldcode), firstchar(Code2Str(oldcode)));
        WriteStr(Code2Str(stridx - 1));
        oldcode := curcode;
      end;
    end;
    curcode := getnextcode;
  end;
  DoneStringTable;
  if not endofsrc then
  begin
    bytinbuf:= 0;
    AStream.Read(bytinbuf, 1);
    if bytinbuf <> 0 then
      raise exception.Create('Invalid GIF format: expecting block terminator');
  end;
end;

//Encode an image supplied as an sequence of bytes, from left to right and top to bottom.
//Adapted from the work of Udo Schmal, http://www.gocher.me/FPWriteGIF
procedure GIFEncodeLZW(AStream: TStream; AImageData: PByte;
          AImageWidth, AImageHeight: integer; ABitDepth: integer);
var
   LZWSize: byte;
   OutputBufferSize: NativeInt;
   OutputBuffer: packed array[0..255] of byte;

   rPrefix: array[0..GIFCodeTableSize-1] of integer; // string prefixes
   rSuffix: array[0..GIFCodeTableSize-1] of integer; // string suffixes
   rCodeStack: array[0..GIFCodeTableSize-1] of byte; // encoded pixels
   rSP: integer; // pointer into CodeStack
   rClearCode: integer; // reset decode params
   rEndCode: integer; // last code in input stream
   rCurSize: integer; // current code size
   rBitString: integer; // steady stream of bits to be decoded
   rBits: integer; // number of valid bits in BitString
   rMaxVal: boolean; // max code value found?
   rCurX: integer; // position of next pixel
   rCurY: integer; // position of next pixel
   rCurScan: PByte;
   rFirstSlot: integer; // for encoding an image
   rNextSlot: integer; // for encoding
   rRowsLeft: integer; // rows left to do
   rLast: integer; // last byte read in
   rUnget: boolean; // read a new byte, or use zLast?

   procedure FlushOutput;
   begin
     if OutputBufferSize > 0 then
     begin
       OutputBuffer[0] := OutputBufferSize;
       AStream.WriteBuffer(OutputBuffer, OutputBufferSize+1);
       OutputBufferSize := 0;
     end;
   end;

   procedure OutputByte(AValue: byte);
   begin
     if OutputBufferSize = 255 then FlushOutput;
     inc(OutputBufferSize);
     OutputBuffer[OutputBufferSize] := AValue;
   end;

   procedure LZWReset;
   var i: integer;
   begin
     for i := 0 to (GIFCodeTableSize - 1) do
     begin
       rPrefix[i] := 0;
       rSuffix[i] := 0;
     end;
     rCurSize := LZWSize + 1;
     rClearCode := (1 shl LZWSize);
     rEndCode := rClearCode + 1;
     rFirstSlot := (1 shl (rCurSize - 1)) + 2;
     rNextSlot := rFirstSlot;
     rMaxVal := false;
   end;

   // save a code value on the code stack
   procedure LZWSaveCode(Code: integer);
   begin
     rCodeStack[rSP] := Code;
     inc(rSP);
   end;

   // save the code in the output data stream
   procedure LZWPutCode(code: integer);
   var
     n: integer;
     b: byte;
   begin
     // write out finished bytes
     // a literal "8" for 8 bits per byte
     while (rBits >= 8) do
     begin
       b := (rBitString and $ff);
       rBitString := (rBitString shr 8);
       rBits := rBits - 8;
       OutputByte(b);
     end;
     // make sure no junk bits left above the first byte
     rBitString := (rBitString and $ff);
     // and save out-going code
     n := (code shl rBits);
     rBitString := (rBitString or n);
     rBits := rBits + rCurSize;
   end;

   // get the next pixel from the bitmap, and return it as an index into the colormap
   function LZWReadBitmap: integer;
   begin
     if rUnget then
     begin
       result := rLast;
       rUnget := false;
     end
     else
     begin
       if rCurScan = nil then
         rCurScan := AImageData + rCurY*AImageWidth;
       result := (rCurScan+rCurX)^;
       inc(rCurX); // inc X position
       if (rCurX >= AImageWidth) then // bumping Y ?
       begin
         rCurX := 0;
         inc(rCurY);
         rCurScan := nil;
         dec(rRowsLeft);
       end;
     end;
     rLast := result;
   end;

var
   i,n,
   cc: integer; // current code to translate
   oc: integer; // last code encoded
   found: boolean; // decoded string in prefix table?
   pixel: byte; // lowest code to search for
   ldx: integer; // last index found
   fdx: integer; // current index found
   b: byte;
begin
   LZWSize := ABitDepth;
   AStream.WriteBuffer(LZWSize, 1);
   OutputBufferSize := 0;

   // init data block
   fillchar(rCodeStack, sizeof(rCodeStack), 0);
   rBitString := 0;
   rBits := 0;
   rCurX := 0;
   rCurY := 0;
   rCurScan := nil;
   rLast := 0;
   rUnget:= false;

   LZWReset;
   // all within the data record
   // always save the clear code first ...
   LZWPutCode(rClearCode);
   // and first pixel
   oc := LZWReadBitmap;
   LZWPutCode(oc);
   // nothing found yet (but then, we haven't searched)
   ldx := 0;
   fdx := 0;
   // and the rest of the pixels
   rRowsLeft := AImageHeight;
   while (rRowsLeft > 0) do
   begin
     rSP := 0; // empty the stack of old data
     n := LZWReadBitmap; // next pixel from the bitmap
     LZWSaveCode(n);
     cc := rCodeStack[0]; // beginning of the string
     // add new encode table entry
     rPrefix[rNextSlot] := oc;
     rSuffix[rNextSlot] := cc;
     inc(rNextSlot);
     if (rNextSlot >= GIFCodeTableSize) then
       rMaxVal := true
     else if (rNextSlot > (1 shl rCurSize)) then
       inc(rCurSize);
     // find the running string of matching codes
     ldx := cc;
     found := true;
     while (found and (rRowsLeft > 0)) do
     begin
       n := LZWReadBitmap;
       LZWSaveCode(n);
       cc := rCodeStack[0];
       if (ldx < rFirstSlot) then
         i := rFirstSlot
       else
         i := ldx + 1;
       pixel := rCodeStack[rSP - 1];
       found := false;
       while ((not found) and (i < rNextSlot)) do
       begin
         found := ((rPrefix[i] = ldx) and (rSuffix[i] = pixel));
         inc(i);
       end;
       if (found) then
       begin
         ldx := i - 1;
         fdx := i - 1;
       end;
     end;
     // if not found, save this index, and get the same code again
     if (not found) then
     begin
       rUnget := true;
       rLast := rCodeStack[rSP-1];
       dec(rSP);
       cc := ldx;
     end
     else
       cc := fdx;
     // whatever we got, write it out as current table entry
     LZWPutCode(cc);
     if (rMaxVal and (rRowsLeft > 0)) then
     begin
       LZWPutCode(rClearCode);
       LZWReset;
       cc := LZWReadBitmap;
       LZWPutCode(cc);
     end;
     oc := cc;
   end;
   LZWPutCode(rEndCode);
   // write out the rest of the bit string
   while (rBits > 0) do
   begin
     b := (rBitString and $ff);
     rBitString := (rBitString shr 8);
     rBits := rBits - 8;
     OutputByte(b);
   end;
   FlushOutput;
   b := 0;
   AStream.Write(b, 1);
end;

function GIFLoadFromStream(stream: TStream): TGIFData;

  procedure DumpData;
  var
    Count: byte;
  begin
    repeat
      Count := 0;
      stream.Read(Count, 1);
      stream.position := stream.position + Count;
    until (Count = 0) or (stream.position >= stream.size);
  end;

var
  NbImages:  integer;

  GIFSignature: TGIFSignature;
  GIFScreenDescriptor: TGIFScreenDescriptor;
  GIFBlockID:   char;
  GIFImageDescriptor: TGIFImageDescriptor;

  globalPalette: ArrayOfTBGRAPixel;
  localPalette:  ArrayOfTBGRAPixel;

  transcolorIndex: integer;
  DelayMs: integer;
  disposeMode: TDisposeMode;

  procedure LoadGlobalPalette;
  var
    NbEntries, i: integer;
    rgb: TPackedRGBTriple;
  begin
    NbEntries := 1 shl (GIFScreenDescriptor.flags and $07 + 1);
    setlength(globalPalette, NbEntries);
    for i := 0 to NbEntries - 1 do
    begin
      stream.ReadBuffer({%H-}rgb, 3);
      globalPalette[i] := PackedRgbTribleToBGRA(rgb);
    end;
  end;

  procedure LoadLocalPalette;
  var
    NbEntries, i: integer;
    rgb: TPackedRGBTriple;
  begin
    NbEntries := 1 shl (GIFImageDescriptor.flags and $07 + 1);
    setlength(localPalette, NbEntries);
    for i := 0 to NbEntries - 1 do
    begin
      stream.ReadBuffer({%H-}rgb, 3);
      localPalette[i] := PackedRgbTribleToBGRA(rgb);
    end;
  end;

  procedure LoadImage;
  var
    imgWidth, imgHeight: integer;
    img:     TBGRABitmap;
    Interlaced: boolean;
    palette: ArrayOfTBGRAPixel;
  begin
    stream.Read(GIFImageDescriptor, sizeof(GIFImageDescriptor));
    GIFImageDescriptor.Width := LEtoN(GIFImageDescriptor.Width);
    GIFImageDescriptor.Height := LEtoN(GIFImageDescriptor.Height);
    GIFImageDescriptor.x := LEtoN(GIFImageDescriptor.x);
    GIFImageDescriptor.y := LEtoN(GIFImageDescriptor.y);
    if (GIFImageDescriptor.flags and GIFImageDescriptor_LocalColorTableFlag =
      GIFImageDescriptor_LocalColorTableFlag) then
      LoadLocalPalette
    else
      localPalette := nil;

    if localPalette <> nil then
      palette := localPalette
    else
      palette := globalPalette;
    imgWidth := GIFImageDescriptor.Width;
    imgHeight := GIFImageDescriptor.Height;

    if length(result.Images) <= NbImages then
      setlength(result.Images, length(result.Images) * 2 + 1);
    img := TBGRABitmap.Create(imgWidth, imgHeight);
    img.Fill(BGRAPixelTransparent);
    result.Images[NbImages].Image    := img;
    result.Images[NbImages].Position := point(GIFImageDescriptor.x, GIFImageDescriptor.y);
    result.Images[NbImages].DelayMs    := DelayMs;
    result.Images[NbImages].DisposeMode := disposeMode;
    result.Images[NbImages].HasLocalPalette := localPalette <> nil;
    Inc(NbImages);

    Interlaced := GIFImageDescriptor.flags and GIFImageDescriptor_InterlacedFlag =
      GIFImageDescriptor_InterlacedFlag;
    GIFDecodeLZW(stream, img, palette, transcolorIndex, Interlaced);
  end;

  procedure ReadExtension;
  var
    GIFExtensionBlock: TGIFExtensionBlock;
    GIFGraphicControlExtension: TGIFGraphicControlExtension;
    mincount, Count:   byte;

  begin
    stream.ReadBuffer({%H-}GIFExtensionBlock, sizeof(GIFExtensionBlock));
    case GIFExtensionBlock.FunctionCode of
      $F9:
      begin
        Count := 0;
        stream.Read(Count, 1);
        if Count < sizeof(GIFGraphicControlExtension) then
          mincount := 0
        else
        begin
          mincount := sizeof(GIFGraphicControlExtension);
          stream.ReadBuffer({%H-}GIFGraphicControlExtension, mincount);
          GIFGraphicControlExtension.DelayHundredthSec := LEtoN(GIFGraphicControlExtension.DelayHundredthSec);

          if GIFGraphicControlExtension.flags and
            GIFGraphicControlExtension_TransparentFlag =
            GIFGraphicControlExtension_TransparentFlag then
            transcolorIndex := GIFGraphicControlExtension.TransparentColorIndex
          else
            transcolorIndex := -1;
          if GIFGraphicControlExtension.DelayHundredthSec <> 0 then
            DelayMs     := GIFGraphicControlExtension.DelayHundredthSec * 10;
          DisposeMode := TDisposeMode((GIFGraphicControlExtension.flags shr GIFGraphicControlExtension_DisposeModeShift) and 7);
        end;
        stream.Position := Stream.Position + Count - mincount;
        DumpData;
      end;
      else
      begin
        DumpData;
      end;
    end;
  end;

begin
  result.Width := 0;
  result.Height := 0;
  result.BackgroundColor := clNone;
  result.Images := nil;
  result.AspectRatio := 1;
  if stream = nil then exit;

  NbImages  := 0;
  transcolorIndex := -1;
  DelayMs     := 100;
  disposeMode := dmErase;

  FillChar({%H-}GIFSignature,sizeof(GIFSignature),0);
  stream.Read(GIFSignature, sizeof(GIFSignature));
  if (GIFSignature[1] = 'G') and (GIFSignature[2] = 'I') and (GIFSignature[3] = 'F') then
  begin
    stream.ReadBuffer({%H-}GIFScreenDescriptor, sizeof(GIFScreenDescriptor));
    GIFScreenDescriptor.Width := LEtoN(GIFScreenDescriptor.Width);
    GIFScreenDescriptor.Height := LEtoN(GIFScreenDescriptor.Height);
    result.Width  := GIFScreenDescriptor.Width;
    result.Height := GIFScreenDescriptor.Height;
    if GIFScreenDescriptor.AspectRatio64 = 0 then
      result.AspectRatio:= 1
    else
      result.AspectRatio:= (GIFScreenDescriptor.AspectRatio64+15)/64;
    if (GIFScreenDescriptor.flags and GIFScreenDescriptor_GlobalColorTableFlag =
      GIFScreenDescriptor_GlobalColorTableFlag) then
    begin
      LoadGlobalPalette;
      if GIFScreenDescriptor.BackgroundColorIndex < length(globalPalette) then
        result.BackgroundColor :=
          BGRAToColor(globalPalette[GIFScreenDescriptor.BackgroundColorIndex]);
    end;
    repeat
      stream.ReadBuffer({%H-}GIFBlockID, sizeof(GIFBlockID));
      case GIFBlockID of
        ';': ;
        ',': LoadImage;
        '!': ReadExtension;
        else
        begin
          raise Exception.Create('TBGRAAnimatedGif: unexpected block type');
          break;
        end;
      end;
    until (GIFBlockID = ';') or (stream.Position >= stream.size);
  end
  else
    raise Exception.Create('TBGRAAnimatedGif: invalid header');
  setlength(result.Images, NbImages);
end;

procedure GIFSaveToStream(AData: TGifData; Stream: TStream; AQuantizerFactory: TBGRAColorQuantizerAny;
          ADitheringAlgorithm: TDitheringAlgorithm);
var
  signature: TGIFSignature;
  screenDescriptor: TGIFScreenDescriptor;
  globalPalette: TBGRAPalette;
  globalQuantizer: TBGRACustomColorQuantizer;
  globalTranspIndex: integer;

  procedure AddColorsToPalette(AImage: TBGRACustomBitmap; APalette: TBGRAPalette);
  var n: integer;
    p: PBGRAPixel;
    c: TBGRAPixel;
  begin
    p := AImage.Data;
    for n := AImage.NbPixels-1 downto 0 do
    begin
      if p^.alpha < 255 then //transparent color will be needed to dither properly
        APalette.AddColor(BGRAPixelTransparent);
      if p^.alpha > 0 then //color may be needed to dither properly
      begin
        c := p^;
        c.alpha := 255;
        APalette.AddColor(c);
      end;
      inc(p);
    end;
  end;

  function ImageCount: integer;
  begin
    result := length(AData.Images);
  end;

  function NeedGlobalPalette: boolean;
  var i: integer;
  begin
    for i := 0 to ImageCount-1 do
      if not AData.Images[i].HasLocalPalette then
      begin
        result := true;
        exit;
      end;
  end;

  function IndexOfGlobalColor(AColor: TBGRAPixel): integer;
  begin
    if Assigned(globalQuantizer) then
      result := globalQuantizer.ReducedPalette.FindNearestColorIndex(AColor)
    else
      result := globalPalette.IndexOfColor(AColor);
  end;

  procedure MakeGlobalPalette;
  var i: integer;
    indexed: TBGRAIndexedPalette;
    bitDepth: integer;
  begin
    globalPalette := TBGRAPalette.Create;
    for i := 0 to ImageCount-1 do
      if not AData.Images[i].HasLocalPalette then
        AddColorsToPalette(AData.Images[i].Image, globalPalette);
    if AData.BackgroundColor <> clNone then
      globalPalette.AddColor(ColorToBGRA(ColorToRGB(AData.BackgroundColor)));

    if globalPalette.Count > 256 then
    begin
      if Assigned(AQuantizerFactory) then
      begin
        globalQuantizer:= AQuantizerFactory.Create(globalPalette, False, 256);
        globalPalette.Free;
        globalPalette := TBGRAIndexedPalette.Create(globalQuantizer.ReducedPalette);
      end
      else
      begin
        globalPalette.Free;
        raise EColorQuantizerMissing.Create;
      end;
    end else
    begin
      indexed := TBGRAIndexedPalette.Create(globalPalette);
      globalPalette.Free;
      globalPalette := indexed;
    end;

    globalTranspIndex:= globalPalette.IndexOfColor(BGRAPixelTransparent);
    if AData.BackgroundColor <> clNone then
      screenDescriptor.BackgroundColorIndex:= IndexOfGlobalColor(ColorToBGRA(ColorToRGB(AData.BackgroundColor))) and 255;

    bitDepth := CeilLn2(globalPalette.Count);
    if bitDepth > 8 then bitDepth:= 8;
    if bitDepth < 1 then bitDepth:= 1;
    screenDescriptor.flags := screenDescriptor.flags or GIFScreenDescriptor_GlobalColorTableFlag;
    screenDescriptor.flags := screenDescriptor.flags or (bitDepth-1);
  end;

  procedure WritePalette(pal: TBGRAPalette; bitDepth: integer);
  var i: integer;
    numberToWrite,numberFromPal: Integer;
    rgbs: ^TPackedRGBTriple;
    black: TPackedRGBTriple;
  begin
    if not Assigned(pal) then exit;
    numberToWrite:= 1 shl bitDepth;
    numberFromPal := pal.Count;
    if numberFromPal > numberToWrite then numberFromPal:= numberToWrite;
    getmem(rgbs, numberToWrite*sizeof(TPackedRGBTriple));
    try
      for i := 0 to numberFromPal-1 do
        rgbs[i] := BGRAToPackedRgbTriple(pal.Color[i]);
      black := BGRAToPackedRgbTriple(ColorToBGRA(clBlack));
      for i := numberFromPal to numberToWrite-1 do
        rgbs[i] := black;
      Stream.WriteBuffer(rgbs^,sizeof(TPackedRGBTriple)*numberToWrite);
    finally
      freemem(rgbs);
    end;
  end;

  procedure WriteGlobalPalette;
  begin
    WritePalette(globalPalette, (screenDescriptor.flags and 7)+1);
  end;

  procedure FreeGlobalPalette;
  begin
    FreeAndNil(globalPalette);
    FreeAndNil(globalQuantizer);
  end;

  procedure WriteImages;
  var
    localPalette: TBGRAPalette;
    localQuantizer: TBGRACustomColorQuantizer;
    localTranspIndex: integer;
    imageDescriptor: TGIFImageDescriptorWithHeader;

    procedure MakeLocalPalette(AFrameIndex: integer);
    var
      indexed: TBGRAIndexedPalette;
      bitDepth: integer;
    begin
      localPalette := TBGRAPalette.Create;
      AddColorsToPalette(AData.Images[AFrameIndex].Image, localPalette);
      if localPalette.Count > 256 then
      begin
        if Assigned(AQuantizerFactory) then
        begin
          localQuantizer:= AQuantizerFactory.Create(localPalette, False, 256);
          localPalette.Free;
          localPalette := TBGRAIndexedPalette.Create(localQuantizer.ReducedPalette);
        end
        else
        begin
          localPalette.Free;
          raise EColorQuantizerMissing.Create;
        end;
      end else
      begin
        indexed := TBGRAIndexedPalette.Create(localPalette);
        localPalette.Free;
        localPalette := indexed;
      end;

      localTranspIndex:= localPalette.IndexOfColor(BGRAPixelTransparent);

      bitDepth := CeilLn2(localPalette.Count);
      if bitDepth > 8 then bitDepth:= 8;
      if bitDepth < 1 then bitDepth:= 1;
      imageDescriptor.Image.flags := imageDescriptor.Image.flags or GIFImageDescriptor_LocalColorTableFlag;
      imageDescriptor.Image.flags := imageDescriptor.Image.flags or (bitDepth-1);
    end;

    procedure WriteLocalPalette;
    begin
      WritePalette(localPalette, (imageDescriptor.Image.flags and 7)+1);
    end;

    procedure FreeLocalPalette;
    begin
      FreeAndNil(localPalette);
      FreeAndNil(localQuantizer);
      localTranspIndex:= -1;
    end;

    procedure DitherAndCompressImage(AFrame: integer; APalette: TBGRAPalette; AQuantizer: TBGRACustomColorQuantizer);
    var ImageData: Pointer;
      Image: TBGRABitmap;
      y,x: NativeInt;
      psource: PBGRAPixel;
      pdest: PByte;
    begin
      Image := AData.Images[AFrame].Image;
      if Assigned(AQuantizer) then
        ImageData := AQuantizer.GetDitheredBitmapIndexedData(8, ADitheringAlgorithm, Image)
      else
      begin
        GetMem(ImageData, Image.Width*Image.Height);
        pdest := ImageData;
        for y := 0 to Image.Height -1 do
        begin
          psource := Image.ScanLine[y];
          for x := 0 to Image.Width -1 do
          begin
            pdest^ := APalette.IndexOfColor(psource^);
            inc(psource);
            inc(pdest);
          end;
        end;
      end;
      try
        GIFEncodeLZW(Stream, ImageData, Image.Width, Image.Height, CeilLn2(APalette.Count));
      finally
        FreeMem(ImageData);
      end;
    end;

    procedure WriteImage(AFrame: integer);
    var
      ext: TGIFGraphicControlExtensionWithHeader;
      transpIndex: integer;
    begin
      fillchar({%H-}ext, sizeof(ext), 0);
      try
        ext.ExtensionIntroducer := GIFExtensionIntroducer;
        ext.FunctionCode := GIFGraphicControlExtension_FunctionCode;
        ext.BlockSize := sizeof(ext.GraphicControl);
        ext.GraphicControl.DelayHundredthSec := (AData.Images[AFrame].DelayMs+5) div 10;
        ext.GraphicControl.TransparentColorIndex := 0;
        ext.GraphicControl.flags := integer(AData.Images[AFrame].DisposeMode) shl GIFGraphicControlExtension_DisposeModeShift;
        ext.BlockTerminator := GIFBlockTerminator;
        with AData.Images[AFrame].Position do
        begin
          imageDescriptor.Image.x := x;
          imageDescriptor.Image.y := y;
        end;
        with AData.Images[AFrame].Image do
        begin
          imageDescriptor.Image.Width := Width;
          imageDescriptor.Image.Height := Height;
        end;
        imageDescriptor.Image.flags := 0;

        if AData.Images[AFrame].HasLocalPalette then MakeLocalPalette(AFrame);

        if AData.Images[AFrame].Image.HasTransparentPixels then
        begin
          if AData.Images[AFrame].HasLocalPalette then
            transpIndex := localTranspIndex
          else
            transpIndex := globalTranspIndex;
        end else
          transpIndex := -1;
        if (transpIndex >= 0) and (transpIndex <= 255) then
        begin
          ext.GraphicControl.flags := ext.GraphicControl.flags or GIFGraphicControlExtension_TransparentFlag;
          ext.GraphicControl.TransparentColorIndex := transpIndex;
        end;

        Stream.WriteBuffer(ext, sizeof(ext));
        Stream.WriteBuffer(imageDescriptor, sizeof(imageDescriptor));
        WriteLocalPalette;

        if AData.Images[AFrame].HasLocalPalette then
          DitherAndCompressImage(AFrame, localPalette, localQuantizer)
        else
          DitherAndCompressImage(AFrame, globalPalette, globalQuantizer);
      finally
        FreeLocalPalette;
      end;
    end;

  var
    i: integer;
  begin
    localPalette := nil;
    localQuantizer := nil;
    localTranspIndex:= -1;
    fillchar({%H-}imageDescriptor, sizeof(imageDescriptor), 0);
    imageDescriptor.ImageIntroducer := GIFImageIntroducer;
    for i := 0 to ImageCount-1 do
      WriteImage(i);
  end;

begin
  globalPalette := nil;
  globalQuantizer := nil;
  globalTranspIndex:= -1;
  try
    signature := 'GIF89a';
    screenDescriptor.Width := NtoLE(AData.Width);
    screenDescriptor.Height := NtoLE(AData.Height);
    screenDescriptor.flags := $70;               //suppose 8-bit screen
    screenDescriptor.BackgroundColorIndex := 0;  //not specified for now
    screenDescriptor.AspectRatio64 := round(AData.AspectRatio*64)-15;
    if NeedGlobalPalette then MakeGlobalPalette;

    Stream.WriteBuffer(signature, sizeof(signature));
    Stream.WriteBuffer(screenDescriptor, sizeof(screenDescriptor));
    WriteGlobalPalette;

    WriteImages;
    Stream.WriteByte($3B); //end of file

  finally
    FreeGlobalPalette;
  end;
end;

{ EColorQuantizerMissing }

constructor EColorQuantizerMissing.Create;
begin
  inherited Create('Please provide a color quantizer class (one is provided in BGRAColorQuantization)')
end;

constructor EColorQuantizerMissing.Create(AMessage: string);
begin
  inherited Create(AMessage);
end;

end.

