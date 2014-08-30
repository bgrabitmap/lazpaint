unit BGRAAnimatedGif;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, FPImage, BGRABitmap, BGRABitmapTypes;

type
  TDisposeMode = (dmNone, dmKeep, dmErase, dmRestore);

  TGifSubImage = record
    Image:    TBGRABitmap;
    Position: TPoint;
    Delay:    integer;
    disposeMode: TDisposeMode;
    TransparentColor: TBGRAPixel;
  end;
  TGifSubImageArray = array of TGifSubImage;

  TGifBackgroundMode = (gbmSimplePaint, gbmEraseBackground,
    gbmSaveBackgroundOnce, gbmUpdateBackgroundContinuously);

  { TBGRAAnimatedGif }

  TBGRAAnimatedGif = class(TGraphic)
  private
    FWidth, FHeight:  integer;
    FBackgroundColor: TColor;

    FPrevDate: TDateTime;
    FPaused:   boolean;
    FTimeAccumulator: double;
    FCurrentImage, FWantedImage: integer;
    FFullAnimationTime: double;
    FPreviousDisposeMode: TDisposeMode;

    FBackgroundImage, FPreviousVirtualScreen, FStretchedVirtualScreen,
    FInternalVirtualScreen, FRestoreImage: TBGRABitmap;
    FImageChanged: boolean;

    function GetCount: integer;
    function GetTimeUntilNextImage: integer;
    procedure Render(StretchWidth, StretchHeight: integer);
    procedure UpdateSimple(Canvas: TCanvas; ARect: TRect;
      DrawOnlyIfChanged: boolean = True);
    procedure UpdateEraseBackground(Canvas: TCanvas; ARect: TRect;
      DrawOnlyIfChanged: boolean = True);
    procedure Init;
    function GetBitmap: TBitmap;
    function GetMemBitmap: TBGRABitmap;
    procedure SaveBackgroundOnce(Canvas: TCanvas; ARect: TRect);
    procedure SetCurrentImage(Index: integer);

  protected
    FImages: TGifSubImageArray;
    procedure LoadImages(stream: TStream);

    {TGraphic}
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    function GetEmpty: boolean; override;
    function GetHeight: integer; override;
    function GetTransparent: boolean; override;
    function GetWidth: integer; override;
    procedure SetHeight(Value: integer); override;
    procedure SetTransparent(Value: boolean); override;
    procedure SetWidth(Value: integer); override;

  public
    EraseColor:     TColor;
    BackgroundMode: TGifBackgroundMode;

    constructor Create(filenameUTF8: string);
    constructor Create(stream: TStream);
    constructor Create; override;
    function Duplicate: TBGRAAnimatedGif;

    {TGraphic}
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromFile(const AFilenameUTF8: string); override;
    procedure SaveToFile(const AFilenameUTF8: string); override;
    class function GetFileExtensions: string; override;

    procedure Clear; override;
    destructor Destroy; override;
    procedure Pause;
    procedure Resume;

    procedure Show(Canvas: TCanvas; ARect: TRect); overload;
    procedure Update(Canvas: TCanvas; ARect: TRect); overload;
    procedure Hide(Canvas: TCanvas; ARect: TRect); overload;

    property BackgroundColor: TColor Read FBackgroundColor;
    property Count: integer Read GetCount;
    property Width: integer Read FWidth;
    property Height: integer Read FHeight;
    property Paused: boolean Read FPaused;
    property Bitmap: TBitmap Read GetBitmap;
    property MemBitmap: TBGRABitmap Read GetMemBitmap;
    property CurrentImage: integer Read FCurrentImage Write SetCurrentImage;
    property TimeUntilNextImageMs: integer read GetTimeUntilNextImage;
  end;

  { TFPReaderGIF }

  TFPReaderGIF = class(TFPCustomImageReader)
  protected
    procedure InternalRead(Str: TStream; Img: TFPCustomImage); override;
    function InternalCheck(Str: TStream): boolean; override;
  end;

const
  GifBackgroundModeStr: array[TGifBackgroundMode] of string =
    ('gbmSimplePaint', 'gbmEraseBackground', 'gbmSaveBackgroundOnce',
    'gbmUpdateBackgroundContinuously');

implementation

uses BGRABlend, lazutf8classes;

const
  AlphaMask = $FF000000;

type
  TGIFSignature = packed array[1..6] of char;

  TGIFScreenDescriptor = packed record
    Width, Height: word;
    flags, background, map: byte;
  end;

  TGIFImageDescriptor = packed record
    x, y, Width, Height: word;
    flags: byte;
  end;

  TGIFExtensionBlock = packed record
    functioncode: byte;
  end;

  TGIFGraphicControlExtension = packed record
    flags:      byte;
    delaytime:  word;
    transcolor: byte;
  end;

{ TBGRAAnimatedGif }

class function TBGRAAnimatedGif.GetFileExtensions: string;
begin
  Result := 'gif';
end;

procedure TBGRAAnimatedGif.Render(StretchWidth, StretchHeight: integer);
var
  curDate: TDateTime;
  previousImage, nextImage: integer;

begin
  if FInternalVirtualScreen = nil then
  begin
    FInternalVirtualScreen := TBGRABitmap.Create(FWidth, FHeight);
    if Count = 0 then
      FInternalVirtualScreen.Fill(BackgroundColor)
    else
      FInternalVirtualScreen.Fill(BGRAPixelTransparent);
    FImageChanged := True;
  end;

  if Count = 0 then
    exit;

  previousImage := FCurrentImage;

  curDate := Now;
  if FWantedImage <> -1 then
  begin
    nextImage    := FWantedImage;
    FTimeAccumulator := 0;
    FWantedImage := -1;
  end
  else
  if FCurrentImage = -1 then
  begin
    nextImage := 0;
    FTimeAccumulator := 0;
    FPreviousDisposeMode := dmNone;
  end
  else
  begin
    if not FPaused then
      FTimeAccumulator += (curDate - FPrevDate) * 24 * 60 * 60 * 1000;
    if FFullAnimationTime > 0 then FTimeAccumulator:= frac(FTimeAccumulator/FFullAnimationTime)*FFullAnimationTime;
    nextImage := FCurrentImage;
    while FTimeAccumulator > FImages[nextImage].Delay do
    begin
      FTimeAccumulator -= FImages[nextImage].Delay;
      Inc(nextImage);
      if nextImage >= Count then
        nextImage := 0;

      if nextImage = previousImage then
      begin
        Inc(nextImage);
        if nextImage >= Count then
          nextImage := 0;
        break;
      end;
    end;
  end;
  FPrevDate := curDate;

  while FCurrentImage <> nextImage do
  begin
    Inc(FCurrentImage);
    if FCurrentImage >= Count then
    begin
      FCurrentImage := 0;
      FPreviousDisposeMode := dmErase;
    end;

    case FPreviousDisposeMode of
      dmErase: FInternalVirtualScreen.Fill(BGRAPixelTransparent);
      dmRestore: if FRestoreImage <> nil then
          FInternalVirtualScreen.PutImage(0, 0, FRestoreImage, dmSet);
    end;

    with FImages[FCurrentImage] do
    begin
      if disposeMode = dmRestore then
      begin
        if FRestoreImage = nil then
          FRestoreImage := TBGRABitmap.Create(FWidth, FHeight);
        FRestoreImage.PutImage(0, 0, FInternalVirtualScreen, dmSet);
      end;

      if Image <> nil then
        FInternalVirtualScreen.PutImage(Position.X, Position.Y, Image,
          dmSetExceptTransparent);
      FPreviousDisposeMode := disposeMode;
    end;

    FImageChanged := True;
    previousImage := FCurrentImage;
    FInternalVirtualScreen.InvalidateBitmap;
  end;

  if FStretchedVirtualScreen <> nil then
    FStretchedVirtualScreen.FreeReference;
  if (FInternalVirtualScreen.Width = StretchWidth) and
    (FInternalVirtualScreen.Height = StretchHeight) then
    FStretchedVirtualScreen := TBGRABitmap(FInternalVirtualScreen.NewReference)
  else
    FStretchedVirtualScreen :=
      TBGRABitmap(FInternalVirtualScreen.Resample(StretchWidth, StretchHeight));
end;

procedure TBGRAAnimatedGif.UpdateSimple(Canvas: TCanvas; ARect: TRect;
  DrawOnlyIfChanged: boolean = True);
begin
  if FPreviousVirtualScreen <> nil then
  begin
    FPreviousVirtualScreen.FreeReference;
    FPreviousVirtualScreen := nil;
  end;

  Render(ARect.Right - ARect.Left, ARect.Bottom - ARect.Top);
  if FImageChanged then
  begin
    FStretchedVirtualScreen.Draw(Canvas, ARect.Left, ARect.Top, False);
    FImageChanged := False;
  end
  else
  if not DrawOnlyIfChanged then
    FStretchedVirtualScreen.Draw(Canvas, ARect.Left, ARect.Top, False);

  FPreviousVirtualScreen := TBGRABitmap(FStretchedVirtualScreen.NewReference);
end;

function TBGRAAnimatedGif.GetCount: integer;
begin
  Result := length(FImages);
end;

function TBGRAAnimatedGif.GetTimeUntilNextImage: integer;
var
  acc: double;
begin
  if Count <= 1 then result := 60*1000 else
  if (FWantedImage <> -1) or (FCurrentImage = -1) then
    result := 0
  else
  begin
    acc := FTimeAccumulator;
    if not FPaused then acc += (Now- FPrevDate) * 24 * 60 * 60 * 1000;
    if acc >= FImages[FCurrentImage].Delay then
      result := 0
    else
      result := round(FImages[FCurrentImage].Delay-FTimeAccumulator);
  end;
end;

constructor TBGRAAnimatedGif.Create(filenameUTF8: string);
var
  Stream: TFileStreamUTF8;
begin
  inherited Create;
  Init;
  Stream := TFileStreamUTF8.Create(filenameUTF8, fmOpenRead or fmShareDenyWrite);
  LoadFromStream(Stream);
  Stream.Free;
end;

constructor TBGRAAnimatedGif.Create(stream: TStream);
begin
  inherited Create;
  Init;
  LoadFromStream(stream);
end;

constructor TBGRAAnimatedGif.Create;
begin
  inherited Create;
  Init;
  LoadFromStream(nil);
end;

function TBGRAAnimatedGif.Duplicate: TBGRAAnimatedGif;
var
  i: integer;
begin
  Result := TBGRAAnimatedGif.Create;
  setlength(Result.FImages, length(FImages));
  for i := 0 to high(FImages) do
  begin
    Result.FImages[i] := FImages[i];
    FImages[i].Image.NewReference;
  end;
  Result.FWidth  := FWidth;
  Result.FHeight := FHeight;
  Result.FBackgroundColor := FBackgroundColor;
end;

procedure TBGRAAnimatedGif.LoadFromStream(Stream: TStream);
begin
  FCurrentImage    := -1;
  FWantedImage     := -1;
  FTimeAccumulator := 0;

  if FStretchedVirtualScreen <> nil then
    FStretchedVirtualScreen.FreeReference;
  if FPreviousVirtualScreen <> nil then
    FPreviousVirtualScreen.FreeReference;
  FInternalVirtualScreen.Free;
  FRestoreImage.Free;
  FBackgroundImage.Free;

  FInternalVirtualScreen := nil;
  FStretchedVirtualScreen := nil;
  FRestoreImage    := nil;
  FBackgroundImage := nil;
  FPreviousVirtualScreen := nil;

  EraseColor := clBlack;
  FPreviousDisposeMode := dmNone;

  FWidth  := 0;
  FHeight := 0;

  if Stream <> nil then
    LoadImages(Stream);
end;

procedure TBGRAAnimatedGif.SaveToStream(Stream: TStream);
begin
  //not implemented
end;

procedure TBGRAAnimatedGif.LoadFromFile(const AFilenameUTF8: string);
var stream: TFileStreamUTF8;
begin
  stream := TFileStreamUTF8.Create(AFilenameUTF8,fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TBGRAAnimatedGif.SaveToFile(const AFilenameUTF8: string);
var
  Stream: TFileStreamUTF8;
begin
  Stream := TFileStreamUTF8.Create(AFilenameUTF8, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

{$HINTS OFF}
procedure TBGRAAnimatedGif.LoadImages(stream: TStream);

  procedure DumpData;
  var
    Count: byte;
  begin
    repeat
      stream.Read(Count, 1);
      stream.position := stream.position + Count;
    until (Count = 0) or (stream.position >= stream.size);
  end;

type
  TRGB = packed record
    r, g, b: byte;
  end;

  TPalette = array of TBGRAPixel;

  function rgbToColor(rgb: TRGB): TBGRAPixel;
  begin
    Result.red   := rgb.r;
    Result.green := rgb.g;
    Result.blue  := rgb.b;
    Result.alpha := 255;
  end;

const
  GIFScreenDescriptor_GlobalColorTableFlag = $80;
  GIFImageDescriptor_LocalColorTableFlag = $80;
  GIFImageDescriptor_InterlacedFlag = $40;
  GIFGraphicControlExtension_TransparentFlag = $01;

const
  ilstart: array[1..4] of longint = (0, 4, 2, 1);
  ilstep: array[1..4] of longint = (8, 8, 4, 2);

var
  NewImages: array of TGifSubImage;
  NbImages:  integer;

  GIFSignature: TGIFSignature;
  GIFScreenDescriptor: TGIFScreenDescriptor;
  GIFBlockID:   char;
  GIFImageDescriptor: TGIFImageDescriptor;

  globalPalette: TPalette;
  localPalette:  TPalette;

  transcolorIndex: integer;
  delay: integer;
  disposeMode: TDisposeMode;

  procedure LoadGlobalPalette;
  var
    NbEntries, i: integer;
    rgb: TRGB;
  begin
    NbEntries := 1 shl (GIFScreenDescriptor.flags and $07 + 1);
    setlength(globalPalette, NbEntries);
    for i := 0 to NbEntries - 1 do
    begin
      stream.Read(rgb, 3);
      globalPalette[i] := rgbToColor(rgb);
    end;
  end;

  procedure LoadLocalPalette;
  var
    NbEntries, i: integer;
    rgb: TRGB;
  begin
    NbEntries := 1 shl (GIFImageDescriptor.flags and $07 + 1);
    setlength(localPalette, NbEntries);
    for i := 0 to NbEntries - 1 do
    begin
      stream.Read(rgb, 3);
      localPalette[i] := rgbToColor(rgb);
    end;
  end;

  procedure decodeGIFLZW(image: TBGRABitmap; const pal: TPalette; interlaced: boolean);
  var
    xd, yd: longint;
  const
    tablen = 4095;
  type
    Pstr = ^Tstr;

    Tstr = record
      prefix: Pstr;
      suffix: longint;
    end;
    Pstrtab = ^Tstrtab;
    Tstrtab = array[0..tablen] of Tstr;

  var
    strtab:   Pstrtab;
    oldcode, curcode, clearcode, endcode: longint;
    codesize, codelen, codemask: longint;
    stridx:   longint;
    bitbuf, bitsinbuf: longint;
    bytbuf:   array[0..255] of byte;
    bytinbuf, bytbufidx: byte;
    endofsrc: boolean;
    xcnt, ycnt, ystep, pass: longint;

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
      for i := clearcode to tablen do
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
      for i := clearcode to tablen do
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
          stream.Read(bytinbuf, 1);
          if (bytinbuf = 0) then
            endofsrc := True;
          stream.Read(bytbuf, bytinbuf);
          bytbufidx := 0;
        end;
        bitbuf := bitbuf or (longint(byte(bytbuf[bytbufidx])) shl bitsinbuf);
        Inc(bytbufidx);
        Dec(bytinbuf);
        Inc(bitsinbuf, 8);
      end;
      Result := bitbuf and codemask;
      {DBG(bitbuf AND codemask);}
      bitbuf := bitbuf shr codelen;
      Dec(bitsinbuf, codelen);
    end;

    procedure AddStr2Tab(prefix: Pstr; suffix: longint);
    begin
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
            ycnt  := ilstart[pass];
            ystep := ilstep[pass];
          end;
        end;
      end;

      colorIndex := s^.suffix;
      if (colorIndex <> transcolorIndex) and (colorIndex >= 0) and
        (colorIndex < length(pal)) then
        image.setpixel(xcnt, ycnt, pal[colorIndex]);

      Inc(xcnt);
      if (xcnt >= xd) then
      begin
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
    {DBG('lzw start');}
    endofsrc := False;
    xd   := image.Width;
    yd   := image.Height;
    xcnt := 0;
    if interlaced then
    begin
      pass  := 1;
      ycnt  := ilstart[pass];
      ystep := ilstep[pass];
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
    stream.Read(codesize, 1);
    {DBG(codesize);}
    InitStringTable;
    curcode := getnextcode;
    {DBG(curcode);}
    while (curcode <> endcode) and (pass < 5) and not endofsrc{ AND NOT finished} do
    begin
{DBG('-----');
DBG(curcode);
DBGw(stridx);}
      if (curcode = clearcode) then
      begin
        ClearStringTable;
        repeat
          curcode := getnextcode;
          {DBG('lzw clear');}
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
    {putimage(0,0,image);}
{DBG('lzw end');
DBG(bytinbuf);}
    if not endofsrc then
      DumpData;
    {DBG('lzw finished');}
  end;

  procedure LoadImage;
  var
    imgWidth, imgHeight: integer;
    img:     TBGRABitmap;
    Interlaced: boolean;
    palette: TPalette;
  begin
    stream.Read(GIFImageDescriptor, sizeof(GIFImageDescriptor));
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

    if length(NewImages) <= NbImages then
      setlength(NewImages, length(NewImages) * 2 + 1);
    img := TBGRABitmap.Create(imgWidth, imgHeight);
    img.Fill(BGRAPixelTransparent);
    NewImages[NbImages].Image    := img;
    NewImages[NbImages].Position := point(GIFImageDescriptor.x, GIFImageDescriptor.y);
    NewImages[NbImages].Delay    := Delay;
    NewImages[NbImages].disposeMode := disposeMode;

    if (transcolorIndex >= 0) and (transcolorIndex < length(palette)) then
      NewImages[nbImages].TransparentColor := palette[transcolorIndex]
    else
      NewImages[nbImages].TransparentColor := BGRAPixelTransparent;

    Inc(NbImages);

    Interlaced := GIFImageDescriptor.flags and GIFImageDescriptor_InterlacedFlag =
      GIFImageDescriptor_InterlacedFlag;
    DecodeGIFLZW(img, palette, Interlaced);
  end;

  procedure ChangeImages;
  var
    i: integer;
  begin
    Clear;
    SetLength(FImages, NbImages);
    FFullAnimationTime:= 0;
    for i := 0 to Count - 1 do
    begin
      FImages[i] := NewImages[i];
      FFullAnimationTime += NewImages[i].Delay;
    end;
  end;

  procedure ReadExtension;
  var
    GIFExtensionBlock: TGIFExtensionBlock;
    GIFGraphicControlExtension: TGIFGraphicControlExtension;
    mincount, Count:   byte;

  begin
    stream.Read(GIFExtensionBlock, sizeof(GIFExtensionBlock));
    case GIFExtensionBlock.functioncode of
      $F9:
      begin
        stream.Read(Count, 1);
        if Count < sizeof(GIFGraphicControlExtension) then
          mincount := 0
        else
        begin
          mincount := sizeof(GIFGraphicControlExtension);
          stream.Read(GIFGraphicControlExtension, mincount);

          if GIFGraphicControlExtension.flags and
            GIFGraphicControlExtension_TransparentFlag =
            GIFGraphicControlExtension_TransparentFlag then
            transcolorIndex := GIFGraphicControlExtension.transcolor
          else
            transcolorIndex := -1;
          if GIFGraphicControlExtension.delaytime <> 0 then
            Delay     := GIFGraphicControlExtension.delaytime * 10;
          disposeMode := TDisposeMode((GIFGraphicControlExtension.flags shr 2) and 7);
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
  NewImages := nil;
  NbImages  := 0;
  transcolorIndex := -1;
  Delay     := 100;
  FBackgroundColor := clBlack;
  FWidth    := 0;
  FHeight   := 0;
  disposeMode := dmErase;

  stream.Read(GIFSignature, sizeof(GIFSignature));
  if (GIFSignature[1] = 'G') and (GIFSignature[2] = 'I') and (GIFSignature[3] = 'F') then
  begin
    stream.Read(GIFScreenDescriptor, sizeof(GIFScreenDescriptor));
    FWidth  := GIFScreenDescriptor.Width;
    FHeight := GIFScreenDescriptor.Height;
    if (GIFScreenDescriptor.flags and GIFScreenDescriptor_GlobalColorTableFlag =
      GIFScreenDescriptor_GlobalColorTableFlag) then
    begin
      LoadGlobalPalette;
      if GIFScreenDescriptor.background < length(globalPalette) then
        FBackgroundColor :=
          BGRAToColor(globalPalette[GIFScreenDescriptor.background]);
    end;
    repeat
      stream.Read(GIFBlockID, sizeof(GIFBlockID));
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
  ChangeImages;
end;

procedure TBGRAAnimatedGif.Draw(ACanvas: TCanvas; const Rect: TRect);
begin
  if FBackgroundImage <> nil then
    FreeAndNil(FBackgroundImage);
  SaveBackgroundOnce(ACanvas, Rect);

  if FPreviousVirtualScreen <> nil then
  begin
    FPreviousVirtualScreen.FreeReference;
    FPreviousVirtualScreen := nil;
  end;

  Render(Rect.Right - Rect.Left, Rect.Bottom - Rect.Top);
  FStretchedVirtualScreen.Draw(ACanvas, Rect.Left, Rect.Top, false);
  FImageChanged := False;

  FPreviousVirtualScreen := TBGRABitmap(FStretchedVirtualScreen.Duplicate);
end;

function TBGRAAnimatedGif.GetEmpty: boolean;
begin
  Result := (length(FImages) = 0);
end;

function TBGRAAnimatedGif.GetHeight: integer;
begin
  Result := FHeight;
end;

function TBGRAAnimatedGif.GetTransparent: boolean;
begin
  Result := True;
end;

function TBGRAAnimatedGif.GetWidth: integer;
begin
  Result := FWidth;
end;

procedure TBGRAAnimatedGif.SetHeight(Value: integer);
begin
  //not implemented
end;

procedure TBGRAAnimatedGif.SetTransparent(Value: boolean);
begin
  //not implemented
end;

procedure TBGRAAnimatedGif.SetWidth(Value: integer);
begin
  //not implemented
end;

procedure TBGRAAnimatedGif.SaveBackgroundOnce(Canvas: TCanvas; ARect: TRect);
begin
  if (FBackgroundImage <> nil) and
    ((FBackgroundImage.Width <> ARect.Right - ARect.Left) or
    (FBackgroundImage.Height <> ARect.Bottom - ARect.Top)) then
    FreeAndNil(FBackgroundImage);

  if (BackgroundMode in [gbmSaveBackgroundOnce, gbmUpdateBackgroundContinuously]) and
    (FBackgroundImage = nil) then
  begin
    FBackgroundImage := TBGRABitmap.Create(ARect.Right - ARect.Left,
      ARect.Bottom - ARect.Top);
    FBackgroundImage.GetImageFromCanvas(Canvas, ARect.Left, ARect.Top);
  end;
end;

procedure TBGRAAnimatedGif.SetCurrentImage(Index: integer);
begin
  if (Index >= 0) and (Index < Length(FImages)) then
    FWantedImage := Index;
end;

{$HINTS ON}

procedure TBGRAAnimatedGif.Clear;
var
  i: integer;
begin
  inherited Clear;

  for i := 0 to Count - 1 do
    FImages[i].Image.FreeReference;
  FImages := nil;
end;

destructor TBGRAAnimatedGif.Destroy;
begin
  Clear;

  if FStretchedVirtualScreen <> nil then
    FStretchedVirtualScreen.FreeReference;
  if FPreviousVirtualScreen <> nil then
    FPreviousVirtualScreen.FreeReference;
  FInternalVirtualScreen.Free;
  FRestoreImage.Free;
  FBackgroundImage.Free;
  inherited Destroy;
end;

procedure TBGRAAnimatedGif.Pause;
begin
  FPaused := True;
end;

procedure TBGRAAnimatedGif.Resume;
begin
  FPaused := False;
end;

procedure TBGRAAnimatedGif.Show(Canvas: TCanvas; ARect: TRect);
begin
  Canvas.StretchDraw(ARect, self);
end;

procedure TBGRAAnimatedGif.Update(Canvas: TCanvas; ARect: TRect);
var
  n: integer;
  PChangePix, PNewPix, PBackground, PNewBackground: PLongWord;
  oldpix, newpix, newbackpix: longword;
  NewBackgroundImage: TBGRABitmap;
begin
  if (BackgroundMode = gbmUpdateBackgroundContinuously) and
    (FBackgroundImage = nil) then
    BackgroundMode := gbmSaveBackgroundOnce;

  SaveBackgroundOnce(Canvas, ARect);

  case BackgroundMode of
    gbmSimplePaint:
    begin
      UpdateSimple(Canvas, ARect);
      exit;
    end;
    gbmEraseBackground:
    begin
      UpdateEraseBackground(Canvas, ARect);
      exit;
    end;
    gbmSaveBackgroundOnce, gbmUpdateBackgroundContinuously:
    begin
      if FPreviousVirtualScreen <> nil then
      begin
        if (FPreviousVirtualScreen.Width <> ARect.Right - ARect.Left) or
          (FPreviousVirtualScreen.Height <> ARect.Bottom - ARect.Top) then
        begin
          FPreviousVirtualScreen.FreeReference;
          FPreviousVirtualScreen := nil;
        end
        else
          FPreviousVirtualScreen := TBGRABitmap(FPreviousVirtualScreen.GetUnique);
      end;

      Render(ARect.Right - ARect.Left, ARect.Bottom - ARect.Top);

      if FImageChanged then
      begin
        if BackgroundMode = gbmUpdateBackgroundContinuously then
        begin
          NewBackgroundImage :=
            TBGRABitmap.Create(FStretchedVirtualScreen.Width,
            FStretchedVirtualScreen.Height);
          NewBackgroundImage.GetImageFromCanvas(Canvas, ARect.Left, ARect.Top);

          if FPreviousVirtualScreen = nil then
          begin
            FPreviousVirtualScreen := TBGRABitmap.Create(FWidth, FHeight);
            FPreviousVirtualScreen.Fill(BGRAPixelTransparent);
          end;

          PChangePix  := PLongWord(FPreviousVirtualScreen.ScanLine[0]);
          PNewPix     := PLongWord(FStretchedVirtualScreen.ScanLine[0]);
          PBackground := PLongWord(FBackgroundImage.ScanLine[0]);
          PNewBackground := PLongWord(NewBackgroundImage.ScanLine[0]);
          for n := FStretchedVirtualScreen.NbPixels - 1 downto 0 do
          begin
            oldpix := PChangePix^;

            if (oldpix and AlphaMask = AlphaMask) then //pixel opaque précédent
            begin
              newbackpix := PNewBackground^;
              if (newbackpix <> oldpix) then //stocke nouveau fond
                PBackground^ := newbackpix;
            end;

            newpix := PNewPix^;

            if newpix and AlphaMask = AlphaMask then
              PChangePix^ := newpix //pixel opaque
            else if newpix and AlphaMask > 0 then
            begin
              PChangePix^ := PBackground^;
              DrawPixelInlineNoAlphaCheck(PBGRAPixel(PChangePix), PBGRAPixel(@newpix)^);
            end
            else if PChangePix^ and AlphaMask <> 0 then
              PChangePix^ := PBackground^; //efface précédent

{               if newpix and AlphaMask > AlphaLimit then PChangePix^ := newpix or AlphaMask //pixel opaque
               else if PChangePix^ and AlphaMask <> 0 then PChangePix^ := PBackground^; //efface précédent}

            Inc(PNewPix);
            Inc(PChangePix);
            Inc(PBackground);
            Inc(PNewBackground);
          end;
          NewBackgroundImage.Free;
          FPreviousVirtualScreen.InvalidateBitmap;
          FPreviousVirtualScreen.Draw(Canvas, ARect.Left, ARect.Top, false);
          FPreviousVirtualScreen.PutImage(0, 0, FStretchedVirtualScreen, dmSet);
        end
        else
        begin
          if FPreviousVirtualScreen = nil then
          begin
            FStretchedVirtualScreen.Draw(Canvas, ARect.Left, ARect.Top, false);
            FPreviousVirtualScreen :=
              TBGRABitmap(FStretchedVirtualScreen.NewReference);
          end
          else
          begin
            PChangePix  := PLongWord(FPreviousVirtualScreen.ScanLine[0]);
            PNewPix     := PLongWord(FStretchedVirtualScreen.ScanLine[0]);
            PBackground := PLongWord(FBackgroundImage.ScanLine[0]);
            for n := FStretchedVirtualScreen.NbPixels - 1 downto 0 do
            begin
              newpix := PNewPix^;

              if newpix and AlphaMask = AlphaMask then
                PChangePix^ := newpix //pixel opaque
              else if newpix and AlphaMask > 0 then
              begin
                PChangePix^ := PBackground^;
                DrawPixelInlineNoAlphaCheck(PBGRAPixel(PChangePix), PBGRAPixel(@newpix)^);
              end
              else if PChangePix^ and AlphaMask <> 0 then
                PChangePix^ := PBackground^; //efface précédent

{                 if newpix and AlphaMask > AlphaLimit then PChangePix^ := newpix or AlphaMask //pixel opaque
                 else if PChangePix^ and AlphaMask <> 0 then PChangePix^ := PBackground^; //efface précédent}

              Inc(PNewPix);
              Inc(PChangePix);
              Inc(PBackground);
            end;
            FPreviousVirtualScreen.InvalidateBitmap;
            FPreviousVirtualScreen.Draw(Canvas, ARect.Left, ARect.Top, false);
            FPreviousVirtualScreen.PutImage(0, 0, FStretchedVirtualScreen, dmSet);
          end;
        end;
        FImageChanged := False;
      end;
    end;
  end;
end;

procedure TBGRAAnimatedGif.Hide(Canvas: TCanvas; ARect: TRect);
var
  shape: TBGRABitmap;
  p, pback: PBGRAPixel;
  MemEraseColor: TBGRAPixel;
  n: integer;
begin
  MemEraseColor := ColorToBGRA(EraseColor);
  if FPreviousVirtualScreen <> nil then
  begin
    if (FPreviousVirtualScreen.Width <> ARect.Right - ARect.Left) or
      (FPreviousVirtualScreen.Height <> ARect.Bottom - ARect.Top) then
    begin
      FPreviousVirtualScreen.FreeReference;
      FPreviousVirtualScreen := nil;
    end;
  end;

  case BackgroundMode of
    gbmEraseBackground, gbmSimplePaint:
    begin
      if FPreviousVirtualScreen <> nil then
      begin
        shape := TBGRABitmap(FPreviousVirtualScreen.Duplicate);
        p     := shape.ScanLine[0];
        for n := shape.NbPixels - 1 downto 0 do
        begin
          if p^.alpha <> 0 then
            p^ := MemEraseColor
          else
            p^ := BGRAPixelTransparent;
          Inc(p);
        end;
        shape.Draw(Canvas, ARect.Left, ARect.Top, false);
        shape.FreeReference;
      end;
    end;
    gbmSaveBackgroundOnce, gbmUpdateBackgroundContinuously:
    begin
      if (FPreviousVirtualScreen <> nil) and (FBackgroundImage <> nil) then
      begin
        shape := TBGRABitmap(FPreviousVirtualScreen.Duplicate);
        p     := shape.ScanLine[0];
        pback := FBackgroundImage.ScanLine[0];
        for n := shape.NbPixels - 1 downto 0 do
        begin
          if p^.alpha <> 0 then
            p^ := pback^
          else
            p^ := BGRAPixelTransparent;
          Inc(p);
          Inc(pback);
        end;
        shape.Draw(Canvas, ARect.Left, ARect.Top, false);
        shape.FreeReference;
      end;
    end;
  end;
end;

procedure TBGRAAnimatedGif.UpdateEraseBackground(Canvas: TCanvas;
  ARect: TRect; DrawOnlyIfChanged: boolean);
var
  n:      integer;
  PChangePix, PNewPix: PLongWord;
  newpix: longword;
  MemPixEraseColor: longword;
begin
  if EraseColor = clNone then
  begin
    UpdateSimple(Canvas, ARect, DrawOnlyIfChanged);
    exit;
  end;

  if FPreviousVirtualScreen <> nil then
  begin
    if (FPreviousVirtualScreen.Width <> ARect.Right - ARect.Left) or
      (FPreviousVirtualScreen.Height <> ARect.Bottom - ARect.Top) then
    begin
      FPreviousVirtualScreen.FreeReference;
      FPreviousVirtualScreen := nil;
    end
    else
      FPreviousVirtualScreen := TBGRABitmap(FPreviousVirtualScreen.GetUnique);
  end;

  Render(ARect.Right - ARect.Left, ARect.Bottom - ARect.Top);
  if FImageChanged then
  begin
    PBGRAPixel(@MemPixEraseColor)^ := ColorToBGRA(EraseColor);
    if FPreviousVirtualScreen = nil then
    begin
      FStretchedVirtualScreen.Draw(Canvas, ARect.Left, ARect.Top, false);
      FPreviousVirtualScreen := TBGRABitmap(FStretchedVirtualScreen.NewReference);
    end
    else
    begin
      PChangePix := PLongWord(FPreviousVirtualScreen.ScanLine[0]);
      PNewPix    := PLongWord(FStretchedVirtualScreen.ScanLine[0]);
      for n := FStretchedVirtualScreen.NbPixels - 1 downto 0 do
      begin
        newpix := PNewPix^;

        if newpix and AlphaMask = AlphaMask then
          PChangePix^ := newpix //pixel opaque
        else if newpix and AlphaMask > 0 then
        begin
          PChangePix^ := MemPixEraseColor;
          DrawPixelInlineNoAlphaCheck(PBGRAPixel(PChangePix), PBGRAPixel(@newpix)^);
        end
        else if PChangePix^ and AlphaMask <> 0 then
          PChangePix^ := MemPixEraseColor; //efface précédent
{           if newpix and AlphaMask > AlphaLimit then PChangePix^ := newpix or AlphaMask //pixel opaque
           else if PChangePix^ and AlphaMask <> 0 then PChangePix^ := MemPixEraseColor; //efface précédent}

        Inc(PNewPix);
        Inc(PChangePix);
      end;
      FPreviousVirtualScreen.InvalidateBitmap;
      FPreviousVirtualScreen.Draw(Canvas, ARect.Left, ARect.Top, false);
      FPreviousVirtualScreen.PutImage(0, 0, FStretchedVirtualScreen, dmSet);
    end;

    FImageChanged := False;
  end;
end;

procedure TBGRAAnimatedGif.Init;
begin
  BackgroundMode := gbmSaveBackgroundOnce;
end;

function TBGRAAnimatedGif.GetBitmap: TBitmap;
begin
  Render(FWidth, FHeight);
  Result := FStretchedVirtualScreen.Bitmap;
end;

function TBGRAAnimatedGif.GetMemBitmap: TBGRABitmap;
begin
  Render(FWidth, FHeight);
  Result := FStretchedVirtualScreen;
end;

{ TFPReaderGIF }

procedure TFPReaderGIF.InternalRead(Str: TStream; Img: TFPCustomImage);
var
  gif:  TBGRAAnimatedGif;
  x, y: integer;
  Mem:  TBGRABitmap;
begin
  gif := TBGRAAnimatedGif.Create(Str);
  Mem := gif.MemBitmap;
  if Img is TBGRABitmap then
  begin
    TBGRABitmap(Img).Assign(Mem);
  end
  else
  begin
    Img.SetSize(gif.Width, gif.Height);
    for y := 0 to gif.Height - 1 do
      for x := 0 to gif.Width - 1 do
        with Mem.GetPixel(x, y) do
          Img.Colors[x, y] := FPColor(red * $101, green * $101, blue *
            $101, alpha * $101);
  end;
  gif.Free;
end;

{$HINTS OFF}
function TFPReaderGIF.InternalCheck(Str: TStream): boolean;
var
  GIFSignature: TGIFSignature;
  savepos:      int64;
begin
  savepos := str.Position;
  try
    str.Read(GIFSignature, sizeof(GIFSignature));
    if (GIFSignature[1] = 'G') and (GIFSignature[2] = 'I') and
      (GIFSignature[3] = 'F') then
    begin
      Result := True;
    end
    else
      Result := False;
  except
    on ex: Exception do
      Result := False;
  end;
  str.Position := savepos;
end;

{$HINTS ON}

initialization

  //Free Pascal Image
  ImageHandlers.RegisterImageReader('Animated GIF', TBGRAAnimatedGif.GetFileExtensions,
    TFPReaderGIF);

  //Lazarus Picture
  TPicture.RegisterFileFormat(TBGRAAnimatedGif.GetFileExtensions, 'Animated GIF',
    TBGRAAnimatedGif);

end.

