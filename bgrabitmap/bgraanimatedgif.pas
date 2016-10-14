unit BGRAAnimatedGif;

{$mode objfpc}{$H+}
{$i bgrabitmap.inc}

interface

uses
  Classes, SysUtils, BGRAGraphics, FPImage, BGRABitmap, BGRABitmapTypes,
  BGRAPalette, BGRAGifFormat;

type
  TDisposeMode = BGRAGifFormat.TDisposeMode;
  TGifSubImage = BGRAGifFormat.TGifSubImage;
  TGifSubImageArray = BGRAGifFormat.TGifSubImageArray;

  //how to deal with the background under the GIF animation
  TGifBackgroundMode = (gbmSimplePaint, gbmEraseBackground,
    gbmSaveBackgroundOnce, gbmUpdateBackgroundContinuously);

  { TBGRAAnimatedGif }

  TBGRAAnimatedGif = class(TGraphic)
  private
    FAspectRatio: single;
    FWidth, FHeight:  integer;
    FBackgroundColor: TColor;

    FPrevDate: TDateTime;
    FPaused:   boolean;
    FTimeAccumulator: double;
    FCurrentImage, FWantedImage: integer;
    FTotalAnimationTime: int64;
    FPreviousDisposeMode: TDisposeMode;

    FBackgroundImage, FPreviousVirtualScreen, FStretchedVirtualScreen,
    FInternalVirtualScreen, FRestoreImage: TBGRABitmap;
    FImageChanged: boolean;

    procedure CheckFrameIndex(AIndex: integer);
    function GetCount: integer;
    function GetFrameDelayMs(AIndex: integer): integer;
    function GetFrameDisposeMode(AIndex: integer): TDisposeMode;
    function GetFrameHasLocalPalette(AIndex: integer): boolean;
    function GetFrameImage(AIndex: integer): TBGRABitmap;
    function GetFrameImagePos(AIndex: integer): TPoint;
    function GetTimeUntilNextImage: integer;
    procedure Render(StretchWidth, StretchHeight: integer);
    procedure SetAspectRatio(AValue: single);
    procedure SetBackgroundColor(AValue: TColor);
    procedure SetFrameDelayMs(AIndex: integer; AValue: integer);
    procedure SetFrameDisposeMode(AIndex: integer; AValue: TDisposeMode);
    procedure SetFrameHasLocalPalette(AIndex: integer; AValue: boolean);
    procedure SetFrameImage(AIndex: integer; AValue: TBGRABitmap);
    procedure SetFrameImagePos(AIndex: integer; AValue: TPoint);
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

    {TGraphic}
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    function GetEmpty: boolean; override;
    function GetHeight: integer; override;
    function GetTransparent: boolean; override;
    function GetWidth: integer; override;
    procedure SetHeight({%H-}Value: integer); override;
    procedure SetTransparent({%H-}Value: boolean); override;
    procedure SetWidth({%H-}Value: integer); override;
    procedure ClearViewer; virtual;

  public
    EraseColor:     TColor;
    BackgroundMode: TGifBackgroundMode;

    constructor Create(filenameUTF8: string);
    constructor Create(stream: TStream);
    constructor Create; override;
    function Duplicate: TBGRAAnimatedGif;
    function AddFrame(AImage: TFPCustomImage; X,Y: integer; ADelayMs: integer;
      ADisposeMode: TDisposeMode = dmErase; AHasLocalPalette: boolean = false) : integer;
    procedure InsertFrame(AIndex: integer; AImage: TBGRABitmap; X,Y: integer; ADelayMs: integer;
      ADisposeMode: TDisposeMode = dmErase; AHasLocalPalette: boolean = false);

    {TGraphic}
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromFile(const AFilenameUTF8: string); override;
    procedure SaveToFile(const AFilenameUTF8: string); override;
    class function GetFileExtensions: string; override;

    procedure SetSize(AWidth,AHeight: integer); virtual;
    procedure SaveToStream(Stream: TStream; AQuantizer: TBGRAColorQuantizerAny;
      ADitheringAlgorithm: TDitheringAlgorithm); virtual; overload;
    procedure Clear; override;
    destructor Destroy; override;
    procedure Pause;
    procedure Resume;

    procedure Show(Canvas: TCanvas; ARect: TRect); overload;
    procedure Update(Canvas: TCanvas; ARect: TRect); overload;
    procedure Hide(Canvas: TCanvas; ARect: TRect); overload;

    property BackgroundColor: TColor Read FBackgroundColor write SetBackgroundColor;
    property Count: integer Read GetCount;
    property Width: integer Read FWidth;
    property Height: integer Read FHeight;
    property Paused: boolean Read FPaused;
    property Bitmap: TBitmap Read GetBitmap;
    property MemBitmap: TBGRABitmap Read GetMemBitmap;
    property CurrentImage: integer Read FCurrentImage Write SetCurrentImage;
    property TimeUntilNextImageMs: integer read GetTimeUntilNextImage;
    property FrameImage[AIndex: integer]: TBGRABitmap read GetFrameImage write SetFrameImage;
    property FrameHasLocalPalette[AIndex: integer]: boolean read GetFrameHasLocalPalette write SetFrameHasLocalPalette;
    property FrameImagePos[AIndex: integer]: TPoint read GetFrameImagePos write SetFrameImagePos;
    property FrameDelayMs[AIndex: integer]: integer read GetFrameDelayMs write SetFrameDelayMs;
    property FrameDisposeMode[AIndex: integer]: TDisposeMode read GetFrameDisposeMode write SetFrameDisposeMode;
    property AspectRatio: single read FAspectRatio write SetAspectRatio;
    property TotalAnimationTimeMs: Int64 read FTotalAnimationTime;
  end;

  { TBGRAReaderGIF }

  TBGRAReaderGIF = class(TFPCustomImageReader)
  protected
    procedure InternalRead(Str: TStream; Img: TFPCustomImage); override;
    function InternalCheck(Str: TStream): boolean; override;
  end;

  { TBGRAWriterGIF }

  TBGRAWriterGIF = class(TFPCustomImageWriter)
  protected
    procedure InternalWrite(Str: TStream; Img: TFPCustomImage); override;
  end;

const
  GifBackgroundModeStr: array[TGifBackgroundMode] of string =
    ('gbmSimplePaint', 'gbmEraseBackground', 'gbmSaveBackgroundOnce',
    'gbmUpdateBackgroundContinuously');

implementation

uses BGRABlend, BGRAUTF8{$IFDEF BGRABITMAP_USE_LCL}, Graphics{$ENDIF};

const
  {$IFDEF ENDIAN_LITTLE}
  AlphaMask = $FF000000;
  {$ELSE}
  AlphaMask = $000000FF;
  {$ENDIF}


{ TBGRAAnimatedGif }

class function TBGRAAnimatedGif.GetFileExtensions: string;
begin
  Result := 'gif';
end;

procedure TBGRAAnimatedGif.SetSize(AWidth, AHeight: integer);
begin
  ClearViewer;
  FWidth := AWidth;
  FHeight := AHeight;
end;

procedure TBGRAAnimatedGif.SaveToStream(Stream: TStream;
      AQuantizer: TBGRAColorQuantizerAny;
      ADitheringAlgorithm: TDitheringAlgorithm);
var data: TGIFData;
begin
  data.Height:= Height;
  data.Width := Width;
  data.AspectRatio := 1;
  data.BackgroundColor := BackgroundColor;
  data.Images := FImages;
  GIFSaveToStream(data, Stream, AQuantizer, ADitheringAlgorithm);
end;

procedure TBGRAAnimatedGif.Render(StretchWidth, StretchHeight: integer);
var
  curDate: TDateTime;
  previousImage, nextImage: integer;

begin
  if FInternalVirtualScreen = nil then
  begin
    FInternalVirtualScreen := TBGRABitmap.Create(FWidth, FHeight);
    if (Count = 0) and (BackgroundColor <> clNone) then
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
    if FTotalAnimationTime > 0 then FTimeAccumulator:= frac(FTimeAccumulator/FTotalAnimationTime)*FTotalAnimationTime;
    nextImage := FCurrentImage;
    while FTimeAccumulator > FImages[nextImage].DelayMs do
    begin
      FTimeAccumulator -= FImages[nextImage].DelayMs;
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
      FPreviousDisposeMode := DisposeMode;
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

procedure TBGRAAnimatedGif.SetAspectRatio(AValue: single);
begin
  if AValue < 0.25 then AValue := 0.25;
  if AValue > 4 then AValue := 4;
  if FAspectRatio=AValue then Exit;
  FAspectRatio:=AValue;
end;

procedure TBGRAAnimatedGif.SetBackgroundColor(AValue: TColor);
begin
  if FBackgroundColor=AValue then Exit;
  FBackgroundColor:=AValue;
end;

procedure TBGRAAnimatedGif.SetFrameDelayMs(AIndex: integer; AValue: integer);
begin
  CheckFrameIndex(AIndex);
  if AValue < 0 then AValue := 0;
  FTotalAnimationTime := FTotalAnimationTime + AValue - FImages[AIndex].DelayMs;
  FImages[AIndex].DelayMs := AValue;
end;

procedure TBGRAAnimatedGif.SetFrameDisposeMode(AIndex: integer;
  AValue: TDisposeMode);
begin
  CheckFrameIndex(AIndex);
  FImages[AIndex].DisposeMode := AValue;
end;

procedure TBGRAAnimatedGif.SetFrameHasLocalPalette(AIndex: integer;
  AValue: boolean);
begin
  CheckFrameIndex(AIndex);
  FImages[AIndex].HasLocalPalette := AValue;

end;

procedure TBGRAAnimatedGif.SetFrameImage(AIndex: integer; AValue: TBGRABitmap);
var ACopy: TBGRABitmap;
begin
  CheckFrameIndex(AIndex);
  ACopy := AValue.Duplicate as TBGRABitmap;
  FImages[AIndex].Image.FreeReference;
  FImages[AIndex].Image := ACopy;
end;

procedure TBGRAAnimatedGif.SetFrameImagePos(AIndex: integer; AValue: TPoint);
begin
  CheckFrameIndex(AIndex);
  FImages[AIndex].Position := AValue;
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

procedure TBGRAAnimatedGif.CheckFrameIndex(AIndex: integer);
begin
  if (AIndex < 0) or (AIndex >= Count) then Raise ERangeError.Create('Index out of bounds');
end;

function TBGRAAnimatedGif.GetCount: integer;
begin
  Result := length(FImages);
end;

function TBGRAAnimatedGif.GetFrameDelayMs(AIndex: integer): integer;
begin
  CheckFrameIndex(AIndex);
  result := FImages[AIndex].DelayMs;
end;

function TBGRAAnimatedGif.GetFrameDisposeMode(AIndex: integer): TDisposeMode;
begin
  CheckFrameIndex(AIndex);
  result := FImages[AIndex].DisposeMode;
end;

function TBGRAAnimatedGif.GetFrameHasLocalPalette(AIndex: integer): boolean;
begin
  CheckFrameIndex(AIndex);
  result := FImages[AIndex].HasLocalPalette;
end;

function TBGRAAnimatedGif.GetFrameImage(AIndex: integer): TBGRABitmap;
begin
  CheckFrameIndex(AIndex);
  result := FImages[AIndex].Image;
end;

function TBGRAAnimatedGif.GetFrameImagePos(AIndex: integer): TPoint;
begin
  CheckFrameIndex(AIndex);
  result := FImages[AIndex].Position;
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
    if acc >= FImages[FCurrentImage].DelayMs then
      result := 0
    else
      result := round(FImages[FCurrentImage].DelayMs-FTimeAccumulator);
  end;
end;

constructor TBGRAAnimatedGif.Create(filenameUTF8: string);
begin
  inherited Create;
  Init;
  LoadFromFile(filenameUTF8);
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

function TBGRAAnimatedGif.AddFrame(AImage: TFPCustomImage; X, Y: integer;
  ADelayMs: integer; ADisposeMode: TDisposeMode; AHasLocalPalette: boolean
  ): integer;
begin
  result := length(FImages);
  setlength(FImages, length(FImages)+1);
  if ADelayMs < 0 then ADelayMs:= 0;
  with FImages[result] do
  begin
    Image := TBGRABitmap.Create(AImage);
    Position := Point(x,y);
    DelayMs := ADelayMs;
    HasLocalPalette := AHasLocalPalette;
    DisposeMode := ADisposeMode;
  end;
  inc(FTotalAnimationTime, ADelayMs);
end;

procedure TBGRAAnimatedGif.InsertFrame(AIndex: integer; AImage: TBGRABitmap; X,
  Y: integer; ADelayMs: integer; ADisposeMode: TDisposeMode;
  AHasLocalPalette: boolean);
var i: integer;
begin
  if (AIndex < 0) or (AIndex > Count) then
    raise ERangeError.Create('Index out of bounds');
  setlength(FImages, length(FImages)+1);
  if ADelayMs < 0 then ADelayMs:= 0;
  for i := high(FImages) downto AIndex+1 do
    FImages[i] := FImages[i-1];
  with FImages[AIndex] do
  begin
    Image := AImage.Duplicate as TBGRABitmap;
    Position := Point(x,y);
    DelayMs := ADelayMs;
    HasLocalPalette := AHasLocalPalette;
    DisposeMode := ADisposeMode;
  end;
  inc(FTotalAnimationTime, ADelayMs);
end;

procedure TBGRAAnimatedGif.LoadFromStream(Stream: TStream);
var data: TGIFData;
  i: integer;
begin
  data := GIFLoadFromStream(Stream);

  ClearViewer;
  Clear;
  FWidth  := data.Width;
  FHeight := data.Height;
  FBackgroundColor := data.BackgroundColor;
  FAspectRatio:= data.AspectRatio;

  SetLength(FImages, length(data.Images));
  FTotalAnimationTime:= 0;
  for i := 0 to high(FImages) do
  begin
    FImages[i] := data.Images[i];
    FTotalAnimationTime += FImages[i].DelayMs;
  end;
end;

procedure TBGRAAnimatedGif.SaveToStream(Stream: TStream);
begin
  SaveToStream(Stream, BGRAColorQuantizerFactory, daFloydSteinberg);
end;

procedure TBGRAAnimatedGif.LoadFromFile(const AFilenameUTF8: string);
var stream: TFileStreamUTF8;
begin
  stream := TFileStreamUTF8.Create(AFilenameUTF8,fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(stream);
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

procedure TBGRAAnimatedGif.ClearViewer;
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

  FPreviousDisposeMode := dmNone;
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

          PChangePix  := PLongWord(FPreviousVirtualScreen.Data);
          PNewPix     := PLongWord(FStretchedVirtualScreen.Data);
          PBackground := PLongWord(FBackgroundImage.Data);
          PNewBackground := PLongWord(NewBackgroundImage.Data);
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
            PChangePix  := PLongWord(FPreviousVirtualScreen.Data);
            PNewPix     := PLongWord(FStretchedVirtualScreen.Data);
            PBackground := PLongWord(FBackgroundImage.Data);
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
        p     := shape.Data;
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
        p     := shape.Data;
        pback := FBackgroundImage.Data;
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
      PChangePix := PLongWord(FPreviousVirtualScreen.Data);
      PNewPix    := PLongWord(FStretchedVirtualScreen.Data);
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

{ TBGRAReaderGIF }

procedure TBGRAReaderGIF.InternalRead(Str: TStream; Img: TFPCustomImage);
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

function TBGRAReaderGIF.InternalCheck(Str: TStream): boolean;
var
  GIFSignature: TGIFSignature;
  savepos:      int64;
begin
  savepos := str.Position;
  try
    fillchar({%H-}GIFSignature, sizeof(GIFSignature), 0);
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

{ TBGRAWriterGIF }

procedure TBGRAWriterGIF.InternalWrite(Str: TStream; Img: TFPCustomImage);
var
  gif: TBGRAAnimatedGif;
begin
  gif := TBGRAAnimatedGif.Create;
  try
    gif.SetSize(Img.Width,Img.Height);
    gif.AddFrame(Img, 0,0,0);
    gif.SaveToStream(Str, BGRAColorQuantizerFactory, daFloydSteinberg);
  except
    on ex: EColorQuantizerMissing do
    begin
      FreeAndNil(gif);
      raise EColorQuantizerMissing.Create('Please define the color quantizer factory. You can do that with the following statements: Uses BGRAPalette, BGRAColorQuantization; BGRAColorQuantizerFactory:= TBGRAColorQuantizer;');
    end;
    on ex: Exception do
    begin
      FreeAndNil(gif);
      raise ex;
    end;
  end;
  FreeAndNil(gif);
end;

initialization

  DefaultBGRAImageReader[ifGif] := TBGRAReaderGIF;
  DefaultBGRAImageWriter[ifGif] := TBGRAWriterGIF;

  //Free Pascal Image
  ImageHandlers.RegisterImageReader('Animated GIF', TBGRAAnimatedGif.GetFileExtensions,
    TBGRAReaderGIF);
  ImageHandlers.RegisterImageWriter('Animated GIF', TBGRAAnimatedGif.GetFileExtensions,
    TBGRAWriterGIF);

  {$IFDEF BGRABITMAP_USE_LCL}
  //Lazarus Picture
  TPicture.RegisterFileFormat(TBGRAAnimatedGif.GetFileExtensions, 'Animated GIF',
    TBGRAAnimatedGif);
  {$ENDIF}
end.

