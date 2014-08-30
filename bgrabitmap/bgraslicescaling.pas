unit BGRASliceScaling;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, BGRABitmap, BGRABitmapTypes, IniFiles, FileUtil;

type
  TMargins = record
    top, right, bottom, left: integer;
  end;
  TSlicePosition = (spTopLeft, spTop, spTopRight, spLeft, spMiddle, spRight,
    spBottomLeft, spBottom, spBottomRight);
  TSliceBitmapArray = array[TSlicePosition] of TBGRABitmap;
  TSliceRectArray = array[TSlicePosition] of TRect;
  TSliceRepeatPosition = (srpTop, srpLeft, srpMiddleHorizontal,
    srpMiddleVertical, srpRight, srpBottom);
  TSliceRepeatArray = array[TSliceRepeatPosition] of boolean;

const
  SliceRepeatPositionStr : array[TSliceRepeatPosition] of string =
      ('Top','Left','MiddleHorizontal','MiddleVertical','Right','Bottom');

function Margins(ATop, ARight, ABottom, ALeft: integer): TMargins;

type

  { TBGRASliceScaling }

  TBGRASliceScaling = class
  private
    FSliceRectArray: TSliceRectArray;
    FSliceBitmapArray: TSliceBitmapArray;
    FSliceRepeat: TSliceRepeatArray;
    FBitmap: TBGRABitmap;
    FBitmapOwned: boolean;
    FBitmapSourceRect: TRect;
    FMargins: TMargins;
    FDrawMode: TDrawMode;
    FResampleMode: TResampleMode;
    FResampleFilter: TResampleFilter;
    function GetBitmapHeight: integer;
    function GetBitmapWidth: integer;
    function GetSlice(APosition: TSlicePosition): TBGRABitmap;
    function GetSliceRepeat(Aposition: TSliceRepeatPosition): boolean;
    function GetSliceRepeatAsString: string;
    procedure SetBitmap(AValue: TBGRABitmap);
    procedure SetBitmapSourceRect(AValue: TRect);
    procedure SetDrawMode(AValue: TDrawMode);
    procedure SetResampleFilter(AValue: TResampleFilter);
    procedure SetResampleMode(AValue: TResampleMode);
    procedure SetSliceRepeat(Aposition: TSliceRepeatPosition; AValue: boolean);
    procedure SetSliceRepeatAsString(AValue: string);
  protected
    // Stuff
    procedure UpdateSliceRectArray;
    function ComputeSliceRectArray(ARect: TRect): TSliceRectArray;
    procedure SliceScalingDraw(ADest: TBGRABitmap; ADestRect: TRect;
      DrawGrid: boolean = False);
    procedure Init;
    procedure ClearBitmapArray;
  public
    // Create an instance and stores the bitmap, either as a reference to a TBGRABitmap from the caller,
    // or as a local owned copy in other cases
    constructor Create(ABitmap: TBGRABitmap;
      AMarginTop, AMarginRight, AMarginBottom, AMarginLeft: integer; ABitmapOwner: boolean = false);
    constructor Create(ABitmap: TBitmap;
      AMarginTop, AMarginRight, AMarginBottom, AMarginLeft: integer);
    constructor Create(AFilename: string;
      AMarginTop, AMarginRight, AMarginBottom, AMarginLeft: integer);
    constructor Create(AFilename: string; AIsUtf8: boolean;
      AMarginTop, AMarginRight, AMarginBottom, AMarginLeft: integer);
    constructor Create(AStream: TStream;
      AMarginTop, AMarginRight, AMarginBottom, AMarginLeft: integer);
    constructor Create(ABitmap: TBGRABitmap; ABitmapOwner: boolean = false);
    constructor Create(ABitmap: TBitmap);
    constructor Create(AFilename: string);
    constructor Create(AFilename: string; AIsUtf8: boolean);
    constructor Create(AStream: TStream);
    constructor Create;
    procedure SetMargins(AMarginTop, AMarginRight, AMarginBottom, AMarginLeft: integer);
    procedure SetMargins(AMargins: TMargins);
    destructor Destroy; override;
  public
    procedure NotifyBitmapChanged; //to notify the source bitmap has changed
    //so new bitmaps should be used
    // Draw
    procedure Draw(ABitmap: TBGRABitmap; ARect: TRect; DrawGrid: boolean = False);
    procedure Draw(ABitmap: TBGRABitmap; ALeft, ATop, AWidth, AHeight: integer;
      DrawGrid: boolean = False);
    procedure AutodetectRepeat;
  public
    // Property
    property DrawMode: TDrawMode read FDrawMode write SetDrawMode;
    property ResampleMode: TResampleMode read FResampleMode write SetResampleMode;
    property ResampleFilter: TResampleFilter read FResampleFilter
      write SetResampleFilter;
    property BitmapWidth: integer read GetBitmapWidth;
    property BitmapHeight: integer read GetBitmapHeight;
    property BitmapSource: TBGRABitmap read FBitmap write SetBitmap;
    property BitmapSourceRect: TRect read FBitmapSourceRect write SetBitmapSourceRect;
    property Margins: TMargins read FMargins write SetMargins;
    property SliceBitmap[APosition: TSlicePosition]: TBGRABitmap read GetSlice;
    property SliceRepeat[Aposition: TSliceRepeatPosition]: boolean
      read GetSliceRepeat write SetSliceRepeat;
    property SliceRepeatAsString: string read GetSliceRepeatAsString write SetSliceRepeatAsString;
  end;

  TSliceScalingArray = array of TBGRASliceScaling;
  TSliceScalingDirection = (sdHorizontal, sdVertical);
  TBGRABitmapArray = array of TBGRABitmap;

  { TBGRAMultiSliceScaling }

  TBGRAMultiSliceScaling = class
  private
    FSliceScalingArray: TSliceScalingArray;
    FBitmapOwned: boolean;
    FBitmap: TBGRABitmap;
    procedure SetFSliceScalingArray(AValue: TSliceScalingArray);
  public
    constructor Create(ABitmap: TBGRABitmap;
      AMarginTop, AMarginRight, AMarginBottom, AMarginLeft, NumberOfItems: integer;
      Direction: TSliceScalingDirection; ABitmapOwner: boolean = false);
    constructor Create(ABitmap: TBitmap;
      AMarginTop, AMarginRight, AMarginBottom, AMarginLeft, NumberOfItems: integer;
      Direction: TSliceScalingDirection);
    constructor Create(ABitmapFilename: string;
      AMarginTop, AMarginRight, AMarginBottom, AMarginLeft, NumberOfItems: integer;
      Direction: TSliceScalingDirection);
    constructor Create(ABitmapFilename: string; AIsUtf8: boolean;
      AMarginTop, AMarginRight, AMarginBottom, AMarginLeft, NumberOfItems: integer;
      Direction: TSliceScalingDirection);
    constructor Create(AStream: TStream;
      AMarginTop, AMarginRight, AMarginBottom, AMarginLeft, NumberOfItems: integer;
      Direction: TSliceScalingDirection);
    destructor Destroy; override;
    constructor Create(AIniFilename, ASection: string; AIsUtf8Filename: boolean= false);
  public
    procedure Draw(ItemNumber: integer; ABitmap: TBGRABitmap;
      ARect: TRect; DrawGrid: boolean = False);
    procedure Draw(ItemNumber: integer; ABitmap: TBGRABitmap;
      ALeft, ATop, AWidth, AHeight: integer; DrawGrid: boolean = False);
  public
    property SliceScalingArray: TSliceScalingArray
      read FSliceScalingArray write SetFSliceScalingArray;
  end;

implementation

uses types;

function Margins(ATop, ARight, ABottom, ALeft: integer): TMargins;
begin
  Result.top := atop;
  Result.right := aright;
  Result.bottom := abottom;
  Result.left := aleft;
end;

{ TBGRAMultiSliceScaling }

procedure TBGRAMultiSliceScaling.SetFSliceScalingArray(AValue: TSliceScalingArray);
begin
  if FSliceScalingArray = AValue then
    Exit;
  FSliceScalingArray := AValue;
end;

constructor TBGRAMultiSliceScaling.Create(ABitmap: TBGRABitmap;
  AMarginTop, AMarginRight, AMarginBottom, AMarginLeft, NumberOfItems: integer;
  Direction: TSliceScalingDirection; ABitmapOwner: boolean = false);
var
  i: integer;
  ItemWidth,ItemHeight,ItemStepX,ItemStepY: integer;
begin
  FBitmap := ABitmap;
  FBitmapOwned := ABitmapOwner;
  ItemWidth := ABitmap.Width;
  ItemHeight := ABitmap.Height;
  ItemStepX := 0;
  ItemStepY := 0;
  case Direction of
    sdVertical: begin
                  ItemHeight:= ABitmap.Height div NumberOfItems;
                  ItemStepY := ItemHeight;
                end;
    sdHorizontal:
                begin
                  ItemWidth:= ABitmap.Width div NumberOfItems;
                  ItemStepX := ItemWidth;
                end;
  end;

  SetLength(FSliceScalingArray, NumberOfItems);
  for i := Low(FSliceScalingArray) to High(FSliceScalingArray) do
  begin
    FSliceScalingArray[i] := TBGRASliceScaling.Create(ABitmap,
      AMarginTop, AMarginRight, AMarginBottom, AMarginLeft);
    FSliceScalingArray[i].BitmapSourceRect := rect(ItemStepX*i,ItemStepY*i,ItemStepX*i+ItemWidth,ItemStepY*i+ItemHeight);
  end;
end;

constructor TBGRAMultiSliceScaling.Create(ABitmap: TBitmap;
  AMarginTop, AMarginRight, AMarginBottom, AMarginLeft, NumberOfItems: integer;
  Direction: TSliceScalingDirection);
begin
  Create(TBGRABitmap.Create(ABitmap), AMarginTop, AMarginRight, AMarginBottom, AMarginLeft,
    NumberOfItems, Direction, True);
end;

constructor TBGRAMultiSliceScaling.Create(ABitmapFilename: string;
  AMarginTop, AMarginRight, AMarginBottom, AMarginLeft, NumberOfItems: integer;
  Direction: TSliceScalingDirection);
begin
  Create(TBGRABitmap.Create(ABitmapFilename), AMarginTop, AMarginRight, AMarginBottom, AMarginLeft,
    NumberOfItems, Direction, True);
end;

constructor TBGRAMultiSliceScaling.Create(ABitmapFilename: string; AIsUtf8: boolean;
  AMarginTop, AMarginRight, AMarginBottom, AMarginLeft, NumberOfItems: integer;
  Direction: TSliceScalingDirection);
begin
  Create(TBGRABitmap.Create(ABitmapFilename,AIsUtf8), AMarginTop, AMarginRight, AMarginBottom, AMarginLeft,
    NumberOfItems, Direction, True);
end;

constructor TBGRAMultiSliceScaling.Create(AStream: TStream;
  AMarginTop, AMarginRight, AMarginBottom, AMarginLeft, NumberOfItems: integer;
  Direction: TSliceScalingDirection);
begin
  Create(TBGRABitmap.Create(AStream), AMarginTop, AMarginRight, AMarginBottom, AMarginLeft,
    NumberOfItems, Direction, True);
end;

destructor TBGRAMultiSliceScaling.Destroy;
var
  i: integer;
begin
  for i := Low(FSliceScalingArray) to High(FSliceScalingArray) do
    FSliceScalingArray[i].Free;
  if FBitmapOwned then FBitmap.Free;

  inherited Destroy;
end;

constructor TBGRAMultiSliceScaling.Create(AIniFilename, ASection: string;
  AIsUtf8Filename: boolean);
var
  i: integer;
  temp: TMemIniFile;
  Direction: TSliceScalingDirection;
  defaultRepeat: string;
  IniPathUTF8,BitmapFilename: string;
begin
  if AIsUtf8Filename then
  begin
    if not FileExistsUTF8(AIniFilename) then exit;
    temp := TMemIniFile.Create(UTF8ToSys(AIniFilename));
    IniPathUTF8 := ExtractFilePath(AIniFilename);
  end else
  begin
    if not FileExists(AIniFilename) then exit;
    temp := TMemIniFile.Create(AIniFilename);
    IniPathUTF8 := SysToUTF8(ExtractFilePath(AIniFilename));
  end;

  if temp.ReadBool(ASection, 'HorizontalDirection', False) then
    Direction := sdHorizontal
  else
    Direction := sdVertical;

  BitmapFilename := temp.ReadString(ASection, 'Bitmap', '');
  if (copy(BitmapFilename,1,2) = '.\') or (copy(BitmapFilename,1,2) = './') then
    BitmapFilename := IniPathUTF8+SysToUTF8(copy(BitmapFilename,3,Length(BitmapFilename)-2));
  Create(
    BitmapFilename,True,
    temp.ReadInteger(ASection, 'MarginTop', 0),
    temp.ReadInteger(ASection, 'MarginRight', 0),
    temp.ReadInteger(ASection, 'MarginBottom', 0),
    temp.ReadInteger(ASection, 'MarginLeft', 0),
    temp.ReadInteger(ASection, 'NumberOfItems', 1),
    Direction);

  defaultRepeat := temp.ReadString(ASection, 'Repeat', 'Auto');
  for i := 0 to High(FSliceScalingArray) do
    FSliceScalingArray[i].SliceRepeatAsString := temp.ReadString(ASection, 'Repeat'+IntToStr(i+1), defaultRepeat);

  temp.Free;
end;

procedure TBGRAMultiSliceScaling.Draw(ItemNumber: integer; ABitmap: TBGRABitmap;
  ARect: TRect; DrawGrid: boolean);
begin
  FSliceScalingArray[ItemNumber].Draw(ABitmap, ARect, DrawGrid);
end;

procedure TBGRAMultiSliceScaling.Draw(ItemNumber: integer; ABitmap: TBGRABitmap;
  ALeft, ATop, AWidth, AHeight: integer; DrawGrid: boolean);
begin
  FSliceScalingArray[ItemNumber].Draw(ABitmap, ALeft, ATop, AWidth, AHeight, DrawGrid);
end;

{ TBGRASliceScaling }

procedure TBGRASliceScaling.SetDrawMode(AValue: TDrawMode);
begin
  if FDrawMode = AValue then
    Exit;
  FDrawMode := AValue;
end;

procedure TBGRASliceScaling.SetBitmap(AValue: TBGRABitmap);
begin
  if FBitmap = AValue then
    Exit;
  if FBitmapOwned then
    FBitmap.Free;
  FBitmap := AValue;
  FBitmapOwned := False;
  UpdateSliceRectArray;
end;

procedure TBGRASliceScaling.SetBitmapSourceRect(AValue: TRect);
begin
  if (FBitmapSourceRect.Left=AValue.Left) and
     (FBitmapSourceRect.Right=AValue.Right) and
     (FBitmapSourceRect.Top=AValue.Top) and
     (FBitmapSourceRect.Bottom=AValue.Bottom) then Exit;
  FBitmapSourceRect:=AValue;
  UpdateSliceRectArray;
end;

function TBGRASliceScaling.GetSlice(APosition: TSlicePosition): TBGRABitmap;
begin
  if FSliceBitmapArray[APosition] = nil then
    with FSliceRectArray[APosition] do
    begin
      FSliceBitmapArray[APosition] := TBGRABitmap.Create(right - left, bottom - top);
      FSliceBitmapArray[APosition].PutImage(-left, -top, FBitmap, dmSet);
    end;
  Result := FSliceBitmapArray[APosition];
end;

function TBGRASliceScaling.GetBitmapHeight: integer;
begin
  result := FBitmapSourceRect.Bottom - FBitmapSourceRect.Top;
end;

function TBGRASliceScaling.GetBitmapWidth: integer;
begin
  result := FBitmapSourceRect.Right - FBitmapSourceRect.Left;
end;

function TBGRASliceScaling.GetSliceRepeat(Aposition: TSliceRepeatPosition): boolean;
begin
  Result := FSliceRepeat[Aposition];
end;

function TBGRASliceScaling.GetSliceRepeatAsString: string;
var p: TSliceRepeatPosition;
begin
  result := '';
  for p := low(TSliceRepeatPosition) to high(TSliceRepeatPosition) do
    if SliceRepeat[p] then
    begin
      if result <> '' then result += '+';
      result += SliceRepeatPositionStr[p];
    end;
end;

procedure TBGRASliceScaling.SetResampleFilter(AValue: TResampleFilter);
begin
  if FResampleFilter = AValue then
    Exit;
  FResampleFilter := AValue;
end;

procedure TBGRASliceScaling.SetResampleMode(AValue: TResampleMode);
begin
  if FResampleMode = AValue then
    Exit;
  FResampleMode := AValue;
end;

procedure TBGRASliceScaling.SetSliceRepeat(Aposition: TSliceRepeatPosition;
  AValue: boolean);
begin
  FSliceRepeat[Aposition] := AValue;
end;

procedure TBGRASliceScaling.SetSliceRepeatAsString(AValue: string);
var p: TSliceRepeatPosition;
  attr: string;
  idx: integer;
begin
  AValue := trim(AValue);
  if compareText(AValue,'All')=0 then
  begin
    for p := low(TSliceRepeatPosition) to high(TSliceRepeatPosition) do
      SliceRepeat[p] := true;
    exit;
  end;
  for p := low(TSliceRepeatPosition) to high(TSliceRepeatPosition) do
    SliceRepeat[p] := false;
  if compareText(AValue,'None')=0 then exit;

  while AValue <> '' do
  begin
    idx := pos('+',AValue);
    if idx <> 0 then
    begin
      attr := copy(AValue,1,idx-1);
      delete(AValue,1,idx);
    end else
    begin
      attr := AValue;
      AValue := '';
    end;
    for p := low(TSliceRepeatPosition) to high(TSliceRepeatPosition) do
      if CompareText(SliceRepeatPositionStr[p],attr)=0 then
      begin
        SliceRepeat[p] := true;
        attr := '';
        break;
      end;
    if compareText(attr,'Auto')=0 then AutodetectRepeat else
      if attr <> '' then
        raise exception.Create('Unknown slice repeat attribute ('+attr+')');
  end;
end;

procedure TBGRASliceScaling.UpdateSliceRectArray;
begin
  ClearBitmapArray;
  if FBitmap = nil then exit;
  FSliceRectArray := ComputeSliceRectArray(FBitmapSourceRect);
end;

function TBGRASliceScaling.ComputeSliceRectArray(ARect: TRect): TSliceRectArray;
var
  Width, Height: integer;
  pos: TSlicePosition;
  lMargins: TMargins;
  ratio: single;
begin
  Width := ARect.Right - ARect.Left;
  Height := ARect.Bottom - ARect.Top;
  if (Width <= 0) or (Height <= 0) then
    raise Exception.Create('Empty rectangle');

  lMargins := FMargins;
  if lMargins.top < 0 then
    lMargins.top := 0;
  if lMargins.right < 0 then
    lMargins.right := 0;
  if lMargins.bottom < 0 then
    lMargins.bottom := 0;
  if lMargins.left < 0 then
    lMargins.left := 0;
  if lmargins.left + lMargins.right >= Width then
  begin
    ratio := Width / (lmargins.left + lMargins.right + 1);
    lMargins.left := trunc(lMargins.left * ratio);
    lMargins.right := trunc(lMargins.right * ratio);
  end;
  if lmargins.top + lMargins.bottom >= Height then
  begin
    ratio := Height / (lmargins.top + lMargins.bottom + 1);
    lMargins.top := trunc(lMargins.top * ratio);
    lMargins.bottom := trunc(lMargins.bottom * ratio);
  end;
  with lMargins do
  begin
    Result[spTopLeft] := rect(0, 0, Left, Top);
    Result[spTop] := rect(Left, 0, Width - Right, Top);
    Result[spTopRight] := rect(Width - Right, 0, Width, Top);
    Result[spLeft] := rect(0, Top, Left, Height - Bottom);
    Result[spMiddle] := rect(Left, Top, Width - Right, Height - Bottom);
    Result[spRight] := rect(Width - Right, Top, Width, Height - Bottom);
    Result[spBottomLeft] := rect(0, Height - Bottom, Left, Height);
    Result[spBottom] := rect(Left, Height - Bottom, Width - Right, Height);
    Result[spBottomRight] := rect(Width - Right, Height - Bottom, Width, Height);
  end;
  for pos := low(TSlicePosition) to high(TSlicePosition) do
    OffsetRect(Result[pos], ARect.Left, ARect.Top);
end;

procedure TBGRASliceScaling.SliceScalingDraw(ADest: TBGRABitmap;
  ADestRect: TRect; DrawGrid: boolean);
var
  pos: TSlicePosition;
  tempBGRA: TBGRABitmap;
  DestSliceRect: TSliceRectArray;
  repeatSlice: boolean;
begin
  if (ADestRect.Right <= ADestRect.Left) or (ADestRect.Bottom <= ADestRect.Top) then
    exit;
  DestSliceRect := ComputeSliceRectArray(ADestRect);
  for pos := Low(TSlicePosition) to High(TSlicePosition) do
  begin
    with DestSliceRect[pos] do
    begin
      if (right > left) and (bottom > top) then
      begin
        case pos of
          spTop: repeatSlice := SliceRepeat[srpTop];
          spRight: repeatSlice := SliceRepeat[srpRight];
          spBottom: repeatSlice := SliceRepeat[srpBottom];
          spLeft: repeatSlice := SliceRepeat[srpLeft];
          spMiddle: repeatSlice :=
              SliceRepeat[srpMiddleHorizontal] and SliceRepeat[srpMiddleVertical];
          else
            repeatSlice := False;
        end;
        //simple copy
        if (right - left = FSliceRectArray[pos].right - FSliceRectArray[pos].left) and
          (bottom - top = FSliceRectArray[pos].bottom - FSliceRectArray[pos].top) then
        begin
          FBitmap.ScanOffset :=
            point(FSliceRectArray[pos].left - left, FSliceRectArray[pos].top - top);
          ADest.FillRect(left, top, right, bottom, FBitmap, FDrawMode);
        end
        else
        //repeat in both direction
        if repeatSlice then
        begin
          tempBGRA := SliceBitmap[pos];
          tempBGRA.ScanOffset := point(-left, -top);
          ADest.FillRect(left, top, right, bottom, tempBGRA, FDrawMode);
        end
        else
        //resample in both directions (or in one direction if the other direction has the same size)
        if (pos <> spMiddle) or (not SliceRepeat[srpMiddleHorizontal] and
          not SliceRepeat[srpMiddleVertical]) then
        begin
          SliceBitmap[pos].ResampleFilter := ResampleFilter;
          tempBGRA := SliceBitmap[pos].Resample(right - left, bottom -
            top, FResampleMode) as TBGRABitmap;
          ADest.PutImage(left, top, tempBGRA, FDrawMode);
          tempBGRA.Free;
        end
        else //one dimension resample, other dimension resample
        begin
          SliceBitmap[pos].ResampleFilter := ResampleFilter;
          if not SliceRepeat[srpMiddleHorizontal] then
            tempBGRA := SliceBitmap[pos].Resample(
              right - left, SliceBitmap[pos].Height, FResampleMode) as TBGRABitmap
          else
            tempBGRA := SliceBitmap[pos].Resample(
              SliceBitmap[pos].Width, bottom - top, FResampleMode) as TBGRABitmap;
          tempBGRA.ScanOffset := point(-left, -top);
          ADest.FillRect(left, top, right, bottom, tempBGRA, FDrawMode);
          tempBGRA.Free;
        end;
      end;
    end;
  end;
  if DrawGrid then
  begin
    ADest.DrawLineAntialias(DestSliceRect[spTop].left, DestSliceRect[spTop].top,
      DestSliceRect[spBottom].left, DestSliceRect[spBottom].bottom,
      BGRA(255, 0, 0, 255), BGRAPixelTransparent, 1, False);
    ADest.DrawLineAntialias(DestSliceRect[spTop].right - 1, DestSliceRect[spTop].top,
      DestSliceRect[spBottom].right - 1, DestSliceRect[spBottom].bottom,
      BGRA(255, 0, 0, 255), BGRAPixelTransparent, 1, False);
    ADest.DrawLineAntialias(DestSliceRect[spLeft].left, DestSliceRect[spLeft].top,
      DestSliceRect[spRight].right, DestSliceRect[spRight].top,
      BGRA(255, 0, 0, 255), BGRAPixelTransparent, 1, False);
    ADest.DrawLineAntialias(DestSliceRect[spLeft].left, DestSliceRect[spLeft].bottom - 1,
      DestSliceRect[spRight].right, DestSliceRect[spRight].bottom - 1,
      BGRA(255, 0, 0, 255), BGRAPixelTransparent, 1, False);
  end;
end;

procedure TBGRASliceScaling.Init;
var
  pos: TSliceRepeatPosition;
begin
  FBitmap := nil;
  FBitmapOwned := False;
  for pos := low(TSliceRepeatPosition) to high(TSliceRepeatPosition) do
    FSliceRepeat[pos] := False;
  SetMargins(0, 0, 0, 0);
  FBitmapSourceRect := rect(0,0,0,0);
  DrawMode := dmDrawWithTransparency;
  ResampleMode := rmFineResample;
  ResampleFilter := rfHalfCosine;
end;

procedure TBGRASliceScaling.ClearBitmapArray;
var
  pos: TSlicePosition;
begin
  for pos := low(TSlicePosition) to high(TSlicePosition) do
    FreeAndNil(FSliceBitmapArray[pos]);
end;

constructor TBGRASliceScaling.Create(ABitmap: TBGRABitmap;
  AMarginTop, AMarginRight, AMarginBottom, AMarginLeft: integer; ABitmapOwner: boolean = false);
begin
  Create(ABitmap, ABitmapOwner);
  SetMargins(AMarginTop, AMarginRight, AMarginBottom, AMarginLeft);
end;

constructor TBGRASliceScaling.Create(ABitmap: TBitmap;
  AMarginTop, AMarginRight, AMarginBottom, AMarginLeft: integer);
begin
  Create(ABitmap);
  SetMargins(AMarginTop, AMarginRight, AMarginBottom, AMarginLeft);
end;

constructor TBGRASliceScaling.Create(AFilename: string;
  AMarginTop, AMarginRight, AMarginBottom, AMarginLeft: integer);
begin
  Create(AFilename);
  SetMargins(AMarginTop, AMarginRight, AMarginBottom, AMarginLeft);
end;

constructor TBGRASliceScaling.Create(AFilename: string; AIsUtf8: boolean;
  AMarginTop, AMarginRight, AMarginBottom, AMarginLeft: integer);
begin
  Create(AFilename, AIsUtf8);
  SetMargins(AMarginTop, AMarginRight, AMarginBottom, AMarginLeft);
end;

constructor TBGRASliceScaling.Create(AStream: TStream;
  AMarginTop, AMarginRight, AMarginBottom, AMarginLeft: integer);
begin
  Create(AStream);
  SetMargins(AMarginTop, AMarginRight, AMarginBottom, AMarginLeft);
end;

constructor TBGRASliceScaling.Create(ABitmap: TBGRABitmap; ABitmapOwner: boolean = false);
begin
  Init;
  FBitmap := ABitmap;
  FBitmapOwned := ABitmapOwner;
  FBitmapSourceRect := rect(0,0,FBitmap.Width,FBitmap.Height);
end;

constructor TBGRASliceScaling.Create(ABitmap: TBitmap);
begin
  Init;
  FBitmap := TBGRABitmap.Create(ABitmap);
  FBitmapOwned := True;
  FBitmapSourceRect := rect(0,0,FBitmap.Width,FBitmap.Height);
end;

constructor TBGRASliceScaling.Create(AFilename: string);
begin
  Init;
  FBitmap := TBGRABitmap.Create(AFilename);
  FBitmapOwned := True;
  FBitmapSourceRect := rect(0,0,FBitmap.Width,FBitmap.Height);
end;

constructor TBGRASliceScaling.Create(AFilename: string; AIsUtf8: boolean);
begin
  Init;
  FBitmap := TBGRABitmap.Create(AFilename,AIsUtf8);
  FBitmapOwned := True;
  FBitmapSourceRect := rect(0,0,FBitmap.Width,FBitmap.Height);
end;

constructor TBGRASliceScaling.Create(AStream: TStream);
begin
  Init;
  FBitmap := TBGRABitmap.Create(AStream);
  FBitmapOwned := True;
  FBitmapSourceRect := rect(0,0,FBitmap.Width,FBitmap.Height);
end;

constructor TBGRASliceScaling.Create;
begin
  Init;
end;

procedure TBGRASliceScaling.SetMargins(AMarginTop, AMarginRight,
  AMarginBottom, AMarginLeft: integer);
begin
  SetMargins(BGRASliceScaling.Margins(AMarginTop, AMarginRight, AMarginBottom, AMarginLeft));
end;

procedure TBGRASliceScaling.SetMargins(AMargins: TMargins);
begin
  if (AMargins.top <> FMargins.top) or (AMargins.right <> FMargins.right) or
    (AMargins.bottom <> FMargins.bottom) or (AMargins.left <> FMargins.left) then
  begin
    FMargins := AMargins;
    UpdateSliceRectArray;
  end;
end;

destructor TBGRASliceScaling.Destroy;
begin
  ClearBitmapArray;
  if FBitmapOwned then
    FreeAndNil(FBitmap);
  inherited Destroy;
end;

procedure TBGRASliceScaling.NotifyBitmapChanged;
begin
  ClearBitmapArray;
end;

procedure TBGRASliceScaling.Draw(ABitmap: TBGRABitmap; ARect: TRect; DrawGrid: boolean);
begin
  SliceScalingDraw(ABitmap, ARect, DrawGrid);
end;

procedure TBGRASliceScaling.Draw(ABitmap: TBGRABitmap;
  ALeft, ATop, AWidth, AHeight: integer; DrawGrid: boolean);
begin
  Draw(ABitmap, rect(ALeft, ATop, ALeft + AWidth, ATop + AHeight), DrawGrid);
end;

procedure TBGRASliceScaling.AutodetectRepeat;
var
  middleSlice: TBGRABitmap;
  x, y: integer;
  p: PBGRAPixel;
  c0: TBGRAPixel;
  isRepeating: boolean;
begin
  middleSlice := SliceBitmap[spMiddle];
  isRepeating := True;
  for y := 0 to middleSlice.Height - 1 do
  begin
    p := middleSlice.ScanLine[y];
    c0 := p^;
    for x := middleSlice.Width - 1 downto 0 do
    begin
      if p^ <> c0 then
      begin
        isRepeating := False;
        break;
      end;
      Inc(p);
    end;
    if not isRepeating then
      break;
  end;
  if isRepeating then
    SliceRepeat[srpMiddleHorizontal] := True;

  isRepeating := True;
  for x := 0 to middleSlice.Width - 1 do
  begin
    c0 := middleSlice.GetPixel(x, 0);
    for y := middleSlice.Height - 1 downto 1 do
    begin
      if middleSlice.GetPixel(x, y) <> c0 then
      begin
        isRepeating := False;
        break;
      end;
      Inc(p);
    end;
    if not isRepeating then
      break;
  end;
  if isRepeating then
    SliceRepeat[srpMiddleVertical] := True;
end;

end.
