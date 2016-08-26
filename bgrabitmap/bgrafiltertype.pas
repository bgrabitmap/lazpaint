unit BGRAFilterType;

{$mode objfpc}{$H+}

interface

uses
  Classes, BGRABitmapTypes;

const
    FilterScannerChunkSize = 16;

type
  TCheckShouldStopFunc = function(ACurrentY: integer) : boolean of object;

  { TFilterTask }

  TFilterTask = class
  private
    FCheckShouldStop: TCheckShouldStopFunc;
    FScanOffset: TPoint;
    procedure SetDestination(AValue: TBGRACustomBitmap);
    function GetInplace: boolean;
    procedure SetInplace(AValue: boolean);
  protected
    FDestination: TBGRACustomBitmap;
    FSource: TBGRACustomBitmap;
    FSourceScanner: IBGRAScanner;
    FCurrentY: integer;
    function GetShouldStop(ACurrentY: integer): boolean;
    procedure DoExecute; virtual; abstract;
    function RequestSourceScanLine(X,Y,Count: Integer): PBGRAPixel;
    procedure ReleaseSourceScanLine(P: PBGRAPixel);
    function RequestSourceExpandedScanLine(X,Y,Count: Integer): PExpandedPixel;
    procedure ReleaseSourceExpandedScanLine(P: PExpandedPixel);
    procedure SetSource(ABitmap: TBGRACustomBitmap); overload;
    procedure SetSource(AScanner: IBGRAScanner); overload;
  public
    function Execute: TBGRACustomBitmap;
    property Destination: TBGRACustomBitmap read FDestination write SetDestination;
    property CheckShouldStop: TCheckShouldStopFunc read FCheckShouldStop write FCheckShouldStop;
    property CurrentY: integer read FCurrentY;
    property ScanOffset: TPoint read FScanOffset write FScanOffset;
    property Inplace: boolean read GetInplace write SetInplace;
  end;

  { TBGRAFilterScanner }

  TBGRAFilterScanner = class(TBGRACustomScanner)
  private
    FAllowDirectRead: boolean;
    FCurX,FCurY: integer;
    FSource: IBGRAScanner;
    FOffset: TPoint;
    FVariablePixelBuffer: TBGRAPixelBuffer;
    FOutputBuffer: packed array[0..FilterScannerChunkSize-1] of TBGRAPixel;
    FOutputBufferPos: integer;
  public
    constructor Create(ASource: IBGRAScanner; AOffset: TPoint);
    procedure ComputeFilter(ASource: IBGRAScanner; X,Y: Integer; ADest: PBGRAPixel; ACount: integer); virtual; abstract;
    function ScanAtInteger(X,Y: integer): TBGRAPixel; override;
    procedure ScanMoveTo(X,Y: Integer); override;
    function ScanNextPixel: TBGRAPixel; override;
    procedure ScanPutPixels(pdest: PBGRAPixel; count: integer; mode: TDrawMode); override;
    function IsScanPutPixelsDefined: boolean; override;
    function ScanAt(X,Y: Single): TBGRAPixel; override;
    property Source: IBGRAScanner read FSource;
    property Offset: TPoint read FOffset;
    property AllowDirectRead: boolean read FAllowDirectRead write FAllowDirectRead;
  end;

  { TBGRAFilterScannerPixelwise }

  TBGRAFilterScannerPixelwise = class(TBGRAFilterScanner)
  private
    FBuffer: TBGRAPixelBuffer;
    FGammaCorrection: boolean;
  protected
    procedure DoComputeFilterAt(ASource: PBGRAPixel; ADest: PBGRAPixel;
            ACount: integer; AGammaCorrection: boolean); virtual;
  public
    constructor Create(ASource: IBGRAScanner; AOffset: TPoint; AGammaCorrection: boolean = true);
    procedure ComputeFilter(ASource: IBGRAScanner; X, Y: Integer; ADest: PBGRAPixel;
      ACount: integer); override;
    class procedure ComputeFilterAt(ASource: PBGRAPixel; ADest: PBGRAPixel;
      ACount: integer; AGammaCorrection: boolean); virtual; abstract;
    class procedure ComputeFilterInplace(ABitmap: TBGRACustomBitmap; ABounds: TRect;
      AGammaCorrection: boolean);
    property GammaCorrection: boolean read FGammaCorrection write FGammaCorrection;
  end;

  { TBGRAFilterScannerMultipixel }

  TBGRAFilterScannerMultipixel = class(TBGRAFilterScanner)
  private
    FSourceBounds: TRect;
    FKernelWidth,FKernelHeight: integer;
    FCurBufferY: integer;
    FCurBufferYDefined: boolean;
    FBuffers: array of TBGRAPixelBuffer;
    FPBuffers: array of PBGRAPixel;
  protected
    procedure DoComputeFilter(BufferX: Integer; const Buffers: array of PBGRAPixel;
      BufferWidth: integer; ADest: PBGRAPixel; ACount: integer); virtual; abstract;
    procedure LoadBuffer(ASource: IBGRAScanner; X,Y: Integer; BufferIndex: Integer; ACount: integer); virtual;
  public
    constructor Create(ASource: IBGRAScanner; ASourceBounds: TRect; AOffset: TPoint;
      AKernelWidth,AKernelHeight: Integer);
    procedure ComputeFilter(ASource: IBGRAScanner; X, Y: Integer; ADest: PBGRAPixel;
      ACount: integer); override;
    property KernelWidth: integer read FKernelWidth;
    property KernelHeight: integer read FKernelHeight;
    property SourceBounds: TRect read FSourceBounds;
  end;

implementation

uses Types, SysUtils, BGRABlend;

{ TFilterTask }

function TFilterTask.GetShouldStop(ACurrentY: integer): boolean;
begin
  FCurrentY:= ACurrentY;
  if Assigned(FCheckShouldStop) then
    result := FCheckShouldStop(ACurrentY)
  else
    result := false;
end;

function TFilterTask.RequestSourceScanLine(X, Y, Count: Integer): PBGRAPixel;
begin
  if FSource <> nil then
    result := FSource.ScanLine[y]+x
  else
  begin
    getmem(result, sizeof(TBGRAPixel)*Count);
    FSourceScanner.ScanMoveTo(X+FScanOffset.X,Y+FScanOffset.Y);
    FSourceScanner.ScanPutPixels(result,count,dmSet);
  end;
end;

procedure TFilterTask.ReleaseSourceScanLine(P: PBGRAPixel);
begin
  if FSource = nil then
    if p <> nil then freemem(p);
end;

function TFilterTask.RequestSourceExpandedScanLine(X, Y, Count: Integer
  ): PExpandedPixel;
var p: PBGRAPixel;
   pexp: PExpandedPixel;
begin
  getmem(result, sizeof(TExpandedPixel)*Count);
  if FSource <> nil then
  begin
    p := FSource.ScanLine[Y]+x;
    pexp := result;
    while Count > 0 do
    begin
      pexp^ := GammaExpansion(p^);
      inc(pexp);
      inc(p);
      dec(Count);
    end;
  end else
  begin
    FSourceScanner.ScanMoveTo(X,Y);
    pexp := result;
    while Count > 0 do
    begin
      pexp^ := FSourceScanner.ScanNextExpandedPixel;
      inc(pexp);
      dec(Count);
    end;
  end;
end;

procedure TFilterTask.ReleaseSourceExpandedScanLine(P: PExpandedPixel);
begin
  if p <> nil then freemem(p);
end;

procedure TFilterTask.SetSource(ABitmap: TBGRACustomBitmap);
begin
  FSource := ABitmap;
  FSourceScanner := nil;
end;

procedure TFilterTask.SetSource(AScanner: IBGRAScanner);
begin
  FSource := nil;
  FSourceScanner := AScanner;
end;

function TFilterTask.Execute: TBGRACustomBitmap;
var DestinationOwned: boolean;
begin
  FCurrentY := 0;
  if Destination = nil then
  begin
    if FSource = nil then //using default factory
      FDestination := BGRABitmapFactory.create(FSource.Width,FSource.Height)
    else
      FDestination := FSource.NewBitmap(FSource.Width,FSource.Height);
    DestinationOwned:= true;
  end else
    DestinationOwned:= false;
  try
    DoExecute;
    result := Destination;
    FDestination := nil;
  except
    on ex: exception do
    begin
      if DestinationOwned then FreeAndNil(FDestination);
      raise ex;
    end;
  end;
end;

procedure TFilterTask.SetDestination(AValue: TBGRACustomBitmap);
begin
  if FDestination <> nil then
    raise exception.Create('Destination is already defined');
  FDestination := AValue;
end;

function TFilterTask.GetInplace: boolean;
begin
  result := (Destination = FSource) and (FSource <> nil);
end;

procedure TFilterTask.SetInplace(AValue: boolean);
begin
  if AValue = InPlace then exit;
  if AValue and (FSource = nil) then
     raise exception.Create('Inplace is valid only when source image is defined');
  Destination := FSource;
end;

{ TBGRAFilterScanner }

constructor TBGRAFilterScanner.Create(ASource: IBGRAScanner; AOffset: TPoint);
begin
  FSource := ASource;
  FOffset := AOffset;
  FOutputBufferPos := FilterScannerChunkSize;
end;

function TBGRAFilterScanner.ScanAtInteger(X, Y: integer): TBGRAPixel;
begin
  ScanMoveTo(X,Y);
  result := ScanNextPixel;
end;

procedure TBGRAFilterScanner.ScanMoveTo(X, Y: Integer);
begin
  FCurX := X;
  FCurY := Y;
  FOutputBufferPos := FilterScannerChunkSize;
end;

function TBGRAFilterScanner.ScanNextPixel: TBGRAPixel;
begin
  if FOutputBufferPos >= FilterScannerChunkSize then
  begin
    ComputeFilter(FSource,FCurX+FOffset.X,FCurY+FOffset.Y,@FOutputBuffer[0],FilterScannerChunkSize);
    FOutputBufferPos := 0;
  end;
  Result:= FOutputBuffer[FOutputBufferPos];
  inc(FOutputBufferPos);
  inc(FCurX);
end;

procedure TBGRAFilterScanner.ScanPutPixels(pdest: PBGRAPixel; count: integer;
  mode: TDrawMode);
begin
  if mode = dmSet then
  begin
    ComputeFilter(FSource,FCurX+FOffset.X,FCurY+FOffset.Y,pdest,count);
    inc(FCurX,count);
  end else
  begin
    AllocateBGRAPixelBuffer(FVariablePixelBuffer, count);
    ComputeFilter(FSource,FCurX+FOffset.X,FCurY+FOffset.Y,@FVariablePixelBuffer[0],count);
    inc(FCurX,count);
    PutPixels(pdest, @FVariablePixelBuffer[0], count, mode, 255);
  end;
end;

function TBGRAFilterScanner.IsScanPutPixelsDefined: boolean;
begin
  Result:= true;
end;

function TBGRAFilterScanner.ScanAt(X, Y: Single): TBGRAPixel;
begin
  result := ScanAtInteger(round(X),round(Y));
end;

{ TBGRAFilterScannerPixelwise }

procedure TBGRAFilterScannerPixelwise.DoComputeFilterAt(ASource: PBGRAPixel;
  ADest: PBGRAPixel; ACount: integer; AGammaCorrection: boolean);
begin
  ComputeFilterAt(ASource,ADest,ACount,AGammaCorrection);
end;

constructor TBGRAFilterScannerPixelwise.Create(ASource: IBGRAScanner;
  AOffset: TPoint; AGammaCorrection: boolean);
begin
  inherited Create(ASource,AOffset);
  GammaCorrection := AGammaCorrection;
  //it is most likely that direct read can be used, the only exception being
  //that the destination would be equal to the source and that there would
  //be an offset
  AllowDirectRead := true;
end;

procedure TBGRAFilterScannerPixelwise.ComputeFilter(ASource: IBGRAScanner; X,
  Y: Integer; ADest: PBGRAPixel; ACount: integer);
var p: PBGRAPixel;
begin
  if AllowDirectRead and ASource.ProvidesScanline(rect(x,y,x+ACount,y+1)) then
  begin
    p := ASource.GetScanlineAt(x,y);
  end else
  begin
    AllocateBGRAPixelBuffer(FBuffer, ACount);
    ASource.ScanMoveTo(X,Y);
    p := @FBuffer[0];
    ASource.ScanPutPixels(p, ACount, dmSet);
  end;
  DoComputeFilterAt(p,ADest,ACount,GammaCorrection);
end;

class procedure TBGRAFilterScannerPixelwise.ComputeFilterInplace(
  ABitmap: TBGRACustomBitmap; ABounds: TRect; AGammaCorrection: boolean);
var
  yb: LongInt;
  p: Pointer;
begin
  ABitmap.LoadFromBitmapIfNeeded;
  if (ABounds.Left = 0) and (ABounds.Top = 0) and
     (ABounds.Right = ABitmap.Width) and (ABounds.Bottom = ABitmap.Height) then
    ComputeFilterAt(ABitmap.Data,ABitmap.Data,ABitmap.NbPixels,AGammaCorrection)
  else
    for yb := ABounds.Top to ABounds.Bottom-1 do
    begin
      p := ABitmap.ScanLine[yb]+ABounds.Left;
      ComputeFilterAt(p,p,ABounds.Right-ABounds.Left,AGammaCorrection);
    end;
  ABitmap.InvalidateBitmap;
end;

{ TBGRAFilterScannerMultipixel }

procedure TBGRAFilterScannerMultipixel.LoadBuffer(ASource: IBGRAScanner; X,
  Y: Integer; BufferIndex: Integer; ACount: integer);
begin
  if (Y < FSourceBounds.Top) or (Y >= FSourceBounds.Bottom) then
    FPBuffers[BufferIndex] := nil
  else
  if AllowDirectRead and ASource.ProvidesScanline(rect(x,y,x+ACount,y+1)) then
  begin
    FPBuffers[BufferIndex] := ASource.GetScanlineAt(X,Y);
  end else
  begin
    AllocateBGRAPixelBuffer(FBuffers[BufferIndex], ACount);
    ASource.ScanMoveTo(X,Y);
    FPBuffers[BufferIndex] := @(FBuffers[BufferIndex][0]);
    ASource.ScanPutPixels(FPBuffers[BufferIndex], ACount, dmSet);
  end;
end;

constructor TBGRAFilterScannerMultipixel.Create(ASource: IBGRAScanner;
  ASourceBounds: TRect; AOffset: TPoint; AKernelWidth, AKernelHeight: Integer);
var
  temp: Integer;
begin
  inherited Create(ASource,AOffset);
  FSourceBounds := ASourceBounds;
  if FSourceBounds.Left > FSourceBounds.Right then
  begin
    temp := FSourceBounds.Left;
    FSourceBounds.Left := FSourceBounds.Right;
    FSourceBounds.Right := temp;
  end;
  if FSourceBounds.Top > FSourceBounds.Bottom then
  begin
    temp := FSourceBounds.Top;
    FSourceBounds.Top := FSourceBounds.Bottom;
    FSourceBounds.Bottom := temp;
  end;
  FKernelWidth := AKernelWidth;
  FKernelHeight:= AKernelHeight;
  SetLength(FBuffers, FKernelHeight);
  SetLength(FPBuffers, FKernelHeight);
  FCurBufferYDefined := false;
  //it is not sure that direct read can be used, because if the destination
  //is equal to the source, the output will change the input buffers
  AllowDirectRead := false;
end;

procedure TBGRAFilterScannerMultipixel.ComputeFilter(ASource: IBGRAScanner; X,
  Y: Integer; ADest: PBGRAPixel; ACount: integer);
var
  yb,dy: Integer;
  temp: TBGRAPixelBuffer;
  p: PBGRAPixel;
begin
  if not FCurBufferYDefined or (Abs(Y-FCurBufferY)>=FKernelHeight) then
  begin
    FCurBufferY := y;
    FCurBufferYDefined := true;
    for yb := 0 to FKernelHeight-1 do
      LoadBuffer(ASource,FSourceBounds.Left,Y+yb,yb,SourceBounds.Right-FSourceBounds.Left);
  end else
  if Y < FCurBufferY then
  begin
    dy := FCurBufferY-y;
    for yb := FKernelHeight-1 downto dy do
    begin
      temp := FBuffers[yb];
      FBuffers[yb] := FBuffers[yb-dy];
      FBuffers[yb-dy] := temp;
      p := FPBuffers[yb];
      FPBuffers[yb] := FPBuffers[yb-dy];
      FPBuffers[yb-dy] := p;
    end;
    for yb := 0 to dy-1 do
      LoadBuffer(ASource,FSourceBounds.Left,Y+yb,yb,FSourceBounds.Right-FSourceBounds.Left);
    FCurBufferY := y;
  end else
  if Y > FCurBufferY then
  begin
    dy := y-FCurBufferY;
    for yb := 0 to FKernelHeight-1-dy do
    begin
      temp := FBuffers[yb];
      FBuffers[yb] := FBuffers[yb+dy];
      FBuffers[yb+dy] := temp;
      p := FPBuffers[yb];
      FPBuffers[yb] := FPBuffers[yb+dy];
      FPBuffers[yb+dy] := p;
    end;
    for yb := FKernelHeight-1-dy+1 to FKernelHeight-1 do
      LoadBuffer(ASource,FSourceBounds.Left,Y+yb,yb,FSourceBounds.Right-FSourceBounds.Left);
    FCurBufferY := y;
  end;
  DoComputeFilter(X-FSourceBounds.Left,FPBuffers,FSourceBounds.Right-FSourceBounds.Left,ADest,ACount);
end;

end.

