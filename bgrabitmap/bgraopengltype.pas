unit BGRAOpenGLType;

{$mode objfpc}{$H+}

interface

uses
  BGRAGraphics, BGRABitmap, BGRABitmapTypes,
  FPimage, Classes, SysUtils, BGRATransform;

type
  TBGLTextureHandle = type Pointer;
  TOpenGLResampleFilter = (orfBox,orfLinear);

  { IBGLFont }

  IBGLFont = interface
    function GetClipped: boolean;
    function GetUseGradientColors: boolean;
    function GetHorizontalAlign: TAlignment;
    function GetJustify: boolean;
    function GetScale: single;
    function GetStepX: single;
    function GetVerticalAlign: TTextLayout;
    procedure SetClipped(AValue: boolean);
    procedure SetUseGradientColors(AValue: boolean);
    procedure SetHorizontalAlign(AValue: TAlignment);
    procedure SetJustify(AValue: boolean);
    procedure SetScale(AValue: single);
    procedure SetStepX(AValue: single);
    procedure SetVerticalAlign(AValue: TTextLayout);
    procedure TextOut(X, Y: Single; const Text : UTF8String); overload;
    procedure TextOut(X, Y: Single; const Text : UTF8String; AColor: TBGRAPixel); overload;
    procedure TextOut(X, Y: Single; const Text : UTF8String; AHorizAlign: TAlignment; AVertAlign: TTextLayout = tlTop); overload;
    procedure TextOut(X, Y: Single; const Text : UTF8String; AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    procedure TextRect(X, Y, Width, Height: Single; const Text : UTF8String); overload;
    procedure TextRect(X, Y, Width, Height: Single; const Text : UTF8String; AColor: TBGRAPixel); overload;
    procedure TextRect(X, Y, Width, Height: Single; const Text : UTF8String; AVertAlign: TTextLayout); overload;
    procedure TextRect(X, Y, Width, Height: Single; const Text : UTF8String; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    procedure TextRect(X, Y, Width, Height: Single; const Text : UTF8String; AHorizAlign: TAlignment; AVertAlign: TTextLayout = tlTop); overload;
    procedure TextRect(X, Y, Width, Height: Single; const Text : UTF8String; AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    function TextWidth(const Text: UTF8String): single;
    function TextHeight(const Text: UTF8String): single; overload;
    function TextHeight(const Text: UTF8String; AWidth: single): single; overload;
    procedure SetGradientColors(ATopLeft, ATopRight, ABottomRight, ABottomLeft: TBGRAPixel);

    property Scale: single read GetScale write SetScale;
    property StepX: single read GetStepX write SetStepX;
    property Justify: boolean read GetJustify write SetJustify;
    property Clipped: boolean read GetClipped write SetClipped;
    property HorizontalAlign: TAlignment read GetHorizontalAlign write SetHorizontalAlign;
    property VerticalAlign: TTextLayout read GetVerticalAlign write SetVerticalAlign;
    property GradientColors: boolean read GetUseGradientColors write SetUseGradientColors;
  end;

  { TBGLCustomFont }

  TBGLCustomFont = class(TInterfacedObject, IBGLFont)
  protected
    FScale, FStepX: single;
    FFlags: LongWord;
    FHorizontalAlign: TAlignment;
    FVerticalAlign: TTextLayout;
    FJustify: boolean;
    procedure Init; virtual;
    function LoadFromFile(AFilename: UTF8String): boolean; virtual; abstract;
    procedure FreeMemoryOnDestroy; virtual;

    function GetScale: single; virtual;
    function GetStepX: single; virtual;
    procedure SetScale(AValue: single); virtual;
    procedure SetStepX(AValue: single); virtual;

    function GetHorizontalAlign: TAlignment; virtual;
    function GetJustify: boolean; virtual;
    function GetVerticalAlign: TTextLayout; virtual;
    procedure SetHorizontalAlign(AValue: TAlignment); virtual;
    procedure SetJustify(AValue: boolean); virtual;
    procedure SetVerticalAlign(AValue: TTextLayout); virtual;

    function GetClipped: boolean; virtual; abstract;
    function GetUseGradientColors: boolean; virtual; abstract;
    procedure SetClipped(AValue: boolean); virtual; abstract;
    procedure SetUseGradientColors(AValue: boolean); virtual; abstract;

    procedure DoTextOut(X, Y: Single; const Text : UTF8String; AColor: TBGRAPixel); virtual; abstract;
    procedure DoTextRect(X, Y, Width, Height: Single; const Text : UTF8String; AColor: TBGRAPixel); virtual; abstract;
  public
    constructor Create(AFilename: UTF8String);
    procedure FreeMemory; virtual;
    destructor Destroy; override;
    procedure TextOut(X, Y: Single; const Text : UTF8String); overload;
    procedure TextOut(X, Y: Single; const Text : UTF8String; AColor: TBGRAPixel); overload;
    procedure TextOut(X, Y: Single; const Text : UTF8String; AHorizAlign: TAlignment; AVertAlign: TTextLayout = tlTop); overload;
    procedure TextOut(X, Y: Single; const Text : UTF8String; AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    procedure TextRect(X, Y, Width, Height: Single; const Text : UTF8String); overload;
    procedure TextRect(X, Y, Width, Height: Single; const Text : UTF8String; AColor: TBGRAPixel); overload;
    procedure TextRect(X, Y, Width, Height: Single; const Text : UTF8String; AVertAlign: TTextLayout); overload;
    procedure TextRect(X, Y, Width, Height: Single; const Text : UTF8String; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    procedure TextRect(X, Y, Width, Height: Single; const Text : UTF8String; AHorizAlign: TAlignment; AVertAlign: TTextLayout = tlTop); overload;
    procedure TextRect(X, Y, Width, Height: Single; const Text : UTF8String; AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    function TextWidth(const Text: UTF8String): single; virtual; abstract;
    function TextHeight(const Text: UTF8String): single; virtual; abstract; overload;
    function TextHeight(const Text: UTF8String; AWidth: single): single; virtual; abstract; overload;
    procedure SetGradientColors(ATopLeft, ATopRight, ABottomRight, ABottomLeft: TBGRAPixel); virtual; abstract;

    property Scale: single read GetScale write SetScale;
    property StepX: single read GetStepX write SetStepX;
    property Justify: boolean read GetJustify write SetJustify;
    property Clipped: boolean read GetClipped write SetClipped;
    property HorizontalAlign: TAlignment read GetHorizontalAlign write SetHorizontalAlign;
    property VerticalAlign: TTextLayout read GetVerticalAlign write SetVerticalAlign;
    property GradientColors: boolean read GetUseGradientColors write SetUseGradientColors;
  end;

  { IBGLTexture }

  IBGLTexture = interface
    function GetFlipX: IBGLTexture;
    function GetFlipY: IBGLTexture;
    function GetFrame(AIndex: integer): IBGLTexture;
    function GetFrameCount: integer;
    function GetFrameHeight: integer;
    function GetFrameWidth: integer;
    function GetHeight: integer;
    function GetImageCenter: TPointF;
    function GetMask: IBGLTexture;
    function GetOpenGLTexture: TBGLTextureHandle;
    function GetResampleFilter: TOpenGLResampleFilter;
    function GetWidth: integer;

    procedure SetFrameSize(x,y: integer);
    procedure SetImageCenter(const AValue: TPointF);
    procedure SetResampleFilter(AValue: TOpenGLResampleFilter);
    procedure Update(ARGBAData: PBGRAPixel; AllocatedWidth, AllocatedHeight, ActualWidth,ActualHeight: integer);
    procedure ToggleFlipX;
    procedure ToggleFlipY;
    procedure ToggleMask;
    procedure SetFrame(AIndex: integer);
    procedure FreeMemory;

    procedure Draw(x,y: single; AAlpha: byte = 255); overload;
    procedure Draw(x,y: single; AColor: TBGRAPixel); overload;
    procedure Draw(x,y: single; AHorizAlign: TAlignment; AVertAlign: TTextLayout = tlTop; AAlpha: byte = 255); overload;
    procedure Draw(x,y: single; AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    procedure StretchDraw(x,y,w,h: single; AAlpha: byte = 255); overload;
    procedure StretchDraw(x,y,w,h: single; AColor: TBGRAPixel); overload;
    procedure StretchDraw(x,y,w,h: single; AHorizAlign: TAlignment; AVertAlign: TTextLayout = tlTop; AAlpha: byte = 255); overload;
    procedure StretchDraw(x,y,w,h: single; AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    procedure DrawAngle(x,y,angleDeg: single; const imageCenter: TPointF; ARestoreOffsetAfterRotation: boolean; AAlpha: byte = 255); overload;
    procedure DrawAngle(x,y,angleDeg: single; const imageCenter: TPointF; ARestoreOffsetAfterRotation: boolean; AColor: TBGRAPixel); overload;
    procedure DrawAngle(x,y,angleDeg: single; AAlpha: byte = 255); overload;
    procedure DrawAngle(x,y,angleDeg: single; AColor: TBGRAPixel); overload;
    procedure DrawAngle(x,y,angleDeg: single; AHorizAlign: TAlignment; AVertAlign: TTextLayout = tlTop; AAlpha: byte = 255); overload;
    procedure DrawAngle(x,y,angleDeg: single; AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    procedure StretchDrawAngle(x,y,w,h,angleDeg: single; const imageCenter: TPointF; ARestoreOffsetAfterRotation: boolean; AAlpha: byte = 255); overload;
    procedure StretchDrawAngle(x,y,w,h,angleDeg: single; const imageCenter: TPointF; ARestoreOffsetAfterRotation: boolean; AColor: TBGRAPixel); overload;
    procedure StretchDrawAngle(x,y,w,h,angleDeg: single; AAlpha: byte = 255); overload;
    procedure StretchDrawAngle(x,y,w,h,angleDeg: single; AColor: TBGRAPixel); overload;
    procedure StretchDrawAngle(x,y,w,h,angleDeg: single; AHorizAlign: TAlignment; AVertAlign: TTextLayout = tlTop; AAlpha: byte = 255); overload;
    procedure StretchDrawAngle(x,y,w,h,angleDeg: single; AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    procedure DrawAffine(const Origin, HAxis, VAxis: TPointF; AAlpha: byte = 255); overload;
    procedure DrawAffine(const Origin, HAxis, VAxis: TPointF; AColor: TBGRAPixel); overload;
    procedure DrawAffine(x,y: single; const AMatrix: TAffineMatrix; AAlpha: byte = 255); overload;
    procedure DrawAffine(x,y: single; const AMatrix: TAffineMatrix; AColor: TBGRAPixel); overload;

    property Width: integer read GetWidth;
    property Height: integer read GetHeight;
    property FrameCount: integer read GetFrameCount;
    property Frame[AIndex: integer]: IBGLTexture read GetFrame;
    property FrameWidth: integer read GetFrameWidth;
    property FrameHeight: integer read GetFrameHeight;
    property FlipX: IBGLTexture read GetFlipX;
    property FlipY: IBGLTexture read GetFlipY;
    property Mask: IBGLTexture read GetMask;
    property Handle: TBGLTextureHandle read GetOpenGLTexture;
    property ImageCenter: TPointF read GetImageCenter write SetImageCenter;
    property ResampleFilter: TOpenGLResampleFilter read GetResampleFilter write SetResampleFilter;
  end;

  { TBGLCustomBitmap }

  TBGLCustomBitmap = class(TBGRABitmap)
  protected
    FActualWidth,FActualHeight,
    FAllocatedWidth,FAllocatedHeight: integer;
    FTextureInvalidated: boolean;
    FActualRect: TRect;
    FTexture: IBGLTexture;
    procedure Init; override;
    function GetTexture: IBGLTexture; virtual;
    function GetOpenGLMaxTexSize: integer; virtual; abstract;
    procedure NotifySizeTooBigForOpenGL; virtual;
    procedure NotifyOpenGLContextNotCreatedYet; virtual;
  public
    procedure InvalidateBitmap; override;
    procedure Fill(c: TBGRAPixel); override;
    procedure NoClip; override;
    destructor Destroy; override;
    function Resample(newWidth, newHeight: integer; mode: TResampleMode=rmFineResample): TBGRACustomBitmap; override;
    procedure ApplyGlobalOpacity(alpha: byte); override; overload;
    procedure ReplaceColor(before, after: TColor); override; overload;
    procedure ReplaceColor(before, after: TBGRAPixel); override; overload;
    procedure ReplaceTransparent(after: TBGRAPixel); override; overload;
    procedure SetClipRect(const AValue: TRect); override;
    procedure SetSize(AWidth, AHeight: integer); override;
    property Width: integer read FActualWidth;
    property Height: integer read FActualHeight;
    property AllocatedWidth: integer read FAllocatedWidth;
    property AllocatedHeight: integer read FAllocatedHeight;
    function MakeTextureAndFree: IBGLTexture;
    property Texture: IBGLTexture read GetTexture;
    property MaxTextureSize: integer read GetOpenGLMaxTexSize;
  end;

  { TBGLCustomTexture }

  TBGLCustomTexture = class(TInterfacedObject, IBGLTexture)
  private
    function GetFlipX: IBGLTexture;
    function GetFlipY: IBGLTexture;
    function GetFrame(AIndex: integer): IBGLTexture;
    function GetFrameCount: integer;
    function GetFrameHeight: integer;
    function GetFrameWidth: integer;
    function GetHeight: integer;
    function GetMask: IBGLTexture;
    function GetOpenGLTexture: TBGLTextureHandle;
    function GetWidth: integer;
    function GetImageCenter: TPointF;
    procedure SetImageCenter(const AValue: TPointF);
    function GetResampleFilter: TOpenGLResampleFilter;
    procedure SetResampleFilter(AValue: TOpenGLResampleFilter);
  protected
    FOpenGLTexture: TBGLTextureHandle;
    FOpenGLTextureOwned: boolean;
    FResampleFilter: TOpenGLResampleFilter;
    FWidth,FHeight: integer;
    FImageCenter: TPointF;
    FFrame: integer;
    FFrameWidth,FFrameHeight: integer;
    FIsMask: boolean;

    function GetOpenGLMaxTexSize: integer; virtual; abstract;
    function CreateOpenGLTexture(ARGBAData: PBGRAPixel; AAllocatedWidth, AAllocatedHeight, AActualWidth, AActualHeight: integer): TBGLTextureHandle; virtual; abstract;
    procedure UpdateOpenGLTexture(ATexture: TBGLTextureHandle; ARGBAData: PBGRAPixel; AAllocatedWidth, AAllocatedHeight, AActualWidth,AActualHeight: integer); virtual; abstract;
    procedure SetOpenGLTextureSize(ATexture: TBGLTextureHandle; AAllocatedWidth, AAllocatedHeight, AActualWidth, AActualHeight: integer); virtual; abstract;
    procedure ComputeOpenGLFramesCoord(ATexture: TBGLTextureHandle; FramesX: Integer=1; FramesY: Integer=1); virtual; abstract;
    function GetOpenGLFrameCount(ATexture: TBGLTextureHandle): integer; virtual; abstract;
    function GetEmptyTexture: TBGLTextureHandle; virtual; abstract;
    procedure FreeOpenGLTexture(ATexture: TBGLTextureHandle); virtual; abstract;
    procedure UpdateGLResampleFilter(ATexture: TBGLTextureHandle; AFilter: TOpenGLResampleFilter); virtual; abstract;

    procedure DoStretchDraw(x,y,w,h: single; AColor: TBGRAPixel); virtual; abstract;
    procedure DoStretchDrawAngle(x,y,w,h,angleDeg: single; rotationCenter: TPointF; AColor: TBGRAPixel); virtual; abstract;
    procedure DoDrawAffine(Origin, HAxis, VAxis: TPointF; AColor: TBGRAPixel); virtual; abstract;
    function NewEmpty: TBGLCustomTexture; virtual; abstract;
    function NewFromTexture(ATexture: TBGLTextureHandle; AWidth,AHeight: integer): TBGLCustomTexture; virtual; abstract;
    procedure NotifyInvalidFrameSize; virtual;
    procedure NotifyErrorLoadingFile({%H-}AFilename: string); virtual;

    procedure Init(ATexture: TBGLTextureHandle; AWidth,AHeight: integer; AOwned: boolean); virtual;
    function Duplicate: TBGLCustomTexture; virtual;
    procedure FreeMemoryOnDestroy; virtual;

    procedure InitEmpty;
    procedure InitFromData(ARGBAData: PBGRAPixel; AllocatedWidth,AllocatedHeight, ActualWidth,ActualHeight: integer);
    procedure InitFromStream(AStream: TStream);
  public
    destructor Destroy; override;
    constructor Create; overload;
    constructor Create(ATexture: TBGLTextureHandle; AWidth,AHeight: integer); overload;
    constructor Create(ARGBAData: PBGRAPixel; AllocatedWidth,AllocatedHeight, ActualWidth,ActualHeight: integer); overload;
    constructor Create(AFPImage: TFPCustomImage); overload;
    constructor Create(ABitmap: TBitmap); overload;
    constructor Create(AWidth, AHeight: integer; Color: TColor); overload;
    constructor Create(AWidth, AHeight: integer; Color: TBGRAPixel); overload;
    constructor Create(AFilenameUTF8: string); overload;
    constructor Create(AStream: TStream); overload;
    procedure ToggleFlipX; virtual; abstract;
    procedure ToggleFlipY; virtual; abstract;
    procedure ToggleMask; virtual;

    procedure SetFrameSize(x,y: integer);
    procedure Update(ARGBAData: PBGRAPixel; AllocatedWidth, AllocatedHeight, ActualWidth,ActualHeight: integer);
    procedure SetFrame(AIndex: integer);
    procedure FreeMemory;

    procedure Draw(x,y: single; AAlpha: byte = 255); overload;
    procedure Draw(x,y: single; AColor: TBGRAPixel); overload;
    procedure Draw(x,y: single; AHorizAlign: TAlignment; AVertAlign: TTextLayout = tlTop; AAlpha: byte = 255); overload;
    procedure Draw(x,y: single; AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    procedure StretchDraw(x,y,w,h: single; AAlpha: byte = 255); overload;
    procedure StretchDraw(x,y,w,h: single; AColor: TBGRAPixel); overload;
    procedure StretchDraw(x,y,w,h: single; AHorizAlign: TAlignment; AVertAlign: TTextLayout = tlTop; AAlpha: byte = 255); overload;
    procedure StretchDraw(x,y,w,h: single; AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    procedure DrawAngle(x,y,angleDeg: single; const imageCenter: TPointF; ARestoreOffsetAfterRotation: boolean; AAlpha: byte = 255); overload;
    procedure DrawAngle(x,y,angleDeg: single; const imageCenter: TPointF; ARestoreOffsetAfterRotation: boolean; AColor: TBGRAPixel); overload;
    procedure DrawAngle(x,y,angleDeg: single; AAlpha: byte = 255); overload;
    procedure DrawAngle(x,y,angleDeg: single; AColor: TBGRAPixel); overload;
    procedure DrawAngle(x,y,angleDeg: single; AHorizAlign: TAlignment; AVertAlign: TTextLayout = tlTop; AAlpha: byte = 255); overload;
    procedure DrawAngle(x,y,angleDeg: single; AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    procedure StretchDrawAngle(x,y,w,h,angleDeg: single; const imageCenter: TPointF; ARestoreOffsetAfterRotation: boolean; AAlpha: byte = 255); overload;
    procedure StretchDrawAngle(x,y,w,h,angleDeg: single; const imageCenter: TPointF; ARestoreOffsetAfterRotation: boolean; AColor: TBGRAPixel); overload;
    procedure StretchDrawAngle(x,y,w,h,angleDeg: single; AAlpha: byte = 255); overload;
    procedure StretchDrawAngle(x,y,w,h,angleDeg: single; AColor: TBGRAPixel); overload;
    procedure StretchDrawAngle(x,y,w,h,angleDeg: single; AHorizAlign: TAlignment; AVertAlign: TTextLayout = tlTop; AAlpha: byte = 255); overload;
    procedure StretchDrawAngle(x,y,w,h,angleDeg: single; AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel); overload;
    procedure DrawAffine(const Origin, HAxis, VAxis: TPointF; AAlpha: byte = 255); overload;
    procedure DrawAffine(const Origin, HAxis, VAxis: TPointF; AColor: TBGRAPixel); overload;
    procedure DrawAffine(x,y: single; const AMatrix: TAffineMatrix; AAlpha: byte = 255); overload;
    procedure DrawAffine(x,y: single; const AMatrix: TAffineMatrix; AColor: TBGRAPixel); overload;

    property Width: integer read GetWidth;
    property Height: integer read GetHeight;
    property FrameCount: integer read GetFrameCount;
    property Frame[AIndex: integer]: IBGLTexture read GetFrame;
    property FrameWidth: integer read GetFrameWidth;
    property FrameHeight: integer read GetFrameHeight;
    property FlipX: IBGLTexture read GetFlipX;
    property FlipY: IBGLTexture read GetFlipY;
    property Mask: IBGLTexture read GetMask;
    property Handle: TBGLTextureHandle read GetOpenGLTexture;
  end;

type
  TBGLBitmapAny = class of TBGLCustomBitmap;
  TBGLTextureAny = class of TBGLCustomTexture;
var
  BGLBitmapFactory : TBGLBitmapAny;
  BGLTextureFactory: TBGLTextureAny;

function GetPowerOfTwo( Value : Integer ) : Integer;

implementation

uses Types;

function GetPowerOfTwo( Value : Integer ) : Integer;
begin
  Result := Value - 1;
  Result := Result or ( Result shr 1 );
  Result := Result or ( Result shr 2 );
  Result := Result or ( Result shr 4 );
  Result := Result or ( Result shr 8 );
  Result := Result or ( Result shr 16 );
  Result := Result + 1;
end;

{ TBGLCustomTexture }

function TBGLCustomTexture.GetFlipX: IBGLTexture;
begin
  result := Duplicate;
  result.ToggleFlipX;
end;

function TBGLCustomTexture.GetFlipY: IBGLTexture;
begin
  result := Duplicate;
  result.ToggleFlipY;
end;

function TBGLCustomTexture.GetFrame(AIndex: integer): IBGLTexture;
var fc: integer;
begin
  fc := GetFrameCount;
  if fc <= 1 then
    result := self
  else
    begin
      if (AIndex < 1) or (AIndex > fc) then
        result := NewEmpty
      else
      begin
        result := Duplicate;
        result.SetFrame(AIndex);
      end;
    end;
end;

function TBGLCustomTexture.GetFrameCount: integer;
begin
  result := GetOpenGLFrameCount(FOpenGLTexture);
end;

function TBGLCustomTexture.GetFrameHeight: integer;
begin
  result := FFrameHeight;
end;

function TBGLCustomTexture.GetFrameWidth: integer;
begin
  result := FFrameWidth;
end;

function TBGLCustomTexture.GetHeight: integer;
begin
  result := FHeight;
end;

function TBGLCustomTexture.GetMask: IBGLTexture;
begin
  result := Duplicate;
  result.ToggleMask;
end;

function TBGLCustomTexture.GetOpenGLTexture: TBGLTextureHandle;
begin
  result := FOpenGLTexture;
end;

function TBGLCustomTexture.GetWidth: integer;
begin
  result := FWidth;
end;

function TBGLCustomTexture.GetImageCenter: TPointF;
begin
  result := FImageCenter;
end;

procedure TBGLCustomTexture.SetImageCenter(const AValue: TPointF);
begin
  FImageCenter := AValue;
end;

function TBGLCustomTexture.GetResampleFilter: TOpenGLResampleFilter;
begin
  result := FResampleFilter;
end;

procedure TBGLCustomTexture.SetResampleFilter(AValue: TOpenGLResampleFilter);
begin
  if AValue <> FResampleFilter then
  begin
    FResampleFilter:= AValue;
    UpdateGLResampleFilter(FOpenGLTexture, AValue);
  end;
end;

procedure TBGLCustomTexture.ToggleMask;
begin
  FIsMask := not FIsMask;
end;

procedure TBGLCustomTexture.Update(ARGBAData: PBGRAPixel; AllocatedWidth,
  AllocatedHeight, ActualWidth, ActualHeight: integer);
begin
  UpdateOpenGLTexture(FOpenGLTexture, ARGBAData, AllocatedWidth, AllocatedHeight, ActualWidth,ActualHeight);
  ComputeOpenGLFramesCoord(FOpenGLTexture, round(FWidth/FFrameWidth),round(FWidth/FFrameHeight));
  FWidth := ActualWidth;
  FHeight := ActualHeight;
  FImageCenter := PointF(FWidth*0.5,FHeight*0.5);
end;

procedure TBGLCustomTexture.SetFrame(AIndex: integer);
begin
  if (AIndex >= 1) and (AIndex <= GetFrameCount) then
    begin
      FFrame := AIndex;
      FWidth := FFrameWidth;
      FHeight:= FFrameHeight;
      FImageCenter := PointF(FWidth*0.5,FHeight*0.5);
    end;
end;

procedure TBGLCustomTexture.FreeMemory;
begin
  if FOpenGLTextureOwned then
  begin
    FreeOpenGLTexture(FOpenGLTexture);
    FOpenGLTexture := GetEmptyTexture;
    FOpenGLTextureOwned := false;
  end;
end;

procedure TBGLCustomTexture.NotifyInvalidFrameSize;
begin
  //
end;

procedure TBGLCustomTexture.NotifyErrorLoadingFile(AFilename: string);
begin
  //
end;

procedure TBGLCustomTexture.Init(ATexture: TBGLTextureHandle; AWidth,
  AHeight: integer; AOwned: boolean);
begin
  FOpenGLTexture:= ATexture;
  FWidth := AWidth;
  FHeight := AHeight;
  FImageCenter := PointF(FWidth*0.5,FHeight*0.5);
  FFrame:= 0;
  FFrameWidth := AWidth;
  FFrameHeight := AHeight;
  FIsMask:= false;
  FOpenGLTextureOwned := AOwned;
end;

function TBGLCustomTexture.Duplicate: TBGLCustomTexture;
begin
  result := NewFromTexture(FOpenGLTexture, FWidth, FHeight);
  result.FFrame := FFrame;
  result.FFrameWidth := FFrameWidth;
  result.FFrameHeight := FFrameHeight;
  result.FIsMask := FIsMask;
  result.FResampleFilter := FResampleFilter;
end;

procedure TBGLCustomTexture.FreeMemoryOnDestroy;
begin
  FreeMemory;
end;

procedure TBGLCustomTexture.InitEmpty;
begin
  Init(GetEmptyTexture,0,0,False);
end;

procedure TBGLCustomTexture.InitFromData(ARGBAData: PBGRAPixel;
  AllocatedWidth, AllocatedHeight, ActualWidth, ActualHeight: integer);
var tex: TBGLTextureHandle;
    MaxTexSize: integer;
begin
  MaxTexSize := GetOpenGLMaxTexSize;
  if ( AllocatedWidth > MaxTexSize ) or ( AllocatedHeight > MaxTexSize ) or
    (AllocatedWidth <= 0) or (AllocatedHeight <= 0) then
    InitEmpty
  else
  begin
    tex := CreateOpenGLTexture(ARGBAData,AllocatedWidth,AllocatedHeight,ActualWidth,ActualHeight);
    FResampleFilter := orfLinear;
    ComputeOpenGLFramesCoord(tex);
    Init(tex,ActualWidth,ActualHeight,True);
  end;
end;

procedure TBGLCustomTexture.InitFromStream(AStream: TStream);
var bmp: TBGLCustomBitmap;
begin
  bmp := nil;
  try
    bmp := BGLBitmapFactory.Create(AStream);
    InitFromData(bmp.Data, bmp.AllocatedWidth,bmp.AllocatedHeight, bmp.Width,bmp.Height);
  except
    InitEmpty;
  end;
  bmp.Free;
end;

destructor TBGLCustomTexture.Destroy;
begin
  FreeMemoryOnDestroy;
  inherited Destroy;
end;

constructor TBGLCustomTexture.Create;
begin
  InitEmpty;
end;

constructor TBGLCustomTexture.Create(ATexture: TBGLTextureHandle; AWidth,
  AHeight: integer);
begin
  Init(ATexture, AWidth,AHeight, False);
end;

constructor TBGLCustomTexture.Create(ARGBAData: PBGRAPixel; AllocatedWidth,
  AllocatedHeight, ActualWidth, ActualHeight: integer);
begin
  InitFromData(ARGBAData,AllocatedWidth,AllocatedHeight,ActualWidth,ActualHeight);
end;

constructor TBGLCustomTexture.Create(AFPImage: TFPCustomImage);
var bmp: TBGLCustomBitmap;
begin
  if (AFPImage is TBGRACustomBitmap) and
    (AFPImage.Width = GetPowerOfTwo(AFPImage.Width)) and
    (AFPImage.Height = GetPowerOfTwo(AFPImage.Height)) then
  begin
    with TBGRACustomBitmap(AFPImage) do
      InitFromData(Data, Width,Height, Width,Height);
  end else
  begin
    bmp := BGLBitmapFactory.Create(AFPImage);
    InitFromData(bmp.Data, bmp.AllocatedWidth,bmp.AllocatedHeight, bmp.Width,bmp.Height);
    bmp.Free;
  end;
end;

constructor TBGLCustomTexture.Create(ABitmap: TBitmap);
var bmp: TBGLCustomBitmap;
begin
  bmp := BGLBitmapFactory.Create(ABitmap);
  InitFromData(bmp.Data, bmp.AllocatedWidth,bmp.AllocatedHeight, bmp.Width,bmp.Height);
  bmp.Free;
end;

constructor TBGLCustomTexture.Create(AWidth, AHeight: integer; Color: TColor);
var bmp: TBGLCustomBitmap;
begin
  bmp := BGLBitmapFactory.Create(AWidth,AHeight,Color);
  InitFromData(bmp.Data, bmp.AllocatedWidth,bmp.AllocatedHeight, bmp.Width,bmp.Height);
  bmp.Free;
end;

constructor TBGLCustomTexture.Create(AWidth, AHeight: integer;
  Color: TBGRAPixel);
var bmp: TBGLCustomBitmap;
begin
  bmp := BGLBitmapFactory.Create(AWidth,AHeight,Color);
  InitFromData(bmp.Data, bmp.AllocatedWidth,bmp.AllocatedHeight, bmp.Width,bmp.Height);
  bmp.Free;
end;

constructor TBGLCustomTexture.Create(AFilenameUTF8: string);
var bmp: TBGLCustomBitmap;
begin
  bmp := nil;
  try
    bmp := BGLBitmapFactory.Create(AFilenameUTF8, True);
    InitFromData(bmp.Data, bmp.AllocatedWidth,bmp.AllocatedHeight, bmp.Width,bmp.Height);
  except
    InitEmpty;
    NotifyErrorLoadingFile(AFilenameUTF8);
  end;
  bmp.Free;
end;

constructor TBGLCustomTexture.Create(AStream: TStream);
begin
  InitFromStream(AStream);
end;

procedure TBGLCustomTexture.SetFrameSize(x, y: integer);
begin
  if (FWidth = 0) or (FHeight = 0) then exit;
  if (x <= 0) or (y <= 0) or (x > FWidth) or (y > FHeight) then
  begin
    NotifyInvalidFrameSize;
    exit;
  end;
  ComputeOpenGLFramesCoord(FOpenGLTexture, FWidth div x,FHeight div y);
  FFrameWidth:= x;
  FFrameHeight:= y;
end;

procedure TBGLCustomTexture.Draw(x, y: single; AAlpha: byte);
begin
  DoStretchDraw(x,y,FWidth,FHeight,BGRA(255,255,255,AAlpha));
end;

procedure TBGLCustomTexture.Draw(x, y: single; AColor: TBGRAPixel);
begin
  DoStretchDraw(x,y,FWidth,FHeight,AColor);
end;

procedure TBGLCustomTexture.Draw(x, y: single; AHorizAlign: TAlignment;
  AVertAlign: TTextLayout; AAlpha: byte);
begin
  Draw(x,y, AHorizAlign, AVertAlign, BGRA(255,255,255,AAlpha));
end;

procedure TBGLCustomTexture.Draw(x, y: single; AHorizAlign: TAlignment;
  AVertAlign: TTextLayout; AColor: TBGRAPixel);
begin
  StretchDraw(x,y, FWidth,FHeight, AHorizAlign,AVertAlign, AColor);
end;

procedure TBGLCustomTexture.StretchDraw(x, y, w, h: single; AAlpha: byte);
begin
  DoStretchDraw(x,y,w,h, BGRA(255,255,255,AAlpha));
end;

procedure TBGLCustomTexture.StretchDraw(x, y, w, h: single;
  AColor: TBGRAPixel);
begin
  DoStretchDraw(x,y,w,h,AColor);
end;

procedure TBGLCustomTexture.StretchDraw(x, y, w, h: single;
  AHorizAlign: TAlignment; AVertAlign: TTextLayout; AAlpha: byte);
begin
  StretchDraw(x,y,w,h, AHorizAlign,AVertAlign, BGRA(255,255,255,AAlpha));
end;

procedure TBGLCustomTexture.StretchDraw(x, y, w, h: single;
  AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel);
begin
  case AHorizAlign of
  taCenter: x -= w*0.5;
  taRightJustify: x -= w-1;
  end;
  case AVertAlign of
  tlCenter: y -= h*0.5;
  tlBottom: y -= h;
  end;
  DoStretchDraw(x,y,w,h,AColor);
end;

procedure TBGLCustomTexture.DrawAngle(x, y, angleDeg: single;
  const imageCenter: TPointF; ARestoreOffsetAfterRotation: boolean; AAlpha: byte);
begin
  StretchDrawAngle(x,y,FWidth,FHeight,angleDeg,imageCenter,ARestoreOffsetAfterRotation,BGRA(255,255,255,AAlpha));
end;

procedure TBGLCustomTexture.DrawAngle(x, y, angleDeg: single;
  const imageCenter: TPointF; ARestoreOffsetAfterRotation: boolean; AColor: TBGRAPixel);
begin
  StretchDrawAngle(x,y,FWidth,FHeight,angleDeg,imageCenter,ARestoreOffsetAfterRotation,AColor);
end;

procedure TBGLCustomTexture.DrawAngle(x, y, angleDeg: single; AAlpha: byte);
begin
  StretchDrawAngle(x,y, FWidth,FHeight, angleDeg, AAlpha);
end;

procedure TBGLCustomTexture.DrawAngle(x, y, angleDeg: single; AColor: TBGRAPixel);
begin
  StretchDrawAngle(x,y, FWidth,FHeight, angleDeg, AColor);
end;

procedure TBGLCustomTexture.DrawAngle(x, y, angleDeg: single;
  AHorizAlign: TAlignment; AVertAlign: TTextLayout; AAlpha: byte);
begin
  StretchDrawAngle(x,y,FWidth,FHeight,angleDeg, AHorizAlign, AVertAlign, AAlpha);
end;

procedure TBGLCustomTexture.DrawAngle(x, y, angleDeg: single;
  AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel);
begin
  StretchDrawAngle(x,y,FWidth,FHeight, angleDeg, AHorizAlign, AVertAlign, AColor);
end;

procedure TBGLCustomTexture.StretchDrawAngle(x, y,w,h, angleDeg: single;
  const imageCenter: TPointF; ARestoreOffsetAfterRotation: boolean; AAlpha: byte);
begin
  StretchDrawAngle(x,y,w,h,angleDeg,imageCenter,ARestoreOffsetAfterRotation,BGRA(255,255,255,AAlpha));
end;

procedure TBGLCustomTexture.StretchDrawAngle(x, y,w,h, angleDeg: single;
  const imageCenter: TPointF; ARestoreOffsetAfterRotation: boolean; AColor: TBGRAPixel);
var
  rotationCenter: TPointF;
begin
  if (FWidth=0) or (FHeight = 0) then exit;
  rotationCenter := PointF(imageCenter.x*w/FWidth, imageCenter.y*h/FHeight);
  if not ARestoreOffsetAfterRotation then
  begin
    x -= rotationCenter.x;
    y -= rotationCenter.y;
  end;
  DoStretchDrawAngle(x,y,w,h,angleDeg,rotationCenter+PointF(x,y),AColor);
end;

procedure TBGLCustomTexture.StretchDrawAngle(x, y,w,h, angleDeg: single; AAlpha: byte);
begin
  StretchDrawAngle(x, y, w,h, angleDeg, FImageCenter, True, BGRA(255,255,255,AAlpha));
end;

procedure TBGLCustomTexture.StretchDrawAngle(x, y,w,h, angleDeg: single;
  AColor: TBGRAPixel);
begin
  StretchDrawAngle(x, y, w,h, angleDeg, FImageCenter, True, AColor);
end;

procedure TBGLCustomTexture.StretchDrawAngle(x, y,w,h, angleDeg: single;
  AHorizAlign: TAlignment; AVertAlign: TTextLayout; AAlpha: byte);
begin
  StretchDrawAngle(x,y,w,h,angleDeg, AHorizAlign, AVertAlign, BGRA(255,255,255,AAlpha));
end;

procedure TBGLCustomTexture.StretchDrawAngle(x, y,w,h, angleDeg: single;
  AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel);
var imageCenter: TPointF;
begin
  case AHorizAlign of
  taCenter: imageCenter.x := FWidth*0.5;
  taRightJustify: imageCenter.x := FWidth;
  else imageCenter.x := 0;
  end;
  case AVertAlign of
  tlCenter: imageCenter.y := FHeight*0.5;
  tlBottom: imageCenter.y := FHeight;
  else imageCenter.y := 0;
  end;
  StretchDrawAngle(x,y,w,h, angleDeg, imageCenter, False, AColor);
end;

procedure TBGLCustomTexture.DrawAffine(const Origin, HAxis, VAxis: TPointF;
  AAlpha: byte);
begin
  DoDrawAffine(Origin,HAxis,VAxis, BGRA(255,255,255,AAlpha));
end;

procedure TBGLCustomTexture.DrawAffine(const Origin, HAxis, VAxis: TPointF;
  AColor: TBGRAPixel);
begin
  DoDrawAffine(Origin,HAxis,VAxis, AColor);
end;

procedure TBGLCustomTexture.DrawAffine(x, y: single;
  const AMatrix: TAffineMatrix; AAlpha: byte);
begin
  DoDrawAffine(AMatrix*PointF(0,0) + PointF(x,y), AMatrix*PointF(Width,0) + PointF(x,y),
     AMatrix*PointF(0,Height) + PointF(x,y), BGRA(255,255,255,AAlpha));
end;

procedure TBGLCustomTexture.DrawAffine(x, y: single;
  const AMatrix: TAffineMatrix; AColor: TBGRAPixel);
begin
  DoDrawAffine(AMatrix*PointF(0,0) + PointF(x,y), AMatrix*PointF(Width,0) + PointF(x,y),
     AMatrix*PointF(0,Height) + PointF(x,y), AColor);
end;

{ TBGLCustomFont }

function TBGLCustomFont.GetScale: single;
begin
  result := FScale;
end;

function TBGLCustomFont.GetStepX: single;
begin
  result := FStepX;
end;

procedure TBGLCustomFont.SetScale(AValue: single);
begin
  FScale:= AValue;
end;

procedure TBGLCustomFont.SetStepX(AValue: single);
begin
  FStepX:= AValue;
end;

function TBGLCustomFont.GetHorizontalAlign: TAlignment;
begin
  result := FHorizontalAlign;
end;

function TBGLCustomFont.GetJustify: boolean;
begin
  result := FJustify;
end;

function TBGLCustomFont.GetVerticalAlign: TTextLayout;
begin
  result := FVerticalAlign;
end;

procedure TBGLCustomFont.SetHorizontalAlign(AValue: TAlignment);
begin
  FHorizontalAlign:= AValue;
end;

procedure TBGLCustomFont.SetJustify(AValue: boolean);
begin
  FJustify:= AValue;
end;

procedure TBGLCustomFont.SetVerticalAlign(AValue: TTextLayout);
begin
  FVerticalAlign := AValue;
end;

procedure TBGLCustomFont.Init;
begin
  FScale:= 1;
  FStepX:= 0;
  FHorizontalAlign:= taLeftJustify;
  FVerticalAlign:= tlTop;
  FJustify:= false;
end;

procedure TBGLCustomFont.FreeMemoryOnDestroy;
begin
  FreeMemory;
end;

procedure TBGLCustomFont.FreeMemory;
begin

end;

constructor TBGLCustomFont.Create(AFilename: UTF8String);
begin
  Init;
  LoadFromFile(AFilename);
end;

destructor TBGLCustomFont.Destroy;
begin
  FreeMemoryOnDestroy;
  inherited Destroy;
end;

procedure TBGLCustomFont.TextOut(X, Y: Single; const Text: UTF8String);
begin
  DoTextOut(X,Y,Text,BGRAWhite);
end;

procedure TBGLCustomFont.TextOut(X, Y: Single; const Text: UTF8String;
  AColor: TBGRAPixel);
begin
  DoTextOut(X,Y,Text,AColor);
end;

procedure TBGLCustomFont.TextOut(X, Y: Single; const Text: UTF8String;
  AHorizAlign: TAlignment; AVertAlign: TTextLayout);
begin
  TextOut(X,Y,Text,AHorizAlign,AVertAlign,BGRAWhite);
end;

procedure TBGLCustomFont.TextOut(X, Y: Single; const Text: UTF8String;
  AHorizAlign: TAlignment; AVertAlign: TTextLayout; AColor: TBGRAPixel);
var PrevHorizAlign: TAlignment;
    PrevVertAlign: TTextLayout;
begin
  PrevHorizAlign:= GetHorizontalAlign;
  PrevVertAlign:= GetVerticalAlign;
  SetHorizontalAlign(AHorizAlign);
  SetVerticalAlign(AVertAlign);
  DoTextOut(X,Y,Text,AColor);
  SetHorizontalAlign(PrevHorizAlign);
  SetVerticalAlign(PrevVertAlign);
end;

procedure TBGLCustomFont.TextRect(X, Y, Width, Height: Single;
  const Text: UTF8String);
begin
  DoTextRect(X,Y,Width,Height,Text,BGRAWhite);
end;

procedure TBGLCustomFont.TextRect(X, Y, Width, Height: Single;
  const Text: UTF8String; AColor: TBGRAPixel);
begin
  DoTextRect(X,Y,Width,Height,Text,AColor);
end;

procedure TBGLCustomFont.TextRect(X, Y, Width, Height: Single;
  const Text: UTF8String; AVertAlign: TTextLayout);
begin
  TextRect(X,Y,Width,Height,Text,taLeftJustify,AVertAlign,BGRAWhite);
end;

procedure TBGLCustomFont.TextRect(X, Y, Width, Height: Single;
  const Text: UTF8String; AVertAlign: TTextLayout; AColor: TBGRAPixel);
begin
  TextRect(X,Y,Width,Height,Text,taLeftJustify,AVertAlign,AColor);
end;

procedure TBGLCustomFont.TextRect(X, Y, Width, Height: Single;
  const Text: UTF8String; AHorizAlign: TAlignment; AVertAlign: TTextLayout);
begin
  TextRect(X,Y,Width,Height,Text,AHorizAlign,AVertAlign,BGRAWhite);
end;

procedure TBGLCustomFont.TextRect(X, Y, Width, Height: Single;
  const Text: UTF8String; AHorizAlign: TAlignment; AVertAlign: TTextLayout;
  AColor: TBGRAPixel);
var PrevHorizAlign: TAlignment;
    PrevVertAlign: TTextLayout;
begin
  PrevHorizAlign:= GetHorizontalAlign;
  PrevVertAlign:= GetVerticalAlign;
  SetHorizontalAlign(AHorizAlign);
  SetVerticalAlign(AVertAlign);
  DoTextRect(X,Y,Width,Height,Text,AColor);
  SetHorizontalAlign(PrevHorizAlign);
  SetVerticalAlign(PrevVertAlign);
end;

{ TBGLCustomBitmap }

procedure TBGLCustomBitmap.Init;
begin
  inherited Init;
  FTextureInvalidated := true;
  FActualRect := rect(0,0,0,0);
  FScanWidth := 0;
  FScanHeight:= 0;
  FTexture := nil;
  FLineOrder := riloTopToBottom;
end;

function TBGLCustomBitmap.GetTexture: IBGLTexture;
begin
  if (Width = 0) or (Height = 0) then
    result := BGLTextureFactory.Create
  else
  begin
    if FTextureInvalidated then
    begin
      FTextureInvalidated := false;
      if FTexture = nil then
        FTexture := BGLTextureFactory.Create(self.Data, AllocatedWidth,AllocatedHeight, Width,Height)
      else
        FTexture.Update(self.Data, AllocatedWidth,AllocatedHeight, Width,Height);
    end;
    result := FTexture;
  end;
end;

procedure TBGLCustomBitmap.NotifySizeTooBigForOpenGL;
begin
  raise exception.Create('Size too big for OpenGL');
end;

procedure TBGLCustomBitmap.NotifyOpenGLContextNotCreatedYet;
begin
  raise exception.Create('OpenGL context has not been created yet');
end;

procedure TBGLCustomBitmap.InvalidateBitmap;
begin
  inherited InvalidateBitmap;
  FTextureInvalidated := true;
end;

procedure TBGLCustomBitmap.Fill(c: TBGRAPixel);
var oldClip: TRect;
begin
  oldClip := ClipRect;
  NoClip;
  FillRect(ClipRect, c, dmSet);
  ClipRect := oldClip;
end;

procedure TBGLCustomBitmap.NoClip;
begin
  ClipRect := FActualRect;
end;

destructor TBGLCustomBitmap.Destroy;
begin
  if FTexture <> nil then
  begin
    //always free the memory of the texture
    FTexture.FreeMemory;
    FTexture := nil;
  end;
  inherited Destroy;
end;

function TBGLCustomBitmap.Resample(newWidth, newHeight: integer;
  mode: TResampleMode): TBGRACustomBitmap;
var temp,resampled: TBGRACustomBitmap;
begin
  temp := (inherited GetPart(FActualRect)) as TBGRACustomBitmap;
  temp.ResampleFilter := ResampleFilter;
  resampled := temp.Resample(NewWidth,NewHeight,mode);
  temp.Free;
  Result:= NewBitmap(resampled);
  resampled.Free;
end;

procedure TBGLCustomBitmap.ApplyGlobalOpacity(alpha: byte);
var oldClip: TRect;
begin
  oldClip := ClipRect;
  NoClip;
  ApplyGlobalOpacity(FActualRect,alpha);
  ClipRect := oldClip;
end;

procedure TBGLCustomBitmap.ReplaceColor(before, after: TColor);
var oldClip: TRect;
begin
  oldClip := ClipRect;
  NoClip;
  ReplaceColor(FActualRect, before, after);
  ClipRect := oldClip;
end;

procedure TBGLCustomBitmap.ReplaceColor(before, after: TBGRAPixel);
var oldClip: TRect;
begin
  oldClip := ClipRect;
  NoClip;
  ReplaceColor(FActualRect, before, after);
  ClipRect := oldClip;
end;

procedure TBGLCustomBitmap.ReplaceTransparent(after: TBGRAPixel);
var oldClip: TRect;
begin
  oldClip := ClipRect;
  NoClip;
  ReplaceTransparent(FActualRect,after);
  ClipRect := oldClip;
end;

procedure TBGLCustomBitmap.SetClipRect(const AValue: TRect);
var r: TRect;
begin
  r := AValue;
  IntersectRect(r, r,FActualRect);
  inherited SetClipRect(r);
end;

procedure TBGLCustomBitmap.SetSize(AWidth, AHeight: integer);
var AllocatedWidthNeeded,AllocatedHeightNeeded,
    MaxTexSize: Integer;
begin
  if AWidth < 0 then AWidth := 0;
  if AHeight < 0 then AHeight := 0;
  if (AWidth = Width) and (AHeight = Height) then exit;
  AllocatedWidthNeeded := GetPowerOfTwo(AWidth);
  AllocatedHeightNeeded := GetPowerOfTwo(AHeight);
  MaxTexSize := GetOpenGLMaxTexSize;
  if (AllocatedWidthNeeded > MaxTexSize) or
     (AllocatedHeightNeeded > MaxTexSize) then
  begin
    if MaxTexSize = 0 then
      NotifyOpenGLContextNotCreatedYet
    else
      NotifySizeTooBigForOpenGL;
    if AllocatedWidthNeeded > MaxTexSize then
    begin
      AllocatedWidthNeeded := MaxTexSize;
      AWidth := MaxTexSize;
    end;
    if AllocatedHeightNeeded > MaxTexSize then
    begin
      AllocatedHeightNeeded := MaxTexSize;
      AHeight := MaxTexSize;
    end;
  end;
  FActualWidth := AWidth;
  FActualHeight := AHeight;
  FAllocatedWidth := AllocatedWidthNeeded;
  FAllocatedHeight := AllocatedHeightNeeded;
  FActualRect := rect(0,0,FActualWidth,FActualHeight);
  if (FAllocatedWidth <> inherited Width) or
     (FAllocatedHeight <> inherited Height) then
    inherited SetSize(FAllocatedWidth, FAllocatedHeight);
  inherited NoClip;
  inherited FillRect(Width,0,FAllocatedWidth,Height, BGRAPixelTransparent, dmSet);
  inherited FillRect(0,Height,FAllocatedWidth,FAllocatedHeight, BGRAPixelTransparent, dmSet);
  NoClip;
  FScanWidth := Width;
  FScanHeight:= Height;
  FTextureInvalidated:= true;
end;

function TBGLCustomBitmap.MakeTextureAndFree: IBGLTexture;
begin
  result := Texture;
  FTexture := nil;
  Free;
end;

end.

