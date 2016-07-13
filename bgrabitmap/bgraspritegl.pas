unit BGRASpriteGL;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRAOpenGLType,
  BGRABitmapTypes;

type
  { TBGLCustomSprite }

  TBGLCustomSprite = class
  protected
    FHandle: Pointer;
    FTexture: IBGLTexture;
    FFrameLoopStart: integer;
    FFrameLoopEnd : integer;
    procedure SetFrameLoopEnd(AValue: integer);
    procedure SetFrameLoopStart(AValue: integer);
    function GetHorizontalAlign: TAlignment; virtual; abstract;
    function GetVerticalAlign: TTextLayout; virtual; abstract;
    procedure SetHorizontalAlign(AValue: TAlignment); virtual; abstract;
    procedure SetVerticalAlign(AValue: TTextLayout); virtual; abstract;
    function GetAlpha: Integer; virtual; abstract;
    function GetAngle: Single; virtual; abstract;
    function GetColor: TBGRAPixel; virtual; abstract;
    function GetActualFrame: Single; virtual; abstract;
    function GetFrame: Single;
    function GetH: Single; virtual; abstract;
    function GetLayer: Integer; virtual; abstract;
    function GetLocation: TPointF; virtual;
    function GetW: Single; virtual; abstract;
    function GetX: Single; virtual; abstract;
    function GetY: Single; virtual; abstract;
    function GetTexture: IBGLTexture; virtual;
    function GetHandle: Pointer; virtual;
    procedure SetAlpha(AValue: Integer); virtual; abstract;
    procedure SetAngle(AValue: Single); virtual; abstract;
    procedure SetColor(AValue: TBGRAPixel); virtual; abstract;
    procedure SetFrame(AValue: Single);
    procedure SetActualFrame(AValue: Single); virtual; abstract;
    procedure SetH(AValue: Single); virtual; abstract;
    procedure SetLayer(AValue: Integer); virtual; abstract;
    procedure SetLocation(AValue: TPointF); virtual;
    procedure SetW(AValue: Single); virtual; abstract;
    procedure SetX(AValue: Single); virtual; abstract;
    procedure SetY(AValue: Single); virtual; abstract;
    procedure CreateHandle({%H-}ATexture: IBGLTexture; {%H-}ALayer: Integer); virtual;
    procedure OnInit; virtual;
  public
    constructor Create(ATexture: IBGLTexture; ALayer: integer);
    destructor Destroy; override;
    procedure OnDraw; virtual;
    procedure OnTimer; virtual;
    procedure QueryDestroy; virtual; abstract;
    property Layer   : Integer read GetLayer write SetLayer;
    property Location: TPointF read GetLocation write SetLocation;
    property X       : Single read GetX write SetX;
    property Y       : Single read GetY write SetY;
    property W       : Single read GetW write SetW;
    property H       : Single read GetH write SetH;
    property Angle   : Single read GetAngle write SetAngle;
    property Frame   : Single read GetFrame write SetFrame;
    property FrameLoopStart : integer read FFrameLoopStart write SetFrameLoopStart;
    property FrameLoopEnd : integer read FFrameLoopEnd write SetFrameLoopEnd;
    property Alpha   : Integer read GetAlpha write SetAlpha;
    property Color   : TBGRAPixel read GetColor write SetColor;
    property HorizontalAlign: TAlignment read GetHorizontalAlign write SetHorizontalAlign;
    property VerticalAlign: TTextLayout read GetVerticalAlign write SetVerticalAlign;
    property Texture : IBGLTexture read GetTexture;
    property Handle  : Pointer read GetHandle;
  end;

  { TBGLDefaultSprite }

  TBGLDefaultSprite = class(TBGLCustomSprite)
  protected
    FColor  : TBGRAPixel;
    FLocation,FSize: TPointF;
    FAngle,FFrame  : single;
    FHorizontalAlign: TAlignment;
    FVerticalAlign: TTextLayout;
    FQueryDestroy: boolean;
    FLayer: integer;
    function GetHorizontalAlign: TAlignment; override;
    function GetVerticalAlign: TTextLayout; override;
    procedure SetHorizontalAlign(AValue: TAlignment); override;
    procedure SetVerticalAlign(AValue: TTextLayout); override;
    function GetAlpha: Integer; override;
    function GetAngle: Single; override;
    function GetColor: TBGRAPixel; override;
    function GetDestroy: Boolean;
    function GetActualFrame: Single; override;
    function GetH: Single; override;
    function GetLayer: Integer; override;
    function GetW: Single; override;
    function GetX: Single; override;
    function GetY: Single; override;
    procedure SetAlpha(AValue: Integer); override;
    procedure SetAngle(AValue: Single); override;
    procedure SetColor(AValue: TBGRAPixel); override;
    procedure SetDestroy(AValue: Boolean);
    procedure SetActualFrame(AValue: Single); override;
    procedure SetH(AValue: Single); override;
    procedure SetLayer(AValue: Integer); override;
    procedure SetW(AValue: Single); override;
    procedure SetX(AValue: Single); override;
    procedure SetY(AValue: Single); override;
    procedure CreateHandle({%H-}ATexture: IBGLTexture; {%H-}ALayer: Integer); override;
  public
    procedure QueryDestroy; override;
  end;

  { TBGLCustomSpriteEngine }

  TBGLCustomSpriteEngine = class
  protected
    function GetSprite(AIndex: integer): TBGLCustomSprite; virtual; abstract;
    function GetCount: integer; virtual; abstract;
  public
    procedure Add(ASprite: TBGLCustomSprite); virtual; abstract;
    procedure Remove(ASprite: TBGLCustomSprite); virtual; abstract;
    procedure OnDraw; virtual; abstract;
    procedure OnTimer; virtual; abstract;
    procedure Clear; virtual; abstract;
    procedure Delete(AIndex: integer); virtual; abstract;
    property Count: Integer read GetCount;
    property Sprite[AIndex: integer]: TBGLCustomSprite read GetSprite;
  end;

  { TBGLDefaultSpriteEngine }

  TBGLDefaultSpriteEngine = class(TBGLCustomSpriteEngine)
  protected
    FSpriteRemoved: TBGLCustomSprite;
    FSprites: array of TBGLDefaultSprite;
    FSpritesCount: integer;
    function GetSprite(AIndex: integer): TBGLCustomSprite; override;
    function GetCount: integer; override;
  public
    constructor Create;
    procedure Add(ASprite: TBGLCustomSprite); override;
    procedure Remove(ASprite: TBGLCustomSprite); override;
    procedure OnDraw; override;
    procedure OnTimer; override;
    procedure Clear; override;
    procedure Delete(AIndex: integer); override;
  end;

var
  BGLSpriteEngine  : TBGLCustomSpriteEngine;

implementation

{ TBGLDefaultSpriteEngine }

function TBGLDefaultSpriteEngine.GetSprite(AIndex: integer): TBGLCustomSprite;
begin
  if (AIndex < 0) or (Aindex >= Count) then
    raise ERangeError.Create('Index out of bounds');
  result := FSprites[AIndex];
end;

function TBGLDefaultSpriteEngine.GetCount: integer;
begin
  result := FSpritesCount;
end;

constructor TBGLDefaultSpriteEngine.Create;
begin
  FSpritesCount := 0;
end;

procedure TBGLDefaultSpriteEngine.Add(ASprite: TBGLCustomSprite);
var
  i: Integer;
begin
  if ASprite = nil then exit;
  if not (ASprite is TBGLDefaultSprite) then
    raise exception.Create('Invalid class');
  for i := 0 to Count-1 do
    if FSprites[i] = ASprite then exit;
  if Count = length(FSprites) then
    setlength(FSprites, length(FSprites)*2 + 1);
  FSprites[Count] := TBGLDefaultSprite(ASprite);
  Inc(FSpritesCount);
end;

procedure TBGLDefaultSpriteEngine.Remove(ASprite: TBGLCustomSprite);
var
  i: Integer;
begin
  if ASprite = FSpriteRemoved then exit;
  for i := 0 to Count-1 do
    if FSprites[i] = ASprite then
    begin
      Delete(i);
      exit;
    end;
end;

procedure TBGLDefaultSpriteEngine.OnDraw;
var i: integer;
begin
  for i := 0 to Count-1 do
    FSprites[i].OnDraw;
end;

procedure TBGLDefaultSpriteEngine.OnTimer;
var i,j,k: integer;
    temp: TBGLDefaultSprite;
begin
  for i := 0 to Count-1 do
    FSprites[i].OnTimer;
  for i := Count-1 downto 0 do
    if FSprites[i].FQueryDestroy then
      Delete(i);
  for i := 1 to Count-1 do
  begin
    j := i;
    while (j > 0) and (FSprites[j-1].Layer > FSprites[i].Layer) do dec(j);
    if j <> i then
      begin
        temp := FSprites[i];
        for k := i downto j+1 do
          FSprites[k] := FSprites[k-1];
        FSprites[j] := temp;
      end;
  end;
end;

procedure TBGLDefaultSpriteEngine.Clear;
var i: integer;
begin
  for i := 0 to Count-1 do
  begin
    FSpriteRemoved := FSprites[i];
    FSpriteRemoved.Free;
    FSpriteRemoved := nil;
  end;
  FSprites := nil;
  FSpritesCount := 0;
end;

procedure TBGLDefaultSpriteEngine.Delete(AIndex: integer);
var i: integer;
begin
  if (AIndex < 0) or (AIndex >= Count) then exit;
  FSpriteRemoved := FSprites[AIndex];
  for i := AIndex to Count-1 do
    FSprites[i] := FSprites[i+1];
  dec(FSpritesCount);
  if FSpritesCount <= length(FSprites) div 2 then
    setlength(FSprites,FSpritesCount);
  FSpriteRemoved.Free;
  FSpriteRemoved := nil;
end;

{ TBGLDefaultSprite }

function TBGLDefaultSprite.GetHorizontalAlign: TAlignment;
begin
  result := FHorizontalAlign;
end;

function TBGLDefaultSprite.GetVerticalAlign: TTextLayout;
begin
  result := FVerticalAlign;
end;

procedure TBGLDefaultSprite.SetHorizontalAlign(AValue: TAlignment);
begin
  FHorizontalAlign:= AValue;
end;

procedure TBGLDefaultSprite.SetVerticalAlign(AValue: TTextLayout);
begin
  FVerticalAlign := AValue;
end;

function TBGLDefaultSprite.GetAlpha: Integer;
begin
  result := FColor.alpha;
end;

function TBGLDefaultSprite.GetAngle: Single;
begin
  result := FAngle;
end;

function TBGLDefaultSprite.GetColor: TBGRAPixel;
begin
  result := FColor;
end;

function TBGLDefaultSprite.GetDestroy: Boolean;
begin
  result := FQueryDestroy;
end;

function TBGLDefaultSprite.GetActualFrame: Single;
begin
  result := FFrame;
end;

function TBGLDefaultSprite.GetH: Single;
begin
  result := FSize.Y;
end;

function TBGLDefaultSprite.GetLayer: Integer;
begin
  result := FLayer;
end;

function TBGLDefaultSprite.GetW: Single;
begin
  result := FSize.X;
end;

function TBGLDefaultSprite.GetX: Single;
begin
  result := FLocation.X;
end;

function TBGLDefaultSprite.GetY: Single;
begin
  result := FLocation.Y;
end;

procedure TBGLDefaultSprite.SetAlpha(AValue: Integer);
begin
  FColor.Alpha := AValue;
end;

procedure TBGLDefaultSprite.SetAngle(AValue: Single);
begin
  FAngle:= AValue;
end;

procedure TBGLDefaultSprite.SetColor(AValue: TBGRAPixel);
begin
  FColor := AValue;
end;

procedure TBGLDefaultSprite.SetDestroy(AValue: Boolean);
begin
  FQueryDestroy:= AValue;
end;

procedure TBGLDefaultSprite.SetActualFrame(AValue: Single);
begin
  FFrame:= AValue;
end;

procedure TBGLDefaultSprite.SetH(AValue: Single);
begin
  FSize.Y := AValue;
end;

procedure TBGLDefaultSprite.SetLayer(AValue: Integer);
begin
  FLayer:= AValue;
end;

procedure TBGLDefaultSprite.SetW(AValue: Single);
begin
  FSize.X := AValue;
end;

procedure TBGLDefaultSprite.SetX(AValue: Single);
begin
  FLocation.X := AValue;
end;

procedure TBGLDefaultSprite.SetY(AValue: Single);
begin
  FLocation.Y := AValue;
end;

procedure TBGLDefaultSprite.CreateHandle(ATexture: IBGLTexture; ALayer: Integer);
begin
  inherited CreateHandle(ATexture, ALayer);
  FQueryDestroy := false;
  FLayer:= ALayer;
end;

procedure TBGLDefaultSprite.QueryDestroy;
begin
  SetDestroy(True);
end;

{ TBGLCustomSprite }

function TBGLCustomSprite.GetTexture: IBGLTexture;
begin
  result := FTexture;
end;

function TBGLCustomSprite.GetHandle: Pointer;
begin
  result := FHandle;
end;

procedure TBGLCustomSprite.SetFrame(AValue: Single);
var loopLength: integer;
begin
  if (FrameLoopEnd <> 0) and (FrameLoopStart <> 0) then
    begin
      loopLength := FrameLoopEnd-FrameLoopStart+1;
      if AValue >= FrameLoopEnd+0.49 then
        begin
          if loopLength <= 1 then
            AValue := FrameLoopEnd+0.49
          else
          begin
            AValue -= Trunc((AValue-(FrameLoopStart-0.5))/loopLength)*loopLength;
            if AValue > FrameLoopEnd+0.49 then AValue := FrameLoopStart-0.49;
            if AValue < FrameLoopStart-0.49 then AValue := FrameLoopStart-0.49;
          end;
        end else
      if AValue < FrameLoopStart-0.49 then
        begin
          if loopLength <= 1 then
            AValue := FrameLoopStart-0.49
          else
          begin
            AValue += Trunc((FrameLoopEnd+0.5-AValue)/loopLength)*loopLength;
            if AValue > FrameLoopEnd+0.49 then AValue := FrameLoopEnd+0.49;
            if AValue < FrameLoopStart-0.49 then AValue := FrameLoopEnd+0.49;
          end;
        end;
    end;
  SetActualFrame(AValue);
end;

procedure TBGLCustomSprite.SetFrameLoopEnd(AValue: integer);
begin
  FFrameLoopEnd := AValue;
  if FFrameLoopEnd < FFrameLoopStart then
    FFrameLoopStart := FFrameLoopEnd;
end;

procedure TBGLCustomSprite.SetFrameLoopStart(AValue: integer);
begin
  FFrameLoopStart := AValue;
  if FFrameLoopStart > FFrameLoopEnd then
    FFrameLoopEnd := FFrameLoopStart;
end;

function TBGLCustomSprite.GetFrame: Single;
begin
  result := GetActualFrame;
end;

function TBGLCustomSprite.GetLocation: TPointF;
begin
  result := PointF(X,Y);
end;

procedure TBGLCustomSprite.SetLocation(AValue: TPointF);
begin
  X := AValue.X;
  Y := AValue.Y;
end;

procedure TBGLCustomSprite.CreateHandle(ATexture: IBGLTexture; ALayer: Integer);
begin
  FHandle := nil;
end;

procedure TBGLCustomSprite.OnInit;
begin
  //nothing
end;

constructor TBGLCustomSprite.Create(ATexture: IBGLTexture; ALayer: integer);
begin
  CreateHandle(ATexture,ALayer);
  FTexture := ATexture;
  Layer := ALayer;
  if ATexture = nil then
    begin
      W := 0;
      H := 0;
    end else
    begin
      W := ATexture.FrameWidth;
      H := ATexture.FrameHeight;
    end;
  HorizontalAlign := taLeftJustify;
  VerticalAlign:= tlTop;
  Color := BGRAWhite;
  FrameLoopStart := 1;
  FrameLoopEnd := 0;
  OnInit;
  BGLSpriteEngine.Add(self);
end;

destructor TBGLCustomSprite.Destroy;
begin
  if Assigned(BGLSpriteEngine) then
    BGLSpriteEngine.Remove(self);
  inherited Destroy;
end;

procedure TBGLCustomSprite.OnDraw;
var NumFrame: integer;
begin
  if Texture <> nil then
    begin
      NumFrame := Trunc(Frame+0.5);
      if Angle <> 0 then
        Texture.Frame[NumFrame].StretchDrawAngle(X,Y,W,H,Angle,HorizontalAlign,VerticalAlign, Color)
      else
        Texture.Frame[NumFrame].StretchDraw(X,Y,W,H,HorizontalAlign,VerticalAlign, Color)
    end;
end;

procedure TBGLCustomSprite.OnTimer;
begin
  //nothing by default
end;

end.

