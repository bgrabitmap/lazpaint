{$IFDEF INCLUDE_USES}
{$UNDEF INCLUDE_USES}
,FPImgCanv
{$ENDIF}

{$IFDEF INCLUDE_INTERFACE}
{$UNDEF INCLUDE_INTERFACE}
type
  TColor = type LongInt;
  {$warnings off}
  TRawImage = class(TFPMemoryImage)
    procedure BGRASetSizeAndTransparency(AWidth,AHeight: Integer; {%H-}ATransparent: boolean);
    constructor Create;
  end;
  {$warnings on}
  TGUICanvas = class(TFPImageCanvas)
    procedure DrawImage(x,y: integer; AImage: TFPCustomImage);
  end;

const
  clNone    = TColor($1FFFFFFF);
  clDefault = TColor($20000000);
  clBlack   = TColor($000000);
  clMaroon  = TColor($000080);
  clGreen   = TColor($008000);
  clOlive   = TColor($008080);
  clNavy    = TColor($800000);
  clPurple  = TColor($800080);
  clTeal    = TColor($808000);
  clGray    = TColor($808080);
  clSilver  = TColor($C0C0C0);
  clRed     = TColor($0000FF);
  clLime    = TColor($00FF00);
  clYellow  = TColor($00FFFF);
  clBlue    = TColor($FF0000);
  clFuchsia = TColor($FF00FF);
  clAqua    = TColor($FFFF00);
  clLtGray  = TColor($C0C0C0); // clSilver alias
  clDkGray  = TColor($808080); // clGray alias
  clWhite   = TColor($FFFFFF);

  clRgbBtnHighlight = TColor($E0E0E0);
  clRgbBtnShadow = TColor($808080);

function ColorToRGB(c: TColor): TColor; inline;
function RGBToColor(R, G, B: Byte): TColor;
procedure RedGreenBlue(rgb: TColor; out Red, Green, Blue: Byte); // does not work on system color

function GetScreenDPIX: integer;
function GetScreenDPIY: integer;
{$ENDIF}

{$IFDEF INCLUDE_IMPLEMENTATION}
{$UNDEF INCLUDE_IMPLEMENTATION}
procedure TRawImage.BGRASetSizeAndTransparency(AWidth,AHeight: Integer; ATransparent: boolean);
begin
  SetSize(AWidth,AHeight);
end;

constructor TRawImage.Create;
begin
  inherited Create(0,0);
end;

procedure TGUICanvas.DrawImage(x,y: integer; AImage: TFPCustomImage);
begin
  Draw(x,y, AImage);
end;

function ColorToRGB(c: TColor): TColor; inline;
begin
  result := c;
end;

function RGBToColor(R, G, B: Byte): TColor;
begin
  Result := (B shl 16) or (G shl 8) or R;
end;

procedure RedGreenBlue(rgb: TColor; out Red, Green, Blue: Byte);
begin
  Red := rgb and $000000ff;
  Green := (rgb shr 8) and $000000ff;
  Blue := (rgb shr 16) and $000000ff;
end;

function GetScreenDPIX: integer;
begin
  result := 96;
end;

function GetScreenDPIY: integer;
begin
  result := 96;
end;

{$ENDIF}
