{$IFDEF INCLUDE_USES}
{$UNDEF INCLUDE_USES}
,fpg_base, fpg_main
{$ENDIF}

{$IFDEF INCLUDE_INTERFACE}
{$UNDEF INCLUDE_INTERFACE}
type
  TColor = TfpgColor;
  TRawImage = class(TfpgImage)
    procedure BGRASetSizeAndTransparency(AWidth,AHeight: Integer; ATransparent: boolean);  
  end;
  TGUICanvas = TfpgCanvas;

const
  clNone = fpg_base.clNone;
  clBlack = fpg_base.clBlack;
  clWhite = fpg_base.clWhite;

function clRgbBtnHighlight: TColor;
function clRgbBtnShadow: TColor;
function ColorToRGB(c: TColor): TColor; inline;
function RGBToColor(R, G, B: Byte): TColor;
procedure RedGreenBlue(rgb: TColor; out Red, Green, Blue: Byte); // does not work on system color
function GetScreenDPIX: integer;
function GetScreenDPIY: integer;
{$ENDIF}

{$IFDEF INCLUDE_IMPLEMENTATION}
{$UNDEF INCLUDE_IMPLEMENTATION}

procedure TRawImage.BGRASetSizeAndTransparency(AWidth,AHeight: Integer; ATransparent: boolean);  
var
  tempData: pointer;
begin
  if (Width <> AWidth) or (Height <> AHeight) then
  begin
    AllocateImage(32,AWidth,AHeight);
    if ATransparent then AllocateMask;
  end else
  begin
    if ATransparent and not Masked then AllocateMask else
    if not ATransparent and Masked then
    begin
      getmem(tempData, ImageDataSize);
      if tempData <> nil then
      begin
        move(ImageData^, tempData^, ImageDataSize);
        FreeImage;
        AllocateImage(32,AWidth,AHeight);
        move(tempData^, ImageData^, ImageDataSize);
        freemem(tempData);
      end;
    end;
  end;
end;

function clRgbBtnHighlight: TColor;
begin
  result := fpgColorToRGB(fpg_base.clHilite2);
end;

function clRgbBtnShadow: TColor;
begin
  result := fpgColorToRGB(fpg_base.clShadow2);
end;

function ColorToRGB(c: TColor): TColor; inline;
begin
  result := fpgColorToRGB(c);
end;

function RGBToColor(R, G, B: Byte): TColor;
begin
  Result := (R shl 16) or (G shl 8) or B;
end;

procedure RedGreenBlue(rgb: TColor; out Red, Green, Blue: Byte);
begin
  Blue := rgb and $000000ff;
  Green := (rgb shr 8) and $000000ff;
  Red := (rgb shr 16) and $000000ff;
end;

function GetScreenDPIX: integer;
begin
  result := fpgApplication.Screen_dpi_x;
end;

function GetScreenDPIY: integer;
begin
  result := fpgApplication.Screen_dpi_y;
end;

{$ENDIF}
