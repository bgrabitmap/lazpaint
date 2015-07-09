unit BGRAPolygonAliased;

{$mode objfpc}{$H+}

{$i bgrasse.inc}

interface

{ This unit provides fast aliased polygon routines.

  To do aliased drawing, only one line is intersected with polygons for each output scanline.
  Along with intersection coordinates, color and texture coordinates are computed using
  linear interpolation. Inverse values are used for projective transform. }

uses
  Classes, SysUtils, BGRABitmapTypes, BGRAFillInfo, BGRASSE;

type
  //segment information for linear color
  TLinearColorInfo = record
    Color, ColorSlopes: TColorF;
  end;
  PLinearColorInfo = ^TLinearColorInfo;
  ArrayOfTColorF = array of TColorF;

  //add a color information to intersection info
  TLinearColorGradientIntersectionInfo = class(TIntersectionInfo)
    Color: TColorF;
  end;

  { TPolygonLinearColorGradientInfo }

  TPolygonLinearColorGradientInfo = class(TOnePassFillPolyInfo)
  protected
    FColors: array of TColorF;
    procedure SetIntersectionValues(AInter: TIntersectionInfo; AInterX: Single; AWinding,
      ANumSegment: integer; dy: single; AData: pointer); override;
  public
    constructor Create(const points: array of TPointF; const Colors: array of TBGRAPixel);
    function CreateSegmentData(numPt,nextPt: integer; x,y: single): pointer; override;
    function CreateIntersectionInfo: TIntersectionInfo; override;
  end;

procedure PolygonLinearColorGradientAliased(bmp: TBGRACustomBitmap; polyInfo: TPolygonLinearColorGradientInfo;
  NonZeroWinding: boolean); overload;
procedure PolygonLinearColorGradientAliased(bmp: TBGRACustomBitmap; const points: array of TPointF;
  const Colors: array of TBGRAPixel; NonZeroWinding: boolean); overload;

type
  //segment information for linear color
  TPerspectiveColorInfo = record
    ColorDivZ, ColorSlopesDivZ: TColorF;
    InvZ, InvZSlope: single;
  end;
  PPerspectiveColorInfo = ^TPerspectiveColorInfo;

  //add a color information to intersection info
  TPerspectiveColorGradientIntersectionInfo = class(TIntersectionInfo)
    ColorDivZ: TColorF;
    coordInvZ: single;
  end;

  { TPolygonPerspectiveColorGradientInfo }

  TPolygonPerspectiveColorGradientInfo = class(TOnePassFillPolyInfo)
  protected
    FColors: array of TColorF;
    FPointsZ: array of single;
    procedure SetIntersectionValues(AInter: TIntersectionInfo; AInterX: Single; AWinding,
      ANumSegment: integer; dy: single; AData: pointer); override;
  public
    constructor Create(const points: array of TPointF; const pointsZ: array of single; const Colors: array of TBGRAPixel);
    function CreateSegmentData(numPt,nextPt: integer; x,y: single): pointer; override;
    function CreateIntersectionInfo: TIntersectionInfo; override;
  end;

procedure PolygonPerspectiveColorGradientAliased(bmp: TBGRACustomBitmap; polyInfo: TPolygonPerspectiveColorGradientInfo;
  NonZeroWinding: boolean; zbuffer: psingle = nil); overload;
procedure PolygonPerspectiveColorGradientAliased(bmp: TBGRACustomBitmap; const points: array of TPointF;
  const pointsZ: array of single; const Colors: array of TBGRAPixel; NonZeroWinding: boolean; zbuffer: psingle = nil); overload;

type
  //segment information for linear texture
  TLinearTextureInfo = record
    TexCoord: TPointF;
    TexCoordSlopes: TPointF;
    lightness: single;
    lightnessSlope: single;
  end;
  PLinearTextureInfo = ^TLinearTextureInfo;

  //add a texture coordinate to intersection info
  TLinearTextureMappingIntersectionInfo = class(TIntersectionInfo)
    texCoord: TPointF;
    lightness: word;
  end;

  { TPolygonLinearTextureMappingInfo }

  TPolygonLinearTextureMappingInfo = class(TOnePassFillPolyInfo)
  protected
    FTexCoords: array of TPointF;
    FLightnesses: array of Word;
    procedure SetIntersectionValues(AInter: TIntersectionInfo; AInterX: Single; AWinding,
      ANumSegment: integer; dy: single; AData: pointer); override;
  public
    constructor Create(const points: array of TPointF; const texCoords: array of TPointF);
    constructor Create(const points: array of TPointF; const texCoords: array of TPointF; const lightnesses: array of word);
    function CreateSegmentData(numPt,nextPt: integer; x,y: single): pointer; override;
    function CreateIntersectionInfo: TIntersectionInfo; override;
  end;

procedure PolygonLinearTextureMappingAliased(bmp: TBGRACustomBitmap; polyInfo: TPolygonLinearTextureMappingInfo;
  texture: IBGRAScanner; TextureInterpolation: Boolean; NonZeroWinding: boolean); overload;

procedure PolygonLinearTextureMappingAliased(bmp: TBGRACustomBitmap; const points: array of TPointF; texture: IBGRAScanner;
  const texCoords: array of TPointF; TextureInterpolation: Boolean; NonZeroWinding: boolean); overload;
procedure PolygonLinearTextureMappingAliasedWithLightness(bmp: TBGRACustomBitmap; const points: array of TPointF; texture: IBGRAScanner;
  const texCoords: array of TPointF; TextureInterpolation: Boolean; lightnesses: array of word; NonZeroWinding: boolean); overload;

type
  //segment information for perspective texture. Use inverse Z and slopes.
  TPerspectiveTextureInfo = record
    InvZ,InvZSlope: Single;
    TexCoordDivByZ: TPointF;
    TexCoordDivByZSlopes: TPointF;
    lightness: single;
    lightnessSlope: single;
    Position3D, Normal3D: TPoint3D_128;
    Position3DSlope, Normal3DSlope: TPoint3D_128;
  end;
  PPerspectiveTextureInfo = ^TPerspectiveTextureInfo;

  //add a texture coordinate and depth to intersection info (stored as inverse)
  TPerspectiveTextureMappingIntersectionInfo = class(TIntersectionInfo)
    texCoordDivByZ: TPointF;
    coordInvZ: single;
    lightness: word;
    Position3D, Normal3D: TPoint3D_128;
  end;

  { TPolygonPerspectiveTextureMappingInfo }

  TPolygonPerspectiveTextureMappingInfo = class(TOnePassFillPolyInfo)
  protected
    FTexCoords: array of TPointF;
    FPointsZ: array of single;
    FLightnesses: array of Word;
    procedure SetIntersectionValues(AInter: TIntersectionInfo; AInterX: Single; AWinding,
      ANumSegment: integer; dy: single; AData: pointer); override;
  public
    constructor Create(const points: array of TPointF; const pointsZ: array of single; const texCoords: array of TPointF);
    constructor Create(const points: array of TPointF; const pointsZ: array of single; const texCoords: array of TPointF; const lightnesses: array of word);
    function CreateSegmentData(numPt,nextPt: integer; x,y: single): pointer; override;
    function CreateIntersectionInfo: TIntersectionInfo; override;
  end;

  { TPolygonPerspectiveMappingShaderInfo }

  TPolygonPerspectiveMappingShaderInfo = class(TOnePassFillPolyInfo)
  protected
    FTexCoords: array of TPointF;
    FPositions3D, FNormals3D: array of TPoint3D_128;
    procedure SetIntersectionValues(AInter: TIntersectionInfo; AInterX: Single; AWinding,
      ANumSegment: integer; dy: single; AData: pointer); override;
  public
    constructor Create(const points: array of TPointF; const points3D: array of TPoint3D; const normals: array of TPoint3D; const texCoords: array of TPointF);
    constructor Create(const points: array of TPointF; const points3D: array of TPoint3D_128; const normals: array of TPoint3D_128; const texCoords: array of TPointF);
    function CreateSegmentData(numPt,nextPt: integer; x,y: single): pointer; override;
    function CreateIntersectionInfo: TIntersectionInfo; override;
  end;

  TShaderFunction3D = function (Context: PBasicLightingContext; Color: TBGRAPixel): TBGRAPixel of object;

procedure PolygonPerspectiveTextureMappingAliased(bmp: TBGRACustomBitmap; polyInfo: TPolygonPerspectiveTextureMappingInfo;
         texture: IBGRAScanner; TextureInterpolation: Boolean; NonZeroWinding: boolean; zbuffer: psingle = nil); overload;
procedure PolygonPerspectiveTextureMappingAliased(bmp: TBGRACustomBitmap; const points: array of TPointF; const pointsZ: array of single; texture: IBGRAScanner;
           const texCoords: array of TPointF; TextureInterpolation: Boolean; NonZeroWinding: boolean; zbuffer: psingle = nil); overload;
procedure PolygonPerspectiveTextureMappingAliasedWithLightness(bmp: TBGRACustomBitmap; const points: array of TPointF; const pointsZ: array of single; texture: IBGRAScanner;
           const texCoords: array of TPointF; TextureInterpolation: Boolean; lightnesses: array of word; NonZeroWinding: boolean; zbuffer: psingle = nil); overload;

procedure PolygonPerspectiveMappingShaderAliased(bmp: TBGRACustomBitmap; polyInfo: TPolygonPerspectiveMappingShaderInfo;
         texture: IBGRAScanner; TextureInterpolation: Boolean; ShaderFunction: TShaderFunction3D; NonZeroWinding: boolean;
         solidColor: TBGRAPixel; zbuffer: psingle = nil; ShaderContext: PBasicLightingContext= nil); overload;
procedure PolygonPerspectiveMappingShaderAliased(bmp: TBGRACustomBitmap; const points: array of TPointF; const points3D: array of TPoint3D;
           const normals: array of TPoint3D; texture: IBGRAScanner; const texCoords: array of TPointF;
           TextureInterpolation: Boolean; ShaderFunction: TShaderFunction3D; NonZeroWinding: boolean;
           solidColor: TBGRAPixel; zbuffer: psingle = nil; ShaderContext: PBasicLightingContext= nil); overload;
procedure PolygonPerspectiveMappingShaderAliased(bmp: TBGRACustomBitmap; const points: array of TPointF; const points3D: array of TPoint3D_128;
           const normals: array of TPoint3D_128; texture: IBGRAScanner; const texCoords: array of TPointF;
           TextureInterpolation: Boolean; ShaderFunction: TShaderFunction3D; NonZeroWinding: boolean;
           solidColor: TBGRAPixel; zbuffer: psingle = nil; ShaderContext: PBasicLightingContext= nil); overload;

{ Aliased round rectangle }
procedure BGRARoundRectAliased(dest: TBGRACustomBitmap; X1, Y1, X2, Y2: integer;
  DX, DY: integer; BorderColor, FillColor: TBGRAPixel; FillTexture: IBGRAScanner = nil; ADrawMode: TDrawMode = dmDrawWithTransparency;
  skipFill: boolean = false);

implementation

uses Math, BGRABlend;

{ TPolygonPerspectiveColorGradientInfo }

procedure TPolygonPerspectiveColorGradientInfo.SetIntersectionValues(
  AInter: TIntersectionInfo; AInterX: Single; AWinding, ANumSegment: integer;
  dy: single; AData: pointer);
var
  info: PPerspectiveColorInfo;
begin
  AInter.SetValues(AInterX,AWinding,ANumSegment);
  info := PPerspectiveColorInfo(AData);
  TPerspectiveColorGradientIntersectionInfo(AInter).coordInvZ := dy*info^.InvZSlope + info^.InvZ;
  TPerspectiveColorGradientIntersectionInfo(AInter).ColorDivZ := info^.ColorDivZ + info^.ColorSlopesDivZ*dy;
end;

constructor TPolygonPerspectiveColorGradientInfo.Create(
  const points: array of TPointF; const pointsZ: array of single;
  const Colors: array of TBGRAPixel);
var
  i: Integer;
  lPoints: array of TPointF;
  nbP: integer;
  ec: TExpandedPixel;
begin
  if (length(Colors) <> length(points)) or (length(points) <> length(pointsZ)) then
    raise Exception.Create('Dimensions mismatch');

  setlength(lPoints, length(points));
  SetLength(FColors, length(points));
  SetLength(FPointsZ, length(points));
  nbP := 0;
  for i := 0 to high(points) do
  if (i=0) or (points[i]<>points[i-1]) then
  begin
    lPoints[nbP] := points[i];
    FPointsZ[nbP] := PointsZ[i];
    ec := GammaExpansion(Colors[i]);
    FColors[nbP] := ColorF(ec.red,ec.green,ec.blue,ec.alpha);
    inc(nbP);
  end;
  if (nbP>0) and (lPoints[nbP-1] = lPoints[0]) then dec(NbP);
  setlength(lPoints, nbP);
  SetLength(FPointsZ, nbP);
  SetLength(FColors, nbP);

  inherited Create(lPoints);
end;

{$hints off}
function TPolygonPerspectiveColorGradientInfo.CreateSegmentData(numPt,
  nextPt: integer; x, y: single): pointer;
var
  info: PPerspectiveColorInfo;
  InvTy,dy: single;
  CurColorDivByZ,NextColorDivByZ: TColorF;
  CurInvZ,NextInvZ: single;
begin
  New(info);
  InvTy := 1/(FPoints[nextPt].y-FPoints[numPt].y);

  CurInvZ := 1/FPointsZ[numPt];
  CurColorDivByZ := FColors[numPt]*CurInvZ;
  NextInvZ := 1/FPointsZ[nextPt];
  NextColorDivByZ := FColors[nextPt]*NextInvZ;

  info^.ColorSlopesDivZ := (NextColorDivByZ - CurColorDivByZ)*InvTy;
  dy := y-FPoints[numPt].y;
  info^.ColorDivZ := CurColorDivByZ + info^.ColorSlopesDivZ*dy;

  info^.InvZSlope := (NextInvZ-CurInvZ)*InvTy;
  info^.InvZ := CurInvZ+dy*info^.InvZSlope;

  Result:= info;
end;
{$hints on}

function TPolygonPerspectiveColorGradientInfo.CreateIntersectionInfo: TIntersectionInfo;
begin
  Result:= TPerspectiveColorGradientIntersectionInfo.Create;
end;

{ TPolygonLinearColorGradientInfo }

procedure TPolygonLinearColorGradientInfo.SetIntersectionValues(
  AInter: TIntersectionInfo; AInterX: Single; AWinding, ANumSegment: integer;
  dy: single; AData: pointer);
var
  info: PLinearColorInfo;
begin
  AInter.SetValues(AInterX,AWinding,ANumSegment);
  info := PLinearColorInfo(AData);
  TLinearColorGradientIntersectionInfo(AInter).color := info^.Color + info^.ColorSlopes*dy;
end;

constructor TPolygonLinearColorGradientInfo.Create(
  const points: array of TPointF; const Colors: array of TBGRAPixel);
var
  i: Integer;
  lPoints: array of TPointF;
  nbP: integer;
  ec: TExpandedPixel;
begin
  if length(Colors) <> length(points) then
    raise Exception.Create('Dimensions mismatch');

  setlength(lPoints, length(points));
  SetLength(FColors, length(points));
  nbP := 0;
  for i := 0 to high(points) do
  if (i=0) or (points[i]<>points[i-1]) then
  begin
    lPoints[nbP] := points[i];
    ec := GammaExpansion(Colors[i]);
    FColors[nbP] := ColorF(ec.red,ec.green,ec.blue,ec.alpha);
    inc(nbP);
  end;
  if (nbP>0) and (lPoints[nbP-1] = lPoints[0]) then dec(NbP);
  setlength(lPoints, nbP);
  SetLength(FColors, nbP);

  inherited Create(lPoints);
end;

{$hints off}
function TPolygonLinearColorGradientInfo.CreateSegmentData(numPt, nextPt: integer; x,
  y: single): pointer;
var
  info: PLinearColorInfo;
  ty,dy: single;
begin
  New(info);
  ty := FPoints[nextPt].y-FPoints[numPt].y;
  info^.ColorSlopes := (FColors[nextPt] - FColors[numPt])*(1/ty);
  dy := y-FPoints[numPt].y;
  info^.Color := FColors[numPt] + info^.ColorSlopes*dy;
  Result:= info;
end;
{$hints on}

function TPolygonLinearColorGradientInfo.CreateIntersectionInfo: TIntersectionInfo;
begin
  Result:= TLinearColorGradientIntersectionInfo.Create;
end;

procedure PolygonLinearColorGradientAliased(bmp: TBGRACustomBitmap;
  polyInfo: TPolygonLinearColorGradientInfo; NonZeroWinding: boolean);
var
  inter:    array of TIntersectionInfo;
  nbInter:  integer;

  procedure DrawGradientLine(yb: integer; ix1: integer; ix2: integer;
    x1: Single; c1: TColorF; x2: Single; c2: TColorF);
  var
    colorPos: TColorF;
    colorStep: TColorF;
    t: single;
    pdest: PBGRAPixel;
    i: LongInt;
    ec: TExpandedPixel;
    {%H-}cInt: packed record
        r,g,b,a: integer;
       end;
    {$IFDEF BGRASSE_AVAILABLE} c: TBGRAPixel; {$ENDIF}
  begin
    t := ((ix1+0.5)-x1)/(x2-x1);
    colorPos := c1 + (c2-c1)*t;
    colorStep := (c2-c1)*(1/(x2-x1));
    pdest := bmp.ScanLine[yb]+ix1;

    {$IFDEF BGRASSE_AVAILABLE} {$asmmode intel}
    If UseSSE then
    begin
      asm
        movups xmm4, colorPos
        movups xmm5, colorStep
      end;
      If UseSSE2 then
      begin
        for i := ix1 to ix2 do
        begin
          asm
            cvtps2dq xmm0,xmm4
            movups cInt, xmm0
            addps xmm4,xmm5
          end;
          c.red := GammaCompressionTab[cInt.r];
          c.green := GammaCompressionTab[cInt.g];
          c.blue := GammaCompressionTab[cInt.b];
          c.alpha := GammaCompressionTab[cInt.a];
          DrawPixelInlineWithAlphaCheck(pdest, c);
          inc(pdest);
        end;
      end else
      begin
        for i := ix1 to ix2 do
        begin
          asm
            movups colorPos, xmm4
            addps xmm4,xmm5
          end;
          ec.red := round(colorPos[1]);
          ec.green := round(colorPos[2]);
          ec.blue := round(colorPos[3]);
          ec.alpha := round(colorPos[4]);
          DrawPixelInlineWithAlphaCheck(pdest, GammaCompression(ec));
          inc(pdest);
        end;
      end;
    end else
    {$ENDIF}
    for i := ix1 to ix2 do
    begin
      ec.red := round(colorPos[1]);
      ec.green := round(colorPos[2]);
      ec.blue := round(colorPos[3]);
      ec.alpha := round(colorPos[4]);
      DrawPixelInlineWithAlphaCheck(pdest, GammaCompression(ec));
      colorPos += colorStep;
      inc(pdest);
    end;
  end;

var
  miny, maxy, minx, maxx: integer;

  yb, i: integer;
  x1, x2: single;

  ix1, ix2: integer;

begin
  If not BGRAShapeComputeMinMax(polyInfo,minx,miny,maxx,maxy,bmp) then exit;
  inter := polyInfo.CreateIntersectionArray;

  //vertical scan
  for yb := miny to maxy do
  begin
    //find intersections
    polyInfo.ComputeAndSort(yb+0.5001,inter,nbInter,NonZeroWinding);

    for i := 0 to nbinter div 2 - 1 do
    begin
      x1 := inter[i + i].interX;
      x2 := inter[i + i+ 1].interX;

      if x1 <> x2 then
      begin
        ComputeAliasedRowBounds(x1,x2, minx,maxx, ix1,ix2);
        if ix1 <= ix2 then
          DrawGradientLine(yb,ix1,ix2,
            x1,TLinearColorGradientIntersectionInfo(inter[i+i]).Color,
            x2,TLinearColorGradientIntersectionInfo(inter[i+i+1]).Color);
      end;
    end;
  end;

  polyInfo.FreeIntersectionArray(inter);
  bmp.InvalidateBitmap;
end;

procedure PolygonLinearColorGradientAliased(bmp: TBGRACustomBitmap;
  const points: array of TPointF; const Colors: array of TBGRAPixel;
  NonZeroWinding: boolean);
var polyInfo: TPolygonLinearColorGradientInfo;
begin
  polyInfo := TPolygonLinearColorGradientInfo.Create(points,Colors);
  PolygonLinearColorGradientAliased(bmp,polyInfo,NonZeroWinding);
  polyInfo.Free;
end;

{ TPolygonLinearTextureMappingInfo }

procedure TPolygonLinearTextureMappingInfo.SetIntersectionValues(
  AInter: TIntersectionInfo; AInterX: Single; AWinding, ANumSegment: integer;
  dy: single; AData: pointer);
var
  info: PLinearTextureInfo;
begin
  AInter.SetValues(AInterX,AWinding,ANumSegment);
  info := PLinearTextureInfo(AData);
  TLinearTextureMappingIntersectionInfo(AInter).texCoord := info^.TexCoord + info^.TexCoordSlopes*dy;
  if FLightnesses<>nil then
    TLinearTextureMappingIntersectionInfo(AInter).lightness := round(info^.lightness + info^.lightnessSlope*dy)
  else
    TLinearTextureMappingIntersectionInfo(AInter).lightness := 32768;
end;

constructor TPolygonLinearTextureMappingInfo.Create(const points: array of TPointF;
  const texCoords: array of TPointF);
var
  i: Integer;
  lPoints: array of TPointF;
  nbP: integer;
begin
  if length(texCoords) <> length(points) then
    raise Exception.Create('Dimensions mismatch');

  setlength(lPoints, length(points));
  SetLength(FTexCoords, length(points));
  nbP := 0;
  for i := 0 to high(points) do
  if (i=0) or (points[i]<>points[i-1]) then
  begin
    lPoints[nbP] := points[i];
    FTexCoords[nbP] := texCoords[i];
    inc(nbP);
  end;
  if (nbP>0) and (lPoints[nbP-1] = lPoints[0]) then dec(NbP);
  setlength(lPoints, nbP);
  SetLength(FTexCoords, nbP);

  inherited Create(lPoints);
end;

constructor TPolygonLinearTextureMappingInfo.Create(
  const points: array of TPointF; const texCoords: array of TPointF;
  const lightnesses: array of word);
var
  i: Integer;
  lPoints: array of TPointF;
  nbP: integer;
begin
  if (length(texCoords) <> length(points)) or (length(lightnesses) <> length(points)) then
    raise Exception.Create('Dimensions mismatch');

  setlength(lPoints, length(points));
  SetLength(FTexCoords, length(points));
  setlength(FLightnesses, length(lightnesses));
  nbP := 0;
  for i := 0 to high(points) do
  if (i=0) or (points[i]<>points[i-1]) then
  begin
    lPoints[nbP] := points[i];
    FTexCoords[nbP] := texCoords[i];
    FLightnesses[nbP] := lightnesses[i];
    inc(nbP);
  end;
  if (nbP>0) and (lPoints[nbP-1] = lPoints[0]) then dec(NbP);
  setlength(lPoints, nbP);
  SetLength(FTexCoords, nbP);
  SetLength(FLightnesses, nbP);

  inherited Create(lPoints);
end;

{$hints off}
function TPolygonLinearTextureMappingInfo.CreateSegmentData(numPt, nextPt: integer; x,
  y: single): pointer;
var
  info: PLinearTextureInfo;
  ty,dy: single;
begin
  New(info);
  ty := FPoints[nextPt].y-FPoints[numPt].y;
  dy := y-FPoints[numPt].y;
  info^.TexCoordSlopes := (FTexCoords[nextPt] - FTexCoords[numPt])*(1/ty);
  info^.TexCoord := FTexCoords[numPt] + info^.TexCoordSlopes*dy;
  if FLightnesses <> nil then
  begin
    info^.lightnessSlope := (FLightnesses[nextPt] - FLightnesses[numPt])*(1/ty);
    info^.lightness := FLightnesses[numPt] + info^.lightnessSlope*dy;
  end else
  begin
    info^.lightness := 32768;
    info^.lightnessSlope := 0;
  end;
  Result:= info;
end;
{$hints on}

function TPolygonLinearTextureMappingInfo.CreateIntersectionInfo: TIntersectionInfo;
begin
  result := TLinearTextureMappingIntersectionInfo.Create;
end;

{$hints off}

procedure PolygonPerspectiveColorGradientAliased(bmp: TBGRACustomBitmap;
  polyInfo: TPolygonPerspectiveColorGradientInfo; NonZeroWinding: boolean; zbuffer: psingle);
var
  inter:    array of TIntersectionInfo;
  nbInter:  integer;

  procedure DrawGradientLine(yb: integer; ix1: integer; ix2: integer;
    x1: Single; info1: TPerspectiveColorGradientIntersectionInfo; x2: Single; info2: TPerspectiveColorGradientIntersectionInfo);
  var
    diff,colorPos,{%H-}colorPosByZ: TColorF;
    colorStep: TColorF;
    t: single;
    pdest: PBGRAPixel;
    i: LongInt;
    ec: TExpandedPixel;
    invDx: single;
    z,invZ,InvZStep: single;
    r,g,b,a: integer;
    {$IFDEF BGRASSE_AVAILABLE}minVal,maxVal: single;
    cInt: packed record
      r,g,b,a: integer;
    end;
    c: TBGRAPixel;{$ENDIF}
    zbufferpos: psingle;

  begin
    invDx := 1/(x2-x1);
    t := ((ix1+0.5)-x1)*InvDx;
    diff := info2.ColorDivZ-info1.ColorDivZ;
    colorPos := info1.ColorDivZ + diff*t;
    colorStep := diff*InvDx;
    invZ := info1.coordInvZ + (info2.coordInvZ-info1.coordInvZ)*t;
    InvZStep := (info2.coordInvZ-info1.coordInvZ)*InvDx;
    pdest := bmp.ScanLine[yb]+ix1;
    if zbuffer <> nil then
    begin
    {$DEFINE PARAM_USEZBUFFER}
      zbufferpos := zbuffer + yb*bmp.Width + ix1;
      {$IFDEF BGRASSE_AVAILABLE}
      If UseSSE then
      begin
        {$DEFINE PARAM_USESSE}
        If UseSSE2 then
        begin
          {$DEFINE PARAM_USESSE2}
          {$i perspectivecolorscan.inc}
          {$UNDEF PARAM_USESSE2}
        end else
        begin
          {$i perspectivecolorscan.inc}
        end;
        {$UNDEF PARAM_USESSE}
      end else
      {$ENDIF}
      begin
        {$i perspectivecolorscan.inc}
      end;
    {$UNDEF PARAM_USEZBUFFER}
    end else
    begin
      {$IFDEF BGRASSE_AVAILABLE}
      If UseSSE then
      begin
        {$DEFINE PARAM_USESSE}
        If UseSSE2 then
        begin
          {$DEFINE PARAM_USESSE2}
          {$i perspectivecolorscan.inc}
          {$UNDEF PARAM_USESSE2}
        end else
        begin
          {$i perspectivecolorscan.inc}
        end;
        {$UNDEF PARAM_USESSE}
      end else
      {$ENDIF}
      begin
        {$i perspectivecolorscan.inc}
      end;
    end;
  end;

var
  miny, maxy, minx, maxx: integer;

  yb, i: integer;
  x1, x2: single;

  ix1, ix2: integer;

begin
  If not BGRAShapeComputeMinMax(polyInfo,minx,miny,maxx,maxy,bmp) then exit;
  inter := polyInfo.CreateIntersectionArray;

  //vertical scan
  for yb := miny to maxy do
  begin
    //find intersections
    polyInfo.ComputeAndSort(yb+0.5001,inter,nbInter,NonZeroWinding);

    for i := 0 to nbinter div 2 - 1 do
    begin
      x1 := inter[i + i].interX;
      x2 := inter[i + i+ 1].interX;

      if x1 <> x2 then
      begin
        ComputeAliasedRowBounds(x1,x2, minx,maxx, ix1,ix2);
        if ix1 <= ix2 then
          DrawGradientLine(yb,ix1,ix2,
            x1,TPerspectiveColorGradientIntersectionInfo(inter[i+i]),
            x2,TPerspectiveColorGradientIntersectionInfo(inter[i+i+1]));
      end;
    end;
  end;

  polyInfo.FreeIntersectionArray(inter);
  bmp.InvalidateBitmap;
end;

procedure PolygonPerspectiveColorGradientAliased(bmp: TBGRACustomBitmap;
  const points: array of TPointF; const pointsZ: array of single;
  const Colors: array of TBGRAPixel; NonZeroWinding: boolean; zbuffer: psingle);
var polyInfo: TPolygonPerspectiveColorGradientInfo;
begin
  polyInfo := TPolygonPerspectiveColorGradientInfo.Create(points,pointsZ,Colors);
  PolygonPerspectiveColorGradientAliased(bmp,polyInfo,NonZeroWinding,zbuffer);
  polyInfo.Free;
end;

procedure PolygonLinearTextureMappingAliased(bmp: TBGRACustomBitmap; polyInfo: TPolygonLinearTextureMappingInfo;
  texture: IBGRAScanner; TextureInterpolation: Boolean; NonZeroWinding: boolean);
var
  inter:    array of TIntersectionInfo;
  nbInter:  integer;
  scanAtFunc: function(X,Y: Single): TBGRAPixel of object;
  scanAtIntegerFunc: function(X,Y: integer): TBGRAPixel of object;

  procedure DrawTextureLineWithoutLight(yb: integer; ix1: integer; ix2: integer;
    info1,info2: TLinearTextureMappingIntersectionInfo;
    WithInterpolation: boolean);
    {$i lineartexscan.inc}

  procedure DrawTextureLineWithLight(yb: integer; ix1: integer; ix2: integer;
    info1,info2: TLinearTextureMappingIntersectionInfo;
    WithInterpolation: boolean);
    {$define PARAM_USELIGHTING}
    {$i lineartexscan.inc}

var
  miny, maxy, minx, maxx: integer;

  yb, i: integer;
  x1, x2: single;

  ix1, ix2: integer;

begin
  If not BGRAShapeComputeMinMax(polyInfo,minx,miny,maxx,maxy,bmp) then exit;

  scanAtFunc := @texture.ScanAt;
  scanAtIntegerFunc := @texture.ScanAtInteger;

  inter := polyInfo.CreateIntersectionArray;

  //vertical scan
  for yb := miny to maxy do
  begin
    //find intersections
    polyInfo.ComputeAndSort(yb+0.5001,inter,nbInter,NonZeroWinding);
    for i := 0 to nbinter div 2 - 1 do
    begin
      x1 := inter[i + i].interX;
      x2 := inter[i + i+ 1].interX;

      if x1 <> x2 then
      begin
        ComputeAliasedRowBounds(x1,x2, minx,maxx, ix1,ix2);
        if ix1 <= ix2 then
        begin
          if (TLinearTextureMappingIntersectionInfo(inter[i+i]).lightness = 32768) and
             (TLinearTextureMappingIntersectionInfo(inter[i+i+1]).lightness = 32768) then
            DrawTextureLineWithoutLight(yb,ix1,ix2,
               TLinearTextureMappingIntersectionInfo(inter[i+i]),
               TLinearTextureMappingIntersectionInfo(inter[i+i+1]),
               TextureInterpolation)
          else
            DrawTextureLineWithLight(yb,ix1,ix2,
               TLinearTextureMappingIntersectionInfo(inter[i+i]),
               TLinearTextureMappingIntersectionInfo(inter[i+i+1]),
               TextureInterpolation);
        end;
      end;
    end;
  end;

  polyInfo.FreeIntersectionArray(inter);
  bmp.InvalidateBitmap;
end;
{$hints on}

procedure PolygonLinearTextureMappingAliased(bmp: TBGRACustomBitmap;
  const points: array of TPointF; texture: IBGRAScanner;
  const texCoords: array of TPointF; TextureInterpolation: Boolean; NonZeroWinding: boolean);
var polyInfo: TPolygonLinearTextureMappingInfo;
begin
  polyInfo := TPolygonLinearTextureMappingInfo.Create(points,texCoords);
  PolygonLinearTextureMappingAliased(bmp,polyInfo,texture,TextureInterpolation,NonZeroWinding);
  polyInfo.Free;
end;

procedure PolygonLinearTextureMappingAliasedWithLightness(
  bmp: TBGRACustomBitmap; const points: array of TPointF;
  texture: IBGRAScanner; const texCoords: array of TPointF;
  TextureInterpolation: Boolean; lightnesses: array of word;
  NonZeroWinding: boolean);
var polyInfo: TPolygonLinearTextureMappingInfo;
begin
  polyInfo := TPolygonLinearTextureMappingInfo.Create(points,texCoords,lightnesses);
  PolygonLinearTextureMappingAliased(bmp,polyInfo,texture,TextureInterpolation,NonZeroWinding);
  polyInfo.Free;
end;

{$i polyaliaspersp.inc}

{From LazRGBGraphics}
procedure BGRARoundRectAliased(dest: TBGRACustomBitmap; X1, Y1, X2, Y2: integer;
  DX, DY: integer; BorderColor, FillColor: TBGRAPixel; FillTexture: IBGRAScanner = nil; ADrawMode: TDrawMode = dmDrawWithTransparency;
  skipFill: boolean = false);
var
  CX, CY, CX1, CY1, A, B, NX, NY: single;
  X, Y, EX, EY: integer;
  LX1, LY1: integer;
  LX2, LY2: integer;
  DivSqrA, DivSqrB: single;
  I, J, S: integer;
  EdgeList: array of TPoint;
  temp:   integer;
  LX, LY: integer;
  RowStart,RowEnd: integer;
  PixelProc: procedure (x, y: int32or64; c: TBGRAPixel) of object;
  skipBorder: boolean;

  procedure AddEdge(X, Y: integer);
  begin
    If (Y > High(EdgeList)) or (Y < 0) then exit;
    if (EdgeList[Y].X = -1) or (X < EdgeList[Y].X) then
      EdgeList[Y].X := X;
    if (EdgeList[Y].Y = -1) or (X > EdgeList[Y].Y) then
      EdgeList[Y].Y := X;
  end;

begin
  if (x1 > x2) then
  begin
    temp := x1;
    x1   := x2;
    x2   := temp;
  end;
  if (y1 > y2) then
  begin
    temp := y1;
    y1   := y2;
    y2   := temp;
  end;
  if (x2 - x1 <= 0) or (y2 - y1 <= 0) then
    exit;
  LX := x2 - x1 - DX;
  LY := y2 - y1 - DY;
  if LX < 0 then LX := 0;
  if LY < 0 then LY := 0;
  Dec(x2);
  Dec(y2);

  if (X1 = X2) and (Y1 = Y2) then
  begin
    dest.DrawPixel(X1, Y1, BorderColor, ADrawMode);
    Exit;
  end;

  if (X2 - X1 = 1) or (Y2 - Y1 = 1) then
  begin
    dest.FillRect(X1, Y1, X2 + 1, Y2 + 1, BorderColor, ADrawMode);
    Exit;
  end;

  if (LX > X2 - X1) or (LY > Y2 - Y1) then
  begin
    dest.Rectangle(X1, Y1, X2 + 1, Y2 + 1, BorderColor, ADrawMode);
    if not skipFill then
      if FillTexture <> nil then
        dest.FillRect(X1 + 1, Y1 + 1, X2, Y2, FillTexture, ADrawMode) else
        dest.FillRect(X1 + 1, Y1 + 1, X2, Y2, FillColor, ADrawMode);
    Exit;
  end;

  SetLength(EdgeList, Ceil((Y2 - Y1 + 1) / 2));
  for I := 0 to Pred(High(EdgeList)) do
    EdgeList[I] := Point(-1, -1);
  EdgeList[High(EdgeList)] := Point(0, 0);

  A  := (X2 - X1 + 1 - LX) / 2;
  B  := (Y2 - Y1 + 1 - LY) / 2;
  CX := (X2 + X1 + 1) / 2;
  CY := (Y2 + Y1 + 1) / 2;

  CX1 := X2 + 1 - A - Floor(CX);
  CY1 := Y2 + 1 - B - Floor(CY);

  EX := Floor(Sqr(A) / Sqrt(Sqr(A) + Sqr(B)) + Frac(A));
  EY := Floor(Sqr(B) / Sqrt(Sqr(A) + Sqr(B)) + Frac(B));

  DivSqrA := 1 / Sqr(A);
  DivSqrB := 1 / Sqr(B);

  NY := B;
  AddEdge(Floor(CX1), Round(CY1 + B) - 1);
  for X := 1 to Pred(EX) do
  begin
    NY := B * Sqrt(1 - Sqr(X + 0.5 - Frac(A)) * DivSqrA);

    AddEdge(Floor(CX1) + X, Round(CY1 + NY) - 1);
  end;

  LX1 := Floor(CX1) + Pred(EX);
  LY1 := Round(CY1 + NY) - 1;

  NX := A;
  AddEdge(Round(CX1 + A) - 1, Floor(CY1));
  for Y := 1 to Pred(EY) do
  begin
    NX := A * Sqrt(1 - Sqr(Y + 0.5 - Frac(B)) * DivSqrB);

    AddEdge(Round(CX1 + NX) - 1, Floor(CY1) + Y);
  end;

  LX2 := Round(CX1 + NX) - 1;
  LY2 := Floor(CY1) + Pred(EY);

  if Abs(LX1 - LX2) > 1 then
  begin
    if Abs(LY1 - LY2) > 1 then
      AddEdge(LX1 + 1, LY1 - 1)
    else
      AddEdge(LX1 + 1, LY1);
  end
  else
  if Abs(LY1 - LY2) > 1 then
    AddEdge(LX2, LY1 - 1);

  for I := 0 to High(EdgeList) do
  begin
    if EdgeList[I].X = -1 then
      EdgeList[I] := Point(Round(CX1 + A) - 1, Round(CX1 + A) - 1)
    else
      Break;
  end;

  case ADrawMode of
  dmSetExceptTransparent: begin PixelProc := @dest.SetPixel; skipBorder:= BorderColor.alpha <> 255; end;  dmDrawWithTransparency: begin PixelProc := @dest.DrawPixel; skipBorder:= BorderColor.alpha = 0; end;
  dmXor: begin PixelProc := @dest.XorPixel; skipBorder:= DWord(BorderColor) = 0; end;
  dmLinearBlend: begin PixelProc := @dest.FastBlendPixel; skipBorder:= BorderColor.alpha = 0; end;
  else
  begin PixelProc := @dest.SetPixel; skipBorder := false; end;
  end;

  J := 0;
  while J < Length(EdgeList) do
  begin
    if (J = 0) and (Frac(CY) > 0) then
    begin
      if not skipBorder then
      for I := EdgeList[J].X to EdgeList[J].Y do
      begin
        PixelProc(Floor(CX) + I, Floor(CY) + J, BorderColor);
        PixelProc(Ceil(CX) - Succ(I), Floor(CY) + J, BorderColor);
      end;

      if not SkipFill then
        if FillTexture <> nil then
          dest.HorizLine(Ceil(CX) - EdgeList[J].X, Floor(CY) + J, Floor(CX) +
            Pred(EdgeList[J].X), FillTexture, ADrawMode) else
          dest.HorizLine(Ceil(CX) - EdgeList[J].X, Floor(CY) + J, Floor(CX) +
            Pred(EdgeList[J].X), FillColor, ADrawMode);
    end
    else
    if (J = High(EdgeList)) then
    begin
      if Frac(CX) > 0 then
        S := -EdgeList[J].Y
      else
        S := -Succ(EdgeList[J].Y);

      if not skipBorder then
      for I := S to EdgeList[J].Y do
      begin
        PixelProc(Floor(CX) + I, Floor(CY) + J, BorderColor);
        PixelProc(Floor(CX) + I, Ceil(CY) - Succ(J), BorderColor);
      end;
    end
    else
    begin
      if not skipBorder then
      for I := EdgeList[J].X to EdgeList[J].Y do
      begin
        PixelProc(Floor(CX) + I, Floor(CY) + J, BorderColor);
        PixelProc(Floor(CX) + I, Ceil(CY) - Succ(J), BorderColor);
        if Floor(CX) + I <> Ceil(CX) - Succ(I) then
        begin
          PixelProc(Ceil(CX) - Succ(I), Floor(CY) + J, BorderColor);
          PixelProc(Ceil(CX) - Succ(I), Ceil(CY) - Succ(J), BorderColor);
        end;
      end;

      if not SkipFill then
      begin
        RowStart := Ceil(CX) - EdgeList[J].X;
        RowEnd := Floor(CX) + Pred(EdgeList[J].X);
        if RowEnd >= RowStart then
        begin
          if FillTexture <> nil then
          begin
            dest.HorizLine(RowStart, Floor(CY) + J,
              RowEnd, FillTexture, ADrawMode);
            dest.HorizLine(RowStart, Ceil(CY) - Succ(J),
              RowEnd, FillTexture, ADrawMode);
          end else
          begin
            dest.HorizLine(RowStart, Floor(CY) + J,
              RowEnd, FillColor, ADrawMode);
            dest.HorizLine(RowStart, Ceil(CY) - Succ(J),
              RowEnd, FillColor, ADrawMode);
          end;
        end;
      end;

    end;
    Inc(J);
  end;
end;

end.

