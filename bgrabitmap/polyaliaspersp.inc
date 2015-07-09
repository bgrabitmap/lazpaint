{*****************************************TEXTURE WITHOUT SHADING *********************************}
{with shading: second part of this file}

{ TPolygonPerspectiveTextureMappingInfo }

procedure TPolygonPerspectiveTextureMappingInfo.SetIntersectionValues(
  AInter: TIntersectionInfo; AInterX: Single; AWinding, ANumSegment: integer;
  dy: single; AData: pointer);
var info: PPerspectiveTextureInfo;
begin
  AInter.SetValues(AInterX,AWinding,ANumSegment);
  info := PPerspectiveTextureInfo(AData);
  TPerspectiveTextureMappingIntersectionInfo(AInter).coordInvZ := dy*info^.InvZSlope + info^.InvZ;
  TPerspectiveTextureMappingIntersectionInfo(AInter).texCoordDivByZ := info^.TexCoordDivByZ + info^.TexCoordDivByZSlopes*dy;
  if FLightnesses<>nil then
    TPerspectiveTextureMappingIntersectionInfo(AInter).lightness := round(info^.lightness + info^.lightnessSlope*dy)
  else
    TPerspectiveTextureMappingIntersectionInfo(AInter).lightness := 32768;
end;

constructor TPolygonPerspectiveTextureMappingInfo.Create(
  const points: array of TPointF; const pointsZ: array of single;
  const texCoords: array of TPointF);
var
  i: Integer;
  lPoints: array of TPointF;
  nbP: integer;
begin
  if (length(texCoords) <> length(points)) or (length(pointsZ) <> length(points)) then
    raise Exception.Create('Dimensions mismatch');

  setlength(lPoints, length(points));
  SetLength(FTexCoords, length(points));
  SetLength(FPointsZ, length(points));
  nbP := 0;
  for i := 0 to high(points) do
  if (i=0) or (points[i].x<>points[i-1].X) or (points[i].y<>points[i-1].y) then
  begin
    lPoints[nbP] := points[i];
    FTexCoords[nbP] := texCoords[i];
    FPointsZ[nbP] := abs(pointsZ[i]);
    inc(nbP);
  end;
  if (nbP>0) and (lPoints[nbP-1].X = lPoints[0].X) and (lPoints[nbP-1].Y = lPoints[0].Y) then dec(NbP);
  setlength(lPoints, nbP);
  SetLength(FTexCoords, nbP);
  SetLength(FPointsZ, nbP);

  inherited Create(lPoints);
end;

constructor TPolygonPerspectiveTextureMappingInfo.Create(
  const points: array of TPointF; const pointsZ: array of single;
  const texCoords: array of TPointF; const lightnesses: array of word);
var
  i: Integer;
  lPoints: array of TPointF;
  nbP: integer;
begin
  if (length(texCoords) <> length(points)) or (length(pointsZ) <> length(points)) or
     (length(lightnesses) <> length(points)) then
    raise Exception.Create('Dimensions mismatch');

  setlength(lPoints, length(points));
  SetLength(FTexCoords, length(points));
  SetLength(FPointsZ, length(points));
  setLength(FLightnesses, length(points));
  nbP := 0;
  for i := 0 to high(points) do
  if (i=0) or (points[i].x<>points[i-1].X) or (points[i].y<>points[i-1].y) then
  begin
    lPoints[nbP] := points[i];
    FTexCoords[nbP] := texCoords[i];
    FPointsZ[nbP] := abs(pointsZ[i]);
    FLightnesses[nbP] := lightnesses[i];
    inc(nbP);
  end;
  if (nbP>0) and (lPoints[nbP-1].X = lPoints[0].X) and (lPoints[nbP-1].Y = lPoints[0].Y) then dec(NbP);
  setlength(lPoints, nbP);
  SetLength(FTexCoords, nbP);
  SetLength(FPointsZ, nbP);
  SetLength(FLightnesses, nbP);

  inherited Create(lPoints);
end;

{$hints off}

function TPolygonPerspectiveTextureMappingInfo.CreateSegmentData(numPt,
  nextPt: integer; x, y: single): pointer;
var
  info: PPerspectiveTextureInfo;
  ty,dy: single;
  CurInvZ,NextInvZ: single;
  CurTexCoordDivByZ: TPointF;
  NextTexCoordDivByZ: TPointF;
begin
  New(info);
  CurInvZ := 1/FPointsZ[numPt];
  CurTexCoordDivByZ := FTexCoords[numPt]*CurInvZ;
  NextInvZ := 1/FPointsZ[nextPt];
  NextTexCoordDivByZ := FTexCoords[nextPt]*NextInvZ;
  ty := FPoints[nextPt].y-FPoints[numPt].y;
  info^.TexCoordDivByZSlopes := (NextTexCoordDivByZ - CurTexCoordDivByZ)*(1/ty);
  dy := y-FPoints[numPt].y;
  info^.TexCoordDivByZ := CurTexCoordDivByZ + info^.TexCoordDivByZSlopes*dy;
  info^.InvZSlope := (NextInvZ-CurInvZ)/ty;
  info^.InvZ := CurInvZ+dy*info^.InvZSlope;
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

function TPolygonPerspectiveTextureMappingInfo.CreateIntersectionInfo: TIntersectionInfo;
begin
  Result:= TPerspectiveTextureMappingIntersectionInfo.Create;
end;

{$hints off}
procedure PolygonPerspectiveTextureMappingAliased(bmp: TBGRACustomBitmap;
  polyInfo: TPolygonPerspectiveTextureMappingInfo; texture: IBGRAScanner;
  TextureInterpolation: Boolean; NonZeroWinding: boolean; zbuffer: psingle);
var
  inter:    array of TIntersectionInfo;
  nbInter:  integer;

  scanAtFunc: TScanAtFunction;
  scanAtIntegerFunc: TScanAtIntegerFunction;

  procedure DrawTextureLineWithoutLight(yb: integer; ix1: integer; ix2: integer;
      info1, info2 : TPerspectiveTextureMappingIntersectionInfo; WithInterpolation: boolean);
    {$i perspectivescan.inc}

  procedure DrawTextureLineWithLight(yb: integer; ix1: integer; ix2: integer;
      info1, info2 : TPerspectiveTextureMappingIntersectionInfo; WithInterpolation: boolean);
    {$define PARAM_USELIGHTING}
    {$i perspectivescan.inc}

  procedure DrawTextureLineWithoutLightZBuffer(yb: integer; ix1: integer; ix2: integer;
      info1, info2 : TPerspectiveTextureMappingIntersectionInfo; WithInterpolation: boolean);
    {$define PARAM_USEZBUFFER}
    {$i perspectivescan.inc}

  procedure DrawTextureLineWithLightZBuffer(yb: integer; ix1: integer; ix2: integer;
      info1, info2 : TPerspectiveTextureMappingIntersectionInfo; WithInterpolation: boolean);
    {$define PARAM_USEZBUFFER}
    {$define PARAM_USELIGHTING}
    {$i perspectivescan.inc}

var
  miny, maxy, minx, maxx: integer;

  yb, i : integer;
  x1, x2: single;

  ix1, ix2: integer;

begin
  If not BGRAShapeComputeMinMax(polyInfo,minx,miny,maxx,maxy,bmp) then exit;

  inter := polyInfo.CreateIntersectionArray;
  scanAtFunc := @texture.ScanAt;
  scanAtIntegerFunc := @texture.ScanAtInteger;

  if zbuffer = nil then
  begin
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
            if (TPerspectiveTextureMappingIntersectionInfo(inter[i+i]).lightness = 32768) and
               (TPerspectiveTextureMappingIntersectionInfo(inter[i+i+1]).lightness = 32768) then
              DrawTextureLineWithoutLight(yb,ix1,ix2,
                TPerspectiveTextureMappingIntersectionInfo(inter[i+i]),
                TPerspectiveTextureMappingIntersectionInfo(inter[i+i+1]),
                TextureInterpolation)
            else
              DrawTextureLineWithLight(yb,ix1,ix2,
                TPerspectiveTextureMappingIntersectionInfo(inter[i+i]),
                TPerspectiveTextureMappingIntersectionInfo(inter[i+i+1]),
                TextureInterpolation);
          end;
        end;
      end;
    end;
  end else
  begin
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
            if (TPerspectiveTextureMappingIntersectionInfo(inter[i+i]).lightness = 32768) and
               (TPerspectiveTextureMappingIntersectionInfo(inter[i+i+1]).lightness = 32768) then
              DrawTextureLineWithoutLightZBuffer(yb,ix1,ix2,
                TPerspectiveTextureMappingIntersectionInfo(inter[i+i]),
                TPerspectiveTextureMappingIntersectionInfo(inter[i+i+1]),
                TextureInterpolation)
            else
              DrawTextureLineWithLightZBuffer(yb,ix1,ix2,
                TPerspectiveTextureMappingIntersectionInfo(inter[i+i]),
                TPerspectiveTextureMappingIntersectionInfo(inter[i+i+1]),
                TextureInterpolation);
          end;
        end;
      end;
    end;
  end;

  polyInfo.FreeIntersectionArray(inter);
  bmp.InvalidateBitmap;
end;
{$hints on}

procedure PolygonPerspectiveTextureMappingAliased(bmp: TBGRACustomBitmap;
  const points: array of TPointF; const pointsZ: array of single;
  texture: IBGRAScanner; const texCoords: array of TPointF;
  TextureInterpolation: Boolean; NonZeroWinding: boolean; zbuffer: psingle);
var polyInfo: TPolygonPerspectiveTextureMappingInfo;
begin
  polyInfo := TPolygonPerspectiveTextureMappingInfo.Create(points,pointsZ,texCoords);
  PolygonPerspectiveTextureMappingAliased(bmp,polyInfo,texture,TextureInterpolation, NonZeroWinding, zbuffer);
  polyInfo.Free;
end;

procedure PolygonPerspectiveTextureMappingAliasedWithLightness(
  bmp: TBGRACustomBitmap; const points: array of TPointF;
  const pointsZ: array of single; texture: IBGRAScanner;
  const texCoords: array of TPointF; TextureInterpolation: Boolean;
  lightnesses: array of word; NonZeroWinding: boolean; zbuffer: psingle);
var polyInfo: TPolygonPerspectiveTextureMappingInfo;
begin
  polyInfo := TPolygonPerspectiveTextureMappingInfo.Create(points,pointsZ,texCoords,lightnesses);
  PolygonPerspectiveTextureMappingAliased(bmp,polyInfo,texture,TextureInterpolation, NonZeroWinding, zbuffer);
  polyInfo.Free;
end;

{****************************************** WITH SHADING ******************************************}

{$hints off}
procedure PolygonPerspectiveMappingShaderAliased_DrawTextureLine(bmp: TBGRACustomBitmap; ShaderFunction: TShaderFunction3D; ShaderContext: PBasicLightingContext;
    solidColor: TBGRAPixel; scanAtFunc: TScanAtFunction; scanAtIntegerFunc: TScanAtIntegerFunction; zbuffer: psingle;
    yb: integer; ix1: integer; ix2: integer;
    info1, info2 : TPerspectiveTextureMappingIntersectionInfo; WithInterpolation: boolean);
  {$define PARAM_USESHADER}
  {$i perspectivescan.inc}

procedure PolygonPerspectiveMappingShaderAliased_DrawSolidColorLine(bmp: TBGRACustomBitmap; ShaderFunction: TShaderFunction3D; ShaderContext: PBasicLightingContext;
    solidColor: TBGRAPixel; scanAtFunc: TScanAtFunction; scanAtIntegerFunc: TScanAtIntegerFunction; zbuffer: psingle;
    yb: integer; ix1: integer; ix2: integer;
    info1, info2 : TPerspectiveTextureMappingIntersectionInfo; WithInterpolation: boolean);
  {$define PARAM_USESOLIDCOLOR}
  {$define PARAM_USESHADER}
  {$i perspectivescan.inc}

procedure PolygonPerspectiveMappingShaderAliased_DrawTextureLineZBuffer(bmp: TBGRACustomBitmap; ShaderFunction: TShaderFunction3D; ShaderContext: PBasicLightingContext;
    solidColor: TBGRAPixel; scanAtFunc: TScanAtFunction; scanAtIntegerFunc: TScanAtIntegerFunction; zbuffer: psingle;
    yb: integer; ix1: integer; ix2: integer;
    info1, info2 : TPerspectiveTextureMappingIntersectionInfo; WithInterpolation: boolean);
  {$define PARAM_USESHADER}
  {$define PARAM_USEZBUFFER}
  {$i perspectivescan.inc}

procedure PolygonPerspectiveMappingShaderAliased_DrawSolidColorLineZBuffer(bmp: TBGRACustomBitmap; ShaderFunction: TShaderFunction3D; ShaderContext: PBasicLightingContext;
    solidColor: TBGRAPixel; scanAtFunc: TScanAtFunction; scanAtIntegerFunc: TScanAtIntegerFunction; zbuffer: psingle;
    yb: integer; ix1: integer; ix2: integer;
    info1, info2 : TPerspectiveTextureMappingIntersectionInfo; WithInterpolation: boolean);
  {$define PARAM_USESOLIDCOLOR}
  {$define PARAM_USESHADER}
  {$define PARAM_USEZBUFFER}
  {$i perspectivescan.inc}

procedure PolygonPerspectiveMappingAliased_DrawTextureLine(bmp: TBGRACustomBitmap; ShaderFunction: TShaderFunction3D; ShaderContext: PBasicLightingContext;
    solidColor: TBGRAPixel; scanAtFunc: TScanAtFunction; scanAtIntegerFunc: TScanAtIntegerFunction; zbuffer: psingle;
    yb: integer; ix1: integer; ix2: integer;
    info1, info2 : TPerspectiveTextureMappingIntersectionInfo; WithInterpolation: boolean);
  {$i perspectivescan.inc}

procedure PolygonPerspectiveMappingAliased_DrawSolidColorLine(bmp: TBGRACustomBitmap; ShaderFunction: TShaderFunction3D; ShaderContext: PBasicLightingContext;
    solidColor: TBGRAPixel; scanAtFunc: TScanAtFunction; scanAtIntegerFunc: TScanAtIntegerFunction; zbuffer: psingle;
    yb: integer; ix1: integer; ix2: integer;
    info1, info2 : TPerspectiveTextureMappingIntersectionInfo; WithInterpolation: boolean);
  {$define PARAM_USESOLIDCOLOR}
  {$i perspectivescan.inc}

procedure PolygonPerspectiveMappingAliased_DrawTextureLineZBuffer(bmp: TBGRACustomBitmap; ShaderFunction: TShaderFunction3D; ShaderContext: PBasicLightingContext;
    solidColor: TBGRAPixel; scanAtFunc: TScanAtFunction; scanAtIntegerFunc: TScanAtIntegerFunction; zbuffer: psingle;
    yb: integer; ix1: integer; ix2: integer;
    info1, info2 : TPerspectiveTextureMappingIntersectionInfo; WithInterpolation: boolean);
  {$define PARAM_USEZBUFFER}
  {$i perspectivescan.inc}

procedure PolygonPerspectiveMappingAliased_DrawSolidColorLineZBuffer(bmp: TBGRACustomBitmap; ShaderFunction: TShaderFunction3D; ShaderContext: PBasicLightingContext;
    solidColor: TBGRAPixel; scanAtFunc: TScanAtFunction; scanAtIntegerFunc: TScanAtIntegerFunction; zbuffer: psingle;
    yb: integer; ix1: integer; ix2: integer;
    info1, info2 : TPerspectiveTextureMappingIntersectionInfo; WithInterpolation: boolean);
  {$define PARAM_USESOLIDCOLOR}
  {$define PARAM_USEZBUFFER}
  {$i perspectivescan.inc}
{$hints on}

{$hints off}
procedure PolygonPerspectiveMappingShaderAliased(bmp: TBGRACustomBitmap;
  polyInfo: TPolygonPerspectiveMappingShaderInfo; texture: IBGRAScanner;
  TextureInterpolation: Boolean; ShaderFunction: TShaderFunction3D;
  NonZeroWinding: boolean; solidColor: TBGRAPixel; zbuffer: psingle; ShaderContext: PBasicLightingContext);
var
  inter:    array of TIntersectionInfo;
  nbInter:  integer;

  scanAtFunc: TScanAtFunction;
  scanAtIntegerFunc: TScanAtIntegerFunction;

  drawFunc : procedure(bmp: TBGRACustomBitmap; ShaderFunction: TShaderFunction3D; ShaderContext: PBasicLightingContext;
    solidColor: TBGRAPixel; scanAtFunc: TScanAtFunction; scanAtIntegerFunc: TScanAtIntegerFunction; zbuffer: psingle;
    yb: integer; ix1: integer; ix2: integer;
    info1, info2 : TPerspectiveTextureMappingIntersectionInfo; WithInterpolation: boolean);

var
  miny, maxy, minx, maxx: integer;

  yb, i : integer;
  x1, x2: single;

  ix1, ix2: integer;
  shaderContextMem: TMemoryBlockAlign128;
  shaderContextPtr: PBasicLightingContext;

  inter1,inter2: TPerspectiveTextureMappingIntersectionInfo;

begin
  If not BGRAShapeComputeMinMax(polyInfo,minx,miny,maxx,maxy,bmp) then exit;

  inter := polyInfo.CreateIntersectionArray;

  if texture <> nil then
  begin
    scanAtFunc := @texture.ScanAt;
    scanAtIntegerFunc := @texture.ScanAtInteger;
  end else
  begin
    scanAtFunc := nil;
    scanAtIntegerFunc := nil;
  end;

  shaderContextMem := nil;
  shaderContextPtr := nil;

  if ShaderFunction <> nil then
  begin
    if ShaderContext = nil then
    begin
      shaderContextMem := TMemoryBlockAlign128.Create(sizeof(TBasicLightingContext));
      shaderContextPtr := PBasicLightingContext( shaderContextMem.Data);
    end
    else
      shaderContextPtr := shaderContext;
    if texture <> nil then
    begin
      if zbuffer = nil then
        drawFunc := @PolygonPerspectiveMappingShaderAliased_DrawTextureLine
      else
        drawFunc := @PolygonPerspectiveMappingShaderAliased_DrawTextureLineZBuffer;
    end
    else
    begin
      if zbuffer = nil then
        drawFunc := @PolygonPerspectiveMappingShaderAliased_DrawSolidColorLine
      else
        drawFunc := @PolygonPerspectiveMappingShaderAliased_DrawSolidColorLineZBuffer;
    end;
  end else
  begin
    if texture <> nil then
    begin
      if zbuffer = nil then
        drawFunc := @PolygonPerspectiveMappingAliased_DrawTextureLine
      else
        drawFunc := @PolygonPerspectiveMappingAliased_DrawTextureLineZBuffer;
    end
    else
    begin
      if zbuffer = nil then
        drawFunc := @PolygonPerspectiveMappingAliased_DrawSolidColorLine
      else
        drawFunc := @PolygonPerspectiveMappingAliased_DrawSolidColorLineZBuffer;
    end;
  end;

  //vertical scan
  for yb := miny to maxy do
  begin
    //find intersections
    polyInfo.ComputeAndSort(yb+0.5001,inter,nbInter,NonZeroWinding);

    for i := 0 to nbinter div 2 - 1 do
    begin
      inter1 := TPerspectiveTextureMappingIntersectionInfo(inter[i+i]);
      inter2 := TPerspectiveTextureMappingIntersectionInfo(inter[i+i+1]);
      x1 := inter1.interX;
      x2 := inter2.interX;

      if x1 <> x2 then
      begin
        ComputeAliasedRowBounds(x1,x2, minx,maxx, ix1,ix2);
        if ix1 <= ix2 then
        begin
          drawFunc(bmp,ShaderFunction,shaderContextPtr,
              solidColor,scanAtFunc,scanAtIntegerFunc,zbuffer,
              yb,ix1,ix2,
              inter1,inter2,TextureInterpolation);
        end;
      end;
    end;
  end;

  polyInfo.FreeIntersectionArray(inter);
  bmp.InvalidateBitmap;
  shaderContextMem.Free;
end;
{$hints on}

procedure PolygonPerspectiveMappingShaderAliased(bmp: TBGRACustomBitmap;
  const points: array of TPointF; const points3D: array of TPoint3D;
  const normals: array of TPoint3D; texture: IBGRAScanner;
  const texCoords: array of TPointF; TextureInterpolation: Boolean;
  ShaderFunction: TShaderFunction3D; NonZeroWinding: boolean; solidColor: TBGRAPixel; zbuffer: psingle; ShaderContext: PBasicLightingContext);
var polyInfo: TPolygonPerspectiveMappingShaderInfo;
begin
  polyInfo := TPolygonPerspectiveMappingShaderInfo.Create(points,points3D,normals,texCoords);
  PolygonPerspectiveMappingShaderAliased(bmp,polyInfo,texture,TextureInterpolation, ShaderFunction, NonZeroWinding, solidColor, zbuffer, ShaderContext);
  polyInfo.Free;
end;

procedure PolygonPerspectiveMappingShaderAliased(bmp: TBGRACustomBitmap;
  const points: array of TPointF; const points3D: array of TPoint3D_128;
  const normals: array of TPoint3D_128; texture: IBGRAScanner;
  const texCoords: array of TPointF; TextureInterpolation: Boolean;
  ShaderFunction: TShaderFunction3D; NonZeroWinding: boolean;
  solidColor: TBGRAPixel; zbuffer: psingle; ShaderContext: PBasicLightingContext);
var polyInfo: TPolygonPerspectiveMappingShaderInfo;
begin
  polyInfo := TPolygonPerspectiveMappingShaderInfo.Create(points,points3D,normals,texCoords);
  PolygonPerspectiveMappingShaderAliased(bmp,polyInfo,texture,TextureInterpolation, ShaderFunction, NonZeroWinding, solidColor, zbuffer, ShaderContext);
  polyInfo.Free;
end;

{ TPolygonPerspectiveMappingShaderInfo }

procedure TPolygonPerspectiveMappingShaderInfo.SetIntersectionValues(
  AInter: TIntersectionInfo; AInterX: Single; AWinding, ANumSegment: integer;
  dy: single; AData: pointer);
var info : PPerspectiveTextureInfo;
begin
  AInter.SetValues(AInterX,AWinding,ANumSegment);
  info := PPerspectiveTextureInfo(AData);
  TPerspectiveTextureMappingIntersectionInfo(AInter).coordInvZ := dy*info^.InvZSlope + info^.InvZ;
  TPerspectiveTextureMappingIntersectionInfo(AInter).texCoordDivByZ := info^.TexCoordDivByZ + info^.TexCoordDivByZSlopes*dy;
  TPerspectiveTextureMappingIntersectionInfo(AInter).Position3D := info^.Position3D + info^.Position3DSlope*dy;
  TPerspectiveTextureMappingIntersectionInfo(AInter).Normal3D := info^.Normal3D + info^.Normal3DSlope*dy;
end;

constructor TPolygonPerspectiveMappingShaderInfo.Create(
  const points: array of TPointF; const points3D: array of TPoint3D;
  const normals: array of TPoint3D; const texCoords: array of TPointF);
var
  i: Integer;
  lPoints: array of TPointF;
  nbP: integer;
begin
  if (length(texCoords) <> length(points)) or (length(points3D) <> length(points)) or (length(normals) <> length(points)) then
    raise Exception.Create('Dimensions mismatch');

  setlength(lPoints, length(points));
  SetLength(FTexCoords, length(points));
  SetLength(FPositions3D, length(points));
  SetLength(FNormals3D, length(points));
  nbP := 0;
  for i := 0 to high(points) do
  if (i=0) or (points[i]<>points[i-1]) then
  begin
    lPoints[nbP] := points[i];
    FTexCoords[nbP] := texCoords[i];
    FPositions3D[nbP] := Point3D_128(points3D[i]);
    FNormals3D[nbP] := Point3D_128(normals[i]);
    inc(nbP);
  end;
  if (nbP>0) and (lPoints[nbP-1].X = lPoints[0].X) and (lPoints[nbP-1].Y = lPoints[0].Y) then dec(NbP);
  setlength(lPoints, nbP);
  SetLength(FTexCoords, nbP);
  SetLength(FPositions3D, nbP);
  SetLength(FNormals3D, nbP);

  inherited Create(lPoints);
end;

constructor TPolygonPerspectiveMappingShaderInfo.Create(
  const points: array of TPointF; const points3D: array of TPoint3D_128;
  const normals: array of TPoint3D_128; const texCoords: array of TPointF);
var
  i: Integer;
  lPoints: array of TPointF;
  nbP: integer;
begin
  if (length(texCoords) <> length(points)) or (length(points3D) <> length(points)) or (length(normals) <> length(points)) then
    raise Exception.Create('Dimensions mismatch');

  setlength(lPoints, length(points));
  SetLength(FTexCoords, length(points));
  SetLength(FPositions3D, length(points));
  SetLength(FNormals3D, length(points));
  nbP := 0;
  for i := 0 to high(points) do
  if (i=0) or (points[i]<>points[i-1]) then
  begin
    lPoints[nbP] := points[i];
    FTexCoords[nbP] := texCoords[i];
    FPositions3D[nbP] := points3D[i];
    FNormals3D[nbP] := normals[i];
    inc(nbP);
  end;
  if (nbP>0) and (lPoints[nbP-1].X = lPoints[0].X) and (lPoints[nbP-1].Y = lPoints[0].Y) then dec(NbP);
  setlength(lPoints, nbP);
  SetLength(FTexCoords, nbP);
  SetLength(FPositions3D, nbP);
  SetLength(FNormals3D, nbP);

  inherited Create(lPoints);
end;

{$hints off}
function TPolygonPerspectiveMappingShaderInfo.CreateSegmentData(numPt,
  nextPt: integer; x, y: single): pointer;
var
  info: PPerspectiveTextureInfo;
  ty,dy: single;
  CurInvZ,NextInvZ: single;
  CurTexCoordDivByZ: TPointF;
  NextTexCoordDivByZ: TPointF;

  Cur3DDivByZ,Next3DDivByZ: TPoint3D_128;
begin
  New(info);
  CurInvZ := FPositions3D[numPt].z;
  if CurInvZ = 0 then CurInvZ := 1 else CurInvZ := 1/CurInvZ;
  CurTexCoordDivByZ := FTexCoords[numPt]*CurInvZ;
  NextInvZ := FPositions3D[nextPt].z;
  if NextInvZ = 0 then NextInvZ := 1 else NextInvZ := 1/NextInvZ;
  NextTexCoordDivByZ := FTexCoords[nextPt]*NextInvZ;
  ty := FPoints[nextPt].y-FPoints[numPt].y;
  info^.TexCoordDivByZSlopes := (NextTexCoordDivByZ - CurTexCoordDivByZ)*(1/ty);
  dy := y-FPoints[numPt].y;
  info^.TexCoordDivByZ := CurTexCoordDivByZ + info^.TexCoordDivByZSlopes*dy;
  info^.InvZSlope := (NextInvZ-CurInvZ)/ty;
  info^.InvZ := CurInvZ+dy*info^.InvZSlope;

  Cur3DDivByZ := FPositions3D[numPt]*CurInvZ;
  Next3DDivByZ := FPositions3D[nextPt]*NextInvZ;
  info^.Position3DSlope := (Next3DDivByZ - Cur3DDivByZ)*(1/ty);
  info^.Position3D := Cur3DDivByZ + info^.Position3DSlope*dy;

  Cur3DDivByZ := FNormals3D[numPt]*CurInvZ;
  Next3DDivByZ := FNormals3D[nextPt]*NextInvZ;
  info^.Normal3DSlope := (Next3DDivByZ - Cur3DDivByZ)*(1/ty);
  info^.Normal3D := Cur3DDivByZ + info^.Normal3DSlope*dy;

  Result:= info;
end;
{$hints on}

function TPolygonPerspectiveMappingShaderInfo.CreateIntersectionInfo: TIntersectionInfo;
begin
  Result:= TPerspectiveTextureMappingIntersectionInfo.Create;
end;

