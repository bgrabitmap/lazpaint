{$ifdef PARAM_PHONGSSE}
  {$asmmode intel}
  //SSE rotate singles
  const Shift231 = 1 + 8;
        Shift312 = 2 + 16;
{$endif}

var
  //Light source normal.
  vL: TPoint3D_128;  {xmm0}
  //Light source position.
  vLS: TPoint3D_128; {xmm1}
  //Vector H is the unit normal to the hypothetical surface oriented
  //halfway between the light direction vector (L) and the viewing vector (V).
  vH: TPoint3D_128;  {xmm2}

  vN: TPoint3D_128;  {xmm3}        // surface normal
  vP: TPoint3D_128;  {xmm4}        // position of lighted pixel
  vV: TPoint3D_128;                // viewer direction
{$ifdef PARAM_PHONGSSE}
  LightDestFactor4: TPoint3D_128;  // for multiplication
{$endif}

  //Calculate LdotN and NnH
  NH: Single;
{$ifndef PARAM_PHONGSSE}
  vD: TPoint3D_128;
{$endif}

  Iw, Ic: integer;    // Iw: specular intensity, Ic: ambient+diffuse intensity
  sIw: single;        // floating point value for Iw

  z, LdotN, NnH,
  dist, distfactor, diffuseterm, specularterm: single;
  eLight: TExpandedPixel;
  mc,mcLeft,mcRight,mcTop,mcBottom: TBGRAPixel; ///map values

{$ifdef PARAM_SIMPLECOLOR}
  eColor: TExpandedPixel;
{$else}
  {$ifndef PARAM_SCANNER}
    pcolormap: PBGRAPixel;
  {$endif}
{$endif}

  {$hints off}
  function ComputePixel(x,y: integer; DiffuseLight, SpecularLight: Word; Alpha: Byte): TBGRAPixel; inline;
  var ec: TExpandedPixel;
    {$ifndef PARAM_SIMPLECOLOR}
      eColor: TExpandedPixel;
    {$endif}
  begin
    {$ifndef PARAM_SIMPLECOLOR}
      {$ifdef PARAM_SCANNER}
        eColor := GammaExpansion(ColorScan.ScanNextPixel);
      {$else}
        eColor := GammaExpansion(pcolormap^);
      {$endif}
    {$endif}
    Alpha := ApplyOpacity(Alpha, eColor.alpha shr 8);
    ec.red := (eColor.Red*DiffuseLight+eLight.Red*SpecularLight+PhongLightPrecisionDiv2) shr PhongLightPrecisionSh;
    ec.green := (eColor.Green*DiffuseLight+eLight.Green*SpecularLight+PhongLightPrecisionDiv2) shr PhongLightPrecisionSh;
    ec.blue := (eColor.Blue*DiffuseLight+eLight.Blue*SpecularLight+PhongLightPrecisionDiv2) shr PhongLightPrecisionSh;
    ec.alpha := Alpha shl 8+Alpha;
    result := GammaCompression(ec);
  end;
  {$hints on}

var
  minx,miny,maxx,maxy: integer;
  pmap: PBGRAPixel;
  pdest: PBGRAPixel;
  x,y : integer;      // Coordinates of point in height map.
  vS1,vS2: TPoint3D_128; // surface vectors (plane)
  deltaDown: Int32or64;
  IsLineUp,IsLineDown: boolean;

begin
  if map = nil then exit;
  {$ifndef PARAM_SIMPLECOLOR}
    {$ifndef PARAM_SCANNER}
      if (colorMap.Width < map.width) or (colorMap.Height < map.height) then
        raise Exception.Create('Dimension mismatch');
    {$endif}
  {$endif}

  if (map.width = 0) or (map.Height = 0) then exit;
  if ofsX >= dest.ClipRect.Right then exit;
  if ofsY >= dest.ClipRect.Bottom then exit;
  if ofsX <= dest.ClipRect.Left-map.Width then exit;
  if ofsY <= dest.ClipRect.Top-map.Height then exit;

  minx := 0;
  miny := 0;
  maxx := map.Width-1;
  maxy := map.Height-1;
  if ofsX < dest.clipRect.Left then minx := dest.clipRect.Left-ofsX;
  if ofsY < dest.clipRect.Top then miny := dest.clipRect.Top-ofsY;
  if OfsX+maxx > dest.ClipRect.Right-1 then maxx := dest.ClipRect.Right-1-ofsX;
  if OfsY+maxy > dest.ClipRect.Bottom-1 then maxy := dest.ClipRect.Bottom-1-ofsY;

  eLight := GammaExpansion(LightColor);
  {$ifdef PARAM_SIMPLECOLOR}
    eColor := GammaExpansion(color);
  {$endif}

  //light origin
  vLS := Point3D_128(LightPosition.X-ofsX,
                  LightPosition.Y-ofsY,
                  LightPositionZ);

  //surface vectors
  vS1 := Point3D_128(1,0,0);
  vS2 := Point3D_128(0,1,0);

  vV := Point3D_128(0,0,1);

  dist := 0;
  LdotN := 0;
  NnH := 0;

  {$ifdef PARAM_PHONGSSE}
  LightDestFactor4 := Point3D_128(LightDestFactor,LightDestFactor,LightDestFactor,LightDestFactor);
  {$endif}

  if map.LineOrder = riloTopToBottom then
    deltaDown := map.Width*sizeof(TBGRAPixel)
  else
    deltaDown := -map.Width*sizeof(TBGRAPixel);
  for y := miny to maxy do
  begin
    //read map values
    pmap := map.ScanLine[y]+minx;
    mc := BGRAPixelTransparent;
    mcRight := pmap^;
    pdest := dest.ScanLine[y+ofsY]+ofsX+minx;
    {$ifndef PARAM_SIMPLECOLOR}
      {$ifdef PARAM_SCANNER}
        ColorScan.ScanMoveTo(OfsX+minx,OfsY+Y);
      {$else}
        pcolormap := ColorMap.ScanLine[y];
      {$endif}
    {$endif}
    IsLineUp := y > 0;
    IsLineDown := y < map.Height-1;
    mcTop := BGRAPixelTransparent;
    mcBottom := BGRAPixelTransparent;
    for x := minx to maxx do
    begin
      mcLeft := mc;
      mc := mcRight;
      if x < map.width-1 then
        mcRight := (pmap+1)^ else
        mcRight := BGRAPixelTransparent;
      if mc.alpha = 0 then
      begin
        {$ifndef PARAM_SIMPLECOLOR}
          {$ifdef PARAM_SCANNER}
            ColorScan.ScanNextPixel;
          {$else}
            inc(pcolormap);
          {$endif}
        {$endif}
        inc(pdest);
        inc(pmap);
        continue;
      end;

      //compute surface vectors
      if IsLineUp then mcTop := pbgrapixel(pbyte(pmap)-deltaDown)^;
      if IsLineDown then mcBottom := pbgrapixel(pbyte(pmap)+deltaDown)^;
      inc(pmap);

      z := MapHeight(mc)*mapAltitude;
      if mcLeft.alpha = 0 then
      begin
        if mcRight.alpha = 0 then
          vS1.z := 0
        else
          vS1.z := (MapHeight(mcRight)-MapHeight(mc))*mapAltitude*2;
      end else
      begin
        if mcRight.alpha = 0 then
          vS1.z := (MapHeight(mc)-MapHeight(mcLeft))*mapAltitude*2
        else
          vS1.z := (MapHeight(mcRight)-MapHeight(mcLeft))*mapAltitude;
      end;
      if mcTop.alpha = 0 then
      begin
        if mcBottom.alpha = 0 then
          vS2.z := 0
        else
          vS2.z := (MapHeight(mcBottom)-MapHeight(mc))*mapAltitude*2;
      end else
      begin
        if mcBottom.alpha = 0 then
          vS2.z := (MapHeight(mc)-MapHeight(mcTop))*mapAltitude*2
        else
          vS2.z := (MapHeight(mcBottom)-MapHeight(mcTop))*mapAltitude;
      end;

      //position vector
      vP := Point3D_128(x, y, z);
      {$ifdef PARAM_PHONGSSE}
      if UseSSE3 then
      begin
        {$DEFINE PARAM_USESSE3}
        asm
          movups xmm1, vLS
        end;
        {$i phongdrawsse.inc}
        {$UNDEF PARAM_USESSE3}
      end else
      begin
        asm
          movups xmm1, vLS
        end;
        {$i phongdrawsse.inc}
      end;
      {$else}
        vP := Point3D_128(x, y, z);
        vL := vLS- vP*LightDestFactor;
        Normalize3D_128(vL);

        //compute bisector of angle between light and observer
        vH := vL + vV;
        Normalize3D_128(vH);

        // compute normal vector to the surface
        VectProduct3D_128(vS1,vS2,vN);
        Normalize3D_128(vN);

        //Calculate LdotN and NnH
        LdotN := DotProduct3D_128(vN,vL);
        vD := vLS-vP;
        dist := sqrt(DotProduct3D_128(vD,vD));

        NH := DotProduct3D_128(vH,vN);
      {$endif}

      if NH <= 0 then
        NnH := 0
      else
        NnH := exp(SpecularIndex*ln(NH));  //to be optimized

      distfactor := LightSourceIntensity / (dist*LightSourceDistanceFactor + LightSourceDistanceTerm);

      if (LdotN <= 0) then //Point is not illuminated by light source.
        //Use negative diffuse for contrast
        diffuseterm := distfactor * NegativeDiffusionFactor * LdotN
      else
        diffuseterm := distfactor * DiffusionFactor * LdotN;
      Ic := round((AmbientFactor + diffuseterm)*PhongLightPrecision);

      //specular (reflection)
      specularterm := distfactor * SpecularFactor * NnH;
      sIw := specularterm*PhongLightPrecision;
      if sIw > PhongLightPrecision then Iw := PhongLightPrecision else
        Iw := round(sIw);

      //intensity bounds (0..PhongLightPrecision)
      If Ic < 0 then Ic := 0;
      If Ic > PhongLightPrecision then
      begin
        If DiffuseSaturation then
        begin
          Iw := Iw+(Ic-PhongLightPrecision);
          if Iw > PhongLightPrecision then Iw := PhongLightPrecision;
        end;
        Ic := PhongLightPrecision;
      end;
      Ic := Ic*(PhongLightPrecision-Iw) shr PhongLightPrecisionSh;

      DrawPixelInlineWithAlphaCheck(pdest, ComputePixel(x,y,Ic,Iw,mc.alpha));
      {$ifndef PARAM_SIMPLECOLOR}
        {$ifndef PARAM_SCANNER}
          inc(pcolormap);
        {$endif}
      {$endif}
      inc(pdest); //go to next pixel
    end;
  end;
end;

{$undef PARAM_PHONGSSE}
{$undef PARAM_SIMPLECOLOR}
{$undef PARAM_SCANNER}

