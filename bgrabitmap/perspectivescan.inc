{$i bgrasse.inc}

  var
    //loop variables
    pdest: PBGRAPixel;
    i: LongInt;

    InvXLen: single;    //inverse of horizontal length in pixels
    t: single;          //initial horizontal position in [0..1]
   {$IFNDEF PARAM_USESOLIDCOLOR}
     texPosByZ: TPointF;
     intTexPos: packed record
       x,y: integer;
     end;
     texPos: TPoint3D_128;    //texture start position
   {$ENDIF}
   InvZPos: single;         //Z: depth start position

   {$IFNDEF PARAM_USESOLIDCOLOR}
     texStep: TPoint3D_128;   //texture step
   {$ENDIF}
   zStep: single;      //depth step

  {$IFNDEF PARAM_USESOLIDCOLOR}
    texVect: TPointF;   //texture vector between start and end of line
  {$ENDIF}
    zLen: single;       //depth difference
  {$IFDEF PARAM_USESHADER}
    tempVect3D: TPoint3D_128;
  {$ENDIF}

  {$IFNDEF PARAM_USESOLIDCOLOR}
    {$DEFINE PARAM_USEZPOS}
  {$ENDIF}
  {$IFDEF PARAM_USESHADER}
    {$DEFINE PARAM_USEZPOS}
  {$ENDIF}

  {$IFDEF PARAM_USEZPOS}
    zPos: single;
  {$ENDIF}

  {$IFDEF PARAM_USEZBUFFER}
    pzbuffer: PSingle;
  {$ENDIF}

  {$IFDEF PARAM_USELIGHTING}
    light,lightStep,lightDiff,lightAcc,lightMod: word;
    lightLen: integer;

    procedure NextLight; inline;
    begin
      inc(light,lightStep);
      inc(lightAcc,lightDiff);
      if lightAcc >= lightMod then
      begin
        dec(lightAcc,lightMod);
        inc(light);
      end;
    end;
  {$ENDIF}

  begin
    InvXLen := 1/(info2.interX - info1.interX);
    t := ((ix1+0.5)-info1.interX)*InvXLen;

  {$IFNDEF PARAM_USESOLIDCOLOR}
    texVect := info2.texCoordDivByZ-info1.texCoordDivByZ;
    texPos := Point3D_128(info1.texCoordDivByZ + texVect*t);
    texStep := Point3D_128(texVect*InvXLen);
  {$ENDIF}
    zLen := info2.coordInvZ-info1.coordInvZ;
    InvZPos := info1.coordInvZ+t*zLen;
    zStep := zLen*InvXLen;

  {$IFDEF PARAM_USESHADER}
    tempVect3D := info2.Position3D - info1.Position3D;
    ShaderContext^.PositionInvZ := info1.Position3D + tempVect3D*t;
    ShaderContext^.PositionStepInvZ := tempVect3D*InvXLen;

    tempVect3D := info2.Normal3D - info1.Normal3D;
    ShaderContext^.NormalInvZ := info1.Normal3D + tempVect3D*t;
    ShaderContext^.NormalStepInvZ := tempVect3D*InvXLen;
  {$endif}

    pdest := bmp.ScanLine[yb]+ix1;
  {$IFDEF PARAM_USEZBUFFER}
    pzbuffer := zbuffer + yb*bmp.Width + ix1;
  {$ENDIF}

  {$IFDEF PARAM_USELIGHTING}
    if ix2 = ix1 then
    begin
      light := (info1.lightness+info2.lightness) div 2;
      lightStep := 0;
      lightDiff := 0;
      lightMod := 1;
    end
    else
    begin
      light := info1.lightness;
      lightLen := info2.lightness-info1.lightness;
      lightStep := lightLen div (ix2-ix1);
      lightMod := ix2-ix1;
      lightDiff := lightLen - lightStep*(ix2-ix1);
    end;
    lightAcc := lightDiff div 2;
  {$ENDIF}

  {$IFDEF BGRASSE_AVAILABLE}
    if UseSSE then
    begin
      {$DEFINE PARAM_USESSE}
      if UseSSE3 then
      begin
        {$DEFINE PARAM_USESSE3}
        if WithInterpolation then
        begin
          {$DEFINE PARAM_USEINTERPOLATION}
          {$i perspectivescan2.inc}
          {$UNDEF PARAM_USEINTERPOLATION}
        end else
        begin
          {$i perspectivescan2.inc}
        end;
        {$UNDEF PARAM_USESSE3}
      end else
      begin
        if WithInterpolation then
        begin
          {$DEFINE PARAM_USEINTERPOLATION}
          {$i perspectivescan2.inc}
          {$UNDEF PARAM_USEINTERPOLATION}
        end else
        begin
          {$i perspectivescan2.inc}
        end;
      end;
      {$UNDEF PARAM_USESSE}
    end else
  {$ENDIF}
    begin
      if WithInterpolation then
      begin
        {$DEFINE PARAM_USEINTERPOLATION}
        {$i perspectivescan2.inc}
        {$UNDEF PARAM_USEINTERPOLATION}
      end else
      begin
        {$i perspectivescan2.inc}
      end;
    end;
  end;
{$undef PARAM_USELIGHTING}
{$undef PARAM_USESHADER}
{$undef PARAM_USESOLIDCOLOR}
{$undef PARAM_USEZBUFFER}
{$undef PARAM_USEZPOS}
