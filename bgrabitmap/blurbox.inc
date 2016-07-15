type
  TVertical = record red,green,blue,alpha,count: TAccumulator; end;
  PVertical = ^TVertical;
var
  verticals: PVertical;
  left,right,width,height: NativeInt;
  delta: PtrInt;
  iRadiusX,iRadiusY: NativeInt;
  factExtraX,factExtraY: NativeUInt;

  procedure PrepareVerticals;
  var
    xb,yb: NativeInt;
    psrc,p: PBGRAPixel;
    pvert : PVertical;
    a2: NativeUInt;
  begin
    fillchar(verticals^, width*sizeof(TVertical), 0);
    psrc := FSource.ScanLine[FBounds.Top];
    pvert := verticals;
    if factExtraY = 0 then
    begin
      for xb := left to right-1 do
      begin
        p := psrc+xb;
        for yb := 0 to iRadiusY-1 do
        begin
          if yb = height then break;
          if p^.alpha <> 0 then
          begin
            a2 := p^.alpha;
            {$HINTS OFF}
            pvert^.red += p^.red * a2;
            pvert^.green += p^.green * a2;
            pvert^.blue += p^.blue * a2;
            pvert^.alpha += a2;
            {$HINTS ON}
          end;
          inc(pvert^.count);
          PByte(p) += delta;
        end;
        inc(pvert);
      end;
    end else
    begin
      for xb := left to right-1 do
      begin
        p := psrc+xb;
        for yb := 0 to iRadiusY-1 do
        begin
          if yb = height then break;
          if p^.alpha <> 0 then
          begin
            a2 := p^.alpha * factMainY;
            {$HINTS OFF}
            pvert^.red += p^.red * a2;
            pvert^.green += p^.green * a2;
            pvert^.blue += p^.blue * a2;
            pvert^.alpha += a2;
            {$HINTS ON}
          end;
          inc(pvert^.count, factMainY);
          PByte(p) += delta;
        end;
        if iRadiusY < height then
        begin
          if p^.alpha <> 0 then
          begin
            a2 := p^.alpha * factExtraY;
            {$HINTS OFF}
            pvert^.red += p^.red * a2;
            pvert^.green += p^.green * a2;
            pvert^.blue += p^.blue * a2;
            pvert^.alpha += a2;
            {$HINTS ON}
          end;
          inc(pvert^.count, factExtraY);
        end;
        inc(pvert);
      end;
    end;
  end;

  procedure NextVerticals(y: integer);
  var
    psrc0,psrc1,psrc2,psrc3: PBGRAPixel;
    pvert : PVertical;
    xb: NativeInt;
    a2: NativeUInt;
  begin
    pvert := verticals;
    if y-iRadiusY-1 >= FBounds.Top then
      psrc1 := FSource.ScanLine[y-iRadiusY-1]+left
    else
      psrc1 := nil;
    if y+iRadiusY < FBounds.Bottom then
      psrc2 := FSource.ScanLine[y+iRadiusY]+left
    else
      psrc2 := nil;
    if factExtraY = 0 then
    begin
      for xb := width-1 downto 0 do
      begin
        if psrc1 <> nil then
        begin
          if psrc1^.alpha <> 0 then
          begin
            {$HINTS OFF}
            pvert^.red -= psrc1^.red * psrc1^.alpha;
            pvert^.green -= psrc1^.green * psrc1^.alpha;
            pvert^.blue -= psrc1^.blue * psrc1^.alpha;
            pvert^.alpha -= psrc1^.alpha;
            {$HINTS ON}
          end;
          dec(pvert^.count);
          inc(psrc1);
        end;
        if psrc2 <> nil then
        begin
          if psrc2^.alpha <> 0 then
          begin
            {$HINTS OFF}
            pvert^.red += psrc2^.red * psrc2^.alpha;
            pvert^.green += psrc2^.green * psrc2^.alpha;
            pvert^.blue += psrc2^.blue * psrc2^.alpha;
            pvert^.alpha += psrc2^.alpha;
            {$HINTS ON}
          end;
          inc(pvert^.count);
          inc(psrc2);
        end;
        inc(pvert);
      end;
    end else
    begin
      if y-iRadiusY-2 >= FBounds.Top then
        psrc0 := FSource.ScanLine[y-iRadiusY-2]+left
      else
        psrc0 := nil;
      if y+iRadiusY+1 < FBounds.Bottom then
        psrc3 := FSource.ScanLine[y+iRadiusY+1]+left
      else
        psrc3 := nil;
      for xb := width-1 downto 0 do
      begin
        if psrc0 <> nil then
        begin
          if psrc0^.alpha <> 0 then
          begin
            a2 := psrc0^.alpha*factExtraY;
            {$HINTS OFF}
            pvert^.red -= psrc0^.red * a2;
            pvert^.green -= psrc0^.green * a2;
            pvert^.blue -= psrc0^.blue * a2;
            pvert^.alpha -= a2;
            {$HINTS ON}
          end;
          dec(pvert^.count,factExtraY);
          inc(psrc0);
        end;
        if psrc1 <> nil then
        begin
          if psrc1^.alpha <> 0 then
          begin
            a2 := psrc1^.alpha*(factMainY - factExtraY);
            {$HINTS OFF}
            pvert^.red -= psrc1^.red * a2;
            pvert^.green -= psrc1^.green * a2;
            pvert^.blue -= psrc1^.blue * a2;
            pvert^.alpha -= a2;
            {$HINTS ON}
          end;
          dec(pvert^.count, factMainY - factExtraY);
          inc(psrc1);
        end;
        if psrc2 <> nil then
        begin
          if psrc2^.alpha <> 0 then
          begin
            a2 := psrc2^.alpha*(factMainY - factExtraY);
            {$HINTS OFF}
            pvert^.red += psrc2^.red * a2;
            pvert^.green += psrc2^.green * a2;
            pvert^.blue += psrc2^.blue * a2;
            pvert^.alpha += a2;
            {$HINTS ON}
          end;
          inc(pvert^.count, factMainY - factExtraY);
          inc(psrc2);
        end;
        if psrc3 <> nil then
        begin
          if psrc3^.alpha <> 0 then
          begin
            a2 := psrc3^.alpha*factExtraY;
            {$HINTS OFF}
            pvert^.red += psrc3^.red * a2;
            pvert^.green += psrc3^.green * a2;
            pvert^.blue += psrc3^.blue * a2;
            pvert^.alpha += a2;
            {$HINTS ON}
          end;
          inc(pvert^.count,factExtraY);
          inc(psrc3);
        end;
        inc(pvert);
      end;
    end;
  end;

  procedure MainLoop;
  var
    xb,yb,xdest: NativeInt;
    pdest: PBGRAPixel;
    pvert : PVertical;
    sumRed,sumGreen,sumBlue,sumAlpha,sumCount,
    sumRed2,sumGreen2,sumBlue2,sumAlpha2,sumCount2,
    sumRed3,sumGreen3,sumBlue3,sumAlpha3,sumCount3: TAccumulator;
  begin
    for yb := FBounds.Top to FBounds.Bottom-1 do
    begin
      NextVerticals(yb);
      if GetShouldStop(yb) then exit;
      pdest := Destination.ScanLine[yb]+left;
      sumRed := 0;
      sumGreen := 0;
      sumBlue := 0;
      sumAlpha := 0;
      sumCount := 0;
      pvert := verticals;
      for xb := 0 to iRadiusX-1 do
      begin
        if xb = width then break;
        sumRed += pvert^.red;
        sumGreen += pvert^.green;
        sumBlue += pvert^.blue;
        sumAlpha += pvert^.alpha;
        sumCount += pvert^.count;
        inc(pvert);
      end;
      if factExtraX <> 0 then
      begin
        for xdest := 0 to width-1 do
        begin
          sumRed2 := 0;
          sumGreen2 := 0;
          sumBlue2 := 0;
          sumAlpha2 := 0;
          sumCount2 := 0;
          if xdest-iRadiusX-1 >= 0 then
          begin
            pvert := verticals+(xdest-iRadiusX-1);
            sumRed -= pvert^.red;
            sumGreen -= pvert^.green;
            sumBlue -= pvert^.blue;
            sumAlpha -= pvert^.alpha;
            sumCount -= pvert^.count;

            sumRed2 += pvert^.red;
            sumGreen2 += pvert^.green;
            sumBlue2 += pvert^.blue;
            sumAlpha2 += pvert^.alpha;
            sumCount2 += pvert^.count;
          end;
          if xdest+iRadiusX < width then
          begin
            pvert := verticals+(xdest+iRadiusX);
            sumRed += pvert^.red;
            sumGreen += pvert^.green;
            sumBlue += pvert^.blue;
            sumAlpha += pvert^.alpha;
            sumCount += pvert^.count;
          end;
          if xdest+iRadiusX+1 < width then
          begin
            pvert := verticals+(xdest+iRadiusX+1);
            sumRed2 += pvert^.red;
            sumGreen2 += pvert^.green;
            sumBlue2 += pvert^.blue;
            sumAlpha2 += pvert^.alpha;
            sumCount2 += pvert^.count;
          end;
          sumAlpha3 := sumAlpha*factMainX + sumAlpha2*factExtraX;
          sumCount3 := sumCount*factMainX + sumCount2*factExtraX;
          if (sumAlpha3 >= (sumCount3+1) shr 1) and (sumCount3 > 0) then
          begin
            sumRed3 := sumRed*factMainX + sumRed2*factExtraX;
            sumGreen3 := sumGreen*factMainX + sumGreen2*factExtraX;
            sumBlue3 := sumBlue*factMainX + sumBlue2*factExtraX;
            pdest^.red := (sumRed3+(sumAlpha3 shr 1)) div sumAlpha3;
            pdest^.green := (sumGreen3+(sumAlpha3 shr 1)) div sumAlpha3;
            pdest^.blue := (sumBlue3+(sumAlpha3 shr 1)) div sumAlpha3;
            pdest^.alpha := (sumAlpha3+(sumCount3 shr 1)) div sumCount3;
          end else
            pdest^ := BGRAPixelTransparent;
          inc(pdest);
        end;
      end else
      begin
        for xdest := 0 to width-1 do
        begin
          if xdest-iRadiusX-1 >= 0 then
          begin
            pvert := verticals+(xdest-iRadiusX-1);
            sumRed -= pvert^.red;
            sumGreen -= pvert^.green;
            sumBlue -= pvert^.blue;
            sumAlpha -= pvert^.alpha;
            sumCount -= pvert^.count;
          end;
          if xdest+iRadiusX < width then
          begin
            pvert := verticals+(xdest+iRadiusX);
            sumRed += pvert^.red;
            sumGreen += pvert^.green;
            sumBlue += pvert^.blue;
            sumAlpha += pvert^.alpha;
            sumCount += pvert^.count;
          end;
          if (sumAlpha >= (sumCount+1) shr 1) then
          begin
            pdest^.red := (sumRed+(sumAlpha shr 1)) div sumAlpha;
            pdest^.green := (sumGreen+(sumAlpha shr 1)) div sumAlpha;
            pdest^.blue := (sumBlue+(sumAlpha shr 1)) div sumAlpha;
            pdest^.alpha := (sumAlpha+(sumCount shr 1)) div sumCount;
          end else
            pdest^ := BGRAPixelTransparent;
          inc(pdest);
        end;
      end;
    end;
  end;

begin
  if (FBounds.Right <= FBounds.Left) or (FBounds.Bottom <= FBounds.Top) then exit;
  iRadiusX := floor(FRadiusX+0.5/factMainX);
  iRadiusY := floor(FRadiusY+0.5/factMainY);
  factExtraX := trunc(frac(FRadiusX+0.5/factMainX)*factMainX);
  factExtraY := trunc(frac(FRadiusY+0.5/factMainY)*factMainY);

  if (iRadiusX <= 0) and (iRadiusY <= 0) and (factExtraX <= 0) and (factExtraY <= 0) then
  begin
    Destination.PutImagePart(FBounds.Left,FBounds.Top,FSource,FBounds,dmSet);
    exit;
  end;
  left := FBounds.left;
  right := FBounds.right;
  width := right-left;
  height := FBounds.bottom-FBounds.top;
  delta := FSource.Width*SizeOf(TBGRAPixel);
  if FSource.LineOrder = riloBottomToTop then delta := -delta;

  getmem(verticals, width*sizeof(TVertical));
  try
    PrepareVerticals;
    MainLoop;
  finally
    freemem(verticals);
  end;
end;
