
var
  blurRowY,blurRowX: packed array of NativeUInt;
  iRadiusX,iRadiusY: NativeInt;
  weightFactor: NativeUInt;

  { Compute weights of pixels in a row }
  procedure ComputeBlurRow;
  var
    i: NativeInt;
    ofs: single;
  begin
    SetLength(blurRowX, 2*iRadiusX+1);
    if frac(radiusX)=0 then ofs := 1 else ofs := frac(radiusX);
    for i := 0 to iRadiusX do
    begin
      blurRowX[i] := round((i+ofs)*weightFactor);
      blurRowX[high(blurRowX)-i] := blurRowX[i];
    end;
    SetLength(blurRowY, 2*iRadiusY+1);
    if frac(radiusY)=0 then ofs := 1 else ofs := frac(radiusY);
    for i := 0 to iRadiusY do
    begin
      blurRowY[i] := round((i+ofs)*weightFactor);
      blurRowY[high(blurRowY)-i] := blurRowY[i];
    end;
  end;


var
  srcDelta,
  verticalWeightShift, horizontalWeightShift: NativeInt;
  ys1,ys2: NativeInt;

  { Compute blur result in a vertical direction }
  procedure ComputeVerticalRow(psrc: PBGRAPixel; var sums: TRowSum; pw: PNativeUInt; count: NativeInt);
  var w: NativeUInt;
      c: DWord;
  begin
    while count > 0 do
    with sums do
    begin
      dec(count);
      w := pw^; //apply pixel weight
      inc(pw);
      c := PDWord(psrc)^;
      inc(PByte(psrc),srcDelta);
      aDiv += w;
      w *= ((c shr TBGRAPixel_AlphaShift) and $ff);
      sumA += w;
      w := w shr verticalWeightShift;
      rgbDiv += w;
      {$hints off}
      sumR += ((c shr TBGRAPixel_RedShift) and $ff)*w;
      sumG += ((c shr TBGRAPixel_GreenShift) and $ff)*w;
      sumB += ((c shr TBGRAPixel_BlueShift) and $ff)*w;
      {$hints on}
    end;
  end;

var
  psum, psumEnd: PRowSum;
  sums: packed array of TRowSum;
  sumStartIndex: NativeInt;
  total: TRowSum;
  extendedTotal: TExtendedRowSum;
  yb,xb,xs,x,xEnd: NativeInt;
  w: NativeUInt;
  pw: PNativeUInt;
  psrc,pdest: PBGRAPixel;
  bmpWidth,bmpHeight : NativeInt;
  accumulationFactor: double;
  bounds: TRect;
  highSum: NativeInt;
  tempDest: TBGRACustomBitmap;

begin
  radiusX := round(radiusX*10)*0.1;
  radiusY := round(radiusY*10)*0.1;
  if (radiusX <= 0) and (radiusY <= 0) then
  begin
    ADestination.PutImage(0,0,bmp,dmSet);
    exit;
  end;
  iRadiusX := ceil(radiusX);
  iRadiusY := ceil(radiusY);
  if (frac(radiusX)=0) and (frac(radiusY)=0) then
    weightFactor:= 1
  else
    weightFactor:= 10;
  bmpWidth := bmp.Width;
  bmpHeight := bmp.Height;
  //create output
  if (ADestination.Width <> bmp.Width) or (ADestination.Height <> bmp.Height) then
    raise exception.Create('Dimension mismatch');
  bounds := bmp.GetImageBounds;
  if IsRectEmpty(bounds) then exit;
  bounds.Left   := max(0, bounds.Left - iRadiusX);
  bounds.Top    := max(0, bounds.Top - iRadiusY);
  bounds.Right  := min(bmp.Width, bounds.Right + iRadiusX);
  bounds.Bottom := min(bmp.Height, bounds.Bottom + iRadiusY);
  if not IntersectRect(bounds,bounds,ABounds) then exit;

  if radiusX*radiusY >= 100 then
  begin
    tempDest := ADestination.NewBitmap(ADestination.Width,ADestination.Height);
    FilterBlurBox(bmp,bounds,radiusX/3.2,radiusY/3.2,tempDest);
    FilterBlurBox(tempDest,bounds,radiusX/2.9,radiusY/2.9,ADestination);
    FilterBlurBox(ADestination,bounds,radiusX/3.2,radiusY/3.2,tempDest);
    FilterBlurBox(tempDest,bounds,radiusX/2.3,radiusY/2.3,ADestination, ACheckShouldStop);
    tempDest.Free;
    exit;
  end;

  accumulationFactor := (iRadiusY+2)*(iRadiusY+1) div 2 + (iRadiusY+1)*iRadiusY div 2;
  accumulationFactor *= sqr(weightFactor);
  verticalWeightShift := 0;
  while accumulationFactor > (high(NativeUInt) shr 16) + 1 do
  begin
    inc(verticalWeightShift);
    accumulationFactor *= 0.5;
  end;
  horizontalWeightShift:= 0;
  accumulationFactor *= ((iRadiusX+2)*(iRadiusX+1) div 2 + (iRadiusX+1)*iRadiusX div 2);
  accumulationFactor *= sqr(weightFactor);
  while accumulationFactor > (high(NativeUInt) shr 16) + 1 do
  begin
    inc(horizontalWeightShift);
    accumulationFactor *= 0.5;
  end;
  ComputeBlurRow;
  //current vertical sums
  setlength(sums, 2*iRadiusX+1);
  highSum := high(Sums);
  psumEnd := @sums[highSum];
  inc(psumEnd);
  if bmp.LineOrder = riloTopToBottom then
    srcDelta := bmpWidth*sizeof(TBGRAPixel) else
      srcDelta := -bmpWidth*sizeof(TBGRAPixel);

  xEnd := bounds.left-iRadiusX+highSum;
  if xEnd >= bmpWidth then xEnd := bmpWidth-1;
  //loop through destination bitmap
  for yb := bounds.top to bounds.bottom-1 do
  begin
    if (ACheckShouldStop <> nil) and ACheckShouldStop(yb) then break;
    //evalute available vertical range
    if yb - iRadiusY < 0 then
      ys1 := iRadiusY - yb
    else
      ys1 := 0;
    if yb + iRadiusY >= bmpHeight then
      ys2 := bmpHeight-1 - yb + iRadiusY
    else
      ys2 := 2*iRadiusY;

    { initial vertical rows are computed here. Later,
      for each pixel, vertical sums are shifted, so there
      is only one vertical sum to calculate }
    fillchar(sums[0],sizeof(TRowSum)*length(sums),0);
    x := bounds.left-iRadiusX;
    if x < 0 then
    begin
      xs := -x;
      x := 0;
    end else
      xs := 0;
    psrc := bmp.ScanLine[yb-iRadiusY+ys1]+x;
    psum := @sums[xs];
    pw := @blurRowY[ys1];
    while true do
    begin
      ComputeVerticalRow(psrc,psum^,pw,ys2-ys1+1);
      inc(x);
      inc(psrc);
      if x > xEnd then break;
      inc(psum);
    end;
    sumStartIndex := 0;

    pdest := ADestination.scanline[yb]+bounds.left;
    for xb := bounds.left to bounds.right-1 do
    begin
      //add vertical rows
      pw := @blurRowX[0];
      psum := @sums[sumStartIndex];
      if horizontalWeightShift > 4 then
      begin //we don't want to loose too much precision
        fillchar({%H-}extendedTotal,sizeof(extendedTotal),0);
        for xs := highSum downto 0 do
        with psum^ do
        begin
          w := pw^;
          inc(pw);
          extendedTotal.sumA += TExtendedRowValue(sumA)*w;
          extendedTotal.aDiv += TExtendedRowValue(aDiv)*w;
          extendedTotal.sumR += TExtendedRowValue(sumR)*w;
          extendedTotal.sumG += TExtendedRowValue(sumG)*w;
          extendedTotal.sumB += TExtendedRowValue(sumB)*w;
          extendedTotal.rgbDiv += TExtendedRowValue(rgbDiv)*w;
          inc(psum);
          if psum >= psumEnd then pSum := @sums[0];
        end;
        if (extendedTotal.aDiv > 0) and (extendedTotal.rgbDiv > 0) then
          pdest^:= ComputeExtendedAverage(extendedTotal)
        else
          pdest^:= BGRAPixelTransparent;
      end else
      if horizontalWeightShift > 0 then
      begin //lossy but efficient way
        fillchar({%H-}total,sizeof(total),0);
        for xs := highSum downto 0 do
        with psum^ do
        begin
          w := pw^;
          inc(pw);
          total.sumA += sumA*w shr horizontalWeightShift;
          total.aDiv += aDiv*w shr horizontalWeightShift;
          total.sumR += sumR*w shr horizontalWeightShift;
          total.sumG += sumG*w shr horizontalWeightShift;
          total.sumB += sumB*w shr horizontalWeightShift;
          total.rgbDiv += rgbDiv*w shr horizontalWeightShift;
          inc(psum);
          if psum >= psumEnd then pSum := @sums[0];
        end;
        if (total.aDiv > 0) and (total.rgbDiv > 0) then
          pdest^:= ComputeClampedAverage(total)
        else
          pdest^:= BGRAPixelTransparent;
      end else
      begin //normal way
        {$hints off}
        fillchar(total,sizeof(total),0);
        {$hints on}
        for xs := highSum downto 0 do
        with psum^ do
        begin
          w := pw^;
          inc(pw);
          total.sumA += sumA*w;
          total.aDiv += aDiv*w;
          total.sumR += sumR*w;
          total.sumG += sumG*w;
          total.sumB += sumB*w;
          total.rgbDiv += rgbDiv*w;
          inc(psum);
          if psum >= psumEnd then pSum := @sums[0];
        end;
        if (total.aDiv > 0) and (total.rgbDiv > 0) then
          pdest^ := ComputeAverage(total)
        else
          pdest^:= BGRAPixelTransparent;
      end;
      inc(pdest);
      //shift vertical rows
      psum := @sums[sumStartIndex];
      fillchar(psum^,sizeof(TRowSum),0);
      if x < bmpWidth then
      begin
        ComputeVerticalRow(psrc,psum^,@blurRowY[ys1],ys2-ys1+1);
        inc(x);
        inc(psrc);
      end;
      inc(sumStartIndex);
      if sumStartIndex > highSum then sumStartIndex := 0;
    end;
  end;
  ADestination.InvalidateBitmap;
end;

