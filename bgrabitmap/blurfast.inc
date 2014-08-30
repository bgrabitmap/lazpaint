
var
  blurRow: array of cardinal;

  { Compute weights of pixels in a row }
  procedure ComputeBlurRow;
  var
    i: Integer;
  begin
    SetLength(blurRow, 2*radius+1);
    for i := 0 to radius do
    begin
      blurRow[i] := i+1;
      blurRow[high(blurRow)-i] := blurRow[i];
    end;
  end;


var
  srcDelta,
  weightShift: integer;

  { Compute blur result in a vertical direction }
  procedure ComputeVerticalRow(psrc: PBGRAPixel; var sums: TRowSum; ys1,ys2: integer); inline;
  var ys: integer;
      c: TBGRAPixel;
      w,aw: cardinal;
  begin
    for ys := ys1 to ys2 do
    with sums do
    begin
      c := psrc^;
      w := blurRow[ys]; //apply pixel weight
      aw := c.alpha*w;
      sumA += aw;
      aDiv += w;

      aw := aw shr weightShift;
      {$hints off}
      sumR += c.red*aw;
      sumG += c.green*aw;
      sumB += c.blue*aw;
      rgbDiv += aw;
      {$hints on}
      inc(psrc,srcDelta);
    end;
  end;

var
  sums: array of TRowSum;
  sumStartIndex,curIndex: integer;
  total: TRowSum;
  yb,xb,xs,ys1,ys2,x: integer;
  w: cardinal;
  pdest: PBGRAPixel;
  bmpWidth,bmpHeight : integer;
  radiusSquare: integer;
  bounds: TRect;

begin
  if radius = 0 then
  begin
    ADestination.PutImage(0,0,bmp,dmSet);
    exit;
  end;
  bmpWidth := bmp.Width;
  bmpHeight := bmp.Height;
  //create output
  if (ADestination.Width <> bmp.Width) or (ADestination.Height <> bmp.Height) then
    raise exception.Create('Dimension mismatch');
  bounds := bmp.GetImageBounds;
  if IsRectEmpty(bounds) then exit;
  bounds.Left   := max(0, bounds.Left - radius);
  bounds.Top    := max(0, bounds.Top - radius);
  bounds.Right  := min(bmp.Width, bounds.Right + radius);
  bounds.Bottom := min(bmp.Height, bounds.Bottom + radius);
  if not IntersectRect(bounds,bounds,ABounds) then exit;

  radiusSquare := sqr((radius+1)*(radius+2) div 2);
  weightShift := 0;
  while radiusSquare > 16384 do //18496 do
  begin
    radiusSquare := radiusSquare shr 1;
    inc(weightShift);
  end;
  ComputeBlurRow;
  //current vertical sums
  setlength(sums, 2*radius+1);
  if bmp.LineOrder = riloTopToBottom then
    srcDelta := bmpWidth else
      srcDelta := -bmpWidth;
  //loop through destination bitmap
  for yb := bounds.top to bounds.bottom-1 do
  begin
    if (ACheckShouldStop <> nil) and ACheckShouldStop(yb) then break;
    //evalute available vertical range
    if yb - radius < 0 then
      ys1 := radius - yb
    else
      ys1 := 0;
    if yb + radius >= bmpHeight then
      ys2 := bmpHeight - yb + radius - 1
    else
      ys2 := high(sums);

    { initial vertical rows are computed here. Later,
      for each pixel, vertical sums are shifted, so there
      is only one vertical sum to calculate }
    for xs := 0 to high(sums) do
    begin
      fillchar(sums[xs],sizeof(TRowSum),0);
      x := bounds.left-radius+xs;
      if (x >= 0) and (x < bmpWidth) then
        ComputeVerticalRow(bmp.ScanLine[yb-radius+ys1]+x,sums[xs],ys1,ys2);
    end;
    sumStartIndex := 0;

    pdest := ADestination.scanline[yb]+bounds.left;
    for xb := bounds.left to bounds.right-1 do
    begin
      //add vertical rows
      {$hints off}
      fillchar(total,sizeof(total),0);
      {$hints on}
      curIndex:= sumStartIndex;
      for xs := 0 to high(sums) do
      with sums[curIndex] do
      begin
        w := blurRow[xs];
        total.sumA += sumA*w;
        total.aDiv += aDiv*w;
        total.sumR += sumR*w;
        total.sumG += sumG*w;
        total.sumB += sumB*w;
        total.rgbDiv += rgbDiv*w;
        inc(curIndex);
        if curIndex = length(sums) then curIndex := 0;
      end;
      if (total.aDiv > 0) and (total.rgbDiv > 0) then
        pdest^:= ComputeAverage(total)
      else
        pdest^:= BGRAPixelTransparent;
      inc(pdest);
      //shift vertical rows
      fillchar(sums[sumStartIndex],sizeof(TRowSum),0);
      x := xb+1-radius+high(sums);
      if (x >= 0) and (x < bmpWidth) then
        ComputeVerticalRow(bmp.ScanLine[yb-radius+ys1]+x,sums[sumStartIndex],ys1,ys2);
      inc(sumStartIndex);
      if sumStartIndex = length(sums) then sumStartIndex := 0;
    end;
  end;
  ADestination.InvalidateBitmap;
end;

