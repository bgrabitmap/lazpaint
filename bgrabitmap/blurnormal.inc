var
  maskWidth,maskHeight: integer;
  blurOfs:      TPoint;
  PixelWeight:  array of integer;
  PixelOfs:     array of TPoint;
  PixelArrayLineStart: array of integer;
  DiffPixelWeight:  array of integer;
  DiffPixelOfs:     array of TPoint;
  DiffPixelArrayLineStart: array of integer;

  procedure LoadMask;
  var x,y,n: integer;
      tempWeight: integer;
      diffMask: array of array of integer;
  begin
    blurOfs := point(blurMask.Width shr 1, blurMask.Height shr 1);

    //count number of non empty pixels
    maskWidth := blurMask.Width;
    maskHeight := blurMask.Height;
    n := 0;
    for y := 0 to maskHeight - 1 do
      for x := 0 to maskWidth - 1 do
        if blurMask.GetPixel(x, y).red <> 0 then Inc(n);

    //initialize arrays
    setlength(diffMask, maskHeight, maskWidth+1);
    for y := 0 to maskHeight - 1 do
      for x := 0 to maskWidth do
        diffMask[y,x] := 0;

    setlength(PixelWeight, n);
    setlength(PixelOfs, n);
    setlength(PixelArrayLineStart, maskHeight+1);  //stores the first pixel of each line
    n := 0;
    //compute mask variations and initial mask pixel list
    for y := 0 to maskHeight - 1 do
    begin
      PixelArrayLineStart[y] := n;
      for x := 0 to maskWidth - 1 do
      begin
        tempWeight := blurMask.GetPixel(x, y).red;
        diffMask[y,x] -= tempWeight;
        diffMask[y,x+1] += tempWeight;

        if tempWeight <> 0 then
        begin
          PixelWeight[n] := tempWeight;
          PixelOfs[n] := Point(x,y);
          Inc(n);
        end;
      end;
    end;
    PixelArrayLineStart[maskHeight] := n;

    //count number of diff pixels
    n := 0;
    for y := 0 to maskHeight - 1 do
      for x := 0 to maskWidth do
        if diffMask[y,x] <> 0 then Inc(n);

    //initialize arrays
    setlength(DiffPixelWeight, n);
    setlength(DiffPixelOfs, n);
    setlength(DiffPixelArrayLineStart, maskHeight+1);  //stores the first pixel of each diff line
    n := 0;
    //compute diff pixel list
    for y := 0 to maskHeight - 1 do
    begin
      DiffPixelArrayLineStart[y] := n;
      for x := 0 to maskWidth do
      begin
        tempWeight := diffMask[y,x];
        if tempWeight <> 0 then
        begin
          DiffPixelWeight[n] := tempWeight;
          DiffPixelOfs[n] := Point(x-1,y);
          Inc(n);
        end;
      end;
    end;
    DiffPixelArrayLineStart[maskHeight] := n;
  end;

var
  curScans: array of PBGRAPixel;
  bounds: TRect;

  {procedure ShowCurScans;
  var str: string;
    i: Integer;
  begin
    str := '';
    for i := 0 to high(curScans) do
    begin
      if i <> 0 then str += ', ';
      if curScans[i]=nil then str += 'nil' else
        str += 'bmp['+inttostr(curScans[i]-bmp.Data)+']';
    end;
    ShowMessage(str);
  end;}

  function PrepareScan: boolean;
  var
    bmpY: integer;
    y   : Integer;
  begin
    //evaluate required bounds taking blur radius into acount
    bounds := bmp.GetImageBounds;
    if IsRectEmpty(bounds) then
    begin
      result := false;
      exit;
    end;
    bounds.Left   := max(0, bounds.Left - blurOfs.X);
    bounds.Top    := max(0, bounds.Top - blurOfs.Y);
    bounds.Right  := min(bmp.Width, bounds.Right + maskWidth - 1 - blurOfs.X);
    bounds.Bottom := min(bmp.Height, bounds.Bottom + maskHeight - 1 - blurOfs.Y);
    if not IntersectRect(bounds, bounds, ABounds) then
    begin
      result := false;
      exit;
    end;

    //init scanlines
    setlength(curScans, maskHeight);
    for y := 0 to maskHeight-1 do
    begin
      bmpY := y+bounds.Top-blurOfs.Y;
      if (bmpY < 0) or (bmpY >= bmp.Height) then
        curScans[y] := nil else
          curScans[y] := bmp.ScanLine[bmpY];
    end;
    //ShowCurScans;
    result := true;
  end;

  procedure ShiftScan(NewY: integer); inline;
  var y: integer;
  begin
    for y := 0 to maskHeight-2 do
     curScans[y] := curScans[y+1];

    //get next scanline
    if newY >= bmp.Height then
      curScans[maskHeight-1] := nil
    else
      curScans[maskHeight-1] := bmp.ScanLine[newY];
    //ShowCurScans;
  end;

var
  yb, xb: integer;
  mindy, maxdy, n: integer;
  bmpWidth,bmpX: integer;
  pixMaskAlpha, maskAlpha: integer;
  tempPixel: TBGRAPixel;
  pdest : PBGRAPixel;
  pt: TPoint;

begin
  LoadMask;

  if (ADestination.Width <> bmp.Width) or (ADestination.Height <> bmp.Height) then
    raise exception.Create('Dimension mismatch');

  if not PrepareScan then exit; //nothing to do

  bmpWidth := bmp.Width;
  //loop through destination
  for yb := bounds.Top to bounds.Bottom - 1 do
  begin
    if (ACheckShouldStop <> nil) and ACheckShouldStop(yb) then break;
    pdest := ADestination.ScanLine[yb] + bounds.Left;
    //compute vertical range
    mindy := max(-blurOfs.Y, -yb);
    maxdy := min(blurMask.Height - 1 - blurOfs.Y, bmp.Height - 1 - yb);

    sumR   := 0;
    sumG   := 0;
    sumB   := 0;
    sumA   := 0;
    Adiv   := 0;
    {$ifdef PARAM_MASKSHIFT}
    RGBdiv := 0;
    {$endif}

    //go through pixel list of the current vertical range
    for n := PixelArrayLineStart[mindy+blurOfs.Y] to PixelArrayLineStart[maxdy+blurOfs.Y+1]-1 do
    begin
      pt := PixelOfs[n];
      bmpX := bounds.Left-blurOfs.X+pt.x;
      //check horizontal range
      if (bmpX >= 0) and (bmpX < bmpWidth) then
      begin
        tempPixel := (curScans[pt.y]+bmpX)^;
        maskAlpha := PixelWeight[n];
        pixMaskAlpha := maskAlpha * tempPixel.alpha;
        sumA    += pixMaskAlpha;
        Adiv    += maskAlpha;
        {$ifdef PARAM_MASKSHIFT}
        pixMaskAlpha := pixMaskAlpha shr maskShift;
        RGBdiv  += pixMaskAlpha;
        {$endif}
        {$hints off}
        sumR    += tempPixel.red * pixMaskAlpha;
        sumG    += tempPixel.green * pixMaskAlpha;
        sumB    += tempPixel.blue * pixMaskAlpha;
        {$hints on}
      end;
    end;

    for xb := bounds.Left to Bounds.Right - 1 do
    begin
      if xb > bounds.left then
      begin
        for n := DiffPixelArrayLineStart[mindy+blurOfs.Y] to DiffPixelArrayLineStart[maxdy+blurOfs.Y+1]-1 do
        begin
          pt := DiffPixelOfs[n];
          bmpX := xb-blurOfs.X+pt.x;
          if (bmpX >= 0) and (bmpX < bmpWidth) then
          begin
            tempPixel := (curScans[pt.y]+bmpX)^;
            maskAlpha := DiffPixelWeight[n];
            pixMaskAlpha := maskAlpha * tempPixel.alpha;
            sumA    += pixMaskAlpha;
            Adiv    += maskAlpha;
            {$ifdef PARAM_MASKSHIFT}
            pixMaskAlpha := (cardinal(pixMaskAlpha)+$80000000) shr maskShift - ($80000000 shr maskShift);
            RGBdiv  += pixMaskAlpha;
            {$endif}
            {$hints off}
            sumR    += tempPixel.red * pixMaskAlpha;
            sumG    += tempPixel.green * pixMaskAlpha;
            sumB    += tempPixel.blue * pixMaskAlpha;
            {$hints on}
          end;
        end;
      end;

      //compute average
      if (Adiv <= 0) {$ifdef PARAM_MASKSHIFT} or (RGBdiv <= 0) {$endif} then
        pdest^ := BGRAPixelTransparent
      else
        pdest^ := computeAverage;

      Inc(pdest);
    end;

    ShiftScan(yb-blurOfs.Y+maskHeight);
  end;
  ADestination.InvalidateBitmap;
end;
{$undef PARAM_MASKSHIFT}

