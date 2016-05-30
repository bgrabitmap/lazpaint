type
  PWeightedPixel = ^TWeightedPixel;
  TWeightedPixel = packed record
    Coord: TPoint;
    Weight: NativeInt;
    PtrOfs: NativeInt;
  end;

var
  maskWidth,maskHeight: integer;
  blurOfs: TPoint;
  ppixel: PWeightedPixel;
  Pixel: array of TWeightedPixel;
  PixelArrayLineStart: array of integer;
  DiffPixel: array of TWeightedPixel;
  DiffPixelArrayLineStart: array of integer;

  bmpWidth,bmpHeight,lineDelta: NativeInt;

  procedure LoadMask(out ABlurOfs: TPoint);
  var x,y,n,i: NativeInt;
      tempWeight: NativeInt;
      diffMask: array of packed array of NativeInt;
      p: PBGRAPixel;
  begin
    ABlurOfs := point(blurMask.Width shr 1, blurMask.Height shr 1);

    //count number of non empty pixels
    maskWidth := blurMask.Width;
    maskHeight := blurMask.Height;
    n := 0;
    p := blurMask.Data;
    for i := blurMask.NbPixels-1 downto 0 do
    begin
      if p^.red <> 0 then inc(n);
      inc(p);
    end;

    //initialize arrays
    setlength(diffMask, maskHeight, maskWidth+1);
    for y := 0 to maskHeight - 1 do
      fillchar(diffMask[y,0], (maskWidth+1)*sizeof(NativeInt), 0);

    setlength(Pixel, n);
    setlength(PixelArrayLineStart, maskHeight+1);  //stores the first pixel of each line
    n := 0;
    //compute mask variations and initial mask pixel list
    for y := 0 to maskHeight - 1 do
    begin
      PixelArrayLineStart[y] := n;
      p := blurMask.ScanLine[y];
      for x := 0 to maskWidth - 1 do
      begin
        tempWeight := p^.red;
        inc(p);
        diffMask[y,x] -= tempWeight;
        diffMask[y,x+1] += tempWeight;

        if tempWeight <> 0 then
        begin
          Pixel[n].Weight := tempWeight;
          Pixel[n].Coord := Point(x,y);
          Pixel[n].PtrOfs := (y-ABlurOfs.Y)*lineDelta + (x-ABlurOfs.X)*sizeof(TBGRAPixel);
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
    setlength(DiffPixel, n);
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
          DiffPixel[n].Weight := tempWeight;
          DiffPixel[n].Coord := Point(x-1,y);
          DiffPixel[n].PtrOfs := (y-ABlurOfs.Y)*lineDelta + (x-ABlurOfs.X-1)*sizeof(TBGRAPixel);
          Inc(n);
        end;
      end;
    end;
    DiffPixelArrayLineStart[maskHeight] := n;
  end;

  function PrepareScan(AWantedBounds: TRect; out AClippedBounds: TRect): boolean;
  begin
    //evaluate required bounds taking blur radius into acount
    AClippedBounds := bmp.GetImageBounds;
    if IsRectEmpty(AClippedBounds) then
    begin
      result := false;
      exit;
    end;
    AClippedBounds.Left   := max(0, AClippedBounds.Left - blurOfs.X);
    AClippedBounds.Top    := max(0, AClippedBounds.Top - blurOfs.Y);
    AClippedBounds.Right  := min(bmpWidth, AClippedBounds.Right + maskWidth - 1 - blurOfs.X);
    AClippedBounds.Bottom := min(bmpHeight, AClippedBounds.Bottom + maskHeight - 1 - blurOfs.Y);
    if not IntersectRect(AClippedBounds, AClippedBounds, AWantedBounds) then
    begin
      result := false;
      exit;
    end;

    result := true;
  end;

var
  bounds: TRect;
  yb, xb: NativeInt;
  mindy, maxdy, n, nStart, nCount, nDiffStart, nDiffCount: NativeInt;
  bmpX,bmpXBase,bmpYBase: NativeInt;
  pixMaskAlpha, maskAlpha: NativeInt;
  tempPixel: TBGRAPixel;
  pdest : PBGRAPixel;
  psrc : PByte;

begin
  bmpWidth := bmp.Width;
  bmpHeight:= bmp.Height;
  if bmp.LineOrder = riloTopToBottom then
    lineDelta := bmpWidth*sizeof(TBGRAPixel) else
    lineDelta := -bmpWidth*sizeof(TBGRAPixel);

  if (ADestination.Width <> bmpWidth) or (ADestination.Height <> bmpHeight) then
    raise exception.Create('Dimension mismatch');

  LoadMask(blurOfs);
  if not PrepareScan(ABounds, bounds) then exit; //nothing to do

  bmpYBase := bounds.Top - blurOfs.Y;

  //loop through destination
  for yb := bounds.Top to bounds.Bottom - 1 do
  begin
    if (ACheckShouldStop <> nil) and ACheckShouldStop(yb) then break;
    psrc := PByte(bmp.ScanLine[yb]+bounds.Left);
    pdest := ADestination.ScanLine[yb] + bounds.Left;
    //compute vertical range
    mindy := max(-blurOfs.Y, -yb);
    maxdy := min(blurMask.Height - 1 - blurOfs.Y, bmpHeight - 1 - yb);

    sumR   := 0;
    sumG   := 0;
    sumB   := 0;
    sumA   := 0;
    Adiv   := 0;
    {$ifdef PARAM_MASKSHIFT}
    RGBdiv := 0;
    {$endif}

    bmpXBase := bounds.Left-blurOfs.X;
    nStart := PixelArrayLineStart[mindy+blurOfs.Y];
    nCount  := PixelArrayLineStart[maxdy+blurOfs.Y+1]-nStart;
    ppixel:= @Pixel[nStart];
    //go through pixel list of the current vertical range
    for n := nCount-1 downto 0 do
    begin
      bmpX := bmpXBase+ppixel^.Coord.x;
      //check horizontal range
      if (bmpX >= 0) and (bmpX < bmpWidth) then
      begin
        tempPixel := PBGRAPixel(psrc + ppixel^.PtrOfs)^;
        maskAlpha := ppixel^.Weight;
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
      inc(ppixel);
    end;

    //compute average
    if (Adiv <= 0) {$ifdef PARAM_MASKSHIFT} or (RGBdiv <= 0) {$endif} then
      pdest^ := BGRAPixelTransparent
    else
      pdest^ := computeAverage;

    nDiffStart := DiffPixelArrayLineStart[mindy+blurOfs.Y];
    nDiffCount := DiffPixelArrayLineStart[maxdy+blurOfs.Y+1]-nDiffStart;

    if nDiffCount < nCount then
    begin
      for xb := bounds.Left+1 to Bounds.Right - 1 do
      begin
        Inc(pdest);
        inc(bmpXBase);
        inc(psrc,sizeof(TBGRAPixel));

        ppixel:= @DiffPixel[nDiffStart];
        for n := nDiffCount-1 downto 0 do
        begin
          bmpX := bmpXBase+ppixel^.Coord.x;
          if (bmpX >= 0) and (bmpX < bmpWidth) then
          begin
            tempPixel := PBGRAPixel(psrc + ppixel^.PtrOfs)^;
            maskAlpha := ppixel^.Weight;
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
          inc(ppixel);
        end;

        //compute average
        if (Adiv <= 0) {$ifdef PARAM_MASKSHIFT} or (RGBdiv <= 0) {$endif} then
          pdest^ := BGRAPixelTransparent
        else
          pdest^ := ComputeAverage;
      end;
    end else
    begin
      for xb := bounds.Left+1 to Bounds.Right - 1 do
      begin
        Inc(pdest);
        inc(bmpXBase);
        inc(psrc,sizeof(TBGRAPixel));

        sumR   := 0;
        sumG   := 0;
        sumB   := 0;
        sumA   := 0;
        Adiv   := 0;
        {$ifdef PARAM_MASKSHIFT}
        RGBdiv := 0;
        {$endif}

        ppixel:= @Pixel[nStart];
        for n := nCount-1 downto 0 do
        begin
          bmpX := bmpXBase+ppixel^.Coord.x;
          //check horizontal range
          if (bmpX >= 0) and (bmpX < bmpWidth) then
          begin
            tempPixel := PBGRAPixel(psrc + ppixel^.PtrOfs)^;
            maskAlpha := ppixel^.Weight;
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
          inc(ppixel);
        end;

        //compute average
        if (Adiv <= 0) {$ifdef PARAM_MASKSHIFT} or (RGBdiv <= 0) {$endif} then
          pdest^ := BGRAPixelTransparent
        else
          pdest^ := computeAverage;
      end;
    end;

    inc(bmpYBase);
  end;
  ADestination.InvalidateBitmap;
end;
{$undef PARAM_MASKSHIFT}

