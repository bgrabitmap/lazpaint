{$IFDEF INCLUDE_FILLDENSITY}
{$UNDEF INCLUDE_FILLDENSITY}

	{$IFNDEF PARAM_SINGLESEGMENT}
	begin
	  { this loops fill one scanline of densities by adding 256 for full horizontal pixels }

	  for i := 0 to nbinter div 2 - 1 do
	  begin
		x1 := inter[i + i].interX;
		x2 := inter[i + i + 1].interX;
	{$ENDIF}

		if (x1 <> x2) and (x1 < maxx + 1) and (x2 >= minx) then
		begin
		  if x1 < minx then
			x1 := minx;
		  if x2 >= maxx + 1 then
			x2 := maxx + 1;
		  ix1  := floor(x1);
		  ix2  := floor(x2);

		  //here it may go one pixel further if x2 is an integer
		  if ix1 < densMinx then densMinx := ix1;
		  if ix2 > densMaxx then densMaxx := ix2;

		  if ix1 = ix2 then
			(density + (ix1 - minx))^ += round((x2-ix2)*256) - round((x1-ix1)*256)
		  else
		  begin
			(density + (ix1 - minx))^ += 256 - round((x1 - ix1)*256);
			if (ix2 <= maxx) then
			  (density + (ix2 - minx))^ += round((x2 - ix2)*256);
		  end;
		  if ix2 > ix1 + 1 then
		  begin
			AddDensity(density, ix1+1 - minx, ix2-(ix1+1), 256);
		  end;
		end;
	
	{$IFNDEF PARAM_SINGLESEGMENT}
	  end;
	end;
	{$ENDIF}
	
{$UNDEF PARAM_SINGLESEGMENT}
{$ENDIF}

{$IFDEF INCLUDE_RENDERDENSITY}
{$UNDEF INCLUDE_RENDERDENSITY}
  begin
    if densMinX <= densMaxX then
    begin
      if densMinX < minx then densMinX := minx;
      if densMaxX > maxx then densMaxX := maxx;

      pdest := bmp.ScanLine[yb] + densMinX;
      pdens := density + (densMinX-minx);
      //render scanline
      if scan <> nil then //with texture scan
      begin
        scan.ScanMoveTo(densMinX,yb);
        if MemScanCopy <> nil then
        begin
          scan.ScanPutPixels(MemScanCopy,densMaxX-densMinX+1,dmSet);
          pscan := MemScanCopy;
          for xb := densMinX to densMaxX do
          begin
            tempDensity := pdens^;
            Inc(pdens);
            if tempDensity >= 256 {$ifdef PARAM_ANTIALIASINGFACTOR}shl AntialiasPrecisionShift{$endif} then
              {$ifdef PARAM_LINEARANTIALIASING}
              FastBlendPixelInline
              {$else}
              DrawPixelInlineWithAlphaCheck{$endif}(pdest, pscan^) else
            if tempDensity <> 0 then
            begin
              c := pscan^;
              {$ifdef PARAM_LINEARANTIALIASING}
              FastBlendPixelInline
              {$else}
              DrawPixelInlineWithAlphaCheck{$endif}(pdest, BGRA(c.red, c.green, c.blue,
              {$ifdef PARAM_ANTIALIASINGFACTOR}DivByAntialiasPrecision256{$endif}
              (c.alpha * tempDensity
              {$ifdef PARAM_ANTIALIASINGFACTOR} ) {$else} +128) shr 8 {$endif}
              ));
            end;
            inc(pscan);
            Inc(pdest);
          end;
        end else
        begin
          for xb := densMinX to densMaxX do
          begin
            tempDensity := pdens^;
            Inc(pdens);
            c := ScanNextPixelProc();
            if tempDensity <> 0 then
              {$ifdef PARAM_LINEARANTIALIASING}
              FastBlendPixelInline
              {$else}
              DrawPixelInlineWithAlphaCheck{$endif}(pdest, BGRA(c.red, c.green, c.blue,
              {$ifdef PARAM_ANTIALIASINGFACTOR}DivByAntialiasPrecision256{$endif}
              (c.alpha * tempDensity
              {$ifdef PARAM_ANTIALIASINGFACTOR} ) {$else} +128) shr 8 {$endif}
              ));
            Inc(pdest);
          end;
        end;

      end else
      if EraseMode then //erase with alpha
      begin
        for xb := densMinX to densMaxX do
        begin
          tempDensity := pdens^;
          Inc(pdens);
          if tempDensity <> 0 then
            ErasePixelInline(pdest,
            {$ifdef PARAM_ANTIALIASINGFACTOR}DivByAntialiasPrecision256{$endif}
            (c.alpha * tempDensity
            {$ifdef PARAM_ANTIALIASINGFACTOR} ) {$else} +128) shr 8 {$endif}
            );
          Inc(pdest);
        end;
      end
      else
      begin  //solid color
        for xb := densMinX to densMaxX do
        begin
          tempDensity := pdens^;
          Inc(pdens);
          if tempDensity <> 0 then
          begin
            c2.alpha :=
              {$ifdef PARAM_ANTIALIASINGFACTOR}DivByAntialiasPrecision256{$endif}
              (c.alpha * tempDensity
              {$ifdef PARAM_ANTIALIASINGFACTOR} ) {$else} +128) shr 8 {$endif}
              ;
            {$ifdef PARAM_LINEARANTIALIASING}
            FastBlendPixelInline(pdest, c2);
            {$else}
            DrawPixelInlineExpandedOrNotWithAlphaCheck(pdest, ec, c2);
            {$endif}
          end;
          Inc(pdest);
        end;
      end;
    end;
  end
  
{$undef PARAM_ANTIALIASINGFACTOR}
{$undef PARAM_LINEARANTIALIASING}
{$ENDIF}
