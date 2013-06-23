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
            j := pdens^;
            Inc(pdens);
            c := pscan^;
            inc(pscan);
            if j <> 0 then
              DrawPixelInlineWithAlphaCheck(pdest, BGRA(c.red, c.green, c.blue,
              {$ifdef PARAM_ANTIALIASINGFACTOR}DivByAntialiasPrecision256{$endif}
              (c.alpha * j
              {$ifdef PARAM_ANTIALIASINGFACTOR} ) {$else} +128) shr 8 {$endif}
              ));
            Inc(pdest);
          end;
        end else
        begin
          for xb := densMinX to densMaxX do
          begin
            j := pdens^;
            Inc(pdens);
            c := ScanNextPixelProc();
            if j <> 0 then
              DrawPixelInlineWithAlphaCheck(pdest, BGRA(c.red, c.green, c.blue,
              {$ifdef PARAM_ANTIALIASINGFACTOR}DivByAntialiasPrecision256{$endif}
              (c.alpha * j
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
          j := pdens^;
          Inc(pdens);
          if j <> 0 then
            ErasePixelInline(pdest,
            {$ifdef PARAM_ANTIALIASINGFACTOR}DivByAntialiasPrecision256{$endif}
            (c.alpha * j
            {$ifdef PARAM_ANTIALIASINGFACTOR} ) {$else} +128) shr 8 {$endif}
            );
          Inc(pdest);
        end;
      end
      else
      begin  //solid color
        for xb := densMinX to densMaxX do
        begin
          j := pdens^;
          Inc(pdens);
          if j <> 0 then
          begin
            c2.alpha :=
              {$ifdef PARAM_ANTIALIASINGFACTOR}DivByAntialiasPrecision256{$endif}
              (c.alpha * j
              {$ifdef PARAM_ANTIALIASINGFACTOR} ) {$else} +128) shr 8 {$endif}
              ;
            DrawPixelInlineExpandedOrNotWithAlphaCheck(pdest, ec, c2);
          end;
          Inc(pdest);
        end;
      end;
    end;
  end
{$undef PARAM_ANTIALIASINGFACTOR}
