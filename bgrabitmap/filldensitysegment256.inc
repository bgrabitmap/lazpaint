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

