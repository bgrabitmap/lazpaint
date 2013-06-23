begin
  { this loops fill one scanline of densities by adding 256 for full horizontal pixels }

  for i := 0 to nbinter div 2 - 1 do
  begin
    x1 := inter[i + i].interX;
    x2 := inter[i + i + 1].interX;
    {$i filldensitysegment256.inc}
  end;
end;
