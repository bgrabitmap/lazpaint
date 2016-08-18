procedure FastBlendPixels(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer);
begin
  while Count > 0 do
      begin
        FastBlendPixelInline(pdest, psrc^);
        Inc(pdest);
        Inc(psrc);
        Dec(Count);
      end;
end;

procedure DrawTransparentPixels(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer);
begin
  while Count > 0 do
        begin
          DrawPixelInlineWithAlphaCheck(pdest, psrc^);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure LinearMultiplyPixels(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer);
begin
  while Count > 0 do
        begin
          LinearMultiplyPixelInline(pdest, psrc^);  //same look with non linear
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure AddPixels(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer);
begin
  while Count > 0 do
        begin
          AddPixelInline(pdest, psrc^);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure LinearAddPixels(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer);
begin
  while Count > 0 do
        begin
          LinearAddPixelInline(pdest, psrc^);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure ColorBurnPixels(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer);
begin
  while Count > 0 do
      begin
        ColorBurnPixelInline(pdest, psrc^);
        Inc(pdest);
        Inc(psrc);
        Dec(Count);
      end;
end;

procedure ColorDodgePixels(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer);
begin
  while Count > 0 do
        begin
          ColorDodgePixelInline(pdest, psrc^);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure DividePixels(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer);
begin
  while Count > 0 do
      begin
        DividePixelInline(pdest, psrc^);
        Inc(pdest);
        Inc(psrc);
        Dec(Count);
      end;
end;

procedure ReflectPixels(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer);
begin
  while Count > 0 do
        begin
          ReflectPixelInline(pdest, psrc^);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure GlowPixels(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer);
begin
  while Count > 0 do
        begin
          GlowPixelInline(pdest, psrc^);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure NiceGlowPixels(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer);
begin
  while Count > 0 do
        begin
          NiceGlowPixelInline(pdest, psrc^);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure OverlayPixels(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer);
begin
  while Count > 0 do
        begin
          OverlayPixelInline(pdest, psrc^);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure LinearOverlayPixels(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer);
begin
  while Count > 0 do
        begin
          LinearOverlayPixelInline(pdest, psrc^);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure DifferencePixels(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer);
begin
  while Count > 0 do
        begin
          DifferencePixelInline(pdest, psrc^);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure LinearDifferencePixels(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer);
begin
  while Count > 0 do
        begin
          LinearDifferencePixelInline(pdest, psrc^);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure ExclusionPixels(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer);
begin
  while Count > 0 do
      begin
        ExclusionPixelInline(pdest, psrc^);
        Inc(pdest);
        Inc(psrc);
        Dec(Count);
      end;
end;

procedure LinearExclusionPixels(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer);
begin
  while Count > 0 do
        begin
          LinearExclusionPixelInline(pdest, psrc^);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure LinearSubtractPixels(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer);
begin
  while Count > 0 do
        begin
          LinearSubtractPixelInline(pdest, psrc^);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure LinearSubtractInversePixels(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer);
begin
  while Count > 0 do
        begin
          LinearSubtractInversePixelInline(pdest, psrc^);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure SubtractPixels(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer);
begin
  while Count > 0 do
        begin
          SubtractPixelInline(pdest, psrc^);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure SubtractInversePixels(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer);
begin
  while Count > 0 do
        begin
          SubtractInversePixelInline(pdest, psrc^);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure NegationPixels(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer);
begin
  while Count > 0 do
        begin
          NegationPixelInline(pdest, psrc^);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure LinearNegationPixels(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer);
begin
  while Count > 0 do
        begin
          LinearNegationPixelInline(pdest, psrc^);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure LightenPixels(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer);
begin
  while Count > 0 do
        begin
          LightenPixelInline(pdest, psrc^);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure DarkenPixels(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer);
begin
  while Count > 0 do
        begin
          DarkenPixelInline(pdest, psrc^);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure ScreenPixels(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer);
begin
  while Count > 0 do
      begin
        ScreenPixelInline(pdest, psrc^);
        Inc(pdest);
        Inc(psrc);
        Dec(Count);
      end;
end;

procedure SoftLightPixels(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer);
begin
  while Count > 0 do
        begin
          SoftLightPixelInline(pdest, psrc^);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure SvgSoftLightPixels(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer);
begin
  while Count > 0 do
        begin
          SvgSoftLightPixelInline(pdest, psrc^);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure HardLightPixels(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer);
begin
  while Count > 0 do
        begin
          HardLightPixelInline(pdest, psrc^);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure BlendXorPixels(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer);
begin
  while Count > 0 do
        begin
          BlendXorPixelInline(pdest, psrc^);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

type
  TBlendPixelsProc = procedure(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer);

const
  BlendPixelsProc: array[TBlendOperation] of TBlendPixelsProc =
    ( @FastBlendPixels, @DrawTransparentPixels,
      @LightenPixels, @ScreenPixels, @AddPixels, @LinearAddPixels, @ColorDodgePixels, @DividePixels, @NiceGlowPixels, @SoftLightPixels, @HardLightPixels,
      @GlowPixels, @ReflectPixels, @LinearOverlayPixels, @OverlayPixels, @DarkenPixels, @LinearMultiplyPixels, @ColorBurnPixels,
      @DifferencePixels, @LinearDifferencePixels, @ExclusionPixels, @LinearExclusionPixels, @SubtractPixels, @LinearSubtractPixels,
      @SubtractInversePixels, @LinearSubtractInversePixels, @NegationPixels, @LinearNegationPixels, @BlendXorPixels, @SvgSoftLightPixels);

procedure BlendPixels(pdest: PBGRAPixel; psrc: PBGRAPixel;
  blendOp: TBlendOperation; Count: integer);
begin
  BlendPixelsProc[blendOp](pdest,psrc,count);
end;

