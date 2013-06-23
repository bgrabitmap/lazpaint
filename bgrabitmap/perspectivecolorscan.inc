    {$IFDEF PARAM_USESSE} {$asmmode intel}
      minVal := 0;
      maxVal := 65535;
      asm
        movss xmm6, invZ
        shufps xmm6,xmm6,0  //xmm6 = invZ
        movss xmm7, invZStep
        shufps xmm7,xmm7,0 //xmm7 = invZStep
        movups xmm4, colorPos
        movups xmm5, colorStep

        movss xmm2, minVal
        shufps xmm2,xmm2,0 //xmm2 = minVal
        movss xmm3, maxVal
        shufps xmm3,xmm3,0 //xmm3 = maxVal
      end;

      for i := ix1 to ix2 do
      begin
        {$IFDEF PARAM_USEZBUFFER}
        if invZ > zbufferpos^ then
        {$ENDIF}
        begin
          {$IFDEF PARAM_USEZBUFFER}
          zbufferpos^ := invz;
          {$ENDIF}
          asm
            movaps xmm0,xmm6
            rcpps xmm0,xmm0
            mulps xmm0, xmm4
            minps xmm0, xmm3
            maxps xmm0, xmm2
            {$IFDEF PARAM_USESSE2}
            cvtps2dq xmm0,xmm0
            movups cInt, xmm0
            {$ELSE}
            movups colorPosByZ, xmm0
            {$ENDIF}
          end;
          {$IFDEF PARAM_USESSE2}
          c.red := GammaCompressionTab[cInt.r];
          c.green := GammaCompressionTab[cInt.g];
          c.blue := GammaCompressionTab[cInt.b];
          c.alpha := GammaCompressionTab[cInt.a];
          DrawPixelInlineWithAlphaCheck(pdest, c);
          {$ELSE}
          ec.red := round(colorPosByZ[1]);
          ec.green := round(colorPosByZ[2]);
          ec.blue := round(colorPosByZ[3]);
          ec.alpha := round(colorPosByZ[4]);
          DrawPixelInlineWithAlphaCheck(pdest, GammaCompression(ec));
          {$ENDIF}
        end;
        asm
          addps xmm6,xmm7
          addps xmm4,xmm5
          {$IFDEF PARAM_USEZBUFFER}
          movss invZ,xmm6
          {$ENDIF}
        end;
        inc(pdest);
        {$IFDEF PARAM_USEZBUFFER}
        inc(zbufferpos);
        {$ENDIF}
      end;
    {$ELSE}
      for i := ix1 to ix2 do
      begin
        {$IFDEF PARAM_USEZBUFFER}
        if invZ > zbufferpos^ then
        {$ENDIF}
        begin
          {$IFDEF PARAM_USEZBUFFER}
          zbufferpos^ := invz;
          {$ENDIF}
          z := 1/invZ;
          r := round(z*colorPos[1]);
          g := round(z*colorPos[2]);
          b := round(z*colorPos[3]);
          a := round(z*colorPos[4]);
          if r < 0 then ec.red := 0 else
          if r > 65535 then ec.red := 65535
          else ec.red := r;
          if g < 0 then ec.green := 0 else
          if g > 65535 then ec.green := 65535
          else ec.green := g;
          if b < 0 then ec.blue := 0 else
          if b > 65535 then ec.blue := 65535
          else ec.blue := b;
          if a < 0 then ec.alpha := 0 else
          if a > 65535 then ec.alpha := 65535
          else ec.alpha := a;
          DrawPixelInlineWithAlphaCheck(pdest, GammaCompression(ec));
        end;
        colorPos += colorStep;
        invZ += invZStep;
        inc(pdest);
        {$IFDEF PARAM_USEZBUFFER}
        inc(zbufferpos);
        {$ENDIF}
      end;
    {$ENDIF}

