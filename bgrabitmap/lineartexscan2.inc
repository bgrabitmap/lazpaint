      {$IFDEF PARAM_USESSE} {$asmmode intel}
      asm
         xorps xmm4,xmm4
         xorps xmm5,xmm5
         movlps xmm4, texPos
         movlps xmm5, texStep
         {$IFNDEF PARAM_USEINTERPOLATION}
           {$IFDEF PARAM_USESSE2}
           cvtps2dq xmm3,xmm4
           movlps intTexPos,xmm3
           {$ENDIF}
         {$ENDIF}
      end;
      {$ENDIF}

      for i := ix1 to ix2 do
      begin
        DrawPixelInlineWithAlphaCheck(pdest,
          {$IFDEF PARAM_USELIGHTING} ApplyLightnessFast( {$ENDIF}
          {$IFDEF PARAM_USEINTERPOLATION}
            scanAtFunc(texPos.x,texPos.y)
          {$ELSE}
            {$IFDEF PARAM_USESSE2}
              scanAtIntegerFunc(intTexPos.x,intTexPos.y)
            {$ELSE}
              scanAtIntegerFunc(round(texPos.x),round(texPos.y))
            {$ENDIF}
          {$ENDIF}
          {$IFDEF PARAM_USELIGHTING} ,light) {$ENDIF}
          );
        {$IFDEF PARAM_USESSE}
        asm
           addps xmm4,xmm5
           {$IFNDEF PARAM_USEINTERPOLATION}
             {$IFDEF PARAM_USESSE2}
             cvtps2dq xmm3,xmm4
             movlps intTexPos,xmm3
             {$ELSE}
             movlps texPos,xmm4
             {$ENDIF}
           {$ELSE}
             movlps texPos,xmm4
           {$ENDIF}
        end;
        {$ELSE}
          texPos += texStep;
        {$ENDIF}

        {$IFDEF PARAM_USELIGHTING}
          NextLight;
        {$ENDIF}
        inc(pdest);
      end;
