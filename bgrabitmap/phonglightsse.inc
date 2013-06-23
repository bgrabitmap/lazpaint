    {$asmmode intel}
    asm
      mov eax, Context
      movaps xmm0,[eax+160] //Context^.vL
      movaps xmm2,[eax+192] //Context^.vH
      movaps xmm1,[eax+16] //Context^.Normal

      {$IFDEF PARAM_POINTLIGHT}
        movaps xmm6,[eax+0]  //Context^.Position
        subps xmm0,xmm6      //xmm0 = vL
        movaps xmm6, xmm0
        mulps xmm6, xmm6

        {$IFDEF PARAM_USESSE3}
        haddps xmm6,xmm6
        haddps xmm6,xmm6
        {$ELSE}
        //mix1
        movaps xmm7, xmm6
        shufps xmm7, xmm7, $4e
        addps xmm6, xmm7
        //mix2
        movaps xmm7, xmm6
        shufps xmm7, xmm7, $11
        addps xmm6, xmm7
        {$ENDIF}

        movss dist2, xmm6  //dist2 := vL*vL;

        rsqrtps xmm6, xmm6
        mulps xmm0, xmm6  //xmm0 = normalized vL
      {$ENDIF}

      {$IFDEF PARAM_POINTLIGHT}
        addps xmm2,xmm0  //vH += normalized vL
        movaps xmm6, xmm2
        mulps xmm2, xmm2

        {$IFDEF PARAM_USESSE3}
        haddps xmm2,xmm2
        haddps xmm2,xmm2
        {$ELSE}
        //mix1
        movaps xmm7, xmm2
        shufps xmm7, xmm7, $4e
        addps xmm2, xmm7
        //mix2
        movaps xmm7, xmm2
        shufps xmm7, xmm7, $11
        addps xmm2, xmm7
        {$ENDIF}

        rsqrtps xmm2, xmm2
        mulps xmm2, xmm6  //xmm2 = normalized vH
      {$ENDIF}

      //vL*Normal
      mulps xmm0, xmm1
      //vH*Normal
      mulps xmm2, xmm1

      {$IFDEF PARAM_USESSE3}
      haddps xmm0,xmm0
      haddps xmm0,xmm0
      {$ELSE}
      //mix1
      movaps xmm7, xmm0
      shufps xmm7, xmm7, $4e
      addps xmm0, xmm7
      //mix2
      movaps xmm7, xmm0
      shufps xmm7, xmm7, $11
      addps xmm0, xmm7
      {$ENDIF}

      {$IFDEF PARAM_USESSE3}
      haddps xmm2,xmm2
      haddps xmm2,xmm2
      {$ELSE}
      //mix1
      movaps xmm7, xmm2
      shufps xmm7, xmm7, $4e
      addps xmm2, xmm7
      //mix2
      movaps xmm7, xmm2
      shufps xmm7, xmm7, $11
      addps xmm2, xmm7
      {$ENDIF}

      movss LdotN, xmm0
      movss NdotH, xmm2
    end;

