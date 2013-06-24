asm
        //vL := vLS- vP*LightDestFactor;
        movups xmm4, vP
        movups xmm6,LightDestFactor4
        mulps xmm6, xmm4   //keep xmm4 = vP
        movaps xmm0, xmm1
        subps xmm0, xmm6

        movups xmm2, vV //preload xmm2 (A)

        //normalize(vL);
        movaps xmm6, xmm0   //save
        mulps xmm0, xmm0

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

        //1/sqrt
        rsqrtps xmm0, xmm0
        mulps xmm0, xmm6  //apply


        //vH := vL + vV;  (A)
        //xmm2 = vV
        addps xmm2, xmm0



        //vectproduct(vS1,vS2,vN);
        movups xmm6, vS1
        shufps xmm6, xmm6, Shift231

        movups xmm7, vS2
        shufps xmm7, xmm7, Shift312

        movaps xmm3,xmm6
        mulps xmm3,xmm7

        movups xmm6, vS1
        shufps xmm6, xmm6, Shift312

        movups xmm7, vS2
        shufps xmm7, xmm7, Shift231

        mulps xmm7,xmm6
        subps xmm3,xmm7
        //end of vectproduct


        //normalize(vH);
        movaps xmm6, xmm2   //save
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

        //1/sqrt
        rsqrtps xmm2, xmm2
        mulps xmm2, xmm6  //apply

        //normalize(vN);
        movaps xmm6, xmm3   //save
        mulps xmm3, xmm3

        {$IFDEF PARAM_USESSE3}
        haddps xmm3,xmm3
        haddps xmm3,xmm3
        {$ELSE}
        //mix1
        movaps xmm7, xmm3
        shufps xmm7, xmm7, $4e
        addps xmm3, xmm7
        //mix2
        movaps xmm7, xmm3
        shufps xmm7, xmm7, $11
        addps xmm3, xmm7
        {$ENDIF}

        //1/sqrt
        rsqrtps xmm3, xmm3
        mulps xmm3, xmm6  //apply

        //LdotN := vN * vL;
        movups xmm5, xmm3
        mulps xmm5, xmm0
        //mix1
        movaps xmm7, xmm5
        shufps xmm7, xmm7, $4e
        addps xmm5, xmm7
        //mix2
        movaps xmm7, xmm5
        shufps xmm7, xmm7, $11
        addps xmm5, xmm7
        //:=
        movss LdotN, xmm5

        //vD := vLS-vP;
        movaps xmm5, xmm1
        subps xmm5, xmm4

        //dist := sqrt(vD*vD);
        mulps xmm5, xmm5

        {$IFDEF PARAM_USESSE3}
        haddps xmm5,xmm5
        haddps xmm5,xmm5
        {$ELSE}
        //mix1
        movaps xmm7, xmm5
        shufps xmm7, xmm7, $4e
        addps xmm5, xmm7
        //mix2
        movaps xmm7, xmm5
        shufps xmm7, xmm7, $11
        addps xmm5, xmm7
        {$ENDIF}

        //:= sqrt
        sqrtss xmm5,xmm5
        movss dist, xmm5

        //NH := vH * vN;
        movups xmm6, xmm2
        mulps xmm6, xmm3

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
        addss xmm6, xmm7
        {$ENDIF}
        //:=
        movss NH, xmm6
      end;
