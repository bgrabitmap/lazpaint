      begin
        for k := 0 to NbShapeRows-1 do
        with shapeRow[shapeRowsList[k]],shapes[shapeRowsList[k]] do
        begin
          if texture <> nil then
          begin
            texture.ScanMoveTo(densMinx,yb);
            ScanNextFunc := @texture.ScanNextPixel;
            pdens := density+(densMinX-minx);
            for xb := densMinx to densMaxx do
            with sums[xb-minx] do
            begin
              j := pdens^; inc(pdens);
              if j <> 0 then
              begin
                ec := GammaExpansion(ScanNextFunc());
                {$ifdef PARAM_ANTIALIASINGFACTOR}
                  w := DivByAntialiasPrecision65536(j*ec.alpha);
                {$else}
                  w := (j*ec.alpha) shr 16;
                {$endif}
                if w <> 0 then
                begin
                  inc(sumR,ec.red*w);
                  inc(sumG,ec.green*w);
                  inc(sumB,ec.blue*w);
                  inc(sumA,w);
                end;
              end else
                ScanNextFunc();
            end;
          end else
          begin
            ec := color;
            pdens := density+(densMinX-minx);
            for xb := densMinx to densMaxx do
             with sums[xb-minx] do
             begin
               j := pdens^; inc(pdens);
               if j <> 0 then
               begin
                 {$ifdef PARAM_ANTIALIASINGFACTOR}
                   w := DivByAntialiasPrecision65536(j*ec.alpha);
                 {$else}
                   w := (j*ec.alpha) shr 16;
                 {$endif}
                 if w <> 0 then
                 begin
                   inc(sumR,ec.red*w);
                   inc(sumG,ec.green*w);
                   inc(sumB,ec.blue*w);
                   inc(sumA,w);
                 end;
               end;
             end;
          end;
        end;

      end
{$undef PARAM_ANTIALIASINGFACTOR}
