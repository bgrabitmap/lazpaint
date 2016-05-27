{$i bgrasse.inc}
var
  {%H-}dist2,LdotN,NdotH,lightEnergy,diffuse : single;
begin
  {$IFDEF BGRASSE_AVAILABLE}If UseSSE then
  begin
    with Context^ do
    begin
      {$IFDEF PARAM_POINTLIGHT}
      vL := FVertex.ViewCoord_128;
      vH := FViewVector;
      {$ELSE}
      vL := FDirection;
      vH := FBetweenDirectionAndObserver;
      {$ENDIF}
    end;
    if UseSSE3 then
    begin
      {$DEFINE PARAM_USESSE3}
      {$i phonglightsse.inc}
      {$UNDEF PARAM_USESSE3}
    end else
    begin
      {$i phonglightsse.inc}
    end;
  end else
  {$ENDIF}
  with Context^ do
  begin
    {$IFDEF PARAM_POINTLIGHT}
    vL := FVertex.ViewCoord_128 - basic.Position;
    Normalize3D_128_SqLen(vL, dist2);
    //compute bisector of angle between light and observer
    vH := vL + FViewVector;
    Normalize3D_128(vH);
    //Calculate LdotN and NnH
    LdotN := DotProduct3D_128(basic.Normal, vL);
    NdotH := DotProduct3D_128(basic.Normal, vH);
    {$ELSE}
    LdotN := DotProduct3D_128(basic.Normal, FDirection);
    NdotH := DotProduct3D_128(basic.Normal, FBetweenDirectionAndObserver);
    {$ENDIF}
  end;

  {$IFDEF PARAM_POINTLIGHT}
  if dist2 = 0 then
    lightEnergy := 0
  else
    lightEnergy := FIntensity / dist2;
  diffuse := LdotN*lightEnergy;
  {$ELSE}
  lightEnergy := 1;
  diffuse := LdotN;
  {$ENDIF}
  if diffuse < FMinIntensity then diffuse:= FMinIntensity;

  if Context^.LightThrough and (diffuse < 0) then diffuse := -diffuse*Context^.LightThroughFactor;
  TBGRAMaterial3D(Context^.material).ComputeDiffuseAndSpecularColor(Context, diffuse, lightEnergy, NdotH, FColorInt);
end;

{$UNDEF PARAM_POINTLIGHT}
