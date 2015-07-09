type

  { TBGRASphere3D }

  TBGRASphere3D = class(TBGRAObject3D)
    constructor Create(AScene: TBGRAScene3D; ARadius: single; HorizPrecision: integer = 8; VerticalPrecision : integer = 6; HalfSphere: boolean = false);
  end;

{ TBGRASphere3D }

constructor TBGRASphere3D.Create(AScene: TBGRAScene3D; ARadius: single; HorizPrecision: integer; VerticalPrecision : integer; HalfSphere: boolean);
var prevAlpha,prevBeta,alpha,beta,NbAlpha,NbBeta: integer;
    sinBeta,cosBeta: single;
    sinAlpha,cosAlpha: single;
    v: IBGRAPart3D;
    vTop,vBottom: IBGRAVertex3D;
    alphaFactor: single;
    startAlpha: integer;
begin
  inherited Create(AScene);
  NbAlpha := HorizPrecision;
  if NbAlpha < 4 then NbAlpha := 4;
  NbBeta := VerticalPrecision-1;
  if NbBeta < 2 then NbBeta := 2;
  v := GetMainPart;


  if halfSphere then
    alphaFactor := Pi/(NbAlpha-1)
  else
    alphaFactor := (2*Pi)/NbAlpha;

  for beta := 1 to NbBeta-1 do
  begin
    sinBeta := sin(beta*Pi/NbBeta);
    cosBeta := -cos(beta*Pi/NbBeta);
    for alpha := 0 to NbAlpha-1 do
    begin
      sinAlpha := -sin(alpha*alphaFactor);
      cosAlpha := -cos(alpha*alphaFactor);
      v.Add( cosAlpha*sinBeta*ARadius, cosBeta*ARadius, sinAlpha*sinBeta*ARadius );
    end;
  end;

  if halfSphere then
    startAlpha := 1
  else
    startAlpha := 0;

  vTop := v.Add(0,-ARadius,0);
  prevAlpha := (startAlpha+NbAlpha-1) mod NbAlpha;
  for alpha := startAlpha to NbAlpha-1 do
  begin
    AddFace( [v.Vertex[prevAlpha], vTop, v.Vertex[alpha]], HalfSphere );
    prevAlpha := alpha;
  end;

  prevBeta := 0;
  for beta := 1 to NbBeta-2 do
  begin
    prevAlpha := (startAlpha+NbAlpha-1) mod NbAlpha;
    for alpha := startAlpha to NbAlpha-1 do
    begin
      AddFace( [v.Vertex[prevAlpha + prevBeta*NbAlpha], v.Vertex[alpha + prevBeta*NbAlpha],
                v.Vertex[alpha + beta*NbAlpha], v.Vertex[prevAlpha + beta*NbAlpha]], HalfSphere );
      prevAlpha := alpha;
    end;
    prevBeta := beta;
  end;

  vBottom := v.Add(0,ARadius,0);
  prevAlpha := (startAlpha+NbAlpha-1) mod NbAlpha;
  for alpha := startAlpha to NbAlpha-1 do
  begin
    AddFace( [v.Vertex[prevAlpha + prevBeta*NbAlpha], v.Vertex[alpha + prevBeta*NbAlpha], vBottom], HalfSphere );
    prevAlpha := alpha;
  end;
end;
