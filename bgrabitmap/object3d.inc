{ TBGRAObject3D }

procedure TBGRAObject3D.AddFace(AFace: IBGRAFace3D);
begin
  if FFaceCount = length(FFaces) then
     setlength(FFaces,FFaceCount*2+3);
  FFaces[FFaceCount] := AFace;
  inc(FFaceCount);
end;

constructor TBGRAObject3D.Create(AScene: TBGRAScene3D);
begin
  FColor := BGRAWhite;
  FLight := 1;
  FTexture := nil;
  FMainPart := TBGRAPart3D.Create(self,nil);
  FLightingNormal:= AScene.DefaultLightingNormal;
  FParentLighting:= True;
  FScene := AScene;
  FFaceColorsInvalidated := true;
  FMaterialInvalidated := false;
end;

destructor TBGRAObject3D.Destroy;
begin
  FMaterial := nil;
  fillchar(FTexture,sizeof(FTexture),0);
  inherited Destroy;
end;

procedure TBGRAObject3D.Clear;
begin
  FFaces := nil;
  FFaceCount := 0;
  FMainPart.Clear(True);
end;

procedure TBGRAObject3D.InvalidateColor;
begin
  FFaceColorsInvalidated := true;
end;

procedure TBGRAObject3D.InvalidateMaterial;
begin
  FMaterialInvalidated := true;
end;

function TBGRAObject3D.GetColor: TBGRAPixel;
begin
  result := FColor;
end;

function TBGRAObject3D.GetLight: Single;
begin
  result := FLight;
end;

function TBGRAObject3D.GetTexture: IBGRAScanner;
begin
  result := FTexture;
end;

function TBGRAObject3D.GetMainPart: IBGRAPart3D;
begin
  result := FMainPart;
end;

procedure TBGRAObject3D.SetColor(const AValue: TBGRAPixel);
begin
  FColor := AValue;
  FTexture := nil;
  InvalidateColor;
end;

procedure TBGRAObject3D.SetLight(const AValue: Single);
begin
  FLight := AValue;
end;

procedure TBGRAObject3D.SetTexture(const AValue: IBGRAScanner);
begin
  FTexture := AValue;
  InvalidateMaterial;
end;

procedure TBGRAObject3D.SetMaterial(const AValue: IBGRAMaterial3D);
begin
  FMaterial := AValue;
  InvalidateMaterial;
end;

procedure TBGRAObject3D.RemoveUnusedVertices;
begin
  GetMainPart.RemoveUnusedVertices;
end;

procedure TBGRAObject3D.SeparatePart(APart: IBGRAPart3D);
var
  vertexInfo: array of record
       orig,dup: IBGRAVertex3D;
     end;

  i,j: integer;
  inPart,outPart: boolean;
  idxV: integer;
begin
  setlength(vertexInfo, APart.VertexCount);
  for i := 0 to high(vertexInfo) do
    with vertexInfo[i] do
    begin
      orig := APart.Vertex[i];
      dup := APart.Add(orig.SceneCoord_128);
    end;

  for i := 0 to GetFaceCount-1 do
    with GetFace(i) do
    begin
      inPart := false;
      outPart := false;
      for j := 0 to VertexCount-1 do
        if (APart.IndexOf(Vertex[j]) <> -1) then
          inPart := true
        else
          outPart := true;

      if inPart and not outPart then
      begin
        for j := 0 to VertexCount-1 do
        begin
          idxV := APart.IndexOf(Vertex[j]);
          if idxV <> -1 then
            Vertex[j] := vertexInfo[idxV].dup;
        end;
      end;
    end;

  for i := APart.VertexCount-1 downto 0 do
    APart.RemoveVertex(i);
end;

function TBGRAObject3D.GetScene: TObject;
begin
  result := FScene;
end;

function TBGRAObject3D.GetRefCount: integer;
begin
  result := RefCount;
end;

procedure TBGRAObject3D.SetBiface(AValue: boolean);
var i: integer;
begin
  for i := 0 to GetFaceCount-1 do
    GetFace(i).Biface := AValue;
end;

procedure TBGRAObject3D.ForEachVertex(ACallback: TVertex3DCallback);
begin
  FMainPart.ForEachVertex(ACallback);
end;

procedure TBGRAObject3D.ForEachFace(ACallback: TFace3DCallback);
var i: integer;
begin
  for i := 0 to GetFaceCount-1 do
    ACallback(GetFace(i));
end;

procedure TBGRAObject3D.Update;
var
  i: Integer;
begin
  if FParentLighting and (FLightingNormal <> FScene.DefaultLightingNormal) then
    FLightingNormal := FScene.DefaultLightingNormal;

  if FFaceColorsInvalidated then
  begin
    for i := 0 to FFaceCount-1 do
      FFaces[i].ComputeVertexColors;
    FFaceColorsInvalidated := false;
  end;

  if FMaterialInvalidated then
  begin
    for i := 0 to FFaceCount-1 do
      FFaces[i].UpdateMaterial;
    FMaterialInvalidated := false;
  end;
end;

function TBGRAObject3D.GetLightingNormal: TLightingNormal3D;
begin
  result := FLightingNormal;
end;

function TBGRAObject3D.GetParentLighting: boolean;
begin
  result := FParentLighting;
end;

procedure TBGRAObject3D.SetLightingNormal(const AValue: TLightingNormal3D);
begin
  FLightingNormal := AValue;
  FParentLighting:= False;
end;

procedure TBGRAObject3D.SetParentLighting(const AValue: boolean);
begin
  FParentLighting:= AValue;
end;

procedure TBGRAObject3D.ComputeWithMatrix(constref AMatrix: TMatrix3D; constref AProjection: TProjection3D);
var
  i: Integer;
begin
  FMainPart.ComputeWithMatrix(AMatrix,AProjection);
  for i := 0 to FFaceCount-1 do
    FFaces[i].ComputeViewNormalAndCenter;
  FMainPart.NormalizeViewNormal;
end;

function TBGRAObject3D.AddFaceReversed(const AVertices: array of IBGRAVertex3D
  ): IBGRAFace3D;
var
  tempVertices: array of IBGRAVertex3D;
  i: Integer;
begin
  setlength(tempVertices,length(AVertices));
  for i := 0 to high(tempVertices) do
    tempVertices[i] := AVertices[high(AVertices)-i];
  result := AddFace(tempVertices);
end;

function TBGRAObject3D.AddFace(const AVertices: array of IBGRAVertex3D): IBGRAFace3D;
begin
  result := TBGRAFace3D.Create(self,AVertices);
  AddFace(result);
end;

function TBGRAObject3D.AddFace(const AVertices: array of IBGRAVertex3D;
  ABiface: boolean): IBGRAFace3D;
begin
  result := TBGRAFace3D.Create(self,AVertices);
  result.Biface := ABiface;
  AddFace(result);
end;

function TBGRAObject3D.AddFace(const AVertices: array of IBGRAVertex3D; ATexture: IBGRAScanner): IBGRAFace3D;
var Face: IBGRAFace3D;
begin
  Face := TBGRAFace3D.Create(self,AVertices);
  Face.Texture := ATexture;
  AddFace(Face);
  result := face;
end;

function TBGRAObject3D.AddFace(const AVertices: array of IBGRAVertex3D;
  AColor: TBGRAPixel): IBGRAFace3D;
var Face: IBGRAFace3D;
begin
  Face := TBGRAFace3D.Create(self,AVertices);
  Face.SetColor(AColor);
  Face.Texture := nil;
  AddFace(Face);
  result := face;
end;

function TBGRAObject3D.AddFace(const AVertices: array of IBGRAVertex3D;
  AColors: array of TBGRAPixel): IBGRAFace3D;
var
  i: Integer;
begin
  if length(AColors) <> length(AVertices) then
    raise Exception.Create('Dimension mismatch');
  result := TBGRAFace3D.Create(self,AVertices);
  for i := 0 to high(AColors) do
    result.VertexColor[i] := AColors[i];
  AddFace(result);
end;

function TBGRAObject3D.GetFace(AIndex: integer): IBGRAFace3D;
begin
  if (AIndex < 0) or (AIndex >= FFaceCount) then
    raise Exception.Create('Index out of bounds');
  result := FFaces[AIndex];
end;

function TBGRAObject3D.GetFaceCount: integer;
begin
  result := FFaceCount;
end;

function TBGRAObject3D.GetTotalVertexCount: integer;
begin
  result := GetMainPart.TotalVertexCount;
end;

function TBGRAObject3D.GetTotalNormalCount: integer;
begin
  result := GetMainPart.TotalNormalCount;
end;

function TBGRAObject3D.GetMaterial: IBGRAMaterial3D;
begin
  result := FMaterial;
end;


