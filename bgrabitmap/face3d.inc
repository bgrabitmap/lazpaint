type
  PBGRAFaceVertexDescription = ^TBGRAFaceVertexDescription;
  TBGRAFaceVertexDescription = record
       Vertex: IBGRAVertex3D;
       Normal: IBGRANormal3D;
       Color: TBGRAPixel;
       TexCoord: TPointF;
       ColorOverride: boolean;
       TexCoordOverride: boolean;
       ActualColor: TBGRAPixel;
       ActualTexCoord: TPointF;
     end;

  { TBGRAFace3D }

  TBGRAFace3D = class(TInterfacedObject,IBGRAFace3D)
  private
    FVertices: packed array of TBGRAFaceVertexDescription;
    FVertexCount: integer;
    FTexture, FActualTexture: IBGRAScanner;
    FMaterial: IBGRAMaterial3D;
    FActualMaterial: TBGRAMaterial3D;
    FMaterialName: string;
    FParentTexture: boolean;
    FViewNormal: TPoint3D_128;
    FViewCenter: TPoint3D_128;
    FObject3D : IBGRAObject3D;
    FBiface: boolean;
    FLightThroughFactor: single;
    FLightThroughFactorOverride: boolean;
    FCustomFlags: DWord;
    function GetCustomFlags: DWord;
    function GetVertexDescription(AIndex : integer): PBGRAFaceVertexDescription;
    procedure SetCustomFlags(AValue: DWord);
    procedure ComputeActualVertexColor(AIndex: integer);
    procedure ComputeActualTexCoord(AMinIndex, AMaxIndex: integer);
    procedure UpdateTexture;
  public
    function GetObject3D: IBGRAObject3D;
    constructor Create(AObject3D: IBGRAObject3D; AVertices: array of IBGRAVertex3D);
    destructor Destroy; override;
    procedure ComputeVertexColors;
    procedure UpdateMaterial;
    procedure FlipFace;
    function AddVertex(AVertex: IBGRAVertex3D): integer;
    function GetParentTexture: boolean;
    function GetTexture: IBGRAScanner;
    function GetVertex(AIndex: Integer): IBGRAVertex3D;
    function GetVertexColor(AIndex: Integer): TBGRAPixel;
    function GetVertexColorOverride(AIndex: Integer): boolean;
    function GetVertexCount: integer;
    function GetNormal(AIndex: Integer): IBGRANormal3D;
    function GetMaterial: IBGRAMaterial3D;
    function GetMaterialName: string;
    function GetTexCoord(AIndex: Integer): TPointF;
    function GetTexCoordOverride(AIndex: Integer): boolean;
    function GetViewNormal: TPoint3D;
    function GetViewNormal_128: TPoint3D_128;
    function GetViewCenter: TPoint3D;
    function GetViewCenter_128: TPoint3D_128;
    function GetViewCenterZ: single;
    function GetBiface: boolean;
    function GetLightThroughFactor: single;
    function GetLightThroughFactorOverride: boolean;
    procedure SetParentTexture(const AValue: boolean);
    procedure SetTexture(const AValue: IBGRAScanner);
    procedure SetColor(AColor: TBGRAPixel);
    procedure SetVertexColor(AIndex: Integer; const AValue: TBGRAPixel);
    procedure SetVertexColorOverride(AIndex: Integer; const AValue: boolean);
    procedure SetTexCoord(AIndex: Integer; const AValue: TPointF);
    procedure SetTexCoordOverride(AIndex: Integer; const AValue: boolean);
    procedure SetBiface(const AValue: boolean);
    procedure SetLightThroughFactor(const AValue: single);
    procedure SetLightThroughFactorOverride(const AValue: boolean);
    procedure SetVertex(AIndex: Integer; AValue: IBGRAVertex3D);
    procedure SetNormal(AIndex: Integer; AValue: IBGRANormal3D);
    procedure ComputeViewNormalAndCenter;
    procedure SetMaterial(const AValue: IBGRAMaterial3D);
    procedure SetMaterialName(const AValue: string);
    function GetAsObject: TObject;
    property Texture: IBGRAScanner read GetTexture write SetTexture;
    property ParentTexture: boolean read GetParentTexture write SetParentTexture;
    property VertexCount: integer read GetVertexCount;
    property Vertex[AIndex: Integer]: IBGRAVertex3D read GetVertex write SetVertex;
    property Normal[AIndex: Integer]: IBGRANormal3D read GetNormal write SetNormal;
    property VertexColor[AIndex: Integer]: TBGRAPixel read GetVertexColor write SetVertexColor;
    property VertexColorOverride[AIndex: Integer]: boolean read GetVertexColorOverride write SetVertexColorOverride;
    property TexCoord[AIndex: Integer]: TPointF read GetTexCoord write SetTexCoord;
    property TexCoordOverride[AIndex: Integer]: boolean read GetTexCoordOverride write SetTexCoordOverride;
    property ViewNormal: TPoint3D read GetViewNormal;
    property ViewNormal_128: TPoint3D_128 read GetViewNormal_128;
    property ViewCenter: TPoint3D read GetViewCenter;
    property ViewCenter_128: TPoint3D_128 read GetViewCenter_128;
    property ViewCenterZ: single read GetViewCenterZ;
    property Object3D: IBGRAObject3D read GetObject3D;
    property Biface: boolean read GetBiface write SetBiface;
    property LightThroughFactor: single read GetLightThroughFactor write SetLightThroughFactor;
    property LightThroughFactorOverride: boolean read GetLightThroughFactorOverride write SetLightThroughFactorOverride;
    property Material: IBGRAMaterial3D read GetMaterial write SetMaterial;
    property ActualMaterial: TBGRAMaterial3D read FActualMaterial;
    property ActualTexture: IBGRAScanner read FActualTexture;
    property VertexDescription[AIndex : integer]: PBGRAFaceVertexDescription read GetVertexDescription;
    property CustomFlags: DWord read GetCustomFlags write SetCustomFlags;
  end;

{ TBGRAFace3D }

function TBGRAFace3D.GetVertexDescription(AIndex : integer
  ): PBGRAFaceVertexDescription;
begin
  result := @FVertices[AIndex];
end;

function TBGRAFace3D.GetCustomFlags: DWord;
begin
  result := FCustomFlags;
end;

function TBGRAFace3D.GetNormal(AIndex: Integer): IBGRANormal3D;
begin
  result := FVertices[AIndex].Normal;
end;

procedure TBGRAFace3D.SetCustomFlags(AValue: DWord);
begin
  FCustomFlags:= AValue;
end;

procedure TBGRAFace3D.ComputeActualVertexColor(AIndex: integer);
begin
  with FVertices[AIndex] do
  begin
    if ColorOverride then
      ActualColor := Color
    else
    if Vertex.ParentColor then
      ActualColor := FObject3D.Color
    else
      ActualColor := Vertex.Color;
  end;
end;

procedure TBGRAFace3D.ComputeActualTexCoord(AMinIndex, AMaxIndex: integer);
var
  i: Integer;
  zoom: TPointF;
  m: IBGRAMaterial3D;
begin
  m := ActualMaterial;
  if m <> nil then zoom := m.TextureZoom
  else zoom := PointF(1,1);
  for i := AMinIndex to AMaxIndex do
    with FVertices[i] do
    begin
      if TexCoordOverride then
        ActualTexCoord := TexCoord
      else
        ActualTexCoord := Vertex.TexCoord;
      ActualTexCoord.x *= zoom.x;
      ActualTexCoord.y *= zoom.y;
    end;
end;

procedure TBGRAFace3D.UpdateTexture;
begin
  if FParentTexture then
  begin
    FActualTexture := nil;
    if FActualMaterial <> nil then
      FActualTexture := FActualMaterial.GetTexture;
    if FActualTexture = nil then
      FActualTexture := FObject3D.Texture
  end
  else
    FActualTexture := FTexture;
end;

procedure TBGRAFace3D.SetNormal(AIndex: Integer; AValue: IBGRANormal3D);
begin
  FVertices[AIndex].Normal := AValue;
end;

function TBGRAFace3D.GetObject3D: IBGRAObject3D;
begin
  result := FObject3D;
end;

constructor TBGRAFace3D.Create(AObject3D: IBGRAObject3D;
  AVertices: array of IBGRAVertex3D);
var
  i: Integer;
begin
  FObject3D := AObject3D;
  FBiface := false;
  FParentTexture := True;
  FLightThroughFactor:= 0;
  FLightThroughFactorOverride:= false;

  UpdateMaterial;

  SetLength(FVertices, length(AVertices));
  for i:= 0 to high(AVertices) do
    AddVertex(AVertices[i]);
end;

destructor TBGRAFace3D.Destroy;
begin
  FMaterial := nil;
  fillchar(FTexture,sizeof(FTexture),0);
  fillchar(FActualTexture,sizeof(FActualTexture),0);
  inherited Destroy;
end;

procedure TBGRAFace3D.ComputeVertexColors;
var
  i: Integer;
begin
  for i := 0 to FVertexCount-1 do
    ComputeActualVertexColor(i);
end;

procedure TBGRAFace3D.UpdateMaterial;
begin
  if Material <> nil then
    FActualMaterial := TBGRAMaterial3D(Material.GetAsObject)
  else if FObject3D.Material <> nil then
    FActualMaterial := TBGRAMaterial3D(FObject3D.Material.GetAsObject)
  else if TBGRAScene3D(FObject3D.Scene).DefaultMaterial <> nil then
    FActualMaterial := TBGRAMaterial3D(TBGRAScene3D(FObject3D.Scene).DefaultMaterial.GetAsObject);

  UpdateTexture;

  ComputeActualTexCoord(0,FVertexCount-1);
end;

procedure TBGRAFace3D.FlipFace;
var i: integer;
  temp: TBGRAFaceVertexDescription;
begin
  for i := 0 to (VertexCount div 2)-1 do
  begin
    temp := FVertices[i];
    FVertices[i] := FVertices[VertexCount-1-i];
    FVertices[VertexCount-1-i] := temp;
  end;
end;

function TBGRAFace3D.AddVertex(AVertex: IBGRAVertex3D): integer;
begin
  if FVertexCount = length(FVertices) then
    setlength(FVertices, FVertexCount*2+3);
  result := FVertexCount;
  with FVertices[result] do
  begin
    Color := BGRAWhite;
    ColorOverride := false;
    TexCoord := PointF(0,0);
    TexCoordOverride := false;
    Vertex := AVertex;
    Normal := nil;
  end;
  ComputeActualVertexColor(result);
  ComputeActualTexCoord(result,result);
  inc(FVertexCount);
end;

function TBGRAFace3D.GetParentTexture: boolean;
begin
  result := FParentTexture;
end;

function TBGRAFace3D.GetTexture: IBGRAScanner;
begin
  result := FTexture;
end;

function TBGRAFace3D.GetVertex(AIndex: Integer): IBGRAVertex3D;
begin
  if (AIndex < 0) or (AIndex >= FVertexCount) then
    raise Exception.Create('Index out of bounds');
  result := FVertices[AIndex].Vertex;
end;

procedure TBGRAFace3D.SetVertex(AIndex: Integer; AValue: IBGRAVertex3D);
begin
  if (AIndex < 0) or (AIndex >= FVertexCount) then
    raise Exception.Create('Index out of bounds');
  FVertices[AIndex].Vertex := AValue;
  ComputeActualVertexColor(AIndex);
end;

function TBGRAFace3D.GetVertexColor(AIndex: Integer): TBGRAPixel;
begin
  if (AIndex < 0) or (AIndex >= FVertexCount) then
    raise Exception.Create('Index out of bounds');
  result := FVertices[AIndex].ActualColor;
end;

function TBGRAFace3D.GetVertexColorOverride(AIndex: Integer): boolean;
begin
  if (AIndex < 0) or (AIndex >= FVertexCount) then
    raise Exception.Create('Index out of bounds');
  result := FVertices[AIndex].ColorOverride;
end;

function TBGRAFace3D.GetVertexCount: integer;
begin
  result := FVertexCount;
end;

function TBGRAFace3D.GetMaterial: IBGRAMaterial3D;
begin
  result := FMaterial;
end;

function TBGRAFace3D.GetMaterialName: string;
begin
  result := FMaterialName;
end;

procedure TBGRAFace3D.SetParentTexture(const AValue: boolean);
begin
  FParentTexture := AValue;
  UpdateTexture;
end;

procedure TBGRAFace3D.SetTexture(const AValue: IBGRAScanner);
begin
  FTexture := AValue;
  FParentTexture := false;
  UpdateTexture;
end;

procedure TBGRAFace3D.SetColor(AColor: TBGRAPixel);
var i: integer;
begin
  for i := 0 to GetVertexCount-1 do
    SetVertexColor(i,AColor);
end;

procedure TBGRAFace3D.SetVertexColor(AIndex: Integer; const AValue: TBGRAPixel
  );
begin
  if (AIndex < 0) or (AIndex >= FVertexCount) then
    raise Exception.Create('Index out of bounds');
  with FVertices[AIndex] do
  begin
    Color := AValue;
    ColorOverride := true;
  end;
  ComputeActualVertexColor(AIndex);
end;

procedure TBGRAFace3D.SetVertexColorOverride(AIndex: Integer;
  const AValue: boolean);
begin
  if (AIndex < 0) or (AIndex >= FVertexCount) then
    raise Exception.Create('Index out of bounds');
  FVertices[AIndex].ColorOverride := AValue;
  ComputeActualVertexColor(AIndex);
end;

function TBGRAFace3D.GetTexCoord(AIndex: Integer): TPointF;
begin
  if (AIndex < 0) or (AIndex >= FVertexCount) then
    raise Exception.Create('Index out of bounds');
  result := FVertices[AIndex].TexCoord;
end;

function TBGRAFace3D.GetTexCoordOverride(AIndex: Integer): boolean;
begin
  if (AIndex < 0) or (AIndex >= FVertexCount) then
    raise Exception.Create('Index out of bounds');
  result := FVertices[AIndex].TexCoordOverride;
end;

procedure TBGRAFace3D.SetTexCoord(AIndex: Integer; const AValue: TPointF);
begin
  if (AIndex < 0) or (AIndex >= FVertexCount) then
    raise Exception.Create('Index out of bounds');
  FVertices[AIndex].TexCoord := AValue;
  FVertices[AIndex].TexCoordOverride := true;
  ComputeActualTexCoord(AIndex, AIndex);
end;

procedure TBGRAFace3D.SetTexCoordOverride(AIndex: Integer; const AValue: boolean
  );
begin
  if (AIndex < 0) or (AIndex >= FVertexCount) then
    raise Exception.Create('Index out of bounds');
  FVertices[AIndex].TexCoordOverride := AValue;
end;

function TBGRAFace3D.GetViewNormal: TPoint3D;
begin
  result := Point3D(FViewNormal);
end;

function TBGRAFace3D.GetViewNormal_128: TPoint3D_128;
begin
  result := FViewNormal;
end;

function TBGRAFace3D.GetViewCenter: TPoint3D;
begin
  result := Point3D(FViewCenter);
end;

function TBGRAFace3D.GetViewCenter_128: TPoint3D_128;
begin
  result := FViewCenter;
end;

function TBGRAFace3D.GetViewCenterZ: single;
begin
  result := FViewCenter.Z;
end;

function TBGRAFace3D.GetBiface: boolean;
begin
  result := FBiface;
end;

procedure TBGRAFace3D.SetBiface(const AValue: boolean);
begin
  FBiface := AValue;
end;

function TBGRAFace3D.GetLightThroughFactor: single;
begin
  result := FLightThroughFactor;
end;

function TBGRAFace3D.GetLightThroughFactorOverride: boolean;
begin
  result := FLightThroughFactorOverride;
end;

procedure TBGRAFace3D.SetLightThroughFactor(const AValue: single);
begin
  if AValue < 0 then
    FLightThroughFactor := 0
  else
    FLightThroughFactor:= AValue;
  FLightThroughFactorOverride := true;
end;

procedure TBGRAFace3D.SetLightThroughFactorOverride(const AValue: boolean);
begin
  FLightThroughFactorOverride := AValue;
end;

procedure TBGRAFace3D.ComputeViewNormalAndCenter;
var v1,v2: TPoint3D_128;
  i: Integer;
  p0,p1,p2: IBGRAVertex3D;
begin
  if FVertexCount < 3 then
    ClearPoint3D_128(FViewNormal)
  else
  begin
    p0 := FVertices[0].Vertex;
    p1 := FVertices[1].Vertex;
    p2 := FVertices[2].Vertex;
    v1 := p1.ViewCoord_128 - p0.ViewCoord_128;
    v2 := p2.ViewCoord_128 - p1.ViewCoord_128;
    VectProduct3D_128(v2,v1,FViewNormal);
    Normalize3D_128(FViewNormal);
    for i := 0 to FVertexCount-1 do
      FVertices[i].Vertex.AddViewNormal(FViewNormal);
  end;
  ClearPoint3D_128(FViewCenter);
  if FVertexCount > 0 then
  begin
    for i := 0 to FVertexCount-1 do
      FViewCenter += FVertices[i].Vertex.ViewCoord_128;
    FViewCenter *= 1/FVertexCount;
  end;
end;

procedure TBGRAFace3D.SetMaterial(const AValue: IBGRAMaterial3D);
begin
  if AValue <> FMaterial then
  begin
    FMaterial := AValue;
    UpdateMaterial;
  end;
end;

procedure TBGRAFace3D.SetMaterialName(const AValue: string);
begin
  if AValue <> FMaterialName then
  begin
    FMaterialName := AValue;
    TBGRAScene3D(FObject3D.Scene).UseMaterial(FMaterialName, self);
  end;
end;

function TBGRAFace3D.GetAsObject: TObject;
begin
  result := self;
end;


