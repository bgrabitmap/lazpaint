type
  { TBGRAObject3D }

  TBGRAObject3D = class(TInterfacedObject,IBGRAObject3D)
  private
    FColor: TBGRAPixel;
    FLight: Single;
    FTexture: IBGRAScanner;
    FMainPart: IBGRAPart3D;
    FFaces: array of IBGRAFace3D;
    FFaceCount: integer;
    FLightingNormal : TLightingNormal3D;
    FParentLighting: boolean;
    FMaterial: IBGRAMaterial3D;
    FScene: TBGRAScene3D;
    FFaceColorsInvalidated,
    FMaterialInvalidated: boolean;
    procedure AddFace(AFace: IBGRAFace3D);
  public
    constructor Create(AScene: TBGRAScene3D);
    destructor Destroy; override;
    procedure Clear;
    procedure InvalidateColor;
    procedure InvalidateMaterial;
    function AddFace(const AVertices: array of IBGRAVertex3D): IBGRAFace3D;
    function AddFace(const AVertices: array of IBGRAVertex3D; ABiface: boolean): IBGRAFace3D;
    function AddFace(const AVertices: array of IBGRAVertex3D; ATexture: IBGRAScanner): IBGRAFace3D;
    function AddFace(const AVertices: array of IBGRAVertex3D; AColor: TBGRAPixel): IBGRAFace3D;
    function AddFace(const AVertices: array of IBGRAVertex3D; AColors: array of TBGRAPixel): IBGRAFace3D;
    function AddFaceReversed(const AVertices: array of IBGRAVertex3D): IBGRAFace3D;
    procedure ComputeWithMatrix(constref AMatrix: TMatrix3D; constref AProjection: TProjection3D);
    function GetColor: TBGRAPixel;
    function GetLight: Single;
    function GetTexture: IBGRAScanner;
    function GetMainPart: IBGRAPart3D;
    function GetLightingNormal: TLightingNormal3D;
    function GetParentLighting: boolean;
    function GetFace(AIndex: integer): IBGRAFace3D;
    function GetFaceCount: integer;
    function GetTotalVertexCount: integer;
    function GetTotalNormalCount: integer;
    function GetMaterial: IBGRAMaterial3D;
    procedure SetLightingNormal(const AValue: TLightingNormal3D);
    procedure SetParentLighting(const AValue: boolean);
    procedure SetColor(const AValue: TBGRAPixel);
    procedure SetLight(const AValue: Single);
    procedure SetTexture(const AValue: IBGRAScanner);
    procedure SetMaterial(const AValue: IBGRAMaterial3D);
    procedure RemoveUnusedVertices;
    procedure SeparatePart(APart: IBGRAPart3D);
    function GetScene: TObject;
    function GetRefCount: integer;
    procedure SetBiface(AValue : boolean);
    procedure ForEachVertex(ACallback: TVertex3DCallback);
    procedure ForEachFace(ACallback: TFace3DCallback);
    procedure Update;
  end;

  { TBGRAVertex3D }

  TBGRAVertex3D = class(TInterfacedObject,IBGRAVertex3D)
  private
    FColor: TBGRAPixel;
    FParentColor: boolean;
    FLight: Single;
    FTexCoord: TPointF;
    FCoordPool: TBGRACoordPool3D;
    FCoordPoolIndex: integer;
    FCustomFlags: DWord;
    FObject3D: TBGRAObject3D;
    function GetCoordData: PBGRACoordData3D;
    procedure Init(AObject3D: TBGRAObject3D; ACoordPool: TBGRACoordPool3D; ASceneCoord: TPoint3D_128);
  public
    constructor Create(AObject3D: TBGRAObject3D; ACoordPool: TBGRACoordPool3D; ASceneCoord: TPoint3D); overload;
    constructor Create(AObject3D: TBGRAObject3D; ACoordPool: TBGRACoordPool3D; ASceneCoord: TPoint3D_128); overload;
    destructor Destroy; override;
    function GetColor: TBGRAPixel;
    function GetLight: Single;
    function GetViewNormal: TPoint3D;
    function GetViewNormal_128: TPoint3D_128;
    function GetCustomNormal: TPoint3D;
    function GetCustomNormal_128: TPoint3D_128;
    function GetSceneCoord: TPoint3D;
    function GetSceneCoord_128: TPoint3D_128;
    function GetTexCoord: TPointF;
    function GetViewCoord: TPoint3D;
    function GetViewCoord_128: TPoint3D_128;
    function GetUsage: integer;
    function GetCustomFlags: DWord;
    procedure SetColor(const AValue: TBGRAPixel);
    procedure SetLight(const AValue: Single);
    procedure SetViewNormal(const AValue: TPoint3D);
    procedure SetViewNormal_128(const AValue: TPoint3D_128);
    procedure SetCustomNormal(AValue: TPoint3D);
    procedure SetCustomNormal_128(AValue: TPoint3D_128);
    procedure NormalizeViewNormal;
    procedure AddViewNormal(const AValue: TPoint3D_128);
    procedure SetCustomFlags(AValue: DWord);
    procedure SetSceneCoord(const AValue: TPoint3D);
    procedure SetSceneCoord_128(const AValue: TPoint3D_128);
    procedure SetTexCoord(const AValue: TPointF);
    procedure SetViewCoord(const AValue: TPoint3D);
    procedure SetViewCoord_128(const AValue: TPoint3D_128);
    function GetViewCoordZ: single;
    function GetParentColor: Boolean;
    procedure SetParentColor(const AValue: Boolean);
    function GetProjectedCoord: TPointF;
    procedure SetProjectedCoord(const AValue: TPointF);
    procedure ComputeCoordinateAndClearNormal(const AMatrix: TMatrix3D; const AProjection: TProjection3D);
    property SceneCoord: TPoint3D read GetSceneCoord write SetSceneCoord;
    property SceneCoord_128: TPoint3D_128 read GetSceneCoord_128 write SetSceneCoord_128;
    property ViewCoord: TPoint3D read GetViewCoord write SetViewCoord;
    property ViewCoord_128: TPoint3D_128 read GetViewCoord_128 write SetViewCoord_128;
    property ViewCoordZ: single read GetViewCoordZ;
    property ProjectedCoord: TPointF read GetProjectedCoord write SetProjectedCoord;
    property TexCoord: TPointF read GetTexCoord write SetTexCoord;
    property Color: TBGRAPixel read GetColor write SetColor;
    property ParentColor: Boolean read GetParentColor write SetParentColor;
    property Light: Single read GetLight write SetLight;
    property ViewNormal: TPoint3D read GetViewNormal write SetViewNormal;
    property ViewNormal_128: TPoint3D_128 read GetViewNormal_128 write SetViewNormal_128;
    property CustomNormal: TPoint3D read GetCustomNormal write SetCustomNormal;
    property CustomNormal_128: TPoint3D_128 read GetCustomNormal_128 write SetCustomNormal_128;
    property Usage: integer read GetUsage;
    property CoordData: PBGRACoordData3D read GetCoordData;
    function GetAsObject: TObject;
  end;

  { TBGRANormal3D }

  TBGRANormal3D = class(TInterfacedObject,IBGRANormal3D)
  private
    FPool: TBGRANormalPool3D;
    FPoolIndex: integer;
    function GetCustomNormal: TPoint3D;
    function GetCustomNormal_128: TPoint3D_128;
    function GetUsage: integer;
    function GetViewNormal: TPoint3D;
    function GetViewNormal_128: TPoint3D_128;
    procedure SetCustomNormal(AValue: TPoint3D);
    procedure SetCustomNormal_128(AValue: TPoint3D_128);
    procedure SetViewNormal(AValue: TPoint3D);
    procedure SetViewNormal_128(AValue: TPoint3D_128);
  public
    constructor Create(ANormalPool: TBGRANormalPool3D; ACustomNormal: TPoint3D); overload;
    constructor Create(ANormalPool: TBGRANormalPool3D; ACustomNormal: TPoint3D_128); overload;
    destructor Destroy; override;
    property ViewNormal: TPoint3D read GetViewNormal write SetViewNormal;
    property ViewNormal_128: TPoint3D_128 read GetViewNormal_128 write SetViewNormal_128;
    property CustomNormal: TPoint3D read GetCustomNormal write SetCustomNormal;
    property CustomNormal_128: TPoint3D_128 read GetCustomNormal_128 write SetCustomNormal_128;
    property Usage: integer read GetUsage;
  end;

{ TBGRANormal3D }

function TBGRANormal3D.GetCustomNormal: TPoint3D;
begin
  result := Point3D((FPool.NormalData[FPoolIndex])^.customNormal);
end;

function TBGRANormal3D.GetCustomNormal_128: TPoint3D_128;
begin
  result := (FPool.NormalData[FPoolIndex])^.customNormal;
end;

function TBGRANormal3D.GetUsage: integer;
begin
  result := frefcount;
end;

function TBGRANormal3D.GetViewNormal: TPoint3D;
begin
  result := Point3D((FPool.NormalData[FPoolIndex])^.viewNormal);
end;

function TBGRANormal3D.GetViewNormal_128: TPoint3D_128;
begin
  result := (FPool.NormalData[FPoolIndex])^.viewNormal;
end;

procedure TBGRANormal3D.SetCustomNormal(AValue: TPoint3D);
begin
  (FPool.NormalData[FPoolIndex])^.customNormal := Point3D_128(AValue);
end;

procedure TBGRANormal3D.SetCustomNormal_128(AValue: TPoint3D_128);
begin
  (FPool.NormalData[FPoolIndex])^.customNormal := AValue;
end;

procedure TBGRANormal3D.SetViewNormal(AValue: TPoint3D);
begin
  (FPool.NormalData[FPoolIndex])^.viewNormal := Point3D_128(AValue);
end;

procedure TBGRANormal3D.SetViewNormal_128(AValue: TPoint3D_128);
begin
  (FPool.NormalData[FPoolIndex])^.viewNormal := AValue;
end;

constructor TBGRANormal3D.Create(ANormalPool: TBGRANormalPool3D;
  ACustomNormal: TPoint3D);
begin
  FPool := ANormalPool;
  FPoolIndex:= FPool.Add;
  CustomNormal := ACustomNormal;
end;

constructor TBGRANormal3D.Create(ANormalPool: TBGRANormalPool3D;
  ACustomNormal: TPoint3D_128);
begin
  FPool := ANormalPool;
  FPoolIndex:= FPool.Add;
  CustomNormal_128 := ACustomNormal;
end;

destructor TBGRANormal3D.Destroy;
begin
  FPool.Remove(FPoolIndex);
  inherited Destroy;
end;

{ TBGRAVertex3D }

procedure TBGRAVertex3D.Init(AObject3D: TBGRAObject3D; ACoordPool: TBGRACoordPool3D; ASceneCoord: TPoint3D_128);
begin
  FObject3D := AObject3D;
  FCoordPool := ACoordPool;
  FCoordPoolIndex := FCoordPool.Add;
  FColor := BGRAWhite;
  FParentColor := True;
  FLight := 1;
  SceneCoord_128 := ASceneCoord;
end;

procedure TBGRAVertex3D.SetCustomNormal(AValue: TPoint3D);
begin
  with FCoordPool.CoordData[FCoordPoolIndex]^ do
  begin
    customNormal := Point3D_128(AValue);
    customNormalUsed := not CompareMem(@customNormal,@Point3D_128_Zero,sizeof(Point3D_128_Zero));
  end;
end;

procedure TBGRAVertex3D.SetCustomNormal_128(AValue: TPoint3D_128);
begin
  with FCoordPool.CoordData[FCoordPoolIndex]^ do
  begin
    customNormal := AValue;
    customNormalUsed := not CompareMem(@customNormal,@Point3D_128_Zero,sizeof(Point3D_128_Zero));
  end;
end;

function TBGRAVertex3D.GetCoordData: PBGRACoordData3D;
begin
  result := FCoordPool.CoordData[FCoordPoolIndex];
end;

function TBGRAVertex3D.GetCustomNormal: TPoint3D;
begin
  result := Point3D(FCoordPool.CoordData[FCoordPoolIndex]^.customNormal);
end;

function TBGRAVertex3D.GetCustomNormal_128: TPoint3D_128;
begin
  result := FCoordPool.CoordData[FCoordPoolIndex]^.customNormal;
end;

constructor TBGRAVertex3D.Create(AObject3D: TBGRAObject3D; ACoordPool: TBGRACoordPool3D; ASceneCoord: TPoint3D);
begin
  Init(AObject3D, ACoordPool, Point3D_128(ASceneCoord));
end;

constructor TBGRAVertex3D.Create(AObject3D: TBGRAObject3D; ACoordPool: TBGRACoordPool3D; ASceneCoord: TPoint3D_128);
begin
  Init(AObject3D, ACoordPool, ASceneCoord);
end;

destructor TBGRAVertex3D.Destroy;
begin
  FCoordPool.Remove(FCoordPoolIndex);
  inherited Destroy;
end;

function TBGRAVertex3D.GetColor: TBGRAPixel;
begin
  result := FColor;
end;

function TBGRAVertex3D.GetLight: Single;
begin
  result := FLight;
end;

function TBGRAVertex3D.GetViewNormal: TPoint3D;
begin
  result := Point3D(FCoordPool.CoordData[FCoordPoolIndex]^.viewNormal);
end;

function TBGRAVertex3D.GetViewNormal_128: TPoint3D_128;
begin
  result := FCoordPool.CoordData[FCoordPoolIndex]^.viewNormal;
end;

function TBGRAVertex3D.GetSceneCoord: TPoint3D;
begin
  result := Point3D(FCoordPool.CoordData[FCoordPoolIndex]^.sceneCoord);
end;

function TBGRAVertex3D.GetSceneCoord_128: TPoint3D_128;
begin
  result := FCoordPool.CoordData[FCoordPoolIndex]^.sceneCoord;
end;

function TBGRAVertex3D.GetTexCoord: TPointF;
begin
  result := FTexCoord;
end;

function TBGRAVertex3D.GetViewCoord: TPoint3D;
begin
  result := Point3D(FCoordPool.CoordData[FCoordPoolIndex]^.viewCoord);
end;

function TBGRAVertex3D.GetViewCoord_128: TPoint3D_128;
begin
  result := FCoordPool.CoordData[FCoordPoolIndex]^.viewCoord;
end;

function TBGRAVertex3D.GetUsage: integer;
begin
  result := frefcount;
end;

function TBGRAVertex3D.GetCustomFlags: DWord;
begin
  result := FCustomFlags;
end;

procedure TBGRAVertex3D.SetColor(const AValue: TBGRAPixel);
begin
  FColor := AValue;
  FParentColor := false;
  FObject3D.InvalidateColor;
end;

procedure TBGRAVertex3D.SetLight(const AValue: Single);
begin
  FLight := AValue;
end;

procedure TBGRAVertex3D.SetViewNormal(const AValue: TPoint3D);
begin
  FCoordPool.CoordData[FCoordPoolIndex]^.viewNormal := Point3D_128(AValue);
end;

procedure TBGRAVertex3D.SetViewNormal_128(const AValue: TPoint3D_128);
begin
  FCoordPool.CoordData[FCoordPoolIndex]^.viewNormal := AValue;
end;

procedure TBGRAVertex3D.SetSceneCoord(const AValue: TPoint3D);
begin
  FCoordPool.CoordData[FCoordPoolIndex]^.sceneCoord := Point3D_128(AValue);
end;

procedure TBGRAVertex3D.SetSceneCoord_128(const AValue: TPoint3D_128);
begin
  FCoordPool.CoordData[FCoordPoolIndex]^.sceneCoord := AValue;
end;

procedure TBGRAVertex3D.SetTexCoord(const AValue: TPointF);
begin
  FTexCoord := AValue;
end;

procedure TBGRAVertex3D.SetViewCoord(const AValue: TPoint3D);
begin
  FCoordPool.CoordData[FCoordPoolIndex]^.viewCoord := Point3D_128(AValue);
end;

procedure TBGRAVertex3D.SetViewCoord_128(const AValue: TPoint3D_128);
begin
  FCoordPool.CoordData[FCoordPoolIndex]^.viewCoord := AValue;
end;

function TBGRAVertex3D.GetViewCoordZ: single;
begin
  result := FCoordPool.CoordData[FCoordPoolIndex]^.viewCoord.Z;
end;

function TBGRAVertex3D.GetParentColor: Boolean;
begin
  result := FParentColor;
end;

procedure TBGRAVertex3D.SetParentColor(const AValue: Boolean);
begin
  FParentColor := AValue;
  FObject3D.InvalidateColor;
end;

function TBGRAVertex3D.GetProjectedCoord: TPointF;
begin
  result := FCoordPool.CoordData[FCoordPoolIndex]^.projectedCoord;
end;

procedure TBGRAVertex3D.SetProjectedCoord(const AValue: TPointF);
begin
  FCoordPool.CoordData[FCoordPoolIndex]^.projectedCoord := AValue;
end;

procedure TBGRAVertex3D.ComputeCoordinateAndClearNormal(const AMatrix: TMatrix3D; const AProjection : TProjection3D);
var P: PBGRACoordData3D;
begin
  P := FCoordPool.CoordData[FCoordPoolIndex];
  with p^ do
  begin
    viewCoord := AMatrix*sceneCoord;
    if customNormalUsed then
      viewNormal := MultiplyVect3DWithoutTranslation(AMatrix,customNormal)
    else
      ClearPoint3D_128(viewNormal);
    if viewCoord.z > 0 then
    begin
      InvZ := 1/viewCoord.z;
      projectedCoord := PointF(viewCoord.x*InvZ*AProjection.Zoom.x + AProjection.Center.x,
                               viewCoord.y*InvZ*AProjection.Zoom.Y + AProjection.Center.y);
    end else
      projectedCoord := PointF(0,0);
  end;
end;

function TBGRAVertex3D.GetAsObject: TObject;
begin
  result := self;
end;

procedure TBGRAVertex3D.NormalizeViewNormal;
begin
  Normalize3D_128(FCoordPool.CoordData[FCoordPoolIndex]^.viewNormal);
end;

procedure TBGRAVertex3D.AddViewNormal(const AValue: TPoint3D_128);
begin
  with FCoordPool.CoordData[FCoordPoolIndex]^ do
    if not customNormalUsed then
      Add3D_Aligned(viewNormal, AValue);
end;

procedure TBGRAVertex3D.SetCustomFlags(AValue: DWord);
begin
  FCustomFlags:= AValue;
end;

