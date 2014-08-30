unit BGRAScene3D;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmapTypes, BGRAColorInt, BGRASSE, BGRAMatrix3D;

type
  PSceneLightingContext = ^TSceneLightingContext;
  TSceneLightingContext = packed record
    basic: TBasicLightingContext;
    {128} diffuseColor, {144} specularColor: TColorInt65536;
    {160} vL, {176} dummy: TPoint3D_128;
    {192} vH: TPoint3D_128;
    {208} lightness: integer;
    {212} material : TObject;
    LightThroughFactor: single;
    LightThrough: LongBool;
    SaturationLow: integer;
    SaturationLowF: single;
    SaturationHigh: integer;
    SaturationHighF: single;
  end;

  TProjection3D = packed record
    Zoom, Center: TPointF;
  end;

  TBox3D = record
    min,max: TPoint3D;
  end;

  TLightingNormal3D = (lnNone, lnFace, lnVertex, lnFaceVertexMix);
  TLightingInterpolation3D = (liLowQuality, liSpecularHighQuality, liAlwaysHighQuality);
  TAntialiasingMode3D = (am3dNone, am3dMultishape, am3dResample);
  TPerspectiveMode3D = (pmLinearMapping, pmPerspectiveMapping, pmZBuffer);

type
  TRenderingOptions = record
    LightingInterpolation: TLightingInterpolation3D;
    AntialiasingMode: TAntialiasingMode3D;
    AntialiasingResampleLevel: integer;
    PerspectiveMode: TPerspectiveMode3D;
    TextureInterpolation: boolean;
    MinZ: single;
  end;

  TBGRAScene3D = class;

{$i bgrascene3Dinterface.inc}

type
  { TBGRAScene3D }

  TBGRAScene3D = class
  private
    FSurface: TBGRACustomBitmap;
    FViewCenter: TPointF;
    FAutoViewCenter: boolean;
    FObjects: array of IBGRAObject3D;
    FObjectCount: integer;
    FMaterials: array of IBGRAMaterial3D;
    FMaterialCount: integer;
    FMatrix: TMatrix3D;
    FViewPoint: TPoint3D_128;
    FLookWhere, FTopDir: TPoint3D_128;
    FZoom: TPointF;
    FAutoZoom: Boolean;
    FLights: TList;
    FAmbiantLightness: integer;
    FAmbiantLightColor: TColorInt65536;
    FRenderedFaceCount: integer;
    FProjection: TProjection3D;
    function GetAmbiantLightColorF: TColorF;
    function GetAmbiantLightness: single;
    function GetAmbiantLightColor: TBGRAPixel;
    function GetFaceCount: integer;
    function GetLight(AIndex: integer): IBGRALight3D;
    function GetLightCount: integer;
    function GetMaterial(AIndex: integer): IBGRAMaterial3D;
    function GetObject(AIndex: integer): IBGRAObject3D;
    function GetVertexCount: integer;
    function GetViewCenter: TPointF;
    function GetViewPoint: TPoint3D;
    function GetZoom: TPointF;
    procedure SetAmbiantLightColorF(const AValue: TColorF);
    procedure SetAmbiantLightness(const AValue: single);
    procedure SetAmbiantLightColor(const AValue: TBGRAPixel);
    procedure SetAutoViewCenter(const AValue: boolean);
    procedure SetAutoZoom(const AValue: boolean);
    procedure SetViewCenter(const AValue: TPointF);
    procedure SetViewPoint(const AValue: TPoint3D);
    procedure ComputeView(ScaleX,ScaleY: single);
    function ComputeCoordinate(ASceneCoord: TPoint3D_128; APart: IBGRAPart3D): TPointF; overload;
    function ComputeCoordinate(AViewCoord: TPoint3D_128): TPointF; overload;
    procedure ComputeLight;
    procedure ComputeMatrix;
    procedure AddObject(AObj: IBGRAObject3D);
    procedure AddLight(ALight: TObject);
    procedure AddMaterial(AMaterial: IBGRAMaterial3D);
    procedure Init;
    procedure InternalRender(ASurface: TBGRACustomBitmap; AAntialiasingMode: TAntialiasingMode3D; GlobalScale: single); virtual;

  protected
    function ApplyLightingWithLightness(Context: PSceneLightingContext; Color: TBGRAPixel): TBGRAPixel; virtual;
    function ApplyLightingWithDiffuseColor(Context: PSceneLightingContext; Color: TBGRAPixel): TBGRAPixel; virtual;
    function ApplyLightingWithDiffuseAndSpecularColor(Context: PSceneLightingContext; Color: TBGRAPixel): TBGRAPixel; virtual;
    function ApplyLightingWithAmbiantLightnessOnly(Context: PSceneLightingContext; Color: TBGRAPixel): TBGRAPixel; virtual;
    function ApplyNoLighting(Context: PSceneLightingContext; Color: TBGRAPixel): TBGRAPixel; virtual;
    procedure UseMaterial(AMaterialName: string; AFace: IBGRAFace3D); virtual;

  public
    DefaultLightingNormal: TLightingNormal3D;
    DefaultMaterial : IBGRAMaterial3D;
    RenderingOptions: TRenderingOptions;

    constructor Create;
    constructor Create(ASurface: TBGRACustomBitmap);
    destructor Destroy; override;
    procedure Clear;
    function LoadObjectFromFile(AFilename: string; SwapFacesOrientation: boolean = true): IBGRAObject3D;
    function LoadObjectFromFileUTF8(AFilename: string; SwapFacesOrientation: boolean = true): IBGRAObject3D;
    function LoadObjectFromStream(AStream: TStream; SwapFacesOrientation: boolean = true): IBGRAObject3D;
    procedure LookAt(AWhere: TPoint3D; ATopDir: TPoint3D);
    procedure LookLeft(angleDeg: single);
    procedure LookRight(angleDeg: single);
    procedure LookUp(angleDeg: single);
    procedure LookDown(angleDeg: single);
    procedure Render; virtual;
    function CreateObject: IBGRAObject3D; overload;
    function CreateObject(ATexture: IBGRAScanner): IBGRAObject3D; overload;
    function CreateObject(AColor: TBGRAPixel): IBGRAObject3D; overload;
    function CreateSphere(ARadius: Single; AHorizPrecision: integer = 8; AVerticalPrecision : integer = 6): IBGRAObject3D; overload;
    function CreateSphere(ARadius: Single; AColor: TBGRAPixel; AHorizPrecision: integer = 8; AVerticalPrecision : integer = 6): IBGRAObject3D; overload;
    function CreateHalfSphere(ARadius: Single; AHorizPrecision: integer = 6; AVerticalPrecision : integer = 6): IBGRAObject3D; overload;
    function CreateHalfSphere(ARadius: Single; AColor: TBGRAPixel; AHorizPrecision: integer = 6; AVerticalPrecision : integer = 6): IBGRAObject3D; overload;
    procedure RemoveObject(AObject: IBGRAObject3D);
    function AddDirectionalLight(ADirection: TPoint3D; ALightness: single = 1; AMinIntensity : single = 0): IBGRADirectionalLight3D;
    function AddDirectionalLight(ADirection: TPoint3D; AColor: TBGRAPixel; AMinIntensity: single = 0): IBGRADirectionalLight3D;
    function AddPointLight(AVertex: IBGRAVertex3D; AOptimalDistance: single; ALightness: single = 1; AMinIntensity : single = 0): IBGRAPointLight3D;
    function AddPointLight(AVertex: IBGRAVertex3D; AOptimalDistance: single; AColor: TBGRAPixel; AMinIntensity: single = 0): IBGRAPointLight3D;
    procedure RemoveLight(ALight: IBGRALight3D);
    procedure SetZoom(value: Single); overload;
    procedure SetZoom(value: TPointF); overload;
    function CreateMaterial: IBGRAMaterial3D;
    function CreateMaterial(ASpecularIndex: integer): IBGRAMaterial3D;
    procedure UpdateMaterials; virtual;
    procedure UpdateMaterial(AMaterialName: string); virtual;
    procedure ForEachVertex(ACallback: TVertex3DCallback);
    procedure ForEachFace(ACallback: TFace3DCallback);
    property ViewCenter: TPointF read GetViewCenter write SetViewCenter;
    property AutoViewCenter: boolean read FAutoViewCenter write SetAutoViewCenter;
    property AutoZoom: boolean read FAutoZoom write SetAutoZoom;
    property Surface: TBGRACustomBitmap read FSurface write FSurface;
    property Object3D[AIndex: integer]: IBGRAObject3D read GetObject;
    property Object3DCount: integer read FObjectCount;
    property VertexCount: integer read GetVertexCount;
    property FaceCount: integer read GetFaceCount;
    property Zoom: TPointF read GetZoom write SetZoom;
    property AmbiantLightness: single read GetAmbiantLightness write SetAmbiantLightness;
    property AmbiantLightColor: TBGRAPixel read GetAmbiantLightColor write SetAmbiantLightColor;
    property AmbiantLightColorF: TColorF read GetAmbiantLightColorF write SetAmbiantLightColorF;
    property LightCount: integer read GetLightCount;
    property Light[AIndex: integer]: IBGRALight3D read GetLight;
    property ViewPoint: TPoint3D read GetViewPoint write SetViewPoint;
    property RenderedFaceCount : integer read FRenderedFaceCount;
    property Material[AIndex: integer] : IBGRAMaterial3D read GetMaterial;
    property MaterialCount: integer read FMaterialCount;
  end;

implementation

uses BGRAPolygon, BGRAPolygonAliased, BGRACoordPool3D, BGRAResample,
  lazutf8classes;

{$i lightingclasses3d.inc}

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
    procedure AddFace(AFace: IBGRAFace3D);
  public
    constructor Create(AScene: TBGRAScene3D);
    destructor Destroy; override;
    procedure Clear;

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
    function GetMaterial: IBGRAMaterial3D;
    procedure SetLightingNormal(const AValue: TLightingNormal3D);
    procedure SetParentLighting(const AValue: boolean);
    procedure SetColor(const AValue: TBGRAPixel);
    procedure SetLight(const AValue: Single);
    procedure SetTexture(const AValue: IBGRAScanner);
    procedure SetMaterial(const AValue: IBGRAMaterial3D);
    procedure RemoveUnusedVertices;
    procedure SeparatePart(APart: IBGRAPart3D);
    function GetScene: TBGRAScene3D;
    function GetRefCount: integer;
    procedure SetBiface(AValue : boolean);
    procedure ForEachVertex(ACallback: TVertex3DCallback);
    procedure ForEachFace(ACallback: TFace3DCallback);
  end;

{$i shape3D.inc}

type
  { TBGRAPart3D }

  TBGRAPart3D = class(TInterfacedObject,IBGRAPart3D)
  private
    FVertices: array of IBGRAVertex3D;
    FVertexCount: integer;
    FMatrix: TMatrix3D;
    FParts: array of IBGRAPart3D;
    FPartCount: integer;
    FContainer: IBGRAPart3D;
    FCoordPool: TBGRACoordPool3D;
  public
    constructor Create(AContainer: IBGRAPart3D);
    destructor Destroy; override;
    procedure Clear(ARecursive: boolean);
    function Add(x,y,z: single): IBGRAVertex3D;
    function Add(pt: TPoint3D): IBGRAVertex3D;
    function Add(pt: TPoint3D_128): IBGRAVertex3D;
    function Add(const coords: array of single): arrayOfIBGRAVertex3D;
    function Add(const pts: array of TPoint3D): arrayOfIBGRAVertex3D;
    function Add(const pts: array of TPoint3D_128): arrayOfIBGRAVertex3D;
    procedure Add(const pts: array of IBGRAVertex3D);
    procedure Add(AVertex: IBGRAVertex3D);
    procedure RemoveVertex(Index: integer);
    function GetBoundingBox: TBox3D;
    function GetRadius: single;
    function GetMatrix: TMatrix3D;
    function GetPart(AIndex: Integer): IBGRAPart3D;
    function GetPartCount: integer;
    function GetVertex(AIndex: Integer): IBGRAVertex3D;
    function GetVertexCount: integer;
    function GetTotalVertexCount: integer;
    function GetContainer: IBGRAPart3D;
    procedure SetVertex(AIndex: Integer; const AValue: IBGRAVertex3D);
    procedure ResetTransform;
    procedure Translate(x,y,z: single; Before: boolean = true);
    procedure Translate(ofs: TPoint3D; Before: boolean = true);
    procedure Scale(size: single; Before: boolean = true);
    procedure Scale(x,y,z: single; Before: boolean = true);
    procedure Scale(size: TPoint3D; Before: boolean = true);
    procedure RotateXDeg(angle: single; Before: boolean = true);
    procedure RotateYDeg(angle: single; Before: boolean = true);
    procedure RotateZDeg(angle: single; Before: boolean = true);
    procedure RotateXRad(angle: single; Before: boolean = true);
    procedure RotateYRad(angle: single; Before: boolean = true);
    procedure RotateZRad(angle: single; Before: boolean = true);
    procedure SetMatrix(const AValue: TMatrix3D);
    procedure ComputeWithMatrix(const AMatrix: TMatrix3D; const AProjection: TProjection3D);
    function ComputeCoordinate(var ASceneCoord: TPoint3D_128; const AProjection: TProjection3D): TPointF;
    procedure NormalizeViewNormal;
    function CreatePart: IBGRAPart3D;
    procedure LookAt(ALookWhere,ATopDir: TPoint3D);
    procedure RemoveUnusedVertices;
    function IndexOf(AVertex: IBGRAVertex3D): integer;
    procedure ForEachVertex(ACallback: TVertex3DCallback);
  end;

  { TBGRAFace3D }

  PBGRAFaceVertexDescription = ^TBGRAFaceVertexDescription;
  TBGRAFaceVertexDescription = record
       Vertex: IBGRAVertex3D;
       Color: TBGRAPixel;
       TexCoord: TPointF;
       ColorOverride: boolean;
       TexCoordOverride: boolean;
     end;

  TBGRAFace3D = class(TInterfacedObject,IBGRAFace3D)
  private
    FVertices: packed array of TBGRAFaceVertexDescription;
    FVertexCount: integer;
    FTexture: IBGRAScanner;
    FMaterial: IBGRAMaterial3D;
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
  public
    function GetObject3D: IBGRAObject3D;
    constructor Create(AObject3D: IBGRAObject3D; AVertices: array of IBGRAVertex3D);
    destructor Destroy; override;
    procedure AddVertex(AVertex: IBGRAVertex3D);
    function GetParentTexture: boolean;
    function GetTexture: IBGRAScanner;
    function GetVertex(AIndex: Integer): IBGRAVertex3D;
    function GetVertexColor(AIndex: Integer): TBGRAPixel;
    function GetVertexColorOverride(AIndex: Integer): boolean;
    function GetVertexCount: integer;
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
    procedure SetVertex(AIndex: Integer; const AValue: IBGRAVertex3D);
    procedure ComputeViewNormalAndCenter;
    procedure SetMaterial(const AValue: IBGRAMaterial3D);
    procedure SetMaterialName(const AValue: string);
    function GetAsObject: TObject;
    property Texture: IBGRAScanner read GetTexture write SetTexture;
    property ParentTexture: boolean read GetParentTexture write SetParentTexture;
    property VertexCount: integer read GetVertexCount;
    property Vertex[AIndex: Integer]: IBGRAVertex3D read GetVertex write SetVertex;
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
    property VertexDescription[AIndex : integer]: PBGRAFaceVertexDescription read GetVertexDescription;
    property CustomFlags: DWord read GetCustomFlags write SetCustomFlags;
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
    function GetCoordData: PBGRACoordData3D;
    procedure Init(ACoordPool: TBGRACoordPool3D; ASceneCoord: TPoint3D_128);
  public
    constructor Create(ACoordPool: TBGRACoordPool3D; ASceneCoord: TPoint3D); overload;
    constructor Create(ACoordPool: TBGRACoordPool3D; ASceneCoord: TPoint3D_128); overload;
    destructor Destroy; override;
    function GetColor: TBGRAPixel;
    function GetLight: Single;
    function GetViewNormal: TPoint3D;
    function GetViewNormal_128: TPoint3D_128;
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
    property Usage: integer read GetUsage;
    property CoordData: PBGRACoordData3D read GetCoordData;
    function GetAsObject: TObject;
  end;

{ TBGRAVertex3D }

procedure TBGRAVertex3D.Init(ACoordPool: TBGRACoordPool3D; ASceneCoord: TPoint3D_128);
begin
  FCoordPool := ACoordPool;
  FCoordPoolIndex := FCoordPool.Add;
  FColor := BGRAWhite;
  FParentColor := True;
  FLight := 1;
  SceneCoord_128 := ASceneCoord;
end;

function TBGRAVertex3D.GetCoordData: PBGRACoordData3D;
begin
  result := FCoordPool.CoordData[FCoordPoolIndex];
end;

constructor TBGRAVertex3D.Create(ACoordPool: TBGRACoordPool3D; ASceneCoord: TPoint3D);
begin
  Init(ACoordPool, Point3D_128(ASceneCoord));
end;

constructor TBGRAVertex3D.Create(ACoordPool: TBGRACoordPool3D; ASceneCoord: TPoint3D_128);
begin
  Init(ACoordPool, ASceneCoord);
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
  Add3D_Aligned(FCoordPool.CoordData[FCoordPoolIndex]^.viewNormal, AValue);
end;

procedure TBGRAVertex3D.SetCustomFlags(AValue: DWord);
begin
  FCustomFlags:= AValue;
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

procedure TBGRAFace3D.SetCustomFlags(AValue: DWord);
begin
  FCustomFlags:= AValue;
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
  SetLength(FVertices, length(AVertices));
  for i:= 0 to high(AVertices) do
    AddVertex(AVertices[i]);
  FObject3D := AObject3D;
  FBiface := false;
  FParentTexture := True;
  FLightThroughFactor:= 0;
  FLightThroughFactorOverride:= false;
end;

destructor TBGRAFace3D.Destroy;
begin
  fillchar(FTexture,sizeof(FTexture),0);
  inherited Destroy;
end;

procedure TBGRAFace3D.AddVertex(AVertex: IBGRAVertex3D);
begin
  if FVertexCount = length(FVertices) then
    setlength(FVertices, FVertexCount*2+3);
  with FVertices[FVertexCount] do
  begin
    Color := BGRAWhite;
    ColorOverride := false;
    TexCoord := PointF(0,0);
    TexCoordOverride := false;
    Vertex := AVertex;
  end;
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

procedure TBGRAFace3D.SetVertex(AIndex: Integer; const AValue: IBGRAVertex3D);
begin
  if (AIndex < 0) or (AIndex >= FVertexCount) then
    raise Exception.Create('Index out of bounds');
  FVertices[AIndex].Vertex := AValue;
end;

function TBGRAFace3D.GetVertexColor(AIndex: Integer): TBGRAPixel;
begin
  if (AIndex < 0) or (AIndex >= FVertexCount) then
    raise Exception.Create('Index out of bounds');
  result := FVertices[AIndex].Color;
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
end;

procedure TBGRAFace3D.SetTexture(const AValue: IBGRAScanner);
begin
  FTexture := AValue;
  FParentTexture := false;
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
end;

procedure TBGRAFace3D.SetVertexColorOverride(AIndex: Integer;
  const AValue: boolean);
begin
  if (AIndex < 0) or (AIndex >= FVertexCount) then
    raise Exception.Create('Index out of bounds');
  FVertices[AIndex].ColorOverride := AValue;
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
  FMaterial := AValue;
end;

procedure TBGRAFace3D.SetMaterialName(const AValue: string);
begin
  if AValue <> FMaterialName then
  begin
    FMaterialName := AValue;
    FObject3D.Scene.UseMaterial(FMaterialName, self);
  end;
end;

function TBGRAFace3D.GetAsObject: TObject;
begin
  result := self;
end;

{ TBGRAPart3D }

procedure TBGRAPart3D.LookAt(ALookWhere,ATopDir: TPoint3D);
var ZDir, XDir, YDir: TPoint3D_128;
    ViewPoint: TPoint3D_128;
    CurPart: IBGRAPart3D;
    ComposedMatrix: TMatrix3D;
begin
  YDir := -Point3D_128(ATopDir);
  if IsPoint3D_128_Zero(YDir) then exit;
  Normalize3D_128(YDir);

  ComposedMatrix := FMatrix;
  CurPart := self.FContainer;
  while CurPart <> nil do
  begin
    ComposedMatrix := CurPart.Matrix*ComposedMatrix;
    CurPart := CurPart.Container;
  end;
  ViewPoint := ComposedMatrix*Point3D_128_Zero;

  ZDir := Point3D_128(ALookWhere)-ViewPoint;
  if IsPoint3D_128_Zero(ZDir) then exit;
  Normalize3D_128(ZDir);

  VectProduct3D_128(YDir,ZDir,XDir);
  VectProduct3D_128(ZDir,XDir,YDir); //correct Y dir

  FMatrix := Matrix3D(XDir,YDir,ZDir,ViewPoint);
  ComposedMatrix := MatrixIdentity3D;
  CurPart := self.FContainer;
  while CurPart <> nil do
  begin
    ComposedMatrix := CurPart.Matrix*ComposedMatrix;
    CurPart := CurPart.Container;
  end;
  FMatrix := MatrixInverse3D(ComposedMatrix)*FMatrix;
end;

procedure TBGRAPart3D.RemoveUnusedVertices;
var
  i: Integer;
begin
  for i := FVertexCount-1 downto 0 do
    if FVertices[i].Usage <= 2 then RemoveVertex(i);
  for i := 0 to FPartCount-1 do
    FParts[i].RemoveUnusedVertices;
end;

function TBGRAPart3D.IndexOf(AVertex: IBGRAVertex3D): integer;
var i: integer;
begin
  for i := 0 to FVertexCount-1 do
    if FVertices[i] = AVertex then
    begin
      result := i;
      exit;
    end;
  result := -1;
end;

procedure TBGRAPart3D.ForEachVertex(ACallback: TVertex3DCallback);
var i: integer;
begin
  for i := 0 to FVertexCount-1 do
    ACallback(FVertices[i]);
end;

procedure TBGRAPart3D.Add(AVertex: IBGRAVertex3D);
begin
  if FVertexCount = length(FVertices) then
    setlength(FVertices, FVertexCount*2+3);
  FVertices[FVertexCount] := AVertex;
  inc(FVertexCount);
end;

procedure TBGRAPart3D.RemoveVertex(Index: integer);
var i: integer;
begin
  if (Index >= 0) and (Index < FVertexCount) then
  begin
    for i := Index to FVertexCount-2 do
      FVertices[i] := FVertices[i+1];
    FVertices[FVertexCount-1] := nil;
    dec(FVertexCount);
  end;
end;

function TBGRAPart3D.GetRadius: single;
var i: integer;
    pt: TPoint3D_128;
    d: single;
begin
  result := 0;
  for i := 0 to GetVertexCount-1 do
  begin
    pt := GetVertex(i).SceneCoord_128;
    d:= sqrt(DotProduct3D_128(pt,pt));
    if d > result then result := d;
  end;
end;

constructor TBGRAPart3D.Create(AContainer: IBGRAPart3D);
begin
  FContainer := AContainer;
  FMatrix := MatrixIdentity3D;
  FCoordPool := TBGRACoordPool3D.Create(4);
end;

destructor TBGRAPart3D.Destroy;
begin
  FCoordPool.Free;
  inherited Destroy;
end;

procedure TBGRAPart3D.Clear(ARecursive: boolean);
var i: integer;
begin
  FVertices := nil;
  FVertexCount := 0;
  if ARecursive then
  begin
    for i := 0 to FPartCount-1 do
      FParts[i].Clear(ARecursive);
    FParts := nil;
    FPartCount := 0;
  end;
end;

function TBGRAPart3D.Add(x, y, z: single): IBGRAVertex3D;
begin
  result := TBGRAVertex3D.Create(FCoordPool,Point3D(x,y,z));
  Add(result);
end;

function TBGRAPart3D.Add(pt: TPoint3D): IBGRAVertex3D;
begin
  result := TBGRAVertex3D.Create(FCoordPool,pt);
  Add(result);
end;

function TBGRAPart3D.Add(pt: TPoint3D_128): IBGRAVertex3D;
begin
  result := TBGRAVertex3D.Create(FCoordPool,pt);
  Add(result);
end;

function TBGRAPart3D.Add(const coords: array of single
  ): arrayOfIBGRAVertex3D;
var pts: array of TPoint3D;
    CoordsIdx: integer;
    i: Integer;
begin
  if length(coords) mod 3 <> 0 then
    raise exception.Create('Array size must be a multiple of 3');
  setlength(pts, length(coords) div 3);
  coordsIdx := 0;
  for i := 0 to high(pts) do
  begin
    pts[i] := Point3D(coords[CoordsIdx],coords[CoordsIdx+1],coords[CoordsIdx+2]);
    inc(coordsIdx,3);
  end;
  result := Add(pts);
end;

function TBGRAPart3D.Add(const pts: array of TPoint3D): arrayOfIBGRAVertex3D;
var
  i: Integer;
begin
  setlength(result, length(pts));
  for i := 0 to high(pts) do
    result[i] := TBGRAVertex3D.Create(FCoordPool,pts[i]);
  Add(result);
end;

function TBGRAPart3D.Add(const pts: array of TPoint3D_128
  ): arrayOfIBGRAVertex3D;
var
  i: Integer;
begin
  setlength(result, length(pts));
  for i := 0 to high(pts) do
    result[i] := TBGRAVertex3D.Create(FCoordPool,pts[i]);
  Add(result);
end;

procedure TBGRAPart3D.Add(const pts: array of IBGRAVertex3D);
var
  i: Integer;
begin
  if FVertexCount + length(pts) > length(FVertices) then
    setlength(FVertices, (FVertexCount*2 + length(pts))+1);
  for i := 0 to high(pts) do
  begin
    FVertices[FVertexCount] := pts[i];
    inc(FVertexCount);
  end;
end;

function TBGRAPart3D.GetBoundingBox: TBox3D;
var i: integer;
    pt: TPoint3D_128;
begin
  if GetVertexCount > 0 then
  begin
    result.min := GetVertex(0).SceneCoord;
    result.max := result.min;
  end else
  begin
    result.min := Point3D(0,0,0);
    result.max := Point3D(0,0,0);
    exit;
  end;
  for i := 1 to GetVertexCount-1 do
  begin
    pt := GetVertex(i).SceneCoord_128;
    if pt.x < result.min.x then result.min.x := pt.x else
    if pt.x > result.max.x then result.max.x := pt.x;
    if pt.y < result.min.y then result.min.y := pt.y else
    if pt.y > result.max.y then result.max.y := pt.y;
    if pt.z < result.min.z then result.min.z := pt.z else
    if pt.z > result.max.z then result.max.z := pt.z;
  end;
end;

function TBGRAPart3D.GetMatrix: TMatrix3D;
begin
  result := FMatrix;
end;

function TBGRAPart3D.GetPart(AIndex: Integer): IBGRAPart3D;
begin
  if (AIndex < 0) or (AIndex >= FPartCount) then
    raise exception.Create('Index of out bounds');
  result := FParts[AIndex];
end;

function TBGRAPart3D.GetPartCount: integer;
begin
  result := FPartCount;
end;

function TBGRAPart3D.GetVertex(AIndex: Integer): IBGRAVertex3D;
begin
  if (AIndex < 0) or (AIndex >= FVertexCount) then
    raise exception.Create('Index of out bounds');
  result := FVertices[AIndex];
end;

function TBGRAPart3D.GetVertexCount: integer;
begin
  result := FVertexCount;
end;

function TBGRAPart3D.GetTotalVertexCount: integer;
var i: integer;
begin
  result := GetVertexCount;
  for i := 0 to GetPartCount-1 do
    result += GetPart(i).GetTotalVertexCount;
end;

procedure TBGRAPart3D.ResetTransform;
begin
  FMatrix := MatrixIdentity3D;
end;

procedure TBGRAPart3D.Scale(size: single; Before: boolean = true);
begin
  Scale(size,size,size,Before);
end;

procedure TBGRAPart3D.Scale(x, y, z: single; Before: boolean = true);
begin
  Scale(Point3D(x,y,z),Before);
end;

procedure TBGRAPart3D.Scale(size: TPoint3D; Before: boolean = true);
begin
  if Before then
    FMatrix *= MatrixScale3D(size)
  else
    FMatrix := MatrixScale3D(size)*FMatrix;
end;

procedure TBGRAPart3D.RotateXDeg(angle: single; Before: boolean = true);
begin
  RotateXRad(-angle*Pi/180, Before);
end;

procedure TBGRAPart3D.RotateYDeg(angle: single; Before: boolean = true);
begin
  RotateYRad(-angle*Pi/180, Before);
end;

procedure TBGRAPart3D.RotateZDeg(angle: single; Before: boolean = true);
begin
  RotateZRad(-angle*Pi/180, Before);
end;

procedure TBGRAPart3D.RotateXRad(angle: single; Before: boolean = true);
begin
  if Before then
    FMatrix *= MatrixRotateX(angle)
  else
    FMatrix := MatrixRotateX(angle) * FMatrix;
end;

procedure TBGRAPart3D.RotateYRad(angle: single; Before: boolean = true);
begin
  if Before then
    FMatrix *= MatrixRotateY(angle)
  else
    FMatrix := MatrixRotateY(angle) * FMatrix;
end;

procedure TBGRAPart3D.RotateZRad(angle: single; Before: boolean = true);
begin
  if Before then
    FMatrix *= MatrixRotateZ(angle)
  else
    FMatrix := MatrixRotateZ(angle) * FMatrix;
end;

procedure TBGRAPart3D.SetMatrix(const AValue: TMatrix3D);
begin
  FMatrix := AValue;
end;

procedure TBGRAPart3D.ComputeWithMatrix(const AMatrix: TMatrix3D; const AProjection: TProjection3D);
var
  i: Integer;
  Composed: TMatrix3D;
  P: PBGRACoordData3D;
begin
  Composed := AMatrix* self.FMatrix;
  {$IFDEF CPUI386}
  if UseSSE then
  begin
    Matrix3D_SSE_Load(Composed);
    asm
      mov eax,[AProjection]
      movups xmm4,[eax]
      xorps xmm1,xmm1
    end;
    P := FCoordPool.CoordData[0];
    i := FCoordPool.UsedCapacity;
    if UseSSE3 then
    begin
      while i > 0 do
      with P^ do
      begin
        MatrixMultiplyVect3D_SSE3_Aligned(sceneCoord,viewCoord);
        if viewCoord.z > 0 then
        begin
          asm
            mov eax, P
            movaps xmm3, [eax+16] //viewCoord
            movaps xmm2,xmm3
            shufps xmm2,xmm3,2+8+32+128
            rcpps xmm2,xmm2  //xmm2 = InvZ
            movss [eax+40],xmm2 //-> InvZ

            mulps xmm3,xmm4  //xmm3 *= Projection.Zoom
            mulps xmm3,xmm2  //xmm3 *= InvZ

            movhlps xmm0,xmm4  //xmm2 = Projection.Center
            addps xmm3,xmm0  //xmm3 += Projection.Center

            movlps [eax+32],xmm3 //->projectedCoord
            movaps [eax+48],xmm1 //->normal
          end;
        end else
        asm
          mov eax, P
          movlps [eax+32],xmm1  //0 ->projectedCoord
          movaps [eax+48],xmm1 //->normal
        end;
        dec(i);
        inc(p);
      end;
    end else
    begin
      while i > 0 do
      with P^ do
      begin
        MatrixMultiplyVect3D_SSE_Aligned(sceneCoord,viewCoord);
        if viewCoord.z > 0 then
        begin
          asm
            mov eax, P
            movaps xmm3, [eax+16] //viewCoord
            movaps xmm2,xmm3
            shufps xmm2,xmm3,2+8+32+128
            rcpps xmm2,xmm2  //xmm2 = InvZ
            movss [eax+40],xmm2 //-> InvZ

            mulps xmm3,xmm4  //xmm3 *= Projection.Zoom
            mulps xmm3,xmm2  //xmm3 *= InvZ

            movhlps xmm0,xmm4  //xmm2 = Projection.Center
            addps xmm3,xmm0  //xmm3 += Projection.Center

            movlps [eax+32],xmm3 //->projectedCoord
            movaps [eax+48],xmm1 //->normal
          end;
        end else
        asm
          mov eax, P
          movlps [eax+32],xmm1  //0 ->projectedCoord
          movaps [eax+48],xmm1 //->normal
        end;
        dec(i);
        inc(p);
      end;
    end;
  end
  else
  {$ENDIF}
  begin
    P := FCoordPool.CoordData[0];
    i := FCoordPool.UsedCapacity;
    while i > 0 do
    with P^ do
    begin
      viewCoord := Composed*sceneCoord;
      ClearPoint3D_128(viewNormal);
      if viewCoord.z > 0 then
      begin
        InvZ := 1/viewCoord.z;
        projectedCoord := PointF(viewCoord.x*InvZ*AProjection.Zoom.x + AProjection.Center.x,
                                 viewCoord.y*InvZ*AProjection.Zoom.Y + AProjection.Center.y);
      end else
        projectedCoord := PointF(0,0);
      dec(i);
      inc(p);
    end;
  end;
  for i := 0 to FPartCount-1 do
    FParts[i].ComputeWithMatrix(Composed,AProjection);
end;

function TBGRAPart3D.ComputeCoordinate(var ASceneCoord: TPoint3D_128; const AProjection: TProjection3D): TPointF;
var part: IBGRAPart3D;
  newViewCoord: TPoint3D_128;
  InvZ: single;
begin
  newViewCoord := FMatrix * ASceneCoord;
  part := FContainer;
  while part <> nil do
  begin
    newViewCoord := part.Matrix * newViewCoord;
    part := part.Container;
  end;
  if NewViewCoord.z > 0 then
  begin
    InvZ := 1/NewViewCoord.z;
    result := PointF(NewViewCoord.x*InvZ*AProjection.Zoom.x + AProjection.Center.x,
                     NewViewCoord.y*InvZ*AProjection.Zoom.Y + AProjection.Center.y);
  end else
    result := PointF(0,0);
end;

procedure TBGRAPart3D.NormalizeViewNormal;
var
  i: Integer;
begin
  for i := 0 to FVertexCount-1 do
    FVertices[i].NormalizeViewNormal;
  for i := 0 to FPartCount-1 do
    FParts[i].NormalizeViewNormal;
end;

procedure TBGRAPart3D.Translate(x, y, z: single; Before: boolean = true);
begin
  Translate(Point3D(x,y,z),Before);
end;

procedure TBGRAPart3D.Translate(ofs: TPoint3D; Before: boolean = true);
begin
  if Before then
    FMatrix *= MatrixTranslation3D(ofs)
  else
    FMatrix := MatrixTranslation3D(ofs)*FMatrix;
end;

function TBGRAPart3D.CreatePart: IBGRAPart3D;
begin
  if FPartCount = length(FParts) then
    setlength(FParts, FPartCount*2+1);
  result := TBGRAPart3D.Create(self);
  FParts[FPartCount] := result;
  inc(FPartCount);
end;

function TBGRAPart3D.GetContainer: IBGRAPart3D;
begin
  result := FContainer;
end;

procedure TBGRAPart3D.SetVertex(AIndex: Integer; const AValue: IBGRAVertex3D);
begin
  if (AIndex < 0) or (AIndex >= FVertexCount) then
    raise exception.Create('Index of out bounds');
  FVertices[AIndex] := AValue;
end;

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
  FMainPart := TBGRAPart3D.Create(nil);
  FLightingNormal:= AScene.DefaultLightingNormal;
  FParentLighting:= True;
  FScene := AScene;
end;

destructor TBGRAObject3D.Destroy;
begin
  fillchar(FTexture,sizeof(FTexture),0);
  inherited Destroy;
end;

procedure TBGRAObject3D.Clear;
begin
  FFaces := nil;
  FFaceCount := 0;
  FMainPart.Clear(True);
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
end;

procedure TBGRAObject3D.SetLight(const AValue: Single);
begin
  FLight := AValue;
end;

procedure TBGRAObject3D.SetTexture(const AValue: IBGRAScanner);
begin
  FTexture := AValue;
end;

procedure TBGRAObject3D.SetMaterial(const AValue: IBGRAMaterial3D);
begin
  FMaterial := AValue;
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

function TBGRAObject3D.GetScene: TBGRAScene3D;
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

function TBGRAObject3D.GetMaterial: IBGRAMaterial3D;
begin
  result := FMaterial;
end;

{ TBGRAScene3D }

function TBGRAScene3D.GetViewCenter: TPointF;
begin
  if FAutoViewCenter then
  begin
    if Surface = nil then
      result := PointF(0,0)
    else
      result := PointF((Surface.Width-1)/2,(Surface.Height-1)/2)
  end
  else
    result := FViewCenter;
end;

function TBGRAScene3D.GetViewPoint: TPoint3D;
begin
  result := Point3D(FViewPoint);
end;

function TBGRAScene3D.GetZoom: TPointF;
var size: single;
begin
  if FAutoZoom then
  begin
    if FSurface = nil then
      result := PointF(1,1)
    else
    begin
      Size := sqrt(FSurface.Width*FSurface.Height)*0.8;
      result := PointF(size,size);
    end;
  end else
    result := FZoom;
end;

procedure TBGRAScene3D.SetAmbiantLightColorF(const AValue: TColorF);
begin
  FAmbiantLightColor := ColorFToColorInt65536(AValue);
  FAmbiantLightness := (FAmbiantLightColor.r + FAmbiantLightColor.g + FAmbiantLightColor.b) div 6;
end;

procedure TBGRAScene3D.SetAmbiantLightness(const AValue: single);
begin
  FAmbiantLightness:= round(AValue*32768);
  FAmbiantLightColor := ColorInt65536(FAmbiantLightness*2, FAmbiantLightness*2, FAmbiantLightness*2);
end;

procedure TBGRAScene3D.SetAmbiantLightColor(const AValue: TBGRAPixel);
begin
  FAmbiantLightColor := BGRAToColorInt(AValue);
  FAmbiantLightness := (FAmbiantLightColor.r + FAmbiantLightColor.g + FAmbiantLightColor.b) div 6;
end;

function TBGRAScene3D.GetObject(AIndex: integer): IBGRAObject3D;
begin
  if (AIndex < 0) or (AIndex >= FObjectCount) then
    raise exception.Create('Index out of bounds');
  result := FObjects[AIndex];
end;

function TBGRAScene3D.GetVertexCount: integer;
var i: integer;
begin
  result := 0;
  for i := 0 to Object3DCount-1 do
    result += Object3D[i].TotalVertexCount;
end;

function TBGRAScene3D.GetAmbiantLightColor: TBGRAPixel;
begin
  result := ColorIntToBGRA(FAmbiantLightColor);
end;

function TBGRAScene3D.GetFaceCount: integer;
var i: integer;
begin
  result := 0;
  for i := 0 to Object3DCount-1 do
    result += Object3D[i].FaceCount;
end;

function TBGRAScene3D.GetLight(AIndex: integer): IBGRALight3D;
begin
  if (AIndex < 0) or (AIndex >= FLights.Count) then
    result := nil
  else
    result := TBGRALight3D(FLights[AIndex]);
end;

function TBGRAScene3D.GetLightCount: integer;
begin
  result := FLights.Count;
end;

function TBGRAScene3D.GetMaterial(AIndex: integer): IBGRAMaterial3D;
begin
  if (AIndex < 0) or (AIndex >= FMaterialCount) then
    raise exception.Create('Index out of bounds');
  result := FMaterials[AIndex];
end;

function TBGRAScene3D.GetAmbiantLightness: single;
begin
  result := FAmbiantLightness/32768;
end;

function TBGRAScene3D.GetAmbiantLightColorF: TColorF;
begin
  result := ColorInt65536ToColorF(FAmbiantLightColor);
end;

procedure TBGRAScene3D.SetAutoViewCenter(const AValue: boolean);
begin
  if FAutoViewCenter=AValue then exit;
  if not AValue then
    FViewCenter := ViewCenter;
  FAutoViewCenter:=AValue;
end;

procedure TBGRAScene3D.SetAutoZoom(const AValue: boolean);
begin
  if FAutoZoom=AValue then exit;
  if not AValue then
    FZoom := Zoom;
  FAutoZoom:=AValue;
end;

procedure TBGRAScene3D.SetViewCenter(const AValue: TPointF);
begin
  FViewCenter := AValue;
  FAutoViewCenter:= False;
end;

procedure TBGRAScene3D.ComputeMatrix;
var ZDir, XDir, YDir: TPoint3D_128;
begin
  if IsPoint3D_128_Zero(FTopDir) then exit;
  YDir := -FTopDir;
  Normalize3D_128(YDir);

  ZDir := FLookWhere-FViewPoint;
  if IsPoint3D_128_Zero(ZDir) then exit;
  Normalize3D_128(ZDir);

  VectProduct3D_128(YDir,ZDir,XDir);
  VectProduct3D_128(ZDir,XDir,YDir); //correct Y dir
  Normalize3D_128(XDir);
  Normalize3D_128(YDir);

  FMatrix := Matrix3D(XDir,YDir,ZDir,FViewPoint);
  FMatrix := MatrixInverse3D(FMatrix);
end;

procedure TBGRAScene3D.AddObject(AObj: IBGRAObject3D);
begin
  if FObjectCount = length(FObjects) then
    setlength(FObjects, FObjectCount*2+1);
  FObjects[FObjectCount] := AObj;
  inc(FObjectCount);
end;

procedure TBGRAScene3D.AddLight(ALight: TObject);
begin
  FLights.Add(ALight);
  IBGRALight3D(TBGRALight3D(ALight))._AddRef;
end;

procedure TBGRAScene3D.AddMaterial(AMaterial: IBGRAMaterial3D);
begin
  if FMaterialCount = length(FMaterials) then
    setlength(FMaterials, FMaterialCount*2+1);
  FMaterials[FMaterialCount] := AMaterial;
  inc(FMaterialCount);
end;

procedure TBGRAScene3D.Init;
begin
  FAutoZoom := True;
  FAutoViewCenter := True;
  ViewPoint := Point3D(0,0,-100);
  LookAt(Point3D(0,0,0), Point3D(0,-1,0));
  with RenderingOptions do
  begin
    TextureInterpolation := False;
    PerspectiveMode := pmPerspectiveMapping;
    LightingInterpolation := liSpecularHighQuality;
    AntialiasingMode := am3dNone;
    AntialiasingResampleLevel := 2;
  end;
  AmbiantLightness := 1;
  AmbiantLightColor := BGRAWhite;
  DefaultLightingNormal := lnFaceVertexMix;
  FLights := TList.Create;
  FRenderedFaceCount:= 0;
  FMaterialCount := 0;
  FObjectCount := 0;
  DefaultMaterial := CreateMaterial;
  RenderingOptions.MinZ := 1;
end;

constructor TBGRAScene3D.Create;
begin
  Init;
end;

constructor TBGRAScene3D.Create(ASurface: TBGRACustomBitmap);
begin
  FSurface := ASurface;
  Init;
end;

destructor TBGRAScene3D.Destroy;
begin
  Clear;
  FLights.Free;
  inherited Destroy;
end;

procedure TBGRAScene3D.Clear;
var i: integer;
begin
  for i := 0 to FObjectCount-1 do
    FObjects[i].Clear;
  FObjects := nil;
  FObjectCount := 0;
  for i := 0 to FLights.Count-1 do
    IBGRALight3D(TBGRALight3D(FLights[i]))._Release;
  FLights.Clear;
end;

{$hints off}
procedure TBGRAScene3D.UseMaterial(AMaterialName: string; AFace: IBGRAFace3D);
var color: TBGRAPixel;
begin
  color := BGRA(0,128,255);
  AFace.SetColor(color);
end;
{$hints on}

function TBGRAScene3D.LoadObjectFromFile(AFilename: string; SwapFacesOrientation: boolean): IBGRAObject3D;
var source: TFileStream;
begin
  source := TFileStream.Create(AFilename,fmOpenRead,fmShareDenyWrite);
  try
    result := LoadObjectFromStream(source,SwapFacesOrientation);
  finally
    source.free;
  end;
end;

function TBGRAScene3D.LoadObjectFromFileUTF8(AFilename: string;
  SwapFacesOrientation: boolean): IBGRAObject3D;
var source: TFileStreamUTF8;
begin
  source := TFileStreamUTF8.Create(AFilename,fmOpenRead,fmShareDenyWrite);
  try
    result := LoadObjectFromStream(source,SwapFacesOrientation);
  finally
    source.free;
  end;
end;

function TBGRAScene3D.LoadObjectFromStream(AStream: TStream;
  SwapFacesOrientation: boolean): IBGRAObject3D;
var s: string;

  function GetNextToken: string;
  var idxStart,idxEnd: integer;
  begin
    idxStart := 1;
    while (idxStart <= length(s)) and (s[idxStart]=' ') do inc(idxStart);
    if idxStart > length(s) then
    begin
      result := '';
      exit;
    end;
    idxEnd := idxStart;
    while (idxEnd < length(s)) and (s[idxEnd+1]<> ' ') do inc(idxEnd);
    result := copy(s,idxStart, idxEnd-idxStart+1);
    delete(s,1,idxEnd);
    if pos('/',result) <> 0 then result := copy(result,1,pos('/',result)-1);
  end;

var lineType : string;
    x,y,z : single;
    code : integer;
    vertices: array of IBGRAVertex3D;
    NbVertices,v,i: integer;
    tempV: IBGRAVertex3D;
    materialname: string;
    face: IBGRAFace3D;
    lines: TStringList;
    lineIndex: integer;

begin
  lines := TStringList.Create;
  lines.LoadFromStream(AStream);
  result := CreateObject;
  vertices := nil;
  NbVertices:= 0;
  materialname := 'default';
  lineIndex := 0;
  while lineIndex < lines.Count do
  begin
    s := lines[lineIndex];
    inc(lineIndex);
    lineType := GetNextToken;
    if lineType = 'v' then
    begin
      val(GetNextToken,x,code);
      val(GetNextToken,y,code);
      val(GetNextToken,z,code);
      result.MainPart.Add(x,y,z);
    end else
    if lineType = 'usemtl' then
      materialname := trim(s)
    else
    if lineType = 'f' then
    begin
      NbVertices:= 0;
      repeat
        val(GetNextToken,v,code);
        if (code = 0) and (v >= 1) and (v <= result.MainPart.VertexCount) then
        begin
          if length(vertices) = nbvertices then
            setlength(vertices, length(vertices)*2+1);
          vertices[NbVertices] := result.MainPart.Vertex[v-1];
          inc(NbVertices);
        end else break;
      until false;
      if NbVertices > 2 then
      begin
        if SwapFacesOrientation then
          for i := 0 to NbVertices div 2-1 do
          begin
            tempV := vertices[i];
            vertices[i] := vertices[NbVertices-1-i];
            vertices[NbVertices-1-i] := tempV;
          end;
        face := result.AddFace(slice(vertices,NbVertices));
        face.MaterialName := materialname;
      end;
    end;
  end;
  lines.Free;
end;

procedure TBGRAScene3D.LookAt(AWhere: TPoint3D; ATopDir: TPoint3D);
begin
  FLookWhere := Point3D_128(AWhere);
  FTopDir := Point3D_128(ATopDir);
end;

procedure TBGRAScene3D.LookLeft(angleDeg: single);
var m,inv: TMatrix3D;
begin
  inv := MatrixInverse3D(FMatrix);
  m := MatrixRotateY(angleDeg*Pi/180);
  FLookWhere := inv*m*FMatrix*FLookWhere;
end;

procedure TBGRAScene3D.LookRight(angleDeg: single);
begin
  LookLeft(-angleDeg);
end;

procedure TBGRAScene3D.LookUp(angleDeg: single);
var m,inv: TMatrix3D;
begin
  inv := MatrixInverse3D(FMatrix);
  m := MatrixRotateX(-angleDeg*Pi/180);
  FLookWhere := inv*m*FMatrix*FLookWhere;
end;

procedure TBGRAScene3D.LookDown(angleDeg: single);
begin
  LookUp(-angleDeg);
end;

procedure TBGRAScene3D.Render;
begin
  InternalRender(FSurface, RenderingOptions.AntialiasingMode, 1);
end;

procedure TBGRAScene3D.ComputeView(ScaleX,ScaleY: single);
var
  i: Integer;
begin
  ComputeMatrix;

  FProjection.Zoom := Zoom;
  FProjection.Zoom.X *= ScaleX;
  FProjection.Zoom.Y *= ScaleY;
  FProjection.Center := ViewCenter;
  FProjection.Center.X *= ScaleX;
  FProjection.Center.Y *= ScaleY;
  for i := 0 to FObjectCount-1 do
    FObjects[i].ComputeWithMatrix(FMatrix, FProjection);
end;

function TBGRAScene3D.ComputeCoordinate(ASceneCoord: TPoint3D_128; APart: IBGRAPart3D): TPointF;
begin
  result := APart.ComputeCoordinate(ASceneCoord, FProjection);
end;

function TBGRAScene3D.ComputeCoordinate(AViewCoord: TPoint3D_128): TPointF;
var InvZ: single;
begin
  if AViewCoord.z > 0 then
  begin
    InvZ := 1/AViewCoord.z;
    result := PointF(AViewCoord.x*InvZ*FProjection.Zoom.x + FProjection.Center.x,
                     AViewCoord.y*InvZ*FProjection.Zoom.Y + FProjection.Center.y);
  end else
    result := PointF(0,0);
end;

procedure TBGRAScene3D.ComputeLight;
begin

end;

type
  arrayOfTBGRAFace3D = array of TBGRAFace3D;

procedure InsertionSortFaces(var a: arrayOfTBGRAFace3D);
var i,j: integer;
    temp: TBGRAFace3D;
begin
  for i := 1 to high(a) do
  begin
    Temp := a[i];
    j := i;
    while (j>0) and (a[j-1].ViewCenterZ > Temp.ViewCenterZ) do
    begin
      a[j] := a[j-1];
      dec(j);
    end;
    a[j] := Temp;
  end;
end;

function PartitionFaces(var a: arrayOfTBGRAFace3D; left,right: integer): integer;

  procedure Swap(idx1,idx2: integer); inline;
  var temp: TBGRAFace3D;
  begin
    temp := a[idx1];
    a[idx1] := a[idx2];
    a[idx2] := temp;
  end;

var pivotIndex: integer;
    pivotValue: TBGRAFace3D;
    storeIndex: integer;
    i: integer;

begin
  pivotIndex := left + random(right-left+1);
  pivotValue := a[pivotIndex];
  swap(pivotIndex,right);
  storeIndex := left;
  for i := left to right-1 do
    if a[i].ViewCenterZ <= pivotValue.ViewCenterZ then
    begin
      swap(i,storeIndex);
      inc(storeIndex);
    end;
  swap(storeIndex,right);
  result := storeIndex;
end;

procedure QuickSortFaces(var a: arrayOfTBGRAFace3D; left,right: integer);
var pivotNewIndex: integer;
begin
  if right > left+9 then
  begin
    pivotNewIndex := PartitionFaces(a,left,right);
    QuickSortFaces(a,left,pivotNewIndex-1);
    QuickSortFaces(a,pivotNewIndex+1,right);
  end;
end;

procedure SortFaces(var a: arrayOfTBGRAFace3D);
begin
  if length(a) < 10 then InsertionSortFaces(a) else
  begin
    QuickSortFaces(a,0,high(a));
    InsertionSortFaces(a);
  end;
end;

function IsPolyVisible(const p : array of TPointF; ori: integer = 1) : boolean;
var i: integer;
begin
  i := 0;
  while i<=high(p)-2 do
  begin
    if ori*
    ( (p[i+1].x-p[i].x)*(p[i+2].y-p[i].y) -
      (p[i+1].y-p[i].y)*(p[i+2].x-p[i].x)) > 0 then
    begin
        result := true;
        exit;
    end;
    inc(i);
  end;
  result := false;
end;

procedure TBGRAScene3D.InternalRender(ASurface: TBGRACustomBitmap; AAntialiasingMode: TAntialiasingMode3D; GlobalScale: single);
var
  LFaces: array of TBGRAFace3D;
  LFaceOpaque: array of boolean;
  LFaceCount: integer;

  procedure PrepareFaces;
  var i,j, LFaceIndex: integer;
      obj: IBGRAObject3D;
  begin
    LFaces := nil;
    LFaceCount := 0;
    for i := 0 to FObjectCount-1 do
    begin
      obj := FObjects[i];
      inc(LFaceCount, obj.GetFaceCount);
      if obj.GetParentLighting then
      begin
        obj.SetLightingNormal(Self.DefaultLightingNormal);
        obj.SetParentLighting(True);
      end;
    end;
    setlength(LFaces, LFaceCount);
    LFaceIndex := 0;
    for i := 0 to FObjectCount-1 do
      with FObjects[i] do
      begin
        for j := 0 to GetFaceCount-1 do
        begin
          LFaces[LFaceIndex] := TBGRAFace3D(GetFace(j).GetAsObject);
          inc(LFaceIndex);
        end;
      end;
  end;

var
  multi: TBGRAMultishapeFiller;
  ColorGradientTempBmp: TBGRACustomBitmap;
  zbuffer: psingle;

  LVertices: array of TBGRAVertex3D;
  LColors: array of TBGRAPixel;
  LTexCoord: array of TPointF;
  LZ: array of single;
  LProj: array of TPointF;
  LPos3D, LNormal3D: array of TPoint3D_128;
  LLighting: array of word;
  shaderContext: TMemoryBlockAlign128;
  lightingProc: TShaderFunction3D;
  UseAmbiantColor: boolean;

  procedure DrawFace(numFace: integer);

    procedure DrawAliasedColoredFace(shader: TShaderFunction3D; VCount: integer; context: PBasicLightingContext);
    var j,k: integer;
        SameColor: boolean;
        center: record
          proj: TPointF;
          pos3D,normal3D: TPoint3D_128;
          color: TBGRAPixel;
        end;

    begin
      SameColor := True;
      for j := 1 to VCount-1 do
        if (LColors[j]<>LColors[j-1]) then SameColor := False;

      if shader <> nil then
      begin
        if SameColor then
        begin
          BGRAPolygonAliased.PolygonPerspectiveMappingShaderAliased(ASurface,
            slice(LProj,VCount),slice(LPos3D,VCount),slice(LNormal3D,VCount),nil,
              slice(LTexCoord,VCount),False,shader,True,LColors[0],zbuffer,context);
        end else
        if VCount = 3 then
        begin
          ColorGradientTempBmp.SetPixel(0,0,LColors[0]);
          ColorGradientTempBmp.SetPixel(1,0,LColors[1]);
          ColorGradientTempBmp.SetPixel(0,1,LColors[2]);
          ColorGradientTempBmp.SetPixel(1,1,MergeBGRA(LColors[1],LColors[2]));
          BGRAPolygonAliased.PolygonPerspectiveMappingShaderAliased(ASurface,
            slice(LProj,VCount),slice(LPos3D,VCount),slice(LNormal3D,VCount),ColorGradientTempBmp,
              [PointF(0,0),PointF(1,0),PointF(0,1)],True,shader,True, BGRAPixelTransparent,zbuffer,context);
        end else
        if VCount = 4 then
        begin
          ColorGradientTempBmp.SetPixel(0,0,LColors[0]);
          ColorGradientTempBmp.SetPixel(1,0,LColors[1]);
          ColorGradientTempBmp.SetPixel(1,1,LColors[2]);
          ColorGradientTempBmp.SetPixel(0,1,LColors[3]);
          BGRAPolygonAliased.PolygonPerspectiveMappingShaderAliased(ASurface,
            slice(LProj,VCount),slice(LPos3D,VCount),slice(LNormal3D,VCount),ColorGradientTempBmp,
              [PointF(0,0),PointF(1,0),PointF(1,1),PointF(0,1)],True,shader,True, BGRAPixelTransparent,zbuffer,context);
        end else
        if VCount >= 3 then
        begin //split into triangles
          with center do
          begin
            ClearPoint3D_128(pos3D);
            ClearPoint3D_128(normal3D);
            color := MergeBGRA(slice(LColors,VCount));
          end;
          for j := 0 to VCount-1 do
          begin
            center.pos3D += LPos3D[j];
            center.normal3D += LNormal3D[j];
          end;
          with center do
          begin
            pos3D *= (1/VCount);
            Normalize3D_128(normal3D);
          end;
          center.proj := ComputeCoordinate(center.pos3D);
          k := VCount-1;
          for j := 0 to VCount-1 do
          begin
            ColorGradientTempBmp.SetPixel(0,0,LColors[k]);
            ColorGradientTempBmp.SetPixel(1,0,LColors[j]);
            ColorGradientTempBmp.SetPixel(0,1,center.color);
            ColorGradientTempBmp.SetPixel(1,1,MergeBGRA(LColors[j],center.color));
            BGRAPolygonAliased.PolygonPerspectiveMappingShaderAliased(ASurface,
              [LProj[k],LProj[j],center.proj], [LPos3D[k],LPos3D[j],center.pos3D],
              [LNormal3D[k],LNormal3D[j],center.normal3D], ColorGradientTempBmp,
                [PointF(0,0),PointF(1,0),PointF(0,1)],True,shader,True, BGRAPixelTransparent,zbuffer,context);
            k := j;
          end;
        end;
      end else
      begin
        if SameColor then
        begin
          if RenderingOptions.PerspectiveMode = pmZBuffer then
            BGRAPolygonAliased.PolygonPerspectiveColorGradientAliased(ASurface, slice(LProj,VCount),
            slice(LZ,VCount), slice(LColors,VCount),True,zbuffer)
          else
            ASurface.FillPoly(slice(LProj,VCount),LColors[0],dmDrawWithTransparency);
        end
        else
        begin
          if VCount > 4 then
          begin //split into triangles
            with center do
            begin
              ClearPoint3D_128(pos3D);
              color := MergeBGRA(slice(LColors,VCount));
            end;
            for j := 0 to VCount-1 do
              center.pos3D += LPos3D[j];
            with center do
              pos3D *= (1/VCount);
            center.proj := ComputeCoordinate(center.pos3D);
            k := VCount-1;
            if RenderingOptions.PerspectiveMode = pmLinearMapping then
            begin
              for j := 0 to VCount-1 do
              begin
                ASurface.FillPolyLinearColor([LProj[k],LProj[j],center.proj],[LColors[k],LColors[j],center.color]);
                k := j;
              end;
            end else
            begin
              for j := 0 to VCount-1 do
              begin
                BGRAPolygonAliased.PolygonPerspectiveColorGradientAliased(ASurface, [LProj[k],LProj[j],center.proj],
                 [LZ[k],LZ[j],center.pos3D.z], [LColors[k],LColors[j],center.color],True,zbuffer);
                k := j;
              end;
            end;
          end else
          begin
            if RenderingOptions.PerspectiveMode = pmLinearMapping then
              ASurface.FillPolyLinearColor(slice(LProj,VCount),slice(LColors,VCount))
            else
              BGRAPolygonAliased.PolygonPerspectiveColorGradientAliased(ASurface, slice(LProj,VCount),
               slice(LZ,VCount), slice(LColors,VCount),True,zbuffer);
          end;
        end;
      end;
    end;

  var
    j,k: Integer;
    LTexture: IBGRAScanner;
    LMaterial: TBGRAMaterial3D;
    SameColor: boolean;
    LLightNormal : TLightingNormal3D;
    LNoLighting: boolean;
    PtCenter: TPointF;
    PtCenter3D: TPoint3D_128;
    ColorCenter: TBGRAPixel;
    VCount,NewVCount: integer;
    ctx: PSceneLightingContext;
    NegNormals, UseDiffuseColor,
    UseDiffuseLightness{, OnlyDirectionalLight}: boolean;
    LastVisibleVertex: integer;

    procedure AddZIntermediate(n1,n2: integer);
    var t: single;
        v1,v2: TBGRAVertex3D;
    begin
       v1 := LVertices[n1];
       v2 := LVertices[n2];
       t := (RenderingOptions.MinZ - v1.ViewCoord.z)/(v2.ViewCoord.z - v1.ViewCoord.z);
       LVertices[NewVCount] := nil; //computed

       LColors[NewVCount] := MergeBGRA(LColors[n1],round((1-t)*65536),LColors[n2],round(t*65536));
       LTexCoord[NewVCount] := LTexCoord[n1]*(1-t) + LTexCoord[n2]*t;
       LPos3D[NewVCount] := LPos3D[n1]*(1-t) + LPos3D[n2]*t;
       LNormal3D[NewVCount] := LNormal3D[n1]*(1-t) + LNormal3D[n2]*t;
       LZ[NewVCount] := LZ[n1]*(1-t) + LZ[n2]*t;
       LProj[NewVCount] := ComputeCoordinate(LPos3D[NewVCount]);
       NewVCount += 1;
    end;

    procedure LoadVertex(idxL: integer; idxV: integer);
    var desc: PBGRAFaceVertexDescription;
        tempV: TBGRAVertex3D;
    begin
      with LFaces[numFace] do
      begin
        desc := VertexDescription[idxV];
        with desc^ do
        begin
          tempV := TBGRAVertex3D(vertex.GetAsObject);
          LVertices[idxL] := tempV;

          if LTexture <> nil then
            LColors[idxL] := BGRA(128,128,128)
          else
            if ColorOverride then
              LColors[idxL] := Color
            else
            begin
              if tempV.ParentColor then
                LColors[idxL] := Object3D.Color
              else
                LColors[idxL] := tempV.Color;
            end;

          if TexCoordOverride then
            LTexCoord[idxL] := TexCoord
          else
            LTexCoord[idxL] := tempV.TexCoord;

          with tempV.CoordData^ do
          begin
            LPos3D[idxL] := viewCoord;
            LNormal3D[idxL] := viewNormal;
            LProj[idxL] := projectedCoord;
            LZ[idxL] := viewCoord.Z;
          end;
        end;
      end;
    end;

  begin
     with LFaces[numFace] do
     begin
       VCount := VertexCount;
       if VCount < 3 then exit;

       if ParentTexture then
         LTexture := Object3D.Texture
       else
         LTexture := Texture;

       if Material <> nil then
         LMaterial := TBGRAMaterial3D(Material.GetAsObject)
       else if Object3D.Material <> nil then
         LMaterial := TBGRAMaterial3D(Object3D.Material.GetAsObject)
       else if self.DefaultMaterial <> nil then
         LMaterial := TBGRAMaterial3D(self.DefaultMaterial.GetAsObject)
       else
         exit;

       LLightNormal := Object3D.LightingNormal;

       if length(LVertices) < VCount+3 then  //keep margin for z-clip
       begin
         setlength(LVertices, (VCount+3)*2);
         setlength(LColors, length(LVertices));
         setlength(LTexCoord, length(LVertices));
         setlength(LZ, length(LVertices));
         setlength(LProj, length(LVertices));
         setlength(LPos3D, length(LVertices));
         setlength(LNormal3D, length(LVertices));
         setlength(LLighting, length(LVertices));
       end;

       NewVCount := 0;
       LastVisibleVertex := -1;
       for k := VCount-1 downto 0 do
         if Vertex[k].ViewCoordZ >= RenderingOptions.MinZ then
         begin
           LastVisibleVertex := k;
           break;
         end;
       if LastVisibleVertex = -1 then exit;

       k := VCount-1;
       for j := 0 to VCount-1 do
       begin
         if Vertex[j].ViewCoordZ >= RenderingOptions.MinZ then
         begin
           if k <> LastVisibleVertex then   //one or more vertices is out
           begin
             LoadVertex(NewVCount+1, LastVisibleVertex);
             LoadVertex(NewVCount+2, (LastVisibleVertex+1) mod VertexCount);
             AddZIntermediate(NewVCount+1,NewVCount+2);

             LoadVertex(NewVCount+1, j);
             LoadVertex(NewVCount+2, k);

             AddZIntermediate(NewVCount+1,NewVCount+2);
             inc(NewVCount);
           end else
           begin
             LoadVertex(NewVCount, j);
             NewVCount += 1;
           end;
           LastVisibleVertex := j;
         end;
         k := j;
       end;
       VCount := NewVCount;
       if VCount < 3 then exit; //after z-clipping

       if not IsPolyVisible(slice(LProj,VCount)) then
       begin
         if not Biface then exit;
         NegNormals := True;
       end else
       begin
         NegNormals := False;
       end;

       //from here we assume the face will be drawn
       inc(FRenderedFaceCount);

       //compute normals
       case LLightNormal of
         lnFace: for j := 0 to VCount-1 do
                   LNormal3D[j] := ViewNormal_128;
         lnFaceVertexMix:
             for j := 0 to VCount-1 do
             begin
               LNormal3D[j] += ViewNormal_128;
               Normalize3D_128(LNormal3D[j]);
             end;
       end;
       if NegNormals then
         for j := 0 to VCount-1 do
           LNormal3D[j] := -LNormal3D[j];

       //prepare lighting
       {OnlyDirectionalLight := true;
       for j := 0 to LightCount-1 do
         if not Light[j].IsDirectional then OnlyDirectionalLight := false; }

       if LMaterial.GetSpecularOn then
        lightingProc:= TShaderFunction3D(@ApplyLightingWithDiffuseAndSpecularColor) else
       begin
         UseDiffuseColor := UseAmbiantColor;
         if not UseDiffuseColor then
         begin
           with LMaterial.GetDiffuseColorInt do
            UseDiffuseColor := (r <> g) or (g <> b);
           if not UseDiffuseColor and LMaterial.GetAutoDiffuseColor then
           begin
             for j := 0 to LightCount-1 do
               if Light[j].ColoredLight then
               begin
                 UseDiffuseColor := true;
                 break;
               end;
           end;
         end;
         if UseDiffuseColor then
           lightingProc := TShaderFunction3D(@ApplyLightingWithDiffuseColor) else
         begin
           UseDiffuseLightness := FAmbiantLightness <> 32768;
           if not UseDiffuseLightness then
           begin
             if LightCount <> 0 then
               UseDiffuseLightness := true;
           end;

           if UseDiffuseLightness then
             lightingProc := TShaderFunction3D(@ApplyLightingWithLightness) else
           if FAmbiantLightness <> 32768 then
            lightingProc := TShaderFunction3D(@ApplyLightingWithAmbiantLightnessOnly) else
              lightingProc := TShaderFunction3D(@ApplyNoLighting);
         end;
       end;

       ctx := PSceneLightingContext( shaderContext.Data );
       ctx^.material := LMaterial;
       if LightThroughFactorOverride then
         ctx^.LightThroughFactor := LightThroughFactor
       else
         ctx^.LightThroughFactor := LMaterial.GetLightThroughFactor;
       ctx^.LightThrough := ctx^.LightThroughFactor > 0;
       ctx^.SaturationHighF := LMaterial.GetSaturationHigh;
       ctx^.SaturationLowF := LMaterial.GetSaturationLow;
       ctx^.SaturationHigh := round(LMaterial.GetSaturationHigh*32768);
       ctx^.SaturationLow := round(LMaterial.GetSaturationLow*32768);

       //high-quality lighting interpolation, necessary for Phong and high-quality Gouraud
       if (
           (RenderingOptions.LightingInterpolation = liAlwaysHighQuality) or
           ((RenderingOptions.LightingInterpolation = liSpecularHighQuality) and LMaterial.GetSpecularOn)
       ) and (LLightNormal <> lnNone) {and (not (LLightNormal = lnFace) and OnlyDirectionalLight) }then
       begin
         if LTexture = nil then
           DrawAliasedColoredFace(lightingProc,VCount,PBasicLightingContext(ctx)) //use shader
         else
           BGRAPolygonAliased.PolygonPerspectiveMappingShaderAliased(ASurface,
               slice(LProj,VCount),slice(LPos3D,VCount),slice(LNormal3D,VCount),LTexture,
                 slice(LTexCoord,VCount),RenderingOptions.TextureInterpolation,lightingProc,True, BGRAPixelTransparent,zbuffer,PBasicLightingContext(ctx));

         exit;
       end;

       //Vertex lighting interpolation (low-quality Gouraud, low-quality Phong)
       LNoLighting := True;
       for j := 0 to VCount-1 do
       begin
         with ctx^ do
         begin
           basic.Position := LPos3D[j];
           basic.Normal := LNormal3D[j];
         end;
         LColors[j] := lightingProc(PBasicLightingContext(ctx),LColors[j]);
         if LColors[j] <> BGRA(128,128,128) then
           LNoLighting := false;
       end;

       if (AAntialiasingMode = am3dMultishape) and not (RenderingOptions.PerspectiveMode = pmZBuffer) then //high-quality antialiasing
       begin
         if LTexture <> nil then
         begin
           if (RenderingOptions.PerspectiveMode <> pmLinearMapping) and (VCount=4) then
             multi.AddQuadPerspectiveMapping(LProj[0],LProj[1],LProj[2],LProj[3],LTexture,LTexCoord[0],LTexCoord[1],LTexCoord[2],LTexCoord[3])
           else
           if VCount>=3 then
           begin
             for j := 0 to VCount-3 do
               multi.AddTriangleLinearMapping(LProj[j],LProj[j+1],LProj[j+2],LTexture,LTexCoord[j],LTexCoord[j+1],LTexCoord[j+2]);
           end;
         end
         else
         begin
           SameColor := True;
           for j := 1 to VCount-1 do
             if (LColors[j]<>LColors[j-1]) then SameColor := False;

           if SameColor then
             multi.AddPolygon(slice(LProj,VCount),LColors[0])
           else
           if VCount=3 then
             multi.AddTriangleLinearColor(LProj[0],LProj[1],LProj[2],LColors[0],LColors[1],LColors[2])
           else
           if VCount>=3 then
           begin  //split into triangles
             PtCenter3D := Point3D_128_Zero;
             for j := 0 to VCount-1 do
               PtCenter3D += LPos3D[j];
             PtCenter3D *= (1/VCount);
             PtCenter := ComputeCoordinate(PtCenter3D);
             ColorCenter := MergeBGRA(slice(LColors,VCount));
             k := VCount-1;
             for j := 0 to VCount-1 do
             begin
               multi.AddTriangleLinearColor(LProj[k],LProj[j],PtCenter,LColors[k],LColors[j],ColorCenter);
               k := j;
             end;
           end;
         end;
       end else
       begin
         if LTexture <> nil then
         begin
           if LNoLighting then
           begin
             if RenderingOptions.PerspectiveMode <> pmLinearMapping then
               ASurface.FillPolyPerspectiveMapping(slice(LProj,VCount),slice(LZ,VCount),LTexture,slice(LTexCoord,VCount),RenderingOptions.TextureInterpolation, zbuffer)
             else
               ASurface.FillPolyLinearMapping(slice(LProj,VCount),LTexture,slice(LTexCoord,VCount),RenderingOptions.TextureInterpolation);
           end else
           begin
             for j := 0 to VCount-1 do
               LLighting[j] := LColors[j].green shl 8;
             if RenderingOptions.PerspectiveMode <> pmLinearMapping then
               ASurface.FillPolyPerspectiveMappingLightness(slice(LProj,VCount),slice(LZ,VCount),LTexture,slice(LTexCoord,VCount),slice(LLighting,VCount),RenderingOptions.TextureInterpolation, zbuffer)
             else
               ASurface.FillPolyLinearMappingLightness(slice(LProj,VCount),LTexture,slice(LTexCoord,VCount),slice(LLighting,VCount),RenderingOptions.TextureInterpolation);
           end;
         end
         else
           DrawAliasedColoredFace(nil,VCount,PBasicLightingContext(ctx));  //already low-quality shaded
       end;
     end;
  end;

  procedure DrawWithResample;
  var
    tempSurface: TBGRACustomBitmap;
  begin
    tempSurface := ASurface.NewBitmap(ASurface.Width*RenderingOptions.AntialiasingResampleLevel,ASurface.Height*RenderingOptions.AntialiasingResampleLevel);
    InternalRender(tempSurface, am3dNone, RenderingOptions.AntialiasingResampleLevel);
    BGRAResample.DownSamplePutImage(tempSurface,RenderingOptions.AntialiasingResampleLevel,RenderingOptions.AntialiasingResampleLevel,
                 ASurface, 0,0, dmDrawWithTransparency);
    tempSurface.Free;
  end;

var i,j: integer;

begin
  FRenderedFaceCount:= 0;

  if ASurface = nil then
    raise exception.Create('No surface specified');

  if (AAntialiasingMode = am3dResample) and (RenderingOptions.AntialiasingResampleLevel > 1) then
  begin
    DrawWithResample;
    exit;
  end;

  PrepareFaces;
  ComputeView(GlobalScale,GlobalScale);
  ComputeLight;
  UseAmbiantColor := (FAmbiantLightColor.r <> FAmbiantLightColor.g) or (FAmbiantLightColor.g <> FAmbiantLightColor.b);

  SortFaces(LFaces);
  LVertices := nil;

  if AAntialiasingMode = am3dMultishape then
  begin
    multi := TBGRAMultishapeFiller.Create;
    multi.PolygonOrder := poLastOnTop;
  end
  else
    multi := nil;

  ColorGradientTempBmp := ASurface.NewBitmap(2,2);
  ColorGradientTempBmp.ScanInterpolationFilter := rfLinear;

  if RenderingOptions.PerspectiveMode = pmZBuffer then
  begin
    getmem(zbuffer, ASurface.NbPixels*sizeof(single));
    FillDWord(zbuffer^, ASurface.NbPixels, dword(single(0)));
  end
  else
    zbuffer := nil;

  shaderContext := TMemoryBlockAlign128.Create(sizeof(TSceneLightingContext));

  if zbuffer <> nil then
  begin
    setlength(LFaceOpaque, length(LFaces));
    for i := 0 to High(LFaces) do
    begin
      if (LFaces[i].Texture = nil) then
      begin
        LFaceOpaque[i] := true;
        with LFaces[i] do
          for j := 0 to VertexCount-1 do
            if VertexColor[j].alpha <> 255 then
            begin
              LFaceOpaque[i] := false;
              break;
            end;
      end else
        LFaceOpaque[i] := true;
    end;

    //draw near opaque faces first
    for i := 0 to High(LFaces) do
      if LFaceOpaque[i] then DrawFace(i);

    //draw other faces
    for i := High(LFaces) downto 0 do
      if not LFaceOpaque[i] then DrawFace(i);
  end else
  begin
    for i := High(LFaces) downto 0 do
      DrawFace(i);
  end;

  shaderContext.Free;
  if zbuffer <> nil then freemem(zbuffer);
  ColorGradientTempBmp.Free;

  if multi <> nil then
  begin
    multi.Draw(ASurface);
    multi.Free;
  end;
end;

procedure TBGRAScene3D.SetViewPoint(const AValue: TPoint3D);
begin
  FViewPoint := Point3D_128(AValue);
end;

function TBGRAScene3D.ApplyLightingWithLightness(Context: PSceneLightingContext;
  Color: TBGRAPixel): TBGRAPixel;
var i: Integer;
begin
  Context^.lightness := FAmbiantLightness;

  i := FLights.Count-1;
  while i >= 0 do
  begin
    TBGRALight3D(FLights[i]).ComputeDiffuseLightness(Context);
    dec(i);
  end;

  with Context^ do
    if Lightness <= 0 then
      result := BGRA(0,0,0,color.alpha)
    else
    begin
      if Lightness <= SaturationLow then
        result := ApplyIntensityFast(Color, Lightness)
      else if Lightness >= SaturationHigh then
        result := BGRA(255,255,255,color.alpha)
      else
        result := ApplyLightnessFast( ApplyIntensityFast(Color, SaturationLow),
                              (Lightness - SaturationLow)*32767 div (SaturationHigh-SaturationLow)+32768 );
    end;
end;

function TBGRAScene3D.ApplyLightingWithDiffuseColor(Context: PSceneLightingContext;
  Color: TBGRAPixel): TBGRAPixel;
var i: Integer;
begin
  Context^.diffuseColor := FAmbiantLightColor;

  i := FLights.Count-1;
  while i >= 0 do
  begin
    TBGRALight3D(FLights[i]).ComputeDiffuseColor(Context);
    dec(i);
  end;

  result := ColorIntToBGRA(BGRAToColorIntMultiply(Color,Context^.diffuseColor));
  result.alpha := Color.alpha;
end;

function TBGRAScene3D.ApplyLightingWithDiffuseAndSpecularColor(Context: PSceneLightingContext;
  Color: TBGRAPixel): TBGRAPixel;
var i: Integer;
begin
  Context^.diffuseColor := FAmbiantLightColor;
  Context^.specularColor := ColorInt65536(0,0,0,0);

  i := FLights.Count-1;
  while i >= 0 do
  begin
    TBGRALight3D(FLights[i]).ComputeDiffuseAndSpecularColor(Context);
    dec(i);
  end;

  with Context^ do
  begin
    diffuseColor.a := 65536;
    result := ColorIntToBGRA(BGRAToColorIntMultiply(Color,diffuseColor) + specularColor);
  end;
end;

{$hints off}
function TBGRAScene3D.ApplyNoLighting(Context: PSceneLightingContext;
  Color: TBGRAPixel): TBGRAPixel;
begin
  result := Color;
end;

function TBGRAScene3D.ApplyLightingWithAmbiantLightnessOnly(
  Context: PSceneLightingContext; Color: TBGRAPixel): TBGRAPixel;
begin
  if FAmbiantLightness <= 0 then
    result := BGRA(0,0,0,color.alpha)
  else
    result := ApplyIntensityFast(Color, FAmbiantLightness);
end;

{$hints on}

function TBGRAScene3D.CreateObject: IBGRAObject3D;
begin
  result := TBGRAObject3D.Create(self);
  AddObject(result);
end;

function TBGRAScene3D.CreateObject(ATexture: IBGRAScanner): IBGRAObject3D;
begin
  result := TBGRAObject3D.Create(self);
  result.Texture := ATexture;
  AddObject(result);
end;

function TBGRAScene3D.CreateObject(AColor: TBGRAPixel): IBGRAObject3D;
begin
  result := TBGRAObject3D.Create(self);
  result.Color := AColor;
  AddObject(result);
end;

function TBGRAScene3D.CreateSphere(ARadius: Single; AHorizPrecision: integer; AVerticalPrecision : integer): IBGRAObject3D;
begin
  result := TBGRASphere3D.Create(self, ARadius, AHorizPrecision, AVerticalPrecision);
  AddObject(result);
end;

function TBGRAScene3D.CreateSphere(ARadius: Single; AColor: TBGRAPixel; AHorizPrecision: integer; AVerticalPrecision : integer): IBGRAObject3D;
begin
  result := TBGRASphere3D.Create(self, ARadius, AHorizPrecision, AVerticalPrecision);
  result.Color := AColor;
  AddObject(result);
end;

function TBGRAScene3D.CreateHalfSphere(ARadius: Single;
  AHorizPrecision: integer; AVerticalPrecision: integer): IBGRAObject3D;
begin
  result := TBGRASphere3D.Create(self, ARadius, AHorizPrecision, AVerticalPrecision, True);
  AddObject(result);
end;

function TBGRAScene3D.CreateHalfSphere(ARadius: Single; AColor: TBGRAPixel;
  AHorizPrecision: integer; AVerticalPrecision: integer): IBGRAObject3D;
begin
  result := TBGRASphere3D.Create(self, ARadius, AHorizPrecision, AVerticalPrecision, True);
  result.Color := AColor;
  AddObject(result);
end;

procedure TBGRAScene3D.RemoveObject(AObject: IBGRAObject3D);
var
  i,j: Integer;
begin
  for i := FObjectCount-1 downto 0 do
    if FObjects[i] = AObject then
    begin
      dec(FObjectCount);
      FObjects[i] := nil;
      for j := i to FObjectCount-1 do
        FObjects[j] := FObjects[j+1];
    end;
end;

function TBGRAScene3D.AddDirectionalLight(ADirection: TPoint3D;
  ALightness: single; AMinIntensity: single): IBGRADirectionalLight3D;
var lightObj: TBGRADirectionalLight3D;
begin
  lightObj := TBGRADirectionalLight3D.Create(ADirection);
  result := lightObj;
  result.ColorF := ColorF(ALightness,ALightness,ALightness,1);
  result.MinIntensity := AMinIntensity;
  AddLight(lightObj);
end;

function TBGRAScene3D.AddPointLight(AVertex: IBGRAVertex3D;
  AOptimalDistance: single; ALightness: single; AMinIntensity: single
  ): IBGRAPointLight3D;
var lightObj: TBGRAPointLight3D;
begin
  lightObj := TBGRAPointLight3D.Create(AVertex, ALightness*sqr(AOptimalDistance));
  result := lightObj;
  result.MinIntensity := AMinIntensity;
  AddLight(lightObj);
end;

function TBGRAScene3D.AddDirectionalLight(ADirection: TPoint3D;
  AColor: TBGRAPixel; AMinIntensity: single): IBGRADirectionalLight3D;
var lightObj: TBGRADirectionalLight3D;
begin
  lightObj := TBGRADirectionalLight3D.Create(ADirection);
  result := lightObj;
  result.MinIntensity := AMinIntensity;
  result.Color := AColor;
  AddLight(lightObj);
end;

function TBGRAScene3D.AddPointLight(AVertex: IBGRAVertex3D;
  AOptimalDistance: single; AColor: TBGRAPixel; AMinIntensity: single
  ): IBGRAPointLight3D;
var lightObj: TBGRAPointLight3D;
begin
  lightObj := TBGRAPointLight3D.Create(AVertex,sqr(AOptimalDistance));
  result := lightObj;
  result.Color := AColor;
  result.MinIntensity := AMinIntensity;
  AddLight(lightObj);
end;

procedure TBGRAScene3D.RemoveLight(ALight: IBGRALight3D);
var idx: integer;
begin
  idx := FLights.IndexOf(ALight.GetAsObject);
  if idx <> -1 then
  begin
    ALight._Release;
    FLights.Delete(Idx);
  end;
end;

procedure TBGRAScene3D.SetZoom(value: Single);
begin
  SetZoom(PointF(value,value));
end;

procedure TBGRAScene3D.SetZoom(value: TPointF);
begin
  FZoom := value;
  FAutoZoom := false;
end;

function TBGRAScene3D.CreateMaterial: IBGRAMaterial3D;
begin
  result := TBGRAMaterial3D.Create;
  AddMaterial(result);
end;

function TBGRAScene3D.CreateMaterial(ASpecularIndex: integer): IBGRAMaterial3D;
begin
  result := TBGRAMaterial3D.Create;
  result.SpecularIndex := ASpecularIndex;
  result.SpecularColor := BGRAWhite;
  AddMaterial(result);
end;

procedure TBGRAScene3D.UpdateMaterials;
var i,j: integer;
  obj: IBGRAObject3D;
  face: IBGRAFace3D;
begin
  for i := 0 to Object3DCount-1 do
  begin
    obj := Object3D[i];
    for j := 0 to obj.FaceCount-1 do
    begin
      face := obj.Face[j];
      if face.MaterialName <> '' then
        UseMaterial(face.MaterialName,face);
    end;
  end;
end;

procedure TBGRAScene3D.UpdateMaterial(AMaterialName: string);
var i,j: integer;
  obj: IBGRAObject3D;
  face: IBGRAFace3D;
begin
  for i := 0 to Object3DCount-1 do
  begin
    obj := Object3D[i];
    for j := 0 to obj.FaceCount-1 do
    begin
      face := obj.Face[j];
      if face.MaterialName = AMaterialName then
        UseMaterial(face.MaterialName,face);
    end;
  end;
end;

procedure TBGRAScene3D.ForEachVertex(ACallback: TVertex3DCallback);
var i: integer;
begin
  for i := 0 to Object3DCount-1 do
    Object3D[i].ForEachVertex(ACallback);
end;

procedure TBGRAScene3D.ForEachFace(ACallback: TFace3DCallback);
var i: integer;
begin
  for i := 0 to Object3DCount-1 do
    Object3D[i].ForEachFace(ACallback);
end;

initialization

  Randomize;

end.

