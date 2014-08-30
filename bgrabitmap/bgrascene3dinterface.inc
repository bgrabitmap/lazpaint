type
  IBGRAVertex3D = interface;

  { IBGRALight3D }

  IBGRALight3D = interface
    procedure ComputeDiffuseLightness(Context: PSceneLightingContext);
    procedure ComputeDiffuseColor(Context: PSceneLightingContext);
    procedure ComputeDiffuseAndSpecularColor(Context: PSceneLightingContext);

    function GetColor: TBGRAPixel;
    function GetColoredLight: boolean;
    function GetColorF: TColorF;
    function GetColorInt: TColorInt65536;
    function GetLightnessF: single;
    function GetAsObject: TObject;
    procedure SetColor(const AValue: TBGRAPixel);
    procedure SetColorF(const AValue: TColorF);
    procedure SetColorInt(const AValue: TColorInt65536);
    property Color: TBGRAPixel read GetColor write SetColor;
    property ColorF: TColorF read GetColorF write SetColorF;
    property ColorInt: TColorInt65536 read GetColorInt write SetColorInt;
    property LightnessF: single read GetLightnessF;
    property ColoredLight: boolean read GetColoredLight;

    function GetMinIntensity: single;
    procedure SetMinIntensity(const AValue: single);
    property MinIntensity: single read GetMinIntensity write SetMinIntensity;
    function IsDirectional: boolean;
  end;

  IBGRAPointLight3D = interface(IBGRALight3D)
    function GetVertex: IBGRAVertex3D;
    procedure SetVertex(const AValue: IBGRAVertex3D);
    property Vertex: IBGRAVertex3D read GetVertex write SetVertex;
  end;

  IBGRADirectionalLight3D = interface(IBGRALight3D)
    function GetDirection: TPoint3D;
    procedure SetDirection(const AValue: TPoint3D);
    property Direction: TPoint3D read GetDirection write SetDirection;
  end;

  { IBGRAMaterial3D }

  IBGRAMaterial3D = interface
    function GetAutoDiffuseColor: boolean;
    function GetAutoSpecularColor: boolean;
    function GetDiffuseColor: TBGRAPixel;
    function GetDiffuseColorF: TColorF;
    function GetDiffuseColorInt: TColorInt65536;
    function GetLightThroughFactor: single;
    function GetSaturationHigh: single;
    function GetSaturationLow: single;
    function GetSpecularColor: TBGRAPixel;
    function GetSpecularColorF: TColorF;
    function GetSpecularColorInt: TColorInt65536;
    function GetSpecularIndex: integer;
    function GetSpecularOn: boolean;
    procedure SetAutoDiffuseColor(const AValue: boolean);
    procedure SetAutoSpecularColor(const AValue: boolean);
    procedure SetDiffuseColor(const AValue: TBGRAPixel);
    procedure SetDiffuseColorF(const AValue: TColorF);
    procedure SetDiffuseColorInt(const AValue: TColorInt65536);
    procedure SetLightThroughFactor(const AValue: single);
    procedure SetSaturationHigh(const AValue: single);
    procedure SetSaturationLow(const AValue: single);
    procedure SetSpecularColor(const AValue: TBGRAPixel);
    procedure SetSpecularColorF(const AValue: TColorF);
    procedure SetSpecularColorInt(const AValue: TColorInt65536);
    procedure SetSpecularIndex(const AValue: integer);
    function GetAsObject: TObject;

    property DiffuseColor: TBGRAPixel read GetDiffuseColor write SetDiffuseColor;
    property DiffuseColorF: TColorF read GetDiffuseColorF write SetDiffuseColorF;
    property DiffuseColorInt: TColorInt65536 read GetDiffuseColorInt write SetDiffuseColorInt;
    property AutoDiffuseColor: boolean read GetAutoDiffuseColor write SetAutoDiffuseColor;
    property SaturationLow: single read GetSaturationLow write SetSaturationLow;
    property SaturationHigh: single read GetSaturationHigh write SetSaturationHigh;

    property SpecularColor: TBGRAPixel read GetSpecularColor write SetSpecularColor;
    property SpecularColorF: TColorF read GetSpecularColorF write SetSpecularColorF;
    property SpecularColorInt: TColorInt65536 read GetSpecularColorInt write SetSpecularColorInt;
    property AutoSpecularColor: boolean read GetAutoSpecularColor write SetAutoSpecularColor;
    property SpecularIndex: integer read GetSpecularIndex write SetSpecularIndex;
    property SpecularOn: boolean read GetSpecularOn;

    property LightThroughFactor: single read GetLightThroughFactor write SetLightThroughFactor;
  end;

  { IBGRAVertex3D }

  IBGRAVertex3D = interface
    function GetColor: TBGRAPixel;
    function GetCustomFlags: DWord;
    function GetLight: Single;
    function GetProjectedCoord: TPointF;
    function GetUsage: integer;
    function GetViewNormal: TPoint3D;
    function GetViewNormal_128: TPoint3D_128;
    function GetParentColor: Boolean;
    function GetSceneCoord: TPoint3D;
    function GetSceneCoord_128: TPoint3D_128;
    function GetTexCoord: TPointF;
    function GetViewCoord: TPoint3D;
    function GetViewCoord_128: TPoint3D_128;
    procedure ComputeCoordinateAndClearNormal(const AMatrix: TMatrix3D; const AProjection: TProjection3D);
    function GetViewCoordZ: single;
    procedure SetColor(const AValue: TBGRAPixel);
    procedure SetCustomFlags(AValue: DWord);
    procedure SetLight(const AValue: Single);
    procedure SetProjectedCoord(const AValue: TPointF);
    procedure SetViewNormal(const AValue: TPoint3D);
    procedure SetViewNormal_128(const AValue: TPoint3D_128);
    procedure SetParentColor(const AValue: Boolean);
    procedure SetSceneCoord(const AValue: TPoint3D);
    procedure SetSceneCoord_128(const AValue: TPoint3D_128);
    procedure SetTexCoord(const AValue: TPointF);
    procedure SetViewCoord(const AValue: TPoint3D);
    procedure SetViewCoord_128(const AValue: TPoint3D_128);
    procedure NormalizeViewNormal;
    procedure AddViewNormal(const AValue: TPoint3D_128);
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
    property CustomFlags: DWord read GetCustomFlags write SetCustomFlags;
    function GetAsObject: TObject;
  end;

  arrayOfIBGRAVertex3D = array of IBGRAVertex3D;
  TVertex3DCallback = procedure(AVertex: IBGRAVertex3D) of object;

  { IBGRAPart3D }

  IBGRAPart3D = interface
    procedure Clear(ARecursive: boolean);
    function Add(x,y,z: single): IBGRAVertex3D;
    function Add(pt: TPoint3D): IBGRAVertex3D;
    function Add(pt: TPoint3D_128): IBGRAVertex3D;
    function Add(const coords: array of single): arrayOfIBGRAVertex3D;
    function Add(const pts: array of TPoint3D): arrayOfIBGRAVertex3D;
    function Add(const pts_128: array of TPoint3D_128): arrayOfIBGRAVertex3D;
    procedure Add(const pts: array of IBGRAVertex3D);
    procedure Add(AVertex: IBGRAVertex3D);
    function IndexOf(AVertex: IBGRAVertex3D): integer;
    procedure RemoveVertex(Index: integer);
    function GetBoundingBox: TBox3D;
    function GetMatrix: TMatrix3D;
    function GetPart(AIndex: Integer): IBGRAPart3D;
    function GetPartCount: integer;
    function GetRadius: single;
    function GetVertex(AIndex: Integer): IBGRAVertex3D;
    function GetVertexCount: integer;
    function GetTotalVertexCount: integer;
    function GetContainer: IBGRAPart3D;
    procedure ResetTransform;
    procedure Scale(size: single; Before: boolean = true);
    procedure Scale(x,y,z: single; Before: boolean = true);
    procedure Scale(size: TPoint3D; Before: boolean = true);
    procedure SetMatrix(const AValue: TMatrix3D);
    procedure SetVertex(AIndex: Integer; const AValue: IBGRAVertex3D);
    procedure Translate(x,y,z: single; Before: boolean = true);
    procedure Translate(ofs: TPoint3D; Before: boolean = true);
    procedure RotateXDeg(angle: single; Before: boolean = true);
    procedure RotateYDeg(angle: single; Before: boolean = true);
    procedure RotateZDeg(angle: single; Before: boolean = true);
    procedure RotateXRad(angle: single; Before: boolean = true);
    procedure RotateYRad(angle: single; Before: boolean = true);
    procedure RotateZRad(angle: single; Before: boolean = true);
    procedure ComputeWithMatrix(const AMatrix: TMatrix3D; const AProjection: TProjection3D);
    function ComputeCoordinate(var ASceneCoord: TPoint3D_128; const AProjection: TProjection3D): TPointF;
    procedure NormalizeViewNormal;
    procedure LookAt(AWhere: TPoint3D; ATopDir: TPoint3D);
    procedure RemoveUnusedVertices;
    function CreatePart: IBGRAPart3D;
    procedure ForEachVertex(ACallback: TVertex3DCallback);
    property VertexCount: integer read GetVertexCount;
    property Vertex[AIndex: Integer]: IBGRAVertex3D read GetVertex write SetVertex;
    property Matrix: TMatrix3D read GetMatrix write SetMatrix;
    property PartCount: integer read GetPartCount;
    property Part[AIndex: Integer]: IBGRAPart3D read GetPart;
    property Radius: single read GetRadius;
    property BoundingBox: TBox3D read GetBoundingBox;
    property TotalVertexCount: integer read GetTotalVertexCount;
    property Container: IBGRAPart3D read GetContainer;
  end;

  IBGRAObject3D = interface;

  { IBGRAFace3D }

  IBGRAFace3D = interface
    procedure AddVertex(AVertex: IBGRAVertex3D);
    function GetBiface: boolean;
    function GetCustomFlags: DWord;
    function GetLightThroughFactorOverride: boolean;
    function GetMaterial: IBGRAMaterial3D;
    function GetMaterialName: string;
    function GetObject3D: IBGRAObject3D;
    function GetParentTexture: boolean;
    function GetTexCoord(AIndex: Integer): TPointF;
    function GetTexCoordOverride(AIndex: Integer): boolean;
    function GetTexture: IBGRAScanner;
    function GetVertex(AIndex: Integer): IBGRAVertex3D;
    function GetVertexColor(AIndex: Integer): TBGRAPixel;
    function GetVertexColorOverride(AIndex: Integer): boolean;
    function GetVertexCount: integer;
    function GetViewCenter: TPoint3D;
    function GetViewCenter_128: TPoint3D_128;
    function GetViewCenterZ: single;
    function GetViewNormal: TPoint3D;
    function GetViewNormal_128: TPoint3D_128;
    function GetLightThroughFactor: single;
    procedure SetCustomFlags(AValue: DWord);
    procedure SetLightThroughFactor(const AValue: single);
    procedure SetBiface(const AValue: boolean);
    procedure SetLightThroughFactorOverride(const AValue: boolean);
    procedure SetMaterial(const AValue: IBGRAMaterial3D);
    procedure SetMaterialName(const AValue: string);
    procedure SetParentTexture(const AValue: boolean);
    procedure SetTexCoord(AIndex: Integer; const AValue: TPointF);
    procedure SetTexCoordOverride(AIndex: Integer; const AValue: boolean);
    procedure SetTexture(const AValue: IBGRAScanner);
    procedure SetVertex(AIndex: Integer; const AValue: IBGRAVertex3D);
    procedure SetVertexColor(AIndex: Integer; const AValue: TBGRAPixel);
    procedure SetVertexColorOverride(AIndex: Integer; const AValue: boolean);
    procedure ComputeViewNormalAndCenter;
    procedure SetColor(AColor: TBGRAPixel);
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
    property MaterialName: string read GetMaterialName write SetMaterialName;
    function GetAsObject: TObject;
    property CustomFlags: DWord read GetCustomFlags write SetCustomFlags;
  end;

  TFace3DCallback = procedure(AFace: IBGRAFace3D) of object;

  { IBGRAObject3D }

  IBGRAObject3D = interface
    procedure Clear;
    function GetColor: TBGRAPixel;
    function GetFace(AIndex: integer): IBGRAFace3D;
    function GetFaceCount: integer;
    function GetMaterial: IBGRAMaterial3D;
    function GetRefCount: integer;
    function GetTotalVertexCount: integer;
    function GetLight: Single;
    function GetLightingNormal: TLightingNormal3D;
    function GetParentLighting: boolean;
    function GetTexture: IBGRAScanner;
    function GetMainPart: IBGRAPart3D;
    function GetScene: TBGRAScene3D;
    procedure SetColor(const AValue: TBGRAPixel);
    procedure SetLight(const AValue: Single);
    procedure SetLightingNormal(const AValue: TLightingNormal3D);
    procedure SetMaterial(const AValue: IBGRAMaterial3D);
    procedure SetParentLighting(const AValue: boolean);
    procedure SetTexture(const AValue: IBGRAScanner);
    procedure ComputeWithMatrix(constref AMatrix: TMatrix3D; constref AProjection: TProjection3D);
    procedure RemoveUnusedVertices;
    procedure ForEachVertex(ACallback: TVertex3DCallback);
    procedure ForEachFace(ACallback: TFace3DCallback);
    function AddFaceReversed(const AVertices: array of IBGRAVertex3D): IBGRAFace3D;
    function AddFace(const AVertices: array of IBGRAVertex3D): IBGRAFace3D;
    function AddFace(const AVertices: array of IBGRAVertex3D; ABiface: boolean): IBGRAFace3D;
    function AddFace(const AVertices: array of IBGRAVertex3D; ATexture: IBGRAScanner): IBGRAFace3D;
    function AddFace(const AVertices: array of IBGRAVertex3D; AColor: TBGRAPixel): IBGRAFace3D;
    function AddFace(const AVertices: array of IBGRAVertex3D; AColors: array of TBGRAPixel): IBGRAFace3D;
    procedure SetBiface(AValue : boolean);
    procedure SeparatePart(APart: IBGRAPart3D);
    property MainPart: IBGRAPart3D read GetMainPart;
    property Texture: IBGRAScanner read GetTexture write SetTexture;
    property Light: Single read GetLight write SetLight;
    property Color: TBGRAPixel read GetColor write SetColor;
    property Face[AIndex: integer]: IBGRAFace3D read GetFace;
    property FaceCount: integer read GetFaceCount;
    property LightingNormal: TLightingNormal3D read GetLightingNormal write SetLightingNormal;
    property ParentLighting: boolean read GetParentLighting write SetParentLighting;
    property TotalVertexCount: integer read GetTotalVertexCount;
    property Material: IBGRAMaterial3D read GetMaterial write SetMaterial;
    property Scene: TBGRAScene3D read GetScene;
    property RefCount: integer read GetRefCount;
  end;
