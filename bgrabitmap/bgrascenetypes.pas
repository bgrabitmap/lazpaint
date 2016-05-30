unit BGRASceneTypes;

{$mode objfpc}{$H+}

interface

uses BGRABitmapTypes, BGRASSE, BGRAMatrix3D, BGRAColorInt;

type
  TLightingNormal3D = (lnNone, lnFace, lnVertex, lnFaceVertexMix);
  TLightingInterpolation3D = (liLowQuality, liSpecularHighQuality, liAlwaysHighQuality);
  TAntialiasingMode3D = (am3dNone, am3dMultishape, am3dResample);
  TPerspectiveMode3D = (pmLinearMapping, pmPerspectiveMapping, pmZBuffer);

  TRenderingOptions = record
    LightingInterpolation: TLightingInterpolation3D;
    AntialiasingMode: TAntialiasingMode3D;
    AntialiasingResampleLevel: integer;
    PerspectiveMode: TPerspectiveMode3D;
    TextureInterpolation: boolean;
    MinZ: single;
  end;

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

  TBox3D = record
    min,max: TPoint3D;
  end;

  IBGRAVertex3D = interface;

  { IBGRALight3D }

  IBGRALight3D = interface ['{85C683B6-07AC-4B8D-9324-06BC22882433}']
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

  IBGRAPointLight3D = interface(IBGRALight3D) ['{C939900D-DDD6-49F0-B1E9-E29F94FDB4C8}']
    function GetVertex: IBGRAVertex3D;
    procedure SetVertex(const AValue: IBGRAVertex3D);
    property Vertex: IBGRAVertex3D read GetVertex write SetVertex;
  end;

  IBGRADirectionalLight3D = interface(IBGRALight3D) ['{8D575CEE-8DD2-46FB-9BCC-17DE3DAAF53D}']
    function GetDirection: TPoint3D;
    procedure SetDirection(const AValue: TPoint3D);
    property Direction: TPoint3D read GetDirection write SetDirection;
  end;

  { IBGRAMaterial3D }

  IBGRAMaterial3D = interface
    function GetAmbiantAlpha: byte;
    function GetAutoAmbiantColor: boolean;
    function GetAutoDiffuseColor: boolean;
    function GetAutoSimpleColor: boolean;
    function GetAutoSpecularColor: boolean;
    function GetAmbiantColor: TBGRAPixel;
    function GetAmbiantColorF: TColorF;
    function GetAmbiantColorInt: TColorInt65536;
    function GetDiffuseAlpha: byte;
    function GetDiffuseColor: TBGRAPixel;
    function GetDiffuseColorF: TColorF;
    function GetDiffuseColorInt: TColorInt65536;
    function GetLightThroughFactor: single;
    function GetName: string;
    function GetSaturationHigh: single;
    function GetSaturationLow: single;
    function GetSimpleAlpha: byte;
    function GetSimpleColor: TBGRAPixel;
    function GetSimpleColorF: TColorF;
    function GetSimpleColorInt: TColorInt65536;
    function GetSpecularColor: TBGRAPixel;
    function GetSpecularColorF: TColorF;
    function GetSpecularColorInt: TColorInt65536;
    function GetSpecularIndex: integer;
    function GetSpecularOn: boolean;
    function GetTexture: IBGRAScanner;
    function GetTextureZoom: TPointF;
    function GetAsObject: TObject;

    procedure SetAmbiantAlpha(AValue: byte);
    procedure SetAutoDiffuseColor(const AValue: boolean);
    procedure SetAutoSpecularColor(const AValue: boolean);
    procedure SetAmbiantColor(const AValue: TBGRAPixel);
    procedure SetAmbiantColorF(const AValue: TColorF);
    procedure SetAmbiantColorInt(const AValue: TColorInt65536);
    procedure SetDiffuseAlpha(AValue: byte);
    procedure SetDiffuseColor(const AValue: TBGRAPixel);
    procedure SetDiffuseColorF(const AValue: TColorF);
    procedure SetDiffuseColorInt(const AValue: TColorInt65536);
    procedure SetLightThroughFactor(const AValue: single);
    procedure SetName(const AValue: string);
    procedure SetSaturationHigh(const AValue: single);
    procedure SetSaturationLow(const AValue: single);
    procedure SetSimpleAlpha(AValue: byte);
    procedure SetSimpleColor(AValue: TBGRAPixel);
    procedure SetSimpleColorF(AValue: TColorF);
    procedure SetSimpleColorInt(AValue: TColorInt65536);
    procedure SetSpecularColor(const AValue: TBGRAPixel);
    procedure SetSpecularColorF(const AValue: TColorF);
    procedure SetSpecularColorInt(const AValue: TColorInt65536);
    procedure SetSpecularIndex(const AValue: integer);
    procedure SetTexture(AValue: IBGRAScanner);
    procedure SetTextureZoom(AValue: TPointF);

    property AutoSimpleColor: boolean read GetAutoSimpleColor;
    property SimpleColor: TBGRAPixel read GetSimpleColor write SetSimpleColor;
    property SimpleColorF: TColorF read GetSimpleColorF write SetSimpleColorF;
    property SimpleColorInt: TColorInt65536 read GetSimpleColorInt write SetSimpleColorInt;
    property SimpleAlpha: byte read GetSimpleAlpha write SetSimpleAlpha;

    property AmbiantColor: TBGRAPixel read GetAmbiantColor write SetAmbiantColor;
    property AmbiantColorF: TColorF read GetAmbiantColorF write SetAmbiantColorF;
    property AmbiantColorInt: TColorInt65536 read GetAmbiantColorInt write SetAmbiantColorInt;
    property AutoAmbiantColor: boolean read GetAutoAmbiantColor;
    property AmbiantAlpha: byte read GetAmbiantAlpha write SetAmbiantAlpha;
    property Texture: IBGRAScanner read GetTexture write SetTexture;
    property TextureZoom: TPointF read GetTextureZoom write SetTextureZoom;

    property DiffuseColor: TBGRAPixel read GetDiffuseColor write SetDiffuseColor;
    property DiffuseColorF: TColorF read GetDiffuseColorF write SetDiffuseColorF;
    property DiffuseColorInt: TColorInt65536 read GetDiffuseColorInt write SetDiffuseColorInt;
    property AutoDiffuseColor: boolean read GetAutoDiffuseColor write SetAutoDiffuseColor;
    property DiffuseAlpha: byte read GetDiffuseAlpha write SetDiffuseAlpha;
    property SaturationLow: single read GetSaturationLow write SetSaturationLow;
    property SaturationHigh: single read GetSaturationHigh write SetSaturationHigh;

    property SpecularColor: TBGRAPixel read GetSpecularColor write SetSpecularColor;
    property SpecularColorF: TColorF read GetSpecularColorF write SetSpecularColorF;
    property SpecularColorInt: TColorInt65536 read GetSpecularColorInt write SetSpecularColorInt;
    property AutoSpecularColor: boolean read GetAutoSpecularColor write SetAutoSpecularColor;
    property SpecularIndex: integer read GetSpecularIndex write SetSpecularIndex;
    property SpecularOn: boolean read GetSpecularOn;

    property LightThroughFactor: single read GetLightThroughFactor write SetLightThroughFactor;
    property Name: string read GetName write SetName;
  end;

  { IBGRANormal3D }

  IBGRANormal3D = interface
    function GetCustomNormal: TPoint3D;
    function GetCustomNormal_128: TPoint3D_128;
    function GetViewNormal: TPoint3D;
    function GetViewNormal_128: TPoint3D_128;
    procedure SetCustomNormal(AValue: TPoint3D);
    procedure SetCustomNormal_128(AValue: TPoint3D_128);
    procedure SetViewNormal(AValue: TPoint3D);
    procedure SetViewNormal_128(AValue: TPoint3D_128);
    property ViewNormal: TPoint3D read GetViewNormal write SetViewNormal;
    property ViewNormal_128: TPoint3D_128 read GetViewNormal_128 write SetViewNormal_128;
    property CustomNormal: TPoint3D read GetCustomNormal write SetCustomNormal;
    property CustomNormal_128: TPoint3D_128 read GetCustomNormal_128 write SetCustomNormal_128;
  end;

  { IBGRAVertex3D }

  IBGRAVertex3D = interface
    function GetColor: TBGRAPixel;
    function GetCustomFlags: DWord;
    function GetCustomNormal: TPoint3D;
    function GetCustomNormal_128: TPoint3D_128;
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
    procedure SetCustomNormal(AValue: TPoint3D);
    procedure SetCustomNormal_128(AValue: TPoint3D_128);
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
    property CustomNormal: TPoint3D read GetCustomNormal write SetCustomNormal;
    property CustomNormal_128: TPoint3D_128 read GetCustomNormal_128 write SetCustomNormal_128;
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
    function Add(pt: TPoint3D; normal: TPoint3D): IBGRAVertex3D;
    function Add(pt: TPoint3D_128): IBGRAVertex3D;
    function Add(pt: TPoint3D_128; normal: TPoint3D_128): IBGRAVertex3D;
    function AddNormal(x,y,z: single): IBGRANormal3D;
    function AddNormal(pt: TPoint3D): IBGRANormal3D;
    function AddNormal(pt: TPoint3D_128): IBGRANormal3D;
    function Add(const coords: array of single): arrayOfIBGRAVertex3D;
    function Add(const pts: array of TPoint3D): arrayOfIBGRAVertex3D;
    function Add(const pts_128: array of TPoint3D_128): arrayOfIBGRAVertex3D;
    procedure Add(const pts: array of IBGRAVertex3D);
    procedure Add(AVertex: IBGRAVertex3D);
    function GetTotalNormalCount: integer;
    function IndexOf(AVertex: IBGRAVertex3D): integer;
    procedure RemoveVertex(Index: integer);
    procedure RemoveNormal(Index: integer);
    function GetBoundingBox: TBox3D;
    function GetMatrix: TMatrix3D;
    function GetPart(AIndex: Integer): IBGRAPart3D;
    function GetPartCount: integer;
    function GetRadius: single;
    function GetVertex(AIndex: Integer): IBGRAVertex3D;
    function GetVertexCount: integer;
    function GetNormal(AIndex: Integer): IBGRANormal3D;
    function GetNormalCount: integer;
    function GetTotalVertexCount: integer;
    function GetContainer: IBGRAPart3D;
    procedure ResetTransform;
    procedure Scale(size: single; Before: boolean = true);
    procedure Scale(x,y,z: single; Before: boolean = true);
    procedure Scale(size: TPoint3D; Before: boolean = true);
    procedure SetMatrix(const AValue: TMatrix3D);
    procedure SetNormal(AIndex: Integer; AValue: IBGRANormal3D);
    procedure SetVertex(AIndex: Integer; AValue: IBGRAVertex3D);
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
    property NormalCount: integer read GetNormalCount;
    property Vertex[AIndex: Integer]: IBGRAVertex3D read GetVertex write SetVertex;
    property Normal[AIndex: Integer]: IBGRANormal3D read GetNormal write SetNormal;
    property Matrix: TMatrix3D read GetMatrix write SetMatrix;
    property PartCount: integer read GetPartCount;
    property Part[AIndex: Integer]: IBGRAPart3D read GetPart;
    property Radius: single read GetRadius;
    property BoundingBox: TBox3D read GetBoundingBox;
    property TotalVertexCount: integer read GetTotalVertexCount;
    property TotalNormalCount: integer read GetTotalNormalCount;
    property Container: IBGRAPart3D read GetContainer;
  end;

  IBGRAObject3D = interface;

  { IBGRAFace3D }

  IBGRAFace3D = interface
    procedure FlipFace;
    function AddVertex(AVertex: IBGRAVertex3D): integer;
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
    function GetNormal(AIndex: Integer): IBGRANormal3D;
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
    procedure SetVertex(AIndex: Integer; AValue: IBGRAVertex3D);
    procedure SetNormal(AIndex: Integer; AValue: IBGRANormal3D);
    procedure SetVertexColor(AIndex: Integer; const AValue: TBGRAPixel);
    procedure SetVertexColorOverride(AIndex: Integer; const AValue: boolean);
    procedure ComputeViewNormalAndCenter;
    procedure ComputeVertexColors;
    procedure UpdateMaterial;
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
    function GetTotalNormalCount: integer;
    function GetTotalVertexCount: integer;
    function GetLight: Single;
    function GetLightingNormal: TLightingNormal3D;
    function GetParentLighting: boolean;
    function GetTexture: IBGRAScanner;
    function GetMainPart: IBGRAPart3D;
    function GetScene: TObject;
    procedure SetColor(const AValue: TBGRAPixel);
    procedure SetLight(const AValue: Single);
    procedure SetLightingNormal(const AValue: TLightingNormal3D);
    procedure SetMaterial(const AValue: IBGRAMaterial3D);
    procedure SetParentLighting(const AValue: boolean);
    procedure SetTexture(const AValue: IBGRAScanner);
    procedure ComputeWithMatrix(constref AMatrix: TMatrix3D; constref AProjection: TProjection3D);
    procedure RemoveUnusedVertices;
    procedure InvalidateColor;
    procedure InvalidateMaterial;
    procedure ForEachVertex(ACallback: TVertex3DCallback);
    procedure ForEachFace(ACallback: TFace3DCallback);
    function AddFaceReversed(const AVertices: array of IBGRAVertex3D): IBGRAFace3D;
    function AddFace(const AVertices: array of IBGRAVertex3D): IBGRAFace3D;
    function AddFace(const AVertices: array of IBGRAVertex3D; ABiface: boolean): IBGRAFace3D;
    function AddFace(const AVertices: array of IBGRAVertex3D; ATexture: IBGRAScanner): IBGRAFace3D;
    function AddFace(const AVertices: array of IBGRAVertex3D; AColor: TBGRAPixel): IBGRAFace3D;
    function AddFace(const AVertices: array of IBGRAVertex3D; AColors: array of TBGRAPixel): IBGRAFace3D;
    procedure Update;
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
    property TotalNormalCount: integer read GetTotalNormalCount;
    property Material: IBGRAMaterial3D read GetMaterial write SetMaterial;
    property Scene: TObject read GetScene;
    property RefCount: integer read GetRefCount;
  end;

  TBGRAMaterialTextureChangedEvent = procedure(ASender: TObject) of object;

  { TBGRAMaterial3D }

  TBGRAMaterial3D = class(TInterfacedObject, IBGRAMaterial3D)
  private
    FName: string;
    FAutoSimpleColor,FAutoAmbiantColor,FAutoDiffuseColor,FAutoSpecularColor: boolean;
    FSimpleColorInt, FAmbiantColorInt, FDiffuseColorInt: TColorInt65536;
    FDiffuseLightness: integer;

    FSpecularColorInt: TColorInt65536;
    FSpecularIndex: integer;
    FSpecularOn: boolean;

    FSaturationLowF: single;
    FSaturationHighF: single;
    FLightThroughFactor: single;

    FTexture: IBGRAScanner;
    FTextureZoom: TPointF;
    FOnTextureChanged: TBGRAMaterialTextureChangedEvent;

    //phong precalc
    FPowerTable: array of single;
    FPowerTableSize, FPowerTableExp2: integer;
    FPowerTableSizeF: single;

    procedure UpdateSpecular;
    procedure UpdateSimpleColor;
    procedure ComputePowerTable;
  public
    constructor Create;
    destructor Destroy; override;

    function GetAutoAmbiantColor: boolean;
    function GetAutoDiffuseColor: boolean;
    function GetAutoSpecularColor: boolean;
    function GetAutoSimpleColor: boolean;
    function GetAmbiantAlpha: byte;
    function GetAmbiantColor: TBGRAPixel;
    function GetAmbiantColorF: TColorF;
    function GetAmbiantColorInt: TColorInt65536;
    function GetDiffuseAlpha: byte;
    function GetDiffuseColor: TBGRAPixel;
    function GetDiffuseColorF: TColorF;
    function GetDiffuseColorInt: TColorInt65536;
    function GetLightThroughFactor: single;
    function GetSpecularColor: TBGRAPixel;
    function GetSpecularColorF: TColorF;
    function GetSpecularColorInt: TColorInt65536;
    function GetSpecularIndex: integer;
    function GetSaturationHigh: single;
    function GetSaturationLow: single;
    function GetSimpleAlpha: byte;
    function GetSimpleColor: TBGRAPixel;
    function GetSimpleColorF: TColorF;
    function GetSimpleColorInt: TColorInt65536;
    function GetTextureZoom: TPointF;
    function GetSpecularOn: boolean;
    function GetAsObject: TObject;
    function GetName: string;

    procedure SetAutoAmbiantColor(const AValue: boolean);
    procedure SetAutoDiffuseColor(const AValue: boolean);
    procedure SetAutoSpecularColor(const AValue: boolean);
    procedure SetAmbiantAlpha(AValue: byte);
    procedure SetAmbiantColor(const AValue: TBGRAPixel);
    procedure SetAmbiantColorF(const AValue: TColorF);
    procedure SetAmbiantColorInt(const AValue: TColorInt65536);
    procedure SetDiffuseAlpha(AValue: byte);
    procedure SetDiffuseColor(const AValue: TBGRAPixel);
    procedure SetDiffuseColorF(const AValue: TColorF);
    procedure SetDiffuseColorInt(const AValue: TColorInt65536);
    procedure SetLightThroughFactor(const AValue: single);
    procedure SetSpecularColor(const AValue: TBGRAPixel);
    procedure SetSpecularColorF(const AValue: TColorF);
    procedure SetSpecularColorInt(const AValue: TColorInt65536);
    procedure SetSpecularIndex(const AValue: integer); virtual;
    procedure SetSaturationHigh(const AValue: single);
    procedure SetSaturationLow(const AValue: single);
    procedure SetSimpleAlpha(AValue: byte);
    procedure SetSimpleColor(AValue: TBGRAPixel);
    procedure SetSimpleColorF(AValue: TColorF);
    procedure SetSimpleColorInt(AValue: TColorInt65536);
    procedure SetTextureZoom(AValue: TPointF);
    procedure SetName(const AValue: string);

    function GetTexture: IBGRAScanner;
    procedure SetTexture(AValue: IBGRAScanner);

    procedure ComputeDiffuseAndSpecularColor(Context: PSceneLightingContext; DiffuseIntensity, SpecularIntensity, SpecularCosine: single; const ALightColor: TColorInt65536);
    procedure ComputeDiffuseColor(Context: PSceneLightingContext; const DiffuseIntensity: single; const ALightColor: TColorInt65536);
    procedure ComputeDiffuseLightness(Context: PSceneLightingContext; DiffuseLightnessTerm32768: integer; ALightLightness: integer);

    property OnTextureChanged: TBGRAMaterialTextureChangedEvent read FOnTextureChanged write FOnTextureChanged;

  end;

  TFaceRenderingDescription = record
    NormalsMode: TLightingNormal3D;

    Material: TBGRAMaterial3D;
    Texture: IBGRAScanner;
    LightThroughFactor: single;
    Biface: boolean;

    NbVertices: Integer;
    Projections: array of TPointF;
    Colors: array of TBGRAPixel;
    Positions3D, Normals3D: array of TPoint3D_128;
    TexCoords: array of TPointF;
  end;

  { TCustomRenderer3D }

  TCustomRenderer3D = class
  private
    FProjection: TProjection3D;
    FProjectionDefined: boolean;
    function GetProjectionDefined: boolean;
  protected
    function GetGlobalScale: single; virtual; abstract;
    function GetHasZBuffer: boolean; virtual; abstract;
    function GetHandlesNearClipping: boolean; virtual; abstract;
    function GetHandlesFaceCulling: boolean; virtual; abstract;
    function GetSurfaceWidth: integer; virtual; abstract;
    function GetSurfaceHeight: integer; virtual; abstract;
    procedure SetProjection(const AValue: TProjection3D); virtual;
  public
    function RenderFace(var ADescription: TFaceRenderingDescription;
      AComputeCoordinate: TComputeProjectionFunc): boolean; virtual; abstract;
    property GlobalScale: single read GetGlobalScale;
    property HasZBuffer: boolean read GetHasZBuffer;
    property SurfaceWidth: integer read GetSurfaceWidth;
    property SurfaceHeight: integer read GetSurfaceHeight;
    property Projection: TProjection3D read FProjection write SetProjection;
    property ProjectionDefined: boolean read GetProjectionDefined;
    property HandlesNearClipping: boolean read GetHandlesNearClipping;
    property HandlesFaceCulling: boolean read GetHandlesFaceCulling;
  end;

  { TBGRALight3D }

  TBGRALight3D = class(TInterfacedObject,IBGRALight3D)
  protected
    FMinIntensity: single;
    FColorInt: TColorInt65536;
    FViewVector : TPoint3D_128;
    FLightness: integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ReleaseInterface;

    procedure ComputeDiffuseLightness(Context: PSceneLightingContext); virtual; abstract;
    procedure ComputeDiffuseColor(Context: PSceneLightingContext); virtual; abstract;
    procedure ComputeDiffuseAndSpecularColor(Context: PSceneLightingContext); virtual; abstract;

    function GetLightnessF: single;
    function GetColor: TBGRAPixel;
    function GetColorF: TColorF;
    function GetColorInt: TColorInt65536;
    function GetAsObject: TObject;
    procedure SetColor(const AValue: TBGRAPixel);
    procedure SetColorF(const AValue: TColorF);
    procedure SetColorInt(const AValue: TColorInt65536);
    function GetColoredLight: boolean;

    function GetMinIntensity: single;
    procedure SetMinIntensity(const AValue: single);
    function IsDirectional: boolean; virtual; abstract;

    function GetIntensity: single; virtual;
    function GetPosition: TPoint3D; virtual;
    function GetDirection: TPoint3D; virtual;
  end;

implementation

{ TCustomRenderer3D }

function TCustomRenderer3D.GetProjectionDefined: boolean;
begin
  result := FProjectionDefined;
end;

{$PUSH}{$OPTIMIZATION OFF} // avoids internal error 2012090607
procedure TCustomRenderer3D.SetProjection(const AValue: TProjection3D);
begin
  FProjection := AValue;
  FProjectionDefined := true;
end;
{$POP}

{ TBGRAMaterial3D }

procedure TBGRAMaterial3D.UpdateSpecular;
begin
  FAutoSpecularColor := (FSpecularColorInt.r = 65536) and (FSpecularColorInt.g = 65536) and (FSpecularColorInt.b = 65536) and (FSpecularColorInt.a = 65536);
  FSpecularOn := (FSpecularIndex > 0) and ((FSpecularColorInt.r <> 0) or (FSpecularColorInt.g <> 0) or (FSpecularColorInt.b <> 0) or
                                            FAutoSpecularColor);
end;

procedure TBGRAMaterial3D.UpdateSimpleColor;
begin
  FSimpleColorInt := (FAmbiantColorInt+FDiffuseColorInt)*32768;
  FAutoSimpleColor := (FSimpleColorInt.r = 65536) and (FSimpleColorInt.g = 65536) and (FSimpleColorInt.b = 65536) and (FSimpleColorInt.a = 65536);
end;

procedure TBGRAMaterial3D.ComputePowerTable;
var i: integer;
    Exponent: single;
begin
  //exponent computed by squares
  Exponent := 1;
  FPowerTableExp2 := 0;
  While Exponent*FPowerTableSize/16 < FSpecularIndex do
  begin
    Exponent *= 2;
    Inc(FPowerTableExp2);
  end;

  //remaining exponent
  setlength(FPowerTable,FPowerTableSize+3);
  FPowerTable[0] := 0; //out of bound
  FPowerTable[1] := 0; //image of zero
  for i := 1 to FPowerTableSize do // ]0;1]
    FPowerTable[i+1] := Exp(ln(i/(FPowerTableSize-1))*FSpecularIndex/Exponent);
  FPowerTable[FPowerTableSize+2] := 1; //out of bound
end;

constructor TBGRAMaterial3D.Create;
begin
  SetAmbiantColorInt(ColorInt65536(65536,65536,65536));
  SetDiffuseColorInt(ColorInt65536(65536,65536,65536));
  FSpecularIndex := 10;
  SetSpecularColorInt(ColorInt65536(0,0,0));
  FLightThroughFactor:= 0;
  SetSaturationLow(2);
  SetSaturationHigh(3);

  FTexture := nil;
  FTextureZoom := PointF(1,1);

  FPowerTableSize := 128;
  FPowerTableSizeF := FPowerTableSize;
  FPowerTable := nil;
end;

destructor TBGRAMaterial3D.Destroy;
begin
  inherited Destroy;
end;

function TBGRAMaterial3D.GetAutoAmbiantColor: boolean;
begin
  result := FAutoAmbiantColor;
end;

procedure TBGRAMaterial3D.SetDiffuseAlpha(AValue: byte);
begin
  if AValue = 0 then
    FDiffuseColorInt.a := 0
  else
    FDiffuseColorInt.a := AValue*257+1;
  UpdateSimpleColor;
end;

function TBGRAMaterial3D.GetAutoDiffuseColor: boolean;
begin
  result := FAutoDiffuseColor;
end;

function TBGRAMaterial3D.GetAutoSpecularColor: boolean;
begin
  result := FAutoSpecularColor;
end;

function TBGRAMaterial3D.GetAutoSimpleColor: boolean;
begin
  result := FAutoSimpleColor;
end;

function TBGRAMaterial3D.GetAmbiantAlpha: byte;
var v: integer;
begin
  if FAmbiantColorInt.a < 128 then
    result := 0
  else
  begin
    v := (FAmbiantColorInt.a-128) shr 8;
    if v > 255 then v := 255;
    result := v;
  end;
end;

function TBGRAMaterial3D.GetAmbiantColor: TBGRAPixel;
begin
  result := ColorIntToBGRA(FAmbiantColorInt,True);
end;

function TBGRAMaterial3D.GetAmbiantColorF: TColorF;
begin
  result := ColorInt65536ToColorF(FAmbiantColorInt);
end;

function TBGRAMaterial3D.GetAmbiantColorInt: TColorInt65536;
begin
  result := FAmbiantColorInt;
end;

function TBGRAMaterial3D.GetDiffuseAlpha: byte;
var v: integer;
begin
  if FDiffuseColorInt.a < 128 then
    result := 0
  else
  begin
    v := (FDiffuseColorInt.a-128) shr 8;
    if v > 255 then v := 255;
    result := v;
  end;
end;

function TBGRAMaterial3D.GetDiffuseColor: TBGRAPixel;
begin
  result := ColorIntToBGRA(FDiffuseColorInt,True);
end;

function TBGRAMaterial3D.GetDiffuseColorF: TColorF;
begin
  result := ColorInt65536ToColorF(FDiffuseColorInt);
end;

function TBGRAMaterial3D.GetDiffuseColorInt: TColorInt65536;
begin
  result := FDiffuseColorInt;
end;

function TBGRAMaterial3D.GetLightThroughFactor: single;
begin
  result := FLightThroughFactor;
end;

function TBGRAMaterial3D.GetSpecularColor: TBGRAPixel;
begin
  result := ColorIntToBGRA(FSpecularColorInt,True);
end;

function TBGRAMaterial3D.GetSpecularColorF: TColorF;
begin
  result := ColorInt65536ToColorF(FSpecularColorInt);
end;

function TBGRAMaterial3D.GetSpecularColorInt: TColorInt65536;
begin
  result := FSpecularColorInt;
end;

function TBGRAMaterial3D.GetSpecularIndex: integer;
begin
  result := FSpecularIndex;
end;

function TBGRAMaterial3D.GetSaturationHigh: single;
begin
  result := FSaturationHighF;
end;

function TBGRAMaterial3D.GetSaturationLow: single;
begin
  result := FSaturationLowF;
end;

function TBGRAMaterial3D.GetSimpleAlpha: byte;
begin
  result := (GetAmbiantAlpha + GetDiffuseAlpha) shr 1;
end;

function TBGRAMaterial3D.GetSimpleColor: TBGRAPixel;
begin
  result := ColorIntToBGRA(GetSimpleColorInt,True);
end;

function TBGRAMaterial3D.GetSimpleColorF: TColorF;
begin
  result := ColorInt65536ToColorF(GetSimpleColorInt);
end;

function TBGRAMaterial3D.GetSimpleColorInt: TColorInt65536;
begin
  result := (GetAmbiantColorInt + GetDiffuseColorInt)*32768;
end;

function TBGRAMaterial3D.GetTexture: IBGRAScanner;
begin
  result := FTexture;
end;

function TBGRAMaterial3D.GetTextureZoom: TPointF;
begin
  result := FTextureZoom;
end;

procedure TBGRAMaterial3D.SetAutoAmbiantColor(const AValue: boolean);
begin
  If AValue then
    SetAmbiantColorInt(ColorInt65536(65536,65536,65536));
end;

procedure TBGRAMaterial3D.SetAutoDiffuseColor(const AValue: boolean);
begin
  If AValue then
    SetDiffuseColorInt(ColorInt65536(65536,65536,65536));
end;

procedure TBGRAMaterial3D.SetAutoSpecularColor(const AValue: boolean);
begin
  If AValue then
    SetSpecularColorInt(ColorInt65536(65536,65536,65536));
end;

procedure TBGRAMaterial3D.SetAmbiantAlpha(AValue: byte);
begin
  if AValue = 0 then
    FAmbiantColorInt.a := 0
  else
    FAmbiantColorInt.a := AValue*257+1;
  UpdateSimpleColor;
end;

procedure TBGRAMaterial3D.SetAmbiantColor(const AValue: TBGRAPixel);
begin
  FAmbiantColorInt := BGRAToColorInt(AValue,True);
  FAutoAmbiantColor := (FAmbiantColorInt.r = 65536) and (FAmbiantColorInt.g = 65536) and (FAmbiantColorInt.b = 65536) and (FAmbiantColorInt.a = 65536);
  UpdateSimpleColor;
end;

procedure TBGRAMaterial3D.SetAmbiantColorF(const AValue: TColorF);
begin
  FAmbiantColorInt := ColorFToColorInt65536(AValue);
  FAutoAmbiantColor := (FAmbiantColorInt.r = 65536) and (FAmbiantColorInt.g = 65536) and (FAmbiantColorInt.b = 65536) and (FAmbiantColorInt.a = 65536);
  UpdateSimpleColor;
end;

procedure TBGRAMaterial3D.SetAmbiantColorInt(const AValue: TColorInt65536);
begin
  FAmbiantColorInt := AValue;
  FAutoAmbiantColor := (FAmbiantColorInt.r = 65536) and (FAmbiantColorInt.g = 65536) and (FAmbiantColorInt.b = 65536) and (FAmbiantColorInt.a = 65536);
  UpdateSimpleColor;
end;

procedure TBGRAMaterial3D.SetDiffuseColor(const AValue: TBGRAPixel);
begin
  FDiffuseColorInt := BGRAToColorInt(AValue,True);
  FDiffuseLightness := (FDiffuseColorInt.r + FDiffuseColorInt.g + FDiffuseColorInt.b) div 6;
  FAutoDiffuseColor:= (FDiffuseColorInt.r = 65536) and (FDiffuseColorInt.g = 65536) and (FDiffuseColorInt.b = 65536);
  UpdateSimpleColor;
end;

procedure TBGRAMaterial3D.SetDiffuseColorF(const AValue: TColorF);
begin
  FDiffuseColorInt := ColorFToColorInt65536(AValue);
  FDiffuseLightness := (FDiffuseColorInt.r + FDiffuseColorInt.g + FDiffuseColorInt.b) div 6;
  FAutoDiffuseColor:= (FDiffuseColorInt.r = 65536) and (FDiffuseColorInt.g = 65536) and (FDiffuseColorInt.b = 65536);
  UpdateSimpleColor;
end;

procedure TBGRAMaterial3D.SetDiffuseColorInt(const AValue: TColorInt65536);
begin
  FDiffuseColorInt := AValue;
  FDiffuseLightness := (FDiffuseColorInt.r + FDiffuseColorInt.g + FDiffuseColorInt.b) div 6;
  FAutoDiffuseColor:= (FDiffuseColorInt.r = 65536) and (FDiffuseColorInt.g = 65536) and (FDiffuseColorInt.b = 65536);
  UpdateSimpleColor;
end;

procedure TBGRAMaterial3D.SetLightThroughFactor(const AValue: single);
begin
  FLightThroughFactor:= AValue;
end;

procedure TBGRAMaterial3D.SetSpecularColor(const AValue: TBGRAPixel);
begin
  FSpecularColorInt := BGRAToColorInt(AValue,True);
  UpdateSpecular;
end;

procedure TBGRAMaterial3D.SetSpecularColorF(const AValue: TColorF);
begin
  FSpecularColorInt := ColorFToColorInt65536(AValue);
  UpdateSpecular;
end;

procedure TBGRAMaterial3D.SetSpecularColorInt(const AValue: TColorInt65536);
begin
  FSpecularColorInt := AValue;
  UpdateSpecular;
end;

procedure TBGRAMaterial3D.SetSpecularIndex(const AValue: integer);
begin
  FSpecularIndex := AValue;
  UpdateSpecular;

  FPowerTable := nil;
end;

procedure TBGRAMaterial3D.SetSaturationHigh(const AValue: single);
begin
  FSaturationHighF:= AValue;
end;

procedure TBGRAMaterial3D.SetSaturationLow(const AValue: single);
begin
  FSaturationLowF:= AValue;
end;

procedure TBGRAMaterial3D.SetSimpleAlpha(AValue: byte);
begin
  SetAmbiantAlpha(AValue);
  SetDiffuseAlpha(AValue);
end;

procedure TBGRAMaterial3D.SetSimpleColor(AValue: TBGRAPixel);
begin
  SetAmbiantColor(AValue);
  SetDiffuseColor(AValue);
end;

procedure TBGRAMaterial3D.SetSimpleColorF(AValue: TColorF);
begin
  SetAmbiantColorF(AValue);
  SetDiffuseColorF(AValue);
end;

procedure TBGRAMaterial3D.SetSimpleColorInt(AValue: TColorInt65536);
begin
  SetAmbiantColorInt(AValue);
  SetDiffuseColorInt(AValue);
end;

procedure TBGRAMaterial3D.SetTexture(AValue: IBGRAScanner);
begin
  If AValue <> FTexture then
  begin
    FTexture := AValue;
    if Assigned(FOnTextureChanged) then
      FOnTextureChanged(self);
  end;
end;

procedure TBGRAMaterial3D.SetTextureZoom(AValue: TPointF);
begin
  if AValue <> FTextureZoom then
  begin
    FTextureZoom := AValue;
    if Assigned(FOnTextureChanged) then
      FOnTextureChanged(self);
  end;
end;

function TBGRAMaterial3D.GetName: string;
begin
  result := FName;
end;

procedure TBGRAMaterial3D.SetName(const AValue: string);
begin
  FName := AValue;
end;

function TBGRAMaterial3D.GetSpecularOn: boolean;
begin
  result := FSpecularOn;
end;

function TBGRAMaterial3D.GetAsObject: TObject;
begin
  result := self;
end;

procedure TBGRAMaterial3D.ComputeDiffuseAndSpecularColor(Context: PSceneLightingContext; DiffuseIntensity, SpecularIntensity, SpecularCosine: single; const ALightColor: TColorInt65536);
var
  NH,PowerTablePos: single; //keep first for asm

  NnH: single;
  PowerTableFPos: single;
  PowerTableIPos,i: NativeInt;
begin
  if SpecularCosine <= 0 then
    NnH := 0
  else
  if SpecularCosine >= 1 then
    NnH := 1 else
  begin
    NH := SpecularCosine;
    if FPowerTable = nil then ComputePowerTable;
    {$IFDEF CPUI386} {$asmmode intel}
    i := FPowerTableExp2;
    if i > 0 then
    begin
      PowerTablePos := FPowerTableSize;
      asm
        db $d9,$45,$f0  //flds NH
        mov ecx,i
      @loop:
        db $dc,$c8      //fmul st,st(0)
        dec ecx
        jnz @loop
        db $d8,$4d,$ec  //fmuls PowerTablePos
        db $d9,$5d,$ec  //fstps PowerTablePos
      end;
    end
    else
      PowerTablePos := NH*FPowerTableSize;
    {$ELSE}
    PowerTablePos := NH;
    for i := FPowerTableExp2-1 downto 0 do
      PowerTablePos := PowerTablePos*PowerTablePos;
    PowerTablePos *= FPowerTableSize;
    {$ENDIF}
    PowerTableIPos := round(PowerTablePos+0.5);
    PowerTableFPos := PowerTablePos-PowerTableIPos;
    NnH := FPowerTable[PowerTableIPos]*(1-PowerTableFPos)+FPowerTable[PowerTableIPos+1]*PowerTableFPos;
  end; //faster than NnH := exp(FSpecularIndex*ln(NH)); !

  if FAutoDiffuseColor then
    Context^.diffuseColor += ALightColor*round(DiffuseIntensity*65536)
  else
    Context^.diffuseColor += ALightColor*FDiffuseColorInt*round(DiffuseIntensity*65536);

  if FAutoSpecularColor then
    Context^.specularColor += ALightColor*round(SpecularIntensity* NnH*65536)
  else
    Context^.specularColor += ALightColor*FSpecularColorInt*round(SpecularIntensity* NnH*65536);
end;

procedure TBGRAMaterial3D.ComputeDiffuseColor(Context: PSceneLightingContext;
  const DiffuseIntensity: single; const ALightColor: TColorInt65536);
begin
  if FAutoDiffuseColor then
    Context^.diffuseColor += ALightColor*round(DiffuseIntensity*65536)
  else
    Context^.diffuseColor += ALightColor*FDiffuseColorInt*round(DiffuseIntensity*65536);
end;

procedure TBGRAMaterial3D.ComputeDiffuseLightness(
  Context: PSceneLightingContext; DiffuseLightnessTerm32768: integer; ALightLightness: integer);
begin
  if FAutoDiffuseColor then
  begin
    if ALightLightness <> 32768 then
      Context^.lightness += CombineLightness(DiffuseLightnessTerm32768,ALightLightness)
    else
      Context^.lightness += DiffuseLightnessTerm32768;
  end else
  begin
    if FDiffuseLightness <> 32768 then
      Context^.lightness += CombineLightness(DiffuseLightnessTerm32768,CombineLightness(FDiffuseLightness,ALightLightness))
    else
      Context^.lightness += CombineLightness(DiffuseLightnessTerm32768,ALightLightness);
  end;
end;

{ TBGRALight3D }

constructor TBGRALight3D.Create;
begin
  SetColorF(ColorF(1,1,1,1));
  FViewVector := Point3D_128(0,0,-1);
  FMinIntensity:= 0;
end;

destructor TBGRALight3D.Destroy;
begin
  inherited Destroy;
end;

procedure TBGRALight3D.ReleaseInterface;
begin
  _Release;
end;

function TBGRALight3D.GetLightnessF: single;
begin
  result := FLightness/32768;
end;

function TBGRALight3D.GetColor: TBGRAPixel;
begin
  result := ColorIntToBGRA(FColorInt,True);
end;

function TBGRALight3D.GetColorF: TColorF;
begin
  result := ColorInt65536ToColorF(FColorInt);
end;

function TBGRALight3D.GetColorInt: TColorInt65536;
begin
  result := FColorInt;
end;

function TBGRALight3D.GetAsObject: TObject;
begin
  result := self;
end;

procedure TBGRALight3D.SetColor(const AValue: TBGRAPixel);
begin
  SetColorInt(BGRAToColorInt(AValue,True));
end;

procedure TBGRALight3D.SetColorF(const AValue: TColorF);
begin
  SetColorInt(ColorFToColorInt65536(AValue));
end;

procedure TBGRALight3D.SetColorInt(const AValue: TColorInt65536);
begin
  FColorInt := AValue;
  FLightness:= (AValue.r+AValue.g+AValue.b) div 6;
end;

function TBGRALight3D.GetColoredLight: boolean;
begin
  result := (FColorInt.r <> FColorInt.g) or (FColorInt.g <> FColorInt.b);
end;

function TBGRALight3D.GetMinIntensity: single;
begin
  result := FMinIntensity;
end;

procedure TBGRALight3D.SetMinIntensity(const AValue: single);
begin
  FMinIntensity := AValue;
end;

function TBGRALight3D.GetIntensity: single;
begin
  result := 1;
end;

function TBGRALight3D.GetPosition: TPoint3D;
begin
  result := Point3D(0,0,0);
end;

function TBGRALight3D.GetDirection: TPoint3D;
begin
  result := Point3D(0,0,0);
end;

end.
