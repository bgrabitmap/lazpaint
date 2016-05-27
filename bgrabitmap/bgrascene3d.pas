unit BGRAScene3D;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmapTypes, BGRAColorInt,
  BGRASSE, BGRAMatrix3D,
  BGRASceneTypes, BGRARenderer3D;

type
  TProjection3D = BGRAMatrix3D.TProjection3D;
  TLightingNormal3D = BGRASceneTypes.TLightingNormal3D;
  TLightingInterpolation3D = BGRASceneTypes.TLightingInterpolation3D;
  TAntialiasingMode3D = BGRASceneTypes.TAntialiasingMode3D;
  TPerspectiveMode3D = BGRASceneTypes.TPerspectiveMode3D;
  TRenderingOptions = BGRASceneTypes.TRenderingOptions;

  IBGRAVertex3D = BGRASceneTypes.IBGRAVertex3D;
  IBGRANormal3D = BGRASceneTypes.IBGRANormal3D;
  IBGRALight3D = BGRASceneTypes.IBGRALight3D;
  IBGRADirectionalLight3D = BGRASceneTypes.IBGRADirectionalLight3D;
  IBGRAPointLight3D = BGRASceneTypes.IBGRAPointLight3D;
  IBGRAMaterial3D = BGRASceneTypes.IBGRAMaterial3D;
  IBGRAFace3D = BGRASceneTypes.IBGRAFace3D;
  IBGRAPart3D = BGRASceneTypes.IBGRAPart3D;
  IBGRAObject3D = BGRASceneTypes.IBGRAObject3D;

  arrayOfIBGRAVertex3D = BGRASceneTypes.arrayOfIBGRAVertex3D;

const
  lnNone = BGRASceneTypes.lnNone;
  lnFace = BGRASceneTypes.lnFace;
  lnVertex = BGRASceneTypes.lnVertex;
  lnFaceVertexMix = BGRASceneTypes.lnFaceVertexMix;

  liLowQuality = BGRASceneTypes.liLowQuality;
  liSpecularHighQuality = BGRASceneTypes.liSpecularHighQuality;
  liAlwaysHighQuality = BGRASceneTypes.liAlwaysHighQuality;

  am3dNone = BGRASceneTypes.am3dNone;
  am3dMultishape = BGRASceneTypes.am3dMultishape;
  am3dResample = BGRASceneTypes.am3dResample;

  pmLinearMapping = BGRASceneTypes.pmLinearMapping;
  pmPerspectiveMapping = BGRASceneTypes.pmPerspectiveMapping;
  pmZBuffer = BGRASceneTypes.pmZBuffer;

type

  { TCamera3D }

  TCamera3D = class
  private
    procedure ComputeMatrix;
    function GetLookWhere: TPoint3D;
    function GetMatrix: TMatrix3D;
    function GetViewPoint: TPoint3D;
    procedure SetMatrix(AValue: TMatrix3D);
    procedure SetViewPoint(AValue: TPoint3D);
  protected
    FMatrix: TMatrix3D;
    FMatrixComputed: boolean;
    FViewPoint: TPoint3D_128;
    FLookWhere, FTopDir: TPoint3D_128;
  public
    procedure LookAt(AWhere: TPoint3D; ATopDir: TPoint3D);
    procedure LookDown(angleDeg: single);
    procedure LookLeft(angleDeg: single);
    procedure LookRight(angleDeg: single);
    procedure LookUp(angleDeg: single);
    property ViewPoint: TPoint3D read GetViewPoint write SetViewPoint;
    property LookWhere: TPoint3D read GetLookWhere;
    property Matrix: TMatrix3D read GetMatrix write SetMatrix;
  end;

  { TBGRAScene3D }

  TBGRAScene3D = class
  private
    FSurface: TBGRACustomBitmap; //destination of software renderer
    FViewCenter: TPointF;        //where origin is drawn
    FAutoViewCenter: boolean;    //use middle of the screen
    FZoom: TPointF;              //how much the drawing is zoomed
    FAutoZoom: Boolean;          //display 1 as 80% of surface size
    FProjection: TProjection3D;  //current projection
    FRenderedFaceCount: integer; //current counter of rendered faces

    FCamera: TCamera3D;

    FObjects: array of IBGRAObject3D;
    FObjectCount: integer;
    FMaterials: array of IBGRAMaterial3D;
    FMaterialCount: integer;
    FDefaultMaterial : IBGRAMaterial3D;

    FAmbiantLightColorF: TColorF;        //lightness without light sources
    FLights: TList;                      //individual light sources

    function GetAmbiantLightColorF: TColorF;
    function GetAmbiantLightness: single;
    function GetAmbiantLightColor: TBGRAPixel;
    function GetFaceCount: integer;
    function GetLight(AIndex: integer): IBGRALight3D;
    function GetLightCount: integer;
    function GetMaterial(AIndex: integer): IBGRAMaterial3D;
    function GetNormalCount: integer;
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
    function ComputeCoordinate(AViewCoord: TPoint3D_128): TPointF;
    procedure AddObject(AObj: IBGRAObject3D);
    procedure AddLight(ALight: TObject);
    procedure AddMaterial(AMaterial: IBGRAMaterial3D);
    procedure Init;

  protected
    FRenderer: TCustomRenderer3D;
    FMaterialLibrariesFetched: array of string;
    FTexturesFetched: array of record
        Name: string;
        Bitmap: TBGRACustomBitmap;
      end;
    procedure UseMaterial(AMaterialName: string; AFace: IBGRAFace3D); virtual;
    function LoadBitmapFromFileUTF8(AFilenameUTF8: string): TBGRACustomBitmap; virtual;
    function FetchTexture(AName: string; out texSize: TPointF): IBGRAScanner; virtual;
    procedure HandleFetchException(AException: Exception); virtual;
    procedure DoRender; virtual;
    procedure DoClear; virtual;
    function GetRenderWidth: integer;
    function GetRenderHeight: integer;
    procedure OnMaterialTextureChanged({%H-}ASender: TObject); virtual;
    procedure SetDefaultMaterial(AValue: IBGRAMaterial3D);
    procedure InvalidateMaterial;

  public
    DefaultLightingNormal: TLightingNormal3D;
    RenderingOptions: TRenderingOptions;
    UnknownColor: TBGRAPixel;
    FetchDirectory: string;
    FetchThrowsException: boolean;

    constructor Create;
    constructor Create(ASurface: TBGRACustomBitmap);
    destructor Destroy; override;
    procedure Clear; virtual;
    function FetchObject(AName: string; SwapFacesOrientation: boolean = true): IBGRAObject3D;
    procedure FetchMaterials(ALibraryName: string); virtual;
    function LoadObjectFromFile(AFilename: string; SwapFacesOrientation: boolean = true): IBGRAObject3D;
    function LoadObjectFromFileUTF8(AFilename: string; SwapFacesOrientation: boolean = true): IBGRAObject3D;
    function LoadObjectFromStream(AStream: TStream; SwapFacesOrientation: boolean = true): IBGRAObject3D;
    procedure LoadMaterialsFromFile(AFilename: string);
    procedure LoadMaterialsFromFileUTF8(AFilename: string);
    procedure LoadMaterialsFromStream(AStream: TStream);
    procedure LookAt(AWhere: TPoint3D; ATopDir: TPoint3D);
    procedure LookLeft(angleDeg: single);
    procedure LookRight(angleDeg: single);
    procedure LookUp(angleDeg: single);
    procedure LookDown(angleDeg: single);
    procedure Render; virtual;
    procedure Render(ARenderer: TCustomRenderer3D);
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
    function GetMaterialByName(AName: string): IBGRAMaterial3D;
    procedure UpdateMaterials; virtual;
    procedure UpdateMaterial(AMaterialName: string); virtual;
    procedure ForEachVertex(ACallback: TVertex3DCallback);
    procedure ForEachFace(ACallback: TFace3DCallback);
    function MakeLightList: TList;

    property ViewCenter: TPointF read GetViewCenter write SetViewCenter;
    property AutoViewCenter: boolean read FAutoViewCenter write SetAutoViewCenter;
    property AutoZoom: boolean read FAutoZoom write SetAutoZoom;
    property Surface: TBGRACustomBitmap read FSurface write FSurface;
    property Object3D[AIndex: integer]: IBGRAObject3D read GetObject;
    property Object3DCount: integer read FObjectCount;
    property VertexCount: integer read GetVertexCount;
    property NormalCount: integer read GetNormalCount;
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
    property Camera: TCamera3D read FCamera;
    property DefaultMaterial: IBGRAMaterial3D read FDefaultMaterial write SetDefaultMaterial;
  end;

implementation

uses BGRACoordPool3D, BGRAUTF8;

{$i lightingclasses3d.inc}
{$i vertex3d.inc}
{$i face3d.inc}
{$i part3d.inc}
{$i object3d.inc}
{$i shapes3d.inc}

{ TCamera3D }

function TCamera3D.GetLookWhere: TPoint3D;
begin
  result := Point3D(FLookWhere);
end;

function TCamera3D.GetMatrix: TMatrix3D;
begin
  if not FMatrixComputed then
  begin
    ComputeMatrix;
    FMatrixComputed := true;
  end;
  result := FMatrix;
end;

function TCamera3D.GetViewPoint: TPoint3D;
begin
  result := Point3D(FViewPoint);
end;

procedure TCamera3D.SetMatrix(AValue: TMatrix3D);
begin
  FMatrix := AValue;
  FMatrixComputed:= true;
  FViewPoint := Point3D_128(FMatrix[1,4],FMatrix[2,4],FMatrix[3,4]);
end;

procedure TCamera3D.SetViewPoint(AValue: TPoint3D);
begin
  FViewPoint := Point3D_128(AValue);
  FMatrix[1,4] := FViewPoint.x;
  FMatrix[2,4] := FViewPoint.y;
  FMatrix[3,4] := FViewPoint.z;
  FMatrixComputed := false;
end;

procedure TCamera3D.ComputeMatrix;
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

procedure TCamera3D.LookAt(AWhere: TPoint3D; ATopDir: TPoint3D);
begin
  FLookWhere := Point3D_128(AWhere);
  FTopDir := Point3D_128(ATopDir);
  FMatrixComputed := false;
end;

procedure TCamera3D.LookLeft(angleDeg: single);
var m,inv: TMatrix3D;
begin
  inv := MatrixInverse3D(Matrix);
  m := MatrixRotateY(angleDeg*Pi/180);
  FLookWhere := inv*m*Matrix*FLookWhere;
  FMatrixComputed := false;
end;

procedure TCamera3D.LookRight(angleDeg: single);
begin
  LookLeft(-angleDeg);
end;

procedure TCamera3D.LookUp(angleDeg: single);
var m,inv: TMatrix3D;
begin
  inv := MatrixInverse3D(Matrix);
  m := MatrixRotateX(-angleDeg*Pi/180);
  FLookWhere := inv*m*Matrix*FLookWhere;
  FMatrixComputed := false;
end;

procedure TCamera3D.LookDown(angleDeg: single);
begin
  LookUp(-angleDeg);
end;


{ TBGRAScene3D }

function TBGRAScene3D.GetViewCenter: TPointF;
begin
  if FAutoViewCenter then
  begin
    result := PointF((GetRenderWidth-1)/2,(GetRenderHeight-1)/2)
  end
  else
    result := FViewCenter;
end;

function TBGRAScene3D.GetViewPoint: TPoint3D;
begin
  result := Camera.ViewPoint;
end;

function TBGRAScene3D.GetZoom: TPointF;
var size: single;
begin
  if FAutoZoom then
  begin
    Size := sqrt(GetRenderWidth*GetRenderHeight)*0.8;
    if Size = 0 then
      result := PointF(1,1)
    else
      result := PointF(size,size);
  end else
    result := FZoom;
end;

procedure TBGRAScene3D.SetAmbiantLightColorF(const AValue: TColorF);
begin
  FAmbiantLightColorF := AValue;
end;

procedure TBGRAScene3D.SetAmbiantLightness(const AValue: single);
begin
  FAmbiantLightColorF := ColorF(AValue, AValue, AValue, 1);
end;

procedure TBGRAScene3D.SetAmbiantLightColor(const AValue: TBGRAPixel);
begin
  FAmbiantLightColorF := ColorInt65536ToColorF(BGRAToColorInt(AValue,True));
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
  result := ColorIntToBGRA(ColorFToColorInt65536(FAmbiantLightColorF),True);
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

function TBGRAScene3D.GetNormalCount: integer;
var i: integer;
begin
  result := 0;
  for i := 0 to Object3DCount-1 do
    result += Object3D[i].TotalNormalCount;
end;

function TBGRAScene3D.GetAmbiantLightness: single;
begin
  result := (FAmbiantLightColorF[1]+FAmbiantLightColorF[2]+FAmbiantLightColorF[3])/3;
end;

function TBGRAScene3D.GetAmbiantLightColorF: TColorF;
begin
  result := FAmbiantLightColorF;
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

procedure TBGRAScene3D.SetDefaultMaterial(AValue: IBGRAMaterial3D);
begin
  if FDefaultMaterial=AValue then Exit;
  FDefaultMaterial:=AValue;
  InvalidateMaterial;
end;

procedure TBGRAScene3D.SetViewCenter(const AValue: TPointF);
begin
  FViewCenter := AValue;
  FAutoViewCenter:= False;
end;

procedure TBGRAScene3D.SetViewPoint(const AValue: TPoint3D);
begin
  Camera.ViewPoint := AValue;
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
  UnknownColor := BGRA(0,128,255);
  FAutoZoom := True;
  FAutoViewCenter := True;

  FCamera := TCamera3D.Create;
  Camera.ViewPoint := Point3D(0,0,-100);
  Camera.LookAt(Point3D(0,0,0), Point3D(0,-1,0));
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
var
  i: Integer;
begin
  DoClear;
  FreeAndNil(FLights);
  FreeAndNil(FCamera);
  for i := 0 to high(FTexturesFetched) do
    FTexturesFetched[i].Bitmap.Free;
  inherited Destroy;
end;

procedure TBGRAScene3D.Clear;
begin
  DoClear;
  DefaultMaterial := CreateMaterial;
end;

function TBGRAScene3D.FetchObject(AName: string; SwapFacesOrientation: boolean
  ): IBGRAObject3D;
begin
  if FetchDirectory = '' then raise exception.Create('Please define first the FetchDirectory');
  try
    result := LoadObjectFromFileUTF8(ConcatPaths([FetchDirectory,AName]), SwapFacesOrientation);
  except
    on ex:Exception do
      HandleFetchException(ex);
  end;
end;

procedure TBGRAScene3D.UseMaterial(AMaterialName: string; AFace: IBGRAFace3D);

  function ParseColor(text: string): TBGRAPixel;
  var
    color,tempColor: TBGRAPixel;
  begin
    color := UnknownColor;

    if copy(text,1,2) = 'dk' then
    begin
      tempcolor := ParseColor(copy(text,3,length(text)-2));
      tempcolor := MergeBGRA(tempcolor,3,BGRABlack,1);
      color := StrToBGRA('dark'+copy(text,3,length(text)-2),tempcolor);
    end;
    if copy(text,1,2) = 'lt' then
    begin
      tempcolor := ParseColor(copy(text,3,length(text)-2));
      tempcolor := MergeBGRA(tempcolor,3,BGRAWhite,1);
      color := StrToBGRA('light'+copy(text,3,length(text)-2),tempcolor);
    end;
    Color := StrToBGRA(StringReplace(text,'deep','dark',[]),Color);
    Color := StrToBGRA(StringReplace(text,'dark','deep',[]),Color);
    Color := StrToBGRA(text,Color);
    result := color;
  end;

var
  mat: IBGRAMaterial3D;
  c: TBGRAPixel;
begin
  mat := GetMaterialByName(AMaterialName);
  if mat = nil then
  begin
    mat := CreateMaterial;
    mat.Name := AMaterialName;
    c := ParseColor(AMaterialName);
    mat.AmbiantColor := c;
    mat.DiffuseColor := c;
  end;
  AFace.Material := mat;
end;

function TBGRAScene3D.LoadBitmapFromFileUTF8(AFilenameUTF8: string): TBGRACustomBitmap;
begin
  result := BGRABitmapFactory.Create(AfileNameUTF8,True);
end;

function TBGRAScene3D.FetchTexture(AName: string; out texSize: TPointF): IBGRAScanner;
var
  i: Integer;
  bmp: TBGRACustomBitmap;
begin
  bmp := nil;
  for i := 0 to high(FTexturesFetched) do
    if FTexturesFetched[i].Name = AName then
    begin
      bmp := FTexturesFetched[i].Bitmap;
      result := bmp;
      texSize := PointF(bmp.Width,bmp.Height);
      exit;
    end;
  if FetchDirectory <> '' then
  begin
    try
      bmp := LoadBitmapFromFileUTF8(ConcatPaths([FetchDirectory,AName]));
    except
      on ex:Exception do
        HandleFetchException(ex);
    end;
  end;
  if bmp = nil then
  begin
    result := nil;
    texSize := PointF(1,1);
  end else
  begin
    setlength(FTexturesFetched, length(FTexturesFetched)+1);
    FTexturesFetched[high(FTexturesFetched)].Name := AName;
    FTexturesFetched[high(FTexturesFetched)].Bitmap := bmp;
    result := bmp;
    texSize := PointF(bmp.Width,bmp.Height);
  end;
end;

procedure TBGRAScene3D.FetchMaterials(ALibraryName: string);
var
  i: Integer;
begin
  if FetchDirectory <> '' then
  begin
    for i := 0 to high(FMaterialLibrariesFetched) do
      if FMaterialLibrariesFetched[i]=ALibraryName then exit;
    setlength(FMaterialLibrariesFetched,length(FMaterialLibrariesFetched)+1);
    FMaterialLibrariesFetched[high(FMaterialLibrariesFetched)] := ALibraryName;
    try
      LoadMaterialsFromFile(ConcatPaths([FetchDirectory,ALibraryName]));
    except
      on ex:Exception do
        HandleFetchException(ex);
    end;
  end;
end;

procedure TBGRAScene3D.HandleFetchException(AException: Exception);
begin
  if FetchThrowsException then
    raise AException;
end;

procedure TBGRAScene3D.DoClear;
var i: integer;
begin
  for i := 0 to FLights.Count-1 do
    TBGRALight3D(FLights[i]).ReleaseInterface;
  FLights.Clear;

  for i := 0 to FObjectCount-1 do
  begin
    FObjects[i].Clear;
    FObjects[i] := nil;
  end;
  FObjects := nil;
  FObjectCount := 0;

  FMaterials := nil;
  FMaterialCount := 0;
  DefaultMaterial := nil;
end;

function TBGRAScene3D.GetRenderWidth: integer;
begin
  if Assigned(FRenderer) then
    result := FRenderer.SurfaceWidth
  else
  if Assigned(FSurface) then
    result := FSurface.Width
  else
    result := 0;
end;

function TBGRAScene3D.GetRenderHeight: integer;
begin
  if Assigned(FRenderer) then
    result := FRenderer.SurfaceHeight
  else
  if Assigned(FSurface) then
    result := FSurface.Height
  else
    result := 0;
end;

procedure TBGRAScene3D.OnMaterialTextureChanged(ASender: TObject);
begin
  InvalidateMaterial;
end;

procedure TBGRAScene3D.InvalidateMaterial;
var
  i: Integer;
begin
  for i := 0 to FObjectCount-1 do
    FObjects[i].InvalidateMaterial;
end;

function TBGRAScene3D.LoadObjectFromFile(AFilename: string; SwapFacesOrientation: boolean): IBGRAObject3D;
begin
  result := LoadObjectFromFileUTF8(SysToUTF8(AFilename), SwapFacesOrientation);
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
  secondValue,thirdValue: string;

  function GetNextToken: string;
  var idxStart,idxEnd,idxSlash: integer;
  begin
    idxStart := 1;
    while (idxStart <= length(s)) and (s[idxStart]in[' ',#9]) do inc(idxStart);
    if idxStart > length(s) then
    begin
      result := '';
      exit;
    end;
    idxEnd := idxStart;
    while (idxEnd < length(s)) and not (s[idxEnd+1]in[' ',#9]) do inc(idxEnd);
    result := copy(s,idxStart, idxEnd-idxStart+1);
    delete(s,1,idxEnd);
    idxSlash := pos('/',result);
    if idxSlash <> 0 then
    begin
      secondValue:= copy(result,idxSlash+1,length(result)-idxSlash);
      result := copy(result,1,idxSlash-1);
      idxSlash:= pos('/',secondValue);
      if idxSlash <> 0 then
      begin
        thirdValue:= copy(secondValue,idxSlash+1,length(secondValue)-idxSlash);
        secondValue:= copy(secondValue,1,idxSlash-1);
      end else
        thirdValue:= '';
    end else
    begin
      secondValue:= '';
      thirdValue:= '';
    end;
  end;

type
  TFaceVertexExtra = record
    normal: IBGRANormal3D;
    texCoord: TPointF;
  end;

var lineType : string;
    x,y,z : single;
    code : integer;
    faceVertices: array of IBGRAVertex3D;
    faceExtra: array of TFaceVertexExtra;
    NbFaceVertices,v,v2,v3,i: integer;
    tempV: IBGRAVertex3D;
    tempN: TFaceVertexExtra;
    materialname: string;
    face: IBGRAFace3D;
    lines: TStringList;
    lineIndex: integer;
    texCoords: array of TPointF;
    nbTexCoords: integer;

begin
  lines := TStringList.Create;
  lines.LoadFromStream(AStream);
  result := CreateObject;
  faceVertices := nil;
  faceExtra := nil;
  NbFaceVertices:= 0;
  materialname := 'default';
  lineIndex := 0;
  texCoords := nil;
  nbTexCoords:= 0;
  while lineIndex < lines.Count do
  begin
    s := lines[lineIndex];
    if pos('#',s) <> 0 then
      s := copy(s,1,pos('#',s)-1);
    inc(lineIndex);
    lineType := GetNextToken;
    if lineType = 'v' then
    begin
      val(GetNextToken,x,code);
      val(GetNextToken,y,code);
      val(GetNextToken,z,code);
      result.MainPart.Add(x,y,z);
    end else
    if lineType = 'vt' then
    begin
      val(GetNextToken,x,code);
      val(GetNextToken,y,code);
      if nbTexCoords >= length(texCoords) then
        setlength(texCoords, length(texCoords)*2+1);
      texCoords[nbTexCoords] := PointF(x,y);
      inc(nbTexCoords);
    end else
    if lineType = 'vn' then
    begin
      val(GetNextToken,x,code);
      val(GetNextToken,y,code);
      val(GetNextToken,z,code);
      result.MainPart.AddNormal(x,y,z);
      result.LightingNormal := lnVertex;
    end else
    if lineType = 'mtllib' then
      FetchMaterials(trim(s))
    else
    if lineType = 'usemtl' then
      materialname := trim(s)
    else
    if lineType = 'f' then
    begin
      NbFaceVertices:= 0;
      repeat
        val(GetNextToken,v,code);
        if (code = 0) and (v < 0) then v := result.MainPart.VertexCount+1+v;
        if (code = 0) and (v >= 1) and (v <= result.MainPart.VertexCount) then
        begin
          if length(faceVertices) = NbFaceVertices then
          begin
            setlength(faceVertices, length(faceVertices)*2+1);
            setlength(faceExtra, length(faceExtra)*2+1);
          end;
          faceVertices[NbFaceVertices] := result.MainPart.Vertex[v-1];
          val(secondValue,v2,code);
          if (code = 0) and (v2 < 0) then v2 := nbTexCoords+1+v2;
          if (code = 0) and (v2 >= 1) and (v2-1 < nbTexCoords) then
            faceExtra[NbFaceVertices].texCoord := texCoords[v2-1]
          else if nbTexCoords > v-1 then
            faceExtra[NbFaceVertices].texCoord := texCoords[v-1]
          else
            faceExtra[NbFaceVertices].texCoord := PointF(0,0);
          val(thirdValue,v3,code);
          if (code = 0) and (v3 < 0) then v3 := result.MainPart.NormalCount+1+v3;
          if code = 0 then
            faceExtra[NbFaceVertices].normal := result.MainPart.Normal[v3-1]
          else if result.MainPart.NormalCount > v-1 then
            faceExtra[NbFaceVertices].normal := result.MainPart.Normal[v-1]
          else
            faceExtra[NbFaceVertices].normal := nil;
          inc(NbFaceVertices);
        end else break;
      until false;
      if NbFaceVertices > 2 then
      begin
        if SwapFacesOrientation then
          for i := 0 to NbFaceVertices div 2-1 do
          begin
            tempV := faceVertices[i];
            faceVertices[i] := faceVertices[NbFaceVertices-1-i];
            faceVertices[NbFaceVertices-1-i] := tempV;
            tempN := faceExtra[i];
            faceExtra[i] := faceExtra[NbFaceVertices-1-i];
            faceExtra[NbFaceVertices-1-i] := tempN;
          end;
        face := result.AddFace(slice(faceVertices,NbFaceVertices));
        for i := 0 to NbFaceVertices-1 do
        begin
          face.SetNormal(i, faceExtra[i].normal);
          face.SetTexCoord(i, faceExtra[i].texCoord);
        end;
        face.MaterialName := materialname;
      end;
    end;
  end;
  lines.Free;
end;

procedure TBGRAScene3D.LoadMaterialsFromFile(AFilename: string);
var source: TFileStream;
begin
  source := TFileStream.Create(AFilename,fmOpenRead,fmShareDenyWrite);
  try
    LoadMaterialsFromStream(source);
  finally
    source.free;
  end;
end;

procedure TBGRAScene3D.LoadMaterialsFromFileUTF8(AFilename: string);
var source: TFileStreamUTF8;
begin
  source := TFileStreamUTF8.Create(AFilename,fmOpenRead,fmShareDenyWrite);
  try
    LoadMaterialsFromStream(source);
  finally
    source.free;
  end;
end;

procedure TBGRAScene3D.LoadMaterialsFromStream(AStream: TStream);
var
  s: String;

  function GetNextToken: string;
  var idxStart,idxEnd: integer;
  begin
    idxStart := 1;
    while (idxStart <= length(s)) and (s[idxStart]in[#9,' ']) do inc(idxStart);
    if idxStart > length(s) then
    begin
      result := '';
      exit;
    end;
    idxEnd := idxStart;
    while (idxEnd < length(s)) and not (s[idxEnd+1]in[#9,' ']) do inc(idxEnd);
    result := copy(s,idxStart, idxEnd-idxStart+1);
    delete(s,1,idxEnd);
  end;

  function GetSingle: single;
  var code: integer;
  begin
    val(GetNextToken,result,code);
  end;

  function GetColorF: TColorF;
  var r,g,b: single;
    code: integer;
  begin
    val(GetNextToken,r,code);
    val(GetNextToken,g,code);
    val(GetNextToken,b,code);
    result := ColorF(r,g,b,1);
  end;

var
  lines: TStringList;
  lineIndex: integer;
  lineType: String;
  currentMaterial: IBGRAMaterial3D;
  materialName: string;
  texZoom: TPointF;
  v: single;

begin
  lines := TStringList.Create;
  lines.LoadFromStream(AStream);
  lineIndex := 0;
  while lineIndex < lines.Count do
  begin
    s := lines[lineIndex];
    if pos('#',s) <> 0 then
      s := copy(s,1,pos('#',s)-1);
    inc(lineIndex);
    lineType := GetNextToken;
    if lineType = 'newmtl' then
    begin
      materialName := trim(s);
      currentMaterial := GetMaterialByName(materialName);
      if currentMaterial = nil then
      begin
        currentMaterial := CreateMaterial;
        currentMaterial.Name := materialName;
      end;
    end else
    if currentMaterial <> nil then
    begin
      if lineType = 'Ka' then currentMaterial.AmbiantColorF := GetColorF else
      if lineType = 'Kd' then currentMaterial.DiffuseColorF := GetColorF else
      if lineType = 'Ks' then currentMaterial.SpecularColorF := GetColorF else
      if (lineType = 'map_Ka') or (lineType = 'map_Kd') then
      begin
        currentMaterial.Texture := FetchTexture(trim(s),texZoom);
        texZoom.y := -texZoom.y;
        currentMaterial.TextureZoom := texZoom;
      end else
      if lineType = 'Ns' then currentMaterial.SpecularIndex := round(GetSingle) else
      if lineType = 'd' then
      begin
        v := GetSingle;
        if v > 1 then
          currentMaterial.SimpleAlpha := 255
        else if v < 0 then
          currentMaterial.SimpleAlpha := 0
        else
          currentMaterial.SimpleAlpha := round(v*255);
      end;
    end;
  end;
  lines.Free;
end;

procedure TBGRAScene3D.LookAt(AWhere: TPoint3D; ATopDir: TPoint3D);
begin
  Camera.LookAt(AWhere,ATopDir);
end;

procedure TBGRAScene3D.LookLeft(angleDeg: single);
begin
  Camera.LookLeft(angleDeg);
end;

procedure TBGRAScene3D.LookRight(angleDeg: single);
begin
  Camera.LookRight(angleDeg);
end;

procedure TBGRAScene3D.LookUp(angleDeg: single);
begin
  Camera.LookUp(angleDeg);
end;

procedure TBGRAScene3D.LookDown(angleDeg: single);
begin
  Camera.LookDown(angleDeg);
end;

procedure TBGRAScene3D.Render;
begin
  FRenderer := TBGRARenderer3D.Create(FSurface, RenderingOptions,
    FAmbiantLightColorF,
    FLights);
  DoRender;
  FRenderer.Free;
end;

procedure TBGRAScene3D.Render(ARenderer: TCustomRenderer3D);
begin
  FRenderer := ARenderer;
  DoRender;
  FRenderer := nil;
end;

procedure TBGRAScene3D.ComputeView(ScaleX,ScaleY: single);
var
  i: Integer;
begin
  FProjection.Zoom := Zoom;
  FProjection.Zoom.X *= ScaleX;
  FProjection.Zoom.Y *= ScaleY;
  FProjection.Center := ViewCenter;
  FProjection.Center.X *= ScaleX;
  FProjection.Center.Y *= ScaleY;
  for i := 0 to FObjectCount-1 do
    FObjects[i].ComputeWithMatrix(Camera.Matrix, FProjection);
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

procedure TBGRAScene3D.DoRender;
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
      obj.Update;
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
  faceDesc: TFaceRenderingDescription;
  LVertices: array of TBGRAVertex3D;

  procedure DrawFace(numFace: integer);
  var
    j,k: Integer;
    VCount,NewVCount: integer;
    NegNormals: boolean;
    LastVisibleVertex: integer;

    procedure AddZIntermediate(n1,n2: integer);
    var t: single;
        v1,v2: TBGRAVertex3D;
    begin
       v1 := LVertices[n1];
       v2 := LVertices[n2];
       t := (RenderingOptions.MinZ - v1.ViewCoord.z)/(v2.ViewCoord.z - v1.ViewCoord.z);
       LVertices[NewVCount] := nil; //computed

       faceDesc.Colors[NewVCount] := MergeBGRA(faceDesc.Colors[n1],round((1-t)*65536),faceDesc.Colors[n2],round(t*65536));
       faceDesc.TexCoords[NewVCount] := faceDesc.TexCoords[n1]*(1-t) + faceDesc.TexCoords[n2]*t;
       faceDesc.Positions3D[NewVCount] := faceDesc.Positions3D[n1]*(1-t) + faceDesc.Positions3D[n2]*t;
       faceDesc.Normals3D[NewVCount] := faceDesc.Normals3D[n1]*(1-t) + faceDesc.Normals3D[n2]*t;
       faceDesc.Projections[NewVCount] := ComputeCoordinate(faceDesc.Positions3D[NewVCount]);
       NewVCount += 1;
    end;

    procedure LoadVertex(idxL: integer; idxV: integer);
    var vertexDesc: PBGRAFaceVertexDescription;
        tempV: TBGRAVertex3D;
    begin
      with LFaces[numFace] do
      begin
        vertexDesc := VertexDescription[idxV];
        with vertexDesc^ do
        begin
          tempV := TBGRAVertex3D(vertex.GetAsObject);
          LVertices[idxL] := tempV;

          faceDesc.Colors[idxL] := ActualColor;
          faceDesc.TexCoords[idxL] := ActualTexCoord;

          with tempV.CoordData^ do
          begin
            faceDesc.Positions3D[idxL] := viewCoord;
            facedesc.Normals3D[idxL] := viewNormal;
            faceDesc.Projections[idxL] := projectedCoord;
          end;
          if Normal <> nil then
            facedesc.Normals3D[idxL] := Normal.ViewNormal_128;
          Normalize3D_128(facedesc.Normals3D[idxL]);
        end;
      end;
    end;

  begin
     with LFaces[numFace] do
     begin
       VCount := VertexCount;
       if VCount < 3 then exit;

       faceDesc.NormalsMode := Object3D.LightingNormal;

       faceDesc.Material := ActualMaterial;
       if faceDesc.Material = nil then exit;
       faceDesc.Texture := ActualTexture;

       if length(LVertices) < VCount+3 then  //keep margin for z-clip
       begin
         setlength(LVertices, (VCount+3)*2);
         setlength(faceDesc.Colors, length(LVertices));
         setlength(faceDesc.TexCoords, length(LVertices));
         setlength(faceDesc.Projections, length(LVertices));
         setlength(faceDesc.Positions3D, length(LVertices));
         setlength(faceDesc.Normals3D, length(LVertices));
       end;

       if FRenderer.HandlesNearClipping then
       begin
         for j := 0 to VCount-1 do
           LoadVertex(j,j);
       end else
       begin
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
       end;

       if not FRenderer.HandlesFaceCulling then
       begin
         if not IsPolyVisible(slice(faceDesc.Projections,VCount)) then
         begin
           if not Biface then exit;
           NegNormals := True;
         end else
         begin
           NegNormals := False;
         end;
       end else
         NegNormals := false;

       //compute normals
       case faceDesc.NormalsMode of
         lnFace: for j := 0 to VCount-1 do
                   faceDesc.Normals3D[j] := ViewNormal_128;
         lnFaceVertexMix:
             for j := 0 to VCount-1 do
             begin
               faceDesc.Normals3D[j] += ViewNormal_128;
               Normalize3D_128(faceDesc.Normals3D[j]);
             end;
       end;
       if NegNormals then
         for j := 0 to VCount-1 do
           faceDesc.Normals3D[j] := -faceDesc.Normals3D[j];

       if LightThroughFactorOverride then
         faceDesc.LightThroughFactor := LightThroughFactor
       else
         faceDesc.LightThroughFactor := faceDesc.Material.GetLightThroughFactor;

       faceDesc.NbVertices:= VCount;
       faceDesc.Biface := Biface;

       if FRenderer.RenderFace(faceDesc, @ComputeCoordinate) then
         inc(FRenderedFaceCount);
     end;
  end;

var i,j: integer;

begin
  FRenderedFaceCount:= 0;

  PrepareFaces;
  ComputeView(FRenderer.GlobalScale,FRenderer.GlobalScale);
  FRenderer.Projection := FProjection;

  SortFaces(LFaces);
  LVertices := nil;

  //if there is a Z-Buffer, it is possible to avoid drawing things that
  //are hidden by opaque faces by drawing first all opaque faces
  if FRenderer.HasZBuffer then
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
end;

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
var m: TBGRAMaterial3D;
begin
  m := TBGRAMaterial3D.Create;
  m.OnTextureChanged := @OnMaterialTextureChanged;
  result := m;
  AddMaterial(result);
end;

function TBGRAScene3D.CreateMaterial(ASpecularIndex: integer): IBGRAMaterial3D;
var m: TBGRAMaterial3D;
begin
  m := TBGRAMaterial3D.Create;
  m.SetSpecularIndex(ASpecularIndex);
  m.SetSpecularColor(BGRAWhite);
  m.OnTextureChanged := @OnMaterialTextureChanged;
  result := m;
  AddMaterial(result);
end;

function TBGRAScene3D.GetMaterialByName(AName: string): IBGRAMaterial3D;
var i: integer;
begin
  for i := 0 to MaterialCount-1 do
    if AName = Material[i].Name then
    begin
      result := Material[i];
      exit;
    end;
  result := nil;
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

function TBGRAScene3D.MakeLightList: TList;
var i: integer;
begin
  result := TList.Create;
  for i := 0 to FLights.Count-1 do
    result.Add(FLights[i]);
end;

initialization

  Randomize;

end.

