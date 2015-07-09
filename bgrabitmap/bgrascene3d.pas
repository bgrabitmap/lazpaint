unit BGRAScene3D;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmapTypes, BGRAColorInt, BGRASSE, BGRAMatrix3D;

type
  TProjection3D = BGRAMatrix3D.TProjection3D;
  TBox3D = record
    min,max: TPoint3D;
  end;

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
    function FetchTexture({%H-}AName: string; out texSize: TPointF): IBGRAScanner; virtual;

  public
    DefaultLightingNormal: TLightingNormal3D;
    DefaultMaterial : IBGRAMaterial3D;
    RenderingOptions: TRenderingOptions;
    UnknownColor: TBGRAPixel;

    constructor Create;
    constructor Create(ASurface: TBGRACustomBitmap);
    destructor Destroy; override;
    procedure Clear; virtual;
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
  end;

implementation

uses BGRAPolygon, BGRAPolygonAliased, BGRACoordPool3D, BGRAResample,
  BGRAUTF8;

{$i lightingclasses3d.inc}
{$i vertex3d.inc}
{$i face3d.inc}

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
    function GetScene: TBGRAScene3D;
    function GetRefCount: integer;
    procedure SetBiface(AValue : boolean);
    procedure ForEachVertex(ACallback: TVertex3DCallback);
    procedure ForEachFace(ACallback: TFace3DCallback);
  end;

{$i part3d.inc}
{$i object3d.inc}
{$i shapes3d.inc}

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

function TBGRAScene3D.GetNormalCount: integer;
var i: integer;
begin
  result := 0;
  for i := 0 to Object3DCount-1 do
    result += Object3D[i].TotalNormalCount;
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
  UnknownColor := BGRA(0,128,255);
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
  for i := 0 to FLights.Count-1 do
    TBGRALight3D(FLights[i])._Release;
  FLights.Clear;

  for i := 0 to FObjectCount-1 do
    FObjects[i].Clear;
  FObjects := nil;
  FObjectCount := 0;

  FMaterials := nil;
  FMaterialCount := 0;
  DefaultMaterial := CreateMaterial;
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

function TBGRAScene3D.FetchTexture(AName: string; out texSize: TPointF): IBGRAScanner;
begin
  result := nil;
  texSize := PointF(1,1);
end;

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
          begin
            if ColorOverride then
              LColors[idxL] := Color
            else
            begin
              if tempV.ParentColor then
                LColors[idxL] := Object3D.Color
              else
                LColors[idxL] := tempV.Color;
            end;
          end;

          if TexCoordOverride then
            LTexCoord[idxL] := TexCoord
          else
            LTexCoord[idxL] := tempV.TexCoord;
          with LMaterial.GetTextureZoom do
          begin
            LTexCoord[idxL].x *= x;
            LTexCoord[idxL].y *= y;
          end;

          with tempV.CoordData^ do
          begin
            LPos3D[idxL] := viewCoord;
            LNormal3D[idxL] := viewNormal;
            LProj[idxL] := projectedCoord;
            LZ[idxL] := viewCoord.Z;
          end;
          if Normal <> nil then
            LNormal3D[idxL] := Normal.ViewNormal_128;
        end;
      end;
    end;

  begin
     with LFaces[numFace] do
     begin
       VCount := VertexCount;
       if VCount < 3 then exit;

       if Material <> nil then
         LMaterial := TBGRAMaterial3D(Material.GetAsObject)
       else if Object3D.Material <> nil then
         LMaterial := TBGRAMaterial3D(Object3D.Material.GetAsObject)
       else if self.DefaultMaterial <> nil then
         LMaterial := TBGRAMaterial3D(self.DefaultMaterial.GetAsObject)
       else
         exit;

       if ParentTexture then
       begin
         if LMaterial.GetTexture <> nil then
           LTexture := LMaterial.GetTexture
         else
           LTexture := Object3D.Texture
       end
       else
         LTexture := Texture;

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
  m: TBGRAMaterial3D;
begin
  m := TBGRAMaterial3D(Context^.material);
  if not m.GetAutoSimpleColor then Color := ColorIntToBGRA(BGRAToColorIntMultiply(Color, m.GetSimpleColorInt));

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
  m: TBGRAMaterial3D;
begin
  m := TBGRAMaterial3D(Context^.material);

  if m.GetAutoAmbiantColor then
    Context^.diffuseColor := FAmbiantLightColor
  else
    Context^.diffuseColor := FAmbiantLightColor*m.GetAmbiantColorInt;

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
  m: TBGRAMaterial3D;
begin
  m := TBGRAMaterial3D(Context^.material);

  if m.GetAutoAmbiantColor then
    Context^.diffuseColor := FAmbiantLightColor
  else
    Context^.diffuseColor := FAmbiantLightColor*m.GetAmbiantColorInt;
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

function TBGRAScene3D.ApplyNoLighting(Context: PSceneLightingContext;
  Color: TBGRAPixel): TBGRAPixel;
var
  m: TBGRAMaterial3D;
begin
  m := TBGRAMaterial3D(Context^.material);

  if not m.GetAutoAmbiantColor then
    result := ColorIntToBGRA(BGRAToColorIntMultiply(Color, m.GetAmbiantColorInt))
  else
    result := Color;
end;

function TBGRAScene3D.ApplyLightingWithAmbiantLightnessOnly(
  Context: PSceneLightingContext; Color: TBGRAPixel): TBGRAPixel;
var
  m: TBGRAMaterial3D;
begin
  m := TBGRAMaterial3D(Context^.material);

  if not m.GetAutoAmbiantColor then
    Color := ColorIntToBGRA(BGRAToColorIntMultiply(Color, m.GetAmbiantColorInt));

  if FAmbiantLightness <= 0 then
    result := BGRA(0,0,0,color.alpha)
  else
    result := ApplyIntensityFast(Color, FAmbiantLightness);
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

initialization

  Randomize;

end.

