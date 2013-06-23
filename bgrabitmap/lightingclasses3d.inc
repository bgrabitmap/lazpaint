type

  { TBGRAMaterial3D }

  TBGRAMaterial3D = class(TInterfacedObject, IBGRAMaterial3D)
  private
    FDiffuseColorInt: TColorInt65536;
    FDiffuseLightness: integer;
    FSpecularColorInt: TColorInt65536;
    FAutoDiffuseColor,FAutoSpecularColor: boolean;
    FSpecularIndex: integer;
    FSpecularOn: boolean;
    FSaturationLowF: single;
    FSaturationHighF: single;
    FLightThroughFactor: single;

    //phong precalc
    FPowerTable: array of single;
    FPowerTableSize, FPowerTableExp2: integer;
    FPowerTableSizeF: single;

    procedure UpdateSpecular;
    procedure ComputePowerTable;
  public
    constructor Create;
    destructor Destroy; override;

    function GetAutoDiffuseColor: boolean;
    function GetAutoSpecularColor: boolean;
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
    procedure SetAutoDiffuseColor(const AValue: boolean);
    procedure SetAutoSpecularColor(const AValue: boolean);
    procedure SetDiffuseColor(const AValue: TBGRAPixel);
    procedure SetDiffuseColorF(const AValue: TColorF);
    procedure SetDiffuseColorInt(const AValue: TColorInt65536);
    procedure SetLightThroughFactor(const AValue: single);
    procedure SetSpecularColor(const AValue: TBGRAPixel);
    procedure SetSpecularColorF(const AValue: TColorF);
    procedure SetSpecularColorInt(const AValue: TColorInt65536);
    procedure SetSpecularIndex(const AValue: integer);
    procedure SetSaturationHigh(const AValue: single);
    procedure SetSaturationLow(const AValue: single);

    function GetSpecularOn: boolean;
    function GetAsObject: TObject;
    procedure ComputeDiffuseAndSpecularColor(Context: PSceneLightingContext; DiffuseIntensity, SpecularIntensity, SpecularCosine: single; const ALightColor: TColorInt65536);
    procedure ComputeDiffuseColor(Context: PSceneLightingContext; const DiffuseIntensity: single; const ALightColor: TColorInt65536);
    procedure ComputeDiffuseLightness(Context: PSceneLightingContext; DiffuseLightnessTerm32768: integer; ALightLightness: integer);

  end;

{ TBGRAMaterial3D }

procedure TBGRAMaterial3D.UpdateSpecular;
begin
  FSpecularOn := (FSpecularIndex > 0) and ((FSpecularColorInt.r <> 0) or (FSpecularColorInt.g <> 0) or (FSpecularColorInt.b <> 0) or
                                            FAutoSpecularColor);
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
  SetDiffuseColorInt(ColorInt65536(65536,65536,65536));
  FAutoDiffuseColor:= True;
  FSpecularColorInt := ColorInt65536(0,0,0);
  FAutoSpecularColor:= True;
  FSpecularIndex := 10;
  FSpecularOn := false;
  FLightThroughFactor:= 0;
  SetSaturationLow(2);
  SetSaturationHigh(3);

  FPowerTableSize := 128;
  FPowerTableSizeF := FPowerTableSize;
  FPowerTable := nil;
end;

destructor TBGRAMaterial3D.Destroy;
begin
  inherited Destroy;
end;

function TBGRAMaterial3D.GetAutoDiffuseColor: boolean;
begin
  result := FAutoDiffuseColor;
end;

function TBGRAMaterial3D.GetAutoSpecularColor: boolean;
begin
  result := FAutoSpecularColor;
end;

function TBGRAMaterial3D.GetDiffuseColor: TBGRAPixel;
begin
  result := ColorIntToBGRA(FDiffuseColorInt);
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
  result := ColorIntToBGRA(FSpecularColorInt);
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

procedure TBGRAMaterial3D.SetAutoDiffuseColor(const AValue: boolean);
begin
  FAutoDiffuseColor:= AValue;
end;

procedure TBGRAMaterial3D.SetAutoSpecularColor(const AValue: boolean);
begin
  FAutoSpecularColor:= AValue;
  UpdateSpecular;
end;

procedure TBGRAMaterial3D.SetDiffuseColor(const AValue: TBGRAPixel);
begin
  FDiffuseColorInt := BGRAToColorInt(AValue);
  FDiffuseLightness := (FDiffuseColorInt.r + FDiffuseColorInt.g + FDiffuseColorInt.b) div 6;
end;

procedure TBGRAMaterial3D.SetDiffuseColorF(const AValue: TColorF);
begin
  FDiffuseColorInt := ColorFToColorInt65536(AValue);
  FDiffuseLightness := (FDiffuseColorInt.r + FDiffuseColorInt.g + FDiffuseColorInt.b) div 6;
end;

procedure TBGRAMaterial3D.SetDiffuseColorInt(const AValue: TColorInt65536);
begin
  FDiffuseColorInt := AValue;
  FDiffuseLightness := (FDiffuseColorInt.r + FDiffuseColorInt.g + FDiffuseColorInt.b) div 6;
end;

procedure TBGRAMaterial3D.SetLightThroughFactor(const AValue: single);
begin
  FLightThroughFactor:= AValue;
end;

procedure TBGRAMaterial3D.SetSpecularColor(const AValue: TBGRAPixel);
begin
  FSpecularColorInt := BGRAToColorInt(AValue);
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
  FPowerTable := nil;
  UpdateSpecular;
end;

procedure TBGRAMaterial3D.SetSaturationHigh(const AValue: single);
begin
  FSaturationHighF:= AValue;
end;

procedure TBGRAMaterial3D.SetSaturationLow(const AValue: single);
begin
  FSaturationLowF:= AValue;
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
  PowerTableIPos,i: integer;
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
      NH *= FPowerTableSize;
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
    Context^.diffuseColor += FDiffuseColorInt*round(DiffuseIntensity*65536);

  if FAutoSpecularColor then
    Context^.specularColor += ALightColor*round(SpecularIntensity* NnH*65536)
  else
    Context^.specularColor += FSpecularColorInt*round(SpecularIntensity* NnH*65536);
end;

procedure TBGRAMaterial3D.ComputeDiffuseColor(Context: PSceneLightingContext;
  const DiffuseIntensity: single; const ALightColor: TColorInt65536);
begin
  if FAutoDiffuseColor then
    Context^.diffuseColor += ALightColor*round(DiffuseIntensity*65536)
  else
    Context^.diffuseColor += FDiffuseColorInt*round(DiffuseIntensity*65536);
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
      Context^.lightness += CombineLightness(DiffuseLightnessTerm32768,FDiffuseLightness)
    else
      Context^.lightness += DiffuseLightnessTerm32768;
  end;
end;

type

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
  end;

  { TBGRADirectionalLight3D }

  TBGRADirectionalLight3D = class(TBGRALight3D,IBGRADirectionalLight3D)
  protected
    FDirection, FBetweenDirectionAndObserver: TPoint3D_128;
  public
    constructor Create(ADirection: TPoint3D);
    function GetDirection: TPoint3D;
    procedure SetDirection(const AValue: TPoint3D);

    procedure ComputeDiffuseAndSpecularColor(Context: PSceneLightingContext); override;
    procedure ComputeDiffuseColor(Context: PSceneLightingContext); override;
    procedure ComputeDiffuseLightness(Context: PSceneLightingContext); override;
    function IsDirectional: boolean; override;
  end;

  { TBGRAPointLight3D }

  TBGRAPointLight3D = class(TBGRALight3D,IBGRAPointLight3D)
  protected
    FVertex: IBGRAVertex3D;
    FIntensity: single;
  public
    constructor Create(AVertex: IBGRAVertex3D; AIntensity: single);
    function GetIntensity: single;
    procedure SetIntensity(const AValue: single);

    function GetVertex: IBGRAVertex3D;
    procedure SetVertex(const AValue: IBGRAVertex3D);

    procedure ComputeDiffuseAndSpecularColor(Context: PSceneLightingContext); override;
    procedure ComputeDiffuseLightness(Context: PSceneLightingContext); override;
    procedure ComputeDiffuseColor(Context: PSceneLightingContext); override;
    function IsDirectional: boolean; override;
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

function TBGRALight3D.GetLightnessF: single;
begin
  result := FLightness/32768;
end;

function TBGRALight3D.GetColor: TBGRAPixel;
begin
  result := ColorIntToBGRA(FColorInt);
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
  SetColorInt(BGRAToColorInt(AValue));
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

{ TBGRAPointLight3D }

constructor TBGRAPointLight3D.Create(AVertex: IBGRAVertex3D; AIntensity: single);
begin
  inherited Create;
  FVertex:= AVertex;
  FIntensity := AIntensity;
end;

function TBGRAPointLight3D.GetIntensity: single;
begin
  result := FIntensity;
end;

procedure TBGRAPointLight3D.SetIntensity(const AValue: single);
begin
  FIntensity:= AValue;
end;

function TBGRAPointLight3D.GetVertex: IBGRAVertex3D;
begin
  result := FVertex;
end;

procedure TBGRAPointLight3D.SetVertex(const AValue: IBGRAVertex3D);
begin
  FVertex := AValue;
end;

procedure TBGRAPointLight3D.ComputeDiffuseAndSpecularColor(Context: PSceneLightingContext);
  {$DEFINE PARAM_POINTLIGHT}
  {$i phonglight.inc}

procedure TBGRAPointLight3D.ComputeDiffuseLightness(Context: PSceneLightingContext);
const maxValue = 100*32768;
var
  vect: TPoint3D_128;
  dist2,intensity: single;
begin
  vect := FVertex.ViewCoord_128 - Context^.basic.Position;
  dist2 := DotProduct3D_128(vect,vect);
  if dist2 = 0 then
    TBGRAMaterial3D(Context^.material).ComputeDiffuseLightness(Context,maxValue,FLightness)
  else
  begin
    intensity := (DotProduct3D_128(vect, Context^.basic.Normal))/(dist2*sqrt(dist2))*FIntensity;
    if Context^.LightThrough and (intensity < 0) then intensity := -intensity*Context^.LightThroughFactor;
    if intensity > 100 then intensity := 100;
    if intensity < FMinIntensity then intensity := FMinIntensity;
    TBGRAMaterial3D(Context^.material).ComputeDiffuseLightness(Context,round(intensity*32768),FLightness);
  end;
end;

procedure TBGRAPointLight3D.ComputeDiffuseColor(Context: PSceneLightingContext
  );
var
  vect: TPoint3D_128;
  intensity,dist2: single;
begin
  vect := FVertex.ViewCoord_128 - Context^.basic.Position;
  dist2 := DotProduct3D_128(vect,vect);
  if dist2 = 0 then
    intensity := 100
  else
  begin
    intensity := (DotProduct3D_128(vect, Context^.basic.Normal))/(dist2*sqrt(dist2))*FIntensity;
    if Context^.LightThrough and (intensity < 0) then intensity := -intensity*Context^.LightThroughFactor;
    if intensity > 100 then intensity := 100;
    if intensity < FMinIntensity then intensity := FMinIntensity;
  end;

  TBGRAMaterial3D(Context^.material).ComputeDiffuseColor(Context,intensity, FColorInt);
end;

function TBGRAPointLight3D.IsDirectional: boolean;
begin
  result := false;
end;

{ TBGRADirectionalLight3D }

constructor TBGRADirectionalLight3D.Create(ADirection: TPoint3D);
begin
  inherited Create;
  SetDirection(ADirection);
end;

function TBGRADirectionalLight3D.GetDirection: TPoint3D;
begin
  result := Point3D(FDirection.x,FDirection.y,FDirection.z);
end;

procedure TBGRADirectionalLight3D.SetDirection(const AValue: TPoint3D);
begin
  FDirection := -Point3D_128(AValue.x,AValue.y,AValue.z);
  Normalize3D_128(FDirection);
  FBetweenDirectionAndObserver := FDirection + FViewVector;
  Normalize3D_128(FBetweenDirectionAndObserver);
end;

procedure TBGRADirectionalLight3D.ComputeDiffuseAndSpecularColor(Context: PSceneLightingContext);
  {$i phonglight.inc}

procedure TBGRADirectionalLight3D.ComputeDiffuseColor(Context: PSceneLightingContext);
var
  intensity: single;
begin
  intensity:= DotProduct3D_128(Context^.basic.Normal, FDirection);
  if Context^.LightThrough and (intensity < 0) then intensity := -intensity*Context^.LightThroughFactor;
  if intensity < FMinIntensity then intensity := FMinIntensity;

  TBGRAMaterial3D(Context^.material).ComputeDiffuseColor(Context,intensity,FColorInt);
end;

procedure TBGRADirectionalLight3D.ComputeDiffuseLightness(
  Context: PSceneLightingContext);
var
  intensity: single;
begin
  intensity:= DotProduct3D_128(Context^.basic.Normal, FDirection);
  if Context^.LightThrough and (intensity < 0) then intensity := -intensity*Context^.LightThroughFactor;
  if intensity < FMinIntensity then intensity := FMinIntensity;

  TBGRAMaterial3D(Context^.material).ComputeDiffuseLightness(Context,round(intensity*32768),FLightness);
end;

function TBGRADirectionalLight3D.IsDirectional: boolean;
begin
  result := true;
end;

