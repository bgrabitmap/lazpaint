type
  { TBGRADirectionalLight3D }

  TBGRADirectionalLight3D = class(TBGRALight3D,IBGRADirectionalLight3D)
  protected
    FDirection, FBetweenDirectionAndObserver: TPoint3D_128;
  public
    constructor Create(ADirection: TPoint3D);
    function GetDirection: TPoint3D; override;
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
    function GetIntensity: single; override;
    procedure SetIntensity(const AValue: single);

    function GetVertex: IBGRAVertex3D;
    procedure SetVertex(const AValue: IBGRAVertex3D);
    function GetPosition: TPoint3D; override;

    procedure ComputeDiffuseAndSpecularColor(Context: PSceneLightingContext); override;
    procedure ComputeDiffuseLightness(Context: PSceneLightingContext); override;
    procedure ComputeDiffuseColor(Context: PSceneLightingContext); override;
    function IsDirectional: boolean; override;
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

function TBGRAPointLight3D.GetPosition: TPoint3D;
begin
  Result:= FVertex.GetViewCoord;
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
    intensity := DotProduct3D_128(vect, Context^.basic.Normal)/(dist2*sqrt(dist2))*FIntensity;
    if Context^.LightThrough and (intensity < 0) then intensity := -intensity*Context^.LightThroughFactor;
    if intensity > 100 then intensity := 100;
    if intensity < FMinIntensity then intensity := FMinIntensity;
    TBGRAMaterial3D(Context^.material).ComputeDiffuseLightness(Context,round(intensity*32768),FLightness);
  end;
end;

procedure TBGRAPointLight3D.ComputeDiffuseColor(Context: PSceneLightingContext);
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
    intensity := DotProduct3D_128(vect, Context^.basic.Normal)/(dist2*sqrt(dist2))*FIntensity;
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
  result := Point3D(-FDirection.x,-FDirection.y,-FDirection.z);
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

