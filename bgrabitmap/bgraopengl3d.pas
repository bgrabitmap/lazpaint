unit BGRAOpenGL3D;

{$mode objfpc}{$H+}

interface

uses BGRABitmapTypes,
  BGRASceneTypes, BGRASSE,
  Classes, BGRAMatrix3D,
  BGRACanvasGL,
  BGRAScene3D,
  BGRAOpenGLType,
  BGRATransform,
  BGRARenderer3D;

type
  TAttributeVariable = BGRACanvasGL.TAttributeVariable;

  TBGLShader3D = class;

  { TBGLLighting3D }

  TBGLLighting3D = class
  private
    procedure SetUseBuiltIn(AValue: boolean);
  protected
    FCanvas: TBGLCustomCanvas;
    FLights: TList;
    FAmbiantLight: TColorF;
    FShaderLightingCode: string;
    FUseBuiltIn: boolean;
    procedure Init;
  public
    constructor Create(ACanvas: TBGLCustomCanvas; AAmbiantLight: TColorF; ALights: TList);
    procedure SetSpecularIndex(AIndex: Integer);
    destructor Destroy; override;
    property ShaderLightingCode: string read FShaderLightingCode;
    property UseOpenGLBuiltInLighting: boolean read FUseBuiltIn write SetUseBuiltIn;
  end;

  { TBGLRenderer3D }

  TBGLRenderer3D = class(TCustomRenderer3D)
  protected
    FCanvas: TBGLCustomCanvas;
    FHasZBuffer: Boolean;
    FGlobalScale: single;
    FOptions: TRenderingOptions;
    FFactorZ, FAddZ: single;
    FLightingGL: TBGLLighting3D;
    FLights: TList;
    FAmbiantLight: TColorF;
    FFar: single;
    FOldCulling: TFaceCulling;
    FOldMatrix: TAffineMatrix;
    FOldProjection, FProjectionMatrix: TMatrix4D;
    FShader, FShaderWithTexture: TBGLCustomShader;
    FBGRAShader: TBGRAShader3D;
    FShadedColorsF: array of TColorF;
    FShadedColors: array of TBGRAPixel;
    function GetHasZBuffer: boolean; override;
    function GetGlobalScale: single; override;
    function GetSurfaceWidth: integer; override;
    function GetSurfaceHeight: integer; override;
    procedure SetProjection(const AValue: TProjection3D); override;
    function GetHandlesNearClipping: boolean; override;
    function GetHandlesFaceCulling: boolean; override;
    procedure InitLighting(AUseOpenGLBuiltInLighting: boolean);
  public
    constructor Create(ACanvas: TBGLCustomCanvas;
      AScene: TBGRAScene3D; AFar: single);
    function RenderFace(var ADescription: TFaceRenderingDescription;
      {%H-}AComputeCoordinate: TComputeProjectionFunc): boolean; override;
    destructor Destroy; override;
    property Canvas: TBGLCustomCanvas read FCanvas;
  end;

  { TBGLScene3D }

  TBGLScene3D = class(TBGRAScene3D)
  protected
    function LoadBitmapFromFileUTF8(AFilenameUTF8: string): TBGRACustomBitmap; override;
  public
    procedure RenderGL(ACanvas: TBGLCustomCanvas; AMaxZ: single = 1000); virtual;
  end;

  { TUniformVariable }

  TUniformVariable = object
  private
    FProgram: TBGLShader3D;
    FVariable: DWord;
    procedure Init(AProgram: TBGLShader3D; AVariable: DWord);
  end;

  { TUniformVariableSingle }

  TUniformVariableSingle = object(TUniformVariable)
  private
    FValue: single;
    procedure SetValue(const AValue: single);
  public
    procedure Update;
    property Value: single read FValue write SetValue;
  end;

  { TUniformVariablePointF }

  TUniformVariablePointF = object(TUniformVariable)
  private
    FValue: TPointF;
    procedure SetValue(const AValue: TPointF);
  public
    procedure Update;
    property Value: TPointF read FValue write SetValue;
  end;

  { TUniformVariablePoint3D }

  TUniformVariablePoint3D = object(TUniformVariable)
  private
    FValue: TPoint3D;
    procedure SetValue(const AValue: TPoint3D);
  public
    procedure Update;
    property Value: TPoint3D read FValue write SetValue;
  end;

  { TUniformVariableInteger }

  TUniformVariableInteger = object(TUniformVariable)
  private
    FValue: Integer;
    procedure SetValue(const AValue: Integer);
  public
    procedure Update;
    property Value: Integer read FValue write SetValue;
  end;

  { TUniformVariablePoint }

  TUniformVariablePoint = object(TUniformVariable)
  private
    FValue: TPoint;
    procedure SetValue(const AValue: TPoint);
  public
    procedure Update;
    property Value: TPoint read FValue write SetValue;
  end;

  { TAttributeVariableSingle }

  TAttributeVariableSingle = object(TAttributeVariable)
  protected
    procedure Init(AProgram: TObject; AAttribute: DWord);
  end;

  { TAttributeVariablePointF }

  TAttributeVariablePointF = object(TAttributeVariable)
  protected
    procedure Init(AProgram: TObject; AAttribute: DWord);
  end;

  { TAttributeVariablePoint3D }

  TAttributeVariablePoint3D = object(TAttributeVariable)
  protected
    procedure Init(AProgram: TObject; AAttribute: DWord);
  end;

  { TAttributeVariableInteger }

  TAttributeVariableInteger = object(TAttributeVariable)
  protected
    procedure Init(AProgram: TObject; AAttribute: DWord);
  end;

  { TAttributeVariablePoint }

  TAttributeVariablePoint = object(TAttributeVariable)
  protected
    procedure Init(AProgram: TObject; AAttribute: DWord);
  end;

  { TBGLShader3D }

  TBGLShader3D = class(TBGLCustomShader)
  protected
    FUsed: boolean;
    FCanvas: TBGLCustomCanvas;
    FLighting: TBGLCustomLighting;
    FVertexShaderSource,
    FFragmentShaderSource: string;
    FVertexShader,
    FFragmentShader,
    FProgram: DWord;
    function GetUniformVariableSingle(AName: string): TUniformVariableSingle;
    function GetUniformVariablePointF(AName: string): TUniformVariablePointF;
    function GetUniformVariablePoint3D(AName: string): TUniformVariablePoint3D;
    function GetUniformVariableInteger(AName: string): TUniformVariableInteger;
    function GetUniformVariablePoint(AName: string): TUniformVariablePoint;
    function GetAttributeVariableInteger(AName: string): TAttributeVariableInteger;
    function GetAttributeVariablePoint(AName: string): TAttributeVariablePoint;
    function GetAttributeVariableSingle(AName: string): TAttributeVariableSingle;
    function GetAttributeVariablePointF(AName: string): TAttributeVariablePointF;
    function GetAttributeVariablePoint3D(AName: string): TAttributeVariablePoint3D;
    procedure SetUniformSingle(AVariable: DWord; const AValue; ACount: integer);
    procedure SetUniformInteger(AVariable: DWord; const AValue; ACount: integer);
    procedure CheckUsage(AUsing: boolean);
    procedure StartUse; override;
    procedure EndUse; override;
  public
    constructor Create(ACanvas: TBGLCustomCanvas; AVertexShaderSource: string;
        AFragmentShaderSource: string; AVaryingVariables: string = '';
        AVersion: string = '110');
    destructor Destroy; override;
    property UniformSingle[AName: string]: TUniformVariableSingle read GetUniformVariableSingle;
    property UniformPointF[AName: string]: TUniformVariablePointF read GetUniformVariablePointF;
    property UniformPoint3D[AName: string]: TUniformVariablePoint3D read GetUniformVariablePoint3D;
    property UniformInteger[AName: string]: TUniformVariableInteger read GetUniformVariableInteger;
    property UniformPoint[AName: string]: TUniformVariablePoint read GetUniformVariablePoint;
    property AttributeSingle[AName: string]: TAttributeVariableSingle read GetAttributeVariableSingle;
    property AttributePointF[AName: string]: TAttributeVariablePointF read GetAttributeVariablePointF;
    property AttributePoint3D[AName: string]: TAttributeVariablePoint3D read GetAttributeVariablePoint3D;
    property AttributeInteger[AName: string]: TAttributeVariableInteger read GetAttributeVariableInteger;
    property AttributePoint[AName: string]: TAttributeVariablePoint read GetAttributeVariablePoint;
    property IsUsed: boolean read FUsed;
  end;

function ProjectionToOpenGL(AProj: TProjection3D; ANear, AFar: Single): TMatrix4D;

implementation

uses SysUtils, BGRAColorInt;

type

  { TShaderWithTexture }

  TShaderWithTexture = class(TBGLShader3D)
  private
    function GetTexture: integer;
    procedure SetTexture(AValue: integer);
  protected
    FTextureUniform: TUniformVariableInteger;
    procedure StartUse; override;
  public
    class function GetCodeForTextureColor: string;
    constructor Create(ACanvas: TBGLCustomCanvas; AFragmentShader: string; ATexture: integer = 0);
    property Texture: integer read GetTexture write SetTexture;
  end;

function ProjectionToOpenGL(AProj: TProjection3D; ANear, AFar: Single): TMatrix4D;
begin
  result[1,1] := AProj.Zoom.X; result[2,1] := 0;            result[3,1] := -(AProj.Center.x + 0.5); result[4,1] := 0;
  result[1,2] := 0;            result[2,2] := AProj.Zoom.Y; result[3,2] := -(AProj.Center.y + 0.5); result[4,2] := 0;
  result[1,3] := 0;            result[2,3] := 0;            result[3,3] := -2/(AFar-ANear);         result[4,3] := -1 - AFar*result[3,3];
  result[1,4] := 0;            result[2,4] := 0;            result[3,4] := -1;                      result[4,4] := 0;
end;

{ TShaderWithTexture }

function TShaderWithTexture.GetTexture: integer;
begin
  result := FTextureUniform.Value;
end;

procedure TShaderWithTexture.SetTexture(AValue: integer);
begin
  FTextureUniform.Value := AValue;
end;

procedure TShaderWithTexture.StartUse;
begin
  inherited StartUse;
  FTextureUniform.Update;
end;

class function TShaderWithTexture.GetCodeForTextureColor: string;
begin
  result := 'texture2D(texture, texture_coordinate)';
end;

constructor TShaderWithTexture.Create(ACanvas: TBGLCustomCanvas;
  AFragmentShader: string; ATexture: integer);
begin
  inherited Create(ACanvas,
    'void main(void) ' +
    '{ ' +
    '  gl_Position = gl_ProjectionMatrix * gl_Vertex; ' +
    '  texture_coordinate = vec2(gl_MultiTexCoord0); ' +
    '  N = gl_Normal; ' +
    '  V = vec3(gl_Vertex); ' +
    '} ',

    'uniform sampler2D texture; ' +
    AFragmentShader,

    'varying vec2 texture_coordinate; ' +
    'varying vec3 N; ' +
    'varying vec3 V; ');
  FTextureUniform := UniformInteger['texture'];
  Texture := ATexture;
end;

{ TAttributeVariablePoint3D }

procedure TAttributeVariablePoint3D.Init(AProgram: TObject; AAttribute: DWord);
begin
  inherited Init(AProgram,AAttribute,3,True);
end;

{ TAttributeVariablePointF }

procedure TAttributeVariablePointF.Init(AProgram: TObject; AAttribute: DWord);
begin
  inherited Init(AProgram,AAttribute,2,True);
end;

{ TAttributeVariableInteger }

procedure TAttributeVariableInteger.Init(AProgram: TObject; AAttribute: DWord);
begin
  inherited Init(AProgram,AAttribute,1,False);
end;

{ TAttributeVariablePoint }

procedure TAttributeVariablePoint.Init(AProgram: TObject; AAttribute: DWord);
begin
  inherited Init(AProgram,AAttribute,2,False);
end;

{ TAttributeVariableSingle }

procedure TAttributeVariableSingle.Init(AProgram: TObject; AAttribute: DWord);
begin
  inherited Init(AProgram,AAttribute,1,True);
end;

{ TUniformVariablePoint }

procedure TUniformVariablePoint.SetValue(const AValue: TPoint);
begin
  if (FValue.x=AValue.x) and (FValue.y=AValue.y) then Exit;
  FValue:=AValue;
  if FProgram.IsUsed then Update;
end;

procedure TUniformVariablePoint.Update;
begin
  FProgram.SetUniformInteger(FVariable, FValue, 2);
end;

{ TUniformVariableInteger }

procedure TUniformVariableInteger.SetValue(const AValue: Integer);
begin
  if FValue=AValue then Exit;
  FValue:=AValue;
  if FProgram.IsUsed then Update;
end;

procedure TUniformVariableInteger.Update;
begin
  FProgram.SetUniformInteger(FVariable, FValue, 1);
end;

{ TUniformVariablePoint3D }

procedure TUniformVariablePoint3D.SetValue(const AValue: TPoint3D);
begin
  if (FValue.x=AValue.x) and (FValue.y=AValue.y) and (FValue.z=AValue.z) then Exit;
  FValue:=AValue;
  if FProgram.IsUsed then Update;
end;

procedure TUniformVariablePoint3D.Update;
begin
  FProgram.SetUniformSingle(FVariable, FValue, 3);
end;

{ TUniformVariablePointF }

procedure TUniformVariablePointF.SetValue(const AValue: TPointF);
begin
  if (FValue.x=AValue.x) and (FValue.y=AValue.y) then Exit;
  FValue:=AValue;
  if FProgram.IsUsed then Update;
end;

procedure TUniformVariablePointF.Update;
begin
  FProgram.SetUniformSingle(FVariable, FValue, 2);
end;

{ TUniformVariableSingle }

procedure TUniformVariableSingle.SetValue(const AValue: single);
begin
  if FValue=AValue then Exit;
  FValue:=AValue;
  if FProgram.IsUsed then Update;
end;

procedure TUniformVariableSingle.Update;
begin
  FProgram.SetUniformSingle(FVariable, FValue, 1);
end;

{ TUniformVariable }

procedure TUniformVariable.Init(AProgram: TBGLShader3D; AVariable: DWord);
begin
  FProgram := AProgram;
  FVariable := AVariable;
end;

{ TBGLShader3D }

function TBGLShader3D.GetUniformVariableSingle(AName: string): TUniformVariableSingle;
begin
  {$push}{$hints off}
  fillchar(result,sizeof(result),0);
  result.Init(self, FCanvas.Lighting.GetUniformVariable(FProgram, AName));
  {$pop}
end;

function TBGLShader3D.GetUniformVariablePointF(AName: string): TUniformVariablePointF;
begin
  {$push}{$hints off}
  fillchar(result,sizeof(result),0);
  result.Init(self, FCanvas.Lighting.GetUniformVariable(FProgram, AName));
  {$pop}
end;

function TBGLShader3D.GetUniformVariablePoint3D(AName: string): TUniformVariablePoint3D;
begin
  {$push}{$hints off}
  fillchar(result,sizeof(result),0);
  result.Init(self, FCanvas.Lighting.GetUniformVariable(FProgram, AName));
  {$pop}
end;

function TBGLShader3D.GetUniformVariableInteger(AName: string): TUniformVariableInteger;
begin
  {$push}{$hints off}
  fillchar(result,sizeof(result),0);
  result.Init(self, FCanvas.Lighting.GetUniformVariable(FProgram, AName));
  {$pop}
end;

function TBGLShader3D.GetUniformVariablePoint(AName: string): TUniformVariablePoint;
begin
  {$push}{$hints off}
  fillchar(result,sizeof(result),0);
  result.Init(self, FCanvas.Lighting.GetUniformVariable(FProgram, AName));
  {$pop}
end;

procedure TBGLShader3D.CheckUsage(AUsing: boolean);
begin
  if AUsing <> FUsed then
  begin
     if FUsed then raise exception.Create('Shader is in use') else
       raise exception.Create('Shader is not in use');
  end;
end;

function TBGLShader3D.GetAttributeVariableSingle(AName: string): TAttributeVariableSingle;
begin
  {$push}{$hints off}
  fillchar(result,sizeof(result),0);
  result.Init(self, FCanvas.Lighting.GetAttribVariable(FProgram, AName));
  {$pop}
end;

function TBGLShader3D.GetAttributeVariablePointF(AName: string): TAttributeVariablePointF;
begin
  {$push}{$hints off}
  fillchar(result,sizeof(result),0);
  result.Init(self, FCanvas.Lighting.GetAttribVariable(FProgram, AName));
  {$pop}
end;

function TBGLShader3D.GetAttributeVariablePoint3D(AName: string): TAttributeVariablePoint3D;
begin
  {$push}{$hints off}
  fillchar(result,sizeof(result),0);
  result.Init(self, FCanvas.Lighting.GetAttribVariable(FProgram, AName));
  {$pop}
end;

function TBGLShader3D.GetAttributeVariableInteger(AName: string): TAttributeVariableInteger;
begin
  {$push}{$hints off}
  fillchar(result,sizeof(result),0);
  result.Init(self, FCanvas.Lighting.GetAttribVariable(FProgram, AName));
  {$pop}
end;

function TBGLShader3D.GetAttributeVariablePoint(AName: string): TAttributeVariablePoint;
begin
  {$push}{$hints off}
  fillchar(result,sizeof(result),0);
  result.Init(self, FCanvas.Lighting.GetAttribVariable(FProgram, AName));
  {$pop}
end;

procedure TBGLShader3D.SetUniformSingle(AVariable: DWord; const AValue; ACount: integer);
begin
  CheckUsage(True);
  FCanvas.Lighting.SetUniformSingle(AVariable, AValue, ACount);
end;

procedure TBGLShader3D.SetUniformInteger(AVariable: DWord; const AValue; ACount: integer);
begin
  CheckUsage(True);
  FCanvas.Lighting.SetUniformInteger(AVariable, AValue, ACount);
end;

constructor TBGLShader3D.Create(ACanvas: TBGLCustomCanvas;
  AVertexShaderSource: string; AFragmentShaderSource: string;
  AVaryingVariables: string; AVersion: string);
begin
  FCanvas := ACanvas;
  FLighting := FCanvas.Lighting;
  FVertexShaderSource:= '#define version ' + AVersion + #10 + AVaryingVariables + #10 + AVertexShaderSource;
  FFragmentShaderSource:= '#define version ' + AVersion + #10 + AVaryingVariables + #10 + AFragmentShaderSource;
  FVertexShader := 0;
  FFragmentShader := 0;
  FProgram := 0;
  try
    FVertexShader := FLighting.MakeVertexShader(FVertexShaderSource);
    FFragmentShader := FLighting.MakeFragmentShader(FFragmentShaderSource);
    FProgram := FLighting.MakeShaderProgram(FVertexShader,FFragmentShader);
  except on ex:Exception do
    begin
      FLighting.DeleteShaderProgram(FProgram);
      FLighting.DeleteShaderObject(FFragmentShader);
      FLighting.DeleteShaderObject(FVertexShader);
      raise ex;
    end;
  end;
end;

destructor TBGLShader3D.Destroy;
begin
  if IsUsed then raise exception.Create('Shader is still in use');
  inherited Destroy;
end;

procedure TBGLShader3D.StartUse;
begin
  CheckUsage(False);
  FLighting.UseProgram(FProgram);
  FUsed:= True;
end;

procedure TBGLShader3D.EndUse;
begin
  CheckUsage(True);
  FLighting.UseProgram(0);
  FUsed:= False;
end;

{ TBGLLighting3D }

procedure TBGLLighting3D.SetUseBuiltIn(AValue: boolean);
begin
  if FUseBuiltIn=AValue then Exit;
  FUseBuiltIn:=AValue;
  FCanvas.Lighting.BuiltInLightingEnabled := FUseBuiltIn;
end;

procedure TBGLLighting3D.Init;
var
  i: Integer;
  v: TPoint3D;
  int: single;
  num: string;
  minInt: string;
  colorMult: TColorF;
begin
  FShaderLightingCode:=
        'void main(void) ' +
        '{ ' +
        '  vec3 L, H; float d; float sat, sumUnsat; vec4 color, clampedColor; vec4 unsat; ' +
        '  vec3 Idiff = vec3(gl_LightModel.ambient); ' +
        '  vec4 Ispec = vec4(0,0,0,0); ' +
        '  vec3 NN = normalize(N); ';
  with FCanvas.Lighting do
  begin
    AmbiantLightF := FAmbiantLight;
    for i := 0 to FLights.Count-1 do
    with TBGRALight3D(FLights[i]) do
    begin
      str(GetMinIntensity,minInt);
      if IsDirectional then
      begin
        v := -GetDirection;
        v.z := -v.z;
        num := IntToStr(AddDirectionalLight(GetColorF, v));
        str(GetMinIntensity,minInt);
        FShaderLightingCode +=
        '  L = gl_LightSource['+num+'].position.xyz; ' +
        '  Idiff += vec3(gl_LightSource['+num+'].diffuse * max(dot(NN,L), '+minInt+') ); ' +
        '  if (gl_FrontMaterial.shininess > 0) { ' +
        '    H = normalize(L + vec3(0,0,1)); ' +
        '    Ispec += gl_LightSource['+num+'].specular * pow(abs(dot(NN,H)), gl_FrontMaterial.shininess*2); ' +
        '  } ';
      end
      else
      begin
        int := GetIntensity*0.75;
        if int > 0 then
        begin
          v := GetPosition;
          v.z := -v.z;
          colorMult := GetColorF * ColorF(int,int,int,1);
          num := IntToStr(AddPointLight(colorMult, v, 0,1));
          str(GetMinIntensity/int,minInt);
          FShaderLightingCode +=
        '  L = (gl_LightSource['+num+'].position.xyz - V).xyz; ' +
        '  d = length(L); ' +
        '  L *= 1/d; ' +
        '  Idiff += vec3(gl_LightSource['+num+'].diffuse * max(dot(NN,L)/(d*d), '+minInt+') ); ' +
        '  if (gl_FrontMaterial.shininess > 0) { ' +
        '    H = normalize(L + vec3(0,0,1)); ' +
        '    Ispec += gl_LightSource['+num+'].specular  * pow(abs(dot(NN,H))/(d*d), gl_FrontMaterial.shininess*2); ' +
        '  } ';
        end;
      end;

    end;
  end;
  FShaderLightingCode +=
        '  color = #color# * vec4(Idiff,1) + Ispec; ' +
        '  clampedColor = clamp(color,0,1); ' +
        '  sat = dot( color - clampedColor, vec4(1) ); ' +
        '  if (sat > 0) { ' +
        '    unsat = vec4(1) - clampedColor; ' +
        '    sumUnsat = unsat[0]+unsat[1]+unsat[2]; ' +
        '    if (sumUnsat > 0) { ' +
        '      sat *= max(max(unsat[0],unsat[1]),unsat[2]) / sumUnsat; ' +
        '      gl_FragColor = clamp(color + vec4(sat,sat,sat,0),0,1); ' +
        '    } ' +
        '    else gl_FragColor = clampedColor; ' +
        '  } ' +
        '  else gl_FragColor = clampedColor; ' +
        '} ';
end;

constructor TBGLLighting3D.Create(ACanvas: TBGLCustomCanvas; AAmbiantLight: TColorF; ALights: TList);
begin
  FCanvas := ACanvas;
  FLights := ALights;
  FAmbiantLight := AAmbiantLight;
  Init;
end;

procedure TBGLLighting3D.SetSpecularIndex(AIndex: Integer);
begin
  FCanvas.Lighting.SetSpecularIndex(AIndex);
end;

destructor TBGLLighting3D.Destroy;
begin
  FCanvas.Lighting.SetSpecularIndex(0);
  FCanvas.Lighting.ClearLights;
  UseOpenGLBuiltInLighting := false;
  inherited Destroy;
end;

{ TBGLScene3D }

function TBGLScene3D.LoadBitmapFromFileUTF8(AFilenameUTF8: string
  ): TBGRACustomBitmap;
begin
  if BGLBitmapFactory <> nil then
    Result:= BGLBitmapFactory.Create(AFilenameUTF8,True)
  else
    result := inherited LoadBitmapFromFileUTF8(AFilenameUTF8);
end;

procedure TBGLScene3D.RenderGL(ACanvas: TBGLCustomCanvas; AMaxZ: single);
var
  renderer: TBGLRenderer3D;
begin
  renderer := TBGLRenderer3D.Create(ACanvas, self, AMaxZ);
  Render(renderer);
  renderer.Free;
end;

{ TBGLRenderer3D }

function TBGLRenderer3D.GetHasZBuffer: boolean;
begin
  result := FHasZBuffer;
end;

function TBGLRenderer3D.GetGlobalScale: single;
begin
  result := FGlobalScale;
end;

function TBGLRenderer3D.GetSurfaceWidth: integer;
begin
  result := FCanvas.Width;
end;

function TBGLRenderer3D.GetSurfaceHeight: integer;
begin
  result := FCanvas.Height;
end;

procedure TBGLRenderer3D.SetProjection(const AValue: TProjection3D);
begin
  inherited SetProjection(AValue);
  FProjectionMatrix := ProjectionToOpenGL(AValue, FOptions.MinZ, FFar) *
    OrthoProjectionToOpenGL(0,0,FCanvas.Width,FCanvas.Height);
  FCanvas.ProjectionMatrix := FProjectionMatrix;
end;

function TBGLRenderer3D.GetHandlesNearClipping: boolean;
begin
  result := true;
end;

function TBGLRenderer3D.GetHandlesFaceCulling: boolean;
begin
  result := FShader <> nil;
end;

procedure TBGLRenderer3D.InitLighting(AUseOpenGLBuiltInLighting: boolean);
var
  fragmentShaderCode: string;
begin
  if not Assigned(FLightingGL) then
  begin
    FLightingGL := TBGLLighting3D.Create(FCanvas, FAmbiantLight, FLights);

    if (FOptions.LightingInterpolation <> liLowQuality) and FCanvas.Lighting.SupportShaders then
    begin
      fragmentShaderCode := StringReplace(FLightingGL.ShaderLightingCode, '#color#', 'gl_Color', []);
      FShader := FCanvas.Lighting.Shader[fragmentShaderCode];
      if (FShader = nil) and FCanvas.Lighting.SupportShaders then
      begin
        FShader := TBGLShader3D.Create(FCanvas,
          'void main(void) ' +
          '{ ' +
          '  gl_Position = gl_ProjectionMatrix * gl_Vertex; ' +
          '  gl_FrontColor = gl_Color; ' +
          '  gl_BackColor = gl_Color; ' +
          '  N = gl_Normal; ' +
          '  V = vec3(gl_Vertex); ' +
          '} ',

          fragmentShaderCode,

          'varying vec3 N; ' +
          'varying vec3 V; ');
        FCanvas.Lighting.Shader[fragmentShaderCode] := FShader;
      end;

      fragmentShaderCode := StringReplace(FLightingGL.ShaderLightingCode, '#color#', TShaderWithTexture.GetCodeForTextureColor, []);
      FShaderWithTexture := FCanvas.Lighting.Shader[fragmentShaderCode];
      if (FShaderWithTexture = nil) and FCanvas.Lighting.SupportShaders then
      begin
        FShaderWithTexture := TShaderWithTexture.Create(FCanvas, fragmentShaderCode, 0);
        FCanvas.Lighting.Shader[fragmentShaderCode] := FShaderWithTexture;
      end;
    end else
    begin
      FLightingGL.UseOpenGLBuiltInLighting := AUseOpenGLBuiltInLighting;
      if not AUseOpenGLBuiltInLighting then
        FBGRAShader := TBGRAShader3D.Create(FAmbiantLight, FLights);
    end;
  end;
end;

constructor TBGLRenderer3D.Create(ACanvas: TBGLCustomCanvas;
            AScene: TBGRAScene3D; AFar: single);
begin
  FCanvas := ACanvas;
  FOptions := AScene.RenderingOptions;
  FLights := AScene.MakeLightList;
  FAmbiantLight := AScene.AmbiantLightColorF;
  FGlobalScale:= 1;
  FHasZBuffer := FOptions.PerspectiveMode = pmZBuffer;
  FFactorZ := -2/(FFar-FOptions.MinZ);
  FAddZ := -1 - FFar*FFactorZ;
  FFar := AFar;
  if FHasZBuffer then ACanvas.StartZBuffer;
  FOldCulling:= FCanvas.FaceCulling;
  FOldMatrix := FCanvas.Matrix;
  FCanvas.ResetTransform;
  FOldProjection := FCanvas.ProjectionMatrix;
  FCanvas.ProjectionMatrix := MatrixIdentity4D;

  FShader := nil;
  FShaderWithTexture := nil;

  InitLighting(False);
end;

function TBGLRenderer3D.RenderFace(var ADescription: TFaceRenderingDescription;
  AComputeCoordinate: TComputeProjectionFunc): boolean;
var
  NormalCenter3D,PtCenter3D: TPoint3D_128;
  ColorCenter: TBGRAPixel;

  procedure ComputeCenter;
  var j: NativeInt;
  begin
    with ADescription do
    begin
      PtCenter3D := Point3D_128_Zero;
      NormalCenter3D := Point3D_128_Zero;
      for j := 0 to NbVertices-1 do
      begin
        PtCenter3D += Positions3D[j];
        NormalCenter3D += Normals3D[j];
      end;
      PtCenter3D *= (1/NbVertices);
      Normalize3D_128(NormalCenter3D);
      ColorCenter := MergeBGRA(slice(Colors,NbVertices));
    end;
  end;

var tex: IBGLTexture;
  i,j: NativeInt;
begin
  result := true;

  if not ProjectionDefined then
    raise exception.Create('Projection must be defined before drawing faces');

  If ADescription.Texture <> nil then
    tex := ADescription.Texture.GetTextureGL as IBGLTexture
  else
    tex := nil;

  with ADescription do
  begin
    if ADescription.Biface then
      FCanvas.FaceCulling := fcNone
    else
      FCanvas.FaceCulling := fcKeepCW;

    if ADescription.Material.GetSpecularOn then
      FLightingGL.SetSpecularIndex(ADescription.Material.GetSpecularIndex)
    else
      FLightingGL.SetSpecularIndex(0);

    if tex <> nil then
    begin
      FCanvas.Lighting.ActiveShader := FShaderWithTexture;

      if Assigned(FBGRAShader) then
      begin
        FBGRAShader.Prepare(ADescription);

        if length(FShadedColorsF) < NbVertices then
          setlength(FShadedColorsF, NbVertices);
        for i := 0 to NbVertices-1 do
          FShadedColorsF[i] := BGRAToColorF(ColorIntToBGRA(FBGRAShader.Int65536Apply(Positions3D[i],Normals3D[i],BGRAWhite), true), false);

        for i := 0 to NbVertices-1 do
          Positions3D[i].z := -Positions3D[i].z;

        if NbVertices = 3 then
          tex.DrawTriangle(slice(Positions3D,3),slice(TexCoords,3),slice(FShadedColorsF,3))
        else if NbVertices = 4 then
          tex.DrawQuad(slice(Positions3D,4),slice(TexCoords,4),slice(FShadedColorsF,4));
      end else
      begin
        for i := 0 to NbVertices-1 do
        begin
          Positions3D[i].z := -Positions3D[i].z;
          Normals3D[i].z := -Normals3D[i].z;
        end;

        if NbVertices = 3 then
          tex.DrawTriangle(slice(Positions3D,3),slice(Normals3D,3),slice(TexCoords,3))
        else if NbVertices = 4 then
          tex.DrawQuad(slice(Positions3D,4),slice(Normals3D,4),slice(TexCoords,4));
      end;
    end
    else
    begin
      FCanvas.Lighting.ActiveShader := FShader;

      if Assigned(FBGRAShader) then
      begin
        FBGRAShader.Prepare(ADescription);

        if length(FShadedColors) < NbVertices then
          setlength(FShadedColors, NbVertices);
        for i := 0 to NbVertices-1 do
          FShadedColors[i] := FBGRAShader.Apply(Positions3D[i],Normals3D[i],Colors[i]);

        if NbVertices > 4 then
        begin
          ComputeCenter;
          ColorCenter := FBGRAShader.Apply(PtCenter3D,NormalCenter3D,MergeBGRA(slice(Colors,NbVertices)));

          for i := 0 to NbVertices-1 do
            Positions3D[i].z := -Positions3D[i].z;
          PtCenter3D.z := -PtCenter3D.z;

          i := NbVertices-1;
          for j := 0 to NbVertices-1 do
          begin
            FCanvas.FillTrianglesLinearColor(
               [Positions3D[i],Positions3D[j],PtCenter3D],
               [FShadedColors[i],FShadedColors[j],ColorCenter]);
            i := j;
          end;
        end else
        begin
          for i := 0 to NbVertices-1 do
            Positions3D[i].z := -Positions3D[i].z;

          if NbVertices = 3 then
            FCanvas.FillTrianglesLinearColor(slice(Positions3D,3),slice(FShadedColors,3))
          else if NbVertices = 4 then
            FCanvas.FillQuadsLinearColor(slice(Positions3D,4),slice(FShadedColors,4));
        end;
      end else
      begin
        for i := 0 to NbVertices-1 do
        begin
          Positions3D[i].z := -Positions3D[i].z;
          Normals3D[i].z := -Normals3D[i].z;
        end;

        if NbVertices > 4 then
        begin
          ComputeCenter;

          i := NbVertices-1;
          for j := 0 to NbVertices-1 do
          begin
            FCanvas.FillTrianglesLinearColor(
               [Positions3D[i],Positions3D[j],PtCenter3D],
               [Normals3D[i],Normals3D[j],NormalCenter3D],
               [Colors[i],Colors[j],ColorCenter]);
            i := j;
          end;
        end else
        begin
          if NbVertices = 3 then
            FCanvas.FillTrianglesLinearColor(slice(Positions3D,3),slice(Normals3D,3),slice(Colors,3))
          else if NbVertices = 4 then
            FCanvas.FillQuadsLinearColor(slice(Positions3D,4),slice(Normals3D,4),slice(Colors,4));
        end;
      end;
    end;
  end;
end;

destructor TBGLRenderer3D.Destroy;
begin
  FreeAndNil(FBGRAShader);
  FCanvas.Lighting.ActiveShader := nil;
  FCanvas.ProjectionMatrix := FOldProjection;
  FCanvas.Matrix := FOldMatrix;
  FCanvas.FaceCulling := FOldCulling;
  FreeAndNil(FLightingGL);
  if FHasZBuffer then FCanvas.EndZBuffer;
  FLights.Free;
  inherited Destroy;
end;


end.
