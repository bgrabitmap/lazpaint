unit uvectorialfill;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRATransform, BGRAGradientOriginal, BGRABitmap, BGRABitmapTypes;

type

  { TVectorialFill }

  TVectorialFill = class
  protected
    FColor: TBGRAPixel;
    FIsSolid: boolean;
    FTexture: TBGRABitmap;
    FTextureMatrix: TAffineMatrix;
    FTextureOpacity: byte;
    FGradient: TBGRALayerGradientOriginal;
    FOnChange: TNotifyEvent;
    procedure GradientChange({%H-}ASender: TObject; {%H-}ABounds: PRectF=nil);
    procedure Init; virtual;
    function GetIsGradient: boolean;
    function GetIsTexture: boolean;
    function GetIsNone: boolean;
    procedure SetOnChange(AValue: TNotifyEvent);
    procedure SetSolid(AValue: TBGRAPixel);
    procedure SetTextureMatrix(AValue: TAffineMatrix);
    procedure SetTextureOpacity(AValue: byte);
    procedure InternalClear;
    procedure Changed;
  public
    constructor Create;
    procedure Clear;
    constructor CreateAsSolid(AColor: TBGRAPixel);
    constructor CreateAsTexture(ATexture: TBGRABitmap; AMatrix: TAffineMatrix; AOpacity: byte = 255);
    constructor CreateAsGradient(AGradient: TBGRALayerGradientOriginal; AOwned: boolean);
    constructor SetSolid(AColor: TBGRAPixel);
    constructor SetTexture(ATexture: TBGRABitmap; AMatrix: TAffineMatrix; AOpacity: byte = 255);
    constructor SetGradient(AGradient: TBGRALayerGradientOriginal; AOwned: boolean);
    function CreateScanner(AMatrix: TAffineMatrix; ADraft: boolean): TBGRACustomScanner;
    function IsSlow(AMatrix: TAffineMatrix): boolean;
    function Duplicate: TVectorialFill; virtual;
    destructor Destroy; override;
    function Equals(Obj: TObject): boolean; override;
    procedure Assign(Obj: TObject);
    property IsNone: boolean read GetIsNone;
    property IsSolid: boolean read FIsSolid;
    property IsTexture: boolean read GetIsTexture;
    property IsGradient: boolean read GetIsGradient;
    property Gradient: TBGRALayerGradientOriginal read FGradient;
    property SolidColor: TBGRAPixel read FColor write SetSolid;
    property Texture: TBGRABitmap read FTexture;
    property TextureMatrix: TAffineMatrix read FTextureMatrix write SetTextureMatrix;
    property TextureOpacity: byte read FTextureOpacity write SetTextureOpacity;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;

implementation

uses BGRAGradientScanner;

{ TVectorialFill }

function TVectorialFill.GetIsGradient: boolean;
begin
  result:= Assigned(FGradient);
end;

function TVectorialFill.GetIsTexture: boolean;
begin
  result:= Assigned(FTexture);
end;

procedure TVectorialFill.SetOnChange(AValue: TNotifyEvent);
begin
  if FOnChange=AValue then Exit;
  FOnChange:=AValue;
end;

procedure TVectorialFill.SetSolid(AValue: TBGRAPixel);
begin
  if FColor=AValue then Exit;
  FColor:=AValue;
  FIsSolid:= true;
end;

procedure TVectorialFill.SetTextureMatrix(AValue: TAffineMatrix);
begin
  if not IsTexture then raise exception.Create('Not a texture fill');
  if FTextureMatrix=AValue then Exit;
  FTextureMatrix:=AValue;
  Changed;
end;

procedure TVectorialFill.SetTextureOpacity(AValue: byte);
begin
  if not IsTexture then raise exception.Create('Not a texture fill');
  if FTextureOpacity=AValue then Exit;
  FTextureOpacity:=AValue;
  Changed;
end;

procedure TVectorialFill.InternalClear;
begin
  if Assigned(FTexture) then
  begin
    FTexture.FreeReference;
    FTexture := nil;
  end;
  if Assigned(FGradient) then
  begin
    FGradient.OnChange := nil;
    FreeAndNil(FGradient);
  end;
  FIsSolid := false;
  FColor := BGRAPixelTransparent;
  FTextureMatrix := AffineMatrixIdentity;
end;

procedure TVectorialFill.Changed;
begin
  if Assigned(OnChange) then OnChange(self);
end;

procedure TVectorialFill.Init;
begin
  FColor := BGRAPixelTransparent;
  FTexture := nil;
  FTextureMatrix := AffineMatrixIdentity;
  FTextureOpacity:= 255;
  FGradient := nil;
  FIsSolid := false;
end;

function TVectorialFill.GetIsNone: boolean;
begin
  result:= not IsSolid and not IsGradient and not IsTexture;
end;

procedure TVectorialFill.GradientChange(ASender: TObject; ABounds: PRectF=nil);
begin
  Changed;
end;

constructor TVectorialFill.Create;
begin
  Init;
end;

procedure TVectorialFill.Clear;
var
  notify: Boolean;
begin
  notify := IsSolid or IsGradient or IsTexture;
  InternalClear;
  if notify then Changed;
end;

constructor TVectorialFill.CreateAsSolid(AColor: TBGRAPixel);
begin
  Init;
  SetSolid(AColor);
end;

constructor TVectorialFill.CreateAsTexture(ATexture: TBGRABitmap;
  AMatrix: TAffineMatrix; AOpacity: byte);
begin
  Init;
  SetTexture(ATexture,AMatrix,AOpacity);
end;

constructor TVectorialFill.CreateAsGradient(
  AGradient: TBGRALayerGradientOriginal; AOwned: boolean);
begin
  Init;
  SetGradient(AGradient,AOwned);
end;

constructor TVectorialFill.SetSolid(AColor: TBGRAPixel);
begin
  InternalClear;
  if AColor.alpha = 0 then AColor := BGRAPixelTransparent;
  FColor := AColor;
  FIsSolid:= true;
  Changed;
end;

constructor TVectorialFill.SetTexture(ATexture: TBGRABitmap;
  AMatrix: TAffineMatrix; AOpacity: byte);
begin
  InternalClear;
  FTexture := ATexture.NewReference as TBGRABitmap;
  FTextureMatrix := AMatrix;
  FTextureOpacity:= AOpacity;
  Changed;
end;

constructor TVectorialFill.SetGradient(AGradient: TBGRALayerGradientOriginal;
  AOwned: boolean);
begin
  InternalClear;
  if AOwned then FGradient := AGradient
  else FGradient := AGradient.Duplicate as TBGRALayerGradientOriginal;
  FGradient.OnChange:=@GradientChange;
  Changed;
end;

function TVectorialFill.CreateScanner(AMatrix: TAffineMatrix; ADraft: boolean
  ): TBGRACustomScanner;
var
  bmpTransf: TBGRAAffineBitmapTransform;
  filter: TResampleFilter;
  m: TAffineMatrix;
begin
  if Assigned(FTexture) then
  begin
    m := AMatrix*FTextureMatrix;
    if ADraft or TBGRABitmap.IsAffineRoughlyTranslation(m, rect(0,0,FTexture.Width,FTexture.Height)) then filter := rfBox
    else filter := rfHalfCosine;
    bmpTransf := TBGRAAffineBitmapTransform.Create(FTexture,true,filter);
    bmpTransf.ViewMatrix := m;
    if FTextureOpacity <> 255 then
      result:= TBGRAOpacityScanner.Create(bmpTransf, FTextureOpacity, true)
    else
      result := bmpTransf;
  end else
  if Assigned(FGradient) then
    result := FGradient.CreateScanner(AMatrix)
  else if FIsSolid then
    result := TBGRAConstantScanner.Create(FColor)
  else
    result := nil;
end;

function TVectorialFill.IsSlow(AMatrix: TAffineMatrix): boolean;
var
  m: TAffineMatrix;
begin
  if Assigned(FTexture) then
  begin
    m := AMatrix*FTextureMatrix;
    result := not TBGRABitmap.IsAffineRoughlyTranslation(m, rect(0,0,FTexture.Width,FTexture.Height));
  end else
    result := false;
end;

function TVectorialFill.Duplicate: TVectorialFill;
begin
  result := TVectorialFill.Create;
  result.Assign(self);
end;

destructor TVectorialFill.Destroy;
begin
  InternalClear;
  inherited Destroy;
end;

function TVectorialFill.Equals(Obj: TObject): boolean;
var
  other: TVectorialFill;
begin
  if inherited Equals(Obj) then
    result := true
  else
  if Obj = nil then
    result := IsNone
  else
  if Obj is TVectorialFill then
  begin
    other := TVectorialFill(Obj);
    if Self = nil then
      result := other.IsNone
    else
    begin
      if other.IsSolid then result := IsSolid and (other.SolidColor = SolidColor) else
      if other.IsGradient then result := IsGradient and (other.Gradient.Equals(Gradient)) else
      if other.IsTexture then result := IsTexture and (other.Texture = Texture) and
                       (other.TextureMatrix = TextureMatrix) and (other.TextureOpacity = TextureOpacity) else
        result := IsNone;
    end;
  end else
    result:= false;
end;

procedure TVectorialFill.Assign(Obj: TObject);
var
  other: TVectorialFill;
begin
  if Obj = nil then Clear else
  if Obj is TVectorialFill then
  begin
    other := TVectorialFill(Obj);
    if other.IsSolid then SetSolid(other.SolidColor) else
    if other.IsGradient then SetGradient(other.Gradient,false) else
    if other.IsTexture then SetTexture(other.Texture,other.TextureMatrix,other.TextureOpacity) else
      Clear;
  end else
    raise exception.Create('Incompatible type');
end;

end.

