unit uvectorialfill;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRATransform, BGRAGradientOriginal, BGRABitmap, BGRABitmapTypes;

type

  { TVectorialFill }

  TVectorialFill = class
  private
    function GetIsGradient: boolean;
    function GetIsTexture: boolean;
    procedure SetSolid(AValue: TBGRAPixel);
    procedure SetTextureMatrix(AValue: TAffineMatrix);
    procedure SetTextureOpacity(AValue: byte);
  protected
    FColor: TBGRAPixel;
    FIsSolid: boolean;
    FTexture: TBGRABitmap;
    FTextureMatrix: TAffineMatrix;
    FTextureOpacity: byte;
    FGradient: TBGRALayerGradientOriginal;
    procedure Init; virtual;
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
    property IsSolid: boolean read FIsSolid;
    property IsTexture: boolean read GetIsTexture;
    property IsGradient: boolean read GetIsGradient;
    property Gradient: TBGRALayerGradientOriginal read FGradient;
    property SolidColor: TBGRAPixel read FColor write SetSolid;
    property Texture: TBGRABitmap read FTexture;
    property TextureMatrix: TAffineMatrix read FTextureMatrix write SetTextureMatrix;
    property TextureOpacity: byte read FTextureOpacity write SetTextureOpacity;
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
end;

procedure TVectorialFill.SetTextureOpacity(AValue: byte);
begin
  if not IsTexture then raise exception.Create('Not a texture fill');
  if FTextureOpacity=AValue then Exit;
  FTextureOpacity:=AValue;
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

constructor TVectorialFill.Create;
begin
  Init;
end;

procedure TVectorialFill.Clear;
begin
  if Assigned(FTexture) then
  begin
    FTexture.FreeReference;
    FTexture := nil;
  end;
  FreeAndNil(FGradient);
  FIsSolid := false;
  FColor := BGRAPixelTransparent;
  FTextureMatrix := AffineMatrixIdentity;
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
  Clear;
  if AColor.alpha = 0 then AColor := BGRAPixelTransparent;
  FColor := AColor;
  FIsSolid:= true;
end;

constructor TVectorialFill.SetTexture(ATexture: TBGRABitmap;
  AMatrix: TAffineMatrix; AOpacity: byte);
begin
  Clear;
  FTexture := ATexture.NewReference as TBGRABitmap;
  FTextureMatrix := AMatrix;
  FTextureOpacity:= AOpacity;
end;

constructor TVectorialFill.SetGradient(AGradient: TBGRALayerGradientOriginal;
  AOwned: boolean);
begin
  Clear;
  if AOwned then FGradient := AGradient
  else FGradient := AGradient.Duplicate as TBGRALayerGradientOriginal;
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
  if Assigned(FTexture) then
    result := TVectorialFill.CreateAsTexture(FTexture,FTextureMatrix,FTextureOpacity)
  else if Assigned(FGradient) then
    result := TVectorialFill.CreateAsGradient(FGradient,False)
  else if FIsSolid then
    result := TVectorialFill.CreateAsSolid(FColor)
  else
    result := TVectorialFill.Create;
end;

destructor TVectorialFill.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TVectorialFill.Equals(Obj: TObject): boolean;
begin
  Result:=inherited Equals(Obj);
end;

end.

