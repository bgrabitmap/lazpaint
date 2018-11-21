unit uvectorialfill;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRATransform, BGRAGradientOriginal, BGRABitmap, BGRABitmapTypes,
  BGRALayerOriginal;

type
  TTextureRepetition = (trNone, trRepeatX, trRepeatY, trRepeatBoth);
  TVectorialFillType = (vftNone, vftSolid, vftGradient, vftTexture);

  { TVectorialFill }

  TVectorialFill = class
  protected
    FColor: TBGRAPixel;
    FIsSolid: boolean;
    FTexture: TBGRABitmap;
    FTextureMatrix: TAffineMatrix;
    FTextureMatrixBackup: TAffineMatrix;
    FTextureOpacity: byte;
    FTextureRepetition: TTextureRepetition;
    FGradient: TBGRALayerGradientOriginal;
    FOnChange: TNotifyEvent;
    procedure GradientChange({%H-}ASender: TObject; {%H-}ABounds: PRectF=nil);
    procedure Init; virtual;
    function GetFillType: TVectorialFillType;
    function GetIsEditable: boolean;
    procedure SetOnChange(AValue: TNotifyEvent);
    procedure SetTextureMatrix(AValue: TAffineMatrix);
    procedure SetTextureOpacity(AValue: byte);
    procedure SetTextureRepetition(AValue: TTextureRepetition);
    procedure InternalClear;
    procedure Changed;
    procedure ConfigureTextureEditor(AEditor: TBGRAOriginalEditor);
    procedure TextureMoveOrigin({%H-}ASender: TObject; {%H-}APrevCoord,
      ANewCoord: TPointF; {%H-}AShift: TShiftState);
    procedure TextureMoveXAxis({%H-}ASender: TObject; {%H-}APrevCoord,
      ANewCoord: TPointF; AShift: TShiftState);
    procedure TextureMoveYAxis({%H-}ASender: TObject; {%H-}APrevCoord,
      ANewCoord: TPointF; AShift: TShiftState);
    procedure TextureStartMove(ASender: TObject; AIndex: integer;
      AShift: TShiftState);
  public
    constructor Create;
    procedure Clear;
    constructor CreateAsSolid(AColor: TBGRAPixel);
    constructor CreateAsTexture(ATexture: TBGRABitmap; AMatrix: TAffineMatrix; AOpacity: byte = 255;
                                ATextureRepetition: TTextureRepetition = trRepeatBoth);
    constructor CreateAsGradient(AGradient: TBGRALayerGradientOriginal; AOwned: boolean);
    procedure SetSolid(AColor: TBGRAPixel);
    procedure SetTexture(ATexture: TBGRABitmap; AMatrix: TAffineMatrix; AOpacity: byte = 255;
                         ATextureRepetition: TTextureRepetition = trRepeatBoth);
    procedure SetGradient(AGradient: TBGRALayerGradientOriginal; AOwned: boolean);
    procedure ConfigureEditor(AEditor: TBGRAOriginalEditor);
    function CreateScanner(AMatrix: TAffineMatrix; ADraft: boolean): TBGRACustomScanner;
    function IsSlow(AMatrix: TAffineMatrix): boolean;
    function IsFullyTransparent: boolean;
    procedure Transform(AMatrix: TAffineMatrix);
    function Duplicate: TVectorialFill; virtual;
    destructor Destroy; override;
    function Equals(Obj: TObject): boolean; override;
    procedure Assign(Obj: TObject);
    property FillType: TVectorialFillType read GetFillType;
    property IsEditable: boolean read GetIsEditable;
    property Gradient: TBGRALayerGradientOriginal read FGradient;
    property SolidColor: TBGRAPixel read FColor write SetSolid;
    property Texture: TBGRABitmap read FTexture;
    property TextureMatrix: TAffineMatrix read FTextureMatrix write SetTextureMatrix;
    property TextureOpacity: byte read FTextureOpacity write SetTextureOpacity;
    property TextureRepetition: TTextureRepetition read FTextureRepetition write SetTextureRepetition;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;

implementation

uses BGRAGradientScanner;

{ TVectorialFill }

procedure TVectorialFill.SetOnChange(AValue: TNotifyEvent);
begin
  if FOnChange=AValue then Exit;
  FOnChange:=AValue;
end;

procedure TVectorialFill.SetTextureMatrix(AValue: TAffineMatrix);
begin
  if FillType <> vftTexture then raise exception.Create('Not a texture fill');
  if FTextureMatrix=AValue then Exit;
  FTextureMatrix:=AValue;
  Changed;
end;

procedure TVectorialFill.SetTextureOpacity(AValue: byte);
begin
  if FillType <> vftTexture then raise exception.Create('Not a texture fill');
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
  FTextureRepetition:= trRepeatBoth;
end;

procedure TVectorialFill.Changed;
begin
  if Assigned(OnChange) then OnChange(self);
end;

procedure TVectorialFill.ConfigureTextureEditor(AEditor: TBGRAOriginalEditor);
var
  origin, xAxisRel, yAxisRel: TPointF;
begin
  if Assigned(FTexture) then
  begin
    origin := PointF(FTextureMatrix[1,3],FTextureMatrix[2,3]);
    xAxisRel := PointF(FTextureMatrix[1,1],FTextureMatrix[2,1]);
    yAxisRel := PointF(FTextureMatrix[1,2],FTextureMatrix[2,2]);
    AEditor.AddPoint(origin, @TextureMoveOrigin, true);
    if FTexture.Width > 0 then
      AEditor.AddArrow(origin, origin+xAxisRel*FTexture.Width, @TextureMoveXAxis);
    if FTexture.Height > 0 then
      AEditor.AddArrow(origin, origin+yAxisRel*FTexture.Height, @TextureMoveYAxis);
    AEditor.AddStartMoveHandler(@TextureStartMove);
  end;
end;

procedure TVectorialFill.TextureMoveOrigin(ASender: TObject; APrevCoord,
  ANewCoord: TPointF; AShift: TShiftState);
begin
  FTextureMatrix[1,3] := ANewCoord.x;
  FTextureMatrix[2,3] := ANewCoord.y;
  Changed;
end;

procedure TVectorialFill.TextureMoveXAxis(ASender: TObject; APrevCoord,
  ANewCoord: TPointF; AShift: TShiftState);
var
  origin, xAxisRel: TPointF;
begin
  FTextureMatrix := FTextureMatrixBackup;
  origin := PointF(FTextureMatrix[1,3],FTextureMatrix[2,3]);
  xAxisRel := (ANewCoord - origin)*(1/FTexture.Width);
  if ssAlt in AShift then
  begin
    FTextureMatrix[1,1] := xAxisRel.x;
    FTextureMatrix[2,1] := xAxisRel.y;
  end
  else
    FTextureMatrix := AffineMatrixTranslation(origin.x,origin.y)*
                     AffineMatrixScaledRotation(PointF(FTextureMatrix[1,1],FTextureMatrix[2,1]), xAxisRel)*
                     AffineMatrixLinear(FTextureMatrix);
  Changed;
end;

procedure TVectorialFill.TextureMoveYAxis(ASender: TObject; APrevCoord,
  ANewCoord: TPointF; AShift: TShiftState);
var
  origin, yAxisRel: TPointF;
begin
  FTextureMatrix := FTextureMatrixBackup;
  origin := PointF(FTextureMatrix[1,3],FTextureMatrix[2,3]);
  yAxisRel := (ANewCoord - origin)*(1/FTexture.Height);
  if ssAlt in AShift then
  begin
    FTextureMatrix[1,2] := yAxisRel.x;
    FTextureMatrix[2,2] := yAxisRel.y;
  end
  else
    FTextureMatrix := AffineMatrixTranslation(origin.x,origin.y)*
                     AffineMatrixScaledRotation(PointF(FTextureMatrix[1,2],FTextureMatrix[2,2]), yAxisRel)*
                     AffineMatrixLinear(FTextureMatrix);
  Changed;
end;

procedure TVectorialFill.TextureStartMove(ASender: TObject; AIndex: integer;
  AShift: TShiftState);
begin
  FTextureMatrixBackup := FTextureMatrix;
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

function TVectorialFill.GetIsEditable: boolean;
begin
  result:= FillType in [vftGradient, vftTexture];
end;

procedure TVectorialFill.SetTextureRepetition(AValue: TTextureRepetition);
begin
  if FillType <> vftTexture then raise exception.Create('Not a texture fill');
  if FTextureRepetition=AValue then Exit;
  FTextureRepetition:=AValue;
  Changed;
end;

function TVectorialFill.GetFillType: TVectorialFillType;
begin
  if FIsSolid then result:= vftSolid
  else if Assigned(FGradient) then result := vftGradient
  else if Assigned(FTexture) then result := vftTexture
  else result := vftNone;
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
  notify := FillType <> vftNone;
  InternalClear;
  if notify then Changed;
end;

constructor TVectorialFill.CreateAsSolid(AColor: TBGRAPixel);
begin
  Init;
  SetSolid(AColor);
end;

constructor TVectorialFill.CreateAsTexture(ATexture: TBGRABitmap;
  AMatrix: TAffineMatrix; AOpacity: byte; ATextureRepetition: TTextureRepetition);
begin
  Init;
  SetTexture(ATexture,AMatrix,AOpacity,ATextureRepetition);
end;

constructor TVectorialFill.CreateAsGradient(
  AGradient: TBGRALayerGradientOriginal; AOwned: boolean);
begin
  Init;
  SetGradient(AGradient,AOwned);
end;

procedure TVectorialFill.SetSolid(AColor: TBGRAPixel);
begin
  if (FillType = vftSolid) and (SolidColor = AColor) then exit;
  InternalClear;
  if AColor.alpha = 0 then AColor := BGRAPixelTransparent;
  FColor := AColor;
  FIsSolid:= true;
  Changed;
end;

procedure TVectorialFill.SetTexture(ATexture: TBGRABitmap;
  AMatrix: TAffineMatrix; AOpacity: byte; ATextureRepetition: TTextureRepetition);
begin
  InternalClear;
  FTexture := ATexture.NewReference as TBGRABitmap;
  FTextureMatrix := AMatrix;
  FTextureOpacity:= AOpacity;
  FTextureRepetition:= ATextureRepetition;
  Changed;
end;

procedure TVectorialFill.SetGradient(AGradient: TBGRALayerGradientOriginal;
  AOwned: boolean);
begin
  InternalClear;
  if AOwned then FGradient := AGradient
  else FGradient := AGradient.Duplicate as TBGRALayerGradientOriginal;
  FGradient.OnChange:=@GradientChange;
  Changed;
end;

procedure TVectorialFill.ConfigureEditor(AEditor: TBGRAOriginalEditor);
begin
  case FillType of
  vftGradient: Gradient.ConfigureEditor(AEditor);
  vftTexture: ConfigureTextureEditor(AEditor);
  end;
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
    bmpTransf := TBGRAAffineBitmapTransform.Create(FTexture,
                    FTextureRepetition in[trRepeatX,trRepeatBoth],
                    FTextureRepetition in[trRepeatY,trRepeatBoth], filter);
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

function TVectorialFill.IsFullyTransparent: boolean;
begin
  case FillType of
  vftNone: result := true;
  vftSolid: result:= SolidColor.alpha = 0;
  else result:= false;
  end;
end;

procedure TVectorialFill.Transform(AMatrix: TAffineMatrix);
begin
  case FillType of
  vftGradient: Gradient.Transform(AMatrix);
  vftTexture:
    begin
      FTextureMatrix := AMatrix*FTextureMatrix;
      Changed;
    end;
  end;
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
    result := (FillType = vftNone)
  else
  if Obj is TVectorialFill then
  begin
    other := TVectorialFill(Obj);
    if Self = nil then
      result := (other.FillType = vftNone)
    else
    begin
      case other.FillType of
      vftSolid: result := (FillType = vftSolid) and (other.SolidColor = SolidColor);
      vftGradient: result := (FillType = vftGradient) and (other.Gradient.Equals(Gradient));
      vftTexture: result := (FillType = vftTexture) and (other.Texture = Texture) and
                       (other.TextureMatrix = TextureMatrix) and (other.TextureOpacity = TextureOpacity)
                       and (other.TextureRepetition = TextureRepetition);
      else
        result := FillType = vftNone;
      end;
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
    case other.FillType of
    vftSolid: SetSolid(other.SolidColor);
    vftGradient: SetGradient(other.Gradient,false);
    vftTexture: SetTexture(other.Texture,other.TextureMatrix,other.TextureOpacity,other.TextureRepetition);
    else Clear;
    end;
  end else
    raise exception.Create('Incompatible type');
end;

end.

