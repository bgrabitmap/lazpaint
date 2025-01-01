// SPDX-License-Identifier: GPL-3.0-only
unit LCVectorialFill;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRATransform, BGRAGradientOriginal, BGRABitmap, BGRABitmapTypes,
  BGRALayerOriginal;

type
  TTextureRepetition = (trNone, trRepeatX, trRepeatY, trRepeatBoth);
  TTransparentMode = (tmEnforeAllChannelsZero, tmAlphaZeroOnly, tmNoFill);
  TVectorialFillType = (vftNone, vftSolid, vftGradient, vftTexture);
  TVectorialFillTypes = set of TVectorialFillType;
  TVectorialFill = class;

  TCustomVectorialFillDiff = class
    procedure Apply(AFill: TVectorialFill); virtual; abstract;
    procedure Unapply(AFill: TVectorialFill); virtual; abstract;
    function IsIdentity: boolean; virtual; abstract;
    function CanAppend(ADiff: TCustomVectorialFillDiff): boolean; virtual; abstract;
    procedure Append(ADiff: TCustomVectorialFillDiff); virtual; abstract;
  end;

  TVectorialFillChangeEvent = procedure(ASender: TObject; var ADiff: TCustomVectorialFillDiff) of object;

  { TVectorialFillGradientDiff }

  TVectorialFillGradientDiff = class(TCustomVectorialFillDiff)
  protected
    FGradientDiff: TBGRAGradientOriginalDiff;
  public
    constructor Create(AGradientDiff: TBGRAGradientOriginalDiff);
    destructor Destroy; override;
    procedure Apply(AFill: TVectorialFill); override;
    procedure Unapply(AFill: TVectorialFill); override;
    function IsIdentity: boolean; override;
    function CanAppend(ADiff: TCustomVectorialFillDiff): boolean; override;
    procedure Append(ADiff: TCustomVectorialFillDiff); override;
  end;

  { TVectorialFillDiff }

  TVectorialFillDiff = class(TCustomVectorialFillDiff)
  protected
    FStart,FEnd: TVectorialFill;
    FTransparentMode: TTransparentMode;
  public
    constructor Create(AFrom: TVectorialFill);
    procedure ComputeDiff(ATo: TVectorialFill);
    destructor Destroy; override;
    procedure Apply(AFill: TVectorialFill); override;
    procedure Unapply(AFill: TVectorialFill); override;
    function IsIdentity: boolean; override;
    function CanAppend(ADiff: TCustomVectorialFillDiff): boolean; override;
    procedure Append(ADiff: TCustomVectorialFillDiff); override;
  end;

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
    FTextureAverageColor: TBGRAPixel;
    FTextureAverageColorComputed: boolean;
    FTransparentMode: TTransparentMode;
    FGradient: TBGRALayerGradientOriginal;
    FOnChange: TVectorialFillChangeEvent;
    FOnBeforeChange: TNotifyEvent;
    FDiff: TVectorialFillDiff;
    procedure GradientChange({%H-}ASender: TObject; {%H-}ABounds: PRectF; var ADiff: TBGRAOriginalDiff);
    procedure Init; virtual;
    function GetFillType: TVectorialFillType;
    function GetIsEditable: boolean;
    function GetAverageColor: TBGRAPixel;
    procedure SetOnChange(AValue: TVectorialFillChangeEvent);
    procedure SetTextureMatrix(AValue: TAffineMatrix);
    procedure SetTextureOpacity(AValue: byte);
    procedure SetTextureRepetition(AValue: TTextureRepetition);
    procedure SetTransparentMode(AValue: TTransparentMode);
    procedure InternalClear;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure NotifyChangeWithoutDiff;
    procedure ConfigureTextureEditor(AEditor: TBGRAOriginalEditor);
    procedure TextureMoveOrigin({%H-}ASender: TObject; {%H-}APrevCoord,
      ANewCoord: TPointF; {%H-}AShift: TShiftState);
    procedure TextureMoveXAxis({%H-}ASender: TObject; {%H-}APrevCoord,
      ANewCoord: TPointF; AShift: TShiftState);
    procedure TextureMoveYAxis({%H-}ASender: TObject; {%H-}APrevCoord,
      ANewCoord: TPointF; AShift: TShiftState);
    procedure TextureStartMove({%H-}ASender: TObject; {%H-}AIndex: integer;
      {%H-}AShift: TShiftState);
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
    class function Equal(AFill1, AFill2: TVectorialFill): boolean;
    procedure Assign(Obj: TObject);
    procedure AssignExceptGeometry(Obj: TObject);
    procedure FitGeometry(const ABox: TAffineBox);
    procedure ApplyOpacity(AOpacity: Byte);
    property FillType: TVectorialFillType read GetFillType;
    property IsEditable: boolean read GetIsEditable;
    property Gradient: TBGRALayerGradientOriginal read FGradient;
    property SolidColor: TBGRAPixel read FColor write SetSolid;
    property AverageColor: TBGRAPixel read GetAverageColor;
    property Texture: TBGRABitmap read FTexture;
    property TextureMatrix: TAffineMatrix read FTextureMatrix write SetTextureMatrix;
    property TextureOpacity: byte read FTextureOpacity write SetTextureOpacity;
    property TextureRepetition: TTextureRepetition read FTextureRepetition write SetTextureRepetition;
    property OnChange: TVectorialFillChangeEvent read FOnChange write SetOnChange;
    property OnBeforeChange: TNotifyEvent read FOnBeforeChange write FOnBeforeChange;
    property TransparentMode: TTransparentMode read FTransparentMode write SetTransparentMode;
  end;

implementation

uses BGRAGradientScanner, BGRABlend, LCResourceString;

{ TVectorialFillDiff }

constructor TVectorialFillDiff.Create(AFrom: TVectorialFill);
begin
  FStart := TVectorialFill.Create;
  FStart.TransparentMode:= AFrom.TransparentMode;
  FStart.Assign(AFrom);
end;

procedure TVectorialFillDiff.ComputeDiff(ATo: TVectorialFill);
begin
  FEnd := TVectorialFill.Create;
  FEnd.TransparentMode := ATo.TransparentMode;
  FEnd.Assign(ATo);
end;

destructor TVectorialFillDiff.Destroy;
begin
  FStart.Free;
  FEnd.Free;
  inherited Destroy;
end;

procedure TVectorialFillDiff.Apply(AFill: TVectorialFill);
var
  oldChange: TVectorialFillChangeEvent;
begin
  oldChange := AFill.OnChange;
  AFill.OnChange := nil;
  AFill.Assign(FEnd);
  AFill.OnChange := oldChange;
  AFill.NotifyChangeWithoutDiff;
end;

procedure TVectorialFillDiff.Unapply(AFill: TVectorialFill);
var
  oldChange: TVectorialFillChangeEvent;
begin
  oldChange := AFill.OnChange;
  AFill.OnChange := nil;
  AFill.Assign(FStart);
  AFill.OnChange := oldChange;
  AFill.NotifyChangeWithoutDiff;
end;

function TVectorialFillDiff.IsIdentity: boolean;
begin
  result := TVectorialFill.Equal(FStart,FEnd);
end;

function TVectorialFillDiff.CanAppend(ADiff: TCustomVectorialFillDiff
  ): boolean;
begin
  result := ADiff is TVectorialFillDiff;
end;

procedure TVectorialFillDiff.Append(ADiff: TCustomVectorialFillDiff);
begin
  FEnd.Assign((ADiff as TVectorialFillDiff).FEnd);
end;

{ TVectorialFillGradientDiff }

constructor TVectorialFillGradientDiff.Create(
  AGradientDiff: TBGRAGradientOriginalDiff);
begin
  FGradientDiff := AGradientDiff;
end;

destructor TVectorialFillGradientDiff.Destroy;
begin
  FGradientDiff.Free;
  inherited Destroy;
end;

procedure TVectorialFillGradientDiff.Apply(AFill: TVectorialFill);
begin
  if AFill.FillType = vftGradient then
    FGradientDiff.Apply(AFill.Gradient);
end;

procedure TVectorialFillGradientDiff.Unapply(AFill: TVectorialFill);
begin
  if AFill.FillType = vftGradient then
    FGradientDiff.Unapply(AFill.Gradient);
end;

function TVectorialFillGradientDiff.IsIdentity: boolean;
begin
  result := false;
end;

function TVectorialFillGradientDiff.CanAppend(ADiff: TCustomVectorialFillDiff): boolean;
begin
  result := (ADiff is TVectorialFillGradientDiff) and
    FGradientDiff.CanAppend(TVectorialFillGradientDiff(ADiff).FGradientDiff);
end;

procedure TVectorialFillGradientDiff.Append(ADiff: TCustomVectorialFillDiff);
var
  nextDiff: TVectorialFillGradientDiff;
begin
  nextDiff := ADiff as TVectorialFillGradientDiff;
  FGradientDiff.Append(nextDiff.FGradientDiff);
end;

{ TVectorialFill }

procedure TVectorialFill.SetOnChange(AValue: TVectorialFillChangeEvent);
begin
  if FOnChange=AValue then Exit;
  FOnChange:=AValue;
end;

procedure TVectorialFill.SetTextureMatrix(AValue: TAffineMatrix);
begin
  if FillType <> vftTexture then raise exception.Create(rsNotTextureFill);
  if FTextureMatrix=AValue then Exit;
  BeginUpdate;
  FTextureMatrix:=AValue;
  EndUpdate;
end;

procedure TVectorialFill.SetTextureOpacity(AValue: byte);
begin
  if FillType <> vftTexture then raise exception.Create(rsNotTextureFill);
  if FTextureOpacity=AValue then Exit;
  BeginUpdate;
  FTextureOpacity:=AValue;
  EndUpdate;
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
  FTextureAverageColorComputed:= false;
end;

procedure TVectorialFill.BeginUpdate;
begin
  if Assigned(OnBeforeChange) then
    OnBeforeChange(self);
  if Assigned(OnChange) and (FDiff = nil) then
    FDiff := TVectorialFillDiff.Create(self);
end;

procedure TVectorialFill.EndUpdate;
begin
  if Assigned(OnChange) then
  begin
    if Assigned(FDiff) then
    begin
      FDiff.ComputeDiff(self);
      if not FDiff.IsIdentity then OnChange(self, FDiff);
    end
    else
      OnChange(self, FDiff);
  end;
  FreeAndNil(FDiff);
end;

procedure TVectorialFill.NotifyChangeWithoutDiff;
var diff: TCustomVectorialFillDiff;
begin
  if Assigned(FOnChange) then
  begin
    diff := nil;
    FOnChange(self, diff);
  end;
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
  BeginUpdate;
  FTextureMatrix[1,3] := ANewCoord.x;
  FTextureMatrix[2,3] := ANewCoord.y;
  EndUpdate;
end;

procedure TVectorialFill.TextureMoveXAxis(ASender: TObject; APrevCoord,
  ANewCoord: TPointF; AShift: TShiftState);
var
  origin, xAxisRel: TPointF;
begin
  BeginUpdate;
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
  EndUpdate;
end;

procedure TVectorialFill.TextureMoveYAxis(ASender: TObject; APrevCoord,
  ANewCoord: TPointF; AShift: TShiftState);
var
  origin, yAxisRel: TPointF;
begin
  BeginUpdate;
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
  EndUpdate;
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
  FTextureAverageColorComputed:= false;
  FGradient := nil;
  FIsSolid := false;
  FTransparentMode := tmEnforeAllChannelsZero;
end;

function TVectorialFill.GetIsEditable: boolean;
begin
  result:= FillType in [vftGradient, vftTexture];
end;

procedure TVectorialFill.SetTextureRepetition(AValue: TTextureRepetition);
begin
  if FillType <> vftTexture then raise exception.Create(rsNotTextureFill);
  if FTextureRepetition=AValue then Exit;
  BeginUpdate;
  FTextureRepetition:=AValue;
  EndUpdate;
end;

function TVectorialFill.GetFillType: TVectorialFillType;
begin
  if FIsSolid then result:= vftSolid
  else if Assigned(FGradient) then result := vftGradient
  else if Assigned(FTexture) then result := vftTexture
  else result := vftNone;
end;

function TVectorialFill.GetAverageColor: TBGRAPixel;
begin
  case FillType of
  vftNone: result := BGRAPixelTransparent;
  vftGradient: result := Gradient.AverageColor;
  vftTexture: begin
      if not FTextureAverageColorComputed then
      begin
        if Assigned(FTexture) then
          FTextureAverageColor := FTexture.AverageColor
        else
          FTextureAverageColor := BGRAPixelTransparent;
        FTextureAverageColorComputed := true;
      end;
      result := FTextureAverageColor;
      result.alpha := BGRABlend.ApplyOpacity(result.alpha, TextureOpacity);
    end
  else {vftSolid} result := SolidColor;
  end;
end;

procedure TVectorialFill.SetTransparentMode(AValue: TTransparentMode);
begin
  if FTransparentMode=AValue then Exit;
  if (FillType = vftSolid) and (SolidColor.alpha = 0) then
  begin
    case FTransparentMode of
    tmNoFill: Clear;
    tmEnforeAllChannelsZero: SolidColor := BGRAPixelTransparent;
    end;
  end;
  FTransparentMode:=AValue;
end;

procedure TVectorialFill.GradientChange(ASender: TObject; ABounds: PRectF; var ADiff: TBGRAOriginalDiff);
var
  fillDiff: TVectorialFillGradientDiff;
begin
  if Assigned(FDiff) then
  begin
    FreeAndNil(ADiff);
    exit;
  end;
  if Assigned(OnChange) then
  begin
    if Assigned(ADiff) then
    begin
      fillDiff := TVectorialFillGradientDiff.Create(ADiff as TBGRAGradientOriginalDiff);
      ADiff := nil;
    end else
      fillDiff := nil;
    FOnChange(self, fillDiff);
    fillDiff.Free;
  end;
end;

constructor TVectorialFill.Create;
begin
  Init;
end;

procedure TVectorialFill.Clear;
begin
  if FillType <> vftNone then
  begin
    BeginUpdate;
    InternalClear;
    EndUpdate;
  end else
    InternalClear;
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
  if AColor.alpha = 0 then
  case TransparentMode of
  tmNoFill: begin Clear; exit; end;
  tmEnforeAllChannelsZero: AColor := BGRAPixelTransparent;
  end;
  if (FillType = vftSolid) and SolidColor.EqualsExactly(AColor) then exit;
  BeginUpdate;
  InternalClear;
  FColor := AColor;
  FIsSolid:= true;
  EndUpdate;
end;

procedure TVectorialFill.SetTexture(ATexture: TBGRABitmap;
  AMatrix: TAffineMatrix; AOpacity: byte; ATextureRepetition: TTextureRepetition);
begin
  BeginUpdate;
  InternalClear;
  FTexture := ATexture.NewReference as TBGRABitmap;
  FTextureMatrix := AMatrix;
  FTextureOpacity:= AOpacity;
  FTextureRepetition:= ATextureRepetition;
  FTextureAverageColorComputed:= false;
  EndUpdate;
end;

procedure TVectorialFill.SetGradient(AGradient: TBGRALayerGradientOriginal;
  AOwned: boolean);
begin
  BeginUpdate;
  InternalClear;
  if AOwned then FGradient := AGradient
  else FGradient := AGradient.Duplicate as TBGRALayerGradientOriginal;
  FGradient.OnChange:= @GradientChange;
  EndUpdate;
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
    result := FGradient.CreateScanner(AMatrix, ADraft)
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
    result := (FillType = vftGradient);
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
      BeginUpdate;
      FTextureMatrix := AMatrix*FTextureMatrix;
      EndUpdate;
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
      vftSolid: result := (FillType = vftSolid) and other.SolidColor.EqualsExactly(SolidColor);
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

class function TVectorialFill.Equal(AFill1, AFill2: TVectorialFill): boolean;
begin
  if AFill1 = nil then
  begin
    if AFill2 = nil then result := true
    else result := (AFill2.FillType = vftNone);
  end else
    result := AFill1.Equals(AFill2);
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
    vftGradient: SetGradient(other.Gradient, false);
    vftTexture: SetTexture(other.Texture, other.TextureMatrix, other.TextureOpacity, other.TextureRepetition);
    else Clear;
    end;
  end else
    raise exception.Create(rsIncompatibleType);
end;

procedure TVectorialFill.AssignExceptGeometry(Obj: TObject);
var
  other: TVectorialFill;
  tempGrad: TBGRALayerGradientOriginal;
begin
  if Obj = nil then Clear else
  if Obj is TVectorialFill then
  begin
    other := TVectorialFill(Obj);
    case other.FillType of
    vftSolid: SetSolid(other.SolidColor);
    vftGradient: begin
        if self.FillType = vftGradient then
          tempGrad := self.Gradient.Duplicate as TBGRALayerGradientOriginal
        else
          tempGrad := TBGRALayerGradientOriginal.Create;
        tempGrad.AssignExceptGeometry(other.Gradient);
        SetGradient(tempGrad, true);
      end;
    vftTexture: if self.FillType = vftTexture then
        SetTexture(other.Texture, self.TextureMatrix, other.TextureOpacity, other.TextureRepetition)
        else SetTexture(other.Texture, AffineMatrixIdentity, other.TextureOpacity, other.TextureRepetition);
    else Clear;
    end;
  end else
    raise exception.Create(rsIncompatibleType);
end;

procedure TVectorialFill.FitGeometry(const ABox: TAffineBox);
var
  sx,sy: single;
  u, v: TPointF;
begin
  case FillType of
  vftTexture:
    if Assigned(Texture) then
    begin
      if not (TextureRepetition in [trRepeatX,trRepeatBoth]) and (Texture.Width > 0) then
        sx:= 1/Texture.Width else if ABox.Width > 0 then sx:= 1/ABox.Width else sx := 1;
      if not (TextureRepetition in [trRepeatY,trRepeatBoth]) and (Texture.Height > 0) then
        sy:= 1/Texture.Height else if ABox.Height > 0 then sy:= 1/ABox.Height else sy := 1;

      u := (ABox.TopRight-ABox.TopLeft)*sx;
      v := (ABox.BottomLeft-ABox.TopLeft)*sy;
      TextureMatrix := AffineMatrix(u, v, ABox.TopLeft);
    end;
  vftGradient:
    Gradient.FitGeometry(ABox);
  end;
end;

procedure TVectorialFill.ApplyOpacity(AOpacity: Byte);
var
  c: TBGRAPixel;
begin
  case FillType of
  vftSolid: begin
      c := SolidColor;
      c.alpha := BGRABlend.ApplyOpacity(c.alpha, AOpacity);
      SolidColor := c;
    end;
  vftGradient: Gradient.ApplyOpacity(AOpacity);
  vftTexture: TextureOpacity := BGRABlend.ApplyOpacity(TextureOpacity, AOpacity);
  end;
end;

end.

