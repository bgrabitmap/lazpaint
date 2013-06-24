unit UStateType;

{$mode objfpc}

interface

uses
  Types, Classes, SysUtils, BGRABitmap, BGRABitmapTypes, BGRALayers, BGRACompressableBitmap;

const MinSizeToCompress = 4096;
const MinSerializedSize = 16384;

type
  TState = class;

  { TStateDifference }

  TStateDifference = class
    function TryCompress: boolean; virtual;
    procedure ApplyTo(AState: TState); virtual; abstract;
    procedure UnapplyTo(AState: TState); virtual; abstract;
    function UsedMemory: int64; virtual;
  end;

  TState = class
    saved: boolean;
    procedure ApplyDifference(ADifference: TStateDifference); virtual; abstract;
    procedure ReverseDifference(ADifference: TStateDifference); virtual; abstract;
    function Duplicate: TState; virtual; abstract;
  end;

  TImageDifferenceKind = (idkChangeImageAndSelection, idkChangeStack, idkChangeSelection,
                           idkChangeImage);

  { TCustomImageDifference }

  TCustomImageDifference = class(TStateDifference)
  protected
    FSavedBefore, FSavedAfter: boolean;
    function GetIsIdentity: boolean; virtual;
    function GetImageDifferenceKind: TImageDifferenceKind; virtual;
  public
    constructor Create(AFromState: TState; AToState: TState);
    constructor Create(AFromState: TState);
    constructor Create(ASavedBefore : boolean; ASavedAfter: boolean);
    procedure ApplyTo(AState: TState); override;
    procedure UnapplyTo(AState: TState); override;
    property SavedBefore: boolean read FSavedBefore write FSavedBefore;
    property SavedAfter: boolean read FSavedAfter write FSavedAfter;
    property Kind: TImageDifferenceKind read GetImageDifferenceKind;
    property IsIdentity: boolean read GetIsIdentity;
  end;

function DuplicateBitmap(ABitmap: TBGRABitmap): TBGRABitmap;
function DuplicateLayeredBitmap(ALayeredBitmap: TBGRALayeredBitmap): TBGRALayeredBitmap;
procedure SerializeBitmap(ABitmap: TBGRABitmap; AStream: TStream);

{*********** Layer info *************}

type
  TLayerInfo = record
    Id: integer;
    Visible: boolean;
    BlendOp: TBlendOperation;
    Name: string;
    Opactiy: byte;
  end;

procedure ApplyLayerInfo(AInfo: TLayerInfo; ALayeredBitmap: TBGRALayeredBitmap; AIndex: integer);
procedure ApplyLayerInfo(AInfo: TLayerInfo; ALayeredBitmap: TBGRALayeredBitmap);
function GetLayerInfo(ALayeredBitmap: TBGRALayeredBitmap; AIndex: integer): TLayerInfo;

{*********** Inversible **************}

type
  TInversibleAction = (iaHorizontalFlip, iaVerticalFlip, iaRotateCW, iaRotateCCW, iaRotate180, iaSwapRedBlue, iaLinearNegative);

function GetInverseAction(AAction: TInversibleAction): TInversibleAction;
function CanCombineInversibleAction(AAction1, AAction2: TInversibleAction; out
  ACombined: TInversibleAction): boolean;

type
  { TImageDiff }

  TImageDiff = class
  private
    FSavedFilename: string;
    FBitmapDiff: TBGRACompressableBitmap;
    function GetBitmapDiff: TBGRACompressableBitmap;
    procedure DiscardFile;
    function GetIsIdentity: boolean;
  public
    SizeBefore, SizeAfter: TSize;
    constructor Create(Image1,Image2: TBGRABitmap);
    procedure SetBitmapDiff(AValue: TBGRACompressableBitmap);
    function Compress: boolean;
    destructor Destroy; override;
    function UsedMemory: int64;
    property BitmapDiff: TBGRACompressableBitmap read GetBitmapDiff;
    property IsIdentity: boolean read GetIsIdentity;
  end;

function ComputeFromImageDiff(FromImage: TBGRABitmap; ADiff: TImageDiff; AReverse: boolean): TBGRABitmap;
procedure ApplyImageDiffAndReplace(var AImage: TBGRABitmap; ADiff: TImageDiff; AReverse: boolean);

type
  { TStoredImage }

  TStoredImage = class(TImageDiff)
  private
    function GetStoredImage: TBGRACompressableBitmap;
    procedure SetStoredImage(AValue: TBGRACompressableBitmap);
  public
    constructor Create(ABitmap: TBGRABitmap);
    function GetBitmap: TBGRABitmap;
    property StoredImage: TBGRACompressableBitmap read GetStoredImage;
  end;

implementation

uses Math;

{ TStateDifference }

function TStateDifference.TryCompress: boolean;
begin
  result := false;
end;

function TStateDifference.UsedMemory: int64;
begin
  result := 0;
end;

{ TCustomImageDifference }

function TCustomImageDifference.GetIsIdentity: boolean;
begin
  result := false;
end;

function TCustomImageDifference.GetImageDifferenceKind: TImageDifferenceKind;
begin
  result := idkChangeImageAndSelection;
end;

constructor TCustomImageDifference.Create(AFromState: TState; AToState: TState);
begin
  FSavedBefore:= AFromState.saved;
  FSavedAfter:= AToState.saved;
end;

constructor TCustomImageDifference.Create(AFromState: TState);
begin
  FSavedBefore:= AFromState.saved;
  FSavedAfter:= false;
end;

constructor TCustomImageDifference.Create(ASavedBefore: boolean;
  ASavedAfter: boolean);
begin
  FSavedBefore := ASavedBefore;
  FSavedAfter := ASavedAfter;
end;

procedure TCustomImageDifference.ApplyTo(AState: TState);
begin
  AState.saved:= FSavedAfter;
end;

procedure TCustomImageDifference.UnapplyTo(AState: TState);
begin
  AState.saved:= FSavedBefore;
end;

{***********************************}

function DuplicateBitmap(ABitmap: TBGRABitmap): TBGRABitmap;
begin
  if ABitmap = nil then
    result := nil
  else
    result := ABitmap.Duplicate as TBGRABitmap;
end;

function DuplicateLayeredBitmap(ALayeredBitmap: TBGRALayeredBitmap): TBGRALayeredBitmap;
begin
  if ALayeredBitmap = nil then
    result := nil
  else
    result := ALayeredBitmap.Duplicate(True); //keep same layer ids for undo list
end;

procedure SerializeBitmap(ABitmap: TBGRABitmap; AStream: TStream);
begin
if ABitmap <> nil then
  ABitmap.Serialize(AStream) else
  TBGRABitmap.SerializeEmpty(AStream);
end;

procedure ApplyImageDiffAndReplace(var AImage: TBGRABitmap; ADiff: TImageDiff; AReverse: boolean);
var tempBmp: TBGRABitmap;
begin
  if ADiff = nil then exit;
  tempBmp := ComputeFromImageDiff(AImage, ADiff, AReverse);
  FreeAndNil(AImage);
  AImage := tempBmp;
end;

{*********** Layer info *************}

procedure ApplyLayerInfo(AInfo: TLayerInfo; ALayeredBitmap: TBGRALayeredBitmap; AIndex: integer);
begin
  if (AIndex < 0) or (AIndex >= ALayeredBitmap.NbLayers) then
      raise exception.Create('Out of bounds');
  ALayeredBitmap.LayerUniqueId[AIndex] := AInfo.Id;
  ALayeredBitmap.LayerVisible[AIndex] := AInfo.Visible;
  ALayeredBitmap.BlendOperation[AIndex] := AInfo.BlendOp;
  ALayeredBitmap.LayerName[AIndex] := AInfo.Name;
  ALayeredBitmap.LayerOpacity[AIndex] := AInfo.Opactiy;
end;

procedure ApplyLayerInfo(AInfo: TLayerInfo; ALayeredBitmap: TBGRALayeredBitmap);
var idx: integer;
begin
  idx := ALayeredBitmap.GetLayerIndexFromId(AInfo.Id);
  if idx = -1 then
    raise exception.Create('Layer not found');
  ApplyLayerInfo(AInfo, ALayeredBitmap, idx);
end;

function GetLayerInfo(ALayeredBitmap: TBGRALayeredBitmap; AIndex: integer): TLayerInfo;
begin
  if (AIndex < 0) or (AIndex >= ALayeredBitmap.NbLayers) then
      raise exception.Create('Out of bounds');
  result.Id := ALayeredBitmap.LayerUniqueId[AIndex];
  result.Visible := ALayeredBitmap.LayerVisible[AIndex];
  result.BlendOp := ALayeredBitmap.BlendOperation[AIndex];
  result.Name := ALayeredBitmap.LayerName[AIndex];
  result.Opactiy := ALayeredBitmap.LayerOpacity[AIndex];
end;

{*********** Inversible **************}

function GetInverseAction(AAction: TInversibleAction): TInversibleAction;
begin
  case AAction of
  iaRotateCW: result := iaRotateCCW;
  iaRotateCCW: result := iaRotateCW;
  else
    result := AAction;
  end;
end;

function CanCombineInversibleAction(AAction1, AAction2: TInversibleAction; out
  ACombined: TInversibleAction): boolean;
begin
  result := true;
  ACombined:= iaHorizontalFlip; //some default value
  case AAction1 of
  iaSwapRedBlue: result := false;
  iaRotate180: case AAction2 of
               iaRotateCW: ACombined:= iaRotateCCW;
               iaRotateCCW: ACombined := iaRotateCW;
               iaHorizontalFlip: ACombined := iaVerticalFlip;
               iaVerticalFlip: ACombined:= iaHorizontalFlip;
               else result := false;
               end;
  iaHorizontalFlip: case AAction2 of
                    iaVerticalFlip: ACombined:= iaRotate180;
                    else result := false;
                    end;
  iaVerticalFlip: case AAction2 of
                    iaHorizontalFlip: ACombined:= iaRotate180;
                    else result := false;
                    end;
  iaRotateCW: case AAction2 of
              iaRotateCW: ACombined:= iaRotate180;
              iaRotate180: ACombined:= iaRotateCCW;
              else result := false;
              end;
  iaRotateCCW: case AAction2 of
               iaRotateCCW: ACombined:= iaRotate180;
               iaRotate180: ACombined:= iaRotateCW;
               else result := false;
               end;
  else
    result := false;
  end;
end;

{**************** Image diff ****************}

function ComputeFromImageDiff(FromImage: TBGRABitmap; ADiff: TImageDiff; AReverse: boolean): TBGRABitmap;
var tempBmp: TBGRABitmap;
  DestSize: TSize;
begin
  if ADiff = nil then
  begin
    result := FromImage.Duplicate as TBGRABitmap;
    exit;
  end;
  if AReverse then DestSize := ADiff.SizeBefore else
    DestSize := ADiff.SizeAfter;
  if (DestSize.cx = 0) or (DestSize.cy = 0) or ((FromImage = nil) and (ADiff.BitmapDiff = nil)) then
    result := nil
  else
  begin
    result := TBGRABitmap.Create(Destsize.cx,Destsize.cy);
    if FromImage <> nil then
      result.PutImage(0,0,FromImage,dmXor);
    if ADiff.BitmapDiff <> nil then
    begin
      tempBmp := ADiff.BitmapDiff.GetBitmap;
      result.PutImage(0,0,tempBmp,dmXor);
      tempBmp.Free;
    end;
  end;
end;

{ TImageDiff }

function TImageDiff.GetBitmapDiff: TBGRACompressableBitmap;
var
  FSavedFile: TFileStream;
begin
  if (FBitmapDiff = nil) and (FSavedFilename <> '') then
  begin
    FSavedFile := TFileStream.Create(FSavedFilename,fmOpenRead);
    try
      FBitmapDiff := TBGRACompressableBitmap.Create;
      FBitmapDiff.ReadFromStream(FSavedFile);
    finally
      FSavedFile.Free;
    end;
  end;
  result := FBitmapDiff;
end;

procedure TImageDiff.DiscardFile;
begin
  if FSavedFilename <> '' then
  begin
    try
      if FileExists(FSavedFilename) then
        DeleteFile(FSavedFilename);
    except on ex:exception do begin end;
    end;
    FSavedFilename:= '';
  end;
end;

function TImageDiff.GetIsIdentity: boolean;
begin
  result := (SizeBefore.cx = SizeAfter.cx) and (SizeBefore.cy = SizeAfter.cy) and (FBitmapDiff=nil) and (FSavedFilename = '');
end;

constructor TImageDiff.Create(Image1, Image2: TBGRABitmap);
var tx,ty: integer;
  uncompressedDiff: TBGRABitmap;
  n: integer;
  p: PBGRAPixel;
  empty : boolean;
begin
  FBitmapDiff := nil;
  if Image1 = nil then
  begin
    SizeBefore.cx := 0;
    SizeBefore.cy := 0;
  end else
  begin
    SizeBefore.cx := Image1.Width;
    SizeBefore.cy := Image1.Height;
  end;

  if Image2 = nil then
  begin
    SizeAfter.cx := 0;
    SizeAfter.cy := 0;
  end else
  begin
    SizeAfter.cx := Image2.Width;
    SizeAfter.cy := Image2.Height;
  end;

  tx := max(SizeBefore.cx,SizeAfter.cx);
  ty := max(SizeBefore.cy,SizeAfter.cy);
  if (tx<>0) and (ty<>0) then
  begin
    uncompressedDiff := TBGRABitmap.Create(tx,ty);
    if Image1<>nil then
      uncompressedDiff.PutImage(0,0,Image1,dmXor);
    if Image2<>nil then
      uncompressedDiff.PutImage(0,0,Image2,dmXor);
    empty := true;
    p := uncompressedDiff.Data;
    for n := uncompressedDiff.NbPixels-1 downto 0 do
    begin
      if PLongWord(p)^ <> 0 then
      begin
        empty := false;
        break;
      end;
      inc(p);
    end;
    if not empty then
      SetBitmapDiff(TBGRACompressableBitmap.Create(uncompressedDiff));
    uncompressedDiff.free;
  end;
end;

procedure TImageDiff.SetBitmapDiff(AValue: TBGRACompressableBitmap);
begin
  DiscardFile;
  if FBitmapDiff <> nil then FreeAndNil(FBitmapDiff);
  FBitmapDiff := AValue;
end;

function TImageDiff.Compress: boolean;
var
  FSavedFile: TFileStream;
begin
  if FBitmapDiff = nil then
    result := false
  else
  if FSavedFilename = '' then
  begin
    if FBitmapDiff.UsedMemory < MinSizeToCompress then
    begin
      result := false;
      exit;
    end;
    result := FBitmapDiff.Compress;
    if not result and (FBitmapDiff.UsedMemory >= MinSerializedSize) then
    begin
      FSavedFilename := GetTempFileName;
      try
        FSavedFile := TFileStream.Create(FSavedFilename,fmOpenWrite or fmCreate);
        try
          FBitmapDiff.WriteToStream(FSavedFile);
          FreeAndNil(FBitmapDiff);
          result := true;
        finally
          FSavedFile.Free;
        end;
      except
        on ex: exception do
        begin
          if FileExists(FSavedFilename) then DeleteFile(FSavedFilename);
          FSavedFilename := '';
          result := false;
        end;
      end;
    end;
  end else
  begin
    FreeAndNil(FBitmapDiff);
    result := true;
  end;
end;

destructor TImageDiff.Destroy;
begin
  BitmapDiff.Free;
  DiscardFile;
  inherited Destroy;
end;

function TImageDiff.UsedMemory: int64;
begin
  if Assigned(BitmapDiff) then
    result := BitmapDiff.UsedMemory
  else
  result := 0;
end;

{ TStoredImage }

function TStoredImage.GetStoredImage: TBGRACompressableBitmap;
begin
  result := BitmapDiff;
end;

procedure TStoredImage.SetStoredImage(AValue: TBGRACompressableBitmap);
begin
  SetBitmapDiff(AValue);
end;

constructor TStoredImage.Create(ABitmap: TBGRABitmap);
begin
  SetStoredImage(TBGRACompressableBitmap.Create(ABitmap));
end;

function TStoredImage.GetBitmap: TBGRABitmap;
var stored: TBGRACompressableBitmap;
begin
  stored := StoredImage;
  if stored <> nil then
    result := stored.GetBitmap
  else
    result := nil;
end;

end.

