// SPDX-License-Identifier: GPL-3.0-only
unit UStateType;

{$mode objfpc}

interface

uses
  Types, Classes, SysUtils, BGRABitmap, BGRABitmapTypes, BGRALayers,
  BGRALayerOriginal, fgl, UImageBackup;

const MinSizeToCompress = 512; //set to 1 if you want always compression
const MinSerializedSize = 16384;

type
  TState = class;

  { TStateDifference }

  TStateDifference = class
    function TryCompress: boolean; virtual;
    procedure ApplyTo(AState: TState); virtual; abstract;
    procedure UnapplyTo(AState: TState); virtual; abstract;
    function UsedMemory: int64; virtual;
    function ToString: ansistring; override;
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
    function GetChangingBounds: TRect; virtual;
    function GetChangingBoundsDefined: boolean; virtual;
    function GetCost: integer; virtual;
  public
    constructor Create(AFromState: TState; AToState: TState);
    constructor Create(AFromState: TState);
    constructor Create(ASavedBefore : boolean; ASavedAfter: boolean);
    procedure ApplyTo(AState: TState); override;
    procedure UnapplyTo(AState: TState); override;
    property SavedBefore: boolean read FSavedBefore write FSavedBefore;
    property SavedAfter: boolean read FSavedAfter write FSavedAfter;
    property Kind: TImageDifferenceKind read GetImageDifferenceKind;
    property ChangingBounds: TRect read GetChangingBounds;
    property ChangingBoundsDefined: boolean read GetChangingBoundsDefined;
    property IsIdentity: boolean read GetIsIdentity;
  end;

  TImageDifferenceList = specialize TFPGObjectList<TCustomImageDifference>;

  { TComposedImageDifference }

  TComposedImageDifference = class(TCustomImageDifference)
  private
    function GetCount: integer;
    function GetItem(AIndex: integer): TCustomImageDifference;
    function GetTotalCount: integer;
  protected
    FDiffs: TImageDifferenceList;
    FAgglutinate: boolean;
    function GetIsIdentity: boolean; override;
    function GetImageDifferenceKind: TImageDifferenceKind; override;
    function GetChangingBounds: TRect; override;
    function GetChangingBoundsDefined: boolean; override;
  public
    constructor Create(AAgglutinate: boolean = false);
    procedure ReleaseDiffs;
    procedure StopAgglutinate;
    destructor Destroy; override;
    function TryCompress: boolean; override;
    function UsedMemory: int64; override;
    procedure Add(ADiff: TCustomImageDifference);
    procedure AddRange(AComposed: TComposedImageDifference);
    procedure ApplyTo(AState: TState); override;
    procedure UnapplyTo(AState: TState); override;
    procedure Clear;
    procedure Delete(AIndex: integer);
    procedure DeleteFrom(AIndex: integer);
    function GetLast: TCustomImageDifference;
    function ToString: ansistring; override;
    property Count: integer read GetCount;
    property TotalCount: integer read GetTotalCount;
    property Agglutinate: boolean read FAgglutinate;
    property Item[AIndex: integer]: TCustomImageDifference read GetItem; default;
  end;

{*********** Layer info *************}

type
  TLayerInfo = record
    Id: integer;
    Visible: boolean;
    BlendOp: TBlendOperation;
    Name: string;
    Opactiy: byte;
    Offset: TPoint;
  end;

procedure ApplyLayerInfo(AInfo: TLayerInfo; ALayeredBitmap: TBGRALayeredBitmap; AIndex: integer);
function GetLayerInfo(ALayeredBitmap: TBGRALayeredBitmap; AIndex: integer): TLayerInfo;

{*********** Inversible **************}

type
  TInversibleAction = (iaHorizontalFlip, iaHorizontalFlipLayer, iaVerticalFlip, iaVerticalFlipLayer, iaRotateCW, iaRotateCCW, iaRotate180, iaSwapRedBlue, iaLinearNegative);

const
  InversibleActionStr : array[TInversibleAction] of string =
    ('HorizontalFlip', 'HorizontalFlipLayer', 'VerticalFlip', 'VerticalFlipLayer', 'RotateCW', 'RotateCCW', 'Rotate180', 'SwapRedBlue', 'LinearNegative');

function GetInverseAction(AAction: TInversibleAction): TInversibleAction;
function CanCombineInversibleAction(AAction1, AAction2: TInversibleAction; out
  ACombined: TInversibleAction): boolean;

type
  { TImageDiff }

  TImageDiff = class
  private
    FChangeRect: TRect;
    FGrayscale: boolean;
    FSavedFilename: string;
    FImageBackup: TImageBackup;
    FSizeBefore, FSizeAfter: TSize;
    function GetIsIdentity: boolean;
    procedure DiscardFile;
    procedure Decompress;
    procedure Init(Image1,Image2: TBGRABitmap; AGrayscale: boolean); overload;
    procedure Init(Image1,Image2: TBGRABitmap; AGrayscale: boolean; AChangeRect: TRect); overload;
  protected
    function CreateNew(AWidth, AHeight: integer): TBGRABitmap;
  public
    constructor Create(Image1: TBGRABitmap; AGrayscale: boolean);
    constructor Create(Image1: TBGRABitmap; AGrayscale: boolean; AChangeRect: TRect);
    constructor Create(Image1,Image2: TBGRABitmap; AGrayscale: boolean);
    constructor Create(Image1,Image2: TBGRABitmap; AGrayscale: boolean; AChangeRect: TRect);
    procedure ApplyInPlace(ADest: TBGRABitmap; AXor: boolean);
    function ApplyInNew(ASource: TBGRABitmap; AReverse: boolean; AXor: boolean): TBGRABitmap;
    function ApplyCanCreateNew(ASource: TBGRABitmap; AReverse: boolean; AXor: boolean): TBGRABitmap;
    function Compress: boolean;
    destructor Destroy; override;
    function UsedMemory: int64;
    property ChangeRect: TRect read FChangeRect;
    property IsIdentity: boolean read GetIsIdentity;
    property SizeBefore: TSize read FSizeBefore;
    property SizeAfter: TSize read FSizeAfter;
  end;

type
  { TStoredImage }

  TStoredImage = class(TImageDiff)
  public
    function GetBitmap: TBGRABitmap;
  end;

  { TStoredLayer }

  TStoredLayer = class(TStoredImage)
  private
    function GetId: integer;
    function GetOffset: TPoint;
  protected
    FInfo: TLayerInfo;
    FIndex: integer;
    FOriginalData: TMemoryStream;
    FOriginalBitmapStored: boolean;
    FOriginalRenderStatus: TOriginalRenderStatus;
    FOriginalMatrix: TAffineMatrix;
    FOriginalDraft: boolean;
    FOriginalGuid: TGuid;
    FRegistryData: TMemoryStream;
  public
    constructor Create(ALayeredImage: TBGRALayeredBitmap; AIndex: integer);
    constructor Create(ALayeredImage: TBGRALayeredBitmap; AIndex: integer;
                       AAlwaysStoreBitmap: boolean);
    destructor Destroy; override;
    procedure Restore(ALayeredImage: TBGRALayeredBitmap);
    procedure Replace(ALayeredImage: TBGRALayeredBitmap);
    property LayerIndex: integer read FIndex;
    property LayerId: integer read GetId;
    property Offset: TPoint read GetOffset;
  end;

implementation

uses Math, UFileSystem;

{ TComposedImageDifference }

function TComposedImageDifference.GetCount: integer;
begin
  result := FDiffs.Count;
end;

function TComposedImageDifference.GetItem(AIndex: integer): TCustomImageDifference;
begin
  result := FDiffs[AIndex];
end;

function TComposedImageDifference.GetTotalCount: integer;
var
  i: Integer;
begin
  result := 0;
  for i := 0 to FDiffs.Count-1 do
    if FDiffs[i] is TComposedImageDifference then
      inc(result, TComposedImageDifference(FDiffs[i]).TotalCount)
    else
      inc(result, FDiffs[i].GetCost);
end;

function TComposedImageDifference.GetIsIdentity: boolean;
var
  i: Integer;
begin
  if FAgglutinate then exit(false);
  for i := 0 to FDiffs.Count-1 do
    if not FDiffs[i].GetIsIdentity then exit(false);
  exit(true);
end;

function TComposedImageDifference.GetImageDifferenceKind: TImageDifferenceKind;
var
  i: Integer;
begin
  result := idkChangeStack;
  for i := 0 to FDiffs.Count-1 do
    case FDiffs[i].GetImageDifferenceKind of
      idkChangeImageAndSelection: result := idkChangeImageAndSelection;
      idkChangeSelection: if result in[idkChangeImage,idkChangeImageAndSelection] then
                            result := idkChangeImageAndSelection
                          else result := idkChangeSelection;
      idkChangeImage: if result in[idkChangeImageAndSelection,idkChangeSelection] then
                            result := idkChangeImageAndSelection
                          else result := idkChangeImage;
    end;
end;

function TComposedImageDifference.GetChangingBounds: TRect;
var
  i: Integer;
  r: TRect;
begin
  result:= EmptyRect;
  for i := 0 to FDiffs.Count-1 do
  begin
    r := FDiffs[i].GetChangingBounds;
    if not IsRectEmpty(r) then
    begin
      if IsRectEmpty(result) then result:= r
      else UnionRect(result, result,r);
    end;
  end;
end;

function TComposedImageDifference.GetChangingBoundsDefined: boolean;
var
  i: Integer;
begin
  for i := 0 to FDiffs.Count-1 do
    if not FDiffs[i].GetChangingBoundsDefined then exit(false);
  exit(true);
end;

constructor TComposedImageDifference.Create(AAgglutinate: boolean);
begin
  FDiffs := TImageDifferenceList.Create;
  FAgglutinate:= AAgglutinate;
end;

procedure TComposedImageDifference.ReleaseDiffs;
begin
  FDiffs.FreeObjects:= false;
  FDiffs.Clear;
  FDiffs.FreeObjects:= true;
end;

procedure TComposedImageDifference.StopAgglutinate;
begin
  FAgglutinate:= false;
end;

destructor TComposedImageDifference.Destroy;
begin
  FDiffs.Free;
  inherited Destroy;
end;

function TComposedImageDifference.TryCompress: boolean;
var
  i: Integer;
begin
  for i := 0 to FDiffs.Count-1 do
    if FDiffs[i].TryCompress then exit(true);
  exit(false);
end;

function TComposedImageDifference.UsedMemory: int64;
var
  i: Integer;
begin
  result := 0;
  for i := 0 to FDiffs.Count-1 do
    inc(result, FDiffs[i].UsedMemory);
end;

procedure TComposedImageDifference.Add(ADiff: TCustomImageDifference);
begin
  FDiffs.Add(ADiff);
end;

procedure TComposedImageDifference.AddRange(AComposed: TComposedImageDifference);
var
  i: Integer;
begin
  for i:= 0 to AComposed.Count-1 do
    Add(AComposed.FDiffs[i]);
end;

procedure TComposedImageDifference.ApplyTo(AState: TState);
var
  i: Integer;
begin
  for i := 0 to FDiffs.Count-1 do
    FDiffs[i].ApplyTo(AState);
end;

procedure TComposedImageDifference.UnapplyTo(AState: TState);
var
  i: Integer;
begin
  for i := FDiffs.Count-1 downto 0 do
    FDiffs[i].UnapplyTo(AState);
end;

procedure TComposedImageDifference.Clear;
begin
  FDiffs.Clear;
end;

procedure TComposedImageDifference.Delete(AIndex: integer);
begin
  FDiffs.Delete(AIndex);
end;

procedure TComposedImageDifference.DeleteFrom(AIndex: integer);
var
  i: Integer;
begin
  for i := Count-1 downto AIndex do
    Delete(i);
end;

function TComposedImageDifference.GetLast: TCustomImageDifference;
begin
  if Count = 0 then result := nil
  else result := FDiffs[Count-1];
end;

function TComposedImageDifference.ToString: ansistring;
var
  i: Integer;
begin
  Result:= '[';
  for i := 0 to Count-1 do
  begin
    if i <> 0 then result += ', ';
    result += FDiffs[i].ToString;
  end;
  result += ']';
end;

{ TStateDifference }

function TStateDifference.TryCompress: boolean;
begin
  result := false;
end;

function TStateDifference.UsedMemory: int64;
begin
  result := 0;
end;

function TStateDifference.ToString: ansistring;
begin
  Result:= ClassName;
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

function TCustomImageDifference.GetChangingBounds: TRect;
begin
  result := rect(0,0,0,0);
end;

function TCustomImageDifference.GetChangingBoundsDefined: boolean;
begin
  result := false;
end;

function TCustomImageDifference.GetCost: integer;
begin
  result := 1;
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
  if ALayeredBitmap.LayerOriginalGuid[AIndex] = GUID_NULL then
    ALayeredBitmap.LayerOffset[AIndex] := AInfo.Offset;
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
  result.Offset := ALayeredBitmap.LayerOffset[AIndex];
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

{ TImageDiff }

function TImageDiff.GetIsIdentity: boolean;
begin
  result := (FSizeBefore.cx = FSizeAfter.cx) and (FSizeBefore.cy = FSizeAfter.cy)
            and ChangeRect.IsEmpty;
end;

procedure TImageDiff.DiscardFile;
begin
  if FSavedFilename <> '' then
  begin
    try
      if FileManager.FileExists(FSavedFilename) then
        FileManager.DeleteFile(FSavedFilename);
    except on ex:exception do begin end;
    end;
    FSavedFilename:= '';
  end;
end;

procedure TImageDiff.Decompress;
var
  stream: TStream;
begin
  if (FImageBackup = nil) and (FSavedFilename <> '') then
  begin
    stream := nil;
    FImageBackup := TImageBackup.Create;
    try
      stream := FileManager.CreateFileStream(FSavedFilename,fmOpenRead or fmShareDenyWrite);
      FImageBackup.LoadFromStream(stream);
    except
    end;
    stream.free;
  end;
end;

procedure TImageDiff.Init(Image1, Image2: TBGRABitmap; AGrayscale: boolean);
var r: TRect;
begin
  r := rect(0,0,0,0);
  if Assigned(image1) then
  begin
    r.Right := image1.Width;
    r.Bottom := image1.Height;
  end;
  if Assigned(image2) then
  begin
    r.Right := max(r.Right, image2.Width);
    r.Bottom := max(r.Bottom, image2.Height);
  end;
  Init(Image1, Image2, AGrayscale, r);
end;

procedure TImageDiff.Init(Image1, Image2: TBGRABitmap; AGrayscale: boolean;
  AChangeRect: TRect);
begin
  if Image1 = nil then FSizeBefore := Size(0, 0) else
    FSizeBefore := Size(Image1.Width, Image1.Height);
  if Image2 = nil then FSizeAfter := Size(0, 0) else
    FSizeAfter := Size(Image2.Width, Image2.Height);

  FImageBackup := TImageBackup.Create(Image1, Image2, AGrayscale, AChangeRect);
  FChangeRect := FImageBackup.Bounds;
  FGrayscale := AGrayscale;
end;

function TImageDiff.CreateNew(AWidth, AHeight: integer): TBGRABitmap;
begin
  if FGrayscale then
    result := TBGRABitmap.Create(AWidth, AHeight, BGRABlack)
  else
    result := TBGRABitmap.Create(AWidth, AHeight);
end;

constructor TImageDiff.Create(Image1: TBGRABitmap; AGrayscale: boolean);
begin
  Init(Image1, nil, AGrayscale);
end;

constructor TImageDiff.Create(Image1: TBGRABitmap; AGrayscale: boolean;
  AChangeRect: TRect);
begin
  Init(Image1, nil, AGrayscale, AChangeRect);
end;

constructor TImageDiff.Create(Image1, Image2: TBGRABitmap; AGrayscale: boolean);
begin
  Init(Image1, Image2, AGrayscale);
end;

constructor TImageDiff.Create(Image1, Image2: TBGRABitmap; AGrayscale: boolean;
  AChangeRect: TRect);
begin
  Init(Image1, Image2, AGrayscale, AChangeRect);
end;

procedure TImageDiff.ApplyInPlace(ADest: TBGRABitmap; AXor: boolean);
begin
  if ADest = nil then raise exception.Create('Unexpected nil reference');
  Decompress;
  if Assigned(FImageBackup) then
    FImageBackup.Restore(ADest, ChangeRect, AXor)
  else
  begin
    if not AXor then
      ADest.EraseRect(ChangeRect, 255);
  end;
end;

function TImageDiff.ApplyInNew(ASource: TBGRABitmap; AReverse: boolean; AXor: boolean): TBGRABitmap;
var
  destSize: TSize;
begin
  if (self = nil) or IsIdentity then
  begin
    if ASource = nil then
      result := nil
    else
      result := ASource.Duplicate as TBGRABitmap
  end
  else
  begin
    if AReverse then destSize := FSizeBefore else
      destSize := FSizeAfter;

    if (destSize.cx = 0) or (destSize.cy = 0) then
      result := nil
    else
    begin
      result := CreateNew(destSize.cx, destSize.cy);
      if ASource <> nil then
        result.PutImage(0, 0, ASource, dmSet);
      ApplyInPlace(result, AXor);
    end;
  end;
end;

function TImageDiff.ApplyCanCreateNew(ASource: TBGRABitmap; AReverse: boolean; AXor: boolean): TBGRABitmap;
begin
  if (self = nil) or IsIdentity then exit(ASource); //keep

  if (ASource = nil) or
     ((FSizeAfter.cx <> FSizeBefore.cx) or
     (FSizeAfter.cy <> FSizeBefore.cy)) then
     exit(ApplyInNew(ASource, AReverse, AXor))
  else
  begin
    ApplyInPlace(ASource, AXor);
    exit(ASource);
  end;
end;

function TImageDiff.Compress: boolean;
var
  savedFile: TStream;
begin
  if Assigned(FImageBackup) and (FSavedFilename = '') then
  begin
    FSavedFilename := GetTempFileName;
    try
      savedFile := FileManager.CreateFileStream(FSavedFilename,fmCreate);
      try
        FImageBackup.SaveToStream(savedFile);
        FreeAndNil(FImageBackup);
        result := true;
      finally
        savedFile.Free;
      end;
    except
      on ex: exception do
      begin
        if FileManager.FileExists(FSavedFilename) then FileManager.DeleteFile(FSavedFilename);
        FSavedFilename := '';
        result := false;
      end;
    end;
  end else
  if Assigned(FImageBackup) then
  begin
    FreeAndNil(FImageBackup);
    result := true;
  end else
    result := false;
end;

destructor TImageDiff.Destroy;
begin
  FImageBackup.Free;
  DiscardFile;
  inherited Destroy;
end;

function TImageDiff.UsedMemory: int64;
begin
  if Assigned(FImageBackup) then result := FImageBackup.UsedMemory
  else result := 0;
end;

{ TStoredImage }

function TStoredImage.GetBitmap: TBGRABitmap;
begin
  result := ApplyInNew(nil, true, false);
end;

{ TStoredLayer }

function TStoredLayer.GetId: integer;
begin
  result := FInfo.Id;
end;

function TStoredLayer.GetOffset: TPoint;
begin
  result := FInfo.Offset;
end;

constructor TStoredLayer.Create(ALayeredImage: TBGRALayeredBitmap;
  AIndex: integer);
var
  {%H-}orig: TBGRALayerCustomOriginal;
  alwaysStoreBitmap: Boolean;
begin
  alwaysStoreBitmap := false;
  if (ALayeredImage.LayerOriginalGuid[AIndex]<>GUID_NULL) and
    ALayeredImage.LayerOriginalKnown[AIndex] then
  begin
    try
      orig := ALayeredImage.LayerOriginal[AIndex];
    except
      on ex:exception do
        alwaysStoreBitmap:= true;
    end;
  end;
  Create(ALayeredImage, AIndex, alwaysStoreBitmap);
end;

constructor TStoredLayer.Create(ALayeredImage: TBGRALayeredBitmap;
  AIndex: integer; AAlwaysStoreBitmap: boolean);
begin
  FIndex := AIndex;
  FInfo := GetLayerInfo(ALayeredImage, AIndex);
  if ALayeredImage.LayerOriginalGuid[AIndex]<>GUID_NULL then
  begin
    FOriginalBitmapStored := AAlwaysStoreBitmap or not ALayeredImage.LayerOriginalKnown[AIndex];
    FOriginalRenderStatus:= ALayeredImage.LayerOriginalRenderStatus[AIndex];

    if not FOriginalBitmapStored then
      inherited Create(nil, False)
    else
      inherited Create(ALayeredImage.LayerBitmap[AIndex], False);

    FOriginalData := TMemoryStream.Create;
    FOriginalGuid := ALayeredImage.LayerOriginalGuid[AIndex];
    ALayeredImage.SaveOriginalToStream(FOriginalGuid, FOriginalData);
    FOriginalMatrix := ALayeredImage.LayerOriginalMatrix[AIndex];
    FOriginalDraft := ALayeredImage.LayerOriginalRenderStatus[AIndex] in[orsDraft,orsPartialDraft];
  end else
  begin
    inherited Create(ALayeredImage.LayerBitmap[AIndex], False);
    FOriginalData := nil;
  end;
  FRegistryData := TMemoryStream.Create;
  ALayeredImage.SaveLayerRegistryToStream(AIndex, FRegistryData);
end;

destructor TStoredLayer.Destroy;
begin
  FRegistryData.Free;
  FOriginalData.Free;
  inherited Destroy;
end;

procedure TStoredLayer.Restore(ALayeredImage: TBGRALayeredBitmap);
var
  tempIdx, idxOrig: Integer;
begin
  if Assigned(FOriginalData) then
  begin
    FOriginalData.Position:= 0;
    idxOrig := ALayeredImage.IndexOfOriginal(FOriginalGuid);
    if idxOrig = -1 then
      idxOrig := ALayeredImage.AddOriginalFromStream(FOriginalData, FOriginalGuid, true);

    if not FOriginalBitmapStored then
    begin
      tempIdx := ALayeredImage.AddLayerFromOriginal(ALayeredImage.Original[idxOrig].Guid, FOriginalMatrix);
      ALayeredImage.RenderLayerFromOriginal(tempIdx, FOriginalDraft);
    end else
    begin
      tempIdx := ALayeredImage.AddOwnedLayer(GetBitmap);
      ALayeredImage.LayerOffset[tempIdx] := FInfo.Offset;
      ALayeredImage.LayerOriginalGuid[tempIdx] := ALayeredImage.OriginalGuid[idxOrig];
      ALayeredImage.LayerOriginalMatrix[tempIdx] := FOriginalMatrix;
      ALayeredImage.LayerOriginalRenderStatus[tempIdx] := FOriginalRenderStatus;
    end;
  end else
    tempIdx := ALayeredImage.AddOwnedLayer(GetBitmap);

  ApplyLayerInfo(FInfo,ALayeredImage,tempIdx);
  FRegistryData.Position := 0;
  ALayeredImage.LoadLayerRegistryFromStream(tempIdx, FRegistryData);
  ALayeredImage.InsertLayer(FIndex,tempIdx);
end;

procedure TStoredLayer.Replace(ALayeredImage: TBGRALayeredBitmap);
var
  idxOrig: Integer;
begin
  if Assigned(FOriginalData) then
  begin
    FOriginalData.Position:= 0;
    idxOrig := ALayeredImage.IndexOfOriginal(FOriginalGuid);
    if idxOrig = -1 then
      idxOrig := ALayeredImage.AddOriginalFromStream(FOriginalData, FOriginalGuid, true);
    if not FOriginalBitmapStored then
    begin
      ALayeredImage.LayerOriginalGuid[FIndex] := ALayeredImage.OriginalGuid[idxOrig];
      ALayeredImage.LayerOriginalMatrix[FIndex] := FOriginalMatrix;
      ALayeredImage.RenderLayerFromOriginal(FIndex, FOriginalDraft);
    end else
    begin
      ALayeredImage.SetLayerBitmap(FIndex, GetBitmap, True);
      ALayeredImage.LayerOffset[FIndex] := FInfo.Offset;
      ALayeredImage.LayerOriginalGuid[FIndex] := ALayeredImage.OriginalGuid[idxOrig];
      ALayeredImage.LayerOriginalMatrix[FIndex] := FOriginalMatrix;
      ALayeredImage.LayerOriginalRenderStatus[FIndex] := FOriginalRenderStatus;
    end;
  end else
    ALayeredImage.SetLayerBitmap(FIndex, GetBitmap, True);
  ALayeredImage.RemoveUnusedOriginals;

  ApplyLayerInfo(FInfo,ALayeredImage,FIndex);
  FRegistryData.Position := 0;
  ALayeredImage.LoadLayerRegistryFromStream(FIndex, FRegistryData);
end;

end.

