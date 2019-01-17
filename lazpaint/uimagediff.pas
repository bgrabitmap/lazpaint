unit UImageDiff;

{$mode objfpc}

interface

uses
  Classes, SysUtils, UStateType, BGRABitmap, BGRABitmapTypes, BGRALayers,
  BGRALayerOriginal;

function IsInverseImageDiff(ADiff1, ADiff2: TCustomImageDifference): boolean;
function TryCombineImageDiff(ANewDiff, APrevDiff: TCustomImageDifference): boolean;

type
  { TInversibleStateDifference }

  TInversibleStateDifference = class(TCustomImageDifference)
  private
    FAction: TInversibleAction;
    FLayerIndex: integer;
  public
    constructor Create(AState: TState; AAction: TInversibleAction; ALayerIndex : integer = -1);
    procedure ApplyTo(AState: TState); override;
    procedure UnApplyTo(AState: TState); override;
    procedure ApplyAction(AState: TState; AAction: TInversibleAction; AInverse: boolean);
    property Action: TInversibleAction read FAction write FAction;
    property LayerIndex: integer read FLayerIndex;
  end;

type
  { TImageLayerStateDifference }

  TImageLayerStateDifference = class(TCustomImageDifference)
  private
    function GetChangeImageLayer: boolean;
  protected
    function GetImageDifferenceKind: TImageDifferenceKind; override;
    function GetIsIdentity: boolean; override;
    function GetChangingBoundsDefined: boolean; override;
    function GetChangingBounds: TRect; override;
    procedure Init(AToState: TState; APreviousImage: TBGRABitmap; APreviousImageChangeRect: TRect;
        APreviousSelection: TBGRABitmap; APreviousSelectionChangeRect: TRect;
        APreviousSelectionLayer: TBGRABitmap; APreviousSelectionLayerChangeRect: TRect;
        APreviousLayerOriginalData: TStream;
        APreviousLayerOriginalMatrix: TAffineMatrix);
  public
    layerId: integer;
    imageDiff, selectionLayerDiff: TImageDiff;
    selectionDiff: TGrayscaleImageDiff;
    layerOriginalChange: boolean;
    prevLayerOriginalData, nextLayerOriginalData: TStream;
    prevLayerOriginalMatrix, nextLayerOriginalMatrix: TAffineMatrix;
    function TryCompress: boolean; override;
    procedure ApplyTo(AState: TState); override;
    procedure UnapplyTo(AState: TState); override;
    function UsedMemory: int64; override;
    constructor Create(AFromState, AToState: TState);
    constructor Create(AToState: TState; APreviousImage: TBGRABitmap; APreviousImageDefined: boolean;
        APreviousSelection: TBGRABitmap; APreviousSelectionDefined: boolean;
        APreviousSelectionLayer: TBGRABitmap; APreviousSelectionLayerDefined: boolean;
        APreviousLayerOriginalData: TStream;
        APreviousLayerOriginalMatrix: TAffineMatrix); overload;
    constructor Create(AToState: TState; APreviousImage: TBGRABitmap; APreviousImageChangeRect: TRect;
        APreviousSelection: TBGRABitmap; APreviousSelectionChangeRect: TRect;
        APreviousSelectionLayer: TBGRABitmap; APreviousSelectionLayerChangeRect: TRect;
        APreviousLayerOriginalData: TStream;
        APreviousLayerOriginalMatrix: TAffineMatrix); overload;
    destructor Destroy; override;
    property ChangeImageLayer: boolean read GetChangeImageLayer;
  end;

  { TSetLayerNameStateDifference }

  TSetLayerNameStateDifference = class(TCustomImageDifference)
  private
    previousName,nextName: ansistring;
    layerId: integer;
  protected
    function GetImageDifferenceKind: TImageDifferenceKind; override;
    function GetIsIdentity: boolean; override;
  public
    constructor Create(ADestination: TState; ALayerId: integer; ANewName: ansistring);
    procedure ApplyTo(AState: TState); override;
    procedure UnapplyTo(AState: TState); override;
  end;

  { TSetLayerOpacityStateDifference }

  TSetLayerOpacityStateDifference = class(TCustomImageDifference)
  private
    previousOpacity,nextOpacity: byte;
    layerId: integer;
  protected
    function GetImageDifferenceKind: TImageDifferenceKind; override;
    function GetIsIdentity: boolean; override;
  public
    constructor Create(ADestination: TState; ALayerId: integer; ANewOpacity: byte);
    procedure ApplyTo(AState: TState); override;
    procedure UnapplyTo(AState: TState); override;
  end;

  { TSetLayerOffsetStateDifference }

  TSetLayerOffsetStateDifference = class(TCustomImageDifference)
  private
    previousOffset,nextOffset: TPoint;
    layerId: integer;
  protected
    function GetImageDifferenceKind: TImageDifferenceKind; override;
    function GetIsIdentity: boolean; override;
  public
    constructor Create(ADestination: TState; ALayerId: integer; ANewOffset: TPoint);
    procedure ApplyTo(AState: TState); override;
    procedure UnapplyTo(AState: TState); override;
  end;

  { TApplyLayerOffsetStateDifference }

  TApplyLayerOffsetStateDifference = class(TCustomImageDifference)
  private
    previousBounds,nextBounds,unchangedBounds: TRect;
    clippedData: TMemoryStream;
    useOriginal: boolean;
    previousOriginalRenderStatus: TOriginalRenderStatus;
    layerId: integer;
    FDestination: TState;
    previousLayerOffset: TPoint;
  protected
    function GetImageDifferenceKind: TImageDifferenceKind; override;
    function GetIsIdentity: boolean; override;
  public
    constructor Create(ADestination: TState; ALayerId: integer; AOffsetX, AOffsetY: integer; AApplyNow: boolean);
    destructor Destroy; override;
    procedure ApplyTo(AState: TState); override;
    procedure UnapplyTo(AState: TState); override;
  end;

  { TSetLayerVisibleStateDifference }

  TSetLayerVisibleStateDifference = class(TCustomImageDifference)
  private
    previousVisible,nextVisible: boolean;
    layerId: integer;
  protected
    function GetImageDifferenceKind: TImageDifferenceKind; override;
    function GetIsIdentity: boolean; override;
  public
    constructor Create(ADestination: TState; ALayerId: integer; ANewVisible: boolean);
    procedure ApplyTo(AState: TState); override;
    procedure UnapplyTo(AState: TState); override;
  end;

  { TSetLayerBlendOpStateDifference }

  TSetLayerBlendOpStateDifference = class(TCustomImageDifference)
  private
    previousBlendOp,nextBlendOp: TBlendOperation;
    layerId: integer;
  protected
    function GetImageDifferenceKind: TImageDifferenceKind; override;
    function GetIsIdentity: boolean; override;
  public
    constructor Create(ADestination: TState; ALayerId: integer; ANewBlendOp: TBlendOperation);
    procedure ApplyTo(AState: TState); override;
    procedure UnapplyTo(AState: TState); override;
  end;

  { TAddLayerStateDifference }

  TAddLayerStateDifference = class(TCustomImageDifference)
  protected
    function GetImageDifferenceKind: TImageDifferenceKind; override;
  public
    layerId: integer;
    content: TStoredImage;
    previousActiveLayerId: integer;
    name: ansistring;
    blendOp: TBlendOperation;
    function UsedMemory: int64; override;
    function TryCompress: boolean; override;
    procedure ApplyTo(AState: TState); override;
    procedure UnapplyTo(AState: TState); override;
    constructor Create(ADestination: TState; AContent: TBGRABitmap; AName: ansistring; ABlendOp: TBlendOperation);
    destructor Destroy; override;
  end;

  { TAddLayerFromOwnedOriginalStateDifference }

  TAddLayerFromOwnedOriginalStateDifference = class(TCustomImageDifference)
  protected
    function GetImageDifferenceKind: TImageDifferenceKind; override;
    procedure Uncompress;
  public
    layerId: integer;
    originalData: TStream;
    compressedData: TStream;
    previousActiveLayerId: integer;
    name: ansistring;
    blendOp: TBlendOperation;
    function UsedMemory: int64; override;
    function TryCompress: boolean; override;
    procedure ApplyTo(AState: TState); override;
    procedure UnapplyTo(AState: TState); override;
    constructor Create(ADestination: TState; AOriginal: TBGRALayerCustomOriginal; AName: ansistring; ABlendOp: TBlendOperation);
    destructor Destroy; override;
  end;

  { TRemoveLayerStateDifference }

  TRemoveLayerStateDifference = class(TCustomImageDifference)
  protected
    function GetImageDifferenceKind: TImageDifferenceKind; override;
  public
    content: TStoredLayer;
    nextActiveLayerId: integer;
    function UsedMemory: int64; override;
    function TryCompress: boolean; override;
    procedure ApplyTo(AState: TState); override;
    procedure UnapplyTo(AState: TState); override;
    constructor Create(AState: TState);
    destructor Destroy; override;
  end;

  { TAssignStateDifference }

  TAssignStateDifference = class(TCustomImageDifference)
  protected
    FStreamBefore, FStreamAfter: TMemoryStream;
    FSelectionDiff,FSelectionLayerDiff: TImageDiff;
    procedure Init(AState: TState; AValue: TBGRALayeredBitmap; AOwned: boolean; ASelectedLayerIndex: integer);
  public
    constructor Create(AState: TState; AValue: TBGRALayeredBitmap; AOwned: boolean; ASelectedLayerIndex: integer);
    constructor Create(AState: TState; AValue: TBGRALayeredBitmap; AOwned: boolean; ASelectedLayerIndex: integer; ACurrentSelection: TBGRABitmap; ASelectionLayer: TBGRABitmap);
    function UsedMemory: int64; override;
    function TryCompress: boolean; override;
    procedure ApplyTo(AState: TState); override;
    procedure UnApplyTo(AState: TState); override;
    destructor Destroy; override;
  end;

  { TAssignStateDifferenceAfter }

  TAssignStateDifferenceAfter = class(TAssignStateDifference)
  public
    constructor Create(AState: TState; ABackup: TState);
  end;

  { TDuplicateLayerStateDifference }

  TDuplicateLayerStateDifference = class(TCustomImageDifference)
  protected
    function GetImageDifferenceKind: TImageDifferenceKind; override;
  public
    sourceLayerId: integer;
    duplicateId: integer;
    procedure ApplyTo(AState: TState); override;
    procedure UnapplyTo(AState: TState); override;
    constructor Create(ADestination: TState);
  end;

  { TMoveLayerStateDifference }

  TMoveLayerStateDifference = class(TCustomImageDifference)
  protected
    function GetIsIdentity: boolean; override;
    function GetImageDifferenceKind: TImageDifferenceKind; override;
  public
    sourceIndex,destIndex: integer;
    procedure ApplyTo(AState: TState); override;
    procedure UnapplyTo(AState: TState); override;
    constructor Create(ADestination: TState; AFromIndex, AToIndex: integer);
  end;

  { TMergeLayerOverStateDifference }

  TMergeLayerOverStateDifference = class(TCustomImageDifference)
  protected
    function GetImageDifferenceKind: TImageDifferenceKind; override;
  public
    previousActiveLayerId: integer;
    layerOverIndex: integer;
    layerOverCompressedBackup: TStoredLayer;
    layerUnderCompressedBackup: TStoredLayer;
    constructor Create(ADestination: TState; ALayerOverIndex: integer);
    function UsedMemory: int64; override;
    function TryCompress: boolean; override;
    procedure ApplyTo(AState: TState); override;
    procedure UnapplyTo(AState: TState); override;
    destructor Destroy; override;
  end;

implementation

uses BGRAWriteLzp, BGRAReadLzp, UImageState, BGRAStreamLayers, BGRALzpCommon, ugraph, Types,
  BGRATransform, zstream;

function IsInverseImageDiff(ADiff1, ADiff2: TCustomImageDifference): boolean;
begin
  if (ADiff1 is TInversibleStateDifference) and (ADiff2 is TInversibleStateDifference) then
    result := ((ADiff1 as TInversibleStateDifference).Action = GetInverseAction( (ADiff2 as TInversibleStateDifference).Action ))
          and ((ADiff1 as TInversibleStateDifference).LayerIndex = (ADiff2 as TInversibleStateDifference).LayerIndex)
  else
  if (ADiff1 is TSetLayerNameStateDifference) and (ADiff2 is TSetLayerNameStateDifference) then
  begin
    result := ((ADiff1 as TSetLayerNameStateDifference).nextName = (ADiff2 as TSetLayerNameStateDifference).previousName) and
      ((ADiff1 as TSetLayerNameStateDifference).previousName = (ADiff2 as TSetLayerNameStateDifference).nextName);
  end
  else
  if (ADiff1 is TSetLayerVisibleStateDifference) and (ADiff2 is TSetLayerVisibleStateDifference) then
  begin
    result := ((ADiff1 as TSetLayerVisibleStateDifference).nextVisible = (ADiff2 as TSetLayerVisibleStateDifference).previousVisible) and
      ((ADiff1 as TSetLayerVisibleStateDifference).previousVisible = (ADiff2 as TSetLayerVisibleStateDifference).nextVisible);
  end
  else
  if (ADiff1 is TSetLayerOpacityStateDifference) and (ADiff2 is TSetLayerOpacityStateDifference) then
  begin
    result := ((ADiff1 as TSetLayerOpacityStateDifference).nextOpacity = (ADiff2 as TSetLayerOpacityStateDifference).previousOpacity) and
      ((ADiff1 as TSetLayerOpacityStateDifference).previousOpacity = (ADiff2 as TSetLayerOpacityStateDifference).nextOpacity);
  end
  else
  if (ADiff1 is TSetLayerBlendOpStateDifference) and (ADiff2 is TSetLayerBlendOpStateDifference) then
  begin
    result := ((ADiff1 as TSetLayerBlendOpStateDifference).nextBlendOp = (ADiff2 as TSetLayerBlendOpStateDifference).previousBlendOp) and
      ((ADiff1 as TSetLayerBlendOpStateDifference).previousBlendOp = (ADiff2 as TSetLayerBlendOpStateDifference).nextBlendOp);
  end
  else
    result := false;
end;

function TryCombineImageDiff(ANewDiff, APrevDiff: TCustomImageDifference): boolean;
var
  combined: TInversibleAction;
begin
  if (APrevDiff is TInversibleStateDifference) and (ANewDiff is TInversibleStateDifference) then
  begin
    if CanCombineInversibleAction((APrevDiff as TInversibleStateDifference).Action, (ANewDiff as TInversibleStateDifference).Action, combined) then
    begin
      (APrevDiff as TInversibleStateDifference).Action := combined;
      result := true;
    end
    else result := false;
  end
  else
  if (APrevDiff is TSetLayerNameStateDifference) and (ANewDiff is TSetLayerNameStateDifference) then
  begin
    if (APrevDiff as TSetLayerNameStateDifference).nextName = (ANewDiff as TSetLayerNameStateDifference).previousName then
    begin
      (APrevDiff as TSetLayerNameStateDifference).nextName := (ANewDiff as TSetLayerNameStateDifference).nextName;
      result := true;
    end
    else result := false;
  end
  else
  if (APrevDiff is TSetLayerOpacityStateDifference) and (ANewDiff is TSetLayerOpacityStateDifference) then
  begin
    if (APrevDiff as TSetLayerOpacityStateDifference).nextOpacity = (ANewDiff as TSetLayerOpacityStateDifference).previousOpacity then
    begin
      (APrevDiff as TSetLayerOpacityStateDifference).nextOpacity := (ANewDiff as TSetLayerOpacityStateDifference).nextOpacity;
      result := true;
    end
    else result := false;
  end
  else
  if (APrevDiff is TSetLayerOffsetStateDifference) and (ANewDiff is TSetLayerOffsetStateDifference) then
  begin
    if ((APrevDiff as TSetLayerOffsetStateDifference).nextOffset.x = (ANewDiff as TSetLayerOffsetStateDifference).previousOffset.x)
    and ((APrevDiff as TSetLayerOffsetStateDifference).nextOffset.y = (ANewDiff as TSetLayerOffsetStateDifference).previousOffset.y) then
    begin
      (APrevDiff as TSetLayerOffsetStateDifference).nextOffset := (ANewDiff as TSetLayerOffsetStateDifference).nextOffset;
      result := true;
    end
    else result := false;
  end
  else
  if (APrevDiff is TSetLayerBlendOpStateDifference) and (ANewDiff is TSetLayerBlendOpStateDifference) then
  begin
    if (APrevDiff as TSetLayerBlendOpStateDifference).nextBlendOp = (ANewDiff as TSetLayerBlendOpStateDifference).previousBlendOp then
    begin
      (APrevDiff as TSetLayerBlendOpStateDifference).nextBlendOp := (ANewDiff as TSetLayerBlendOpStateDifference).nextBlendOp;
      result := true;
    end
    else result := false;
  end
  else
    result := false;
end;

{ TAddLayerFromOwnedOriginalStateDifference }

function TAddLayerFromOwnedOriginalStateDifference.GetImageDifferenceKind: TImageDifferenceKind;
begin
  Result:= idkChangeImage;
end;

procedure TAddLayerFromOwnedOriginalStateDifference.Uncompress;
var
  decompression: Tdecompressionstream;
  uncompressedSize: Int64;
begin
  if Assigned(compressedData) and not Assigned(originalData) then
  begin
    originalData:= TMemoryStream.Create;
    compressedData.Position := 0;
    uncompressedSize:= 0;
    compressedData.ReadBuffer(uncompressedSize, sizeof(uncompressedSize));
    decompression := Tdecompressionstream.Create(compressedData, true);
    originalData.CopyFrom(decompression, uncompressedSize);
    decompression.Free;
    FreeAndNil(compressedData);
  end
end;

function TAddLayerFromOwnedOriginalStateDifference.UsedMemory: int64;
begin
  if Assigned(originalData) then
    result := originalData.Size
  else
  if Assigned(compressedData) then
    result := compressedData.Size
  else
    result := 0;
end;

function TAddLayerFromOwnedOriginalStateDifference.TryCompress: boolean;
var
  compression: Tcompressionstream;
  uncompressedSize: Int64;
begin
  if Assigned(originalData) and not Assigned(compressedData) then
  begin
    compressedData:= TMemoryStream.Create;
    uncompressedSize := originalData.Size;
    compressedData.WriteBuffer(uncompressedSize, sizeof(uncompressedSize));
    compression := Tcompressionstream.Create(cldefault, compressedData, true);
    originalData.Position:= 0;
    compression.CopyFrom(originalData, originalData.Size);
    compression.Free;
    FreeAndNil(originalData);
    result := true;
  end
  else
    result := false;
end;

procedure TAddLayerFromOwnedOriginalStateDifference.ApplyTo(AState: TState);
var idx, origIdx: integer;
begin
  inherited ApplyTo(AState);
  Uncompress;
  if not Assigned(originalData) then
    raise exception.Create('Original data missing');

  with AState as TImageState do
  begin
    originalData.Position:= 0;
    origIdx:= currentLayeredBitmap.AddOriginalFromStream(originalData);
    idx := currentLayeredBitmap.AddLayerFromOriginal(currentLayeredBitmap.Original[origIdx].Guid, self.blendOp);
    currentLayeredBitmap.LayerUniqueId[idx] := self.layerId;
    currentLayeredBitmap.LayerName[idx] := name;
    currentLayerIndex := idx;
  end;
end;

procedure TAddLayerFromOwnedOriginalStateDifference.UnapplyTo(AState: TState);
var idx: integer;
begin
  inherited UnapplyTo(AState);
  with AState as TImageState do
  begin
    idx := currentLayeredBitmap.GetLayerIndexFromId(self.layerId);
    currentLayeredBitmap.RemoveLayer(idx);
    currentLayerIndex := currentLayeredBitmap.GetLayerIndexFromId(self.previousActiveLayerId);
  end;
end;

constructor TAddLayerFromOwnedOriginalStateDifference.Create(ADestination: TState;
  AOriginal: TBGRALayerCustomOriginal; AName: ansistring; ABlendOp: TBlendOperation);
var idx: integer;
  imgDest: TImageState;
begin
  inherited Create(ADestination);
  imgDest := ADestination as TImageState;
  if imgDest.currentLayeredBitmap = nil then
    raise exception.Create('Layered bitmap not created');

  self.originalData := TMemoryStream.Create;
  AOriginal.SaveToStream(originalData);

  self.name := AName;
  self.blendOp:= AblendOp;
  self.previousActiveLayerId := imgDest.currentLayeredBitmap.LayerUniqueId[imgDest.currentLayerIndex];
  idx := imgDest.currentLayeredBitmap.AddLayerFromOwnedOriginal(AOriginal, ABlendOp);
  imgDest.currentLayeredBitmap.LayerName[idx] := name;
  self.layerId := imgDest.currentLayeredBitmap.LayerUniqueId[idx];
  imgDest.currentLayerIndex := idx;
end;

destructor TAddLayerFromOwnedOriginalStateDifference.Destroy;
begin
  originalData.Free;
  compressedData.Free;
  inherited Destroy;
end;

{ TApplyLayerOffsetStateDifference }

function TApplyLayerOffsetStateDifference.GetImageDifferenceKind: TImageDifferenceKind;
begin
  Result:= idkChangeLayer;
end;

function TApplyLayerOffsetStateDifference.GetIsIdentity: boolean;
begin
  Result:= (previousBounds.Left = nextBounds.Left) and (previousBounds.Top = nextBounds.Top) and
     (previousBounds.right = nextBounds.Right) and (previousBounds.bottom = nextBounds.Bottom);
end;

constructor TApplyLayerOffsetStateDifference.Create(ADestination: TState;
  ALayerId: integer; AOffsetX, AOffsetY: integer; AApplyNow: boolean);
var idx: integer;
  layers: TBGRALayeredBitmap;
  clippedImage: TBGRABitmap;
begin
  inherited Create(ADestination);
  FDestination := ADestination;
  layerId:= ALayerId;
  layers := (FDestination as TImageState).currentLayeredBitmap;
  idx := layers.GetLayerIndexFromId(ALayerId);
  if idx = -1 then raise exception.Create('Invalid layer Id');
  nextBounds := rect(0,0,layers.Width,layers.Height);
  previousBounds.Left := AOffsetX;
  previousBounds.Top := AOffsetY;
  previousBounds.Right := previousBounds.Left+layers.LayerBitmap[idx].Width;
  previousBounds.Bottom := previousBounds.Top+layers.LayerBitmap[idx].Height;
  previousLayerOffset := layers.LayerOffset[idx];
  if IsIdentity then
  begin
    clippedData := nil;
    useOriginal := false;
    unchangedBounds := previousBounds;
  end else
  begin
    unchangedBounds := previousBounds;
    IntersectRect(unchangedBounds, unchangedBounds, nextBounds);
    OffsetRect(unchangedBounds, -AOffsetX, -AOffsetY);
    useOriginal:= (layers.LayerOriginalGuid[idx]<>GUID_NULL) and (layers.LayerOriginalKnown[idx]);
    previousOriginalRenderStatus:= layers.LayerOriginalRenderStatus[idx];

    clippedImage := layers.LayerBitmap[idx].Duplicate as TBGRABitmap;
    clippedImage.FillRect(unchangedBounds,BGRAPixelTransparent,dmSet);
    clippedData := TMemoryStream.Create;
    TBGRAWriterLazPaint.WriteRLEImage(clippedData, clippedImage);
    clippedImage.Free;
  end;
  if AApplyNow then ApplyTo(ADestination);
end;

destructor TApplyLayerOffsetStateDifference.Destroy;
begin
  FreeAndNil(clippedData);
  inherited Destroy;
end;

procedure TApplyLayerOffsetStateDifference.ApplyTo(AState: TState);
var idx: integer;
begin
  inherited ApplyTo(AState);
  if IsIdentity then exit;
  idx := (AState as TImageState).currentLayeredBitmap.GetLayerIndexFromId(layerId);
  if idx =-1 then raise exception.Create('Layer not found');
  (AState as TImageState).currentLayeredBitmap.ApplyLayerOffset(idx, true);
end;

procedure TApplyLayerOffsetStateDifference.UnapplyTo(AState: TState);
var idx: integer;
  newContent: TBGRABitmap;
  layers: TBGRALayeredBitmap;
  shifted: TRect;
  dummyCaption: ansistring;
  guid: TGuid;
  m: TAffineMatrix;
begin
  inherited ApplyTo(AState);
  if IsIdentity then exit;
  layers := (AState as TImageState).currentLayeredBitmap;
  idx := layers.GetLayerIndexFromId(layerId);
  if idx =-1 then
    raise exception.Create('Layer not found');

  newContent := TBGRABitmap.Create;
  clippedData.Position:= 0;
  TBGRAReaderLazPaint.LoadRLEImage(clippedData,newContent,dummyCaption);
  shifted := unchangedBounds;
  OffsetRect(shifted, previousBounds.left-nextBounds.left,previousBounds.top-nextBounds.top);
  newContent.PutImagePart(unchangedBounds.Left,unchangedBounds.Top, layers.LayerBitmap[idx],shifted, dmSet);
  layers.SetLayerBitmap(idx,newContent,True);
  guid := layers.LayerOriginalGuid[idx];
  m := layers.LayerOriginalMatrix[idx];
  layers.LayerOffset[idx] := previousLayerOffset;
  if useOriginal then
  begin
    layers.LayerOriginalGuid[idx] := guid;
    layers.LayerOriginalMatrix[idx] := m;
    layers.LayerOriginalRenderStatus[idx] := previousOriginalRenderStatus;
  end;
end;

{ TSetLayerOffsetStateDifference }

function TSetLayerOffsetStateDifference.GetImageDifferenceKind: TImageDifferenceKind;
begin
  Result:= idkChangeImage;
end;

function TSetLayerOffsetStateDifference.GetIsIdentity: boolean;
begin
  Result:=(previousOffset.x = nextOffset.x) and (previousOffset.y = nextOffset.y);
end;

constructor TSetLayerOffsetStateDifference.Create(ADestination: TState;
  ALayerId: integer; ANewOffset: TPoint);
var idx: integer;
  imgDest: TImageState;
begin
  inherited Create(Adestination);
  imgDest := ADestination as TImageState;
  layerId:= ALayerId;
  nextOffset:= ANewOffset;
  idx := imgDest.currentLayeredBitmap.GetLayerIndexFromId(ALayerId);
  if idx =-1 then
    raise exception.Create('Layer not found');
  previousOffset:= imgDest.LayerOffset[idx];
  ApplyTo(imgDest);
end;

procedure TSetLayerOffsetStateDifference.ApplyTo(AState: TState);
var idx: integer;
begin
  inherited ApplyTo(AState);
  idx := TImageState(AState).currentLayeredBitmap.GetLayerIndexFromId(layerId);
  if idx =-1 then
    raise exception.Create('Layer not found');
  TImageState(AState).currentLayeredBitmap.LayerOffset[idx] := nextOffset;
end;

procedure TSetLayerOffsetStateDifference.UnapplyTo(AState: TState);
var idx: integer;
begin
  inherited UnapplyTo(AState);
  idx := TImageState(AState).currentLayeredBitmap.GetLayerIndexFromId(layerId);
  if idx =-1 then
    raise exception.Create('Layer not found');
  TImageState(AState).currentLayeredBitmap.LayerOffset[idx] := previousOffset;
end;

{ TSetLayerBlendOpStateDifference }

function TSetLayerBlendOpStateDifference.GetImageDifferenceKind: TImageDifferenceKind;
begin
  Result:= idkChangeLayer;
end;

function TSetLayerBlendOpStateDifference.GetIsIdentity: boolean;
begin
  Result:=previousBlendOp = nextBlendOp;
end;

constructor TSetLayerBlendOpStateDifference.Create(ADestination: TState;
  ALayerId: integer; ANewBlendOp: TBlendOperation);
var idx: integer;
  imgDest: TImageState;
begin
  inherited Create(Adestination);
  imgDest := ADestination as TImageState;
  layerId:= ALayerId;
  nextBlendOp:= ANewBlendOp;
  idx := imgDest.currentLayeredBitmap.GetLayerIndexFromId(ALayerId);
  if idx =-1 then
    raise exception.Create('Layer not found');
  previousBlendOp:= imgDest.BlendOperation[idx];
  ApplyTo(imgDest);
end;

procedure TSetLayerBlendOpStateDifference.ApplyTo(AState: TState);
var idx: integer;
begin
  inherited ApplyTo(AState);
  idx := TImageState(AState).currentLayeredBitmap.GetLayerIndexFromId(layerId);
  if idx =-1 then
    raise exception.Create('Layer not found');
  TImageState(AState).currentLayeredBitmap.BlendOperation[idx] := nextBlendOp;
end;

procedure TSetLayerBlendOpStateDifference.UnapplyTo(AState: TState);
var idx: integer;
begin
  inherited UnapplyTo(AState);
  idx := TImageState(AState).currentLayeredBitmap.GetLayerIndexFromId(layerId);
  if idx =-1 then
    raise exception.Create('Layer not found');
  TImageState(AState).currentLayeredBitmap.BlendOperation[idx] := previousBlendOp;
end;

{ TSetLayerVisibleStateDifference }

function TSetLayerVisibleStateDifference.GetImageDifferenceKind: TImageDifferenceKind;
begin
  Result:= idkChangeLayer;
end;

function TSetLayerVisibleStateDifference.GetIsIdentity: boolean;
begin
  Result:= previousVisible=nextVisible;
end;

constructor TSetLayerVisibleStateDifference.Create(ADestination: TState;
  ALayerId: integer; ANewVisible: boolean);
var idx: integer;
  imgDest: TImageState;
begin
  inherited Create(Adestination);
  imgDest := ADestination as TImageState;
  layerId:= ALayerId;
  nextVisible:= ANewVisible;
  idx := imgDest.currentLayeredBitmap.GetLayerIndexFromId(ALayerId);
  if idx =-1 then
    raise exception.Create('Layer not found');
  previousVisible:= imgDest.LayerVisible[idx];
  ApplyTo(imgDest);
end;

procedure TSetLayerVisibleStateDifference.ApplyTo(AState: TState);
var idx: integer;
begin
  inherited ApplyTo(AState);
  idx := TImageState(AState).currentLayeredBitmap.GetLayerIndexFromId(layerId);
  if idx =-1 then
    raise exception.Create('Layer not found');
  TImageState(AState).currentLayeredBitmap.LayerVisible[idx] := nextVisible;
end;

procedure TSetLayerVisibleStateDifference.UnapplyTo(AState: TState);
var idx: integer;
begin
  inherited UnapplyTo(AState);
  idx := TImageState(AState).currentLayeredBitmap.GetLayerIndexFromId(layerId);
  if idx =-1 then
    raise exception.Create('Layer not found');
  TImageState(AState).currentLayeredBitmap.LayerVisible[idx] := previousVisible;
end;

{ TSetLayerOpacityStateDifference }

function TSetLayerOpacityStateDifference.GetImageDifferenceKind: TImageDifferenceKind;
begin
  Result:= idkChangeLayer;
end;

function TSetLayerOpacityStateDifference.GetIsIdentity: boolean;
begin
  Result:= (previousOpacity=nextOpacity);
end;

constructor TSetLayerOpacityStateDifference.Create(ADestination: TState;
  ALayerId: integer; ANewOpacity: byte);
var idx: integer;
  imgDest: TImageState;
begin
  inherited Create(Adestination);
  imgDest := ADestination as TImageState;
  layerId:= ALayerId;
  nextOpacity:= ANewOpacity;
  idx := imgDest.currentLayeredBitmap.GetLayerIndexFromId(ALayerId);
  if idx =-1 then
    raise exception.Create('Layer not found');
  previousOpacity:= imgDest.LayerOpacity[idx];
  ApplyTo(imgDest);
end;

procedure TSetLayerOpacityStateDifference.ApplyTo(AState: TState);
var idx: integer;
begin
  inherited ApplyTo(AState);
  idx := TImageState(AState).currentLayeredBitmap.GetLayerIndexFromId(layerId);
  if idx =-1 then
    raise exception.Create('Layer not found');
  TImageState(AState).currentLayeredBitmap.LayerOpacity[idx] := nextOpacity;
end;

procedure TSetLayerOpacityStateDifference.UnapplyTo(AState: TState);
var idx: integer;
begin
  inherited UnapplyTo(AState);
  idx := TImageState(AState).currentLayeredBitmap.GetLayerIndexFromId(layerId);
  if idx =-1 then
    raise exception.Create('Layer not found');
  TImageState(AState).currentLayeredBitmap.LayerOpacity[idx] := previousOpacity;
end;

{ TSetLayerNameStateDifference }

function TSetLayerNameStateDifference.GetImageDifferenceKind: TImageDifferenceKind;
begin
  Result:= idkChangeStack;
end;

function TSetLayerNameStateDifference.GetIsIdentity: boolean;
begin
  Result:= (previousName=nextName);
end;

constructor TSetLayerNameStateDifference.Create(ADestination: TState;
  ALayerId: integer; ANewName: ansistring);
var idx: integer;
  imgDest: TImageState;
begin
  inherited Create(Adestination);
  imgDest := ADestination as TImageState;
  layerId:= ALayerId;
  nextName:= ANewName;
  idx := imgDest.currentLayeredBitmap.GetLayerIndexFromId(ALayerId);
  if idx =-1 then
    raise exception.Create('Layer not found');
  previousName:= imgDest.LayerName[idx];
  ApplyTo(imgDest);
end;

procedure TSetLayerNameStateDifference.ApplyTo(AState: TState);
var idx: integer;
begin
  inherited ApplyTo(AState);
  idx := TImageState(AState).currentLayeredBitmap.GetLayerIndexFromId(layerId);
  if idx =-1 then
    raise exception.Create('Layer not found');
  TImageState(AState).currentLayeredBitmap.LayerName[idx] := nextName;
end;

procedure TSetLayerNameStateDifference.UnapplyTo(AState: TState);
var idx: integer;
begin
  inherited ApplyTo(AState);
  idx := TImageState(AState).currentLayeredBitmap.GetLayerIndexFromId(layerId);
  if idx =-1 then
    raise exception.Create('Layer not found');
  TImageState(AState).currentLayeredBitmap.LayerName[idx] := previousName;
end;

{ TAssignStateDifferenceAfter }

constructor TAssignStateDifferenceAfter.Create(AState: TState; ABackup: TState);
var imgState,imgBackup: TImageState;
begin
  imgState := AState as TImageState;
  imgBackup := ABackup as TImageState;
  FSavedBefore := imgState.saved;
  FSavedAfter := False;
  FStreamBefore := TMemoryStream.Create;
  SaveLayersToStream(FStreamBefore,imgBackup.currentLayeredBitmap,imgBackup.currentLayerIndex,lzpRLE);
  FStreamAfter := TMemoryStream.Create;
  SaveLayersToStream(FStreamAfter,imgState.currentLayeredBitmap,imgState.currentLayerIndex,lzpRLE);
  FSelectionDiff := TImageDiff.Create(imgBackup.currentSelection, imgState.currentSelection);
  FSelectionLayerDiff := TImageDiff.Create(imgBackup.selectionLayer, imgState.selectionLayer);
end;

{ TAssignStateDifference }

procedure TAssignStateDifference.Init(AState: TState; AValue: TBGRALayeredBitmap; AOwned: boolean; ASelectedLayerIndex: integer);
begin
  with AState as TImageState do
  begin
    FStreamBefore := TMemoryStream.Create;
    SaveLayersToStream(FStreamBefore,currentLayeredBitmap,currentLayerIndex,lzpRLE);
    FStreamAfter := TMemoryStream.Create;
    SaveLayersToStream(FStreamAfter,AValue,ASelectedLayerIndex,lzpRLE);
    Assign(AValue, AOwned);
    currentLayerIndex := ASelectedLayerIndex;
  end;
  FSelectionDiff := nil;
  FSelectionLayerDiff := nil;
end;

constructor TAssignStateDifference.Create(AState: TState;
  AValue: TBGRALayeredBitmap; AOwned: boolean; ASelectedLayerIndex: integer);
begin
  inherited Create(AState);
  Init(AState,AValue,AOwned,ASelectedLayerIndex);
end;

constructor TAssignStateDifference.Create(AState: TState;
  AValue: TBGRALayeredBitmap; AOwned: boolean; ASelectedLayerIndex: integer;
  ACurrentSelection: TBGRABitmap; ASelectionLayer: TBGRABitmap);
begin
  inherited Create(AState);
  Init(AState,AValue,AOwned,ASelectedLayerIndex);
  FSelectionDiff := TImageDiff.Create((AState as TImageState).currentSelection, ACurrentSelection);
  FSelectionLayerDiff := TImageDiff.Create((AState as TImageState).selectionLayer, ASelectionLayer);
  BGRAReplace((AState as TImageState).currentSelection, ACurrentSelection);
  BGRAReplace((AState as TImageState).selectionLayer, ASelectionLayer);
end;

function TAssignStateDifference.UsedMemory: int64;
begin
  Result:= 0;
  if Assigned(FStreamBefore) then result += FStreamBefore.Size;
  if Assigned(FStreamAfter) then result += FStreamAfter.Size;
  if Assigned(FSelectionDiff) then result += FSelectionDiff.UsedMemory;
  if Assigned(FSelectionLayerDiff) then result += FSelectionLayerDiff.UsedMemory;
end;

function TAssignStateDifference.TryCompress: boolean;
begin
  result := false;
  if Assigned(FSelectionDiff) then result := result or FSelectionDiff.Compress;
  if not result and Assigned(FSelectionLayerDiff) then result := result or FSelectionLayerDiff.Compress;
end;

procedure TAssignStateDifference.ApplyTo(AState: TState);
var temp: TBGRALayeredBitmap;
  index: integer;
begin
  inherited ApplyTo(AState);
  FStreamAfter.Position:= 0;
  temp := LoadLayersFromStream(FStreamAfter, index, True);
  (AState as TImageState).Assign(temp, True);
  (AState as TImageState).currentLayerIndex := index;
  ApplyImageDiffAndReplace((AState as TImageState).currentSelection, FSelectionDiff, False);
  ApplyImageDiffAndReplace((AState as TImageState).selectionLayer, FSelectionLayerDiff, False);
end;

procedure TAssignStateDifference.UnApplyTo(AState: TState);
var temp: TBGRALayeredBitmap;
  index: integer;
begin
  inherited UnapplyTo(AState);
  FStreamBefore.Position:= 0;
  temp := LoadLayersFromStream(FStreamBefore, index, True);
  (AState as TImageState).Assign(temp, True);
  (AState as TImageState).currentLayerIndex := index;
  ApplyImageDiffAndReplace((AState as TImageState).currentSelection, FSelectionDiff, true);
  ApplyImageDiffAndReplace((AState as TImageState).selectionLayer, FSelectionLayerDiff, true);
end;

destructor TAssignStateDifference.Destroy;
begin
  FStreamBefore.Free;
  FStreamAfter.Free;
  FSelectionDiff.Free;
  FSelectionLayerDiff.Free;
  inherited Destroy;
end;

{ TInversibleStateDifference }

constructor TInversibleStateDifference.Create(AState: TState;
  AAction: TInversibleAction; ALayerIndex : integer = -1);
begin
  inherited Create(AState);
  FAction := AAction;
  FLayerIndex:= ALayerIndex;
  ApplyTo(AState);
end;

procedure TInversibleStateDifference.ApplyTo(AState: TState);
begin
  inherited ApplyTo(AState);
  ApplyAction(AState as TImageState, FAction, False);
end;

procedure TInversibleStateDifference.UnApplyTo(AState: TState);
begin
  inherited UnapplyTo(AState);
  ApplyAction(AState as TImageState, FAction, True);
end;

procedure TInversibleStateDifference.ApplyAction(AState: TState;
  AAction: TInversibleAction; AInverse: boolean);
var i: integer;
  imgState: TImageState;
begin
  imgState := AState as TImageState;
  if AInverse then AAction := GetInverseAction(AAction);
  case AAction of
  iaSwapRedBlue,iaLinearNegative:
    begin
      for i := 0 to imgState.NbLayers-1 do
        if imgState.LayerOriginalDefined[i] then
          raise exception.Create('Cannot do an inversible raster action with layer originals');
      case AAction of
        iaSwapRedBlue: for i := 0 to imgState.NbLayers-1 do imgState.LayerBitmap[i].SwapRedBlue;
        iaLinearNegative: for i := 0 to imgState.NbLayers-1 do imgState.LayerBitmap[i].LinearNegative;
      else
        raise exception.Create('Unhandled case');
      end;
    end;
  iaHorizontalFlip: imgState.currentLayeredBitmap.HorizontalFlip;
  iaHorizontalFlipLayer: imgState.currentLayeredBitmap.HorizontalFlip(FLayerIndex);
  iaVerticalFlip: imgState.currentLayeredBitmap.VerticalFlip;
  iaVerticalFlipLayer: imgState.currentLayeredBitmap.VerticalFlip(FLayerIndex);
  iaRotate180: begin
      imgState.currentLayeredBitmap.HorizontalFlip;
      imgState.currentLayeredBitmap.VerticalFlip;
    end;
  iaRotateCW: begin
      imgState.currentLayeredBitmap.RotateCW;
      if imgState.currentSelection <> nil then
        BGRAReplace(imgState.currentSelection, imgState.currentSelection.RotateCW);
      if imgState.selectionLayer <> nil then
        BGRAReplace(imgState.selectionLayer, imgState.selectionLayer.RotateCW);
    end;
  iaRotateCCW: begin
    imgState.currentLayeredBitmap.RotateCCW;
    if imgState.currentSelection <> nil then
      BGRAReplace(imgState.currentSelection, imgState.currentSelection.RotateCCW);
    if imgState.selectionLayer <> nil then
      BGRAReplace(imgState.selectionLayer, imgState.selectionLayer.RotateCCW);
    end;
  end;
end;

{ TRemoveLayerStateDifference }

function TRemoveLayerStateDifference.GetImageDifferenceKind: TImageDifferenceKind;
begin
  Result:=idkChangeImage;
end;

function TRemoveLayerStateDifference.UsedMemory: int64;
begin
  if Assigned(content) then
    result := content.UsedMemory
  else
    result := 0;
end;

function TRemoveLayerStateDifference.TryCompress: boolean;
begin
  Result:= content.Compress;
end;

procedure TRemoveLayerStateDifference.ApplyTo(AState: TState);
var idx: integer;
begin
  inherited ApplyTo(AState);
  with AState as TImageState do
  begin
    idx := currentLayeredBitmap.GetLayerIndexFromId(content.LayerId);
    currentLayeredBitmap.RemoveLayer(idx);
    currentLayerIndex := currentLayeredBitmap.GetLayerIndexFromId(self.nextActiveLayerId);
  end;
end;

procedure TRemoveLayerStateDifference.UnapplyTo(AState: TState);
var idx: integer;
begin
  inherited UnapplyTo(AState);
  with AState as TImageState do
  begin
    content.Restore(currentLayeredBitmap);
    currentLayerIndex := content.LayerIndex;
  end;
end;

constructor TRemoveLayerStateDifference.Create(AState: TState);
var idx,nextIdx: integer;
  imgState: TImageState;
begin
  inherited Create(AState);
  imgState := AState as TImageState;
  if imgState.currentLayeredBitmap = nil then
    raise exception.Create('Layered bitmap not created');
  if imgState.NbLayers = 1 then
    raise exception.Create('Impossible to remove last layer');
  idx := imgState.currentLayerIndex;
  if idx = -1 then
    raise exception.Create('No layer selected');
  self.content := TStoredLayer.Create(imgState.currentLayeredBitmap, idx);
  if idx+1 < imgState.NbLayers then
    nextIdx := idx+1 else nextIdx := idx-1;
  self.nextActiveLayerId := imgState.currentLayeredBitmap.LayerUniqueId[nextIdx];
  imgState.currentLayeredBitmap.RemoveLayer(idx);
  imgState.currentLayerIndex:= imgState.currentLayeredBitmap.GetLayerIndexFromId(self.nextActiveLayerId);
end;

destructor TRemoveLayerStateDifference.Destroy;
begin
  self.content.Free;
  inherited Destroy;
end;

{ TMergeLayerOverStateDifference }

function TMergeLayerOverStateDifference.GetImageDifferenceKind: TImageDifferenceKind;
begin
  Result:= idkChangeImage; //includes stack
end;

constructor TMergeLayerOverStateDifference.Create(ADestination: TState;
  ALayerOverIndex: integer);
var
  imgDest: TImageState;
begin
  inherited Create(ADestination);
  imgDest := ADestination as TImageState;
  if (ALayerOverIndex < 0) or (ALayerOverIndex >= imgDest.NbLayers) then
    raise exception.Create('Index out of bounds');
  if ALayerOverIndex = 0 then
    raise exception.Create('First layer cannot be merged over');

  layerOverIndex := ALayerOverIndex;
  with imgDest.currentLayeredBitmap do
  begin
    previousActiveLayerId:= LayerUniqueId[imgDest.currentLayerIndex];
    layerOverCompressedBackup := TStoredLayer.Create(imgDest.currentLayeredBitmap, ALayerOverIndex);
    layerUnderCompressedBackup := TStoredLayer.Create(imgDest.currentLayeredBitmap, ALayerOverIndex-1);
  end;

  //select layer under and merge
  ApplyTo(imgDest);
end;

function TMergeLayerOverStateDifference.UsedMemory: int64;
begin
  Result:=0;
  if Assigned(layerOverCompressedBackup) then result += layerOverCompressedBackup.UsedMemory;
  if Assigned(layerUnderCompressedBackup) then result += layerUnderCompressedBackup.UsedMemory;
end;

function TMergeLayerOverStateDifference.TryCompress: boolean;
begin
  result := layerOverCompressedBackup.Compress or layerUnderCompressedBackup.Compress;
end;

procedure TMergeLayerOverStateDifference.ApplyTo(AState: TState);
var
  merged: TBGRABitmap;
begin
  inherited ApplyTo(AState);
  with AState as TImageState do
  begin
    if layerOverIndex >= NbLayers then exit;

     currentLayerIndex := layerOverIndex-1;
     if (LayerBitmap[layerOverIndex-1].Width <> Width) or
        (LayerBitmap[layerOverIndex-1].Height <> Height) or
        (LayerOffset[layerOverIndex-1].X <> 0) or
        (LayerOffset[layerOverIndex-1].Y <> 0) then
     begin
       merged := TBGRABitmap.Create(Width,Height);
       merged.PutImage(LayerOffset[layerOverIndex-1].X,LayerOffset[layerOverIndex-1].Y,LayerBitmap[layerOverIndex-1],dmSet);
       merged.BlendImageOver(LayerOffset[layerOverIndex].X,LayerOffset[layerOverIndex].Y,LayerBitmap[layerOverIndex],
                             BlendOperation[layerOverIndex],LayerOpacity[layerOverIndex],LinearBlend);
       currentLayeredBitmap.SetLayerBitmap(layerOverIndex-1, merged,true);
       currentLayeredBitmap.LayerOffset[layerOverIndex-1] := Point(0,0);
     end else
     begin
       currentLayeredBitmap.LayerOriginalGuid[layerOverIndex-1] := GUID_NULL;
       currentLayeredBitmap.LayerOriginalMatrix[layerOverIndex-1] := AffineMatrixIdentity;
       LayerBitmap[layerOverIndex-1].BlendImageOver(LayerOffset[layerOverIndex].X,LayerOffset[layerOverIndex].Y,LayerBitmap[layerOverIndex],
                             BlendOperation[layerOverIndex],LayerOpacity[layerOverIndex],LinearBlend);
     end;
     currentLayeredBitmap.RemoveLayer(layerOverIndex);
     currentLayeredBitmap.RemoveUnusedOriginals;
  end;
end;

procedure TMergeLayerOverStateDifference.UnapplyTo(AState: TState);
begin
  inherited UnapplyTo(AState);
  with AState as TImageState do
  begin
    layerOverCompressedBackup.Restore(currentLayeredBitmap);
    layerUnderCompressedBackup.Replace(currentLayeredBitmap);

    //select previous layer
    currentLayerIndex := currentLayeredBitmap.GetLayerIndexFromId(Self.previousActiveLayerId);
  end;
end;

destructor TMergeLayerOverStateDifference.Destroy;
begin
  layerOverCompressedBackup.Free;
  layerUnderCompressedBackup.Free;
  inherited Destroy;
end;

{ TMoveLayerStateDifference }

function TMoveLayerStateDifference.GetIsIdentity: boolean;
begin
  Result:= (sourceIndex = destIndex);
end;

function TMoveLayerStateDifference.GetImageDifferenceKind: TImageDifferenceKind;
begin
  Result:=idkChangeImage; //includes stack
end;

procedure TMoveLayerStateDifference.ApplyTo(AState: TState);
begin
  inherited ApplyTo(AState);
  with AState as TImageState do
    currentLayeredBitmap.InsertLayer(destIndex, sourceIndex);
end;

procedure TMoveLayerStateDifference.UnapplyTo(AState: TState);
begin
  inherited UnapplyTo(AState);
  with AState as TImageState do
    currentLayeredBitmap.InsertLayer(sourceIndex, destIndex);
end;

constructor TMoveLayerStateDifference.Create(ADestination: TState;
  AFromIndex, AToIndex: integer);
begin
  inherited Create(ADestination);
  self.sourceIndex := AFromIndex;
  self.destIndex := AToIndex;
  ApplyTo(ADestination);
end;

{ TDuplicateLayerStateDifference }

function TDuplicateLayerStateDifference.GetImageDifferenceKind: TImageDifferenceKind;
begin
  Result:=idkChangeImage;
end;

procedure TDuplicateLayerStateDifference.ApplyTo(AState: TState);
var sourceLayerIndex,duplicateIndex: integer;
  copy: integer;
begin
  inherited ApplyTo(AState);
  with AState as TImageState do
  begin
    sourceLayerIndex := currentLayeredBitmap.GetLayerIndexFromId(self.sourceLayerId);
    duplicateIndex := sourceLayerIndex+1;
    with currentLayeredBitmap do
    begin
      copy := AddLayer(LayerBitmap[sourceLayerIndex],BlendOperation[sourceLayerIndex],LayerOpacity[sourceLayerIndex]);
      LayerName[copy] := LayerName[sourceLayerIndex];
      LayerOffset[copy] := LayerOffset[sourceLayerIndex];
      LayerVisible[copy] := LayerVisible[sourceLayerIndex];
      LayerUniqueId[copy] := duplicateId;
      InsertLayer(duplicateIndex, copy);
    end;
    currentLayerIndex := duplicateIndex;
  end;
end;

procedure TDuplicateLayerStateDifference.UnapplyTo(AState: TState);
var sourceLayerIndex,duplicateIndex: integer;
begin
  inherited UnapplyTo(AState);
  with AState as TImageState do
  begin
    sourceLayerIndex := currentLayeredBitmap.GetLayerIndexFromId(self.sourceLayerId);
    duplicateIndex := currentLayeredBitmap.GetLayerIndexFromId(self.duplicateId);
    currentLayeredBitmap.RemoveLayer(duplicateIndex);
    currentLayerIndex := sourceLayerIndex;
  end;
end;

constructor TDuplicateLayerStateDifference.Create(ADestination: TState);
var imgDest: TImageState;
begin
  inherited Create(ADestination);
  imgDest := ADestination as TImageState;
  with imgDest do
  begin
    self.sourceLayerId := currentLayeredBitmap.LayerUniqueId[currentLayerIndex];
    self.duplicateId := currentLayeredBitmap.ProduceLayerUniqueId;
  end;
  ApplyTo(imgDest);
end;

{ TAddLayerStateDifference }

function TAddLayerStateDifference.GetImageDifferenceKind: TImageDifferenceKind;
begin
  Result:= idkChangeImage;
end;

function TAddLayerStateDifference.UsedMemory: int64;
begin
  if Assigned(content) then
    result := content.UsedMemory
  else
    result := 0;
end;

function TAddLayerStateDifference.TryCompress: boolean;
begin
  if Assigned(content) then
    Result := content.Compress
  else
    result := false;
end;

procedure TAddLayerStateDifference.ApplyTo(AState: TState);
var idx: integer;
  bmp: TBGRABitmap;
begin
  inherited ApplyTo(AState);
  with AState as TImageState do
  begin
    bmp := content.GetBitmap;
    if bmp = nil then
      raise exception.Create('Bitmap not found');
    idx := currentLayeredBitmap.AddOwnedLayer(bmp);
    currentLayeredBitmap.LayerUniqueId[idx] := self.layerId;
    currentLayeredBitmap.LayerName[idx] := name;
    currentLayeredBitmap.BlendOperation[idx] := self.blendOp;
    currentLayerIndex := idx;
  end;
end;

procedure TAddLayerStateDifference.UnapplyTo(AState: TState);
var idx: integer;
begin
  inherited UnapplyTo(AState);
  with AState as TImageState do
  begin
    idx := currentLayeredBitmap.GetLayerIndexFromId(self.layerId);
    currentLayeredBitmap.RemoveLayer(idx);
    currentLayerIndex := currentLayeredBitmap.GetLayerIndexFromId(self.previousActiveLayerId);
  end;
end;

constructor TAddLayerStateDifference.Create(ADestination: TState;
  AContent: TBGRABitmap; AName: ansistring; ABlendOp: TBlendOperation);
var idx: integer;
  imgDest: TImageState;
begin
  inherited Create(ADestination);
  imgDest := ADestination as TImageState;
  if imgDest.currentLayeredBitmap = nil then
    raise exception.Create('Layered bitmap not created');
  self.content := TStoredImage.Create(AContent);
  self.name := AName;
  self.blendOp:= AblendOp;
  self.previousActiveLayerId := imgDest.currentLayeredBitmap.LayerUniqueId[imgDest.currentLayerIndex];
  idx := imgDest.currentLayeredBitmap.AddLayer(AContent, ABlendOp);
  imgDest.currentLayeredBitmap.LayerName[idx] := name;
  self.layerId := imgDest.currentLayeredBitmap.LayerUniqueId[idx];
  imgDest.currentLayerIndex := idx;
end;

destructor TAddLayerStateDifference.Destroy;
begin
  self.content.Free;
  inherited Destroy;
end;

{ TImageLayerStateDifference }

function TImageLayerStateDifference.GetChangeImageLayer: boolean;
begin
  result := imageDiff <> nil;
end;

function TImageLayerStateDifference.GetImageDifferenceKind: TImageDifferenceKind;
begin
  if (imageDiff <> nil) or (selectionLayerDiff <> nil) then
  begin
    if selectionDiff <> nil then
      result := idkChangeImageAndSelection
    else if selectionLayerDiff <> nil then
      result := idkChangeImage
    else
      result := idkChangeLayer;
  end
  else if selectionDiff <> nil then
    result := idkChangeSelection
  else
    result := idkChangeStack; //some default value
end;

function TImageLayerStateDifference.GetIsIdentity: boolean;
begin
  Result:= ((imageDiff= nil) or imageDiff.IsIdentity) and
    ((selectionDiff= nil) or selectionDiff.IsIdentity) and
    ((selectionLayerDiff= nil) or selectionLayerDiff.IsIdentity);
end;

function TImageLayerStateDifference.GetChangingBoundsDefined: boolean;
begin
  Result:=true;
end;

function TImageLayerStateDifference.GetChangingBounds: TRect;
begin
  result := EmptyRect;
  if imageDiff <> nil then result := RectUnion(result, imageDiff.ChangeRect);
  if selectionLayerDiff <> nil then result := RectUnion(result, selectionLayerDiff.ChangeRect);
  if selectionDiff <> nil then result := RectUnion(result, selectionDiff.ChangeRect);
end;

procedure TImageLayerStateDifference.Init(AToState: TState; APreviousImage: TBGRABitmap; APreviousImageChangeRect: TRect;
        APreviousSelection: TBGRABitmap; APreviousSelectionChangeRect: TRect;
        APreviousSelectionLayer: TBGRABitmap; APreviousSelectionLayerChangeRect: TRect;
        APreviousLayerOriginalData: TStream;
        APreviousLayerOriginalMatrix: TAffineMatrix);
var
  next: TImageState;
  curIdx: integer;
  nextOrig: TBGRALayerCustomOriginal;
begin
  inherited Create((AToState as TImageState).saved,false);
  layerId := -1;
  imageDiff := nil;
  selectionDiff := nil;
  selectionLayerDiff := nil;
  prevLayerOriginalData := nil;
  nextLayerOriginalData := nil;
  layerOriginalChange:= false;

  next := AToState as TImageState;
  layerId := next.selectedLayerId;
  curIdx := next.currentLayeredBitmap.GetLayerIndexFromId(LayerId);
  if curIdx = -1 then
  begin
    layerId:= -1;
    prevLayerOriginalMatrix := AffineMatrixIdentity;
    nextLayerOriginalMatrix := AffineMatrixIdentity;
    APreviousLayerOriginalData.Free;
  end
  else
  begin
    if not IsRectEmpty(APreviousImageChangeRect) then
      imageDiff := TImageDiff.Create(APreviousImage,next.LayerBitmap[curIdx],APreviousImageChangeRect);
    if not IsRectEmpty(APreviousSelectionChangeRect) then
      selectionDiff := TGrayscaleImageDiff.Create(APreviousSelection,next.currentSelection,APreviousSelectionChangeRect);
    if not IsRectEmpty(APreviousSelectionLayerChangeRect) then
      selectionLayerDiff := TImageDiff.Create(APreviousSelectionLayer,next.selectionLayer,APreviousSelectionLayerChangeRect);
    prevLayerOriginalMatrix := APreviousLayerOriginalMatrix;
    nextLayerOriginalMatrix := next.LayerOriginalMatrix[curIdx];
    nextOrig := next.LayerOriginal[curIdx];
    //only case handled here: when going from original to raster bitmap
    if APreviousLayerOriginalData <> nil then
    begin
      layerOriginalChange:= true;
      prevLayerOriginalData := APreviousLayerOriginalData;
      if Assigned(nextOrig) then
      begin
        nextLayerOriginalData := TMemoryStream.Create;
        nextOrig.SaveToStream(nextLayerOriginalData);
      end;
    end else
      APreviousLayerOriginalData.Free;
  end;
end;

function TImageLayerStateDifference.TryCompress: boolean;
begin
  result := false;
  if imageDiff <> nil then result := result or imageDiff.Compress;
  if selectionDiff <> nil then result := result or selectionDiff.Compress;
  if selectionLayerDiff <> nil then result := result or selectionLayerDiff.Compress;
end;

procedure TImageLayerStateDifference.ApplyTo(AState: TState);
var
  idx, origIdx: integer;
  lState: TImageState;
begin
  inherited ApplyTo(AState);
  lState := AState as TImageState;
  if layerId <> -1 then
  begin
    idx := lState.currentLayeredBitmap.GetLayerIndexFromId(layerId);
    if idx = -1 then raise exception.Create('Layer not found');
    if imageDiff <> nil then
    begin
      lState.currentLayeredBitmap.SetLayerBitmap(idx, ComputeFromImageDiff(lState.LayerBitmap[idx],imageDiff,False), True);
      lState.currentLayeredBitmap.RemoveUnusedOriginals;
    end;
    if nextLayerOriginalData <> nil then
    begin
      nextLayerOriginalData.Position := 0;
      origIdx := lState.currentLayeredBitmap.AddOriginalFromStream(nextLayerOriginalData);
      lState.currentLayeredBitmap.LayerOriginalGuid[idx] := lState.currentLayeredBitmap.Original[origIdx].Guid;
      lState.currentLayeredBitmap.LayerOriginalMatrix[idx] := nextLayerOriginalMatrix;
      lState.currentLayeredBitmap.LayerOriginalRenderStatus[idx] := orsProof;
    end else
    if lState.currentLayeredBitmap.LayerOriginal[idx] <> nil then
    begin
      lState.currentLayeredBitmap.LayerOriginalMatrix[idx] := nextLayerOriginalMatrix;
      lState.currentLayeredBitmap.LayerOriginalRenderStatus[idx] := orsProof;
    end;
    if selectionDiff <> nil then ApplyGrayscaleImageDiffAndReplace(lState.currentSelection,selectionDiff,False);
    if selectionLayerDiff <> nil then ApplyImageDiffAndReplace(lState.selectionLayer,selectionLayerDiff,False);
  end;
end;

procedure TImageLayerStateDifference.UnapplyTo(AState: TState);
var
  idx, origIdx: integer;
  lState: TImageState;
begin
  inherited UnapplyTo(AState);
  lState := AState as TImageState;
  if layerId <> -1 then
  begin
    idx := lState.currentLayeredBitmap.GetLayerIndexFromId(layerId);
    if idx = -1 then raise exception.Create('Layer not found');
    if imageDiff <> nil then
    begin
      lState.currentLayeredBitmap.SetLayerBitmap(idx, ComputeFromImageDiff(lState.LayerBitmap[idx],imageDiff,True), True);
      lState.currentLayeredBitmap.RemoveUnusedOriginals;
    end;
    if prevLayerOriginalData <> nil then
    begin
      prevLayerOriginalData.Position:= 0;
      origIdx := lState.currentLayeredBitmap.AddOriginalFromStream(prevLayerOriginalData);
      lState.currentLayeredBitmap.LayerOriginalGuid[idx] := lState.currentLayeredBitmap.Original[origIdx].Guid;
      lState.currentLayeredBitmap.LayerOriginalMatrix[idx] := prevLayerOriginalMatrix;
      lState.currentLayeredBitmap.LayerOriginalRenderStatus[idx] := orsProof;
    end else
    if lState.currentLayeredBitmap.LayerOriginal[idx] <> nil then
    begin
      lState.currentLayeredBitmap.LayerOriginalMatrix[idx] := prevLayerOriginalMatrix;
      lState.currentLayeredBitmap.LayerOriginalRenderStatus[idx] := orsProof;
    end;
    if selectionDiff <> nil then ApplyGrayscaleImageDiffAndReplace(lState.currentSelection,selectionDiff,True);
    if selectionLayerDiff <> nil then ApplyImageDiffAndReplace(lState.selectionLayer,selectionLayerDiff,True);
  end;
end;

function TImageLayerStateDifference.UsedMemory: int64;
begin
  Result:= 0;
  if Assigned(imageDiff) then result += imageDiff.UsedMemory;
  if Assigned(selectionDiff) then result += selectionDiff.UsedMemory;
  if Assigned(selectionLayerDiff) then result += selectionLayerDiff.UsedMemory;
end;

constructor TImageLayerStateDifference.Create(AFromState, AToState: TState);
var
  prev,next: TImageState;
  prevIdx,curIdx: integer;
  prevOrig,nextOrig: TBGRALayerCustomOriginal;
begin
  inherited Create(AFromState,AToState);
  layerId := -1;
  imageDiff := nil;
  selectionDiff := nil;
  selectionLayerDiff := nil;
  prevLayerOriginalData := nil;
  nextLayerOriginalData := nil;
  layerOriginalChange := false;

  prev := AFromState as TImageState;
  next := AToState as TImageState;
  layerId := next.selectedLayerId;
  if layerId <> prev.selectedLayerId then
    raise exception.Create('Inconsistent layer id');
  prevIdx := prev.currentLayeredBitmap.GetLayerIndexFromId(LayerId);
  curIdx := next.currentLayeredBitmap.GetLayerIndexFromId(LayerId);
  if (curIdx = -1) or (prevIdx = -1) then
  begin
    layerId:= -1;
    prevLayerOriginalMatrix := AffineMatrixIdentity;
    nextLayerOriginalMatrix := AffineMatrixIdentity;
  end
  else
  begin
    imageDiff := TImageDiff.Create(prev.LayerBitmap[prevIdx],next.LayerBitmap[curIdx]);
    selectionDiff := TGrayscaleImageDiff.Create(prev.currentSelection,next.currentSelection);
    selectionLayerDiff := TImageDiff.Create(prev.selectionLayer,next.selectionLayer);
    prevOrig := prev.LayerOriginal[prevIdx];
    prevLayerOriginalMatrix := prev.LayerOriginalMatrix[prevIdx];
    nextOrig := next.LayerOriginal[curIdx];
    nextLayerOriginalMatrix := next.LayerOriginalMatrix[curIdx];
    if prevOrig <> nextOrig then
    begin
      layerOriginalChange := true;
      if Assigned(prevOrig) then
      begin
        prevLayerOriginalData := TMemoryStream.Create;
        prevOrig.SaveToStream(prevLayerOriginalData);
      end;
      if Assigned(nextOrig) then
      begin
        nextLayerOriginalData := TMemoryStream.Create;
        nextOrig.SaveToStream(nextLayerOriginalData);
      end;
    end;
  end;
end;

constructor TImageLayerStateDifference.Create(AToState: TState;
  APreviousImage: TBGRABitmap; APreviousImageDefined: boolean;
  APreviousSelection: TBGRABitmap; APreviousSelectionDefined: boolean;
  APreviousSelectionLayer: TBGRABitmap; APreviousSelectionLayerDefined: boolean;
  APreviousLayerOriginalData: TStream;
  APreviousLayerOriginalMatrix: TAffineMatrix);
var
  r1,r2,r3: TRect;
  w,h: integer;
begin
  w := (AToState as TImageState).Width;
  h := (AToState as TImageState).Height;
  if APreviousImageDefined then r1 := rect(0,0,w,h) else r1 := EmptyRect;
  if APreviousSelectionDefined then r2 := rect(0,0,w,h) else r2 := EmptyRect;
  if APreviousSelectionLayerDefined then r3 := rect(0,0,w,h) else r3 := EmptyRect;
  Init(AToState,APreviousImage,r1,APreviousSelection,r2,APreviousSelectionLayer,r3,
       APreviousLayerOriginalData, APreviousLayerOriginalMatrix);
end;

constructor TImageLayerStateDifference.Create(AToState: TState;
  APreviousImage: TBGRABitmap; APreviousImageChangeRect: TRect;
  APreviousSelection: TBGRABitmap; APreviousSelectionChangeRect: TRect;
  APreviousSelectionLayer: TBGRABitmap; APreviousSelectionLayerChangeRect: TRect;
  APreviousLayerOriginalData: TStream;
  APreviousLayerOriginalMatrix: TAffineMatrix);
begin
  Init(AToState, APreviousImage, APreviousImageChangeRect, APreviousSelection,
    APreviousSelectionChangeRect, APreviousSelectionLayer, APreviousSelectionLayerChangeRect,
    APreviousLayerOriginalData, APreviousLayerOriginalMatrix);
end;

destructor TImageLayerStateDifference.Destroy;
begin
  imageDiff.Free;
  selectionDiff.Free;
  selectionLayerDiff.Free;
  prevLayerOriginalData.Free;
  nextLayerOriginalData.Free;
  inherited Destroy;
end;

end.

