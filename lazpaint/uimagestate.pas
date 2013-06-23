unit uimagestate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ustatehandler, BGRABitmap, BGRACompressableBitmap, BGRABitmapTypes, Types, BGRALayers;

type

  { TImageState }

  TImageState = class(TState)
  private
    function GetCurrentLayer: TBGRABitmap;
    function GetCurrentLayerIndex: integer;
    procedure SetCurrentLayerIndex(AValue: integer);
  public
    currentLayeredBitmap: TBGRALayeredBitmap;
    currentSelection, selectionLayer: TBGRABitmap;
    selectedLayerId: integer;
    filename: string;
    constructor Create;
    destructor Destroy; override;
    function Equals(Obj: TObject): boolean; override;
    function ComputeDifferenceFrom(AState: TState): TStateDifference; override;
    function ApplyDifference(ADifference: TStateDifference): TState; override;
    function ReverseDifference(ADifference: TStateDifference): TState; override;
    function Duplicate: TState; override;
    property currentLayer: TBGRABitmap read GetCurrentLayer;
    property currentLayerIndex: integer read GetCurrentLayerIndex write SetCurrentLayerIndex;
  end;

  { TImageStateDifference }

  TImageStateDifference = class(TStateDifference)
  public
    imageDiff, selectionDiff, selectionLayerDiff: TBGRACompressableBitmap;
    previousImageSize, previousSelectionSize, previousSelectionLayerSize: TSize;
    nextImageSize, nextSelectionSize, nextSelectionLayerSize: TSize;
    function TryCompress: boolean; override;
    function UsedMemory: int64;
    constructor Create;
    destructor Destroy; override;
  end;

function DuplicateBitmap(ABitmap: TBGRABitmap): TBGRABitmap;
function DuplicateLayeredBitmap(ALayeredBitmap: TBGRALayeredBitmap): TBGRALayeredBitmap;

implementation

uses Math;

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

procedure ComputeImageDiff(Image1,Image2: TBGRABitmap; out diff: TBGRABitmap; out Size1,Size2: TSize);
var tx,ty: integer;
begin
  if Image1 = nil then
  begin
    Size1.cx := 0;
    size1.cy := 0;
  end else
  begin
    Size1.cx := Image1.Width;
    Size1.cy := Image1.Height;
  end;

  if Image2 = nil then
  begin
    Size2.cx := 0;
    size2.cy := 0;
  end else
  begin
    Size2.cx := Image2.Width;
    Size2.cy := Image2.Height;
  end;

  tx := max(Size1.cx,Size2.cx);
  ty := max(Size1.cy,Size2.cy);
  if (tx=0) or (ty=0) then
    diff := nil
  else
  begin
    diff := TBGRABitmap.Create(tx,ty);
    if Image1<>nil then
      diff.PutImage(0,0,Image1,dmXor);
    if Image2<>nil then
      diff.PutImage(0,0,Image2,dmXor);
  end;
end;

function ComputeFromImageDiff(FromImage: TBGRABitmap; Diff: TBGRACompressableBitmap; DestSize: TSize): TBGRABitmap;
var tempBmp: TBGRABitmap;
begin
  if (DestSize.cx = 0) or (DestSize.cy = 0) or ((FromImage = nil) and (Diff = nil)) then
    result := nil
  else
  begin
    result := TBGRABitmap.Create(Destsize.cx,Destsize.cy);
    if FromImage <> nil then
      result.PutImage(0,0,FromImage,dmXor);
    if Diff <> nil then
    begin
      tempBmp := Diff.GetBitmap;
      result.PutImage(0,0,tempBmp,dmXor);
      tempBmp.Free;
    end;
  end;
end;

{ TImageStateDifference }

function TImageStateDifference.TryCompress: boolean;
begin
  result := false;
  if imageDiff <> nil then result := result or imageDiff.Compress;
  if selectionDiff <> nil then result := result or selectionDiff.Compress;
  if selectionLayerDiff <> nil then result := result or selectionLayerDiff.Compress;
end;

function TImageStateDifference.UsedMemory: int64;
begin
  result := 0;
  if imageDiff<>nil then inc(result,imageDiff.UsedMemory);
  if selectionDiff<>nil then inc(result,selectionDiff.UsedMemory);
  if selectionLayerDiff<>nil then inc(result,selectionLayerDiff.UsedMemory);
end;

constructor TImageStateDifference.Create;
begin
  imageDiff := nil;
  selectionDiff := nil;
  selectionLayerDiff := nil;
end;

destructor TImageStateDifference.Destroy;
begin
  imageDiff.Free;
  selectionDiff.Free;
  selectionLayerDiff.Free;
  inherited Destroy;
end;

{ TImageState }

function TImageState.GetCurrentLayer: TBGRABitmap;
var idx: integer;
begin
  if currentLayeredBitmap = nil then
  begin
    result := nil;
    exit;
  end else
  begin
    idx := currentLayeredBitmap.GetLayerIndexFromId(selectedLayerId);
    if idx = -1 then result := nil else
      result := currentLayeredBitmap.LayerBitmap[idx]; //assume direct access to bitmap
  end;
end;

function TImageState.GetCurrentLayerIndex: integer;
begin
  if currentLayeredBitmap = nil then
  begin
    result := -1;
  end else
    result := currentLayeredBitmap.GetLayerIndexFromId(selectedLayerId);
end;

procedure TImageState.SetCurrentLayerIndex(AValue: integer);
begin
  if (currentLayeredBitmap = nil) or (AValue < 0) or (AValue >= currentLayeredBitmap.NbLayers) then
  begin
    selectedLayerId := -1;
  end else
  begin
    selectedLayerId := currentLayeredBitmap.LayerUniqueId[AValue];
  end;
end;

constructor TImageState.Create;
begin
  currentLayeredBitmap := nil;
  currentSelection := nil;
  selectionLayer := nil;
  selectedLayerId := -1;
end;

destructor TImageState.Destroy;
begin
  currentLayeredBitmap.Free;
  currentSelection.free;
  selectionLayer.Free;
  inherited Destroy;
end;

function TImageState.Equals(Obj: TObject): boolean;
var other: TImageState;
  selectedLayerIndex, otherSelectedLayerIndex: integer;
begin
  if obj is TImageState then
  begin
    other := obj as TImageState;
    result := false;
    if selectedLayerId <> -1 then //compare active layer (where modifications are expected to be)
    begin
      selectedLayerIndex := currentLayeredBitmap.GetLayerIndexFromId(selectedLayerId);
      otherSelectedLayerIndex := other.currentLayeredBitmap.GetLayerIndexFromId(selectedLayerId);
      if (selectedLayerIndex <> -1) and (otherSelectedLayerIndex <> -1) then
        if not other.currentLayeredBitmap.LayerBitmap[otherSelectedLayerIndex].Equals(currentLayeredBitmap.LayerBitmap[selectedLayerIndex]) then exit;
    end;
    if (other.currentSelection = nil) and (currentSelection <> nil) and not currentSelection.Equals(BGRABlack) then exit;
    if (other.currentSelection <> nil) and (currentSelection = nil) and not other.currentSelection.Equals(BGRABlack) then exit;
    if (other.currentSelection <> nil) and (currentSelection <> nil) and not other.currentSelection.Equals(currentSelection) then exit;
    if (other.selectionLayer = nil) and (selectionLayer <> nil) and not selectionLayer.Empty then exit;
    if (other.selectionLayer <> nil) and (selectionLayer = nil) and not other.selectionLayer.Empty then exit;
    if (other.selectionLayer <> nil) and (selectionLayer <> nil) and not other.selectionLayer.Equals(selectionLayer) then exit;
    if (other.filename <> filename) then exit;
    result := true;
  end
  else
    Result:=inherited Equals(Obj);
end;

function TImageState.ComputeDifferenceFrom(AState: TState): TStateDifference;
{var tempImageDiff,tempSelectionDiff,tempSelectionLayerDiff: TBGRABitmap;
    prev: TImageState;
    diff: TImageStateDifference;}
begin
  result := nil;
  raise exception.Create('Not implemented');
{  prev := AState as TImageState;
  diff := TImageStateDifference.Create;

  ComputeImageDiff(prev.currentBitmap,currentBitmap,tempImageDiff,diff.previousImageSize,diff.nextImageSize);
  if tempImageDiff <> nil then
  begin
    diff.imageDiff := TBGRACompressableBitmap.Create(tempImageDiff);
    tempImageDiff.Free;
  end else
    diff.imageDiff := nil;
  ComputeImageDiff(prev.currentSelection,currentSelection,tempSelectionDiff,diff.previousSelectionSize,diff.nextSelectionSize);
  if tempSelectionDiff <> nil then
  begin
    diff.selectionDiff := TBGRACompressableBitmap.Create(tempSelectionDiff);
    tempSelectionDiff.Free;
  end else
    diff.selectionDiff := nil;
  ComputeImageDiff(prev.selectionLayer,selectionLayer,tempSelectionLayerDiff,diff.previousSelectionLayerSize,diff.nextSelectionLayerSize);
  if tempSelectionLayerDiff <> nil then
  begin
    diff.selectionLayerDiff := TBGRACompressableBitmap.Create(tempSelectionLayerDiff);
    tempSelectionLayerDiff.Free;
  end else
    diff.selectionLayerDiff := nil;

  result := diff;}
end;

function TImageState.ApplyDifference(ADifference: TStateDifference): TState;
{var next: TImageState;
    diff: TImageStateDifference;}
begin
  result := nil;
  raise exception.Create('Not implemented');
{  next := TImageState.Create;
  diff := ADifference as TImageStateDifference;

  next.currentBitmap := ComputeFromImageDiff(currentBitmap,diff.imageDiff,diff.nextImageSize);
  next.currentSelection := ComputeFromImageDiff(currentSelection,diff.selectionDiff,diff.nextSelectionSize);
  next.selectionLayer := ComputeFromImageDiff(selectionLayer,diff.selectionLayerDiff,diff.nextSelectionLayerSize);

  result := next;}
end;

function TImageState.ReverseDifference(ADifference: TStateDifference): TState;
{var prev: TImageState;
    diff: TImageStateDifference;}
begin
  result := nil;
  raise exception.Create('Not implemented');
{  prev := TImageState.Create;
  diff := ADifference as TImageStateDifference;

  prev.currentBitmap := ComputeFromImageDiff(currentBitmap,diff.imageDiff,diff.previousImageSize);
  prev.currentSelection := ComputeFromImageDiff(currentSelection,diff.selectionDiff,diff.previousSelectionSize);
  prev.selectionLayer := ComputeFromImageDiff(selectionLayer,diff.selectionLayerDiff,diff.previousSelectionLayerSize);

  result := prev;}
end;

function TImageState.Duplicate: TState;
var copy: TImageState;
begin
  copy := TImageState.Create;
  copy.currentLayeredBitmap := DuplicateLayeredBitmap(currentLayeredBitmap);
  copy.currentSelection := DuplicateBitmap(currentSelection);
  copy.selectionLayer := DuplicateBitmap(selectionLayer);
  copy.selectedLayerId := selectedLayerId;
  result := copy;
end;

end.

