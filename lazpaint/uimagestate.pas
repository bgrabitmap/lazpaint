unit UImageState;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UStateType, BGRABitmap, BGRABitmapTypes, Types, BGRALayers,
  UImageType, BGRAWriteLzp, BGRAReadLzp, FPimage, BGRALayerOriginal;

type
  { TImageState }

  TImageState = class(TState)
  private
    FSelectionMask: TBGRABitmap;
    function GetBlendOp(Index: integer): TBlendOperation;
    function GetSelectedImageLayer: TBGRABitmap;
    function GetCurrentLayerIndex: integer;
    function GetCurrentSelectionMask: TBGRABitmap;
    function GetHasOriginals: boolean;
    function GetHeight: integer;
    function GetLayerBitmap(Index: integer): TBGRABitmap;
    function GetLayerBitmapById(AId: integer): TBGRABitmap;
    function GetLayerId(Index: integer): integer;
    function GetLayerName(Index: integer): string;
    function GetLayerOffset(Index: integer): TPoint;
    function GetLayerOpacity(Index: integer): byte;
    function GetLayerOriginal(Index: integer): TBGRALayerCustomOriginal;
    function GetLayerOriginalDefined(Index: integer): boolean;
    function GetLayerOriginalKnown(Index: integer): boolean;
    function GetLayerOriginalMatrix(Index: integer): TAffineMatrix;
    function GetLayerVisible(Index: integer): boolean;
    function GetLinearBlend: boolean;
    function GetNbLayers: integer;
    function GetWidth: integer;
    procedure SelectImageLayer(AValue: TBGRABitmap);
    procedure SelectImageLayerByIndex(AValue: integer);
    procedure SetLinearBlend(AValue: boolean);
  public
    currentLayeredBitmap: TBGRALayeredBitmap;
    SelectionLayer: TBGRABitmap;
    selectedLayerId: integer;
    filenameUTF8: string;
    constructor Create;
    destructor Destroy; override;
    function Equals(Obj: TObject): boolean; override;
    procedure ApplyDifference(ADifference: TStateDifference); override;
    procedure ReverseDifference(ADifference: TStateDifference); override;
    function Duplicate: TState; override;
    procedure Resample(AWidth,AHeight: integer; AQuality: TResampleMode; AFilter: TResampleFilter);
    procedure SetLayerBitmap(layer: integer; ABitmap: TBGRABitmap; AOwned: boolean);
    procedure SetSize(AWidth,AHeight: integer);
    procedure PrepareForRendering;
    function ComputeFlatImageWithoutSelection(ASeparateXorMask: boolean): TBGRABitmap;
    procedure Assign(AValue: TBGRABitmap; AOwned: boolean);
    procedure Assign(AValue: TBGRALayeredBitmap; AOwned: boolean);
    procedure Assign(AValue: TImageState; AOwned: boolean);
    procedure RemoveSelection;
    procedure ReplaceSelection(ASelectionMask, ASelectionLayer: TBGRABitmap);
    function AssignWithUndo(AValue: TBGRALayeredBitmap; AOwned: boolean; ASelectedLayerIndex: integer): TCustomImageDifference;
    function AssignWithUndo(AValue: TBGRALayeredBitmap; AOwned: boolean; ASelectedLayerIndex: integer; ACurrentSelection: TBGRABitmap; ASelectionLayer:TBGRABitmap): TCustomImageDifference;
    function AssignWithUndo(AState: TImageState; AOwned: boolean): TCustomImageDifference;
    function GetUndoAfterAssign(ABackup: TImageState): TCustomImageDifference;
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
    procedure SaveToStreamAs(AStream: TStream; AFormat: TBGRAImageFormat);
    procedure SaveOriginalToStream(AStream: TStream);
    procedure SaveToFile(AFilenameUTF8: string);
    function AddNewLayer(ALayer: TBGRABitmap; AName: string; ABlendOp: TBlendOperation): TCustomImageDifference;
    function AddNewLayer(AOriginal: TBGRALayerCustomOriginal; AName: string; ABlendOp: TBlendOperation): TCustomImageDifference;
    function DuplicateLayer: TCustomImageDifference;
    function MergerLayerOver(ALayerOverIndex: integer): TCustomImageDifference;
    function MoveLayer(AFromIndex,AToIndex: integer): TCustomImageDifference;
    function RemoveLayer: TCustomImageDifference;
    function DiscardOriginal(ACreateUndo: boolean): TCustomImageDifference;
    function HorizontalFlip: TCustomImageDifference; overload;
    function HorizontalFlip(ALayerIndex: integer): TCustomImageDifference; overload;
    function VerticalFlip: TCustomImageDifference; overload;
    function VerticalFlip(ALayerIndex: integer): TCustomImageDifference; overload;
    function SwapRedBlue: TCustomImageDifference;
    function LinearNegative: TCustomImageDifference;
    function Negative: TCustomImageDifference;
    function RotateCW: TCustomImageDifference;
    function RotateCCW: TCustomImageDifference;
    function ComputeLayerOffsetDifference(AOffsetX, AOffsetY: integer): TCustomImageDifference;
    function ComputeLayerDifference(APreviousImage: TBGRABitmap; APreviousImageDefined: boolean;
        APreviousSelection: TBGRABitmap; APreviousSelectionDefined: boolean;
        APreviousSelectionLayer: TBGRABitmap; APreviousSelectionLayerDefined: boolean;
        APreviousLayerOriginalData: TStream;
        APreviousLayerOriginalMatrix: TAffineMatrix): TCustomImageDifference; overload;
    function ComputeLayerDifference(APreviousImage: TBGRABitmap; APreviousImageChangeRect: TRect;
        APreviousSelection: TBGRABitmap; APreviousSelectionChangeRect: TRect;
        APreviousSelectionLayer: TBGRABitmap; APreviousSelectionLayerChangeRect: TRect;
        APreviousLayerOriginalData: TStream;
        APreviousLayerOriginalMatrix: TAffineMatrix): TCustomImageDifference; overload;
    function GetLayeredBitmapCopy: TBGRALayeredBitmap;
    function ComputeFlatImage(AFromLayer, AToLayer: integer; ASeparateXorMask: boolean): TBGRABitmap;
    function ComputeFlatImage(ARect: TRect; AFromLayer, AToLayer: integer; ASeparateXorMask: boolean): TBGRABitmap;
    function SetLayerName(Index: integer; AValue: string): TCustomImageDifference;
    function SetLayerOpacity(Index: integer; AValue: byte): TCustomImageDifference;
    function SetLayerVisible(Index: integer; AValue: boolean): TCustomImageDifference;
    function SetLayerOffset(Index: integer; AValue: TPoint): TCustomImageDifference;
    function SetBlendOp(Index: integer; AValue: TBlendOperation): TCustomImageDifference;
    procedure DrawLayers(ADest: TBGRABitmap; X,Y: Integer; AIconCursor: boolean);
    property SelectedImageLayer: TBGRABitmap read GetSelectedImageLayer write SelectImageLayer;
    property SelectedImageLayerIndex: integer read GetCurrentLayerIndex write SelectImageLayerByIndex;
    property LayerOriginal[Index: integer]: TBGRALayerCustomOriginal read GetLayerOriginal;
    property LayerOriginalDefined[Index: integer]: boolean read GetLayerOriginalDefined;
    property LayerOriginalKnown[Index: integer]: boolean read GetLayerOriginalKnown;
    property LayerOriginalMatrix[Index: integer]: TAffineMatrix read GetLayerOriginalMatrix;
    property LayerBitmap[Index: integer]: TBGRABitmap read GetLayerBitmap;
    property LayerBitmapById[AId: integer]: TBGRABitmap read GetLayerBitmapById;
    property BlendOperation[Index: integer]: TBlendOperation read GetBlendOp;
    property LayerOpacity[Index: integer]: byte read GetLayerOpacity;
    property LayerOffset[Index: integer]: TPoint read GetLayerOffset;
    property LayerName[Index: integer]: string read GetLayerName;
    property LayerId[Index: integer]: integer read GetLayerId;
    property NbLayers: integer read GetNbLayers;
    property Width: integer read GetWidth;
    property Height: integer read GetHeight;
    property LinearBlend: boolean read GetLinearBlend write SetLinearBlend;
    property LayerVisible[Index: integer]: boolean read GetLayerVisible;
    property HasOriginals: boolean read GetHasOriginals;
    property SelectionMask: TBGRABitmap read GetCurrentSelectionMask write FSelectionMask;
  end;

implementation

uses BGRAStreamLayers, UImageDiff, BGRALzpCommon, UFileSystem, BGRATransform;

{ TImageState }

function TImageState.GetSelectedImageLayer: TBGRABitmap;
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

function TImageState.GetBlendOp(Index: integer): TBlendOperation;
begin
  if currentLayeredBitmap = nil then
    result := boTransparent
  else
    result := currentLayeredBitmap.BlendOperation[Index];
end;

function TImageState.GetCurrentLayerIndex: integer;
begin
  if currentLayeredBitmap = nil then
  begin
    result := -1;
  end else
    result := currentLayeredBitmap.GetLayerIndexFromId(selectedLayerId);
end;

function TImageState.GetCurrentSelectionMask: TBGRABitmap;
begin
  result := FSelectionMask;
  if Assigned(result) then result.LinearAntialiasing := true;
end;

function TImageState.GetHasOriginals: boolean;
var
  i: Integer;
begin
  if currentLayeredBitmap= nil then
    result := false
  else
  begin
    for i := 0 to NbLayers-1 do
      if LayerOriginalDefined[i] then
        exit(true);
    result := false
  end;
end;

function TImageState.GetHeight: integer;
begin
  if currentLayeredBitmap= nil then
    result := 0
  else
    result := currentLayeredBitmap.Height;
end;

function TImageState.GetLayerBitmap(Index: integer): TBGRABitmap;
begin
  result := currentLayeredBitmap.LayerBitmap[Index];
end;

function TImageState.GetLayerBitmapById(AId: integer): TBGRABitmap;
var idx: integer;
begin
  idx := currentLayeredBitmap.GetLayerIndexFromId(AId);
  if idx = -1 then
    result := nil
  else
    result := currentLayeredBitmap.LayerBitmap[idx];
end;

function TImageState.GetLayerId(Index: integer): integer;
begin
  if currentLayeredBitmap = nil then
    result := -1
  else
    result := currentLayeredBitmap.LayerUniqueId[index];
end;

function TImageState.GetLayerName(Index: integer): string;
begin
  if currentLayeredBitmap = nil then
    result := ''
  else
    result := currentLayeredBitmap.LayerName[index];
end;

function TImageState.GetLayerOffset(Index: integer): TPoint;
begin
  if currentLayeredBitmap = nil then
    result := point(0,0)
  else
    result := currentLayeredBitmap.LayerOffset[index];
end;

function TImageState.GetLayerOpacity(Index: integer): byte;
begin
  if currentLayeredBitmap = nil then
    result := 255
  else
    result := currentLayeredBitmap.LayerOpacity[index];
end;

function TImageState.GetLayerOriginal(Index: integer): TBGRALayerCustomOriginal;
begin
  if currentLayeredBitmap = nil then
    result := nil
  else
    result := currentLayeredBitmap.LayerOriginal[Index];
end;

function TImageState.GetLayerOriginalDefined(Index: integer): boolean;
begin
  if currentLayeredBitmap = nil then
    result := false
  else
    result := currentLayeredBitmap.LayerOriginalGuid[Index] <> GUID_NULL;
end;

function TImageState.GetLayerOriginalKnown(Index: integer): boolean;
begin
  if currentLayeredBitmap = nil then
    result := false
  else
    result := currentLayeredBitmap.LayerOriginalKnown[Index];
end;

function TImageState.GetLayerOriginalMatrix(Index: integer): TAffineMatrix;
begin
  if currentLayeredBitmap = nil then
    result := AffineMatrixIdentity
  else
    result := currentLayeredBitmap.LayerOriginalMatrix[Index];
end;

function TImageState.GetLayerVisible(Index: integer): boolean;
begin
  if currentLayeredBitmap = nil then
    result := false
  else
    result := currentLayeredBitmap.LayerVisible[Index];
end;

function TImageState.GetLinearBlend: boolean;
begin
  if currentLayeredBitmap = nil then
    result := true
  else
    result := currentLayeredBitmap.LinearBlend;
end;

function TImageState.GetNbLayers: integer;
begin
  if currentLayeredBitmap = nil then
    result := 0
  else
    result := currentLayeredBitmap.NbLayers;
end;

function TImageState.GetWidth: integer;
begin
  if currentLayeredBitmap= nil then
    result := 0
  else
    result := currentLayeredBitmap.Width;
end;

procedure TImageState.SelectImageLayer(AValue: TBGRABitmap);
var
  i: Integer;
begin
  if currentLayeredBitmap = nil then exit;

  for i := 0 to NbLayers-1 do
    if currentLayeredBitmap.LayerBitmap[i] = AValue then
    begin
      selectedLayerId := currentLayeredBitmap.LayerUniqueId[i];
      exit;
    end;
  selectedLayerId := -1;
end;

procedure TImageState.SelectImageLayerByIndex(AValue: integer);
begin
  if (currentLayeredBitmap = nil) or (AValue < 0) or (AValue >= currentLayeredBitmap.NbLayers) then
  begin
    selectedLayerId := -1;
  end else
  begin
    selectedLayerId := currentLayeredBitmap.LayerUniqueId[AValue];
    currentLayeredBitmap.Unfreeze(AValue);
  end;
end;

function TImageState.SetLayerName(Index: integer; AValue: string): TCustomImageDifference;
begin
  if currentLayeredBitmap <> nil then
  begin
    if LayerName[Index] <> AValue then
      result := TSetLayerNameStateDifference.Create(self,currentLayeredBitmap.LayerUniqueId[index],AValue)
    else
      result := nil;
  end
  else
    result := nil;
end;

function TImageState.SetLayerOffset(Index: integer; AValue: TPoint
  ): TCustomImageDifference;
begin
  if currentLayeredBitmap <> nil then
  begin
    if (LayerOffset[index].x <> AValue.x) or (LayerOffset[index].y <> AValue.y) then
      result := TSetLayerOffsetStateDifference.Create(self,currentLayeredBitmap.LayerUniqueId[index],AValue)
    else
      result := nil;
  end
  else
    result := nil;
end;

function TImageState.SetLayerOpacity(Index: integer; AValue: byte
  ): TCustomImageDifference;
begin
  if currentLayeredBitmap <> nil then
  begin
    if LayerOpacity[index] <> AValue then
      result := TSetLayerOpacityStateDifference.Create(self,currentLayeredBitmap.LayerUniqueId[index],AValue)
    else
      result := nil;
  end
  else
    result := nil;
end;

function TImageState.SetLayerVisible(Index: integer; AValue: boolean
  ): TCustomImageDifference;
begin
  if currentLayeredBitmap <> nil then
  begin
    if LayerVisible[Index] <> AValue then
      result := TSetLayerVisibleStateDifference.Create(self,currentLayeredBitmap.LayerUniqueId[index],AValue)
    else
      result := nil;
  end
  else
    result := nil;
end;

function TImageState.SetBlendOp(Index: integer; AValue: TBlendOperation
  ): TCustomImageDifference;
begin
  if currentLayeredBitmap <> nil then
  begin
    if BlendOperation[index] <> Avalue then
      result := TSetLayerBlendOpStateDifference.Create(self,currentLayeredBitmap.LayerUniqueId[index],AValue)
    else
      result := nil;
  end
  else
    result := nil;
end;

procedure TImageState.SetLinearBlend(AValue: boolean);
begin
  if currentLayeredBitmap <> nil then
    currentLayeredBitmap.LinearBlend := AValue;
end;

procedure TImageState.SetLayerBitmap(layer: integer; ABitmap: TBGRABitmap;
  AOwned: boolean);
begin
  if currentLayeredBitmap <> nil then
    currentLayeredBitmap.SetLayerBitmap(layer,ABitmap,AOwned);
end;

procedure TImageState.SetSize(AWidth, AHeight: integer);
begin
  if currentLayeredBitmap <> nil then
    currentLayeredBitmap.SetSize(AWidth,AHeight);
end;

procedure TImageState.PrepareForRendering;
begin
  if currentLayeredBitmap <> nil then
    currentLayeredBitmap.FreezeExceptOneLayer(SelectedImageLayerIndex);
end;

function TImageState.ComputeFlatImageWithoutSelection(ASeparateXorMask: boolean): TBGRABitmap;
begin
  if currentLayeredBitmap <> nil then
    result := currentLayeredBitmap.ComputeFlatImage(ASeparateXorMask)
  else
    result := TBGRABitmap.Create(Width,Height);
end;

procedure TImageState.Assign(AValue: TBGRABitmap; AOwned: boolean);
var
  xorMask: TBGRABitmap;
begin
  if currentLayeredBitmap = nil then
    currentLayeredBitmap := TBGRALayeredBitmap.Create;

  currentLayeredBitmap.Clear;
  currentLayeredBitmap.SetSize(AValue.Width,AValue.Height);
  if AOwned then
  begin
    currentLayeredBitmap.AddOwnedLayer(AValue);
    if Assigned(AValue.XorMask) then
    begin
      xorMask := TBGRABitmap.Create(AValue.XorMask);
      xorMask.AlphaFill(255);
      xorMask.ReplaceColor(BGRABlack,BGRAPixelTransparent);
      currentLayeredBitmap.LayerName[currentLayeredBitmap.AddOwnedLayer(xorMask,boXor)] := 'Xor';
      AValue.DiscardXorMask;
    end;
  end
  else
  begin
    currentLayeredBitmap.AddLayer(AValue);
    if Assigned(AValue.XorMask) then
    begin
      xorMask := AValue.XorMask.Duplicate as TBGRABitmap;
      xorMask.AlphaFill(255);
      xorMask.ReplaceColor(BGRABlack,BGRAPixelTransparent);
      currentLayeredBitmap.LayerName[currentLayeredBitmap.AddOwnedLayer(xorMask,boXor)] := 'Xor';
    end;
  end;
  SelectedImageLayerIndex := 0;
end;

procedure TImageState.Assign(AValue: TBGRALayeredBitmap; AOwned: boolean);
begin
  if AOwned then
  begin
    currentLayeredBitmap.Free;
    currentLayeredBitmap := AValue;
  end else
    currentLayeredBitmap.Assign(AValue,true);
  if NbLayers > 0 then
  begin
    SelectedImageLayerIndex := 0
  end
  else
    SelectedImageLayerIndex := -1;
end;

procedure TImageState.Assign(AValue: TImageState; AOwned: boolean);
begin
  Assign(AValue.currentLayeredBitmap, AOwned);
  if AOwned then AValue.currentLayeredBitmap := nil;
  BGRAReplace(FSelectionMask, AValue.SelectionMask);
  if AOwned then AValue.SelectionMask := nil;
  BGRAReplace(SelectionLayer, AValue.SelectionLayer);
  if AOwned then AValue.SelectionLayer := nil;
  if AOwned then AValue.Free;
end;

procedure TImageState.RemoveSelection;
begin
  FreeAndNil(SelectionLayer);
  FreeAndNil(FSelectionMask);
end;

procedure TImageState.ReplaceSelection(ASelectionMask,
  ASelectionLayer: TBGRABitmap);
begin
  if ASelectionMask<>FSelectionMask then
  begin
    FSelectionMask.Free;
    FSelectionMask := ASelectionMask;
  end;
  if ASelectionLayer<>SelectionLayer then
  begin
    SelectionLayer.Free;
    SelectionLayer := ASelectionLayer;
  end;
end;

function TImageState.AssignWithUndo(AValue: TBGRALayeredBitmap;
  AOwned: boolean; ASelectedLayerIndex: integer): TCustomImageDifference;
begin
  if currentLayeredBitmap = nil then
    result := nil
  else
    result := TAssignStateDifference.Create(self, AValue, AOwned, ASelectedLayerIndex);
end;

function TImageState.AssignWithUndo(AValue: TBGRALayeredBitmap;
  AOwned: boolean; ASelectedLayerIndex: integer; ACurrentSelection: TBGRABitmap;
  ASelectionLayer: TBGRABitmap): TCustomImageDifference;
begin
  if currentLayeredBitmap = nil then
    result := nil
  else
    result := TAssignStateDifference.Create(self, AValue, AOwned, ASelectedLayerIndex, ACurrentSelection, ASelectionLayer);
end;

function TImageState.AssignWithUndo(AState: TImageState; AOwned: boolean): TCustomImageDifference;
begin
  result := AssignWithUndo(AState.currentLayeredBitmap, AOwned, SelectedImageLayerIndex, AState.SelectionMask, AState.SelectionLayer);
  if AOwned then AState.Free;
end;

function TImageState.GetUndoAfterAssign(ABackup: TImageState): TCustomImageDifference;
begin
  if currentLayeredBitmap = nil then
    result := nil
  else
    result := TAssignStateDifferenceAfter.Create(self, ABackup);
end;

procedure TImageState.LoadFromStream(AStream: TStream);
var loadedLayerIndex: integer;
  loadedLayeredBitmap: TBGRALayeredBitmap;
begin
  loadedLayeredBitmap := LoadLayersFromStream(AStream, loadedLayerIndex, False);
  Assign(loadedLayeredBitmap,True);
  SelectedImageLayerIndex:= loadedLayerIndex;
end;

procedure TImageState.SaveToStream(AStream: TStream);
begin
  SaveLayersToStream(AStream, currentLayeredBitmap, SelectedImageLayerIndex, lzpRLE);
end;

procedure TImageState.SaveToStreamAs(AStream: TStream; AFormat: TBGRAImageFormat);
begin
  if currentLayeredBitmap <> nil then
    currentLayeredBitmap.SaveToStreamAs(AStream, SuggestImageExtension(AFormat));
end;

procedure TImageState.SaveOriginalToStream(AStream: TStream);
begin
  currentLayeredBitmap.SaveOriginalToStream(
    currentLayeredBitmap.LayerOriginalGuid[SelectedImageLayerIndex],
    AStream);
end;

procedure TImageState.SaveToFile(AFilenameUTF8: string);
var
  s: TStream;
begin
  if currentLayeredBitmap <> nil then
  begin
    s := FileManager.CreateFileStream(AFilenameUTF8, fmCreate);
    try
      currentLayeredBitmap.SaveToStream(s);
    finally
      s.Free;
    end;
  end;
end;

function TImageState.AddNewLayer(ALayer: TBGRABitmap; AName: string; ABlendOp: TBlendOperation): TCustomImageDifference;
begin
  //no undo if no previous image
  if currentLayeredBitmap = nil then
  begin
    currentLayeredBitmap := TBGRALayeredBitmap.Create;
    currentLayeredBitmap.AddOwnedLayer(ALayer, ABlendOp);
    result := nil;
  end else
  begin
    result := TAddLayerStateDifference.Create(self, ALayer, AName, ABlendOp);
    ALayer.Free;
  end;
end;

function TImageState.AddNewLayer(AOriginal: TBGRALayerCustomOriginal;
  AName: string; ABlendOp: TBlendOperation): TCustomImageDifference;
begin
  //no undo if no previous image
  if currentLayeredBitmap = nil then
  begin
    currentLayeredBitmap := TBGRALayeredBitmap.Create;
    currentLayeredBitmap.AddLayerFromOwnedOriginal(AOriginal, ABlendOp);
    result := nil;
  end else
    result := TAddLayerFromOwnedOriginalStateDifference.Create(self, AOriginal, AName, ABlendOp);
end;

function TImageState.DuplicateLayer: TCustomImageDifference;
begin
  if currentLayeredBitmap = nil then
    result := nil
  else
    result := TDuplicateLayerStateDifference.Create(self);
end;

function TImageState.MergerLayerOver(ALayerOverIndex: integer): TCustomImageDifference;
begin
  if (currentLayeredBitmap = nil) or (ALayerOverIndex <= 0) or (ALayerOverIndex >= currentLayeredBitmap.NbLayers) then
    result := nil
  else
    result := TMergeLayerOverStateDifference.Create(self, ALayerOverIndex);
end;

function TImageState.MoveLayer(AFromIndex, AToIndex: integer): TCustomImageDifference;
begin
  if currentLayeredBitmap = nil then
    result := nil
  else
    result := TMoveLayerStateDifference.Create(self, AFromIndex, AToIndex);
end;

function TImageState.RemoveLayer: TCustomImageDifference;
begin
  if currentLayeredBitmap = nil then
    result := nil
  else
  begin
    result := TRemoveLayerStateDifference.Create(self);
    result.ApplyTo(self);
  end;
end;

function TImageState.DiscardOriginal(ACreateUndo: boolean): TCustomImageDifference;
var
  prevOriginal: TBGRALayerCustomOriginal;
  prevOriginalMatrix: TAffineMatrix;
  prevOriginalData: TStream;
  prevOriginalGuid: TGuid;
begin
  prevOriginalGuid := currentLayeredBitmap.LayerOriginalGuid[SelectedImageLayerIndex];
  if prevOriginalGuid=GUID_NULL then exit;
  prevOriginalMatrix:= currentLayeredBitmap.LayerOriginalMatrix[SelectedImageLayerIndex];
  currentLayeredBitmap.LayerOriginalGuid[SelectedImageLayerIndex] := GUID_NULL;
  currentLayeredBitmap.LayerOriginalMatrix[SelectedImageLayerIndex] := AffineMatrixIdentity;
  if ACreateUndo then
  begin
    prevOriginalData:= TMemoryStream.Create;
    prevOriginal := currentLayeredBitmap.Original[currentLayeredBitmap.IndexOfOriginal(prevOriginalGuid)];
    prevOriginal.SaveToStream(prevOriginalData);
    result := TImageLayerStateDifference.Create(self, nil,false,nil,false,nil,false,prevOriginalData,prevOriginalMatrix);
  end
  else
    result := nil;
  currentLayeredBitmap.RemoveUnusedOriginals;
end;

function TImageState.HorizontalFlip: TCustomImageDifference;
begin
  if currentLayeredBitmap = nil then
    result := nil
  else
    result := TInversibleStateDifference.Create(self, iaHorizontalFlip);
end;

function TImageState.HorizontalFlip(ALayerIndex: integer): TCustomImageDifference;
begin
  if currentLayeredBitmap = nil then
    result := nil
  else
    result := TInversibleStateDifference.Create(self, iaHorizontalFlipLayer, ALayerIndex);
end;

function TImageState.VerticalFlip: TCustomImageDifference;
begin
  if currentLayeredBitmap = nil then
    result := nil
  else
  result := TInversibleStateDifference.Create(self, iaVerticalFlip);
end;

function TImageState.VerticalFlip(ALayerIndex: integer): TCustomImageDifference;
begin
  if currentLayeredBitmap = nil then
    result := nil
  else
    result := TInversibleStateDifference.Create(self, iaVerticalFlipLayer, ALayerIndex);
end;

function TImageState.SwapRedBlue: TCustomImageDifference;
var
  newImg: TBGRALayeredBitmap;
  newLayer: TBGRABitmap;
  idxLayer, i: Integer;
begin
  if currentLayeredBitmap = nil then
    result := nil
  else
  begin
    if HasOriginals then
    begin
      newImg := TBGRALayeredBitmap.Create(Width,Height);
      for i := 0 to NbLayers-1 do
      begin
        newLayer := TBGRABitmap.Create(Width,Height);
        newLayer.PutImage(LayerOffset[i].x,LayerOffset[i].y, LayerBitmap[i], dmSet);
        newLayer.SwapRedBlue;
        idxLayer := newImg.AddOwnedLayer(newLayer, BlendOperation[i], LayerOpacity[i]);
        newImg.LayerName[idxLayer] := LayerName[i];
      end;
      result := AssignWithUndo(newImg, true, SelectedImageLayerIndex);
    end else
      result := TInversibleStateDifference.Create(self, iaSwapRedBlue);
  end;
end;

function TImageState.LinearNegative: TCustomImageDifference;
var
  newImg: TBGRALayeredBitmap;
  newLayer: TBGRABitmap;
  idxLayer, i: Integer;
begin
  if currentLayeredBitmap = nil then
    result := nil
  else
  begin
    if HasOriginals then
    begin
      newImg := TBGRALayeredBitmap.Create(Width,Height);
      for i := 0 to NbLayers-1 do
      begin
        newLayer := TBGRABitmap.Create(Width,Height);
        newLayer.PutImage(LayerOffset[i].x,LayerOffset[i].y, LayerBitmap[i], dmSet);
        newLayer.LinearNegative;
        idxLayer := newImg.AddOwnedLayer(newLayer, BlendOperation[i], LayerOpacity[i]);
        newImg.LayerName[idxLayer] := LayerName[i];
      end;
      result := AssignWithUndo(newImg, true, SelectedImageLayerIndex);
    end else
      result := TInversibleStateDifference.Create(self, iaLinearNegative);
  end;
end;

function TImageState.Negative: TCustomImageDifference;
var
  newImg: TBGRALayeredBitmap;
  newLayer: TBGRABitmap;
  idxLayer, i: Integer;
begin
  newImg := TBGRALayeredBitmap.Create(Width,Height);
  for i := 0 to NbLayers-1 do
  begin
    newLayer := TBGRABitmap.Create(Width,Height);
    newLayer.PutImage(LayerOffset[i].x,LayerOffset[i].y, LayerBitmap[i], dmSet);
    newLayer.Negative;
    idxLayer := newImg.AddOwnedLayer(newLayer, BlendOperation[i], LayerOpacity[i]);
    newImg.LayerName[idxLayer] := LayerName[i];
  end;
  result := AssignWithUndo(newImg, true, SelectedImageLayerIndex);
end;

function TImageState.RotateCW: TCustomImageDifference;
begin
  if currentLayeredBitmap = nil then
    result := nil
  else
  result := TInversibleStateDifference.Create(self, iaRotateCW);
end;

function TImageState.RotateCCW: TCustomImageDifference;
begin
  if currentLayeredBitmap = nil then
    result := nil
  else
  result := TInversibleStateDifference.Create(self, iaRotateCCW);
end;

function TImageState.ComputeLayerOffsetDifference(AOffsetX, AOffsetY: integer): TCustomImageDifference;
begin
  result := TApplyLayerOffsetStateDifference.Create(self, currentLayeredBitmap.LayerUniqueId[SelectedImageLayerIndex], AOffsetX,AOffsetY, false);
end;

function TImageState.ComputeLayerDifference(APreviousImage: TBGRABitmap;
  APreviousImageDefined: boolean; APreviousSelection: TBGRABitmap;
  APreviousSelectionDefined: boolean; APreviousSelectionLayer: TBGRABitmap;
  APreviousSelectionLayerDefined: boolean; APreviousLayerOriginalData: TStream;
  APreviousLayerOriginalMatrix: TAffineMatrix): TCustomImageDifference;
begin
  if currentLayeredBitmap = nil then
    result := nil
  else
    result := TImageLayerStateDifference.Create(self, APreviousImage, APreviousImageDefined,
      APreviousSelection, APreviousSelectionDefined, APreviousSelectionLayer, APreviousSelectionLayerDefined,
      APreviousLayerOriginalData, APreviousLayerOriginalMatrix);
end;

function TImageState.ComputeLayerDifference(APreviousImage: TBGRABitmap;
  APreviousImageChangeRect: TRect; APreviousSelection: TBGRABitmap;
  APreviousSelectionChangeRect: TRect; APreviousSelectionLayer: TBGRABitmap;
  APreviousSelectionLayerChangeRect: TRect; APreviousLayerOriginalData: TStream;
  APreviousLayerOriginalMatrix: TAffineMatrix): TCustomImageDifference;
begin
  if currentLayeredBitmap = nil then
    result := nil
  else
    result := TImageLayerStateDifference.Create(self, APreviousImage, APreviousImageChangeRect,
      APreviousSelection, APreviousSelectionChangeRect, APreviousSelectionLayer, APreviousSelectionLayerChangeRect,
      APreviousLayerOriginalData, APreviousLayerOriginalMatrix);
end;

function TImageState.GetLayeredBitmapCopy: TBGRALayeredBitmap;
begin
  result := currentLayeredBitmap.Duplicate;
end;

function TImageState.ComputeFlatImage(AFromLayer, AToLayer: integer; ASeparateXorMask: boolean): TBGRABitmap;
begin
  result := currentLayeredBitmap.ComputeFlatImage(AFromLayer,AToLayer,ASeparateXorMask);
end;

function TImageState.ComputeFlatImage(ARect: TRect; AFromLayer,
  AToLayer: integer; ASeparateXorMask: boolean): TBGRABitmap;
begin
  result := currentLayeredBitmap.ComputeFlatImage(ARect,AFromLayer,AToLayer,ASeparateXorMask);
end;

procedure TImageState.DrawLayers(ADest: TBGRABitmap; X, Y: Integer; AIconCursor: boolean);
begin
  if currentLayeredBitmap <> nil then
    currentLayeredBitmap.Draw(ADest,X,Y, AIconCursor);
end;

constructor TImageState.Create;
begin
  currentLayeredBitmap := nil;
  SelectionMask := nil;
  SelectionLayer := nil;
  selectedLayerId := -1;
end;

destructor TImageState.Destroy;
begin
  currentLayeredBitmap.Free;
  SelectionMask.free;
  SelectionLayer.Free;
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
    if (other.SelectionMask = nil) and (SelectionMask <> nil) and not SelectionMask.Equals(BGRABlack) then exit;
    if (other.SelectionMask <> nil) and (SelectionMask = nil) and not other.SelectionMask.Equals(BGRABlack) then exit;
    if (other.SelectionMask <> nil) and (SelectionMask <> nil) and not other.SelectionMask.Equals(SelectionMask) then exit;
    if (other.SelectionLayer = nil) and (SelectionLayer <> nil) and not SelectionLayer.Empty then exit;
    if (other.SelectionLayer <> nil) and (SelectionLayer = nil) and not other.SelectionLayer.Empty then exit;
    if (other.SelectionLayer <> nil) and (SelectionLayer <> nil) and not other.SelectionLayer.Equals(SelectionLayer) then exit;
    if (other.filenameUTF8 <> filenameUTF8) then exit;
    result := true;
  end
  else
    Result:=inherited Equals(Obj);
end;

procedure TImageState.ApplyDifference(ADifference: TStateDifference);
begin
  ADifference.ApplyTo(self);
end;

procedure TImageState.ReverseDifference(ADifference: TStateDifference);
begin
  ADifference.UnapplyTo(self);
end;

function TImageState.Duplicate: TState;
var copy: TImageState;
begin
  copy := TImageState.Create;
  copy.currentLayeredBitmap := DuplicateLayeredBitmap(currentLayeredBitmap);
  copy.SelectionMask := DuplicateBitmap(SelectionMask);
  copy.SelectionLayer := DuplicateBitmap(SelectionLayer);
  copy.selectedLayerId := selectedLayerId;
  result := copy;
end;

procedure TImageState.Resample(AWidth, AHeight: integer;
  AQuality: TResampleMode; AFilter: TResampleFilter);
begin
  currentLayeredBitmap.Resample(AWidth,AHeight,AQuality,AFilter);
  if SelectionMask <> nil then
  begin
    SelectionMask.ResampleFilter := AFilter;
    BGRAReplace(FSelectionMask, FSelectionMask.Resample(AWidth, AHeight,AQuality));
  end;
  if SelectionLayer <> nil then
  begin
    SelectionLayer.ResampleFilter := AFilter;
    BGRAReplace(SelectionLayer, SelectionLayer.Resample(AWidth, AHeight,AQuality));
  end;
end;

end.

