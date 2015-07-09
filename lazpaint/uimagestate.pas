unit UImageState;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UStateType, BGRABitmap, BGRABitmapTypes, Types, BGRALayers,
  UImageType, BGRAWriteLzp, BGRAReadLzp, FPimage;

type
  { TImageState }

  TImageState = class(TState)
  private
    function GetBlendOp(Index: integer): TBlendOperation;
    function GetCurrentLayer: TBGRABitmap;
    function GetCurrentLayerIndex: integer;
    function GetHeight: integer;
    function GetLayerBitmap(Index: integer): TBGRABitmap;
    function GetLayerBitmapById(AId: integer): TBGRABitmap;
    function GetLayerId(Index: integer): integer;
    function GetLayerName(Index: integer): string;
    function GetLayerOffset(Index: integer): TPoint;
    function GetLayerOpacity(Index: integer): byte;
    function GetLayerVisible(Index: integer): boolean;
    function GetLinearBlend: boolean;
    function GetNbLayers: integer;
    function GetWidth: integer;
    procedure SetCurrentLayer(AValue: TBGRABitmap);
    procedure SetCurrentLayerIndex(AValue: integer);
    procedure SetLinearBlend(AValue: boolean);
  public
    currentLayeredBitmap: TBGRALayeredBitmap;
    currentSelection, selectionLayer: TBGRABitmap;
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
    function ComputeFlatImageWithoutSelection: TBGRABitmap;
    procedure Assign(AValue: TBGRABitmap; AOwned: boolean);
    procedure Assign(AValue: TBGRALayeredBitmap; AOwned: boolean);
    procedure Assign(AValue: TImageState; AOwned: boolean);
    function AssignWithUndo(AValue: TBGRALayeredBitmap; AOwned: boolean; ASelectedLayerIndex: integer): TCustomImageDifference;
    function AssignWithUndo(AValue: TBGRALayeredBitmap; AOwned: boolean; ASelectedLayerIndex: integer; ACurrentSelection: TBGRABitmap; ASelectionLayer:TBGRABitmap): TCustomImageDifference;
    function AssignWithUndo(AState: TImageState; AOwned: boolean): TCustomImageDifference;
    function GetUndoAfterAssign(ABackup: TImageState): TCustomImageDifference;
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
    procedure SaveToFile(AFilenameUTF8: string);
    procedure AdaptLayers;
    function AddNewLayer(ALayer: TBGRABitmap; AName: string): TCustomImageDifference;
    function DuplicateLayer: TCustomImageDifference;
    function MergerLayerOver(ALayerOverIndex: integer): TCustomImageDifference;
    function MoveLayer(AFromIndex,AToIndex: integer): TCustomImageDifference;
    function RemoveLayer: TCustomImageDifference;
    function HorizontalFlip: TCustomImageDifference;
    function VerticalFlip: TCustomImageDifference;
    function SwapRedBlue: TCustomImageDifference;
    function LinearNegative: TCustomImageDifference;
    function RotateCW: TCustomImageDifference;
    function RotateCCW: TCustomImageDifference;
    function ApplyLayerOffset(AOffsetX, AOffsetY: integer): TCustomImageDifference;
    function ComputeLayerDifference(APreviousImage: TBGRABitmap; APreviousImageDefined: boolean;
        APreviousSelection: TBGRABitmap; APreviousSelectionDefined: boolean;
        APreviousSelectionLayer: TBGRABitmap; APreviousSelectionLayerDefined: boolean): TCustomImageDifference; overload;
    function ComputeLayerDifference(APreviousImage: TBGRABitmap; APreviousImageChangeRect: TRect;
        APreviousSelection: TBGRABitmap; APreviousSelectionChangeRect: TRect;
        APreviousSelectionLayer: TBGRABitmap; APreviousSelectionLayerChangeRect: TRect): TCustomImageDifference; overload;
    function GetLayeredBitmapCopy: TBGRALayeredBitmap;
    function ComputeFlatImage(AFromLayer, AToLayer: integer): TBGRABitmap;
    function SetLayerName(Index: integer; AValue: string): TCustomImageDifference;
    function SetLayerOpacity(Index: integer; AValue: byte): TCustomImageDifference;
    function SetLayerVisible(Index: integer; AValue: boolean): TCustomImageDifference;
    function SetLayerOffset(Index: integer; AValue: TPoint): TCustomImageDifference;
    function SetBlendOp(Index: integer; AValue: TBlendOperation): TCustomImageDifference;
    procedure DrawLayers(ADest: TBGRABitmap; X,Y: Integer);
    property currentLayer: TBGRABitmap read GetCurrentLayer write SetCurrentLayer;
    property currentLayerIndex: integer read GetCurrentLayerIndex write SetCurrentLayerIndex;
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
  end;

  { TBGRAWriterLazPaintWithLayers }

  TBGRAWriterLazPaintWithLayers = class(TBGRAWriterLazPaint)
    protected
      FState: TImageState;
      function GetNbLayers: integer; override;
      function InternalWriteLayers(Str: TStream; {%H-}Img: TFPCustomImage): boolean; override;
    public
      constructor Create(AState: TImageState); overload;
  end;

  { TBGRAReaderLazPaintWithLayers }

  TBGRAReaderLazPaintWithLayers = class(TBGRAReaderLazPaint)
    protected
      FState: TImageState;
      FLayersLoaded: boolean;
      procedure InternalReadLayers(str: TStream; {%H-}Img: TFPCustomImage); override;
    public
      constructor Create(AState: TImageState); overload;
      property LayersLoaded: boolean read FLayersLoaded;
  end;

implementation

uses BGRAStreamLayers, UImageDiff, BGRALzpCommon;

{ TBGRAReaderLazPaintWithLayers }

procedure TBGRAReaderLazPaintWithLayers.InternalReadLayers(str: TStream;
  Img: TFPCustomImage);
begin
  if Assigned(FState) then
  begin
    if (Caption = 'Preview') and CheckStreamForLayers(str) then
    begin
      FState.LoadFromStream(str);
      FLayersLoaded := true;
    end;
  end;
end;

constructor TBGRAReaderLazPaintWithLayers.Create(AState: TImageState);
begin
  FLayersLoaded := false;
  FState := AState;
end;

{ TBGRAWriterLazPaintWithLayers }

function TBGRAWriterLazPaintWithLayers.GetNbLayers: integer;
begin
  if Assigned(FState) then
    Result:= FState.NbLayers
  else
    Result := 1;
end;

function TBGRAWriterLazPaintWithLayers.InternalWriteLayers(Str: TStream;
  Img: TFPCustomImage): boolean;
begin
  If Assigned(FState) then
  begin
    FState.SaveToStream(Str);
    Result:=true;
  end;
end;

constructor TBGRAWriterLazPaintWithLayers.Create(AState: TImageState);
begin
  inherited Create;
  FState := AState;
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

procedure TImageState.SetCurrentLayer(AValue: TBGRABitmap);
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

procedure TImageState.SetCurrentLayerIndex(AValue: integer);
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
    currentLayeredBitmap.FreezeExceptOneLayer(currentLayerIndex);
end;

function TImageState.ComputeFlatImageWithoutSelection: TBGRABitmap;
begin
  if currentLayeredBitmap <> nil then
    result := currentLayeredBitmap.ComputeFlatImage
  else
    result := TBGRABitmap.Create(Width,Height);
end;

procedure TImageState.Assign(AValue: TBGRABitmap; AOwned: boolean);
begin
  if currentLayeredBitmap = nil then
    currentLayeredBitmap := TBGRALayeredBitmap.Create;

  currentLayeredBitmap.Clear;
  currentLayeredBitmap.SetSize(AValue.Width,AValue.Height);
  if AOwned then
    currentLayeredBitmap.AddOwnedLayer(AValue)
  else
    currentLayeredBitmap.AddLayer(AValue);
  currentLayerIndex := 0;
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
    currentLayerIndex := 0
  end
  else
    currentLayerIndex := -1;
end;

procedure TImageState.Assign(AValue: TImageState; AOwned: boolean);
begin
  Assign(AValue.currentLayeredBitmap, AOwned);
  if AOwned then AValue.currentLayeredBitmap := nil;
  BGRAReplace(currentSelection, AValue.currentSelection);
  if AOwned then AValue.currentSelection := nil;
  BGRAReplace(selectionLayer, AValue.selectionLayer);
  if AOwned then AValue.selectionLayer := nil;
  if AOwned then AValue.Free;
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
  result := AssignWithUndo(AState.currentLayeredBitmap, AOwned, currentLayerIndex, AState.currentSelection, AState.selectionLayer);
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
  currentLayerIndex:= loadedLayerIndex;
end;

procedure TImageState.SaveToStream(AStream: TStream);
begin
  SaveLayersToStream(AStream, currentLayeredBitmap, currentLayerIndex, lzpRLE);
end;

procedure TImageState.SaveToFile(AFilenameUTF8: string);
begin
  if currentLayeredBitmap <> nil then
    currentLayeredBitmap.SaveToFile(AFilenameUTF8);
end;

procedure TImageState.AdaptLayers;
var
  i: integer;
  newbmp: TBGRABitmap;
begin
  with currentLayeredBitmap do
  for i := 0 to NbLayers-1 do
    if (LayerBitmap[i].Width <> Width) or
      (LayerBitmap[i].Height <> Height) or
      (LayerOffset[i].x <> 0) or
      (LayerOffset[i].y <> 0) then
    begin
      newbmp := TBGRABitmap.Create(Width,Height);
      newbmp.PutImage(LayerOffset[i].x,LayerOffset[i].y,LayerBitmap[i],dmSet);
      SetLayerBitmap(i,newbmp,true);
      LayerOffset[i] := Point(0,0);
    end;
end;

function TImageState.AddNewLayer(ALayer: TBGRABitmap; AName: string): TCustomImageDifference;
begin
  //no undo if no previous image
  if currentLayeredBitmap = nil then
  begin
    currentLayeredBitmap := TBGRALayeredBitmap.Create;
    currentLayeredBitmap.AddOwnedLayer(ALayer);
    result := nil;
  end else
  begin
    result := TAddLayerStateDifference.Create(self, ALayer, AName);
    ALayer.Free;
  end;
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
    result := TRemoveLayerStateDifference.Create(self);
end;

function TImageState.HorizontalFlip: TCustomImageDifference;
begin
  if currentLayeredBitmap = nil then
    result := nil
  else
    result := TInversibleStateDifference.Create(self, iaHorizontalFlip);
end;

function TImageState.VerticalFlip: TCustomImageDifference;
begin
  if currentLayeredBitmap = nil then
    result := nil
  else
  result := TInversibleStateDifference.Create(self, iaVerticalFlip);
end;

function TImageState.SwapRedBlue: TCustomImageDifference;
begin
  if currentLayeredBitmap = nil then
    result := nil
  else
    result := TInversibleStateDifference.Create(self, iaSwapRedBlue);
end;

function TImageState.LinearNegative: TCustomImageDifference;
begin
  if currentLayeredBitmap = nil then
    result := nil
  else
    result := TInversibleStateDifference.Create(self, iaLinearNegative);
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

function TImageState.ApplyLayerOffset(AOffsetX,AOffsetY: integer): TCustomImageDifference;
begin
  result := TApplyLayerOffsetStateDifference.Create(self, currentLayeredBitmap.LayerUniqueId[currentLayerIndex], AOffsetX,AOffsetY);
end;

function TImageState.ComputeLayerDifference(APreviousImage: TBGRABitmap;
  APreviousImageDefined: boolean; APreviousSelection: TBGRABitmap;
  APreviousSelectionDefined: boolean; APreviousSelectionLayer: TBGRABitmap;
  APreviousSelectionLayerDefined: boolean): TCustomImageDifference;
begin
  if currentLayeredBitmap = nil then
    result := nil
  else
    result := TImageLayerStateDifference.Create(self, APreviousImage, APreviousImageDefined,
      APreviousSelection, APreviousSelectionDefined, APreviousSelectionLayer, APreviousSelectionLayerDefined);
end;

function TImageState.ComputeLayerDifference(APreviousImage: TBGRABitmap;
  APreviousImageChangeRect: TRect; APreviousSelection: TBGRABitmap;
  APreviousSelectionChangeRect: TRect; APreviousSelectionLayer: TBGRABitmap;
  APreviousSelectionLayerChangeRect: TRect): TCustomImageDifference;
begin
  if currentLayeredBitmap = nil then
    result := nil
  else
    result := TImageLayerStateDifference.Create(self, APreviousImage, APreviousImageChangeRect,
      APreviousSelection, APreviousSelectionChangeRect, APreviousSelectionLayer, APreviousSelectionLayerChangeRect);
end;

function TImageState.GetLayeredBitmapCopy: TBGRALayeredBitmap;
begin
  result := currentLayeredBitmap.Duplicate;
end;

function TImageState.ComputeFlatImage(AFromLayer, AToLayer: integer
  ): TBGRABitmap;
begin
  result := currentLayeredBitmap.ComputeFlatImage(AFromLayer,AToLayer);
end;

procedure TImageState.DrawLayers(ADest: TBGRABitmap; X, Y: Integer);
begin
  if currentLayeredBitmap <> nil then
    currentLayeredBitmap.Draw(ADest,X,Y);
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
  copy.currentSelection := DuplicateBitmap(currentSelection);
  copy.selectionLayer := DuplicateBitmap(selectionLayer);
  copy.selectedLayerId := selectedLayerId;
  result := copy;
end;

procedure TImageState.Resample(AWidth, AHeight: integer;
  AQuality: TResampleMode; AFilter: TResampleFilter);
begin
  currentLayeredBitmap.Resample(AWidth,AHeight,AQuality,AFilter);
  if currentSelection <> nil then
  begin
    currentSelection.ResampleFilter := AFilter;
    BGRAReplace(currentSelection, currentSelection.Resample(AWidth, AHeight,AQuality));
  end;
  if selectionLayer <> nil then
  begin
    selectionLayer.ResampleFilter := AFilter;
    BGRAReplace(selectionLayer, selectionLayer.Resample(AWidth, AHeight,AQuality));
  end;
end;

end.

