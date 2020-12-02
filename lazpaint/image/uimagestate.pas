// SPDX-License-Identifier: GPL-3.0-only
unit UImageState;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UStateType, BGRABitmap, BGRABitmapTypes, Types, BGRALayers,
  UImageType, BGRAWriteLzp, BGRAReadLzp, FPimage, BGRALayerOriginal;

type
  TBoundsState = (bsUndefined, bsWithinCurrent, bsDefined);

  { TImageState }

  TImageState = class(TState)
  private
    FLayeredBitmap: TBGRALayeredBitmap;
    FOnActionDone: TNotifyEvent;
    FOnActionProgress: TLayeredActionProgressEvent;
    FOnOriginalChange: TEmbeddedOriginalChangeEvent;
    FOnOriginalEditingChange: TEmbeddedOriginalEditingChangeEvent;
    FOnOriginalLoadError: TEmbeddedOriginalLoadErrorEvent;
    FOnSizeChanged: TNotifyEvent;
    FSelectionMask: TBGRABitmap;
    FLastSelectionMaskBoundsIsDefined,
    FLastSelectionLayerBoundsIsDefined: TBoundsState;
    FLastSelectionMaskBounds, FLastSelectionLayerBounds: TRect;
    FSelectionTransform: TAffineMatrix;
    function GetBlendOp(Index: integer): TBlendOperation;
    function GetSelectedImageLayer: TBGRABitmap;
    function GetCurrentLayerIndex: integer;
    function GetSelectionMask: TBGRABitmap;
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
    function GetLayerOriginalClass(Index: integer): TBGRALayerOriginalAny;
    function GetLayerOriginalMatrix(Index: integer): TAffineMatrix;
    function GetLayerVisible(Index: integer): boolean;
    function GetLinearBlend: boolean;
    function GetNbLayers: integer;
    function GetWidth: integer;
    procedure LayeredActionDone(Sender: TObject);
    procedure LayeredActionProgress({%H-}ASender: TObject; AProgressPercent: integer);
    procedure OriginalChange({%H-}ASender: TObject;
      AOriginal: TBGRALayerCustomOriginal; var ADiff: TBGRAOriginalDiff);
    procedure OriginalEditingChange({%H-}ASender: TObject;
      AOriginal: TBGRALayerCustomOriginal);
    procedure OriginalLoadError(ASender: TObject; AError: string;
      var ARaise: boolean);
    procedure SelectImageLayer(AValue: TBGRABitmap);
    procedure SelectImageLayerByIndex(AValue: integer);
    procedure SetLayeredBitmap(AValue: TBGRALayeredBitmap);
    procedure SetLinearBlend(AValue: boolean);
    procedure SetOnActionDone(AValue: TNotifyEvent);
    procedure SetOnActionProgress(AValue: TLayeredActionProgressEvent);
    procedure SetOnSizeChanged(AValue: TNotifyEvent);
    procedure SetSelectionMask(AValue: TBGRABitmap);
    function MakeLayerName: string;
    procedure CheckSizeChanged(APrevSize: TSize);
    function GetSize: TSize;
  public
    SelectionLayer: TBGRABitmap;
    selectedLayerId: integer;
    filenameUTF8: string;
    DiscardOriginalDiff: boolean;

    // generic state functions
    constructor Create;
    destructor Destroy; override;
    function Equals(Obj: TObject): boolean; override;
    procedure ApplyDifference(ADifference: TStateDifference); override;
    procedure ReverseDifference(ADifference: TStateDifference); override;
    function Duplicate: TState; override;

    // whole image
    procedure SetSize(AWidth,AHeight: integer);
    procedure NotifySizeChanged;
    procedure Assign(AValue: TBGRABitmap; AOwned: boolean);
    procedure Assign(AValue: TBGRACustomLayeredBitmap; AOwned: boolean);
    procedure Assign(AValue: TImageState; AOwned: boolean);

    function RotateCW: TCustomImageDifference;
    function RotateCCW: TCustomImageDifference;
    function Rotate180: TCustomImageDifference;
    function HorizontalFlip: TCustomImageDifference; overload;
    function VerticalFlip: TCustomImageDifference; overload;
    procedure Resample(AWidth,AHeight: integer; AQuality: TResampleMode; AFilter: TResampleFilter);

    // layer
    procedure SetLayerBitmap(layer: integer; ABitmap: TBGRABitmap; AOwned: boolean);
    function HorizontalFlip(ALayerIndex: integer): TCustomImageDifference; overload;
    function VerticalFlip(ALayerIndex: integer): TCustomImageDifference; overload;

    // selection mask
    procedure QuerySelectionMask;
    procedure ReplaceSelection(ASelectionMask, ASelectionLayer: TBGRABitmap);
    procedure RemoveSelection;

    procedure DiscardSelectionMaskBounds(const AChangeRect: TRect);
    procedure DiscardSelectionMaskBoundsCompletely;
    function GetSelectionMaskBounds: TRect;
    function SelectionMaskEmpty: boolean;
    function SelectionMaskEmptyComputed: boolean;
    function GetTransformedSelectionMaskBounds: TRect;
    procedure ComputeTransformedSelectionMask(out ANewMask: TBGRABitmap; out ALeft,ATop: integer);

    // selection layer
    function GetOrCreateSelectionLayer: TBGRABitmap;
    procedure ReplaceSelectionLayer(bmp: TBGRABitmap; AOwned: boolean);
    procedure ComputeTransformedSelectionLayer(out ANewLayer: TBGRABitmap; out ALeft,ATop: integer);

    procedure DiscardSelectionLayerBounds(const AChangeRect: TRect);
    procedure DiscardSelectionLayerBoundsCompletely;
    function GetSelectionLayerBounds: TRect;
    function SelectionLayerEmpty: boolean;

    procedure PrepareForRendering;
    function ComputeFlatImageWithoutSelection(ASeparateXorMask: boolean): TBGRABitmap;
    function AssignWithUndo(AValue: TBGRACustomLayeredBitmap; AOwned: boolean; ASelectedLayerIndex: integer): TCustomImageDifference;
    function AssignWithUndo(AValue: TBGRACustomLayeredBitmap; AOwned: boolean; ASelectedLayerIndex: integer; ACurrentSelection: TBGRABitmap; ASelectionLayer:TBGRABitmap): TCustomImageDifference;
    function AssignWithUndo(AState: TImageState; AOwned: boolean): TCustomImageDifference;
    function GetUndoAfterAssign(ABackup: TImageState): TCustomImageDifference;
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
    procedure SaveToStreamAs(AStream: TStream; AFormat: TBGRAImageFormat);
    procedure SaveOriginalToStream(AStream: TStream);
    procedure SaveToFile(AFilenameUTF8: string);
    function AddNewLayer(ALayer: TBGRABitmap; AName: string; AOffset: TPoint; ABlendOp: TBlendOperation; AOpacity: byte = 255): TCustomImageDifference;
    function AddNewLayer(AOriginal: TBGRALayerCustomOriginal; AName: string; ABlendOp: TBlendOperation; AMatrix: TAffineMatrix; AOpacity: byte = 255): TCustomImageDifference;
    function DuplicateLayer: TCustomImageDifference;
    function MergerLayerOver(ALayerOverIndex: integer): TCustomImageDifference;
    function MoveLayer(AFromIndex,AToIndex: integer): TCustomImageDifference;
    function RemoveLayer: TCustomImageDifference;
    function DiscardOriginal(ACreateUndo: boolean): TCustomImageDifference;
    function SwapRedBlue: TCustomImageDifference;
    function LinearNegative: TCustomImageDifference;
    function Negative: TCustomImageDifference;
    function ClearLayer: TCustomImageDifference;
    function ComputeLayerOffsetDifference(AOffsetX, AOffsetY: integer): TCustomImageDifference;
    function ComputeSelectionTransformDifference: TCustomImageDifference;
    function ComputeLayerMatrixDifference(AIndex: integer; APrevMatrix, ANewMatrix: TAffineMatrix): TCustomImageDifference;
    function ComputeLayerDifference(APreviousImage: TBGRABitmap; APreviousImageDefined: boolean;
        APreviousSelection: TBGRABitmap; APreviousSelectionDefined: boolean;
        APreviousSelectionLayer: TBGRABitmap; APreviousSelectionLayerDefined: boolean): TCustomImageDifference; overload;
    function ComputeLayerDifference(APreviousImage: TBGRABitmap; APreviousImageChangeRect: TRect;
        APreviousSelection: TBGRABitmap; APreviousSelectionChangeRect: TRect;
        APreviousSelectionLayer: TBGRABitmap; APreviousSelectionLayerChangeRect: TRect): TCustomImageDifference; overload;
    function GetLayeredBitmapCopy: TBGRALayeredBitmap;
    function ComputeFlatImage(AFromLayer, AToLayer: integer; ASeparateXorMask: boolean): TBGRABitmap;
    function ComputeFlatImage(ARect: TRect; AFromLayer, AToLayer: integer; ASeparateXorMask: boolean): TBGRABitmap;
    function SetLayerName(Index: integer; AValue: string): TCustomImageDifference;
    function SetLayerOpacity(Index: integer; AValue: byte): TCustomImageDifference;
    function SetLayerVisible(Index: integer; AValue: boolean): TCustomImageDifference;
    function SetLayerOffset(Index: integer; AValue: TPoint): TCustomImageDifference;
    function SetBlendOp(Index: integer; AValue: TBlendOperation): TCustomImageDifference;
    procedure DrawLayers(ADest: TBGRABitmap; X,Y: Integer; AIconCursor: boolean; ADestinationEmpty: boolean = false);
    property SelectedImageLayer: TBGRABitmap read GetSelectedImageLayer write SelectImageLayer;
    property SelectedImageLayerIndex: integer read GetCurrentLayerIndex write SelectImageLayerByIndex;
    property LayerOriginal[Index: integer]: TBGRALayerCustomOriginal read GetLayerOriginal;
    property LayerOriginalDefined[Index: integer]: boolean read GetLayerOriginalDefined;
    property LayerOriginalKnown[Index: integer]: boolean read GetLayerOriginalKnown;
    property LayerOriginalClass[Index: integer]: TBGRALayerOriginalAny read GetLayerOriginalClass;
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
    property SelectionMask: TBGRABitmap read GetSelectionMask write SetSelectionMask;
    property LayeredBitmap: TBGRALayeredBitmap read FLayeredBitmap;
    property SelectionTransform: TAffineMatrix read FSelectionTransform write FSelectionTransform;
    property OnOriginalChange: TEmbeddedOriginalChangeEvent read FOnOriginalChange write FOnOriginalChange;
    property OnOriginalEditingChange: TEmbeddedOriginalEditingChangeEvent read FOnOriginalEditingChange write FOnOriginalEditingChange;
    property OnOriginalLoadError: TEmbeddedOriginalLoadErrorEvent read FOnOriginalLoadError write FOnOriginalLoadError;
    property OnActionProgress: TLayeredActionProgressEvent read FOnActionProgress write SetOnActionProgress;
    property OnActionDone: TNotifyEvent read FOnActionDone write SetOnActionDone;
    property OnSizeChanged: TNotifyEvent read FOnSizeChanged write SetOnSizeChanged;
  end;

implementation

uses BGRAStreamLayers, UImageDiff, BGRALzpCommon, UFileSystem, BGRATransform,
  UResourceStrings, LCVectorOriginal, UGraph;

{ TImageState }

function TImageState.GetSelectedImageLayer: TBGRABitmap;
var idx: integer;
begin
  if LayeredBitmap = nil then
  begin
    result := nil;
    exit;
  end else
  begin
    idx := LayeredBitmap.GetLayerIndexFromId(selectedLayerId);
    if idx = -1 then result := nil else
      result := LayeredBitmap.LayerBitmap[idx]; //assume direct access to bitmap
  end;
end;

function TImageState.GetBlendOp(Index: integer): TBlendOperation;
begin
  if LayeredBitmap = nil then
    result := boTransparent
  else
    result := LayeredBitmap.BlendOperation[Index];
end;

function TImageState.GetLayerOriginalClass(Index: integer): TBGRALayerOriginalAny;
begin
  if LayeredBitmap = nil then
    result := nil
  else
    result := LayeredBitmap.LayerOriginalClass[Index];
end;

function TImageState.GetCurrentLayerIndex: integer;
begin
  if LayeredBitmap = nil then
  begin
    result := -1;
  end else
    result := LayeredBitmap.GetLayerIndexFromId(selectedLayerId);
end;

function TImageState.GetSelectionMask: TBGRABitmap;
begin
  result := FSelectionMask;
  if Assigned(result) then result.LinearAntialiasing := true;
end;

function TImageState.GetHasOriginals: boolean;
var
  i: Integer;
begin
  if LayeredBitmap= nil then
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
  if LayeredBitmap= nil then
    result := 0
  else
    result := LayeredBitmap.Height;
end;

function TImageState.GetLayerBitmap(Index: integer): TBGRABitmap;
begin
  result := LayeredBitmap.LayerBitmap[Index];
end;

function TImageState.GetLayerBitmapById(AId: integer): TBGRABitmap;
var idx: integer;
begin
  idx := LayeredBitmap.GetLayerIndexFromId(AId);
  if idx = -1 then
    result := nil
  else
    result := LayeredBitmap.LayerBitmap[idx];
end;

function TImageState.GetLayerId(Index: integer): integer;
begin
  if LayeredBitmap = nil then
    result := -1
  else
    result := LayeredBitmap.LayerUniqueId[index];
end;

function TImageState.GetLayerName(Index: integer): string;
begin
  if LayeredBitmap = nil then
    result := ''
  else
    result := LayeredBitmap.LayerName[index];
end;

function TImageState.GetLayerOffset(Index: integer): TPoint;
begin
  if LayeredBitmap = nil then
    result := point(0,0)
  else
    result := LayeredBitmap.LayerOffset[index];
end;

function TImageState.GetLayerOpacity(Index: integer): byte;
begin
  if LayeredBitmap = nil then
    result := 255
  else
    result := LayeredBitmap.LayerOpacity[index];
end;

function TImageState.GetLayerOriginal(Index: integer): TBGRALayerCustomOriginal;
begin
  if LayeredBitmap = nil then
    result := nil
  else
    result := LayeredBitmap.LayerOriginal[Index];
end;

function TImageState.GetLayerOriginalDefined(Index: integer): boolean;
begin
  if LayeredBitmap = nil then
    result := false
  else
    result := LayeredBitmap.LayerOriginalGuid[Index] <> GUID_NULL;
end;

function TImageState.GetLayerOriginalKnown(Index: integer): boolean;
begin
  if LayeredBitmap = nil then
    result := false
  else
    result := LayeredBitmap.LayerOriginalKnown[Index];
end;

function TImageState.GetLayerOriginalMatrix(Index: integer): TAffineMatrix;
begin
  if LayeredBitmap = nil then
    result := AffineMatrixIdentity
  else
    result := LayeredBitmap.LayerOriginalMatrix[Index];
end;

function TImageState.GetLayerVisible(Index: integer): boolean;
begin
  if LayeredBitmap = nil then
    result := false
  else
    result := LayeredBitmap.LayerVisible[Index];
end;

function TImageState.GetLinearBlend: boolean;
begin
  if LayeredBitmap = nil then
    result := true
  else
    result := LayeredBitmap.LinearBlend;
end;

function TImageState.GetNbLayers: integer;
begin
  if LayeredBitmap = nil then
    result := 0
  else
    result := LayeredBitmap.NbLayers;
end;

function TImageState.GetWidth: integer;
begin
  if LayeredBitmap= nil then
    result := 0
  else
    result := LayeredBitmap.Width;
end;

procedure TImageState.LayeredActionDone(Sender: TObject);
begin
  if Assigned(FOnActionDone) then
    FOnActionDone(self);
end;

procedure TImageState.LayeredActionProgress(ASender: TObject;
  AProgressPercent: integer);
begin
  if Assigned(FOnActionProgress) then
    FOnActionProgress(self, AProgressPercent);
end;

procedure TImageState.OriginalChange(ASender: TObject;
  AOriginal: TBGRALayerCustomOriginal; var ADiff: TBGRAOriginalDiff);
begin
  If Assigned(FOnOriginalChange) then
  begin
    if DiscardOriginalDiff then FreeAndNil(ADiff);
    FOnOriginalChange(self, AOriginal, ADiff);
  end;
end;

procedure TImageState.OriginalEditingChange(ASender: TObject;
  AOriginal: TBGRALayerCustomOriginal);
begin
  If Assigned(FOnOriginalEditingChange) then
    FOnOriginalEditingChange(self, AOriginal);
end;

procedure TImageState.OriginalLoadError(ASender: TObject; AError: string;
  var ARaise: boolean);
begin
  If Assigned(FOnOriginalLoadError) then
    FOnOriginalLoadError(self, AError, ARaise);
end;

procedure TImageState.SelectImageLayer(AValue: TBGRABitmap);
var
  i: Integer;
begin
  if LayeredBitmap = nil then exit;

  for i := 0 to NbLayers-1 do
    if LayeredBitmap.LayerBitmap[i] = AValue then
    begin
      selectedLayerId := LayeredBitmap.LayerUniqueId[i];
      exit;
    end;
  selectedLayerId := -1;
end;

procedure TImageState.SelectImageLayerByIndex(AValue: integer);
begin
  if (LayeredBitmap = nil) or (AValue < 0) or (AValue >= LayeredBitmap.NbLayers) then
  begin
    selectedLayerId := -1;
  end else
  begin
    selectedLayerId := LayeredBitmap.LayerUniqueId[AValue];
    LayeredBitmap.Unfreeze(AValue);
  end;
end;

procedure TImageState.SetLayeredBitmap(AValue: TBGRALayeredBitmap);
begin
  if FLayeredBitmap=AValue then Exit;

  if Assigned(FLayeredBitmap) then
  begin
    FLayeredBitmap.OnOriginalChange:= nil;
    FLayeredBitmap.OnOriginalEditingChange:= nil;
    FLayeredBitmap.OnActionProgress:= nil;
    FLayeredBitmap.OnActionDone:= nil;
    FLayeredBitmap.OnOriginalLoadError:= nil;
  end;
  FLayeredBitmap:=AValue;
  if Assigned(FLayeredBitmap) then
  begin
    FLayeredBitmap.OnOriginalChange:= @OriginalChange;
    FLayeredBitmap.OnOriginalEditingChange:= @OriginalEditingChange;
    FLayeredBitmap.OnActionProgress:= @LayeredActionProgress;
    FLayeredBitmap.OnActionDone:=@LayeredActionDone;
    FLayeredBitmap.OnOriginalLoadError:=@OriginalLoadError;
  end;
end;

function TImageState.SetLayerName(Index: integer; AValue: string): TCustomImageDifference;
begin
  if LayeredBitmap <> nil then
  begin
    if LayerName[Index] <> AValue then
      result := TSetLayerNameStateDifference.Create(self,LayeredBitmap.LayerUniqueId[index],AValue)
    else
      result := nil;
  end
  else
    result := nil;
end;

function TImageState.SetLayerOffset(Index: integer; AValue: TPoint
  ): TCustomImageDifference;
begin
  if LayeredBitmap <> nil then
  begin
    if (LayerOffset[index].x <> AValue.x) or (LayerOffset[index].y <> AValue.y) then
      result := TSetLayerOffsetStateDifference.Create(self,LayeredBitmap.LayerUniqueId[index],AValue)
    else
      result := nil;
  end
  else
    result := nil;
end;

function TImageState.SetLayerOpacity(Index: integer; AValue: byte
  ): TCustomImageDifference;
begin
  if LayeredBitmap <> nil then
  begin
    if LayerOpacity[index] <> AValue then
      result := TSetLayerOpacityStateDifference.Create(self,LayeredBitmap.LayerUniqueId[index],AValue)
    else
      result := nil;
  end
  else
    result := nil;
end;

function TImageState.SetLayerVisible(Index: integer; AValue: boolean
  ): TCustomImageDifference;
begin
  if LayeredBitmap <> nil then
  begin
    if LayerVisible[Index] <> AValue then
      result := TSetLayerVisibleStateDifference.Create(self,LayeredBitmap.LayerUniqueId[index],AValue)
    else
      result := nil;
  end
  else
    result := nil;
end;

function TImageState.SetBlendOp(Index: integer; AValue: TBlendOperation
  ): TCustomImageDifference;
begin
  if LayeredBitmap <> nil then
  begin
    if BlendOperation[index] <> Avalue then
      result := TSetLayerBlendOpStateDifference.Create(self,LayeredBitmap.LayerUniqueId[index],AValue)
    else
      result := nil;
  end
  else
    result := nil;
end;

procedure TImageState.SetLinearBlend(AValue: boolean);
begin
  if LayeredBitmap <> nil then
    LayeredBitmap.LinearBlend := AValue;
end;

procedure TImageState.SetOnActionDone(AValue: TNotifyEvent);
begin
  if FOnActionDone=AValue then Exit;
  FOnActionDone:=AValue;
end;

procedure TImageState.SetOnActionProgress(AValue: TLayeredActionProgressEvent);
begin
  if FOnActionProgress=AValue then Exit;
  FOnActionProgress:=AValue;
end;

procedure TImageState.SetOnSizeChanged(AValue: TNotifyEvent);
begin
  if FOnSizeChanged=AValue then Exit;
  FOnSizeChanged:=AValue;
end;

procedure TImageState.SetSelectionMask(AValue: TBGRABitmap);
begin
  If AValue = FSelectionMask then exit;
  FSelectionMask := AValue;
  DiscardSelectionMaskBoundsCompletely;
end;

function TImageState.MakeLayerName: string;

  function LayerNameUsed(AName: string): Boolean;
  var
    j: Integer;
  begin
    if LayeredBitmap = nil then
      result := false else
    begin
      for j := 0 to NbLayers-1 do
        if trim(LayerName[j]) = trim(AName) then exit(true);
      result := false;
    end;
  end;

var i: integer;
begin
  for i := 1 to 1000 do
  begin
    result := rsLayer + inttostr(i);
    if not LayerNameUsed(result) and not LayerNameUsed('Layer' + inttostr(i)) then exit;
  end;
end;

procedure TImageState.CheckSizeChanged(APrevSize: TSize);
begin
  if GetSize <> APrevSize then NotifySizeChanged;
end;

function TImageState.GetSize: TSize;
begin
  if Assigned(FLayeredBitmap) then
    result := Size(FLayeredBitmap.Width, FLayeredBitmap.Height)
    else result := Size(0,0);
end;

procedure TImageState.SetLayerBitmap(layer: integer; ABitmap: TBGRABitmap;
  AOwned: boolean);
begin
  if LayeredBitmap <> nil then
    LayeredBitmap.SetLayerBitmap(layer,ABitmap,AOwned);
end;

function TImageState.GetOrCreateSelectionLayer: TBGRABitmap;
begin
    if SelectionMask = nil then
      raise Exception.Create(rsNoActiveSelection) else
    begin
      if SelectionLayer = nil then
      begin
        SelectionLayer := TBGRABitmap.Create(Width,Height);
        FLastSelectionLayerBounds := EmptyRect;
        FLastSelectionLayerBoundsIsDefined := bsDefined;
      end;
      result := SelectionLayer;
    end;
end;

procedure TImageState.ReplaceSelectionLayer(bmp: TBGRABitmap; AOwned: boolean);
begin
  if (SelectionMask <> nil) then
  begin
    if AOwned or (bmp= nil) then
    begin
      if (SelectionLayer <> nil) and (SelectionLayer <> bmp) then FreeAndNil(SelectionLayer);
      SelectionLayer := bmp;
    end
    else
    begin
      if SelectionLayer <> nil then FreeAndNil(SelectionLayer);
      SelectionLayer := bmp.Duplicate(True) as TBGRABitmap;
    end;
  end else
  begin
    if (bmp = nil) then FreeAndNil(SelectionLayer);
    if AOwned and (bmp <>nil) then bmp.Free; //ignore if there is no active selection
  end;
end;

procedure TImageState.ComputeTransformedSelectionLayer(out
  ANewLayer: TBGRABitmap; out ALeft, ATop: integer);
var
  r: TRect;
begin
  if SelectionLayer = nil then
  begin
    ANewLayer := nil;
    ALeft := 0;
    ATop := 0;
  end else
  begin
    r := SelectionLayer.GetImageAffineBounds(FSelectionTransform, GetSelectionLayerBounds);
    ANewLayer := TBGRABitmap.Create(r.Width,r.Height);
    ANewLayer.PutImageAffine(AffineMatrixTranslation(-r.Left,-r.Top)*FSelectionTransform, SelectionLayer, rfHalfCosine);
    ALeft := r.Left;
    ATop := r.Top;
  end;
end;

procedure TImageState.DiscardSelectionLayerBounds(const AChangeRect: TRect);
begin
  if FLastSelectionLayerBoundsIsDefined in[bsDefined,bsWithinCurrent] then
  begin
    FLastSelectionLayerBounds := RectUnion(FLastSelectionLayerBounds, AChangeRect);
    FLastSelectionLayerBoundsIsDefined := bsWithinCurrent;
  end;
end;

procedure TImageState.DiscardSelectionLayerBoundsCompletely;
begin
  FLastSelectionLayerBoundsIsDefined := bsUndefined;
end;

function TImageState.GetSelectionLayerBounds: TRect;
begin
  if FLastSelectionLayerBoundsIsDefined = bsDefined then
    result := FLastSelectionLayerBounds
  else
  if SelectionLayer = nil then
  begin
    result := EmptyRect;
    FLastSelectionLayerBounds := result;
    FLastSelectionLayerBoundsIsDefined := bsDefined;
  end else
  begin
    if FLastSelectionLayerBoundsIsDefined = bsWithinCurrent then
      result := SelectionLayer.GetImageBoundsWithin(FLastSelectionLayerBounds)
      else result := SelectionLayer.GetImageBounds;
    FLastSelectionLayerBounds := result;
    FLastSelectionLayerBoundsIsDefined := bsDefined;
  end;
end;

function TImageState.SelectionLayerEmpty: boolean;
begin
  result := IsRectEmpty(GetSelectionLayerBounds);
end;

procedure TImageState.SetSize(AWidth, AHeight: integer);
var
  prevSize: TSize;
begin
  if LayeredBitmap <> nil then
  begin
    prevSize := GetSize;
    LayeredBitmap.SetSize(AWidth,AHeight);
    CheckSizeChanged(prevSize);
  end;
end;

procedure TImageState.NotifySizeChanged;
begin
  if Assigned(OnSizeChanged) then OnSizeChanged(self);
end;

procedure TImageState.PrepareForRendering;
begin
  if LayeredBitmap <> nil then
    LayeredBitmap.FreezeExceptOneLayer(SelectedImageLayerIndex);
end;

function TImageState.ComputeFlatImageWithoutSelection(ASeparateXorMask: boolean): TBGRABitmap;
begin
  if LayeredBitmap <> nil then
    result := LayeredBitmap.ComputeFlatImage(ASeparateXorMask)
  else
    result := TBGRABitmap.Create(Width,Height);
end;

procedure TImageState.Assign(AValue: TBGRABitmap; AOwned: boolean);
var
  xorMask: TBGRABitmap;
  idx: Integer;
  prevSize: TSize;
begin
  prevSize := GetSize;

  if LayeredBitmap = nil then
    SetLayeredBitmap(TBGRALayeredBitmap.Create);

  LayeredBitmap.Clear;
  LayeredBitmap.SetSize(AValue.Width,AValue.Height);
  if AOwned then
  begin
    idx := LayeredBitmap.AddOwnedLayer(AValue);
    LayeredBitmap.LayerName[idx] := MakeLayerName;
    if Assigned(AValue.XorMask) then
    begin
      xorMask := TBGRABitmap.Create(AValue.XorMask);
      xorMask.AlphaFill(255);
      xorMask.ReplaceColor(BGRABlack,BGRAPixelTransparent);
      LayeredBitmap.LayerName[LayeredBitmap.AddOwnedLayer(xorMask,boXor)] := 'Xor';
      AValue.DiscardXorMask;
    end;
  end
  else
  begin
    idx := LayeredBitmap.AddLayer(AValue);
    LayeredBitmap.LayerName[idx] := MakeLayerName;
    if Assigned(AValue.XorMask) then
    begin
      xorMask := AValue.XorMask.Duplicate as TBGRABitmap;
      xorMask.AlphaFill(255);
      xorMask.ReplaceColor(BGRABlack,BGRAPixelTransparent);
      LayeredBitmap.LayerName[LayeredBitmap.AddOwnedLayer(xorMask,boXor)] := 'Xor';
    end;
  end;
  SelectedImageLayerIndex := 0;

  CheckSizeChanged(prevSize);
end;

procedure TImageState.Assign(AValue: TBGRACustomLayeredBitmap; AOwned: boolean);
var
  prevSize: TSize;
begin
  if AOwned and (AValue is TBGRALayeredBitmap) then
  begin
    prevSize := GetSize;
    FreeAndNil(FLayeredBitmap);
    SetLayeredBitmap(TBGRALayeredBitmap(AValue));
    CheckSizeChanged(prevSize);
  end else
  begin
    LayeredBitmap.Assign(AValue, true, true);
    if AOwned then AValue.Free;
  end;
  if NbLayers > 0 then
  begin
    SelectedImageLayerIndex := 0
  end
  else
    SelectedImageLayerIndex := -1;
end;

procedure TImageState.Assign(AValue: TImageState; AOwned: boolean);
var
  layered: TBGRALayeredBitmap;
begin
  layered := AValue.LayeredBitmap;
  Assign(layered, AOwned);
  if AOwned then AValue.SetLayeredBitmap(nil);
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
  FSelectionTransform := AffineMatrixIdentity;
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

function TImageState.AssignWithUndo(AValue: TBGRACustomLayeredBitmap;
  AOwned: boolean; ASelectedLayerIndex: integer): TCustomImageDifference;
begin
  if LayeredBitmap = nil then
    result := nil
  else
    result := TAssignStateDifference.Create(self, AValue, AOwned, ASelectedLayerIndex);
end;

function TImageState.AssignWithUndo(AValue: TBGRACustomLayeredBitmap;
  AOwned: boolean; ASelectedLayerIndex: integer; ACurrentSelection: TBGRABitmap;
  ASelectionLayer: TBGRABitmap): TCustomImageDifference;
begin
  if LayeredBitmap = nil then
    result := nil
  else
    result := TAssignStateDifference.Create(self, AValue, AOwned, ASelectedLayerIndex, ACurrentSelection, ASelectionLayer);
end;

function TImageState.AssignWithUndo(AState: TImageState; AOwned: boolean): TCustomImageDifference;
begin
  result := AssignWithUndo(AState.LayeredBitmap, AOwned, SelectedImageLayerIndex, AState.SelectionMask, AState.SelectionLayer);
  if AOwned then AState.Free;
end;

function TImageState.GetUndoAfterAssign(ABackup: TImageState): TCustomImageDifference;
begin
  if LayeredBitmap = nil then
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
  SaveLayersToStream(AStream, LayeredBitmap, SelectedImageLayerIndex, lzpRLE);
end;

procedure TImageState.SaveToStreamAs(AStream: TStream; AFormat: TBGRAImageFormat);
var
  i: Integer;
  curGuid: TGuid;
begin
  if LayeredBitmap <> nil then
  begin
    if AFormat = ifLazPaint then
    begin
      curGuid := LayeredBitmap.LayerOriginalGuid[GetCurrentLayerIndex];
      for i := 0 to LayeredBitmap.OriginalCount-1 do
        if LayeredBitmap.OriginalGuid[i] <> curGuid then
          LayeredBitmap.UnloadOriginal(i);
    end;
    LayeredBitmap.SaveToStreamAs(AStream, SuggestImageExtension(AFormat));
  end;
end;

procedure TImageState.SaveOriginalToStream(AStream: TStream);
begin
  LayeredBitmap.SaveOriginalToStream(
    LayeredBitmap.LayerOriginalGuid[SelectedImageLayerIndex],
    AStream);
end;

procedure TImageState.SaveToFile(AFilenameUTF8: string);
var
  s: TStream;
begin
  if LayeredBitmap <> nil then
  begin
    s := FileManager.CreateFileStream(AFilenameUTF8, fmCreate);
    try
      LayeredBitmap.SaveToStream(s);
    finally
      s.Free;
    end;
  end;
end;

function TImageState.AddNewLayer(ALayer: TBGRABitmap; AName: string; AOffset: TPoint; ABlendOp: TBlendOperation; AOpacity: byte): TCustomImageDifference;
var
  idxLayer: Integer;
begin
  if AName = '' then AName := MakeLayerName;
  //no undo if no previous image
  if LayeredBitmap = nil then
  begin
    SetLayeredBitmap(TBGRALayeredBitmap.Create);
    idxLayer := LayeredBitmap.AddOwnedLayer(ALayer, ABlendOp, AOpacity);
    LayeredBitmap.LayerOffset[idxLayer] := AOffset;
    LayeredBitmap.LayerName[idxLayer] := AName;
    result := nil;
  end else
  begin
    result := TAddLayerStateDifference.Create(self, ALayer, AName, AOffset, ABlendOp, AOpacity);
    ALayer.Free;
  end;
end;

function TImageState.AddNewLayer(AOriginal: TBGRALayerCustomOriginal;
  AName: string; ABlendOp: TBlendOperation; AMatrix: TAffineMatrix; AOpacity: byte): TCustomImageDifference;
var
  idx: Integer;
begin
  if AName = '' then AName := MakeLayerName;
  //no undo if no previous image
  if LayeredBitmap = nil then
  begin
    SetLayeredBitmap(TBGRALayeredBitmap.Create);
    idx := LayeredBitmap.AddLayerFromOwnedOriginal(AOriginal, ABlendOp, AOpacity);
    LayeredBitmap.LayerOriginalMatrix[idx] := AMatrix;
    LayeredBitmap.LayerName[idx] := AName;
    LayeredBitmap.RenderLayerFromOriginal(idx);
    result := nil;
  end else
    result := TAddLayerFromOwnedOriginalStateDifference.Create(self, AOriginal, AName, ABlendOp, AMatrix, AOpacity);
end;

function TImageState.DuplicateLayer: TCustomImageDifference;
begin
  if LayeredBitmap = nil then
    result := nil
  else
    result := TDuplicateLayerStateDifference.Create(self, true);
end;

function TImageState.MergerLayerOver(ALayerOverIndex: integer): TCustomImageDifference;
begin
  if (LayeredBitmap = nil) or (ALayerOverIndex <= 0) or (ALayerOverIndex >= LayeredBitmap.NbLayers) then
    result := nil
  else
    result := TMergeLayerOverStateDifference.Create(self, ALayerOverIndex);
end;

function TImageState.MoveLayer(AFromIndex, AToIndex: integer): TCustomImageDifference;
begin
  if LayeredBitmap = nil then
    result := nil
  else
    result := TMoveLayerStateDifference.Create(self, AFromIndex, AToIndex);
end;

function TImageState.RemoveLayer: TCustomImageDifference;
begin
  if LayeredBitmap = nil then
    result := nil
  else
  begin
    result := TRemoveLayerStateDifference.Create(self, True);
  end;
end;

function TImageState.DiscardOriginal(ACreateUndo: boolean): TCustomImageDifference;
begin
  if ACreateUndo then
    result := TDiscardOriginalDifference.Create(self, SelectedImageLayerIndex, true)
  else
  begin
    LayeredBitmap.LayerOriginalGuid[SelectedImageLayerIndex] := GUID_NULL;
    LayeredBitmap.LayerOriginalMatrix[SelectedImageLayerIndex] := AffineMatrixIdentity;
    LayeredBitmap.RemoveUnusedOriginals;
  end;
end;

function TImageState.HorizontalFlip: TCustomImageDifference;
begin
  if LayeredBitmap = nil then
    result := nil
  else
    result := TInversibleStateDifference.Create(self, iaHorizontalFlip);
end;

function TImageState.HorizontalFlip(ALayerIndex: integer): TCustomImageDifference;
begin
  if LayeredBitmap = nil then
    result := nil
  else
    result := TInversibleStateDifference.Create(self, iaHorizontalFlipLayer, ALayerIndex);
end;

function TImageState.VerticalFlip: TCustomImageDifference;
begin
  if LayeredBitmap = nil then
    result := nil
  else
  result := TInversibleStateDifference.Create(self, iaVerticalFlip);
end;

function TImageState.VerticalFlip(ALayerIndex: integer): TCustomImageDifference;
begin
  if LayeredBitmap = nil then
    result := nil
  else
    result := TInversibleStateDifference.Create(self, iaVerticalFlipLayer, ALayerIndex);
end;

procedure TImageState.QuerySelectionMask;
begin
  if SelectionMask = nil then
  begin
    SelectionMask := TBGRABitmap.Create(Width,Height, BGRABlack);
    FLastSelectionMaskBoundsIsDefined := bsDefined;
    FLastSelectionMaskBounds := EmptyRect;
  end;
end;

procedure TImageState.DiscardSelectionMaskBounds(const AChangeRect: TRect);
begin
  if FLastSelectionMaskBoundsIsDefined in[bsDefined,bsWithinCurrent] then
  begin
    FLastSelectionMaskBounds := RectUnion(FLastSelectionMaskBounds, AChangeRect);
    FLastSelectionMaskBoundsIsDefined := bsWithinCurrent;
  end;
end;

procedure TImageState.DiscardSelectionMaskBoundsCompletely;
begin
  FLastSelectionMaskBoundsIsDefined := bsUndefined;
end;

function TImageState.GetSelectionMaskBounds: TRect;
begin
  if FLastSelectionMaskBoundsIsDefined = bsDefined then
    result := FLastSelectionMaskBounds
  else
  if SelectionMask = nil then
  begin
    result := EmptyRect;
    FLastSelectionMaskBounds := result;
    FLastSelectionMaskBoundsIsDefined := bsDefined;
  end else
  begin
    if FLastSelectionMaskBoundsIsDefined = bsWithinCurrent then
      result := SelectionMask.GetImageBoundsWithin(FLastSelectionMaskBounds, cGreen)
      else result := SelectionMask.GetImageBounds(cGreen);
    FLastSelectionMaskBounds := result;
    FLastSelectionMaskBoundsIsDefined := bsDefined;
  end;
end;

function TImageState.SelectionMaskEmpty: boolean;
begin
  result := IsRectEmpty(GetSelectionMaskBounds);
end;

function TImageState.SelectionMaskEmptyComputed: boolean;
begin
  result := FLastSelectionMaskBoundsIsDefined = bsDefined;
end;

function TImageState.GetTransformedSelectionMaskBounds: TRect;
begin
  if SelectionMaskEmpty then
    result := EmptyRect
  else
  begin
    result := SelectionMask.GetImageAffineBounds(SelectionTransform, GetSelectionMaskBounds);
  end;
end;

procedure TImageState.ComputeTransformedSelectionMask(out ANewMask: TBGRABitmap; out ALeft,ATop: integer);
var
  r: TRect;
begin
  if SelectionMask = nil then
  begin
    ANewMask := nil;
    ALeft := 0;
    ATop := 0;
  end else
  begin
    r := SelectionMask.GetImageAffineBounds(FSelectionTransform, GetSelectionMaskBounds);
    ANewMask := TBGRABitmap.Create(r.Width,r.Height,BGRABlack);
    ANewMask.PutImageAffine(AffineMatrixTranslation(-r.Left,-r.Top)*FSelectionTransform, SelectionMask, rfHalfCosine, dmLinearBlend);
    ALeft := r.Left;
    ATop := r.Top;
  end;
end;

function TImageState.SwapRedBlue: TCustomImageDifference;
var
  newImg: TBGRALayeredBitmap;
  newLayer: TBGRABitmap;
  idxLayer, i: Integer;
begin
  if LayeredBitmap = nil then
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
  if LayeredBitmap = nil then
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

function TImageState.ClearLayer: TCustomImageDifference;
begin
  if (LayeredBitmap = nil) or SelectedImageLayer.Empty then
    result := nil
  else
  begin
    result := TReplaceLayerByCustomOriginalDifference.Create(self,
                   SelectedImageLayerIndex, true,
                   TVectorOriginal.Create);
  end;
end;

function TImageState.RotateCW: TCustomImageDifference;
begin
  if LayeredBitmap = nil then
    result := nil
  else
  result := TInversibleStateDifference.Create(self, iaRotateCW);
end;

function TImageState.RotateCCW: TCustomImageDifference;
begin
  if LayeredBitmap = nil then
    result := nil
  else
  result := TInversibleStateDifference.Create(self, iaRotateCCW);
end;

function TImageState.Rotate180: TCustomImageDifference;
begin
  if LayeredBitmap = nil then
    result := nil
  else
  result := TInversibleStateDifference.Create(self, iaRotate180);
end;

function TImageState.ComputeLayerOffsetDifference(AOffsetX, AOffsetY: integer): TCustomImageDifference;
begin
  result := TApplyLayerOffsetStateDifference.Create(self, LayeredBitmap.LayerUniqueId[SelectedImageLayerIndex], AOffsetX,AOffsetY, false);
end;

function TImageState.ComputeSelectionTransformDifference: TCustomImageDifference;
begin
  result := TSelectionTransformDifference.Create(self, false);
end;

function TImageState.ComputeLayerMatrixDifference(AIndex: integer; APrevMatrix,
  ANewMatrix: TAffineMatrix): TCustomImageDifference;
begin
  result := TSetLayerMatrixDifference.Create(self, LayeredBitmap.LayerUniqueId[AIndex], APrevMatrix,ANewMatrix);
end;

function TImageState.ComputeLayerDifference(APreviousImage: TBGRABitmap;
  APreviousImageDefined: boolean; APreviousSelection: TBGRABitmap;
  APreviousSelectionDefined: boolean; APreviousSelectionLayer: TBGRABitmap;
  APreviousSelectionLayerDefined: boolean): TCustomImageDifference;
begin
  if LayeredBitmap = nil then
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
  if LayeredBitmap = nil then
    result := nil
  else
    result := TImageLayerStateDifference.Create(self, APreviousImage, APreviousImageChangeRect,
      APreviousSelection, APreviousSelectionChangeRect, APreviousSelectionLayer, APreviousSelectionLayerChangeRect);
end;

function TImageState.GetLayeredBitmapCopy: TBGRALayeredBitmap;
begin
  result := LayeredBitmap.Duplicate;
end;

function TImageState.ComputeFlatImage(AFromLayer, AToLayer: integer; ASeparateXorMask: boolean): TBGRABitmap;
begin
  result := LayeredBitmap.ComputeFlatImage(AFromLayer,AToLayer,ASeparateXorMask);
end;

function TImageState.ComputeFlatImage(ARect: TRect; AFromLayer,
  AToLayer: integer; ASeparateXorMask: boolean): TBGRABitmap;
begin
  result := LayeredBitmap.ComputeFlatImage(ARect,AFromLayer,AToLayer,ASeparateXorMask);
end;

procedure TImageState.DrawLayers(ADest: TBGRABitmap; X, Y: Integer; AIconCursor: boolean; ADestinationEmpty: boolean);
begin
  if LayeredBitmap <> nil then
    LayeredBitmap.Draw(ADest,X,Y, AIconCursor, ADestinationEmpty);
end;

constructor TImageState.Create;
begin
  FLayeredBitmap := nil;
  SelectionMask := nil;
  SelectionLayer := nil;
  selectedLayerId := -1;
  FLastSelectionMaskBoundsIsDefined := bsUndefined;
  FLastSelectionLayerBoundsIsDefined := bsUndefined;
  FSelectionTransform := AffineMatrixIdentity;
  DiscardOriginalDiff := true;
end;

destructor TImageState.Destroy;
begin
  LayeredBitmap.Free;
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
      selectedLayerIndex := LayeredBitmap.GetLayerIndexFromId(selectedLayerId);
      otherSelectedLayerIndex := other.LayeredBitmap.GetLayerIndexFromId(selectedLayerId);
      if (selectedLayerIndex <> -1) and (otherSelectedLayerIndex <> -1) then
        if not other.LayeredBitmap.LayerBitmap[otherSelectedLayerIndex].Equals(LayeredBitmap.LayerBitmap[selectedLayerIndex]) then exit;
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
  copy.SetLayeredBitmap(DuplicateLayeredBitmap(LayeredBitmap));
  copy.SelectionMask := DuplicateBitmap(SelectionMask);
  copy.SelectionLayer := DuplicateBitmap(SelectionLayer);
  copy.selectedLayerId := selectedLayerId;
  result := copy;
end;

procedure TImageState.Resample(AWidth, AHeight: integer;
  AQuality: TResampleMode; AFilter: TResampleFilter);
begin
  LayeredBitmap.Resample(AWidth,AHeight,AQuality,AFilter);
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

