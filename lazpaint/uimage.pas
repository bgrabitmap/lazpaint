unit UImage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes, types,
  UImageState, UStateType, Graphics, BGRALayers, UImageObservation, FPWriteBMP,
  UImageType, BGRALzpCommon, UZoom;

const
  MaxLayersToAdd = 99;
  MaxImageWidth = 8192;
  MaxImageHeight = 8192;
  MaxLayerNameLength = 255;
  MaxUndoCount = 200;
  MaxUsedMemoryWithoutCompression = 512*1024*1024;

type
  TLayeredBitmapAndSelection = record
        layeredBitmap: TBGRALayeredBitmap;
        selection: TBGRABitmap;
        selectionLayer: TBGRABitmap;
      end;

  TLazPaintImage = class;
  TOnCurrentSelectionChanged = procedure(ASender: TLazPaintImage; AOffsetOnly: boolean) of object;
  TOnSelectedLayerIndexChanged = procedure(ASender: TLazPaintImage) of object;
  TOnStackChanged = procedure(ASender: TLazPaintImage; AScrollIntoView: boolean) of object;
  TImageExceptionHandler = procedure(AFunctionName: string; AException: Exception) of object;
  TOnCurrentFilenameChanged = procedure(ASender: TLazPaintImage) of object;

  TOnQueryExitToolHandler = procedure(sender: TLazPaintImage) of object;

  { TLazPaintImage }

  TLazPaintImage = class
  private

    FLastSelectionBoundsIsDefined,
    FLastSelectionLayerBoundsIsDefined: boolean;
    FLastSelectionBounds, FLastSelectionLayerBounds: TRect;
    FOnSelectionChanged: TOnCurrentSelectionChanged;
    FOnSelectedLayerIndexChanged: TOnSelectedLayerIndexChanged;
    FOnStackChanged: TOnStackChanged;
    FOnQueryExitToolHandler: TOnQueryExitToolHandler;
    FCurrentState: TImageState;
    FRenderedImage: TBGRABitmap;
    FRenderedImageInvalidated: TRect;
    FOnImageChanged: TLazPaintImageObservable;
    FUndoList: TList;
    FUndoPos: integer;
    FRenderUpdateRectInPicCoord, FRenderUpdateRectInVSCoord: TRect;
    FOnCurrentFilenameChanged: TOnCurrentFilenameChanged;

    FSelectionOffset: TPoint;
    FSelectionRotateAngle: Single;
    FSelectionRotateCenter: TPointF;
    FDisplayedSelectionBounds: TRect;
    FSelectionLayerAfterMask: TBGRABitmap;
    FSelectionLayerAfterMaskOffset: TPoint;
    FSelectionLayerAfterMaskDefined: boolean;

    procedure DiscardSelectionLayerAfterMask;
    function GetLayerBitmapById(AId: integer): TBGRABitmap;
    function GetLayerId(AIndex: integer): integer;
    procedure NeedSelectionLayerAfterMask;
    function GetBlendOperation(AIndex: integer): TBlendOperation;
    function GetCurrentFilenameUTF8: string;
    function GetCurrentLayerVisible: boolean;
    function GetCurrentImageLayerIndex:integer;
    function GetEmpty: boolean;
    function GetHeight: integer;
    function GetCurrentSelection: TBGRABitmap;
    function GetLayerBitmap(AIndex: integer): TBGRABitmap;
    function GetLayerName(AIndex: integer): string;
    function GetLayerOffset(AIndex: integer): TPoint;
    function GetLayerOpacity(AIndex: integer): byte;
    function GetLayerVisible(AIndex: integer): boolean;
    function GetNbLayers: integer;
    function GetRenderedImage: TBGRABitmap;
    function GetSelectedLayerPixel(X, Y: Integer): TBGRAPixel;
    function GetSelectionBounds(AOffseted: boolean): TRect;
    function GetSelectionLayerBounds(AOffseted: boolean): TRect;
    function GetWidth: integer;
    function GetZoomFactor: single;
    procedure InvalidateImageDifference(ADiff: TCustomImageDifference);
    procedure MergeWithSelection(AApplyMask: boolean = true);
    procedure SetBlendOperation(AIndex: integer; AValue: TBlendOperation);
    procedure SetCurrentFilenameUTF8(AValue: string);
    procedure SelectImageLayer(AValue: TBGRABitmap);
    procedure LayeredBitmapReplaced;
    procedure SetLayerName(AIndex: integer; AValue: string);
    procedure SetLayerOffset(AIndex: integer; AValue: TPoint);
    procedure SetLayerOpacity(AIndex: integer; AValue: byte);
    procedure SetLayerVisible(AIndex: integer; AValue: boolean);
    procedure LayerBlendMayChange(AIndex: integer);
    function GetDrawingLayer: TBGRABitmap;
    procedure SelectionOffsetChanged;
    procedure CompressUndoIfNecessary;
    procedure NotifyException(AFunctionName: string; AException: Exception);
    procedure SetSelectionOffset(AValue: TPoint);
    procedure ClearUndoAfter;

  public
    ActionInProgress: TCustomLayerAction;
    OnException: TImageExceptionHandler;
    ImageOffset: TPoint;
    Zoom: TZoom;

    procedure DiscardSelectionBounds;
    procedure DiscardSelectionLayerBounds;
    function MakeLayeredBitmapCopy: TBGRALayeredBitmap;
    function MakeLayeredBitmapAndSelectionCopy: TLayeredBitmapAndSelection;
    function MakeBitmapCopy(backgroundColor: TColor): TBitmap;
    function MakeCroppedLayer: TBGRABitmap;
    function CanUndo: boolean;
    function CanRedo: boolean;
    procedure Undo;
    procedure Redo;
    procedure ClearUndo;
    procedure CompressUndo;
    function UsedMemory: int64;
    procedure ImageMayChange(ARect: TRect; ADiscardSelectionLayerAfterMask: boolean = true);
    procedure ImageMayChangeCompletely;
    procedure LayerMayChange(ALayer: TBGRABitmap; ARect: TRect);
    procedure LayerMayChangeCompletely(ALayer: TBGRABitmap);
    procedure SelectionMayChange(ARect: TRect);
    procedure SelectionMayChangeCompletely;
    procedure RenderMayChange(ARect: TRect; APicCoords: boolean = false);
    procedure ResetRenderUpdateRect;
    function SetCurrentImageLayerIndex(AValue: integer): boolean;
    procedure SetLayerOffset(AIndex: integer; AValue: TPoint; APrecomputedLayerBounds: TRect);

    function SelectionEmpty: boolean;
    function SelectionEmptyComputed: boolean;
    function SelectionNil: boolean;
    function SelectionLayerIsEmpty: boolean;
    procedure ReleaseEmptySelection;
    function SelectedLayerEmpty: boolean;
    function SelectedLayerEquals(AColor: TBGRAPixel): boolean;
    property SelectedLayerPixel[X,Y: Integer]: TBGRAPixel read GetSelectedLayerPixel;
    function GetSelectionCenter: TPointF;
    procedure SaveSelectionToFileUTF8(AFilename: string);
    procedure SetSelectionRotateCenter(ACenter: TPointF);
    procedure SetSelectionRotateAngle(AValue: single);
    function GetSelectionRotateAngle: Single;
    function GetSelectionRotateCenter: TPointF;
    function GetSelectionOffset: TPoint;
    function SelectionReadonly: TBGRABitmap;
    function SelectionLayerReadonly: TBGRABitmap;
    function SelectedImageLayerReadOnly: TBGRABitmap;

    function GetSelectedImageLayer: TBGRABitmap;
    function GetSelectionLayerIfExists: TBGRABitmap;
    procedure ReplaceSelectionLayer(bmp: TBGRABitmap; AOwned: boolean);
    procedure ReplaceCurrentSelection(const AValue: TBGRABitmap);
    procedure ReplaceDrawingLayer(bmp: TBGRABitmap; AOwned: boolean);
    procedure QuerySelection;
    procedure RemoveSelection;
    procedure EraseSelectionInBitmap;
    procedure ReleaseSelection;
    procedure RetrieveSelection;
    function RetrieveSelectionIfLayerEmpty(removeFromBitmap: boolean = false): boolean;
    function GetOrCreateSelectionLayer: TBGRABitmap;
    procedure ApplySelectionTransform(ApplyToMask: boolean= true);
    procedure ApplySelectionMask;
    procedure ReplaceSelectedLayer(AValue: TBGRABitmap; AOwned: boolean);
    procedure AddUndo(AUndoAction: TCustomImageDifference);
    procedure AddLayerUndo(APreviousImage: TBGRABitmap; APreviousImageDefined: boolean;
        APreviousSelection: TBGRABitmap; APreviousSelectionDefined: boolean;
        APreviousSelectionLayer: TBGRABitmap; APreviousSelectionLayerDefined: boolean); overload;
    procedure AddLayerUndo(APreviousImage: TBGRABitmap; APreviousImageChangeRect:TRect;
        APreviousSelection: TBGRABitmap; APreviousSelectionChangeRect:TRect;
        APreviousSelectionLayer: TBGRABitmap; APreviousSelectionLayerChangeRect:TRect); overload;

    function ComputeRotatedSelection: TBGRABitmap;
    function ApplySmartZoom3: boolean;
    procedure Resample(AWidth, AHeight: integer; filter: TResampleFilter);
    procedure LoadFromFileUTF8(AFilename: string);
    procedure LoadFromStreamAsLZP(AStream: TStream);
    procedure Assign(const AValue: TBGRABitmap; AOwned: boolean; AUndoable: boolean); overload;
    procedure Assign(const AValue: TBGRALayeredBitmap; AOwned: boolean; AUndoable: boolean); overload;
    procedure Assign(const AValue: TLayeredBitmapAndSelection; AOwned: boolean; AUndoable: boolean); overload;
    procedure ApplyLayerOffset(AOffsetX, AOffsetY: integer);

    function AbleToSaveAsUTF8(AFilename: string): boolean;
    function AbleToSaveSelectionAsUTF8(AFilename: string): boolean;
    procedure SaveToFileUTF8(AFilename: string);
    procedure SaveToStreamAsLZP(AStream: TStream);
    procedure SetSavedFlag;
    function IsFileModified: boolean;
    function IsFileModifiedAndSaved: boolean;
    function FlatImageEquals(ABitmap: TBGRABitmap): boolean;
    procedure Flatten;
    procedure PrepareForRendering;
    function ComputeFlatImage(AFromLayer,AToLayer: integer): TBGRABitmap;

    procedure Draw(ADest: TBGRABitmap; x,y: integer);
    procedure AddNewLayer;
    procedure AddNewLayer(ALayer: TBGRABitmap; AName: string);
    procedure DuplicateLayer;
    procedure MergeLayerOver;
    procedure MoveLayer(AFromIndex,AToIndex: integer);
    procedure RemoveLayer;
    procedure SwapRedBlue;
    procedure LinearNegativeAll;
    procedure HorizontalFlip;
    procedure VerticalFlip;
    procedure RotateCW;
    procedure RotateCCW;
    function CheckCurrentLayerVisible: boolean;
    function CheckNoAction(ASilent: boolean = false): boolean;
    procedure ZoomFit;

    property currentFilenameUTF8: string read GetCurrentFilenameUTF8 write SetCurrentFilenameUTF8;
    property currentImageLayerIndex: integer read GetCurrentImageLayerIndex;
    property CurrentSelection: TBGRABitmap read GetCurrentSelection;
    property RenderedImage: TBGRABitmap read GetRenderedImage;
    property Width: integer read GetWidth;
    property Height: integer read GetHeight;
    property OnSelectionChanged: TOnCurrentSelectionChanged read FOnSelectionChanged write FOnSelectionChanged;
    property OnSelectedLayerIndexChanged: TOnSelectedLayerIndexChanged read FOnSelectedLayerIndexChanged write FOnSelectedLayerIndexChanged;
    property OnStackChanged: TOnStackChanged read FOnStackChanged write FOnStackChanged;
    property OnImageChanged: TLazPaintImageObservable read FOnImageChanged;
    property NbLayers: integer read GetNbLayers;
    property Empty: boolean read GetEmpty;
    property SelectionLayerBounds[AOffseted: boolean]: TRect read GetSelectionLayerBounds;
    property SelectionBounds[AOffseted: boolean]: TRect read GetSelectionBounds;
    property LayerName[AIndex: integer]: string read GetLayerName write SetLayerName;
    property LayerBitmap[AIndex: integer]: TBGRABitmap read GetLayerBitmap;
    property LayerBitmapById[AIndex: integer]: TBGRABitmap read GetLayerBitmapById;
    property LayerId[AIndex: integer]: integer read GetLayerId;
    property LayerVisible[AIndex: integer]: boolean read GetLayerVisible write SetLayerVisible;
    property LayerOpacity[AIndex: integer]: byte read GetLayerOpacity write SetLayerOpacity;
    property LayerOffset[AIndex: integer]: TPoint read GetLayerOffset write SetLayerOffset;
    property BlendOperation[AIndex: integer]: TBlendOperation read GetBlendOperation write SetBlendOperation;
    property CurrentLayerVisible: boolean read GetCurrentLayerVisible;
    property OnQueryExitToolHandler: TOnQueryExitToolHandler read FOnQueryExitToolHandler write FOnQueryExitToolHandler;
    property OnCurrentFilenameChanged: TOnCurrentFilenameChanged read FOnCurrentFilenameChanged write FOnCurrentFilenameChanged;
    property RenderUpdateRectInPicCoord: TRect read FRenderUpdateRectInPicCoord;
    property RenderUpdateRectInVSCoord: TRect read FRenderUpdateRectInVSCoord;
    property SelectionOffset: TPoint read FSelectionOffset write SetSelectionOffset;
    property ZoomFactor: single read GetZoomFactor;
    constructor Create;
    destructor Destroy; override;
  end;

function ComputeAcceptableImageSize(AWidth,AHeight: integer): TSize;

implementation

uses UGraph, UResourceStrings, Dialogs,
    BGRAOpenRaster, BGRAPaintNet, UImageDiff, ULoading,
    BGRAWriteLzp, lazutf8classes, LCLProc,
    BGRAPalette, BGRAColorQuantization;

function ComputeAcceptableImageSize(AWidth, AHeight: integer): TSize;
var ratio,newRatio: single;
begin
  ratio := 1;
  if AWidth > MaxImageWidth then ratio := MaxImageWidth/AWidth;
  if AHeight > MaxImageHeight then
  begin
    newRatio := MaxImageHeight/AHeight;
    if newRatio < ratio then ratio := newRatio;
  end;
  if ratio < 1 then
  begin
    result.cx := round(AWidth*ratio);
    result.cy := round(AHeight*ratio);
  end else
  begin
    result.cx := AWidth;
    result.cy := AHeight;
  end;
end;

{ TLazPaintImage }

function TLazPaintImage.GetOrCreateSelectionLayer: TBGRABitmap;
begin
    if currentSelection = nil then
      raise Exception.Create(rsNoActiveSelection) else
    begin
      if FCurrentState.selectionLayer = nil then
      begin
        FCurrentState.selectionLayer := TBGRABitmap.Create(Width,Height);
        FLastSelectionLayerBounds := EmptyRect;
        FLastSelectionLayerBoundsIsDefined := true;
      end;
      result := FCurrentState.selectionLayer;
    end;
end;

function TLazPaintImage.GetSelectionLayerIfExists: TBGRABitmap;
begin
  result := FCurrentState.selectionLayer;
end;

procedure TLazPaintImage.ReplaceSelectionLayer(bmp: TBGRABitmap; AOwned: boolean);
begin
  if (FCurrentState.currentSelection <> nil) then
  begin
    if AOwned or (bmp= nil) then
    begin
      if (FCurrentState.selectionLayer <> nil) and (FCurrentState.selectionLayer <> bmp) then FreeAndNil(FCurrentState.selectionLayer);
      FCurrentState.selectionLayer := bmp;
    end
    else
    begin
      if FCurrentState.selectionLayer <> nil then FreeAndNil(FCurrentState.selectionLayer);
      FCurrentState.selectionLayer := bmp.Duplicate(True) as TBGRABitmap;
    end;
    ImageMayChangeCompletely;
  end else
    if AOwned and (bmp <>nil) then bmp.Free; //ignore if there is no active selection
end;

function TLazPaintImage.ComputeRotatedSelection: TBGRABitmap;
begin
    if currentSelection = nil then result := nil else
    begin
      currentSelection.GrayscaleToAlpha;
      result := currentSelection.FilterRotate(FSelectionRotateCenter,FSelectionRotateAngle,False) as TBGRABitmap;
      currentSelection.AlphaToGrayscale;
      result.AlphaToGrayscale;
      result.AlphaFill(255);
    end;
end;

procedure TLazPaintImage.ApplySelectionTransform(ApplyToMask: boolean= true);
var temp: TBGRABitmap;
begin
  if FSelectionRotateAngle <> 0 then
  begin
    if ApplyToMask and (currentSelection <> nil) then
      ReplaceCurrentSelection(ComputeRotatedSelection);

    if GetSelectionLayerIfExists <> nil then
    begin
      temp := GetSelectionLayerIfExists.FilterRotate(FSelectionRotateCenter,FSelectionRotateAngle,True) as TBGRABitmap;
      ReplaceSelectionLayer(temp, True);
    end;

    FSelectionRotateAngle := 0;
  end;

  if (SelectionOffset.X <> 0) or (SelectionOffset.Y <> 0) then
  begin
    if currentSelection <> nil then
    begin
      temp := TBGRABitmap.Create(Width,Height, BGRABlack);
      temp.PutImage(SelectionOffset.X, SelectionOffset.Y, currentSelection, dmFastBlend);
      ReplaceCurrentSelection(temp);
    end;
    if GetSelectionLayerIfExists <> nil then
    begin
      temp := TBGRABitmap.Create(Width,Height);
      temp.PutImage(SelectionOffset.X, SelectionOffset.Y, GetSelectionLayerIfExists, dmSet);
      ReplaceSelectionLayer(temp,true);
    end;
    FSelectionOffset := point(0,0);
  end;
end;

procedure TLazPaintImage.ApplySelectionMask;
var r: TRect;
begin
  if (currentSelection <> nil) and (GetSelectionLayerIfExists <> nil) then
  begin
    r := SelectionLayerBounds[false];
    if not IsRectEmpty(r) then
    begin
      GetOrCreateSelectionLayer.ApplyMask(currentSelection,r);
      LayerMayChange(GetOrCreateSelectionLayer,r);
    end;
  end;
end;

function TLazPaintImage.MakeCroppedLayer: TBGRABitmap;
var r: TRect;
begin
  result := DuplicateBitmap(GetSelectionLayerIfExists);
  if (result <> nil) and (currentSelection <> nil) then result.ApplyMask(CurrentSelection);
  if (result <> nil) and result.Empty then FreeAndNil(result);
  if result = nil then
  begin
    result := DuplicateBitmap(GetSelectedImageLayer);
    if (result <> nil) and (CurrentSelection <> nil) then result.ApplyMask(CurrentSelection);
  end;
  if result <> nil then
  begin
    if currentselection = nil then
      r := result.GetImageBounds
    else
      r := SelectionBounds[False];
    if IsRectEmpty(r) then
      FreeAndNil(result)
    else
      if (r.left <> 0) or (r.top <> 0) or (r.right <> result.Width) or (r.bottom <> result.Height) then
        BGRAReplace(result, result.GetPart(r));
  end;
end;

function TLazPaintImage.ApplySmartZoom3: boolean;
var i: integer;
  zoomed: TLayeredBitmapAndSelection;
begin
  result := false;
  if not CheckNoAction then exit;
  try
    zoomed.layeredBitmap := TBGRALayeredBitmap.Create(Width,Height);
    for i := 0 to NbLayers-1 do
      zoomed.layeredBitmap.AddOwnedLayer(FCurrentState.LayerBitmap[i].FilterSmartZoom3(moMediumSmooth) as TBGRABitmap,
        FCurrentState.BlendOperation[i], FCurrentState.LayerOpacity[i]);
    if currentSelection <> nil then
      zoomed.selection:= currentSelection.FilterSmartZoom3(moMediumSmooth) as TBGRABitmap
    else zoomed.Selection := nil;
    if GetSelectionLayerIfExists <> nil then
      zoomed.selectionLayer := GetSelectionLayerIfExists.FilterSmartZoom3(moMediumSmooth) as TBGRABitmap
    else
      zoomed.selectionLayer := nil;
    AddUndo(FCurrentState.AssignWithUndo(zoomed.layeredBitmap,true, FCurrentState.currentLayerIndex, zoomed.selection, zoomed.selectionLayer));
    result := true;
  except on ex: exception do NotifyException('ApplySmartZoom3',ex);
  end;
  ImageMayChangeCompletely;
  SelectionMayChangeCompletely;
end;

procedure TLazPaintImage.Resample(AWidth, AHeight: integer; filter: TResampleFilter);
var quality : TResampleMode;
    backup: TImageState;
begin
  if not CheckNoAction then exit;
  try
    backup := FCurrentState.Duplicate as TImageState;

    if filter = rfBox then
      quality := rmSimpleStretch
    else
      quality := rmFineResample;

    FCurrentState.Resample(AWidth,AHeight,quality,filter);
    LayeredBitmapReplaced;
    AddUndo(FCurrentState.GetUndoAfterAssign(backup));
    SelectionMayChangeCompletely;
    backup.Free;
  except on ex: exception do NotifyException('Resample',ex);
  end;
end;

function TLazPaintImage.AbleToSaveAsUTF8(AFilename: string): boolean;
var format: TBGRAImageFormat;
begin
  format := SuggestImageFormat(AFilename);
  result := DefaultBGRAImageWriter[format] <> nil;
  if result and (format = ifXPixMap) then
  begin
    if (Width > 256) or (Height > 256) then
    begin
      ShowMessage(rsNotReasonableFormat);
      result := false;
    end;
  end;

end;

function TLazPaintImage.AbleToSaveSelectionAsUTF8(AFilename: string): boolean;
var ext: string;
begin
  ext := UTF8LowerCase(ExtractFileExt(AFilename));
  if (ext='.bmp') or (ext='.jpg') or (ext='.jpeg')
    or (ext='.png') or (ext='.pcx') or (ext='.tga') or (ext='.lzp') then
    result := true else
      result := false;
end;

procedure TLazPaintImage.SaveToFileUTF8(AFilename: string);
var s: TFileStreamUTF8;
  format: TBGRAImageFormat;
begin
  format := SuggestImageFormat(AFilename);
  if format = ifLazPaint then
  begin
    s := TFileStreamUTF8.Create(AFilename, fmCreate);
    try
      SaveToStreamAsLZP(s);
    finally
      s.Free;
    end;
    SetSavedFlag;
  end else
  if format = ifOpenRaster then
  begin
    FCurrentState.SaveToFile(AFilename);
    SetSavedFlag;
  end else
  begin
    if RenderedImage = nil then exit;
    RenderedImage.SaveToFileUTF8(AFilename);
    if NbLayers = 1 then SetSavedFlag;
  end;
end;

procedure TLazPaintImage.LoadFromFileUTF8(AFilename: string);
var s: TFileStreamUTF8;
  ext: string;
  bmp: TBGRALayeredBitmap;
begin
  if not CheckNoAction then exit;
  ext := UTF8LowerCase(ExtractFileExt(AFilename));
  if ext = '.lzp' then
  begin
    s := TFileStreamUTF8.Create(AFilename, fmOpenRead or fmShareDenyWrite);
    try
      LoadFromStreamAsLZP(s);
    finally
      s.Free;
    end;
  end else
  begin
    bmp := TBGRALayeredBitmap.Create;
    try
      bmp.LoadFromFile(AFilename);
      with ComputeAcceptableImageSize(bmp.Width,bmp.Height) do
        if (cx < bmp.Width) or (cy < bmp.Height) then
          bmp.Resample(cx,cy,rmFineResample);
      Assign(bmp,true,false);
      bmp := nil;
    finally
      bmp.Free;
    end;
  end;
end;

procedure TLazPaintImage.SaveToStreamAsLZP(AStream: TStream);
var
  writer: TBGRAWriterLazPaint;
begin
  if (RenderedImage = nil) or (NbLayers = 0) or (Width = 0) or (Height = 0) then exit;

  if (NbLayers > 1) or (LayerOpacity[0] <> 255) or not LayerVisible[0] or (BlendOperation[0]<>boTransparent) then
  begin
    writer := TBGRAWriterLazPaintWithLayers.Create(FCurrentState);
    writer.IncludeThumbnail := true;// (Width*Height >= 262144);
    writer.Caption := 'Preview';
    RenderedImage.SaveToStream(AStream, writer);
    writer.free;
  end else
  begin
    writer := TBGRAWriterLazPaint.Create;
    writer.IncludeThumbnail := true;// (Width*Height >= 262144);
    writer.Caption := LayerName[0];
    RenderedImage.SaveToStream(AStream, writer);
    writer.free;
  end;
end;

procedure TLazPaintImage.LoadFromStreamAsLZP(AStream: TStream);
var
  {%H-}header: TLazPaintImageHeader;
  bmp: TBGRABitmap;
  reader: TBGRAReaderLazPaintWithLayers;
begin
  if not CheckNoAction then exit;
  RemoveSelection;

  AStream.ReadBuffer({%H-}header, sizeof(header));
  LazPaintImageHeader_SwapEndianIfNeeded(header);
  AStream.Position:= AStream.Position-sizeof(header);

  //use shortcut if possible
  if (header.magic = LAZPAINT_MAGIC_HEADER) and (header.zero1 = 0)
   and (header.layersOffset >= sizeof(header)) then
  begin
    AStream.Position:= AStream.Position+header.layersOffset;
    FCurrentState.LoadFromStream(AStream);
    ImageMayChangeCompletely;
  end else
  begin
    reader := TBGRAReaderLazPaintWithLayers.Create(FCurrentState);
    bmp := TBGRABitmap.Create;
    bmp.LoadFromStream(AStream, reader);
    if reader.LayersLoaded then
    begin
      bmp.Free;
      ImageMayChangeCompletely;
    end else
    begin
      with ComputeAcceptableImageSize(bmp.Width,bmp.Height) do
        if (cx < bmp.Width) or (cy < bmp.Height) then
          BGRAReplace(bmp,bmp.Resample(cx,cy,rmFineResample));
      Assign(bmp, true,false);
      LayerName[0] := reader.Caption;
    end;
    reader.Free;
  end;
  ClearUndo;
end;

procedure TLazPaintImage.SetSavedFlag;
var i: integer;
begin
  FCurrentState.saved := true;
  for i := 0 to FUndoList.Count-1 do
  begin
    TCustomImageDifference(FUndoList[i]).SavedBefore := (i = FUndoPos+1);
    TCustomImageDifference(FUndoList[i]).SavedAfter := (i = FUndoPos);
  end;
end;

function TLazPaintImage.IsFileModified: boolean;
begin
  result := not FCurrentState.saved;
end;

function TLazPaintImage.IsFileModifiedAndSaved: boolean;
var
  undolistcount: integer;
  saved:Boolean;
begin
  undolistcount:= FUndoList.Count;
  saved:= FCurrentState.saved;
  result := saved and (undolistcount>0);
end;

function TLazPaintImage.FlatImageEquals(ABitmap: TBGRABitmap): boolean;
begin
  if ABitmap = nil then result := RenderedImage = nil
  else
    result := ABitmap.Equals(RenderedImage);
end;

procedure TLazPaintImage.Flatten;
begin
  Assign(RenderedImage,False,True);
end;

function TLazPaintImage.GetDrawingLayer: TBGRABitmap;
begin
   if SelectionEmpty then result := GetSelectedImageLayer else
     result := GetOrCreateSelectionLayer;
end;

procedure TLazPaintImage.ReplaceDrawingLayer(bmp: TBGRABitmap; AOwned: boolean);
begin
   if SelectionEmpty then
     ReplaceSelectedLayer(bmp, AOwned)
   else
     ReplaceSelectionLayer(bmp, AOwned);
end;

procedure TLazPaintImage.LayeredBitmapReplaced;
begin
  FreeAndNil(FRenderedImage);
  FCurrentState.AdaptLayers;
  if FCurrentState.NbLayers = 0 then
    raise Exception.Create('No layer')
  else
    if FCurrentState.currentLayerIndex = -1 then
      FCurrentState.currentLayerIndex := 0;

  if Assigned(FOnStackChanged) then FOnStackChanged(self,True);
  OnImageChanged.NotifyObservers;
  ImageMayChangeCompletely;
end;

procedure TLazPaintImage.AddUndo(AUndoAction: TCustomImageDifference);
var
  prevAction: TCustomImageDifference;
begin
  if AUndoAction <> nil then
  begin
    if AUndoAction.IsIdentity then
    begin
      AUndoAction.Free;
      exit;
    end;
    if FUndoPos > -1 then
    begin
      prevAction := TObject(FUndoList[FUndoPos]) as TCustomImageDifference;
      if IsInverseImageDiff(AUndoAction,prevAction) then
      begin
        AUndoAction.Free;
        FCurrentState.saved := prevAction.SavedBefore;
        Dec(FUndoPos);
        ClearUndoAfter;
        exit;
      end else
      if not prevAction.savedAfter and TryCombineImageDiff(AUndoAction,prevAction) then
      begin
        AUndoAction.Free;
        If prevAction.IsIdentity then
        begin
          FCurrentState.saved := prevAction.SavedBefore;
          Dec(FUndoPos);
          ClearUndoAfter;
        end;
        exit;
      end;
    end;
    ClearUndoAfter;
    if FUndoList.Count >= MaxUndoCount then
    begin
      FUndoList.Delete(0);
      FUndoList.Add(AUndoAction);
    end else
    begin
      FUndoList.Add(AUndoAction);
      inc(FUndoPos);
    end;
    FCurrentState.saved := AUndoAction.SavedAfter;
    CompressUndoIfNecessary;
  end;
end;

procedure TLazPaintImage.AddLayerUndo(APreviousImage: TBGRABitmap;
  APreviousImageDefined: boolean; APreviousSelection: TBGRABitmap;
  APreviousSelectionDefined: boolean; APreviousSelectionLayer: TBGRABitmap;
  APreviousSelectionLayerDefined: boolean);
var diff: TCustomImageDifference;
begin
  diff := FCurrentState.ComputeLayerDifference(APreviousImage,APreviousImageDefined,
    APreviousSelection,APreviousSelectionDefined,
    APreviousSelectionLayer,APreviousSelectionLayerDefined);
  if diff.IsIdentity then
  begin
    diff.free;
    exit;
  end;
  AddUndo(diff);
end;

procedure TLazPaintImage.AddLayerUndo(APreviousImage: TBGRABitmap;
  APreviousImageChangeRect: TRect; APreviousSelection: TBGRABitmap;
  APreviousSelectionChangeRect: TRect; APreviousSelectionLayer: TBGRABitmap;
  APreviousSelectionLayerChangeRect: TRect);
var diff: TCustomImageDifference;
begin
  diff := FCurrentState.ComputeLayerDifference(APreviousImage,APreviousImageChangeRect,
    APreviousSelection,APreviousSelectionChangeRect,
    APreviousSelectionLayer,APreviousSelectionLayerChangeRect);
  if diff.IsIdentity then
  begin
    diff.free;
    exit;
  end;
  AddUndo(diff);
end;

procedure TLazPaintImage.CompressUndoIfNecessary;
var i: integer;
begin
  for i := 0 to FUndoList.Count-1 do
    if UsedMemory <= MaxUsedMemoryWithoutCompression then break else
    repeat
      if not (TObject(FUndoList[i]) as TStateDifference).TryCompress then break;
    until UsedMemory <= MaxUsedMemoryWithoutCompression;
end;

procedure TLazPaintImage.NotifyException(AFunctionName: string;
  AException: Exception);
begin
  if Assigned(OnException) then
    OnException(AFunctionName,AException)
  else
    MessageDlg(AFunctionName,AException.Message,mtError,[mbOk],0);
end;

procedure TLazPaintImage.SetSelectionOffset(AValue: TPoint);
begin
  if (FSelectionOffset.X <> AValue.X) or
    (FSelectionOffset.Y <> AValue.Y) then
  begin
    FSelectionOffset := AValue;
    SelectionOffsetChanged;
  end;
end;

procedure TLazPaintImage.ClearUndoAfter;
var I: integer;
begin
  for I := FUndoList.Count-1 downto FUndoPos+1 do
  begin
    TObject(FUndoList[i]).Free;
    FUndoList.Delete(i);
  end;
end;

procedure TLazPaintImage.DiscardSelectionBounds;
begin
  FLastSelectionBoundsIsDefined := false;
end;

procedure TLazPaintImage.DiscardSelectionLayerBounds;
begin
  FLastSelectionLayerBoundsIsDefined := false;
end;

procedure TLazPaintImage.SetLayerName(AIndex: integer; AValue: string);
begin
  AddUndo(FCurrentState.SetLayerName(AIndex,Avalue));
  OnImageChanged.NotifyObservers;
end;

procedure TLazPaintImage.SetLayerOffset(AIndex: integer; AValue: TPoint);
var bounds: TRect;
begin
  bounds := FCurrentState.LayerBitmap[AIndex].GetImageBounds;
  SetLayerOffset(AIndex,AValue,bounds);
end;

procedure TLazPaintImage.SetLayerOpacity(AIndex: integer; AValue: byte);
begin
  AddUndo(FCurrentState.SetLayerOpacity(AIndex,AValue));
  LayerBlendMayChange(AIndex);
end;

procedure TLazPaintImage.SetLayerVisible(AIndex: integer; AValue: boolean);
begin
  if not CheckNoAction then exit;
  if not SelectionLayerIsEmpty then
  begin
    MessagePopup(rsMustReleaseSelection,2000);
    exit;
  end;
  AddUndo(FCurrentState.SetLayerVisible(AIndex,AValue));
  LayerBlendMayChange(AIndex);
  OnImageChanged.NotifyObservers; //to show/hide tools
end;

function TLazPaintImage.MakeBitmapCopy(backgroundColor: TColor): TBitmap;
begin
  result := RenderedImage.MakeBitmapCopy(backgroundColor);
end;

function TLazPaintImage.CanUndo: boolean;
begin
  result := FUndoPos >= 0;
end;

function TLazPaintImage.CanRedo: boolean;
begin
  result := FUndoPos < (FUndoList.Count-1);
end;

procedure TLazPaintImage.Undo;
var diff: TCustomImageDifference;
begin
  if CanUndo then
  begin
    if not CheckNoAction then exit;
    try
      diff := TCustomImageDifference(FUndoList[FUndoPos]);
      diff.UnapplyTo(FCurrentState);
      Dec(FUndoPos);
      InvalidateImageDifference(diff);
    except
      on ex:Exception do
      begin
        NotifyException('Undo',ex);
        ClearUndo;
        ImageMayChangeCompletely;
        SelectionMayChangeCompletely;
      end;
    end;
    CompressUndoIfNecessary;
  end;
end;

procedure TLazPaintImage.InvalidateImageDifference(ADiff: TCustomImageDifference);
var kind:TImageDifferenceKind;
begin
  kind := ADiff.Kind;
  case kind of
  idkChangeStack: OnImageChanged.NotifyObservers;
  idkChangeImageAndSelection: begin
    if ADiff.ChangingBoundsDefined then
    begin
      ImageMayChange(ADiff.ChangingBounds);
      SelectionMayChange(ADiff.ChangingBounds);
    end else
    begin
      ImageMayChangeCompletely;
      SelectionMayChangeCompletely;
    end;
  end;
  idkChangeImage:
      if ADiff.ChangingBoundsDefined then
        ImageMayChange(ADiff.ChangingBounds)
      else
        ImageMayChangeCompletely;
  idkChangeLayer:
      if ADiff.ChangingBoundsDefined then
        LayerMayChange(SelectedImageLayerReadOnly, ADiff.ChangingBounds)
      else
        LayerMayChangeCompletely(SelectedImageLayerReadOnly);
  idkChangeSelection:
      if ADiff.ChangingBoundsDefined then
        SelectionMayChange(ADiff.ChangingBounds)
      else
        SelectionMayChangeCompletely;
  end;
end;

procedure TLazPaintImage.Redo;
var diff: TCustomImageDifference;
begin
  if CanRedo then
  begin
    if not CheckNoAction then exit;
    try
      inc(FUndoPos);
      diff := TCustomImageDifference(FUndoList[FUndoPos]);
      diff.ApplyTo(FCurrentState);
      InvalidateImageDifference(diff);
    except
      on ex:Exception do
      begin
        NotifyException('Redo',ex);
        ClearUndo;
        ImageMayChangeCompletely;
        SelectionMayChangeCompletely;
      end;
    end;
    CompressUndoIfNecessary;
  end;
end;

procedure TLazPaintImage.ClearUndo;
var i: integer;
begin
  try
    for i := 0 to FUndoList.Count-1 do
      TObject(FUndoList[i]).Free;
  except
    on ex: exception do
    begin
      //ignore
    end;
  end;
  FUndoList.Clear;
  FUndoPos := -1;
end;

procedure TLazPaintImage.CompressUndo;
var i: integer;
begin
  for i := 0 to FUndoList.Count-1 do
    if (TObject(FUndoList[i]) as TStateDifference).TryCompress then exit;
end;

function TLazPaintImage.UsedMemory: int64;
var i: integer;
begin
  result := 0;
  if Assigned(FUndoList) then
    for i := 0 to FUndoList.Count-1 do
      result += (TObject(FUndoList[i]) as TStateDifference).UsedMemory;
end;

procedure TLazPaintImage.ImageMayChange(ARect: TRect;
  ADiscardSelectionLayerAfterMask: boolean);
begin
  IntersectRect(ARect, ARect, rect(0,0,Width,Height));
  if IsRectEmpty(ARect) then exit;

  if ADiscardSelectionLayerAfterMask then DiscardSelectionLayerAfterMask;
  FRenderUpdateRectInPicCoord := RectUnion(FRenderUpdateRectInPicCoord,ARect);
  FRenderedImageInvalidated := RectUnion(FRenderedImageInvalidated, ARect);
  FLastSelectionLayerBoundsIsDefined := false;
  OnImageChanged.NotifyObservers;
end;

procedure TLazPaintImage.ImageMayChangeCompletely;
var i,w,h: integer;
begin
  w := 0;
  h := 0;
  for i := 0 to NbLayers-1 do
    with FCurrentState.LayerBitmap[i] do
    begin
      if Width > w then w := Width;
      if Height > h then h := Height;
    end;
  if (Width <> w) or (Height <> h) then
    FCurrentState.SetSize(w,h);
  ImageMayChange(rect(0,0,Width,Height));
end;

procedure TLazPaintImage.LayerMayChange(ALayer: TBGRABitmap; ARect: TRect);
begin
  If ALayer = nil then exit;
  if ALayer = CurrentSelection then
  begin
    SelectionMayChange(ARect);
    exit;
  end;
  if ALayer = SelectionLayerReadonly then DiscardSelectionLayerAfterMask;
  if ALayer = SelectedImageLayerReadOnly then
    with LayerOffset[currentImageLayerIndex] do
      OffsetRect(ARect,X,Y);
  ImageMayChange(ARect);
end;

procedure TLazPaintImage.LayerMayChangeCompletely(ALayer: TBGRABitmap);
begin
  If ALayer = nil then exit;
  LayerMayChange(ALayer,rect(0,0,ALayer.Width,ALayer.Height));
end;

procedure TLazPaintImage.SelectionMayChange(ARect: TRect);
var temp: TRect;
begin
  IntersectRect(ARect, ARect, rect(0,0,Width,Height));
  if IsRectEmpty(ARect) then exit;

  DiscardSelectionLayerAfterMask;
  if not IsRectEmpty(ARect) then
  begin
    temp := ARect;
    InflateRect(temp,1,1);
    FRenderUpdateRectInPicCoord := RectUnion(FRenderUpdateRectInPicCoord,temp);
  end;
  FLastSelectionBoundsIsDefined := false;
  if Assigned(FOnSelectionChanged) then FOnSelectionChanged(self,False);
  if GetSelectionLayerIfExists <> nil then
    LayerMayChange(GetSelectionLayerIfExists, ARect)
  else
    OnImageChanged.NotifyObservers;
end;

procedure TLazPaintImage.SelectionMayChangeCompletely;
begin
  SelectionMayChange(rect(0,0,Width,Height));
end;

procedure TLazPaintImage.RenderMayChange(ARect: TRect; APicCoords: boolean = false);
begin
  if APicCoords then
     FRenderUpdateRectInPicCoord := RectUnion(FRenderUpdateRectInPicCoord,ARect)
  else
      FRenderUpdateRectInVSCoord := RectUnion(FRenderUpdateRectInVSCoord,ARect);
end;

procedure TLazPaintImage.LayerBlendMayChange(AIndex: integer);
var r: TRect;
begin
  r := FCurrentState.LayerBitmap[AIndex].GetImageBounds;
  with LayerOffset[AIndex] do OffsetRect(r, x,y);
  ImageMayChange(r);
end;

procedure TLazPaintImage.SelectionOffsetChanged;
begin
  if Assigned(FOnSelectionChanged) then FOnSelectionChanged(self,True);
end;

function TLazPaintImage.MakeLayeredBitmapAndSelectionCopy: TLayeredBitmapAndSelection;
begin
  result.layeredBitmap := FCurrentState.GetLayeredBitmapCopy;
  result.selection := DuplicateBitmap(CurrentSelection);
  result.selectionLayer := DuplicateBitmap(GetSelectionLayerIfExists);
end;

{--------------------- Selection --------------------------------------}

procedure TLazPaintImage.QuerySelection;
begin
    if currentSelection = nil then
    begin
      ReplaceCurrentSelection(TBGRABitmap.Create(Width,Height, BGRABlack));
      FLastSelectionBoundsIsDefined := true;
      FLastSelectionBounds := EmptyRect;
    end;
end;

function TLazPaintImage.SelectionEmpty: boolean;
begin
  result := IsRectEmpty(SelectionBounds[False]);
end;

function TLazPaintImage.SelectionEmptyComputed: boolean;
begin
  result := FLastSelectionBoundsIsDefined;
end;

function TLazPaintImage.SelectionNil: boolean;
begin
  result := (CurrentSelection = nil);
end;

function TLazPaintImage.GetHeight: integer;
begin
  result := FCurrentState.Height;
end;

function TLazPaintImage.GetSelectedImageLayer: TBGRABitmap;
begin
  result := FCurrentState.currentLayer;
  if (result = nil) and (NbLayers > 0) then
  begin
    SetCurrentImageLayerIndex(0);
    result := FCurrentState.currentLayer;
  end;
end;

function TLazPaintImage.GetCurrentImageLayerIndex: integer;
begin
  result := FCurrentState.currentLayerIndex;
  if (result = -1) and (NbLayers > 0) then
  begin
    SetCurrentImageLayerIndex(0);
    result := 0;
  end;
end;

function TLazPaintImage.GetCurrentFilenameUTF8: string;
begin
  result := FCurrentState.filenameUTF8;
end;

function TLazPaintImage.GetCurrentLayerVisible: boolean;
var idx: integer;
begin
  idx := currentImageLayerIndex;
  if (idx < 0) or (idx >= NbLayers) then
    result := false
  else
    result := LayerVisible[currentImageLayerIndex];
end;

procedure TLazPaintImage.DiscardSelectionLayerAfterMask;
begin
  if FSelectionLayerAfterMaskDefined then
  begin
    FreeAndNil(FSelectionLayerAfterMask);
    FSelectionLayerAfterMaskOffset := Point(0,0);
    FSelectionLayerAfterMaskDefined := false;
  end;
end;

function TLazPaintImage.GetLayerBitmapById(AId: integer): TBGRABitmap;
begin
  result := FCurrentState.LayerBitmapById[AId];
end;

function TLazPaintImage.GetLayerId(AIndex: integer): integer;
begin
  result := FCurrentState.LayerId[AIndex];
end;

procedure TLazPaintImage.NeedSelectionLayerAfterMask;
var r: TRect;
begin
  if not FSelectionLayerAfterMaskDefined then
  begin
    FSelectionLayerAfterMask := (SelectionLayerReadonly.Duplicate) as TBGRABitmap;
    if FSelectionLayerAfterMask <> nil then
    begin
      if Assigned(CurrentSelection) then FSelectionLayerAfterMask.ApplyMask(CurrentSelection);
      r := FSelectionLayerAfterMask.GetImageBounds;
      FSelectionLayerAfterMaskOffset := Point(r.left,r.top);
      if IsRectEmpty(r) then FreeAndNil(FSelectionLayerAfterMask) else
      if (r.left <> 0) or (r.right <> FSelectionLayerAfterMask.Width) or
       (r.top <> 0) or (r.bottom <> FSelectionLayerAfterMask.Height) then
        BGRAReplace(FSelectionLayerAfterMask, FSelectionLayerAfterMask.GetPart(r));
    end;
    FSelectionLayerAfterMaskDefined := true;
  end;
end;

function TLazPaintImage.GetBlendOperation(AIndex: integer): TBlendOperation;
begin
  result := FCurrentState.BlendOperation[AIndex];
end;

function TLazPaintImage.GetEmpty: boolean;
begin
  result := (NbLayers = 0) or ((NbLayers = 1) and FCurrentState.LayerBitmap[0].Empty);
end;

procedure TLazPaintImage.MergeWithSelection(AApplyMask: boolean);
var offs: TPoint;
  sourceRect,destRect: TRect;
begin
    if not SelectionLayerIsEmpty and not (AApplyMask and SelectionEmpty) then
    begin
      sourceRect := SelectionLayerBounds[False];
      if AApplyMask then
      begin
        GetSelectionLayerIfExists.ApplyMask(currentSelection,SelectionLayerBounds[False]);
        IntersectRect(sourceRect,sourceRect,SelectionBounds[False]);
        LayerMayChange(GetSelectionLayerIfExists,SelectionLayerBounds[False]);
      end;
      offs := LayerOffset[currentImageLayerIndex];
      destRect := sourceRect;
      OffsetRect(destRect, -offs.x,-offs.y);
      GetSelectedImageLayer.PutImagePart(destRect.left,destRect.top,GetSelectionLayerIfExists,sourceRect,dmDrawWithTransparency);
      LayerMayChange(GetSelectedImageLayer,destRect);
      ReplaceSelectionLayer(nil,true);
    end;
end;

procedure TLazPaintImage.SetBlendOperation(AIndex: integer;
  AValue: TBlendOperation);
begin
  AddUndo(FCurrentState.SetBlendOp(AIndex,AValue));
  LayerBlendMayChange(AIndex);
end;

procedure TLazPaintImage.SetCurrentFilenameUTF8(AValue: string);
begin
  FCurrentState.filenameUTF8 := AValue;
  if Assigned(FOnCurrentFilenameChanged) then
    FOnCurrentFilenameChanged(self);
end;

procedure TLazPaintImage.SelectImageLayer(AValue: TBGRABitmap);
begin
  FCurrentState.currentLayer := AValue;
end;

function TLazPaintImage.SetCurrentImageLayerIndex(AValue: integer): boolean;
begin
  if not CheckNoAction then
  begin
    result := false;
    exit;
  end;
  FCurrentState.currentLayerIndex := AValue;
  if assigned(OnSelectedLayerIndexChanged) then OnSelectedLayerIndexChanged(self);
  result := true;
end;

procedure TLazPaintImage.SetLayerOffset(AIndex: integer; AValue: TPoint;
  APrecomputedLayerBounds: TRect);
begin
  OffsetRect(APrecomputedLayerBounds, LayerOffset[AIndex].x,LayerOffset[AIndex].y);
  ImageMayChange(APrecomputedLayerBounds);
  OffsetRect(APrecomputedLayerBounds, -LayerOffset[AIndex].x,-LayerOffset[AIndex].y);
  AddUndo(FCurrentState.SetLayerOffset(AIndex,AValue));
  OffsetRect(APrecomputedLayerBounds, LayerOffset[AIndex].x,LayerOffset[AIndex].y);
  ImageMayChange(APrecomputedLayerBounds);
  OffsetRect(APrecomputedLayerBounds, -LayerOffset[AIndex].x,-LayerOffset[AIndex].y);
end;

function TLazPaintImage.CheckNoAction(ASilent: boolean): boolean;
begin
  result := true;
  if ActionInProgress <> nil then
  begin
    ActionInProgress.TryStop;
    if ActionInProgress <> nil then
    begin
      if Assigned(FOnQueryExitToolHandler) then
        FOnQueryExitToolHandler(self);
      if ActionInProgress <> nil then
      begin
        if not ASilent then MessagePopup(rsActionInProgress,2000);
        result := false;
      end;
    end;
  end;
end;

procedure TLazPaintImage.ZoomFit;
begin
  if Assigned(Zoom) then Zoom.ZoomFit(Width,Height);
end;

procedure TLazPaintImage.ResetRenderUpdateRect;
begin
  FRenderUpdateRectInPicCoord := rect(0,0,0,0);
  FRenderUpdateRectInVSCoord := rect(0,0,0,0);
end;

function TLazPaintImage.GetCurrentSelection: TBGRABitmap;
begin
  result := FCurrentState.currentSelection;
  if result <> nil then result.LinearAntialiasing:= True;
end;

function TLazPaintImage.GetLayerBitmap(AIndex: integer): TBGRABitmap;
begin
  result := FCurrentState.LayerBitmap[AIndex];
end;

function TLazPaintImage.GetLayerName(AIndex: integer): string;
begin
  result := FCurrentState.LayerName[AIndex];
end;

function TLazPaintImage.GetLayerOffset(AIndex: integer): TPoint;
begin
  result := FCurrentState.LayerOffset[AIndex];
end;

function TLazPaintImage.GetLayerOpacity(AIndex: integer): byte;
begin
  result := FCurrentState.LayerOpacity[AIndex];
end;

function TLazPaintImage.GetLayerVisible(AIndex: integer): boolean;
begin
  result := FCurrentState.LayerVisible[AIndex];
end;

function TLazPaintImage.GetNbLayers: integer;
begin
  result := FCurrentState.NbLayers;
end;

function TLazPaintImage.GetRenderedImage: TBGRABitmap;
var
  backupCurrentLayer : TBGRABitmap;
  backupTopLeft: TPoint;
  shownSelectionLayer : TBGRABitmap;
  rectSelectionLayer, rectOutput: TRect;
  actualOffset: TPoint;
begin
  if (FRenderedImage = nil) or ((FRenderedImageInvalidated.Right > FRenderedImageInvalidated.Left) and
     (FRenderedImageInvalidated.Bottom > FRenderedImageInvalidated.Top)) then
  begin
    if FCurrentState = nil then
    begin
      FreeAndNil(FRenderedImage);
      result := nil;
      exit;
    end;
    PrepareForRendering;

    backupCurrentLayer := nil;
    FDisplayedSelectionBounds := EmptyRect;
    //if there is an overlapping selection, then we must draw it on current layer
    if (currentSelection <> nil) and (GetSelectedImageLayer <> nil) then
    begin
      actualOffset := LayerOffset[currentImageLayerIndex];
      actualOffset := Point(SelectionOffset.X-actualOffset.X,SelectionOffset.Y-actualOffset.Y);
      shownSelectionLayer := GetSelectionLayerIfExists;
      if shownSelectionLayer <> nil then
      begin
         if FSelectionRotateAngle <> 0 then
         begin
           NeedSelectionLayerAfterMask;
           if FSelectionLayerAfterMask <> nil then
           begin
             rectOutput := GetSelectedImageLayer.GetImageAngleBounds(
                actualOffset.X+FSelectionLayerAfterMaskOffset.x,actualOffset.Y+FSelectionLayerAfterMaskOffset.Y,FSelectionLayerAfterMask,
                FSelectionRotateAngle,FSelectionRotateCenter.X-FSelectionLayerAfterMaskOffset.X,
                FSelectionRotateCenter.Y-FSelectionLayerAfterMaskOffset.Y,True);
             FDisplayedSelectionBounds := rectOutput;
             IntersectRect(rectOutput,rectOutput,FRenderedImageInvalidated);
             if not IsRectEmpty(rectOutput) then
             begin
               backupCurrentLayer := GetSelectedImageLayer.GetPart(rectOutput) as TBGRABitmap;
               backupTopLeft := rectoutput.TopLeft;
               GetSelectedImageLayer.PutImageAngle(
                 actualOffset.X+FSelectionLayerAfterMaskOffset.X,actualOffset.Y+FSelectionLayerAfterMaskOffset.Y,FSelectionLayerAfterMask,
                 FSelectionRotateAngle,rectOutput,FSelectionRotateCenter.X-FSelectionLayerAfterMaskOffset.X,
                FSelectionRotateCenter.Y-FSelectionLayerAfterMaskOffset.Y,255,True,True);
             end;
           end;
         end else
         begin
           DiscardSelectionLayerAfterMask;
           FDisplayedSelectionBounds := rect(0,0,Width,Height);
           rectSelectionLayer := FRenderedImageInvalidated;
           OffsetRect(rectSelectionLayer, -SelectionOffset.X,-SelectionOffset.Y);
           IntersectRect(rectSelectionLayer, rectSelectionLayer, rect(0,0,shownSelectionLayer.Width,shownSelectionLayer.Height));
           if not IsRectEmpty(rectSelectionLayer) then
           begin
             shownSelectionLayer := shownSelectionLayer.GetPart(rectSelectionLayer) as TBGRABitmap;
             shownSelectionLayer.ApplyMask(CurrentSelection,rect(0,0,shownSelectionLayer.Width,shownSelectionLayer.Height),
               Point(rectSelectionLayer.Left,rectSelectionLayer.Top));
             if not shownSelectionLayer.Empty then
             begin
               OffsetRect(rectSelectionLayer, actualOffset.X,actualOffset.Y);
               backupCurrentLayer := GetSelectedImageLayer.GetPart(rectSelectionLayer) as TBGRABitmap;
               backupTopLeft := Point(rectSelectionLayer.Left,rectSelectionLayer.Top);
               GetSelectedImageLayer.PutImage(rectSelectionLayer.Left,rectSelectionLayer.Top,
                 shownSelectionLayer,dmDrawWithTransparency);
             end;
             FreeAndNil(shownSelectionLayer);
           end;
         end;
      end;
    end;

    if (FRenderedImage <> nil) and ((FRenderedImage.Width <> Width) or (FRenderedImage.Height <> Height)) then
      FreeAndNil(FRenderedImage);
    if FRenderedImage = nil then
    begin
      FRenderedImage := FCurrentState.ComputeFlatImageWithoutSelection;
    end else
    begin
      FRenderedImage.ClipRect := FRenderedImageInvalidated;
      FRenderedImage.FillRect(FRenderedImageInvalidated,BGRAPixelTransparent,dmSet);
      FCurrentState.DrawLayers(FRenderedImage,0,0);
    end;

    //restore
    if backupCurrentLayer <> nil then
    begin
      GetSelectedImageLayer.PutImage(backupTopLeft.X,backupTopLeft.Y,backupCurrentLayer,dmSet);
      backupCurrentLayer.Free;
    end;
    FRenderedImageInvalidated := EmptyRect; //up to date
  end;
  result := FRenderedImage;
end;

function TLazPaintImage.GetSelectedLayerPixel(X, Y: Integer): TBGRAPixel;
begin
  result := GetSelectedImageLayer.GetPixel(X,Y);
end;

function TLazPaintImage.GetSelectionBounds(AOffseted: boolean): TRect;
begin
  if FLastSelectionBoundsIsDefined then
    result := FLastSelectionBounds
  else
  if CurrentSelection = nil then
  begin
    result := EmptyRect;
    FLastSelectionBounds := result;
    FLastSelectionBoundsIsDefined := true;
  end else
  begin
    result := CurrentSelection.GetImageBounds(cGreen);
    FLastSelectionBounds := result;
    FLastSelectionBoundsIsDefined := true;
    if not IsRectEmpty(result) and AOffseted then
      OffsetRect(result,SelectionOffset.X,SelectionOffset.Y);
  end;
end;

function TLazPaintImage.GetSelectionLayerBounds(AOffseted: boolean): TRect;
begin
  if FLastSelectionLayerBoundsIsDefined then
    result := FLastSelectionLayerBounds
  else
  if GetSelectionLayerIfExists = nil then
  begin
    result := EmptyRect;
    FLastSelectionLayerBounds := result;
    FLastSelectionLayerBoundsIsDefined := true;
  end else
  begin
    result := GetSelectionLayerIfExists.GetImageBounds;
    FLastSelectionLayerBounds := result;
    FLastSelectionLayerBoundsIsDefined := true;
    if not IsRectEmpty(result) and AOffseted then
      OffsetRect(result,SelectionOffset.X,SelectionOffset.Y);
  end;
end;

function TLazPaintImage.GetWidth: integer;
begin
  result := FCurrentState.Width;
end;

function TLazPaintImage.GetZoomFactor: single;
begin
  if Assigned(Zoom) then result := Zoom.Factor else result := 1;
end;

procedure TLazPaintImage.Assign(const AValue: TBGRABitmap; AOwned: boolean; AUndoable: boolean);
var layeredBmp: TBGRALayeredBitmap;
begin
  if not CheckNoAction then exit;
  if not AUndoable then
  begin
    FCurrentState.Assign(AValue, AOwned);
    FreeAndNil(FCurrentState.selectionLayer);
    FreeAndNil(FCurrentState.currentSelection);
    LayeredBitmapReplaced;
    ImageMayChangeCompletely;
    SelectionMayChangeCompletely;
    ClearUndo;
  end else
  begin
    layeredBmp := TBGRALayeredBitmap.Create(AValue.Width,AValue.Height);
    if AOwned then
      layeredBmp.AddOwnedLayer(AValue)
    else
      layeredBmp.AddLayer(AValue);
    Assign(layeredBmp,True,AUndoable);
  end;
end;

procedure TLazPaintImage.Assign(const AValue: TBGRALayeredBitmap;
  AOwned: boolean; AUndoable: boolean);
var idx: integer;
begin
  if not CheckNoAction then exit;
  if AValue.NbLayers = 0 then
  begin
    Assign(TBGRABitmap.Create(AValue.Width,AValue.Height),True,AUndoable);
    if AOwned then AValue.Free;
    exit;
  end;
  if AUndoable then
  begin
    idx := FCurrentState.currentLayerIndex;
    if idx > AValue.NbLayers-1 then idx := 0;
    AddUndo(FCurrentState.AssignWithUndo(AValue,AOwned,idx,nil,nil));
    ImageMayChangeCompletely;
    SelectionMayChangeCompletely;
  end else
  begin
    FCurrentState.Assign(AValue,AOwned);
    FreeAndNil(FCurrentState.selectionLayer);
    FreeAndNil(FCurrentState.currentSelection);
    LayeredBitmapReplaced;
    ImageMayChangeCompletely;
    SelectionMayChangeCompletely;
    ClearUndo;
  end;
end;

procedure TLazPaintImage.Assign(const AValue: TLayeredBitmapAndSelection;
  AOwned: boolean; AUndoable: boolean);
begin
  if not CheckNoAction then exit;
  if AUndoable then
  begin
    AddUndo(FCurrentState.AssignWithUndo(AValue.layeredBitmap,AOwned,FCurrentState.currentLayerIndex,AValue.selection,AValue.selectionLayer));
    ImageMayChangeCompletely;
    SelectionMayChangeCompletely;
  end
  else
  begin
    with AValue do
    begin
      Assign(layeredBitmap,AOwned,False);
      if not AOwned then
        ReplaceCurrentSelection(selection.Duplicate(True) as TBGRABitmap)
      else
        ReplaceCurrentSelection(selection);
      ReplaceSelectionLayer(selectionLayer,AOwned);
    end;
  end;
  OnImageChanged.NotifyObservers;
end;

procedure TLazPaintImage.ApplyLayerOffset(AOffsetX,AOffsetY: integer);
begin
  AddUndo(FCurrentState.ApplyLayerOffset(AOffsetX,AOffsetY));
end;

procedure TLazPaintImage.ReplaceSelectedLayer(AValue: TBGRABitmap; AOwned: boolean);
var dest: TBGRABitmap;
begin
  dest := GetSelectedImageLayer;
  if dest = nil then exit;
  if (AValue.Width = dest.Width) and (AValue.Height = dest.Height) then
  begin
    dest.PutImage(0,0,AValue,dmSet);
    LayerMayChange(dest, AValue.GetDifferenceBounds(dest));
  end else
  begin
    dest.FillTransparent;
    dest.PutImage((dest.Width-AValue.Width) div 2,(dest.Height-AValue.Height) div 2,AValue,dmSet);
    LayerMayChangeCompletely(dest);
  end;
  if AOwned then AValue.Free;
end;

procedure TLazPaintImage.Draw(ADest: TBGRABitmap; x, y: integer);
var bmp: TBGRABitmap;
begin
  if (NbLayers = 1) and ((currentSelection = nil) or (GetSelectedImageLayer = nil)) then
  begin
    if FCurrentState <> nil then
      FCurrentState.DrawLayers(ADest,x,y);
  end else
  begin
    bmp := RenderedImage;
    if bmp <> nil then
      if FCurrentState.LinearBlend then
        ADest.PutImage(x,y,bmp,dmLinearBlend)
      else
        ADest.PutImage(x,y,bmp,dmDrawWithTransparency);
  end;
end;

procedure TLazPaintImage.AddNewLayer;
begin
  if not CheckNoAction then exit;
  try
    AddUndo(FCurrentState.AddNewLayer(TBGRABitmap.Create(Width,Height),''));
  except on ex: exception do NotifyException('AddNewLayer',ex);
  end;
  OnImageChanged.NotifyObservers;
end;

procedure TLazPaintImage.AddNewLayer(ALayer: TBGRABitmap; AName: string);
var temp: TBGRAbitmap;
begin
  if not CheckNoAction then exit;
  try
    If (ALayer.Width <> Width) or (ALayer.Height <> Height) then
    begin
      temp := TBGRABitmap.Create(Width,Height);
      temp.PutImage((Width-ALayer.Width) div 2,(Height-ALayer.Height) div 2,ALayer,dmSet);
      ALayer.Free;
      ALayer := temp;
    end;
    AddUndo(FCurrentState.AddNewLayer(ALayer, AName));
    ImageMayChangeCompletely;
  except on ex: exception do NotifyException('AddNewLayer',ex);
  end;
  OnImageChanged.NotifyObservers;
end;

procedure TLazPaintImage.DuplicateLayer;
begin
  if not CheckNoAction then exit;
  try
    AddUndo(FCurrentState.DuplicateLayer);
    LayerBlendMayChange(currentImageLayerIndex);
    OnImageChanged.NotifyObservers;
  except on ex: exception do
    begin
      NotifyException('DuplicateLayer',ex);
      ImageMayChangeCompletely;
    end;
  end;
end;

procedure TLazPaintImage.MergeLayerOver;
begin
  if not CheckNoAction then exit;
  try
    AddUndo(FCurrentState.MergerLayerOver(currentImageLayerIndex));
  except on ex: exception do NotifyException('MergeLayerOver',ex);
  end;
  ImageMayChangeCompletely;
end;

procedure TLazPaintImage.PrepareForRendering;
begin
  if FCurrentState <> nil then FCurrentState.PrepareForRendering;
end;

function TLazPaintImage.MakeLayeredBitmapCopy: TBGRALayeredBitmap;
begin
  result := FCurrentState.GetLayeredBitmapCopy;
end;

function TLazPaintImage.ComputeFlatImage(AFromLayer, AToLayer: integer
  ): TBGRABitmap;
begin
  result := FCurrentState.ComputeFlatImage(AFromLayer,AToLayer);
end;

procedure TLazPaintImage.MoveLayer(AFromIndex, AToIndex: integer);
begin
  if not CheckNoAction then exit;
  try
    AddUndo(FCurrentState.MoveLayer(AFromIndex,AToIndex));
    LayerBlendMayChange(AToIndex);
  except on ex: exception do
    begin
      NotifyException('MoveLayer',ex);
      ImageMayChangeCompletely;
    end;
  end;
end;

procedure TLazPaintImage.RemoveLayer;
begin
  if not CheckNoAction then exit;
  try
    AddUndo(FCurrentState.RemoveLayer);
  except on ex: exception do NotifyException('RemoveLayer',ex);
  end;
  ImageMayChangeCompletely;
end;

procedure TLazPaintImage.SwapRedBlue;
begin
  if not CheckNoAction then exit;
  try
    AddUndo(FCurrentState.SwapRedBlue);
  except on ex: exception do NotifyException('SwapRedBlue',ex);
  end;
  ImageMayChangeCompletely;
end;

procedure TLazPaintImage.LinearNegativeAll;
begin
  if not CheckNoAction then exit;
  try
    AddUndo(FCurrentState.LinearNegative);
  except on ex: exception do NotifyException('LinearNegativeAll',ex);
  end;
  ImageMayChangeCompletely;
end;

procedure TLazPaintImage.HorizontalFlip;
begin
  if not CheckNoAction then exit;
  try
    AddUndo(FCurrentState.HorizontalFlip);
  except on ex: exception do NotifyException('HorizontalFlip',ex);
  end;
  ImageMayChangeCompletely;
end;

procedure TLazPaintImage.VerticalFlip;
begin
  if not CheckNoAction then exit;
  try
    AddUndo(FCurrentState.VerticalFlip);
  except on ex: exception do NotifyException('VerticalFlip',ex);
  end;
  ImageMayChangeCompletely;
end;

procedure TLazPaintImage.RotateCW;
begin
  if not CheckNoAction then exit;
  try
    AddUndo(FCurrentState.RotateCW);
  except on ex: exception do NotifyException('RotateCW',ex);
  end;
  ImageMayChangeCompletely;
  SelectionMayChangeCompletely;
end;

procedure TLazPaintImage.RotateCCW;
begin
  if not CheckNoAction then exit;
  try
    AddUndo(FCurrentState.RotateCCW);
  except on ex: exception do NotifyException('RotateCCW',ex);
  end;
  ImageMayChangeCompletely;
  SelectionMayChangeCompletely;
end;

function TLazPaintImage.CheckCurrentLayerVisible: boolean;
begin
  result := CurrentLayerVisible;
  if not result then
    MessagePopup(rsMustShowLayer,2000);
end;

procedure TLazPaintImage.ReplaceCurrentSelection(const AValue: TBGRABitmap);
begin
  if FCurrentState.currentSelection = AValue then exit;
  FreeAndNil(FCurrentState.currentSelection);
  FCurrentState.currentSelection := AValue;
  SelectionMayChangeCompletely;
end;

procedure TLazPaintImage.RemoveSelection;
var bounds: TRect;
begin
   if currentSelection <> nil then
   begin
      bounds := SelectionBounds[False];
      ReplaceSelectionLayer(nil,true);
      ReplaceCurrentSelection(nil);
      SelectionMayChange(bounds);
   end;
end;

procedure TLazPaintImage.EraseSelectionInBitmap;
var offs: TPoint;
  r: TRect;
begin
  if not SelectionEmpty then
  begin
    offs := LayerOffset[currentImageLayerIndex];
    r := SelectionBounds[False];
    SubstractMask(GetSelectedImageLayer,-offs.X+r.left,-offs.Y+r.top,CurrentSelection,r);
    OffsetRect(r,-offs.x,-offs.y);
    LayerMayChange(GetSelectedImageLayer,r);
  end;
end;

procedure TLazPaintImage.ReleaseSelection;
begin
   if currentSelection <> nil then
   begin
      ApplySelectionMask;
      ApplySelectionTransform(False);
      MergeWithSelection(False);
      ReplaceCurrentSelection(nil);
   end;
end;

procedure TLazPaintImage.RetrieveSelection;
var temp : TBGRABitmap;
  offs: TPoint;
  r: TRect;
begin
    if currentSelection <> nil then
    begin
      MergeWithSelection;
      offs := LayerOffset[currentImageLayerIndex];
      r := SelectionBounds[False];
      OffsetRect(r, -offs.x, -offs.y);
      IntersectRect(r, r, rect(0,0,GetSelectedImageLayer.Width,GetSelectedImageLayer.Height));
      temp := TBGRABitmap.Create(Width,Height);
      temp.PutImagePart(r.left+offs.x,r.top+offs.y,GetSelectedImageLayer,r,dmSet);
      temp.ApplyMask(CurrentSelection,SelectionBounds[False]);
      BGRAReplace(FCurrentState.selectionLayer,temp);
      LayerMayChange(GetSelectionLayerIfExists,SelectionBounds[False]);
    end;
end;

function TLazPaintImage.SelectionLayerIsEmpty: boolean;
begin
  result := IsRectEmpty(SelectionLayerBounds[False]);
end;

procedure TLazPaintImage.ReleaseEmptySelection;
begin
  if SelectionEmpty then ReleaseSelection;
end;

function TLazPaintImage.SelectedLayerEmpty: boolean;
begin
  result := GetSelectedImageLayer.Empty;
end;

function TLazPaintImage.SelectedLayerEquals(AColor: TBGRAPixel): boolean;
begin
  result := GetSelectedImageLayer.Equals(AColor);
end;

function TLazPaintImage.GetSelectionCenter: TPointF;
begin
  result := ugraph.GetSelectionCenter(currentSelection);
end;

procedure TLazPaintImage.SaveSelectionToFileUTF8(AFilename: string);
begin
  if CurrentSelection = nil then exit;
  try
    CurrentSelection.SaveToFileUTF8(AFilename);
  except on ex: exception do NotifyException('SaveSelectionToFile',ex);
  end;
end;

procedure TLazPaintImage.SetSelectionRotateCenter(ACenter: TPointF);
begin
  if FSelectionRotateAngle = 0 then
    FSelectionRotateCenter := ACenter;
end;

procedure TLazPaintImage.SetSelectionRotateAngle(AValue: single);
begin
  FSelectionRotateAngle := AValue;
end;

function TLazPaintImage.GetSelectionRotateAngle: Single;
begin
  result := FSelectionRotateAngle;
end;

function TLazPaintImage.GetSelectionRotateCenter: TPointF;
begin
  result := FSelectionRotateCenter;
end;

function TLazPaintImage.GetSelectionOffset: TPoint;
begin
  result := SelectionOffset;
end;

function TLazPaintImage.SelectionReadonly: TBGRABitmap;
begin
  result := CurrentSelection;
end;

function TLazPaintImage.SelectionLayerReadonly: TBGRABitmap;
begin
  result := GetSelectionLayerIfExists;
end;

function TLazPaintImage.SelectedImageLayerReadOnly: TBGRABitmap;
begin
  result := GetSelectedImageLayer;
end;

function TLazPaintImage.RetrieveSelectionIfLayerEmpty(removeFromBitmap: boolean
  ): boolean;
begin
  if SelectionLayerIsEmpty then
  begin
    RetrieveSelection;
    if removeFromBitmap then EraseSelectionInBitmap;
    result := true;
  end
  else result := false;
end;

constructor TLazPaintImage.Create;
begin
  FCurrentState := TImageState.Create;
  FRenderUpdateRectInPicCoord := rect(0,0,0,0);
  FRenderUpdateRectInVSCoord := rect(0,0,0,0);
  FOnSelectionChanged := nil;
  FOnSelectedLayerIndexChanged := nil;
  FOnStackChanged := nil;
  FOnImageChanged := TLazPaintImageObservable.Create(self);
  FUndoList := TList.Create;
  FUndoPos := -1;

  //current transform
  ImageOffset := Point(0,0);
  FSelectionOffset := Point(0,0);
  FSelectionRotateCenter := PointF(0,0);
  FSelectionRotateAngle := 0;
  FDisplayedSelectionBounds := EmptyRect;

  FLastSelectionBoundsIsDefined := false;
  FLastSelectionLayerBoundsIsDefined := false;
end;

destructor TLazPaintImage.Destroy;
begin
  ClearUndo;
  FUndoList.Free;
  FreeAndNil(FRenderedImage);
  FCurrentState.Free;
  FOnImageChanged.Free;

  inherited Destroy;
end;

initialization

  RegisterPaintNetFormat;
  RegisterOpenRasterFormat;
  BGRAColorQuantizerFactory := TBGRAColorQuantizer;

end.
