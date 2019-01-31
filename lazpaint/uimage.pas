unit UImage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes, types,
  UImageState, UStateType, Graphics, BGRALayers, UImageObservation, FPWriteBMP,
  UImageType, UZoom, BGRATransform, BGRALayerOriginal;

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
  TOnSelectionMaskChanged = procedure(ASender: TLazPaintImage; const ARect: TRect) of object;
  TOnCurrentLayerIndexChanged = procedure(ASender: TLazPaintImage) of object;
  TOnStackChanged = procedure(ASender: TLazPaintImage; AScrollIntoView: boolean) of object;
  TImageExceptionHandler = procedure(AFunctionName: string; AException: Exception) of object;
  TOnCurrentFilenameChanged = procedure(ASender: TLazPaintImage) of object;

  TOnQueryExitToolHandler = procedure(sender: TLazPaintImage) of object;

  { TLazPaintImage }

  TLazPaintImage = class
  private
    FOnSelectedLayerIndexChanging: TOnCurrentLayerIndexChanged;
    FOnSelectionMaskChanged: TOnSelectionMaskChanged;
    FOnSelectedLayerIndexChanged: TOnCurrentLayerIndexChanged;
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

    FSelectionLayerAfterMask: TBGRABitmap;
    FSelectionLayerAfterMaskOffset: TPoint;
    FSelectionLayerAfterMaskDefined: boolean;
    FDraftOriginal: boolean;

    procedure DiscardSelectionLayerAfterMask;
    function GetIsIconCursor: boolean;
    function GetIsTiff: boolean;
    function GetIsGif: boolean;
    function GetLayerBitmapById(AId: integer): TBGRABitmap;
    function GetLayerId(AIndex: integer): integer;
    function GetLayerOriginal(AIndex: integer): TBGRALayerCustomOriginal;
    function GetLayerOriginalDefined(AIndex: integer): boolean;
    function GetLayerOriginalKnown(AIndex: integer): boolean;
    function GetLayerOriginalMatrix(AIndex: integer): TAffineMatrix;
    function GetSelectionLayerEmpty: boolean;
    function GetSelectionMaskBounds: TRect;
    function GetSelectionMaskEmpty: boolean;
    function GetSelectionTransform: TAffineMatrix;
    procedure NeedSelectionLayerAfterMask;
    function GetBlendOperation(AIndex: integer): TBlendOperation;
    function GetCurrentFilenameUTF8: string;
    function GetCurrentLayerVisible: boolean;
    function GetCurrentLayerIndex:integer;
    function GetEmpty: boolean;
    function GetHeight: integer;
    function GetSelectionMask: TBGRABitmap;
    function GetSelectedImageLayer: TBGRABitmap;
    function GetLayerBitmap(AIndex: integer): TBGRABitmap;
    function GetLayerName(AIndex: integer): string;
    function GetLayerOffset(AIndex: integer): TPoint;
    function GetLayerOpacity(AIndex: integer): byte;
    function GetLayerVisible(AIndex: integer): boolean;
    function GetNbLayers: integer;
    function GetRenderedImage: TBGRABitmap;
    function GetSelectedLayerPixel(X, Y: Integer): TBGRAPixel;
    function GetSelectionLayerBounds: TRect;
    function GetWidth: integer;
    function GetZoomFactor: single;
    procedure InvalidateImageDifference(ADiff: TCustomImageDifference);
    procedure SetBlendOperation(AIndex: integer; AValue: TBlendOperation);
    procedure SetCurrentFilenameUTF8(AValue: string);
    procedure LayeredBitmapReplaced;
    procedure SetDraftOriginal(AValue: boolean);
    procedure SetLayerName(AIndex: integer; AValue: string);
    procedure SetLayerOffset(AIndex: integer; AValue: TPoint);
    procedure SetLayerOpacity(AIndex: integer; AValue: byte);
    procedure SetLayerOriginalMatrix(AIndex: integer; AValue: TAffineMatrix);
    procedure SetLayerVisible(AIndex: integer; AValue: boolean);
    procedure LayerBlendMayChange(AIndex: integer);
    function GetDrawingLayer: TBGRABitmap;
    procedure CompressUndoIfNecessary;
    procedure NotifyException(AFunctionName: string; AException: Exception);
    procedure SetSelectionTransform(ATransform: TAffineMatrix);
    procedure ClearUndoAfter;
    procedure UpdateIconFileUTF8(AFilename: string; AOutputFilename: string = '');
    procedure UpdateTiffFileUTF8(AFilename: string; AOutputFilename: string = '');
    procedure UpdateGifFileUTF8(AFilename: string; AOutputFilename: string = '');
    procedure ReplaceCurrentSelectionWithoutUndo(const AValue: TBGRABitmap);

  public
    ActionInProgress: TCustomLayerAction;
    OnException: TImageExceptionHandler;
    ImageOffset: TPoint;
    Zoom: TZoom;
    CursorHotSpot: TPoint;
    BPP, FrameIndex: integer;

    // make copy
    function MakeLayeredBitmapCopy: TBGRALayeredBitmap;
    function MakeLayeredBitmapAndSelectionCopy: TLayeredBitmapAndSelection;
    function MakeBitmapCopy(backgroundColor: TColor): TBitmap;
    function MakeCroppedLayer: TBGRABitmap;

    // undo/redo
    procedure AddUndo(AUndoAction: TCustomImageDifference);
    function CanUndo: boolean;
    function CanRedo: boolean;
    procedure Undo;
    procedure Redo;
    procedure ClearUndo;
    procedure CompressUndo;
    function UsedMemory: int64;

    // invalidating
    procedure ImageMayChange(ARect: TRect; ADiscardSelectionLayerAfterMask: boolean = true);
    procedure ImageMayChangeCompletely;
    procedure LayerMayChange(ALayer: TBGRABitmap; ARect: TRect);
    procedure LayerMayChangeCompletely(ALayer: TBGRABitmap);
    procedure SelectionMaskMayChange(ARect: TRect);
    procedure SelectionMaskMayChangeCompletely;
    procedure RenderMayChange(ARect: TRect; APicCoords: boolean = false);
    procedure ResetRenderUpdateRect;

    // selection mask
    function SelectionMaskNil: boolean;
    function GetSelectionMaskCenter: TPointF;
    procedure SaveSelectionMaskToFileUTF8(AFilename: string);
    function SelectionMaskReadonly: TBGRABitmap;
    procedure ReleaseEmptySelection;

    // selection layer
    function SelectionLayerReadonly: TBGRABitmap;

    // image layer
    function SetCurrentLayerByIndex(AValue: integer): boolean;
    function CurrentLayerEmpty: boolean;
    function CurrentLayerEquals(AColor: TBGRAPixel): boolean;
    property CurrentLayerPixel[X,Y: Integer]: TBGRAPixel read GetSelectedLayerPixel;
    procedure SetLayerOffset(AIndex: integer; AValue: TPoint; APrecomputedLayerBounds: TRect);
    function CurrentLayerReadOnly: TBGRABitmap;

    procedure AddNewLayer;
    procedure AddNewLayer(AOriginal: TBGRALayerCustomOriginal; AName: string; ABlendOp: TBlendOperation);
    procedure AddNewLayer(ALayer: TBGRABitmap; AName: string; ABlendOp: TBlendOperation);
    procedure DuplicateLayer;
    procedure MergeLayerOver;
    procedure MoveLayer(AFromIndex,AToIndex: integer);
    procedure RemoveLayer;

    procedure HorizontalFlip(ALayerIndex: integer); overload;
    procedure VerticalFlip(ALayerIndex: integer); overload;

    // whole image
    procedure Assign(const AValue: TBGRABitmap; AOwned: boolean; AUndoable: boolean); overload;
    procedure Assign(const AValue: TBGRALayeredBitmap; AOwned: boolean; AUndoable: boolean); overload;
    procedure Assign(const AValue: TLayeredBitmapAndSelection; AOwned: boolean; AUndoable: boolean); overload;

    procedure SwapRedBlue;
    procedure LinearNegativeAll;
    procedure NegativeAll;
    procedure HorizontalFlip; overload;
    procedure VerticalFlip; overload;
    procedure RotateCW;
    procedure RotateCCW;
    procedure Resample(AWidth, AHeight: integer; filter: TResampleFilter);
    function ApplySmartZoom3: boolean;

    procedure Flatten;
    function FlatImageEquals(ABitmap: TBGRABitmap): boolean;
    function ComputeFlatImage(AFromLayer,AToLayer: integer; ASeparateXorMask: boolean): TBGRABitmap;
    procedure PrepareForRendering;
    procedure Draw(ADest: TBGRABitmap; x,y: integer);

    // input/output
    function DetectImageFormat(AFilename: string): TBGRAImageFormat;
    procedure LoadFromFileUTF8(AFilename: string);
    function AbleToSaveAsUTF8(AFilename: string): boolean;
    function AbleToSaveSelectionAsUTF8(AFilename: string): boolean;
    procedure SaveToFileUTF8(AFilename: string);
    procedure UpdateMultiImage(AOutputFilename: string = '');
    procedure SetSavedFlag(ASavedBPP: integer = 0; ASavedFrameIndex: integer = 0);
    function IsFileModified: boolean;
    function IsFileModifiedAndSaved: boolean;
    procedure SaveOriginalToStream(AStream: TStream);

    function CheckCurrentLayerVisible: boolean;
    function CheckNoAction(ASilent: boolean = false): boolean;
    procedure ZoomFit;

    property CurrentState: TImageState read FCurrentState;
    property currentFilenameUTF8: string read GetCurrentFilenameUTF8 write SetCurrentFilenameUTF8;
    property CurrentLayerIndex: integer read GetCurrentLayerIndex;
    property SelectionMask: TBGRABitmap read GetSelectionMask;
    property RenderedImage: TBGRABitmap read GetRenderedImage;
    property Width: integer read GetWidth;
    property Height: integer read GetHeight;
    property OnSelectionChanged: TOnSelectionMaskChanged read FOnSelectionMaskChanged write FOnSelectionMaskChanged;
    property OnSelectedLayerIndexChanging: TOnCurrentLayerIndexChanged read FOnSelectedLayerIndexChanging write FOnSelectedLayerIndexChanging;
    property OnSelectedLayerIndexChanged: TOnCurrentLayerIndexChanged read FOnSelectedLayerIndexChanged write FOnSelectedLayerIndexChanged;
    property OnStackChanged: TOnStackChanged read FOnStackChanged write FOnStackChanged;
    property OnImageChanged: TLazPaintImageObservable read FOnImageChanged;
    property NbLayers: integer read GetNbLayers;
    property Empty: boolean read GetEmpty;
    property SelectionLayerBounds: TRect read GetSelectionLayerBounds;
    property SelectionLayerIsEmpty: boolean read GetSelectionLayerEmpty;
    property SelectionMaskBounds: TRect read GetSelectionMaskBounds;
    property SelectionMaskEmpty: boolean read GetSelectionMaskEmpty;
    property LayerName[AIndex: integer]: string read GetLayerName write SetLayerName;
    property LayerBitmap[AIndex: integer]: TBGRABitmap read GetLayerBitmap;
    property LayerBitmapById[AIndex: integer]: TBGRABitmap read GetLayerBitmapById;
    property LayerOriginal[AIndex: integer]: TBGRALayerCustomOriginal read GetLayerOriginal;
    property LayerOriginalDefined[AIndex: integer]: boolean read GetLayerOriginalDefined;
    property LayerOriginalKnown[AIndex: integer]: boolean read GetLayerOriginalKnown;
    property LayerOriginalMatrix[AIndex: integer]: TAffineMatrix read GetLayerOriginalMatrix write SetLayerOriginalMatrix;
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
    property SelectionTransform: TAffineMatrix read GetSelectionTransform write SetSelectionTransform;
    property ZoomFactor: single read GetZoomFactor;
    property DraftOriginal: boolean read FDraftOriginal write SetDraftOriginal;
    property IsIconCursor: boolean read GetIsIconCursor;
    property IsTiff: boolean read GetIsTiff;
    property IsGif: boolean read GetIsGif;
    constructor Create;
    destructor Destroy; override;
  end;

function ComputeAcceptableImageSize(AWidth,AHeight: integer): TSize;

implementation

uses UGraph, UResourceStrings, Dialogs,
    BGRAOpenRaster, BGRAPhoxo, BGRAPaintNet, UImageDiff, ULoading,
    BGRAWriteLzp, BGRAUTF8,
    BGRAPalette, BGRAColorQuantization, UFileSystem,
    BGRAThumbnail, BGRAIconCursor, UTiff, LazPaintType,
    BGRALazPaint, BGRAAnimatedGif;

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

function TLazPaintImage.MakeCroppedLayer: TBGRABitmap;
var r: TRect;
begin
  result := DuplicateBitmap(FCurrentState.SelectionLayer);
  if (result <> nil) and (SelectionMask <> nil) then result.ApplyMask(SelectionMask);
  if (result <> nil) and result.Empty then FreeAndNil(result);
  if result = nil then
  begin
    result := DuplicateBitmap(GetSelectedImageLayer);
    if (result <> nil) and (SelectionMask <> nil) then result.ApplyMask(SelectionMask);
  end;
  if result <> nil then
  begin
    if SelectionMask = nil then
      r := result.GetImageBounds
    else
      r := SelectionMaskBounds;
    if IsRectEmpty(r) then
      FreeAndNil(result)
    else
      if (r.left <> 0) or (r.top <> 0) or (r.right <> result.Width) or (r.bottom <> result.Height) then
        BGRAReplace(result, result.GetPart(r));
  end;
end;

function TLazPaintImage.ApplySmartZoom3: boolean;
var i, idx: integer;
  zoomed: TLayeredBitmapAndSelection;
  ofs: TPoint;
  withOfs: TBGRABitmap;
begin
  result := false;
  if not CheckNoAction then exit;
  try
    zoomed.layeredBitmap := TBGRALayeredBitmap.Create(Width*3,Height*3);
    for i := 0 to NbLayers-1 do
    begin
      idx := zoomed.layeredBitmap.AddOwnedLayer(FCurrentState.LayerBitmap[i].FilterSmartZoom3(moMediumSmooth) as TBGRABitmap,
        FCurrentState.BlendOperation[i], FCurrentState.LayerOpacity[i]);
      ofs := FCurrentState.LayerOffset[i];
      if (ofs.x <> 0) or (ofs.y <> 0) or (zoomed.layeredBitmap.LayerBitmap[idx].Width <> zoomed.layeredBitmap.Width)
        or (zoomed.layeredBitmap.LayerBitmap[idx].Height <> zoomed.layeredBitmap.Height) then
      begin
        withOfs := TBGRABitmap.Create(zoomed.layeredBitmap.Width, zoomed.layeredBitmap.Height);
        withOfs.PutImage(ofs.x*3,ofs.y*3, zoomed.layeredBitmap.LayerBitmap[idx], dmSet);
        zoomed.layeredBitmap.SetLayerBitmap(idx, withOfs, true);
      end;
    end;
    if SelectionMask <> nil then
      zoomed.selection:= SelectionMask.FilterSmartZoom3(moMediumSmooth) as TBGRABitmap
    else zoomed.Selection := nil;
    if FCurrentState.SelectionLayer <> nil then
      zoomed.selectionLayer := FCurrentState.SelectionLayer.FilterSmartZoom3(moMediumSmooth) as TBGRABitmap
    else
      zoomed.selectionLayer := nil;
    AddUndo(FCurrentState.AssignWithUndo(zoomed.layeredBitmap,true, FCurrentState.SelectedImageLayerIndex, zoomed.selection, zoomed.selectionLayer));
    result := true;
  except on ex: exception do NotifyException('ApplySmartZoom3',ex);
  end;
  ImageMayChangeCompletely;
  SelectionMaskMayChangeCompletely;
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
    SelectionMaskMayChangeCompletely;
    backup.Free;
  except on ex: exception do NotifyException('Resample',ex);
  end;
end;

function TLazPaintImage.DetectImageFormat(AFilename: string): TBGRAImageFormat;
var
  s: TStream;
begin
  s := FileManager.CreateFileStream(AFilename, fmOpenRead);
  try
    result := DetectFileFormat(s, ExtractFileExt(AFilename));
  finally
    s.Free;
  end;
end;

function TLazPaintImage.AbleToSaveAsUTF8(AFilename: string): boolean;
var format: TBGRAImageFormat;
begin
  format := SuggestImageFormat(AFilename);
  result := (DefaultBGRAImageWriter[format] <> nil) or (format in [ifIco,ifCur]);
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
var s: TStream;
  format: TBGRAImageFormat;
begin
  format := SuggestImageFormat(AFilename);
  if format in[ifOpenRaster,ifPhoxo,ifLazPaint] then
  begin
    s := FileManager.CreateFileStream(AFilename, fmCreate);
    try
      FCurrentState.SaveToStreamAs(s, format);
    finally
      s.Free;
    end;
    SetSavedFlag;
  end else
  begin
    if RenderedImage = nil then exit;
    s := FileManager.CreateFileStream(AFilename, fmCreate);
    try
      RenderedImage.SaveToStreamAs(s, SuggestImageFormat(AFilename));
    finally
      s.Free;
    end;
    if NbLayers = 1 then SetSavedFlag;
  end;
end;

procedure TLazPaintImage.UpdateMultiImage(AOutputFilename: string = '');
begin
  if IsIconCursor then
    UpdateIconFileUTF8(currentFilenameUTF8, AOutputFilename)
  else if IsTiff then
    UpdateTiffFileUTF8(currentFilenameUTF8, AOutputFilename)
  else if IsGif then
    UpdateGifFileUTF8(currentFilenameUTF8, AOutputFilename)
  else
    ShowMessage(rsFileExtensionNotSupported);
end;

procedure TLazPaintImage.UpdateIconFileUTF8(AFilename: string; AOutputFilename: string);
var
  s: TStream;
  icoCur: TBGRAIconCursor;
  frame: TBGRABitmap;
  newFrameIndex: integer;
begin
  if bpp = 0 then
  begin
    if RenderedImage.HasTransparentPixels then
      bpp := 32
    else
      bpp := 24;
  end;

  if AOutputFilename = '' then AOutputFilename := AFilename;

  frame := BGRADitherIconCursor(RenderedImage, bpp, daFloydSteinberg) as TBGRABitmap;
  icoCur := TBGRAIconCursor.Create;

  try
    if FileManager.FileExists(AFilename) then
    begin
      s := FileManager.CreateFileStream(AFilename,fmOpenRead or fmShareDenyWrite);
      try
        icoCur.LoadFromStream(s);
      finally
        s.Free;
      end;
    end;

    newFrameIndex := icoCur.Add(frame, bpp, true);
    icoCur.FileType:= SuggestImageFormat(AOutputFilename);

    s := FileManager.CreateFileStream(AOutputFilename,fmCreate);
    try
      icoCur.SaveToStream(s);
      SetSavedFlag(bpp, newFrameIndex);
    finally
      s.Free;
    end;
  finally

    frame.free;
    icoCur.Free;
  end;
end;

procedure TLazPaintImage.UpdateTiffFileUTF8(AFilename: string;
  AOutputFilename: string);
var
  s, sAdded: TStream;
  tiff, addedTiff: TTiff;
  newFrameIndex: integer;
begin
  if AOutputFilename = '' then AOutputFilename := AFilename;

  tiff := TTiff.Create;
  addedTiff := TTiff.Create;
  sAdded := nil;
  s := nil;
  try
    if FileManager.FileExists(AFilename) then
    begin
      s := FileManager.CreateFileStream(AFilename,fmOpenRead or fmShareDenyWrite);
      if tiff.LoadFromStream(s) <> teNone then
        raise Exception.Create(StringReplace(rsErrorOnOpeningFile,'%1', AFilename, []));
      FreeAndNil(s);
    end;

    sAdded := TMemoryStream.Create;
    RenderedImage.SaveToStreamAs(sAdded, ifTiff);
    sAdded.Position:= 0;
    if addedTiff.LoadFromStream(sAdded) <> teNone then
      raise Exception.Create(rsInternalError);
    FreeAndNil(sAdded);

    if FrameIndex = TImageEntry.NewFrameIndex then
      newFrameIndex := tiff.Move(addedTiff,0)
    else
    begin
      newFrameIndex := FrameIndex;
      tiff.Delete(newFrameIndex);
      tiff.Move(addedTiff,0,newFrameIndex);
    end;

    s := FileManager.CreateFileStream(AOutputFilename,fmCreate);
    try
      tiff.SaveToStream(s);
      SetSavedFlag(bpp, newFrameIndex);
    finally
      FreeAndNil(s);
    end;
  finally

    addedTiff.Free;
    sAdded.Free;
    tiff.Free;
    s.Free;
  end;

end;

procedure TLazPaintImage.UpdateGifFileUTF8(AFilename: string;
  AOutputFilename: string);
var
  s: TStream;
  gif: TBGRAAnimatedGif;
  newFrameIndex: integer;
begin
  if AOutputFilename = '' then AOutputFilename := AFilename;

  gif := TBGRAAnimatedGif.Create;
  s := nil;
  try
    if FileManager.FileExists(AFilename) then
    begin
      s := FileManager.CreateFileStream(AFilename,fmOpenRead or fmShareDenyWrite);
      gif.LoadFromStream(s);
      FreeAndNil(s);
    end;

    if FrameIndex = TImageEntry.NewFrameIndex then
      newFrameIndex := gif.AddFullFrame(RenderedImage, gif.AverageDelayMs)
    else
    begin
      newFrameIndex := FrameIndex;
      gif.ReplaceFullFrame(newFrameIndex, RenderedImage, gif.FrameDelayMs[newFrameIndex]);
    end;

    s := FileManager.CreateFileStream(AOutputFilename,fmCreate);
    try
      gif.SaveToStream(s);
      SetSavedFlag(bpp, newFrameIndex);
    finally
      FreeAndNil(s);
    end;
  finally

    gif.Free;
    s.Free;
  end;

end;

procedure TLazPaintImage.LoadFromFileUTF8(AFilename: string);
var s: TStream;
  ext: string;
  bmp: TBGRABitmap;
  layeredBmp: TBGRALayeredBitmap;
begin
  if not CheckNoAction then exit;

  ext := UTF8LowerCase(ExtractFileExt(AFilename));
  bmp := nil;
  s := nil;
  try
    s := FileManager.CreateFileStream(AFilename, fmOpenRead or fmShareDenyWrite);

    layeredBmp := TryCreateLayeredBitmapReader(ext) as TBGRALayeredBitmap;
    if Assigned(layeredBmp) then
    begin
      layeredBmp.LoadFromStream(s);
      with ComputeAcceptableImageSize(layeredBmp.Width,layeredBmp.Height) do
        if (cx < layeredBmp.Width) or (cy < layeredBmp.Height) then
          layeredBmp.Resample(cx,cy,rmFineResample);
      CursorHotSpot := Point(0,0);
      Assign(layeredBmp,true,false);
      if layeredBmp is TBGRALazPaintImage then
        SetCurrentLayerByIndex(TBGRALazPaintImage(layeredBmp).SelectedLayerIndex);
      layeredBmp := nil;
    end else
    begin
      bmp := TBGRABitmap.Create;
      bmp.LoadFromStream(s);
      Assign(bmp,true,false);
      bmp := nil;
    end;

  finally
    bmp.Free;
    s.Free;
  end;
end;

procedure TLazPaintImage.SetSavedFlag(ASavedBPP: integer; ASavedFrameIndex: integer);
var i: integer;
begin
  FCurrentState.saved := true;
  self.BPP := ASavedBPP;
  self.FrameIndex := ASavedFrameIndex;
  for i := 0 to FUndoList.Count-1 do
  begin
    TCustomImageDifference(FUndoList[i]).SavedBefore := (i = FUndoPos+1);
    TCustomImageDifference(FUndoList[i]).SavedAfter := (i = FUndoPos);
  end;
  OnImageChanged.NotifyObservers;
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
   if SelectionMaskEmpty then result := GetSelectedImageLayer else
     result := FCurrentState.GetOrCreateSelectionLayer;
end;

procedure TLazPaintImage.LayeredBitmapReplaced;
begin
  FreeAndNil(FRenderedImage);
  if FCurrentState.NbLayers = 0 then
    raise Exception.Create('No layer')
  else
    if FCurrentState.SelectedImageLayerIndex = -1 then
      FCurrentState.SelectedImageLayerIndex := 0;

  if Assigned(FOnStackChanged) then FOnStackChanged(self,True);
  OnImageChanged.NotifyObservers;
  ImageMayChangeCompletely;
end;

procedure TLazPaintImage.SetDraftOriginal(AValue: boolean);
var
  r: TRect;
begin
  if FDraftOriginal=AValue then Exit;
  FDraftOriginal:=AValue;
  if not FDraftOriginal then
  begin
    r := FCurrentState.LayeredBitmap.RenderOriginalsIfNecessary(FDraftOriginal);
    ImageMayChange(r, false);
  end;
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
        writeln('Inverse');
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
          writeln('Inverse (combine)');
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
    writeln(AUndoAction.ToString);
    FCurrentState.saved := AUndoAction.SavedAfter;
    CompressUndoIfNecessary;
  end;
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

procedure TLazPaintImage.SetSelectionTransform(ATransform: TAffineMatrix);

  procedure InvalidateTransformedSelection;
  var selectionChangeRect: TRect;
  begin
    selectionChangeRect := FCurrentState.GetTransformedSelectionMaskBounds;
    if not SelectionLayerIsEmpty then
      ImageMayChange(selectionChangeRect,False);
    if not IsRectEmpty(selectionChangeRect) then
    begin
      InflateRect(selectionChangeRect,1,1);
      RenderMayChange(selectionChangeRect,true);
    end;
  end;

var
  diff: TSetSelectionTransformDifference;
begin
  if ATransform <> CurrentState.SelectionTransform then
  begin
    InvalidateTransformedSelection;
    diff := TSetSelectionTransformDifference.Create(FCurrentState, ATransform);
    diff.ApplyTo(FCurrentState);
    InvalidateTransformedSelection;
    AddUndo(diff);
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

procedure TLazPaintImage.SetLayerOriginalMatrix(AIndex: integer;
  AValue: TAffineMatrix);
var
  prevMatrix: TAffineMatrix;
  r: TRect;
begin
  if LayerOriginalDefined[AIndex] then
  begin
    if not LayerOriginalKnown[AIndex] then
      raise exception.Create('Unknown original cannot be transformed');
    prevMatrix := LayerOriginalMatrix[AIndex];
    FCurrentState.LayeredBitmap.LayerOriginalMatrix[AIndex] := AValue;
    r := FCurrentState.LayeredBitmap.RenderOriginalsIfNecessary(FDraftOriginal);
    ImageMayChange(r, false);
    AddUndo(FCurrentState.ComputeLayerMatrixDifference(AIndex, prevMatrix, AValue));
  end else
  if not IsAffineMatrixIdentity(AValue) then
    raise exception.Create('Raster layer cannot have a matrix transform');
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
        SelectionMaskMayChangeCompletely;
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
      SelectionMaskMayChange(ADiff.ChangingBounds);
    end else
    begin
      ImageMayChangeCompletely;
      SelectionMaskMayChangeCompletely;
    end;
  end;
  idkChangeImage:
      if ADiff.ChangingBoundsDefined then
        ImageMayChange(ADiff.ChangingBounds)
      else
        ImageMayChangeCompletely;
  idkChangeLayer:
      if ADiff.ChangingBoundsDefined then
        LayerMayChange(CurrentLayerReadOnly, ADiff.ChangingBounds)
      else
        LayerMayChangeCompletely(CurrentLayerReadOnly);
  idkChangeSelection:
      if ADiff.ChangingBoundsDefined then
        SelectionMaskMayChange(ADiff.ChangingBounds)
      else
        SelectionMaskMayChangeCompletely;
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
        SelectionMaskMayChangeCompletely;
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
  FCurrentState.DiscardSelectionLayerBounds;
  OnImageChanged.NotifyObservers;
end;

procedure TLazPaintImage.ImageMayChangeCompletely;
begin
  ImageMayChange(rect(0,0,Width,Height));
end;

procedure TLazPaintImage.LayerMayChange(ALayer: TBGRABitmap; ARect: TRect);
var
  ab: TAffineBox;
begin
  If ALayer = nil then exit;
  if ALayer = SelectionMask then
  begin
    SelectionMaskMayChange(ARect);
    exit;
  end;
  if ALayer = SelectionLayerReadonly then
  begin
    DiscardSelectionLayerAfterMask;
    ab := SelectionTransform*TAffineBox.AffineBox(rectF(ARect.Left,ARect.Top,ARect.Right,ARect.Bottom));
    ARect := ab.RectBounds;
  end;
  if ALayer = CurrentLayerReadOnly then
    with LayerOffset[CurrentLayerIndex] do
      OffsetRect(ARect,X,Y);
  ImageMayChange(ARect);
end;

procedure TLazPaintImage.LayerMayChangeCompletely(ALayer: TBGRABitmap);
begin
  If ALayer = nil then exit;
  LayerMayChange(ALayer,rect(0,0,ALayer.Width,ALayer.Height));
end;

procedure TLazPaintImage.SelectionMaskMayChange(ARect: TRect);
var temp: TRect;
  ab: TAffineBox;
begin
  IntersectRect(ARect, ARect, rect(0,0,Width,Height));
  if IsRectEmpty(ARect) then exit;

  DiscardSelectionLayerAfterMask;

  ab := SelectionTransform*TAffineBox.AffineBox(rectF(ARect.Left,ARect.Top,ARect.Right,ARect.Bottom));
  temp := ab.RectBounds;
  InflateRect(temp,1,1);
  FRenderUpdateRectInPicCoord := RectUnion(FRenderUpdateRectInPicCoord,temp);

  FCurrentState.DiscardSelectionMaskBounds;
  if Assigned(FOnSelectionMaskChanged) then FOnSelectionMaskChanged(self, ARect);
  if FCurrentState.SelectionLayer <> nil then
    LayerMayChange(FCurrentState.SelectionLayer, ARect)
  else
    OnImageChanged.NotifyObservers;
end;

procedure TLazPaintImage.SelectionMaskMayChangeCompletely;
begin
  SelectionMaskMayChange(rect(0,0,Width,Height));
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

function TLazPaintImage.MakeLayeredBitmapAndSelectionCopy: TLayeredBitmapAndSelection;
begin
  result.layeredBitmap := FCurrentState.GetLayeredBitmapCopy;
  result.selection := DuplicateBitmap(SelectionMask);
  result.selectionLayer := DuplicateBitmap(FCurrentState.SelectionLayer);
end;

{--------------------- Selection --------------------------------------}

function TLazPaintImage.SelectionMaskNil: boolean;
begin
  result := (SelectionMask = nil);
end;

function TLazPaintImage.GetHeight: integer;
begin
  result := FCurrentState.Height;
end;

function TLazPaintImage.GetSelectedImageLayer: TBGRABitmap;
begin
  result := FCurrentState.SelectedImageLayer;
  if (result = nil) and (NbLayers > 0) then
  begin
    SetCurrentLayerByIndex(0);
    result := FCurrentState.SelectedImageLayer;
  end;
end;

function TLazPaintImage.GetCurrentLayerIndex: integer;
begin
  result := FCurrentState.SelectedImageLayerIndex;
  if (result = -1) and (NbLayers > 0) then
  begin
    SetCurrentLayerByIndex(0);
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
  idx := CurrentLayerIndex;
  if (idx < 0) or (idx >= NbLayers) then
    result := false
  else
    result := LayerVisible[CurrentLayerIndex];
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

function TLazPaintImage.GetIsIconCursor: boolean;
begin
  result := SuggestImageFormat(currentFilenameUTF8) in [ifIco,ifCur];
end;

function TLazPaintImage.GetIsTiff: boolean;
begin
  result := SuggestImageFormat(currentFilenameUTF8) = ifTiff;
end;

function TLazPaintImage.GetIsGif: boolean;
begin
  result := SuggestImageFormat(currentFilenameUTF8) = ifGif;
end;

function TLazPaintImage.GetLayerBitmapById(AId: integer): TBGRABitmap;
begin
  result := FCurrentState.LayerBitmapById[AId];
end;

function TLazPaintImage.GetLayerId(AIndex: integer): integer;
begin
  result := FCurrentState.LayerId[AIndex];
end;

function TLazPaintImage.GetLayerOriginal(AIndex: integer): TBGRALayerCustomOriginal;
begin
  result := FCurrentState.LayerOriginal[AIndex];
end;

function TLazPaintImage.GetLayerOriginalDefined(AIndex: integer): boolean;
begin
  result := FCurrentState.LayerOriginalDefined[AIndex];
end;

function TLazPaintImage.GetLayerOriginalKnown(AIndex: integer): boolean;
begin
  result := FCurrentState.LayerOriginalKnown[AIndex];
end;

function TLazPaintImage.GetLayerOriginalMatrix(AIndex: integer): TAffineMatrix;
begin
  result := FCurrentState.LayerOriginalMatrix[AIndex];
end;

function TLazPaintImage.GetSelectionLayerEmpty: boolean;
begin
  result := FCurrentState.SelectionLayerEmpty;
end;

function TLazPaintImage.GetSelectionMaskBounds: TRect;
begin
  result := FCurrentState.GetSelectionMaskBounds;
end;

function TLazPaintImage.GetSelectionMaskEmpty: boolean;
begin
  result := FCurrentState.SelectionMaskEmpty;
end;

function TLazPaintImage.GetSelectionTransform: TAffineMatrix;
begin
  result := FCurrentState.SelectionTransform;
end;

procedure TLazPaintImage.NeedSelectionLayerAfterMask;
var
  bounds,
  boundsAfter: TRect;
begin
  if not FSelectionLayerAfterMaskDefined then
  begin
    if SelectionMaskEmpty or SelectionLayerIsEmpty then
      FreeAndNil(FSelectionLayerAfterMask)
    else
    begin
      bounds := SelectionLayerBounds;
      FSelectionLayerAfterMask := SelectionLayerReadonly.GetPart(bounds) as TBGRABitmap;
      FSelectionLayerAfterMask.ApplyMask(SelectionMask,
          Rect(0,0,FSelectionLayerAfterMask.Width,FSelectionLayerAfterMask.Height),
          bounds.TopLeft);
      FSelectionLayerAfterMaskOffset := bounds.TopLeft;

      boundsAfter := FSelectionLayerAfterMask.GetImageBounds;
      if IsRectEmpty(boundsAfter) then FreeAndNil(FSelectionLayerAfterMask) else
      if (boundsAfter.left > FSelectionLayerAfterMask.Width div 10) or (boundsAfter.right < FSelectionLayerAfterMask.Width*9 div 10) or
         (boundsAfter.top > FSelectionLayerAfterMask.Height div 10) or (boundsAfter.bottom < FSelectionLayerAfterMask.Height*9 div 10) then
      begin
        BGRAReplace(FSelectionLayerAfterMask, FSelectionLayerAfterMask.GetPart(boundsAfter));
        FSelectionLayerAfterMaskOffset.x += boundsAfter.Left;
        FSelectionLayerAfterMaskOffset.y += boundsAfter.Top;
      end;
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

procedure TLazPaintImage.SetBlendOperation(AIndex: integer;
  AValue: TBlendOperation);
begin
  AddUndo(FCurrentState.SetBlendOp(AIndex,AValue));
  LayerBlendMayChange(AIndex);
end;

procedure TLazPaintImage.SetCurrentFilenameUTF8(AValue: string);
var oldIsIco: boolean;
begin
  oldIsIco := IsIconCursor;
  FCurrentState.filenameUTF8 := AValue;
  if oldIsIco <> IsIconCursor then ImageMayChangeCompletely;
  if Assigned(FOnCurrentFilenameChanged) then
    FOnCurrentFilenameChanged(self);
end;

function TLazPaintImage.SetCurrentLayerByIndex(AValue: integer): boolean;
var
  composedDiff: TComposedImageDifference;
  changeIndex: TSelectCurrentLayer;
  applyOfs: TCustomImageDifference;
begin
  if AValue = FCurrentState.SelectedImageLayerIndex then exit(true);
  if (AValue < 0) or (AValue >= NbLayers) then exit(false);
  if not CheckNoAction then
  begin
    result := false;
    exit;
  end;

  if assigned(OnSelectedLayerIndexChanging) then OnSelectedLayerIndexChanging(self);
  if not SelectionLayerIsEmpty then
  begin
    composedDiff := TComposedImageDifference.Create;
    try
      changeIndex := TSelectCurrentLayer.Create(FCurrentState, AValue);
      composedDiff.Add(changeIndex);
      changeIndex.ApplyTo(FCurrentState);
      try
        with FCurrentState.LayerOffset[AValue] do
          applyOfs := FCurrentState.ComputeLayerOffsetDifference(X,Y);
        composedDiff.Add(applyOfs);
      except
        changeIndex.UnApplyTo(FCurrentState);
        composedDiff.Free;
        exit(false);
      end;
      applyOfs.ApplyTo(FCurrentState);
      AddUndo(composedDiff);
      composedDiff:= nil;
    finally
      composedDiff.Free;
    end;
  end else
    FCurrentState.SelectedImageLayerIndex := AValue;
  if assigned(OnSelectedLayerIndexChanged) then OnSelectedLayerIndexChanged(self);
  ImageMayChangeCompletely;

  result := true;
end;

procedure TLazPaintImage.SetLayerOffset(AIndex: integer; AValue: TPoint;
  APrecomputedLayerBounds: TRect);
var
  discardOrig: TDiscardOriginalStateDifference;
  comb: TComposedImageDifference;
begin
  OffsetRect(APrecomputedLayerBounds, LayerOffset[AIndex].x,LayerOffset[AIndex].y);
  ImageMayChange(APrecomputedLayerBounds);
  OffsetRect(APrecomputedLayerBounds, -LayerOffset[AIndex].x,-LayerOffset[AIndex].y);
  if FCurrentState.LayerOriginalDefined[AIndex] then
  begin
    discardOrig := TDiscardOriginalStateDifference.Create(FCurrentState,AIndex);
    discardOrig.ApplyTo(FCurrentState);
    comb := TComposedImageDifference.Create;
    comb.Add(discardOrig);
    comb.Add(FCurrentState.SetLayerOffset(AIndex,AValue));
    AddUndo(comb);
  end else
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

function TLazPaintImage.GetSelectionMask: TBGRABitmap;
begin
  result := FCurrentState.SelectionMask;
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
  backupTopLeft, ofs: TPoint;
  shownSelectionLayer , temp: TBGRABitmap;
  rectOutput, invalidatedRect: TRect;
  actualTransformation: TAffineMatrix;
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
    backupTopLeft := Point(0,0);
    //if there is an overlapping selection, then we must draw it on current layer
    if (SelectionMask <> nil) and (GetSelectedImageLayer <> nil) then
    begin
      shownSelectionLayer := FCurrentState.SelectionLayer;
      if shownSelectionLayer <> nil then
      begin
         if not IsAffineMatrixIdentity(SelectionTransform) then
         begin
           NeedSelectionLayerAfterMask;
           ofs := LayerOffset[CurrentLayerIndex];
           actualTransformation := AffineMatrixTranslation(-ofs.X,-ofs.Y)*SelectionTransform*
                                AffineMatrixTranslation(FSelectionLayerAfterMaskOffset.X,FSelectionLayerAfterMaskOffset.Y);
           if FSelectionLayerAfterMask <> nil then
             shownSelectionLayer := FSelectionLayerAfterMask;
           if shownSelectionLayer <> nil then
           begin
             rectOutput := GetSelectedImageLayer.GetImageAffineBounds(
                actualTransformation, shownSelectionLayer);
             invalidatedRect := FRenderedImageInvalidated;
             OffsetRect(invalidatedRect, -ofs.x,-ofs.y);
             IntersectRect(rectOutput,rectOutput,invalidatedRect);
             if not IsRectEmpty(rectOutput) then
             begin
               backupCurrentLayer := GetSelectedImageLayer.GetPart(rectOutput) as TBGRABitmap;
               backupTopLeft := rectoutput.TopLeft;
               GetSelectedImageLayer.PutImageAffine(
                 actualTransformation, shownSelectionLayer,
                 rectOutput,255,True);
             end;
           end;
         end else
         begin
           DiscardSelectionLayerAfterMask;
           rectoutput := FRenderedImageInvalidated;
           IntersectRect(rectoutput, rectoutput, SelectionLayerBounds);
           IntersectRect(rectoutput, rectoutput, SelectionMaskBounds);
           ofs := LayerOffset[CurrentLayerIndex];
           OffsetRect(rectoutput, -ofs.X,-ofs.Y);
           if not IsRectEmpty(rectoutput) then
           begin
             backupTopLeft := rectOutput.TopLeft;
             backupCurrentLayer := GetSelectedImageLayer.GetPart(rectoutput) as TBGRABitmap;
             shownSelectionLayer.ScanOffset := Point(ofs.x,ofs.y);
             GetSelectedImageLayer.ClipRect := rectOutput;
             GetSelectedImageLayer.FillMask(-ofs.X,-ofs.Y,SelectionMask, shownSelectionLayer, dmDrawWithTransparency);
             shownSelectionLayer.ScanOffset := Point(0,0);
             GetSelectedImageLayer.NoClip;
           end;
         end;
      end;
    end;

    if (FRenderedImage <> nil) and ((FRenderedImage.Width <> Width) or (FRenderedImage.Height <> Height)) then
      FreeAndNil(FRenderedImage);

    if FRenderedImage = nil then FRenderedImage := TBGRABitmap.Create(Width,Height);

    if IsIconCursor then
    begin
      temp := FCurrentState.ComputeFlatImage(FRenderedImageInvalidated,0,NbLayers-1,True);
      FRenderedImage.PutImage(FRenderedImageInvalidated.Left,FRenderedImageInvalidated.Top, temp, dmSet);
      if temp.XorMask <> nil then
      begin
        FRenderedImage.NeedXorMask;
        FRenderedImage.XorMask.PutImage(FRenderedImageInvalidated.Left,FRenderedImageInvalidated.Top, temp.XorMask, dmSet);
      end else
        FRenderedImage.DiscardXorMask;
      temp.Free;
    end else
    begin
      FRenderedImage.ClipRect := FRenderedImageInvalidated;
      FRenderedImage.FillRect(FRenderedImageInvalidated,BGRAPixelTransparent,dmSet);
      FRenderedImage.DiscardXorMask;
      FCurrentState.DrawLayers(FRenderedImage,0,0,False);
      FRenderedImage.NoClip;
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

function TLazPaintImage.GetSelectionLayerBounds: TRect;
begin
  result := FCurrentState.GetSelectionLayerBounds;
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
  mask: TBGRABitmap;
begin
  if not CheckNoAction then exit;
  CursorHotSpot := AValue.HotSpot;
  if not AUndoable then
  begin
    FCurrentState.Assign(AValue, AOwned);
    FCurrentState.RemoveSelection;
    LayeredBitmapReplaced;
    ImageMayChangeCompletely;
    SelectionMaskMayChangeCompletely;
    ClearUndo;
  end else
  begin
    layeredBmp := TBGRALayeredBitmap.Create(AValue.Width,AValue.Height);
    if AOwned then
    begin
      layeredBmp.AddOwnedLayer(AValue);
      if Assigned(AValue.XorMask) then
      begin
        mask := AValue.XorMask.Duplicate as TBGRABitmap;
        mask.AlphaFill(255);
        mask.ReplaceColor(BGRABlack,BGRAPixelTransparent);
        layeredBmp.LayerName[layeredBmp.AddOwnedLayer(mask,boXor)] := 'Xor';
        AValue.DiscardXorMask;
      end;
    end
    else
    begin
      layeredBmp.AddLayer(AValue);
      if Assigned(AValue.XorMask) then
      begin
        mask := AValue.XorMask.Duplicate as TBGRABitmap;
        mask.AlphaFill(255);
        mask.ReplaceColor(BGRABlack,BGRAPixelTransparent);
        layeredBmp.LayerName[layeredBmp.AddOwnedLayer(mask,boXor)] := 'Xor';
      end;
    end;
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
    idx := FCurrentState.SelectedImageLayerIndex;
    if idx > AValue.NbLayers-1 then idx := 0;
    AddUndo(FCurrentState.AssignWithUndo(AValue,AOwned,idx,nil,nil));
    ImageMayChangeCompletely;
    SelectionMaskMayChangeCompletely;
  end else
  begin
    FCurrentState.Assign(AValue,AOwned);
    FCurrentState.RemoveSelection;
    LayeredBitmapReplaced;
    ImageMayChangeCompletely;
    SelectionMaskMayChangeCompletely;
    ClearUndo;
  end;
end;

procedure TLazPaintImage.Assign(const AValue: TLayeredBitmapAndSelection;
  AOwned: boolean; AUndoable: boolean);
begin
  if not CheckNoAction then exit;
  if AUndoable then
  begin
    AddUndo(FCurrentState.AssignWithUndo(AValue.layeredBitmap,AOwned,FCurrentState.SelectedImageLayerIndex,AValue.selection,AValue.selectionLayer));
    ImageMayChangeCompletely;
    SelectionMaskMayChangeCompletely;
  end
  else
  begin
    with AValue do
    begin
      Assign(layeredBitmap,AOwned,False);
      if not AOwned then
        ReplaceCurrentSelectionWithoutUndo(selection.Duplicate(True) as TBGRABitmap)
      else
        ReplaceCurrentSelectionWithoutUndo(selection);
      FCurrentState.ReplaceSelectionLayer(selectionLayer,AOwned);
    end;
  end;
  OnImageChanged.NotifyObservers;
end;

procedure TLazPaintImage.Draw(ADest: TBGRABitmap; x, y: integer);
var bmp: TBGRABitmap;
begin
  if (NbLayers = 1) and ((SelectionMask = nil) or (GetSelectedImageLayer = nil)) then
  begin
    if FCurrentState <> nil then
      FCurrentState.DrawLayers(ADest,x,y,IsIconCursor);
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
    AddUndo(FCurrentState.AddNewLayer(TBGRABitmap.Create(Width,Height),'',boTransparent));
  except on ex: exception do NotifyException('AddNewLayer',ex);
  end;
  OnImageChanged.NotifyObservers;
end;

procedure TLazPaintImage.AddNewLayer(AOriginal: TBGRALayerCustomOriginal;
  AName: string; ABlendOp: TBlendOperation);
begin
  if not CheckNoAction then exit;
  try
    AddUndo(FCurrentState.AddNewLayer(AOriginal,AName,ABlendOp));
    ImageMayChangeCompletely;
  except on ex: exception do NotifyException('AddNewLayer',ex);
  end;
  OnImageChanged.NotifyObservers;
end;

procedure TLazPaintImage.AddNewLayer(ALayer: TBGRABitmap; AName: string; ABlendOp: TBlendOperation);
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
    AddUndo(FCurrentState.AddNewLayer(ALayer, AName,ABlendOp));
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
    LayerBlendMayChange(CurrentLayerIndex);
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
    AddUndo(FCurrentState.MergerLayerOver(CurrentLayerIndex));
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

function TLazPaintImage.ComputeFlatImage(AFromLayer, AToLayer: integer;
  ASeparateXorMask: boolean): TBGRABitmap;
begin
  result := FCurrentState.ComputeFlatImage(AFromLayer,AToLayer,ASeparateXorMask);
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

procedure TLazPaintImage.SaveOriginalToStream(AStream: TStream);
begin
  FCurrentState.LayeredBitmap.SaveOriginalToStream(
    FCurrentState.LayeredBitmap.LayerOriginalGuid[CurrentLayerIndex],
    AStream);
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

procedure TLazPaintImage.NegativeAll;
begin
  if not CheckNoAction then exit;
  try
    AddUndo(FCurrentState.Negative);
  except on ex: exception do NotifyException('NegativeAll',ex);
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

procedure TLazPaintImage.HorizontalFlip(ALayerIndex: integer);
begin
  if not CheckNoAction then exit;
  try
    AddUndo(FCurrentState.HorizontalFlip(ALayerIndex));
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

procedure TLazPaintImage.VerticalFlip(ALayerIndex: integer);
begin
  if not CheckNoAction then exit;
  try
    AddUndo(FCurrentState.VerticalFlip(ALayerIndex));
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
  SelectionMaskMayChangeCompletely;
end;

procedure TLazPaintImage.RotateCCW;
begin
  if not CheckNoAction then exit;
  try
    AddUndo(FCurrentState.RotateCCW);
  except on ex: exception do NotifyException('RotateCCW',ex);
  end;
  ImageMayChangeCompletely;
  SelectionMaskMayChangeCompletely;
end;

function TLazPaintImage.CheckCurrentLayerVisible: boolean;
begin
  result := CurrentLayerVisible;
  if not result then
    MessagePopup(rsMustShowLayer,2000);
end;

procedure TLazPaintImage.ReplaceCurrentSelectionWithoutUndo(const AValue: TBGRABitmap);
begin
  if FCurrentState.SelectionMask = AValue then exit;
  FCurrentState.SelectionMask.Free;
  FCurrentState.SelectionMask := AValue;
  SelectionMaskMayChangeCompletely;
end;

procedure TLazPaintImage.ReleaseEmptySelection;
begin
  if SelectionMaskEmpty and SelectionLayerIsEmpty then
    FCurrentState.ReplaceSelection(nil,nil);
end;

function TLazPaintImage.CurrentLayerEmpty: boolean;
begin
  result := GetSelectedImageLayer.Empty;
end;

function TLazPaintImage.CurrentLayerEquals(AColor: TBGRAPixel): boolean;
begin
  result := GetSelectedImageLayer.Equals(AColor);
end;

function TLazPaintImage.GetSelectionMaskCenter: TPointF;
begin
  result := ugraph.GetSelectionCenter(SelectionMask);
end;

procedure TLazPaintImage.SaveSelectionMaskToFileUTF8(AFilename: string);
var s: TStream;
begin
  if SelectionMask = nil then exit;
  try
    s := FileManager.CreateFileStream(AFilename, fmCreate);
    try
      SelectionMask.SaveToStreamAs(s, SuggestImageFormat(AFilename));
    finally
      s.Free;
    end;
  except on ex: exception do NotifyException('SaveSelectionToFile',ex);
  end;
end;

function TLazPaintImage.SelectionMaskReadonly: TBGRABitmap;
begin
  result := SelectionMask;
end;

function TLazPaintImage.SelectionLayerReadonly: TBGRABitmap;
begin
  result := FCurrentState.SelectionLayer;
end;

function TLazPaintImage.CurrentLayerReadOnly: TBGRABitmap;
begin
  result := GetSelectedImageLayer;
end;

constructor TLazPaintImage.Create;
begin
  FCurrentState := TImageState.Create;
  FRenderUpdateRectInPicCoord := rect(0,0,0,0);
  FRenderUpdateRectInVSCoord := rect(0,0,0,0);
  FOnSelectionMaskChanged := nil;
  FOnSelectedLayerIndexChanged := nil;
  FOnStackChanged := nil;
  FOnImageChanged := TLazPaintImageObservable.Create(self);
  FUndoList := TList.Create;
  FUndoPos := -1;
  ImageOffset := Point(0,0);
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
  RegisterPhoxoFormat;
  RegisterLazPaintFormat;
  BGRAColorQuantizerFactory := TBGRAColorQuantizer;

end.
