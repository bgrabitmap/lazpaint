unit UImage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes, types,
  UImageState, UStateType, Graphics, BGRALayers, UImageObservation;

const
  MaxLayersToAdd = 99;
  MaxImageWidth = 10000;
  MaxImageHeight = 10000;
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
  TOnStackChanged = procedure(ASender: TLazPaintImage; AScrollIntoView: boolean) of object;

  TLayerAction = class;
  TOnTryStopEventHandler = procedure(ASender: TLayerAction) of object;

  { TLayerAction }

  TLayerAction = class
  private
    FImage: TLazPaintImage;
    FBackupSelectedLayer, FBackupSelectionLayer, FBackupSelection: TBGRABitmap;
    FBackupSelectedLayerDefined, FBackupSelectionLayerDefined, FBackupSelectionDefined: boolean;
    FDone: boolean;
    FOnTryStop: TOnTryStopEventHandler;
    function GetBackupDrawingLayer: TBGRABitmap;
    function GetBackupSelectedLayer: TBGRABitmap;
    function GetBackupSelection: TBGRABitmap;
    function GetBackupSelectionLayer: TBGRABitmap;
    function GetCurrentSelection: TBGRABitmap;
    function GetSelectedImageLayer: TBGRABitmap;
    function GetDrawingLayer: TBGRABitmap;
    function GetSelectionOffset: TPoint;
    function GetSelectionRotateAngle: Single;
    function GetSelectionRotateCenter: TPointF;
    procedure SetSelectionOffset(AValue: TPoint);
    procedure SetSelectionRotateAngle(AValue: Single);
    procedure SetSelectionRotateCenter(AValue: TPointF);
  protected
    procedure Cancel;
    procedure NeedSelectionBackup;
    procedure NeedSelectedLayerBackup;
    procedure NeedSelectionLayerBackup;
  public
    constructor Create(AImage: TLazPaintImage);
    procedure Validate;
    destructor Destroy; override;

    procedure QuerySelection;
    procedure RemoveSelection;
    procedure EraseSelectionInBitmap;
    procedure ReleaseSelection;
    procedure RetrieveSelection;
    procedure RetrieveSelectionIfLayerEmpty(removeFromBitmap: boolean = false);
    procedure ApplySelectionTransform(ApplyToMask: boolean= true);
    procedure ApplySelectionMask;

    procedure ReplaceSelectedLayer(AValue: TBGRABitmap; AOwned: boolean);
    procedure ReplaceSelectionLayer(bmp: TBGRABitmap; AOwned: boolean);
    procedure ReplaceDrawingLayer(bmp: TBGRABitmap; AOwned: boolean);
    procedure ReplaceCurrentSelection(AValue: TBGRABitmap);

    procedure TryStop;

    function GetOrCreateSelectionLayer: TBGRABitmap;
    function GetSelectionLayerIfExists: TBGRABitmap;
    property SelectedImageLayer: TBGRABitmap read GetSelectedImageLayer;
    property DrawingLayer: TBGRABitmap read GetDrawingLayer;
    property CurrentSelection: TBGRABitmap read GetCurrentSelection;
    property SelectionOffset: TPoint read GetSelectionOffset write SetSelectionOffset;
    property SelectionRotateCenter: TPointF read GetSelectionRotateCenter write SetSelectionRotateCenter;
    property SelectionRotateAngle: Single read GetSelectionRotateAngle write SetSelectionRotateAngle;
    property BackupSelection: TBGRABitmap read GetBackupSelection;
    property BackupSelectionLayer: TBGRABitmap read GetBackupSelectionLayer;
    property BackupSelectedLayer: TBGRABitmap read GetBackupSelectedLayer;
    property BackupDrawingLayer: TBGRABitmap read GetBackupDrawingLayer;
    property OnTryStop: TOnTryStopEventHandler read FOnTryStop write FOnTryStop;
    property Done: boolean read FDone;
  end;

  TOnQueryExitToolHandler = procedure(sender: TLazPaintImage) of object;

  { TLazPaintImage }

  TLazPaintImage = class
  private
    FActionInProgress: TLayerAction;

    FLastSelectionEmpty, FLastSelectionEmptyIsDefined,
    FLastSelectionLayerEmpty, FLastSelectionLayerEmptyIsDefined: boolean;
    FOnSelectionChanged: TOnCurrentSelectionChanged;
    FOnStackChanged: TOnStackChanged;
    FOnQueryExitToolHandler: TOnQueryExitToolHandler;
    FCurrentState: TImageState;
    FRenderedImage: TBGRABitmap;
    FRenderedImageInvalidated: TRect;
    FOnImageChanged: TLazPaintImageObservable;
    FUndoList: TList;
    FUndoPos: integer;

    SelectionOffset: TPoint;
    SelectionRotateAngle: Single;
    SelectionRotateCenter: TPointF;

    function GetBlendOperation(AIndex: integer): TBlendOperation;
    function GetCurrentFilename: string;
    function GetCurrentLayerVisible: boolean;
    function GetSelectedImageLayer: TBGRABitmap;
    function GetCurrentImageLayerIndex:integer;
    function GetEmpty: boolean;
    function GetHeight: integer;
    function GetCurrentSelection: TBGRABitmap;
    function GetLayerBitmap(AIndex: integer): TBGRABitmap;
    function GetLayerName(AIndex: integer): string;
    function GetLayerOpacity(AIndex: integer): byte;
    function GetLayerVisible(AIndex: integer): boolean;
    function GetNbLayers: integer;
    function GetRenderedImage: TBGRABitmap;
    function GetSelectedLayerPixel(X, Y: Integer): TBGRAPixel;
    function GetSelectionLayerBounds(AOffseted: boolean): TRect;
    function GetWidth: integer;
    procedure InvalidateImageDifference(ADiff: TCustomImageDifference);
    procedure MergeWithSelection;
    procedure SetBlendOperation(AIndex: integer; AValue: TBlendOperation);
    procedure SetCurrentFilename(AValue: string);
    procedure SelectImageLayer(AValue: TBGRABitmap);
    procedure ReplaceCurrentSelection(const AValue: TBGRABitmap);
    procedure LayeredBitmapReplaced;
    procedure SetLayerName(AIndex: integer; AValue: string);
    procedure SetLayerOpacity(AIndex: integer; AValue: byte);
    procedure SetLayerVisible(AIndex: integer; AValue: boolean);
    procedure LayerBlendMayChange(AIndex: integer);
    function GetDrawingLayer: TBGRABitmap;
    procedure ReplaceDrawingLayer(bmp: TBGRABitmap; AOwned: boolean);
    procedure QuerySelection;
    procedure RemoveSelection;
    procedure EraseSelectionInBitmap;
    procedure ReleaseSelection;
    procedure RetrieveSelection;
    procedure RetrieveSelectionIfLayerEmpty(removeFromBitmap: boolean = false);
    function GetOrCreateSelectionLayer: TBGRABitmap;
    function GetSelectionLayerIfExists: TBGRABitmap;
    procedure ReplaceSelectionLayer(bmp: TBGRABitmap; AOwned: boolean);
    procedure ApplySelectionTransform(ApplyToMask: boolean= true);
    procedure ApplySelectionMask;
    procedure ReplaceSelectedLayer(AValue: TBGRABitmap; AOwned: boolean);
    procedure SelectionOffsetChanged;
    property CurrentSelection: TBGRABitmap read GetCurrentSelection;
    procedure AddUndo(AUndoAction: TCustomImageDifference);
    procedure CompressUndoIfNecessary;

  public
    ImageOffset: TPoint;

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
    procedure ImageMayChange(ARect: TRect);
    procedure ImageMayChangeCompletely;
    procedure SelectionMayChange(ARect: TRect);
    procedure SelectionMayChangeCompletely;
    function SetCurrentImageLayerIndex(AValue: integer): boolean;

    function SelectionEmpty: boolean;
    function SelectionNil: boolean;
    function SelectionLayerIsEmpty: boolean;
    procedure ReleaseEmptySelection;
    function SelectedLayerEmpty: boolean;
    function SelectedLayerEquals(AColor: TBGRAPixel): boolean;
    property SelectedLayerPixel[X,Y: Integer]: TBGRAPixel read GetSelectedLayerPixel;
    function GetSelectionCenter: TPointF;
    procedure SaveSelectionToFile(AFilename: string);
    procedure SetSelectionRotateCenter(ACenter: TPointF);
    function GetSelectionRotateAngle: Single;
    function GetSelectionRotateCenter: TPointF;
    function GetSelectionOffset: TPoint;
    function SelectionReadonly: TBGRABitmap;
    function SelectionLayerReadonly: TBGRABitmap;
    function SelectedImageLayerReadOnly: TBGRABitmap;

    function ComputeRotatedSelection: TBGRABitmap;
    procedure ApplySmartZoom3;
    procedure Resample(AWidth, AHeight: integer; qualityStr: string);
    procedure LoadFromFileSys(AFilename: string);
    procedure LoadFromStreamAsLZP(AStream: TStream);
    procedure Assign(const AValue: TBGRABitmap; AOwned: boolean; AUndoable: boolean); overload;
    procedure Assign(const AValue: TBGRALayeredBitmap; AOwned: boolean; AUndoable: boolean); overload;
    procedure Assign(const AValue: TLayeredBitmapAndSelection; AOwned: boolean; AUndoable: boolean); overload;

    function AbleToSaveAsSys(AFilename: string): boolean;
    function AbleToSaveSelectionAsSys(AFilename: string): boolean;
    procedure SaveToFileSys(AFilename: string);
    procedure SaveToStreamAsLZP(AStream: TStream);
    procedure SetSavedFlag;
    function IsFileModified: boolean;
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

    property currentFilename: string read GetCurrentFilename write SetCurrentFilename;
    property currentImageLayerIndex: integer read GetCurrentImageLayerIndex;
    property RenderedImage: TBGRABitmap read GetRenderedImage;
    property Width: integer read GetWidth;
    property Height: integer read GetHeight;
    property OnSelectionChanged: TOnCurrentSelectionChanged read FOnSelectionChanged write FOnSelectionChanged;
    property OnStackChanged: TOnStackChanged read FOnStackChanged write FOnStackChanged;
    property OnImageChanged: TLazPaintImageObservable read FOnImageChanged;
    property NbLayers: integer read GetNbLayers;
    property Empty: boolean read GetEmpty;
    property SelectionLayerBounds[AOffseted: boolean]: TRect read GetSelectionLayerBounds;
    property LayerName[AIndex: integer]: string read GetLayerName write SetLayerName;
    property LayerBitmap[AIndex: integer]: TBGRABitmap read GetLayerBitmap;
    property LayerVisible[AIndex: integer]: boolean read GetLayerVisible write SetLayerVisible;
    property LayerOpacity[AIndex: integer]: byte read GetLayerOpacity write SetLayerOpacity;
    property BlendOperation[AIndex: integer]: TBlendOperation read GetBlendOperation write SetBlendOperation;
    property CurrentLayerVisible: boolean read GetCurrentLayerVisible;
    property OnQueryExitToolHandler: TOnQueryExitToolHandler read FOnQueryExitToolHandler write FOnQueryExitToolHandler;

    constructor Create;
    destructor Destroy; override;
  end;

function ComputeAcceptableImageSize(AWidth,AHeight: integer): TSize;

implementation

uses UGraph, UResourceStrings, Dialogs, BGRACompressableBitmap, zstream,
    BGRAOpenRaster, BGRAPaintNet, BGRAStreamLayers, UImageDiff;

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

{ TLayerAction }

function TLayerAction.GetSelectedImageLayer: TBGRABitmap;
begin
  NeedSelectedLayerBackup;
  result := FImage.GetSelectedImageLayer;
end;

function TLayerAction.GetCurrentSelection: TBGRABitmap;
begin
  NeedSelectionBackup;
  result := fImage.GetCurrentSelection;
end;

function TLayerAction.GetBackupSelectedLayer: TBGRABitmap;
begin
  NeedSelectedLayerBackup;
  result := FBackupSelectedLayer;
end;

function TLayerAction.GetBackupDrawingLayer: TBGRABitmap;
begin
  if FImage.SelectionEmpty then result := BackupSelectedLayer else
    result := BackupSelectionLayer;
end;

function TLayerAction.GetBackupSelection: TBGRABitmap;
begin
  NeedSelectionBackup;
  result := FBackupSelection;
end;

function TLayerAction.GetBackupSelectionLayer: TBGRABitmap;
begin
  NeedSelectionLayerBackup;
  result := FBackupSelectionLayer;
end;

function TLayerAction.GetDrawingLayer: TBGRABitmap;
begin
  if FImage.SelectionEmpty then result := GetSelectedImageLayer else
    result := GetOrCreateSelectionLayer;
end;

function TLayerAction.GetSelectionOffset: TPoint;
begin
  result := FImage.SelectionOffset;
end;

function TLayerAction.GetSelectionRotateAngle: Single;
begin
  result := FImage.SelectionRotateAngle;
end;

function TLayerAction.GetSelectionRotateCenter: TPointF;
begin
  result := FImage.SelectionRotateCenter;
end;

procedure TLayerAction.SetSelectionOffset(AValue: TPoint);
begin
  if (FImage.SelectionOffset.X <> AValue.X) or
    (FImage.SelectionOffset.Y <> AValue.Y) then
  begin
    FImage.SelectionOffset := AValue;
    FImage.SelectionOffsetChanged;
  end;
end;

procedure TLayerAction.SetSelectionRotateAngle(AValue: Single);
begin
  FImage.SelectionRotateAngle := AValue;
end;

procedure TLayerAction.SetSelectionRotateCenter(AValue: TPointF);
begin
  FImage.SelectionRotateCenter := AValue;
end;

procedure TLayerAction.Cancel;
begin
  if FDone then raise Exception.Create('Already done');
  if FBackupSelectedLayerDefined then
  begin
    FImage.GetSelectedImageLayer.PutImage(0,0,FBackupSelectedLayer,dmSet);
    FImage.ImageMayChangeCompletely;
  end;
  if FBackupSelectionLayerDefined then
  begin
    if (FImage.GetSelectionLayerIfExists = nil) or (FBackupSelectionLayer = nil) then
    begin
      FImage.ReplaceSelectionLayer(FBackupSelectionLayer,True);
      FBackupSelectionLayer := nil;
    end
    else
      FImage.GetSelectionLayerIfExists.PutImage(0,0,FBackupSelectionLayer,dmSet);
    FImage.ImageMayChangeCompletely;
  end;
  if FBackupSelectionDefined then
  begin
    if (FImage.GetCurrentSelection = nil) or (FBackupSelection = nil) then
    begin
      FImage.ReplaceCurrentSelection(FBackupSelection);
      FBackupSelection := nil;
    end
    else
      FImage.GetCurrentSelection.PutImage(0,0,FBackupSelection,dmSet);
    FImage.SelectionMayChangeCompletely;
  end;
  FDone := true;
end;

procedure TLayerAction.NeedSelectionBackup;
begin
  if not FBackupSelectionDefined then
  begin
    FBackupSelection := DuplicateBitmap(FImage.GetCurrentSelection);
    FBackupSelectionDefined := true;
  end;
end;

procedure TLayerAction.NeedSelectedLayerBackup;
begin
  if not FBackupSelectedLayerDefined then
  begin
    FBackupSelectedLayer := DuplicateBitmap(FImage.GetSelectedImageLayer);
    FBackupSelectedLayerDefined := true;
  end;
end;

procedure TLayerAction.NeedSelectionLayerBackup;
begin
  if not FBackupSelectionLayerDefined then
  begin
    FBackupSelectionLayer := DuplicateBitmap(FImage.GetSelectionLayerIfExists);
    FBackupSelectionLayerDefined := true;
  end;
end;

constructor TLayerAction.Create(AImage: TLazPaintImage);
begin
  if AImage <> nil then
  begin
    if not AImage.CheckNoAction(True) then
      raise exception.Create(rsConflictingActions);
  end;
  FImage := AImage;
  FImage.FActionInProgress := self;
  FBackupSelectedLayer := nil;
  FBackupSelection := nil;
  FBackupSelectionLayer := nil;
  FBackupSelectedLayerDefined := false;
  FBackupSelectionDefined := false;
  FBackupSelectionLayerDefined := false;
  FDone := false;
end;

destructor TLayerAction.Destroy;
begin
  if not FDone then Cancel;
  FBackupSelectedLayer.Free;
  FBackupSelection.Free;
  FBackupSelectionLayer.Free;
  if FImage <> nil then
  begin
    if FImage.FActionInProgress = self then
      FImage.FActionInProgress := nil;
  end;
  inherited Destroy;
end;

procedure TLayerAction.ReplaceDrawingLayer(bmp: TBGRABitmap; AOwned: boolean);
begin
  GetDrawingLayer;
  FImage.ReplaceDrawingLayer(bmp,AOwned);
end;

procedure TLayerAction.ReplaceCurrentSelection(AValue: TBGRABitmap);
begin
  NeedSelectionBackup;
  FImage.ReplaceCurrentSelection(AValue);
end;

procedure TLayerAction.TryStop;
begin
  if Assigned(FOnTryStop) then
    FOnTryStop(self);
end;

procedure TLayerAction.QuerySelection;
begin
  NeedSelectionBackup;
  FImage.QuerySelection;
end;

procedure TLayerAction.RemoveSelection;
begin
  if not FImage.SelectionEmpty or (FImage.GetSelectionLayerIfExists <> nil) then
  begin
    NeedSelectionBackup;
    NeedSelectionLayerBackup;
    FImage.RemoveSelection;
  end;
end;

procedure TLayerAction.EraseSelectionInBitmap;
begin
  if not FImage.SelectionEmpty then
  begin
    NeedSelectedLayerBackup;
    FImage.EraseSelectionInBitmap;
  end;
end;

procedure TLayerAction.ReleaseSelection;
begin
  if not FImage.SelectionEmpty then
  begin
    NeedSelectedLayerBackup;
    NeedSelectionBackup;
    NeedSelectionLayerBackup;
    FImage.ReleaseSelection;
  end;
end;

procedure TLayerAction.RetrieveSelection;
begin
  if not FImage.SelectionEmpty then
  begin
    NeedSelectedLayerBackup;
    NeedSelectionLayerBackup;
    FImage.RetrieveSelection;
  end;
end;

procedure TLayerAction.RetrieveSelectionIfLayerEmpty(removeFromBitmap: boolean);
begin
  NeedSelectedLayerBackup;
  NeedSelectionLayerBackup;
  FImage.RetrieveSelectionIfLayerEmpty(removeFromBitmap);
end;

function TLayerAction.GetOrCreateSelectionLayer: TBGRABitmap;
begin
  NeedSelectionLayerBackup;
  result := FImage.GetOrCreateSelectionLayer;
end;

function TLayerAction.GetSelectionLayerIfExists: TBGRABitmap;
begin
  NeedSelectionLayerBackup;
  result := FImage.GetSelectionLayerIfExists;
end;

procedure TLayerAction.ReplaceSelectionLayer(bmp: TBGRABitmap; AOwned: boolean);
begin
  NeedSelectionLayerBackup;
  FImage.ReplaceSelectionLayer(bmp,AOwned);
end;

procedure TLayerAction.ApplySelectionTransform(ApplyToMask: boolean);
begin
  NeedSelectionLayerBackup;
  FImage.ApplySelectionTransform(ApplyToMask);
end;

procedure TLayerAction.ApplySelectionMask;
begin
  NeedSelectionLayerBackup;
  FImage.ApplySelectionMask;
end;

procedure TLayerAction.ReplaceSelectedLayer(AValue: TBGRABitmap; AOwned: boolean);
begin
  NeedSelectedLayerBackup;
  FImage.ReplaceSelectedLayer(AValue, AOwned);
end;

procedure TLayerAction.Validate;
begin
  if FDone then raise Exception.Create('Already done');
  if FBackupSelectedLayerDefined or FBackupSelectionDefined or FBackupSelectionLayerDefined then
  begin
    if FBackupSelectionLayerDefined then
    begin
      if FImage.SelectionLayerIsEmpty then
        FImage.ReplaceSelectionLayer(nil,True);
    end;
    if FBackupSelectionDefined then
    begin
      if FImage.SelectionEmpty then
      begin
        FImage.ReplaceSelectionLayer(nil,true);
        FImage.ReplaceCurrentSelection(nil);
      end;
    end;
    FImage.AddUndo(FImage.FCurrentState.ComputeLayerDifference(FBackupSelectedLayer, FBackupSelectedLayerDefined,
      FBackupSelection, FBackupSelectionDefined, FBackupSelectionLayer, FBackupSelectionLayerDefined));
    FImage.OnImageChanged.NotifyObservers;
  end;
  FDone := true;
end;

{ TLazPaintImage }

function TLazPaintImage.GetOrCreateSelectionLayer: TBGRABitmap;
begin
    if currentSelection = nil then
      raise Exception.Create(rsNoActiveSelection) else
    begin
      if FCurrentState.selectionLayer = nil then
        FCurrentState.selectionLayer := TBGRABitmap.Create(Width,Height);
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
var temp: TBGRABitmap;
begin
    if currentSelection = nil then result := nil else
    begin
      currentSelection.GrayscaleToAlpha;
      temp := currentSelection.FilterRotate(SelectionRotateCenter,SelectionRotateAngle) as TBGRABitmap;
      currentSelection.AlphaToGrayscale;
      temp.AlphaToGrayscale;
      result := TBGRABitmap.Create(currentSelection.width,currentSelection.Height,BGRABlack) as TBGRABitmap;
      result.PutImage(0,0,temp,dmDrawWithTransparency);
      temp.Free;
    end;
end;

procedure TLazPaintImage.ApplySelectionTransform(ApplyToMask: boolean= true);
var temp: TBGRABitmap;
begin
  if SelectionRotateAngle <> 0 then
  begin
    if ApplyToMask and (currentSelection <> nil) then
      ReplaceCurrentSelection(ComputeRotatedSelection);

    if GetSelectionLayerIfExists <> nil then
      ReplaceSelectionLayer(GetSelectionLayerIfExists.FilterRotate(SelectionRotateCenter,SelectionRotateAngle) as TBGRABitmap, True);

    SelectionRotateAngle := 0;
  end;

  if (SelectionOffset.X <> 0) or (SelectionOffset.Y <> 0) then
  begin
    if currentSelection <> nil then
    begin
      temp := TBGRABitmap.Create(Width,Height, BGRABlack);
      temp.PutImage(SelectionOffset.X, SelectionOffset.Y, currentSelection, dmSet);
      ReplaceCurrentSelection(temp);
    end;
    if GetSelectionLayerIfExists <> nil then
    begin
      temp := TBGRABitmap.Create(Width,Height);
      temp.PutImage(SelectionOffset.X, SelectionOffset.Y, GetSelectionLayerIfExists, dmSet);
      ReplaceSelectionLayer(temp,true);
    end;
    SelectionOffset := point(0,0);
  end;
end;

procedure TLazPaintImage.ApplySelectionMask;
begin
     if (currentSelection <> nil) and (GetSelectionLayerIfExists <> nil) then
     begin
       GetSelectionLayerIfExists.ApplyMask(currentSelection);
       ImageMayChangeCompletely;
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
      r := CurrentSelection.GetImageBounds(cGreen);
    if IsRectEmpty(r) then
      FreeAndNil(result)
    else
      if (r.left <> 0) or (r.top <> 0) or (r.right <> result.Width) or (r.bottom <> result.Height) then
        BGRAReplace(result, result.GetPart(r));
  end;
end;

procedure TLazPaintImage.ApplySmartZoom3;
var i: integer;
  zoomed: TLayeredBitmapAndSelection;
begin
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
  except on ex: exception do
    begin
      MessageDlg(ex.Message,mtError,[mbOk],0);
    end;
  end;
  ImageMayChangeCompletely;
  SelectionMayChangeCompletely;
end;

procedure TLazPaintImage.Resample(AWidth, AHeight: integer; qualityStr: string);
var quality : TResampleMode;
    filter : TResampleFilter;
    backup: TImageState;
begin
  if not CheckNoAction then exit;
  try
    backup := FCurrentState.Duplicate as TImageState;

    if qualityStr = rsFast then
      quality := rmSimpleStretch
    else
      quality := rmFineResample;

    filter := rfLinear;
    if qualityStr = rsLinear then
      filter := rfLinear else
    if qualityStr = rsHalfCosine then
      filter := rfHalfCosine else
    if qualityStr = rsCosine then
      filter := rfCosine else
    if qualityStr = rsMitchell then
      filter := rfMitchell else
    if qualityStr = rsSpline then
      filter := rfSpline else
    if qualityStr = rsBestQuality then
      filter := rfBestQuality;

    FCurrentState.Resample(AWidth,AHeight,quality,filter);
    LayeredBitmapReplaced;
    AddUndo(FCurrentState.GetUndoAfterAssign(backup));
    SelectionMayChangeCompletely;
    backup.Free;
  except on ex: exception do
    begin
      MessageDlg(ex.Message,mtError,[mbOk],0);
    end;
  end;
end;

function TLazPaintImage.AbleToSaveAsSys(AFilename: string): boolean;
var ext: string;
begin
  ext := LowerCase(ExtractFileExt(AFilename));
  if (ext='.bmp') or (ext='.jpg') or (ext='.jpeg')
    or (ext='.png') or (ext='.pcx') or (ext='.xpm') or
    (ext='.lzp') or (ext='.ora') then
    result := true else
      result := false;
end;

function TLazPaintImage.AbleToSaveSelectionAsSys(AFilename: string): boolean;
var ext: string;
begin
  ext := LowerCase(ExtractFileExt(AFilename));
  if (ext='.bmp') or (ext='.jpg') or (ext='.jpeg')
    or (ext='.png') or (ext='.pcx') or (ext='.xpm') then
    result := true else
      result := false;
end;

procedure TLazPaintImage.SaveToFileSys(AFilename: string);
var s: TFileStream;
  temp: TBGRABitmap;
  p: pbgrapixel;
  n: integer;
begin
  if LowerCase(ExtractFileExt(AFilename))='.lzp' then
  begin
    s := TFileStream.Create(AFilename, fmCreate);
    try
      SaveToStreamAsLZP(s);
    finally
      s.Free;
    end;
    SetSavedFlag;
  end else
  if LowerCase(ExtractFileExt(AFilename))='.ora' then
  begin
    FCurrentState.SaveToFile(AFilename);
    SetSavedFlag;
  end else
  begin
    if RenderedImage = nil then exit;
    temp := RenderedImage.Duplicate as TBGRABitmap;
    if (LowerCase(ExtractFileExt(AFilename))='.png') and temp.HasTransparentPixels then
    begin
      //avoid png bug with black color
      p := temp.data;
      for n := temp.NbPixels-1 downto 0 do
      begin
        if (p^.alpha <> 0) and (p^.red = 0) and (p^.green = 0) and (p^.blue = 0) then
          p^.blue := 1;
        inc(p);
      end;
    end;
    temp.SaveToFile(AFilename);
    temp.Free;
    if NbLayers = 1 then SetSavedFlag;
  end;
end;

procedure TLazPaintImage.LoadFromFileSys(AFilename: string);
var s: TFileStream;
  ext: string;
  bmp: TBGRALayeredBitmap;
begin
  if not CheckNoAction then exit;
  ext := LowerCase(ExtractFileExt(AFilename));
  if ext = '.lzp' then
  begin
    s := TFileStream.Create(AFilename, fmOpenRead);
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
  comp: TBGRACompressableBitmap;
begin
  if (RenderedImage = nil) or (NbLayers = 0) then exit;

  if (NbLayers > 1) or (LayerOpacity[0] <> 255) or not LayerVisible[0] or (BlendOperation[0]<>boTransparent) then
  begin
    comp := TBGRACompressableBitmap.Create(RenderedImage);
    comp.CompressionLevel := cldefault;
    comp.Caption := 'Preview';
    comp.WriteToStream(AStream);
    comp.Free;
    FCurrentState.SaveToStream(AStream);
  end else
  begin
    comp := TBGRACompressableBitmap.Create(RenderedImage);
    comp.CompressionLevel := cldefault;
    comp.Caption := LayerName[0];
    comp.WriteToStream(AStream);
    comp.Free;
  end;
end;

procedure TLazPaintImage.LoadFromStreamAsLZP(AStream: TStream);
var
  comp: TBGRACompressableBitmap;
  bmp: TBGRABitmap;
begin
  if not CheckNoAction then exit;
  RemoveSelection;
  comp := TBGRACompressableBitmap.Create;
  comp.ReadFromStream(AStream);
  if (comp.Caption = 'Preview') and (AStream.Position < AStream.Size) and CheckStreamForLayers(AStream) then
  begin
    comp.Free; //ignore preview
    FCurrentState.LoadFromStream(AStream);
    ImageMayChangeCompletely;
  end else
  begin
    bmp := comp.GetBitmap;
    with ComputeAcceptableImageSize(bmp.Width,bmp.Height) do
      if (cx < bmp.Width) or (cy < bmp.Height) then
        BGRAReplace(bmp,bmp.Resample(cx,cy,rmFineResample));
    Assign(bmp, true,false);
    LayerName[0] := comp.Caption;
    comp.Free;
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
  i: integer;
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
        exit;
      end else
      if not prevAction.savedAfter and TryCombineImageDiff(AUndoAction,prevAction) then
      begin
        AUndoAction.Free;
        exit;
      end;
    end;
    for I := FUndoList.Count-1 downto FUndoPos+1 do
    begin
      TObject(FUndoList[i]).Free;
      FUndoList.Delete(i);
    end;
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

procedure TLazPaintImage.CompressUndoIfNecessary;
var i: integer;
begin
  for i := 0 to FUndoList.Count-1 do
    if UsedMemory <= MaxUsedMemoryWithoutCompression then break else
    repeat
      if not (TObject(FUndoList[i]) as TStateDifference).TryCompress then break;
    until UsedMemory <= MaxUsedMemoryWithoutCompression;
end;

procedure TLazPaintImage.SetLayerName(AIndex: integer; AValue: string);
begin
  AddUndo(FCurrentState.SetLayerName(AIndex,Avalue));
  OnImageChanged.NotifyObservers;
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
    MessageDlg(rsMustReleaseSelection,mtInformation,[mbOK],0);
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
        MessageDlg(ex.Message,mtError,[mbOK],0);
        ClearUndo;
        ImageMayChangeCompletely;
        SelectionMayChangeCompletely;
      end;
    end;
    CompressUndoIfNecessary;
  end;
end;

procedure TLazPaintImage.InvalidateImageDifference(ADiff: TCustomImageDifference);
begin
  case ADiff.Kind of
  idkChangeStack: OnImageChanged.NotifyObservers;
  idkChangeImageAndSelection: begin
    ImageMayChangeCompletely;
    SelectionMayChangeCompletely;
  end;
  idkChangeImage: ImageMayChangeCompletely;
  idkChangeSelection: SelectionMayChangeCompletely;
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
        MessageDlg(ex.Message,mtError,[mbOK],0);
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

procedure TLazPaintImage.ImageMayChange(ARect: TRect);
begin
  IntersectRect(ARect, ARect, rect(0,0,Width,Height));
  if IsRectEmpty(ARect) then exit;

  FRenderedImageInvalidated := RectUnion(FRenderedImageInvalidated, ARect);
  FLastSelectionLayerEmptyIsDefined := false;
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

procedure TLazPaintImage.SelectionMayChange(ARect: TRect);
begin
  IntersectRect(ARect, ARect, rect(0,0,Width,Height));
  if IsRectEmpty(ARect) then exit;

  FLastSelectionEmptyIsDefined := false;
  if Assigned(FOnSelectionChanged) then FOnSelectionChanged(self,False);
  if GetSelectionLayerIfExists <> nil then
    ImageMayChange(ARect)
  else
    OnImageChanged.NotifyObservers;
end;

procedure TLazPaintImage.SelectionMayChangeCompletely;
begin
  SelectionMayChange(rect(0,0,Width,Height));
end;

procedure TLazPaintImage.LayerBlendMayChange(AIndex: integer);
begin
  ImageMayChange(FCurrentState.LayerBitmap[AIndex].GetImageBounds);
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
        ReplaceCurrentSelection(TBGRABitmap.Create(Width,Height, BGRABlack));
end;

function TLazPaintImage.SelectionEmpty: boolean;
begin
  if FLastSelectionEmptyIsDefined then
    result := FLastSelectionEmpty
  else
  begin
    result := (currentSelection = nil) or currentSelection.Equals(BGRABlack);
    FLastSelectionEmpty := result;
    FLastSelectionEmptyIsDefined := true;
  end;
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

function TLazPaintImage.GetCurrentFilename: string;
begin
  result := FCurrentState.filename;
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

function TLazPaintImage.GetBlendOperation(AIndex: integer): TBlendOperation;
begin
  result := FCurrentState.BlendOperation[AIndex];
end;

function TLazPaintImage.GetEmpty: boolean;
begin
  result := (NbLayers = 0) or ((NbLayers = 1) and FCurrentState.LayerBitmap[0].Empty);
end;

procedure TLazPaintImage.MergeWithSelection;
begin
    if (GetSelectionLayerIfExists <> nil) and (currentSelection <> nil) then
    begin
      GetSelectionLayerIfExists.ApplyMask(currentSelection);
      GetSelectedImageLayer.PutImage(0,0,GetSelectionLayerIfExists,dmDrawWithTransparency);
      ReplaceSelectionLayer(nil,true);
      ImageMayChangeCompletely;
    end;
end;

procedure TLazPaintImage.SetBlendOperation(AIndex: integer;
  AValue: TBlendOperation);
begin
  AddUndo(FCurrentState.SetBlendOp(AIndex,AValue));
  LayerBlendMayChange(AIndex);
end;

procedure TLazPaintImage.SetCurrentFilename(AValue: string);
begin
  FCurrentState.filename := AValue;
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
  result := true;
end;

function TLazPaintImage.CheckNoAction(ASilent: boolean): boolean;
begin
  result := true;
  if FActionInProgress <> nil then
  begin
    FActionInProgress.TryStop;
    if FActionInProgress <> nil then
    begin
      if Assigned(FOnQueryExitToolHandler) then
        FOnQueryExitToolHandler(self);
      if FActionInProgress <> nil then
      begin
        if not ASilent then MessageDlg(rsActionInProgress,mtInformation,[mbOK],0);
        result := false;
      end;
    end;
  end;
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
  shownSelectionLayer : TBGRABitmap;
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
    //if there is an overlapping selection, then we must draw it on current layer
    if (currentSelection <> nil) and (GetSelectedImageLayer <> nil) then
    begin
      shownSelectionLayer := GetSelectionLayerIfExists;
      if shownSelectionLayer <> nil then
      begin
         shownSelectionLayer := shownSelectionLayer.Duplicate as TBGRABitmap;
         shownSelectionLayer.ApplyMask(currentSelection);
         if not shownSelectionLayer.Empty then
         begin
           backupCurrentLayer := GetSelectedImageLayer.Duplicate(True) as TBGRABitmap;
           if SelectionRotateAngle <> 0 then
           begin
             GetSelectedImageLayer.PutImageAngle(SelectionOffset.X,SelectionOffset.Y,shownSelectionLayer,SelectionRotateAngle,SelectionRotateCenter.X,SelectionRotateCenter.Y,255,True);
           end else
             GetSelectedImageLayer.PutImage(SelectionOffset.X,SelectionOffset.Y,shownSelectionLayer,dmDrawWithTransparency);
           FreeAndNil(shownSelectionLayer);
         end else
           FreeAndNil(shownSelectionLayer);
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
    FRenderedImageInvalidated := EmptyRect; //up to date

    //restore
    if backupCurrentLayer <> nil then
    begin
      GetSelectedImageLayer.PutImage(0,0,backupCurrentLayer,dmSet);
      backupCurrentLayer.Free;
    end;
  end;
  result := FRenderedImage;
end;

function TLazPaintImage.GetSelectedLayerPixel(X, Y: Integer): TBGRAPixel;
begin
  result := GetSelectedImageLayer.GetPixel(X,Y);
end;

function TLazPaintImage.GetSelectionLayerBounds(AOffseted: boolean): TRect;
begin
  if SelectionLayerIsEmpty then
    result := EmptyRect
  else
  begin
    result := GetSelectionLayerIfExists.GetImageBounds;
    if not IsRectEmpty(result) and AOffseted then
      OffsetRect(result,SelectionOffset.X,SelectionOffset.Y);
  end;
end;

function TLazPaintImage.GetWidth: integer;
begin
  result := FCurrentState.Width;
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

procedure TLazPaintImage.ReplaceSelectedLayer(AValue: TBGRABitmap; AOwned: boolean);
var dest: TBGRABitmap;
begin
  dest := GetSelectedImageLayer;
  if dest = nil then exit;
  if (AValue.Width = dest.Width) and (AValue.Height = dest.Height) then
  begin
    ImageMayChange(AValue.GetDifferenceBounds(dest));
    dest.PutImage(0,0,AValue,dmSet);
  end else
  begin
    dest.FillTransparent;
    dest.PutImage((dest.Width-AValue.Width) div 2,(dest.Height-AValue.Height) div 2,AValue,dmSet);
    ImageMayChangeCompletely;
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
  except on ex: exception do
    begin
      MessageDlg(ex.Message,mtError,[mbOk],0);
    end;
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
      temp.PutImage(0,0,ALayer,dmSet);
      ALayer.Free;
      ALayer := temp;
    end;
    AddUndo(FCurrentState.AddNewLayer(ALayer, AName));
    ImageMayChangeCompletely;
  except on ex: exception do
    begin
      MessageDlg(ex.Message,mtError,[mbOk],0);
    end;
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
      MessageDlg(ex.Message,mtError,[mbOk],0);
      ImageMayChangeCompletely;
    end;
  end;
end;

procedure TLazPaintImage.MergeLayerOver;
begin
  if not CheckNoAction then exit;
  try
    AddUndo(FCurrentState.MergerLayerOver(currentImageLayerIndex));
  except on ex: exception do
    begin
      MessageDlg(ex.Message,mtError,[mbOk],0);
    end;
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
      MessageDlg(ex.Message,mtError,[mbOk],0);
      ImageMayChangeCompletely;
    end;
  end;
end;

procedure TLazPaintImage.RemoveLayer;
begin
  if not CheckNoAction then exit;
  try
    AddUndo(FCurrentState.RemoveLayer);
  except on ex: exception do
    begin
      MessageDlg(ex.Message,mtError,[mbOk],0);
    end;
  end;
  ImageMayChangeCompletely;
end;

procedure TLazPaintImage.SwapRedBlue;
begin
  if not CheckNoAction then exit;
  try
    AddUndo(FCurrentState.SwapRedBlue);
  except on ex: exception do
    begin
      MessageDlg(ex.Message,mtError,[mbOk],0);
    end;
  end;
  ImageMayChangeCompletely;
end;

procedure TLazPaintImage.LinearNegativeAll;
begin
  if not CheckNoAction then exit;
  try
    AddUndo(FCurrentState.LinearNegative);
  except on ex: exception do
    begin
      MessageDlg(ex.Message,mtError,[mbOk],0);
    end;
  end;
  ImageMayChangeCompletely;
end;

procedure TLazPaintImage.HorizontalFlip;
begin
  if not CheckNoAction then exit;
  try
    AddUndo(FCurrentState.HorizontalFlip);
  except on ex: exception do
    begin
      MessageDlg(ex.Message,mtError,[mbOk],0);
    end;
  end;
  ImageMayChangeCompletely;
end;

procedure TLazPaintImage.VerticalFlip;
begin
  if not CheckNoAction then exit;
  try
    AddUndo(FCurrentState.VerticalFlip);
  except on ex: exception do
    begin
      MessageDlg(ex.Message,mtError,[mbOk],0);
    end;
  end;
  ImageMayChangeCompletely;
end;

procedure TLazPaintImage.RotateCW;
begin
  if not CheckNoAction then exit;
  try
    AddUndo(FCurrentState.RotateCW);
  except on ex: exception do
    begin
      MessageDlg(ex.Message,mtError,[mbOk],0);
    end;
  end;
  ImageMayChangeCompletely;
  SelectionMayChangeCompletely;
end;

procedure TLazPaintImage.RotateCCW;
begin
  if not CheckNoAction then exit;
  try
    AddUndo(FCurrentState.RotateCCW);
  except on ex: exception do
    begin
      MessageDlg(ex.Message,mtError,[mbOk],0);
    end;
  end;
  ImageMayChangeCompletely;
  SelectionMayChangeCompletely;
end;

function TLazPaintImage.CheckCurrentLayerVisible: boolean;
begin
  result := CurrentLayerVisible;
  if not result then
    MessageDlg(rsMustShowLayer,mtInformation,[mbOK],0);
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
      bounds := CurrentSelection.GetImageBounds(cGreen);
      ReplaceSelectionLayer(nil,true);
      ReplaceCurrentSelection(nil);
      SelectionMayChange(bounds);
   end;
end;

procedure TLazPaintImage.EraseSelectionInBitmap;
begin
  if currentSelection <> nil then
  begin
    SubstractMask(GetSelectedImageLayer,currentSelection);
    ImageMayChange(currentSelection.GetImageBounds(cGreen));
  end;
end;

procedure TLazPaintImage.ReleaseSelection;
begin
   if currentSelection <> nil then
   begin
      ApplySelectionMask;
      currentSelection.Fill(BGRAWhite);
      ApplySelectionTransform(False);
      MergeWithSelection;

      ReplaceCurrentSelection(nil);
   end;
end;

procedure TLazPaintImage.RetrieveSelection;
begin
    if currentSelection <> nil then
    begin
      MergeWithSelection;
      ReplaceSelectionLayer(GetSelectedImageLayer,False);
      GetSelectionLayerIfExists.ApplyMask(currentSelection);
      ImageMayChange(currentSelection.GetImageBounds(cGreen));
    end;
end;

function TLazPaintImage.SelectionLayerIsEmpty: boolean;
begin
  if FLastSelectionLayerEmptyIsDefined then
    result := FLastSelectionLayerEmpty
  else
  begin
    result := (GetSelectionLayerIfExists = nil) or (GetSelectionLayerIfExists.Empty);
    FLastSelectionLayerEmpty := result;
    FLastSelectionLayerEmptyIsDefined := true;
  end;
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
  result := ugraph.GetSelectionCenter(GetCurrentSelection);
end;

procedure TLazPaintImage.SaveSelectionToFile(AFilename: string);
begin
  if CurrentSelection = nil then exit;
  try
    CurrentSelection.SaveToFile(AFilename);
  except on ex: exception do
    begin
      MessageDlg(ex.Message,mtError,[mbOk],0);
    end;
  end;
end;

procedure TLazPaintImage.SetSelectionRotateCenter(ACenter: TPointF);
begin
  if SelectionRotateAngle = 0 then
    SelectionRotateCenter := ACenter;
end;

function TLazPaintImage.GetSelectionRotateAngle: Single;
begin
  result := SelectionRotateAngle;
end;

function TLazPaintImage.GetSelectionRotateCenter: TPointF;
begin
  result := SelectionRotateCenter;
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

procedure TLazPaintImage.RetrieveSelectionIfLayerEmpty(removeFromBitmap: boolean = false);
begin
  if SelectionLayerIsEmpty then
  begin
    RetrieveSelection;
    if removeFromBitmap then EraseSelectionInBitmap;
  end;
end;

constructor TLazPaintImage.Create;
begin
  FCurrentState := TImageState.Create;
  FOnSelectionChanged := nil;
  FOnStackChanged := nil;
  FOnImageChanged := TLazPaintImageObservable.Create(self);
  FUndoList := TList.Create;
  FUndoPos := -1;

  //current transform
  ImageOffset := Point(0,0);
  SelectionOffset := Point(0,0);
  SelectionRotateCenter := PointF(0,0);
  SelectionRotateAngle := 0;

  FLastSelectionEmptyIsDefined := false;
  FLastSelectionLayerEmptyIsDefined := false;
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

end.
