unit ULayerAction;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UImage, BGRABitmap, BGRABitmapTypes, UImageType,
  UStateType;

type
  { TLayerAction }

  TLayerAction = class(TCustomLayerAction)
  private
    FAllChangesNotified: boolean;
    FImage: TLazPaintImage;
    FPrediff: TCustomImageDifference;
    FBackupSelectedLayer, FBackupSelectionLayer, FBackupSelection: TBGRABitmap;
    FBackupSelectedLayerDefined, FBackupSelectionLayerDefined, FBackupSelectionDefined: boolean;
    FSelectedLayerChangedArea, FSelectionLayerChangedArea, FSelectionChangedArea: TRect;
    FDone: boolean;
    FOnTryStop: TOnTryStopEventHandler;
    function GetBackupDrawingLayer: TBGRABitmap;
    function GetBackupSelectedLayer: TBGRABitmap;
    function GetBackupSelection: TBGRABitmap;
    function GetBackupSelectionLayer: TBGRABitmap;
    function GetCurrentSelection: TBGRABitmap;
    function GetSelectedImageLayer: TBGRABitmap;
    function GetDrawingLayer: TBGRABitmap;
    function GetSelectedImageLayerOffset: TPoint;
  protected
    procedure Cancel;
    procedure NeedSelectionBackup;
    procedure NeedSelectedLayerBackup;
    procedure NeedSelectionLayerBackup;
    function GetSelectionTransform: TAffineMatrix;
    procedure SetSelectionTransform(AValue: TAffineMatrix);
  public
    constructor Create(AImage: TLazPaintImage; AApplyOfsBefore: boolean = false);
    procedure Validate;
    procedure PartialValidate(ADiscardBackup: boolean = false);
    procedure PartialCancel;
    destructor Destroy; override;

    procedure QuerySelection;
    procedure RemoveSelection;
    procedure EraseSelectionInBitmap;
    procedure ReleaseSelection;
    procedure RetrieveSelection;
    function RetrieveSelectionIfLayerEmpty(removeFromBitmap: boolean = false): boolean;
    procedure ApplySelectionTransform(ApplyToMask: boolean= true);
    procedure ApplySelectionMask;

    procedure ReplaceSelectedLayer(AValue: TBGRABitmap; AOwned: boolean);
    procedure ReplaceSelectionLayer(bmp: TBGRABitmap; AOwned: boolean);
    procedure ReplaceDrawingLayer(bmp: TBGRABitmap; AOwned: boolean);
    procedure ReplaceCurrentSelection(AValue: TBGRABitmap);
    procedure NotifyChange(ADest: TBGRABitmap; ARect: TRect);
    procedure RestoreSelection;
    procedure RestoreDrawingLayer;
    procedure RestoreSelectedLayer;
    procedure RestoreSelectionLayer;

    procedure TryStop; override;

    function GetOrCreateSelectionLayer: TBGRABitmap;
    function GetSelectionLayerIfExists: TBGRABitmap;
    property SelectedImageLayer: TBGRABitmap read GetSelectedImageLayer;
    property SelectedImageLayerOffset: TPoint read GetSelectedImageLayerOffset;
    property DrawingLayer: TBGRABitmap read GetDrawingLayer;
    property CurrentSelection: TBGRABitmap read GetCurrentSelection;
    property BackupSelection: TBGRABitmap read GetBackupSelection;
    property BackupSelectionLayer: TBGRABitmap read GetBackupSelectionLayer;
    property BackupSelectedLayer: TBGRABitmap read GetBackupSelectedLayer;
    property BackupDrawingLayer: TBGRABitmap read GetBackupDrawingLayer;
    property OnTryStop: TOnTryStopEventHandler read FOnTryStop write FOnTryStop;
    property SelectionTransform: TAffineMatrix read GetSelectionTransform write SetSelectionTransform;
    property Done: boolean read FDone;
    property AllChangesNotified: boolean read FAllChangesNotified write FAllChangesNotified;
  end;

implementation

uses UResourceStrings, UGraph, Types, Dialogs, BGRATransform, BGRALayerOriginal, UImageDiff;

{ TLayerAction }

function TLayerAction.GetSelectedImageLayer: TBGRABitmap;
begin
  NeedSelectedLayerBackup;
  result := FImage.GetSelectedImageLayer;
end;

function TLayerAction.GetCurrentSelection: TBGRABitmap;
begin
  NeedSelectionBackup;
  result := fImage.currentSelection;
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

function TLayerAction.GetSelectedImageLayerOffset: TPoint;
begin
  result := FImage.LayerOffset[FImage.currentImageLayerIndex];
end;

function TLayerAction.GetSelectionTransform: TAffineMatrix;
begin
  result := FImage.SelectionTransform;
end;

procedure TLayerAction.SetSelectionTransform(AValue: TAffineMatrix);
begin
  FImage.SelectionTransform := AValue;
end;

procedure TLayerAction.Cancel;
begin
  if FDone then raise Exception.Create('Already done');
  RestoreSelectedLayer;
  RestoreSelectionLayer;
  RestoreSelection;
  if Assigned(FPrediff) then
  begin
    FImage.ChangeState(FPrediff,true);
    FreeAndNil(FPrediff);
  end;
  FDone := true;
end;

procedure TLayerAction.NeedSelectionBackup;
begin
  if not FBackupSelectionDefined then
  begin
    FBackupSelection := DuplicateBitmap(FImage.currentSelection);
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

constructor TLayerAction.Create(AImage: TLazPaintImage; AApplyOfsBefore: boolean = false);
begin
  if AImage <> nil then
  begin
    if not AImage.CheckNoAction(True) then
      raise exception.Create(rsConflictingActions);
  end;
  FImage := AImage;
  FImage.ActionInProgress := self;
  FBackupSelectedLayer := nil;
  FBackupSelection := nil;
  FBackupSelectionLayer := nil;
  FBackupSelectedLayerDefined := false;
  FBackupSelectionDefined := false;
  FBackupSelectionLayerDefined := false;
  FSelectedLayerChangedArea := EmptyRect;
  FSelectionLayerChangedArea := EmptyRect;
  FSelectionChangedArea := EmptyRect;
  FDone := false;
  if AApplyOfsBefore then
  begin
    with AImage.LayerOffset[AImage.currentImageLayerIndex] do
      FPrediff := AImage.ComputeLayerOffsetDifference(X,Y);
    if FPrediff.IsIdentity then FreeAndNil(FPrediff)
    else FImage.ChangeState(FPreDiff);
  end else
    FPrediff := nil;
end;

destructor TLayerAction.Destroy;
begin
  if not FDone then Cancel;
  FBackupSelectedLayer.Free;
  FBackupSelection.Free;
  FBackupSelectionLayer.Free;
  if FImage <> nil then
  begin
    if FImage.ActionInProgress = self then
      FImage.ActionInProgress := nil;
  end;
  inherited Destroy;
end;

procedure TLayerAction.ReplaceDrawingLayer(bmp: TBGRABitmap; AOwned: boolean);
begin
  FImage.ReplaceDrawingLayer(bmp,AOwned);
end;

procedure TLayerAction.ReplaceCurrentSelection(AValue: TBGRABitmap);
begin
  NeedSelectionBackup;
  if (AValue.Width = FImage.CurrentSelection.Width) and
    (AValue.Height = FImage.CurrentSelection.Height) then
    NotifyChange(FImage.CurrentSelection, AValue.GetDifferenceBounds(FImage.CurrentSelection))
  else
  begin
    NotifyChange(FImage.CurrentSelection, rect(0,0,FImage.CurrentSelection.Width,FImage.CurrentSelection.Height));
    NotifyChange(AValue, rect(0,0,AValue.Width,AValue.Height));
  end;
  FImage.ReplaceCurrentSelection(AValue);
end;

procedure TLayerAction.NotifyChange(ADest: TBGRABitmap; ARect: TRect);
begin
  if ADest = nil then exit;
  if not IntersectRect(ARect, ARect, rect(0,0,FImage.Width,FImage.Height)) then exit;
  if ADest = FImage.CurrentSelection then
    FSelectionChangedArea := RectUnion(FSelectionChangedArea, ARect)
  else if ADest = FImage.GetSelectedImageLayer then
    FSelectedLayerChangedArea := RectUnion(FSelectedLayerChangedArea, ARect)
  else if ADest = FImage.GetSelectionLayerIfExists then
    FSelectionLayerChangedArea := RectUnion(FSelectionLayerChangedArea, ARect);
end;

procedure TLayerAction.RestoreSelection;
var prevClip: TRect;
begin
  if FBackupSelectionDefined then
  begin
    if not AllChangesNotified then FSelectionChangedArea := rect(0,0,FImage.Width,FImage.Height);
    if IsRectEmpty(FSelectionChangedArea) then exit;
    prevClip := FImage.CurrentSelection.ClipRect;
    FImage.CurrentSelection.ClipRect := FSelectionChangedArea;
    if Assigned(FBackupSelection) then
      FImage.CurrentSelection.PutImage(0,0,FBackupSelection,dmSet)
    else
      FImage.CurrentSelection.FillRect(0,0,FImage.Width,FImage.Height,BGRABlack,dmSet);
    FImage.CurrentSelection.ClipRect := prevClip;
    FImage.SelectionMayChange(FSelectionChangedArea);
    FSelectionChangedArea := EmptyRect;
  end;
end;

procedure TLayerAction.RestoreDrawingLayer;
begin
  if FImage.SelectionEmpty then RestoreSelectedLayer
    else RestoreSelectionLayer;
end;

procedure TLayerAction.RestoreSelectedLayer;
var prevClip: TRect;
begin
  if FBackupSelectedLayerDefined then
  begin
    if not AllChangesNotified then FSelectedLayerChangedArea := rect(0,0,FImage.Width,FImage.Height);
    if IsRectEmpty(FSelectedLayerChangedArea) then exit;
    prevClip := FImage.GetSelectedImageLayer.ClipRect;
    FImage.GetSelectedImageLayer.ClipRect := FSelectedLayerChangedArea;
    if Assigned(FBackupSelectedLayer) then
      FImage.GetSelectedImageLayer.PutImage(0,0,FBackupSelectedLayer,dmSet)
    else
      FImage.GetSelectedImageLayer.FillRect(0,0,FImage.Width,FImage.Height,BGRAPixelTransparent,dmSet);
    FImage.GetSelectedImageLayer.ClipRect := prevClip;
    FImage.LayerMayChange(FImage.GetSelectedImageLayer,FSelectedLayerChangedArea);
    FSelectedLayerChangedArea := EmptyRect;
  end;
end;

procedure TLayerAction.RestoreSelectionLayer;
var prevClip: TRect;
begin
  if FBackupSelectionLayerDefined and (GetSelectionLayerIfExists <> nil) then
  begin
    if not AllChangesNotified then FSelectionLayerChangedArea := rect(0,0,FImage.Width,FImage.Height);
    if IsRectEmpty(FSelectionLayerChangedArea) then exit;
    prevClip := FImage.GetSelectionLayerIfExists.ClipRect;
    FImage.GetSelectionLayerIfExists.ClipRect := FSelectionLayerChangedArea;
    if Assigned(FBackupSelectionLayer) then
      FImage.GetSelectionLayerIfExists.PutImage(0,0,FBackupSelectionLayer,dmSet)
    else
      FImage.GetSelectionLayerIfExists.FillRect(0,0,FImage.Width,FImage.Height,BGRAPixelTransparent,dmSet);
    FImage.GetSelectionLayerIfExists.ClipRect := prevClip;
    FImage.LayerMayChange(FImage.GetSelectionLayerIfExists,FSelectionLayerChangedArea);
    FSelectionLayerChangedArea := EmptyRect;
  end;
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
var bounds: TRect;
begin
  if not FImage.SelectionEmpty then
  begin
    bounds := FImage.SelectionBounds;
    NeedSelectionBackup;
    NotifyChange(FImage.CurrentSelection, bounds);
    if FImage.GetSelectionLayerIfExists <> nil then
    begin
      NeedSelectedLayerBackup;
      NotifyChange(FImage.GetSelectedImageLayer, bounds);
      NeedSelectionLayerBackup;
      NotifyChange(FImage.GetSelectionLayerIfExists, bounds);
    end;
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

function TLayerAction.RetrieveSelectionIfLayerEmpty(removeFromBitmap: boolean
  ): boolean;
begin
  NeedSelectedLayerBackup;
  NeedSelectionLayerBackup;
  result := FImage.RetrieveSelectionIfLayerEmpty(removeFromBitmap);
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
var dest:TBGRABitmap;
begin
  NeedSelectionLayerBackup;
  dest := GetSelectionLayerIfExists;
  if (dest <> nil) and (bmp <> nil) and (bmp.Width = dest.Width) and
    (bmp.Height = dest.Height) then
    NotifyChange(dest, bmp.GetDifferenceBounds(dest))
  else
  begin
    if dest <> nil then NotifyChange(dest, rect(0,0,dest.Width,dest.Height));
    if bmp <> nil then NotifyChange(bmp, rect(0,0,bmp.Width,bmp.Height));
  end;
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
var dest: TBGRABitmap;
begin
  NeedSelectedLayerBackup;
  dest := GetSelectedImageLayer;
  if (dest <> nil) and (AValue.Width = dest.Width) and
    (AValue.Height = dest.Height) then
    NotifyChange(dest, AValue.GetDifferenceBounds(dest))
  else
  begin
    if dest <> nil then NotifyChange(dest, rect(0,0,dest.Width,dest.Height));
    NotifyChange(AValue, rect(0,0,AValue.Width,AValue.Height));
  end;
  FImage.ReplaceSelectedLayer(AValue, AOwned);
end;

procedure TLayerAction.Validate;
begin
  if FDone then raise Exception.Create('Already done');
  PartialValidate(True);
  FDone := true;
end;

procedure TLayerAction.PartialCancel;
begin
  RestoreSelectedLayer;
  RestoreSelectionLayer;
  RestoreSelection;
end;

procedure TLayerAction.PartialValidate(ADiscardBackup: boolean = false);
var prevLayerOriginal: TBGRALayerCustomOriginal;
  prevLayerOriginalMatrix: TAffineMatrix;
  prevLayerOriginaData: TStream;
  imgDiff: TImageLayerStateDifference;
  composedDiff: TComposedImageDifference;
  ofs: TPoint;
  applyOfs: TCustomImageDifference;
  appendOfs: boolean;
begin
  if FBackupSelectedLayerDefined or FBackupSelectionDefined or FBackupSelectionLayerDefined then
  begin
    if AllChangesNotified then
      if IsRectEmpty(FSelectedLayerChangedArea) and IsRectEmpty(FSelectionChangedArea) and
         IsRectEmpty(FSelectionLayerChangedArea) then exit;
    if FBackupSelectionLayerDefined then
    begin
      FImage.DiscardSelectionLayerBounds;
      if FImage.SelectionLayerIsEmpty then
        FImage.ReplaceSelectionLayer(nil,True);
    end;
    if FBackupSelectionDefined then
    begin
      FImage.DiscardSelectionBounds;
      if FImage.SelectionEmpty then
      begin
        FImage.ReplaceSelectionLayer(nil,true);
        FImage.ReplaceCurrentSelection(nil);
      end;
    end;
    //original will be backed up if there are changes in the raster image of the selected layer
    prevLayerOriginal:= FImage.LayerOriginal[FImage.currentImageLayerIndex];
    if Assigned(prevLayerOriginal) and (FBackupSelectedLayerDefined or not IsRectEmpty(FSelectedLayerChangedArea)) then
    begin
      prevLayerOriginaData:= TMemoryStream.Create;
      prevLayerOriginal.SaveToStream(prevLayerOriginaData);
      prevLayerOriginalMatrix:= FImage.LayerOriginalMatrix[FImage.currentImageLayerIndex];
      FImage.DiscardOriginal(false);
    end else
    begin
      prevLayerOriginaData := nil;
      prevLayerOriginalMatrix:= AffineMatrixIdentity;
    end;

    if AllChangesNotified then
      imgDiff := FImage.ComputeLayerDifference(FBackupSelectedLayer, FSelectedLayerChangedArea,
        FBackupSelection, FSelectionChangedArea,
        FBackupSelectionLayer, FSelectionLayerChangedArea,
        prevLayerOriginaData, prevLayerOriginalMatrix) as TImageLayerStateDifference
    else
      imgDiff := FImage.ComputeLayerDifference(FBackupSelectedLayer, FBackupSelectedLayerDefined,
        FBackupSelection, FBackupSelectionDefined,
        FBackupSelectionLayer, FBackupSelectionLayerDefined,
        prevLayerOriginaData, prevLayerOriginalMatrix) as TImageLayerStateDifference;

    if ADiscardBackup then
    begin
      FreeAndNil(FBackupSelectionLayer);
      FreeAndNil(FBackupSelectedLayer);
      FreeAndNil(FBackupSelection);
      FBackupSelectedLayerDefined := false;
      FBackupSelectedLayerDefined := false;
      FBackupSelectionDefined := false;

      appendOfs:= Assigned(imgDiff) and imgDiff.ChangeImageLayer;
    end else
    begin
      if FBackupSelectionLayerDefined then
      begin
        if (FBackupSelectionLayer = nil) and (FImage.GetSelectionLayerIfExists <> nil) then
        begin
          if not FImage.SelectionLayerIsEmpty then
            FBackupSelectionLayer := Fimage.GetSelectionLayerIfExists.Duplicate as TBGRABitmap;
        end else
        if Assigned(FBackupSelectionLayer) then
        begin
          if AllChangesNotified then FBackupSelectionLayer.ClipRect := FSelectionLayerChangedArea;
          if Assigned(FImage.GetSelectionLayerIfExists) then
            FBackupSelectionLayer.PutImage(0,0,FImage.GetSelectionLayerIfExists,dmSet)
          else
            FBackupSelectionLayer.FillRect(0,0,FImage.Width,FImage.Height,BGRAPixelTransparent,dmSet);
          FBackupSelectionLayer.NoClip;
        end;
        FSelectionLayerChangedArea := EmptyRect;
      end;
      if FBackupSelectedLayerDefined then
      begin
        if AllChangesNotified then FBackupSelectedLayer.ClipRect := FSelectedLayerChangedArea;
        if Assigned(FImage.GetSelectedImageLayer) then
          FBackupSelectedLayer.PutImage(0,0,FImage.GetSelectedImageLayer,dmSet)
        else
          FBackupSelectedLayer.FillRect(0,0,FImage.Width,FImage.Height,BGRAPixelTransparent,dmSet);
        FBackupSelectedLayer.NoClip;
        FSelectedLayerChangedArea := EmptyRect;
      end;
      if FBackupSelectionDefined then
      begin
        if (FBackupSelection = nil) and (FImage.CurrentSelection <> nil) then
        begin
          if not FImage.SelectionEmpty then
            FBackupSelection := Fimage.CurrentSelection.Duplicate as TBGRABitmap;
        end else
        if (FBackupSelection <> nil) then
        begin
          if AllChangesNotified then FBackupSelection.ClipRect := FSelectionChangedArea;
          if Assigned(FImage.CurrentSelection) then
            FBackupSelection.PutImage(0,0,FImage.CurrentSelection,dmSet)
          else
            FBackupSelection.FillRect(0,0,FImage.Width,FImage.Height,BGRABlack,dmSet);
          FBackupSelection.NoClip;
        end;
        FSelectionChangedArea := EmptyRect;
      end;

      appendOfs := false;
    end;

    if appendOfs or Assigned(FPrediff) then
    begin
      composedDiff := TComposedImageDifference.Create;
      if Assigned(FPrediff) then
      begin
        composedDiff.Add(FPrediff);
        FPrediff := nil;
      end;
      composedDiff.Add(imgDiff);
      if appendOfs then
      begin
        ofs := FImage.LayerOffset[FImage.currentImageLayerIndex];
        applyOfs:= FImage.ComputeLayerOffsetDifference(ofs.x, ofs.y);
        if not applyOfs.IsIdentity then
        begin
          composedDiff.Add(applyOfs);
          FImage.ChangeState(applyOfs);
        end else
          applyOfs.Free;
      end;
      FImage.AddUndo(composedDiff);
    end else
      FImage.AddUndo(imgDiff);

    FImage.OnImageChanged.NotifyObservers;
  end else
  begin
    if Assigned(FPrediff) then
    begin
      FImage.AddUndo(FPrediff);
      FPrediff := nil;
    end;
  end;
end;


end.

