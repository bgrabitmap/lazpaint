unit ULayerAction;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UImage, BGRABitmap, BGRABitmapTypes, UImageType,
  UStateType, UImageState;

type
  { TLayerAction }

  TLayerAction = class(TCustomLayerAction)
  private
    FAllChangesNotified: boolean;
    FImage: TLazPaintImage;
    FPrediff: TCustomImageDifference;
    FBackupSelectedLayer, FBackupSelectionLayer, FBackupSelection: TBGRABitmap;
    FBackupSelectedLayerDefined, FBackupSelectionLayerDefined, FBackupSelectionDefined: boolean;
    FSelectedImageLayerChangedArea, FSelectionLayerChangedArea, FSelectionMaskChangedArea: TRect;
    FDone: boolean;
    FOnTryStop: TOnTryStopEventHandler;
    function GetBackupDrawingLayer: TBGRABitmap;
    function GetBackupSelectedLayer: TBGRABitmap;
    function GetBackupSelection: TBGRABitmap;
    function GetBackupSelectionLayer: TBGRABitmap;
    function GetCurrentSelection: TBGRABitmap;
    function GetCurrentState: TImageState;
    function GetSelectedImageLayer: TBGRABitmap;
    function GetDrawingLayer: TBGRABitmap;
    function GetSelectedImageLayerOffset: TPoint;
  protected
    procedure Cancel;
    procedure NeedSelectionMaskBackup;
    procedure NeedSelectedLayerBackup;
    procedure NeedSelectionLayerBackup;
    property CurrentState: TImageState read GetCurrentState;
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
    procedure RestoreSelectionMask;
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
    property Done: boolean read FDone;
    property AllChangesNotified: boolean read FAllChangesNotified write FAllChangesNotified;
  end;

implementation

uses UResourceStrings, UGraph, Types, Dialogs, BGRATransform, BGRALayerOriginal, UImageDiff;

{ TLayerAction }

function TLayerAction.GetSelectedImageLayer: TBGRABitmap;
begin
  result := CurrentState.SelectedImageLayer;
  if not Assigned(result) then
    raise exception.Create('No image layer selected');
  NeedSelectedLayerBackup;
end;

function TLayerAction.GetCurrentSelection: TBGRABitmap;
begin
  NeedSelectionMaskBackup;
  result := CurrentState.SelectionMask;
end;

function TLayerAction.GetCurrentState: TImageState;
begin
  result := FImage.CurrentState;
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
  NeedSelectionMaskBackup;
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
  result := CurrentState.LayerOffset[CurrentState.SelectedImageLayerIndex];
end;

procedure TLayerAction.Cancel;
begin
  if FDone then raise Exception.Create('Already done');
  RestoreSelectedLayer;
  RestoreSelectionLayer;
  RestoreSelectionMask;
  if Assigned(FPrediff) then
  begin
    FPrediff.UnapplyTo(CurrentState);
    FreeAndNil(FPrediff);
  end;
  FDone := true;
end;

procedure TLayerAction.NeedSelectionMaskBackup;
begin
  if not FBackupSelectionDefined then
  begin
    FBackupSelection := DuplicateBitmap(CurrentState.SelectionMask);
    FBackupSelectionDefined := true;
  end;
end;

procedure TLayerAction.NeedSelectedLayerBackup;
begin
  if not FBackupSelectedLayerDefined then
  begin
    FBackupSelectedLayer := DuplicateBitmap(CurrentState.SelectedImageLayer);
    FBackupSelectedLayerDefined := true;
  end;
end;

procedure TLayerAction.NeedSelectionLayerBackup;
begin
  if not FBackupSelectionLayerDefined then
  begin
    FBackupSelectionLayer := DuplicateBitmap(CurrentState.SelectionLayer);
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
  FSelectedImageLayerChangedArea := EmptyRect;
  FSelectionLayerChangedArea := EmptyRect;
  FSelectionMaskChangedArea := EmptyRect;
  FDone := false;
  if AApplyOfsBefore then
  begin
    with AImage.LayerOffset[AImage.currentImageLayerIndex] do
      FPrediff := CurrentState.ComputeLayerOffsetDifference(X,Y);
    if FPrediff.IsIdentity then FreeAndNil(FPrediff)
    else FPreDiff.ApplyTo(CurrentState);
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
  if AValue = CurrentState.SelectionMask then exit;
  NeedSelectionMaskBackup;
  if Assigned(AValue) and Assigned(CurrentState.SelectionMask) and
    (AValue.Width = CurrentState.SelectionMask.Width) and
    (AValue.Height = CurrentState.SelectionMask.Height) then
    NotifyChange(CurrentState.SelectionMask, AValue.GetDifferenceBounds(CurrentState.SelectionMask))
  else
  begin
    if Assigned(CurrentState.SelectionMask) then
      NotifyChange(CurrentState.SelectionMask, rect(0,0,CurrentState.SelectionMask.Width,CurrentState.SelectionMask.Height));
    if Assigned(AValue) then
      NotifyChange(AValue, rect(0,0,AValue.Width,AValue.Height));
  end;
  CurrentState.SelectionMask.Free;
  CurrentState.SelectionMask := AValue
end;

procedure TLayerAction.NotifyChange(ADest: TBGRABitmap; ARect: TRect);
begin
  if ADest = nil then exit;
  if not IntersectRect(ARect, ARect, rect(0,0,CurrentState.Width,CurrentState.Height)) then exit;
  if ADest = CurrentState.SelectionMask then
    FSelectionMaskChangedArea := RectUnion(FSelectionMaskChangedArea, ARect)
  else if ADest = CurrentState.SelectedImageLayer then
    FSelectedImageLayerChangedArea := RectUnion(FSelectedImageLayerChangedArea, ARect)
  else if ADest = CurrentState.SelectionLayer then
    FSelectionLayerChangedArea := RectUnion(FSelectionLayerChangedArea, ARect);
end;

procedure TLayerAction.RestoreSelectionMask;
var prevClip: TRect;
begin
  if FBackupSelectionDefined then
  begin
    if not AllChangesNotified then FSelectionMaskChangedArea := rect(0,0,CurrentState.Width,CurrentState.Height);
    if IsRectEmpty(FSelectionMaskChangedArea) then exit;
    prevClip := CurrentState.SelectionMask.ClipRect;
    CurrentState.SelectionMask.ClipRect := FSelectionMaskChangedArea;
    if Assigned(FBackupSelection) then
      CurrentState.SelectionMask.PutImage(0,0,FBackupSelection,dmSet)
    else
      CurrentState.SelectionMask.FillRect(0,0,CurrentState.Width,CurrentState.Height,BGRABlack,dmSet);
    CurrentState.SelectionMask.ClipRect := prevClip;
    FImage.SelectionMayChange(FSelectionMaskChangedArea);
    FSelectionMaskChangedArea := EmptyRect;
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
    if not AllChangesNotified then FSelectedImageLayerChangedArea := rect(0,0,CurrentState.Width,CurrentState.Height);
    if IsRectEmpty(FSelectedImageLayerChangedArea) then exit;
    prevClip := CurrentState.SelectedImageLayer.ClipRect;
    CurrentState.SelectedImageLayer.ClipRect := FSelectedImageLayerChangedArea;
    if Assigned(FBackupSelectedLayer) then
      CurrentState.SelectedImageLayer.PutImage(0,0,FBackupSelectedLayer,dmSet)
    else
      CurrentState.SelectedImageLayer.FillRect(0,0,CurrentState.Width,CurrentState.Height,BGRAPixelTransparent,dmSet);
    CurrentState.SelectedImageLayer.ClipRect := prevClip;
    FImage.LayerMayChange(CurrentState.SelectedImageLayer,FSelectedImageLayerChangedArea);
    FSelectedImageLayerChangedArea := EmptyRect;
  end;
end;

procedure TLayerAction.RestoreSelectionLayer;
var prevClip: TRect;
begin
  if FBackupSelectionLayerDefined and (CurrentState.SelectionLayer <> nil) then
  begin
    if not AllChangesNotified then FSelectionLayerChangedArea := rect(0,0,CurrentState.Width,CurrentState.Height);
    if IsRectEmpty(FSelectionLayerChangedArea) then exit;
    prevClip := CurrentState.SelectionLayer.ClipRect;
    CurrentState.SelectionLayer.ClipRect := FSelectionLayerChangedArea;
    if Assigned(FBackupSelectionLayer) then
      CurrentState.SelectionLayer.PutImage(0,0,FBackupSelectionLayer,dmSet)
    else
      CurrentState.SelectionLayer.FillRect(0,0,CurrentState.Width,CurrentState.Height,BGRAPixelTransparent,dmSet);
    CurrentState.SelectionLayer.ClipRect := prevClip;
    FImage.LayerMayChange(CurrentState.SelectionLayer,FSelectionLayerChangedArea);
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
  NeedSelectionMaskBackup;
  FImage.QuerySelection;
end;

procedure TLayerAction.RemoveSelection;
begin
  if not FImage.SelectionEmpty or (FImage.GetSelectionLayerIfExists <> nil) then
  begin
    NeedSelectionMaskBackup;
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
    bounds := FImage.SelectionMaskBounds;
    NeedSelectionMaskBackup;
    NotifyChange(CurrentState.SelectionMask, bounds);
    if CurrentState.SelectionLayer <> nil then
    begin
      NeedSelectedLayerBackup;
      NotifyChange(CurrentState.SelectedImageLayer, bounds);
      NeedSelectionLayerBackup;
      NotifyChange(CurrentState.SelectionLayer, bounds);
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
  result := CurrentState.SelectionLayer;
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
  RestoreSelectionMask;
end;

procedure TLayerAction.PartialValidate(ADiscardBackup: boolean = false);
var
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
      if IsRectEmpty(FSelectedImageLayerChangedArea) and IsRectEmpty(FSelectionMaskChangedArea) and
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
    if CurrentState.LayerOriginalDefined[CurrentState.SelectedImageLayerIndex] and
       (FBackupSelectedLayerDefined or not IsRectEmpty(FSelectedImageLayerChangedArea)) then
    begin
      prevLayerOriginaData:= TMemoryStream.Create;
      CurrentState.SaveOriginalToStream(prevLayerOriginaData);
      prevLayerOriginalMatrix:= CurrentState.LayerOriginalMatrix[CurrentState.SelectedImageLayerIndex];
      CurrentState.DiscardOriginal(false);
    end else
    begin
      prevLayerOriginaData := nil;
      prevLayerOriginalMatrix:= AffineMatrixIdentity;
    end;

    if AllChangesNotified then
      imgDiff := CurrentState.ComputeLayerDifference(FBackupSelectedLayer, FSelectedImageLayerChangedArea,
        FBackupSelection, FSelectionMaskChangedArea,
        FBackupSelectionLayer, FSelectionLayerChangedArea,
        prevLayerOriginaData, prevLayerOriginalMatrix) as TImageLayerStateDifference
    else
      imgDiff := CurrentState.ComputeLayerDifference(FBackupSelectedLayer, FBackupSelectedLayerDefined,
        FBackupSelection, FBackupSelectionDefined,
        FBackupSelectionLayer, FBackupSelectionLayerDefined,
        prevLayerOriginaData, prevLayerOriginalMatrix) as TImageLayerStateDifference;
    if imgDiff.IsIdentity then FreeAndNil(imgDiff);

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
        if (FBackupSelectionLayer = nil) and (CurrentState.SelectionLayer <> nil) then
        begin
          if not FImage.SelectionLayerIsEmpty then
            FBackupSelectionLayer := CurrentState.SelectionLayer.Duplicate as TBGRABitmap;
        end else
        if Assigned(FBackupSelectionLayer) then
        begin
          if AllChangesNotified then FBackupSelectionLayer.ClipRect := FSelectionLayerChangedArea;
          if Assigned(CurrentState.SelectionLayer) then
            FBackupSelectionLayer.PutImage(0,0,CurrentState.SelectionLayer,dmSet)
          else
            FBackupSelectionLayer.FillRect(0,0,CurrentState.Width,CurrentState.Height,BGRAPixelTransparent,dmSet);
          FBackupSelectionLayer.NoClip;
        end;
        FSelectionLayerChangedArea := EmptyRect;
      end;
      if FBackupSelectedLayerDefined then
      begin
        if AllChangesNotified then FBackupSelectedLayer.ClipRect := FSelectedImageLayerChangedArea;
        if Assigned(CurrentState.SelectedImageLayer) then
          FBackupSelectedLayer.PutImage(0,0,CurrentState.SelectedImageLayer,dmSet)
        else
          FBackupSelectedLayer.FillRect(0,0,CurrentState.Width,CurrentState.Height,BGRAPixelTransparent,dmSet);
        FBackupSelectedLayer.NoClip;
        FSelectedImageLayerChangedArea := EmptyRect;
      end;
      if FBackupSelectionDefined then
      begin
        if (FBackupSelection = nil) and (CurrentState.SelectionMask <> nil) then
        begin
          if not FImage.SelectionEmpty then
            FBackupSelection := CurrentState.SelectionMask.Duplicate as TBGRABitmap;
        end else
        if (FBackupSelection <> nil) then
        begin
          if AllChangesNotified then FBackupSelection.ClipRect := FSelectionMaskChangedArea;
          if Assigned(CurrentState.SelectionMask) then
            FBackupSelection.PutImage(0,0,CurrentState.SelectionMask,dmSet)
          else
            FBackupSelection.FillRect(0,0,CurrentState.Width,CurrentState.Height,BGRABlack,dmSet);
          FBackupSelection.NoClip;
        end;
        FSelectionMaskChangedArea := EmptyRect;
      end;

      appendOfs := false;
    end;

    if assigned(imgDiff) then
    begin
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
          ofs := CurrentState.LayerOffset[CurrentState.SelectedImageLayerIndex];
          applyOfs:= CurrentState.ComputeLayerOffsetDifference(ofs.x, ofs.y);
          if not applyOfs.IsIdentity then
          begin
            composedDiff.Add(applyOfs);
            applyOfs.ApplyTo(CurrentState);
          end else
            applyOfs.Free;
        end;
        FImage.AddUndo(composedDiff);
      end else
        FImage.AddUndo(imgDiff);
    end;

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

