// SPDX-License-Identifier: GPL-3.0-only
unit ULayerAction;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes, UImageType,
  UStateType, UImageState;

type
  TNotifyChangeEvent = procedure(ASender: TObject; ALayer: TBGRABitmap; ARect: TRect) of object;
  TNotifyUndoEvent = procedure(ASender: TObject; AUndo: TCustomImageDifference; var Owned: boolean) of object;

  { TLayerAction }

  TLayerAction = class(TCustomLayerAction)
  private
    FChangeBoundsNotified: boolean;
    FImageState: TImageState;
    FOnDestroy: TNotifyEvent;
    FOnNotifyChange: TNotifyChangeEvent;
    FOnNotifyUndo: TNotifyUndoEvent;
    FPrediff: TComposedImageDifference;
    FBackupSelectedLayer, FBackupSelectionLayer, FBackupSelection: TBGRABitmap;
    FBackupSelectedLayerDefined, FBackupSelectionLayerDefined, FBackupSelectionMaskDefined: boolean;
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
    function GetSelectionLayerBounds: TRect;
    procedure SetOnDestroy(AValue: TNotifyEvent);
    procedure SetOnNotifyChange(AValue: TNotifyChangeEvent);
    procedure SetOnNotifyUndo(AValue: TNotifyUndoEvent);
  protected
    procedure Cancel;
    procedure NeedSelectionMaskBackup;
    procedure NeedSelectedLayerBackup;
    procedure NeedSelectionLayerBackup;
    property CurrentState: TImageState read GetCurrentState;
  public
    constructor Create(AState: TImageState; AApplyOfsBefore: boolean = false; AApplySelectionTransformBefore: boolean = false);
    procedure Validate;
    procedure PartialValidate(ADiscardBackup: boolean = false);
    procedure PartialCancel;
    destructor Destroy; override;

    procedure QuerySelection;
    procedure RemoveSelection;
    procedure EraseSelectionInBitmap;
    procedure MergeWithSelection(AApplyMask: boolean = true);
    procedure ReleaseSelection;
    procedure RetrieveSelection;
    function RetrieveSelectionIfLayerEmpty(removeFromBitmap: boolean = false): boolean;
    procedure ApplySelectionMask;

    procedure ReplaceSelectionLayer(bmp: TBGRABitmap; AOwned: boolean);
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
    property ChangeBoundsNotified: boolean read FChangeBoundsNotified write FChangeBoundsNotified;
    property SelectionLayerBounds: TRect read GetSelectionLayerBounds;
    property OnNotifyChange: TNotifyChangeEvent read FOnNotifyChange write SetOnNotifyChange;
    property OnNotifyUndo: TNotifyUndoEvent read FOnNotifyUndo write SetOnNotifyUndo;
    property OnDestroy: TNotifyEvent read FOnDestroy write SetOnDestroy;
    property Prediff: TComposedImageDifference read FPrediff;
  end;

implementation

uses UGraph, Types, Dialogs, BGRATransform, BGRALayerOriginal, UImageDiff;

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
  result := FImageState;
end;

function TLayerAction.GetBackupSelectedLayer: TBGRABitmap;
begin
  NeedSelectedLayerBackup;
  result := FBackupSelectedLayer;
end;

function TLayerAction.GetBackupDrawingLayer: TBGRABitmap;
begin
  if CurrentState.SelectionMaskEmpty then result := BackupSelectedLayer else
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
  if CurrentState.SelectionMaskEmpty then result := GetSelectedImageLayer else
    result := GetOrCreateSelectionLayer;
end;

function TLayerAction.GetSelectedImageLayerOffset: TPoint;
begin
  result := CurrentState.LayerOffset[CurrentState.SelectedImageLayerIndex];
end;

function TLayerAction.GetSelectionLayerBounds: TRect;
begin
  result := CurrentState.GetSelectionLayerBounds;
end;

procedure TLayerAction.SetOnDestroy(AValue: TNotifyEvent);
begin
  if FOnDestroy=AValue then Exit;
  FOnDestroy:=AValue;
end;

procedure TLayerAction.SetOnNotifyChange(AValue: TNotifyChangeEvent);
begin
  if FOnNotifyChange=AValue then Exit;
  FOnNotifyChange:=AValue;
end;

procedure TLayerAction.SetOnNotifyUndo(AValue: TNotifyUndoEvent);
begin
  if FOnNotifyUndo=AValue then Exit;
  FOnNotifyUndo:=AValue;
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
    if (FPrediff.Kind in [idkChangeImageAndSelection,idkChangeSelection]) and
       Assigned(FImageState.SelectionMask) then
      NotifyChange(FImageState.SelectionMask, rect(0,0,FImageState.SelectionMask.Width,FImageState.SelectionMask.Height));
    FreeAndNil(FPrediff);
  end;
  FDone := true;
end;

procedure TLayerAction.NeedSelectionMaskBackup;
begin
  if not FBackupSelectionMaskDefined then
  begin
    FBackupSelection := DuplicateBitmap(CurrentState.SelectionMask);
    FBackupSelectionMaskDefined := true;
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

constructor TLayerAction.Create(AState: TImageState; AApplyOfsBefore: boolean;
  AApplySelectionTransformBefore: boolean);
var
  layerOfsDiff,selTransfDiff: TCustomImageDifference;
begin
  FImageState := AState;
  FBackupSelectedLayer := nil;
  FBackupSelection := nil;
  FBackupSelectionLayer := nil;
  FBackupSelectedLayerDefined := false;
  FBackupSelectionMaskDefined := false;
  FBackupSelectionLayerDefined := false;
  FSelectedImageLayerChangedArea := EmptyRect;
  FSelectionLayerChangedArea := EmptyRect;
  FSelectionMaskChangedArea := EmptyRect;
  FDone := false;
  FPrediff := TComposedImageDifference.Create;
  if AApplyOfsBefore then
  begin
    with CurrentState.LayerOffset[CurrentState.SelectedImageLayerIndex] do
      layerOfsDiff := CurrentState.ComputeLayerOffsetDifference(X,Y);
    if layerOfsDiff.IsIdentity then FreeAndNil(layerOfsDiff)
    else
    begin
      layerOfsDiff.ApplyTo(CurrentState);
      FPrediff.Add(layerOfsDiff);
    end;
  end;
  if AApplySelectionTransformBefore and not IsAffineMatrixIdentity(CurrentState.SelectionTransform) then
  begin
    selTransfDiff := CurrentState.ComputeSelectionTransformDifference;
    if selTransfDiff.IsIdentity then FreeAndNil(selTransfDiff)
    else
    begin
      selTransfDiff.ApplyTo(CurrentState);
      FPrediff.Add(selTransfDiff);
    end;
  end;
  if FPrediff.Count = 0 then FreeAndNil(FPrediff);
end;

destructor TLayerAction.Destroy;
begin
  if not FDone then Cancel;
  FPrediff.Free;
  FBackupSelectedLayer.Free;
  FBackupSelection.Free;
  FBackupSelectionLayer.Free;
  if Assigned(FOnDestroy) then FOnDestroy(self);
  inherited Destroy;
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
  if Assigned(FOnNotifyChange) then
    FOnNotifyChange(self, ADest, ARect);
end;

procedure TLayerAction.RestoreSelectionMask;
var prevClip: TRect;
begin
  if FBackupSelectionMaskDefined then
  begin
    if not ChangeBoundsNotified then FSelectionMaskChangedArea := rect(0,0,CurrentState.Width,CurrentState.Height);
    if IsRectEmpty(FSelectionMaskChangedArea) then exit;
    prevClip := CurrentState.SelectionMask.ClipRect;
    CurrentState.SelectionMask.ClipRect := FSelectionMaskChangedArea;
    if Assigned(FBackupSelection) then
      CurrentState.SelectionMask.PutImage(0,0,FBackupSelection,dmSet)
    else
      CurrentState.SelectionMask.FillRect(0,0,CurrentState.Width,CurrentState.Height,BGRABlack,dmSet);
    CurrentState.SelectionMask.ClipRect := prevClip;
    If Assigned(FOnNotifyChange) then
      FOnNotifyChange(self, CurrentState.SelectionMask, FSelectionMaskChangedArea);
    FSelectionMaskChangedArea := EmptyRect;
  end;
end;

procedure TLayerAction.RestoreDrawingLayer;
begin
  if CurrentState.SelectionMaskEmpty then RestoreSelectedLayer
    else RestoreSelectionLayer;
end;

procedure TLayerAction.RestoreSelectedLayer;
var prevClip: TRect;
begin
  if FBackupSelectedLayerDefined then
  begin
    if not ChangeBoundsNotified then FSelectedImageLayerChangedArea := rect(0,0,CurrentState.Width,CurrentState.Height);
    if IsRectEmpty(FSelectedImageLayerChangedArea) then exit;
    prevClip := CurrentState.SelectedImageLayer.ClipRect;
    CurrentState.SelectedImageLayer.ClipRect := FSelectedImageLayerChangedArea;
    if Assigned(FBackupSelectedLayer) then
      CurrentState.SelectedImageLayer.PutImage(0,0,FBackupSelectedLayer,dmSet)
    else
      CurrentState.SelectedImageLayer.FillRect(0,0,CurrentState.Width,CurrentState.Height,BGRAPixelTransparent,dmSet);
    CurrentState.SelectedImageLayer.ClipRect := prevClip;
    If Assigned(FOnNotifyChange) then
      FOnNotifyChange(self, CurrentState.SelectedImageLayer, FSelectedImageLayerChangedArea);
    FSelectedImageLayerChangedArea := EmptyRect;
  end;
end;

procedure TLayerAction.RestoreSelectionLayer;
var prevClip: TRect;
begin
  if FBackupSelectionLayerDefined and (CurrentState.SelectionLayer <> nil) then
  begin
    if not ChangeBoundsNotified then FSelectionLayerChangedArea := rect(0,0,CurrentState.Width,CurrentState.Height);
    if IsRectEmpty(FSelectionLayerChangedArea) then exit;
    prevClip := CurrentState.SelectionLayer.ClipRect;
    CurrentState.SelectionLayer.ClipRect := FSelectionLayerChangedArea;
    if Assigned(FBackupSelectionLayer) then
      CurrentState.SelectionLayer.PutImage(0,0,FBackupSelectionLayer,dmSet)
    else
      CurrentState.SelectionLayer.FillRect(0,0,CurrentState.Width,CurrentState.Height,BGRAPixelTransparent,dmSet);
    CurrentState.SelectionLayer.ClipRect := prevClip;
    If Assigned(FOnNotifyChange) then
      FOnNotifyChange(self, CurrentState.SelectionLayer, FSelectionLayerChangedArea);
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
  CurrentState.QuerySelectionMask;
end;

procedure TLayerAction.RemoveSelection;
var bounds: TRect;
begin
  if not CurrentState.SelectionMaskEmpty or (CurrentState.SelectionLayer <> nil) then
  begin
    NeedSelectionMaskBackup;
    NeedSelectionLayerBackup;
    bounds := CurrentState.GetTransformedSelectionMaskBounds;
    NotifyChange(CurrentState.SelectionLayer, bounds);
    CurrentState.RemoveSelection;
  end;
end;

procedure TLayerAction.EraseSelectionInBitmap;
var offs: TPoint;
  r: TRect;
begin
  if not CurrentState.SelectionMaskEmpty then
  begin
    NeedSelectedLayerBackup;
    offs := CurrentState.LayerOffset[CurrentState.SelectedImageLayerIndex];
    r := CurrentState.GetSelectionMaskBounds;
    SubstractMask(GetSelectedImageLayer,-offs.X+r.left,-offs.Y+r.top,CurrentState.SelectionMask,r);
    OffsetRect(r,-offs.x,-offs.y);
    NotifyChange(GetSelectedImageLayer,r);
  end;
end;

procedure TLayerAction.MergeWithSelection(AApplyMask: boolean);
var offs: TPoint;
  sourceRect,destRect: TRect;
begin
  if not IsAffineMatrixIdentity(CurrentState.SelectionTransform) then raise exception.Create('Unexpected selection transform');
  if not CurrentState.SelectionLayerEmpty and not (AApplyMask and CurrentState.SelectionMaskEmpty) then
  begin
    sourceRect := CurrentState.GetSelectionLayerBounds;
    if AApplyMask then
    begin
      CurrentState.SelectionLayer.ApplyMask(CurrentState.SelectionMask,CurrentState.GetSelectionLayerBounds);
      IntersectRect(sourceRect,sourceRect,CurrentState.GetSelectionMaskBounds);
      NotifyChange(CurrentState.SelectionLayer,CurrentState.GetSelectionLayerBounds);
    end;
    offs := CurrentState.LayerOffset[CurrentState.SelectedImageLayerIndex];
    destRect := sourceRect;
    OffsetRect(destRect, -offs.x,-offs.y);
    GetSelectedImageLayer.PutImagePart(destRect.left,destRect.top,CurrentState.SelectionLayer,sourceRect,dmDrawWithTransparency);
    NotifyChange(GetSelectedImageLayer,destRect);
    CurrentState.ReplaceSelectionLayer(nil,true);
  end;
end;

procedure TLayerAction.ReleaseSelection;
var bounds: TRect;
begin
  if not IsAffineMatrixIdentity(CurrentState.SelectionTransform) then raise exception.Create('Unexpected selection transform');
  if not CurrentState.SelectionMaskEmpty then
  begin
    bounds := CurrentState.GetSelectionMaskBounds;
    NeedSelectionMaskBackup;
    NotifyChange(CurrentState.SelectionMask, bounds);
    if CurrentState.SelectionLayer <> nil then
    begin
      NeedSelectedLayerBackup;
      NotifyChange(CurrentState.SelectedImageLayer, bounds);
      NeedSelectionLayerBackup;
      NotifyChange(CurrentState.SelectionLayer, bounds);
    end;

    ApplySelectionMask;
    CurrentState.SelectionMask.Free;
    CurrentState.SelectionMask := nil;
    MergeWithSelection(False);
  end;
end;

procedure TLayerAction.RetrieveSelection;
var temp : TBGRABitmap;
  offs: TPoint;
  r, maskBounds: TRect;
begin
  if not IsAffineMatrixIdentity(CurrentState.SelectionTransform) then raise exception.Create('Unexpected selection transform');
  if not CurrentState.SelectionMaskEmpty then
  begin
    NeedSelectedLayerBackup;
    NeedSelectionLayerBackup;
    MergeWithSelection;
    offs := CurrentState.LayerOffset[CurrentState.SelectedImageLayerIndex];
    maskBounds := CurrentState.GetSelectionMaskBounds;
    r := maskBounds;
    OffsetRect(r, -offs.x, -offs.y);
    IntersectRect(r, r, rect(0,0,GetSelectedImageLayer.Width,GetSelectedImageLayer.Height));
    temp := TBGRABitmap.Create(CurrentState.Width,CurrentState.Height);
    temp.PutImagePart(r.left+offs.x,r.top+offs.y,GetSelectedImageLayer,r,dmSet);
    temp.ApplyMask(CurrentState.SelectionMask,maskBounds);
    BGRAReplace(CurrentState.SelectionLayer,temp);
    NotifyChange(CurrentState.SelectionLayer,maskBounds);
  end;
end;

function TLayerAction.RetrieveSelectionIfLayerEmpty(removeFromBitmap: boolean): boolean;
begin
  if not IsAffineMatrixIdentity(CurrentState.SelectionTransform) then raise exception.Create('Unexpected selection transform');
  NeedSelectedLayerBackup;
  NeedSelectionLayerBackup;
  if CurrentState.SelectionLayerEmpty then
  begin
    RetrieveSelection;
    if removeFromBitmap then EraseSelectionInBitmap;
    result := true;
  end
  else result := false;
end;

function TLayerAction.GetOrCreateSelectionLayer: TBGRABitmap;
begin
  NeedSelectionLayerBackup;
  result := CurrentState.GetOrCreateSelectionLayer;
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
    if dest <> nil then NotifyChange(dest, dest.GetImageBounds);
    if bmp <> nil then NotifyChange(bmp, bmp.GetImageBounds);
  end;
  CurrentState.ReplaceSelectionLayer(bmp,AOwned);
end;

procedure TLayerAction.ApplySelectionMask;
var r: TRect;
begin
  NeedSelectionLayerBackup;
  if (CurrentState.SelectionMask <> nil) and (CurrentState.SelectionLayer <> nil) then
  begin
    r := GetSelectionLayerBounds;
    if not IsRectEmpty(r) then
    begin
      GetOrCreateSelectionLayer.ApplyMask(CurrentState.SelectionMask,r);
      NotifyChange(CurrentState.GetOrCreateSelectionLayer,r);
    end;
  end;
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
  imgDiff: TImageLayerStateDifference;
  composedDiff: TComposedImageDifference;
  owned, rasterizeOriginal: boolean;

  procedure NotifyPrediff;
  begin
    if Assigned(FPrediff) then
    begin
      if Assigned(FOnNotifyUndo) then
      begin
        owned := false;
        FOnNotifyUndo(self, FPrediff, owned);
        if not owned then FPrediff.Free;
      end else
        FPrediff.Free;
      FPrediff := nil;
    end;
  end;

begin
  if (FBackupSelectedLayerDefined or FBackupSelectionMaskDefined or FBackupSelectionLayerDefined) and
     not (ChangeBoundsNotified and IsRectEmpty(FSelectedImageLayerChangedArea) and IsRectEmpty(FSelectionMaskChangedArea) and
         IsRectEmpty(FSelectionLayerChangedArea)) then
  begin
    if FBackupSelectionLayerDefined then
    begin
      if ChangeBoundsNotified then
        CurrentState.DiscardSelectionLayerBounds(FSelectionLayerChangedArea)
        else CurrentState.DiscardSelectionLayerBoundsCompletely;
      if CurrentState.SelectionLayerEmpty then
        CurrentState.ReplaceSelectionLayer(nil,True);
    end;
    if FBackupSelectionMaskDefined then
    begin
      if ChangeBoundsNotified then
        CurrentState.DiscardSelectionMaskBounds(FSelectionMaskChangedArea)
        else CurrentState.DiscardSelectionMaskBoundsCompletely;
      if CurrentState.SelectionMaskEmpty then
        CurrentState.RemoveSelection;
    end;

    if ChangeBoundsNotified then
      imgDiff := CurrentState.ComputeLayerDifference(FBackupSelectedLayer, FSelectedImageLayerChangedArea,
        FBackupSelection, FSelectionMaskChangedArea,
        FBackupSelectionLayer, FSelectionLayerChangedArea) as TImageLayerStateDifference
    else
      imgDiff := CurrentState.ComputeLayerDifference(FBackupSelectedLayer, FBackupSelectedLayerDefined,
        FBackupSelection, FBackupSelectionMaskDefined,
        FBackupSelectionLayer, FBackupSelectionLayerDefined) as TImageLayerStateDifference;
    if imgDiff.IsIdentity then FreeAndNil(imgDiff);

    if ADiscardBackup then
    begin
      FreeAndNil(FBackupSelectionLayer);
      FreeAndNil(FBackupSelectedLayer);
      FreeAndNil(FBackupSelection);
      FBackupSelectedLayerDefined := false;
      FBackupSelectedLayerDefined := false;
      FBackupSelectionMaskDefined := false;
    end else
    begin
      if FBackupSelectionLayerDefined then
      begin
        if (FBackupSelectionLayer = nil) and (CurrentState.SelectionLayer <> nil) then
        begin
          if not CurrentState.SelectionLayerEmpty then
            FBackupSelectionLayer := CurrentState.SelectionLayer.Duplicate as TBGRABitmap;
        end else
        if Assigned(FBackupSelectionLayer) then
        begin
          if ChangeBoundsNotified then FBackupSelectionLayer.ClipRect := FSelectionLayerChangedArea;
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
        if ChangeBoundsNotified then FBackupSelectedLayer.ClipRect := FSelectedImageLayerChangedArea;
        if Assigned(CurrentState.SelectedImageLayer) then
          FBackupSelectedLayer.PutImage(0,0,CurrentState.SelectedImageLayer,dmSet)
        else
          FBackupSelectedLayer.FillRect(0,0,CurrentState.Width,CurrentState.Height,BGRAPixelTransparent,dmSet);
        FBackupSelectedLayer.NoClip;
        FSelectedImageLayerChangedArea := EmptyRect;
      end;
      if FBackupSelectionMaskDefined then
      begin
        if (FBackupSelection = nil) and (CurrentState.SelectionMask <> nil) then
        begin
          if not CurrentState.SelectionMaskEmpty then
            FBackupSelection := CurrentState.SelectionMask.Duplicate as TBGRABitmap;
        end else
        if (FBackupSelection <> nil) then
        begin
          if ChangeBoundsNotified then FBackupSelection.ClipRect := FSelectionMaskChangedArea;
          if Assigned(CurrentState.SelectionMask) then
            FBackupSelection.PutImage(0,0,CurrentState.SelectionMask,dmSet)
          else
            FBackupSelection.FillRect(0,0,CurrentState.Width,CurrentState.Height,BGRABlack,dmSet);
          FBackupSelection.NoClip;
        end;
        FSelectionMaskChangedArea := EmptyRect;
      end;
    end;

    if assigned(imgDiff) then
    begin
      rasterizeOriginal := CurrentState.LayerOriginalDefined[CurrentState.SelectedImageLayerIndex] and imgDiff.ChangeImageLayer;
      if Assigned(FPrediff) or rasterizeOriginal then
      begin
        composedDiff := TComposedImageDifference.Create;
        if Assigned(FPrediff) then
        begin
          composedDiff.AddRange(FPrediff);
          FPrediff.ReleaseDiffs;
          FreeAndNil(FPrediff);
        end;
        if rasterizeOriginal then
          composedDiff.Add(TDiscardOriginalDifference.Create(CurrentState,
            CurrentState.SelectedImageLayerIndex, true));
        composedDiff.Add(imgDiff);
        if Assigned(FOnNotifyUndo) then
        begin
          owned := false;
          FOnNotifyUndo(self, composedDiff, owned);
          if not owned then composedDiff.Free;
        end else
          composedDiff.Free;
      end else
      begin
        if Assigned(FOnNotifyUndo) then
        begin
          owned := false;
          FOnNotifyUndo(self, imgDiff, owned);
          if not owned then imgDiff.Free;
        end else
          imgDiff.Free;
      end;
    end else NotifyPrediff;
  end else NotifyPrediff;
end;


end.

