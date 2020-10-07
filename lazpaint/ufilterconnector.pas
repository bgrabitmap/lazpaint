// SPDX-License-Identifier: GPL-3.0-only
unit UFilterConnector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UImage, ULayerAction, UImageType, Forms,
  LazPaintType, BGRABitmap, BGRABitmapTypes, uscripting;

type
  TFilterConnector = class;
  TFilterOnTryStopActionHandler = procedure(sender: TFilterConnector) of object;

  { TFilterConnector }

  TFilterConnector = class
  private
    FLazPaintInstance : TLazPaintCustomInstance;
    FAction: TLayerAction;
    FActionOwned: boolean;
    FOnTryStopAction: TFilterOnTryStopActionHandler;
    FWorkArea: TRect;
    FWorkAreaFullySelected: boolean;
    FParameters: TVariableSet;
    function GetActionDone: boolean;
    function GetActiveLayeOffset: TPoint;
    function GetActiveLayer: TBGRABitmap;
    function GetBackupLayer: TBGRABitmap;
    function GetCurrentSelection: TBGRABitmap;
    procedure Init(ALazPaintInstance: TLazPaintCustomInstance; AAction: TLayerAction; AOwned: boolean;
                   AParameters: TVariableSet; AApplyOfsBefore: boolean);
    procedure OnTryStop({%H-}sender: TCustomLayerAction);
    procedure DiscardAction;
    procedure ApplySelectionMaskOn(AFilteredLayer: TBGRABitmap);
    procedure ImageChanged;
  public
    ApplyOnSelectionLayer: boolean;
    Form: TForm;
    constructor Create(ALazPaintInstance : TLazPaintCustomInstance; AParameters: TVariableSet; AApplyOfsBefore: boolean);
    constructor Create(ALazPaintInstance : TLazPaintCustomInstance; AParameters: TVariableSet; AAction: TLayerAction; AOwned: boolean);
    destructor Destroy; override;
    procedure ValidateAction;
    procedure InvalidateActiveLayer; overload;
    procedure InvalidateActiveLayer(ARect: TRect); overload;
    procedure PutImage(AFilteredLayer: TBGRABitmap; AMayBeColored: boolean; AOwner: boolean; ADrawMode: TDrawMode = dmSet);
    procedure PutImage(AFilteredLayer: TBGRABitmap; AModifiedRect: TRect; AMayBeColored: boolean; AOwner: boolean; ADrawMode: TDrawMode = dmSet);
    procedure RestoreBackup;
    property BackupLayer: TBGRABitmap read GetBackupLayer;
    property CurrentSelection: TBGRABitmap read GetCurrentSelection;
    property OnTryStopAction: TFilterOnTryStopActionHandler read FOnTryStopAction write FOnTryStopAction;
    property ActionDone: boolean read GetActionDone;
    property LazPaintInstance: TLazPaintCustomInstance read FLazPaintInstance;
    property ActiveLayer: TBGRABitmap read GetActiveLayer;
    property ActiveLayerOffset: TPoint read GetActiveLayeOffset;
    property WorkArea: TRect read FWorkArea;
    property Parameters: TVariableSet read FParameters;
  end;

implementation

uses Types;

{ TFilterConnector }

function TFilterConnector.GetActiveLayer: TBGRABitmap;
begin
  if FAction = nil then
    result := nil
  else if ApplyOnSelectionLayer then
    result := FAction.GetSelectionLayerIfExists
  else
    result := FAction.SelectedImageLayer;
end;

function TFilterConnector.GetActionDone: boolean;
begin
  if FAction = nil then
    result := false
  else
    result := FAction.Done;
end;

function TFilterConnector.GetActiveLayeOffset: TPoint;
begin
  if ApplyOnSelectionLayer then
    result := Point(0,0)
  else
    result := FAction.SelectedImageLayerOffset;
end;

function TFilterConnector.GetBackupLayer: TBGRABitmap;
begin
  if ApplyOnSelectionLayer then
    result := FAction.BackupSelectionLayer
  else
    result := FAction.BackupSelectedLayer;
end;

function TFilterConnector.GetCurrentSelection: TBGRABitmap;
begin
  if ApplyOnSelectionLayer or FLazPaintInstance.Image.SelectionMaskEmpty then
    result := nil
  else
  begin
    result := FLazPaintInstance.Image.SelectionMaskReadonly;
    if (result.Width <> ActiveLayer.Width) or (result.Height <> ActiveLayer.Height) then
      result := nil;
  end;
end;

procedure TFilterConnector.Init(ALazPaintInstance: TLazPaintCustomInstance; AAction: TLayerAction; AOwned: boolean;
                                AParameters: TVariableSet; AApplyOfsBefore: boolean);
var sel: TBGRABitmap;
  y,x: integer;
  p : PBGRAPixel;

begin
  FLazPaintInstance := ALazPaintInstance;
  FParameters := AParameters;
  ApplyOnSelectionLayer:= not FLazPaintInstance.Image.SelectionLayerIsEmpty;

  if AAction = nil then
    AAction := ALazPaintInstance.Image.CreateAction(
                  AApplyOfsBefore and not ApplyOnSelectionLayer,
                  ApplyOnSelectionLayer);

  FAction := AAction;
  FActionOwned:= AOwned;
  FAction.OnTryStop := @OnTryStop;

  sel := CurrentSelection;
  if sel <> nil then
    FWorkArea := FLazPaintInstance.Image.SelectionMaskBounds
  else
    FWorkArea := rect(0,0,ActiveLayer.Width,ActiveLayer.Height);
  FWorkAreaFullySelected := true;
  if sel <> nil then
    for y := FWorkArea.Top to FWorkArea.Bottom-1 do
    begin
      p := sel.ScanLine[y]+FWorkArea.Left;
      for x := FWorkArea.Left to FWorkArea.Right-1 do
      begin
        if p^.green <> 255 then
        begin
          FWorkAreaFullySelected := false;
          break;
        end;
        inc(p);
      end;
      if not FWorkAreaFullySelected then break;
    end;
end;

procedure TFilterConnector.OnTryStop(sender: TCustomLayerAction);
begin
  DiscardAction;
  If FOnTryStopAction <> nil then FOnTryStopAction(self);
end;

procedure TFilterConnector.DiscardAction;
begin
  if FActionOwned then
    FreeAndNil(FAction)
  else
  if FAction <> nil then
  begin
    FAction.OnTryStop := nil;
    FAction := nil;
  end;
end;

constructor TFilterConnector.Create(ALazPaintInstance : TLazPaintCustomInstance; AParameters: TVariableSet; AApplyOfsBefore: boolean);
begin
  Init(ALazPaintInstance,nil,True,AParameters,AApplyOfsBefore);
end;

constructor TFilterConnector.Create(ALazPaintInstance : TLazPaintCustomInstance; AParameters: TVariableSet; AAction: TLayerAction; AOwned: boolean);
begin
  Init(ALazPaintInstance,AAction,AOwned,AParameters,false);
end;

destructor TFilterConnector.Destroy;
begin
  DiscardAction;
  inherited Destroy;
end;

procedure TFilterConnector.ValidateAction;
begin
  FAction.Validate;
end;

procedure TFilterConnector.InvalidateActiveLayer;
begin
  InvalidateActiveLayer(FWorkArea);
end;

procedure TFilterConnector.InvalidateActiveLayer(ARect: TRect);
begin
  if IntersectRect(ARect, ARect, FWorkArea) then
  begin
    with FLazPaintInstance.Image.LayerOffset[FLazPaintInstance.Image.CurrentLayerIndex] do
      OffsetRect(ARect, X,Y);
    FLazPaintInstance.NotifyImageChange(True, ARect);
  end;
end;

procedure TFilterConnector.PutImage(AFilteredLayer: TBGRABitmap; AMayBeColored: boolean; AOwner: boolean; ADrawMode: TDrawMode);
begin
  PutImage(AFilteredLayer,FWorkArea,AMayBeColored,AOwner,ADrawMode);
end;

procedure TFilterConnector.PutImage(AFilteredLayer: TBGRABitmap;
  AModifiedRect: TRect; AMayBeColored: boolean; AOwner: boolean; ADrawMode: TDrawMode);
var AMine: boolean;
  imgRect: TRect;
begin
  if IntersectRect(AModifiedRect,AModifiedRect,FWorkArea) then
  begin
    AMine := AOwner;
    if AMayBeColored and LazPaintInstance.BlackAndWhite then
    begin
      if not AMine then
      begin
        AFilteredLayer := AFilteredLayer.Duplicate as TBGRABitmap;
        AMine := true;
      end;
      AFilteredLayer.InplaceGrayscale(AModifiedRect);
    end;
    if not FWorkAreaFullySelected then
    begin
      if not AMine then
      begin
        AFilteredLayer := AFilteredLayer.Duplicate as TBGRABitmap;
        AMine := true;
      end;
      ApplySelectionMaskOn(AFilteredLayer);
    end;
    ActiveLayer.PutImagePart(AModifiedRect.Left,AModifiedRect.Top,AFilteredLayer,AModifiedRect,ADrawMode);
    if AMine then AFilteredLayer.Free;
    imgRect := AModifiedRect;
    with ActiveLayerOffset do
      OffsetRect(imgRect, X,Y);
    FLazPaintInstance.NotifyImageChange(True, imgRect);
  end;
end;

procedure TFilterConnector.RestoreBackup;
var oldClip: TRect;
begin
  if not IsRectEmpty(FWorkArea) then
  begin
    oldClip := ActiveLayer.ClipRect;
    ActiveLayer.ClipRect := FWorkArea;
    ActiveLayer.PutImage(0,0,BackupLayer,dmSet);
    ActiveLayer.ClipRect := oldClip;
    FLazPaintInstance.NotifyImageChange(False, FWorkArea);
  end;
end;

procedure TFilterConnector.ImageChanged;
begin
  FLazPaintInstance.NotifyImageChangeCompletely(True);
end;

procedure TFilterConnector.ApplySelectionMaskOn(AFilteredLayer: TBGRABitmap);
var
  curSel: TBGRABitmap;
  pfiltered,psource,pselection: PBGRAPixel;
  n: integer;
  alpha: byte;
begin
  curSel := CurrentSelection;
  if curSel <> nil then
  begin
    if (curSel.Width = AFilteredLayer.Width) and
      (curSel.Height = AFilteredLayer.Height) and
      (AFilteredLayer.Width = ActiveLayer.Width) and (AFilteredLayer.Height = ActiveLayer.Height) then
    begin
      pfiltered := AFilteredLayer.data;
      psource := BackupLayer.data;
      pselection:= curSel.data;
      for n := AFilteredLayer.NbPixels-1 downto 0 do
      begin
        alpha := pselection^.green;
        if alpha = 0 then
          pfiltered^ := psource^
        else if alpha <> 255 then
          pfiltered^ := MergeBGRAWithGammaCorrection(pfiltered^,alpha,psource^,not alpha);
        inc(pfiltered);
        inc(psource);
        inc(pselection);
      end;
    end;
  end;
end;

end.

