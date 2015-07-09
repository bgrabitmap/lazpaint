unit UFilterConnector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UImage, ULayerAction, UImageType, Forms,
  LazPaintType, BGRABitmap, uscripting;

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
    function GetActiveLayer: TBGRABitmap;
    function GetBackupLayer: TBGRABitmap;
    function GetCurrentSelection: TBGRABitmap;
    procedure Init(ALazPaintInstance: TLazPaintCustomInstance; AAction: TLayerAction; AOwned: boolean; AParameters: TVariableSet);
    procedure OnTryStop({%H-}sender: TCustomLayerAction);
    procedure DiscardAction;
    procedure ApplySelectionMaskOn(AFilteredLayer: TBGRABitmap);
    procedure ImageChanged;
  public
    ApplyOnSelectionLayer: boolean;
    Form: TForm;
    constructor Create(ALazPaintInstance : TLazPaintCustomInstance; AParameters: TVariableSet);
    constructor Create(ALazPaintInstance : TLazPaintCustomInstance; AParameters: TVariableSet; AAction: TLayerAction; AOwned: boolean);
    destructor Destroy; override;
    procedure ValidateAction;
    procedure InvalidateActiveLayer; overload;
    procedure InvalidateActiveLayer(ARect: TRect); overload;
    procedure PutImage(AFilteredLayer: TBGRABitmap; AMayBeColored: boolean; AOwner: boolean);
    procedure PutImage(AFilteredLayer: TBGRABitmap; AModifiedRect: TRect; AMayBeColored: boolean; AOwner: boolean);
    procedure RestoreBackup;
    property BackupLayer: TBGRABitmap read GetBackupLayer;
    property CurrentSelection: TBGRABitmap read GetCurrentSelection;
    property OnTryStopAction: TFilterOnTryStopActionHandler read FOnTryStopAction write FOnTryStopAction;
    property ActionDone: boolean read GetActionDone;
    property LazPaintInstance: TLazPaintCustomInstance read FLazPaintInstance;
    property ActiveLayer: TBGRABitmap read GetActiveLayer;
    property WorkArea: TRect read FWorkArea;
    property Parameters: TVariableSet read FParameters;
  end;

implementation

uses Types, BGRABitmapTypes;

{ TFilterConnector }

function TFilterConnector.GetActiveLayer: TBGRABitmap;
begin
  if ApplyOnSelectionLayer then
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

function TFilterConnector.GetBackupLayer: TBGRABitmap;
begin
  if ApplyOnSelectionLayer then
    result := FAction.BackupSelectionLayer
  else
    result := FAction.BackupSelectedLayer;
end;

function TFilterConnector.GetCurrentSelection: TBGRABitmap;
begin
  if ApplyOnSelectionLayer or FLazPaintInstance.Image.SelectionEmpty then
    result := nil
  else
  begin
    result := FLazPaintInstance.Image.SelectionReadonly;
    if (result.Width <> ActiveLayer.Width) or (result.Height <> ActiveLayer.Height) then
      result := nil;
  end;
end;

procedure TFilterConnector.Init(ALazPaintInstance: TLazPaintCustomInstance; AAction: TLayerAction; AOwned: boolean; AParameters: TVariableSet);
var sel: TBGRABitmap;
  y,x: integer;
  p : PBGRAPixel;

begin
  FLazPaintInstance := ALazPaintInstance;
  FParameters := AParameters;
  FAction := AAction;
  FActionOwned:= AOwned;
  FAction.OnTryStop := @OnTryStop;
  ApplyOnSelectionLayer:= not FLazPaintInstance.Image.SelectionLayerIsEmpty;
  sel := CurrentSelection;
  if sel <> nil then
    FWorkArea := FLazPaintInstance.Image.SelectionBounds[false]
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

constructor TFilterConnector.Create(ALazPaintInstance : TLazPaintCustomInstance; AParameters: TVariableSet);
begin
  Init(ALazPaintInstance,TLayerAction.Create(ALazPaintInstance.Image),True,AParameters);
end;

constructor TFilterConnector.Create(ALazPaintInstance : TLazPaintCustomInstance; AParameters: TVariableSet; AAction: TLayerAction; AOwned: boolean);
begin
  Init(ALazPaintInstance,AAction,AOwned,AParameters);
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
  FLazPaintInstance.NotifyImageChange(True, FWorkArea);
end;

procedure TFilterConnector.InvalidateActiveLayer(ARect: TRect);
begin
  if IntersectRect(ARect, ARect, FWorkArea) then
    FLazPaintInstance.NotifyImageChange(True, ARect);
end;

procedure TFilterConnector.PutImage(AFilteredLayer: TBGRABitmap; AMayBeColored: boolean; AOwner: boolean);
begin
  PutImage(AFilteredLayer,FWorkArea,AMayBeColored,AOwner);
end;

procedure TFilterConnector.PutImage(AFilteredLayer: TBGRABitmap;
  AModifiedRect: TRect; AMayBeColored: boolean; AOwner: boolean);
var AMine: boolean;
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
    ActiveLayer.PutImagePart(AModifiedRect.Left,AModifiedRect.Top,AFilteredLayer,AModifiedRect,dmSet);
    if AMine then AFilteredLayer.Free;
    FLazPaintInstance.NotifyImageChange(True, AModifiedRect);
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

