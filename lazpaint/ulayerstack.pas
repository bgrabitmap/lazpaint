unit ULayerstack;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, BGRAVirtualScreen, BCPanel, LazPaintType,
  BGRABitmap, UVolatileScrollBar, Types, BGRABitmapTypes, UImageObservation;

type
  TDrawLayerItemResult = record
    PreviewPts: array of TPointF;
    NameRect,OpacityBar: TRect;
  end;

  { TFLayerStack }

  TFLayerStack = class(TForm)
    Panel_WindowTitle: TBCPanel;
    BGRALayerStack: TBGRAVirtualScreen;
    ComboBox_BlendOp: TComboBox;
    Panel1: TPanel;
    TimerScroll: TTimer;
    ToolBar1: TToolBar;
    ToolBar2: TToolBar;
    ToolBar3: TToolBar;
    ToolButton1: TToolButton;
    ToolZoomLayerStackIn: TToolButton;
    ToolZoomLayerStackOut: TToolButton;
    ToolBlendOp: TToolButton;
    procedure BGRALayerStackMouseDown(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure BGRALayerStackMouseMove(Sender: TObject; {%H-}Shift: TShiftState; X,
      Y: Integer);
    procedure BGRALayerStackMouseUp(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure BGRALayerStackMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure BGRALayerStackRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure BGRALayerStackResize(Sender: TObject);
    procedure ComboBox_BlendOpChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TimerScrollTimer(Sender: TObject);
    procedure ToolBlendOpClick(Sender: TObject);
    procedure ToolZoomLayerStackInClick(Sender: TObject);
    procedure ToolZoomLayerStackOutClick(Sender: TObject);
    procedure HandleChangeLayerOpacity(X,{%H-}Y: integer);
    procedure HandleSelectLayer(i,x,y: integer);
    procedure UpdateComboBlendOp;
    procedure UpdateLayerStackItem(idx: integer);
  private
    FCompletelyResizeable: boolean;
    { private declarations }
    UpdatingComboBlendOp : boolean;
    background,iconBackground: TBGRABitmap;
    ZoomFactor: single;
    renaming: boolean;
    LayerRectWidth,LayerRectHeight, StackWidth,StackHeight: integer;
    Offset,ScrollPos,MaxScrollPos: TPoint;
    AvailableWidth,AvailableHeight: integer;
    changingLayerOpacity: integer;
    VolatileHorzScrollBar, VolatileVertScrollBar: TVolatileScrollBar;
    ScrollButtonRect: TRect;
    InterruptorWidth,InterruptorHeight: integer;
    interruptors: array of TRect;
    LayerInfo: array of TDrawLayerItemResult;
    movingItemStart: boolean;
    movingItem: TBGRABitmap;
    movingItemSourceIndex: integer;
    movingItemMousePos,movingItemMouseOrigin,movingItemOrigin: TPoint;
    timerScrollDeltaY: integer;
    updatingImageOnly: boolean;
    ScrollStackItemIntoView : boolean;
    procedure ComputeLayout(ABitmap: TBGRABitmap);
    procedure ComputeScrolling(AWithHorzScrollBar,AWithVertScrollBar: boolean);
    function DrawLayerItem(ABitmap: TBGRABitmap; layerPos: TPoint;
      layerIndex: integer; ASelected: boolean): TDrawLayerItemResult;
    procedure RedrawLayerStack(Bitmap: TBGRABitmap; Layout: boolean; UpdateItem: Integer);
    procedure SetCompletelyResizeable(AValue: boolean);
    procedure UpdateImage;
    procedure OnImageChangedHandler(AEvent: TLazPaintImageObservationEvent);
    procedure SelectBlendOp;
    procedure DoScrollVertically(AAmount: integer);
  public
    { public declarations }
    LazPaintInstance: TLazPaintCustomInstance;
    procedure InvalidateStack(AScrollIntoView: boolean);
    procedure SetLayerStackScrollPosOnItem(idx: integer);
    procedure AddButton(AAction: TBasicAction);
    procedure AddSeparator;
    property CompletelyResizeable: boolean read FCompletelyResizeable write SetCompletelyResizeable;
  end;

var TFLayerStack_CustomDPI: integer = 96;

implementation

uses BGRAFillInfo,LCScaleDPI,uresourcestrings,ublendop, uimage, utool, BGRAText, BGRAThumbnail,
   BGRALayerOriginal, math, BGRATransform, BGRASVGOriginal;

function TFLayerStack.DrawLayerItem(ABitmap: TBGRABitmap; layerPos: TPoint; layerIndex: integer; ASelected: boolean): TDrawLayerItemResult;
var
  lColor,lColorTransp: TBGRAPixel;
  barwidth: integer;
  sourceCoords: Array Of TPointF;
  reduced: TBGRABitmap;
  reducedBounds: TRect;
begin
  if ASelected then
    lColor := ColorToBGRA(ColorToRGB(clHighlightText))
  else
    lColor := ColorToBGRA(ColorToRGB(clWindowText));
  lColorTransp := lColor;
  lColorTransp.alpha := lColorTransp.alpha div 2;

  result.PreviewPts := PointsF([pointf(layerPos.X+0.25*LayerRectWidth,layerPos.Y+round(LayerRectHeight*0.1)),
     pointf(layerPos.X+0.9*LayerRectWidth,layerPos.Y+round(LayerRectHeight*0.1)),
    pointf(layerPos.X+0.7*LayerRectWidth,layerPos.Y+round(LayerRectHeight*0.9)),
    pointf(layerPos.X+0.05*LayerRectWidth,layerPos.Y+round(LayerRectHeight*0.9))]);
  reduced := TBGRABitmap.Create(round(LayerRectWidth*0.65), round(LayerRectHeight*0.8));
  reducedBounds := RectWithSize(LazPaintInstance.Image.LayerOffset[layerIndex].X,
                        LazPaintInstance.Image.LayerOffset[layerIndex].Y,
                        LazPaintInstance.Image.LayerBitmap[layerIndex].Width,
                        LazPaintInstance.Image.LayerBitmap[layerIndex].Height);
  reducedBounds.Left := round(reducedBounds.Left*reduced.Width/LazPaintInstance.Image.Width);
  reducedBounds.Top := round(reducedBounds.Top*reduced.Height/LazPaintInstance.Image.Height);
  reducedBounds.Right := round(reducedBounds.Right*reduced.Width/LazPaintInstance.Image.Width);
  reducedBounds.Bottom := round(reducedBounds.Bottom*reduced.Height/LazPaintInstance.Image.Height);
  reduced.StretchPutImage(reducedBounds, LazPaintInstance.Image.LayerBitmap[layerIndex], dmDrawWithTransparency);

  result.PreviewPts[0].y += 0.5;
  result.PreviewPts[1].y += 0.5;
  result.PreviewPts[2].y -= 0.5;
  result.PreviewPts[3].y -= 0.5;
  if LazPaintInstance.Image.IsIconCursor then
    ABitmap.FillPolyAntialias(result.PreviewPts,iconBackground)
  else
    ABitmap.FillPolyAntialias(result.PreviewPts,background);

  sourceCoords := PointsF([pointf(-0.49,-0.49),pointf(reduced.Width-0.51,-0.49),
            pointf(reduced.Width-0.51,reduced.Height-0.51),pointf(-0.49,reduced.Height-0.51)]);
  ABitmap.FillPolyLinearMapping(result.PreviewPts, reduced, sourceCoords, False);
  reduced.Free;

  result.PreviewPts[0].y -= 0.5;
  result.PreviewPts[1].y -= 0.5;
  result.PreviewPts[2].y += 0.5;
  result.PreviewPts[3].y += 0.5;
  if not LazPaintInstance.Image.LayerVisible[LayerIndex] then ABitmap.CustomPenStyle := BGRAPenStyle(LayerRectHeight/8,LayerRectHeight/8);
  ABitmap.DrawPolygonAntialias( result.PreviewPts, lColor,1);
  ABitmap.PenStyle := psSolid;
  if ASelected then
  begin
    result.NameRect := rect(layerpos.X+round(LayerRectWidth*0.95),layerpos.Y,layerpos.X+StackWidth,layerPos.Y+LayerRectHeight div 2);
    barwidth := StackWidth-InterruptorWidth-Int32or64(round(LayerRectWidth*1.1));
    if barwidth > LayerRectWidth then barwidth := LayerRectWidth;
    result.OpacityBar := rect(layerpos.X+LayerRectWidth,layerpos.Y+LayerRectHeight div 2,layerpos.X+LayerRectWidth+barwidth,layerpos.Y+LayerRectHeight);
    ABitmap.Rectangle(result.OpacityBar.left,(result.OpacityBar.top*3+result.OpacityBar.bottom) div 4,result.OpacityBar.right,
        (result.OpacityBar.top+result.OpacityBar.bottom*3) div 4,lColor,dmSet);
    ABitmap.FillRect(result.OpacityBar.left+1,(result.OpacityBar.top*3+result.OpacityBar.bottom) div 4+1,result.OpacityBar.left+1+
      Int32or64(round((result.OpacityBar.right-result.OpacityBar.left-2)*LazPaintInstance.Image.LayerOpacity[LayerIndex]/255)),
       (result.OpacityBar.top+result.OpacityBar.bottom*3) div 4-1,lColorTransp,dmDrawWithTransparency);
  end
  else
  begin
    result.NameRect := rect(layerpos.X+round(LayerRectWidth*0.95),layerpos.Y,layerpos.X+StackWidth,layerPos.Y+LayerRectHeight);
    result.OpacityBar := EmptyRect;
  end;
  {$IFDEF DARWIN}
  ABitmap.FontQuality := fqFineAntialiasing;
  {$ENDIF}
  ABitmap.TextOut(result.NameRect.Left,result.NameRect.Top+(result.NameRect.bottom-result.NameRect.top -ABitmap.FontFullHeight) div 2,
    LazPaintInstance.Image.LayerName[layerIndex],lColor);
end;

{ TFLayerStack }

procedure TFLayerStack.FormCreate(Sender: TObject);
begin
  ScaleControl(Self,OriginalDPI,TFLayerStack_CustomDPI,TFLayerStack_CustomDPI);
  FCompletelyResizeable:= true;
  Position := poDesigned;
  ZoomFactor := TFLayerStack_CustomDPI/96;
  ScrollPos := point(0,0);
  VolatileHorzScrollBar := nil;
  VolatileVertScrollBar := nil;
  changingLayerOpacity:= -1;
  self.EnsureVisible(False);
  background := TBGRABitmap.Create(4,4,ImageCheckersColor1);
  background.FillRect(0,0,2,2,ImageCheckersColor2,dmDrawWithTransparency);
  background.FillRect(2,2,4,4,ImageCheckersColor2,dmDrawWithTransparency);
  iconBackground := TBGRABitmap.Create(4,4,IconCheckersColor1);
  iconBackground.FillRect(0,0,2,2,IconCheckersColor2,dmDrawWithTransparency);
  iconBackground.FillRect(2,2,4,4,IconCheckersColor2,dmDrawWithTransparency);

  renaming := false;
  Visible := false;
  movingItemStart := false;
  Panel_WindowTitle.Caption := ' '+self.Caption;
end;

procedure TFLayerStack.FormDeactivate(Sender: TObject);
begin
  Panel_WindowTitle.Background.Color := clInactiveCaption;
  Panel_WindowTitle.FontEx.Color := clInactiveCaptionText;
end;

procedure TFLayerStack.FormDestroy(Sender: TObject);
begin
  background.Free;
  iconBackground.Free;
  FreeAndNil(movingItem);
  FreeAndNil(VolatileHorzScrollBar);
  FreeAndNil(VolatileVertScrollBar);
end;

procedure TFLayerStack.FormHide(Sender: TObject);
begin
  LazPaintInstance.Image.OnImageChanged.RemoveObserver(@OnImageChangedHandler);
end;

procedure TFLayerStack.FormShow(Sender: TObject);
var iconSize: integer;
    images: TImageList;
begin
  LazPaintInstance.Image.OnImageChanged.AddObserver(@OnImageChangedHandler);
  iconSize := DoScaleX(16, 96, TFLayerStack_CustomDPI);

  images := LazPaintInstance.Icons[iconSize];
  ToolBar1.Images := images;
  ToolBar1.ButtonWidth := images.Width+DoScaleX(4,96,TFLayerStack_CustomDPI);
  ToolBar1.ButtonHeight := images.Height+DoScaleY(4,96,TFLayerStack_CustomDPI);
  ToolBar1.Height := ToolBar1.ButtonHeight+1;
  ToolBar2.Images := images;
  ToolBar2.ButtonWidth := images.Width+DoScaleX(4,96,TFLayerStack_CustomDPI);
  ToolBar2.ButtonHeight := images.Height+DoScaleY(4,96,TFLayerStack_CustomDPI);
  ToolBar2.Height := ToolBar1.ButtonHeight+1;
  ToolBar3.Images := images;
  ToolBar3.ButtonWidth := images.Width+DoScaleX(4,96,TFLayerStack_CustomDPI);
  ToolBar3.ButtonHeight := images.Height+DoScaleY(4,96,TFLayerStack_CustomDPI);
  ToolBar3.Height := ToolBar1.ButtonHeight+1;

  ClientWidth := ToolBar2.ButtonCount * (ToolBar2.ButtonWidth+1) + 5 + Toolbar2.Left;
  Constraints.MinWidth := Width;

  ToolBar1.Width := ToolBar1.ButtonCount * (ToolBar1.ButtonWidth+1) + 8;
  ToolBar3.Width := ToolBar3.ButtonCount * (ToolBar3.ButtonWidth+1) + 5;

  ComboBox_BlendOp.Left := Toolbar3.Left+Toolbar3.Width;
  Toolbar1.Left := ClientWidth-Toolbar1.Width-6;
  ComboBox_BlendOp.Width := Toolbar1.Left - ComboBox_BlendOp.Left;
  Toolbar2.Top := Toolbar1.Top + Toolbar1.Height;
  Panel1.Height := Toolbar2.Top+Toolbar2.Height+2;

  ComboBox_BlendOp.Font.Height := -FontEmHeightSign * ((images.Height-2) * 6 div 10 + 2);

  if Toolbar2.Top < ComboBox_BlendOp.Top + ComboBox_BlendOp.Height then
    Toolbar2.Top := ComboBox_BlendOp.Top + ComboBox_BlendOp.Height;
  if Toolbar2.Top+Toolbar2.Height+2 > Panel1.Height then
    Panel1.Height := Toolbar2.Top+Toolbar2.Height+2;
end;

procedure TFLayerStack.TimerScrollTimer(Sender: TObject);
begin
  TimerScroll.Enabled := False;
  DoScrollVertically(TimerScrollDeltaY);
end;

procedure TFLayerStack.ToolBlendOpClick(Sender: TObject);
begin
  SelectBlendOp;
end;

procedure TFLayerStack.ToolZoomLayerStackInClick(Sender: TObject);
var prevZoom: single;
begin
  prevZoom:= ZoomFactor;
  zoomFactor *= 1.3;
  if zoomFactor > 10 then zoomFactor := 10;
  ScrollPos.Y := round(ScrollPos.Y*ZoomFactor/prevZoom);
  BGRALayerStack.RedrawBitmap;
end;

procedure TFLayerStack.ToolZoomLayerStackOutClick(Sender: TObject);
var prevZoom: single;
begin
  prevZoom:= ZoomFactor;
  zoomFactor /= 1.3;
  if zoomFactor < 1/2 then zoomFactor := 1/2;
  ScrollPos.Y := round(ScrollPos.Y*ZoomFactor/prevZoom);
  BGRALayerStack.RedrawBitmap;
end;

procedure TFLayerStack.HandleChangeLayerOpacity(X, Y: integer);
var newOpacity: integer;
begin
  if (changingLayerOpacity <> -1) and (changingLayerOpacity <= high(LayerInfo)) then
  with LayerInfo[changingLayerOpacity] do
  begin
    if changingLayerOpacity >= LazPaintInstance.Image.NbLayers then exit;
    newOpacity := round((X-(OpacityBar.left+1))/(OpacityBar.right-OpacityBar.left-2)*255);
    if newOpacity < 0 then newOpacity:= 0;
    if newOpacity > 255 then newOpacity:= 255;
    if LazPaintInstance.Image.LayerOpacity[changingLayerOpacity] = newOpacity then exit;
    updatingImageOnly := true;
    LazPaintInstance.Image.LayerOpacity[changingLayerOpacity] := newOpacity;
    updatingImageOnly := false;
    UpdateLayerStackItem(changingLayerOpacity);
  end;
end;

procedure TFLayerStack.HandleSelectLayer(i,x,y: integer);
var topmostInfo: TTopMostInfo; res: integer;
begin
  if i < LazPaintInstance.Image.NbLayers then
  begin
    if not LazPaintInstance.Image.SelectionLayerIsEmpty and
        (i <> LazPaintInstance.Image.CurrentLayerIndex) then
    begin
      topmostInfo := LazPaintInstance.HideTopmost;
      res := MessageDlg(rsTransferSelectionToOtherLayer,mtConfirmation,[mbOk,mbCancel],0);
      LazPaintInstance.ShowTopmost(topmostInfo);
      if res = mrOk then
      begin
        if LazPaintInstance.Image.SetCurrentLayerByIndex(i) then
        begin
          renaming := false;
          BGRALayerStack.RedrawBitmap;
        end;
      end;
      exit;
    end;
    if LazPaintInstance.Image.SetCurrentLayerByIndex(i) then
    begin
      renaming := false;
      movingItemStart := true;
      movingItemSourceIndex := i;
      movingItemMouseOrigin := point(x,y);
      movingItemMousePos := point(x,y);
      BGRALayerStack.RedrawBitmap;
    end;
  end;
end;

procedure TFLayerStack.UpdateComboBlendOp;
var
  blendOps: TStringList;
  str,selectedStr: string;
  i: integer;
begin
  UpdatingComboBlendOp := true;
  blendOps := TStringList.Create;
  selectedStr := '';
  blendOps.AddStrings(ComboBox_BlendOp.Items);
  i := blendOps.IndexOf(rsOtherBlendOp);
  if i <> -1 then blendOps.Delete(i);
  i := blendOps.IndexOf(rsNormalBlendOp);
  if i <> -1 then blendOps.Delete(i);
  with LazPaintInstance.Image do
    for i := 0 to NbLayers-1 do
    begin
      str := BlendOperationStr[BlendOperation[i]];
      if blendOps.IndexOf(str) = -1 then
        blendOps.Add(str);
      if i = LazPaintInstance.Image.CurrentLayerIndex then
        selectedStr := str;
    end;
  if selectedStr = BlendOperationStr[boTransparent] then
    selectedStr := rsNormalBlendOp;
  i := blendOps.IndexOf(BlendOperationStr[boTransparent]);
  if i <> -1 then blendOps.Delete(i);
  blendOps.Sort;
  blendOps.Insert(0,rsNormalBlendOp);
  blendOps.Add(rsOtherBlendOp);
  if not blendOps.Equals(ComboBox_BlendOp.Items) then
    ComboBox_BlendOp.Items.Assign(blendOps);
  if ComboBox_BlendOp.ItemIndex <> ComboBox_BlendOp.Items.IndexOf(selectedStr) then
    ComboBox_BlendOp.ItemIndex := ComboBox_BlendOp.Items.IndexOf(selectedStr);
  blendOps.Free;
  UpdatingComboBlendOp := false;
end;

procedure TFLayerStack.UpdateLayerStackItem(idx: integer);
begin
  RedrawLayerStack(BGRALayerStack.Bitmap,False,idx);
  BGRALayerStack.Repaint;
end;

procedure TFLayerStack.ComputeLayout(ABitmap: TBGRABitmap);
var i,temp,h: integer;
begin
  interruptors:= nil;
  LayerInfo := nil;
  LayerRectWidth := round(100*zoomFactor);
  LayerRectHeight := round(50*zoomFactor);
  ABitmap.FontName := 'Arial';
  ABitmap.FontQuality := fqSystemClearType;

  temp := ScaleY(20,OriginalDPI);
  h := LayerRectHeight div 3;
  if h > temp then h := temp;
  temp := ScaleY(12,OriginalDPI);
  if h < temp then h := temp;
  ABitmap.FontFullHeight := h;

  InterruptorWidth := LayerRectHeight div 4;
  InterruptorHeight := LayerRectHeight div 4;
  temp := ScaleY(28,OriginalDPI);
  if InterruptorWidth > temp then InterruptorWidth := temp;
  if InterruptorHeight > temp then InterruptorHeight := temp;
  temp := ScaleY(7,OriginalDPI);
  if InterruptorHeight < temp then InterruptorHeight := temp;
  if InterruptorWidth < temp then InterruptorWidth := temp;
  StackWidth := InterruptorWidth+LayerRectWidth+ABitmap.TextSize('Some layer name').cx;
  StackHeight := LayerRectHeight*LazPaintInstance.Image.NbLayers;
  for i := 0 to LazPaintInstance.Image.NbLayers-1 do
  begin
    temp := InterruptorWidth+LayerRectWidth+ABitmap.TextSize(LazPaintInstance.Image.LayerName[i]).cx;
    if temp > StackWidth then StackWidth := temp;
  end;
  if ((VolatileHorzScrollBar = nil) or not VolatileHorzScrollBar.ScrollThumbDown) and
    ((VolatileVertScrollBar = nil) or not VolatileVertScrollBar.ScrollThumbDown) then
  begin
    FreeAndNil(VolatileHorzScrollBar);
    FreeAndNil(VolatileVertScrollBar);
    ComputeScrolling(False,False);
  end;
  Offset := ScrollPos;
  if StackHeight < AvailableHeight then Offset.Y := -(AvailableHeight-StackHeight);

  if (VolatileHorzScrollBar <> nil) and (VolatileVertScrollBar <> nil) then
    ScrollButtonRect := rect(AvailableWidth,AvailableHeight,AvailableWidth+VolatileScrollBarSize,AvailableHeight+VolatileScrollBarSize)
  else
    ScrollButtonRect := EmptyRect;
end;

procedure TFLayerStack.ComputeScrolling(AWithHorzScrollBar, AWithVertScrollBar: boolean);
var
  NeedHorzScrollBar,NeedVertScrollBar: boolean;
  WithHorzScrollBar,WithVertScrollBar: boolean;
begin
  WithHorzScrollBar:= AWithHorzScrollBar;
  WithVertScrollBar:= AWithVertScrollBar;
  AvailableWidth := BGRALayerStack.Width;
  AvailableHeight := BGRALayerStack.Height;
  if AvailableWidth <= VolatileThumbSize then WithHorzScrollBar := false;
  if AvailableHeight <= VolatileThumbSize then WithVertScrollBar := false;
  if AvailableWidth <= VolatileScrollBarSize then WithVertScrollBar:= false;
  if AvailableHeight <= VolatileScrollBarSize then WithHorzScrollBar := false;
  if WithVertScrollBar then dec(AvailableWidth, VolatileScrollBarSize);
  if AvailableWidth <= VolatileThumbSize then WithHorzScrollBar := false;
  if WithHorzScrollBar then dec(AvailableHeight, VolatileScrollBarSize);
  if AvailableHeight <= VolatileThumbSize then
  begin
    WithVertScrollBar := false;
    AvailableWidth := BGRALayerStack.Width;
  end;

  MaxScrollPos := point(StackWidth-AvailableWidth,StackHeight-AvailableHeight);
  if MaxScrollPos.X < 0 then MaxScrollPos.X := 0;
  if MaxScrollPos.Y < 0 then MaxScrollPos.Y := 0;

  //check if scrollbars should be added
  if not AWithHorzScrollBar or not AWithVertScrollBar then
  begin
    NeedHorzScrollBar:= (MaxScrollPos.X > 0);
    NeedVertScrollBar:= (MaxScrollPos.Y > 0);
    if (NeedHorzScrollBar and not AWithHorzScrollBar) or (NeedVertScrollBar and not AWithVertScrollBar) then
    begin
      ComputeScrolling(WithHorzScrollBar or NeedHorzScrollBar, WithVertScrollBar or NeedVertScrollBar);
      exit;
    end;
  end;

  if ScrollStackItemIntoView then
  begin
    ScrollPos.X := 0;
    ScrollPos.Y := (LazPaintInstance.Image.NbLayers-1-LazPaintInstance.Image.CurrentLayerIndex)*LayerRectHeight;
    ScrollStackItemIntoView := false;
  end;

  if ScrollPos.X < 0 then ScrollPos.X := 0;
  if ScrollPos.Y < 0 then ScrollPos.Y := 0;
  if ScrollPos.X > MaxScrollPos.X then ScrollPos.X := MaxScrollPos.X;
  if ScrollPos.Y > MaxScrollPos.Y then ScrollPos.Y := MaxScrollPos.Y;

  if WithHorzScrollBar then
    VolatileHorzScrollBar := TVolatileScrollBar.Create(0,AvailableHeight,AvailableWidth,VolatileScrollBarSize,sbHorizontal,ScrollPos.X,0,MaxScrollPos.X);
  if WithVertScrollBar then
    VolatileVertScrollBar := TVolatileScrollBar.Create(AvailableWidth,0,VolatileScrollBarSize,AvailableHeight,sbVertical,ScrollPos.Y,0,MaxScrollPos.Y);
end;

procedure TFLayerStack.InvalidateStack(AScrollIntoView: boolean);
begin
  if not updatingImageOnly then
  begin
    BGRALayerStack.DiscardBitmap;
    if AScrollIntoView then ScrollStackItemIntoView := true;
    renaming := false;
  end;
end;

procedure TFLayerStack.SetLayerStackScrollPosOnItem(idx: integer);
begin
  ScrollPos.X := 0;
  ScrollPos.Y := (LazPaintInstance.Image.NbLayers-1-Idx)*LayerRectHeight+LayerRectHeight div 2-AvailableHeight div 2;
end;

procedure TFLayerStack.AddButton(AAction: TBasicAction);
var button: TToolButton;
begin
  button := TToolButton.Create(ToolBar2);
  button.Parent := Toolbar2;
  button.Action := AAction;
  button.Style := tbsButton;
end;

procedure TFLayerStack.AddSeparator;
var button: TToolButton;
begin
  button := TToolButton.Create(Toolbar2);
  button.Style := tbsSeparator;
  button.Parent := Toolbar2;
end;

procedure TFLayerStack.BGRALayerStackRedraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  RedrawLayerStack(Bitmap,True,-1);
end;

procedure TFLayerStack.BGRALayerStackResize(Sender: TObject);
begin
  BGRALayerStack.DiscardBitmap;
end;

procedure TFLayerStack.RedrawLayerStack(Bitmap: TBGRABitmap; Layout: boolean; UpdateItem: Integer);
var i: integer;
  layerPos: TPoint;
  lSelected: boolean;
  y: integer;
  clipping, rKind: TRect;
  lColor, lColorTrans: TBGRAPixel;

  procedure DrawKindUnknown;
  var
    eb: TEasyBezierCurve;
    w: single;
    i: integer;
    m: TAffineMatrix;
  begin
    eb := EasyBezierCurve([PointF(0.25,0.25),PointF(0.32,0.07),PointF(0.5,0),PointF(0.68,0.07),PointF(0.75,0.20),
                           PointF(0.75,0.30),PointF(0.70,0.40),PointF(0.5,0.5),PointF(0.5,0.70)],False,cmCurve);
    m := AffineMatrixTranslation(rKind.Left,rKind.Top)*AffineMatrixScale(rKind.Width,rKind.Height);
    for i := 0 to eb.PointCount-1 do eb.Point[i] := m*eb.Point[i];
    w := max(1,rKind.Height/10);
    Bitmap.DrawPolyLineAntialias(eb.ToPoints, lColor, w, true);
    Bitmap.FillEllipseAntialias((rKind.Left+rKind.Right)/2, rKind.Bottom - 1 - (w-1)/2, w*0.6,w*0.6, lColor);
  end;

  procedure DrawKind(AClass: TBGRALayerOriginalAny);
  var
    eb: TEasyBezierCurve;
    w: single;
    i: integer;
    m: TAffineMatrix;
    r: TRect;
  begin
    if AClass = nil then
    begin
      Bitmap.Rectangle(rKind, lColor,lColorTrans, dmDrawWithTransparency);
      Bitmap.HorizLine(rKind.Left+1,rKind.Top+(rKind.Height-1) div 2,rKind.Right-2, lColor, dmDrawWithTransparency);
      Bitmap.VertLine(rKind.Left+(rKind.Width-1) div 2,rKind.Top+1,rKind.Bottom-2, lColor, dmDrawWithTransparency);
    end else
    if AClass = TBGRALayerImageOriginal then
    begin
      r := rect(rKind.Left,(rKind.Top+rKind.Bottom) div 2, (rKind.Left+rKind.Right) div 2, rKind.Bottom);
      w := max(1,rKind.Height/10);
      Bitmap.Rectangle(r, lColor,lColorTrans, dmDrawWithTransparency);
      eb := EasyBezierCurve([PointF(rKind.Left,rKind.Top+rKind.Height/4),
                             PointF(rKind.Left+rKind.Width/2,rKind.Top+rKind.Height/4),
                             PointF(rKind.Left+rKind.Width*3/4,rKind.Top+rKind.Height/2),
                             PointF(rKind.Left+rKind.Width*3/4,rKind.Bottom)],False,cmCurve);
      Bitmap.Arrow.StartAsClassic;
      Bitmap.Arrow.EndAsClassic;
      Bitmap.DrawPolyLineAntialias(eb.ToPoints, lColor, w);
      Bitmap.Arrow.StartAsNone;
      Bitmap.Arrow.EndAsNone;
    end else
    if AClass = TBGRALayerSVGOriginal then
    begin
      m := AffineMatrixTranslation(rKind.Left,rKind.Top+rKind.Height*0.1)*AffineMatrixScale(rKind.Width,rKind.Height*0.8);
      w := max(1,rKind.Height/10);
      eb := EasyBezierCurve([PointF(0.28,0),PointF(0,0),PointF(0,0.5),PointF(0.28,0.5),PointF(1/3,1),PointF(0,1)],False,cmCurve);
      for i := 0 to eb.PointCount-1 do eb.Point[i] := m*eb.Point[i];
      Bitmap.DrawPolyLineAntialias(eb.ToPoints, lColor, w, true);
      eb := EasyBezierCurve([PointF(0.33,0),PointF(0.47,1),PointF(0.6,0)],False,cmAngle);
      for i := 0 to eb.PointCount-1 do eb.Point[i] := m*eb.Point[i];
      Bitmap.DrawPolyLineAntialias(eb.ToPoints, lColor, w, true);
      eb := EasyBezierCurve([PointF(1,0),PointF(0.7,0),PointF(2/3,1),PointF(1,1),PointF(1,0.5),PointF(5/6,0.5)],False,cmCurve);
      eb.CurveMode[eb.PointCount-2] := cmAngle;
      for i := 0 to eb.PointCount-1 do eb.Point[i] := m*eb.Point[i];
      Bitmap.DrawPolyLineAntialias(eb.ToPoints, lColor, w, true);
    end else
    begin
      Bitmap.EllipseAntialias(rKind.Left+rKind.Width / 3, rKind.Top+rKind.Height / 3,rKind.Width / 3,rKind.Height / 3,
                              lColor, 1, lColorTrans);
      Bitmap.DrawPolygonAntialias([PointF(rKind.Left+rKind.Width/4,rKind.Bottom),
                                   PointF(rKind.Left+rKind.Width/2,rKind.Top+rKind.Height/4),
                                   PointF(rKind.Right,rKind.Bottom)],lColor,1, lColorTrans);
    end;
  end;

begin
  if Layout then
  begin
    ComputeLayout(Bitmap);
    UpdateItem := -1;
  end;
  layerPos.x := -Offset.X;
  layerPos.y := -Offset.Y;
  SetLength(interruptors,LazPaintInstance.Image.NbLayers);
  SetLength(LayerInfo,LazPaintInstance.Image.NbLayers);
  clipping := EmptyRect;
  for i := LazPaintInstance.Image.NbLayers-1 downto 0 do
  begin
    if (i = UpdateItem) or (UpdateItem = -1) then
    begin
      with LazPaintInstance.Image do
      begin
        if i = CurrentLayerIndex then
        begin
          Bitmap.FillRect(layerPos.X,layerPos.Y,layerPos.X+StackWidth,layerPos.Y+LayerRectHeight,ColorToBGRA(ColorToRGB(clHighlight)),dmSet);
          lSelected:= true;
        end else
        begin
          if UpdateItem <> -1 then
            Bitmap.FillRect(layerPos.X,layerPos.Y,layerPos.X+StackWidth,layerPos.Y+LayerRectHeight,ColorToBGRA(ColorToRGB(clWindow)),dmSet);
          lSelected:= false;
        end;
        if UpdateItem <> -1 then clipping := rect(layerPos.X,layerPos.Y,layerPos.X+StackWidth,layerPos.Y+LayerRectHeight);

        interruptors[i] := RectWithSize(layerPos.X+InterruptorWidth div 5,layerpos.Y+(LayerRectHeight-5*InterruptorHeight div 2) div 2,
                                        InterruptorWidth, InterruptorHeight);

        if (layerpos.Y+LayerRectHeight > 0) and (layerpos.Y < Bitmap.Height) then
        begin
          if lSelected then
            lColor := ColorToBGRA(ColorToRGB(clHighlightText))
          else
            lColor := ColorToBGRA(ColorToRGB(clWindowText));

          lColorTrans := lColor;
          lColorTrans.alpha := lColorTrans.alpha div 3;

          Bitmap.Rectangle(interruptors[i],lColor,dmDrawWithTransparency);
          if LayerVisible[i] then
          with interruptors[i] do
          begin
            Bitmap.DrawPolyLineAntialias(Bitmap.ComputeBezierSpline([

               BezierCurve(pointF(left+2,top+3),PointF((left+right-1)/2,bottom-3)),

               BezierCurve(PointF((left+right-1)/2,bottom-3),
                  PointF((left+right-1)/2,(top*2+bottom-1)/3),
                  PointF(right-2,top-2))]),lColor,1.5);
          end;

          rKind := interruptors[i];
          rKind.Offset(0, InterruptorHeight*3 div 2);
          if LayerOriginalDefined[i] then
          begin
            if LayerOriginalKnown[i] then
              DrawKind(LayerOriginalClass[i])
            else
              DrawKindUnknown;
          end
          else
            DrawKind(nil);

          inc(layerPos.X,InterruptorWidth);
          if movingItemStart and (i= movingItemSourceIndex) then
          begin
            FreeAndNil(movingItem);
            movingItem := TBGRABitmap.Create(StackWidth-InterruptorWidth,LayerRectHeight,ColorToBGRA(ColorToRGB(clHighlight)));
            movingItem.FontName := Bitmap.FontName;
            movingItem.FontQuality := Bitmap.FontQuality;
            movingItem.FontFullHeight := Bitmap.FontFullHeight;
            DrawLayerItem(movingItem,Point(0,0),i,lSelected);
            movingItemOrigin := point(layerPos.X+Offset.X,layerPos.Y+Offset.Y);
            movingItemStart:= false;
          end;

          LayerInfo[i] := DrawLayerItem(Bitmap,layerPos,i,lSelected);
          dec(layerPos.X,InterruptorWidth);
        end;
      end;
    end;
    inc(layerPos.Y, LayerRectHeight);
  end;

  if (clipping.right > clipping.left) and (clipping.bottom > clipping.top) then Bitmap.ClipRect := clipping;
  if VolatileHorzScrollBar <> nil then VolatileHorzScrollBar.Draw(Bitmap);
  if VolatileVertScrollBar <> nil then VolatileVertScrollBar.Draw(Bitmap);
  if not IsRectEmpty(ScrollButtonRect) then
    Bitmap.FillRect(ScrollButtonRect, ColorToBGRA(ColorToRGB(clBtnFace)), dmSet);
  Bitmap.NoClip;

  if Layout then
  begin
    if (movingItem <> nil) and ((movingItemMousePos.X <> movingItemMouseOrigin.X) or (movingItemMousePos.Y <> movingItemMouseOrigin.Y)) then
    begin
      y := movingItemOrigin.Y + movingItemMousePos.Y - movingItemMouseOrigin.Y - Offset.Y;
      if y < 0 then
      begin
        timerScrollDeltaY := -movingItem.Height div 3;
        TimerScroll.Enabled := true;
      end else
      if y + movingItem.Height > Bitmap.Height then
      begin
        timerScrollDeltaY := +movingItem.Height div 3;
        TimerScroll.Enabled := true;
      end;
      Bitmap.PutImage(movingItemOrigin.X + movingItemMousePos.X - movingItemMouseOrigin.X - Offset.X,
        y,movingItem,dmDrawWithTransparency,128);
    end;

    UpdateComboBlendOp;
  end;
  if not CompletelyResizeable then
  begin
    Bitmap.DrawVertLine(0,0,Bitmap.Height-1, BGRA(0,0,0,128));
    Bitmap.DrawVertLine(Bitmap.Width-1,0,Bitmap.Height-1, BGRA(0,0,0,128));
  end;
  movingItemStart := false;
end;

procedure TFLayerStack.SetCompletelyResizeable(AValue: boolean);
begin
  if FCompletelyResizeable=AValue then Exit;
  FCompletelyResizeable:=AValue;
  if AValue then
  begin
    BorderStyle := bsSizeToolWin;
    BGRALayerStack.Align := alNone;
    Panel_WindowTitle.Visible := false;
    BGRALayerStack.Align := alClient;
  end else
  begin
    BorderStyle := bsNone;
    BGRALayerStack.Align := alNone;
    Panel_WindowTitle.Visible := true;
    BGRALayerStack.Align := alClient;
  end;
end;

procedure TFLayerStack.UpdateImage;
begin
  updatingImageOnly := true;
  LazPaintInstance.NotifyImageChangeCompletely(True);
  updatingImageOnly := false;
end;

procedure TFLayerStack.OnImageChangedHandler(
  AEvent: TLazPaintImageObservationEvent);
begin
  if not AEvent.DelayedStackUpdate then InvalidateStack(False);
end;

procedure TFLayerStack.SelectBlendOp;
var blendOp: TBlendOperation;
  topmostInfo: TTopMostInfo;
  tempUnder: TBGRABitmap;
begin
  blendOp := boTransparent;
  topmostInfo := LazPaintInstance.HideTopmost;
  if LazPaintInstance.Image.CurrentLayerIndex > 0 then
    tempUnder := LazPaintInstance.Image.ComputeFlatImage(0,LazPaintInstance.Image.CurrentLayerIndex-1,False)
  else
    tempUnder := TBGRABitmap.Create(1,1);
  if ublendop.ShowBlendOpDialog(LazPaintInstance, blendOp, tempUnder,LazPaintInstance.Image.CurrentLayerReadOnly) then
  begin
    updatingImageOnly := true;
    LazPaintInstance.Image.BlendOperation[LazPaintInstance.Image.CurrentLayerIndex] := blendOp;
    updatingImageOnly := false;
    UpdateComboBlendOp;
  end;
  tempUnder.Free;
  LazPaintInstance.ShowTopmost(topmostInfo);
  if LazPaintInstance.Image.CurrentLayerIndex = 0 then
    LazPaintInstance.ToolManager.ToolPopup(tpmBlendOpBackground);
end;

procedure TFLayerStack.DoScrollVertically(AAmount: integer);
var prevY: integer;
begin
  prevY := scrollPos.Y;
  ScrollPos.Y += AAmount;
  if ScrollPos.Y < 0 then ScrollPos.Y := 0;
  if ScrollPos.Y > MaxScrollPos.Y then ScrollPos.Y := MaxScrollPos.Y;
  movingItemMouseOrigin.Y -= ScrollPos.Y-prevY;
  BGRALayerStack.DiscardBitmap;
end;

procedure TFLayerStack.ComboBox_BlendOpChange(Sender: TObject);
var blendOp: TBlendOperation;
  itemStr: string;
begin
  if not UpdatingComboBlendOp then
  begin
    if ComboBox_BlendOp.ItemIndex <> -1 then
    begin
      itemStr := ComboBox_BlendOp.Items[ComboBox_BlendOp.ItemIndex];
      if itemStr <> rsOtherBlendOp then
      begin
        if itemStr = rsNormalBlendOp then
          blendOp := boTransparent
        else
          blendOp := StrToBlendOperation(itemStr);
        updatingImageOnly := true;
        LazPaintInstance.Image.BlendOperation[LazPaintInstance.Image.CurrentLayerIndex] := blendOp;
        if LazPaintInstance.Image.CurrentLayerIndex = 0 then
          LazPaintInstance.ToolManager.ToolPopup(tpmBlendOpBackground);
        updatingImageOnly := false;
      end else
        SelectBlendOp;
    end;
  end;
end;

procedure TFLayerStack.FormActivate(Sender: TObject);
begin
  Panel_WindowTitle.Background.Color := clActiveCaption;
  Panel_WindowTitle.FontEx.Color := clCaptionText;
end;

procedure TFLayerStack.BGRALayerStackMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i: integer;
  str: string;
begin
  if PtInRect(Point(X,Y),ScrollButtonRect) then exit;
  If (Button = mbLeft) then
  begin
    if ((VolatileHorzScrollBar <> nil) and VolatileHorzScrollBar.MouseDown(X,Y)) or
      ((VolatileVertScrollBar <> nil) and VolatileVertScrollBar.MouseDown(X,Y)) then
    begin
      if VolatileHorzScrollBar <> nil then ScrollPos.X := VolatileHorzScrollBar.Position;
      if VolatileVertScrollBar <> nil then ScrollPos.Y := VolatileVertScrollBar.Position;
      BGRALayerStack.RedrawBitmap;
      exit;
    end;
    for i := 0 to high(interruptors) do
      if PtInRect(Point(x,Y),interruptors[i]) then
      begin
        if i < LazPaintInstance.Image.NbLayers then
        begin
          updatingImageOnly:= true;
          LazPaintInstance.Image.LayerVisible[i] := not LazPaintInstance.Image.LayerVisible[i];
          updatingImageOnly:= false;
          UpdateLayerStackItem(i);
        end;
        exit;
      end;
    for i := 0 to high(LayerInfo) do
      if IsPointInPolygon(LayerInfo[i].PreviewPts,pointF(x,y),true) then
      begin
        HandleSelectLayer(i,x,y);
        exit;
      end;
    for i := 0 to high(LayerInfo) do
      if PtInRect(Point(x,Y),LayerInfo[i].NameRect) then
      begin
        if i < LazPaintInstance.Image.NbLayers then
        begin
          if (i <> LazPaintInstance.image.CurrentLayerIndex) and not renaming then
            HandleSelectLayer(i,x,y)
          else
          begin
            renaming := true;
            str := InputBox(self.Caption,rsEnterLayerName,LazPaintInstance.Image.LayerName[i]);
            if str <> '' then
            begin
              if length(str) > MaxLayerNameLength then str := copy(str,1,MaxLayerNameLength);
              LazPaintInstance.Image.LayerName[i] := str;
            end;
            BGRALayerStack.RedrawBitmap; //layer stack width may change
          end;
        end;
        exit;
      end;
    for i := 0 to high(LayerInfo) do
      if PtInRect(Point(x,Y),LayerInfo[i].OpacityBar) or PtInRect(Point(x+4,Y),LayerInfo[i].OpacityBar) or
        PtInRect(Point(x-4,Y),LayerInfo[i].OpacityBar) then
      begin
        if i < LazPaintInstance.Image.NbLayers then
        begin
          changingLayerOpacity := i;
          HandleChangeLayerOpacity(X,Y);
        end;
        exit;
      end;
  end;
end;

procedure TFLayerStack.BGRALayerStackMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if movingItem <> nil then
  begin
    movingItemMousePos := point(X,Y);
    BGRALayerStack.DiscardBitmap;
    exit;
  end;
  if ((VolatileVertScrollBar <> nil) and VolatileVertScrollBar.MouseMove(X,Y)) or
    ((VolatileHorzScrollBar <> nil) and VolatileHorzScrollBar.MouseMove(X,Y)) then
  begin
    if VolatileHorzScrollBar <> nil then ScrollPos.X := VolatileHorzScrollBar.Position;
    if VolatileVertScrollBar <> nil then ScrollPos.Y := VolatileVertScrollBar.Position;
    BGRALayerStack.DiscardBitmap;
    exit;
  end;
  if changingLayerOpacity <> -1 then
  begin
    HandleChangeLayerOpacity(X,Y);
    exit;
  end;
end;

procedure TFLayerStack.BGRALayerStackMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var destinationIndex: integer;
  indexF: single;
begin
  if Button = mbLeft then
  begin
    movingItemStart := false;
    if movingItem <> nil then
    begin
      FreeAndNil(movingItem);
      indexF := LazPaintInstance.Image.NbLayers-1 - (movingItemOrigin.Y+movingItemMousePos.Y-movingItemMouseOrigin.Y)/LayerRectHeight;
      if indexF < movingItemSourceIndex-1.15 then indexF += 0.15 else
      if indexF > movingItemSourceIndex+1.15 then indexF -= 0.15;
      destinationIndex := Int32or64(round(indexF));
      if destinationIndex = -1 then destinationIndex := 0;
      if destinationIndex = LazPaintInstance.Image.NbLayers then destinationIndex := LazPaintInstance.Image.NbLayers-1;
      if (destinationIndex >= 0) and (destinationIndex < LazPaintInstance.Image.NbLayers) and (destinationIndex <> movingItemSourceIndex) then
      begin
        updatingImageOnly:= true;
        LazPaintInstance.Image.MoveLayer(movingItemSourceIndex, destinationIndex);
        updatingImageOnly:= false;
        UpdateImage;
      end;
      BGRALayerStack.RedrawBitmap;
    end;
    if ((VolatileVertScrollBar <> nil) and VolatileVertScrollBar.MouseUp(X,Y)) or
      ((VolatileHorzScrollBar <> nil) and VolatileHorzScrollBar.MouseUp(X,Y)) then
    begin
      BGRALayerStack.RedrawBitmap;
      exit;
    end;
    if changingLayerOpacity <> -1 then changingLayerOpacity := -1;
  end;
end;

procedure TFLayerStack.BGRALayerStackMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  DoScrollVertically(round(-WheelDelta*ZoomFactor*50/120));
end;

{$R *.lfm}

end.

