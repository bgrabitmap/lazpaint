unit ulayerstack;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, BGRAVirtualScreen, LazPaintType, BGRABitmap,
  UVolatileScrollBar, BGRAGradients, BGRABitmapTypes;

const MaxLayersToAdd = 99;

type
  TDrawLayerItemResult = record
    PreviewPts: array of TPointF;
    NameRect,OpacityBar: TRect;
  end;

  { TFLayerStack }

  TFLayerStack = class(TForm)
    BGRALayerStack: TBGRAVirtualScreen;
    ComboBox_BlendOp: TComboBox;
    Panel1: TPanel;
    TimerScroll: TTimer;
    ToolBar1: TToolBar;
    ToolButton_AddNewLayer: TToolButton;
    ToolButton_RemoveLayer: TToolButton;
    ToolZoomLayerStackIn: TToolButton;
    ToolZoomLayerStackOut: TToolButton;
    procedure BGRALayerStackMouseDown(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure BGRALayerStackMouseMove(Sender: TObject; {%H-}Shift: TShiftState; X,
      Y: Integer);
    procedure BGRALayerStackMouseUp(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure BGRALayerStackRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure ComboBox_BlendOpChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerScrollTimer(Sender: TObject);
    procedure ToolButton_AddNewLayerClick(Sender: TObject);
    procedure ToolButton_RemoveLayerClick(Sender: TObject);
    procedure ToolZoomLayerStackInClick(Sender: TObject);
    procedure ToolZoomLayerStackOutClick(Sender: TObject);
    procedure HandleChangeLayerOpacity(X,{%H-}Y: integer);
    procedure HandleSelectLayer(i,x,y: integer);
    procedure UpdateComboBlendOp;
    procedure UpdateLayerStackItem(idx: integer);
  private
    { private declarations }
    UpdatingComboBlendOp : boolean;
    background: TBGRABitmap;
    ZoomFactor: single;
    renaming: boolean;
    LayerRectWidth,LayerRectHeight, StackWidth,StackHeight: integer;
    Offset,ScrollPos,MaxScrollPos: TPoint;
    AvailableWidth,AvailableHeight: integer;
    changingLayerOpacity: integer;
    VolatileHorzScrollBar, VolatileVertScrollBar: TVolatileScrollBar;
    InterruptorWidth,InterruptorHeight: integer;
    interruptors: array of TRect;
    LayerInfo: array of TDrawLayerItemResult;
    phong: TPhongShading;
    movingItemStart: boolean;
    movingItem: TBGRABitmap;
    movingItemSourceIndex: integer;
    movingItemMousePos,movingItemMouseOrigin,movingItemOrigin: TPoint;
    timerScrollDeltaY: integer;
    updatingImage: boolean;
    ScrollStackItemIntoView : boolean;
    procedure ComputeLayout(ABitmap: TBGRABitmap);
    procedure ComputeScrolling(AWithHorzScrollBar,AWithVertScrollBar: boolean);
    function DrawLayerItem(ABitmap: TBGRABitmap; layerPos: TPoint;
      layerIndex: integer; ASelected: boolean): TDrawLayerItemResult;
    procedure RedrawLayerStack(Bitmap: TBGRABitmap; Layout: boolean; UpdateItem: Integer);
    procedure UpdateImage;
  public
    { public declarations }
    LazPaintInstance: TLazPaintCustomInstance;
    procedure InvalidateStack(AScrollIntoView: boolean);
  end;

implementation

uses BGRAFillInfo,uscaledpi,uresourcestrings,ublendop;

function TFLayerStack.DrawLayerItem(ABitmap: TBGRABitmap; layerPos: TPoint; layerIndex: integer; ASelected: boolean): TDrawLayerItemResult;
var LayerBmp: TBGRABitmap;
    lColor,lColorTransp: TBGRAPixel;
    barwidth: integer;
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

  result.PreviewPts[0].y += 0.5;
  result.PreviewPts[1].y += 0.5;
  result.PreviewPts[2].y -= 0.5;
  result.PreviewPts[3].y -= 0.5;
  ABitmap.FillPolyAntialias(result.PreviewPts,background);
  layerBmp := LazPaintInstance.Image.currentLayeredBitmap.LayerBitmap[layerIndex];
  ABitmap.FillPolyLinearMapping( result.PreviewPts, layerBmp, [pointf(-0.49,-0.49),pointf(layerBmp.Width-0.51,-0.49),
    pointf(layerBmp.Width-0.51,layerBmp.Height-0.51),pointf(-0.49,layerBmp.Height-0.51)],False);
  result.PreviewPts[0].y -= 0.5;
  result.PreviewPts[1].y -= 0.5;
  result.PreviewPts[2].y += 0.5;
  result.PreviewPts[3].y += 0.5;
  if not LazPaintInstance.Image.currentLayeredBitmap.LayerVisible[LayerIndex] then ABitmap.CustomPenStyle := BGRAPenStyle(LayerRectHeight/8,LayerRectHeight/8);
  ABitmap.DrawPolygonAntialias( result.PreviewPts, lColor,1);
  ABitmap.PenStyle := psSolid;
  if ASelected then
  begin
    result.NameRect := rect(layerpos.X+round(LayerRectWidth*0.95),layerpos.Y,layerpos.X+StackWidth,layerPos.Y+LayerRectHeight div 2);
    barwidth := StackWidth-InterruptorWidth-round(LayerRectWidth*1.1);
    if barwidth > LayerRectWidth then barwidth := LayerRectWidth;
    result.OpacityBar := rect(layerpos.X+LayerRectWidth,layerpos.Y+LayerRectHeight div 2,layerpos.X+LayerRectWidth+barwidth,layerpos.Y+LayerRectHeight);
    ABitmap.Rectangle(result.OpacityBar.left,(result.OpacityBar.top*3+result.OpacityBar.bottom) div 4,result.OpacityBar.right,
        (result.OpacityBar.top+result.OpacityBar.bottom*3) div 4,lColor,dmSet);
    ABitmap.FillRect(result.OpacityBar.left+1,(result.OpacityBar.top*3+result.OpacityBar.bottom) div 4+1,result.OpacityBar.left+1+
      round((result.OpacityBar.right-result.OpacityBar.left-2)*LazPaintInstance.Image.currentLayeredBitmap.LayerOpacity[LayerIndex]/255),
       (result.OpacityBar.top+result.OpacityBar.bottom*3) div 4-1,lColorTransp,dmDrawWithTransparency);
  end
  else
  begin
    result.NameRect := rect(layerpos.X+round(LayerRectWidth*0.95),layerpos.Y,layerpos.X+StackWidth,layerPos.Y+LayerRectHeight);
    result.OpacityBar := EmptyRect;
  end;
  ABitmap.TextOut(result.NameRect.Left,result.NameRect.Top+(result.NameRect.bottom-result.NameRect.top -ABitmap.FontFullHeight) div 2,
    LazPaintInstance.Image.currentLayeredBitmap.LayerName[layerIndex],lColor);
end;

{ TFLayerStack }

procedure TFLayerStack.FormCreate(Sender: TObject);
begin
  ScaleDPI(Self,OriginalDPI);
  Position := poDesigned;
  ZoomFactor := 1;
  ScrollPos := point(0,0);
  VolatileHorzScrollBar := nil;
  VolatileVertScrollBar := nil;
  changingLayerOpacity:= -1;
  self.EnsureVisible(False);
  background := TBGRABitmap.Create(4,4,ColorToBGRA(ColorToRGB(clWindow)));
  background.FillRect(0,0,2,2,BGRA(0,0,0,64),dmDrawWithTransparency);
  background.FillRect(2,2,4,4,BGRA(0,0,0,64),dmDrawWithTransparency);
  phong := TPhongShading.Create;
  renaming := false;
  Visible := false;
  movingItemStart := false;
end;

procedure TFLayerStack.FormDestroy(Sender: TObject);
begin
  phong.Free;
  background.Free;
  FreeAndNil(movingItem);
  FreeAndNil(VolatileHorzScrollBar);
  FreeAndNil(VolatileVertScrollBar);
end;

procedure TFLayerStack.TimerScrollTimer(Sender: TObject);
var prevY: integer;
begin
  prevY := scrollPos.Y;
  ScrollPos.Y += TimerScrollDeltaY;
  if ScrollPos.Y < 0 then ScrollPos.Y := 0;
  if ScrollPos.Y > MaxScrollPos.Y then ScrollPos.Y := MaxScrollPos.Y;
  movingItemMouseOrigin.Y -= ScrollPos.Y-prevY;
  TimerScroll.Enabled := False;
  BGRALayerStack.RedrawBitmap;
end;

procedure TFLayerStack.ToolButton_AddNewLayerClick(Sender: TObject);
begin
  if LazPaintInstance.image.NbLayers < MaxLayersToAdd then
  begin
    LazPaintInstance.Image.AddNewLayer;
    ScrollPos.X := 0;
    ScrollPos.Y := 0;
    UpdateImage;
    BGRALayerStack.RedrawBitmap;
  end;
end;

procedure TFLayerStack.ToolButton_RemoveLayerClick(Sender: TObject);
var idx: integer;
begin
  if (LazPaintInstance.Image.currentImageLayer <> nil) and (LazPaintInstance.image.NbLayers > 1) then
  begin
    idx := LazPaintInstance.Image.currentImageLayerIndex;
    LazPaintInstance.Image.currentLayeredBitmap.RemoveLayer(idx);
    if idx >= LazPaintInstance.image.NbLayers then
      idx := LazPaintInstance.image.NbLayers-1;
    LazPaintInstance.Image.currentImageLayer := LazPaintInstance.Image.currentLayeredBitmap.LayerBitmap[idx];
    ScrollPos.X := 0;
    ScrollPos.Y := (LazPaintInstance.Image.NbLayers-1-Idx)*LayerRectHeight+LayerRectHeight div 2-AvailableHeight div 2;
    UpdateImage;
    BGRALayerStack.RedrawBitmap;
  end;
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
    if LazPaintInstance.Image.currentLayeredBitmap.LayerOpacity[changingLayerOpacity] = newOpacity then exit;
    LazPaintInstance.Image.currentLayeredBitmap.LayerOpacity[changingLayerOpacity] := newOpacity;
    UpdateLayerStackItem(changingLayerOpacity);
    UpdateImage;
  end;
end;

procedure TFLayerStack.HandleSelectLayer(i,x,y: integer);
begin
  if i < LazPaintInstance.Image.currentLayeredBitmap.NbLayers then
  begin
    LazPaintInstance.Image.currentImageLayer := LazPaintInstance.Image.currentLayeredBitmap.LayerBitmap[i];
    renaming := false;
    movingItemStart := true;
    movingItemSourceIndex := i;
    movingItemMouseOrigin := point(x,y);
    movingItemMousePos := point(x,y);
    UpdateImage;
    BGRALayerStack.RedrawBitmap;
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
  with LazPaintInstance.Image.currentLayeredBitmap do
    for i := 0 to NbLayers-1 do
    begin
      str := BlendOperationStr[BlendOperation[i]];
      if blendOps.IndexOf(str) = -1 then
        blendOps.Add(str);
      if i = LazPaintInstance.Image.currentImageLayerIndex then
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
  temp := ScaleY(20,OriginalDPI);
  if InterruptorWidth > temp then InterruptorWidth := temp;
  if InterruptorHeight > temp then InterruptorHeight := temp;
  temp := ScaleY(10,OriginalDPI);
  if InterruptorHeight < temp then InterruptorHeight := temp;
  if InterruptorWidth < temp then InterruptorWidth := temp;
  StackWidth := InterruptorWidth+LayerRectWidth+ABitmap.TextSize('Layer').cx;
  StackHeight := LayerRectHeight*LazPaintInstance.Image.NbLayers;
  for i := 0 to LazPaintInstance.Image.NbLayers-1 do
  begin
    temp := InterruptorWidth+LayerRectWidth+ABitmap.TextSize(LazPaintInstance.Image.currentLayeredBitmap.LayerName[i]).cx;
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
    ScrollPos.Y := (LazPaintInstance.Image.NbLayers-1-LazPaintInstance.Image.currentImageLayerIndex)*LayerRectHeight;
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
  if not updatingImage then
  begin
    BGRALayerStack.DiscardBitmap;
    if AScrollIntoView then ScrollStackItemIntoView := true;
    renaming := false;
  end;
end;

procedure TFLayerStack.BGRALayerStackRedraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  RedrawLayerStack(Bitmap,True,-1);
end;

procedure TFLayerStack.RedrawLayerStack(Bitmap: TBGRABitmap; Layout: boolean; UpdateItem: Integer);
var i: integer;
  layerPos: TPoint;
  lSelected: boolean;
  y: integer;
  clipping: TRect;
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
      with LazPaintInstance.Image.currentLayeredBitmap do
      begin
        if LayerBitmap[i] = LazPaintInstance.Image.currentImageLayer then
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

        interruptors[i] := rect(layerPos.X+InterruptorWidth div 5,layerpos.Y+(LayerRectHeight-InterruptorHeight) div 2,layerPos.X+InterruptorWidth,
           layerpos.Y+(LayerRectHeight-InterruptorHeight) div 2+InterruptorHeight);

        if (layerpos.Y+LayerRectHeight > 0) and (layerpos.Y < Bitmap.Height) then
        begin
          phong.DrawRectangle(Bitmap, interruptors[i],1,-1, ApplyLightnessFast(ColorToBGRA(ColorToRGB(clBtnFace)),16384),False,[]);
          if LayerVisible[i] then
            phong.DrawRectangle(Bitmap, rect((interruptors[i].Left*2 + interruptors[i].Right) div 3+1,interruptors[i].top+1,interruptors[i].right+1,interruptors[i].Bottom-1),1,3, ColorToBGRA(ColorToRGB(clBtnFace)),False,[rmoLinearBorder])
          else
            phong.DrawRectangle(Bitmap, rect(interruptors[i].left-1,interruptors[i].top+1,(interruptors[i].Left + interruptors[i].Right*2) div 3-1,interruptors[i].Bottom-1),1,3, ColorToBGRA(ColorToRGB(clBtnFace)),False,[rmoLinearBorder]);

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
  Bitmap.NoClip;

  if Layout then
  begin
    if (movingItem <> nil) and ((movingItemMousePos.X <> movingItemMouseOrigin.X) or (movingItemMousePos.Y <> movingItemMouseOrigin.Y)) then
    begin
      y := movingItemOrigin.Y + movingItemMousePos.Y - movingItemMouseOrigin.Y - Offset.Y;
      if y < 0 then
      begin
        timerScrollDeltaY := -movingItem.Height div 10;
        TimerScroll.Enabled := true;
      end else
      if y + movingItem.Height > Bitmap.Height then
      begin
        timerScrollDeltaY := +movingItem.Height div 10;
        TimerScroll.Enabled := true;
      end;
      Bitmap.PutImage(movingItemOrigin.X + movingItemMousePos.X - movingItemMouseOrigin.X - Offset.X,
        y,movingItem,dmDrawWithTransparency,128);
    end;

    UpdateComboBlendOp;
    ToolButton_AddNewLayer.Enabled := LazPaintInstance.Image.NbLayers < MaxLayersToAdd;
    ToolButton_RemoveLayer.Enabled := LazPaintInstance.Image.NbLayers > 1;
  end;

  movingItemStart := false;
end;

procedure TFLayerStack.UpdateImage;
begin
  updatingImage := true;
  LazPaintInstance.NotifyImageChangeCompletely(True);
  updatingImage := false;
end;

procedure TFLayerStack.ComboBox_BlendOpChange(Sender: TObject);
var blendOp: TBlendOperation;
  itemStr: string;
  tempUnder: TBGRABitmap;
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
        LazPaintInstance.Image.currentLayeredBitmap.BlendOperation[LazPaintInstance.Image.currentImageLayerIndex] := blendOp;
        UpdateImage;
      end else
      begin
        blendOp := boTransparent;
        LazPaintInstance.HideTopmost;
        if LazPaintInstance.Image.currentImageLayerIndex > 0 then
          tempUnder := LazPaintInstance.Image.currentLayeredBitmap.ComputeFlatImage(0,LazPaintInstance.Image.currentImageLayerIndex-1)
        else
          tempUnder := TBGRABitmap.Create(1,1);
        if ublendop.ShowBlendOpDialog(blendOp, tempUnder,LazPaintInstance.Image.currentImageLayer) then
        begin
          LazPaintInstance.Image.currentLayeredBitmap.BlendOperation[LazPaintInstance.Image.currentImageLayerIndex] := blendOp;
          UpdateImage;
          UpdateComboBlendOp;
        end;
        tempUnder.Free;
        LazPaintInstance.ShowTopmost;
      end;
    end;
  end;
end;

procedure TFLayerStack.BGRALayerStackMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i: integer;
  str: string;
begin
  If (Button = mbLeft) then
  begin
    if ((VolatileHorzScrollBar <> nil) and VolatileHorzScrollBar.MouseDown(X,Y)) or
      ((VolatileVertScrollBar <> nil) and VolatileVertScrollBar.MouseDown(X,Y)) then
    begin
      BGRALayerStack.RedrawBitmap;
      exit;
    end;
    for i := 0 to high(interruptors) do
      if PtInRect(Point(x,Y),interruptors[i]) then
      begin
        if i < LazPaintInstance.Image.currentLayeredBitmap.NbLayers then
        begin
          LazPaintInstance.Image.currentLayeredBitmap.LayerVisible[i] := not LazPaintInstance.Image.currentLayeredBitmap.LayerVisible[i];
          UpdateLayerStackItem(i);
          UpdateImage;
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
        if i < LazPaintInstance.Image.currentLayeredBitmap.NbLayers then
        begin
          if (i <> LazPaintInstance.image.currentImageLayerIndex) and not renaming then
            HandleSelectLayer(i,x,y)
          else
          begin
            renaming := true;
            str := InputBox(self.Caption,rsEnterLayerName,LazPaintInstance.Image.currentLayeredBitmap.LayerName[i]);
            if str <> '' then
              LazPaintInstance.Image.currentLayeredBitmap.LayerName[i] := str;
            BGRALayerStack.RedrawBitmap;
          end;
        end;
        exit;
      end;
    for i := 0 to high(LayerInfo) do
      if PtInRect(Point(x,Y),LayerInfo[i].OpacityBar) or PtInRect(Point(x+4,Y),LayerInfo[i].OpacityBar) or
        PtInRect(Point(x-4,Y),LayerInfo[i].OpacityBar) then
      begin
        if i < LazPaintInstance.Image.currentLayeredBitmap.NbLayers then
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
    BGRALayerStack.RedrawBitmap;
    exit;
  end;
  if ((VolatileVertScrollBar <> nil) and VolatileVertScrollBar.MouseMove(X,Y)) or
    ((VolatileHorzScrollBar <> nil) and VolatileHorzScrollBar.MouseMove(X,Y)) then
  begin
    if VolatileHorzScrollBar <> nil then ScrollPos.X := VolatileHorzScrollBar.Position;
    if VolatileVertScrollBar <> nil then ScrollPos.Y := VolatileVertScrollBar.Position;
    BGRALayerStack.RedrawBitmap;
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
begin
  if Button = mbLeft then
  begin
    movingItemStart := false;
    if movingItem <> nil then
    begin
      FreeAndNil(movingItem);
      destinationIndex := LazPaintInstance.Image.currentLayeredBitmap.NbLayers-1 - round((movingItemOrigin.Y+movingItemMousePos.Y-movingItemMouseOrigin.Y)/LayerRectHeight);
      if destinationIndex = -1 then destinationIndex := 0;
      if destinationIndex = LazPaintInstance.Image.currentLayeredBitmap.NbLayers then destinationIndex := LazPaintInstance.Image.currentLayeredBitmap.NbLayers-1;
      if (destinationIndex >= 0) and (destinationIndex < LazPaintInstance.Image.currentLayeredBitmap.NbLayers) and (destinationIndex <> movingItemSourceIndex) then
      begin
        LazPaintInstance.Image.currentLayeredBitmap.InsertLayer(destinationIndex, movingItemSourceIndex);
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

initialization
  {$I ulayerstack.lrs}

end.

