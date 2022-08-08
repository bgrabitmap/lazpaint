// SPDX-License-Identifier: GPL-3.0-only
unit ULayerStackInterface;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ComCtrls, ExtCtrls, BGRAVirtualScreen, BCComboBox,
  BGRABitmap, BGRABitmapTypes, BGRAGraphics, Menus,
  LazPaintType, UDarkTheme, UVolatileScrollBar, UImageObservation;

type
  TDrawLayerItemResult = record
    PreviewPts: array of TPointF;
    NameRect,OpacityBar: TRect;
  end;
  TLayerItemInfo = record
    FullRect: TRect;
    RightPart: TDrawLayerItemResult;
    VisibleCheckbox: TRect;
    KindIcon: TRect;
    KindIconHint: string;
  end;

  { TLayerStackInterface }

  TLayerStackInterface = class
  private
    LazPaintInstance: TLazPaintCustomInstance;
    Container: TWinControl;
    BGRALayerStack: TBGRAVirtualScreen;
    TimerScroll, TimerQuery: TTimer;
    PanelToolbar: TPanel;
    Toolbar: TToolbar;
    ComboBox_BlendOp: TBCComboBox;
    VolatileHorzScrollBar, VolatileVertScrollBar: TVolatileScrollBar;
    procedure BGRALayerStack_MouseDown(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure BGRALayerStack_MouseMove(Sender: TObject; {%H-}Shift: TShiftState; X,
      Y: Integer);
    procedure BGRALayerStack_MouseUp(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure BGRALayerStack_MouseWheel(Sender: TObject; {%H-}Shift: TShiftState;
      WheelDelta: Integer; {%H-}MousePos: TPoint; var Handled: Boolean);
    procedure BGRALayerStack_Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure ComboBox_BlendOpChange(Sender: TObject);
    procedure TimerQuery_Timer(Sender: TObject);
    procedure TimerScroll_Timer(Sender: TObject);
    procedure Toolbar_Resize(Sender: TObject);
    procedure ToolSelectBlendOperation_Click(Sender: TObject);
    procedure ToolZoomLayerStackIn_Click(Sender: TObject);
    procedure ToolZoomLayerStackOut_Click(Sender: TObject);
    procedure LazPaint_ImageChanged(AEvent: TLazPaintImageObservationEvent);
  protected
    FDPI: integer;
    FScaling: Double;
    FDarkTheme: boolean;
    FScrollStackItemIntoView: Boolean;
    FUpdatingComboBlendOp, FQuerySelectBlendOp: Boolean;
    FPartialRedraw: Boolean;
    FRenaming: Boolean;
    FDontUpdateStack: Boolean;
    FZoomFactor: single;
    FRegularBackground, FIconBackground: TBGRABitmap;
    FLayerRectWidth, FLayerRectHeight, FStackWidth, FStackHeight: integer;
    FOffset, FScrollPos, FMaxScrollPos: TPoint;
    FAvailableWidth, FAvailableHeight: integer;
    FChangingLayerOpacityIndex, FAskTransferSelectionLayerIndex: integer;
    FScrollButtonRect: TRect;
    FInterruptorWidth,FInterruptorHeight: integer;
    FLayerInfo: array of TLayerItemInfo;
    FMovingItemStart: boolean;
    FMovingItemBitmap: TBGRABitmap;
    FMovingItemSourceIndex: integer;
    FMovingItemMousePos, FMovingItemMouseOrigin, FMovingItemOrigin: TPoint;
    FRightClickOrigin: TPoint;
    FTimerScrollDeltaY: integer;
    FInHandleSelectLayer: Boolean;
    FLayerMenu: TPopupMenu;
    FQueryLayerMenu: boolean;
    FLayerMenuCoord: TPoint;
    procedure ApplyThemeAndDPI;
    procedure SetDPI(AValue: integer);
    procedure SetDarkTheme(AValue: boolean);
    function GetBackColor(ASelected: boolean): TColor;
    function GetTextColor(ASelected: boolean): TColor;
    procedure UpdateComboBlendOp;
    procedure SelectBlendOp;
    procedure QuerySelectBlendOp;
    procedure SetZoomFactor(AValue: single);
    function DrawLayerItem(ABitmap: TBGRABitmap; ALayerPos: TPoint; ALayerIndex: integer; ASelected: boolean): TDrawLayerItemResult;
    procedure DrawLayerStack(ABitmap: TBGRABitmap; ALayout: boolean; AUpdateItem: Integer);
    procedure ComputeLayout(ABitmap: TBGRABitmap);
    procedure ComputeScrolling(AWithHorzScrollBar,AWithVertScrollBar: boolean);
    procedure DoScrollVertically(AAmount: integer);
    function HandleSelectLayer(i,x,y: integer; AStartMoving: boolean = true): boolean;
    procedure HandleChangeLayerOpacity(X,{%H-}Y: integer);
    procedure UpdateLayerStackItem(AIndex: integer);
    procedure NeedCheckers;
  public
    constructor Create(AContainer: TWinControl; AInstance: TLazPaintCustomInstance);
    destructor Destroy; override;
    procedure AddButton(AAction: TBasicAction);
    procedure AddButton(ACaption: string; AImageIndex: integer; AOnClick: TNotifyEvent);
    procedure AddLayerMenu(AAction: TBasicAction);
    procedure ScrollToItem(AIndex: integer; AUpdateStack: boolean = true);
    procedure InvalidateStack(AScrollIntoView: boolean);
    function GetWidthFor(AButtonCount: integer): integer;
    property DPI: integer read FDPI write SetDPI;
    property DarkTheme: boolean read FDarkTheme write SetDarkTheme;
    property ZoomFactor: single read FZoomFactor write SetZoomFactor;
  end;

implementation

uses Dialogs, LCScaleDPI, GraphType, Graphics, Toolwin, Forms,
  BGRAFillInfo, BGRATransform, BGRALayerOriginal, BGRASVGOriginal, BGRAThumbnail,
  UTool, UImage, UBlendOp, UResourceStrings, math;

{ TLayerStackInterface }

procedure TLayerStackInterface.SetDarkTheme(AValue: boolean);
begin
  if FDarkTheme=AValue then Exit;
  FDarkTheme:=AValue;
  ApplyThemeAndDPI;
end;

procedure TLayerStackInterface.ToolSelectBlendOperation_Click(Sender: TObject);
begin
  SelectBlendOp;
end;

procedure TLayerStackInterface.ComboBox_BlendOpChange(Sender: TObject);
var blendOp: TBlendOperation;
  itemStr: string;
begin
  if Assigned(LazPaintInstance) then
    LazPaintInstance.ExitColorEditor;
  if not FUpdatingComboBlendOp then
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
        FDontUpdateStack := true;
        LazPaintInstance.Image.BlendOperation[LazPaintInstance.Image.CurrentLayerIndex] := blendOp;
        if LazPaintInstance.Image.CurrentLayerIndex = 0 then
          LazPaintInstance.ToolManager.ToolPopup(tpmBlendOpBackground);
        FDontUpdateStack := false;
        FQuerySelectBlendOp:= false;
      end else
        QuerySelectBlendOp;
    end;
  end;
end;

procedure TLayerStackInterface.TimerQuery_Timer(Sender: TObject);
begin
  if FQuerySelectBlendOp then
  begin
    FQuerySelectBlendOp := false;
    SelectBlendOp;
  end else
  if FQueryLayerMenu then
  begin
    FQueryLayerMenu := false;
    with FLayerMenuCoord do
      FLayerMenu.PopUp(X, Y);
  end
  else
    TimerQuery.Enabled:= false;
end;

procedure TLayerStackInterface.TimerScroll_Timer(Sender: TObject);
begin
  TimerScroll.Enabled := False;
  DoScrollVertically(FTimerScrollDeltaY);
end;

procedure TLayerStackInterface.Toolbar_Resize(Sender: TObject);
begin
  PanelToolbar.ClientHeight := Toolbar.Height + PanelToolbar.ChildSizing.TopBottomSpacing*2;
end;

procedure TLayerStackInterface.BGRALayerStack_Redraw(Sender: TObject;
  Bitmap: TBGRABitmap);
begin
  TVolatileScrollBar.InitDPI((Sender as TControl).GetCanvasScaleFactor);
  DrawLayerStack(Bitmap, not FPartialRedraw, -1);
end;

procedure TLayerStackInterface.BGRALayerStack_MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i: integer;
  str: string;
  isRightClick: boolean;
begin
  if Assigned(LazPaintInstance) then LazPaintInstance.ExitColorEditor;
  X := round(X*FScaling);
  Y := round(Y*FScaling);
  if PtInRect(Point(X,Y),FScrollButtonRect) then exit;
  isRightClick := (Button = mbRight) {$IFDEF DARWIN}or ((Button = mbLeft) and (ssCtrl in Shift)){$ENDIF};
  If (Button = mbLeft) and not isRightClick then
  begin
    FRightClickOrigin := EmptyPoint;
    if ((VolatileHorzScrollBar <> nil) and VolatileHorzScrollBar.MouseDown(X,Y)) or
      ((VolatileVertScrollBar <> nil) and VolatileVertScrollBar.MouseDown(X,Y)) then
    begin
      if VolatileHorzScrollBar <> nil then FScrollPos.X := VolatileHorzScrollBar.Position;
      if VolatileVertScrollBar <> nil then FScrollPos.Y := VolatileVertScrollBar.Position;
      BGRALayerStack.RedrawBitmap;
      exit;
    end;
    for i := 0 to high(FLayerInfo) do
      if PtInRect(Point(x,Y),FLayerInfo[i].VisibleCheckbox) then
      begin
        if i < LazPaintInstance.Image.NbLayers then
        begin
          FDontUpdateStack:= true;
          LazPaintInstance.Image.LayerVisible[i] := not LazPaintInstance.Image.LayerVisible[i];
          FDontUpdateStack:= false;
          UpdateLayerStackItem(i);
        end;
        exit;
      end;
    for i := 0 to high(FLayerInfo) do
      if IsPointInPolygon(FLayerInfo[i].RightPart.PreviewPts,pointF(x,y),true) then
      begin
        HandleSelectLayer(i,x,y);
        exit;
      end;
    for i := 0 to high(FLayerInfo) do
      if PtInRect(Point(x,Y),FLayerInfo[i].RightPart.NameRect) then
      begin
        if i < LazPaintInstance.Image.NbLayers then
        begin
          if (i <> LazPaintInstance.image.CurrentLayerIndex) and not FRenaming then
            HandleSelectLayer(i,x,y)
          else
          begin
            FRenaming := true;
            str := InputBox(rsLayers, rsEnterLayerName, LazPaintInstance.Image.LayerName[i]);
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
    for i := 0 to high(FLayerInfo) do
      if PtInRect(Point(x,Y),FLayerInfo[i].RightPart.OpacityBar) or PtInRect(Point(x+4,Y),FLayerInfo[i].RightPart.OpacityBar) or
        PtInRect(Point(x-4,Y),FLayerInfo[i].RightPart.OpacityBar) then
      begin
        if i < LazPaintInstance.Image.NbLayers then
        begin
          FChangingLayerOpacityIndex := i;
          HandleChangeLayerOpacity(X,Y);
        end;
        exit;
      end;
  end else
  if isRightClick then
    FRightClickOrigin := Point(X,Y);
end;

procedure TLayerStackInterface.BGRALayerStack_MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
begin
  X := round(X*FScaling);
  Y := round(Y*FScaling);
  if FMovingItemBitmap <> nil then
  begin
    FMovingItemMousePos := point(X,Y);
    BGRALayerStack.DiscardBitmap;
    exit;
  end;
  if ((VolatileVertScrollBar <> nil) and VolatileVertScrollBar.MouseMove(X,Y)) or
    ((VolatileHorzScrollBar <> nil) and VolatileHorzScrollBar.MouseMove(X,Y)) then
  begin
    if VolatileHorzScrollBar <> nil then FScrollPos.X := VolatileHorzScrollBar.Position;
    if VolatileVertScrollBar <> nil then FScrollPos.Y := VolatileVertScrollBar.Position;
    BGRALayerStack.DiscardBitmap;
    exit;
  end;
  if FChangingLayerOpacityIndex <> -1 then
  begin
    HandleChangeLayerOpacity(X,Y);
    exit;
  end;
  for i := 0 to high(FLayerInfo) do
    if FLayerInfo[i].KindIcon.Contains(Point(X,Y)) then
    begin
      BGRALayerStack.Hint := FLayerInfo[i].KindIconHint;
      BGRALayerStack.ShowHint:= true;
      exit;
    end else
    if FLayerInfo[i].VisibleCheckbox.Contains(Point(X,Y)) then
    begin
      BGRALayerStack.Hint := rsVisible;
      BGRALayerStack.ShowHint:= true;
      exit;
    end else
    if FLayerInfo[i].RightPart.OpacityBar.Contains(Point(X,Y)) then
    begin
      BGRALayerStack.Hint := rsOpacity;
      BGRALayerStack.ShowHint:= true;
      exit;
    end;
  BGRALayerStack.ShowHint:= false;
end;

procedure TLayerStackInterface.BGRALayerStack_MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var destinationIndex, prevIndex, i: integer;
  indexF: single;
  res: TModalResult;
  topmostInfo: TTopMostInfo;
  isRightClick: Boolean;
begin
  X := round(X*FScaling);
  Y := round(Y*FScaling);
  isRightClick := (Button = mbRight) {$IFDEF DARWIN}or ((Button = mbLeft) and not IsEmptyPoint(FRightClickOrigin)){$ENDIF};
  if (Button = mbLeft) and not isRightClick then
  begin
    FMovingItemStart := false;
    if FMovingItemBitmap <> nil then
    begin
      FreeAndNil(FMovingItemBitmap);
      indexF := LazPaintInstance.Image.NbLayers-1 - (FMovingItemOrigin.Y+FMovingItemMousePos.Y-FMovingItemMouseOrigin.Y)/FLayerRectHeight;
      if indexF < FMovingItemSourceIndex-1.15 then indexF += 0.15 else
      if indexF > FMovingItemSourceIndex+1.15 then indexF -= 0.15;
      destinationIndex := Int32or64(round(indexF));
      if destinationIndex = -1 then destinationIndex := 0;
      if destinationIndex = LazPaintInstance.Image.NbLayers then destinationIndex := LazPaintInstance.Image.NbLayers-1;
      if (destinationIndex >= 0) and (destinationIndex < LazPaintInstance.Image.NbLayers) and (destinationIndex <> FMovingItemSourceIndex) then
      begin
        FDontUpdateStack:= true;
        LazPaintInstance.Image.MoveLayer(FMovingItemSourceIndex, destinationIndex);
        FDontUpdateStack:= false;
      end;
      BGRALayerStack.RedrawBitmap;
    end;
    if ((VolatileVertScrollBar <> nil) and VolatileVertScrollBar.MouseUp(X,Y)) or
      ((VolatileHorzScrollBar <> nil) and VolatileHorzScrollBar.MouseUp(X,Y)) then
    begin
      BGRALayerStack.RedrawBitmap;
      exit;
    end;
    if FChangingLayerOpacityIndex <> -1 then FChangingLayerOpacityIndex := -1;
    if FAskTransferSelectionLayerIndex <> -1 then
    begin
      FInHandleSelectLayer := true;
      topmostInfo := LazPaintInstance.HideTopmost;
      res := MessageDlg(rsTransferSelectionToOtherLayer,mtConfirmation,[mbOk,mbCancel],0);
      LazPaintInstance.ShowTopmost(topmostInfo);
      if res = mrOk then
      begin
        prevIndex := LazPaintInstance.Image.CurrentLayerIndex;
        if LazPaintInstance.Image.SetCurrentLayerByIndex(FAskTransferSelectionLayerIndex) then
        begin
          FRenaming := false;
          UpdateLayerStackItem(prevIndex);
          UpdateLayerStackItem(FAskTransferSelectionLayerIndex);
        end;
      end;
      FInHandleSelectLayer := false;
      FAskTransferSelectionLayerIndex := -1;
    end;
  end else
  if isRightClick and (Abs(X - FRightClickOrigin.X) <= 2) and
    (Abs(Y - FRightClickOrigin.Y) <= 2) then
  begin
    for i := 0 to high(FLayerInfo) do
      if IsPointInPolygon(FLayerInfo[i].RightPart.PreviewPts,pointF(FRightClickOrigin.x,FRightClickOrigin.y),true) then
      begin
        if HandleSelectLayer(i,x,y,false) then
        begin
          FQueryLayerMenu := true;
          FLayerMenuCoord := BGRALayerStack.ClientToScreen(Point(
            round(FRightClickOrigin.X / FScaling),
            round(FRightClickOrigin.Y / FScaling)));
          TimerQuery.Enabled:= true;
        end;
        exit;
      end;
  end;
end;

procedure TLayerStackInterface.BGRALayerStack_MouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  DoScrollVertically(round(-WheelDelta*ZoomFactor*50/120*FScaling));
  Handled := true;
end;

procedure TLayerStackInterface.SetZoomFactor(AValue: single);
var
  prevZoom: Single;
begin
  if AValue > 10 then AValue := 10;
  if AValue < 1/2 then AValue := 1/2;
  if FZoomFactor=AValue then Exit;
  prevZoom := FZoomFactor;
  FZoomFactor := AValue;
  if prevZoom > 0 then
    FScrollPos.Y := round(FScrollPos.Y * FZoomFactor / prevZoom);
  BGRALayerStack.DiscardBitmap;
end;

function TLayerStackInterface.DrawLayerItem(ABitmap: TBGRABitmap;
  ALayerPos: TPoint; ALayerIndex: integer; ASelected: boolean): TDrawLayerItemResult;
var
  lColor: TBGRAPixel;
  barwidth: integer;
  sourceCoords: Array Of TPointF;
  reduced, bmpOpacity: TBGRABitmap;
  reducedBounds, rOpacity, rFill: TRect;
  percentStr: String;
begin
  lColor := GetTextColor(ASelected);

  result.PreviewPts := PointsF([pointf(ALayerPos.X+0.25*FLayerRectWidth,ALayerPos.Y+round(FLayerRectHeight*0.1)),
     pointf(ALayerPos.X+0.9*FLayerRectWidth,ALayerPos.Y+round(FLayerRectHeight*0.1)),
    pointf(ALayerPos.X+0.7*FLayerRectWidth,ALayerPos.Y+round(FLayerRectHeight*0.9)),
    pointf(ALayerPos.X+0.05*FLayerRectWidth,ALayerPos.Y+round(FLayerRectHeight*0.9))]);
  reduced := TBGRABitmap.Create(round(FLayerRectWidth*0.65), round(FLayerRectHeight*0.8));
  reducedBounds := RectWithSize(LazPaintInstance.Image.LayerOffset[ALayerIndex].X,
                        LazPaintInstance.Image.LayerOffset[ALayerIndex].Y,
                        LazPaintInstance.Image.LayerBitmap[ALayerIndex].Width,
                        LazPaintInstance.Image.LayerBitmap[ALayerIndex].Height);
  if LazPaintInstance.Image.Width <> 0 then
  begin
    reducedBounds.Left := round(reducedBounds.Left*reduced.Width/LazPaintInstance.Image.Width);
    reducedBounds.Right := round(reducedBounds.Right*reduced.Width/LazPaintInstance.Image.Width);
  end;
  if LazPaintInstance.Image.Height <> 0 then
  begin
    reducedBounds.Top := round(reducedBounds.Top*reduced.Height/LazPaintInstance.Image.Height);
    reducedBounds.Bottom := round(reducedBounds.Bottom*reduced.Height/LazPaintInstance.Image.Height);
  end;
  reduced.StretchPutImage(reducedBounds, LazPaintInstance.Image.LayerBitmap[ALayerIndex], dmDrawWithTransparency);

  result.PreviewPts[0].y += 0.5;
  result.PreviewPts[1].y += 0.5;
  result.PreviewPts[2].y -= 0.5;
  result.PreviewPts[3].y -= 0.5;
  NeedCheckers;
  if LazPaintInstance.Image.IsIconCursor then
    ABitmap.FillPolyAntialias(result.PreviewPts, FIconBackground)
  else
    ABitmap.FillPolyAntialias(result.PreviewPts, FRegularBackground);

  sourceCoords := PointsF([pointf(-0.49,-0.49),pointf(reduced.Width-0.51,-0.49),
            pointf(reduced.Width-0.51,reduced.Height-0.51),pointf(-0.49,reduced.Height-0.51)]);
  ABitmap.FillPolyLinearMapping(result.PreviewPts, reduced, sourceCoords, False);
  reduced.Free;

  result.PreviewPts[0].y -= 0.5;
  result.PreviewPts[1].y -= 0.5;
  result.PreviewPts[2].y += 0.5;
  result.PreviewPts[3].y += 0.5;
  if not LazPaintInstance.Image.LayerVisible[ALayerIndex] then ABitmap.CustomPenStyle := BGRAPenStyle(FLayerRectHeight/8, FLayerRectHeight/8);
  ABitmap.DrawPolygonAntialias( result.PreviewPts, lColor,1);
  ABitmap.PenStyle := psSolid;
  if ASelected then
  begin
    result.NameRect := rect(ALayerPos.X+round(FLayerRectWidth*0.95), ALayerPos.Y,
                            ALayerPos.X+FStackWidth, ALayerPos.Y + FLayerRectHeight div 2);
    barwidth := FStackWidth - FInterruptorWidth - round(FLayerRectWidth*0.92)- ABitmap.FontFullHeight div 2;
    result.OpacityBar := rect(ALayerPos.X + round(FLayerRectWidth*0.92), ALayerPos.Y + FLayerRectHeight div 2,
                              ALayerPos.X + round(FLayerRectWidth*0.92)+barwidth, ALayerPos.Y+FLayerRectHeight);
    rOpacity := rect(result.OpacityBar.left,(result.OpacityBar.top*7+result.OpacityBar.bottom) div 8,
                     result.OpacityBar.right,(result.OpacityBar.top+result.OpacityBar.bottom*7) div 8);
    ABitmap.Rectangle(rOpacity,lColor,dmSet);
    rOpacity.Inflate(-1,-1);
    bmpOpacity := TBGRABitmap.Create(rOpacity.Width,rOpacity.Height,BGRABlack);
    rFill := rect(0,0,round(bmpOpacity.Width*LazPaintInstance.Image.LayerOpacity[ALayerIndex]/255),bmpOpacity.Height);
    bmpOpacity.ClipRect := rFill;
    bmpOpacity.FillRect(rFill, CSSSilver,dmSet);
    bmpOpacity.FontFullHeight := min(bmpOpacity.Height, ABitmap.FontFullHeight*2);
    bmpOpacity.FontName := ABitmap.FontName;
    bmpOpacity.FontStyle:= [fsBold];
    bmpOpacity.FontVerticalAnchor:= fvaCenter;
    percentStr := IntToStr(round(LazPaintInstance.Image.LayerOpacity[ALayerIndex]*100/255))+'%';
    bmpOpacity.TextOut(bmpOpacity.Width/2,bmpOpacity.Height/2, percentStr, BGRABlack, taCenter);
    bmpOpacity.ClipRect := rect(rFill.Right,rFill.Top,bmpOpacity.Width,rFill.Bottom);
    bmpOpacity.TextOut(bmpOpacity.Width/2,bmpOpacity.Height/2, percentStr, BGRAWhite, taCenter);
    ABitmap.FillMask(rOpacity.Left,rOpacity.Top,bmpOpacity, lColor, dmDrawWithTransparency);
    bmpOpacity.Free;
  end
  else
  begin
    result.NameRect := rect(ALayerPos.X + round(FLayerRectWidth*0.95), ALayerPos.Y,
                            ALayerPos.X + FStackWidth, ALayerPos.Y + FLayerRectHeight);
    result.OpacityBar := EmptyRect;
  end;
  {$IFDEF DARWIN}
  ABitmap.FontQuality := fqFineAntialiasing;
  {$ENDIF}
  ABitmap.TextOut(result.NameRect.Left,result.NameRect.Top+(result.NameRect.bottom-result.NameRect.top -ABitmap.FontFullHeight) div 2,
    LazPaintInstance.Image.LayerName[ALayerIndex],lColor);
end;

procedure TLayerStackInterface.DrawLayerStack(ABitmap: TBGRABitmap;
  ALayout: boolean; AUpdateItem: Integer);
var i: integer;
  layerPos: TPoint;
  lSelected: boolean;
  y: integer;
  clipping, prevClip: TRect;
  lColor, lColorTrans: TBGRAPixel;
  penWidth: single;

  procedure DrawKindUnknown(rKind: TRect; out HintText: string);
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
    ABitmap.DrawPolyLineAntialias(eb.ToPoints, lColor, w, true);
    ABitmap.FillEllipseAntialias((rKind.Left+rKind.Right)/2, rKind.Bottom - 1 - (w-1)/2, w*0.6,w*0.6, lColor);
    HintText := rsUnknownOriginal;
  end;

  procedure DrawKind(AClass: TBGRALayerOriginalAny; rKind: TRect; out HintText: string);
  var
    eb: TEasyBezierCurve;
    w: single;
    i: integer;
    m: TAffineMatrix;
    r: TRect;
  begin
    if AClass = nil then
    begin
      ABitmap.Rectangle(rKind, lColor,lColorTrans, dmDrawWithTransparency);
      ABitmap.HorizLine(rKind.Left+1,rKind.Top+(rKind.Height-1) div 2,rKind.Right-2, lColor, dmDrawWithTransparency);
      ABitmap.VertLine(rKind.Left+(rKind.Width-1) div 2,rKind.Top+1,rKind.Bottom-2, lColor, dmDrawWithTransparency);
      HintText := rsRasterLayer;
    end else
    if AClass = TBGRALayerImageOriginal then
    begin
      r := rect(rKind.Left,(rKind.Top+rKind.Bottom) div 2, (rKind.Left+rKind.Right) div 2, rKind.Bottom);
      w := max(1,rKind.Height/10);
      ABitmap.Rectangle(r, lColor,lColorTrans, dmDrawWithTransparency);
      eb := EasyBezierCurve([PointF(rKind.Left,rKind.Top+rKind.Height/4),
                             PointF(rKind.Left+rKind.Width/2,rKind.Top+rKind.Height/4),
                             PointF(rKind.Left+rKind.Width*3/4,rKind.Top+rKind.Height/2),
                             PointF(rKind.Left+rKind.Width*3/4,rKind.Bottom)],False,cmCurve);
      ABitmap.Arrow.StartAsClassic;
      ABitmap.Arrow.EndAsClassic;
      ABitmap.DrawPolyLineAntialias(eb.ToPoints, lColor, w);
      ABitmap.Arrow.StartAsNone;
      ABitmap.Arrow.EndAsNone;
      HintText := rsTransformedRasterLayer;
    end else
    if AClass = TBGRALayerSVGOriginal then
    begin
      m := AffineMatrixTranslation(rKind.Left,rKind.Top+rKind.Height*0.1)*AffineMatrixScale(rKind.Width,rKind.Height*0.8);
      w := max(1,rKind.Height/10);
      eb := EasyBezierCurve([PointF(0.28,0),PointF(0,0),PointF(0,0.5),PointF(0.28,0.5),PointF(1/3,1),PointF(0,1)],False,cmCurve);
      for i := 0 to eb.PointCount-1 do eb.Point[i] := m*eb.Point[i];
      ABitmap.DrawPolyLineAntialias(eb.ToPoints, lColor, w, true);
      eb := EasyBezierCurve([PointF(0.33,0),PointF(0.47,1),PointF(0.6,0)],False,cmAngle);
      for i := 0 to eb.PointCount-1 do eb.Point[i] := m*eb.Point[i];
      ABitmap.DrawPolyLineAntialias(eb.ToPoints, lColor, w, true);
      eb := EasyBezierCurve([PointF(1,0),PointF(0.7,0),PointF(2/3,1),PointF(1,1),PointF(1,0.5),PointF(5/6,0.5)],False,cmCurve);
      eb.CurveMode[eb.PointCount-2] := cmAngle;
      for i := 0 to eb.PointCount-1 do eb.Point[i] := m*eb.Point[i];
      ABitmap.DrawPolyLineAntialias(eb.ToPoints, lColor, w, true);
      HintText := rsVectorialLayer;
    end else
    begin
      ABitmap.EllipseAntialias(rKind.Left+rKind.Width / 3, rKind.Top+rKind.Height / 3,rKind.Width / 3,rKind.Height / 3,
                              lColor, penWidth, lColorTrans);
      ABitmap.DrawPolygonAntialias([PointF(rKind.Left+rKind.Width/4,rKind.Bottom),
                                   PointF(rKind.Left+rKind.Width/2,rKind.Top+rKind.Height/4),
                                   PointF(rKind.Right,rKind.Bottom)],lColor,penWidth, lColorTrans);
      HintText := rsVectorialLayer;
    end;
  end;

begin
  if ALayout then
  begin
    ComputeLayout(ABitmap);
    AUpdateItem := -1;
  end;
  penWidth := 1.25/50*FLayerRectHeight;
  if penWidth < 1 then penWidth := 1;
  if penWidth > 3 then penWidth := 3;
  layerPos.x := -FOffset.X;
  layerPos.y := -FOffset.Y;
  SetLength(FLayerInfo, LazPaintInstance.Image.NbLayers);
  clipping := EmptyRect;
  for i := LazPaintInstance.Image.NbLayers-1 downto 0 do
  begin
    if (i = AUpdateItem) or (AUpdateItem = -1) then
    begin
      with LazPaintInstance.Image do
      begin
        FLayerInfo[i].FullRect := rect(layerPos.X, layerPos.Y,
                                       layerPos.X + FStackWidth, layerPos.Y + FLayerRectHeight);

        if i = CurrentLayerIndex then
        begin
          ABitmap.FillRect(FLayerInfo[i].FullRect,
                           GetBackColor(true),dmSet);
          lSelected:= true;
        end else
        begin
          if AUpdateItem <> -1 then
            ABitmap.FillRect(FLayerInfo[i].FullRect,
                             GetBackColor(false),dmSet);
          lSelected:= false;
        end;
        if AUpdateItem <> -1 then
          clipping := FLayerInfo[i].FullRect;

        FLayerInfo[i].VisibleCheckbox := RectWithSize(layerPos.X + FInterruptorWidth div 5, layerpos.Y + (FLayerRectHeight-5*FInterruptorHeight div 2) div 2,
                                        FInterruptorWidth, FInterruptorHeight);

        if FLayerInfo[i].FullRect.IntersectsWith(ABitmap.ClipRect) then
        begin
          lColor := GetTextColor(lSelected);

          lColorTrans := lColor;
          lColorTrans.alpha := lColorTrans.alpha div 3;

          with FLayerInfo[i].VisibleCheckbox do
            ABitmap.RectangleAntialias(left, top, right-1, bottom-1, lColor, penWidth);
          if LayerVisible[i] then
          with FLayerInfo[i].VisibleCheckbox do
          begin
            ABitmap.DrawPolyLineAntialias(ABitmap.ComputeBezierSpline([

               BezierCurve(pointF(left+2,top+3),PointF((left+right-1)/2,bottom-3)),

               BezierCurve(PointF((left+right-1)/2,bottom-3),
                  PointF((left+right-1)/2,(top*2+bottom-1)/3),
                  PointF(right-2,top-2))]),lColor, penWidth);
          end;

          FLayerInfo[i].KindIcon := FLayerInfo[i].VisibleCheckbox;
          FLayerInfo[i].KindIcon.Offset(0, FInterruptorHeight*3 div 2);
          if LayerOriginalDefined[i] then
          begin
            if LayerOriginalKnown[i] then
              DrawKind(LayerOriginalClass[i], FLayerInfo[i].KindIcon, FLayerInfo[i].KindIconHint)
            else
              DrawKindUnknown(FLayerInfo[i].KindIcon, FLayerInfo[i].KindIconHint);
          end
          else
            DrawKind(nil, FLayerInfo[i].KindIcon, FLayerInfo[i].KindIconHint);

          inc(layerPos.X, FInterruptorWidth);
          if FMovingItemStart and (i = FMovingItemSourceIndex) then
          begin
            FreeAndNil(FMovingItemBitmap);
            FMovingItemBitmap := TBGRABitmap.Create(FStackWidth - FInterruptorWidth, FLayerRectHeight, GetBackColor(true));
            FMovingItemBitmap.FontName := ABitmap.FontName;
            FMovingItemBitmap.FontQuality := ABitmap.FontQuality;
            FMovingItemBitmap.FontFullHeight := ABitmap.FontFullHeight;
            DrawLayerItem(FMovingItemBitmap, Point(0,0), i, lSelected);
            FMovingItemOrigin := point(layerPos.X + FOffset.X, layerPos.Y + FOffset.Y);
            FMovingItemStart:= false;
          end;

          FLayerInfo[i].RightPart := DrawLayerItem(ABitmap,layerPos,i,lSelected);
          dec(layerPos.X, FInterruptorWidth);
        end;
      end;
    end;
    inc(layerPos.Y, FLayerRectHeight);
  end;
  ABitmap.HorizLine(0, 0, ABitmap.Width-1, DarkThemeInstance.GetColorButtonFace(DarkTheme),
    dmDrawWithTransparency, 32768);

  prevClip := ABitmap.ClipRect;
  if (clipping.right > clipping.left) and (clipping.bottom > clipping.top) then
    ABitmap.IntersectClip(clipping);
  if VolatileHorzScrollBar <> nil then VolatileHorzScrollBar.Draw(ABitmap);
  if VolatileVertScrollBar <> nil then VolatileVertScrollBar.Draw(ABitmap);
  if not FScrollButtonRect.IsEmpty then
    ABitmap.FillRect(FScrollButtonRect, ColorToBGRA(ColorToRGB(clBtnFace)), dmSet);
  ABitmap.ClipRect := prevClip;

  if ALayout then
  begin
    if (FMovingItemBitmap <> nil) and ((FMovingItemMousePos.X <> FMovingItemMouseOrigin.X) or (FMovingItemMousePos.Y <> FMovingItemMouseOrigin.Y)) then
    begin
      y := FMovingItemOrigin.Y + FMovingItemMousePos.Y - FMovingItemMouseOrigin.Y - FOffset.Y;
      if y < 0 then
      begin
        FTimerScrollDeltaY := -FMovingItemBitmap.Height div 3;
        TimerScroll.Enabled := true;
      end else
      if y + FMovingItemBitmap.Height > ABitmap.Height then
      begin
        FTimerScrollDeltaY := +FMovingItemBitmap.Height div 3;
        TimerScroll.Enabled := true;
      end;
      ABitmap.PutImage(FMovingItemOrigin.X + FMovingItemMousePos.X - FMovingItemMouseOrigin.X - FOffset.X,
                       y, FMovingItemBitmap, dmDrawWithTransparency,128);
    end;
  end;
  FMovingItemStart := false;
end;

procedure TLayerStackInterface.ComputeLayout(ABitmap: TBGRABitmap);
var i,temp,h: integer;
begin
  FScaling := Container.GetCanvasScaleFactor;
  FLayerInfo := nil;
  FLayerRectWidth := round(100*zoomFactor*FScaling);
  FLayerRectHeight := round(50*zoomFactor*FScaling);
  ABitmap.FontName := 'Arial';
  ABitmap.FontQuality := fqSystemClearType;

  temp := ScaleY(round(20*FScaling),OriginalDPI);
  h := FLayerRectHeight div 3;
  if h > temp then h := temp;
  temp := ScaleY(round(12*FScaling),OriginalDPI);
  if h < temp then h := temp;
  ABitmap.FontFullHeight := h;

  FInterruptorWidth := FLayerRectHeight div 4;
  FInterruptorHeight := FLayerRectHeight div 4;
  temp := ScaleY(round(28*FScaling),OriginalDPI);
  if FInterruptorWidth > temp then FInterruptorWidth := temp;
  if FInterruptorHeight > temp then FInterruptorHeight := temp;
  temp := ScaleY(round(7*FScaling),OriginalDPI);
  if FInterruptorHeight < temp then FInterruptorHeight := temp;
  if FInterruptorWidth < temp then FInterruptorWidth := temp;
  FStackWidth := FInterruptorWidth + FLayerRectWidth + FInterruptorWidth*6;
  FStackHeight := FLayerRectHeight * LazPaintInstance.Image.NbLayers;
  for i := 0 to LazPaintInstance.Image.NbLayers-1 do
  begin
    temp := FInterruptorWidth+ FLayerRectWidth+ ABitmap.TextSize(LazPaintInstance.Image.LayerName[i]).cx;
    if temp > FStackWidth then FStackWidth := temp;
  end;
  if ((VolatileHorzScrollBar = nil) or not VolatileHorzScrollBar.ScrollThumbDown) and
    ((VolatileVertScrollBar = nil) or not VolatileVertScrollBar.ScrollThumbDown) then
    ComputeScrolling(False, False);
  FOffset := FScrollPos;
  if FStackHeight < FAvailableHeight then FOffset.Y := -(FAvailableHeight-FStackHeight);

  if (VolatileHorzScrollBar <> nil) and (VolatileVertScrollBar <> nil) then
    FScrollButtonRect := rect(FAvailableWidth,FAvailableHeight,FAvailableWidth+VolatileScrollBarSize,FAvailableHeight+VolatileScrollBarSize)
  else
    FScrollButtonRect := EmptyRect;
end;

procedure TLayerStackInterface.ComputeScrolling(AWithHorzScrollBar,
  AWithVertScrollBar: boolean);
var
  NeedHorzScrollBar,NeedVertScrollBar: boolean;
  WithHorzScrollBar,WithVertScrollBar: boolean;
begin
  FreeAndNil(VolatileHorzScrollBar);
  FreeAndNil(VolatileVertScrollBar);
  WithHorzScrollBar:= AWithHorzScrollBar;
  WithVertScrollBar:= AWithVertScrollBar;
  FAvailableWidth := round(BGRALayerStack.Width*FScaling);
  FAvailableHeight := round(BGRALayerStack.Height*FScaling);
  if FAvailableWidth <= VolatileThumbSize then WithHorzScrollBar := false;
  if FAvailableHeight <= VolatileThumbSize then WithVertScrollBar := false;
  if FAvailableWidth <= VolatileScrollBarSize then WithVertScrollBar:= false;
  if FAvailableHeight <= VolatileScrollBarSize then WithHorzScrollBar := false;
  if WithVertScrollBar then dec(FAvailableWidth, VolatileScrollBarSize);
  if FAvailableWidth <= VolatileThumbSize then WithHorzScrollBar := false;
  if WithHorzScrollBar then dec(FAvailableHeight, VolatileScrollBarSize);
  if FAvailableHeight <= VolatileThumbSize then
  begin
    WithVertScrollBar := false;
    FAvailableWidth := BGRALayerStack.Width;
  end;

  FMaxScrollPos := point(FStackWidth-FAvailableWidth,FStackHeight-FAvailableHeight);
  if FMaxScrollPos.X < 0 then FMaxScrollPos.X := 0;
  if FMaxScrollPos.Y < 0 then FMaxScrollPos.Y := 0;

  //check if scrollbars should be added
  if not AWithHorzScrollBar or not AWithVertScrollBar then
  begin
    NeedHorzScrollBar:= (FMaxScrollPos.X > 0);
    NeedVertScrollBar:= (FMaxScrollPos.Y > 0);
    if (NeedHorzScrollBar and not AWithHorzScrollBar) or (NeedVertScrollBar and not AWithVertScrollBar) then
    begin
      ComputeScrolling(WithHorzScrollBar or NeedHorzScrollBar, WithVertScrollBar or NeedVertScrollBar);
      exit;
    end;
  end;

  if FScrollStackItemIntoView then
  begin
    FScrollPos.X := 0;
    FScrollPos.Y := (LazPaintInstance.Image.NbLayers-1-LazPaintInstance.Image.CurrentLayerIndex)*FLayerRectHeight;
    FScrollStackItemIntoView := false;
  end;

  if FScrollPos.X < 0 then FScrollPos.X := 0;
  if FScrollPos.Y < 0 then FScrollPos.Y := 0;
  if FScrollPos.X > FMaxScrollPos.X then FScrollPos.X := FMaxScrollPos.X;
  if FScrollPos.Y > FMaxScrollPos.Y then FScrollPos.Y := FMaxScrollPos.Y;

  if WithHorzScrollBar then
    VolatileHorzScrollBar := TVolatileScrollBar.Create(0,FAvailableHeight,FAvailableWidth,VolatileScrollBarSize,sbHorizontal,FScrollPos.X,0,FMaxScrollPos.X);
  if WithVertScrollBar then
    VolatileVertScrollBar := TVolatileScrollBar.Create(FAvailableWidth,0,VolatileScrollBarSize,FAvailableHeight,sbVertical,FScrollPos.Y,0,FMaxScrollPos.Y);
end;

procedure TLayerStackInterface.DoScrollVertically(AAmount: integer);
var prevY: integer;
begin
  prevY := FScrollPos.Y;
  FScrollPos.Y += AAmount;
  if FScrollPos.Y < 0 then FScrollPos.Y := 0;
  if FScrollPos.Y > FMaxScrollPos.Y then FScrollPos.Y := FMaxScrollPos.Y;
  if FScrollPos.Y <> prevY then
  begin
    FMovingItemMouseOrigin.Y -= FScrollPos.Y-prevY;
    BGRALayerStack.DiscardBitmap;
  end;
end;

function TLayerStackInterface.HandleSelectLayer(i, x, y: integer; AStartMoving: boolean): boolean;
var prevIndex: integer;
begin
  result := false;
  FInHandleSelectLayer := true;
  if (i < LazPaintInstance.Image.NbLayers) then
  begin
    prevIndex := LazPaintInstance.Image.CurrentLayerIndex;
    if not LazPaintInstance.Image.SelectionLayerIsEmpty and
      (i <> LazPaintInstance.Image.CurrentLayerIndex) then
    begin
      FAskTransferSelectionLayerIndex := i;
      exit;
    end;
    if LazPaintInstance.Image.SetCurrentLayerByIndex(i) then
    begin
      FRenaming := false;
      UpdateLayerStackItem(prevIndex);
      if AStartMoving then
      begin
        FMovingItemStart := true;
        FMovingItemSourceIndex := i;
        FMovingItemMouseOrigin := point(x,y);
        FMovingItemMousePos := point(x,y);
      end;
      UpdateLayerStackItem(i);
      result := true;
    end;
  end;
  FInHandleSelectLayer := false;
end;

procedure TLayerStackInterface.HandleChangeLayerOpacity(X, Y: integer);
var newOpacity: integer;
begin
  if (FChangingLayerOpacityIndex <> -1) and (FChangingLayerOpacityIndex <= high(FLayerInfo)) then
  with FLayerInfo[FChangingLayerOpacityIndex].RightPart do
  begin
    if FChangingLayerOpacityIndex >= LazPaintInstance.Image.NbLayers then exit;
    newOpacity := round((X-(OpacityBar.left+1))/(OpacityBar.right-OpacityBar.left-2)*255);
    if newOpacity < 0 then newOpacity:= 0;
    if newOpacity > 255 then newOpacity:= 255;
    if LazPaintInstance.Image.LayerOpacity[FChangingLayerOpacityIndex] = newOpacity then exit;
    FDontUpdateStack := true;
    LazPaintInstance.Image.LayerOpacity[FChangingLayerOpacityIndex] := newOpacity;
    FDontUpdateStack := false;
    UpdateLayerStackItem(FChangingLayerOpacityIndex);
  end;
end;

procedure TLayerStackInterface.UpdateLayerStackItem(AIndex: integer);
begin
  if Assigned(BGRALayerStack.Bitmap) then
  begin
    FPartialRedraw := true;
    if (AIndex >= 0) and (AIndex < length(FLayerInfo)) then
      BGRALayerStack.RedrawBitmap(FLayerInfo[AIndex].FullRect);
    FPartialRedraw := false;
  end;
end;

procedure TLayerStackInterface.NeedCheckers;
var
  checkerSize: Integer;
begin
  checkerSize := DoScaleX(round(2*FScaling), OriginalDPI, DPI);
  if Assigned(FRegularBackground) and (FRegularBackground.Width <> checkerSize*2) then
  begin
    FreeAndNil(FRegularBackground);
    FreeAndNil(FIconBackground);
  end;
  if FRegularBackground = nil then
  begin
    FRegularBackground := TBGRABitmap.Create(checkerSize*2,checkerSize*2, ImageCheckersColor1);
    FRegularBackground.FillRect(0,0, checkerSize,checkerSize, ImageCheckersColor2, dmDrawWithTransparency);
    FRegularBackground.FillRect(checkerSize,checkerSize, checkerSize*2,checkerSize*2, ImageCheckersColor2, dmDrawWithTransparency);
    FIconBackground := TBGRABitmap.Create(checkerSize*2,checkerSize*2, IconCheckersColor1);
    FIconBackground.FillRect(0,0, checkerSize,checkerSize, IconCheckersColor2, dmDrawWithTransparency);
    FIconBackground.FillRect(checkerSize,checkerSize, checkerSize*2,checkerSize*2, IconCheckersColor2, dmDrawWithTransparency);
  end;
end;

procedure TLayerStackInterface.LazPaint_ImageChanged(
  AEvent: TLazPaintImageObservationEvent);
begin
  UpdateComboBlendOp;

  if not AEvent.DelayedStackUpdate and
     not FInHandleSelectLayer then InvalidateStack(False);
end;

procedure TLayerStackInterface.UpdateComboBlendOp;
var
  blendOps: TStringList;
  str,selectedStr: string;
  i: integer;
begin
  if FUpdatingComboBlendOp then exit;
  FUpdatingComboBlendOp := true;
  FQuerySelectBlendOp:= false;
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
  FUpdatingComboBlendOp := false;
end;

procedure TLayerStackInterface.SelectBlendOp;
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
  if UBlendOp.ShowBlendOpDialog(LazPaintInstance, blendOp, tempUnder,LazPaintInstance.Image.CurrentLayerReadOnly) then
  begin
    FDontUpdateStack := true;
    LazPaintInstance.Image.BlendOperation[LazPaintInstance.Image.CurrentLayerIndex] := blendOp;
    FDontUpdateStack := false;
  end;
  UpdateComboBlendOp;
  tempUnder.Free;
  LazPaintInstance.ShowTopmost(topmostInfo);
  if LazPaintInstance.Image.CurrentLayerIndex = 0 then
    LazPaintInstance.ToolManager.ToolPopup(tpmBlendOpBackground);
end;

procedure TLayerStackInterface.QuerySelectBlendOp;
begin
  TimerQuery.Enabled := true;
  FQuerySelectBlendOp := true;
end;

procedure TLayerStackInterface.ToolZoomLayerStackIn_Click(Sender: TObject);
begin
  if Assigned(LazPaintInstance) then
    LazPaintInstance.ExitColorEditor;
  ZoomFactor := ZoomFactor * 1.3;
end;

procedure TLayerStackInterface.ToolZoomLayerStackOut_Click(Sender: TObject);
begin
  if Assigned(LazPaintInstance) then
    LazPaintInstance.ExitColorEditor;
  ZoomFactor := ZoomFactor / 1.3;
end;

procedure TLayerStackInterface.ApplyThemeAndDPI;
var
  spacing, iconSize: Integer;
begin
  iconSize := DoScaleX(16, OriginalDPI, DPI);
  Toolbar.Images := LazPaintInstance.Icons[iconSize];
  Toolbar.ButtonWidth:= iconSize + DoScaleX(4, OriginalDPI, DPI);
  Toolbar.ButtonHeight:= iconSize + DoScaleY(4, OriginalDPI, DPI);
  FLayerMenu.Images := LazPaintInstance.Icons[DoScaleX(20,OriginalDPI)];
  ComboBox_BlendOp.Width := Toolbar.ButtonWidth*7;
  ComboBox_BlendOp.Height := Toolbar.ButtonHeight;
  DarkThemeInstance.Apply(PanelToolbar, DarkTheme);
  BGRALayerStack.Color:= GetBackColor(False);
  BGRALayerStack.DiscardBitmap;
  spacing := DoScaleX(2, OriginalDPI, DPI);
  if PanelToolbar.BevelOuter <> bvNone then dec(spacing, PanelToolbar.BevelWidth);
  PanelToolbar.ChildSizing.TopBottomSpacing:= spacing;
  PanelToolbar.ChildSizing.LeftRightSpacing:= spacing;
  Container.Color := DarkThemeInstance.GetColorButtonFace(DarkTheme);
end;

procedure TLayerStackInterface.SetDPI(AValue: integer);
var
  prevDPI: Integer;
begin
  if FDPI=AValue then Exit;
  prevDPI := FDPI;
  FDPI:=AValue;
  ApplyThemeAndDPI;
  if prevDPI > 0 then
    ZoomFactor:= ZoomFactor*AValue/prevDPI;
end;

constructor TLayerStackInterface.Create(AContainer: TWinControl; AInstance: TLazPaintCustomInstance);
begin
  Container := AContainer;
  LazPaintInstance := AInstance;
  FDPI := OriginalDPI;

  PanelToolbar := TPanel.Create(Container);
  PanelToolbar.Parent := Container;
  PanelToolbar.Align := alBottom;
  PanelToolbar.AutoSize:= false;
  PanelToolbar.Height := 200;
  Toolbar := TToolBar.Create(PanelToolbar);
  Toolbar.Parent := PanelToolbar;
  Toolbar.ShowHint := true;
  Toolbar.Wrapable := true;
  Toolbar.AutoSize := true;
  Toolbar.EdgeInner:= esNone;
  Toolbar.EdgeOuter:= esNone;
  Toolbar.Indent := 0;
  Toolbar.Align:= alTop;
  Toolbar.OnResize:= @Toolbar_Resize;
  ComboBox_BlendOp := TBCComboBox.Create(ToolBar);
  ComboBox_BlendOp.Parent := Toolbar;
  ComboBox_BlendOp.OnChange:= @ComboBox_BlendOpChange;
  UpdateComboBlendOp;
  AddButton(rsSelectBlendOperation, 103, @ToolSelectBlendOperation_Click);
  AddButton(rsZoomLayerStackIn, 6, @ToolZoomLayerStackIn_Click);
  AddButton(rsZoomLayerStackOut, 7, @ToolZoomLayerStackOut_Click);

  FRenaming := false;
  FMovingItemStart := false;
  FScrollPos := point(0,0);
  FZoomFactor := 1;
  FChangingLayerOpacityIndex:= -1;
  FAskTransferSelectionLayerIndex := -1;
  VolatileHorzScrollBar := nil;
  VolatileVertScrollBar := nil;

  BGRALayerStack := TBGRAVirtualScreen.Create(Container);
  BGRALayerStack.Parent := Container;
  BGRALayerStack.Align:= alClient;
  BGRALayerStack.Caption := '';
  BGRALayerStack.OnRedraw:= @BGRALayerStack_Redraw;
  BGRALayerStack.OnMouseDown:=@BGRALayerStack_MouseDown;
  BGRALayerStack.OnMouseMove:=@BGRALayerStack_MouseMove;
  BGRALayerStack.OnMouseUp:=@BGRALayerStack_MouseUp;
  BGRALayerStack.OnMouseWheel:=@BGRALayerStack_MouseWheel;
  BGRALayerStack.BitmapAutoScale:= false;

  TimerScroll := TTimer.Create(Container);
  TimerScroll.Enabled := false;
  TimerScroll.Interval := 30;
  TimerScroll.OnTimer:=@TimerScroll_Timer;
  TimerQuery := TTimer.Create(Container);
  TimerQuery.Enabled := false;
  TimerQuery.Interval := 200;
  TimerQuery.OnTimer:=@TimerQuery_Timer;
  FQuerySelectBlendOp:= false;

  FLayerMenu := TPopupMenu.Create(AContainer);
  FQueryLayerMenu:= false;

  ApplyThemeAndDPI;
  LazPaintInstance.Image.OnImageChanged.AddObserver(@LazPaint_ImageChanged);
end;

destructor TLayerStackInterface.Destroy;
begin
  LazPaintInstance.Image.OnImageChanged.RemoveObserver(@LazPaint_ImageChanged);
  FreeAndNil(VolatileHorzScrollBar);
  FreeAndNil(VolatileVertScrollBar);
  FreeAndNil(FRegularBackground);
  FreeAndNil(FIconBackground);
  FreeAndNil(FMovingItemBitmap);
  inherited Destroy;
end;

procedure TLayerStackInterface.AddButton(AAction: TBasicAction);
var button: TToolButton;
begin
  if not Assigned(Toolbar) then exit;
  button := TToolButton.Create(Toolbar);
  button.Action := AAction;
  button.Style := tbsButton;
  button.Parent := Toolbar;
end;

function TLayerStackInterface.GetTextColor(ASelected: boolean): TColor;
begin
  if ASelected then
    result := DarkThemeInstance.GetColorHighlightText(DarkTheme)
    else result := DarkThemeInstance.GetColorEditableText(DarkTheme);
end;

procedure TLayerStackInterface.AddButton(ACaption: string;
  AImageIndex: integer; AOnClick: TNotifyEvent);
var button: TToolButton;
begin
  if not Assigned(Toolbar) then exit;
  button := TToolButton.Create(Toolbar);
  button.Hint := ACaption;
  button.ImageIndex := AImageIndex;
  button.Style := tbsButton;
  button.Parent := Toolbar;
  button.OnClick := AOnClick;
end;

procedure TLayerStackInterface.AddLayerMenu(AAction: TBasicAction);
var
  item: TMenuItem;
begin
  item := TMenuItem.Create(FLayerMenu);
  item.Action := AAction;
  FLayerMenu.Items.Add(item);
end;

procedure TLayerStackInterface.ScrollToItem(AIndex: integer; AUpdateStack: boolean);
begin
  FScrollPos.X := 0;
  FScrollPos.Y := (LazPaintInstance.Image.NbLayers-1-AIndex)*FLayerRectHeight+FLayerRectHeight div 2-FAvailableHeight div 2;
  if AUpdateStack then BGRALayerStack.DiscardBitmap;
end;

procedure TLayerStackInterface.InvalidateStack(AScrollIntoView: boolean);
begin
  if not FDontUpdateStack then
  begin
    if Assigned(BGRALayerStack) then
      BGRALayerStack.DiscardBitmap;
    if AScrollIntoView then FScrollStackItemIntoView := true;
    FRenaming := false;
  end;
end;

function TLayerStackInterface.GetWidthFor(AButtonCount: integer): integer;
begin
  result := Toolbar.ButtonWidth*AButtonCount + DoScaleX(2, OriginalDPI, DPI)*2;
end;

function TLayerStackInterface.GetBackColor(ASelected: boolean): TColor;
begin
  if ASelected then
    result := DarkThemeInstance.GetColorHighlightBack(DarkTheme) else
  begin
    if DarkTheme then
      result := MergeBGRAWithGammaCorrection(
                  DarkThemeInstance.GetColorButtonFace(true), 1,
                  DarkThemeInstance.GetColorEditableFace(true), 1)
    else
      result := DarkThemeInstance.GetColorEditableFace(DarkTheme);
  end;
end;

end.

