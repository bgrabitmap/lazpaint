// SPDX-License-Identifier: GPL-3.0-only
unit UImageView;

{$mode objfpc}{$H+}
{$IF defined(LINUX) and not defined(LCLqt5)}{$DEFINE IMAGEVIEW_DIRECTUPDATE}{$ENDIF}
{$DEFINE DRAW_TOOL_OUTSIDE_IMAGE}
{$IF not defined(DARWIN) and not defined(LCLqt5)}{$DEFINE IMAGEVIEW_QUICKUPDATE}{$ENDIF}

interface

uses
  Classes, SysUtils, USelectionHighlight, BGRABitmap, BGRABitmapTypes,
  LazPaintType, UImage, UZoom, Graphics, Controls, LCLType, UImageObservation,
  laztablet, LMessages;

type
  TPictureMouseMoveEvent = procedure(ASender: TObject; APosition: TPointF) of object;
  TPictureMouseBeforeEvent = procedure(ASender: TObject; AShift: TShiftState) of object;

  { TOpaquePaintBox }

  TOpaquePaintBox = class(TCustomControl)
  protected
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
    procedure WMPaint(var Message: TLMPaint); message LM_PAINT;
  public
    PaintRect: TRect;
    procedure InvalidateRect(ARect: TRect);
  end;

  { TImageView }

  TImageView = class
  private
    function GetMouseButtonState: TShiftState;
    procedure SetCanvasScale(AValue: integer);
  protected
    FVirtualScreen: TBGRABitmap;
    FUpdatingPopup: boolean;
    FPenCursorVisible: boolean;
    FPenCursorPos,FPenCursorPosBefore: TVSCursorPosition;
    FQueryPaintVirtualScreen: boolean;
    FSelectionHighlight: TSelectionHighlight;
    FShowSelection: boolean;
    FInstance: TLazPaintCustomInstance;
    FLastPictureParameters: record
       defined: boolean;
       canvasScale: integer;
       workArea, scaledWorkArea: TRect;
       zoomedArea, scaledZoomedArea: TRect;
       virtualScreenArea, scaledVirtualScreenArea: TRect;
       originInVS: TPoint;
       zoomFactorX, zoomFactorY,
       scaledZoomFactorX, scaledZoomFactorY: double;
       imageOffset: TPoint;
       imageWidth,imageHeight: integer;
    end;
    FZoom: TZoom;
    FPaintBox: TOpaquePaintBox;
    FormMouseMovePos: TPoint;
    InFormMouseMove: boolean;
    InFormPaint: boolean;
    FOnPaint: TNotifyEvent;
    FOnToolbarUpdate: TNotifyEvent;
    FOnMouseMove: TPictureMouseMoveEvent;
    FOnMouseBefore: TPictureMouseBeforeEvent;
    btnLeftDown, btnRightDown, btnMiddleDown: boolean;
    FLastPaintDate: TDateTime;
    FUpdateStackWhenIdle: boolean;
    FCatchPaintPicture, FPaintPictureCatched: boolean;
    InShowNoPicture: boolean;
    FCanCompressOrUpdateStack: boolean;
    FTablet: TLazTablet;
    FImagePos: TPointF;
    FCanvasScale: integer;
    function GetImage: TLazPaintImage;
    function GetPictureCanvas: TCanvas;
    function GetWorkArea: TRect;
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseEnter(Sender: TObject);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure PaintBoxPaint(Sender: TObject);
    procedure ImageChanged(AEvent: TLazPaintImageObservationEvent);
    function GetRenderUpdateRectVS(AIncludeCurrentToolEditor: boolean): TRect;
    function GetFillSelectionHighlight: boolean;
    function GetPenCursorPosition: TVSCursorPosition;
    function GetWorkspaceColor: TColor;
    procedure PaintPictureImplementation(AWorkArea: TRect; AInvalidatedPart: TRect);
    procedure PaintVirtualScreenImplementation(AWorkArea: TRect; AInvalidatedPart: TRect);
    procedure PaintBlueAreaImplementation(AWorkArea: TRect);
    procedure PaintBlueAreaOnly(AWorkArea: TRect);
    procedure ComputePictureParams(AWorkArea: TRect);
    procedure SetFillSelectionHighlight(AValue: boolean);
    procedure SetShowSelection(AValue: boolean);
    procedure PictureSelectionChanged({%H-}sender: TLazPaintImage; const ARect: TRect);
    procedure ToolManagerRenderChanged(Sender: TObject);
    procedure PaintVirtualScreenCursor({%H-}AWorkArea: TRect);
    function GetRectToInvalidate(AInvalidateAll: boolean; AWorkArea: TRect): TRect;
    function GetPictureCoordsDefined: boolean;
    procedure DoInvalidatePicture(AInvalidateAll: boolean; AWorkArea: TRect);
    procedure DoPaint(AWorkArea: TRect; AShowNoPicture: boolean);
    procedure DoUpdatePicture(AWorkArea: TRect);
    procedure ReleaseMouseButtons(Shift: TShiftState);
    function GetCurrentPressure: single;
  public
    constructor Create(AInstance: TLazPaintCustomInstance; AZoom: TZoom; APaintBox: TOpaquePaintBox);
    destructor Destroy; override;
    function CatchToolKeyDown(var AKey: Word): boolean;
    function CatchToolKeyPress(var AKey: TUTF8Char): boolean;
    function CatchToolKeyUp(var AKey: Word): boolean;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: integer);
    procedure UpdatePicture;
    procedure ShowNoPicture;
    procedure InvalidatePicture(AInvalidateAll: boolean);
    procedure OnZoomChanged({%H-}sender: TZoom; {%H-}ANewZoom: single);
    procedure UpdateCursor(X,Y: integer);
    function BitmapToForm(pt: TPointF): TPointF;
    function BitmapToForm(X, Y: Single): TPointF;
    function BitmapToVirtualScreen(ptF: TPointF): TPointF;
    function BitmapToVirtualScreen(X, Y: Single): TPointF;
    function FormToBitmap(pt: TPoint): TPointF;
    function FormToBitmap(X, Y: Integer): TPointF;
    property Image: TLazPaintImage read GetImage;
    property Zoom: TZoom read FZoom;
    property CanvasScale: integer read FCanvasScale write SetCanvasScale;
    property LazPaintInstance: TLazPaintCustomInstance read FInstance;
    property PictureCanvas: TCanvas read GetPictureCanvas;
    property FillSelectionHighlight: boolean read GetFillSelectionHighlight write SetFillSelectionHighlight;
    property ShowSelection: boolean read FShowSelection write SetShowSelection;
    property WorkspaceColor: TColor read GetWorkspaceColor;
    property WorkArea: TRect read GetWorkArea;
    property PictureCoordsDefined: boolean read GetPictureCoordsDefined;
    property UpdatingPopup: boolean read FUpdatingPopup write FUpdatingPopup;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnMouseMove: TPictureMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseBefore: TPictureMouseBeforeEvent read FOnMouseBefore write FOnMouseBefore;
    property OnToolbarUpdate: TNotifyEvent read FOnToolbarUpdate write FOnToolbarUpdate;
    property LastPaintDate: TDateTime read FLastPaintDate;
    property MouseButtonState: TShiftState read GetMouseButtonState;
    property CanCompressOrUpdateStack: boolean read FCanCompressOrUpdateStack;
  end;

implementation

uses BGRATransform, LCLIntf, Types, ugraph, math, UTool, BGRAThumbnail, LCScaleDPI, Forms,
  UToolVectorial, ExtCtrls;

procedure InvalidateControlRect(AControl: TControl; AArea: TRect);
var
  h: HWND;
begin
  if AControl is TWinControl then
    h := TWinControl(AControl).Handle
  else
  begin
    AArea.Offset(AControl.Left, AControl.Top);
    h := AControl.Parent.Handle;
  end;
  InvalidateRect(h, @AArea, False);
end;

{ TOpaquePaintBox }

procedure TOpaquePaintBox.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  //nothing
end;

procedure TOpaquePaintBox.WMPaint(var Message: TLMPaint);
begin
  if Assigned(Message.PaintStruct) then
    PaintRect := Message.PaintStruct^.rcPaint
    else PaintRect := rect(0,0,ClientWidth,ClientHeight);

  inherited WMPaint(Message);
end;

procedure TOpaquePaintBox.InvalidateRect(ARect: TRect);
begin
  InvalidateControlRect(self, ARect);
end;

function TImageView.GetFillSelectionHighlight: boolean;
begin
  result := FSelectionHighlight.FillSelection;
end;

procedure TImageView.SetFillSelectionHighlight(AValue: boolean);
begin
  if AValue = FSelectionHighlight.FillSelection then exit;
  FSelectionHighlight.FillSelection := AValue;
  Image.ImageMayChangeCompletely;
end;

function TImageView.GetWorkspaceColor: TColor;
begin
  result := LazPaintInstance.Config.GetWorkspaceColor;
end;

procedure TImageView.SetShowSelection(AValue: boolean);
begin
  if FShowSelection=AValue then Exit;
  FShowSelection:=AValue;
  Image.RenderMayChange(rect(0,0,Image.Width,Image.Height), true);
end;

function TImageView.GetPictureCoordsDefined: boolean;
begin
  result := FLastPictureParameters.defined;
end;

procedure TImageView.ToolManagerRenderChanged(Sender: TObject);
begin
  if Assigned(FVirtualScreen) then
    Image.RenderMayChange(LazPaintInstance.ToolManager.GetRenderBounds(
                            FVirtualScreen.Width, FVirtualScreen.Height));
end;

procedure TImageView.PaintBoxMouseEnter(Sender: TObject);
begin
  Image.PrepareForRendering;
end;

procedure TImageView.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  updateForVSCursor: boolean;
begin
  if Assigned(FOnMouseBefore) then
    FOnMouseBefore(self, Shift);
  ReleaseMouseButtons(Shift);
  Image.CurrentState.LayeredBitmap.EditorFocused := true;

  FormMouseMovePos := Point(X,Y);
  if InFormMouseMove then exit;
  InFormMouseMove := True;
  if not PictureCoordsDefined then
    Application.ProcessMessages; //empty message stack
  if not PictureCoordsDefined then
  begin
    InFormMouseMove:= false;
    exit;
  end;

  FImagePos := FormToBitmap(FormMouseMovePos);
  if Assigned(FOnMouseMove) then
    FOnMouseMove(self, FImagePos);

  updateForVSCursor:= false;
  if LazPaintInstance.ToolManager.ToolMove(FImagePos, GetCurrentPressure) then
    UpdatePicture
  else
    updateForVSCursor := true;
  if Assigned(FOnToolbarUpdate) then
    FOnToolbarUpdate(self);

  if updateForVSCursor then
    UpdateCursor(X,Y);

  InFormMouseMove := False;
end;

procedure TImageView.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var redraw: boolean;
begin
  redraw := false;
  if (btnLeftDown and (Button = mbLeft)) or (btnRightDown and (Button=mbRight))
    or (btnMiddleDown and (Button = mbMiddle)) then
  begin
    if PictureCoordsDefined then
      redraw := LazPaintInstance.ToolManager.ToolMove(FormToBitmap(X,Y), GetCurrentPressure)
      else redraw := false;
    if LazPaintInstance.ToolManager.ToolUp then redraw := true;
    btnLeftDown := false;
    btnRightDown := false;
    btnMiddleDown:= false;
  end;
  if redraw then UpdatePicture;
  if FUpdateStackWhenIdle then
  begin
    LazPaintInstance.UpdateLayerStackOnTimer;
    FUpdateStackWhenIdle:= false;
  end;
  if Assigned(FOnToolbarUpdate) then
    FOnToolbarUpdate(self);
  ReleaseMouseButtons(Shift);
end;

procedure TImageView.PaintBoxMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if not PictureCoordsDefined then exit;
  if ssAlt in Shift then
  begin
    if WheelDelta > 0 then LazPaintInstance.ToolManager.StepPenSize(false)
    else if WheelDelta < 0 then LazPaintInstance.ToolManager.StepPenSize(true);
  end else
  begin
    Zoom.SetPosition(FormToBitmap(MousePos.X,MousePos.Y), MousePos);
    if WheelDelta > 0 then Zoom.ZoomIn(ssSnap in Shift) else
    if WheelDelta < 0 then Zoom.ZoomOut(ssSnap in Shift);
    Zoom.ClearPosition;
  end;
  Handled := True;
end;

procedure TImageView.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ReleaseMouseButtons(Shift);
  if not (Button in[mbLeft,mbRight,mbMiddle]) or not PictureCoordsDefined then exit;
  FCanCompressOrUpdateStack := false;
  if Assigned(LazPaintInstance) then LazPaintInstance.ExitColorEditor;
  Image.OnImageChanged.DelayedStackUpdate := True;

  if btnLeftDown or btnRightDown or btnMiddleDown then exit;

  if Button = mbMiddle then
  begin
    btnMiddleDown:= true;
    if not LazPaintInstance.ToolManager.ToolSleeping and not (ssAlt in Shift) then LazPaintInstance.ToolManager.ToolSleep;
  end;

  if PictureCoordsDefined then
  begin
    if Button = mbLeft then
      btnLeftDown := true else
    if Button = mbRight then
      btnRightDown := true;

    with LazPaintInstance.ToolManager do
    begin
      if (
          (GetCurrentToolType = ptHand) or
          ((GetCurrentToolType = ptEditShape) and
            Assigned(CurrentTool) and
            (CurrentTool as TEditShapeTool).NothingSelected)
         )  and
         (ssShift in Shift) then
        Image.SelectLayerContainingPixelAt(FormToBitmap(X,Y).Round);

      if ToolDown(FormToBitmap(X,Y),
          btnRightDown{$IFDEF DARWIN} or (ssCtrl in Shift){$ENDIF},
          GetCurrentPressure) then
          UpdatePicture;
    end;

    if Assigned(FOnToolbarUpdate) then
      FOnToolbarUpdate(self);
  end;
end;

function TImageView.GetPictureCanvas: TCanvas;
begin
  result := FPaintBox.Canvas;
end;

procedure TImageView.PaintBoxPaint(Sender: TObject);
begin
  if InFormPaint then exit;
  InFormPaint := true;

  DoPaint(WorkArea, InShowNoPicture);
  LazPaintInstance.NotifyImagePaint;

  InFormPaint := false;
  FLastPaintDate := Now;
end;

function TImageView.GetWorkArea: TRect;
begin
  result := rect(0, 0, FPaintBox.Width, FPaintBox.Height);
end;

procedure TImageView.ImageChanged(AEvent: TLazPaintImageObservationEvent);
begin
  if AEvent.DelayedStackUpdate then FUpdateStackWhenIdle := true;
  if FCatchPaintPicture then
    FPaintPictureCatched := true
    else InvalidatePicture(false);
end;

function TImageView.GetMouseButtonState: TShiftState;
begin
  result := [];
  if btnLeftDown then include(result, ssLeft);
  if btnMiddleDown then include(result, ssMiddle);
  if btnRightDown then include(result, ssRight);
end;

procedure TImageView.SetCanvasScale(AValue: integer);
begin
  if FCanvasScale=AValue then Exit;
  FCanvasScale:=AValue;
  ugraph.CanvasScale:= AValue;
  LazPaintInstance.ToolManager.CanvasScale := AValue;
end;

function TImageView.GetImage: TLazPaintImage;
begin
  result := FInstance.Image;
end;

procedure TImageView.PaintPictureImplementation(AWorkArea: TRect; AInvalidatedPart: TRect);
var
  renderRect: TRect;
  picParamWereDefined: boolean;

  procedure DrawSelectionHighlight(ARenderRect: TRect);
  var renderVisibleBounds: TRect;
    transform, invTransform: TAffineMatrix;
    renderWidth, renderHeight: integer;
  begin
    if Assigned(FSelectionHighlight) and ShowSelection then
    begin
      renderVisibleBounds := rect(0,0,FVirtualScreen.Width,FVirtualScreen.Height);
      renderWidth := ARenderRect.Right-ARenderRect.Left;
      renderHeight := ARenderRect.Bottom-ARenderRect.Top;
      transform := AffineMatrixTranslation(ARenderRect.Left,ARenderRect.Top) *
             AffineMatrixScale(renderWidth/Image.Width, renderHeight/Image.Height) *
             AffineMatrixTranslation(0.5,0.5) * Image.SelectionTransform * AffineMatrixTranslation(-0.5,-0.5) *
             AffineMatrixScale(Image.Width/renderWidth, Image.Height/renderHeight);
      try
        invTransform := AffineMatrixInverse(transform);
        renderVisibleBounds := FVirtualScreen.GetImageAffineBounds(invTransform, renderVisibleBounds,False);
        FSelectionHighlight.Update(ARenderRect.Right-ARenderRect.Left,ARenderRect.Bottom-ARenderRect.Top, renderVisibleBounds);
      except
      end;
      FSelectionHighlight.DrawAffine(FVirtualScreen, transform, rfBox, dmLinearBlend);
    end;
  end;

begin
  picParamWereDefined := FLastPictureParameters.defined;
  ComputePictureParams(AWorkArea);
  if not FLastPictureParameters.defined then
  begin
    FreeAndNil(FVirtualScreen);
    exit;
  end;

  if not picParamWereDefined then
    FPenCursorPos := GetPenCursorPosition;

  if Assigned(FVirtualScreen) and ((FVirtualScreen.Width <> FLastPictureParameters.scaledVirtualScreenArea.Width) or
     (FVirtualScreen.Height <> FLastPictureParameters.scaledVirtualScreenArea.Height)) then
    FreeAndNil(FVirtualScreen);

  if not Assigned(FVirtualScreen) then
  begin
    FVirtualScreen := TBGRABitmap.Create(FLastPictureParameters.scaledVirtualScreenArea.Width,
                                        FLastPictureParameters.scaledVirtualScreenArea.Height, WorkspaceColor);
    Image.ResetRenderUpdateRect;
    Image.RenderMayChange(rect(0,0,FVirtualScreen.Width,FVirtualScreen.Height), false, false);
  end;

  if not FUpdatingPopup then
  begin
    if picParamWereDefined then FVirtualScreen.ClipRect := GetRenderUpdateRectVS(False);
    Image.ResetRenderUpdateRect;

    if not FVirtualScreen.ClipRect.IsEmpty then
    begin
      renderRect := FLastPictureParameters.scaledZoomedArea;
      OffsetRect(renderRect, -FLastPictureParameters.scaledVirtualScreenArea.Left,
                             -FLastPictureParameters.scaledVirtualScreenArea.Top);

      DrawThumbnailCheckers(FVirtualScreen, renderRect, Image.IsIconCursor, DoScaleX(60*CanvasScale, OriginalDPI)/60);

      //draw image (with merged selection)
      FVirtualScreen.StretchPutImage(renderRect,Image.RenderedImage,dmDrawWithTransparency);
      if (Zoom.Factor > DoScaleX(MinZoomForGrid, OriginalDPI)) and LazPaintInstance.GridVisible then
        DrawGrid(FVirtualScreen, FLastPictureParameters.scaledZoomFactorX,
          FLastPictureParameters.scaledZoomFactorY, FLastPictureParameters.originInVS.X,
          FLastPictureParameters.originInVS.Y);

      DrawSelectionHighlight(renderRect);

      {$IFDEF DRAW_TOOL_OUTSIDE_IMAGE}
      //paint blue area in virtual screen
      if FLastPictureParameters.scaledZoomedArea.Top > 0 then
        FVirtualScreen.FillRect(0, 0,
          FVirtualScreen.Width, FLastPictureParameters.scaledZoomedArea.Top, WorkspaceColor, dmSet);
      if FLastPictureParameters.scaledZoomedArea.Left > 0 then
        FVirtualScreen.FillRect(0, FLastPictureParameters.scaledZoomedArea.Top,
          FLastPictureParameters.scaledZoomedArea.Left, FLastPictureParameters.scaledZoomedArea.Bottom,
          WorkspaceColor, dmSet);
      if FLastPictureParameters.scaledZoomedArea.Right < FVirtualScreen.Width then
        FVirtualScreen.FillRect(FLastPictureParameters.scaledZoomedArea.Right, FLastPictureParameters.scaledZoomedArea.Top,
          FVirtualScreen.Width, FLastPictureParameters.scaledZoomedArea.Bottom,
          WorkspaceColor, dmSet);
      if FLastPictureParameters.scaledZoomedArea.Bottom < FVirtualScreen.Height then
        FVirtualScreen.FillRect(0, FLastPictureParameters.scaledZoomedArea.Bottom,
          FVirtualScreen.Width, FVirtualScreen.Height, WorkspaceColor, dmSet);
      {$ENDIF}
    end;
    FVirtualScreen.NoClip;

    //show tools info
    Image.RenderMayChange(LazPaintInstance.ToolManager.RenderTool(FVirtualScreen), false, false);
  end;

  PaintVirtualScreenImplementation(AWorkArea, AInvalidatedPart);
  Image.VisibleArea := TRectF.Intersect(rectF(FormToBitmap(AWorkArea.Left, AWorkArea.Top),
                                              FormToBitmap(AWorkArea.Right, AWorkArea.Bottom)),
                          rectF(-0.5,-0.5,Image.Width-0.5,Image.Height-0.5));
end;

procedure TImageView.PaintVirtualScreenImplementation(AWorkArea: TRect; AInvalidatedPart: TRect);
var cursorBack: TBGRABitmap;
    DestCanvas: TCanvas;
    cursorContourF: array of TPointF;
    rectBack, oldClip: TRect;
    cursorPos: TVSCursorPosition;

  procedure DrawPart;
  var
    vsPart: TRect;
  begin
    with AInvalidatedPart do
      vsPart := rect(Left*CanvasScale, Top*CanvasScale, Right*CanvasScale, Bottom*CanvasScale);
    with FLastPictureParameters.scaledVirtualScreenArea do
      vsPart.Offset(-Left, -Top);
    FVirtualScreen.DrawPart(vsPart, DestCanvas, AInvalidatedPart, True);
  end;

begin
  if (FVirtualScreen = nil) or not FLastPictureParameters.defined then exit;
  AInvalidatedPart.Intersect(FLastPictureParameters.virtualScreenArea);
  if AInvalidatedPart.IsEmpty then exit;

  DestCanvas := PictureCanvas;

  cursorPos := FPenCursorPos;
  if FPenCursorVisible and not IsRectEmpty(cursorPos.bounds) then
  begin
    rectBack := cursorPos.bounds;
    IntersectRect(rectBack,rectBack,rect(0,0,FVirtualScreen.Width,FVirtualScreen.Height));
    if not IsRectEmpty(rectBack) then
    begin
      cursorBack := FVirtualScreen.GetPart(rectBack) as TBGRABitmap;

      cursorContourF := FVirtualScreen.ComputeEllipseContour(cursorPos.c.x,cursorPos.c.y,cursorPos.rx,cursorPos.ry);
      oldClip := FVirtualScreen.ClipRect;
      FVirtualScreen.ClipRect := rectBack;
      FVirtualScreen.PenStyle := psSolid;
      FVirtualScreen.DrawPolygonAntialias(cursorcontourF,BGRA(0,0,0,192),3*CanvasScale);
      FVirtualScreen.DrawPolygonAntialias(cursorcontourF,BGRA(255,255,255,255),1*CanvasScale);
      FVirtualScreen.ClipRect := oldClip;
      DrawPart;

      FVirtualScreen.PutImage(rectBack.left,rectBack.Top,cursorBack,dmSet);
      cursorBack.Free;
    end else
      DrawPart;
  end else
    DrawPart;

  if (image.Width = 0) or (image.Height = 0) then
    Zoom.MinFactor := 1
  else
    Zoom.MinFactor := max(8/image.Width, 8/image.Height);
  with AWorkArea do
    Zoom.MaxFactor := min(32,max(1,min((right-left)/8,(bottom-top)/8)));
end;

procedure TImageView.PaintBlueAreaImplementation(AWorkArea: TRect);
var
  lastWorkArea, zoomedArea: TRect;
begin
  if FLastPictureParameters.defined then
  begin
    lastWorkArea := FLastPictureParameters.WorkArea;
    if (lastWorkArea.Right <= lastWorkArea.Left) or (lastWorkArea.Bottom <= lastWorkArea.Top) then exit;
    zoomedArea := FLastPictureParameters.virtualScreenArea;
    IntersectRect(zoomedArea, zoomedArea,lastWorkArea);
    with PictureCanvas do
    begin
      Brush.Color := WorkspaceColor;
      if zoomedArea.Left > lastWorkArea.Left then
        FillRect(lastWorkArea.Left, zoomedArea.Top, zoomedArea.Left, zoomedArea.Bottom);
      if zoomedArea.Top > lastWorkArea.Top then
        FillRect(lastWorkArea.Left, lastWorkArea.Top, lastWorkArea.Right, zoomedArea.Top);
      if zoomedArea.Right < lastWorkArea.Right then
        FillRect(zoomedArea.Right, zoomedArea.Top, lastWorkArea.Right, zoomedArea.Bottom);
      if zoomedArea.Bottom < lastWorkArea.Bottom then
        FillRect(lastWorkArea.Left, zoomedArea.Bottom, lastWorkArea.Right, lastWorkArea.Bottom);
    end;
  end else
    PaintBlueAreaOnly(AWorkArea);
end;

procedure TImageView.PaintBlueAreaOnly(AWorkArea: TRect);
begin
  if (AWorkArea.Right <= AWorkArea.Left) or (AWorkArea.Bottom <= AWorkArea.Top) then exit;
  with PictureCanvas do
  begin
    Brush.Color := WorkspaceColor;
    FillRect(AWorkArea);
  end;
  FLastPictureParameters.defined := false;
end;

constructor TImageView.Create(AInstance: TLazPaintCustomInstance; AZoom: TZoom;
  APaintBox: TOpaquePaintBox);
begin
  FInstance := AInstance;
  FZoom := AZoom;
  FCanvasScale:= round(APaintBox.GetCanvasScaleFactor);
  AInstance.ToolManager.CanvasScale := FCanvasScale;
  ugraph.CanvasScale:= FCanvasScale;

  FPaintBox := APaintBox;
  FPaintBox.OnMouseEnter:=@PaintBoxMouseEnter;
  FPaintBox.OnMouseDown:= @PaintBoxMouseDown;
  FPaintBox.OnMouseMove:= @PaintBoxMouseMove;
  FPaintBox.OnMouseUp:=   @PaintBoxMouseUp;
  FPaintBox.OnMouseWheel:=@PaintBoxMouseWheel;
  FPaintBox.OnPaint:=@PaintBoxPaint;
  //recursive calls
  InFormMouseMove:= false;
  InFormPaint := false;
  FLastPictureParameters.defined:= false;
  FSelectionHighlight := TSelectionHighlight.Create(Image);
  FShowSelection:= true;
  Image.OnSelectionChanged := @PictureSelectionChanged;
  Image.OnImageChanged.AddObserver(@ImageChanged);
  if Assigned(LazPaintInstance.ToolManager) then
  begin
    LazPaintInstance.ToolManager.OnToolRenderChanged:=@ToolManagerRenderChanged;
    LazPaintInstance.ToolManager.BitmapToVirtualScreen := @BitmapToVirtualScreen;
  end;

  //mouse status
  btnLeftDown := false;
  btnRightDown := false;
  btnMiddleDown:= false;
  FImagePos := EmptyPointF;
  try
    FTablet := TLazTablet.Create(nil);
  except
    on ex: exception do
      FTablet := nil;
  end;
end;

destructor TImageView.Destroy;
begin
  FreeAndNil(FTablet);
  if Assigned(LazPaintInstance.ToolManager) then
  begin
    LazPaintInstance.ToolManager.OnToolRenderChanged := nil;
    LazPaintInstance.ToolManager.BitmapToVirtualScreen := nil;
  end;
  Image.OnImageChanged.RemoveObserver(@ImageChanged);
  Image.OnSelectionChanged := nil;
  FreeAndNil(FSelectionHighlight);
  FreeAndNil(FVirtualScreen);
  inherited Destroy;
end;

procedure TImageView.SetBounds(ALeft, ATop, AWidth, AHeight: integer);
var picBoundsChanged: boolean;
begin
  picBoundsChanged := (FPaintBox.Left <> ALeft) or (FPaintBox.Top <> ATop) or
      (FPaintBox.Width <> AWidth) or (FPaintBox.Height <> AHeight);
  FPaintBox.SetBounds(ALeft, ATop, AWidth, AHeight);
  CanvasScale:= round(FPaintBox.GetCanvasScaleFactor);
  if picBoundsChanged then
    InvalidatePicture(True);
end;

function TImageView.CatchToolKeyDown(var AKey: Word): boolean;
begin
  FCatchPaintPicture:= true;
  FPaintPictureCatched := false;
  try
    result := LazPaintInstance.ToolManager.ToolKeyDown(AKey) or FPaintPictureCatched;
  finally
    FCatchPaintPicture:= false;
  end;
end;

function TImageView.CatchToolKeyUp(var AKey: Word): boolean;
begin
  FCatchPaintPicture:= true;
  FPaintPictureCatched := false;
  try
     result := LazPaintInstance.ToolManager.ToolKeyUp(AKey) or FPaintPictureCatched;
  finally
    FCatchPaintPicture:= false;
  end;
end;

function TImageView.CatchToolKeyPress(var AKey: TUTF8Char): boolean;
begin
  FCatchPaintPicture:= true;
  FPaintPictureCatched := false;
  try
    result := LazPaintInstance.ToolManager.ToolKeyPress(AKey) or FPaintPictureCatched;
  finally
    FCatchPaintPicture:= false;
  end;
end;

procedure TImageView.UpdatePicture;
begin
  DoUpdatePicture(WorkArea);
  if not Image.OnImageChanged.DelayedStackUpdate then LazPaintInstance.InvalidateLayerStack;
end;

procedure TImageView.ShowNoPicture;
begin
  InShowNoPicture := true;
  try
    DoUpdatePicture(WorkArea);
  finally
    InShowNoPicture := false;
  end;
end;

procedure TImageView.DoPaint(AWorkArea: TRect; AShowNoPicture: boolean);
begin
  if AShowNoPicture then
    PaintBlueAreaOnly(AWorkArea)
  else
  begin
    if FQueryPaintVirtualScreen and
       (FLastPictureParameters.defined and
        IsRectEmpty(GetRenderUpdateRectVS(False))) then
       PaintVirtualScreenImplementation(AWorkArea, FPaintBox.PaintRect)
    else
      PaintPictureImplementation(AWorkArea, FPaintBox.PaintRect);
    PaintBlueAreaImplementation(AWorkArea);
  end;
  if Assigned(FOnPaint) then FOnPaint(self);
end;

procedure TImageView.InvalidatePicture(AInvalidateAll: boolean);
begin
  DoInvalidatePicture(AInvalidateAll, WorkArea);
end;

procedure TImageView.ComputePictureParams(AWorkArea: TRect);
var
  croppedArea: TRect;
begin
  FLastPictureParameters.canvasScale:= CanvasScale;
  FLastPictureParameters.imageWidth:= Image.Width;
  FLastPictureParameters.imageHeight:= Image.Height;
  FLastPictureParameters.zoomFactorX := Zoom.Factor;
  FLastPictureParameters.zoomFactorY := Zoom.Factor;
  FLastPictureParameters.zoomFactorX := Zoom.Factor*CanvasScale;
  FLastPictureParameters.zoomFactorY := Zoom.Factor*CanvasScale;
  FLastPictureParameters.zoomedArea := EmptyRect;
  FLastPictureParameters.scaledZoomedArea := EmptyRect;
  FLastPictureParameters.imageOffset := Image.ImageOffset;
  FLastPictureParameters.originInVS := Point(0,0);
  FLastPictureParameters.virtualScreenArea := EmptyRect;
  FLastPictureParameters.scaledVirtualScreenArea := EmptyRect;

  FLastPictureParameters.workArea := AWorkArea;
  if (AWorkArea.Right <= AWorkArea.Left) or (AWorkArea.Bottom <= AWorkArea.Top) or not Assigned(Zoom) then
  begin
    FLastPictureParameters.defined := false;
    exit;
  end;
  FLastPictureParameters.scaledWorkArea := rect(AWorkArea.Left*CanvasScale,
    AWorkArea.Top*CanvasScale, AWorkArea.Right*CanvasScale, AWorkArea.Bottom*CanvasScale);

  FLastPictureParameters.zoomedArea := Zoom.GetScaledArea(AWorkArea, image.Width, image.Height, image.ImageOffset);
  with FLastPictureParameters.zoomedArea do
    FLastPictureParameters.scaledZoomedArea := rect(Left*CanvasScale, Top*CanvasScale,
      Right*CanvasScale, Bottom*CanvasScale);

  {$IFDEF DRAW_TOOL_OUTSIDE_IMAGE}
  croppedArea := FLastPictureParameters.workArea;
  {$ELSE}
  croppedArea := RectInter(FLastPictureParameters.zoomedArea, FLastPictureParameters.workArea);
  {$ENDIF}
  if IsRectEmpty(croppedArea) then
  begin
    FLastPictureParameters.defined := false;
    exit;
  end;

  FLastPictureParameters.zoomFactorX := FLastPictureParameters.zoomedArea.Width/Image.Width;
  FLastPictureParameters.zoomFactorY := FLastPictureParameters.zoomedArea.Height/Image.Height;
  FLastPictureParameters.scaledZoomFactorX := FLastPictureParameters.scaledZoomedArea.Width/Image.Width;
  FLastPictureParameters.scaledZoomFactorY := FLastPictureParameters.scaledZoomedArea.Height/Image.Height;

  FLastPictureParameters.virtualScreenArea := croppedArea;
  with FLastPictureParameters.virtualScreenArea do
    FLastPictureParameters.scaledVirtualScreenArea := rect(Left*CanvasScale, Top*CanvasScale,
      Right*CanvasScale, Bottom*CanvasScale);

  FLastPictureParameters.originInVS.X := FLastPictureParameters.scaledZoomedArea.Left - FLastPictureParameters.scaledVirtualScreenArea.Left;
  FLastPictureParameters.originInVS.Y := FLastPictureParameters.scaledZoomedArea.Top  - FLastPictureParameters.scaledVirtualScreenArea.Top;
  FLastPictureParameters.defined := true;
end;

procedure TImageView.OnZoomChanged(sender: TZoom; ANewZoom: single);
Var
  NewBitmapPos: TPointF;
begin
  if sender.PositionDefined then
  begin
    ComputePictureParams(WorkArea);
    NewBitmapPos := FormToBitmap(sender.MousePosition.X,sender.MousePosition.Y);
    image.ImageOffset:= point(image.ImageOffset.X + round(NewBitmapPos.X-sender.BitmapPosition.X),
                              image.ImageOffset.Y + round(NewBitmapPos.Y-sender.BitmapPosition.Y));
    FPenCursorPos.bounds := EmptyRect;
  end;
  FLastPictureParameters.defined := false;
end;

function TImageView.GetRenderUpdateRectVS(AIncludeCurrentToolEditor: boolean): TRect;
const displayMargin = 1;
begin
  result := Image.RenderUpdateRectInPicCoord;
  if not IsRectEmpty(result) then
  begin
    with BitmapToVirtualScreen(result.Left-0.5,result.Top-0.5) do
    begin
      result.Left := Math.floor(X) - displayMargin;
      result.Top := Math.floor(Y) - displayMargin;
    end;
    with BitmapToVirtualScreen(result.Right-0.5,result.Bottom-0.5) do
    begin
      result.Right := ceil(X) + displayMargin;
      result.Bottom := ceil(Y) + displayMargin;
    end;
  end;
  result := RectUnion(result, Image.RenderUpdateRectInVSCoord);
  if AIncludeCurrentToolEditor and Assigned(FVirtualScreen) then
    result := RectUnion(result, LazPaintInstance.ToolManager.GetRenderBounds(FVirtualScreen.Width,FVirtualScreen.Height));
end;

function TImageView.FormToBitmap(X, Y: Integer): TPointF;
begin
  if not FLastPictureParameters.defined then
    result.X := 0 else
     result.x := ((x-FLastPictureParameters.zoomedArea.Left)*CanvasScale+0.5)/FLastPictureParameters.scaledZoomFactorX - 0.5;
  if not FLastPictureParameters.defined then
    result.Y := 0 else
     result.y := ((y-FLastPictureParameters.zoomedArea.Top)*CanvasScale+0.5)/FLastPictureParameters.scaledZoomFactorY - 0.5;
end;

function TImageView.FormToBitmap(pt: TPoint): TPointF;
begin
  result := FormToBitmap(pt.X,pt.Y);
end;

function TImageView.BitmapToForm(X, Y: Single): TPointF;
begin
  if not FLastPictureParameters.defined then
    result.X := 0 else
     result.X := ((X+0.5)*FLastPictureParameters.scaledZoomFactorX - 0.5)/CanvasScale + FLastPictureParameters.zoomedArea.Left;
  if not FLastPictureParameters.defined then
    result.Y := 0 else
     result.Y := ((Y+0.5)*FLastPictureParameters.scaledZoomFactorY - 0.5)/CanvasScale + FLastPictureParameters.zoomedArea.Top;
end;

function TImageView.BitmapToForm(pt: TPointF): TPointF;
begin
  result := BitmapToForm(pt.x,pt.y);
end;

function TImageView.BitmapToVirtualScreen(X, Y: Single): TPointF;
begin
  if not FLastPictureParameters.defined then
    result.X := 0 else
     result.X := ((X+0.5)*FLastPictureParameters.scaledZoomFactorX - 0.5)
       + FLastPictureParameters.scaledZoomedArea.Left - FLastPictureParameters.scaledVirtualScreenArea.Left;
  if not FLastPictureParameters.defined then
    result.Y := 0 else
     result.Y := ((Y+0.5)*FLastPictureParameters.scaledZoomFactorY - 0.5)
       + FLastPictureParameters.scaledZoomedArea.Top - FLastPictureParameters.scaledVirtualScreenArea.Top;
end;

function TImageView.BitmapToVirtualScreen(ptF: TPointF): TPointF;
begin
  result := BitmapToVirtualScreen(ptF.X,ptF.Y);
end;

function TImageView.GetPenCursorPosition: TVSCursorPosition;
const margin = 2;
var
  tl,br: TPointF;
begin
  with LazPaintInstance.ToolManager do
  begin
    result.c := self.BitmapToVirtualScreen(ToolCurrentCursorPos);
    tl := self.BitmapToVirtualScreen(ToolCurrentCursorPos.X-PenWidth/2,ToolCurrentCursorPos.Y-PenWidth/2);
    br := self.BitmapToVirtualScreen(ToolCurrentCursorPos.X+PenWidth/2,ToolCurrentCursorPos.Y+PenWidth/2);
  end;
  result.rx := (br.x-tl.x)/2-0.5;
  result.ry := (br.y-tl.y)/2-0.5;
  if FPenCursorVisible then
  begin
    result.bounds.left := floor(tl.x)-margin*CanvasScale;
    result.bounds.top := floor(tl.y)-margin*CanvasScale;
    result.bounds.right := ceil(br.x)+1+2*margin*CanvasScale;
    result.bounds.bottom := ceil(br.y)+1+2*margin*CanvasScale;
  end else
    result.bounds := EmptyRect;
end;

procedure TImageView.DoInvalidatePicture(AInvalidateAll: boolean; AWorkArea: TRect);
var
  area: TRect;
begin
  area := GetRectToInvalidate(AInvalidateAll, AWorkArea);
  IntersectRect(area, area, AWorkArea);
  FPaintBox.InvalidateRect(area);
end;

procedure TImageView.PictureSelectionChanged(sender: TLazPaintImage; const ARect: TRect);
begin
  if Assigned(FSelectionHighlight) then FSelectionHighlight.NotifyChange(ARect);
end;

procedure TImageView.PaintVirtualScreenCursor(AWorkArea: TRect);
var area: TRect;
begin
  area := FPenCursorPos.bounds;
  FPenCursorPos := GetPenCursorPosition;
  area := RectUnion(area, FPenCursorPos.bounds);
  if CanvasScale > 1 then
  begin
    area.Left := area.Left div CanvasScale;
    area.Top := area.Top div CanvasScale;
    area.Right := (area.Right+CanvasScale-1) div CanvasScale;
    area.Bottom := (area.Bottom+CanvasScale-1) div CanvasScale;
  end;
  OffsetRect(area, FLastPictureParameters.virtualScreenArea.Left,
                   FLastPictureParameters.virtualScreenArea.Top);
  {$IFDEF IMAGEVIEW_DIRECTUPDATE}
  PaintVirtualScreenImplementation(AWorkArea, area);
  {$ELSE}
  FQueryPaintVirtualScreen := True;
  FPaintBox.InvalidateRect(area);
  {$IFDEF IMAGEVIEW_QUICKUPDATE}FPaintBox.Update;{$ENDIF}
  FQueryPaintVirtualScreen := False;
  {$ENDIF}
end;

function TImageView.GetRectToInvalidate(AInvalidateAll: boolean;
  AWorkArea: TRect): TRect;
begin
  if not AInvalidateAll and FLastPictureParameters.defined and
    (FLastPictureParameters.canvasScale = CanvasScale) and
    (FLastPictureParameters.imageWidth = image.Width) and (FLastPictureParameters.imageHeight = image.Height) and
    (FLastPictureParameters.imageOffset.x = Image.ImageOffset.x) and (FLastPictureParameters.imageOffset.y = Image.ImageOffset.y) and
    (FLastPictureParameters.workArea.Left = AWorkArea.Left) and (FLastPictureParameters.workArea.Top = AWorkArea.Top) and
    (FLastPictureParameters.workArea.Right = AWorkArea.Right) and (FLastPictureParameters.workArea.Bottom = AWorkArea.Bottom) then
  begin
    result := GetRenderUpdateRectVS(True);
    result := RectUnion(result,FPenCursorPosBefore.bounds);
    result := RectUnion(result,FPenCursorPos.bounds);
    OffsetRect(result, FLastPictureParameters.scaledVirtualScreenArea.Left,
                     FLastPictureParameters.scaledVirtualScreenArea.Top);
    if CanvasScale > 1 then
    begin
      result.Left := result.Left div CanvasScale;
      result.Top := result.Top div CanvasScale;
      result.Right := (result.Right+CanvasScale-1) div CanvasScale;
      result.Bottom := (result.Bottom+CanvasScale-1) div CanvasScale;
    end;
  end
  else
  begin
    FLastPictureParameters.defined:=false;
    result:= rect(-maxlongint div 2,-maxlongint div 2,maxlongint div 2,maxlongint div 2);
  end;
end;

procedure TImageView.UpdateCursor(X,Y: integer);
var virtualScreenPenCursorBefore: boolean;
    wantedCursor: TCursor;

  function UseVSPenCursor: boolean;
  begin
    if FLastPictureParameters.Defined and
      (LazPaintInstance.ToolManager.PenWidth * FLastPictureParameters.zoomFactorX > 6) and
      PtInRect(FLastPictureParameters.zoomedArea, Point(X,Y)) then
    begin
      FPenCursorVisible := True;
      {$IFNDEF DARWIN}wantedCursor := crNone;{$ENDIF}
      result := true;
    end else
      result := false;
  end;

begin
  virtualScreenPenCursorBefore := FPenCursorVisible;
  FPenCursorVisible := false;
  wantedCursor := LazPaintInstance.ToolManager.Cursor;
  if LazPaintInstance.ToolManager.GetCurrentToolType in[ptPen,ptEraser,ptBrush,ptClone] then UseVSPenCursor;
  if not PtInRect(WorkArea, Point(X,Y)) then wantedCursor:= crDefault;
  if FPaintBox.Cursor <> wantedCursor then FPaintBox.Cursor := wantedCursor;
  if virtualScreenPenCursorBefore or FPenCursorVisible then
    PaintVirtualScreenCursor(WorkArea);
end;

procedure TImageView.DoUpdatePicture(AWorkArea: TRect);
var
  updateArea: TRect;
  {$IFDEF IMAGEVIEW_DIRECTUPDATE}prevVSArea: TRect;{$ENDIF}
begin
  {$IFDEF IMAGEVIEW_DIRECTUPDATE}
  prevVSArea := FLastPictureParameters.virtualScreenArea;
  {$ENDIF}
  FPenCursorPosBefore := FPenCursorPos;
  FPenCursorPos := GetPenCursorPosition;
  updateArea := GetRectToInvalidate(false, AWorkArea);
  FPenCursorPosBefore.bounds := EmptyRect;
  {$IFDEF IMAGEVIEW_DIRECTUPDATE}
  PaintPictureImplementation(AWorkArea, updateArea);
  if prevVSArea <> FLastPictureParameters.virtualScreenArea then
    PaintBlueAreaImplementation(AWorkArea);
  {$ELSE}
  if IntersectRect(updateArea, updateArea, AWorkArea) then
  begin
    FPaintBox.InvalidateRect(updateArea);
    {$IFDEF IMAGEVIEW_QUICKUPDATE}FPaintBox.Update;{$ENDIF}
  end;
  {$ENDIF}
end;

procedure TImageView.ReleaseMouseButtons(Shift: TShiftState);
begin
  if not (ssLeft in Shift) and btnLeftDown then
  begin
    btnLeftDown := false;
    if LazPaintInstance.ToolManager.ToolUp then UpdatePicture;
  end;
  if not (ssRight in Shift) and btnRightDown then
  begin
    btnRightDown := false;
    if LazPaintInstance.ToolManager.ToolUp then UpdatePicture;
  end;
  if not (ssMiddle in Shift) and btnMiddleDown then
  begin
    btnMiddleDown := false;
    if LazPaintInstance.ToolManager.ToolUp then UpdatePicture;
  end;
  if not btnLeftDown and not btnRightDown then
  begin
    FCanCompressOrUpdateStack := true;
    Image.OnImageChanged.DelayedStackUpdate := False;
  end;
end;

function TImageView.GetCurrentPressure: single;
begin
  if Assigned(FTablet) and FTablet.Present and FTablet.Entering and (FTablet.Max > 0) then
    result := FTablet.Pressure/FTablet.Max
  else
    result := 1;
end;

end.

