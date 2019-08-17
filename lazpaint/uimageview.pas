unit UImageView;

{$mode objfpc}{$H+}
{$IFDEF LINUX}{$DEFINE IMAGEVIEW_DIRECTUPDATE}{$ENDIF}

interface

uses
  Classes, SysUtils, USelectionHighlight, BGRABitmap, BGRABitmapTypes,
  LazPaintType, UImage, UZoom, Graphics, Controls;

type

  { TImageView }

  TImageView = class
  protected
    FVirtualScreen : TBGRABitmap;
    FPenCursorVisible: boolean;
    FPenCursorPos,FPenCursorPosBefore: TVSCursorPosition;
    FQueryPaintVirtualScreen: boolean;
    FSelectionHighlight: TSelectionHighlight;
    FShowSelection: boolean;
    FInstance: TLazPaintCustomInstance;
    FLastPictureParameters: record
       defined: boolean;
       workArea: TRect;
       scaledArea: TRect;
       virtualScreenArea: TRect;
       originInVS: TPoint;
       zoomFactorX,zoomFactorY: double;
       imageOffset: TPoint;
       imageWidth,imageHeight: integer;
    end;
    FZoom: TZoom;
    FPictureCanvas: TCanvas;
    function GetImage: TLazPaintImage;
    function GetRenderUpdateRectVS(AIncludeLastToolState: boolean): TRect;
    function GetFillSelectionHighlight: boolean;
    function GetPenCursorPosition: TVSCursorPosition;
    function GetWorkspaceColor: TColor;
    procedure PaintPictureImplementation(ACanvasOfs: TPoint; AWorkArea: TRect; AVSPart: TRect);
    procedure PaintVirtualScreenImplementation(ACanvasOfs: TPoint; AWorkArea: TRect; AVSPart: TRect);
    procedure PaintBlueAreaImplementation(ACanvasOfs: TPoint; AWorkArea: TRect);
    procedure PaintBlueAreaOnly(ACanvasOfs: TPoint; AWorkArea: TRect);
    procedure ComputePictureParams(AWorkArea: TRect);
    procedure SetFillSelectionHighlight(AValue: boolean);
    procedure SetShowSelection(AValue: boolean);
    procedure PictureSelectionChanged({%H-}sender: TLazPaintImage; const ARect: TRect);
    procedure PaintVirtualScreenCursor({%H-}ACanvasOfs: TPoint; {%H-}AWorkArea: TRect; {%H-}AWinControlOfs: TPoint; {%H-}AWinControl: TWinControl);
    function GetRectToInvalidate(AInvalidateAll: boolean; AWorkArea: TRect): TRect;
  public
    constructor Create(AInstance: TLazPaintCustomInstance; AZoom: TZoom; ACanvas: TCanvas);
    destructor Destroy; override;
    procedure DoPaint(ACanvasOfs: TPoint; AWorkArea: TRect; AShowNoPicture: boolean);
    procedure InvalidatePicture(AInvalidateAll: boolean; AWorkArea: TRect; AControlOfs: TPoint; AWinControl: TWinControl);
    procedure OnZoomChanged({%H-}sender: TZoom; {%H-}ANewZoom: single; AWorkArea: TRect);
    procedure UpdateCursor(X,Y: integer; ACanvasOfs: TPoint; AWorkArea: TRect; AControl: TControl;
                          AWinControlOfs: TPoint; AWinControl: TWinControl);
    procedure UpdatePicture(ACanvasOfs: TPoint; AWorkArea: TRect; {%H-}AWinControl: TWinControl);
    function BitmapToForm(pt: TPointF): TPointF;
    function BitmapToForm(X, Y: Single): TPointF;
    function BitmapToVirtualScreen(ptF: TPointF): TPointF;
    function BitmapToVirtualScreen(X, Y: Single): TPointF;
    function FormToBitmap(pt: TPoint): TPointF;
    function FormToBitmap(X, Y: Integer): TPointF;
    property Image: TLazPaintImage read GetImage;
    property Zoom: TZoom read FZoom;
    property LazPaintInstance: TLazPaintCustomInstance read FInstance;
    property PictureCanvas: TCanvas read FPictureCanvas;
    property FillSelectionHighlight: boolean read GetFillSelectionHighlight write SetFillSelectionHighlight;
    property ShowSelection: boolean read FShowSelection write SetShowSelection;
    property WorkspaceColor: TColor read GetWorkspaceColor;
  end;

implementation

uses BGRATransform, LCLIntf, Types, ugraph, math, UTool, BGRAThumbnail;

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
  Image.ImageMayChangeCompletely;
end;

function TImageView.GetImage: TLazPaintImage;
begin
  result := FInstance.Image;
end;

procedure TImageView.PaintPictureImplementation(ACanvasOfs: TPoint; AWorkArea: TRect; AVSPart: TRect);
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

  if Assigned(FVirtualScreen) and ((FVirtualScreen.Width <> FLastPictureParameters.virtualScreenArea.Right-FLastPictureParameters.virtualScreenArea.Left) or
     (FVirtualScreen.Height <> FLastPictureParameters.virtualScreenArea.Bottom-FLastPictureParameters.virtualScreenArea.Top)) then
  begin
    FreeAndNil(FVirtualScreen);
    FLastPictureParameters.defined := false;
  end;

  if not Assigned(FVirtualScreen) then
  begin
    FVirtualScreen := TBGRABitmap.Create(FLastPictureParameters.virtualScreenArea.Right-FLastPictureParameters.virtualScreenArea.Left,
                                        FLastPictureParameters.virtualScreenArea.Bottom-FLastPictureParameters.virtualScreenArea.Top);
    FLastPictureParameters.defined := false;
  end;

  if picParamWereDefined then FVirtualScreen.ClipRect := GetRenderUpdateRectVS(False);
  Image.ResetRenderUpdateRect;
  FLastPictureParameters.defined := true;

  renderRect := FLastPictureParameters.scaledArea;
  OffsetRect(renderRect, -FLastPictureParameters.virtualScreenArea.Left,
                         -FLastPictureParameters.virtualScreenArea.Top);

  DrawThumbnailCheckers(FVirtualScreen,renderRect,Image.IsIconCursor);

  //draw image (with merged selection)
  FVirtualScreen.StretchPutImage(renderRect,Image.RenderedImage,dmDrawWithTransparency);
  if (Zoom.Factor > MinZoomForGrid) and LazPaintInstance.GridVisible then
    DrawGrid(FVirtualScreen,FLastPictureParameters.zoomFactorX,FLastPictureParameters.zoomFactorY,
       FLastPictureParameters.originInVS.X,FLastPictureParameters.originInVS.Y);

  DrawSelectionHighlight(renderRect);
  FVirtualScreen.NoClip;

  //show tools info
  LazPaintInstance.ToolManager.RenderTool(FVirtualScreen);

  PaintVirtualScreenImplementation(ACanvasOfs, AWorkArea, AVSPart);
end;

procedure TImageView.PaintVirtualScreenImplementation(ACanvasOfs: TPoint; AWorkArea: TRect; AVSPart: TRect);
var cursorBack: TBGRABitmap;
    DestCanvas: TCanvas;
    DrawOfs: TPoint;
    cursorContourF: array of TPointF;
    rectBack: TRect;
    cursorPos: TVSCursorPosition;

  procedure DrawPart;
  begin
    FVirtualScreen.DrawPart(AVSPart, DestCanvas, DrawOfs.X+AVSPart.Left, DrawOfs.Y+AVSPart.Top, True);
  end;

begin
  if (FVirtualScreen = nil) or not FLastPictureParameters.defined then exit;
  AVSPart.Intersect(rect(0,0, FVirtualScreen.Width,FVirtualScreen.Height));
  if AVSPart.IsEmpty then exit;

  DestCanvas := PictureCanvas;
  DrawOfs := ACanvasOfs;
  Inc(DrawOfs.X, FLastPictureParameters.virtualScreenArea.Left);
  inc(DrawOfs.Y, FLastPictureParameters.virtualScreenArea.Top);

  cursorPos := FPenCursorPos;
  if FPenCursorVisible and not IsRectEmpty(cursorPos.bounds) then
  begin
    rectBack := cursorPos.bounds;
    IntersectRect(rectBack,rectBack,rect(0,0,FVirtualScreen.Width,FVirtualScreen.Height));
    if not IsRectEmpty(rectBack) then
    begin
      cursorBack := FVirtualScreen.GetPart(rectBack) as TBGRABitmap;

      cursorContourF := FVirtualScreen.ComputeEllipseContour(cursorPos.c.x,cursorPos.c.y,cursorPos.rx,cursorPos.ry);
      FVirtualScreen.PenStyle := psSolid;
      FVirtualScreen.DrawPolygonAntialias(cursorcontourF,BGRA(0,0,0,192),3);
      FVirtualScreen.DrawPolygonAntialias(cursorcontourF,BGRA(255,255,255,255),1);
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

procedure TImageView.PaintBlueAreaImplementation(ACanvasOfs: TPoint; AWorkArea: TRect);
var
  DrawOfs: TPoint;
  workArea, scaledArea: TRect;
begin
  if FLastPictureParameters.defined then
  begin
    workArea := FLastPictureParameters.workArea;
    if (workArea.Right <= workArea.Left) or (workArea.Bottom <= workArea.Top) then exit;
    scaledArea := FLastPictureParameters.scaledArea;
    with PictureCanvas do
    begin
      Brush.Color := WorkspaceColor;
      DrawOfs := ACanvasOfs;
      if scaledArea.Left > workArea.Left then
        FillRect(workArea.Left+DrawOfs.X,scaledArea.Top+DrawOfs.Y,scaledArea.Left+DrawOfs.X,scaledArea.Bottom+DrawOfs.Y);
      if scaledArea.Top > workArea.Top then
        FillRect(workArea.Left+DrawOfs.X,workArea.Top+DrawOfs.Y,workArea.Right+DrawOfs.X,scaledArea.Top+DrawOfs.Y);
      if scaledArea.Right < workArea.Right then
        FillRect(scaledArea.Right+DrawOfs.X,scaledArea.Top+DrawOfs.Y,workArea.Right+DrawOfs.X,scaledArea.Bottom+DrawOfs.Y);
      if scaledArea.Bottom < workArea.Bottom then
        FillRect(workArea.Left+DrawOfs.X,scaledArea.Bottom+DrawOfs.Y,workArea.Right+DrawOfs.X,workArea.Bottom+DrawOfs.Y);
    end;
  end else
    PaintBlueAreaOnly(ACanvasOfs, AWorkArea);
end;

procedure TImageView.PaintBlueAreaOnly(ACanvasOfs: TPoint; AWorkArea: TRect);
var
  DrawOfs: TPoint;
begin
  if (AWorkArea.Right <= AWorkArea.Left) or (AWorkArea.Bottom <= AWorkArea.Top) then exit;
  with PictureCanvas do
  begin
    Brush.Color := WorkspaceColor;
    DrawOfs := ACanvasOfs;
    FillRect(AWorkArea.Left+DrawOfs.X,AWorkArea.Top+DrawOfs.Y,AWorkArea.Right+DrawOfs.X,AWorkArea.Bottom+DrawOfs.Y);
  end;
  FLastPictureParameters.defined := false;
end;

constructor TImageView.Create(AInstance: TLazPaintCustomInstance; AZoom: TZoom;
  ACanvas: TCanvas);
begin
  FInstance := AInstance;
  FZoom := AZoom;
  FPictureCanvas := ACanvas;

  FVirtualScreen := nil;
  FLastPictureParameters.defined:= false;
  FSelectionHighlight := TSelectionHighlight.Create(Image);
  FShowSelection:= true;
  Image.OnSelectionChanged := @PictureSelectionChanged;
  LazPaintInstance.ToolManager.BitmapToVirtualScreen := @BitmapToVirtualScreen;
end;

destructor TImageView.Destroy;
begin
  if Assigned(LazPaintInstance.ToolManager) then
    LazPaintInstance.ToolManager.BitmapToVirtualScreen := nil;
  Image.OnSelectionChanged := nil;
  FreeAndNil(FSelectionHighlight);
  FreeAndNil(FVirtualScreen);
  inherited Destroy;
end;

procedure TImageView.DoPaint(ACanvasOfs: TPoint; AWorkArea: TRect; AShowNoPicture: boolean);
begin
   if AShowNoPicture then
   begin
     PaintBlueAreaOnly(ACanvasOfs, AWorkArea);
     exit;
   end;
   if FQueryPaintVirtualScreen and
      (FLastPictureParameters.defined and
       IsRectEmpty(GetRenderUpdateRectVS(False))) then
     PaintVirtualScreenImplementation(ACanvasOfs, AWorkArea, rect(0,0,maxLongint,maxLongint))
   else
     PaintPictureImplementation(ACanvasOfs, AWorkArea, rect(0,0,maxLongint,maxLongint));
   PaintBlueAreaImplementation(ACanvasOfs, AWorkArea);
end;

procedure TImageView.ComputePictureParams(AWorkArea: TRect);
var
  scaledArea, croppedArea: TRect;
begin
  FLastPictureParameters.imageWidth:= Image.Width;
  FLastPictureParameters.imageHeight:= Image.Height;
  FLastPictureParameters.zoomFactorX := Zoom.Factor;
  FLastPictureParameters.zoomFactorY := Zoom.Factor;
  FLastPictureParameters.scaledArea := EmptyRect;
  FLastPictureParameters.imageOffset := Image.ImageOffset;
  FLastPictureParameters.originInVS := Point(0,0);
  FLastPictureParameters.virtualScreenArea := EmptyRect;

  FLastPictureParameters.workArea := AWorkArea;
  if (AWorkArea.Right <= AWorkArea.Left) or (AWorkArea.Bottom <= AWorkArea.Top) or not Assigned(Zoom) then
  begin
    FLastPictureParameters.defined := false;
    exit;
  end;

  scaledArea := Zoom.GetScaledArea(AWorkArea, image.Width, image.Height, image.ImageOffset);
  FLastPictureParameters.scaledArea := scaledArea;
  croppedArea := RectInter(scaledArea,AWorkArea);
  if IsRectEmpty(croppedArea) then
  begin
    FLastPictureParameters.defined := false;
    exit;
  end;

  FLastPictureParameters.zoomFactorX := (scaledArea.Right-scaledArea.Left)/Image.Width;
  FLastPictureParameters.zoomFactorY := (scaledArea.Bottom-scaledArea.Top)/Image.Height;

  FLastPictureParameters.virtualScreenArea := croppedArea;

  FLastPictureParameters.originInVS.X := scaledArea.Left - FLastPictureParameters.virtualScreenArea.Left;
  FLastPictureParameters.originInVS.Y := scaledArea.Top  - FLastPictureParameters.virtualScreenArea.Top;
  FLastPictureParameters.defined := true;
end;

procedure TImageView.OnZoomChanged(sender: TZoom; ANewZoom: single; AWorkArea: TRect);
Var
  NewBitmapPos: TPointF;
begin
  if sender.PositionDefined then
  begin
    ComputePictureParams(AWorkArea);
    NewBitmapPos := FormToBitmap(sender.MousePosition.X,sender.MousePosition.Y);
    image.ImageOffset:= point(image.ImageOffset.X + round(NewBitmapPos.X-sender.BitmapPosition.X),
                              image.ImageOffset.Y + round(NewBitmapPos.Y-sender.BitmapPosition.Y));
    FPenCursorPos.bounds := EmptyRect;
  end;
  FLastPictureParameters.defined := false;
end;

function TImageView.GetRenderUpdateRectVS(AIncludeLastToolState: boolean): TRect;
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
  if AIncludeLastToolState and Assigned(FVirtualScreen) then
    result := RectUnion(result, LazPaintInstance.ToolManager.GetRenderBounds(FVirtualScreen.Width,FVirtualScreen.Height));
end;

function TImageView.FormToBitmap(X, Y: Integer): TPointF;
begin
  if not FLastPictureParameters.defined then
    result.X := 0 else
     result.x := (x+0.5-FLastPictureParameters.scaledArea.Left)/FLastPictureParameters.zoomFactorX - 0.5;
  if not FLastPictureParameters.defined then
    result.Y := 0 else
     result.y := (y+0.5-FLastPictureParameters.scaledArea.Top)/FLastPictureParameters.zoomFactorY - 0.5;
end;

function TImageView.FormToBitmap(pt: TPoint): TPointF;
begin
  result := FormToBitmap(pt.X,pt.Y);
end;

function TImageView.BitmapToForm(X, Y: Single): TPointF;
begin
  if not FLastPictureParameters.defined then
    result.X := 0 else
     result.X := (X+0.5)*FLastPictureParameters.zoomFactorX + FLastPictureParameters.scaledArea.Left-0.5;
  if not FLastPictureParameters.defined then
    result.Y := 0 else
     result.Y := (Y+0.5)*FLastPictureParameters.zoomFactorY + FLastPictureParameters.scaledArea.Top-0.5;
end;

function TImageView.BitmapToForm(pt: TPointF): TPointF;
begin
  result := BitmapToForm(pt.x,pt.y);
end;

function TImageView.BitmapToVirtualScreen(X, Y: Single): TPointF;
begin
  result := BitmapToForm(X,Y) - PointF(FLastPictureParameters.virtualScreenArea.Left, FLastPictureParameters.virtualScreenArea.Top);
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
    result.bounds.left := floor(tl.x)-margin;
    result.bounds.top := floor(tl.y)-margin;
    result.bounds.right := ceil(br.x)+1+2*margin;
    result.bounds.bottom := ceil(br.y)+1+2*margin;
  end else
    result.bounds := EmptyRect;
end;

procedure TImageView.InvalidatePicture(AInvalidateAll: boolean; AWorkArea: TRect; AControlOfs: TPoint; AWinControl: TWinControl);
var
  area: TRect;
begin
  area := GetRectToInvalidate(AInvalidateAll, AWorkArea);
  OffsetRect(area, AControlOfs.X,AControlOfs.Y);
  InvalidateRect(AWinControl.Handle,@area,False);
end;

procedure TImageView.PictureSelectionChanged(sender: TLazPaintImage; const ARect: TRect);
begin
  if Assigned(FSelectionHighlight) then FSelectionHighlight.NotifyChange(ARect);
end;

procedure TImageView.PaintVirtualScreenCursor(ACanvasOfs: TPoint; AWorkArea: TRect;
                                              AWinControlOfs: TPoint; AWinControl: TWinControl);
var area: TRect;
begin
  area := FPenCursorPos.bounds;
  FPenCursorPos := GetPenCursorPosition;
  area := RectUnion(area, FPenCursorPos.bounds);
  OffsetRect(area, FLastPictureParameters.virtualScreenArea.Left,
                   FLastPictureParameters.virtualScreenArea.Top);
  {$IFDEF IMAGEVIEW_DIRECTUPDATE}
  OffsetRect(area, -FLastPictureParameters.virtualScreenArea.Left, -FLastPictureParameters.virtualScreenArea.Top);
  PaintVirtualScreenImplementation(ACanvasOfs, AWorkArea, area);
  {$ELSE}
  FQueryPaintVirtualScreen := True;
  OffsetRect(area, AWinControlOfs.X,AWinControlOfs.Y);
  InvalidateRect(AWinControl.Handle,@area,False);
  AWinControl.Update;
  FQueryPaintVirtualScreen := False;
  {$ENDIF}
end;

function TImageView.GetRectToInvalidate(AInvalidateAll: boolean;
  AWorkArea: TRect): TRect;
begin
  if not AInvalidateAll and FLastPictureParameters.defined and
    (FLastPictureParameters.imageWidth = image.Width) and (FLastPictureParameters.imageHeight = image.Height) and
    (FLastPictureParameters.imageOffset.x = Image.ImageOffset.x) and (FLastPictureParameters.imageOffset.y = Image.ImageOffset.y) and
    (FLastPictureParameters.workArea.Left = AWorkArea.Left) and (FLastPictureParameters.workArea.Top = AWorkArea.Top) and
    (FLastPictureParameters.workArea.Right = AWorkArea.Right) and (FLastPictureParameters.workArea.Bottom = AWorkArea.Bottom) then
  begin
    result := GetRenderUpdateRectVS(True);
    result := RectUnion(result,FPenCursorPosBefore.bounds);
    result := RectUnion(result,FPenCursorPos.bounds);
    OffsetRect(result, FLastPictureParameters.virtualScreenArea.Left,
                     FLastPictureParameters.virtualScreenArea.Top);
  end
  else
  begin
    FLastPictureParameters.defined:=false;
    result:= rect(-maxlongint div 2,-maxlongint div 2,maxlongint div 2,maxlongint div 2);
  end;
end;

procedure TImageView.UpdateCursor(X,Y: integer; ACanvasOfs: TPoint; AWorkArea: TRect; AControl: TControl;
                                 AWinControlOfs: TPoint; AWinControl: TWinControl);
var virtualScreenPenCursorBefore: boolean;
    wantedCursor: TCursor;

  function UseVSPenCursor: boolean;
  begin
    if FLastPictureParameters.Defined and
      (LazPaintInstance.ToolManager.PenWidth * FLastPictureParameters.zoomFactorX > 6) and
      PtInRect(FLastPictureParameters.scaledArea, Point(X,Y)) then
    begin
      FPenCursorVisible := True;
      wantedCursor := crNone;
      result := true;
    end else
      result := false;
  end;

begin
  virtualScreenPenCursorBefore := FPenCursorVisible;
  FPenCursorVisible := false;
  wantedCursor := LazPaintInstance.ToolManager.Cursor;
  if LazPaintInstance.ToolManager.GetCurrentToolType in[ptPen,ptEraser,ptBrush,ptClone] then UseVSPenCursor;
  if not PtInRect(AWorkArea, Point(X,Y)) then wantedCursor:= crDefault;
  if AControl.Cursor <> wantedCursor then AControl.Cursor := wantedCursor;
  if virtualScreenPenCursorBefore or FPenCursorVisible then
    PaintVirtualScreenCursor(ACanvasOfs, AWorkArea, AWinControlOfs, AWinControl);
end;

procedure TImageView.UpdatePicture(ACanvasOfs: TPoint; AWorkArea: TRect; AWinControl: TWinControl);
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
  if FLastPictureParameters.defined then
    OffsetRect(updateArea, -FLastPictureParameters.virtualScreenArea.Left,-FLastPictureParameters.virtualScreenArea.Top);
  PaintPictureImplementation(ACanvasOfs, AWorkArea, updateArea);
  if prevVSArea <> FLastPictureParameters.virtualScreenArea then
    PaintBlueAreaImplementation(ACanvasOfs, AWorkArea);
  {$ELSE}
  InvalidateRect(AWinControl.Handle, @updateArea, false);
  AWinControl.Update;
  {$ENDIF}
end;

end.

