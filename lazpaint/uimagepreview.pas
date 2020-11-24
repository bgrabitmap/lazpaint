// SPDX-License-Identifier: GPL-3.0-only
unit UImagePreview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Graphics, Controls, BGRAVirtualScreen,
  LazPaintType, UGraph, UResourceStrings, UFileSystem, Forms, UVolatileScrollBar,
  BGRABitmap, BGRAAnimatedGif, BGRAIconCursor, BGRABitmapTypes, BGRAThumbnail,
  UTiff, fgl;

const
  IconSize = 32;
  SubImageSize = 128;

type
  TBGRABitmapList = specialize TFPGObjectList<TBGRABitmap>;

  { TImagePreview }

  TImagePreview = class
  private
    function GetScaledIconSize: integer;
  protected
    FSurface: TBGRAVirtualScreen;
    FScaling: single;
    FSurfaceScaledHeight: Integer;
    FScrollbar: TVolatileScrollBar;
    FScrolling: boolean;
    FStatus: TLabel;

    FFilename: string;
    FLoadError: string;
    FInUpdatePreview: boolean;

    FImageFormat: TBGRAImageFormat;
    FImageNbLayers: integer;
    FSingleImage: TBGRABitmap;
    FAnimatedGif: TBGRAAnimatedGif;   //has frames
    FTiff: TTiff;                     //has entries
    FIconCursor: TBGRAIconCursor;     //has entries
    FThumbnails: TBGRABitmapList;
    FDuplicateEntrySourceIndex: integer;

    FSelectedMenuIndex: integer;
    FImageMenu: array of record
                 Area, IconArea: TRect;
                 DeleteArea: TRect;
                 FrameIndex: integer;
                 IsNew,IsDuplicate,IsLoopCount: boolean;
               end;

    FOnValidate: TNotifyEvent;
    FOnEscape: TNotifyEvent;
    FAnimate: boolean;

    function GetPreviewDataLoss: boolean;
    procedure SetFilename(AValue: string);

    procedure SurfaceRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure SurfaceMouseDown(Sender: TObject; {%H-}Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure SurfaceMouseMove(Sender: TObject; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure SurfaceMouseUp(Sender: TObject; {%H-}Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure SurfaceMouseWheel(Sender: TObject; {%H-}Shift: TShiftState;
                         WheelDelta: Integer; {%H-}MousePos: TPoint; var Handled: Boolean);
    procedure SurfaceDblClick(Sender: TObject);
    procedure SurfaceKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);

    procedure DrawMenu(Bitmap: TBGRABitmap);
    function TryMenuLayout(AWidth: integer; AColCount, ABottom: integer): integer;
    procedure ScrollToSelectedMenu;

    function CanAddNewEntry: boolean;
    function CanDuplicateEntry: boolean;
    function CanDeleteEntry(index: integer): boolean;
    procedure DeleteEntry(i: integer);
    function GetEntryCount: integer;
    function GetEntryWidth(index: integer): integer;
    function GetEntryHeight(index: integer): integer;
    function GetEntryBitDepth(index: integer): integer;
    function GetEntryBitmap(index: integer): TImageEntry;
    procedure SetEntryBitmap(var AEntry: TImageEntry);
    function GetEntryThumbnail(index: integer; stretchWidth, stretchHeight: integer): TBGRABitmap;

    procedure DrawCurrentFrame(Bitmap: TBGRABitmap);
    function GetCurrentFrameBitmap: TBGRABitmap;
    procedure ClearMenu;
    procedure ClearThumbnails;
    procedure DoValidate;
    procedure SetLoopCount;
    procedure FinishUpdatePreview;
  public
    LazPaintInstance: TLazPaintCustomInstance;
    constructor Create(ASurface: TBGRAVirtualScreen; AStatus: TLabel; AAnimate: boolean);
    destructor Destroy; override;
    procedure UpdatePreview;
    procedure HandleTimer;
    property Filename: string read FFilename write SetFilename;
    property PreviewDataLoss: boolean read GetPreviewDataLoss;
    property OnValidate: TNotifyEvent read FOnValidate write FOnValidate;
    property OnEscape: TNotifyEvent read FOnEscape write FOnEscape;
    property EntryCount: integer read GetEntryCount;
    function GetPreviewBitmap: TImageEntry;
    property DuplicateEntrySourceIndex: integer read FDuplicateEntrySourceIndex write FDuplicateEntrySourceIndex;
    property ScaledIconSize: integer read GetScaledIconSize;
  end;

implementation

uses FPimage, BGRAReadJpeg, BGRAOpenRaster, BGRAPaintNet, BGRAReadLzp, Dialogs, UNewimage,
  LCLType, BGRAPhoxo, BGRASVG, math, URaw, UImage, LCScaleDPI, BGRAUnits;

{ TImagePreview }

function TImagePreview.GetScaledIconSize: integer;
begin
  result := round(IconSize * FScaling);
end;

function TImagePreview.GetPreviewDataLoss: boolean;
begin
  FinishUpdatePreview;
  result := (FImageFormat in[ifJpeg,     {compression loss}
                             ifLazPaint, {layer loss}
                             ifOpenRaster,
                             ifSvg,      {vector loss}
                             ifPhoxo,
                             ifPaintDotNet])
         or (FAnimate and Assigned(FAnimatedGif) and (FAnimatedGif.Count > 1)); {frame loss}
end;

procedure TImagePreview.SetFilename(AValue: string);
begin
  if FFilename=AValue then Exit;
  FFilename:=AValue;
  UpdatePreview;
end;

procedure TImagePreview.SurfaceRedraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  FScaling := FSurface.GetCanvasScaleFactor;
  TVolatileScrollBar.InitDPI(FScaling);
  FSurfaceScaledHeight := Bitmap.Height;
  if (Bitmap.Width = 0) or (Bitmap.Height = 0) then
  begin
    ClearMenu;
    exit;
  end;
  if (CanAddNewEntry or (GetEntryCount > 1)) and not (Assigned(FAnimatedGif) and FAnimate) then
    DrawMenu(Bitmap)
  else
    DrawCurrentFrame(Bitmap);
end;


procedure TImagePreview.SurfaceMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
  scrollPos: integer;
begin
  X := round(X*FScaling);
  Y := round(Y*FScaling);
  if (Button = mbLeft) and Assigned(FScrollbar) and FScrollbar.MouseDown(X,Y) then
  begin
    FScrolling:= true;
    FSurface.DiscardBitmap;
  end else
  begin
    if Assigned(FScrollbar) then
      scrollPos := FScrollbar.Position
    else scrollPos := 0;

    for i := 0 to high(FImageMenu) do
      if PtInRect(Point(x,y+scrollPos), FImageMenu[i].DeleteArea) then
      begin
        DeleteEntry(FImageMenu[i].FrameIndex);
        exit;
      end;

    for i := 0 to high(FImageMenu) do
      if PtInRect(Point(x,y+scrollPos), FImageMenu[i].Area) then
      begin
        FSelectedMenuIndex:= i;
        ScrollToSelectedMenu;
        break;
      end;
  end;
end;

procedure TImagePreview.SurfaceMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  i, scrollPos: Integer;
begin
  X := round(X*FScaling);
  Y := round(Y*FScaling);
  if FScrolling and Assigned(FScrollbar) and FScrollbar.MouseMove(X,Y) then
     FSurface.DiscardBitmap else
  begin
    if Assigned(FScrollbar) then
      scrollPos := FScrollbar.Position
    else scrollPos := 0;

    for i := 0 to high(FImageMenu) do
      if PtInRect(Point(x,y+scrollPos), FImageMenu[i].DeleteArea) then
      begin
        FSurface.Cursor := crHandPoint;
        exit;
      end;
    FSurface.Cursor := crDefault;
  end;
end;

procedure TImagePreview.SurfaceMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  X := round(X*FScaling);
  Y := round(Y*FScaling);
  if (Button = mbLeft) and FScrolling and Assigned(FScrollbar) and FScrollbar.MouseUp(X,Y) then
  begin
     FSurface.DiscardBitmap;
     FScrolling:= false;
  end;
end;

procedure TImagePreview.SurfaceMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if Assigned(FScrollbar) then
  begin
    FScrollbar.Position := FScrollbar.Position - WheelDelta*32 div 120;
    FSurface.DiscardBitmap;
    Handled := true;
  end;
end;

procedure TImagePreview.SurfaceDblClick(Sender: TObject);
begin
  DoValidate;
end;

procedure TImagePreview.SurfaceKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  If Key = VK_UP then
  begin
    Key := 0;
    if FSelectedMenuIndex > 0 then
    begin
      dec(FSelectedMenuIndex);
      ScrollToSelectedMenu;
    end;
  end else
  if Key = VK_DOWN then
  begin
    Key := 0;
    if FSelectedMenuIndex < High(FImageMenu) then
    begin
      inc(FSelectedMenuIndex);
      ScrollToSelectedMenu;
    end;
  end else
  if Key = VK_HOME then
  begin
    Key := 0;
    if (FSelectedMenuIndex <> 0) and (length(FImageMenu) > 0) then
    begin
      FSelectedMenuIndex:= 0;
      ScrollToSelectedMenu;
    end;
  end else
  if Key = VK_END then
  begin
    Key := 0;
    if (FSelectedMenuIndex <> high(FImageMenu)) and (length(FImageMenu) > 0) then
    begin
      FSelectedMenuIndex:= high(FImageMenu);
      ScrollToSelectedMenu;
    end;
  end else
  if Key = VK_RETURN then
  begin
    Key := 0;
    DoValidate;
  end else
  if Key = VK_ESCAPE then
  begin
    Key := 0;
    if Assigned(FOnEscape) then FOnEscape(self);
  end else
  if Key = VK_DELETE then
  begin
    Key := 0;
    if (FSelectedMenuIndex >= 0) and (FSelectedMenuIndex <= high(FImageMenu)) and
      not FImageMenu[FSelectedMenuIndex].IsNew and
      not FImageMenu[FSelectedMenuIndex].IsDuplicate then
    begin
      DeleteEntry(FImageMenu[FSelectedMenuIndex].FrameIndex);
    end;
  end;
end;

procedure TImagePreview.DrawCurrentFrame(Bitmap: TBGRABitmap);
var x,y,w,h,ofs: integer;
  frame: TBGRABitmap;
  checkerScale: single;
begin
  ClearMenu;
  frame := GetCurrentFrameBitmap;
  if Assigned(frame) then
  begin
    w := frame.Width;
    h := frame.Height;
  end
  else
    exit;

  w := round(w*FScaling);
  h := round(h*FScaling);

  if w > bitmap.Width then
  begin
    h := round(h/w*bitmap.Width);
    w := bitmap.Width;
  end;
  if h > bitmap.Height then
  begin
    w := round(w/h*bitmap.Height);
    h := bitmap.Height;
  end;
  x := (bitmap.Width-w) div 2;
  y := (bitmap.Height-h) div 2;
  checkerScale := DoScaleX(round(60*FScaling), OriginalDPI)/60;
  ofs := round(4*checkerScale);
  if w < ofs then ofs := w;
  if h < ofs then ofs := h;
  bitmap.FillRect(rect(x+w,y+ofs,x+ofs+w,y+ofs+h), BGRA(0,0,0,128),dmDrawWithTransparency);
  bitmap.FillRect(rect(x+ofs,y+h,x+w,y+ofs+h), BGRA(0,0,0,128),dmDrawWithTransparency);

  DrawThumbnailCheckers(Bitmap, rect(x,y,x+w,y+h), false, checkerScale);
  bitmap.StretchPutImage(rect(x,y,x+w,y+h), frame, dmDrawWithTransparency)
end;

procedure TImagePreview.DrawMenu(Bitmap: TBGRABitmap);

  procedure DrawSheet(x,y,sw,sh: single);
  var
    ptsF,ptsF2: ArrayOfTPointF;
    j: integer;
  begin
    ptsF := PointsF([PointF(x+sw*0.20,y+sh*0.1),PointF(x+sw*0.55,y+sh*0.1),PointF(x+sw*0.75,y+sh*0.3),
                     PointF(x+sw*0.75,y+sh*0.9),PointF(x+sw*0.20,y+sh*0.9)]);
    setlength(ptsF2,length(ptsF));
    for j := 0 to high(ptsF) do
        ptsF2[j] := ptsF[j] + PointF(3,3);
    bitmap.FillPolyAntialias(ptsF2, BGRA(0,0,0,96));
    bitmap.FillPolyAntialias(ptsF, BGRAWhite);
    bitmap.DrawPolygonAntialias(ptsF, BGRABlack, 1.5);
    bitmap.DrawPolyLineAntialias([PointF(x+sw*0.55,y+sh*0.1),PointF(x+sw*0.55,y+sh*0.3),PointF(x+sw*0.75,y+sh*0.3)], BGRABlack,1.5);
  end;

var scrollPos, totalHeight, maxScroll, availableWidth: integer;
  i: integer;
  x,y,sw,sh: integer;
  textRight, bpp: integer;
  iconCaption: string;
  scrolledArea, inter: TRect;
begin
  if (Bitmap.Width < 8) or (Bitmap.Height < 8) or (GetEntryCount = 0) then exit;
  if Assigned(FScrollbar) then
  begin
    scrollPos := FScrollbar.Position;
  end else
    scrollPos := 0;

  if not FScrolling then
  begin
    FreeAndNil(FScrollbar);
    availableWidth := Bitmap.Width;
    totalHeight := TryMenuLayout(availableWidth, 1, 0);
    if (totalHeight > Bitmap.Height) and (GetEntryCount > 1) and (Bitmap.Width >= 500) then
    begin
      totalHeight := TryMenuLayout(availableWidth, 2, totalHeight div 2);
    end;
    maxScroll := totalHeight-Bitmap.Height;
    if maxScroll < 0 then maxScroll := 0;
    if scrollPos > maxScroll then scrollPos := maxScroll;

    if (totalHeight > Bitmap.Height) and (Bitmap.Width > 8+VolatileScrollBarSize) and
      (Bitmap.Height > VolatileThumbSize) then
    begin
      availableWidth -= VolatileScrollBarSize;
      FScrollbar := TVolatileScrollBar.Create(availableWidth,0,VolatileScrollBarSize,Bitmap.Height,sbVertical,
                                              scrollPos,0,maxScroll);
      totalHeight := TryMenuLayout(availableWidth, 1, 0);
      if (totalHeight > Bitmap.Height) and (GetEntryCount > 1) and (Bitmap.Width >= 500) then
      begin
        totalHeight := TryMenuLayout(availableWidth, 2, totalHeight div 2);
      end;
    end else
    begin
      scrollPos := 0;
    end;
  end else
  begin
    availableWidth := Bitmap.Width;
    if Assigned(FScrollbar) then
      availableWidth -= VolatileScrollBarSize;
  end;

  if FSelectedMenuIndex >= length(FImageMenu) then
    FSelectedMenuIndex:= -1;
  if (FSelectedMenuIndex = -1) and (length(FImageMenu) > 0) then
  begin
    FSelectedMenuIndex:= 0;
    while (FSelectedMenuIndex < length(FImageMenu)) and
      (FImageMenu[FSelectedMenuIndex].IsNew or FImageMenu[FSelectedMenuIndex].IsDuplicate
       or FImageMenu[FSelectedMenuIndex].IsLoopCount) do
      inc(FSelectedMenuIndex);
    //do not select special entries by default
  end;

  for i := 0 to high(FImageMenu) do
  with FImageMenu[i] do
  begin
    DeleteArea := EmptyRect;
    textRight := availableWidth;
    scrolledArea := rect(Area.Left, Area.Top-scrollPos, Area.Right, Area.Bottom-scrollPos);
    inter := RectInter(scrolledArea, bitmap.ClipRect);
    if (inter.Width = 0) or (inter.Height = 0) then continue;

    if i = FSelectedMenuIndex then
    begin
      bitmap.FillRect(scrolledArea, ColorToRGB(clHighlight));
      if not IsNew and not IsLoopCount and not IsDuplicate and (Area.Right - IconArea.Right > ScaledIconSize) and CanDeleteEntry(FrameIndex) then
      begin
        sh := (Area.Right - IconArea.Right - 8) div 4;
        if sh < ScaledIconSize div 2 then sh := ScaledIconSize div 2;
        if sh > ScaledIconSize then sh := ScaledIconSize;
        if sh > Area.Bottom-Area.Top-4 then sh := Area.Bottom-Area.Top-4;
        sw := sh;
        DeleteArea := RectWithSize(Area.Right-8-sw,(Area.Top+Area.Bottom-sh) div 2, sw,sh);
        bitmap.DrawLineAntialias(DeleteArea.Left+3,DeleteArea.Top+3-scrollPos,DeleteArea.Right-4,DeleteArea.Bottom-4-scrollPos,BGRABlack,6);
        bitmap.DrawLineAntialias(DeleteArea.Left+3,DeleteArea.Bottom-4-scrollPos,DeleteArea.Right-4,DeleteArea.Top+3-scrollPos,BGRABlack,6);
        bitmap.DrawLineAntialias(DeleteArea.Left+3,DeleteArea.Bottom-4-scrollPos,DeleteArea.Right-4,DeleteArea.Top+3-scrollPos,CSSRed,4);
        bitmap.DrawLineAntialias(DeleteArea.Left+3-1.5,DeleteArea.Bottom-4-1.5-scrollPos,DeleteArea.Right-4-1.5,DeleteArea.Top+3-1.5-scrollPos,BGRA(255,255,255,128),1);
        bitmap.FillEllipseAntialias(DeleteArea.Left+3,DeleteArea.Top+3-scrollPos,2,2, CSSRed);
        bitmap.FillEllipseAntialias(DeleteArea.Left+3,DeleteArea.Top+3-scrollPos,2,2, BGRA(255,255,255,128));
        bitmap.FillEllipseAntialias(DeleteArea.Right-4,DeleteArea.Bottom-4-scrollPos,2,2, CSSRed);
        bitmap.FillEllipseAntialias(DeleteArea.Right-4,DeleteArea.Bottom-4-scrollPos,2,2, BGRA(0,0,0,128));
        bitmap.DrawLineAntialias(DeleteArea.Left+4,DeleteArea.Top+4-scrollPos,DeleteArea.Right-5,DeleteArea.Bottom-5-scrollPos,CSSRed,4);
        textRight := DeleteArea.Left;
      end;
    end;

    x := IconArea.left;
    y := IconArea.Top;
    sw := IconArea.Right-x;
    sh := IconArea.Bottom-y;
    y -= scrollPos;

    if IsLoopCount then
    begin
      bitmap.EllipseAntialias(x+sw*0.5,y+sw*0.5, sw*0.3,sh*0.3, BGRABlack, sw*0.2);
      bitmap.EllipseAntialias(x+sw*0.5,y+sw*0.5, sw*0.3,sh*0.3, CSSGreen, sw*0.1);
    end else
    if IsNew then
    begin
      DrawSheet(x,y,sw,sh);
    end else
    if IsDuplicate then
    begin
      DrawSheet(x-sw*0.15,y-sh*0.1,sw,sh*0.9);
      DrawSheet(x+sw*0.1,y+sh*0.1,sw,sh*0.9);
      bitmap.FontFullHeight:= round(sh*0.7);
    end else
    begin
      bitmap.FillRect(rect(x+2,y+2, x+sw+2,y+sh+2), BGRA(0,0,0,96), dmDrawWithTransparency);
      bitmap.StretchPutImage(rect(x,y,x+sw,y+sh), GetEntryThumbnail(FrameIndex, sw,sh), dmDrawWithTransparency);
    end;

    if IsNew then iconCaption := rsNewImage else
    if IsDuplicate then iconCaption := rsDuplicateImage else
    if IsLoopCount then
    begin
      iconCaption:= rsLoopCount+': ';
      if FAnimatedGif.LoopCount = 0 then
        iconCaption += rsInfinity
      else
        iconCaption += inttostr(FAnimatedGif.LoopCount);
    end else
    begin
       if Assigned(FAnimatedGif) then
       begin
         iconCaption := '#' + inttostr(FrameIndex+1) + ', ' + inttostr(FAnimatedGif.FrameDelayMs[FrameIndex])+' ms';
       end else
       begin
         iconCaption := inttostr(GetEntryWidth(FrameIndex))+'x'+inttostr(GetEntryHeight(FrameIndex));
         if Assigned(FTiff) then iconCaption += ' #' + inttostr(FrameIndex+1)
         else if Assigned(FIconCursor) then
         begin
           bpp := GetEntryBitDepth(FrameIndex);
           if bpp <> 0 then iconCaption += ' '+inttostr(bpp)+'bit';
         end;
       end;
    end;

    if (y+16 < bitmap.height) and (y+sh-16 > 0) then
      NiceText(bitmap, x+sw+4,y+sh div 2, textRight,bitmap.height,
                       iconCaption, taLeftJustify, tlCenter);
  end;

  if Assigned(FScrollbar) then FScrollbar.Draw(Bitmap);
end;

function TImagePreview.TryMenuLayout(AWidth: integer; AColCount, ABottom: integer): integer;
var x,y,i,frameIndex,h,w,sw,sh: integer;
  newItem, LoopCountItem, DuplicateItem,
  colLeft,colRight, maxWidth, maxHeight: integer;
  currentCol: integer;

  procedure ComputeColumn;
  var
    scaledSubImageSize: integer;
  begin
    colLeft := (AWidth*currentCol) div AColCount;
    colRight := (AWidth*(currentCol+1)) div AColCount;
    x := colLeft+2;
    y := 2;
    maxWidth := colRight-colLeft-8;

    scaledSubImageSize := round(SubImageSize*FScaling);
    if maxWidth > scaledSubImageSize then maxWidth := scaledSubImageSize;
    maxHeight := scaledSubImageSize;
  end;

begin
  ClearMenu;
  currentCol := 0;

  ComputeColumn;
  result := y+2;

  if Assigned(FAnimatedGif) then LoopCountItem := 1 else LoopCountItem:= 0;
  if CanAddNewEntry then NewItem := 1 else NewItem := 0;
  if CanDuplicateEntry then DuplicateItem := 1 else DuplicateItem := 0;
  setlength(FImageMenu, GetEntryCount + LoopCountItem + NewItem + DuplicateItem);
  for i := 0 to high(FImageMenu) do
  begin
    if (LoopCountItem = 1) and (i = 0) then
    begin
      frameIndex := -1;
      FImageMenu[i].IsLoopCount := true;
      w := ScaledIconSize;
      h := w;
    end else
    if (NewItem = 1) and (i = LoopCountItem) then
    begin
      frameIndex := GetEntryCount;
      FImageMenu[i].IsNew := true;
      w := ScaledIconSize;
      h := w;
    end
    else
    if (DuplicateItem = 1) and (i = LoopCountItem + NewItem) then
    begin
      frameIndex := GetEntryCount;
      FImageMenu[i].IsDuplicate := true;
      w := ScaledIconSize;
      h := w;
    end
    else
    begin
      frameIndex := i-NewItem-LoopCountItem-DuplicateItem;
      w := round(GetEntryWidth(frameIndex)*FScaling);
      h := round(GetEntryHeight(frameIndex)*FScaling);
    end;
    if w > maxWidth then
    begin
      sw := maxWidth;
      sh := round(h/w*maxWidth);
      if (sh = 0) and (h <> 0) then sh := 1;
    end else
    begin
      sw := w;
      sh := h;
    end;
    if sh > maxHeight then
    begin
      sw := round(sw/sh*maxHeight);
      sh := maxHeight;
      if (sw = 0) and (w <> 0) then sw := 1;
    end;

    FImageMenu[i].FrameIndex:= frameIndex;
    FImageMenu[i].Area := rect(x,y,colRight-2,y+sh+4);
    FImageMenu[i].IconArea := rect(x+2,y+2,x+2+sw,y+2+sh);

    y += sh+4;
    if y+2 > result then result := y+2;
    if (y+2 > ABottom) and (currentCol+1 < AColCount) then
    begin
      currentCol += 1;
      ComputeColumn;
    end;
  end;
end;

function TImagePreview.CanAddNewEntry: boolean;
begin
  result := Assigned(FIconCursor) or Assigned(FTiff) or Assigned(FAnimatedGif);
end;

function TImagePreview.CanDuplicateEntry: boolean;
begin
  result := (Assigned(FTiff) or Assigned(FAnimatedGif)) and
    (FDuplicateEntrySourceIndex >= 0) and (FDuplicateEntrySourceIndex < EntryCount);
end;

function TImagePreview.GetEntryCount: integer;
begin
  if Assigned(FIconCursor) then
    result := FIconCursor.Count
  else if Assigned(FTiff) then
    result := FTiff.Count
  else if Assigned(FAnimatedGif) and not FAnimate then
    result := FAnimatedGif.Count
  else
    result := 1;
end;

function TImagePreview.GetEntryWidth(index: integer): integer;
begin
  if Assigned(FIconCursor) then
    result := FIconCursor.Width[index]
  else if Assigned(FTiff) then
    result := FTiff.Entry[index].Width
  else if Assigned(FAnimatedGif) then
    result := FAnimatedGif.Width
  else if Assigned(FSingleImage) then
    result := FSingleImage.Width;
end;

function TImagePreview.GetEntryHeight(index: integer): integer;
begin
  if Assigned(FIconCursor) then
    result := FIconCursor.Height[index]
  else if Assigned(FTiff) then
    result := FTiff.Entry[index].Height
  else if Assigned(FAnimatedGif) then
    result := FAnimatedGif.Height
  else if Assigned(FSingleImage) then
    result := FSingleImage.Height;
end;

function TImagePreview.GetEntryBitDepth(index: integer): integer;
begin
  if Assigned(FIconCursor) then
    result := FIconCursor.BitDepth[index]
  else if Assigned(FTiff) then
    result := FTiff.Entry[index].BitDepth
  else
    result := 0;
end;

function TImagePreview.GetEntryBitmap(index: integer): TImageEntry;
var
  mem: TMemoryStream;
begin
  if (index < 0) or (index >= GetEntryCount) then
    raise exception.Create('Index out of bounds');
  result := TImageEntry.Empty;
  result.frameCount:= EntryCount;
  try
    if Assigned(FIconCursor) then
    begin
      result.bmp := FIconCursor.GetBitmap(index) as TBGRABitmap;
      result.bpp := FIconCursor.BitDepth[index];
      result.frameIndex := index;
    end
    else
    if Assigned(FTiff) then
    begin
      mem := TMemoryStream.Create;
      try
        FTiff.SaveToStream(mem, [index]);
        mem.Position:= 0;
        result.bmp := TBGRABitmap.Create(mem);
        result.bpp := FTiff.Entry[index].BitDepth;
        result.frameIndex := index;
      finally
        mem.Free;
      end;
    end else
    if Assigned(FAnimatedGif) and not FAnimate then
    begin
      FAnimatedGif.CurrentImage := index;
      result.bmp := FAnimatedGif.MemBitmap.Duplicate as TBGRABitmap;
      result.frameIndex := index;
    end;
  except on ex:exception do
    begin
      if result.bmp = nil then
      begin
        result.bmp := TBGRABitmap.Create(GetEntryWidth(index), GetEntryHeight(index), BGRAWhite);
        result.bpp := GetEntryBitDepth(index);
        result.frameIndex:= index;
      end;
    end;
  end;
end;

procedure TImagePreview.SetEntryBitmap(var AEntry: TImageEntry);
var
  sAddedTiff: TMemoryStream;
  addedTiff: TTiff;
  sOut: TStream;
begin
  if (AEntry.frameIndex < 0) or (AEntry.frameIndex > GetEntryCount) then
    raise exception.Create('Index out of bounds');
  if Filename = '' then raise exception.create('Filename undefined');

  AEntry.frameCount:= GetEntryCount;

  if Assigned(FTiff) then
  begin
    addedTiff := TTiff.Create;
    sAddedTiff := TMemoryStream.Create;
    try
      AEntry.bmp.SaveToStreamAs(sAddedTiff, ifTiff);
      sAddedTiff.Position:= 0;
      if addedTiff.LoadFromStream(sAddedTiff) <> teNone then
        raise Exception.Create(rsInternalError);
      if AEntry.frameIndex > FTiff.Count then
        AEntry.frameIndex := FTiff.Count;
      FTiff.Move(addedTiff,0, AEntry.frameIndex);

      sOut := FileManager.CreateFileStream(Filename,fmCreate);
      try
        FTiff.SaveToStream(sOut);
      finally
        sOut.Free;
      end;
    finally
      sAddedTiff.Free;
      addedTiff.Free;
    end;
  end else
  if Assigned(FAnimatedGif) then
  begin
    if AEntry.frameIndex >= FAnimatedGif.Count then
      AEntry.frameIndex := FAnimatedGif.AddFullFrame(AEntry.bmp, FAnimatedGif.AverageDelayMs)
    else
      FAnimatedGif.ReplaceFullFrame(AEntry.frameIndex, AEntry.bmp, FAnimatedGif.FrameDelayMs[AEntry.frameIndex]);

    sOut := FileManager.CreateFileStream(Filename,fmCreate);
    try
      FAnimatedGif.SaveToStream(sOut);
    finally
      sOut.Free;
    end;
  end;
end;

function TImagePreview.GetEntryThumbnail(index: integer; stretchWidth, stretchHeight: integer): TBGRABitmap;
var
  entry: TImageEntry;
begin
  if (index < FThumbnails.Count) and Assigned(FThumbnails[index]) then
  begin
    result := FThumbnails[index];
    exit;
  end;
  entry := GetEntryBitmap(index);
  if Assigned(entry.bmp) then
  begin
    try
      if Assigned(FIconCursor) then
        result := GetBitmapThumbnail(entry.bmp, FIconCursor.FileType,stretchWidth,stretchHeight, BGRAPixelTransparent,true)
      else
        result := GetBitmapThumbnail(entry.bmp, stretchWidth,stretchHeight, BGRAPixelTransparent,true);
      while FThumbnails.Count < index+1 do FThumbnails.Add(nil);
      FThumbnails[index] := result;
    finally
      entry.bmp.free;
    end;
  end else
    result := nil;
end;

function TImagePreview.CanDeleteEntry(index: integer): boolean;
begin
  result := (Assigned(FIconCursor) or Assigned(FTiff) or Assigned(FAnimatedGif)) and
            (index >= 0) and (index < GetEntryCount) and
            (GetEntryCount > 1);
end;

function TImagePreview.GetCurrentFrameBitmap: TBGRABitmap;
begin
  if Assigned(FSingleImage) then
    result := FSingleImage
  else if Assigned(FAnimatedGif) then
    result := FAnimatedGif.MemBitmap
  else if Assigned(FTiff) then
  begin
    FSingleImage := GetEntryBitmap(0).bmp;
    result := FSingleImage;
  end
  else
    result := nil;
end;

procedure TImagePreview.ClearMenu;
begin
  FImageMenu := nil;
end;

procedure TImagePreview.ClearThumbnails;
begin
  FThumbnails.Clear;
end;

procedure TImagePreview.DoValidate;
begin
  if (FSelectedMenuIndex >= 0) and (FSelectedMenuIndex < length(FImageMenu)) and
     FImageMenu[FSelectedMenuIndex].IsLoopCount then
  begin
    SetLoopCount;
  end else
    if Assigned(FOnValidate) then FOnValidate(self);
end;

procedure TImagePreview.SetLoopCount;
var newLoopCount: Word;
  errPos: integer;
  outputStream: TStream;
begin
  if Assigned(FAnimatedGif) then
  begin
    val(InputBox(rsAnimatedGIF, rsLoopCount+' (0='+ rsInfinity+'):', inttostr(FAnimatedGif.LoopCount)), newLoopCount, errPos);
    if errPos = 0 then
    begin
      if newLoopCount > 65534 then newLoopCount:= 0;

      if newLoopCount <> FAnimatedGif.LoopCount then
      begin
        FAnimatedGif.LoopCount := newLoopCount;
        outputStream := nil;
        try
          outputStream := FileManager.CreateFileStream(Filename, fmCreate);
          FAnimatedGif.SaveToStream(outputStream);
          FreeAndNil(outputStream);
        except
          on ex:exception do
          begin
            if Assigned(outputStream) then
              FileManager.CancelStreamAndFree(outputStream);
            ShowMessage(ex.Message);
          end;
        end;
        outputStream.Free;
        FSurface.DiscardBitmap;
      end;
    end;
  end;
end;

procedure TImagePreview.FinishUpdatePreview;
var reader: TFPCustomImageReader;
  jpegReader: TBGRAReaderJpeg;
  source: TStream;
  svg: TBGRASVG;
  tr: TTiffError;
  screenDpi: Integer;
  singleSize: string;
begin
  if FInUpdatePreview then
  begin
    source := nil;
    singleSize := '';
    try
      source := FileManager.CreateFileStream(FFilename, fmOpenRead or fmShareDenyWrite);
      FImageFormat := DetectFileFormat(source,ExtractFileExt(FFilename));
      if IsRawFilename(FFilename) then
      begin
        try
          FSingleImage := GetRawStreamImage(source);
          FImageNbLayers := 1;
        except
          on ex: Exception do
          begin
            FLoadError:= ex.Message;
            FreeAndNil(FSingleImage);
          end;
        end;
      end else
      case FImageFormat of
      ifGif:
        begin
          try
            FAnimatedGif := TBGRAAnimatedGif.Create;
            FAnimatedGif.LoadFromStream(source);
            FImageNbLayers := 1;
          except
            on ex: Exception do
            begin
              FLoadError := ex.Message;
              FreeAndNil(FAnimatedGif);
            end;
          end;
        end;
      ifTiff:
        begin
          try
            FTiff := TTiff.Create;
            tr := FTiff.LoadFromStream(source);
            if tr <> teNone then
              raise exception.Create(rsCannotOpenFile+' (TIFF '+inttostr(ord(tr))+')');

            FImageNbLayers := 1;
            if FTiff.Count = 0 then
            begin
              FreeAndNil(FTiff);
              FLoadError := rsFileCannotBeEmpty;
            end;
          except
            on ex: Exception do
            begin
              FLoadError := ex.Message;
              FreeAndNil(FTiff);
            end;
          end;
        end;
      ifIco,ifCur:
        begin
          FIconCursor := TBGRAIconCursor.Create;
          try
            FIconCursor.LoadFromStream(source);
            FImageNbLayers := 1;
          except
            on ex: Exception do
            begin
              FLoadError:= ex.Message;
              FreeAndNil(FIconCursor);
            end;
          end;
        end;
      ifJpeg:
        begin
          jpegReader := TBGRAReaderJpeg.Create;
          jpegReader.Performance := jpBestSpeed;
          jpegReader.MinWidth := Screen.Width;
          jpegReader.MinHeight := Screen.Height;
          try
            FSingleImage := TBGRABitmap.Create;
            FSingleImage.LoadFromStream(source,jpegReader);
            FImageNbLayers := 1;
          except
            on ex: Exception do
            begin
              FLoadError:= ex.Message;
              FreeAndNil(FSingleImage);
            end;
          end;
          jpegReader.Free;
        end;
      ifSvg:
        begin
          svg := TBGRASVG.Create(source);
          singleSize := svg.Units.formatValue(svg.Width) + ' x ' + svg.Units.formatValue(svg.Height);
          FImageNbLayers:= max(1, svg.LayerCount);
          screenDpi:= Screen.PixelsPerInch * CanvasScale;
          svg.Units.ContainerWidth := FloatWithCSSUnit(Screen.Width * CanvasScale / screenDpi * svg.DefaultDpi, cuPixel);
          svg.Units.ContainerHeight := FloatWithCSSUnit(Screen.Height * CanvasScale / screenDpi * svg.DefaultDpi, cuPixel);
          svg.CropToViewBox(screenDpi / svg.DefaultDpi);
          with ComputeAcceptableImageSize(floor(svg.WidthAsPixel + 0.95), floor(svg.HeightAsPixel + 0.95)) do
            FSingleImage := TBGRABitmap.Create(cx,cy);
          svg.StretchDraw(FSingleImage.Canvas2d,0,0,FSingleImage.Width,FSingleImage.Height);
          svg.Free;
        end
      else
        begin
          reader := CreateBGRAImageReader(FImageFormat);
          try
            FSingleImage := TBGRABitmap.Create;
            FSingleImage.LoadFromStream(source,reader);
            if reader is TFPReaderOpenRaster then FImageNbLayers := TFPReaderOpenRaster(reader).NbLayers else
            if reader is TFPReaderPaintDotNet then FImageNbLayers := TFPReaderPaintDotNet(reader).NbLayers else
            if reader is TBGRAReaderLazPaint then FImageNbLayers := TBGRAReaderLazPaint(reader).NbLayers else
            if reader is TBGRAReaderOXO then FImageNbLayers := TBGRAReaderOXO(reader).NbLayers else
              FImageNbLayers := 1;
          except
            on ex: Exception do
            begin
              FLoadError:= ex.Message;
              FreeAndNil(FSingleImage);
            end;
          end;
          reader.Free;
        end;
      end;
    except
      on ex: Exception do
        FLoadError:= ex.Message;
    end;
    source.Free;

    if Assigned(FIconCursor) then
    begin
      if FIconCursor.Count > 0 then
        FStatus.Caption := rsCanvasSize + ': ' + IntToStr(FIconCursor.Width[0])+'x'+IntToStr(FIconCursor.Height[0])+ ', ' +
                           rsEntries + ': ' + IntToStr(FIconCursor.Count)
      else
        FStatus.Caption := rsEntries + ': ' + IntToStr(FIconCursor.Count);
    end else
    if Assigned(FAnimatedGif) then
    begin
      FStatus.Caption := rsCanvasSize + ': ' + IntToStr(FAnimatedGif.Width)+'x'+IntToStr(FAnimatedGif.Height)+', '+
                         rsFrames+': '+IntToStr(FAnimatedGif.Count);
    end else
    if Assigned(FTiff) then
    begin
      with FTiff.GetBiggestImage do
        FStatus.Caption := rsCanvasSize + ': ' + IntToStr(Width)+'x'+IntToStr(Height)+', '+
                           rsEntries+': '+IntToStr(FTiff.Count);
    end else
    if Assigned(FSingleImage) then
    begin
      if singleSize = '' then singleSize := IntToStr(FSingleImage.Width)+'x'+IntToStr(FSingleImage.Height);
      FStatus.Caption := rsCanvasSize + ': ' + singleSize +', '+
                         rsLayers+': '+IntToStr(FImageNbLayers);
    end else
    if FLoadError <> '' then
    begin
      FStatus.Caption := FLoadError;
    end else
      FStatus.Caption := '';

    FInUpdatePreview := false;
    FSurface.RedrawBitmap;
  end;
end;

procedure TImagePreview.DeleteEntry(i: integer);
var outputStream: TStream;
begin
  if (assigned(FIconCursor) or assigned(FTiff) or assigned(FAnimatedGif)) and (i < GetEntryCount) and (i >= 0) then
  begin
    if GetEntryCount = 1 then
    begin
      ShowMessage(rsFileCannotBeEmpty);
    end else
    if QuestionDlg (rsDeleteFile,rsDeleteImageEntry,mtConfirmation,[mrOk,rsOkay,mrCancel,rsCancel],'') = mrOk then
    begin
      try
        if assigned(FIconCursor) then
          FIconCursor.Delete(i)
        else if assigned(FTiff) then
          FTiff.Delete(i)
        else if assigned(FAnimatedGif) then
          FAnimatedGif.DeleteFrame(i, true);

        if FThumbnails.Count >= i+1 then
          FThumbnails.Delete(i);
        outputStream := FileManager.CreateFileStream(Filename, fmCreate);
        try
          if assigned(FIconCursor) then
            FIconCursor.SaveToStream(outputStream)
          else if assigned(FTiff) then
            FTiff.SaveToStream(outputStream)
          else if assigned(FAnimatedGif) then
            FAnimatedGif.SaveToStream(outputStream);
          outputStream.Free;

          if (LazPaintInstance.Image.currentFilenameUTF8 = Filename) and
             (LazPaintInstance.Image.FrameIndex >= i) then
          begin
            if LazPaintInstance.Image.FrameIndex > i then
              dec(LazPaintInstance.Image.FrameIndex)
            else
              LazPaintInstance.Image.FrameIndex := TImageEntry.NewFrameIndex;
            LazPaintInstance.Image.OnImageChanged.NotifyObservers;
          end;
          dec(LazPaintInstance.Image.FrameCount);
        except on ex: Exception do
          begin
            FileManager.CancelStreamAndFree(outputStream);
            ShowMessage(ex.Message);
          end;
        end;
        if FSelectedMenuIndex = high(FImageMenu) then
          dec(FSelectedMenuIndex);
        FSurface.RedrawBitmap;
      except
        on ex:exception do
          ShowMessage(ex.Message);
      end;

    end;
  end;
end;

procedure TImagePreview.ScrollToSelectedMenu;
var
  scrollPos: Integer;
begin
  if Assigned(FScrollbar) then
    scrollPos := FScrollbar.Position
  else scrollPos := 0;

  if (FSelectedMenuIndex >= 0) and (FSelectedMenuIndex <= high(FImageMenu)) then
  begin
    if scrollPos < FImageMenu[FSelectedMenuIndex].Area.Bottom-FSurfaceScaledHeight then
      scrollPos := FImageMenu[FSelectedMenuIndex].Area.Bottom-FSurfaceScaledHeight;
    if scrollPos > FImageMenu[FSelectedMenuIndex].Area.Top then
      scrollPos := FImageMenu[FSelectedMenuIndex].Area.Top;
    if Assigned(FScrollbar) then FScrollbar.Position := scrollPos;
    FSurface.DiscardBitmap;
  end;
end;

constructor TImagePreview.Create(ASurface: TBGRAVirtualScreen; AStatus: TLabel; AAnimate: boolean);
begin
  FSurface := ASurface;
  FSurface.BitmapAutoScale:= false;
  FStatus := AStatus;
  FAnimate:= AAnimate;
  FSelectedMenuIndex := -1;
  FDuplicateEntrySourceIndex := -1;
  {$IFDEF WINDOWS}
  ASurface.Color := clAppWorkspace;
  {$ENDIF}
  FSurface.OnRedraw:= @SurfaceRedraw;
  FSurface.OnMouseDown:= @SurfaceMouseDown;
  FSurface.OnMouseMove:= @SurfaceMouseMove;
  FSurface.OnMouseUp:= @SurfaceMouseUp;
  FSurface.OnMouseWheel:= @SurfaceMouseWheel;
  FSurface.OnDblClick:= @SurfaceDblClick;
  FSurface.OnKeyDown:= @SurfaceKeyDown;
  FSingleImage := nil;
  FAnimatedGif := nil;
  FIconCursor := nil;
  FTiff := nil;
  FThumbnails := TBGRABitmapList.Create;
  FOnValidate := nil;
  FScrollbar := nil;
  FScrolling:= false;
end;

destructor TImagePreview.Destroy;
begin
  ClearMenu;
  ClearThumbnails;
  if FSurface.OnRedraw = @SurfaceRedraw then FSurface.OnRedraw:= nil;
  if FSurface.OnMouseDown = @SurfaceMouseDown then FSurface.OnMouseDown:= nil;
  if FSurface.OnMouseMove = @SurfaceMouseMove then FSurface.OnMouseMove:= nil;
  if FSurface.OnMouseUp = @SurfaceMouseUp then FSurface.OnMouseUp:= nil;
  if FSurface.OnMouseWheel = @SurfaceMouseWheel then FSurface.OnMouseWheel:= nil;
  if FSurface.OnDblClick = @SurfaceDblClick then FSurface.OnDblClick := nil;
  if FSurface.OnKeyDown = @SurfaceKeyDown then FSurface.OnKeyDown := nil;
  FreeAndNil(FSingleImage);
  FreeAndNil(FTiff);
  FreeAndNil(FAnimatedGif);
  FreeAndNil(FIconCursor);
  FreeAndNil(FThumbnails);
  FreeAndNil(FScrollbar);
  inherited Destroy;
end;

procedure TImagePreview.UpdatePreview;
begin
  ClearThumbnails;
  FreeAndNil(FSingleImage);
  FreeAndNil(FTiff);
  FreeAndNil(FAnimatedGif);
  FreeAndNil(FIconCursor);
  FImageNbLayers := 0;
  FImageFormat:= ifUnknown;
  FLoadError := '';
  ClearMenu;
  FreeAndNil(FScrollbar);
  FSelectedMenuIndex := -1;
  FSurface.RedrawBitmap;
  FStatus.Caption := rsLoading+'...';
  FStatus.Update;
  FInUpdatePreview := true;
  {$IFDEF LINUX}
  Application.ProcessMessages;
  {$ENDIF}
  FinishUpdatePreview;
end;

procedure TImagePreview.HandleTimer;
begin
  if FAnimate and assigned(FAnimatedGif) and (FAnimatedGif.TimeUntilNextImageMs <= 0) then
    FSurface.RedrawBitmap;
end;

function TImagePreview.GetPreviewBitmap: TImageEntry;
var tx,ty,bpp: integer; back: TBGRAPixel;
begin
  FinishUpdatePreview;
  result := TImageEntry.Empty;
  result.frameCount := GetEntryCount;

  if Assigned(FIconCursor) then
  begin
    if Assigned(FImageMenu) then
    begin
      if (FSelectedMenuIndex >= 0) and (FSelectedMenuIndex < length(FImageMenu)) then
      begin
        if FImageMenu[FSelectedMenuIndex].IsNew then
        begin
          if Assigned(LazPaintInstance) and ShowNewImageDlg(LazPaintInstance, true, tx,ty,bpp, back) then
          begin
            if FIconCursor.IndexOf(tx,ty,bpp)<>-1 then
              LazPaintInstance.ShowMessage(rsNewImage, rsIconImageAlreadyExists)
            else
            begin
              result.bmp := TBGRABitmap.Create(tx,ty,back);
              result.bpp := bpp;
              result.frameIndex:= TImageEntry.NewFrameIndex;
            end;
          end;
        end else
          result := GetEntryBitmap(FImageMenu[FSelectedMenuIndex].FrameIndex);
      end;
    end else
    if GetEntryCount > 0 then
      result := GetEntryBitmap(0);
  end else
  if Assigned(FTiff) then
  begin
    if FImageMenu <> nil then
    begin
      if (FSelectedMenuIndex >= 0) and (FSelectedMenuIndex < length(FImageMenu)) then
      begin
        if FImageMenu[FSelectedMenuIndex].IsNew then
        begin
          if Assigned(LazPaintInstance) and ShowNewImageDlg(LazPaintInstance, false, tx,ty,bpp, back) then
          begin
            result.bmp := TBGRABitmap.Create(tx,ty,back);
            result.frameIndex:= TImageEntry.NewFrameIndex;
          end;
        end else
        if FImageMenu[FSelectedMenuIndex].IsDuplicate then
        begin
          result := GetEntryBitmap(DuplicateEntrySourceIndex);
          result.frameIndex:= GetEntryCount;
          result.isDuplicate:= true;
          SetEntryBitmap(result);
        end
        else
          result := GetEntryBitmap(FImageMenu[FSelectedMenuIndex].FrameIndex);
      end;
    end else
    if GetEntryCount > 0 then
      result := GetEntryBitmap(0);
  end else
  if Assigned(FSingleImage) then
  begin
    result.bmp := FSingleImage;
    result.frameIndex:= 0;
    FSingleImage := nil;
  end else
  if Assigned(FAnimatedGif) then
  begin
    if FAnimate and (FAnimatedGif.Count = 1) then
      result.bmp := FAnimatedGif.MemBitmap.Duplicate as TBGRABitmap
    else
    if not FAnimate then
    begin
      if FImageMenu <> nil then
      begin
        if (FSelectedMenuIndex >= 0) and (FSelectedMenuIndex < length(FImageMenu)) then
        begin
          if FImageMenu[FSelectedMenuIndex].IsNew then
          begin
              result.bmp := TBGRABitmap.Create(FAnimatedGif.Width,FAnimatedGif.Height,BGRAPixelTransparent);
              result.frameIndex:= TImageEntry.NewFrameIndex;
          end else
          if FImageMenu[FSelectedMenuIndex].IsDuplicate then
          begin
            result := GetEntryBitmap(DuplicateEntrySourceIndex);
            result.frameIndex:= GetEntryCount;
            result.isDuplicate:= true;
            SetEntryBitmap(result);
          end
          else
            result := GetEntryBitmap(FImageMenu[FSelectedMenuIndex].FrameIndex);
        end;
      end else
      if GetEntryCount > 0 then
        result := GetEntryBitmap(0);
    end;
  end;
end;

end.

