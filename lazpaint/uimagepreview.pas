unit UImagePreview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Graphics, Controls, BGRAVirtualScreen,
  LazPaintType, UGraph, UResourceStrings, UFileSystem, Forms, UVolatileScrollBar,
  BGRABitmap, BGRAAnimatedGif, BGRAIconCursor, BGRABitmapTypes, BGRAThumbnail,
  BGRAReadTiff;

type

  { TImagePreview }

  TImagePreview = class
  protected
    FSurface: TBGRAVirtualScreen;
    FScrollbar: TVolatileScrollBar;
    FScrolling: boolean;
    FStatus: TLabel;

    FFilename: string;
    FLoadError: string;

    FImageFormat: TBGRAImageFormat;
    FImageNbLayers: integer;
    FSingleImage: TBGRABitmap;
    FAnimatedGif: TBGRAAnimatedGif;        //has frames
    FTiff: TBGRAReaderTiff;                //has entries
    FIconCursor: TBGRAIconCursor;   //has entries

    FSelectedMenuIndex: integer;
    FImageMenu: array of record
                 Area, IconArea: TRect;
                 DeleteArea: TRect;
                 FrameIndex: integer;
                 IsNew: boolean;
               end;

    FOnValidate: TNotifyEvent;
    FOnEscape: TNotifyEvent;

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
    function CanDeleteEntry(index: integer): boolean;
    procedure DeleteEntry(i: integer);
    function GetEntryCount: integer;
    function GetEntryWidth(index: integer): integer;
    function GetEntryHeight(index: integer): integer;
    function GetEntryBitDepth(index: integer): integer;
    function GetEntryThumbnail(index: integer; stretchWidth, stretchHeight: integer): TBGRABitmap;

    procedure DrawCurrentFrame(Bitmap: TBGRABitmap);
    function GetCurrentFrameBitmap: TBGRABitmap;
  public
    LazPaintInstance: TLazPaintCustomInstance;
    constructor Create(ASurface: TBGRAVirtualScreen; AStatus: TLabel);
    destructor Destroy; override;
    procedure UpdatePreview;
    procedure HandleTimer;
    property Filename: string read FFilename write SetFilename;
    property PreviewDataLoss: boolean read GetPreviewDataLoss;
    property OnValidate: TNotifyEvent read FOnValidate write FOnValidate;
    property OnEscape: TNotifyEvent read FOnEscape write FOnEscape;
    function GetPreviewBitmap: TImageEntry;
  end;

implementation

uses FPimage, BGRAReadJpeg, BGRAOpenRaster, BGRAPaintNet, BGRAReadLzp, Dialogs, UNewimage,
  LCLType;

{ TImagePreview }

function TImagePreview.GetPreviewDataLoss: boolean;
begin
  result := (FImageFormat in[ifJpeg,     {compression loss}
                             ifLazPaint, {layer loss}
                             ifOpenRaster,
                             ifPhoxo])
         or (Assigned(FAnimatedGif) and (FAnimatedGif.Count > 1)); {frame loss}
end;

procedure TImagePreview.SetFilename(AValue: string);
begin
  if FFilename=AValue then Exit;
  FFilename:=AValue;
  UpdatePreview;
end;

procedure TImagePreview.SurfaceRedraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  if (Bitmap.Width = 0) or (Bitmap.Height = 0) then
  begin
    FImageMenu := nil;
    exit;
  end;
  if CanAddNewEntry or (GetEntryCount > 1) then
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
  if Assigned(FOnValidate) then FOnValidate(self);
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
    if Assigned(FOnValidate) then FOnValidate(self);
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
      not FImageMenu[FSelectedMenuIndex].IsNew then
    begin
      DeleteEntry(FImageMenu[FSelectedMenuIndex].FrameIndex);
    end;
  end;
end;

procedure TImagePreview.DrawCurrentFrame(Bitmap: TBGRABitmap);
var x,y,w,h,ofs: integer;
  frame: TBGRABitmap;
begin
  FImageMenu := nil;
  frame := GetCurrentFrameBitmap;
  if Assigned(frame) then
  begin
    w := frame.Width;
    h := frame.Height;
  end
  else
    exit;

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
  ofs := 4;
  if w < ofs then ofs := w;
  if h < ofs then ofs := h;
  bitmap.FillRect(rect(x+w,y+ofs,x+ofs+w,y+ofs+h), BGRA(0,0,0,128),dmDrawWithTransparency);
  bitmap.FillRect(rect(x+ofs,y+h,x+w,y+ofs+h), BGRA(0,0,0,128),dmDrawWithTransparency);

  DrawThumbnailCheckers(Bitmap, rect(x,y,x+w,y+h));
  bitmap.StretchPutImage(rect(x,y,x+w,y+h), frame, dmDrawWithTransparency)
end;

procedure TImagePreview.DrawMenu(Bitmap: TBGRABitmap);
var scrollPos, totalHeight, maxScroll, availableWidth: integer;
  i,j: integer;
  temp: TBGRABitmap;
  x,y,sw,sh: integer;
  textRight, bpp: integer;
  iconCaption: string;
  ptsF,ptsF2: ArrayOfTPointF;
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
    if (length(FImageMenu)>=2) and FImageMenu[0].IsNew then
      FSelectedMenuIndex:= 1 //do not select "add new" entry by default
    else
      FSelectedMenuIndex:= 0;
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
      if not IsNew and (Area.Right - IconArea.Right > 32) and CanDeleteEntry(FrameIndex) then
      begin
        sh := (Area.Right - IconArea.Right - 8) div 4;
        if sh < 16 then sh := 16;
        if sh > 32 then sh := 32;
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

    if not IsNew then
    begin
      bitmap.FillRect(rect(x+2,y+2, x+sw+2,y+sh+2), BGRA(0,0,0,96), dmDrawWithTransparency);
      temp := GetEntryThumbnail(FrameIndex, sw,sh);
      bitmap.PutImage(x,y, temp, dmDrawWithTransparency);
      temp.free;
    end else
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

    if IsNew then iconCaption := rsNewImage else
    begin
       iconCaption := inttostr(GetEntryWidth(FrameIndex))+'x'+inttostr(GetEntryHeight(FrameIndex));
       bpp := GetEntryBitDepth(FrameIndex);
       if bpp <> 0 then iconCaption += ' '+inttostr(bpp)+'bit';
    end;

    if (y+16 < bitmap.height) and (y+sh-16 > 0) then
      NiceText(bitmap, x+sw+4,y+sh div 2, textRight,bitmap.height,
                       iconCaption, taLeftJustify, tlCenter);
  end;

  if Assigned(FScrollbar) then FScrollbar.Draw(Bitmap);
end;

function TImagePreview.TryMenuLayout(AWidth: integer; AColCount, ABottom: integer): integer;
var x,y,i,frameIndex,h,w,sw,sh: integer;
  newItem, colLeft,colRight, maxWidth, maxHeight: integer;
  currentCol: integer;

  procedure ComputeColumn;
  begin
    colLeft := (AWidth*currentCol) div AColCount;
    colRight := (AWidth*(currentCol+1)) div AColCount;
    x := colLeft+2;
    y := 2;
    maxWidth := colRight-colLeft-8;

    if maxWidth > 128 then maxWidth := 128;
    maxHeight := 128;
  end;

begin
  FImageMenu := nil;
  currentCol := 0;

  ComputeColumn;
  result := y+2;

  if CanAddNewEntry then NewItem := 1 else NewItem := 0;
  setlength(FImageMenu, GetEntryCount + NewItem);
  for i := 0 to GetEntryCount-1 + NewItem do
  begin
    if (NewItem = 1) and (i = 0) then
    begin
      frameIndex := GetEntryCount;
      FImageMenu[i].IsNew := true;
      w := 32;
      h := 32;
    end
    else
    begin
      frameIndex := i-NewItem;
      w := GetEntryWidth(frameIndex);
      h := GetEntryHeight(frameIndex);
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

  result := y+2;
end;

function TImagePreview.CanAddNewEntry: boolean;
begin
  result := Assigned(FIconCursor);
end;

function TImagePreview.GetEntryCount: integer;
begin
  if Assigned(FIconCursor) then
    result := FIconCursor.Count
  else if Assigned(FTiff) then
    result := FTiff.ImageCount
  else
    result := 1;
end;

function TImagePreview.GetEntryWidth(index: integer): integer;
begin
  if Assigned(FIconCursor) then
    result := FIconCursor.Width[index]
  else if Assigned(FTiff) then
    result := FTiff.Images[index].ImageWidth
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
    result := FTiff.Images[index].ImageHeight
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
  begin
    with FTiff.Images[index] do
      result := RedBits + GreenBits + BlueBits + GrayBits + AlphaBits;
  end
  else
    result := 0;
end;

function TImagePreview.GetEntryThumbnail(index: integer; stretchWidth, stretchHeight: integer): TBGRABitmap;
var
  entry: TBGRABitmap;
begin
  if Assigned(FIconCursor) then
  begin
    entry := FIconCursor.GetBitmap(index) as TBGRABitmap;
    try
      result := GetBitmapThumbnail(entry, FIconCursor.FileType, stretchWidth, stretchHeight, BGRAPixelTransparent,true);
    finally
      entry.free;
    end;
  end else
  if Assigned(FTiff) then
  begin
    result := GetBitmapThumbnail(FTiff.Images[index].Img as TBGRABitmap, stretchWidth,stretchHeight, BGRAPixelTransparent,true);
  end else
    result := nil;
end;

function TImagePreview.CanDeleteEntry(index: integer): boolean;
begin
  result := Assigned(FIconCursor) and
            (index >= 0) and (index < FIconCursor.Count) and
            (FIconCursor.Count > 1);
end;

function TImagePreview.GetCurrentFrameBitmap: TBGRABitmap;
begin
  if Assigned(FSingleImage) then
    result := FSingleImage
  else if Assigned(FAnimatedGif) then
    result := FAnimatedGif.MemBitmap
  else if Assigned(FTiff) then
    result := FTiff.GetBiggestImage.Img as TBGRABitmap
  else
    result := nil;
end;

procedure TImagePreview.DeleteEntry(i: integer);
var outputStream: TStream;
begin
  if assigned(FIconCursor) and (i < FIconCursor.Count) and (i >= 0) then
  begin
    if FIconCursor.Count = 1 then
    begin
      ShowMessage(rsFileCannotBeEmpty);
    end else
    if QuestionDlg (rsDeleteFile,rsDeleteIconImage,mtConfirmation,[mrOk,rsOkay,mrCancel,rsCancel],'') = mrOk then
    begin
      try
        FIconCursor.Delete(i);
        outputStream := FileManager.CreateFileStream(Filename, fmCreate);
        try
          FIconCursor.SaveToStream(outputStream);
          outputStream.Free;
        except on ex: Exception do
          begin
            FileManager.CancelStreamAndFree(outputStream);
            ShowMessage(ex.Message);
          end;
        end;
        if FSelectedMenuIndex >= FIconCursor.Count then
          FSelectedMenuIndex := FIconCursor.Count-1;
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
    if scrollPos < FImageMenu[FSelectedMenuIndex].Area.Bottom-FSurface.Height then
      scrollPos := FImageMenu[FSelectedMenuIndex].Area.Bottom-FSurface.Height;
    if scrollPos > FImageMenu[FSelectedMenuIndex].Area.Top then
      scrollPos := FImageMenu[FSelectedMenuIndex].Area.Top;
    if Assigned(FScrollbar) then FScrollbar.Position := scrollPos;
    FSurface.DiscardBitmap;
  end;
end;

constructor TImagePreview.Create(ASurface: TBGRAVirtualScreen; AStatus: TLabel);
begin
  FSurface := ASurface;
  FStatus := AStatus;
  FSelectedMenuIndex := -1;
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
  FOnValidate := nil;
  FScrollbar := nil;
  FScrolling:= false;
end;

destructor TImagePreview.Destroy;
begin
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
  FreeAndNil(FScrollbar);
  inherited Destroy;
end;

procedure TImagePreview.UpdatePreview;
var reader: TFPCustomImageReader;
  jpegReader: TBGRAReaderJpeg;
  source: TStream;
begin
  FreeAndNil(FSingleImage);
  FreeAndNil(FTiff);
  FreeAndNil(FAnimatedGif);
  FreeAndNil(FIconCursor);
  FImageNbLayers := 0;
  FImageFormat:= ifUnknown;
  FLoadError := '';
  FImageMenu := nil;
  FreeAndNil(FScrollbar);
  FSelectedMenuIndex := -1;
  FSurface.RedrawBitmap;
  FStatus.Caption := rsLoading+'...';
  FStatus.Update;
  {$IFDEF LINUX}
  Application.ProcessMessages;
  {$ENDIF}
  source := nil;
  try
    source := FileManager.CreateFileStream(FFilename, fmOpenRead or fmShareDenyWrite);
    FImageFormat := DetectFileFormat(source,ExtractFileExt(FFilename));
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
          FTiff := TBGRAReaderTiff.Create;
          FTiff.LoadFromStream(source);
          FImageNbLayers := 1;
          if FTiff.ImageCount = 0 then
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
    else
      begin
        reader := CreateBGRAImageReader(FImageFormat);
        try
          FSingleImage := TBGRABitmap.Create;
          FSingleImage.LoadFromStream(source,reader);
          if reader is TFPReaderOpenRaster then FImageNbLayers := TFPReaderOpenRaster(reader).NbLayers else
          if reader is TFPReaderPaintDotNet then FImageNbLayers := TFPReaderPaintDotNet(reader).NbLayers else
          if reader is TBGRAReaderLazPaint then FImageNbLayers := TBGRAReaderLazPaint(reader).NbLayers else
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
      FStatus.Caption := rsCanvasSize + ': ' + IntToStr(ImageWidth)+'x'+IntToStr(ImageHeight)+', '+
                         rsEntries+': '+IntToStr(FTiff.ImageCount);
  end else
  if Assigned(FSingleImage) then
  begin
    FStatus.Caption := rsCanvasSize + ': ' + IntToStr(FSingleImage.Width)+'x'+IntToStr(FSingleImage.Height)+', '+
                       rsLayers+': '+IntToStr(FImageNbLayers);
  end else
  if FLoadError <> '' then
  begin
    FStatus.Caption := FLoadError;
  end else
    FStatus.Caption := '';
  FSurface.RedrawBitmap;
end;

procedure TImagePreview.HandleTimer;
begin
  if assigned(FAnimatedGif) and (FAnimatedGif.TimeUntilNextImageMs <= 0) then
    FSurface.RedrawBitmap;
end;

function TImagePreview.GetPreviewBitmap: TImageEntry;
var tx,ty,bpp, frameIndex: integer; back: TBGRAPixel;
begin
  result := TImageEntry.Empty;

  if Assigned(FIconCursor) then
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
          end;
        end;
      end else
      begin
        frameIndex := FImageMenu[FSelectedMenuIndex].FrameIndex;
        result.bmp := FIconCursor.GetBitmap(frameIndex) as TBGRABitmap;
        result.bpp := FIconCursor.BitDepth[frameIndex];
      end;
    end;
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
            result.bmp := TBGRABitmap.Create(tx,ty,back);
        end else
        begin
          frameIndex := FImageMenu[FSelectedMenuIndex].FrameIndex;
          result.bmp := FTiff.Images[frameIndex].Img as TBGRABitmap;
          FTiff.Images[frameIndex].Img := nil;
        end;
      end;
    end else
    if FTiff.ImageCount > 0 then
    begin
      frameIndex := 0;
      result.bmp := FTiff.Images[frameIndex].Img as TBGRABitmap;
      FTiff.Images[frameIndex].Img := nil;
    end;
  end else
  if Assigned(FSingleImage) then
  begin
    result.bmp := FSingleImage;
    FSingleImage := nil;
  end else
  if Assigned(FAnimatedGif) and (FAnimatedGif.Count = 1) then
  begin
    result.bmp := FAnimatedGif.MemBitmap.Duplicate as TBGRABitmap;
  end;
end;

end.

