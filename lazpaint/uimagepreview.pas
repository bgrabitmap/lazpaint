unit UImagePreview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Graphics, Controls, BGRAVirtualScreen,
  LazPaintType, UGraph, UResourceStrings, UFileSystem, Forms, UVolatileScrollBar,
  BGRABitmap, BGRAAnimatedGif, BGRAIconCursor, BGRABitmapTypes, BGRAThumbnail;

type

  { TImagePreview }

  TImagePreview = class
  protected
    FSelectedMenuIndex: integer;
    FSurface: TBGRAVirtualScreen;
    FStatus: TLabel;
    FFilename: string;
    FCurrentImage: TBGRABitmap;
    FCurrentAnimatedGif: TBGRAAnimatedGif;
    FCurrentIconCursor: TBGRAIconCursor;
    FCurrentImageFormat: TBGRAImageFormat;
    FCurrentImageNbLayers: integer;
    FCurrentImageError: string;
    FCurrentImageMenu: array of record
                           Area, IconArea: TRect;
                           DeleteArea: TRect;
                           FrameIndex: integer;
                           IsNew: boolean;
                         end;
    FScrollbar: TVolatileScrollBar;
    FScrolling: boolean;
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
    procedure DrawCurrentFrame(Bitmap: TBGRABitmap);
    procedure DrawIcons(Bitmap: TBGRABitmap);
    function TryIconLayout(AWidth: integer): integer;
    procedure DeleteFrame(i: integer);
    procedure ScrollToSelectedMenu;
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
  result := (FCurrentImageFormat in[ifJpeg, ifLazPaint, ifOpenRaster])
  or (Assigned(FCurrentAnimatedGif) and (FCurrentAnimatedGif.Count > 1));
end;

procedure TImagePreview.SetFilename(AValue: string);
begin
  if FFilename=AValue then Exit;
  FFilename:=AValue;
  UpdatePreview;
end;

procedure TImagePreview.SurfaceRedraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  if (Bitmap.Width = 0) or (Bitmap.Height = 0) then exit;
  if Assigned(FCurrentImage) or Assigned(FCurrentAnimatedGif) then
    DrawCurrentFrame(Bitmap)
  else
  if Assigned(FCurrentIconCursor) then
    DrawIcons(Bitmap);
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

    for i := 0 to high(FCurrentImageMenu) do
      if PtInRect(Point(x,y+scrollPos), FCurrentImageMenu[i].DeleteArea) then
      begin
        DeleteFrame(FCurrentImageMenu[i].FrameIndex);
        exit;
      end;

    for i := 0 to high(FCurrentImageMenu) do
      if PtInRect(Point(x,y+scrollPos), FCurrentImageMenu[i].Area) then
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

    for i := 0 to high(FCurrentImageMenu) do
      if PtInRect(Point(x,y+scrollPos), FCurrentImageMenu[i].DeleteArea) then
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
    if FSelectedMenuIndex < High(FCurrentImageMenu) then
    begin
      inc(FSelectedMenuIndex);
      ScrollToSelectedMenu;
    end;
  end else
  if Key = VK_HOME then
  begin
    Key := 0;
    if (FSelectedMenuIndex <> 0) and (length(FCurrentImageMenu) > 0) then
    begin
      FSelectedMenuIndex:= 0;
      ScrollToSelectedMenu;
    end;
  end else
  if Key = VK_END then
  begin
    Key := 0;
    if (FSelectedMenuIndex <> high(FCurrentImageMenu)) and (length(FCurrentImageMenu) > 0) then
    begin
      FSelectedMenuIndex:= high(FCurrentImageMenu);
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
    if (FSelectedMenuIndex >= 0) and (FSelectedMenuIndex <= high(FCurrentImageMenu)) and
      not FCurrentImageMenu[FSelectedMenuIndex].IsNew then
    begin
      DeleteFrame(FCurrentImageMenu[FSelectedMenuIndex].FrameIndex);
    end;
  end;
end;

procedure TImagePreview.DrawCurrentFrame(Bitmap: TBGRABitmap);
var x,y,w,h,ofs: integer;
  temp: TBGRABitmap;
begin
  if Assigned(FCurrentImage) then
  begin
    w := FCurrentImage.Width;
    h := FCurrentImage.Height;
  end else
  if Assigned(FCurrentAnimatedGif) then
  begin
    w := FCurrentAnimatedGif.Width;
    h := FCurrentAnimatedGif.Height;
  end else
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
  if (FCurrentImageFormat in [ifCur,ifIco]) and Assigned(FCurrentImage) then
  begin
    temp := GetBitmapThumbnail(FCurrentImage, FCurrentImageFormat, w,h, BGRAPixelTransparent, True);
    bitmap.PutImage(x,y, temp, dmSet);
    temp.Free;
  end else
  begin
    DrawThumbnailCheckers(Bitmap, rect(x,y,x+w,y+h));
    if Assigned(FCurrentImage) then
      bitmap.StretchPutImage(rect(x,y,x+w,y+h), FCurrentImage, dmDrawWithTransparency)
    else
    if Assigned(FCurrentAnimatedGif) then
      bitmap.StretchPutImage(rect(x,y,x+w,y+h), FCurrentAnimatedGif.MemBitmap, dmDrawWithTransparency)
  end;
end;

procedure TImagePreview.DrawIcons(Bitmap: TBGRABitmap);
var scrollPos, totalHeight, maxScroll, availableWidth: integer;
  i,j: integer;
  temp,entry: TBGRABitmap;
  x,y,sw,sh: integer;
  textRight: integer;
  iconCaption: string;
  ptsF,ptsF2: ArrayOfTPointF;
begin
  if (Bitmap.Width < 8) or (Bitmap.Height < 8) or not Assigned(FCurrentIconCursor) then exit;

  if Assigned(FScrollbar) then
  begin
    scrollPos := FScrollbar.Position;
  end else
    scrollPos := 0;

  if not FScrolling then
  begin
    FreeAndNil(FScrollbar);
    availableWidth := Bitmap.Width;
    totalHeight := TryIconLayout(availableWidth);
    maxScroll := totalHeight-Bitmap.Height;
    if maxScroll < 0 then maxScroll := 0;
    if scrollPos > maxScroll then scrollPos := maxScroll;

    if (totalHeight > Bitmap.Height) and (Bitmap.Width > 8+VolatileScrollBarSize) and
      (Bitmap.Height > VolatileThumbSize) then
    begin
      availableWidth -= VolatileScrollBarSize;
      FScrollbar := TVolatileScrollBar.Create(availableWidth,0,VolatileScrollBarSize,Bitmap.Height,sbVertical,
                                              scrollPos,0,maxScroll);
      totalHeight := TryIconLayout(availableWidth);
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

  if FSelectedMenuIndex >= length(FCurrentImageMenu) then
    FSelectedMenuIndex:= -1;
  if (FSelectedMenuIndex = -1) and (length(FCurrentImageMenu) > 0) then
  begin
    if length(FCurrentImageMenu)>=2 then
      FSelectedMenuIndex:= 1 //if there is an existing frame
    else
      FSelectedMenuIndex:= 0; //otherwise new frame
  end;

  for i := 0 to high(FCurrentImageMenu) do
  with FCurrentImageMenu[i] do
  begin
    try
      if IsNew then
        entry := nil
      else
        entry := FCurrentIconCursor.GetBitmap(FrameIndex) as TBGRABitmap;
    except
      entry := nil;
    end;

    DeleteArea := EmptyRect;
    textRight := availableWidth;

    if i = FSelectedMenuIndex then
    begin
      bitmap.FillRect(Area.Left, Area.Top-scrollPos, Area.Right, Area.Bottom-scrollPos, ColorToRGB(clHighlight));
      if not IsNew and (Area.Right - IconArea.Right > 32) then
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

    if Assigned(entry) then
    begin
      bitmap.FillRect(rect(x+2,y+2, x+sw+2,y+sh+2), BGRA(0,0,0,96), dmDrawWithTransparency);
      temp := GetBitmapThumbnail(entry, FCurrentIconCursor.FileType, sw, sh, BGRAWhite,true);
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
       iconCaption := inttostr(FCurrentIconCursor.Width[FrameIndex])+'x'+inttostr(FCurrentIconCursor.Height[FrameIndex])+
                       ' '+inttostr(FCurrentIconCursor.BitDepth[FrameIndex])+'bit';

    if (y+16 < bitmap.height) and (y+sh-16 > 0) then
      NiceText(bitmap, x+sw+4,y+sh div 2, textRight,bitmap.height,
                       iconCaption, taLeftJustify, tlCenter);

    entry.Free;
  end;

  if Assigned(FScrollbar) then FScrollbar.Draw(Bitmap);
end;

function TImagePreview.TryIconLayout(AWidth: integer): integer;
var x,y,i,frameIndex,h,w,sw,sh: integer;
begin
  x := 2;
  y := 2;
  FCurrentImageMenu := nil;
  setlength(FCurrentImageMenu, FCurrentIconCursor.Count+1);
  for i := 0 to FCurrentIconCursor.Count do
  begin

    if i = 0 then
    begin
      frameIndex := FCurrentIconCursor.Count;
      FCurrentImageMenu[i].IsNew := true;
      w := 32;
      h := 32;
    end
    else
    begin
      frameIndex := i-1;
      w := FCurrentIconCursor.Width[frameIndex];
      h := FCurrentIconCursor.Height[frameIndex];
    end;
    if w > AWidth-8 then
    begin
      sw := AWidth-8;
      sh := round(h/w*(AWidth-8));
    end else
    begin
      sw := w;
      sh := h;
    end;

    FCurrentImageMenu[i].FrameIndex:= frameIndex;
    FCurrentImageMenu[i].Area := rect(x,y,AWidth-2,y+sh+4);
    FCurrentImageMenu[i].IconArea := rect(x+2,y+2,x+2+sw,y+2+sh);

    y += sh+4;
  end;

  result := y+2;
end;

procedure TImagePreview.DeleteFrame(i: integer);
var outputStream: TStream;
begin
  if assigned(FCurrentIconCursor) and (i < FCurrentIconCursor.Count) and (i >= 0) then
  begin
    if FCurrentIconCursor.Count = 1 then
    begin
      ShowMessage(rsFileCannotBeEmpty);
    end else
    if QuestionDlg (rsDeleteFile,rsDeleteIconImage,mtConfirmation,[mrOk,rsOkay,mrCancel,rsCancel],'') = mrOk then
    begin
      try
        FCurrentIconCursor.Delete(i);
        outputStream := FileManager.CreateFileStream(Filename, fmCreate);
        try
          FCurrentIconCursor.SaveToStream(outputStream);
          outputStream.Free;
        except on ex: Exception do
          begin
            FileManager.CancelStreamAndFree(outputStream);
            ShowMessage(ex.Message);
          end;
        end;
        if FSelectedMenuIndex >= FCurrentIconCursor.Count then
          FSelectedMenuIndex := FCurrentIconCursor.Count-1;
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

  if (FSelectedMenuIndex >= 0) and (FSelectedMenuIndex <= high(FCurrentImageMenu)) then
  begin
    if scrollPos < FCurrentImageMenu[FSelectedMenuIndex].Area.Bottom-FSurface.Height then
      scrollPos := FCurrentImageMenu[FSelectedMenuIndex].Area.Bottom-FSurface.Height;
    if scrollPos > FCurrentImageMenu[FSelectedMenuIndex].Area.Top then
      scrollPos := FCurrentImageMenu[FSelectedMenuIndex].Area.Top;
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
  FCurrentImage := nil;
  FCurrentAnimatedGif := nil;
  FCurrentIconCursor := nil;
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
  FreeAndNil(FCurrentImage);
  FreeAndNil(FCurrentAnimatedGif);
  FreeAndNil(FCurrentIconCursor);
  FreeAndNil(FScrollbar);
  inherited Destroy;
end;

procedure TImagePreview.UpdatePreview;
var reader: TFPCustomImageReader;
  jpegReader: TBGRAReaderJpeg;
  source: TStream;
begin
  FreeAndNil(FCurrentImage);
  FreeAndNil(FCurrentAnimatedGif);
  FreeAndNil(FCurrentIconCursor);
  FCurrentImageNbLayers := 0;
  FCurrentImageFormat:= ifUnknown;
  FCurrentImageError := '';
  FCurrentImageMenu := nil;
  FreeAndNil(FScrollbar);
  FSelectedMenuIndex := -1;
  FSurface.RedrawBitmap;
  FStatus.Caption := rsLoading+'...';
  FStatus.Update;
  try
    source := FileManager.CreateFileStream(FFilename, fmOpenRead or fmShareDenyWrite);
    try
      FCurrentImageFormat := DetectFileFormat(source,ExtractFileExt(FFilename));
      if FCurrentImageFormat = ifGif then
      begin
        try
          FCurrentAnimatedGif := TBGRAAnimatedGif.Create(source);
        except
        end;
      end else
      if FCurrentImageFormat in [ifIco,ifCur] then
      begin
        FCurrentIconCursor := TBGRAIconCursor.Create;
        try
          FCurrentIconCursor.LoadFromStream(source);
        except
          on ex: Exception do
          begin
            FCurrentImageError:= ex.Message;
            FreeAndNil(FCurrentIconCursor);
          end;
        end;
      end;

      if (FCurrentAnimatedGif = nil) and (FCurrentIconCursor = nil) and (FCurrentImageError = '') then
      begin
        if FCurrentImageFormat = ifJpeg then
        begin
          jpegReader := TBGRAReaderJpeg.Create;
          jpegReader.Performance := jpBestSpeed;
          jpegReader.MinWidth := Screen.Width;
          jpegReader.MinHeight := Screen.Height;
          reader := jpegReader;
        end else
          reader := CreateBGRAImageReader(FCurrentImageFormat);
        try
          FCurrentImage := TBGRABitmap.Create;
          try
            FCurrentImage.LoadFromStream(source,reader);
            FCurrentImageNbLayers := 1;
            if reader is TFPReaderOpenRaster then FCurrentImageNbLayers := TFPReaderOpenRaster(reader).NbLayers else
            if reader is TFPReaderPaintDotNet then FCurrentImageNbLayers := TFPReaderPaintDotNet(reader).NbLayers else
            if reader is TBGRAReaderLazPaint then FCurrentImageNbLayers := TBGRAReaderLazPaint(reader).NbLayers;
          except
            on ex: Exception do
            begin
              FCurrentImageError:= ex.Message;
              FreeAndNil(FCurrentImage);
            end;
          end;
        finally
          reader.Free;
        end;
      end;
    finally
      source.Free;
    end;
  except
  end;
  if Assigned(FCurrentIconCursor) then
  begin
    if FCurrentIconCursor.Count > 0 then
      FStatus.Caption := rsCanvasSize + ': ' + IntToStr(FCurrentIconCursor.Width[0])+'x'+IntToStr(FCurrentIconCursor.Height[0])
                            + ', ' + rsEntries + ': ' + IntToStr(FCurrentIconCursor.Count)
    else
      FStatus.Caption := rsEntries + ': ' + IntToStr(FCurrentIconCursor.Count);
  end else
  if Assigned(FCurrentAnimatedGif) then
  begin
    FStatus.Caption := rsCanvasSize + ': ' + IntToStr(FCurrentAnimatedGif.Width)+'x'+IntToStr(FCurrentAnimatedGif.Height)+', '+
                       rsFrames+': '+IntToStr(FCurrentAnimatedGif.Count);
  end else
  if Assigned(FCurrentImage) then
  begin
    FStatus.Caption := rsCanvasSize + ': ' + IntToStr(FCurrentImage.Width)+'x'+IntToStr(FCurrentImage.Height)+', '+rsLayers+': '+IntToStr(FCurrentImageNbLayers);
  end else
  if FCurrentImageError <> '' then
  begin
    FStatus.Caption := FCurrentImageError;
  end else
    FStatus.Caption := '';
  FSurface.RedrawBitmap;
end;

procedure TImagePreview.HandleTimer;
begin
  if assigned(FCurrentAnimatedGif) and (FCurrentAnimatedGif.TimeUntilNextImageMs <= 0) then
    FSurface.RedrawBitmap;
end;

function TImagePreview.GetPreviewBitmap: TImageEntry;
var tx,ty,bpp, frameIndex: integer; back: TBGRAPixel;
begin
  result := TImageEntry.Empty;

  if Assigned(FCurrentIconCursor) then
  begin
    if (FSelectedMenuIndex >= 0) and (FSelectedMenuIndex < length(FCurrentImageMenu)) then
    begin
      if FCurrentImageMenu[FSelectedMenuIndex].IsNew then
      begin
        if Assigned(LazPaintInstance) and ShowNewImageDlg(LazPaintInstance, true, tx,ty,bpp, back) then
        begin
          if FCurrentIconCursor.IndexOf(tx,ty,bpp)<>-1 then
            LazPaintInstance.ShowMessage(rsNewImage, rsIconImageAlreadyExists)
          else
          begin
            result.bmp := TBGRABitmap.Create(tx,ty,back);
            result.bpp := bpp;
          end;
        end;
      end else
      begin
        frameIndex := FCurrentImageMenu[FSelectedMenuIndex].FrameIndex;
        result.bmp := FCurrentIconCursor.GetBitmap(frameIndex) as TBGRABitmap;
        result.bpp := FCurrentIconCursor.BitDepth[frameIndex];
      end;
    end;
  end else
  if Assigned(FCurrentImage) then
  begin
    result.bmp := FCurrentImage;
  end else
  if Assigned(FCurrentAnimatedGif) and (FCurrentAnimatedGif.Count = 1) then
  begin
    result.bmp := FCurrentAnimatedGif.MemBitmap.Duplicate as TBGRABitmap;
  end;
end;

end.

