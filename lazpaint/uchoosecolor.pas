unit UChooseColor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, BGRABitmap, BGRABitmapTypes, StdCtrls, Buttons,
  BGRAVirtualScreen, BCButton, LazPaintType, LCScaleDPI, uresourcestrings;

const
  ExternalMargin = 3;

type

  { TFChooseColor }

  TFChooseColor = class(TForm)
    BCButton_AddToPalette: TBCButton;
    BCButton_RemoveFromPalette: TBCButton;
    vsColorView: TBGRAVirtualScreen;
    EColor: TEdit;
    LColor: TLabel;
    procedure BCButton_AddToPaletteClick(Sender: TObject);
    procedure BCButton_RemoveFromPaletteClick(Sender: TObject);
    procedure EColorChange(Sender: TObject);
    procedure EColorKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LColorMouseDown(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure vsColorViewMouseDown(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure vsColorViewMouseMove(Sender: TObject; {%H-}Shift: TShiftState; X,
      Y: Integer);
    procedure vsColorViewMouseUp(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure vsColorViewRedraw(Sender: TObject; Bitmap: TBGRABitmap);
  private
    FDarkTheme: boolean;
    FTextAreaHeight: integer;
    FBarsAlign, FButtonsAlign: TAlign;
    FButtonsCenter: boolean;
    FTitleFontHeight: integer;
    FColorTitleVisible: boolean;
    FNeedUpdateLayout: Boolean;
    FWheelArea: TRect;
    FormBackgroundColor, FormTextColor: TBGRAPixel;
    ColorBeforeEColor: TBGRAPixel;

    InFormMouseMove: Boolean;
    FormMouseMovePos: TPoint;

    FCurrentColor: TBGRAPixel;
    FColorLight: word;
    ColorX,colorY: integer;
    selectZone: (szNone, szColorCircle, szLightScale, szAlphascale);
    FColorCircle: record center: TPointF;
                         bounds: TRect;
                         bmp,bmpMaxlight: TBGRABitmap; end;
    FLightscale: record bounds: TRect;
                        bmp: TBGRABitmap;
                        cursorRect: TRect; end;
    FAlphascale: record bounds: TRect;
                       bmp: TBGRABitmap;
                       cursorRect: TRect; end;
    function GetAvailableBmpHeight: integer;
    function GetAvailableBmpWidth: integer;
    procedure SetAvailableBmpHeight(AValue: integer);
    procedure SetDarkTheme(AValue: boolean);
    procedure SetLazPaintInstance(AValue: TLazPaintCustomInstance);
    procedure UpdateColorview(UpdateColorCircle, UpdateLightScale, Redraw: boolean);
    procedure SetColorLight(value: word);
    function ColorWithLight(c: TBGRAPixel; light: word): TBGRAPixel;
    function BitmapWithLight(bmpsrc: TBGRABitmap; light: word; lookFor: TBGRAPixel; var pColorX,pColorY: integer): TBGRABitmap;
    function ColorLightOf(c: TBGRAPixel): word;
    function DrawTriangleCursor(dest: TBGRABitmap; bounds: TRect; value: byte): TRect;
    procedure DoSelect(X,Y: integer);
    function MakeIconBase(size: integer): TBitmap;
    function MakeAddIcon(size: integer): TBitmap;
    function MakeRemoveIcon(size: integer): TBitmap;
  protected
    FInitialized: boolean;
    FTopMargin, FBarWidth, FButtonSize, FCursorPlace, FMargin, FInternalMargin,
    FCursorMargin, FCursorSize, FColorXYSize, FCursorXYOpacity: integer;
    FLazPaintInstance: TLazPaintCustomInstance;
    procedure ApplyTheme;
    procedure UpdateButtonLayout;
    function SetRectBounds(var ABounds: TRect; ANewBounds: TRect): boolean;
    function PreferredBarsAlignWithWidth: TAlign;
    procedure UpdateLayout;
  public
    { public declarations }
    ColorTarget: TColorTarget;
    procedure SetCurrentColor(value: TBGRAPixel);
    function GetCurrentColor: TBGRAPixel;
    property AvailableBmpHeight: integer read GetAvailableBmpHeight write SetAvailableBmpHeight;
    property AvailableBmpWidth: integer read GetAvailableBmpWidth;
    property DarkTheme: boolean read FDarkTheme write SetDarkTheme;
    property LazPaintInstance: TLazPaintCustomInstance read FLazPaintInstance write SetLazPaintInstance;
  end;

var TFChooseColor_CustomDPI: integer = 96;

implementation

uses ugraph, math, LCLType, BGRAText, udarktheme;

{ TFChooseColor }

procedure TFChooseColor.FormCreate(Sender: TObject);
begin
   BorderStyle := bsSizeToolWin;
   ApplyTheme;

   EColor.Font.Height := -FontEmHeightSign*DoScaleY(14,OriginalDPI,TFChooseColor_CustomDPI);
   LColor.Font.Height := -FontEmHeightSign*DoScaleY(14,OriginalDPI,TFChooseColor_CustomDPI);
   EColor.Text:= '#FFFFFF';
   EColor.AdjustSize;
   LColor.Visible := true;
   LColor.Caption := '#FFFFFF';
   LColor.AdjustSize;
   FTextAreaHeight := max(EColor.Height, LColor.Height);
   vsColorView.Left := ExternalMargin;
   vsColorView.Top := 0;
   FormResize(Sender);
   EColor.Visible := false;

   FColorXYSize := DoScaleX(3, OriginalDPI, TFChooseColor_CustomDPI);
   FCursorXYOpacity := DoScaleX(128, OriginalDPI, TFChooseColor_CustomDPI);
   if FCursorXYOpacity > 255 then FCursorXYOpacity := 255;

   selectZone := szNone;
   InFormMouseMove := False;

   FAlphascale.bmp := nil;
   FLightscale.bmp := nil;
   FColorCircle.bmp := nil;
   FColorCircle.bmpMaxlight := nil;

   SetCurrentColor(BGRAWhite);
   FInitialized:= true;
end;

procedure TFChooseColor.FormDeactivate(Sender: TObject);
begin
  if EColor.Visible then
    begin
      EColor.Hide;
      LColor.Show;
    end;
end;

procedure TFChooseColor.EColorChange(Sender: TObject);
var newColor: TBGRAPixel;
  error: boolean;
  errPos,value: integer;
begin
   if (FLazPaintInstance <> nil) and EColor.Visible then
   begin
     if FLazPaintInstance.BlackAndWhite then
     begin
       val(EColor.Text,value,errPos);
       if (errPos = 0) and (value >= 0) and (value <= 255) then
       begin
         error := false;
         newColor.green := value;
         newColor.red := value;
         newColor.blue := value;
       end else
         error := true;
     end else
       newColor := PartialStrToBGRA(EColor.Text,ColorBeforeEColor,error);
     if not error then
     begin
       newColor.alpha := ColorBeforeEColor.alpha;
       SetCurrentColor(newColor);
       FLazPaintInstance.ColorFromFChooseColor;
     end;
   end;
end;

procedure TFChooseColor.BCButton_AddToPaletteClick(Sender: TObject);
begin
  FLazPaintInstance.AddColorToPalette(GetCurrentColor);
end;

procedure TFChooseColor.BCButton_RemoveFromPaletteClick(Sender: TObject);
begin
  FLazPaintInstance.RemoveColorFromPalette(GetCurrentColor);
end;

procedure TFChooseColor.EColorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    EColor.Hide;
    LColor.Show;
  end
  else
  if Key = VK_ESCAPE then
  begin
    EColor.Hide;
    LColor.Show;
    SetCurrentColor(ColorBeforeEColor);
    FLazPaintInstance.ColorFromFChooseColor;
  end;
end;

procedure TFChooseColor.FormDestroy(Sender: TObject);
begin
  FColorCircle.bmp.Free;
  FColorCircle.bmpMaxlight.Free;
  FLightscale.Bmp.Free;
  FAlphascale.Bmp.Free;
end;

procedure TFChooseColor.FormResize(Sender: TObject);
begin
  vsColorView.Width := AvailableBmpWidth;
  vsColorView.Height := AvailableBmpHeight;
  EColor.Left := ExternalMargin;
  EColor.Width := ClientWidth-2*ExternalMargin;
  LColor.Left := ExternalMargin;
  LColor.Width := ClientWidth-2*ExternalMargin;
  EColor.Top := ClientHeight-FTextAreaHeight + (FTextAreaHeight-EColor.Height) div 2;
  LColor.Top := ClientHeight-FTextAreaHeight + (FTextAreaHeight-LColor.Height) div 2;
  FNeedUpdateLayout:= true;
end;

procedure TFChooseColor.FormShow(Sender: TObject);
begin
  Position := poDesigned;
  FNeedUpdateLayout := true;
  LColor.Top := ClientHeight-LColor.Height;
  self.EnsureVisible(False);
end;

procedure TFChooseColor.LColorMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  LColor.Visible := false;
  ColorBeforeEColor := GetCurrentColor;
  if FLazPaintInstance.BlackAndWhite then
  begin
    EColor.Text := inttostr(ColorBeforeEColor.green);
  end else
    EColor.Text := '#' + copy(BGRAToStr(ColorBeforeEColor),1,6);
  EColor.Visible := true;
  EColor.Top := ClientHeight-EColor.Height;
  SafeSetFocus(EColor);
end;

procedure TFChooseColor.vsColorViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    if EColor.Visible then
    begin
      EColor.Hide;
      LColor.Show;
    end;
    if PtInRect(Point(X,Y), FColorCircle.Bounds) then
    begin
      SelectZone := szColorCircle;
      DoSelect(X,Y);
    end else
    if PtInRect(Point(X,Y), FAlphascale.Bounds) or PtInRect(Point(X,Y), FAlphascale.cursorRect) then
    begin
      SelectZone := szAlphascale;
      DoSelect(X,Y);
    end else
    if PtInRect(Point(X,Y), FLightscale.Bounds) or PtInRect(Point(X,Y), FLightscale.cursorRect) then
    begin
      SelectZone := szLightscale;
      DoSelect(X,Y);
    end;
  end;
end;

procedure TFChooseColor.vsColorViewMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  FormMouseMovePos := Point(X,Y);
  if InFormMouseMove then Exit;
  InFormMouseMove := True;
  Application.ProcessMessages; //empty message stack
  DoSelect(FormMouseMovePos.X,FormMouseMovePos.Y);
  InFormMouseMove := False;
end;

procedure TFChooseColor.vsColorViewMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then selectZone := szNone;
end;

procedure TFChooseColor.vsColorViewRedraw(Sender: TObject; Bitmap: TBGRABitmap);
var x,y: integer;
  boundRight,w: integer;
  size: single;
  c: TBGRAPixel;
begin
  if FNeedUpdateLayout then
  begin
    UpdateLayout;
    FNeedUpdateLayout := false;
  end;

  Bitmap.FontHeight := FTitleFontHeight;

  Bitmap.Fill(FormBackgroundColor);
  Bitmap.FontAntialias := True;
  boundRight := Bitmap.Width;
  if FAlphascale.Bmp<>nil then
  begin
    w := Bitmap.TextSize(rsOpacity).cx;
    if FBarsAlign = alRight then
    begin
      x := (FAlphascale.bounds.Left+FAlphascale.bounds.Right-w) div 2;
      if x + w > boundRight then
        x := boundRight-w;
      Bitmap.TextOut(x,FMargin div 2,rsOpacity,FormTextColor,taLeftJustify);
      boundRight := x - FMargin div 2;
    end;
    Bitmap.PutImage(FAlphascale.bounds.Left,FAlphascale.bounds.top,FAlphascale.Bmp,dmDrawWithTransparency);
    Bitmap.Rectangle(FAlphascale.bounds,BGRA(FormTextColor.red,FormTextColor.green,FormTextColor.Blue,128),dmDrawWithTransparency);
    FAlphascale.cursorRect := DrawTriangleCursor(Bitmap, FAlphascale.bounds, FCurrentColor.alpha);
  end else FAlphascale.cursorRect := EmptyRect;

  if FLightscale.Bmp<>nil then
  begin
    w := Bitmap.TextSize(rsLight).cx;
    if FBarsAlign = alRight then
    begin
      x := (FLightscale.bounds.Left+FLightscale.bounds.Right-w) div 2;
      if x+ w > boundRight then
        x:= boundRight-w;
      Bitmap.TextOut(x,FMargin div 2,rsLight,FormTextColor,taLeftJustify);
      boundRight := x - FMargin div 2;
    end;
    Bitmap.PutImage(FLightscale.bounds.Left,FLightscale.bounds.top,FLightscale.Bmp,dmFastBlend);
    Bitmap.Rectangle(FLightscale.bounds,BGRA(FormTextColor.red,FormTextColor.green,FormTextColor.Blue,128),dmDrawWithTransparency);
    FLightscale.cursorRect := DrawTriangleCursor(Bitmap, FLightscale.bounds, FColorLight div 256);
  end else FLightscale.cursorRect := EmptyRect;

  if (FColorCircle.Bmp<>nil) and not FLazPaintInstance.BlackAndWhite then
  begin
    if FColorTitleVisible then
    begin
      w := Bitmap.TextSize(rsColors).cx;
      x := round(FColorCircle.center.X - w/2);
      x := min(x, boundRight-w);
      x := max(x, FButtonSize + FMargin + FMargin div 2);
      if x <= boundRight-w then
        Bitmap.TextOut(x,FMargin div 2,rsColors,FormTextColor,taLeftJustify);
    end;
    Bitmap.PutImage(FColorCircle.bounds.Left,FColorCircle.bounds.top,FColorCircle.Bmp,dmDrawWithTransparency);
    x := FColorCircle.bounds.Left+ColorX;
    y := FColorCircle.bounds.top+ColorY;
    Bitmap.Rectangle(x-FColorXYSize-1,y-FColorXYSize-1,x+FColorXYSize+2,y+FColorXYSize+2,BGRA(0,0,0,FCursorXYOpacity),dmDrawWithTransparency);
    Bitmap.Rectangle(x-FColorXYSize,y-FColorXYSize,x+FColorXYSize+1,y+FColorXYSize+1,BGRA(255,255,255,FCursorXYOpacity),dmDrawWithTransparency);
    Bitmap.Rectangle(x-FColorXYSize+1,y-FColorXYSize+1,x+FColorXYSize,y+FColorXYSize,BGRA(0,0,0,FCursorXYOpacity),dmDrawWithTransparency);
    size := round(FBarWidth*0.9);
    c := GetCurrentColor;
    c.alpha := 255;
    x := FMargin - ExternalMargin;
    if FBarsAlign = alBottom then y := round(FLightscale.bounds.Top-FMargin-1-size)
    else y := round(Bitmap.Height - FMargin - size);
    Bitmap.RoundRectAntialias(x, y, x + size, y + size,
        size/6,size/6, BGRA(0,0,0,192),1,c, []);
  end;
end;

function TFChooseColor.DrawTriangleCursor(dest: TBGRABitmap; bounds: TRect;
  value: byte): TRect;
var x,y: integer;
begin
  if FBarsAlign = alRight then
  begin
    x := bounds.right + FCursorMargin;
    y := bounds.bottom-1 - round(value/255*(bounds.height-1));
    dest.FillPolyAntialias([pointF(x, y), pointF(x+FCursorSize, y-FCursorSize),
                            pointF(x+FCursorSize, y+FCursorSize)], FormTextColor);
    result := rect(floor(x-FCursorSize/2), floor(y-FCursorSize*1.5),
                   ceil(x+FCursorSize*1.5), ceil(y+FCursorSize*1.5));
  end else
  begin
    x := bounds.left + round(value/255*(bounds.Width-1));
    y := bounds.bottom + FCursorMargin;
    dest.FillPolyAntialias([pointF(x, y), pointF(x+FCursorSize,y+FCursorSize),
                            pointF(x-FCursorSize, y+FCursorSize)], FormTextColor);
    result := rect(floor(x-FCursorSize*1.5), floor(y-FCursorSize/2),
                   ceil(x+FCursorSize*1.5), ceil(y+FCursorSize*1.5));
  end;
end;

procedure TFChooseColor.DoSelect(X, Y: integer);
var pix: TBGRAPixel;
  newLight: Word;
begin
  case selectZone of
  szAlphascale:
    begin
      if FBarsAlign = alRight then
        FCurrentColor.alpha := max(0, min(255, 255-round((Y-FAlphascale.Bounds.Top)/(FAlphascale.Bounds.Height-1)*255)))
        else FCurrentColor.alpha := max(0, min(255, round((X-FAlphascale.Bounds.Left)/(FAlphascale.Bounds.Width-1)*255)));
      UpdateColorview(False,False,True);
    end;
  szColorCircle:
    if PtInRect(point(x,y),FColorCircle.Bounds) then
    begin
      pix := FColorCircle.bmpMaxlight.GetPixel(x-FColorCircle.Bounds.Left,y-FColorCircle.Bounds.top);
      if pix.alpha <> 0 then
      begin
        FCurrentColor := BGRA(pix.Red,pix.Green,pix.Blue,FCurrentColor.Alpha);
        ColorX := x-FColorCircle.Bounds.Left;
        ColorY := y-FColorCircle.Bounds.top;
        UpdateColorview(False,True,True);
      end;
    end;
  szLightScale:
    begin
      if FBarsAlign = alRight then
        newLight := max(0, min(65535, 65535 - round((Y-FLightscale.Bounds.Top)/(FLightscale.Bounds.Height-1)*65535)))
        else newLight := max(0, min(65535, round((X-FLightscale.Bounds.Left)/(FLightscale.Bounds.Width-1)*65535)));
      SetColorLight(newLight);
    end;
  else exit;
  end;
  FLazPaintInstance.ColorFromFChooseColor;
end;

function TFChooseColor.MakeIconBase(size: integer): TBitmap;
var sq,offs: integer;
begin
  result := TBitmap.Create;
  result.Width := size;
  result.Height := size;

  with result.Canvas do
  begin
    Brush.Color := clFuchsia;
    FillRect(0,0,size,size);

    sq := size div 3;
    offs := size div 6;

    Pen.Color := clBlack;
    Brush.Color := BGRAToColor(CSSLawnGreen);
    Rectangle(offs+0,offs+0,offs+sq+1,offs+sq+1);
    Brush.Color := clYellow;
    Rectangle(offs+sq,offs+0,offs+sq+sq+1,offs+sq+1);
    Brush.Color := BGRAToColor(CSSDodgerBlue);
    Rectangle(offs+0,offs+sq,offs+sq+1,offs+sq+sq+1);
    Brush.Color := BGRAToColor(CSSBlue);
    Rectangle(offs+sq,offs+sq,offs+sq+sq+1,offs+sq+sq+1);
  end;

  result.TransparentMode := tmFixed;
  result.TransparentColor := clFuchsia;
  result.Transparent := true;
end;

function TFChooseColor.MakeAddIcon(size: integer): TBitmap;
begin
  result := MakeIconBase(size);

  with result.Canvas do
  begin
    Pen.Color := clBlack;
    Brush.Color := clWhite;
    Polygon([Point(size div 2-1,size div 2-1),Point(size div 2-1,size),
                        Point(size*5 div 8,size*5 div 6),
                        Point(size*5 div 6,size*5 div 6)]);
  end;
end;

function TFChooseColor.MakeRemoveIcon(size: integer): TBitmap;
var w: integer;
begin
  result := MakeIconBase(size);

  with result.Canvas do
  begin
    w := (size+2) div 6;
    Pen.Color := BGRAToColor(BGRA(128,0,0));
    Pen.Width := w;
    MoveTo(size div 2-1,size div 2-1);
    LineTo(size-w,size-w);
    MoveTo(size div 2-1,size-w);
    LineTo(size-w,size div 2-1);

    Pen.Color := BGRAToColor(BGRA(255,80,80));
    Pen.Width := w-2;
    MoveTo(size div 2-1,size div 2-1);
    LineTo(size-w,size-w);
    MoveTo(size div 2-1,size-w);
    LineTo(size-w,size div 2-1);
  end;
end;

procedure TFChooseColor.ApplyTheme;
begin
  DarkThemeInstance.Apply(self, FDarkTheme);
  DarkThemeInstance.Apply(BCButton_AddToPalette, FDarkTheme);
  DarkThemeInstance.Apply(BCButton_RemoveFromPalette, FDarkTheme);
  if DarkTheme then
  begin
    FormBackgroundColor := clDarkBtnFace;
    FormTextColor := clLightText;
  end else
  begin
    FormBackgroundColor := ColorToBGRA(ColorToRGB({$IFDEF DARWIN}clWindow{$ELSE}clBtnFace{$ENDIF}));
    FormTextColor := ColorToBGRA(ColorToRGB(clWindowText));
  end;
  vsColorView.DiscardBitmap;
end;

procedure TFChooseColor.UpdateButtonLayout;
var
  iconSize: Integer;
  tmpIcon: TBitmap;
begin
  FButtonSize := FBarWidth;
  BCButton_AddToPalette.Width := FButtonSize;
  BCButton_AddToPalette.Height := FButtonSize;
  BCButton_RemoveFromPalette.Width := FButtonSize;
  BCButton_RemoveFromPalette.Height := FButtonSize;
  BCButton_AddToPalette.Left := FMargin - ExternalMargin;
  BCButton_AddToPalette.Top := FMargin div 2;
  if FButtonsAlign = alLeft then
  begin
    if FButtonsCenter then
      BCButton_AddToPalette.Top := (AvailableBmpHeight -
        BCButton_AddToPalette.Height - BCButton_RemoveFromPalette.Height) div 2;
    BCButton_RemoveFromPalette.Left := FMargin - ExternalMargin;
    BCButton_RemoveFromPalette.Top := BCButton_AddToPalette.Top+BCButton_AddToPalette.Height;
  end else
  begin
    if FButtonsCenter then
      BCButton_AddToPalette.Left := (AvailableBmpWidth -
        BCButton_AddToPalette.Width - BCButton_RemoveFromPalette.Width) div 2;
    BCButton_RemoveFromPalette.Left := BCButton_AddToPalette.Left+BCButton_AddToPalette.Width;
    BCButton_RemoveFromPalette.Top := FMargin div 2;
  end;
  iconSize := FButtonSize-4;
  if not Assigned(BCButton_AddToPalette.Glyph) or (BCButton_AddToPalette.Glyph.Width <> iconSize) then
  begin
    tmpIcon:= MakeAddIcon(iconSize);
    BCButton_AddToPalette.Glyph.Assign(tmpIcon);
    tmpIcon.Free;
  end;
  if not Assigned(BCButton_RemoveFromPalette.Glyph) or (BCButton_RemoveFromPalette.Glyph.Width <> iconSize) then
  begin
    tmpIcon:= MakeRemoveIcon(iconSize);
    BCButton_RemoveFromPalette.Glyph.Assign(tmpIcon);
    tmpIcon.Free;
  end;
end;

function TFChooseColor.SetRectBounds(var ABounds: TRect; ANewBounds: TRect): boolean;
begin
  result := (ABounds.Width <> ANewBounds.Width) or (ABounds.Height <> ANewBounds.Height);
  ABounds := ANewBounds;
end;

function TFChooseColor.PreferredBarsAlignWithWidth: TAlign;
var
  totalBarWidth, tx,ty: Integer;
begin
  totalBarWidth := (FBarWidth+FCursorPlace+FMargin)*2;
  if Assigned(LazPaintInstance) and not LazPaintInstance.BlackAndWhite then
  begin
    tx := AvailableBmpWidth - 2*FInternalMargin;
    ty := AvailableBmpHeight - FMargin - FTopMargin;
    if tx >= ty + totalBarWidth then
      result := alRight
      else result := alBottom;
  end else
  begin
    tx := AvailableBmpWidth - 2*FInternalMargin;
    if tx <= totalBarWidth*4 then
      result := alRight
    else
      result := alBottom;
  end;
end;

procedure TFChooseColor.UpdateLayout;
var bmpWidth, bmpHeight: integer;
    prevBarsAlign: TAlign;
    newAlphaBounds, newLightscaleBounds: TRect;
    needUpdateLightscale, needUpdateColorCircle: Boolean;
    diffXY, delta: integer;
    reductionFactor: single;

  function AdaptSizeX(ASize: integer): integer;
  begin
    result := min(DoScaleX(ASize, OriginalDPI, TFChooseColor_CustomDPI), round(ASize*reductionFactor));
  end;
  function AdaptSizeY(ASize: integer): integer;
  begin
    result := min(DoScaleY(ASize, OriginalDPI, TFChooseColor_CustomDPI), round(ASize*reductionFactor));
  end;

begin
  if FLazPaintInstance = nil then exit;

  prevBarsAlign := FBarsAlign;
  bmpWidth := AvailableBmpWidth;
  bmpHeight := AvailableBmpHeight;

  if LazPaintInstance.BlackAndWhite then
    reductionFactor := min(bmpWidth, bmpHeight)/105
  else
    reductionFactor := max(bmpWidth, bmpHeight)/200;
  FBarWidth := AdaptSizeX(18);
  FCursorPlace := AdaptSizeX(10);
  FCursorMargin := AdaptSizeX(2);
  FCursorSize := AdaptSizeX(6);
  FTopMargin := AdaptSizeY(27);
  FMargin := AdaptSizeY(8);
  FInternalMargin := FMargin-ExternalMargin;
  if FInternalMargin < 0 then FInternalMargin := 0;
  FTitleFontHeight := max(AdaptSizeY(12), 10);

  FWheelArea := Rect(FInternalMargin, FMargin, bmpWidth - FInternalMargin, bmpHeight - FMargin);

  if FLazPaintInstance.BlackAndWhite then
  begin
    FButtonsCenter := true;
    if FWheelArea.Width <= FWheelArea.Height then
    begin
      FButtonsAlign := alLeft;
      FBarsAlign := alRight;
      UpdateButtonLayout;
    end else
    begin
      FButtonsAlign := alTop;
      FBarsAlign := alBottom;
      UpdateButtonLayout;
    end;
  end
  else
  begin
    FButtonsAlign := alLeft;
    FButtonsCenter := false;
    if FWheelArea.Width >= FWheelArea.Height then
      FBarsAlign := alRight else FBarsAlign := alBottom;
    UpdateButtonLayout;
  end;

  FWheelArea.Top := FTopMargin;

  if FBarsAlign = alRight then
  begin
    newAlphaBounds := RectWithSize(FWheelArea.Right - FCursorPlace - FBarWidth, FWheelArea.Top,
                                   FBarWidth, FWheelArea.Height);
    FWheelArea.Right := newAlphaBounds.Left - FMargin;
  end
  else
  begin
    newAlphaBounds := RectWithSize(FWheelArea.Left, FWheelArea.Bottom - FCursorPlace - FBarWidth,
                                   FWheelArea.Width, FBarWidth);
    FWheelArea.Bottom := newAlphaBounds.Top - FMargin;
  end;
  if SetRectBounds(FAlphascale.bounds, newAlphaBounds) or (FBarsAlign <> prevBarsAlign) then
  begin
    BGRAReplace(FAlphascale.bmp, TBGRABitmap.Create(FAlphascale.Bounds.Width, FAlphascale.Bounds.Height));
    if FBarsAlign = alRight then
      FAlphascale.bmp.GradientFill(0, 0, FAlphascale.bmp.width, FAlphascale.bmp.height,
        FormTextColor, Color, gtLinear,
        PointF(0, -0.5), PointF(0, FAlphascale.bmp.Height-0.5), dmSet, True)
    else
    begin
      FAlphascale.bmp.GradientFill(0, 0, FAlphascale.bmp.width, FAlphascale.bmp.height,
        FormTextColor, Color, gtLinear,
        PointF(FAlphascale.bmp.Width-0.5, 0), PointF(-0.5, 0), dmSet, True);
      FAlphascale.bmp.FontHeight := FTitleFontHeight;
      FAlphascale.bmp.FontVerticalAnchor:= fvaCapCenter;
      FAlphascale.bmp.TextOut(FMargin/2, FAlphascale.bmp.Height/2, rsOpacity, FormTextColor, taLeftJustify);
      FAlphascale.bmp.FontVerticalAnchor:= fvaTop;
    end;
  end;

  if FBarsAlign = alRight then
  begin
    newLightscaleBounds := RectWithSize(FWheelArea.Right - FCursorPlace - FBarWidth, FWheelArea.Top,
                                   FBarWidth, FWheelArea.Height);
    FWheelArea.Right := newLightscaleBounds.Left - FMargin;
  end
  else
  begin
    newLightscaleBounds := RectWithSize(FWheelArea.Left, FWheelArea.Bottom - FCursorPlace - FBarWidth,
                                    FWheelArea.Width, FBarWidth);
    FWheelArea.Bottom := newLightscaleBounds.Top - FMargin;
  end;
  needUpdateLightscale := SetRectBounds(FLightscale.bounds, newLightscaleBounds)
                          or (FBarsAlign <> prevBarsAlign);

  diffXY := FWheelArea.Width - FWheelArea.Height;
  if diffXY > 0 then
  begin
    inc(FWheelArea.Left, diffXY div 2);
    FWheelArea.Right := FWheelArea.Left + FWheelArea.Height;
  end else
  begin
    dec(FWheelArea.Top, diffXY div 2);
    FWheelArea.Bottom := FWheelArea.Top + FWheelArea.Width;
  end;
  delta := min(FWheelArea.Left - (FButtonSize + FMargin) div 2, FWheelArea.Top - FMargin div 2);
  delta := min(delta, DoScaleX(120, OriginalDPI, TFChooseColor_CustomDPI) - FWheelArea.Width);
  if delta > 0 then
  begin
    dec(FWheelArea.Left, delta div 2);
    inc(FWheelArea.Right, (delta+1) div 2);
    dec(FWheelArea.Top, delta);
    FColorTitleVisible := delta <= FMargin div 2;
  end else
    FColorTitleVisible := true;

  FColorCircle.center := PointF((FWheelArea.Left + FWheelArea.Right)/2 - 0.5,
                               (FWheelArea.Top + FWheelArea.Bottom)/2 - 0.5);

  needUpdateColorCircle := SetRectBounds(FColorCircle.bounds, FWheelArea);
  if needUpdateColorCircle then
    BGRAReplace(FColorCircle.bmpMaxlight,
                ComputeColorCircle(FColorCircle.Bounds.Width, FColorCircle.Bounds.Height, $FFFF));

  UpdateColorview(needUpdateColorCircle, needUpdateLightscale, False);
end;

procedure TFChooseColor.SetCurrentColor(value: TBGRAPixel);
var newcolorlight: word;
  newcurrentcolor: TBGRAPixel;
begin
   newcolorlight := ColorLightOf(value);
   newcurrentColor := ColorWithLight(value,$FFFF);
   if (newcolorlight<>FColorLight) or (newcurrentcolor <> FCurrentColor) then
   begin
     FColorLight := newcolorlight;
     FCurrentColor := newcurrentcolor;
     UpdateColorView(true,true,true);
   end;
end;

function TFChooseColor.GetCurrentColor: TBGRAPixel;
begin
   result := ColorWithLight(FCurrentColor,FColorLight);
end;

procedure TFChooseColor.UpdateColorview(UpdateColorCircle, UpdateLightScale, Redraw: boolean);
var tempColor: TBGRAPixel;
  idxCSS: Integer;
  strColor:string;
begin
   if not FInitialized then exit;
   if UpdateLightScale and (FLightscale.Bounds.Right > FLightscale.Bounds.Left) then
   begin
     if FLightscale.bmp <> nil then FreeAndNil(FLightscale.bmp);
     FLightscale.bmp := TBGRABitmap.Create(FLightscale.Bounds.Right-FLightscale.Bounds.Left,FLightscale.Bounds.Bottom-FLightscale.Bounds.Top);
     tempColor := ColorWithLight(FCurrentColor,$FFFF);
     tempColor.alpha := 255;
     if FBarsAlign = alRight then
       FLightscale.bmp.GradientFill(0, 0, FLightscale.bmp.width, FLightscale.bmp.height,
         tempColor, BGRABlack, gtLinear,
         PointF(0, -0.5), PointF(0, FLightscale.bmp.height-0.5), dmSet, True)
     else
     begin
       FLightscale.bmp.GradientFill(0, 0, FLightscale.bmp.width, FLightscale.bmp.height,
         tempColor, BGRABlack, gtLinear,
         PointF(FLightscale.bmp.width-0.5, 0), PointF(-0.5, 0), dmSet, True);
       FLightscale.bmp.FontHeight := FTitleFontHeight;
       FLightscale.bmp.FontVerticalAnchor:= fvaCapCenter;
       FLightscale.bmp.TextOut(FMargin/2, FLightscale.bmp.Height/2, rsLight, BGRA(210,210,210), taLeftJustify);
       FLightscale.bmp.FontVerticalAnchor:= fvaTop;
     end;
   end;

   if UpdateColorCircle and (FColorCircle.bmpMaxLight <> nil) then
   begin
     if FColorCircle.bmp <> nil then FreeAndNil(FColorCircle.bmp);
     FColorCircle.bmp := BitmapWithLight(FColorCircle.bmpMaxlight, max(10000, FColorLight), FCurrentColor,ColorX,ColorY);
   end;

   tempColor := GetCurrentColor;
   if (FLazPaintInstance = nil) or FLazPaintInstance.BlackAndWhite then
     strColor := inttostr(tempColor.green) + ', a:' + FloatToStr(round(tempColor.alpha/255*100)/100)
   else
     begin
       strColor := '#'+copy(BGRAToStr(tempColor),1,6);
       idxCSS:= CSSColors.IndexOfColor(BGRA(tempColor.red,tempColor.green,tempColor.blue),500);
       if idxCSS <> -1 then strColor:= strColor + ', ' + CSSColors.Name[idxCSS] + ', a:' + FloatToStr(round(tempColor.alpha/255*100)/100) else
         strColor := strColor+ '  rgba(' + IntToStr(tempColor.red) + ',' + IntToStr(tempColor.green) + ',' +
           IntToStr(tempColor.blue) + ',' + FloatToStr(round(tempColor.alpha/255*100)/100) + ')';
     end;

   LColor.Caption := strColor;
   if Redraw then vsColorView.RedrawBitmap;
end;

function TFChooseColor.GetAvailableBmpHeight: integer;
begin
  result := ClientHeight - FTextAreaHeight - ExternalMargin;
end;

function TFChooseColor.GetAvailableBmpWidth: integer;
begin
  result := ClientWidth - ExternalMargin*2;
end;

procedure TFChooseColor.SetAvailableBmpHeight(AValue: integer);
begin
  ClientHeight := AValue+EColor.Height+ExternalMargin;
end;

procedure TFChooseColor.SetDarkTheme(AValue: boolean);
begin
  if FDarkTheme=AValue then Exit;
  FDarkTheme:=AValue;
  ApplyTheme;
end;

procedure TFChooseColor.SetLazPaintInstance(AValue: TLazPaintCustomInstance);
begin
  if FLazPaintInstance=AValue then Exit;
  FLazPaintInstance:=AValue;
  If (FLazPaintInstance = nil) or FLazPaintInstance.BlackAndWhite then
  begin
    ClientWidth := DoScaleY(110,OriginalDPI,TFChooseColor_CustomDPI);
    ClientHeight := DoScaleY(190,OriginalDPI,TFChooseColor_CustomDPI);
  end else
  begin
    ClientWidth := DoScaleY(223,OriginalDPI,TFChooseColor_CustomDPI);
    ClientHeight := DoScaleY(190,OriginalDPI,TFChooseColor_CustomDPI);
  end;
  Constraints.MinWidth := ClientWidth div 2 + (Width - ClientWidth);
  Constraints.MinHeight := ClientHeight div 2 + (Height - ClientHeight);
end;

procedure TFChooseColor.SetColorLight(value: word);
begin
   if FColorLight <> value then
   begin
     FColorLight := value;
     UpdateColorview(True,False,True);
   end;
end;

function TFChooseColor.ColorWithLight(c: TBGRAPixel; light: word): TBGRAPixel;
var
  ec: TExpandedPixel;
  cur: word;
begin
   ec := GammaExpansion(c);
   cur := max(max(ec.Red,ec.green),ec.blue);
   if cur = 0 then
   begin
     ec.red := light;
     ec.green := light;
     ec.blue := light;
   end else
   begin
     ec.red := (ec.red*light+cur shr 1) div cur;
     ec.green := (ec.green*light+cur shr 1) div cur;
     ec.blue := (ec.blue*light+cur shr 1) div cur;
   end;
   result := GammaCompression(ec)
end;

function TFChooseColor.BitmapWithLight(bmpsrc: TBGRABitmap; light: word;
  lookFor: TBGRAPixel; var pColorX, pColorY: integer): TBGRABitmap;
var xb,yb: integer;
    psrc,pdest: PBGRAPixel;
    dist,newDist: integer;
    c: TBGRAPixel;
    colorXsum,colorYsum,colorXYnb: integer;
begin
  result:= TBGRABitmap.Create(bmpsrc.Width,bmpsrc.Height);
  dist := 256*3;
  colorXsum := 0;
  colorYsum := 0;
  colorXYnb := 0;
  for yb := 0 to bmpsrc.Height-1 do
  begin
    psrc := bmpsrc.scanline[yb];
    pdest := result.scanline[yb];
    For xb := 0 to bmpsrc.Width-1 do
    begin
      c := psrc^;
      if c.alpha <> 0 then
      begin
        newDist := abs(c.red-lookFor.red)+abs(c.green-lookFor.green)+abs(c.blue-lookFor.blue);
        if newDist < dist then
        begin
          dist := newDist;
          colorXsum := xb;
          colorYsum := yb;
          colorXYnb := 1;
        end else
        if newDist = dist then
        begin
          inc(colorXsum,xb);
          inc(colorYsum,yb);
          inc(colorXYnb);
        end;
        pdest^ := ColorWithLight(c,light);
      end;
      inc(pdest);
      inc(psrc);
    end;
  end;
  result.InvalidateBitmap;
  if colorXYnb <> 0 then
  begin
    pColorX := (ColorXsum + colorXYnb shr 1) div colorXYnb;
    pColorY := (ColorYsum + colorXYnb shr 1) div colorXYnb;
  end;
end;

function TFChooseColor.ColorLightOf(c: TBGRAPixel): word;
var
  ec: TExpandedPixel;
begin
   ec := GammaExpansion(c);
   result := max(max(ec.Red,ec.green),ec.blue);
end;

{$R *.lfm}

end.

