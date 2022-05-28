// SPDX-License-Identifier: GPL-3.0-only
unit UChooseColorInterface;

{$mode objfpc}{$H+}

interface

uses
  Types, Classes, SysUtils, Controls, BGRAVirtualScreen, BCButton, StdCtrls,
  BGRABitmap, BGRABitmapTypes, LazPaintType, Graphics;

const
  ExternalMargin = 3;

type

  { TChooseColorInterface }

  TChooseColorInterface = class
  private
    Container: TWinControl;
    vsColorView: TBGRAVirtualScreen;
    BCButton_AddToPalette: TBCButton;
    BCButton_RemoveFromPalette: TBCButton;
    EColor: TEdit;
    LColor: TLabel;
    procedure ContainerResize(Sender: TObject);
    procedure SetColorTarget(AValue: TColorTarget);
    procedure vsColorViewMouseDown(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure vsColorViewMouseMove(Sender: TObject; {%H-}Shift: TShiftState; X,
      Y: Integer);
    procedure vsColorViewMouseUp(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure vsColorViewRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure BCButton_AddToPaletteClick(Sender: TObject);
    procedure BCButton_RemoveFromPaletteClick(Sender: TObject);
    procedure EColorChange(Sender: TObject);
    procedure EColorKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure LColorMouseDown(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
  protected
    FDPI: integer;
    FDarkTheme: boolean;
    FTextAreaHeight: integer;
    FTextAreaHeightComputed: Boolean;
    FBarsAlign, FButtonsAlign: TAlign;
    FButtonsCenter: boolean;
    FTitleFontHeight: single;
    FColorTitleVisible: boolean;
    FFormBackgroundColor, FFormTextColor: TBGRAPixel;
    FColorBeforeEColor: TBGRAPixel;

    FInFormMouseMove: Boolean;
    FormMouseMovePos: TPointF;

    FColorTarget: TColorTarget;
    FCurrentColor: TBGRAPixel;
    FCurrentColorFormatError: boolean;
    FColorLight: word;
    FColorX,FColorY: single;
    FSelectZone: (szNone, szColorCircle, szLightScale, szAlphascale);
    FBitmapScale: single;
    FColorCircle: record center: TPointF;
                         bounds: TRectF;
                         bmp,bmpMaxlight: TBGRABitmap; end;
    FLightscale: record bounds: TRectF;
                        bmp: TBGRABitmap;
                        cursorRect: TRectF; end;
    FAlphascale: record bounds: TRectF;
                       bmp: TBGRABitmap;
                       cursorRect: TRectF; end;

    FTopMargin, FBarWidth, FCursorPlace, FMargin, FInternalMargin,
    FCursorMargin, FCursorSize, FColorXYSize, FCursorXYWidth: single;
    FButtonSize: integer;

    FInitialized: boolean;
    FLazPaintInstance: TLazPaintCustomInstance;

    function GetAvailableVSHeight: integer;
    function GetAvailableVSWidth: integer;
    function InterfaceToPixel(APoint: TPointF): TPoint;
    function InterfaceToPixel(ARect: TRectF): TRect;
    function LCLPosToInterface(APoint: TPoint): TPointF;
    function PixelToInterface(APoint: TPoint): TPointF;
    function GetEditorVisible: boolean;
    procedure SetDarkTheme(AValue: boolean);
    procedure SetLazPaintInstance(AValue: TLazPaintCustomInstance);
    procedure UpdateColorview(UpdateColorCircle, UpdateLightScale, Redraw: boolean);
    procedure SetColorLight(value: word);
    function ColorWithLight(c: TBGRAPixel; light: word): TBGRAPixel;
    function BitmapWithLight(bmpsrc: TBGRABitmap; light: word; lookFor: TBGRAPixel; var pColorX,pColorY: single): TBGRABitmap;
    function ColorLightOf(c: TBGRAPixel): word;
    function DrawTriangleCursor(dest: TBGRABitmap; bounds: TRectF; value, maxValue: integer): TRectF;
    procedure DoClick(X,Y: single);
    procedure DoSelect(X,Y: single);
    function MakeIconBase(size: integer): TBitmap;
    function MakeAddIcon(size: integer): TBitmap;
    function MakeRemoveIcon(size: integer): TBitmap;
    procedure NeedTextAreaHeight;

    procedure ApplyTheme;
    procedure UpdateButtonLayout;
    function SetRectBounds(var ABounds: TRectF; ANewBounds: TRectF): boolean;
    function PreferredBarsAlignWithWidth: TAlign;
    procedure UpdateLayout;
    function GetAlphaScaleBmp: TBGRABitmap;
    function GetLightScaleBmp: TBGRABitmap;
    function GetColorCircleMaxLightBmp: TBGRABitmap;
    function GetColorCircleBmp: TBGRABitmap;
    property AvailableVSHeight: integer read GetAvailableVSHeight;
    property AvailableVSWidth: integer read GetAvailableVSWidth;
  public


    constructor Create(AContainer: TWinControl; ADPI: integer);
    destructor Destroy; override;

    procedure SetCurrentColor(value: TBGRAPixel; AFromEdit: boolean = false);
    function GetCurrentColor: TBGRAPixel;
    procedure HideEditor;
    function GetPreferredSize: TSize;
    procedure AdjustControlHeight;

    property DarkTheme: boolean read FDarkTheme write SetDarkTheme;
    property LazPaintInstance: TLazPaintCustomInstance read FLazPaintInstance write SetLazPaintInstance;
    property EditorVisible: boolean read GetEditorVisible;
    property ColorTarget: TColorTarget read FColorTarget write SetColorTarget;
  end;

implementation

uses math, Forms, UResourceStrings, LCLType, UDarkTheme, LCScaleDPI, UGraph, BGRAText,
  BGRAClasses, ULoading;

{ TChooseColorInterface }

procedure TChooseColorInterface.vsColorViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    if EColor.Visible then
    begin
      EColor.Hide;
      LColor.Show;
    end;
    with LCLPosToInterface(Point(X, Y)) do
      DoClick(X,Y);
  end;
end;

procedure TChooseColorInterface.ContainerResize(Sender: TObject);
begin
  NeedTextAreaHeight;
  vsColorView.Width := AvailableVSWidth;
  vsColorView.Height := AvailableVSHeight;
  EColor.Left := ExternalMargin;
  EColor.Width := Container.ClientWidth - 2*ExternalMargin;
  LColor.Left := ExternalMargin;
  LColor.Width := Container.ClientWidth - 2*ExternalMargin;
  EColor.Top := Container.ClientHeight - FTextAreaHeight + (FTextAreaHeight-EColor.Height) div 2;
  LColor.Top := Container.ClientHeight - FTextAreaHeight + (FTextAreaHeight-LColor.Height) div 2;
  UpdateLayout;
end;

procedure TChooseColorInterface.SetColorTarget(AValue: TColorTarget);
begin
  if FColorTarget=AValue then Exit;
  FColorTarget:=AValue;
  vsColorView.DiscardBitmap;
end;

procedure TChooseColorInterface.vsColorViewMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  FormMouseMovePos := LCLPosToInterface(Point(X,Y));
  if FInFormMouseMove then Exit;
  FInFormMouseMove := True;
  Application.ProcessMessages; //empty message stack
  DoSelect(FormMouseMovePos.X, FormMouseMovePos.Y);
  FInFormMouseMove := False;
end;

procedure TChooseColorInterface.vsColorViewMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then FSelectZone := szNone;
end;

procedure TChooseColorInterface.vsColorViewRedraw(Sender: TObject;
  Bitmap: TBGRABitmap);
var
  textBoundRight: single;

  procedure AddTitle(xRef: single; AText: string);
  var x, w: single;
  begin
    w := Bitmap.TextSize(AText).cx / FBitmapScale;
    x := xRef - w/2;
    x := min(x, textBoundRight - w);
    x := max(x, FButtonSize + FMargin + FMargin / 2);
    if x > textBoundRight - w then exit;
    with InterfaceToPixel(PointF(x, FMargin/2)) do
      Bitmap.TextOut(x, y, AText, FFormTextColor, taLeftJustify);
    textBoundRight := x - FMargin/2;
  end;

var
  bmpRect: TRect;
  previewSize: single;
  previewRect: TRectF;
  c: TBGRAPixel;
  bmpColorXYSize: integer;
  bmpCursorWidth, i: integer;
  bmpCursorOpacity: byte;
  s: String;
begin
  if vsColorView.Width <> 0 then
    FBitmapScale := Bitmap.Width / vsColorView.Width
    else FBitmapScale := 1;
  Bitmap.FontHeight := round(FTitleFontHeight * FBitmapScale);
  Bitmap.FontAntialias := True;
  textBoundRight := Bitmap.Width / FBitmapScale;
  if Assigned(GetAlphaScaleBmp) then
  begin
    if FBarsAlign = alRight then
      AddTitle((FAlphascale.bounds.Left + FAlphascale.bounds.Right) / 2, rsOpacity);
    bmpRect := InterfaceToPixel(FAlphascale.bounds);
    Bitmap.PutImage(bmpRect.Left, bmpRect.Top, GetAlphaScaleBmp, dmDrawWithTransparency);
    Bitmap.Rectangle(bmpRect,
                     BGRA(FFormTextColor.red, FFormTextColor.green, FFormTextColor.Blue,128),
                     dmDrawWithTransparency);
    FAlphascale.cursorRect := DrawTriangleCursor(Bitmap, FAlphascale.bounds, FCurrentColor.alpha, 255);
  end else FAlphascale.cursorRect := EmptyRectF;

  if Assigned(GetLightscaleBmp) then
  begin
    if FBarsAlign = alRight then
      AddTitle((FLightscale.bounds.Left + FLightscale.bounds.Right) / 2, rsLight);
    bmpRect := InterfaceToPixel(FLightscale.bounds);
    Bitmap.PutImage(bmpRect.Left, bmpRect.top, GetLightscaleBmp, dmFastBlend);
    Bitmap.Rectangle(bmpRect,
                     BGRA(FFormTextColor.red, FFormTextColor.green, FFormTextColor.Blue, 128),
                     dmDrawWithTransparency);
    FLightscale.cursorRect := DrawTriangleCursor(Bitmap, FLightscale.bounds, FColorLight, 65535);
  end else FLightscale.cursorRect := EmptyRectF;

  if Assigned(GetColorCircleBmp) and not FColorCircle.bounds.IsEmpty then
  begin
    if FColorTitleVisible then
    begin
      case ColorTarget of
        ctForeColorSolid..ctForeColorEndGrad: s := rsPen;
        ctBackColorSolid..ctBackColorEndGrad: s := rsBack;
        ctOutlineColorSolid..ctOutlineColorEndGrad: s := rsTextOutline;
        else s := rsColors;
      end;
      case ColorTarget of
        ctForeColorStartGrad,ctBackColorStartGrad,ctOutlineColorStartGrad: s += '[1]';
        ctForeColorEndGrad,ctBackColorEndGrad,ctOutlineColorEndGrad: s += '[2]';
      end;
      AddTitle(FColorCircle.center.X, s);
    end;
    bmpRect := InterfaceToPixel(FColorCircle.bounds);
    Bitmap.PutImage(bmpRect.Left, bmpRect.top, GetColorCircleBmp, dmDrawWithTransparency);
    bmpCursorWidth := round(FCursorXYWidth * FBitmapScale);
    if FCursorXYWidth * FBitmapScale < 1 then
      bmpCursorOpacity := round(FCursorXYWidth * FBitmapScale * 255)
      else bmpCursorOpacity := 255;
    with InterfaceToPixel(FColorCircle.bounds.TopLeft + PointF(FColorX, FColorY)) do
    begin
      bmpColorXYSize := round(FColorXYSize * FBitmapScale);
      bmpRect := rect(x-bmpColorXYSize, y-bmpColorXYSize, x+bmpColorXYSize+1, y+bmpColorXYSize+1);
      bmpRect.Inflate(-1, -1);
      Bitmap.Rectangle(bmpRect, BGRA(0,0,0,bmpCursorOpacity), dmDrawWithTransparency);
      bmpRect.Inflate(1, 1);
      for i := 1 to bmpCursorWidth do
      begin
        Bitmap.Rectangle(bmpRect, BGRA(255,255,255,bmpCursorOpacity), dmDrawWithTransparency);
        bmpRect.Inflate(1, 1);
      end;
      Bitmap.Rectangle(bmpRect, BGRA(0,0,0,bmpCursorOpacity), dmDrawWithTransparency);
    end;
    previewSize := round(FBarWidth*0.9);
    previewRect.Left := FMargin - ExternalMargin;
    if FBarsAlign = alBottom then
      previewRect.Top := FLightscale.bounds.Top - FMargin - previewSize
      else previewRect.Top := Bitmap.Height/FBitmapScale - FMargin - previewSize;
    previewRect.Right := previewRect.Left + previewSize;
    previewRect.Bottom := previewRect.Top + previewSize;

    if previewRect.Top >= BCButton_RemoveFromPalette.Top + BCButton_RemoveFromPalette.Height
                          + FMargin / 2 then
    begin
      c := GetCurrentColor;
      c.alpha := 255;
      with InterfaceToPixel(previewRect) do
        Bitmap.RoundRectAntialias(Left, Top, Right - 1, Bottom - 1,
            previewSize/6, previewSize/6, BGRA(0,0,0,192), bmpCursorWidth, c, []);
    end;
  end;
end;

procedure TChooseColorInterface.BCButton_AddToPaletteClick(Sender: TObject);
begin
  FLazPaintInstance.AddColorToPalette(GetCurrentColor);
end;

procedure TChooseColorInterface.BCButton_RemoveFromPaletteClick(Sender: TObject);
begin
  FLazPaintInstance.RemoveColorFromPalette(GetCurrentColor);
end;

procedure TChooseColorInterface.EColorChange(Sender: TObject);
var newColor: TBGRAPixel;
  errPos,value: integer;
begin
   if (FLazPaintInstance <> nil) and EColor.Visible then
   begin
     if FLazPaintInstance.BlackAndWhite then
     begin
       val(EColor.Text,value,errPos);
       if (errPos = 0) and (value >= 0) and (value <= 255) then
       begin
         FCurrentColorFormatError := false;
         newColor.green := value;
         newColor.red := value;
         newColor.blue := value;
       end else
         FCurrentColorFormatError := true;
     end else
       newColor := PartialStrToBGRA(EColor.Text,FColorBeforeEColor,FCurrentColorFormatError);
     if not FCurrentColorFormatError then
     begin
       newColor.alpha := FColorBeforeEColor.alpha;
       SetCurrentColor(newColor, true);
       FLazPaintInstance.ColorFromFChooseColor;
     end;
   end;
end;

procedure TChooseColorInterface.EColorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    if FCurrentColorFormatError then
    begin
      MessagePopup(rsInvalidName, 3000);
      FCurrentColorFormatError := false;
    end;
    HideEditor;
    Key := 0;
  end
  else
  if Key = VK_ESCAPE then
  begin
    HideEditor;
    Key := 0;
    SetCurrentColor(FColorBeforeEColor);
    FLazPaintInstance.ColorFromFChooseColor;
  end;
end;

procedure TChooseColorInterface.LColorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  LColor.Visible := false;
  FColorBeforeEColor := GetCurrentColor;
  if FLazPaintInstance.BlackAndWhite then
  begin
    EColor.Text := inttostr(FColorBeforeEColor.green);
  end else
    EColor.Text := '#' + copy(BGRAToStr(FColorBeforeEColor),1,6);
  EColor.Visible := true;
  EColor.Top := Container.ClientHeight - EColor.Height;
  FCurrentColorFormatError := false;
  SafeSetFocus(EColor);
end;

function TChooseColorInterface.GetAvailableVSHeight: integer;
begin
  result := Container.ClientHeight - FTextAreaHeight - ExternalMargin;
end;

function TChooseColorInterface.GetAvailableVSWidth: integer;
begin
  result := Container.ClientWidth - ExternalMargin*2;
end;

function TChooseColorInterface.InterfaceToPixel(APoint: TPointF): TPoint;
begin
  result := (APoint * FBitmapScale).Truncate;
end;

function TChooseColorInterface.InterfaceToPixel(ARect: TRectF): TRect;
begin
  result.TopLeft := InterfaceToPixel(ARect.TopLeft);
  result.BottomRight := InterfaceToPixel(ARect.BottomRight);
end;

function TChooseColorInterface.LCLPosToInterface(APoint: TPoint): TPointF;
begin
  result.x := APoint.x + 0.5 / FBitmapScale;
  result.y := APoint.y + 0.5 / FBitmapScale;
end;

function TChooseColorInterface.PixelToInterface(APoint: TPoint): TPointF;
begin
  result.x := (APoint.x + 0.5) / FBitmapScale;
  result.y := (APoint.y + 0.5) / FBitmapScale;
end;

function TChooseColorInterface.GetEditorVisible: boolean;
begin
  result := Assigned(EColor) and EColor.Visible;
end;

procedure TChooseColorInterface.SetDarkTheme(AValue: boolean);
begin
  if FDarkTheme=AValue then Exit;
  FDarkTheme:=AValue;
  ApplyTheme;
end;

procedure TChooseColorInterface.SetLazPaintInstance(
  AValue: TLazPaintCustomInstance);
begin
  if FLazPaintInstance=AValue then Exit;
  FLazPaintInstance:=AValue;
end;

procedure TChooseColorInterface.UpdateColorview(UpdateColorCircle,
  UpdateLightScale, Redraw: boolean);
var tempColor: TBGRAPixel;
  idxCSS: Integer;
  strColor:string;
begin
   if not FInitialized then exit;
   if UpdateLightScale then FreeAndNil(FLightscale.bmp);
   if UpdateColorCircle then FreeAndNil(FColorCircle.bmp);

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

procedure TChooseColorInterface.SetColorLight(value: word);
begin
  if FColorLight <> value then
  begin
    FColorLight := value;
    UpdateColorview(True, False, True);
  end;
end;

function TChooseColorInterface.ColorWithLight(c: TBGRAPixel; light: word): TBGRAPixel;
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

function TChooseColorInterface.BitmapWithLight(bmpsrc: TBGRABitmap;
  light: word; lookFor: TBGRAPixel; var pColorX, pColorY: single): TBGRABitmap;
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
    with PixelToInterface(Point((ColorXsum + colorXYnb shr 1) div colorXYnb,
                                (ColorYsum + colorXYnb shr 1) div colorXYnb)) do
    begin
      pColorX := x;
      pColorY := y;
    end;
  end;
end;

function TChooseColorInterface.ColorLightOf(c: TBGRAPixel): word;
var
  ec: TExpandedPixel;
begin
   ec := GammaExpansion(c);
   result := max(max(ec.Red,ec.green),ec.blue);
end;

function TChooseColorInterface.DrawTriangleCursor(dest: TBGRABitmap;
  bounds: TRectF; value, maxValue: integer): TRectF;
var x,y: single;
  bmpCursorSize: integer;
begin
  bmpCursorSize := round(FCursorSize * FBitmapScale);
  if FBarsAlign = alRight then
  begin
    x := bounds.right + FCursorMargin;
    y := bounds.top + (maxValue - value) / (maxValue + 1) * bounds.height;
    with InterfaceToPixel(PointF(x, y)) do
      dest.FillPolyAntialias([pointF(x, y), pointF(x+bmpCursorSize, y-bmpCursorSize),
                              pointF(x+bmpCursorSize, y+bmpCursorSize)], FFormTextColor);
    result := rectF(x - FCursorSize/2, y - FCursorSize*1.5,
                    x + FCursorSize*1.5, y + FCursorSize*1.5);
  end else
  begin
    x := bounds.left + value / (maxValue + 1) * bounds.Width;
    y := bounds.bottom + FCursorMargin;
    with InterfaceToPixel(PointF(x, y)) do
      dest.FillPolyAntialias([pointF(x, y), pointF(x+bmpCursorSize,y+bmpCursorSize),
                              pointF(x-bmpCursorSize, y+bmpCursorSize)], FFormTextColor);
    result := rectF(x - FCursorSize*1.5, y - FCursorSize/2,
                    x + FCursorSize*1.5, y + FCursorSize*1.5);
  end;
end;

procedure TChooseColorInterface.DoClick(X, Y: single);
begin
  if FColorCircle.Bounds.Contains(PointF(X,Y)) and
     (VectLen(PointF(X,Y) - FColorCircle.center) <=
      max(FColorCircle.bounds.Width, FColorCircle.bounds.Height)/2 + DoScaleX(14, 96)) then
  begin
    FSelectZone := szColorCircle;
    DoSelect(X,Y);
  end else
  if FAlphascale.Bounds.Contains(PointF(X,Y)) or FAlphascale.cursorRect.Contains(PointF(X,Y)) then
  begin
    FSelectZone := szAlphascale;
    DoSelect(X,Y);
  end else
  if FLightscale.Bounds.Contains(PointF(X,Y)) or FLightscale.cursorRect.Contains(PointF(X,Y)) then
  begin
    FSelectZone := szLightscale;
    DoSelect(X,Y);
  end;
end;

procedure TChooseColorInterface.DoSelect(X, Y: single);
var pix, newColor: TBGRAPixel;
  newLight: Word;
  dist: single;
begin
  case FSelectZone of
  szAlphascale:
    begin
      if FBarsAlign = alRight then
        FCurrentColor.alpha := max(0, min(255, 255 -
          trunc((Y-FAlphascale.Bounds.Top) / FAlphascale.Bounds.Height * 256) ))
        else FCurrentColor.alpha := max(0, min(255,
          trunc((X-FAlphascale.Bounds.Left) / FAlphascale.Bounds.Width * 256) ));
      UpdateColorview(False, False, True);
    end;
  szColorCircle:
    if Assigned(FColorCircle.bmpMaxlight) then
    begin
      dist := sqrt(sqr((x-FColorCircle.center.X)/FColorCircle.bounds.Width*2) +
              sqr((y-FColorCircle.center.Y)/FColorCircle.bounds.Height*2));
      if dist > 1 then
      begin
        x := FColorCircle.center.X + (x - FColorCircle.center.X)/dist;
        y := FColorCircle.center.Y + (y - FColorCircle.center.Y)/dist;
      end;
      with InterfaceToPixel(PointF(x,y) - FColorCircle.bounds.TopLeft) do
        pix := FColorCircle.bmpMaxlight.GetPixel(x,y);
      if pix.alpha <> 0 then
      begin
        newColor := BGRA(pix.Red,pix.Green,pix.Blue,FCurrentColor.Alpha);
        if not FCurrentColor.EqualsExactly(newColor) then
        begin
          FCurrentColor := newColor;
          FColorX := x-FColorCircle.Bounds.Left;
          FColorY := y-FColorCircle.Bounds.top;
          UpdateColorview(False, True, True);
        end;
      end;
    end;
  szLightScale:
    begin
      if FBarsAlign = alRight then
        newLight := max(0, min(65535, 65535 -
          trunc((Y-FLightscale.Bounds.Top) / FLightscale.Bounds.Height * 65536) ))
        else newLight := max(0, min(65535,
          trunc((X-FLightscale.Bounds.Left) / FLightscale.Bounds.Width * 65536)));
      SetColorLight(newLight);
    end;
  else exit;
  end;
  FLazPaintInstance.ColorFromFChooseColor;
end;

function TChooseColorInterface.MakeIconBase(size: integer): TBitmap;
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

function TChooseColorInterface.MakeAddIcon(size: integer): TBitmap;
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

function TChooseColorInterface.MakeRemoveIcon(size: integer): TBitmap;
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

procedure TChooseColorInterface.NeedTextAreaHeight;
begin
  if not FTextAreaHeightComputed then
  begin
    EColor.Show;
    EColor.AdjustSize;
    FTextAreaHeight := max(EColor.Height, LColor.Height);
    FTextAreaHeightComputed := true;
    EColor.Hide;
  end;
end;

procedure TChooseColorInterface.ApplyTheme;
begin
  DarkThemeInstance.Apply(BCButton_AddToPalette, FDarkTheme);
  DarkThemeInstance.Apply(BCButton_RemoveFromPalette, FDarkTheme);
  DarkThemeInstance.Apply(LColor, FDarkTheme);
  FFormBackgroundColor := DarkThemeInstance.GetColorForm(FDarkTheme);
  FFormTextColor := DarkThemeInstance.GetColorButtonText(FDarkTheme);
  Container.Color := FFormBackgroundColor;
  vsColorView.Color := FFormBackgroundColor;
  vsColorView.DiscardBitmap;
end;

procedure TChooseColorInterface.UpdateButtonLayout;
var
  iconSize: Integer;
  tmpIcon: TBitmap;
  scaling, invScaling: single;
begin
  FButtonSize := round(FBarWidth);
  BCButton_AddToPalette.Width := FButtonSize;
  BCButton_AddToPalette.Height := FButtonSize;
  BCButton_RemoveFromPalette.Width := FButtonSize;
  BCButton_RemoveFromPalette.Height := FButtonSize;
  BCButton_AddToPalette.Left := round(FMargin - ExternalMargin);
  BCButton_AddToPalette.Top := round(FMargin / 2);
  if FButtonsAlign = alLeft then
  begin
    if FButtonsCenter then
      BCButton_AddToPalette.Top := (AvailableVSHeight -
        BCButton_AddToPalette.Height - BCButton_RemoveFromPalette.Height) div 2;
    BCButton_RemoveFromPalette.Left := round(FMargin - ExternalMargin);
    BCButton_RemoveFromPalette.Top := BCButton_AddToPalette.Top + BCButton_AddToPalette.Height;
  end else
  begin
    if FButtonsCenter then
      BCButton_AddToPalette.Left := (AvailableVSWidth -
        BCButton_AddToPalette.Width - BCButton_RemoveFromPalette.Width) div 2;
    BCButton_RemoveFromPalette.Left := BCButton_AddToPalette.Left + BCButton_AddToPalette.Width;
    BCButton_RemoveFromPalette.Top := round(FMargin / 2);
  end;
  scaling := Container.GetCanvasScaleFactor;
  iconSize := round((FButtonSize-4)*scaling);
  invScaling := 1/scaling;
  if not Assigned(BCButton_AddToPalette.Glyph) or (BCButton_AddToPalette.Glyph.Width <> iconSize)
     or (BCButton_AddToPalette.GlyphScale <> invScaling) then
  begin
    tmpIcon:= MakeAddIcon(iconSize);
    BCButton_AddToPalette.Glyph.Assign(tmpIcon);
    BCButton_AddToPalette.GlyphScale:= invScaling;
    tmpIcon.Free;
  end;
  if not Assigned(BCButton_RemoveFromPalette.Glyph) or (BCButton_RemoveFromPalette.Glyph.Width <> iconSize)
     or (BCButton_RemoveFromPalette.GlyphScale <> invScaling) then
  begin
    tmpIcon:= MakeRemoveIcon(iconSize);
    BCButton_RemoveFromPalette.Glyph.Assign(tmpIcon);
    BCButton_RemoveFromPalette.GlyphScale:= invScaling;
    tmpIcon.Free;
  end;
end;

function TChooseColorInterface.SetRectBounds(var ABounds: TRectF;
  ANewBounds: TRectF): boolean;
begin
  result := (ABounds.Width <> ANewBounds.Width) or (ABounds.Height <> ANewBounds.Height);
  ABounds := ANewBounds;
end;

function TChooseColorInterface.PreferredBarsAlignWithWidth: TAlign;
var
  oneBarWidth, internalMargin, margin, tx, ty: single;
begin
  margin := DoScaleYF(8, OriginalDPI, FDPI);
  oneBarWidth := DoScaleXF(18, OriginalDPI, FDPI) + DoScaleXF(10, OriginalDPI, FDPI) +
                 margin;
  internalMargin := max(0, margin - ExternalMargin);
  if (FLazPaintInstance = nil) or LazPaintInstance.BlackAndWhite then
  begin
    tx := AvailableVSWidth - 2*internalMargin;
    if tx <= oneBarWidth*4 then
      result := alRight
    else
      result := alBottom;
  end else
  begin
    tx := AvailableVSWidth - 2*internalMargin;
    ty := AvailableVSHeight - 2*margin;
    if tx >= ty then
      result := alRight
      else result := alBottom;
  end;
end;

procedure TChooseColorInterface.UpdateLayout;
var vsWidth, vsHeight: single;
    prevBarsAlign: TAlign;
    newAlphaBounds, newLightscaleBounds: TRectF;
    newColorCircleArea: TRectF;
    needUpdateLightscale, needUpdateColorCircle: Boolean;
    diffXY, delta: single;
    reductionFactor: single;

  function AdaptSizeX(ASize: integer): single;
  begin
    result := min(DoScaleXF(ASize, OriginalDPI, FDPI), ASize * reductionFactor);
  end;
  function AdaptSizeY(ASize: integer): single;
  begin
    result := min(DoScaleYF(ASize, OriginalDPI, FDPI), ASize * reductionFactor);
  end;

begin
  if FLazPaintInstance = nil then exit;

  prevBarsAlign := FBarsAlign;
  vsWidth := AvailableVSWidth;
  vsHeight := AvailableVSHeight;

  if LazPaintInstance.BlackAndWhite then
    reductionFactor := min(vsWidth, vsHeight)/105
  else
    reductionFactor := max(vsWidth, vsHeight)/200;
  FBarWidth := AdaptSizeX(18);
  FCursorPlace := AdaptSizeX(10);
  FCursorMargin := AdaptSizeX(2);
  FCursorSize := AdaptSizeX(6);
  FTopMargin := AdaptSizeY(27);
  FMargin := AdaptSizeY(8);
  FInternalMargin := FMargin-ExternalMargin;
  if FInternalMargin < 0 then FInternalMargin := 0;
  FTitleFontHeight := max(AdaptSizeY(12), 10);
  FColorXYSize := DoScaleXF(3, OriginalDPI, FDPI);
  FCursorXYWidth := DoScaleXF(0.5, OriginalDPI, FDPI);

  newColorCircleArea := RectF(FInternalMargin, FMargin, vsWidth - FInternalMargin, vsHeight - FMargin);

  if FLazPaintInstance.BlackAndWhite then
  begin
    FButtonsCenter := true;
    if newColorCircleArea.Width <= newColorCircleArea.Height then
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
    if newColorCircleArea.Width >= newColorCircleArea.Height then
      FBarsAlign := alRight else FBarsAlign := alBottom;
    UpdateButtonLayout;
  end;

  newColorCircleArea.Top := FTopMargin;

  if FBarsAlign = alRight then
  begin
    newAlphaBounds := RectWithSizeF(newColorCircleArea.Right - FCursorPlace - FBarWidth, newColorCircleArea.Top,
                                   FBarWidth, newColorCircleArea.Height);
    newColorCircleArea.Right := newAlphaBounds.Left - FMargin;
  end
  else
  begin
    newAlphaBounds := RectWithSizeF(newColorCircleArea.Left, newColorCircleArea.Bottom - FCursorPlace - FBarWidth,
                                   newColorCircleArea.Width, FBarWidth);
    newColorCircleArea.Bottom := newAlphaBounds.Top - FMargin;
  end;
  if SetRectBounds(FAlphascale.bounds, newAlphaBounds) or (FBarsAlign <> prevBarsAlign) then
    FreeAndNil(FAlphascale.bmp);

  if FBarsAlign = alRight then
  begin
    newLightscaleBounds := RectWithSizeF(newColorCircleArea.Right - FCursorPlace - FBarWidth, newColorCircleArea.Top,
                                   FBarWidth, newColorCircleArea.Height);
    newColorCircleArea.Right := newLightscaleBounds.Left - FMargin;
  end
  else
  begin
    newLightscaleBounds := RectWithSizeF(newColorCircleArea.Left, newColorCircleArea.Bottom - FCursorPlace - FBarWidth,
                                    newColorCircleArea.Width, FBarWidth);
    newColorCircleArea.Bottom := newLightscaleBounds.Top - FMargin;
  end;
  needUpdateLightscale := SetRectBounds(FLightscale.bounds, newLightscaleBounds)
                          or (FBarsAlign <> prevBarsAlign);

  if FButtonsAlign = alLeft then
  begin
    delta := FMargin - (newColorCircleArea.Left +
                        min(newColorCircleArea.Width, newColorCircleArea.Height)*(1-0.8) -
                        (FMargin - ExternalMargin + FButtonSize) );
    if delta >= 0 then
      incF(newColorCircleArea.Left, delta);
  end;

  diffXY := newColorCircleArea.Width - newColorCircleArea.Height;
  if diffXY > 0 then
  begin
    incF(newColorCircleArea.Left, diffXY / 2);
    newColorCircleArea.Right := newColorCircleArea.Left + newColorCircleArea.Height;
  end else
  begin
    decF(newColorCircleArea.Top, diffXY / 2);
    newColorCircleArea.Bottom := newColorCircleArea.Top + newColorCircleArea.Width;
  end;

  delta := min(newColorCircleArea.Left - (FButtonSize + FMargin) / 2, newColorCircleArea.Top - FMargin / 2);
  delta := min(delta, DoScaleXF(120, OriginalDPI, FDPI) - newColorCircleArea.Width);
  if delta > 0 then
  begin
    decF(newColorCircleArea.Left, delta / 2);
    incF(newColorCircleArea.Right, (delta+1) / 2);
    decF(newColorCircleArea.Top, delta);
    FColorTitleVisible := delta <= FMargin / 2;
  end else
    FColorTitleVisible := true;

  FColorCircle.center := PointF((newColorCircleArea.Left + newColorCircleArea.Right)/2 - 0.5,
                               (newColorCircleArea.Top + newColorCircleArea.Bottom)/2 - 0.5);

  needUpdateColorCircle := SetRectBounds(FColorCircle.bounds, newColorCircleArea);
  if needUpdateColorCircle then FreeAndNil(FColorCircle.bmpMaxlight);

  UpdateColorview(needUpdateColorCircle, needUpdateLightscale, False);
end;

function TChooseColorInterface.GetAlphaScaleBmp: TBGRABitmap;
begin
  if (FAlphascale.bmp = nil) and not FAlphascale.Bounds.IsEmpty then
  begin
    FAlphascale.bmp := TBGRABitmap.Create( round(FAlphascale.Bounds.Width * FBitmapScale),
                         round(FAlphascale.Bounds.Height * FBitmapScale) );
    if FBarsAlign = alRight then
      FAlphascale.bmp.GradientFill(0, 0, FAlphascale.bmp.width, FAlphascale.bmp.height,
        FFormTextColor, vsColorView.Color, gtLinear,
        PointF(0, -0.5), PointF(0, FAlphascale.bmp.Height-0.5), dmSet, True)
    else
    begin
      FAlphascale.bmp.GradientFill(0, 0, FAlphascale.bmp.width, FAlphascale.bmp.height,
        FFormTextColor, vsColorView.Color, gtLinear,
        PointF(FAlphascale.bmp.Width-0.5, 0), PointF(-0.5, 0), dmSet, True);
      FAlphascale.bmp.FontHeight := round(FTitleFontHeight * FBitmapScale);
      FAlphascale.bmp.FontVerticalAnchor:= fvaCapCenter;
      FAlphascale.bmp.TextOut(FMargin/2, FAlphascale.bmp.Height/2, rsOpacity, FFormTextColor, taLeftJustify);
      FAlphascale.bmp.FontVerticalAnchor:= fvaTop;
    end;
  end;
  result := FAlphascale.bmp;
end;

function TChooseColorInterface.GetLightScaleBmp: TBGRABitmap;
var
  tempColor: TBGRAPixel;
begin
  if (FLightscale.bmp = nil) and not FLightscale.Bounds.IsEmpty then
  begin
    FLightscale.bmp := TBGRABitmap.Create( round(FLightscale.Bounds.Width * FBitmapScale),
                         round(FLightscale.Bounds.Height * FBitmapScale) );
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
      FLightscale.bmp.FontHeight := round(FTitleFontHeight * FBitmapScale);
      FLightscale.bmp.FontVerticalAnchor:= fvaCapCenter;
      FLightscale.bmp.TextOut(FMargin/2, FLightscale.bmp.Height/2, rsLight, BGRA(210,210,210), taLeftJustify);
      FLightscale.bmp.FontVerticalAnchor:= fvaTop;
    end;
  end;
  result := FLightscale.bmp;
end;

function TChooseColorInterface.GetColorCircleMaxLightBmp: TBGRABitmap;
begin
  if (FColorCircle.bmpMaxlight = nil) and not LazPaintInstance.BlackAndWhite
     and not FColorCircle.bounds.IsEmpty then
    FColorCircle.bmpMaxlight := ComputeColorCircle(round(FColorCircle.Bounds.Width * FBitmapScale),
      round(FColorCircle.Bounds.Height * FBitmapScale), $FFFF);

  result := FColorCircle.bmpMaxlight;
end;

function TChooseColorInterface.GetColorCircleBmp: TBGRABitmap;
begin
  if (FColorCircle.bmp = nil) and Assigned(GetColorCircleMaxLightBmp) then
    FColorCircle.bmp := BitmapWithLight(GetColorCircleMaxLightBmp, max(10000, FColorLight),
                                        FCurrentColor, FColorX, FColorY);
  result := FColorCircle.bmp;
end;

constructor TChooseColorInterface.Create(AContainer: TWinControl; ADPI: integer);
begin
  Container := AContainer;
  Container.OnResize:=@ContainerResize;
  FDPI := ADPI;

  EColor := TEdit.Create(Container);
  EColor.Parent := Container;
  EColor.OnChange:= @EColorChange;
  EColor.OnKeyDown:= @EColorKeyDown;
  EColor.Name := 'EColor';
  LColor := TLabel.Create(Container);
  LColor.Parent := Container;
  LColor.OnMouseDown:= @LColorMouseDown;
  vsColorView := TBGRAVirtualScreen.Create(Container);
  vsColorView.Caption := '';
  vsColorView.Parent := Container;
  vsColorView.BitmapAutoScale:= false;
  vsColorView.OnMouseDown:= @vsColorViewMouseDown;
  vsColorView.OnMouseMove:= @vsColorViewMouseMove;
  vsColorView.OnMouseUp:= @vsColorViewMouseUp;
  vsColorView.OnRedraw:= @vsColorViewRedraw;
  FBitmapScale := 1;
  BCButton_AddToPalette := TBCButton.Create(vsColorView);
  BCButton_AddToPalette.Parent := vsColorView;
  BCButton_AddToPalette.OnClick := @BCButton_AddToPaletteClick;
  BCButton_AddToPalette.Hint := rsAddToPalette;
  BCButton_AddToPalette.ShowHint := true;
  BCButton_RemoveFromPalette := TBCButton.Create(vsColorView);
  BCButton_RemoveFromPalette.Parent := vsColorView;
  BCButton_RemoveFromPalette.OnClick := @BCButton_RemoveFromPaletteClick;
  BCButton_RemoveFromPalette.Hint := rsRemoveFromPalette;
  BCButton_RemoveFromPalette.ShowHint := true;
  ApplyTheme;

  EColor.Font.Height := FontEmHeightSign * (DoScaleY(11, OriginalDPI, FDPI) + 1);
  LColor.Font.Height := FontEmHeightSign * (DoScaleY(11, OriginalDPI, FDPI) + 1);
  EColor.Text:= '#FFFFFF';
  LColor.Visible := true;
  LColor.Caption := '#FFFFFF';
  LColor.Hint := rsColorDescription;
  FCurrentColorFormatError:= false;
  vsColorView.Left := ExternalMargin;
  vsColorView.Top := 0;

  FSelectZone := szNone;
  FInFormMouseMove := False;

  FAlphascale.bmp := nil;
  FLightscale.bmp := nil;
  FColorCircle.bmp := nil;
  FColorCircle.bmpMaxlight := nil;

  SetCurrentColor(BGRAWhite);
  FInitialized:= true;
end;

destructor TChooseColorInterface.Destroy;
begin
  FreeAndNil(FColorCircle.bmp);
  FreeAndNil(FColorCircle.bmpMaxlight);
  FreeAndNil(FLightscale.Bmp);
  FreeAndNil(FAlphascale.Bmp);
  inherited Destroy;
end;

procedure TChooseColorInterface.SetCurrentColor(value: TBGRAPixel; AFromEdit: boolean);
var newcolorlight: word;
  newcurrentcolor: TBGRAPixel;
begin
   newcolorlight := ColorLightOf(value);
   newcurrentColor := ColorWithLight(value,$FFFF);
   if (newcolorlight<>FColorLight) or not newcurrentcolor.EqualsExactly(FCurrentColor) then
   begin
     FColorLight := newcolorlight;
     FCurrentColor := newcurrentcolor;
     UpdateColorView(true, true, true);
   end;
   if not AFromEdit then HideEditor;
end;

function TChooseColorInterface.GetCurrentColor: TBGRAPixel;
begin
  result := ColorWithLight(FCurrentColor,FColorLight);
end;

procedure TChooseColorInterface.HideEditor;
begin
  if EColor.Visible then
  begin
    EColor.Hide;
    LColor.Show;
  end;
end;

function TChooseColorInterface.GetPreferredSize: TSize;
begin
  If (FLazPaintInstance = nil) or FLazPaintInstance.BlackAndWhite then
  begin
    result.cx := DoScaleY(110, OriginalDPI, FDPI);
    result.cy := DoScaleY(190, OriginalDPI, FDPI);
  end else
  begin
    result.cx := DoScaleY(224, OriginalDPI, FDPI);
    result.cy := DoScaleY(190, OriginalDPI, FDPI);
  end;
end;

procedure TChooseColorInterface.AdjustControlHeight;
var
  oneBarWidth, h, margin, topMargin, barWidth: single;
  buttonSize: Integer;
begin
  if not ((FLazPaintInstance = nil) or FLazPaintInstance.BlackAndWhite) then exit;

  margin := DoScaleYF(8, OriginalDPI, FDPI);
  topMargin := DoScaleYF(27, OriginalDPI, FDPI);
  barWidth := DoScaleXF(18, OriginalDPI, FDPI);
  buttonSize := round(barWidth);
  oneBarWidth := barWidth + DoScaleXF(10, OriginalDPI, FDPI) +
                 margin;
  if PreferredBarsAlignWithWidth = alRight then
  begin
    if (FLazPaintInstance = nil) or FLazPaintInstance.BlackAndWhite then
      h := oneBarWidth*4 + margin
    else
      h := AvailableVSWidth - oneBarWidth*2 + topMargin;
  end else
  begin
    if (FLazPaintInstance = nil) or FLazPaintInstance.BlackAndWhite then
      h := oneBarWidth*2 + buttonSize + margin
    else
      h := AvailableVSWidth + oneBarWidth*2 + topMargin;
  end;
  NeedTextAreaHeight;
  Container.Height := round(h + FTextAreaHeight + ExternalMargin);
end;

end.

