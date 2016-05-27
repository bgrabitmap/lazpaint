unit UChooseColor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, BGRABitmap, BGRABitmapTypes, StdCtrls, Buttons,
  BGRAVirtualScreen, BCButton, LazPaintType, uscaledpi, uresourcestrings;

const externalMargin = 3;

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
    { private declarations }
    FormBackgroundColor, FormTextColor: TBGRAPixel;
    PreviousClientWidth: integer;
    ColorBeforeEColor: TBGRAPixel;

    InFormMouseMove: Boolean;
    FormMouseMovePos: TPoint;

    currentColor: TBGRAPixel;
    colorLight: word;
    ColorX,colorY: integer;
    selectZone: (szNone, szColorCircle, szLightScale, szAlphascale);
    ColorCircle: record center: TPoint;
                        radius: integer;
                        bounds: TRect;
                        bmp,bmpMaxlight: TBGRABitmap; end;
    Lightscale: record bounds: TRect;
                      bmp: TBGRABitmap; end;
    Alphascale: record bounds: TRect;
                       bmp: TBGRABitmap; end;
    function GetAvailableBmpHeight: integer;
    function GetAvailableBmpWidth: integer;
    procedure SetAvailableBmpHeight(AValue: integer);
    procedure UpdateColorview(UpdateColorCircle, UpdateLightScale: boolean);
    procedure SetColorLight(value: word);
    function ColorWithLight(c: TBGRAPixel; light: word): TBGRAPixel;
    function BitmapWithLight(bmpsrc: TBGRABitmap; light: word; lookFor: TBGRAPixel; var pColorX,pColorY: integer): TBGRABitmap;
    function ColorLightOf(c: TBGRAPixel): word;
    procedure DrawTriangleCursor(dest: TBGRABitmap; bounds: TRect; value: byte);
    procedure DoSelect(X,Y: integer);
    function MakeIconBase(size: integer): TBitmap;
    function MakeAddIcon(size: integer): TBitmap;
    function MakeRemoveIcon(size: integer): TBitmap;
  public
    { public declarations }
    colorTarget: TColorTarget;
    LazPaintInstance: TLazPaintCustomInstance;
    topmargin,barwidth,cursorplace,margin,cursormargin,cursorsize,colorXYsize,cursorxyopacity: integer;
    procedure UpdateLayout;
    procedure SetCurrentColor(value: TBGRAPixel);
    function GetCurrentColor: TBGRAPixel;
    property AvailableBmpHeight: integer read GetAvailableBmpHeight write SetAvailableBmpHeight;
    property AvailableBmpWidth: integer read GetAvailableBmpWidth;
  end;

var TFChooseColor_CustomDPI: integer = 0;

implementation

uses ugraph, math, LCLType, BGRAText;

{ TFChooseColor }

procedure TFChooseColor.FormCreate(Sender: TObject);
begin
   {$IFDEF LINUX}
   BorderStyle:= bsDialog;
   {$ENDIF}
   ClientHeight := DoScaleY(160,OriginalDPI,TFChooseColor_CustomDPI);
   ScaleDPI(Self,OriginalDPI,TFChooseColor_CustomDPI);
   EColor.Font.Height := FontFullHeightSign*DoScaleY(14,OriginalDPI,TFChooseColor_CustomDPI);
   LColor.Font.Height := FontFullHeightSign*DoScaleY(14,OriginalDPI,TFChooseColor_CustomDPI);
   EColor.AdjustSize;
   EColor.Text:= '';
   EColor.Top := ClientHeight;
   LColor.Visible := true;
   LColor.Caption := '';
   LColor.Height := BGRATextSize(LColor.Font, fqSystemClearType, 'Hg', 1).cy+2;// DoScaleY(14,OriginalDPI,TFChooseColor_CustomDPI)+2;
   LColor.Top := ClientHeight;
   vsColorView.Left := externalMargin;
   vsColorView.Top := 0;
   ClientHeight := ClientHeight+EColor.Height;
   BCAssignSystemStyle(BCButton_AddToPalette);
   BCAssignSystemStyle(BCButton_RemoveFromPalette);
   FormResize(Sender);
   EColor.Visible := false;

   topmargin := DoScaleY(27,OriginalDPI,TFChooseColor_CustomDPI);
   barwidth := DoScaleX(20,OriginalDPI,TFChooseColor_CustomDPI);
   cursorplace := DoScaleX(10,OriginalDPI,TFChooseColor_CustomDPI);
   margin := DoScaleY(8,OriginalDPI,TFChooseColor_CustomDPI);
   cursormargin := DoScaleX(2,OriginalDPI,TFChooseColor_CustomDPI);
   cursorsize := DoScaleX(6,OriginalDPI,TFChooseColor_CustomDPI);
   colorXYsize := DoScaleX(3,OriginalDPI,TFChooseColor_CustomDPI);
   cursorxyopacity := DoScaleX(128,OriginalDPI,TFChooseColor_CustomDPI);
   if cursorxyopacity > 255 then cursorxyopacity := 255;

   selectZone := szNone;
   InFormMouseMove := False;

   Alphascale.bmp := nil;
   Lightscale.bmp := nil;
   ColorCircle.bmp := nil;
   ColorCircle.bmpMaxlight := nil;

   FormBackgroundColor := ColorToBGRA(ColorToRGB({$IFDEF DARWIN}clWindow{$ELSE}clBtnFace{$ENDIF}));
   FormTextColor := ColorToBGRA(ColorToRGB(clWindowText));

   If (LazPaintInstance = nil) or (LazPaintInstance.BlackAndWhite) then
     SetCurrentColor(BGRAWhite)
   else
     SetCurrentColor(BGRA(255,0,255,128));

   UpdateLayout;
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
   if (LazPaintInstance <> nil) and EColor.Visible then
   begin
     if LazPaintInstance.BlackAndWhite then
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
       LazPaintInstance.ColorFromFChooseColor;
     end;
   end;
end;

procedure TFChooseColor.BCButton_AddToPaletteClick(Sender: TObject);
begin
  LazPaintInstance.AddColorToPalette(GetCurrentColor);
end;

procedure TFChooseColor.BCButton_RemoveFromPaletteClick(Sender: TObject);
begin
  LazPaintInstance.RemoveColorFromPalette(GetCurrentColor);
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
    LazPaintInstance.ColorFromFChooseColor;
  end;
end;

procedure TFChooseColor.FormDestroy(Sender: TObject);
begin
  ColorCircle.bmp.Free;
  ColorCircle.bmpMaxlight.Free;
  Lightscale.Bmp.Free;
  Alphascale.Bmp.Free;
end;

procedure TFChooseColor.FormResize(Sender: TObject);
begin
  vsColorView.Width := AvailableBmpWidth;
  vsColorView.Height := AvailableBmpHeight;
  EColor.Left := externalMargin;
  EColor.Width := ClientWidth-2*externalMargin;
  LColor.Left := externalMargin;
  LColor.Width := ClientWidth-2*externalMargin;
  EColor.Top := ClientHeight-EColor.Height;
  LColor.Top := ClientHeight-LColor.Height;
end;

procedure TFChooseColor.FormShow(Sender: TObject);
begin
  Position := poDesigned;
  UpdateLayout;
  LColor.Top := ClientHeight-LColor.Height;
  self.EnsureVisible(False);
end;

procedure TFChooseColor.LColorMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  LColor.Visible := false;
  ColorBeforeEColor := GetCurrentColor;
  if LazPaintInstance.BlackAndWhite then
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
    if PtInRect(Point(X,Y),colorcircle.Bounds) then
    begin
      SelectZone := szColorCircle;
      DoSelect(X,Y);
    end else
    if PtInRect(Point(X,Y),Alphascale.Bounds) then
    begin
      SelectZone := szAlphascale;
      DoSelect(X,Y);
    end else
    if PtInRect(Point(X,Y),lightscale.Bounds) then
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
  Bitmap.FontHeight := topmargin-2*margin;

  Bitmap.Fill(FormBackgroundColor);
  Bitmap.FontAntialias := True;
  boundRight := Bitmap.Width;
  if alphascale.Bmp<>nil then
  begin
    w := Bitmap.TextSize(rsOpacity).cx;
    x := (alphascale.bounds.Left+alphascale.bounds.Right-w) div 2;
    if x + w > boundRight then
      x := boundRight-w;
    Bitmap.TextOut(x,margin div 2,rsOpacity,FormTextColor,taLeftJustify);
    boundRight := x-4;
    Bitmap.PutImage(alphascale.bounds.Left,alphascale.bounds.top,alphascale.Bmp,dmDrawWithTransparency);
    Bitmap.Rectangle(alphascale.bounds,BGRA(FormTextColor.red,FormTextColor.green,FormTextColor.Blue,128),dmDrawWithTransparency);
    DrawTriangleCursor(Bitmap, alphascale.bounds, currentColor.alpha);
  end;
  if Lightscale.Bmp<>nil then
  begin
    w := Bitmap.TextSize(rsLight).cx;
    x := (Lightscale.bounds.Left+Lightscale.bounds.Right-w) div 2;
    if x+ w > boundRight then
      x:= boundRight-w;
    Bitmap.TextOut(x,margin div 2,rsLight,FormTextColor,taLeftJustify);
    boundRight := x-4;
    Bitmap.PutImage(Lightscale.bounds.Left,Lightscale.bounds.top,Lightscale.Bmp,dmFastBlend);
    Bitmap.Rectangle(Lightscale.bounds,BGRA(FormTextColor.red,FormTextColor.green,FormTextColor.Blue,128),dmDrawWithTransparency);
    DrawTriangleCursor(Bitmap, Lightscale.bounds, colorLight div 256);
  end;
  if (colorCircle.Bmp<>nil) and not LazPaintInstance.BlackAndWhite then
  begin
    w := Bitmap.TextSize(rsColors).cx;
    x := colorCircle.center.X-(w div 2);
    if x + w > boundRight then
      x := boundRight-w;
    Bitmap.TextOut(x,margin div 2,rsColors,FormTextColor,taLeftJustify);
    Bitmap.PutImage(colorCircle.bounds.Left,colorCircle.bounds.top,colorCircle.Bmp,dmDrawWithTransparency);
    x := colorCircle.bounds.Left+ColorX;
    y := colorCircle.bounds.top+ColorY;
    Bitmap.Rectangle(x-colorxysize-1,y-colorxysize-1,x+colorxysize+2,y+colorxysize+2,BGRA(0,0,0,cursorxyopacity),dmDrawWithTransparency);
    Bitmap.Rectangle(x-colorxysize,y-colorxysize,x+colorxysize+1,y+colorxysize+1,BGRA(255,255,255,cursorxyopacity),dmDrawWithTransparency);
    Bitmap.Rectangle(x-colorxysize+1,y-colorxysize+1,x+colorxysize,y+colorxysize,BGRA(0,0,0,cursorxyopacity),dmDrawWithTransparency);
    size := (ColorCircle.bounds.Bottom-ColorCircle.bounds.Top)/10;
    c := GetCurrentColor;
    c.alpha := 255;
    Bitmap.RoundRectAntialias(0,colorCircle.bounds.Bottom-1-size,size,colorCircle.bounds.Bottom-1,
        size/6,size/6, BGRA(0,0,0,192),1,c,[]);
  end;
end;

procedure TFChooseColor.DrawTriangleCursor(dest: TBGRABitmap; bounds: TRect;
  value: byte);
var x,y: integer;
begin
     x := bounds.right+cursormargin;
     y := bounds.bottom-1 + integer(round(value/255*(bounds.top-(bounds.bottom-1))));
     dest.FillPolyAntialias([pointF(x,y),pointF(x+cursorsize,y-cursorsize),pointF(x+cursorsize,y+cursorsize)],FormTextColor);
end;

procedure TFChooseColor.DoSelect(X, Y: integer);
var pix: TBGRAPixel;
begin
  case selectZone of
  szAlphascale: begin
                   currentColor.alpha := max(0,min(255,255-round((Y-Alphascale.Bounds.Top)/(Alphascale.Bounds.Bottom-1-alphascale.Bounds.top)*255)));
                   UpdateColorview(False,False);
                end;
  szColorCircle: if PtInRect(point(x,y),ColorCircle.Bounds) then
                 begin
                   pix := Colorcircle.bmpMaxlight.GetPixel(x-ColorCircle.Bounds.Left,y-ColorCircle.Bounds.top);
                   if pix.alpha <> 0 then
                   begin
                     currentColor := BGRA(pix.Red,pix.Green,pix.Blue,currentColor.Alpha);
                     ColorX := x-ColorCircle.Bounds.Left;
                     ColorY := y-ColorCircle.Bounds.top;
                     UpdateColorview(False,True);
                   end;
                 end;
  szLightScale: begin
                   SetColorLight(max(0,min(65535,65535-round((Y-lightscale.Bounds.Top)/(lightscale.Bounds.Bottom-1-lightscale.Bounds.top)*65535))));
                end;
  end;
  LazPaintInstance.ColorFromFChooseColor;
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

procedure TFChooseColor.UpdateLayout;
var tmpIcon: TBitmap; bmpWidth,bmpHeight,internalMargin,deltaY, iconSize: integer;
begin
  if LazPaintInstance = nil then exit;

  internalMargin := margin-externalMargin;
  if internalMargin < 0 then internalMargin := 0;

  BCButton_AddToPalette.Width := barwidth;
  BCButton_AddToPalette.Height := barwidth;
  BCButton_RemoveFromPalette.Width := barwidth;
  BCButton_RemoveFromPalette.Height := barwidth;
  BCButton_RemoveFromPalette.Top := BCButton_AddToPalette.Top+BCButton_AddToPalette.Height;
  iconSize := barwidth-4;
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
  if LazPaintInstance.BlackAndWhite then
  begin
    ClientWidth := BCButton_AddToPalette.Width + 2*(externalMargin+internalMargin)+cursorplace+barwidth+margin+cursorplace+barwidth+margin;
    deltaY := (ClientHeight-BCButton_AddToPalette.Height-BCButton_RemoveFromPalette.Height) div 2 - BCButton_AddToPalette.Top;
    BCButton_AddToPalette.Top := BCButton_AddToPalette.Top + deltaY;
    BCButton_RemoveFromPalette.Top := BCButton_RemoveFromPalette.Top + deltaY;
  end
  else
    ClientWidth := AvailableBmpHeight-topmargin+ 2*(externalMargin+internalMargin) +cursorplace+barwidth+margin+cursorplace+barwidth+margin;

  if ClientWidth <> PreviousClientWidth then
  begin
    PreviousClientWidth := ClientWidth;
    bmpWidth := AvailableBmpWidth;
    bmpHeight := AvailableBmpHeight;

    Alphascale.bounds := rect(bmpWidth-margin-cursorplace-barwidth,topmargin,bmpWidth-margin-cursorplace,bmpHeight-margin);
    BGRAReplace(Alphascale.bmp,TBGRABitmap.Create(Alphascale.Bounds.Right-Alphascale.Bounds.Left,Alphascale.Bounds.Bottom-Alphascale.Bounds.Top));
    Alphascale.bmp.GradientFill(0,0,Alphascale.bmp.width,Alphascale.bmp.height,FormTextColor,BGRAPixelTransparent,gtLinear,PointF(0,-0.5),PointF(0,Alphascale.bmp.height-0.5),dmSet,True);

    Lightscale.bounds := rect(Alphascale.bounds.left-margin-cursorplace-barwidth,Alphascale.Bounds.Top,Alphascale.bounds.left-margin-cursorplace,Alphascale.Bounds.Bottom);

    ColorCircle.radius := min((Lightscale.bounds.left-margin-internalMargin) div 2, (bmpHeight-topmargin-margin) div 2);
    ColorCircle.center := point(ColorCircle.radius+internalMargin,topmargin+ColorCircle.radius-1);
    ColorCircle.bounds := rect(ColorCircle.center.x-ColorCircle.radius,
      ColorCircle.center.y-ColorCircle.radius,ColorCircle.center.x+ColorCircle.radius+1,
      ColorCircle.center.y+ColorCircle.radius+1);
    BGRAReplace(ColorCircle.bmpMaxlight,ComputeColorCircle(ColorCircle.Bounds.Right-ColorCircle.Bounds.Left,ColorCircle.Bounds.Bottom-ColorCircle.Bounds.Top, $FFFF));

    UpdateColorview(true,true);
  end;
end;

procedure TFChooseColor.SetCurrentColor(value: TBGRAPixel);
var newcolorlight: word;
  newcurrentcolor: TBGRAPixel;
begin
   newcolorlight := ColorLightOf(value);
   newcurrentColor := ColorWithLight(value,$FFFF);
   if (newcolorlight<>colorlight) or (newcurrentcolor <> currentColor) then
   begin
     colorLight := newcolorlight;
     currentColor := newcurrentcolor;
     UpdateColorView(true,true);
   end;
end;

function TFChooseColor.GetCurrentColor: TBGRAPixel;
begin
   result := ColorWithLight(currentColor,colorlight);
end;

procedure TFChooseColor.UpdateColorview(UpdateColorCircle, UpdateLightScale: boolean);
var tempColor: TBGRAPixel;
  idxCSS: Integer;
  strColor:string;
begin
   if UpdateLightScale and (Lightscale.Bounds.Right > Lightscale.Bounds.Left) then
   begin
     if Lightscale.bmp <> nil then FreeAndNil(Lightscale.bmp);
     Lightscale.bmp := TBGRABitmap.Create(Lightscale.Bounds.Right-Lightscale.Bounds.Left,Lightscale.Bounds.Bottom-Lightscale.Bounds.Top);
     tempColor := ColorWithLight(currentColor,$FFFF);
     tempColor.alpha := 255;
     Lightscale.bmp.GradientFill(0,0,Lightscale.bmp.width,Lightscale.bmp.height,tempColor,BGRABlack,gtLinear,PointF(0,-0.5),PointF(0,Lightscale.bmp.height-0.5),dmSet,True);
   end;

   if UpdateColorCircle and (ColorCircle.bmpMaxLight <> nil) then
   begin
     if ColorCircle.bmp <> nil then FreeAndNil(ColorCircle.bmp);
     ColorCircle.bmp := BitmapWithLight(ColorCircle.bmpMaxlight, max(10000, ColorLight), currentColor,ColorX,ColorY);
   end;

   tempColor := GetCurrentColor;
   if (LazPaintInstance = nil) or LazPaintInstance.BlackAndWhite then
     strColor := inttostr(tempColor.green) + ', a:' + FloatToStr(round(tempColor.alpha/255*100)/100)
   else
     begin
       strColor := '#'+copy(BGRAToStr(tempColor),1,6);
       idxCSS:= CSSColors.IndexOfColor(BGRA(tempColor.red,tempColor.green,tempColor.blue),500);
       if idxCSS <> -1 then strColor:= strColor + ', ' + CSSColors.Name[idxCSS] + ', a:' + FloatToStr(round(tempColor.alpha/255*100)/100) else
         strColor := strColor+ ', rgba(' + IntToStr(tempColor.red) + ',' + IntToStr(tempColor.green) + ',' +
           IntToStr(tempColor.blue) + ',' + FloatToStr(round(tempColor.alpha/255*100)/100) + ')';
     end;

   LColor.Caption := strColor;
   vsColorView.RedrawBitmap;
end;

function TFChooseColor.GetAvailableBmpHeight: integer;
begin
  result := ClientHeight-max(EColor.Height, LColor.Height)-externalMargin;
end;

function TFChooseColor.GetAvailableBmpWidth: integer;
begin
  result := ClientWidth-externalMargin*2;
end;

procedure TFChooseColor.SetAvailableBmpHeight(AValue: integer);
begin
  ClientHeight := AValue+EColor.Height+externalMargin;
end;

procedure TFChooseColor.SetColorLight(value: word);
begin
   if colorlight <> value then
   begin
     colorlight := value;
     UpdateColorview(True,False);
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

