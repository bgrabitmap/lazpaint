unit uchoosecolor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, BGRABitmap, BGRABitmapTypes, LMessages, LazPaintType, uscaledpi, uresourcestrings;

type

  { TFChooseColor }

  TFChooseColor = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
  private
    { private declarations }
    bmp: TBGRABitmap;
    FormBackgroundColor, FormTextColor: TBGRAPixel;
    PreviousClientWidth: integer;

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
    procedure UpdateColorview(UpdateColorCircle, UpdateLightScale: boolean);
    procedure SetColorLight(value: word);
    function ColorWithLight(c: TBGRAPixel; light: word): TBGRAPixel;
    function BitmapWithLight(bmpsrc: TBGRABitmap; light: word; lookFor: TBGRAPixel; var pColorX,pColorY: integer): TBGRABitmap;
    function ColorLightOf(c: TBGRAPixel): word;
    procedure DrawTriangleCursor(dest: TBGRABitmap; bounds: TRect; value: byte);
    procedure DoSelect(X,Y: integer);
    procedure UpdateLayout;
  public
    { public declarations }
    colorTarget: TColorTarget;
    LazPaintInstance: TLazPaintCustomInstance;
    topmargin,barwidth,cursorplace,margin,cursormargin,cursorsize,colorXYsize,cursorxyopacity: integer;
    procedure SetCurrentColor(value: TBGRAPixel);
    function GetCurrentColor: TBGRAPixel;
  end;

implementation

uses ugraph, math;

{ TFChooseColor }

procedure TFChooseColor.FormCreate(Sender: TObject);
begin
   ScaleDPI(Self,OriginalDPI);
   topmargin := DoScaleY(27,OriginalDPI);
   barwidth := DoScaleX(20,OriginalDPI);
   cursorplace := DoScaleX(10,OriginalDPI);
   margin := DoScaleY(8,OriginalDPI);
   cursormargin := DoScaleX(2,OriginalDPI);
   cursorsize := DoScaleX(6,OriginalDPI);
   colorXYsize := DoScaleX(3,OriginalDPI);
   cursorxyopacity := DoScaleX(128,OriginalDPI);
   if cursorxyopacity > 255 then cursorxyopacity := 255;

   selectZone := szNone;
   InFormMouseMove := False;

   bmp := TBGRABitmap.Create(ClientWidth,ClientHeight);
   Alphascale.bmp := nil;
   Lightscale.bmp := nil;
   ColorCircle.bmp := nil;
   ColorCircle.bmpMaxlight := nil;

   FormBackgroundColor := ColorToBGRA(ColorToRGB(clBtnFace));
   FormTextColor := ColorToBGRA(ColorToRGB(clWindowText));
   bmp.FontHeight := topmargin-2*margin;

   If (LazPaintInstance = nil) or (LazPaintInstance.BlackAndWhite) then
     SetCurrentColor(BGRAWhite)
   else
     SetCurrentColor(BGRA(255,0,255,128));

   UpdateLayout;
end;

procedure TFChooseColor.FormDestroy(Sender: TObject);
begin
  bmp.Free;
  ColorCircle.bmp.Free;
  ColorCircle.bmpMaxlight.Free;
  Lightscale.Bmp.Free;
  Alphascale.Bmp.Free;
end;

procedure TFChooseColor.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
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

procedure TFChooseColor.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  FormMouseMovePos := Point(X,Y);
  if InFormMouseMove then Exit;
  InFormMouseMove := True;
  Application.ProcessMessages; //empty message stack
  DoSelect(FormMouseMovePos.X,FormMouseMovePos.Y);
  InFormMouseMove := False;
end;

procedure TFChooseColor.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then selectZone := szNone;
end;

procedure TFChooseColor.FormPaint(Sender: TObject);
var x,y: integer;
begin
  bmp.Fill(FormBackgroundColor);
  bmp.FontAntialias := True;
  if (colorCircle.Bmp<>nil) and not LazPaintInstance.BlackAndWhite then
  begin
    bmp.TextOut(colorCircle.center.X,margin div 2,rsColors,FormTextColor,taCenter);
    bmp.PutImage(colorCircle.bounds.Left,colorCircle.bounds.top,colorCircle.Bmp,dmDrawWithTransparency);
    x := colorCircle.bounds.Left+ColorX;
    y := colorCircle.bounds.top+ColorY;
    bmp.Rectangle(x-colorxysize-1,y-colorxysize-1,x+colorxysize+2,y+colorxysize+2,BGRA(0,0,0,cursorxyopacity),dmDrawWithTransparency);
    bmp.Rectangle(x-colorxysize,y-colorxysize,x+colorxysize+1,y+colorxysize+1,BGRA(255,255,255,cursorxyopacity),dmDrawWithTransparency);
    bmp.Rectangle(x-colorxysize+1,y-colorxysize+1,x+colorxysize,y+colorxysize,BGRA(0,0,0,cursorxyopacity),dmDrawWithTransparency);
  end;
  if Lightscale.Bmp<>nil then
  begin
    bmp.TextOut((Lightscale.bounds.Left+Lightscale.bounds.Right) div 2,margin div 2,rsLight,FormTextColor,taCenter);
    bmp.PutImage(Lightscale.bounds.Left,Lightscale.bounds.top,Lightscale.Bmp,dmFastBlend);
    bmp.Rectangle(Lightscale.bounds,BGRA(FormTextColor.red,FormTextColor.green,FormTextColor.Blue,128),dmDrawWithTransparency);
    DrawTriangleCursor(bmp, Lightscale.bounds, colorLight div 256);
  end;
  if alphascale.Bmp<>nil then
  begin
    bmp.TextOut((alphascale.bounds.Left+alphascale.bounds.Right) div 2,margin div 2,rsOpacity,FormTextColor,taCenter);
    bmp.PutImage(alphascale.bounds.Left,alphascale.bounds.top,alphascale.Bmp,dmDrawWithTransparency);
    bmp.Rectangle(alphascale.bounds,BGRA(FormTextColor.red,FormTextColor.green,FormTextColor.Blue,128),dmDrawWithTransparency);
    DrawTriangleCursor(bmp, alphascale.bounds, currentColor.alpha);
  end;
  bmp.Draw(Canvas,0,0,True);
end;

procedure TFChooseColor.FormShow(Sender: TObject);
begin
  Position := poDesigned;
  UpdateLayout;
  self.EnsureVisible(False);
end;

procedure TFChooseColor.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
   //  block Erasing background
   //  inherited EraseBackground(DC);
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
                   Repaint;
                end;
  szColorCircle: if PtInRect(point(x,y),ColorCircle.Bounds) then
                 begin
                   pix := Colorcircle.bmpMaxlight.GetPixel(integer(x-ColorCircle.Bounds.Left),integer(y-ColorCircle.Bounds.top));
                   if pix.alpha <> 0 then
                   begin
                     currentColor := BGRA(pix.Red,pix.Green,pix.Blue,currentColor.Alpha);
                     ColorX := x-ColorCircle.Bounds.Left;
                     ColorY := y-ColorCircle.Bounds.top;
                     UpdateColorview(False,True);
                     Repaint;
                   end;
                 end;
  szLightScale: begin
                   SetColorLight(max(0,min(65535,65535-round((Y-lightscale.Bounds.Top)/(lightscale.Bounds.Bottom-1-lightscale.Bounds.top)*65535))));
                   Repaint;
                end;
  end;
  LazPaintInstance.ColorFromFChooseColor;
end;

procedure TFChooseColor.UpdateLayout;
begin
  if LazPaintInstance = nil then exit;

  if LazPaintInstance.BlackAndWhite then
    ClientWidth := 2*margin+cursorplace+barwidth+margin+cursorplace+barwidth+margin
  else
    ClientWidth := clientheight-topmargin+margin + margin+cursorplace+barwidth+margin+cursorplace+barwidth+margin;

  if ClientWidth <> PreviousClientWidth then
  begin
    bmp.SetSize(ClientWidth,ClientHeight);

    Alphascale.bounds := rect(clientwidth-margin-cursorplace-barwidth,topmargin,clientwidth-margin-cursorplace,clientheight-margin);
    BGRAReplace(Alphascale.bmp,TBGRABitmap.Create(Alphascale.Bounds.Right-Alphascale.Bounds.Left,Alphascale.Bounds.Bottom-Alphascale.Bounds.Top));
    Alphascale.bmp.GradientFill(0,0,Alphascale.bmp.width,Alphascale.bmp.height,FormTextColor,BGRAPixelTransparent,gtLinear,PointF(0,-0.5),PointF(0,Alphascale.bmp.height-0.5),dmSet,True);

    Lightscale.bounds := rect(Alphascale.bounds.left-margin-cursorplace-barwidth,Alphascale.Bounds.Top,Alphascale.bounds.left-margin-cursorplace,Alphascale.Bounds.Bottom);

    ColorCircle.radius := min((Lightscale.bounds.left-2*margin-1) div 2, (clientHeight-topmargin-(margin+1)) div 2);
    ColorCircle.center := point(ColorCircle.radius+margin,topmargin+ColorCircle.radius);
    ColorCircle.bounds := rect(ColorCircle.center.x-ColorCircle.radius,
      ColorCircle.center.y-ColorCircle.radius,ColorCircle.center.x+ColorCircle.radius+1,
      ColorCircle.center.y+ColorCircle.radius+1);
    BGRAReplace(ColorCircle.bmpMaxlight,ComputeColorCircle(ColorCircle.Bounds.Right-ColorCircle.Bounds.Left,ColorCircle.Bounds.Bottom-ColorCircle.Bounds.Top, $FFFF));
    PreviousClientWidth := ClientWidth;

    UpdateColorview(true,true);
  end;
end;

procedure TFChooseColor.SetCurrentColor(value: TBGRAPixel);
begin
   colorlight := ColorLightOf(value);
   currentColor := ColorWithLight(value,$FFFF);
   UpdateColorView(true,true);
   Invalidate;
end;

function TFChooseColor.GetCurrentColor: TBGRAPixel;
begin
   result := ColorWithLight(currentColor,colorlight);
end;

procedure TFChooseColor.UpdateColorview(UpdateColorCircle, UpdateLightScale: boolean);
var tempColor: TBGRAPixel;
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
end;

procedure TFChooseColor.SetColorLight(value: word);
begin
   if colorlight <> value then
   begin
     colorlight := value;
     UpdateColorview(True,False);
     Invalidate;
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

initialization
  {$I uchoosecolor.lrs}

end.

