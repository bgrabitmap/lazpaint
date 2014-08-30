unit UChooseColor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, BGRABitmap, BGRABitmapTypes, StdCtrls, BGRAVirtualScreen,
  LazPaintType, uscaledpi, uresourcestrings;

type

  { TFChooseColor }

  TFChooseColor = class(TForm)
    vsColorView: TBGRAVirtualScreen;
    EColor: TEdit;
    LColor: TLabel;
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
    procedure SetAvailableBmpHeight(AValue: integer);
    procedure UpdateColorview(UpdateColorCircle, UpdateLightScale: boolean);
    procedure SetColorLight(value: word);
    function ColorWithLight(c: TBGRAPixel; light: word): TBGRAPixel;
    function BitmapWithLight(bmpsrc: TBGRABitmap; light: word; lookFor: TBGRAPixel; var pColorX,pColorY: integer): TBGRABitmap;
    function ColorLightOf(c: TBGRAPixel): word;
    procedure DrawTriangleCursor(dest: TBGRABitmap; bounds: TRect; value: byte);
    procedure DoSelect(X,Y: integer);
  public
    { public declarations }
    colorTarget: TColorTarget;
    LazPaintInstance: TLazPaintCustomInstance;
    topmargin,barwidth,cursorplace,margin,cursormargin,cursorsize,colorXYsize,cursorxyopacity: integer;
    procedure UpdateLayout;
    procedure SetCurrentColor(value: TBGRAPixel);
    function GetCurrentColor: TBGRAPixel;
    property AvailableBmpHeight: integer read GetAvailableBmpHeight write SetAvailableBmpHeight;
  end;

implementation

uses ugraph, math, LCLType;

{ TFChooseColor }

procedure TFChooseColor.FormCreate(Sender: TObject);
begin
   ScaleDPI(Self,OriginalDPI);
   EColor.Visible := false;
   EColor.Text:= '';
   LColor.Visible := true;
   LColor.Caption := '';
   LColor.Height := EColor.Height;
   vsColorView.Left := 0;
   vsColorView.Top := 0;
   ClientHeight := ClientHeight+EColor.Height;
   FormResize(Sender);

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

   Alphascale.bmp := nil;
   Lightscale.bmp := nil;
   ColorCircle.bmp := nil;
   ColorCircle.bmpMaxlight := nil;

   FormBackgroundColor := ColorToBGRA(ColorToRGB(clBtnFace));
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
var bmpHeight: integer;
begin
  bmpHeight := AvailableBmpHeight;
  EColor.Top := bmpHeight;
  EColor.Left := 0;
  EColor.Width := ClientWidth;
  LColor.Top := bmpHeight;
  LColor.Left := 0;
  LColor.Width := ClientWidth;
  vsColorView.Width := ClientWidth;
  vsColorView.Height := bmpHeight;
end;

procedure TFChooseColor.FormShow(Sender: TObject);
begin
  Position := poDesigned;
  UpdateLayout;
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
  EColor.SetFocus;
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

procedure TFChooseColor.UpdateLayout;
var bmpWidth,bmpHeight: integer;
begin
  if LazPaintInstance = nil then exit;

  if LazPaintInstance.BlackAndWhite then
    ClientWidth := 2*margin+cursorplace+barwidth+margin+cursorplace+barwidth+margin
  else
    ClientWidth := AvailableBmpHeight-topmargin+margin + margin+cursorplace+barwidth+margin+cursorplace+barwidth+margin;

  if ClientWidth <> PreviousClientWidth then
  begin
    PreviousClientWidth := ClientWidth;
    bmpWidth := ClientWidth;
    bmpHeight := AvailableBmpHeight;

    Alphascale.bounds := rect(bmpWidth-margin-cursorplace-barwidth,topmargin,bmpWidth-margin-cursorplace,bmpHeight-margin);
    BGRAReplace(Alphascale.bmp,TBGRABitmap.Create(Alphascale.Bounds.Right-Alphascale.Bounds.Left,Alphascale.Bounds.Bottom-Alphascale.Bounds.Top));
    Alphascale.bmp.GradientFill(0,0,Alphascale.bmp.width,Alphascale.bmp.height,FormTextColor,BGRAPixelTransparent,gtLinear,PointF(0,-0.5),PointF(0,Alphascale.bmp.height-0.5),dmSet,True);

    Lightscale.bounds := rect(Alphascale.bounds.left-margin-cursorplace-barwidth,Alphascale.Bounds.Top,Alphascale.bounds.left-margin-cursorplace,Alphascale.Bounds.Bottom);

    ColorCircle.radius := min((Lightscale.bounds.left-2*margin-1) div 2, (bmpHeight-topmargin-(margin+1)) div 2);
    ColorCircle.center := point(ColorCircle.radius+margin,topmargin+ColorCircle.radius);
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

   LColor.Caption := ' '+strColor;
   vsColorView.RedrawBitmap;
end;

function TFChooseColor.GetAvailableBmpHeight: integer;
begin
  result := ClientHeight-EColor.Height;
end;

procedure TFChooseColor.SetAvailableBmpHeight(AValue: integer);
begin
  ClientHeight := AValue+EColor.Height;
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

