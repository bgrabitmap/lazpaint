unit UBarUpDown;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRAVirtualScreen, Forms, Controls, BGRABitmap, ExtCtrls, LCLType;

const
  LongTimeInterval = 400;
  SmallTimeInterval = 100;

type

  { TBarUpDown }

  TBarUpDown = class
    constructor Create(AVirtualScreen: TBGRAVirtualScreen; AMinValue,AMaxValue,AValue: integer);
    destructor Destroy; override;
  protected
    FTimer: TTimer;
    procedure Timer(Sender: TObject);
    procedure Enter(Sender: TObject);
    procedure Leave(Sender: TObject);
    procedure KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure MouseDown(Sender: TObject;
      Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure MouseMove(Sender: TObject; {%H-}Shift: TShiftState;
      X, {%H-}Y: Integer);
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure Redraw(Sender: TObject; Bitmap: TBGRABitmap);
  private
    FBarWidth,FBarHeight,FUpDownWidth,FUpDownLeft,FTextLeft: integer;
    FOnExit: TNotifyEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnChange: TNotifyEvent;
    FIncrement: integer;
    FVirtualScreen: TBGRAVirtualScreen;
    FMinValue,FMaxValue,FValue: integer;
    FBarClick,FUpClick,FDownClick: boolean;
    FSelStart,FSelLength: integer;
    FEmptyText: boolean;
    FBarExponent: single;
    function GetEnabled: boolean;
    function GetFocused: boolean;
    function GetHeight: integer;
    function GetLeft: integer;
    function GetText: string;
    function GetTop: integer;
    function GetVisible: boolean;
    function GetWidth: integer;
    procedure SetBarExponent(AValue: single);
    procedure SetEnabled(AValue: boolean);
    procedure SetIncrement(AValue: integer);
    procedure SetMaxValue(AValue: integer);
    procedure SetMinValue(AValue: integer);
    procedure SetText(AValue: string);
    procedure SetValue(AValue: integer);
    procedure SetVisible(AValue: boolean);
    function ValueToBarPos(AValue: integer): integer;
    function BarPosToValue(ABarPos: integer): integer;
  public
    procedure SelectAll;
    procedure SetFocus;
    procedure DelayTimer;
    function RemoveSelection: boolean;
    property Value: integer read FValue write SetValue;
    property MinValue: integer read FMinValue write SetMinValue;
    property MaxValue: integer read FMaxValue write SetMaxValue;
    property Increment: integer read FIncrement write SetIncrement;
    property Text: string read GetText write SetText;
    property SelStart: integer read FSelStart;
    property SelLength: integer read FSelLength;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnExit: TNotifyEvent read FOnExit write FOnExit;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property BarExponent: single read FBarExponent write SetBarExponent;
    property Focused: boolean read GetFocused;
    property Enabled: boolean read GetEnabled write SetEnabled;
    property Left: integer read GetLeft;
    property Width: integer read GetWidth;
    property Top: integer read GetTop;
    property Height: integer read GetHeight;
    property Visible: boolean read GetVisible write SetVisible;
  end;

implementation

uses BGRABitmapTypes, Graphics, Types, Math, LazPaintType;

{ TBarUpDown }

constructor TBarUpDown.Create(AVirtualScreen: TBGRAVirtualScreen; AMinValue,
  AMaxValue, AValue: integer);
begin
  FVirtualScreen := AVirtualScreen;
  FVirtualScreen.TabStop:= true;
  AVirtualScreen.OnRedraw := @Redraw;
  AVirtualScreen.OnMouseDown := @MouseDown;
  AVirtualScreen.OnMouseMove := @MouseMove;
  AVirtualScreen.OnMouseUp := @MouseUp;
  AVirtualScreen.OnEnter := @Enter;
  AVirtualScreen.OnExit := @Leave;
  AVirtualScreen.OnUTF8KeyPress:= @KeyPress;
  FMinValue:= AMinValue;
  FMaxValue := AMaxValue;
  FValue := AValue;
  FIncrement := 1;
  FBarExponent:= 1;
  FTimer := TTimer.Create(AVirtualScreen);
  FTimer.Enabled := false;
  FTimer.OnTimer := @Timer;
  SelectAll;
end;

destructor TBarUpDown.Destroy;
begin
  FVirtualScreen.OnRedraw := nil;
  FVirtualScreen.OnMouseDown := nil;
  FVirtualScreen.OnMouseMove := nil;
  FVirtualScreen.OnMouseUp := nil;
  FVirtualScreen.OnEnter := nil;
  FVirtualScreen.OnExit := nil;
  FVirtualScreen.OnUTF8KeyPress:= nil;
  FreeAndNil(FTimer);
  inherited Destroy;
end;

procedure TBarUpDown.Timer(Sender: TObject);
begin
  if FUpClick then
  begin
    Value := Value + Increment;
  end else
  if FDownClick then
    Value := Value - Increment;
  FTimer.Interval := SmallTimeInterval;
end;

procedure TBarUpDown.Enter(Sender: TObject);
begin
  FVirtualScreen.DiscardBitmap;
end;

procedure TBarUpDown.Leave(Sender: TObject);
begin
  FEmptyText:= false;
  FVirtualScreen.DiscardBitmap;
  if Assigned(FOnExit) then FOnExit(Sender);
end;

procedure TBarUpDown.KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
var tempText: string;
begin
  if UTF8Key = #8 then
  begin
    if not RemoveSelection and (SelStart > 0) then
    begin
      tempText := Text;
      Dec(FSelStart);
      Delete(tempText,SelStart+1,1);
      Text := tempText;
      FVirtualScreen.DiscardBitmap;
    end;
    UTF8Key:= #0;
  end else
  if (length(UTF8Key)=1) and (UTF8Key[1] in['0'..'9']) then
  begin
    RemoveSelection;
    tempText := Text;
    Insert(UTF8Key,tempText,SelStart+1);
    Text := tempText;
    if FSelStart < length(Text) then inc(FSelStart);
    FVirtualScreen.DiscardBitmap;
    UTF8Key:= #0;
  end;
end;

procedure TBarUpDown.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  vs: TBGRAVirtualScreen;
begin
  if Button = mbLeft then
  begin
    vs := Sender as TBGRAVirtualScreen;
    if X >= FUpDownLeft then
    begin
      if Y > vs.Height div 2 then
      begin
        FDownClick:= true;
        Value := Value-Increment;
        vs.DiscardBitmap;
        FTimer.Interval := LongTimeInterval;
        FTimer.Enabled:= true;
      end else
      if Y < vs.Height div 2 then
      begin
        FUpClick:= true;
        Value := Value+Increment;
        vs.DiscardBitmap;
        FTimer.Interval := LongTimeInterval;
        FTimer.Enabled:= true;
      end;
    end else
    if (Y >= vs.Height-FBarHeight-1) and (FBarWidth>1) then
    begin
      FBarClick:= true;
      Value := BarPosToValue(X);
      vs.RedrawBitmap;
    end else
    if vs.Focused then
    begin

    end;
  end;
  if not vs.Focused then
  begin
    SafeSetFocus(vs);
    SelectAll;
  end;
end;

procedure TBarUpDown.MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  vs: TBGRAVirtualScreen;
begin
  vs := Sender as TBGRAVirtualScreen;
  if FBarClick and (FBarWidth>1) then
  begin
    vs := Sender as TBGRAVirtualScreen;
    Value := BarPosToValue(X);
    vs.RedrawBitmap;
  end;
  if Assigned(FOnMouseMove) then FOnMouseMove(Sender,Shift,X,Y);
end;

procedure TBarUpDown.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    if FBarClick then FBarClick:= false else
    if FUpClick then
    begin
      FUpClick:= false;
      FVirtualScreen.RedrawBitmap;
      FTimer.Enabled:= false;
    end else
    if FDownClick then
    begin
      FDownClick:= false;
      FVirtualScreen.RedrawBitmap;
      FTimer.Enabled:= false;
    end;
  end;
end;

procedure TBarUpDown.Redraw(Sender: TObject; Bitmap: TBGRABitmap);
var bordercolor,bgcolor,fgcolor,btncolor,btnshadow,btntext,c: TBGRAPixel;
  x,ty,barx: integer;
  s: TSize;
  midy: integer;
  midx: single;
  beforeSel,inSel,afterSel: string;
begin
  bgcolor := ColorToBGRA(ColorToRGB(clWindow));
  fgcolor := ColorToBGRA(ColorToRGB(clWindowText));
  btncolor := ColorToBGRA(ColorToRGB(clBtnFace));
  btntext := ColorToBGRA(ColorToRGB(clBtnText));
  btnshadow := ColorToBGRA(ColorToRGB(clBtnShadow));
  bordercolor := MergeBGRA(fgcolor,btncolor);
  Bitmap.Rectangle(0,0,Bitmap.Width,bitmap.Height,bordercolor,bgcolor,dmSet);
  ty := Bitmap.Height-2;
  FTextLeft := 1+((ty+5) div 10);
  FBarHeight := (bitmap.Height+3) div 5+1;
  FUpDownWidth := (ty*3+3) div 5;
  FUpDownLeft := Bitmap.Width-FUpDownWidth;
  FBarWidth := Bitmap.Width-1-FUpDownWidth-FBarHeight+1;
  Bitmap.FontHeight := ((ty-FBarHeight+1)*8+4) div 9;
  Bitmap.FontQuality := fqFineAntialiasing;

  x := FTextLeft;
  if FVirtualScreen.Focused then
  begin
    if SelStart = 0 then
    begin
      beforeSel := '';
      inSel := Text;
    end else
    begin
      beforeSel := copy(Text,1,SelStart);
      inSel := copy(Text,SelStart+1,length(Text)-SelStart);
    end;
    if length(inSel)>SelLength then
    begin
      afterSel:= copy(inSel,SelLength+1,length(inSel)-SelLength);
      inSel := copy(inSel,1,SelLength);
    end else
      afterSel := '';
    Bitmap.TextOut(x,1,beforeSel,fgcolor);
    inc(x, Bitmap.TextSize(beforeSel).cx);
    if inSel = '' then Bitmap.SetVertLine(x,1,1+Bitmap.FontFullHeight-1,fgcolor)
    else
    begin
      s := Bitmap.TextSize(inSel);
      Bitmap.FillRect(x,1+1,x+s.cx,1+s.cy,ColorToBGRA(ColorToRGB(clHighlight)),dmSet);
      Bitmap.TextOut(x,1,inSel,ColorToBGRA(ColorToRGB(clHighlightText)));
      inc(x,s.cx);
    end;
    Bitmap.TextOut(x,1,afterSel,fgcolor);
  end else
    Bitmap.TextOut(x,1,Text,fgcolor);

  if not Enabled then
  begin
    bgcolor.alpha := 128;
    Bitmap.Rectangle(0,0,Bitmap.Width,bitmap.Height,bordercolor,bgcolor,dmDrawWithTransparency);
  end;

  barx := ValueToBarPos(Value);
  Bitmap.FillPolyAntialias([PointF(barx,Bitmap.Height-FBarHeight),PointF(barx+FBarHeight,Bitmap.Height),
  PointF(barx-FBarHeight,Bitmap.Height)],fgcolor);
  if FUpClick then c := btnshadow else c := btncolor;
  Bitmap.Rectangle(FUpDownLeft,0,Bitmap.Width,Bitmap.Height div 2+1,bordercolor,c,dmSet);
  if FDownClick then c := btnshadow else c := btncolor;
  Bitmap.Rectangle(FUpDownLeft,Bitmap.Height div 2,Bitmap.Width,Bitmap.Height,bordercolor,c,dmSet);
  midy := Bitmap.Height div 2;
  Bitmap.SetHorizLine(FUpDownLeft,midy,Bitmap.Width-1,bordercolor);
  midx := FUpDownLeft+(FUpDownWidth-1)/2;
  Bitmap.FillPolyAntialias([PointF(FUpDownLeft+2,midy*4/5),PointF(midx,midy/5),PointF(FUpDownLeft+FUpDownWidth-3,midy*4/5)],btntext);
  Bitmap.FillPolyAntialias([PointF(FUpDownLeft+2,midy*6/5),PointF(midx,Bitmap.Height-midy/5),PointF(FUpDownLeft+FUpDownWidth-3,midy*6/5)],btntext);
end;

procedure TBarUpDown.SetMaxValue(AValue: integer);
begin
  if FMaxValue=AValue then Exit;
  FMaxValue:=AValue;
  if AValue < FMinValue then FMinValue:= AValue;
  FVirtualScreen.DiscardBitmap;
end;

procedure TBarUpDown.SetIncrement(AValue: integer);
begin
  if FIncrement=AValue then Exit;
  FIncrement:=AValue;
end;

function TBarUpDown.GetText: string;
begin
  if FEmptyText then result := '' else
    result := IntToStr(Value);
end;

function TBarUpDown.GetTop: integer;
begin
  result := FVirtualScreen.Top;
end;

function TBarUpDown.GetVisible: boolean;
begin
  result := FVirtualScreen.Visible;
end;

function TBarUpDown.GetWidth: integer;
begin
  result := FVirtualScreen.Width;
end;

function TBarUpDown.GetFocused: boolean;
begin
  result := FVirtualScreen.Focused;
end;

function TBarUpDown.GetHeight: integer;
begin
  result := FVirtualScreen.Height;
end;

function TBarUpDown.GetLeft: integer;
begin
  result := FVirtualScreen.Left;
end;

function TBarUpDown.GetEnabled: boolean;
begin
  result := FVirtualScreen.Enabled;
end;

procedure TBarUpDown.SetBarExponent(AValue: single);
begin
  if AValue <= 0 then exit;
  if FBarExponent=AValue then Exit;
  FBarExponent:=AValue;
  FVirtualScreen.DiscardBitmap;
end;

procedure TBarUpDown.SetEnabled(AValue: boolean);
begin
  FVirtualScreen.Enabled := AValue;
end;

procedure TBarUpDown.SetMinValue(AValue: integer);
begin
  if FMinValue=AValue then Exit;
  FMinValue:=AValue;
  if AValue > FMaxValue then FMaxValue:= AValue;
  FVirtualScreen.DiscardBitmap;
end;

procedure TBarUpDown.SetText(AValue: string);
var errPos,tempValue: integer;
  txt: string;
begin
  if trim(AValue) = '' then
  begin
    FEmptyText:= true;
    FVirtualScreen.DiscardBitmap;
    exit;
  end;
  val(AValue,tempValue,errPos);
  if errPos = 0 then
  begin
    if tempValue < FMinValue then tempValue := FMinValue;
    if tempValue > FMaxValue then tempValue := FMaxValue;
    if (FValue = tempValue) and not FEmptyText then exit;
    FValue := tempValue;
    FEmptyText:= false;
  end;
  txt := Text;
  if FSelStart > length(txt) then FSelStart := length(txt);
  if FSelStart+FSelLength > length(txt) then FSelLength:= length(txt)-FSelStart;
  FVirtualScreen.RedrawBitmap;
  if Assigned(FOnChange) then FOnChange(self);
end;

procedure TBarUpDown.SetValue(AValue: integer);
begin
  if AValue < FMinValue then AValue := FMinValue;
  if AValue > FMaxValue then AValue := FMaxValue;
  if FValue=AValue then Exit;
  FValue:=AValue;
  FEmptyText:= false;
  SelectAll;
  FVirtualScreen.RedrawBitmap;
  if Assigned(FOnChange) then FOnChange(self);
end;

procedure TBarUpDown.SetVisible(AValue: boolean);
begin
  FVirtualScreen.Visible := AValue;
end;

function TBarUpDown.ValueToBarPos(AValue: integer): integer;
var t: single;
begin
  if FMaxValue>FMinValue then
  begin
    t := (AValue-FMinValue)/(FMaxValue-FMinValue);
    if t < 0 then t := 0;
    if t > 1 then t := 1;
    result := 1+round(power(t,1/FBarExponent)*(FBarWidth-1))
  end
  else
    result := 1;
end;

function TBarUpDown.BarPosToValue(ABarPos: integer): integer;
var t: single;
begin
  if FBarWidth > 1 then
  begin
    t := (ABarPos-1)/(FBarWidth-1);
    if t < 0 then t := 0;
    if t > 1 then t := 1;
    result := round(power(t,FBarExponent)*(FMaxValue-FMinValue))+FMinValue
  end
  else
    result := FMinValue;
end;

procedure TBarUpDown.SelectAll;
begin
  FSelStart := 0;
  FSelLength := length(Text);
  FVirtualScreen.DiscardBitmap;
end;

procedure TBarUpDown.SetFocus;
begin
   SafeSetFocus(FVirtualScreen);
end;

procedure TBarUpDown.DelayTimer;
begin
  if FTimer.Enabled then
  begin
    FTimer.Enabled:= false;
    FTimer.Enabled:= true;
  end;
end;

function TBarUpDown.RemoveSelection: boolean;
var
  tempText: string;
  len:integer;
begin
  if SelLength > 0 then
  begin
    tempText := Text;
    len := FSelLength;
    FSelLength := 0;
    Delete(tempText,SelStart+1,len);
    Text := tempText;
    FVirtualScreen.DiscardBitmap;
    result := true
  end else
    result := false;
end;

end.
