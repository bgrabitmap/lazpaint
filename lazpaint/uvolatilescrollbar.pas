unit UVolatileScrollBar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, BGRABitmap, BGRAGradients;

var
  VolatileScrollBarSize : integer = 16;
  VolatileThumbSize : integer = 24;
  VolatileBorderSize : integer = 3;

type

  { TVolatileScrollBar }

  TVolatileScrollBar = class
  private
    function GetScrollThumbBounds: TRect;
    procedure SetScrollThumbBounds(AValue: TRect);
  protected
    FBounds: TRect;
    FWidth,FHeight: integer;
    FDirection: TScrollBarKind;
    FPosition, FMinimum, FMaximum: integer;
    FPhong: TPhongShading;
    FScrollThumbDown: boolean;
    FMouseOrigin: TPoint;
    FScrollThumbBoundsOrigin: TRect;
  public
    constructor Create(X,Y,AWidth,AHeight: integer; ADirection: TScrollBarKind; APosition, AMinimum, AMaximum: integer);
    destructor Destroy; override;
    function MouseDown(X,Y: integer): boolean;
    function MouseMove(X,Y: integer): boolean;
    function MouseUp({%H-}X,{%H-}Y: integer): boolean;
    procedure Draw(ADest: TBGRABitmap);
    property ScrollThumbBounds: TRect read GetScrollThumbBounds write SetScrollThumbBounds;
    property ScrollThumbDown: boolean read FScrollThumbDown;
    property Minimum: integer read FMinimum;
    property Maximum: integer read FMaximum;
    property Position: integer read FPosition;
  end;

implementation

uses Graphics, Types, BGRABitmapTypes, LazPaintType;

{ TVolatileScrollBar }

function TVolatileScrollBar.GetScrollThumbBounds: TRect;
begin
  if FMaximum <= FMinimum then
    result := EmptyRect
  else
  if FDirection = sbHorizontal then
  begin
    result.left := FBounds.Left+round((FPosition-FMinimum)/(FMaximum-FMinimum)*(FWidth-VolatileThumbSize));
    result.top := FBounds.Top;
    result.right := result.left+VolatileThumbSize;
    result.bottom := FBounds.Bottom;
  end else //sbVertical
  begin
    result.left := FBounds.Left;
    result.top := FBounds.Top+round((FPosition-FMinimum)/(FMaximum-FMinimum)*(FHeight-VolatileThumbSize));
    result.right := FBounds.Right;
    result.bottom := result.top+VolatileThumbSize;
  end;
end;

procedure TVolatileScrollBar.SetScrollThumbBounds(AValue: TRect);
begin
  if FMaximum > FMinimum then
  begin
    if (FDirection = sbHorizontal) and (FWidth-VolatileThumbSize > 0) then
    begin
      FPosition:= FMinimum + round((AValue.Left-FBounds.Left)/(FWidth-VolatileThumbSize)*(FMaximum-FMinimum));
    end else
    if (FDirection = sbVertical) and (FHeight-VolatileThumbSize > 0) then
    begin
      FPosition:= FMinimum + round((AValue.Top-FBounds.Top)/(FHeight-VolatileThumbSize)*(FMaximum-FMinimum));
    end;
    if FPosition < FMinimum then FPosition := FMinimum;
    if FPosition > FMaximum then FPosition := FMaximum;
  end;
end;

constructor TVolatileScrollBar.Create(X, Y, AWidth, AHeight: integer;
  ADirection: TScrollBarKind; APosition, AMinimum, AMaximum: integer);
begin
     FWidth := AWidth;
     FHeight := AHeight;
     if FWidth < VolatileScrollBarSize then FWidth := VolatileScrollBarSize;
     if FHeight < VolatileScrollBarSize then FHeight := VolatileScrollBarSize;
     FDirection:= ADirection;
     if FDirection = sbHorizontal then
     begin
       FHeight := VolatileScrollBarSize;
       if FWidth < VolatileThumbSize then FWidth := VolatileThumbSize;
     end;
     if FDirection = sbVertical then
     begin
       FWidth := VolatileScrollBarSize;
       if FHeight < VolatileThumbSize then FHeight := VolatileThumbSize;
     end;
     FBounds := rect(X,Y,X+FWidth,Y+FHeight);
     FPosition := APosition;
     FMinimum := AMinimum;
     FMaximum:= AMaximum;
     if FMaximum < FMinimum then FMaximum := FMinimum;
     if FPosition < FMinimum then FPosition := FMinimum;
     if FPosition > FMaximum then FPosition := FMaximum;
     FPhong := TPhongShading.Create;
     FPhong.LightPositionZ := 400;
end;

destructor TVolatileScrollBar.Destroy;
begin
  FPhong.Free;
  inherited Destroy;
end;

function TVolatileScrollBar.MouseDown(X, Y: integer): boolean;
var NewThumbBounds: TRect;
begin
  if not FScrollThumbDown and PtInRect(point(x,y),ScrollThumbBounds) then
  begin
    result := true;
    FScrollThumbDown:= true;
    FMouseOrigin := point(X,Y);
    FScrollThumbBoundsOrigin := ScrollThumbBounds;
  end else
  if PtInRect(point(x,y),FBounds) then
  begin
    NewThumbBounds := ScrollThumbBounds;
    OffsetRect(NewThumbBounds, X-(NewThumbBounds.Left+NewThumbBounds.Right) div 2,
      Y-(NewThumbBounds.Top+NewThumbBounds.Bottom) div 2);
    ScrollThumbBounds := NewThumbBounds;
    FScrollThumbDown := true;
    FMouseOrigin := point(X,Y);
    FScrollThumbBoundsOrigin := NewThumbBounds;
    result := true;
  end else
    result := false;
end;

function TVolatileScrollBar.MouseMove(X, Y: integer): boolean;
var NewThumbBounds: TRect;
   PreviousPos: integer;
begin
  if FScrollThumbDown then
  begin
    NewThumbBounds := FScrollThumbBoundsOrigin;
    OffsetRect(NewThumbBounds, X-FMouseOrigin.X, Y-FMouseOrigin.Y);
    PreviousPos := FPosition;
    ScrollThumbBounds := NewThumbBounds;
    result := (FPosition <> PreviousPos);
  end else
    result := false;
end;

function TVolatileScrollBar.MouseUp(X, Y: integer): boolean;
begin
  if FScrollThumbDown then
  begin
    FScrollThumbDown := false;
    result := true;
  end
   else
    result := false;
end;

procedure TVolatileScrollBar.Draw(ADest: TBGRABitmap);
var lThumb: TRect; h: integer;
begin
  ADest.FillRect(FBounds,ColorToBGRA(ColorToRGB(clBtnFace),192),dmDrawWithTransparency);
  lThumb := GetScrollThumbBounds;
  if FScrollThumbDown then
    h := -3
  else
    h := 5;
  FPhong.DrawRectangle(ADest,lThumb,VolatileBorderSize,h,ColorToBGRA(ColorToRGB(clBtnFace)),true,[]);
end;

initialization

  VolatileScrollBarSize := ScaleX(VolatileScrollBarSize, OriginalDPI);
  VolatileThumbSize := ScaleX(VolatileThumbSize, OriginalDPI);

end.

