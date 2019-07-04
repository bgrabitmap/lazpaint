unit UGridBitmap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmapTypes;

type
  TDrawInCellProc = procedure(ACellBitmap: TBGRACustomBitmap; const ACellRect: TRect; AData: pointer) of object;
  TCellExistence = (ceDontMind, ceExistsAlready, ceDoNotExist);

  { TGridBitmap }

  TGridBitmap = class
  protected
    FWidth,FHeight: integer;
    FCellHeight: integer;
    FCellWidth: integer;
    FBitmaps: array of array of TBGRACustomBitmap;
    FGridWidth, FGridHeight: integer;
    FBitmapFactory: TBGRABitmapAny;
    FBackgroundColor: TBGRAPixel;
    procedure FillCell(ACellBitmap: TBGRACustomBitmap; const {%H-}ACellRect: TRect; AData: pointer);
    procedure FillCellWithTexture(ACellBitmap: TBGRACustomBitmap; const ACellRect: TRect; AData: pointer);
    procedure DrawCell(ACellBitmap: TBGRACustomBitmap; const ACellRect: TRect; AData: pointer);
    procedure StretchDrawCell(ACellBitmap: TBGRACustomBitmap; const ACellRect: TRect; AData: pointer);
    procedure DrawAffineCell(ACellBitmap: TBGRACustomBitmap; const ACellRect: TRect; AData: pointer);
    procedure GetCellBounds(ACellBitmap: TBGRACustomBitmap; const ACellRect: TRect; AData: pointer);
  public
    constructor Create(AWidth,AHeight: Integer; ACellWidth: integer = 128; ACellHeight: integer = 128);
    destructor Destroy; override;
    procedure Clear;
    procedure ForEachCell(ABounds: TRect; ADrawCallback: TDrawInCellProc; AData: Pointer = nil; ACellExistence: TCellExistence = ceDontMind);
    procedure FillRect(ABounds: TRect; AColor: TBGRAPixel; AMode: TDrawMode = dmSet; ACellExistence: TCellExistence = ceDontMind);
    procedure FillRect(ABounds: TRect; ATexture: IBGRAScanner; AMode: TDrawMode = dmSet; ACellExistence: TCellExistence = ceDontMind);
    procedure FillRect(ABounds: TRect; ATexture: IBGRAScanner; AMode: TDrawMode; AScanOffset: TPoint; ACellExistence: TCellExistence = ceDontMind);
    procedure ForEachExistingCell(ADrawCallback: TDrawInCellProc; AData: Pointer = nil);
    procedure Draw(ADestination: TBGRACustomBitmap; X,Y: Integer; AMode: TDrawMode = dmDrawWithTransparency; AOpacity: byte = 255);
    procedure StretchDraw(ADestination: TBGRACustomBitmap; ARect: TRect; AMode: TDrawMode = dmDrawWithTransparency; AOpacity: byte = 255);
    procedure DrawAffine(ADestination: TBGRACustomBitmap; AMatrix: TAffineMatrix; AFilter: TResampleFilter; AMode: TDrawMode = dmDrawWithTransparency; AOpacity: byte = 255);
    procedure Crop(ABounds: TRect);
    function InflateRectToCells(ABounds: TRect): TRect;
    function GetImageBounds(AChannel: TChannel = cAlpha; ANothingValue: Byte = 0): TRect;
    property Width: integer read FWidth;
    property Height: integer read FHeight;
    property CellWidth: integer read FCellWidth;
    property CellHeight: integer read FCellHeight;
    property GridWidth: integer read FGridWidth;
    property GridHeight: integer read FGridHeight;
    property BitmapFactory: TBGRABitmapAny read FBitmapFactory write FBitmapFactory;
    property BackgroundColor: TBGRAPixel read FBackgroundColor write FBackgroundColor;
  end;

implementation

uses Types, BGRATransform;

type
  TFillRectParam = object
    Color: TBGRAPixel;
    Mode: TDrawMode;
  end;
  TFillRectWithTextureParam = object
    Texture: IBGRAScanner;
    ScanOffset: TPoint;
    Mode: TDrawMode;
  end;
  TDrawCellParam = object
    OfsX,OfsY: integer;
    Destination: TBGRACustomBitmap;
    Mode: TDrawMode;
    Opacity: Byte;
  end;
  TStretchDrawCellParam = object(TDrawCellParam)
    ScaleX,ScaleY: double;
  end;
  TCellBoundsParam = object
    Channel: TChannel;
    NothingValue: Byte;
    CellBounds: TRect;
    BoundsAccumulator: TRect;
  end;
  TDrawAffineParam = object
    Destination: TBGRACustomBitmap;
    Matrix: TAffineMatrix;
    Filter: TResampleFilter;
    Opacity: byte;
    Mode: TDrawMode;
  end;

{ TGridBitmap }

procedure TGridBitmap.FillCell(ACellBitmap: TBGRACustomBitmap;
  const ACellRect: TRect; AData: pointer);
begin
  with TFillRectParam(AData^) do
    ACellBitmap.FillRect(rect(0,0,ACellBitmap.Width,ACellBitmap.Height),Color,Mode);
end;

procedure TGridBitmap.FillCellWithTexture(ACellBitmap: TBGRACustomBitmap;
  const ACellRect: TRect; AData: pointer);
begin
  with TFillRectWithTextureParam(AData^) do
    ACellBitmap.FillRect(rect(0,0,ACellBitmap.Width,ACellBitmap.Height),Texture,Mode,Point(ScanOffset.X+ACellRect.Left,ScanOffset.Y+ACellRect.Top));
end;

procedure TGridBitmap.DrawCell(ACellBitmap: TBGRACustomBitmap;
  const ACellRect: TRect; AData: pointer);
begin
  with TDrawCellParam(AData^) do
    Destination.PutImage(ACellRect.Left+OfsX,ACellRect.Top+OfsY,ACellBitmap,Mode,Opacity);
end;

procedure TGridBitmap.StretchDrawCell(ACellBitmap: TBGRACustomBitmap;
  const ACellRect: TRect; AData: pointer);
begin
  with TStretchDrawCellParam(AData^) do
    Destination.StretchPutImage(rect(round(ACellRect.Left*ScaleX)+OfsX,round(ACellRect.Top*ScaleY)+OfsY,
         round(ACellRect.Right*ScaleX)+OfsX,round(ACellRect.Bottom*ScaleY)+OfsY),
         ACellBitmap,Mode,Opacity);
end;

procedure TGridBitmap.DrawAffineCell(ACellBitmap: TBGRACustomBitmap;
  const ACellRect: TRect; AData: pointer);
var m: TAffineMatrix;
  ofs: TPointF;
begin
  with TDrawAffineParam(AData^) do
  begin
    m := Matrix;
    m[1,3] := 0;
    m[2,3] := 0;
    ofs := m*PointF(ACellRect.Left,ACellRect.Top);
    Destination.PutImageAffine(AffineMatrixTranslation(ofs.x,ofs.y)*Matrix,ACellBitmap,Filter,Opacity);
  end;
end;

procedure TGridBitmap.GetCellBounds(ACellBitmap: TBGRACustomBitmap;
  const ACellRect: TRect; AData: pointer);
begin
  with TCellBoundsParam(AData^) do
  begin
    CellBounds := ACellBitmap.GetImageBounds(Channel,NothingValue);
    if not IsRectEmpty(CellBounds) then
    begin
      OffsetRect(CellBounds, ACellRect.Left,ACellRect.Top);
      if IsRectEmpty(BoundsAccumulator) then
        BoundsAccumulator := CellBounds
      else
        UnionRect(BoundsAccumulator, BoundsAccumulator, CellBounds);
    end;
  end;
end;

constructor TGridBitmap.Create(AWidth, AHeight: Integer; ACellWidth: integer;
  ACellHeight: integer);
begin
  FWidth := AWidth;
  FHeight := AHeight;
  FCellWidth:= ACellWidth;
  FCellHeight:= ACellHeight;
  FGridWidth:= (FWidth + FCellWidth-1) div FCellWidth;
  FGridHeight:= (FHeight + FCellHeight-1) div FCellHeight;
  FBitmapFactory:= BGRABitmapFactory;
  FBackgroundColor := BGRAPixelTransparent;
  setlength(FBitmaps, FGridHeight, FGridWidth);
end;

destructor TGridBitmap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TGridBitmap.Clear;
var x,y: integer;
begin
  for y := 0 to FGridHeight-1 do
    for x := 0 to FGridWidth-1 do
      FreeAndNil(FBitmaps[y,x]);
end;

procedure TGridBitmap.ForEachCell(ABounds: TRect; ADrawCallback: TDrawInCellProc; AData: Pointer; ACellExistence: TCellExistence);
var x,y,x1,y1,x2,y2: integer;
  cellRect,clip: TRect;
begin
  if ABounds.Left < 0 then ABounds.Left := 0;
  if ABounds.Top < 0 then ABounds.Top := 0;
  if ABounds.Right > FWidth then ABounds.Right := FWidth;
  if ABounds.Bottom > FHeight then ABounds.Bottom := FHeight;
  if (ABounds.Right <= ABounds.Left) or (ABounds.Bottom <= ABounds.Top) then exit;
  x1 := ABounds.Left div FCellWidth;
  x2 := (ABounds.Right + FCellWidth-1) div FCellWidth -1;
  y1 := ABounds.Top div FCellHeight;
  y2 := (ABounds.Bottom + FCellHeight-1) div FCellHeight -1;
  cellRect.Top := y1*FCellHeight;
  cellRect.Bottom := cellRect.Top+FCellHeight;
  for y := y1 to y2 do
  begin
    cellRect.Left := x1*FCellWidth-FCellWidth;
    cellRect.Right := cellRect.Left+FCellWidth;
    for x := x1 to x2 do
    begin
      cellRect.Left += FCellWidth;
      cellRect.Right += FCellWidth;
      if FBitmaps[y,x] = nil then
      begin
        if ACellExistence = ceExistsAlready then Continue;
        FBitmaps[y,x] := BitmapFactory.create(FCellWidth,FCellHeight,FBackgroundColor);
      end else
        if ACellExistence = ceDoNotExist then Continue;

      clip := EmptyRect;
      IntersectRect(clip,cellRect,ABounds);
      OffsetRect(clip, -cellRect.Left,-cellRect.Top);
      FBitmaps[y,x].ClipRect := clip;
      ADrawCallback(FBitmaps[y,x], cellRect, AData);
      FBitmaps[y,x].NoClip;
    end;
    cellRect.Top += FCellHeight;
    cellRect.Bottom += FCellHeight;
  end;
end;

procedure TGridBitmap.FillRect(ABounds: TRect; AColor: TBGRAPixel;
  AMode: TDrawMode; ACellExistence: TCellExistence);
var param: TFillRectParam;
begin
  param.Color := AColor;
  param.Mode := AMode;
  ForEachCell(ABounds, @FillCell, @param, ACellExistence);
end;

procedure TGridBitmap.FillRect(ABounds: TRect; ATexture: IBGRAScanner;
  AMode: TDrawMode; ACellExistence: TCellExistence);
begin
  FillRect(ABounds,ATexture,AMode,Point(0,0), ACellExistence);
end;

procedure TGridBitmap.FillRect(ABounds: TRect; ATexture: IBGRAScanner;
  AMode: TDrawMode; AScanOffset: TPoint; ACellExistence: TCellExistence);
var param: TFillRectWithTextureParam;
begin
  param.Texture := ATexture;
  param.Mode := AMode;
  param.ScanOffset := AScanOffset;
  ForEachCell(ABounds, @FillCellWithTexture, @param, ACellExistence);
end;

procedure TGridBitmap.ForEachExistingCell(ADrawCallback: TDrawInCellProc; AData: Pointer);
var x,y: integer;
  cellRect: TRect;
begin
  cellRect.Top := 0;
  cellRect.Bottom := cellRect.Top+FCellHeight;
  for y := 0 to FGridHeight-1 do
  begin
    cellRect.Left := 0;
    cellRect.Right := cellRect.Left+FCellWidth;
    for x := 0 to FGridWidth-1 do
    begin
      if FBitmaps[y,x] <> nil then
        ADrawCallback(FBitmaps[y,x], cellRect, AData);
      cellRect.Left += FCellWidth;
      cellRect.Right += FCellWidth;
    end;
    cellRect.Top += FCellHeight;
    cellRect.Bottom += FCellHeight;
  end;
end;

procedure TGridBitmap.Draw(ADestination: TBGRACustomBitmap; X, Y: Integer;
  AMode: TDrawMode; AOpacity: byte);
var param: TDrawCellParam;
begin
  param.OfsX := X;
  param.OfsY := Y;
  param.Destination := ADestination;
  param.Mode := AMode;
  param.Opacity:= AOpacity;
  ForEachExistingCell(@DrawCell,@param);
end;

procedure TGridBitmap.StretchDraw(ADestination: TBGRACustomBitmap; ARect: TRect;
  AMode: TDrawMode; AOpacity: byte);
var param: TStretchDrawCellParam;
begin
  if (ARect.Right <= ARect.Left) or
    (ARect.Bottom <= ARect.Top) or
    (Width = 0) or (Height = 0) then exit;
  param.OfsX := ARect.Left;
  param.OfsY := ARect.Top;
  param.Destination := ADestination;
  param.Mode := AMode;
  param.Opacity:= AOpacity;
  param.ScaleX := (ARect.Right-ARect.Left)/Width;
  param.ScaleY := (ARect.Bottom-ARect.Top)/Height;
  ForEachExistingCell(@StretchDrawCell,@param);
end;

procedure TGridBitmap.DrawAffine(ADestination: TBGRACustomBitmap;
  AMatrix: TAffineMatrix; AFilter: TResampleFilter; AMode: TDrawMode; AOpacity: byte);
var param: TDrawAffineParam;
begin
  if ADestination.IsAffineRoughlyTranslation(AMatrix, rect(0,0,Width,Height)) then
    Draw(ADestination, round(AMatrix[1,3]),round(AMatrix[2,3]), AMode, AOpacity)
  else
  begin
    param.Destination := ADestination;
    param.Matrix := AMatrix;
    param.Filter := AFilter;
    param.Opacity := AOpacity;
    param.Mode := AMode;
    ForEachExistingCell(@DrawAffineCell,@param);
  end;
end;

procedure TGridBitmap.Crop(ABounds: TRect);
var x,y,x1,y1,x2,y2: integer;
begin
  if ABounds.Left < 0 then ABounds.Left := 0;
  if ABounds.Top < 0 then ABounds.Top := 0;
  if ABounds.Right > FWidth then ABounds.Right := FWidth;
  if ABounds.Bottom > FHeight then ABounds.Bottom := FHeight;
  if (ABounds.Right <= ABounds.Left) or (ABounds.Bottom <= ABounds.Top) then
  begin
    Clear;
    exit;
  end;
  x1 := ABounds.Left div FCellWidth;
  x2 := (ABounds.Right + FCellWidth-1) div FCellWidth;
  y1 := ABounds.Top div FCellHeight;
  y2 := (ABounds.Bottom + FCellHeight-1) div FCellHeight;
  for y := 0 to FGridHeight-1 do
    for x := 0 to FGridWidth-1 do
      if (x < x1) or (x >= x2) or (y < y1) or (y >= y2) then
        FreeAndNil(FBitmaps[y,x]);
end;

function TGridBitmap.InflateRectToCells(ABounds: TRect): TRect;
var x1,y1,x2,y2: integer;
begin
  if ABounds.Left < 0 then ABounds.Left := 0;
  if ABounds.Top < 0 then ABounds.Top := 0;
  if ABounds.Right > FWidth then ABounds.Right := FWidth;
  if ABounds.Bottom > FHeight then ABounds.Bottom := FHeight;
  if (ABounds.Right <= ABounds.Left) or (ABounds.Bottom <= ABounds.Top) then
  begin
    result := EmptyRect;
    exit;
  end;
  x1 := ABounds.Left div FCellWidth;
  x2 := (ABounds.Right + FCellWidth-1) div FCellWidth;
  y1 := ABounds.Top div FCellHeight;
  y2 := (ABounds.Bottom + FCellHeight-1) div FCellHeight;
  result := rect(x1*FCellWidth,y1*FCellHeight,x2*FCellWidth,y2*FCellHeight);
end;

function TGridBitmap.GetImageBounds(AChannel: TChannel; ANothingValue: Byte): TRect;
var param: TCellBoundsParam;
begin
  param.Channel := AChannel;
  param.NothingValue := ANothingValue;
  param.BoundsAccumulator := EmptyRect;
  param.CellBounds := EmptyRect;
  ForEachExistingCell(@GetCellBounds,@param);
  result := param.BoundsAccumulator;
end;

end.

