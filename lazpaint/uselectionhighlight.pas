// SPDX-License-Identifier: GPL-3.0-only
unit USelectionHighlight;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes, UImage;

const
  GridSize = 128;

type
  { TSelectionHighlight }

  TSelectionHighlight = class
  protected
    FGrid: array of array of record
      selectionPoly: array of TPointF;
      selectionPolyComputed: boolean;
    end;
    FGridRows,FGridCols: integer;
    FFillSelection: boolean;
    FZoom: TPointF;
    FImage: TLazPaintImage;
    procedure SetFillSelection(AValue: boolean);
    procedure ComputeGridCells(const APixelRect: TRect);
    procedure DiscardGridCells(const APixelRect: TRect);
    function GetGridRect(const APixelRect: TRect): TRect;
    procedure ComputeGridCell(x,y: integer);
  public
    constructor Create(AImage: TLazPaintImage);
    destructor Destroy; override;
    procedure Update(ARenderWidth, ARenderHeight: Integer; {%H-}ARenderVisibleBounds: TRect);
    procedure Discard;
    procedure DrawAffine(ADestination: TBGRACustomBitmap; const AMatrix: TAffineMatrix; AFilter: TResampleFilter; AMode: TDrawMode = dmDrawWithTransparency; AOpacity: byte = 255);
    procedure NotifyChange(const ARect: TRect);
    property FillSelection: boolean read FFillSelection write SetFillSelection;
  end;

implementation

uses ugraph, Types, BGRATransform, BGRAVectorize, BGRAGradientScanner;

{ TSelectionHighlight }

procedure TSelectionHighlight.SetFillSelection(AValue: boolean);
begin
  if FFillSelection=AValue then Exit;
  FFillSelection:=AValue;
  Discard;
end;

procedure TSelectionHighlight.ComputeGridCells(const APixelRect: TRect);
var
  y, x: integer;
  gr: TRect;
begin
  gr := GetGridRect(APixelRect);
  for y := gr.Top to gr.Bottom-1 do
    for x := gr.Left to gr.Right-1 do
      ComputeGridCell(x,y);
end;

procedure TSelectionHighlight.DiscardGridCells(const APixelRect: TRect);
var
  y, x: integer;
  gr: TRect;
begin
  gr := GetGridRect(APixelRect);
  for y := gr.Top to gr.Bottom-1 do
    for x := gr.Left to gr.Right-1 do
    with FGrid[y,x] do
    begin
      selectionPoly := nil;
      selectionPolyComputed:= false;
    end;
end;

function TSelectionHighlight.GetGridRect(const APixelRect: TRect): TRect;
begin
  result := rect(APixelRect.Left div GridSize, APixelRect.Top div GridSize,
                 (APixelRect.Right+GridSize-1) div GridSize, (APixelRect.Bottom+GridSize-1) div GridSize);
  IntersectRect(result, result, rect(0,0,FGridCols,FGridRows));
end;

procedure TSelectionHighlight.ComputeGridCell(x, y: integer);
const vectorizeMargin = 2;
begin
  with FGrid[y,x] do
  begin
    if not selectionPolyComputed then
    begin
      selectionPoly := VectorizeMonochrome(FImage.SelectionMaskReadonly,
        rect(x*GridSize-vectorizeMargin,y*Gridsize-vectorizeMargin,
        (x+1)*GridSize+vectorizeMargin,(y+1)*GridSize+vectorizeMargin), 1,false,false);
      selectionPolyComputed:= true;
    end;
  end;
end;

constructor TSelectionHighlight.Create(AImage: TLazPaintImage);
begin
  FFillSelection:= true;
  FImage := AImage;
  FZoom := PointF(1,1);
  FGridRows := 0;
  FGridCols := 0;
end;

destructor TSelectionHighlight.Destroy;
begin
  inherited Destroy;
end;

procedure TSelectionHighlight.Update(ARenderWidth, ARenderHeight: Integer; ARenderVisibleBounds: TRect);
begin
  if FImage.SelectionMaskEmpty then
  begin
    FGrid := nil;
    FGridRows := 0;
    FGridCols := 0;
  end
  else
  begin
    FGridRows := (FImage.Height+GridSize-1) div GridSize;
    FGridCols := (FImage.Width+GridSize-1) div GridSize;
    setlength(FGrid, FGridRows, FGridCols);
  end;
  FZoom := PointF(ARenderWidth/FImage.Width,ARenderHeight/FImage.Height);
end;

procedure TSelectionHighlight.Discard;
var
  y, x: Integer;
begin
  for y := 0 to FGridRows-1 do
    for x := 0 to FGridCols-1 do
      FGrid[y,x].selectionPolyComputed:= false;
end;

procedure TSelectionHighlight.DrawAffine(ADestination: TBGRACustomBitmap;
  const AMatrix: TAffineMatrix; AFilter: TResampleFilter; AMode: TDrawMode; AOpacity: byte);

  procedure DrawPoly(const AMatrix: TAffineMatrix; const APoly: array of TPointF;
            const ABounds: TRectF);
  const
    CoordShift = 8;
    CoordPrecision = 1 shl CoordShift;
  var
    m: TAffineMatrix;
    pts: array of record
           coord: TPoint;
           insideX,insideY: integer;
         end;

    function CoordShr(AValue: integer): integer; inline;
    begin
      if AValue >= 0 then
        result := AValue shr CoordShift
      else result := -((-AValue + CoordPrecision-1) shr CoordShift);
    end;

    procedure DrawSeg(AFrom,ATo: integer);
    var
      len, lenX,lenY, dotProd, value, value2: integer;
      ptFrom,ptTo: TPoint;
      u: TPointF;
      t: single;
    begin
      if (abs(pts[ATo].insideX+pts[AFrom].insideX)=2) or
         (abs(pts[ATo].insideY+pts[AFrom].insideY)=2) then exit;

      ptFrom := pts[AFrom].coord;
      ptTo := pts[ATo].coord;

      if (pts[ATo].insideX <> 0) or (pts[AFrom].insideX <> 0) or
         (pts[ATo].insideY <> 0) or (pts[AFrom].insideY <> 0) then
      begin
        u := APoly[ATo]-APoly[AFrom];
        if u.x <> 0 then
        begin
          if pts[ATo].insideX > 0 then
          begin
            t := (ABounds.Right-APoly[AFrom].x)/u.x;
            ptTo := (m*(APoly[AFrom]+u*t)).Round;
          end
          else if pts[ATo].insideX < 0 then
          begin
            t := (ABounds.Left-APoly[AFrom].x)/u.x;
            ptTo := (m*(APoly[AFrom]+u*t)).Round;
          end;
          if pts[AFrom].insideX > 0 then
          begin
            t := (ABounds.Right-APoly[ATo].x)/u.x;
            ptFrom := (m*(APoly[ATo]+u*t)).Round;
          end
          else if pts[AFrom].insideX < 0 then
          begin
            t := (ABounds.Left-APoly[ATo].x)/u.x;
            ptFrom := (m*(APoly[ATo]+u*t)).Round;
          end;
        end;
        if u.y <> 0 then
        begin
          if pts[ATo].insideY > 0 then
          begin
            t := (ABounds.Bottom-APoly[AFrom].y)/u.y;
            ptTo := (m*(APoly[AFrom]+u*t)).Round;
          end
          else if pts[ATo].insideY < 0 then
          begin
            t := (ABounds.Top-APoly[AFrom].y)/u.y;
            ptTo := (m*(APoly[AFrom]+u*t)).Round;
          end;
          if pts[AFrom].insideY > 0 then
          begin
            t := (ABounds.Bottom-APoly[ATo].y)/u.y;
            ptFrom := (m*(APoly[ATo]+u*t)).Round;
          end
          else if pts[AFrom].insideY < 0 then
          begin
            t := (ABounds.Top-APoly[ATo].y)/u.y;
            ptFrom := (m*(APoly[ATo]+u*t)).Round;
          end;
        end;
      end;
      lenX := abs(ptTo.x-ptFrom.x);
      lenY := abs(ptTo.y-ptFrom.y);
      len := lenX+lenY;
      if len = 0 then exit;
      dotProd := -(ptTo.x-ptFrom.x)+
                 (ptTo.y-ptFrom.y);
      value := dotProd*127 div len + 128;
      value2 := (value*2+128) div 3;
      ptFrom.x := CoordShr(ptFrom.x);
      ptFrom.y := CoordShr(ptFrom.y);
      ptTo.x := CoordShr(ptTo.x);
      ptTo.y := CoordShr(ptTo.y);
      if value <= 128 then
      begin
        if lenY < lenX then
          ADestination.DrawLineAntialias(ptFrom.x,ptFrom.y-1,
            ptTo.x,ptTo.y-1, BGRA(value2,value2,value2), true) else
          ADestination.DrawLineAntialias(ptFrom.x-1,ptFrom.y,
            ptTo.x-1,ptTo.y, BGRA(value2,value2,value2), true);
      end else
      begin
        if lenY < lenX then
          ADestination.DrawLineAntialias(ptFrom.x,ptFrom.y-1,
            ptTo.x,ptTo.y-1, BGRA(value2,value2,value2), true) else
          ADestination.DrawLineAntialias(ptFrom.x-1,ptFrom.y,
            ptTo.x-1,ptTo.y, BGRA(value2,value2,value2), true);
      end;
      ADestination.DrawLineAntialias(ptFrom.x,ptFrom.y,
        ptTo.x,ptTo.y, BGRA(value,value,value), false);
    end;

  var
    i, j: Integer;
  begin
    m := AffineMatrixTranslation(CoordPrecision div 2, CoordPrecision div 2)*
         AffineMatrixScale(CoordPrecision,CoordPrecision)*AMatrix;
    pts := nil;
    setlength(pts, length(APoly));
    for i := 0 to high(APoly) do
    begin
      pts[i].coord := (m*APoly[i]).Round;
      if APoly[i].x <= ABounds.Left then pts[i].insideX := -1
      else if APoly[i].x >= ABounds.Right then pts[i].insideX := +1
      else pts[i].insideX:= 0;
      if APoly[i].y <= ABounds.Top then pts[i].insideY := -1
      else if APoly[i].y >= ABounds.Bottom then pts[i].insideY := +1
      else pts[i].insideY:= 0;
    end;

    j := 0;
    for i := 0 to high(APoly) do
      if IsEmptyPoint(pts[i].coord) then
      begin
        if i > j then DrawSeg(i-1,j);
        j := i+1;
      end else
      begin
        if i > j then DrawSeg(i-1,i);
      end;
    if length(APoly) > j then DrawSeg(high(APoly),j);
  end;

var
  m: TAffineMatrix;
  aff: TBGRAAffineBitmapTransform;
  x,y: integer;
  b: TRectF;
  transf: TRect;
  band: TBGRABitmap;

begin
  if FImage.SelectionMaskEmpty then exit;

  m := AMatrix*AffineMatrixScale(FZoom.x,FZoom.y);
  if FillSelection then
  begin
    aff := TBGRAAffineBitmapTransform.Create(FImage.SelectionMaskReadonly,false,AFilter);
    aff.ViewMatrix := AffineMatrixTranslation(-0.5,-0.5)*m*AffineMatrixTranslation(0.5,0.5);
    band := TBGRABitmap.Create(ADestination.ClipRect.Width,1);
    x := ADestination.ClipRect.Left;
    for y := ADestination.ClipRect.Top to ADestination.ClipRect.Bottom-1 do
    begin
      band.FillRect(0,0,band.Width,1, aff, dmSet, Point(x,y));
      ADestination.FillMask(x,y,band,BGRA(64,128,192,AOpacity div 2),AMode);
    end;
    band.Free;
    aff.Free;
  end;

  for y := 0 to FGridRows-1 do
    for x := 0 to FGridCols-1 do
    begin
      b := rectF(x*GridSize,y*GridSize,(x+1)*GridSize,(y+1)*GridSize);
      transf := (m*TAffineBox.AffineBox(b)).RectBounds;
      if transf.IntersectsWith(ADestination.ClipRect) then
      begin
        ComputeGridCell(x,y);
        if x = 0 then b.Left -= 1;
        if y = 0 then b.Top -= 1;
        if x = FGridCols-1 then b.right += 1;
        if y = FGridRows-1 then b.Bottom += 1;
        DrawPoly(m, FGrid[y,x].selectionPoly, b);
      end;
    end;
end;

procedure TSelectionHighlight.NotifyChange(const ARect: TRect);
begin
  DiscardGridCells(ARect);
end;

end.

