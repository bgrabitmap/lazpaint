unit USelectionHighlight;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes, UImage, UGridBitmap;

type
  { TSelectionHighlight }

  TSelectionHighlight = class
  protected
    FHighlight: TGridBitmap;
    FInvalidatedSelectionRect: TRect;
    FFillSelection: boolean;
    FRenderWidth, FRenderHeight: integer;
    FSelectionWidth, FSelectionHeight: integer;
    FImage: TLazPaintImage;
    procedure SetFillSelection(AValue: boolean);
  public
    constructor Create(AImage: TLazPaintImage);
    destructor Destroy; override;
    procedure Update(ARenderWidth, ARenderHeight: Integer; ARenderVisibleBounds: TRect);
    procedure Discard;
    procedure Draw(ADestination: TBGRABitmap; X,Y: Integer; AMode: TDrawMode = dmDrawWithTransparency; AOpacity: byte = 255);
    procedure StretchDraw(ADestination: TBGRABitmap; ARect: TRect; AMode: TDrawMode = dmDrawWithTransparency; AOpacity: byte = 255);
    procedure DrawAffine(ADestination: TBGRACustomBitmap; AMatrix: TAffineMatrix; AFilter: TResampleFilter; AOpacity: byte = 255);
    procedure NotifyChange(const ARect: TRect);
    property FillSelection: boolean read FFillSelection write SetFillSelection;
  end;

implementation

uses UGraph, Types, BGRAFilterScanner, BGRATransform, Math;

{ TSelectionHighlight }

procedure TSelectionHighlight.SetFillSelection(AValue: boolean);
begin
  if FFillSelection=AValue then Exit;
  FFillSelection:=AValue;
  if Assigned(FHighlight) then FImage.RenderMayChange(FHighlight.GetImageBounds);
  Discard;
end;

constructor TSelectionHighlight.Create(AImage: TLazPaintImage);
begin
  FHighlight := nil;
  FFillSelection:= true;
  FImage := AImage;
end;

destructor TSelectionHighlight.Destroy;
begin
   FreeAndNil(FHighlight);
  inherited Destroy;
end;

procedure TSelectionHighlight.Update(ARenderWidth, ARenderHeight: Integer; ARenderVisibleBounds: TRect);
var filter: TBGRAEmbossHightlightScanner;
  affine: TBGRAAffineBitmapTransform;
  boundsToRedraw,boundsToRedrawAffine,
  boundsForFilter,boundsForFilterAffine: TRect;
  fx,fy: double;
  selection: TBGRABitmap;
  selectionBounds: TRect;
  gridCoverage, extendedGridCoverage: TRect;
begin
  selection := FImage.SelectionReadonly;
  selectionBounds := FImage.SelectionBounds;
  if (selection = nil) or (selection.Width = 0) or (selection.Width = 0) or
     IsRectEmpty(selectionBounds) then
  begin
    FreeAndNil(FHighlight);
    FInvalidatedSelectionRect := EmptyRect;
    FSelectionWidth := 0;
    FSelectionHeight := 0;
  end else
  begin
    if (FHighlight = nil) or (FRenderWidth <> ARenderWidth) or (FRenderHeight <> ARenderHeight)
      or (FSelectionWidth <> selection.Width) or (FSelectionHeight <> selection.Height) then
    begin
      FRenderWidth := ARenderWidth;
      FRenderHeight := ARenderHeight;

      FreeAndNil(FHighlight);
      FHighlight := TGridBitmap.Create(FRenderWidth,FRenderHeight,256,256);

      FSelectionWidth := selection.Width;
      FSelectionHeight := selection.Height;
      FInvalidatedSelectionRect := rect(0,0,FSelectionWidth,FSelectionHeight);
    end;

    fx := FRenderWidth/selection.Width;
    fy := FRenderHeight/selection.Height;

    extendedGridCoverage := selectionBounds;
    extendedGridCoverage := rect(floor(extendedGridCoverage.Left*fx),floor(extendedGridCoverage.Top*fy),
                            ceil(extendedGridCoverage.Right*fx), ceil(extendedGridCoverage.Bottom*fy));
    InflateRect(extendedGridCoverage,1,1);
    gridCoverage := RectInter(extendedGridCoverage, ARenderVisibleBounds);
    extendedGridCoverage := RectInter(extendedGridCoverage, FHighlight.InflateRectToCells(ARenderVisibleBounds) );
    FHighlight.Crop(gridCoverage);
    if IsRectEmpty(gridCoverage) then
    begin
      FInvalidatedSelectionRect := EmptyRect;
      exit;
    end;

    if (fx <> 1) or (fy <> 1) then
    begin
      affine := TBGRAAffineBitmapTransform.Create(selection,false,rfBox);
      affine.Matrix := AffineMatrixScale(1/fx,1/fy);
      boundsForFilterAffine := extendedGridCoverage;
      InflateRect(boundsForFilterAffine,1,1);
      filter := TBGRAEmbossHightlightScanner.Create(affine,
                    boundsForFilterAffine,
                    True);
      filter.FillSelection := FFillSelection;
    end else
    begin
      affine := nil;
      filter := TBGRAEmbossHightlightScanner.Create(selection, True);
      filter.FillSelection := FFillSelection;
    end;

    if not IsRectEmpty(FInvalidatedSelectionRect) then
    begin
      boundsToRedraw := FInvalidatedSelectionRect;
      InflateRect(boundsToRedraw,1,1);
      boundsToRedraw := RectInter(boundsToRedraw,
                              rect(0,0,selection.Width,selection.Width));
      boundsToRedrawAffine := rect(floor(boundsToRedraw.Left*fx),floor(boundsToRedraw.Top*fy),
                              ceil(boundsToRedraw.Right*fx), ceil(boundsToRedraw.Bottom*fy));

      if IsRectEmpty(selectionBounds) then
        FHighlight.Clear
      else
      begin
        boundsForFilter := selectionBounds;
        InflateRect(boundsForFilter,1,1);
        boundsForFilter := RectInter(boundsForFilter, boundsToRedraw);

        if IsRectEmpty(boundsForFilter) then
          FHighlight.FillRect(boundsToRedrawAffine, BGRAPixelTransparent, dmSet, ceExistsAlready)
        else
        begin
          boundsForFilterAffine := rect(floor(boundsForFilter.Left*fx),floor(boundsForFilter.Top*fy),
                                ceil(boundsForFilter.Right*fx), ceil(boundsForFilter.Bottom*fy));

          FHighlight.FillRect(rect(boundsToRedrawAffine.Left,boundsToRedrawAffine.Top,boundsToRedrawAffine.Right,boundsForFilterAffine.Top), BGRAPixelTransparent, dmSet, ceExistsAlready);
          FHighlight.FillRect(rect(boundsToRedrawAffine.Left,boundsForFilterAffine.Top,boundsForFilterAffine.Left,boundsForFilterAffine.Bottom), BGRAPixelTransparent, dmSet, ceExistsAlready);
          FHighlight.FillRect(rect(boundsForFilterAffine.Right,boundsForFilterAffine.Top,boundsToRedrawAffine.Right,boundsForFilterAffine.Bottom), BGRAPixelTransparent, dmSet, ceExistsAlready);
          FHighlight.FillRect(rect(boundsToRedrawAffine.Left,boundsForFilterAffine.Bottom,boundsToRedrawAffine.Right,boundsToRedrawAffine.Bottom), BGRAPixelTransparent, dmSet, ceExistsAlready);

          FHighlight.FillRect(boundsForFilterAffine, filter, dmSet, ceExistsAlready);
        end;
      end;
      FInvalidatedSelectionRect := EmptyRect;
    end;

    FHighlight.FillRect(extendedGridCoverage, filter, dmSet, ceDoNotExist);

    filter.Free;
    affine.Free;
  end;
end;

procedure TSelectionHighlight.Discard;
begin
  if Assigned(FHighlight) then
    FInvalidatedSelectionRect := rect(0,0,FSelectionWidth,FSelectionHeight)
  else
    FInvalidatedSelectionRect := EmptyRect;
end;

procedure TSelectionHighlight.Draw(ADestination: TBGRABitmap; X, Y: Integer;
  AMode: TDrawMode; AOpacity: byte);
begin
  if Assigned(FHighlight) then FHighlight.Draw(ADestination,X,Y,AMode,AOpacity);
end;

procedure TSelectionHighlight.StretchDraw(ADestination: TBGRABitmap;
  ARect: TRect; AMode: TDrawMode; AOpacity: byte);
begin
  if Assigned(FHighlight) then FHighlight.StretchDraw(ADestination,ARect,AMode,AOpacity);
end;

procedure TSelectionHighlight.DrawAffine(ADestination: TBGRACustomBitmap;
  AMatrix: TAffineMatrix; AFilter: TResampleFilter; AOpacity: byte);
begin
  if Assigned(FHighlight) then FHighlight.DrawAffine(ADestination,AMatrix,AFilter,AOpacity);
end;

procedure TSelectionHighlight.NotifyChange(const ARect: TRect);
begin
  if Assigned(FHighlight) and not IsRectEmpty(ARect) then
  begin
    FInvalidatedSelectionRect := RectUnion(FInvalidatedSelectionRect, ARect);
    FInvalidatedSelectionRect := RectInter(FInvalidatedSelectionRect, rect(0,0,FSelectionWidth,FSelectionHeight));
   end;
end;

end.

