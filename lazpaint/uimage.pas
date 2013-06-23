unit uimage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes, types,
  uimagestate, Graphics, BGRALayers;

type
  TLayeredBitmapAndSelection = record
        layeredBitmap: TBGRALayeredBitmap;
        selection: TBGRABitmap;
        selectionLayer: TBGRABitmap;
      end;

  TLazPaintImage = class;
  TOnCurrentSelectionChanged = procedure(ASender: TLazPaintImage; AOffsetOnly: boolean) of object;
  TOnStackChanged = procedure(ASender: TLazPaintImage; AScrollIntoView: boolean) of object;

  { TLazPaintImage }

  TLazPaintImage = class
  private
    FLastSelectionEmpty, FLastSelectionEmptyIsDefined,
    FLastSelectionLayerEmpty, FLastSelectionLayerEmptyIsDefined: boolean;
    FOnSelectionChanged: TOnCurrentSelectionChanged;
    FOnStackChanged: TOnStackChanged;
    FCurrentState: TImageState;
    FRenderedImage: TBGRABitmap;
    FRenderedImageInvalidated: TRect;
    function GetCurrentFilename: string;
    function GetCurrentImageLayer: TBGRABitmap;
    function GetCurrentImageLayerIndex:integer;
    function GetCurrentLayeredBitmap: TBGRALayeredBitmap;
    function GetEmpty: boolean;
    function GetHeight: integer;
    function GetCurrentSelection: TBGRABitmap;
    function GetNbLayers: integer;
    function GetRenderedImage: TBGRABitmap;
    function GetSelectionLayerBounds(AOffseted: boolean): TRect;
    function GetWidth: integer;
    procedure MergeWithSelection;
    procedure SetCurrentFilename(AValue: string);
    procedure SetCurrentImageLayer(AValue: TBGRABitmap);
    procedure SetCurrentImageLayerIndex(AValue: integer);
    procedure SetCurrentSelection(const AValue: TBGRABitmap);
    procedure LayeredBitmapReplaced;

  public
    ImageOffset, SelectionOffset: TPoint;
    SelectionRotateCenter: TPointF;
    SelectionRotateAngle: Single;

    function MakeBitmapCopy(backgroundColor: TColor): TBitmap;
    procedure SaveLayerOrSelectionUndo;
    procedure SaveImageUndo;
    function CanUndo: boolean;
    function CanRedo: boolean;
    procedure Undo;
    procedure Redo;
    procedure CompressUndo;
    procedure ImageMayChange(ARect: TRect);
    procedure ImageMayChangeCompletely;
    procedure SelectionMayChange(AOffsetOnly: boolean= false);

    procedure QuerySelection;
    function SelectionEmpty: boolean;
    procedure RemoveSelection;
    procedure EraseSelectionInBitmap;
    procedure ReleaseSelection;
    procedure RetrieveSelection;
    function SelectionLayerIsEmpty: boolean;
    procedure RetrieveSelectionIfLayerEmpty(removeFromBitmap: boolean = false);
    function GetDrawingLayer: TBGRABitmap;
    function GetOrCreateSelectionLayer: TBGRABitmap;
    function GetSelectionLayerIfExists: TBGRABitmap;
    procedure SetSelectionLayer(bmp: TBGRABitmap; AOwned: boolean);

    function ComputeRotatedSelection: TBGRABitmap;
    procedure ApplySelectionTransform(ApplyToMask: boolean= true);
    procedure ApplySelectionMask;
    procedure ApplySmartZoom3;
    procedure Resample(AWidth, AHeight: integer; qualityStr: string);

    procedure SaveToFile(AFilename: string);
    procedure LoadFromFile(AFilename: string);
    procedure SaveToStreamAsLZP(AStream: TStream);
    procedure LoadFromStreamAsLZP(AStream: TStream);
    procedure SetSavedFlag;
    function IsFileModified: boolean;
    function FlatImageEquals(ABitmap: TBGRABitmap): boolean;

    procedure Assign(const AValue: TBGRABitmap; AOwned: boolean); overload;
    procedure Assign(const AValue: TBGRALayeredBitmap; AOwned: boolean); overload;
    procedure Assign(const AValue: TLayeredBitmapAndSelection; AOwned: boolean); overload;
    procedure ReplaceCurrentLayer(AValue: TBGRABitmap; AOwned: boolean);
    procedure Draw(ADest: TBGRABitmap; x,y: integer);
    procedure AddNewLayer;

    property currentFilename: string read GetCurrentFilename write SetCurrentFilename;
    property currentImageLayer: TBGRABitmap read GetCurrentImageLayer write SetCurrentImageLayer;
    property currentImageLayerIndex: integer read GetCurrentImageLayerIndex write SetCurrentImageLayerIndex;
    property currentSelection: TBGRABitmap read GetCurrentSelection write SetCurrentSelection;
    property currentLayeredBitmap: TBGRALayeredBitmap read GetCurrentLayeredBitmap;
    property RenderedImage: TBGRABitmap read GetRenderedImage;
    property Width: integer read GetWidth;
    property Height: integer read GetHeight;
    property OnSelectionChanged: TOnCurrentSelectionChanged read FOnSelectionChanged write FOnSelectionChanged;
    property OnStackChanged: TOnStackChanged read FOnStackChanged write FOnStackChanged;
    property NbLayers: integer read GetNbLayers;
    property Empty: boolean read GetEmpty;
    property SelectionLayerBounds[AOffseted: boolean]: TRect read GetSelectionLayerBounds;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses ugraph, uresourcestrings, dialogs, BGRACompressableBitmap, zstream;

function TLazPaintImage.GetOrCreateSelectionLayer: TBGRABitmap;
begin
    if currentSelection = nil then
      raise Exception.Create(rsNoActiveSelection) else
    begin
      if FCurrentState.selectionLayer = nil then
        FCurrentState.selectionLayer := TBGRABitmap.Create(Width,Height);
      result := FCurrentState.selectionLayer;
    end;
end;

function TLazPaintImage.GetSelectionLayerIfExists: TBGRABitmap;
begin
  result := FCurrentState.selectionLayer;
end;

procedure TLazPaintImage.SetSelectionLayer(bmp: TBGRABitmap; AOwned: boolean);
begin
  if (FCurrentState.currentSelection <> nil) then
  begin
    if AOwned or (bmp= nil) then
    begin
      if (FCurrentState.selectionLayer <> nil) and (FCurrentState.selectionLayer <> bmp) then FreeAndNil(FCurrentState.selectionLayer);
      FCurrentState.selectionLayer := bmp;
    end
    else
    begin
      if FCurrentState.selectionLayer <> nil then FreeAndNil(FCurrentState.selectionLayer);
      FCurrentState.selectionLayer := bmp.Duplicate(True) as TBGRABitmap;
    end;
    ImageMayChangeCompletely;
  end else
    if AOwned and (bmp <>nil) then bmp.Free; //ignore if there is no active selection
end;

function TLazPaintImage.ComputeRotatedSelection: TBGRABitmap;
var temp: TBGRABitmap;
begin
    if currentSelection = nil then result := nil else
    begin
      currentSelection.GrayscaleToAlpha;
      temp := currentSelection.FilterRotate(SelectionRotateCenter,SelectionRotateAngle) as TBGRABitmap;
      currentSelection.AlphaToGrayscale;
      temp.AlphaToGrayscale;
      result := TBGRABitmap.Create(currentSelection.width,currentSelection.Height,BGRABlack) as TBGRABitmap;
      result.PutImage(0,0,temp,dmDrawWithTransparency);
      temp.Free;
    end;
end;

procedure TLazPaintImage.ApplySelectionTransform(ApplyToMask: boolean= true);
var temp: TBGRABitmap;
begin
  if SelectionRotateAngle <> 0 then
  begin
    if ApplyToMask and (currentSelection <> nil) then
      currentSelection := ComputeRotatedSelection;

    if GetSelectionLayerIfExists <> nil then
      SetSelectionLayer(GetSelectionLayerIfExists.FilterRotate(SelectionRotateCenter,SelectionRotateAngle) as TBGRABitmap, True);

    SelectionRotateAngle := 0;
  end;

  if (SelectionOffset.X <> 0) or (SelectionOffset.Y <> 0) then
  begin
    if currentSelection <> nil then
    begin
      temp := TBGRABitmap.Create(Width,Height);
      temp.PutImage(SelectionOffset.X, SelectionOffset.Y, currentSelection, dmSet);
      currentSelection := temp;
    end;
    if GetSelectionLayerIfExists <> nil then
    begin
      temp := TBGRABitmap.Create(Width,Height);
      temp.PutImage(SelectionOffset.X, SelectionOffset.Y, GetSelectionLayerIfExists, dmSet);
      SetSelectionLayer(temp,true);
    end;
    SelectionOffset := point(0,0);
  end;
end;

procedure TLazPaintImage.ApplySelectionMask;
begin
     if (currentSelection <> nil) and (GetSelectionLayerIfExists <> nil) then
     begin
       GetSelectionLayerIfExists.ApplyMask(currentSelection);
       ImageMayChangeCompletely;
     end;
end;

procedure TLazPaintImage.ApplySmartZoom3;
var i: integer;
begin
  for i := 0 to currentLayeredBitmap.NbLayers-1 do
    currentLayeredBitmap.SetLayerBitmap(i, currentLayeredBitmap.LayerBitmap[i].FilterSmartZoom3(moMediumSmooth) as TBGRABitmap, True);
  if currentSelection <> nil then
    currentSelection := currentSelection.FilterSmartZoom3(moMediumSmooth) as TBGRABitmap;
  if GetSelectionLayerIfExists <> nil then
    SetSelectionLayer(GetSelectionLayerIfExists.FilterSmartZoom3(moMediumSmooth) as TBGRABitmap,True);
  ImageMayChangeCompletely;
end;

procedure TLazPaintImage.Resample(AWidth, AHeight: integer; qualityStr: string);
var quality : TResampleMode;
    filter : TResampleFilter;
begin
  if qualityStr = rsFast then
    quality := rmSimpleStretch
  else
    quality := rmFineResample;

  filter := rfLinear;
  if qualityStr = rsLinear then
    filter := rfLinear else
  if qualityStr = rsHalfCosine then
    filter := rfHalfCosine else
  if qualityStr = rsCosine then
    filter := rfCosine else
  if qualityStr = rsMitchell then
    filter := rfMitchell else
  if qualityStr = rsSpline then
    filter := rfSpline else
  if qualityStr = rsBestQuality then
    filter := rfBestQuality;

  currentLayeredBitmap.Resample(AWidth,AHeight,quality,filter);
  if currentSelection <> nil then
  begin
    currentSelection.ResampleFilter := filter;
    currentSelection := currentSelection.Resample(AWidth, AHeight,quality) as TBGRABitmap;
  end;
  if GetSelectionLayerIfExists <> nil then
  begin
    GetSelectionLayerIfExists.ResampleFilter := filter;
    SetSelectionLayer(GetSelectionLayerIfExists.Resample(AWidth, AHeight,quality) as TBGRABitmap,True);
  end;
  ImageMayChangeCompletely;
end;

procedure TLazPaintImage.SaveToFile(AFilename: string);
var s: TFileStream;
  temp: TBGRABitmap;
  p: pbgrapixel;
  n: integer;
begin
  if LowerCase(ExtractFileExt(AFilename))='.lzp' then
  begin
    s := TFileStream.Create(AFilename, fmCreate);
    try
      SaveToStreamAsLZP(s);
    finally
      s.Free;
    end;
  end else
  begin
    if RenderedImage = nil then exit;
    temp := RenderedImage.Duplicate as TBGRABitmap;
    if (LowerCase(ExtractFileExt(AFilename))='.png') and temp.HasTransparentPixels then
    begin
      //avoid png bug with black color
      p := temp.data;
      for n := temp.NbPixels-1 downto 0 do
      begin
        if (p^.alpha <> 0) and (p^.red = 0) and (p^.green = 0) and (p^.blue = 0) then
          p^.blue := 1;
        inc(p);
      end;
    end;
    temp.SaveToFile(AFilename);
    temp.Free;
  end;
  if NbLayers = 1 then SetSavedFlag;
end;

procedure TLazPaintImage.LoadFromFile(AFilename: string);
var s: TFileStream;
  ext: string;
  bmp: TBGRALayeredBitmap;
begin
  ext := LowerCase(ExtractFileExt(AFilename));
  if ext = '.lzp' then
  begin
    s := TFileStream.Create(AFilename, fmOpenRead);
    try
      LoadFromStreamAsLZP(s);
    finally
      s.Free;
    end;
  end else
  begin
    bmp := TBGRALayeredBitmap.Create;
    try
      bmp.LoadFromFile(AFilename);
      Assign(bmp,true);
      bmp := nil;
    finally
      bmp.Free;
    end;
  end;
end;

procedure TLazPaintImage.SaveToStreamAsLZP(AStream: TStream);
var
  comp: TBGRACompressableBitmap;
begin
  if RenderedImage = nil then exit;
  comp := TBGRACompressableBitmap.Create(RenderedImage);
  comp.CompressionLevel := cldefault;
  comp.WriteToStream(AStream);
  comp.Free;
end;

procedure TLazPaintImage.LoadFromStreamAsLZP(AStream: TStream);
var
  comp: TBGRACompressableBitmap;
begin
  RemoveSelection;
  comp := TBGRACompressableBitmap.Create;
  comp.ReadFromStream(AStream);
  Assign(comp.GetBitmap, true);
  comp.Free;
end;

procedure TLazPaintImage.SetSavedFlag;
begin

end;

function TLazPaintImage.IsFileModified: boolean;
begin
  result := true;
end;

function TLazPaintImage.FlatImageEquals(ABitmap: TBGRABitmap): boolean;
begin
  if ABitmap = nil then result := RenderedImage = nil
  else
    result := ABitmap.Equals(RenderedImage);
end;

function TLazPaintImage.GetDrawingLayer: TBGRABitmap;
begin
   if SelectionEmpty then result := currentImageLayer else
     result := GetOrCreateSelectionLayer;
end;

procedure TLazPaintImage.LayeredBitmapReplaced;
var i: integer;
  newbmp: TBGRABitmap;
begin
  FreeAndNil(FRenderedImage);

  for i := 0 to currentLayeredBitmap.NbLayers-1 do
    if (currentLayeredBitmap.LayerBitmap[i].Width <> Width) or
      (currentLayeredBitmap.LayerBitmap[i].Height <> Height) or
      (currentLayeredBitmap.LayerOffset[i].x <> 0) or
      (currentLayeredBitmap.LayerOffset[i].y <> 0) then
    begin
      newbmp := TBGRABitmap.Create(Width,Height);
      newbmp.PutImage(currentLayeredBitmap.LayerOffset[i].x,currentLayeredBitmap.LayerOffset[i].y,currentLayeredBitmap.LayerBitmap[i],dmSet);
      currentLayeredBitmap.SetLayerBitmap(i,newbmp,true);
      currentLayeredBitmap.LayerOffset[i] := Point(0,0);
    end;

  if FCurrentState.currentLayeredBitmap.NbLayers = 0 then
    raise Exception.Create('No layer')
  else
    currentImageLayer := FCurrentState.currentLayeredBitmap.LayerBitmap[0];

  if Assigned(FOnStackChanged) then FOnStackChanged(self,True);
end;

function TLazPaintImage.MakeBitmapCopy(backgroundColor: TColor): TBitmap;
begin
  result := RenderedImage.MakeBitmapCopy(backgroundColor);
end;

procedure TLazPaintImage.SaveLayerOrSelectionUndo;
begin
  //to do
end;

procedure TLazPaintImage.SaveImageUndo;
begin
  //
end;

function TLazPaintImage.CanUndo: boolean;
begin
  result := false;
end;

function TLazPaintImage.CanRedo: boolean;
begin
  result := false;
end;

procedure TLazPaintImage.Undo;
begin

end;

procedure TLazPaintImage.Redo;
begin

end;

procedure TLazPaintImage.CompressUndo;
begin

end;

procedure TLazPaintImage.ImageMayChange(ARect: TRect);
begin
  IntersectRect(ARect, ARect, rect(0,0,Width,Height));
  if IsRectEmpty(ARect) then exit;

  FRenderedImageInvalidated := RectUnion(FRenderedImageInvalidated, ARect);
  FLastSelectionLayerEmptyIsDefined := false;
end;

procedure TLazPaintImage.ImageMayChangeCompletely;
var i,w,h: integer;
begin
  w := 0;
  h := 0;
  for i := 0 to NbLayers-1 do
    with currentLayeredBitmap.LayerBitmap[i] do
    begin
      if Width > w then w := Width;
      if Height > h then h := Height;
    end;
  if (currentLayeredBitmap.width <> w) or (currentLayeredBitmap.height <> h) then
    currentLayeredBitmap.SetSize(w,h);
  ImageMayChange(rect(0,0,Width,Height));
end;

procedure TLazPaintImage.SelectionMayChange(AOffsetOnly: boolean);
begin
  FLastSelectionEmptyIsDefined := false;
  if Assigned(FOnSelectionChanged) then FOnSelectionChanged(self,AOffsetOnly);
end;

{--------------------- Selection --------------------------------------}

procedure TLazPaintImage.QuerySelection;
begin
    if currentSelection = nil then
        currentSelection := TBGRABitmap.Create(Width,Height, BGRABlack);
end;

function TLazPaintImage.SelectionEmpty: boolean;
begin
  if FLastSelectionEmptyIsDefined then
    result := FLastSelectionEmpty
  else
  begin
    result := (currentSelection = nil) or currentSelection.Equals(BGRABlack);
    FLastSelectionEmpty := result;
    FLastSelectionEmptyIsDefined := true;
  end;
end;

function TLazPaintImage.GetHeight: integer;
begin
  result := FCurrentState.currentLayeredBitmap.Height;
end;

function TLazPaintImage.GetCurrentImageLayer: TBGRABitmap;
begin
  result := FCurrentState.currentLayer;
end;

function TLazPaintImage.GetCurrentImageLayerIndex: integer;
begin
  result := FCurrentState.currentLayerIndex;
end;

function TLazPaintImage.GetCurrentLayeredBitmap: TBGRALayeredBitmap;
begin
  result := FCurrentState.currentLayeredBitmap;
end;

function TLazPaintImage.GetCurrentFilename: string;
begin
  result := FCurrentState.filename;
end;

function TLazPaintImage.GetEmpty: boolean;
begin
  result := (NbLayers = 0) or ((NbLayers = 1) and FCurrentState.currentLayeredBitmap.LayerBitmap[0].Empty);
end;

procedure TLazPaintImage.MergeWithSelection;
begin
    if (GetSelectionLayerIfExists <> nil) and (currentSelection <> nil) then
    begin
      GetSelectionLayerIfExists.ApplyMask(currentSelection);
      currentImageLayer.PutImage(0,0,GetSelectionLayerIfExists,dmDrawWithTransparency);
      SetSelectionLayer(nil,true);
      ImageMayChangeCompletely;
    end;
end;

procedure TLazPaintImage.SetCurrentFilename(AValue: string);
begin
  FCurrentState.filename := AValue;
end;

procedure TLazPaintImage.SetCurrentImageLayer(AValue: TBGRABitmap);
var i: integer;
begin
  for i := 0 to currentLayeredBitmap.NbLayers-1 do
    if currentLayeredBitmap.LayerBitmap[i] = AValue then
    begin
      FCurrentState.selectedLayerId := currentLayeredBitmap.LayerUniqueId[i];
      currentLayeredBitmap.Unfreeze(i);
      exit;
    end;
end;

procedure TLazPaintImage.SetCurrentImageLayerIndex(AValue: integer);
begin
  FCurrentState.currentLayerIndex := AValue;
end;

function TLazPaintImage.GetCurrentSelection: TBGRABitmap;
begin
  result := FCurrentState.currentSelection;
end;

function TLazPaintImage.GetNbLayers: integer;
begin
  if FCurrentState.currentLayeredBitmap = nil then
    result := 0
  else
    result := FCurrentState.currentLayeredBitmap.NbLayers;
end;

function TLazPaintImage.GetRenderedImage: TBGRABitmap;
var
  backupCurrentLayer : TBGRABitmap;
  shownSelectionLayer : TBGRABitmap;
begin
  if (FRenderedImage = nil) or ((FRenderedImageInvalidated.Right > FRenderedImageInvalidated.Left) and
     (FRenderedImageInvalidated.Bottom > FRenderedImageInvalidated.Top)) then
  begin
    if currentLayeredBitmap = nil then
    begin
      FreeAndNil(FRenderedImage);
      result := nil;
      exit;
    end;
    currentLayeredBitmap.FreezeExceptOneLayer(currentImageLayerIndex);

    backupCurrentLayer := nil;
    //if there is an overlapping selection, then we must draw it on current layer
    if (currentSelection <> nil) and (currentImageLayer <> nil) then
    begin
      shownSelectionLayer := GetSelectionLayerIfExists;
      if shownSelectionLayer <> nil then
      begin
         shownSelectionLayer := shownSelectionLayer.Duplicate as TBGRABitmap;
         shownSelectionLayer.ApplyMask(currentSelection);
         if not shownSelectionLayer.Empty then
         begin
           backupCurrentLayer := currentImageLayer.Duplicate(True) as TBGRABitmap;
           if SelectionRotateAngle <> 0 then
           begin
             currentImageLayer.PutImageAngle(SelectionOffset.X,SelectionOffset.Y,shownSelectionLayer,SelectionRotateAngle,SelectionRotateCenter.X,SelectionRotateCenter.Y,255,True);
           end else
             currentImageLayer.PutImage(SelectionOffset.X,SelectionOffset.Y,shownSelectionLayer,dmDrawWithTransparency);
           FreeAndNil(shownSelectionLayer);
         end else
         begin
           FreeAndNil(shownSelectionLayer);
           if SelectionLayerIsEmpty then
             SetSelectionLayer(nil,True);
         end;
      end;
    end;

    if (FRenderedImage <> nil) and ((FRenderedImage.Width <> Width) or (FRenderedImage.Height <> Height)) then
      FreeAndNil(FRenderedImage);
    if FRenderedImage = nil then
    begin
      FRenderedImage := currentLayeredBitmap.ComputeFlatImage;
    end else
    begin
      FRenderedImage.ClipRect := FRenderedImageInvalidated;
      FRenderedImage.FillRect(FRenderedImageInvalidated,BGRAPixelTransparent,dmSet);
      currentLayeredBitmap.Draw(FRenderedImage,0,0);
    end;
    FRenderedImageInvalidated := EmptyRect; //up to date

    //restore
    if backupCurrentLayer <> nil then
    begin
      currentImageLayer.PutImage(0,0,backupCurrentLayer,dmSet);
      backupCurrentLayer.Free;
    end;
  end;
  result := FRenderedImage;
end;

function TLazPaintImage.GetSelectionLayerBounds(AOffseted: boolean): TRect;
begin
  if SelectionLayerIsEmpty then
    result := EmptyRect
  else
  begin
    result := GetSelectionLayerIfExists.GetImageBounds;
    if not IsRectEmpty(result) and AOffseted then
      OffsetRect(result,SelectionOffset.X,SelectionOffset.Y);
  end;
end;

function TLazPaintImage.GetWidth: integer;
begin
  result := FCurrentState.currentLayeredBitmap.Width;
end;

procedure TLazPaintImage.Assign(const AValue: TBGRABitmap; AOwned: boolean);
begin
  if FCurrentState.currentLayeredBitmap = nil then
    FCurrentState.currentLayeredBitmap := TBGRALayeredBitmap.Create;

  FCurrentState.currentLayeredBitmap.Clear;
  FCurrentState.currentLayeredBitmap.SetSize(AValue.Width,AValue.Height);
  if AOwned then
    FCurrentState.currentLayeredBitmap.AddOwnedLayer(AValue)
  else
    FCurrentState.currentLayeredBitmap.AddLayer(AValue);
  LayeredBitmapReplaced;
end;

procedure TLazPaintImage.Assign(const AValue: TBGRALayeredBitmap;
  AOwned: boolean);
begin
  if AValue.NbLayers = 0 then
  begin
    if AOwned then AValue.Free;
    exit;
  end;
  if AOwned then
  begin
    FCurrentState.currentLayeredBitmap.Free;
    FCurrentState.currentLayeredBitmap := AValue;
  end else
    FCurrentState.currentLayeredBitmap.Assign(AValue,true);
  FCurrentState.selectedLayerId := FCurrentState.currentLayeredBitmap.LayerUniqueId[0];
  LayeredBitmapReplaced;
end;

procedure TLazPaintImage.Assign(const AValue: TLayeredBitmapAndSelection;
  AOwned: boolean);
begin
  with AValue do
  begin
    Assign(layeredBitmap,AOwned);
    if not AOwned then
      currentSelection := selection.Duplicate(True) as TBGRABitmap
    else
      currentSelection := selection;
    SetSelectionLayer(selectionLayer,AOwned);
  end;
end;

procedure TLazPaintImage.ReplaceCurrentLayer(AValue: TBGRABitmap; AOwned: boolean);
var dest: TBGRABitmap;
begin
  dest := currentImageLayer;
  if dest = nil then exit;
  if (AValue.Width = dest.Width) and (AValue.Height = dest.Height) then
  begin
    ImageMayChange(AValue.GetDifferenceBounds(dest));
    dest.PutImage(0,0,AValue,dmSet);
  end else
  begin
    dest.FillTransparent;
    dest.PutImage((dest.Width-AValue.Width) div 2,(dest.Height-AValue.Height) div 2,AValue,dmSet);
    ImageMayChangeCompletely;
  end;
  if AOwned then AValue.Free;
end;

procedure TLazPaintImage.Draw(ADest: TBGRABitmap; x, y: integer);
var bmp: TBGRABitmap;
begin
  if (NbLayers = 1) and ((currentSelection = nil) or (currentImageLayer = nil)) then
  begin
    if currentLayeredBitmap <> nil then
      currentLayeredBitmap.Draw(ADest,x,y);
  end else
  begin
    bmp := RenderedImage;
    if bmp <> nil then
      if currentLayeredBitmap.LinearBlend then
        ADest.PutImage(x,y,bmp,dmLinearBlend)
      else
        ADest.PutImage(x,y,bmp,dmDrawWithTransparency);
  end;
end;

procedure TLazPaintImage.AddNewLayer;
var bmp: TBGRABitmap;
begin
  if currentLayeredBitmap <> nil then
  begin
    bmp := TBGRABitmap.Create(Width,Height);
    currentLayeredBitmap.AddOwnedLayer(bmp);
    currentImageLayer := bmp;
  end;
end;

procedure TLazPaintImage.SetCurrentSelection(const AValue: TBGRABitmap);
begin
  if FCurrentState.currentSelection = AValue then exit;
  FreeAndNil(FCurrentState.currentSelection);
  FCurrentState.currentSelection := AValue;
  SelectionMayChange;
end;

procedure TLazPaintImage.RemoveSelection;
begin
   if currentSelection <> nil then
   begin
      SetSelectionLayer(nil,true);
      currentSelection := nil;
      SelectionMayChange;
   end;
end;

procedure TLazPaintImage.EraseSelectionInBitmap;
begin
  if currentSelection <> nil then
  begin
    SubstractMask(currentImageLayer,currentSelection);
    ImageMayChange(currentSelection.GetImageBounds(cGreen));
  end;
end;

procedure TLazPaintImage.ReleaseSelection;
begin
   if currentSelection <> nil then
   begin
      ApplySelectionMask;
      currentSelection.Fill(BGRAWhite);
      ApplySelectionTransform(False);
      MergeWithSelection;

      currentSelection := nil;
      SelectionMayChange;
   end;
end;

procedure TLazPaintImage.RetrieveSelection;
begin
    if currentSelection <> nil then
    begin
      MergeWithSelection;
      SetSelectionLayer(currentImageLayer,False);
      GetSelectionLayerIfExists.ApplyMask(currentSelection);
      ImageMayChange(currentSelection.GetImageBounds(cGreen));
    end;
end;

function TLazPaintImage.SelectionLayerIsEmpty: boolean;
begin
  if FLastSelectionLayerEmptyIsDefined then
    result := FLastSelectionLayerEmpty
  else
  begin
    result := (GetSelectionLayerIfExists = nil) or (GetSelectionLayerIfExists.Empty);
    FLastSelectionLayerEmpty := result;
    FLastSelectionLayerEmptyIsDefined := true;
  end;
end;

procedure TLazPaintImage.RetrieveSelectionIfLayerEmpty(removeFromBitmap: boolean = false);
begin
  if SelectionLayerIsEmpty then
  begin
    RetrieveSelection;
    if removeFromBitmap then EraseSelectionInBitmap;
  end;
end;

constructor TLazPaintImage.Create;
begin
  FCurrentState := TImageState.Create;
  FOnSelectionChanged := nil;
  FOnStackChanged := nil;

  //current transform
  ImageOffset := Point(0,0);
  SelectionOffset := Point(0,0);
  SelectionRotateCenter := PointF(0,0);
  SelectionRotateAngle := 0;

  FLastSelectionEmptyIsDefined := false;
  FLastSelectionLayerEmptyIsDefined := false;
end;

destructor TLazPaintImage.Destroy;
begin
  FreeAndNil(FRenderedImage);
  FCurrentState.Free;

  inherited Destroy;
end;

end.
