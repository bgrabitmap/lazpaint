unit UImageAction;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazPaintType, BGRABitmap, UImage, UTool, UScripting,
  ULayerAction, UImageType, BGRABitmapTypes, BGRALayerOriginal, BGRASVGOriginal;

type

  { TImageActions }

  TImageActions = class
  private
    FInstance: TLazPaintCustomInstance;
    function GetCurrentTool: TPaintToolType;
    function GetImage: TLazPaintImage;
    function GetToolManager: TToolManager;
    procedure ChooseTool(ATool: TPaintToolType);
    procedure RegisterScripts(ARegister: Boolean);
    function GenericScriptFunction(AVars: TVariableSet): TScriptResult;
    procedure ReleaseSelection;
  public
    constructor Create(AInstance: TLazPaintCustomInstance);
    destructor Destroy; override;
    procedure ClearAlpha;
    procedure FillBackground;
    function SmartZoom3: boolean;
    procedure Undo;
    procedure Redo;
    procedure SetCurrentBitmap(bmp: TBGRABitmap; AUndoable: boolean;
      ACaption: string = ''; AOpacity: byte = 255);
    procedure CropToSelectionAndLayer;
    procedure CropToSelection;
    procedure HorizontalFlip(AOption: TFlipOption);
    procedure VerticalFlip(AOption: TFlipOption);
    procedure RotateCW;
    procedure RotateCCW;
    procedure LinearNegativeAll;
    procedure NegativeAll;
    procedure SwapRedBlueAll;
    procedure InvertSelection;
    procedure Deselect;
    procedure CopySelection;
    procedure CutSelection;
    procedure RetrieveSelection;
    procedure DeleteSelection;
    procedure RemoveSelection;
    procedure Paste;
    procedure PasteAsNewLayer;
    procedure SelectAll;
    procedure SelectionFit;
    procedure NewLayer; overload;
    function NewLayer(ALayer: TBGRABitmap; AName: string; ABlendOp: TBlendOperation): boolean; overload;
    function NewLayer(ALayer: TBGRALayerCustomOriginal; AName: string; ABlendOp: TBlendOperation; AMatrix: TAffineMatrix): boolean; overload;
    procedure DuplicateLayer;
    procedure RasterizeLayer;
    procedure MergeLayerOver;
    procedure RemoveLayer;
    procedure EditSelection(ACallback: TModifyImageCallback);
    procedure Import3DObject(AFilenameUTF8: string);
    function TryAddLayerFromFile(AFilenameUTF8: string; ALoadedImage: TBGRABitmap = nil): boolean;
    function AddLayerFromBitmap(ABitmap: TBGRABitmap; AName: string): boolean;
    function AddLayerFromOriginal(AOriginal: TBGRALayerCustomOriginal; AName: string): boolean;
    function AddLayerFromOriginal(AOriginal: TBGRALayerCustomOriginal; AName: string; AMatrix: TAffineMatrix): boolean;
    function LoadSelection(AFilenameUTF8: string; ALoadedImage: PImageEntry = nil): boolean;
    property Image: TLazPaintImage read GetImage;
    property ToolManager: TToolManager read GetToolManager;
    property CurrentTool: TPaintToolType read GetCurrentTool;
  end;

implementation

uses Controls, Dialogs, UResourceStrings, UObject3D,
     ULoadImage, UGraph, UClipboard, Types, BGRAGradientOriginal,
     BGRATransform, ULoading, math, LCVectorClipboard;

{ TImageActions }

function TImageActions.GetImage: TLazPaintImage;
begin
  result := FInstance.Image;
end;

function TImageActions.GetCurrentTool: TPaintToolType;
begin
  if FInstance.ToolManager.CurrentTool = nil then
    result := ptHand
  else
    result := FInstance.ToolManager.GetCurrentToolType;
end;

function TImageActions.GetToolManager: TToolManager;
begin
  result := FInstance.ToolManager;
end;

procedure TImageActions.ChooseTool(ATool: TPaintToolType);
begin
  FInstance.ChooseTool(ATool);
end;

procedure TImageActions.RegisterScripts(ARegister: Boolean);
var Scripting: TScriptContext;
begin
  Scripting := FInstance.ScriptContext;
  Scripting.RegisterScriptFunction('ImageHorizontalFlip',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('ImageVerticalFlip',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('SelectionHorizontalFlip',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('SelectionVerticalFlip',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('ImageSmartZoom3',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('ImageCropLayer',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('ImageClearAlpha',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('ImageCrop',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('ImageFillBackground',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('ImageRotateCW',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('ImageRotateCCW',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('ImageLinearNegative',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('ImageNegative',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('ImageSwapRedBlue',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('EditUndo',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('EditRedo',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('EditInvertSelection',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('EditDeselect',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('EditCopy',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('EditCut',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('EditDeleteSelection',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('EditPaste',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('EditPasteAsNewLayer',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('EditSelectAll',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('EditSelectionFit',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('LayerHorizontalFlip',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('LayerVerticalFlip',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('LayerAddNew',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('LayerDuplicate',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('LayerRasterize',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('LayerMergeOver',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('LayerRemoveCurrent',@GenericScriptFunction,ARegister);
end;

constructor TImageActions.Create(AInstance: TLazPaintCustomInstance);
begin
  FInstance := AInstance;
  RegisterScripts(True);
end;

destructor TImageActions.Destroy;
begin
  RegisterScripts(False);
  inherited Destroy;
end;

function TImageActions.GenericScriptFunction(AVars: TVariableSet): TScriptResult;
var f: string;
begin
  result := srOk;
  f := AVars.FunctionName;
  //for script purposes, Image always means the whole picture and Selection the selection
  if f = 'ImageHorizontalFlip' then HorizontalFlip(foWholePicture) else
  if f = 'ImageVerticalFlip' then VerticalFlip(foWholePicture) else
  if f = 'SelectionHorizontalFlip' then HorizontalFlip(foSelection) else
  if f = 'SelectionVerticalFlip' then VerticalFlip(foSelection) else

  //those script functions are the same as the menu actions
  if f = 'ImageSmartZoom3' then SmartZoom3 else
  if f = 'ImageCropLayer' then CropToSelectionAndLayer else
  if f = 'ImageClearAlpha' then ClearAlpha else
  if f = 'ImageCrop' then CropToSelection else
  if f = 'ImageFillBackground' then FillBackground else
  if f = 'ImageRotateCW' then RotateCW else
  if f = 'ImageRotateCCW' then RotateCCW else
  if f = 'ImageLinearNegative' then LinearNegativeAll else
  if f = 'ImageNegative' then NegativeAll else
  if f = 'ImageSwapRedBlue' then SwapRedBlueAll else
  if f = 'EditUndo' then Undo else
  if f = 'EditRedo' then Redo else
  if f = 'EditInvertSelection' then InvertSelection else
  if f = 'EditDeselect' then Deselect else
  if f = 'EditCopy' then CopySelection else
  if f = 'EditCut' then CutSelection else
  if f = 'EditDeleteSelection' then DeleteSelection else
  if f = 'EditPaste' then Paste else
  if f = 'EditPasteAsNewLayer' then PasteAsNewLayer else
  if f = 'EditSelectAll' then SelectAll else
  if f = 'EditSelectionFit' then SelectionFit else
  if f = 'LayerHorizontalFlip' then HorizontalFlip(foCurrentLayer) else
  if f = 'LayerVerticalFlip' then VerticalFlip(foCurrentLayer) else
  if f = 'LayerAddNew' then NewLayer else
  if f = 'LayerDuplicate' then DuplicateLayer else
  if f = 'LayerRasterize' then RasterizeLayer else
  if f = 'LayerMergeOver' then MergeLayerOver else
  if f = 'LayerRemoveCurrent' then RemoveLayer else
    result := srFunctionNotDefined;
end;

procedure TImageActions.ClearAlpha;
var c: TBGRAPixel;
    n: integer;
    p: PBGRAPixel;
    LayerAction: TLayerAction;
begin
  if not Image.CheckNoAction then exit;
  LayerAction := nil;
  try
    c := ToolManager.BackColor;
    c.alpha := 255;
    LayerAction := Image.CreateAction(true);
    LayerAction.SelectedImageLayer.ReplaceColor(BGRAPixelTransparent,c);
    p := LayerAction.SelectedImageLayer.Data;
    for n := LayerAction.SelectedImageLayer.NbPixels-1 downto 0 do
    begin
       p^.alpha := 255;
       inc(p);
    end;
    LayerAction.SelectedImageLayer.InvalidateBitmap;
    Image.LayerMayChangeCompletely(LayerAction.SelectedImageLayer);
    LayerAction.Validate;
  except
    on ex:Exception do
      FInstance.ShowError('ClearAlpha',ex.Message);
  end;
  LayerAction.Free;
end;

procedure TImageActions.FillBackground;
var tempBmp: TBGRABitmap;
    c: TBGRAPixel;
    LayerAction: TLayerAction;
    y: Integer;
begin
  if not Image.CheckNoAction then exit;
  LayerAction := nil;
  try
    c := ToolManager.BackColor;
    c.alpha := 255;
    LayerAction := Image.CreateAction(True);
    tempBmp := TBGRABitmap.Create(LayerAction.SelectedImageLayer.Width,1);
    for y := 0 to LayerAction.SelectedImageLayer.Height-1 do
    begin
       tempBmp.Fill(c);
       tempBmp.PutImage(0,-y,LayerAction.SelectedImageLayer,dmDrawWithTransparency);
       LayerAction.SelectedImageLayer.PutImage(0,y,tempBmp,dmSet);
    end;
    tempBmp.Free;
    image.LayerMayChangeCompletely(LayerAction.SelectedImageLayer);
    LayerAction.Validate;
  except
    on ex:Exception do
      FInstance.ShowError('FillBackground',ex.Message);
  end;
  LayerAction.Free;
end;

function TImageActions.SmartZoom3: boolean;
begin
  result := false;
  if (image.Width * 3 > MaxImageWidth) or
    (image.Height * 3 > MaxImageHeight) then
  begin
    FInstance.ShowMessage(rsLazPaint,rsImageTooBig);
    exit;
  end;
  ChooseTool(ptHand);
  try
    image.ApplySmartZoom3;
    result := true;
  except
    on ex:Exception do
      FInstance.ShowError('SmartZoom3',ex.Message);
  end;
end;

procedure TImageActions.Undo;
begin
  try
    if CurrentTool in[ptMoveSelection,ptRotateSelection] then ChooseTool(ptHand);
    if ToolManager.ToolProvideCommand(tcFinish) then ToolManager.ToolCommand(tcFinish);
    if image.CanUndo then
    begin
      ToolManager.ToolCloseDontReopen;
      image.Undo;
      ToolManager.ToolOpen;
    end;
  except
    on ex:Exception do
      FInstance.ShowError('Undo',ex.Message);
  end;
end;

procedure TImageActions.Redo;
begin
  try
    if CurrentTool in[ptTextureMapping,ptLayerMapping,ptMoveSelection,ptRotateSelection] then
      ChooseTool(ptHand);
    if image.CanRedo then
    begin
      ToolManager.ToolCloseDontReopen;
      image.Redo;
      ToolManager.ToolOpen;
    end;
  except
    on ex:Exception do
      FInstance.ShowError('Redo',ex.Message);
  end;
end;

procedure TImageActions.Import3DObject(AFilenameUTF8: string);
var image3D: TBGRABitmap;
begin
  try
    image3D := ShowObject3DDlg(FInstance, AFileNameUTF8, Image.Width, Image.Height);
    if image3D <> nil then
    begin
      if image3D.NbPixels <> 0 then
        NewLayer(image3d, ExtractFileName(AFilenameUTF8), boTransparent)
      else
        image3D.Free;
    end;
  except
    on ex:Exception do
      FInstance.ShowError('Import3DObject',ex.Message);
  end;
end;

function TImageActions.LoadSelection(AFilenameUTF8: string; ALoadedImage: PImageEntry = nil): boolean;
var
  newSelection: TBGRABitmap;
  LayerAction: TLayerAction;
begin
  LayerAction := nil;
  result := false;
  try
    if Assigned(ALoadedImage) and Assigned(ALoadedImage^.bmp) then
    begin
      newSelection := ALoadedImage^.bmp;
      ALoadedImage^.FreeAndNil;
    end
    else
      newSelection := LoadFlatImageUTF8(AFilenameUTF8).bmp;
    newSelection.InplaceGrayscale;
    if not (CurrentTool in[ptDeformation,ptTextureMapping,ptLayerMapping,ptMoveSelection,ptRotateSelection]) then
      ChooseTool(ptMoveSelection);

    if Image.CheckNoAction then
    begin
      LayerAction := Image.CreateAction;
      LayerAction.RemoveSelection;
      LayerAction.QuerySelection;
      LayerAction.CurrentSelection.PutImage(0,0,newSelection,dmSet);
      LayerAction.NotifyChange(Image.SelectionMask,Image.SelectionMaskBounds);
      LayerAction.Validate;
      result := true;
    end;
  except
    on ex: exception do
      FInstance.ShowError('LoadSelection',ex.Message);
  end;
  FreeAndNil(newSelection);
  LayerAction.Free;
end;

procedure TImageActions.CropToSelectionAndLayer;
var partial: TBGRABitmap; r: TRect; top: TTopMostInfo;
begin
  if not image.CheckNoAction then exit;
  if not image.CheckCurrentLayerVisible then exit;
  try
    if image.SelectionMaskEmpty then
    begin
      FInstance.ShowMessage(rsCrop, rsEmptySelection);
      exit;
    end;
    if (CurrentTool in[ptRotateSelection,ptMoveSelection,ptDeformation,ptTextureMapping,ptLayerMapping]) then
      ChooseTool(ptHand);
    partial := image.MakeCroppedLayer;
    if partial <> nil then
    begin
      r := partial.GetImageBounds;
      if (r.right > r.left) and (r.bottom > r.top) then
      begin
        if (r.left <> 0) or (r.top <> 0) or
          (r.right <> partial.width) or (r.bottom <> partial.height) then
        begin
          top := FInstance.HideTopmost;
          case MessageDlg(rsCrop,rsKeepEmptySpace,mtConfirmation,mbYesNo,0) of
          mrNo: BGRAReplace(partial, partial.GetPart(r));
          end;
          FInstance.ShowTopmost(top);
        end;
        SetCurrentBitmap(partial,true,image.LayerName[image.CurrentLayerIndex],image.LayerOpacity[image.CurrentLayerIndex]);
      end
      else
        partial.Free;
    end;
  except
    on ex:Exception do
      FInstance.ShowError('CropToSelectionAndLayer',ex.Message);
  end;
end;

procedure TImageActions.CropToSelection;
var cropped: TLayeredBitmapAndSelection;
    r, subBounds: TRect;
    i,selectedLayer: integer;
    ofs: TPoint;
    tempLayer, flattened: TBGRABitmap;
    selectionIsRect: Boolean;
    top: TTopMostInfo;
begin
  if not image.CheckNoAction then exit;
  try
    if image.SelectionMaskEmpty then
    begin
      FInstance.ShowMessage(rsCrop,rsEmptySelection);
      exit;
    end;
    if not image.SelectionMaskEmpty then
    begin
      r := image.SelectionMaskBounds;
      if (r.left = 0) and (r.Top = 0) and (r.right = image.width) and (r.Bottom =image.height) then exit;
      cropped := image.MakeLayeredBitmapAndSelectionCopy;
      BGRAReplace(cropped.selection,cropped.selection.GetPart(r));
      selectionIsRect := cropped.selection.Equals(BGRAWhite);
      if cropped.selectionLayer <> nil then BGRAReplace(cropped.selectionLayer,cropped.selectionLayer.GetPart(r));
      selectedLayer := image.CurrentLayerIndex;
      for i := 0 to cropped.layeredBitmap.NbLayers-1 do
      begin
        tempLayer := TBGRABitmap.Create(r.Width,r.Height);
        if selectionIsRect and (cropped.layeredBitmap.LayerOriginalGuid[i]<>GUID_NULL) and
          cropped.layeredBitmap.LayerOriginalKnown[i] then
        begin
          ofs := cropped.layeredBitmap.LayerOffset[i];
          cropped.layeredBitmap.LayerOriginalMatrix[i] :=
             AffineMatrixTranslation(-r.Left, -r.Top)*
             cropped.layeredBitmap.LayerOriginalMatrix[i];
          cropped.layeredBitmap.RenderLayerFromOriginal(i);
        end else
        begin
          ofs := cropped.layeredBitmap.LayerOffset[i];
          tempLayer.PutImage(ofs.x-r.Left,ofs.y-r.Top, cropped.layeredBitmap.LayerBitmap[i], dmSet);
          tempLayer.ApplyMask(cropped.selection);
          cropped.layeredBitmap.SetLayerBitmap(i, tempLayer, true);
          cropped.layeredBitmap.LayerOffset[i] := Point(0,0);
        end;
      end;
      if cropped.selectionLayer = nil then
      begin
        FreeAndNil(cropped.selection);
        if (CurrentTool in [ptMoveSelection,ptRotateSelection]) then
          ChooseTool(ptHand);
      end;
      cropped.layeredBitmap.SetSize(r.right-r.left,r.Bottom-r.top);
      cropped.layeredBitmap.RemoveUnusedOriginals;
      flattened := cropped.layeredBitmap.ComputeFlatImage;
      subBounds := flattened.GetImageBounds;
      flattened.Free;
      if cropped.selectionLayer<>nil then
        subBounds := RectUnion(subBounds, cropped.selectionLayer.GetImageBounds);
      if (subBounds.Left > 0) or (subBounds.Top > 0) or
        (subBounds.Right < cropped.layeredBitmap.Width) or (subBounds.Bottom < cropped.layeredBitmap.Height) then
      begin
        top := FInstance.HideTopmost;
        case MessageDlg(rsCrop,rsKeepEmptySpace,mtConfirmation,mbYesNo,0) of
        mrNo: begin
            for i := 0 to cropped.layeredBitmap.NbLayers-1 do
            begin
              if cropped.layeredBitmap.LayerOriginalGuid[i]=GUID_NULL then
              begin
                ofs := cropped.layeredBitmap.LayerOffset[i];
                cropped.layeredBitmap.LayerOffset[i] := Point(ofs.x-subBounds.Left,ofs.y-subBounds.Top);
              end else
              begin
                cropped.layeredBitmap.LayerOriginalMatrix[i] :=
                  AffineMatrixTranslation(-subBounds.Left,-subBounds.Top)*
                  cropped.layeredBitmap.LayerOriginalMatrix[i];
                cropped.layeredBitmap.RenderLayerFromOriginal(i);
              end;
            end;
            cropped.layeredBitmap.SetSize(subBounds.Width, subBounds.Height);
          end;
        end;
        FInstance.ShowTopmost(top);
      end;
      image.Assign(cropped,true,true);
      image.SetCurrentLayerByIndex(selectedLayer);
    end;
  except
    on ex:Exception do
      FInstance.ShowError('CropToSelection',ex.Message);
  end;
end;

procedure TImageActions.SetCurrentBitmap(bmp: TBGRABitmap; AUndoable : boolean;
  ACaption: string; AOpacity: byte);
begin
  ToolManager.ToolCloseDontReopen;
  try
    image.Assign(bmp,True,AUndoable, ACaption,AOpacity);
  finally
    ToolManager.ToolOpen;
  end;
end;

function TImageActions.TryAddLayerFromFile(AFilenameUTF8: string; ALoadedImage: TBGRABitmap = nil): boolean;
var
  newPicture: TBGRABitmap;
  svgOrig: TBGRALayerSVGOriginal;
  ratio: Single;
  m: TAffineMatrix;
begin
  result := false;
  if not AbleToLoadUTF8(AFilenameUTF8) then
  begin
    FInstance.ShowMessage(rsOpen,rsFileExtensionNotSupported);
    FreeAndNil(ALoadedImage);
    exit;
  end;
  try
    if Image.DetectImageFormat(AFilenameUTF8) = ifSvg then
    begin
      svgOrig := LoadSVGOriginalUTF8(AFilenameUTF8);
      ratio := max(svgOrig.Width/Image.Width, svgOrig.Height/Image.Height);
      m := AffineMatrixTranslation(-svgOrig.Width/2,-svgOrig.Height/2);
      if ratio > 1 then m := AffineMatrixScale(1/ratio,1/ratio)*m;
      m := AffineMatrixTranslation(Image.Width/2,Image.Height/2)*m;
      AddLayerFromOriginal(svgOrig, ExtractFileName(AFilenameUTF8), m);
      FreeAndNil(ALoadedImage);
    end else
    begin
      if Assigned(ALoadedImage) then
      begin
        newPicture := ALoadedImage;
        ALoadedImage := nil;
      end
      else
        newPicture := LoadFlatImageUTF8(AFilenameUTF8).bmp;
      AddLayerFromBitmap(newPicture, ExtractFileName(AFilenameUTF8));
    end;

  except
    on ex: Exception do
    begin
      ALoadedImage.Free;
      FInstance.ShowError('TryAddLayerFromFile',ex.Message);
    end;
  end;
end;

function TImageActions.AddLayerFromBitmap(ABitmap: TBGRABitmap; AName: string): boolean;
var
  ratio: single;
  xorMask: TBGRABitmap;
begin
  if (ABitmap <> nil) and (ABitmap.Width > 0) and (ABitmap.Height > 0) then
  begin
    if CurrentTool in [ptDeformation,ptRotateSelection,ptMoveSelection,ptTextureMapping,
         ptLayerMapping,ptEditShape] then
      ChooseTool(ptHand);
    if image.CheckNoAction then
    begin
      if not Image.SelectionMaskEmpty then ReleaseSelection;
      if (ABitmap.Width > Image.Width) or (ABitmap.Height > Image.Height) then
      begin
        ratio := 1;
        if ABitmap.Width > Image.Width then ratio := Image.Width/ABitmap.Width;
        if ABitmap.Height*ratio > Image.Height then ratio := Image.Height/ABitmap.Height;
        ABitmap.ResampleFilter := rfBestQuality;
        BGRAReplace(ABitmap, ABitmap.Resample(round(ABitmap.Width*ratio),round(ABitmap.Height*ratio)));
      end;
      if Assigned(ABitmap.XorMask) then
      begin
        xorMask := ABitmap.XorMask.Duplicate as TBGRABitmap;
        xorMask.AlphaFill(255);
        xorMask.ReplaceColor(BGRABlack,BGRAPixelTransparent);
        ABitmap.DiscardXorMask;
      end
      else
        xorMask := nil;
      if NewLayer(ABitmap, AName, boTransparent) then
      begin
        if Assigned(xorMask) then
          result := NewLayer(xorMask, AName + ' (xor)', boXor)
        else
          result := true;
      end else
      begin
        xorMask.Free;
        result := false;
      end;
    end else
    begin
      ABitmap.Free;
      result := false;
    end;
  end else
  begin
    ABitmap.Free;
    result := false;
  end;
end;

function TImageActions.AddLayerFromOriginal(
  AOriginal: TBGRALayerCustomOriginal; AName: string): boolean;
begin
  result := AddLayerFromOriginal(AOriginal,AName,AffineMatrixIdentity);
end;

function TImageActions.AddLayerFromOriginal(AOriginal: TBGRALayerCustomOriginal;
  AName: string; AMatrix: TAffineMatrix): boolean;
begin
  if AOriginal <> nil then
  begin
    if CurrentTool in [ptDeformation,ptRotateSelection,ptMoveSelection,ptTextureMapping,
         ptLayerMapping,ptEditShape] then
      ChooseTool(ptHand);
    if image.CheckNoAction then
    begin
      if not Image.SelectionMaskEmpty then ReleaseSelection;
      result := NewLayer(AOriginal, AName, boTransparent, AMatrix);
    end else
    begin
      AOriginal.Free;
      result := false;
    end;
  end else
  begin
    AOriginal.Free;
    result := false;
  end;
end;

procedure TImageActions.HorizontalFlip(AOption: TFlipOption);
begin
  try
    if (AOption = foCurrentLayer) then
      image.HorizontalFlip(Image.CurrentLayerIndex) else
    if ((AOption = foAuto) and not image.SelectionMaskEmpty) or (AOption = foSelection) then
    begin
      if not image.SelectionMaskEmpty then
      begin
        ChooseTool(ptMoveSelection);
        if not Image.CheckNoAction then exit;
        Image.SelectionTransform := AffineMatrixTranslation(+Image.Width/2,0)*AffineMatrixScale(-1,1)*AffineMatrixTranslation(-Image.Width/2,0)*Image.SelectionTransform;
      end else
        exit;
    end else
    if ((AOption = foAuto) and image.SelectionMaskEmpty) or (AOption = foWholePicture) then
      image.HorizontalFlip;
  except
    on ex:Exception do
      FInstance.ShowError('HorizontalFlip',ex.Message);
  end;
end;

procedure TImageActions.VerticalFlip(AOption: TFlipOption);
begin
  try
    if (AOption = foCurrentLayer) then
      image.VerticalFlip(Image.CurrentLayerIndex) else
    if ((AOption = foAuto) and not image.SelectionMaskEmpty) or (AOption = foSelection) then
    begin
      if not image.SelectionMaskEmpty then
      begin
        ChooseTool(ptMoveSelection);
        if not Image.CheckNoAction then exit;
        Image.SelectionTransform := AffineMatrixTranslation(0,+Image.Height/2)*AffineMatrixScale(1,-1)*AffineMatrixTranslation(0,-Image.Height/2)*Image.SelectionTransform;
      end else
        exit;
    end else
    if ((AOption = foAuto) and image.SelectionMaskEmpty) or (AOption = foWholePicture) then
      image.VerticalFlip;
  except
    on ex:Exception do
      FInstance.ShowError('VerticalFlip',ex.Message);
  end;
end;

procedure TImageActions.RotateCW;
begin
  Image.RotateCW;
end;

procedure TImageActions.RotateCCW;
begin
  Image.RotateCCW;
end;

procedure TImageActions.LinearNegativeAll;
begin
  Image.LinearNegativeAll;
end;

procedure TImageActions.NegativeAll;
begin
  Image.NegativeAll;
end;

procedure TImageActions.SwapRedBlueAll;
begin
  Image.SwapRedBlue;
end;

procedure TImageActions.InvertSelection;
var LayerAction: TLayerAction;
    p : PBGRAPixel;
    n: integer;
begin
  LayerAction := nil;
  try
    LayerAction := Image.CreateAction(false,true);
    LayerAction.QuerySelection;
    p := LayerAction.CurrentSelection.Data;
    for n := LayerAction.CurrentSelection.NbPixels-1 downto 0 do
    begin
      if p^.alpha <> 255 then p^ := BGRABlack;
      inc(p);
    end;
    LayerAction.CurrentSelection.InvalidateBitmap;
    LayerAction.CurrentSelection.LinearNegative;
    LayerAction.Validate;
    Image.SelectionMaskMayChangeCompletely;
  except
    on ex:Exception do
      FInstance.ShowError('InvertSelection',ex.Message);
  end;
  LayerAction.Free;
  if Image.SelectionMaskEmpty then ChooseTool(ptHand) else
  if not (CurrentTool in[ptSelectRect,ptSelectEllipse]) then ChooseTool(ptSelectRect);
end;

procedure TImageActions.Deselect;
begin
  if (CurrentTool in[ptRotateSelection,ptMoveSelection]) then
    ChooseTool(ptHand);
  if not Image.CheckNoAction then exit;
  try
    if not image.SelectionMaskEmpty then ReleaseSelection;
  except
    on ex:Exception do
      FInstance.ShowError('Deselect',ex.Message);
  end;
end;

procedure TImageActions.CopySelection;
var layer, partial : TBGRABitmap; r: TRect;
    LayerAction: TLayerAction;
    bounds: TRect;
begin
  LayerAction := nil;
  try
    if not image.CheckNoAction then exit;
    bounds := Image.SelectionMaskBounds;
    if IsRectEmpty(bounds) then exit;
    LayerAction := Image.CreateAction(true,true);
    LayerAction.ApplySelectionMask;
    if Image.SelectionLayerIsEmpty then LayerAction.RetrieveSelection;
    layer := LayerAction.GetOrCreateSelectionLayer;
    r := layer.GetImageBounds; //bounds may have been changed
    if (r.right > r.left) and (r.bottom > r.top) then
    begin
      partial := layer.GetPart(r) as TBGRABitmap;
      CopyToClipboard(partial);
      partial.Free;
    end;
    FreeAndNil(LayerAction);
  except
    on ex:Exception do
    begin
      FreeAndNil(LayerAction);
      FInstance.ShowError('CopySelection',ex.Message);
    end;
  end;
end;

procedure TImageActions.CutSelection;
var LayerAction: TLayerAction;
begin
  if image.SelectionMaskEmpty then exit;
  if not image.CheckNoAction then exit;
  LayerAction := nil;
  try
    CopySelection;
    LayerAction := Image.CreateAction(false,true);
    if (LayerAction.GetSelectionLayerIfExists = nil) or (LayerAction.GetSelectionLayerIfExists.Empty) then
      LayerAction.EraseSelectionInBitmap;
    LayerAction.RemoveSelection;
    LayerAction.Validate;
  except
    on ex:Exception do
      FInstance.ShowError('CutSelection',ex.Message);
  end;
  LayerAction.Free;
  if (CurrentTool = ptRotateSelection) or
     (CurrentTool = ptMoveSelection) then
    ChooseTool(ptHand);
end;

procedure TImageActions.RetrieveSelection;
var LayerAction: TLayerAction;
  r: TRect;
begin
  if image.SelectionMaskEmpty then exit;
  if not image.CheckNoAction then exit;
  LayerAction := nil;
  try
    LayerAction := Image.CreateAction(false, true);
    if LayerAction.RetrieveSelectionIfLayerEmpty(True) then
    begin
      r := Image.SelectionMaskBounds;
      ComputeSelectionMask(LayerAction.GetOrCreateSelectionLayer,LayerAction.CurrentSelection,r);
      LayerAction.NotifyChange(LayerAction.GetOrCreateSelectionLayer, r);
      LayerAction.NotifyChange(LayerAction.CurrentSelection, r);
      LayerAction.Validate;
    end;
    if image.SelectionLayerIsEmpty then MessagePopup(rsNothingToBeRetrieved,2000);
  except on ex:exception do FInstance.ShowError('RetrieveSelection',ex.Message);
  end;
  LayerAction.Free;
end;

procedure TImageActions.DeleteSelection;
var LayerAction: TLayerAction;
  doErase: Boolean;
begin
  if image.SelectionMaskEmpty then exit;
  if not image.CheckNoAction then exit;
  LayerAction := nil;
  try
    doErase := Image.SelectionLayerIsEmpty;
    LayerAction := Image.CreateAction(false, doErase);
    if doErase then LayerAction.EraseSelectionInBitmap;
    LayerAction.RemoveSelection;
    LayerAction.Validate;
  except
    on ex:Exception do
      FInstance.ShowError('DeleteSelection',ex.Message);
  end;
  LayerAction.Free;
  if (CurrentTool = ptRotateSelection) or
     (CurrentTool = ptMoveSelection) then
    ChooseTool(ptHand);
end;

procedure TImageActions.RemoveSelection;
var LayerAction: TLayerAction;
begin
  if image.SelectionMaskEmpty then exit;
  if not image.CheckNoAction then exit;
  LayerAction := nil;
  try
    LayerAction := Image.CreateAction;
    LayerAction.RemoveSelection;
    LayerAction.Validate;
  except on ex:exception do FInstance.ShowError('RemoveSelection',ex.Message);
  end;
  LayerAction.Free;
  if (CurrentTool = ptRotateSelection) or
     (CurrentTool = ptMoveSelection) then
    ChooseTool(ptHand);
end;

procedure TImageActions.ReleaseSelection;
var
  layeraction: TLayerAction;
begin
  if image.SelectionMaskEmpty then exit;
  layeraction := image.CreateAction(true, true);
  layeraction.ReleaseSelection;
  layeraction.Validate;
  layeraction.Free;
end;

procedure TImageActions.Paste;
var partial: TBGRABitmap;
    layeraction: TLayerAction;
    pastePos: TPoint;
begin
  try
    if ClipboardHasShapes then
    begin
      ChooseTool(ptEditShape);
      ToolManager.ToolCommand(tcPaste);
    end else
    begin
      partial := GetBitmapFromClipboard;
      if partial<>nil then
      begin
        if partial.NbPixels <> 0 then
        begin
          ToolManager.ToolCloseDontReopen;
          layeraction := Image.CreateAction(true, true);
          layeraction.ReleaseSelection;
          layeraction.QuerySelection;
          pastePos := Point((image.Width - partial.Width) div 2 - image.ImageOffset.X,
             (image.Height - partial.Height) div 2 - image.ImageOffset.Y);
          if pastePos.x+partial.width > image.width then pastePos.x := image.width-partial.width;
          if pastePos.y+partial.Height > image.Height then pastePos.y := image.Height-partial.Height;
          if pastePos.x < 0 then pastePos.x := 0;
          if pastePos.y < 0 then pastePos.y := 0;
          layeraction.GetOrCreateSelectionLayer.PutImage(pastePos.x,pastePos.y,partial,dmFastBlend);
          ComputeSelectionMask(layeraction.GetOrCreateSelectionLayer,layeraction.currentSelection,
            rect(pastePos.x,pastePos.y,pastePos.x+partial.Width,pastePos.y+partial.Height));
          Image.SelectionMaskMayChange(rect(pastePos.x,pastePos.y,pastePos.x+partial.Width,pastePos.y+partial.Height));
          layeraction.Validate;
          layeraction.Free;
          ChooseTool(ptMoveSelection);
        end;
        partial.Free;
      end;
    end;
  except
    on ex:Exception do
      FInstance.ShowError('Paste',ex.Message);
  end;
end;

procedure TImageActions.PasteAsNewLayer;
var partial: TBGRABitmap;
begin
  try
    partial := GetBitmapFromClipboard;
    if partial<>nil then
    begin
      if partial.NbPixels <> 0 then
      begin
        AddLayerFromBitmap(partial,'');
        ChooseTool(ptMoveLayer);
      end
      else
        partial.Free;
    end;
  except
    on ex:Exception do
      FInstance.ShowError('Paste',ex.Message);
  end;
end;

procedure TImageActions.SelectAll;
var LayerAction : TLayerAction;
begin
  try
    LayerAction := Image.CreateAction;
    LayerAction.QuerySelection;
    LayerAction.currentSelection.Fill(BGRAWhite);
    Image.SelectionMaskMayChangeCompletely;
    LayerAction.Validate;
    LayerAction.Free;
    if not ToolManager.IsSelectingTool then ChooseTool(ptSelectRect);
  except
    on ex:Exception do
      FInstance.ShowError('SelectAll',ex.Message);
  end;
end;

procedure TImageActions.SelectionFit;
var LayerAction: TLayerAction;
    bounds: TRect;
begin
  if not image.CheckNoAction then exit;
  try
    LayerAction := Image.CreateAction(false,true);
    LayerAction.ChangeBoundsNotified := true;

    if image.SelectionMaskEmpty then
    begin
      bounds := rect(0,0,Image.width,image.height);
      LayerAction.QuerySelection;
      LayerAction.currentSelection.Fill(BGRAWhite);
      LayerAction.NotifyChange(LayerAction.currentSelection, bounds);
      Image.SelectionMaskMayChange(bounds);
    end else
    begin
      bounds := image.SelectionLayerBounds;
      Image.SelectionMaskMayChange(bounds);
      LayerAction.ApplySelectionMask;
      LayerAction.NotifyChange(LayerAction.GetSelectionLayerIfExists, bounds);
      bounds := image.SelectionMaskBounds;
      Image.SelectionMaskMayChange(bounds);
    end;

    if LayerAction.RetrieveSelectionIfLayerEmpty(True) then
    begin
      LayerAction.NotifyChange(LayerAction.GetSelectionLayerIfExists, bounds);
      LayerAction.NotifyChange(LayerAction.SelectedImageLayer, bounds);
    end;
    ComputeSelectionMask(LayerAction.GetOrCreateSelectionLayer,LayerAction.currentSelection,bounds);
    LayerAction.NotifyChange(LayerAction.CurrentSelection, bounds);
    LayerAction.NotifyChange(LayerAction.GetOrCreateSelectionLayer, bounds);
    LayerAction.Validate;
    LayerAction.Free;
    if image.SelectionMaskEmpty then
    begin
      if (CurrentTool = ptRotateSelection) or
         (CurrentTool = ptMoveSelection) then
        ChooseTool(ptHand);
    end else
      if not ToolManager.IsSelectingTool then ChooseTool(ptMoveSelection);
  except
    on ex:Exception do
      FInstance.ShowError('SelectionFit',ex.Message);
  end;
end;

procedure TImageActions.NewLayer;
{var top: TTopMostInfo;
    res: integer;}
begin
  {if not image.SelectionLayerIsEmpty then
  begin
    top := FInstance.HideTopmost;
    res := MessageDlg(rsTransferSelectionToOtherLayer,mtConfirmation,[mbOk,mbCancel],0);
    FInstance.ShowTopmost(top);
    if res <> mrOk then exit;
  end;}
  if image.NbLayers < MaxLayersToAdd then
  begin
    Image.AddNewLayer;
    FInstance.ScrollLayerStackOnItem(Image.CurrentLayerIndex);
  end;
end;

function TImageActions.NewLayer(ALayer: TBGRABitmap; AName: string;
  ABlendOp: TBlendOperation): boolean;
begin
  if image.NbLayers < MaxLayersToAdd then
  begin
    Image.AddNewLayer(ALayer, AName, ABlendOp);
    FInstance.ScrollLayerStackOnItem(Image.CurrentLayerIndex);
    result := true;
  end else
  begin
    FInstance.ShowMessage(rsLayers, rsTooManyLayers);
    ALayer.Free;
    result := false;
  end;
end;

function TImageActions.NewLayer(ALayer: TBGRALayerCustomOriginal;
  AName: string; ABlendOp: TBlendOperation; AMatrix: TAffineMatrix): boolean;
begin
  if image.NbLayers < MaxLayersToAdd then
  begin
    Image.AddNewLayer(ALayer, AName, ABlendOp, AMatrix);
    FInstance.ScrollLayerStackOnItem(Image.CurrentLayerIndex);
    result := true;
  end else
  begin
    FInstance.ShowMessage(rsLayers, rsTooManyLayers);
    ALayer.Free;
    result := false;
  end;
end;

procedure TImageActions.DuplicateLayer;
begin
  if image.NbLayers < MaxLayersToAdd then
  begin
    Image.DuplicateLayer;
    FInstance.ScrollLayerStackOnItem(Image.CurrentLayerIndex);
  end;
end;

procedure TImageActions.RasterizeLayer;
begin
  if CurrentTool in[ptMoveLayer,ptRotateLayer,ptZoomLayer,ptLayerMapping] then
    ChooseTool(ptHand);
  Image.RasterizeLayer;
  FInstance.ScrollLayerStackOnItem(Image.CurrentLayerIndex);
end;

procedure TImageActions.MergeLayerOver;
begin
  if (Image.CurrentLayerIndex <> -1) and (image.NbLayers > 1) then
  begin
    Image.MergeLayerOver;
    FInstance.ScrollLayerStackOnItem(Image.CurrentLayerIndex);
  end;
end;

procedure TImageActions.RemoveLayer;
var idx: integer;
begin
  if (Image.CurrentLayerIndex <> -1) and (Image.NbLayers > 1) then
  begin
    idx := Image.CurrentLayerIndex;
    Image.RemoveLayer;
    FInstance.ScrollLayerStackOnItem(idx);
  end;
end;

procedure TImageActions.EditSelection(ACallback: TModifyImageCallback);
var lSelection,lTemp: TBGRABitmap;
    LayerAction: TLayerAction;
begin
  if not image.CheckNoAction then exit;
  try
    LayerAction := Image.CreateAction;
    try
      LayerAction.QuerySelection;
      lSelection:= LayerAction.currentSelection.Duplicate as TBGRABitmap;
      lSelection.LinearAntialiasing := False;
      lSelection.ConvertFromLinearRGB;
      try
        ACallback(lSelection);
      except on ex:Exception do FInstance.ShowError('EditSelection',ex.Message);
      end;
      lSelection.InplaceGrayscale;
      lTemp := TBGRABitmap.Create(lSelection.Width,lSelection.Height,BGRABlack);
      lTemp.PutImage(0,0,lSelection,dmDrawWithTransparency);
      lSelection.Free;
      lSelection := lTemp;
      lTemp := nil;
      lSelection.ConvertToLinearRGB;
      lSelection.LinearAntialiasing := True;
      LayerAction.ReplaceCurrentSelection(lSelection);
      LayerAction.Validate;
    finally
      LayerAction.Free;
      Image.SelectionMaskMayChangeCompletely;
    end;
  except on ex:Exception do FInstance.ShowError('EditSelection',ex.Message);
  end;
end;

end.

