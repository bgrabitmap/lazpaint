unit UImageAction;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazPaintType, BGRABitmap, UImage, UTool, UScripting,
  ULayerAction, UImageType;

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
  public
    constructor Create(AInstance: TLazPaintCustomInstance);
    destructor Destroy; override;
    procedure ClearAlpha;
    procedure FillBackground;
    function SmartZoom3: boolean;
    procedure Undo;
    procedure Redo;
    procedure SetCurrentBitmap(bmp: TBGRABitmap; AUndoable: boolean);
    procedure CropToSelectionAndLayer;
    procedure CropToSelection;
    procedure HorizontalFlip(AOption: TFlipOption);
    procedure VerticalFlip(AOption: TFlipOption);
    procedure RotateCW;
    procedure RotateCCW;
    procedure LinearNegativeAll;
    procedure InvertSelection;
    procedure Deselect;
    procedure CopySelection;
    procedure CutSelection;
    procedure DeleteSelection;
    procedure Paste;
    procedure PasteAsNewLayer;
    procedure SelectAll;
    procedure SelectionFit;
    procedure NewLayer;
    procedure NewLayer(ALayer: TBGRABitmap; AName: string);
    procedure DuplicateLayer;
    procedure MergeLayerOver;
    procedure RemoveLayer;
    procedure EditSelection(ACallback: TModifyImageCallback);
    procedure Import3DObject(AFilenameUTF8: string);
    function TryAddLayerFromFile(AFilenameUTF8: string): boolean;
    function AddLayerFromBitmap(ABitmap: TBGRABitmap; AName: string): boolean;
    function LoadSelection(AFilenameUTF8: string): boolean;
    property Image: TLazPaintImage read GetImage;
    property ToolManager: TToolManager read GetToolManager;
    property CurrentTool: TPaintToolType read GetCurrentTool;
  end;

implementation

uses Controls, Dialogs, BGRABitmapTypes, UResourceStrings, UObject3D,
     ULoadImage, UGraph, UClipboard, Types;

{ TImageActions }

function TImageActions.GetImage: TLazPaintImage;
begin
  result := FInstance.Image;
end;

function TImageActions.GetCurrentTool: TPaintToolType;
begin
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
    c := ToolManager.ToolBackColor;
    c.alpha := 255;
    LayerAction := TLayerAction.Create(Image);
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
begin
  if not Image.CheckNoAction then exit;
  LayerAction := nil;
  try
    c := ToolManager.ToolBackColor;
    c.alpha := 255;
    LayerAction := TLayerAction.Create(Image);
    tempBmp := TBGRABitmap.Create(image.Width,image.Height,c);
    tempBmp.PutImage(0,0,LayerAction.SelectedImageLayer,dmDrawWithTransparency);
    LayerAction.ReplaceSelectedLayer(tempBmp, True);
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
    if CurrentTool in[ptTextureMapping,ptLayerMapping,ptMoveSelection,ptRotateSelection] then
      ChooseTool(ptHand);
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
        NewLayer(image3d, ExtractFileName(AFilenameUTF8))
      else
        image3D.Free;
    end;
  except
    on ex:Exception do
      FInstance.ShowError('Import3DObject',ex.Message);
  end;
end;

function TImageActions.LoadSelection(AFilenameUTF8: string): boolean;
var
  newSelection: TBGRABitmap;
  LayerAction: TLayerAction;
  outFilename: string;
begin
  LayerAction := nil;
  result := false;
  try
    newSelection := LoadFlatImageUTF8(AFilenameUTF8,outFilename,'');
    newSelection.InplaceGrayscale;
    if not (CurrentTool in[ptDeformation,ptTextureMapping,ptLayerMapping,ptMoveSelection,ptRotateSelection]) then
      ChooseTool(ptMoveSelection);

    if Image.CheckNoAction then
    begin
      LayerAction := TLayerAction.Create(Image);
      LayerAction.QuerySelection;
      LayerAction.CurrentSelection.Fill(BGRABlack);
      LayerAction.CurrentSelection.PutImage(0,0,newSelection,dmSet);
      Image.SelectionMayChangeCompletely;
      newSelection.Free;
      LayerAction.Validate;
      result := true;
    end;
  except
    on ex: exception do
      FInstance.ShowError('LoadSelection',ex.Message);
  end;
  LayerAction.Free;
end;

procedure TImageActions.CropToSelectionAndLayer;
var partial: TBGRABitmap; r: TRect; top: TTopMostInfo;
begin
  if not image.CheckNoAction then exit;
  if not image.CheckCurrentLayerVisible then exit;
  try
    if image.SelectionEmpty then
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
        SetCurrentBitmap(partial,true);
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
    r: TRect;
    i,selectedLayer: integer;
begin
  if not image.CheckNoAction then exit;
  if image.NbLayers = 1 then
  begin
    CropToSelectionAndLayer;
    exit;
  end;
  try
    if image.SelectionEmpty then
    begin
      FInstance.ShowMessage(rsCrop,rsEmptySelection);
      exit;
    end;
    if not image.SelectionEmpty then
    begin
      r := image.SelectionBounds[False];
      if (r.left = 0) and (r.Top = 0) and (r.right = image.width) and (r.Bottom =image.height) then exit;
      cropped := image.MakeLayeredBitmapAndSelectionCopy;
      selectedLayer := image.currentImageLayerIndex;
      for i := 0 to cropped.layeredBitmap.NbLayers-1 do
      begin
        cropped.layeredBitmap.LayerBitmap[i].ApplyMask(cropped.selection);
        cropped.layeredBitmap.SetLayerBitmap(i, cropped.layeredBitmap.LayerBitmap[i].GetPart(r) as TBGRABitmap, true);
      end;
      cropped.layeredBitmap.SetSize(r.right-r.left,r.Bottom-r.top);
      BGRAReplace(cropped.selection,cropped.selection.GetPart(r));
      if cropped.selectionLayer <> nil then BGRAReplace(cropped.selectionLayer,cropped.selectionLayer.GetPart(r));
      image.Assign(cropped,true,true);
      image.SetCurrentImageLayerIndex(selectedLayer);
    end;
  except
    on ex:Exception do
      FInstance.ShowError('CropToSelection',ex.Message);
  end;
end;

procedure TImageActions.SetCurrentBitmap(bmp: TBGRABitmap; AUndoable : boolean);
begin
  ToolManager.ToolCloseDontReopen;
  try
    image.Assign(bmp,True,AUndoable);
  finally
    ToolManager.ToolOpen;
  end;
end;

function TImageActions.TryAddLayerFromFile(AFilenameUTF8: string): boolean;
var
  newPicture: TBGRABitmap;
  finalFilename: string;
begin
  result := false;
  if not AbleToLoadUTF8(AFilenameUTF8) then
  begin
    FInstance.ShowMessage(rsOpen,rsFileExtensionNotSupported);
    exit;
  end;
  try
    newPicture := LoadFlatImageUTF8(AFilenameUTF8, finalFilename, '');
    AddLayerFromBitmap(newPicture, ExtractFileName(finalFilename));
  except
    on ex: Exception do
      FInstance.ShowError('TryAddLayerFromFile',ex.Message);
  end;
end;

function TImageActions.AddLayerFromBitmap(ABitmap: TBGRABitmap; AName: string
  ): boolean;
var
  layeraction: TLayerAction;
  ratio: single;
begin
  if (ABitmap <> nil) and (ABitmap.Width > 0) and (ABitmap.Height > 0) then
  begin
    if ToolManager.GetCurrentToolType in [ptDeformation,ptRotateSelection,ptMoveSelection,ptTextureMapping,ptLayerMapping] then
      ChooseTool(ptHand);
    if image.CheckNoAction then
    begin
      if not Image.SelectionEmpty then
      begin
        layeraction := TLayerAction.Create(image);
        layeraction.ReleaseSelection;
        layeraction.Validate;
        layeraction.Free;
      end;
      if (ABitmap.Width > Image.Width) or (ABitmap.Height > Image.Height) then
      begin
        ratio := 1;
        if ABitmap.Width > Image.Width then ratio := Image.Width/ABitmap.Width;
        if ABitmap.Height*ratio > Image.Height then ratio := Image.Height/ABitmap.Height;
        ABitmap.ResampleFilter := rfBestQuality;
        BGRAReplace(ABitmap, ABitmap.Resample(round(ABitmap.Width*ratio),round(ABitmap.Height*ratio)));
      end;
      NewLayer(ABitmap, AName);
      result := true;
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

procedure TImageActions.HorizontalFlip(AOption: TFlipOption);
var bounds: TRect;
    LayerAction: TLayerAction;
begin
  try
    if (AOption = foCurrentLayer) then
    begin
      if not Image.CheckNoAction then exit;
      LayerAction := TLayerAction.Create(Image);
      LayerAction.SelectedImageLayer.HorizontalFlip;
      Image.LayerMayChangeCompletely(LayerAction.SelectedImageLayer);
      LayerAction.Validate;
      LayerAction.Free;
    end else
    if ((AOption = foAuto) and not image.SelectionEmpty) or (AOption = foSelection) then
    begin
      if not image.SelectionEmpty then
      begin
        ChooseTool(ptMoveSelection);
        if not Image.CheckNoAction then exit;
        LayerAction := TLayerAction.Create(Image);
        LayerAction.AllChangesNotified := true;
        bounds := Image.SelectionBounds[false];
        LayerAction.currentSelection.HorizontalFlip(bounds);
        LayerAction.NotifyChange(LayerAction.currentSelection,bounds);
        Image.SelectionMayChange(bounds);
        if LayerAction.DrawingLayer <> nil then
        begin
          LayerAction.DrawingLayer.HorizontalFlip(bounds);
          LayerAction.NotifyChange(LayerAction.DrawingLayer,bounds);
          Image.LayerMayChange(LayerAction.DrawingLayer,bounds);
        end;
        LayerAction.Validate;
        LayerAction.Free;
      end else
        exit;
    end else
    if ((AOption = foAuto) and image.SelectionEmpty) or (AOption = foWholePicture) then
      image.HorizontalFlip;
  except
    on ex:Exception do
      FInstance.ShowError('HorizontalFlip',ex.Message);
  end;
end;

procedure TImageActions.VerticalFlip(AOption: TFlipOption);
var bounds: TRect;
    LayerAction: TLayerAction;
begin
  try
    if (AOption = foCurrentLayer) then
    begin
      if not Image.CheckNoAction then exit;
      LayerAction := TLayerAction.Create(Image);
      LayerAction.SelectedImageLayer.VerticalFlip;
      Image.LayerMayChangeCompletely(LayerAction.SelectedImageLayer);
      LayerAction.Validate;
      LayerAction.Free;
    end else
    if ((AOption = foAuto) and not image.SelectionEmpty) or (AOption = foSelection) then
    begin
      if not image.SelectionEmpty then
      begin
        ChooseTool(ptMoveSelection);
        if not Image.CheckNoAction then exit;
        LayerAction := TLayerAction.Create(Image);
        bounds := Image.SelectionBounds[False];
        LayerAction.currentSelection.VerticalFlip(bounds);
        LayerAction.NotifyChange(LayerAction.currentSelection,bounds);
        Image.SelectionMayChange(bounds);
        if LayerAction.DrawingLayer <> nil then
        begin
          LayerAction.DrawingLayer.VerticalFlip(bounds);
          LayerAction.NotifyChange(LayerAction.DrawingLayer,bounds);
          Image.LayerMayChange(LayerAction.DrawingLayer,bounds);
        end;
        LayerAction.Validate;
        LayerAction.Free;
      end else
        exit;
    end else
    if ((AOption = foAuto) and image.SelectionEmpty) or (AOption = foWholePicture) then
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

procedure TImageActions.InvertSelection;
var LayerAction: TLayerAction;
    p : PBGRAPixel;
    n: integer;
begin
  LayerAction := nil;
  try
    if not ToolManager.IsSelectingTool then ChooseTool(ptSelectRect);
    LayerAction := TLayerAction.Create(Image);
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
    Image.SelectionMayChangeCompletely;
  except
    on ex:Exception do
      FInstance.ShowError('InvertSelection',ex.Message);
  end;
  LayerAction.Free;
end;

procedure TImageActions.Deselect;
var LayerAction: TLayerAction;
begin
  if (CurrentTool in[ptRotateSelection,ptMoveSelection]) then
    ChooseTool(ptHand);
  if not Image.CheckNoAction then exit;
  LayerAction := nil;
  try
    if not image.SelectionEmpty then
    begin
      LayerAction := TLayerAction.Create(Image);
      LayerAction.AllChangesNotified:= true;
      LayerAction.ReleaseSelection;
      LayerAction.Validate;
    end;
  except
    on ex:Exception do
      FInstance.ShowError('Deselect',ex.Message);
  end;
  LayerAction.Free;
end;

procedure TImageActions.CopySelection;
var layer, partial : TBGRABitmap; r: TRect;
    LayerAction: TLayerAction;
    bounds: TRect;
begin
  LayerAction := nil;
  try
    if not image.CheckNoAction then exit;
    bounds := Image.SelectionBounds[False];
    if IsRectEmpty(bounds) then exit;
    LayerAction := TLayerAction.Create(Image);
    LayerAction.ApplySelectionMask;
    if Image.SelectionLayerIsEmpty then
      LayerAction.RetrieveSelection;
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
  if not image.CheckNoAction then exit;
  LayerAction := nil;
  try
    if image.SelectionEmpty then exit;
    CopySelection;
    LayerAction := TLayerAction.Create(Image);
    if (LayerAction.GetSelectionLayerIfExists = nil) or (LayerAction.GetSelectionLayerIfExists.Empty) then
      LayerAction.EraseSelectionInBitmap;
    LayerAction.RemoveSelection;
    LayerAction.Validate;
    if (CurrentTool = ptRotateSelection) or
       (CurrentTool = ptMoveSelection) then
      ChooseTool(ptHand);
  except
    on ex:Exception do
      FInstance.ShowError('CutSelection',ex.Message);
  end;
  LayerAction.Free;
end;

procedure TImageActions.DeleteSelection;
var LayerAction: TLayerAction;
begin
  if not image.CheckNoAction then exit;
  LayerAction := nil;
  try
    if image.SelectionEmpty then exit;

    LayerAction := TLayerAction.Create(Image);
    if Image.SelectionLayerIsEmpty then
      LayerAction.EraseSelectionInBitmap;
    LayerAction.RemoveSelection;
    LayerAction.Validate;
    if (CurrentTool = ptRotateSelection) or
       (CurrentTool = ptMoveSelection) then
      ChooseTool(ptHand);
  except
    on ex:Exception do
      FInstance.ShowError('DeleteSelection',ex.Message);
  end;
  LayerAction.Free;
end;

procedure TImageActions.Paste;
var partial: TBGRABitmap;
    layeraction: TLayerAction;
    pastePos: TPoint;
begin
  try
    partial := GetBitmapFromClipboard;
    if partial<>nil then
    begin
      if partial.NbPixels <> 0 then
      begin
        ToolManager.ToolCloseDontReopen;
        layeraction := TLayerAction.Create(Image);
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
        Image.SelectionMayChange(rect(pastePos.x,pastePos.y,pastePos.x+partial.Width,pastePos.y+partial.Height));
        layeraction.Validate;
        layeraction.Free;
        ChooseTool(ptMoveSelection);
      end;
      partial.Free;
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
    if not ToolManager.IsSelectingTool then ChooseTool(ptSelectRect);
    LayerAction := TLayerAction.Create(Image);
    LayerAction.QuerySelection;
    LayerAction.currentSelection.Fill(BGRAWhite);
    Image.SelectionMayChangeCompletely;
    LayerAction.Validate;
    LayerAction.Free;
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
    LayerAction := TLayerAction.Create(Image);
    LayerAction.AllChangesNotified := true;

    if image.SelectionEmpty then
    begin
      bounds := rect(0,0,Image.width,image.height);
      LayerAction.QuerySelection;
      LayerAction.currentSelection.Fill(BGRAWhite);
      LayerAction.NotifyChange(LayerAction.currentSelection, bounds);
      Image.SelectionMayChange(bounds);
    end else
    begin
      bounds := image.SelectionLayerBounds[False];
      Image.SelectionMayChange(bounds);
      LayerAction.ApplySelectionMask;
      LayerAction.NotifyChange(LayerAction.GetSelectionLayerIfExists, bounds);
      bounds := image.SelectionBounds[False];
      Image.SelectionMayChange(bounds);
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
    if image.SelectionEmpty then
    begin
      if (CurrentTool = ptRotateSelection) or
         (CurrentTool  = ptMoveSelection) then
        ChooseTool(ptHand);
    end else
      if not ToolManager.IsSelectingTool then ChooseTool(ptMoveSelection);
  except
    on ex:Exception do
      FInstance.ShowError('SelectionFit',ex.Message);
  end;
end;

procedure TImageActions.NewLayer;
var top: TTopMostInfo;
    res: integer;
begin
  if not image.SelectionLayerIsEmpty then
  begin
    top := FInstance.HideTopmost;
    res := MessageDlg(rsTransferSelectionToOtherLayer,mtConfirmation,[mbOk,mbCancel],0);
    FInstance.ShowTopmost(top);
    if res <> mrOk then exit;
  end;
  if image.NbLayers < MaxLayersToAdd then
  begin
    Image.AddNewLayer;
    FInstance.ScrollLayerStackOnItem(Image.currentImageLayerIndex);
  end;
end;

procedure TImageActions.NewLayer(ALayer: TBGRABitmap; AName: string);
begin
  if image.NbLayers < MaxLayersToAdd then
  begin
    Image.AddNewLayer(ALayer, AName);
    FInstance.ScrollLayerStackOnItem(Image.currentImageLayerIndex);
  end;
end;

procedure TImageActions.DuplicateLayer;
begin
  if image.NbLayers < MaxLayersToAdd then
  begin
    Image.DuplicateLayer;
    FInstance.ScrollLayerStackOnItem(Image.currentImageLayerIndex);
  end;
end;

procedure TImageActions.MergeLayerOver;
begin
  if (Image.currentImageLayerIndex <> -1) and (image.NbLayers > 1) then
  begin
    Image.MergeLayerOver;
    FInstance.ScrollLayerStackOnItem(Image.currentImageLayerIndex);
  end;
end;

procedure TImageActions.RemoveLayer;
var idx: integer;
begin
  if (Image.currentImageLayerIndex <> -1) and (Image.NbLayers > 1) then
  begin
    idx := Image.currentImageLayerIndex;
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
    LayerAction := TLayerAction.Create(Image);
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
      Image.SelectionMayChangeCompletely;
    end;
  except on ex:Exception do FInstance.ShowError('EditSelection',ex.Message);
  end;
end;

end.

