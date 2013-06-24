unit UImageAction;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazPaintType, BGRABitmap, uimage, utool;

type

  { TImageActions }

  TImageActions = class
  private
    FInstance: TLazPaintCustomInstance;
    function GetCurrentTool: TPaintToolType;
    function GetImage: TLazPaintImage;
    function GetToolManager: TToolManager;
    procedure ChooseTool(ATool: TPaintToolType);
  public
    constructor Create(AInstance: TLazPaintCustomInstance);
    procedure ClearAlpha;
    procedure FillBackground;
    procedure SmartZoom3;
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
    procedure SelectAll;
    procedure SelectionFit;
    procedure Import3DObject(AFilename: string);
    function TryAddLayerFromFile(AFilename: string): boolean;
    function LoadSelection(AFilename: string): boolean;
    property Image: TLazPaintImage read GetImage;
    property ToolManager: TToolManager read GetToolManager;
    property CurrentTool: TPaintToolType read GetCurrentTool;
  end;

implementation

uses Controls, Dialogs, BGRABitmapTypes, UResourceStrings, UObject3D,
     ULoadImage, UGraph, UClipboard;

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

constructor TImageActions.Create(AInstance: TLazPaintCustomInstance);
begin
  FInstance := AInstance;
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
    Image.ImageMayChangeCompletely;
    LayerAction.Validate;
  except
    on ex:Exception do
      ShowMessage('ClearAlpha: '+ex.Message);
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
    LayerAction.Validate;
  except
    on ex:Exception do
      ShowMessage('FillBackground: '+ex.Message);
  end;
  LayerAction.Free;
end;

procedure TImageActions.SmartZoom3;
begin
  if (image.Width * 3 > MaxImageWidth) or
    (image.Height * 3 > MaxImageHeight) then
  begin
    MessageDlg(rsImageTooBig,mtInformation,[mbOK],0);
    exit;
  end;
  ChooseTool(ptHand);
  try
    image.ApplySmartZoom3;
  except
    on ex:Exception do
      ShowMessage('SmartZoom3: '+ex.Message);
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
      ShowMessage('Undo: '+ex.Message);
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
      ShowMessage('Redo: '+ex.Message);
  end;
end;

procedure TImageActions.Import3DObject(AFilename: string);
var image3D: TBGRABitmap;
    LayerAction: TLayerAction;
begin
  try
    image3D := ShowObject3DDlg(FInstance, AFileName, Image.Width, Image.Height);
    if image3D <> nil then
    begin
      if image3D.NbPixels <> 0 then
      begin
        LayerAction := TLayerAction.Create(Image);
        LayerAction.ReleaseSelection;
        ChooseTool(ptMoveSelection);
        LayerAction.QuerySelection;
        LayerAction.GetOrCreateSelectionLayer.PutImage((image.Width - image3D.Width) div 2,
           (image.Height - image3D.Height) div 2,image3D,dmFastBlend);
        LayerAction.CurrentSelection.FillRect((image.Width - image3D.Width) div 2,
           (image.Height - image3D.Height) div 2,
           (image.Width - image3D.Width) div 2+image3D.Width,
           (image.Height - image3D.Height) div 2+image3D.Height,BGRAWhite,dmSet);
        LayerAction.SelectionOffset := Point(- image.ImageOffset.X,- image.ImageOffset.Y );
        image.ImageMayChangeCompletely;
        LayerAction.Validate;
        LayerAction.Free;
      end;
      image3D.Free;
    end;
  except
    on ex:Exception do
      ShowMessage('Import3DObject: '+ex.Message);
  end;
end;

function TImageActions.LoadSelection(AFilename: string): boolean;
var
  newSelection: TBGRABitmap;
  LayerAction: TLayerAction;
  outFilename: string;
begin
  LayerAction := nil;
  result := false;
  try
    newSelection := LoadFlatImageSys(AFilename,outFilename,'');
    newSelection.InplaceGrayscale;
    if not (CurrentTool in[ptDeformation,ptTextureMapping,ptLayerMapping,ptMoveSelection,ptRotateSelection]) then
      ChooseTool(ptMoveSelection);

    if Image.CheckNoAction then
    begin
      LayerAction := TLayerAction.Create(Image);
      LayerAction.QuerySelection;
      LayerAction.CurrentSelection.Fill(BGRABlack);
      LayerAction.CurrentSelection.PutImage(0,0,newSelection,dmSet);
      newSelection.Free;
      LayerAction.Validate;
      result := true;
    end;
  except
    on ex: exception do
      MessageDlg(ex.Message,mtError,[mbOk],0);
  end;
  LayerAction.Free;
end;

procedure TImageActions.CropToSelectionAndLayer;
var partial: TBGRABitmap; r: TRect;
begin
  if not image.CheckNoAction then exit;
  if not image.CheckCurrentLayerVisible then exit;
  try
    if image.SelectionEmpty then
    begin
      ShowMessage(rsEmptySelection);
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
          case MessageDlg(rsCrop,rsKeepEmptySpace,mtConfirmation,mbYesNo,0) of
          mrNo: BGRAReplace(partial, partial.GetPart(r));
          end;
        end;
        SetCurrentBitmap(partial,true);
      end
      else
        partial.Free;
    end;
  except
    on ex:Exception do
      ShowMessage('CropToSelectionAndLayer: '+ex.Message);
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
      ShowMessage(rsEmptySelection);
      exit;
    end;
    if not image.SelectionEmpty then
    begin
      r := image.SelectionReadonly.GetImageBounds(cGreen);
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
      ShowMessage('CropToSelection: '+ex.Message);
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

function TImageActions.TryAddLayerFromFile(AFilename: string): boolean;
var
  newPicture: TBGRABitmap;
  finalFilename: string;
  layeraction: TLayerAction;
begin
  result := false;
  if not AbleToLoadSys(AFilename) then
  begin
    ShowMessage(rsFileExtensionNotSupported);
    exit;
  end;
  try
    newPicture := LoadFlatImageSys(AFilename, finalFilename, '');
    if (newPicture <> nil) and (newPicture.Width > 0) and (newPicture.Height > 0) then
    begin
      if ToolManager.GetCurrentToolType in [ptDeformation,ptRotateSelection,ptMoveSelection,ptTextureMapping,ptLayerMapping] then
        ChooseTool(ptHand);
      if image.CheckNoAction then
      begin
        layeraction := TLayerAction.Create(image);
        layeraction.ReleaseSelection;
        layeraction.Validate;
        layeraction.Free;
        FInstance.NewLayer(newPicture, ExtractFileName(finalFilename));
        result := true;
      end;
    end else newPicture.Free;
  except
    on ex: Exception do
      MessageDlg(ex.Message,mtError,[mbOk],0);
  end;
end;

procedure TImageActions.HorizontalFlip(AOption: TFlipOption);
var center: TPointF;
    LayerAction: TLayerAction;
begin
  try
    if (AOption = foCurrentLayer) then
    begin
      if not Image.CheckNoAction then exit;
      LayerAction := TLayerAction.Create(Image);
      LayerAction.ReleaseSelection;
      LayerAction.SelectedImageLayer.HorizontalFlip;
      LayerAction.Validate;
      LayerAction.Free;
      Image.ImageMayChangeCompletely;
    end else
    if ((AOption = foAuto) and not image.SelectionEmpty) or (AOption = foSelection) then
    begin
      if not image.SelectionEmpty then
      begin
        ChooseTool(ptMoveSelection);
        if not Image.CheckNoAction then exit;
        LayerAction := TLayerAction.Create(Image);
        center := GetSelectionCenter(LayerAction.currentSelection);
        LayerAction.currentSelection.HorizontalFlip;
        LayerAction.SelectionOffset := point(round((center.X-(image.Width/2-0.5))*2),0);
        if LayerAction.DrawingLayer <> nil then
          LayerAction.DrawingLayer.HorizontalFlip;
        LayerAction.Validate;
        LayerAction.Free;
        Image.ImageMayChangeCompletely;
      end else
        exit;
    end else
    if ((AOption = foAuto) and image.SelectionEmpty) or (AOption = foWholePicture) then
      image.HorizontalFlip;
  except
    on ex:Exception do
      ShowMessage('HorizontalFlip: '+ex.Message);
  end;
end;

procedure TImageActions.VerticalFlip(AOption: TFlipOption);
var center: TPointF;
    LayerAction: TLayerAction;
begin
  try
    if (AOption = foCurrentLayer) then
    begin
      if not Image.CheckNoAction then exit;
      LayerAction := TLayerAction.Create(Image);
      LayerAction.ReleaseSelection;
      LayerAction.SelectedImageLayer.VerticalFlip;
      LayerAction.Validate;
      LayerAction.Free;
      Image.ImageMayChangeCompletely;
    end else
    if ((AOption = foAuto) and not image.SelectionEmpty) or (AOption = foSelection) then
    begin
      if not image.SelectionEmpty then
      begin
        ChooseTool(ptMoveSelection);
        if not Image.CheckNoAction then exit;
        LayerAction := TLayerAction.Create(Image);
        center := GetSelectionCenter(LayerAction.currentSelection);
        LayerAction.currentSelection.VerticalFlip;
        LayerAction.SelectionOffset := point(0,round((center.Y-(image.Height/2-0.5))*2));
        if LayerAction.DrawingLayer <> nil then
          LayerAction.DrawingLayer.VerticalFlip;
        LayerAction.Validate;
        LayerAction.Free;
        Image.ImageMayChangeCompletely;
      end else
        exit;
    end else
    if ((AOption = foAuto) and image.SelectionEmpty) or (AOption = foWholePicture) then
      image.VerticalFlip;
  except
    on ex:Exception do
      ShowMessage('VerticalFlip: '+ex.Message);
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
begin
  LayerAction := nil;
  try
    if not ToolManager.IsSelectingTool then ChooseTool(ptSelectRect);
    LayerAction := TLayerAction.Create(Image);
    LayerAction.QuerySelection;
    LayerAction.CurrentSelection.Negative;
    LayerAction.Validate;
    Image.SelectionMayChangeCompletely;
  except
    on ex:Exception do
      ShowMessage('InvertSelection: '+ex.Message);
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
      LayerAction.ReleaseSelection;
      LayerAction.Validate;
    end;
  except
    on ex:Exception do
      ShowMessage('Deselect: '+ex.Message);
  end;
  LayerAction.Free;
end;

procedure TImageActions.CopySelection;
var layer, partial, backupSelectionLayer : TBGRABitmap; r: TRect;
    LayerAction: TLayerAction;
begin
  LayerAction := nil;
  try
    if not image.CheckNoAction then exit;
    if image.SelectionEmpty then exit;

    LayerAction := TLayerAction.Create(Image);
    if LayerAction.GetSelectionLayerIfExists <> nil then
      backupSelectionLayer := LayerAction.GetSelectionLayerIfExists.Duplicate as TBGRABitmap else
        backupSelectionLayer := nil;
    LayerAction.ApplySelectionMask;
    LayerAction.RetrieveSelectionIfLayerEmpty;
    layer := LayerAction.GetOrCreateSelectionLayer;
    r := layer.GetImageBounds;
    if (r.right > r.left) and (r.bottom > r.top) then
    begin
      partial := layer.GetPart(r) as TBGRABitmap;
      CopyToClipboard(partial);
      partial.Free;
    end;
    LayerAction.ReplaceSelectionLayer(backupSelectionLayer,True);
    FreeAndNil(LayerAction);
  except
    on ex:Exception do
    begin
      FreeAndNil(LayerAction);
      ShowMessage('CopySelection: '+ex.Message);
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
      ShowMessage('CutSelection: '+ex.Message);
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
    LayerAction.RetrieveSelectionIfLayerEmpty(true);
    LayerAction.RemoveSelection;
    LayerAction.Validate;
    if (CurrentTool = ptRotateSelection) or
       (CurrentTool = ptMoveSelection) then
      ChooseTool(ptHand);
  except
    on ex:Exception do
      ShowMessage('DeleteSelection: '+ex.Message);
  end;
  LayerAction.Free;
end;

procedure TImageActions.Paste;
var partial: TBGRABitmap;
    layeraction: TLayerAction;
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
        layeraction.GetOrCreateSelectionLayer.PutImage((image.Width - partial.Width) div 2,
           (image.Height - partial.Height) div 2,partial,dmFastBlend);
        ComputeSelectionMask(layeraction.GetOrCreateSelectionLayer,layeraction.currentSelection);
        layeraction.SelectionOffset := Point(- image.ImageOffset.X, - image.ImageOffset.Y);
        Image.SelectionMayChangeCompletely;
        layeraction.Validate;
        layeraction.Free;
        ChooseTool(ptMoveSelection);
      end;
      partial.Free;
    end;
  except
    on ex:Exception do
      ShowMessage('Paste: '+ex.Message);
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
      ShowMessage('SelectAll: '+ex.Message);
  end;
end;

procedure TImageActions.SelectionFit;
var LayerAction: TLayerAction;
begin
  if not image.CheckNoAction then exit;
  try
    LayerAction := TLayerAction.Create(Image);
    LayerAction.ApplySelectionMask;
    if image.SelectionEmpty then
    begin
      LayerAction.QuerySelection;
      LayerAction.currentSelection.Fill(BGRAWhite);
    end;
    LayerAction.RetrieveSelectionIfLayerEmpty(True);
    ComputeSelectionMask(LayerAction.GetOrCreateSelectionLayer,LayerAction.currentSelection);
    Image.SelectionMayChangeCompletely;
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
      ShowMessage('SelectionFit: '+ex.Message);
  end;
end;

end.

