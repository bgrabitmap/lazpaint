// SPDX-License-Identifier: GPL-3.0-only
unit UImageAction;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPimage, LazPaintType, BGRABitmap, UImage, UTool,
  UScripting, ULayerAction, UImageType, BGRABitmapTypes, BGRALayerOriginal,
  BGRASVGOriginal;

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
    function ScriptGetAllLayersId(AVars: TVariableSet): TScriptResult;
    function ScriptGetLayerIndex(AVars: TVariableSet): TScriptResult;
    function ScriptImageMoveLayerIndex(AVars: TVariableSet): TScriptResult;
    function ScriptLayerFromFile(AVars: TVariableSet): TScriptResult;
    function ScriptImageGetRegistry(AVars: TVariableSet): TScriptResult;
    function ScriptLayerGetId(AVars: TVariableSet): TScriptResult;
    function ScriptLayerGetRegistry(AVars: TVariableSet): TScriptResult;
    function ScriptLayerSaveAs(AVars: TVariableSet): TScriptResult;
    function ScriptLayerSelectId(AVars: TVariableSet): TScriptResult;
    function ScriptLayerAddNew(AVars: TVariableSet): TScriptResult;
    function ScriptImageSetRegistry(AVars: TVariableSet): TScriptResult;
    function ScriptLayerSetRegistry(AVars: TVariableSet): TScriptResult;
    function ScriptPasteAsNewLayer(AVars: TVariableSet): TScriptResult;
    function ScriptLayerDuplicate(AVars: TVariableSet): TScriptResult;
    function ScriptPutImage(AVars: TVariableSet): TScriptResult;
    function ScriptGetImage(AVars: TVariableSet): TScriptResult;
    function ScriptLayerFill(AVars: TVariableSet): TScriptResult;
    function ScriptGetFrameIndex(AVars: TVariableSet): TScriptResult;
    procedure ReleaseSelection;
    function ScriptSelectLayerIndex(AVars: TVariableSet): TScriptResult;
    function ScriptClearAlpha(AVars: TVariableSet): TScriptResult;
    function ScriptFillBackground(AVars: TVariableSet): TScriptResult;
  public
    constructor Create(AInstance: TLazPaintCustomInstance);
    destructor Destroy; override;
    procedure ClearAlpha;
    procedure FillBackground;
    procedure ClearAlpha(AColor: TBGRAPixel);
    procedure FillBackground(AColor: TBGRAPixel);
    function SmartZoom3: boolean;
    procedure Undo;
    procedure Redo;
    procedure DoBegin;
    function DoEnd: boolean;
    procedure SetCurrentBitmap(bmp: TBGRABitmap; AUndoable: boolean;
      ACaption: string = ''; AOpacity: byte = 255);
    procedure CropToSelectionAndLayer;
    procedure CropToSelection;
    procedure Flatten;
    procedure HorizontalFlip(AOption: TFlipOption);
    procedure VerticalFlip(AOption: TFlipOption);
    procedure RotateCW;
    procedure RotateCCW;
    procedure Rotate180;
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
    function PasteAsNewLayer: integer;
    procedure SelectAll;
    procedure SelectionFit;
    function NewLayer: boolean; overload;
    function NewLayer(ALayer: TBGRABitmap; AName: string; ABlendOp: TBlendOperation; AOpacity: byte = 255): boolean; overload;
    function NewLayer(ALayer: TBGRABitmap; AName: string; AOffset: TPoint; ABlendOp: TBlendOperation; AOpacity: byte = 255): boolean; overload;
    function NewLayer(ALayer: TBGRALayerCustomOriginal; AName: string; ABlendOp: TBlendOperation; AMatrix: TAffineMatrix; AOpacity: byte = 255): boolean; overload;
    function DuplicateLayer: boolean;
    procedure RasterizeLayer;
    procedure MergeLayerOver;
    function RemoveLayer: boolean;
    procedure EditSelection(ACallback: TModifyImageCallback);
    procedure Import3DObject(AFilenameUTF8: string);
    function GetPixel(X,Y: Integer): TBGRAPixel;
    function PutImage(X,Y: integer; AImage: TBGRACustomBitmap; AMode: TDrawMode; AOpacity: byte): boolean;
    function LayerFill(AColor: TBGRAPixel; AMode: TDrawMode): boolean;
    function TryAddLayerFromFile(AFilenameUTF8: string; ALoadedImage: TBGRABitmap = nil): ArrayOfLayerId;
    function AddLayerFromBitmap(ABitmap: TBGRABitmap; AName: string): boolean;
    function AddLayerFromOriginal(AOriginal: TBGRALayerCustomOriginal; AName: string): boolean;
    function AddLayerFromOriginal(AOriginal: TBGRALayerCustomOriginal; AName: string; AMatrix: TAffineMatrix; ABlendOp: TBlendOperation = boTransparent; AOpacity: byte = 255): boolean;
    function LoadSelection(AFilenameUTF8: string; ALoadedImage: PImageEntry = nil): boolean;
    property Image: TLazPaintImage read GetImage;
    property ToolManager: TToolManager read GetToolManager;
    property CurrentTool: TPaintToolType read GetCurrentTool;
  end;

implementation

uses Controls, Dialogs, UResourceStrings, UObject3D,
     ULoadImage, UGraph, UClipboard, Types, BGRAGradientOriginal,
     BGRATransform, ULoading, math, LCVectorClipboard, LCVectorOriginal, LCVectorRectShapes,
     BGRALayers, BGRAUTF8, UFileSystem, Forms, UTranslation;

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
  Scripting.RegisterScriptFunction('ImageCrop',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('ImageCropLayer',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('ImageFlatten',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('ImageClearAlpha',@ScriptClearAlpha,ARegister);
  Scripting.RegisterScriptFunction('ImageFillBackground',@ScriptFillBackground,ARegister);
  Scripting.RegisterScriptFunction('ImageSmartZoom3',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('ImageHorizontalFlip',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('ImageVerticalFlip',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('SelectionHorizontalFlip',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('SelectionVerticalFlip',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('ImageRotateCW',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('ImageRotateCCW',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('ImageRotate180',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('ImageLinearNegative',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('ImageNegative',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('ImageSwapRedBlue',@GenericScriptFunction,ARegister);

  Scripting.RegisterScriptFunction('EditUndo',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('EditRedo',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('EditDoBegin',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('EditDoEnd',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('EditInvertSelection',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('EditDeselect',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('EditCopy',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('EditCut',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('EditDeleteSelection',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('EditPaste',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('EditPasteAsNewLayer',@ScriptPasteAsNewLayer,ARegister);
  Scripting.RegisterScriptFunction('EditSelectAll',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('EditSelectionFit',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('IsSelectionMaskEmpty',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('IsSelectionLayerEmpty',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('IsLayerEmpty',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('IsLayerTransparent',@GenericScriptFunction,ARegister);

  Scripting.RegisterScriptFunction('LayerHorizontalFlip',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('LayerVerticalFlip',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('LayerGetId',@ScriptLayerGetId,ARegister);
  Scripting.RegisterScriptFunction('LayerGetName',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('LayerGetOpacity',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('LayerGetBlendOp',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('LayerGetVisible',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('LayerSelectId',@ScriptLayerSelectId,ARegister);
  Scripting.RegisterScriptFunction('LayerSetName',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('LayerSetOpacity',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('LayerSetBlendOp',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('LayerSetVisible',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('LayerAddNew',@ScriptLayerAddNew,ARegister);
  Scripting.RegisterScriptFunction('LayerFromFile',@ScriptLayerFromFile,ARegister);
  Scripting.RegisterScriptFunction('LayerSaveAs',@ScriptLayerSaveAs,ARegister);
  Scripting.RegisterScriptFunction('LayerDuplicate',@ScriptLayerDuplicate,ARegister);
  Scripting.RegisterScriptFunction('LayerRasterize',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('LayerMergeOver',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('LayerRemoveCurrent',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('LayerGetRegistry',@ScriptLayerGetRegistry,ARegister);
  Scripting.RegisterScriptFunction('LayerSetRegistry',@ScriptLayerSetRegistry,ARegister);
  Scripting.RegisterScriptFunction('ImageGetRegistry',@ScriptImageGetRegistry,ARegister);
  Scripting.RegisterScriptFunction('ImageSetRegistry',@ScriptImageSetRegistry,ARegister);
  Scripting.RegisterScriptFunction('ImageMoveLayerIndex',@ScriptImageMoveLayerIndex,ARegister);
  Scripting.RegisterScriptFunction('GetLayerIndex',@ScriptGetLayerIndex,ARegister);
  Scripting.RegisterScriptFunction('GetAllLayersId',@ScriptGetAllLayersId,ARegister);
  Scripting.RegisterScriptFunction('SelectLayerIndex',@ScriptSelectLayerIndex,ARegister);
  Scripting.RegisterScriptFunction('GetLayerCount',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('GetFrameIndex',@ScriptGetFrameIndex,ARegister);
  Scripting.RegisterScriptFunction('GetFrameCount',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('GetPixel',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('GetImageWidth',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('GetImageHeight',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('GetImageSize',@GenericScriptFunction,ARegister);
  Scripting.RegisterScriptFunction('PutImage',@ScriptPutImage,ARegister);
  Scripting.RegisterScriptFunction('GetImage',@ScriptGetImage,ARegister);
  Scripting.RegisterScriptFunction('LayerFill',@ScriptLayerFill,ARegister);
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
  if f = 'ImageCrop' then CropToSelection else
  if f = 'ImageFlatten' then Flatten else
  if f = 'ImageRotateCW' then RotateCW else
  if f = 'ImageRotateCCW' then RotateCCW else
  if f = 'ImageRotate180' then Rotate180 else
  if f = 'ImageLinearNegative' then LinearNegativeAll else
  if f = 'ImageNegative' then NegativeAll else
  if f = 'ImageSwapRedBlue' then SwapRedBlueAll else
  if f = 'EditUndo' then Undo else
  if f = 'EditRedo' then Redo else
  if f = 'EditDoBegin' then DoBegin else
  if f = 'EditDoEnd' then AVars.Booleans['Result'] := DoEnd else
  if f = 'EditInvertSelection' then InvertSelection else
  if f = 'EditDeselect' then Deselect else
  if f = 'EditCopy' then CopySelection else
  if f = 'EditCut' then CutSelection else
  if f = 'EditDeleteSelection' then DeleteSelection else
  if f = 'EditPaste' then Paste else
  if f = 'EditSelectAll' then SelectAll else
  if f = 'EditSelectionFit' then SelectionFit else
  if f = 'IsSelectionMaskEmpty' then AVars.Booleans['Result'] := Image.SelectionMaskEmpty else
  if f = 'IsSelectionLayerEmpty' then AVars.Booleans['Result'] := Image.SelectionLayerIsEmpty else
  if f = 'IsLayerEmpty' then AVars.Booleans['Result'] := Image.CurrentLayerEmpty else
  if f = 'IsLayerTransparent' then AVars.Booleans['Result'] := Image.CurrentLayerTransparent else
  if f = 'LayerHorizontalFlip' then HorizontalFlip(foCurrentLayer) else
  if f = 'LayerVerticalFlip' then VerticalFlip(foCurrentLayer) else
  if f = 'LayerGetName' then AVars.Strings['Result'] := Image.LayerName[Image.CurrentLayerIndex] else
  if f = 'LayerGetOpacity' then AVars.Integers['Result'] := Image.LayerOpacity[Image.CurrentLayerIndex] else
  if f = 'LayerGetBlendOp' then AVars.Strings['Result'] := BlendOperationStr[Image.BlendOperation[Image.CurrentLayerIndex]] else
  if f = 'LayerGetVisible' then AVars.Booleans['Result'] := Image.LayerVisible[Image.CurrentLayerIndex] else
  if f = 'LayerSetName' then Image.LayerName[Image.CurrentLayerIndex] := AVars.Strings['Name'] else
  if f = 'LayerSetOpacity' then Image.LayerOpacity[Image.CurrentLayerIndex] := min(255, max(0, AVars.Integers['Opacity'])) else
  if f = 'LayerSetBlendOp' then Image.BlendOperation[Image.CurrentLayerIndex] := StrToBlendOperation(AVars.Strings['BlendOp']) else
  if f = 'LayerSetVisible' then Image.LayerVisible[Image.CurrentLayerIndex] := AVars.Booleans['Visible'] else
  if f = 'LayerRasterize' then RasterizeLayer else
  if f = 'LayerMergeOver' then MergeLayerOver else
  if f = 'LayerRemoveCurrent' then begin if not RemoveLayer then result := srException end else
  if f = 'GetLayerCount' then AVars.Integers['Result']:= Image.NbLayers else
  if f = 'GetFrameCount' then AVars.Integers['Result']:= Image.FrameCount else
  if f = 'GetPixel' then AVars.Pixels['Result']:= GetPixel(AVars.Integers['X'],AVars.Integers['Y']) else
  if f = 'GetImageSize' then AVars.Points2D['Result']:= PointF(Image.Width,Image.Height) else
  if f = 'GetImageWidth' then AVars.Integers['Result']:= Image.Width else
  if f = 'GetImageHeight' then AVars.Integers['Result']:= Image.Height else
    result := srFunctionNotDefined;
end;

function TImageActions.ScriptGetAllLayersId(AVars: TVariableSet): TScriptResult;
var
  idList: TScriptVariableReference;
  i: Integer;
begin
  idList := AVars.AddGuidList('Result');
  for i := 0 to Image.NbLayers-1 do
    if not AVars.AppendGuid(idList, Image.LayerGuid[i]) then
      exit(srException);
  result := srOk;
end;

function TImageActions.ScriptGetLayerIndex(AVars: TVariableSet): TScriptResult;
var
  idx: Integer;
  layerGuid: TGUID;
begin
  if AVars.IsDefined('LayerId') then
  begin
    if not TryStringToGUID('{'+AVars.Strings['LayerId']+'}', layerGuid) then
      exit(srInvalidParameters);
    idx := Image.GetLayerIndexByGuid(layerGuid);
    if idx <> -1 then
      AVars.Integers['Result']:= idx+1
    else
      AVars.Remove('Result');
  end else
    AVars.Integers['Result']:= Image.CurrentLayerIndex+1;
  result := srOk;
end;

function TImageActions.ScriptImageMoveLayerIndex(AVars: TVariableSet): TScriptResult;
begin
  try
    Image.MoveLayer(AVars.Integers['FromIndex']-1, AVars.Integers['ToIndex']-1);
    result := srOk;
  except
    on ex:exception do
      result := srException;
  end;
end;

function TImageActions.ScriptLayerFromFile(AVars: TVariableSet): TScriptResult;
var
  ids: ArrayOfLayerId;
  i: Integer;
  guidList: TScriptVariableReference;
begin
  if not AVars.IsDefined('FileName') then exit(srInvalidParameters) else
  begin
    ids := TryAddLayerFromFile(AVars.Strings['FileName']);
    if length(ids) = 0 then exit(srException) else
    begin
      if not AVars.IgnoreResult then
      begin
        guidList := AVars.AddGuidList('Result');
        for i := 0 to high(ids) do
          AVars.AppendGuid(guidList, Image.LayerGuid[Image.GetLayerIndexById(ids[i])]);
      end;
      exit(srOk);
    end;
  end;
end;

function TImageActions.ScriptImageGetRegistry(AVars: TVariableSet): TScriptResult;
var
  identifier: String;
begin
  identifier := AVars.Strings['Identifier'];
  if length(identifier)=0 then exit(srInvalidParameters);
  AVars.Strings['Result'] := Image.GetRegistry(identifier);
  result := srOk;
end;

function TImageActions.ScriptLayerGetId(AVars: TVariableSet): TScriptResult;
begin
  AVars.Guids['Result'] := Image.LayerGuid[Image.CurrentLayerIndex];
  result := srOk;
end;

function TImageActions.ScriptLayerGetRegistry(AVars: TVariableSet): TScriptResult;
var
  identifier: String;
begin
  identifier := AVars.Strings['Identifier'];
  if length(identifier)=0 then exit(srInvalidParameters);
  AVars.Strings['Result'] := Image.GetLayerRegistry(Image.CurrentLayerIndex, identifier);
  result := srOk;
end;

function TImageActions.ScriptLayerSaveAs(AVars: TVariableSet): TScriptResult;
var
  name, ext: String;
  layerCopy: TBGRABitmap;
  layerIdx, origIdx: Integer;
  writer: TFPCustomImageWriter;
  imgFormat, imgFormatFromName: TBGRAImageFormat;
  streamOut: TStream;
  layeredCopy: TBGRALayeredBitmap;
begin
  name := AVars.Strings['FileName'];
  imgFormatFromName := SuggestImageFormat(name);
  if AVars.Strings['Format'] = '' then
    imgFormat := imgFormatFromName
  else
    imgFormat := SuggestImageFormat(AVars.Strings['Format']);
  ext := UTF8LowerCase(ExtractFileExt(name));
  if imgFormat = ifUnknown then
  begin
    if ext = '.tmp' then
      imgFormat := ifPng
    else
      exit(srInvalidParameters);
  end;
  //wont overwrite a file that is probably not an image
  if FileManager.FileExists(name) and (imgFormatFromName = ifUnknown) then
    exit(srInvalidParameters);
  streamOut := FileManager.CreateFileStream(name, fmCreate);
  try
    layerIdx := Image.CurrentLayerIndex;
    if imgFormatFromName in[ifLazPaint, ifPhoxo, ifSvg, ifOpenRaster] then
    begin
      layeredCopy := TBGRALayeredBitmap.Create(Image.Width,Image.Height);
      try
        if Image.LayerOriginalDefined[layerIdx] and Image.LayerOriginalKnown[layerIdx] then
        begin
          origIdx := layeredCopy.AddOriginal(Image.LayerOriginal[layerIdx], false);
          layeredCopy.AddLayerFromOriginal(layeredCopy.Original[origIdx].Guid,
            Image.LayerOriginalMatrix[layerIdx], Image.BlendOperation[layerIdx],
            Image.LayerOpacity[layerIdx]);
          layeredCopy.LayerName[0] := Image.LayerName[layerIdx];
        end;
        layeredCopy.RenderOriginalsIfNecessary;
        layeredCopy.SaveToStreamAs(streamOut, SuggestImageExtension(imgFormat));
      finally
        layeredCopy.Free;
      end;
    end else
    begin
      layerCopy := TBGRABitmap.Create(Image.Width, Image.Height);
      layerCopy.FillTransparent;
      writer := CreateBGRAImageWriter(imgFormat, true);
      try
        layerCopy.PutImage(Image.LayerOffset[layerIdx].x, Image.LayerOffset[layerIdx].y,
          Image.LayerBitmap[layerIdx], dmSet);
        layerCopy.SaveToStream(streamOut, writer);
        result := srOk;
        AVars.Strings['Result'] := name;
      except
        on ex: Exception do
        begin
          FInstance.ShowError(rsSave, ex.Message);
          result := srException;
        end;
      end;
      layerCopy.Free;
      writer.Free;
    end;
  finally
    streamOut.Free;
  end;
end;

function TImageActions.ScriptLayerSelectId(AVars: TVariableSet): TScriptResult;
var
  idx: Integer;
  layerGuid: TGUID;
begin
  layerGuid := AVars.Guids['Id'];
  if layerGuid = GUID_NULL then exit(srInvalidParameters);
  idx := Image.GetLayerIndexByGuid(layerGuid);
  if idx = -1 then exit(srInvalidParameters)
  else if not Image.SetCurrentLayerByIndex(idx) then exit(srException)
  else exit(srOk);
end;

function TImageActions.ScriptLayerAddNew(AVars: TVariableSet): TScriptResult;
begin
  if not NewLayer then result := srException
  else
  begin
    if not AVars.IgnoreResult then
      AVars.Guids['Result'] := Image.LayerGuid[Image.CurrentLayerIndex];
    result := srOk;
  end;
end;

function TImageActions.ScriptImageSetRegistry(AVars: TVariableSet): TScriptResult;
var
  identifier: String;
begin
  identifier := AVars.Strings['Identifier'];
  if length(identifier)=0 then exit(srInvalidParameters);
  if not AVars.IsDefined('Value') then exit(srInvalidParameters);
  Image.SetRegistry(identifier, AVars.Strings['Value']);
  result := srOk;
end;

function TImageActions.ScriptLayerSetRegistry(AVars: TVariableSet): TScriptResult;
var
  identifier: String;
begin
  identifier := AVars.Strings['Identifier'];
  if length(identifier)=0 then exit(srInvalidParameters);
  if not AVars.IsDefined('Value') then exit(srInvalidParameters);
  Image.SetLayerRegistry(Image.CurrentLayerIndex, identifier, AVars.Strings['Value']);
  result := srOk;
end;

function TImageActions.ScriptPasteAsNewLayer(AVars: TVariableSet): TScriptResult;
var
  id, idx: Integer;
begin
  id := PasteAsNewLayer;
  if (id >= 0) and not AVars.IgnoreResult then
  begin
    idx := Image.GetLayerIndexById(id);
    AVars.Guids['Result'] := Image.LayerGuid[idx];
  end
  else AVars.Remove('Result');
  result := srOk;
end;

function TImageActions.ScriptLayerDuplicate(AVars: TVariableSet): TScriptResult;
begin
  if not DuplicateLayer then result := srException else
  begin
    if not AVars.IgnoreResult then
      AVars.Guids['Result'] := Image.LayerGuid[Image.CurrentLayerIndex];
    result := srOk;
  end;
end;

function TImageActions.ScriptPutImage(AVars: TVariableSet): TScriptResult;
var
  x, y, width, height, opacity, yb, dataPos, xb: integer;
  dataStr, modeStr: String;
  mode: TDrawMode;
  bmp: TBGRABitmap;
  p: PBGRAPixel;

  function HexDigit(APos: integer): byte;
  begin
    result := ord(dataStr[APos]);
    if result < ord('0') then result := 0
    else if result <= ord('9') then dec(result, ord('0'))
    else if result < ord('A') then result := 9
    else if result <= ord('F') then result := result - ord('A') + 10
    else result := 15;
  end;

  function HexValue(APos: integer): byte;
  begin
    result := (HexDigit(APos) shl 4) + HexDigit(APos+1);
  end;

begin
  x := AVars.Integers['X'];
  y := AVars.Integers['Y'];
  width := AVars.Integers['Width'];
  height := AVars.Integers['Height'];
  dataStr := AVars.Strings['Data'];
  modeStr := AVars.Strings['Mode'];
  opacity := AVars.Integers['Opacity'];
  case modeStr of
  'dmDrawWithTransparency': mode := dmDrawWithTransparency;
  'dmLinearBlend': mode := dmLinearBlend;
  'dmSet': mode := dmSet;
  'dmSetExceptTransparent': mode := dmSetExceptTransparent;
  'dmXor': mode := dmXor;
  else exit(srInvalidParameters);
  end;
  if (opacity < 0) or (opacity > 255) then exit(srInvalidParameters);
  if length(dataStr)<>width*height*8 then exit(srInvalidParameters);

  if (width = 0) or (height = 0) then exit(srOk);
  if opacity = 0 then exit(srOk);
  bmp := TBGRABitmap.Create(width,height);
  try
    dataPos := 1;
    for yb := 0 to height-1 do
    begin
      p := bmp.ScanLine[yb];
      for xb := 0 to width-1 do
      begin
        p^.alpha := HexValue(dataPos+6);
        if p^.alpha = 0 then p^ := BGRAPixelTransparent
        else
        begin
          p^.red := HexValue(dataPos);
          p^.green := HexValue(dataPos+2);
          p^.blue := HexValue(dataPos+4);
        end;
        inc(dataPos,8);
        inc(p);
      end;
    end;
    bmp.InvalidateBitmap;

    if PutImage(x,y,bmp,mode,opacity) then
    begin
      result := srOk;
      FInstance.UpdateWindows;
    end
    else
      result := srException;
  finally
    bmp.Free;
  end;
end;

function TImageActions.ScriptGetImage(AVars: TVariableSet): TScriptResult;
var
  str: string;
  strPos: integer;

  procedure writeStrHex(AValue: byte);
  const digits : array[0..15] of char = '0123456789ABCDEF';
  begin
    str[strPos] := digits[AValue shr 4];
    str[strPos+1] := digits[AValue and 15];
    inc(strPos, 2);
  end;

var
  x, y, width, height, yb, xb: Integer;
  copy, img: TBGRABitmap;
  ofs: TPoint;
  p: PBGRAPixel;


begin
  if not AVars.IsDefined('X') then
    x := 0 else x := AVars.Integers['X'];
  if not AVars.IsDefined('Y') then
    y := 0 else y := AVars.Integers['Y'];
  if not AVars.IsDefined('Width') then
    width := Image.Width-x else width := AVars.Integers['Width'];
  if not AVars.IsDefined('Height') then
    height := Image.Height-y else height := AVars.Integers['Height'];
  if (width > MaxImageWidth) or (height > MaxImageHeight) then exit(srException);
  if Image.SelectionLayerIsEmpty then
  begin
    copy := TBGRABitmap.Create(width, height);
    ofs := Image.LayerOffset[Image.CurrentLayerIndex];
    copy.PutImage(ofs.X, ofs.Y, Image.LayerBitmap[Image.CurrentLayerIndex], dmSet);
    img := copy
  end else
  begin
    copy := nil;
    img := Image.SelectionLayerReadonly;
  end;
  try
    setlength(str, img.width*img.height*8);
    strPos := 1;
    for yb := 0 to img.Height-1 do
    begin
      p := img.ScanLine[yb];
      for xb := img.Width-1 downto 0 do
      begin
        writeStrHex(p^.red);
        writeStrHex(p^.green);
        writeStrHex(p^.blue);
        writeStrHex(p^.alpha);
        inc(p);
      end;
    end;
  finally
    copy.Free;
  end;
  AVars.Strings['Result'] := str;
  result := srOk;
end;

function TImageActions.ScriptLayerFill(AVars: TVariableSet): TScriptResult;
var
  modeStr: String;
  mode: TDrawMode;
begin
  modeStr := AVars.Strings['Mode'];
  case modeStr of
  'dmDrawWithTransparency': mode := dmDrawWithTransparency;
  'dmLinearBlend': mode := dmLinearBlend;
  'dmSet': mode := dmSet;
  'dmSetExceptTransparent': mode := dmSetExceptTransparent;
  'dmXor': mode := dmXor;
  else exit(srInvalidParameters);
  end;
  if LayerFill(AVars.Pixels['Color'], mode) then
  begin
    result := srOk;
    FInstance.UpdateWindows;
  end
  else
    result := srException;
end;

function TImageActions.ScriptGetFrameIndex(AVars: TVariableSet): TScriptResult;
begin
  if Image.FrameIndex <> -1 then
    AVars.Integers['Result']:= Image.FrameIndex+1
  else
    AVars.Remove('Result');
  result := srOk;
end;

procedure TImageActions.ClearAlpha;
var
  c: TBGRAPixel;
begin
  c := ToolManager.BackColor;
  c.alpha := 255;
  ClearAlpha(c);
end;

procedure TImageActions.FillBackground;
var
  c: TBGRAPixel;
begin
  c := ToolManager.BackColor;
  c.alpha := 255;
  FillBackground(c);
end;

procedure TImageActions.ClearAlpha(AColor: TBGRAPixel);
var n: integer;
    p: PBGRAPixel;
    LayerAction: TLayerAction;
begin
  if not Image.CheckNoAction then exit;
  LayerAction := nil;
  try
    LayerAction := Image.CreateAction(true);
    LayerAction.SelectedImageLayer.ReplaceColor(BGRAPixelTransparent, AColor);
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

procedure TImageActions.FillBackground(AColor: TBGRAPixel);
var tempBmp: TBGRABitmap;
    LayerAction: TLayerAction;
    y: Integer;
    orig: TVectorOriginal;
    ab: TAffineBox;
    backRect: TRectShape;
begin
  if not Image.CheckNoAction then exit;
  LayerAction := nil;
  try
    if Image.LayerOriginalClass[Image.CurrentLayerIndex] = TVectorOriginal then
    begin
      Image.CurrentState.DiscardOriginalDiff := false;
      try
        orig := Image.LayerOriginal[Image.CurrentLayerIndex] as TVectorOriginal;
        backRect := TRectShape.Create(nil);
        ab := AffineMatrixInverse(Image.LayerOriginalMatrix[Image.CurrentLayerIndex]) *
              TAffineBox.AffineBox(rectF(-0.5, -0.5, Image.Width-0.5, Image.Height-0.5));
        backRect.Origin := ab.Center;
        backRect.XAxis := backRect.Origin + (ab.TopRight - ab.TopLeft)*0.5;
        backRect.YAxis := backRect.Origin + (ab.BottomLeft - ab.TopLeft)*0.5;
        backRect.BackFill.SolidColor := AColor;
        orig.InsertShape(backRect, 0);
      finally
        Image.CurrentState.DiscardOriginalDiff := true;
      end;
    end else
    begin
      LayerAction := Image.CreateAction(True);
      tempBmp := TBGRABitmap.Create(LayerAction.SelectedImageLayer.Width,1);
      for y := 0 to LayerAction.SelectedImageLayer.Height-1 do
      begin
         tempBmp.Fill(AColor);
         tempBmp.PutImage(0,-y,LayerAction.SelectedImageLayer,dmDrawWithTransparency);
         LayerAction.SelectedImageLayer.PutImage(0,y,tempBmp,dmSet);
      end;
      tempBmp.Free;
      image.LayerMayChangeCompletely(LayerAction.SelectedImageLayer);
      LayerAction.Validate;
    end;
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
    if CurrentTool in[ptLayerMapping,ptMoveSelection,ptRotateSelection] then
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

procedure TImageActions.DoBegin;
begin
  if CurrentTool in[ptMoveSelection,ptRotateSelection] then ChooseTool(ptHand);
  if ToolManager.ToolProvideCommand(tcFinish) then ToolManager.ToolCommand(tcFinish);
  Image.DoBegin;
end;

function TImageActions.DoEnd: boolean;
var
  found: boolean;
begin
  if CurrentTool in[ptMoveSelection,ptRotateSelection] then ChooseTool(ptHand);
  if ToolManager.ToolProvideCommand(tcFinish) then ToolManager.ToolCommand(tcFinish);
  Image.DoEnd(found, result);
  if not found then raise exception.Create(rsEndWithoutMatchingBegin);
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

function TImageActions.GetPixel(X, Y: Integer): TBGRAPixel;
var
  ofs: TPoint;
begin
  ofs := Image.LayerOffset[Image.CurrentLayerIndex];
  result := Image.LayerBitmap[Image.CurrentLayerIndex].GetPixel(X-ofs.X,y-ofs.Y);
end;

function TImageActions.PutImage(X, Y: integer; AImage: TBGRACustomBitmap;
  AMode: TDrawMode; AOpacity: byte): boolean;
var
  LayerAction: TLayerAction;
begin
  result := false;
  if not Image.CheckNoAction then exit;
  LayerAction := nil;
  try
    LayerAction := Image.CreateAction(true);
    LayerAction.ChangeBoundsNotified:= true;
    LayerAction.SelectedImageLayer.PutImage(X,Y,AImage,AMode,AOpacity);
    LayerAction.NotifyChange(LayerAction.SelectedImageLayer, RectWithSize(X,Y,AImage.Width,AImage.Height));
    LayerAction.Validate;
    result := true;
  except
    on ex:Exception do
      FInstance.ShowError('PutImage',ex.Message);
  end;
  LayerAction.Free;
end;

function TImageActions.LayerFill(AColor: TBGRAPixel; AMode: TDrawMode): boolean;
var
  LayerAction: TLayerAction;
begin
  if (AColor.alpha=0) and (AMode in[dmDrawWithTransparency,dmLinearBlend]) then exit(true);
  result := false;
  if not Image.CheckNoAction then exit;
  LayerAction := nil;
  try
    LayerAction := Image.CreateAction(true);
    LayerAction.ChangeBoundsNotified:= true;
    LayerAction.SelectedImageLayer.Fill(AColor, AMode);
    LayerAction.NotifyChange(LayerAction.SelectedImageLayer,
        rect(0,0,LayerAction.SelectedImageLayer.Width,
              LayerAction.SelectedImageLayer.Height));
    LayerAction.Validate;
    result := true;
  except
    on ex:Exception do
      FInstance.ShowError('LayerFill',ex.Message);
  end;
  LayerAction.Free;
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
    if not (CurrentTool in[ptDeformation,ptLayerMapping,ptMoveSelection,ptRotateSelection]) then
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
    if (CurrentTool in[ptRotateSelection,ptMoveSelection,ptDeformation,ptLayerMapping]) then
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

procedure TImageActions.Flatten;
begin
  ChooseTool(ptHand);
  image.Flatten;
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

function TImageActions.TryAddLayerFromFile(AFilenameUTF8: string; ALoadedImage: TBGRABitmap = nil): ArrayOfLayerId;

  function ComputeStretchMatrix(ASourceWidth, ASourceHeight: single): TAffineMatrix;
  var
    ratio: Single;
  begin
    ratio := max(ASourceWidth/Image.Width, ASourceHeight/Image.Height);
    result := AffineMatrixTranslation(-ASourceWidth/2, -ASourceHeight/2);
    if ratio > 1 then result := AffineMatrixScale(1/ratio, 1/ratio)*result;
    result := AffineMatrixTranslation(Image.Width/2, Image.Height/2)*result;
  end;

var
  layeredBmp: TBGRACustomLayeredBitmap;

  procedure ImportLayeredBmp;
  var
    m: TAffineMatrix;
    i: Integer;
    ofsF: TPointF;
    bmpOrig: TBGRALayerImageOriginal;
    doFound, somethingDone: boolean;
  begin
    m := ComputeStretchMatrix(layeredBmp.Width, layeredBmp.Height);
    try
      Image.DoBegin;
      for i := 0 to layeredBmp.NbLayers-1 do
      begin
        if (layeredBmp.LayerOriginalGuid[i] <> GUID_NULL) and
           layeredBmp.LayerOriginalKnown[i] then
        begin
          if not AddLayerFromOriginal(layeredBmp.LayerOriginal[i].Duplicate,
            layeredBmp.LayerName[i], m*layeredBmp.LayerOriginalMatrix[i],
            layeredBmp.BlendOperation[i], layeredBmp.LayerOpacity[i]) then break;
        end else
        begin
          if IsAffineMatrixTranslation(m) then
          begin
            ofsF := m*PointF(layeredBmp.LayerOffset[i].x, layeredBmp.LayerOffset[i].y);
            if not NewLayer(layeredBmp.GetLayerBitmapCopy(i), layeredBmp.LayerName[i],
                     Point(round(ofsF.X), round(ofsF.Y)),
                     layeredBmp.BlendOperation[i], layeredBmp.LayerOpacity[i]) then break;
          end else
          begin
            bmpOrig := TBGRALayerImageOriginal.Create;
            bmpOrig.AssignImage(layeredBmp.GetLayerBitmapDirectly(i));
            if not AddLayerFromOriginal(bmpOrig, layeredBmp.LayerName[i],
              m * AffineMatrixTranslation(layeredBmp.LayerOffset[i].x, layeredBmp.LayerOffset[i].y),
              layeredBmp.BlendOperation[i], layeredBmp.LayerOpacity[i]) then break;
          end;
        end;
        setlength(result, length(result)+1);
        result[high(result)] := Image.LayerId[image.CurrentLayerIndex];
      end;
    finally
      image.DoEnd(doFound, somethingDone);
    end;
  end;

var
  imgFormat: TBGRAImageFormat;
  s: TStream;
  newPicture: TBGRABitmap;
  flattened: TBGRABitmap;
  ext: String;

begin
  result := nil;
  if not AbleToLoadUTF8(AFilenameUTF8) then
  begin
    FInstance.ShowMessage(rsOpen,rsFileExtensionNotSupported);
    FreeAndNil(ALoadedImage);
    exit;
  end;
  try
    imgFormat := Image.DetectImageFormat(AFilenameUTF8);
    case imgFormat of
    ifLazPaint, ifOpenRaster, ifSvg, ifPaintDotNet, ifPhoxo:
    begin
      ext := UTF8LowerCase(ExtractFileExt(AFilenameUTF8));
      layeredBmp := TryCreateLayeredBitmapReader(ext);
      if layeredBmp is TBGRALayeredSVG then
      with TBGRALayeredSVG(layeredBmp) do
      begin
        ContainerWidth := Image.Width;
        ContainerHeight := Image.Height;
        DPI:= Screen.PixelsPerInch;
        DefaultLayerName:= rsLayer;
      end;
      try
        s := FileManager.CreateFileStream(AFilenameUTF8, fmOpenRead or fmShareDenyWrite);
        try
          if Assigned(FInstance) then FInstance.StartLoadingImage(AFilenameUTF8);
          try
            layeredBmp.LoadFromStream(s);
          finally
            if Assigned(FInstance) then FInstance.EndLoadingImage;
          end;
          if layeredBmp.NbLayers > 1 then
          begin
            case QuestionDlg(rsOpen, AppendQuestionMark(rsFlattenImage), mtInformation,
              [mrYes, rsYes, mrNo, rsNo, mrCancel, rsCancel], 0) of
            mrYes: begin
                flattened := layeredBmp.ComputeFlatImage;
                FreeAndNil(layeredBmp);
                layeredBmp:= TBGRALayeredBitmap.Create(flattened.Width, flattened.Height);
                TBGRALayeredBitmap(layeredBmp).AddOwnedLayer(flattened);
                ImportLayeredBmp;
              end;
            mrNo: ImportLayeredBmp;
            end;
          end else
            ImportLayeredBmp;
        finally
          s.Free;
        end;
      finally
        layeredBmp.Free;
      end;
    end
    else
      begin
        if Assigned(ALoadedImage) then
        begin
          newPicture := ALoadedImage;
          ALoadedImage := nil;
        end
        else
        begin
          if Assigned(FInstance) then FInstance.StartLoadingImage(AFilenameUTF8);
          try
            newPicture := LoadFlatImageUTF8(AFilenameUTF8).bmp;
          finally
            if Assigned(FInstance) then FInstance.EndLoadingImage;
          end;
        end;
        AddLayerFromBitmap(newPicture, ExtractFileName(AFilenameUTF8));
        setlength(result, 1);
        result[0] := Image.LayerId[image.CurrentLayerIndex];
      end;
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
    if CurrentTool in [ptDeformation,ptRotateSelection,ptMoveSelection,
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
  AName: string; AMatrix: TAffineMatrix; ABlendOp: TBlendOperation; AOpacity: byte): boolean;
begin
  if AOriginal <> nil then
  begin
    if CurrentTool in [ptDeformation,ptRotateSelection,ptMoveSelection,
         ptLayerMapping,ptEditShape] then
      ChooseTool(ptHand);
    if image.CheckNoAction then
    begin
      if not Image.SelectionMaskEmpty then ReleaseSelection;
      result := NewLayer(AOriginal, AName, ABlendOp, AMatrix, AOpacity);
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

procedure TImageActions.Rotate180;
begin
  Image.Rotate180;
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
  layeraction.ChangeBoundsNotified:= true;
  layeraction.ReleaseSelection;
  layeraction.Validate;
  layeraction.Free;
end;

function TImageActions.ScriptSelectLayerIndex(AVars: TVariableSet): TScriptResult;
var
  index: Int64;
begin
  index := AVars.Integers['Index'];
  if (AVars.Integers['Index'] < 1) or (AVars.Integers['Index'] > Image.NbLayers) then exit(srInvalidParameters);
  if not Image.SetCurrentLayerByIndex(index-1) then result := srException
  else result := srOk;
end;

function TImageActions.ScriptClearAlpha(AVars: TVariableSet): TScriptResult;
begin
  if AVars.IsDefined('BackColor') then
    ClearAlpha(AVars.Pixels['BackColor'])
  else
    ClearAlpha;
  result := srOk;
end;

function TImageActions.ScriptFillBackground(AVars: TVariableSet): TScriptResult;
begin
  if AVars.IsDefined('BackColor') then
    FillBackground(AVars.Pixels['BackColor'])
  else
    FillBackground;
  result := srOk;
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

function TImageActions.PasteAsNewLayer: integer;
var partial: TBGRABitmap;
  orig: TVectorOriginal;
begin
  result := -1;
  try
    if ClipboardHasShapes then
    begin
      orig := TVectorOriginal.Create;
      PasteShapesFromClipboard(orig, AffineMatrixIdentity, EmptyRectF);
      if AddLayerFromOriginal(orig, '') then
        result := Image.LayerId[Image.CurrentLayerIndex];
    end else
    begin
      partial := GetBitmapFromClipboard;
      if partial<>nil then
      begin
        if partial.NbPixels <> 0 then
        begin
          AddLayerFromBitmap(partial,'');
          ChooseTool(ptMoveLayer);
          result := Image.LayerId[Image.CurrentLayerIndex];
        end
        else
          partial.Free;
      end else
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

function TImageActions.NewLayer: boolean;
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
    if CurrentTool in[ptMoveLayer,ptRotateLayer,ptZoomLayer,ptLayerMapping,ptDeformation] then
      ChooseTool(ptHand);
    ToolManager.ToolCloseDontReopen;
    Image.AddNewLayer;
    ToolManager.ToolOpen;
    FInstance.ScrollLayerStackOnItem(Image.CurrentLayerIndex);
    result := true;
  end else
    result := false;
end;

function TImageActions.NewLayer(ALayer: TBGRABitmap; AName: string;
  ABlendOp: TBlendOperation; AOpacity: byte): boolean;
begin
  if image.NbLayers < MaxLayersToAdd then
  begin
    if CurrentTool in[ptMoveLayer,ptRotateLayer,ptZoomLayer,ptLayerMapping,ptDeformation] then
      ChooseTool(ptHand);
    ToolManager.ToolCloseDontReopen;
    Image.AddNewLayer(ALayer, AName, ABlendOp, AOpacity);
    ToolManager.ToolOpen;
    FInstance.ScrollLayerStackOnItem(Image.CurrentLayerIndex);
    result := true;
  end else
  begin
    FInstance.ShowMessage(rsLayers, rsTooManyLayers);
    ALayer.Free;
    result := false;
  end;
end;

function TImageActions.NewLayer(ALayer: TBGRABitmap; AName: string;
  AOffset: TPoint; ABlendOp: TBlendOperation; AOpacity: byte): boolean;
begin
  if image.NbLayers < MaxLayersToAdd then
  begin
    if CurrentTool in[ptMoveLayer,ptRotateLayer,ptZoomLayer,ptLayerMapping,ptDeformation] then
      ChooseTool(ptHand);
    ToolManager.ToolCloseDontReopen;
    Image.AddNewLayer(ALayer, AName, AOffset, ABlendOp, AOpacity);
    ToolManager.ToolOpen;
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
  AName: string; ABlendOp: TBlendOperation; AMatrix: TAffineMatrix; AOpacity: byte): boolean;
begin
  if image.NbLayers < MaxLayersToAdd then
  begin
    if CurrentTool in[ptMoveLayer,ptRotateLayer,ptZoomLayer,ptLayerMapping,ptDeformation] then
      ChooseTool(ptHand);
    ToolManager.ToolCloseDontReopen;
    Image.AddNewLayer(ALayer, AName, ABlendOp, AMatrix, AOpacity);
    ToolManager.ToolOpen;
    FInstance.ScrollLayerStackOnItem(Image.CurrentLayerIndex);
    result := true;
  end else
  begin
    FInstance.ShowMessage(rsLayers, rsTooManyLayers);
    ALayer.Free;
    result := false;
  end;
end;

function TImageActions.DuplicateLayer: boolean;
begin
  if image.NbLayers < MaxLayersToAdd then
  begin
    Image.DuplicateLayer;
    FInstance.ScrollLayerStackOnItem(Image.CurrentLayerIndex);
    result := true;
  end else
    result := false;
end;

procedure TImageActions.RasterizeLayer;
begin
  if CurrentTool in[ptMoveLayer,ptRotateLayer,ptZoomLayer,ptLayerMapping,ptDeformation] then
    ChooseTool(ptHand);
  ToolManager.ToolCloseDontReopen;
  Image.RasterizeLayer;
  ToolManager.ToolOpen;
  FInstance.ScrollLayerStackOnItem(Image.CurrentLayerIndex);
end;

procedure TImageActions.MergeLayerOver;
begin
  if (Image.CurrentLayerIndex <> -1) and (image.NbLayers > 1) then
  begin
    ChooseTool(ptHand);
    Image.MergeLayerOver;
    FInstance.ScrollLayerStackOnItem(Image.CurrentLayerIndex);
  end;
end;

function TImageActions.RemoveLayer: boolean;
var idx: integer;
begin
  if (Image.CurrentLayerIndex <> -1) and (Image.NbLayers > 1) then
  begin
    idx := Image.CurrentLayerIndex;
    if CurrentTool in[ptMoveLayer,ptRotateLayer,ptZoomLayer,ptLayerMapping,ptDeformation] then
      ChooseTool(ptHand);
    ToolManager.ToolCloseDontReopen;
    Image.RemoveLayer;
    ToolManager.ToolOpen;
    FInstance.ScrollLayerStackOnItem(idx);
    result := true;
  end else result := false;
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

