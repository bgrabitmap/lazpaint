// SPDX-License-Identifier: GPL-3.0-only
unit UResourceStrings;

{$mode objfpc}

interface

uses
  Classes, SysUtils, Dialogs, ActnList, BGRABitmapTypes;

procedure AppendShortcut(AAction: TAction; AShortcut: string);
function CaptionToResampleFilter(AText: string): TResampleFilter;

const
  rsOpenRaster = 'OpenRaster';
  rsPaintNET = 'Paint.NET';
  rsPhotoshop = 'Photoshop';

resourcestring
  rsLazPaint = 'LazPaint';
  rsDonate = 'Donate...';
  rsScript = 'Script';
  rsFunctionNotDefined = 'The function %1 is not defined.';
  rsPythonUnexpectedVersion = 'Expected Python version %1 but %2 found.';
  rsOpening='Opening';
  rsLoading='Loading';
  rsRecentDirectories='Recent directories:';
  rsNoName='noname';
  rsOtherBlendOp='Other...';
  rsNormalBlendOp='Normal';
  rsAllApplications='all';
  rsLazPaintOnly='LazPaint only';
  rsEnterLayerName='Enter layer name:';
  rsFileExtensionNotSupported='This file extension is not supported.';
  rsFileFormatNotRecognized='The file format has not been recognized.';
  rsErrorDecodingRaw='Error decoding raw image.';
  rsErrorLoadingOriginal='Error while loading original however layer can be rasterized.';
  rsRasterLayer = 'Raster layer';
  rsVisible = 'Visibile';
  rsTransformedRasterLayer = 'Transformed raster layer';
  rsVectorialLayer = 'Vectorial layer';
  rsUnknownOriginal = 'Unknown original';
  rsFileName = 'Filename';
  rsFileSize = 'Size';
  rsFileType = 'Type';
  rsFileDate = 'Date';
  rsBytes = 'Bytes';
  rsFolder = 'Folder';
  rsIconSize = 'Icon size';
  rsAlignShape = 'Align shape';
  rsAutodetect = 'Autodetect';
  rsNotReasonableFormat='It is not reasonable to save such a big image in this file format.';
  rsEditTexture='Edit texture';
  rsEditSelection='Edit selection';
  rsExitRequest='Exit request';
  rsSaveChanges='Current bitmap has been modified. Do you want to save changes?';
  rsSaveAsButtonHint='Save with specified filename';
  rsCloseRequest='Close request';
  rsKeepChanges='Do you want to keep changes?';
  rsNothingToBeDeformed='There is nothing to be deformed';
  rsMovingOrRotatingSelection='Moving or rotating selection';
  rsRetrieveSelectedArea='Do you want to retrieve selected area?';
  rsTextureMapping='Texture mapping';
  rsTransformSelectionContent='Do you want to transform content of the selection?';
  rsToolOnInvisibleLayer = 'Tool cannot be used on an invisible layer';
  rsNothingToBeRetrieved='There is nothing to be retrieved';
  rsCrop='Crop';
  rsMergeSelection='Do you want to merge selection?';
  rsSave='Save';
  rsNewImage='New image';
  rsDuplicateImage='Duplicate image';
  rsOpen='Open';
  rsReload='Reload';
  rsReloadChanged='Bitmap has been modified. Do you really want to reload?';
  rsConflictingActions = 'Conflicting actions';
  rsActionInProgress = 'Action in progress';
  rsTransferSelectionToOtherLayer='Transfer selection to other layer?';
  rsImageTooBig = 'Image is too big';
  rsLatestVersion = 'The latest version of LazPaint available online is';
  rsMustReleaseSelection = 'You must first release the selection';
  rsMustShowLayer = 'You must first make the layer visible';
  rsCtrl = 'CTRL';
  rsAlt = 'ALT';
  rsShift = 'SHIFT';
  rsCmd = 'CMD';
  rsHoldKeyForSquare = 'Hold %1 to draw a square or a circle';
  rsHoldKeySnapToPixel = 'Hold %1 to snap to pixels';
  rsReturnValides = 'Press ENTER to validate';
  rsBackspaceRemoveLastPoint = 'Press BACKSPACE to remove last point';
  rsRightClickFinishShape = 'Use RIGHT click to finish shape';
  rsHoldKeyRestrictRotation = 'Hold %1 to restrict rotation angle';
  rsHoldKeysScaleMode = 'Hold %1 or %2 to scale';
  rsCurveModeHint = 'Press S or X to set the curve mode of the last point';
  rsSelectBlendOperation = 'Select blend operation';
  rsBlendOpNotUsedForBackground = 'The blend operation is applied only if there is a layer underneath';
  rsZoomLayerStackIn = 'Zoom layer stack in';
  rsZoomLayerStackOut = 'Zoom layer stack out';
  rsRightClickForSource = 'Use RIGHT click to define source';

  rsFileCannotBeEmpty = 'File cannot be empty';
  rsEmptySelection='Selection is empty';
  rsEmptyLayer='Layer is empty';
  rsKeepEmptySpace='Keep empty space around opaque pixels?';

  rsImage='Image';
  rsRepeatImage='Repeat image';
  rsCanvasSize='Canvas size';
  rsResamplingImage='Resampling image...';
  rsFlattenImage='Flatten image';

  rsRed='Red';
  rsGreen='Green';
  rsBlue='Blue';
  rsSaturation='Saturation';
  rsHue='Hue';

  rsRGB = 'RGB';
  rsLinearRGB = 'Linear RGB';
  rsHueCW = 'Hue CW';
  rsHueCCW = 'Hue CCW';
  rsCorrectedHueCW = 'Corr. Hue CW';
  rsCorrectedHueCCW = 'Corr. Hue CCW';

  rsPen = 'Pen';
  rsBack = 'Back';
  rsTextOutline = 'Text outline';

  rsAddToPalette = 'Add color to palette';
  rsRemoveFromPalette = 'Remove color from palette';
  rsColors='Colors';
  rsLight='Light';
  rsOpacity='Opacity';
  rsLightPosition='Light position';
  rsSourcePosition='Source position';
  rsBrightness='Brightness';
  rsContrast='Contrast';
  rsPresetName='Preset name';

  rsIntensity='Intensity';
  rsLightness='Lightness';
  rsColorDescription='Color description: click to type in a color with the keyboard using color names or CSS notation.';

  rsHotSpot='Hot spot';
  rsEntries='Entries';
  rsLayers='Layers';
  rsLayer='Layer';
  rsFrames='Frames';
  rsLoopCount='Loop count';
  rsInfinity='Infinity';

  rsEditMask='Edit mask';
  rsMask='Mask';
  rsChannels='Channels';
  rsMakeMonochromatic='Make monochromatic';
  rsNotChromaticChannel='This is not a chromatic channel';
  rsMerge='Merge';
  rsCurrentLayerNotSplit='Current layer is not split';
  rsSplitRGB='Split RGB';
  rsSplitCMYK='Split CMYK';
  rsCyan='Cyan';
  rsMagenta='Magenta';
  rsYellow='Yellow';
  rsBlack='Black';
  rsSplitHSL='Split HSL';
  rsLayerAlreadySplit='Layer already split';
  rsLayerEffect='Layer effect';
  rsRender='Render';
  rsVersion='Version';
  rsColorOverlay='Color overlay';
  rsDropShadow='Drop shadow';
  rsInnerLight='Inner light';
  rsInnerShadow='Inner shadow';
  rsStroke='Stroke';
  rsMaskFromAlphaChannel='Mask from alpha channel';
  rsNewMask='New mask';
  rsFractalTree='Fractal tree';
  rsInvalidAngle='Invalid angle';
  rsVerticalSize='Vertical size';
  rsLava='Lava';

  rsFast='Fast';
  rsLinear='Linear';
  rsHalfCosine='Half-cosine';
  rsCosine='Cosine';
  rsMitchell='Mitchell';
  rsSpline='Spline';
  rsLanczos='Lanczos %1';
  rsBestQuality='Best quality';

  rsPortait = 'Portrait';
  rsLandscape = 'Landscape';

  rsNoActiveLayer='No active layer';
  rsNoActiveSelection='There is no active selection';
  rsUnableToLoadFile='Unable to load file : ';
  rsUnableToApplyFilter='Unable to apply filter : ';
  rsInvalidOpacity='Invalid opacity : ';
  rsInvalidResampleSize='Invalid resample size : ';
  rsInvalidSizeForNew='Invalid size for new : ';
  rsUnknownCommand='Unknown command : ';
  rsUnableToSaveFile='Unable to save file : ';
  rsInternalError='Internal error';

  rsExpectNParameters='expects N parameters : ';
  rsExpect1Parameter='expects one parameter : ';
  rsExpect2Parameters='expects two parameters : ';
  rsInvalidParameters='Invalid parameters';
  rsException='An exception was encountered';

  rsPercent='%';
  rsPx='px';

  rsNumber= '№';
  rsToDo= 'To do';
  rsAllSupportedFiletypes= 'All supported filetypes';
  rsFileNotSaved='File not saved';
  rsYes='Yes';
  rsNo='No';
  rsOkay='Okay';
  rsCancel='Cancel';
  rsCancelledByUser='Cancelled by user';
  rsNoAndProceedToNext='Do not save and open another file';
  rsInformation='Information';
  rsDownload='Download';
  rsError='Error';
  rsEndWithoutMatchingBegin = 'End without matching begin';
  rsThereAreNoCheckedItems='There are no checked items. Check some items or add some new ones.';
  rsThereIsNoFileNameGivenForThisFileUseSaveAs='There is no file name given for this file. Use "Save as..." from the main menu.';
  rsFileNotFound='File not found!';
  rsCannotOpenFile='Cannot open file';
  rsOpenMultipleImageFiles='Open multiple image files';
  rsMoreThanOneFile='You are trying to open more than one file. How would you like these files to be opened?';
  rsOpenFilesAsLayers='Open files as layers in a single image';
  rsTooManyLayers='Too many layers';
  rsTooManyShapesInLayer='Too many shapes in layer';
  rsTooManyActions='Too many actions';
  rsCannotDrawShapeOnSVGLayer='Cannot draw shape on SVG layer';
  rsAddToImageList='Add files to the image processing list';
  rsOpenFirstFileOnly='Open the first file only';
  rsLayeredImage = 'Layered image';
  rsBitmap = 'Bitmap';
  rsAnimatedGIF = 'Animated GIF';
  rsIconOrCursor = 'Icon/cursor';
  rsTotalImages= 'Total images: %1';
  rsToDoImages= 'Images left: %1';
  rsErrorOnOpeningFile='Error on opening file "%1"';
  rsFollowingErrorsOccurred='Following errors occurred:';

  rsRemovableDrive = 'Removable';
  rsFixedDrive = 'Fixed';
  rsNetworkDrive = 'Network';
  rsCdRom = 'CD';
  rsRamDisk = 'RAM disk';

  rsFileSystem = 'File system';
  rsStorageDevice = 'Device';
  rsOverwriteFile = 'File already exists. Do you want to overwrite it?';
  rsDeleteFile = 'Delete';
  rsDeleteImageEntry = 'Delete selected image entry?';
  rsIconImageAlreadyExists = 'There is already an image with this size and depth.';
  rsConfirmMoveToTrash = 'Are you sure you want to move this file to the trash?';
  rsConfirmMoveMultipleToTrash = 'Are you sure you want to move these %1 files to the trash?';
  rsConfirmDeleteFromContainer = 'Are you sure you want to delete this file from the container?';
  rsConfirmDeleteMultipleFromContainer = 'Are you sure you want to delete these %1 files from the container?';
  rsEnterFolderOrContainerName = 'Enter name for new folder or container (using RES or LRS extension):';
  rsFolderOrContainerAlreadyExists = 'Folder or container already exists.';
  rsInvalidName = 'Invalid name';
  rsDirectoryNotEmpty = 'Directory is not empty';

  rsClearPalette = 'Clear palette';
  rsDefaultPalette = 'Default palette';
  rsMakeNColorsPaletteFromBitmap = 'Make %1-colors palette from image';
  rsPosterizeLayerUsingPalette = 'Posterize layer using palette';
  rsDitherLayerUsingPalette = 'Dither layer using palette';
  rsLoadPalette = 'Load palette...';
  rsLoadAndMergePalette = 'Load and merge palette...';
  rsSavePaletteAs = 'Save palette as...';
  rsPaletteIncludesAlphaChannel = 'Palette includes alpha channel';
  rsShowPalette = 'Show palette';
  rsPaletteOptions = 'Palette options';

function RemoveTrail(ACaption: string): string;

implementation

function RemoveTrail(ACaption: string): string;
begin
  result := Trim(ACaption);
  while (result<>'') and (result[length(result)] in[' ',':','.','?','!']) do
    delete(result,length(result),1);
end;

function ApplyShortcutStr(ACaption, AShortcut: string): string;
var idxPar: integer;
begin
  idxPar := Pos('(',ACaption);
  if idxPar <> 0 then
    result := Trim(copy(ACaption,1,idxPar-1))
  else
    result := Trim(ACaption);
  result := result+' ('+AShortcut+')';
end;

procedure AppendShortcut(AAction: TAction; AShortcut: string);
begin
  if AAction.Hint <> '' then
    AAction.Hint := ApplyShortcutStr(AAction.Hint, AShortcut);
  if AAction.Caption = '' then
    AAction.Caption := AAction.Hint
  else
    AAction.Caption := ApplyShortcutStr(AAction.Caption, AShortcut);
end;

function CaptionToResampleFilter(AText: string): TResampleFilter;
begin
  result := rfLinear;
  if AText = rsFast then
    result := rfBox else
  if AText = rsLinear then
    result := rfLinear else
  if AText = rsHalfCosine then
    result := rfHalfCosine else
  if AText = rsCosine then
    result := rfCosine else
  if AText = rsMitchell then
    result := rfMitchell else
  if AText = rsSpline then
    result := rfSpline else
  if AText = StringReplace(rsLanczos,'%1','2',[]) then
    result := rfLanczos2 else
  if AText = StringReplace(rsLanczos,'%1','3',[]) then
    result := rfLanczos3 else
  if AText = StringReplace(rsLanczos,'%1','4',[]) then
    result := rfLanczos4 else
  if AText = rsBestQuality then
    result := rfBestQuality;
end;

end.

