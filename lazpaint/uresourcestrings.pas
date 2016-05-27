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
  rsScript = 'Script';
  rsFunctionNotDefined = 'The function %1 is not defined.';
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
  rsFileName = 'Filename';
  rsFileSize = 'Size';
  rsFileType = 'Type';
  rsFileDate = 'Date';
  rsBytes = 'Bytes';
  rsFolder = 'Folder';
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
  rsHoldShiftForSquare = 'Hold SHIFT to draw a square or a circle';
  rsHoldCtrlSnapToPixel = 'Hold CTRL to snap to pixels';
  rsReturnValides = 'Press ENTER to validate';
  rsBackspaceRemoveLastPoint = 'Press BACKSPACE to remove last point';
  rsCtrlRestrictRotation = 'Hold CTRL to restrict rotation angle';
  rsAltShiftScaleMode = 'Hold ALT or SHIFT to scale';
  rsCurveModeHint = 'Press S or X to set the curve mode of the last point';
  rsBlendOpNotUsedForBackground = 'The blend operation is applied only if there is a layer underneath';
  rsRightClickForSource = 'Use RIGHT click to define source';

  rsEmptySelection='Selection is empty';
  rsEmptyLayer='Layer is empty';
  rsKeepEmptySpace='Keep empty space around opaque pixels?';

  rsRepeatImage='Repeat image';
  rsCanvasSize='Canvas size';

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

  rsEditMask='Edit mask';

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
  rsNoAndProceedToNext='Do not save and open another file';
  rsInformation='Information';
  rsError='Error';
  rsThereAreNoCheckedItems='There are no checked items. Check some items or add some new ones.';
  rsThereIsNoFileNameGivenForThisFileUseSaveAs='There is no file name given for this file. Use "Save as..." from the main menu.';
  rsFileNotFound='File not found!';
  rsCannotOpenFile='Cannot open file';
  rsOpenMultipleImageFiles='Open multiple image files';
  rsMoreThanOneFile='You are trying to open more than one file. How would you like these files to be opened?';
  rsOpenFilesAsLayers='Open files as layers in a single image';
  rsAddToImageList='Add files to the image processing list';
  rsOpenFirstFileOnly='Open the first file only';
  rsLayeredImage = 'Layered image';
  rsBitmap = 'Bitmap';
  rsAnimatedGIF = 'Animated GIF';
  rsIconOrCursor = 'Icon/cursor';
  rsTotalImages= 'Total images: %1';
  rsToDoImages= 'Images left: %1';
  rsErrorOnOpeningFile='Error on opening file "%1"';
  rsFollowingErrorsOccured='Following errors occured:';

  rsRemovableDrive = 'Removable';
  rsFixedDrive = 'Fixed';
  rsNetworkDrive = 'Network';
  rsCdRom = 'CD';
  rsRamDisk = 'RAM disk';

  rsFileSystem = 'File system';
  rsStorageDevice = 'Device';
  rsOverwriteFile = 'File already exists. Do you want to overwrite it?';
  rsDeleteFile = 'Delete';
  rsConfirmMoveToTrash = 'Are you sure you want to move this file to the trash?';
  rsConfirmMoveMultipleToTrash = 'Are you sure you want to move these %1 files to the trash?';

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

implementation

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
  if AAction.Caption = '' then
    AAction.Caption := ApplyShortcutStr(AAction.Hint, AShortcut)
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

