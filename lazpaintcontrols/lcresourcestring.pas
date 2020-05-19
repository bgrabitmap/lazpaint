// SPDX-License-Identifier: GPL-3.0-only
unit LCResourceString;

{$mode objfpc}{$H+}

interface

uses BGRABitmapTypes;

function GradientTypeToTranslatedStr(AGradientType: TGradientType): string;

resourcestring
  rsPreview = 'Preview';
  rsNoFill = 'No fill';

  rsSolidColor = 'Solid color';
  rsColor = 'Color';
  rsOpacity = 'Opacity';

  rsGradientFill = 'Gradient fill';
  rsSwapColors = 'Swap colors';
  rsStartOpacity = 'Start opacity';
  rsEndOpacity = 'End opacity';

  rsGradientLinear = 'Linear';
  rsGradientReflected = 'Reflected';
  rsGradientDiamond = 'Diamond';
  rsGradientRadial = 'Radial';
  rsGradientAngular = 'Angular';

  rsGradientRepetition = 'Gradient repetition';
  rsGrPad = 'Pad';
  rsGrRepeat = 'Repeat';
  rsGrReflect = 'Reflect';
  rsGrSine = 'Sine';

  rsColorInterpolation = 'Color interpolation';
  rsCiStdRGB = 'sRGB';
  rsCiLinearRGB = 'RGB';
  rsCiLinearHSLPositive = 'HSL CW';
  rsCiLinearHSLNegative = 'HSL CCW';
  rsCiGSBPositive = 'Corr. HSL CW';
  rsCiGSBNegative = 'Corr. HSL CCW';

  rsTextureFill = 'Texture fill';
  rsTextureRepetition = 'Texture repetition';
  rsLoadTexture = 'Load texture';
  rsTrNone = 'No repetition';
  rsTrRepeatX = 'Repeat X';
  rsTrRepeatY = 'Repeat Y';
  rsTrRepeatBoth = 'Repeat both';

  rsEditGradTexPoints = 'Edit gradient/texture points';
  rsAdjustToShape = 'Adjust to shape';

  rsNotTextureFill = 'It is not a texture fill';
  rsIncompatibleType = 'Incompatible type';

  rsLightPosition = 'Light position';

  rsShapeClassNotSpecified = 'Shape class not specified';
  rsUnknownShapeClass = 'Unknown shape class "%1"';
  rsShapeNotFound = 'Shape not found';
  rsIndexOutOfBounds = 'Index out of bounds';

implementation

function GradientTypeToTranslatedStr(AGradientType: TGradientType): string;
begin
  case AGradientType of
  gtLinear: result := rsGradientLinear;
  gtReflected: result := rsGradientReflected;
  gtDiamond: result := rsGradientDiamond;
  gtRadial: result := rsGradientRadial;
  gtAngular: result := rsGradientAngular;
  else result := '?';
  end;
end;

end.

