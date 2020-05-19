// SPDX-License-Identifier: GPL-3.0-only
unit UFilters;

{$mode objfpc}

interface

uses
  Classes, SysUtils, LazPaintType, uscripting;

function ExecuteFilter(AInstance: TLazPaintCustomInstance; filter: TPictureFilter;
  AParameters: TVariableSet; skipDialog: boolean = false; defaultCaption: string = ''): TScriptResult;

implementation

uses UFilterConnector, BGRABitmap, BGRABitmapTypes, UGraph, BGRAGradients, Dialogs, UColorFilters;

function ExecuteFilter(AInstance: TLazPaintCustomInstance; filter: TPictureFilter;
  AParameters: TVariableSet; skipDialog: boolean = false; defaultCaption: string = ''): TScriptResult;
var
    FilterConnector: TFilterConnector;
    filteredLayer: TBGRABitmap;

  function GetCaption: string;
  begin
    result := Trim(AParameters.Strings['Caption']);
    if result = '' then result := defaultCaption;
  end;

  function GetSkip: boolean;
  begin
    result := skipDialog or AParameters.Booleans['Validate'];
  end;

  procedure DoBlurCustom;
  var
    blurMask,blurMaskCopy: TBGRABitmap;
  begin
    if GetSkip and (AInstance.Config.DefaultCustomBlurMaskUTF8 <> '') then
    begin
      try
        blurMask := TBGRABitmap.Create(AInstance.Config.DefaultCustomBlurMaskUTF8,True);
        blurMaskCopy := blurMask.FilterGrayscale as TBGRABitmap;
        blurMask.Fill(BGRABlack);
        blurMask.PutImage(0,0,blurMaskCopy,dmDrawWithTransparency);
        blurMaskCopy.Free;
        filteredLayer := FilterConnector.ActiveLayer.FilterCustomBlur(FilterConnector.WorkArea, blurMask) as TBGRABitmap;
        blurMask.Free;
      except
        on ex: exception do
          AInstance.ShowError(PictureFilterStr[filter],ex.Message);
      end;
    end
    else
      result := AInstance.ShowCustomBlurDlg(FilterConnector);
  end;

  procedure DoSimpleBlur;
  var
    blurType: TRadialBlurType;
    radiusX,radiusY: single;
  begin
    case filter of
      pfBlurPrecise: blurType := rbPrecise;
      pfBlurRadial: blurType := rbNormal;
      pfBlurCorona: blurType := rbCorona;
      pfBlurDisk: blurType := rbDisk;
      pfBlurFast: blurType := rbFast;
      pfBlurBox: blurType := rbBox;
    else
      exit;
    end;

    if GetSkip then
    begin
      if AParameters.IsDefined('Radius') then
      begin
        radiusX := AParameters.Floats['Radius'];
        radiusY := radiusX;
      end else
      begin
        if AParameters.IsDefined('RadiusX') then
          radiusX := AParameters.Floats['RadiusX']
        else radiusX := AInstance.Config.DefaultBlurRadius;

        if AParameters.IsDefined('RadiusY') then
          radiusY := AParameters.Floats['RadiusY']
        else radiusY := AInstance.Config.DefaultBlurRadius;
      end;

      filteredLayer := FilterConnector.ActiveLayer.FilterBlurRadial(FilterConnector.WorkArea, radiusX,radiusY,blurType) as TBGRABitmap
    end
    else
      result := AInstance.ShowRadialBlurDlg(FilterConnector, blurType, GetCaption);
  end;

  procedure DoMetalFloor;
  var temp: TBGRABitmap;
  begin
     temp := CreateMetalFloorTexture(100);
     filteredLayer := temp.GetPart(rect(0,0,FilterConnector.ActiveLayer.Width,FilterConnector.ActiveLayer.Height)) as TBGRABitmap;
     temp.Free;
  end;

var
  layer: TBGRABitmap;
  applyOfsBefore: Boolean;

begin
  result := srException;
  if filter = pfNone then exit(srInvalidParameters);
  if not AInstance.Image.CheckNoAction then exit;
  if not AInstance.image.CheckCurrentLayerVisible then exit;
  if skipDialog then AParameters.Booleans['Validate'] := true;

  if (filter = pfLinearNegative) and AInstance.Image.SelectionMaskEmpty and (AInstance.Image.NbLayers = 1) then
  begin
      AInstance.Image.LinearNegativeAll;
      result := srOk;
      exit;
  end;

  applyOfsBefore:= false;
  if not (filter in[pfSharpen, pfSmooth, pfClearType, pfClearTypeInverse, pfNormalize, pfMedian,
            pfNegative, pfLinearNegative, pfComplementaryColor, pfGrayscale]) then
    if AInstance.Image.SelectionLayerIsEmpty then
      applyOfsBefore := true;

  try
    FilterConnector := TFilterConnector.Create(AInstance, AParameters, applyOfsBefore);
    layer := FilterConnector.ActiveLayer;

    filteredLayer := nil;
    case filter of
    pfSharpen: result := AInstance.ShowSharpenDlg(FilterConnector);
    pfSmooth: filteredLayer := layer.FilterSmooth as TBGRABitmap;
    pfClearTypeInverse: filteredLayer := ClearTypeInverseFilter(layer) as TBGRABitmap;
    pfClearType: filteredLayer := ClearTypeFilter(layer) as TBGRABitmap;
    pfSphere: filteredLayer := layer.FilterSphere as TBGRABitmap;
    pfPlane: filteredLayer := layer.FilterPlane as TBGRABitmap;
    pfCylinder: filteredLayer := layer.FilterCylinder as TBGRABitmap;
    pfNormalize: filteredLayer := layer.FilterNormalize(FilterConnector.WorkArea) as TBGRABitmap;
    pfMedian: filteredLayer := layer.FilterMedian(moLowSmooth) as TBGRABitmap;
    pfNegative:
      begin
        filteredLayer := layer.Duplicate as TBGRABitmap;
        filteredLayer.NegativeRect(FilterConnector.WorkArea);
      end;
    pfLinearNegative:
      begin
        filteredLayer := layer.Duplicate as TBGRABitmap;
        filteredLayer.LinearNegativeRect(FilterConnector.WorkArea)
      end;
    pfComplementaryColor:
      begin
        filteredLayer := layer.Duplicate as TBGRABitmap;
        FilterComplementaryColor(filteredLayer,FilterConnector.WorkArea);
      end;
    pfBlurPrecise, pfBlurRadial, pfBlurCorona, pfBlurDisk, pfBlurFast, pfBlurBox: DoSimpleBlur;
    pfBlurMotion: result := AInstance.ShowMotionBlurDlg(FilterConnector);
    pfBlurCustom: DoBlurCustom;
    pfEmboss: result := AInstance.ShowEmbossDlg(FilterConnector);
    pfRain: result := AInstance.ShowRainDlg(FilterConnector);
    pfPhong: result := AInstance.ShowPhongFilterDlg(FilterConnector);
    pfFunction: result := AInstance.ShowFunctionFilterDlg(FilterConnector);
    pfNoise: result := AInstance.ShowNoiseFilterDlg(FilterConnector);
    pfPixelate: result := AInstance.ShowPixelateDlg(FilterConnector);
    pfTwirl: result := AInstance.ShowTwirlDlg(FilterConnector);
    pfWaveDisplacement: result := AInstance.ShowWaveDisplacementDlg(FilterConnector);
    pfContour: filteredLayer := layer.FilterContour as TBGRABitmap;
    pfGrayscale: filteredLayer := layer.FilterGrayscale(FilterConnector.WorkArea) as TBGRABitmap;
    pfPerlinNoise: filteredLayer := CreatePerlinNoiseMap(layer.Width,layer.Height,layer.Width/256,layer.Height/256,1,rfBestQuality);
    pfCyclicPerlinNoise: filteredLayer := CreateCyclicPerlinNoiseMap(layer.Width,layer.Height,layer.Width/256,layer.Height/256,1,rfBestQuality);
    pfClouds:
      begin
        filteredLayer := layer.Duplicate as TBGRABitmap;
        RenderCloudsOn(filteredLayer,AInstance.ToolManager.ForeColor);
      end;
    pfCustomWater:
    begin
      filteredLayer := layer.Duplicate as TBGRABitmap;
      RenderWaterOn(filteredLayer,AInstance.ToolManager.ForeColor,AInstance.ToolManager.BackColor);
    end;
    pfWater: filteredLayer := CreateWaterTexture(layer.Width,layer.Height);
    pfWood: filteredLayer := CreateWoodTexture(layer.Width,layer.Height);
    pfWoodVertical: filteredLayer := CreateVerticalWoodTexture(layer.Width,layer.Height);
    pfPlastik: filteredLayer := CreatePlastikTexture(layer.Width,layer.Height);
    pfMetalFloor: DoMetalFloor;
    pfCamouflage: filteredLayer := CreateCamouflageTexture(layer.Width,layer.Height);
    pfSnowPrint: filteredLayer := CreateSnowPrintTexture(layer.Width,layer.Height);
    pfStone: filteredLayer := CreateStoneTexture(layer.Width,layer.Height);
    pfRoundStone: filteredLayer := CreateRoundStoneTexture(layer.Width,layer.Height);
    pfMarble: filteredLayer := CreateMarbleTexture(layer.Width,layer.Height);
    end;

    if filteredLayer <> nil then
    begin
      if FilterConnector.ActionDone then
        filteredLayer.Free
      else
      begin
        FilterConnector.PutImage(filteredLayer,IsColoredFilter[filter],True);
        FilterConnector.ValidateAction;
      end;
    end;
  except
    on ex: Exception do
      AInstance.ShowError(PictureFilterStr[filter],ex.Message);
  end;
  if FilterConnector.ActionDone then
    result := srOk;
  FilterConnector.Free;
end;

end.

