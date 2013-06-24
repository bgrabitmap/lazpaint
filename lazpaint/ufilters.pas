unit UFilters;

{$mode objfpc}

interface

uses
  Classes, SysUtils, LazPaintType;

function ExecuteFilter(AInstance: TLazPaintCustomInstance; filter: TPictureFilter; skipDialog: boolean = false): boolean;

implementation

uses UFilterConnector, BGRABitmap, BGRABitmapTypes, UGraph, BGRAGradients, Dialogs;

function ExecuteFilter(AInstance: TLazPaintCustomInstance;filter: TPictureFilter; skipDialog: boolean = false): boolean;
var
    FilterConnector: TFilterConnector;
    filteredLayer: TBGRABitmap;

  procedure DoBlurCustom;
  var
    blurMask,blurMaskCopy: TBGRABitmap;
  begin
    AInstance.HideTopmost;
    if skipDialog and (AInstance.Config.DefaultCustomBlurMask <> '') then
    begin
      try
        blurMask := TBGRABitmap.Create(AInstance.Config.DefaultCustomBlurMask);
        blurMaskCopy := blurMask.FilterGrayscale as TBGRABitmap;
        blurMask.Fill(BGRABlack);
        blurMask.PutImage(0,0,blurMaskCopy,dmDrawWithTransparency);
        blurMaskCopy.Free;
        filteredLayer := FilterConnector.ActiveLayer.FilterCustomBlur(FilterConnector.WorkArea, blurMask) as TBGRABitmap;
        blurMask.Free;
      except
        on ex: exception do
          ShowMessage(ex.Message);
      end;
    end
    else
      AInstance.ShowCustomBlurDlg(FilterConnector);
    AInstance.ShowTopmost;
  end;

  procedure DoSimpleBlur;
  var
    blurType: TRadialBlurType;
  begin
    AInstance.HideTopmost;
    case filter of
      pfBlurPrecise: blurType := rbPrecise;
      pfBlurRadial: blurType := rbNormal;
      pfBlurCorona: blurType := rbCorona;
      pfBlurDisk: blurType := rbDisk;
      pfBlurFast: blurType := rbFast;
    end;
    if skipDialog then
      filteredLayer := FilterConnector.ActiveLayer.FilterBlurRadial(FilterConnector.WorkArea, AInstance.Config.DefaultBlurRadius,blurType) as TBGRABitmap
    else
      AInstance.ShowRadialBlurDlg(FilterConnector,blurType);
    AInstance.ShowTopmost;
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

begin
  if filter = pfNone then exit;
  if not AInstance.Image.CheckNoAction then exit;
  if not AInstance.image.CheckCurrentLayerVisible then exit;
  if (filter = pfLinearNegative) and AInstance.Image.SelectionEmpty and (AInstance.Image.NbLayers = 1) then
  begin
      AInstance.Image.LinearNegativeAll;
      result := true;
      exit;
  end;
  result := false;
  if not skipDialog then AInstance.HideTopmost;
  try
    FilterConnector := TFilterConnector.Create(AInstance);
    layer := FilterConnector.ActiveLayer;

    filteredLayer := nil;
    case filter of
    pfSharpen: if skipDialog then filteredLayer := layer.FilterSharpen(FilterConnector.WorkArea,AInstance.Config.DefaultSharpenAmount) as TBGRABitmap
               else AInstance.ShowSharpenDlg(FilterConnector);
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
    pfBlurPrecise, pfBlurRadial, pfBlurCorona, pfBlurDisk, pfBlurFast: DoSimpleBlur;
    pfBlurMotion:
        if skipDialog then
          filteredLayer := layer.FilterBlurMotion(FilterConnector.WorkArea, AInstance.Config.DefaultBlurMotionDistance,AInstance.Config.DefaultBlurMotionAngle,AInstance.Config.DefaultBlurMotionOriented) as TBGRABitmap
        else
          AInstance.ShowMotionBlurDlg(FilterConnector);
    pfBlurCustom: DoBlurCustom;
    pfEmboss:
        if skipDialog then
          filteredLayer := layer.FilterEmboss(AInstance.Config.DefaultEmbossAngle,FilterConnector.WorkArea) as TBGRABitmap
        else
          AInstance.ShowEmbossDlg(FilterConnector);
    pfPhong: AInstance.ShowPhongFilterDlg(FilterConnector);
    pfFunction: AInstance.ShowFunctionFilterDlg(FilterConnector);
    pfPixelate:
        if skipDialog then
          filteredLayer := DoPixelate(layer,AInstance.Config.DefaultPixelateSize,AInstance.config.DefaultPixelateQuality)
        else
          AInstance.ShowPixelateDlg(FilterConnector);
    pfTwirl:
        if skipDialog then
          filteredLayer := layer.FilterTwirl(FilterConnector.WorkArea, Point(layer.Width div 2,layer.Height div 2), AInstance.Config.DefaultTwirlRadius, AInstance.Config.DefaultTwirlTurn ) as TBGRABitmap
        else
          AInstance.ShowTwirlDlg(FilterConnector);
    pfContour: filteredLayer := layer.FilterContour as TBGRABitmap;
    pfGrayscale: filteredLayer := layer.FilterGrayscale(FilterConnector.WorkArea) as TBGRABitmap;
    pfPerlinNoise: filteredLayer := CreatePerlinNoiseMap(layer.Width,layer.Height,layer.Width/256,layer.Height/256,1,rfBestQuality);
    pfCyclicPerlinNoise: filteredLayer := CreateCyclicPerlinNoiseMap(layer.Width,layer.Height,layer.Width/256,layer.Height/256,1,rfBestQuality);
    pfClouds:
      begin
        filteredLayer := layer.Duplicate as TBGRABitmap;
        RenderCloudsOn(filteredLayer,AInstance.ToolManager.ToolForeColor);
      end;
    pfCustomWater:
    begin
      filteredLayer := layer.Duplicate as TBGRABitmap;
      RenderWaterOn(filteredLayer,AInstance.ToolManager.ToolForeColor,AInstance.ToolManager.ToolBackColor);
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
        FilterConnector.PutImage(filteredLayer,IsColoredFilter[filter]);
        filteredLayer.Free;
        FilterConnector.ValidateAction;
      end;
    end;
  except
    on ex: Exception do
      ShowMessage('ExecuteFilter: '+ex.Message);
  end;
  if not skipDialog then AInstance.ShowTopmost;
  result:= FilterConnector.ActionDone;
  FilterConnector.Free;
end;

end.

