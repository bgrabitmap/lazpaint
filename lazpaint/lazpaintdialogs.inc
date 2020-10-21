// SPDX-License-Identifier: GPL-3.0-only
procedure TLazPaintInstance.ShowPrintDlg;
var f: TFPrint;
    wasVisible: boolean;
begin
  wasVisible := false;
  if (FMain <> nil) and FMain.Visible then
  begin
    wasVisible := true;
    FMain.Hide;
  end;
  f := TFPrint.Create(nil);
  f.Instance := self;
  f.ShowModal;
  f.Free;
  if (FMain <> nil) and wasVisible then FMain.Show;
end;

procedure TLazPaintInstance.ShowCanvasSizeDlg;
var topmostInfo: TTopMostInfo;
begin
  FormsNeeded;
  topmostInfo := HideTopmost;
  try
    FormCanvasSize.repeatImage := False;
    if FormCanvasSize.ShowModal = mrOk then
      Image.Assign(FormCanvasSize.canvasSizeResult, true, True);
  except
    on ex:Exception do
      ShowError('ShowCanvasSizeDlg',ex.Message);
  end;
  ShowTopmost(topmostInfo);
end;

procedure TLazPaintInstance.ShowRepeatImageDlg;
var topmostInfo: TTopMostInfo;
begin
  FormsNeeded;
  topmostInfo := HideTopmost;
  try
    FormCanvasSize.repeatImage := True;
    if FormCanvasSize.ShowModal = mrOk then
      image.Assign(FormCanvasSize.canvasSizeResult,true,True);
  except
    on ex:Exception do
      ShowError('ShowRepeatImageDlg',ex.Message);
  end;
  ShowTopmost(topmostInfo);
end;

function TLazPaintInstance.ScriptFileNew(AVars: TVariableSet): TScriptResult;
var
  bitmapRepl: TBGRABitmap;
  vW,vH,vBack: TScriptVariableReference;
  w,h: NativeInt;
  back: TBGRAPixel;
  whDefined, dlgSuccess: boolean;
  topMostInfo: TTopMostInfo;
begin
  if ToolManager.ToolSleeping then
  begin
    result := srException;
    exit;
  end;
  vW := AVars.GetVariable('Width');
  w := 1;
  vH := AVars.GetVariable('Height');
  h := 1;
  vBack := AVars.GetVariable('BackColor');
  back := BGRAPixelTransparent;
  whDefined := AVars.IsReferenceDefined(vW) and AVars.IsReferenceDefined(vH);
  if whDefined then
  begin
    w := AVars.GetInteger(vW);
    h := AVars.GetInteger(vH);
    if AVars.IsReferenceDefined(vBack) then
      back := AVars.GetPixel(vBack)
    else
      back := BGRAPixelTransparent;
    if (w < 1) or (w > MaxImageWidth) or (h < 1) or (h > MaxImageHeight) then
    begin
      result := srInvalidParameters;
      exit;
    end;
  end else
  if AVars.IsReferenceDefined(vW) or AVars.IsReferenceDefined(vH) then //partial parameters
  begin
    result := srInvalidParameters;
    exit;
  end;
  if Image.IsFileModified and not AVars.Booleans['IgnoreModified'] then
  begin
    case SaveQuestion(rsNewImage) of
    IDYES: begin
             result := ScriptContext.CallScriptFunction('FileSave');
             if result <> srOk then exit;
           end;
    IDCANCEL: begin
        result := srCancelledByUser;
        exit;
      end;
    end;
  end;
  if whDefined then
    bitmapRepl := MakeNewBitmapReplacement(w,h,back)
  else
  begin
    topMostInfo := HideTopmost;
    try
      dlgSuccess := ShowNewImageDlg(bitmapRepl);
    finally
      ShowTopmost(topMostInfo);
    end;
    if not dlgSuccess then
    begin
      result := srCancelledByUser;
      exit;
    end else
    if Assigned(ScriptContext.RecordingFunctionParameters) then
    begin
      ScriptContext.RecordingFunctionParameters.AddInteger('Width', bitmapRepl.Width);
      ScriptContext.RecordingFunctionParameters.AddInteger('Height', bitmapRepl.Height);
      ScriptContext.RecordingFunctionParameters.AddPixel('BackColor', bitmapRepl.GetPixel(0,0));
    end;
  end;
  ChooseTool(ptHand);
  image.Assign(bitmapRepl, True, False);
  Image.CurrentFilenameUTF8 := '';
  image.SetSavedFlag(0,-1,0);
  UpdateWindows;
  result := srOk;
end;

function TLazPaintInstance.ScriptGetName(AVars: TVariableSet): TScriptResult;
begin
  AVars.Strings['Result'] := FScriptName;
  result := srOk;
end;

function TLazPaintInstance.ScriptImageCanvasSize(AVars: TVariableSet): TScriptResult;
var
  w, h: Int64;
  anchor: String;
begin
  w := AVars.Integers['Width'];
  h := AVars.Integers['Height'];
  if (w = Image.Width) and (h = Image.Height) then exit(srOk);
  if (w < 1) or (h < 1) or (w > MaxImageWidth) or (h > MaxImageHeight) then exit(srInvalidParameters);
  anchor := AVars.Strings['Anchor'];
  try
    Image.Assign(ComputeNewCanvasSize(self, w,h, anchor, false,false), true, True);
  except
    on ex: exception do
      result := srException;
  end;
  result := srOk;
end;

function TLazPaintInstance.ScriptImageRepeat(AVars: TVariableSet): TScriptResult;
var
  w, h: Int64;
  anchor: String;
  flipMode: Boolean;
begin
  w := AVars.Integers['Width'];
  h := AVars.Integers['Height'];
  if (w = Image.Width) and (h = Image.Height) then exit(srOk);
  if (w < 1) or (h < 1) or (w > MaxImageWidth) or (h > MaxImageHeight) then exit(srInvalidParameters);
  anchor := AVars.Strings['Anchor'];
  flipMode := AVars.Booleans['Flip'];
  try
    Image.Assign(ComputeNewCanvasSize(self, w,h, anchor, true,flipMode), true, True);
  except
    on ex: exception do
      result := srException;
  end;
  result := srOk;
end;

function TLazPaintInstance.ShowNewImageDlg(out bitmap: TBGRABitmap
  ): boolean;
var tx,ty,bpp: integer;
  back: TBGRAPixel;
begin
  FormsNeeded;

  Result:= unewimage.ShowNewImageDlg(self,false,tx,ty,bpp,back);
  if result then
    bitmap := MakeNewBitmapReplacement(tx,ty,back)
  else
    bitmap := nil;
end;

function TLazPaintInstance.ScriptImageResample(AParams: TVariableSet): TScriptResult;
var w,h: NativeInt;
    f: TResampleFilter;
begin
  if Assigned(ScriptContext.RecordingFunctionParameters) then AParams := ScriptContext.RecordingFunctionParameters;
  if AParams.IsDefined('Width') and AParams.IsDefined('Height') and AParams.IsDefined('Quality') then
  begin
    w := AParams.Integers['Width'];
    h := AParams.Integers['Height'];
    f := StrToResampleFilter(AParams.Strings['Quality']);
    if (CompareText(AParams.Strings['Quality'],ResampleFilterStr[f])<>0) or
     (w < 1) or (w > MaxImageWidth) or (h < 1) or (h > MaxImageHeight) then
      result := srInvalidParameters
    else
    try
      Image.Resample(w,h,f);
      result := srOk;
    except
      on ex:exception do
        result := srException;
    end;
    UpdateWindows;
  end else
    if ShowResampleDialog(AParams) then
      result := srOk
    else
      result := srCancelledByUser;
end;

function TLazPaintInstance.ScriptLazPaintGetVersion(AVars: TVariableSet): TScriptResult;
var
  resList: TScriptVariableReference;
begin
  resList := AVars.AddIntegerList('Result');
  AVars.AppendInteger(resList, LazPaintVersion div 1000000);
  AVars.AppendInteger(resList, (LazPaintVersion div 10000) mod 100);
  AVars.AppendInteger(resList, (LazPaintVersion div 100) mod 100);
  result := srOk;
end;

function TLazPaintInstance.ScriptShowDirectoryDialog(AVars: TVariableSet): TScriptResult;
var
  chosenDir: string;
begin
  if SelectDirectory(AVars.Strings['Prompt'], AVars.Strings['InitialDir'], chosenDir) then
  begin
    Avars.Strings['Result'] := chosenDir;
    result := srOk;
  end else
    result := srCancelledByUser;
end;

function TLazPaintInstance.ScriptTranslateGetLanguage(AVars: TVariableSet
  ): TScriptResult;
begin
  AVars.Strings['Result'] := ActiveLanguage;
  result := srOk;
end;

function TLazPaintInstance.ScriptTranslateText(AVars: TVariableSet): TScriptResult;
var
  textVar, resVar: TScriptVariableReference;
  i: Integer;
begin
  textVar := AVars.GetVariable('Text');
  if not AVars.IsReferenceDefined(textVar) then exit(srException);
  if AVars.IsList(textVar) then
  begin
    resVar := AVars.AddStringList('Result');
    for i := 0 to AVars.GetListCount(textVar)-1 do
      AVars.AppendString(resVar, DoTranslate('', AVars.GetStringAt(textVar, i)));
  end else
    AVars.Strings['Result'] := DoTranslate('', AVars.Strings['Text']);
  result := srOk;
end;

function TLazPaintInstance.ShowResampleDialog(AParameters: TVariableSet): boolean;
begin
  FormsNeeded;
  Result:= uresample.ShowResampleDialog(self,AParameters);
end;

function TLazPaintInstance.ScriptColorIntensity(AVars: TVariableSet): TScriptResult;
begin
  if Assigned(ScriptContext.RecordingFunctionParameters) then AVars := ScriptContext.RecordingFunctionParameters;
  if not Assigned(Image) or not image.CheckCurrentLayerVisible then
  begin result := srException; exit; end;
  result := ShowColorIntensityDlg(AVars);
end;

function TLazPaintInstance.ShowColorIntensityDlg(AParameters: TVariableSet): TScriptResult;
var oldSelectionNormal: boolean;
begin
  FormsNeeded;
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  try
    case FormColorIntensity.ShowModal(self,ciIntensity,AParameters) of
    mrOK: result := srOk;
    else result := srCancelledByUser;
    end;
  except
    on ex:Exception do
    begin
      result := srException;
      ShowError(FormColorIntensity.Caption, ex.Message);
    end;
  end;
  ShowSelectionNormal := oldSelectionNormal;
end;

function TLazPaintInstance.ScriptColorLightness(AVars: TVariableSet): TScriptResult;
begin
  if Assigned(ScriptContext.RecordingFunctionParameters) then AVars := ScriptContext.RecordingFunctionParameters;
  if not Assigned(Image) or not image.CheckCurrentLayerVisible then
  begin result := srException; exit; end;
  result := ShowColorLightnessDlg(AVars);
end;

function TLazPaintInstance.ShowColorLightnessDlg(AParameters: TVariableSet
  ): TScriptResult;
var oldSelectionNormal: boolean;
begin
  FormsNeeded;
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  try
    case FormColorIntensity.ShowModal(self,ciLightness,AParameters) of
    mrOk: result := srOk;
    else result := srCancelledByUser;
    end;
  except
    on ex:Exception do
    begin
      result := srException;
      ShowError(FormColorIntensity.Caption,ex.Message);
    end;
  end;
  ShowSelectionNormal := oldSelectionNormal;
end;

function TLazPaintInstance.ScriptColorShiftColors(AVars: TVariableSet): TScriptResult;
begin
  if Assigned(ScriptContext.RecordingFunctionParameters) then AVars := ScriptContext.RecordingFunctionParameters;
  if not Assigned(Image) or not image.CheckCurrentLayerVisible then
  begin result := srException; exit; end;
  result := ShowShiftColorsDlg(AVars);
end;

function TLazPaintInstance.ShowShiftColorsDlg(AParameters: TVariableSet): TScriptResult;
var oldSelectionNormal: boolean;
begin
  FormsNeeded;
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  try
    case FormShiftColors.ShowModal(self,AParameters) of
    mrOk: result := srOk;
    else result := srCancelledByUser;
    end;
  except
    on ex:Exception do
    begin
      result := srException;
      ShowError(FormShiftColors.Caption,ex.Message);
    end;
  end;
  ShowSelectionNormal := oldSelectionNormal;
end;

function TLazPaintInstance.ScriptColorColorize(AVars: TVariableSet): TScriptResult;
begin
  if Assigned(ScriptContext.RecordingFunctionParameters) then AVars := ScriptContext.RecordingFunctionParameters;
  if not Assigned(Image) or not image.CheckCurrentLayerVisible then
  begin result := srException; exit; end;
  result := ShowColorizeDlg(AVars);
end;

function TLazPaintInstance.ShowColorizeDlg(AParameters: TVariableSet): TScriptResult;
var oldSelectionNormal: boolean;
begin
  FormsNeeded;
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  try
    case FormColorize.ShowModal(self,AParameters) of
    mrOk: result := srOk;
    else result := srCancelledByUser;
    end;
  except
    on ex:Exception do
    begin
      result := srException;
      ShowError(FormColorize.Caption, ex.Message);
    end;
  end;
  ShowSelectionNormal := oldSelectionNormal;
end;

function TLazPaintInstance.ScriptColorCurves(AVars: TVariableSet): TScriptResult;
begin
  if Assigned(ScriptContext.RecordingFunctionParameters) then AVars := ScriptContext.RecordingFunctionParameters;
  if not Assigned(Image) or not Image.CheckCurrentLayerVisible then
  begin result := srException; exit; end;
  result := ShowColorCurvesDlg(AVars);
end;

function TLazPaintInstance.ShowColorCurvesDlg(AParameters: TVariableSet): TScriptResult;
var oldSelectionNormal: boolean;
begin
  FormsNeeded;
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  try
    case FormAdjustCurves.ShowModal(self,AParameters) of
    mrOk: result := srOk;
    else result := srCancelledByUser;
    end;
  except
    on ex:Exception do
    begin
      result := srException;
      ShowError('ShowColorCurvesDlg',ex.Message);
    end;
  end;
  ShowSelectionNormal := oldSelectionNormal;
end;

function TLazPaintInstance.ShowRadialBlurDlg(AFilterConnector: TObject; blurType: TRadialBlurType; ACaption: string): TScriptResult;
var oldSelectionNormal: boolean;
    top: TTopMostInfo;
begin
  top := self.HideTopmost;
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  result := URadialBlur.ShowRadialBlurDlg(AFilterConnector, blurType, ACaption);
  ShowSelectionNormal := oldSelectionNormal;
  self.ShowTopmost(top);
end;

function TLazPaintInstance.ShowMotionBlurDlg(AFilterConnector: TObject): TScriptResult;
var oldSelectionNormal: boolean;
    top: TTopMostInfo;
begin
  top := self.HideTopmost;
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  result := UMotionBlur.ShowMotionBlurDlg(AFilterConnector);
  ShowSelectionNormal := oldSelectionNormal;
  self.ShowTopmost(top);
end;

function TLazPaintInstance.ShowCustomBlurDlg(AFilterConnector: TObject): TScriptResult;
var oldSelectionNormal: boolean;
    top: TTopMostInfo;
begin
  top := self.HideTopmost;
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  result := FormCustomBlur.ShowDlg(AFilterConnector);
  ShowSelectionNormal := oldSelectionNormal;
  self.ShowTopmost(top);
end;

function TLazPaintInstance.ShowEmbossDlg(AFilterConnector: TObject): TScriptResult;
var oldSelectionNormal: boolean;
    top: TTopMostInfo;
begin
  top := self.HideTopmost;
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  result := UEmboss.ShowEmbossDlg(AFilterConnector);
  ShowSelectionNormal := oldSelectionNormal;
  self.ShowTopmost(top);
end;

function TLazPaintInstance.ShowRainDlg(AFilterConnector: TObject): TScriptResult;
var oldSelectionNormal: boolean;
    top: TTopMostInfo;
begin
  top := self.HideTopmost;
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  result := uformrain.ShowRainDlg(AFilterConnector);
  ShowSelectionNormal := oldSelectionNormal;
  self.ShowTopmost(top);
end;

function TLazPaintInstance.ShowPixelateDlg(AFilterConnector: TObject): TScriptResult;
var oldSelectionNormal: boolean;
    top: TTopMostInfo;
begin
  top := self.HideTopmost;
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  result := UPixelate.ShowPixelateDlg(AFilterConnector);
  ShowSelectionNormal := oldSelectionNormal;
  self.ShowTopmost(top);
end;

function TLazPaintInstance.ShowNoiseFilterDlg(AFilterConnector: TObject): TScriptResult;
var oldSelectionNormal: boolean;
    top: TTopMostInfo;
begin
  top := self.HideTopmost;
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  result := unoisefilter.ShowNoiseFilterDlg(AFilterConnector);
  ShowSelectionNormal := oldSelectionNormal;
  self.ShowTopmost(top);
end;

function TLazPaintInstance.ShowTwirlDlg(AFilterConnector: TObject): TScriptResult;
var oldSelectionNormal: boolean;
    top: TTopMostInfo;
begin
  top := self.HideTopmost;
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  result := utwirl.ShowTwirlDlg(AFilterConnector);
  ShowSelectionNormal := oldSelectionNormal;
  self.ShowTopmost(top);
end;

function TLazPaintInstance.ShowWaveDisplacementDlg(AFilterConnector: TObject): TScriptResult;
var oldSelectionNormal: boolean;
    top: TTopMostInfo;
begin
  top := self.HideTopmost;
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  result := UWaveDisplacement.ShowWaveDisplacementDlg(AFilterConnector);
  ShowSelectionNormal := oldSelectionNormal;
  self.ShowTopmost(top);
end;

function TLazPaintInstance.ShowPhongFilterDlg(AFilterConnector: TObject): TScriptResult;
var oldSelectionNormal: boolean;
    top: TTopMostInfo;
begin
  top := self.HideTopmost;
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  result := UPhongFilter.ShowPhongFilterDlg(AFilterConnector);
  ShowSelectionNormal := oldSelectionNormal;
  self.ShowTopmost(top);
end;

function TLazPaintInstance.ShowFunctionFilterDlg(AFilterConnector: TObject): TScriptResult;
var oldSelectionNormal: boolean;
    top: TTopMostInfo;
begin
  top := self.HideTopmost;
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  result := UFilterFunction.ShowFilterFunctionDlg(AFilterConnector);
  ShowSelectionNormal := oldSelectionNormal;
  self.ShowTopmost(top);
end;

function TLazPaintInstance.ShowSharpenDlg(AFilterConnector: TObject): TScriptResult;
var oldSelectionNormal: boolean;
    top: TTopMostInfo;
begin
  top := self.HideTopmost;
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  result := USharpen.ShowSharpenDlg(AFilterConnector, smSharpen);
  ShowSelectionNormal := oldSelectionNormal;
  self.ShowTopmost(top);
end;

function TLazPaintInstance.ScriptColorPosterize(AVars: TVariableSet): TScriptResult;
begin
  if Assigned(ScriptContext.RecordingFunctionParameters) then AVars := ScriptContext.RecordingFunctionParameters;
  if not Assigned(Image) or not Image.CheckCurrentLayerVisible then
  begin result := srException; exit; end;
  result := ShowPosterizeDlg(AVars);
end;

function TLazPaintInstance.ShowPosterizeDlg(AParameters: TVariableSet): TScriptResult;
var oldSelectionNormal: boolean;
    top: TTopMostInfo;
begin
  top := self.HideTopmost;
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  result := uposterize.ShowPosterizeDlg(self, AParameters);
  ShowSelectionNormal := oldSelectionNormal;
  self.ShowTopmost(top);
end;


