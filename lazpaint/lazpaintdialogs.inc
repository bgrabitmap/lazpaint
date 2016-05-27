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
    FCanvasSize.repeatImage := False;
    if FCanvasSize.ShowModal = mrOk then
      Image.Assign(FCanvasSize.canvasSizeResult, true, True);
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
    FCanvasSize.repeatImage := True;
    if FCanvasSize.ShowModal = mrOk then
      image.Assign(FCanvasSize.canvasSizeResult,true,True);
  except
    on ex:Exception do
      ShowError('ShowRepeatImageDlg',ex.Message);
  end;
  ShowTopmost(topmostInfo);
end;

function TLazPaintInstance.ScriptFileNew(AVars: TVariableSet): TScriptResult;
var
  bitmapRepl: TBGRABitmap;
  vW,vH: TScriptVariableReference;
  w,h: NativeInt;
  whDefined: boolean;
begin
  if ToolManager.ToolSleeping then
  begin
    result := srException;
    exit;
  end;
  vW := AVars.GetVariable('Width');
  vH := AVars.GetVariable('Height');
  whDefined := AVars.IsReferenceDefined(vW) and AVars.IsReferenceDefined(vH);
  if whDefined then
  begin
    w := AVars.GetInteger(vW);
    h := AVars.GetInteger(vH);
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
  if Image.IsFileModified then
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
    bitmapRepl := MakeNewBitmapReplacement(w,h)
  else
  begin
    if not ShowNewImageDlg(bitmapRepl) then
    begin
      result := srCancelledByUser;
      exit;
    end else
    if Assigned(ScriptContext.RecordingFunctionParameters) then
    begin
      ScriptContext.RecordingFunctionParameters.AddInteger('Width', bitmapRepl.Width);
      ScriptContext.RecordingFunctionParameters.AddInteger('Height', bitmapRepl.Height);
    end;
  end;
  ChooseTool(ptHand);
  image.Assign(bitmapRepl, True, False);
  Image.CurrentFilenameUTF8 := '';
  image.SetSavedFlag;
  result := srOk;
end;

function TLazPaintInstance.ShowNewImageDlg(out bitmap: TBGRABitmap
  ): boolean;
var tx,ty: integer;
begin
  FormsNeeded;

  Result:= unewimage.ShowNewImageDlg(self,tx,ty);
  if result then
    bitmap := MakeNewBitmapReplacement(tx,ty)
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
  end else
    if ShowResampleDialog(AParams) then
      result := srOk
    else
      result := srCancelledByUser;
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
  if ShowColorIntensityDlg(AVars) then
    result := srOk else result := srCancelledByUser;
end;

function TLazPaintInstance.ShowColorIntensityDlg(AParameters: TVariableSet
  ): boolean;
var oldSelectionNormal: boolean;
begin
  result := false;
  FormsNeeded;
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  try
    result := FColorIntensity.ShowModal(self,ciIntensity,AParameters) = mrOK;
  except
    on ex:Exception do
      ShowError('ShowColorIntensityDlg',ex.Message);
  end;
  ShowSelectionNormal := oldSelectionNormal;
end;

function TLazPaintInstance.ScriptColorLightness(AVars: TVariableSet): TScriptResult;
begin
  if Assigned(ScriptContext.RecordingFunctionParameters) then AVars := ScriptContext.RecordingFunctionParameters;
  if not Assigned(Image) or not image.CheckCurrentLayerVisible then
  begin result := srException; exit; end;
  if ShowColorLightnessDlg(AVars) then
    result := srOk else result := srCancelledByUser;
end;

function TLazPaintInstance.ShowColorLightnessDlg(AParameters: TVariableSet
  ): boolean;
var oldSelectionNormal: boolean;
begin
  result := false;
  FormsNeeded;
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  try
    result := FColorIntensity.ShowModal(self,ciLightness,AParameters) = mrOk;
  except
    on ex:Exception do
      ShowError('ShowColorLightnessDlg',ex.Message);
  end;
  ShowSelectionNormal := oldSelectionNormal;
end;

function TLazPaintInstance.ScriptColorShiftColors(AVars: TVariableSet): TScriptResult;
begin
  if Assigned(ScriptContext.RecordingFunctionParameters) then AVars := ScriptContext.RecordingFunctionParameters;
  if not Assigned(Image) or not image.CheckCurrentLayerVisible then
  begin result := srException; exit; end;
  if ShowShiftColorsDlg(AVars) then
    result := srOk else result := srCancelledByUser;
end;

function TLazPaintInstance.ShowShiftColorsDlg(AParameters: TVariableSet
  ): boolean;
var oldSelectionNormal: boolean;
begin
  result := false;
  FormsNeeded;
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  try
    result := FShiftColors.ShowModal(self,AParameters) = mrOk;
  except
    on ex:Exception do
      ShowError('ShowShiftColorsDlg',ex.Message);
  end;
  ShowSelectionNormal := oldSelectionNormal;
end;

function TLazPaintInstance.ScriptColorColorize(AVars: TVariableSet): TScriptResult;
begin
  if Assigned(ScriptContext.RecordingFunctionParameters) then AVars := ScriptContext.RecordingFunctionParameters;
  if not Assigned(Image) or not image.CheckCurrentLayerVisible then
  begin result := srException; exit; end;
  if ShowColorizeDlg(AVars) then
    result := srOk else result := srCancelledByUser;
end;

function TLazPaintInstance.ShowColorizeDlg(AParameters: TVariableSet): boolean;
var oldSelectionNormal: boolean;
begin
  result := false;
  FormsNeeded;
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  try
    result := FColorize.ShowModal(self,AParameters) = mrOk;
  except
    on ex:Exception do
      ShowError('ShowColorizeDlg',ex.Message);
  end;
  ShowSelectionNormal := oldSelectionNormal;
end;

function TLazPaintInstance.ScriptColorCurves(AVars: TVariableSet): TScriptResult;
begin
  if Assigned(ScriptContext.RecordingFunctionParameters) then AVars := ScriptContext.RecordingFunctionParameters;
  if not Assigned(Image) or not Image.CheckCurrentLayerVisible then
  begin result := srException; exit; end;
  if ShowColorCurvesDlg(AVars) then
    result := srOk else result := srCancelledByUser;
end;

function TLazPaintInstance.ShowColorCurvesDlg(AParameters: TVariableSet
  ): boolean;
var oldSelectionNormal: boolean;
begin
  result := false;
  FormsNeeded;
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  try
    result := FColorCurves.ShowModal(self,AParameters) = mrOk;
  except
    on ex:Exception do
      ShowError('ShowColorCurvesDlg',ex.Message);
  end;
  ShowSelectionNormal := oldSelectionNormal;
end;

function TLazPaintInstance.ShowRadialBlurDlg(AFilterConnector: TObject; blurType: TRadialBlurType; ACaption: string): boolean;
var oldSelectionNormal: boolean;
    top: TTopMostInfo;
begin
  top := self.HideTopmost;
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  result := uradialblur.ShowRadialBlurDlg(AFilterConnector,blurType,ACaption);
  ShowSelectionNormal := oldSelectionNormal;
  self.ShowTopmost(top);
end;

function TLazPaintInstance.ShowMotionBlurDlg(AFilterConnector: TObject):boolean;
var oldSelectionNormal: boolean;
    top: TTopMostInfo;
begin
  top := self.HideTopmost;
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  result := umotionblur.ShowMotionBlurDlg(AFilterConnector);
  ShowSelectionNormal := oldSelectionNormal;
  self.ShowTopmost(top);
end;

function TLazPaintInstance.ShowCustomBlurDlg(AFilterConnector: TObject):boolean;
var oldSelectionNormal: boolean;
    top: TTopMostInfo;
begin
  top := self.HideTopmost;
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  result := FCustomBlur.ShowDlg(AFilterConnector);
  ShowSelectionNormal := oldSelectionNormal;
  self.ShowTopmost(top);
end;

function TLazPaintInstance.ShowEmbossDlg(AFilterConnector: TObject): boolean;
var oldSelectionNormal: boolean;
    top: TTopMostInfo;
begin
  top := self.HideTopmost;
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  result := uemboss.ShowEmbossDlg(AFilterConnector);
  ShowSelectionNormal := oldSelectionNormal;
  self.ShowTopmost(top);
end;

function TLazPaintInstance.ShowRainDlg(AFilterConnector: TObject): boolean;
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

function TLazPaintInstance.ShowPixelateDlg(AFilterConnector: TObject): boolean;
var oldSelectionNormal: boolean;
    top: TTopMostInfo;
begin
  top := self.HideTopmost;
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  result := upixelate.ShowPixelateDlg(AFilterConnector);
  ShowSelectionNormal := oldSelectionNormal;
  self.ShowTopmost(top);
end;

function TLazPaintInstance.ShowNoiseFilterDlg(AFilterConnector: TObject
  ): boolean;
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

function TLazPaintInstance.ShowTwirlDlg(AFilterConnector: TObject): boolean;
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

function TLazPaintInstance.ShowPhongFilterDlg(AFilterConnector: TObject): boolean;
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

function TLazPaintInstance.ShowFunctionFilterDlg(AFilterConnector: TObject
  ): boolean;
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

function TLazPaintInstance.ShowSharpenDlg(AFilterConnector: TObject): boolean;
var oldSelectionNormal: boolean;
    top: TTopMostInfo;
begin
  top := self.HideTopmost;
  oldSelectionNormal := ShowSelectionNormal;
  ShowSelectionNormal := true;
  result := USharpen.ShowSharpenDlg(AFilterConnector,smSharpen);
  ShowSelectionNormal := oldSelectionNormal;
  self.ShowTopmost(top);
end;

function TLazPaintInstance.ScriptColorPosterize(AVars: TVariableSet): TScriptResult;
begin
  if Assigned(ScriptContext.RecordingFunctionParameters) then AVars := ScriptContext.RecordingFunctionParameters;
  if not Assigned(Image) or not Image.CheckCurrentLayerVisible then
  begin result := srException; exit; end;
  if ShowPosterizeDlg(AVars) then
    result := srOk else result := srCancelledByUser;
end;

function TLazPaintInstance.ShowPosterizeDlg(AParameters: TVariableSet): boolean;
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


