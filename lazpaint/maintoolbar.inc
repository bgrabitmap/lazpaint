// SPDX-License-Identifier: GPL-3.0-only
const SwitchColorShortcut = 'R';

procedure TFMain.CreateToolbarElements;

  procedure InitVectorialFill(vf: TLCVectorialFillControl; grad: TBGRALayerGradientOriginal;
      lbl: TLabel; pnl: TPanel; mouseMove: TMouseMoveEvent; mouseClick: TNotifyEvent; AIsTarget: boolean);
  begin
    vf.PopupMenu := nil;
    vf.VerticalPadding:= DoScaleY(6, OriginalDPI);
    vf.OnChooseColor :=  @VectorialFill_ChooseColor;
    vf.OnTextureClick := @VectorialFill_TextureClick;
    vf.OnFillChange:=    @VectorialFill_Change;
    vf.AutoSize := False;
    vf.OnFillTypeChange:=    @VectorialFill_TypeChange;
    vf.OnOpacityChange:=     @VectorialFill_OpacityChange;
    vf.OnAdjustToShape:=     @VectorialFill_AdjustToShape;
    vf.OnEditGradTexPoints:= @VectorialFill_EditGradTexPoints;
    vf.OnResize:= @VectorialFill_Resize;
    with grad do
    begin
      vf.GradStartColor:= StartColor;
      vf.GradEndColor:= EndColor;
      vf.GradientType:= GradientType;
      vf.GradRepetition:= Repetition;
      vf.GradInterpolation:= ColorInterpolation;
    end;
    if Assigned(lbl) then
    begin
      lbl.OnMouseMove:= mouseMove;
      lbl.OnClick:= mouseClick;
    end;
    pnl.PopupMenu := nil;
    pnl.OnMouseMove:= mouseMove;
    vf.OnMouseMove:= mouseMove;
    vf.IsTarget:= AIsTarget;
  end;

var
  ps: TPenStyle;
begin
  if FToolbarElementsInitDone then exit;
  FToolbarElementsInitDone:= true;

  Panel_ToolbarBackground.PopupMenu := PopupToolbar;
  Perspective_Repeat.OnClick:=@Perspective_RepeatClick;
  Perspective_TwoPlanes.OnClick := @Perspective_TwoPlanesClick;
  InitVectorialFill(VectorialFill_Pen, ToolManager.ForeLastGradient, Label_Pen, Panel_PenFill,
    @VectorialFill_ShowPenFill, @vectorialFill_ClickLabel,
    LazPaintInstance.ChooseColorTarget in[ctForeColorSolid..ctForeColorEndGrad]);
  VectorialFill_Pen.SolidColor := ToolManager.ForeColor;
  InitVectorialFill(VectorialFill_Back, ToolManager.BackLastGradient, Label_Back, Panel_BackFill,
    @VectorialFill_ShowBackFill, @vectorialFill_ClickLabel,
    LazPaintInstance.ChooseColorTarget in[ctBackColorSolid..ctBackColorEndGrad]);
  VectorialFill_Back.SolidColor := ToolManager.BackColor;
  InitVectorialFill(VectorialFill_Outline, ToolManager.OutlineLastGradient, nil, Panel_OutlineFill,
    @VectorialFill_ShowOutlineFill, @vectorialFill_ClickLabel,
    LazPaintInstance.ChooseColorTarget in[ctOutlineColorSolid..ctOutlineColorEndGrad]);
  VectorialFill_Outline.SolidColor := ToolManager.OutlineColor;
  Image_SwapColors.OnMouseDown := @Image_SwapColorsMouseDown;
  Tool_DrawShapeBorder.OnClick := @Tool_DrawShapeBorderClick;
  Tool_Aliasing.OnClick := @Tool_AliasingClick;
  Tool_FillShape.OnClick := @Tool_FillShapeClick;
  Tool_CloseShape.OnClick := @Tool_CloseShapeClick;
  Tool_CapFlat.OnClick := @Tool_CapFlatClick;
  Tool_CapRound.OnClick := @Tool_CapRoundClick;
  Tool_CapSquare.OnClick := @Tool_CapSquareClick;
  ComboBox_ArrowStart.OnChange := @ComboBox_ArrowStartChange;
  ComboBox_ArrowStart.OnDrawItem := @ComboBox_ArrowStartDrawItem;
  ComboBox_ArrowStart.OnDrawSelectedItem := @ComboBox_ArrowStartDrawSelectedItem;
  ComboBox_ArrowStart.CanvasScaleMode:= csmFullResolution;
  ComboBox_ArrowEnd.OnChange := @ComboBox_ArrowEndChange;
  ComboBox_ArrowEnd.OnDrawItem := @ComboBox_ArrowEndDrawItem;
  ComboBox_ArrowEnd.OnDrawSelectedItem := @ComboBox_ArrowEndDrawSelectedItem;
  ComboBox_ArrowEnd.CanvasScaleMode:= csmFullResolution;
  Tool_JoinBevel.OnClick := @Tool_JoinBevelClick;
  Tool_JoinRound.OnClick := @Tool_JoinRoundClick;
  Tool_JoinMiter.OnClick := @Tool_JoinMiterClick;
  ComboBox_PenStyle.OnChange:=@ComboBox_PenStyleChange;
  ComboBox_PenStyle.OnDrawItem:=@ComboBox_PenStyleDrawItem;
  ComboBox_PenStyle.OnDrawSelectedItem:=@ComboBox_PenStyleDrawSelectedItem;
  ComboBox_PenStyle.CanvasScaleMode:= csmFullResolution;
  for ps := psSolid to psDashDotDot do ComboBox_PenStyle.Items.Add(inttostr(ord(ps)));
  Tool_GridMoveWithoutDeformation.OnClick := @Tool_GridMoveWithoutDeformationClick;
  Combo_SplineStyle.Items.Add('Inside');
  Combo_SplineStyle.Items.Add('Inside + ends');
  Combo_SplineStyle.Items.Add('Crossing');
  Combo_SplineStyle.Items.Add('Crossing + ends');
  Combo_SplineStyle.Items.Add('Outside');
  Combo_SplineStyle.Items.Add('Round outside');
  Combo_SplineStyle.Items.Add('Vertex to side');
  Combo_SplineStyle.Items.Add('Easy Bézier');
  Combo_SplineStyle.OnChange := @Combo_SplineStyleChange;
  Tool_CurveMovePoint.OnClick := @Tool_CurveMovePointClick;
  Tool_CurveModeAuto.OnClick := @Tool_CurveModeAutoClick;
  Tool_CurveModeAngle.OnClick := @Tool_CurveModeAngleClick;
  Tool_CurveModeCurve.OnClick := @Tool_CurveModeCurveClick;
  Tool_ProgressiveFloodfill.OnClick := @Tool_ProgressiveFloodfillClick;
  Panel_PenWidth.OnMouseMove := @Panel_PenWidthMouseMove;
  Panel_PenWidthPreview.OnMouseMove := @Panel_PenWidthMouseMove;
  PaintBox_PenPreview.OnMouseMove := @Panel_PenWidthMouseMove;
  PaintBox_PenPreview.OnMouseDown := @PaintBox_PenPreviewMouseDown;
  PaintBox_PenPreview.OnPaint := @PaintBox_PenPreviewPaint;
  PaintBox_PenPreview.Align := alClient;
  Tool_PhongShapeRectangle.OnClick := @Tool_PhongShapeRectangleClick;
  Tool_PhongShapeRoundRect.OnClick := @Tool_PhongShapeRoundRectClick;
  Tool_PhongShapeSphere.OnClick := @Tool_PhongShapeSphereClick;
  Tool_PhongShapeCone.OnClick := @Tool_PhongShapeConeClick;
  Tool_PhongShapeVerticalCone.OnClick := @Tool_PhongShapeVerticalConeClick;
  Tool_PhongShapeHorizontalCylinder.OnClick := @Tool_PhongShapeHorizontalCylinderClick;
  Tool_PhongShapeVerticalCylinder.OnClick := @Tool_PhongShapeVerticalCylinderClick;
  Tool_TextFont.OnClick := @Tool_TextFontClick;
  Tool_TextAlignLeft.OnClick := @Tool_TextAlignClick;
  Tool_TextAlignCenter.OnClick := @Tool_TextAlignClick;
  Tool_TextAlignRight.OnClick := @Tool_TextAlignClick;
  Tool_TextBidirectional.OnClick := @Tool_TextBidiModeClick;
  Tool_TextLTR.OnClick := @Tool_TextBidiModeClick;
  Tool_TextRTL.OnClick := @Tool_TextBidiModeClick;
  Tool_TextAlignTop.OnClick := @Tool_TextAlignClick;
  Tool_TextAlignMiddle.OnClick := @Tool_TextAlignClick;
  Tool_TextAlignBottom.OnClick := @Tool_TextAlignClick;
  Tool_TextBold.OnClick := @Tool_TextStyleClick;
  Tool_TextItalic.OnClick := @Tool_TextStyleClick;
  Tool_TextUnderline.OnClick := @Tool_TextStyleClick;
  Tool_TextStrikeout.OnClick := @Tool_TextStyleClick;
  Tool_TextPhong.OnClick := @Tool_TextPhongClick;
  Tool_TextShadow.OnClick := @Tool_TextShadowClick;
  Tool_TextOutline.OnClick := @Tool_TextOutlineClick;
  Panel_TextMore.AutoSize := true;
  Panel_ToolbarBackground.OnMouseMove := @Panel_ToolbarBackgroundMouseMove;
  TimerHidePenPreview.OnTimer := @TimerHidePenPreviewTimer;
  TimerHideTextMore.OnTimer := @TimerHideTextMoreTimer;
  ComboBox_BrushSelect.OnChange := @ComboBox_BrushSelectChange;
  ComboBox_BrushSelect.OnDrawItem := @ComboBox_BrushSelectDrawItem;
  ComboBox_BrushSelect.OnDrawSelectedItem:= @ComboBox_BrushSelectDrawSelectedItem;
  ComboBox_BrushSelect.CanvasScaleMode:= csmFullResolution;
  Combo_Ratio.Items.Add(rsImage);
  Combo_Ratio.OnChange := @Combo_RatioChange;

  Panel_LineCap_FullSize := Panel_LineCap.Width;

  SpinEdit_Eraser.Increment := 15;
  SpinEdit_Eraser.OnChange := @SpinEdit_EraserChange;
  Tool_EraseAlpha.OnClick := @Tool_EraseOptionClick;
  Tool_EraseSharpen.OnClick := @Tool_EraseOptionClick;
  Tool_EraseBlur.OnClick := @Tool_EraseOptionClick;
  Tool_EraseLighten.OnClick := @Tool_EraseOptionClick;
  Tool_EraseDarken.OnClick := @Tool_EraseOptionClick;

  SpinEdit_Tolerance.Value := 128;
  SpinEdit_Tolerance.Increment := 5;
  SpinEdit_Tolerance.OnChange := @SpinEdit_ToleranceChange;

  SpinEdit_PenWidth.MinValue := 1;
  SpinEdit_PenWidth.MaxValue := round(MaxPenWidth*PenWidthFactor);
  SpinEdit_PenWidth.Increment := 10;
  SpinEdit_PenWidth.OnChange := @SpinEdit_PenWidthChange;
  SpinEdit_PenWidth.OnMouseMove := @SpinEdit_PenWidthMouseMove;
  SpinEdit_PenWidth.OnExit:= @SpinEdit_PenWidthExit;
  SpinEdit_PenWidth.BarExponent := 3;

  SpinEdit_ArrowSizeX.MinValue := round(MinArrowSize*PenWidthFactor);
  SpinEdit_ArrowSizeX.MaxValue := round(MaxArrowSize*PenWidthFactor);
  SpinEdit_ArrowSizeX.Value := 20;
  SpinEdit_ArrowSizeX.Increment := 5;
  SpinEdit_ArrowSizeX.OnChange := @SpinEdit_ArrowSizeChange;

  SpinEdit_ArrowSizeY.MinValue := round(MinArrowSize*PenWidthFactor);
  SpinEdit_ArrowSizeY.MaxValue := round(MaxArrowSize*PenWidthFactor);
  SpinEdit_ArrowSizeY.Value := 20;
  SpinEdit_ArrowSizeY.Increment := 5;
  SpinEdit_ArrowSizeY.OnChange := @SpinEdit_ArrowSizeChange;

  SpinEdit_TextShadowX.AllowNegativeValues := true;
  SpinEdit_TextShadowX.MinValue := -100;
  SpinEdit_TextShadowX.MaxValue := 100;
  SpinEdit_TextShadowX.Value := 0;
  SpinEdit_TextShadowX.Increment := 1;
  SpinEdit_TextShadowX.OnChange := @SpinEdit_TextShadowXChange;

  SpinEdit_TextShadowX.AllowNegativeValues := true;
  SpinEdit_TextShadowY.MinValue := -100;
  SpinEdit_TextShadowy.MaxValue := 100;
  SpinEdit_TextShadowY.Value := 0;
  SpinEdit_TextShadowY.Increment := 1;
  SpinEdit_TextShadowY.OnChange := @SpinEdit_TextShadowYChange;

  SpinEdit_TextOutlineWidth.MinValue := 1;
  SpinEdit_TextOutlineWidth.MaxValue := 999;
  SpinEdit_TextOutlineWidth.Value := 20;
  SpinEdit_TextOutlineWidth.Increment := 5;
  SpinEdit_TextOutlineWidth.OnChange := @SpinEdit_TextOutlineWidthChange;

  SpinEdit_TextSize.MinValue := 1;
  SpinEdit_TextSize.MaxValue := 999;
  SpinEdit_TextSize.Value := 12;
  SpinEdit_TextSize.Increment := 5;
  SpinEdit_TextSize.OnChange := @SpinEdit_TextSizeChange;
  SpinEdit_TextSize.BarExponent:= 3;

  SpinEdit_TextBlur.MinValue := 1;
  SpinEdit_TextBlur.MaxValue := 999;
  SpinEdit_TextBlur.Value := 40;
  SpinEdit_TextBlur.Increment := 5;
  SpinEdit_TextBlur.BarExponent:= 3;
  SpinEdit_TextBlur.OnChange := @SpinEdit_TextBlurChange;

  SpinEdit_GridNbX.MinValue := MinDeformationGridSize-1;
  SpinEdit_GridNbX.MaxValue := 100;
  SpinEdit_GridNbX.Value := 10;
  SpinEdit_GridNbX.Increment := 1;
  SpinEdit_GridNbX.OnChange := @GridNb_SpinEditChange;
  SpinEdit_GridNbX.OnExit := @SpinEdit_GridNbExit;

  SpinEdit_GridNbY.MinValue := MinDeformationGridSize-1;
  SpinEdit_GridNbY.MaxValue := 100;
  SpinEdit_GridNbY.Value := 10;
  SpinEdit_GridNbY.Increment := 1;
  SpinEdit_GridNbY.OnChange := @GridNb_SpinEditChange;
  SpinEdit_GridNbY.OnExit := @SpinEdit_GridNbExit;

  SpinEdit_PhongBorderSize.MinValue := MinPhongBorderSize;
  SpinEdit_PhongBorderSize.MaxValue := MaxPhongBorderSize;
  SpinEdit_PhongBorderSize.Value := 10;
  SpinEdit_PhongBorderSize.Increment := 3;
  SpinEdit_PhongBorderSize.OnChange := @SpinEdit_PhongBorderSizeChange;

  SpinEdit_ShapeAltitude.MinValue := MinPhongShapeAltitude;
  SpinEdit_ShapeAltitude.MaxValue := MaxPhongShapeAltitude;
  SpinEdit_ShapeAltitude.Value := 10;
  SpinEdit_ShapeAltitude.Increment := 1;
  SpinEdit_ShapeAltitude.BarExponent:= 3;
  SpinEdit_ShapeAltitude.OnChange := @SpinEdit_ShapeAltitudeChange;

  SpinEdit_BrushSpacing.MinValue := 1;
  SpinEdit_BrushSpacing.MaxValue := MaxBrushSpacing;
  SpinEdit_BrushSpacing.Value := 1;
  SpinEdit_BrushSpacing.Increment := 1;
  SpinEdit_BrushSpacing.OnChange := @SpinEdit_BrushSpacingChange;

  Label_Coordinates.Caption := '';
  FCoordinatesCaption:= '';
  FCoordinatesCaptionCount := 0;
  Image_SwapColors.Hint := Image_SwapColors.Hint + ' (' + SwitchColorShortcut + ')';

  Tool_CurveModeAuto.Hint := Tool_CurveModeAuto.Hint + ' (A)';
  Tool_CurveModeAngle.Hint := Tool_CurveModeAngle.Hint + ' (X)';
  Tool_CurveModeCurve.Hint := Tool_CurveModeCurve.Hint + ' (S)';
  Tool_CurveMovePoint.Hint := Tool_CurveMovePoint.Hint + ' (Z)';

  Button_Donate.Caption := DoTranslate('', 'Donate...');
end;

procedure TFMain.UpdateTextSizeIncrement;
begin
  if SpinEdit_TextSize.Value < 10 then
    SpinEdit_TextSize.Increment := 1
  else if SpinEdit_TextSize.Value < 20 then
    SpinEdit_TextSize.Increment := 2
  else if SpinEdit_TextSize.Value < 30 then
    SpinEdit_TextSize.Increment := 3
  else
    SpinEdit_TextSize.Increment := 5;
end;

function TFMain.GetCurrentToolAction: TAction;
begin
  Case CurrentTool of
  ptMoveLayer: result := LayerMove;
  ptRotateLayer: result := LayerRotate;
  ptZoomLayer: result := LayerZoom;
  else result := ActionList1.ActionByName('Tool'+PaintToolTypeStr[CurrentTool]) as TAction;
  end;
end;

procedure TFMain.LabelAutosize(ALabel: TLabel; ATargetDPI: integer);
var
  delta, nextPos: integer;
  container: TWinControl;
  nextControl: TControl;
  I, prefWidth, prefHeight: Integer;
begin
  ALabel.Font.Height := -DoScaleY(12, OriginalDPI, ATargetDPI);
  prefWidth := ALabel.Width;
  prefHeight := ALabel.Width;
  ALabel.GetPreferredSize(prefWidth, prefHeight);
  ALabel.Width := prefWidth+DoScaleX(5, OriginalDPI, ATargetDPI);
  nextPos := MaxLongInt;
  container := ALabel.Parent;
  for i := 0 to container.ControlCount-1 do
  begin
    nextControl := container.Controls[I];
    if (nextControl.Left > ALabel.Left) and (nextControl.Left < nextPos) then
      nextPos := nextControl.Left;
  end;
  if nextPos < MaxLongInt then
  begin
    delta := ALabel.Left+ALabel.Width-nextPos;
    for i := 0 to container.ControlCount-1 do
    begin
      nextControl := container.Controls[I];
      if nextControl.Left > ALabel.Left then
        nextControl.Left := nextControl.Left + delta;
    end;
    container.Width := container.Width + delta;
  end;
end;

procedure TFMain.RegisterToolbarElements;
begin
  ToolManager.PenWidthControls.Add(Panel_PenWidth);
  ToolManager.AliasingControls.Add(Panel_Aliasing);
  ToolManager.ShapeControls.Add(Panel_ShapeOption);
  ToolManager.PenStyleControls.Add(Panel_PenStyle);
  ToolManager.JoinStyleControls.Add(Panel_JoinStyle);
  ToolManager.SplineStyleControls.Add(Panel_SplineStyle);
  ToolManager.CloseShapeControls.Add(Panel_CloseShape);
  ToolManager.LineCapControls.Add(Panel_LineCap);
  ToolManager.EraserControls.Add(Panel_Eraser);
  ToolManager.ToleranceControls.Add(Panel_Tolerance);
  ToolManager.DeformationControls.Add(Panel_Grid);
  ToolManager.TextControls.Add(Panel_Text);
  ToolManager.TextShadowControls.Add(Panel_TextShadow);
  ToolManager.TextControls.Add(Panel_TextOutline);
  ToolManager.PhongControls.Add(Panel_PhongShape);
  ToolManager.AltitudeControls.Add(Panel_Altitude);
  ToolManager.PerspectiveControls.Add(Panel_PerspectiveOption);
  ToolManager.FillControls.Add(Panel_PenFill);
  ToolManager.FillControls.Add(Panel_SwapColor);
  ToolManager.FillControls.Add(Panel_BackFill);
  ToolManager.FillControls.Add(Panel_ColorDiff);
  ToolManager.OutlineFillControls.Add(Panel_OutlineFill);
  ToolManager.BrushControls.Add(Panel_Brush);
  ToolManager.RatioControls.Add(Panel_Ratio);
  ToolManager.DonateControls.Add(Panel_Donate);
end;

procedure TFMain.UpdateToolOptions;
begin
  Tool_Aliasing.Down := toAliasing in ToolManager.ShapeOptions;
  Tool_DrawShapeBorder.Down := toDrawShape in ToolManager.ShapeOptions;
  Tool_FillShape.Down := toFillShape in ToolManager.ShapeOptions;
  Tool_CloseShape.Down := toCloseShape in ToolManager.ShapeOptions;
  UpdateLineCapBar;
end;

procedure TFMain.UpdateEraserToolbar;
begin
  if FInEraserOption then exit;
  FInEraserOption:= true;
  case ToolManager.EraserMode of
  emEraseAlpha: Tool_EraseAlpha.Down:= true;
  emSharpen: Tool_EraseSharpen.Down:= true;
  emSoften: Tool_EraseBlur.Down:= true;
  emLighten: Tool_EraseLighten.Down:= true;
  emDarken: Tool_EraseDarken.Down:= true;
  end;
  SpinEdit_Eraser.Value := ToolManager.EraserAlpha;
  FInEraserOption:= false;
end;

procedure TFMain.UpdatePenStyleToolbar;
begin
  ComboBox_PenStyle.ItemIndex := ord(ToolManager.PenStyle);
end;

procedure TFMain.UpdatePenWidthToolbar;
begin
  if FInPenWidthChange then exit;
  FInPenWidthChange:= true;
  SpinEdit_PenWidth.Value := round(ToolManager.PenWidth*PenWidthFactor);
  FInPenWidthChange:= false;
  Panel_PenWidthPreview.Invalidate;
end;

procedure TFMain.UpdatePhongToolbar;
begin
  case ToolManager.PhongShapeKind of
  pskRoundRectangle: Tool_PhongShapeRoundRect.Down := true;
  pskHalfSphere: Tool_PhongShapeSphere.Down := true;
  pskConeTop: Tool_PhongShapeCone.Down := true;
  pskConeSide: Tool_PhongShapeVerticalCone.Down := true;
  pskHorizCylinder: Tool_PhongShapeHorizontalCylinder.Down := true;
  pskVertCylinder: Tool_PhongShapeVerticalCylinder.Down := true;
  else Tool_PhongShapeRectangle.Down := true;
  end;
  SpinEdit_ShapeAltitude.Value := ToolManager.PhongShapeAltitude;
  SpinEdit_PhongBorderSize.Value := ToolManager.PhongShapeBorderSize;
end;

procedure TFMain.UpdateToleranceToolbar;
begin
  if FInTolerance then exit;
  SpinEdit_Tolerance.Value:= ToolManager.Tolerance;
end;

procedure TFMain.UpdateDeformationGridToolbar;
begin
  if FInGridNb then exit;
  SpinEdit_GridNbX.Value := ToolManager.DeformationGridNbX-1;
  SpinEdit_GridNbY.Value := ToolManager.DeformationGridNbY-1;
  Tool_GridMoveWithoutDeformation.Down := (ToolManager.DeformationGridMode = gmMovePointWithoutDeformation);
end;

procedure TFMain.UpdateFloodfillToolbar;
begin
  if FInFloodfillOption then exit;
  Tool_ProgressiveFloodfill.Down := ffProgressive in ToolManager.FloodFillOptions;
end;

procedure TFMain.UpdatePerspectiveToolbar;
begin
  if FInPerspective then exit;
  Perspective_TwoPlanes.Down := poTwoPlanes in ToolManager.PerspectiveOptions;
end;

procedure TFMain.UpdateJoinStyleToolbar;
begin
  case ToolManager.JoinStyle of
    pjsBevel: Tool_JoinBevel.Down := true;
    pjsMiter: Tool_JoinMiter.Down := true;
    pjsRound: Tool_JoinRound.Down := true;
  end;
end;

procedure TFMain.UpdateShapeRatio;
begin
  if FInShapeRatio then exit;
  Combo_Ratio.Text := RatioToStr(ToolManager.ShapeRatio);
end;

procedure TFMain.UpdateTextFontToolbar(AForce: boolean);
begin
  if FInTextFont and not AForce then exit;
  SpinEdit_TextSize.Value := round(ToolManager.TextFontSize);
  UpdateTextSizeIncrement;
  Tool_TextBold.Down := fsBold in ToolManager.TextFontStyle;
  Tool_TextItalic.Down := fsItalic in ToolManager.TextFontStyle;
  Tool_TextUnderline.Down := fsUnderline in ToolManager.TextFontStyle;
  Tool_TextStrikeout.Down := fsStrikeOut in ToolManager.TextFontStyle;
end;

procedure TFMain.UpdateTextAlign;
begin
  if FInTextAlign then exit;
  case ToolManager.TextAlign of
  taLeftJustify: Tool_TextAlignLeft.Down := true;
  taCenter: Tool_TextAlignCenter.Down := true;
  taRightJustify: Tool_TextAlignRight.Down := true;
  end;
  Tool_TextAlignTop.Down := ToolManager.TextVerticalAlign = tlTop;
  Tool_TextAlignMiddle.Down := ToolManager.TextVerticalAlign = tlCenter;
  Tool_TextAlignBottom.Down := ToolManager.TextVerticalAlign = tlBottom;
end;

procedure TFMain.UpdateTextOutlineToolbar;
begin
  Tool_TextOutline.Down := ToolManager.TextOutline;
  SpinEdit_TextOutlineWidth.Value := round(ToolManager.TextOutlineWidth*PenWidthFactor);
  if SpinEdit_TextOutlineWidth.Visible <> ToolManager.TextOutline then
  begin
    Label_OutlineWidth.Visible := ToolManager.TextOutline;
    SpinEdit_TextOutlineWidth.Visible := ToolManager.TextOutline;
    QueryArrange;
  end;
end;

procedure TFMain.UpdateTextPhongToolbar;
begin
  Tool_TextPhong.Down := ToolManager.TextPhong;
  if Panel_Text.Visible then
    Panel_Altitude.Visible := ToolManager.TextPhong;
end;

procedure TFMain.UpdateTextShadowToolbar;
var
  shadowOn: Boolean;
begin
  if FInTextShadow then exit;
  shadowOn := ToolManager.TextShadow;
  Tool_TextShadow.Down := shadowOn;
  SpinEdit_TextShadowX.Value := ToolManager.TextShadowOffset.X;
  SpinEdit_TextShadowY.Value := ToolManager.TextShadowOffset.Y;
  SpinEdit_TextBlur.Value := round(ToolManager.TextShadowBlurRadius*PenWidthFactor);
  Label_TextBlur.Visible := shadowOn;
  SpinEdit_TextBlur.Visible := shadowOn;
  Label_ShadowOffset.Visible := shadowOn;
  SpinEdit_TextShadowX.Visible := shadowOn;
  SpinEdit_TextShadowY.Visible := shadowOn;
  if Panel_TextShadow.Visible then QueryArrange;
end;

procedure TFMain.UpdateLineCapToolbar;
begin
  case ToolManager.LineCap of
    pecFlat: Tool_CapFlat.Down := true;
    pecRound: Tool_CapRound.Down := true;
    pecSquare: Tool_CapSquare.Down := true;
  end;
  SpinEdit_ArrowSizeX.Value := round(ToolManager.ArrowSize.X*PenWidthFactor);
  SpinEdit_ArrowSizeY.Value := round(ToolManager.ArrowSize.Y*PenWidthFactor);
  ComboBox_ArrowStart.ItemIndex := ord(ToolManager.ArrowStart);
  ComboBox_ArrowEnd.ItemIndex := ord(ToolManager.ArrowEnd);
end;

procedure TFMain.UpdateSplineStyleToolbar;
begin
  Combo_SplineStyle.ItemIndex:= ord(ToolManager.SplineStyle);
end;

procedure TFMain.InitToolbarElements;
var
  i: Integer;
  ak: TArrowKind;
begin
  UpdateToolImage;
  UpdateFillToolbar(true);
  UpdatePenWidthToolbar;
  UpdatePenStyleToolbar;
  UpdateJoinStyleToolbar;
  for ak := low(TArrowKind) to high(TArrowKind) do
  begin
    ComboBox_ArrowStart.Items.Add(ArrowKindToStr[ak]);
    ComboBox_ArrowEnd.Items.Add(ArrowKindToStr[ak]);
  end;
  UpdateLineCapToolbar;
  UpdateToolOptions;
  UpdateEraserToolbar;
  UpdateToleranceToolbar;
  UpdateFloodfillToolbar;
  UpdatePerspectiveToolbar;
  UpdateDeformationGridToolbar;
  UpdateSplineStyleToolbar;
  UpdateCurveModeToolbar;
  UpdateTextOutlineToolbar;
  UpdateTextPhongToolbar;
  UpdateTextFontToolbar(False);
  UpdateTextShadowToolbar;
  UpdatePhongToolbar;
  ComboBox_BrushSelect.Clear;
  for i := 0 to ToolManager.BrushCount-1 do
    ComboBox_BrushSelect.Items.Add(inttostr(i));
  if ComboBox_BrushSelect.Items.Count > 0 then
    ComboBox_BrushSelect.ItemIndex := 0;
end;

procedure TFMain.UpdateFillToolbar(AUpdateColorDiff: boolean);
var
  colorChange: boolean;
  prevPenColor, prevBackColor, newPenColor, newBackColor: TBGRAPixel;
begin
  if FInFillChange then exit;
  FInFillChange:= true;
  prevPenColor := VectorialFill_Pen.AverageColor;
  prevBackColor := VectorialFill_Back.AverageColor;
  VectorialFill_Pen.AssignFill(ToolManager.ForeFill);
  VectorialFill_Back.AssignFill(ToolManager.BackFill);
  VectorialFill_Outline.AssignFill(ToolManager.OutlineFill);
  newPenColor := VectorialFill_Pen.AverageColor;
  newBackColor := VectorialFill_Back.AverageColor;
  FInFillChange:= false;

  colorChange := (newPenColor <> prevPenColor) or (newBackColor <> prevBackColor);

  if colorChange or AUpdateColorDiff then
  begin
    Label_CurrentDiff.Caption := inttostr(round(BGRAWordDiff(ToolManager.ForeColor,ToolManager.BackColor)/65535*100))+'%';
    Label_CurrentDiff.Update;
  end;
end;

procedure TFMain.UpdateToolbar;
var maxGridNb: TSize;
begin
  maxGridNb := ToolManager.MaxDeformationGridSize;
  SpinEdit_GridNbX.MaxValue := maxGridNb.cx-1;
  SpinEdit_GridNbY.MaxValue := maxGridNb.cy-1;

  UpdateCurveModeToolbar;

  UpdateAllowedFillTypes;
  VectorialFill_Pen.CanAdjustToShape := ToolManager.ToolProvideCommand(tcForeAdjustToShape);
  VectorialFill_Back.CanAdjustToShape := ToolManager.ToolProvideCommand(tcBackAdjustToShape);
  VectorialFill_Outline.CanAdjustToShape := ToolManager.ToolProvideCommand(tcOutlineAdjustToShape);
  VectorialFill_Pen.CanEditGradTexPoints := ToolManager.ToolProvideCommand(tcForeEditGradTexPoints);
  VectorialFill_Back.CanEditGradTexPoints := ToolManager.ToolProvideCommand(tcBackEditGradTexPoints);
  VectorialFill_Outline.CanEditGradTexPoints := ToolManager.ToolProvideCommand(tcOutlineEditGradTexPoints);
  VectorialFill_Pen.EditingGradTexPoints := ToolManager.IsForeEditGradTexPoints;
  VectorialFill_Back.EditingGradTexPoints := ToolManager.IsBackEditGradTexPoints;
  VectorialFill_Outline.EditingGradTexPoints := ToolManager.IsOutlineEditGradTexPoints;
end;

procedure TFMain.QueryArrange;
begin
  if FShouldArrange and not (Assigned(FLayout) and FLayout.Arranging)
     and not FInHideFill then
    TimerArrange.Enabled := true;
end;

procedure TFMain.UpdateLineCapBar;
var
  newVisible: Boolean;
begin
  newVisible := ToolManager.ToolHasLineCap;
  if newVisible <> Panel_LineCap.Visible then
  begin
    Panel_LineCap.Visible := newVisible;
    QueryArrange;
  end;
end;

procedure TFMain.UpdateToolImage(AForceUpdate: boolean);
var img: integer; ToolBmp, IconBmp: TBitmap; IconBGRA, Blur, ToolBGRA: TBGRABitmap;
    a: TAction;
    il: TImageList;
begin
  if not Visible then exit;

  a := CurrentToolAction;
  if a <> nil then
  begin
    img := a.ImageIndex;
    currentToolLabel := a.Caption;
  end
  else
  begin
    img := -1;
    currentToolLabel := '';
  end;
  if AForceUpdate or (img <> previousToolImg) then
  begin
    if img <> -1 then
    begin
      IconBGRA := TBGRABitmap.Create(Image_CurrentTool.Width,Image_CurrentTool.Height);

      ToolBmp := TBitmap.Create;
      il := FLayout.Menu.ImageList;
      if il=nil then il := ImageList16;
      il.GetBitmap(img,ToolBmp);
      ToolBGRA := TBGRABitmap.Create(ToolBmp);
      FreeAndNil(ToolBmp);

      IconBGRA.PutImage((IconBGRA.Width-ToolBGRA.Width) div 2, (IconBGRA.Height-ToolBGRA.Height) div 2,ToolBGRA,dmSet);
      Blur := IconBGRA.FilterBlurRadial(3,rbNormal) as TBGRABitmap;
      IconBGRA.PutImage(0,0,Blur,dmSet);
      FreeAndNil(Blur);
      IconBGRA.PutImage((IconBGRA.Width-ToolBGRA.Width) div 2, (IconBGRA.Height-ToolBGRA.Height) div 2,ToolBGRA,dmDrawWithTransparency);
      FreeAndNil(ToolBGRA);

      IconBmp := IconBGRA.MakeBitmapCopy(Panel_Tool.Color);
      FreeAndNil(IconBGRA);
      Image_CurrentTool.Picture.Assign(IconBmp);
      IconBmp.Free;
    end else
      Image_CurrentTool.Picture.Clear;
    previousToolImg := img;
  end;
end;

function TFMain.TextSpinEditFocused: boolean;
begin
  result := SpinEdit_TextSize.Focused or SpinEdit_TextBlur.Focused or SpinEdit_TextShadowX.Focused or
           SpinEdit_TextShadowY.Focused or SpinEdit_ShapeAltitude.Focused or
           SpinEdit_TextOutlineWidth.Focused;
end;

procedure TFMain.UpdateBrush;
begin
  if FInBrush then exit;
  SpinEdit_BrushSpacing.Value:= ToolManager.BrushSpacing;
  if (ToolManager.BrushIndex >= 0) and (ToolManager.BrushIndex < ComboBox_BrushSelect.Items.Count) then
    ComboBox_BrushSelect.ItemIndex := ToolManager.BrushIndex;
end;

procedure TFMain.UpdateBrushList;
var oldInit: boolean;
  i: Integer;
begin
  oldInit:= FInitialized;
  FInitialized := false;
  try
    Panel_PenWidthPreview.Invalidate;
    ComboBox_BrushSelect.Clear;
    for i := 0 to ToolManager.BrushCount-1 do
      ComboBox_BrushSelect.Items.Add(inttostr(i));
    if (ToolManager.BrushIndex >= 0) and
      (ToolManager.BrushIndex < ComboBox_BrushSelect.Items.Count) then
      ComboBox_BrushSelect.ItemIndex := ToolManager.BrushIndex;
  except
    on ex:exception do
      LazPaintInstance.ShowError(rsLazPaint,ex.Message);
  end;
  FInitialized := oldInit;
end;

function TFMain.CatchToolKeyDown(var AKey: Word): boolean;
begin
  if Assigned(FLayout) then
    result := FLayout.CatchToolKeyDown(AKey)
    else result := false;
end;

function TFMain.CatchToolKeyUp(var AKey: Word): boolean;
begin
  if Assigned(FLayout) then
    result := FLayout.CatchToolKeyUp(AKey)
    else result := false;
end;

function TFMain.CatchToolKeyPress(var AKey: TUTF8Char): boolean;
begin
  if Assigned(FLayout) then
    result := FLayout.CatchToolKeyPress(AKey)
    else result := false;
end;

procedure TFMain.SpinEdit_ShapeAltitudeChange(Sender: TObject; AByUser: boolean);
begin
  if SpinEdit_ShapeAltitude.Value < 6 then
    SpinEdit_ShapeAltitude.Increment := 1
  else if SpinEdit_ShapeAltitude.Value < 25 then
    SpinEdit_ShapeAltitude.Increment := 3
  else
    SpinEdit_ShapeAltitude.Increment := 5;
  if AByUser and initialized then
  begin
    if ToolManager.PhongShapeAltitude = SpinEdit_ShapeAltitude.Value then exit;
    ToolManager.PhongShapeAltitude := SpinEdit_ShapeAltitude.Value;
    UpdateEditPicture;
  end;
end;

procedure TFMain.SpinEdit_TextSizeChange(Sender: TObject; AByUser: boolean);
begin
  if AByUser and initialized and not FInTextFont then
  begin
    if round(ToolManager.TextFontSize) = SpinEdit_TextSize.Value then exit;
    FInTextFont:= true;
    ToolManager.SetTextFont(ToolManager.TextFontName,
      SpinEdit_TextSize.Value, ToolManager.TextFontStyle);
    UpdateTextSizeIncrement;
    FInTextFont:= false;
  end;
end;

procedure TFMain.SpinEdit_TextBlurChange(Sender: TObject; AByUser: boolean);
begin
  if AByUser and initialized then
  begin
    if ToolManager.TextShadowBlurRadius = SpinEdit_TextBlur.Value/PenWidthFactor then exit;
    FInTextShadow := true;
    ToolManager.TextShadowBlurRadius := SpinEdit_TextBlur.Value/PenWidthFactor;
    FInTextShadow := false;
    UpdateEditPicture(True);
  end;
end;

procedure TFMain.GridNb_SpinEditChange(Sender: TObject; AByUser: boolean);
begin
  if not AByUser or not initialized then exit;
  FInGridNb := true;
  ToolManager.SetDeformationGridSize(Size(SpinEdit_GridNbX.Value+1,SpinEdit_GridNbY.Value+1));
  FInGridNb := false;
end;

procedure TFMain.TimerArrangeTimer(Sender: TObject);
  procedure SetTarget(AFillControl: TLCVectorialFillControl);
  begin
    if (AFillControl.FillType = vftGradient) then
    begin
      if AFillControl = VectorialFill_Pen then
        LazPaintInstance.ChooseColorTarget:= ctForeColorStartGrad else
      if AFillControl = VectorialFill_Back then
        LazPaintInstance.ChooseColorTarget:= ctBackColorStartGrad else
      if AFillControl = VectorialFill_Outline then
        LazPaintInstance.ChooseColorTarget:= ctOutlineColorStartGrad;
    end else
    begin
      if AFillControl = VectorialFill_Pen then
        LazPaintInstance.ChooseColorTarget:= ctForeColorSolid else
      if AFillControl = VectorialFill_Back then
        LazPaintInstance.ChooseColorTarget:= ctBackColorSolid else
      if AFillControl = VectorialFill_Outline then
        LazPaintInstance.ChooseColorTarget:= ctOutlineColorSolid;
    end;
  end;
var
  ct: TContextualToolbars;
begin
  if Assigned(FLayout) then
  begin
    UpdateAllowedFillTypes;
    FLayout.Arrange;
    ct := ToolManager.GetContextualToolbars;
    if (LazPaintInstance.ChooseColorTarget in [ctForeColorSolid, ctForeColorStartGrad,
      ctForeColorEndGrad]) and not (ctPenFill in ct) then
    begin
      if ctBackFill in ct then SetTarget(VectorialFill_Back)
      else if ctOutlineFill in ct then SetTarget(VectorialFill_Outline);
    end else
    if (LazPaintInstance.ChooseColorTarget in [ctBackColorSolid, ctBackColorStartGrad,
      ctBackColorEndGrad]) and not (ctBackFill in ct) then
    begin
      if ctOutlineFill in ct then SetTarget(VectorialFill_Outline)
      else if ctPenFill in ct then SetTarget(VectorialFill_Pen);
    end else
    if (LazPaintInstance.ChooseColorTarget in [ctOutlineColorSolid, ctOutlineColorStartGrad,
      ctOutlineColorEndGrad]) and not (ctOutlineFill in ct) then
    begin
      if ctBackFill in ct then SetTarget(VectorialFill_Back)
      else if ctPenFill in ct then SetTarget(VectorialFill_Pen);
    end;
  end;
  TimerArrange.Enabled:= false;
end;

procedure TFMain.TimerHideFillTimer(Sender: TObject);
begin
  TimerHideFill.Enabled := false;
  FInHideFill := true;
  VectorialFill_Pen.Height := min(VectorialFill_Pen.ToolIconSize + VectorialFill_Pen.VerticalPadding,
                               Panel_SwapColor.ClientHeight - VectorialFill_Pen.Top - 1);
  VectorialFill_Pen.Tag := 0;
  VectorialFill_Back.Height := min(VectorialFill_Back.ToolIconSize + VectorialFill_Back.VerticalPadding,
                               Panel_SwapColor.ClientHeight - VectorialFill_Back.Top - 1);
  VectorialFill_Back.Tag := 0;
  VectorialFill_Outline.Height := min(VectorialFill_Outline.ToolIconSize + VectorialFill_Outline.VerticalPadding,
                                  Panel_SwapColor.ClientHeight - VectorialFill_Outline.Top - 1);
  VectorialFill_Outline.Tag := 0;
  Panel_PenFill.Height := Panel_SwapColor.Height;
  Panel_BackFill.Height := Panel_SwapColor.Height;
  Panel_OutlineFill.Height := Panel_SwapColor.Height;
  FInHideFill := false
end;

function TFMain.CanClickDonate: boolean;
begin
  result := Now > FLastDonateClick + (10 / (24*60*60) );
end;

procedure TFMain.ToolButton_DonateClick(Sender: TObject);
begin
  if CanClickDonate then
  begin
    MessagePopup(rsDonate, 3000);
    FLastDonateClick := Now;
    LazPaintInstance.Donate;
  end;
end;

procedure TFMain.SpinEdit_GridNbExit(Sender: TObject);
begin
  if SpinEdit_GridNbX.Value < MinDeformationGridSize-1 then SpinEdit_GridNbX.Value := MinDeformationGridSize-1;
  if SpinEdit_GridNbY.Value < MinDeformationGridSize-1 then SpinEdit_GridNbY.Value := MinDeformationGridSize-1;
end;

procedure TFMain.ViewWorkspaceColorExecute(Sender: TObject);
begin
  ColorDialog1.Color:= Config.GetWorkspaceColor;
  if ColorDialog1.Execute then
  begin
    Config.SetWorkspaceColor(ColorDialog1.Color);
    InvalidatePicture;
  end;
end;

procedure TFMain.SpinEdit_TextOutlineWidthChange(Sender: TObject; AByUser: boolean);
begin
  if AByUser and initialized then
    ToolManager.SetTextOutline(ToolManager.TextOutline, SpinEdit_TextOutlineWidth.Value/PenWidthFactor);
end;

procedure TFMain.SpinEdit_TextShadowXChange(Sender: TObject; AByUser: boolean);
begin
  if AByUser and initialized then
  begin
    if ToolManager.TextShadowOffset.X = SpinEdit_TextShadowX.Value then exit;
    FInTextShadow := true;
    ToolManager.TextShadowOffset := Point(SpinEdit_TextShadowX.Value, ToolManager.TextShadowOffset.Y);
    FInTextShadow := false;
    UpdateEditPicture(True);
  end;
end;

procedure TFMain.SpinEdit_TextShadowYChange(Sender: TObject; AByUser: boolean);
begin
  if AByUser and initialized then
  begin
    if ToolManager.TextShadowOffset.Y = SpinEdit_TextShadowY.Value then exit;
    FInTextShadow := true;
    ToolManager.TextShadowOffset := Point(ToolManager.TextShadowOffset.X, SpinEdit_TextShadowY.Value);
    FInTextShadow := false;
    UpdateEditPicture(True);
  end;
end;

procedure TFMain.Perspective_TwoPlanesClick(Sender: TObject);
begin
  if initialized then
  begin
    FInPerspective:= true;
    if Perspective_TwoPlanes.Down then
      ToolManager.PerspectiveOptions := ToolManager.PerspectiveOptions + [poTwoPlanes]
    else
      ToolManager.PerspectiveOptions := ToolManager.PerspectiveOptions - [poTwoPlanes];
    FInPerspective:= false;
    UpdateEditPicture;
  end;
end;

procedure TFMain.Perspective_RepeatClick(Sender: TObject);
begin
  if initialized then
  begin
    FInPerspective:= true;
    if Perspective_Repeat.Down then
      ToolManager.PerspectiveOptions := ToolManager.PerspectiveOptions + [poRepeat]
    else
      ToolManager.PerspectiveOptions := ToolManager.PerspectiveOptions - [poRepeat];
    FInPerspective:= false;
    UpdateEditPicture;
  end;
end;

procedure TFMain.UpdateChooseColors;
  procedure UpdateFor(AFillControl: TLCVectorialFillControl; ATargetSolid, ATargetStart, ATargetEnd: TColorTarget);
  begin
    if (AFillControl.FillType = vftSolid) and
       (LazPaintInstance.ChooseColorTarget in [ATargetStart, ATargetEnd]) then
      LazPaintInstance.ChooseColorTarget := ATargetSolid else
    if (AFillControl.FillType = vftGradient) and
       (LazPaintInstance.ChooseColorTarget = ATargetSolid) then
      LazPaintInstance.ChooseColorTarget := ATargetStart;
  end;

begin
  UpdateFor(VectorialFill_Back, ctBackColorSolid, ctBackColorStartGrad, ctBackColorEndGrad);
  UpdateFor(VectorialFill_Pen, ctForeColorSolid, ctForeColorStartGrad, ctForeColorEndGrad);
  UpdateFor(VectorialFill_Outline, ctOutlineColorSolid, ctOutlineColorStartGrad, ctOutlineColorEndGrad);
end;

procedure TFMain.UpdateAllowedFillTypes;
  procedure UpdateFor(AFillControl: TLCVectorialFillControl; AAllowedFillTypes: TVectorialFillTypes;
      ANeedGradientProc: TProcedureOfObject);
  begin
    if AFillControl.AllowedFillTypes <> AAllowedFillTypes then
    begin
      if not (AFillControl.FillType in AAllowedFillTypes) then
      begin
        if vftGradient in AAllowedFillTypes then
        begin
          AFillControl.AllowedFillTypes:= [AFillControl.FillType] + AAllowedFillTypes;
          ANeedGradientProc();
        end else
        if vftTexture in AAllowedFillTypes then
        begin
          AFillControl.AllowedFillTypes:= [AFillControl.FillType] + AAllowedFillTypes;
          AFillControl.FillType := vftTexture;
        end else
        if vftSolid in AAllowedFillTypes then
        begin
          AFillControl.AllowedFillTypes:= [AFillControl.FillType] + AAllowedFillTypes;
          AFillControl.FillType := vftSolid;
        end;
      end;
      AFillControl.AllowedFillTypes:= AAllowedFillTypes;
      AFillControl.Width := AFillControl.PreferredSize.cx;
    end;
  end;

begin
  UpdateFor(VectorialFill_Pen, ToolManager.AllowedForeFillTypes, @ToolManager.NeedForeGradient);
  UpdateFor(VectorialFill_Back, ToolManager.AllowedBackFillTypes, @ToolManager.NeedBackGradient);
  UpdateFor(VectorialFill_Outline, ToolManager.AllowedOutlineFillTypes, @ToolManager.NeedOutlineGradient);

  Perspective_Repeat.Visible := (CurrentTool = ptLayerMapping);
end;

procedure TFMain.SwitchColors;
begin
  if ToolManager.SwapToolColors then
  begin
    HideFill;
    UpdateChooseColors;
    UpdateEditPicture;
  end;
end;

function TFMain.EditingColors: boolean;
begin
  result := Assigned(ActiveControl) and (ActiveControl.Name = 'EColor');
end;

procedure TFMain.VectorialFill_EditGradTexPoints(Sender: TObject);
begin
  if Sender = VectorialFill_Pen then ToolManager.ToolCommand(tcForeEditGradTexPoints)
  else if Sender = VectorialFill_Back then ToolManager.ToolCommand(tcBackEditGradTexPoints)
  else if Sender = VectorialFill_Outline then ToolManager.ToolCommand(tcOutlineEditGradTexPoints);
end;

procedure TFMain.VectorialFill_AdjustToShape(Sender: TObject);
begin
  if Sender = VectorialFill_Pen then ToolManager.ToolCommand(tcForeAdjustToShape)
  else if Sender = VectorialFill_Back then ToolManager.ToolCommand(tcBackAdjustToShape)
  else if Sender = VectorialFill_Outline then ToolManager.ToolCommand(tcOutlineAdjustToShape);
end;

procedure TFMain.VectorialFill_Change(Sender: TObject);
var
  tempFill, targetFill: TVectorialFill;
  vf: TLCVectorialFillControl;
begin
  if FInFillChange then exit;
  FInFillChange:= true;
  vf := Sender as TLCVectorialFillControl;
  if vf = VectorialFill_Pen then targetFill := ToolManager.ForeFill
  else if vf = VectorialFill_Back then targetFill := ToolManager.BackFill
  else if vf = VectorialFill_Outline then targetFill := ToolManager.OutlineFill
  else exit;

  if targetFill.FillType <> vf.FillType then
  begin
    tempFill := TVectorialFill.Create;
    vf.UpdateFillExceptGeometry(tempFill);
    tempFill.FitGeometry(ToolManager.SuggestGradientBox);
    targetFill.Assign(tempFill);
    tempFill.Free;
  end else
    vf.UpdateFillExceptGeometry(targetFill);

  UpdateChooseColors;
  UpdateEditPicture;
  FInFillChange:= false;
end;

procedure TFMain.vectorialFill_ClickLabel(Sender: TObject);
  procedure CheckFor(ALabel: TLabel; ASolid, AStart, AEnd: TColorTarget;
      AControl: TLCVectorialFillControl);
  begin
    if Sender = ALabel then
    begin
      if LazPaintInstance.ChooseColorTarget = ASolid then exit;
      if LazPaintInstance.ChooseColorTarget = AStart then
        LazPaintInstance.ChooseColorTarget := AEnd
      else if LazPaintInstance.ChooseColorTarget = AEnd then
        LazPaintInstance.ChooseColorTarget := AStart
      else
        begin
          case AControl.FillType of
            vftSolid: LazPaintInstance.ChooseColorTarget:= ASolid;
            vftGradient: LazPaintInstance.ChooseColorTarget:= AStart;
          end;
        end;
    end;
  end;

begin
  CheckFor(Label_Pen, ctForeColorSolid, ctForeColorStartGrad, ctForeColorEndGrad, VectorialFill_Pen);
  CheckFor(Label_Back, ctBackColorSolid, ctBackColorStartGrad, ctBackColorEndGrad, VectorialFill_Back);
end;

procedure TFMain.VectorialFill_Resize(Sender: TObject);
begin
  QueryArrange;
end;

procedure TFMain.VectorialFill_TypeChange(Sender: TObject);
var
  vf: TLCVectorialFillControl;
begin
  vf := Sender as TLCVectorialFillControl;
  DarkThemeInstance.Apply(vf, DarkTheme);
  vf.Width := vf.PreferredSize.cx;
end;

procedure TFMain.VectorialFill_OpacityChange(Sender: TObject);
begin
  HideFill(3000, true);
end;

procedure TFMain.VectorialFill_ShowBackFill(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  ShowFill(VectorialFill_Back, Panel_BackFill);
end;

procedure TFMain.VectorialFill_ShowPenFill(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  ShowFill(VectorialFill_Pen, Panel_PenFill);
end;

procedure TFMain.VectorialFill_ShowOutlineFill(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  ShowFill(VectorialFill_Outline, Panel_OutlineFill);
end;

procedure TFMain.VectorialFill_ChooseColor(ASender: TObject; AButton: TMouseButton;
  AColorIndex: integer; var AColorValue: TBGRAPixel; out AHandled: boolean);
var
  target: TColorTarget;
begin
  AHandled := false;
  if ASender = VectorialFill_Pen then
    case AColorIndex of
      -1: target := ctForeColorSolid;
      0: target := ctForeColorStartGrad;
      1: target := ctForeColorEndGrad;
      else exit;
    end
  else if ASender = VectorialFill_Back then
    case AColorIndex of
      -1: target := ctBackColorSolid;
      0: target := ctBackColorStartGrad;
      1: target := ctBackColorEndGrad;
      else exit;
    end
  else if ASender = VectorialFill_Outline then
    case AColorIndex of
      -1: target := ctOutlineColorSolid;
      0: target := ctOutlineColorStartGrad;
      1: target := ctOutlineColorEndGrad;
      else exit;
    end
  else exit;

  AHandled := true;
  if LazPaintInstance.ChooseColorVisible and (AButton = mbLeft) then
    LazPaintInstance.ChooseColorTarget := target
  else
  begin
    if ShowColorDialogFor(target) then
      AColorValue := LazPaintInstance.GetColor(target);
  end;
end;

function TFMain.ShowColorDialogFor(ATarget: TColorTarget): boolean;
var sourceColor: TBGRAPixel;
begin
  sourceColor := LazPaintInstance.GetColor(ATarget);
  ColorDialog1.Color := sourceColor.ToColor;
  if ColorDialog1.Execute then
  begin
    LazPaintInstance.SetColor(ATarget, ColorToBGRA(ColorDialog1.Color,sourceColor.alpha));
    result := true;
  end else
    result := false;
end;

procedure TFMain.Image_SwapColorsMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then SwitchColors;
end;

procedure TFMain.SpinEdit_ToleranceChange(Sender: TObject; AByUser: boolean);
begin
  if AByUser and initialized then
  begin
    if ToolManager.Tolerance = SpinEdit_Tolerance.Value then exit;
    FInTolerance:= true;
    ToolManager.Tolerance := SpinEdit_Tolerance.Value;
    FInTolerance:= false;
  end;
end;

procedure TFMain.VectorialFill_TextureClick(Sender: TObject);
var
  vfc: TLCVectorialFillControl;
  newTex: TBGRABitmap;
  target: TVectorialFill;
begin
  vfc := Sender as TLCVectorialFillControl;
  newTex := LazPaintInstance.EditTexture(vfc.Texture);
  if Assigned(newTex) then
  begin
    FInFillChange:= true;
    vfc.Texture := newTex;
    FInFillChange:= false;
    if vfc = VectorialFill_Back then target := ToolManager.BackFill
    else if vfc = VectorialFill_Pen then target := ToolManager.ForeFill
    else target := nil;
    if Assigned(target) then
      target.SetTexture(newTex, AffineMatrixIdentity, target.TextureOpacity, target.TextureRepetition);
    newTex.FreeReference;
    UpdateEditPicture;
  end;
end;

procedure TFMain.Tool_AliasingClick(Sender: TObject);
begin
  if initialized then
  begin
    if Tool_Aliasing.Down then
      ToolManager.ShapeOptions := ToolManager.ShapeOptions + [toAliasing]
    else
      ToolManager.ShapeOptions := ToolManager.ShapeOptions - [toAliasing];
  end;
end;

procedure TFMain.Tool_DrawShapeBorderClick(Sender: TObject);
var
  opt: TShapeOptions;
begin
  if initialized then
  begin
     if not Tool_DrawShapeBorder.Down and not Tool_FillShape.Down then
       Tool_FillShape.Down := true;
     opt := ToolManager.ShapeOptions;
     if Tool_DrawShapeBorder.Down then include(opt, toDrawShape) else exclude(opt, toDrawShape);
     if Tool_FillShape.Down then include(opt, toFillShape) else exclude(opt, toFillShape);
     ToolManager.ShapeOptions := opt;
     QueryArrange;
  end;
end;

procedure TFMain.Tool_FillShapeClick(Sender: TObject);
var
  opt: TShapeOptions;
begin
  if initialized then
  begin
     if not Tool_DrawShapeBorder.Down and not Tool_FillShape.Down then
        Tool_DrawShapeBorder.Down := true;
     opt := ToolManager.ShapeOptions;
     if Tool_DrawShapeBorder.Down then include(opt, toDrawShape) else exclude(opt, toDrawShape);
     if Tool_FillShape.Down then include(opt, toFillShape) else exclude(opt, toFillShape);
     ToolManager.ShapeOptions := opt;
  end;
end;

procedure TFMain.Tool_CloseShapeClick(Sender: TObject);
begin
  if initialized then
  begin
     if (toCloseShape in ToolManager.ShapeOptions) <> Tool_CloseShape.Down then
     begin
        if Tool_CloseShape.Down then
          ToolManager.ShapeOptions := ToolManager.ShapeOptions + [toCloseShape]
        else
          ToolManager.ShapeOptions := ToolManager.ShapeOptions - [toCloseShape];
     end;
  end;
end;

procedure TFMain.Tool_CapFlatClick(Sender: TObject);
begin
  if Tool_CapFlat.Down then
  begin
    ToolManager.LineCap := pecFlat;
    ComboBox_ArrowStart.Refresh;
    ComboBox_ArrowEnd.Refresh;
    UpdateEditPicture;
  end;
end;

procedure TFMain.Tool_CapRoundClick(Sender: TObject);
begin
  if Tool_CapRound.Down then
  begin
    ToolManager.LineCap := pecRound;
    ComboBox_ArrowStart.Refresh;
    ComboBox_ArrowEnd.Refresh;
    UpdateEditPicture;
  end;
end;

procedure TFMain.Tool_CapSquareClick(Sender: TObject);
begin
  if Tool_CapSquare.Down then
  begin
    ToolManager.LineCap := pecSquare;
    ComboBox_ArrowStart.Refresh;
    ComboBox_ArrowEnd.Refresh;
    UpdateEditPicture;
  end;
end;

procedure TFMain.ComboBox_ArrowStartChange(Sender: TObject);
begin
  if initialized then
  begin
    ToolManager.ArrowStart:= StrToArrowKind(ComboBox_ArrowStart.Text);
    UpdateEditPicture;
  end;
end;

procedure TFMain.ComboBox_ArrowStartDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  kind: String;
begin
  if Index = -1 then exit;
  kind := ComboBox_ArrowStart.Items[Index];
  DrawArrow(ComboBox_ArrowStart,ARect,True,kind,pecRound,State);
end;

procedure TFMain.ComboBox_ArrowStartDrawSelectedItem(Sender: TObject; const ABGRA: TBGRABitmap;
    AState: TBCButtonState; ARect: TRect);
var
  cb: TBCComboBox;
  kind: String;
begin
  cb := ComboBox_ArrowStart;
  if cb.ItemIndex = -1 then exit;
  kind := cb.Text;
  ARect.Inflate(-2,-2);
  DrawArrow(ABGRA,ARect,True,kind,pecRound,AState.FontEx.Color);
end;

procedure TFMain.ComboBox_ArrowEndChange(Sender: TObject);
begin
  if initialized then
  begin
    ToolManager.ArrowEnd:= StrToArrowKind(ComboBox_ArrowEnd.Text);
    UpdateEditPicture;
  end;
end;

procedure TFMain.BrushLoadFromFileExecute(Sender: TObject);
begin
  ShowOpenBrushDialog;
end;

procedure TFMain.BrushLoadFromFileUpdate(Sender: TObject);
begin
  BrushLoadFromFile.Enabled := ToolManager.BrushCount < 9;
end;

procedure TFMain.BrushRemoveCurrentExecute(Sender: TObject);
begin
  ToolManager.RemoveBrushAt(ToolManager.BrushIndex);
end;

procedure TFMain.BrushRemoveCurrentUpdate(Sender: TObject);
begin
  BrushRemoveCurrent.Enabled := ToolManager.BrushIndex > 0;
end;

procedure TFMain.ComboBox_PenStyleChange(Sender: TObject);
var
  cb: TBCComboBox;
  newPS: TPenStyle;
begin
  cb := ComboBox_PenStyle;
  if cb.ItemIndex = -1 then exit;
  if initialized then
  begin
    newPS := TPenStyle(cb.ItemIndex);
    if newPS <> ToolManager.PenStyle then
      ToolManager.PenStyle := newPS;
  end;
end;

procedure TFMain.ComboBox_PenStyleDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
begin
  if Index = -1 then exit;
  DrawPenStyle(ComboBox_PenStyle, ARect, TPenStyle(Index), State);
end;

procedure TFMain.ComboBox_PenStyleDrawSelectedItem(Sender: TObject;
  const ABGRA: TBGRABitmap; AState: TBCButtonState; ARect: TRect);
var
  cb: TBCComboBox;
begin
  cb := ComboBox_PenStyle;
  if cb.ItemIndex = -1 then exit;
  DrawPenStyle(ABGRA, ARect, TPenStyle(cb.ItemIndex), AState.FontEx.Color);
end;

procedure TFMain.ComboBox_ArrowEndDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  kind: String;
begin
  if Index = -1 then exit;
  kind := ComboBox_ArrowEnd.Items[Index];
  DrawArrow(ComboBox_ArrowEnd,ARect,False,kind,pecRound,State);
end;

procedure TFMain.ComboBox_ArrowEndDrawSelectedItem(Sender: TObject; const ABGRA: TBGRABitmap;
    AState: TBCButtonState; ARect: TRect);
var
  cb: TBCComboBox;
  kind: String;
begin
  cb := ComboBox_ArrowEnd;
  if cb.ItemIndex = -1 then exit;
  kind := cb.Text;
  ARect.Inflate(-2,-2);
  DrawArrow(ABGRA,ARect,False,kind,pecRound,AState.FontEx.Color);
end;

procedure TFMain.SpinEdit_ArrowSizeChange(Sender: TObject; AByUser: boolean);
begin
  if AByUser and initialized then
  begin
    if (round(ToolManager.ArrowSize.x*PenWidthFactor) = SpinEdit_ArrowSizeX.value) and
       (round(ToolManager.ArrowSize.y*PenWidthFactor) = SpinEdit_ArrowSizeY.value) then exit;
    ToolManager.ArrowSize := PointF(SpinEdit_ArrowSizeX.value/PenWidthFactor,SpinEdit_ArrowSizeY.value/PenWidthFactor);
    UpdateEditPicture;
  end;
end;

procedure TFMain.Tool_JoinBevelClick(Sender: TObject);
begin
  if Tool_JoinBevel.Down then
    ToolManager.JoinStyle := pjsBevel;
end;

procedure TFMain.Tool_JoinRoundClick(Sender: TObject);
begin
  if Tool_JoinRound.Down then
    ToolManager.JoinStyle := pjsRound;
end;

procedure TFMain.Tool_JoinMiterClick(Sender: TObject);
begin
  if Tool_JoinMiter.Down then
    ToolManager.JoinStyle := pjsMiter;
end;

procedure TFMain.SpinEdit_EraserChange(Sender: TObject; AByUser: boolean);
begin
    if AByUser and initialized and not FInEraserOption then
    begin
      if ToolManager.EraserAlpha = SpinEdit_Eraser.value then exit;
      FInEraserOption := true;
      ToolManager.EraserAlpha := SpinEdit_Eraser.Value;
      FInEraserOption := false;
    end;
end;

procedure TFMain.Tool_EraseOptionClick(Sender: TObject);
begin
  if FInEraserOption then exit;
  FInEraserOption := true;
  if Tool_EraseAlpha.Down then ToolManager.EraserMode:= emEraseAlpha else
  if Tool_EraseSharpen.Down then ToolManager.EraserMode:= emSharpen else
  if Tool_EraseBlur.Down then ToolManager.EraserMode:= emSoften else
  if Tool_EraseLighten.Down then ToolManager.EraserMode:= emLighten else
  if Tool_EraseDarken.Down then ToolManager.EraserMode:= emDarken;
  FInEraserOption := false;
end;

procedure TFMain.Tool_GridMoveWithoutDeformationClick(Sender: TObject);
begin
  if initialized then
  begin
    if Tool_GridMoveWithoutDeformation.Down then
      ToolManager.DeformationGridMode := gmMovePointWithoutDeformation
    else
      ToolManager.DeformationGridMode := gmDeform;
  end;
end;

procedure TFMain.SetCurveMode(AMode: TToolSplineMode);
begin
  if (ToolManager.CurrentTool <> nil) and
     (ToolManager.CurrentTool is TToolSpline) then
  begin
    (ToolManager.CurrentTool as TToolSpline).CurrentMode := AMode;
    UpdateCurveModeToolbar;
  end else
  if (ToolManager.CurrentTool <> nil) and
     (ToolManager.CurrentTool is TEditShapeTool) then
  begin
    (ToolManager.CurrentTool as TEditShapeTool).CurrentSplineMode := AMode;
    UpdateCurveModeToolbar;
  end;
end;

procedure TFMain.UpdateCurveModeToolbar;
var
  cm: TToolSplineMode;
  splineTool: TToolSpline;
begin
  if (ToolManager.CurrentTool <> nil) and (ToolManager.CurrentTool is TToolSpline) then
  begin
    splineTool := ToolManager.CurrentTool as TToolSpline;
    Tool_CurveMovePoint.Enabled := not splineTool.IsHandDrawing and not splineTool.IsIdle;
    cm := splineTool.CurrentMode;
  end else
  if (ToolManager.CurrentTool <> nil) and (ToolManager.CurrentTool is TEditShapeTool) then
  begin
    Tool_CurveMovePoint.Enabled := true;
    cm := TEditShapeTool(ToolManager.CurrentTool).CurrentSplineMode;
  end else exit;
  if Tool_CurveMovePoint.Down <> (cm = tsmMovePoint) then
    Tool_CurveMovePoint.Down := (cm = tsmMovePoint);
  if Tool_CurveModeAuto.Down <> (cm = tsmCurveModeAuto) then
    Tool_CurveModeAuto.Down := (cm = tsmCurveModeAuto);
  if Tool_CurveModeAngle.Down <> (cm = tsmCurveModeAngle) then
    Tool_CurveModeAngle.Down := (cm = tsmCurveModeAngle);
  if Tool_CurveModeCurve.Down <> (cm = tsmCurveModeSpline) then
    Tool_CurveModeCurve.Down := (cm = tsmCurveModeSpline);
end;

procedure TFMain.Combo_SplineStyleChange(Sender: TObject);
var v: string;
begin
  if initialized and not FInSplineStyleChange then
  begin
    FInSplineStyleChange := true;
    v := Combo_SplineStyle.Text;
    if v = 'Inside' then ToolManager.SplineStyle := ssInside else
    if v = 'Inside + ends' then ToolManager.SplineStyle := ssInsideWithEnds else
    if v = 'Crossing' then ToolManager.SplineStyle := ssCrossing else
    if v = 'Crossing + ends' then ToolManager.SplineStyle := ssCrossingWithEnds else
    if v = 'Outside' then ToolManager.SplineStyle := ssOutside else
    if v = 'Round outside' then ToolManager.SplineStyle:= ssRoundOutside else
    if v = 'Vertex to side' then ToolManager.SplineStyle:= ssVertexToSide else
    if v = 'Easy Bézier' then ToolManager.SplineStyle := ssEasyBezier;
    if ToolManager.SplineStyle<>ssEasyBezier then
    begin
      Tool_CurveMovePoint.Down := true;
      Tool_CurveModeAuto.Enabled := false;
      Tool_CurveModeAngle.Enabled := false;
      Tool_CurveModeCurve.Enabled := false;
      SetCurveMode(tsmMovePoint);
    end else
    begin
      Tool_CurveModeAuto.Enabled := true;
      Tool_CurveModeAngle.Enabled := true;
      Tool_CurveModeCurve.Enabled := true;
      UpdateCurveModeToolbar;
    end;
    UpdateEditPicture(True);
    FInSplineStyleChange := false;
  end;
end;

procedure TFMain.Tool_CurveMovePointClick(Sender: TObject);
begin
  SetCurveMode(tsmMovePoint);
end;

procedure TFMain.Tool_CurveModeAutoClick(Sender: TObject);
begin
  SetCurveMode(tsmCurveModeAuto);
end;

procedure TFMain.Tool_CurveModeAngleClick(Sender: TObject);
begin
  SetCurveMode(tsmCurveModeAngle);
end;

procedure TFMain.Tool_CurveModeCurveClick(Sender: TObject);
begin
  SetCurveMode(tsmCurveModeSpline);
end;

procedure TFMain.Tool_ProgressiveFloodfillClick(Sender: TObject);
begin
  if initialized then
  begin
    FInFloodfillOption:= true;
    if Tool_ProgressiveFloodfill.Down then
      ToolManager.FloodFillOptions := ToolManager.FloodFillOptions + [ffProgressive]
    else
      ToolManager.FloodFillOptions := ToolManager.FloodFillOptions - [ffProgressive];
    FInFloodfillOption:= true;
  end;
end;

procedure TFMain.ShowPenPreview(ShouldRepaint: boolean);
begin
  if not Panel_PenWidthPreview.Visible then
  begin
    Panel_PenWidthPreview.Left := min(Panel_PenWidth.Left, ClientWidth-Panel_PenWidthPreview.Width);
    Panel_PenWidthPreview.Top := Panel_PenWidth.Top+Panel_PenWidth.Height;
    Panel_PenWidthPreview.Visible := True;
  end else
    if ShouldRepaint then
      PaintBox_PenPreview.Repaint;
  HidePenPreview(3000, true);
end;

procedure TFMain.ShowTextMore;
begin
  if not Panel_TextMore.Visible then
  begin
    Panel_TextMore.Left := min(Panel_Text.Left, ClientWidth-Panel_TextMore.Width);
    Panel_TextMore.Top := Panel_Text.Top+Panel_Text.Height;
    Panel_TextMore.Visible := True;
  end;
  HideTextMore(3000, true);
end;

procedure TFMain.HidePenPreview(ATimeMs: Integer; AClearTime: boolean);
begin
  if AClearTime then TimerHidePenPreview.Enabled := false;
  if Panel_PenWidthPreview.Visible then
  begin
    TimerHidePenPreview.Interval := ATimeMs;
    TimerHidePenPreview.Enabled := true;
  end;
end;

procedure TFMain.HideTextMore(ATimeMs: Integer; AClearTime: boolean);
begin
  if AClearTime then TimerHideTextMore.Enabled := false;
  if Panel_TextMore.Visible then
  begin
    TimerHideTextMore.Interval := ATimeMs;
    TimerHideTextMore.Enabled := true;
  end;
end;

procedure TFMain.ShowFill(AFillControl: TLCVectorialFillControl; APanel: TPanel);
begin
  AFillControl.BringToFront;
  AFillControl.Height := AFillControl.PreferredSize.cy;
  APanel.Height := AFillControl.Top + AFillControl.Height + DoScaleY(3, OriginalDPI);
  AFillControl.Tag := 1;
  HideFill(3000, true);
end;

procedure TFMain.HideFill(ATimeMs: Integer; AClearTime: boolean);
begin
  if (VectorialFill_Pen.Tag = 1) or (VectorialFill_Back.Tag = 1) or
     (VectorialFill_Outline.Tag = 1) then
  begin
    if AClearTime then TimerHideFill.Enabled := false;
    TimerHideFill.Interval := ATimeMs;
    TimerHideFill.Enabled := true;
  end;
end;

procedure TFMain.Panel_ToolbarBackgroundMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  HidePenPreview;
  HideTextMore;
  HideFill;
end;

procedure TFMain.Panel_PenWidthMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  ShowPenPreview;
end;

procedure TFMain.SpinEdit_PenWidthMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  ShowPenPreview(False);
end;

procedure TFMain.SpinEdit_PenWidthChange(Sender: TObject; AByUser: boolean);
begin
  if AByUser and initialized and not FInPenWidthChange then
    UpdatePenWidthFromSpinEdit;
end;

procedure TFMain.UpdatePenWidthFromSpinEdit;
var
  newWidth: single;
begin
  newWidth := max(SpinEdit_PenWidth.Value/PenWidthFactor, MinPenWidth);
  if newWidth = ToolManager.PenWidth then exit;
  FInPenWidthChange:= true;
  ToolManager.PenWidth := newWidth;
  ShowPenPreview(True);
  FInPenWidthChange:= false;
end;

procedure TFMain.SpinEdit_PenWidthExit(Sender: TObject);
begin
  if SpinEdit_PenWidth.Value < MinPenWidth*PenWidthFactor then SpinEdit_PenWidth.Value := MinPenWidth*PenWidthFactor;
end;

procedure TFMain.PaintBox_PenPreviewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var t: integer;
begin
  t := FPenPlusMinus;
  if (X < t) and (Y < t) then ToolManager.StepPenSize(True) else
  if (X >= PaintBox_PenPreview.Width-t) and (Y < t) then ToolManager.StepPenSize(False);
  ShowPenPreview(False);
end;

procedure TFMain.Panel_TextMoreMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  ShowTextMore;
end;

procedure TFMain.Panel_TextMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  ShowTextMore;
end;

procedure TFMain.PopupToolbarPopup(Sender: TObject);
begin
  MenuFileToolbar.Checked := Config.DefaultFileToolbarVisible;
  MenuZoomToolbar.Checked := Panel_Zoom.Visible;
  MenuCopyPasteToolbar.Checked := Panel_CopyPaste.Visible;
  MenuCoordinatesToolbar.Checked := Panel_Coordinates.Visible;
  MenuUndoRedoToolbar.Checked := Panel_Undo.Visible;
  MenuShowPalette.Checked := Layout.PaletteVisible;
end;

procedure TFMain.PopupToolboxPopup(Sender: TObject);
begin
  MenuDockToolboxLeft.Enabled := Layout.ToolBoxDocking <> twLeft;
  MenuDockToolboxRight.Enabled := Layout.ToolBoxDocking <> twRight;
  MenuUndockToolbox.Enabled := Layout.ToolBoxDocking <> twWindow;
end;

procedure TFMain.SelectionHorizontalFlipUpdate(Sender: TObject);
begin
  SelectionHorizontalFlip.Enabled := not image.SelectionMaskEmpty;
end;

procedure TFMain.SelectionVerticalFlipUpdate(Sender: TObject);
begin
  SelectionVerticalFlip.Enabled := not image.SelectionMaskEmpty;
end;

procedure TFMain.PaintBox_PenPreviewPaint(Sender: TObject);
var bmp: TBGRABitmap;
    x,y,t,margin: integer;
    c: TBGRAPixel;
    m: single;
    bi: TLazPaintBrush;
    visualSize: single;
    scaling: Double;
begin
  scaling := PaintBox_PenPreview.GetCanvasScaleFactor;
  bmp := TBGRABitmap.Create(round(PaintBox_PenPreview.Width*scaling),
    round(PaintBox_PenPreview.Height*scaling), Panel_PenWidthPreview.Color);
  bmp.ClipRect := rect(1,1,bmp.Width-1,bmp.Height-1);
  c := DarkThemeInstance.GetColorButtonText(DarkTheme);
  x := round(bmp.Width/2);
  y := round(bmp.Height/2);
  if Assigned(Zoom) then visualSize := ToolManager.PenWidth * Zoom.Factor else visualSize := ToolManager.PenWidth;
  visualSize *= scaling;
  if (CurrentTool in[ptBrush,ptClone]) and (visualSize <= 2*bmp.width) and (visualSize <= 2*bmp.Height) then
  begin
    bi := ToolManager.BrushInfo;
    if bi <> nil then
      with bi do
      begin
        Size := visualSize;
        bi.Put(bmp, x,y,c);
      end;
  end else
    bmp.FillEllipseAntialias(x,y,visualSize/2,visualSize/2,c);
  t := round(ImageList16.Width*scaling);
  margin := DoScaleX(round(4*scaling),OriginalDPI);
  FPenPlusMinus := round((t+margin)/scaling);
  m := t/10;
  x := margin;
  y := margin;
  bmp.EllipseAntialias(x+(t-1)/2, y+(t-1)/2, t/2-m, t/2-m, BGRABlack,m,BGRAWhite);
  bmp.DrawLineAntialias(x+m*2.6, y+(t-1)/2, x+t-1-m*2.6, y+(t-1)/2, BGRABlack,m);
  x := bmp.Width-t-margin;
  y := margin;
  bmp.EllipseAntialias(x+(t-1)/2, y+(t-1)/2, t/2-m, t/2-m, BGRABlack,m,BGRAWhite);
  bmp.DrawLineAntialias(x+(t-1)/2, y+m*2.6, x+(t-1)/2, y+t-1-m*2.6, BGRABlack,m);
  bmp.DrawLineAntialias(x+m*2.6, y+(t-1)/2, x+t-1-m*2.6, y+(t-1)/2, BGRABlack,m);

  bmp.NoClip;
  bmp.HorizLine(0,0,bmp.Width-1, ApplyLightnessFast(Panel_PenWidthPreview.Color, 48000));
  bmp.VertLine(0,0,bmp.Height-1, ApplyLightnessFast(Panel_PenWidthPreview.Color, 48000));
  bmp.HorizLine(0,bmp.Height-1,bmp.Width-1, ApplyLightnessFast(Panel_PenWidthPreview.Color, 16000));
  bmp.VertLine(bmp.Width-1,0,bmp.Height-1, ApplyLightnessFast(Panel_PenWidthPreview.Color, 16000));

  bmp.Draw(PaintBox_PenPreview.Canvas,
    rect(0, 0, PaintBox_PenPreview.Width, PaintBox_PenPreview.Height));
  bmp.Free;
end;

procedure TFMain.TimerHidePenPreviewTimer(Sender: TObject);
begin
  Panel_PenWidthPreview.Visible := False;
  TimerHidePenPreview.Enabled := false;
end;

procedure TFMain.TimerHideTextMoreTimer(Sender: TObject);
begin
  Panel_TextMore.Visible := False;
  TimerHideTextMore.Enabled := false;
end;

procedure TFMain.ToolChangeDockingExecute(Sender: TObject);
begin
  if Layout.ToolBoxDocking = twLeft then
    Layout.ToolBoxDocking := twRight else
    Layout.ToolBoxDocking := twLeft;
end;

procedure TFMain.ToolHotSpotUpdate(Sender: TObject);
begin
  ToolHotSpot.Visible := Image.IsCursor;
end;

procedure TFMain.SpinEdit_PhongBorderSizeChange(Sender: TObject; AByUser: boolean);
begin
  if AByUser and initialized then
  begin
    if ToolManager.PhongShapeBorderSize = SpinEdit_PhongBorderSize.Value then exit;
    ToolManager.PhongShapeBorderSize := SpinEdit_PhongBorderSize.Value;
    UpdateEditPicture;
  end;
end;

procedure TFMain.Tool_PhongShapeRectangleClick(Sender: TObject);
begin
  if initialized and Tool_PhongShapeRectangle.Down then
  begin
    ToolManager.PhongShapeKind := pskRectangle;
    UpdateEditPicture;
  end;
end;

procedure TFMain.Tool_PhongShapeRoundRectClick(Sender: TObject);
begin
  if initialized and Tool_PhongShapeRoundRect.Down then
  begin
    ToolManager.PhongShapeKind := pskRoundRectangle;
    UpdateEditPicture;
  end;
end;

procedure TFMain.Tool_PhongShapeSphereClick(Sender: TObject);
begin
  if initialized and Tool_PhongShapeSphere.Down then
  begin
    ToolManager.PhongShapeKind := pskHalfSphere;
    UpdateEditPicture;
  end;
end;

procedure TFMain.Tool_PhongShapeConeClick(Sender: TObject);
begin
  if initialized and Tool_PhongShapeCone.Down then
  begin
    ToolManager.PhongShapeKind := pskConeTop;
    UpdateEditPicture;
  end;
end;

procedure TFMain.Tool_PhongShapeVerticalConeClick(Sender: TObject);
begin
  if initialized and Tool_PhongShapeVerticalCone.Down then
  begin
    ToolManager.PhongShapeKind := pskConeSide;
    UpdateEditPicture;
  end
end;

procedure TFMain.Tool_PhongShapeHorizontalCylinderClick(Sender: TObject);
begin
  if initialized and Tool_PhongShapeHorizontalCylinder.Down then
  begin
    ToolManager.PhongShapeKind := pskHorizCylinder;
    UpdateEditPicture;
  end
end;

procedure TFMain.Tool_PhongShapeVerticalCylinderClick(Sender: TObject);
begin
  if initialized and Tool_PhongShapeVerticalCylinder.Down then
  begin
    ToolManager.PhongShapeKind := pskVertCylinder;
    UpdateEditPicture;
  end
end;

procedure TFMain.Tool_TextFontClick(Sender: TObject);
var topmostInfo: TTopMostInfo;
begin
  FInTextFont := true;
  FontDialog1.Font.Name := ToolManager.TextFontName;
  FontDialog1.Font.Size := round(ToolManager.TextFontSize);
  FontDialog1.Font.Style := ToolManager.TextFontStyle;
  FontDialog1.Font.Color := BGRAToColor(ToolManager.ForeColor);
  topmostInfo := LazPaintInstance.HideTopmost;
  if FontDialog1.Execute then
  begin
    ToolManager.SetTextFont(FontDialog1.Font);
    ToolManager.ForeColor := ColorToBGRA(FontDialog1.Font.Color,ToolManager.ForeColor.alpha);
    UpdateTextFontToolbar(true);
    UpdateEditPicture;
  end;
  LazPaintInstance.ShowTopmost(topmostInfo);
  FInTextFont := false;
end;

procedure TFMain.Tool_TextAlignClick(Sender: TObject);
var newAlign: TAlignment;
    newVAlign: TTextLayout;
begin
  if initialized then
  begin
    newAlign := taLeftJustify;
    newVAlign:= ToolManager.TextVerticalAlign;
    if Tool_TextAlignCenter.Down then newAlign:= taCenter;
    if Tool_TextAlignRight.Down then newAlign := taRightJustify;
    if Tool_TextAlignTop.Down and (sender = Tool_TextAlignTop) then newVAlign := tlTop;
    if Tool_TextAlignMiddle.Down and (sender = Tool_TextAlignMiddle) then newVAlign := tlCenter;
    if Tool_TextAlignBottom.Down and (sender = Tool_TextAlignBottom) then newVAlign := tlBottom;
    FInTextAlign := true;
    Tool_TextAlignTop.Down := newVAlign = tlTop;
    Tool_TextAlignMiddle.Down := newVAlign = tlCenter;
    Tool_TextAlignBottom.Down := newVAlign = tlBottom;
    FInTextAlign := false;
    if (newAlign <> ToolManager.TextAlign) or (newVAlign <> ToolManager.TextVerticalAlign) then
    begin
      FInTextAlign := true;
      ToolManager.TextAlign := newAlign;
      ToolManager.TextVerticalAlign := newVAlign;
      FInTextAlign := false;
      UpdateEditPicture;
    end;
  end;
end;

procedure TFMain.Tool_TextBidiModeClick(Sender: TObject);
var newMode: TFontBidiMode;
begin
  if initialized then
  begin
    newMode := fbmAuto;
    if Tool_TextLTR.Down then newMode:= fbmLeftToRight;
    if Tool_TextRTL.Down then newMode:= fbmRightToLeft;
    if newMode <> ToolManager.TextBidiMode then
    begin
      FInTextAlign := true;
      ToolManager.TextBidiMode := newMode;
      FInTextAlign := false;
      UpdateTextAlign;
    end;
  end;
end;

procedure TFMain.Tool_TextStyleClick(Sender: TObject);
var newStyle: TFontStyles;
begin
  if initialized then
  begin
    newStyle := [];
    if Tool_TextBold.Down then include(newStyle, fsBold);
    if Tool_TextItalic.Down then include(newStyle, fsItalic);
    if Tool_TextUnderline.Down then include(newStyle, fsUnderline);
    if Tool_TextStrikeout.Down then include(newStyle, fsStrikeOut);
    if newStyle <> ToolManager.TextFontStyle then
    begin
      FInTextFont := true;
      ToolManager.TextFontStyle := newStyle;
      FInTextFont := false;
      UpdateEditPicture;
    end;
  end;
end;

procedure TFMain.Tool_TextPhongClick(Sender: TObject);
begin
  if initialized then
  begin
     if ToolManager.TextPhong <> Tool_TextPhong.Down then
     begin
       ToolManager.TextPhong:= Tool_TextPhong.Down;
       QueryArrange;
       UpdateEditPicture;
     end;
  end;
end;

procedure TFMain.Tool_TextShadowClick(Sender: TObject);
begin
  if initialized then
  begin
     if ToolManager.TextShadow <> Tool_TextShadow.Down then
     begin
       ToolManager.TextShadow:= Tool_TextShadow.Down;
       UpdateEditPicture;
     end;
  end;
end;

procedure TFMain.Tool_TextOutlineClick(Sender: TObject);
begin
  if initialized then
    ToolManager.SetTextOutline(Tool_TextOutline.Down, ToolManager.TextOutlineWidth);
end;

procedure TFMain.ComboBox_BrushSelectDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var brushIndex: integer;
    bi : TLazPaintBrush;
    bmp: TBGRABitmap;
    c2: TBGRAPixel;
    c: TBGRAPixel;
    cb: TBCComboBox;
begin
  if Index = -1 then exit;
  cb := ComboBox_BrushSelect;
  if odSelected in State then
  begin
    c2 := ColorToBGRA(cb.DropDownHighlight);
    c := ColorToBGRA(cb.DropDownFontHighlight);
  end else
  begin
    c2 := ColorToBGRA(cb.DropDownColor);
    c := ColorToBGRA(cb.DropDownFontColor);
  end;
  brushIndex := StrToInt(cb.Items[Index]);
  bi := ToolManager.BrushAt[brushIndex];
  if bi <> nil then
  with bi do
  begin
    bi.Size := ARect.Bottom-ARect.Top-2;
    bmp := TBGRABitmap.Create(ARect.Right-ARect.Left,ARect.Bottom-ARect.Top, c2);
    bi.Put(bmp, (bmp.Width-1) div 2, bi.BrushImage.Height div 2 + 1, c);
    cb.Canvas.Draw(ARect.Left,ARect.Top,bmp.Bitmap);
    bmp.Free;
  end;
end;

procedure TFMain.ComboBox_BrushSelectDrawSelectedItem(Sender: TObject; const ABGRA: TBGRABitmap;
    AState: TBCButtonState; ARect: TRect);
var
  cb: TBCComboBox;
  brushIndex: LongInt;
  bi: TLazPaintBrush;
  c: TColor;
begin
  cb := ComboBox_BrushSelect;
  if cb.ItemIndex = -1 then exit;
  brushIndex := StrToInt(cb.Text);
  bi := ToolManager.BrushAt[brushIndex];
  c:= AState.FontEx.Color;
  if bi <> nil then
  begin
    bi.Size := ARect.Height-2;
    bi.Put(ABGRA, ARect.Left+ ARect.Width div 2, ARect.Top+ bi.BrushImage.Height div 2 +1, c);
  end;
end;

procedure TFMain.ComboBox_BrushSelectChange(Sender: TObject);
begin
  if initialized then
  begin
    FInBrush := true;
    ToolManager.BrushIndex := ComboBox_BrushSelect.ItemIndex;
    FInBrush := false;
    Panel_PenWidthPreview.Invalidate;
  end;
end;

procedure TFMain.SpinEdit_BrushSpacingChange(Sender: TObject; AByUser: boolean);
begin
  if AByUser and initialized then
  begin
    if ToolManager.BrushSpacing = SpinEdit_BrushSpacing.Value then exit;
    FInBrush := true;
    ToolManager.BrushSpacing := SpinEdit_BrushSpacing.Value;
    FInBrush := false;
  end;
end;

procedure TFMain.Combo_RatioChange(Sender: TObject);
var
  newRatio: Single;
begin
  if initialized then
  begin
    if CompareText(Combo_Ratio.Text, rsImage)=0 then
      newRatio := Image.Width/Image.Height
    else
      newRatio := ComputeRatio(Combo_Ratio.Text);
    if newRatio <> ToolManager.ShapeRatio then
    begin
      FInShapeRatio := true;
      ToolManager.ShapeRatio := newRatio;
      FInShapeRatio := false;
      ToolManager.ToolUpdate;
    end;
  end;
end;

