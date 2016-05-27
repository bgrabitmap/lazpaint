procedure TFMain.CreateToolbarElements;
var gcs: TGradientColorspace;
begin
  if Assigned(SpinEdit_PenOpacity) then exit;

  Perspective_Repeat.OnClick := @Perspective_RepeatClick;
  Perspective_TwoPlanes.OnClick := @Perspective_TwoPlanesClick;
  Shape_PenColor.OnMouseDown := @Shape_PenColorMouseDown;
  Shape_BackColor.OnMouseDown := @Shape_BackColorMouseDown;
  Image_SwapColors.OnMouseDown := @Image_SwapColorsMouseDown;
  Image_CurrentTexture.OnMouseDown := @Image_CurrentTextureMouseDown;
  Tool_DrawShapeBorder.OnClick := @Tool_DrawShapeBorderClick;
  Tool_FillShape.OnClick := @Tool_FillShapeClick;
  Tool_CloseShape.OnClick := @Tool_CloseShapeClick;
  Tool_CapFlat.OnClick := @Tool_CapFlatClick;
  Tool_CapRound.OnClick := @Tool_CapRoundClick;
  Tool_CapSquare.OnClick := @Tool_CapSquareClick;
  ComboBox_ArrowStart.OnChange := @ComboBox_ArrowStartChange;
  ComboBox_ArrowStart.OnDrawItem := @ComboBox_ArrowStartDrawItem;
  ComboBox_ArrowEnd.OnChange := @ComboBox_ArrowEndChange;
  ComboBox_ArrowEnd.OnDrawItem := @ComboBox_ArrowEndDrawItem;
  Tool_JoinBevel.OnClick := @Tool_JoinBevelClick;
  Tool_JoinRound.OnClick := @Tool_JoinRoundClick;
  Tool_JoinMiter.OnClick := @Tool_JoinMiterClick;
  Tool_PenSolid.OnClick := @Tool_PenSolidClick;
  Tool_PenDash.OnClick := @Tool_PenDashClick;
  Tool_PenDashDot.OnClick := @Tool_PenDashDotClick;
  Tool_PenDashDotDot.OnClick := @Tool_PenDashDotDotClick;
  Tool_PenDot.OnClick := @Tool_PenDotClick;
  Tool_EraseAlpha.OnClick := @Tool_EraseOptionClick;
  Tool_EraseBlur.OnClick := @Tool_EraseOptionClick;
  Tool_GridMoveWithoutDeformation.OnClick := @Tool_GridMoveWithoutDeformationClick;
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
  Tool_LinearGradient.OnClick := @Tool_LinearGradientClick;
  Tool_ReflectedGradient.OnClick := @Tool_ReflectedGradientClick;
  Tool_DiamondGradient.OnClick := @Tool_DiamondGradientClick;
  Tool_RadialGradient.OnClick := @Tool_RadialGradientClick;
  Tool_SinGradient.OnClick := @Tool_SinGradientClick;
  for gcs := low(TGradientColorspace) to high(TGradientColorspace) do
    Combo_GradientColorspace.Items.Add(GradientColorSpaceToDisplay(gcs));
  Combo_GradientColorspace.OnChange := @Combo_GradientColorspaceChange;
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
  Tool_TextPhong.OnClick := @Tool_TextPhongClick;
  Tool_TextShadow.OnClick := @Tool_TextShadowClick;
  Tool_TextOutline.OnClick := @Tool_TextOutlineClick;
  Panel_ToolbarBackground.OnMouseMove := @Panel_ToolbarBackgroundMouseMove;
  TimerHidePenPreview.OnTimer := @TimerHidePenPreviewTimer;
  ComboBox_BrushSelect.OnChange := @ComboBox_BrushSelectChange;
  ComboBox_BrushSelect.OnDrawItem := @ComboBox_BrushSelectDrawItem;

  Panel_LineCap_FullSize := Panel_LineCap.Width;
  SpinEdit_PenOpacity := TBarUpDown.Create(vsPenOpacity,0,255,255);
  SpinEdit_PenOpacity.Increment := 15;
  SpinEdit_PenOpacity.OnChange := @SpinEdit_PenOpacityChange;

  SpinEdit_BackOpacity := TBarUpDown.Create(vsBackOpacity,0,255,255);
  SpinEdit_BackOpacity.Increment := 15;
  SpinEdit_BackOpacity.OnChange := @SpinEdit_BackOpacityChange;

  SpinEdit_TextureOpacity := TBarUpDown.Create(vsTextureOpacity,0,255,255);
  SpinEdit_TextureOpacity.Increment := 15;
  SpinEdit_TextureOpacity.OnChange := @SpinEdit_TextureOpacityChange;

  SpinEdit_Eraser := TBarUpDown.Create(vsEraserOpacity,0,255,255);
  SpinEdit_Eraser.Increment := 15;
  SpinEdit_Eraser.OnChange := @SpinEdit_EraserChange;

  SpinEdit_Tolerance := TBarUpDown.Create(vsTolerance,0,255,128);
  SpinEdit_Tolerance.Increment := 5;
  SpinEdit_Tolerance.OnChange := @SpinEdit_ToleranceChange;

  SpinEdit_PenWidth := TBarUpDown.Create(vsPenWidth,1,9999,10);
  SpinEdit_PenWidth.OnChange := @SpinEdit_PenWidthChange;
  SpinEdit_PenWidth.OnMouseMove := @SpinEdit_PenWidthMouseMove;
  SpinEdit_PenWidth.OnExit:= @SpinEdit_PenWidthExit;
  SpinEdit_PenWidth.BarExponent := 3;

  ComboBox_ArrowStart.ItemIndex := 0;
  ComboBox_ArrowEnd.ItemIndex := 0;

  SpinEdit_ArrowSizeX := TBarUpDown.Create(vsArrowSizeX,10,99,20);
  SpinEdit_ArrowSizeX.Increment := 5;
  SpinEdit_ArrowSizeX.OnChange := @SpinEdit_ArrowSizeChange;

  SpinEdit_ArrowSizeY := TBarUpDown.Create(vsArrowSizeY,10,99,20);
  SpinEdit_ArrowSizeY.Increment := 5;
  SpinEdit_ArrowSizeY.OnChange := @SpinEdit_ArrowSizeChange;

  SpinEdit_TextShadowX := TBarUpDown.Create(vsTextShadowX,-100,100,0);
  SpinEdit_TextShadowX.Increment := 1;
  SpinEdit_TextShadowX.OnChange := @SpinEdit_TextShadowXChange;

  SpinEdit_TextShadowY := TBarUpDown.Create(vsTextShadowY,-100,100,0);
  SpinEdit_TextShadowY.Increment := 1;
  SpinEdit_TextShadowY.OnChange := @SpinEdit_TextShadowYChange;

  SpinEdit_TextOutlineWidth := TBarUpDown.Create(vsTextOutlineWidth,1,100,3);
  SpinEdit_TextOutlineWidth.Increment := 1;
  SpinEdit_TextOutlineWidth.OnChange := @SpinEdit_TextOutlineWidthChange;

  SpinEdit_TextSize := TBarUpDown.Create(vsTextSize,1,999,12);
  SpinEdit_TextSize.Increment := 5;
  SpinEdit_TextSize.OnChange := @SpinEdit_TextSizeChange;
  SpinEdit_TextSize.BarExponent:= 3;

  SpinEdit_TextBlur := TBarUpDown.Create(vsTextBlur,1,100,3);
  SpinEdit_TextBlur.Increment := 1;
  SpinEdit_TextBlur.OnChange := @SpinEdit_TextBlurChange;

  SpinEdit_GridNbX := TBarUpDown.Create(vsGridNbX,1,100,10);
  SpinEdit_GridNbX.Increment := 1;
  SpinEdit_GridNbX.OnChange := @GridNb_SpinEditChange;
  SpinEdit_GridNbX.OnExit := @SpinEdit_GridNbExit;

  SpinEdit_GridNbY := TBarUpDown.Create(vsGridNbY,1,100,10);
  SpinEdit_GridNbY.Increment := 1;
  SpinEdit_GridNbY.OnChange := @GridNb_SpinEditChange;
  SpinEdit_GridNbY.OnExit := @SpinEdit_GridNbExit;

  SpinEdit_PhongBorderSize := TBarUpDown.Create(vsPhongBorderSize,1,100,10);
  SpinEdit_PhongBorderSize.Increment := 3;
  SpinEdit_PhongBorderSize.OnChange := @SpinEdit_PhongBorderSizeChange;

  SpinEdit_ShapeAltitude := TBarUpDown.Create(vsShapeAltitude,1,100,10);
  SpinEdit_ShapeAltitude.Increment := 1;
  SpinEdit_ShapeAltitude.OnChange := @SpinEdit_ShapeAltitudeChange;

  SpinEdit_BrushSpacing := TBarUpDown.Create(vsBrushSpacing,1,99,1);
  SpinEdit_BrushSpacing.Increment := 1;
  SpinEdit_BrushSpacing.OnChange := @SpinEdit_BrushSpacingChange;

  Label_Coordinates.Caption := '';
  FCoordinatesCaption:= '';
  FCoordinatesCaptionCount := 0;
  LabelAutosize(Label_Pen);
  LabelAutosize(Label_Back);
  LabelAutosize(Label_PenWidth);
  LabelAutosize(Label_Eraser);
  LabelAutosize(Label_Tolerance);
  LabelAutosize(Label_Grid);
  LabelAutosize(Label_Curve);
  LabelAutosize(Label_Text);
  LabelAutosize(Label_TextBlur);
  LabelAutosize(Label_ShadowOffset);
  LabelAutosize(Label_Shape);
  LabelAutosize(Label_PhongBorder);
  LabelAutosize(Label_Altitude);
  LabelAutosize(Label_OutlineWidth);
  LabelAutosize(Label_Brush);
  LabelAutosize(Label_Spacing);
  Image_SwapColors.Hint := Image_SwapColors.Hint + ' (X)';
  NoTextureIcon;

  Tool_CurveModeAuto.Hint := Tool_CurveModeAuto.Hint + ' (A)';
  Tool_CurveModeAngle.Hint := Tool_CurveModeAngle.Hint + ' (X)';
  Tool_CurveModeCurve.Hint := Tool_CurveModeCurve.Hint + ' (S)';
  Tool_CurveMovePoint.Hint := Tool_CurveMovePoint.Hint + ' (Z)';
end;

function TFMain.GetCurrentToolAction: TAction;
begin
  Case CurrentTool of
  ptHand: result := ToolHand;
  ptPen: result := ToolPen;
  ptBrush: result := ToolBrush;
  ptClone: result := ToolClone;
  ptColorPicker: result := ToolColorPicker;
  ptEraser: result := ToolEraser;
  ptRect: result := ToolRect;
  ptEllipse: result := ToolEllipse;
  ptPolygon: result := ToolPolygon;
  ptSpline: result := ToolSpline;
  ptFloodFill: result := ToolFloodfill;
  ptGradient: result := ToolGradient;
  ptSelectRect: result := ToolSelectRect;
  ptSelectEllipse: result := ToolSelectEllipse;
  ptSelectPoly: result := ToolSelectPoly;
  ptSelectSpline: result := ToolSelectSpline;
  ptSelectPen: result := ToolSelectPen;
  ptMoveSelection: result := ToolMoveSelection;
  ptRotateSelection: result := ToolRotateSelection;
  ptMagicWand: result := ToolMagicWand;
  ptText: result := ToolText;
  ptDeformation: result := ToolDeformation;
  ptTextureMapping: result := ToolTextureMapping;
  ptLayerMapping: result := ToolLayerMapping;
  ptPhong: result := ToolPhong;
  ptMoveLayer: result := LayerMove;
  else result := nil;
  end;
end;

procedure TFMain.LabelAutosize(ALabel: TLabel);
var
  NewSize,Delta: integer;
  Container: TWinControl;
  ANext: TControl;
  I: Integer;
begin
  NewSize := ALabel.Canvas.TextWidth(ALabel.Caption+' ');
  if ALabel.Width <> NewSize then
  begin
    Delta := NewSize-ALabel.Width;
    Container := ALabel.Parent;
    Container.Width := Container.Width+Delta;
    For I := 0 to Container.ControlCount-1 do
    begin
      ANext := Container.Controls[I];
      if ANext.Left > ALabel.Left then
        ANext.Left := ANext.Left+Delta;
    end;
    ALabel.Width := NewSize;
  end;
end;

procedure TFMain.DestroyMenuAndToolbar;
begin
  if not Assigned(SpinEdit_PenOpacity) then exit;

  FreeAndNil(SpinEdit_PenOpacity);
  FreeAndNil(SpinEdit_BackOpacity);
  FreeAndNil(SpinEdit_TextureOpacity);
  FreeAndNil(SpinEdit_Eraser);
  FreeAndNil(SpinEdit_Tolerance);
  FreeAndNil(SpinEdit_PenWidth);

  FreeAndNil(SpinEdit_ArrowSizeX);
  FreeAndNil(SpinEdit_ArrowSizeY);

  FreeAndNil(SpinEdit_TextShadowX);
  FreeAndNil(SpinEdit_TextOutlineWidth);
  FreeAndNil(SpinEdit_TextShadowY);
  FreeAndNil(SpinEdit_TextSize);
  FreeAndNil(SpinEdit_TextBlur);

  FreeAndNil(SpinEdit_GridNbX);
  FreeAndNil(SpinEdit_GridNbY);

  FreeAndNil(SpinEdit_PhongBorderSize);
  FreeAndNil(SpinEdit_ShapeAltitude);

  FreeAndNil(SpinEdit_BrushSpacing);
end;

procedure TFMain.RegisterToolbarElements;
begin
  ToolManager.PenWidthControls.Add(Panel_PenWidth);
  ToolManager.ShapeControls.Add(Panel_ShapeOption);
  ToolManager.ShapeControls.Add(Panel_PenStyle);
  ToolManager.JoinStyleControls.Add(Panel_JoinStyle);
  ToolManager.SplineStyleControls.Add(Panel_SplineStyle);
  ToolManager.LineCapControls.Add(Panel_LineCap);
  ToolManager.EraserControls.Add(Panel_Eraser);
  ToolManager.ToleranceControls.Add(Panel_Tolerance);
  ToolManager.GradientControls.Add(Panel_GradientType);
  ToolManager.DeformationControls.Add(Panel_Grid);
  ToolManager.TextControls.Add(Panel_Text);
  ToolManager.TextControls.Add(Panel_TextOutline);
  ToolManager.PhongControls.Add(Panel_PhongShape);
  ToolManager.AltitudeControls.Add(Panel_Altitude);
  ToolManager.PerspectiveControls.Add(Panel_PerspectiveOption);
  ToolManager.PenColorControls.Add(Panel_Color);
  ToolManager.TextureControls.Add(Panel_Texture);
  ToolManager.BrushControls.Add(Panel_Brush);
end;

procedure TFMain.InitToolbarElements;
var
  i: Integer;
begin
  Panel_Embedded.Visible := LazPaintInstance.Embedded;
  Panel_File.Visible := not LazPaintInstance.Embedded;

  UpdateToolImage;
  Shape_PenColor.Brush.Color := BGRAToColor(ToolManager.ToolForeColor);
  SpinEdit_PenOpacity.Value := ToolManager.ToolForeColor.alpha;
  Shape_BackColor.Brush.Color := BGRAToColor(ToolManager.ToolBackColor);
  SpinEdit_BackOpacity.Value := ToolManager.ToolBackColor.alpha;
  SpinEdit_PenWidth.Value := Round(ToolManager.ToolPenWidth*PenWidthFactor);
  SpinEdit_ArrowSizeX.Value := round(ToolManager.ToolArrowSize.x*PenWidthFactor);
  SpinEdit_ArrowSizeY.Value := round(ToolManager.ToolArrowSize.y*PenWidthFactor);
  Tool_DrawShapeBorder.Down := ToolManager.ToolOptionDrawShape;
  Tool_FillShape.Down := ToolManager.ToolOptionFillShape;
  Tool_CloseShape.Down := ToolManager.ToolOptionCloseShape;
  SpinEdit_Eraser.Value := ToolManager.ToolEraserAlpha;
  SpinEdit_Tolerance.Value := ToolManager.ToolTolerance;
  Tool_ProgressiveFloodfill.Down := ToolManager.ToolFloodFillOptionProgressive;
  Tool_SinGradient.Down := ToolManager.ToolGradientSine;
  Combo_GradientColorspace.ItemIndex := Combo_GradientColorspace.Items.IndexOf(GradientColorSpaceToDisplay(ToolManager.ToolGradientColorspace));
  Tool_GridMoveWithoutDeformation.Down := ToolManager.ToolDeformationGridMoveWithoutDeformation;
  SpinEdit_GridNbX.Value := ToolManager.ToolDeformationGridNbX-1;
  SpinEdit_GridNbY.Value := ToolManager.ToolDeformationGridNbY-1;
  if ToolManager.ToolSplineEasyBezier then
    Combo_SplineStyle.ItemIndex := Combo_SplineStyle.Items.IndexOf('Easy Bézier')
  else
    Combo_SplineStyle.ItemIndex:= ord(ToolManager.ToolSplineStyle);
  UpdateCurveMode;
  Tool_TextOutline.Down := ToolManager.ToolTextOutline;
  SpinEdit_TextOutlineWidth.Value := ToolManager.ToolTextOutlineWidth;
  Tool_TextShadow.Down := ToolManager.ToolTextShadow;
  Tool_TextPhong.Down := ToolManager.ToolTextPhong;
  FontDialog1.Font := ToolManager.ToolTextFont;
  SpinEdit_TextBlur.Value := ToolManager.ToolTextBlur;
  SpinEdit_TextSize.Value := ToolManager.ToolTextFont.Size;
  SpinEdit_TextShadowX.Value := ToolManager.ToolTextShadowOffset.X;
  SpinEdit_TextShadowY.Value := ToolManager.ToolTextShadowOffset.Y;
  SpinEdit_TextureOpacity.Value := ToolManager.ToolTextureOpacity;
  if ToolManager.ToolShapeType = 'RoundRectangle' then
    Tool_PhongShapeRoundRect.Down := true;
  if ToolManager.ToolShapeType = 'Sphere' then
    Tool_PhongShapeSphere.Down := true;
  if ToolManager.ToolShapeType = 'Cone' then
    Tool_PhongShapeCone.Down := true;
  if ToolManager.ToolShapeType = 'VerticalCone' then
    Tool_PhongShapeVerticalCone.Down := true;
  if ToolManager.ToolShapeType = 'HorizontalCylinder' then
    Tool_PhongShapeHorizontalCylinder.Down := true;
  if ToolManager.ToolShapeType = 'VerticalCylinder' then
    Tool_PhongShapeVerticalCylinder.Down := true;
  UpdatePanelPhongShape;
  SpinEdit_ShapeAltitude.Value := ToolManager.ToolShapeAltitude;
  SpinEdit_ShapeAltitudeChange(nil);
  SpinEdit_PhongBorderSize.Value := ToolManager.ToolShapeBorderSize;
  UpdateLineCapBar;
  ComboBox_BrushSelect.Clear;
  for i := 0 to ToolManager.ToolBrushCount-1 do
    ComboBox_BrushSelect.Items.Add(inttostr(i));
  if ComboBox_BrushSelect.Items.Count > 0 then
    ComboBox_BrushSelect.ItemIndex := 0;
end;

procedure TFMain.UpdateToolbar;
var maxGridNbX,maxGridNbY: integer;
    colorChange: boolean;
begin
  colorChange:= false;
  if Shape_PenColor.Brush.Color <> BGRAToColor(ToolManager.ToolForeColor) then
  begin
    Shape_PenColor.Brush.Color := BGRAToColor(ToolManager.ToolForeColor);
    colorChange:= true;
  end;
  if SpinEdit_PenOpacity.Value <> ToolManager.ToolForeColor.alpha then
  begin
    SpinEdit_PenOpacity.Value := ToolManager.ToolForeColor.alpha;
    colorChange:= true;
  end;

  if not FInPenWidthChange then
  begin
    if SpinEdit_PenWidth.Value<> round(ToolManager.ToolPenWidth*PenWidthFactor) then
    begin
      FInPenWidthChange:= true;
      SpinEdit_PenWidth.Value := round(ToolManager.ToolPenWidth*PenWidthFactor);
      FInPenWidthChange:= false;
    end;
  end;

  if Shape_BackColor.Brush.Color <> BGRAToColor(ToolManager.ToolBackColor) then
  begin
    Shape_BackColor.Brush.Color := BGRAToColor(ToolManager.ToolBackColor);
    colorChange:= true;
  end;
  if SpinEdit_BackOpacity.Value <> ToolManager.ToolBackColor.alpha then
  begin
    SpinEdit_BackOpacity.Value := ToolManager.ToolBackColor.alpha;
    colorChange:= true;
  end;

  if colorChange then
  begin
    Label_CurrentDiff.Caption := inttostr(round(BGRAWordDiff(ToolManager.ToolForeColor,ToolManager.ToolBackColor)/65535*100))+'%';
    Label_CurrentDiff.Update;
  end;

  maxGridNbX := Max(2,Min(image.Width div 2,50));
  maxGridNbY := Max(2,Min(image.Height div 2,50));
  if SpinEdit_GridNbX.MaxValue <> maxGridNbX then
    SpinEdit_GridNbX.MaxValue := maxGridNbX;
  if SpinEdit_GridNbY.MaxValue <> maxGridNbY then
    SpinEdit_GridNbY.MaxValue := maxGridNbY;

  if SpinEdit_TextOutlineWidth.Enabled <> not ToolManager.ToolTextPhong then
    SpinEdit_TextOutlineWidth.Enabled := not ToolManager.ToolTextPhong;

  LazPaintInstance.ColorToFChooseColor;

  UpdateCurveMode;
end;

procedure TFMain.UpdateLineCapBar;
begin
  if ToolManager.ToolOptionCloseShape then begin
    Panel_LineCap.Width := DoScaleX(27,OriginalDPI);
    with ToolBar10 do begin
      Tool_CapFlat.Visible:=False;
      Tool_CapRound.Visible:=False;
      Tool_CapSquare.Visible:=False;
    end;
  end
  else begin
    Panel_LineCap.Width := Panel_LineCap_FullSize;
    with ToolBar10 do begin
      Tool_CapFlat.Visible:=True;
      Tool_CapRound.Visible:=True;
      Tool_CapSquare.Visible:=True;
    end;
  end;
end;

procedure TFMain.UpdateToolImage;
var img: integer; ToolBmp, IconBmp: TBitmap; IconBGRA, Blur, ToolBGRA: TBGRABitmap;
    a: TAction;
begin
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
  if img <> previousToolImg then
  begin
    if img <> -1 then
    begin
      IconBGRA := TBGRABitmap.Create(Image_CurrentTool.Width,Image_CurrentTool.Height);

      ToolBmp := TBitmap.Create;
      ImageList16.GetBitmap(img,ToolBmp);
      ToolBGRA := TBGRABitmap.Create(ToolBmp);
      FreeAndNil(ToolBmp);

      IconBGRA.PutImage((IconBGRA.Width-ToolBGRA.Width) div 2, (IconBGRA.Height-ToolBGRA.Height) div 2,ToolBGRA,dmSet);
      Blur := IconBGRA.FilterBlurRadial(3,rbNormal) as TBGRABitmap;
      IconBGRA.PutImage(0,0,Blur,dmSet);
      FreeAndNil(Blur);
      IconBGRA.PutImage((IconBGRA.Width-ToolBGRA.Width) div 2, (IconBGRA.Height-ToolBGRA.Height) div 2,ToolBGRA,dmDrawWithTransparency);
      FreeAndNil(ToolBGRA);

      IconBmp := IconBGRA.MakeBitmapCopy(ColorToRGB(clBtnFace));
      FreeAndNil(IconBGRA);
      Image_CurrentTool.Picture.Assign(IconBmp);
      IconBmp.Free;
    end else
      Image_CurrentTool.Picture.Clear;
    previousToolImg := img;
  end;
  Panel_Texture.Enabled := (CurrentTool <> ptTextureMapping);
end;

procedure TFMain.NoTextureIcon;
var lIcon: TBitmap;
begin
  Image_CurrentTexture.Picture.Clear;
  lIcon := TBitmap.Create;
  ImageList16.GetBitmap(56,lIcon);
  Image_CurrentTexture.Picture.Assign(lIcon);
  lIcon.Free;
end;

procedure TFMain.UpdateTextureIcon;
var
  IconBGRA: TBGRABitmap;
  IconBmp: TBitmap;
begin
  if ToolManager.GetToolTexture = nil then
  begin
    NoTextureIcon;
    exit;
  end;
  IconBGRA := ToolManager.GetToolTexture.Resample(Image_CurrentTexture.Width,Image_CurrentTexture.Height) as TBGRABitmap;
  IconBmp := IconBGRA.MakeBitmapCopy(ColorToRGB(clBtnFace));
  FreeAndNil(IconBGRA);
  Image_CurrentTexture.Picture.Assign(IconBmp);
  IconBmp.Free;
end;

function TFMain.TextSpinEditFocused: boolean;
begin
  result := SpinEdit_TextSize.Focused or SpinEdit_TextBlur.Focused or SpinEdit_TextShadowX.Focused or
           SpinEdit_TextShadowY.Focused or SpinEdit_ShapeAltitude.Focused or
           SpinEdit_TextOutlineWidth.Focused;
end;

procedure TFMain.UpdateBrush;
var oldInit: boolean;
  i: Integer;
begin
  oldInit:= initialized;
  initialized := false;
  try
    Panel_PenWidthPreview.Invalidate;
    ComboBox_BrushSelect.Clear;
    for i := 0 to ToolManager.ToolBrushCount-1 do
      ComboBox_BrushSelect.Items.Add(inttostr(i));
    if (ToolManager.ToolBrushInfoIndex >= 0) and
      (ToolManager.ToolBrushInfoIndex < ComboBox_BrushSelect.Items.Count) then
      ComboBox_BrushSelect.ItemIndex := ToolManager.ToolBrushInfoIndex;
  except
    on ex:exception do
      LazPaintInstance.ShowError(rsLazPaint,ex.Message);
  end;
  initialized := oldInit;
end;

procedure TFMain.SpinEdit_ShapeAltitudeChange(Sender: TObject);
begin
  if SpinEdit_ShapeAltitude.Value < 6 then
    SpinEdit_ShapeAltitude.Increment := 1
  else if SpinEdit_ShapeAltitude.Value < 25 then
    SpinEdit_ShapeAltitude.Increment := 3
  else
    SpinEdit_ShapeAltitude.Increment := 5;
  if initialized then
  begin
    if ToolManager.ToolShapeAltitude = SpinEdit_ShapeAltitude.Value then exit;
    ToolManager.ToolShapeAltitude := SpinEdit_ShapeAltitude.Value;
    UpdateEditPicture;
  end;
end;

procedure TFMain.SpinEdit_TextSizeChange(Sender: TObject);
begin
  if initialized and not FInTextFont then
  begin
    if ToolManager.ToolTextFont.Size = SpinEdit_TextSize.Value then exit;
    ToolManager.ToolTextFont.Size := SpinEdit_TextSize.Value;
    UpdateEditPicture(True);
    FActiveSpinEdit := SpinEdit_TextSize;
  end;
end;

procedure TFMain.SpinEdit_TextureOpacityChange(Sender: TObject);
begin
  if initialized then
  begin
    if ToolManager.ToolTextureOpacity = SpinEdit_TextureOpacity.Value then exit;
    ToolManager.ToolTextureOpacity := SpinEdit_TextureOpacity.Value;
    UpdateEditPicture(True);
    FActiveSpinEdit := SpinEdit_TextureOpacity;
  end;
end;

procedure TFMain.SpinEdit_TextBlurChange(Sender: TObject);
begin
  if initialized then
  begin
    if ToolManager.ToolTextBlur = SpinEdit_TextBlur.Value then exit;
    ToolManager.ToolTextBlur := SpinEdit_TextBlur.Value;
    UpdateEditPicture(True);
    FActiveSpinEdit := SpinEdit_TextBlur;
  end;
end;

procedure TFMain.GridNb_SpinEditChange(Sender: TObject);
begin
  if not initialized then exit;
  if ToolManager.SetToolDeformationGridSize(SpinEdit_GridNbX.Value+1,SpinEdit_GridNbY.Value+1) then
    image.OnImageChanged.NotifyObservers;
end;

procedure TFMain.SpinEdit_GridNbExit(Sender: TObject);
begin
  if SpinEdit_GridNbX.Value < 2 then SpinEdit_GridNbX.Value := 2;
  if SpinEdit_GridNbY.Value < 2 then SpinEdit_GridNbY.Value := 2;
end;

procedure TFMain.SpinEdit_TextOutlineWidthChange(Sender: TObject);
begin
  if initialized then
  begin
     if ToolManager.ToolTextOutlineWidth <> SpinEdit_TextOutlineWidth.Value then
     begin
       ToolManager.ToolTextOutlineWidth:= SpinEdit_TextOutlineWidth.Value;
       UpdateEditPicture;
     end;
  end;
end;

procedure TFMain.SpinEdit_TextShadowXChange(Sender: TObject);
begin
  if initialized then
  begin
    if ToolManager.ToolTextShadowOffset.X = SpinEdit_TextShadowX.Value then exit;
    ToolManager.ToolTextShadowOffset.X := SpinEdit_TextShadowX.Value;
    UpdateEditPicture(True);
    FActiveSpinEdit := SpinEdit_TextShadowX;
  end;
end;

procedure TFMain.SpinEdit_TextShadowYChange(Sender: TObject);
begin
  if initialized then
  begin
    if ToolManager.ToolTextShadowOffset.Y = SpinEdit_TextShadowY.Value then exit;
    ToolManager.ToolTextShadowOffset.Y := SpinEdit_TextShadowY.Value;
    UpdateEditPicture(True);
    FActiveSpinEdit := SpinEdit_TextShadowY;
  end;
end;

procedure TFMain.Perspective_RepeatClick(Sender: TObject);
begin
  if initialized then
  begin
    ToolManager.ToolPerspectiveRepeat := Perspective_Repeat.Down;
    UpdateEditPicture;
  end;
end;

procedure TFMain.Perspective_TwoPlanesClick(Sender: TObject);
begin
  if initialized then
  begin
    ToolManager.ToolPerspectiveTwoPlanes := Perspective_TwoPlanes.Down;
    UpdateEditPicture;
  end;
end;

procedure TFMain.SwitchColors;
var temp: TBGRAPixel;
begin
  temp := ToolManager.ToolForeColor;
  ToolManager.ToolForeColor := ToolManager.ToolBackColor;
  ToolManager.ToolBackColor := temp;
  UpdateToolbar;
  UpdateEditPicture;
end;

procedure TFMain.Shape_PenColorMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if LazPaintInstance.ChooseColorVisible and (Button = mbLeft) then
    LazPaintInstance.ChooseColorTarget := ctForeColor
  else
    ShowColorDialogForPen;
end;

procedure TFMain.ShowColorDialogForPen;
begin
  ColorDialog1.Color := BGRAToColor(ToolManager.ToolForeColor);
  if ColorDialog1.Execute then
  begin
       ToolManager.ToolForeColor := ColorToBGRA(ColorDialog1.Color,ToolManager.ToolForeColor.alpha);
       Shape_PenColor.Brush.Color := BGRAToColor(ToolManager.ToolForeColor);
       LazPaintInstance.ColorToFChooseColor;
       UpdateEditPicture;
  end;
end;

procedure TFMain.SpinEdit_PenOpacityChange(Sender: TObject);
begin
  if initialized then
  begin
    if ToolManager.ToolForeColor.alpha = SpinEdit_PenOpacity.value then exit;
    with ToolManager.ToolForeColor do
      ToolManager.ToolForeColor := BGRA(red,green,blue,SpinEdit_PenOpacity.value);
    LazPaintInstance.ColorToFChooseColor;
    UpdateEditPicture;
  end;
end;

procedure TFMain.Shape_BackColorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if LazPaintInstance.ChooseColorVisible and (Button = mbLeft) then
    LazPaintInstance.ChooseColorTarget := ctBackColor
  else
    ShowColorDialogForBack;
end;

procedure TFMain.ShowColorDialogForBack;
begin
  ColorDialog1.Color := BGRAToColor(ToolManager.ToolBackColor);
  if ColorDialog1.Execute then
  begin
       ToolManager.ToolBackColor := ColorToBGRA(ColorDialog1.Color,ToolManager.ToolBackColor.alpha);
       Shape_BackColor.Brush.Color := BGRAToColor(ToolManager.ToolBackColor);
       LazPaintInstance.ColorToFChooseColor;
       UpdateEditPicture;
  end;
end;

procedure TFMain.SpinEdit_BackOpacityChange(Sender: TObject);
begin
  if initialized then
  begin
    if ToolManager.ToolBackColor.alpha = SpinEdit_BackOpacity.value then exit;
    with ToolManager.ToolBackColor do
      ToolManager.ToolBackColor := BGRA(red,green,blue,SpinEdit_BackOpacity.value);
    LazPaintInstance.ColorToFChooseColor;
    UpdateEditPicture;
  end;
end;

procedure TFMain.Image_SwapColorsMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  SwitchColors;
end;

procedure TFMain.SpinEdit_ToleranceChange(Sender: TObject);
begin
  if initialized then
  begin
    if ToolManager.ToolTolerance = SpinEdit_Tolerance.Value then exit;
    ToolManager.ToolTolerance := SpinEdit_Tolerance.Value;
  end;
end;

procedure TFMain.Image_CurrentTextureMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  LazPaintInstance.EditTexture;
  UpdateTextureIcon;
  UpdateEditPicture;
end;

procedure TFMain.Tool_DrawShapeBorderClick(Sender: TObject);
begin
  if initialized then
  begin
     if not Tool_DrawShapeBorder.Down and not Tool_FillShape.Down then
        Tool_DrawShapeBorder.Down := true;
     ToolManager.ToolOptionDrawShape:= Tool_DrawShapeBorder.Down;
     UpdateEditPicture;
  end;
end;

procedure TFMain.Tool_FillShapeClick(Sender: TObject);
begin
  if initialized then
  begin
     if not Tool_DrawShapeBorder.Down and not Tool_FillShape.Down then
        Tool_FillShape.Down := true;
     ToolManager.ToolOptionFillShape:= Tool_FillShape.Down;
     UpdateEditPicture;
  end;
end;

procedure TFMain.Tool_CloseShapeClick(Sender: TObject);
begin
  if initialized then
  begin
     if ToolManager.ToolOptionCloseShape <> Tool_CloseShape.Down then
     begin
       ToolManager.ToolOptionCloseShape:= Tool_CloseShape.Down;
       UpdateLineCapBar;
       FLayout.Arrange;
       UpdateEditPicture;
     end;
  end;
end;

procedure TFMain.Tool_CapFlatClick(Sender: TObject);
begin
  if Tool_CapFlat.Down then
  begin
    ToolManager.ToolLineCap := pecFlat;
    ComboBox_ArrowStart.Refresh;
    ComboBox_ArrowEnd.Refresh;
    UpdateEditPicture;
  end;
end;

procedure TFMain.Tool_CapRoundClick(Sender: TObject);
begin
  if Tool_CapRound.Down then
  begin
    ToolManager.ToolLineCap := pecRound;
    ComboBox_ArrowStart.Refresh;
    ComboBox_ArrowEnd.Refresh;
    UpdateEditPicture;
  end;
end;

procedure TFMain.Tool_CapSquareClick(Sender: TObject);
begin
  if Tool_CapSquare.Down then
  begin
    ToolManager.ToolLineCap := pecSquare;
    ComboBox_ArrowStart.Refresh;
    ComboBox_ArrowEnd.Refresh;
    UpdateEditPicture;
  end;
end;

procedure TFMain.ComboBox_ArrowStartChange(Sender: TObject);
begin
  if initialized then UpdateEditPicture;
end;

procedure TFMain.ComboBox_ArrowStartDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
begin
  if Index = -1 then exit;
  ToolManager.ToolArrowStart := ComboBox_ArrowStart.Items[Index];
  DrawArrow(ComboBox_ArrowStart.Canvas,ARect,True,ToolManager.ToolArrowStart,ToolManager.ToolLineCap,State);
end;

procedure TFMain.ComboBox_ArrowEndChange(Sender: TObject);
begin
  if initialized then UpdateEditPicture;
end;

procedure TFMain.BrushLoadFromFileExecute(Sender: TObject);
begin
  ShowOpenBrushDialog;
end;

procedure TFMain.BrushLoadFromFileUpdate(Sender: TObject);
begin
  BrushLoadFromFile.Enabled := ToolManager.ToolBrushCount < 9;
end;

procedure TFMain.BrushRemoveCurrentExecute(Sender: TObject);
begin
  ToolManager.RemoveBrushAt(ToolManager.ToolBrushInfoIndex);
  UpdateBrush;
end;

procedure TFMain.BrushRemoveCurrentUpdate(Sender: TObject);
begin
  BrushRemoveCurrent.Enabled := ToolManager.ToolBrushInfoIndex > 0;
end;

procedure TFMain.ComboBox_ArrowEndDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
begin
  if Index = -1 then exit;
  ToolManager.ToolArrowEnd := ComboBox_ArrowEnd.Items[Index];
  DrawArrow(ComboBox_ArrowEnd.Canvas,ARect,False,ToolManager.ToolArrowEnd,ToolManager.ToolLineCap,State);
end;

procedure TFMain.SpinEdit_ArrowSizeChange(Sender: TObject);
begin
  if initialized then
  begin
    if (round(ToolManager.ToolArrowSize.x*PenWidthFactor) = SpinEdit_ArrowSizeX.value) and
       (round(ToolManager.ToolArrowSize.y*PenWidthFactor) = SpinEdit_ArrowSizeY.value) then exit;
    ToolManager.ToolArrowSize := PointF(SpinEdit_ArrowSizeX.value/PenWidthFactor,SpinEdit_ArrowSizeY.value/PenWidthFactor);
    UpdateEditPicture;
  end;
end;

procedure TFMain.Tool_JoinBevelClick(Sender: TObject);
begin
  if Tool_JoinBevel.Down then
  begin
    ToolManager.ToolJoinStyle := pjsBevel;
    UpdateEditPicture;
  end;
end;

procedure TFMain.Tool_JoinRoundClick(Sender: TObject);
begin
  if Tool_JoinRound.Down then
  begin
    ToolManager.ToolJoinStyle := pjsRound;
    UpdateEditPicture;
  end;
end;

procedure TFMain.Tool_JoinMiterClick(Sender: TObject);
begin
  if Tool_JoinMiter.Down then
  begin
    ToolManager.ToolJoinStyle := pjsMiter;
    UpdateEditPicture;
  end;
end;

procedure TFMain.Tool_PenSolidClick(Sender: TObject);
begin
  if Tool_PenSolid.Down then
  begin
    ToolManager.ToolPenStyle := psSolid;
    UpdateEditPicture;
  end;
end;

procedure TFMain.Tool_PenDashClick(Sender: TObject);
begin
  if Tool_PenDash.Down then
  begin
    ToolManager.ToolPenStyle := psDash;
    UpdateEditPicture;
  end;
end;

procedure TFMain.Tool_PenDashDotClick(Sender: TObject);
begin
  if Tool_PenDashDot.Down then
  begin
    ToolManager.ToolPenStyle := psDashDot;
    UpdateEditPicture;
  end;
end;

procedure TFMain.Tool_PenDashDotDotClick(Sender: TObject);
begin
  if Tool_PenDashDotDot.Down then
  begin
    ToolManager.ToolPenStyle := psDashDotDot;
    UpdateEditPicture;
  end;
end;

procedure TFMain.Tool_PenDotClick(Sender: TObject);
begin
  if Tool_PenDot.Down then
  begin
    ToolManager.ToolPenStyle := psDot;
    UpdateEditPicture;
  end;
end;

procedure TFMain.SpinEdit_EraserChange(Sender: TObject);
begin
    if initialized then
    begin
      if ToolManager.ToolEraserAlpha = SpinEdit_Eraser.value then exit;
      ToolManager.ToolEraserAlpha := SpinEdit_Eraser.Value;
    end;
end;

procedure TFMain.Tool_EraseOptionClick(Sender: TObject);
begin
  if Tool_EraseAlpha.Down then ToolManager.ToolEraserMode:= emEraseAlpha else
  if Tool_EraseBlur.Down then ToolManager.ToolEraserMode:= emSoften;
end;

procedure TFMain.Tool_GridMoveWithoutDeformationClick(Sender: TObject);
begin
    if initialized then
      ToolManager.ToolDeformationGridMoveWithoutDeformation :=
        Tool_GridMoveWithoutDeformation.Down;
end;

procedure TFMain.SetCurveMode(AMode: TToolSplineMode);
begin
  if (ToolManager.CurrentTool <> nil) and
     (ToolManager.CurrentTool is TToolGenericSpline) then
  begin
    (ToolManager.CurrentTool as TToolGenericSpline).CurrentMode := AMode;
    UpdateCurveMode;
  end;
end;

procedure TFMain.UpdateCurveMode;
var
  cm: TToolSplineMode;
  splineTool: TToolGenericSpline;
begin
  if (ToolManager.CurrentTool <> nil) and (ToolManager.CurrentTool is TToolGenericSpline) then
  begin
    splineTool := ToolManager.CurrentTool as TToolGenericSpline;
    Tool_CurveMovePoint.Enabled := not splineTool.IsHandDrawing and not splineTool.IsIdle;
    cm := splineTool.CurrentMode;
    if Tool_CurveMovePoint.Down <> (cm = tsmMovePoint) then
      Tool_CurveMovePoint.Down := (cm = tsmMovePoint);
    if Tool_CurveModeAuto.Down <> (cm = tsmCurveModeAuto) then
      Tool_CurveModeAuto.Down := (cm = tsmCurveModeAuto);
    if Tool_CurveModeAngle.Down <> (cm = tsmCurveModeAngle) then
      Tool_CurveModeAngle.Down := (cm = tsmCurveModeAngle);
    if Tool_CurveModeCurve.Down <> (cm = tsmCurveModeSpline) then
      Tool_CurveModeCurve.Down := (cm = tsmCurveModeSpline);
  end;
end;

procedure TFMain.Combo_SplineStyleChange(Sender: TObject);
var v: string;
begin
  if initialized then
  begin
    v := Combo_SplineStyle.Text;
    ToolManager.ToolSplineEasyBezier := false;
    if v = 'Inside' then ToolManager.ToolSplineStyle := ssInside else
    if v = 'Inside + ends' then ToolManager.ToolSplineStyle := ssInsideWithEnds else
    if v = 'Crossing' then ToolManager.ToolSplineStyle := ssCrossing else
    if v = 'Crossing + ends' then ToolManager.ToolSplineStyle := ssCrossingWithEnds else
    if v = 'Outside' then ToolManager.ToolSplineStyle := ssOutside else
    if v = 'Round outside' then ToolManager.ToolSplineStyle:= ssRoundOutside else
    if v = 'Vertex to side' then ToolManager.ToolSplineStyle:= ssVertexToSide else
    if v = 'Easy Bézier' then ToolManager.ToolSplineEasyBezier := true;
    UpdateEditPicture(True);
    UpdateCurveMode;
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
       ToolManager.ToolFloodFillOptionProgressive := Tool_ProgressiveFloodfill.Down;
end;

procedure TFMain.ShowPenPreview(ShouldRepaint: boolean);
begin
  if not Panel_PenWidthPreview.Visible then
  begin
    Panel_PenWidthPreview.Left := Panel_PenWidth.Left;
    Panel_PenWidthPreview.Top := Panel_PenWidth.Top+Panel_PenWidth.Height;
    Panel_PenWidthPreview.Visible := True;
  end else
    if ShouldRepaint then
      PaintBox_PenPreview.Repaint;
  TimerHidePenPreview.Enabled := false;
  HidePenPreview(3000);
end;

procedure TFMain.HidePenPreview(TimeMs: Integer);
begin
  if Panel_PenWidthPreview.Visible then
  begin
    TimerHidePenPreview.Interval := TimeMs;
    TimerHidePenPreview.Enabled := true;
  end;
end;

procedure TFMain.Panel_ToolbarBackgroundMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  HidePenPreview();
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

procedure TFMain.SpinEdit_PenWidthChange(Sender: TObject);
begin
  if initialized and not FInPenWidthChange then
  begin
    if round(ToolManager.ToolPenWidth*PenWidthFactor) = max(SpinEdit_PenWidth.Value,MinPenWidthValue) then exit;
    FInPenWidthChange:= true;
    ToolManager.ToolPenWidth := max(SpinEdit_PenWidth.Value,MinPenWidthValue)/PenWidthFactor;
    ShowPenPreview(True);
    UpdateEditPicture;
    FInPenWidthChange:= false;
  end;
end;

procedure TFMain.SpinEdit_PenWidthExit(Sender: TObject);
begin
  if SpinEdit_PenWidth.Value < MinPenWidthValue then SpinEdit_PenWidth.Value := MinPenWidthValue;
end;

procedure TFMain.IncreasePenSize;
begin
  SpinEdit_PenWidth.Value := max(SpinEdit_PenWidth.Value+PenSizeDelta(1),MinPenWidthValue);
end;

procedure TFMain.DecreasePenSize;
begin
  SpinEdit_PenWidth.Value := max(SpinEdit_PenWidth.Value-PenSizeDelta(-1),MinPenWidthValue);
end;

function TFMain.PenSizeDelta(direction: integer): integer;
var v: integer;
begin
  v := SpinEdit_PenWidth.Value;
  if direction < 0 then dec(v);
  if v < 100 then result := 10 else
  if v < 200 then result := 20 else
  if v < 500 then result := 50 else
  if v < 1000 then result := 100 else
  if v < 2000 then result := 200 else
  if v < 5000 then result := 500 else
    result := 1000;
end;

procedure TFMain.PaintBox_PenPreviewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var t: integer;
begin
  t := ImageList16.Width;
  if (X < t) and (Y < t) then DecreasePenSize else
  if (X >= PaintBox_PenPreview.Width-t) and (Y < t) then IncreasePenSize;
  ShowPenPreview(False);
end;

procedure TFMain.PopupToolbarPopup(Sender: TObject);
begin
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

procedure TFMain.PaintBox_PenPreviewPaint(Sender: TObject);
var bmp: TBGRABitmap;
    x,y,t: integer;
    m: single;
    bi: TLazPaintBrush;
    visualSize: single;
begin
  bmp := TBGRABitmap.Create(PaintBox_PenPreview.Width,PaintBox_PenPreview.Height,ColorToBGRA(ColorToRGB(clBtnFace)));
  x := round(PaintBox_PenPreview.Width/2);
  y := round(PaintBox_PenPreview.Height/2);
  visualSize := ToolManager.ToolPenWidth * ZoomFactor;
  if (CurrentTool in[ptBrush,ptClone]) and (visualSize <= 2*bmp.width) and (visualSize <= 2*bmp.Height) then
  begin
    bi := ToolManager.ToolBrushInfo;
    if bi <> nil then
      with bi do
      begin
        Size := visualSize;
        bi.Put(bmp, x,y,ColorToBGRA(ColorToRGB(clBtnText)));
      end;
  end else
    bmp.FillEllipseAntialias(x,y,visualSize/2,visualSize/2,ColorToBGRA(ColorToRGB(clBtnText)));
  t := ImageList16.Width;
  m := t/10;
  x := 0;
  bmp.EllipseAntialias(x+(t-1)/2,(t-1)/2,t/2-m,t/2-m,BGRABlack,m,BGRAWhite);
  bmp.DrawLineAntialias(x+m*2.6,(t-1)/2,x+t-1-m*2.6,(t-1)/2,BGRABlack,m);
  x := bmp.Width-t;
  bmp.EllipseAntialias(x+(t-1)/2,(t-1)/2,t/2-m,t/2-m,BGRABlack,m,BGRAWhite);
  bmp.DrawLineAntialias(x+(t-1)/2,m*2.6,x+(t-1)/2,t-1-m*2.6,BGRABlack,m);
  bmp.DrawLineAntialias(x+m*2.6,(t-1)/2,x+t-1-m*2.6,(t-1)/2,BGRABlack,m);

  bmp.Draw(PaintBox_PenPreview.Canvas,0,0);
  bmp.Free;
end;

procedure TFMain.TimerHidePenPreviewTimer(Sender: TObject);
begin
  Panel_PenWidthPreview.Visible := False;
  TimerHidePenPreview.Enabled := false;
end;

procedure TFMain.ToolChangeDockingExecute(Sender: TObject);
begin
  if Layout.ToolBoxDocking = twLeft then
    Layout.ToolBoxDocking := twRight else
    Layout.ToolBoxDocking := twLeft;
end;

procedure TFMain.Tool_LinearGradientClick(Sender: TObject);
begin
     if Tool_LinearGradient.Down then
     begin
       ToolManager.ToolGradientType := gtLinear;
       UpdateEditPicture;
     end;
end;

procedure TFMain.Tool_ReflectedGradientClick(Sender: TObject);
begin
     if Tool_ReflectedGradient.Down then
     begin
       ToolManager.ToolGradientType := gtReflected;
       UpdateEditPicture;
     end;
end;

procedure TFMain.Tool_DiamondGradientClick(Sender: TObject);
begin
     if Tool_DiamondGradient.Down then
     begin
       ToolManager.ToolGradientType := gtDiamond;
       UpdateEditPicture;
     end;
end;

procedure TFMain.Tool_RadialGradientClick(Sender: TObject);
begin
     if Tool_RadialGradient.Down then
     begin
       ToolManager.ToolGradientType := gtRadial;
       UpdateEditPicture;
     end;
end;

procedure TFMain.Tool_SinGradientClick(Sender: TObject);
begin
  if initialized then
  begin
    ToolManager.ToolGradientSine := Tool_SinGradient.Down;
    UpdateEditPicture;
  end;
end;

procedure TFMain.Combo_GradientColorspaceChange(Sender: TObject);
begin
  if initialized then
  begin
    ToolManager.ToolGradientColorspace := DisplayToGradientColorSpace(Combo_GradientColorspace.Text);
    UpdateEditPicture(True);
  end;
end;

procedure TFMain.SpinEdit_PhongBorderSizeChange(Sender: TObject);
begin
  if initialized then
  begin
    if ToolManager.ToolShapeBorderSize = SpinEdit_PhongBorderSize.Value then exit;
    ToolManager.ToolShapeBorderSize := SpinEdit_PhongBorderSize.Value;
    UpdateEditPicture;
  end;
end;

procedure TFMain.UpdatePanelPhongShape;
begin
  SpinEdit_PhongBorderSize.Enabled := (ToolManager.ToolShapeType = 'Rectangle') or
        (ToolManager.ToolShapeType = 'RoundRectangle');
end;

procedure TFMain.Tool_PhongShapeRectangleClick(Sender: TObject);
begin
    if Tool_PhongShapeRectangle.Down then
    begin
      if initialized then ToolManager.ToolShapeType := 'Rectangle';
      UpdatePanelPhongShape;
      UpdateEditPicture;
    end;
end;

procedure TFMain.Tool_PhongShapeRoundRectClick(Sender: TObject);
begin
    if Tool_PhongShapeRoundRect.Down then
    begin
      if initialized then ToolManager.ToolShapeType := 'RoundRectangle';
      UpdatePanelPhongShape;
      UpdateEditPicture;
    end;
end;

procedure TFMain.Tool_PhongShapeSphereClick(Sender: TObject);
begin
    if Tool_PhongShapeSphere.Down then
    begin
      if initialized then ToolManager.ToolShapeType := 'Sphere';
      UpdatePanelPhongShape;
      UpdateEditPicture;
    end;
end;

procedure TFMain.Tool_PhongShapeConeClick(Sender: TObject);
begin
    if Tool_PhongShapeCone.Down then
    begin
      if initialized then ToolManager.ToolShapeType := 'Cone';
      UpdatePanelPhongShape;
      UpdateEditPicture;
    end;
end;

procedure TFMain.Tool_PhongShapeVerticalConeClick(Sender: TObject);
begin
  if Tool_PhongShapeVerticalCone.Down then
  begin
    if initialized then ToolManager.ToolShapeType := 'VerticalCone';
    UpdatePanelPhongShape;
    UpdateEditPicture;
  end
end;

procedure TFMain.Tool_PhongShapeHorizontalCylinderClick(Sender: TObject);
begin
  if Tool_PhongShapeHorizontalCylinder.Down then
  begin
    if initialized then ToolManager.ToolShapeType := 'HorizontalCylinder';
    UpdatePanelPhongShape;
    UpdateEditPicture;
  end
end;

procedure TFMain.Tool_PhongShapeVerticalCylinderClick(Sender: TObject);
begin
  if Tool_PhongShapeVerticalCylinder.Down then
  begin
    if initialized then ToolManager.ToolShapeType := 'VerticalCylinder';
    UpdatePanelPhongShape;
    UpdateEditPicture;
  end
end;

procedure TFMain.UpdatePanelTextWidth;
var showElem: boolean;
begin
  if ToolManager.ToolTextShadow then
  begin
    Panel_Text.Width := SpinEdit_TextShadowY.Left+SpinEdit_TextShadowY.Width+6;
    showElem := true;
  end
  else
  begin
    Panel_Text.Width := Toolbar15.Left + Toolbar15.Width + 6;
    showElem:= false;
  end;
  Label_TextBlur.Visible := showElem;
  SpinEdit_TextBlur.Visible := showElem;
  Label_ShadowOffset.Visible := showElem;
  SpinEdit_TextShadowX.Visible := showElem;
  SpinEdit_TextShadowY.Visible := showElem;
end;

procedure TFMain.Tool_TextFontClick(Sender: TObject);
var topmostInfo: TTopMostInfo;
begin
  FInTextFont := true;
  FontDialog1.Font.Assign(ToolManager.ToolTextFont);
  FontDialog1.Font.Color := BGRAToColor(ToolManager.ToolForeColor);
  topmostInfo := LazPaintInstance.HideTopmost;
  if FontDialog1.Execute then
  begin
    ToolManager.ToolTextFont.Assign(FontDialog1.Font);
    SpinEdit_TextSize.Value := FontDialog1.Font.Size;
    if FontDialog1.Font.Color <> BGRAToColor(ToolManager.ToolForeColor) then
    begin
      ToolManager.ToolForeColor := ColorToBGRA(FontDialog1.Font.Color,ToolManager.ToolForeColor.alpha);
      Shape_PenColor.Brush.Color := BGRAToColor(ToolManager.ToolForeColor);
      LazPaintInstance.ColorToFChooseColor;
    end;
    UpdateEditPicture;
  end;
  LazPaintInstance.ShowTopmost(topmostInfo);
  FInTextFont := false;
end;

procedure TFMain.Tool_TextAlignClick(Sender: TObject);
var newAlign: TAlignment;
begin
  if initialized then
  begin
    newAlign := taLeftJustify;
    if Tool_TextAlignCenter.Down then newAlign:= taCenter;
    if Tool_TextAlignRight.Down then newAlign := taRightJustify;
    if newAlign <> ToolManager.ToolTextAlign then
    begin
      ToolManager.ToolTextAlign := newAlign;
      UpdateEditPicture;
    end;
  end;
end;

procedure TFMain.Tool_TextPhongClick(Sender: TObject);
begin
  if initialized then
  begin
     if ToolManager.ToolTextPhong <> Tool_TextPhong.Down then
     begin
       ToolManager.ToolTextPhong:= Tool_TextPhong.Down;
       Panel_Altitude.Visible := ToolManager.ToolTextPhong;
       FLayout.Arrange;
       UpdateToolbar;
       UpdateEditPicture;
     end;
  end;
end;

procedure TFMain.Tool_TextShadowClick(Sender: TObject);
begin
  if initialized then
  begin
     if ToolManager.ToolTextShadow <> Tool_TextShadow.Down then
     begin
       ToolManager.ToolTextShadow:= Tool_TextShadow.Down;

       UpdatePanelTextWidth;
       FLayout.Arrange;
       UpdateToolbar;

       UpdateEditPicture;
     end;
  end;
end;

procedure TFMain.Tool_TextOutlineClick(Sender: TObject);
begin
  if initialized then
  begin
     if ToolManager.ToolTextOutline <> Tool_TextOutline.Down then
     begin
       ToolManager.ToolTextOutline:= Tool_TextOutline.Down;
       UpdateEditPicture;
     end;
  end;
end;

procedure TFMain.ComboBox_BrushSelectDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var brushIndex: integer;
    bi : TLazPaintBrush;
    bmp: TBGRABitmap;
    c2: TBGRAPixel;
    c: TBGRAPixel;
begin
  if Index = -1 then exit;
  if odSelected in State then
  begin
    c2 := ColorToBGRA(ColorToRGB(clHighlight));
    c := ColorToBGRA(ColorToRGB(clHighlightText));
  end else
  begin
    c2 := ColorToBGRA(ColorToRGB(clWindow));
    c := ColorToBGRA(ColorToRGB(clWindowText));
  end;
  brushIndex := StrToInt(ComboBox_BrushSelect.Items[Index]);
  bi := ToolManager.ToolBrushAt[brushIndex];
  if bi <> nil then
  with bi do
  begin
    bi.Size := ARect.Bottom-ARect.Top-2;
    bmp := TBGRABitmap.Create(ARect.Right-ARect.Left,ARect.Bottom-ARect.Top, c2);
    bi.Put(bmp, (bmp.Width-1) div 2, bi.BrushImage.Height div 2 + 1, c);
    ComboBox_BrushSelect.Canvas.Draw(ARect.Left,ARect.Top,bmp.Bitmap);
    bmp.Free;
  end;
end;

procedure TFMain.ComboBox_BrushSelectChange(Sender: TObject);
begin
  if initialized then
  begin
    ToolManager.ToolBrushInfoIndex := ComboBox_BrushSelect.ItemIndex;
    Panel_PenWidthPreview.Invalidate;
  end;
end;

procedure TFMain.SpinEdit_BrushSpacingChange(Sender: TObject);
begin
  if initialized then
  begin
    if ToolManager.ToolBrushSpacing = SpinEdit_BrushSpacing.Value then exit;
    ToolManager.ToolBrushSpacing := SpinEdit_BrushSpacing.Value;
  end;
end;

