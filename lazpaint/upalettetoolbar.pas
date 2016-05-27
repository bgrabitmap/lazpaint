unit UPaletteToolbar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls,
  BGRAVirtualScreen, BGRABitmap,
  LazPaintType, UVolatileScrollBar,
  BGRAPalette, BCButton, Menus,
  Dialogs, BGRABitmapTypes;

type
  TPaletteVisibilityChangedByUserHandler = procedure(Sender:TObject) of object;

  { TPaletteToolbar }

  TPaletteToolbar = class
    procedure DoClearPalette(Sender: TObject);
    procedure DoDefaultPalette(Sender: TObject);
    procedure DoDitherUsingPalette(Sender: TObject);
    procedure DoLoadAndMergePalette(Sender: TObject);
    procedure DoLoadPalette(Sender: TObject);
    procedure DoMake16Palette(Sender: TObject);
    procedure DoMake32Palette(Sender: TObject);
    procedure DoMake64Palette(Sender: TObject);
    procedure DoMake96Palette(Sender: TObject);
    procedure DoPosterizeUsingPalette(Sender: TObject);
    procedure DoSavePalette(Sender: TObject);
    procedure DoToggleAlphaChannel(Sender: TObject);
    procedure DoToggleShowPalette(Sender: TObject);
    procedure MenuButtonDown(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure PopupClose(Sender: TObject);
    procedure PopupOpen(Sender: TObject);
  private
    FPaletteItemHeight: integer;
    FPaletteItemWidth: integer;
    FPaletteAlphaWidth:integer;
    FContainer: TWinControl;
    FLazPaintInstance: TLazPaintCustomInstance;
    FOnVisibilityChangedByUser: TPaletteVisibilityChangedByUserHandler;
    FPanelPalette: TBGRAVirtualScreen;
    FVisible: boolean;
    FScrollbar: TVolatileScrollBar;
    FScrollPos: integer;
    FColors: TBGRAPalette;
    FMenuButton: TBCButton;
    FPopupMenu: TPopupMenu;
    FMergePalette, FTransparentPalette: boolean;
    FPopupCloseTime: TDateTime;
    FItemToggleAlpha: TMenuItem;
    FItemToggleVisible: TMenuItem;
    FPaletteColorRect: TRect;
    FPaletteColorItemHeight: integer;
    function GetPanelPalette: TBGRAVirtualScreen;
    procedure MakePalette(ACount: integer);
    procedure Quantize(ADither: TDitheringAlgorithm);
    function GetHeight: integer;
    function GetWidth: integer;
    procedure SetContainer(AValue: TWinControl);
    procedure SetLazPaintInstance(AValue: TLazPaintCustomInstance);
    procedure SetOnVisibilityChangedByUser(AValue: TPaletteVisibilityChangedByUserHandler);
    procedure SetVisible(AValue: boolean);
    procedure TryLoadPaletteFrom(AFilename: string);
    procedure TrySavePaletteTo(AFilename: string);
  protected
    procedure PickColor(Shift: TShiftState; X, Y: Integer);
    procedure PaletteChanged;
    procedure RepaintPalette(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FillPaletteWithDefault;
    procedure PanelMouseDown(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure PanelMouseMove(Sender: TObject; {%H-}Shift: TShiftState; X, Y: Integer
      );
    procedure PanelMouseUp(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure CreatePopupMenu;
    property PanelPalette: TBGRAVirtualScreen read GetPanelPalette;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddColor(AColor: TBGRAPixel);
    procedure RemoveColor(AColor: TBGRAPixel);
    procedure SetBounds(ALeft,ATop,AWidth,AHeight: integer);
    property Container: TWinControl read FContainer write SetContainer;
    property LazPaintInstance: TLazPaintCustomInstance read FLazPaintInstance write SetLazPaintInstance;
    property Visible: boolean read FVisible write SetVisible;
    property Width: integer read GetWidth;
    property Height: integer read GetHeight;
    property OnVisibilityChangedByUser: TPaletteVisibilityChangedByUserHandler read FOnVisibilityChangedByUser write SetOnVisibilityChangedByUser;
  end;

implementation

uses UScaleDPI, Graphics, Forms, UGraph,
  UResourceStrings, BGRAColorQuantization,
  ULayerAction, UCursors;

{ TPaletteToolbar }

procedure TPaletteToolbar.SetContainer(AValue: TWinControl);
begin
  if FContainer=AValue then Exit;
  if Assigned(FPanelPalette) and Assigned(FContainer) then
    FContainer.RemoveControl(FPanelPalette);
  FContainer:=AValue;
  if Assigned(FPanelPalette) and Assigned(FContainer) then
    FContainer.InsertControl(FPanelPalette);
end;

function TPaletteToolbar.GetWidth: integer;
begin
  result := PanelPalette.Width;
end;

procedure TPaletteToolbar.PanelMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and Assigned(FScrollbar) then
  begin
    if FScrollbar.MouseDown(X,Y) then
    begin
      FScrollPos := FScrollBar.Position;
      PanelPalette.RedrawBitmap;
    end;
  end;
  PickColor(Shift,X,Y);
end;

procedure TPaletteToolbar.PanelMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if PtInRect(Point(x,y),FPaletteColorRect) then
    PanelPalette.Cursor := crCustomColorPicker
  else
    PanelPalette.Cursor := crArrow;
  if Assigned(FScrollbar) then
  begin
    if FScrollbar.MouseMove(X,Y) then
    begin
      FScrollPos := FScrollBar.Position;
      PanelPalette.RedrawBitmap;
    end;
  end;
  PickColor(Shift,X,Y);
end;

procedure TPaletteToolbar.PanelMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and Assigned(FScrollbar) then
  begin
    if FScrollbar.MouseUp(X,Y) then
    begin
      FScrollPos := FScrollBar.Position;
      PanelPalette.RedrawBitmap;
    end;
  end;
end;

procedure TPaletteToolbar.CreatePopupMenu;
var
  item: TMenuItem;
begin
  if Assigned(FPopupMenu) then exit;
  FPopupMenu := TPopupMenu.Create(PanelPalette);
  FPopupMenu.OnPopup:=@PopupOpen;
  FPopupMenu.OnClose:=@PopupClose;

  item := TMenuItem.Create(FPopupMenu);
  item.Caption := rsLoadPalette;
  item.OnClick:=@DoLoadPalette;
  FPopupMenu.Items.Add(item);
  item := TMenuItem.Create(FPopupMenu);
  item.Caption := rsLoadAndMergePalette;
  item.OnClick:=@DoLoadAndMergePalette;
  FPopupMenu.Items.Add(item);
  item := TMenuItem.Create(FPopupMenu);
  item.Caption := rsSavePaletteAs;
  item.OnClick:=@DoSavePalette;
  FPopupMenu.Items.Add(item);

  FPopupMenu.Items.AddSeparator;

  item := TMenuItem.Create(FPopupMenu);
  item.Caption := StringReplace(rsMakeNColorsPaletteFromBitmap, '%1', '16', []);
  item.OnClick:=@DoMake16Palette;
  FPopupMenu.Items.Add(item);
  item := TMenuItem.Create(FPopupMenu);
  item.Caption := StringReplace(rsMakeNColorsPaletteFromBitmap, '%1', '32', []);
  item.OnClick:=@DoMake32Palette;
  FPopupMenu.Items.Add(item);
  item := TMenuItem.Create(FPopupMenu);
  item.Caption := StringReplace(rsMakeNColorsPaletteFromBitmap, '%1', '64', []);
  item.OnClick:=@DoMake64Palette;
  FPopupMenu.Items.Add(item);
  item := TMenuItem.Create(FPopupMenu);
  item.Caption := StringReplace(rsMakeNColorsPaletteFromBitmap, '%1', '96', []);
  item.OnClick:=@DoMake96Palette;
  FPopupMenu.Items.Add(item);

  FPopupMenu.Items.AddSeparator;

  item := TMenuItem.Create(FPopupMenu);
  item.Caption := rsClearPalette;
  item.OnClick:=@DoClearPalette;
  FPopupMenu.Items.Add(item);
  item := TMenuItem.Create(FPopupMenu);
  item.Caption := rsDefaultPalette;
  item.OnClick:=@DoDefaultPalette;
  FPopupMenu.Items.Add(item);

  FPopupMenu.Items.AddSeparator;

  item := TMenuItem.Create(FPopupMenu);
  item.Caption := rsPosterizeLayerUsingPalette;
  item.OnClick:=@DoPosterizeUsingPalette;
  FPopupMenu.Items.Add(item);
  item := TMenuItem.Create(FPopupMenu);
  item.Caption := rsDitherLayerUsingPalette;
  item.OnClick:=@DoDitherUsingPalette;
  FPopupMenu.Items.Add(item);

  FPopupMenu.Items.AddSeparator;

  item := TMenuItem.Create(FPopupMenu);
  item.Caption := rsPaletteIncludesAlphaChannel;
  item.OnClick:=@DoToggleAlphaChannel;
  FPopupMenu.Items.Add(item);
  FItemToggleAlpha := item;

  item := TMenuItem.Create(FPopupMenu);
  item.Caption := rsShowPalette;
  item.OnClick:=@DoToggleShowPalette;
  FPopupMenu.Items.Add(item);
  FItemToggleVisible := item;
end;

procedure TPaletteToolbar.DoClearPalette(Sender: TObject);
begin
  FColors.Clear;
  PaletteChanged;
end;

procedure TPaletteToolbar.DoDefaultPalette(Sender: TObject);
begin
  FillPaletteWithDefault;
end;

procedure TPaletteToolbar.DoDitherUsingPalette(Sender: TObject);
begin
  Quantize(daFloydSteinberg);
end;

procedure TPaletteToolbar.DoLoadAndMergePalette(Sender: TObject);
begin
  FMergePalette:= true;
  DoLoadPalette(Sender);
  FMergePalette:= false;
end;

procedure TPaletteToolbar.DoLoadPalette(Sender: TObject);
var
  openDialog: TOpenDialog;
begin
  openDialog := TOpenDialog.Create(nil);
  try
    if Assigned(FLazPaintInstance) then
      openDialog.InitialDir := FLazPaintInstance.Config.DefaultPaletteDirectory
    else
      openDialog.InitialDir := '';
    openDialog.Filter := BGRARegisteredPaletteFormatFilter(rsAllSupportedFiletypes);
    openDialog.DefaultExt := 'txt';
    if openDialog.Execute then
    begin
      if Assigned(FLazPaintInstance) then
        FLazPaintInstance.Config.SetDefaultPaletteDirectory(ExtractFilePath(openDialog.FileName));
      TryLoadPaletteFrom(openDialog.FileName);
    end;
  except
    on ex: exception do
      ShowMessage(ex.Message);
  end;
  openDialog.Free;
end;

procedure TPaletteToolbar.DoMake16Palette(Sender: TObject);
begin
  MakePalette(16);
end;

procedure TPaletteToolbar.DoMake32Palette(Sender: TObject);
begin
  MakePalette(32);
end;

procedure TPaletteToolbar.DoMake64Palette(Sender: TObject);
begin
  MakePalette(64);
end;

procedure TPaletteToolbar.DoMake96Palette(Sender: TObject);
begin
  MakePalette(96);
end;

procedure TPaletteToolbar.DoPosterizeUsingPalette(Sender: TObject);
begin
  Quantize(daNearestNeighbor);
end;

procedure TPaletteToolbar.DoSavePalette(Sender: TObject);
var
  saveDialog: TSaveDialog;
begin
  saveDialog := TSaveDialog.Create(nil);
  try
    if Assigned(FLazPaintInstance) then
      saveDialog.InitialDir := FLazPaintInstance.Config.DefaultPaletteDirectory
    else
      saveDialog.InitialDir := '';
    saveDialog.Filter := BGRARegisteredPaletteFormatFilter(rsAllSupportedFiletypes);
    saveDialog.DefaultExt := 'txt';
    if saveDialog.Execute then
    begin
      if Assigned(FLazPaintInstance) then
        FLazPaintInstance.Config.SetDefaultPaletteDirectory(ExtractFilePath(saveDialog.FileName));
      TrySavePaletteTo(saveDialog.FileName);
    end;
  except
    on ex: exception do
      ShowMessage(ex.Message);
  end;
  saveDialog.Free;
end;

procedure TPaletteToolbar.DoToggleAlphaChannel(Sender: TObject);
var temp: TBGRAPalette;
  i: Integer;
begin
  FTransparentPalette := not FTransparentPalette;
  if not FTransparentPalette then
  begin
    temp := TBGRAPalette.Create;
    for i := 0 to FColors.Count-1 do
      temp.AddColor(FColors.Color[i]);
    FColors.Clear;
    for i := 0 to temp.Count-1 do
      AddColor(temp.Color[i]);
    temp.Free;
  end;
  PaletteChanged;
end;

procedure TPaletteToolbar.DoToggleShowPalette(Sender: TObject);
begin
  Visible := not Visible;
  if Assigned(FOnVisibilityChangedByUser) then
    FOnVisibilityChangedByUser(self);
end;

procedure TPaletteToolbar.MenuButtonDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Now-FPopupCloseTime > 1/(24*60*60*10) then
    with FMenuButton.ClientToScreen(Point(0,FMenuButton.Height)) do
      FPopupMenu.PopUp(x,y);
end;

procedure TPaletteToolbar.PopupClose(Sender: TObject);
begin
  FMenuButton.Down := false;
  FPopupCloseTime := Now;
end;

procedure TPaletteToolbar.PopupOpen(Sender: TObject);
begin
  FMenuButton.Down := true;
  FItemToggleAlpha.Checked := FTransparentPalette;
  FItemToggleVisible.Checked := Visible;
end;

procedure TPaletteToolbar.MakePalette(ACount: integer);
var
  quant: TBGRAColorQuantizer;
  i: Integer;
begin
  DoClearPalette(nil);
  PanelPalette.Enabled := false;
  PanelPalette.Cursor := crHourGlass;
  Application.ProcessMessages;
  PanelPalette.Update;
  try
    if FTransparentPalette then
      quant := TBGRAColorQuantizer.Create(LazPaintInstance.Image.RenderedImage, acFullChannelInPalette, ACount)
    else
      quant := TBGRAColorQuantizer.Create(LazPaintInstance.Image.RenderedImage, acIgnore, ACount);
    Application.ProcessMessages;

    try
      with quant.ReducedPalette do
      begin
        self.FColors.Clear;
        for i := 0 to Count-1 do
          self.FColors.AddColor(Color[i]);
      end;
    finally
      quant.Free;
      PaletteChanged;
    end;
  except
    on ex: exception do
      ShowMessage(ex.Message);
  end;
  PanelPalette.Enabled := true;
  PanelPalette.Cursor := crDefault;
end;

function TPaletteToolbar.GetPanelPalette: TBGRAVirtualScreen;
var
  glyphBmp: TBitmap;
  size: integer;
begin
  if not Assigned(FPanelPalette) then
  begin
    if Assigned(LazPaintInstance) then
      FPaletteItemWidth := LazPaintInstance.Config.DefaultIconSize(24)
    else
      FPaletteItemWidth := 24;
    FPaletteItemHeight := FPaletteItemWidth*2 div 3;
    FPaletteAlphaWidth := FPaletteItemWidth div 3;

    FPanelPalette := TBGRAVirtualScreen.Create(nil);
    FPanelPalette.Width := DoScaleX(FPaletteItemWidth, OriginalDPI)+VolatileScrollBarSize;
    FPanelPalette.Visible := false;
    FPanelPalette.OnRedraw := @RepaintPalette;
    FPanelPalette.OnMouseDown:=@PanelMouseDown;
    FPanelPalette.OnMouseUp:=@PanelMouseUp;
    FPanelPalette.OnMouseMove:=@PanelMouseMove;
    FPanelPalette.BevelOuter := bvRaised;
    FPanelPalette.Color := clBtnFace;
    FPanelPalette.Caption := '';
    FColors := TBGRAPalette.Create;
    FTransparentPalette:= false;
    FMergePalette:= false;

    FMenuButton := TBCButton.Create(FPanelPalette);
    FMenuButton.Cursor := crArrow;
    FMenuButton.Rounding.RoundX := 6;
    FMenuButton.Rounding.RoundY := 6;
    BCAssignSystemStyle(FMenuButton);
    FMenuButton.DropDownArrow := true;
    FMenuButton.DropDownArrowSize := DoScaleY(FPaletteItemHeight div 2, OriginalDPI);
    glyphBmp := TBitmap.Create;
    size := DoScaleY(FPaletteItemHeight*3 div 5, OriginalDPI);
    if not odd(size) then size += 1;
    glyphBmp.Width := size;
    glyphBmp.Height := size;
    glyphBmp.Canvas.Pen.Color := clBlack;
    glyphBmp.Canvas.Brush.Color := BGRAToColor(CSSLawnGreen);
    glyphBmp.Canvas.Rectangle(0,0,glyphBmp.Width div 2+1,glyphBmp.Height div 2+1);
    glyphBmp.Canvas.Brush.Color := clYellow;
    glyphBmp.Canvas.Rectangle(glyphBmp.Width div 2,0,glyphBmp.Width,glyphBmp.Height div 2+1);
    glyphBmp.Canvas.Brush.Color := BGRAToColor(CSSDodgerBlue);
    glyphBmp.Canvas.Rectangle(0,glyphBmp.Height div 2,glyphBmp.Width div 2+1,glyphBmp.Height);
    glyphBmp.Canvas.Brush.Color := BGRAToColor(CSSBlue);
    glyphBmp.Canvas.Rectangle(glyphBmp.Width div 2,glyphBmp.Height div 2,glyphBmp.Width,glyphBmp.Height);
    FMenuButton.Glyph := glyphBmp;
    glyphBmp.Free;
    FMenuButton.DropDownStyle := bdsCommon;
    FMenuButton.Caption := '';
    FMenuButton.SetBounds(2,2,FPanelPalette.Width-4, DoScaleY(FPaletteItemHeight*4 div 3, OriginalDPI));
    FMenuButton.OnMouseDown:=@MenuButtonDown;
    FMenuButton.Hint := rsPaletteOptions;
    FPanelPalette.InsertControl(FMenuButton);

    CreatePopupMenu;

    if Assigned(FPanelPalette) and Assigned(FContainer) then
        FContainer.InsertControl(FPanelPalette);
  end;
  result := FPanelPalette;
end;

procedure TPaletteToolbar.Quantize(ADither: TDitheringAlgorithm);
var
  LayerAction: TLayerAction;
  quant: TBGRAColorQuantizer;
begin
  if not LazPaintInstance.Image.CheckNoAction then exit;
  quant := TBGRAColorQuantizer.Create(FColors, not FTransparentPalette);
  LayerAction := nil;
  try
    LayerAction := TLayerAction.Create(LazPaintInstance.Image);
    LayerAction.ReplaceSelectedLayer(quant.GetDitheredBitmap(ADither,LayerAction.SelectedImageLayer) as TBGRABitmap, True);
    LazPaintInstance.image.LayerMayChangeCompletely(LayerAction.SelectedImageLayer);
    LayerAction.Validate;
  except
    on ex:Exception do
      LazPaintInstance.ShowError('Quantize',ex.Message);
  end;
  LayerAction.Free;
  quant.Free;
end;

function TPaletteToolbar.GetHeight: integer;
begin
  result := PanelPalette.Height;
end;

procedure TPaletteToolbar.SetLazPaintInstance(AValue: TLazPaintCustomInstance);
begin
  if FLazPaintInstance=AValue then Exit;
  FLazPaintInstance:=AValue;
  if Assigned(FLazPaintInstance) then
  begin
    FVisible := FLazPaintInstance.Config.DefaultPaletteToolbarVisible;
    PanelPalette.Visible := FVisible;
    FillPaletteWithDefault;
  end else
  begin
    FColors.Clear;
  end;
end;

procedure TPaletteToolbar.SetOnVisibilityChangedByUser(
  AValue: TPaletteVisibilityChangedByUserHandler);
begin
  if FOnVisibilityChangedByUser=AValue then Exit;
  FOnVisibilityChangedByUser:=AValue;
end;

procedure TPaletteToolbar.SetVisible(AValue: boolean);
begin
  if FVisible=AValue then Exit;
  FVisible:=AValue;
  PanelPalette.Visible := AValue;
  if Assigned(FLazPaintInstance) then
    FLazPaintInstance.Config.SetDefaultPaletteToolbarVisible(AValue);
end;

procedure TPaletteToolbar.TryLoadPaletteFrom(AFilename: string);
var tempPal: TBGRAPalette;
  i: Integer;
begin
  if not FMergePalette then FColors.Clear;
  tempPal:= TBGRAPalette.Create;
  try
    tempPal.LoadFromFile(AFilename);
    for i := 0 to tempPal.Count-1 do
      AddColor(tempPal.Color[i]);
  except
    on ex:Exception do
      ShowMessage(ex.Message);
  end;
  tempPal.Free;
  PaletteChanged;
end;

procedure TPaletteToolbar.TrySavePaletteTo(AFilename: string);
begin
  try
    FColors.SaveToFile(AFilename);
  except
    on ex:Exception do
      ShowMessage(ex.Message);
  end;
end;

procedure TPaletteToolbar.PickColor(Shift: TShiftState; X, Y: Integer);
var idx: integer;
  c : TBGRAPixel;
begin
  if PtInRect(Point(X,Y),FPaletteColorRect) then
  begin
    idx := (Y-FPaletteColorRect.Top) div FPaletteColorItemHeight;
    if (idx < 0) or (idx >= FColors.Count) then exit;
    if (ssLeft in Shift) and not (ssRight in Shift) then
    begin
      c := FColors.Color[idx];
      if not FTransparentPalette then c.alpha := LazPaintInstance.ToolManager.ToolForeColor.alpha;
      LazPaintInstance.ToolManager.ToolForeColor := c;
    end else
    if not (ssLeft in Shift) and (ssRight in Shift) then
    begin
      c := FColors.Color[idx];
      if not FTransparentPalette then c.alpha := LazPaintInstance.ToolManager.ToolBackColor.alpha;
      LazPaintInstance.ToolManager.ToolBackColor := c;
    end else
        exit;
    LazPaintInstance.UpdateToolbar;
    LazPaintInstance.UpdateEditPicture(False);
  end;
end;

procedure TPaletteToolbar.PaletteChanged;
begin
  FreeAndNil(FScrollbar);
  PanelPalette.DiscardBitmap;
end;

procedure TPaletteToolbar.RepaintPalette(Sender: TObject; Bitmap: TBGRABitmap);
var i,x,y,w,aw,a,h: integer;
  c: TBGRAPixel;
  nbVisible, maxScroll: integer;
  clInterm: TBGRAPixel;
begin
  clInterm := MergeBGRA(ColorToBGRA(ColorToRGB(clBtnFace)),ColorToBGRA(ColorToRGB(clBtnText)));
  x := 2;
  y := FMenuButton.Top+FMenuButton.Height+1;
  w := Bitmap.Width-(VolatileScrollBarSize+1)-x;
  aw := DoScaleX(FPaletteAlphaWidth, OriginalDPI);
  h := DoScaleY(FPaletteItemHeight, OriginalDPI);
  if h < 3 then h := 3;
  nbVisible := (Bitmap.Height - 2 - y - 1) div (h-1);
  if nbVisible < 1 then nbVisible:= 1;
  maxScroll := FColors.Count-nbVisible;
  if maxScroll < 0 then maxScroll:= 0;

  if FScrollPos < 0 then FScrollPos := 0;
  if FScrollPos > maxScroll then FScrollPos:= maxScroll;
  if maxScroll > 0 then
    if not Assigned(FScrollbar) then
      FScrollbar := TVolatileScrollBar.Create(Bitmap.Width-(VolatileScrollBarSize+1),y,
                              VolatileScrollBarSize,Bitmap.Height-2 - y,
                              sbVertical,FScrollPos,0,maxScroll);
  if Assigned(FScrollbar) then FScrollbar.Draw(Bitmap);
  if not Assigned(FScrollbar) then
    w := Bitmap.Width-2-x;
  FPaletteColorRect := rect(x,y,x+w,y);
  FPaletteColorItemHeight := h-1;
  for i := FScrollPos to FScrollPos+nbVisible-1 do
  if (i >= 0) and (i < FColors.Count) then
  begin
    c := FColors.Color[i];
    a := c.alpha;
    c.alpha := 255;
    if a = 0 then
    begin
      DrawCheckers(Bitmap, rect(x,y,x+w,y+h));
      Bitmap.Rectangle(x,y,x+w+1,y+h,clInterm,dmSet);
    end else
    if FTransparentPalette then
    begin
      Bitmap.Rectangle(x,y,x+w-aw+1,y+h,clInterm,c,dmSet);
      Bitmap.Rectangle(x+w-aw,y,x+w,y+h,clInterm,MergeBGRAWithGammaCorrection(BGRABlack,a,BGRAWhite,255-a),dmSet);
    end else
    begin
      Bitmap.Rectangle(x,y,x+w,y+h,clInterm,c,dmSet);
    end;
    y += h-1;
  end;
  FPaletteColorRect.Bottom := y;
end;

procedure TPaletteToolbar.FillPaletteWithDefault;
const defaultPal = '; paint.net Palette File'#13#10'FF000000'#13#10'FF101B3C'#13#10+
  'FF1C4C1B'#13#10'FF833D0A'#13#10'FF7A4049'#13#10'FF2850B6'#13#10'FF76319E'#13#10+
  'FFCF1718'#13#10'FF657728'#13#10'FF4664B4'#13#10'FF3DB239'#13#10'FF7F84D1'#13#10+
  'FFB7876B'#13#10'FF389AFF'#13#10'FFA991A1'#13#10'FFFC6E56'#13#10'FF45ADF0'#13#10+
  'FFCB985A'#13#10'FFCA69FB'#13#10'FF6CBA96'#13#10'FFE2B3F9'#13#10'FFFFAAE2'#13#10+
  'FFF1D66F'#13#10'FFD1CCF9'#13#10'FF6EF7EC'#13#10'FFECD98E'#13#10'FFA7E1FF'#13#10+
  'FFFAD0D0'#13#10'FFFEEE48'#13#10'FFB4FFFE'#13#10'FFFAEAED'#13#10'FFF9F7D1';
var
  stream: TStringStream;
  i: Integer;
begin
  if Assigned(LazPaintInstance) and LazPaintInstance.BlackAndWhite then
  begin
    FColors.Clear;
    for i := 0 to 15 do
      AddColor(GrayscaleToBGRA(i*$1111));
  end else
  begin
    stream := TStringStream.Create(defaultPal);
    try
      FColors.LoadFromStream(stream, FColors.DetectPaletteFormat(stream));
    except
      on ex:exception do
      ShowMessage(ex.Message);
    end;
    stream.Free;
  end;
  PaletteChanged;
end;

constructor TPaletteToolbar.Create;
begin
  FPanelPalette := nil;
end;

destructor TPaletteToolbar.Destroy;
begin
  Container := nil;
  FreeAndNil(FScrollbar);
  FreeAndNil(FPanelPalette);
  FreeAndNil(FColors);
  inherited Destroy;
end;

procedure TPaletteToolbar.AddColor(AColor: TBGRAPixel);
begin
  if not FTransparentPalette then
  begin
    if AColor.alpha = 0 then exit;
    AColor.alpha := 255;
  end;
  if LazPaintInstance.BlackAndWhite then
    AColor := BGRAToGrayscale(AColor);
  if FColors.AddColor(AColor) then PaletteChanged;
end;

procedure TPaletteToolbar.RemoveColor(AColor: TBGRAPixel);
begin
  if not FTransparentPalette then
  begin
    if AColor.alpha = 0 then exit;
    AColor.alpha := 255;
  end;
  if LazPaintInstance.BlackAndWhite then
    AColor := BGRAToGrayscale(AColor);
  if FColors.RemoveColor(AColor) then PaletteChanged;
end;

procedure TPaletteToolbar.SetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  FPanelPalette.SetBounds(ALeft,ATop,AWidth,AHeight);
  FreeAndNil(FScrollbar);
  FPanelPalette.DiscardBitmap;
end;

end.

