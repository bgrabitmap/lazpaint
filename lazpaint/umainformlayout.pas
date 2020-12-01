// SPDX-License-Identifier: GPL-3.0-only
unit UMainFormLayout;

{$mode objfpc}{$H+}

interface

uses
  Classes, LCLType, SysUtils, UMenu, Forms, LazPaintType, UZoom, ExtCtrls, ComCtrls,
  Menus, UPaletteToolbar, BGRABitmapTypes, Controls, UImageView;

const SelectionPaintDelay = 100/(1000*60*60*24);
  VerticalSplitterWidth = 4;

type
  TOnPictureAreaChange = procedure(ASender: TObject; ANewArea: TRect) of object;
  TToolWindowDocking = (twNone, twWindow, twLeft, twTop, twRight, twBottom);
  TLayoutStage = (lsAfterTopToolbar, lsAfterDockedToolBox, lsAfterPaletteToolbar,
               lsAfterDockedControlsPanel, lsAfterVerticalSplitter, lsAfterStatusBar);

  { TMainFormLayout }

  TMainFormLayout = class(TCustomMainFormLayout)
  private
    FForm: TForm;
    FOnPaintPicture: TNotifyEvent;
    FOnPictureMouseBefore: TPictureMouseBeforeEvent;
    FOnPictureMouseMove: TPictureMouseMoveEvent;
    FOnToolbarUpdate: TNotifyEvent;
    FZoom: TZoom;
    FMenu: TMainFormMenu;
    FLazPaintInstance: TLazPaintCustomInstance;
    FToolBoxDocking: TToolWindowDocking;
    FInSetToolBoxDocking: boolean;
    FPanelToolBox: TPanel;
    FDockedToolBoxToolBar: TToolBar;
    FPaletteToolbar: TPaletteToolbar;
    FStatusBarVisible: boolean;
    FStatusBar: TPanel;
    FStatusText: string;
    FStatusTextSplit: TStringList;
    FDarkTheme: boolean;
    FDockedControlsPanel: TPanel;
    FDockedChooseColorSplitter: TSplitter;
    FVerticalSplitter: TSplitter;
    FPaintBox: TOpaquePaintBox;
    FImageView: TImageView;
    FArranging: boolean;
    function GetFillSelectionHighlight: boolean;
    function GetMouseButtonState: TShiftState;
    function GetPaletteVisible: boolean;
    function GetPopupToolbox: TPopupMenu;
    function GetStatusBarVisible: boolean;
    function GetStatusText: string;
    function GetToolBoxVisible: boolean;
    function GetUpdatingPopup: boolean;
    procedure ImageToolbarUpdate(Sender: TObject);
    procedure ImageViewMouseBefore(ASender: TObject; AShift: TShiftState);
    procedure ImageViewMouseMove(Sender: TObject; APosition: TPointF);
    procedure ImageViewOnPaint(Sender: TObject);
    procedure SetFillSelectionHighlight(AValue: boolean);
    procedure SetOnPaintPicture(AValue: TNotifyEvent);
    procedure SetUpdatingPopup(AValue: boolean);
    procedure StatusBar_Paint(Sender: TObject);
    procedure ToolboxGroupMainButton_MouseMove(Sender: TObject; {%H-}Shift: TShiftState; {%H-}X,
      {%H-}Y: Integer);
    procedure SetDarkTheme(AValue: boolean);
    procedure SetLazPaintInstance(AValue: TLazPaintCustomInstance);
    procedure SetPaletteVisible(AValue: boolean);
    procedure SetPopupToolbox(AValue: TPopupMenu);
    procedure SetStatusBarVisible(AValue: boolean);
    procedure SetStatusText(AValue: string);
    procedure SetToolBoxDocking(AValue: TToolWindowDocking);
    procedure SetToolBoxVisible(AValue: boolean);
    function GetDefaultToolboxDocking: TToolWindowDocking;
    procedure ToolboxGroupButton_Click(Sender: TObject);
    procedure ToolboxGroupTimer_Timer(Sender: TObject);
    procedure ToolboxGroupToolbarButton_MouseEnter(Sender: TObject);
    procedure ToolboxGroupToolbarButton_MouseLeave(Sender: TObject);
    procedure ToolboxSimpleButton_Click(Sender: TObject);
    procedure PaletteVisibilityChangedByUser(Sender: TObject);
    procedure VerticalSplitterMoved(Sender: TObject);
    procedure ZoomChanged(sender: TZoom; ANewZoom: single);
  protected
    FDockedToolboxGroup: array of record
        Button: TToolButton;
        Panel: TPanel;
        Toolbar: TToolBar;
        Timer: TTimer;
      end;
    function GetWorkArea: TRect; override;
    function GetWorkAreaAt(AStage: TLayoutStage): TRect;
    function VerticalSplitterNeeded: boolean;
    procedure DoArrange;
    procedure ApplyTheme;
    function DockedControlsPanelWidth: integer;
    function DockedControlsPanelInnerMargin: integer;
    procedure ApplyThemeToDockedToolboxGroup(AGroupIndex: integer);
    procedure ShowToolboxGroup(AGroupIndex: integer);
    procedure HideToolboxGroup(AGroupIndex: integer);
    procedure HideToolboxGroups;
  public
    DelayedPaintPicture: boolean;
    constructor Create(AForm: TForm; AZoom: TZoom);
    destructor Destroy; override;
    function CatchToolKeyDown(var AKey: Word): boolean;
    function CatchToolKeyPress(var AKey: TUTF8Char): boolean;
    function CatchToolKeyUp(var AKey: Word): boolean;
    procedure Arrange;
    procedure FocusImage;
    procedure CheckDelayedUpdate;
    procedure DockedToolBoxAddButton(AAction: TBasicAction);
    procedure DockedToolBoxAddGroup(AActions: array of TBasicAction);
    procedure DockedToolBoxSetImages(AImages: TImageList);
    procedure AddColorToPalette(AColor : TBGRAPixel);
    procedure RemoveColorFromPalette(AColor : TBGRAPixel);
    procedure AddDockedControl(AControl: TControl);
    procedure RemoveDockedControl(AControl: TControl);
    procedure PaintPictureNow;
    procedure InvalidatePicture(AInvalidateAll: boolean);
    procedure ShowNoPicture;
    property Menu: TMainFormMenu read FMenu write FMenu;
    property Arranging: boolean read FArranging;
    property ToolBoxDocking: TToolWindowDocking read FToolBoxDocking write SetToolBoxDocking;
    property ToolBoxVisible: boolean read GetToolBoxVisible write SetToolBoxVisible;
    property LazPaintInstance: TLazPaintCustomInstance read FLazPaintInstance write SetLazPaintInstance;
    property ToolboxPopup: TPopupMenu read GetPopupToolbox write SetPopupToolbox;
    property PaletteVisible: boolean read GetPaletteVisible write SetPaletteVisible;
    property StatusBarVisible: boolean read GetStatusBarVisible write SetStatusBarVisible;
    property StatusText: string read GetStatusText write SetStatusText;
    property DefaultToolboxDocking: TToolWindowDocking read GetDefaultToolboxDocking;
    property DarkTheme: boolean read FDarkTheme write SetDarkTheme;
    property OnPaintPicture: TNotifyEvent read FOnPaintPicture write SetOnPaintPicture;
    property OnToolbarUpdate: TNotifyEvent read FOnToolbarUpdate write FOnToolbarUpdate;
    property OnPictureMouseMove: TPictureMouseMoveEvent read FOnPictureMouseMove write FOnPictureMouseMove;
    property OnPictureMouseBefore: TPictureMouseBeforeEvent read FOnPictureMouseBefore write FOnPictureMouseBefore;
    property FillSelectionHighlight: boolean read GetFillSelectionHighlight write SetFillSelectionHighlight;
    property UpdatingPopup: boolean read GetUpdatingPopup write SetUpdatingPopup;
    property MouseButtonState: TShiftState read GetMouseButtonState;
  end;

function ToolWindowDockingToStr(AValue: TToolWindowDocking): string;
function StrToToolWindowDocking(AValue: string): TToolWindowDocking;

implementation

uses Graphics, Toolwin, math, UDarkTheme, LCScaleDPI;

function ToolWindowDockingToStr(AValue: TToolWindowDocking): string;
begin
  case AValue of
    twNone: result := 'None';
    twWindow: result := 'Window';
    twLeft: result := 'Left';
    twRight: result := 'Right';
    twTop: result := 'Top';
    twBottom: result := 'Bottom';
  else
    result := 'Window';
  end;
end;

function StrToToolWindowDocking(AValue: string): TToolWindowDocking;
begin
  if CompareText(AValue,'None') = 0 then
    result := twNone
  else if CompareText(AValue,'Window') = 0 then
    result := twWindow
  else if CompareText(AValue,'Left') = 0 then
    result := twLeft
  else if CompareText(AValue,'Top') = 0 then
    result := twTop
  else if CompareText(AValue,'Right') = 0 then
    result := twRight
  else if CompareText(AValue,'Bottom') = 0 then
    result := twBottom
  else
    result := twWindow;
end;

{ TMainFormLayout }

constructor TMainFormLayout.Create(AForm: TForm; AZoom: TZoom);
begin
  FForm := AForm;
  FZoom := AZoom;
  FZoom.OnZoomChanged:=@ZoomChanged;
  FZoom.Layout := self;
  FPanelToolBox := TPanel.Create(nil);
  FPanelToolBox.BevelInner := bvNone;
  FPanelToolBox.BevelOuter := bvNone;
  FPanelToolBox.Width := 20;
  FPanelToolBox.Visible := false;
  FPanelToolBox.Cursor := crArrow;
  FDockedToolBoxToolBar := TToolBar.Create(FPanelToolBox);
  FDockedToolBoxToolBar.Align := alClient;
  FDockedToolBoxToolBar.EdgeBorders := [ebLeft,ebRight];
  FDockedToolBoxToolBar.Indent := 0;
  FDockedToolBoxToolBar.EdgeInner := esNone;
  FDockedToolBoxToolBar.EdgeOuter := esNone;
  FDockedToolBoxToolBar.ShowHint := true;
  FDockedToolBoxToolBar.Cursor := crArrow;
  FPanelToolBox.InsertControl(FDockedToolBoxToolBar);
  FForm.InsertControl(FPanelToolBox);

  FPaletteToolbar := TPaletteToolbar.Create;
  FPaletteToolbar.DarkTheme:= DarkTheme;
  FPaletteToolbar.OnVisibilityChangedByUser:= @PaletteVisibilityChangedByUser;
  FPaletteToolbar.Container := FForm;

  FVerticalSplitter := TSplitter.Create(nil);
  FVerticalSplitter.Visible := False;
  FVerticalSplitter.Align := alNone;
  FVerticalSplitter.Width := DoScaleX(VerticalSplitterWidth, OriginalDPI);
  FVerticalSplitter.Anchors := [akRight,akTop,akBottom];
  FForm.InsertControl(FVerticalSplitter);

  FDockedControlsPanel := TPanel.Create(nil);
  FDockedControlsPanel.Visible := false;
  FDockedControlsPanel.Anchors:= [akRight,akTop,akBottom];
  FForm.InsertControl(FDockedControlsPanel);

  FStatusBar := TPanel.Create(nil);
  FStatusBar.Align := alNone;
  FStatusBar.Visible := false;
  FStatusBar.Anchors := [akLeft,akRight,akBottom];
  FStatusBar.BevelOuter:= bvNone;
  FStatusBar.BevelInner:= bvNone;
  FStatusBar.Height := DoScaleY(15, OriginalDPI);
  FStatusBar.Font.Height := -DoScaleY(12, OriginalDPI);
  FStatusBar.OnPaint:=@StatusBar_Paint;
  FStatusTextSplit := TStringList.Create;
  FForm.InsertControl(FStatusBar);

  FPaintBox := TOpaquePaintBox.Create(nil);
  FForm.InsertControl(FPaintBox,0);
  FImageView := nil;
  FArranging := false;

  ApplyTheme;
end;

destructor TMainFormLayout.Destroy;
begin
  FZoom.Layout := nil;
  if not FDockedControlsPanel.Visible then
    LazPaintInstance.Config.SetDefaultDockLayersAndColorsWidth(0);
  FreeAndNil(FImageView);
  FreeAndNil(FPaintBox);
  FreeAndNil(FDockedControlsPanel);
  FreeAndNil(FVerticalSplitter);
  FreeAndNil(FStatusBar);
  FreeAndNil(FStatusTextSplit);
  FreeAndNil(FPaletteToolbar);
  FreeAndNil(FPanelToolBox);
  FreeAndNil(FMenu);
  inherited Destroy;
end;

function TMainFormLayout.CatchToolKeyDown(var AKey: Word): boolean;
begin
  if Assigned(FImageView) then
    result := FImageView.CatchToolKeyDown(AKey)
    else result := false;
end;

function TMainFormLayout.CatchToolKeyUp(var AKey: Word): boolean;
begin
  if Assigned(FImageView) then
    result := FImageView.CatchToolKeyUp(AKey)
    else result := false;
end;

function TMainFormLayout.CatchToolKeyPress(var AKey: TUTF8Char): boolean;
begin
  if Assigned(FImageView) then
    result := FImageView.CatchToolKeyPress(AKey)
    else result := false;
end;

procedure TMainFormLayout.SetToolBoxDocking(AValue: TToolWindowDocking);
begin
  if FInSetToolBoxDocking or (FToolBoxDocking=AValue) then Exit;
  FInSetToolBoxDocking := true;
  FToolBoxDocking:=AValue;
  if Assigned(FLazPaintInstance) then
  begin
    FLazPaintInstance.ToolboxVisible := AValue <> twNone;
    if AValue <> twNone then
      FLazPaintInstance.Config.SetDefaultToolboxDocking(ToolWindowDockingToStr(AValue));
  end;
  DoArrange;
  FInSetToolBoxDocking := false;
end;

function TMainFormLayout.GetToolBoxVisible: boolean;
begin
  result := LazPaintInstance.ToolboxVisible;
end;

function TMainFormLayout.GetUpdatingPopup: boolean;
begin
  if Assigned(FImageView) then
    result := FImageView.UpdatingPopup
    else result := False;
end;

procedure TMainFormLayout.ImageToolbarUpdate(Sender: TObject);
begin
  if Assigned(FOnToolbarUpdate) then
    FOnToolbarUpdate(self);
end;

procedure TMainFormLayout.ImageViewMouseBefore(ASender: TObject;
  AShift: TShiftState);
begin
  if Assigned(FOnPictureMouseBefore) then
    FOnPictureMouseBefore(self, AShift);
end;

procedure TMainFormLayout.ImageViewMouseMove(Sender: TObject; APosition: TPointF);
begin
  if Assigned(FOnPictureMouseMove) then
    FOnPictureMouseMove(self, APosition);
end;

procedure TMainFormLayout.ImageViewOnPaint(Sender: TObject);
begin
  if Assigned(FOnPaintPicture) then
    FOnPaintPicture(self);
end;

procedure TMainFormLayout.SetFillSelectionHighlight(AValue: boolean);
begin
  if Assigned(FImageView) then
    FImageView.FillSelectionHighlight := AValue;
end;

procedure TMainFormLayout.SetOnPaintPicture(AValue: TNotifyEvent);
begin
  if FOnPaintPicture=AValue then Exit;
  FOnPaintPicture:=AValue;
end;

procedure TMainFormLayout.SetUpdatingPopup(AValue: boolean);
begin
  if Assigned(FImageView) then
    FImageView.UpdatingPopup := AValue;
end;

procedure TMainFormLayout.StatusBar_Paint(Sender: TObject);
var
  colWidth, spacing, i, x: Integer;
begin
  if FStatusTextSplit.Count > 0 then
  begin
    FStatusBar.Canvas.Brush.Style := bsClear;
    spacing := DoScaleX(6, OriginalDPI);
    colWidth := (FStatusBar.ClientWidth - spacing*2) div FStatusTextSplit.Count;
    if colWidth > spacing*2 then
    begin
      FStatusBar.Canvas.Pen.Color := DarkThemeInstance.GetColorPanelHighlight(DarkTheme);
      FStatusBar.Canvas.Line(0,0, FStatusBar.Width,0);
      x := 0;
      for i := 0 to FStatusTextSplit.Count-1 do
      begin
        FStatusBar.Canvas.Font := FStatusBar.Font;
        FStatusBar.Canvas.Font.Color := DarkThemeInstance.GetColorButtonText(DarkTheme);
        FStatusBar.Canvas.TextOut(x + spacing,
          (FStatusBar.ClientHeight - FStatusBar.Canvas.TextHeight('Hg')) div 2,
          FStatusTextSplit[i]);

        if i > 0 then
        begin
          FStatusBar.Canvas.Pen.Color := DarkThemeInstance.GetColorPanelShadow(DarkTheme);
          FStatusBar.Canvas.Line(x-1,0, x-1,FStatusBar.Height);
          FStatusBar.Canvas.Pen.Color := DarkThemeInstance.GetColorPanelHighlight(DarkTheme);
          FStatusBar.Canvas.Line(x,0, x,FStatusBar.Height);
        end;

        inc(x, max(colWidth, FStatusBar.Canvas.TextWidth(FStatusTextSplit[i]) + 2*spacing));
      end;
    end;
  end;
end;

procedure TMainFormLayout.ToolboxGroupMainButton_MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  button: TToolButton;
begin
  button := Sender as TToolButton;
  ShowToolboxGroup(button.Tag);
end;

procedure TMainFormLayout.SetDarkTheme(AValue: boolean);
begin
  if FDarkTheme=AValue then Exit;
  FDarkTheme:=AValue;
  ApplyTheme;
  if Assigned(FPaletteToolbar) then
    FPaletteToolbar.DarkTheme:= AValue;
  if Assigned(FMenu) then
    FMenu.DarkTheme:= AValue;
  InvalidatePicture(true);
end;

procedure TMainFormLayout.SetLazPaintInstance(AValue: TLazPaintCustomInstance);
begin
  if FLazPaintInstance=AValue then Exit;
  FLazPaintInstance:=AValue;
  FPaletteToolbar.LazPaintInstance:= AValue;
  FStatusBarVisible := LazPaintInstance.Config.GetStatusBarVisible;
  if Assigned(FImageView) then FreeAndNil(FImageView);
  FImageView := TImageView.Create(FLazPaintInstance, FZoom, FPaintBox);
  FImageView.OnPaint:=@ImageViewOnPaint;
  FImageView.OnToolbarUpdate:=@ImageToolbarUpdate;
  FImageView.OnMouseMove:=@ImageViewMouseMove;
  FImageView.OnMouseBefore:=@ImageViewMouseBefore;
end;

procedure TMainFormLayout.SetPaletteVisible(AValue: boolean);
begin
  if FPaletteToolbar.Visible=AValue then Exit;
  FPaletteToolbar.Visible:=AValue;
  Arrange;
end;

function TMainFormLayout.GetPopupToolbox: TPopupMenu;
begin
  result := FPanelToolBox.PopupMenu;
end;

function TMainFormLayout.GetStatusBarVisible: boolean;
begin
  result := FStatusBarVisible;
end;

function TMainFormLayout.GetStatusText: string;
begin
  result := FStatusText;
end;

procedure TMainFormLayout.PaletteVisibilityChangedByUser(Sender: TObject);
begin
  Arrange;
end;

procedure TMainFormLayout.VerticalSplitterMoved(Sender: TObject);
begin
  LazPaintInstance.Config.SetDefaultDockLayersAndColorsWidth(
    FDockedControlsPanel.Width - DockedControlsPanelInnerMargin);
  if Assigned(FImageView) then
    FImageView.InvalidatePicture(true);
end;

procedure TMainFormLayout.ZoomChanged(sender: TZoom; ANewZoom: single);
begin
  if Assigned(FImageView) then
  begin
    if not LazPaintInstance.Image.SelectionMaskEmpty then
      FImageView.ShowSelection := false;
    FImageView.OnZoomChanged(sender, ANewZoom);
  end;
  PaintPictureNow;
end;

function TMainFormLayout.GetPaletteVisible: boolean;
begin
  result := FPaletteToolbar.Visible;
end;

function TMainFormLayout.GetFillSelectionHighlight: boolean;
begin
  if Assigned(FImageView) then
    result := FillSelectionHighlight
    else result := false;
end;

function TMainFormLayout.GetMouseButtonState: TShiftState;
begin
  if Assigned(FImageView) then
    result := FImageView.MouseButtonState
    else result := [];
end;

procedure TMainFormLayout.SetPopupToolbox(AValue: TPopupMenu);
begin
  FPanelToolBox.PopupMenu := AValue;
  if LazPaintInstance <> nil then
    LazPaintInstance.ToolboxWindowPopup := AValue;
end;

procedure TMainFormLayout.SetStatusBarVisible(AValue: boolean);
begin
  FStatusBarVisible := AValue;
  LazPaintInstance.Config.SetStatusBarVisible(AValue);
  Arrange;
end;

procedure TMainFormLayout.SetStatusText(AValue: string);
begin
  if AValue = FStatusText then exit;
  FStatusText := AValue;
  FStatusTextSplit.Delimiter:= '|';
  FStatusTextSplit.StrictDelimiter:= true;
  FStatusTextSplit.DelimitedText := FStatusText;
  FStatusBar.Invalidate;
end;

procedure TMainFormLayout.SetToolBoxVisible(AValue: boolean);
begin
  if AValue then
    ToolBoxDocking:= DefaultToolBoxDocking
  else
    ToolBoxDocking := twNone;
end;

function TMainFormLayout.GetDefaultToolboxDocking: TToolWindowDocking;
begin
  result := StrToToolWindowDocking(FLazPaintInstance.Config.DefaultToolboxDocking);
end;

procedure TMainFormLayout.ToolboxGroupButton_Click(Sender: TObject);
var
  button: TToolButton;
  tb: TToolBar;
  groupIndex: integer;
begin
  button := Sender as TToolButton;
  tb := button.Parent as TToolBar;
  groupIndex := tb.Tag;
  FDockedToolboxGroup[groupIndex].Button.Action := button.Action;
end;

procedure TMainFormLayout.ToolboxGroupTimer_Timer(Sender: TObject);
var
  groupIdx: integer;
begin
  groupIdx := (Sender as TTimer).Tag;
  HideToolboxGroup(groupIdx);
end;

procedure TMainFormLayout.ToolboxGroupToolbarButton_MouseEnter(Sender: TObject);
var
  button: TToolButton;
  groupIndex: integer;
  tb: TToolBar;
begin
  button := Sender as TToolButton;
  tb := button.Parent as TToolbar;
  groupIndex := tb.Tag;
  FDockedToolboxGroup[groupIndex].Timer.Enabled := false;
end;

procedure TMainFormLayout.ToolboxGroupToolbarButton_MouseLeave(Sender: TObject);
var
  button: TToolButton;
  groupIndex: integer;
  tb: TToolBar;
begin
  button := Sender as TToolButton;
  tb := button.Parent as TToolbar;
  groupIndex := tb.Tag;
  FDockedToolboxGroup[groupIndex].Timer.Enabled := true;
end;

procedure TMainFormLayout.ToolboxSimpleButton_Click(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to high(FDockedToolboxGroup) do
  begin
    FDockedToolboxGroup[i].Panel.Visible:= false;
    FDockedToolboxGroup[i].Timer.Enabled:= false;
  end;
end;

function TMainFormLayout.GetWorkArea: TRect;
begin
  result := GetWorkAreaAt(high(TLayoutStage));
end;

function TMainFormLayout.GetWorkAreaAt(AStage: TLayoutStage): TRect;
begin
  result := Rect(0,0,FForm.ClientWidth,FForm.ClientHeight);

  if Assigned(FMenu) then
    result.top += FMenu.ToolbarsHeight;
  if AStage = lsAfterTopToolbar then exit;

  if FToolBoxDocking = twLeft then result.Left += FPanelToolBox.Width else
  if FToolBoxDocking = twRight then result.Right -= FPanelToolBox.Width;
  if AStage = lsAfterDockedToolBox then exit;

  if PaletteVisible then result.Right -= FPaletteToolbar.Width;
  if AStage = lsAfterPaletteToolbar then exit;

  if VerticalSplitterNeeded then result.Right -= DockedControlsPanelWidth;
  if AStage = lsAfterDockedControlsPanel then exit;

  if (AStage = lsAfterVerticalSplitter) and VerticalSplitterNeeded then
    result.Right -= DoScaleX(VerticalSplitterWidth, OriginalDPI);
  if AStage = lsAfterVerticalSplitter then exit;

  if StatusBarVisible then result.Bottom -= FStatusBar.Height;
  if AStage = lsAfterStatusBar then exit;
end;

function TMainFormLayout.VerticalSplitterNeeded: boolean;
begin
  result := FDockedControlsPanel.ControlCount > 0;
end;

procedure TMainFormLayout.DoArrange;
var nbY,nbX,w,i: integer;
  updateChooseColorHeight: Boolean;
begin
  if Assigned(FMenu) then
    FMenu.ArrangeToolbars(FForm.ClientWidth);
  if FToolBoxDocking in [twLeft,twRight] then
  begin
    with GetWorkAreaAt(lsAfterTopToolbar) do
    begin
      if FToolBoxDocking = twLeft then FDockedToolBoxToolBar.Align:= alLeft
      else FDockedToolBoxToolBar.Align:= alRight;
      FPanelToolBox.Top := top;
      FPanelToolBox.Height:= bottom-top;
      nbY := FPanelToolBox.ClientHeight div FDockedToolBoxToolBar.ButtonHeight;
      if nbY = 0 then nbY := 1;
      nbX := (FDockedToolBoxToolBar.ButtonCount+nbY-1) div nbY;
      if nbX> 5 then nbX := 5;
      w := FDockedToolBoxToolBar.ButtonWidth * nbX+2;
      FDockedToolBoxToolBar.Width := w;
      FPanelToolBox.Width := w;
      if FToolBoxDocking = twLeft then
      begin
        FPanelToolBox.Left:= Left;
        FPanelToolBox.Anchors:= [akLeft,akTop,akBottom];
      end else
      begin
        FPanelToolBox.Left:= Right-FPanelToolBox.Width;
        FPanelToolBox.Anchors:= [akRight,akTop,akBottom];
      end;
      for i := 0 to FDockedToolBoxToolBar.ButtonCount-1 do
        FDockedToolBoxToolBar.Buttons[i].Top := i*FDockedToolBoxToolBar.ButtonHeight;
    end;
    if not FPanelToolBox.Visible then
    begin
      FPanelToolBox.Visible := true;
      FPanelToolBox.SendToBack;
    end;
  end else
    FPanelToolBox.Visible := false;
  if PaletteVisible then
    with GetWorkAreaAt(lsAfterDockedToolBox) do
      FPaletteToolbar.SetBounds(Right - FPaletteToolbar.Width,Top,FPaletteToolbar.Width,Bottom-Top);

  //temporarily disable controlside anchors
  FVerticalSplitter.OnMoved:= nil;
  FDockedControlsPanel.AnchorSideLeft.Control := nil;
  FDockedControlsPanel.Anchors := FDockedControlsPanel.Anchors - [akLeft];
  FPaintBox.AnchorSideRight.Control := nil;
  FPaintBox.Anchors := FPaintBox.Anchors - [akRight];
  FStatusBar.AnchorSideRight.Control := nil;

  if FDockedControlsPanel.ControlCount > 0 then
  begin
    with GetWorkAreaAt(lsAfterPaletteToolbar) do
    begin
      w := DockedControlsPanelWidth;
      updateChooseColorHeight := (FDockedControlsPanel.Width <> w) or not FDockedControlsPanel.Visible;
      FDockedControlsPanel.SetBounds(Right - w, Top, w, Bottom - Top);
      for i := 0 to FDockedControlsPanel.ControlCount-1 do
        if FDockedControlsPanel.Controls[i].Name = 'ChooseColorControl' then
        begin
          FDockedControlsPanel.Controls[i].Width := w - DockedControlsPanelInnerMargin;
          if updateChooseColorHeight then
            LazPaintInstance.AdjustChooseColorHeight;
        end;
    end;
    if not FDockedControlsPanel.Visible then
    begin
      FDockedControlsPanel.Visible:= true;
      FDockedControlsPanel.SendToBack;
    end;
  end else
    FDockedControlsPanel.Visible:= false;
  if StatusBarVisible then
  begin
    with GetWorkAreaAt(lsAfterVerticalSplitter) do
      FStatusBar.SetBounds(Left,Bottom-FStatusBar.Height,Right-Left,FStatusBar.Height);
    if not FStatusBar.Visible then
    begin
      FStatusBar.Visible := true;
      FStatusBar.SendToBack;
    end;
  end
  else FStatusBar.Visible := false;
  if Assigned(FImageView) then
    with WorkArea do
    FImageView.SetBounds(Left, Top, Width, Height);

  // enable controlside anchors
  if VerticalSplitterNeeded then
  begin
    FVerticalSplitter.Left := FDockedControlsPanel.Left - FVerticalSplitter.Width;
    FVerticalSplitter.Top := FDockedControlsPanel.Top;
    FVerticalSplitter.Height := FDockedControlsPanel.Height;
    FDockedControlsPanel.AnchorSideLeft.Side := asrRight;
    FDockedControlsPanel.AnchorSideLeft.Control := FVerticalSplitter;
    FDockedControlsPanel.Anchors := FDockedControlsPanel.Anchors + [akLeft];
    FPaintBox.AnchorSideRight.Side := asrLeft;
    FPaintBox.AnchorSideRight.Control := FVerticalSplitter;
    FPaintBox.Anchors := FPaintBox.Anchors + [akRight];
    FStatusBar.AnchorSideRight.Side := asrLeft;
    FStatusBar.AnchorSideRight.Control := FVerticalSplitter;
    if not FVerticalSplitter.Visible then
    begin
      FVerticalSplitter.Visible := true;
      FVerticalSplitter.SendToBack;
    end;
    FVerticalSplitter.OnMoved:=@VerticalSplitterMoved;
  end else
    FVerticalSplitter.Visible := false;
end;

procedure TMainFormLayout.ApplyTheme;
var
  bevelOfs, newSpacing, i: Integer;
begin
  FPanelToolBox.Color := DarkThemeInstance.GetColorButtonFace(DarkTheme);
  FStatusBar.Color:= DarkThemeInstance.GetColorButtonFace(DarkTheme);
  if DarkTheme then
  begin
    FDockedToolBoxToolBar.EdgeInner := esNone;
    FDockedToolBoxToolBar.EdgeOuter := esNone;
    FDockedToolBoxToolBar.OnPaint := @DarkThemeInstance.ToolBarPaintDark;
    FDockedToolBoxToolBar.OnPaintButton:= @DarkThemeInstance.ToolBarPaintButtonDark;
  end
  else
  begin
    FDockedToolBoxToolBar.EdgeInner := esRaised;
    FDockedToolBoxToolBar.EdgeOuter := esNone;
    if DarkThemeInstance.IsLclDarkTheme then
    begin
      FDockedToolBoxToolBar.OnPaint := @DarkThemeInstance.ToolBarPaintLight;
      FDockedToolBoxToolBar.OnPaintButton:= @DarkThemeInstance.ToolBarPaintButtonLight;
    end else
    begin
      FDockedToolBoxToolBar.OnPaint := nil;
      FDockedToolBoxToolBar.OnPaintButton:= nil;
    end;
  end;
  DarkThemeInstance.Apply(FDockedControlsPanel, DarkTheme, false);
  FVerticalSplitter.Color := DarkThemeInstance.GetColorButtonFace(DarkTheme);
  FDockedControlsPanel.Color := DarkThemeInstance.GetColorForm(DarkTheme);
  bevelOfs := integer(FDockedControlsPanel.BevelOuter <> bvNone)*FDockedControlsPanel.BevelWidth;
  newSpacing := DoScaleX(2, OriginalDPI) - bevelOfs;
  FDockedControlsPanel.ChildSizing.LeftRightSpacing:= newSpacing;
  FDockedControlsPanel.ChildSizing.TopBottomSpacing:= newSpacing;
  for i := 0 to High(FDockedToolboxGroup) do
    ApplyThemeToDockedToolboxGroup(i);
end;

function TMainFormLayout.DockedControlsPanelWidth: integer;
var
  w, i: Integer;
begin
  w := 0;
  if FDockedControlsPanel.ControlCount > 0 then
  begin
    w := LazPaintInstance.Config.DefaultDockLayersAndColorsWidth;
    if w = 0 then
    begin
      for i := 0 to FDockedControlsPanel.ControlCount-1 do
        w := max(w, FDockedControlsPanel.Controls[i].Tag);
    end;
    w := min(w, GetWorkAreaAt(lsAfterPaletteToolbar).Width * 3 div 4);
    inc(w, DockedControlsPanelInnerMargin);
  end;
  result := w;
end;

function TMainFormLayout.DockedControlsPanelInnerMargin: integer;
var
  bevelOfs: Integer;
begin
  bevelOfs := integer(FDockedControlsPanel.BevelOuter <> bvNone)*FDockedControlsPanel.BevelWidth;
  result := (FDockedControlsPanel.ChildSizing.LeftRightSpacing + bevelOfs)*2;
end;

procedure TMainFormLayout.ApplyThemeToDockedToolboxGroup(AGroupIndex: integer);
var
  panel: TPanel;
  spacing: Integer;
begin
  panel := FDockedToolboxGroup[AGroupIndex].panel;
  DarkThemeInstance.Apply(panel, DarkTheme);
  spacing := DoScaleX(2, OriginalDPI) - integer(panel.BevelOuter <> bvNone)*panel.BevelWidth;
  panel.ChildSizing.LeftRightSpacing:= spacing;
  panel.ChildSizing.TopBottomSpacing:= spacing;
end;

procedure TMainFormLayout.ShowToolboxGroup(AGroupIndex: integer);
var
  panel: TPanel;
  button: TToolButton;
  tb: TToolBar;
  i: Integer;
begin
  for i := 0 to high(FDockedToolboxGroup) do
    if i <> AGroupIndex then
    begin
      FDockedToolboxGroup[i].Panel.Visible:= false;
      FDockedToolboxGroup[i].Timer.Enabled:= false;
    end;
  button := FDockedToolboxGroup[AGroupIndex].Button;
  tb := FDockedToolboxGroup[AGroupIndex].Toolbar;
  panel := FDockedToolboxGroup[AGroupIndex].Panel;
  panel.Left := button.Left + button.Width + tb.Left + FPanelToolBox.Left;
  panel.Top := button.Top + tb.Top + FPanelToolBox.Top - DoScaleX(4, OriginalDPI);
  panel.Visible := true;
  FDockedToolboxGroup[AGroupIndex].Timer.Enabled:= false;
  FDockedToolboxGroup[AGroupIndex].Timer.Enabled:= true;
end;

procedure TMainFormLayout.HideToolboxGroup(AGroupIndex: integer);
begin
  FDockedToolboxGroup[AGroupIndex].Panel.Visible:= false;
  FDockedToolboxGroup[AGroupIndex].Timer.Enabled := false;
end;

procedure TMainFormLayout.HideToolboxGroups;
var
  i: Integer;
begin
  for i := 0 to High(FDockedToolboxGroup) do
    HideToolboxGroup(i);
end;

procedure TMainFormLayout.Arrange;
begin
  if FArranging then exit;
  FArranging := true;
  try
    DoArrange;
    HideToolboxGroups;
    if Assigned(FMenu) then
      FMenu.RepaintToolbar;
    if Assigned(FPaletteToolbar) then
      FPaletteToolbar.Arrange;
  finally
    FArranging := false;
  end;
end;

procedure TMainFormLayout.FocusImage;
begin
  if Assigned(FPaintBox) then SafeSetFocus(FPaintBox);
end;

procedure TMainFormLayout.CheckDelayedUpdate;
begin
  if (LazPaintInstance = nil) or (FImageView = nil) then exit;

  if FImageView.CanCompressOrUpdateStack and LazPaintInstance.UpdateStackOnTimer then
  begin
    LazPaintInstance.NotifyStackChange;
    LazPaintInstance.UpdateStackOnTimer := false;
  end else
  begin
    if FImageView.CanCompressOrUpdateStack then LazPaintInstance.Image.CompressUndo;
  end;

  if DelayedPaintPicture or LazPaintInstance.ToolManager.ToolUpdateNeeded or
   (not FImageView.ShowSelection and
    (Now > FImageView.LastPaintDate+SelectionPaintDelay) ) then
  begin
    if LazPaintInstance.ToolManager.ToolUpdateNeeded then LazPaintInstance.ToolManager.ToolUpdate;
    if Assigned(FImageView) then FImageView.ShowSelection := true;
    PaintPictureNow;
  end;
end;

procedure TMainFormLayout.DockedToolBoxAddButton(AAction: TBasicAction);
var button: TToolButton;
begin
  button := TToolButton.Create(FDockedToolBoxToolBar);
  button.Parent := FDockedToolBoxToolBar;
  button.Action := AAction;
  button.Style := tbsButton;
  button.OnClick:=@ToolboxSimpleButton_Click;
end;

procedure TMainFormLayout.DockedToolBoxAddGroup(AActions: array of TBasicAction);
var button: TToolButton;
  panel: TPanel;
  tb: TToolBar;
  i, groupIdx: Integer;
  timer: TTimer;
begin
  if length(AActions) = 0 then exit else
  if length(AActions) = 1 then DockedToolBoxAddButton(AActions[0]) else
  begin
    groupIdx := Length(FDockedToolboxGroup);
    setlength(FDockedToolboxGroup, length(FDockedToolboxGroup)+1);

    button := TToolButton.Create(FDockedToolBoxToolBar);
    button.Parent := FDockedToolBoxToolBar;
    button.Action := AActions[0];
    button.Style := tbsButton;
    button.OnMouseMove:=@ToolboxGroupMainButton_MouseMove;
    button.Tag := groupIdx;
    FDockedToolboxGroup[groupIdx].Button := button;

    panel := TPanel.Create(FForm);
    panel.Visible:= false;
    panel.AutoSize:= true;
    panel.Parent := FForm;
    FDockedToolboxGroup[groupIdx].Panel := panel;

    tb := TToolBar.Create(panel);
    tb.Tag := groupIdx;
    tb.EdgeBorders:= [];
    tb.AutoSize:= true;
    tb.Parent := panel;
    tb.Images := FDockedToolBoxToolBar.Images;
    tb.ButtonWidth := FDockedToolBoxToolBar.ButtonWidth;
    tb.ButtonHeight := FDockedToolBoxToolBar.ButtonHeight;
    FDockedToolboxGroup[groupIdx].Toolbar := tb;

    ApplyThemeToDockedToolboxGroup(groupIdx);

    for i := 0 to high(AActions) do
    begin
      button := TToolButton.Create(tb);
      button.Parent := tb;
      button.Action := AActions[i];
      button.Style := tbsButton;
      button.OnClick:= @ToolboxGroupButton_Click;
      button.OnMouseEnter:=@ToolboxGroupToolbarButton_MouseEnter;
      button.OnMouseLeave:=@ToolboxGroupToolbarButton_MouseLeave;
    end;

    timer := TTimer.Create(FForm);
    timer.Tag := groupIdx;
    timer.Interval:= 2000;
    timer.Enabled:= false;
    timer.OnTimer:=@ToolboxGroupTimer_Timer;
    FDockedToolboxGroup[groupIdx].Timer := timer;
  end;
end;

procedure TMainFormLayout.DockedToolBoxSetImages(AImages: TImageList);
var
  i: Integer;
begin
  FDockedToolBoxToolBar.Images := AImages;
  FDockedToolBoxToolBar.ButtonWidth := Max(AImages.Width+4, 23);
  FDockedToolBoxToolBar.ButtonHeight := Max(AImages.Height+4, 22);
  for i := 0 to high(FDockedToolboxGroup) do
  begin
    FDockedToolboxGroup[i].Toolbar.Images := AImages;
    FDockedToolboxGroup[i].Toolbar.ButtonWidth := FDockedToolBoxToolBar.ButtonWidth;
    FDockedToolboxGroup[i].Toolbar.ButtonHeight := FDockedToolBoxToolBar.ButtonHeight;
  end;
end;

procedure TMainFormLayout.AddColorToPalette(AColor: TBGRAPixel);
begin
  FPaletteToolbar.AddColor(AColor);
end;

procedure TMainFormLayout.RemoveColorFromPalette(AColor: TBGRAPixel);
begin
  FPaletteToolbar.RemoveColor(AColor);
end;

procedure TMainFormLayout.AddDockedControl(AControl: TControl);
begin
  if not FDockedControlsPanel.ContainsControl(AControl) then
  begin
    AControl.Tag := AControl.Width;
    if AControl.Name = 'ChooseColorControl' then
      AControl.Align:= alTop
    else
      AControl.Align:= alClient;
    FDockedControlsPanel.InsertControl(AControl);

    if (AControl.Name = 'ChooseColorControl') and not Assigned(FDockedChooseColorSplitter) then
    begin
      FDockedChooseColorSplitter := TSplitter.Create(FDockedControlsPanel);
      FDockedChooseColorSplitter.Align := alTop;
      FDockedChooseColorSplitter.Height := DoScaleY(8, OriginalDPI);
      FDockedChooseColorSplitter.MinSize:= DoScaleY(85, OriginalDPI);
      FDockedChooseColorSplitter.Top := AControl.Height;
      FDockedControlsPanel.InsertControl(FDockedChooseColorSplitter);
    end;
  end;
end;

procedure TMainFormLayout.RemoveDockedControl(AControl: TControl);
begin
  if FDockedControlsPanel.ContainsControl(AControl) then
  begin
    if (AControl.Name = 'ChooseColorControl') and Assigned(FDockedChooseColorSplitter) then
    begin
      FDockedControlsPanel.RemoveControl(FDockedChooseColorSplitter);
      FreeAndNil(FDockedChooseColorSplitter);
    end;
    FDockedControlsPanel.RemoveControl(AControl);
    AControl.Align:= alNone;
  end;
end;

procedure TMainFormLayout.PaintPictureNow;
begin
  if Assigned(FImageView) then
    FImageView.UpdatePicture;
end;

procedure TMainFormLayout.InvalidatePicture(AInvalidateAll: boolean);
begin
  if Assigned(FImageView) then
    FImageView.InvalidatePicture(AInvalidateAll);
end;

procedure TMainFormLayout.ShowNoPicture;
begin
  if Assigned(FImageView) then
    FImageView.ShowNoPicture;
end;

end.

