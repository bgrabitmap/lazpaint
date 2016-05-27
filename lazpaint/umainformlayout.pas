unit UMainFormLayout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UMenu, Forms, LazPaintType, UZoom, ExtCtrls, ComCtrls,
  Menus, UPaletteToolbar, BGRABitmapTypes, Controls;

type
  TOnPictureAreaChange = procedure(ASender: TObject; ANewArea: TRect) of object;
  TToolWindowDocking = (twNone, twWindow, twLeft, twTop, twRight, twBottom);
  TLayoutStage = (lsAfterTopToolbar, lsAfterDockedToolBox, lsAfterPaletteToolbar,
               lsAfterStatusBar);

  { TMainFormLayout }

  TMainFormLayout = class(TCustomMainFormLayout)
    procedure PaletteVisibilityChangedByUser(Sender: TObject);
  private
    FForm: TForm;
    FMenu: TMainFormMenu;
    FLazPaintInstance: TLazPaintCustomInstance;
    FOnPictureAreaChange: TOnPictureAreaChange;
    FToolBoxDocking: TToolWindowDocking;
    FInSetToolBoxDocking: boolean;
    FPanelToolBox: TPanel;
    FDockedToolBoxToolBar: TToolBar;
    FPaletteToolbar: TPaletteToolbar;
    FStatusBarVisible: boolean;
    FStatusBar: TStatusBar;
    FStatusText: string;
    function GetPaletteVisible: boolean;
    function GetPopupToolbox: TPopupMenu;
    function GetStatusBarVisible: boolean;
    function GetStatusText: string;
    function GetToolBoxVisible: boolean;
    procedure SetLazPaintInstance(AValue: TLazPaintCustomInstance);
    procedure SetPaletteVisible(AValue: boolean);
    procedure SetPopupToolbox(AValue: TPopupMenu);
    procedure SetStatusBarVisible(AValue: boolean);
    procedure SetStatusText(AValue: string);
    procedure SetToolBoxDocking(AValue: TToolWindowDocking);
    procedure SetToolBoxVisible(AValue: boolean);
    function GetDefaultToolboxDocking: TToolWindowDocking;
  protected
    FLastPictureArea: TRect;
    function GetPictureArea: TRect; override;
    function GetPictureAreaAt(AStage: TLayoutStage): TRect;
    procedure RaisePictureAreaChange;
    procedure DoArrange;
  public
    constructor Create(AForm: TForm);
    destructor Destroy; override;
    procedure Arrange;
    procedure DockedToolBoxAddButton(AAction: TBasicAction);
    procedure DockedToolBoxSetImages(AImages: TImageList);
    procedure AddColorToPalette(AColor : TBGRAPixel);
    procedure RemoveColorFromPalette(AColor : TBGRAPixel);
    property Menu: TMainFormMenu read FMenu write FMenu;
    property ToolBoxDocking: TToolWindowDocking read FToolBoxDocking write SetToolBoxDocking;
    property ToolBoxVisible: boolean read GetToolBoxVisible write SetToolBoxVisible;
    property OnPictureAreaChange: TOnPictureAreaChange read FOnPictureAreaChange write FOnPictureAreaChange;
    property LazPaintInstance: TLazPaintCustomInstance read FLazPaintInstance write SetLazPaintInstance;
    property ToolboxPopup: TPopupMenu read GetPopupToolbox write SetPopupToolbox;
    property PaletteVisible: boolean read GetPaletteVisible write SetPaletteVisible;
    property StatusBarVisible: boolean read GetStatusBarVisible write SetStatusBarVisible;
    property StatusText: string read GetStatusText write SetStatusText;
    property DefaultToolboxDocking: TToolWindowDocking read GetDefaultToolboxDocking;
  end;

function ToolWindowDockingToStr(AValue: TToolWindowDocking): string;
function StrToToolWindowDocking(AValue: string): TToolWindowDocking;

implementation

uses Graphics, Toolwin, math;

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

constructor TMainFormLayout.Create(AForm: TForm);
begin
  FForm := AForm;
  FPanelToolBox := TPanel.Create(FForm);
  FPanelToolBox.Color := clBtnFace;
  FPanelToolBox.BevelInner := bvNone;
  FPanelToolBox.BevelOuter := bvNone;
  FPanelToolBox.Width := 20;
  FPanelToolBox.Visible := false;
  FPanelToolBox.Cursor := crArrow;
  FDockedToolBoxToolBar := TToolBar.Create(FPanelToolBox);
  FDockedToolBoxToolBar.Align := alClient;
  FDockedToolBoxToolBar.EdgeBorders := [ebLeft,ebRight];
  FDockedToolBoxToolBar.Indent := 0;
  FDockedToolBoxToolBar.EdgeInner := esRaised;
  FDockedToolBoxToolBar.EdgeOuter := esNone;
  FDockedToolBoxToolBar.ShowHint := true;
  FDockedToolBoxToolBar.Cursor := crArrow;
  FPanelToolBox.InsertControl(FDockedToolBoxToolBar);
  FForm.InsertControl(FPanelToolBox);
  FPaletteToolbar := TPaletteToolbar.Create;
  FPaletteToolbar.Container := FForm;
  FPaletteToolbar.OnVisibilityChangedByUser:=@PaletteVisibilityChangedByUser;
  FStatusBar := TStatusBar.Create(FForm);
  FStatusBar.SizeGrip := false;
  FStatusBar.Align := alNone;
  FStatusBar.Visible := false;
  FForm.InsertControl(FStatusBar);
end;

destructor TMainFormLayout.Destroy;
begin
  FreeAndNil(FStatusBar);
  FreeAndNil(FPaletteToolbar);
  FForm.RemoveControl(FPanelToolBox);
  FreeAndNil(FPanelToolBox);
  FreeAndNil(FMenu);
  inherited Destroy;
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
  RaisePictureAreaChange;
  FInSetToolBoxDocking := false;
end;

function TMainFormLayout.GetToolBoxVisible: boolean;
begin
  result := LazPaintInstance.ToolboxVisible;
end;

procedure TMainFormLayout.SetLazPaintInstance(AValue: TLazPaintCustomInstance);
begin
  if FLazPaintInstance=AValue then Exit;
  FLazPaintInstance:=AValue;
  FPaletteToolbar.LazPaintInstance:= AValue;
  FStatusBarVisible := LazPaintInstance.Config.GetStatusBarVisible;
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
  result := FStatusBar.SimpleText;
end;

procedure TMainFormLayout.PaletteVisibilityChangedByUser(Sender: TObject);
begin
  Arrange;
end;

function TMainFormLayout.GetPaletteVisible: boolean;
begin
  result := FPaletteToolbar.Visible;
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
var elems: TStringList;
  i,w: Integer;
  idxDelim: integer;
begin
  if AValue = FStatusText then exit;
  FStatusText := AValue;
  if pos('|',AValue) = 0 then
  begin
    if FStatusBar.SimplePanel <> true then
      FStatusBar.SimplePanel := true;
    FStatusBar.SimpleText := AValue;
  end else
  begin
    elems := TStringList.Create;
    repeat
      idxDelim := pos('|',AValue);
      if idxDelim = 0 then
      begin
        elems.Add(AValue);
        break;
      end;
      elems.Add(copy(AValue,1,idxDelim-1));
      AValue := copy(AValue,idxDelim+1,length(AValue)-idxDelim);
    until false;
    if FStatusBar.SimplePanel <> false then
      FStatusBar.SimplePanel := false;
    while FStatusBar.Panels.Count < elems.Count do
      with FStatusBar.Panels.Add do
        Width := FStatusBar.Height*10;
    w := FStatusBar.ClientWidth div elems.Count;
    for i := 0 to elems.Count-1 do
      with FStatusBar.Panels[i] do
      begin
        Text := elems[i];
        Width := w;
      end;
    elems.Free;
  end;
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

function TMainFormLayout.GetPictureArea: TRect;
begin
  result := GetPictureAreaAt(high(TLayoutStage));
end;

function TMainFormLayout.GetPictureAreaAt(AStage: TLayoutStage): TRect;
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

  if StatusBarVisible then result.Bottom -= FStatusBar.Height;
  if AStage = lsAfterStatusBar then exit;
end;

procedure TMainFormLayout.RaisePictureAreaChange;
begin
  if Assigned(FOnPictureAreaChange) then
    FOnPictureAreaChange(self, PictureArea);
end;

procedure TMainFormLayout.DoArrange;
var nbY,nbX,w,i: integer;
begin
  FMenu.ArrangeToolbars(FForm.ClientWidth);
  if FToolBoxDocking in [twLeft,twRight] then
  begin
    with GetPictureAreaAt(lsAfterTopToolbar) do
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
      if FToolBoxDocking = twLeft then FPanelToolBox.Left:= Left
      else FPanelToolBox.Left:= Right-FPanelToolBox.Width;
    end;
    FPanelToolBox.Visible := true;
  end else
    FPanelToolBox.Visible := false;
  if PaletteVisible then
    with GetPictureAreaAt(lsAfterDockedToolBox) do
      FPaletteToolbar.SetBounds(Right - FPaletteToolbar.Width,Top,FPaletteToolbar.Width,Bottom-Top);
  if StatusBarVisible then
  begin
    with GetPictureAreaAt(lsAfterPaletteToolbar) do
      FStatusBar.SetBounds(Left,Bottom-FStatusBar.Height,Right-Left,FStatusBar.Height);
    if not FStatusBar.SimplePanel then
    begin
      w := FStatusBar.ClientWidth div FStatusBar.Panels.Count;
      for i := 0 to FStatusBar.Panels.Count-1 do
        FStatusBar.Panels[i].Width := w;
    end;
    FStatusBar.Visible := true;
  end
  else FStatusBar.Visible := false;
end;

procedure TMainFormLayout.Arrange;
var picAreaBeforeArrange,newPicArea: TRect;
begin
  picAreaBeforeArrange := PictureArea;
  DoArrange;
  newPicArea := PictureArea;
  if (newPicArea.Left <> picAreaBeforeArrange.Left) or
     (newPicArea.Top <> picAreaBeforeArrange.Top) or
     (newPicArea.Right <> picAreaBeforeArrange.Right) or
     (newPicArea.Bottom <> picAreaBeforeArrange.Bottom) or
     (newPicArea.Left <> FLastPictureArea.Left) or
     (newPicArea.Top <> FLastPictureArea.Top) or
     (newPicArea.Right <> FLastPictureArea.Right) or
     (newPicArea.Bottom <> FLastPictureArea.Bottom) then
  begin
    RaisePictureAreaChange;
    FLastPictureArea := newPicArea;
  end;
  FMenu.RepaintToolbar;
end;

procedure TMainFormLayout.DockedToolBoxAddButton(AAction: TBasicAction);
var button: TToolButton;
begin
  button := TToolButton.Create(FDockedToolBoxToolBar);
  button.Parent := FDockedToolBoxToolBar;
  button.Action := AAction;
  button.Style := tbsButton;
end;

procedure TMainFormLayout.DockedToolBoxSetImages(AImages: TImageList);
begin
  FDockedToolBoxToolBar.Images := AImages;
  FDockedToolBoxToolBar.ButtonWidth := Max(AImages.Width+4, 23);
  FDockedToolBoxToolBar.ButtonHeight := Max(AImages.Height+4, 22);
end;

procedure TMainFormLayout.AddColorToPalette(AColor: TBGRAPixel);
begin
  FPaletteToolbar.AddColor(AColor);
end;

procedure TMainFormLayout.RemoveColorFromPalette(AColor: TBGRAPixel);
begin
  FPaletteToolbar.RemoveColor(AColor);
end;

end.

