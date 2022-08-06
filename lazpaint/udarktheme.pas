// SPDX-License-Identifier: GPL-3.0-only
unit UDarkTheme;

{$mode objfpc}{$H+}
{$IF defined(DARWIN) and defined(CPU64)}{$DEFINE DARWIN_DARK_THEME}{$ENDIF}
{$IFDEF DARWIN_DARK_THEME}{$modeswitch objectivec1}{$ENDIF}

interface

uses
  Classes, SysUtils, Forms, ComCtrls, StdCtrls, Controls, ExtCtrls, Graphics,
  LazPaintType, BCButton, BCComboBox, BCTrackbarUpdown, LCVectorialFillControl;

type

  { TDarkTheme }

  TDarkTheme = class
  private
    FLastSystemDarkTheme: boolean;
  public
    constructor Create;
    procedure PanelPaint(Sender: TObject; ADarkTheme: boolean);
    procedure PanelPaintDark(Sender: TObject);
    procedure PanelPaintLight(Sender: TObject);
    procedure ToolBarPaint(Sender: TObject; ADarkTheme: boolean);
    procedure ToolBarPaintLight(Sender: TObject);
    procedure ToolBarPaintDark(Sender: TObject);
    procedure ToolBarPaintButton(Sender: TToolButton; State: integer; {%H-}ADarkTheme: boolean);
    procedure ToolBarPaintButtonLight(Sender: TToolButton; State: integer);
    procedure ToolBarPaintButtonDark(Sender: TToolButton; State: integer);
    procedure Apply(AForm: TForm; AThemeEnabled: boolean; ARecursive: boolean = true); overload;
    procedure Apply(APanel: TPanel; AThemeEnabled: boolean; ARecursive: boolean = true); overload;
    procedure Apply(AVectorialFill: TLCVectorialFillControl; AThemeEnabled: boolean); overload;
    procedure Apply(AToolbar: TToolbar; AThemeEnabled: boolean); overload;
    procedure Apply(AButton: TBCButton; ADarkTheme: boolean; AFontHeightRatio: single = 0.5); overload;
    procedure Apply(ACombo: TBCComboBox; ADarkTheme: boolean; AFontHeightRatio: single = 0.5); overload;
    procedure Apply(AUpDown: TBCTrackbarUpdown; ADarkTheme: boolean); overload;
    procedure Apply(ALabel: TLabel; ADarkTheme: boolean); overload;
    function IsSystemDarkTheme: boolean;
    function IsLclDarkTheme: boolean;
    function IsLclLightThemeSafe: boolean;
    function HasSystemDarkThemeChanged: boolean;
    function GetColorButtonHighlight(ADarkTheme: boolean): TColor;
    function GetColorButtonFace(ADarkTheme: boolean): TColor;
    function GetColorButtonText(ADarkTheme: boolean): TColor;
    function GetColorForm(ADarkTheme: boolean): TColor;
    function GetColorEditableFace(ADarkTheme: boolean): TColor;
    function GetColorEditableText(ADarkTheme: boolean): TColor;
    function GetColorPanelHighlight(ADarkTheme: boolean): TColor;
    function GetColorPanelShadow(ADarkTheme: boolean): TColor;
    function GetColorHighlightBack(ADarkTheme: boolean): TColor;
    function GetColorHighlightText(ADarkTheme: boolean): TColor;
  end;

var
  DarkThemeInstance: TDarkTheme;

implementation

uses
  BCTypes, BGRABitmap, BGRABitmapTypes, GraphType, BGRACustomDrawn, LCScaleDPI
  {$IFDEF DARWIN_DARK_THEME}, CocoaAll, CocoaUtils{$ENDIF}
  {$IFDEF WINDOWS}, Win32Proc, Registry{$ENDIF};

const
  clDarkBtnHighlight = $e0e0e0;
  clDarkBtnFace = $606060;
  clDarkEditableFace = $808080;
  clLightText = $f0f0f0;
  clDarkPanelHighlight = $909090;
  clDarkPanelShadow = $404040;

{$IFDEF DARWIN_DARK_THEME}
{ returns true, if this app runs on macOS 10.14 Mojave or newer }
function IsMojaveOrNewer: boolean;
var
  minOsVer: NSOperatingSystemVersion;
begin
  // Setup minimum version (Mojave)
  minOsVer.majorVersion:= 10;
  minOsVer.minorVersion:= 14;
  minOsVer.patchVersion:= 0;

  // Check minimum version
  if NSProcessInfo.ProcessInfo.isOperatingSystemAtLeastVersion(minOSVer) then
    Result := True
  else
    Result := False;
end;

function GetPrefString(const KeyName : string) : string;
begin
  Result := NSStringToString(NSUserDefaults.standardUserDefaults.stringForKey(NSStr(@KeyName[1])));
end;

function IsMojaveDarkTheme: boolean;
begin
  Result := pos('DARK',UpperCase(GetPrefString('AppleInterfaceStyle'))) > 0;
end;
{$ENDIF}

{$IFDEF WINDOWS}
type
  TWinDarkThemeMode = (dtmLight, dtmDark, dtmUnknown);

// by "jwdietrich" from Lazarus forum
// IsDarkTheme: Detects if the Dark Theme (true) has been enabled or not (false)
function GetWinDarkTheme: TWinDarkThemeMode;
const
  KEYPATH = '\Software\Microsoft\Windows\CurrentVersion\Themes\Personalize';
  KEYNAME = 'AppsUseLightTheme';
var
  Registry: TRegistry;
begin
  Result := dtmUnknown;
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_CURRENT_USER;
    if Registry.OpenKeyReadOnly(KEYPATH) then
      begin
        if Registry.ValueExists(KEYNAME) then
        begin
          if Registry.ReadBool(KEYNAME) then
            result := dtmLight
            else result := dtmDark;
        end;
      end;
  finally
    Registry.Free;
  end;
end;
{$ENDIF}

procedure BCAssignSystemState(AState: TBCButtonState; AFontColor, ATopColor, AMiddleTopColor, AMiddleBottomColor, ABottomColor, ABorderColor: TColor);
var middleColor: TColor;
begin
  with AState do
  begin
    Border.Style := bboSolid;
    Border.Color := ABorderColor;
    Border.ColorOpacity := 192;
    FontEx.Color := AFontColor;
    FontEx.Style := [];
    FontEx.Shadow := True;
    FontEx.ShadowColor := clBlack;
    FontEx.ShadowColorOpacity := 192;
    FontEx.ShadowOffsetX := 1;
    FontEx.ShadowOffsetY := 1;
    FontEx.ShadowRadius := 2;
    middleColor := MergeBGRA(AMiddleTopColor.ToExpandedPixel, AMiddleBottomColor.ToExpandedPixel);
    FontEx.DisabledColor := MergeBGRA(middleColor.ToExpandedPixel, AFontColor.ToExpandedPixel);
    Background.Gradient1EndPercent := 60;
    Background.Style := bbsGradient;
    // Gradient1
    with Background.Gradient1 do
    begin
      GradientType := gtLinear;
      StartColor := ATopColor;
      EndColor := AMiddleTopColor;
      Point1XPercent := 0;
      Point1YPercent := 0;
      Point2XPercent := 0;
      Point2YPercent := 100;
    end;
    // Gradient2
    with Background.Gradient2 do
    begin
      StartColor := AMiddleBottomColor;
      EndColor := ABottomColor;
      GradientType := gtLinear;
      Point1XPercent := 0;
      Point1YPercent := 0;
      Point2XPercent := 0;
      Point2YPercent := 100;
    end;
  end;
end;

{ TDarkTheme }

constructor TDarkTheme.Create;
begin
  FLastSystemDarkTheme := IsSystemDarkTheme;
end;

procedure TDarkTheme.PanelPaint(Sender: TObject; ADarkTheme: boolean);
var
  c: TCanvas;
begin
  if Sender is TCustomControl then
  begin
    c := TCustomControl(Sender).Canvas;
    c.Pen.Color := GetColorPanelHighlight(ADarkTheme);
    c.Line(0, 0, c.Width, 0);
    c.Line(0, 0, 0, c.Height);
    c.Pen.Color := GetColorPanelShadow(ADarkTheme);
    c.Line(0, c.Height-1, c.Width, c.Height-1);
    c.Line(c.Width-1, 0, c.Width-1, c.Height);
  end;
end;

procedure TDarkTheme.PanelPaintDark(Sender: TObject);
begin
  PanelPaint(Sender, true);
end;

procedure TDarkTheme.PanelPaintLight(Sender: TObject);
begin
  PanelPaint(Sender, false);
end;

procedure TDarkTheme.ToolBarPaint(Sender: TObject; ADarkTheme: boolean);
var
  T: TToolBar;
begin
  if Sender is TToolBar then
  begin
    T := TToolBar(Sender);
    if T.Align = alLeft then
    begin
      T.Canvas.Pen.Color := GetColorPanelShadow(ADarkTheme);
      T.Canvas.Line(T.Width-1, 0, T.Width-1, T.Height)
    end
    else if T.Align = alRight then
    begin
      T.Canvas.Pen.Color := GetColorPanelHighlight(ADarkTheme);
      T.Canvas.Line(0, 0, 0, T.Height)
    end
    else
    begin
      T.Canvas.Pen.Color := GetColorPanelShadow(ADarkTheme);
      T.Canvas.Line(0, 0, T.Width, 0);
    end;
  end;
end;

procedure TDarkTheme.ToolBarPaintLight(Sender: TObject);
begin
  ToolbarPaint(Sender, false);
end;

procedure TDarkTheme.ToolBarPaintDark(Sender: TObject);
begin
  ToolbarPaint(Sender, true);
end;

procedure TDarkTheme.ToolBarPaintButton(Sender: TToolButton; State: integer;
  ADarkTheme: boolean);
var
  Bitmap: TBGRABitmap;
  //ts: TSize;
  T: TToolBar;
  imgW, imgH: integer;
  imgS: TGraphicsDrawEffect;
begin
  Bitmap := nil;

  if Sender.Style in[tbsButton,tbsCheck] then
  begin
    if Sender.Enabled then
    begin
      if (State = 3) or Sender.Down then
      begin
        { Button Down }
        Bitmap := TBGRABitmap.Create(Sender.Width, Sender.Height);
        Bitmap.Rectangle(0, 0, Sender.Width, Sender.Height - 1,
          GetColorPanelShadow(ADarkTheme), dmSet);
        Bitmap.Rectangle(1, 1, Sender.Width - 1, Sender.Height - 2,
          MergeBGRA(ColorToBGRA(GetColorPanelShadow(ADarkTheme)), 2, ColorToBGRA(GetColorButtonFace(ADarkTheme)), 1),
          MergeBGRA(ColorToBGRA(GetColorPanelShadow(ADarkTheme)), 1, ColorToBGRA(GetColorButtonFace(ADarkTheme)), 2), dmSet);
        Bitmap.SetHorizLine(0, Sender.Height - 1, Sender.Width - 1,
          GetColorButtonFace(ADarkTheme));
      end
      else
      begin
        if State = 2 then
        begin
          { Button Hovered }
          Bitmap := TBGRABitmap.Create(Sender.Width, Sender.Height);
          Bitmap.GradientFill(0, 0, Sender.Width, Sender.Height, GetColorPanelHighlight(ADarkTheme),
            GetColorButtonFace(ADarkTheme), gtLinear, PointF(0, 0),
            PointF(0, Sender.Height), dmSet);
          Bitmap.Rectangle(0, 0, Sender.Width, Sender.Height - 1,
            GetColorPanelShadow(ADarkTheme),
            dmSet);
          Bitmap.SetHorizLine(1, 1, Sender.Width - 2,
            MergeBGRA(ColorToBGRA(GetColorPanelHighlight(ADarkTheme)), ColorToBGRA(GetColorButtonHighlight(ADarkTheme))));
          Bitmap.SetHorizLine(0, Sender.Height - 1, Sender.Width - 1,
            MergeBGRA(ColorToBGRA(GetColorPanelShadow(ADarkTheme)), ColorToBGRA(GetColorButtonFace(ADarkTheme))));
        end
        else
          { Button Normal }
          //Bitmap.Fill(BGRA(83, 83, 83));
      end;
    end
    else
    begin
      { Button Disabled }
      {Bitmap.Rectangle(0, 0, Sender.Width, Sender.Height - 1, BGRA(66, 66, 66),
        BGRA(71, 71, 71), dmSet);
      Bitmap.SetHorizLine(0, Sender.Height - 1, Sender.Width - 1, BGRA(83, 83, 83));}
    end;

    {Bitmap.FontName := Sender.Font.Name;
    Bitmap.FontStyle := Sender.Font.Style;
    Bitmap.FontHeight := Sender.Font.Height;
    Bitmap.FontQuality := fqSystemClearType;
    ts := Bitmap.TextSize(Sender.Caption);

    if Sender.Enabled then
    begin
      // Text Enabled
      Bitmap.TextOut((Sender.Width - ts.cx) div 2, ((Sender.Height - ts.cy) div 2) -
        1, Sender.Caption, BGRA(47, 47, 47));
      Bitmap.TextOut((Sender.Width - ts.cx) div 2, (Sender.Height - ts.cy) div 2,
        Sender.Caption, BGRA(229, 229, 229));
    end
    else
      // Text Disabled
      Bitmap.TextOut((Sender.Width - ts.cx) div 2, (Sender.Height - ts.cy) div 2,
        Sender.Caption, BGRA(170, 170, 170));}
  end;

  if Assigned(Bitmap) then
  begin
    Bitmap.Draw(Sender.Canvas, 0, 0, False);
    Bitmap.Free;
  end;

  if (Sender.Parent is TToolBar) and
     (Sender.Style in [tbsButton,tbsButtonDrop,tbsCheck]) then
  begin
    T := TToolBar(Sender.Parent);
    if Assigned(T.Images) then
    begin
      imgW := T.Images.Width;
      imgH := T.Images.Height;

      if Sender.Enabled then
        imgS := gdeNormal
      else
        imgS := gdeDisabled;

      T.Images.Draw(Sender.Canvas, (Sender.Width - imgW) div 2, (Sender.Height - imgH) div
        2, Sender.ImageIndex, imgS);
    end;
  end;
end;

procedure TDarkTheme.ToolBarPaintButtonLight(Sender: TToolButton; State: integer);
begin
  ToolBarPaintButton(Sender, State, false);
end;

procedure TDarkTheme.ToolBarPaintButtonDark(Sender: TToolButton; State: integer);
begin
  ToolBarPaintButton(Sender, State, true);
end;

procedure TDarkTheme.Apply(AForm: TForm; AThemeEnabled: boolean; ARecursive: boolean);
var
  i: Integer;
begin
  AForm.Color := GetColorButtonFace(AThemeEnabled);
  if ARecursive then
  for i := 0 to AForm.ControlCount-1 do
  begin
    if AForm.Controls[i] is TPanel then
      Apply(TPanel(AForm.Controls[i]), AThemeEnabled) else
    if AForm.Controls[i] is TToolBar then
      Apply(TToolBar(AForm.Controls[i]), AThemeEnabled) else
    if AForm.Controls[i] is TBCButton then
      Apply(TBCButton(AForm.Controls[i]), AThemeEnabled) else
    if AForm.Controls[i] is TLabel then
      Apply(TLabel(AForm.Controls[i]), AThemeEnabled);
  end;
end;

procedure TDarkTheme.Apply(APanel: TPanel; AThemeEnabled: boolean; ARecursive: boolean);
var
  i: Integer;
begin
  if AThemeEnabled then
  begin
    APanel.BevelOuter:= bvNone;
    if (APanel.OnPaint = nil) or (APanel.OnPaint = @PanelPaintLight) then
      APanel.OnPaint := @PanelPaintDark;
  end else
  begin
    if not IsLclLightThemeSafe then
    begin
      APanel.BevelOuter:= bvNone;
      APanel.OnPaint := @PanelPaintLight;
    end else
    begin
      APanel.BevelOuter:= bvRaised;
      if APanel.OnPaint = @PanelPaintDark then APanel.OnPaint := nil;
    end;
  end;
  APanel.Color := GetColorButtonFace(AThemeEnabled);
  if ARecursive then
  for i := 0 to APanel.ControlCount-1 do
  begin
    if APanel.Controls[i] is TPanel then
      Apply(TPanel(APanel.Controls[i]), AThemeEnabled) else
    if APanel.Controls[i] is TToolBar then
      Apply(TToolBar(APanel.Controls[i]), AThemeEnabled) else
    if APanel.Controls[i] is TBCButton then
      Apply(TBCButton(APanel.Controls[i]), AThemeEnabled) else
    if APanel.Controls[i] is TBCComboBox then
      Apply(TBCComboBox(APanel.Controls[i]), AThemeEnabled) else
    if APanel.Controls[i] is TBCTrackbarUpdown then
      Apply(TBCTrackbarUpdown(APanel.Controls[i]), AThemeEnabled) else
    if APanel.Controls[i] is TLabel then
      Apply(TLabel(APanel.Controls[i]), AThemeEnabled) else
    if APanel.Controls[i] is TLCVectorialFillControl then
      Apply(TLCVectorialFillControl(APanel.Controls[i]), AThemeEnabled);
  end;
end;

procedure TDarkTheme.Apply(AVectorialFill: TLCVectorialFillControl;
  AThemeEnabled: boolean);
var
  i: Integer;
begin
  AVectorialFill.Color := GetColorButtonFace(AThemeEnabled);

  for i := 0 to AVectorialFill.ControlCount-1 do
  begin
    if AVectorialFill.Controls[i] is TPanel then
      Apply(TPanel(AVectorialFill.Controls[i]), AThemeEnabled) else
    if AVectorialFill.Controls[i] is TToolBar then
      Apply(TToolBar(AVectorialFill.Controls[i]), AThemeEnabled) else
    if AVectorialFill.Controls[i] is TBCButton then
      Apply(TBCButton(AVectorialFill.Controls[i]), AThemeEnabled) else
    if AVectorialFill.Controls[i] is TLabel then
      Apply(TLabel(AVectorialFill.Controls[i]), AThemeEnabled);
  end;
end;

procedure TDarkTheme.Apply(AToolbar: TToolbar; AThemeEnabled: boolean);
var
  i: Integer;
begin
  if AThemeEnabled then
  begin
    if (AToolbar.OnPaintButton = nil) or (AToolbar.OnPaintButton = @ToolBarPaintButtonLight) then
      AToolbar.OnPaintButton := @ToolBarPaintButtonDark;
  end else
  begin
    if not IsLclLightThemeSafe then  AToolbar.OnPaintButton := @ToolBarPaintButtonLight else
    if AToolbar.OnPaintButton = @ToolBarPaintButtonDark then AToolbar.OnPaintButton := nil;
  end;
  AToolbar.Color := GetColorButtonFace(AThemeEnabled);
  for i := 0 to AToolbar.ControlCount-1 do
  begin
    if AToolbar.Controls[i] is TBCButton then
      Apply(TBCButton(AToolbar.Controls[i]), AThemeEnabled, 0.55) else
    if AToolbar.Controls[i] is TBCComboBox then
      Apply(TBCComboBox(AToolbar.Controls[i]), AThemeEnabled, 0.55) else
    if AToolbar.Controls[i] is TBCTrackbarUpdown then
      Apply(TBCTrackbarUpdown(AToolbar.Controls[i]), AThemeEnabled);
  end;
end;

procedure TDarkTheme.Apply(AButton: TBCButton; ADarkTheme: boolean; AFontHeightRatio: single);

  function MergeColor(AColor1,AColor2:TColor):TColor;
  begin
    result:= BGRAToColor(MergeBGRAWithGammaCorrection(ColorToBGRA(ColorToRGB(AColor1)),1,
    ColorToBGRA(ColorToRGB(AColor2)),1));
  end;

  function HoverColor(AColor1: TColor): TColor;
  var hsla1, hsla2: THSLAPixel;
  begin
    hsla1 := BGRAToHSLA(ColorToBGRA(ColorToRGB(AColor1)));
    hsla2 := BGRAToHSLA(ColorToBGRA(ColorToRGB(clHighlight)));
    hsla1.hue := hsla2.hue;
    hsla1.saturation:= hsla2.saturation;
    result := BGRAToColor(HSLAToBGRA(hsla1));
  end;

var highlight, btnFace, btnShadow, btnText, gradMiddle: TColor;
  fh: Int64;
begin
  if ADarkTheme then
  begin
    highlight := $a0a0a0;
  end else
  begin
    {$IFDEF DARWIN}
    highlight := MergeColor(GetColorButtonFace(false),clWhite);
    {$ELSE}
    highlight := GetColorButtonHighlight(false);
    {$ENDIF}
  end;
  btnFace := GetColorButtonFace(ADarkTheme);
  btnText := GetColorButtonText(ADarkTheme);
  btnShadow := GetColorPanelShadow(ADarkTheme);
  gradMiddle := MergeColor(btnFace,highlight);
  with AButton do
  begin
    Rounding.RoundX := DoScaleX(3, OriginalDPI);
    Rounding.RoundY := DoScaleX(3, OriginalDPI);
    BCAssignSystemState(StateNormal, btnText, btnFace, highlight,
      gradMiddle, btnShadow, btnShadow);
    BCAssignSystemState(StateHover, HoverColor(btnText), HoverColor(btnFace), HoverColor(highlight),
      HoverColor(gradMiddle), HoverColor(btnShadow), HoverColor(btnShadow));
    BCAssignSystemState(StateClicked, HoverColor(btnText), HoverColor(MergeColor(btnFace,btnShadow)),
      HoverColor(btnFace), HoverColor(MergeColor(btnFace,btnShadow)), HoverColor(btnShadow), HoverColor(btnShadow));
    fh := round((AButton.Height+4)*AFontHeightRatio);
    StateNormal.Border.LightWidth := 0;
    StateNormal.FontEx.Height := fh;
    StateNormal.FontEx.ShadowColorOpacity:= 70;
    StateNormal.FontEx.TextAlignment:= bcaLeftCenter;
    StateNormal.FontEx.PaddingLeft:= DoScaleX(3, OriginalDPI);
    StateHover.Border.LightWidth := 0;
    StateHover.FontEx.Height := fh;
    StateHover.FontEx.ShadowColorOpacity:= 70;
    StateHover.FontEx.TextAlignment:= bcaLeftCenter;
    StateHover.FontEx.PaddingLeft:= DoScaleX(3, OriginalDPI);
    StateClicked.Border.LightWidth := 0;
    StateClicked.FontEx.Height := fh;
    StateClicked.FontEx.ShadowColorOpacity:= 70;
    StateClicked.FontEx.TextAlignment:= bcaLeftCenter;
    StateClicked.FontEx.PaddingLeft:= DoScaleX(3, OriginalDPI);
    GlyphMargin := DoScaleX(3, OriginalDPI);
  end;
end;

procedure TDarkTheme.Apply(ACombo: TBCComboBox; ADarkTheme: boolean;
  AFontHeightRatio: single);
var
  fh: Int64;
begin
  Apply(ACombo.Button, ADarkTheme, AFontHeightRatio);
  with ACombo do
  begin
    FocusBorderOpacity:= 128;
    fh := round((Height+4)*AFontHeightRatio);
    Button.StateNormal.FontEx.Height := fh;
    Button.StateNormal.FontEx.ShadowColorOpacity:= 96;
    Button.StateClicked.FontEx.Height := fh;
    Button.StateClicked.FontEx.ShadowColorOpacity:= 96;
    Button.StateHover.FontEx.Height := fh;
    Button.StateHover.FontEx.ShadowColorOpacity:= 96;
    DropDownFontColor:= GetColorEditableText(ADarkTheme);
    DropDownColor:= GetColorEditableFace(ADarkTheme);
    FocusBorderColor:= GetColorEditableText(ADarkTheme);
    DropDownBorderColor:= MergeBGRA(ColorToBGRA(DropDownFontColor), ColorToBGRA(DropDownColor));
    DropDownFontHighlight:= GetColorHighlightText(ADarkTheme);
    DropDownHighlight:= GetColorHighlightBack(ADarkTheme);
  end;
end;

procedure TDarkTheme.Apply(AUpDown: TBCTrackbarUpdown; ADarkTheme: boolean);
begin
  if ADarkTheme then
    AUpDown.ButtonBackground.Color:= $a0a0a0
  else
    AUpDown.ButtonBackground.Color:= GetColorButtonFace(ADarkTheme);

  AUpDown.Border.Color := GetColorPanelShadow(ADarkTheme);
  AUpDown.Background.Color := GetColorEditableFace(ADarkTheme);
  AUpDown.ButtonBackground.Style:= bbsColor;
  AUpDown.Font.Color := GetColorEditableText(ADarkTheme);
end;

procedure TDarkTheme.Apply(ALabel: TLabel; ADarkTheme: boolean);
begin
  ALabel.Font.Color := GetColorEditableText(ADarkTheme);
end;

function TDarkTheme.IsSystemDarkTheme: boolean;
begin
  {$IFDEF DARWIN_DARK_THEME}
  if IsMojaveOrNewer then
    exit(IsMojaveDarkTheme);
  {$ENDIF}
  {$IFDEF WINDOWS}
  case GetWinDarkTheme of
    dtmLight: exit(false);
    dtmDark: exit(true);
  end;
  {$ENDIF}
  result := IsLclDarkTheme;
end;

function TDarkTheme.IsLclDarkTheme: boolean;
const
  cMax = $A0;
var
  N: TColor;
begin
  N:= ColorToRGB(clWindow);
  Result:= (Red(N)<cMax) and (Green(N)<cMax) and (Blue(N)<cMax);
end;

function TDarkTheme.IsLclLightThemeSafe: boolean;
begin
  result := not IsLclDarkTheme and not IsSystemDarkTheme;
end;

function TDarkTheme.HasSystemDarkThemeChanged: boolean;
var
  newState: Boolean;
begin
  newState := IsSystemDarkTheme;
  if newState <> FLastSystemDarkTheme then
  begin
    result := true;
    FLastSystemDarkTheme:= newState;
  end else
    result := false;
end;

function TDarkTheme.GetColorButtonHighlight(ADarkTheme: boolean): TColor;
begin
  if ADarkTheme then result := clDarkBtnHighlight
  else if not IsLclLightThemeSafe then result := $f0f0f0
  else result := clBtnHighlight;
end;

function TDarkTheme.GetColorButtonFace(ADarkTheme: boolean): TColor;
begin
  if ADarkTheme then result := clDarkBtnFace
  else if not IsLclLightThemeSafe then result := $d8d8d8
  else result := clBtnFace;
end;

function TDarkTheme.GetColorForm(ADarkTheme: boolean): TColor;
begin
  if ADarkTheme then result := clDarkBtnFace
  else if not IsLclLightThemeSafe then result := $d8d8d8
  else result := clForm;
end;

function TDarkTheme.GetColorEditableFace(ADarkTheme: boolean): TColor;
begin
  if ADarkTheme then result := clDarkEditableFace
  else if not IsLclLightThemeSafe then result := $ffffff
  else result := clWindow;
end;

function TDarkTheme.GetColorEditableText(ADarkTheme: boolean): TColor;
begin
  if ADarkTheme then result := clLightText
  else if not IsLclLightThemeSafe then result := $303030
  else result := clWindowText;
end;

function TDarkTheme.GetColorButtonText(ADarkTheme: boolean): TColor;
begin
  if ADarkTheme then result := clLightText
  else if not IsLclLightThemeSafe then result := clBlack
  else result := clBtnText;
end;

function TDarkTheme.GetColorPanelHighlight(ADarkTheme: boolean): TColor;
begin
  if ADarkTheme then result := clDarkPanelHighlight
  else if not IsLclLightThemeSafe then result := $f0f0f0
  else result := clBtnHighlight;
end;

function TDarkTheme.GetColorPanelShadow(ADarkTheme: boolean): TColor;
begin
  if ADarkTheme then result := clDarkPanelShadow
  else if not IsLclLightThemeSafe then result := $808080
  else result := clBtnShadow;
end;

function TDarkTheme.GetColorHighlightBack(ADarkTheme: boolean): TColor;
begin
  if ADarkTheme then
  begin
    if BGRADiff(ColorToBGRA(clHighlight), clDarkBtnFace)>=64 then
      result := clHighlight
      else result := MergeBGRA(ColorToBGRA(clLightText), ColorToBGRA(clHighlight));
  end
  else result := clHighlight;
end;

function TDarkTheme.GetColorHighlightText(ADarkTheme: boolean): TColor;
begin
  if ADarkTheme then
  begin
    if BGRADiff(ColorToBGRA(clHighlight), clDarkBtnFace)>=64 then
      result := clHighlightText
      else result := clBlack;
  end
  else result := clHighlightText;
end;

initialization
  DarkThemeInstance := TDarkTheme.Create;

finalization
  DarkThemeInstance.Free;

end.
