// SPDX-License-Identifier: GPL-3.0-only
unit UDarkTheme;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, ComCtrls, StdCtrls, Controls, ExtCtrls,
  LazPaintType, BCButton, BCComboBox, BCTrackbarUpdown, LCVectorialFillControl;

const
  clDarkBtnHighlight = $e0e0e0;
  clDarkBtnFace = $606060;
  clDarkEditableFace = $808080;
  clLightText = $f0f0f0;
  clDarkPanelHighlight = $909090;
  clDarkPanelShadow = $404040;

type

  { TDarkTheme }

  TDarkTheme = class
    procedure PanelPaint(Sender: TObject);
    procedure ToolBarPaint(Sender: TObject);
    procedure ToolBarPaintButton(Sender: TToolButton; State: integer);
    procedure Apply(AForm: TForm; AThemeEnabled: boolean; ARecursive: boolean = true); overload;
    procedure Apply(APanel: TPanel; AThemeEnabled: boolean; ARecursive: boolean = true); overload;
    procedure Apply(AVectorialFill: TLCVectorialFillControl; AThemeEnabled: boolean); overload;
    procedure Apply(AToolbar: TToolbar; AThemeEnabled: boolean); overload;
    procedure Apply(AButton: TBCButton; ADarkTheme: boolean; AFontHeightRatio: single = 0.5); overload;
    procedure Apply(ACombo: TBCComboBox; ADarkTheme: boolean; AFontHeightRatio: single = 0.5); overload;
    procedure Apply(AUpDown: TBCTrackbarUpdown; ADarkTheme: boolean); overload;
    procedure Apply(ALabel: TLabel; ADarkTheme: boolean); overload;
  end;

var
  DarkThemeInstance: TDarkTheme;


implementation

uses
  BCTypes, BGRABitmap, BGRABitmapTypes, GraphType, Graphics, BGRACustomDrawn, LCScaleDPI;

procedure BCAssignSystemState(AState: TBCButtonState; AFontColor, ATopColor, AMiddleTopColor, AMiddleBottomColor, ABottomColor, ABorderColor: TColor);
begin
  with AState do
  begin
    Border.Style := bboSolid;
    Border.Color := ABorderColor;
    Border.ColorOpacity := 255;
    FontEx.Color := AFontColor;
    FontEx.Style := [];
    FontEx.Shadow := True;
    FontEx.ShadowColor := clBlack;
    FontEx.ShadowColorOpacity := 192;
    FontEx.ShadowOffsetX := 1;
    FontEx.ShadowOffsetY := 1;
    FontEx.ShadowRadius := 2;
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

procedure TDarkTheme.PanelPaint(Sender: TObject);
var
  c: TCanvas;
begin
  if Sender is TCustomControl then
  begin
    c := TCustomControl(Sender).Canvas;
    c.Pen.Color := clDarkPanelHighlight;
    c.Line(0, 0, c.Width, 0);
    c.Line(0, 0, 0, c.Height);
    c.Pen.Color := clDarkPanelShadow;
    c.Line(0, c.Height-1, c.Width, c.Height-1);
    c.Line(c.Width-1, 0, c.Width-1, c.Height);
  end;
end;

procedure TDarkTheme.ToolBarPaint(Sender: TObject);
var
  T: TToolBar;
begin
  if Sender is TToolBar then
  begin
    T := TToolBar(Sender);
    if T.Align = alLeft then
    begin
      T.Canvas.Pen.Color := clDarkPanelShadow;
      T.Canvas.Line(T.Width-1, 0, T.Width-1, T.Height)
    end
    else if T.Align = alRight then
    begin
      T.Canvas.Pen.Color := clDarkPanelHighlight;
      T.Canvas.Line(0, 0, 0, T.Height)
    end
    else
    begin
      T.Canvas.Pen.Color := clDarkPanelShadow;
      T.Canvas.Line(0, 0, T.Width, 0);
    end;
  end;
end;

procedure TDarkTheme.ToolBarPaintButton(Sender: TToolButton; State: integer);
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
        Bitmap.Rectangle(0, 0, Sender.Width, Sender.Height - 1, BGRA(58, 58, 58),
          BGRA(71, 71, 71), dmSet);
        Bitmap.Rectangle(1, 1, Sender.Width - 1, Sender.Height - 2, BGRA(65, 65, 65),
          BGRA(71, 71, 71), dmSet);
        Bitmap.SetHorizLine(0, Sender.Height - 1, Sender.Width - 1, BGRA(93, 93, 93));
      end
      else
      begin
        if State = 2 then
        begin
          { Button Hovered }
          Bitmap := TBGRABitmap.Create(Sender.Width, Sender.Height);
          Bitmap.GradientFill(0, 0, Sender.Width, Sender.Height, BGRA(132, 132, 132),
            BGRA(109, 109, 109), gtLinear, PointF(0, 0),
            PointF(0, Sender.Height), dmSet);
          Bitmap.Rectangle(0, 0, Sender.Width, Sender.Height - 1,
            BGRA(48, 48, 48), dmSet);
          Bitmap.SetHorizLine(1, 1, Sender.Width - 2, BGRA(160, 160, 160));
          Bitmap.SetHorizLine(0, Sender.Height - 1, Sender.Width - 1, BGRA(83, 83, 83));
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

procedure TDarkTheme.Apply(AForm: TForm; AThemeEnabled: boolean; ARecursive: boolean);
var
  i: Integer;
begin
  if AThemeEnabled then
    AForm.Color := clDarkBtnFace
  else
    AForm.Color := clBtnFace;
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
    if APanel.OnPaint = nil then APanel.OnPaint := @PanelPaint;
    APanel.Color := clDarkBtnFace;
  end else
  begin
    APanel.BevelOuter:= bvRaised;
    if APanel.OnPaint = @PanelPaint then APanel.OnPaint := nil;
    APanel.Color := clBtnFace;
  end;
  if ARecursive then
  for i := 0 to APanel.ControlCount-1 do
  begin
    if APanel.Controls[i] is TPanel then
      Apply(TPanel(APanel.Controls[i]), AThemeEnabled) else
    if APanel.Controls[i] is TToolBar then
      Apply(TToolBar(APanel.Controls[i]), AThemeEnabled) else
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
  if AThemeEnabled then
    AVectorialFill.Color := clDarkBtnFace
  else
    AVectorialFill.Color := clBtnFace;

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
    if AToolbar.OnPaintButton = nil then AToolbar.OnPaintButton := @ToolBarPaintButton;
    AToolbar.Color := clDarkBtnFace;
  end else
  begin
    if AToolbar.OnPaintButton = @ToolBarPaintButton then AToolbar.OnPaintButton := nil;
    AToolbar.Color := clBtnFace;
  end;
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

var highlight, btnFace, btnShadow, btnText: TColor;
  fh: Int64;
begin
  if ADarkTheme then
  begin
    highlight := $a0a0a0;
    btnFace := clDarkEditableFace;
    btnText := clLightText;
    btnShadow:= clDarkPanelShadow;
  end else
  begin
    {$IFDEF DARWIN}
    highlight := MergeColor(clBtnFace,clWhite);
    {$ELSE}
    highlight := clBtnHighlight;
    {$ENDIF}
    btnFace := clBtnFace;
    btnText := clBtnText;
    btnShadow := clBtnShadow;
  end;
  with AButton do
  begin
    Rounding.RoundX := DoScaleX(3, OriginalDPI);
    Rounding.RoundY := DoScaleX(3, OriginalDPI);
    BCAssignSystemState(StateNormal, btnText, btnFace, highlight, btnFace, btnShadow, btnShadow);
    BCAssignSystemState(StateHover, HoverColor(btnText), HoverColor(btnFace), HoverColor(highlight), HoverColor(btnFace), HoverColor(btnShadow), HoverColor(btnShadow));
    BCAssignSystemState(StateClicked, HoverColor(btnText), HoverColor(MergeColor(btnFace,btnShadow)), HoverColor(btnFace), HoverColor(MergeColor(btnFace,btnShadow)), HoverColor(btnShadow), HoverColor(btnShadow));
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
    if ADarkTheme then
    begin
      DropDownBorderColor:= clBlack;
      DropDownFontColor:= clLightText;
      DropDownColor:= clDarkBtnFace;
      FocusBorderColor:= clLightText;
    end else
    begin
      DropDownBorderColor := MergeBGRA(ColorToBGRA(clWindowText),ColorToBGRA(clWindow));
      DropDownFontColor:= clWindowText;
      DropDownColor:= clWindow;
      FocusBorderColor:= clWindowText;
    end;
    DropDownFontHighlight:= clHighlightText;
    DropDownHighlight:= clHighlight;
  end;
end;

procedure TDarkTheme.Apply(AUpDown: TBCTrackbarUpdown; ADarkTheme: boolean);
begin
  if ADarkTheme then
  begin
    AUpDown.Border.Color := clDarkPanelShadow;
    AUpDown.Background.Color := clDarkEditableFace;
    AUpDown.ButtonBackground.Style:= bbsColor;
    AUpDown.ButtonBackground.Color:= $a0a0a0;
    AUpDown.Font.Color := clLightText;
  end else
  begin
    AUpDown.Border.Color := MergeBGRA(ColorToBGRA(clWindowText),ColorToBGRA(clBtnFace));
    AUpDown.Background.Color := clWindow;
    AUpDown.ButtonBackground.Style:= bbsColor;
    AUpDown.ButtonBackground.Color:= clBtnFace;
    AUpDown.Font.Color := clWindowText;
  end;
end;

procedure TDarkTheme.Apply(ALabel: TLabel; ADarkTheme: boolean);
begin
  if ADarkTheme then
    ALabel.Font.Color := clLightText
  else
    ALabel.Font.Color := clWindowText;
end;

initialization
  DarkThemeInstance := TDarkTheme.Create;

finalization
  DarkThemeInstance.Free;

end.
