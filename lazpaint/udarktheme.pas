unit UDarkTheme;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, ComCtrls, Controls, ExtCtrls;

const
  clDarkBtnHighlight = $e0e0e0;
  clDarkBtnFace = $606060;
  clLightText = $f0f0f0;
  clDarkPanelHighlight = $909090;
  clDarkPanelShadow = $404040;

type

  { TDarkTheme }

  TDarkTheme = class
    procedure PanelPaint(Sender: TObject);
    procedure ToolBarPaint(Sender: TObject);
    procedure ToolBarPaintButton(Sender: TToolButton; State: integer);
    procedure Apply(APanel: TPanel; AThemeEnabled: boolean); overload;
    procedure Apply(AToolbar: TToolbar; AThemeEnabled: boolean); overload;
  end;

var
  DarkThemeInstance: TDarkTheme;

implementation

uses
  BGRABitmap, BGRABitmapTypes, GraphType, Graphics, BGRACustomDrawn;

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
        Bitmap.Rectangle(0, 0, Sender.Width, Sender.Height - 1, BGRA(48, 48, 48),
          BGRA(61, 61, 61), dmSet);
        Bitmap.Rectangle(1, 1, Sender.Width - 1, Sender.Height - 2, BGRA(55, 55, 55),
          BGRA(61, 61, 61), dmSet);
        Bitmap.SetHorizLine(0, Sender.Height - 1, Sender.Width - 1, BGRA(83, 83, 83));
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
      { Text Enabled }
      Bitmap.TextOut((Sender.Width - ts.cx) div 2, ((Sender.Height - ts.cy) div 2) -
        1, Sender.Caption, BGRA(47, 47, 47));
      Bitmap.TextOut((Sender.Width - ts.cx) div 2, (Sender.Height - ts.cy) div 2,
        Sender.Caption, BGRA(229, 229, 229));
    end
    else
      { Text Disabled }
      Bitmap.TextOut((Sender.Width - ts.cx) div 2, (Sender.Height - ts.cy) div 2,
        Sender.Caption, BGRA(170, 170, 170));}
  end;

  if Assigned(Bitmap) then
  begin
    Bitmap.Draw(Sender.Canvas, 0, 0, False);
    Bitmap.Free;
  end;

  if Sender.Parent is TToolBar then
  begin
    T := TToolBar(Sender.Parent);
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

procedure TDarkTheme.Apply(APanel: TPanel; AThemeEnabled: boolean);
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
end;

procedure TDarkTheme.Apply(AToolbar: TToolbar; AThemeEnabled: boolean);
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
end;

initialization
  DarkThemeInstance := TDarkTheme.Create;

finalization
  DarkThemeInstance.Free;

end.
