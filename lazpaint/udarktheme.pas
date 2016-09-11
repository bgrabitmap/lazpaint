unit udarktheme;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, ComCtrls, Controls, ExtCtrls;

type

  { TDarkTheme }

  TDarkTheme = class
  private
    procedure PatchInstanceClass(Instance: TObject; NewClass: TClass);
  public
    procedure SetDarkTheme(Control: TControl);
    procedure ToolBarPaint(Sender: TObject);
    procedure ToolBarPaintButton(Sender: TToolButton; State: integer);
  end;

var
  DarkTheme: TDarkTheme;

implementation

uses
  BGRABitmap, BGRABitmapTypes, GraphType, Graphics, BGRACustomDrawn;

{ TDarkTheme }

procedure TDarkTheme.PatchInstanceClass(Instance: TObject; NewClass: TClass);
type
  PClass = ^TClass;
begin
  //if Assigned(Instance) and Assigned(NewClass) and NewClass.InheritsFrom(Instance.ClassType) and (NewClass.InstanceSize = Instance.InstanceSize) then

  //begin
    PClass(Instance)^ := NewClass;
  //end;
end;

procedure TDarkTheme.SetDarkTheme(Control: TControl);
var
  i: integer;
  WinControl: TWinControl;
begin
  if Control is TToolBar then
  begin
    TToolBar(Control).OnPaint := @ToolBarPaint;
    TToolBar(Control).OnPaintButton := @ToolBarPaintButton;
  end;

  if Control is TPanel then
  begin
    // change color of panel
    //TPanel(Control).Color := $00535353;
    // change class of panel
    PatchInstanceClass(Control, TBCDPanel);
  end;

  if Control is TWinControl then begin
    WinControl:=TWinControl(Control);
    if WinControl.ControlCount > 0 then begin
      for i:=0 to WinControl.ControlCount-1 do begin
        if WinControl.Controls[i] is TControl then begin
          SetDarkTheme(WinControl.Controls[i]);
        end;
      end;
    end;
  end;
end;

procedure TDarkTheme.ToolBarPaint(Sender: TObject);
var
  T: TToolBar;
begin
  if Sender is TToolBar then
  begin
    T := TToolBar(Sender);
    T.Canvas.Brush.Color := $00535353;
    T.Canvas.FillRect(0, 0, T.Width, T.Height);

    // fill the same line that is in the TBCDPanel
    T.Canvas.Pen.Color := RGBToColor(106, 106, 106);
    T.Canvas.Line(0, 0, T.Width, 0);
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
  Bitmap := TBGRABitmap.Create(Sender.Width, Sender.Height);

  if Sender.Style = tbsButton then
  begin
    if Sender.Enabled then
    begin
      if State = 3 then
      begin
        { Button Down }
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
      Bitmap.Rectangle(0, 0, Sender.Width, Sender.Height - 1, BGRA(66, 66, 66),
        BGRA(71, 71, 71), dmSet);
      Bitmap.SetHorizLine(0, Sender.Height - 1, Sender.Width - 1, BGRA(83, 83, 83));
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

  Bitmap.Draw(Sender.Canvas, 0, 0, False);
  Bitmap.Free;

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

initialization
  DarkTheme := TDarkTheme.Create;

finalization
  DarkTheme.Free;

end.
