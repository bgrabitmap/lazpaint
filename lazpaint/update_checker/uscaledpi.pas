unit uscaledpi;

{$mode objfpc}{$H+}

interface

uses
  Forms, Graphics, Controls, ComCtrls;

procedure HighDPI(FromDPI: Integer);
procedure ScaleDPI(Control: TControl; FromDPI: Integer);

implementation

procedure HighDPI(FromDPI: Integer);
var
  i: Integer;
begin
  for i:=0 to Screen.FormCount-1 do begin
    ScaleDPI(Screen.Forms[i],FromDPI);
  end;
end;

procedure ScaleDPI(Control: TControl; FromDPI: Integer);
var
  n: Integer;
  WinControl: TWinControl;
  ToolBarControl: TToolBar;
begin
  if Screen.PixelsPerInch = FromDPI then exit;

  with Control do begin
    Left:=ScaleX(Left,FromDPI);
    Top:=ScaleY(Top,FromDPI);
    Width:=ScaleX(Width,FromDPI);
    Height:=ScaleY(Height,FromDPI);
    Font.Height := ScaleY(Font.GetTextHeight('Hg'),FromDPI);
  end;

  if Control is TToolBar then begin
    ToolBarControl:=TToolBar(Control);
    with ToolBarControl do begin
      ButtonWidth:=ScaleX(ButtonWidth,FromDPI);
      ButtonHeight:=ScaleY(ButtonHeight,FromDPI);
    end;
  end;

  if Control is TWinControl then begin
    WinControl:=TWinControl(Control);
    if WinControl.ControlCount > 0 then begin
      for n:=0 to WinControl.ControlCount-1 do begin
        if WinControl.Controls[n] is TControl then begin
          ScaleDPI(WinControl.Controls[n],FromDPI);
        end;
      end;
    end;
  end;
end;

end.

