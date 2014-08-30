unit UScaleDPI;

{$mode objfpc}{$H+}

interface

uses
  Forms, Graphics, Controls, ComCtrls;

procedure HighDPI(FromDPI: Integer);
procedure ScaleDPI(Control: TControl; FromDPI: Integer);
procedure ScaleImageList(ImgList: TImageList; FromDPI: Integer);
function DoScaleX(Size: Integer; FromDPI: Integer): integer;
function DoScaleY(Size: Integer; FromDPI: Integer): integer;

implementation

uses BGRABitmap, BGRABitmapTypes;

procedure HighDPI(FromDPI: Integer);
var
  i: Integer;
begin
  for i:=0 to Screen.FormCount-1 do begin
    ScaleDPI(Screen.Forms[i],FromDPI);
  end;
end;

procedure ScaleImageList(ImgList: TImageList; FromDPI: Integer);
var
  TempBmp: TBitmap;
  TempBGRA: array of TBGRABitmap;
  NewWidth,NewHeight: integer;
  i: Integer;

begin
  if Screen.PixelsPerInch <= FromDPI*1.1 then exit;

  NewWidth := ScaleX(ImgList.Width,FromDPI);
  NewHeight := ScaleY(ImgList.Height,FromDPI);

  setlength(TempBGRA, ImgList.Count);
  TempBmp := TBitmap.Create;
  for i := 0 to ImgList.Count-1 do
  begin
    ImgList.GetBitmap(i,TempBmp);
    TempBGRA[i] := TBGRABitmap.Create(TempBmp);
    TempBGRA[i].ResampleFilter := rfBestQuality;
    if (TempBGRA[i].width=0) or (TempBGRA[i].height=0) then continue;
    while (TempBGRA[i].Width < NewWidth) or (TempBGRA[i].Height < NewHeight) do
      BGRAReplace(TempBGRA[i], TempBGRA[i].FilterSmartZoom3(moLowSmooth));
    BGRAReplace(TempBGRA[i], TempBGRA[i].Resample(NewWidth,NewHeight));
  end;
  TempBmp.Free;

  ImgList.Clear;
  ImgList.Width:= NewWidth;
  ImgList.Height:= NewHeight;

  for i := 0 to high(TempBGRA) do
  begin
    ImgList.Add(TempBGRA[i].Bitmap,nil);
    TempBGRA[i].Free;
  end;
end;

function DoScaleX(Size: Integer; FromDPI: Integer): integer;
begin
  if Screen.PixelsPerInch <= FromDPI then
    result := Size
  else
    result := ScaleX(Size, FromDPI);
end;

function DoScaleY(Size: Integer; FromDPI: Integer): integer;
begin
  if Screen.PixelsPerInch <= FromDPI then
    result := Size
  else
    result := ScaleY(Size, FromDPI);
end;

procedure ScaleDPI(Control: TControl; FromDPI: Integer);
var
  n: Integer;
  WinControl: TWinControl;
  ToolBarControl: TToolBar;
begin
  if Screen.PixelsPerInch <= FromDPI then exit;

  with Control do begin
    Left:=ScaleX(Left,FromDPI);
    Top:=ScaleY(Top,FromDPI);
    Width:=ScaleX(Width,FromDPI);
    Height:=ScaleY(Height,FromDPI);
    {$IFDEF LCL Qt}
      Font.Size := 0;
    {$ELSE}
      Font.Height := ScaleY(Font.GetTextHeight('Hg'),FromDPI);
    {$ENDIF}
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