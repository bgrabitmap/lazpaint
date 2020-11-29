// SPDX-License-Identifier: GPL-3.0-only
unit LCScaleDPI;

{$mode objfpc}{$H+}

interface

uses
  Forms, Graphics, Controls, ComCtrls;

procedure ScaleForms(FromDPI: Integer);
procedure ScaleControl(Control: TControl; FromDPI: Integer;
  ToDPI_X: Integer = 0; ToDPI_Y: Integer = 0; ScaleToolbar: boolean = false);
procedure ScaleImageList(SourceList: TImageList; newWidth, newHeight: Integer; TargetList: TImageList);
function DoScaleX(Size: Integer; FromDPI: Integer; ToDPI: Integer = 0): integer;
function DoScaleY(Size: Integer; FromDPI: Integer; ToDPI: Integer = 0): integer;
function DoScaleXF(Size: single; FromDPI: Integer; ToDPI: Integer = 0): single;
function DoScaleYF(Size: single; FromDPI: Integer; ToDPI: Integer = 0): single;

implementation

uses BGRABitmap, BGRABitmapTypes, LCLType;

procedure ScaleForms(FromDPI: Integer);
var
  i: Integer;
begin
  for i:=0 to Screen.FormCount-1 do begin
    ScaleControl(Screen.Forms[i],FromDPI);
  end;
end;

procedure ScaleImageList(SourceList: TImageList; newWidth, newHeight: Integer; TargetList: TImageList);
var
  TempBmp: TBitmap;
  TempBGRA: array of TBGRABitmap;
  i: Integer;

begin
  if (TargetList = SourceList) and (newWidth = SourceList.Width) and
    (newHeight = SourceList.Height) then exit;

  setlength(TempBGRA, SourceList.Count);
  TempBmp := TBitmap.Create;
  for i := 0 to SourceList.Count-1 do
  begin
    SourceList.GetBitmap(i,TempBmp);
    TempBGRA[i] := TBGRABitmap.Create(TempBmp);
    TempBGRA[i].ResampleFilter := rfBestQuality;
    if (TempBGRA[i].width=0) or (TempBGRA[i].height=0) then continue;
    while (TempBGRA[i].Width < NewWidth) or (TempBGRA[i].Height < NewHeight) do
      BGRAReplace(TempBGRA[i], TempBGRA[i].FilterSmartZoom3(moLowSmooth));
    BGRAReplace(TempBGRA[i], TempBGRA[i].Resample(NewWidth,NewHeight));
    BGRAReplace(TempBGRA[i], TempBGRA[i].FilterSharpen(0.50));
  end;
  TempBmp.Free;

  TargetList.Clear;
  TargetList.Width:= NewWidth;
  TargetList.Height:= NewHeight;

  for i := 0 to high(TempBGRA) do
  begin
    {$IFDEF LCLWin32}
    If TBGRAPixel_RGBAOrder then TempBGRA[i].SwapRedBlue;
    {$ENDIF}
    TargetList.Add(TempBGRA[i].Bitmap,nil);
    TempBGRA[i].Free;
  end;
end;

function DoScaleX(Size: Integer; FromDPI: Integer; ToDPI: Integer): integer;
begin
  if ToDPI = 0 then ToDPI := ScreenInfo.PixelsPerInchX;
  if ToDPI <= FromDPI then
    result := Size
  else
    Result := MulDiv(Size, ToDPI, FromDPI);
end;

function DoScaleY(Size: Integer; FromDPI: Integer; ToDPI: Integer): integer;
begin
  if ToDPI = 0 then ToDPI := ScreenInfo.PixelsPerInchY;
  if ToDPI <= FromDPI then
    result := Size
  else
    Result := MulDiv(Size, ToDPI, FromDPI);
end;

function DoScaleXF(Size: single; FromDPI: Integer; ToDPI: Integer): single;
begin
  if ToDPI = 0 then ToDPI := ScreenInfo.PixelsPerInchX;
  if ToDPI <= FromDPI then
    result := Size
  else
    Result := Size * ToDPI / FromDPI;
end;

function DoScaleYF(Size: single; FromDPI: Integer; ToDPI: Integer): single;
begin
  if ToDPI = 0 then ToDPI := ScreenInfo.PixelsPerInchY;
  if ToDPI <= FromDPI then
    result := Size
  else
    Result := Size * ToDPI / FromDPI;
end;

procedure ScaleControl(Control: TControl; FromDPI: Integer; ToDPI_X: Integer;
  ToDPI_Y: Integer; ScaleToolbar: boolean);
var
  n: Integer;
  WinControl: TWinControl;
  ToolBarControl: TToolBar;
begin
  if ToDPI_X = 0 then ToDPI_X := ScreenInfo.PixelsPerInchX;
  if ToDPI_Y = 0 then ToDPI_Y := ScreenInfo.PixelsPerInchY;
  if ToDPI_X < FromDPI then ToDPI_X := FromDPI;
  if ToDPI_Y < FromDPI then ToDPI_Y := FromDPI;
  if (ToDPI_X = FromDPI) and (ToDPI_Y = FromDPI) then exit;

  with Control do begin
    Left:=DoScaleX(Left,FromDPI,ToDPI_X);
    Top:=DoScaleY(Top,FromDPI,ToDPI_Y);
    Width:=DoScaleX(Width,FromDPI,ToDPI_X);
    Height:=DoScaleY(Height,FromDPI,ToDPI_Y);
    if not IsParentFont then
    begin
      if Font.Size = 0 then
        Font.Height := -DoScaleY(12,FromDPI,ToDPI_Y)
      else
        Font.Size:= round(Font.Size * ToDPI_Y / FromDPI);
    end;
  end;

  if Control is TToolBar then begin
    if not ScaleToolbar then exit;
    ToolBarControl:=TToolBar(Control);
    with ToolBarControl do begin
      ButtonWidth:=DoScaleX(ButtonWidth,FromDPI,ToDPI_X);
      ButtonHeight:=DoScaleY(ButtonHeight,FromDPI,ToDPI_Y);
    end;
    exit;
  end;

  if Control is TWinControl then begin
    WinControl:=TWinControl(Control);
    with WinControl.ChildSizing do
    begin
      HorizontalSpacing := DoScaleX(HorizontalSpacing, FromDPI, ToDPI_X);
      LeftRightSpacing := DoScaleX(LeftRightSpacing, FromDPI, ToDPI_X);
      TopBottomSpacing := DoScaleY(TopBottomSpacing, FromDPI, ToDPI_Y);
      VerticalSpacing := DoScaleY(VerticalSpacing, FromDPI, ToDPI_Y);
    end;
    if WinControl.ControlCount > 0 then begin
      for n:=0 to WinControl.ControlCount-1 do begin
        if WinControl.Controls[n] is TControl then begin
          ScaleControl(WinControl.Controls[n],FromDPI,ToDPI_X,ToDPI_Y,
            ScaleToolbar);
        end;
      end;
    end;
  end;
end;

end.
