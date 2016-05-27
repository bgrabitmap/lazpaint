unit UScaleDPI;

{$mode objfpc}{$H+}

interface

uses
  Forms, Graphics, Controls, ComCtrls;

procedure HighDPI(FromDPI: Integer);
procedure ScaleDPI(Control: TControl; FromDPI: Integer; ToDPI_X: Integer = 0; ToDPI_Y: Integer = 0);
procedure ScaleImageList(SourceList: TImageList; newWidth, newHeight: Integer; TargetList: TImageList);
function DoScaleX(Size: Integer; FromDPI: Integer; ToDPI: Integer = 0): integer;
function DoScaleY(Size: Integer; FromDPI: Integer; ToDPI: Integer = 0): integer;

implementation

uses BGRABitmap, BGRABitmapTypes, LCLType;

procedure HighDPI(FromDPI: Integer);
var
  i: Integer;
begin
  for i:=0 to Screen.FormCount-1 do begin
    ScaleDPI(Screen.Forms[i],FromDPI);
  end;
end;

procedure ScaleImageList(SourceList: TImageList; newWidth, newHeight: Integer; TargetList: TImageList);
var
  TempBmp: TBitmap;
  TempBGRA: array of TBGRABitmap;
  i: Integer;

begin
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
    {$IFDEF WINDOWS}
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

procedure ScaleDPI(Control: TControl; FromDPI: Integer; ToDPI_X, ToDPI_Y: integer);
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
    {$IFDEF LCL Qt}
      Font.Size := 0;
    {$ELSE}
      Font.Height := ScaleY(Font.GetTextHeight('Hg'),FromDPI);
    {$ENDIF}
  end;

  if Control is TToolBar then begin
    ToolBarControl:=TToolBar(Control);
    with ToolBarControl do begin
      ButtonWidth:=DoScaleX(ButtonWidth,FromDPI,ToDPI_X);
      ButtonHeight:=DoScaleY(ButtonHeight,FromDPI,ToDPI_Y);
    end;
  end;

  if Control is TWinControl then begin
    WinControl:=TWinControl(Control);
    if WinControl.ControlCount > 0 then begin
      for n:=0 to WinControl.ControlCount-1 do begin
        if WinControl.Controls[n] is TControl then begin
          ScaleDPI(WinControl.Controls[n],FromDPI,ToDPI_X,ToDPI_Y);
        end;
      end;
    end;
  end;
end;

end.
