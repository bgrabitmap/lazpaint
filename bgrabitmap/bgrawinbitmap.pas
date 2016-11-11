{
 /**************************************************************************\
                             bgrawinbitmap.pas
                             -----------------
                 This unit should NOT be added to the 'uses' clause.
                 It contains accelerations for Windows. Notably, it
                 provides direct access to bitmap data.

 ****************************************************************************
 *                                                                          *
 *  This file is part of BGRABitmap library which is distributed under the  *
 *  modified LGPL.                                                          *
 *                                                                          *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,   *
 *  for details about the copyright.                                        *
 *                                                                          *
 *  This program is distributed in the hope that it will be useful,         *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of          *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    *
 *                                                                          *
 ****************************************************************************
}

unit BGRAWinBitmap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRALCLBitmap, Windows, Graphics, GraphType;

type
  { TBGRAWinBitmap }

  TBGRAWinBitmap = class(TBGRALCLBitmap)
  private
    procedure AlphaCorrectionNeeded;
  protected
    DIB_SectionHandle: HBITMAP;
    FReversed: boolean;
    function DIBitmapInfo(AWidth, AHeight: integer): TBitmapInfo;

    procedure ReallocData; override;
    procedure FreeData; override;

    procedure RebuildBitmap; override;
    procedure FreeBitmap; override;

    procedure Init; override;
    function GetBitmap: TBitmap; override;

  public
    procedure LoadFromBitmapIfNeeded; override;
    procedure Draw(ACanvas: TCanvas; x, y: integer; Opaque: boolean=True); override;
    procedure Draw(ACanvas: TCanvas; Rect: TRect; Opaque: boolean = True); override;
    procedure DataDrawOpaque(ACanvas: TCanvas; Rect: TRect; AData: Pointer;
      ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer); override;
    procedure GetImageFromCanvas(CanvasSource: TCanvas; x, y: integer); override;
  end;

implementation

uses BGRADefaultBitmap, BGRABitmapTypes;

type
  { TWinBitmapTracker }

  TWinBitmapTracker = class(TBitmap)
  protected
    FUser: TBGRAWinBitmap;
    procedure Changed(Sender: TObject); override;
  public
    constructor Create(AUser: TBGRAWinBitmap); overload;
  end;

procedure TWinBitmapTracker.Changed(Sender: TObject);
begin
  if FUser <> nil then
    FUser.AlphaCorrectionNeeded;
  inherited Changed(Sender);
end;

constructor TWinBitmapTracker.Create(AUser: TBGRAWinBitmap);
begin
  FUser := AUser;
  inherited Create;
end;

{ TBGRAWinBitmap }

procedure TBGRAWinBitmap.FreeData;
begin
  if DIB_SectionHandle <> 0 then
  begin
    DeleteObject(DIB_SectionHandle);
    FData := nil;
    DIB_SectionHandle := 0;
  end;
end;

procedure TBGRAWinBitmap.RebuildBitmap;
begin
  if FBitmap = nil then
  begin
    FBitmap := TWinBitmapTracker.Create(self);
    FBitmap.Handle := DIB_SectionHandle;
  end;
end;

procedure TBGRAWinBitmap.FreeBitmap;
begin
  if FBitmap <> nil then
  begin
    FBitmap.Handle := 0;
    FBitmap.Free;
    FBitmap := nil;
  end;
end;

procedure TBGRAWinBitmap.Init;
begin
  inherited Init;
  FLineOrder := riloBottomToTop;
end;

function TBGRAWinBitmap.GetBitmap: TBitmap;
begin
  Result:=inherited GetBitmap;
  if (LineOrder = riloTopToBottom) and not FReversed then
  begin
    VerticalFlip;
    FReversed:= true;
  end;
end;

procedure TBGRAWinBitmap.LoadFromBitmapIfNeeded;
begin
  if FReversed then
  begin
    FReversed := false;
    VerticalFlip;
  end;
  if FAlphaCorrectionNeeded then
  begin
    DoAlphaCorrection;
  end;
end;

procedure TBGRAWinBitmap.Draw(ACanvas: TCanvas; x, y: integer; Opaque: boolean);
begin
  if self = nil then exit;
  Draw(ACanvas, Classes.Rect(x,y,x+Width,y+Height), Opaque);
end;

procedure TBGRAWinBitmap.Draw(ACanvas: TCanvas; Rect: TRect; Opaque: boolean);
var
  info:      TBITMAPINFO;
begin
  if (self = nil) or (Width = 0) or (Height = 0) then exit;
  if TBGRAPixel_RGBAOrder then SwapRedBlue;
  if Opaque then
  begin
    info := DIBitmapInfo(Width, Height);
    if LineOrder = riloTopToBottom then
      StretchDIBits(ACanvas.Handle, Rect.Left, Rect.Bottom, Rect.Right -
        Rect.Left, Rect.Top - Rect.Bottom,
        0, 0, Width, Height, Data, info, DIB_RGB_COLORS, SRCCOPY)
    else
      StretchDIBits(ACanvas.Handle, Rect.Left, Rect.Top, Rect.Right -
        Rect.Left, Rect.Bottom - Rect.Top,
        0, 0, Width, Height, Data, info, DIB_RGB_COLORS, SRCCOPY);
  end
  else
  begin
    if Empty then exit;
    if LineOrder = riloTopToBottom then VerticalFlip;
    LoadFromBitmapIfNeeded;
    ACanvas.StretchDraw(Rect, Bitmap);
    if LineOrder = riloTopToBottom then VerticalFlip;
  end;
  if TBGRAPixel_RGBAOrder then SwapRedBlue;
end;

procedure TBGRAWinBitmap.DataDrawOpaque(ACanvas: TCanvas; Rect: TRect;
  AData: Pointer; ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer);
var
  info:      TBITMAPINFO;
  IsFlipped: boolean;
  Temp:      TBGRAPtrBitmap;
begin
  Temp      := nil;
  IsFlipped := False;
  if ALineOrder = riloTopToBottom then
  begin
    Temp := TBGRAPtrBitmap.Create(AWidth, AHeight, AData);
    Temp.VerticalFlip;
    IsFlipped := True;
  end;
  if TBGRAPixel_RGBAOrder then
  begin
    if Temp = nil then
      Temp := TBGRAPtrBitmap.Create(AWidth, AHeight, AData);
    Temp.SwapRedBlue;
  end;

  info := DIBitmapInfo(AWidth, AHeight);
  StretchDIBits(ACanvas.Handle, Rect.Left, Rect.Top, Rect.Right -
    Rect.Left, Rect.Bottom - Rect.Top,
    0, 0, AWidth, AHeight, AData, info, DIB_RGB_COLORS, SRCCOPY);

  if Temp <> nil then
  begin
    if TBGRAPixel_RGBAOrder then Temp.SwapRedBlue;
    if IsFlipped then
      Temp.VerticalFlip;
    Temp.Free;
  end;
end;

procedure TBGRAWinBitmap.AlphaCorrectionNeeded; inline;
begin
  FAlphaCorrectionNeeded := True;
end;

function TBGRAWinBitmap.DIBitmapInfo(AWidth, AHeight: integer): TBitmapInfo;
begin
  with {%H-}Result.bmiHeader do
  begin
    biSize      := sizeof(Result.bmiHeader);
    biWidth     := AWidth;
    biHeight    := AHeight;
    biPlanes    := 1;
    biBitCount  := 32;
    biCompression := BI_RGB;
    biSizeImage := 0;
    biXPelsPerMeter := 0;
    biYPelsPerMeter := 0;
    biClrUsed   := 0;
    biClrImportant := 0;
  end;
end;

procedure TBGRAWinBitmap.ReallocData;
var
  ScreenDC: HDC;
  info:     TBitmapInfo;
begin
  FreeData;
  if (Width <> 0) and (Height <> 0) then
  begin
    ScreenDC := GetDC(0);
    info     := DIBitmapInfo(Width, Height);
    DIB_SectionHandle := CreateDIBSection(ScreenDC, info, DIB_RGB_COLORS, FData, 0, 0);

    if (NbPixels > 0) and (FData = nil) then
      raise EOutOfMemory.Create('TBGRAWinBitmap.ReallocBitmap: Windows error ' +
        IntToStr(GetLastError));

    ReleaseDC(0, ScreenDC);
  end;
  InvalidateBitmap;
end;

procedure TBGRAWinBitmap.GetImageFromCanvas(CanvasSource: TCanvas; x, y: integer);
begin
  self.Canvas.CopyRect(Classes.rect(0, 0, Width, Height), CanvasSource,
    Classes.rect(X, Y, X + Width, Y + Height));
end;

end.

