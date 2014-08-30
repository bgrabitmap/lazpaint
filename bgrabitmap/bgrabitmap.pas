{
 /**************************************************************************\
                                bgrabitmap.pas
                                --------------
                 Free easy-to-use memory bitmap 32-bit,
                 8-bit for each channel, transparency.
                 Channels in that order : B G R A

                 - Drawing primitives
                 - Resample
                 - Reference counter
                 - Drawing on LCL canvas
                 - Loading and saving images

                 Note : line order can change, so if you access
                 directly to bitmap data, check LineOrder value
                 or use Scanline to compute position.


       --> Include BGRABitmap and BGRABitmapTypes in the 'uses' clause.

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

unit BGRABitmap;

{$mode objfpc}{$H+}

interface

{ Compiler directives are used to include the best version according
  to the platform }

uses
  Classes, SysUtils,
{$IFDEF LCLwin32}
  BGRAWinBitmap,
{$ELSE}
  {$IFDEF LCLgtk}
  BGRAGtkBitmap,
  {$ELSE}
    {$IFDEF LCLgtk2}
  BGRAGtkBitmap,
    {$ELSE}
      {$IFDEF LCLqt}
  BGRAQtBitmap,
      {$ELSE}
  BGRADefaultBitmap,
      {$ENDIF} 
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
  Graphics;

type
{$IFDEF LCLwin32}
  TBGRABitmap = TBGRAWinBitmap;
{$ELSE}
  {$IFDEF LCLgtk}
  TBGRABitmap = TBGRAGtkBitmap;
  {$ELSE}
    {$IFDEF LCLgtk2}
  TBGRABitmap = TBGRAGtkBitmap;
    {$ELSE}
      {$IFDEF LCLqt}
  TBGRABitmap = TBGRAQtBitmap;
      {$ELSE}
  TBGRABitmap = TBGRADefaultBitmap;
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

// draw a bitmap from pure data
procedure BGRABitmapDraw(ACanvas: TCanvas; Rect: TRect; AData: Pointer;
  VerticalFlip: boolean; AWidth, AHeight: integer; Opaque: boolean);
  
{ Replace the content of the variable Destination with the variable
  Temp and frees previous object contained in Destination.
  
  This function is useful as a shortcut for :
 
  var
    temp: TBGRABitmap;
  begin
    ...
    temp := someBmp.Filter... as TBGRABitmap;
    someBmp.Free;
    someBmp := temp;
  end;
  
  which becomes :
  
  begin
    ...
    BGRAReplace(temp, someBmp.Filter... );
  end;
}
procedure BGRAReplace(var Destination: TBGRABitmap; Temp: TObject);

implementation

uses GraphType, BGRABitmapTypes, BGRAReadBMP, BGRAReadGif,
  BGRAReadIco, bgrareadjpeg, BGRAReadLzp, BGRAReadPCX,
  BGRAReadPng, BGRAReadPSD, BGRAReadTGA, BGRAReadXPM,
  BGRAWriteLzp;

var
  tempBmp: TBGRABitmap;

procedure BGRABitmapDraw(ACanvas: TCanvas; Rect: TRect; AData: Pointer;
  VerticalFlip: boolean; AWidth, AHeight: integer; Opaque: boolean);
var
  LineOrder: TRawImageLineOrder;
begin
  if tempBmp = nil then
    tempBmp := TBGRABitmap.Create;
  if VerticalFlip then
    LineOrder := riloBottomToTop
  else
    LineOrder := riloTopToBottom;
  if Opaque then
    tempBmp.DataDrawOpaque(ACanvas, Rect, AData, LineOrder, AWidth, AHeight)
  else
    tempBmp.DataDrawTransparent(ACanvas, Rect, AData, LineOrder, AWidth, AHeight);
end;

procedure BGRAReplace(var Destination: TBGRABitmap; Temp: TObject);
begin
  Destination.Free;
  Destination := Temp as TBGRABitmap;
end;

initialization

  //this variable is assigned to access appropriate functions
  //depending on the platform
  BGRABitmapFactory := TBGRABitmap;

finalization

  tempBmp.Free;

end.

