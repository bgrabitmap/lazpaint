// SPDX-License-Identifier: GPL-3.0-only
unit UToolIcon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, utool, BGRABitmapTypes, BGRABitmap, UImage;

type

  { TCursorHotspotTool }

  TCursorHotspotTool = class(TReadonlyTool)
  protected
    function DoToolDown({%H-}toolDest: TBGRABitmap; pt: TPoint; {%H-}ptF: TPointF;
  {%H-}rightBtn: boolean): TRect; override;
    function GetStatusText: string; override;
  public
    function GetContextualToolbars: TContextualToolbars; override;
    function Render(VirtualScreen: TBGRABitmap; VirtualScreenWidth,
      VirtualScreenHeight: integer;
      BitmapToVirtualScreen: TBitmapToVirtualScreenFunction): TRect; override;
  end;

implementation

uses UGraph, UResourceStrings, LazPaintType;

{ TCursorHotspotTool }

function TCursorHotspotTool.DoToolDown(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF; rightBtn: boolean): TRect;
begin
  Manager.Image.CursorHotSpot := pt;
  result := OnlyRenderChange;
end;

function TCursorHotspotTool.GetStatusText: string;
begin
  Result:= rsHotSpot + '|x = ' + IntToStr(Manager.Image.CursorHotSpot.X)+'|y = '+IntToStr(Manager.Image.CursorHotSpot.Y);
end;

function TCursorHotspotTool.GetContextualToolbars: TContextualToolbars;
begin
  Result:= [];
end;

function TCursorHotspotTool.Render(VirtualScreen: TBGRABitmap;
  VirtualScreenWidth, VirtualScreenHeight: integer;
  BitmapToVirtualScreen: TBitmapToVirtualScreenFunction): TRect;
var
  hotspotPos: TPointF;
begin
  hotspotPos := BitmapToVirtualScreen(PointF(Manager.Image.CursorHotSpot.X,Manager.Image.CursorHotSpot.Y));
  result := NicePoint(VirtualScreen, hotspotPos.X,hotspotPos.Y);
  if hotspotPos.Y > virtualScreenHeight/2 then
    result := RectUnion(result,NiceText(VirtualScreen, round(hotspotPos.X),round(hotspotPos.Y-6), VirtualScreenWidth,VirtualScreenHeight, rsHotSpot, taCenter, tlBottom))
  else
    result := RectUnion(result,NiceText(VirtualScreen, round(hotspotPos.X),round(hotspotPos.Y+6), VirtualScreenWidth,VirtualScreenHeight, rsHotSpot, taCenter, tlTop));
end;

initialization

  RegisterTool(ptHotSpot, TCursorHotspotTool);

end.

