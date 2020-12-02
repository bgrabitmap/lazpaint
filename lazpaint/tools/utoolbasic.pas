// SPDX-License-Identifier: GPL-3.0-only
unit UToolBasic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, utool, BGRABitmapTypes, BGRABitmap, BGRALayerOriginal,
  UImage, LCVectorOriginal, LCLType;

type

  { TToolHand }

  TToolHand = class(TReadonlyTool)
  protected
    handMoving: boolean;
    handOriginF: TPointF;
    function FixSelectionTransform: boolean; override;
    function FixLayerOffset: boolean; override;
    function DoToolDown({%H-}toolDest: TBGRABitmap; {%H-}pt: TPoint; ptF: TPointF;
      {%H-}rightBtn: boolean): TRect; override;
    function DoToolMove({%H-}toolDest: TBGRABitmap; {%H-}pt: TPoint; ptF: TPointF): TRect; override;
    function GetStatusText: string; override;
  public
    constructor Create(AManager: TToolManager); override;
    function ToolUp: TRect; override;
  end;

  { TToolColorPicker }

  TToolColorPicker = class(TReadonlyTool)
  protected
    colorpicking,colorpickingRight: boolean;
    function DoToolDown(toolDest: TBGRABitmap; pt: TPoint; ptF: TPointF;
      rightBtn: boolean): TRect; override;
    function DoToolMove(toolDest: TBGRABitmap; pt: TPoint; {%H-}ptF: TPointF): TRect; override;
    function FixLayerOffset: boolean; override;
  public
    function ToolUp: TRect; override;
    function GetContextualToolbars: TContextualToolbars; override;
  end;

  { TToolPen }

  TToolPen = class(TGenericTool)
  protected
    class var HintShowed: boolean;
    penDrawing, penDrawingRight: boolean;
    shiftClicking, shiftClickingRight: boolean;
    penOrigin: TPointF;
    function PickColorWithShift: boolean; virtual;
    function GetIsSelectingTool: boolean; override;
    function GetUniversalBrush(ARightButton: boolean): TUniversalBrush; virtual;
    function StartDrawing(toolDest: TBGRABitmap; ptF: TPointF; rightBtn: boolean): TRect; virtual;
    function ContinueDrawing(toolDest: TBGRABitmap; originF, destF: TPointF; rightBtn: boolean): TRect; virtual;
    function DoToolDown(toolDest: TBGRABitmap; pt: TPoint; ptF: TPointF; rightBtn: boolean): TRect; override;
    function DoToolMove(toolDest: TBGRABitmap; pt: TPoint; ptF: TPointF): TRect; override;
    function DoToolShiftClick(toolDest: TBGRABitmap; ptF: TPointF; rightBtn: boolean): TRect; virtual;
  public
    function ToolUp: TRect; override;
    function GetContextualToolbars: TContextualToolbars; override;
    destructor Destroy; override;
  end;

  { TToolErase }

  TToolErase = class(TToolPen)
  protected
    procedure ApplyEraseMode(var image: TBGRABitmap);
    function BlurRadius: single;
    function StartDrawing(toolDest: TBGRABitmap; ptF: TPointF; {%H-}rightBtn: boolean): TRect; override;
    function ContinueDrawing(toolDest: TBGRABitmap; originF, destF: TPointF; {%H-}rightBtn: boolean): TRect; override;
  public
    function GetContextualToolbars: TContextualToolbars; override;
  end;


implementation

uses Types, Graphics, ugraph, Controls, LazPaintType,
  UResourceStrings, BGRAPen, math;

{ TToolErase }

procedure TToolErase.ApplyEraseMode(var image: TBGRABitmap);
var
  radius: single;
  p: PBGRAPixel;
  ec: TExpandedPixel;
  curLight: Word;
  i: Integer;
  lightDiff: word;
begin
  case Manager.EraserMode of
  emSoften: begin
      radius := BlurRadius;
      if radius < 2.5 then
        BGRAReplace(image, image.FilterBlurRadial(round(radius*10),rbPrecise)) else
        BGRAReplace(image, image.FilterBlurRadial(round(radius),rbFast));
    end;
  emSharpen: BGRAReplace(image, image.FilterSharpen(Manager.EraserAlpha/255));
  emLighten:
    begin
      p := image.Data;
      lightDiff := round(Manager.EraserAlpha*32768/255);
      for i := 0 to image.NbPixels-1 do
        begin
          ec := p^.ToExpanded;
          curLight := GetLightness(ec);
          p^ := SetLightness(ec, min(curLight + lightDiff, 65535), curLight);
          inc(p);
        end;
    end;
  emDarken:
    begin
      p := image.Data;
      lightDiff := round(Manager.EraserAlpha*32768/255);
      for i := 0 to image.NbPixels-1 do
        begin
          ec := p^.ToExpanded;
          curLight := GetLightness(ec);
          p^ := SetLightness(ec, max(curLight - lightDiff, 0), curLight);
          inc(p);
        end;
    end;
  end;
end;

function TToolErase.BlurRadius: single;
begin
  result := manager.PenWidth/4*Manager.EraserAlpha/255;
end;

function TToolErase.StartDrawing(toolDest: TBGRABitmap; ptF: TPointF;
  rightBtn: boolean): TRect;
var ix,iy: integer;
  areaCopy, mask: TBGRABitmap;
  r: TRect;
begin
  if Manager.EraserMode <> emEraseAlpha then
  begin
    result := GetShapeBounds([ptF],Manager.PenWidth+BlurRadius);
    if IntersectRect(result, result, rect(0,0,toolDest.width,toolDest.height)) then
    begin
      areaCopy := toolDest.GetPart(result) as TBGRABitmap;
      ApplyEraseMode(areaCopy);
      mask := TBGRABitmap.Create(result.Right-result.left,result.bottom-result.top, BGRABlack);
      mask.LinearAntialiasing := true;
      if Manager.ShapeOptionAliasing then
      begin
        r := rect(round(ptF.X-result.left-Manager.PenWidth/2+0.5),round(ptF.Y-result.top-Manager.PenWidth/2+0.5),
                  round(ptF.X-result.left+Manager.PenWidth/2+0.5),round(ptF.Y-result.top+Manager.PenWidth/2+0.5));
        mask.FillEllipseInRect(r,Manager.ApplyPressure(BGRAWhite));
      end
      else
        mask.FillEllipseAntialias(ptF.X-result.left,ptF.Y-result.top,
          Manager.PenWidth/2,Manager.PenWidth/2,Manager.ApplyPressure(BGRAWhite));
      areaCopy.ScanOffset := Point(-result.left,-result.top);
      mask.ScanOffset := Point(-result.left,-result.top);
      toolDest.CrossFade(result, toolDest, areaCopy, mask, dmSet);
      mask.Free;
      areaCopy.Free;
    end;
  end else
  begin
    if ((ssSnap in ShiftState) or Manager.ShapeOptionAliasing) and (Manager.PenWidth = 1) then
    begin
      ix := round(ptF.X);
      iy := round(ptF.Y);
      toolDest.ErasePixel(ix,iy,Manager.ApplyPressure(Manager.EraserAlpha));
      result := rect(ix,iy,ix+1,iy+1);
    end
    else
    begin
      result := GetShapeBounds([ptF],Manager.PenWidth);
      toolDest.ClipRect := result;
      if Manager.ShapeOptionAliasing then
      begin
        r := rect(round(ptF.X-Manager.PenWidth/2+0.5),round(ptF.Y-Manager.PenWidth/2+0.5),
             round(ptF.X+Manager.PenWidth/2+0.5),round(ptF.Y+Manager.PenWidth/2+0.5));
        toolDest.EraseEllipseInRect(r,Manager.ApplyPressure(Manager.EraserAlpha));
      end else
        toolDest.EraseEllipseAntialias(ptF.X,ptF.Y,Manager.PenWidth/2,Manager.PenWidth/2,Manager.ApplyPressure(Manager.EraserAlpha));
      toolDest.NoClip;
    end;
  end;
end;

function TToolErase.ContinueDrawing(toolDest: TBGRABitmap; originF,
  destF: TPointF; rightBtn: boolean): TRect;
var areaCopy, mask: TBGRABitmap;
  pts: ArrayOfTPointF;
  cOpacity: TBGRAPixel;
begin
  if Manager.EraserMode <> emEraseAlpha then
  begin
    result := GetShapeBounds([destF,originF],Manager.PenWidth+BlurRadius);
    if IntersectRect(result, result, rect(0,0,toolDest.width,toolDest.height)) then
    begin
      areaCopy := toolDest.GetPart(result) as TBGRABitmap;
      ApplyEraseMode(areaCopy);
      mask := TBGRABitmap.Create(result.Right-result.left,result.bottom-result.top, BGRABlack);
      mask.LinearAntialiasing := true;

      if Manager.EraserMode in [emLighten,emDarken] then
        cOpacity := BGRA(0,0,0, Manager.EraserAlpha div 2)
      else
        cOpacity := BGRA(0,0,0, Manager.EraserAlpha);

      pts := toolDest.Pen.ComputePolyline(
        [PointF(destF.X-result.left,destF.Y-result.top),
         PointF(originF.X-result.left,originF.Y-result.top)],
         Manager.PenWidth,cOpacity,False);

      if Manager.ShapeOptionAliasing then
        mask.FillPoly(pts,BGRAWhite)
      else
        mask.FillPolyAntialias(pts,BGRAWhite);

      areaCopy.ScanOffset := Point(-result.left,-result.top);
      mask.ScanOffset := Point(-result.left,-result.top);
      toolDest.CrossFade(result, toolDest, areaCopy, mask, dmSet);
      mask.Free;
      areaCopy.Free;
    end;
  end else
  if Manager.ShapeOptionAliasing then
  begin
    if Manager.PenWidth = 1 then
    begin
      toolDest.EraseLine(round(destF.X),round(destF.Y),round(originF.X),round(originF.Y),Manager.ApplyPressure(Manager.EraserAlpha),false);
      result := GetShapeBounds([destF,originF],1);
    end else
    begin
      pts := toolDest.Pen.ComputePolyline([PointF(destF.X,destF.Y),PointF(originF.X,originF.Y)],Manager.PenWidth,BGRAPixelTransparent,False);
      toolDest.ErasePoly(pts, Manager.ApplyPressure(Manager.EraserAlpha));
      result := GetShapeBounds([destF,originF],Manager.PenWidth);
    end;
  end else
  begin
    if (ssSnap in ShiftState) and (Manager.PenWidth = 1) then
    begin
      toolDest.EraseLineAntialias(round(destF.X),round(destF.Y),round(originF.X),round(originF.Y),Manager.ApplyPressure(Manager.EraserAlpha),false);
      result := GetShapeBounds([destF,originF],1);
    end else
    begin
      toolDest.EraseLineAntialias(destF.X,destF.Y,originF.X,originF.Y,Manager.ApplyPressure(Manager.EraserAlpha),Manager.PenWidth,False);
      result := GetShapeBounds([destF,originF],Manager.PenWidth);
    end;
  end;
end;

function TToolErase.GetContextualToolbars: TContextualToolbars;
begin
  Result:= [ctPenWidth,ctEraserOption,ctAliasing];
end;

{ TToolPen }

function TToolPen.PickColorWithShift: boolean;
begin
  result := true;
end;

function TToolPen.GetIsSelectingTool: boolean;
begin
  Result:= false;
end;

function TToolPen.GetUniversalBrush(ARightButton: boolean): TUniversalBrush;
begin
  if ARightButton then result := GetBackUniversalBrush
  else result := GetForeUniversalBrush;
end;

function TToolPen.StartDrawing(toolDest: TBGRABitmap; ptF: TPointF;
  rightBtn: boolean): TRect;
var ix,iy: integer;
  r: TRect;
  b: TUniversalBrush;
begin
  b := GetUniversalBrush(rightBtn);
  if ((ssSnap in ShiftState) or Manager.ShapeOptionAliasing) and (Manager.PenWidth = 1) then
  begin
    ix := round(ptF.X);
    iy := round(ptF.Y);
    toolDest.DrawPixel(ix, iy, b);
    result := rect(ix,iy,ix+1,iy+1);
  end else
  begin
    result := GetShapeBounds([ptF],Manager.PenWidth);
    toolDest.ClipRect := result;
    if Manager.ShapeOptionAliasing then
    begin
      r := rect(round(ptF.X-Manager.PenWidth/2+0.5),round(ptF.Y-Manager.PenWidth/2+0.5),
                round(ptF.X+Manager.PenWidth/2+0.5),round(ptF.Y+Manager.PenWidth/2+0.5));
      toolDest.FillEllipseInRect(r, b);
    end
    else
      toolDest.FillEllipseAntialias(ptF.X, ptF.Y, Manager.PenWidth/2, Manager.PenWidth/2, b);
    toolDest.NoClip;
  end;
  ReleaseUniversalBrushes;
end;

function TToolPen.ContinueDrawing(toolDest: TBGRABitmap; originF, destF: TPointF; rightBtn: boolean): TRect;
var
  pts: ArrayOfTPointF;
  b: TUniversalBrush;
  testPix: TBGRAPixel;
  testContext: TUniBrushContext;
begin
  b := GetUniversalBrush(rightBtn);
  if ((ssSnap in ShiftState) or Manager.ShapeOptionAliasing) and (Manager.PenWidth = 1) then
  begin
    if Manager.ShapeOptionAliasing then
      toolDest.DrawLine(round(destF.X), round(destF.Y), round(originF.X), round(originF.Y), b, false)
    else
      toolDest.DrawLineAntialias(round(destF.X), round(destF.Y),
        round(originF.X), round(originF.Y), b, false);
    result := GetShapeBounds([destF,originF],1);
  end else
  begin
    result := GetShapeBounds([destF,originF],Manager.PenWidth+1);
    toolDest.ClipRect := result;
    if Manager.ShapeOptionAliasing then
    begin
      pts := toolDest.Pen.ComputePolyline([PointF(destF.X,destF.Y),PointF(originF.X,originF.Y)],
       Manager.PenWidth, BGRAPixelTransparent, False);
      toolDest.FillPoly(pts, b);
    end else
    begin
      testPix := BGRAPixelTransparent;
      b.MoveTo(@testContext, @testPix, round(originF.X), round(originF.Y));
      b.PutNextPixels(@testContext, 65535, 1);
      pts := toolDest.Pen.ComputePolyline([PointF(destF.X,destF.Y),PointF(originF.X,originF.Y)],
       Manager.PenWidth, testPix, False);
      toolDest.FillPolyAntialias(pts, b);
    end;
    toolDest.NoClip;
  end;
  ReleaseUniversalBrushes;
end;

function TToolPen.DoToolDown(toolDest: TBGRABitmap; pt: TPoint; ptF: TPointF;
  rightBtn: boolean): TRect;
begin
  if ssSnap in ShiftState then ptF := PointF(pt.X,pt.Y);
  if not penDrawing then
  begin
    if PickColorWithShift and (ssShift in ShiftState) then
    begin
      result := DoToolShiftClick(toolDest, ptF, rightBtn);
      shiftClicking := true;
      shiftClickingRight := rightBtn;
    end else
    begin
      toolDest.PenStyle := psSolid;
      penDrawing := true;
      penDrawingRight := rightBtn;
      result := StartDrawing(toolDest,ptF,rightBtn);
      penOrigin := ptF;
    end;
  end else
    result := EmptyRect;
end;

function TToolPen.DoToolMove(toolDest: TBGRABitmap; pt: TPoint; ptF: TPointF): TRect;
begin
  if (manager.PenWidth <= 3) and not HintShowed then
  begin
    Manager.ToolPopup(tpmHoldKeySnapToPixel, VK_CONTROL);
    HintShowed:= true;
  end;
  if ssSnap in ShiftState then ptF := PointF(pt.X,pt.Y);
  result := EmptyRect;
  if penDrawing and (sqr(penOrigin.X-ptF.X)+sqr(penOrigin.Y-ptF.Y) >= 0.999) then
  begin
    toolDest.PenStyle := psSolid;
    result := ContinueDrawing(toolDest,penOrigin,ptF,penDrawingRight);
    penOrigin := ptF;
  end else
  if shiftClicking then
    DoToolShiftClick(toolDest,ptF,shiftClickingRight);
end;

function TToolPen.DoToolShiftClick(toolDest: TBGRABitmap; ptF: TPointF;
  rightBtn: boolean): TRect;
var
  c: TBGRAPixel;
begin
  c := toolDest.GetPixel(round(ptF.X),round(ptF.Y));
  if rightBtn then Manager.BackColor := c
    else Manager.ForeColor := c;
  result := EmptyRect;
end;

function TToolPen.ToolUp: TRect;
begin
  if penDrawing then
  begin
    penDrawing:= false;
    penDrawingRight := false;
    ValidateActionPartially;
  end else
  if shiftClicking then
  begin
    shiftClicking := false;
    shiftClickingRight := false;
  end;
  result := EmptyRect;
end;

function TToolPen.GetContextualToolbars: TContextualToolbars;
begin
  Result:= [ctPenFill,ctBackFill,ctPenWidth,ctAliasing];
end;

destructor TToolPen.Destroy;
begin
  if penDrawing then ValidateAction;
  inherited Destroy;
end;

{ TToolColorPicker }

function TToolColorPicker.DoToolDown(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF; rightBtn: boolean): TRect;
begin
  result := EmptyRect;
  if not colorpicking then
  begin
    colorpicking := true;
    colorpickingRight := rightBtn;
    DoToolMove(toolDest,pt,ptF);
  end;
end;

function TToolColorPicker.DoToolMove(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF): TRect;
var
  c: TBGRAPixel;
begin
  result := EmptyRect;
  if colorpicking then
  begin
    if (pt.X >= 0) and (pt.Y >= 0) and (pt.X < toolDest.Width) and (pt.Y < toolDest.Height) then
    begin
      if ssShift in ShiftState then
        c := Manager.Image.RenderedImage.GetPixel(pt.X,pt.Y)
        else c := toolDest.GetPixel(pt.X,pt.Y);
      if colorpickingRight then
      begin
        Manager.BackColor := c;
        Manager.QueryColorTarget(Manager.BackFill);
      end else
      begin
        Manager.ForeColor := c;
        Manager.QueryColorTarget(Manager.ForeFill);
      end;
    end;
  end;
end;

function TToolColorPicker.FixLayerOffset: boolean;
begin
  Result:= not (ssShift in ShiftState);
end;

function TToolColorPicker.ToolUp: TRect;
begin
  Result:= EmptyRect;
  colorpicking := false;
end;

function TToolColorPicker.GetContextualToolbars: TContextualToolbars;
begin
  Result:= [ctPenFill,ctBackFill];
end;

{ TToolHand }

function TToolHand.FixSelectionTransform: boolean;
begin
  Result:= false;
end;

function TToolHand.FixLayerOffset: boolean;
begin
  Result:= false;
end;

function TToolHand.DoToolDown(toolDest: TBGRABitmap; pt: TPoint; ptF: TPointF;
  rightBtn: boolean): TRect;
begin
  result := EmptyRect;
  if not handMoving then
  begin
    handMoving := true;
    handOriginF := ptF;
  end;
end;

function TToolHand.DoToolMove(toolDest: TBGRABitmap; pt: TPoint; ptF: TPointF): TRect;
var
  newOfs: TPoint;
begin
  result := EmptyRect;
  if handMoving then
  begin
    newOfs := Point(Manager.Image.ImageOffset.X+round(ptF.X-HandOriginF.X),
                    Manager.Image.ImageOffset.Y+round(ptF.Y-HandOriginF.Y));
    if newOfs <> Manager.Image.ImageOffset then
    begin
      Manager.Image.ImageOffset := newOfs;
      result := OnlyRenderChange;
    end;
  end;
end;

function TToolHand.GetStatusText: string;
var w,h,i,j: integer;
  smallestNum, smallestDenom: integer;
begin
  w := Manager.Image.Width;
  h := Manager.Image.Height;
  Result:= rsCanvasSize + ' = ' + inttostr(w) + ' x ' + inttostr(h);
  if h > 0 then
  begin
    result += '|Δx/Δy = ' + FloatToStrF(w/h, ffFixed, 6, 2);
    smallestNum := 0;
    smallestDenom := 0;
    for i := 2 to 9 do
      for j := i+1 to i*2-1 do
        if j mod i <> 0 then
          if w*j = h*i then
          begin
            if (smallestNum = 0) or (i+j < smallestNum+smallestDenom) then
            begin
              smallestNum:= i;
              smallestDenom := j;
            end;
          end else
          if w*i = h*j then
          begin
            if (smallestNum = 0) or (i+j < smallestNum+smallestDenom) then
            begin
              smallestNum:= j;
              smallestDenom := i;
            end;
          end;
    if (smallestNum <> 0) then
      result += ' = ' + inttostr(smallestNum)+'/'+inttostr(smallestDenom);
  end;
end;

constructor TToolHand.Create(AManager: TToolManager);
begin
  inherited Create(AManager);
  handMoving := false;
end;

function TToolHand.ToolUp: TRect;
begin
  handMoving := false;
  result := EmptyRect;
end;

initialization

  RegisterTool(ptHand,TToolHand);
  RegisterTool(ptColorPicker,TToolColorPicker);
  RegisterTool(ptPen,TToolPen);
  RegisterTool(ptEraser,TToolErase);
end.

