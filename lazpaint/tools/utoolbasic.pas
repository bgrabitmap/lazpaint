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
  public
    function ToolUp: TRect; override;
    function GetContextualToolbars: TContextualToolbars; override;
  end;

  { TToolPen }

  TToolPen = class(TGenericTool)
  protected
    class var HintShowed: boolean;
    penDrawing: boolean;
    penOrigin: TPointF;
    penColor: TBGRAPixel;
    snapToPixel: boolean;
    function GetIsSelectingTool: boolean; override;
    function StartDrawing(toolDest: TBGRABitmap; ptF: TPointF; rightBtn: boolean): TRect; virtual;
    function ContinueDrawing(toolDest: TBGRABitmap; originF, destF: TPointF): TRect; virtual;
    function DoToolDown(toolDest: TBGRABitmap; pt: TPoint; ptF: TPointF; rightBtn: boolean): TRect; override;
    function DoToolMove(toolDest: TBGRABitmap; pt: TPoint; ptF: TPointF): TRect; override;
  public
    constructor Create(AManager: TToolManager); override;
    function ToolKeyDown(var key: Word): TRect; override;
    function ToolKeyUp(var key: Word): TRect; override;
    function ToolUp: TRect; override;
    function GetContextualToolbars: TContextualToolbars; override;
    destructor Destroy; override;
  end;

  { TToolErase }

  TToolErase = class(TToolPen)
  protected
    procedure ApplySoften(var image: TBGRABitmap);
    function BlurRadius: single;
    function StartDrawing(toolDest: TBGRABitmap; ptF: TPointF; {%H-}rightBtn: boolean): TRect; override;
    function ContinueDrawing(toolDest: TBGRABitmap; originF, destF: TPointF): TRect; override;
  public
    function GetContextualToolbars: TContextualToolbars; override;
  end;


implementation

uses Types, Graphics, ugraph, Controls, LazPaintType,
  UResourceStrings, BGRAPen;

{ TToolErase }

procedure TToolErase.ApplySoften(var image: TBGRABitmap);
var
  radius: single;
begin
  radius := BlurRadius;
  if radius < 2.5 then
    BGRAReplace(image, image.FilterBlurRadial(round(radius*10),rbPrecise)) else
    BGRAReplace(image, image.FilterBlurRadial(round(radius),rbFast));
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
  if Manager.EraserMode = emSoften then
  begin
    result := GetShapeBounds([ptF],Manager.PenWidth+BlurRadius);
    if IntersectRect(result, result, rect(0,0,toolDest.width,toolDest.height)) then
    begin
      areaCopy := toolDest.GetPart(result) as TBGRABitmap;
      ApplySoften(areaCopy);
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
      mask.ScanOffset := Point(-result.left,-result.top);
      areaCopy.ScanOffset := Point(-result.left,-result.top);
      toolDest.CrossFade(result,toolDest,areaCopy,mask,dmSet);
      mask.Free;
      areaCopy.Free;
    end;
  end else
  begin
    if (snapToPixel or Manager.ShapeOptionAliasing) and (Manager.PenWidth = 1) then
    begin
      ix := round(ptF.X);
      iy := round(ptF.Y);
      toolDest.ErasePixel(ix,iy,round(Manager.EraserAlpha*Manager.ToolPressure));
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
        toolDest.EraseEllipseInRect(r,round(Manager.EraserAlpha*Manager.ToolPressure));
      end else
        toolDest.EraseEllipseAntialias(ptF.X,ptF.Y,Manager.PenWidth/2,Manager.PenWidth/2,round(Manager.EraserAlpha*Manager.ToolPressure));
      toolDest.NoClip;
    end;
  end;
end;

function TToolErase.ContinueDrawing(toolDest: TBGRABitmap; originF,
  destF: TPointF): TRect;
var areaCopy, mask: TBGRABitmap;
  pts: ArrayOfTPointF;
begin
  if Manager.EraserMode = emSoften then
  begin
    result := GetShapeBounds([destF,originF],Manager.PenWidth+BlurRadius);
    if IntersectRect(result, result, rect(0,0,toolDest.width,toolDest.height)) then
    begin
      areaCopy := toolDest.GetPart(result) as TBGRABitmap;
      ApplySoften(areaCopy);
      mask := TBGRABitmap.Create(result.Right-result.left,result.bottom-result.top, BGRABlack);
      mask.LinearAntialiasing := true;
      if Manager.ShapeOptionAliasing then
      begin
        pts := toolDest.Pen.ComputePolyline(
          [PointF(destF.X-result.left,destF.Y-result.top),
           PointF(originF.X-result.left,originF.Y-result.top)],
           Manager.PenWidth,BGRAPixelTransparent,False);
        mask.FillPoly(pts,BGRAWhite);
      end else
        mask.DrawLineAntialias(destF.X-result.left,destF.Y-result.top,originF.X-result.left,originF.Y-result.top,
          Manager.ApplyPressure(BGRAWhite),
          Manager.PenWidth,false);
      mask.ScanOffset := Point(-result.left,-result.top);
      areaCopy.ScanOffset := Point(-result.left,-result.top);
      toolDest.CrossFade(result,toolDest,areaCopy,mask,dmSet);
      mask.Free;
      areaCopy.Free;
    end;
  end else
  if Manager.ShapeOptionAliasing then
  begin
    if Manager.PenWidth = 1 then
    begin
      toolDest.EraseLine(round(destF.X),round(destF.Y),round(originF.X),round(originF.Y),round(Manager.EraserAlpha*Manager.ToolPressure),false);
      result := GetShapeBounds([destF,originF],1);
    end else
    begin
      pts := toolDest.Pen.ComputePolyline([PointF(destF.X,destF.Y),PointF(originF.X,originF.Y)],Manager.PenWidth,BGRAPixelTransparent,False);
      toolDest.ErasePoly(pts, round(Manager.EraserAlpha*Manager.ToolPressure));
      result := GetShapeBounds([destF,originF],Manager.PenWidth);
    end;
  end else
  begin
    if snapToPixel and (Manager.PenWidth = 1) then
    begin
      toolDest.EraseLineAntialias(round(destF.X),round(destF.Y),round(originF.X),round(originF.Y),round(Manager.EraserAlpha*Manager.ToolPressure),false);
      result := GetShapeBounds([destF,originF],1);
    end else
    begin
      toolDest.EraseLineAntialias(destF.X,destF.Y,originF.X,originF.Y,round(Manager.EraserAlpha*Manager.ToolPressure),Manager.PenWidth,False);
      result := GetShapeBounds([destF,originF],Manager.PenWidth);
    end;
  end;
end;

function TToolErase.GetContextualToolbars: TContextualToolbars;
begin
  Result:= [ctPenWidth,ctEraserOption,ctAliasing];
end;

{ TToolPen }

function TToolPen.GetIsSelectingTool: boolean;
begin
  Result:= false;
end;

function TToolPen.StartDrawing(toolDest: TBGRABitmap; ptF: TPointF;
  rightBtn: boolean): TRect;
var ix,iy: integer;
  r: TRect;
begin
  if rightBtn then penColor := Manager.BackColor else penColor := Manager.ForeColor;
  if (snapToPixel or Manager.ShapeOptionAliasing) and (Manager.PenWidth = 1) and (Manager.GetTexture = nil) then
  begin
    ix := round(ptF.X);
    iy := round(ptF.Y);
    toolDest.DrawPixel(ix,iy,Manager.ApplyPressure(penColor));
    result := rect(ix,iy,ix+1,iy+1);
  end else
  begin
    result := GetShapeBounds([ptF],Manager.PenWidth);
    toolDest.ClipRect := result;
    if Manager.ShapeOptionAliasing then
    begin
      r := rect(round(ptF.X-Manager.PenWidth/2+0.5),round(ptF.Y-Manager.PenWidth/2+0.5),
                round(ptF.X+Manager.PenWidth/2+0.5),round(ptF.Y+Manager.PenWidth/2+0.5));
      if Manager.GetTextureAfterAlpha <> nil then
        toolDest.FillEllipseInRect(r,Manager.GetTextureAfterAlpha,dmDrawWithTransparency)
      else
        toolDest.FillEllipseInRect(r,Manager.ApplyPressure(penColor),dmDrawWithTransparency);
    end
    else
    begin
      if Manager.GetTextureAfterAlpha <> nil then
        toolDest.FillEllipseAntialias(ptF.X,ptF.Y,Manager.PenWidth/2,Manager.PenWidth/2,Manager.GetTextureAfterAlpha)
      else
        toolDest.FillEllipseAntialias(ptF.X,ptF.Y,Manager.PenWidth/2,Manager.PenWidth/2,Manager.ApplyPressure(penColor));
    end;
    toolDest.NoClip;
  end;
end;

function TToolPen.ContinueDrawing(toolDest: TBGRABitmap; originF, destF: TPointF): TRect;
var
  pts: ArrayOfTPointF;
begin
  if (snapToPixel or Manager.ShapeOptionAliasing) and (Manager.PenWidth = 1) and (Manager.GetTexture = nil) then
  begin
    if Manager.ShapeOptionAliasing then
      toolDest.DrawLine(round(destF.X),round(destF.Y),round(originF.X),round(originF.Y),Manager.ApplyPressure(penColor),false)
    else
      toolDest.DrawLineAntialias(round(destF.X),round(destF.Y),round(originF.X),round(originF.Y),Manager.ApplyPressure(penColor),false);
    result := GetShapeBounds([destF,originF],1);
  end else
  begin
    result := GetShapeBounds([destF,originF],Manager.PenWidth+1);
    toolDest.ClipRect := result;
    if Manager.ShapeOptionAliasing then
    begin
      pts := toolDest.Pen.ComputePolyline([PointF(destF.X,destF.Y),PointF(originF.X,originF.Y)],Manager.PenWidth,BGRAPixelTransparent,False);
      if Manager.GetTextureAfterAlpha <> nil then
        toolDest.FillPoly(pts,Manager.GetTextureAfterAlpha,dmDrawWithTransparency)
      else
        toolDest.FillPoly(pts,Manager.ApplyPressure(penColor),dmDrawWithTransparency);
    end else
    begin
      if Manager.GetTextureAfterAlpha <> nil then
        toolDest.DrawLineAntialias(destF.X,destF.Y,originF.X,originF.Y,Manager.GetTextureAfterAlpha,Manager.PenWidth,False)
      else
        toolDest.DrawLineAntialias(destF.X,destF.Y,originF.X,originF.Y,Manager.ApplyPressure(penColor),Manager.PenWidth,False);
    end;
    toolDest.NoClip;
  end;
end;

function TToolPen.DoToolDown(toolDest: TBGRABitmap; pt: TPoint; ptF: TPointF;
  rightBtn: boolean): TRect;
begin
  if snapToPixel then ptF := PointF(pt.X,pt.Y);
  if not penDrawing then
  begin
    toolDest.PenStyle := psSolid;
    penDrawing := true;
    result := StartDrawing(toolDest,ptF,rightBtn);
    penOrigin := ptF;
  end else
    result := EmptyRect;
end;

function TToolPen.DoToolMove(toolDest: TBGRABitmap; pt: TPoint; ptF: TPointF): TRect;
begin
  if (manager.PenWidth <= 3) and not HintShowed then
  begin
    Manager.ToolPopup(tpmHoldKeySnapToPixel, VK_SNAP);
    HintShowed:= true;
  end;
  if snapToPixel then ptF := PointF(pt.X,pt.Y);
  result := EmptyRect;
  if penDrawing and (sqr(penOrigin.X-ptF.X)+sqr(penOrigin.Y-ptF.Y) >= 0.999) then
  begin
    toolDest.PenStyle := psSolid;
    result := ContinueDrawing(toolDest,penOrigin,ptF);
    penOrigin := ptF;
  end;
end;

constructor TToolPen.Create(AManager: TToolManager);
begin
  inherited Create(AManager);
  snapToPixel:= false;
end;

function TToolPen.ToolKeyDown(var key: Word): TRect;
begin
  if key = VK_SNAP then
  begin
    snapToPixel := true;
    Key := 0;
  end;
  Result:=EmptyRect;
end;

function TToolPen.ToolKeyUp(var key: Word): TRect;
begin
  if key = VK_SNAP then
  begin
    snapToPixel := false;
    key := 0;
  end;
  Result:=EmptyRect;
end;

function TToolPen.ToolUp: TRect;
begin
  if penDrawing then
  begin
    penDrawing:= false;
    ValidateActionPartially;
  end;
  result := EmptyRect;
end;

function TToolPen.GetContextualToolbars: TContextualToolbars;
begin
  Result:= [ctColor,ctTexture,ctPenWidth,ctAliasing];
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
begin
  result := EmptyRect;
  if colorpicking then
  begin
    if (pt.X >= 0) and (pt.Y >= 0) and (pt.X < toolDest.Width) and (pt.Y < toolDest.Height) then
    begin
      if colorpickingRight then Manager.BackColor := toolDest.GetPixel(pt.X,pt.Y) else
        Manager.ForeColor := toolDest.GetPixel(pt.X,pt.Y);
    end;
  end;
end;

function TToolColorPicker.ToolUp: TRect;
begin
  Result:= EmptyRect;
  colorpicking := false;
end;

function TToolColorPicker.GetContextualToolbars: TContextualToolbars;
begin
  Result:= [ctColor];
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

