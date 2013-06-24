unit UToolBasic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, utool, BGRABitmapTypes, BGRABitmap, uimage;

type

  { TToolHand }

  TToolHand = class(TGenericTool)
  protected
    handMoving: boolean;
    handOrigin: TPoint;
    function GetAction: TLayerAction; override;
    function GetIsSelectingTool: boolean; override;
    function DoToolDown({%H-}toolDest: TBGRABitmap; pt: TPoint; {%H-}ptF: TPointF;
      {%H-}rightBtn: boolean): TRect; override;
    function DoToolMove({%H-}toolDest: TBGRABitmap; pt: TPoint; {%H-}ptF: TPointF): TRect; override;
    procedure DoToolMoveAfter(pt: TPoint; {%H-}ptF: TPointF); override;
  public
    function GetToolDrawingLayer: TBGRABitmap; override;
    function ToolUp: TRect; override;
  end;

  { TToolColorPicker }

  TToolColorPicker = class(TGenericTool)
  protected
    colorpicking,colorpickingRight: boolean;
    function GetIsSelectingTool: boolean; override;
    function DoToolDown(toolDest: TBGRABitmap; pt: TPoint; ptF: TPointF;
      rightBtn: boolean): TRect; override;
    function DoToolMove(toolDest: TBGRABitmap; pt: TPoint; {%H-}ptF: TPointF): TRect; override;
  public
    function ToolUp: TRect; override;
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
    function ToolKeyDown(key: Word): TRect; override;
    function ToolKeyUp(key: Word): TRect; override;
    function ToolUp: TRect; override;
    destructor Destroy; override;
  end;

  { TToolErase }

  TToolErase = class(TToolPen)
  protected
    function StartDrawing(toolDest: TBGRABitmap; ptF: TPointF; {%H-}rightBtn: boolean): TRect; override;
    function ContinueDrawing(toolDest: TBGRABitmap; originF, destF: TPointF): TRect; override;
  end;

  TRectangularBorderTest = set of (btOriginX,btOriginY,btDestX,btDestY);

  { TToolRectangular }

  TToolRectangular = class(TGenericTool)
  protected
    class var HintShowed: boolean;
    swapedColor: boolean;
    rectDrawing,afterRectDrawing: boolean;
    rectOrigin, rectDest: TPoint;
    previousRect: TRect;
    rectMovingPoint: boolean;
    rectMovingPointValueDiff: TPoint;
    rectMovingPointClick: TPoint;
    rectMovingBorderTest: TRectangularBorderTest;
    rectMovingX,rectMovingY: PInteger;
    lastMousePos: TPointF;
    squareConstraint: boolean;
    function GetFillColor: TBGRAPixel; virtual;
    function GetPenColor: TBGRAPixel; virtual;
    function GetIsSelectingTool: boolean; override;
    function BorderTest(ptF: TPointF): TRectangularBorderTest; virtual;
    function DoToolDown({%H-}toolDest: TBGRABitmap; pt: TPoint; {%H-}ptF: TPointF; rightBtn: boolean): TRect; override;
    function DoToolMove(toolDest: TBGRABitmap; pt: TPoint; {%H-}ptF: TPointF): TRect; override;
    function UpdateShape(toolDest: TBGRABitmap): TRect; virtual; abstract;
    function FinishShape(toolDest: TBGRABitmap): TRect; virtual; abstract;
    procedure PrepareDrawing(rightBtn: boolean); virtual;
    function ValidateDrawing: TRect;
    procedure ClearShape;
    function ShouldFinishShapeWhenFirstMouseUp: boolean; virtual; abstract;
    function RenderAllCornerPositions: boolean; virtual;
    procedure UpdateCursor(ptF: TPointF); virtual;
    function DoToolUpdate(toolDest: TBGRABitmap): TRect; override;
    procedure ApplyConstraint(px,py: PInteger);
  public
    constructor Create(AManager: TToolManager); override;
    function ToolUp: TRect; override;
    function ToolKeyDown(key: Word): TRect; override;
    function ToolKeyUp(key: Word): TRect; override;
    procedure Render(VirtualScreen: TBGRABitmap;
      BitmapToVirtualScreen: TBitmapToVirtualScreenFunction); override;
    destructor Destroy; override;
    property PenColor: TBGRAPixel read GetPenColor;
    property FillColor: TBGRAPixel read GetFillColor;
  end;

  { TToolRectangle }

  TToolRectangle = class(TToolRectangular)
  protected
    function UpdateShape(toolDest: TBGRABitmap): TRect; override;
    function FinishShape(toolDest: TBGRABitmap): TRect;  override;
    function ShouldFinishShapeWhenFirstMouseUp: boolean; override;
  end;

  { TToolEllipse }

  TToolEllipse = class(TToolRectangle)
  protected
    circleConstraint: boolean;
    function BorderTest(ptF: TPointF): TRectangularBorderTest; override;
    function UpdateShape(toolDest: TBGRABitmap): TRect; override;
  end;

implementation

uses BGRAPolygon, Graphics, LCLType, ugraph, Controls, math;

{ TToolEllipse }

function TToolEllipse.BorderTest(ptF: TPointF): TRectangularBorderTest;
begin
  Result:=inherited BorderTest(ptF);
  if (result = [btOriginY,btOriginX]) or (result = [btDestY,btDestX]) then exit; //ok
  if (result = [btDestX,btOriginY]) then result := [btDestX] else
  if (result = [btDestY,btOriginX]) then result := [btDestY] else
    result := [];
end;

function TToolEllipse.UpdateShape(toolDest: TBGRABitmap): TRect;
var
  multi: TBGRAMultishapeFiller;
  rx,ry: single;
  pts: array of TPointF;
begin
   ClearShape;
   multi := TBGRAMultishapeFiller.Create;
   rx := abs(rectDest.X-rectOrigin.X);
   ry := abs(rectDest.Y-rectOrigin.Y);

   if Manager.ToolOptionDrawShape then
   begin
     result := GetShapeBounds([PointF(rectOrigin.x-rx,rectOrigin.y-ry),PointF(rectOrigin.x+rx,rectOrigin.y+ry)],Manager.ToolPenWidth);
     if Manager.ToolPenStyle = psSolid then
     begin
       if not Manager.ToolOptionFillShape and (Manager.GetToolTextureAfterAlpha <> nil) then
         multi.AddEllipseBorder(rectOrigin.X,rectOrigin.Y,rx,ry,Manager.ToolPenWidth,Manager.GetToolTextureAfterAlpha) else
         multi.AddEllipseBorder(rectOrigin.X,rectOrigin.Y,rx,ry,Manager.ToolPenWidth,penColor);
     end else
     begin
        with toolDest do
          pts := ComputeEllipseContour(rectOrigin.X,rectOrigin.Y,rx,ry);

        toolDest.JoinStyle := pjsRound;
        if not Manager.ToolOptionFillShape and (Manager.GetToolTextureAfterAlpha <> nil) then
          toolDest.DrawPolygonAntialias(pts,Manager.GetToolTextureAfterAlpha,Manager.ToolPenWidth)
        else
          toolDest.DrawPolygonAntialias(pts,penColor,Manager.ToolPenWidth);
     end;

     rx -= Manager.ToolPenWidth/2;
     ry -= Manager.ToolPenWidth/2;
   end else
   begin
     rx += 0.5;
     ry += 0.5;
     result := GetShapeBounds([PointF(rectOrigin.x-rx,rectOrigin.y-ry),PointF(rectOrigin.x+rx,rectOrigin.y+ry)],1);
   end;
   if Manager.ToolOptionFillShape then
     if (rx>0) and (ry>0) then
     begin
       if not Manager.ToolOptionDrawShape and (Manager.GetToolTextureAfterAlpha <> nil) then
         multi.AddEllipse(rectOrigin.X,rectOrigin.Y,rx,ry,Manager.GetToolTextureAfterAlpha)
       else
         multi.AddEllipse(rectOrigin.X,rectOrigin.Y,rx,ry,fillColor);
     end;
   multi.Draw(toolDest);
   multi.Free;
end;

{ TToolRectangle }

function TToolRectangle.UpdateShape(toolDest: TBGRABitmap): TRect;
var sx,sy: integer;
begin
  ClearShape;
  if Manager.ToolOptionFillShape and not Manager.ToolOptionDrawShape then
  begin
    if rectDest.X > rectOrigin.X then sx := 1 else sx := -1;
    if rectDest.Y > rectOrigin.Y then sy := 1 else sy := -1;
    result := GetShapeBounds([PointF(rectOrigin.x,rectOrigin.y),PointF(rectDest.x,rectDest.y)],1);
    if Manager.GetToolTextureAfterAlpha <> nil then
      toolDest.FillRectAntialias(rectOrigin.X-0.5*sx,rectOrigin.Y-0.5*sy,rectDest.X+0.5*sx,rectDest.Y+0.5*sy,Manager.GetToolTextureAfterAlpha) else
      toolDest.FillRectAntialias(rectOrigin.X-0.5*sx,rectOrigin.Y-0.5*sy,rectDest.X+0.5*sx,rectDest.Y+0.5*sy,fillColor);
  end else
  if Manager.ToolOptionDrawShape and not Manager.ToolOptionFillShape then
  begin
    result := GetShapeBounds([PointF(rectOrigin.x,rectOrigin.y),PointF(rectDest.x,rectDest.y)],Manager.ToolPenWidth);
    if Manager.GetToolTextureAfterAlpha <> nil then
      toolDest.RectangleAntialias(rectOrigin.X,rectOrigin.Y,rectDest.X,rectDest.Y,Manager.GetToolTextureAfterAlpha,Manager.ToolPenWidth) else
      toolDest.RectangleAntialias(rectOrigin.X,rectOrigin.Y,rectDest.X,rectDest.Y,penColor,Manager.ToolPenWidth)
  end else
  if Manager.ToolOptionDrawShape and Manager.ToolOptionFillShape then
  begin
    result := GetShapeBounds([PointF(rectOrigin.x,rectOrigin.y),PointF(rectDest.x,rectDest.y)],Manager.ToolPenWidth);
    toolDest.RectangleAntialias(rectOrigin.X,rectOrigin.Y,rectDest.X,rectDest.Y,penColor,Manager.ToolPenWidth,fillColor);
  end else
    result := EmptyRect;
end;

function TToolRectangle.FinishShape(toolDest: TBGRABitmap): TRect;
begin
  result := UpdateShape(toolDest);
end;

function TToolRectangle.ShouldFinishShapeWhenFirstMouseUp: boolean;
begin
  result := false;
end;

{ TToolRectangular }

function TToolRectangular.GetFillColor: TBGRAPixel;
begin
  if swapedColor then
    result := Manager.ToolForeColor
  else
    result := Manager.ToolBackColor;
end;

function TToolRectangular.GetPenColor: TBGRAPixel;
begin
  if swapedColor then
    result := Manager.ToolBackColor
  else
    result := Manager.ToolForeColor;
end;

function TToolRectangular.GetIsSelectingTool: boolean;
begin
  Result:= false;
end;

function TToolRectangular.BorderTest(ptF: TPointF): TRectangularBorderTest;
var maxDist,d1,d2: single;
begin
   maxDist := SelectionMaxPointDistance;
   result := [];
   if not RenderAllCornerPositions then
   begin
       d1 := sqr(ptF.X-rectOrigin.X) + sqr(ptF.Y-rectOrigin.Y);
       d2 := sqr(ptF.X-rectDest.X) + sqr(ptF.Y-rectDest.Y);
       if (d1 <= sqr(maxDist)) and (d1 <= d2) then
         result := [btOriginX,btOriginY] else
       if (d2 <= sqr(maxDist)) and (d2 <= d1) then
         result := [btDestX,btDestY];
       if result <> [] then exit;
   end;

   if (((ptF.X >= rectOrigin.X - maxDist) and (ptF.X <= rectDest.X + maxDist)) or
      ((ptF.X >= rectDest.X - maxDist) and (ptF.X <= rectOrigin.X + maxDist))) and
   (((ptF.Y >= rectOrigin.Y - maxDist) and (ptF.Y <= rectDest.Y + maxDist)) or
      ((ptF.Y >= rectDest.Y - maxDist) and (ptF.Y <= rectOrigin.Y + maxDist))) then
   begin
     if abs(ptF.x-rectOrigin.X) < abs(ptF.x-rectDest.X) then
     begin
       if abs(ptF.X - rectOrigin.X) < maxDist then
         result += [btOriginX];
     end else
     begin
        if abs(ptF.X - rectDest.X) < maxDist then
          result += [btDestX];
     end;
     if abs(ptF.y-rectOrigin.y) < abs(ptF.y-rectDest.y) then
     begin
       if abs(ptF.y - rectOrigin.y) < maxDist then
         result += [btOriginY];
     end else
     begin
       if abs(ptF.Y - rectDest.Y) < maxDist then
         result += [btDestY];
     end;
   end;
end;

function TToolRectangular.DoToolDown(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF; rightBtn: boolean): TRect;
var test: TRectangularBorderTest;
begin
  result := EmptyRect;
  lastMousePos := ptF;
  if afterRectDrawing and not rightBtn then
  begin
    rectMovingPointClick := pt;
    rectMovingPoint := false;
    rectMovingX := nil;
    rectMovingY := nil;
    test := BorderTest(ptF);
    if btOriginX in test then
    begin
        rectMovingX := @rectOrigin.X;
        rectMovingPoint := true;
    end;
    if btDestX in test then
    begin
       rectMovingX := @rectDest.X;
       rectMovingPoint := true;
    end;
    if btOriginY in test then
    begin
      rectMovingY := @rectOrigin.Y;
      rectMovingPoint := true;
    end;
    if btDestY in test then
    begin
      rectMovingY := @rectDest.Y;
      rectMovingPoint := true;
    end;
    if rectMovingPoint then
    begin
      rectMovingPointValueDiff := point(0,0);
      if rectMovingX <> nil then rectMovingPointValueDiff.X := rectMovingX^ - rectMovingPointClick.X;
      if rectMovingY <> nil then rectMovingPointValueDiff.Y := rectMovingY^ - rectMovingPointClick.Y;
      rectMovingBorderTest := test;
      exit;
    end else
      rectMovingBorderTest := [];
  end;
  if not rectDrawing then
  begin
    if afterRectDrawing then
    begin
      ValidateAction;
      afterRectDrawing:= false;
    end;
    rectDrawing := true;
    rectOrigin := pt;
    rectDest := pt;
    PrepareDrawing(rightBtn);
    previousRect := ToolRepaintOnly;
  end;
end;

function TToolRectangular.DoToolMove(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF): TRect;
var currentRect: TRect;
begin
  result := EmptyRect;
  if not HintShowed then
  begin
    Manager.ToolPopup(tpmHoldShiftForSquare);
    HintShowed:= true;
  end;
  lastMousePos := ptF;
  if rectMovingPoint then
  begin
    if Assigned(rectMovingX) then rectMovingX^ := pt.X+rectMovingPointValueDiff.X;
    if Assigned(rectMovingY) then rectMovingY^ := pt.Y+rectMovingPointValueDiff.Y;
    ApplyConstraint(rectMovingX,rectMovingY);
    currentRect := FinishShape(toolDest);
    result := RectUnion(previousRect,currentRect);
    previousRect := currentRect;
  end else
  if rectDrawing and ((rectDest.X <> pt.X) or (rectDest.Y <> pt.Y)) then
  begin
    rectDest := pt;
    ApplyConstraint(@rectDest.X,@rectDest.Y);
    currentRect := UpdateShape(toolDest);
    result := RectUnion(previousRect,currentRect);
    previousRect := currentRect;
  end;
  UpdateCursor(ptF);
end;

procedure TToolRectangular.PrepareDrawing(rightBtn: boolean);
begin
  swapedColor := rightBtn;
end;

function TToolRectangular.ToolUp: TRect;
var currentRect: TRect;
begin
  if rectMovingPoint then
  begin
    rectMovingPoint := false;
    rectMovingX := nil;
    rectMovingY := nil;
    UpdateCursor(lastMousePos);
  end else
  if rectDrawing then
  begin
    currentRect := ValidateDrawing;
    result := RectUnion(previousRect,currentRect);
    previousRect := currentRect;
    afterRectDrawing := true;
    UpdateCursor(lastMousePos);
  end;
end;

function TToolRectangular.ToolKeyDown(key: Word): TRect;
begin
  result := EmptyRect;
  if Key = VK_SHIFT then
  begin
    squareConstraint:= true;
  end else
  if (Key = VK_RETURN) and afterRectDrawing then
  begin
    ValidateAction;
    afterRectDrawing:= false;
    result := ToolRepaintOnly;
    Cursor := crDefault;
  end;
end;

function TToolRectangular.ToolKeyUp(key: Word): TRect;
begin
  if Key = VK_SHIFT then
    squareConstraint:= false;
  result := EmptyRect;
end;

procedure TToolRectangular.Render(VirtualScreen: TBGRABitmap;
  BitmapToVirtualScreen: TBitmapToVirtualScreenFunction);
var curPt: TPointF;
begin
  if afterRectDrawing then
  begin
    curPt := BitmapToVirtualScreen(PointF(rectOrigin.X,rectOrigin.Y));
    NicePoint(VirtualScreen, curPt.X,curPt.Y);
    curPt := BitmapToVirtualScreen(PointF(rectDest.X,rectDest.Y));
    NicePoint(VirtualScreen, curPt.X,curPt.Y);
    if RenderAllCornerPositions then
    begin
      curPt := BitmapToVirtualScreen(PointF(rectDest.X,rectOrigin.Y));
      NicePoint(VirtualScreen, curPt.X,curPt.Y);
      curPt := BitmapToVirtualScreen(PointF(rectOrigin.X,rectDest.Y));
      NicePoint(VirtualScreen, curPt.X,curPt.Y);
    end;
  end;
end;

function TToolRectangular.ValidateDrawing: TRect;
begin
  if rectDrawing then
  begin
    result := FinishShape(GetToolDrawingLayer);
    rectDrawing := false;
    afterRectDrawing:= true;
  end else
    result := EmptyRect;
end;

procedure TToolRectangular.ClearShape;
begin
  RestoreBackupDrawingLayer;
end;

function TToolRectangular.RenderAllCornerPositions: boolean;
begin
  result := true;
end;

procedure TToolRectangular.UpdateCursor(ptF: TPointF);
var test: TRectangularBorderTest;
begin
  Cursor := crDefault;
  if afterRectDrawing then
  begin
    if rectMovingPoint then
    begin
      test := rectMovingBorderTest;
    end else
      test := BorderTest(ptF);
    if (test = [btOriginX,btOriginY]) or (test = [btDestX,btDestY]) or
      (test = [btOriginX,btDestY]) or (test = [btDestX,btOriginY]) then
    begin
      if (rectOrigin.X > rectDest.X) xor (rectOrigin.Y > rectDest.Y) xor
        ((test = [btOriginX,btDestY]) or (test = [btDestX,btOriginY])) then
        Cursor := crSizeNESW
      else
        Cursor := crSizeNWSE;
    end else
    if (test = [btOriginX]) or (test = [btDestX]) then
      Cursor := crSizeWE
    else
    if (test = [btOriginY]) or (test = [btDestY]) then
      Cursor := crSizeNS;
  end;
end;

function TToolRectangular.DoToolUpdate(toolDest: TBGRABitmap): TRect;
var currentRect: TRect;
begin
  if rectDrawing then
  begin
    currentRect := UpdateShape(toolDest);
    result := RectUnion(previousRect,currentRect);
    previousRect := currentRect;
  end else
  if afterRectDrawing then
  begin
    currentRect := FinishShape(toolDest);
    result := RectUnion(previousRect,currentRect);
    previousRect := currentRect;
  end
  else
    result := EmptyRect;
end;

procedure TToolRectangular.ApplyConstraint(px, py: PInteger);
var a: integer;
  px2,py2: PInteger;
begin
  if squareConstraint then
  begin
    if px <> nil then
      a := abs(rectDest.X-rectOrigin.X) else a := 0;
    if py <> nil then
      a := max(a,abs(rectDest.Y-rectOrigin.Y));

    if px = nil then
    begin
      if py = @rectDest.Y then px := @rectDest.X;
      if py = @rectOrigin.Y then px := @rectOrigin.X;
    end;
    if py = nil then
    begin
      if px = @rectDest.X then py := @rectDest.Y;
      if px = @rectOrigin.X then py := @rectOrigin.Y;
    end;

    if py = @rectOrigin.Y then py2 := @rectDest.Y else
    if py = @rectDest.Y then py2 := @rectOrigin.Y else py2 := nil;
    if px = @rectOrigin.X then px2 := @rectDest.X else
    if px = @rectDest.X then px2 := @rectOrigin.X else px2 := nil;

    if (px <> nil) and (px2 <> nil) then
    begin
      if px^ >= px2^ then px^ := px2^+a else px^ := px2^-a;
    end;
    if (py <> nil) and (py2 <> nil) then
    begin
      if py^ >= py2^ then py^ := py2^+a else py^ := py2^-a;
    end;
  end;
end;

constructor TToolRectangular.Create(AManager: TToolManager);
begin
  inherited Create(AManager);
  rectMovingPoint := false;
  afterRectDrawing:= false;
  previousRect := EmptyRect;
  squareConstraint := false;
end;

destructor TToolRectangular.Destroy;
begin
  if afterRectDrawing then ValidateAction;
  inherited Destroy;
end;

{ TToolErase }

function TToolErase.StartDrawing(toolDest: TBGRABitmap; ptF: TPointF;
  rightBtn: boolean): TRect;
var ix,iy: integer;
begin
  if snapToPixel and (Manager.ToolPenWidth = 1) then
  begin
    ix := round(ptF.X);
    iy := round(ptF.Y);
    toolDest.ErasePixel(ix,iy,Manager.ToolEraserAlpha);
    result := rect(ix,iy,ix+1,iy+1);
  end
  else
  begin
    toolDest.EraseLineAntialias(ptF.X,ptF.Y,ptF.X,ptF.Y,Manager.ToolEraserAlpha,Manager.ToolPenWidth,True);
    result := GetShapeBounds([ptF],Manager.ToolPenWidth);
  end;
end;

function TToolErase.ContinueDrawing(toolDest: TBGRABitmap; originF,
  destF: TPointF): TRect;
begin
  if snapToPixel and (Manager.ToolPenWidth = 1) then
  begin
    toolDest.EraseLineAntialias(round(destF.X),round(destF.Y),round(originF.X),round(originF.Y),Manager.ToolEraserAlpha,false);
    result := GetShapeBounds([destF,originF],1);
  end else
  begin
    toolDest.EraseLineAntialias(destF.X,destF.Y,originF.X,originF.Y,Manager.ToolEraserAlpha,Manager.ToolPenWidth,False);
    result := GetShapeBounds([destF,originF],Manager.ToolPenWidth);
  end;
end;

{ TToolPen }

function TToolPen.GetIsSelectingTool: boolean;
begin
  Result:= false;
end;

function TToolPen.StartDrawing(toolDest: TBGRABitmap; ptF: TPointF;
  rightBtn: boolean): TRect;
var ix,iy: integer;
begin
  if rightBtn then penColor := Manager.ToolBackColor else penColor := Manager.ToolForeColor;
  if snapToPixel and (Manager.ToolPenWidth = 1) and (Manager.GetToolTexture = nil) then
  begin
    ix := round(ptF.X);
    iy := round(ptF.Y);
    toolDest.DrawPixel(ix,iy,penColor);
    result := rect(ix,iy,ix+1,iy+1);
  end else
  begin
     if Manager.GetToolTextureAfterAlpha <> nil then
       toolDest.FillEllipseAntialias(ptF.X,ptF.Y,Manager.ToolPenWidth/2,Manager.ToolPenWidth/2,Manager.GetToolTextureAfterAlpha)
     else
       toolDest.FillEllipseAntialias(ptF.X,ptF.Y,Manager.ToolPenWidth/2,Manager.ToolPenWidth/2,penColor);
     result := GetShapeBounds([ptF],Manager.ToolPenWidth);
  end;
end;

function TToolPen.ContinueDrawing(toolDest: TBGRABitmap; originF, destF: TPointF): TRect;
begin
  if snapToPixel and (Manager.ToolPenWidth = 1) and (Manager.GetToolTexture = nil) then
  begin
    toolDest.DrawLineAntialias(round(destF.X),round(destF.Y),round(originF.X),round(originF.Y),penColor,false);
    result := GetShapeBounds([destF,originF],1);
  end else
  begin
     if Manager.GetToolTextureAfterAlpha <> nil then
       toolDest.DrawLineAntialias(destF.X,destF.Y,originF.X,originF.Y,Manager.GetToolTextureAfterAlpha,Manager.ToolPenWidth,False)
     else
       toolDest.DrawLineAntialias(destF.X,destF.Y,originF.X,originF.Y,penColor,Manager.ToolPenWidth,False);
     result := GetShapeBounds([destF,originF],Manager.ToolPenWidth+1);
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

function TToolPen.DoToolMove(toolDest: TBGRABitmap; pt: TPoint; ptF: TPointF
  ): TRect;
begin
  if (manager.ToolPenWidth <= 3) and not HintShowed then
  begin
    Manager.ToolPopup(tpmHoldCtrlSnapToPixel);
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
end;

function TToolPen.ToolKeyDown(key: Word): TRect;
begin
  if key = VK_CONTROL then snapToPixel := true;
  Result:=EmptyRect;
end;

function TToolPen.ToolKeyUp(key: Word): TRect;
begin
  if key = VK_CONTROL then snapToPixel := false;
  Result:=EmptyRect;
end;

function TToolPen.ToolUp: TRect;
begin
  if penDrawing then
  begin
    penDrawing:= false;
    ValidateAction;
  end;
  result := EmptyRect;
end;

destructor TToolPen.Destroy;
begin
  if penDrawing then ValidateAction;
  inherited Destroy;
end;

{ TToolColorPicker }

function TToolColorPicker.GetIsSelectingTool: boolean;
begin
  Result:=false;
end;

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
      if colorpickingRight then Manager.ToolBackColor := toolDest.GetPixel(pt.X,pt.Y) else
        Manager.ToolForeColor := toolDest.GetPixel(pt.X,pt.Y);
    end;
  end;
end;

function TToolColorPicker.ToolUp: TRect;
begin
  Result:= EmptyRect;
  colorpicking := false;
end;

{ TToolHand }

function TToolHand.GetAction: TLayerAction;
begin
  Result:=nil;
end;

function TToolHand.GetIsSelectingTool: boolean;
begin
  result := false;
end;

function TToolHand.DoToolDown(toolDest: TBGRABitmap; pt: TPoint; ptF: TPointF;
  rightBtn: boolean): TRect;
begin
  result := EmptyRect;
  if not handMoving then
  begin
    handMoving := true;
    handOrigin := pt;
  end;
end;

function TToolHand.DoToolMove(toolDest: TBGRABitmap; pt: TPoint; ptF: TPointF
  ): TRect;
begin
  if handMoving and ((handOrigin.X <> pt.X) or (handOrigin.Y <> pt.Y)) then
  begin
    Manager.Image.ImageOffset := Point(Manager.Image.ImageOffset.X+pt.X-HandOrigin.X,
                                       Manager.Image.ImageOffset.Y+pt.Y-HandOrigin.Y);
    result := ToolRepaintOnly;
  end else
    result := EmptyRect;
end;

procedure TToolHand.DoToolMoveAfter(pt: TPoint; ptF: TPointF);
begin
  if handMoving then handOrigin := pt;
end;

function TToolHand.GetToolDrawingLayer: TBGRABitmap;
begin
  Result:= Manager.Image.SelectedImageLayerReadOnly;   //do not create a selection layer
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
  RegisterTool(ptRect,TToolRectangle);
  RegisterTool(ptEllipse,TToolEllipse);
end.

