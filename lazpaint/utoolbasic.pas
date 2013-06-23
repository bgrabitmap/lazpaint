unit utoolbasic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, utool, BGRABitmapTypes, BGRABitmap;

type

  { TToolHand }

  TToolHand = class(TGenericTool)
  protected
    handMoving: boolean;
    handOrigin: TPoint;
    function GetIsSelectingTool: boolean; override;
    function DoToolDown({%H-}toolDest: TBGRABitmap; pt: TPoint; {%H-}ptF: TPointF;
      rightBtn: boolean): TRect; override;
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

  { TToolRectangular }

  TToolRectangular = class(TGenericTool)
  protected
    rectDrawing: boolean;
    rectOrigin, rectDest: TPoint;
    rectBackup,rectBackupSource: TBGRABitmap;
    penColor,fillColor : TBGRAPixel;
    previousRect: TRect;
    function GetIsSelectingTool: boolean; override;
    function DoToolDown({%H-}toolDest: TBGRABitmap; pt: TPoint; {%H-}ptF: TPointF; rightBtn: boolean): TRect; override;
    function DoToolMove(toolDest: TBGRABitmap; pt: TPoint; {%H-}ptF: TPointF): TRect; override;
    function UpdateShape(toolDest: TBGRABitmap): TRect; virtual; abstract;
    function FinishShape(toolDest: TBGRABitmap): TRect; virtual; abstract;
    procedure PrepareDrawing(rightBtn: boolean); virtual; abstract;
    function NeedClearShape: boolean; virtual; abstract;
    function ValidateDrawing: TRect;
    procedure ClearShape;
  public
    function ToolUp: TRect; override;
    destructor Destroy; override;
  end;

  { TToolRectangle }

  TToolRectangle = class(TToolRectangular)
  protected
    function UpdateShape(toolDest: TBGRABitmap): TRect; override;
    procedure PrepareDrawing(rightBtn: boolean); override;
    function FinishShape({%H-}toolDest: TBGRABitmap): TRect;  override;
    function NeedClearShape: boolean; override;
  end;

  { TToolEllipse }

  TToolEllipse = class(TToolRectangle)
  protected
    function UpdateShape(toolDest: TBGRABitmap): TRect; override;
  end;

implementation

uses BGRAPolygon, Graphics, LCLType, ugraph;

{ TToolEllipse }

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
       if not Manager.ToolOptionFillShape and (Manager.ToolTexture <> nil) then
         multi.AddEllipseBorder(rectOrigin.X,rectOrigin.Y,rx,ry,Manager.ToolPenWidth,Manager.ToolTexture) else
         multi.AddEllipseBorder(rectOrigin.X,rectOrigin.Y,rx,ry,Manager.ToolPenWidth,penColor);
     end else
     begin
        with toolDest do
          pts := ComputeEllipseContour(rectOrigin.X,rectOrigin.Y,rx,ry);

        toolDest.JoinStyle := pjsRound;
        if not Manager.ToolOptionFillShape and (Manager.ToolTexture <> nil) then
          toolDest.DrawPolygonAntialias(pts,Manager.ToolTexture,Manager.ToolPenWidth)
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
       if not Manager.ToolOptionDrawShape and (Manager.ToolTexture <> nil) then
         multi.AddEllipse(rectOrigin.X,rectOrigin.Y,rx,ry,Manager.ToolTexture)
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
    if Manager.ToolTexture <> nil then
      toolDest.FillRectAntialias(rectOrigin.X-0.5*sx,rectOrigin.Y-0.5*sy,rectDest.X+0.5*sx,rectDest.Y+0.5*sy,Manager.ToolTexture) else
      toolDest.FillRectAntialias(rectOrigin.X-0.5*sx,rectOrigin.Y-0.5*sy,rectDest.X+0.5*sx,rectDest.Y+0.5*sy,fillColor);
  end else
  if Manager.ToolOptionDrawShape and not Manager.ToolOptionFillShape then
  begin
    result := GetShapeBounds([PointF(rectOrigin.x,rectOrigin.y),PointF(rectDest.x,rectDest.y)],Manager.ToolPenWidth);
    if Manager.ToolTexture <> nil then
      toolDest.RectangleAntialias(rectOrigin.X,rectOrigin.Y,rectDest.X,rectDest.Y,Manager.ToolTexture,Manager.ToolPenWidth) else
      toolDest.RectangleAntialias(rectOrigin.X,rectOrigin.Y,rectDest.X,rectDest.Y,penColor,Manager.ToolPenWidth)
  end else
  if Manager.ToolOptionDrawShape and Manager.ToolOptionFillShape then
  begin
    result := GetShapeBounds([PointF(rectOrigin.x,rectOrigin.y),PointF(rectDest.x,rectDest.y)],Manager.ToolPenWidth);
    toolDest.RectangleAntialias(rectOrigin.X,rectOrigin.Y,rectDest.X,rectDest.Y,penColor,Manager.ToolPenWidth,fillColor);
  end else
    result := EmptyRect;
end;

procedure TToolRectangle.PrepareDrawing(rightBtn: boolean);
begin
   if rightBtn then
   begin
     penColor := Manager.ToolBackColor;
     fillColor := Manager.ToolForeColor;
   end else
   begin
     penColor := Manager.ToolForeColor;
     fillColor := Manager.ToolBackColor;
   end;
end;

function TToolRectangle.FinishShape(toolDest: TBGRABitmap): TRect;
begin
  result := EmptyRect;
end;

function TToolRectangle.NeedClearShape: boolean;
begin
  result := true;
end;

{ TToolRectangular }

function TToolRectangular.GetIsSelectingTool: boolean;
begin
  Result:= false;
end;

function TToolRectangular.DoToolDown(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF; rightBtn: boolean): TRect;
begin
  result := EmptyRect;
  if not rectDrawing then
  begin
    FreeAndNil(rectBackup);
    rectBackupSource := nil;
    rectDrawing := true;
    rectOrigin := pt;
    rectDest := pt;
    PrepareDrawing(rightBtn);
    previousRect := EmptyRect;
  end;
end;

function TToolRectangular.DoToolMove(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF): TRect;
var currentRect: TRect;
begin
  result := EmptyRect;
  if rectDrawing and ((rectDest.X <> pt.X) or (rectDest.Y <> pt.Y)) then
  begin
    rectDest := pt;
    if (rectBackup = nil) and NeedClearShape then
    begin
      rectBackup := toolDest.Duplicate as TBGRABitmap;
      rectBackupSource := toolDest;
    end;
    currentRect := UpdateShape(toolDest);
    result := RectUnion(previousRect,currentRect);
    previousRect := currentRect;
  end;
 end;

function TToolRectangular.ToolUp: TRect;
var currentRect: TRect;
begin
  currentRect := ValidateDrawing;
  result := RectUnion(previousRect,currentRect);
  previousRect := currentRect;
end;

function TToolRectangular.ValidateDrawing: TRect;
begin
  if rectDrawing then
  begin
    result := FinishShape(GetToolDrawingLayer);
    rectDrawing := false;
    FreeAndNil(rectBackup);
    rectBackupSource := nil;
    Manager.Image.SaveLayerOrSelectionUndo;
  end else
    result := EmptyRect;
end;

procedure TToolRectangular.ClearShape;
begin
  if (rectBackup <> nil) and (rectBackupSource <> nil) then
    rectBackupSource.PutImage(0,0,rectBackup,dmSet);
end;

destructor TToolRectangular.Destroy;
begin
  ValidateDrawing;
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
  if snapToPixel and (Manager.ToolPenWidth = 1) and (Manager.ToolTexture = nil) then
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
  if snapToPixel and (Manager.ToolPenWidth = 1) and (Manager.ToolTexture = nil) then
  begin
    ix := round(ptF.X);
    iy := round(ptF.Y);
    toolDest.DrawPixel(ix,iy,penColor);
    result := rect(ix,iy,ix+1,iy+1);
  end else
  begin
     if Manager.ToolTexture <> nil then
       toolDest.FillEllipseAntialias(ptF.X,ptF.Y,Manager.ToolPenWidth/2,Manager.ToolPenWidth/2,Manager.ToolTexture)
     else
       toolDest.FillEllipseAntialias(ptF.X,ptF.Y,Manager.ToolPenWidth/2,Manager.ToolPenWidth/2,penColor);
     result := GetShapeBounds([ptF],Manager.ToolPenWidth);
  end;
end;

function TToolPen.ContinueDrawing(toolDest: TBGRABitmap; originF, destF: TPointF): TRect;
begin
  if snapToPixel and (Manager.ToolPenWidth = 1) and (Manager.ToolTexture = nil) then
  begin
    toolDest.DrawLineAntialias(round(destF.X),round(destF.Y),round(originF.X),round(originF.Y),penColor,false);
    result := GetShapeBounds([destF,originF],1);
  end else
  begin
     if Manager.ToolTexture <> nil then
       toolDest.DrawLineAntialias(destF.X,destF.Y,originF.X,originF.Y,Manager.ToolTexture,Manager.ToolPenWidth,False)
     else
       toolDest.DrawLineAntialias(destF.X,destF.Y,originF.X,originF.Y,penColor,Manager.ToolPenWidth,False);
     result := GetShapeBounds([destF,originF],Manager.ToolPenWidth);
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
  if snapToPixel then ptF := PointF(pt.X,pt.Y);
  result := EmptyRect;
  if penDrawing and (sqr(penOrigin.X-ptF.X)+sqr(penOrigin.Y-ptF.Y) >= 0.999) then
  begin
    toolDest.PenStyle := psSolid;
    result := ContinueDrawing(toolDest,penOrigin,ptF);
    penOrigin := ptF;
  end;
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
    Manager.Image.SaveLayerOrSelectionUndo;
  end;
  result := EmptyRect;
end;

destructor TToolPen.Destroy;
begin
  if penDrawing then Manager.Image.SaveLayerOrSelectionUndo;
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
  Result:= Manager.Image.currentImageLayer;   //do not create a selection layer
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

