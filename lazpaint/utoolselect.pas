unit utoolselect;

//result := currentTool in [ptMoveSelection,ptRotateSelection];

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, utool, utoolpolygon, utoolbasic, BGRABitmapTypes, BGRABitmap;

type

  { TToolSelectPoly }

  TToolSelectPoly = class(TToolGenericPolygon)
  protected
    fillColor: TBGRAPixel;
    function DoUpdatePolygonView(toolDest: TBGRABitmap): TRect; override;
    function DoValidatePolygon(toolDest: TBGRABitmap): TRect; override;
    procedure StartPolygon(rightBtn: boolean); override;
    function GetIsSelectingTool: boolean; override;
  end;

  { TToolSelectSpline }

  TToolSelectSpline = class(TToolGenericSpline)
  protected
    fillColor: TBGRAPixel;
    function DoUpdatePolygonView(toolDest: TBGRABitmap): TRect; override;
    function DoValidatePolygon(toolDest: TBGRABitmap): TRect; override;
    procedure StartPolygon(rightBtn: boolean); override;
    function GetIsSelectingTool: boolean; override;
  end;

  { TToolMagicWand }

  TToolMagicWand = class(TGenericTool)
  protected
    function GetIsSelectingTool: boolean; override;
    function DoToolDown(toolDest: TBGRABitmap; pt: TPoint; ptF: TPointF;
      rightBtn: boolean): TRect; override;
  end;

  { TToolSelectionPen }

  TToolSelectionPen = class(TToolPen)
  protected
    function GetIsSelectingTool: boolean; override;
    function StartDrawing(toolDest: TBGRABitmap; ptF: TPointF; rightBtn: boolean): TRect; override;
    function ContinueDrawing(toolDest: TBGRABitmap; originF, destF: TPointF): TRect; override;
  end;

  { TToolSelectRect }

  TToolSelectRect = class(TToolRectangular)
  protected
    function GetIsSelectingTool: boolean; override;
    function UpdateShape(toolDest: TBGRABitmap): TRect; override;
    function FinishShape(toolDest: TBGRABitmap): TRect; override;
    procedure PrepareDrawing(rightBtn: boolean); override;
    function BigImage: boolean;
    function NeedClearShape: boolean; override;
  public
    procedure Render(VirtualScreen: TBGRABitmap; BitmapToVirtualScreen: TBitmapToVirtualScreenFunction); override;
  end;

  { TToolSelectEllipse }

  TToolSelectEllipse = class(TToolSelectRect)
  protected
    function FinishShape(toolDest: TBGRABitmap): TRect; override;
  public
    procedure Render(VirtualScreen: TBGRABitmap; BitmapToVirtualScreen: TBitmapToVirtualScreenFunction); override;
  end;

  { TToolMoveSelection }

  TToolMoveSelection = class(TGenericTool)
  protected
    handMoving: boolean;
    handOrigin: TPoint;
    function GetIsSelectingTool: boolean; override;
    function DoToolDown(toolDest: TBGRABitmap; pt: TPoint; ptF: TPointF;
      rightBtn: boolean): TRect; override;
    function DoToolMove(toolDest: TBGRABitmap; pt: TPoint; ptF: TPointF): TRect; override;
    procedure DoToolMoveAfter(pt: TPoint; ptF: TPointF); override;
  public
    function ToolUp: TRect; override;
    destructor Destroy; override;
  end;

  { TToolRotateSelection }

  TToolRotateSelection = class(TGenericTool)
  protected
    handMoving: boolean;
    handOrigin: TPoint;
    snapRotate: boolean;
    snapAngle: single;
    function GetIsSelectingTool: boolean; override;
    function DoToolDown(toolDest: TBGRABitmap; pt: TPoint; ptF: TPointF;
      rightBtn: boolean): TRect; override;
    function DoToolMove(toolDest: TBGRABitmap; pt: TPoint; ptF: TPointF): TRect; override;
  public
    function ToolKeyDown(key: Word): TRect; override;
    function ToolKeyUp(key: Word): TRect; override;
    function ToolUp: TRect; override;
    procedure Render(VirtualScreen: TBGRABitmap; BitmapToVirtualScreen: TBitmapToVirtualScreenFunction); override;
    destructor Destroy; override;
  end;

implementation

uses types, ugraph, LCLType;

{ TToolRotateSelection }

function TToolRotateSelection.GetIsSelectingTool: boolean;
begin
  Result:= true;
end;

function TToolRotateSelection.DoToolDown(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF; rightBtn: boolean): TRect;
begin
  result := EmptyRect;
  if not handMoving and not Manager.Image.SelectionEmpty then
  begin
    if rightBtn then
    begin
      Manager.Image.SelectionRotateAngle := 0;
      Manager.Image.SelectionRotateCenter := ptF;
      Manager.Image.SelectionMayChange;
      result := rect(0,0,Manager.Image.Width,Manager.Image.Height);
    end else
    begin
      handMoving := true;
      handOrigin := pt;
    end;
  end;
end;

function TToolRotateSelection.DoToolMove(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF): TRect;
var angleDiff: single;
begin
  if handMoving and ((handOrigin.X <> pt.X) or (handOrigin.Y <> pt.Y)) then
  begin
    angleDiff := ComputeAngle(pt.X-Manager.Image.SelectionRotateCenter.X,pt.Y-Manager.Image.SelectionRotateCenter.Y)-
                 ComputeAngle(handOrigin.X-Manager.Image.SelectionRotateCenter.X,handOrigin.Y-Manager.Image.SelectionRotateCenter.Y);
    if snapRotate then
    begin
      snapAngle += angleDiff;
      Manager.Image.SelectionRotateAngle := round(snapAngle/15)*15;
    end
     else
       Manager.Image.SelectionRotateAngle += angleDiff;
    Manager.Image.SelectionMayChange;
    handOrigin := pt;
    result := rect(0,0,Manager.Image.Width,Manager.Image.Height);
  end else
    result := EmptyRect;
end;

function TToolRotateSelection.ToolKeyDown(key: Word): TRect;
begin
  if key = VK_CONTROL then
  begin
    if not snapRotate then
    begin
      snapRotate := true;
      snapAngle := Manager.Image.SelectionRotateAngle;
    end;
  end;
  result := EmptyRect;
end;

function TToolRotateSelection.ToolKeyUp(key: Word): TRect;
begin
  if key = VK_CONTROL then snapRotate := false;
  result := EmptyRect;
end;

function TToolRotateSelection.ToolUp: TRect;
begin
  handMoving:= false;
  Result:= EmptyRect;
end;

procedure TToolRotateSelection.Render(VirtualScreen: TBGRABitmap; BitmapToVirtualScreen: TBitmapToVirtualScreenFunction);
var pictureRotateCenter: TPointF;
begin
  pictureRotateCenter := BitmapToVirtualScreen(Manager.image.SelectionRotateCenter);
  NicePoint(VirtualScreen, pictureRotateCenter.X,pictureRotateCenter.Y);
end;

destructor TToolRotateSelection.Destroy;
begin
  if handMoving then handMoving := false;
  if (Manager.Image.SelectionOffset.X <> 0) or (Manager.Image.SelectionOffset.Y <> 0) or (Manager.Image.SelectionRotateAngle <> 0) then
  begin
    Manager.Image.ApplySelectionTransform;
    Manager.Image.SaveLayerOrSelectionUndo;
  end;
  inherited Destroy;
end;

{ TToolMoveSelection }

function TToolMoveSelection.GetIsSelectingTool: boolean;
begin
  result := true;
end;

function TToolMoveSelection.DoToolDown(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF; rightBtn: boolean): TRect;
begin
  if not handMoving then
  begin
    handMoving := true;
    handOrigin := pt;
  end;
  result := EmptyRect;
end;

function TToolMoveSelection.DoToolMove(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF): TRect;
var dx,dy: integer;
begin
  result := EmptyRect;
  if handMoving and ((handOrigin.X <> pt.X) or (handOrigin.Y <> pt.Y)) then
  begin
    result := Manager.Image.SelectionLayerBounds[True];
    dx := pt.X-HandOrigin.X;
    dy := pt.Y-HandOrigin.Y;
    Manager.Image.SelectionOffset := Point(Manager.Image.SelectionOffset.X+dx,
                                   Manager.Image.SelectionOffset.Y+dy);
    Manager.Image.SelectionMayChange(True);
    if IsRectEmpty(result) then Result:= ToolRepaintOnly else
      begin
        result := RectUnion(result,RectOfs(result,dx,dy));
      end;
  end;
end;

procedure TToolMoveSelection.DoToolMoveAfter(pt: TPoint; ptF: TPointF);
begin
  if handMoving then handOrigin := pt;
end;

function TToolMoveSelection.ToolUp: TRect;
begin
  handMoving := false;
  result := EmptyRect;
end;

destructor TToolMoveSelection.Destroy;
begin
  if handMoving then handMoving := false;
  if (Manager.Image.SelectionOffset.X <> 0) or (Manager.Image.SelectionOffset.Y <> 0) or (Manager.Image.SelectionRotateAngle <> 0) then
  begin
    Manager.Image.ApplySelectionTransform;
    Manager.Image.SaveLayerOrSelectionUndo;
  end;
  inherited Destroy;
end;

{ TToolSelectEllipse }

function TToolSelectEllipse.FinishShape(toolDest: TBGRABitmap): TRect;
var rx,ry: single;
begin
   ClearShape;
   rx := abs(rectDest.X-rectOrigin.X)+0.5;
   ry := abs(rectDest.Y-rectOrigin.Y)+0.5;
   Manager.Image.currentSelection.FillEllipseAntialias(rectOrigin.X,rectOrigin.Y,rx,ry,fillColor);
   Manager.Image.SelectionMayChange;
   result := ToolRepaintOnly;
end;

procedure TToolSelectEllipse.Render(VirtualScreen: TBGRABitmap;
  BitmapToVirtualScreen: TBitmapToVirtualScreenFunction);
var toolPtF,toolPtF2: TPointF;
    rx,ry: single;
    ptsF: Array of TPointF;
    pts: array of TPoint;
    i: integer;
begin
  if rectDrawing and BigImage then
  begin
    rx := abs(rectDest.X-rectOrigin.X)+0.5;
    ry := abs(rectDest.Y-rectOrigin.Y)+0.5;

    toolPtF := pointf(rectOrigin.X-rx,rectOrigin.Y-ry);
    toolPtF2 := pointf(rectOrigin.X+rx,rectOrigin.Y+ry);

    toolPtF := BitmapToVirtualScreen(toolPtF);
    toolPtF2 := BitmapToVirtualScreen(toolPtF2);
    toolPtF2.x -= 1;
    toolPtF2.y -= 1;

    ptsF := VirtualScreen.ComputeEllipseContour((toolPtF.X+toolPtF2.X)/2,
      (toolPtF.Y+toolPtF2.Y)/2,(toolPtF2.X-toolPtF.X)/2,(toolPtF2.Y-toolPtF.Y)/2,0.1);

    setlength(pts, length(ptsF));
    for i := 0 to high(pts) do
      pts[i] := point(round(ptsF[i].x),round(ptsF[i].y));
    setlength(pts, length(pts)+1);
    pts[high(pts)] := pts[0];

    virtualscreen.DrawPolyLineAntialias(pts,BGRA(255,255,255,192),BGRA(0,0,0,192),FrameDashLength,False);
  end;
end;

{ TToolSelectRect }

function TToolSelectRect.GetIsSelectingTool: boolean;
begin
  Result:= true;
end;

function TToolSelectRect.UpdateShape(toolDest: TBGRABitmap): TRect;
begin
  if not BigImage then FinishShape(toolDest);
  result := ToolRepaintOnly;
end;

function TToolSelectRect.FinishShape(toolDest: TBGRABitmap): TRect;
var sx,sy :integer;
begin
  ClearShape;
  if rectDest.X > rectOrigin.X then sx := 1 else sx := -1;
  if rectDest.Y > rectOrigin.Y then sy := 1 else sy := -1;

  toolDest.FillRectAntialias(rectOrigin.X-0.5*sx,rectOrigin.Y-0.5*sy,rectDest.X+0.5*sx,rectDest.Y+0.5*sy,fillColor);
  Manager.Image.SelectionMayChange;
  result := ToolRepaintOnly;
end;

procedure TToolSelectRect.PrepareDrawing(rightBtn: boolean);
begin
  if rightBtn then fillColor := BGRABlack else
    fillColor := BGRAWhite;
end;

function TToolSelectRect.BigImage: boolean;
begin
  result := GetToolDrawingLayer.NbPixels > 480000;
end;

function TToolSelectRect.NeedClearShape: boolean;
begin
  result := not BigImage;
end;

procedure TToolSelectRect.Render(VirtualScreen: TBGRABitmap;
  BitmapToVirtualScreen: TBitmapToVirtualScreenFunction);
var toolPtF,toolPtF2: TPointF;
    r: TRect;
begin
  if rectDrawing and BigImage then
  begin
    if rectDest.X > rectOrigin.X then
    begin
      r.Left := rectOrigin.x;
      r.Right := rectDest.x;
    end else
    begin
      r.Right := rectOrigin.x;
      r.Left := rectDest.x;
    end;
    if rectDest.Y > rectOrigin.Y then
    begin
      r.Top := rectOrigin.Y;
      r.Bottom := rectDest.Y;
    end else
    begin
      r.Bottom := rectOrigin.Y;
      r.Top := rectDest.Y;
    end;

    toolPtF := BitmapToVirtualScreen(pointf(r.Left-0.5,r.Top-0.5));
    toolPtF2 := BitmapToVirtualScreen(pointf(r.Right+0.5,r.Bottom+0.5));
    toolPtF2.x -= 1;
    toolPtF2.y -= 1;

    virtualScreen.DrawpolylineAntialias([point(round(toolPtF.X),round(toolPtF.Y)),
             point(round(toolPtF2.X),round(toolPtF.Y)),
             point(round(toolPtF2.X),round(toolPtF2.Y)),
             point(round(toolPtF.X),round(toolPtF2.Y)),
             point(round(toolPtF.X),round(toolPtF.Y))],BGRA(255,255,255,192),BGRA(0,0,0,192),FrameDashLength,False);
  end;
end;

{ TToolSelectSpline }

function TToolSelectSpline.DoUpdatePolygonView(toolDest: TBGRABitmap): TRect;
var
   splinePoints: ArrayOfTPointF;
begin
  splinePoints := toolDest.ComputeClosedSpline(polygonPoints,Manager.ToolSplineStyle);
  if length(splinePoints) > 2 then
    toolDest.FillPolyAntialias(splinePoints, fillColor);
  Manager.Image.SelectionMayChange;
  result := ToolRepaintOnly;
end;

function TToolSelectSpline.DoValidatePolygon(toolDest: TBGRABitmap): TRect;
begin
  //nothing
  result := EmptyRect;
end;

procedure TToolSelectSpline.StartPolygon(rightBtn: boolean);
begin
  if rightBtn then
    fillColor := BGRABlack
  else
    fillColor := BGRAWhite;
end;

function TToolSelectSpline.GetIsSelectingTool: boolean;
begin
  Result:= true;
end;

{ TToolSelectionPen }

function TToolSelectionPen.GetIsSelectingTool: boolean;
begin
  Result:= true;
end;

function TToolSelectionPen.StartDrawing(toolDest: TBGRABitmap; ptF: TPointF;
  rightBtn: boolean): TRect;
begin
  if rightBtn then penColor := BGRABlack else penColor := BGRAWhite;
  toolDest.DrawLineAntialias(ptF.X,ptF.Y,ptF.X,ptF.Y,penColor,Manager.ToolPenWidth,True);
  Manager.Image.SelectionMayChange;
  result := ToolRepaintOnly;
end;

function TToolSelectionPen.ContinueDrawing(toolDest: TBGRABitmap; originF,
  destF: TPointF): TRect;
begin
  Manager.Image.currentSelection.DrawLineAntialias(destF.X,destF.Y,originF.X,originF.Y,penColor,Manager.ToolPenWidth,False);
  Manager.Image.SelectionMayChange;
  result := ToolRepaintOnly;
end;

{ TToolMagicWand }

function TToolMagicWand.GetIsSelectingTool: boolean;
begin
  Result:= true;
end;

function TToolMagicWand.DoToolDown(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF; rightBtn: boolean): TRect;
var penColor: TBGRAPixel;
begin
  if rightBtn then penColor := BGRABlack else penColor := BGRAWhite;
  Manager.Image.currentImageLayer.ParallelFloodFill(pt.X,pt.Y,toolDest,penColor,fmDrawWithTransparency,Manager.ToolTolerance);
  Manager.Image.SelectionMayChange;
  Manager.Image.SaveLayerOrSelectionUndo;
  result := ToolRepaintOnly;
end;

{ TToolSelectPoly }

function TToolSelectPoly.DoUpdatePolygonView(toolDest: TBGRABitmap): TRect;
begin
  result := ToolRepaintOnly;
  //nothing
end;

function TToolSelectPoly.DoValidatePolygon(toolDest: TBGRABitmap): TRect;
begin
  toolDest.PutImage(0,0,polygonBackup,dmSet);
  if length(polygonPoints) > 2 then
    toolDest.FillPolyAntialias(polygonPoints, fillColor);
  Manager.Image.SelectionMayChange;
  result := ToolRepaintOnly;
end;

procedure TToolSelectPoly.StartPolygon(rightBtn: boolean);
begin
  if rightBtn then
    fillColor := BGRABlack
  else
    fillColor := BGRAWhite;
end;

function TToolSelectPoly.GetIsSelectingTool: boolean;
begin
  result := true;
end;

initialization

  RegisterTool(ptMagicWand,TToolMagicWand);
  RegisterTool(ptSelectPen,TToolSelectionPen);
  RegisterTool(ptSelectRect,TToolSelectRect);
  RegisterTool(ptSelectEllipse,TToolSelectEllipse);
  RegisterTool(ptSelectPoly,TToolSelectPoly);
  RegisterTool(ptSelectSpline,TToolSelectSpline);
  RegisterTool(ptMoveSelection,TToolMoveSelection);
  RegisterTool(ptRotateSelection,TToolRotateSelection);

end.

