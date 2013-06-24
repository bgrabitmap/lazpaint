unit UToolPolygon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, utool, BGRABitmap, BGRABitmapTypes;

const
  EasyBezierMinimumDotProduct = 0.5;

type

  { TToolGenericPolygon }

  TToolGenericPolygon = class(TGenericTool)
  strict private
    swapColorKey: boolean;
    FCurrentBounds: TRect;
    lastMousePos: TPointF;
  protected
    class var HintShowed: boolean;
  protected
    polygonPoints: array of TPointF;
    swapedColor : boolean;
    snapToPixel : boolean;
    FAfterHandDrawing: boolean;
    FHoveredPoint: integer;
    FMovingPoint: integer;
    FMovingPointDelta: TPointF;
    function HandDrawingPolygonView(toolDest: TBGRABitmap): TRect; virtual; abstract;
    function FinalPolygonView(toolDest: TBGRABitmap): TRect; virtual; abstract;
    procedure ValidatePolygon(toolDest: TBGRABitmap);
    function UpdatePolygonView(toolDest: TBGRABitmap): TRect;
    procedure StartPolygon(rightBtn: boolean); virtual;
    function SnapToPixelEdge: boolean; virtual;
    procedure DoSnap(var ptF: TPointF); virtual;
    function DoToolDown(toolDest: TBGRABitmap; {%H-}pt: TPoint; ptF: TPointF;
      rightBtn: boolean): TRect; override;
    function DoToolMove({%H-}toolDest: TBGRABitmap; {%H-}pt: TPoint; ptF: TPointF): TRect;
      override;
    function DoToolUpdate(toolDest: TBGRABitmap): TRect; override;
    function GetFillColor: TBGRAPixel; virtual;
    function GetPenColor: TBGRAPixel; virtual;
    function FinishHandDrawing: TRect;
    function AddLastClickedPoint: boolean; virtual;
  public
    constructor Create(AToolManager: TToolManager); override;
    function GetCurrentPolygonPoints: ArrayOfTPointF;
    function ToolKeyUp(key: Word): TRect; override;
    function ToolKeyDown(key: Word): TRect; override;
    function ToolUp: TRect; override;
    procedure Render(VirtualScreen: TBGRABitmap; BitmapToVirtualScreen: TBitmapToVirtualScreenFunction); override;
    destructor Destroy; override;
    property PenColor: TBGRAPixel read GetPenColor;
    property FillColor: TBGRAPixel read GetFillColor;
  end;

  { TToolPolygon }

  TToolPolygon = class(TToolGenericPolygon)
  protected
    function SnapToPixelEdge: boolean; override;
    function GetIsSelectingTool: boolean; override;
    function HandDrawingPolygonView(toolDest: TBGRABitmap): TRect; override;
    function FinalPolygonView(toolDest: TBGRABitmap): TRect; override;
  end;

  { TToolGenericSpline }

  TToolGenericSpline = class(TToolGenericPolygon)
    FCurrentMousePos: TPointF;
    function DoToolMove(toolDest: TBGRABitmap; {%H-}pt: TPoint; ptF: TPointF): TRect; override;
    function AddLastClickedPoint: boolean; override;
  end;

  { TToolSpline }

  TToolSpline = class(TToolGenericSpline)
  protected
    function SnapToPixelEdge: boolean; override;
    function GetIsSelectingTool: boolean; override;
    function HandDrawingPolygonView(toolDest: TBGRABitmap): TRect; override;
    function FinalPolygonView({%H-}toolDest: TBGRABitmap):TRect; override;
  end;

implementation

uses Types, Graphics, LCLType, ugraph, Dialogs, BGRATypewriter, Controls;

{ TToolGenericSpline }

function TToolGenericSpline.DoToolMove(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF): TRect;
var inheritedResult: TRect;
begin
  inheritedResult := inherited DoToolMove(toolDest,pt,ptF);
  DoSnap(ptF);
  if not FAfterHandDrawing and (length(polygonPoints) > 0) then
  begin
    setlength(polygonPoints, length(polygonPoints)+1);
    polygonPoints[high(polygonPoints)] := ptF;
    result := UpdatePolygonView(toolDest);
    setlength(polygonPoints, length(polygonPoints)-1);
  end
  else
    result := EmptyRect;

  result := RectUnion(result,inheritedResult);
end;

function TToolGenericSpline.AddLastClickedPoint: boolean;
begin
  Result:=true;
end;

{ TToolSpline }

function TToolSpline.SnapToPixelEdge: boolean;
begin
  Result:= not Manager.ToolOptionDrawShape;
end;

function TToolSpline.GetIsSelectingTool: boolean;
begin
  Result:= false;
end;

function TToolSpline.HandDrawingPolygonView(toolDest: TBGRABitmap): TRect;
var
   splinePoints: ArrayOfTPointF;
begin
  toolDest.JoinStyle := pjsRound;

  if Manager.ToolSplineEasyBezier then
  begin
    splinePoints := ComputeEasyBezier(polygonPoints,Manager.ToolOptionCloseShape,EasyBezierMinimumDotProduct);
  end else
  begin
    if Manager.ToolOptionCloseShape then
    begin
      splinePoints := toolDest.ComputeClosedSpline(polygonPoints,Manager.ToolSplineStyle);
    end else
     splinePoints := toolDest.ComputeOpenedSpline(polygonPoints,Manager.ToolSplineStyle);
  end;

  result := EmptyRect;
  if length(splinePoints) > 2 then
  begin
    if Manager.ToolOptionFillShape then
    begin
      if not Manager.ToolOptionDrawShape and (Manager.GetToolTextureAfterAlpha <> nil) then
        toolDest.FillPolyAntialias(splinePoints, Manager.GetToolTextureAfterAlpha) else
        toolDest.FillPolyAntialias(splinePoints, fillColor);
      result := GetShapeBounds(splinePoints,1);
    end;
    if Manager.ToolOptionDrawShape then
    begin
      if not Manager.ToolOptionFillShape and (Manager.GetToolTextureAfterAlpha <> nil) then
      begin
        if Manager.ToolOptionCloseShape then
          toolDest.DrawPolygonAntialias(splinePoints,Manager.GetToolTextureAfterAlpha,Manager.ToolPenWidth)
        else
          toolDest.DrawPolylineAntialias(splinePoints,Manager.GetToolTextureAfterAlpha,Manager.ToolPenWidth);
      end else
      if Manager.ToolOptionCloseShape then
        toolDest.DrawPolygonAntialias(splinePoints,penColor,Manager.ToolPenWidth)
      else
        toolDest.DrawPolylineAntialias(splinePoints,penColor,Manager.ToolPenWidth);
      result := GetShapeBounds(splinePoints,Manager.ToolPenWidth);
    end;
  end else
  if length(splinePoints) > 0 then
  begin
    if Manager.ToolOptionDrawShape then
    begin
     if not Manager.ToolOptionFillShape and (Manager.GetToolTextureAfterAlpha <> nil) then
       toolDest.DrawLineAntialias(splinePoints[0].X,splinePoints[0].Y,
             splinePoints[high(splinePoints)].X,splinePoints[high(splinePoints)].Y,
             Manager.GetToolTextureAfterAlpha,Manager.ToolPenWidth) else
       toolDest.DrawLineAntialias(splinePoints[0].X,splinePoints[0].Y,
             splinePoints[high(splinePoints)].X,splinePoints[high(splinePoints)].Y,
             penColor,Manager.ToolPenWidth);
     result := GetShapeBounds(splinePoints,Manager.ToolPenWidth);
    end;
  end;
end;

function TToolSpline.FinalPolygonView(toolDest: TBGRABitmap): TRect;
begin
  if FAfterHandDrawing then
    result := HandDrawingPolygonView(toolDest)
  else
    result := EmptyRect;
end;

{ TToolPolygon }

function TToolPolygon.SnapToPixelEdge: boolean;
begin
  Result:= not Manager.ToolOptionDrawShape;
end;

function TToolPolygon.GetIsSelectingTool: boolean;
begin
  Result:= false;
end;

function TToolPolygon.HandDrawingPolygonView(toolDest: TBGRABitmap): TRect;
begin
   if Manager.ToolOptionDrawShape then
   begin
     if not Manager.ToolOptionFillShape and (Manager.GetToolTextureAfterAlpha <>nil) then
       toolDest.DrawPolyLineAntialias(polygonPoints,Manager.GetToolTextureAfterAlpha,Manager.ToolPenWidth) else
       toolDest.DrawPolyLineAntialias(polygonPoints,penColor,Manager.ToolPenWidth);
     result := GetShapeBounds(polygonPoints,Manager.ToolPenWidth);
   end else
     result := EmptyRect;
end;

function TToolPolygon.FinalPolygonView(toolDest: TBGRABitmap): TRect;
begin
   if length(polygonPoints) > 2 then
   begin
     if Manager.ToolOptionFillShape then
     begin
       if not Manager.ToolOptionDrawShape and (Manager.GetToolTextureAfterAlpha <> nil) then
         toolDest.FillPolyAntialias(polygonPoints, Manager.GetToolTextureAfterAlpha) else
         toolDest.FillPolyAntialias(polygonPoints, fillColor);
       result := GetShapeBounds(polygonPoints,1);
     end;
     if Manager.ToolOptionDrawShape then
     begin
       if not Manager.ToolOptionFillShape and (Manager.GetToolTextureAfterAlpha <> nil) then
       begin
         if Manager.ToolOptionCloseShape then
           toolDest.DrawPolygonAntialias(polygonPoints,Manager.GetToolTextureAfterAlpha,Manager.ToolPenWidth) else
             toolDest.DrawPolyLineAntialias(polygonPoints,Manager.GetToolTextureAfterAlpha,Manager.ToolPenWidth);
       end else
       if Manager.ToolOptionCloseShape then
         toolDest.DrawPolygonAntialias(polygonPoints,penColor,Manager.ToolPenWidth) else
           toolDest.DrawPolyLineAntialias(polygonPoints,penColor,Manager.ToolPenWidth);
       result := GetShapeBounds(polygonPoints,Manager.ToolPenWidth);
     end;
   end else
   if length(polygonPoints) = 2 then
   begin
     if Manager.ToolOptionDrawShape then
     begin
      if not Manager.ToolOptionFillShape and (Manager.GetToolTextureAfterAlpha <> nil) then
        toolDest.DrawLineAntialias(polygonPoints[0].X,polygonPoints[0].Y,
              polygonPoints[1].X,polygonPoints[1].Y,
              Manager.GetToolTextureAfterAlpha,Manager.ToolPenWidth) else
        toolDest.DrawLineAntialias(polygonPoints[0].X,polygonPoints[0].Y,
              polygonPoints[1].X,polygonPoints[1].Y,
              penColor,Manager.ToolPenWidth);
      result := GetShapeBounds(polygonPoints,Manager.ToolPenWidth);
     end;
   end;
end;

{ TToolGenericPolygon }

function TToolGenericPolygon.GetFillColor: TBGRAPixel;
begin
   if swapedColor then
     result := Manager.ToolForeColor
   else
     result := Manager.ToolBackColor;
end;

function TToolGenericPolygon.GetPenColor: TBGRAPixel;
begin
   if swapedColor then
     result := Manager.ToolBackColor
   else
     result := Manager.ToolForeColor;
end;

function TToolGenericPolygon.FinishHandDrawing: TRect;
begin
  if AddLastClickedPoint then
  begin
    setlength(polygonPoints, length(polygonPoints)+1);
    polygonPoints[high(polygonPoints)] := lastMousePos;
  end;
  result := FinalPolygonView(GetToolDrawingLayer);
  if IsRectEmpty(result) then
    result := ToolRepaintOnly;
  FAfterHandDrawing := True;
end;

function TToolGenericPolygon.AddLastClickedPoint: boolean;
begin
  result := false;
end;

constructor TToolGenericPolygon.Create(AToolManager: TToolManager);
begin
  inherited Create(AToolManager);
  FAfterHandDrawing:= false;
  polygonPoints := nil;
  FMovingPoint:= -1;
end;

procedure TToolGenericPolygon.ValidatePolygon(toolDest: TBGRABitmap);
var r: TRect;
begin
  if not FAfterHandDrawing then
  begin
    r := FinalPolygonView(toolDest);
    setlength(polygonPoints,0);
    if IsSelectingTool then
      Manager.Image.SelectionMayChange(r)
    else
      Manager.Image.ImageMayChange(r);
    FAfterHandDrawing:= True;
  end else
  begin
    ValidateAction;
    FAfterHandDrawing:= false;
    polygonPoints := nil;
  end;
end;

function TToolGenericPolygon.UpdatePolygonView(toolDest: TBGRABitmap): TRect;
var
   previousBounds : TRect;
begin
  previousBounds := FCurrentBounds;
  RestoreBackupDrawingLayer;
  if FAfterHandDrawing then
    FCurrentBounds := FinalPolygonView(toolDest)
  else
    FCurrentBounds := HandDrawingPolygonView(toolDest);
  result := RectUnion(previousBounds,FCurrentBounds);
  if IsRectEmpty(result) then result := ToolRepaintOnly;
end;

procedure TToolGenericPolygon.StartPolygon(rightBtn: boolean);
begin
  swapedColor := rightBtn;
  FCurrentBounds := EmptyRect;
end;

function TToolGenericPolygon.SnapToPixelEdge: boolean;
begin
  result := true;
end;

procedure TToolGenericPolygon.DoSnap(var ptF: TPointF);
begin
 if snapToPixel then
 begin
   if SnapToPixelEdge then
     ptF := pointF(round(ptF.X+0.5)-0.5,round(ptF.Y+0.5)-0.5)
   else
     ptF := pointF(round(ptF.X),round(ptF.Y));
 end;
end;

function TToolGenericPolygon.GetCurrentPolygonPoints: ArrayOfTPointF;
var i: integer;
begin
  setlength(result, length(polygonPoints));
  for i := 0 to high(result) do
    result[i]:= polygonPoints[i];
end;

function TToolGenericPolygon.ToolKeyUp(key: Word): TRect;
begin
  if key = VK_CONTROL then snapToPixel := false;
  if key = VK_SHIFT then swapColorKey := false;
  Result:=EmptyRect;
end;

function TToolGenericPolygon.DoToolDown(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF; rightBtn: boolean): TRect;
begin
   result := EmptyRect;
   if not HintShowed then
   begin
     Manager.ToolPopup(tpmBackspaceRemoveLastPoint);
     HintShowed := true;
   end;
   if length(polygonPoints)=0 then
   begin
     DoSnap(ptF);
     if not rightBtn then
     begin
       FAfterHandDrawing:= false;
       setlength(polygonPoints,1);
       polygonPoints[0] := ptF;
       StartPolygon(rightBtn xor swapColorKey);

       result := UpdatePolygonView(toolDest);
     end;
   end else
   if not FAfterHandDrawing then
   begin
     DoSnap(ptF);
     if not rightBtn then
     begin
       setlength(polygonPoints, length(polygonPoints)+1);
       polygonPoints[high(polygonPoints)] := ptF;
       result := UpdatePolygonView(toolDest);
     end else
     begin
       result := FinishHandDrawing;
     end;
   end else
   if (FHoveredPoint <> -1) and (FHoveredPoint < length(polygonPoints)) then
   begin
     FMovingPoint := FHoveredPoint;
     FMovingPointDelta := polygonPoints[FMovingPoint]-ptF;
   end;
end;

function TToolGenericPolygon.DoToolMove(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF): TRect;
var i: integer;
   minDist,dist: single;
   newPos: TPointF;
begin
  lastMousePos := ptF;
  Result:= EmptyRect;
  if FMovingPoint = -1 then
  begin
    Cursor := crDefault;
    FHoveredPoint := -1;
    if FAfterHandDrawing then
    begin
      minDist:= SelectionMaxPointDistance;
      for i := 0 to high(polygonPoints) do
      begin
        dist := VectLen(polygonPoints[i]-ptF);
        if dist < minDist then
        begin
          FHoveredPoint := i;
          mindist := dist;
          Cursor := crSizeAll;
        end;
      end;
    end;
  end else
  begin
    if FMovingPoint >= length(polygonPoints) then
      FMovingPoint := -1
    else
    begin
      newPos := ptF+FMovingPointDelta;
      DoSnap(newPos);
      polygonPoints[FMovingPoint] := newPos;
      result := UpdatePolygonView(toolDest);
    end;
  end;
end;

function TToolGenericPolygon.DoToolUpdate(toolDest: TBGRABitmap): TRect;
begin
  result := UpdatePolygonView(toolDest);
end;

function TToolGenericPolygon.ToolKeyDown(key: Word): TRect;
begin
  result := EmptyRect;
  if Key=VK_BACK then
  begin
    if length(polygonPoints) > 0 then
    begin
      if length(polygonPoints) > 1 then
      begin
        setlength(polygonPoints, length(polygonPoints)-1);
        result := UpdatePolygonView(GetToolDrawingLayer);
      end else
      begin
        polygonPoints := nil;
        result := UpdatePolygonView(GetToolDrawingLayer);
        CancelAction;
      end;
    end;
  end else
  if (Key=VK_RETURN) or (Key=VK_ESCAPE) then
  begin
    if length(polygonPoints)<>0 then
    begin
      if not FAfterHandDrawing then
      begin
        if Key=VK_ESCAPE then
        begin
          RestoreBackupDrawingLayer;
          polygonPoints := nil;
          result := FCurrentBounds;
          FCurrentBounds := EmptyRect;
        end else
          result := FinishHandDrawing
      end
      else
      begin
        ValidatePolygon(GetToolDrawingLayer);
        result := ToolRepaintOnly;
      end;
    end;
  end else
  if key = VK_CONTROL then
    snapToPixel := true else
  if key = VK_SHIFT then
    swapColorKey := true;
end;

function TToolGenericPolygon.ToolUp: TRect;
begin
  Result:=inherited ToolUp;
  if FMovingPoint <> -1 then
  begin
    FMovingPoint := -1;
    Cursor := crDefault;
  end;
end;

procedure TToolGenericPolygon.Render(VirtualScreen: TBGRABitmap; BitmapToVirtualScreen: TBitmapToVirtualScreenFunction);
var
   poly: array of TPointF;
   toolPtF, toolPtF2: TPointF;
   i,maxi: integer;
begin
  poly := GetCurrentPolygonPoints;
  if length(poly) > 0 then
  begin
    if FAfterHandDrawing then maxi := high(poly) else maxi := high(poly)-1;
    for i := 0 to maxi do
    begin
        toolPtF := BitmapToVirtualScreen(poly[i]);
        toolPtF2 := BitmapToVirtualScreen(poly[(i+1) mod length(poly)]);
        virtualScreen.DrawLineAntialias(round(toolPtF.X),round(toolPtF.Y),
           round(toolPtF2.X),round(toolPtF2.Y),BGRA(255,255,255,192),BGRA(0,0,0,192),FrameDashLength,False);
    end;
    if FAfterHandDrawing then
    begin
      for i := 0 to high(poly) do
      begin
        toolPtF := BitmapToVirtualScreen(poly[i]);
        NicePoint(virtualScreen, toolPtF.X,toolPtF.Y);
      end;
    end else
    begin
      toolPtF := BitmapToVirtualScreen(poly[0]);
      NicePoint(virtualScreen, toolPtF.X,toolPtF.Y);
      if length(poly) > 1 then
      begin
        toolPtF := BitmapToVirtualScreen(poly[high(poly)]);
        NicePoint(virtualScreen, toolPtF.X,toolPtF.Y);
      end;
    end;
  end;
end;

destructor TToolGenericPolygon.Destroy;
begin
  if length(polygonPoints)<>0 then
    ValidatePolygon(GetToolDrawingLayer);
  inherited Destroy;
end;

initialization

  RegisterTool(ptPolygon,TToolPolygon);
  RegisterTool(ptSpline,TToolSpline);

end.

