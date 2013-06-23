unit utoolpolygon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, utool, BGRABitmap, BGRABitmapTypes;

type

  { TToolGenericPolygon }

  TToolGenericPolygon = class(TGenericTool)
  protected
    polygonPoints: array of TPointF;
    polygonBackup: TBGRABitmap;
    snapToPixel : boolean;
    swapColor: boolean;
    function DoUpdatePolygonView(toolDest: TBGRABitmap): TRect; virtual; abstract;
    function DoValidatePolygon(toolDest: TBGRABitmap): TRect; virtual; abstract;
    function ValidatePolygon(toolDest: TBGRABitmap): TRect;
    function UpdatePolygonView(toolDest: TBGRABitmap): TRect;
    procedure StartPolygon(rightBtn: boolean); virtual; abstract;
    function SnapToPixelEdge: boolean; virtual;
    procedure DoSnap(var ptF: TPointF); virtual;
  public
    function GetCurrentPolygonPoints: ArrayOfTPointF;
    function ToolKeyUp(key: Word): TRect; override;
    function DoToolDown(toolDest: TBGRABitmap; {%H-}pt: TPoint; ptF: TPointF;
      rightBtn: boolean): TRect; override;
    function ToolKeyDown(key: Word): TRect; override;
    procedure Render(VirtualScreen: TBGRABitmap; BitmapToVirtualScreen: TBitmapToVirtualScreenFunction); override;
    destructor Destroy; override;
  end;

  { TToolPolygon }

  TToolPolygon = class(TToolGenericPolygon)
  protected
    FPreviousBounds,FCurrentBounds: TRect;
    penColor: TBGRAPixel;
    fillColor: TBGRAPixel;
    function SnapToPixelEdge: boolean; override;
    function GetIsSelectingTool: boolean; override;
    function DoUpdatePolygonView(toolDest: TBGRABitmap): TRect; override;
    function DoValidatePolygon(toolDest: TBGRABitmap): TRect; override;
    procedure StartPolygon(rightBtn: boolean); override;
  end;

  { TToolGenericSpline }

  TToolGenericSpline = class(TToolGenericPolygon)
    FPreviousBounds,FCurrentBounds: TRect;
    function DoToolMove(toolDest: TBGRABitmap; {%H-}pt: TPoint; ptF: TPointF): TRect; override;
  protected
    procedure StartPolygon({%H-}rightBtn: boolean); override;
  end;

  { TToolSpline }

  TToolSpline = class(TToolGenericSpline)
  protected
    penColor: TBGRAPixel;
    fillColor: TBGRAPixel;
    function SnapToPixelEdge: boolean; override;
    function GetIsSelectingTool: boolean; override;
    function DoUpdatePolygonView(toolDest: TBGRABitmap): TRect; override;
    function DoValidatePolygon({%H-}toolDest: TBGRABitmap):TRect; override;
    procedure StartPolygon(rightBtn: boolean); override;
  end;

implementation

uses Graphics, LCLType, ugraph, Dialogs;

{ TToolGenericSpline }

function TToolGenericSpline.DoToolMove(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF): TRect;
begin
  FPreviousBounds := FCurrentBounds;
  DoSnap(ptF);
  if length(polygonPoints) > 0 then
  begin
    setlength(polygonPoints, length(polygonPoints)+1);
    polygonPoints[high(polygonPoints)] := ptF;
    FCurrentBounds := UpdatePolygonView(toolDest);
    setlength(polygonPoints, length(polygonPoints)-1);
  end else
    FCurrentBounds := EmptyRect;
  result := RectUnion(FPreviousBounds,FCurrentBounds);
end;

procedure TToolGenericSpline.StartPolygon(rightBtn: boolean);
begin
  FCurrentBounds := EmptyRect;
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

function TToolSpline.DoUpdatePolygonView(toolDest: TBGRABitmap): TRect;
var
   splinePoints: ArrayOfTPointF;
begin
  toolDest.JoinStyle := pjsRound;

  if Manager.ToolOptionCloseShape then
  begin
    splinePoints := toolDest.ComputeClosedSpline(polygonPoints,Manager.ToolSplineStyle);
  end else
   splinePoints := toolDest.ComputeOpenedSpline(polygonPoints,Manager.ToolSplineStyle);

  result := EmptyRect;
  if length(splinePoints) > 2 then
  begin
    if Manager.ToolOptionFillShape then
    begin
      if not Manager.ToolOptionDrawShape and (Manager.ToolTexture <> nil) then
        toolDest.FillPolyAntialias(splinePoints, Manager.ToolTexture) else
        toolDest.FillPolyAntialias(splinePoints, fillColor);
      result := GetShapeBounds(splinePoints,1);
    end;
    if Manager.ToolOptionDrawShape then
    begin
      if not Manager.ToolOptionFillShape and (Manager.ToolTexture <> nil) then
      begin
        if Manager.ToolOptionCloseShape then
          toolDest.DrawPolygonAntialias(splinePoints,Manager.ToolTexture,Manager.ToolPenWidth)
        else
          toolDest.DrawPolylineAntialias(splinePoints,Manager.ToolTexture,Manager.ToolPenWidth);
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
     if not Manager.ToolOptionFillShape and (Manager.ToolTexture <> nil) then
       toolDest.DrawLineAntialias(splinePoints[0].X,splinePoints[0].Y,
             splinePoints[high(splinePoints)].X,splinePoints[high(splinePoints)].Y,
             Manager.ToolTexture,Manager.ToolPenWidth) else
       toolDest.DrawLineAntialias(splinePoints[0].X,splinePoints[0].Y,
             splinePoints[high(splinePoints)].X,splinePoints[high(splinePoints)].Y,
             penColor,Manager.ToolPenWidth);
     result := GetShapeBounds(splinePoints,Manager.ToolPenWidth);
    end;
  end;
end;

function TToolSpline.DoValidatePolygon(toolDest: TBGRABitmap): TRect;
begin
  //nothing
  result := EmptyRect;
end;

procedure TToolSpline.StartPolygon(rightBtn: boolean);
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

{ TToolPolygon }

function TToolPolygon.SnapToPixelEdge: boolean;
begin
  Result:= not Manager.ToolOptionDrawShape;
end;

function TToolPolygon.GetIsSelectingTool: boolean;
begin
  Result:= false;
end;

function TToolPolygon.DoUpdatePolygonView(toolDest: TBGRABitmap): TRect;
begin
   FPreviousBounds := FCurrentBounds;
   if Manager.ToolOptionDrawShape then
   begin
     if not Manager.ToolOptionFillShape and (Manager.ToolTexture <>nil) then
       toolDest.DrawPolyLineAntialias(polygonPoints,Manager.ToolTexture,Manager.ToolPenWidth) else
       toolDest.DrawPolyLineAntialias(polygonPoints,penColor,Manager.ToolPenWidth);
     FCurrentBounds := GetShapeBounds(polygonPoints,Manager.ToolPenWidth);
   end else
     FCurrentBounds := EmptyRect;
   result := RectUnion(FPreviousBounds,FCurrentBounds);
end;

function TToolPolygon.DoValidatePolygon(toolDest: TBGRABitmap): TRect;
var mergedBounds: TRect;
begin
   FPreviousBounds := FCurrentBounds;
   FCurrentBounds := EmptyRect;
   toolDest.PutImage(0,0,polygonBackup,dmSet);
   if length(polygonPoints) > 2 then
   begin
     if Manager.ToolOptionFillShape then
     begin
       if not Manager.ToolOptionDrawShape and (Manager.ToolTexture <> nil) then
         toolDest.FillPolyAntialias(polygonPoints, Manager.ToolTexture) else
         toolDest.FillPolyAntialias(polygonPoints, fillColor);
       FCurrentBounds := GetShapeBounds(polygonPoints,1);
     end;
     if Manager.ToolOptionDrawShape then
     begin
       if not Manager.ToolOptionFillShape and (Manager.ToolTexture <> nil) then
       begin
         if Manager.ToolOptionCloseShape then
           toolDest.DrawPolygonAntialias(polygonPoints,Manager.ToolTexture,Manager.ToolPenWidth) else
             toolDest.DrawPolyLineAntialias(polygonPoints,Manager.ToolTexture,Manager.ToolPenWidth);
       end else
       if Manager.ToolOptionCloseShape then
         toolDest.DrawPolygonAntialias(polygonPoints,penColor,Manager.ToolPenWidth) else
           toolDest.DrawPolyLineAntialias(polygonPoints,penColor,Manager.ToolPenWidth);
       FCurrentBounds := GetShapeBounds(polygonPoints,Manager.ToolPenWidth);
     end;
   end else
   if length(polygonPoints) = 2 then
   begin
     if Manager.ToolOptionDrawShape then
     begin
      if not Manager.ToolOptionFillShape and (Manager.ToolTexture <> nil) then
        toolDest.DrawLineAntialias(polygonPoints[0].X,polygonPoints[0].Y,
              polygonPoints[1].X,polygonPoints[1].Y,
              Manager.ToolTexture,Manager.ToolPenWidth) else
        toolDest.DrawLineAntialias(polygonPoints[0].X,polygonPoints[0].Y,
              polygonPoints[1].X,polygonPoints[1].Y,
              penColor,Manager.ToolPenWidth);
      FCurrentBounds := GetShapeBounds(polygonPoints,Manager.ToolPenWidth);
     end;
   end;
   mergedBounds := RectUnion(FCurrentBounds,FPreviousBounds);
   Manager.Image.ImageMayChange(mergedBounds);
   result := mergedBounds;
end;

procedure TToolPolygon.StartPolygon(rightBtn: boolean);
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
   FCurrentBounds := EmptyRect;
end;

{ TToolGenericPolygon }

function TToolGenericPolygon.ValidatePolygon(toolDest: TBGRABitmap): TRect;
begin
  result := DoValidatePolygon(toolDest);
  setlength(polygonPoints,0);
  FreeAndNil(polygonBackup);
  Manager.Image.SaveLayerOrSelectionUndo;
end;

function TToolGenericPolygon.UpdatePolygonView(toolDest: TBGRABitmap): TRect;
begin
  toolDest.PutImage(0,0,polygonBackup,dmSet);
  result := DoUpdatePolygonView(toolDest);
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
  if key = VK_SHIFT then swapColor := false;
  Result:=EmptyRect;
end;

function TToolGenericPolygon.DoToolDown(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF; rightBtn: boolean): TRect;
begin
   result := EmptyRect;
   DoSnap(ptF);
   if length(polygonPoints)=0 then
   begin
     if not rightBtn then
     begin
       polygonBackup := toolDest.Duplicate as TBGRABitmap;

       setlength(polygonPoints,1);
       polygonPoints[0] := ptF;
       StartPolygon(rightBtn xor swapColor);

       result := UpdatePolygonView(toolDest);
     end;
   end else
   begin
     if not rightBtn then
     begin
       setlength(polygonPoints, length(polygonPoints)+1);
       polygonPoints[high(polygonPoints)] := ptF;

       result := UpdatePolygonView(toolDest);
     end else
     begin
       result := ValidatePolygon(toolDest);
     end;
   end;
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
        FreeAndNil(polygonBackup);
      end;
    end;
  end else
  if Key=VK_RETURN then
  begin
    if length(polygonPoints)<>0 then
     begin
       result := ValidatePolygon(GetToolDrawingLayer);
     end;
  end else
  if key = VK_CONTROL then
    snapToPixel := true else
  if key = VK_SHIFT then
    swapColor := true;
end;

procedure TToolGenericPolygon.Render(VirtualScreen: TBGRABitmap; BitmapToVirtualScreen: TBitmapToVirtualScreenFunction);
var
   poly: array of TPointF;
   toolPtF, toolPtF2: TPointF;
   i: integer;
begin
  poly := GetCurrentPolygonPoints;
  if length(poly) > 0 then
  begin
    toolPtF := BitmapToVirtualScreen(poly[0]);
    NicePoint(virtualScreen, toolPtF.X,toolPtF.Y);
    for i := 0 to high(poly)-1 do
    begin
        toolPtF := BitmapToVirtualScreen(poly[i]);
        toolPtF2 := BitmapToVirtualScreen(poly[i+1]);
        virtualScreen.DrawLineAntialias(round(toolPtF.X),round(toolPtF.Y),
           round(toolPtF2.X),round(toolPtF2.Y),BGRA(255,255,255,192),BGRA(0,0,0,192),FrameDashLength,False);
    end;
    if length(poly) > 1 then
    begin
      toolPtF := BitmapToVirtualScreen(poly[high(poly)]);
      NicePoint(virtualScreen, toolPtF.X,toolPtF.Y);
    end;
  end;
end;

destructor TToolGenericPolygon.Destroy;
begin
  if length(polygonPoints)<>0 then
    ValidatePolygon(GetToolDrawingLayer);
  FreeAndNil(polygonBackup);
end;

initialization

  RegisterTool(ptPolygon,TToolPolygon);
  RegisterTool(ptSpline,TToolSpline);

end.

