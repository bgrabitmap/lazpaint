unit UToolPolygon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, utool, BGRABitmap, BGRABitmapTypes, ULayerAction, BGRATypewriter;

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
    FMovingWholePoly: Boolean;
    FMovingPointDelta: TPointF;
    FRenderedPolygonPoints: ArrayOfTPointF;
    function GetIsHandDrawing: boolean;
    function GetIsIdle: boolean;
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
    function GetAction: TLayerAction; override;
    procedure OnFinishHandDrawing; virtual;
    procedure OnAddPoint({%H-}AIndex: integer); virtual;
    procedure OnDeletePoint({%H-}AIndex: integer); virtual;
    procedure OnValidatePolygon; virtual;
    function MustUpdateOnAddPoint: boolean; virtual;
    procedure StartArrow(dest: TBGRABitmap); virtual;
    procedure EndArrow(dest: TBGRABitmap); virtual;
    function GetStatusText: string; override;
  public
    constructor Create(AToolManager: TToolManager); override;
    function GetCurrentPolygonPoints: ArrayOfTPointF;
    function ToolKeyUp(var key: Word): TRect; override;
    function ToolKeyDown(var key: Word): TRect; override;
    function ToolUp: TRect; override;
    function Render(VirtualScreen: TBGRABitmap; {%H-}VirtualScreenWidth, {%H-}VirtualScreenHeight: integer; BitmapToVirtualScreen: TBitmapToVirtualScreenFunction):TRect; override;
    destructor Destroy; override;
    property PenColor: TBGRAPixel read GetPenColor;
    property FillColor: TBGRAPixel read GetFillColor;
    property IsHandDrawing: boolean read GetIsHandDrawing;
    property IsIdle: boolean read GetIsIdle;
  end;

  { TToolPolygon }

  TToolPolygon = class(TToolGenericPolygon)
  protected
    function SnapToPixelEdge: boolean; override;
    function GetIsSelectingTool: boolean; override;
    function HandDrawingPolygonView(toolDest: TBGRABitmap): TRect; override;
    function FinalPolygonView(toolDest: TBGRABitmap): TRect; override;
  end;

  TToolSplineMode = (tsmMovePoint, tsmCurveModeAuto, tsmCurveModeAngle, tsmCurveModeSpline);

  { TToolGenericSpline }

  TToolGenericSpline = class(TToolGenericPolygon)
  private
    function GetCurrentMode: TToolSplineMode;
    function GetDefaultCurveMode: TGlyphPointCurveMode;
    procedure SetCurrentMode(AValue: TToolSplineMode);
    procedure SetDefaultCurveMode(AValue: TGlyphPointCurveMode);
  protected
    FCurrentMousePos: TPointF;
    FCurveMode: array of TGlyphPointCurveMode;
    FCurveModeHintShown: boolean;
    FCurrentMode: TToolSplineMode;
    procedure NeedCurveMode;
    function GetCurveMode(AIndex: integer): TGlyphPointCurveMode;
    procedure SetCurveMode(AIndex: integer; AValue: TGlyphPointCurveMode);
    procedure OnFinishHandDrawing; override;
    procedure OnAddPoint(AIndex: integer); override;
    procedure OnDeletePoint(AIndex: integer); override;
    function EndsMode(AMode: TGlyphPointCurveMode): TGlyphPointCurveMode;
    function MustUpdateOnAddPoint: boolean; override;
    property DefaultCurveMode: TGlyphPointCurveMode read GetDefaultCurveMode
      write SetDefaultCurveMode;
    procedure OnValidatePolygon; override;
  public
    constructor Create(AToolManager: TToolManager); override;
    function DoToolDown(toolDest: TBGRABitmap; pt: TPoint; ptF: TPointF;
      rightBtn: boolean): TRect; override;
    function DoToolMove(toolDest: TBGRABitmap; {%H-}pt: TPoint; ptF: TPointF): TRect; override;
    function ToolKeyDown(var key: Word): TRect; override;
    function AddLastClickedPoint: boolean; override;
    property CurveMode[AIndex: integer]: TGlyphPointCurveMode read GetCurveMode write SetCurveMode;
    property CurrentMode: TToolSplineMode read GetCurrentMode write SetCurrentMode;
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

uses Types, Graphics, LCLType, ugraph, Dialogs, Controls, LazPaintType,
  BGRAFillInfo;

{ TToolGenericSpline }

function TToolGenericSpline.GetCurrentMode: TToolSplineMode;
begin
  result := FCurrentMode;
end;

function TToolGenericSpline.GetDefaultCurveMode: TGlyphPointCurveMode;
begin
  if CurrentMode = tsmCurveModeAngle then
    result := cmAngle else
  if CurrentMode = tsmCurveModeSpline then
    result := cmCurve
  else
    result := cmAuto;
end;

procedure TToolGenericSpline.SetCurrentMode(AValue: TToolSplineMode);
begin
  if (FMovingPoint <> -1) or FMovingWholePoly then exit;
  if (AValue = tsmMovePoint) and IsHandDrawing then
    exit;
  FCurrentMode := AValue;
end;

procedure TToolGenericSpline.SetDefaultCurveMode(AValue: TGlyphPointCurveMode);
begin
  if AValue = cmAngle then CurrentMode := tsmCurveModeAngle else
  if AValue = cmCurve then CurrentMode := tsmCurveModeSpline else
    CurrentMode := tsmCurveModeAuto;
end;

procedure TToolGenericSpline.NeedCurveMode;
begin
  if length(FCurveMode) <> length(polygonPoints) then
    setlength(FCurveMode, length(polygonPoints));
end;

function TToolGenericSpline.GetCurveMode(AIndex: integer): TGlyphPointCurveMode;
begin
  NeedCurveMode;
  if (AIndex < 0) or (AIndex >= length(FCurveMode)) then
    result := cmAuto else result := FCurveMode[AIndex];
end;

procedure TToolGenericSpline.SetCurveMode(AIndex: integer;
  AValue: TGlyphPointCurveMode);
begin
  NeedCurveMode;
  if (AIndex < 0) or (AIndex >= length(FCurveMode)) then exit;
  FCurveMode[AIndex] := AValue;
end;

procedure TToolGenericSpline.OnFinishHandDrawing;
begin
  if Length(polygonPoints) > 0 then CurveMode[length(polygonPoints)-1] := EndsMode(DefaultCurveMode);
  FCurrentMode := tsmMovePoint;
end;

procedure TToolGenericSpline.OnAddPoint(AIndex: integer);
var i:integer;
begin
  if AIndex <= length(FCurveMode)-1 then
    begin
      setlength(FCurveMode,length(FCurveMode)+1);
      for i := high(FCurveMode) downto AIndex+1 do
        FCurveMode[i] := FCurveMode[i-1];
    end;
  if AIndex = 0 then CurveMode[AIndex] := EndsMode(DefaultCurveMode) else
      curveMode[AIndex] := DefaultCurveMode;
end;

procedure TToolGenericSpline.OnDeletePoint(AIndex: integer);
var i: integer;
begin
  if AIndex >= length(FCurveMode)-1 then
    begin
      for i:= AIndex to high(FCurveMode)-1 do
        FCurveMode[i] := FCurveMode[i+1];
      setlength(FCurveMode,length(FCurveMode)-1);
    end;
  inherited OnDeletePoint(AIndex);
end;

function TToolGenericSpline.EndsMode(AMode: TGlyphPointCurveMode
  ): TGlyphPointCurveMode;
begin
  result := AMode;
end;

function TToolGenericSpline.MustUpdateOnAddPoint: boolean;
begin
  Result:=false;
end;

procedure TToolGenericSpline.OnValidatePolygon;
begin
  FCurrentMode:= tsmCurveModeAuto;
  inherited OnValidatePolygon;
end;

constructor TToolGenericSpline.Create(AToolManager: TToolManager);
begin
  inherited Create(AToolManager);
  FCurrentMousePos := EmptyPointF;
  FCurrentMode := tsmCurveModeAuto;
end;

function TToolGenericSpline.DoToolDown(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF; rightBtn: boolean): TRect;
begin
  if IsHandDrawing or IsIdle or (CurrentMode = tsmMovePoint) then
    Result:=inherited DoToolDown(toolDest, pt, ptF, rightBtn)
  else
  begin
    result := EmptyRect;
    if FAfterHandDrawing and (FHoveredPoint <> -1) and (CurrentMode in[tsmCurveModeAngle,tsmCurveModeAuto,tsmCurveModeSpline]) then
    begin
      CurveMode[FHoveredPoint] := DefaultCurveMode;
      result := UpdatePolygonView(GetToolDrawingLayer);
    end;
  end;
end;

function TToolGenericSpline.DoToolMove(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF): TRect;
var inheritedResult: TRect;
begin
  inheritedResult := inherited DoToolMove(toolDest,pt,ptF);
  if (FHoveredPoint <> -1) and not IsHandDrawing and (CurrentMode <> tsmMovePoint) then
    begin
      Cursor := crHandPoint;
    end;
  DoSnap(ptF);
  if not FAfterHandDrawing and (length(polygonPoints) > 0) then
  begin
    if length(polygonPoints) > 1 then
    begin
      if not FCurveModeHintShown then
      begin
        Manager.ToolPopup(tpmCurveModeHint);
        FCurveModeHintShown := true;
      end;
    end;
    setlength(polygonPoints, length(polygonPoints)+1);
    polygonPoints[high(polygonPoints)] := ptF;
    CurveMode[high(polygonPoints)] := EndsMode(DefaultCurveMode);
    FCurrentMousePos := ptF;
    result := UpdatePolygonView(toolDest);
    CurveMode[high(polygonPoints)] := DefaultCurveMode;
    setlength(polygonPoints, length(polygonPoints)-1);
  end
  else
    result := EmptyRect;

  result := RectUnion(result,inheritedResult);
end;

function TToolGenericSpline.ToolKeyDown(var key: Word): TRect;
var idx: integer;
   newCurveMode: TGlyphPointCurveMode;
begin
  if (Key = VK_S) or (Key = VK_X) or (Key = VK_A) or (Key = VK_Z) then
  begin
    if IsHandDrawing then idx := length(polygonPoints)-1
      else idx := FHoveredPoint;
    if Key = VK_S then
      newCurveMode := cmCurve
    else if Key = VK_X then
      newCurveMode := cmAngle else
    if Key = VK_A then
      newCurveMode := cmAuto else
    if Key = VK_Z then
    begin
      if not IsHandDrawing and not IsIdle then CurrentMode := tsmMovePoint;
      exit;
    end;
    DefaultCurveMode := newCurveMode;
    if idx = -1 then
    begin
      Key := 0;
      result := EmptyRect;
      exit;
    end;
    if length(polygonPoints) > 1 then
      CurveMode[idx] := newCurveMode else
      CurveMode[idx] := EndsMode(newCurveMode);
    Key := 0;
    if not isEmptyPointF(FCurrentMousePos) and not FAfterHandDrawing then
    begin
      setlength(polygonPoints, length(polygonPoints)+1);
      polygonPoints[high(polygonPoints)] := FCurrentMousePos;
      CurveMode[high(polygonPoints)] := EndsMode(DefaultCurveMode);
      result := UpdatePolygonView(GetToolDrawingLayer);
      CurveMode[high(polygonPoints)] := DefaultCurveMode;
      setlength(polygonPoints, length(polygonPoints)-1);
    end else
      result := UpdatePolygonView(GetToolDrawingLayer);
  end else
    Result:=inherited ToolKeyDown(key);
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
   splinePoints, outline: ArrayOfTPointF;
begin
  result := EmptyRect;
  toolDest.JoinStyle := pjsRound;

  if Manager.ToolSplineEasyBezier then
  begin
    NeedCurveMode;
    splinePoints := ComputeEasyBezier(polygonPoints,FCurveMode,Manager.ToolOptionCloseShape,EasyBezierMinimumDotProduct);
  end else
  begin
    if Manager.ToolOptionCloseShape then
    begin
      splinePoints := toolDest.ComputeClosedSpline(polygonPoints,Manager.ToolSplineStyle);
    end else
     splinePoints := toolDest.ComputeOpenedSpline(polygonPoints,Manager.ToolSplineStyle);
  end;
  FRenderedPolygonPoints := splinePoints;

  if Manager.ToolOptionFillShape and (length(splinePoints) > 2) then
  begin
    if not Manager.ToolOptionDrawShape and (Manager.GetToolTextureAfterAlpha <> nil) then
      toolDest.FillPolyAntialias(splinePoints, Manager.GetToolTextureAfterAlpha) else
      toolDest.FillPolyAntialias(splinePoints, fillColor);
    result := GetShapeBounds(splinePoints,1);
  end;
  if Manager.ToolOptionDrawShape and (length(splinePoints) >= 2) then
  begin
    if length(splinePoints) > 2 then
    begin
      if Manager.ToolOptionCloseShape then
        outline := toolDest.ComputeWidePolygon(splinePoints,Manager.ToolPenWidth)
      else
      begin
        StartArrow(toolDest);
        outline := toolDest.ComputeWidePolyline(splinePoints,Manager.ToolPenWidth);
        EndArrow(toolDest);
      end;
    end else
    begin
      StartArrow(toolDest);
      outline := toolDest.ComputeWidePolyline(splinePoints,Manager.ToolPenWidth);
      EndArrow(toolDest);
    end;

    if not Manager.ToolOptionFillShape and (Manager.GetToolTextureAfterAlpha <> nil) then
      toolDest.FillPolyAntialias(outline,Manager.GetToolTextureAfterAlpha)
    else
      toolDest.FillPolyAntialias(outline,penColor);
    result := GetShapeBounds(outline,1);
  end else
  if Manager.ToolOptionDrawShape and (length(splinePoints) = 1) then
  begin
   if not Manager.ToolOptionFillShape and (Manager.GetToolTextureAfterAlpha <> nil) then
     toolDest.DrawLineAntialias(splinePoints[0].X,splinePoints[0].Y,
           splinePoints[0].X,splinePoints[0].Y,
           Manager.GetToolTextureAfterAlpha,Manager.ToolPenWidth) else
     toolDest.DrawLineAntialias(splinePoints[0].X,splinePoints[0].Y,
           splinePoints[0].X,splinePoints[0].Y,
           penColor,Manager.ToolPenWidth);
   result := GetShapeBounds(splinePoints,Manager.ToolPenWidth);
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
var
  outline: ArrayOfTPointF;
begin
  if Manager.ToolOptionDrawShape and (length(polygonPoints)>0) then
  begin
    if Manager.ToolOptionCloseShape then
      outline := toolDest.ComputeWidePolyline(polygonPoints,Manager.ToolPenWidth)
    else
    begin
      StartArrow(toolDest);
      outline := toolDest.ComputeWidePolyline(polygonPoints,Manager.ToolPenWidth);
      EndArrow(toolDest);
    end;
    if not Manager.ToolOptionFillShape and (Manager.GetToolTextureAfterAlpha <> nil) then
      toolDest.FillPolyAntialias(outline,Manager.GetToolTextureAfterAlpha)
    else
      toolDest.FillPolyAntialias(outline,penColor);
    result := GetShapeBounds(outline,1);
  end else
     result := EmptyRect;
end;

function TToolPolygon.FinalPolygonView(toolDest: TBGRABitmap): TRect;
var i: integer;
   outline: ArrayOfTPointF;
begin
   result := EmptyRect;
   setlength(FRenderedPolygonPoints, length(polygonPoints));
   for i := 0 to high(polygonPoints) do
     FRenderedPolygonPoints[i] := polygonPoints[i];

   if Manager.ToolOptionFillShape and (length(polygonPoints) > 2) then
   begin
     if not Manager.ToolOptionDrawShape and (Manager.GetToolTextureAfterAlpha <> nil) then
       toolDest.FillPolyAntialias(polygonPoints, Manager.GetToolTextureAfterAlpha) else
       toolDest.FillPolyAntialias(polygonPoints, fillColor);
     result := GetShapeBounds(polygonPoints,1);
   end;
   if Manager.ToolOptionDrawShape and (length(polygonPoints)>0) then
   begin
     if Manager.ToolOptionCloseShape then
       outline := toolDest.ComputeWidePolygon(polygonPoints,Manager.ToolPenWidth)
     else
     begin
       StartArrow(toolDest);
       outline := toolDest.ComputeWidePolyline(polygonPoints,Manager.ToolPenWidth);
       EndArrow(toolDest);
     end;
     if not Manager.ToolOptionFillShape and (Manager.GetToolTextureAfterAlpha <> nil) then
       toolDest.FillPolyAntialias(outline,Manager.GetToolTextureAfterAlpha)
     else
       toolDest.FillPolyAntialias(outline,penColor);
     result := GetShapeBounds(outline,1);
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
  FAfterHandDrawing := True;
  OnFinishHandDrawing;
  result := UpdatePolygonView(GetToolDrawingLayer);
  if IsRectEmpty(result) then
    result := OnlyRenderChange;
  if length(polygonPoints) = 2 then ValidatePolygon(GetToolDrawingLayer);
end;

function TToolGenericPolygon.AddLastClickedPoint: boolean;
begin
  result := false;
end;

function TToolGenericPolygon.GetAction: TLayerAction;
begin
  Result:=inherited GetAction;
  result.AllChangesNotified := true;
end;

procedure TToolGenericPolygon.OnFinishHandDrawing;
begin

end;

procedure TToolGenericPolygon.OnAddPoint(AIndex: integer);
begin
  //nothing
end;

procedure TToolGenericPolygon.OnDeletePoint(AIndex: integer);
begin
  //nothing
end;

procedure TToolGenericPolygon.OnValidatePolygon;
begin
  //nothing
end;

function TToolGenericPolygon.MustUpdateOnAddPoint: boolean;
begin
  result := true;
end;

procedure TToolGenericPolygon.StartArrow(dest: TBGRABitmap);
begin
  ApplyArrowStyle(True,Manager.ToolArrowStart,dest,Manager.ToolArrowSize);
  ApplyArrowStyle(False,Manager.ToolArrowEnd,dest,Manager.ToolArrowSize);
end;

procedure TToolGenericPolygon.EndArrow(dest: TBGRABitmap);
begin
  dest.ArrowStartAsNone;
  dest.ArrowEndAsNone;
end;

function TToolGenericPolygon.GetStatusText: string;
begin
  if length(polygonPoints) > 0 then
    result := 'n = ' + inttostr(length(polygonPoints))
  else
    Result:=inherited GetStatusText;
end;

constructor TToolGenericPolygon.Create(AToolManager: TToolManager);
begin
  inherited Create(AToolManager);
  FAfterHandDrawing:= false;
  polygonPoints := nil;
  FMovingPoint:= -1;
end;

function TToolGenericPolygon.GetIsHandDrawing: boolean;
begin
  result := not FAfterHandDrawing and (length(polygonPoints)>0);
end;

function TToolGenericPolygon.GetIsIdle: boolean;
begin
  result := length(polygonPoints)=0;
end;

procedure TToolGenericPolygon.ValidatePolygon(toolDest: TBGRABitmap);
var r: TRect;
begin
  if not FAfterHandDrawing then
  begin
    r := FinalPolygonView(toolDest);
    Action.NotifyChange(toolDest,r);
    setlength(polygonPoints,0);
    Manager.Image.LayerMayChange(toolDest,r);
    FAfterHandDrawing:= True;
  end else
  begin
    OnValidatePolygon;
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
  Action.NotifyChange(toolDest, FCurrentBounds);
  result := RectUnion(previousBounds,FCurrentBounds);
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

function TToolGenericPolygon.ToolKeyUp(var key: Word): TRect;
begin
  if key = VK_CONTROL then
  begin
    snapToPixel := false;
    Key := 0
  end else
  if key = VK_SHIFT then
  begin
    swapColorKey := false;
    key := 0
  end;
  Result:=EmptyRect;
end;

function TToolGenericPolygon.DoToolDown(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF; rightBtn: boolean): TRect;
var originalPtF: TPointF;
begin
   result := EmptyRect;
   if not HintShowed then
   begin
     Manager.ToolPopup(tpmBackspaceRemoveLastPoint);
     HintShowed := true;
   end;
   if length(polygonPoints)=0 then
   begin
     originalPtF := ptF;
     DoSnap(ptF);
     if not rightBtn then
     begin
       FAfterHandDrawing:= false;
       setlength(polygonPoints,1);
       polygonPoints[0] := ptF;
       OnAddPoint(0);
       StartPolygon(rightBtn xor swapColorKey);

       result := UpdatePolygonView(toolDest);
       if IsRectEmpty(result) then result := OnlyRenderChange;

       FMovingPoint := high(polygonPoints);
       FMovingPointDelta := polygonPoints[FMovingPoint]-originalPtF;
     end;
   end else
   if not FAfterHandDrawing then
   begin
     originalPtF := ptF;
     DoSnap(ptF);
     if not rightBtn then
     begin
       setlength(polygonPoints, length(polygonPoints)+1);
       polygonPoints[high(polygonPoints)] := ptF;
       OnAddPoint(high(polygonPoints));
       if MustUpdateOnAddPoint then result := UpdatePolygonView(toolDest);

       FMovingPoint := high(polygonPoints);
       FMovingPointDelta := polygonPoints[FMovingPoint]-originalPtF;
     end else
     begin
       if length(polygonPoints) >= 2 then result := FinishHandDrawing;
     end;
     if IsRectEmpty(result) then result := OnlyRenderChange;
   end else
   if (FHoveredPoint <> -1) and (FHoveredPoint < length(polygonPoints)) then
   begin
     FMovingPoint := FHoveredPoint;
     FMovingPointDelta := polygonPoints[FMovingPoint]-ptF;
   end else
   if (length(polygonPoints) > 2) and (IsPointInPolygon(FRenderedPolygonPoints, ptF,true)) then
   begin
     FMovingWholePoly := true;
     FMovingPointDelta := polygonPoints[0]-ptF;
     Cursor := crSizeAll;
   end;
end;

function TToolGenericPolygon.DoToolMove(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF): TRect;
var i: integer;
   minDist,dist: single;
   newPos,diff: TPointF;
begin
  lastMousePos := ptF;
  Result:= EmptyRect;
  if FMovingWholePoly then
  begin
    newPos := ptF+FMovingPointDelta;
    diff := newPos-polygonPoints[0];
    for i := 0 to high(polygonPoints) do
      polygonPoints[i] += diff;
    result := UpdatePolygonView(toolDest);
  end else
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

function TToolGenericPolygon.ToolKeyDown(var key: Word): TRect;
var i: integer;
   bestSegmentIndex: integer;
   bestSegmentDist,segmentDist,segmentLen,segmentPos: single;
   u,n: TPointF;
begin
  result := EmptyRect;
  if (Key=VK_BACK) and not FAfterHandDrawing then
  begin
    if length(polygonPoints) > 0 then
    begin
      if length(polygonPoints) > 1 then
      begin
        setlength(polygonPoints, length(polygonPoints)-1);
        result := UpdatePolygonView(GetToolDrawingLayer);
        if IsRectEmpty(result) then Result := OnlyRenderChange;
      end else
      begin
        polygonPoints := nil;
        result := UpdatePolygonView(GetToolDrawingLayer);
        if IsRectEmpty(result) then Result := OnlyRenderChange;
        CancelAction;
      end;
      Key := 0;
    end;
  end else
  if ((Key = VK_BACK) or (KEY = VK_DELETE)) and not IsIdle then
  begin
    if (FHoveredPoint <> -1) and (length(polygonPoints)>3) then
    begin
      OnDeletePoint(FHoveredPoint);
      for i := FHoveredPoint to high(polygonPoints)-1 do
        polygonPoints[i] := polygonPoints[i+1];
      setlength(polygonPoints, length(polygonPoints)-1);
      result := UpdatePolygonView(GetToolDrawingLayer);
      if IsRectEmpty(result) then Result := OnlyRenderChange;
      Cursor := crDefault;
      FHoveredPoint:= -1;
    end;
    Key := 0;
  end else
  if (key = VK_INSERT) and not IsIdle then
  begin
    if FHoveredPoint = -1 then
    begin
      bestSegmentIndex := -1;
      bestSegmentDist := SelectionMaxPointDistance*2;
      for i := 0 to high(polygonPoints) do
      begin
        u := polygonPoints[(i+1) mod Length(polygonPoints)] - polygonPoints[i];
        segmentLen := VectLen(u);
        if segmentLen > 0 then
        begin
          u *= 1/segmentLen;
          segmentPos := (lastMousePos-polygonPoints[i])*u;
          if (segmentPos > 0) and (segmentPos< segmentLen) then
          begin
            n := PointF(u.y,-u.x);
            segmentDist := abs((lastMousePos-polygonPoints[i])*n);
            if segmentDist <= bestSegmentDist then
            begin
              bestSegmentDist := segmentDist;
              bestSegmentIndex := i;
            end;
          end;
        end;
      end;
      if bestSegmentIndex <> -1 then
      begin
        OnAddPoint(bestSegmentIndex+1);
        setlength(polygonPoints,length(polygonPoints)+1);
        for i:= high(polygonPoints) downto bestSegmentIndex+2 do
          polygonPoints[i] := polygonPoints[i-1];
        polygonPoints[bestSegmentIndex+1] := lastMousePos;
        FHoveredPoint:= bestSegmentIndex+1;
        result := UpdatePolygonView(GetToolDrawingLayer);
        if IsRectEmpty(result) then Result := OnlyRenderChange;
      end;
    end;
    Key := 0;
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
          if IsRectEmpty(result) then result := OnlyRenderChange;
          FCurrentBounds := EmptyRect;
        end else
          result := FinishHandDrawing
      end
      else
      begin
        ValidatePolygon(GetToolDrawingLayer);
        result := OnlyRenderChange;
      end;
      Key := 0;
    end;
  end else
  if key = VK_CONTROL then
  begin
    snapToPixel := true;
    Key := 0;
  end else
  if key = VK_SHIFT then
  begin
    swapColorKey := true;
    Key := 0;
  end;
end;

function TToolGenericPolygon.ToolUp: TRect;
begin
  Result:=inherited ToolUp;
  if FMovingPoint <> -1 then
  begin
    FMovingPoint := -1;
    Cursor := crDefault;
  end;
  if FMovingWholePoly then
  begin
    FMovingWholePoly := false;
    Cursor := crDefault;
  end;
end;

function TToolGenericPolygon.Render(VirtualScreen: TBGRABitmap;
  VirtualScreenWidth, VirtualScreenHeight: integer; BitmapToVirtualScreen: TBitmapToVirtualScreenFunction): TRect;
var
   poly: array of TPointF;
   toolPtF, toolPtF2: TPointF;
   i,maxi: integer;
begin
  result := EmptyRect;
  poly := GetCurrentPolygonPoints;
  if length(poly) > 0 then
  begin
    if FAfterHandDrawing then maxi := high(poly) else maxi := high(poly)-1;
    for i := 0 to maxi do
    begin
        toolPtF := BitmapToVirtualScreen(poly[i]);
        toolPtF2 := BitmapToVirtualScreen(poly[(i+1) mod length(poly)]);
        if Assigned(VirtualScreen) then
          virtualScreen.DrawLineAntialias(round(toolPtF.X),round(toolPtF.Y),
             round(toolPtF2.X),round(toolPtF2.Y),BGRA(255,255,255,192),BGRA(0,0,0,192),FrameDashLength,False);
    end;
    for i := 0 to high(poly) do
    begin
      toolPtF := BitmapToVirtualScreen(poly[i]);
      result := RectUnion(result, NicePointBounds(toolPtF.x, toolPtF.y));
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

