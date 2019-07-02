unit UToolBasic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, utool, BGRABitmapTypes, BGRABitmap, BGRALayerOriginal,
  UImage, ULayerAction, LCVectorOriginal;

type

  { TToolHand }

  TToolHand = class(TReadonlyTool)
  protected
    handMoving: boolean;
    handOrigin: TPoint;
    function FixSelectionTransform: boolean; override;
    function DoToolDown({%H-}toolDest: TBGRABitmap; pt: TPoint; {%H-}ptF: TPointF;
      {%H-}rightBtn: boolean): TRect; override;
    function DoToolMove({%H-}toolDest: TBGRABitmap; pt: TPoint; {%H-}ptF: TPointF): TRect; override;
    procedure DoToolMoveAfter(pt: TPoint; {%H-}ptF: TPointF); override;
    function GetStatusText: string; override;
  public
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
    destructor Destroy; override;
  end;

  { TToolErase }

  TToolErase = class(TToolPen)
  protected
    procedure ApplySoften(var image: TBGRABitmap);
    function BlurRadius: single;
    function StartDrawing(toolDest: TBGRABitmap; ptF: TPointF; {%H-}rightBtn: boolean): TRect; override;
    function ContinueDrawing(toolDest: TBGRABitmap; originF, destF: TPointF): TRect; override;
  end;

  { TVectorialTool }

  TVectorialTool = class(TGenericTool)
  protected
    FShape: TVectorShape;
    FSwapColor: boolean;
    FQuickDefine: Boolean;
    FQuickDefineStartPoint: TPointF;
    FQuickSquare: boolean;
    FPreviousUpdateBounds, FPreviousEditorBounds: TRect;
    FEditor: TBGRAOriginalEditor;
    FShiftState: TShiftState;
    FRightDown, FLeftDown: boolean;
    FLastPos: TPointF;
    function CreateShape: TVectorShape; virtual; abstract;
    procedure AssignShapeStyle; virtual;
    function RoundCoordinate(ptF: TPointF): TPointF; virtual;
    function GetIsSelectingTool: boolean; override;
    function UpdateShape(toolDest: TBGRABitmap): TRect; virtual;
    procedure UpdateCursor(ACursor: TOriginalEditorCursor);
    function DoToolDown({%H-}toolDest: TBGRABitmap; {%H-}pt: TPoint; {%H-}ptF: TPointF; rightBtn: boolean): TRect; override;
    function DoToolMove({%H-}toolDest: TBGRABitmap; {%H-}pt: TPoint; {%H-}ptF: TPointF): TRect; override;
    function DoToolUpdate({%H-}toolDest: TBGRABitmap): TRect; override;
    procedure ShapeChange({%H-}ASender: TObject; ABounds: TRectF); virtual;
    procedure ShapeEditingChange({%H-}ASender: TObject); virtual;
    function GetStatusText: string; override;
    function SlowShape: boolean; virtual;
  public
    constructor Create(AManager: TToolManager); override;
    function ToolUp: TRect; override;
    function ToolKeyDown(var key: Word): TRect; override;
    function ToolKeyUp(var key: Word): TRect; override;
    function Render(VirtualScreen: TBGRABitmap; {%H-}VirtualScreenWidth, {%H-}VirtualScreenHeight: integer; BitmapToVirtualScreen: TBitmapToVirtualScreenFunction):TRect; override;
    destructor Destroy; override;
  end;

  { TToolRectangle }

  TToolRectangle = class(TVectorialTool)
  protected
    function CreateShape: TVectorShape; override;
  end;

  { TToolEllipse }

  TToolEllipse = class(TVectorialTool)
  protected
    function CreateShape: TVectorShape; override;
  end;

  TRectangularBorderTest = set of (btOriginX,btOriginY,btDestX,btDestY,btCenter);

  { TToolRectangular }

  TToolRectangular = class(TGenericTool)
  protected
    class var HintShowed: boolean;
    swapedColor: boolean;
    rectDrawing,afterRectDrawing: boolean;
    rectOrigin, rectDest: TPointF;
    rectMovingPoint,rectMovingCenterPoint: boolean;
    rectMovingPointValueDiff: TPointF;
    rectMovingPointClick: TPointF;
    rectMovingBorderTest: TRectangularBorderTest;
    rectMovingX,rectMovingY: PSingle;
    lastMousePos: TPointF;
    squareConstraint: boolean;
    function GetFillColor: TBGRAPixel; virtual;
    function GetPenColor: TBGRAPixel; virtual;
    function GetIsSelectingTool: boolean; override;
    function BorderTest(ptF: TPointF): TRectangularBorderTest; virtual;
    function DoToolDown({%H-}toolDest: TBGRABitmap; {%H-}pt: TPoint; {%H-}ptF: TPointF; rightBtn: boolean): TRect; override;
    function DoToolMove(toolDest: TBGRABitmap; {%H-}pt: TPoint; {%H-}ptF: TPointF): TRect; override;
    function UpdateShape(toolDest: TBGRABitmap): TRect; virtual; abstract;
    function FinishShape(toolDest: TBGRABitmap): TRect; virtual; abstract;
    procedure PrepareDrawing(rightBtn: boolean); virtual;
    function ValidateDrawing: TRect;
    procedure ClearShape;
    function ShouldFinishShapeWhenFirstMouseUp: boolean; virtual; abstract;
    function RenderAllCornerPositions: boolean; virtual;
    procedure UpdateCursor(ptF: TPointF); virtual;
    function DoToolUpdate(toolDest: TBGRABitmap): TRect; override;
    procedure ApplyConstraint(px,py: PSingle);
    function RoundCoordinate(ptF: TPointF): TPointF; virtual;
    function LeaveMovingPoint: TRect; virtual;
    function GetStatusText: string; override;
    function ConstraintEnabled: boolean; virtual;
  public
    constructor Create(AManager: TToolManager); override;
    function ToolUp: TRect; override;
    function ToolKeyDown(var key: Word): TRect; override;
    function ToolKeyUp(var key: Word): TRect; override;
    function Render(VirtualScreen: TBGRABitmap; {%H-}VirtualScreenWidth, {%H-}VirtualScreenHeight: integer; BitmapToVirtualScreen: TBitmapToVirtualScreenFunction):TRect; override;
    destructor Destroy; override;
    property PenColor: TBGRAPixel read GetPenColor;
    property FillColor: TBGRAPixel read GetFillColor;
  end;

implementation

uses Types, Graphics, LCLType, ugraph, Controls, LazPaintType,
  UResourceStrings, BGRATransform, Math, BGRAPen, LCVectorRectShapes;

function PenStyleToBGRA(APenStyle: TPenStyle): TBGRAPenStyle;
begin
  Case APenStyle of
  psSolid: result := SolidPenStyle;
  psDash: result := DashPenStyle;
  psDot: result := DotPenStyle;
  psDashDot: result := DashDotPenStyle;
  psDashDotDot: result := DashDotDotPenStyle;
  else result := ClearPenStyle;
  end;
end;

{ TVectorialTool }

procedure TVectorialTool.ShapeChange(ASender: TObject; ABounds: TRectF);
var
  toolDest: TBGRABitmap;
  r: TRect;
begin
  toolDest := GetToolDrawingLayer;
  with ABounds do r := rect(floor(Left),floor(Top),ceil(Right),ceil(Bottom));
  UpdateShape(toolDest);
  Action.NotifyChange(toolDest, r);
  with FShape.GetRenderBounds(rect(0,0,toolDest.Width,toolDest.Height),
       AffineMatrixIdentity,[]) do
    FPreviousUpdateBounds := rect(floor(Left),floor(Top),ceil(Right),ceil(Bottom));
end;

procedure TVectorialTool.ShapeEditingChange(ASender: TObject);
var
  toolDest: TBGRABitmap;
  newEditorBounds: TRect;
begin
  toolDest := GetToolDrawingLayer;
  newEditorBounds := FEditor.GetRenderBounds(rect(0,0,toolDest.Width,toolDest.Height));
  Manager.Image.RenderMayChange(RectUnion(FPreviousEditorBounds,newEditorBounds),false);
  FPreviousEditorBounds := newEditorBounds;
end;

function TVectorialTool.GetStatusText: string;
var
  corner1, corner2: TPointF;
begin
  if Assigned(FShape) then
  begin
    if FShape is TEllipseShape then
      with TEllipseShape(FShape) do
        result := 'x = '+FloatToStrF(Origin.x,ffFixed,6,1)+'|y = '+FloatToStrF(Origin.y,ffFixed,6,1)+'|'+
        'rx = '+FloatToStrF(VectLen(XAxis-Origin),ffFixed,6,1)+'|ry = '+FloatToStrF(VectLen(YAxis-Origin),ffFixed,6,1)
    else if FShape is TCustomRectShape then
      with TCustomRectShape(FShape) do
      begin
        corner1 := Origin-(XAxis-Origin)-(YAxis-Origin);
        corner2 := XAxis + (YAxis-Origin);
        result := 'x1 = '+FloatToStrF(corner1.x,ffFixed,6,1)+'|y1 = '+FloatToStrF(corner1.y,ffFixed,6,1)+'|'+
        'x2 = '+FloatToStrF(corner2.x,ffFixed,6,1)+'|y2 = '+FloatToStrF(corner2.y,ffFixed,6,1)+'|'+
        'Δx = '+FloatToStrF(VectLen(XAxis-Origin)*2,ffFixed,6,1)+'|Δy = '+FloatToStrF(VectLen(YAxis-Origin)*2,ffFixed,6,1);
      end;
  end
  else
    Result:=inherited GetStatusText;
end;

function TVectorialTool.SlowShape: boolean;
begin
  result := false;
end;

procedure TVectorialTool.AssignShapeStyle;
var
  f: TVectorShapeFields;
begin
  f:= FShape.Fields;
  if vsfPenFill in f then
  begin
    if Manager.ToolOptionDrawShape then
    begin
      if (not (vsfBackFill in f) or not Manager.ToolOptionFillShape) and (Manager.GetToolTexture <> nil) then
        FShape.PenFill.SetTexture(Manager.GetToolTexture,AffineMatrixIdentity,Manager.ToolTextureOpacity)
      else
      begin
        if FSwapColor then
          FShape.PenFill.SetSolid(Manager.ToolBackColor)
        else
          FShape.PenFill.SetSolid(Manager.ToolForeColor);
      end;
    end else
      FShape.PenFill.Clear;
  end;
  if vsfPenWidth in f then FShape.PenWidth := Manager.ToolPenWidth;
  if vsfPenStyle in f Then FShape.PenStyle := PenStyleToBGRA(Manager.ToolPenStyle);
  if vsfJoinStyle in f then FShape.JoinStyle:= Manager.ToolJoinStyle;
  if vsfBackFill in f then
  begin
    if Manager.ToolOptionFillShape then
    begin
      if Manager.GetToolTexture <> nil then
        FShape.BackFill.SetTexture(Manager.GetToolTexture,AffineMatrixIdentity,Manager.ToolTextureOpacity)
      else
      begin
        if FSwapColor then
          FShape.BackFill.SetSolid(Manager.ToolForeColor)
        else
          FShape.BackFill.SetSolid(Manager.ToolBackColor);
      end;
    end;
  end;
end;

function TVectorialTool.RoundCoordinate(ptF: TPointF): TPointF;
begin
  if not Manager.ToolOptionDrawShape or
    (Assigned(FShape) and not (vsfPenFill in FShape.Fields)) then
    result := PointF(floor(ptF.x)+0.5,floor(ptF.y)+0.5)
  else
    result := PointF(round(ptF.x),round(ptF.y));
end;

function TVectorialTool.GetIsSelectingTool: boolean;
begin
  result := false;
end;

function TVectorialTool.UpdateShape(toolDest: TBGRABitmap): TRect;
var
  newBounds: TRect;
  oldClip: TRect;
begin
  result := FPreviousUpdateBounds;
  RestoreBackupDrawingLayer;
  with FShape.GetRenderBounds(toolDest.ClipRect,AffineMatrixIdentity,[]) do
    newBounds := rect(floor(Left),floor(Top),ceil(Right),ceil(Bottom));
  result := RectUnion(result, newBounds);
  oldClip := toolDest.IntersectClip(newBounds);
  FShape.Render(toolDest,AffineMatrixIdentity,(FRightDown or FLeftDown) and SlowShape);
  toolDest.ClipRect := oldClip;
  FPreviousUpdateBounds := newBounds;
end;

procedure TVectorialTool.UpdateCursor(ACursor: TOriginalEditorCursor);
begin
  case ACursor of
    oecMove: Cursor := crSizeAll;
    oecMoveN: Cursor := crSizeN;
    oecMoveS: Cursor := crSizeS;
    oecMoveE: Cursor := crSizeE;
    oecMoveW: Cursor := crSizeW;
    oecMoveNE: Cursor := crSizeNE;
    oecMoveSW: Cursor := crSizeSW;
    oecMoveNW: Cursor := crSizeNW;
    oecMoveSE: Cursor := crSizeSE;
    oecHandPoint: Cursor := crHandPoint;
    oecText: Cursor := crIBeam;
    else Cursor := crDefault;
  end;
end;

function TVectorialTool.DoToolDown(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF; rightBtn: boolean): TRect;
var
  viewPt: TPointF;
  cur: TOriginalEditorCursor;
  handled: boolean;
begin
  FRightDown := rightBtn;
  FLeftDown := not rightBtn;
  FLastPos := ptF;
  if Assigned(FShape) then
  begin
    viewPt := FEditor.Matrix*ptF;
    FEditor.MouseDown(rightBtn, FShiftState, viewPt.X,viewPt.Y, cur, handled);
    if not handled and Assigned(FShape) then
      FShape.MouseDown(rightBtn, FShiftState, ptF.X,ptF.Y, cur, handled);
    UpdateCursor(cur);
    result := EmptyRect;
    if handled then exit
    else
    begin
      ValidateAction;
      FreeAndNil(FShape);
    end;
  end;

  if FShape=nil then
  begin
    FSwapColor:= rightBtn;
    FShape := CreateShape;
    FQuickDefine := true;
    FQuickDefineStartPoint := RoundCoordinate(ptF);
    FShape.QuickDefine(FQuickDefineStartPoint,FQuickDefineStartPoint);
    AssignShapeStyle;
    FShape.OnChange:= @ShapeChange;
    FShape.OnEditingChange:=@ShapeEditingChange;
    result := UpdateShape(toolDest);
  end;
end;

function TVectorialTool.DoToolMove(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF): TRect;
var
  secondCoord, s: TPointF;
  avg: single;
  viewPt: TPointF;
  handled: boolean;
  cur: TOriginalEditorCursor;
begin
  FLastPos := ptF;
  if FQuickDefine then
  begin
    secondCoord := RoundCoordinate(ptF);
    if FQuickSquare then
    begin
      s := secondCoord-FQuickDefineStartPoint;
      avg := sqrt(abs(s.x*s.y));
      if s.x > 0 then secondCoord.x := FQuickDefineStartPoint.x + avg else secondCoord.x := FQuickDefineStartPoint.x - avg;
      if s.y > 0 then secondCoord.y := FQuickDefineStartPoint.y + avg else secondCoord.y := FQuickDefineStartPoint.y - avg;
    end;
    FShape.QuickDefine(FQuickDefineStartPoint, secondCoord);
    result := EmptyRect;
  end else
  begin
    viewPt := FEditor.Matrix*ptF;
    FEditor.MouseMove(FShiftState, viewPt.X,viewPt.Y, cur, handled);
    if not handled and Assigned(FShape) then
      FShape.MouseMove(FShiftState, ptF.X,ptF.Y, cur, handled);
    UpdateCursor(cur);
    result := EmptyRect;
  end;
end;

function TVectorialTool.DoToolUpdate(toolDest: TBGRABitmap): TRect;
begin
  if Assigned(FShape) then AssignShapeStyle;
  result := EmptyRect;
end;

constructor TVectorialTool.Create(AManager: TToolManager);
begin
  inherited Create(AManager);
  Action.ChangeBoundsNotified:= true;
  FPreviousUpdateBounds := EmptyRect;
  FEditor := TVectorOriginalEditor.Create(nil);
  FEditor.GridMatrix := AffineMatrixScale(0.5,0.5);
  FPreviousEditorBounds := EmptyRect;
end;

function TVectorialTool.ToolUp: TRect;
var
  viewPt: TPointF;
  cur: TOriginalEditorCursor;
  handled, wasRight, wasLeft: boolean;
begin
  wasRight := FRightDown;
  wasLeft := FLeftDown;
  FRightDown := false;
  FLeftDown := false;
  if FQuickDefine then
  begin
    FQuickDefine := false;
    result := EmptyRect;
  end else
  begin
    viewPt := FEditor.Matrix*FLastPos;
    FEditor.MouseUp(wasRight, FShiftState, viewPt.X,viewPt.Y, cur, handled);
    if not handled and Assigned(FShape) then
      FShape.MouseUp(wasRight, FShiftState, FLastPos.X,FLastPos.Y, cur, handled);
    UpdateCursor(cur);
    result := EmptyRect;
  end;
  if SlowShape then
    result := UpdateShape(GetToolDrawingLayer);
end;

function TVectorialTool.ToolKeyDown(var key: Word): TRect;
var
  handled: boolean;
begin
  result := EmptyRect;
  if Key = VK_SHIFT then
  begin
    if FQuickDefine then
    begin
      FQuickSquare:= true;
      Key := 0;
    end else
    begin
      Include(FShiftState, ssShift);
      Key := 0;
    end;
  end else
  if (Key = VK_CONTROL) and not FQuickDefine then
  begin
    Include(FShiftState, ssCtrl);
    FEditor.GridActive := true;
    Key := 0;
  end else
  if (Key = VK_MENU) and not FQuickDefine then
  begin
    Include(FShiftState, ssAlt);
    Key := 0;
  end else
  if (Key = VK_RETURN) and not FQuickDefine and
    Assigned(FShape) then
  begin
    ValidateActionPartially;
    FreeAndNil(FShape);
    result := OnlyRenderChange;
    Cursor := crDefault;
    Key := 0;
  end else
  if (Key = VK_ESCAPE) and not FQuickDefine and
    Assigned(FShape) then
  begin
    CancelActionPartially;
    FreeAndNil(FShape);
    result := OnlyRenderChange;
    Cursor := crDefault;
    Key := 0;
  end else
  begin
    FEditor.KeyDown(FShiftState, LCLKeyToSpecialKey(Key, FShiftState), handled);
    if handled then Key := 0;
  end;
end;

function TVectorialTool.ToolKeyUp(var key: Word): TRect;
var
  handled: boolean;
begin
  result := EmptyRect;
  if Key = VK_SHIFT then
  begin
    if FQuickDefine then
    begin
      FQuickSquare:= false;
      Key := 0;
    end else
    begin
      Exclude(FShiftState, ssShift);
      Key := 0;
    end;
  end else
  if (Key = VK_CONTROL) and not FQuickDefine then
  begin
    Exclude(FShiftState, ssCtrl);
    FEditor.GridActive := false;
    Key := 0;
  end else
  if (Key = VK_MENU) and not FQuickDefine then
  begin
    Exclude(FShiftState, ssAlt);
    Key := 0;
  end else
  begin
    FEditor.KeyUp(FShiftState, LCLKeyToSpecialKey(Key, FShiftState), handled);
    if handled then Key := 0;
  end;
end;

function TVectorialTool.Render(VirtualScreen: TBGRABitmap; VirtualScreenWidth,
  VirtualScreenHeight: integer;
  BitmapToVirtualScreen: TBitmapToVirtualScreenFunction): TRect;
var
  orig, xAxis, yAxis: TPointF;
begin
  orig := BitmapToVirtualScreen(PointF(0,0));
  xAxis := BitmapToVirtualScreen(PointF(1,0));
  yAxis := BitmapToVirtualScreen(PointF(0,1));
  FEditor.Matrix := AffineMatrix(xAxis-orig,yAxis-orig,orig);
  FEditor.Clear;
  if Assigned(FShape) then FShape.ConfigureEditor(FEditor);
  if Assigned(VirtualScreen) then
    Result:= FEditor.Render(VirtualScreen, rect(0,0,VirtualScreen.Width,VirtualScreen.Height))
  else
    Result:= FEditor.GetRenderBounds(rect(0,0,VirtualScreenWidth,VirtualScreenHeight));
  FPreviousEditorBounds := result
end;

destructor TVectorialTool.Destroy;
begin
  if Assigned(FShape) then
  begin
    ValidateAction;
    FShape.Free;
  end;
  FEditor.Free;
  inherited Destroy;
end;

{ TToolEllipse }

function TToolEllipse.CreateShape: TVectorShape;
begin
  result := TEllipseShape.Create(nil);
end;

{ TToolRectangle }

function TToolRectangle.CreateShape: TVectorShape;
begin
  result := TRectShape.Create(nil);
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
  d: TPointF;
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

   if result = [] then
   begin
     d := ptF - (rectOrigin+rectDest)*0.5;
     if d*d <= sqr(maxDist) then result += [btCenter];
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
    rectMovingPointClick := RoundCoordinate(ptF);
    rectMovingPoint := false;
    rectMovingCenterPoint := false;
    rectMovingX := nil;
    rectMovingY := nil;
    test := BorderTest(ptF);
    if test = [btCenter] then
    begin
      rectMovingCenterPoint := true;
      rectMovingPoint := true;
    end;
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
      rectMovingPointValueDiff := pointF(0,0);
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
      ValidateActionPartially;
      afterRectDrawing:= false;
    end;
    rectDrawing := true;
    rectOrigin := RoundCoordinate(ptF);
    rectDest := RoundCoordinate(ptF);
    PrepareDrawing(rightBtn);
  end;
end;

function TToolRectangular.DoToolMove(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF): TRect;
var
  delta: TPointF;
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
    if Assigned(rectMovingX) then rectMovingX^ := RoundCoordinate(ptF).X+rectMovingPointValueDiff.X;
    if Assigned(rectMovingY) then rectMovingY^ := RoundCoordinate(ptF).Y+rectMovingPointValueDiff.Y;
    if rectMovingCenterPoint then
    begin
      delta := ptF-rectMovingPointClick;
      rectMovingPointClick := ptF;
      rectOrigin += delta;
      rectDest += delta;
    end;
    ApplyConstraint(rectMovingX,rectMovingY);
    result := FinishShape(toolDest);
  end else
  if rectDrawing and (rectDest <> RoundCoordinate(ptF)) then
  begin
    rectDest := RoundCoordinate(ptF);
    ApplyConstraint(@rectDest.X,@rectDest.Y);
    result := UpdateShape(toolDest);
  end;
  UpdateCursor(ptF);
end;

procedure TToolRectangular.PrepareDrawing(rightBtn: boolean);
begin
  swapedColor := rightBtn;
end;

function TToolRectangular.ToolUp: TRect;
begin
  if rectMovingPoint then
  begin
    rectMovingPoint := false;
    rectMovingX := nil;
    rectMovingY := nil;
    UpdateCursor(lastMousePos);
    result := LeaveMovingPoint;
  end else
  if rectDrawing then
  begin
    result := ValidateDrawing;
    if IsRectEmpty(result) then
      result := OnlyRenderChange;
    afterRectDrawing := true;
    UpdateCursor(lastMousePos);
  end
  else
    result := EmptyRect;
end;

function TToolRectangular.ToolKeyDown(var key: Word): TRect;
begin
  result := EmptyRect;
  if Key = VK_SHIFT then
  begin
    squareConstraint:= true;
    Key := 0;
  end else
  if (Key = VK_RETURN) and afterRectDrawing then
  begin
    ValidateActionPartially;
    afterRectDrawing:= false;
    result := OnlyRenderChange;
    Cursor := crDefault;
    Key := 0;
  end else
  if (Key = VK_ESCAPE) and afterRectDrawing then
  begin
    CancelActionPartially;
    afterRectDrawing:= false;
    result := OnlyRenderChange;
    Cursor := crDefault;
    Key := 0;
  end;
end;

function TToolRectangular.ToolKeyUp(var key: Word): TRect;
begin
  if Key = VK_SHIFT then
  begin
    squareConstraint:= false;
    Key := 0;
  end;
  result := EmptyRect;
end;

function TToolRectangular.Render(VirtualScreen: TBGRABitmap;
  VirtualScreenWidth, VirtualScreenHeight: integer; BitmapToVirtualScreen: TBitmapToVirtualScreenFunction): TRect;
var curPt: TPointF;
begin
  result := EmptyRect;
  if afterRectDrawing then
  begin
    curPt := BitmapToVirtualScreen(rectOrigin);
    result := RectUnion(result, NicePoint(VirtualScreen, curPt.X,curPt.Y));
    curPt := BitmapToVirtualScreen(rectDest);
    result := RectUnion(result, NicePoint(VirtualScreen, curPt.X,curPt.Y));
    if RenderAllCornerPositions then
    begin
      curPt := BitmapToVirtualScreen(PointF(rectDest.X,rectOrigin.Y));
      result := RectUnion(result, NicePoint(VirtualScreen, curPt.X,curPt.Y));
      curPt := BitmapToVirtualScreen(PointF(rectOrigin.X,rectDest.Y));
      result := RectUnion(result, NicePoint(VirtualScreen, curPt.X,curPt.Y));
      curPt := BitmapToVirtualScreen((rectOrigin+rectDest)*0.5);
      result := RectUnion(result, NicePoint(VirtualScreen, curPt.X,curPt.Y));
    end;
  end;
end;

function TToolRectangular.ValidateDrawing: TRect;
begin
  if rectDrawing then
  begin
    rectDrawing := false;
    result := FinishShape(GetToolDrawingLayer);
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
      Cursor := crSizeNS else
    if test = [btCenter] then Cursor := crSizeAll;
  end;
end;

function TToolRectangular.DoToolUpdate(toolDest: TBGRABitmap): TRect;
begin
  if rectDrawing then
    result := UpdateShape(toolDest)
  else
  if afterRectDrawing then
    result := FinishShape(toolDest)
  else
    result := EmptyRect;
end;

procedure TToolRectangular.ApplyConstraint(px, py: PSingle);
var tx,ty: Single;
  px2,py2: PSingle;
  ratio: single;
begin
  if not ConstraintEnabled then exit;
  if squareConstraint then ratio := 1
  else ratio := Manager.ToolRatio;
  if ratio <= 0 then exit;

  if px <> nil then
    tx := abs(rectDest.X-rectOrigin.X) else tx := -1;
  if py <> nil then
    ty := abs(rectDest.Y-rectOrigin.Y) else ty := -1;

  if (tx <> -1) and (ty <> -1) then
  begin
    if ty*ratio > tx then tx := -1 else ty := -1;
  end;
  if (tx = -1) and (ty <> -1) then tx := ty*ratio;
  if (ty = -1) and (tx <> -1) then ty := tx/ratio;

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
    if px^ >= px2^ then px^ := px2^+tx else px^ := px2^-tx;
  end;
  if (py <> nil) and (py2 <> nil) then
  begin
    if py^ >= py2^ then py^ := py2^+ty else py^ := py2^-ty;
  end;
end;

function TToolRectangular.RoundCoordinate(ptF: TPointF): TPointF;
begin
  result := PointF(Round(ptF.X),round(ptF.y));
end;

function TToolRectangular.LeaveMovingPoint: TRect;
begin
  result := EmptyRect;
end;

function TToolRectangular.GetStatusText: string;
begin
  if rectDrawing or afterRectDrawing then
    result := 'x1 = '+inttostr(round(rectOrigin.x))+'|y1 = '+inttostr(round(rectOrigin.y))+'|'+
    'x2 = '+inttostr(round(rectDest.x))+'|y2 = '+inttostr(round(rectDest.y))+'|'+
    'Δx = '+inttostr(abs(round(rectDest.x-rectOrigin.x))+1)+'|Δy = '+inttostr(abs(round(rectDest.y-rectOrigin.y))+1)
  else
    Result:=inherited GetStatusText;
end;

function TToolRectangular.ConstraintEnabled: boolean;
begin
  result := true;
end;

constructor TToolRectangular.Create(AManager: TToolManager);
begin
  inherited Create(AManager);
  Action.ChangeBoundsNotified:= true;
  rectMovingPoint := false;
  afterRectDrawing:= false;
  squareConstraint := false;
end;

destructor TToolRectangular.Destroy;
begin
  if afterRectDrawing then ValidateAction;
  inherited Destroy;
end;

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
  result := manager.ToolPenWidth/4*Manager.ToolEraserAlpha/255;
end;

function TToolErase.StartDrawing(toolDest: TBGRABitmap; ptF: TPointF;
  rightBtn: boolean): TRect;
var ix,iy: integer;
  areaCopy, mask: TBGRABitmap;
begin
  if Manager.ToolEraserMode = emSoften then
  begin
    result := GetShapeBounds([ptF],Manager.ToolPenWidth+BlurRadius);
    if IntersectRect(result, result, rect(0,0,toolDest.width,toolDest.height)) then
    begin
      areaCopy := toolDest.GetPart(result) as TBGRABitmap;
      ApplySoften(areaCopy);
      mask := TBGRABitmap.Create(result.Right-result.left,result.bottom-result.top, BGRABlack);
      mask.LinearAntialiasing := true;
      mask.DrawLineAntialias(ptF.X-result.left,ptF.Y-result.top,ptF.X-result.left,ptF.Y-result.top,
        Manager.ApplyPressure(BGRA(255,255,255,255)),
        Manager.ToolPenWidth,True);
      mask.ScanOffset := Point(-result.left,-result.top);
      areaCopy.ScanOffset := Point(-result.left,-result.top);
      toolDest.CrossFade(result,toolDest,areaCopy,mask,dmSet);
      mask.Free;
      areaCopy.Free;
    end;
  end else
  begin
    if snapToPixel and (Manager.ToolPenWidth = 1) then
    begin
      ix := round(ptF.X);
      iy := round(ptF.Y);
      toolDest.ErasePixel(ix,iy,round(Manager.ToolEraserAlpha*Manager.ToolPressure));
      result := rect(ix,iy,ix+1,iy+1);
    end
    else
    begin
      result := GetShapeBounds([ptF],Manager.ToolPenWidth);
      toolDest.ClipRect := result;
      toolDest.EraseLineAntialias(ptF.X,ptF.Y,ptF.X,ptF.Y,round(Manager.ToolEraserAlpha*Manager.ToolPressure),Manager.ToolPenWidth,True);
      toolDest.NoClip;
    end;
  end;
end;

function TToolErase.ContinueDrawing(toolDest: TBGRABitmap; originF,
  destF: TPointF): TRect;
var areaCopy, mask: TBGRABitmap;
begin
  if Manager.ToolEraserMode = emSoften then
  begin
    result := GetShapeBounds([destF,originF],Manager.ToolPenWidth+BlurRadius);
    if IntersectRect(result, result, rect(0,0,toolDest.width,toolDest.height)) then
    begin
      areaCopy := toolDest.GetPart(result) as TBGRABitmap;
      ApplySoften(areaCopy);
      mask := TBGRABitmap.Create(result.Right-result.left,result.bottom-result.top, BGRABlack);
      mask.LinearAntialiasing := true;
      mask.DrawLineAntialias(destF.X-result.left,destF.Y-result.top,originF.X-result.left,originF.Y-result.top,
        Manager.ApplyPressure(BGRA(255,255,255,255)),
        Manager.ToolPenWidth,false);
      mask.ScanOffset := Point(-result.left,-result.top);
      areaCopy.ScanOffset := Point(-result.left,-result.top);
      toolDest.CrossFade(result,toolDest,areaCopy,mask,dmSet);
      mask.Free;
      areaCopy.Free;
    end;
  end else
  if snapToPixel and (Manager.ToolPenWidth = 1) then
  begin
    toolDest.EraseLineAntialias(round(destF.X),round(destF.Y),round(originF.X),round(originF.Y),round(Manager.ToolEraserAlpha*Manager.ToolPressure),false);
    result := GetShapeBounds([destF,originF],1);
  end else
  begin
    toolDest.EraseLineAntialias(destF.X,destF.Y,originF.X,originF.Y,round(Manager.ToolEraserAlpha*Manager.ToolPressure),Manager.ToolPenWidth,False);
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
  r: TRect;
begin
  if rightBtn then penColor := Manager.ToolBackColor else penColor := Manager.ToolForeColor;
  if (snapToPixel or Manager.ToolOptionAliasing) and (Manager.ToolPenWidth = 1) and (Manager.GetToolTexture = nil) then
  begin
    ix := round(ptF.X);
    iy := round(ptF.Y);
    toolDest.DrawPixel(ix,iy,Manager.ApplyPressure(penColor));
    result := rect(ix,iy,ix+1,iy+1);
  end else
  begin
    result := GetShapeBounds([ptF],Manager.ToolPenWidth);
    toolDest.ClipRect := result;
    if Manager.ToolOptionAliasing then
    begin
      r := rect(round(ptF.X-Manager.ToolPenWidth/2+0.5),round(ptF.Y-Manager.ToolPenWidth/2+0.5),
                round(ptF.X+Manager.ToolPenWidth/2+0.5),round(ptF.Y+Manager.ToolPenWidth/2+0.5));
      if Manager.GetToolTextureAfterAlpha <> nil then
        toolDest.FillEllipseInRect(r,Manager.GetToolTextureAfterAlpha,dmDrawWithTransparency)
      else
        toolDest.FillEllipseInRect(r,Manager.ApplyPressure(penColor),dmDrawWithTransparency);
    end
    else
    begin
      if Manager.GetToolTextureAfterAlpha <> nil then
        toolDest.FillEllipseAntialias(ptF.X,ptF.Y,Manager.ToolPenWidth/2,Manager.ToolPenWidth/2,Manager.GetToolTextureAfterAlpha)
      else
        toolDest.FillEllipseAntialias(ptF.X,ptF.Y,Manager.ToolPenWidth/2,Manager.ToolPenWidth/2,Manager.ApplyPressure(penColor));
    end;
    toolDest.NoClip;
  end;
end;

function TToolPen.ContinueDrawing(toolDest: TBGRABitmap; originF, destF: TPointF): TRect;
var
  pts: ArrayOfTPointF;
begin
  if (snapToPixel or Manager.ToolOptionAliasing) and (Manager.ToolPenWidth = 1) and (Manager.GetToolTexture = nil) then
  begin
    if Manager.ToolOptionAliasing then
      toolDest.DrawLine(round(destF.X),round(destF.Y),round(originF.X),round(originF.Y),Manager.ApplyPressure(penColor),false)
    else
      toolDest.DrawLineAntialias(round(destF.X),round(destF.Y),round(originF.X),round(originF.Y),Manager.ApplyPressure(penColor),false);
    result := GetShapeBounds([destF,originF],1);
  end else
  begin
    result := GetShapeBounds([destF,originF],Manager.ToolPenWidth+1);
    toolDest.ClipRect := result;
    if Manager.ToolOptionAliasing then
    begin
      pts := toolDest.Pen.ComputePolyline([PointF(destF.X,destF.Y),PointF(originF.X,originF.Y)],Manager.ToolPenWidth,BGRAPixelTransparent,False);
      if Manager.GetToolTextureAfterAlpha <> nil then
        toolDest.FillPoly(pts,Manager.GetToolTextureAfterAlpha,dmDrawWithTransparency)
      else
        toolDest.FillPoly(pts,Manager.ApplyPressure(penColor),dmDrawWithTransparency);
    end else
    begin
      if Manager.GetToolTextureAfterAlpha <> nil then
        toolDest.DrawLineAntialias(destF.X,destF.Y,originF.X,originF.Y,Manager.GetToolTextureAfterAlpha,Manager.ToolPenWidth,False)
      else
        toolDest.DrawLineAntialias(destF.X,destF.Y,originF.X,originF.Y,Manager.ApplyPressure(penColor),Manager.ToolPenWidth,False);
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

function TToolPen.ToolKeyDown(var key: Word): TRect;
begin
  if key = VK_CONTROL then
  begin
    snapToPixel := true;
    Key := 0;
  end;
  Result:=EmptyRect;
end;

function TToolPen.ToolKeyUp(var key: Word): TRect;
begin
  if key = VK_CONTROL then
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

function TToolHand.FixSelectionTransform: boolean;
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
    result := OnlyRenderChange;
  end else
    result := EmptyRect;
end;

procedure TToolHand.DoToolMoveAfter(pt: TPoint; ptF: TPointF);
begin
  if handMoving then handOrigin := pt;
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

