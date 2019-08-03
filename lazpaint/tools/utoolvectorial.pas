unit UToolVectorial;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, BGRABitmap, BGRABitmapTypes,
  BGRALayerOriginal, LCVectorOriginal,
  UTool, UImageType;

type
  { TVectorialTool }

  TVectorialTool = class(TGenericTool)
  private
    function GetIsHandDrawing: boolean;
    function GetIsIdle: boolean;
  protected
    FShape: TVectorShape;
    FSwapColor: boolean;
    FQuickDefine: Boolean;
    FQuickDefineStartPoint, FQuickDefineEndPoint: TPointF;
    FQuickSquare: boolean;
    FPreviousUpdateBounds, FPreviousEditorBounds: TRect;
    FEditor: TBGRAOriginalEditor;
    FShiftState: TShiftState;
    FRightDown, FLeftDown: boolean;
    FLastPos: TPointF;
    FLastShapeTransform: TAffineMatrix;
    FUseOriginal: boolean;
    function AlwaysRasterizeShape: boolean; virtual;
    function CreateShape: TVectorShape; virtual; abstract;
    function UseOriginal: boolean; virtual;
    function GetCustomShapeBounds(ADestBounds: TRect; AMatrix: TAffineMatrix; {%H-}ADraft: boolean): TRect; virtual;
    procedure DrawCustomShape(ADest: TBGRABitmap; AMatrix: TAffineMatrix; ADraft: boolean); virtual;
    procedure AssignShapeStyle(AMatrix: TAffineMatrix); virtual;
    procedure QuickDefineShape(AStart,AEnd: TPointF); virtual;
    function RoundCoordinate(ptF: TPointF): TPointF; virtual;
    function GetIsSelectingTool: boolean; override;
    function UpdateShape(toolDest: TBGRABitmap): TRect; virtual;
    function VectorTransform: TAffineMatrix;
    procedure UpdateCursor(ACursor: TOriginalEditorCursor);
    function FixLayerOffset: boolean; override;
    function DoToolDown({%H-}toolDest: TBGRABitmap; {%H-}pt: TPoint; {%H-}ptF: TPointF; rightBtn: boolean): TRect; override;
    function DoToolMove({%H-}toolDest: TBGRABitmap; {%H-}pt: TPoint; {%H-}ptF: TPointF): TRect; override;
    function DoToolUpdate({%H-}toolDest: TBGRABitmap): TRect; override;
    procedure ShapeChange({%H-}ASender: TObject; ABounds: TRectF; ADiff: TVectorShapeDiff); virtual;
    procedure ShapeEditingChange({%H-}ASender: TObject); virtual;
    procedure ShapeRemoveQuery({%H-}ASender: TObject; var AHandled: boolean);
    function GetStatusText: string; override;
    function SlowShape: boolean; virtual;
    procedure QuickDefineEnd; virtual;
    procedure OnTryStop({%H-}sender: TCustomLayerAction); override;
    procedure UpdateUseOriginal;
  public
    function ValidateShape: TRect;
    function CancelShape: TRect;
    constructor Create(AManager: TToolManager); override;
    function ToolUp: TRect; override;
    function ToolKeyDown(var key: Word): TRect; override;
    function ToolKeyPress(var key: TUTF8Char): TRect; override;
    function ToolKeyUp(var key: Word): TRect; override;
    function Render(VirtualScreen: TBGRABitmap; {%H-}VirtualScreenWidth, {%H-}VirtualScreenHeight: integer; BitmapToVirtualScreen: TBitmapToVirtualScreenFunction):TRect; override;
    property IsIdle: boolean read GetIsIdle;
    property IsHandDrawing: boolean read GetIsHandDrawing;
    destructor Destroy; override;
  end;

implementation

uses LazPaintType, LCVectorRectShapes, BGRASVGOriginal, ULoading, BGRATransform, math,
  UStateType, UImageDiff, Controls, BGRAPen, UResourceStrings, ugraph;

{ TVectorialTool }

procedure TVectorialTool.ShapeChange(ASender: TObject; ABounds: TRectF; ADiff: TVectorShapeDiff);
var
  toolDest: TBGRABitmap;
  r: TRect;
  matrix: TAffineMatrix;
begin
  toolDest := GetToolDrawingLayer;
  matrix := VectorTransform;
  r := (matrix*TAffineBox.AffineBox(ABounds)).RectBounds;
  UpdateShape(toolDest);
  Action.NotifyChange(toolDest, r);
  with FShape.GetRenderBounds(rect(0,0,toolDest.Width,toolDest.Height),matrix,[]) do
    FPreviousUpdateBounds := rect(floor(Left),floor(Top),ceil(Right),ceil(Bottom));
  if r.IsEmpty then ShapeEditingChange(ASender);
  ADiff.Free;
end;

procedure TVectorialTool.ShapeEditingChange(ASender: TObject);
var
  toolDest: TBGRABitmap;
  newEditorBounds, r: TRect;
begin
  toolDest := GetToolDrawingLayer;
  with (FEditor.Matrix*PointF(toolDest.Width,toolDest.Height)) do
    newEditorBounds := FEditor.GetRenderBounds(rect(0,0,ceil(x),ceil(y)));
  r := RectUnion(FPreviousEditorBounds,newEditorBounds);
  if not r.IsEmpty then
  begin
    Manager.Image.RenderMayChange(r,false);
    Manager.Image.OnImageChanged.NotifyObservers;
  end;
  FPreviousEditorBounds := newEditorBounds;
end;

procedure TVectorialTool.ShapeRemoveQuery(ASender: TObject;
  var AHandled: boolean);
var
  r: TRect;
  toolDest: TBGRABitmap;
begin
  if ASender <> FShape then exit;
  toolDest := GetToolDrawingLayer;
  r := CancelShape;
  Action.NotifyChange(toolDest, r);
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

procedure TVectorialTool.QuickDefineEnd;
begin
  //nothing
end;

procedure TVectorialTool.OnTryStop(sender: TCustomLayerAction);
begin
  ValidateShape;
end;

procedure TVectorialTool.UpdateUseOriginal;
begin
  if not IsSelectingTool and Manager.Image.SelectionMaskEmpty and
     Manager.Image.LayerOriginalDefined[Manager.Image.CurrentLayerIndex] and
     Manager.Image.LayerOriginalKnown[Manager.Image.CurrentLayerIndex] and
    (Manager.Image.LayerOriginalClass[Manager.Image.CurrentLayerIndex] = TVectorOriginal) then
    FUseOriginal:= Assigned(Manager.Image.LayerOriginal[Manager.Image.CurrentLayerIndex])
  else
    FUseOriginal:= false;
end;

function TVectorialTool.ValidateShape: TRect;
var
  diff: TComposedImageDifference;
  layerId: LongInt;
  replaceDiff: TReplaceLayerByVectorOriginalDifference;
  transf: TAffineMatrix;
  addDiff: TAddShapeToVectorOriginalDifference;
  rF: TRectF;
begin
  if Assigned(FShape) then
  begin
    FShape.OnChange:= nil;
    FShape.OnEditingChange:= nil;
    FShape.OnRemoveQuery:= nil;
    if not AlwaysRasterizeShape and Manager.Image.SelectionMaskEmpty then
    begin
      CancelAction;
      if FShape.Usermode = vsuCreate then FShape.Usermode:= vsuEdit;
      rF := FShape.GetRenderBounds(rect(0,0,Manager.Image.Width,Manager.Image.Height), VectorTransform);
      if rF.IntersectsWith(rectF(0,0,Manager.Image.Width,Manager.Image.Height)) then
      begin
        layerId := Manager.Image.LayerId[Manager.Image.CurrentLayerIndex];
        if UseOriginal then
        begin
          addDiff := TAddShapeToVectorOriginalDifference.Create(Manager.Image.CurrentState,layerId,FShape);
          Manager.Image.AddUndo(addDiff);
        end
        else
        begin
          transf := VectorTransform;
          diff := TComposedImageDifference.Create;
          replaceDiff := TReplaceLayerByVectorOriginalDifference.Create(Manager.Image.CurrentState,Manager.Image.CurrentLayerIndex,
                           Manager.Image.LayerOriginalClass[Manager.Image.CurrentLayerIndex]=TVectorOriginal);
          diff.Add(replaceDiff);
          transf := AffineMatrixInverse(Manager.Image.LayerOriginalMatrix[Manager.Image.CurrentLayerIndex])*transf;
          FShape.Transform(transf);
          addDiff := TAddShapeToVectorOriginalDifference.Create(Manager.Image.CurrentState,layerId,FShape);
          diff.Add(addDiff);
          Manager.Image.AddUndo(diff);
        end;
        Manager.Image.ImageMayChange(addDiff.ChangingBounds);
      end else
        FShape.Free;
      FShape := nil;
      FEditor.Clear;
    end else
    begin
      ValidateActionPartially;
      FreeAndNil(FShape);
      FEditor.Clear;
    end;
    Cursor := crDefault;
    result := OnlyRenderChange;
    UpdateUseOriginal;
  end else
    result := EmptyRect;
end;

function TVectorialTool.CancelShape: TRect;
begin
  CancelActionPartially;
  FreeAndNil(FShape);
  FEditor.Clear;
  Cursor := crDefault;
  result := OnlyRenderChange;
end;

function TVectorialTool.GetIsHandDrawing: boolean;
begin
  result := Assigned(FShape) and (FQuickDefine or (FShape.Usermode = vsuCreate));
end;

function TVectorialTool.GetIsIdle: boolean;
begin
  result := FShape = nil;
end;

function TVectorialTool.AlwaysRasterizeShape: boolean;
begin
  result := false;
end;

function TVectorialTool.UseOriginal: boolean;
begin
  result := FUseOriginal;
end;

function TVectorialTool.GetCustomShapeBounds(ADestBounds: TRect; AMatrix: TAffineMatrix; ADraft: boolean): TRect;
begin
  with FShape.GetRenderBounds(ADestBounds,AMatrix,[]) do
    result := rect(floor(Left),floor(Top),ceil(Right),ceil(Bottom));
end;

procedure TVectorialTool.DrawCustomShape(ADest: TBGRABitmap; AMatrix: TAffineMatrix; ADraft: boolean);
begin
  FShape.Render(ADest,AMatrix,ADraft);
end;

procedure TVectorialTool.AssignShapeStyle(AMatrix: TAffineMatrix);
var
  f: TVectorShapeFields;
  zoom: Single;
begin
  zoom := (VectLen(AMatrix[1,1],AMatrix[2,1])+VectLen(AMatrix[1,2],AMatrix[2,2]))/2;
  f:= FShape.Fields;
  if vsfPenFill in f then
  begin
    if Manager.ToolOptionDrawShape then
    begin
      if (not (vsfBackFill in f) or not Manager.ToolOptionFillShape) and (Manager.GetToolTexture <> nil) then
        FShape.PenFill.SetTexture(Manager.GetToolTexture,AMatrix,Manager.ToolTextureOpacity)
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
  if vsfPenWidth in f then FShape.PenWidth := zoom*Manager.ToolPenWidth;
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
    end else
      FShape.BackFill.Clear;
  end;
end;

procedure TVectorialTool.QuickDefineShape(AStart, AEnd: TPointF);
begin
  FShape.QuickDefine(AStart, AEnd);
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
  draft: Boolean;
  matrix: TAffineMatrix;
begin
  result := FPreviousUpdateBounds;
  RestoreBackupDrawingLayer;
  matrix := VectorTransform;
  draft := (FRightDown or FLeftDown) and SlowShape;
  newBounds := GetCustomShapeBounds(toolDest.ClipRect,matrix,draft);
  result := RectUnion(result, newBounds);
  oldClip := toolDest.IntersectClip(newBounds);
  DrawCustomShape(toolDest,matrix,draft);
  toolDest.ClipRect := oldClip;
  FPreviousUpdateBounds := newBounds;
end;

function TVectorialTool.VectorTransform: TAffineMatrix;
begin
  if not UseOriginal then
    result := AffineMatrixIdentity
  else
    result := Manager.Image.LayerOriginalMatrix[Manager.Image.CurrentLayerIndex];
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

function TVectorialTool.FixLayerOffset: boolean;
begin
  Result:= false;
end;

function TVectorialTool.DoToolDown(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF; rightBtn: boolean): TRect;
var
  viewPt: TPointF;
  cur: TOriginalEditorCursor;
  handled: boolean;
begin
  result := EmptyRect;
  FRightDown := rightBtn;
  FLeftDown := not rightBtn;
  FLastPos := ptF;
  if Assigned(FShape) then
  begin
    with Manager.Image.LayerOffset[Manager.Image.CurrentLayerIndex] do
      viewPt := FEditor.Matrix*AffineMatrixInverse(VectorTransform)*AffineMatrixTranslation(X,Y)*ptF;
    FEditor.MouseDown(rightBtn, FShiftState, viewPt.X,viewPt.Y, cur, handled);
    if not handled and Assigned(FShape) then
      FShape.MouseDown(rightBtn, FShiftState, ptF.X,ptF.Y, cur, handled);
    UpdateCursor(cur);
    if handled then exit
    else result := RectUnion(result, ValidateShape);
  end;

  if FShape=nil then
  begin
    if UseOriginal and
      ((Manager.Image.LayerOriginal[Manager.Image.CurrentLayerIndex] as TVectorOriginal).ShapeCount >= 10) then
    begin
      MessagePopup(rsTooManyShapesInLayer, 3000);
    end
    else
    if Manager.Image.LayerOriginalClass[Manager.Image.CurrentLayerIndex] = TBGRALayerSVGOriginal then
    begin
      MessagePopup(rsCannotDrawShapeOnSVGLayer, 3000);
    end
    else
    begin
      toolDest := GetToolDrawingLayer;
      FSwapColor:= rightBtn;
      FShape := CreateShape;
      FQuickDefine := true;
      FQuickDefineStartPoint := RoundCoordinate(ptF);
      FQuickDefineEndPoint := FQuickDefineStartPoint;
      FShape.BeginUpdate;
        QuickDefineShape(FQuickDefineStartPoint,FQuickDefineEndPoint);
        FLastShapeTransform := AffineMatrixInverse(VectorTransform);
        FShape.Transform(FLastShapeTransform);
        AssignShapeStyle(FLastShapeTransform);
      FShape.EndUpdate;
      FShape.OnChange:= @ShapeChange;
      FShape.OnEditingChange:=@ShapeEditingChange;
      FShape.OnRemoveQuery:= @ShapeRemoveQuery;
      result := RectUnion(result, UpdateShape(toolDest));
    end;
  end;
end;

function TVectorialTool.DoToolMove(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF): TRect;
var
  s: TPointF;
  avg: single;
  viewPt: TPointF;
  handled: boolean;
  cur: TOriginalEditorCursor;
begin
  FLastPos := ptF;
  if FQuickDefine then
  begin
    FQuickDefineEndPoint := RoundCoordinate(ptF);
    if FQuickSquare then
    begin
      s := FQuickDefineEndPoint-FQuickDefineStartPoint;
      avg := sqrt(abs(s.x*s.y));
      if s.x > 0 then FQuickDefineEndPoint.x := FQuickDefineStartPoint.x + avg else FQuickDefineEndPoint.x := FQuickDefineStartPoint.x - avg;
      if s.y > 0 then FQuickDefineEndPoint.y := FQuickDefineStartPoint.y + avg else FQuickDefineEndPoint.y := FQuickDefineStartPoint.y - avg;
    end;
    FShape.BeginUpdate;
      QuickDefineShape(FQuickDefineStartPoint, FQuickDefineEndPoint);
      FLastShapeTransform := AffineMatrixInverse(VectorTransform);
      FShape.Transform(FLastShapeTransform);
      AssignShapeStyle(FLastShapeTransform);
    FShape.EndUpdate;
    result := OnlyRenderChange;
  end else
  begin
    with Manager.Image.LayerOffset[Manager.Image.CurrentLayerIndex] do
      viewPt := FEditor.Matrix*AffineMatrixInverse(VectorTransform)*AffineMatrixTranslation(X,Y)*ptF;
    FEditor.MouseMove(FShiftState, viewPt.X,viewPt.Y, cur, handled);
    if not handled and Assigned(FShape) then
      FShape.MouseMove(FShiftState, ptF.X,ptF.Y, cur, handled);
    UpdateCursor(cur);
    if handled then result := OnlyRenderChange
    else result := EmptyRect;
  end;
end;

function TVectorialTool.DoToolUpdate(toolDest: TBGRABitmap): TRect;
begin
  if Assigned(FShape) then
  begin
    FShape.BeginUpdate;
    AssignShapeStyle(FLastShapeTransform);
    FShape.EndUpdate;
  end;
  result := EmptyRect;
end;

constructor TVectorialTool.Create(AManager: TToolManager);
begin
  inherited Create(AManager);
  UpdateUseOriginal;
  FPreviousUpdateBounds := EmptyRect;
  FEditor := TVectorOriginalEditor.Create(nil);
  FEditor.GridMatrix := AffineMatrixScale(0.5,0.5);
  FEditor.Focused := true;
  FPreviousEditorBounds := EmptyRect;
  FLastShapeTransform := AffineMatrixIdentity;
end;

function TVectorialTool.ToolUp: TRect;
var
  viewPt: TPointF;
  cur: TOriginalEditorCursor;
  handled, wasRight: boolean;
begin
  wasRight := FRightDown;
  FRightDown := false;
  FLeftDown := false;
  if FQuickDefine then
  begin
    FQuickDefine := false;
    result := EmptyRect;
    QuickDefineEnd;
  end else
  begin
    viewPt := FEditor.Matrix*FLastPos;
    FEditor.MouseUp(wasRight, FShiftState, viewPt.X,viewPt.Y, cur, handled);
    if not handled and Assigned(FShape) then
      FShape.MouseUp(wasRight, FShiftState, FLastPos.X,FLastPos.Y, cur, handled);
    UpdateCursor(cur);
    result := EmptyRect;
  end;
  if SlowShape and Assigned(FShape) then
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
    result := ValidateShape;
    Key := 0;
  end else
  if (Key = VK_ESCAPE) and not FQuickDefine and
    Assigned(FShape) then
  begin
    result := CancelShape;
    Key := 0;
  end else
  begin
    FEditor.KeyDown(FShiftState, LCLKeyToSpecialKey(Key, FShiftState), handled);
    if not handled and Assigned(FShape) then FShape.KeyDown(FShiftState, LCLKeyToSpecialKey(Key, FShiftState), handled);
    if handled then Key := 0;
  end;
end;

function TVectorialTool.ToolKeyPress(var key: TUTF8Char): TRect;
var
  handled: boolean;
begin
  result := EmptyRect;
  FEditor.KeyPress(key, handled);
  if not handled and Assigned(FShape) then FShape.KeyPress(key, handled);
  if handled then Key := #0;
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
    if not handled and Assigned(FShape) then FShape.KeyUp(FShiftState, LCLKeyToSpecialKey(Key, FShiftState), handled);
    if handled then Key := 0;
  end;
end;

function TVectorialTool.Render(VirtualScreen: TBGRABitmap; VirtualScreenWidth,
  VirtualScreenHeight: integer;
  BitmapToVirtualScreen: TBitmapToVirtualScreenFunction): TRect;
var
  orig, xAxis, yAxis: TPointF;
begin
  if Assigned(FShape) then
  begin
    orig := BitmapToVirtualScreen(PointF(0,0));
    xAxis := BitmapToVirtualScreen(PointF(1,0));
    yAxis := BitmapToVirtualScreen(PointF(0,1));
    FEditor.Matrix := AffineMatrix(xAxis-orig,yAxis-orig,orig)*VectorTransform;
    FEditor.Clear;
    if Assigned(FShape) then FShape.ConfigureEditor(FEditor);
    if Assigned(VirtualScreen) then
      Result:= FEditor.Render(VirtualScreen, rect(0,0,VirtualScreen.Width,VirtualScreen.Height))
    else
      Result:= FEditor.GetRenderBounds(rect(0,0,VirtualScreenWidth,VirtualScreenHeight));
  end else
  begin
    result := EmptyRect;
    FEditor.Clear;
  end;
  FPreviousEditorBounds := result;
end;

destructor TVectorialTool.Destroy;
begin
  if Assigned(FShape) then ValidateShape;
  FEditor.Free;
  inherited Destroy;
end;

end.

