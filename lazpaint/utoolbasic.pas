unit UToolBasic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, utool, BGRABitmapTypes, BGRABitmap, BGRALayerOriginal,
  UImage, ULayerAction, LCVectorOriginal, LCLType, UImageType;

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
    FQuickDefineStartPoint, FQuickDefineEndPoint: TPointF;
    FQuickSquare: boolean;
    FPreviousUpdateBounds, FPreviousEditorBounds: TRect;
    FEditor: TBGRAOriginalEditor;
    FShiftState: TShiftState;
    FRightDown, FLeftDown: boolean;
    FLastPos: TPointF;
    FLastShapeTransform: TAffineMatrix;
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
    procedure ShapeChange({%H-}ASender: TObject; ABounds: TRectF); virtual;
    procedure ShapeEditingChange({%H-}ASender: TObject); virtual;
    function GetStatusText: string; override;
    function SlowShape: boolean; virtual;
    procedure QuickDefineEnd; virtual;
    procedure OnTryStop({%H-}sender: TCustomLayerAction); override;
  public
    function ValidateShape: TRect;
    function CancelShape: TRect;
    constructor Create(AManager: TToolManager); override;
    function ToolUp: TRect; override;
    function ToolKeyDown(var key: Word): TRect; override;
    function ToolKeyPress(var key: TUTF8Char): TRect; override;
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

implementation

uses Types, Graphics, ugraph, Controls, LazPaintType,
  UResourceStrings, BGRATransform, Math, BGRAPen, LCVectorRectShapes,
  uimagediff, UStateType, ULoading;

{ TVectorialTool }

procedure TVectorialTool.ShapeChange(ASender: TObject; ABounds: TRectF);
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
  if IsRectEmpty(r) then ShapeEditingChange(ASender);
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
  if not IsRectEmpty(r) then
  begin
    Manager.Image.RenderMayChange(r,false);
    Manager.Image.OnImageChanged.NotifyObservers;
  end;
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

procedure TVectorialTool.QuickDefineEnd;
begin
  //nothing
end;

procedure TVectorialTool.OnTryStop(sender: TCustomLayerAction);
begin
  ValidateShape;
end;

function TVectorialTool.ValidateShape: TRect;
var
  diff: TComposedImageDifference;
  layerId: LongInt;
  replaceDiff: TReplaceLayerByVectorOriginalDifference;
  transf: TAffineMatrix;
begin
  if Assigned(FShape) then
  begin
    FShape.OnChange:= nil;
    FShape.OnEditingChange:= nil;
    if not AlwaysRasterizeShape and Manager.Image.SelectionMaskEmpty then
    begin
      CancelAction;
      layerId := Manager.Image.LayerId[Manager.Image.CurrentLayerIndex];
      if UseOriginal then
        Manager.Image.AddUndo(TAddShapeToVectorOriginalDifference.Create(Manager.Image.CurrentState,layerId,FShape))
      else
      begin
        transf := VectorTransform;
        diff := TComposedImageDifference.Create;
        replaceDiff := TReplaceLayerByVectorOriginalDifference.Create(Manager.Image.CurrentState,Manager.Image.CurrentLayerIndex);
        diff.Add(replaceDiff);
        transf := AffineMatrixInverse(VectorTransform)*transf;
        FShape.Transform(transf);
        diff.Add(TAddShapeToVectorOriginalDifference.Create(Manager.Image.CurrentState,layerId,FShape));
        Manager.Image.AddUndo(diff);
      end;
      FShape := nil;
    end else
    begin
      ValidateActionPartially;
      FreeAndNil(FShape);
    end;
    Cursor := crDefault;
    result := OnlyRenderChange;
  end else
    result := EmptyRect;
end;

function TVectorialTool.CancelShape: TRect;
begin
  CancelActionPartially;
  FreeAndNil(FShape);
  Cursor := crDefault;
  result := OnlyRenderChange;
end;

function TVectorialTool.AlwaysRasterizeShape: boolean;
begin
  result := false;
end;

function TVectorialTool.UseOriginal: boolean;
begin
  result := not IsSelectingTool and Manager.Image.SelectionMaskEmpty and
            Manager.Image.LayerOriginalDefined[Manager.Image.CurrentLayerIndex] and
            Manager.Image.LayerOriginalKnown[Manager.Image.CurrentLayerIndex] and
           (Manager.Image.LayerOriginalClass[Manager.Image.CurrentLayerIndex] = TVectorOriginal);
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
    end;
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
    viewPt := FEditor.Matrix*ptF;
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
  if Assigned(FShape) then AssignShapeStyle(FLastShapeTransform);
  result := EmptyRect;
end;

constructor TVectorialTool.Create(AManager: TToolManager);
begin
  inherited Create(AManager);
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
    result := EmptyRect;
  FPreviousEditorBounds := result;
end;

destructor TVectorialTool.Destroy;
begin
  if Assigned(FShape) then ValidateShape;
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

