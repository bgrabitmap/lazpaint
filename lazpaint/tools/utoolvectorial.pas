// SPDX-License-Identifier: GPL-3.0-only
unit UToolVectorial;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, BGRABitmap, BGRABitmapTypes,
  BGRALayerOriginal, BGRAGraphics, LCVectorOriginal, LCVectorialFill,
  UTool, UImageType, ULayerAction, LCVectorRectShapes, LCVectorMultishape,
  BGRAGradientOriginal, UStateType;

type
  TToolSplineMode = (tsmMovePoint, tsmCurveModeAuto, tsmCurveModeAngle, tsmCurveModeSpline);

function ToolSplineModeFromShape(AShape: TVectorShape): TToolSplineMode;

type
  TFitMode = (fmNever, fmIfChange, fmAlways);

type
  { TVectorialTool }

  TVectorialTool = class(TGenericTool)
  private
    function GetEditor: TBGRAOriginalEditor;
    function GetIsHandDrawing: boolean;
    function GetIsIdle: boolean;
  protected
    FLayerWasEmpty: boolean;
    FShape: TVectorShape;
    FTemporaryStorage: TBGRACustomOriginalStorage;
    FLastDraftUpdate: Boolean;
    FSwapColor: boolean;
    FQuickDefine: Boolean;
    FQuickDefineStartPoint, FQuickDefineEndPoint: TPointF;
    FPreviousUpdateBounds, FPreviousEditorBounds: TRect;
    FEditor: TBGRAOriginalEditor;
    FRightDown, FLeftDown: boolean;
    FLastPos: TPointF;
    FLastShapeTransform: TAffineMatrix;
    FUseOriginal: boolean;
    function AlwaysRasterizeShape: boolean; virtual;
    function CreateShape: TVectorShape; virtual;
    procedure ClearShape; virtual;
    function ShapeClass: TVectorShapeAny; virtual; abstract;
    function UseOriginal: boolean; virtual;
    function HasBrush: boolean; virtual;
    function GetCustomShapeBounds(ADestBounds: TRect; AMatrix: TAffineMatrix; {%H-}ADraft: boolean): TRect; virtual;
    procedure DrawCustomShape(ADest: TBGRABitmap; AMatrix: TAffineMatrix; ADraft: boolean); virtual;
    procedure AssignShapeStyle(AMatrix: TAffineMatrix; AAlwaysFit: boolean); virtual;
    function GetManagerShapeOptions: TShapeOptions; virtual;
    procedure QuickDefineShape(AStart,AEnd: TPointF); virtual;
    function RoundCoordinate(constref ptF: TPointF): TPointF; virtual;
    function GetIsSelectingTool: boolean; override;
    function UpdateShape(toolDest: TBGRABitmap): TRect; virtual;
    function PreferDraftUpdate: boolean;
    function VectorTransform(APixelCentered: boolean): TAffineMatrix;
    procedure UpdateCursor(ACursor: TOriginalEditorCursor);
    function FixLayerOffset: boolean; override;
    function DoToolDown({%H-}toolDest: TBGRABitmap; {%H-}pt: TPoint; {%H-}ptF: TPointF; rightBtn: boolean): TRect; override;
    function DoToolMove({%H-}toolDest: TBGRABitmap; {%H-}pt: TPoint; {%H-}ptF: TPointF): TRect; override;
    function DoToolUpdate({%H-}toolDest: TBGRABitmap): TRect; override;
    function DoToolKeyDown(var key: Word): TRect; override;
    function DoToolKeyUp(var key: Word): TRect; override;
    procedure ShapeChange({%H-}ASender: TObject; ABounds: TRectF; ADiff: TVectorShapeDiff); virtual;
    procedure ShapeEditingChange({%H-}ASender: TObject); virtual;
    procedure ShapeRemoveQuery({%H-}ASender: TObject; var AHandled: boolean);
    function GetStatusText: string; override;
    function SlowShape: boolean; virtual;
    procedure QuickDefineEnd; virtual;
    procedure OnTryStop({%H-}sender: TCustomLayerAction); override;
    procedure UpdateUseOriginal;
    function ReplaceLayerAndAddShape(out ARect: TRect): TCustomImageDifference; virtual;
    procedure ShapeValidated; virtual;
    function ForeGradTexMode: TVectorShapeUsermode; virtual;
    function BackGradTexMode: TVectorShapeUsermode; virtual;
    function OutlineGradTexMode: TVectorShapeUsermode; virtual;
    function ForeFitMode: TFitMode;
    function BackFitMode: TFitMode;
    function OutlineFitMode: TFitMode;
    function ManagerForeFill: TVectorialFill;
    function ManagerBackFill: TVectorialFill;
    function ManagerOutlineFill: TVectorialFill;
    function GetIsForeEditGradTexPoints: boolean; override;
    function GetIsBackEditGradTexPoints: boolean; override;
    function GetIsOutlineEditGradTexPoints: boolean; override;
    function GetGridMatrix: TAffineMatrix; virtual;
    property Editor: TBGRAOriginalEditor read GetEditor;
  public
    function ValidateShape: TRect;
    function CancelShape: TRect;
    constructor Create(AManager: TToolManager); override;
    function ToolUp: TRect; override;
    function ToolKeyPress(var key: TUTF8Char): TRect; override;
    function ToolCommand(ACommand: TToolCommand): boolean; override;
    function ToolProvideCommand(ACommand: TToolCommand): boolean; override;
    function SuggestGradientBox: TAffineBox; override;
    function GetContextualToolbars: TContextualToolbars; override;
    function Render(VirtualScreen: TBGRABitmap; {%H-}VirtualScreenWidth, {%H-}VirtualScreenHeight: integer; BitmapToVirtualScreen: TBitmapToVirtualScreenFunction):TRect; override;
    property IsIdle: boolean read GetIsIdle;
    property IsHandDrawing: boolean read GetIsHandDrawing;
    destructor Destroy; override;
  end;

  { TEditShapeTool }

  TEditShapeMode = (esmNone, esmSelection, esmGradient, esmOtherOriginal, esmShape, esmNoShape);
  TEditShapeTool = class(TGenericTool)
  private
    function GetNothingSelected: boolean;
  protected
    FDownHandled,FRectEditorCapture,FLayerOriginalCapture,
    FLeftButton,FRightButton: boolean;
    FLastPos: TPointF;
    FOriginalLayerId: integer;
    FOriginalRect: TRectShape;
    FOriginalRectUntransformed: TRectF;
    FRectEditor: TBGRAOriginalEditor;
    FSelectionRect: TRectShape;
    FSelectionRectUntransformed: TRectF;
    FIsEditingGradient: boolean;
    procedure RetrieveLightPosition;
    procedure UpdateToolManagerFromShape(AShape: TVectorShape);
    procedure UpdateDraftMode;
    procedure BindOriginalEvent(ABind: boolean);
    procedure SelectShape({%H-}ASender: TObject; AShape: TVectorShape; {%H-}APreviousShape: TVectorShape);
    function DoToolDown({%H-}toolDest: TBGRABitmap; {%H-}pt: TPoint; {%H-}ptF: TPointF; rightBtn: boolean): TRect; override;
    function DoToolMove({%H-}toolDest: TBGRABitmap; {%H-}pt: TPoint; {%H-}ptF: TPointF): TRect; override;
    function DoToolUpdate({%H-}toolDest: TBGRABitmap): TRect; override;
    function DoToolKeyDown(var key: Word): TRect; override;
    function DoToolKeyUp(var key: Word): TRect; override;
    procedure UpdateSnap(AEditor: TBGRAOriginalEditor);
    function GetAction: TLayerAction; override;
    function DoGetToolDrawingLayer: TBGRABitmap; override;
    procedure OnTryStop({%H-}sender: TCustomLayerAction); override;
    procedure StopEdit(AUpdateInterface, ARaiseToolUp: boolean);
    function IsEditing: boolean;
    procedure MakeImageOriginal;
    procedure MakeVectorOriginal;
    procedure UpdateMatrixFromRect;
    procedure DoEditSelection;
    function GetMatrixFromRect(ARect: TRectShape; AUntransformedRect: TRectF): TAffineMatrix;
    function CreateRect(AUntransformedRect: TRectF; AMatrix: TAffineMatrix): TRectShape;
    function GetIsSelectingTool: boolean; override;
    function GetVectorOriginal: TVectorOriginal;
    function GetGradientOriginal: TBGRALayerGradientOriginal;
    function GetImageOriginal: TBGRALayerImageOriginal;
    function GetOriginalTransform: TAffineMatrix;
    function FixLayerOffset: boolean; override;
    function GetCurrentSplineMode: TToolSplineMode;
    procedure SetCurrentSplineMode(AMode: TToolSplineMode);
    function ConvertToSpline: boolean;
    function GetEditMode: TEditShapeMode;
    function InvalidEditMode: boolean;
    function ForeGradTexMode: TVectorShapeUsermode; virtual;
    function BackGradTexMode: TVectorShapeUsermode; virtual;
    function OutlineGradTexMode: TVectorShapeUsermode; virtual;
    function ForeFitMode: TFitMode;
    function BackFitMode: TFitMode;
    function OutlineFitMode: TFitMode;
    function GetIsForeEditGradTexPoints: boolean; override;
    function GetIsBackEditGradTexPoints: boolean; override;
    function GetIsOutlineEditGradTexPoints: boolean; override;
    function GetAllowedBackFillTypes: TVectorialFillTypes; override;
    function GetStatusText: string; override;
  public
    constructor Create(AManager: TToolManager); override;
    destructor Destroy; override;
    function GetContextualToolbars: TContextualToolbars; override;
    function Render(VirtualScreen: TBGRABitmap; VirtualScreenWidth, VirtualScreenHeight: integer; BitmapToVirtualScreen: TBitmapToVirtualScreenFunction): TRect; override;
    function ToolKeyPress(var key: TUTF8Char): TRect; override;
    function ToolUp: TRect; override;
    function ToolCommand(ACommand: TToolCommand): boolean; override;
    function ToolProvideCommand(ACommand: TToolCommand): boolean; override;
    function SuggestGradientBox: TAffineBox; override;
    property CurrentSplineMode: TToolSplineMode read GetCurrentSplineMode write SetCurrentSplineMode;
    property NothingSelected: boolean read GetNothingSelected;
  end;

procedure AssignFill(ATarget, ASource: TVectorialFill; const ABox: TAffineBox; AFitMode: TFitMode);
function GetShapeStatusText(AShape: TVectorShape; const AMatrix: TAffineMatrix): string;
function GetGradientStatusText(AGradient: TBGRALayerGradientOriginal; const AMatrix: TAffineMatrix): string;

implementation

uses LazPaintType, LCVectorPolyShapes, LCVectorTextShapes, BGRASVGOriginal,
  ULoading, BGRATransform, math, UImageDiff, Controls, BGRAPen, UResourceStrings, ugraph,
  LCScaleDPI, LCVectorClipboard, BGRAGradientScanner, UClipboard, BGRAUTF8;

const PointSize = 6;

function OriginalCursorToCursor(ACursor: TOriginalEditorCursor): TCursor;
begin
  case ACursor of
    oecMove: result := crSizeAll;
    oecMoveN: result := crSizeN;
    oecMoveS: result := crSizeS;
    oecMoveE: result := crSizeE;
    oecMoveW: result := crSizeW;
    oecMoveNE: result := crSizeNE;
    oecMoveSW: result := crSizeSW;
    oecMoveNW: result := crSizeNW;
    oecMoveSE: result := crSizeSE;
    oecHandPoint: result := crHandPoint;
    oecText: result := crIBeam;
    else result := crDefault;
  end;
end;

function ToolSplineModeFromShape(AShape: TVectorShape): TToolSplineMode;
var
  c: TCurveShape;
begin
  result := tsmMovePoint;
  if not (AShape is TCurveShape) then exit;
  c := TCurveShape(AShape);
  case c.Usermode of
  vsuEdit: result := tsmMovePoint;
  vsuCreate: if c.PointCount > 1 then
             begin
               case c.CurveMode[c.PointCount-2] of
                 cmAuto: result := tsmCurveModeAuto;
                 cmAngle: result := tsmCurveModeAngle;
                 cmCurve: result := tsmCurveModeSpline;
               end;
             end else
               result := tsmCurveModeAuto;
  vsuCurveSetAuto: result := tsmCurveModeAuto;
  vsuCurveSetAngle: result := tsmCurveModeAngle;
  vsuCurveSetCurve: result := tsmCurveModeSpline;
  end;
end;

function ContextualToolbarsFromShape(AShapeClass: TVectorShapeAny; AShape: TVectorShape): TContextualToolbars;
var
  f: TVectorShapeFields;
begin
  result:= [ctPenFill, ctBackFill];
  if Assigned(AShape) then
    f := AShape.MultiFields
    else f := AShapeClass.Fields;
  if vsfPenWidth in f then result += [ctPenWidth];
  if vsfPenStyle in f then result += [ctPenStyle];
  if vsfJoinStyle in f then result += [ctJoinStyle];
  if [vsfPenStyle,vsfPenFill,vsfBackFill] <= f then result += [ctShape];
  if vsfOutlineFill in f then
  begin
    result += [ctOutlineFill];
    if not (vsfBackFill in f) then result -= [ctBackFill];
  end;
  if vsfOutlineWidth in f then result += [ctOutlineWidth];

  if AShapeClass = TCurveShape then result := result + [ctShape,ctCloseShape,ctLineCap,ctSplineStyle]
  else if AShapeClass = TPolylineShape then result := result + [ctShape,ctCloseShape,ctLineCap]
  else if AShapeClass = TPhongShape then result := result + [ctPhong,ctAltitude]
  else if AShapeClass = TTextShape then
  begin
    result := result + [ctText,ctAliasing];
    if TTextShape(AShape).PenPhong then include(result, ctAltitude);
  end;
end;

procedure AlignShape(AShape: TVectorShape; ACommand: TToolCommand; const AMatrix: TAffineMatrix; const ARect: TRect);
begin
  case ACommand of
  tcAlignLeft: AShape.AlignHorizontally(taLeftJustify,AMatrix,ARect);
  tcCenterHorizontally: AShape.AlignHorizontally(taCenter,AMatrix,ARect);
  tcAlignRight: AShape.AlignHorizontally(taRightJustify,AMatrix,ARect);
  tcAlignTop..tcAlignBottom:
      AShape.AlignVertically(TTextLayout(ord(ACommand)-ord(tcAlignTop)+ord(tlTop)),AMatrix,ARect);
  end;
end;

procedure AssignFill(ATarget, ASource: TVectorialFill; const ABox: TAffineBox; AFitMode: TFitMode);
var
  temp: TVectorialFill;
  change: Boolean;
begin
  if ASource.IsFullyTransparent then ATarget.Clear else
  begin
    change := ((ATarget.FillType = vftGradient) and (ASource.FillType = vftGradient) and
       (ATarget.Gradient.GradientType <> ASource.Gradient.GradientType) and
       not ([ATarget.Gradient.GradientType,ASource.Gradient.GradientType] <= [gtRadial,gtDiamond,gtAngular])) or
      ((ATarget.FillType = vftTexture) and (ASource.FillType = vftTexture) and
       (ATarget.TextureRepetition <> ASource.TextureRepetition));
    if ((AFitMode = fmIfChange) and change) or (AFitMode = fmAlways)
        or (ATarget.FillType <> ASource.FillType) then
    begin
      temp := ATarget.Duplicate;
      temp.AssignExceptGeometry(ASource);
      temp.FitGeometry(ABox);
      ATarget.Assign(temp);
      temp.Free;
    end else
      ATarget.AssignExceptGeometry(ASource);
  end;
end;

function GetShapeStatusText(AShape: TVectorShape; const AMatrix: TAffineMatrix): string;
var
  orig, xa, ya, corner1, corner2: TPointF;
  overline: string4;
  i, nb: Integer;
  rF: TRectF;
begin
  if AShape is TEllipseShape then
    with TEllipseShape(AShape) do
    begin
      orig := AMatrix*Origin;
      xa := AMatrix*XAxis;
      ya := AMatrix*YAxis;
      result := 'x = '+FloatToStrF(orig.x,ffFixed,6,1)+'|y = '+FloatToStrF(orig.y,ffFixed,6,1)+'|'+
      'rx = '+FloatToStrF(VectLen(xa-orig),ffFixed,6,1)+'|ry = '+FloatToStrF(VectLen(ya-orig),ffFixed,6,1)
    end
  else if AShape is TCustomRectShape then
    with TCustomRectShape(AShape) do
    begin
      orig := AMatrix*Origin;
      xa := AMatrix*XAxis;
      ya := AMatrix*YAxis;
      corner1 := orig-(xa-orig)-(ya-orig);
      corner2 := xa + (ya-orig);
      result := 'x1 = '+FloatToStrF(corner1.x,ffFixed,6,1)+'|y1 = '+FloatToStrF(corner1.y,ffFixed,6,1)+'|'+
      'x2 = '+FloatToStrF(corner2.x,ffFixed,6,1)+'|y2 = '+FloatToStrF(corner2.y,ffFixed,6,1)+'|'+
      'Δx = '+FloatToStrF(VectLen(xa-orig)*2,ffFixed,6,1)+'|Δy = '+FloatToStrF(VectLen(ya-orig)*2,ffFixed,6,1);
    end
  else if AShape is TCustomPolypointShape then
    with TCustomPolypointShape(AShape) do
    begin
      result := 'count = ';
      nb := 0;
      for i := 0 to PointCount-1 do
        if Points[i].IsEmpty then
        begin
          if nb > 0 then result += inttostr(nb)+', ';
          nb := 0;
        end else inc(nb);
      result += inttostr(nb);
      if not Center.IsEmpty then
      begin
        orig := AMatrix*Center;
        overline := UnicodeCharToUTF8($0305);
        result += '|x'+overline+' = '+FloatToStrF(orig.x,ffFixed,6,1);
        result += '|y'+overline+' = '+FloatToStrF(orig.y,ffFixed,6,1);
      end;
      if (Usermode = vsuCreate) and (PointCount > 0) then
      begin
        xa := AMatrix*Points[PointCount-1];
        result += '|x = '+FloatToStrF(xa.x,ffFixed,6,1);
        result += '|y = '+FloatToStrF(xa.y,ffFixed,6,1);
      end else
      if HoverPoint <> -1 then
      begin
        xa := AMatrix*Points[HoverPoint];
        result += '|x = '+FloatToStrF(xa.x,ffFixed,6,1);
        result += '|y = '+FloatToStrF(xa.y,ffFixed,6,1);
      end else
      begin
        rF := GetPointBounds(AMatrix);
        result += '|Δx = '+FloatToStrF(rF.Width,ffFixed,6,1);
        result += '|Δy = '+FloatToStrF(rF.Height,ffFixed,6,1);
      end;
    end else
      result := '';
end;

function GetGradientStatusText(AGradient: TBGRALayerGradientOriginal; const AMatrix: TAffineMatrix): string;
var
  orig, xa: TPointF;
begin
  with AGradient do
  begin
    orig := AMatrix*Origin;
    xa := AMatrix*XAxis;
    result := 'x1 = '+FloatToStrF(orig.x,ffFixed,6,1)+'|y1 = '+FloatToStrF(orig.y,ffFixed,6,1)+'|'+
      'x2 = '+FloatToStrF(xa.x,ffFixed,6,1)+'|y2 = '+FloatToStrF(xa.y,ffFixed,6,1)+'|'+
      'Δx = '+FloatToStrF(abs(xa.x-orig.x),ffFixed,6,1)+'|Δy = '+FloatToStrF(abs(xa.y-orig.y),ffFixed,6,1);
  end;
end;

{ TEditShapeTool }

procedure TEditShapeTool.SelectShape(ASender: TObject; AShape: TVectorShape;
  APreviousShape: TVectorShape);
begin
  if Assigned(AShape) and (GetCurrentLayerKind = lkVectorial) then
  begin
    UpdateToolManagerFromShape(AShape);
    Manager.UpdateContextualToolbars;
  end else
  if Assigned(APreviousShape) then
    Manager.UpdateContextualToolbars;
end;

function TEditShapeTool.GetNothingSelected: boolean;
begin
  result := GetEditMode in [esmNone, esmNoShape];
end;

procedure TEditShapeTool.RetrieveLightPosition;
var
  shape: TVectorShape;
  m: TAffineMatrix;
begin
  if GetCurrentLayerKind = lkVectorial then
  begin
    shape := GetVectorOriginal.SelectedShape;
    if shape=nil then exit;
    m := Manager.Image.LayerOriginalMatrix[Manager.Image.CurrentLayerIndex];
    if (shape is TTextShape) and TTextShape(shape).PenPhong then
      Manager.LightPosition := m*TTextShape(shape).LightPosition
    else if shape is TPhongShape then
      Manager.LightPosition := m*TPhongShape(shape).LightPosition;
  end;
end;

procedure TEditShapeTool.UpdateToolManagerFromShape(AShape: TVectorShape);
var
  opt: TShapeOptions;
  zoom: single;
  m: TAffineMatrix;
  doFill, doDraw: Boolean;
  f: TVectorShapeFields;
begin
  m := Manager.Image.LayerOriginalMatrix[Manager.Image.CurrentLayerIndex];
  zoom := (VectLen(m[1,1],m[2,1])+VectLen(m[1,2],m[2,2]))/2;
  if AShape.Usermode in [vsuEditBackFill, vsuEditPenFill] then
    AShape.Usermode := vsuEdit;
  opt := Manager.ShapeOptions;
  f := AShape.MultiFields;
  doDraw := vsfPenFill in f;
  doFill := vsfBackFill in f;
  if vsfPenStyle in f then
  begin
    doDraw := AShape.PenVisible;
    if doDraw then doFill := AShape.BackVisible;

    if not doFill then
      exclude(opt,toFillShape)
      else include(opt,toFillShape);
    if not doDraw then
      exclude(opt,toDrawShape)
      else
      begin
        include(opt,toDrawShape);
        Manager.PenStyle := BGRAToPenStyle(AShape.PenStyle);
      end;
  end;
  if doDraw then
  begin
    if not AShape.PenVisible then
      Manager.ForeColor := BGRA(Manager.ForeColor.red,
        Manager.ForeColor.green,Manager.ForeColor.blue,0)
    else
      Manager.ForeFill.Assign(AShape.PenFill);
  end;
  if doFill then
  begin
    if not AShape.BackVisible then
      Manager.BackColor := BGRA(Manager.BackColor.red,
        Manager.BackColor.green,Manager.BackColor.blue,0)
    else
      Manager.BackFill.Assign(AShape.BackFill);
  end;
  if not AShape.OutlineVisible then
    Manager.SetTextOutline(false, Manager.TextOutlineWidth) else
  begin
    Manager.SetTextOutline(true, AShape.OutlineWidth*zoom);
    Manager.OutlineFill.Assign(AShape.OutlineFill);
  end;

  if toDrawShape in opt then
  begin
    if vsfPenWidth in f then Manager.PenWidth := AShape.PenWidth*zoom;
    if vsfJoinStyle in f then Manager.JoinStyle:= AShape.JoinStyle;
    if AShape is TCustomPolypointShape then
    begin
      if TCustomPolypointShape(AShape).Closed then
        include(opt, toCloseShape)
      else
        exclude(opt, toCloseShape);
      Manager.LineCap := TCustomPolypointShape(AShape).LineCap;
      Manager.ArrowSize := TCustomPolypointShape(AShape).ArrowSize;
      Manager.ArrowStart := TCustomPolypointShape(AShape).ArrowStartKind;
      Manager.ArrowEnd := TCustomPolypointShape(AShape).ArrowEndKind;
    end;
    if AShape is TCurveShape then
      Manager.SplineStyle := TCurveShape(AShape).SplineStyle;
  end;

  if AShape is TTextShape then
  with TTextShape(AShape) do
  begin
    Manager.TextPhong := PenPhong;
    Manager.LightPosition := m*LightPosition;
    Manager.PhongShapeAltitude := round(AltitudePercent);
    Manager.TextAlign:= ParagraphAlignment;
    Manager.SetTextFont(FontName, FontEmHeight*zoom*72/Manager.Image.DPI, FontStyle);
    Manager.TextShadow:= false;
    if Aliased then
      include(opt,toAliasing)
      else exclude(opt,toAliasing);
  end;
  Manager.ShapeOptions := opt;

  if AShape is TPhongShape then
  with TPhongShape(AShape) do
  begin
    Manager.PhongShapeKind:= ShapeKind;
    Manager.LightPosition:= LightPosition;
    Manager.PhongShapeAltitude:= round(ShapeAltitudePercent);
    Manager.PhongShapeBorderSize:= round(BorderSizePercent);
  end;
end;

procedure TEditShapeTool.UpdateDraftMode;
begin
  case GetEditMode of
  esmShape: Manager.Image.DraftOriginal:= GetVectorOriginal.PreferDraftMode(Manager.Image.CurrentState.LayeredBitmap.OriginalEditor, GetOriginalTransform);
  esmNoShape: Manager.Image.DraftOriginal:= false;
  else Manager.Image.DraftOriginal:= FLeftButton or FRightButton;
  end;
end;

procedure TEditShapeTool.BindOriginalEvent(ABind: boolean);
begin
  case GetCurrentLayerKind of
  lkVectorial:
    begin
      if ABind then
      begin
        GetVectorOriginal.OnSelectShape:= @SelectShape;
        Manager.Image.CurrentState.DiscardOriginalDiff := false;
      end else
      begin
        GetVectorOriginal.OnSelectShape := nil;
        Manager.Image.CurrentState.DiscardOriginalDiff := true;
      end;
    end;
  lkGradient:
    begin
      if ABind then
      begin
        Manager.Image.CurrentState.DiscardOriginalDiff := false;
      end else
      begin
        Manager.Image.CurrentState.DiscardOriginalDiff := true;
      end;
    end;
  end;
end;

function TEditShapeTool.DoToolDown(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF; rightBtn: boolean): TRect;
var
  cur: TOriginalEditorCursor;
  handled: boolean;
  ptView: TPointF;
begin
  Result:= EmptyRect;
  FRightButton:= rightBtn;
  FLeftButton:= not rightBtn;
  FRectEditorCapture:= false;
  FLayerOriginalCapture:= false;
  FLastPos := ptF;
  handled := false;
  if not handled and (GetEditMode in [esmSelection,esmOtherOriginal]) and Assigned(FRectEditor) then
  begin
    ptView := FRectEditor.Matrix*ptF;
    UpdateSnap(FRectEditor);
    FRectEditor.MouseDown(rightBtn, ShiftState, ptView.X,ptView.Y, cur, handled);
    Cursor := OriginalCursorToCursor(cur);
    if handled then
    begin
      FRectEditorCapture:= true;
      result := OnlyRenderChange;
      UpdateMatrixFromRect;
    end;
  end;
  if not handled and (GetEditMode in [esmShape,esmGradient,esmNoShape]) then
  begin
    BindOriginalEvent(true);
    try
      UpdateSnap(Manager.Image.CurrentState.LayeredBitmap.OriginalEditor);
      Manager.Image.CurrentState.LayeredBitmap.MouseDown(rightBtn, ShiftState, ptF.X,ptF.Y, cur, handled);
    finally
      BindOriginalEvent(false);
    end;
    if handled then
    begin
      Cursor := OriginalCursorToCursor(cur);
      FLayerOriginalCapture:= true;
    end;
  end;
  FDownHandled:= handled;
end;

function TEditShapeTool.DoToolMove(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF): TRect;
var
  cur: TOriginalEditorCursor;
  handled: boolean;
  ptView: TPointF;
begin
  FLastPos := ptF;
  Result:= EmptyRect;
  UpdateDraftMode;

  case GetEditMode of
  esmGradient, esmShape, esmNoShape:
    begin
      BindOriginalEvent(true);
      try
        UpdateSnap(Manager.Image.CurrentState.LayeredBitmap.OriginalEditor);
        Manager.Image.CurrentState.LayeredBitmap.MouseMove(ShiftState, ptF.X,ptF.Y, cur, handled);
      finally
        BindOriginalEvent(false);
      end;
      Cursor := OriginalCursorToCursor(cur);
    end;
  esmSelection, esmOtherOriginal:
    if Assigned(FRectEditor) then
    begin
      ptView := FRectEditor.Matrix*ptF;
      UpdateSnap(FRectEditor);
      FRectEditor.MouseMove(ShiftState, ptView.X,ptView.Y, cur, handled);
      Cursor := OriginalCursorToCursor(cur);
      if handled then
      begin
        result := OnlyRenderChange;
        UpdateMatrixFromRect;
      end;
    end;
  end;
end;

function TEditShapeTool.DoToolUpdate(toolDest: TBGRABitmap): TRect;
var
  doDraw, doFill: Boolean;
  m: TAffineMatrix;
  zoom: Single;
  gradBox: TAffineBox;
  f: TVectorShapeFields;
  shape: TVectorShape;
begin
  shape := nil;
  case GetEditMode of
  esmShape:
    try
      BindOriginalEvent(true);
      shape := GetVectorOriginal.SelectedShape;
      shape.BeginUpdate;
      gradBox := shape.SuggestGradientBox(AffineMatrixIdentity);
      m := AffineMatrixInverse(Manager.Image.LayerOriginalMatrix[Manager.Image.CurrentLayerIndex]);
      zoom := (VectLen(m[1,1],m[2,1])+VectLen(m[1,2],m[2,2]))/2;
      f := shape.MultiFields;
      if f*[vsfPenFill,vsfBackFill,vsfPenStyle] = [vsfPenFill,vsfBackFill,vsfPenStyle] then
      begin
        doDraw := toDrawShape in Manager.ShapeOptions;
        doFill := toFillShape in Manager.ShapeOptions;

        if doDraw then
          shape.PenStyle := PenStyleToBGRA(Manager.PenStyle)
        else
          shape.PenStyle := ClearPenStyle;

        if doDraw and (vsfPenWidth in f) then shape.PenWidth := Manager.PenWidth*zoom;
        if doDraw and (vsfJoinStyle in f) then shape.JoinStyle := Manager.JoinStyle;
        if shape is TCustomPolypointShape then
        begin
          TCustomPolypointShape(shape).Closed := toCloseShape in Manager.ShapeOptions;
          if not TCustomPolypointShape(shape).Closed then
          begin
            TCustomPolypointShape(shape).LineCap:= Manager.LineCap;
            TCustomPolypointShape(shape).ArrowSize:= Manager.ArrowSize;
            TCustomPolypointShape(shape).ArrowStartKind:= Manager.ArrowStart;
            TCustomPolypointShape(shape).ArrowEndKind:= Manager.ArrowEnd;
          end;
        end;
        if shape is TCurveShape then
          TCurveShape(shape).SplineStyle:= Manager.SplineStyle;
      end else
      begin
        doDraw := vsfPenFill in f;
        doFill := vsfBackFill in f;
      end;
      if doFill then AssignFill(shape.BackFill, Manager.BackFill, gradBox, BackFitMode)
      else if vsfBackFill in f then
          shape.BackFill.Clear;
      if doDraw then AssignFill(shape.PenFill, Manager.ForeFill, gradBox, ForeFitMode);
      if (vsfOutlineWidth in f) and Manager.TextOutline then shape.OutlineWidth := Manager.TextOutlineWidth*zoom;
      if vsfOutlineFill in f then
      begin
        if Manager.TextOutline then
          AssignFill(shape.OutLineFill, Manager.OutLineFill, gradBox, OutlineFitMode)
          else shape.OutlineFill.Clear;
      end;

      if shape is TTextShape then
      with TTextShape(shape) do
      begin
        PenPhong := Manager.TextPhong;
        LightPosition := m*Manager.LightPosition;
        AltitudePercent := Manager.PhongShapeAltitude;
        ParagraphAlignment := Manager.TextAlign;
        FontName:= Manager.TextFontName;
        FontEmHeight:= Manager.TextFontSize*zoom*Manager.Image.DPI/72;
        FontStyle := Manager.TextFontStyle;
        Aliased := Manager.ShapeOptionAliasing;
      end;
      if shape is TPhongShape then
      with TPhongShape(shape) do
      begin
        ShapeKind := Manager.PhongShapeKind;
        LightPosition := Manager.LightPosition;
        ShapeAltitudePercent := Manager.PhongShapeAltitude;
        BorderSizePercent := Manager.PhongShapeBorderSize;
      end;
    finally
      if Assigned(shape) then shape.EndUpdate;
      BindOriginalEvent(false);
    end;
  esmGradient:
    try
      BindOriginalEvent(true);
      case Manager.BackFill.FillType of
      vftGradient: GetGradientOriginal.AssignExceptGeometry(Manager.BackFill.Gradient);
      vftSolid: GetGradientOriginal.SetColors(Manager.BackFill.SolidColor, Manager.BackFill.SolidColor);
      end;
    finally
      BindOriginalEvent(false);
    end;
  end;
  Result := EmptyRect;
end;

procedure TEditShapeTool.UpdateSnap(AEditor: TBGRAOriginalEditor);
begin
  if Assigned(AEditor) then
    AEditor.GridActive := ssSnap in ShiftState;
end;

function TEditShapeTool.GetAction: TLayerAction;
begin
  result := nil;
end;

function TEditShapeTool.DoGetToolDrawingLayer: TBGRABitmap;
begin
  Result:= Manager.Image.LayerBitmap[Manager.Image.CurrentLayerIndex];
end;

procedure TEditShapeTool.OnTryStop(sender: TCustomLayerAction);
begin
  StopEdit(True, True);
  inherited OnTryStop(sender);
end;

procedure TEditShapeTool.StopEdit(AUpdateInterface, ARaiseToolUp: boolean);
var
  r: TRect;
begin
  if ARaiseToolUp and (FLeftButton or FRightButton) then
  begin
    r := ToolUp;
    Manager.Image.LayerMayChange(GetToolDrawingLayer,r);
  end;
  case GetEditMode of
  esmShape: GetVectorOriginal.DeselectShapes;
  esmGradient: FIsEditingGradient:= false;
  esmOtherOriginal: FreeAndNil(FOriginalRect);
  esmSelection: FreeAndNil(FSelectionRect);
  end;
  Manager.Image.CurrentState.LayeredBitmap.ClearEditor;
  FLayerOriginalCapture:= false;
  FreeAndNil(FRectEditor);
  FRectEditorCapture := false;
  Cursor := crDefault;
  if AUpdateInterface then
  begin
    Manager.Image.OnImageChanged.NotifyObservers;
    Manager.UpdateContextualToolbars;
  end;
end;

function TEditShapeTool.IsEditing: boolean;
begin
  result := not (GetEditMode in[esmNone, esmNoShape]);
end;

procedure TEditShapeTool.MakeImageOriginal;
var
  diff: TReplaceLayerByImageOriginalDifference;
begin
  if GetCurrentLayerKind = lkBitmap then
  begin
    diff := TReplaceLayerByImageOriginalDifference.Create(Manager.Image.CurrentState,
              Manager.Image.CurrentLayerIndex, false);
    Manager.Image.AddUndo(diff);
    if Assigned(Manager.Image.OnStackChanged) then
      Manager.Image.OnStackChanged(Manager.Image, False);
  end;
end;

procedure TEditShapeTool.MakeVectorOriginal;
var
  diff: TReplaceLayerByVectorOriginalDifference;
begin
  if GetCurrentLayerKind in [lkEmpty,lkBitmap,lkTransformedBitmap] then
  begin
    StopEdit(True, True);
    diff := TReplaceLayerByVectorOriginalDifference.Create(Manager.Image.CurrentState,
              Manager.Image.CurrentLayerIndex, false);
    Manager.Image.AddUndo(diff);
    if Assigned(Manager.Image.OnStackChanged) then
      Manager.Image.OnStackChanged(Manager.Image, False);
  end;
end;

procedure TEditShapeTool.UpdateMatrixFromRect;
begin
  if Assigned(FOriginalRect) then
    Manager.Image.LayerOriginalMatrix[Manager.Image.CurrentLayerIndex] :=
      GetMatrixFromRect(FOriginalRect,FOriginalRectUntransformed);
  if Assigned(FSelectionRect) then
  begin
    Manager.Image.SelectionTransform :=
      GetMatrixFromRect(FSelectionRect,FSelectionRectUntransformed);
    Manager.Image.OnImageChanged.NotifyObservers;
  end;
end;

procedure TEditShapeTool.DoEditSelection;
begin
  if not IsEditing then
  begin
    with Manager.Image.SelectionMaskBounds do
      FSelectionRectUntransformed := rectF(Left-0.5, Top-0.5, Right-0.5, Bottom-0.5);
    FSelectionRect := CreateRect(FSelectionRectUntransformed, Manager.Image.SelectionTransform);
  end;
end;

function TEditShapeTool.GetMatrixFromRect(ARect: TRectShape; AUntransformedRect: TRectF): TAffineMatrix;
var
  u, v: TPointF;
  mAfter, mBefore: TAffineMatrix;
begin
  u := (ARect.XAxis-ARect.Origin)*2;
  v := (ARect.YAxis-ARect.Origin)*2;
  mAfter := AffineMatrix(u,v,ARect.Origin-(u+v)*0.5);
  mBefore := AffineMatrixTranslation(AUntransformedRect.Left,
               AUntransformedRect.Top)
    *AffineMatrixScale(AUntransformedRect.Width,AUntransformedRect.Height);
  if IsAffineMatrixInversible(mBefore) then
    result := mAfter*AffineMatrixInverse(mBefore)
  else
    result := AffineMatrixIdentity;
end;

function TEditShapeTool.CreateRect(AUntransformedRect: TRectF; AMatrix: TAffineMatrix): TRectShape;
var
  box: TAffineBox;
begin
  if (AUntransformedRect.Width > 0) and (AUntransformedRect.Height > 0) then
  begin
    result := TRectShape.Create(nil);
    result.PenStyle := ClearPenStyle;
    result.BackFill.SetSolid(BGRAWhite);
    box := AMatrix*TAffineBox.AffineBox(AUntransformedRect);
    result.Origin := (box.TopLeft+box.BottomRight)*0.5;
    result.XAxis := result.Origin+(box.TopRight-box.TopLeft)*0.5;
    result.YAxis := result.Origin+(box.BottomLeft-box.TopLeft)*0.5;
    Manager.UpdateContextualToolbars;
  end else
    result := nil;
end;

function TEditShapeTool.GetIsSelectingTool: boolean;
begin
  result := false;
end;

function TEditShapeTool.GetVectorOriginal: TVectorOriginal;
begin
  result := Manager.Image.LayerOriginal[Manager.Image.CurrentLayerIndex] as TVectorOriginal;
end;

function TEditShapeTool.GetGradientOriginal: TBGRALayerGradientOriginal;
begin
  result := Manager.Image.LayerOriginal[Manager.Image.CurrentLayerIndex] as TBGRALayerGradientOriginal;
end;

function TEditShapeTool.GetImageOriginal: TBGRALayerImageOriginal;
begin
  result := Manager.Image.LayerOriginal[Manager.Image.CurrentLayerIndex] as TBGRALayerImageOriginal;
end;

function TEditShapeTool.GetOriginalTransform: TAffineMatrix;
begin
  result := Manager.Image.LayerOriginalMatrix[Manager.Image.CurrentLayerIndex];
end;

function TEditShapeTool.FixLayerOffset: boolean;
begin
  Result:= false;
end;

destructor TEditShapeTool.Destroy;
begin
  FreeAndNil(FOriginalRect);
  FreeAndNil(FSelectionRect);
  Manager.Image.CurrentState.LayeredBitmap.ClearEditor;
  FreeAndNil(FRectEditor);
  inherited Destroy;
end;

function TEditShapeTool.GetContextualToolbars: TContextualToolbars;
var
  shape: TVectorShape;
begin
  case GetEditMode of
  esmShape:
    begin
      shape := GetVectorOriginal.SelectedShape;
      result := ContextualToolbarsFromShape(TVectorShapeAny(shape.ClassType), shape);
    end;
  esmGradient: result := [ctBackFill];
  else
    Result:= [ctPenFill, ctBackFill];
  end;
end;

function TEditShapeTool.Render(VirtualScreen: TBGRABitmap; VirtualScreenWidth,
  VirtualScreenHeight: integer;
  BitmapToVirtualScreen: TBitmapToVirtualScreenFunction): TRect;
var
  orig, xAxis, yAxis: TPointF;
  viewMatrix, editMatrix: TAffineMatrix;
begin
  if InvalidEditMode then StopEdit(false,false);
  with LayerOffset do
  begin
    orig := BitmapToVirtualScreen(PointF(-X,-Y));
    xAxis := BitmapToVirtualScreen(PointF(-X+1,-Y));
    yAxis := BitmapToVirtualScreen(PointF(-X,-Y+1));
  end;
  viewMatrix := AffineMatrixTranslation(0.5,0.5)
                *AffineMatrix(xAxis-orig,yAxis-orig,orig)
                *AffineMatrixTranslation(-0.5,-0.5);

  case GetEditMode of
  esmGradient, esmShape:
    begin
      FreeAndNil(FRectEditor);
      FRectEditorCapture := false;
      if Assigned(Manager.Image.CurrentState.LayeredBitmap.OriginalEditor) then
        Manager.Image.CurrentState.LayeredBitmap.OriginalEditor.GridMatrix := AffineMatrixScale(0.5,0.5);
      if Assigned(VirtualScreen) then
        result := Manager.Image.CurrentState.LayeredBitmap.DrawEditor(VirtualScreen,
          Manager.Image.CurrentLayerIndex, viewMatrix, DoScaleX(PointSize*Manager.CanvasScale,OriginalDPI))
      else
        result := Manager.Image.CurrentState.LayeredBitmap.GetEditorBounds(
          rect(0,0,VirtualScreenWidth,VirtualScreenHeight),
          Manager.Image.CurrentLayerIndex, viewMatrix, DoScaleX(PointSize*Manager.CanvasScale,OriginalDPI));
      RetrieveLightPosition;
    end;
  esmSelection, esmOtherOriginal:
    begin
      result := EmptyRect;
      Manager.Image.CurrentState.LayeredBitmap.ClearEditor;
      FLayerOriginalCapture:= false;
      if not Assigned(FRectEditor) then
      begin
        FRectEditor := TVectorOriginalEditor.Create(nil);
        FRectEditor.GridMatrix := AffineMatrixScale(0.5,0.5);
        FRectEditor.Focused := true;
        FRectEditor.PointSize := DoScaleX(PointSize*Manager.CanvasScale,OriginalDPI);
      end;
      FRectEditor.Clear;
      editMatrix := AffineMatrixTranslation(-0.5,-0.5)*viewMatrix*AffineMatrixTranslation(0.5,0.5);
      if IsAffineMatrixInversible(editMatrix) then
      begin
        FRectEditor.Matrix := editMatrix;
        if Assigned(FOriginalRect) then FOriginalRect.ConfigureEditor(FRectEditor);
        if Assigned(FSelectionRect) then FSelectionRect.ConfigureEditor(FRectEditor);
        if Assigned(VirtualScreen) then
          result := FRectEditor.Render(VirtualScreen,
            rect(0,0,VirtualScreenWidth,VirtualScreenHeight))
        else
          result := FRectEditor.GetRenderBounds(
            rect(0,0,VirtualScreenWidth,VirtualScreenHeight));
      end
      else
        result := EmptyRect;
    end;
  else
    begin
      result := EmptyRect;
      Manager.Image.CurrentState.LayeredBitmap.ClearEditor;
      FLayerOriginalCapture := false;
      FreeAndNil(FRectEditor);
      FRectEditorCapture := false;
    end;
  end;
end;

function TEditShapeTool.GetCurrentSplineMode: TToolSplineMode;
var
  orig: TVectorOriginal;
begin
  if GetEditMode = esmShape then
  begin
    orig := GetVectorOriginal;
    if Assigned(orig.SelectedShape) and
      (orig.SelectedShape is TCurveShape) then
      exit(ToolSplineModeFromShape(orig.SelectedShape));
  end;
  result := tsmMovePoint;
end;

procedure TEditShapeTool.SetCurrentSplineMode(AMode: TToolSplineMode);
var
  c: TCurveShape;
begin
  if (GetEditMode = esmShape) and
    (GetVectorOriginal.SelectedShape is TCurveShape) then
  begin
    c := TCurveShape(GetVectorOriginal.SelectedShape);
    case AMode of
    tsmMovePoint: if not (c.Usermode in [vsuEdit,vsuCreate]) then c.Usermode := vsuEdit;
    tsmCurveModeAuto: if c.Usermode <> vsuCreate then c.Usermode := vsuCurveSetAuto else
                      if c.PointCount > 1 then c.CurveMode[c.PointCount-2] := cmAuto;
    tsmCurveModeAngle: if c.Usermode <> vsuCreate then c.Usermode := vsuCurveSetAngle else
                       if c.PointCount > 1 then c.CurveMode[c.PointCount-2] := cmAngle;
    tsmCurveModeSpline: if c.Usermode <> vsuCreate then c.Usermode := vsuCurveSetCurve else
                        if c.PointCount > 1 then c.CurveMode[c.PointCount-2] := cmCurve;
    end;
  end;
end;

function TEditShapeTool.ConvertToSpline: boolean;
var
  shapeBefore: TVectorShape;
  orig: TVectorOriginal;
  shapeAfter: TCurveShape;
begin
  if (GetEditMode = esmShape) and
    TCurveShape.CanCreateFrom(GetVectorOriginal.SelectedShape) then
  begin
    orig := GetVectorOriginal;
    shapeBefore:= orig.SelectedShape;
    shapeAfter := TCurveShape.CreateFrom(orig, shapeBefore);
    shapeAfter.JoinStyle := pjsRound;
    orig.ReplaceShape(orig.IndexOfShape(shapeBefore), shapeAfter);
    orig.SelectShape(shapeAfter, False);
    result := true;
  end else
    result := false;
end;

function TEditShapeTool.GetEditMode: TEditShapeMode;
begin
  if InvalidEditMode then exit(esmNone);
  if Assigned(FSelectionRect) then exit(esmSelection)
  else if Assigned(FOriginalRect) then exit(esmOtherOriginal)
  else
  case GetCurrentLayerKind of
  lkGradient: if FIsEditingGradient then exit(esmGradient) else exit(esmNone);
  lkVectorial: if Assigned(GetVectorOriginal.SelectedShape) then exit(esmShape) else exit(esmNoShape);
  else exit(esmNone);
  end;
end;

function TEditShapeTool.InvalidEditMode: boolean;
begin
  if Assigned(FOriginalRect) and
     ((FOriginalLayerId <> Manager.Image.LayerId[Manager.Image.CurrentLayerIndex])
      or not (GetCurrentLayerKind in[lkTransformedBitmap,lkSVG,lkOther])) then
      result := true
  else if Assigned(FSelectionRect) and
       Manager.Image.SelectionMaskEmpty then result := true
  else if FIsEditingGradient and (GetCurrentLayerKind <> lkGradient) then
      result := true
  else
    result := false;
end;

function TEditShapeTool.ForeGradTexMode: TVectorShapeUsermode;
begin
  result := vsuEditPenFill;
end;

function TEditShapeTool.BackGradTexMode: TVectorShapeUsermode;
begin
  result := vsuEditBackFill;
end;

function TEditShapeTool.OutlineGradTexMode: TVectorShapeUsermode;
begin
  result := vsuEditOutlineFill;
end;

function TEditShapeTool.ForeFitMode: TFitMode;
begin
  if IsForeEditGradTexPoints then result := fmNever
  else result := fmIfChange;
end;

function TEditShapeTool.BackFitMode: TFitMode;
begin
  if IsBackEditGradTexPoints then result := fmNever
  else result := fmIfChange;
end;

function TEditShapeTool.OutlineFitMode: TFitMode;
begin
  if IsOutlineEditGradTexPoints then result := fmNever
  else result := fmIfChange;
end;

function TEditShapeTool.GetIsForeEditGradTexPoints: boolean;
begin
  result := (GetEditMode = esmShape) and (GetVectorOriginal.SelectedShape.Usermode = ForeGradTexMode);
end;

function TEditShapeTool.GetIsBackEditGradTexPoints: boolean;
begin
  result := (GetEditMode = esmShape) and (GetVectorOriginal.SelectedShape.Usermode = BackGradTexMode);
end;

function TEditShapeTool.GetIsOutlineEditGradTexPoints: boolean;
begin
  result := (GetEditMode = esmShape) and (GetVectorOriginal.SelectedShape.Usermode = OutlineGradTexMode);
end;

function TEditShapeTool.GetAllowedBackFillTypes: TVectorialFillTypes;
begin
  if GetEditMode = esmGradient then
    result := [vftGradient] else
    Result:=inherited GetAllowedBackFillTypes;
end;

function TEditShapeTool.GetStatusText: string;
var
  m: TAffineMatrix;
begin
  m := AffineMatrixTranslation(-0.5, -0.5) *
       Manager.Image.LayerOriginalMatrix[Manager.Image.CurrentLayerIndex] *
       AffineMatrixTranslation(0.5, 0.5);

  if (GetEditMode = esmShape) and Assigned(GetVectorOriginal.SelectedShape) then
    result := GetShapeStatusText(GetVectorOriginal.SelectedShape, m) else
  if (GetEditMode = esmSelection) and Assigned(FSelectionRect) then
    result := GetShapeStatusText(FSelectionRect, AffineMatrixIdentity) else
  if (GetEditMode = esmOtherOriginal) and Assigned(FOriginalRect) then
    result := GetShapeStatusText(FOriginalRect, AffineMatrixIdentity) else
  if (GetEditMode = esmGradient) then
    result := GetGradientStatusText(GetGradientOriginal, m) else
    Result:=inherited GetStatusText;
end;

constructor TEditShapeTool.Create(AManager: TToolManager);
var
  orig: TVectorOriginal;
begin
  inherited Create(AManager);
  FRectEditor := nil;
  FSelectionRect := nil;
  FOriginalRect := nil;
  FIsEditingGradient:= false;
  FLeftButton:= false;
  FRightButton:= false;
  if GetCurrentLayerKind = lkVectorial then
    orig := GetVectorOriginal
    else orig := nil;
  if not Manager.Image.SelectionMaskEmpty then
  begin
    if Assigned(orig) and Assigned(orig.SelectedShape) then
      orig.DeselectShapes;
    DoEditSelection;
  end else
  if Assigned(orig) and Assigned(orig.SelectedShape) then
    UpdateToolManagerFromShape(orig.SelectedShape);
end;

function TEditShapeTool.DoToolKeyDown(var key: Word): TRect;
var
  handled: boolean;
  keyUtf8: TUTF8Char;
begin
  Result:= EmptyRect;
  if (Key = VK_SPACE) and (GetEditMode = esmShape) and (GetVectorOriginal.SelectedShape is TTextShape) then
  begin
    keyUtf8:= ' ';
    result := ToolKeyPress(keyUtf8);
    Key := 0;
  end else
  begin
    if GetEditMode in [esmShape,esmNoShape] then
    begin
      BindOriginalEvent(true);
      try
        Manager.Image.CurrentState.LayeredBitmap.KeyDown(ShiftState, LCLKeyToSpecialKey(key, ShiftState), handled);
        if handled then key := 0 else
        begin
          if (key = VK_DELETE) and Assigned(GetVectorOriginal.SelectedShape) then
          begin
            GetVectorOriginal.RemoveShape(GetVectorOriginal.SelectedShape);
            key := 0;
          end else
          if (key = VK_ESCAPE) and Assigned(GetVectorOriginal.SelectedShape) then
          begin
            if GetVectorOriginal.SelectedShape.Usermode = vsuEditText then
              GetVectorOriginal.SelectedShape.Usermode := vsuEdit
            else
              GetVectorOriginal.DeselectShapes;
            key := 0;
          end;
        end;
      finally
        BindOriginalEvent(false);
      end;
    end else
    begin
      if (Key = VK_DELETE) and (GetEditMode in [esmGradient,esmOtherOriginal]) then
      begin
        StopEdit(true, true);
        Manager.Image.ClearLayer;
        Key := 0;
      end else
      if key = VK_RETURN then
      begin
        if IsEditing then
        begin
          StopEdit(true, true);
          key := 0;
        end;
      end;
    end;
  end;
end;

function TEditShapeTool.ToolKeyPress(var key: TUTF8Char): TRect;
var
  handled: boolean;
  keyCode: word;
begin
  Result:= EmptyRect;
  if GetEditMode in [esmShape,esmNoShape] then
  begin
    if Assigned(GetVectorOriginal.SelectedShape) and
      (GetVectorOriginal.SelectedShape is TCustomPolypointShape) and
      ((Key='i') or (Key='I')) then
    begin
      keyCode := VK_INSERT;
      ToolKeyDown(keyCode);
      if keyCode = 0 then key := #0;
      keyCode := VK_INSERT;
      ToolKeyUp(keyCode);
      result := EmptyRect;
    end else
    begin
      BindOriginalEvent(true);
      try
        Manager.Image.CurrentState.LayeredBitmap.KeyPress(key, handled);
        if handled then key := #0;
      finally
        BindOriginalEvent(false);
      end;
    end;
  end;
end;

function TEditShapeTool.DoToolKeyUp(var key: Word): TRect;
var
  handled: boolean;
begin
  Result:= EmptyRect;
  if GetEditMode in [esmShape,esmNoShape] then
  begin
    BindOriginalEvent(true);
    try
      Manager.Image.CurrentState.LayeredBitmap.KeyUp(ShiftState, LCLKeyToSpecialKey(key, ShiftState), handled);
      if handled then key := 0;
    finally
      BindOriginalEvent(false);
    end;
  end;
end;

function TEditShapeTool.ToolUp: TRect;
var
  cur: TOriginalEditorCursor;
  handled: boolean;
  m: TAffineMatrix;
  ptView, ptSel: TPointF;
  zoom: Single;
begin
  Result:= EmptyRect;
  if FLeftButton or FRightButton then
  begin
    handled := false;
    if not handled and FRectEditorCapture and Assigned(FRectEditor) then
    begin
      ptView := FRectEditor.Matrix*FLastPos;
      UpdateSnap(FRectEditor);
      FRectEditor.MouseUp(FRightButton, ShiftState, ptView.X,ptView.Y, cur, handled);
      Cursor := OriginalCursorToCursor(cur);
      if handled then
      begin
        result := OnlyRenderChange;
        UpdateMatrixFromRect;
      end else
      begin
        StopEdit(False, False);
        result := OnlyRenderChange;
      end;
    end;
    if not handled and FLayerOriginalCapture and (GetEditMode in [esmGradient, esmShape, esmNoShape]) then
    begin
      BindOriginalEvent(true);
      try
        Manager.Image.CurrentState.LayeredBitmap.MouseUp(FRightButton, ShiftState, FLastPos.X,FLastPos.Y, cur, handled);
        if handled then
        begin
          Cursor := OriginalCursorToCursor(cur);
          result := OnlyRenderChange;
        end;
      finally
        BindOriginalEvent(false);
      end;
    end;
    if not handled and not Manager.Image.SelectionMaskEmpty then
    begin
      ptSel := AffineMatrixInverse(Manager.Image.SelectionTransform)*FLastPos;
      if (Manager.Image.SelectionMaskReadonly.GetPixel(ptSel.x,ptSel.y).green > 0) then
      begin
        if GetEditMode <> esmSelection then
        begin
          StopEdit(false, false);
          DoEditSelection;
          result := OnlyRenderChange;
        end;
        handled := true;
      end else
        if GetEditMode = esmSelection then
        begin
          StopEdit(false, false);
          result := OnlyRenderChange;
        end;
    end;
    if not handled then
    begin
      case GetEditMode of
      esmShape, esmNoShape:
        if not FDownHandled then
        begin
          m := AffineMatrixInverse(Manager.Image.LayerOriginalMatrix[Manager.Image.CurrentLayerIndex]);
          zoom := (VectLen(m[1,1],m[2,1])+VectLen(m[1,2],m[2,2]))/2/Manager.Image.ZoomFactor;
          BindOriginalEvent(true);
          try
            if GetVectorOriginal.MouseClick(m*FLastPos, DoScaleX(PointSize*Manager.CanvasScale, OriginalDPI)*zoom, ssSnap in ShiftState) then
            begin
              handled := true;
              result := OnlyRenderChange;
            end;
          finally
            BindOriginalEvent(false);
          end;
        end;
      esmGradient:
        begin
          FIsEditingGradient:= false;
          result := OnlyRenderChange;
        end;
      end;
    end;
    if not handled and (GetCurrentLayerKind in [lkBitmap, lkTransformedBitmap, lkSVG, lkOther]) and
       (Manager.Image.CurrentLayerReadOnly.GetPixel(FLastPos.X-LayerOffset.X, FLastPos.Y-LayerOffset.Y).alpha <> 0) then
    begin
      if GetEditMode <> esmOtherOriginal then
      begin
        StopEdit(false, false);
        MakeImageOriginal;
        if GetCurrentLayerKind in [lkTransformedBitmap, lkSVG, lkOther] then
        begin
          with Manager.Image.LayerOriginal[Manager.Image.CurrentLayerIndex].
            GetRenderBounds(InfiniteRect, AffineMatrixIdentity) do
            FOriginalRectUntransformed := rectF(Left-0.5, Top-0.5, Right-0.5, Bottom-0.5);
          FOriginalRect := CreateRect(FOriginalRectUntransformed,
            Manager.Image.LayerOriginalMatrix[Manager.Image.CurrentLayerIndex]);
          FOriginalLayerId:= Manager.Image.LayerId[Manager.Image.CurrentLayerIndex];
          result := OnlyRenderChange;
        end;
      end;
      handled := true;
    end;
    if not handled and (GetCurrentLayerKind = lkGradient) then
    begin
      if not FIsEditingGradient then
      begin
        FIsEditingGradient:= true;
        Manager.BackFill.SetGradient(GetGradientOriginal, false);
        Manager.UpdateContextualToolbars;
        handled := true;
        result := OnlyRenderChange;
      end;
    end;
    FLeftButton:= false;
    FRightButton:= false;
    if Manager.Image.DraftOriginal then
      UpdateDraftMode;
  end;
end;

function TEditShapeTool.ToolCommand(ACommand: TToolCommand): boolean;
var
  key: Word;
  b: TRect;
  bmp: TBGRABitmap;
  s: TRectShape;
begin
  if not ToolProvideCommand(ACommand) then exit(false);
  if ACommand = tcDelete then
  begin
    key := VK_DELETE;
    ToolKeyDown(key);
    if key = 0 then
    begin
      key := VK_DELETE;
      ToolKeyUp(key);
      result := true;
    end else
      result := false;
  end else
  if ACommand = tcPaste then
  begin
    result := false;
    MakeVectorOriginal;
    if GetCurrentLayerKind = lkVectorial then
    begin
      BindOriginalEvent(true);
      try
        PasteShapesFromClipboard(GetVectorOriginal, GetOriginalTransform, Manager.Image.VisibleArea);
      finally
        BindOriginalEvent(false);
      end;
      result := true;
    end;
  end else
  begin
    result := true;
    case GetEditMode of
    esmShape:
      try
        BindOriginalEvent(true);
        case ACommand of
          tcMoveUp: GetVectorOriginal.SelectedShape.MoveUp(true);
          tcMoveToFront: GetVectorOriginal.SelectedShape.BringToFront;
          tcMoveDown: GetVectorOriginal.SelectedShape.MoveDown(true);
          tcMoveToBack: GetVectorOriginal.SelectedShape.SendToBack;
          tcCopy: Result:= CopyShapesToClipboard([GetVectorOriginal.SelectedShape], GetOriginalTransform);
          tcCut: begin
                   result := CopyShapesToClipboard([GetVectorOriginal.SelectedShape], GetOriginalTransform) and
                             GetVectorOriginal.RemoveShape(GetVectorOriginal.SelectedShape);
                 end;
          tcAlignLeft..tcAlignBottom: AlignShape(GetVectorOriginal.SelectedShape, ACommand,
                       Manager.Image.LayerOriginalMatrix[Manager.Image.CurrentLayerIndex],
                       rect(0,0,Manager.Image.Width,Manager.Image.Height));
          tcForeEditGradTexPoints: if GetVectorOriginal.SelectedShape.Usermode = ForeGradTexMode then
                                    GetVectorOriginal.SelectedShape.Usermode := vsuEdit else
                                    GetVectorOriginal.SelectedShape.Usermode := ForeGradTexMode;
          tcBackEditGradTexPoints: if GetVectorOriginal.SelectedShape.Usermode = BackGradTexMode then
                                    GetVectorOriginal.SelectedShape.Usermode := vsuEdit else
                                    GetVectorOriginal.SelectedShape.Usermode := BackGradTexMode;
          tcOutlineEditGradTexPoints: if GetVectorOriginal.SelectedShape.Usermode = OutlineGradTexMode then
                                    GetVectorOriginal.SelectedShape.Usermode := vsuEdit else
                                    GetVectorOriginal.SelectedShape.Usermode := OutlineGradTexMode;
          tcForeAdjustToShape: GetVectorOriginal.SelectedShape.PenFill.FitGeometry(SuggestGradientBox);
          tcBackAdjustToShape: GetVectorOriginal.SelectedShape.BackFill.FitGeometry(SuggestGradientBox);
          tcOutlineAdjustToShape: GetVectorOriginal.SelectedShape.OutlineFill.FitGeometry(SuggestGradientBox);
          tcShapeToSpline: result := ConvertToSpline;
          else result := false;
        end;
      finally
        BindOriginalEvent(false);
      end;
    esmGradient:
      begin
        case ACommand of
          tcBackAdjustToShape: GetGradientOriginal.FitGeometry(SuggestGradientBox);
          tcCut,tcCopy: begin
            s := TRectShape.Create(nil);
            try
              s.PenStyle := ClearPenStyle;
              s.QuickDefine(PointF(-0.5,-0.5),PointF(Manager.Image.Width-0.5,Manager.Image.Height-0.5));
              s.BackFill.SetGradient(GetGradientOriginal, false);
              s.BackFill.Transform(Manager.Image.LayerOriginalMatrix[Manager.Image.CurrentLayerIndex]);
              CopyShapesToClipboard([s],AffineMatrixIdentity);
            finally
              s.free;
            end;
            if ACommand = tcCut then ToolCommand(tcDelete);
          end;
          else result := false;
        end;
      end;
    esmOtherOriginal:
      begin
        case ACommand of
          tcCut,tcCopy: begin
              b := Manager.Image.CurrentLayerReadOnly.GetImageBounds;
              if not b.IsEmpty then
              begin
                if GetCurrentLayerKind = lkTransformedBitmap then
                begin
                  bmp := GetImageOriginal.GetImageCopy;
                  s:= TRectShape.Create(nil);
                  s.QuickDefine(PointF(-0.5,-0.5),PointF(bmp.Width-0.5,bmp.Height-0.5));
                  s.PenStyle := ClearPenStyle;
                  s.BackFill.SetTexture(bmp,AffineMatrixIdentity,255,trNone);
                  s.Transform(Manager.Image.LayerOriginalMatrix[Manager.Image.CurrentLayerIndex]);
                  bmp.FreeReference;
                end else
                begin
                  bmp := Manager.Image.CurrentLayerReadOnly.GetPart(b) as TBGRABitmap;
                  s:= TRectShape.Create(nil);
                  s.QuickDefine(PointF(b.Left-0.5,b.Top-0.5),PointF(b.Right-0.5,b.Bottom-0.5));
                  s.PenStyle := ClearPenStyle;
                  s.BackFill.SetTexture(bmp,AffineMatrixTranslation(b.Left,b.Top),255,trNone);
                  with Manager.Image.LayerOffset[Manager.Image.CurrentLayerIndex] do
                    s.Transform(AffineMatrixTranslation(x,y));
                  bmp.FreeReference;
                end;
                try
                  CopyShapesToClipboard([s],AffineMatrixIdentity);
                finally
                  s.Free;
                end;
                if ACommand = tcCut then ToolCommand(tcDelete);
              end;
            end;
          tcAlignLeft..tcAlignBottom:
              begin
                AlignShape(FOriginalRect, ACommand,
                           AffineMatrixIdentity,rect(0,0,Manager.Image.Width,Manager.Image.Height));
                UpdateMatrixFromRect;
              end
          else result := false;
        end;
      end;
    esmSelection:
      begin
        if ACommand in [tcAlignLeft..tcAlignBottom] then
        begin
          AlignShape(FSelectionRect, ACommand,
                     AffineMatrixIdentity,rect(0,0,Manager.Image.Width,Manager.Image.Height));
          UpdateMatrixFromRect;
        end;
      end;
    end;
  end;
end;

function TEditShapeTool.ToolProvideCommand(ACommand: TToolCommand): boolean;
begin
  case ACommand of
  tcCut,tcCopy,tcDelete: result:= GetEditMode in [esmShape,esmOtherOriginal,esmGradient];
  tcForeAdjustToShape,tcOutlineAdjustToShape: result := GetEditMode = esmShape;
  tcBackAdjustToShape: result := GetEditMode in [esmShape,esmGradient];
  tcForeEditGradTexPoints: result := (GetEditMode = esmShape) and
                     (ForeGradTexMode in GetVectorOriginal.SelectedShape.MultiUsermodes);
  tcBackEditGradTexPoints: result := (GetEditMode = esmShape) and
                     (BackGradTexMode in GetVectorOriginal.SelectedShape.MultiUsermodes);
  tcOutlineEditGradTexPoints: result := (GetEditMode = esmShape) and
                     (OutlineGradTexMode in GetVectorOriginal.SelectedShape.MultiUsermodes);
  tcShapeToSpline: result:= (GetEditMode = esmShape)
                            and TCurveShape.CanCreateFrom(GetVectorOriginal.SelectedShape);
  tcAlignLeft..tcAlignBottom: result:= GetEditMode in [esmShape, esmOtherOriginal, esmSelection];
  tcPaste: result := ClipboardHasShapes and (GetCurrentLayerKind in [lkEmpty,lkBitmap,lkTransformedBitmap,lkVectorial]);
  tcMoveUp,tcMoveToFront: result := (GetEditMode = esmShape)
                                     and not GetVectorOriginal.SelectedShape.IsFront;
  tcMoveDown,tcMoveToBack: result := (GetEditMode = esmShape)
                                      and not GetVectorOriginal.SelectedShape.IsBack;
  else
    result := false;
  end;
end;

function TEditShapeTool.SuggestGradientBox: TAffineBox;
begin
  if GetEditMode = esmShape then
    result := GetVectorOriginal.SelectedShape.SuggestGradientBox(AffineMatrixIdentity)
  else
    result:= inherited SuggestGradientBox;
end;

{ TVectorialTool }

procedure TVectorialTool.ShapeChange(ASender: TObject; ABounds: TRectF; ADiff: TVectorShapeDiff);
var
  toolDest: TBGRABitmap;
  r: TRect;
  matrix: TAffineMatrix;
begin
  toolDest := GetToolDrawingLayer;
  matrix := VectorTransform(false);
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
  with (Editor.Matrix*PointF(toolDest.Width,toolDest.Height)) do
    newEditorBounds := Editor.GetRenderBounds(rect(0,0,ceil(x),ceil(y)));
  r := RectUnion(FPreviousEditorBounds,newEditorBounds);
  if not r.IsEmpty then
    Manager.Image.RenderMayChange(r,false);
  FPreviousEditorBounds := newEditorBounds;
end;

procedure TVectorialTool.ShapeRemoveQuery(ASender: TObject;
  var AHandled: boolean);
var
  r: TRect;
  toolDest: TBGRABitmap;
begin
  if ASender <> FShape then exit;
  AHandled := true;
  toolDest := GetToolDrawingLayer;
  r := CancelShape;
  Action.NotifyChange(toolDest, r);
end;

function TVectorialTool.GetStatusText: string;
begin
  if Assigned(FShape) then
    result := GetShapeStatusText(FShape, VectorTransform(True))
    else Result:=inherited GetStatusText;
end;

function TVectorialTool.SlowShape: boolean;
begin
  if Assigned(FShape) then
    result := FShape.GetIsSlow(VectorTransform(false))
  else
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
     (GetCurrentLayerKind = lkVectorial) then
    FUseOriginal:= true
  else
    FUseOriginal:= false;
end;

function TVectorialTool.ReplaceLayerAndAddShape(out ARect: TRect): TCustomImageDifference;
var
  transf: TAffineMatrix;
  diff: TComposedImageDifference;
  replaceDiff: TReplaceLayerByVectorOriginalDifference;
  layerId: integer;
  addDiff: TAddShapeToVectorOriginalDifference;
begin
  layerId := Manager.Image.LayerId[Manager.Image.CurrentLayerIndex];
  transf := VectorTransform(false);
  diff := TComposedImageDifference.Create;
  replaceDiff := TReplaceLayerByVectorOriginalDifference.Create(Manager.Image.CurrentState,Manager.Image.CurrentLayerIndex,
                   GetCurrentLayerKind = lkVectorial);
  diff.Add(replaceDiff);
  transf := AffineMatrixInverse(Manager.Image.LayerOriginalMatrix[Manager.Image.CurrentLayerIndex])*transf;
  FShape.Transform(transf);
  addDiff := TAddShapeToVectorOriginalDifference.Create(Manager.Image.CurrentState,layerId,FShape);
  diff.Add(addDiff);
  result := diff;
  ARect := addDiff.ChangingBounds;
  FShape := nil;
end;

procedure TVectorialTool.ShapeValidated;
begin
  //nothing
end;

function TVectorialTool.ForeGradTexMode: TVectorShapeUsermode;
begin
  if Assigned(FShape) and FSwapColor then
  begin
    if vsfBackFill in FShape.Fields then
      result := vsuEditBackFill
    else if vsfOutlineFill in FShape.Fields then
      result := vsuEditOutlineFill
    else
      result := vsuEditPenFill;
  end else
    result := vsuEditPenFill;
end;

function TVectorialTool.BackGradTexMode: TVectorShapeUsermode;
begin
  if Assigned(FShape) and FSwapColor and (vsfPenFill in FShape.Fields) then
    result := vsuEditPenFill
  else
    result := vsuEditBackFill;
end;

function TVectorialTool.OutlineGradTexMode: TVectorShapeUsermode;
begin
  if Assigned(FShape) and FSwapColor and ([vsfPenFill,vsfBackFill]*FShape.Fields = [vsfPenFill]) then
    result := vsuEditPenFill
  else
    result := vsuEditOutlineFill;
end;

function TVectorialTool.ForeFitMode: TFitMode;
begin
  if IsForeEditGradTexPoints then result := fmNever
  else result := fmIfChange;
end;

function TVectorialTool.BackFitMode: TFitMode;
begin
  if IsBackEditGradTexPoints then result := fmNever
  else result := fmIfChange;
end;

function TVectorialTool.OutlineFitMode: TFitMode;
begin
  if IsOutlineEditGradTexPoints then result := fmNever
  else result := fmIfChange;
end;

function TVectorialTool.ManagerForeFill: TVectorialFill;
begin
  if Assigned(FShape) and FSwapColor then
  begin
    if vsfBackFill in FShape.Fields then
      result := Manager.BackFill
    else if vsfOutlineFill in FShape.Fields then
      result := Manager.OutlineFill
    else
      result := Manager.ForeFill;
  end else
    result := Manager.ForeFill;
end;

function TVectorialTool.ManagerBackFill: TVectorialFill;
begin
  if Assigned(FShape) and FSwapColor and (vsfPenFill in FShape.Fields) then
    result := Manager.ForeFill
  else
    result := Manager.BackFill;
end;

function TVectorialTool.ManagerOutlineFill: TVectorialFill;
begin
  if Assigned(FShape) and FSwapColor and ([vsfPenFill,vsfBackFill]*FShape.Fields = [vsfPenFill]) then
    result := Manager.ForeFill
  else
    result := Manager.OutlineFill;
end;
   
function TVectorialTool.GetIsForeEditGradTexPoints: boolean;
begin
  result := Assigned(FShape) and (FShape.Usermode = ForeGradTexMode);
end;

function TVectorialTool.GetIsBackEditGradTexPoints: boolean;
begin
  result := Assigned(FShape) and (FShape.Usermode = BackGradTexMode);
end;

function TVectorialTool.GetIsOutlineEditGradTexPoints: boolean;
begin
  result := Assigned(FShape) and (FShape.Usermode = OutlineGradTexMode);
end;

function TVectorialTool.GetGridMatrix: TAffineMatrix;
begin
  if Manager.Image.ZoomFactor > DoScaleX(35, OriginalDPI)/10 then
    result := AffineMatrixScale(0.5,0.5)
  else
  begin
    if Assigned(FShape) and
         (not (vsfPenFill in FShape.MultiFields) or
           (FShape.PenFill.IsFullyTransparent)) then
      result := AffineMatrixTranslation(0.5, 0.5)
    else
      result := AffineMatrixIdentity;
  end;
end;

function TVectorialTool.ValidateShape: TRect;
var
  layerId: LongInt;
  rF: TRectF;
  changeBounds: TRect;
  addDiff: TAddShapeToVectorOriginalDifference;
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
      rF := FShape.GetRenderBounds(rect(0,0,Manager.Image.Width,Manager.Image.Height), VectorTransform(false));
      if rF.IntersectsWith(rectF(0,0,Manager.Image.Width,Manager.Image.Height)) then
      begin
        if UseOriginal then
        begin
          layerId := Manager.Image.LayerId[Manager.Image.CurrentLayerIndex];
          addDiff := TAddShapeToVectorOriginalDifference.Create(Manager.Image.CurrentState,layerId,FShape);
          changeBounds := addDiff.ChangingBounds;
          Manager.Image.AddUndo(addDiff);
          FShape := nil;
        end
        else
          Manager.Image.AddUndo(ReplaceLayerAndAddShape(changeBounds));
        Manager.Image.ImageMayChange(changeBounds);
      end;
      ClearShape;
    end else
    begin
      ValidateActionPartially;
      ClearShape;
    end;
    Cursor := crDefault;
    result := OnlyRenderChange;
    UpdateUseOriginal;
    ShapeValidated;
  end else
    result := EmptyRect;
end;

function TVectorialTool.CancelShape: TRect;
begin
  CancelActionPartially;
  ClearShape;
  Cursor := crDefault;
  result := OnlyRenderChange;
end;

function TVectorialTool.GetEditor: TBGRAOriginalEditor;
begin
  FEditor.GridActive := ssSnap in ShiftState;
  result := FEditor;
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

function TVectorialTool.CreateShape: TVectorShape;
begin
  result := ShapeClass.Create(nil);
end;

procedure TVectorialTool.ClearShape;
begin
  FreeAndNil(FShape);
  FreeAndNil(FTemporaryStorage);
  Editor.Clear;
end;

function TVectorialTool.UseOriginal: boolean;
begin
  result := FUseOriginal;
end;

function TVectorialTool.HasBrush: boolean;
begin
  result := (toFillShape in GetManagerShapeOptions) or not (ctShape in GetContextualToolbars);
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

procedure TVectorialTool.AssignShapeStyle(AMatrix: TAffineMatrix; AAlwaysFit: boolean);
var
  f: TVectorShapeFields;
  zoom: Single;
  gradBox: TAffineBox;
  fitMode: TFitMode;
begin
  zoom := (VectLen(AMatrix[1,1],AMatrix[2,1])+VectLen(AMatrix[1,2],AMatrix[2,2]))/2;
  f := FShape.MultiFields;
  gradBox := FShape.SuggestGradientBox(AffineMatrixIdentity);
  if vsfPenFill in f then
  begin
    if HasPen then
    begin
      if AAlwaysFit then fitMode := fmAlways else fitMode := ForeFitMode;
      AssignFill(FShape.PenFill, ManagerForeFill, gradBox, fitMode)
    end else
      FShape.PenFill.Clear;
  end;
  if vsfPenWidth in f then FShape.PenWidth := zoom*Manager.PenWidth;
  if vsfPenStyle in f Then FShape.PenStyle := PenStyleToBGRA(Manager.PenStyle);
  if vsfJoinStyle in f then FShape.JoinStyle:= Manager.JoinStyle;
  if vsfBackFill in f then
  begin
    if HasBrush then
    begin
      if AAlwaysFit then fitMode := fmAlways else fitMode := BackFitMode;
      AssignFill(FShape.BackFill, ManagerBackFill, gradBox, fitMode)
    end else
      FShape.BackFill.Clear;
  end;
  if vsfOutlineFill in f then
  begin
    if Manager.TextOutline then
    begin
      if AAlwaysFit then fitMode := fmAlways else fitMode := OutlineFitMode;
      AssignFill(FShape.OutlineFill, ManagerOutlineFill, gradBox, fitMode);
    end else
      FShape.OutlineFill.Clear;
  end;
  if (vsfOutlineWidth in f) and Manager.TextOutline then
    FShape.OutlineWidth := zoom*Manager.TextOutlineWidth;
end;

function TVectorialTool.GetManagerShapeOptions: TShapeOptions;
begin
  result := Manager.ShapeOptions;
end;

procedure TVectorialTool.QuickDefineShape(AStart, AEnd: TPointF);
begin
  FShape.QuickDefine(AStart, AEnd);
end;

function TVectorialTool.RoundCoordinate(constref ptF: TPointF): TPointF;
begin
  if not (toDrawShape in GetManagerShapeOptions) or
    (Assigned(FShape) and not (vsfPenFill in FShape.MultiFields)) then
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
  matrix: TAffineMatrix;
begin
  result := FPreviousUpdateBounds;
  RestoreBackupDrawingLayer;
  matrix := VectorTransform(false);
  FLastDraftUpdate := PreferDraftUpdate;
  newBounds := GetCustomShapeBounds(toolDest.ClipRect,matrix,FLastDraftUpdate);
  result := RectUnion(result, newBounds);
  oldClip := toolDest.IntersectClip(newBounds);
  DrawCustomShape(toolDest,matrix,FLastDraftUpdate);
  toolDest.ClipRect := oldClip;
  FPreviousUpdateBounds := newBounds;
end;

function TVectorialTool.PreferDraftUpdate: boolean;
begin
  if Assigned(FShape) then
    result := (FEditor.IsMovingPoint or FShape.IsFollowingMouse or FQuickDefine) and SlowShape
  else
    result := false;
end;

function TVectorialTool.VectorTransform(APixelCentered: boolean): TAffineMatrix;
begin
  if not UseOriginal then
    result := AffineMatrixIdentity
  else
    result := Manager.Image.LayerOriginalMatrix[Manager.Image.CurrentLayerIndex];
  if APixelCentered then
    result := MatrixForPixelCentered(result);
end;

procedure TVectorialTool.UpdateCursor(ACursor: TOriginalEditorCursor);
begin
  Cursor := OriginalCursorToCursor(ACursor);
end;

function TVectorialTool.FixLayerOffset: boolean;
begin
  Result:= false;
end;

function TVectorialTool.DoToolDown(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF; rightBtn: boolean): TRect;
var
  viewPt, shapePt: TPointF;
  cur: TOriginalEditorCursor;
  handled: boolean;
begin
  result := EmptyRect;
  FRightDown := rightBtn;
  FLeftDown := not rightBtn;
  with LayerOffset do
    FLastPos := AffineMatrixTranslation(X,Y)*ptF;
  if Assigned(FShape) then
  begin
    viewPt := Editor.Matrix*AffineMatrixInverse(VectorTransform(true))*FLastPos;
    Editor.MouseDown(rightBtn, ShiftState, viewPt.X,viewPt.Y, cur, handled);
    if not handled and Assigned(FShape) then
    begin
      shapePt := AffineMatrixInverse(VectorTransform(true))*FLastPos;
      If Editor.GridActive then shapePt := Editor.SnapToGrid(shapePt, false);
      FShape.MouseDown(rightBtn, ShiftState, shapePt.X,shapePt.Y, cur, handled);
    end;
    UpdateCursor(cur);
    if handled then exit
    else result := RectUnion(result, ValidateShape);
  end;

  if FShape=nil then
  begin
    if UseOriginal and
      ((Manager.Image.LayerOriginal[Manager.Image.CurrentLayerIndex] as TVectorOriginal).GetShapesCost >= MediumShapeCost) then
    begin
      MessagePopup(rsTooManyShapesInLayer, 3000);
    end
    else
    if (GetCurrentLayerKind = lkSVG) and not IsSelectingTool then
    begin
      MessagePopup(rsCannotDrawShapeOnSVGLayer, 3000);
    end
    else
    begin
      toolDest := GetToolDrawingLayer;
      FSwapColor:= rightBtn;
      if IsSelectingTool then
        FLayerWasEmpty := Manager.Image.SelectionMaskEmpty
      else if Manager.Image.SelectionMaskEmpty then
        FLayerWasEmpty := Manager.Image.CurrentLayerEmpty
      else
        FLayerWasEmpty := Manager.Image.SelectionLayerIsEmpty;
      FShape := CreateShape;
      if FTemporaryStorage = nil then FTemporaryStorage := TBGRAMemOriginalStorage.Create;
      FShape.TemporaryStorage := FTemporaryStorage;
      FQuickDefine := true;
      FQuickDefineStartPoint := RoundCoordinate(FLastPos);
      FQuickDefineEndPoint := FQuickDefineStartPoint;
      FShape.BeginUpdate;
        QuickDefineShape(FQuickDefineStartPoint,FQuickDefineEndPoint);
        FLastShapeTransform := AffineMatrixInverse(VectorTransform(false));
        FShape.Transform(FLastShapeTransform);
        shapePt := AffineMatrixInverse(VectorTransform(true))*FLastPos;
        handled := false;
        FShape.MouseMove(ShiftState, shapePt.X,shapePt.Y, cur, handled);
        AssignShapeStyle(FLastShapeTransform, true);
      FShape.EndUpdate;
      FShape.OnChange:= @ShapeChange;
      FShape.OnEditingChange:=@ShapeEditingChange;
      FShape.OnRemoveQuery:= @ShapeRemoveQuery;
      result := RectUnion(result, UpdateShape(toolDest));
      if FShape is TCustomRectShape then
        Manager.ToolPopup(tpmHoldKeyForSquare, VK_SHIFT);
    end;
  end;
end;

function TVectorialTool.DoToolMove(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF): TRect;
var
  s: TPointF;
  avg: single;
  viewPt, shapePt: TPointF;
  handled: boolean;
  cur: TOriginalEditorCursor;
begin
  Editor.GridMatrix := GetGridMatrix;
  with LayerOffset do
    FLastPos := AffineMatrixTranslation(X,Y)*ptF;
  if FQuickDefine then
  begin
    FQuickDefineEndPoint := RoundCoordinate(ptF);
    if ssShift in ShiftState then
    begin
      s := FQuickDefineEndPoint-FQuickDefineStartPoint;
      avg := sqrt(abs(s.x*s.y));
      if s.x > 0 then FQuickDefineEndPoint.x := FQuickDefineStartPoint.x + avg else FQuickDefineEndPoint.x := FQuickDefineStartPoint.x - avg;
      if s.y > 0 then FQuickDefineEndPoint.y := FQuickDefineStartPoint.y + avg else FQuickDefineEndPoint.y := FQuickDefineStartPoint.y - avg;
    end;
    FShape.BeginUpdate;
      QuickDefineShape(FQuickDefineStartPoint, FQuickDefineEndPoint);
      FLastShapeTransform := AffineMatrixInverse(VectorTransform(false));
      FShape.Transform(FLastShapeTransform);
      AssignShapeStyle(FLastShapeTransform, true);
    FShape.EndUpdate;
    result := OnlyRenderChange;
  end else
  begin
    viewPt := Editor.Matrix*AffineMatrixInverse(VectorTransform(true))*FLastPos;
    Editor.MouseMove(ShiftState, viewPt.X,viewPt.Y, cur, handled);
    if not handled and Assigned(FShape) then
    begin
      shapePt := AffineMatrixInverse(VectorTransform(true))*FLastPos;
      If Editor.GridActive then shapePt := Editor.SnapToGrid(shapePt, false);
      FShape.MouseMove(ShiftState, shapePt.X,shapePt.Y, cur, handled);
    end;
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
    AssignShapeStyle(FLastShapeTransform, false);
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
  FEditor.GridMatrix := GetGridMatrix;
  FEditor.Focused := true;
  FPreviousEditorBounds := EmptyRect;
  FLastShapeTransform := AffineMatrixIdentity;
  FTemporaryStorage := TBGRAMemOriginalStorage.Create;
end;

function TVectorialTool.ToolUp: TRect;
var
  viewPt, shapePt: TPointF;
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
    viewPt := Editor.Matrix*AffineMatrixInverse(VectorTransform(true))*FLastPos;
    Editor.MouseUp(wasRight, ShiftState, viewPt.X,viewPt.Y, cur, handled);
    if not handled and Assigned(FShape) then
    begin
      shapePt := AffineMatrixInverse(VectorTransform(true))*FLastPos;
      FShape.MouseUp(wasRight, ShiftState, shapePt.X,shapePt.Y, cur, handled);
    end;
    UpdateCursor(cur);
    result := EmptyRect;
  end;
  if (FLastDraftUpdate and not PreferDraftUpdate) and Assigned(FShape) then
    result := UpdateShape(GetToolDrawingLayer);
end;

function TVectorialTool.DoToolKeyDown(var key: Word): TRect;
var
  handled: boolean;
begin
  result := EmptyRect;
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
    Editor.KeyDown(ShiftState, LCLKeyToSpecialKey(Key, ShiftState), handled);
    if not handled and Assigned(FShape) then FShape.KeyDown(ShiftState, LCLKeyToSpecialKey(Key, ShiftState), handled);
    if handled then Key := 0;
  end;
end;

function TVectorialTool.ToolKeyPress(var key: TUTF8Char): TRect;
var
  handled: boolean;
begin
  result := EmptyRect;
  Editor.KeyPress(key, handled);
  if not handled and Assigned(FShape) then FShape.KeyPress(key, handled);
  if handled then Key := #0;
end;

function TVectorialTool.DoToolKeyUp(var key: Word): TRect;
var
  handled: boolean;
begin
  result := EmptyRect;
  Editor.KeyUp(ShiftState, LCLKeyToSpecialKey(Key, ShiftState), handled);
  if not handled and Assigned(FShape) then FShape.KeyUp(ShiftState, LCLKeyToSpecialKey(Key, ShiftState), handled);
  if handled then Key := 0;
end;

function TVectorialTool.ToolCommand(ACommand: TToolCommand): boolean;
var
  r: TRect;
  toolDest: TBGRABitmap;
begin
  result := false;
  case ACommand of
  tcCopy:
      if ToolProvideCommand(tcCopy) then
        result := CopyShapesToClipboard([FShape], VectorTransform(false));
  tcCut:
      if ToolCommand(tcCopy) then
      begin
        toolDest := GetToolDrawingLayer;
        r := CancelShape;
        Action.NotifyChange(toolDest, r);
        result := true;
      end;
  tcForeAdjustToShape: if Assigned(FShape) then FShape.PenFill.FitGeometry(SuggestGradientBox);
  tcBackAdjustToShape: if Assigned(FShape) then FShape.BackFill.FitGeometry(SuggestGradientBox);
  tcOutlineAdjustToShape: if Assigned(FShape) then FShape.OutlineFill.FitGeometry(SuggestGradientBox);
  tcForeEditGradTexPoints: if Assigned(FShape) and not FQuickDefine then
                          begin
                            if FShape.Usermode = ForeGradTexMode then
                              FShape.Usermode := vsuEdit else
                              FShape.Usermode := ForeGradTexMode;
                          end;
  tcBackEditGradTexPoints: if Assigned(FShape) and not FQuickDefine then
                          begin
                            if FShape.Usermode = BackGradTexMode then
                              FShape.Usermode := vsuEdit else
                              FShape.Usermode := BackGradTexMode;
                          end;
  tcOutlineEditGradTexPoints: if Assigned(FShape) and not FQuickDefine then
                          begin
                            if FShape.Usermode = OutlineGradTexMode then
                              FShape.Usermode := vsuEdit else
                              FShape.Usermode := OutlineGradTexMode;
                          end;
  tcFinish: begin
              toolDest := GetToolDrawingLayer;
              r := ValidateShape;
              Action.NotifyChange(toolDest, r);
              result := true;
          end;
  tcAlignLeft..tcAlignBottom:
      if ToolProvideCommand(ACommand) then
      begin
        AlignShape(FShape, ACommand,
                 Manager.Image.LayerOriginalMatrix[Manager.Image.CurrentLayerIndex],
                 rect(0,0,Manager.Image.Width,Manager.Image.Height));
        result := true;
      end;
  else
    result := false;
  end;
end;

function TVectorialTool.ToolProvideCommand(ACommand: TToolCommand): boolean;
begin
  case ACommand of
  tcCopy,tcCut: Result:= not IsSelectingTool and not FQuickDefine and Assigned(FShape);
  tcFinish: result := not IsIdle;
  tcForeAdjustToShape, tcBackAdjustToShape, tcOutlineAdjustToShape:
      result := not IsSelectingTool and Assigned(FShape) and not FQuickDefine;
  tcForeEditGradTexPoints: result := not IsSelectingTool and Assigned(FShape) and not FQuickDefine and
                            (ForeGradTexMode in FShape.Usermodes) and not (FShape.Usermode = vsuCreate);
  tcBackEditGradTexPoints: result := not IsSelectingTool and Assigned(FShape) and not FQuickDefine and
                            (BackGradTexMode in FShape.Usermodes) and not (FShape.Usermode = vsuCreate);
  tcOutlineEditGradTexPoints: result := not IsSelectingTool and Assigned(FShape) and not FQuickDefine and
                            (OutlineGradTexMode in FShape.Usermodes) and not (FShape.Usermode = vsuCreate);
  tcShapeToSpline: result:= not IsSelectingTool and not FQuickDefine and Assigned(FShape)
                            and TCurveShape.CanCreateFrom(FShape);
  tcAlignLeft..tcAlignBottom: Result:= not FQuickDefine and Assigned(FShape);
  tcMoveDown,tcMoveToBack: result := not IsSelectingTool and not FQuickDefine and Assigned(FShape)
          and not AlwaysRasterizeShape and Manager.Image.SelectionMaskEmpty and not FLayerWasEmpty;
  else result := false;
  end;
end;

function TVectorialTool.SuggestGradientBox: TAffineBox;
begin
  if Assigned(FShape) then
    result := FShape.SuggestGradientBox(AffineMatrixIdentity)
  else
    result:= inherited SuggestGradientBox;
end;

function TVectorialTool.GetContextualToolbars: TContextualToolbars;
begin
  result := ContextualToolbarsFromShape(ShapeClass, FShape);
end;

function TVectorialTool.Render(VirtualScreen: TBGRABitmap; VirtualScreenWidth,
  VirtualScreenHeight: integer;
  BitmapToVirtualScreen: TBitmapToVirtualScreenFunction): TRect;
var
  orig, xAxis, yAxis: TPointF;
  editMatrix: TAffineMatrix;
begin
  if Assigned(FShape) then
  begin
    with LayerOffset do
    begin
      orig := BitmapToVirtualScreen(PointF(0,0));
      xAxis := BitmapToVirtualScreen(PointF(1,0));
      yAxis := BitmapToVirtualScreen(PointF(0,1));
    end;
    Editor.Clear;
    editMatrix := AffineMatrix(xAxis-orig,yAxis-orig,orig)*VectorTransform(true);
    if IsAffineMatrixInversible(editMatrix) then
    begin
      Editor.Matrix := editMatrix;
      Editor.PointSize := DoScaleX(PointSize*Manager.CanvasScale, OriginalDPI);
      if Assigned(FShape) then FShape.ConfigureEditor(Editor);
      if Assigned(VirtualScreen) then
        Result:= Editor.Render(VirtualScreen, rect(0,0,VirtualScreen.Width,VirtualScreen.Height))
      else
        Result:= Editor.GetRenderBounds(rect(0,0,VirtualScreenWidth,VirtualScreenHeight));
    end else
      result := EmptyRect;
  end else
  begin
    result := EmptyRect;
    Editor.Clear;
  end;
  FPreviousEditorBounds := result;
end;

destructor TVectorialTool.Destroy;
begin
  if Assigned(FShape) then ValidateShape;
  FEditor.Free;
  FTemporaryStorage.Free;
  inherited Destroy;
end;

initialization

  RegisterTool(ptEditShape, TEditShapeTool);

end.

