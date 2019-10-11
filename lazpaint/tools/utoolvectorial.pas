unit UToolVectorial;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, BGRABitmap, BGRABitmapTypes,
  BGRALayerOriginal, BGRAGraphics, LCVectorOriginal,
  UTool, UImageType, ULayerAction, LCVectorRectShapes,
  BGRAGradientOriginal, UStateType;

type
  TToolSplineMode = (tsmMovePoint, tsmCurveModeAuto, tsmCurveModeAngle, tsmCurveModeSpline);

function ToolSplineModeFromShape(AShape: TVectorShape): TToolSplineMode;

type
  { TVectorialTool }

  TVectorialTool = class(TGenericTool)
  private
    function GetIsHandDrawing: boolean;
    function GetIsIdle: boolean;
  protected
    FLayerWasEmpty: boolean;
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
    function VectorTransform(APixelCentered: boolean): TAffineMatrix;
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
    function ReplaceLayerAndAddShape(out ARect: TRect): TCustomImageDifference; virtual;
    procedure ShapeValidated; virtual;
  public
    function ValidateShape: TRect;
    function CancelShape: TRect;
    constructor Create(AManager: TToolManager); override;
    function ToolUp: TRect; override;
    function ToolKeyDown(var key: Word): TRect; override;
    function ToolKeyPress(var key: TUTF8Char): TRect; override;
    function ToolKeyUp(var key: Word): TRect; override;
    function ToolCommand(ACommand: TToolCommand): boolean; override;
    function ToolProvideCommand(ACommand: TToolCommand): boolean; override;
    function Render(VirtualScreen: TBGRABitmap; {%H-}VirtualScreenWidth, {%H-}VirtualScreenHeight: integer; BitmapToVirtualScreen: TBitmapToVirtualScreenFunction):TRect; override;
    property IsIdle: boolean read GetIsIdle;
    property IsHandDrawing: boolean read GetIsHandDrawing;
    destructor Destroy; override;
  end;

  { TEditShapeTool }

  TEditShapeMode = (esmNone, esmSelection, esmGradient, esmOtherOriginal, esmShape, esmNoShape);
  TEditShapeTool = class(TGenericTool)
  protected
    FShiftState: TShiftState;
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
    procedure BindOriginalEvent(ABind: boolean);
    procedure SelectShape({%H-}ASender: TObject; AShape: TVectorShape; {%H-}APreviousShape: TVectorShape);
    function DoToolDown({%H-}toolDest: TBGRABitmap; {%H-}pt: TPoint; {%H-}ptF: TPointF; rightBtn: boolean): TRect; override;
    function DoToolMove({%H-}toolDest: TBGRABitmap; {%H-}pt: TPoint; {%H-}ptF: TPointF): TRect; override;
    function DoToolUpdate({%H-}toolDest: TBGRABitmap): TRect; override;
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
    function GetOriginalTransform: TAffineMatrix;
    function FixLayerOffset: boolean; override;
    function GetCurrentSplineMode: TToolSplineMode;
    procedure SetCurrentSplineMode(AMode: TToolSplineMode);
    function IsGradientShape(AShape: TVectorShape): boolean;
    function ConvertToSpline: boolean;
    function GetEditMode: TEditShapeMode;
    function InvalidEditMode: boolean;
  public
    constructor Create(AManager: TToolManager); override;
    destructor Destroy; override;
    function GetContextualToolbars: TContextualToolbars; override;
    function Render(VirtualScreen: TBGRABitmap; VirtualScreenWidth, VirtualScreenHeight: integer; BitmapToVirtualScreen: TBitmapToVirtualScreenFunction): TRect; override;
    function ToolKeyDown(var key: Word): TRect; override;
    function ToolKeyPress(var key: TUTF8Char): TRect; override;
    function ToolKeyUp(var key: Word): TRect; override;
    function ToolUp: TRect; override;
    function ToolCommand(ACommand: TToolCommand): boolean; override;
    function ToolProvideCommand(ACommand: TToolCommand): boolean; override;
    property CurrentSplineMode: TToolSplineMode read GetCurrentSplineMode write SetCurrentSplineMode;
  end;

implementation

uses LazPaintType, LCVectorPolyShapes, LCVectorTextShapes, LCVectorialFill, BGRASVGOriginal,
  ULoading, BGRATransform, math, UImageDiff, Controls, BGRAPen, UResourceStrings, ugraph,
  LCScaleDPI, LCVectorClipboard, BGRAGradientScanner;

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
  ps: TPenStyle;
  opt: TShapeOptions;
  zoom: single;
  m: TAffineMatrix;
  doFill, doDraw: Boolean;
begin
  m := Manager.Image.LayerOriginalMatrix[Manager.Image.CurrentLayerIndex];
  zoom := (VectLen(m[1,1],m[2,1])+VectLen(m[1,2],m[2,2]))/2;
  if IsGradientShape(AShape) then
  begin
    Manager.ForeColor := AShape.BackFill.Gradient.StartColor;
    Manager.BackColor := AShape.BackFill.Gradient.EndColor;
    Manager.GradientType:= AShape.BackFill.Gradient.GradientType;
    Manager.GradientColorspace:= AShape.BackFill.Gradient.ColorInterpolation;
    Manager.GradientSine:= AShape.BackFill.Gradient.Repetition = grSine;
    Manager.SetTexture(nil);
    AShape.Usermode := vsuEditBackFill;
  end else
  begin
    if AShape.Usermode in [vsuEditBackFill, vsuEditPenFill] then
      AShape.Usermode := vsuEdit;
    opt := Manager.ShapeOptions;
    if AShape.Fields*[vsfPenFill,vsfBackFill,vsfPenStyle] = [vsfPenFill,vsfBackFill,vsfPenStyle] then
    begin
      if AShape.BackFill.FillType = vftNone then
      begin;
        exclude(opt,toFillShape);
        doFill := false;
      end
      else
      begin
        include(opt,toFillShape);
        doFill := true;
      end;
      ps := BGRAToPenStyle(AShape.PenStyle);
      if (ps = psClear) or (AShape.PenFill.FillType = vftNone) then
      begin
        exclude(opt,toDrawShape);
        doDraw := false;
      end
      else
      begin
        include(opt,toDrawShape);
        Manager.PenStyle := ps;
        doDraw := true;
      end;
    end else
    begin
      doDraw := vsfPenFill in AShape.Fields;
      doFill := vsfBackFill in AShape.Fields;
    end;

    if doDraw then
    begin
      case AShape.PenFill.FillType of
        vftSolid: Manager.ForeColor := AShape.PenFill.SolidColor;
        vftNone: Manager.ForeColor := BGRA(Manager.ForeColor.red,
          Manager.ForeColor.green,Manager.ForeColor.blue,0);
      end;
    end;
    if doFill then
    begin
      case AShape.BackFill.FillType of
        vftSolid: Manager.BackColor := AShape.BackFill.SolidColor;
        vftNone: Manager.BackColor := BGRA(Manager.BackColor.red,
          Manager.BackColor.green,Manager.BackColor.blue,0);
      end;
    end;
    if doFill and (AShape.BackFill.FillType = vftTexture) then
      Manager.SetTexture(AShape.BackFill.Texture,AShape.BackFill.TextureOpacity)
    else if doDraw and (AShape.PenFill.FillType = vftTexture) then
      Manager.SetTexture(AShape.PenFill.Texture,AShape.PenFill.TextureOpacity)
    else
      Manager.SetTexture(nil);

    if toDrawShape in opt then
    begin
      if vsfPenWidth in AShape.Fields then Manager.PenWidth := AShape.PenWidth*zoom;
      if vsfJoinStyle in AShape.Fields then Manager.JoinStyle:= AShape.JoinStyle;
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
    Manager.ShapeOptions := opt;
  end;

  if AShape is TTextShape then
  with TTextShape(AShape) do
  begin
    Manager.TextPhong := PenPhong;
    Manager.LightPosition := m*LightPosition;
    Manager.PhongShapeAltitude := round(AltitudePercent);
    Manager.ToolTextAlign:= ParagraphAlignment;
    Manager.SetTextFont(FontName,round(FontEmHeight*zoom*72/Manager.Image.DPI),FontStyle);
    Manager.TextShadow:= false;
    if OutlineFill.FillType = vftNone then
      Manager.SetTextOutline(false, Manager.TextOutlineWidth) else
    begin
      Manager.SetTextOutline(true, OutlineWidth);
      if OutlineFill.FillType = vftSolid then
        Manager.BackColor := OutlineFill.SolidColor;
    end;
  end;

  if AShape is TPhongShape then
  with TPhongShape(AShape) do
  begin
    Manager.PhongShapeKind:= ShapeKind;
    Manager.LightPosition:= LightPosition;
    Manager.PhongShapeAltitude:= round(ShapeAltitudePercent);
    Manager.PhongShapeBorderSize:= round(BorderSizePercent);
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
  Manager.Image.DraftOriginal := true;
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
    FRectEditor.MouseDown(rightBtn, FShiftState, ptView.X,ptView.Y, cur, handled);
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
      Manager.Image.CurrentState.LayeredBitmap.MouseDown(rightBtn, FShiftState, ptF.X,ptF.Y, cur, handled);
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
  case GetEditMode of
  esmGradient, esmShape, esmNoShape:
    begin
      BindOriginalEvent(true);
      try
        UpdateSnap(Manager.Image.CurrentState.LayeredBitmap.OriginalEditor);
        Manager.Image.CurrentState.LayeredBitmap.MouseMove(FShiftState, ptF.X,ptF.Y, cur, handled);
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
      FRectEditor.MouseMove(FShiftState, ptView.X,ptView.Y, cur, handled);
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
begin
  case GetEditMode of
  esmShape:
    with GetVectorOriginal do
    try
      BindOriginalEvent(true);
      m := AffineMatrixInverse(Manager.Image.LayerOriginalMatrix[Manager.Image.CurrentLayerIndex]);
      zoom := (VectLen(m[1,1],m[2,1])+VectLen(m[1,2],m[2,2]))/2;
      if IsGradientShape(SelectedShape) then
      begin
        SelectedShape.BackFill.Gradient.StartColor := Manager.ForeColor;
        SelectedShape.BackFill.Gradient.EndColor := Manager.BackColor;
        SelectedShape.BackFill.Gradient.GradientType := Manager.GradientType;
        SelectedShape.BackFill.Gradient.ColorInterpolation := Manager.GradientColorspace;
        if (SelectedShape.BackFill.Gradient.Repetition in[grSine,grPad]) or
          Manager.GradientSine then
        begin
          if Manager.GradientSine then
            SelectedShape.BackFill.Gradient.Repetition := grSine
          else
            SelectedShape.BackFill.Gradient.Repetition := grPad;
        end;
      end else
      begin
        if SelectedShape.Fields*[vsfPenFill,vsfBackFill,vsfPenStyle] = [vsfPenFill,vsfBackFill,vsfPenStyle] then
        begin
          doDraw := toDrawShape in Manager.ShapeOptions;
          doFill := toFillShape in Manager.ShapeOptions;

          if doDraw then
            SelectedShape.PenStyle := PenStyleToBGRA(Manager.PenStyle)
          else
            SelectedShape.PenStyle := ClearPenStyle;

          if vsfPenWidth in SelectedShape.Fields then SelectedShape.PenWidth := Manager.PenWidth*zoom;
          if vsfJoinStyle in SelectedShape.Fields then SelectedShape.JoinStyle := Manager.JoinStyle;
          if SelectedShape is TCustomPolypointShape then
          begin
            TCustomPolypointShape(SelectedShape).Closed := toCloseShape in Manager.ShapeOptions;
            if not TCustomPolypointShape(SelectedShape).Closed then
            begin
              TCustomPolypointShape(SelectedShape).LineCap:= Manager.LineCap;
              TCustomPolypointShape(SelectedShape).ArrowSize:= Manager.ArrowSize;
              TCustomPolypointShape(SelectedShape).ArrowStartKind:= Manager.ArrowStart;
              TCustomPolypointShape(SelectedShape).ArrowEndKind:= Manager.ArrowEnd;
            end;
          end;
          if SelectedShape is TCurveShape then
            TCurveShape(SelectedShape).SplineStyle:= Manager.SplineStyle;
        end else
        begin
          doDraw := vsfPenFill in SelectedShape.Fields;
          doFill := vsfBackFill in SelectedShape.Fields;
        end;
        if doFill then
        begin
          if Assigned(Manager.GetTexture) then
          begin
            if SelectedShape.BackFill.FillType = vftTexture then
            begin
              SelectedShape.BackFill.SetTexture(Manager.GetTexture,
                SelectedShape.BackFill.TextureMatrix,Manager.TextureOpacity,
                SelectedShape.BackFill.TextureRepetition);
            end else
            if SelectedShape.BackFill.FillType <> vftGradient then
              SelectedShape.BackFill.SetTexture(Manager.GetTexture, AffineMatrixIdentity,
                    Manager.TextureOpacity);
          end else
          begin
            if SelectedShape.BackFill.FillType <> vftGradient then
              SelectedShape.BackFill.SetSolid(Manager.BackColor);
          end;
        end else
          if vsfBackFill in SelectedShape.Fields then
            SelectedShape.BackFill.Clear;
        if doDraw then
        begin
          if Assigned(Manager.GetTexture) and not doFill then
          begin
            if SelectedShape.PenFill.FillType = vftTexture then
            begin
              SelectedShape.PenFill.SetTexture(Manager.GetTexture,
                SelectedShape.PenFill.TextureMatrix,Manager.TextureOpacity,
                SelectedShape.PenFill.TextureRepetition);
            end else
            if SelectedShape.PenFill.FillType <> vftGradient then
              SelectedShape.PenFill.SetTexture(Manager.GetTexture, AffineMatrixIdentity,
                    Manager.TextureOpacity);
          end else
          begin
            if SelectedShape.PenFill.FillType <> vftGradient then
              SelectedShape.PenFill.SetSolid(Manager.ForeColor);
          end;
        end;
        if SelectedShape is TTextShape then
        with TTextShape(SelectedShape) do
        begin
          PenPhong := Manager.TextPhong;
          LightPosition := m*Manager.LightPosition;
          AltitudePercent := Manager.PhongShapeAltitude;
          ParagraphAlignment := Manager.ToolTextAlign;
          FontName:= Manager.TextFontName;
          FontEmHeight:= Manager.TextFontSize*zoom*Manager.Image.DPI/72;
          FontStyle := Manager.TextFontStyle;
          if Manager.TextOutline then
          begin
            OutlineWidth := Manager.TextOutlineWidth;
            if OutlineFill.FillType in[vftNone,vftSolid] then
              OutlineFill.SetSolid(Manager.BackColor);
          end else
            OutlineFill.Clear;
        end;
        if SelectedShape is TPhongShape then
        with TPhongShape(SelectedShape) do
        begin
          ShapeKind := Manager.PhongShapeKind;
          LightPosition := Manager.LightPosition;
          ShapeAltitudePercent := Manager.PhongShapeAltitude;
          BorderSizePercent := Manager.PhongShapeBorderSize;
        end;
      end;
    finally
      BindOriginalEvent(false);
    end;
  esmGradient:
    try
      BindOriginalEvent(true);
      with GetGradientOriginal do
      begin
        StartColor := Manager.ForeColor;
        EndColor := Manager.BackColor;
        GradientType:= Manager.GradientType;
        if (Repetition in [grSine,grPad]) or Manager.GradientSine then
        begin
          if Manager.GradientSine then
            Repetition := grSine
          else
            Repetition := grPad;
        end;
        ColorInterpolation := Manager.GradientColorspace;
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
    AEditor.GridActive := ssCtrl in FShiftState;
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
  esmShape: GetVectorOriginal.DeselectShape;
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
      FSelectionRectUntransformed := rectF(Left,Top,Right,Bottom);
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
  StopEdit(False, False);
  inherited Destroy;
end;

function TEditShapeTool.GetContextualToolbars: TContextualToolbars;
var
  shape: TVectorShape;
begin
  Result:= [ctColor,ctTexture];
  case GetEditMode of
  esmShape:
    begin
      shape := GetVectorOriginal.SelectedShape;
      if shape is TRectShape then result := result + [ctShape,ctPenWidth,ctPenStyle,ctJoinStyle]
      else if shape is TEllipseShape then result := result + [ctShape,ctPenWidth,ctPenStyle]
      else if shape is TCurveShape then result := result + [ctShape,ctPenWidth,ctPenStyle,ctLineCap,ctSplineStyle]
      else if shape is TPolylineShape then result := result + [ctShape,ctPenWidth,ctPenStyle,ctJoinStyle,ctLineCap]
      else if shape is TPhongShape then result := result + [ctPhong,ctAltitude]
      else if shape is TTextShape then
      begin
        result := result + [ctText];
        if TTextShape(shape).PenPhong then include(result, ctAltitude);
      end;
      if IsGradientShape(shape) then
        result := result - [ctShape,ctTexture,ctPenWidth,ctPenStyle,ctJoinStyle,ctLineCap] + [ctGradient];
    end;
  esmGradient: result := [ctColor,ctGradient];
  end;
end;

function TEditShapeTool.Render(VirtualScreen: TBGRABitmap; VirtualScreenWidth,
  VirtualScreenHeight: integer;
  BitmapToVirtualScreen: TBitmapToVirtualScreenFunction): TRect;
var
  orig, xAxis, yAxis: TPointF;
  viewMatrix: TAffineMatrix;
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
          Manager.Image.CurrentLayerIndex, viewMatrix, DoScaleX(PointSize,OriginalDPI))
      else
        result := Manager.Image.CurrentState.LayeredBitmap.GetEditorBounds(
          rect(0,0,VirtualScreenWidth,VirtualScreenHeight),
          Manager.Image.CurrentLayerIndex, viewMatrix, DoScaleX(PointSize,OriginalDPI));
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
        FRectEditor.PointSize := DoScaleX(PointSize,OriginalDPI);
      end;
      FRectEditor.Clear;
      FRectEditor.Matrix := viewMatrix;
      if Assigned(FOriginalRect) then FOriginalRect.ConfigureEditor(FRectEditor);
      if Assigned(FSelectionRect) then FSelectionRect.ConfigureEditor(FRectEditor);
      if Assigned(VirtualScreen) then
        result := FRectEditor.Render(VirtualScreen,
          rect(0,0,VirtualScreenWidth,VirtualScreenHeight))
      else
        result := FRectEditor.GetRenderBounds(
          rect(0,0,VirtualScreenWidth,VirtualScreenHeight));
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

function TEditShapeTool.IsGradientShape(AShape: TVectorShape): boolean;
begin
  result := (vsfBackFill in AShape.Fields) and (AShape.BackFill.FillType = vftGradient) and
        (not (vsfPenFill in AShape.Fields) or (AShape.PenFill.FillType = vftNone)) and
        (not (vsfOutlineFill in AShape.Fields) or (AShape.OutlineFill.FillType = vftNone));
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
    orig.SelectShape(shapeAfter);
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

constructor TEditShapeTool.Create(AManager: TToolManager);
begin
  inherited Create(AManager);
  FRectEditor := nil;
  FSelectionRect := nil;
  FOriginalRect := nil;
  FIsEditingGradient:= false;
  FLeftButton:= false;
  FRightButton:= false;
  if not Manager.Image.SelectionMaskEmpty then
  begin
    if (GetCurrentLayerKind = lkVectorial) and Assigned(GetVectorOriginal.SelectedShape) then
      GetVectorOriginal.DeselectShape;
    DoEditSelection;
  end else
  if (GetCurrentLayerKind = lkVectorial) and Assigned(GetVectorOriginal.SelectedShape) then
    UpdateToolManagerFromShape(GetVectorOriginal.SelectedShape);
end;

function TEditShapeTool.ToolKeyDown(var key: Word): TRect;
var
  handled: boolean;
  keyUtf8: TUTF8Char;
begin
  Result:= EmptyRect;
  if (Key = VK_SNAP) or (Key = VK_SNAP2) then Key := VK_CONTROL;
  if key = VK_SHIFT then
  begin
    include(FShiftState, ssShift);
    key := 0;
  end else
  if key = VK_CONTROL then
  begin
    include(FShiftState, ssCtrl);
    key := 0;
  end else
  if key = VK_MENU then
  begin
    include(FShiftState, ssAlt);
    key := 0;
  end else
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
        Manager.Image.CurrentState.LayeredBitmap.KeyDown(FShiftState, LCLKeyToSpecialKey(key, FShiftState), handled);
        if handled then key := 0 else
        begin
          if (key = VK_DELETE) and Assigned(GetVectorOriginal.SelectedShape) then
          begin
            GetVectorOriginal.RemoveShape(GetVectorOriginal.SelectedShape);
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

function TEditShapeTool.ToolKeyUp(var key: Word): TRect;
var
  handled: boolean;
begin
  Result:= EmptyRect;
  if (Key = VK_SNAP) or (Key = VK_SNAP2) then Key := VK_CONTROL;
  if key = VK_SHIFT then
  begin
    exclude(FShiftState, ssShift);
    key := 0;
  end else
  if key = VK_CONTROL then
  begin
    exclude(FShiftState, ssCtrl);
    key := 0;
  end else
  if key = VK_MENU then
  begin
    exclude(FShiftState, ssAlt);
    key := 0;
  end else
  begin
    if GetEditMode in [esmShape,esmNoShape] then
    begin
      BindOriginalEvent(true);
      try
        Manager.Image.CurrentState.LayeredBitmap.KeyUp(FShiftState, LCLKeyToSpecialKey(key, FShiftState), handled);
        if handled then key := 0;
      finally
        BindOriginalEvent(false);
      end;
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
    Manager.Image.DraftOriginal := false;
    handled := false;
    if not handled and FRectEditorCapture and Assigned(FRectEditor) then
    begin
      ptView := FRectEditor.Matrix*FLastPos;
      UpdateSnap(FRectEditor);
      FRectEditor.MouseUp(FRightButton, FShiftState, ptView.X,ptView.Y, cur, handled);
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
        Manager.Image.CurrentState.LayeredBitmap.MouseUp(FRightButton, FShiftState, FLastPos.X,FLastPos.Y, cur, handled);
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
        begin
          m := AffineMatrixInverse(Manager.Image.LayerOriginalMatrix[Manager.Image.CurrentLayerIndex]);
          zoom := (VectLen(m[1,1],m[2,1])+VectLen(m[1,2],m[2,2]))/2/Manager.Image.ZoomFactor;
          BindOriginalEvent(true);
          try
            GetVectorOriginal.MouseClick(m*FLastPos, DoScaleX(PointSize, OriginalDPI)*zoom);
          finally
            BindOriginalEvent(false);
          end;
          if Assigned(GetVectorOriginal.SelectedShape) then handled := true;
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
            FOriginalRectUntransformed := rectF(Left,Top,Right,Bottom);
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
        with GetGradientOriginal do
        begin
          Manager.ForeColor := StartColor;
          Manager.BackColor := EndColor;
          Manager.GradientType := GradientType;
          Manager.GradientSine := (Repetition = grSine);
          Manager.GradientColorspace := ColorInterpolation;
        end;
        Manager.UpdateContextualToolbars;
        handled := true;
        result := OnlyRenderChange;
      end;
    end;
    FLeftButton:= false;
    FRightButton:= false;
  end;
end;

function TEditShapeTool.ToolCommand(ACommand: TToolCommand): boolean;
var
  key: Word;
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
        PasteShapesFromClipboard(GetVectorOriginal, GetOriginalTransform);
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
          tcShapeToSpline: result := ConvertToSpline;
          else result := false;
        end;
      finally
        BindOriginalEvent(false);
      end;
    esmOtherOriginal:
      begin
        case ACommand of
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
  tcCut,tcCopy,tcDelete: result:= GetEditMode = esmShape;
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
  AHandled := true;
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
      FreeAndNil(FShape);
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
    ShapeValidated;
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
    if Manager.ShapeOptionDraw then
    begin
      if (not (vsfBackFill in f) or not Manager.ShapeOptionFill) and (Manager.GetTexture <> nil) then
        FShape.PenFill.SetTexture(Manager.GetTexture,AMatrix,Manager.TextureOpacity)
      else
      begin
        if FSwapColor then
          FShape.PenFill.SetSolid(Manager.BackColor)
        else
          FShape.PenFill.SetSolid(Manager.ForeColor);
      end;
    end else
      FShape.PenFill.Clear;
  end;
  if vsfPenWidth in f then FShape.PenWidth := zoom*Manager.PenWidth;
  if vsfPenStyle in f Then FShape.PenStyle := PenStyleToBGRA(Manager.PenStyle);
  if vsfJoinStyle in f then FShape.JoinStyle:= Manager.JoinStyle;
  if vsfBackFill in f then
  begin
    if Manager.ShapeOptionFill then
    begin
      if Manager.GetTexture <> nil then
        FShape.BackFill.SetTexture(Manager.GetTexture,AffineMatrixIdentity,Manager.TextureOpacity)
      else
      begin
        if FSwapColor then
          FShape.BackFill.SetSolid(Manager.ForeColor)
        else
          FShape.BackFill.SetSolid(Manager.BackColor);
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
  if not Manager.ShapeOptionDraw or
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
  matrix := VectorTransform(false);
  draft := (FRightDown or FLeftDown) and SlowShape;
  newBounds := GetCustomShapeBounds(toolDest.ClipRect,matrix,draft);
  result := RectUnion(result, newBounds);
  oldClip := toolDest.IntersectClip(newBounds);
  DrawCustomShape(toolDest,matrix,draft);
  toolDest.ClipRect := oldClip;
  FPreviousUpdateBounds := newBounds;
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
    viewPt := FEditor.Matrix*AffineMatrixInverse(VectorTransform(true))*FLastPos;
    FEditor.MouseDown(rightBtn, FShiftState, viewPt.X,viewPt.Y, cur, handled);
    if not handled and Assigned(FShape) then
    begin
      shapePt := AffineMatrixInverse(VectorTransform(true))*FLastPos;
      FShape.MouseDown(rightBtn, FShiftState, shapePt.X,shapePt.Y, cur, handled);
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
    if GetCurrentLayerKind = lkSVG then
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
      FQuickDefine := true;
      FQuickDefineStartPoint := RoundCoordinate(FLastPos);
      FQuickDefineEndPoint := FQuickDefineStartPoint;
      FShape.BeginUpdate;
        QuickDefineShape(FQuickDefineStartPoint,FQuickDefineEndPoint);
        FLastShapeTransform := AffineMatrixInverse(VectorTransform(false));
        FShape.Transform(FLastShapeTransform);
        shapePt := AffineMatrixInverse(VectorTransform(true))*FLastPos;
        handled := false;
        FShape.MouseMove(FShiftState, shapePt.X,shapePt.Y, cur, handled);
        AssignShapeStyle(FLastShapeTransform);
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
  with LayerOffset do
    FLastPos := AffineMatrixTranslation(X,Y)*ptF;
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
      FLastShapeTransform := AffineMatrixInverse(VectorTransform(false));
      FShape.Transform(FLastShapeTransform);
      AssignShapeStyle(FLastShapeTransform);
    FShape.EndUpdate;
    result := OnlyRenderChange;
  end else
  begin
    viewPt := FEditor.Matrix*AffineMatrixInverse(VectorTransform(true))*FLastPos;
    FEditor.MouseMove(FShiftState, viewPt.X,viewPt.Y, cur, handled);
    if not handled and Assigned(FShape) then
    begin
      shapePt := AffineMatrixInverse(VectorTransform(true))*FLastPos;
      FShape.MouseMove(FShiftState, shapePt.X,shapePt.Y, cur, handled);
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
    viewPt := FEditor.Matrix*AffineMatrixInverse(VectorTransform(true))*FLastPos;
    FEditor.MouseUp(wasRight, FShiftState, viewPt.X,viewPt.Y, cur, handled);
    if not handled and Assigned(FShape) then
    begin
      shapePt := AffineMatrixInverse(VectorTransform(true))*FLastPos;
      FShape.MouseUp(wasRight, FShiftState, shapePt.X,shapePt.Y, cur, handled);
    end;
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
  if (Key = VK_SNAP) or (Key = VK_SNAP2) then Key := VK_CONTROL;
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
  if Key = VK_CONTROL then
  begin
    Include(FShiftState, ssCtrl);
    if not FQuickDefine then FEditor.GridActive := true;
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
  if (Key = VK_SNAP) or (Key = VK_SNAP2) then Key := VK_CONTROL;
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
  if Key = VK_CONTROL then
  begin
    Exclude(FShiftState, ssCtrl);
    if not FQuickDefine then FEditor.GridActive := false;
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
  tcShapeToSpline: result:= not IsSelectingTool and not FQuickDefine and Assigned(FShape)
                            and TCurveShape.CanCreateFrom(FShape);
  tcAlignLeft..tcAlignBottom: Result:= not FQuickDefine and Assigned(FShape);
  tcMoveDown,tcMoveToBack: result := not IsSelectingTool and not FQuickDefine and Assigned(FShape)
          and not AlwaysRasterizeShape and Manager.Image.SelectionMaskEmpty and not FLayerWasEmpty;
  else result := false;
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
    with LayerOffset do
    begin
      orig := BitmapToVirtualScreen(PointF(0,0));
      xAxis := BitmapToVirtualScreen(PointF(1,0));
      yAxis := BitmapToVirtualScreen(PointF(0,1));
    end;
    FEditor.Matrix := AffineMatrix(xAxis-orig,yAxis-orig,orig)*VectorTransform(true);
    FEditor.Clear;
    FEditor.PointSize := DoScaleX(PointSize, OriginalDPI);
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

initialization

  RegisterTool(ptEditShape, TEditShapeTool);

end.

