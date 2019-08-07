unit UToolVectorial;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, BGRABitmap, BGRABitmapTypes,
  BGRALayerOriginal, BGRAGraphics, LCVectorOriginal,
  UTool, UImageType, ULayerAction, LCVectorRectShapes,
  BGRAGradientOriginal;

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
    function GetShapesCost(AOriginal: TVectorOriginal): integer;
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
    function ToolCopy: boolean; override;
    function ToolCut: boolean; override;
    function ToolProvideCopy: boolean; override;
    function ToolProvideCut: boolean; override;
    function Render(VirtualScreen: TBGRABitmap; {%H-}VirtualScreenWidth, {%H-}VirtualScreenHeight: integer; BitmapToVirtualScreen: TBitmapToVirtualScreenFunction):TRect; override;
    property IsIdle: boolean read GetIsIdle;
    property IsHandDrawing: boolean read GetIsHandDrawing;
    destructor Destroy; override;
  end;

  { TEditShapeTool }

  TEditShapeTool = class(TGenericTool)
  protected
    FShiftState: TShiftState;
    FDownHandled,FLeftButton,FRightButton: boolean;
    FLastPos: TPointF;
    FOriginalRect: TRectShape;
    FOriginalRectUntransformed: TRectF;
    FOriginalRectEditor: TBGRAOriginalEditor;
    FIsEditingGradient: boolean;
    procedure RetrieveLightPosition;
    procedure UpdateToolManagerFromShape(AShape: TVectorShape);
    procedure BindOriginalEvent(ABind: boolean);
    procedure SelectShape({%H-}ASender: TObject; AShape: TVectorShape; {%H-}APreviousShape: TVectorShape);
    function DoToolDown({%H-}toolDest: TBGRABitmap; {%H-}pt: TPoint; {%H-}ptF: TPointF; rightBtn: boolean): TRect; override;
    function DoToolMove({%H-}toolDest: TBGRABitmap; {%H-}pt: TPoint; {%H-}ptF: TPointF): TRect; override;
    function DoToolUpdate({%H-}toolDest: TBGRABitmap): TRect; override;
    function GetAction: TLayerAction; override;
    function DoGetToolDrawingLayer: TBGRABitmap; override;
    procedure OnTryStop({%H-}sender: TCustomLayerAction); override;
    procedure StopEdit(AUpdateToolbars: boolean);
    function IsVectorOriginal: boolean;
    function IsGradientOriginal: boolean;
    function IsOtherOriginal: boolean;
    function IsBitmap: boolean;
    procedure MakeImageOriginal;
    procedure UpdateOriginalMatrixFromRect;
    function GetIsSelectingTool: boolean; override;
    function GetVectorOriginal: TVectorOriginal;
    function GetGradientOriginal: TBGRALayerGradientOriginal;
    function FixLayerOffset: boolean; override;
    function GetCurrentSplineMode: TToolSplineMode;
    procedure SetCurrentSplineMode(AMode: TToolSplineMode);
    function IsGradientShape(AShape: TVectorShape): boolean;
  public
    constructor Create(AManager: TToolManager); override;
    destructor Destroy; override;
    function GetContextualToolbars: TContextualToolbars; override;
    function Render(VirtualScreen: TBGRABitmap; VirtualScreenWidth, VirtualScreenHeight: integer; BitmapToVirtualScreen: TBitmapToVirtualScreenFunction): TRect; override;
    function ToolKeyDown(var key: Word): TRect; override;
    function ToolKeyPress(var key: TUTF8Char): TRect; override;
    function ToolKeyUp(var key: Word): TRect; override;
    function ToolUp: TRect; override;
    function ToolCopy: boolean; override;
    function ToolCut: boolean; override;
    function ToolPaste: boolean; override;
    function ToolProvideCopy: boolean; override;
    function ToolProvideCut: boolean; override;
    function ToolProvidePaste: boolean; override;
    property CurrentSplineMode: TToolSplineMode read GetCurrentSplineMode write SetCurrentSplineMode;
  end;

implementation

uses LazPaintType, LCVectorPolyShapes, LCVectorTextShapes, LCVectorialFill, BGRASVGOriginal,
  ULoading, BGRATransform, math, UStateType, UImageDiff, Controls, BGRAPen, UResourceStrings, ugraph,
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

{ TEditShapeTool }

procedure TEditShapeTool.SelectShape(ASender: TObject; AShape: TVectorShape;
  APreviousShape: TVectorShape);
begin
  if Assigned(AShape) and IsVectorOriginal then
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
  if IsVectorOriginal then
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
  if IsVectorOriginal then
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
  end else
  if IsGradientOriginal then
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

function TEditShapeTool.DoToolDown(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF; rightBtn: boolean): TRect;
var
  cur: TOriginalEditorCursor;
  handled: boolean;
  box: TAffineBox;
  ptView: TPointF;
begin
  Result:= EmptyRect;
  Manager.Image.DraftOriginal := true;
  FRightButton:= rightBtn;
  FLeftButton:= not rightBtn;
  FLastPos := ptF;
  handled := false;
  if IsVectorOriginal or IsGradientOriginal then
  begin
    if IsGradientOriginal and not FIsEditingGradient then
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
      result := OnlyRenderChange;
    end else
    begin
      BindOriginalEvent(true);
      Manager.Image.CurrentState.LayeredBitmap.MouseDown(rightBtn, FShiftState, ptF.X,ptF.Y, cur, handled);
      BindOriginalEvent(false);
      if handled then Cursor := OriginalCursorToCursor(cur);
    end;
  end else
  begin
    if (FOriginalRect=nil) and
       (toolDest.GetPixel(ptF.X-LayerOffset.X, ptF.Y-LayerOffset.Y).alpha <> 0) then
    begin
      if IsBitmap then MakeImageOriginal;
      if IsOtherOriginal then
      begin
        with Manager.Image.LayerOriginal[Manager.Image.CurrentLayerIndex].
          GetRenderBounds(InfiniteRect, AffineMatrixIdentity) do
          FOriginalRectUntransformed := rectF(Left,Top,Right,Bottom);
        if (FOriginalRectUntransformed.Width > 0) and (FOriginalRectUntransformed.Height > 0) then
        begin
          FOriginalRect := TRectShape.Create(nil);
          FOriginalRect.PenStyle := ClearPenStyle;
          FOriginalRect.BackFill.SetSolid(BGRAWhite);
          box := Manager.Image.LayerOriginalMatrix[Manager.Image.CurrentLayerIndex]
                *TAffineBox.AffineBox(FOriginalRectUntransformed);
          FOriginalRect.Origin := (box.TopLeft+box.BottomRight)*0.5;
          FOriginalRect.XAxis := FOriginalRect.Origin+(box.TopRight-box.TopLeft)*0.5;
          FOriginalRect.YAxis := FOriginalRect.Origin+(box.BottomLeft-box.TopLeft)*0.5;
          Manager.UpdateContextualToolbars;
        end;
        result := OnlyRenderChange;
      end;
    end else
    begin
      if Assigned(FOriginalRectEditor) then
      begin
        ptView := FOriginalRectEditor.Matrix*ptF;
        FOriginalRectEditor.MouseDown(rightBtn, FShiftState, ptView.X,ptView.Y, cur, handled);
        Cursor := OriginalCursorToCursor(cur);
        if handled then
        begin
          result := OnlyRenderChange;
          UpdateOriginalMatrixFromRect;
        end else
          StopEdit(true);
      end;
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
  if IsVectorOriginal or (IsGradientOriginal and FIsEditingGradient) then
  begin
    BindOriginalEvent(true);
    Manager.Image.CurrentState.LayeredBitmap.MouseMove(FShiftState, ptF.X,ptF.Y, cur, handled);
    BindOriginalEvent(false);
    Cursor := OriginalCursorToCursor(cur);
  end else
  if Assigned(FOriginalRectEditor) then
  begin
    ptView := FOriginalRectEditor.Matrix*ptF;
    FOriginalRectEditor.MouseMove(FShiftState, ptView.X,ptView.Y, cur, handled);
    Cursor := OriginalCursorToCursor(cur);
    if handled then
    begin
      result := OnlyRenderChange;
      UpdateOriginalMatrixFromRect;
    end;
  end;
end;

function TEditShapeTool.DoToolUpdate(toolDest: TBGRABitmap): TRect;
var
  doDraw, doFill: Boolean;
  m: TAffineMatrix;
  zoom: Single;
begin
  if IsVectorOriginal then
  with GetVectorOriginal do
  begin
    m := AffineMatrixInverse(Manager.Image.LayerOriginalMatrix[Manager.Image.CurrentLayerIndex]);
    zoom := (VectLen(m[1,1],m[2,1])+VectLen(m[1,2],m[2,2]))/2;
    BindOriginalEvent(true);
    if Assigned(SelectedShape) then
    begin
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
    end;
    BindOriginalEvent(false);
  end else
  if IsGradientOriginal then
  begin
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
    BindOriginalEvent(false);
  end;
  Result := EmptyRect;
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
  StopEdit(True);
  inherited OnTryStop(sender);
end;

procedure TEditShapeTool.StopEdit(AUpdateToolbars: boolean);
var
  r: TRect;
begin
  if FLeftButton or FRightButton then
  begin
    r := ToolUp;
    Manager.Image.LayerMayChange(GetToolDrawingLayer,r);
  end;
  if IsVectorOriginal then GetVectorOriginal.DeselectShape;
  Manager.Image.CurrentState.LayeredBitmap.ClearEditor;
  FIsEditingGradient:= false;
  FreeAndNil(FOriginalRect);
  FreeAndNil(FOriginalRectEditor);
  Cursor := crDefault;
  Manager.Image.OnImageChanged.NotifyObservers;
  if AUpdateToolbars then Manager.UpdateContextualToolbars;
end;

function TEditShapeTool.IsVectorOriginal: boolean;
begin
  result := Manager.Image.LayerOriginalClass[Manager.Image.CurrentLayerIndex]=TVectorOriginal;
end;

function TEditShapeTool.IsGradientOriginal: boolean;
begin
  result := Manager.Image.LayerOriginalClass[Manager.Image.CurrentLayerIndex]=TBGRALayerGradientOriginal;
end;

function TEditShapeTool.IsOtherOriginal: boolean;
begin
  result := Manager.Image.LayerOriginalKnown[Manager.Image.CurrentLayerIndex] and
     not IsVectorOriginal and not IsGradientOriginal;
end;

function TEditShapeTool.IsBitmap: boolean;
begin
  result := (not Manager.Image.LayerOriginalDefined[Manager.Image.CurrentLayerIndex]) and
             not Manager.Image.CurrentLayerEmpty;
end;

procedure TEditShapeTool.MakeImageOriginal;
var
  diff: TReplaceLayerByImageOriginalDifference;
begin
  if IsBitmap then
  begin
    diff := TReplaceLayerByImageOriginalDifference.Create(Manager.Image.CurrentState,
              Manager.Image.CurrentLayerIndex, false);
    Manager.Image.AddUndo(diff);
    if Assigned(Manager.Image.OnStackChanged) then
      Manager.Image.OnStackChanged(Manager.Image, False);
  end;
end;

procedure TEditShapeTool.UpdateOriginalMatrixFromRect;
var
  u, v: TPointF;
  mAfter, mBefore: TAffineMatrix;
begin
  if Assigned(FOriginalRect) then
  begin
    u := (FOriginalRect.XAxis-FOriginalRect.Origin)*2;
    v := (FOriginalRect.YAxis-FOriginalRect.Origin)*2;
    mAfter := AffineMatrix(u,v,FOriginalRect.Origin-(u+v)*0.5);
    mBefore := AffineMatrixTranslation(FOriginalRectUntransformed.Left,
                 FOriginalRectUntransformed.Top)
      *AffineMatrixScale(FOriginalRectUntransformed.Width,FOriginalRectUntransformed.Height);
    if IsAffineMatrixInversible(mBefore) then
    begin
      Manager.Image.LayerOriginalMatrix[Manager.Image.CurrentLayerIndex] :=
        mAfter*AffineMatrixInverse(mBefore);
    end;
  end;
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

function TEditShapeTool.FixLayerOffset: boolean;
begin
  Result:= false;
end;

destructor TEditShapeTool.Destroy;
begin
  StopEdit(False);
  inherited Destroy;
end;

function TEditShapeTool.GetContextualToolbars: TContextualToolbars;
var
  shape: TVectorShape;
begin
  Result:= [ctColor,ctTexture];
  if IsVectorOriginal then
    shape := GetVectorOriginal.SelectedShape
  else shape := nil;
  if Assigned(shape) then
  begin
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
  end else
  if IsGradientOriginal and FIsEditingGradient then
    result := [ctColor,ctGradient];
end;

function TEditShapeTool.Render(VirtualScreen: TBGRABitmap; VirtualScreenWidth,
  VirtualScreenHeight: integer;
  BitmapToVirtualScreen: TBitmapToVirtualScreenFunction): TRect;
var
  orig, xAxis, yAxis: TPointF;
  viewMatrix: TAffineMatrix;
begin
  with LayerOffset do
  begin
    orig := BitmapToVirtualScreen(PointF(-X,-Y));
    xAxis := BitmapToVirtualScreen(PointF(-X+1,-Y));
    yAxis := BitmapToVirtualScreen(PointF(-X,-Y+1));
  end;
  viewMatrix := AffineMatrix(xAxis-orig,yAxis-orig,orig);

  if IsVectorOriginal or (IsGradientOriginal and FIsEditingGradient) then
  begin
    if Assigned(VirtualScreen) then
      result := Manager.Image.CurrentState.LayeredBitmap.DrawEditor(VirtualScreen,
        Manager.Image.CurrentLayerIndex, viewMatrix, DoScaleX(PointSize,OriginalDPI))
    else
      result := Manager.Image.CurrentState.LayeredBitmap.GetEditorBounds(
        rect(0,0,VirtualScreenWidth,VirtualScreenHeight),
        Manager.Image.CurrentLayerIndex, viewMatrix, DoScaleX(PointSize,OriginalDPI));
    RetrieveLightPosition;
  end else
  begin
    result := EmptyRect;
    Manager.Image.CurrentState.LayeredBitmap.ClearEditor;
    If Assigned(FOriginalRect) then
    begin
      if not Assigned(FOriginalRectEditor) then
      begin
        FOriginalRectEditor := TVectorOriginalEditor.Create(nil);
        FOriginalRectEditor.GridMatrix := AffineMatrixScale(0.5,0.5);
        FOriginalRectEditor.Focused := true;
        FOriginalRectEditor.PointSize := DoScaleX(PointSize,OriginalDPI);
      end;
      FOriginalRectEditor.Clear;
      FOriginalRectEditor.Matrix := viewMatrix;
      FOriginalRect.ConfigureEditor(FOriginalRectEditor);
      if Assigned(VirtualScreen) then
        result := FOriginalRectEditor.Render(VirtualScreen,
          rect(0,0,VirtualScreenWidth,VirtualScreenHeight))
      else
        result := FOriginalRectEditor.GetRenderBounds(
          rect(0,0,VirtualScreenWidth,VirtualScreenHeight));
    end;
  end;
end;

function TEditShapeTool.GetCurrentSplineMode: TToolSplineMode;
var
  orig: TVectorOriginal;
begin
  if IsVectorOriginal then
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
  if IsVectorOriginal and Assigned(GetVectorOriginal.SelectedShape) and
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

constructor TEditShapeTool.Create(AManager: TToolManager);
begin
  inherited Create(AManager);
  if IsVectorOriginal and Assigned(GetVectorOriginal.SelectedShape) then
    UpdateToolManagerFromShape(GetVectorOriginal.SelectedShape);
end;

function TEditShapeTool.ToolKeyDown(var key: Word): TRect;
var
  handled: boolean;
begin
  Result:= EmptyRect;
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
  begin
    if IsVectorOriginal then
    begin
      BindOriginalEvent(true);
      Manager.Image.CurrentState.LayeredBitmap.KeyDown(FShiftState, LCLKeyToSpecialKey(key, FShiftState), handled);
      if handled then key := 0;
      if not handled and IsVectorOriginal then
      begin
        if (key = VK_DELETE) and Assigned(GetVectorOriginal.SelectedShape) then
        begin
          GetVectorOriginal.RemoveShape(GetVectorOriginal.SelectedShape);
          key := 0;
        end;
      end;
      BindOriginalEvent(false);
    end else
    begin
      if key = VK_RETURN then
      begin
        StopEdit(true);
        key := 0;
      end;
    end;
  end;
end;

function TEditShapeTool.ToolKeyPress(var key: TUTF8Char): TRect;
var
  handled: boolean;
begin
  Result:= EmptyRect;
  if IsVectorOriginal then
  begin
    BindOriginalEvent(true);
    Manager.Image.CurrentState.LayeredBitmap.KeyPress(key, handled);
    if handled then key := #0;
    BindOriginalEvent(false);
  end;
end;

function TEditShapeTool.ToolKeyUp(var key: Word): TRect;
var
  handled: boolean;
begin
  Result:= EmptyRect;
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
    if IsVectorOriginal then
    begin
      BindOriginalEvent(true);
      Manager.Image.CurrentState.LayeredBitmap.KeyUp(FShiftState, LCLKeyToSpecialKey(key, FShiftState), handled);
      if handled then key := 0;
      BindOriginalEvent(false);
    end;
  end;
end;


function TEditShapeTool.ToolUp: TRect;
var
  cur: TOriginalEditorCursor;
  handled: boolean;
  m: TAffineMatrix;
  ptView: TPointF;
begin
  Result:= EmptyRect;
  if FLeftButton or FRightButton then
  begin
    Manager.Image.DraftOriginal := false;
    if IsVectorOriginal or (IsGradientOriginal and FIsEditingGradient) then
    begin
      BindOriginalEvent(true);
      Manager.Image.CurrentState.LayeredBitmap.MouseUp(FRightButton, FShiftState, FLastPos.X,FLastPos.Y, cur, handled);
      if handled then
      begin
        Cursor := OriginalCursorToCursor(cur);
        result := OnlyRenderChange;
      end;
      if not handled and not FDownHandled and IsVectorOriginal then
      begin
        m := AffineMatrixInverse(Manager.Image.LayerOriginalMatrix[Manager.Image.CurrentLayerIndex]);
        GetVectorOriginal.MouseClick(m*FLastPos);
      end;
      BindOriginalEvent(false);
    end else
    begin
      if Assigned(FOriginalRectEditor) then
      begin
        ptView := FOriginalRectEditor.Matrix*FLastPos;
        FOriginalRectEditor.MouseUp(FRightButton, FShiftState, ptView.X,ptView.Y, cur, handled);
        Cursor := OriginalCursorToCursor(cur);
        if handled then
        begin
          result := OnlyRenderChange;
          UpdateOriginalMatrixFromRect;
        end;
      end;
    end;
    FLeftButton:= false;
    FRightButton:= false;
  end;
end;

function TEditShapeTool.ToolCopy: boolean;
begin
  if IsVectorOriginal and Assigned(GetVectorOriginal.SelectedShape) then
    Result:= CopyShapesToClipboard([GetVectorOriginal.SelectedShape])
  else
    result := false;
end;

function TEditShapeTool.ToolCut: boolean;
begin
  if ToolCopy then
  begin
    BindOriginalEvent(true);
    result := GetVectorOriginal.RemoveShape(GetVectorOriginal.SelectedShape);
    BindOriginalEvent(false);
  end
  else
    result := false;
end;

function TEditShapeTool.ToolPaste: boolean;
begin
  if IsVectorOriginal then
  begin
    BindOriginalEvent(true);
    PasteShapesFromClipboard(GetVectorOriginal);
    BindOriginalEvent(false);
    result := true;
  end else
    Result:= false;
end;

function TEditShapeTool.ToolProvideCopy: boolean;
begin
  Result:= IsVectorOriginal and Assigned(GetVectorOriginal.SelectedShape);
end;

function TEditShapeTool.ToolProvideCut: boolean;
begin
  Result:= IsVectorOriginal and Assigned(GetVectorOriginal.SelectedShape);
end;

function TEditShapeTool.ToolProvidePaste: boolean;
begin
  Result:= IsVectorOriginal and ClipboardHasShapes;
end;

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

function TVectorialTool.GetShapesCost(AOriginal: TVectorOriginal): integer;
var
  i: Integer;
begin
  result := 0;
  for i := 0 to AOriginal.ShapeCount-1 do
    if (AOriginal.Shape[i] is TPhongShape) or
       (AOriginal.Shape[i] is TTextShape) then
       inc(result, 5)
    else if ((vsfBackFill in AOriginal.Shape[i].Fields) and
       (AOriginal.Shape[i].BackFill.FillType = vftGradient)) then
       inc(result,20)
    else
      inc(result,2);
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
  with Manager.Image.LayerOffset[Manager.Image.CurrentLayerIndex] do
    FLastPos := AffineMatrixTranslation(X,Y)*ptF;
  if Assigned(FShape) then
  begin
    viewPt := FEditor.Matrix*AffineMatrixInverse(VectorTransform)*FLastPos;
    FEditor.MouseDown(rightBtn, FShiftState, viewPt.X,viewPt.Y, cur, handled);
    if not handled and Assigned(FShape) then
    begin
      shapePt := AffineMatrixInverse(VectorTransform)*FLastPos;
      FShape.MouseDown(rightBtn, FShiftState, shapePt.X,shapePt.Y, cur, handled);
    end;
    UpdateCursor(cur);
    if handled then exit
    else result := RectUnion(result, ValidateShape);
  end;

  if FShape=nil then
  begin
    if UseOriginal and
      (GetShapesCost(Manager.Image.LayerOriginal[Manager.Image.CurrentLayerIndex] as TVectorOriginal) >= 50) then
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
      FQuickDefineStartPoint := RoundCoordinate(FLastPos);
      FQuickDefineEndPoint := FQuickDefineStartPoint;
      FShape.BeginUpdate;
        QuickDefineShape(FQuickDefineStartPoint,FQuickDefineEndPoint);
        FLastShapeTransform := AffineMatrixInverse(VectorTransform);
        FShape.Transform(FLastShapeTransform);
        shapePt := AffineMatrixInverse(VectorTransform)*FLastPos;
        handled := false;
        FShape.MouseMove(FShiftState, shapePt.X,shapePt.Y, cur, handled);
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
  viewPt, shapePt: TPointF;
  handled: boolean;
  cur: TOriginalEditorCursor;
begin
  with Manager.Image.LayerOffset[Manager.Image.CurrentLayerIndex] do
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
      FLastShapeTransform := AffineMatrixInverse(VectorTransform);
      FShape.Transform(FLastShapeTransform);
      AssignShapeStyle(FLastShapeTransform);
    FShape.EndUpdate;
    result := OnlyRenderChange;
  end else
  begin
    viewPt := FEditor.Matrix*AffineMatrixInverse(VectorTransform)*FLastPos;
    FEditor.MouseMove(FShiftState, viewPt.X,viewPt.Y, cur, handled);
    if not handled and Assigned(FShape) then
    begin
      shapePt := AffineMatrixInverse(VectorTransform)*FLastPos;
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
    viewPt := FEditor.Matrix*AffineMatrixInverse(VectorTransform)*FLastPos;
    FEditor.MouseUp(wasRight, FShiftState, viewPt.X,viewPt.Y, cur, handled);
    if not handled and Assigned(FShape) then
    begin
      shapePt := AffineMatrixInverse(VectorTransform)*FLastPos;
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

function TVectorialTool.ToolCopy: boolean;
begin
  if Assigned(FShape) then
    result := CopyShapesToClipboard([FShape])
  else
    Result:= false;
end;

function TVectorialTool.ToolCut: boolean;
var
  r: TRect;
  toolDest: TBGRABitmap;
begin
  if ToolCopy then
  begin
    toolDest := GetToolDrawingLayer;
    r := CancelShape;
    Action.NotifyChange(toolDest, r);
    result := true;
  end else
    result := false;
end;

function TVectorialTool.ToolProvideCopy: boolean;
begin
  Result:= not FQuickDefine and Assigned(FShape);
end;

function TVectorialTool.ToolProvideCut: boolean;
begin
  Result:= not FQuickDefine and Assigned(FShape);
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
    FEditor.Matrix := AffineMatrix(xAxis-orig,yAxis-orig,orig)*VectorTransform;
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

