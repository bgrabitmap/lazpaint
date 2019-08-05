unit UToolVectorial;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, BGRABitmap, BGRABitmapTypes,
  BGRALayerOriginal, LCVectorOriginal,
  UTool, UImageType, ULayerAction, LCVectorRectShapes;

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
  private
    procedure SelectShape({%H-}ASender: TObject; AShape: TVectorShape;
      {%H-}APreviousShape: TVectorShape);
  protected
    FShiftState: TShiftState;
    FLeftButton,FRightButton: boolean;
    FLastPos: TPointF;
    FOriginalRect: TRectShape;
    FOriginalRectUntransformed: TRectF;
    FOriginalRectEditor: TBGRAOriginalEditor;
    procedure BindOriginalEvent(ABind: boolean);
    function DoToolDown({%H-}toolDest: TBGRABitmap; {%H-}pt: TPoint; {%H-}ptF: TPointF; rightBtn: boolean): TRect; override;
    function DoToolMove({%H-}toolDest: TBGRABitmap; {%H-}pt: TPoint; {%H-}ptF: TPointF): TRect; override;
    function DoToolUpdate({%H-}toolDest: TBGRABitmap): TRect; override;
    function GetAction: TLayerAction; override;
    function DoGetToolDrawingLayer: TBGRABitmap; override;
    procedure OnTryStop({%H-}sender: TCustomLayerAction); override;
    procedure StopEdit;
    function IsVectorOriginal: boolean;
    function IsOtherOriginal: boolean;
    function IsBitmap: boolean;
    procedure MakeImageOriginal;
    procedure UpdateOriginalMatrixFromRect;
    function GetIsSelectingTool: boolean; override;
    function GetVectorOriginal: TVectorOriginal;
    function FixLayerOffset: boolean; override;
  public
    constructor Create(AManager: TToolManager); override;
    destructor Destroy; override;
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
  end;

implementation

uses LazPaintType, LCVectorTextShapes, LCVectorialFill, BGRASVGOriginal,
  ULoading, BGRATransform, math, UStateType, UImageDiff, Controls, BGRAPen, UResourceStrings, ugraph,
  LCScaleDPI, LCVectorClipboard;

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

{ TEditShapeTool }

procedure TEditShapeTool.SelectShape(ASender: TObject; AShape: TVectorShape;
  APreviousShape: TVectorShape);
begin
  if Assigned(AShape) then
  begin
    if (vsfBackFill in AShape.Fields) and (AShape.BackFill.FillType = vftGradient) then
    begin
      Manager.ToolForeColor := AShape.BackFill.Gradient.StartColor;
      Manager.ToolBackColor := AShape.BackFill.Gradient.EndColor;
      Manager.SetToolTexture(nil);
    end else
    begin
      if (vsfPenFill in AShape.Fields) and (AShape.PenFill.FillType = vftSolid) then
        Manager.ToolForeColor := AShape.PenFill.SolidColor;
      if (vsfBackFill in AShape.Fields) and (AShape.BackFill.FillType = vftSolid) then
        Manager.ToolBackColor := AShape.BackFill.SolidColor;
      if (vsfBackFill in AShape.Fields) and (AShape.BackFill.FillType = vftTexture) then
        Manager.SetToolTexture(AShape.BackFill.Texture)
      else if (vsfPenFill in AShape.Fields) and (AShape.PenFill.FillType = vftTexture) then
        Manager.SetToolTexture(AShape.PenFill.Texture)
      else
        Manager.SetToolTexture(nil);
    end;
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
  if IsVectorOriginal then
  begin
    BindOriginalEvent(true);
    Manager.Image.CurrentState.LayeredBitmap.MouseDown(rightBtn, FShiftState, ptF.X,ptF.Y, cur, handled);
    BindOriginalEvent(false);
    if handled then Cursor := OriginalCursorToCursor(cur);
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
        end;
      end;
    end;
  end;
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
  if IsVectorOriginal then
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
begin
  if IsVectorOriginal then
  with GetVectorOriginal do
  begin
    BindOriginalEvent(true);
    if Assigned(SelectedShape) then
    begin
      if (vsfBackFill in SelectedShape.Fields) and (SelectedShape.BackFill.FillType = vftGradient) then
      begin
        if Assigned(Manager.GetToolTexture) then
          SelectedShape.BackFill.SetTexture(Manager.GetToolTexture, AffineMatrixIdentity)
        else
        begin
          SelectedShape.BackFill.Gradient.StartColor := Manager.ToolForeColor;
          SelectedShape.BackFill.Gradient.EndColor := Manager.ToolBackColor;
        end;
      end else
      begin
        if Assigned(Manager.GetToolTexture) then
        begin
          case SelectedShape.BackFill.FillType of
          vftTexture:
            begin
              SelectedShape.BackFill.SetTexture(Manager.GetToolTexture,
                SelectedShape.BackFill.TextureMatrix,SelectedShape.BackFill.TextureOpacity,
                SelectedShape.BackFill.TextureRepetition);
              if (vsfPenFill in SelectedShape.Fields) and (SelectedShape.PenFill.FillType = vftSolid) then
                SelectedShape.PenFill.SolidColor := Manager.ToolForeColor;
            end;
          vftGradient,vftSolid:
            begin
              SelectedShape.BackFill.SetTexture(Manager.GetToolTexture, AffineMatrixIdentity);
              if (vsfPenFill in SelectedShape.Fields) and (SelectedShape.PenFill.FillType = vftSolid) then
                SelectedShape.PenFill.SolidColor := Manager.ToolForeColor;
            end;
          vftNone: begin
              case SelectedShape.PenFill.FillType of
              vftTexture:
                begin
                  SelectedShape.PenFill.SetTexture(Manager.GetToolTexture,
                    SelectedShape.PenFill.TextureMatrix,SelectedShape.PenFill.TextureOpacity,
                    SelectedShape.PenFill.TextureRepetition);
                  if (vsfBackFill in SelectedShape.Fields) and (SelectedShape.BackFill.FillType = vftSolid) then
                    SelectedShape.BackFill.SolidColor := Manager.ToolBackColor;
                end;
              vftGradient,vftSolid:
                begin
                  SelectedShape.PenFill.SetTexture(Manager.GetToolTexture, AffineMatrixIdentity);
                  if (vsfBackFill in SelectedShape.Fields) and (SelectedShape.BackFill.FillType = vftSolid) then
                    SelectedShape.BackFill.SolidColor := Manager.ToolBackColor;
                end;
              end;
            end;
          end;
        end else
        begin
          if (vsfPenFill in SelectedShape.Fields) and (SelectedShape.PenFill.FillType in [vftSolid,vftTexture]) then
            SelectedShape.PenFill.SolidColor := Manager.ToolForeColor;
          if (vsfBackFill in SelectedShape.Fields) and (SelectedShape.BackFill.FillType in [vftSolid,vftTexture]) then
            SelectedShape.BackFill.SolidColor := Manager.ToolBackColor;
        end;
      end;
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
  StopEdit;
  inherited OnTryStop(sender);
end;

procedure TEditShapeTool.StopEdit;
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
  FreeAndNil(FOriginalRect);
  FreeAndNil(FOriginalRectEditor);
  Cursor := crDefault;
  Manager.Image.OnImageChanged.NotifyObservers;
end;

function TEditShapeTool.IsVectorOriginal: boolean;
begin
  result := Manager.Image.LayerOriginalClass[Manager.Image.CurrentLayerIndex]=TVectorOriginal;
end;

function TEditShapeTool.IsOtherOriginal: boolean;
begin
  result := Manager.Image.LayerOriginalKnown[Manager.Image.CurrentLayerIndex] and
     (Manager.Image.LayerOriginalClass[Manager.Image.CurrentLayerIndex]<>TVectorOriginal);
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

function TEditShapeTool.FixLayerOffset: boolean;
begin
  Result:= false;
end;

constructor TEditShapeTool.Create(AManager: TToolManager);
begin
  inherited Create(AManager);
end;

destructor TEditShapeTool.Destroy;
begin
  StopEdit;
  inherited Destroy;
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

  if IsVectorOriginal then
  begin
    if Assigned(VirtualScreen) then
      result := Manager.Image.CurrentState.LayeredBitmap.DrawEditor(VirtualScreen,
        Manager.Image.CurrentLayerIndex, viewMatrix, DoScaleX(PointSize,OriginalDPI))
    else
      result := Manager.Image.CurrentState.LayeredBitmap.GetEditorBounds(
        rect(0,0,VirtualScreenWidth,VirtualScreenHeight),
        Manager.Image.CurrentLayerIndex, viewMatrix, DoScaleX(PointSize,OriginalDPI));
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
      if not handled and IsVectorOriginal then
      begin
        if (key = VK_DELETE) and Assigned(GetVectorOriginal.SelectedShape) then
          GetVectorOriginal.RemoveShape(GetVectorOriginal.SelectedShape);
      end;
      BindOriginalEvent(false);
    end else
    begin
      if key = VK_RETURN then
      begin
        StopEdit;
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
    if IsVectorOriginal then
    begin
      BindOriginalEvent(true);
      Manager.Image.CurrentState.LayeredBitmap.MouseUp(FRightButton, FShiftState, FLastPos.X,FLastPos.Y, cur, handled);
      if handled then
      begin
        Cursor := OriginalCursorToCursor(cur);
        result := OnlyRenderChange;
      end;
      if not handled then
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

