// SPDX-License-Identifier: GPL-3.0-only
unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, FileUtil, Forms, Controls, Graphics, Dialogs,
  LCLType, ExtCtrls, StdCtrls, ComCtrls, ExtDlgs, Menus, ActnList, LCScaleDPI,
  BCTrackbarUpdown, BCPanel, BCButton, BGRAVirtualScreen, BGRAImageList,
  BGRABitmap, BGRABitmapTypes, BGRAGraphics, BGRALazPaint, BGRALayerOriginal,
  BGRATransform, BGRAGradientScanner, LCVectorOriginal, LCVectorShapes,
  LCVectorRectShapes, LCVectorPolyShapes, LCVectorTextShapes,
  LCVectorialFillControl, LCVectorialFill, LCVectorMultishape, fgl;

const
  RenderDelayMs = 100; //minimum delay between the end of the last rendering and the beginning of the next rendering
  ToolIconSize = 36;
  ActionIconSize = 24;
  EditorPointSize = 7;
  DefaultImageWidth = 640;
  DefaultImageHeight = 480;
  PenStyleToStr : array[TPenStyle] of string = ('─────', '─ ─ ─ ─', '···············', '─ · ─ · ─', '─ ·· ─ ·· ╴', 'InsideFrame', 'Pattern', 'Clear');
  PhongShapeKindToStr: array[TPhongShapeKind] of string = ('Rectangle', 'Round rectangle', 'Half sphere', 'Cone top', 'Cone side',
                     'Horizontal cylinder', 'Vertical cylinder');

type
  TPaintTool = (ptHand, ptMovePenFillPoint, ptMoveBackFillPoint, ptMoveOutlineFillPoint, ptRectangle, ptEllipse, ptPolyline, ptCurve, ptPolygon, ptClosedCurve,
                ptPhongShape, ptText);

const
  PaintToolClass : array[TPaintTool] of TVectorShapeAny =
    (nil, nil, nil, nil, TRectShape, TEllipseShape, TPolylineShape, TCurveShape, TPolylineShape, TCurveShape,
     TPhongShape, TTextShape);

function IsCreateShapeTool(ATool: TPaintTool): boolean;

const
  SplineStyleToStr : array[TSplineStyle] of string =
    ('Inside','Inside + ends','Crossing','Crossing + ends','Outside','Round outside','Vertex to side','Easy Bézier');

  FontBidiModeToStr : array[TFontBidiMode] of string =
    ('Auto', 'Left to right', 'Right to left');

type
  TOriginalDiffList = specialize TFPGObjectList<TBGRAOriginalDiff>;

  { TForm1 }

  TForm1 = class(TForm)
    OutlineFillControl: TLCVectorialFillControl;
    ButtonMoveOutlineFillPoints: TToolButton;
    LOutline: TLabel;
    PanelOutlineFill: TBCPanel;
    PanelOutlineFillHead: TPanel;
    PenFillControl: TLCVectorialFillControl;
    ButtonMoveBackFillPoints: TToolButton;
    ButtonMovePenFillPoints: TToolButton;
    LBack: TLabel;
    BackFillControl: TLCVectorialFillControl;
    LPen: TLabel;
    PanelPenFill: TBCPanel;
    PanelBackFillHead: TPanel;
    PanelPenFillHead: TPanel;
    ShapeSendToBack: TAction;
    ShapeBringToFront: TAction;
    ShapeMoveDown: TAction;
    ShapeMoveUp: TAction;
    DelayedRenderTimer: TTimer;
    ToolBarBackFill: TToolBar;
    ToolBarOutlineFill: TToolBar;
    ToolBarPenFill: TToolBar;
    ToolButtonTextShape: TToolButton;
    UpDownOutlineWidth: TBCTrackbarUpdown;
    VectorImageList24: TBGRAImageList;
    ActionList: TActionList;
    EditCopy: TAction;
    EditCut: TAction;
    EditDelete: TAction;
    EditPaste: TAction;
    FileNew: TAction;
    FileOpen: TAction;
    FileSave: TAction;
    FileSaveAs: TAction;
    ButtonPenStyle: TBCButton;
    Label3: TLabel;
    PanelBackFill: TBCPanel;
    PanelBasicStyle: TBCPanel;
    PanelExtendedStyle: TBCPanel;
    PanelFile: TBCPanel;
    PanelShape: TBCPanel;
    PenStyleImageList: TBGRAImageList;
    ToolBarFile: TToolBar;
    ToolBarEdit: TToolBar;
    ToolBarTop: TToolBar;
    ToolBarJoinStyle: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButtonJoinBevel: TToolButton;
    ToolButtonJoinMiter: TToolButton;
    ToolButtonJoinRound: TToolButton;
    ToolButtonPhongShape: TToolButton;
    BCPanelToolChoice: TBCPanel;
    BCPanelToolbar: TBCPanel;
    ToolImageList48: TBGRAImageList;
    BGRAVirtualScreen1: TBGRAVirtualScreen;
    OpenDialog1: TOpenDialog;
    OpenPictureDialog1: TOpenPictureDialog;
    SaveDialog1: TSaveDialog;
    ToolBarTools: TToolBar;
    ToolButtonPolyline: TToolButton;
    ToolButtonCurve: TToolButton;
    ToolButtonMove: TToolButton;
    ToolButtonClosedCurve: TToolButton;
    ToolButtonPolygon: TToolButton;
    ToolButtonRectangle: TToolButton;
    ToolButtonEllipse: TToolButton;
    UpDownPenWidth: TBCTrackbarUpdown;
    procedure BCPanelToolbarResize(Sender: TObject);
    procedure BCPanelToolChoiceResize(Sender: TObject);
    procedure BGRAVirtualScreen1Enter(Sender: TObject);
    procedure BGRAVirtualScreen1Exit(Sender: TObject);
    procedure BGRAVirtualScreen1MouseWheel(Sender: TObject; {%H-}Shift: TShiftState;
      WheelDelta: Integer; {%H-}MousePos: TPoint; var {%H-}Handled: Boolean);
    procedure DelayedRenderTimerTimer(Sender: TObject);
    procedure EditCopyExecute(Sender: TObject);
    procedure EditCutExecute(Sender: TObject);
    procedure EditDeleteExecute(Sender: TObject);
    procedure EditPasteExecute(Sender: TObject);
    procedure EditPasteUpdate(Sender: TObject);
    procedure FileNewExecute(Sender: TObject);
    procedure FileOpenExecute(Sender: TObject);
    procedure FileSaveAsExecute(Sender: TObject);
    procedure FileSaveExecute(Sender: TObject);
    procedure BackFillControlResize(Sender: TObject);
    procedure OutlineFillControlResize(Sender: TObject);
    procedure PanelFileResize(Sender: TObject);
    procedure PanelShapeResize(Sender: TObject);
    procedure ShapeBringToFrontExecute(Sender: TObject);
    procedure ShapeMoveDownExecute(Sender: TObject);
    procedure ShapeMoveUpExecute(Sender: TObject);
    procedure ShapeSendToBackExecute(Sender: TObject);
    procedure ToolButtonJoinClick(Sender: TObject);
    procedure UpDownOutlineWidthChange(Sender: TObject; AByUser: boolean);
    procedure UpDownPenWidthChange(Sender: TObject; AByUser: boolean);
    procedure BGRAVirtualScreen1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BGRAVirtualScreen1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure BGRAVirtualScreen1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure ToolButtonClick(Sender: TObject);
  private
    FSpecialKeyPressed: array[TSpecialKey] of boolean;
    FLastBackspaceOrDel: boolean;
    FLastRenderDateTime: TDateTime;
    FNextRenderDraft: boolean;

    FPenWidth: single;
    FPenStyle: TBGRAPenStyle;
    FPenJoinStyle: TPenJoinStyle;
    FPenStyleMenu: TPopupMenu;
    FOutlineWidth: Single;

    FFlattened: TBGRABitmap;
    FLastEditorBounds: TRect;
    FUpdatingFromShape: boolean;
    FUpdatingSpinEditPenWidth: boolean;
    FCurrentTool: TPaintTool;

    FSplineStyle: TSplineStyle;
    FSplineStyleMenu: TPopupMenu;
    FComboboxSplineStyle: TBCButton;
    FSplineToolbar: TToolBar;

    FPhongShapeKind: TPhongShapeKind;
    FPhongShapeKindToolbar: TToolBar;
    FUpDownPhongBorderSize: TBCTrackbarUpdown;
    FPhongShapeAltitude,FPhongBorderSize: single;

    FTextToolbar: TToolbar;
    FTextDirectionButton: TToolButton;
    FTextFontName: string;
    FTextFontNameEditing: boolean;
    FTextFontStyle: TFontStyles;
    FTextFontHeight: single;
    FTextAlign: TBidiTextAlignment;
    FTextAlignButton: array[TAlignment] of TToolButton;
    FTextDirection: TFontBidiMode;
    FTextDirectionMenu: TPopupMenu;
    FTextPenPhong: boolean;
    FTextAltitudePercent: single;

    FInRemoveShapeIfEmpty: Boolean;
    FFullIconHeight: integer;
    FVectorImageList: TBGRAImageList;
    procedure ComboBoxSplineStyleClick(Sender: TObject);
    function GetOriginalZoomFactor: single;
    function GetOutlineWidth: single;
    function GetPenStyle: TBGRAPenStyle;
    function GetPenWidth: single;
    function GetSplineStyle: TSplineStyle;
    function GetVectorOriginal: TVectorOriginal;
    function GetVectorTransform: TAffineMatrix;
    function GetZoomFactor: single;
    procedure ImageChange(ARectF: TRectF);
    procedure LoadVectorImages;
    procedure OnClickSplineStyleItem(ASender: TObject);
    procedure OnEditingChange({%H-}ASender: TObject; AOriginal: TBGRALayerCustomOriginal);
    procedure OnEditorFocusChange(Sender: TObject);
    procedure OnOriginalChange({%H-}ASender: TObject; AOriginal: TBGRALayerCustomOriginal;
                               var ADiff: TBGRAOriginalDiff);
    procedure OnPhongBorderSizeChange(Sender: TObject; AByUser: boolean);
    procedure OnPhongShapeAltitudeChange(Sender: TObject; AByUser: boolean);
    procedure OnSelectShape(ASender: TObject; AShape: TVectorShape; APreviousShape: TVectorShape);
    procedure OnClickPenStyle(ASender: TObject);
    procedure OnTextAltitudePercentChange(Sender: TObject; AByUser: boolean);
    procedure PhongShapeKindClick(Sender: TObject);
    procedure RequestBackFillUpdate(Sender: TObject);
    procedure RequestOutlineFillUpdate(Sender: TObject);
    procedure OnBackFillChange({%H-}ASender: TObject);
    procedure SetCurrentTool(AValue: TPaintTool);
    procedure SetOutlineWidth(AValue: single);
    procedure SetPenJoinStyle(AValue: TPenJoinStyle);
    procedure SetPenStyle(AValue: TBGRAPenStyle);
    procedure SetPenWidth(AValue: single);
    procedure SetPhongShapeKind(AValue: TPhongShapeKind);
    procedure SetSplineStyle(AValue: TSplineStyle);
    procedure SetVectorLayerIndex(AValue: integer);
    procedure SetZoomFactor(AValue: single);
    procedure SplineToolbarClick(Sender: TObject);
    procedure TextPenPhongClick(Sender: TObject);
    procedure UpdateViewCursor(ACursor: TOriginalEditorCursor);
    procedure RenderAndUpdate(ADraft: boolean);
    procedure DoRenderAndUpdate;
    procedure FocusView;
    procedure UpdateFlattenedImage(ARect: TRect; AUpdateView: boolean = true);
    procedure UpdateView(AImageChangeRect: TRect);
    procedure UpdateToolbarFromShape(AShape: TVectorShape);
    procedure UpdateTitleBar;
    procedure ImageChangesCompletely;
    function CreateShape(const APoint1, APoint2: TPointF): TVectorShape;
    procedure RemoveExtendedStyleControls;
    procedure UpdateBackToolFillPoints;
    procedure UpdatePenToolFillPoints;
    procedure UpdateOutlineToolFillPoints;
    procedure UpdateShapeBackFill;
    procedure UpdateShapePenFill;
    procedure UpdateShapeOutlineFill;
    procedure UpdateShapeUserMode;
    procedure UpdateShapeActions(AShape: TVectorShape);
    procedure RemoveShapeIfEmpty(AShape: TVectorShape);
    function VirtualScreenToImgCoord(X,Y: Integer): TPointF;
    procedure SetEditorGrid(AActive: boolean);
    procedure RequestBackFillAdjustToShape(Sender: TObject);
    procedure PenFillControlResize(Sender: TObject);
    procedure RequestPenFillAdjustToShape(Sender: TObject);
    procedure RequestPenFillUpdate(Sender: TObject);
    procedure RequestOutlineFillAdjustToShape(Sender: TObject);
    procedure AdjustToolbarTop;
    procedure UpdateSplineToolbar;
    function SnapToGrid(APoint: TPointF): TPointF;
    function ImgCoordToOriginalCoord(APoint: TPointF): TPointF;
    procedure TextAlignClick(Sender: TObject);
    procedure OnTextFontHeightChange(Sender: TObject; AByUser: boolean);
    procedure TextDirClick(Sender: TObject);
    procedure OnClickTextDirectionItem(Sender: TObject);
    procedure UpdateTextAlignment;
    procedure TextStyleClick(Sender: TObject);
    procedure TextFontTextBoxChange(Sender: TObject);
    procedure TextFontTextBoxEnter(Sender: TObject);
    procedure TextFontTextBoxExit(Sender: TObject);
    procedure NewImage(AWidth,AHeight: integer);
    procedure SetImage(AImage: TBGRALazPaintImage);
    procedure AddDiff({%H-}AOriginal: TBGRALayerCustomOriginal; ADiff: TBGRAOriginalDiff);
  public
    { public declarations }
    img: TBGRALazPaintImage;
    FVectorLayerIndex: Integer;
    FDiffList: TOriginalDiffList;
    FDiffListPos,
    FDiffListSavePos: integer;
    FDiffAppend: boolean;
    FDiffLastDate: TDateTime;
    filename: string;
    zoom: TAffineMatrix;
    newShape: TVectorShape;
    justDown, shapeAdded: boolean;
    newStartPoint: TPointF;
    newButton: TMouseButton;
    mouseState: TShiftState;
    baseCaption: string;
    procedure DoCopy;
    procedure DoCut;
    procedure DoPaste;
    procedure DoDelete;
    procedure DoUndo;
    procedure DoRedo;
    property vectorTransform: TAffineMatrix read GetVectorTransform;
    property penWidth: single read GetPenWidth write SetPenWidth;
    property penStyle: TBGRAPenStyle read GetPenStyle write SetPenStyle;
    property outlineWidth: single read GetOutlineWidth write SetOutlineWidth;
    property splineStyle: TSplineStyle read GetSplineStyle write SetSplineStyle;
    property currentTool: TPaintTool read FCurrentTool write SetCurrentTool;
    property joinStyle: TPenJoinStyle read FPenJoinStyle write SetPenJoinStyle;
    property phongShapeKind: TPhongShapeKind read FPhongShapeKind write SetPhongShapeKind;
    property originalZoomFactor: single read GetOriginalZoomFactor;
    property zoomFactor: single read GetZoomFactor write SetZoomFactor;
    property vectorOriginal: TVectorOriginal read GetVectorOriginal;
    property vectorLayerIndex: integer read FVectorLayerIndex write SetVectorLayerIndex;
  end;

var
  Form1: TForm1;

implementation

uses math, BGRAPen, BGRAThumbnail, BGRAGradientOriginal, LCVectorClipboard, LResources, LCToolbars,
  BGRAText, BGRAUTF8;

{$R *.lfm}

function IsCreateShapeTool(ATool: TPaintTool): boolean;
begin
  result := PaintToolClass[ATool] <> nil;
end;

procedure TForm1.LoadVectorImages;
var
  lst: TStringList;
  i: Integer;
begin
  FFullIconHeight := ActionIconSize+4;
  ToolbarTop.ButtonHeight:= 2*FFullIconHeight+3;
  LBack.Height := FFullIconHeight;
  LPen.Height := FFullIconHeight;
  LOutline.Height := FFullIconHeight;

  if VectorImageList24.Height = ActionIconSize then
  begin
    FVectorImageList := VectorImageList24;
    exit;
  end;

  FVectorImageList:= TBGRAImageList.Create(self);
  FVectorImageList.Width := ActionIconSize;
  FVectorImageList.Height := ActionIconSize;
  lst := TStringList.Create;
  lst.CommaText := GetResourceString('vectorimages.lst');
  for i := 0 to lst.Count-1 do
    LoadToolbarImage(FVectorImageList, i, lst[i]);
  lst.Free;

  SetToolbarImages(ToolBarFile, FVectorImageList);
  SetToolbarImages(ToolBarEdit, FVectorImageList);
  SetToolBarImages(ToolBarBackFill, FVectorImageList);
  SetToolBarImages(ToolBarPenFill, FVectorImageList);
  SetToolBarImages(ToolBarOutlineFill, FVectorImageList);
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  item: TMenuItem;
  ps: TPenStyle;
  ss: TSplineStyle;
  toolImageList: TBGRAImageList;
  i: Integer;
  td: TFontBidiMode;
begin
  baseCaption:= Caption;
  if ToolIconSize <> ToolImageList48.Width then
  begin
    toolImageList := TBGRAImageList.Create(self);
    ScaleImageList(ToolImageList48, ToolIconSize,ToolIconSize, toolImageList);
    SetToolbarImages(ToolBarTools, toolImageList);
  end;

  LoadVectorImages;

  for i := 0 to ActionList.ActionCount-1 do
    with (ActionList.Actions[i] as TAction) do
      if Hint = '' then Hint := Caption;

  NewImage(DefaultImageWidth, DefaultImageHeight);
  zoom := AffineMatrixScale(1,1);
  FPenStyleMenu := TPopupMenu.Create(nil);
  item:= TMenuItem.Create(FPenStyleMenu); item.Caption := PenStyleToStr[psClear];
  item.OnClick := @OnClickPenStyle;       item.Tag := ord(psClear);
  FPenStyleMenu.Items.Add(item);
  for ps := psSolid to psDashDotDot do
  begin
    item:= TMenuItem.Create(FPenStyleMenu); item.Caption := PenStyleToStr[ps];
    item.OnClick := @OnClickPenStyle;       item.Tag := ord(ps);
    FPenStyleMenu.Items.Add(item);
  end;
  ButtonPenStyle.DropDownMenu := FPenStyleMenu;

  PenFillControl.ToolIconSize:= ActionIconSize;
  PenFillControl.SolidColor := BGRABlack;
  PenFillControl.GradStartColor := BGRAWhite;
  PenFillControl.GradEndColor := BGRABlack;
  PenFillControl.OnFillChange:=@RequestPenFillUpdate;
  PenFillControl.OnAdjustToShape:=@RequestPenFillAdjustToShape;
  PenFillControl.OnResize:=@PenFillControlResize;

  BackFillControl.ToolIconSize:= ActionIconSize;
  BackFillControl.SolidColor := CSSDodgerBlue;
  BackFillControl.GradStartColor := MergeBGRA(CSSDodgerBlue,BGRAWhite);
  BackFillControl.GradEndColor := MergeBGRA(CSSDodgerBlue,BGRABlack);
  BackFillControl.OnFillChange:= @RequestBackFillUpdate;
  BackFillControl.OnAdjustToShape:= @RequestBackFillAdjustToShape;
  BackFillControl.OnResize:= @BackFillControlResize;

  OutlineFillControl.ToolIconSize:= ActionIconSize;
  OutlineFillControl.SolidColor := CSSYellow;
  OutlineFillControl.FillType:= vftNone;
  OutlineFillControl.GradStartColor := BGRAWhite;
  OutlineFillControl.GradEndColor := CSSYellow;
  OutlineFillControl.OnFillChange:= @RequestOutlineFillUpdate;
  OutlineFillControl.OnAdjustToShape:= @RequestOutlineFillAdjustToShape;
  OutlineFillControl.OnResize:= @OutlineFillControlResize;

  FSplineStyleMenu := TPopupMenu.Create(nil);
  for ss := low(TSplineStyle) to high(TSplineStyle) do
  begin
    item := TMenuItem.Create(FSplineStyleMenu); item.Caption := SplineStyleToStr[ss];
    item.OnClick:=@OnClickSplineStyleItem;      item.Tag := ord(ss);
    FSplineStyleMenu.Items.Add(item);
  end;

  FTextDirectionMenu := TPopupMenu.Create(nil);
  FTextDirectionMenu.Images := VectorImageList24;
  for td := low(TFontBidiMode) to high(TFontBidiMode) do
  begin
    item := TMenuItem.Create(FTextDirectionMenu); item.Caption := FontBidiModeToStr[td];
    item.OnClick:=@OnClickTextDirectionItem;      item.Tag := ord(td);
    item.ImageIndex:= 31+ord(td);
    FTextDirectionMenu.Items.Add(item);
  end;

  newShape:= nil;
  penWidth := 5;
  penStyle := SolidPenStyle;
  outlineWidth := DefaultShapeOutlineWidth;
  joinStyle:= pjsBevel;
  currentTool:= ptHand;
  splineStyle:= ssEasyBezier;
  FPhongShapeAltitude := DefaultPhongShapeAltitudePercent;
  FPhongBorderSize := DefaultPhongBorderSizePercent;
  FTextDirection:= fbmAuto;
  FTextAlign:= btaNatural;
  FTextFontName := TTextShape.DefaultFontName;
  FTextFontStyle:= [];
  FTextFontHeight:= TTextShape.DefaultFontEmHeight;
  FTextPenPhong:= false;
  FTextAltitudePercent:= TTextShape.DefaultAltitudePercent;
  UpdateBackToolFillPoints;
  UpdatePenToolFillPoints;
  UpdateOutlineToolFillPoints;
  UpdateShapeActions(nil);
end;

procedure TForm1.BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
var
  topLeftF, bottomRightF: TPointF;
  zoomBounds: TRect;
begin
  topLeftF := zoom*PointF(0,0);
  bottomRightF := zoom*PointF(img.Width,img.Height);
  zoomBounds := Rect(round(topLeftF.X),round(topLeftF.Y),round(bottomRightF.X),round(bottomRightF.Y));
  Bitmap.DrawCheckers(zoomBounds, CSSWhite,CSSSilver);
  if FFlattened = nil then
    UpdateFlattenedImage(rect(0,0,img.Width,img.Height), false);
  Bitmap.StretchPutImage(zoomBounds, FFlattened, dmLinearBlend);
  if vectorLayerIndex <> -1 then
    FLastEditorBounds := img.DrawEditor(Bitmap, vectorLayerIndex, zoom, EditorPointSize)
  else
    FLastEditorBounds := EmptyRect;
end;

procedure TForm1.BGRAVirtualScreen1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  imgPtF: TPointF;
  cur: TOriginalEditorCursor;
  handled: boolean;
begin
  FocusView;
  mouseState:= Shift;
  if [ssLeft,ssRight]*mouseState = [] then FDiffAppend := false;
  imgPtF := VirtualScreenToImgCoord(X,Y);
  SetEditorGrid(ssCtrl in Shift);
  img.MouseDown(Button=mbRight, Shift, imgPtF.x, imgPtF.y, cur, handled);
  UpdateViewCursor(cur);
  if handled then
  begin
    UpdateTextAlignment;
    exit;
  end;

  if not justDown and not Assigned(newShape) then
  begin
    newStartPoint := ImgCoordToOriginalCoord(imgPtF);
    newButton := Button;
    justDown := true;
  end;
end;

procedure TForm1.UpDownPenWidthChange(Sender: TObject; AByUser: boolean);
begin
  if FUpdatingSpinEditPenWidth or not AByUser then exit;
  penWidth := UpDownPenWidth.Value*0.1;
end;

procedure TForm1.ShapeBringToFrontExecute(Sender: TObject);
begin
  if Assigned(vectorOriginal) and
     Assigned(vectorOriginal.SelectedShape) then
  begin
    vectorOriginal.SelectedShape.BringToFront;
    UpdateShapeActions(vectorOriginal.SelectedShape);
  end;
end;

procedure TForm1.ShapeMoveDownExecute(Sender: TObject);
begin
  if Assigned(vectorOriginal) and
     Assigned(vectorOriginal.SelectedShape) then
  begin
    vectorOriginal.SelectedShape.MoveDown(true);
    UpdateShapeActions(vectorOriginal.SelectedShape);
  end;
end;

procedure TForm1.ShapeMoveUpExecute(Sender: TObject);
begin
  if Assigned(vectorOriginal) and
     Assigned(vectorOriginal.SelectedShape) then
  begin
    vectorOriginal.SelectedShape.MoveUp(true);
    UpdateShapeActions(vectorOriginal.SelectedShape);
  end;
end;

procedure TForm1.ShapeSendToBackExecute(Sender: TObject);
begin
  if Assigned(vectorOriginal) and
     Assigned(vectorOriginal.SelectedShape) then
  begin
    vectorOriginal.SelectedShape.SendToBack;
    UpdateShapeActions(vectorOriginal.SelectedShape);
  end;
end;

procedure TForm1.ToolButtonJoinClick(Sender: TObject);
begin
  if (Sender as TToolButton).Down then
  begin
    if Sender = ToolButtonJoinRound then joinStyle := pjsRound else
    if Sender = ToolButtonJoinBevel then joinStyle := pjsBevel else
    if Sender = ToolButtonJoinMiter then joinStyle := pjsMiter;
  end;
end;

procedure TForm1.UpDownOutlineWidthChange(Sender: TObject; AByUser: boolean);
begin
  if not AByUser then exit;
  outlineWidth := UpDownOutlineWidth.Value/10;
end;

procedure TForm1.EditCopyExecute(Sender: TObject);
begin
  DoCopy;
end;

procedure TForm1.EditCutExecute(Sender: TObject);
begin
  DoCut;
end;

procedure TForm1.EditDeleteExecute(Sender: TObject);
begin
  DoDelete;
end;

procedure TForm1.EditPasteExecute(Sender: TObject);
begin
  DoPaste;
end;

procedure TForm1.EditPasteUpdate(Sender: TObject);
begin
  EditPaste.Enabled := ClipboardHasShapes;
end;

procedure TForm1.FileNewExecute(Sender: TObject);
var
  dimStr: String;
  newWidth, newheight, idxX: integer;
begin
  if Assigned(vectorOriginal) then
  begin
    try
      if Assigned(img) then
        dimStr := inttostr(img.Width)+'x'+inttostr(img.Height)
      else
        dimStr := inttostr(DefaultImageWidth)+'x'+inttostr(DefaultImageHeight);
      dimStr := InputBox('New image','Dimensions:',dimStr);
      idxX := pos('x',dimStr);
      if idxX=0 then exit;
      newWidth := StrToInt(copy(dimStr,1,idxX-1));
      newheight := StrToInt(copy(dimStr,idxX+1,length(dimStr)-idxX));
    except
      on ex:exception do
      begin
        ShowMessage(ex.Message);
        exit;
      end;
    end;
    NewImage(newWidth,newheight);
  end;
end;

procedure TForm1.FileOpenExecute(Sender: TObject);
var
  openedImg: TBGRALazPaintImage;
  openedLayer, i: Integer;
begin
  if OpenDialog1.Execute then
  begin
    openedImg := TBGRALazPaintImage.Create;
    try
      openedImg.LoadFromFile(OpenDialog1.FileName);
      openedLayer := -1;
      for i := 0 to openedImg.NbLayers-1 do
        if openedImg.LayerOriginalClass[i] = TVectorOriginal then
        begin
          openedLayer:= i;
          break;
        end;
      if openedLayer= -1 then raise exception.Create('Cannot find any vector layer');

      SetImage(openedImg);
      openedImg := nil;
      vectorLayerIndex:= openedLayer;
      filename:= OpenDialog1.FileName;
      UpdateTitleBar;
    except
      on ex: exception do
        ShowMessage(ex.Message);
    end;
    openedImg.Free;
  end;
end;

procedure TForm1.FileSaveAsExecute(Sender: TObject);
begin
  if not Assigned(img) then exit;
  if SaveDialog1.Execute then
  begin
    try
      if Assigned(vectorOriginal) then RemoveShapeIfEmpty(vectorOriginal.SelectedShape);
      img.SaveToFile(SaveDialog1.FileName);
      filename := SaveDialog1.FileName;
      UpdateTitleBar;
    except
      on ex: exception do
        ShowMessage(ex.Message);
    end;
  end;
end;

procedure TForm1.FileSaveExecute(Sender: TObject);
begin
  if filename = '' then
    FileSaveAs.Execute
  else
  begin
    try
      if Assigned(vectorOriginal) then RemoveShapeIfEmpty(vectorOriginal.SelectedShape);
      img.SaveToFile(filename);
    except
      on ex: exception do
        ShowMessage(ex.Message);
    end;
  end;
end;

procedure TForm1.BackFillControlResize(Sender: TObject);
begin
  PanelBackFill.ClientWidth := PanelBackFillHead.Width+BackFillControl.Width+2;
end;

procedure TForm1.OutlineFillControlResize(Sender: TObject);
begin
  PanelOutlineFill.ClientWidth := PanelOutlineFillHead.Width+OutlineFillControl.Width+2;
end;

procedure TForm1.PanelFileResize(Sender: TObject);
begin
  ToolBarFile.Width := GetToolbarSize(ToolBarFile).cx;
  PanelFile.Width := ToolBarFile.Width+3;
end;

procedure TForm1.PanelShapeResize(Sender: TObject);
begin
  ToolBarEdit.Width := GetToolbarSize(ToolBarEdit).cx;
  PanelShape.Width := ToolBarEdit.Width+3;
end;

procedure TForm1.BCPanelToolChoiceResize(Sender: TObject);
begin
  ToolbarTools.Width := GetToolbarSize(ToolbarTools).cx;
  BCPanelToolChoice.Width := ToolbarTools.Width+3;
end;

procedure TForm1.BGRAVirtualScreen1Enter(Sender: TObject);
begin
  if Assigned(img) then img.EditorFocused:= true;
end;

procedure TForm1.BGRAVirtualScreen1Exit(Sender: TObject);
begin
  if Assigned(img) then img.EditorFocused:= false;
end;

procedure TForm1.BGRAVirtualScreen1MouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  zoomFactor := zoomFactor*power(2, WheelDelta/240);
end;

procedure TForm1.DelayedRenderTimerTimer(Sender: TObject);
begin
  DelayedRenderTimer.Enabled:= false;
  DoRenderAndUpdate;
end;

procedure TForm1.BCPanelToolbarResize(Sender: TObject);
begin
  AdjustToolbarTop;
end;

procedure TForm1.BGRAVirtualScreen1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  imgPtF, ptF: TPointF;
  prevRF, rF: TRectF;
  cur: TOriginalEditorCursor;
  handled: boolean;
  vectorFill: TVectorialFill;
begin
  mouseState:= Shift;
  if [ssLeft,ssRight]*mouseState = [] then FDiffAppend := false;
  imgPtF := VirtualScreenToImgCoord(X,Y);
  SetEditorGrid(ssCtrl in Shift);
  img.MouseMove(Shift, imgPtF.X, imgPtF.Y, cur, handled);
  if handled then UpdateTextAlignment;
  UpdateViewCursor(cur);

  if Assigned(vectorOriginal) then
  begin
    ptF := ImgCoordToOriginalCoord(imgPtF);
    if justDown and not Assigned(newShape) and IsCreateShapeTool(currentTool) and
      (VectLen(ptF-newStartPoint) >= EditorPointSize) then
    begin
      vectorOriginal.DeselectShapes;
      newShape := CreateShape(newStartPoint,ptF);
      shapeAdded := false;
      rF := newShape.GetRenderBounds(InfiniteRect, vectorTransform);
      ImageChange(rF);
      justDown := false;
      if IsEmptyRectF(rF) and newShape.CreateEmpty then
      begin
        vectorOriginal.DeselectShapes;
        vectorOriginal.AddShape(newShape);
        vectorOriginal.SelectShape(newShape);
        currentTool:= ptHand;
        shapeAdded := true;
      end;
    end else
    if Assigned(newShape) then
    begin
      prevRF := newShape.GetRenderBounds(InfiniteRect, vectorTransform);
      newShape.QuickDefine(newStartPoint,ptF);
      if (vsfBackFill in newShape.Fields) and (newShape.BackFill.FillType in [vftGradient, vftTexture]) then
      begin
        vectorFill := BackFillControl.CreateShapeFill(newShape);
        newShape.BackFill := vectorFill;
        vectorFill.Free;
      end;
      if (vsfPenFill in newShape.Fields) and (newShape.PenFill.FillType in [vftGradient, vftTexture]) then
      begin
        vectorFill := PenFillControl.CreateShapeFill(newShape);
        newShape.PenFill := vectorFill;
        vectorFill.Free;
      end;
      if (vsfOutlineFill in newShape.Fields) and (newShape.OutlineFill.FillType in [vftGradient, vftTexture]) then
      begin
        vectorFill := OutlineFillControl.CreateShapeFill(newShape);
        newShape.OutlineFill := vectorFill;
        vectorFill.Free;
      end;
      rF := newShape.GetRenderBounds(InfiniteRect, vectorTransform);
      ImageChange(rF.Union(prevRF, true));
    end;
  end;
end;

procedure TForm1.BGRAVirtualScreen1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  rF: TRectF;
  imgPtF: TPointF;
  handled: boolean;
  cur: TOriginalEditorCursor;
  addedShape, curShape: TVectorShape;
begin
  mouseState:= Shift;
  if [ssLeft,ssRight]*mouseState = [] then FDiffAppend := false;
  imgPtF := VirtualScreenToImgCoord(X,Y);
  SetEditorGrid(ssCtrl in Shift);
  img.MouseUp(Button = mbRight, Shift, imgPtF.X, imgPtF.Y, cur, handled);
  if handled then
  begin
    UpdateTextAlignment;
    RenderAndUpdate(false);
  end;
  UpdateViewCursor(cur);

  if Assigned(vectorOriginal) then
  begin
    if justDown and (Button = newButton) then
    begin
      if IsCreateShapeTool(currentTool) and (vsuCreate in PaintToolClass[currentTool].Usermodes) then
      begin
        vectorOriginal.DeselectShapes;
        vectorOriginal.AddShape(CreateShape(newStartPoint,newStartPoint), vsuCreate);
      end else
      if IsCreateShapeTool(currentTool) and PaintToolClass[currentTool].CreateEmpty then
      begin
        vectorOriginal.DeselectShapes;
        addedShape := CreateShape(newStartPoint,newStartPoint);
        vectorOriginal.AddShape(addedShape);
        vectorOriginal.SelectShape(addedShape);
        currentTool:= ptHand;
      end else
        vectorOriginal.MouseClick(newStartPoint, DoScaleX(6, 96)/zoomFactor*originalZoomFactor,
                                  ssCtrl in Shift);
      justDown:= false;
    end
    else if Assigned(newShape) and (Button = newButton) then
    begin
      if shapeAdded then
        newShape := nil
      else
      begin
        rF := newShape.GetRenderBounds(InfiniteRect, vectorTransform);
        if not IsEmptyRectF(rF) or (vsuCreate in newShape.Usermodes) then
        begin
          addedShape := newShape;
          newShape := nil;
          vectorOriginal.AddShape(addedShape, vsuCreate);
        end
        else
        begin
          FreeAndNil(newShape);
          ShowMessage('Shape is empty and was not added');
        end;
      end;
    end;
  end;

  if Assigned(vectorOriginal) and
     Assigned(vectorOriginal.SelectedShape) then
  begin
    curShape := vectorOriginal.SelectedShape;
    case currentTool of
      ptMoveBackFillPoint: if curShape.Usermode <> vsuEditBackFill then currentTool := ptHand;
      ptMovePenFillPoint: if curShape.Usermode <> vsuEditPenFill then currentTool := ptHand;
      ptMoveOutlineFillPoint: if curShape.Usermode <> vsuEditOutlineFill then currentTool := ptHand;
    end;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  RemoveExtendedStyleControls;
  if (newShape <> nil) and not shapeAdded then FreeAndNil(newShape);
  img.Free;
  FDiffList.Free;
  FFlattened.Free;
  ButtonPenStyle.DropDownMenu := nil;
  FPenStyleMenu.Free;
  FSplineStyleMenu.Free;
  FTextDirectionMenu.Free;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  AHandled: boolean;
  sk: TSpecialKey;
begin
  FLastBackspaceOrDel:= (Key=VK_BACK) or (Key=VK_DELETE);
  if FTextFontNameEditing then
  begin
    if Key=VK_RETURN then
    begin
      FocusView;
      Key := 0;
    end;
    exit;
  end;

  if Assigned(img) and img.EditorFocused then
  begin
    sk := LCLKeyToSpecialKey(Key, Shift);
    if sk<>skUnknown then
    begin
      FSpecialKeyPressed[sk] := true;
      img.KeyDown(Shift, sk, AHandled);
      if AHandled then
      begin
        Key := 0;
        UpdateTextAlignment;
      end;
    end;
  end;

  if (Key = VK_X) and (ssCtrl in Shift) then
  begin
    Key := 0;
    DoCut;
  end else
  if (Key = VK_C) and (ssCtrl in Shift) then
  begin
    Key := 0;
    DoCopy;
  end else
  if (Key = VK_V) and (ssCtrl in Shift) then
  begin
    Key := 0;
    DoPaste;
  end else
  if Key = VK_DELETE then
  begin
    Key := 0;
    DoDelete;
  end else
  if (Key = VK_Z) and ([ssCtrl,ssShift]*Shift=[ssCtrl]) and (FDiffListPos > 0) and
    not FDiffAppend then
  begin
    Key := 0;
    DoUndo;
  end else
  if ( ((Key = VK_Y) and ([ssCtrl,ssShift]*Shift=[ssCtrl])) or
       ((Key = VK_Z) and ([ssCtrl,ssShift]*Shift=[ssCtrl,ssShift])) )
     and Assigned(FDiffList) and (FDiffListPos < FDiffList.Count) and
     not FDiffAppend then
  begin
    Key := 0;
    DoRedo;
  end else
  if (Key = VK_RETURN) and Assigned(vectorOriginal) and Assigned(vectorOriginal.SelectedShape) then
  begin
    Key := 0;
    vectorOriginal.DeselectShapes;
  end;
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  AHandled: boolean;
  sk: TSpecialKey;
begin
  sk := LCLKeyToSpecialKey(Key, Shift);
  if Assigned(img) and FSpecialKeyPressed[sk] then
  begin
    img.KeyUp(Shift, sk, AHandled);
    FSpecialKeyPressed[sk] := false;
    if AHandled then
    begin
      Key:= 0;
      UpdateTextAlignment;
    end;
  end;
end;

procedure TForm1.FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
var AHandled: boolean;
begin
  FLastBackspaceOrDel:= (UTF8Key=#8);
  if FTextFontNameEditing then exit;
  if Assigned(img) and img.EditorFocused then
  begin
    img.KeyPress(UTF8Key, AHandled);
    if AHandled then
    begin
      UTF8Key:= '';
      UpdateTextAlignment;
    end;
  end;
end;

procedure TForm1.ToolButtonClick(Sender: TObject);
begin
  if Sender = ButtonMoveBackFillPoints then
    if ButtonMoveBackFillPoints.Down then
    begin
      ToolButtonMove.Down := true;
      ButtonMovePenFillPoints.Down := false;
      ButtonMoveOutlineFillPoints.Down := false;
    end;
  if Sender = ButtonMovePenFillPoints then
    if ButtonMovePenFillPoints.Down then
    begin
      ToolButtonMove.Down := true;
      ButtonMoveBackFillPoints.Down := false;
      ButtonMoveOutlineFillPoints.Down := false;
    end;
  if Sender = ButtonMoveOutlineFillPoints then
    if ButtonMoveOutlineFillPoints.Down then
    begin
      ToolButtonMove.Down := true;
      ButtonMovePenFillPoints.Down := false;
      ButtonMoveBackFillPoints.Down := false;
    end;

  FCurrentTool := ptHand;
  if ButtonMoveBackFillPoints.Down then FCurrentTool:= ptMoveBackFillPoint;
  if ButtonMovePenFillPoints.Down then FCurrentTool:= ptMovePenFillPoint;
  if ButtonMoveOutlineFillPoints.Down then FCurrentTool:= ptMoveOutlineFillPoint;
  if ToolButtonEllipse.Down then FCurrentTool:= ptEllipse;
  if ToolButtonRectangle.Down then FCurrentTool:= ptRectangle;
  if ToolButtonPolyline.Down then FCurrentTool:= ptPolyline;
  if ToolButtonCurve.Down then FCurrentTool:= ptCurve;
  if ToolButtonPolygon.Down then FCurrentTool:= ptPolygon;
  if ToolButtonClosedCurve.Down then FCurrentTool:= ptClosedCurve;
  if ToolButtonPhongShape.Down then FCurrentTool:= ptPhongShape;
  if ToolButtonTextShape.Down then FCurrentTool:= ptText;

  if currentTool <> ptMoveBackFillPoint then ButtonMoveBackFillPoints.Down := false;
  if currentTool <> ptMovePenFillPoint then ButtonMovePenFillPoints.Down := false;
  if currentTool <> ptMoveOutlineFillPoint then ButtonMoveOutlineFillPoints.Down := false;

  if IsCreateShapeTool(currentTool) then
  begin
    if Assigned(vectorOriginal) and (vectorOriginal.SelectedShape <> nil) then vectorOriginal.DeselectShapes
    else UpdateToolbarFromShape(nil);

    if currentTool in [ptPolyline, ptCurve] then
      BackFillControl.FillType := vftNone;
  end;

  UpdateShapeUserMode;

  if not Assigned(vectorOriginal) or (vectorOriginal.SelectedShape = nil) then
    UpdateToolbarFromShape(nil);
end;

procedure TForm1.ComboBoxSplineStyleClick(Sender: TObject);
var
  btn: TControl;
  i: Integer;
begin
  if Assigned(FSplineStyleMenu) then
  begin
    btn := Sender as TControl;
    for i := 0 to FSplineStyleMenu.Items.Count-1 do
      FSplineStyleMenu.Items[i].Checked:= FSplineStyleMenu.Items[i].Tag=ord(FSplineStyle);
    with btn.ClientToScreen(Point(0,btn.Height)) do
      FSplineStyleMenu.PopUp(X,Y);
  end;
end;

function TForm1.GetOriginalZoomFactor: single;
var
  m: TAffineMatrix;
begin
  m := vectorTransform;
  result := (VectLen(PointF(m[1,1],m[2,1]))+VectLen(PointF(m[1,2],m[2,2])))/2;
end;

function TForm1.GetOutlineWidth: single;
begin
  result := FOutlineWidth;
end;

function TForm1.GetPenStyle: TBGRAPenStyle;
begin
  result := FPenStyle;
end;

function TForm1.GetPenWidth: single;
begin
  result := FPenWidth;
end;

function TForm1.GetSplineStyle: TSplineStyle;
begin
  result := FSplineStyle;
end;

function TForm1.GetVectorOriginal: TVectorOriginal;
begin
  if Assigned(img) and (vectorLayerIndex >= 0) and (vectorLayerIndex < img.NbLayers) and
     (img.LayerOriginalClass[vectorLayerIndex] = TVectorOriginal) then
   result := img.LayerOriginal[vectorLayerIndex] as TVectorOriginal
  else
    result := nil
end;

function TForm1.GetVectorTransform: TAffineMatrix;
begin
  if Assigned(img) and (vectorLayerIndex<>-1) then
    result:= img.LayerOriginalMatrix[vectorLayerIndex]
  else
    result:= AffineMatrixIdentity;
end;

function TForm1.GetZoomFactor: single;
begin
  result := (VectLen(PointF(zoom[1,1],zoom[2,1]))+VectLen(PointF(zoom[1,2],zoom[2,2])))/2;
end;

procedure TForm1.ImageChange(ARectF: TRectF);
var
  changeRect: TRect;
begin
  if not IsEmptyRectF(ARectF) then
  begin
    changeRect := rect(floor(ARectF.Left),floor(ARectF.Top),ceil(ARectF.Right),ceil(ARectF.Bottom));
    UpdateFlattenedImage(changeRect);
  end;
end;

procedure TForm1.OnClickSplineStyleItem(ASender: TObject);
begin
  splineStyle := TSplineStyle((ASender as TMenuItem).Tag);
  UpdateSplineToolbar;
end;

procedure TForm1.OnClickTextDirectionItem(Sender: TObject);
var
  itm: TMenuItem;
  td: TFontBidiMode;
begin
  itm := TMenuItem(Sender);
  td := TFontBidiMode(itm.Tag);
  FTextDirection:= td;
  if Assigned(FTextDirectionButton) then
    FTextDirectionButton.ImageIndex:= 31 + ord(FTextDirection);
  if Assigned(vectorOriginal) and Assigned(vectorOriginal.SelectedShape) and
    (vectorOriginal.SelectedShape is TTextShape) then
  begin
    TTextShape(vectorOriginal.SelectedShape).FontBidiMode:= td;
    UpdateTextAlignment;
  end;
end;

procedure TForm1.UpdateTextAlignment;
var
  alignment: TAlignment;
begin
  if Assigned(vectorOriginal) and Assigned(vectorOriginal.SelectedShape) and
    (vectorOriginal.SelectedShape is TTextShape) then
  begin
    FTextAlign:= TTextShape(vectorOriginal.SelectedShape).BidiParagraphAlignment;
    alignment := TTextShape(vectorOriginal.SelectedShape).ParagraphAlignment;
    if Assigned(FTextAlignButton[alignment]) then FTextAlignButton[alignment].Down := true;
  end;
end;

procedure TForm1.OnEditingChange(ASender: TObject;
  AOriginal: TBGRALayerCustomOriginal);
begin
  if AOriginal <> vectorOriginal then exit;
  UpdateView(EmptyRect);
end;

procedure TForm1.OnEditorFocusChange(Sender: TObject);
begin
  UpdateView(EmptyRect);
end;

procedure TForm1.OnOriginalChange(ASender: TObject; AOriginal: TBGRALayerCustomOriginal;
  var ADiff: TBGRAOriginalDiff);
var
  slowShape: boolean;
begin
  if AOriginal <> vectorOriginal then exit;
  slowShape := false;
  if mouseState * [ssLeft,ssRight] <> [] then
  begin
    if Assigned(vectorOriginal) and Assigned(vectorOriginal.SelectedShape) then
      slowShape := vectorOriginal.SelectedShape.GetIsSlow(vectorTransform);
  end;
  RenderAndUpdate(slowShape);
  AddDiff(AOriginal, ADiff);
  ADiff := nil;
end;

procedure TForm1.OnPhongBorderSizeChange(Sender: TObject; AByUser: boolean);
begin
  FPhongBorderSize:= (Sender as TBCTrackbarUpdown).Value;
  if AByUser and Assigned(vectorOriginal) and Assigned(vectorOriginal.SelectedShape) and
    (vectorOriginal.SelectedShape is TPhongShape) then
    TPhongShape(vectorOriginal.SelectedShape).BorderSizePercent:= FPhongBorderSize;
end;

procedure TForm1.OnPhongShapeAltitudeChange(Sender: TObject; AByUser: boolean);
begin
  FPhongShapeAltitude:= (Sender as TBCTrackbarUpdown).Value;
  if AByUser and Assigned(vectorOriginal) and Assigned(vectorOriginal.SelectedShape) and
    (vectorOriginal.SelectedShape is TPhongShape) then
    TPhongShape(vectorOriginal.SelectedShape).ShapeAltitudePercent:= FPhongShapeAltitude;
end;

procedure TForm1.OnSelectShape(ASender: TObject; AShape: TVectorShape;
  APreviousShape: TVectorShape);
begin
  if ASender <> vectorOriginal then exit;
  UpdateToolbarFromShape(AShape);
  case currentTool of
    ptMoveBackFillPoint: if AShape.Usermode <> vsuEditBackFill then currentTool := ptHand;
    ptMovePenFillPoint: if AShape.Usermode <> vsuEditPenFill then currentTool := ptHand;
    ptMoveOutlineFillPoint: if AShape.Usermode <> vsuEditOutlineFill then currentTool := ptHand;
  end;
  UpdateShapeActions(AShape);
  RemoveShapeIfEmpty(APreviousShape);
end;

procedure TForm1.OnClickPenStyle(ASender: TObject);
begin
  penStyle := PenStyleToBGRA(TPenStyle((ASender as TMenuItem).Tag));
end;

procedure TForm1.OnTextAltitudePercentChange(Sender: TObject; AByUser: boolean);
begin
  if AByUser then
  begin
    FTextAltitudePercent:= TBCTrackbarUpdown(Sender).Value;
    if not FUpdatingFromShape and Assigned(vectorOriginal) and Assigned(vectorOriginal.SelectedShape) then
    begin
      if vectorOriginal.SelectedShape is TTextShape then
        TTextShape(vectorOriginal.SelectedShape).AltitudePercent:= FTextAltitudePercent;
    end;
  end;
end;

procedure TForm1.OnTextFontHeightChange(Sender: TObject; AByUser: boolean);
begin
  if AByUser then
  begin
    FTextFontHeight:= TBCTrackbarUpdown(Sender).Value;
    if not FUpdatingFromShape and Assigned(vectorOriginal) and Assigned(vectorOriginal.SelectedShape) then
    begin
      if vectorOriginal.SelectedShape is TTextShape then
        TTextShape(vectorOriginal.SelectedShape).FontEmHeight:= FTextFontHeight;
    end;
  end;
end;

procedure TForm1.PhongShapeKindClick(Sender: TObject);
begin
  if (Sender as TToolButton).Down then
    phongShapeKind:= TPhongShapeKind((Sender as TToolButton).Tag);
end;

procedure TForm1.RequestBackFillUpdate(Sender: TObject);
begin
  if not FUpdatingFromShape then
  begin
    UpdateShapeBackFill;
    UpdateBackToolFillPoints;
  end;
end;

procedure TForm1.RequestOutlineFillUpdate(Sender: TObject);
begin
  if not FUpdatingFromShape then
  begin
    UpdateShapeOutlineFill;
    UpdateOutlineToolFillPoints;
  end;
end;

procedure TForm1.OnBackFillChange(ASender: TObject);
begin
  if not FUpdatingFromShape then
    UpdateShapeBackFill;
end;

procedure TForm1.SetCurrentTool(AValue: TPaintTool);
begin
  FCurrentTool:=AValue;
  ToolButtonMove.Down := FCurrentTool = ptHand;
  ToolButtonRectangle.Down := FCurrentTool = ptRectangle;
  ToolButtonEllipse.Down := FCurrentTool = ptEllipse;
  ToolButtonPolygon.Down := FCurrentTool = ptPolygon;
  ToolButtonClosedCurve.Down := FCurrentTool = ptClosedCurve;
  ToolButtonPolyline.Down := FCurrentTool = ptPolyline;
  ToolButtonCurve.Down := FCurrentTool = ptCurve;
  ToolButtonPhongShape.Down:= FCurrentTool = ptPhongShape;
  ButtonMoveBackFillPoints.Down := FCurrentTool = ptMoveBackFillPoint;
  ButtonMovePenFillPoints.Down := FCurrentTool = ptMovePenFillPoint;
  ButtonMoveOutlineFillPoints.Down := FCurrentTool = ptMoveOutlineFillPoint;
  UpdateShapeUserMode;
end;

procedure TForm1.SetOutlineWidth(AValue: single);
begin
  FOutlineWidth := AValue;
  UpDownOutlineWidth.Value := round(AValue*10);
  if not FUpdatingFromShape and Assigned(vectorOriginal) and Assigned(vectorOriginal.SelectedShape) then
    vectorOriginal.SelectedShape.OutlineWidth:= FOutlineWidth;
end;

procedure TForm1.SetPenJoinStyle(AValue: TPenJoinStyle);
begin
  if FPenJoinStyle=AValue then Exit;
  FPenJoinStyle:=AValue;
  ToolButtonJoinRound.Down:= FPenJoinStyle = pjsRound;
  ToolButtonJoinBevel.Down:= FPenJoinStyle = pjsBevel;
  ToolButtonJoinMiter.Down:= FPenJoinStyle = pjsMiter;
  if not FUpdatingFromShape and Assigned(vectorOriginal) then
  begin
    if Assigned(vectorOriginal.SelectedShape) then
      vectorOriginal.SelectedShape.JoinStyle := FPenJoinStyle;
  end;
end;

procedure TForm1.SetPenStyle(AValue: TBGRAPenStyle);
begin
  FPenStyle := AValue;
  ButtonPenStyle.Caption:= PenStyleToStr[BGRAToPenStyle(AValue)];
  if not FUpdatingFromShape and Assigned(vectorOriginal) and Assigned(vectorOriginal.SelectedShape) then
    vectorOriginal.SelectedShape.PenStyle := FPenStyle;
end;

procedure TForm1.SetPenWidth(AValue: single);
var
  cur: single;
begin
  FPenWidth := AValue;
  cur := UpDownPenWidth.Value*0.1;
  if AValue <> cur then
  begin
    FUpdatingSpinEditPenWidth:= true;
    UpDownPenWidth.Value := Round(AValue*10);
    FUpdatingSpinEditPenWidth:= false;
  end;
  if not FUpdatingFromShape and Assigned(vectorOriginal) and Assigned(vectorOriginal.SelectedShape) then
    vectorOriginal.SelectedShape.PenWidth:= penWidth;
end;

procedure TForm1.SetPhongShapeKind(AValue: TPhongShapeKind);
var
  btn: TToolButton;
  i: Integer;
begin
  if FPhongShapeKind=AValue then Exit;
  FPhongShapeKind:=AValue;
  if Assigned(FPhongShapeKindToolbar) then
    for i := 0 to FPhongShapeKindToolbar.ButtonCount-1 do
    begin
      btn := FPhongShapeKindToolbar.Buttons[i];
      if btn.Tag = ord(FPhongShapeKind) then btn.Down := true;
    end;
  if Assigned(FUpDownPhongBorderSize) then
    FUpDownPhongBorderSize.Enabled:= (FPhongShapeKind in[pskRectangle,pskRoundRectangle]);

  if not FUpdatingFromShape and Assigned(vectorOriginal) and Assigned(vectorOriginal.SelectedShape) then
  begin
    if vectorOriginal.SelectedShape is TPhongShape then
      TPhongShape(vectorOriginal.SelectedShape).ShapeKind:= FPhongShapeKind;
  end;
end;

procedure TForm1.SetSplineStyle(AValue: TSplineStyle);
begin
  FSplineStyle := AValue;
  if Assigned(FComboboxSplineStyle) then
    FComboboxSplineStyle.Caption:= SplineStyleToStr[FSplineStyle];
  if not FUpdatingFromShape and Assigned(vectorOriginal) and Assigned(vectorOriginal.SelectedShape) and
    (vectorOriginal.SelectedShape is TCurveShape) then
    TCurveShape(vectorOriginal.SelectedShape).SplineStyle := FSplineStyle;
end;

procedure TForm1.SetVectorLayerIndex(AValue: integer);
var
  prevOrig: TVectorOriginal;
begin
  if FVectorLayerIndex=AValue then Exit;
  prevOrig := vectorOriginal;
  if Assigned(prevOrig) then prevOrig.OnSelectShape:= nil;
  if Assigned(img) and (AValue >= 0) and (AValue < img.NbLayers) and
    (img.LayerOriginalClass[AValue] = TVectorOriginal) then
  begin
    FVectorLayerIndex:= AValue;
    vectorOriginal.OnSelectShape:= @OnSelectShape;
  end else
    FVectorLayerIndex:= -1;
end;

procedure TForm1.SetZoomFactor(AValue: single);
begin
  zoom := AffineMatrixScale(AValue,AValue);
  BGRAVirtualScreen1.DiscardBitmap;
end;

procedure TForm1.SplineToolbarClick(Sender: TObject);
var
  btn: TToolButton;
begin
  if Assigned(vectorOriginal) and Assigned(vectorOriginal.SelectedShape) and
     (vectorOriginal.SelectedShape is TCurveShape) then
  begin
    btn := Sender as TToolButton;
    if btn.Down then
      vectorOriginal.SelectedShape.Usermode := TVectorShapeUsermode(btn.Tag);
  end;
end;

procedure TForm1.TextPenPhongClick(Sender: TObject);
var
  btn: TToolButton;
begin
  btn := Sender as TToolButton;
  FTextPenPhong := btn.Down;
  if Assigned(vectorOriginal) and Assigned(vectorOriginal.SelectedShape) and
     (vectorOriginal.SelectedShape is TTextShape) then
     TTextShape(vectorOriginal.SelectedShape).PenPhong:= FTextPenPhong;
end;

procedure TForm1.TextFontTextBoxChange(Sender: TObject);
var
  i, prevSelStart, prevSelLen: Integer;
  tb: TEdit;
  fontFound: Boolean;
  similarCount,similarIndex, nameLen: integer;
  nameTyped: TCaption;
begin
  tb := Sender as TEdit;
  fontFound := false;
  similarCount := 0;
  similarIndex := -1;
  nameTyped := tb.Text;
  nameLen := UTF8Length(nameTyped);
  for i := 0 to Screen.Fonts.Count-1 do
    if CompareText(nameTyped, Screen.Fonts[i])=0 then
    begin
      prevSelStart := tb.SelStart;
      prevSelLen := tb.SelLength;
      tb.Text := Screen.Fonts[i];
      tb.SelStart := prevSelStart;
      tb.SelLength := prevSelLen;
      fontFound := true;
      break;
    end else
    if (tb.SelStart = nameLen) and (CompareText(nameTyped, copy(Screen.Fonts[i],1,length(nameTyped)))=0) then
    begin
      inc(similarCount);
      similarIndex := i;
    end;

  if not fontFound and (similarCount = 1) and not FLastBackspaceOrDel then
  begin
    tb.Text := Screen.Fonts[similarIndex];
    tb.SelStart:= nameLen;
    tb.SelLength:= UTF8Length(tb.Text)-nameLen;
    fontFound := true;
  end;

  if fontFound then
  begin
    FTextFontName := tb.Text;

    if Assigned(vectorOriginal) and Assigned(vectorOriginal.SelectedShape) and
      (vectorOriginal.SelectedShape is TTextShape) then
      TTextShape(vectorOriginal.SelectedShape).FontName := FTextFontName;
  end;
end;

procedure TForm1.TextFontTextBoxEnter(Sender: TObject);
begin
  FTextFontNameEditing := true;
end;

procedure TForm1.TextFontTextBoxExit(Sender: TObject);
var
  tb: TEdit;
begin
  FTextFontNameEditing := false;
  tb := Sender as TEdit;
  tb.Text := FTextFontName;
end;

procedure TForm1.NewImage(AWidth, AHeight: integer);
var
  orig: TVectorOriginal;
  newImg: TBGRALazPaintImage;
  layerIndex: Integer;
begin
  newImg := TBGRALazPaintImage.Create(AWidth,AHeight);
  orig := TVectorOriginal.Create;
  layerIndex := newImg.AddLayerFromOwnedOriginal(orig);
  SetImage(newImg);
  filename := '';
  UpdateTitleBar;
  vectorLayerIndex := layerIndex;
end;

procedure TForm1.SetImage(AImage: TBGRALazPaintImage);
begin
  FreeAndNil(img);
  FreeAndNil(FDiffList);
  FDiffListPos := 0;
  FDiffListSavePos := 0;
  FDiffAppend := false;
  img := AImage;
  img.OnOriginalEditingChange:= @OnEditingChange;
  img.EditorFocused:= BGRAVirtualScreen1.Focused;
  img.OnEditorFocusChanged:=@OnEditorFocusChange;
  img.OnOriginalChange:= @OnOriginalChange;
  FVectorLayerIndex:= -1;
  ImageChangesCompletely;
end;

procedure TForm1.AddDiff(AOriginal: TBGRALayerCustomOriginal;
  ADiff: TBGRAOriginalDiff);
const DiffMinDelay = 1000 / (1000*60*60*24);
begin
  if ADiff = nil then exit;
  if FDiffList=nil then FDiffList := TOriginalDiffList.Create;
  while FDiffList.Count > FDiffListPos do FDiffList.Delete(FDiffList.Count-1);
  if FDiffListSavePos > FDiffList.Count then FDiffListSavePos:= maxLongint;
  if (FDiffList.Count>0) and FDiffList[FDiffList.Count-1].CanAppend(ADiff) and
    (FDiffAppend or (Now < FDiffLastDate+DiffMinDelay)) then
  begin
    FDiffList[FDiffList.Count-1].Append(ADiff);
    ADiff.Free;
  end
  else
  begin
    FDiffListPos := FDiffList.Add(ADiff)+1;
    FDiffAppend:= [ssLeft,ssRight]*mouseState <> [];
  end;
  FDiffLastDate := Now;
  UpdateTitleBar;
end;

procedure TForm1.TextStyleClick(Sender: TObject);
var
  btn: TToolButton;
  s: TFontStyle;
begin
  btn := Sender as TToolButton;
  if Assigned(vectorOriginal) and Assigned(vectorOriginal.SelectedShape) and
    (vectorOriginal.SelectedShape is TTextShape) then
  begin
    FTextFontStyle := TTextShape(vectorOriginal.SelectedShape).FontStyle;
    s := TFontStyle(btn.Tag);
    Exclude(FTextFontStyle, s);
    if btn.Down then Include(FTextFontStyle, s);
    TTextShape(vectorOriginal.SelectedShape).FontStyle := FTextFontStyle;
  end;
end;

procedure TForm1.TextDirClick(Sender: TObject);
var
  btn: TToolButton;
begin
  btn := TToolButton(Sender);
  with btn.ClientToScreen(Point(0,btn.Height)) do
    FTextDirectionMenu.PopUp(X,Y);
end;

procedure TForm1.TextAlignClick(Sender: TObject);
var
  alignment: TAlignment;
begin
  alignment := TAlignment(TToolButton(Sender).Tag);
  FTextAlign:= AlignmentToBidiTextAlignment(alignment, FTextDirection=fbmRightToLeft);
  if not FUpdatingFromShape and Assigned(vectorOriginal) and Assigned(vectorOriginal.SelectedShape) then
  begin
    if vectorOriginal.SelectedShape is TTextShape then
    begin
      TTextShape(vectorOriginal.SelectedShape).ParagraphAlignment:= alignment;
      FTextAlign:= TTextShape(vectorOriginal.SelectedShape).BidiParagraphAlignment;
    end;
  end;
end;

procedure TForm1.UpdateViewCursor(ACursor: TOriginalEditorCursor);
begin
  case ACursor of
    oecDefault: BGRAVirtualScreen1.Cursor := crDefault;
    oecMove: BGRAVirtualScreen1.Cursor := crSizeAll;
    oecMoveN: BGRAVirtualScreen1.Cursor := crSizeN;
    oecMoveS: BGRAVirtualScreen1.Cursor := crSizeS;
    oecMoveE: BGRAVirtualScreen1.Cursor := crSizeE;
    oecMoveW: BGRAVirtualScreen1.Cursor := crSizeW;
    oecMoveNE: BGRAVirtualScreen1.Cursor := crSizeNE;
    oecMoveSW: BGRAVirtualScreen1.Cursor := crSizeSW;
    oecMoveNW: BGRAVirtualScreen1.Cursor := crSizeNW;
    oecMoveSE: BGRAVirtualScreen1.Cursor := crSizeSE;
    oecHandPoint: BGRAVirtualScreen1.Cursor := crHandPoint;
    oecText: BGRAVirtualScreen1.Cursor := crIBeam;
  end;
end;

procedure TForm1.RenderAndUpdate(ADraft: boolean);
const
  MsInDay = 1000*60*60*24;
  RenderDelayDateTime = RenderDelayMs/MsInDay;
var
  nowValue: TDateTime;
begin
  nowValue := Now;
  FNextRenderDraft := ADraft;
  if (FLastRenderDateTime = 0) or (nowValue - FLastRenderDateTime >= RenderDelayDateTime) then
  begin
    DoRenderAndUpdate;
    DelayedRenderTimer.Enabled := false;
  end
  else
  if not DelayedRenderTimer.Enabled then
  begin
    DelayedRenderTimer.Interval:= max(round(RenderDelayMs - (nowValue - FLastRenderDateTime)*MsInDay), 1);
    DelayedRenderTimer.Enabled := true;
  end;
end;

procedure TForm1.DoRenderAndUpdate;
var
  renderedRect: TRect;
begin
  renderedRect := img.RenderOriginalsIfNecessary(FNextRenderDraft);
  UpdateFlattenedImage(renderedRect);
  FLastRenderDateTime:= Now;
end;

procedure TForm1.FocusView;
begin
  if not BGRAVirtualScreen1.Focused then BGRAVirtualScreen1.SetFocus;
end;

procedure TForm1.UpdateFlattenedImage(ARect: TRect; AUpdateView: boolean);
var
  shapeRectF: TRectF;
  shapeRect: TRect;
begin
  img.FreezeExceptOneLayer(vectorLayerIndex);
  if FFlattened = nil then
    FFlattened := img.ComputeFlatImage
  else
  if not IsRectEmpty(ARect) then
  begin
    FFlattened.FillRect(ARect,BGRAPixelTransparent,dmSet);
    FFlattened.ClipRect := ARect;
    img.Draw(FFlattened, 0,0);
    FFlattened.NoClip;
  end;

  if Assigned(newShape) and not IsRectEmpty(ARect) then
  begin
    shapeRectF := newShape.GetRenderBounds(InfiniteRect, vectorTransform);
    with shapeRectF do
      shapeRect := rect(floor(Left),floor(Top),ceil(Right),ceil(Bottom));
    if IntersectRect(shapeRect, shapeRect, ARect) then
    begin
      FFlattened.ClipRect := shapeRect;
      newShape.Render(FFlattened, vectorTransform, newShape.GetIsSlow(vectorTransform));
      FFlattened.NoClip;
    end;
  end;

  if AUpdateView then
    UpdateView(ARect);
end;

procedure TForm1.UpdateView(AImageChangeRect: TRect);
var
  viewRectF: TRectF;
  viewRect, newEditorBounds: TRect;
begin
  if IsRectEmpty(AImageChangeRect) then
  begin
    viewRectF := EmptyRectF;
    viewRect := EmptyRect;
  end
  else
  begin
    with AImageChangeRect do
      viewRectF := RectF(zoom* PointF(Left,Top), zoom* PointF(Right,Bottom));
    viewRect := rect(floor(viewRectF.Left),floor(viewRectF.Top),ceil(viewRectF.Right),ceil(viewRectF.Bottom));
  end;

  if not IsRectEmpty(FLastEditorBounds) then
  begin
    if IsRectEmpty(viewRect) then viewRect := FLastEditorBounds else
      UnionRect(viewRect,viewRect,FLastEditorBounds);
  end;
  if Assigned(img) then
  begin
    if vectorLayerIndex<>-1 then
    begin
      newEditorBounds := img.GetEditorBounds(vectorLayerIndex, zoom, EditorPointSize);
      if not IsRectEmpty(newEditorBounds) then
      begin
        if IsRectEmpty(viewRect) then viewRect := newEditorBounds else
          UnionRect(viewRect,viewRect,newEditorBounds);
      end;
    end;
  end;

  if not IsRectEmpty(viewRect) then
  begin
    viewRect.Inflate(1,1);
    BGRAVirtualScreen1.RedrawBitmap(viewRect);
  end;
end;

procedure TForm1.UpdateToolbarFromShape(AShape: TVectorShape);
var
  f: TVectorShapeFields;
  nextControlPos: TPoint;
  mode: TVectorShapeUsermode;
  sk: TPhongShapeKind;
  btn: TToolButton;
  toolClass: TVectorShapeAny;
  alignment : TAlignment;
  tb: TEdit;
  fh: TBCTrackbarUpdown;
begin
  RemoveExtendedStyleControls;
  alignment := BidiTextAlignmentToAlignment(FTextAlign, FTextDirection=fbmRightToLeft);

  if AShape <> nil then
  begin
    FUpdatingFromShape := true;
    mode := AShape.Usermode;
    f := AShape.MultiFields;
    toolClass := TVectorShapeAny(AShape.ClassType);
    if vsfPenFill in f then PenFillControl.AssignFill(AShape.PenFill);
    if vsfPenWidth in f then penWidth:= AShape.PenWidth;
    if vsfPenStyle in f then penStyle:= AShape.PenStyle;
    if vsfJoinStyle in f then joinStyle:= AShape.JoinStyle;

    if vsfBackFill in f then BackFillControl.AssignFill(AShape.BackFill);
    if vsfOutlineFill in f then
    begin
      OutlineFillControl.AssignFill(AShape.OutlineFill);
      outlineWidth := AShape.OutlineWidth;
    end;

    if AShape is TCurveShape then
      splineStyle:= TCurveShape(AShape).SplineStyle;

    if AShape is TPhongShape then
    begin
      phongShapeKind:= TPhongShape(AShape).ShapeKind;
      FPhongShapeAltitude:= TPhongShape(AShape).ShapeAltitudePercent;
      FPhongBorderSize:= TPhongShape(AShape).BorderSizePercent;
    end;
    if AShape is TTextShape then
    begin
      FTextAlign:= TTextShape(AShape).BidiParagraphAlignment;
      alignment := TTextShape(AShape).ParagraphAlignment;
      FTextFontHeight:= TTextShape(AShape).FontEmHeight;
      FTextFontName:= TTextShape(AShape).FontName;
      FTextFontStyle:= TTextShape(AShape).FontStyle;
      FTextDirection:= TTextShape(AShape).FontBidiMode;
      FTextPenPhong:= TTextShape(AShape).PenPhong;
      FTextAltitudePercent:= TTextShape(AShape).AltitudePercent;
    end;

    FUpdatingFromShape := false;
    PanelPenFill.Visible := vsfPenFill in f;
    PanelBackFill.Visible := vsfBackFill in f;
    PanelOutlineFill.Visible := vsfOutlineFill in f;
  end else
  begin
    toolClass:= PaintToolClass[currentTool];
    mode := vsuEdit;
    if IsCreateShapeTool(currentTool) then
    begin
      f := PaintToolClass[currentTool].Fields;
      PanelPenFill.Visible := vsfPenFill in f;
      PanelBackFill.Visible := vsfBackFill in f;
      PanelOutlineFill.Visible := vsfOutlineFill in f;
    end
    else
    begin
      f := [];
      PanelPenFill.Visible := true;
      PanelBackFill.Visible := true;
      PanelOutlineFill.Visible := true;
    end;
  end;
  UpdateBackToolFillPoints;
  UpdatePenToolFillPoints;
  UpdateOutlineToolFillPoints;
  UpDownPenWidth.Enabled := vsfPenWidth in f;
  ButtonPenStyle.Enabled:= vsfPenStyle in f;
  EnableDisableToolButtons([ToolButtonJoinRound,ToolButtonJoinBevel,ToolButtonJoinMiter], vsfJoinStyle in f);
  PanelBasicStyle.Visible := [vsfPenWidth,vsfPenStyle,vsfJoinStyle]*f <> [];

  PanelExtendedStyle.Visible := false;
  nextControlPos := Point(1,1);

  if toolClass = TCurveShape then
  begin
    PanelExtendedStyle.Visible := true;

    FSplineToolbar := CreateToolBar(FVectorImageList);
    FSplineToolbar.Left := nextControlPos.X;
    FSplineToolbar.Top := nextControlPos.Y;
    FSplineToolbar.Wrapable := false;

    AddToolbarCheckButton(FSplineToolbar, 'Move spline points', 20, @SplineToolbarClick, mode in [vsuEdit, vsuCreate], true, ord(vsuEdit));
    AddToolbarCheckButton(FSplineToolbar, 'Set to autodetect angle (A)', 21, @SplineToolbarClick, mode = vsuCurveSetAuto, true, ord(vsuCurveSetAuto));
    AddToolbarCheckButton(FSplineToolbar, 'Set to smooth (S)', 22, @SplineToolbarClick, mode = vsuCurveSetCurve, true, ord(vsuCurveSetCurve));
    AddToolbarCheckButton(FSplineToolbar, 'Set to angle (X)', 23, @SplineToolbarClick, mode = vsuCurveSetAngle, true, ord(vsuCurveSetAngle)).Wrap:= true;

    FComboboxSplineStyle := TBCButton.Create(FSplineToolbar);
    FComboboxSplineStyle.Style := bbtButton;
    FComboboxSplineStyle.Caption:= SplineStyleToStr[splineStyle];
    FComboboxSplineStyle.Width := 4*FSplineToolbar.ButtonWidth;
    FComboboxSplineStyle.Height := FSplineToolbar.ButtonHeight;
    FComboboxSplineStyle.OnClick:=@ComboBoxSplineStyleClick;
    FComboboxSplineStyle.StateNormal.Assign(ButtonPenStyle.StateNormal);
    FComboboxSplineStyle.StateHover.Assign(ButtonPenStyle.StateHover);
    FComboboxSplineStyle.StateClicked.Assign(ButtonPenStyle.StateClicked);
    FComboboxSplineStyle.Rounding.Assign(ButtonPenStyle.Rounding);
    FComboboxSplineStyle.DropDownArrow:= ButtonPenStyle.DropDownArrow;
    FComboboxSplineStyle.DropDownArrowSize:= ButtonPenStyle.DropDownArrowSize;
    AddToolbarControl(FSplineToolbar, FComboboxSplineStyle);

    UpdateSplineToolbar;
    PanelExtendedStyle.InsertControl(FSplineToolbar);

    with GetToolbarSize(FSplineToolbar,0) do
    begin
      FSplineToolbar.Width := cx+1;
      FSplineToolbar.Height := cy+1;
    end;

    nextControlPos.X := FSplineToolbar.Left + FSplineToolbar.Width;
  end;

  if toolClass = TPhongShape then
  begin
    PanelExtendedStyle.Visible := true;

    FPhongShapeKindToolbar := CreateToolBar(FVectorImageList);
    FPhongShapeKindToolbar.Left := nextControlPos.X;
    FPhongShapeKindToolbar.Top := nextControlPos.Y;
    FPhongShapeKindToolbar.Wrapable := false;

    AddToolbarLabel(FPhongShapeKindToolbar, 'Shape', self);

    for sk := low(TPhongShapeKind) to high(TPhongShapeKind) do
    begin
      btn := AddToolbarCheckButton(FPhongShapeKindToolbar, PhongShapeKindToStr[sk], 13+ord(sk), @PhongShapeKindClick, FPhongShapeKind = sk, true, ord(sk));
      if sk = high(TPhongShapeKind) then btn.Wrap:= true;
    end;

    AddToolbarLabel(FPhongShapeKindToolbar, 'Altitude', self);
    AddToolbarUpdown(FPhongShapeKindToolbar, 'Altitude', 0, 100, round(FPhongShapeAltitude), @OnPhongShapeAltitudeChange);

    AddToolbarLabel(FPhongShapeKindToolbar, 'Border', self);
    FUpDownPhongBorderSize := AddToolbarUpdown(FPhongShapeKindToolbar, 'Border size', 0, 100, round(FPhongBorderSize), @OnPhongBorderSizeChange);
    FUpDownPhongBorderSize.Enabled:= (phongShapeKind in[pskRectangle,pskRoundRectangle]);

    PanelExtendedStyle.InsertControl(FPhongShapeKindToolbar);
    with GetToolbarSize(FPhongShapeKindToolbar,0) do
    begin
      FPhongShapeKindToolbar.Width := cx+1;
      FPhongShapeKindToolbar.Height := cy+1;
    end;

    nextControlPos.X := FPhongShapeKindToolbar.Left + FPhongShapeKindToolbar.Width;
  end;

  if toolClass = TTextShape then
  begin
    PanelExtendedStyle.Visible := true;

    FTextToolbar := CreateToolBar(FVectorImageList);
    FTextToolbar.Left := nextControlPos.X;
    FTextToolbar.Top := nextControlPos.Y;
    FTextToolbar.Wrapable := false;

    AddToolbarLabel(FTextToolbar, 'Font', self);
    tb := AddToolbarTextBox(FTextToolbar, 'Font familiy', FTextFontName, @TextFontTextBoxChange);
    tb.OnEnter:=@TextFontTextBoxEnter;
    tb.OnExit:=@TextFontTextBoxExit;
    //AddToolbarButton(FTextToolbar, 'Choose font', 25, @TextChooseFontClick);
    FTextDirectionButton := AddToolbarButton(FTextToolbar, 'Text direction', 31 + ord(FTextDirection), @TextDirClick);
    FTextAlignButton[taLeftJustify] := AddToolbarCheckButton(FTextToolbar, 'Left align', 26, @TextAlignClick, alignment = taLeftJustify, True, ord(taLeftJustify));
    FTextAlignButton[taCenter] := AddToolbarCheckButton(FTextToolbar, 'Center', 27, @TextAlignClick, alignment = taCenter, True, ord(taCenter));
    FTextAlignButton[taRightJustify] := AddToolbarCheckButton(FTextToolbar, 'Right align', 28, @TextAlignClick, alignment = taRightJustify, True, ord(taRightJustify));
    FTextAlignButton[taRightJustify].Wrap:=true;

    AddToolbarCheckButton(FTextToolbar, 'Bold', 34, @TextStyleClick, fsBold in FTextFontStyle, false, ord(fsBold));
    AddToolbarCheckButton(FTextToolbar, 'Italic', 35, @TextStyleClick, fsItalic in FTextFontStyle, false, ord(fsItalic));
    AddToolbarCheckButton(FTextToolbar, 'Underline', 36, @TextStyleClick, fsUnderline in FTextFontStyle, false, ord(fsUnderline));
    AddToolbarCheckButton(FTextToolbar, 'Strike-out', 37, @TextStyleClick, fsStrikeOut in FTextFontStyle, false, ord(fsStrikeOut));
    AddToolbarLabel(FTextToolbar, 'Height', self);
    fh := AddToolbarUpdown(FTextToolbar, 'Font height', 1, 900, round(FTextFontHeight), @OnTextFontHeightChange);
    fh.BarExponent:= 3;
    AddToolbarCheckButton(FTextToolbar, 'Phong lighting', 15, @TextPenPhongClick, FTextPenPhong, false);
    fh := AddToolbarUpdown(FTextToolbar, 'Altitude', 1, 100, round(FTextAltitudePercent), @OnTextAltitudePercentChange);
    fh.BarExponent:= 3;

    PanelExtendedStyle.InsertControl(FTextToolbar);
    with GetToolbarSize(FTextToolbar,0) do
    begin
      FTextToolbar.Width := cx+1;
      FTextToolbar.Height := cy+1;
    end;

    nextControlPos.X := FTextToolbar.Left + FTextToolbar.Width;
  end;

  PanelExtendedStyle.Width := nextControlPos.X+1;

  AdjustToolbarTop;
end;

procedure TForm1.UpdateTitleBar;
var
  modifStr: string;
begin
  if Assigned(FDiffList) and (FDiffListPos <> FDiffListSavePos) then
    modifStr := '*' else modifStr := '';
  if filename = '' then
    Caption := baseCaption + ' - New image'+modifStr+' - ' + inttostr(img.Width)+'x'+inttostr(img.Height)
  else
    Caption := baseCaption + ' - ' + filename + modifStr+' - ' + inttostr(img.Width)+'x'+inttostr(img.Height);
end;

procedure TForm1.ImageChangesCompletely;
begin
  FreeAndNil(FFlattened);
  BGRAVirtualScreen1.DiscardBitmap;
end;

function TForm1.CreateShape(const APoint1,APoint2: TPointF): TVectorShape;
var
  vectorFill: TVectorialFill;
begin
  if not IsCreateShapeTool(currentTool) then
    raise exception.Create('No shape type selected');
  result := PaintToolClass[currentTool].Create(vectorOriginal);
  if (result is TCustomPolypointShape) and (BackFillControl.FillType = vftGradient) then BackFillControl.FillType := vftSolid;
  if (result is TCustomPolypointShape) and (PenFillControl.FillType = vftGradient) then PenFillControl.FillType := vftSolid;
  if (result is TCustomPolypointShape) and (OutlineFillControl.FillType = vftGradient) then OutlineFillControl.FillType := vftSolid;
  result.PenWidth := penWidth;
  result.PenStyle := penStyle;
  if vsfJoinStyle in result.Fields then result.JoinStyle := joinStyle;
  if currentTool in[ptClosedCurve,ptPolygon] then
    TCustomPolypointShape(result).Closed := true;
  if result is TCurveShape then TCurveShape(result).SplineStyle:= splineStyle;
  if result is TPhongShape then
  begin
    with TPhongShape(result) do
    begin
      ShapeKind:= FPhongShapeKind;
      ShapeAltitudePercent:= FPhongShapeAltitude;
      BorderSizePercent:= FPhongBorderSize;
    end;
  end;
  if result is TTextShape then
  begin
    TTextShape(result).FontName:= FTextFontName;
    TTextShape(result).FontStyle:= FTextFontStyle;
    TTextShape(result).FontEmHeight:= FTextFontHeight;
    TTextShape(result).FontBidiMode:= FTextDirection;
    TTextShape(result).UserMode := vsuEditText;
  end;
  result.QuickDefine(APoint1,APoint2);
  if vsfBackFill in result.Fields then
  begin
    vectorFill := BackFillControl.CreateShapeFill(result);
    result.BackFill := vectorFill;
    vectorFill.Free;
  end;
  if vsfPenFill in result.Fields then
  begin
    vectorFill := PenFillControl.CreateShapeFill(result);
    result.PenFill := vectorFill;
    vectorFill.Free;
  end;
  if vsfOutlineFill in result.Fields then
  begin
    vectorFill := OutlineFillControl.CreateShapeFill(result);
    result.OutlineFill := vectorFill;
    vectorFill.Free;
    result.OutlineWidth:= outlineWidth;
  end;
end;

procedure TForm1.RemoveExtendedStyleControls;
var
  a: TAlignment;
begin
  if Assigned(FSplineToolbar) then
  begin
    PanelExtendedStyle.RemoveControl(FSplineToolbar);
    FreeAndNil(FSplineToolbar);
    FComboboxSplineStyle := nil;
  end;
  if Assigned(FPhongShapeKindToolbar) then
  begin
    PanelExtendedStyle.RemoveControl(FPhongShapeKindToolbar);
    FreeAndNil(FPhongShapeKindToolbar);
    FUpDownPhongBorderSize := nil;
  end;
  if Assigned(FTextToolbar) then
  begin
    PanelExtendedStyle.RemoveControl(FTextToolbar);
    FreeAndNil(FTextToolbar);
    FTextDirectionButton := nil;
    for a := low(TAlignment) to high(TAlignment) do
      FTextAlignButton[a] := nil;
  end;
end;

procedure TForm1.UpdateBackToolFillPoints;
var
  canEdit: Boolean;
begin
  canEdit := (BackFillControl.FillType in[vftGradient,vftTexture]) and
    Assigned(vectorOriginal) and Assigned(vectorOriginal.SelectedShape);
  ButtonMoveBackFillPoints.Enabled := canEdit;
  if (currentTool = ptMoveBackFillPoint) and not canEdit then currentTool:= ptHand;
end;

procedure TForm1.UpdatePenToolFillPoints;
var
  canEdit: Boolean;
begin
  canEdit := (PenFillControl.FillType in[vftGradient,vftTexture]) and
    Assigned(vectorOriginal) and Assigned(vectorOriginal.SelectedShape);
  ButtonMovePenFillPoints.Enabled := canEdit;
  if (currentTool = ptMovePenFillPoint) and not canEdit then currentTool:= ptHand;
end;

procedure TForm1.UpdateOutlineToolFillPoints;
var
  canEdit: Boolean;
begin
  canEdit := (OutlineFillControl.FillType in[vftGradient,vftTexture]) and
    Assigned(vectorOriginal) and Assigned(vectorOriginal.SelectedShape);
  ButtonMoveOutlineFillPoints.Enabled := canEdit;
  if (currentTool = ptMoveOutlineFillPoint) and not canEdit then currentTool:= ptHand;
end;

procedure TForm1.UpdateShapeBackFill;
begin
  if Assigned(vectorOriginal) and Assigned(vectorOriginal.SelectedShape) and
    (vsfBackFill in vectorOriginal.SelectedShape.MultiFields) then
    BackFillControl.UpdateShapeFill(vectorOriginal.SelectedShape, ftBack);
end;

procedure TForm1.UpdateShapePenFill;
begin
  if Assigned(vectorOriginal) and Assigned(vectorOriginal.SelectedShape) and
    (vsfPenFill in vectorOriginal.SelectedShape.MultiFields) then
    PenFillControl.UpdateShapeFill(vectorOriginal.SelectedShape, ftPen);
end;

procedure TForm1.UpdateShapeOutlineFill;
begin
  if Assigned(vectorOriginal) and Assigned(vectorOriginal.SelectedShape) and
    (vsfOutlineFill in vectorOriginal.SelectedShape.MultiFields) then
    OutlineFillControl.UpdateShapeFill(vectorOriginal.SelectedShape, ftOutline);
end;

procedure TForm1.UpdateShapeUserMode;
begin
  if Assigned(vectorOriginal) and Assigned(vectorOriginal.SelectedShape) then
  begin
    if (currentTool = ptMoveBackFillPoint) and
       (vsfBackFill in vectorOriginal.SelectedShape.MultiFields) and
       vectorOriginal.SelectedShape.BackFill.IsEditable then
    begin
      if vectorOriginal.SelectedShape.Usermode <> vsuEditBackFill then
        vectorOriginal.SelectedShape.Usermode := vsuEditBackFill;
    end else
    if (currentTool = ptMovePenFillPoint) and
       (vsfPenFill in vectorOriginal.SelectedShape.MultiFields) and
       vectorOriginal.SelectedShape.PenFill.IsEditable then
    begin
      if vectorOriginal.SelectedShape.Usermode <> vsuEditPenFill then
        vectorOriginal.SelectedShape.Usermode := vsuEditPenFill;
    end else
    if (currentTool = ptMoveOutlineFillPoint) and
       (vsfOutlineFill in vectorOriginal.SelectedShape.MultiFields) and
       vectorOriginal.SelectedShape.OutlineFill.IsEditable then
    begin
      if vectorOriginal.SelectedShape.Usermode <> vsuEditOutlineFill then
        vectorOriginal.SelectedShape.Usermode := vsuEditOutlineFill;
    end else
    begin
      if vectorOriginal.SelectedShape.Usermode in[vsuEditPenFill,vsuEditBackFill,vsuEditOutlineFill] then
        vectorOriginal.SelectedShape.Usermode := vsuEdit;
    end;
  end;
end;

procedure TForm1.UpdateShapeActions(AShape: TVectorShape);
begin
  ShapeBringToFront.Enabled := (AShape <> nil) and not AShape.IsFront;
  ShapeSendToBack.Enabled := (AShape <> nil) and not AShape.IsBack;
  ShapeMoveUp.Enabled := (AShape <> nil) and not AShape.IsFront;
  ShapeMoveDown.Enabled := (AShape <> nil) and not ASHape.IsBack;
  EditCopy.Enabled := AShape <> nil;
  EditCut.Enabled := AShape <> nil;
  EditDelete.Enabled := AShape <> nil;
  BackFillControl.CanAdjustToShape := AShape <> nil;
  PenFillControl.CanAdjustToShape := AShape <> nil;
  OutlineFillControl.CanAdjustToShape := AShape <> nil;
end;

procedure TForm1.RemoveShapeIfEmpty(AShape: TVectorShape);
var
  rF: TRectF;
begin
  if FInRemoveShapeIfEmpty then exit;
  FInRemoveShapeIfEmpty := true;
  if (AShape <> nil) and not AShape.IsRemoving and
     (AShape.GetAsMultishape = nil) then
  begin
    rF := AShape.GetRenderBounds(InfiniteRect, vectorTransform);
    if IsEmptyRectF(rF) then
    begin
      vectorOriginal.RemoveShape(AShape);
      ShowMessage('Shape is empty and has been deleted');
    end else
    if not rF.IntersectsWith(RectF(0,0,img.Width,img.Height)) then
    begin
      vectorOriginal.RemoveShape(AShape);
      ShowMessage('Shape is outside of picture and has been deleted');
    end;
  end;
  FInRemoveShapeIfEmpty := false;
end;

function TForm1.VirtualScreenToImgCoord(X, Y: Integer): TPointF;
begin
  result := AffineMatrixTranslation(-0.5,-0.5)*AffineMatrixInverse(zoom)*AffineMatrixTranslation(0.5,0.5)*PointF(X,Y);
end;

procedure TForm1.SetEditorGrid(AActive: boolean);
var pixelCentered: boolean;
begin
  if Assigned(img) and Assigned(img.OriginalEditor) then
  begin
    if Assigned(vectorOriginal) and Assigned(vectorOriginal.SelectedShape) then
      pixelCentered:= vectorOriginal.SelectedShape.PreferPixelCentered
    else if PaintToolClass[currentTool]<>nil then
      pixelCentered:= PaintToolClass[currentTool].PreferPixelCentered;

    if zoomFactor >= 2.1 then
      img.OriginalEditor.GridMatrix := AffineMatrixTranslation(-0.5,-0.5)*
                                       vectorTransform*AffineMatrixTranslation(0.5,0.5)*
                                       AffineMatrixScale(0.5,0.5)
    else
    if pixelCentered then
      img.OriginalEditor.GridMatrix := AffineMatrixTranslation(-0.5,-0.5)*
                                       vectorTransform*AffineMatrixTranslation(0.5,0.5)
    else
      img.OriginalEditor.GridMatrix := AffineMatrixTranslation(-0.5,-0.5)*vectorTransform;

    img.OriginalEditor.GridActive := AActive or (zoomFactor < 2.1);
  end;
end;

procedure TForm1.RequestBackFillAdjustToShape(Sender: TObject);
var
  vectorFill: TVectorialFill;
begin
  if Assigned(vectorOriginal) and Assigned(vectorOriginal.SelectedShape) and
     (vsfBackFill in vectorOriginal.SelectedShape.MultiFields) then
  begin
    vectorFill := BackFillControl.CreateShapeFill(vectorOriginal.SelectedShape);
    vectorOriginal.SelectedShape.BackFill := vectorFill;
    vectorFill.Free;
  end;
end;

procedure TForm1.PenFillControlResize(Sender: TObject);
begin
  PanelPenFill.ClientWidth := PanelPenFillHead.Width+PenFillControl.Width+2;
end;

procedure TForm1.RequestPenFillAdjustToShape(Sender: TObject);
var
  vectorFill: TVectorialFill;
begin
  if Assigned(vectorOriginal) and Assigned(vectorOriginal.SelectedShape) and
     (vsfPenFill in vectorOriginal.SelectedShape.MultiFields) then
  begin
    vectorFill := PenFillControl.CreateShapeFill(vectorOriginal.SelectedShape);
    vectorOriginal.SelectedShape.PenFill := vectorFill;
    vectorFill.Free;
  end;
end;

procedure TForm1.RequestPenFillUpdate(Sender: TObject);
begin
  if not FUpdatingFromShape then
  begin
    UpdateShapePenFill;
    UpdatePenToolFillPoints;
  end;
end;

procedure TForm1.RequestOutlineFillAdjustToShape(Sender: TObject);
var
  vectorFill: TVectorialFill;
begin
  if Assigned(vectorOriginal) and Assigned(vectorOriginal.SelectedShape) and
     (vsfOutlineFill in vectorOriginal.SelectedShape.MultiFields) then
  begin
    vectorFill := OutlineFillControl.CreateShapeFill(vectorOriginal.SelectedShape);
    vectorOriginal.SelectedShape.OutlineFill := vectorFill;
    vectorFill.Free;
  end;
end;

procedure TForm1.AdjustToolbarTop;
begin
  ReorderToolbarContent(ToolbarTop);

  ToolBarTop.Height := GetToolbarSize(ToolBarTop,0).cy;
  BCPanelToolbar.Height := ToolBarTop.Height;
end;

procedure TForm1.UpdateSplineToolbar;
var
  i: Integer;
begin
  for i := 0 to FSplineToolbar.ButtonCount-1 do
  begin
    if FSplineStyle = ssEasyBezier then
    begin
      FSplineToolbar.Buttons[i].Enabled:= true;
    end else
    begin
      if FSplineToolbar.Buttons[i].Tag = ord(vsuEdit) then
      begin
        if not FSplineToolbar.Buttons[i].Down then
        begin
          FSplineToolbar.Buttons[i].Down := true;
          FSplineToolbar.Buttons[i].OnClick(FSplineToolbar.Buttons[i]);
        end;
      end
      else
        FSplineToolbar.Buttons[i].Enabled:= false;
    end;
  end;
end;

function TForm1.SnapToGrid(APoint: TPointF): TPointF;
begin
  if Assigned(img) and Assigned(img.OriginalEditor) and img.OriginalEditor.GridActive then
    result := img.OriginalEditor.SnapToGrid(APoint, false)
  else
    result := APoint;
end;

function TForm1.ImgCoordToOriginalCoord(APoint: TPointF): TPointF;
begin
  if Assigned(img) and Assigned(img.OriginalEditor) and img.OriginalEditor.GridActive then
    result := SnapToGrid(AffineMatrixInverse(vectorTransform)*APoint)
  else
    result := SnapToGrid(AffineMatrixInverse(AffineMatrixTranslation(-0.5,-0.5)*vectorTransform*AffineMatrixTranslation(0.5,0.5))*APoint);
end;

procedure TForm1.DoCopy;
begin
  if Assigned(vectorOriginal) and Assigned(vectorOriginal.SelectedShape) then
    CopyShapesToClipboard([vectorOriginal.SelectedShape], vectorTransform);
end;

procedure TForm1.DoCut;
begin
  if Assigned(vectorOriginal) and Assigned(vectorOriginal.SelectedShape) then
  begin
    if CopyShapesToClipboard([vectorOriginal.SelectedShape], vectorTransform) then
      vectorOriginal.SelectedShape.Remove;
  end;
end;

procedure TForm1.DoPaste;
begin
  if Assigned(vectorOriginal) then
    PasteShapesFromClipboard(vectorOriginal, vectorTransform, rectF(0,0,img.Width,img.Height));
end;

procedure TForm1.DoDelete;
begin
  if Assigned(vectorOriginal) and Assigned(vectorOriginal.SelectedShape) then
    vectorOriginal.SelectedShape.Remove;
end;

procedure TForm1.DoUndo;
var
  diff: TBGRAOriginalDiff;
begin
  if FDiffListPos <= 0 then exit;
  dec(FDiffListPos);
  diff := FDiffList[FDiffListPos];
  diff.Unapply(vectorOriginal);
  UpdateTitleBar;
end;

procedure TForm1.DoRedo;
var
  diff: TBGRAOriginalDiff;
begin
  diff := FDiffList[FDiffListPos];
  diff.Apply(vectorOriginal);
  inc(FDiffListPos);
  UpdateTitleBar;
end;

end.

