unit LCVectorOriginal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRALayerOriginal, fgl, BGRAGradientOriginal, BGRABitmapTypes,
  BGRAPen, LCVectorialFill;

var
  LightPositionCaption : string = 'Light position';

const
  InfiniteRect : TRect = (Left: -MaxLongInt; Top: -MaxLongInt; Right: MaxLongInt; Bottom: MaxLongInt);
  EmptyTextureId = 0;
  DefaultShapeOutlineWidth = 2;

type
  TVectorOriginal = class;
  ArrayOfBGRABitmap = array of TBGRABitmap;

  TShapeChangeEvent = procedure(ASender: TObject; ABounds: TRectF) of object;
  TShapeEditingChangeEvent = procedure(ASender: TObject) of object;

  TRenderBoundsOption = (rboAssumePenFill, rboAssumeBackFill);
  TRenderBoundsOptions = set of TRenderBoundsOption;
  TVectorShapeField = (vsfPenFill, vsfPenWidth, vsfPenStyle, vsfJoinStyle, vsfBackFill, vsfOutlineFill);
  TVectorShapeFields = set of TVectorShapeField;
  TVectorShapeUsermode = (vsuEdit, vsuCreate, vsuEditPenFill, vsuEditBackFill, vsuEditOutlineFill,
                          vsuCurveSetAuto, vsuCurveSetCurve, vsuCurveSetAngle,
                          vsuEditText);
  TVectorShapeUsermodes = set of TVectorShapeUsermode;

  { TVectorShape }

  TVectorShape = class
  private
    FId: integer;
    FRenderIteration: integer; // increased at each BeginUpdate
    FOnChange: TShapeChangeEvent;
    FOnEditingChange: TShapeEditingChangeEvent;
    FUpdateCount, FUpdateEditingCount: integer;
    FBoundsBeforeUpdate: TRectF;
    FPenFill, FBackFill, FOutlineFill: TVectorialFill;
    FPenWidth: single;
    FOutlineWidth: single;
    FStroker: TBGRAPenStroker;
    FUsermode: TVectorShapeUsermode;
    FContainer: TVectorOriginal;
    FRemoving: boolean;
    function GetIsBack: boolean;
    function GetIsFront: boolean;
    procedure SetContainer(AValue: TVectorOriginal);
    function GetFill(var AFillVariable: TVectorialFill): TVectorialFill;
    procedure SetFill(var AFillVariable: TVectorialFill; AValue: TVectorialFill);
    procedure SetId(AValue: integer);
    procedure SetOutlineWidth(AValue: single);
  protected
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure BeginEditingUpdate;
    procedure EndEditingUpdate;
    procedure DoOnChange(ABoundsBefore: TRectF); virtual;
    function GetPenColor: TBGRAPixel; virtual;
    function GetPenWidth: single; virtual;
    function GetPenStyle: TBGRAPenStyle; virtual;
    function GetJoinStyle: TPenJoinStyle;
    function GetBackFill: TVectorialFill; virtual;
    function GetPenFill: TVectorialFill; virtual;
    function GetOutlineFill: TVectorialFill; virtual;
    procedure SetPenColor(AValue: TBGRAPixel); virtual;
    procedure SetPenWidth(AValue: single); virtual;
    procedure SetPenStyle({%H-}AValue: TBGRAPenStyle); virtual;
    procedure SetJoinStyle(AValue: TPenJoinStyle);
    procedure SetBackFill(AValue: TVectorialFill); virtual;
    procedure SetPenFill(AValue: TVectorialFill); virtual;
    procedure SetOutlineFill(AValue: TVectorialFill); virtual;
    procedure SetUsermode(AValue: TVectorShapeUsermode); virtual;
    procedure LoadFill(AStorage: TBGRACustomOriginalStorage; AObjectName: string; var AValue: TVectorialFill);
    procedure SaveFill(AStorage: TBGRACustomOriginalStorage; AObjectName: string; AValue: TVectorialFill);
    function ComputeStroke(APoints: ArrayOfTPointF; AClosed: boolean; AStrokeMatrix: TAffineMatrix): ArrayOfTPointF;
    function GetStroker: TBGRAPenStroker;
    property Stroker: TBGRAPenStroker read GetStroker;
    procedure FillChange({%H-}ASender: TObject); virtual;
    procedure UpdateRenderStorage(ARenderBounds: TRect; AImage: TBGRACustomBitmap = nil);
    procedure DiscardRenderStorage;
    procedure RetrieveRenderStorage(AMatrix: TAffineMatrix; out ARenderBounds: TRect; out AImage: TBGRABitmap);
    function CanHaveRenderStorage: boolean;
  public
    constructor Create(AContainer: TVectorOriginal); virtual;
    class function CreateFromStorage(AStorage: TBGRACustomOriginalStorage; AContainer: TVectorOriginal): TVectorShape;
    destructor Destroy; override;
    procedure QuickDefine(const APoint1,APoint2: TPointF); virtual; abstract;
    //one of the two Render functions must be overriden
    procedure Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix; ADraft: boolean); virtual;
    procedure Render(ADest: TBGRABitmap; ARenderOffset: TPoint; AMatrix: TAffineMatrix; ADraft: boolean); virtual;
    function GetRenderBounds(ADestRect: TRect; AMatrix: TAffineMatrix; AOptions: TRenderBoundsOptions = []): TRectF; virtual; abstract;
    function SuggestGradientBox(AMatrix: TAffineMatrix): TAffineBox; virtual;
    function PointInShape(APoint: TPointF): boolean; virtual; abstract;
    procedure ConfigureCustomEditor(AEditor: TBGRAOriginalEditor); virtual; abstract;
    procedure ConfigureEditor(AEditor: TBGRAOriginalEditor); virtual;
    procedure LoadFromStorage(AStorage: TBGRACustomOriginalStorage); virtual;
    procedure SaveToStorage(AStorage: TBGRACustomOriginalStorage); virtual;
    procedure MouseMove({%H-}Shift: TShiftState; {%H-}X, {%H-}Y: single; var {%H-}ACursor: TOriginalEditorCursor; var {%H-}AHandled: boolean); virtual;
    procedure MouseDown({%H-}RightButton: boolean; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: single; var {%H-}ACursor: TOriginalEditorCursor; var {%H-}AHandled: boolean); virtual;
    procedure MouseUp({%H-}RightButton: boolean; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: single; var {%H-}ACursor: TOriginalEditorCursor; var {%H-}AHandled: boolean); virtual;
    procedure KeyDown({%H-}Shift: TShiftState; {%H-}Key: TSpecialKey; var {%H-}AHandled: boolean); virtual;
    procedure KeyUp({%H-}Shift: TShiftState; {%H-}Key: TSpecialKey; var {%H-}AHandled: boolean); virtual;
    procedure KeyPress({%H-}UTF8Key: string; var {%H-}AHandled: boolean); virtual;
    procedure BringToFront;
    procedure SendToBack;
    procedure MoveUp(APassNonIntersectingShapes: boolean);
    procedure MoveDown(APassNonIntersectingShapes: boolean);
    procedure Remove;
    function Duplicate: TVectorShape;
    class function StorageClassName: RawByteString; virtual; abstract;
    function GetIsSlow({%H-}AMatrix: TAffineMatrix): boolean; virtual;
    function GetUsedTextures: ArrayOfBGRABitmap;
    class function Fields: TVectorShapeFields; virtual;
    class function Usermodes: TVectorShapeUsermodes; virtual;
    class function PreferPixelCentered: boolean; virtual;
    class function CreateEmpty: boolean; virtual; //create shape even if empty?
    property OnChange: TShapeChangeEvent read FOnChange write FOnChange;
    property OnEditingChange: TShapeEditingChangeEvent read FOnEditingChange write FOnEditingChange;
    property PenColor: TBGRAPixel read GetPenColor write SetPenColor;
    property PenFill: TVectorialFill read GetPenFill write SetPenFill;
    property BackFill: TVectorialFill read GetBackFill write SetBackFill;
    property OutlineFill: TVectorialFill read GetOutlineFill write SetOutlineFill;
    property PenWidth: single read GetPenWidth write SetPenWidth;
    property PenStyle: TBGRAPenStyle read GetPenStyle write SetPenStyle;
    property OutlineWidth: single read FOutlineWidth write SetOutlineWidth;
    property JoinStyle: TPenJoinStyle read GetJoinStyle write SetJoinStyle;
    property Usermode: TVectorShapeUsermode read FUsermode write SetUsermode;
    property Container: TVectorOriginal read FContainer write SetContainer;
    property IsFront: boolean read GetIsFront;
    property IsBack: boolean read GetIsBack;
    property IsRemoving: boolean read FRemoving;
    property Id: integer read FId write SetId;
  end;
  TVectorShapes = specialize TFPGList<TVectorShape>;
  TVectorShapeAny = class of TVectorShape;

  TVectorOriginalSelectShapeEvent = procedure(ASender: TObject; AShape: TVectorShape; APreviousShape: TVectorShape) of object;

  TVectorOriginalEditor = class;

  { TVectorOriginal }

  TVectorOriginal = class(TBGRALayerCustomOriginal)
  private
    function GetShape(AIndex: integer): TVectorShape;
  protected
    FShapes: TVectorShapes;
    FDeletedShapes: TVectorShapes;
    FSelectedShape: TVectorShape;
    FFrozenShapesUnderSelection,
    FFrozenShapesOverSelection: TBGRABitmap;
    FFrozenShapesRenderOffset: TPoint;
    FFrozenShapesComputed: boolean;
    FFrozenShapeMatrix: TAffineMatrix;
    FOnSelectShape: TVectorOriginalSelectShapeEvent;
    FTextures: array of record
                 Bitmap: TBGRABitmap;
                 Id, Counter: integer;
               end;
    FTextureCount: integer;
    FLastTextureId: integer;
    FLastShapeId: integer;
    procedure FreeDeletedShapes;
    procedure OnShapeChange(ASender: TObject; ABounds: TRectF);
    procedure OnShapeEditingChange({%H-}ASender: TObject);
    procedure DiscardFrozenShapes;
    function GetTextureId(ABitmap: TBGRABitmap): integer;
    function IndexOfTexture(AId: integer): integer;
    procedure AddTextureWithId(ATexture: TBGRABitmap; AId: integer);
    procedure ClearTextures;
    function GetShapeCount: integer;
    function OpenShapeRenderStorage(AShapeIndex: integer; ACreate: boolean): TBGRACustomOriginalStorage;
    function FindShapeById(AId: integer): TVectorShape;
    procedure DiscardUnusedRenderStorage;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear;
    function AddTexture(ATexture: TBGRABitmap): integer;
    function GetTexture(AId: integer): TBGRABitmap;
    procedure DiscardUnusedTextures;
    function AddShape(AShape: TVectorShape): integer; overload;
    function AddShape(AShape: TVectorShape; AUsermode: TVectorShapeUsermode): integer; overload;
    function RemoveShape(AShape: TVectorShape): boolean;
    procedure SelectShape(AIndex: integer); overload;
    procedure SelectShape(AShape: TVectorShape); overload;
    procedure DeselectShape;
    procedure MouseClick(APoint: TPointF);
    procedure Render(ADest: TBGRABitmap; ARenderOffset: TPoint; AMatrix: TAffineMatrix; ADraft: boolean); override;
    procedure ConfigureEditor(AEditor: TBGRAOriginalEditor); override;
    function CreateEditor: TBGRAOriginalEditor; override;
    function GetRenderBounds(ADestRect: TRect; {%H-}AMatrix: TAffineMatrix): TRect; override;
    procedure LoadFromStorage(AStorage: TBGRACustomOriginalStorage); override;
    procedure SaveToStorage(AStorage: TBGRACustomOriginalStorage); override;
    function IndexOfShape(AShape: TVectorShape): integer;
    procedure MoveShapeToIndex(AFromIndex: integer; AToIndex: integer);
    class function StorageClassName: RawByteString; override;
    property OnSelectShape: TVectorOriginalSelectShapeEvent read FOnSelectShape write FOnSelectShape;
    property SelectedShape: TVectorShape read FSelectedShape;
    property ShapeCount: integer read GetShapeCount;
    property Shape[AIndex: integer]: TVectorShape read GetShape;
  end;

  { TVectorOriginalEditor }

  TVectorOriginalEditor = class(TBGRAOriginalEditor)
  protected
    FOriginal: TVectorOriginal;
    FLabels: array of record
      Coord: TPointF;
      Text: string;
      HorizAlign: TAlignment;
      VertAlign: TTextLayout;
      Padding: integer;
    end;
    function NiceText(ADest: TBGRABitmap; x, y: integer; const ALayoutRect: TRect;
                      AText: string; AHorizAlign: TAlignment; AVertAlign: TTextLayout;
                      APadding: integer): TRect;
  public
    constructor Create(AOriginal: TVectorOriginal);
    procedure Clear; override;
    function Render(ADest: TBGRABitmap; const ALayoutRect: TRect): TRect; override;
    function GetRenderBounds(const ALayoutRect: TRect): TRect; override;
    procedure AddLabel(const ACoord: TPointF; AText: string; AHorizAlign: TAlignment; AVertAlign: TTextLayout);
    procedure AddLabel(APointIndex: integer; AText: string; AHorizAlign: TAlignment; AVertAlign: TTextLayout);
    procedure MouseMove(Shift: TShiftState; ViewX, ViewY: single; out ACursor: TOriginalEditorCursor; out AHandled: boolean); override;
    procedure MouseDown(RightButton: boolean; Shift: TShiftState; ViewX, ViewY: single; out ACursor: TOriginalEditorCursor; out AHandled: boolean); override;
    procedure MouseUp(RightButton: boolean; {%H-}Shift: TShiftState; {%H-}ViewX, {%H-}ViewY: single; out ACursor: TOriginalEditorCursor; out AHandled: boolean); override;
    procedure KeyDown(Shift: TShiftState; Key: TSpecialKey; out AHandled: boolean); override;
    procedure KeyUp(Shift: TShiftState; Key: TSpecialKey; out AHandled: boolean); override;
    procedure KeyPress(UTF8Key: string; out AHandled: boolean); override;
  end;

function MatrixForPixelCentered(const AMatrix: TAffineMatrix): TAffineMatrix;

procedure RegisterVectorShape(AClass: TVectorShapeAny);
function GetVectorShapeByStorageClassName(AName: string): TVectorShapeAny;

implementation

uses math, BGRATransform, BGRAFillInfo, BGRAGraphics, BGRAPath, Types,
  BGRAText, BGRATextFX;

function MatrixForPixelCentered(const AMatrix: TAffineMatrix): TAffineMatrix;
begin
  result := AffineMatrixTranslation(-0.5,-0.5) * AMatrix * AffineMatrixTranslation(0.5,0.5);
end;

var
  VectorShapeClasses: array of TVectorShapeAny;

function GetVectorShapeByStorageClassName(AName: string): TVectorShapeAny;
var
  i: Integer;
begin
  for i := 0 to high(VectorShapeClasses) do
    if VectorShapeClasses[i].StorageClassName = AName then exit(VectorShapeClasses[i]);
  exit(nil);
end;

procedure RegisterVectorShape(AClass: TVectorShapeAny);
var
  i: Integer;
begin
  for i := 0 to high(VectorShapeClasses) do
    if VectorShapeClasses[i]=AClass then exit;
  if Assigned(GetVectorShapeByStorageClassName(AClass.StorageClassName)) then
    raise exception.Create('Duplicate class name "'+AClass.StorageClassName+'" for vector shape');
  setlength(VectorShapeClasses, length(VectorShapeClasses)+1);
  VectorShapeClasses[high(VectorShapeClasses)] := AClass;
end;

{ TVectorOriginalEditor }

constructor TVectorOriginalEditor.Create(AOriginal: TVectorOriginal);
begin
  inherited Create;
  FOriginal := AOriginal;
end;

procedure TVectorOriginalEditor.Clear;
begin
  inherited Clear;
  FLabels:= nil;
end;

function TVectorOriginalEditor.Render(ADest: TBGRABitmap;
  const ALayoutRect: TRect): TRect;
var
  i: Integer;
  ptF: TPointF;
  r: Classes.TRect;
begin
  Result:=inherited Render(ADest, ALayoutRect);
  for i := 0 to high(FLabels) do
    if not isEmptyPointF(FLabels[i].Coord) then
    begin
      ptF := OriginalCoordToView(FLabels[i].Coord);
      r := NiceText(ADest, round(ptF.x),round(ptF.y), ALayoutRect, FLabels[i].Text, FLabels[i].HorizAlign, FLabels[i].VertAlign, FLabels[i].Padding);
      if not IsRectEmpty(r) then
      begin
        if IsRectEmpty(result) then result:= r
        else UnionRect(result, result, r);
      end;
    end;
end;

function TVectorOriginalEditor.GetRenderBounds(const ALayoutRect: TRect): TRect;
var
  i: Integer;
  ptF: TPointF;
  r: Classes.TRect;
begin
  Result:=inherited GetRenderBounds(ALayoutRect);
  for i := 0 to high(FLabels) do
    if not isEmptyPointF(FLabels[i].Coord) then
    begin
      ptF := OriginalCoordToView(FLabels[i].Coord);
      r := NiceText(nil, round(ptF.x),round(ptF.y), ALayoutRect, FLabels[i].Text, FLabels[i].HorizAlign, FLabels[i].VertAlign, FLabels[i].Padding);
      if not IsRectEmpty(r) then
      begin
        if IsRectEmpty(result) then result:= r
        else UnionRect(result, result, r);
      end;
    end;
end;

procedure TVectorOriginalEditor.AddLabel(const ACoord: TPointF; AText: string;
  AHorizAlign: TAlignment; AVertAlign: TTextLayout);
begin
  setlength(FLabels, length(FLabels)+1);
  with FLabels[high(FLabels)] do
  begin
    Coord := ACoord;
    Text:= AText;
    HorizAlign:= AHorizAlign;
    VertAlign:= AVertAlign;
    Padding := 0;
  end;
end;

procedure TVectorOriginalEditor.AddLabel(APointIndex: integer; AText: string;
  AHorizAlign: TAlignment; AVertAlign: TTextLayout);
begin
  setlength(FLabels, length(FLabels)+1);
  with FLabels[high(FLabels)] do
  begin
    Coord := PointCoord[APointIndex];
    Text:= AText;
    HorizAlign:= AHorizAlign;
    VertAlign:= AVertAlign;
    Padding := round(PointSize);
  end;
end;

function TVectorOriginalEditor.NiceText(ADest: TBGRABitmap; x, y: integer;
      const ALayoutRect: TRect; AText: string; AHorizAlign: TAlignment;
      AVertAlign: TTextLayout; APadding: integer): TRect;
var fx: TBGRATextEffect;
    f: TFont;
    previousClip: TRect;
    shadowRadius: integer;
begin
  f := TFont.Create;
  f.Name := 'default';
  f.Height := round(PointSize*2.5);
  fx := TBGRATextEffect.Create(AText,f,true);

  if (AVertAlign = tlTop) and (AHorizAlign = taCenter) and (y+APadding+fx.TextSize.cy > ALayoutRect.Bottom) then AVertAlign:= tlBottom;
  if (AVertAlign = tlBottom) and (AHorizAlign = taCenter) and (y-APadding-fx.TextSize.cy < ALayoutRect.Top) then AVertAlign:= tlTop;
  if (AHorizAlign = taLeftJustify) and (AVertAlign = tlCenter) and (x+APadding+fx.TextSize.cx > ALayoutRect.Right) then AHorizAlign:= taRightJustify;
  if (AHorizAlign = taRightJustify) and (AVertAlign = tlCenter) and (x-APadding-fx.TextSize.cx < ALayoutRect.Left) then AHorizAlign:= taLeftJustify;

  if AVertAlign = tlBottom then y := y-APadding-fx.TextSize.cy else
  if AVertAlign = tlCenter then y := y-fx.TextSize.cy div 2 else inc(y,APadding);
  if y+fx.TextSize.cy > ALayoutRect.Bottom then y := ALayoutRect.Bottom-fx.TextSize.cy;
  if y < ALayoutRect.Top then y := ALayoutRect.Top;

  if AHorizAlign = taRightJustify then x := x-APadding-fx.TextSize.cx else
  if AHorizAlign = taCenter then x := x-fx.TextSize.cx div 2 else inc(x,APadding);
  if x+fx.TextSize.cx > ALayoutRect.Right then x := ALayoutRect.Right-fx.TextSize.cx;
  if x < ALayoutRect.Left then x := ALayoutRect.Left;

  shadowRadius:= round(PointSize*0.5);
  result := rect(x,y,x+fx.TextWidth+2*shadowRadius,y+fx.TextHeight+2*shadowRadius);
  if Assigned(ADest) then
  begin
    previousClip := ADest.ClipRect;
    ADest.ClipRect := result;
    if shadowRadius <> 0 then
      fx.DrawShadow(ADest,x+shadowRadius,y+shadowRadius,shadowRadius,BGRABlack);
    fx.DrawOutline(ADest,x,y,BGRABlack);
    fx.Draw(ADest,x,y,BGRAWhite);
    ADest.ClipRect := previousClip;
  end;
  fx.Free;
  f.Free;
end;

procedure TVectorOriginalEditor.MouseMove(Shift: TShiftState; ViewX, ViewY: single; out
  ACursor: TOriginalEditorCursor; out AHandled: boolean);
var
  ptF: TPointF;
begin
  inherited MouseMove(Shift, ViewX, ViewY, ACursor, AHandled);
  if not AHandled and Assigned(FOriginal) and Assigned(FOriginal.SelectedShape) then
  begin
    ptF := ViewCoordToOriginal(PointF(ViewX,ViewY));
    if GridActive then ptF := SnapToGrid(ptF, False);
    with ptF do FOriginal.SelectedShape.MouseMove(Shift, X,Y, ACursor, AHandled);
  end;
end;

procedure TVectorOriginalEditor.MouseDown(RightButton: boolean;
  Shift: TShiftState; ViewX, ViewY: single; out ACursor: TOriginalEditorCursor; out
  AHandled: boolean);
var
  ptF: TPointF;
begin
  inherited MouseDown(RightButton, Shift, ViewX, ViewY, ACursor, AHandled);
  if not AHandled and Assigned(FOriginal) and Assigned(FOriginal.SelectedShape) then
  begin
    ptF := ViewCoordToOriginal(PointF(ViewX,ViewY));
    if GridActive then ptF := SnapToGrid(ptF, False);
    with ptF do FOriginal.SelectedShape.MouseDown(RightButton, Shift, X,Y, ACursor, AHandled);
  end;
end;

procedure TVectorOriginalEditor.MouseUp(RightButton: boolean;
  Shift: TShiftState; ViewX, ViewY: single; out ACursor: TOriginalEditorCursor; out
  AHandled: boolean);
var
  ptF: TPointF;
begin
  inherited MouseUp(RightButton, Shift, ViewX, ViewY, ACursor, AHandled);
  if not AHandled and Assigned(FOriginal) and Assigned(FOriginal.SelectedShape) then
  begin
    ptF := ViewCoordToOriginal(PointF(ViewX,ViewY));
    if GridActive then ptF := SnapToGrid(ptF, False);
    with ptF do FOriginal.SelectedShape.MouseUp(RightButton, Shift, X,Y, ACursor, AHandled);
  end;
end;

procedure TVectorOriginalEditor.KeyDown(Shift: TShiftState; Key: TSpecialKey; out
  AHandled: boolean);
begin
  if Assigned(FOriginal) and Assigned(FOriginal.SelectedShape) then
  begin
    AHandled := false;
    FOriginal.SelectedShape.KeyDown(Shift, Key, AHandled);
    if AHandled then exit;

    if (Key = skReturn) and ([ssShift,ssCtrl,ssAlt]*Shift = []) then
    begin
      FOriginal.DeselectShape;
      AHandled := true;
      exit;
    end else
    if (Key = skEscape) and ([ssShift,ssCtrl,ssAlt]*Shift = []) and (FOriginal.SelectedShape.Usermode = vsuCreate) then
    begin
     FOriginal.SelectedShape.Remove;
     AHandled:= true;
    end;
  end;

  inherited KeyDown(Shift, Key, AHandled);
end;

procedure TVectorOriginalEditor.KeyUp(Shift: TShiftState; Key: TSpecialKey; out
  AHandled: boolean);
begin
  if Assigned(FOriginal) and Assigned(FOriginal.SelectedShape) then
  begin
    AHandled := false;
    FOriginal.SelectedShape.KeyUp(Shift, Key, AHandled);
    if AHandled then exit;
  end;

  inherited KeyUp(Shift, Key, AHandled);
end;

procedure TVectorOriginalEditor.KeyPress(UTF8Key: string; out
  AHandled: boolean);
begin
  if Assigned(FOriginal) and Assigned(FOriginal.SelectedShape) then
  begin
    AHandled := false;
    FOriginal.SelectedShape.KeyPress(UTF8Key, AHandled);
    if AHandled then exit;
  end;

  inherited KeyPress(UTF8Key, AHandled);
end;

{ TVectorShape }

function TVectorShape.GetIsSlow(AMatrix: TAffineMatrix): boolean;
begin
  result := false;
end;

function TVectorShape.GetUsedTextures: ArrayOfBGRABitmap;
var
  f: TVectorShapeFields;
  nb: integer;
begin
  f := Fields;
  setlength(result, 3);
  nb := 0;
  if (vsfBackFill in f) and (BackFill.FillType = vftTexture) then
  begin
    result[nb] := BackFill.Texture;
    inc(nb);
  end;
  if (vsfPenFill in f) and (PenFill.FillType = vftTexture) then
  begin
    result[nb] := PenFill.Texture;
    inc(nb);
  end;
  if (vsfOutlineFill in f) and (OutlineFill.FillType = vftTexture) then
  begin
    result[nb] := OutlineFill.Texture;
    inc(nb);
  end;
  setlength(result, nb);
end;

class function TVectorShape.Fields: TVectorShapeFields;
begin
  result := [];
end;

function TVectorShape.GetJoinStyle: TPenJoinStyle;
begin
  result := Stroker.JoinStyle;
end;

procedure TVectorShape.SetJoinStyle(AValue: TPenJoinStyle);
begin
  BeginUpdate;
  Stroker.JoinStyle := AValue;
  EndUpdate;
end;

procedure TVectorShape.SetUsermode(AValue: TVectorShapeUsermode);
begin
  if FUsermode=AValue then Exit;
  BeginEditingUpdate;
  FUsermode:=AValue;
  EndEditingUpdate;
end;

procedure TVectorShape.LoadFill(AStorage: TBGRACustomOriginalStorage;
  AObjectName: string; var AValue: TVectorialFill);
var
  obj: TBGRACustomOriginalStorage;
  texId, texOpacity: integer;
  origin, xAxis, yAxis: TPointF;
  grad: TBGRALayerGradientOriginal;
  repetition: TTextureRepetition;
  c: TBGRAPixel;
begin
  if AValue = nil then
  begin
    AValue := TVectorialFill.Create;
    AValue.OnChange := @FillChange;
  end;

  obj := AStorage.OpenObject(AObjectName+'-fill');
  if obj = nil then
  begin
    c := AStorage.Color[AObjectName+'-color'];
    if c.alpha <> 0 then
      AValue.SetSolid(c);
    exit;
  end;
  try
     case obj.RawString['class'] of
       'solid': AValue.SetSolid(obj.Color['color']);
       'texture': begin
           texId:= obj.Int['tex-id'];
           origin := obj.PointF['origin'];
           xAxis := obj.PointF['x-axis'];
           yAxis := obj.PointF['y-axis'];
           texOpacity := obj.IntDef['opacity',255];
           if texOpacity < 0 then texOpacity:= 0;
           if texOpacity > 255 then texOpacity:= 255;
           case obj.RawString['repetition'] of
             'none': repetition := trNone;
             'repeat-x': repetition := trRepeatX;
             'repeat-y': repetition := trRepeatY;
             else repetition := trRepeatBoth;
           end;
           if Assigned(Container) then
             AValue.SetTexture(Container.GetTexture(texId), AffineMatrix(xAxis,yAxis,origin), texOpacity, repetition)
           else
             AValue.Clear;
         end;
       'gradient': begin
           grad := TBGRALayerGradientOriginal.Create;
           grad.LoadFromStorage(obj);
           AValue.SetGradient(grad,true);
         end;
       else AValue.Clear;
     end;
  finally
    obj.Free;
  end;
end;

procedure TVectorShape.SaveFill(AStorage: TBGRACustomOriginalStorage;
  AObjectName: string; AValue: TVectorialFill);
var
  obj: TBGRACustomOriginalStorage;
  m: TAffineMatrix;
  ft: TVectorialFillType;
begin
  AStorage.RemoveObject(AObjectName+'-fill');
  AStorage.RemoveAttribute(AObjectName+'-color');
  if Assigned(AValue) then
  begin
    ft := AValue.FillType;
    if ft = vftSolid then
    begin
      AStorage.Color[AObjectName+'-color'] := AValue.SolidColor;
      exit;
    end else
    if not (ft in [vftTexture,vftGradient]) then exit;

    obj := AStorage.CreateObject(AObjectName+'-fill');
    try
      if ft = vftSolid then
      begin
        obj.RawString['class'] := 'solid';
        obj.Color['color'] := AValue.SolidColor;
      end
      else
      if ft = vftTexture then
      begin
        obj.RawString['class'] := 'texture';
        obj.Int['tex-id'] := Container.GetTextureId(AValue.Texture);
        m := AValue.TextureMatrix;
        obj.PointF['origin'] := PointF(m[1,3],m[2,3]);
        obj.PointF['x-axis'] := PointF(m[1,1],m[2,1]);
        obj.PointF['y-axis'] := PointF(m[1,2],m[2,2]);
        if AValue.TextureOpacity<>255 then
          obj.Int['opacity'] := AValue.TextureOpacity;
        case AValue.TextureRepetition of
          trNone: obj.RawString['repetition'] := 'none';
          trRepeatX: obj.RawString['repetition'] := 'repeat-x';
          trRepeatY: obj.RawString['repetition'] := 'repeat-y';
          trRepeatBoth: obj.RemoveAttribute('repetition');
        end;
      end else
      if ft = vftGradient then
      begin
        obj.RawString['class'] := 'gradient';
        AValue.Gradient.SaveToStorage(obj);
      end else
        obj.RawString['class'] := 'none';
    finally
      obj.Free;
    end;
  end;
end;

class function TVectorShape.Usermodes: TVectorShapeUsermodes;
begin
  result := [vsuEdit];
  if vsfBackFill in Fields then result += [vsuEditBackFill];
  if vsfPenFill in Fields then result += [vsuEditPenFill];
  if vsfOutlineFill in Fields then result += [vsuEditOutlineFill];
end;

class function TVectorShape.PreferPixelCentered: boolean;
begin
  result := true;
end;

class function TVectorShape.CreateEmpty: boolean;
begin
  result := false;
end;

procedure TVectorShape.SetContainer(AValue: TVectorOriginal);
begin
  if FContainer=AValue then Exit;
  if Assigned(FContainer) then raise exception.Create('Container already assigned');
  FContainer:=AValue;
end;

function TVectorShape.GetFill(var AFillVariable: TVectorialFill): TVectorialFill;
begin
  if AFillVariable = nil then
  begin
    AFillVariable := TVectorialFill.Create;
    AFillVariable.OnChange := @FillChange;
  end;
  result := AFillVariable;
end;

procedure TVectorShape.SetFill(var AFillVariable: TVectorialFill;
  AValue: TVectorialFill);
var
  sharedTex: TBGRABitmap;
  freeTex: Boolean;
begin
  if AFillVariable.Equals(AValue) then exit;
  BeginUpdate;
  freeTex := Assigned(AFillVariable) and Assigned(AFillVariable.Texture) and
    not (Assigned(AValue) and (AValue.FillType = vftTexture) and (AValue.Texture = AFillVariable.Texture));
  if AValue = nil then FreeAndNil(AFillVariable) else
  if AValue.FillType = vftTexture then
  begin
    if Assigned(Container) then
      sharedTex := Container.GetTexture(Container.AddTexture(AValue.Texture))
    else
      sharedTex := AValue.Texture;
    GetFill(AFillVariable).SetTexture(sharedTex, AValue.TextureMatrix, AValue.TextureOpacity, AValue.TextureRepetition);
  end else
    GetFill(AFillVariable).Assign(AValue);
  if Assigned(Container) and freeTex then Container.DiscardUnusedTextures;
  EndUpdate;
end;

procedure TVectorShape.SetId(AValue: integer);
begin
  if FId=AValue then Exit;
  FId:=AValue;
end;

procedure TVectorShape.SetOutlineWidth(AValue: single);
begin
  if AValue < 0 then AValue := 0;
  if FOutlineWidth=AValue then Exit;
  BeginUpdate;
  FOutlineWidth:=AValue;
  EndUpdate;
end;

procedure TVectorShape.SetOutlineFill(AValue: TVectorialFill);
begin
  SetFill(FOutlineFill, AValue);
end;

function TVectorShape.GetIsBack: boolean;
begin
  result := Assigned(Container) and (Container.IndexOfShape(self)=0);
end;

function TVectorShape.GetIsFront: boolean;
begin
  result := Assigned(Container) and (Container.IndexOfShape(self)=Container.ShapeCount-1);
end;

function TVectorShape.GetOutlineFill: TVectorialFill;
begin
  result := GetFill(FOutlineFill);
end;

procedure TVectorShape.BeginUpdate;
begin
  if FUpdateCount = 0 then
  begin
    FBoundsBeforeUpdate := GetRenderBounds(InfiniteRect, AffineMatrixIdentity);
    Inc(FRenderIteration);
  end;
  inc(FUpdateCount);
end;

procedure TVectorShape.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    dec(FUpdateCount);
    if FUpdateCount = 0 then
      DoOnChange(FBoundsBeforeUpdate);
  end;
end;

procedure TVectorShape.BeginEditingUpdate;
begin
  inc(FUpdateEditingCount);
end;

procedure TVectorShape.EndEditingUpdate;
begin
  if FUpdateEditingCount > 0 then
  begin
    dec(FUpdateEditingCount);
    if FUpdateEditingCount = 0 then
    begin
      if Assigned(FOnEditingChange) then
        FOnEditingChange(self);
    end;
  end;
end;

procedure TVectorShape.DoOnChange(ABoundsBefore: TRectF);
var
  boundsAfter: TRectF;
begin
  if Assigned(FOnChange) then
  begin
    boundsAfter := GetRenderBounds(InfiniteRect, AffineMatrixIdentity);
    FOnChange(self, boundsAfter.Union(ABoundsBefore, true));
  end;
end;

function TVectorShape.GetPenColor: TBGRAPixel;
begin
  if Assigned(FPenFill) then
    result := FPenFill.SolidColor
  else
    result := BGRAPixelTransparent;
end;

function TVectorShape.GetPenWidth: single;
begin
  result := FPenWidth;
end;

function TVectorShape.GetPenStyle: TBGRAPenStyle;
begin
  result := Stroker.CustomPenStyle;
end;

function TVectorShape.GetBackFill: TVectorialFill;
begin
  result := GetFill(FBackFill);
end;

function TVectorShape.GetPenFill: TVectorialFill;
begin
  result := GetFill(FPenFill);
end;

function TVectorShape.ComputeStroke(APoints: ArrayOfTPointF; AClosed: boolean; AStrokeMatrix: TAffineMatrix): ArrayOfTPointF;
begin
  Stroker.StrokeMatrix := AStrokeMatrix;
  if AClosed then
    result := Stroker.ComputePolygon(APoints, PenWidth)
  else
    result := Stroker.ComputePolyline(APoints, PenWidth, PenColor);
end;

function TVectorShape.GetStroker: TBGRAPenStroker;
begin
  if FStroker = nil then
  begin
    FStroker := TBGRAPenStroker.Create;
    FStroker.MiterLimit:= sqrt(2);
  end;
  result := FStroker;
end;

procedure TVectorShape.FillChange(ASender: TObject);
begin
  if FUpdateCount=0 then
  begin
    inc(FRenderIteration);
    DoOnChange(EmptyRectF);
  end;
end;

procedure TVectorShape.UpdateRenderStorage(ARenderBounds: TRect; AImage: TBGRACustomBitmap);
var
  obj: TBGRACustomOriginalStorage;
  imgStream: TMemoryStream;
begin
  if CanHaveRenderStorage then
  begin
    obj := Container.RenderStorage.CreateObject(inttostr(Id));
    obj.Int['iteration'] := FRenderIteration;
    obj.Rectangle['bounds'] := ARenderBounds;
    if Assigned(AImage) then
    begin
      imgStream := TMemoryStream.Create;
      AImage.Serialize(imgStream);
      obj.WriteFile('image.data', imgStream, true, true);
    end else
      obj.RemoveFile('image.data');
    obj.Free;
  end;
end;

procedure TVectorShape.DiscardRenderStorage;
begin
  if CanHaveRenderStorage then
    Container.RenderStorage.RemoveObject(inttostr(Id));
end;

procedure TVectorShape.RetrieveRenderStorage(AMatrix: TAffineMatrix; out
  ARenderBounds: TRect; out AImage: TBGRABitmap);
var
  stream: TMemoryStream;
  shapeStorage: TBGRACustomOriginalStorage;
begin
  ARenderBounds := EmptyRect;
  AImage := nil;
  if Assigned(Container) and Assigned(Container.RenderStorage) and (Container.RenderStorage.AffineMatrix['last-matrix']=AMatrix) then
  begin
    shapeStorage := Container.RenderStorage.OpenObject(inttostr(Id));
    if Assigned(shapeStorage) then
    begin
      if shapeStorage.Int['iteration'] = FRenderIteration then
      begin
        ARenderBounds := shapeStorage.Rectangle['bounds'];
        stream := TMemoryStream.Create;
        if shapeStorage.ReadFile('image.data', stream) and (stream.Size>0) then
        begin
          stream.Position:= 0;
          AImage := TBGRABitmap.Create;
          AImage.Deserialize(stream);
        end;
        stream.Free;
      end;
      shapeStorage.Free;
    end;
  end;
end;

function TVectorShape.CanHaveRenderStorage: boolean;
begin
  result := (Id <> 0) and Assigned(Container) and Assigned(Container.RenderStorage);
end;

procedure TVectorShape.SetPenColor(AValue: TBGRAPixel);
var
  vf: TVectorialFill;
begin
  vf := TVectorialFill.CreateAsSolid(AValue);
  PenFill := vf;
  vf.Free;
end;

procedure TVectorShape.SetPenWidth(AValue: single);
begin
  if AValue < 0 then AValue := 0;
  if FPenWidth = AValue then exit;
  BeginUpdate;
  FPenWidth := AValue;
  EndUpdate;
end;

procedure TVectorShape.SetPenStyle(AValue: TBGRAPenStyle);
begin
  BeginUpdate;
  Stroker.CustomPenStyle := AValue;
  EndUpdate;
end;

procedure TVectorShape.SetBackFill(AValue: TVectorialFill);
begin
  SetFill(FBackFill, AValue);
end;

procedure TVectorShape.SetPenFill(AValue: TVectorialFill);
begin
  SetFill(FPenFill, AValue);
end;

constructor TVectorShape.Create(AContainer: TVectorOriginal);
begin
  FContainer := AContainer;
  FPenFill := nil;
  FPenWidth := 1;
  FOutlineWidth := DefaultShapeOutlineWidth;
  FStroker := nil;
  FOnChange := nil;
  FOnEditingChange := nil;
  FBackFill := nil;
  FOutlineFill := nil;
  FUsermode:= vsuEdit;
  FRemoving:= false;
  FId := 0;
  FRenderIteration:= 0;
end;

class function TVectorShape.CreateFromStorage(
  AStorage: TBGRACustomOriginalStorage; AContainer: TVectorOriginal): TVectorShape;
var
  objClassName: RawByteString;
  shapeClass: TVectorShapeAny;
begin
  objClassName := AStorage.RawString['class'];
  if objClassName = '' then raise exception.Create('Shape class not defined');
  shapeClass:= GetVectorShapeByStorageClassName(objClassName);
  if shapeClass = nil then raise exception.Create('Unknown shape class "'+objClassName+'"');
  result := shapeClass.Create(AContainer);
  result.LoadFromStorage(AStorage);
end;

destructor TVectorShape.Destroy;
begin
  FreeAndNil(FStroker);
  FreeAndNil(FPenFill);
  FreeAndNil(FBackFill);
  FreeAndNil(FOutlineFill);
  inherited Destroy;
end;

procedure TVectorShape.Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix;
  ADraft: boolean);
begin
  Render(ADest, Point(0,0), AMatrix, ADraft);
end;

procedure TVectorShape.Render(ADest: TBGRABitmap; ARenderOffset: TPoint;
  AMatrix: TAffineMatrix; ADraft: boolean);
begin
  Render(ADest, AffineMatrixTranslation(ARenderOffset.X,ARenderOffset.Y)*AMatrix, ADraft);
end;

function TVectorShape.SuggestGradientBox(AMatrix: TAffineMatrix): TAffineBox;
var
  rF: TRectF;
begin
  rF := GetRenderBounds(InfiniteRect, AMatrix, [rboAssumeBackFill]);
  result := TAffineBox.AffineBox(rF);
end;

procedure TVectorShape.ConfigureEditor(AEditor: TBGRAOriginalEditor);
begin
  if (Usermode = vsuEditBackFill) and BackFill.IsEditable then
    BackFill.ConfigureEditor(AEditor)
  else
  if (Usermode = vsuEditPenFill) and PenFill.IsEditable then
    PenFill.ConfigureEditor(AEditor)
  else
  if (Usermode = vsuEditOutlineFill) and OutlineFill.IsEditable then
    OutlineFill.ConfigureEditor(AEditor)
  else
    ConfigureCustomEditor(AEditor);
end;

procedure TVectorShape.LoadFromStorage(AStorage: TBGRACustomOriginalStorage);
var
  f: TVectorShapeFields;
begin
  f := Fields;
  if f <> [] then
  begin
    BeginUpdate;
    Id := AStorage.Int['id'];
    FRenderIteration := AStorage.Int['iteration'];
    if vsfPenFill in f then LoadFill(AStorage, 'pen', FPenFill);
    if vsfPenWidth in f then PenWidth := AStorage.FloatDef['pen-width', 0];
    if vsfPenStyle in f then PenStyle := AStorage.FloatArray['pen-style'];
    if vsfJoinStyle in f then
      case AStorage.RawString['join-style'] of
      'round': JoinStyle := pjsRound;
      'bevel': JoinStyle := pjsBevel;
      else JoinStyle := pjsMiter;
      end;
    if vsfBackFill in f then LoadFill(AStorage, 'back', FBackFill);
    if vsfOutlineFill in f then
    begin
      LoadFill(AStorage, 'outline', FOutlineFill);
      OutlineWidth := AStorage.FloatDef['outline-width', DefaultShapeOutlineWidth];
    end;
    EndUpdate;
  end;
end;

procedure TVectorShape.SaveToStorage(AStorage: TBGRACustomOriginalStorage);
var
  f: TVectorShapeFields;
begin
  AStorage.Int['id'] := Id;
  AStorage.Int['iteration'] := FRenderIteration;
  f := Fields;
  if vsfPenFill in f then SaveFill(AStorage, 'pen', FPenFill);
  if vsfPenWidth in f then AStorage.Float['pen-width'] := PenWidth;
  if vsfPenStyle in f then AStorage.FloatArray['pen-style'] := PenStyle;
  if vsfJoinStyle in f then
    case JoinStyle of
    pjsRound: AStorage.RawString['join-style'] := 'round';
    pjsBevel: AStorage.RawString['join-style'] := 'bevel';
    else AStorage.RawString['join-style'] := 'miter';
    end;
  if vsfBackFill in f then SaveFill(AStorage, 'back', FBackFill);
  if vsfOutlineFill in f then
  begin
    SaveFill(AStorage, 'outline', FOutlineFill);
    if OutlineFill.FillType <> vftNone then
      AStorage.Float['outline-width'] := FOutlineWidth
    else
      AStorage.RemoveAttribute('outline-width');
  end;
end;

procedure TVectorShape.MouseMove(Shift: TShiftState; X, Y: single; var
  ACursor: TOriginalEditorCursor; var AHandled: boolean);
begin
  //nothing
end;

procedure TVectorShape.MouseDown(RightButton: boolean; Shift: TShiftState; X,
  Y: single; var ACursor: TOriginalEditorCursor; var AHandled: boolean);
begin
  //nothing
end;

procedure TVectorShape.MouseUp(RightButton: boolean; Shift: TShiftState; X,
  Y: single; var ACursor: TOriginalEditorCursor; var AHandled: boolean);
begin
  //nothing
end;

procedure TVectorShape.KeyDown(Shift: TShiftState; Key: TSpecialKey;
  var AHandled: boolean);
begin
  //nothing
end;

procedure TVectorShape.KeyUp(Shift: TShiftState; Key: TSpecialKey;
  var AHandled: boolean);
begin
  //nothing
end;

procedure TVectorShape.KeyPress(UTF8Key: string; var AHandled: boolean);
begin
  //nothing
end;

procedure TVectorShape.BringToFront;
begin
  if Assigned(Container) then
    Container.MoveShapeToIndex(Container.IndexOfShape(self),Container.ShapeCount-1);
end;

procedure TVectorShape.SendToBack;
begin
  if Assigned(Container) then
    Container.MoveShapeToIndex(Container.IndexOfShape(self),0);
end;

procedure TVectorShape.MoveUp(APassNonIntersectingShapes: boolean);
var
  movedShapeBounds, otherShapeBounds: TRectF;
  sourceIdx,idx: integer;
begin
  if not Assigned(Container) then exit;
  sourceIdx := Container.IndexOfShape(self);
  if sourceIdx = Container.ShapeCount-1 then exit;
  idx := sourceIdx;
  if APassNonIntersectingShapes then
  begin
    movedShapeBounds := self.GetRenderBounds(InfiniteRect, AffineMatrixIdentity);
    while idx < Container.ShapeCount-2 do
    begin
      otherShapeBounds := Container.Shape[idx+1].GetRenderBounds(InfiniteRect, AffineMatrixIdentity);
      if movedShapeBounds.IntersectsWith(otherShapeBounds) then break;
      inc(idx);
    end;
  end;
  inc(idx);
  Container.MoveShapeToIndex(sourceIdx, idx);
end;

procedure TVectorShape.MoveDown(APassNonIntersectingShapes: boolean);
var
  movedShapeBounds, otherShapeBounds: TRectF;
  sourceIdx,idx: integer;
begin
  if not Assigned(Container) then exit;
  sourceIdx := Container.IndexOfShape(self);
  if sourceIdx = 0 then exit;
  idx := sourceIdx;
  if APassNonIntersectingShapes then
  begin
    movedShapeBounds := self.GetRenderBounds(InfiniteRect, AffineMatrixIdentity);
    while idx > 1 do
    begin
      otherShapeBounds := Container.Shape[idx-1].GetRenderBounds(InfiniteRect, AffineMatrixIdentity);
      if movedShapeBounds.IntersectsWith(otherShapeBounds) then break;
      dec(idx);
    end;
  end;
  dec(idx);
  Container.MoveShapeToIndex(sourceIdx, idx);
end;

procedure TVectorShape.Remove;
begin
  if Assigned(Container) then Container.RemoveShape(self)
  else raise exception.Create('Shape does not have a container');
end;

function TVectorShape.Duplicate: TVectorShape;
var temp: TBGRAMemOriginalStorage;
  shapeClass: TVectorShapeAny;
begin
  shapeClass:= GetVectorShapeByStorageClassName(StorageClassName);
  if shapeClass = nil then raise exception.Create('Shape class "'+StorageClassName+'" not registered');

  temp := TBGRAMemOriginalStorage.Create;
  SaveToStorage(temp);
  result := shapeClass.Create(Container);
  result.LoadFromStorage(temp);
  temp.Free;
  result.FContainer := nil;
end;

{ TVectorOriginal }

function TVectorOriginal.GetShapeCount: integer;
begin
  result := FShapes.Count;
end;

function TVectorOriginal.OpenShapeRenderStorage(AShapeIndex: integer; ACreate: boolean): TBGRACustomOriginalStorage;
var
  shapeId: Integer;
begin
  if Assigned(RenderStorage) then
  begin
    shapeId := Shape[AShapeIndex].Id;
    if ACreate then
      result := RenderStorage.CreateObject(inttostr(shapeId))
    else
      result := RenderStorage.OpenObject(inttostr(shapeId));
  end
  else
    result := nil;
end;

function TVectorOriginal.FindShapeById(AId: integer): TVectorShape;
var
  i: Integer;
begin
  for i := 0 to FShapes.Count-1 do
    if FShapes[i].Id = AId then exit(FShapes[i]);
  exit(nil);
end;

procedure TVectorOriginal.DiscardUnusedRenderStorage;
var
  objs: TStringList;
  shapeId, errPos, i: integer;
begin
  if Assigned(RenderStorage) then
  begin
    objs := TStringList.Create;
    RenderStorage.EnumerateObjects(objs);
    for i := 0 to objs.Count-1 do
    begin
      val(objs[i], shapeId, errPos);
      if errPos = 0 then
      begin
        if FindShapeById(shapeId) = nil then
          RenderStorage.RemoveObject(objs[i]);
      end;
    end;
    objs.Free;
  end;
end;

function TVectorOriginal.GetShape(AIndex: integer): TVectorShape;
begin
  result := FShapes[AIndex];
end;

procedure TVectorOriginal.FreeDeletedShapes;
var
  i: Integer;
begin
  for i := 0 to FDeletedShapes.Count-1 do
    FDeletedShapes[i].Free;
  FDeletedShapes.Clear
end;

procedure TVectorOriginal.OnShapeChange(ASender: TObject; ABounds: TRectF);
begin
  if ASender <> FSelectedShape then DiscardFrozenShapes;
  NotifyChange(ABounds);
end;

procedure TVectorOriginal.OnShapeEditingChange(ASender: TObject);
begin
  if ASender = FSelectedShape then
    NotifyEditorChange;
end;

procedure TVectorOriginal.DiscardFrozenShapes;
begin
  FFrozenShapesComputed:= false;
  FreeAndNil(FFrozenShapesUnderSelection);
  FreeAndNil(FFrozenShapesOverSelection);
end;

function TVectorOriginal.GetTextureId(ABitmap: TBGRABitmap): integer;
var
  i: Integer;
begin
  if (ABitmap = nil) or (ABitmap.NbPixels = 0) then exit(EmptyTextureId);
  for i := 0 to FTextureCount-1 do
    if FTextures[i].Bitmap = ABitmap then exit(FTextures[i].Id);
  for i := 0 to FTextureCount-1 do
    if FTextures[i].Bitmap.Equals(ABitmap) then exit(FTextures[i].Id);
  exit(-1);
end;

function TVectorOriginal.IndexOfTexture(AId: integer): integer;
var
  i: Integer;
begin
  if AId = EmptyTextureId then exit(-1);
  for i := 0 to FTextureCount-1 do
    if FTextures[i].Id = AId then exit(i);
  exit(-1);
end;

procedure TVectorOriginal.AddTextureWithId(ATexture: TBGRABitmap; AId: integer);
begin
  if FTextureCount >= length(FTextures) then
    setlength(FTextures, FTextureCount*2+2);
  if AId > FLastTextureId then FLastTextureId:= AId;
  FTextures[FTextureCount].Bitmap := ATexture.NewReference as TBGRABitmap;
  FTextures[FTextureCount].Id := AId;
  inc(FTextureCount);
end;

procedure TVectorOriginal.ClearTextures;
var
  i: Integer;
begin
  if Assigned(FShapes) and (FShapes.Count > 0) then
    raise exception.Create('There are still shapes that could use textures');
  for i := 0 to FTextureCount-1 do
  begin
    FTextures[i].Bitmap.FreeReference;
    FTextures[i].Bitmap := nil;
  end;
  FTextureCount := 0;
  FTextures := nil;
  FLastTextureId:= EmptyTextureId;
end;

constructor TVectorOriginal.Create;
begin
  inherited Create;
  FShapes := TVectorShapes.Create;
  FDeletedShapes := TVectorShapes.Create;
  FSelectedShape := nil;
  FFrozenShapesUnderSelection := nil;
  FFrozenShapesOverSelection := nil;
  FFrozenShapesComputed:= false;
  FLastTextureId:= EmptyTextureId;
  FLastShapeId:= 0;
end;

destructor TVectorOriginal.Destroy;
var
  i: Integer;
begin
  FSelectedShape := nil;
  for i := 0 to FShapes.Count-1 do
    FShapes[i].Free;
  FreeAndNil(FShapes);
  FreeDeletedShapes;
  FreeAndNil(FDeletedShapes);
  FreeAndNil(FFrozenShapesUnderSelection);
  FreeAndNil(FFrozenShapesOverSelection);
  ClearTextures;
  inherited Destroy;
end;

procedure TVectorOriginal.Clear;
var
  i: Integer;
begin
  if FShapes.Count > 0 then
  begin
    FSelectedShape := nil;
    for i := 0 to FShapes.Count-1 do
      FDeletedShapes.Add(FShapes[i]);
    FShapes.Clear;
    FLastShapeId:= 0;
    ClearTextures;
    NotifyChange;
  end;
end;

function TVectorOriginal.AddTexture(ATexture: TBGRABitmap): integer;
begin
  result := GetTextureId(ATexture);
  if result <> -1 then exit;
  result:= FLastTextureId+1;
  AddTextureWithId(ATexture, result);
end;

function TVectorOriginal.GetTexture(AId: integer): TBGRABitmap;
var
  index: Integer;
begin
  index := IndexOfTexture(AId);
  if index = -1 then
    result := nil
  else
    result := FTextures[index].Bitmap;
end;

procedure TVectorOriginal.DiscardUnusedTextures;
var
  i, j: Integer;
  texs: array Of TBGRABitmap;
begin
  for i := 0 to FTextureCount-1 do
    FTextures[i].Counter:= 0;
  for i := 0 to FShapes.Count-1 do
  begin
    texs := FShapes[i].GetUsedTextures;
    for j := 0 to high(texs) do
      inc(FTextures[IndexOfTexture(GetTextureId(texs[j]))].Counter);
  end;
  for i := FTextureCount-1 downto 0 do
    if FTextures[i].Counter = 0 then
    begin
      FTextures[i].Bitmap.FreeReference;
      FTextures[i].Bitmap := nil;
      for j := i to FTextureCount-2 do
        FTextures[j] := FTextures[j+1];
      dec(FTextureCount);
    end;
  if FTextureCount < length(FTextures) div 2 then
    setlength(FTextures, FTextureCount);
end;

function TVectorOriginal.AddShape(AShape: TVectorShape): integer;
var
  texs: ArrayOfBGRABitmap;
  i: Integer;
begin
  if AShape.Container <> self then
  begin
    if AShape.Container = nil then
      AShape.Container := self
    else
      raise exception.Create('Container mismatch');
  end;
  result:= FShapes.Add(AShape);
  inc(FLastShapeId);
  AShape.Id := FLastShapeId;
  texs := AShape.GetUsedTextures;
  for i := 0 to high(texs) do AddTexture(texs[i]);
  AShape.OnChange := @OnShapeChange;
  AShape.OnEditingChange := @OnShapeEditingChange;
  DiscardFrozenShapes;
  NotifyChange(AShape.GetRenderBounds(InfiniteRect, AffineMatrixIdentity));
end;

function TVectorOriginal.AddShape(AShape: TVectorShape;
  AUsermode: TVectorShapeUsermode): integer;
begin
  result := AddShape(AShape);
  AShape.Usermode:= AUsermode;
  SelectShape(result);
end;

function TVectorOriginal.RemoveShape(AShape: TVectorShape): boolean;
var
  idx: LongInt;
  r: TRectF;
begin
  if AShape.FRemoving then exit;
  idx := FShapes.IndexOf(AShape);
  if idx = -1 then exit(false);
  AShape.FRemoving := true;
  if AShape = SelectedShape then DeselectShape;
  AShape.OnChange := nil;
  AShape.OnEditingChange := nil;
  r := AShape.GetRenderBounds(InfiniteRect, AffineMatrixIdentity);
  FShapes.Delete(idx);
  FDeletedShapes.Add(AShape);
  DiscardFrozenShapes;
  NotifyChange(r);
  AShape.FRemoving := false;
end;

procedure TVectorOriginal.SelectShape(AIndex: integer);
begin
  if (AIndex < 0) or (AIndex >= FShapes.Count) then
    raise ERangeError.Create('Index out of bounds');
  SelectShape(FShapes[AIndex]);
end;

procedure TVectorOriginal.SelectShape(AShape: TVectorShape);
var
  prev: TVectorShape;
  prevMode: TVectorShapeUsermode;
begin
  if FSelectedShape <> AShape then
  begin
    if AShape <> nil then
      if FShapes.IndexOf(AShape)=-1 then
        raise exception.Create('Shape not found');
    prev := FSelectedShape;
    FSelectedShape := nil;
    if Assigned(prev) then
    begin
      prevMode := prev.Usermode;
      prev.Usermode := vsuEdit;
    end else
      prevMode := vsuEdit;
    if Assigned(AShape) and (prevMode = vsuEditBackFill) and (prevMode in AShape.Usermodes) and
       AShape.BackFill.IsEditable then AShape.Usermode:= prevMode;
    if Assigned(AShape) and (prevMode = vsuEditPenFill) and (prevMode in AShape.Usermodes) and
       AShape.PenFill.IsEditable then AShape.Usermode:= prevMode;
    if Assigned(AShape) and (prevMode = vsuEditOutlineFill) and (prevMode in AShape.Usermodes) and
       AShape.OutlineFill.IsEditable then AShape.Usermode:= prevMode;
    if Assigned(AShape) and (prevMode = vsuEditText) and (prevMode in AShape.Usermodes) then
      AShape.Usermode := prevMode;
    FSelectedShape := AShape;
    DiscardFrozenShapes;
    NotifyEditorChange;
    if Assigned(FOnSelectShape) then
      FOnSelectShape(self, FSelectedShape, prev);
  end;
end;

procedure TVectorOriginal.DeselectShape;
begin
  SelectShape(nil);
end;

procedure TVectorOriginal.MouseClick(APoint: TPointF);
var
  i: LongInt;
begin
  for i:= FShapes.Count-1 downto 0 do
    if FShapes[i].PointInShape(APoint) then
    begin
      SelectShape(i);
      exit;
    end;
  DeselectShape;
end;

procedure TVectorOriginal.Render(ADest: TBGRABitmap; ARenderOffset: TPoint; AMatrix: TAffineMatrix;
  ADraft: boolean);
var
  i: Integer;
  idxSelected: LongInt;
  clipRectF: TRectF;
  mOfs: TAffineMatrix;
begin
  if AMatrix <> FFrozenShapeMatrix then DiscardFrozenShapes;
  idxSelected := FShapes.IndexOf(FSelectedShape);
  if idxSelected = -1 then
  begin
    FSelectedShape := nil;
    DiscardFrozenShapes;
  end;
  with ADest.ClipRect do
    clipRectF := RectF(Left,Top,Right,Bottom);
  mOfs := AffineMatrixTranslation(ARenderOffset.X,ARenderOffset.Y)*AMatrix;
  if FFrozenShapesComputed then
  begin
    ADest.PutImage(ARenderOffset.X-FFrozenShapesRenderOffset.X,
                   ARenderOffset.Y-FFrozenShapesRenderOffset.Y,
                   FFrozenShapesUnderSelection, dmSet);
    if FSelectedShape.GetRenderBounds(ADest.ClipRect, mOfs, []).IntersectsWith(clipRectF) then
      FSelectedShape.Render(ADest, ARenderOffset, AMatrix, ADraft);
    ADest.PutImage(ARenderOffset.X-FFrozenShapesRenderOffset.X,
                   ARenderOffset.Y-FFrozenShapesRenderOffset.Y,
                   FFrozenShapesOverSelection, dmDrawWithTransparency);
  end else
  begin
    if idxSelected <> -1 then
    begin
      if idxSelected > 0 then
      begin
        FreeAndNil(FFrozenShapesUnderSelection);
        FFrozenShapesUnderSelection := TBGRABitmap.Create(ADest.Width,ADest.Height);
        for i:= 0 to idxSelected-1 do
          if FShapes[i].GetRenderBounds(ADest.ClipRect, mOfs, []).IntersectsWith(clipRectF) then
            FShapes[i].Render(FFrozenShapesUnderSelection, ARenderOffset, AMatrix, false);
        ADest.PutImage(0,0,FFrozenShapesUnderSelection, dmSet);
      end;
      FSelectedShape.Render(ADest, ARenderOffset, AMatrix, ADraft);
      if idxSelected < FShapes.Count-1 then
      begin
        FreeAndNil(FFrozenShapesOverSelection);
        FFrozenShapesOverSelection := TBGRABitmap.Create(ADest.Width,ADest.Height);
        for i:= idxSelected+1 to FShapes.Count-1 do
          if FShapes[i].GetRenderBounds(ADest.ClipRect, mOfs, []).IntersectsWith(clipRectF) then
            FShapes[i].Render(FFrozenShapesOverSelection, ARenderOffset, AMatrix, false);
        ADest.PutImage(0,0,FFrozenShapesOverSelection, dmDrawWithTransparency);
      end;
      FFrozenShapesRenderOffset := ARenderOffset;
      FFrozenShapesComputed := true;
      FFrozenShapeMatrix := AMatrix;
    end else
    begin
      for i:= 0 to FShapes.Count-1 do
        if FShapes[i].GetRenderBounds(ADest.ClipRect, mOfs, []).IntersectsWith(clipRectF) then
          FShapes[i].Render(ADest, ARenderOffset, AMatrix, ADraft);
    end;
  end;
  DiscardUnusedRenderStorage;
end;

procedure TVectorOriginal.ConfigureEditor(AEditor: TBGRAOriginalEditor);
begin
  inherited ConfigureEditor(AEditor);
  if Assigned(FSelectedShape) then
  begin
    if FShapes.IndexOf(FSelectedShape)=-1 then
    begin
      FSelectedShape := nil;
      DiscardFrozenShapes;
    end
    else
      FSelectedShape.ConfigureEditor(AEditor);
  end;
  //no more reference to event handlers
  FreeDeletedShapes;
end;

function TVectorOriginal.CreateEditor: TBGRAOriginalEditor;
begin
  Result:= TVectorOriginalEditor.Create(self);
end;

function TVectorOriginal.GetRenderBounds(ADestRect: TRect;
  AMatrix: TAffineMatrix): TRect;
var
  area, shapeArea: TRectF;
  i: Integer;
  shapeDir: TBGRACustomOriginalStorage;
  useStorage: Boolean;
  iteration: LongInt;
begin
  area:= EmptyRectF;
  useStorage := Assigned(RenderStorage) and (RenderStorage.AffineMatrix['last-matrix']=AMatrix);
  for i:= 0 to FShapes.Count-1 do
  begin
    if useStorage then
    begin
      shapeDir := OpenShapeRenderStorage(i, false);
      if Assigned(shapeDir) then
      begin
        iteration := shapeDir.Int['iteration'];
        if iteration = FShapes[i].FRenderIteration then
        begin
          shapeArea := shapeDir.RectangleF['bounds'];
          area := area.Union(shapeArea, true);
          shapeDir.Free;
          continue;
        end;
      end;
    end;
    shapeArea := FShapes[i].GetRenderBounds(ADestRect, AMatrix);
    area := area.Union(shapeArea, true);
  end;

  if IsEmptyRectF(area) then
    result := EmptyRect
  else
    result := rect(floor(area.Left),floor(area.Top),ceil(area.Right),ceil(area.Bottom));
end;

procedure TVectorOriginal.LoadFromStorage(AStorage: TBGRACustomOriginalStorage);
var
  nb: LongInt;
  i: Integer;
  shapeObj, texObj: TBGRACustomOriginalStorage;
  objClassName, texName: String;
  shapeClass: TVectorShapeAny;
  loadedShape: TVectorShape;
  idList: array of single;
  mem: TMemoryStream;
  texId: integer;
  bmp: TBGRABitmap;
begin
  Clear;

  texObj := AStorage.OpenObject('textures');
  if Assigned(texObj) then
  begin
    try
      idList := texObj.FloatArray['id'];
      for i := 0 to high(idList) do
      begin
        texId:= round(idList[i]);
        texName:= 'tex'+inttostr(texId);
        mem := TMemoryStream.Create;
        try
          if not texObj.ReadFile(texName+'.png', mem) and
             not texObj.ReadFile(texName+'.jpg', mem) then
             raise exception.Create('Unable to find texture');
          mem.Position:= 0;
          bmp := TBGRABitmap.Create(mem);
          AddTextureWithId(bmp, texId);
          bmp.FreeReference;
        finally
          mem.Free;
        end;
      end;
    finally
      texObj.Free;
    end;
  end;

  nb := AStorage.Int['count'];
  for i:= 0 to nb-1 do
  begin
    shapeObj := AStorage.OpenObject('shape'+inttostr(i+1));
    if shapeObj <> nil then
    try
      loadedShape := TVectorShape.CreateFromStorage(shapeObj, self);
      loadedShape.OnChange := @OnShapeChange;
      loadedShape.OnEditingChange := @OnShapeEditingChange;
      if loadedShape.Id > FLastShapeId then FLastShapeId := loadedShape.Id;
      FShapes.Add(loadedShape);
    finally
      shapeObj.Free;
    end;
  end;
  for i := 0 to ShapeCount-1 do
    if Shape[i].Id = 0 then
    begin
      inc(FLastShapeId);
      Shape[i].Id := FLastShapeId;
    end;
  NotifyChange;
end;

procedure TVectorOriginal.SaveToStorage(AStorage: TBGRACustomOriginalStorage);
var
  nb: LongInt;
  i, texIndex: Integer;
  shapeObj, texObj: TBGRACustomOriginalStorage;
  idList: array of single;
  texName: String;
  mem: TMemoryStream;
  texId: integer;
begin
  nb := AStorage.Int['count'];
  for i := 0 to nb-1 do AStorage.RemoveObject('shape'+inttostr(i+1));
  AStorage.Int['count'] := 0;

  for i := 0 to FShapes.Count-1 do
  begin
    shapeObj := AStorage.CreateObject('shape'+inttostr(i+1));
    shapeObj.RawString['class'] := FShapes[i].StorageClassName;
    try
      FShapes[i].SaveToStorage(shapeObj);
      AStorage.Int['count'] := i+1;
    finally
      shapeObj.Free;
    end;
  end;

  if FTextureCount = 0 then
    AStorage.RemoveObject('textures')
  else
  begin
    texObj := nil;
    try
      texObj := AStorage.OpenObject('textures');
      if texObj = nil then
        texObj := AStorage.CreateObject('textures');

      for i := 0 to FTextureCount-1 do
        FTextures[i].Counter:= 0;

      idList := texObj.FloatArray['id'];
      for i := 0 to high(idList) do
      begin
        texId := round(idList[i]);
        texIndex:= IndexOfTexture(texId);
        if texIndex=-1 then
        begin
          texName := 'tex'+inttostr(texId);
          texObj.RemoveFile(texName+'.png');
          texObj.RemoveFile(texName+'.jpg');
        end else
          inc(FTextures[texIndex].Counter);
      end;

      setlength(idList, FTextureCount);
      for i := 0 to FTextureCount-1 do
      begin
        if FTextures[i].Counter = 0 then
        begin
          texName := 'tex'+inttostr(FTextures[i].Id);
          mem := TMemoryStream.Create;
          try
            FTextures[i].Bitmap.SaveToStreamAsPng(mem);
            texObj.WriteFile(texName+'.png', mem, false);
          finally
            mem.Free;
          end;
        end;
        idList[i] := FTextures[i].Id;
      end;
      texObj.FloatArray['id'] := idList;
    finally
      texObj.Free;
    end;
  end;

end;

function TVectorOriginal.IndexOfShape(AShape: TVectorShape): integer;
begin
  result := FShapes.IndexOf(AShape);
end;

procedure TVectorOriginal.MoveShapeToIndex(AFromIndex: integer; AToIndex: integer);
var
  movedShape: TVectorShape;
begin
  if AFromIndex = AToIndex then exit;
  movedShape := FShapes[AFromIndex];
  FShapes.Move(AFromIndex,AToIndex);
  DiscardFrozenShapes;
  NotifyChange(movedShape.GetRenderBounds(InfiniteRect, AffineMatrixIdentity));
end;

class function TVectorOriginal.StorageClassName: RawByteString;
begin
  result := 'vector';
end;

initialization

  RegisterLayerOriginal(TVectorOriginal);

end.

