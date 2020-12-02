// SPDX-License-Identifier: GPL-3.0-only
unit LCVectorOriginal;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRALayerOriginal, fgl, BGRAGradientOriginal, BGRABitmapTypes,
  BGRAPen, LCVectorialFill, LCResourceString, BGRASVGShapes, BGRASVGType,
  BGRASVG, BGRAUnits;

const
  InfiniteRect : TRect = (Left: -MaxLongInt; Top: -MaxLongInt; Right: MaxLongInt; Bottom: MaxLongInt);
  EmptyTextureId = 0;
  DefaultShapeOutlineWidth = 2;
  MediumShapeCost = 100;

  //not translated because unexpected internal errors are not useful for users
  errDuplicateVectorClass = 'Duplicate class name "%1" for vector shape';
  errMergeNotAllowed = 'Merge not allowed';
  errCannotBeComputedFromShape = 'Cannot be computed from shape';
  errFillFieldMismatch = 'Fill field mismatch';
  errInvalidStoredPointer = 'Invalid stored pointer';
  errUndefinedContainer = 'Undefined container';
  errContainerAlreadyAssigned = 'Container already assigned';
  errDiffHandlerOnlyDuringUpdate = 'Diff handler expected only between BeginUpdate and EndUpdate';
  errUnexpectedNil = 'Unexpected nil value';
  errContainerMismatch = 'Container mismatch';
  errAlreadyRemovingShape = 'Already removing shape';
  errUnableToFindTexture = 'Unable to find texture';
  errErrorLoadingShape = 'Error loading shape';

type
  TVectorOriginal = class;
  ArrayOfBGRABitmap = array of TBGRABitmap;
  TVectorShapeDiff = class;

  TShapeChangeEvent = procedure(ASender: TObject; ABounds: TRectF; ADiff: TVectorShapeDiff) of object;
  TShapeEditingChangeEvent = procedure(ASender: TObject) of object;
  TShapeRemoveQuery = procedure(ASender: TObject; var AHandled: boolean) of object;

  TRenderBoundsOption = (rboAssumePenFill, rboAssumeBackFill);
  TRenderBoundsOptions = set of TRenderBoundsOption;
  TVectorShapeField = (vsfPenFill, vsfPenWidth, vsfPenStyle, vsfJoinStyle, vsfBackFill, vsfOutlineFill, vsfOutlineWidth);
  TVectorShapeFields = set of TVectorShapeField;
  TVectorShapeUsermode = (vsuEdit, vsuCreate, vsuEditPenFill, vsuEditBackFill, vsuEditOutlineFill,
                          vsuCurveSetAuto, vsuCurveSetCurve, vsuCurveSetAngle,
                          vsuEditText);
  TVectorShapeUsermodes = set of TVectorShapeUsermode;
  TVectorShape = class;
  TVectorShapes = specialize TFPGList<TVectorShape>;

  { TVectorShapeDiff }

  TVectorShapeDiff = class
    constructor Create(AStartShape: TVectorShape); virtual; abstract;
    procedure ComputeDiff(AEndShape: TVectorShape); virtual; abstract;
    procedure Apply(AStartShape: TVectorShape); virtual; abstract;
    procedure Unapply(AEndShape: TVectorShape); virtual; abstract;
    function CanAppend(ADiff: TVectorShapeDiff): boolean; virtual;
    procedure Append(ADiff: TVectorShapeDiff); virtual; abstract;
    function IsIdentity: boolean; virtual; abstract;
  end;

  TCustomMultiSelectionDiff = class(TVectorShapeDiff)
  protected
    function GetShapeCount: integer; virtual; abstract;
    function GetShapeId(AIndex: integer): integer; virtual; abstract;
  public
    property ShapeCount: integer read GetShapeCount;
    property ShapeId[AIndex: integer]: integer read GetShapeId;
  end;

  TVectorShapeDiffList = specialize TFPGList<TVectorShapeDiff>;
  TVectorShapeDiffAny = class of TVectorShapeDiff;

  { TVectorShapeComposedDiff }

  TVectorShapeComposedDiff = class(TVectorShapeDiff)
  protected
    FDiffs: array of TVectorShapeDiff;
  public
    constructor Create(ADiffs: TVectorShapeDiffList);
    constructor Create(ADiffs: array of TVectorShapeDiff);
    constructor Create({%H-}AStartShape: TVectorShape); override;
    destructor Destroy; override;
    procedure ComputeDiff({%H-}AEndShape: TVectorShape); override;
    procedure Apply(AStartShape: TVectorShape); override;
    procedure Unapply(AEndShape: TVectorShape); override;
    function CanAppend(ADiff: TVectorShapeDiff): boolean; override;
    procedure Append(ADiff: TVectorShapeDiff); override;
    function IsIdentity: boolean; override;
    function GetMultiselection: TCustomMultiSelectionDiff;
  end;

  { TVectorShapeEmbeddedFillDiff }

  TVectorShapeEmbeddedFillDiff = class(TVectorShapeDiff)
  protected
    FField: TVectorShapeField;
    FFillDiff: TCustomVectorialFillDiff;
  public
    constructor Create(AField: TVectorShapeField; AFillDiff: TCustomVectorialFillDiff);
    constructor Create({%H-}AStartShape: TVectorShape); override;
    destructor Destroy; override;
    procedure ComputeDiff({%H-}AEndShape: TVectorShape); override;
    procedure Apply(AStartShape: TVectorShape); override;
    procedure Unapply(AEndShape: TVectorShape); override;
    function CanAppend(ADiff: TVectorShapeDiff): boolean; override;
    procedure Append(ADiff: TVectorShapeDiff); override;
    function IsIdentity: boolean; override;
  end;

  { TVectorShapeCommonDiff }

  TVectorShapeCommonDiff = class(TVectorShapeDiff)
  protected
    FStartPenWidth: single;
    FStartPenStyle: TBGRAPenStyle;
    FStartOutlineWidth: single;
    FStartJoinStyle: TPenJoinStyle;
    FEndPenWidth: single;
    FEndPenStyle: TBGRAPenStyle;
    FEndOutlineWidth: single;
    FEndJoinStyle: TPenJoinStyle;
  public
    constructor Create(AStartShape: TVectorShape); override;
    procedure ComputeDiff(AEndShape: TVectorShape); override;
    procedure Apply(AStartShape: TVectorShape); override;
    procedure Unapply(AEndShape: TVectorShape); override;
    procedure Append(ADiff: TVectorShapeDiff); override;
    function IsIdentity: boolean; override;
  end;

  { TVectorShapeCommonFillDiff }

  TVectorShapeCommonFillDiff = class(TVectorShapeDiff)
  protected
    FStartPenFill: TVectorialFill;
    FStartBackFill: TVectorialFill;
    FStartOutlineFill: TVectorialFill;
    FEndPenFill: TVectorialFill;
    FEndBackFill: TVectorialFill;
    FEndOutlineFill: TVectorialFill;
  public
    constructor Create(AStartShape: TVectorShape); override;
    destructor Destroy; override;
    procedure ComputeDiff(AEndShape: TVectorShape); override;
    procedure Apply(AStartShape: TVectorShape); override;
    procedure Unapply(AEndShape: TVectorShape); override;
    procedure Append(ADiff: TVectorShapeDiff); override;
    function IsIdentity: boolean; override;
  end;

  IVectorMultishape = interface
    procedure ClearShapes;
    procedure AddShape(AShape: TVectorShape);
    procedure RemoveShape(AShape: TVectorShape);
    function ContainsShape(AShape: TVectorShape): boolean;
    function ShapeCount: integer;
    function GetShape(AIndex: integer): TVectorShape;
    function SetShapes(AShapes: TVectorShapes): boolean;
    function FrontShape: TVectorShape;
    function BackShape: TVectorShape;
    procedure SetOnSelectionChange(AHandler: TNotifyEvent);
    function GetOnSelectionChange: TNotifyEvent;
  end;

  { TShapeRenderStorage }

  TShapeRenderStorage = object
      persistent, temporary: TBGRACustomOriginalStorage;
      class function OpenOrCreate(ARenderStorage: TBGRACustomOriginalStorage; AShapeId: integer): TShapeRenderStorage; static;
      class function Open(ARenderStorage: TBGRACustomOriginalStorage; AShapeId: integer): TShapeRenderStorage; static;
      class procedure Discard(ARenderStorage: TBGRACustomOriginalStorage; AShapeId: integer); static;
      class function None: TShapeRenderStorage; static;
      function IsOpened: boolean;
      procedure Close;
  end;

  { TVectorShape }

  TVectorShape = class
  private
    FId: integer;
    FOnRemoveQuery: TShapeRemoveQuery;
    FRenderIteration: integer; // increased at each BeginUpdate
    FOnChange: TShapeChangeEvent;
    FOnEditingChange: TShapeEditingChangeEvent;
    FTemporaryStorage: TBGRACustomOriginalStorage;
    FUpdateCount, FUpdateEditingCount: integer;
    FBoundsBeforeUpdate: TRectF;
    FPenFill, FBackFill, FOutlineFill: TVectorialFill;
    FStoreTexturePointer: boolean;
    FStroker: TBGRAPenStroker;
    FUsermode: TVectorShapeUsermode;
    FContainer: TVectorOriginal;
    FRemoving: boolean;
    FDiffs: TVectorShapeDiffList;
    FFillBeforeChangeBounds: TRectF;
    function GetIsUpdating: boolean;
    procedure SetContainer(AValue: TVectorOriginal);
    function GetFill(var AFillVariable: TVectorialFill): TVectorialFill;
    procedure SetFill(var AFillVariable: TVectorialFill; AValue: TVectorialFill; AUpdate: boolean);
    procedure SetId(AValue: integer);
  protected
    FPenWidth: single;
    FOutlineWidth: single;
    FFillChangeWithoutUpdate: boolean;
    procedure BeginEditingUpdate;
    procedure EndEditingUpdate;
    procedure DoOnChange(ABoundsBefore: TRectF; ADiff: TVectorShapeDiff); virtual;
    function GetIsBack: boolean; virtual;
    function GetIsFront: boolean; virtual;
    function GetPenColor: TBGRAPixel; virtual;
    function GetPenWidth: single; virtual;
    function GetPenStyle: TBGRAPenStyle; virtual;
    function GetJoinStyle: TPenJoinStyle;
    function GetBackFill: TVectorialFill; virtual;
    function GetPenFill: TVectorialFill; virtual;
    function GetOutlineFill: TVectorialFill; virtual;
    function GetOutlineWidth: single; virtual;
    procedure SetPenColor(AValue: TBGRAPixel); virtual;
    procedure SetPenWidth(AValue: single); virtual;
    procedure SetPenStyle({%H-}AValue: TBGRAPenStyle); virtual;
    procedure SetJoinStyle(AValue: TPenJoinStyle); virtual;
    procedure SetBackFill(AValue: TVectorialFill); virtual;
    procedure SetPenFill(AValue: TVectorialFill); virtual;
    procedure SetOutlineFill(AValue: TVectorialFill); virtual;
    procedure SetOutlineWidth(AValue: single); virtual;
    procedure SetUsermode(AValue: TVectorShapeUsermode); virtual;
    function LoadTexture(AStorage: TBGRACustomOriginalStorage; AName: string): TBGRABitmap;
    procedure SaveTexture(AStorage: TBGRACustomOriginalStorage; AName: string; AValue: TBGRABitmap);
    procedure LoadFill(AStorage: TBGRACustomOriginalStorage; AObjectName: string; var AValue: TVectorialFill);
    procedure SaveFill(AStorage: TBGRACustomOriginalStorage; AObjectName: string; AValue: TVectorialFill);
    function ComputeStroke(APoints: ArrayOfTPointF; AClosed: boolean; AStrokeMatrix: TAffineMatrix): ArrayOfTPointF; virtual;
    function ComputeStrokeEnvelope(APoints: ArrayOfTPointF; AClosed: boolean; AWidth: single): ArrayOfTPointF; virtual;
    function GetStroker: TBGRAPenStroker;
    procedure FillChange({%H-}ASender: TObject; var ADiff: TCustomVectorialFillDiff); virtual;
    procedure FillBeforeChange({%H-}ASender: TObject); virtual;
    function OpenRenderStorage(ACreateIfNecessary: boolean): TShapeRenderStorage;
    procedure UpdateRenderStorage(ARenderBounds: TRect; AImage: TBGRACustomBitmap = nil);
    procedure DiscardRenderStorage;
    procedure RetrieveRenderStorage(AMatrix: TAffineMatrix; out ARenderBounds: TRect; out AImage: TBGRABitmap);
    function CanHaveRenderStorage: boolean;
    function AddDiffHandler(AClass: TVectorShapeDiffAny): TVectorShapeDiff;
    procedure AddFillDiffHandler(AFill: TVectorialFill; ADiff: TCustomVectorialFillDiff);
    function GetDiffHandler(AClass: TVectorShapeDiffAny): TVectorShapeDiff;
    function GetIsFollowingMouse: boolean; virtual;
    function GetPenVisible(AAssumePenFill: boolean = False): boolean; virtual;
    function GetPenVisibleNow: boolean;
    function GetBackVisible: boolean; virtual;
    function GetOutlineVisible: boolean; virtual;
    function AppendVectorialFillToSVGDefs(AFill: TVectorialFill; const AMatrix: TAffineMatrix;
      ADefs: TSVGDefine; ANamePrefix: string): string;
    procedure ApplyStrokeStyleToSVG(AElement: TSVGElement; ADefs: TSVGDefine);
    procedure ApplyFillStyleToSVG(AElement: TSVGElement; ADefs: TSVGDefine);
    property Stroker: TBGRAPenStroker read GetStroker;
  public
    constructor Create(AContainer: TVectorOriginal); virtual;
    class function CreateFromStorage(AStorage: TBGRACustomOriginalStorage; AContainer: TVectorOriginal): TVectorShape;
    destructor Destroy; override;
    function AppendToSVG(AContent: TSVGContent; ADefs: TSVGDefine): TSVGElement; virtual; abstract;
    procedure BeginUpdate(ADiffHandler: TVectorShapeDiffAny=nil); virtual;
    procedure EndUpdate; virtual;
    procedure FillFit;
    procedure QuickDefine(constref APoint1,APoint2: TPointF); virtual; abstract;
    //one of the two Render functions must be overriden
    procedure Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix; ADraft: boolean); overload; virtual;
    procedure Render(ADest: TBGRABitmap; ARenderOffset: TPoint; AMatrix: TAffineMatrix; ADraft: boolean); overload; virtual;
    function GetRenderBounds(ADestRect: TRect; AMatrix: TAffineMatrix; AOptions: TRenderBoundsOptions = []): TRectF; virtual; abstract;
    function SuggestGradientBox(AMatrix: TAffineMatrix): TAffineBox; virtual;
    function PointInShape(APoint: TPointF): boolean; overload; virtual; abstract;
    function PointInShape(APoint: TPointF; ARadius: single): boolean; overload; virtual; abstract;
    function PointInBack(APoint: TPointF): boolean; overload; virtual;
    function PointInPen(APoint: TPointF): boolean; overload; virtual;
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
    procedure BringToFront; virtual;
    procedure SendToBack; virtual;
    procedure MoveUp(APassNonIntersectingShapes: boolean); virtual;
    procedure MoveDown(APassNonIntersectingShapes: boolean); virtual;
    procedure Remove;
    procedure AlignHorizontally(AAlign: TAlignment; const AMatrix: TAffineMatrix; const ABounds: TRect); virtual;
    procedure AlignVertically(AAlign: TTextLayout; const AMatrix: TAffineMatrix; const ABounds: TRect); virtual;
    function GetAlignBounds(const ALayoutRect: TRect; const AMatrix: TAffineMatrix): TRectF; virtual;
    procedure AlignTransform(const AMatrix: TAffineMatrix); virtual;
    function Duplicate: TVectorShape;
    class function StorageClassName: RawByteString; virtual; abstract;
    function GetIsSlow(const {%H-}AMatrix: TAffineMatrix): boolean; virtual;
    function GetGenericCost: integer; virtual;
    function GetUsedTextures: ArrayOfBGRABitmap; virtual;
    function GetAsMultishape: IVectorMultishape; virtual;
    procedure Transform(const AMatrix: TAffineMatrix); virtual;
    procedure TransformFrame(const AMatrix: TAffineMatrix); virtual; abstract;
    procedure TransformFill(const AMatrix: TAffineMatrix; ABackOnly: boolean); virtual;
    function AllowShearTransform: boolean; virtual;
    function MultiFields: TVectorShapeFields; virtual;
    class function Fields: TVectorShapeFields; virtual;
    class function Usermodes: TVectorShapeUsermodes; virtual;
    function MultiUsermodes: TVectorShapeUsermodes; virtual;
    class function PreferPixelCentered: boolean; virtual;
    class function CreateEmpty: boolean; virtual; //create shape even if empty?
    property OnChange: TShapeChangeEvent read FOnChange write FOnChange;
    property OnEditingChange: TShapeEditingChangeEvent read FOnEditingChange write FOnEditingChange;
    property OnRemoveQuery: TShapeRemoveQuery read FOnRemoveQuery write FOnRemoveQuery;
    property PenColor: TBGRAPixel read GetPenColor write SetPenColor;
    property PenFill: TVectorialFill read GetPenFill write SetPenFill;
    property BackFill: TVectorialFill read GetBackFill write SetBackFill;
    property OutlineFill: TVectorialFill read GetOutlineFill write SetOutlineFill;
    property PenWidth: single read GetPenWidth write SetPenWidth;
    property PenStyle: TBGRAPenStyle read GetPenStyle write SetPenStyle;
    property OutlineWidth: single read GetOutlineWidth write SetOutlineWidth;
    property JoinStyle: TPenJoinStyle read GetJoinStyle write SetJoinStyle;
    property Usermode: TVectorShapeUsermode read FUsermode write SetUsermode;
    property Container: TVectorOriginal read FContainer write SetContainer;
    property TemporaryStorage: TBGRACustomOriginalStorage read FTemporaryStorage write FTemporaryStorage;
    property IsFront: boolean read GetIsFront;
    property IsBack: boolean read GetIsBack;
    property IsRemoving: boolean read FRemoving;
    property Id: integer read FId write SetId;
    property IsFollowingMouse: boolean read GetIsFollowingMouse;
    property IsUpdating: boolean read GetIsUpdating;
    property BackVisible: boolean read GetBackVisible;
    property PenVisible: boolean read GetPenVisibleNow;
    property OutlineVisible: boolean read GetOutlineVisible;
  end;
  TVectorShapeAny = class of TVectorShape;

  TVectorOriginalSelectShapeEvent = procedure(ASender: TObject; AShape: TVectorShape; APreviousShape: TVectorShape) of object;

  { TVectorOriginalShapeDiff }

  TVectorOriginalShapeDiff = class(TBGRAOriginalDiff)
  protected
    FShapeIndex: integer;
    FShapeDiff: TVectorShapeDiff;
    function GetShape(AOriginal: TBGRALayerCustomOriginal): TVectorShape;
  public
    constructor Create(AShapeIndex: integer; AShapeDiff: TVectorShapeDiff);
    destructor Destroy; override;
    procedure Apply(AOriginal: TBGRALayerCustomOriginal); override;
    procedure Unapply(AOriginal: TBGRALayerCustomOriginal); override;
    function CanAppend(ADiff: TBGRAOriginalDiff): boolean; override;
    procedure Append(ADiff: TBGRAOriginalDiff); override;
    function IsIdentity: boolean; override;
  end;

  { TVectorOriginalShapeRangeDiff }

  TVectorOriginalShapeRangeDiff = class(TBGRAOriginalDiff)
  protected
    FRangeStart: integer;
    FShapesBefore, FShapesAfter: TVectorShapes;
    FSelectedShapeBefore, FSelectedShapeAfter: integer;
  public
    constructor Create(ARangeStart: integer; AShapesBefore, AShapesAfter: TVectorShapes;
      ASelectedShapeBefore, ASelectedShapeAfter: integer);
    destructor Destroy; override;
    procedure Apply(AOriginal: TBGRALayerCustomOriginal); override;
    procedure Unapply(AOriginal: TBGRALayerCustomOriginal); override;
    function CanAppend({%H-}ADiff: TBGRAOriginalDiff): boolean; override;
    procedure Append({%H-}ADiff: TBGRAOriginalDiff); override;
    function IsIdentity: boolean; override;
  end;

  { TVectorOriginalMoveShapeToIndexDiff }

  TVectorOriginalMoveShapeToIndexDiff = class(TBGRAOriginalDiff)
  protected
    FFromIndex,FToIndex: array of integer;
    FShapeCount: integer;
    procedure InternalMove(AOriginal: TBGRALayerCustomOriginal; AFromIndex,AToIndex: array of integer; ASendDiff: boolean);
  public
    constructor Create(AFromIndex,AToIndex: array of integer);
    procedure Apply(AOriginal: TBGRALayerCustomOriginal); overload; override;
    procedure Apply(AOriginal: TBGRALayerCustomOriginal; ASendDiff: boolean); overload;
    procedure Unapply(AOriginal: TBGRALayerCustomOriginal); override;
    function CanAppend(ADiff: TBGRAOriginalDiff): boolean; override;
    procedure Append(ADiff: TBGRAOriginalDiff); override;
    function IsIdentity: boolean; override;
  end;

  TVectorOriginalEditor = class;

  { TVectorOriginal }

  TVectorOriginal = class(TBGRALayerCustomOriginal)
  private
    procedure MultiSelection_SelectionChange(Sender: TObject);
  protected
    FShapes: TVectorShapes;
    FDeletedShapes: TVectorShapes;
    FSelectedShape: TVectorShape;
    FMultiselection: TVectorShape;
    FFrozenShapesUnderSelection,
    FFrozenShapesOverSelection: TBGRABitmap;
    FFrozenShapesUnderBounds,
    FFrozenShapesOverBounds: TRect;
    FFrozenShapesRenderOffset: TPoint;
    FFrozenShapesComputed: boolean;
    FFrozenShapeMatrix: TAffineMatrix;
    FUnfrozenRangeStart, FUnfrozenRangeEnd: integer;
    FOnSelectShape: TVectorOriginalSelectShapeEvent;
    FTextures: array of record
                 Bitmap: TBGRABitmap;
                 Id, Counter: integer;
               end;
    FTextureCount: integer;
    FLastTextureId: integer;
    FLastShapeId: integer;
    procedure FreeDeletedShapes;
    procedure OnShapeChange(ASender: TObject; ABounds: TRectF; ADiff: TVectorShapeDiff);
    procedure OnShapeEditingChange({%H-}ASender: TObject);
    procedure DiscardFrozenShapes;
    function GetShape(AIndex: integer): TVectorShape;
    function GetTextureId(ABitmap: TBGRABitmap): integer;
    function IndexOfTexture(AId: integer): integer;
    procedure AddTextureWithId(ATexture: TBGRABitmap; AId: integer);
    procedure ClearTextures;
    function GetShapeCount: integer;
    function OpenShapeRenderStorage(AShapeIndex: integer; ACreate: boolean): TBGRACustomOriginalStorage;
    procedure DiscardUnusedRenderStorage;
    function InternalInsertShape(AShape: TVectorShape; AIndex: integer): TRectF;
    function InternalInsertShapeRange(AShapes: TVectorShapes; AIndex: integer): TRectF;
    function InternalDeleteShapeRange(AStartIndex,ACount: integer): TRectF;
    function GetNewShapeId: integer;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear;
    function ConvertToSVG(const AMatrix: TAffineMatrix; out AOffset: TPoint): TObject; override;
    function AddTexture(ATexture: TBGRABitmap): integer;
    function GetTexture(AId: integer): TBGRABitmap;
    procedure DiscardUnusedTextures;
    function AddShape(AShape: TVectorShape): integer; overload;
    function AddShape(AShape: TVectorShape; AUsermode: TVectorShapeUsermode): integer; overload;
    function AddShapes(AShapes: TVectorShapes): integer;
    procedure InsertShape(AShape: TVectorShape; AIndex: integer);
    procedure InsertShapes(AShapes: TVectorShapes; AIndex: integer);
    function RemoveShape(AShape: TVectorShape): boolean;
    procedure DeleteShape(AIndex: integer);
    procedure DeleteShapeRange(AStartIndex,ACount: integer);
    procedure ReplaceShape(AIndex: integer; ANewShape: TVectorShape);
    procedure ReplaceShapeRange(AStartIndex: integer; ACountBefore: integer; ANewShapes: TVectorShapes);
    function SelectShapes(AShapes: TVectorShapes): boolean;
    function SelectShape(AIndex: integer; AToggle: boolean = false): boolean; overload;
    function SelectShape(AShape: TVectorShape; AToggle: boolean = false): boolean; overload;
    function DeselectShapes: boolean;
    procedure DeselectShape(AIndex: integer); overload;
    procedure DeselectShape(AShape: TVectorShape); overload;
    function GetShapesCost: integer;
    function PreferDraftMode(AEditor: TBGRAOriginalEditor; const AMatrix: TAffineMatrix): boolean;
    function MouseClick(APoint: TPointF; ARadius: single; AToggle: boolean): boolean;
    procedure Render(ADest: TBGRABitmap; ARenderOffset: TPoint; AMatrix: TAffineMatrix; ADraft: boolean); override;
    procedure ConfigureEditor(AEditor: TBGRAOriginalEditor); override;
    function CreateEditor: TBGRAOriginalEditor; override;
    function GetRenderBounds(ADestRect: TRect; {%H-}AMatrix: TAffineMatrix): TRect; overload; override;
    function GetRenderBounds(ADestRect: TRect; {%H-}AMatrix: TAffineMatrix; AStartIndex, AEndIndex: integer): TRect; overload;
    function GetAlignBounds(ADestRect: TRect; {%H-}AMatrix: TAffineMatrix): TRect;
    procedure LoadFromStorage(AStorage: TBGRACustomOriginalStorage); override;
    procedure SaveToStorage(AStorage: TBGRACustomOriginalStorage); override;
    function IndexOfShape(AShape: TVectorShape): integer;
    function FindShapeById(AId: integer): TVectorShape;
    procedure MoveShapeToIndex(AFromIndex, AToIndex: integer); overload;
    procedure MoveShapeToIndex(AFromIndex, AToIndex: array of integer); overload;
    class function StorageClassName: RawByteString; override;
    class function CanConvertToSVG: boolean; override;
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

var
  VectorMultiselectionFactory: TVectorShapeAny;

implementation

uses math, BGRATransform, BGRAFillInfo, BGRAGraphics, BGRAPath, Types,
  BGRAText, BGRATextFX, BGRALayers;

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
    raise exception.Create(StringReplace(errDuplicateVectorClass, '%1', AClass.StorageClassName, []));
  setlength(VectorShapeClasses, length(VectorShapeClasses)+1);
  VectorShapeClasses[high(VectorShapeClasses)] := AClass;
end;

{ TVectorOriginalMoveShapeToIndexDiff }

type
  TMovedShape = record
      shape: TVectorShape;
      targetIndex: integer;
      class operator =(const ms1, ms2: TMovedShape): boolean;
    end;

{ TShapeRenderStorage }

class function TShapeRenderStorage.OpenOrCreate(ARenderStorage: TBGRACustomOriginalStorage; AShapeId: integer): TShapeRenderStorage;
begin
  result.persistent := ARenderStorage.OpenObject(inttostr(AShapeId));
  if result.persistent = nil then
    result.persistent := ARenderStorage.CreateObject(inttostr(AShapeId));
  result.temporary := result.persistent.OpenObject(RenderTempSubDirectory);
  if result.temporary = nil then
    result.temporary := result.persistent.CreateObject(RenderTempSubDirectory);
end;

class function TShapeRenderStorage.Open(
  ARenderStorage: TBGRACustomOriginalStorage; AShapeId: integer): TShapeRenderStorage;
begin
  result.persistent := ARenderStorage.OpenObject(inttostr(AShapeId));
  if Assigned(result.persistent) then
    result.temporary := result.persistent.OpenObject(RenderTempSubDirectory)
  else
    result.temporary := nil;
end;

class procedure TShapeRenderStorage.Discard(
  ARenderStorage: TBGRACustomOriginalStorage; AShapeId: integer);
begin
  ARenderStorage.RemoveObject(inttostr(AShapeId));
end;

class function TShapeRenderStorage.None: TShapeRenderStorage;
begin
  result.persistent := nil;
  result.temporary := nil;
end;

function TShapeRenderStorage.IsOpened: boolean;
begin
  result := (persistent <> nil) or (temporary <> nil);
end;

procedure TShapeRenderStorage.Close;
var
  freeTemp: Boolean;
begin
  if Assigned(temporary) then
  begin
    freeTemp := temporary.Empty;
    FreeAndNil(temporary);
    if freeTemp and Assigned(persistent) then persistent.RemoveObject(RenderTempSubDirectory);
  end;
  FreeAndNil(persistent);
end;

class operator TMovedShape.=(const ms1, ms2: TMovedShape): boolean;
begin
  result := (ms1.shape = ms2.shape) and (ms1.targetIndex = ms2.targetIndex);
end;

function CompareMovedShapeTargetIndex(const ms1, ms2: TMovedShape): integer;
begin
  result := ms1.targetIndex - ms2.targetIndex;
end;

procedure TVectorOriginalMoveShapeToIndexDiff.InternalMove(AOriginal: TBGRALayerCustomOriginal; AFromIndex,
  AToIndex: array of integer; ASendDiff: boolean);
type
  TMovedShapeList = specialize TFPGList<TMovedShape>;
var
  movedShapes: TMovedShapeList;
  ms: TMovedShape;
  r: TRectF;
  i: Integer;
  orig: TVectorOriginal;
begin
  if FShapeCount = 0 then exit;
  orig := AOriginal as TVectorOriginal;
  movedShapes := TMovedShapeList.Create;
  for i := 0 to FShapeCount-1 do
  begin
    ms.shape := orig.Shape[AFromIndex[i]];
    ms.targetIndex:= AToIndex[i];
    movedShapes.Add(ms);
  end;
  movedShapes.Sort(@CompareMovedShapeTargetIndex);
  if movedShapes[0].targetIndex > orig.IndexOfShape(movedShapes[0].shape) then
  begin
    for i := movedShapes.Count-1 downto 0 do
      orig.FShapes.Move(orig.IndexOfShape(movedShapes[i].shape), movedShapes[i].targetIndex);
  end else
    for i := 0 to movedShapes.Count-1 do
      orig.FShapes.Move(orig.IndexOfShape(movedShapes[i].shape), movedShapes[i].targetIndex);

  orig.DiscardFrozenShapes;
  r := EmptyRectF;
  for i := 0 to movedShapes.Count-1 do
    r := r.Union(movedShapes[i].shape.GetRenderBounds(InfiniteRect, AffineMatrixIdentity), true);
  movedShapes.Free;

  if ASendDiff then orig.NotifyChange(r,self)
  else orig.NotifyChange(r);
end;

constructor TVectorOriginalMoveShapeToIndexDiff.Create(AFromIndex,
  AToIndex: array of integer);
var
  i: Integer;
begin
  if length(AFromIndex) <> length(AToIndex) then
    raise exception.Create('Dimension mismatch');
  FShapeCount:= length(AFromIndex);
  setlength(FFromIndex, FShapeCount);
  setlength(FToIndex, FShapeCount);
  for i := 0 to FShapeCount-1 do
  begin
    FFromIndex[i] := AFromIndex[i];
    FToIndex[i] := AToIndex[i];
  end;
end;

procedure TVectorOriginalMoveShapeToIndexDiff.Apply(
  AOriginal: TBGRALayerCustomOriginal);
begin
  Apply(AOriginal, False);
end;

procedure TVectorOriginalMoveShapeToIndexDiff.Apply(
  AOriginal: TBGRALayerCustomOriginal; ASendDiff: boolean);
begin
  InternalMove(AOriginal, FFromIndex, FToIndex, ASendDiff);
end;

procedure TVectorOriginalMoveShapeToIndexDiff.Unapply(
  AOriginal: TBGRALayerCustomOriginal);
begin
  InternalMove(AOriginal, FToIndex, FFromIndex, False);
end;

function TVectorOriginalMoveShapeToIndexDiff.CanAppend(ADiff: TBGRAOriginalDiff): boolean;
var
  other: TVectorOriginalMoveShapeToIndexDiff;
  i: Integer;
begin
  if ADiff is TVectorOriginalMoveShapeToIndexDiff then
  begin
    other := TVectorOriginalMoveShapeToIndexDiff(ADiff);
    if other.FShapeCount <> FShapeCount then exit(false);
    for i := 0 to FShapeCount-1 do
      if other.FFromIndex[i] <> FToIndex[i] then exit(false);
    result := true;
  end else
    result := false;
end;

procedure TVectorOriginalMoveShapeToIndexDiff.Append(ADiff: TBGRAOriginalDiff);
var
  other: TVectorOriginalMoveShapeToIndexDiff;
  i: Integer;
begin
  if CanAppend(ADiff) then
  begin
    other := ADiff as TVectorOriginalMoveShapeToIndexDiff;
    for i := 0 to FShapeCount-1 do
      FToIndex[i] := other.FToIndex[i];
  end;
end;

function TVectorOriginalMoveShapeToIndexDiff.IsIdentity: boolean;
var
  i: Integer;
begin
  for i := 0 to FShapeCount-1 do
    if FFromIndex[i] <> FToIndex[i] then
      exit(false);
  result := true;
end;

{ TVectorShapeDiff }

function TVectorShapeDiff.CanAppend(ADiff: TVectorShapeDiff): boolean;
begin
  result := (ADiff.ClassType = self.ClassType);
end;

{ TVectorShapeCommonFillDiff }

constructor TVectorShapeCommonFillDiff.Create(AStartShape: TVectorShape);
begin
  with AStartShape do
  begin
    if Assigned(FPenFill) and (FPenFill.FillType <> vftNone) then
      FStartPenFill := FPenFill.Duplicate;
    if Assigned(FBackFill) and (FBackFill.FillType <> vftNone) then
      FStartBackFill := FBackFill.Duplicate;
    if Assigned(FOutlineFill) and (FOutlineFill.FillType <> vftNone) then
      FStartOutlineFill := FOutlineFill.Duplicate;
  end;
end;

destructor TVectorShapeCommonFillDiff.Destroy;
begin
  FStartPenFill.Free;
  FStartBackFill.Free;
  FStartOutlineFill.Free;
  FEndPenFill.Free;
  FEndBackFill.Free;
  FEndOutlineFill.Free;
  inherited Destroy;
end;

procedure TVectorShapeCommonFillDiff.ComputeDiff(AEndShape: TVectorShape);
begin
  with AEndShape do
  begin
    if Assigned(FPenFill) and (FPenFill.FillType <> vftNone) then
      FEndPenFill := FPenFill.Duplicate;
    if Assigned(FBackFill) and (FBackFill.FillType <> vftNone) then
      FEndBackFill := FBackFill.Duplicate;
    if Assigned(FOutlineFill) and (FOutlineFill.FillType <> vftNone) then
      FEndOutlineFill := FOutlineFill.Duplicate;
  end;
end;

procedure TVectorShapeCommonFillDiff.Apply(AStartShape: TVectorShape);
begin
  with AStartShape do
  begin
    BeginUpdate;
    SetFill(FPenFill, FEndPenFill, False);
    SetFill(FBackFill, FEndBackFill, False);
    SetFill(FOutlineFill, FEndOutlineFill, False);
    EndUpdate;
  end;
end;

procedure TVectorShapeCommonFillDiff.Unapply(AEndShape: TVectorShape);
begin
  with AEndShape do
  begin
    BeginUpdate;
    SetFill(FPenFill, FStartPenFill, False);
    SetFill(FBackFill, FStartBackFill, False);
    SetFill(FOutlineFill, FStartOutlineFill, False);
    EndUpdate;
  end;
end;

procedure TVectorShapeCommonFillDiff.Append(ADiff: TVectorShapeDiff);
var
  next: TVectorShapeCommonFillDiff;
begin
  next := ADiff as TVectorShapeCommonFillDiff;
  if Assigned(next.FEndPenFill) then
  begin
    if FEndPenFill = nil then FEndPenFill := TVectorialFill.Create;
    FEndPenFill.Assign(next.FEndPenFill);
  end else FreeAndNil(FEndPenFill);
  if Assigned(next.FEndBackFill) then
  begin
    if FEndBackFill = nil then FEndBackFill := TVectorialFill.Create;
    FEndBackFill.Assign(next.FEndBackFill);
  end else FreeAndNil(FEndBackFill);
  if Assigned(next.FEndOutlineFill) then
  begin
    if FEndOutlineFill = nil then FEndOutlineFill := TVectorialFill.Create;
    FEndOutlineFill.Assign(next.FEndOutlineFill);
  end else FreeAndNil(FEndOutlineFill);
end;

function TVectorShapeCommonFillDiff.IsIdentity: boolean;
begin
  result := TVectorialFill.Equal(FStartPenFill, FEndPenFill) and
    TVectorialFill.Equal(FStartBackFill, FEndBackFill) and
    TVectorialFill.Equal(FStartOutlineFill, FEndOutlineFill);
end;

{ TVectorOriginalShapeRangeDiff }

constructor TVectorOriginalShapeRangeDiff.Create(ARangeStart: integer;
  AShapesBefore, AShapesAfter: TVectorShapes;
  ASelectedShapeBefore, ASelectedShapeAfter: integer);
var
  i: Integer;
begin
  FRangeStart := ARangeStart;
  FShapesBefore := TVectorShapes.Create;
  if Assigned(AShapesBefore) then
    for i := 0 to AShapesBefore.Count-1 do
      FShapesBefore.Add(AShapesBefore[i].Duplicate);
  FSelectedShapeBefore:= ASelectedShapeBefore;
  FShapesAfter := TVectorShapes.Create;
  if Assigned(AShapesAfter) then
    for i := 0 to AShapesAfter.Count-1 do
      FShapesAfter.Add(AShapesAfter[i].Duplicate);
  FSelectedShapeAfter:= ASelectedShapeAfter;
end;

destructor TVectorOriginalShapeRangeDiff.Destroy;
var
  i: Integer;
begin
  for i := 0 to FShapesBefore.Count-1 do FShapesBefore[i].Free;
  FShapesBefore.Free;
  for i := 0 to FShapesAfter.Count-1 do FShapesAfter[i].Free;
  FShapesAfter.Free;
  inherited Destroy;
end;

procedure TVectorOriginalShapeRangeDiff.Apply(
  AOriginal: TBGRALayerCustomOriginal);
var
  i: Integer;
  rRemove, rInsert: TRectF;
  insCopy: TVectorShapes;
begin
  with (AOriginal as TVectorOriginal) do
  begin
    rRemove := InternalDeleteShapeRange(FRangeStart, FShapesBefore.Count);
    insCopy := TVectorShapes.Create;
    for i := 0 to FShapesAfter.Count-1 do insCopy.Add(FShapesAfter[i].Duplicate);
    rInsert := InternalInsertShapeRange(insCopy, FRangeStart);
    insCopy.Free;
    NotifyChange(TRectF.Union(rRemove,rInsert,True));
    SelectShape(FSelectedShapeAfter);
  end;
end;

procedure TVectorOriginalShapeRangeDiff.Unapply(
  AOriginal: TBGRALayerCustomOriginal);
var
  i: Integer;
  rRemove, rInsert: TRectF;
  insCopy: TVectorShapes;
begin
  with (AOriginal as TVectorOriginal) do
  begin
    rRemove := InternalDeleteShapeRange(FRangeStart, FShapesAfter.Count);
    insCopy := TVectorShapes.Create;
    for i := 0 to FShapesBefore.Count-1 do insCopy.Add(FShapesBefore[i].Duplicate);
    rInsert := InternalInsertShapeRange(insCopy, FRangeStart);
    insCopy.Free;
    NotifyChange(TRectF.Union(rRemove,rInsert,True));
    SelectShape(FSelectedShapeBefore);
  end;
end;

function TVectorOriginalShapeRangeDiff.CanAppend(ADiff: TBGRAOriginalDiff): boolean;
begin
  result := false;
end;

procedure TVectorOriginalShapeRangeDiff.Append(ADiff: TBGRAOriginalDiff);
begin
  raise exception.Create(errMergeNotAllowed);
end;

function TVectorOriginalShapeRangeDiff.IsIdentity: boolean;
begin
  result := false;
end;

{ TVectorOriginalShapeDiff }

function TVectorOriginalShapeDiff.GetShape(AOriginal: TBGRALayerCustomOriginal): TVectorShape;

  procedure UpdateMultiSelection(AOriginal: TVectorOriginal; AMultiDiff: TCustomMultiSelectionDiff);
  var
    i: Integer;
    containedShapes: TVectorShapes;
    s, s2: TVectorShape;
  begin
    containedShapes := TVectorShapes.Create;
    for i := 0 to AMultiDiff.ShapeCount-1 do
    begin
      s2 := AOriginal.FindShapeById(AMultiDiff.ShapeId[i]);
      if Assigned(s2) then containedShapes.Add(s2);
    end;
    AOriginal.SelectShapes(containedShapes);
    containedShapes.Free;
  end;

var
  multiDiff: TCustomMultiSelectionDiff;
  orig: TVectorOriginal;

begin
  orig := (AOriginal as TVectorOriginal);
  if FShapeIndex = -2 then
  begin
    result := orig.FMultiselection;
    if FShapeDiff is TCustomMultiSelectionDiff then
      UpdateMultiSelection(orig, TCustomMultiSelectionDiff(FShapeDiff)) else
    if FShapeDiff is TVectorShapeComposedDiff then
    begin
      multiDiff := TVectorShapeComposedDiff(FShapeDiff).GetMultiselection;
      if Assigned(multiDiff) then UpdateMultiSelection(orig, multiDiff);
    end;
  end else
    result := orig.Shape[FShapeIndex];
end;

constructor TVectorOriginalShapeDiff.Create(AShapeIndex: integer;
  AShapeDiff: TVectorShapeDiff);
begin
  FShapeIndex := AShapeIndex;
  FShapeDiff := AShapeDiff;
end;

destructor TVectorOriginalShapeDiff.Destroy;
begin
  FShapeDiff.Free;
  inherited Destroy;
end;

procedure TVectorOriginalShapeDiff.Apply(AOriginal: TBGRALayerCustomOriginal);
begin
  FShapeDiff.Apply(GetShape(AOriginal));
end;

procedure TVectorOriginalShapeDiff.Unapply(AOriginal: TBGRALayerCustomOriginal);
begin
  FShapeDiff.Unapply(GetShape(AOriginal));
end;

function TVectorOriginalShapeDiff.CanAppend(ADiff: TBGRAOriginalDiff): boolean;
begin
  result := (ADiff is TVectorOriginalShapeDiff) and
  (TVectorOriginalShapeDiff(ADiff).FShapeIndex = FShapeIndex) and
  (FShapeDiff.CanAppend(TVectorOriginalShapeDiff(ADiff).FShapeDiff));
end;

procedure TVectorOriginalShapeDiff.Append(ADiff: TBGRAOriginalDiff);
begin
  if CanAppend(ADiff) then
    FShapeDiff.Append(TVectorOriginalShapeDiff(ADiff).FShapeDiff)
  else
    raise exception.Create(errMergeNotAllowed);
end;

function TVectorOriginalShapeDiff.IsIdentity: boolean;
begin
  result := FShapeDiff.IsIdentity;
end;

{ TVectorShapeCommonDiff }

constructor TVectorShapeCommonDiff.Create(AStartShape: TVectorShape);
begin
  with AStartShape do
  begin
    FStartPenWidth:= PenWidth;
    FStartPenStyle:= DuplicatePenStyle(PenStyle);
    FStartOutlineWidth:= OutlineWidth;
    FStartJoinStyle:= JoinStyle;
  end;
end;

procedure TVectorShapeCommonDiff.ComputeDiff(AEndShape: TVectorShape);
begin
  with AEndShape do
  begin
    FEndPenWidth:= PenWidth;
    FEndPenStyle:= DuplicatePenStyle(PenStyle);
    FEndOutlineWidth:= OutlineWidth;
    FEndJoinStyle:= JoinStyle;
  end;
end;

procedure TVectorShapeCommonDiff.Apply(AStartShape: TVectorShape);
begin
  with AStartShape do
  begin
    BeginUpdate;
    FPenWidth := FEndPenWidth;
    Stroker.CustomPenStyle := DuplicatePenStyle(FEndPenStyle);
    FOutlineWidth := FEndOutlineWidth;
    Stroker.JoinStyle := FEndJoinStyle;
    EndUpdate;
  end;
end;

procedure TVectorShapeCommonDiff.Unapply(AEndShape: TVectorShape);
begin
  with AEndShape do
  begin
    BeginUpdate;
    FPenWidth := FStartPenWidth;
    Stroker.CustomPenStyle := DuplicatePenStyle(FStartPenStyle);
    FOutlineWidth := FStartOutlineWidth;
    Stroker.JoinStyle := FStartJoinStyle;
    EndUpdate;
  end;
end;

procedure TVectorShapeCommonDiff.Append(ADiff: TVectorShapeDiff);
var
  next: TVectorShapeCommonDiff;
begin
  next := ADiff as TVectorShapeCommonDiff;
  FEndPenWidth:= next.FEndPenWidth;
  FEndPenStyle:= DuplicatePenStyle(next.FEndPenStyle);
  FEndOutlineWidth:= next.FEndOutlineWidth;
  FEndJoinStyle:= next.FEndJoinStyle;
end;

function TVectorShapeCommonDiff.IsIdentity: boolean;
begin
  result := (FStartPenWidth = FEndPenWidth) and
    PenStyleEqual(FStartPenStyle, FEndPenStyle) and
    (FStartOutlineWidth = FEndOutlineWidth) and
    (FStartJoinStyle = FEndJoinStyle);
end;

{ TVectorShapeEmbeddedFillDiff }

constructor TVectorShapeEmbeddedFillDiff.Create(AField: TVectorShapeField;
  AFillDiff: TCustomVectorialFillDiff);
begin
  FField := AField;
  FFillDiff := AFillDiff;
end;

constructor TVectorShapeEmbeddedFillDiff.Create(AStartShape: TVectorShape);
begin
  raise exception.Create(errCannotBeComputedFromShape);
end;

destructor TVectorShapeEmbeddedFillDiff.Destroy;
begin
  FFillDiff.Free;
  inherited Destroy;
end;

procedure TVectorShapeEmbeddedFillDiff.ComputeDiff(AEndShape: TVectorShape);
begin
  raise exception.Create(errCannotBeComputedFromShape);
end;

procedure TVectorShapeEmbeddedFillDiff.Apply(AStartShape: TVectorShape);
begin
  case FField of
  vsfPenFill: FFillDiff.Apply(AStartShape.PenFill);
  vsfBackFill: FFillDiff.Apply(AStartShape.BackFill);
  vsfOutlineFill: FFillDiff.Apply(AStartShape.OutlineFill);
  end;
end;

procedure TVectorShapeEmbeddedFillDiff.Unapply(AEndShape: TVectorShape);
begin
  case FField of
  vsfPenFill: FFillDiff.Unapply(AEndShape.PenFill);
  vsfBackFill: FFillDiff.Unapply(AEndShape.BackFill);
  vsfOutlineFill: FFillDiff.Unapply(AEndShape.OutlineFill);
  end;
end;

function TVectorShapeEmbeddedFillDiff.CanAppend(ADiff: TVectorShapeDiff): boolean;
begin
  result := (ADiff is TVectorShapeEmbeddedFillDiff) and
    (TVectorShapeEmbeddedFillDiff(ADiff).FField = FField) and
    FFillDiff.CanAppend(TVectorShapeEmbeddedFillDiff(ADiff).FFillDiff);
end;

procedure TVectorShapeEmbeddedFillDiff.Append(ADiff: TVectorShapeDiff);
var
  next: TVectorShapeEmbeddedFillDiff;
begin
  next := ADiff as TVectorShapeEmbeddedFillDiff;
  if next.FField <> FField then raise exception.Create(errFillFieldMismatch);
  FFillDiff.Append(next.FFillDiff);
end;

function TVectorShapeEmbeddedFillDiff.IsIdentity: boolean;
begin
  result := FFillDiff.IsIdentity;
end;

{ TVectorShapeComposedDiff }

constructor TVectorShapeComposedDiff.Create(ADiffs: TVectorShapeDiffList);
var
  i: Integer;
begin
  setlength(FDiffs, ADiffs.Count);
  for i := 0 to high(FDiffs) do
    FDiffs[i] := ADiffs[i];
end;

constructor TVectorShapeComposedDiff.Create(ADiffs: array of TVectorShapeDiff);
var
  i: Integer;
begin
  setlength(FDiffs, length(ADiffs));
  for i := 0 to high(FDiffs) do
    FDiffs[i] := ADiffs[i];
end;

constructor TVectorShapeComposedDiff.Create(AStartShape: TVectorShape);
begin
  raise exception.Create(errCannotBeComputedFromShape);
end;

destructor TVectorShapeComposedDiff.Destroy;
var
  i: Integer;
begin
  for i := 0 to high(FDiffs) do
    FDiffs[i].Free;
  FDiffs := nil;
  inherited Destroy;
end;

procedure TVectorShapeComposedDiff.ComputeDiff(AEndShape: TVectorShape);
begin
  raise exception.Create(errCannotBeComputedFromShape);
end;

procedure TVectorShapeComposedDiff.Apply(AStartShape: TVectorShape);
var
  i: Integer;
begin
  AStartShape.BeginUpdate;
  for i := 0 to high(FDiffs) do
    FDiffs[i].Apply(AStartShape);
  AStartShape.EndUpdate;
end;

procedure TVectorShapeComposedDiff.Unapply(AEndShape: TVectorShape);
var
  i: Integer;
begin
  AEndShape.BeginUpdate;
  for i := high(FDiffs) downto 0 do
    FDiffs[i].Unapply(AEndShape);
  AEndShape.EndUpdate;
end;

function TVectorShapeComposedDiff.CanAppend(ADiff: TVectorShapeDiff): boolean;
var
  next: TVectorShapeComposedDiff;
  i: Integer;
begin
  if ADiff is TVectorShapeComposedDiff then
  begin
    next := TVectorShapeComposedDiff(ADiff);
    for i := 0 to high(next.FDiffs) do
      if not CanAppend(next.FDiffs[i]) then exit(false);
    result := true;
  end else
  begin
    for i := high(FDiffs) downto 0 do
      if FDiffs[i].CanAppend(ADiff) then exit(true);
    exit(false);
  end;
end;

procedure TVectorShapeComposedDiff.Append(ADiff: TVectorShapeDiff);
var
  next: TVectorShapeComposedDiff;
  i: Integer;
begin
  if ADiff is TVectorShapeComposedDiff then
  begin
    next := TVectorShapeComposedDiff(ADiff);
    for i := 0 to high(next.FDiffs) do
      Append(next.FDiffs[i]);
  end else
  begin
    for i := high(FDiffs) downto 0 do
      if FDiffs[i].CanAppend(ADiff) then
      begin
        FDiffs[i].Append(ADiff);
        exit;
      end;
  end;
end;

function TVectorShapeComposedDiff.IsIdentity: boolean;
var
  i: Integer;
begin
  for i := 0 to high(FDiffs) do
    if not FDiffs[i].IsIdentity then exit(false);
  result := true;
end;

function TVectorShapeComposedDiff.GetMultiselection: TCustomMultiSelectionDiff;
var
  i: Integer;
begin
  for i := 0 to high(FDiffs) do
    if FDiffs[i] is TCustomMultiSelectionDiff then
      exit(TCustomMultiSelectionDiff(FDiffs[i]));
  result := nil;
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
      FOriginal.DeselectShapes;
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

function TVectorShape.GetIsSlow(const AMatrix: TAffineMatrix): boolean;
begin
  result := false;
end;

function TVectorShape.GetGenericCost: integer;
begin
  if vsfBackFill in Fields then
  begin
    case BackFill.FillType of
    vftGradient: result := 25;
    vftTexture: result := 10;
    vftSolid: result := 4;
    else {vftNone} result := 1;
    end
  end else
  if vsfPenStyle in Fields then
  begin
    if PenStyleEqual(PenStyle, SolidPenStyle) or
       PenStyleEqual(PenStyle, ClearPenStyle) then
       result := 1
    else
      result := 2;
  end else
    result := 1;
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

function TVectorShape.GetAsMultishape: IVectorMultishape;
begin
  result := nil;
end;

procedure TVectorShape.Transform(const AMatrix: TAffineMatrix);
var
  zoom: Single;
begin
  if IsAffineMatrixIdentity(AMatrix) then exit;
  BeginUpdate;
  TransformFrame(AMatrix);
  TransformFill(AMatrix, False);
  zoom := (VectLen(AMatrix[1,1],AMatrix[2,1])+VectLen(AMatrix[1,2],AMatrix[2,2]))/2;
  if vsfPenWidth in Fields then PenWidth := zoom*PenWidth;
  if vsfOutlineWidth in Fields then OutlineWidth := zoom*OutlineWidth;
  EndUpdate;
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
  if Stroker.JoinStyle = AValue then exit;
  BeginUpdate(TVectorShapeCommonDiff);
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

function TVectorShape.LoadTexture(AStorage: TBGRACustomOriginalStorage;
  AName: string): TBGRABitmap;
var
  texId: Integer;
  pointerData: RawByteString;
begin
  if FStoreTexturePointer then
  begin
    result := nil;
    pointerData := AStorage.RawString[AName+'-ptr'];
    if length(pointerData)<>sizeof(result) then
      raise exception.Create(errInvalidStoredPointer);
    move(pointerData[1],result,sizeof(result));
  end else
  if Assigned(Container) then
  begin
    texId := AStorage.Int[AName+'-id'];
    result := Container.GetTexture(texId);
  end else
    raise exception.Create(errUndefinedContainer);
end;

procedure TVectorShape.SaveTexture(AStorage: TBGRACustomOriginalStorage;
  AName: string; AValue: TBGRABitmap);
var
  texId: Integer;
  pointerData: RawByteString;
begin
  if FStoreTexturePointer then
  begin
    setlength(pointerData, sizeof(AValue));
    move(AValue, pointerData[1], length(pointerData));
    AStorage.RawString[AName+'-ptr'] := pointerData;
  end else
  if Assigned(Container) then
  begin
    texId := Container.GetTextureId(AValue);
    AStorage.Int[AName+'-id'] := texId;
  end else
    raise exception.Create(errUndefinedContainer);
end;

procedure TVectorShape.LoadFill(AStorage: TBGRACustomOriginalStorage;
  AObjectName: string; var AValue: TVectorialFill);
var
  obj: TBGRACustomOriginalStorage;
  tex: TBGRABitmap;
  texOpacity: integer;
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
           tex := LoadTexture(obj, 'tex');
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
           AValue.SetTexture(tex, AffineMatrix(xAxis,yAxis,origin), texOpacity, repetition)
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
        SaveTexture(obj, 'tex', AValue.Texture);
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

function TVectorShape.MultiUsermodes: TVectorShapeUsermodes;
var
  f: TVectorShapeFields;
begin
  result := [vsuEdit];
  f := MultiFields;
  if vsfBackFill in f then result += [vsuEditBackFill];
  if vsfPenFill in f then result += [vsuEditPenFill];
  if vsfOutlineFill in f then result += [vsuEditOutlineFill];
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
  if Assigned(FContainer) then raise exception.Create(errContainerAlreadyAssigned);
  FContainer:=AValue;
end;

function TVectorShape.GetIsUpdating: boolean;
begin
  result := FUpdateCount > 0;
end;

function TVectorShape.GetOutlineWidth: single;
begin
  result := FOutlineWidth;
end;

function TVectorShape.GetFill(var AFillVariable: TVectorialFill): TVectorialFill;
begin
  if AFillVariable = nil then
  begin
    AFillVariable := TVectorialFill.Create;
    AFillVariable.OnChange := @FillChange;
    AFillVariable.OnBeforeChange := @FillBeforeChange;
  end;
  result := AFillVariable;
end;

procedure TVectorShape.SetFill(var AFillVariable: TVectorialFill;
  AValue: TVectorialFill; AUpdate: boolean);
var
  sharedTex: TBGRABitmap;
  freeTex: Boolean;
begin
  if Assigned(AFillVariable) then
  begin
    if AFillVariable.Equals(AValue) then exit;
  end else
    if AValue=nil then exit;
  if not AUpdate then FFillChangeWithoutUpdate := true;
  freeTex := Assigned(AFillVariable) and Assigned(AFillVariable.Texture) and
    not (Assigned(AValue) and (AValue.FillType = vftTexture) and (AValue.Texture = AFillVariable.Texture));
  if AValue = nil then
  begin
    AFillVariable.Clear; //trigger event
    FreeAndNil(AFillVariable);
  end else
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
  if not AUpdate then FFillChangeWithoutUpdate := false;
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
  BeginUpdate(TVectorShapeCommonDiff);
  FOutlineWidth:=AValue;
  EndUpdate;
end;

procedure TVectorShape.SetOutlineFill(AValue: TVectorialFill);
begin
  SetFill(FOutlineFill, AValue, True);
end;

function TVectorShape.GetIsBack: boolean;
begin
  result := Assigned(Container) and (Container.IndexOfShape(self)=0);
end;

function TVectorShape.GetIsFollowingMouse: boolean;
begin
  result := false;
end;

function TVectorShape.GetPenVisible(AAssumePenFill: boolean): boolean;
var
  f: TVectorShapeFields;
begin
  f := Fields;
  result := (vsfPenFill in f) and (not PenFill.IsFullyTransparent or AAssumePenFill);
  if result and (vsfPenWidth in f) then result := result and (PenWidth>0);
  if result and (vsfPenStyle in f) then result := result and not IsClearPenStyle(PenStyle);
end;

function TVectorShape.GetPenVisibleNow: boolean;
begin
  result := GetPenVisible(False);
end;

function TVectorShape.GetBackVisible: boolean;
begin
  result := (vsfBackFill in Fields) and not BackFill.IsFullyTransparent;
end;

function TVectorShape.GetOutlineVisible: boolean;
begin
  result := (vsfOutlineFill in Fields) and not OutlineFill.IsFullyTransparent and
            (not (vsfOutlineWidth in Fields) or (OutlineWidth > 0));
end;

function TVectorShape.AppendVectorialFillToSVGDefs(AFill: TVectorialFill; const AMatrix: TAffineMatrix;
  ADefs: TSVGDefine; ANamePrefix: string): string;
var
  grad: TSVGGradient;
begin
  if AFill.FillType = vftGradient then
  begin
    grad := AFill.Gradient.AddToSVGDefs(AMatrix, ADefs) as TSVGGradient;
    if grad = nil then exit('');
    grad.ID := ANamePrefix + 'grad' + inttostr(Id);
    result := grad.ID;
  end else
    result := '';
end;

procedure TVectorShape.ApplyStrokeStyleToSVG(AElement: TSVGElement; ADefs: TSVGDefine);
var ps: array of single;
  i: Integer;
  fillId: String;
begin
  if PenVisible then
  begin
    if IsAffineMatrixInversible(AElement.matrix[cuPixel]) then
      fillId := AppendVectorialFillToSVGDefs(PenFill,
        AffineMatrixInverse(AElement.matrix[cuPixel]), ADefs, 'stroke')
      else fillId := '';
    if fillId <> '' then
      AElement.stroke:= 'url(#'+fillId+')'
      else AElement.strokeColor := PenColor;

    if IsSolidPenStyle(PenStyle) then
      AElement.strokeDashArrayNone else
      begin
        setlength(ps, length(PenStyle));
        for i := 0 to high(ps) do
          ps[i] := PenStyle[i] * PenWidth;
        AElement.strokeDashArrayF := ps;
      end;
    AElement.strokeLineJoinLCL := JoinStyle;
    AElement.strokeWidth := FloatWithCSSUnit(PenWidth, cuCustom);
  end else
    AElement.strokeNone;
end;

procedure TVectorShape.ApplyFillStyleToSVG(AElement: TSVGElement; ADefs: TSVGDefine);
var
  fillId: String;
begin
  if BackVisible then
  begin
    if IsAffineMatrixInversible(AElement.matrix[cuPixel]) then
      fillId := AppendVectorialFillToSVGDefs(BackFill,
        AffineMatrixInverse(AElement.matrix[cuPixel]), ADefs, 'fill')
      else fillId := '';
    if fillId <> '' then
      AElement.fill:= 'url(#'+fillId+')'
      else AElement.fillColor := BackFill.AverageColor;
  end
  else AElement.fillNone;
end;

procedure TVectorShape.TransformFill(const AMatrix: TAffineMatrix; ABackOnly: boolean);
begin
  BeginUpdate;
  if vsfBackFill in Fields then BackFill.Transform(AMatrix);
  if not ABackOnly then
  begin
    if vsfPenFill in Fields then PenFill.Transform(AMatrix);
    if vsfOutlineFill in Fields then OutlineFill.Transform(AMatrix);
  end;
  EndUpdate;
end;

function TVectorShape.AllowShearTransform: boolean;
begin
  result := true;
end;

function TVectorShape.MultiFields: TVectorShapeFields;
begin
  result := Fields;
end;

function TVectorShape.GetIsFront: boolean;
begin
  result := Assigned(Container) and (Container.IndexOfShape(self)=Container.ShapeCount-1);
end;

function TVectorShape.GetOutlineFill: TVectorialFill;
begin
  result := GetFill(FOutlineFill);
end;

procedure TVectorShape.BeginUpdate(ADiffHandler: TVectorShapeDiffAny);
begin
  if FUpdateCount = 0 then
  begin
    FBoundsBeforeUpdate := GetRenderBounds(InfiniteRect, AffineMatrixIdentity);
    Inc(FRenderIteration);
  end;
  inc(FUpdateCount);
  if ADiffHandler<>nil then AddDiffHandler(ADiffHandler);
end;

procedure TVectorShape.EndUpdate;
var
  i: Integer;
  comp: TVectorShapeComposedDiff;
begin
  if FUpdateCount > 0 then
  begin
    dec(FUpdateCount);
    if FUpdateCount = 0 then
    begin
      if Assigned(FDiffs) and (FDiffs.Count > 0) then
      begin
        for i := 0 to FDiffs.Count-1 do
          FDiffs[i].ComputeDiff(self);
        if FDiffs.Count = 1 then
          DoOnChange(FBoundsBeforeUpdate, FDiffs[0])
        else
        begin
          comp := TVectorShapeComposedDiff.Create(FDiffs);
          DoOnChange(FBoundsBeforeUpdate, comp);
        end;
        FDiffs.Clear;
      end else
        DoOnChange(FBoundsBeforeUpdate, nil);
    end;
  end;
end;

procedure TVectorShape.FillFit;
var
  box: TAffineBox;
begin
  BeginUpdate;
  box := SuggestGradientBox(AffineMatrixIdentity);
  if vsfPenFill in Fields then PenFill.FitGeometry(box);
  if vsfBackFill in Fields then BackFill.FitGeometry(box);
  if vsfOutlineFill in Fields then OutlineFill.FitGeometry(box);
  EndUpdate;
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

procedure TVectorShape.DoOnChange(ABoundsBefore: TRectF; ADiff: TVectorShapeDiff);
var
  boundsAfter: TRectF;
begin
  if Assigned(FOnChange) then
  begin
    boundsAfter := GetRenderBounds(InfiniteRect, AffineMatrixIdentity);
    FOnChange(self, boundsAfter.Union(ABoundsBefore, true), ADiff);
  end else
    ADiff.Free;
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

function TVectorShape.ComputeStrokeEnvelope(APoints: ArrayOfTPointF;
  AClosed: boolean; AWidth: single): ArrayOfTPointF;
var
  opt: TBGRAPolyLineOptions;
begin
  opt := [];
  if AClosed then include(opt, plCycle);
  result := ComputeWidePolyPolylinePoints(APoints, AWidth, BGRABlack, pecRound, pjsMiter, SolidPenStyle, opt);
end;

function TVectorShape.GetStroker: TBGRAPenStroker;
begin
  if FStroker = nil then
  begin
    FStroker := TBGRAPenStroker.Create;
    FStroker.MiterLimit:= 2;
  end;
  result := FStroker;
end;

procedure TVectorShape.FillChange(ASender: TObject; var ADiff: TCustomVectorialFillDiff);
var
  field: TVectorShapeField;
  r: TRectF;
begin
  r := FFillBeforeChangeBounds;
  FFillBeforeChangeBounds := EmptyRectF;
  if FFillChangeWithoutUpdate then exit;
  //if shape is not being updated, send the fill diff as such
  if not IsUpdating then
  begin
    inc(FRenderIteration);
    if ASender = FPenFill then field := vsfPenFill
    else if ASender = FBackFill then field := vsfBackFill
    else if ASender = FOutlineFill then field := vsfOutlineFill
    else
    begin
      ADiff.Free;
      DoOnChange(r, nil);
      exit;
    end;
    if Assigned(ADiff) then
    begin
      DoOnChange(r, TVectorShapeEmbeddedFillDiff.Create(field, ADiff));
      ADiff := nil;
    end else
      DoOnChange(r, nil);
  end else
    AddFillDiffHandler(ASender as TVectorialFill, ADiff);
end;

procedure TVectorShape.FillBeforeChange(ASender: TObject);
begin
  FFillBeforeChangeBounds := GetRenderBounds(InfiniteRect, AffineMatrixIdentity);
end;

function TVectorShape.OpenRenderStorage(ACreateIfNecessary: boolean): TShapeRenderStorage;
begin
  if ACreateIfNecessary then
    result := TShapeRenderStorage.OpenOrCreate(Container.RenderStorage, Id)
  else
    result := TShapeRenderStorage.Open(Container.RenderStorage, Id);
end;

procedure TVectorShape.UpdateRenderStorage(ARenderBounds: TRect; AImage: TBGRACustomBitmap);
var
  imgStream: TMemoryStream;
  shapeStorage: TShapeRenderStorage;
begin
  if CanHaveRenderStorage then
  begin
    shapeStorage := OpenRenderStorage(true);
    shapeStorage.persistent.Int['iteration'] := FRenderIteration;
    shapeStorage.persistent.Rectangle['bounds'] := ARenderBounds;
    if Assigned(AImage) then
    begin
      imgStream := TMemoryStream.Create;
      AImage.Serialize(imgStream);
      shapeStorage.persistent.WriteFile('image.data', imgStream, false, true);
                                                   //will be compressed when saving
    end else
      shapeStorage.persistent.RemoveFile('image.data');
    shapeStorage.Close;
  end;
end;

procedure TVectorShape.DiscardRenderStorage;
begin
  if CanHaveRenderStorage then
    TShapeRenderStorage.Discard(Container.RenderStorage, Id);
end;

procedure TVectorShape.RetrieveRenderStorage(AMatrix: TAffineMatrix; out
  ARenderBounds: TRect; out AImage: TBGRABitmap);
var
  stream: TStream;
  shapeStorage: TShapeRenderStorage;
begin
  ARenderBounds := EmptyRect;
  AImage := nil;
  if Assigned(Container) and Assigned(Container.RenderStorage) and (Container.RenderStorage.AffineMatrix['last-matrix']=AMatrix) then
  begin
    shapeStorage := TShapeRenderStorage.Open(Container.RenderStorage, Id);
    if Assigned(shapeStorage.persistent) then
    begin
      if shapeStorage.persistent.Int['iteration'] = FRenderIteration then
      begin
        ARenderBounds := shapeStorage.persistent.Rectangle['bounds'];
        stream := shapeStorage.persistent.GetFileStream('image.data') ;
        if Assigned(stream) and (stream.Size > 0) then
        begin
          stream.Position:= 0;
          AImage := TBGRABitmap.Create;
          AImage.Deserialize(stream);
        end;
      end;
    end;
    shapeStorage.Close;
  end;
end;

function TVectorShape.CanHaveRenderStorage: boolean;
begin
  result := (Id <> 0) and Assigned(Container) and Assigned(Container.RenderStorage);
end;

function TVectorShape.AddDiffHandler(AClass: TVectorShapeDiffAny): TVectorShapeDiff;
var
  i: Integer;
begin
  result := nil;
  if not IsUpdating then
    raise exception.Create(errDiffHandlerOnlyDuringUpdate);
  if Assigned(FOnChange) then
  begin
    if FDiffs = nil then FDiffs := TVectorShapeDiffList.Create;
    for i := 0 to FDiffs.Count-1 do
      if FDiffs[i] is AClass then exit(FDiffs[i]);
    result := AClass.Create(self);
    FDiffs.Add(result);
  end;
end;

procedure TVectorShape.AddFillDiffHandler(AFill: TVectorialFill; ADiff: TCustomVectorialFillDiff);
var
  h: TVectorShapeCommonFillDiff;
begin
  if Assigned(AFill) and Assigned(ADiff) then
  begin
    //make sure there is a handler for fill diff
    if GetDiffHandler(TVectorShapeCommonFillDiff)=nil then
    begin
      h := AddDiffHandler(TVectorShapeCommonFillDiff) as TVectorShapeCommonFillDiff;
      if Assigned(h) then
      begin
        //handler is initialized with current fill that is already changed
        //so we need to fix the start value using diff
        if AFill = FPenFill then
        begin
          if h.FStartPenFill=nil then h.FStartPenFill := TVectorialFill.Create;
          ADiff.Unapply(h.FStartPenFill)
        end
        else if AFill = FBackFill then
        begin
          if h.FStartBackFill=nil then h.FStartBackFill := TVectorialFill.Create;
          ADiff.Unapply(h.FStartBackFill);
        end
        else if AFill = FOutlineFill then
        begin
          if h.FStartOutlineFill=nil then h.FStartOutlineFill := TVectorialFill.Create;
          ADiff.Unapply(h.FStartOutlineFill);
        end;
      end;
    end;
  end;
end;

function TVectorShape.GetDiffHandler(AClass: TVectorShapeDiffAny): TVectorShapeDiff;
var
  i: Integer;
begin
  if Assigned(FDiffs) then
  begin
    for i := 0 to FDiffs.Count-1 do
      if FDiffs[i] is AClass then exit(FDiffs[i]);
  end;
  result := nil;
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
  BeginUpdate(TVectorShapeCommonDiff);
  FPenWidth := AValue;
  EndUpdate;
end;

procedure TVectorShape.SetPenStyle(AValue: TBGRAPenStyle);
begin
  if PenStyleEqual(AValue, PenStyle) then exit;
  BeginUpdate(TVectorShapeCommonDiff);
  Stroker.CustomPenStyle := AValue;
  EndUpdate;
end;

procedure TVectorShape.SetBackFill(AValue: TVectorialFill);
begin
  SetFill(FBackFill, AValue, True);
end;

procedure TVectorShape.SetPenFill(AValue: TVectorialFill);
begin
  SetFill(FPenFill, AValue, True);
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
  FFillBeforeChangeBounds := EmptyRectF;
end;

class function TVectorShape.CreateFromStorage(
  AStorage: TBGRACustomOriginalStorage; AContainer: TVectorOriginal): TVectorShape;
var
  objClassName: RawByteString;
  shapeClass: TVectorShapeAny;
begin
  objClassName := AStorage.RawString['class'];
  if objClassName = '' then raise exception.Create(rsShapeClassNotSpecified);
  shapeClass:= GetVectorShapeByStorageClassName(objClassName);
  if shapeClass = nil then raise exception.Create(StringReplace(rsUnknownShapeClass, '%1', objClassName, []));
  result := shapeClass.Create(AContainer);
  result.LoadFromStorage(AStorage);
end;

destructor TVectorShape.Destroy;
var
  i: Integer;
begin
  FreeAndNil(FStroker);
  FreeAndNil(FPenFill);
  FreeAndNil(FBackFill);
  FreeAndNil(FOutlineFill);
  if Assigned(FDiffs) then
    for i := 0 to FDiffs.Count-1 do
      FDiffs[i].Free;
  FreeAndNil(FDiffs);
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

function TVectorShape.PointInBack(APoint: TPointF): boolean;
begin
  result := false;
end;

function TVectorShape.PointInPen(APoint: TPointF): boolean;
begin
  result := false;
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
    if vsfOutlineFill in f then LoadFill(AStorage, 'outline', FOutlineFill);
    if vsfOutlineWidth in f then OutlineWidth := AStorage.FloatDef['outline-width', DefaultShapeOutlineWidth];
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
  if OutlineVisible then
  begin
    if vsfOutlineFill in f then SaveFill(AStorage, 'outline', FOutlineFill);
    if vsfOutlineWidth in f then AStorage.Float['outline-width'] := FOutlineWidth
    else AStorage.RemoveAttribute('outline-width');
  end else
  begin
    AStorage.RemoveObject('outline-fill');
    AStorage.RemoveAttribute('outline-color');
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
    movedShapeBounds := self.GetAlignBounds(InfiniteRect, AffineMatrixIdentity);
    while idx < Container.ShapeCount-2 do
    begin
      otherShapeBounds := Container.Shape[idx+1].GetAlignBounds(InfiniteRect, AffineMatrixIdentity);
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
    movedShapeBounds := self.GetAlignBounds(InfiniteRect, AffineMatrixIdentity);
    while idx > 1 do
    begin
      otherShapeBounds := Container.Shape[idx-1].GetAlignBounds(InfiniteRect, AffineMatrixIdentity);
      if movedShapeBounds.IntersectsWith(otherShapeBounds) then break;
      dec(idx);
    end;
  end;
  dec(idx);
  Container.MoveShapeToIndex(sourceIdx, idx);
end;

procedure TVectorShape.Remove;
var handled: boolean;
begin
  if Assigned(OnRemoveQuery) then
  begin
    handled := false;
    OnRemoveQuery(self, handled);
    if handled then exit;
  end;
  if Assigned(Container) then Container.RemoveShape(self)
  else raise exception.Create(errUndefinedContainer);
end;

procedure TVectorShape.AlignHorizontally(AAlign: TAlignment;
  const AMatrix: TAffineMatrix; const ABounds: TRect);
var
  sb: TRectF;
  m: TAffineMatrix;
begin
  if not IsAffineMatrixInversible(AMatrix) then exit;
  sb := GetAlignBounds(ABounds, AMatrix);
  case AAlign of
  taRightJustify: m := AffineMatrixTranslation(ABounds.Right-sb.Right,0);
  taCenter: m := AffineMatrixTranslation((ABounds.Left+ABounds.Right-sb.Left-sb.Right)/2,0);
  else m := AffineMatrixTranslation(ABounds.Left-sb.Left,0);
  end;
  AlignTransform(AffineMatrixInverse(AMatrix)*m*AMatrix);
end;

procedure TVectorShape.AlignVertically(AAlign: TTextLayout;
  const AMatrix: TAffineMatrix; const ABounds: TRect);
var
  sb: TRectF;
  m: TAffineMatrix;
begin
  if not IsAffineMatrixInversible(AMatrix) then exit;
  sb := GetAlignBounds(ABounds, AMatrix);
  case AAlign of
  tlBottom: m := AffineMatrixTranslation(0,ABounds.Bottom-sb.Bottom);
  tlCenter: m := AffineMatrixTranslation(0,(ABounds.Top+ABounds.Bottom-sb.Top-sb.Bottom)/2);
  else m := AffineMatrixTranslation(0,ABounds.Top-sb.Top);
  end;
  AlignTransform(AffineMatrixInverse(AMatrix)*m*AMatrix);
end;

function TVectorShape.GetAlignBounds(const ALayoutRect: TRect;
  const AMatrix: TAffineMatrix): TRectF;
begin
  result := GetRenderBounds(ALayoutRect, AMatrix, []);
end;

procedure TVectorShape.AlignTransform(const AMatrix: TAffineMatrix);
begin
  Transform(AMatrix);
end;

function TVectorShape.Duplicate: TVectorShape;
var temp: TBGRAMemOriginalStorage;
  shapeClass: TVectorShapeAny;
begin
  shapeClass:= GetVectorShapeByStorageClassName(StorageClassName);
  if shapeClass = nil then raise exception.Create(StringReplace(rsUnknownShapeClass, '%1', StorageClassName, []));

  result := nil;
  temp := TBGRAMemOriginalStorage.Create;
  FStoreTexturePointer:= true;
  try
    SaveToStorage(temp);
    result := shapeClass.Create(nil);
    result.FStoreTexturePointer := true;
    result.LoadFromStorage(temp);
  finally
    temp.Free;
    FStoreTexturePointer:= false;
    if assigned(result) then
      result.FStoreTexturePointer := false;
  end;
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

function TVectorOriginal.InternalInsertShape(AShape: TVectorShape;
  AIndex: integer): TRectF;
var
  texs: ArrayOfBGRABitmap;
  i: Integer;
begin
  if AShape = nil then raise exception.Create(errUnexpectedNil);
  if AShape.Container <> self then
  begin
    if AShape.Container = nil then
      AShape.Container := self
    else
      raise exception.Create(errContainerMismatch);
  end;
  if (AIndex < 0) or (AIndex > FShapes.Count) then
    raise exception.Create(rsIndexOutOfBounds);
  FShapes.Insert(AIndex, AShape);
  texs := AShape.GetUsedTextures;
  for i := 0 to high(texs) do AddTexture(texs[i]);
  AShape.OnChange := @OnShapeChange;
  AShape.OnEditingChange := @OnShapeEditingChange;
  DiscardFrozenShapes;
  result := AShape.GetRenderBounds(InfiniteRect, AffineMatrixIdentity);
end;

function TVectorOriginal.InternalInsertShapeRange(AShapes: TVectorShapes;
  AIndex: integer): TRectF;
var
  i: Integer;
  r: TRectF;
begin
  result := EmptyRectF;
  if Assigned(AShapes) then
    for i := 0 to AShapes.Count-1 do
    begin
      r := InternalInsertShape(AShapes[i], AIndex+i);
      if not r.IsEmpty then
      begin
        if result.IsEmpty then result := r
        else result := result.Union(r);
      end;
    end;
end;

function TVectorOriginal.InternalDeleteShapeRange(AStartIndex, ACount: integer): TRectF;
var
  r: TRectF;
  s: TVectorShape;
  i: Integer;
begin
  result := EmptyRectF;
  if (AStartIndex < 0) or (AStartIndex+ACount > ShapeCount) then
    raise exception.Create(rsIndexOutOfBounds);
  for i := AStartIndex to AStartIndex+ACount-1 do
    if Shape[i].FRemoving then
      raise exception.Create(errAlreadyRemovingShape);
  for i := AStartIndex to AStartIndex+ACount-1 do Shape[i].FRemoving := true;
  for i := AStartIndex to AStartIndex+ACount-1 do DeselectShape(i);
  for i := AStartIndex+ACount-1 downto AStartIndex do
  begin
    s := Shape[i];
    s.OnChange := nil;
    s.OnEditingChange := nil;
    r := s.GetRenderBounds(InfiniteRect, AffineMatrixIdentity);
    result := TRectF.Union(result,r,True);
    FShapes.Delete(i);
    FDeletedShapes.Add(s);
    s.FRemoving:= false;
  end;
  DiscardFrozenShapes;
end;

function TVectorOriginal.GetNewShapeId: integer;
begin
  inc(FLastShapeId);
  result := FLastShapeId;
end;

function TVectorOriginal.GetShape(AIndex: integer): TVectorShape;
begin
  result := FShapes[AIndex];
end;

procedure TVectorOriginal.MultiSelection_SelectionChange(Sender: TObject);
begin
  if FMultiselection = FSelectedShape then
  begin
    DiscardFrozenShapes;
    NotifyEditorChange;
  end;
end;

procedure TVectorOriginal.FreeDeletedShapes;
var
  i: Integer;
begin
  for i := 0 to FDeletedShapes.Count-1 do
    FDeletedShapes[i].Free;
  FDeletedShapes.Clear
end;

procedure TVectorOriginal.OnShapeChange(ASender: TObject; ABounds: TRectF; ADiff: TVectorShapeDiff);
var
  embed: TVectorOriginalShapeDiff;
  idxShape: Integer;
begin
  if ASender <> FSelectedShape then DiscardFrozenShapes;
  if DiffExpected and Assigned(ADiff) then
  begin
    if ASender = FMultiselection then
      idxShape := -2
      else idxShape := IndexOfShape(ASender as TVectorShape);
    embed := TVectorOriginalShapeDiff.Create(idxShape, ADiff);
    ADiff := nil;
    NotifyChange(ABounds, embed);
  end else
    NotifyChange(ABounds);
  ADiff.Free;
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
  //note that there are still shapes that could use textures
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
  if VectorMultiselectionFactory <> nil then
  begin
    FMultiselection := VectorMultiselectionFactory.Create(self);
    FMultiselection.Id := -2;
    FMultiselection.OnChange := @OnShapeChange;
    FMultiselection.OnEditingChange := @OnShapeEditingChange;
    FMultiselection.GetAsMultishape.SetOnSelectionChange(@MultiSelection_SelectionChange);
  end
    else FMultiselection := nil;
end;

destructor TVectorOriginal.Destroy;
var
  i: Integer;
begin
  FMultiselection.Free;
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
    DeselectShapes;
    for i := 0 to FShapes.Count-1 do
      FDeletedShapes.Add(FShapes[i]);
    FShapes.Clear;
    FLastShapeId:= 0;
    ClearTextures;
    NotifyChange;
  end;
end;

function TVectorOriginal.ConvertToSVG(const AMatrix: TAffineMatrix; out AOffset: TPoint): TObject;
var
  svg: TBGRASVG;
  rb: TRect;
  vb: TSVGViewBox;
  i: Integer;
  sCopy: TVectorShape;
  m: TAffineMatrix;
  defs: TSVGDefine;
begin
  m := AffineMatrixTranslation(0.5, 0.5) * AMatrix;
  svg := TBGRASVG.Create;
  defs := svg.Content.AppendDefine;
  result := svg;
  rb := GetRenderBounds(InfiniteRect, AffineMatrixIdentity);
  svg.WidthAsPixel:= rb.Width;
  svg.HeightAsPixel := rb.Height;
  AOffset := rb.TopLeft;
  vb.min := PointF(rb.Left, rb.Top);
  vb.size := PointF(rb.Width, rb.Height);
  svg.ViewBox := vb;
  for i := 0 to ShapeCount-1 do
  begin
    if not IsAffineMatrixIdentity(m) then
    begin
      sCopy := Shape[i].Duplicate;
      try
        sCopy.Transform(m);
        sCopy.AppendToSVG(svg.Content, defs);
      finally
        sCopy.Free;
      end;
    end else
      Shape[i].AppendToSVG(svg.Content, defs);
  end;
  if defs.Content.ElementCount = 0 then
    svg.Content.RemoveElement(defs);
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
begin
  result := ShapeCount;
  InsertShape(AShape, result);
end;

function TVectorOriginal.AddShape(AShape: TVectorShape;
  AUsermode: TVectorShapeUsermode): integer;
begin
  result := AddShape(AShape);
  AShape.Usermode:= AUsermode;
  SelectShape(result);
end;

function TVectorOriginal.AddShapes(AShapes: TVectorShapes): integer;
begin
  result := ShapeCount;
  InsertShapes(AShapes, result);
end;

procedure TVectorOriginal.InsertShape(AShape: TVectorShape; AIndex: integer);
var
  newShapes: TVectorShapes;
begin
  newShapes := TVectorShapes.Create;
  newShapes.Add(AShape);
  ReplaceShapeRange(AIndex,0,newShapes);
  newShapes.Free;
end;

procedure TVectorOriginal.InsertShapes(AShapes: TVectorShapes;
  AIndex: integer);
begin
  ReplaceShapeRange(AIndex, 0, AShapes);
end;

function TVectorOriginal.RemoveShape(AShape: TVectorShape): boolean;
var
  idx: LongInt;
  multiSel: IVectorMultishape;
  startIndex, endIndex, nextIndex, i, selCount: Integer;
begin
  if AShape.FRemoving then exit(false);
  if (AShape = FMultiselection) and Assigned(FMultiselection) then
  begin
    multiSel := FMultiselection.GetAsMultishape;
    selCount := multiSel.ShapeCount;
    if selCount = 0 then exit;
    endIndex := IndexOfShape(multiSel.GetShape(selCount-1));
    startIndex := endIndex;
    i := selCount-2;
    while i >= 0 do
    begin
      nextIndex := IndexOfShape(multiSel.GetShape(i));
      if nextIndex < startIndex-1 then
      begin
        DeleteShapeRange(startIndex, endIndex-startIndex+1);
        endIndex := nextIndex;
        startIndex := endIndex;
      end else
        startIndex := nextIndex;
      dec(i);
    end;
    DeleteShapeRange(startIndex, endIndex-startIndex+1);
  end else
  begin
    idx := FShapes.IndexOf(AShape);
    if idx = -1 then exit(false);
    DeleteShapeRange(idx, 1);
    result := true;
  end;
end;

procedure TVectorOriginal.DeleteShape(AIndex: integer);
begin
  DeleteShapeRange(AIndex, 1);
end;

procedure TVectorOriginal.DeleteShapeRange(AStartIndex, ACount: integer);
begin
  ReplaceShapeRange(AStartIndex, ACount, nil);
end;

procedure TVectorOriginal.ReplaceShape(AIndex: integer; ANewShape: TVectorShape);
var newShapes: TVectorShapes;
begin
  if ANewShape = nil then raise exception.Create(errUnexpectedNil);
  newShapes:= TVectorShapes.Create;
  newShapes.Add(ANewShape);
  ReplaceShapeRange(AIndex, 1, newShapes);
  newShapes.Free;
end;

procedure TVectorOriginal.ReplaceShapeRange(AStartIndex: integer;
  ACountBefore: integer; ANewShapes: TVectorShapes);
var
  rDelete, rInsert: TRectF;
  removed: TVectorShapes;
  diff: TVectorOriginalShapeRangeDiff;
  i: Integer;
begin
  if (AStartIndex < 0) or (AStartIndex+ACountBefore > ShapeCount) then
    raise exception.Create(rsIndexOutOfBounds);

  if Assigned(ANewShapes) then
    for i := 0 to ANewShapes.Count-1 do
      if ANewShapes[i] is VectorMultiselectionFactory then
        raise exception.Create('Cannot add a multiselection as a shape');

  if Assigned(ANewShapes) then
    for i := 0 to ANewShapes.Count-1 do
      ANewShapes[i].Id := GetNewShapeId;

  if DiffExpected then
  begin
    if ACountBefore > 0 then
    begin
      removed := TVectorShapes.Create;
      for i := 0 to ACountBefore-1 do removed.Add(Shape[AStartIndex+i]);
    end else removed := nil;
    diff := TVectorOriginalShapeRangeDiff.Create(AStartIndex, removed, ANewShapes,
              -1,-1);
    removed.Free;
  end else diff := nil;

  rDelete := InternalDeleteShapeRange(AStartIndex, ACountBefore);
  rInsert := InternalInsertShapeRange(ANewShapes, AStartIndex);
  NotifyChange(TRectF.Union(rDelete,rInsert,True), diff);
end;

function TVectorOriginal.SelectShapes(AShapes: TVectorShapes): boolean;
begin
  if AShapes.Count = 0 then result := DeselectShapes
  else if AShapes.Count = 1 then result := SelectShape(AShapes[0])
  else
  begin
    FSelectedShape := FMultiselection;
    if FMultiselection.GetAsMultishape.SetShapes(AShapes) then
      NotifyEditorChange;
  end;
end;

function TVectorOriginal.SelectShape(AIndex: integer; AToggle: boolean): boolean;
begin
  if AIndex=-1 then result := SelectShape(nil, AToggle)
  else
  begin
    if (AIndex < 0) or (AIndex >= FShapes.Count) then
      raise ERangeError.Create(rsIndexOutOfBounds);
    result := SelectShape(FShapes[AIndex], AToggle);
  end;
end;

function TVectorOriginal.SelectShape(AShape: TVectorShape; AToggle: boolean): boolean;
var
  prevSel, newSel: TVectorShape;
  prevMode: TVectorShapeUsermode;
  multiSel: IVectorMultishape;
begin
  result := false;
  //when selecting nothing
  if AShape = nil then
  begin
    if not AToggle then
      result := DeselectShapes;
    exit;
  end;

  //selecting current selection
  if AShape = FSelectedShape then
  begin
    if AToggle then
      result := DeselectShapes;
    exit;
  end;

  //check selected shape exists
  if AShape <> nil then
    if FShapes.IndexOf(AShape)=-1 then
      raise exception.Create(rsShapeNotFound);

  //case of modifying multiselection
  if (FSelectedShape = FMultiselection) and Assigned(FMultiselection) and AToggle then
  begin
    multiSel := FSelectedShape.GetAsMultishape;
    if multiSel.ContainsShape(AShape) then
    begin
      multiSel.RemoveShape(AShape);
      if multiSel.ShapeCount = 0 then
      begin
        FSelectedShape := nil;
        exit(true);
      end else
      if multiSel.ShapeCount > 1 then
        exit(true) else
      begin
        SelectShape(multiSel.GetShape(0));
        exit(true);
      end;
    end else
    begin
      multiSel.AddShape(AShape);
      exit(true);
    end;
  end;

  //changing selection completely
  prevSel := FSelectedShape;
  if Assigned(prevSel) then
  begin
    prevMode := prevSel.Usermode;
    prevSel.Usermode := vsuEdit;
  end else
    prevMode := vsuEdit;

  //becomes a multiselection
  if AToggle and (prevSel <> nil) and Assigned(FMultiselection) then
  begin
    multiSel := FMultiselection.GetAsMultishape;
    multiSel.ClearShapes;
    multiSel.AddShape(prevSel);
    multiSel.AddShape(AShape);
    newSel := FMultiselection;
  end else
  begin
    //otherwise simple selection
    newSel := AShape;
  end;

  //transfering user mode
  if (prevMode = vsuEditBackFill) and (prevMode in newSel.Usermodes) and
    newSel.BackFill.IsEditable then newSel.Usermode:= prevMode;
  if (prevMode = vsuEditPenFill) and (prevMode in newSel.Usermodes) and
    newSel.PenFill.IsEditable then newSel.Usermode:= prevMode;
  if (prevMode = vsuEditOutlineFill) and (prevMode in newSel.Usermodes) and
    newSel.OutlineFill.IsEditable then newSel.Usermode:= prevMode;
  if (prevMode = vsuEditText) and (prevMode in newSel.Usermodes) then
    newSel.Usermode := prevMode;

  FSelectedShape := newSel;
  DiscardFrozenShapes;
  NotifyEditorChange;
  if Assigned(FOnSelectShape) then
    FOnSelectShape(self, FSelectedShape, prevSel);

  if (prevSel = FMultiselection) and Assigned(FMultiselection) then
    FMultiselection.GetAsMultishape.ClearShapes;
end;

function TVectorOriginal.DeselectShapes: boolean;
var
  prev: TVectorShape;
begin
  if SelectedShape = nil then exit(false);

  prev := SelectedShape;
  SelectedShape.Usermode := vsuEdit;
  FSelectedShape := nil;

  if (prev = FMultiselection) and Assigned(FMultiselection) then
    FMultiselection.GetAsMultishape.ClearShapes;

  DiscardFrozenShapes;
  NotifyEditorChange;

  if Assigned(FOnSelectShape) then
    FOnSelectShape(self, nil, prev);
  result := true;
end;

procedure TVectorOriginal.DeselectShape(AIndex: integer);
begin
  if (AIndex >= 0) and (AIndex < ShapeCount) then
    DeselectShape(Shape[AIndex]);
end;

procedure TVectorOriginal.DeselectShape(AShape: TVectorShape);
var
  multiSel: IVectorMultishape;
begin
  if AShape = SelectedShape then DeselectShapes else
  begin
    if (SelectedShape = FMultiselection) and Assigned(FMultiselection) then
    begin
      multiSel := SelectedShape.GetAsMultishape;
      if multiSel.ContainsShape(AShape) then
        multiSel.RemoveShape(AShape);
      if multiSel.ShapeCount = 1 then
        SelectShape(multiSel.GetShape(0));
    end;
  end;
end;

function TVectorOriginal.GetShapesCost: integer;
var
  i: Integer;
begin
  result := 0;
  for i := 0 to ShapeCount-1 do
    inc(result, Shape[i].GetGenericCost);
end;

function TVectorOriginal.PreferDraftMode(AEditor: TBGRAOriginalEditor; const AMatrix: TAffineMatrix): boolean;
begin
  if Assigned(SelectedShape) and Assigned(AEditor) then
  begin
    result := (AEditor.IsMovingPoint or SelectedShape.IsFollowingMouse) and
              SelectedShape.GetIsSlow(AMatrix);
  end else
    result := false;
end;

function TVectorOriginal.MouseClick(APoint: TPointF; ARadius: single; AToggle: boolean): boolean;
var
  i: LongInt;
begin
  for i:= FShapes.Count-1 downto 0 do
    if FShapes[i].PointInShape(APoint) then
    begin
      if SelectedShape <> FShapes[i] then
      begin
        SelectShape(i, AToggle);
        exit(true);
      end else
        exit(false);
    end;
  for i:= FShapes.Count-1 downto 0 do
    if FShapes[i].PointInShape(APoint, ARadius) then
    begin
      if SelectedShape <> FShapes[i] then
      begin
        SelectShape(i, AToggle);
        exit(true);
      end else
        exit(false);
    end;
  if (SelectedShape <> nil) and not AToggle then
  begin
    DeselectShapes;
    exit(true);
  end else
    exit(false);
end;

procedure TVectorOriginal.Render(ADest: TBGRABitmap; ARenderOffset: TPoint; AMatrix: TAffineMatrix;
  ADraft: boolean);
var
  i: Integer;
  idxSelected, newUnfrozenRangeStart, newUnfrozenRangeEnd: LongInt;
  shapeRectF, clipRectF, allRectF: TRectF;
  mOfs: TAffineMatrix;
  multiSel: IVectorMultishape;
  ofsRange: TPoint;
  oldClip: TRect;
begin
  if FSelectedShape <> FMultiselection then
  begin
    idxSelected := FShapes.IndexOf(FSelectedShape);
    if idxSelected = -1 then
    begin
      FSelectedShape := nil;
      newUnfrozenRangeStart := 0;
      newUnfrozenRangeEnd := ShapeCount;
    end else
    begin
      newUnfrozenRangeStart := idxSelected;
      newUnfrozenRangeEnd := idxSelected+1;
    end;
  end else
  if FMultiselection = nil then
  begin
    newUnfrozenRangeStart := 0;
    newUnfrozenRangeEnd := ShapeCount;
  end else
  begin
    multiSel := FMultiselection.GetAsMultishape;
    if multiSel.ShapeCount = 0 then
    begin
      FSelectedShape := nil;
      newUnfrozenRangeStart := 0;
      newUnfrozenRangeEnd := ShapeCount;
    end;
    newUnfrozenRangeStart := IndexOfShape(multiSel.BackShape);
    newUnfrozenRangeEnd := IndexOfShape(multiSel.FrontShape)+1;
  end;
  if (newUnfrozenRangeStart <> FUnfrozenRangeStart) or
     (newUnfrozenRangeEnd <> FUnfrozenRangeEnd) or
     (AMatrix <> FFrozenShapeMatrix) then
    DiscardFrozenShapes;
  with ADest.ClipRect do
    clipRectF := RectF(Left,Top,Right,Bottom);
  mOfs := AffineMatrixTranslation(ARenderOffset.X,ARenderOffset.Y)*AMatrix;
  if FFrozenShapesComputed then
  begin
    if Assigned(FFrozenShapesUnderSelection) then
      ADest.PutImage(ARenderOffset.X-FFrozenShapesRenderOffset.X+FFrozenShapesUnderBounds.Left,
                     ARenderOffset.Y-FFrozenShapesRenderOffset.Y+FFrozenShapesUnderBounds.Top,
                     FFrozenShapesUnderSelection, dmSet);
    for i := FUnfrozenRangeStart to FUnfrozenRangeEnd-1 do
    begin
      shapeRectF := FShapes[i].GetRenderBounds(ADest.ClipRect, mOfs, []);
      if shapeRectF.IntersectsWith(clipRectF) then
      begin
        with shapeRectF do
          oldClip := ADest.IntersectClip(rect(floor(Left), floor(Top), ceil(Right), ceil(Bottom)));
        FShapes[i].Render(ADest, ARenderOffset, AMatrix, ADraft);
        ADest.ClipRect := oldClip;
      end;
    end;
    if Assigned(FFrozenShapesOverSelection) then
      ADest.PutImage(ARenderOffset.X-FFrozenShapesRenderOffset.X+FFrozenShapesOverBounds.Left,
                     ARenderOffset.Y-FFrozenShapesRenderOffset.Y+FFrozenShapesOverBounds.Top,
                     FFrozenShapesOverSelection, dmDrawWithTransparency);
  end else
  begin
    if (newUnfrozenRangeStart > 0) or (newUnfrozenRangeEnd < ShapeCount) then
    begin
      allRectF := rectF(0,0,ADest.Width,ADest.Height);
      FUnfrozenRangeStart := newUnfrozenRangeStart;
      FUnfrozenRangeEnd := newUnfrozenRangeEnd;
      FreeAndNil(FFrozenShapesUnderSelection);
      if FUnfrozenRangeStart > 0 then
      begin
        FFrozenShapesUnderBounds := GetRenderBounds(rect(0,0,ADest.Width,ADest.Height), mOfs,
                                      0, FUnfrozenRangeStart-1);
        FFrozenShapesUnderBounds.Intersect(rect(0,0,ADest.Width,ADest.Height));
        FFrozenShapesUnderSelection := TBGRABitmap.Create(FFrozenShapesUnderBounds.Width, FFrozenShapesUnderBounds.Height);
        ofsRange := Point(ARenderOffset.X - FFrozenShapesUnderBounds.Left,
                          ARenderOffset.Y - FFrozenShapesUnderBounds.Top);
        for i:= 0 to FUnfrozenRangeStart-1 do
        begin
          shapeRectF := FShapes[i].GetRenderBounds(rect(0,0,ADest.Width,ADest.Height), mOfs, []);
          if shapeRectF.IntersectsWith(allRectF) then
          begin
            shapeRectF.Offset(-FFrozenShapesUnderBounds.Left, -FFrozenShapesUnderBounds.Top);
            with shapeRectF do
              oldClip := FFrozenShapesUnderSelection.IntersectClip(rect(floor(Left), floor(Top), ceil(Right), ceil(Bottom)));
            FShapes[i].Render(FFrozenShapesUnderSelection, ofsRange, AMatrix, false);
            FFrozenShapesUnderSelection.ClipRect := oldClip;
          end;
        end;
        ADest.PutImage(FFrozenShapesUnderBounds.Left, FFrozenShapesUnderBounds.Top,
                       FFrozenShapesUnderSelection, dmSet);
      end;
      for i := FUnfrozenRangeStart to FUnfrozenRangeEnd-1 do
        if FShapes[i].GetRenderBounds(ADest.ClipRect, mOfs, []).IntersectsWith(clipRectF) then
          FShapes[i].Render(ADest, ARenderOffset, AMatrix, ADraft);
      FreeAndNil(FFrozenShapesOverSelection);
      if FUnfrozenRangeEnd < FShapes.Count then
      begin
        FFrozenShapesOverBounds := GetRenderBounds(rect(0,0,ADest.Width,ADest.Height), mOfs,
                                     FUnfrozenRangeEnd, FShapes.Count-1);
        FFrozenShapesOverBounds.Intersect(rect(0,0,ADest.Width,ADest.Height));
        FFrozenShapesOverSelection := TBGRABitmap.Create(FFrozenShapesOverBounds.Width, FFrozenShapesOverBounds.Height);
        ofsRange := Point(ARenderOffset.X - FFrozenShapesOverBounds.Left,
                          ARenderOffset.Y - FFrozenShapesOverBounds.Top);
        for i:= FUnfrozenRangeEnd to FShapes.Count-1 do
        begin
          shapeRectF := FShapes[i].GetRenderBounds(rect(0,0,ADest.Width,ADest.Height), mOfs, []);
          if shapeRectF.IntersectsWith(allRectF) then
          begin
            shapeRectF.Offset(-FFrozenShapesOverBounds.Left, -FFrozenShapesOverBounds.Top);
            with shapeRectF do
              oldClip := FFrozenShapesOverSelection.IntersectClip(rect(floor(Left), floor(Top), ceil(Right), ceil(Bottom)));
            FShapes[i].Render(FFrozenShapesOverSelection, ofsRange, AMatrix, false);
            FFrozenShapesOverSelection.ClipRect := oldClip;
          end;
        end;
        ADest.PutImage(FFrozenShapesOverBounds.Left, FFrozenShapesOverBounds.Top,
                       FFrozenShapesOverSelection, dmDrawWithTransparency);
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
    if (FShapes.IndexOf(FSelectedShape)=-1) and
       (FSelectedShape <> FMultiselection) then
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
begin
  result := GetRenderBounds(ADestRect, AMatrix, 0, ShapeCount-1);
end;

function TVectorOriginal.GetRenderBounds(ADestRect: TRect;
  AMatrix: TAffineMatrix; AStartIndex, AEndIndex: integer): TRect;
var
  area, shapeArea: TRectF;
  i: Integer;
  shapeDir: TBGRACustomOriginalStorage;
  useStorage: Boolean;
  iteration: LongInt;
begin
  area:= EmptyRectF;
  useStorage := Assigned(RenderStorage) and (RenderStorage.AffineMatrix['last-matrix']=AMatrix);
  for i:= AStartIndex to AEndIndex do
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

function TVectorOriginal.GetAlignBounds(ADestRect: TRect; AMatrix: TAffineMatrix): TRect;
var
  area, shapeArea: TRectF;
  i: Integer;
begin
  area:= EmptyRectF;
  for i:= 0 to FShapes.Count-1 do
  begin
    shapeArea := FShapes[i].GetAlignBounds(ADestRect, AMatrix);
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
  texName: String;
  loadedShape: TVectorShape;
  idList: array of single;
  texId: integer;
  bmp: TBGRABitmap;
  strErrors: string;
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
        try
          bmp := TBGRABitmap.Create;
          if not texObj.ReadBitmap(texName+'.png', bmp) and
             not texObj.ReadBitmap(texName+'.jpg', bmp) then
             raise exception.Create(errUnableToFindTexture);
          AddTextureWithId(bmp, texId);
        finally
          bmp.FreeReference;
        end;
      end;
    finally
      texObj.Free;
    end;
  end;

  strErrors := '';
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
    except
      on ex: exception do
        AppendStr(strErrors, ex.Message + ' ');
    end;
    shapeObj.Free;
  end;
  for i := 0 to ShapeCount-1 do
    if Shape[i].Id = 0 then
      Shape[i].Id := GetNewShapeId;
  NotifyChange;
  if strErrors <> '' then
    raise exception.Create(errErrorLoadingShape + ': ' + Trim(strErrors));
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

procedure TVectorOriginal.MoveShapeToIndex(AFromIndex, AToIndex: integer);
begin
  MoveShapeToIndex([AFromIndex], [AToIndex]);
end;

procedure TVectorOriginal.MoveShapeToIndex(AFromIndex,
  AToIndex: array of integer);
var
  diff: TVectorOriginalMoveShapeToIndexDiff;
begin
  diff := TVectorOriginalMoveShapeToIndexDiff.Create(AFromIndex, AToIndex);
  if diff.IsIdentity then
  begin
    diff.Free;
    exit;
  end;
  diff.Apply(self, true);
end;

class function TVectorOriginal.StorageClassName: RawByteString;
begin
  result := 'vector';
end;

class function TVectorOriginal.CanConvertToSVG: boolean;
begin
  result := true;
end;

initialization

  RegisterLayerOriginal(TVectorOriginal);

end.

