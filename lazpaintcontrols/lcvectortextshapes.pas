// SPDX-License-Identifier: GPL-3.0-only
unit LCVectorTextShapes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCVectorRectShapes, BGRATextBidi, BGRABitmapTypes, LCVectorOriginal,
  BGRAGraphics, BGRABitmap, BGRALayerOriginal, BGRACanvas2D, LCVectorialFill,
  BGRASVGShapes, BGRASVGType, BGRAUnits;

type
  TTextShape = class;

  { TTextShapeFontDiff }

  TTextShapeFontDiff = class(TVectorShapeDiff)
  protected
    FFontBidiModeBefore: TFontBidiMode;
    FFontEmHeightBefore: single;
    FFontNameBefore: string;
    FFontStyleBefore: TFontStyles;
    FAliasedBefore: boolean;
    FFontBidiModeAfter: TFontBidiMode;
    FFontEmHeightAfter: single;
    FFontNameAfter: string;
    FFontStyleAfter: TFontStyles;
    FAliasedAfter: boolean;
  public
    constructor Create(AStartShape: TVectorShape); override;
    procedure ComputeDiff(AEndShape: TVectorShape); override;
    procedure Apply(AStartShape: TVectorShape); override;
    procedure Unapply(AEndShape: TVectorShape); override;
    procedure Append(ADiff: TVectorShapeDiff); override;
    function IsIdentity: boolean; override;
  end;

  { TTextShapePhongDiff }

  TTextShapePhongDiff = class(TVectorShapeDiff)
  protected
    FAltitudePercentBefore: single;
    FPenPhongBefore: boolean;
    FLightPositionBefore: TPointF;
    FAltitudePercentAfter: single;
    FPenPhongAfter: boolean;
    FLightPositionAfter: TPointF;
  public
    constructor Create(AStartShape: TVectorShape); override;
    procedure ComputeDiff(AEndShape: TVectorShape); override;
    procedure Apply(AStartShape: TVectorShape); override;
    procedure Unapply(AEndShape: TVectorShape); override;
    procedure Append(ADiff: TVectorShapeDiff); override;
    function IsIdentity: boolean; override;
  end;

  { TTextShapeTextDiff }

  TTextShapeTextDiff = class(TVectorShapeDiff)
  protected
    FTextBefore: string;
    FSelStartBefore,FSelEndBefore: integer;
    FVertAlignBefore: TTextLayout;
    FParaAlignBefore: array of TBidiTextAlignment;
    FTextAfter: string;
    FSelStartAfter,FSelEndAfter: integer;
    FVertAlignAfter: TTextLayout;
    FParaAlignAfter: array of TBidiTextAlignment;
  public
    constructor Create(AStartShape: TVectorShape); override;
    procedure ComputeDiff(AEndShape: TVectorShape); override;
    procedure Apply(AStartShape: TVectorShape); override;
    procedure Unapply(AEndShape: TVectorShape); override;
    procedure Append(ADiff: TVectorShapeDiff); override;
    function IsIdentity: boolean; override;
  end;

  { TTextShape }

  TTextShape = class(TCustomRectShape)
  private
    FAliased: boolean;
    FAltitudePercent: single;
    FPenPhong: boolean;
    FPenFillIteration: integer;
    FLightPosition: TPointF;
    FFontBidiMode: TFontBidiMode;
    FFontEmHeight: single;
    FFontName: string;
    FFontStyle: TFontStyles;
    FText: string;
    FSelStart,FSelEnd: integer;
    FVertAlign: TTextLayout;
    FEnteringUnicode: boolean;
    FUnicodeValue: cardinal;
    FUnicodeDigitCount: integer;
    FMouseSelecting: boolean;
    function GetBidiParagraphAlignment: TBidiTextAlignment;
    function GetCanPasteSelection: boolean;
    function GetHasSelection: boolean;
    function GetParagraphAlignment: TAlignment;
    procedure InvalidateParagraphLayout(AFrom, ATo: integer);
    procedure LayoutBrokenLinesChanged(ASender: TObject;
      AParagraphIndex: integer; ASubBrokenStart, ASubBrokenChangedCountBefore,
      ASubBrokenChangedCountAfter: integer; ASubBrokenTotalCountBefore,
      ASubBrokenTotalCountAfter: integer);
    procedure LayoutParagraphDeleted(ASender: TObject; AParagraphIndex: integer);
    procedure LayoutParagraphMergedWithNext(ASender: TObject;
      AParagraphIndex: integer);
    procedure LayoutParagraphSplit(ASender: TObject; AParagraphIndex: integer;
      ASubBrokenIndex, ACharIndex: integer);
    procedure OnMoveLightPos({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF;
      {%H-}AShift: TShiftState);
    procedure SetAliased(AValue: boolean);
    procedure SetAltitudePercent(AValue: single);
    procedure SetPenPhong(AValue: boolean);
    procedure SetFontBidiMode(AValue: TFontBidiMode);
    procedure SetFontEmHeight(AValue: single);
    procedure SetFontName(AValue: string);
    procedure SetFontStyle(AValue: TFontStyles);
    procedure SetBidiParagraphAlignment(AValue: TBidiTextAlignment);
    procedure SetLightPosition(AValue: TPointF);
    procedure SetParagraphAlignment(AValue: TAlignment);
    procedure SetText(AValue: string);
    procedure SetVertAlign(AValue: TTextLayout);
  protected
    FTextLayout: TBidiTextLayout;
    FFontRenderer: TBGRACustomFontRenderer;
    FGlobalMatrix: TAffineMatrix;
    FCurBrokenLineImageId: int64;
    FParagraphLayout: array of record
        brokenLines: array of record
              penImageId, penMaskId,
              outlineMaskId: int64;
            end;
      end;
    procedure SetGlobalMatrix(AMatrix: TAffineMatrix);
    function ShowArrows: boolean; override;
    function GetTextLayout: TBidiTextLayout;
    function GetFontRenderer: TBGRACustomFontRenderer;
    function UpdateFontRenderer: boolean;
    function GetTextRenderZoom: single;
    function GetUntransformedMatrix: TAffineMatrix; //matrix before render transform
    function IsTextMirrored(ABox: TAffineBox): boolean;
    procedure SetDefaultFont;
    function GetCornerPositition: single; override;
    procedure DeleteTextBefore(ACount: integer);
    procedure DeleteTextAfter(ACount: integer);
    procedure InsertText(ATextUTF8: string);
    procedure SelectWithMouse(X,Y: single; AExtend: boolean);
    function HasOutline: boolean;
    procedure InsertUnicodeValue;
    procedure FillChange(ASender: TObject; var ADiff: TCustomVectorialFillDiff); override;
    procedure InvalidateAll;
  public
    constructor Create(AContainer: TVectorOriginal); override;
    procedure QuickDefine(constref APoint1,APoint2: TPointF); override;
    procedure LoadFromStorage(AStorage: TBGRACustomOriginalStorage); override;
    procedure SaveToStorage(AStorage: TBGRACustomOriginalStorage); override;
    destructor Destroy; override;
    class function Fields: TVectorShapeFields; override;
    class function PreferPixelCentered: boolean; override;
    class function DefaultFontName: string;
    class function DefaultFontEmHeight: single;
    class function DefaultAltitudePercent: single;
    class function CreateEmpty: boolean; override;
    class function StorageClassName: RawByteString; override;
    class function Usermodes: TVectorShapeUsermodes; override;
    function AppendToSVG(AContent: TSVGContent; ADefs: TSVGDefine): TSVGElement; override;
    procedure ConfigureCustomEditor(AEditor: TBGRAOriginalEditor); override;
    procedure Render(ADest: TBGRABitmap; ARenderOffset: TPoint; AMatrix: TAffineMatrix; ADraft: boolean); overload; override;
    function GetRenderBounds({%H-}ADestRect: TRect; AMatrix: TAffineMatrix; AOptions: TRenderBoundsOptions = []): TRectF; override;
    function PointInShape(APoint: TPointF): boolean; overload; override;
    function PointInShape({%H-}APoint: TPointF; {%H-}ARadius: single): boolean; overload; override;
    function PointInPen(APoint: TPointF): boolean; overload; override;
    function GetIsSlow(const {%H-}AMatrix: TAffineMatrix): boolean; override;
    function GetGenericCost: integer; override;
    procedure MouseMove({%H-}Shift: TShiftState; {%H-}X, {%H-}Y: single; var {%H-}ACursor: TOriginalEditorCursor; var {%H-}AHandled: boolean); override;
    procedure MouseDown({%H-}RightButton: boolean; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: single; var {%H-}ACursor: TOriginalEditorCursor; var {%H-}AHandled: boolean); override;
    procedure MouseUp({%H-}RightButton: boolean; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: single; var {%H-}ACursor: TOriginalEditorCursor; var {%H-}AHandled: boolean); override;
    procedure KeyDown({%H-}Shift: TShiftState; {%H-}Key: TSpecialKey; var {%H-}AHandled: boolean); override;
    procedure KeyPress({%H-}UTF8Key: string; var {%H-}AHandled: boolean); override;
    procedure KeyUp({%H-}Shift: TShiftState; {%H-}Key: TSpecialKey; var {%H-}AHandled: boolean); override;
    procedure SetFontNameAndStyle(AFontName: string; AFontStyle: TFontStyles);
    function CopySelection: boolean;
    function CutSelection: boolean;
    function PasteSelection: boolean;
    function DeleteSelection: boolean;
    function GetAlignBounds(const {%H-}ALayoutRect: TRect; const AMatrix: TAffineMatrix): TRectF; override;
    procedure Transform(const AMatrix: TAffineMatrix); override;
    function AllowShearTransform: boolean; override;
    property HasSelection: boolean read GetHasSelection;
    property CanPasteSelection: boolean read GetCanPasteSelection;
    property Text: string read FText write SetText;
    property FontName: string read FFontName write SetFontName;
    property FontStyle: TFontStyles read FFontStyle write SetFontStyle;
    property FontEmHeight: single read FFontEmHeight write SetFontEmHeight;
    property FontBidiMode: TFontBidiMode read FFontBidiMode write SetFontBidiMode;
    property BidiParagraphAlignment: TBidiTextAlignment read GetBidiParagraphAlignment write SetBidiParagraphAlignment;
    property ParagraphAlignment: TAlignment read GetParagraphAlignment write SetParagraphAlignment;
    property VerticalAlignment: TTextLayout read FVertAlign write SetVertAlign;
    property PenPhong: boolean read FPenPhong write SetPenPhong;
    property LightPosition: TPointF read FLightPosition write SetLightPosition;
    property AltitudePercent: single read FAltitudePercent write SetAltitudePercent;
    property Aliased: boolean read FAliased write SetAliased;
  end;

function FontStyleToStr(AStyle: TFontStyles): string;
function StrToFontStyle(AText: string): TFontStyles;

function FontBidiModeToStr(AMode: TFontBidiMode): string;
function StrToFontBidiMode(AText: string): TFontBidiMode;

implementation

uses BGRATransform, BGRAText, BGRAVectorize, math,
  BGRAUTF8, BGRAUnicode, Graphics, Clipbrd, LCLType, LCLIntf,
  BGRAGradients, BGRACustomTextFX, LCResourceString, BGRAFillInfo,
  BGRAGrayscaleMask, BGRAPath, BGRALzpCommon, BGRADefaultBitmap;

function FontStyleToStr(AStyle: TFontStyles): string;
begin
  result := '';
  if fsBold in AStyle then result += 'b';
  if fsItalic in AStyle then result += 'i';
  if fsStrikeOut in AStyle then result += 's';
  if fsUnderline in AStyle then result += 'u';
end;

function StrToFontStyle(AText: string): TFontStyles;
var
  i: Integer;
begin
  result := [];
  for i := 1 to length(AText) do
    case AText[i] of
    'b': Include(result, fsBold);
    'i': Include(result, fsItalic);
    's': Include(result, fsStrikeOut);
    'u': Include(result, fsUnderline);
    end;
end;

function FontBidiModeToStr(AMode: TFontBidiMode): string;
begin
  case AMode of
  fbmLeftToRight: result := 'ltr';
  fbmRightToLeft: result := 'rtl';
  else {fbmAuto} result := 'auto';
  end;
end;

function StrToFontBidiMode(AText: string): TFontBidiMode;
begin
  if CompareText(AText,'ltr')=0 then result := fbmLeftToRight else
  if CompareText(AText,'rtl')=0 then result := fbmRightToLeft
  else result := fbmAuto;
end;

function GetPointBoundsF(APoints: ArrayOfTPointF): TRectF;
var
  i: Integer;
begin
  result := EmptyRectF;
  i := length(APoints);
  while i > 0 do
  begin
    dec(i);
    if not isEmptyPointF(APoints[i]) then
    begin
      result.TopLeft := APoints[i];
      result.BottomRight := APoints[i];
      break;
    end;
  end;
  while i > 0 do
  begin
    dec(i);
    result.Include(APoints[i]);
  end;
end;

{ TTextShapeTextDiff }

constructor TTextShapeTextDiff.Create(AStartShape: TVectorShape);
var
  tl: TBidiTextLayout;
  i: Integer;
begin
  with (AStartShape as TTextShape) do
  begin
    FTextBefore:= FText;
    FVertAlignBefore:= FVertAlign;
    tl := GetTextLayout;
    FSelStartBefore := FSelStart;
    FSelEndBefore:= FSelEnd;
    setlength(FParaAlignBefore, tl.ParagraphCount);
    for i := 0 to high(FParaAlignBefore) do
      FParaAlignBefore[i] := tl.ParagraphAlignment[i];
  end;
end;

procedure TTextShapeTextDiff.ComputeDiff(AEndShape: TVectorShape);
var
  tl: TBidiTextLayout;
  i: Integer;
begin
  with (AEndShape as TTextShape) do
  begin
    FTextAfter:= FText;
    FVertAlignAfter:= FVertAlign;
    FSelStartAfter := FSelStart;
    FSelEndAfter:= FSelEnd;
    tl := GetTextLayout;
    setlength(FParaAlignAfter, tl.ParagraphCount);
    for i := 0 to high(FParaAlignAfter) do
      FParaAlignAfter[i] := tl.ParagraphAlignment[i];
  end;
end;

procedure TTextShapeTextDiff.Apply(AStartShape: TVectorShape);
var
  tl: TBidiTextLayout;
  i: Integer;
begin
  with (AStartShape as TTextShape) do
  begin
    BeginUpdate;
    FreeAndNil(FTextLayout);
    FText := FTextAfter;
    FVertAlign := FVertAlignAfter;
    FSelStart := FSelStartAfter;
    FSelEnd := FSelEndAfter;
    tl := GetTextLayout;
    for i := 0 to min(length(FParaAlignAfter),tl.ParagraphCount)-1 do
      tl.ParagraphAlignment[i] := FParaAlignAfter[i];
    EndUpdate;
  end;
end;

procedure TTextShapeTextDiff.Unapply(AEndShape: TVectorShape);
var
  tl: TBidiTextLayout;
  i: Integer;
begin
  with (AEndShape as TTextShape) do
  begin
    BeginUpdate;
    FreeAndNil(FTextLayout);
    FText := FTextBefore;
    FVertAlign := FVertAlignBefore;
    FSelStart := FSelStartBefore;
    FSelEnd := FSelEndBefore;
    tl := GetTextLayout;
    for i := 0 to min(length(FParaAlignBefore),tl.ParagraphCount)-1 do
      tl.ParagraphAlignment[i] := FParaAlignBefore[i];
    EndUpdate;
  end;
end;

procedure TTextShapeTextDiff.Append(ADiff: TVectorShapeDiff);
var
  next: TTextShapeTextDiff;
  i: Integer;
begin
  next := ADiff as TTextShapeTextDiff;
  FTextAfter := next.FTextAfter;
  FVertAlignAfter := next.FVertAlignAfter;
  FSelStartAfter := next.FSelStartAfter;
  FSelEndAfter := next.FSelEndAfter;
  setlength(FParaAlignAfter, length(next.FParaAlignAfter));
  for i := 0 to high(FParaAlignAfter) do
    FParaAlignAfter[i] := next.FParaAlignAfter[i];
end;

function TTextShapeTextDiff.IsIdentity: boolean;
var
  i: Integer;
begin
  result := (FTextBefore = FTextAfter) and
    (FSelStartBefore = FSelStartAfter) and
    (FSelEndBefore = FSelEndAfter) and
    (FVertAlignBefore = FVertAlignAfter) and
    (length(FParaAlignBefore) = length(FParaAlignAfter));
  if result then
  begin
    for i := 0 to high(FParaAlignBefore) do
      if FParaAlignBefore[i] <> FParaAlignAfter[i] then
      begin
        result := false;
        break;
      end;
  end;
end;

{ TTextShapePhongDiff }

constructor TTextShapePhongDiff.Create(AStartShape: TVectorShape);
begin
  with (AStartShape as TTextShape) do
  begin
    FAltitudePercentBefore := FAltitudePercent;
    FPenPhongBefore := FPenPhong;
    FLightPositionBefore := FLightPosition;
  end;
end;

procedure TTextShapePhongDiff.ComputeDiff(AEndShape: TVectorShape);
begin
  with (AEndShape as TTextShape) do
  begin
    FAltitudePercentAfter := FAltitudePercent;
    FPenPhongAfter := FPenPhong;
    FLightPositionAfter := FLightPosition;
  end;
end;

procedure TTextShapePhongDiff.Apply(AStartShape: TVectorShape);
begin
  with (AStartShape as TTextShape) do
  begin
    BeginUpdate;
    FAltitudePercent := FAltitudePercentAfter;
    FPenPhong := FPenPhongAfter;
    FLightPosition := FLightPositionAfter;
    EndUpdate;
  end;
end;

procedure TTextShapePhongDiff.Unapply(AEndShape: TVectorShape);
begin
  with (AEndShape as TTextShape) do
  begin
    BeginUpdate;
    FAltitudePercent := FAltitudePercentBefore;
    FPenPhong := FPenPhongBefore;
    FLightPosition := FLightPositionBefore;
    EndUpdate;
  end;
end;

procedure TTextShapePhongDiff.Append(ADiff: TVectorShapeDiff);
var
  next: TTextShapePhongDiff;
begin
  next := ADiff as TTextShapePhongDiff;
  FAltitudePercentAfter:= next.FAltitudePercentAfter;
  FPenPhongAfter:= next.FPenPhongAfter;
  FLightPositionAfter:= next.FLightPositionAfter;
end;

function TTextShapePhongDiff.IsIdentity: boolean;
begin
  result := (FAltitudePercentBefore = FAltitudePercentAfter) and
    (FPenPhongBefore = FPenPhongAfter) and
    (FLightPositionBefore = FLightPositionAfter);
end;

{ TTextShapeFontDiff }

constructor TTextShapeFontDiff.Create(AStartShape: TVectorShape);
begin
  with (AStartShape as TTextShape) do
  begin
    FFontBidiModeBefore:= FFontBidiMode;
    FFontEmHeightBefore:= FFontEmHeight;
    FFontNameBefore:= FFontName;
    FFontStyleBefore:= FFontStyle;
    FAliasedBefore := FAliased;
  end;
end;

procedure TTextShapeFontDiff.ComputeDiff(AEndShape: TVectorShape);
begin
  with (AEndShape as TTextShape) do
  begin
    FFontBidiModeAfter:= FFontBidiMode;
    FFontEmHeightAfter:= FFontEmHeight;
    FFontNameAfter:= FFontName;
    FFontStyleAfter:= FFontStyle;
    FAliasedAfter := FAliased;
  end;
end;

procedure TTextShapeFontDiff.Apply(AStartShape: TVectorShape);
begin
  with (AStartShape as TTextShape) do
  begin
    BeginUpdate;
    FFontBidiMode := FFontBidiModeAfter;
    FFontEmHeight := FFontEmHeightAfter;
    FFontName := FFontNameAfter;
    FFontStyle := FFontStyleAfter;
    FAliased := FAliasedAfter;
    if Assigned(FTextLayout) then FTextLayout.InvalidateLayout;
    EndUpdate;
  end;
end;

procedure TTextShapeFontDiff.Unapply(AEndShape: TVectorShape);
begin
  with (AEndShape as TTextShape) do
  begin
    BeginUpdate;
    FFontBidiMode := FFontBidiModeBefore;
    FFontEmHeight := FFontEmHeightBefore;
    FFontName := FFontNameBefore;
    FFontStyle := FFontStyleBefore;
    FAliased := FAliasedBefore;
    if Assigned(FTextLayout) then FTextLayout.InvalidateLayout;
    EndUpdate;
  end;
end;

procedure TTextShapeFontDiff.Append(ADiff: TVectorShapeDiff);
var
  next: TTextShapeFontDiff;
begin
  next := ADiff as TTextShapeFontDiff;
  FFontBidiModeAfter := next.FFontBidiModeAfter;
  FFontEmHeightAfter := next.FFontEmHeightAfter;
  FFontNameAfter := next.FFontNameAfter;
  FFontStyleAfter := next.FFontStyleAfter;
  FAliasedAfter := next.FAliasedAfter;
end;

function TTextShapeFontDiff.IsIdentity: boolean;
begin
  result := (FFontBidiModeBefore = FFontBidiModeAfter) and
    (FFontEmHeightBefore = FFontEmHeightAfter) and
    (FFontNameBefore = FFontNameAfter) and
    (FFontStyleBefore = FFontStyleAfter) and
    (FAliasedBefore = FAliasedAfter);
end;

{ TTextShape }

procedure TTextShape.SetText(AValue: string);
begin
  if FText=AValue then Exit;
  BeginUpdate(TTextShapeTextDiff);
  FText:=AValue;
  FSelStart:=0;
  FSelEnd :=0;
  FreeAndNil(FTextLayout);
  EndUpdate;
end;

procedure TTextShape.SetFontBidiMode(AValue: TFontBidiMode);
begin
  if FFontBidiMode=AValue then Exit;
  BeginUpdate(TTextShapeFontDiff);
  FFontBidiMode:=AValue;
  InvalidateAll;
  EndUpdate;
end;

function TTextShape.GetBidiParagraphAlignment: TBidiTextAlignment;
var
  tl: TBidiTextLayout;
  paraIndex: Integer;
begin
  tl := GetTextLayout;
  paraIndex := tl.GetParagraphAt(FSelEnd);
  result := tl.ParagraphAlignment[paraIndex];
end;

function TTextShape.GetCanPasteSelection: boolean;
begin
  result := Clipboard.HasFormat(PredefinedClipboardFormat(pcfText));
end;

function TTextShape.GetHasSelection: boolean;
begin
  result := FSelEnd <> FSelStart;
end;

function TTextShape.GetParagraphAlignment: TAlignment;
var
  tl: TBidiTextLayout;
  paraIndex: Integer;
  rtl: Boolean;
begin
  tl := GetTextLayout;
  paraIndex := tl.GetParagraphAt(FSelEnd);
  rtl := tl.ParagraphRightToLeft[paraIndex];
  case tl.ParagraphAlignment[paraIndex] of
  btaCenter: result := taCenter;
  btaRightJustify: result := taRightJustify;
  btaNatural: if rtl then result := taRightJustify else result := taLeftJustify;
  btaOpposite: if rtl then result := taLeftJustify else result := taRightJustify;
  else {btaLeftJustify}
    result := taLeftJustify;
  end;
end;

procedure TTextShape.InvalidateParagraphLayout(AFrom, ATo: integer);
var
  i, j: Integer;
begin
  for i := AFrom to ATo do
    with FParagraphLayout[i] do
    begin
      for j := 0 to high(brokenLines) do
      begin
        brokenLines[j].penImageId := 0;
        brokenLines[j].penMaskId:= 0;
        brokenLines[j].outlineMaskId := 0;
      end;
    end;
end;

procedure TTextShape.LayoutBrokenLinesChanged(ASender: TObject;
  AParagraphIndex: integer; ASubBrokenStart, ASubBrokenChangedCountBefore,
  ASubBrokenChangedCountAfter: integer; ASubBrokenTotalCountBefore,
  ASubBrokenTotalCountAfter: integer);
var
  i: Integer;
begin
  if AParagraphIndex >= length(FParagraphLayout) then exit;
  if (ASubBrokenTotalCountBefore <> length(FParagraphLayout[AParagraphIndex].brokenLines)) then
  begin
    InvalidateParagraphLayout(AParagraphIndex,high(FParagraphLayout));
    FParagraphLayout[AParagraphIndex].brokenLines := nil;
    exit;
  end;
  with FParagraphLayout[AParagraphIndex] do
  begin
    if (ASubBrokenChangedCountBefore <> ASubBrokenChangedCountAfter) then
    begin
      for i := ASubBrokenStart to high(brokenLines) do
      begin
         brokenLines[i].penImageId := 0;
         brokenLines[i].penMaskId := 0;
         brokenLines[i].outlineMaskId := 0;
      end;
      setlength(brokenLines, ASubBrokenTotalCountAfter);
      if ASubBrokenChangedCountAfter > ASubBrokenChangedCountBefore then
      begin
        for i := ASubBrokenTotalCountBefore to high(brokenLines) do
        begin
          brokenLines[i].penImageId := 0;
          brokenLines[i].penMaskId := 0;
          brokenLines[i].outlineMaskId := 0;
        end;
      end;
      InvalidateParagraphLayout(AParagraphIndex+1,high(FParagraphLayout));
    end else
      for i := ASubBrokenStart to ASubBrokenStart+ASubBrokenChangedCountBefore-1 do
      begin
        brokenLines[i].penImageId := 0;
        brokenLines[i].penMaskId := 0;
        brokenLines[i].outlineMaskId := 0;
      end;
  end;
end;

procedure TTextShape.LayoutParagraphDeleted(ASender: TObject;
  AParagraphIndex: integer);
begin
  InvalidateParagraphLayout(AParagraphIndex, high(FParagraphLayout));
end;

procedure TTextShape.LayoutParagraphMergedWithNext(ASender: TObject;
  AParagraphIndex: integer);
begin
  InvalidateParagraphLayout(AParagraphIndex, high(FParagraphLayout));
end;

procedure TTextShape.LayoutParagraphSplit(ASender: TObject;
  AParagraphIndex: integer; ASubBrokenIndex, ACharIndex: integer);
begin
  InvalidateParagraphLayout(AParagraphIndex, high(FParagraphLayout));
end;

procedure TTextShape.OnMoveLightPos(ASender: TObject; APrevCoord,
  ANewCoord: TPointF; AShift: TShiftState);
begin
  LightPosition := ANewCoord;
end;

procedure TTextShape.SetAliased(AValue: boolean);
begin
  if FAliased=AValue then Exit;
  BeginUpdate(TTextShapeFontDiff);
  FAliased:=AValue;
  EndUpdate;
end;

procedure TTextShape.SetAltitudePercent(AValue: single);
begin
  if AValue < 0 then AValue := 0;
  if AValue > 100 then AValue := 100;
  if FAltitudePercent=AValue then Exit;
  BeginUpdate(TTextShapePhongDiff);
  FAltitudePercent:=AValue;
  EndUpdate;
end;

procedure TTextShape.SetPenPhong(AValue: boolean);
begin
  if FPenPhong=AValue then Exit;
  BeginUpdate(TTextShapePhongDiff);
  FPenPhong:=AValue;
  EndUpdate;
end;

procedure TTextShape.SetFontEmHeight(AValue: single);
begin
  if FFontEmHeight=AValue then Exit;
  BeginUpdate(TTextShapeFontDiff);
  FFontEmHeight:=AValue;
  if Assigned(FTextLayout) then FTextLayout.InvalidateLayout;
  EndUpdate;
end;

procedure TTextShape.SetFontName(AValue: string);
begin
  if FFontName=AValue then Exit;
  BeginUpdate(TTextShapeFontDiff);
  FFontName:=AValue;
  InvalidateAll;
  EndUpdate;
end;

procedure TTextShape.SetFontStyle(AValue: TFontStyles);
begin
  if FFontStyle=AValue then Exit;
  BeginUpdate(TTextShapeFontDiff);
  FFontStyle:=AValue;
  InvalidateAll;
  EndUpdate;
end;

procedure TTextShape.SetBidiParagraphAlignment(AValue: TBidiTextAlignment);
var
  tl: TBidiTextLayout;
  paraIndex, paraIndex2, i: Integer;
  needUpdate: boolean;
begin
  tl := GetTextLayout;
  if Usermode <> vsuEditText then
  begin
    if tl.ParagraphCount = 0 then exit;
    paraIndex := 0;
    paraIndex2:= tl.ParagraphCount-1;
  end else
  begin
    paraIndex := tl.GetParagraphAt(FSelStart);
    paraIndex2 := tl.GetParagraphAt(FSelEnd);
  end;
  needUpdate := false;
  for i := min(paraIndex,paraIndex2) to max(paraIndex,paraIndex2) do
  if tl.ParagraphAlignment[i] <> AValue then
  begin
    if not needUpdate then
    begin
      BeginUpdate(TTextShapeTextDiff);
      needUpdate := true;
    end;
    tl.ParagraphAlignment[i] := AValue;
  end;
  if needUpdate then EndUpdate;
end;

procedure TTextShape.SetLightPosition(AValue: TPointF);
begin
  if FLightPosition=AValue then Exit;
  BeginUpdate(TTextShapePhongDiff);
  FLightPosition:=AValue;
  EndUpdate;
end;

procedure TTextShape.SetParagraphAlignment(AValue: TAlignment);
var
  tl: TBidiTextLayout;
  paraIndex, paraIndex2, i: Integer;
  bidiAlign: TBidiTextAlignment;
  rtl, needUpdate: Boolean;
begin
  tl := GetTextLayout;
  if UserMode <> vsuEditText then
  begin
    if tl.ParagraphCount = 0 then exit;
    paraIndex := 0;
    paraIndex2:= tl.ParagraphCount-1;
  end else
  begin
    paraIndex := tl.GetParagraphAt(FSelStart);
    paraIndex2 := tl.GetParagraphAt(FSelEnd);
  end;
  needUpdate := false;
  for i := min(paraIndex,paraIndex2) to max(paraIndex,paraIndex2) do
  begin
    rtl := tl.ParagraphRightToLeft[i];
    case AValue of
    taCenter: bidiAlign:= btaCenter;
    taRightJustify: if rtl then bidiAlign := btaNatural else bidiAlign := btaOpposite;
    else {taLeftJustify}
      if rtl then bidiAlign := btaOpposite else bidiAlign := btaNatural;
    end;
    if tl.ParagraphAlignment[i] <> bidiAlign then
    begin
      if not needUpdate then
      begin
        BeginUpdate(TTextShapeTextDiff);
        needUpdate := true;
      end;
      tl.ParagraphAlignment[i] := bidiAlign;
    end;
  end;
  if needUpdate then EndUpdate;
end;

procedure TTextShape.SetVertAlign(AValue: TTextLayout);
begin
  if FVertAlign=AValue then Exit;
  BeginUpdate(TTextShapeTextDiff);
  FVertAlign:=AValue;
  EndUpdate;
end;

procedure TTextShape.SetGlobalMatrix(AMatrix: TAffineMatrix);
begin
  if AMatrix = FGlobalMatrix then exit;
  FGlobalMatrix := AMatrix;
end;

function TTextShape.AllowShearTransform: boolean;
begin
  Result:= true;
end;

function TTextShape.ShowArrows: boolean;
begin
  Result:= false;
end;

function TTextShape.GetTextLayout: TBidiTextLayout;
var
  box: TAffineBox;
begin
  if FTextLayout = nil then
  begin
    FTextLayout := TBidiTextLayout.Create(GetFontRenderer, FText);
    FTextLayout.OnParagraphDeleted:=@LayoutParagraphDeleted;
    FTextLayout.OnParagraphMergedWithNext:=@LayoutParagraphMergedWithNext;
    FTextLayout.OnParagraphSplit:=@LayoutParagraphSplit;
    FTextLayout.OnBrokenLinesChanged:=@LayoutBrokenLinesChanged;
  end
  else
    if UpdateFontRenderer then FTextLayout.InvalidateLayout;

  box := GetAffineBox(FGlobalMatrix,false);
  FTextLayout.FontBidiMode:= FontBidiMode;
  FTextLayout.TopLeft := PointF(0,0);
  FTextLayout.AvailableWidth:= box.Width;
  FTextLayout.AvailableHeight:= box.Height;
  FTextLayout.ParagraphSpacingBelow:= 0.5;
  result:= FTextLayout;
end;

function TTextShape.GetFontRenderer: TBGRACustomFontRenderer;
begin
  UpdateFontRenderer;
  result := FFontRenderer;
end;

function TTextShape.UpdateFontRenderer: boolean;
var
  newEmHeight: single;
begin
  if FFontRenderer = nil then
  begin
    FFontRenderer := TBGRAVectorizedFontRenderer.Create;
    TBGRAVectorizedFontRenderer(FFontRenderer).QuadraticCurves := true;
    TBGRAVectorizedFontRenderer(FFontRenderer).MinFontResolution := 300;
    TBGRAVectorizedFontRenderer(FFontRenderer).MaxFontResolution := 300;
  end;
  newEmHeight := FontEmHeight*GetTextRenderZoom;
  if (newEmHeight <> FFontRenderer.FontEmHeight) or
     (FFontRenderer.FontName <> FontName) or
     (FFontRenderer.FontStyle <> FontStyle) or
     (FFontRenderer.FontQuality <> fqFineAntialiasing) then
  begin
    FFontRenderer.FontEmHeightF := newEmHeight;
    FFontRenderer.FontName:= FontName;
    FFontRenderer.FontStyle:= FontStyle;
    FFontRenderer.FontQuality:= fqFineAntialiasing;
    exit(true);
  end
  else exit(false);
end;

function TTextShape.GetTextRenderZoom: single;
begin
  //font to be rendered at a sufficient size to avoid stretching
  result := max(VectLen(FGlobalMatrix[1,1],FGlobalMatrix[2,1]),
                VectLen(FGlobalMatrix[1,2],FGlobalMatrix[2,2]));
end;

function TTextShape.GetUntransformedMatrix: TAffineMatrix;
var
  ab: TAffineBox;
  u, v: TPointF;
  lenU, lenV: Single;
begin
  ab := GetAffineBox(AffineMatrixIdentity, false);
  u := ab.TopRight-ab.TopLeft;
  lenU := VectLen(u);
  if lenU<>0 then u *= (1/lenU);
  v := ab.BottomLeft-ab.TopLeft;
  lenV := VectLen(v);
  if lenV<>0 then v *= (1/lenV);
  result := AffineMatrix(u,v,ab.TopLeft);
end;

function TTextShape.IsTextMirrored(ABox: TAffineBox): boolean;
var
  u,v: TPointF;
begin
  u := ABox.TopRight-ABox.TopLeft;
  v := ABox.BottomLeft-ABox.TopLeft;
  result := u.x*v.y - u.y*v.x < 0;
end;

procedure TTextShape.SetDefaultFont;
begin
  FontName := DefaultFontName;
  FontEmHeight := DefaultFontEmHeight;
  FontBidiMode:= fbmAuto;
  FontStyle := [];
end;

function TTextShape.GetCornerPositition: single;
begin
  result := 1;
end;

procedure TTextShape.DeleteTextBefore(ACount: integer);
var
  delCount, selLeft: Integer;
begin
  if UserMode <> vsuEditText then exit;
  BeginUpdate(TTextShapeTextDiff);
  selLeft := Min(FSelStart,FSelEnd);
  if selLeft > 0 then
  begin
    delCount := GetTextLayout.DeleteTextBefore(selLeft, ACount);
    FText := GetTextLayout.TextUTF8;
    dec(selLeft,delCount);
  end;
  inc(selLeft, GetTextLayout.IncludeNonSpacingChars(selLeft, 0));
  FSelStart := selLeft;
  FSelEnd := selLeft;
  EndUpdate;
end;

procedure TTextShape.DeleteTextAfter(ACount: integer);
var
  selRight: Integer;
  tl: TBidiTextLayout;
begin
  if UserMode <> vsuEditText then exit;
  BeginUpdate(TTextShapeTextDiff);
  selRight := Max(FSelStart,FSelEnd);
  tl := GetTextLayout;
  if selRight+ACount <= tl.CharCount then
  begin
    tl.DeleteText(selRight, ACount);
    FText := tl.TextUTF8;
  end;
  inc(selRight, GetTextLayout.IncludeNonSpacingChars(selRight, 0));
  FSelStart := selRight;
  FSelEnd := selRight;
  EndUpdate;
end;

function TTextShape.DeleteSelection: boolean;
var
  selLeft: Integer;
begin
  if FSelStart <> FSelEnd then
  begin
    BeginUpdate(TTextShapeTextDiff);
    selLeft := Min(FSelStart,FSelEnd);
    GetTextLayout.DeleteText(selLeft, Abs(FSelEnd-FSelStart));
    FText := GetTextLayout.TextUTF8;
    inc(selLeft, GetTextLayout.IncludeNonSpacingChars(selLeft, 0));
    FSelStart := selLeft;
    FSelEnd := selLeft;
    EndUpdate;
    result := true;
  end else
    result := false;
end;

function TTextShape.GetAlignBounds(const ALayoutRect: TRect;
  const AMatrix: TAffineMatrix): TRectF;
var
  ab: TAffineBox;
begin
  ab := GetAffineBox(AMatrix, false);
  Result:= ab.RectBoundsF;
end;

procedure TTextShape.InsertText(ATextUTF8: string);
var
  insertCount: Integer;
begin
  if UserMode <> vsuEditText then exit;
  BeginUpdate(TTextShapeTextDiff);
  DeleteSelection;
  insertCount := GetTextLayout.InsertText(ATextUTF8, FSelStart);
  FText := GetTextLayout.TextUTF8;
  Inc(FSelStart, insertCount);
  inc(FSelStart, GetTextLayout.IncludeNonSpacingChars(FSelStart, 0));
  FSelEnd := FSelStart;
  EndUpdate;
end;

procedure TTextShape.SelectWithMouse(X, Y: single; AExtend: boolean);
var
  newPos: Integer;
  tl: TBidiTextLayout;
  zoom: Single;
  untransformed: TAffineMatrix;
begin
  tl := GetTextLayout;
  zoom := GetTextRenderZoom;
  untransformed := GetUntransformedMatrix;
  if not IsAffineMatrixInversible(untransformed) then exit;
  newPos := tl.GetCharIndexAt(AffineMatrixScale(zoom,zoom)*AffineMatrixInverse(untransformed)*PointF(X,Y));
  if newPos<>-1 then
  begin
    if (newPos <> FSelEnd) or (not AExtend and (FSelStart <> FSelEnd)) or (UserMode <> vsuEditText) then
    begin
      BeginEditingUpdate;
      FSelEnd:= newPos;
      if not AExtend or (UserMode <> vsuEditText) then FSelStart:= FSelEnd;
      UserMode := vsuEditText;
      EndEditingUpdate;
    end;
  end;
end;

function TTextShape.HasOutline: boolean;
begin
  result := not OutlineFill.IsFullyTransparent and (OutlineWidth > 0);
end;

procedure TTextShape.InsertUnicodeValue;
begin
  if FEnteringUnicode then
  begin
    if FUnicodeValue <= $10FFFF then
      InsertText(UnicodeCharToUTF8(FUnicodeValue));
    FEnteringUnicode:= false;
  end;
end;

procedure TTextShape.FillChange(ASender: TObject;
  var ADiff: TCustomVectorialFillDiff);
begin
  if ASender = PenFill then inc(FPenFillIteration);
  inherited FillChange(ASender, ADiff);
end;

procedure TTextShape.InvalidateAll;
begin
  if Assigned(FTextLayout) then FTextLayout.InvalidateLayout;
  FParagraphLayout := nil;
end;

constructor TTextShape.Create(AContainer: TVectorOriginal);
begin
  inherited Create(AContainer);
  SetDefaultFont;
  FVertAlign:= tlTop;
  FText := '';
  FSelStart := 0;
  FSelEnd := 0;
  FGlobalMatrix := AffineMatrixIdentity;
  FPenPhong:= false;
  FPenFillIteration := 0;
  FAltitudePercent:= DefaultAltitudePercent;
  FLightPosition := PointF(0,0);
  FAliased := false;
  FCurBrokenLineImageId := 0;
end;

procedure TTextShape.QuickDefine(constref APoint1, APoint2: TPointF);
var minSize: single;
  p2: TPointF;
begin
  minSize := GetFontRenderer.TextSize('Hg').cy/GetTextRenderZoom;
  p2 := APoint2;
  if abs(APoint1.x-p2.x) < minSize then
  begin
    if p2.x < APoint1.x then p2.x := APoint1.x - minSize else
      p2.x := APoint1.x + minSize;
  end;
  if abs(APoint1.y-p2.y) < minSize then
  begin
    if p2.y < APoint1.y then p2.y := APoint1.y - minSize else
      p2.y := APoint1.y + minSize;
  end;
  inherited QuickDefine(APoint1, p2);
end;

procedure TTextShape.LoadFromStorage(AStorage: TBGRACustomOriginalStorage);
var
  font, phongObj: TBGRACustomOriginalStorage;
  tl: TBidiTextLayout;
  paraAlignList: TStringList;
  i: Integer;
  alignment: TAlignment;
begin
  BeginUpdate;
  inherited LoadFromStorage(AStorage);
  Text := AStorage.RawString['text'];
  font := AStorage.OpenObject('font');
  if Assigned(font) then
  begin
    if font.HasAttribute('name') then
      FontName:= font.RawString['name']
    else
      FontName:= AStorage.RawString['name']; //compatibility
    if fontName = '' then fontName := DefaultFontName;

    if font.HasAttribute('em-height') then
      FontEmHeight:= font.FloatDef['em-height', DefaultFontEmHeight]
    else
      FontEmHeight:= AStorage.FloatDef['em-height', DefaultFontEmHeight]; //compatibility

    if Font.HasAttribute('bidi') then
      FontBidiMode:= StrToFontBidiMode(font.RawString['bidi'])
    else
      FontBidiMode:= StrToFontBidiMode(AStorage.RawString['bidi']); //compatibility

    if font.HasAttribute('style') then
      FontStyle:= StrToFontStyle(font.RawString['style'])
    else
      FontStyle:= StrToFontStyle(AStorage.RawString['style']); //compatibility
    font.Free;
  end else
    SetDefaultFont;

  Aliased := AStorage.Bool['aliased'];

  phongObj := AStorage.OpenObject('pen-phong');
  PenPhong := Assigned(phongObj);
  if PenPhong then
  begin
    LightPosition := phongObj.PointF['light-pos'];
    AltitudePercent:= phongObj.FloatDef['altitude-percent', DefaultAltitudePercent];
    phongObj.Free;
  end else
  begin
    LightPosition := PointF(0,0);
    AltitudePercent:= DefaultAltitudePercent;
  end;

  tl := GetTextLayout;
  paraAlignList := TStringList.Create;
  paraAlignList.DelimitedText:= AStorage.RawString['paragraph-align'];
  for i := 0 to min(paraAlignList.Count, tl.ParagraphCount)-1 do
  begin
    case paraAlignList[i] of
    'center': alignment := taCenter;
    'right': alignment := taRightJustify;
    else {'left'} alignment := taLeftJustify;
    end;
    tl.ParagraphAlignment[i] := AlignmentToBidiTextAlignment(alignment, tl.ParagraphRightToLeft[i]);
  end;
  paraAlignList.Free;
  EndUpdate;
end;

procedure TTextShape.SaveToStorage(AStorage: TBGRACustomOriginalStorage);
var
  font, phongObj: TBGRACustomOriginalStorage;
  tl: TBidiTextLayout;
  paraAlignList: TStringList;
  i: Integer;
begin
  inherited SaveToStorage(AStorage);
  AStorage.RawString['text'] := Text;
  font := AStorage.OpenObject('font');
  if font = nil then font := AStorage.CreateObject('font');
  font.RawString['name'] := FontName;
  font.Float['em-height'] := FontEmHeight;
  font.RawString['bidi'] := FontBidiModeToStr(FontBidiMode);
  font.RawString['style'] := FontStyleToStr(FontStyle);
  font.Free;
  AStorage.Bool['aliased'] := Aliased;

  if PenPhong then
  begin
    phongObj := AStorage.OpenObject('pen-phong');
    if phongObj=nil then phongObj := AStorage.CreateObject('pen-phong');
    phongObj.PointF['light-pos'] := LightPosition;
    phongObj.Float['altitude-percent'] := AltitudePercent;
    phongObj.Free;
  end else
    AStorage.RemoveObject('pen-phong');

  tl := GetTextLayout;
  paraAlignList := TStringList.Create;
  for i := 0 to tl.ParagraphCount-1 do
    case tl.ParagraphAlignment[i] of
    btaRightJustify: paraAlignList.Add('right');
    btaCenter: paraAlignList.Add('center');
    btaNatural: if tl.ParagraphRightToLeft[i] then paraAlignList.Add('right') else paraAlignList.Add('left');
    btaOpposite: if tl.ParagraphRightToLeft[i] then paraAlignList.Add('left') else paraAlignList.Add('right');
    else {btaLeftJustify}
      paraAlignList.Add('left');
    end;
  AStorage.RawString['paragraph-align'] := paraAlignList.DelimitedText;
  paraAlignList.Free;
end;

destructor TTextShape.Destroy;
begin
  FreeAndNil(FTextLayout);
  FreeAndNil(FFontRenderer);
  inherited Destroy;
end;

class function TTextShape.Fields: TVectorShapeFields;
begin
  Result:= [vsfPenFill,vsfOutlineFill,vsfOutlineWidth];
end;

class function TTextShape.PreferPixelCentered: boolean;
begin
  Result:= false;
end;

class function TTextShape.DefaultFontName: string;
begin
  result := {$IFDEF WINDOWS}'Arial'{$ELSE}{$IFDEF DARWIN}'Helvetica'{$ELSE}'Liberation Sans'{$ENDIF}{$ENDIF};
end;

class function TTextShape.DefaultFontEmHeight: single;
begin
  result := 20;
end;

class function TTextShape.DefaultAltitudePercent: single;
begin
  result := 30;
end;

class function TTextShape.CreateEmpty: boolean;
begin
  Result:= true;
end;

procedure TTextShape.ConfigureCustomEditor(AEditor: TBGRAOriginalEditor);
var
  caret: TBidiCaretPos;
  orientation: TPointF;
  m: TAffineMatrix;
  tl: TBidiTextLayout;
  pts: Array Of TPointF;
  i, idxLight: Integer;
  c: TBGRAPixel;
  zoom: Single;
begin
  inherited ConfigureCustomEditor(AEditor);
  AEditor.AddPolyline(GetAffineBox(AffineMatrixIdentity,true).AsPolygon, true, opsDashWithShadow);
  if AEditor.Focused and (Usermode = vsuEditText) then
  begin
    tl := GetTextLayout;
    caret:= tl.GetCaret(FSelEnd);
    zoom := GetTextRenderZoom;
    m := AffineMatrixTranslation(-0.5,-0.5)*GetUntransformedMatrix*AffineMatrixScale(1/zoom,1/zoom);
    if FSelStart<>FSelEnd then
    begin
      pts := tl.GetTextEnveloppe(FSelStart, FSelEnd, false, true, true);
      for i := 0 to high(pts) do
        pts[i] := m*pts[i];
      c:= clHighlight;
      c.alpha := 96;
      AEditor.AddPolyline(pts, true, opsDash, c);
    end;
    if (tl.AvailableHeight = EmptySingle) or (caret.Top.y < tl.AvailableHeight) then
    begin
      orientation := (caret.Bottom-caret.Top)*(1/10);
      orientation := PointF(-orientation.y,orientation.x);
      if (tl.AvailableHeight <> EmptySingle) and (caret.Bottom.y <> EmptySingle) and (caret.Bottom.y > tl.AvailableHeight) then caret.Bottom.y := tl.AvailableHeight;
      if (tl.AvailableHeight <> EmptySingle) and (caret.PreviousBottom.y <> EmptySingle) and (caret.PreviousBottom.y > tl.AvailableHeight) then caret.PreviousBottom.y := tl.AvailableHeight;
      if not isEmptyPointF(caret.PreviousTop) and (caret.PreviousRightToLeft<>caret.RightToLeft) then
      begin
        if caret.RightToLeft then orientation := -orientation;
        AEditor.AddPolyline([m*caret.Bottom,m*caret.Top,m*(caret.Top+orientation)],false, opsSolid);
      end else
        AEditor.AddPolyline([m*caret.Bottom,m*caret.Top],false, opsSolid);
    end;
  end;
  if PenPhong then
  begin
    idxLight := AEditor.AddPoint(FLightPosition, @OnMoveLightPos, true);
    if AEditor is TVectorOriginalEditor then
      TVectorOriginalEditor(AEditor).AddLabel(idxLight, rsLightPosition, taCenter, tlTop);
  end;
end;

procedure TTextShape.Render(ADest: TBGRABitmap; ARenderOffset: TPoint; AMatrix: TAffineMatrix;
  ADraft: boolean);

  function GetTextPhongHeight: integer;
  begin
    result := round(AltitudePercent/100 * FontEmHeight*0.15);
  end;

  function CreateShader(AOfsX,AOfsY: integer): TPhongShading;
  var
    lightPosF: TPointF;
    lightPosZ: Single;
  begin
    result := TPhongShading.Create;
    result.AmbientFactor := 0.6;
    result.NegativeDiffusionFactor := 0.15;
    lightPosF := FGlobalMatrix*LightPosition+PointF(AOfsX,AOfsY);
    lightPosZ := max(AltitudePercent, 1.2*GetTextPhongHeight);
    result.LightPosition3D := Point3D(lightPosF.x,lightPosF.y,lightPosZ);
  end;

var
  hasPen: boolean;
  zoom, outlineRenderWidth: Single;
  m: TAffineMatrix;
  tl: TBidiTextLayout;
  fr: TBGRACustomFontRenderer;
  pad, paraIndex, fileIdx: Integer;
  sourceRectF,transfRectF,sourceInvRect,destF: TRectF;
  transfRect, tmpRenderRect, brokenLineRectBounds: TRect;
  tmpTransf: TBGRABitmap;
  tmpBroken: TBGRAMemoryStreamBitmap;
  tmpTransfOutline, tmpBrokenMask: TGrayscaleMask;
  storeImage, useBrokenLinesRender, redrawPen, redrawOutline,
    outlineWidthChange, penPhongChange: Boolean;
  storage: TShapeRenderStorage;
  startBrokenIndex, endBrokenIndex, brokenIndex: LongInt;
  tempRenderNewList, tempRenderCurList: TStringList;
  phongObj, tempStorage: TBGRACustomOriginalStorage;
  brokenRenderOfs: TPoint;
  brokenLinePoints: Array of TPointF;
  brokenLineBoundsF: TRectF;

  procedure ComputeBrokenLinesPath(AStartBroken, AEndBroken: integer);
  var
    p: TBGRAPath;
  begin
    p := TBGRAPath.Create;
    tl.PathBrokenLines(p, AStartBroken, AEndBroken);
    brokenLinePoints := p.ToPoints(m);
    brokenLineBoundsF := GetPointBoundsF(brokenLinePoints);
    p.Free;
  end;

  procedure FillPen(ADestination: TCustomUniversalBitmap; AOffset: TPoint; ABrush: TUniversalBrush);
  var
    pts: ArrayOfTPointF;
    i: Integer;
  begin
    if not hasPen then exit;

    pts := PointsF(brokenLinePoints);
    for i := high(pts) downto 0 do
      pts[i].Offset(AOffset.x, AOffset.Y);

    ADestination.FillMode:= fmWinding;
    if (ADraft and PenPhong) or Aliased then
      ADestination.FillPoly(pts, ABrush, false)
    else
      ADestination.FillPolyAntialias(pts, ABrush, false);
  end;

  procedure RenderPen(ADestination: TBGRACustomBitmap; AOffset: TPoint);
  var
    rF: TRectF;
    r: TRect;
    textMask: TGrayscaleMask;
    textFx: TBGRACustomTextEffect;
    scan: TBGRACustomScanner;
    shader: TPhongShading;
    maskOfs: TPoint;
    b: TUniversalBrush;
  begin
    if not hasPen then exit;

    if PenPhong then
    begin
      rF := brokenLineBoundsF;
      rF.Offset(AOffset.X, AOffset.Y);
      rF := TRectF.Intersect(rF, rectF(0,0,ADestination.Width,ADestination.Height));
      r := Rect(floor(rF.Left), floor(rF.Top), ceil(rF.Right), ceil(rF.Bottom));
      r.Inflate(1, 1);
      maskOfs := Point(AOffset.x - r.Left, AOffset.y - r.Top);

      textMask := TGrayscaleMask.Create(r.Width, r.Height, 0);
      textMask.SolidBrush(b, ByteMaskWhite, dmDrawWithTransparency);
      FillPen(textMask, maskOfs, b);
      textFx := TBGRACustomTextEffect.Create(textMask, true, textMask.Width,textMask.Height, Point(0, 0));

      shader:= CreateShader(AOffset.X, AOffset.Y);
      if PenFill.FillType = vftSolid then
        textFx.DrawShaded(ADestination, r.Left, r.Top, shader, GetTextPhongHeight, PenFill.SolidColor)
      else
      begin
        scan := PenFill.CreateScanner(AffineMatrixTranslation(AOffset.X, AOffset.Y)*FGlobalMatrix, ADraft);
        textFx.DrawShaded(ADestination, r.Left, r.Top, shader, GetTextPhongHeight, scan);
        scan.Free;
      end;
      shader.Free;

      textFx.Free;
    end else
    begin
      if PenFill.FillType = vftSolid then
      begin
        ADestination.SolidBrush(b, PenFill.SolidColor, dmDrawWithTransparency);
        FillPen(ADestination, AOffset, b);
      end else
      begin
        scan := PenFill.CreateScanner(AffineMatrixTranslation(AOffset.X, AOffset.Y)*FGlobalMatrix, ADraft);
        ADestination.ScannerBrush(b, scan, dmDrawWithTransparency);
        FillPen(ADestination, AOffset, b);
        scan.Free;
      end;
    end;
  end;

  procedure RenderPenMask(ADestination: TGrayscaleMask; AOffset: TPoint);
  var
    b: TUniversalBrush;
  begin
    ADestination.SolidBrush(b, ByteMaskWhite, dmDrawWithTransparency);
    FillPen(ADestination, AOffset, b);
  end;

  procedure RenderFromMask(ADestination: TBGRABitmap; AX, AY: Integer; AMask: TGrayscaleMask;
    AFillOffset: TPoint; AFill: TVectorialFill);
  var
    scan: TBGRACustomScanner;
  begin
    if AFill.FillType = vftSolid then
    begin
      ADestination.FillMask(AX, AY, AMask, AFill.SolidColor, dmDrawWithTransparency);
    end else
    if AFill.FillType <> vftNone then
    begin
      scan := AFill.CreateScanner(AffineMatrixTranslation(AFillOffset.X, AFillOffset.Y)*FGlobalMatrix, ADraft);
      ADestination.FillMask(AX, AY, AMask, scan, dmDrawWithTransparency);
      scan.Free;
    end;
  end;

  procedure RenderFromMask(ADestination: TGrayscaleMask; AX, AY: Integer; AMask: TGrayscaleMask);
  begin
    ADestination.FillMask(AX, AY, AMask, ByteMaskWhite);
  end;

  procedure FillOutline(ADestination: TCustomUniversalBitmap; AOffset: TPoint; ABrush: TUniversalBrush);
  var
    pts: ArrayOfTPointF;
    i: Integer;
  begin
    if not HasOutline then exit;
    ADestination.Pen.JoinStyle:= pjsRound;
    ADestination.Pen.Style:= psSolid;
    ADestination.FillMode:= fmWinding;
    pts := ADestination.Pen.ComputePolygon(brokenLinePoints, outlineRenderWidth);
    for i := high(pts) downto 0 do
      pts[i].Offset(AOffset.x, AOffset.Y);
    if ADraft or Aliased then
      ADestination.FillPoly(pts, ABrush, false)
    else
      ADestination.FillPolyAntialias(pts, ABrush, false);
  end;

  procedure RenderOutlineMask(ADestination: TGrayscaleMask; AOffset: TPoint);
  var
    b: TUniversalBrush;
  begin
    ADestination.SolidBrush(b, ByteMaskWhite, dmDrawWithTransparency);
    FillOutline(ADestination, AOffset, b);
  end;

  procedure RenderOutline(ADestination: TBGRABitmap; AOffset: TPoint);
  var
    scan: TBGRACustomScanner;
    b: TUniversalBrush;
  begin
    if OutlineFill.FillType = vftSolid then
    begin
      ADestination.SolidBrush(b, OutlineFill.SolidColor, dmDrawWithTransparency);
      FillOutline(ADestination, AOffset, b);
    end else
    if OutlineFill.FillType <> vftNone then
    begin
      scan := OutlineFill.CreateScanner(AffineMatrixTranslation(AOffset.X, AOffset.Y)*FGlobalMatrix, ADraft);
      ADestination.ScannerBrush(b, scan, dmDrawWithTransparency);
      FillOutline(ADestination, AOffset, b);
      scan.Free;
    end;
  end;

  procedure RenderBrokenLines(ADestination: TBGRABitmap; AStartBroken, AEndBroken: integer; AOffset: TPoint);
  begin
    ComputeBrokenLinesPath(AStartBroken, AEndBroken);
    RenderOutline(ADestination, AOffset);
    RenderPen(ADestination, AOffset);
  end;

  procedure StoreRGBAImage(var AImageId: int64; AImage: TBGRAMemoryStreamBitmap; AImageOffset: TPoint);
  var
    renderObj: TBGRACustomOriginalStorage;
  begin
    if AImageId = 0 then
    begin
      inc(FCurBrokenLineImageId);
      AImageId := FCurBrokenLineImageId;
    end;
    renderObj := tempStorage.CreateObject(inttostr(AImageId));
    renderObj.PointF['size'] := PointF(AImage.Width, AImage.Height);
    renderObj.PointF['offset'] := PointF(AImageOffset) - PointF(ARenderOffset);
    renderObj.WriteFile('image.data', AImage.Stream, false, AImage.OwnStream);
    AImage.OwnStream := false;
    renderObj.Free;
    tempRenderNewList.Add(inttostr(AImageId));
  end;

  procedure EncodeSimpleRLE(AData: PByte; ASize: integer; AStream: TStream);
  var
    repCount, val: Byte;
    repeating: boolean;
  begin
    repeating := true;
    while ASize > 0 do
    begin
      val := AData^; inc(AData); repCount := 1; dec(ASize);
      while (ASize > 0) and (AData^ = val) and (repCount < 254) do
      begin
        inc(AData);
        dec(ASize);
        inc(repCount);
      end;
      if (repCount > 2) or (ASize = 0) then
      begin
        if not repeating then AStream.WriteByte(0);
        AStream.WriteByte(repCount);
        AStream.WriteByte(val);
        repeating := false;
      end else
      begin
        while (ASize > 1) and (repCount < 253) and ((AData^ <> (AData-1)^) or ((AData+1)^ <> (AData-1)^)) do
        begin
          inc(AData, 2);
          dec(ASize, 2);
          inc(repCount, 2);
        end;
        if (ASize = 1) and (repCount < 254) then
        begin
          inc(AData);
          dec(ASize);
          inc(repCount);
        end;
        if repeating then AStream.WriteByte(0);
        AStream.WriteByte(repCount);
        AStream.WriteBuffer((AData-repCount)^, repCount);
        repeating := true;
      end;
    end;
    AStream.WriteByte(255);
  end;

  procedure DecodeSimpleRLE(AData: PByte; ASize: integer; AStream: TStream);
  var
    repCount, val: Byte;
  begin
    while ASize > 0 do
    begin
      repCount := AStream.ReadByte;
      if repCount = 255 then break;
      if repCount > 0 then
      begin
        val := AStream.ReadByte;
        if repCount > ASize then repCount := ASize;
        fillchar(AData^, repCount, val);
        inc(AData, repCount);
        dec(ASize, repCount);
      end;

      repCount := AStream.ReadByte;
      if repCount = 255 then break;
      if repCount > 0 then
      begin
        if repCount > ASize then repCount := ASize;
        AStream.ReadBuffer(AData^, repCount);
        inc(AData, repCount);
        dec(ASize, repCount);
      end;
    end;
    while ASize > 0 do
    begin
      AData^ := 0;
      inc(AData);
      dec(ASize);
    end;
  end;

  procedure StoreMask(var AMaskId: int64; AMask: TGrayscaleMask; AMaskOffset: TPoint);
  var
    imgStream: TMemoryStream;
    renderObj: TBGRACustomOriginalStorage;
  begin
    imgStream := TMemoryStream.Create;
    EncodeSimpleRLE(AMask.Data, AMask.NbPixels, imgStream);
    if AMaskId = 0 then
    begin
      inc(FCurBrokenLineImageId);
      AMaskId := FCurBrokenLineImageId;
    end;
    renderObj := tempStorage.CreateObject(inttostr(AMaskId));
    renderObj.PointF['size'] := PointF(AMask.Width, AMask.Height);
    renderObj.PointF['offset'] := PointF(AMaskOffset) - PointF(ARenderOffset);
    renderObj.WriteFile('mask.data', imgStream, false, true);
    renderObj.Free;
    tempRenderNewList.Add(inttostr(AMaskId));
  end;

  function LoadStoredMask(AMaskId: integer; out AMask: TGrayscaleMask; out AOffset: TPoint): boolean;
  var
    imgStream: TStream;
    renderObj: TBGRACustomOriginalStorage;
    size: TPoint;
  begin
    AMask := nil;
    AOffset := Point(0,0);
    result := false;

    renderObj := tempStorage.OpenObject(inttostr(AMaskId));
    if Assigned(renderObj) then
    begin
      size := renderObj.PointF['size'].Round;
      AOffset := (renderObj.PointF['offset'] + PointF(ARenderOffset)).Round;
      imgStream := renderObj.GetFileStream('mask.data');
      if not Assigned(imgStream) or (imgStream.Size = 0) then
      begin
        renderObj.Free;
        exit;
      end;
      tempRenderNewList.Add(inttostr(AMaskId));
      AMask := TGrayscaleMask.Create;
      AMask.SetSize(size.x, size.y);
      imgStream.Position := 0;
      DecodeSimpleRLE(AMask.Data, AMask.NbPixels, imgStream);
      renderObj.Free;
      result := true;
    end;
  end;

  procedure RenderFromStoredMask(ADestination: TBGRABitmap; AMaskId: integer; AFill: TVectorialFill);
  var
    brokenRenderOfs: TPoint;
    tmpBrokenMask: TGrayscaleMask;
  begin
    if LoadStoredMask(AMaskId, tmpBrokenMask, brokenRenderOfs) then
    begin
      RenderFromMask(ADestination, brokenRenderOfs.X - transfRect.Left,
                     brokenRenderOfs.Y - transfRect.Top, tmpBrokenMask,
                     Point(- transfRect.Left, - transfRect.Top), AFill);
      tmpBrokenMask.Free;
    end;
  end;

  procedure RenderFromStoredMask(ADestination: TGrayscaleMask; AMaskId: integer);
  var
    brokenRenderOfs: TPoint;
    tmpBrokenMask: TGrayscaleMask;
  begin
    if LoadStoredMask(AMaskId, tmpBrokenMask, brokenRenderOfs) then
    begin
      RenderFromMask(ADestination, brokenRenderOfs.X - transfRect.Left,
                     brokenRenderOfs.Y - transfRect.Top, tmpBrokenMask);
      tmpBrokenMask.Free;
    end;
  end;

  procedure RenderFromStoredImage(ADestination: TBGRABitmap; AImageId: integer);
  var
    renderObj: TBGRACustomOriginalStorage;
    brokenRenderOfs, size: TPoint;
    imgStream: TStream;
    tmpBroken: TBGRAMemoryStreamBitmap;
  begin
    renderObj := tempStorage.OpenObject(inttostr(AImageId));
    if Assigned(renderObj) then
    begin
      size := renderObj.PointF['size'].Round;
      brokenRenderOfs := (renderObj.PointF['offset'] + PointF(ARenderOffset)).Round;
      imgStream := renderObj.GetFileStream('image.data');
      if (imgStream = nil) or (imgStream.Size = 0) then
      begin
        renderObj.Free;
        exit;
      end;
      tempRenderNewList.Add(inttostr(AImageId));
      tmpBroken := TBGRAMemoryStreamBitmap.Create(size.x, size.y,
                     imgStream as TMemoryStream, 0, false);
      ADestination.PutImage(brokenRenderOfs.X - transfRect.Left,
                            brokenRenderOfs.Y - transfRect.Top,
                            tmpBroken, dmDrawWithTransparency);
      tmpBroken.Free;
      renderObj.Free;
    end;
  end;

  procedure ApplyClipBox(ADest: TCustomUniversalBitmap);
  var
    maskBox: TAffineBox;
    wholeImage: TAffineBox;
  begin
    maskBox := AffineMatrixTranslation(-transfRect.Left,-transfRect.Top) * m  *
               TAffineBox.AffineBox(sourceRectF);
    wholeImage := TAffineBox.AffineBox(PointF(-1,-1), PointF(-1,ADest.Height+1),
                                       PointF(ADest.Width+1,-1));
    ADest.FillMode := fmWinding;
    ADest.ErasePolyAntialias( ConcatPointsF([wholeImage.AsPolygon, maskBox.AsPolygon], true),
                                  255, false);
  end;

begin
  RetrieveRenderStorage(AMatrix, transfRect, tmpTransf);
  if Assigned(tmpTransf) then
  begin
    ADest.PutImage(transfRect.Left + ARenderOffset.X, transfRect.Top + ARenderOffset.Y,
                   tmpTransf, dmDrawWithTransparency);
    tmpTransf.Free;
    exit;
  end;

  hasPen := not PenFill.IsFullyTransparent;
  if not hasPen and not HasOutline then exit;
  SetGlobalMatrix(AffineMatrixTranslation(ARenderOffset.X,ARenderOffset.Y)*AMatrix);
  zoom := GetTextRenderZoom;
  if zoom = 0 then exit;
  fr := GetFontRenderer;
  if fr.FontEmHeight = 0 then exit;
  pad := fr.FontEmHeight;

  m := FGlobalMatrix*                       //global transform
       GetUntransformedMatrix*              //transform according to shape rectangle
       AffineMatrixScale(1/zoom,1/zoom);    //shrink zoomed text if necessary

  tl := GetTextLayout;
  sourceRectF := RectF(-pad,0,tl.AvailableWidth+pad,min(tl.TotalTextHeight,tl.AvailableHeight));

  if CanHaveRenderStorage then
  begin
    storage := OpenRenderStorage(true);
    tempStorage := storage.temporary;
  end else
  begin
    storage := TShapeRenderStorage.None;
    tempStorage := TemporaryStorage;
  end;

  if Assigned(tempStorage) then
  begin
    tempRenderNewList := TStringList.Create;
    useBrokenLinesRender := not ADraft and (Usermode = vsuEditText);
    if not tempStorage.AffineMatrixEquals('last-matrix', AMatrix) or
       not tempStorage.PointFEquals('origin', Origin) or
       not tempStorage.PointFEquals('x-axis', XAxis) or
       not tempStorage.PointFEquals('y-axis', YAxis) or
       not tempStorage.FloatEquals('em-height', FontEmHeight) or
       not useBrokenLinesRender then
    begin
      //all temp files that are obsolete
      tempRenderCurList := TStringList.Create;
      tempStorage.EnumerateObjects(tempRenderCurList);
      for fileIdx := 0 to tempRenderCurList.Count-1 do
        tempStorage.RemoveObject(tempRenderCurList[fileIdx]);
      tempRenderCurList.Free;

      tempStorage.RemoveAttribute('last-matrix');
      tempStorage.RemoveAttribute('origin');
      tempStorage.RemoveAttribute('x-axis');
      tempStorage.RemoveAttribute('y-axis');
      tempStorage.RemoveAttribute('em-height');
      tempStorage.RemoveAttribute('outline-width');
      tempStorage.RemoveObject('pen-phong');
    end;
  end else
  begin
    tempRenderNewList := nil;
    useBrokenLinesRender := false;
  end;

  storeImage := CanHaveRenderStorage and not ADraft; // and not useBrokenLinesRender;

  if storeImage or useBrokenLinesRender then
    destF := rectF(0,0,ADest.Width,ADest.Height)
  else
    destF := RectF(ADest.ClipRect.Left,ADest.ClipRect.Top,ADest.ClipRect.Right,ADest.ClipRect.Bottom);

  transfRectF := (m*TAffineBox.AffineBox(sourceRectF)).RectBoundsF;
  transfRectF := TRectF.Intersect(transfRectF, destF);

  if not IsAffineMatrixInversible(m) then
  begin
    tempRenderNewList.Free;
    exit;
  end;
  sourceInvRect := (AffineMatrixInverse(m)*TAffineBox.AffineBox(transfRectF)).RectBoundsF;
  sourceInvRect.Top := floor(sourceInvRect.Top);
  sourceInvRect.Bottom := ceil(sourceInvRect.Bottom);
  sourceRectF := TRectF.Intersect(sourceRectF,sourceInvRect);
  if IsEmptyRectF(sourceRectF) then
  begin
    tempRenderNewList.Free;
    exit;
  end;
  sourceRectF.Left := floor(sourceRectF.Left);
  sourceRectF.Top := floor(sourceRectF.Top);
  sourceRectF.Right := floor(sourceRectF.Right);
  sourceRectF.Bottom := sourceRectF.Bottom;

  if tl.TotalTextHeight < tl.AvailableHeight then
  case VerticalAlignment of
  tlBottom: m *= AffineMatrixTranslation(0, tl.AvailableHeight-tl.TotalTextHeight);
  tlCenter: m *= AffineMatrixTranslation(0, (tl.AvailableHeight-tl.TotalTextHeight)/2);
  end;

  with transfRectF do
    transfRect := Rect(floor(Left),floor(Top),ceil(Right),ceil(Bottom));

  if PenPhong and Assigned(tempStorage) then
  begin
    phongObj := tempStorage.OpenObject('pen-phong');
    penPhongChange := not Assigned(phongObj) or
                      not phongObj.PointFEquals('light-pos', LightPosition) or
                      not phongObj.FloatEquals('altitude-percent', AltitudePercent) or
                      (phongObj.Int['fill-iteration'] <> FPenFillIteration);
    phongObj.Free;
  end else
    penPhongChange := false;

  if HasOutline then
  begin
    outlineRenderWidth := zoom*OutlineWidth;
    outlineWidthChange := Assigned(tempStorage) and
      (tempStorage.Float['outline-width'] <> OutlineWidth);
  end else
  begin
    outlineRenderWidth := 0;
    outlineWidthChange := false;
  end;

  if useBrokenLinesRender then
  begin
    brokenLineBoundsF := EmptyRectF;
    if HasOutline then
      tmpTransfOutline := TGrayscaleMask.Create(transfRect.Width, transfRect.Height)
      else tmpTransfOutline := nil;

    //render each broken line independently
    setlength(FParagraphLayout, tl.ParagraphCount);
    for paraIndex := 0 to tl.ParagraphCount-1 do
    with FParagraphLayout[paraIndex] do
    begin
      startBrokenIndex := tl.ParagraphStartBrokenLine[paraIndex];
      endBrokenIndex := tl.ParagraphEndBrokenLine[paraIndex];
      setlength(FParagraphLayout[paraIndex].brokenLines, endBrokenIndex - startBrokenIndex);
      for brokenIndex := startBrokenIndex to endBrokenIndex-1 do
      with brokenLines[brokenIndex - startBrokenIndex] do
      begin
        redrawPen := hasPen and
                       (
                        (PenPhong and (penPhongChange or (penImageId = 0) or
                                    not tempStorage.ObjectExists(inttostr(penImageId))) ) or
                        (not PenPhong and ((penMaskId = 0) or
                                    not tempStorage.ObjectExists(inttostr(penMaskId))) )
                       );
        redrawOutline := HasOutline and ((outlineMaskId = 0) or outlineWidthChange or
                        not tempStorage.ObjectExists(inttostr(outlineMaskId)) );

        if redrawPen or redrawOutline then
          ComputeBrokenLinesPath(brokenIndex, brokenIndex+1);

        if redrawOutline then
        begin
          with brokenLineBoundsF do
            brokenLineRectBounds := Rect(floor(Left - outlineRenderWidth/2),
              floor(Top - outlineRenderWidth/2), ceil(Right + outlineRenderWidth/2),
              ceil(Bottom + outlineRenderWidth/2));
          tmpRenderRect := TRect.Intersect(brokenLineRectBounds, transfRect);
          brokenRenderOfs := tmpRenderRect.TopLeft;
          tmpBrokenMask := TGrayscaleMask.Create(tmpRenderRect.Width, tmpRenderRect.Height);
          RenderOutlineMask(tmpBrokenMask, Point(-brokenRenderOfs.X, -brokenRenderOfs.Y));
          RenderFromMask(tmpTransfOutline, brokenRenderOfs.X - transfRect.Left,
                         brokenRenderOfs.Y - transfRect.Top, tmpBrokenMask);
          StoreMask(outlineMaskId, tmpBrokenMask, brokenRenderOfs);
          tmpBrokenMask.Free;
        end else
        if HasOutline then
          RenderFromStoredMask(tmpTransfOutline, outlineMaskId);


        if redrawPen then
        begin
          if PenPhong then
          begin
            with brokenLineBoundsF do
              brokenLineRectBounds := Rect(floor(Left), floor(Top), ceil(Right), ceil(Bottom));
            tmpRenderRect := TRect.Intersect(brokenLineRectBounds, transfRect);
            brokenRenderOfs := tmpRenderRect.TopLeft;
            tmpBroken := TBGRAMemoryStreamBitmap.Create(tmpRenderRect.Width, tmpRenderRect.Height);
            RenderPen(tmpBroken, Point(- brokenRenderOfs.X,
                                       - brokenRenderOfs.Y));
            StoreRGBAImage(penImageId, tmpBroken, brokenRenderOfs);
            tmpBroken.Free;
          end else
          begin
            with brokenLineBoundsF do
              brokenLineRectBounds := Rect(floor(Left), floor(Top), ceil(Right), ceil(Bottom));
            tmpRenderRect := TRect.Intersect(brokenLineRectBounds, transfRect);
            brokenRenderOfs := tmpRenderRect.TopLeft;
            tmpBrokenMask := TGrayscaleMask.Create(tmpRenderRect.Width, tmpRenderRect.Height);
            RenderPenMask(tmpBrokenMask, Point(-brokenRenderOfs.X, -brokenRenderOfs.Y));
            StoreMask(penMaskId, tmpBrokenMask, brokenRenderOfs);
            tmpBrokenMask.Free;
          end;
        end;

      end;
    end;

    tmpTransf := TBGRABitmap.Create(transfRect.Width, transfRect.Height);
    if Assigned(tmpTransfOutline) then
    begin
      ApplyClipBox(tmpTransfOutline);
      RenderFromMask(tmpTransf, 0, 0, tmpTransfOutline,
                     Point(-transfRect.Left, -transfRect.Top), OutlineFill);
      tmpTransfOutline.Free;
    end;
    if hasPen then
      for paraIndex := 0 to tl.ParagraphCount-1 do
      with FParagraphLayout[paraIndex] do
      begin
        for brokenIndex := 0 to high(brokenLines) do
        with brokenLines[brokenIndex] do
        begin
          if PenPhong then RenderFromStoredImage(tmpTransf, penImageId)
          else RenderFromStoredMask(tmpTransf, penMaskId, PenFill);
        end;
      end;
  end else
  begin
    tmpTransf := TBGRABitmap.Create(transfRect.Width, transfRect.Height);
    RenderBrokenLines(tmpTransf, 0, tl.BrokenLineCount, Point(-transfRect.Left, -transfRect.Top));

    //make list of temp files to keep
    if Assigned(tempStorage) and Assigned(tempRenderNewList) then
      for paraIndex := 0 to high(FParagraphLayout) do
      with FParagraphLayout[paraIndex] do
        for brokenIndex := 0 to high(brokenLines) do
        with brokenLines[brokenIndex] do
        begin
          if hasPen then
          begin
            if not PenPhong and (penImageId <> 0) then
              tempRenderNewList.Add(inttostr(penImageId));
            if PenPhong and not penPhongChange and (penMaskId <> 0) then
              tempRenderNewList.Add(inttostr(penMaskId));
          end;
          if HasOutline and (outlineMaskId <> 0) and not outlineWidthChange then
            tempRenderNewList.Add(inttostr(outlineMaskId));
        end;
  end;

  if Assigned(tempStorage) then
  begin
    //remove temp files that are obsolete
    tempRenderCurList := TStringList.Create;
    tempStorage.EnumerateObjects(tempRenderCurList);
    for fileIdx := 0 to tempRenderCurList.Count-1 do
      if tempRenderNewList.IndexOf(tempRenderCurList[fileIdx]) = -1 then
        tempStorage.RemoveObject(tempRenderCurList[fileIdx]);
    tempRenderCurList.Free;

    tempStorage.AffineMatrix['last-matrix'] := AMatrix;
    tempStorage.PointF['origin'] := Origin;
    tempStorage.PointF['x-axis'] := XAxis;
    tempStorage.PointF['y-axis'] := YAxis;
    tempStorage.Float['em-height'] := FontEmHeight;
    if HasOutline then
      tempStorage.Float['outline-width'] := OutlineWidth
      else tempStorage.RemoveAttribute('outline-width');
    if PenPhong then
    begin
      phongObj := tempStorage.CreateObject('pen-phong');
      phongObj.PointF['light-pos'] := LightPosition;
      phongObj.Float['altitude-percent'] := AltitudePercent;
      phongObj.Int['fill-iteration'] := FPenFillIteration;
      phongObj.Free;
    end else
      tempStorage.RemoveObject('pen-phong');
  end;
  storage.Close;
  tempRenderNewList.Free;

  ApplyClipBox(tmpTransf);
  ADest.PutImage(transfRect.Left, transfRect.Top, tmpTransf, dmDrawWithTransparency);

  transfRect.Offset(-ARenderOffset.X,-ARenderOffset.Y);
  if storeImage then UpdateRenderStorage(transfRect, tmpTransf)
  else UpdateRenderStorage(transfRect);
  tmpTransf.Free;
end;

function TTextShape.GetRenderBounds(ADestRect: TRect; AMatrix: TAffineMatrix;
  AOptions: TRenderBoundsOptions): TRectF;
var
  ab: TAffineBox;
  u: TPointF;
  lenU, margin: Single;
begin
  if (GetPenVisible(rboAssumePenFill in AOptions) or HasOutline) and
    (Text <> '') then
  begin
    ab := GetAffineBox(AMatrix, false);
    //add margin for text that would be out of bound (for example italic j)
    u := ab.TopRight-ab.TopLeft;
    lenU := VectLen(u);
    if lenU<>0 then u *= (1/lenU);
    margin := FontEmHeight;
    u *= margin;
    ab.TopLeft -= u;
    ab.TopRight += u;
    ab.BottomLeft -= u;
    result := ab.RectBoundsF;
  end
  else
    result:= EmptyRectF;
end;

function TTextShape.PointInShape(APoint: TPointF): boolean;
begin
  result := GetAffineBox(AffineMatrixIdentity,true).Contains(APoint);
end;

function TTextShape.PointInShape(APoint: TPointF; ARadius: single): boolean;
begin
  result := false;
end;

function TTextShape.PointInPen(APoint: TPointF): boolean;
var
  tl: TBidiTextLayout;
  pt: TPointF;
  i: Integer;
  untransformed: TAffineMatrix;
begin
  if not GetAffineBox(AffineMatrixIdentity,true).Contains(APoint) then
    exit(false);
  SetGlobalMatrix(AffineMatrixIdentity);
  tl := GetTextLayout;
  untransformed := GetUntransformedMatrix;
  if not IsAffineMatrixInversible(untransformed) then exit(false);
  pt := AffineMatrixInverse(untransformed)*APoint;
  for i := 0 to tl.PartCount-1 do
    if tl.PartAffineBox[i].Contains(pt) then exit(true);
  result := false;
end;

function TTextShape.GetIsSlow(const AMatrix: TAffineMatrix): boolean;
begin
  Result:= true;
end;

function TTextShape.GetGenericCost: integer;
begin
  Result:= 10;
end;

procedure TTextShape.MouseMove(Shift: TShiftState; X, Y: single;
  var ACursor: TOriginalEditorCursor; var AHandled: boolean);
begin
  if FMouseSelecting then
  begin
    SelectWithMouse(X,Y, true);
    ACursor := oecText;
    AHandled:= true;
  end else
  begin
    inherited MouseMove(Shift, X, Y, ACursor, AHandled);
    if (ACursor = oecDefault) and PointInShape(PointF(X,Y)) then ACursor := oecText;
  end;
end;

procedure TTextShape.MouseDown(RightButton: boolean; Shift: TShiftState; X,
  Y: single; var ACursor: TOriginalEditorCursor; var AHandled: boolean);
begin
  inherited MouseDown(RightButton, Shift, X, Y, ACursor, AHandled);
  if not AHandled and not RightButton and PointInShape(PointF(X,Y)) then
  begin
    FMouseSelecting:= true;
    SelectWithMouse(X,Y, ssShift in Shift);
    AHandled:= true;
  end;
  if (ACursor = oecDefault) and PointInShape(PointF(X,Y)) then ACursor := oecText;
end;

procedure TTextShape.MouseUp(RightButton: boolean; Shift: TShiftState; X,
  Y: single; var ACursor: TOriginalEditorCursor; var AHandled: boolean);
begin
  if FMouseSelecting and not RightButton then
  begin
    FMouseSelecting:= false;
    ACursor := oecText;
    AHandled:= true;
  end else
  begin
    inherited MouseUp(RightButton, Shift, X, Y, ACursor, AHandled);
    if (ACursor = oecDefault) and PointInShape(PointF(X,Y)) then ACursor := oecText;
  end;
end;

procedure TTextShape.KeyDown(Shift: TShiftState; Key: TSpecialKey;
  var AHandled: boolean);
var
  idxPara, newPos: Integer;
  tl: TBidiTextLayout;
begin
  if (FTextLayout = nil) or (Usermode <> vsuEditText) then exit;

  if Key = skDelete then
  begin
    if FSelStart <> FSelEnd then DeleteSelection
    else DeleteTextAfter(1);
    AHandled:= true;
  end else
  if Key in [skLeft,skRight] then
  begin
    tl := GetTextLayout;
    if (Key = skLeft) xor tl.ParagraphRightToLeft[tl.GetParagraphAt(FSelEnd)] then
    begin
      BeginEditingUpdate;
      if FSelEnd > 0 then
        Dec(FSelEnd, tl.IncludeNonSpacingCharsBefore(FSelEnd,1) );
      if not (ssShift in Shift) then FSelStart := FSelEnd;
      EndEditingUpdate;
    end else
    begin
      BeginEditingUpdate;
      if FSelEnd < tl.CharCount then
        Inc(FSelEnd, tl.IncludeNonSpacingChars(FSelEnd,1) );
      if not (ssShift in Shift) then FSelStart := FSelEnd;
      EndEditingUpdate;
    end;
    AHandled := true;
  end else
  if Key in [skUp,skDown] then
  begin
    tl := GetTextLayout;
    if Key = skUp then
      newPos := tl.FindTextAbove(FSelEnd)
    else
      newPos := tl.FindTextBelow(FSelEnd);
    if (newPos <> -1) or (not (ssShift in Shift) and (FSelStart <> FSelEnd)) then
    begin
      BeginEditingUpdate;
      FSelEnd := newPos;
      if not (ssShift in Shift) then FSelStart := FSelEnd;
      EndEditingUpdate;
    end;
    AHandled:= true;
  end else
  if Key = skHome then
  begin
    tl := GetTextLayout;
    BeginEditingUpdate;
    if ssCtrl in Shift then
      FSelEnd := 0
    else
    begin
      idxPara := tl.GetParagraphAt(FSelEnd);
      FSelEnd := tl.ParagraphStartIndex[idxPara];
    end;
    if not (ssShift in Shift) then FSelStart := FSelEnd;
    EndEditingUpdate;
    AHandled := true;
  end else
  if Key = skEnd then
  begin
    tl := GetTextLayout;
    BeginEditingUpdate;
    if ssCtrl in Shift then
      FSelEnd := tl.CharCount
    else
    begin
      idxPara := tl.GetParagraphAt(FSelEnd);
      FSelEnd := tl.ParagraphEndIndexBeforeParagraphSeparator[idxPara];
    end;
    if not (ssShift in Shift) then FSelStart := FSelEnd;
    EndEditingUpdate;
    AHandled := true;
  end else
  if (Key = skReturn) and ([ssCtrl,ssShift] <= Shift) and FEnteringUnicode then
  begin
    InsertUnicodeValue;
    AHandled:= true;
  end else
  if Key = skReturn then
  begin
    if ssShift in Shift then
      InsertText(UnicodeCharToUTF8(UNICODE_LINE_SEPARATOR))
    else
      InsertText(#10);
    AHandled := true;
  end else
  if Key = skTab then
  begin
    InsertText(#9);
    AHandled := true;
  end else
  if (Key = skU) and ([ssCtrl,ssShift] <= Shift) then
  begin
    if FEnteringUnicode then InsertUnicodeValue;
    FEnteringUnicode:= true;
    FUnicodeValue:= 0;
    FUnicodeDigitCount:= 0;
    AHandled := true;
  end else
  if (Key in[sk0..sk9,skNum0..skNum9,skA..skF]) and ([ssCtrl,ssShift] <= Shift) and FEnteringUnicode then
  begin
    if FUnicodeDigitCount >= 8 then FEnteringUnicode:= false else
    begin
      FUnicodeValue := (FUnicodeValue shl 4);
      case Key of
      sk0..sk9: inc(FUnicodeValue, ord(Key)-ord(sk0));
      skNum0..skNum9: inc(FUnicodeValue, ord(Key)-ord(sk0));
      skA..skF: inc(FUnicodeValue, ord(Key)-ord(skA)+10);
      end;
    end;
  end else
  if (Key = skC) and (ssCtrl in Shift) then
  begin
    if CopySelection then AHandled:= true;
  end else
  if (Key = skX) and (ssCtrl in Shift) then
  begin
    if CutSelection then AHandled:= true;
  end else
  if (Key = skV) and (ssCtrl in Shift) then
  begin
    if PasteSelection then AHandled := true;
  end else
  if (Key = skA) and (ssCtrl in Shift) then
  begin
    BeginEditingUpdate;
    FSelStart:= 0;
    FSelEnd:= GetTextLayout.CharCount;
    EndEditingUpdate;
    AHandled := true;
  end;
end;

procedure TTextShape.KeyPress(UTF8Key: string; var AHandled: boolean);
begin
  if (Usermode = vsuEditText) and (UTF8Key = #8) then
  begin
    if FSelEnd <> FSelStart then DeleteSelection
    else DeleteTextBefore(1);
    AHandled := true;
  end else
  if UTF8Key >= ' ' then
  begin
    if Usermode <> vsuEditText then
    begin
      if Text = '' then
      begin
        Usermode := vsuEditText;
        InsertText(UTF8Key);
      end;
    end else
      InsertText(UTF8Key);
    AHandled := true;
  end;
end;

procedure TTextShape.KeyUp(Shift: TShiftState; Key: TSpecialKey;
  var AHandled: boolean);
begin
  if (Key in[skCtrl,skShift]) and FEnteringUnicode then
  begin
    InsertUnicodeValue;
    AHandled := true;
  end;
end;

procedure TTextShape.SetFontNameAndStyle(AFontName: string;
  AFontStyle: TFontStyles);
begin
  if (AFontName <> FFontName) or (AFontStyle <> FFontStyle) then
  begin
    BeginUpdate(TTextShapeFontDiff);
    FFontName := AFontName;
    FFontStyle:= AFontStyle;
    EndUpdate;
  end;
end;

function TTextShape.CopySelection: boolean;
var
  stream: TStringStream;
begin
  if HasSelection then
  begin
    stream := nil;
    try
      Clipboard.Clear;
      stream := TStringStream.Create(GetTextLayout.CopyText(min(FSelStart,FSelEnd),abs(FSelEnd-FSelStart)));
      Clipboard.SetFormat(PredefinedClipboardFormat(pcfText), stream);
    finally
      stream.Free;
    end;
    result := true;
  end
  else result := false;
end;

function TTextShape.CutSelection: boolean;
begin
  result := CopySelection;
  if result then DeleteSelection;
end;

function TTextShape.PasteSelection: boolean;
var
  txt: String;
begin
  if CanPasteSelection then
  begin
    txt := Clipboard.AsText;
    txt := StringReplace(txt, #13#10, #10, [rfReplaceAll]);
    txt := StringReplace(txt, #10#13, #10, [rfReplaceAll]);
    txt := StringReplace(txt, #13, #10, [rfReplaceAll]);
    txt := StringReplace(txt, UnicodeCharToUTF8(UNICODE_PARAGRAPH_SEPARATOR), #10, [rfReplaceAll]);
    txt := StringReplace(txt, UnicodeCharToUTF8(UNICODE_NEXT_LINE), #10, [rfReplaceAll]);
    InsertText(txt);
    result := true;
  end else
    result := false;
end;

procedure TTextShape.Transform(const AMatrix: TAffineMatrix);
var
  zoom: Single;
begin
  BeginUpdate;
  AddDiffHandler(TTextShapeFontDiff);
  AddDiffHandler(TTextShapePhongDiff);
  zoom := (VectLen(AMatrix[1,1],AMatrix[2,1])+VectLen(AMatrix[1,2],AMatrix[2,2]))/2;
  FontEmHeight:= zoom*FontEmHeight;
  LightPosition := AMatrix*LightPosition;
  inherited Transform(AMatrix);
  EndUpdate;
end;

class function TTextShape.StorageClassName: RawByteString;
begin
  result := 'text';
end;

class function TTextShape.Usermodes: TVectorShapeUsermodes;
begin
  Result:=inherited Usermodes + [vsuEditText];
end;

function TTextShape.AppendToSVG(AContent: TSVGContent; ADefs: TSVGDefine): TSVGElement;
var
  topLeft, u, v: TPointF;
  w, h, zoom: Single;
  t: TSVGText;
  tl: TBidiTextLayout;
  i: Integer;
  span: TSVGTSpan;
  fm: TFontPixelMetric;
  rF: TRectF;
  penFillId, outlineFillId: String;
begin
  topLeft := Origin - (XAxis - Origin) - (YAxis - Origin);
  w := Width*2; h := Height*2;
  result := AContent.AppendText(topLeft, '');
  if (XAxis.y <> 0) or (YAxis.x <> 0) then
  begin
    u := XAxis - Origin;
    if w > 0 then u *= (2/w);
    v := YAxis - Origin;
    if h > 0 then v *= (2/h);
    result.matrix[cuPixel] := AffineMatrixTranslation(topLeft.X, topLeft.Y) *
                              AffineMatrix(u, v, PointF(0, 0)) *
                              AffineMatrixTranslation(-topLeft.X, -topLeft.Y);
  end;
  if PenVisible then
  begin
    if IsAffineMatrixInversible(result.Matrix[cuPixel]) then
      penFillId := AppendVectorialFillToSVGDefs(PenFill,
        AffineMatrixInverse(result.Matrix[cuPixel]), ADefs, 'fill')
      else penFillId := '';
    if penFillId <> '' then
      result.fill := 'url(#' + penFillId + ')'
      else result.fillColor := PenColor;
  end
    else result.fillNone;
  if OutlineVisible then
  begin
    if IsAffineMatrixInversible(result.Matrix[cuPixel]) then
      outlineFillId := AppendVectorialFillToSVGDefs(OutlineFill,
        AffineMatrixInverse(result.Matrix[cuPixel]), ADefs, 'stroke')
      else outlineFillId:= '';
    if outlineFillId <> '' then
      result.stroke := 'url(#' + outlineFillId + ')'
      else result.strokeColor := OutlineFill.AverageColor;
    result.strokeWidth := FloatWithCSSUnit(OutlineWidth, cuCustom);
    result.strokeLineJoinLCL:= pjsRound;
    result.paintOrder:= spoStrokeFillMarkers;
  end else
    result.strokeNone;
  t := TSVGText(result);
  t.fontStyleLCL:= FontStyle;
  t.fontSize := FloatWithCSSUnit(FontEmHeight, cuPixel);
  t.fontFamily:= FontName;
  SetGlobalMatrix(AffineMatrixIdentity);
  zoom := GetTextRenderZoom;
  tl := GetTextLayout;
  fm := tl.FontRenderer.GetFontPixelMetric;
  for i := 0 to tl.PartCount-1 do
  begin
    rF := tl.PartRectF[i];
    if rF.IsEmpty then continue;
    rF.OffseT(topLeft.x, topLeft.y);
    span := t.Content.AppendTextSpan(tl.GetTextPart(i, true));
    if tl.PartRightToLeft[i] then
      span.textDirection:= stdRtl
      else span.textDirection:= stdLtr;
    with rF do
    begin
      span.x := [FloatWithCSSUnit(Left/zoom, cuCustom)];
      span.y := [FloatWithCSSUnit((Top + fm.Baseline)/zoom, cuCustom)];
      span.textLength := FloatWithCSSUnit(Width/zoom, cuCustom);
    end;
  end;

end;

initialization

  RegisterVectorShape(TTextShape);

end.

