unit LCVectorTextShapes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCVectorRectShapes, BGRATextBidi, BGRABitmapTypes, LCVectorOriginal, BGRAGraphics,
  BGRABitmap, BGRALayerOriginal;

type

  { TTextShape }

  TTextShape = class(TCustomRectShape)
  private
    FFontBidiMode: TFontBidiMode;
    FFontEmHeight: single;
    FFontName: string;
    FFontStyle: TFontStyles;
    FHorizAlign: TBidiTextAlignment;
    FText: string;
    FSelStart,FSelEnd: integer;
    FVertAlign: TTextLayout;
    procedure SetFontBidiMode(AValue: TFontBidiMode);
    procedure SetFontEmHeight(AValue: single);
    procedure SetFontName(AValue: string);
    procedure SetFontStyle(AValue: TFontStyles);
    procedure SetHorizAlign(AValue: TBidiTextAlignment);
    procedure SetText(AValue: string);
    procedure SetVertAlign(AValue: TTextLayout);
  protected
    FTextLayout: TBidiTextLayout;
    FFontRenderer: TBGRACustomFontRenderer;
    function PenVisible(AAssumePenFill: boolean = false): boolean;
    function AllowShearTransform: boolean; override;
    function ShowArrows: boolean; override;
    function GetTextLayout(AMatrix: TAffineMatrix): TBidiTextLayout;
    function GetTextLayoutIgnoreMatrix: TBidiTextLayout;
    function GetFontRenderer(AMatrix: TAffineMatrix): TBGRACustomFontRenderer;
    function GetTextRenderZoom(AMatrix: TAffineMatrix): single;
    function GetUntransformedMatrix: TAffineMatrix; //matrix before render transform
    function IsTextMirrored(ABox: TAffineBox): boolean;
    procedure SetDefaultFont;
    function GetCornerPositition: single; override;
    procedure DeleteTextBefore(ACount: integer);
    procedure DeleteTextAfter(ACount: integer);
    procedure DeleteSelectedText;
    procedure InsertText(ATextUTF8: string);
  public
    constructor Create(AContainer: TVectorOriginal); override;
    procedure QuickDefine(const APoint1,APoint2: TPointF); override;
    procedure LoadFromStorage(AStorage: TBGRACustomOriginalStorage); override;
    procedure SaveToStorage(AStorage: TBGRACustomOriginalStorage); override;
    destructor Destroy; override;
    class function Fields: TVectorShapeFields; override;
    class function PreferPixelCentered: boolean; override;
    class function DefaultFontName: string;
    class function DefaultFontEmHeight: single;
    class function CreateEmpty: boolean; override;
    class function StorageClassName: RawByteString; override;
    procedure ConfigureEditor(AEditor: TBGRAOriginalEditor); override;
    procedure Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix; ADraft: boolean); override;
    function GetRenderBounds({%H-}ADestRect: TRect; AMatrix: TAffineMatrix; AOptions: TRenderBoundsOptions = []): TRectF; override;
    function PointInShape(APoint: TPointF): boolean; override;
    function GetIsSlow({%H-}AMatrix: TAffineMatrix): boolean; override;
    procedure KeyDown({%H-}Shift: TShiftState; {%H-}Key: TSpecialKey; var {%H-}AHandled: boolean); override;
    procedure KeyPress({%H-}UTF8Key: string; var {%H-}AHandled: boolean); override;
    property Text: string read FText write SetText;
    property FontName: string read FFontName write SetFontName;
    property FontStyle: TFontStyles read FFontStyle write SetFontStyle;
    property FontEmHeight: single read FFontEmHeight write SetFontEmHeight;
    property FontBidiMode: TFontBidiMode read FFontBidiMode write SetFontBidiMode;
    property HorizotalAlignment: TBidiTextAlignment read FHorizAlign write SetHorizAlign;
    property VerticalAlignment: TTextLayout read FVertAlign write SetVertAlign;
  end;

function FontStyleToStr(AStyle: TFontStyles): string;
function StrToFontStyle(AText: string): TFontStyles;

function FontBidiModeToStr(AMode: TFontBidiMode): string;
function StrToFontBidiMode(AText: string): TFontBidiMode;

implementation

uses BGRATransform, BGRAText, LCVectorialFill, math, BGRAUTF8, BGRAUnicode, Graphics;

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

{ TTextShape }

procedure TTextShape.SetText(AValue: string);
begin
  if FText=AValue then Exit;
  BeginUpdate;
  FText:=AValue;
  FSelStart:=0;
  FSelEnd :=0;
  FreeAndNil(FTextLayout);
  EndUpdate;
end;

procedure TTextShape.SetFontBidiMode(AValue: TFontBidiMode);
begin
  if FFontBidiMode=AValue then Exit;
  BeginUpdate;
  FFontBidiMode:=AValue;
  EndUpdate;
end;

procedure TTextShape.SetFontEmHeight(AValue: single);
begin
  if FFontEmHeight=AValue then Exit;
  BeginUpdate;
  FFontEmHeight:=AValue;
  EndUpdate;
end;

procedure TTextShape.SetFontName(AValue: string);
begin
  if FFontName=AValue then Exit;
  BeginUpdate;
  FFontName:=AValue;
  EndUpdate;
end;

procedure TTextShape.SetFontStyle(AValue: TFontStyles);
begin
  if FFontStyle=AValue then Exit;
  BeginUpdate;
  FFontStyle:=AValue;
  EndUpdate;
end;

procedure TTextShape.SetHorizAlign(AValue: TBidiTextAlignment);
begin
  if FHorizAlign=AValue then Exit;
  BeginUpdate;
  FHorizAlign:=AValue;
  EndUpdate;
end;

procedure TTextShape.SetVertAlign(AValue: TTextLayout);
begin
  if FVertAlign=AValue then Exit;
  BeginUpdate;
  FVertAlign:=AValue;
  EndUpdate;
end;

function TTextShape.PenVisible(AAssumePenFill: boolean): boolean;
begin
  result := not PenFill.IsFullyTransparent or AAssumePenFill;
end;

function TTextShape.AllowShearTransform: boolean;
begin
  Result:= true;
end;

function TTextShape.ShowArrows: boolean;
begin
  Result:= false;
end;

function TTextShape.GetTextLayout(AMatrix: TAffineMatrix): TBidiTextLayout;
var
  box: TAffineBox;
  i: Integer;
  zoom: Single;
begin
  if FTextLayout = nil then
    FTextLayout := TBidiTextLayout.Create(GetFontRenderer(AMatrix), FText);
  box := GetAffineBox(AMatrix,false);
  FTextLayout.FontBidiMode:= FontBidiMode;
  FTextLayout.TopLeft := PointF(0,0);
  zoom := GetTextRenderZoom(AMatrix);
  FTextLayout.AvailableWidth:= box.Width*zoom;
  FTextLayout.AvailableHeight:= box.Height*zoom;
  for i := 0 to FTextLayout.ParagraphCount-1 do
    FTextLayout.ParagraphAlignment[i] := HorizotalAlignment;
  FTextLayout.ParagraphSpacingBelow:= 0.5;
  result:= FTextLayout;
end;

function TTextShape.GetTextLayoutIgnoreMatrix: TBidiTextLayout;
begin
  if FTextLayout = nil then
    result := GetTextLayout(AffineMatrixIdentity)
  else
    result := FTextLayout;
end;

function TTextShape.GetFontRenderer(AMatrix: TAffineMatrix): TBGRACustomFontRenderer;
begin
  if FFontRenderer = nil then
    FFontRenderer := TLCLFontRenderer.Create;

  FFontRenderer.FontEmHeight := Round(FontEmHeight*GetTextRenderZoom(AMatrix));
  FFontRenderer.FontName:= FontName;
  FFontRenderer.FontStyle:= FontStyle;
  FFontRenderer.FontQuality:= fqFineAntialiasing;
  result := FFontRenderer;
end;

function TTextShape.GetTextRenderZoom(AMatrix: TAffineMatrix): single;
begin
  //font to be rendered at a sufficient size to avoid stretching
  result := max(VectLen(AMatrix[1,1],AMatrix[2,1]),
                VectLen(AMatrix[1,2],AMatrix[2,2]));
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
  BeginUpdate;
  selLeft := Min(FSelStart,FSelEnd);
  if selLeft > 0 then
  begin
    delCount := GetTextLayoutIgnoreMatrix.DeleteTextBefore(selLeft, ACount);
    FText := GetTextLayoutIgnoreMatrix.TextUTF8;
    dec(selLeft,delCount);
  end;
  FSelStart := selLeft;
  FSelEnd := selLeft;
  EndUpdate;
end;

procedure TTextShape.DeleteTextAfter(ACount: integer);
var
  delCount, selRight: Integer;
begin
  BeginUpdate;
  selRight := Max(FSelStart,FSelEnd);
  if selRight > 0 then
  begin
    delCount := GetTextLayoutIgnoreMatrix.DeleteText(selRight, ACount);
    FText := GetTextLayoutIgnoreMatrix.TextUTF8;
    inc(selRight,delCount);
  end;
  FSelStart := selRight;
  FSelEnd := selRight;
  EndUpdate;
end;

procedure TTextShape.DeleteSelectedText;
var
  selLeft: Integer;
begin
  if FSelStart <> FSelEnd then
  begin
    BeginUpdate;
    selLeft := Min(FSelStart,FSelEnd);
    GetTextLayoutIgnoreMatrix.DeleteText(selLeft, Abs(FSelEnd-FSelStart));
    FText := GetTextLayoutIgnoreMatrix.TextUTF8;
    FSelStart := selLeft;
    FSelEnd := selLeft;
    EndUpdate;
  end;
end;

procedure TTextShape.InsertText(ATextUTF8: string);
var
  insertCount: Integer;
begin
  BeginUpdate;
  DeleteSelectedText;
  insertCount := GetTextLayoutIgnoreMatrix.InsertText(ATextUTF8, FSelStart);
  FText := GetTextLayoutIgnoreMatrix.TextUTF8;
  Inc(FSelStart, insertCount);
  FSelEnd := FSelStart;
  EndUpdate;
end;

constructor TTextShape.Create(AContainer: TVectorOriginal);
begin
  inherited Create(AContainer);
  SetDefaultFont;
  FHorizAlign:= btaNatural;
  FVertAlign:= tlTop;
  FText := '';
  FSelStart := 0;
  FSelEnd := 0;
end;

procedure TTextShape.QuickDefine(const APoint1, APoint2: TPointF);
var minSize: single;
  p2: TPointF;
begin
  minSize := GetFontRenderer(AffineMatrixIdentity).TextSize('Hg').cy;
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
  font: TBGRACustomOriginalStorage;
begin
  BeginUpdate;
  inherited LoadFromStorage(AStorage);
  Text := AStorage.RawString['text'];
  Font := AStorage.OpenObject('font');
  if Assigned(font) then
  begin
    FontName:= AStorage.RawString['name'];
    FontEmHeight:= AStorage.FloatDef['em-height', DefaultFontEmHeight];
    FontBidiMode:= StrToFontBidiMode(AStorage.RawString['bidi']);
    FontStyle:= StrToFontStyle(AStorage.RawString['style']);
    font.Free;
  end else
    SetDefaultFont;
  EndUpdate;
end;

procedure TTextShape.SaveToStorage(AStorage: TBGRACustomOriginalStorage);
var
  font: TBGRACustomOriginalStorage;
begin
  inherited SaveToStorage(AStorage);
  AStorage.RawString['text'] := Text;
  font := AStorage.OpenObject('font');
  if font = nil then font := AStorage.CreateObject('font');
  AStorage.RawString['name'] := FontName;
  AStorage.Float['em-height'] := FontEmHeight;
  AStorage.RawString['bidi'] := FontBidiModeToStr(FontBidiMode);
  AStorage.RawString['style'] := FontStyleToStr(FontStyle);
  font.Free;
end;

destructor TTextShape.Destroy;
begin
  FreeAndNil(FTextLayout);
  FreeAndNil(FFontRenderer);
  inherited Destroy;
end;

class function TTextShape.Fields: TVectorShapeFields;
begin
  Result:= [vsfPenFill];
end;

class function TTextShape.PreferPixelCentered: boolean;
begin
  Result:= false;
end;

class function TTextShape.DefaultFontName: string;
begin
  result := {$IFDEF WINDOWS}'Arial'{$ELSE}{$IFDEF DARWIN}'Helvetica'{$ELSE}'FreeSans'{$ENDIF}{$ENDIF};
end;

class function TTextShape.DefaultFontEmHeight: single;
begin
  result := 20;
end;

class function TTextShape.CreateEmpty: boolean;
begin
  Result:= true;
end;

procedure TTextShape.ConfigureEditor(AEditor: TBGRAOriginalEditor);
var
  caret: TBidiCaretPos;
  orientation: TPointF;
  m: TAffineMatrix;
  tl: TBidiTextLayout;
  pts: Array Of TPointF;
  i: Integer;
  c: TBGRAPixel;
begin
  inherited ConfigureEditor(AEditor);
  AEditor.AddPolyline(GetAffineBox(AffineMatrixIdentity,true).AsPolygon, true, opsDashWithShadow);
  tl := GetTextLayout(AffineMatrixIdentity);
  caret:= tl.GetCaret(FSelEnd);
  m := GetUntransformedMatrix;
  if not isEmptyPointF(caret.PreviousTop) and (caret.PreviousRightToLeft<>caret.RightToLeft) then
  begin
    orientation := (caret.Bottom-caret.Top)*(1/10);
    orientation := PointF(-orientation.y,orientation.x);
    if caret.RightToLeft then orientation := -orientation;
    AEditor.AddPolyline([m*caret.Bottom,m*caret.Top,m*(caret.Top+orientation)],false, opsSolid);
  end else
    AEditor.AddPolyline([m*caret.Bottom,m*caret.Top],false, opsSolid);
  if FSelStart<>FSelEnd then
  begin
    pts := tl.GetTextEnveloppe(FSelStart, FSelEnd);
    for i := 0 to high(pts) do
      pts[i] := m*pts[i];
    c:= clHighlight;
    c.alpha := 96;
    AEditor.AddPolyline(pts, true, opsDash, c);
  end;
end;

procedure TTextShape.Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix;
  ADraft: boolean);
var
  zoom: Single;
  m: TAffineMatrix;
  tl: TBidiTextLayout;
  fr: TBGRACustomFontRenderer;
  pad: Integer;
  sourceRect,transfRectF,sourceInvRect,destF: TRectF;
  transfRect: TRect;
  tmpSource, tmpTransf: TBGRABitmap;
  scan: TBGRACustomScanner;
begin
  zoom := GetTextRenderZoom(AMatrix);
  if zoom = 0 then exit;
  fr := GetFontRenderer(AMatrix);
  if fr.FontEmHeight = 0 then exit;
  pad := fr.FontEmHeight div 2;

  m := AMatrix*                             //global transform
       GetUntransformedMatrix*              //transform accordng to shape rectangle
       AffineMatrixScale(1/zoom,1/zoom);    //shrink zoomed text if necessary

  tl := GetTextLayout(AMatrix);
  sourceRect := RectF(-pad,-pad,tl.AvailableWidth+pad,tl.TotalTextHeight+pad);

  destF := RectF(ADest.ClipRect.Left,ADest.ClipRect.Top,ADest.ClipRect.Right,ADest.ClipRect.Bottom);
  transfRectF := (m*TAffineBox.AffineBox(sourceRect)).RectBoundsF;
  transfRectF := TRectF.Intersect(transfRectF, destF);

  if not IsAffineMatrixInversible(m) then exit;
  sourceInvRect := (AffineMatrixInverse(m)*TAffineBox.AffineBox(transfRectF)).RectBoundsF;
  sourceRect := TRectF.Intersect(sourceRect,sourceInvRect);
  if IsEmptyRectF(sourceRect) then exit;
  sourceRect.Left := floor(sourceRect.Left);
  sourceRect.Top := floor(sourceRect.Top);
  sourceRect.Right := floor(sourceRect.Right);
  sourceRect.Bottom := floor(sourceRect.Bottom);

  m := m*AffineMatrixTranslation(sourceRect.Left,sourceRect.Top);
  if tl.TotalTextHeight < tl.AvailableHeight then
  case VerticalAlignment of
  tlBottom: m *= AffineMatrixTranslation(0, tl.AvailableHeight-tl.TotalTextHeight);
  tlCenter: m *= AffineMatrixTranslation(0, (tl.AvailableHeight-tl.TotalTextHeight)/2);
  end;

  tl.TopLeft := PointF(-sourceRect.Left,-sourceRect.Top);
  if PenFill.FillType = vftSolid then
  begin
    tmpSource := TBGRABitmap.Create(round(sourceRect.Width),round(sourceRect.Height));
    tl.DrawText(tmpSource, PenFill.SolidColor);
    ADest.PutImageAffine(m, tmpSource, rfHalfCosine, dmDrawWithTransparency);
    tmpSource.Free;
  end
  else
  begin
    tmpSource := TBGRABitmap.Create(round(sourceRect.Width),round(sourceRect.Height),BGRABlack);
    tl.DrawText(tmpSource, BGRAWhite);
    tmpSource.ConvertToLinearRGB;

    with transfRectF do
      transfRect := Rect(floor(Left),floor(Top),ceil(Right),ceil(Bottom));
    tmpTransf := TBGRABitmap.Create(transfRect.Width,transfRect.Height,BGRABlack);
    tmpTransf.PutImageAffine(AffineMatrixTranslation(-transfRect.Left,-transfRect.Top)*m,
                             tmpSource, rfHalfCosine, dmDrawWithTransparency);
    tmpSource.Free;

    scan := PenFill.CreateScanner(AMatrix, ADraft);
    ADest.FillMask(transfRect.Left, transfRect.Top, tmpTransf, scan, dmDrawWithTransparency);
    scan.Free;
    tmpTransf.Free;
  end;
end;

function TTextShape.GetRenderBounds(ADestRect: TRect; AMatrix: TAffineMatrix;
  AOptions: TRenderBoundsOptions): TRectF;
var
  ab: TAffineBox;
  u, v: TPointF;
  lenU, lenV: Single;
begin
  if PenVisible(rboAssumePenFill in AOptions) and
    (Text <> '') then
  begin
    ab := GetAffineBox(AMatrix, false);
    //add margin for text that would be out of bound (for example italic j)
    u := ab.TopRight-ab.TopLeft;
    lenU := VectLen(u);
    if lenU<>0 then u *= (1/lenU);
    u *=(FontEmHeight/2);
    ab.TopLeft -= u;
    ab.TopRight += u;
    ab.BottomLeft -= u;
    v := ab.BottomLeft-ab.TopLeft;
    lenV := VectLen(v);
    if lenV<>0 then v *= (1/lenV);
    v *= (FontEmHeight/2);
    ab.TopLeft -= v;
    ab.TopRight -= v;
    ab.BottomLeft += v;
    result := ab.RectBoundsF;
  end
  else
    result:= EmptyRectF;
end;

function TTextShape.PointInShape(APoint: TPointF): boolean;
begin
  result := GetAffineBox(AffineMatrixIdentity,true).Contains(APoint);
end;

function TTextShape.GetIsSlow(AMatrix: TAffineMatrix): boolean;
begin
  Result:= true;
end;

procedure TTextShape.KeyDown(Shift: TShiftState; Key: TSpecialKey;
  var AHandled: boolean);
var
  idxPara, newPos: Integer;
begin
  if FTextLayout = nil then exit;

  if Key = skDelete then
  begin
    if FSelStart <> FSelEnd then DeleteSelectedText
    else DeleteTextAfter(1);
    AHandled:= true;
  end else
  if Key in [skLeft,skRight] then
  begin
    if (Key = skLeft) xor GetTextLayoutIgnoreMatrix.ParagraphRightToLeft[GetTextLayoutIgnoreMatrix.GetParagraphAt(FSelEnd)] then
    begin
      BeginUpdate;
      if FSelEnd > 0 then
        Dec(FSelEnd, GetTextLayoutIgnoreMatrix.IncludeNonSpacingCharsBefore(FSelEnd,1) );
      if not (ssShift in Shift) then FSelStart := FSelEnd;
      EndUpdate;
    end else
    begin
      BeginUpdate;
      if FSelEnd < GetTextLayoutIgnoreMatrix.CharCount then
        Inc(FSelEnd, GetTextLayoutIgnoreMatrix.IncludeNonSpacingChars(FSelEnd,1) );
      if not (ssShift in Shift) then FSelStart := FSelEnd;
      EndUpdate;
    end;
    AHandled := true;
  end else
  if Key in [skUp,skDown] then
  begin
    if Key = skUp then
      newPos := GetTextLayoutIgnoreMatrix.FindTextAbove(FSelEnd)
    else
      newPos := GetTextLayoutIgnoreMatrix.FindTextBelow(FSelEnd);
    if (newPos <> -1) or (not (ssShift in Shift) and (FSelStart <> FSelEnd)) then
    begin
      BeginUpdate;
      FSelEnd := newPos;
      if not (ssShift in Shift) then FSelStart := FSelEnd;
      EndUpdate;
    end;
    AHandled:= true;
  end else
  if Key = skHome then
  begin
    BeginUpdate;
    if ssCtrl in Shift then
      FSelEnd := 0
    else
    begin
      idxPara := GetTextLayoutIgnoreMatrix.GetParagraphAt(FSelEnd);
      FSelEnd := GetTextLayoutIgnoreMatrix.ParagraphStartIndex[idxPara];
    end;
    if not (ssShift in Shift) then FSelStart := FSelEnd;
    EndUpdate;
    AHandled := true;
  end else
  if Key = skEnd then
  begin
    BeginUpdate;
    if ssCtrl in Shift then
      FSelEnd := GetTextLayoutIgnoreMatrix.CharCount
    else
    begin
      idxPara := GetTextLayoutIgnoreMatrix.GetParagraphAt(FSelEnd);
      FSelEnd := GetTextLayoutIgnoreMatrix.ParagraphEndIndexBeforeParagraphSeparator[idxPara];
    end;
    if not (ssShift in Shift) then FSelStart := FSelEnd;
    EndUpdate;
    AHandled := true;
  end else
  if Key = skReturn then
  begin
    if ssShift in Shift then
      InsertText(UnicodeCharToUTF8(UNICODE_LINE_SEPARATOR))
    else
      InsertText(LineEnding);
    AHandled := true;
  end else
  if Key = skTab then
  begin
    InsertText(#9);
    AHandled := true;
  end{ else
  If (Key = VK_C) and (ssCtrl in Shift) then
  begin
    if SelLength> 0 then
      SetClipboardAsText(GetTextLayoutIgnoreMatrix.CopyText(SelStart, SelLength));
    Key := 0;
  end else
  If (Key = VK_X) and (ssCtrl in Shift) then
  begin
    if SelLength > 0 then
    begin
      SetClipboardAsText(GetTextLayoutIgnoreMatrix.CopyText(SelStart, SelLength));
      DeleteSelection;
    end;
    Key := 0;
  end else
  If (Key = VK_V) and (ssCtrl in Shift) then
  begin
    InsertText(Clipboard.AsText);
    Key := 0;
  end else
  If (Key = VK_A) and (ssCtrl in Shift) then
  begin
    SelStart:= 0;
    SelLength:= GetTextLayoutIgnoreMatrix.CharCount;
    Key := 0;
  end};
end;

procedure TTextShape.KeyPress(UTF8Key: string; var AHandled: boolean);
begin
  if UTF8Key = #8 then
  begin
    if FSelEnd <> FSelStart then DeleteSelectedText
    else DeleteTextBefore(1);
    AHandled := true;
  end
  else
  if UTF8Key >= ' ' then
  begin
    InsertText(UTF8Key);
    AHandled := true;
  end;
end;

class function TTextShape.StorageClassName: RawByteString;
begin
  result := 'text';
end;

initialization

  RegisterVectorShape(TTextShape);

end.

