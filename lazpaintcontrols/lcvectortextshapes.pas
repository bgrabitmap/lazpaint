unit LCVectorTextShapes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCVectorRectShapes, BGRATextBidi, BGRABitmapTypes, LCVectorOriginal, BGRAGraphics,
  BGRABitmap, BGRALayerOriginal, BGRACanvas2D;

type

  { TTextShape }

  TTextShape = class(TCustomRectShape)
  private
    FAltitudePercent: single;
    FPenPhong: boolean;
    FFontBidiMode: TFontBidiMode;
    FFontEmHeight: single;
    FFontName: string;
    FFontStyle: TFontStyles;
    FLightPosition: TPointF;
    FText: string;
    FSelStart,FSelEnd: integer;
    FMouseSelecting: boolean;
    FVertAlign: TTextLayout;
    function GetBidiParagraphAlignment: TBidiTextAlignment;
    function GetParagraphAlignment: TAlignment;
    procedure OnMoveLightPos({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF;
      {%H-}AShift: TShiftState);
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
    procedure DoOnChange; override;
    procedure SetGlobalMatrix(AMatrix: TAffineMatrix);
    function PenVisible(AAssumePenFill: boolean = false): boolean;
    function AllowShearTransform: boolean; override;
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
    procedure DeleteSelectedText;
    procedure InsertText(ATextUTF8: string);
    procedure SelectWithMouse(X,Y: single; AExtend: boolean);
    function HasOutline: boolean;
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
    class function DefaultAltitudePercent: single;
    class function CreateEmpty: boolean; override;
    class function StorageClassName: RawByteString; override;
    class function Usermodes: TVectorShapeUsermodes; override;
    procedure ConfigureEditor(AEditor: TBGRAOriginalEditor); override;
    procedure Render(ADest: TBGRABitmap; ARenderOffset: TPoint; AMatrix: TAffineMatrix; ADraft: boolean); override;
    function GetRenderBounds({%H-}ADestRect: TRect; AMatrix: TAffineMatrix; AOptions: TRenderBoundsOptions = []): TRectF; override;
    function PointInShape(APoint: TPointF): boolean; override;
    function GetIsSlow({%H-}AMatrix: TAffineMatrix): boolean; override;
    procedure MouseMove({%H-}Shift: TShiftState; {%H-}X, {%H-}Y: single; var {%H-}ACursor: TOriginalEditorCursor; var {%H-}AHandled: boolean); override;
    procedure MouseDown({%H-}RightButton: boolean; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: single; var {%H-}ACursor: TOriginalEditorCursor; var {%H-}AHandled: boolean); override;
    procedure MouseUp({%H-}RightButton: boolean; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: single; var {%H-}ACursor: TOriginalEditorCursor; var {%H-}AHandled: boolean); override;
    procedure KeyDown({%H-}Shift: TShiftState; {%H-}Key: TSpecialKey; var {%H-}AHandled: boolean); override;
    procedure KeyPress({%H-}UTF8Key: string; var {%H-}AHandled: boolean); override;
    procedure SetFontNameAndStyle(AFontName: string; AFontStyle: TFontStyles);
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
  end;

function FontStyleToStr(AStyle: TFontStyles): string;
function StrToFontStyle(AText: string): TFontStyles;

function FontBidiModeToStr(AMode: TFontBidiMode): string;
function StrToFontBidiMode(AText: string): TFontBidiMode;

implementation

uses BGRATransform, BGRAText, BGRAVectorize, LCVectorialFill, math,
  BGRAUTF8, BGRAUnicode, Graphics, Clipbrd, LCLType, LCLIntf,
  BGRAGradients, BGRACustomTextFX;

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

function TTextShape.GetBidiParagraphAlignment: TBidiTextAlignment;
var
  tl: TBidiTextLayout;
  paraIndex: Integer;
begin
  tl := GetTextLayout;
  paraIndex := tl.GetParagraphAt(FSelEnd);
  result := tl.ParagraphAlignment[paraIndex];
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

procedure TTextShape.OnMoveLightPos(ASender: TObject; APrevCoord,
  ANewCoord: TPointF; AShift: TShiftState);
begin
  LightPosition := ANewCoord;
end;

procedure TTextShape.SetAltitudePercent(AValue: single);
begin
  if AValue < 0 then AValue := 0;
  if AValue > 100 then AValue := 100;
  if FAltitudePercent=AValue then Exit;
  BeginUpdate;
  FAltitudePercent:=AValue;
  EndUpdate;
end;

procedure TTextShape.SetPenPhong(AValue: boolean);
begin
  if FPenPhong=AValue then Exit;
  BeginUpdate;
  FPenPhong:=AValue;
  EndUpdate;
end;

procedure TTextShape.SetFontEmHeight(AValue: single);
begin
  if FFontEmHeight=AValue then Exit;
  BeginUpdate;
  FFontEmHeight:=AValue;
  if Assigned(FTextLayout) then FTextLayout.InvalidateLayout;
  EndUpdate;
end;

procedure TTextShape.SetFontName(AValue: string);
begin
  if FFontName=AValue then Exit;
  BeginUpdate;
  FFontName:=AValue;
  if Assigned(FTextLayout) then FTextLayout.InvalidateLayout;
  EndUpdate;
end;

procedure TTextShape.SetFontStyle(AValue: TFontStyles);
begin
  if FFontStyle=AValue then Exit;
  BeginUpdate;
  FFontStyle:=AValue;
  if Assigned(FTextLayout) then FTextLayout.InvalidateLayout;
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
    paraIndex := 0;
    paraIndex2:= tl.ParagraphCount;
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
      BeginUpdate;
      needUpdate := true;
    end;
    tl.ParagraphAlignment[i] := AValue;
  end;
  if needUpdate then EndUpdate;
end;

procedure TTextShape.SetLightPosition(AValue: TPointF);
begin
  if FLightPosition=AValue then Exit;
  BeginUpdate;
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
    paraIndex := 0;
    paraIndex2:= tl.ParagraphCount;
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
        BeginUpdate;
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
  BeginUpdate;
  FVertAlign:=AValue;
  EndUpdate;
end;

procedure TTextShape.DoOnChange;
var freeRenderer: boolean;
begin
  if Assigned(FFontRenderer) then
  begin
    freeRenderer := false;
    if OutlineFill.FillType <> vftNone then
    begin
      if not (FFontRenderer is TBGRAVectorizedFontRenderer) then
        freeRenderer:= true;
    end else
    begin
      if not (FFontRenderer is TLCLFontRenderer) then
        freeRenderer:= true;
    end;
    if freeRenderer then
    begin
      FreeAndNil(FFontRenderer);
      if Assigned(FTextLayout) then
        FTextLayout.FontRenderer := GetFontRenderer;
    end;
  end;
  inherited DoOnChange;
end;

procedure TTextShape.SetGlobalMatrix(AMatrix: TAffineMatrix);
begin
  if AMatrix = FGlobalMatrix then exit;
  FGlobalMatrix := AMatrix;
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

function TTextShape.GetTextLayout: TBidiTextLayout;
var
  box: TAffineBox;
begin
  if FTextLayout = nil then
    FTextLayout := TBidiTextLayout.Create(GetFontRenderer, FText)
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
  newEmHeight: integer;
begin
  if FFontRenderer = nil then
  begin
    if OutlineFill.FillType <> vftNone then
      FFontRenderer := TBGRAVectorizedFontRenderer.Create
    else
    begin
      FFontRenderer := TLCLFontRenderer.Create;
      TLCLFontRenderer(FFontRenderer).OverrideUnderlineDecoration:= true;
    end;
  end;
  newEmHeight := Round(FontEmHeight*GetTextRenderZoom);
  if (newEmHeight <> FFontRenderer.FontEmHeight) or
     (FFontRenderer.FontName <> FontName) or
     (FFontRenderer.FontStyle <> FontStyle) or
     (FFontRenderer.FontQuality <> fqFineAntialiasing) then
  begin
    FFontRenderer.FontEmHeight := newEmHeight;
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
  BeginUpdate;
  selLeft := Min(FSelStart,FSelEnd);
  if selLeft > 0 then
  begin
    delCount := GetTextLayout.DeleteTextBefore(selLeft, ACount);
    FText := GetTextLayout.TextUTF8;
    dec(selLeft,delCount);
  end;
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
  BeginUpdate;
  selRight := Max(FSelStart,FSelEnd);
  tl := GetTextLayout;
  if selRight+ACount <= tl.CharCount then
  begin
    tl.DeleteText(selRight, ACount);
    FText := tl.TextUTF8;
  end;
  FSelStart := selRight;
  FSelEnd := selRight;
  EndUpdate;
end;

procedure TTextShape.DeleteSelectedText;
var
  selLeft: Integer;
begin
  if UserMode <> vsuEditText then exit;
  if FSelStart <> FSelEnd then
  begin
    BeginUpdate;
    selLeft := Min(FSelStart,FSelEnd);
    GetTextLayout.DeleteText(selLeft, Abs(FSelEnd-FSelStart));
    FText := GetTextLayout.TextUTF8;
    FSelStart := selLeft;
    FSelEnd := selLeft;
    EndUpdate;
  end;
end;

procedure TTextShape.InsertText(ATextUTF8: string);
var
  insertCount: Integer;
begin
  if UserMode <> vsuEditText then exit;
  BeginUpdate;
  DeleteSelectedText;
  insertCount := GetTextLayout.InsertText(ATextUTF8, FSelStart);
  FText := GetTextLayout.TextUTF8;
  Inc(FSelStart, insertCount);
  FSelEnd := FSelStart;
  EndUpdate;
end;

procedure TTextShape.SelectWithMouse(X, Y: single; AExtend: boolean);
var
  newPos: Integer;
  tl: TBidiTextLayout;
  zoom: Single;
begin
  tl := GetTextLayout;
  zoom := GetTextRenderZoom;
  newPos := tl.GetCharIndexAt(AffineMatrixScale(zoom,zoom)*AffineMatrixInverse(GetUntransformedMatrix)*PointF(X,Y));
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
  FAltitudePercent:= DefaultAltitudePercent;
  FLightPosition := PointF(0,0);
end;

procedure TTextShape.QuickDefine(const APoint1, APoint2: TPointF);
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
  Result:= [vsfPenFill,vsfOutlineFill];
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

class function TTextShape.DefaultAltitudePercent: single;
begin
  result := 30;
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
  i, idxLight: Integer;
  c: TBGRAPixel;
  zoom: Single;
begin
  inherited ConfigureEditor(AEditor);
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
      TVectorOriginalEditor(AEditor).AddLabel(idxLight, LightPositionCaption, taCenter, tlTop);
  end;
end;

procedure TTextShape.Render(ADest: TBGRABitmap; ARenderOffset: TPoint; AMatrix: TAffineMatrix;
  ADraft: boolean);

  function GetTextPhongHeight: integer;
  begin
    result := round(AltitudePercent/100 * FontEmHeight*0.15);
  end;

  function CreateShader(AOfsX,AOfsY: integer): TPhongShading;
  begin
    result := TPhongShading.Create;
    result.AmbientFactor := 0.6;
    result.NegativeDiffusionFactor := 0.15;
    result.LightPosition := Point(round(LightPosition.x)+AOfsX+ARenderOffset.X,
                                  round(LightPosition.y)+AOfsY+ARenderOffset.Y);
    result.LightPositionZ := round(max(AltitudePercent, 1.2*GetTextPhongHeight));
  end;

var
  zoom: Single;
  m: TAffineMatrix;
  tl: TBidiTextLayout;
  fr: TBGRACustomFontRenderer;
  pad: Integer;
  sourceRectF,transfRectF,sourceInvRect,destF: TRectF;
  transfRect: TRect;
  tmpSource, tmpTransf, tmpTransfMask: TBGRABitmap;
  scan: TBGRACustomScanner;
  ctx: TBGRACanvas2D;
  rf: TResampleFilter;
  storeImage: Boolean;
  shader: TPhongShading;
  textFx: TBGRACustomTextEffect;
begin
  RetrieveRenderStorage(AMatrix, transfRect, tmpTransf);
  if Assigned(tmpTransf) then
  begin
    ADest.PutImage(transfRect.Left+ARenderOffset.X,transfRect.Top+ARenderOffset.Y, tmpTransf,dmDrawWithTransparency);
    tmpTransf.Free;
    exit;
  end;

  if PenFill.IsFullyTransparent and not HasOutline then exit;
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

  storeImage := not ADraft and CanHaveRenderStorage;
  if storeImage then
    destF := rectF(0,0,ADest.Width,ADest.Height)
  else
  begin
    destF := RectF(ADest.ClipRect.Left,ADest.ClipRect.Top,ADest.ClipRect.Right,ADest.ClipRect.Bottom);
    if PenPhong then
    begin
      destF.Left -= 1;
      destF.Top -= 1;
      destF.Right += 1;
      destF.Bottom += 1;
    end;
  end;

  transfRectF := (m*TAffineBox.AffineBox(sourceRectF)).RectBoundsF;
  transfRectF := TRectF.Intersect(transfRectF, destF);

  if not IsAffineMatrixInversible(m) then exit;
  sourceInvRect := (AffineMatrixInverse(m)*TAffineBox.AffineBox(transfRectF)).RectBoundsF;
  sourceInvRect.Top := floor(sourceInvRect.Top);
  sourceInvRect.Bottom := ceil(sourceInvRect.Bottom);
  sourceRectF := TRectF.Intersect(sourceRectF,sourceInvRect);
  if IsEmptyRectF(sourceRectF) then exit;
  sourceRectF.Left := floor(sourceRectF.Left);
  sourceRectF.Top := floor(sourceRectF.Top);
  sourceRectF.Right := floor(sourceRectF.Right);
  sourceRectF.Bottom := sourceRectF.Bottom;

  m := m*AffineMatrixTranslation(sourceRectF.Left,sourceRectF.Top);
  if tl.TotalTextHeight < tl.AvailableHeight then
  case VerticalAlignment of
  tlBottom: m *= AffineMatrixTranslation(0, tl.AvailableHeight-tl.TotalTextHeight);
  tlCenter: m *= AffineMatrixTranslation(0, (tl.AvailableHeight-tl.TotalTextHeight)/2);
  end;

  tl.TopLeft := PointF(-sourceRectF.Left,-sourceRectF.Top);
  with transfRectF do
    transfRect := Rect(floor(Left),floor(Top),ceil(Right),ceil(Bottom));

  if HasOutline then
  begin
    tmpTransf := TBGRABitmap.Create(transfRect.Width,transfRect.Height);
    ctx := tmpTransf.Canvas2D;
    ctx.transform(AffineMatrixTranslation(-transfRect.Left,-transfRect.Top)*m);
    ctx.fillMode := fmWinding;
    ctx.antialiasing:= not ADraft;
    ctx.beginPath;
    tl.PathText(ctx);
    ctx.resetTransform;

    tmpTransfMask := TBGRABitmap.Create(transfRect.Width,transfRect.Height, BGRABlack);
    ctx := tmpTransfMask.Canvas2D;
    ctx.linearBlend:= true;
    ctx.transform(AffineMatrixTranslation(-transfRect.Left,-transfRect.Top)*m);

    if PenPhong and not PenFill.IsFullyTransparent then
    begin
      ctx := tmpTransf.Canvas2D;
      tmpTransf.Fill(BGRABlack);
      ctx.linearBlend:= true;
      ctx.fillStyle(BGRAWhite);
      ctx.fill;
      textFx := TBGRACustomTextEffect.Create(tmpTransf, false, tmpTransf.Width,tmpTransf.Height, Point(0,0));
      tmpTransf.FillTransparent;
      ctx.linearBlend:= false
    end else
      textFx := nil;

    if OutLineFill.FillType <> vftNone then
    begin
      ctx := tmpTransf.Canvas2D;
      ctx.lineWidth := OutlineWidth;
      ctx.lineJoinLCL:= pjsRound;
      ctx.lineStyle(psSolid);
      if OutlineFill.FillType = vftSolid then
      begin
        ctx.strokeStyle(OutlineFill.SolidColor);
        ctx.stroke;
      end else
      if OutlineFill.FillType <> vftNone then
      begin
        scan := OutlineFill.CreateScanner(AffineMatrixTranslation(-transfRect.Left,-transfRect.Top)*FGlobalMatrix, ADraft);
        ctx.strokeStyle(scan);
        ctx.stroke;
        ctx.strokeStyle(BGRABlack);
        scan.Free;
      end;
    end;

    if Assigned(textFx) then
    begin
      scan := PenFill.CreateScanner(AffineMatrixTranslation(-transfRect.Left,-transfRect.Top)*FGlobalMatrix, ADraft);
      shader:= CreateShader(-transfRect.Left, -transfRect.Top);
      textFx.DrawShaded(tmpTransf, 0,0, shader, GetTextPhongHeight, scan);
      shader.Free;
      scan.Free;
      textFx.Free;
    end else
    if not PenFill.IsFullyTransparent then
    begin
      ctx := tmpTransf.Canvas2D;
      if PenFill.FillType = vftSolid then
      begin
        ctx.fillStyle(PenFill.SolidColor);
        ctx.fill;
      end else
      if PenFill.FillType <> vftNone then
      begin
        scan := PenFill.CreateScanner(AffineMatrixTranslation(-transfRect.Left,-transfRect.Top)*FGlobalMatrix, ADraft);
        ctx.fillStyle(scan);
        ctx.fill;
        ctx.fillStyle(BGRABlack);
        scan.Free;
      end;
    end;

    ctx := tmpTransfMask.Canvas2D;
    ctx.beginPath;
    ctx.rect(0,0,sourceRectF.Width,sourceRectF.Height);
    ctx.fillStyle(BGRAWhite);
    ctx.fill;

    tmpTransf.ApplyMask(tmpTransfMask);
    tmpTransfMask.Free;

    ADest.PutImage(transfRect.Left, transfRect.Top, tmpTransf, dmDrawWithTransparency);
  end else
  begin
    if ADraft then rf := rfBox else rf := rfHalfCosine;
    if storeImage then
      tmpTransf := TBGRABitmap.Create(transfRect.Width,transfRect.Height)
    else
      tmpTransf := nil;

    if not PenPhong and (PenFill.FillType = vftSolid) then
    begin
      tmpSource := TBGRABitmap.Create(round(sourceRectF.Width),ceil(sourceRectF.Height));
      tl.DrawText(tmpSource,PenFill.SolidColor);
      if frac(sourceRectF.Height) > 0 then
        tmpSource.EraseLine(0,floor(sourceRectF.Height),tmpSource.Width,floor(sourceRectF.Height), round((1-frac(sourceRectF.Height))*255), false);
      if assigned(tmpTransf) then
        tmpTransf.PutImageAffine(AffineMatrixTranslation(-transfRect.Left,-transfRect.Top)*m, tmpSource, rf, dmDrawWithTransparency, 255, false)
      else
        ADest.PutImageAffine(m, tmpSource, rf, dmDrawWithTransparency, 255, false);
      tmpSource.Free;
    end
    else
    if PenFill.FillType <> vftNone then
    begin
      tmpSource := TBGRABitmap.Create(round(sourceRectF.Width),ceil(sourceRectF.Height),BGRABlack);
      tmpSource.LinearAntialiasing:= true;
      tl.DrawText(tmpSource,BGRAWhite);
      if frac(sourceRectF.Height) > 0 then
        tmpSource.DrawLine(0,floor(sourceRectF.Height),tmpSource.Width,floor(sourceRectF.Height), BGRA(0,0,0,round((1-frac(sourceRectF.Height))*255)), false);

      tmpTransfMask := TBGRABitmap.Create(transfRect.Width,transfRect.Height,BGRABlack);
      tmpTransfMask.PutImageAffine(AffineMatrixTranslation(-transfRect.Left,-transfRect.Top)*m,
                               tmpSource, rf, dmDrawWithTransparency, 255, false);
      tmpSource.Free;

      if Assigned(tmpTransf) then
      begin
        scan := PenFill.CreateScanner(AffineMatrixTranslation(-transfRect.Left,-transfRect.Top)*FGlobalMatrix, ADraft);
        if PenPhong then
        begin
          shader:= CreateShader(-transfRect.Left, -transfRect.Top);
          textFx := TBGRACustomTextEffect.Create(tmpTransfMask, false, tmpTransfMask.Width,tmpTransfMask.Height, Point(0,0));
          textFx.DrawShaded(tmpTransf, 0,0, shader, GetTextPhongHeight, scan);
          textFx.Free;
          shader.Free;
        end else
          tmpTransf.FillMask(0, 0, tmpTransfMask, scan, dmDrawWithTransparency)
      end
      else
      begin
        scan := PenFill.CreateScanner(FGlobalMatrix, ADraft);
        if PenPhong then
        begin
          shader:= CreateShader(0,0);
          textFx := TBGRACustomTextEffect.Create(tmpTransfMask, false, tmpTransfMask.Width,tmpTransfMask.Height, Point(0,0));
          textFx.DrawShaded(ADest, transfRect.Left, transfRect.Top, shader, GetTextPhongHeight, scan);
          textFx.Free;
          shader.Free;
        end else
          ADest.FillMask(transfRect.Left, transfRect.Top, tmpTransfMask, scan, dmDrawWithTransparency);
      end;
      scan.Free;
      tmpTransfMask.Free;
    end;

    if Assigned(tmpTransf) then
      ADest.PutImage(transfRect.Left, transfRect.Top, tmpTransf, dmDrawWithTransparency);
  end;

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
  if (PenVisible(rboAssumePenFill in AOptions) or HasOutline) and
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

function TTextShape.GetIsSlow(AMatrix: TAffineMatrix): boolean;
begin
  Result:= true;
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
  stream: TStringStream;
  tl: TBidiTextLayout;
  txt: String;
begin
  if (FTextLayout = nil) or (Usermode <> vsuEditText) then exit;

  if Key = skDelete then
  begin
    if FSelStart <> FSelEnd then DeleteSelectedText
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
  if (Key in[skC,skX]) and (ssCtrl in Shift) then
  begin
    if FSelEnd <> FSelStart then
    begin
      stream := nil;
      try
        Clipboard.Clear;
        stream := TStringStream.Create(GetTextLayout.CopyText(min(FSelStart,FSelEnd),abs(FSelEnd-FSelStart)));
        Clipboard.SetFormat(PredefinedClipboardFormat(pcfText), stream);
      finally
        stream.Free;
      end;
      if Key = skX then DeleteSelectedText;
      AHandled:= true;
    end;
  end else
  if (Key = skV) and (ssCtrl in Shift) then
  begin
    if Clipboard.HasFormat(PredefinedClipboardFormat(pcfText)) then
    begin
      txt := Clipboard.AsText;
      txt := StringReplace(txt, #13#10, #10, [rfReplaceAll]);
      txt := StringReplace(txt, #10#13, #10, [rfReplaceAll]);
      txt := StringReplace(txt, #13, #10, [rfReplaceAll]);
      txt := StringReplace(txt, UnicodeCharToUTF8(UNICODE_PARAGRAPH_SEPARATOR), #10, [rfReplaceAll]);
      txt := StringReplace(txt, UnicodeCharToUTF8(UNICODE_NEXT_LINE), #10, [rfReplaceAll]);
      InsertText(txt);
      AHandled:= true;
    end;
  end else
  if (Key = skA) and (ssCtrl in Shift) then
  begin
    BeginEditingUpdate;
    FSelStart:= 0;
    FSelEnd:= GetTextLayout.CharCount;
    EndEditingUpdate;
  end;
end;

procedure TTextShape.KeyPress(UTF8Key: string; var AHandled: boolean);
begin
  if (Usermode = vsuEditText) and (UTF8Key = #8) then
  begin
    if FSelEnd <> FSelStart then DeleteSelectedText
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

procedure TTextShape.SetFontNameAndStyle(AFontName: string;
  AFontStyle: TFontStyles);
begin
  if (AFontName <> FFontName) or (AFontStyle <> FFontStyle) then
  begin
    BeginUpdate;
    FFontName := AFontName;
    FFontStyle:= AFontStyle;
    EndUpdate;
  end;
end;

class function TTextShape.StorageClassName: RawByteString;
begin
  result := 'text';
end;

class function TTextShape.Usermodes: TVectorShapeUsermodes;
begin
  Result:=inherited Usermodes + [vsuEditText];
end;

initialization

  RegisterVectorShape(TTextShape);

end.

