unit BGRAFontGL;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRAGraphics, BGRAOpenGLType, BGRABitmapTypes,
  AvgLvlTree;

type
  { TRenderedGlyph }

  TRenderedGlyph = class
  private
    FIdentifier: UTF8String;
    FTexture: IBGLTexture;
    FHorizontalOverflowPx, FVerticalOverflowPx, FAdvancePx: integer;
  public
    constructor Create(AIdentifier: UTF8String; ATexture: IBGLTexture;
      AHorizontalOverflowPx, AVerticalOverflowPx: integer);
    procedure Draw(x,y,Scale: single; AColor: TBGRAPixel); overload;
    procedure Draw(x,y,Scale: single; AGradTopLeft, AGradTopRight, AGradBottomRight, AGradBottomLeft: TBGRAPixel); overload;
    property Identifier: UTF8String read FIdentifier;
    property AdvancePx: integer read FAdvancePx;
  end;

  { IBGLRenderedFont }

  IBGLRenderedFont = interface(IBGLFont)
    function GetBackgroundColor: TBGRAPixel;
    function GetColor: TBGRAPixel;
    function GetFontEmHeight: integer;
    function GetFontFullHeight: integer;
    function GetHorizontalOverflow: single;
    function GetName: string;
    function GetQuality: TBGRAFontQuality;
    function GetStyle: TFontStyles;
    function GetVerticalOverflow: single;
    procedure SetBackgroundColor(AValue: TBGRAPixel);
    procedure SetColor(AValue: TBGRAPixel);
    procedure SetFontEmHeight(AValue: integer);
    procedure SetFontFullHeight(AValue: integer);
    procedure SetHorizontalOverflow(AValue: single);
    procedure SetName(AValue: string);
    procedure SetQuality(AValue: TBGRAFontQuality);
    procedure SetStyle(AValue: TFontStyles);
    procedure SetVerticalOverflow(AValue: single);

    property Name: string read GetName write SetName;
    property Style: TFontStyles read GetStyle write SetStyle;
    property Quality: TBGRAFontQuality read GetQuality write SetQuality;
    property EmHeight: integer read GetFontEmHeight write SetFontEmHeight;
    property FullHeight: integer read GetFontFullHeight write SetFontFullHeight;
    property Color: TBGRAPixel read GetColor write SetColor;
    property HorizontalOverflow: single read GetHorizontalOverflow write SetHorizontalOverflow;
    property VerticalOverflow: single read GetVerticalOverflow write SetVerticalOverflow;
    property BackgroundColor: TBGRAPixel read GetBackgroundColor write SetBackgroundColor;
  end;

  { TBGLRenderedFont }

  TBGLRenderedFont = class(TBGLCustomFont,IBGLRenderedFont)
  private
    FGlyphs: TAvgLvlTree;

    FName: string;
    FColor: TBGRAPixel;
    FBackgroundColor: TBGRAPixel;
    FEmHeight: integer;
    FHorizontalOverflow: single;
    FVerticalOverflow: single;
    FQuality: TBGRAFontQuality;
    FStyle: TFontStyles;
    FGradTopLeft, FGradTopRight, FGradBottomRight, FGradBottomLeft: TBGRAPixel;
    FUseGradientColor: boolean;
    FClipped: boolean;
    FWordBreakHandler: TWordBreakHandler;

    function CompareGlyph({%H-}Tree: TAvgLvlTree; Data1, Data2: Pointer): integer;
    function FindGlyph(AIdentifier: string): TAvgLvlTreeNode;
    function GetBackgroundColor: TBGRAPixel;
    function GetColor: TBGRAPixel;
    function GetFontEmHeight: integer;
    function GetGlyph(AIdentifier: string): TRenderedGlyph;
    function GetHorizontalOverflow: single;
    function GetName: string;
    function GetQuality: TBGRAFontQuality;
    function GetStyle: TFontStyles;
    function GetVerticalOverflow: single;
    procedure SetGlyph(AIdentifier: string; AValue: TRenderedGlyph);
    function GetFontFullHeight: integer;
    procedure SetBackgroundColor(AValue: TBGRAPixel);
    procedure SetColor(AValue: TBGRAPixel);
    procedure SetFontEmHeight(AValue: integer);
    procedure SetFontFullHeight(AValue: integer);
    procedure SetHorizontalOverflow(AValue: single);
    procedure SetName(AValue: string);
    procedure SetQuality(AValue: TBGRAFontQuality);
    procedure SetStyle(AValue: TFontStyles);
    procedure SetVerticalOverflow(AValue: single);
  protected
    FRenderer: TBGRACustomFontRenderer;
    FRendererOwned: boolean;
    function LoadFromFile({%H-}AFilename: UTF8String): boolean; override;
    procedure FreeMemoryOnDestroy; override;
    function CreateGlyph(AIdentifier: string): TRenderedGlyph; virtual;
    procedure CopyFontToRenderer; virtual;
    procedure DoTextOut(X, Y: Single; const Text : UTF8String; AColor: TBGRAPixel; AHorizontalAlign: TAlignment; AVerticalAlign: TTextLayout); virtual;
    procedure DoTextOut(X, Y: Single; const Text : UTF8String; AColor: TBGRAPixel); override;
    procedure DoTextRect(X, Y, Width, Height: Single; const Text : UTF8String; AColor: TBGRAPixel); override;
    function GetClipped: boolean; override;
    function GetUseGradientColors: boolean; override;
    procedure SetClipped(AValue: boolean); override;
    procedure SetUseGradientColors(AValue: boolean); override;
    procedure DiscardGlyphs; virtual;
    procedure DefaultWordBreakHandler(var ABefore, AAfter: string);
    procedure SplitText(var ATextUTF8: string; AMaxWidth: single; out ARemainsUTF8: string);
    function GetWrappedLines(ATextUTF8: string; AWidth: single): TStringList;
  public
    constructor Create(ARenderer: TBGRACustomFontRenderer; ARendererOwned: boolean = true);
    procedure FreeMemory; override;
    function TextWidth(const Text: UTF8String): single; override;
    function TextHeight(const {%H-}Text: UTF8String): single; override;
    function TextHeight(const Text: UTF8String; AWidth: single): single; override;
    procedure SetGradientColors(ATopLeft, ATopRight, ABottomRight, ABottomLeft: TBGRAPixel); override;
    property Name: string read GetName write SetName;
    property Style: TFontStyles read GetStyle write SetStyle;
    property Quality: TBGRAFontQuality read GetQuality write SetQuality;
    property EmHeight: integer read GetFontEmHeight write SetFontEmHeight;
    property FullHeight: integer read GetFontFullHeight write SetFontFullHeight;
    property Color: TBGRAPixel read GetColor write SetColor;
    property HorizontalOverflow: single read GetHorizontalOverflow write SetHorizontalOverflow;
    property VerticalOverflow: single read GetVerticalOverflow write SetVerticalOverflow;
    property BackgroundColor: TBGRAPixel read GetBackgroundColor write SetBackgroundColor;
    property WordBreakHandler: TWordBreakHandler read FWordBreakHandler write FWordBreakHandler;
    property Glyph[AIdentifier: string]: TRenderedGlyph read GetGlyph;
  end;

implementation

uses BGRAUTF8;

{ TRenderedGlyph }

constructor TRenderedGlyph.Create(AIdentifier: UTF8String; ATexture: IBGLTexture;
  AHorizontalOverflowPx, AVerticalOverflowPx: integer);
begin
  FIdentifier := AIdentifier;
  FTexture := ATexture;
  FHorizontalOverflowPx:= AHorizontalOverflowPx;
  FVerticalOverflowPx:= AVerticalOverflowPx;
  FAdvancePx := ATexture.Width - 2*FHorizontalOverflowPx;
end;

procedure TRenderedGlyph.Draw(x, y, Scale: single; AColor: TBGRAPixel);
begin
  FTexture.StretchDraw(x-FHorizontalOverflowPx*Scale,y-FVerticalOverflowPx*Scale, FTexture.Width*Scale, FTexture.Height*Scale, AColor);
end;

procedure TRenderedGlyph.Draw(x, y, Scale: single; AGradTopLeft, AGradTopRight,
  AGradBottomRight, AGradBottomLeft: TBGRAPixel);
begin
  FTexture.SetGradientColors(AGradTopLeft,AGradTopRight, AGradBottomRight,AGradBottomLeft);
  FTexture.StretchDraw(x-FHorizontalOverflowPx*Scale,y-FVerticalOverflowPx*Scale, FTexture.Width*Scale, FTexture.Height*Scale);
  FTexture.GradientColors := false;
end;

{ TBGLRenderedFont }

function TBGLRenderedFont.CompareGlyph(Tree: TAvgLvlTree; Data1, Data2: Pointer): integer;
begin
  result := CompareStr(TRenderedGlyph(Data1).Identifier,TRenderedGlyph(Data2).Identifier);
end;

function TBGLRenderedFont.FindGlyph(AIdentifier: string): TAvgLvlTreeNode;
var Comp: integer;
  Node: TAvgLvlTreeNode;
begin
  Node:=FGlyphs.Root;
  while (Node<>nil) do begin
    Comp:=CompareStr(AIdentifier,TRenderedGlyph(Node.Data).Identifier);
    if Comp=0 then break;
    if Comp<0 then begin
      Node:=Node.Left
    end else begin
      Node:=Node.Right
    end;
  end;
  result := Node;
end;

function TBGLRenderedFont.GetBackgroundColor: TBGRAPixel;
begin
  result := FBackgroundColor;
end;

function TBGLRenderedFont.GetColor: TBGRAPixel;
begin
  result := FColor;
end;

function TBGLRenderedFont.GetFontEmHeight: integer;
begin
  result := FEmHeight;
end;

function TBGLRenderedFont.CreateGlyph(AIdentifier: string): TRenderedGlyph;
var b: TBGLCustomBitmap;
  hOverflow, vOverflow: integer;
begin
  CopyFontToRenderer;
  with FRenderer.TextSize(AIdentifier) do
  begin
    hOverflow := round(cx*HorizontalOverflow)+1;
    vOverflow:= round(cy*VerticalOverflow)+1;
    b:= BGLBitmapFactory.Create(cx+2*hOverflow,cy+2*vOverflow,BackgroundColor);
    FRenderer.TextOut(b, hOverflow,vOverflow, AIdentifier, Color, taLeftJustify);
    result:= TRenderedGlyph.Create(AIdentifier,b.MakeTextureAndFree,hOverflow,vOverflow);
  end;
end;

procedure TBGLRenderedFont.CopyFontToRenderer;
begin
  FRenderer.FontName := FName;
  FRenderer.FontEmHeight := FEmHeight;
  FRenderer.FontOrientation := 0;
  FRenderer.FontQuality := FQuality;
  FRenderer.FontStyle := FStyle;
end;

procedure TBGLRenderedFont.DoTextOut(X, Y: Single; const Text: UTF8String;
  AColor: TBGRAPixel; AHorizontalAlign: TAlignment; AVerticalAlign: TTextLayout);
var
  pstr: pchar;
  left,charlen: integer;
  nextchar: string;
  g: TRenderedGlyph;
begin
  if Text = '' then exit;

  pstr := @Text[1];
  left := length(Text);
  case AHorizontalAlign of
  taCenter: x -= round(TextWidth(Text)/2);
  taRightJustify: x -= TextWidth(Text);
  end;
  case AVerticalAlign of
  tlCenter: y -= round(TextHeight(Text)/2);
  tlBottom: y -= TextHeight(Text)*Scale;
  end;
  while left > 0 do
  begin
    charlen := UTF8CharacterLength(pstr);
    setlength(nextchar, charlen);
    move(pstr^, nextchar[1], charlen);
    inc(pstr,charlen);
    dec(left,charlen);

    g := GetGlyph(nextchar);
    if g <> nil then
    begin
      if FUseGradientColor then
        g.Draw(x,y,Scale,FGradTopLeft,FGradTopRight,FGradBottomRight,FGradBottomLeft)
      else
        g.Draw(x,y,Scale,AColor);
      x += (g.AdvancePx + StepX)  * Scale;
    end;
  end;
end;

procedure TBGLRenderedFont.DoTextOut(X, Y: Single; const Text: UTF8String;
  AColor: TBGRAPixel);
begin
  if Justify then
    DoTextOut(X,Y,Text,AColor,taLeftJustify,VerticalAlign)
  else
    DoTextOut(X,Y,Text,AColor,HorizontalAlign,VerticalAlign);
end;

procedure TBGLRenderedFont.DoTextRect(X, Y, Width, Height: Single;
  const Text: UTF8String; AColor: TBGRAPixel);

  procedure DoDrawTextLine(LineY, LineWidth: Single; ALine: string; AJustify: boolean);
  var CurX: single;
    words: TStringList;
    wordStart: integer;
    i: Integer;
  begin
    if AJustify then
    begin
      words := TStringList.Create;
      wordStart := 1;
      for i := 1 to length(ALine) do
      begin
        if ALine[i]=' ' then
        begin
          words.Add(copy(ALine,wordStart,i-wordStart));
          wordStart := i+1;
        end;
      end;
      words.add(copy(ALine,wordStart,length(ALine)+1-wordStart));
      CurX := X;
      LineWidth := 0;
      for i := 0 to words.Count-1 do
        LineWidth += TextWidth(words[i]);

      for i := 0 to words.Count-1 do
      begin
        DoTextOut(CurX+round((Width-LineWidth)/(words.Count-1)*i),LineY,words[i],AColor,taLeftJustify,tlTop);
        CurX += TextWidth(words[i]);
      end;
      words.Free;
    end else
    begin
      Case HorizontalAlign of
      taCenter: CurX := round(X+(Width-LineWidth)/2);
      taRightJustify: CurX := X+Width-LineWidth;
      else
        CurX := X;
      end;
      DoTextOut(CurX,LineY,ALine,AColor,taLeftJustify,tlTop);
    end;
  end;

var CurY: Single;
  lineHeight: Single;
  lines: TStringList;
  i,originalNbLines: Integer;
  maxLineCount: int64;
begin
  If Text='' then exit;
  lines := GetWrappedLines(Text,Width);
  lineHeight := FullHeight * Scale;
  originalNbLines := lines.Count;

  if Clipped then
  begin
    if lineHeight = 0 then exit;
    maxLineCount := trunc(Height/lineHeight);
    if maxLineCount <= 0 then exit;
    while lines.Count > maxLineCount do
      lines.Delete(lines.Count-1);
  end;

  case VerticalAlign of
  tlCenter: CurY := round(Y+( Height - lines.Count*lineHeight )/2);
  tlBottom: CurY := Y + Height - lines.Count*lineHeight;
  else CurY := Y;
  end;

  for i := 0 to lines.Count-1 do
  begin
    DoDrawTextLine(CurY,TextWidth(lines[i]),lines[i],Justify and (i<>originalNbLines-1));
    CurY += lineHeight;
  end;
  lines.Free;
end;

function TBGLRenderedFont.GetGlyph(AIdentifier: string): TRenderedGlyph;
var Node: TAvgLvlTreeNode;
begin
  Node := FindGlyph(AIdentifier);
  if Node = nil then
  begin
    if UTF8Length(AIdentifier)<>1 then
      result := nil
    else
    begin
      result := CreateGlyph(AIdentifier);
      SetGlyph(AIdentifier, result);
    end;
  end
  else
    result := TRenderedGlyph(Node.Data);
end;

function TBGLRenderedFont.GetHorizontalOverflow: single;
begin
  result := FHorizontalOverflow;
end;

function TBGLRenderedFont.GetName: string;
begin
  result := FName;
end;

function TBGLRenderedFont.GetQuality: TBGRAFontQuality;
begin
  result := FQuality;
end;

function TBGLRenderedFont.GetStyle: TFontStyles;
begin
  result := FStyle;
end;

function TBGLRenderedFont.GetVerticalOverflow: single;
begin
  result := FVerticalOverflow;
end;

procedure TBGLRenderedFont.SetGlyph(AIdentifier: string; AValue: TRenderedGlyph);
var Node: TAvgLvlTreeNode;
begin
  if AValue.Identifier <> AIdentifier then
    raise exception.Create('Identifier mismatch');
  Node := FindGlyph(AIdentifier);
  if Node <> nil then
  begin
    if pointer(AValue) <> Node.Data then
      TRenderedGlyph(Node.Data).Free;
    Node.Data := AValue;
  end else
    FGlyphs.Add(pointer(AValue));
end;

procedure TBGLRenderedFont.SetStyle(AValue: TFontStyles);
begin
  if FStyle=AValue then Exit;
  FStyle:=AValue;
  DiscardGlyphs;
end;

procedure TBGLRenderedFont.SetVerticalOverflow(AValue: single);
begin
  if FVerticalOverflow=AValue then Exit;
  FVerticalOverflow:=AValue;
  DiscardGlyphs;
end;

function TBGLRenderedFont.GetClipped: boolean;
begin
  result := FClipped;
end;

function TBGLRenderedFont.GetUseGradientColors: boolean;
begin
  result := FUseGradientColor;
end;

procedure TBGLRenderedFont.SetClipped(AValue: boolean);
begin
  FClipped:= AValue;
end;

procedure TBGLRenderedFont.SetUseGradientColors(AValue: boolean);
begin
  FUseGradientColor:= AValue;
end;

procedure TBGLRenderedFont.DiscardGlyphs;
begin
  FGlyphs.FreeAndClear;
end;

procedure TBGLRenderedFont.DefaultWordBreakHandler(var ABefore, AAfter: string);
begin
  BGRADefaultWordBreakHandler(ABefore,AAfter);
end;

function TBGLRenderedFont.GetWrappedLines(ATextUTF8: string; AWidth: single
  ): TStringList;
var
  ARemains: string;
begin
  result := TStringList.Create;
  repeat
    SplitText(ATextUTF8, AWidth, ARemains);
    result.Add(ATextUTF8);
    ATextUTF8 := ARemains;
  until ARemains = '';
end;

procedure TBGLRenderedFont.SplitText(var ATextUTF8: string; AMaxWidth: single;
  out ARemainsUTF8: string);
var
  pstr: pchar;
  p,left,charlen: integer;
  totalWidth: single;
  firstChar: boolean;
  nextchar: string;
  g: TRenderedGlyph;
begin
  totalWidth := 0;
  if ATextUTF8 = '' then
  begin
    ARemainsUTF8 := '';
    exit;
  end else
  begin
    p := 1;
    pstr := @ATextUTF8[1];
    left := length(ATextUTF8);
    firstChar := true;
    while left > 0 do
    begin
      if RemoveLineEnding(ATextUTF8,p) then
      begin
        ARemainsUTF8 := copy(ATextUTF8,p,length(ATextUTF8)-p+1);
        ATextUTF8 := copy(ATextUTF8,1,p-1);
        exit;
      end;

      charlen := UTF8CharacterLength(pstr);
      setlength(nextchar, charlen);
      move(pstr^, nextchar[1], charlen);
      inc(pstr,charlen);

      g := GetGlyph(nextchar);
      if g <> nil then
      begin
        if not firstChar then totalWidth += StepX*Scale;
        totalWidth += g.AdvancePx*Scale;
        if not firstChar and (totalWidth > AMaxWidth) then
        begin
          ARemainsUTF8:= copy(ATextUTF8,p,length(ATextUTF8)-p+1);
          ATextUTF8 := copy(ATextUTF8,1,p-1);
          if Assigned(FWordBreakHandler) then
            FWordBreakHandler(ATextUTF8,ARemainsUTF8) else
              DefaultWordBreakHandler(ATextUTF8,ARemainsUTF8);
          exit;
        end;
      end;

      dec(left,charlen);
      inc(p,charlen);
      firstChar := false;
    end;
  end;
  ARemainsUTF8 := ''; //no split
end;

procedure TBGLRenderedFont.SetName(AValue: string);
begin
  if FName=AValue then Exit;
  FName:=AValue;
  DiscardGlyphs;
end;

procedure TBGLRenderedFont.SetFontEmHeight(AValue: integer);
begin
  if FEmHeight=AValue then Exit;
  FEmHeight:=AValue;
  DiscardGlyphs;
end;

function TBGLRenderedFont.GetFontFullHeight: integer;
begin
  if FEmHeight < 0 then
    result := -EmHeight
  else
    result := FRenderer.TextSize('Hg').cy;
end;

procedure TBGLRenderedFont.SetBackgroundColor(AValue: TBGRAPixel);
begin
  if FBackgroundColor=AValue then Exit;
  FBackgroundColor:=AValue;
  DiscardGlyphs;
end;

procedure TBGLRenderedFont.SetColor(AValue: TBGRAPixel);
begin
  if FColor=AValue then Exit;
  FColor:=AValue;
  DiscardGlyphs;
end;

procedure TBGLRenderedFont.SetFontFullHeight(AValue: integer);
begin
  EmHeight:= -AValue;
end;

procedure TBGLRenderedFont.SetHorizontalOverflow(AValue: single);
begin
  if FHorizontalOverflow=AValue then Exit;
  FHorizontalOverflow:=AValue;
  DiscardGlyphs;
end;

procedure TBGLRenderedFont.SetQuality(AValue: TBGRAFontQuality);
begin
  if FQuality=AValue then Exit;
  FQuality:=AValue;
  DiscardGlyphs;
end;

function TBGLRenderedFont.LoadFromFile(AFilename: UTF8String): boolean;
begin
  result := false;
end;

procedure TBGLRenderedFont.FreeMemoryOnDestroy;
begin
  FreeMemory;
  if FRendererOwned then FreeAndNil(FRenderer);
  FreeAndNil(FGlyphs);
end;

constructor TBGLRenderedFont.Create(ARenderer: TBGRACustomFontRenderer;
  ARendererOwned: boolean);
begin
  Init;
  FRenderer := ARenderer;
  FRendererOwned := ARendererOwned;

  FName := 'Arial';
  FColor := BGRAWhite;
  FBackgroundColor := BGRAPixelTransparent;
  FEmHeight := 20;
  FStyle := [];
  FHorizontalOverflow := 0.33;
  FVerticalOverflow := 0;
  FQuality := fqFineAntialiasing;

  FGradTopLeft := BGRAWhite;
  FGradTopRight := BGRAWhite;
  FGradBottomLeft := BGRAWhite;
  FGradBottomRight := BGRAWhite;
  FUseGradientColor:= false;
  FClipped:= false;

  FGlyphs := TAvgLvlTree.CreateObjectCompare(@CompareGlyph);
  FWordBreakHandler:= nil;
end;

procedure TBGLRenderedFont.FreeMemory;
begin
  DiscardGlyphs;
  inherited FreeMemory;
end;

function TBGLRenderedFont.TextWidth(const Text: UTF8String): single;
var
  pstr: pchar;
  left,charlen: integer;
  nextchar: string;
  g: TRenderedGlyph;
  firstChar: boolean;
begin
  result := 0;
  if Text = '' then exit;

  firstChar := true;
  pstr := @Text[1];
  left := length(Text);
  while left > 0 do
  begin
    charlen := UTF8CharacterLength(pstr);
    setlength(nextchar, charlen);
    move(pstr^, nextchar[1], charlen);
    inc(pstr,charlen);
    dec(left,charlen);

    g := GetGlyph(nextchar);
    if g <> nil then
    begin
      if firstChar then
        firstchar := false
      else
        result += StepX * Scale;
      result += g.AdvancePx * Scale;
    end;
  end;
end;

function TBGLRenderedFont.TextHeight(const Text: UTF8String): single;
begin
  result := FullHeight * Scale;
end;

function TBGLRenderedFont.TextHeight(const Text: UTF8String; AWidth: single
  ): single;
var
  lines: TStringList;
begin
  lines := GetWrappedLines(Text, AWidth);
  result := lines.Count * (FullHeight * Scale);
  lines.Free;
end;

procedure TBGLRenderedFont.SetGradientColors(ATopLeft, ATopRight, ABottomRight,
  ABottomLeft: TBGRAPixel);
begin
  FGradTopLeft := ATopLeft;
  FGradTopRight := ATopRight;
  FGradBottomLeft := ABottomLeft;
  FGradBottomRight := ABottomRight;
  GradientColors := true;
end;

end.

