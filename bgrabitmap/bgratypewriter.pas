unit BGRATypewriter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AvgLvlTree, BGRABitmapTypes, BGRACanvas2D, BGRATransform;

type
  TGlyphBoxes = array of record
    Glyph: string;
    Box: TAffineBox;
  end;

  { TBGRAGlyph }

  TBGRAGlyph = class
  protected
    FIdentifier: string;
  public
    Width,Height: single;
    constructor Create(AIdentifier: string); virtual;
    procedure Path(ADest: TBGRACanvas2D; AMatrix: TAffineMatrix); virtual; abstract;
    property Identifier: string read FIdentifier;
  end;

  { TBGRAPolygonalGlyph }

  TBGRAPolygonalGlyph = class(TBGRAGlyph)
  private
    procedure SetQuadraticCurves(AValue: boolean);
  protected
    FQuadraticCurves: boolean;
    Points: array of TPointF;
    Curves: array of record
      isCurvedToNext,isCurvedToPrevious: boolean;
      Center,ControlPoint,NextCenter: TPointF;
    end;
    function MaybeCurve(start1,end1,start2,end2: integer): boolean;
    procedure ComputeQuadraticCurves;
  public
    Offset: TPointF;
    constructor Create(AIdentifier: string); override;
    procedure SetPoints(const APoints: array of TPointF);
    procedure Path(ADest: TBGRACanvas2D; AMatrix: TAffineMatrix); override;
    property QuadraticCurves: boolean read FQuadraticCurves write SetQuadraticCurves;
  end;

  { TBGRACustomTypeWriter }

  TBGRACustomTypeWriter = class
  private
    FGlyphs: TAvgLvlTree;
  protected
    TypeWriterMatrix: TAffineMatrix;
    function CompareGlyph({%H-}Tree: TAvgLvlTree; Data1, Data2: Pointer): integer;
    function FindGlyph(AIdentifier: string): TAvgLvlTreeNode;
    function GetGlyph(AIdentifier: string): TBGRAGlyph; virtual;
    procedure SetGlyph(AIdentifier: string; AValue: TBGRAGlyph);
    procedure TextPath(ADest: TBGRACanvas2D; AText: string; X,Y: Single; AAlign: TBGRATypeWriterAlignment = twaTopLeft);
    procedure GlyphPath(ADest: TBGRACanvas2D; AIdentifier: string; X,Y: Single; AAlign: TBGRATypeWriterAlignment = twaTopLeft);
    procedure DrawLastPath(ADest: TBGRACanvas2D);
    procedure ClearGlyphs;
    procedure RemoveGlyph(AIdentifier: string);
    procedure AddGlyph(AGlyph: TBGRAGlyph);
    function GetGlyphMatrix(AGlyph: TBGRAGlyph; X,Y: Single; AAlign: TBGRATypeWriterAlignment): TAffineMatrix;
    function GetTextMatrix(AText: string; X,Y: Single; AAlign: TBGRATypeWriterAlignment): TAffineMatrix;
    property Glyph[AIdentifier: string]: TBGRAGlyph read GetGlyph write SetGlyph;
  public
    OutlineMode: TBGRATypeWriterOutlineMode;
    constructor Create;
    procedure DrawGlyph(ADest: TBGRACanvas2D; AIdentifier: string; X,Y: Single; AAlign: TBGRATypeWriterAlignment = twaTopLeft);
    procedure DrawText(ADest: TBGRACanvas2D; AText: string; X,Y: Single; AAlign: TBGRATypeWriterAlignment = twaTopLeft);
    function GetGlyphBox(AIdentifier: string; X,Y: Single; AAlign: TBGRATypeWriterAlignment = twaTopLeft): TAffineBox;
    function GetTextBox(AText: string; X,Y: Single; AAlign: TBGRATypeWriterAlignment = twaTopLeft): TAffineBox;
    function GetTextGlyphBoxes(AText: string; X,Y: Single; AAlign: TBGRATypeWriterAlignment = twaTopLeft): TGlyphBoxes;
    destructor Destroy; override;
  end;

implementation

uses LCLProc;

{ TBGRAPolygonalGlyph }

procedure TBGRAPolygonalGlyph.SetQuadraticCurves(AValue: boolean);
begin
  if FQuadraticCurves=AValue then Exit;
  FQuadraticCurves:=AValue;
  Curves := nil;
end;

function TBGRAPolygonalGlyph.MaybeCurve(start1,end1,start2,end2: integer): boolean;
var
  u,v: TPointF;
  lu,lv: single;
begin
  if (start1=-1) or (end1=-1) or (start2=-1) or (end2=-1) then
  begin
    result := false;
    exit;
  end;
  u := pointF(points[end1].x - points[start1].x, points[end1].y - points[start1].y);
  lu := sqrt(u*u);
  if lu <> 0 then u *= 1/lu;
  v := pointF(points[end2].x - points[start2].x, points[end2].y - points[start2].y);
  lv := sqrt(v*v);
  if lv <> 0 then v *= 1/lv;

  result := u*v > 0.707;
end;

procedure TBGRAPolygonalGlyph.ComputeQuadraticCurves;
var
  i,FirstPointIndex,NextPt,NextPt2: integer;
begin
  setlength(Curves, length(points));
  FirstPointIndex := 0;
  for i := 0 to high(points) do
  begin
    Curves[i].isCurvedToNext := false;
    Curves[i].isCurvedToPrevious := false;
    Curves[i].Center := EmptyPointF;
    Curves[i].ControlPoint := EmptyPointF;
    Curves[i].NextCenter := EmptyPointF;

    if IsEmptyPointF(Points[i]) then
    begin
      FirstPointIndex := i+1;
    end else
    begin
      NextPt := i+1;
      if (NextPt = length(points)) or isEmptyPointF(points[NextPt]) then NextPt := FirstPointIndex;
      NextPt2 := NextPt+1;
      if (NextPt2 = length(points)) or isEmptyPointF(points[NextPt2]) then NextPt2 := FirstPointIndex;

      Curves[i].Center := (points[i]+points[NextPt])*0.5;
      Curves[i].NextCenter := (points[NextPt]+points[NextPt2])*0.5;

      Curves[i].isCurvedToNext:= MaybeCurve(i,NextPt,NextPt,NextPt2);
      Curves[NextPt].isCurvedToPrevious := Curves[i].isCurvedToNext;
      Curves[i].ControlPoint := points[NextPt];
    end;
  end;
end;

constructor TBGRAPolygonalGlyph.Create(AIdentifier: string);
begin
  inherited Create(AIdentifier);
  Offset := PointF(0,0);
end;

procedure TBGRAPolygonalGlyph.SetPoints(const APoints: array of TPointF);
var i: integer;
begin
  SetLength(Points,length(APoints));
  for i := 0 to high(points) do
    points[i] := APoints[i];
  Curves := nil;
end;

procedure TBGRAPolygonalGlyph.Path(ADest: TBGRACanvas2D; AMatrix: TAffineMatrix);
var i: integer;
  nextMove: boolean;
  startCoord: TPointF;
begin
  if Points = nil then exit;

  if (Curves = nil) and FQuadraticCurves then ComputeQuadraticCurves;
  nextMove := true;
  AMatrix := AMatrix*AffineMatrixTranslation(Offset.X,Offset.Y);
  for i := 0 to high(Points) do
    if isEmptyPointF(Points[i]) then
    begin
      if not nextMove then ADest.closePath;
      nextMove := true;
    end else
    if FQuadraticCurves then
    begin
      with Curves[i] do
      begin
        if nextMove then
        begin
          if not isCurvedToPrevious then
            startCoord := Points[i]
          else
            startCoord := Center;
          ADest.moveTo(AMatrix*startCoord);
          nextMove := false;
        end else
          if not isCurvedToPrevious then
            ADest.lineTo(AMatrix*Points[i]);

        if isCurvedToNext then
        begin
          if not isCurvedToPrevious then ADest.lineTo(AMatrix*Center);
          ADest.quadraticCurveTo(AMatrix*ControlPoint,AMatrix*NextCenter);
        end;
      end;
    end else
    begin
      if nextMove then
      begin
        ADest.moveTo(AMatrix*Points[i]);
        nextMove := false;
      end else
        ADest.lineTo(AMatrix*Points[i]);
    end;
  if not nextmove then ADest.closePath;
end;

{ TBGRAGlyph }

constructor TBGRAGlyph.Create(AIdentifier: string);
begin
  FIdentifier:= AIdentifier;
end;

{ TBGRACustomTypeWriter }

function TBGRACustomTypeWriter.GetGlyph(AIdentifier: string): TBGRAGlyph;
var Node: TAvgLvlTreeNode;
begin
  Node := FindGlyph(AIdentifier);
  if Node = nil then
    result := nil
  else
    result := TBGRAGlyph(Node.Data);
end;

procedure TBGRACustomTypeWriter.SetGlyph(AIdentifier: string; AValue: TBGRAGlyph);
var Node: TAvgLvlTreeNode;
begin
  if AValue.Identifier <> AIdentifier then
    raise exception.Create('Identifier mismatch');
  Node := FindGlyph(AIdentifier);
  if Node <> nil then
  begin
    if pointer(AValue) <> Node.Data then
      TBGRAGlyph(Node.Data).Free;
    Node.Data := AValue;
  end else
    FGlyphs.Add(pointer(AValue));
end;

function TBGRACustomTypeWriter.CompareGlyph(Tree: TAvgLvlTree; Data1, Data2: Pointer): integer;
begin
  result := CompareStr(TBGRAGlyph(Data1).Identifier,TBGRAGlyph(Data2).Identifier);
end;

function TBGRACustomTypeWriter.FindGlyph(AIdentifier: string): TAvgLvlTreeNode;
var Comp: integer;
  Node: TAvgLvlTreeNode;
begin
  Node:=FGlyphs.Root;
  while (Node<>nil) do begin
    Comp:=CompareStr(AIdentifier,TBGRAGlyph(Node.Data).Identifier);
    if Comp=0 then break;
    if Comp<0 then begin
      Node:=Node.Left
    end else begin
      Node:=Node.Right
    end;
  end;
  result := Node;
end;

constructor TBGRACustomTypeWriter.Create;
begin
  FGlyphs := TAvgLvlTree.CreateObjectCompare(@CompareGlyph);
  TypeWriterMatrix := AffineMatrixIdentity;
  OutlineMode:= twoFill;
end;

procedure TBGRACustomTypeWriter.DrawGlyph(ADest: TBGRACanvas2D;
  AIdentifier: string; X, Y: Single; AAlign: TBGRATypeWriterAlignment);
begin
  GlyphPath(ADest, AIdentifier, X,Y, AAlign);
  DrawLastPath(ADest);
end;

procedure TBGRACustomTypeWriter.DrawText(ADest: TBGRACanvas2D; AText: string;
  X, Y: Single; AAlign: TBGRATypeWriterAlignment);
begin
  TextPath(ADest, AText, X,Y, AAlign);
  DrawLastPath(ADest);
end;

function TBGRACustomTypeWriter.GetGlyphBox(AIdentifier: string; X, Y: Single;
  AAlign: TBGRATypeWriterAlignment): TAffineBox;
var g: TBGRAGlyph;
  m: TAffineMatrix;
begin
  g := GetGlyph(AIdentifier);
  if g = nil then result := TAffineBox.EmptyBox else
  begin
    m := GetGlyphMatrix(g,X,Y,AAlign);
    result := TAffineBox.AffineBox(m*PointF(0,0),m*PointF(g.Width,0),m*PointF(0,g.Height));
  end;
end;

function TBGRACustomTypeWriter.GetTextBox(AText: string; X, Y: Single;
  AAlign: TBGRATypeWriterAlignment): TAffineBox;
var
  m: TAffineMatrix;
  totalWidth,minY,maxY,gMinY,gMaxY: single;

  pstr: pchar;
  left,charlen: integer;
  nextchar: string;
  g: TBGRAGlyph;

begin
  if AText = '' then result := TAffineBox.EmptyBox else
  begin
    m := GetTextMatrix(AText,X,Y,AAlign);
    minY := 0;
    maxY := 0;
    totalWidth := 0;

    pstr := @AText[1];
    left := length(AText);
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
        totalWidth += g.Width;

        if AAlign in [twaLeft,twaMiddle,twaRight] then
        begin
          gMinY := -g.Height/2;
          gMaxY := g.Height/2;
        end else
        if AAlign in [twaBottomLeft,twaBottom,twaBottomRight] then
        begin
          gMinY := -g.Height;
          gMaxY := 0;
        end
        else
        begin
          gMinY := 0;
          gMaxY := g.Height;
        end;
        if gMinY < minY then minY := gMinY;
        if gMaxY > maxY then maxY := gMaxY;
      end;
    end;

    result := TAffineBox.AffineBox(m*PointF(0,minY),m*PointF(totalWidth,minY),m*PointF(0,maxY));
  end;
end;

function TBGRACustomTypeWriter.GetTextGlyphBoxes(AText: string; X, Y: Single;
  AAlign: TBGRATypeWriterAlignment): TGlyphBoxes;
var
  m: TAffineMatrix;
  gMinY,gMaxY: single;

  pstr: pchar;
  left,charlen: integer;
  nextchar: string;
  g: TBGRAGlyph;
  numChar: integer;

begin
  if AText = '' then result := nil else
  begin
    setlength(result, UTF8Length(AText));

    m := GetTextMatrix(AText,X,Y,AAlign);

    pstr := @AText[1];
    left := length(AText);
    numChar := 0;
    while left > 0 do
    begin
      charlen := UTF8CharacterLength(pstr);
      setlength(nextchar, charlen);
      move(pstr^, nextchar[1], charlen);
      inc(pstr,charlen);
      dec(left,charlen);

      result[numChar].Glyph := nextchar;
      g := GetGlyph(nextchar);
      if g <> nil then
      begin
        if AAlign in [twaLeft,twaMiddle,twaRight] then
        begin
          gMinY := -g.Height/2;
          gMaxY := g.Height/2;
        end else
        if AAlign in [twaBottomLeft,twaBottom,twaBottomRight] then
        begin
          gMinY := -g.Height;
          gMaxY := 0;
        end
        else
        begin
          gMinY := 0;
          gMaxY := g.Height;
        end;
        result[numChar].Box := TAffineBox.AffineBox(m*PointF(0,gMinY),m*PointF(g.Width,gMinY),m*PointF(0,gMaxY));
        m := m*AffineMatrixTranslation(g.Width,0);
      end else
        result[numChar].Box := TAffineBox.EmptyBox;

      inc(numChar);
    end;
  end;
end;

procedure TBGRACustomTypeWriter.TextPath(ADest: TBGRACanvas2D; AText: string; X,
  Y: Single; AAlign: TBGRATypeWriterAlignment);
var
  pstr: pchar;
  left,charlen: integer;
  nextchar: string;
  g: TBGRAGlyph;
  m,m2: TAffineMatrix;
begin
  ADest.beginPath;
  if AText = '' then exit;
  m := GetTextMatrix(AText, X,Y,AAlign);
  m2 := m;

  pstr := @AText[1];
  left := length(AText);
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
      if AAlign in [twaLeft,twaMiddle,twaRight] then
        m2 := m*AffineMatrixTranslation(0,-g.Height/2) else
      if AAlign in [twaBottomLeft,twaBottom,twaBottomRight] then
        m2 := m*AffineMatrixTranslation(0,-g.Height)
      else
        m2 := m;
      g.Path(ADest, m2);
      m := m*AffineMatrixTranslation(g.Width,0);
    end;
  end;
end;

procedure TBGRACustomTypeWriter.GlyphPath(ADest: TBGRACanvas2D; AIdentifier: string;
  X, Y: Single; AAlign: TBGRATypeWriterAlignment);
var g: TBGRAGlyph;
begin
  ADest.beginPath;
  g := GetGlyph(AIdentifier);
  if g = nil then exit;
  g.Path(ADest, GetGlyphMatrix(g,X,Y,AAlign));
end;

procedure TBGRACustomTypeWriter.DrawLastPath(ADest: TBGRACanvas2D);
begin
  case OutlineMode of
  twoPath: ;
  twoFill: ADest.fill;
  twoStroke: ADest.stroke;
  twoFillOverStroke: ADest.fillOverStroke;
  twoStrokeOverFill: ADest.strokeOverFill;
  twoFillThenStroke: begin ADest.fill; ADest.stroke; end;
  twoStrokeThenFill: begin ADest.stroke; ADest.fill; end;
  end;
end;

procedure TBGRACustomTypeWriter.ClearGlyphs;
begin
  FGlyphs.FreeAndClear;
end;

procedure TBGRACustomTypeWriter.RemoveGlyph(AIdentifier: string);
var Node: TAvgLvlTreeNode;
begin
  Node := FindGlyph(AIdentifier);
  if Node <> nil then FGlyphs.Delete(Node);
end;

procedure TBGRACustomTypeWriter.AddGlyph(AGlyph: TBGRAGlyph);
begin
  Glyph[AGlyph.Identifier] := AGlyph;
end;

function TBGRACustomTypeWriter.GetGlyphMatrix(AGlyph: TBGRAGlyph; X, Y: Single;
  AAlign: TBGRATypeWriterAlignment): TAffineMatrix;
var tGlyph: TPointF;
begin
  if AGlyph = nil then
  begin
    result := AffineMatrixIdentity;
    exit;
  end;
  tGlyph := PointF(0,0);
  if AAlign in [twaTop,twaMiddle,twaBottom] then tGlyph.X -= AGlyph.Width/2;
  if AAlign in [twaTopRight,twaRight,twaBottomRight] then tGlyph.X -= AGlyph.Width;
  if AAlign in [twaLeft,twaMiddle,twaRight] then tGlyph.Y -= AGlyph.Height/2;
  if AAlign in [twaBottomLeft,twaBottom,twaBottomRight] then tGlyph.Y -= AGlyph.Height;
  result := AffineMatrixTranslation(X,Y)*TypeWriterMatrix*AffineMatrixTranslation(tGlyph.X,tGlyph.Y);
end;

function TBGRACustomTypeWriter.GetTextMatrix(AText: string; X, Y: Single;
  AAlign: TBGRATypeWriterAlignment): TAffineMatrix;
var
  tGlyph: TPointF;
  totalWidth: single;
  pstr: pchar;
  left,charlen: integer;
  nextchar: string;
  g: TBGRAGlyph;
begin
  tGlyph := PointF(0,0);
  if not (AAlign in [twaLeft,twaTopLeft,twaBottomLeft]) then
  begin
    totalWidth := 0;
    pstr := @AText[1];
    left := length(AText);
    while left > 0 do
    begin
      charlen := UTF8CharacterLength(pstr);
      setlength(nextchar, charlen);
      move(pstr^, nextchar[1], charlen);
      inc(pstr,charlen);
      dec(left,charlen);

      g := GetGlyph(nextchar);
      if g <> nil then totalWidth += g.Width;
    end;

    if AAlign in[twaTop,twaMiddle,twaBottom] then tGlyph.X -= totalWidth/2 else
    if AAlign in[twaTopRight, twaRight, twaBottomRight] then tGlyph.X -= totalWidth;
  end;
  result := AffineMatrixTranslation(X,Y)*TypeWriterMatrix*AffineMatrixTranslation(tGlyph.X,tGlyph.Y);
end;

destructor TBGRACustomTypeWriter.Destroy;
begin
  FGlyphs.FreeAndClear;
  FGlyphs.Free;
  inherited Destroy;
end;

end.

