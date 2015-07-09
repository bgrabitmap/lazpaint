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
    procedure WriteHeader(AStream: TStream; AName: string; AContentSize: longint);
    class procedure ReadHeader(AStream: TStream; out AName: string; out AContentSize: longint);
    function ContentSize: integer; virtual;
    function HeaderName: string; virtual;
    procedure WriteContent(AStream: TStream); virtual;
    procedure ReadContent(AStream: TStream); virtual;
  public
    Width,Height: single;
    constructor Create(AIdentifier: string); virtual;
    constructor Create(AStream: TStream); virtual;
    procedure Path({%H-}ADest: IBGRAPath; {%H-}AMatrix: TAffineMatrix); virtual;
    property Identifier: string read FIdentifier;
    procedure SaveToStream(AStream: TStream);
    class function LoadFromStream(AStream: TStream): TBGRAGlyph;
  end;

  TGlyphPointCurveMode= (cmAuto, cmCurve, cmAngle);

  { TBGRAPolygonalGlyph }

  TBGRAPolygonalGlyph = class(TBGRAGlyph)
  private
    procedure SetQuadraticCurves(AValue: boolean);
  protected
    FQuadraticCurves: boolean;
    Points: array of TPointF;
    CurveMode: array of TGlyphPointCurveMode;
    Curves: array of record
      isCurvedToNext,isCurvedToPrevious: boolean;
      Center,ControlPoint,NextCenter: TPointF;
    end;
    function MaybeCurve(start1,end1,start2,end2: integer): boolean;
    procedure ComputeQuadraticCurves;
    function ContentSize: integer; override;
    function HeaderName: string; override;
    procedure WriteContent(AStream: TStream); override;
    procedure ReadContent(AStream: TStream); override;
    procedure Init;
  public
    Offset: TPointF;
    Closed: boolean;
    MinimumDotProduct: single;
    constructor Create(AIdentifier: string); override;
    constructor Create(AStream: TStream); override;
    procedure SetPoints(const APoints: array of TPointF); overload;
    procedure SetPoints(const APoints: array of TPointF; const ACurveMode: array of TGlyphPointCurveMode); overload;
    procedure Path(ADest: IBGRAPath; AMatrix: TAffineMatrix); override;
    property QuadraticCurves: boolean read FQuadraticCurves write SetQuadraticCurves;
  end;

  TBGRACustomTypeWriterHeader = record
    HeaderName: String;
    NbGlyphs: integer;
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
    procedure TextPath(ADest: TBGRACanvas2D; ATextUTF8: string; X,Y: Single; AAlign: TBGRATypeWriterAlignment; ADrawEachChar: boolean);
    procedure GlyphPath(ADest: TBGRACanvas2D; AIdentifier: string; X,Y: Single; AAlign: TBGRATypeWriterAlignment = twaTopLeft);
    procedure DrawLastPath(ADest: TBGRACanvas2D);
    procedure ClearGlyphs;
    procedure RemoveGlyph(AIdentifier: string);
    procedure AddGlyph(AGlyph: TBGRAGlyph);
    function GetGlyphMatrix(AGlyph: TBGRAGlyph; X,Y: Single; AAlign: TBGRATypeWriterAlignment): TAffineMatrix;
    function GetTextMatrix(ATextUTF8: string; X,Y: Single; AAlign: TBGRATypeWriterAlignment): TAffineMatrix;
    property Glyph[AIdentifier: string]: TBGRAGlyph read GetGlyph write SetGlyph;
    function CustomHeaderSize: integer; virtual;
    procedure WriteCustomHeader(AStream: TStream); virtual;
    function ReadCustomTypeWriterHeader(AStream: TStream): TBGRACustomTypeWriterHeader;
    procedure ReadAdditionalHeader({%H-}AStream: TStream); virtual;
    function HeaderName: string; virtual;
  public
    OutlineMode: TBGRATypeWriterOutlineMode;
    DrawGlyphsSimultaneously : boolean;
    constructor Create;
    procedure SaveGlyphsToFile(AFilenameUTF8: string);
    procedure SaveGlyphsToStream(AStream: TStream);
    procedure LoadGlyphsFromFile(AFilenameUTF8: string);
    procedure LoadGlyphsFromStream(AStream: TStream);
    procedure DrawGlyph(ADest: TBGRACanvas2D; AIdentifier: string; X,Y: Single; AAlign: TBGRATypeWriterAlignment = twaTopLeft);
    procedure DrawText(ADest: TBGRACanvas2D; ATextUTF8: string; X,Y: Single; AAlign: TBGRATypeWriterAlignment = twaTopLeft); virtual;
    procedure CopyTextPathTo(ADest: IBGRAPath; ATextUTF8: string; X,Y: Single; AAlign: TBGRATypeWriterAlignment = twaTopLeft); virtual;
    function GetGlyphBox(AIdentifier: string; X,Y: Single; AAlign: TBGRATypeWriterAlignment = twaTopLeft): TAffineBox;
    function GetTextBox(ATextUTF8: string; X,Y: Single; AAlign: TBGRATypeWriterAlignment = twaTopLeft): TAffineBox;
    function GetTextGlyphBoxes(ATextUTF8: string; X,Y: Single; AAlign: TBGRATypeWriterAlignment = twaTopLeft): TGlyphBoxes;
    procedure NeedGlyphRange(AUnicodeFrom, AUnicodeTo: Cardinal);
    procedure NeedGlyphAnsiRange;
    destructor Destroy; override;
  end;

function ComputeEasyBezier(APoints: array of TPointF; AClosed: boolean; AMinimumDotProduct: single = 0.707): ArrayOfTPointF; overload;
function ComputeEasyBezier(APoints: array of TPointF; ACurveMode: array of TGlyphPointCurveMode; AClosed: boolean; AMinimumDotProduct: single = 0.707): ArrayOfTPointF; overload;

implementation

uses BGRAUTF8;

procedure LEWritePointF(Stream: TStream; AValue: TPointF);
begin
  LEWriteSingle(Stream,AValue.x);
  LEWriteSingle(Stream,AValue.y);
end;

function LEReadPointF(Stream: TStream): TPointF;
begin
  result.x := LEReadSingle(Stream);
  result.y := LEReadSingle(Stream);
end;

function ComputeEasyBezier(APoints: array of TPointF; AClosed: boolean; AMinimumDotProduct: single = 0.707): ArrayOfTPointF;
var
  glyph: TBGRAPolygonalGlyph;
  canvas2D: TBGRACanvas2D;
  i: integer;
begin
  if length(APoints) <= 2 then
  begin
    setlength(result, length(APoints));
    for i := 0 to high(result) do
      result[i] := APoints[i];
    exit;
  end;
  glyph := TBGRAPolygonalGlyph.Create('');
  glyph.QuadraticCurves := true;
  glyph.Closed:= AClosed;
  glyph.MinimumDotProduct := AMinimumDotProduct;
  glyph.SetPoints(APoints);
  canvas2D := TBGRACanvas2D.Create(nil);
  canvas2D.pixelCenteredCoordinates := true;
  glyph.Path(canvas2D,AffineMatrixIdentity);
  glyph.Free;
  result := canvas2D.currentPath;
  canvas2D.free;
end;

function ComputeEasyBezier(APoints: array of TPointF;
  ACurveMode: array of TGlyphPointCurveMode; AClosed: boolean;
  AMinimumDotProduct: single): ArrayOfTPointF;
var
  glyph: TBGRAPolygonalGlyph;
  canvas2D: TBGRACanvas2D;
  i: integer;
begin
  if length(APoints) <= 2 then
  begin
    setlength(result, length(APoints));
    for i := 0 to high(result) do
      result[i] := APoints[i];
    exit;
  end;
  glyph := TBGRAPolygonalGlyph.Create('');
  glyph.QuadraticCurves := true;
  glyph.Closed:= AClosed;
  glyph.MinimumDotProduct := AMinimumDotProduct;
  glyph.SetPoints(APoints, ACurveMode);
  canvas2D := TBGRACanvas2D.Create(nil);
  canvas2D.pixelCenteredCoordinates := true;
  glyph.Path(canvas2D,AffineMatrixIdentity);
  glyph.Free;
  result := canvas2D.currentPath;
  canvas2D.free;
end;

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

  result := u*v > MinimumDotProduct;
end;

procedure TBGRAPolygonalGlyph.ComputeQuadraticCurves;
var
  i,FirstPointIndex,NextPt,NextPt2: integer;
begin
  setlength(Curves, length(points));
  FirstPointIndex := 0;
  for i := 0 to high(points) do
    Curves[i].isCurvedToPrevious := false;
  for i := 0 to high(points) do
  begin
    Curves[i].isCurvedToNext := false;
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
      Curves[i].ControlPoint := points[NextPt];

      if (i < high(points)-1) or Closed then
      begin
        case CurveMode[nextPt] of
          cmAuto: Curves[i].isCurvedToNext:= MaybeCurve(i,NextPt,NextPt,NextPt2);
          cmCurve: Curves[i].isCurvedToNext:= true;
          else Curves[i].isCurvedToNext:= false;
        end;
        Curves[NextPt].isCurvedToPrevious := Curves[i].isCurvedToNext;
      end;
    end;
  end;
end;

function TBGRAPolygonalGlyph.ContentSize: integer;
begin
  Result:= (inherited ContentSize) + sizeof(single)*2 + 4 + sizeof(single)*2*length(Points);
end;

function TBGRAPolygonalGlyph.HeaderName: string;
begin
  Result:='TBGRAPolygonalGlyph';
end;

procedure TBGRAPolygonalGlyph.WriteContent(AStream: TStream);
var i: integer;
begin
  inherited WriteContent(AStream);
  LEWritePointF(AStream, Offset);
  LEWriteLongint(AStream,length(Points));
  for i := 0 to high(Points) do
    LEWritePointF(AStream, Points[i]);
end;

procedure TBGRAPolygonalGlyph.ReadContent(AStream: TStream);
var i: integer;
  tempPts: array of TPointF;
begin
  inherited ReadContent(AStream);
  Offset := LEReadPointF(AStream);
  SetLength(tempPts, LEReadLongint(AStream));
  for i := 0 to high(tempPts) do
    tempPts[i] := LEReadPointF(AStream);
  SetPoints(tempPts);
end;

procedure TBGRAPolygonalGlyph.Init;
begin
  Closed := True;
  MinimumDotProduct := 0.707;
end;

constructor TBGRAPolygonalGlyph.Create(AIdentifier: string);
begin
  inherited Create(AIdentifier);
  Offset := PointF(0,0);
  Init;
end;

constructor TBGRAPolygonalGlyph.Create(AStream: TStream);
begin
  inherited Create(AStream);
  Init;
end;

procedure TBGRAPolygonalGlyph.SetPoints(const APoints: array of TPointF);
var i: integer;
begin
  SetLength(Points,length(APoints));
  for i := 0 to high(points) do
    points[i] := APoints[i];
  setlength(CurveMode, length(APoints));
  for i := 0 to high(CurveMode) do
    CurveMode[i] := cmAuto;
  Curves := nil;
end;

procedure TBGRAPolygonalGlyph.SetPoints(const APoints: array of TPointF;
  const ACurveMode: array of TGlyphPointCurveMode);
var i: integer;
begin
  if length(APoints) <> length(ACurveMode) then
    raise exception.Create('Dimension mismatch');
  SetLength(Points,length(APoints));
  for i := 0 to high(points) do
    points[i] := APoints[i];
  setlength(CurveMode, length(ACurveMode));
  for i := 0 to high(CurveMode) do
    CurveMode[i] := ACurveMode[i];
  Curves := nil;
end;

procedure TBGRAPolygonalGlyph.Path(ADest: IBGRAPath; AMatrix: TAffineMatrix);
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
      begin
        ADest.lineTo(AMatrix*Points[i]);
      end;
    end;
  if not nextmove then
    ADest.closePath;
end;

{ TBGRAGlyph }

procedure TBGRAGlyph.WriteHeader(AStream: TStream; AName: string;
  AContentSize: longint);
begin
  LEWriteByte(AStream, length(AName));
  AStream.Write(AName[1],length(AName));
  LEWriteLongint(AStream, AContentSize);
end;

class procedure TBGRAGlyph.ReadHeader(AStream: TStream; out AName: string; out
  AContentSize: longint);
var NameLength: integer;
begin
  NameLength := LEReadByte(AStream);
  setlength(AName,NameLength);
  AStream.Read(AName[1],length(AName));
  AContentSize := LEReadLongint(AStream);
end;

function TBGRAGlyph.ContentSize: integer;
begin
  result := 4+length(FIdentifier)+sizeof(single)*2;
end;

function TBGRAGlyph.HeaderName: string;
begin
  result := 'TBGRAGlyph';
end;

procedure TBGRAGlyph.WriteContent(AStream: TStream);
begin
  LEWriteLongint(AStream,length(FIdentifier));
  AStream.Write(FIdentifier[1],length(FIdentifier));
  LEWriteSingle(AStream,Width);
  LEWriteSingle(AStream,Height);
end;

procedure TBGRAGlyph.ReadContent(AStream: TStream);
var lIdentifierLength: integer;
begin
  lIdentifierLength:= LEReadLongint(AStream);
  setlength(FIdentifier, lIdentifierLength);
  AStream.Read(FIdentifier[1],length(FIdentifier));
  Width := LEReadSingle(AStream);
  Height := LEReadSingle(AStream);
end;

constructor TBGRAGlyph.Create(AIdentifier: string);
begin
  FIdentifier:= AIdentifier;
end;

constructor TBGRAGlyph.Create(AStream: TStream);
begin
  ReadContent(AStream);
end;

procedure TBGRAGlyph.Path(ADest: IBGRAPath; AMatrix: TAffineMatrix);
begin
  //nothing
end;

procedure TBGRAGlyph.SaveToStream(AStream: TStream);
begin
  WriteHeader(AStream, HeaderName, ContentSize);
  WriteContent(AStream);
end;

class function TBGRAGlyph.LoadFromStream(AStream: TStream) : TBGRAGlyph;
var lName: string;
  lContentSize: integer;
  EndPosition: Int64;
begin
  ReadHeader(AStream,lName,lContentSize);
  EndPosition := AStream.Position + lContentSize;
  if lName = 'TBGRAPolygonalGlyph' then
    result := TBGRAPolygonalGlyph.Create(AStream)
  else if lName = 'TBGRAGlyph' then
    result := TBGRAGlyph.Create(AStream)
  else
    raise exception.Create('Unknown glyph type (' + lName + ')');
  AStream.Position:= EndPosition;
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
  DrawGlyphsSimultaneously := false;
end;

procedure TBGRACustomTypeWriter.DrawGlyph(ADest: TBGRACanvas2D;
  AIdentifier: string; X, Y: Single; AAlign: TBGRATypeWriterAlignment);
begin
  GlyphPath(ADest, AIdentifier, X,Y, AAlign);
  DrawLastPath(ADest);
end;

procedure TBGRACustomTypeWriter.DrawText(ADest: TBGRACanvas2D; ATextUTF8: string;
  X, Y: Single; AAlign: TBGRATypeWriterAlignment);
begin
  TextPath(ADest, ATextUTF8, X,Y, AAlign, (OutlineMode <> twoPath) and not DrawGlyphsSimultaneously);
end;

procedure TBGRACustomTypeWriter.CopyTextPathTo(ADest: IBGRAPath; ATextUTF8: string; X,Y: Single; AAlign: TBGRATypeWriterAlignment = twaTopLeft);
var
  pstr: pchar;
  left,charlen: integer;
  nextchar: string;
  g: TBGRAGlyph;
  m,m2: TAffineMatrix;
begin
  if ATextUTF8 = '' then exit;
  m := GetTextMatrix(ATextUTF8, X,Y,AAlign);
  m2 := m;

  pstr := @ATextUTF8[1];
  left := length(ATextUTF8);
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

function TBGRACustomTypeWriter.GetTextBox(ATextUTF8: string; X, Y: Single;
  AAlign: TBGRATypeWriterAlignment): TAffineBox;
var
  m: TAffineMatrix;
  totalWidth,minY,maxY,gMinY,gMaxY: single;

  pstr: pchar;
  left,charlen: integer;
  nextchar: string;
  g: TBGRAGlyph;

begin
  if ATextUTF8 = '' then result := TAffineBox.EmptyBox else
  begin
    m := GetTextMatrix(ATextUTF8,X,Y,AAlign);
    minY := 0;
    maxY := 0;
    totalWidth := 0;

    pstr := @ATextUTF8[1];
    left := length(ATextUTF8);
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

function TBGRACustomTypeWriter.GetTextGlyphBoxes(ATextUTF8: string; X, Y: Single;
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
  if ATextUTF8 = '' then result := nil else
  begin
    setlength(result, UTF8Length(ATextUTF8));

    m := GetTextMatrix(ATextUTF8,X,Y,AAlign);

    pstr := @ATextUTF8[1];
    left := length(ATextUTF8);
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

procedure TBGRACustomTypeWriter.NeedGlyphRange(AUnicodeFrom, AUnicodeTo: Cardinal);
var c: cardinal;
begin
  for c := AUnicodeFrom to AUnicodeTo do
    GetGlyph(UnicodeCharToUTF8(c));
end;

procedure TBGRACustomTypeWriter.NeedGlyphAnsiRange;
var i: integer;
begin
  for i := 0 to 255 do
    GetGlyph(AnsiToUtf8(chr(i)));
end;

procedure TBGRACustomTypeWriter.TextPath(ADest: TBGRACanvas2D; ATextUTF8: string; X,
  Y: Single; AAlign: TBGRATypeWriterAlignment; ADrawEachChar: boolean);
var
  pstr: pchar;
  left,charlen: integer;
  nextchar: string;
  g: TBGRAGlyph;
  m,m2: TAffineMatrix;
begin
  if not ADrawEachChar then ADest.beginPath;
  if ATextUTF8 = '' then exit;
  m := GetTextMatrix(ATextUTF8, X,Y,AAlign);
  m2 := m;

  pstr := @ATextUTF8[1];
  left := length(ATextUTF8);
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
      if ADrawEachChar then ADest.beginPath;
      g.Path(ADest, m2);
      if ADrawEachChar then DrawLastPath(ADest);
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

procedure TBGRACustomTypeWriter.SaveGlyphsToStream(AStream: TStream);
var Enumerator: TAvgLvlTreeNodeEnumerator;
begin
  LEWriteLongint(AStream,CustomHeaderSize);
  WriteCustomHeader(AStream);

  Enumerator := FGlyphs.GetEnumerator;
  while Enumerator.MoveNext do
    TBGRAGlyph(Enumerator.Current.Data).SaveToStream(AStream);
  Enumerator.Free;
end;

procedure TBGRACustomTypeWriter.LoadGlyphsFromFile(AFilenameUTF8: string);
var Stream: TFileStreamUTF8;
begin
  Stream := nil;
  try
    Stream := TFileStreamUTF8.Create(AFilenameUTF8, fmOpenRead);
    LoadGlyphsFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TBGRACustomTypeWriter.LoadGlyphsFromStream(AStream: TStream);
var Header: TBGRACustomTypeWriterHeader;
  i: integer;
  g: TBGRAGlyph;
  HeaderSize: integer;
  GlyphStartPosition: Int64;
begin
  HeaderSize := LEReadLongint(AStream);
  GlyphStartPosition:= AStream.Position+HeaderSize;
  Header := ReadCustomTypeWriterHeader(AStream);
  if header.HeaderName <> HeaderName then
    raise exception.Create('Invalid file format ("'+header.HeaderName+'" should be "'+HeaderName+'")');
  ReadAdditionalHeader(AStream);
  AStream.Position:= GlyphStartPosition;
  for i := 0 to Header.NbGlyphs-1 do
  begin
    g := TBGRAGlyph.LoadFromStream(AStream);
    AddGlyph(g);
  end;
end;

procedure TBGRACustomTypeWriter.SaveGlyphsToFile(AFilenameUTF8: string);
var Stream: TFileStreamUTF8;
begin
  Stream := nil;
  try
    Stream := TFileStreamUTF8.Create(AFilenameUTF8, fmCreate or fmOpenWrite);
    SaveGlyphsToStream(Stream);
  finally
    Stream.Free;
  end;
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

function TBGRACustomTypeWriter.GetTextMatrix(ATextUTF8: string; X, Y: Single;
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
    pstr := @ATextUTF8[1];
    left := length(ATextUTF8);
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

function TBGRACustomTypeWriter.CustomHeaderSize: integer;
begin
  result := 1+length(HeaderName)+4;
end;

procedure TBGRACustomTypeWriter.WriteCustomHeader(AStream: TStream);
var lHeaderName: string;
begin
  lHeaderName:= HeaderName;
  LEWriteByte(AStream,length(lHeaderName));
  AStream.Write(lHeaderName[1],length(lHeaderName));
  LEWriteLongint(AStream,FGlyphs.Count);
end;

function TBGRACustomTypeWriter.ReadCustomTypeWriterHeader(AStream: TStream
  ): TBGRACustomTypeWriterHeader;
begin
  setlength(result.HeaderName, LEReadByte(AStream));
  AStream.Read(result.HeaderName[1],length(result.HeaderName));
  result.NbGlyphs:= LEReadLongint(AStream);
end;

procedure TBGRACustomTypeWriter.ReadAdditionalHeader(AStream: TStream);
begin
  //nothing
end;

function TBGRACustomTypeWriter.HeaderName: string;
begin
  result := 'TBGRACustomTypeWriter';
end;

destructor TBGRACustomTypeWriter.Destroy;
begin
  FGlyphs.FreeAndClear;
  FGlyphs.Free;
  inherited Destroy;
end;

end.

