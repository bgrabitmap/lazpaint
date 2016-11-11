unit BGRAPen;

{$mode objfpc}{$H+}

interface

{ This unit handles pen style and width, as well as line caps and join styles.

  A line consists in two points.
  A polyline consists in one or more lines, defined by two points or more than two points
  A poly-polyline consists in a series of polylines, defined by polyline points separated by empty points (see EmptyPointF) }

uses
  SysUtils, BGRAGraphics, BGRABitmapTypes, BGRATransform;

var   //predefined pen styles
  SolidPenStyle, DashPenStyle, DotPenStyle, DashDotPenStyle, DashDotDotPenStyle, ClearPenStyle: TBGRAPenStyle;

type

  { TBGRAPenStroker }

  TBGRAPenStroker = class(TBGRACustomPenStroker)
    protected
      { Pen style can be defined by PenStyle property of by CustomPenStyle property.
      When PenStyle property is assigned, CustomPenStyle property is assigned the actual
      pen pattern. }
      FCustomPenStyle: TBGRAPenStyle;
      FPenStyle: TPenStyle;
      FArrow: TBGRACustomArrow;
      FArrowOwned: boolean;
      FOriginalStrokeMatrix,FStrokeMatrix,FStrokeMatrixInverse: TAffineMatrix;
      FStrokeZoom: single;
      FStrokeMatrixIdentity: boolean;
      FLineCap: TPenEndCap;
      FJoinStyle: TPenJoinStyle;
      FMiterLimit: single;

      function GetArrow: TBGRACustomArrow; override;
      function GetArrowOwned: boolean; override;
      function GetCustomPenStyle: TBGRAPenStyle; override;
      function GetJoinStyle: TPenJoinStyle; override;
      function GetLineCap: TPenEndCap; override;
      function GetMiterLimit: single; override;
      function GetPenStyle: TPenStyle; override;
      function GetStrokeMatrix: TAffineMatrix; override;
      procedure SetArrow(AValue: TBGRACustomArrow); override;
      procedure SetArrowOwned(AValue: boolean); override;
      procedure SetCustomPenStyle(AValue: TBGRAPenStyle); override;
      procedure SetJoinStyle(AValue: TPenJoinStyle); override;
      procedure SetLineCap(AValue: TPenEndCap); override;
      procedure SetMiterLimit(AValue: single); override;
      procedure SetPenStyle(AValue: TPenStyle); override;
      procedure SetStrokeMatrix(const AValue: TAffineMatrix); override;
    public
      constructor Create;
      destructor Destroy; override;
      function ComputePolyline(const APoints: array of TPointF; AWidth: single; AClosedCap: boolean = true): ArrayOfTPointF; override;
      function ComputePolyline(const APoints: array of TPointF; AWidth: single; APenColor: TBGRAPixel; AClosedCap: boolean = true): ArrayOfTPointF; override;
      function ComputePolylineAutocycle(const APoints: array of TPointF; AWidth: single): ArrayOfTPointF; override;
      function ComputePolygon(const APoints: array of TPointF; AWidth: single): ArrayOfTPointF; override;

  end;

  TBGRAPolyLineOption = (plRoundCapOpen, //specifies that the line ending is opened
                         plCycle,        //specifies that it is a polygon
                         plAutoCycle,    //specifies that a cycle must be used if the last point is the first point
                         plNoStartCap,
                         plNoEndCap);
  TBGRAPolyLineOptions = set of TBGRAPolyLineOption;
  TComputeArrowHeadProc = function(const APosition: TPointF; const ADirection: TPointF; const AWidth: single; const ACurrentPos: single): ArrayOfTPointF of object;

{ Compute the path for a polyline }
function ComputeWidePolylinePoints(const linepts: array of TPointF; width: single;
          pencolor: TBGRAPixel; linecap: TPenEndCap; joinstyle: TPenJoinStyle; const penstyle: TBGRAPenStyle;
          options: TBGRAPolyLineOptions; miterLimit: single = 2; arrow: TBGRACustomArrow = nil): ArrayOfTPointF;

{ Compute the path for a poly-polyline }
function ComputeWidePolyPolylinePoints(const linepts: array of TPointF; width: single;
          pencolor: TBGRAPixel; linecap: TPenEndCap; joinstyle: TPenJoinStyle; const penstyle: TBGRAPenStyle;
          options: TBGRAPolyLineOptions; miterLimit: single = 2; arrow: TBGRACustomArrow = nil): ArrayOfTPointF;

{--------------------- Pixel line procedures --------------------------}
{ These procedures take integer coordinates as parameters and do not handle pen styles and width.
  They are faster and can be useful for drawing a simple frame }

//aliased version
procedure BGRADrawLineAliased(dest: TBGRACustomBitmap; x1, y1, x2, y2: integer; c: TBGRAPixel; DrawLastPixel: boolean; ADrawMode: TDrawMode = dmDrawWithTransparency);
procedure BGRAEraseLineAliased(dest: TBGRACustomBitmap; x1, y1, x2, y2: integer; alpha: byte; DrawLastPixel: boolean);

//antialiased version
procedure BGRADrawLineAntialias({%H-}dest: TBGRACustomBitmap; x1, y1, x2, y2: integer;
  c: TBGRAPixel; DrawLastPixel: boolean; LinearBlend : boolean = false);
procedure BGRAEraseLineAntialias(dest: TBGRACustomBitmap; x1, y1, x2, y2: integer;
  calpha: byte; DrawLastPixel: boolean);

//antialiased version with bicolor dashes (to draw a frame)
procedure BGRADrawLineAntialias(dest: TBGRACustomBitmap; x1, y1, x2, y2: integer;
  c1, c2: TBGRAPixel; dashLen: integer; DrawLastPixel: boolean; var DashPos: integer; LinearBlend : boolean = false);

//length added to ensure accepable alpha join (using TBGRAMultishapeFiller is still better)
function GetAlphaJoinFactor(alpha: byte): single;

//create standard brush texture
function CreateBrushTexture(prototype: TBGRACustomBitmap; brushstyle: TBrushStyle; PatternColor, BackgroundColor: TBGRAPixel;
    width: integer = 8; height: integer = 8; penwidth: single = 1): TBGRACustomBitmap;

//check special pen styles
function IsSolidPenStyle(ACustomPenStyle: TBGRAPenStyle): boolean;
function IsClearPenStyle(ACustomPenStyle: TBGRAPenStyle): boolean;
function DuplicatePenStyle(ACustomPenStyle: array of single): TBGRAPenStyle;

implementation

uses math, BGRAPath;

procedure BGRADrawLineAliased(dest: TBGRACustomBitmap; x1, y1, x2, y2: integer;
  c: TBGRAPixel; DrawLastPixel: boolean; ADrawMode: TDrawMode);
var
  Y, X: integer;
  DX, DY, SX, SY, E: integer;
  PixelProc: procedure (x, y: int32or64; c: TBGRAPixel) of object;
begin
  if (Y1 = Y2) then
  begin
    if (X1 = X2) then
    begin
      if DrawLastPixel then
        dest.DrawPixel(X1, Y1, c, ADrawMode);
    end else
    begin
      if not DrawLastPixel then
      begin
        if X2 > X1 then dec(X2) else inc(X2);
      end;
      dest.HorizLine(X1,Y1,X2,c, ADrawMode);
    end;
    Exit;
  end else
  if (X1 = X2) then
  begin
    if not DrawLastPixel then
    begin
      if Y2 > Y1 then dec(Y2) else inc(Y2);
    end;
    dest.VertLine(X1,Y1,Y2,c, ADrawMode);
	Exit;
  end;

  DX := X2 - X1;
  DY := Y2 - Y1;

  if (ADrawMode = dmSetExceptTransparent) and (c.alpha <> 255) then exit else
  if c.alpha = 0 then
  begin
    if ADrawMode in[dmDrawWithTransparency,dmLinearBlend] then exit;
    if (ADrawMode = dmXor) and (DWord(c)=0) then exit;
  end;
  case ADrawMode of
  dmDrawWithTransparency: PixelProc := @dest.DrawPixel;
  dmXor: PixelProc := @dest.XorPixel;
  dmLinearBlend: PixelProc := @dest.FastBlendPixel;
  else
    PixelProc := @dest.SetPixel;
  end;

  if DX < 0 then
  begin
    SX := -1;
    DX := -DX;
  end
  else
    SX := 1;

  if DY < 0 then
  begin
    SY := -1;
    DY := -DY;
  end
  else
    SY := 1;

  DX := DX shl 1;
  DY := DY shl 1;

  X := X1;
  Y := Y1;
  if DX > DY then
  begin
    E := DY - DX shr 1;

    while X <> X2 do
    begin
      PixelProc(X, Y, c);
      if E >= 0 then
      begin
        Inc(Y, SY);
        Dec(E, DX);
      end;
      Inc(X, SX);
      Inc(E, DY);
    end;
  end
  else
  begin
    E := DX - DY shr 1;

    while Y <> Y2 do
    begin
      PixelProc(X, Y, c);
      if E >= 0 then
      begin
        Inc(X, SX);
        Dec(E, DY);
      end;
      Inc(Y, SY);
      Inc(E, DX);
    end;
  end;

  if DrawLastPixel then
    PixelProc(X2, Y2, c);
end;

procedure BGRAEraseLineAliased(dest: TBGRACustomBitmap; x1, y1, x2,
  y2: integer; alpha: byte; DrawLastPixel: boolean);
var
  Y, X: integer;
  DX, DY, SX, SY, E: integer;
begin

  if (Y1 = Y2) and (X1 = X2) then
  begin
    if DrawLastPixel then
      dest.ErasePixel(X1, Y1, alpha);
    Exit;
  end;

  DX := X2 - X1;
  DY := Y2 - Y1;

  if DX < 0 then
  begin
    SX := -1;
    DX := -DX;
  end
  else
    SX := 1;

  if DY < 0 then
  begin
    SY := -1;
    DY := -DY;
  end
  else
    SY := 1;

  DX := DX shl 1;
  DY := DY shl 1;

  X := X1;
  Y := Y1;
  if DX > DY then
  begin
    E := DY - DX shr 1;

    while X <> X2 do
    begin
      dest.ErasePixel(X, Y, alpha);
      if E >= 0 then
      begin
        Inc(Y, SY);
        Dec(E, DX);
      end;
      Inc(X, SX);
      Inc(E, DY);
    end;
  end
  else
  begin
    E := DX - DY shr 1;

    while Y <> Y2 do
    begin
      dest.ErasePixel(X, Y, alpha);
      if E >= 0 then
      begin
        Inc(X, SX);
        Dec(E, DY);
      end;
      Inc(Y, SY);
      Inc(E, DX);
    end;
  end;

  if DrawLastPixel then
    dest.ErasePixel(X2, Y2, alpha);
end;

procedure BGRADrawLineAntialias(dest: TBGRACustomBitmap; x1, y1, x2, y2: integer;
  c: TBGRAPixel; DrawLastPixel: boolean; LinearBlend : boolean);
var
  Y, X:  integer;
  DX, DY, SX, SY, E: integer;
  alpha: NativeUInt;
  pixelproc: procedure(x,y: int32or64; c: TBGRAPixel) of object;
begin
  if LinearBlend then
    pixelproc := @dest.FastBlendPixel
  else
    pixelproc := @dest.DrawPixel;

  if (Y1 = Y2) and (X1 = X2) then
  begin
    if DrawLastPixel then
      pixelproc(X1, Y1, c);
    Exit;
  end;

  DX := X2 - X1;
  DY := Y2 - Y1;

  if DX < 0 then
  begin
    SX := -1;
    DX := -DX;
  end
  else
    SX := 1;

  if DY < 0 then
  begin
    SY := -1;
    DY := -DY;
  end
  else
    SY := 1;

  DX := DX shl 1;
  DY := DY shl 1;

  X := X1;
  Y := Y1;

  if DX > DY then
  begin
    E := 0;

    while X <> X2 do
    begin
      alpha := c.alpha * E div DX;
      pixelproc(X, Y, BGRA(c.red, c.green, c.blue, c.alpha - alpha));
      pixelproc(X, Y + SY, BGRA(c.red, c.green, c.blue, alpha));
      Inc(E, DY);
      if E >= DX then
      begin
        Inc(Y, SY);
        Dec(E, DX);
      end;
      Inc(X, SX);
    end;
  end
  else
  begin
    E := 0;

    while Y <> Y2 do
    begin
      alpha := c.alpha * E div DY;
      pixelproc(X, Y, BGRA(c.red, c.green, c.blue, c.alpha - alpha));
      pixelproc(X + SX, Y, BGRA(c.red, c.green, c.blue, alpha));
      Inc(E, DX);
      if E >= DY then
      begin
        Inc(X, SX);
        Dec(E, DY);
      end;
      Inc(Y, SY);
    end;
  end;
  if DrawLastPixel then
    pixelproc(X2, Y2, c);
end;

procedure BGRAEraseLineAntialias(dest: TBGRACustomBitmap; x1, y1, x2,
  y2: integer; calpha: byte; DrawLastPixel: boolean);
var
  Y, X:  integer;
  DX, DY, SX, SY, E: integer;
  alpha: NativeUInt;
begin

  if (Y1 = Y2) and (X1 = X2) then
  begin
    if DrawLastPixel then
      dest.ErasePixel(X1, Y1, calpha);
    Exit;
  end;

  DX := X2 - X1;
  DY := Y2 - Y1;

  if DX < 0 then
  begin
    SX := -1;
    DX := -DX;
  end
  else
    SX := 1;

  if DY < 0 then
  begin
    SY := -1;
    DY := -DY;
  end
  else
    SY := 1;

  DX := DX shl 1;
  DY := DY shl 1;

  X := X1;
  Y := Y1;

  if DX > DY then
  begin
    E := 0;

    while X <> X2 do
    begin
      alpha := calpha * E div DX;
      dest.ErasePixel(X, Y, calpha - alpha);
      dest.ErasePixel(X, Y + SY, alpha);
      Inc(E, DY);
      if E >= DX then
      begin
        Inc(Y, SY);
        Dec(E, DX);
      end;
      Inc(X, SX);
    end;
  end
  else
  begin
    E := 0;

    while Y <> Y2 do
    begin
      alpha := calpha * E div DY;
      dest.ErasePixel(X, Y, calpha - alpha);
      dest.ErasePixel(X + SX, Y, alpha);
      Inc(E, DX);
      if E >= DY then
      begin
        Inc(X, SX);
        Dec(E, DY);
      end;
      Inc(Y, SY);
    end;
  end;
  if DrawLastPixel then
    dest.ErasePixel(X2, Y2, calpha);
end;

procedure BGRADrawLineAntialias(dest: TBGRACustomBitmap; x1, y1, x2, y2: integer;
  c1, c2: TBGRAPixel; dashLen: integer; DrawLastPixel: boolean; var DashPos: integer; LinearBlend : boolean);
var
  Y, X:  integer;
  DX, DY, SX, SY, E: integer;
  alpha: NativeUInt;
  c:     TBGRAPixel;
begin
  if (c1.alpha=0) and (c2.alpha=0) then exit;
  if DashLen <= 0 then
  begin
    BGRADrawLineAntialias(dest,x1,y1,x2,y2,MergeBGRA(c1,c2),DrawLastPixel,LinearBlend);
    exit;
  end;

  DashPos := PositiveMod(DashPos,DashLen+DashLen);
  if DashPos < DashLen then c := c1 else c := c2;

  if (Y1 = Y2) and (X1 = X2) then
  begin
    if DrawLastPixel then
      dest.DrawPixel(X1, Y1, c);
    Exit;
  end;

  DX := X2 - X1;
  DY := Y2 - Y1;

  if DX < 0 then
  begin
    SX := -1;
    DX := -DX;
  end
  else
    SX := 1;

  if DY < 0 then
  begin
    SY := -1;
    DY := -DY;
  end
  else
    SY := 1;

  DX := DX shl 1;
  DY := DY shl 1;

  X := X1;
  Y := Y1;

  if DX > DY then
  begin
    E := 0;

    while X <> X2 do
    begin
      alpha := c.alpha * E div DX;
      dest.DrawPixel(X, Y, BGRA(c.red, c.green, c.blue, c.alpha - alpha));
      dest.DrawPixel(X, Y + SY, BGRA(c.red, c.green, c.blue, alpha));
      Inc(E, DY);
      if E >= DX then
      begin
        Inc(Y, SY);
        Dec(E, DX);
      end;
      Inc(X, SX);

      Inc(DashPos);
      if DashPos = DashLen then
        c := c2
      else
      if DashPos = DashLen + DashLen then
      begin
        c := c1;
        DashPos := 0;
      end;
    end;
  end
  else
  begin
    E := 0;

    while Y <> Y2 do
    begin
      alpha := c.alpha * E div DY;
      dest.DrawPixel(X, Y, BGRA(c.red, c.green, c.blue, c.alpha - alpha));
      dest.DrawPixel(X + SX, Y, BGRA(c.red, c.green, c.blue, alpha));
      Inc(E, DX);
      if E >= DY then
      begin
        Inc(X, SX);
        Dec(E, DY);
      end;
      Inc(Y, SY);

      Inc(DashPos);
      if DashPos = DashLen then
        c := c2
      else
      if DashPos = DashLen + DashLen then
      begin
        c := c1;
        DashPos := 0;
      end;
    end;
  end;
  if DrawLastPixel then
  begin
    dest.DrawPixel(X2, Y2, c);
    inc(DashPos);
    if DashPos = DashLen + DashLen then DashPos := 0;
  end;
end;

function GetAlphaJoinFactor(alpha: byte): single;
var t: single;
begin
  if alpha = 255 then result := 1 else
  begin
    result := (power(20,alpha/255)-1)/19*0.5;
    t := power(alpha/255,40);
    result := result*(1-t)+t*0.82;
  end;
end;

function CreateBrushTexture(prototype: TBGRACustomBitmap; brushstyle: TBrushStyle;
  PatternColor, BackgroundColor: TBGRAPixel; width: integer = 8; height: integer = 8; penwidth: single = 1): TBGRACustomBitmap;
begin
  result := prototype.NewBitmap(width,height);
  if brushstyle <> bsClear then
  begin
    result.Fill(BackgroundColor);
    if brushstyle in[bsDiagCross,bsBDiagonal] then
    begin
      result.DrawLineAntialias(-1,height,width,-1,PatternColor,penwidth);
      result.DrawLineAntialias(-1-penwidth,0+penwidth,0+penwidth,-1-penwidth,PatternColor,penwidth);
      result.DrawLineAntialias(width-1-penwidth,height+penwidth,width+penwidth,height-1-penwidth,PatternColor,penwidth);
    end;
    if brushstyle in[bsDiagCross,bsFDiagonal] then
    begin
      result.DrawLineAntialias(-1,-1,width,height,PatternColor,penwidth);
      result.DrawLineAntialias(width-1-penwidth,-1-penwidth,width+penwidth,0+penwidth,PatternColor,penwidth);
      result.DrawLineAntialias(-1-penwidth,height-1-penwidth,0+penwidth,height+penwidth,PatternColor,penwidth);
    end;
    if brushstyle in[bsHorizontal,bsCross] then
      result.DrawLineAntialias(-1,height div 2,width,height div 2,PatternColor,penwidth);
    if brushstyle in[bsVertical,bsCross] then
      result.DrawLineAntialias(width div 2,-1,width div 2,height,PatternColor,penwidth);
  end;
end;

function IsSolidPenStyle(ACustomPenStyle: TBGRAPenStyle): boolean;
begin
  result := ACustomPenStyle = nil;
end;

function IsClearPenStyle(ACustomPenStyle: TBGRAPenStyle): boolean;
begin
  if (length(ACustomPenStyle)=1) and (ACustomPenStyle[0]=0) then
    result := true
  else
    result := false;
end;

function DuplicatePenStyle(ACustomPenStyle: array of single): TBGRAPenStyle;
var
  i: Integer;
begin
  setlength(result,length(ACustomPenStyle));
  for i := 0 to high(result) do
    result[i]:= ACustomPenStyle[i];
end;

procedure ApplyPenStyle(const leftPts, rightPts: array of TPointF; const penstyle: TBGRAPenStyle;
    width: single; var posstyle: single; out styledPts: ArrayOfTPointF);
var
  styleIndex :integer;
  remainingDash: single;

  procedure NextStyleIndex;
  begin
    inc(styleIndex);
    if styleIndex = length(penstyle) then
      styleIndex := 0;
    remainingDash += penstyle[styleindex];
  end;

var
  dashStartIndex: integer;
  dashLeftStartPos,dashRightStartPos : TPointF;
  betweenDash: boolean;

  procedure StartDash(index: integer; t: single);
  begin
    dashStartIndex := index;
    dashLeftStartPos := leftPts[index] + (leftPts[index+1]-leftPts[index])*t;
    dashRightStartPos := rightPts[index] + (rightPts[index+1]-rightPts[index])*t;
    betweenDash := false;
  end;

var
  nbStyled: integer;

  procedure AddPt(pt: TPointF);
  begin
    if (nbStyled = 0) or (pt <> styledPts[nbStyled-1]) then
    begin
      if nbStyled = length(styledPts) then
        setlength(styledPts,nbStyled*2+4);
      styledPts[nbStyled] := pt;
      inc(nbStyled);
    end;
  end;

  procedure StartPolygon;
  begin
    if nbStyled > 0 then AddPt(EmptyPointF);
  end;

  procedure EndDash(index: integer; t: single);
  var dashLeftEndPos,dashRightEndPos: TPointF;
    i: Integer;
  begin
    if t=0 then
    begin
      dashLeftEndPos := leftPts[index];
      dashRightEndPos := rightPts[index];
    end else
    begin
      dashLeftEndPos := leftPts[index] + (leftPts[index+1]-leftPts[index])*t;
      dashRightEndPos := rightPts[index] + (rightPts[index+1]-rightPts[index])*t;
    end;
    StartPolygon;
    AddPt(dashLeftStartPos);
    for i := dashStartIndex+1 to index do
      AddPt(leftPts[i]);
    AddPt(dashLeftEndPos);
    AddPt(dashRightEndPos);
    for i := index downto dashStartIndex+1 do
      AddPt(rightPts[i]);
    AddPt(dashRightStartPos);
    betweenDash := true;
  end;

var
  i,nb: integer;
  styleLength: single;
  len,lenDone: single;

begin
  nbStyled := 0;
  styledPts := nil;
  if IsClearPenStyle(penstyle) then exit;
  if IsSolidPenStyle(penstyle) then
  begin
    for i := 0 to high(leftPts) do AddPt(leftPts[i]);
    for i := high(rightPts) downto 0 do AddPt(rightPts[i]);
    setlength(styledPts,nbStyled);
    exit;
  end;
  if length(leftPts) <> length(rightPts) then
    raise Exception.Create('Dimension mismatch');
  nb := length(leftPts);
  if length(penstyle) mod 2 <> 0 then
    raise Exception.Create('Pen style must contain an even number of values');
  styleLength := 0;
  styleIndex := -1;
  remainingDash := 0;
  betweenDash   := false;
  for i := 0 to high(penstyle) do
    if penstyle[i] <= 0 then
      raise Exception.Create('Invalid pen dash length')
    else
    begin
      styleLength += penstyle[i];
      if styleLength >= posstyle then
      begin
        styleIndex := i;
        remainingDash := styleLength-posstyle;
        break;
      end;
    end;
  if styleIndex = -1 then
  begin
    styleIndex := 0;
    remainingDash := penstyle[0];
  end;

  if styleIndex mod 2 = 0 then
    StartDash(0, 0) else
      betweenDash := true;
  for i := 0 to nb-2 do
  begin
    len := (sqrt(sqr(leftPts[i+1].x-leftPts[i].x) + sqr(leftPts[i+1].y-leftPts[i].y))+
           sqrt(sqr(rightPts[i+1].x-rightPts[i].x) + sqr(rightPts[i+1].y-rightPts[i].y)))/(2*width);
    lenDone := 0;
    while lenDone < len do
    begin
      if len-lenDone < remainingDash then
      begin
        remainingDash -= len-lenDone;
        if remainingDash = 0 then NextStyleIndex;
        lenDone := len;
      end else
      if betweenDash then
      begin
        lenDone += remainingDash;
        StartDash(i, lenDone/len);
        remainingDash := 0;
        NextStyleIndex;
      end else
      begin
        lenDone += remainingDash;
        EndDash(i, lenDone/len);
        remainingDash := 0;
        NextStyleIndex;
      end;
    end;
  end;
  if not betweenDash then
    EndDash(nb-1,0);
  setlength(styledPts,nbStyled);
end;

function ComputeWidePolylinePoints(const linepts: array of TPointF; width: single;
          pencolor: TBGRAPixel; linecap: TPenEndCap; joinstyle: TPenJoinStyle; const penstyle: TBGRAPenStyle;
          options: TBGRAPolyLineOptions; miterLimit: single; arrow: TBGRACustomArrow): ArrayOfTPointF;
const oneOver512 = 1/512;
var
  startArrowPos, startArrowDir, endArrowPos, endArrowDir: TPointF;
  startArrowLinePos, endArrowLinePos: single;
  borders : array of record
              leftSide,rightSide: TLineDef;
              len: single;
              leftDir: TPointF;
            end;
  compPts: array of TPointF;
  nbCompPts: integer;
  revCompPts: array of TPointF;
  nbRevCompPts: integer;
  pts: array of TPointF;
  roundPrecision: integer;
  hw: single; //half-width

  procedure AddPt(normal,rev: TPointF); overload;
  begin
    if (nbCompPts > 0) and (compPts[nbCompPts-1]=normal) and
       (nbRevCompPts > 0) and (revCompPts[nbRevCompPts-1]=rev) then exit;

    if nbCompPts = length(compPts) then
     setlength(compPts, length(compPts)*2);
    compPts[nbCompPts] := normal;
    inc(nbCompPts);

    if nbRevCompPts = length(revCompPts) then
     setlength(revCompPts, length(revCompPts)*2);
    revCompPts[nbRevCompPts] := rev;
    inc(nbRevCompPts);
  end;

  procedure AddPt(xnormal,ynormal: single; xrev,yrev: single); overload;
  begin
    AddPt(PointF(xnormal,ynormal),PointF(xrev,yrev));
  end;

  procedure AddRoundCap(origin: TPointF; dir: TPointF; fromCenter: boolean; flipped: boolean= false);
  var i: integer;
      a,s,c: single;
      offset,flipvalue: single;
  begin
    if fromCenter then offset := 0 else offset := -Pi/2;
    if flipped then flipvalue := -1 else flipvalue := 1;
    for i := 1 to RoundPrecision do
    begin
      a := i/(RoundPrecision+1)*Pi/2 + offset;
      s := sin(a)*hw*flipvalue;
      c := cos(a)*hw;
      AddPt( PointF(origin.x+ dir.x*c - dir.y*s, origin.y + dir.y*c + dir.x*s),
             PointF(origin.x+ dir.x*c + dir.y*s, origin.y + dir.y*c - dir.x*s) );
    end;
  end;

  procedure AddRoundCapAlphaJoin(origin: TPointF; dir: TPointF; fromCenter: boolean; flipped: boolean= false);
  var i: integer;
      a,s,c: single;
      offset,flipvalue: single;
      t,alphaFactor: single; //antialiasing join
  begin
    if fromCenter then offset := 0 else offset := -Pi/2;
    if flipped then flipvalue := -1 else flipvalue := 1;

    alphaFactor := GetAlphaJoinFactor(pencolor.alpha);

    for i := 1 to RoundPrecision do
    begin
      a := i/(RoundPrecision+1)*Pi/2 + offset;
      s := sin(a)*hw*flipvalue;
      c := cos(a);
      t := (1 - c) * (0.2 + alphaFactor*0.3) + alphaFactor;
      c *= hw;
      AddPt( PointF(origin.x+ dir.x*(c-t) - dir.y*s, origin.y + dir.y*(c-t) + dir.x*s),
             PointF(origin.x+ dir.x*(c-t) + dir.y*s, origin.y + dir.y*(c-t) - dir.x*s) );
    end;
  end;

  function ComputeRoundJoin(origin, pt1,pt2: TPointF): ArrayOfTPointF;
  var a1,a2: single;
      da: single;
      precision,i: integer;
  begin
    a1 := arctan2(pt1.y-origin.y,pt1.x-origin.x);
    a2 := arctan2(pt2.y-origin.y,pt2.x-origin.x);
    if a2-a1 > Pi then a2 -= 2*Pi;
    if a1-a2 > Pi then a1 -= 2*Pi;
    if a2=a1 then
    begin
      setlength(result,1);
      result[0] := pt1;
      exit;
    end;
    da := a2-a1;
    precision := round( sqrt( sqr(pt2.x-pt1.x)+sqr(pt2.y-pt1.y) ) ) +2;
    setlength(result,precision);
    for i := 0 to precision-1 do
      result[i] := origin + PointF( cos(a1+i/(precision-1)*da)*hw,
                                    sin(a1+i/(precision-1)*da)*hw );
  end;

var
  joinLeft,joinRight: array of TPointF;
  nbJoinLeft,nbJoinRight: integer;

  procedure SetJoinLeft(joinpts: array of TPointF);
  var i: integer;
  begin
    nbJoinLeft := length(joinpts);
    if length(joinLeft) < nbJoinLeft then setlength(joinLeft,length(joinLeft)+nbJoinLeft+2);
    for i := 0 to nbJoinLeft-1 do
      joinLeft[i] := joinpts[i];
  end;

  procedure SetJoinRight(joinpts: array of TPointF);
  var i: integer;
  begin
    nbJoinRight := length(joinpts);
    if length(joinRight) < nbJoinRight then setlength(joinRight,length(joinRight)+nbJoinRight+2);
    for i := 0 to nbJoinRight-1 do
      joinRight[i] := joinpts[i];
  end;

  procedure AddJoin(index: integer);
  var len,i: integer;
  begin
    len := nbJoinLeft;
    if nbJoinRight > len then
      len := nbJoinRight;
    if len = 0 then exit;
    if (len > 1) and (index <> -1) then
    begin
      if nbJoinLeft=1 then
        AddPt(joinLeft[0], joinLeft[0] - 2*borders[Index].leftDir) else
      if nbJoinRight=1 then
        AddPt( joinRight[0] + 2* borders[index].leftDir, joinRight[0]);
    end;
    for i := 0 to len-1 do
    begin
      AddPt(joinLeft[i*nbJoinLeft div len],
            joinRight[i*nbJoinRight div len]);
    end;
    if (len > 1) and (index <> -1) then
    begin
      if nbJoinLeft=1 then
        AddPt(joinLeft[0], joinLeft[0] - 2*borders[index+1].leftDir) else
      if nbJoinRight=1 then
        AddPt(joinRight[0]+2*borders[index+1].leftDir, joinRight[0]);
    end;
  end;

var
  NbPolyAcc: integer;

  procedure FlushLine(lastPointIndex: integer);
  var
    enveloppe: arrayOfTPointF;
    posstyle: single;
    i,idxInsert: Integer;
  begin
    if lastPointIndex <> -1 then
       AddPt( pts[lastPointIndex] + borders[lastPointIndex-1].leftDir,
              pts[lastPointIndex] - borders[lastPointIndex-1].leftDir);

    if (lastPointIndex = high(pts)) and (linecap = pecRound) and not (plNoEndCap in options) then
    begin
      if not (plRoundCapOpen in options) then
        AddRoundCap(pts[high(pts)],borders[high(pts)-1].leftSide.dir,false)
      else
       AddRoundCapAlphaJoin(pts[high(pts)],
            -borders[high(pts)-1].leftSide.dir, false,true);
    end;
    posstyle := 0;
    ApplyPenStyle(slice(compPts,nbCompPts),slice(revCompPts,nbRevCompPts),penstyle,width,posstyle,enveloppe);

    if Result=nil then
    begin
      Result := enveloppe;
      NbPolyAcc := length(enveloppe);
    end
      else
    if enveloppe <> nil then
    begin
      if NbPolyAcc +1+length(enveloppe) > length(Result) then
        setlength(Result, length(Result)*2+1+length(enveloppe));

      idxInsert := NbPolyAcc+1;
      Result[idxInsert-1] := EmptyPointF;
      for i := 0 to high(enveloppe) do
        Result[idxInsert+i]:= enveloppe[i];
      inc(NbPolyAcc, length(enveloppe)+1);
    end;

    nbCompPts := 0;
    nbRevCompPts := 0;
  end;

  procedure CycleFlush;
  var idx: integer;
  begin
    if Result = nil then
    begin
      if (nbCompPts > 1) and (nbRevCompPts > 1) then
      begin
        compPts[0] := compPts[nbCompPts-1];
        revCompPts[0] := revCompPts[nbRevCompPts-1];
      end;
      FlushLine(-1);
    end else
    begin
      if (nbCompPts >= 1) and (nbRevCompPts >= 1) and (NbPolyAcc >= 2) then
      begin
        Result[0] := compPts[nbCompPts-1];
        idx := 0;
        while (idx < high(Result)) and (not isEmptyPointF(Result[idx+1])) do inc(idx);
        Result[idx] := revCompPts[nbRevCompPts-1];
      end;
      FlushLine(-1);
    end;
  end;

  procedure FinalizeArray;
  var arrowStartData, arrowEndData: ArrayOfTPointF;
    finalNb,i,delta: integer;
    hasStart,hasEnd: boolean;
  begin
    if assigned(arrow) and not isEmptyPointF(startArrowPos) then
      arrowStartData := arrow.ComputeStartAt(startArrowPos, startArrowDir, width, startArrowLinePos)
    else
      arrowStartData := nil;
    if assigned(arrow) and not isEmptyPointF(endArrowPos) then
      arrowEndData := arrow.ComputeEndAt(endArrowPos, endArrowDir, width, endArrowLinePos)
    else
      arrowEndData := nil;
    hasStart := length(arrowStartData)>0;
    hasEnd := length(arrowEndData)>0;
    finalNb := NbPolyAcc;
    if hasStart then
    begin
      delta := length(arrowStartData)+1;
      finalNb += delta;
    end else delta := 0;
    if hasEnd then finalNb += length(arrowEndData)+1;
    SetLength(Result, finalNb);
    if hasStart then
    begin
      for i := NbPolyAcc-1 downto 0 do
        result[i+delta] := result[i];
      result[delta-1] := EmptyPointF;
      for i := 0 to high(arrowStartData) do
        result[i] := arrowStartData[i];
    end;
    if hasEnd then
    begin
      delta += NbPolyAcc+1;
      result[delta-1] := EmptyPointF;
      for i := 0 to high(arrowEndData) do
        result[i+delta] := arrowEndData[i];
    end;
  end;

var
  i: integer;
  dir: TPointF;
  leftInter,rightInter,diff: TPointF;
  len,maxMiter: single;
  littleBorder: TLineDef;
  turn,maxDiff: single;
  nbPts: integer;
  ShouldFlushLine, HasLittleBorder, NormalRestart: Boolean;
  pt1,pt2,pt3,pt4: TPointF;
  linePos: single;
  startArrowDone,endArrowDone: boolean;
  wantedStartArrowPos,wantedEndArrowPos: single;

begin
  Result := nil;

  if (length(linepts)=0) or (width = 0) then exit;
  if IsClearPenStyle(penstyle) then exit;
  for i := 0 to high(linepts) do
    if isEmptyPointF(linepts[i]) then
    begin
      result := ComputeWidePolyPolylinePoints(linepts,width,pencolor,linecap,joinstyle,penstyle,options,miterLimit,arrow);
      exit;
    end;

  if (plAutoCycle in options) and (length(linepts) >= 2) and (linepts[0]=linepts[high(linepts)]) then
    options := options + [plCycle];
  if plNoEndCap in options then options := options - [plRoundCapOpen];

  hw := width / 2;
  case joinstyle of
  pjsBevel,pjsRound: maxMiter := hw*1.001;
  pjsMiter: if miterLimit < 1.001 then maxMiter := hw*1.001 else
               maxMiter := hw*miterLimit;
  else
    raise Exception.Create('Unknown join style');
  end;

  roundPrecision := round(hw)+2;

  nbPts := 0;
  setlength(pts, length(linepts)+2);
  for i := 0 to high(linepts) do
    if (nbPts = 0) or (abs(linepts[i].x-pts[nbPts-1].x)>oneOver512) or (abs(linepts[i].y-pts[nbPts-1].y)>oneOver512) then
    begin
      pts[nbPts]:= linePts[i];
      inc(nbPts);
    end;
  if (nbPts > 1) and (plCycle in options) and
      (abs(pts[0].x-pts[nbPts-1].x)<=oneOver512) and
      (abs(pts[0].y-pts[nbPts-1].y)<=oneOver512) then dec(nbPts);
  if (plCycle in options) and (nbPts > 2) then
  begin
    if (pts[nbPts-1] <> pts[0]) then
    begin
      pts[nbPts] := pts[0];
      inc(nbPts);
    end;
    pts[nbPts] := pts[1];
    inc(nbPts);
  end else
    options -= [plCycle];

  setlength(pts,nbPts);

  if nbPts = 1 then
  begin
    if (linecap <> pecFlat) and ((linecap <> pecRound) or not (plRoundCapOpen in options)) then
      result := ComputeEllipse(pts[0].x,pts[0].y,hw,hw);
    exit;
  end;

  startArrowDir := EmptyPointF;
  startArrowPos := EmptyPointF;
  endArrowDir := EmptyPointF;
  endArrowPos := EmptyPointF;
  if Assigned(arrow) then
  begin
    wantedStartArrowPos:= arrow.StartOffsetX;
    wantedEndArrowPos:= arrow.EndOffsetX;
    startArrowDone := not arrow.IsStartDefined;
    endArrowDone := not arrow.IsEndDefined;
  end
  else
  begin
    wantedStartArrowPos:= 0;
    wantedEndArrowPos:= 0;
    startArrowDone := true;
    endArrowDone := true;
  end;

  //init computed points arrays
  setlength(compPts, length(pts)*2+4);
  setlength(revCompPts, length(pts)*2+4); //reverse order array
  nbCompPts := 0;
  nbRevCompPts := 0;
  NbPolyAcc := 0;

  if not endArrowDone then
  begin
    wantedEndArrowPos:= -wantedEndArrowPos*width;
    linePos := 0;
    for i := high(pts) downto 1 do
    begin
      dir := pts[i-1]-pts[i];
      len := sqrt(dir*dir);
      dir *= 1/len;
      if not endArrowDone and (linePos+len >= wantedEndArrowPos) then
      begin
        endArrowPos := pts[i];
        endArrowDir := -dir;
        endArrowLinePos := -linePos/width;
        endArrowDone := true;
        break;
      end;
      linePos += len;
    end;
  end;

  wantedStartArrowPos:= -wantedStartArrowPos*width;
  linePos := 0;
  //compute borders
  setlength(borders, length(pts)-1);
  for i := 0 to high(pts)-1 do
  begin
    dir := pts[i+1]-pts[i];
    len := sqrt(dir*dir);
    dir *= 1/len;
    if not startArrowDone and (linePos+len >= wantedStartArrowPos) then
    begin
      startArrowPos := pts[i];
      startArrowDir := -dir;
      startArrowLinePos := -linePos/width;
      startArrowDone := true;
    end;
    if (linecap = pecSquare) and ((not (plNoStartCap in options) and (i=0)) or
      (not (plNoEndCap in options) and (i=high(pts)-1))) then //for square cap, just start and end further
    begin
      if i=0 then
        pts[0] -= dir*hw;

      if (i=high(pts)-1) then
        pts[high(pts)] += dir*hw;

      //length changed
      dir := pts[i+1]-pts[i];
      len := sqrt(dir*dir);
      dir *= 1/len;
    end else
    if not (plNoStartCap in options) and (linecap = pecRound) and (i=0) and not (plCycle in options) then
      AddRoundCap(pts[0], -dir ,true);

    borders[i].len := len;
    borders[i].leftDir := PointF(dir.y*hw,-dir.x*hw);
    borders[i].leftSide.origin := pts[i] + borders[i].leftDir;
    borders[i].leftSide.dir := dir;
    borders[i].rightSide.origin := pts[i] - borders[i].leftDir;
    borders[i].rightSide.dir := dir;
    linePos += len;
  end;

  //first points
  AddPt( pts[0] + borders[0].leftDir,
         pts[0] - borders[0].leftDir );

  setlength(joinLeft,1);
  setlength(joinRight,1);
  ShouldFlushLine := False;
  //between first and last points
  for i := 0 to high(pts)-2 do
  begin
    HasLittleBorder := false;

    //determine u-turn
    turn := borders[i].leftSide.dir * borders[i+1].leftSide.dir;
    if turn < -0.99999 then
    begin
      if joinstyle <> pjsRound then
      begin
        littleBorder.origin := pts[i+1] + borders[i].leftSide.dir*maxMiter;
        littleBorder.dir := borders[i].leftDir;
        HasLittleBorder := true;
      end;

      nbJoinLeft := 0;
      nbJoinRight:= 0;

      ShouldFlushLine := True;
    end else
    if turn > 0.99999 then //straight line
    begin
      pt1 := pts[i+1] + borders[i].leftDir;
      pt2 := pts[i+2] + borders[i+1].leftDir;
      SetJoinLeft([pt1, (pt1+pt2)*(1/2),pt2]);

      pt1 := pts[i+1] - borders[i].leftDir;
      pt2 := pts[i+2] - borders[i+1].leftDir;
      SetJoinRight([pt1,(pt1+pt2)*(1/2),pt2]);
    end else
    begin
      //determine turning left or right
      turn := borders[i].leftSide.dir.x*borders[i+1].leftSide.dir.y - borders[i].leftSide.dir.y*borders[i+1].leftSide.dir.x;

      maxDiff := borders[i].len;
      if borders[i+1].len < maxDiff then
        maxDiff := borders[i+1].len;
      if penstyle <> nil then
        if maxDiff > 2*width then maxDiff := 2*width;
      maxDiff := sqrt(sqr(maxDiff)+sqr(hw));

      //leftside join
      leftInter := IntersectLine( borders[i].leftSide, borders[i+1].leftSide );
      diff := leftInter-pts[i+1];
      len := sqrt(diff*diff);
      if (len > maxMiter) and (turn >= 0) then //if miter too far
      begin
        diff.x /= len;
        diff.y /= len;
        if joinstyle <> pjsRound then
        begin
          //compute little border
          littleBorder.origin := pts[i+1]+diff*maxMiter;
          littleBorder.dir := PointF(diff.y,-diff.x);
          HasLittleBorder := true;

          //intersect with each border
          pt1 := IntersectLine(borders[i].leftSide, littleBorder);
          pt2 := IntersectLine(borders[i+1].leftSide, littleBorder);
          SetJoinLeft( [pt1, pt2] );
        end else
        begin
          //perpendicular
          pt1 := PointF(pts[i+1].x+borders[i].leftSide.dir.y*hw,
                        pts[i+1].y-borders[i].leftSide.dir.x*hw);
          pt2 := PointF(pts[i+1].x+borders[i+1].leftSide.dir.y*hw,
                        pts[i+1].y-borders[i+1].leftSide.dir.x*hw);
          SetJoinLeft(ComputeRoundJoin(pts[i+1],pt1,pt2));
        end;
      end else
      if (len > maxDiff) and (turn <= 0) then //if inner intersection too far
      begin
        ShouldFlushLine := True;
        nbJoinLeft := 0;
      end else
      begin
        if (turn > 0) and (len > 1.0001*hw) then
          SetJoinLeft([leftInter,leftInter]) else
        begin
          nbJoinLeft := 1;
          joinLeft[0] := leftInter;
        end;
      end;

      //rightside join
      rightInter := IntersectLine( borders[i].rightSide, borders[i+1].rightSide );
      diff := rightInter-pts[i+1];
      len := sqrt(diff*diff);
      if (len > maxMiter) and (turn <= 0) then //if miter too far
      begin
        diff *= 1/len;

        if joinstyle <> pjsRound then
        begin
          //compute little border
          littleBorder.origin := pts[i+1] + diff*maxMiter;
          littleBorder.dir := PointF(diff.y, -diff.x);
          HasLittleBorder := true;

          //intersect with each border
          pt1 := IntersectLine(borders[i].rightSide, littleBorder);
          pt2 := IntersectLine(borders[i+1].rightSide, littleBorder);
          SetJoinRight( [pt1, pt2] );
        end else
        begin
          //perpendicular
          pt1 := PointF(pts[i+1].x-borders[i].rightSide.dir.y*hw,
                        pts[i+1].y+borders[i].rightSide.dir.x*hw);
          pt2 := PointF(pts[i+1].x-borders[i+1].rightSide.dir.y*hw,
                        pts[i+1].y+borders[i+1].rightSide.dir.x*hw);
          SetJoinRight(ComputeRoundJoin(pts[i+1],pt1,pt2));
        end;
      end else
      if (len > maxDiff) and (turn >= 0) then //if inner intersection too far
      begin
        ShouldFlushLine := True;
        nbJoinRight := 0;
      end else
      begin
        if (turn < 0) and (len > 1.0001*hw) then
          SetJoinRight([rightInter,rightInter]) else
        begin
          nbJoinRight := 1;
          joinRight[0] := rightInter;
        end;
      end;
    end;

    if ShouldFlushLine then
    begin
      NormalRestart := True;
      if HasLittleBorder then
      begin
        if turn >= 0 then
        begin
          //intersect with each border
          pt1 := IntersectLine(borders[i].leftSide, littleBorder);
          pt2 := IntersectLine(borders[i+1].leftSide, littleBorder);
          pt3 := pts[i+1] - borders[i].leftDir;
          pt4 := pts[i+1] + borders[i].leftDir;

          AddPt(pt4,pt3);
          AddPt(pt1,pt2);
        end else
        begin
          //intersect with each border
          pt1 := IntersectLine(borders[i+1].rightSide, littleBorder);
          pt2 := IntersectLine(borders[i].rightSide, littleBorder);
          pt3 := pts[i+1] + borders[i].leftDir;
          pt4 := pts[i+1] - borders[i].leftDir;

          AddPt(pt3,pt4);
          AddPt(pt1,pt2);
        end;

        FlushLine(-1);

        AddPt(pt2,pt1);
      end else
      if joinstyle = pjsRound then
      begin

        if {(penstyle= nil) and} (turn > 0) then
        begin
          pt1 := pts[i+1] + borders[i].leftDir;
          pt2 := pts[i+1] + borders[i+1].leftDir;
          pt3 := pts[i+1] - borders[i].leftDir;
          pt4 := pts[i+1];

          SetJoinLeft([pt1,pt1]);
          SetJoinRight([pt3,pt4]);
          AddJoin(-1);

          SetJoinLeft(ComputeRoundJoin(pts[i+1],pt1,pt2));
          nbJoinRight := 1;
          joinRight[0] := pt4;
          AddJoin(-1);
          FlushLine(-1);
        end else
        if {(penstyle= nil) and} (turn < 0) then
        begin
          pt1 := pts[i+1] - borders[i].leftDir;
          pt2 := pts[i+1] - borders[i+1].leftDir;
          pt3 := pts[i+1] + borders[i].leftDir;
          pt4 := pts[i+1];

          SetJoinRight([pt1,pt1]);
          SetJoinLeft([pt3,pt4]);
          AddJoin(-1);

          SetJoinRight(ComputeRoundJoin(pts[i+1],pt1,pt2));
          nbJoinLeft := 1;
          joinLeft[0] := pt4;
          AddJoin(-1);
          FlushLine(-1);
        end else
        if (nbCompPts > 1) and (nbRevCompPts > 1) then
        begin
          pt1 := pts[i+1]+borders[i].leftDir;
          pt2 := pts[i+1]-borders[i].leftDir;
          AddPt( pt1, pt2 );
          FlushLine(-1);
        end else
        begin
          FlushLine(i+1);
        end;
      end else
      begin
        FlushLine(i+1);
        if turn > 0 then
          AddPt( leftInter, pts[i+1]+borders[i].leftDir ) else
        if turn < 0 then
          AddPt( pts[i+1] - borders[i].leftDir, rightInter );
      end;

      If NormalRestart then
        AddPt(pts[i+1]+borders[i+1].leftDir,
              pts[i+1]-borders[i+1].leftDir);

      ShouldFlushLine := false;
    end else
      AddJoin(i);
  end;

  if plCycle in options then
    CycleFlush
  else
    FlushLine(high(pts));

  FinalizeArray;
end;

function ComputeWidePolyPolylinePoints(const linepts: array of TPointF;
  width: single; pencolor: TBGRAPixel; linecap: TPenEndCap;
  joinstyle: TPenJoinStyle; const penstyle: TBGRAPenStyle;
  options: TBGRAPolyLineOptions; miterLimit: single; arrow: TBGRACustomArrow): ArrayOfTPointF;

var
  results: array of array of TPointF;
  nbResults,nbTotalPts: integer;

  procedure AddWidePolyline(startIndex,endIndexP1: integer);
  var
    tempWidePolyline: array of TPointF;
    subPts: array of TPointF;
    j : integer;
  begin
    if endIndexP1 > startIndex then
    begin
      setlength(subPts,endIndexP1-startIndex);
      for j := startIndex to endIndexP1-1 do
        subPts[j-startIndex] := linepts[j];
      tempWidePolyline := ComputeWidePolylinePoints(subPts,width,pencolor,linecap,joinstyle,penstyle,options,miterLimit,arrow);
      if length(results) = nbresults then
        setlength(results,(nbresults+1)*2);
      results[nbResults] := tempWidePolyline;
      if nbResults <> 0 then inc(nbTotalPts);
      inc(nbResults);
      inc(nbTotalPts,length(tempWidePolyline));
    end;
  end;

var
  start,i,j: integer;

begin
  start := 0;
  nbResults := 0;
  nbTotalPts := 0;
  for i := 0 to high(linepts) do
    if isEmptyPointF(linepts[i]) then
    begin
      AddWidePolyline(start,i);
      start := i+1;
    end;
  AddWidePolyline(start,length(linepts));

  setlength(result, nbTotalPts);
  start := 0;
  for i := 0 to nbResults-1 do
  begin
    if i <> 0 then
    begin
      result[start] := EmptyPointF;
      inc(start);
    end;
    for j := 0 to high(results[i]) do
    begin
      result[start] := results[i][j];
      inc(start);
    end;
  end;
end;

{ TBGRAPenStroker }

function TBGRAPenStroker.GetArrow: TBGRACustomArrow;
begin
  result := FArrow;
end;

function TBGRAPenStroker.GetArrowOwned: boolean;
begin
  result := FArrowOwned;
end;

function TBGRAPenStroker.GetCustomPenStyle: TBGRAPenStyle;
begin
  result := FCustomPenStyle;
end;

function TBGRAPenStroker.GetJoinStyle: TPenJoinStyle;
begin
  result := FJoinStyle;
end;

function TBGRAPenStroker.GetLineCap: TPenEndCap;
begin
  result := FLineCap;
end;

function TBGRAPenStroker.GetMiterLimit: single;
begin
  result := FMiterLimit;
end;

function TBGRAPenStroker.GetPenStyle: TPenStyle;
begin
  result := FPenStyle;
end;

function TBGRAPenStroker.GetStrokeMatrix: TAffineMatrix;
begin
  result := FOriginalStrokeMatrix;
end;

procedure TBGRAPenStroker.SetArrow(AValue: TBGRACustomArrow);
begin
  FArrow := AValue;
end;

procedure TBGRAPenStroker.SetArrowOwned(AValue: boolean);
begin
  FArrowOwned := AValue;
end;

procedure TBGRAPenStroker.SetCustomPenStyle(AValue: TBGRAPenStyle);
begin
  if FCustomPenStyle=AValue then Exit;
  FCustomPenStyle:=AValue;
  if AValue = SolidPenStyle then FPenStyle := psSolid
  else if AValue = ClearPenStyle then FPenStyle:= psClear
  else if AValue = DashPenStyle then FPenStyle:= psDash
  else if AValue = DotPenStyle then FPenStyle := psDot
  else if AValue = DashDotPenStyle then FPenStyle:= psDashDot
  else if AValue = DashDotDotPenStyle then FPenStyle:= psDashDotDot
  else
  begin
    FPenStyle := psPattern;
    FCustomPenStyle:= DuplicatePenStyle(AValue);
  end;
end;

procedure TBGRAPenStroker.SetJoinStyle(AValue: TPenJoinStyle);
begin
  FJoinStyle:= AValue;
end;

procedure TBGRAPenStroker.SetLineCap(AValue: TPenEndCap);
begin
  FLineCap:= AValue;
end;

procedure TBGRAPenStroker.SetMiterLimit(AValue: single);
begin
  FMiterLimit := AValue;
end;

procedure TBGRAPenStroker.SetStrokeMatrix(const AValue: TAffineMatrix);
begin
  if FOriginalStrokeMatrix=AValue then Exit;
  FOriginalStrokeMatrix:=AValue;
  FStrokeMatrix := AValue;
  FStrokeMatrix[1,3] := 0;
  FStrokeMatrix[2,3] := 0;
  FStrokeZoom := max(VectLen(PointF(FStrokeMatrix[1,1],FStrokeMatrix[2,1])),
          VectLen(PointF(FStrokeMatrix[1,2],FStrokeMatrix[2,2])));
  if FStrokeZoom > 0 then
    FStrokeMatrix *= AffineMatrixScale(1/FStrokeZoom,1/FStrokeZoom);
  FStrokeMatrixIdentity := IsAffineMatrixIdentity(FStrokeMatrix);
  FStrokeMatrixInverse := AffineMatrixInverse(FStrokeMatrix);
end;

procedure TBGRAPenStroker.SetPenStyle(AValue: TPenStyle);
begin
  if FPenStyle=AValue then Exit;
  Case AValue of
  psSolid: FCustomPenStyle := SolidPenStyle;
  psDash: FCustomPenStyle := DashPenStyle;
  psDot: FCustomPenStyle := DotPenStyle;
  psDashDot: FCustomPenStyle := DashDotPenStyle;
  psDashDotDot: FCustomPenStyle := DashDotDotPenStyle;
  else FCustomPenStyle := ClearPenStyle;
  end;
  FPenStyle := AValue;
end;

constructor TBGRAPenStroker.Create;
begin
  Style := psSolid;
  LineCap := pecRound;
  JoinStyle := pjsBevel;
  MiterLimit := 2;
  fillchar(FOriginalStrokeMatrix,sizeof(FOriginalStrokeMatrix),0);
  StrokeMatrix := AffineMatrixIdentity;
end;

destructor TBGRAPenStroker.Destroy;
begin
  if ArrowOwned then FreeAndNil(FArrow);
  inherited Destroy;
end;

function TBGRAPenStroker.ComputePolyline(const APoints: array of TPointF;
  AWidth: single; AClosedCap: boolean): ArrayOfTPointF;
var
  c: TBGRAPixel;
begin
  if not AClosedCap then
    c := BGRAWhite //needed for alpha junction
  else
    c := BGRAPixelTransparent;

  if FStrokeMatrixIdentity then
    result := ComputePolyline(APoints,AWidth*FStrokeZoom,c,AClosedCap)
  else
    result := FStrokeMatrix*ComputePolyline(FStrokeMatrixInverse*APoints,AWidth*FStrokeZoom,c,AClosedCap);
end;

function TBGRAPenStroker.ComputePolyline(const APoints: array of TPointF;
  AWidth: single; APenColor: TBGRAPixel; AClosedCap: boolean): ArrayOfTPointF;
var options: TBGRAPolyLineOptions;
begin
  options := [];
  if Assigned(Arrow) and Arrow.IsStartDefined then options += [plNoStartCap];
  if Assigned(Arrow) and Arrow.IsEndDefined then options += [plNoEndCap];
  if not AClosedCap then options += [plRoundCapOpen];
  if FStrokeMatrixIdentity then
    result := BGRAPen.ComputeWidePolylinePoints(APoints, AWidth*FStrokeZoom, APenColor, LineCap, JoinStyle, CustomPenStyle, options, MiterLimit, Arrow)
  else
    result := FStrokeMatrix*BGRAPen.ComputeWidePolylinePoints(FStrokeMatrixInverse*APoints, AWidth*FStrokeZoom, APenColor, LineCap, JoinStyle, CustomPenStyle, options, MiterLimit, Arrow);
end;

function TBGRAPenStroker.ComputePolylineAutocycle(
  const APoints: array of TPointF; AWidth: single): ArrayOfTPointF;
var options: TBGRAPolyLineOptions;
begin
  options := [plAutoCycle];
  if Assigned(Arrow) and Arrow.IsStartDefined then options += [plNoStartCap];
  if Assigned(Arrow) and Arrow.IsEndDefined then options += [plNoEndCap];
  if FStrokeMatrixIdentity then
    result := BGRAPen.ComputeWidePolylinePoints(APoints, AWidth*FStrokeZoom, BGRAPixelTransparent, LineCap, JoinStyle, CustomPenStyle, options, MiterLimit, Arrow)
  else
    result := FStrokeMatrix*BGRAPen.ComputeWidePolylinePoints(FStrokeMatrixInverse*APoints, AWidth*FStrokeZoom, BGRAPixelTransparent, LineCap, JoinStyle, CustomPenStyle, options, MiterLimit, Arrow)
end;

function TBGRAPenStroker.ComputePolygon(const APoints: array of TPointF;
  AWidth: single): ArrayOfTPointF;
begin
  if FStrokeMatrixIdentity then
    result := BGRAPen.ComputeWidePolylinePoints(APoints, AWidth*FStrokeZoom, BGRAPixelTransparent, LineCap, JoinStyle, CustomPenStyle, [plCycle], MiterLimit)
  else
    result := FStrokeMatrix*BGRAPen.ComputeWidePolylinePoints(FStrokeMatrixInverse*APoints, AWidth*FStrokeZoom, BGRAPixelTransparent, LineCap, JoinStyle, CustomPenStyle, [plCycle], MiterLimit);
end;

initialization

  //special pen styles
  SolidPenStyle := nil;

  setlength(ClearPenStyle,1);
  ClearPenStyle[0] := 0;

  DashPenStyle := BGRAPenStyle(3,1);
  DotPenStyle := BGRAPenStyle(1,1);
  DashDotPenStyle := BGRAPenStyle(3,1,1,1);
  DashDotDotPenStyle := BGRAPenStyle(3,1,1,1,1,1);

end.

