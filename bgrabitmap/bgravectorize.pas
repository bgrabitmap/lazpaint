unit BGRAVectorize;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, BGRABitmapTypes, BGRATypewriter, BGRATransform, BGRACanvas2D;

function VectorizeMonochrome(ASource: TBGRACustomBitmap; zoom: single; PixelCenteredCoordinates: boolean): ArrayOfTPointF;

type
  TGlyphSizes = array of record
            Glyph: String;
            Width,Height: single;
  end;

  TWordBreakHandler = procedure(var ABefore, AAfter: string) of object;

  { TBGRAVectorizedFont }

  TBGRAVectorizedFont = class(TBGRACustomTypeWriter)
  private
    FName : string;
    FStyle: TFontStyles;
    FResolution: integer;
    FFont: TFont;
    FBuffer: TBGRACustomBitmap;
    FFullHeight: single;
    FFontMatrix: TAffineMatrix;
    FOrientation: single;
    FQuadraticCurves: boolean;
    FItalicSlope: single;
    FWordBreakHandler: TWordBreakHandler;
    procedure SetItalicSlope(AValue: single);
    procedure SetOrientation(AValue: single);
    procedure SetQuadraticCurves(AValue: boolean);
    procedure SetResolution(AValue: integer);
    procedure SetFontMatrix(AValue: TAffineMatrix);
    procedure SetFullHeight(AValue: single);
    procedure SetName(AValue: string);
    procedure SetStyle(AValue: TFontStyles);
  protected
    procedure UpdateFont;
    procedure UpdateMatrix;
    function GetGlyph(AIdentifier: string): TBGRAGlyph; override;
    procedure DefaultWordBreakHandler(var ABefore, AAfter: string);
  public
    constructor Create;
    destructor Destroy; override;
    function GetGlyphSize(AIdentifier:string): TPointF;
    function GetTextGlyphSizes(AText:string): TGlyphSizes;
    function GetTextSize(AText:string): TPointF;
    procedure SplitText(var AText: string; AMaxWidth: single; out ARemains: string);
    procedure DrawTextWordBreak(ADest: TBGRACanvas2D; AText: string; X, Y, MaxWidth: Single; AAlign: TBGRATypeWriterAlignment=twaTopLeft);
    procedure DrawTextRect(ADest: TBGRACanvas2D; AText: string; X1,Y1,X2,Y2: Single; AAlign: TBGRATypeWriterAlignment=twaTopLeft);
    procedure DrawTextRect(ADest: TBGRACanvas2D; AText: string; ATopLeft,ABottomRight: TPointF; AAlign: TBGRATypeWriterAlignment=twaTopLeft);
    function GetTextWordBreakGlyphBoxes(AText: string; X,Y, MaxWidth: Single; AAlign: TBGRATypeWriterAlignment = twaTopLeft): TGlyphBoxes;
    function GetTextRectGlyphBoxes(AText: string; X1,Y1,X2,Y2: Single; AAlign: TBGRATypeWriterAlignment=twaTopLeft): TGlyphBoxes;
    function GetTextRectGlyphBoxes(AText: string; ATopLeft,ABottomRight: TPointF; AAlign: TBGRATypeWriterAlignment=twaTopLeft): TGlyphBoxes;

    property Resolution: integer read FResolution write SetResolution;
    property Style: TFontStyles read FStyle write SetStyle;
    property Name: string read FName write SetName;
    property FullHeight: single read FFullHeight write SetFullHeight;
    property FontMatrix: TAffineMatrix read FFontMatrix write SetFontMatrix;
    property Orientation: single read FOrientation write SetOrientation;
    property QuadraticCurves: boolean read FQuadraticCurves write SetQuadraticCurves;
    property ItalicSlope: single read FItalicSlope write SetItalicSlope;
    property OnWordBreak: TWordBreakHandler read FWordBreakHandler write FWordBreakHandler;
  end;

implementation

uses BGRAText, LCLProc, Types;

function VectorizeMonochrome(ASource: TBGRACustomBitmap; zoom: single; PixelCenteredCoordinates: boolean): ArrayOfTPointF;
const unitShift = 6;
      iHalf = 1 shl (unitShift-1);
      iOut = 10;  //0.15
      iUnit = 1 shl unitShift;
      iDiag = 13; //0.20
      useNiceLines = true;

var
  n: integer;
  factor: single;
  offset: single;
  p,pprev,pnext : PBGRAPixel;
  x,y,ix,iy: integer;
  points: array of record
            coord: tpoint;
            prev,next: integer;
            drawn,{shouldRemove,}removed: boolean;
          end;
  nbPoints:integer;
  PointsPreviousLineStart,PointsCurrentLineStart: integer;
  cur: packed array[1..9] of boolean;
  ortho: array of array of boolean;

  polygonF: array of TPointF;

  function AddPoint(x,y,APrev,ANext: integer): integer;
  begin
    if nbpoints = length(points) then
      setlength(points, nbpoints*2+1);
    result := nbpoints;
    with points[result] do
    begin
      coord := point(x,y);
      prev := APrev;
      next := ANext;
      drawn := false;
      removed := false;
//      shouldRemove := false;
    end;
    inc(nbpoints);
  end;
  procedure AddLine(x1,y1,x2,y2: integer);
  var i,j,k: integer;
  begin
    for i := PointsPreviousLineStart to nbpoints-1 do
      if (points[i].coord.x = x2) and (points[i].coord.y = y2) and (points[i].prev = -1) then
      begin
        for j := i+1 to nbpoints-1 do
          if (points[j].coord.x = x1) and (points[j].coord.y = y1) and (points[j].next = -1) then
          begin
            points[j].next := i;
            points[i].prev := j;
            exit;
          end;
        k := addpoint(x1,y1,-1,i);
        points[i].prev := k;
        exit;
      end else
      if (points[i].coord.x = x1) and (points[i].coord.y = y1) and (points[i].next = -1) then
      begin
        for j := i+1 to nbpoints-1 do
          if (points[j].coord.x = x2) and (points[j].coord.y = y2) and (points[j].prev = -1) then
          begin
            points[j].prev := i;
            points[i].next := j;
            exit;
          end;
        k := addpoint(x2,y2,i,-1);
        points[i].next := k;
        exit;
      end;
    k := addpoint(x1,y1,-1,-1);
    points[k].next := addpoint(x2,y2,k,-1);
  end;
  procedure AddLine(x1,y1,x2,y2,x3,y3: integer);
  begin
    AddLine(x1,y1,x2,y2);
    AddLine(x2,y2,x3,y3);
  end;
  procedure AddLine(x1,y1,x2,y2,x3,y3,x4,y4: integer);
  begin
    AddLine(x1,y1,x2,y2);
    AddLine(x2,y2,x3,y3);
    AddLine(x3,y3,x4,y4);
  end;
  procedure AddLine(x1,y1,x2,y2,x3,y3,x4,y4,x5,y5: integer);
  begin
    AddLine(x1,y1,x2,y2);
    AddLine(x2,y2,x3,y3);
    AddLine(x3,y3,x4,y4);
    AddLine(x4,y4,x5,y5);
  end;

  procedure AddPolygon(n: integer);

    procedure Rewind(out cycle: boolean);
    var cur: integer;
    begin
      cur := n;
      cycle := false;
      while (points[cur].prev <> -1) do
      begin
        cur := points[cur].prev;
        if cur = n then
        begin
          cycle := true; //identify cycle
          break;
        end;
      end;
      n := cur;
    end;

    function aligned(start1,end1,start2,end2: integer): boolean;
    var
      u,v: TPointF;
      lu,lv: single;
    begin
      if (start1=-1) or (end1=-1) or (start2=-1) or (end2=-1) then
      begin
        result :=false;
        exit;
      end;
      u := pointF(points[end1].coord.x - points[start1].coord.x, points[end1].coord.y - points[start1].coord.y);
      lu := sqrt(u*u);
      if lu <> 0 then u *= 1/lu;
      v := pointF(points[end2].coord.x - points[start2].coord.x, points[end2].coord.y - points[start2].coord.y);
      lv := sqrt(v*v);
      if lv <> 0 then v *= 1/lv;

      result := u*v > 0.999;
    end;

    function angle45(prev,cur,next: integer): boolean;
    var
      u,v: TPointF;
      lu,lv,dp: single;
    begin
      if (prev=-1) or (cur=-1) or (next=-1) then
      begin
        result :=false;
        exit;
      end;
      u := pointF(points[next].coord.x - points[cur].coord.x, points[next].coord.y - points[cur].coord.y);
      lu := sqrt(u*u);
      if lu <> 0 then u *= 1/lu;
      v := pointF(points[cur].coord.x - points[prev].coord.x, points[cur].coord.y - points[prev].coord.y);
      lv := sqrt(v*v);
      if lv <> 0 then v *= 1/lv;

      dp := u*v;
      result := (dp > 0.70) and (dp < 0.72);
    end;

    procedure RemoveAligned;
    var cur,prev,next: integer;
    begin
      cur := n;
      prev := -1;
      while points[cur].next <> -1 do
      begin
        next := points[cur].next;
        //remove aligned points
        if prev <> -1 then
          if aligned(prev,cur,cur,next) then points[cur].removed := true;

        if not points[cur].removed then prev := cur;
        cur := next;

        if next = n then
        begin
          next := points[cur].next;
          if (prev <> -1) and (next <> prev) then
            if aligned(prev,cur,cur,next) then points[cur].removed := true;
          break; //cycle
        end;
      end;
    end;

    procedure MakePolygon(cycle: boolean);
    var ptsF: array of TPointF;
        nbPtsF: integer;
        nb,nb2,cur,i: integer;
    begin
      cur := n;
      nb := 0;
      nb2 := 0;
      repeat
        if not points[cur].removed then inc(nb);
        inc(nb2);
        cur := points[cur].next;
      until (cur = -1) or (cur = n) or (nb2 > nbPoints);
      if (nb2 > nbPoints) or (nb <= 2) then exit;

      setlength(ptsF,nb);
      cur := n;
      nbPtsF := 0;
      repeat
        with points[cur] do
          if not removed then
          begin
            ptsF[nbPtsF] := pointf(coord.x*factor+offset,coord.y*factor+offset);
            points[cur].drawn := true;
            inc(nbPtsF);
          end;
        cur := points[cur].next;
      until (cur = -1) or (cur = n);

      if cycle then
      begin
        if polygonF = nil then
          polygonF := ptsF else
        begin
          cur := length(polygonF);
          setlength(polygonF, length(polygonF)+length(ptsF)+1);
          polygonF[cur] := EmptyPointF;
          for i := 0 to high(ptsF) do
          begin
            inc(cur);
            polygonF[cur] := ptsF[i];
          end;
        end;
      end;
      ptsF := nil;
      //Bitmap.DrawPolyLineAntialias(ptsF,BGRABlack,1);
    end;

    function segabslength(cur,next: integer): integer;
    var
      tx,ty: integer;
    begin
      if (cur = -1) or (next = -1) then result := 0
      else
      begin
        tx := abs(points[next].coord.x - points[cur].coord.x);
        ty := abs(points[next].coord.y - points[cur].coord.y);
        if tx > ty then result := tx else result := ty;
      end;
    end;

    function getnext(cur: integer): integer;
    begin
      result := cur;
      if result <> -1 then
      begin
        repeat
          result := points[result].next;
          if result = cur then result := -1;
        until (result = -1) or not points[result].removed;
      end;
    end;

    function getprev(cur: integer): integer;
    begin
      result := cur;
      if result <> -1 then
      begin
        repeat
          result := points[result].prev;
          if result = cur then result := -1;
        until (result = -1) or not points[result].removed;
      end;
    end;

    procedure NiceLines;
    var next,next2,prev,cur,len,prevlen,nextlen,expectedlen,nb,
        rcur,rprev,rprev2,rnext,rnext2,temp: integer;
    begin
      cur := n;
      nb := 0;
      repeat
        if not points[cur].removed then
        begin
          next := getnext(cur);
          len := segabslength(cur,next);
          if (len > iUnit - (iHalf shr 1)) and (len < iUnit + (iHalf shr 1)) then
          begin
            prev := getprev(cur);
            next2 := getnext(next);
            prevlen := segabslength(prev,cur);
            nextlen := segabslength(next,next2);
            if (prevlen > iUnit - (iHalf shr 1)) and (nextlen > iUnit - (iHalf shr 1)) and angle45(prev,cur,next) and angle45(cur,next,next2) and
              aligned(prev,cur,next,next2) then
            begin
              if prevlen > nextlen then
              begin
                rprev := AddPoint(points[cur].coord.x - (points[next2].coord.x-points[next].coord.x),
                  points[cur].coord.y - (points[next2].coord.y-points[next].coord.y), prev,cur);
                points[prev].next := rprev;
                points[cur].prev := rprev;
                prev := rprev;
                expectedlen := nextlen;
              end else
              if nextlen > prevlen then
              begin
                rnext := AddPoint(points[next].coord.x - (points[prev].coord.x-points[cur].coord.x),
                  points[next].coord.y - (points[prev].coord.y-points[cur].coord.y),
                    next,next2);
                points[next].next := rnext;
                points[next2].prev := rnext;
                next2 := rnext;
                expectedlen := prevlen;
              end else
                expectedlen := (nextlen+prevlen) div 2;

{              points[cur].shouldRemove := true;
              points[next].shouldRemove:= true;}
              points[cur].removed := true;
              rcur := prev;
              rnext := cur;
              temp := prev;
              repeat
                rprev := getprev(rcur);
                if not angle45(rprev,rcur,rnext) or not aligned(rprev,rcur,cur,next) then break;
                prevlen := segabslength(rprev,rcur);
                if (prevlen < iUnit - (iHalf shr 1)) or (prevlen > iUnit + (iHalf shr 1)) then break;
                points[rcur].removed := true;
                temp := rprev;

                rprev2 := getprev(rprev);
                if not angle45(rprev2,rprev,rcur) or not aligned(rprev2,rprev,prev,cur) then break;
                prevlen := segabslength(rprev2,rprev);
                if abs(prevlen-expectedlen) > 0 then break;
                points[rprev].removed := true;
                temp := rprev2;

                rcur := rprev2;
                rnext := rprev;
              until (rcur=-1);
              prev := temp;

              points[next].removed:= true;
              rcur := next2;
              rprev := next;
              temp := next2;
              repeat
                rnext := getnext(rcur);
                if not angle45(rnext,rcur,rprev) or not aligned(rcur,rnext,cur,next) then break;
                nextlen := segabslength(rnext,rcur);
                if (nextlen < iUnit - (iHalf shr 1)) or (nextlen > iUnit + (iHalf shr 1)) then break;
                points[rcur].removed := true;
                temp := rnext;

                rnext2 := getnext(rnext);
                if not angle45(rnext2,rnext,rcur) or not aligned(rnext,rnext2,next,next2) then break;
                nextlen := segabslength(rnext2,rnext);
                if abs(nextlen-expectedlen) > 0 then break;
                points[rnext].removed := true;
                temp := rnext2;

                rcur := rnext2;
                rprev := rnext;
              until (rcur=-1);
              next2 := temp;

              points[prev].next := next2;
              points[next2].prev := prev;

              next := next2;
            end;
          end;
          cur := next;
        end else
          cur := points[cur].next;
        inc(nb);
      until (cur=-1) or (cur = n) or (nb>nbPoints);
{      cur := n;
      nb := 0;
      repeat
        if not points[cur].removed and points[cur].shouldRemove then
        begin
          prev := getprev(cur);
          next := getnext(cur);
          points[prev].next := next;
          points[next].prev := prev;
          points[cur].removed := true;
        end;
        cur := points[cur].next;
        inc(nb);
      until (cur=-1) or (cur = n) or (nb>nbPoints);}
    end;


  var cycle: boolean;
  begin
    //rewind
    Rewind(cycle);
    RemoveAligned;
    if useNiceLines then NiceLines;
    MakePolygon(cycle);
  end;

begin
  nbpoints := 0;
  points := nil;
  polygonF := nil;

  setlength(ortho,ASource.height,ASource.width);
  for y := 0 to ASource.Height-1 do
  begin
    if y = 0 then
      pprev := nil
    else
      pprev := ASource.ScanLine[y-1];
    p := ASource.ScanLine[y];
    if y = ASource.Height-1 then
      pnext := nil
    else
      pnext := ASource.ScanLine[y+1];

    {$hints off}
    fillchar(cur,sizeof(cur),0);
    {$hints on}
    cur[6] := (p^.green <= 128); inc(p);
    if pprev <> nil then begin cur[9] := (pprev^.green <= 128); inc(pprev); end;
    if pnext <> nil then begin cur[3] := (pnext^.green <= 128); inc(pnext); end;
    for x := 0 to ASource.Width-1 do
    begin
      cur[1] := cur[2];
      cur[2] := cur[3];
      cur[4] := cur[5];
      cur[5] := cur[6];
      cur[7] := cur[8];
      cur[8] := cur[9];

      if x = ASource.Width-1 then
      begin
        cur[6]:= false;
        cur[9]:= false;
        cur[3]:= false;
      end else
      begin
        cur[6] := (p^.green <= 128); inc(p);
        if pprev <> nil then begin cur[9] := (pprev^.green <= 128); inc(pprev); end;
        if pnext <> nil then begin cur[3] := (pnext^.green <= 128); inc(pnext); end;
      end;

      ortho[y,x] := (cur[5] and not cur[7] and not cur[9] and not cur[3] and not cur[1]);
      if (not cur[5] and (cur[4] xor cur[6]) and (cur[8] xor cur[2]) and
          (ord(cur[1])+ord(cur[3])+ord(cur[7])+ord(cur[9]) = 3)) then
      begin
        if (not cur[6] and not cur[9] and not cur[8] and ((ASource.getPixel(integer(x-1),integer(y-2)).green <= 128) or (ASource.getPixel(integer(x+2),integer(y+1)).green <= 128)) ) or
          (not cur[8] and not cur[7] and not cur[4] and ((ASource.getPixel(integer(x-2),integer(y+1)).green <= 128) or (ASource.getPixel(integer(x+1),integer(y-2)).green <= 128)) ) or
          (not cur[4] and not cur[1] and not cur[2] and ((ASource.getPixel(integer(x+1),integer(y+2)).green <= 128) or (ASource.getPixel(integer(x-2),integer(y-1)).green <= 128)) ) or
          (not cur[2] and not cur[3] and not cur[6] and ((ASource.getPixel(integer(x-1),integer(y+2)).green <= 128) or (ASource.getPixel(integer(x+2),integer(y-1)).green <= 128)) ) then
            ortho[y,x] := true;
      end;
      { or
        (cur[5] and cur[4] and cur[6] and cur[2] and cur[8] and (Ord(cur[1])+ord(cur[3])+ord(cur[7])+ord(cur[9]) = 3))};
      //if ortho[y,x] then AddPoint(x shl unitShift,y shl unitShift,-1,-1);
    end;
  end;

  PointsCurrentLineStart := nbPoints;
  for y := 0 to ASource.Height-1 do
  begin
    iy := y shl unitShift;

    PointsPreviousLineStart := PointsCurrentLineStart;
    PointsCurrentLineStart := nbPoints;
    if y = 0 then
      pprev := nil
    else
      pprev := ASource.ScanLine[y-1];
    p := ASource.ScanLine[y];
    if y = ASource.Height-1 then
      pnext := nil
    else
      pnext := ASource.ScanLine[y+1];

    {$hints off}
    fillchar(cur,sizeof(cur),0);
    {$hints on}
    cur[6] := (p^.green <= 128); inc(p);
    if pprev <> nil then begin cur[9] := (pprev^.green <= 128); inc(pprev); end;
    if pnext <> nil then begin cur[3] := (pnext^.green <= 128); inc(pnext); end;
    ix := 0;
    for x := 0 to ASource.Width-1 do
    begin
      cur[1] := cur[2];
      cur[2] := cur[3];
      cur[4] := cur[5];
      cur[5] := cur[6];
      cur[7] := cur[8];
      cur[8] := cur[9];

      if x = ASource.Width-1 then
      begin
        cur[6]:= false;
        cur[9]:= false;
        cur[3]:= false;
      end else
      begin
        cur[6] := (p^.green <= 128); inc(p);
        if pprev <> nil then begin cur[9] := (pprev^.green <= 128); inc(pprev); end;
        if pnext <> nil then begin cur[3] := (pnext^.green <= 128); inc(pnext); end;
      end;

      if cur[5] then
      begin
        if not cur[1] and not cur[2] and not cur[3] and not cur[4] and not cur[6] and not cur[7] and not cur[8] and not cur[9] then
        begin
          AddLine(ix-iHalf,iy-iDiag,ix-iDiag,iy-iHalf,ix+iDiag,iy-iHalf,ix+iHalf,iy-iDiag,ix+iHalf,iy+iDiag);
          AddLine(ix+iHalf,iy+iDiag,ix+iDiag,iy+iHalf,ix-iDiag,iy+iHalf,ix-iHalf,iy+iDiag,ix-iHalf,iy-iDiag);
        end else
        if cur[6] and not cur[9] and not cur[8] then
        begin
          if cur[7] then
          begin
            if not ortho[y-1,x] then
            begin
              if ortho[y,x-1] then AddLine(ix+iHalf,iy-iHalf,ix-iHalf,iy-iHalf) else
                AddLine(ix+iHalf,iy-iHalf,ix+iDiag,iy-iHalf,ix-iOut,iy-iUnit+iOut);
            end;
          end else
          if cur[4] then AddLine(ix+iHalf,iy-iHalf,ix-iHalf,iy-iHalf) else
          if cur[1] then AddLine(ix+iHalf,iy-iHalf,ix-iDiag,iy-iHalf,ix-iUnit+iOut,iy+iOut) else
          if cur[2] then AddLine(ix+iHalf,iy-iHalf,ix-iHalf,iy-iHalf,ix-iHalf,iy+iHalf) else
          if cur[3] then
          begin
            if ortho[y,x+1] then AddLine(ix+iHalf,iy-iHalf,ix-iHalf,iy-iHalf,ix-iHalf,iy+iHalf,ix+iHalf,iy+iHalf) else
              AddLine(ix+iHalf,iy-iHalf,ix-iHalf,iy-iHalf,ix-iHalf,iy+iDiag,ix+iOut,iy+iUnit-iOut)
          end else
            AddLine(ix+iHalf,iy-iHalf,ix-iHalf,iy-iHalf,ix-iHalf,iy,ix-iHalf,iy+iHalf,ix+iHalf,iy+iHalf);
        end;
        if cur[8] and not cur[7] and not cur[4] then
        begin
          if cur[1] then
          begin
            if not ortho[y,x-1] then
            begin
              if ortho[y+1,x] then AddLine(ix-iHalf,iy-iHalf,ix-iHalf,iy+iHalf) else
                AddLine(ix-iHalf,iy-iHalf,ix-iHalf,iy-iDiag,ix-iUnit+iOut,iy+iOut);
            end;
          end else
          if cur[2] then AddLine(ix-iHalf,iy-iHalf,ix-iHalf,iy+iHalf) else
          if cur[3] then AddLine(ix-iHalf,iy-iHalf,ix-iHalf,iy+iDiag,ix+iOut,iy+iUnit-iOut) else
          if cur[6] then AddLine(ix-iHalf,iy-iHalf,ix-iHalf,iy+iHalf,ix+iHalf,iy+iHalf) else
          if cur[9] then
          begin
            if ortho[y-1,x] then AddLine(ix-iHalf,iy-iHalf,ix-iHalf,iy+iHalf,ix+iHalf,iy+iHalf,ix+iHalf,iy-iHalf) else
              AddLine(ix-iHalf,iy-iHalf,ix-iHalf,iy+iHalf,ix+iDiag,iy+iHalf,ix+iUnit-iOut,iy-iOut)
          end else
            AddLine(ix-iHalf,iy-iHalf,ix-iHalf,iy+iHalf,ix,iy+iHalf,ix+iHalf,iy+iHalf,ix+iHalf,iy-iHalf);
        end;
        if cur[4] and not cur[1] and not cur[2] then
        begin
          if cur[3] then
          begin
            if not ortho[y+1,x] then
            begin
              if ortho[y,x+1] then AddLine(ix-iHalf,iy+iHalf,ix+iHalf,iy+iHalf) else
                AddLine(ix-iHalf,iy+iHalf,ix-iDiag,iy+iHalf,ix+iOut,iy+iUnit-iOut);
            end;
          end else
          if cur[6] then AddLine(ix-iHalf,iy+iHalf,ix+iHalf,iy+iHalf) else
          if cur[9] then AddLine(ix-iHalf,iy+iHalf,ix+iDiag,iy+iHalf,ix+iUnit-iOut,iy-iOut) else
          if cur[8] then AddLine(ix-iHalf,iy+iHalf,ix+iHalf,iy+iHalf,ix+iHalf,iy-iHalf) else
          if cur[7] then
          begin
            if ortho[y,x-1] then AddLine(ix-iHalf,iy+iHalf,ix+iHalf,iy+iHalf,ix+iHalf,iy-iHalf,ix-iHalf,iy-iHalf) else
              AddLine(ix-iHalf,iy+iHalf,ix+iHalf,iy+iHalf,ix+iHalf,iy-iDiag,ix-iOut,iy-iUnit+iOut)
          end else
            AddLine(ix-iHalf,iy+iHalf,ix+iHalf,iy+iHalf,ix+iHalf,iy,ix+iHalf,iy-iHalf,ix-iHalf,iy-iHalf);
        end;
        if cur[2] and not cur[3] and not cur[6] then
        begin
          if cur[9] then
          begin
            if not ortho[y,x+1] then
            begin
              if ortho[y-1,x] then AddLine(ix+iHalf,iy+iHalf,ix+iHalf,iy-iHalf) else
                AddLine(ix+iHalf,iy+iHalf,ix+iHalf,iy+iDiag,ix+iUnit-iOut,iy-iOut);
            end;
          end else
          if cur[8] then AddLine(ix+iHalf,iy+iHalf,ix+iHalf,iy-iHalf) else
          if cur[7] then AddLine(ix+iHalf,iy+iHalf,ix+iHalf,iy-iDiag,ix-iOut,iy-iUnit+iOut) else
          if cur[4] then AddLine(ix+iHalf,iy+iHalf,ix+iHalf,iy-iHalf,ix-iHalf,iy-iHalf) else
          if cur[1] then
          begin
            if ortho[y+1,x] then AddLine(ix+iHalf,iy+iHalf,ix+iHalf,iy-iHalf,ix-iHalf,iy+iHalf,ix-iHalf,iy+iHalf) else
              AddLine(ix+iHalf,iy+iHalf,ix+iHalf,iy-iHalf,ix-iDiag,iy-iHalf,ix-iUnit+iOut,iy+iOut)
          end else
            AddLine(ix+iHalf,iy+iHalf,ix+iHalf,iy-iHalf,ix,iy-iHalf,ix-iHalf,iy-iHalf,ix-iHalf,iy+iHalf);
        end;

        if cur[3] and not cur[6] then
        begin
          if cur[9] then
          begin
            if ortho[y+1,x] and ortho[y-1,x] then AddLine(ix+iHalf,iy+iHalf,ix+iHalf,iy-iHalf) else
            if ortho[y+1,x] and not ortho[y-1,x] then AddLine(ix+iHalf,iy+iHalf,ix+iHalf,iy+iDiag,ix+iUnit-iOut,iy-iOut) else
            if not ortho[y+1,x] and ortho[y-1,x] then AddLine(ix+iUnit-iOut,iy+iOut,ix+iHalf,iy-iDiag,ix+iHalf,iy-iHalf) else
              AddLine(ix+iUnit-iOut,iy+iOut,ix+iUnit-iOut*2,iy,ix+iUnit-iOut,iy-iOut);
          end else
          if cur[8] then
          begin
            if not ortho[y,x+1] then
            if ortho[y+1,x] then AddLine(ix+iHalf,iy+iHalf,ix+iHalf,iy-iHalf) else
              AddLine(ix+iUnit-iOut,iy+iOut,ix+iHalf,iy-iDiag,ix+iHalf,iy-iHalf)
          end else
          if cur[7] then
          begin
            if ortho[y+1,x] and ortho[y,x-1] then
              AddLine(ix+iHalf,iy+iHalf,ix+iHalf,iy-iHalf,ix-iHalf,iy-iHalf) else
            if ortho[y+1,x] and not ortho[y,x-1] then
              AddLine(ix+iHalf,iy+iHalf,ix+iHalf,iy-iDiag,ix-iOut,iy-iUnit+iOut) else
            if not ortho[y+1,x] and ortho[y,x-1] then
              AddLine(ix+iUnit-iOut,iy+iOut, ix+iDiag,iy-iHalf, ix-iHalf,iy-iHalf) else
              AddLine(ix+iUnit-iOut,iy+iOut,ix-iOut,iy-iUnit+iOut)
          end else
          if cur[4] then AddLine(ix+iUnit-iOut,iy+iOut,ix+iDiag,iy-iHalf,ix-iHalf,iy-iHalf) else
          if cur[1] then
          begin
            if ortho[y+1,x] then AddLine(ix+iHalf,iy+iHalf,ix+iHalf,iy-iHalf,ix-iHalf,iy-iHalf,ix-iHalf,iy+iHalf) else
              AddLine(ix+iUnit-iOut,iy+iOut,ix+iDiag,iy-iHalf,ix-iDiag,iy-iHalf,ix-iUnit+iOut,iy+iOut);
          end else
          if cur[2] then
          begin
            if ortho[y+1,x] then AddLine(ix+iHalf,iy+iHalf,ix+iHalf,iy-iHalf,ix-iHalf,iy-iHalf,ix-iHalf,iy+iHalf) else
              AddLine(ix+iUnit-iOut,iy+iOut,ix+iDiag,iy-iHalf,ix-iHalf,iy-iHalf,ix-iHalf,iy+iHalf);
          end else
            AddLine(ix+iUnit-iOut,iy+iOut,ix+iDiag,iy-iHalf,ix-iHalf,iy-iHalf,ix-iHalf,iy+iDiag,ix+iOut,iy+iUnit-iOut);
        end;

        if cur[9] and not cur[8] then
        begin
          if cur[7] then
          begin
            if ortho[y,x+1] and ortho[y,x-1] then AddLine(ix+iHalf,iy-iHalf,ix-iHalf,iy-iHalf) else
            if ortho[y,x+1] and not ortho[y,x-1] then AddLine(ix+iHalf,iy-iHalf,ix+iDiag,iy-iHalf,ix-iOut,iy-iUnit+iOut) else
            if not ortho[y,x+1] and ortho[y,x-1] then AddLine(ix+iOut,iy-iUnit+iOut,ix-iDiag,iy-iHalf,ix-iHalf,iy-iHalf) else
              AddLine(ix+iOut,iy-iUnit+iOut,ix,iy-iUnit+iOut*2,ix-iOut,iy-iUnit+iOut);
          end else
          if cur[4] then
          begin
            if not ortho[y-1,x] then
            if ortho[y,x+1] then AddLine(ix+iHalf,iy-iHalf,ix-iHalf,iy-iHalf) else
              AddLine(ix+iOut,iy-iUnit+iOut,ix-iDiag,iy-iHalf,ix-iHalf,iy-iHalf)
          end else
          if cur[1] then
          begin
            if ortho[y,x+1] and ortho[y+1,x] then
              AddLine(ix+iHalf,iy-iHalf,ix-iHalf,iy-iHalf,ix-iHalf,iy+iHalf) else
            if ortho[y,x+1] and not ortho[y+1,x] then
              AddLine(ix+iHalf,iy-iHalf,ix-iDiag,iy-iHalf,ix-iUnit+iOut,iy+iOut) else
            if not ortho[y,x+1] and ortho[y+1,x] then
              AddLine(ix+iOut,iy-iUnit+iOut, ix-iHalf,iy-iDiag, ix-iHalf,iy+iHalf) else
              AddLine(ix+iOut,iy-iUnit+iOut,ix-iUnit+iOut,iy+iOut)
          end else
          if cur[2] then AddLine(ix+iOut,iy-iUnit+iOut,ix-iHalf,iy-iDiag,ix-iHalf,iy+iHalf) else
          if cur[3] then
          begin
            if ortho[y,x+1] then AddLine(ix+iHalf,iy-iHalf,ix-iHalf,iy-iHalf,ix-iHalf,iy+iHalf,ix+iHalf,iy+iHalf) else
              AddLine(ix+iOut,iy-iUnit+iOut,ix-iHalf,iy-iDiag,ix-iHalf,iy+iDiag,ix+iOut,iy+iUnit-iOut);
          end else
          if cur[6] then
          begin
            if ortho[y,x+1] then AddLine(ix+iHalf,iy-iHalf,ix-iHalf,iy-iHalf,ix-iHalf,iy+iHalf,ix+iHalf,iy+iHalf) else
              AddLine(ix+iOut,iy-iUnit+iOut,ix-iHalf,iy-iDiag,ix-iHalf,iy+iHalf,ix+iHalf,iy+iHalf);
          end else
            AddLine(ix+iOut,iy-iUnit+iOut,ix-iHalf,iy-iDiag,ix-iHalf,iy+iHalf,ix+iDiag,iy+iHalf,ix+iUnit-iOut,iy-iOut);
        end;

        if cur[7] and not cur[4] then
        begin
          if cur[1] then
          begin
            if ortho[y-1,x] and ortho[y+1,x] then AddLine(ix-iHalf,iy-iHalf,ix-iHalf,iy+iHalf) else
            if ortho[y-1,x] and not ortho[y+1,x] then AddLine(ix-iHalf,iy-iHalf,ix-iHalf,iy-iDiag,ix-iUnit+iOut,iy+iOut) else
            if not ortho[y-1,x] and ortho[y+1,x] then AddLine(ix-iUnit+iOut,iy-iOut,ix-iHalf,iy+iDiag,ix-iHalf,iy+iHalf) else
              AddLine(ix-iUnit+iOut,iy-iOut,ix-iUnit+iOut*2,iy,ix-iUnit+iOut,iy+iOut);
          end else
          if cur[2] then
          begin
            if not ortho[y,x-1] then
            if ortho[y-1,x] then AddLine(ix-iHalf,iy-iHalf,ix-iHalf,iy+iHalf) else
              AddLine(ix-iUnit+iOut,iy-iOut,ix-iHalf,iy+iDiag,ix-iHalf,iy+iHalf)
          end else
          if cur[3] then
          begin
            if ortho[y-1,x] and ortho[y,x+1] then
              AddLine(ix-iHalf,iy-iHalf,ix-iHalf,iy+iHalf,ix+iHalf,iy+iHalf) else
            if ortho[y-1,x] and not ortho[y,x+1] then
              AddLine(ix-iHalf,iy-iHalf,ix-iHalf,iy+iDiag,ix+iOut,iy+iUnit-iOut) else
            if not ortho[y-1,x] and ortho[y,x+1] then
              AddLine(ix-iUnit+iOut,iy-iOut, ix-iDiag,iy+iHalf, ix+iHalf,iy+iHalf) else
              AddLine(ix-iUnit+iOut,iy-iOut,ix+iOut,iy+iUnit-iOut)
          end else
          if cur[6] then AddLine(ix-iUnit+iOut,iy-iOut,ix-iDiag,iy+iHalf,ix+iHalf,iy+iHalf) else
          if cur[9] then
          begin
            if ortho[y-1,x] then AddLine(ix-iHalf,iy-iHalf,ix-iHalf,iy+iHalf,ix+iHalf,iy+iHalf,ix+iHalf,iy-iHalf) else
              AddLine(ix-iUnit+iOut,iy-iOut,ix-iDiag,iy+iHalf,ix+iDiag,iy+iHalf,ix+iUnit-iOut,iy-iOut);
          end else
          if cur[8] then
          begin
            if ortho[y-1,x] then AddLine(ix-iHalf,iy-iHalf,ix-iHalf,iy+iHalf,ix+iHalf,iy+iHalf,ix+iHalf,iy-iHalf) else
              AddLine(ix-iUnit+iOut,iy-iOut,ix-iDiag,iy+iHalf,ix+iHalf,iy+iHalf,ix+iHalf,iy-iHalf);
          end else
            AddLine(ix-iUnit+iOut,iy-iOut,ix-iDiag,iy+iHalf,ix+iHalf,iy+iHalf,ix+iHalf,iy-iDiag,ix-iOut,iy-iUnit+iOut);
        end;

        if cur[1] and not cur[2] then
        begin
          if cur[3] then
          begin
            if ortho[y,x-1] and ortho[y,x+1] then AddLine(ix-iHalf,iy+iHalf,ix+iHalf,iy+iHalf) else
            if ortho[y,x-1] and not ortho[y,x+1] then AddLine(ix-iHalf,iy+iHalf,ix-iDiag,iy+iHalf,ix+iOut,iy+iUnit-iOut) else
            if not ortho[y,x-1] and ortho[y,x+1] then AddLine(ix-iOut,iy+iUnit-iOut,ix+iDiag,iy+iHalf,ix+iHalf,iy+iHalf) else
              AddLine(ix-iOut,iy+iUnit-iOut,ix,iy+iUnit-iOut*2,ix+iOut,iy+iUnit-iOut);
          end else
          if cur[6] then
          begin
            if not ortho[y+1,x] then
            if ortho[y,x-1] then AddLine(ix-iHalf,iy+iHalf,ix+iHalf,iy+iHalf) else
              AddLine(ix-iOut,iy+iUnit-iOut,ix+iDiag,iy+iHalf,ix+iHalf,iy+iHalf)
          end else
          if cur[9] then
          begin
            if ortho[y,x-1] and ortho[y-1,x] then
              AddLine(ix-iHalf,iy+iHalf,ix+iHalf,iy+iHalf,ix+iHalf,iy-iHalf) else
            if ortho[y,x-1] and not ortho[y-1,x] then
              AddLine(ix-iHalf,iy+iHalf,ix+iDiag,iy+iHalf,ix+iUnit-iOut,iy-iOut) else
            if not ortho[y,x-1] and ortho[y-1,x] then
              AddLine(ix-iOut,iy+iUnit-iOut, ix+iHalf,iy+iDiag, ix+iHalf,iy-iHalf) else
              AddLine(ix-iOut,iy+iUnit-iOut,ix+iUnit-iOut,iy-iOut)
          end else
          if cur[8] then AddLine(ix-iOut,iy+iUnit-iOut,ix+iHalf,iy+iDiag,ix+iHalf,iy-iHalf) else
          if cur[7] then
          begin
            if ortho[y,x-1] then AddLine(ix-iHalf,iy+iHalf,ix+iHalf,iy+iHalf,ix+iHalf,iy-iHalf,ix-iHalf,iy-iHalf) else
              AddLine(ix-iOut,iy+iUnit-iOut,ix+iHalf,iy+iDiag,ix+iHalf,iy-iDiag,ix-iOut,iy-iUnit+iOut);
          end else
          if cur[4] then
          begin
            if ortho[y,x-1] then AddLine(ix-iHalf,iy+iHalf,ix+iHalf,iy+iHalf,ix+iHalf,iy-iHalf,ix-iHalf,iy-iHalf) else
              AddLine(ix-iOut,iy+iUnit-iOut,ix+iHalf,iy+iDiag,ix+iHalf,iy-iHalf,ix-iHalf,iy-iHalf);
          end else
            AddLine(ix-iOut,iy+iUnit-iOut,ix+iHalf,iy+iDiag,ix+iHalf,iy-iHalf,ix-iDiag,iy-iHalf,ix-iUnit+iOut,iy+iOut);
        end;
      end else
      if ortho[y,x] then
      begin
        if not cur[9] then AddLine(ix+iHalf,iy+iHalf,ix-iHalf,iy+iHalf,ix-iHalf,iy-iHalf) else
        if not cur[7] then AddLine(ix+iHalf,iy-iHalf,ix+iHalf,iy+iHalf,ix-iHalf,iy+iHalf) else
        if not cur[1] then AddLine(ix-iHalf,iy-iHalf,ix+iHalf,iy-iHalf,ix+iHalf,iy+iHalf) else
        if not cur[3] then AddLine(ix-iHalf,iy+iHalf,ix-iHalf,iy-iHalf,ix+iHalf,iy-iHalf);
      end;
      inc(ix,iUnit);
    end;
  end;

  factor := zoom/iUnit;
  offset := zoom*0.5;
  if PixelCenteredCoordinates then Offset -= 0.5;
  for n := 0 to nbPoints-1 do
    with points[n] do
    if not drawn and not removed then
      AddPolygon(n);

  result := polygonF;
end;

{ TBGRAVectorizedFont }

procedure TBGRAVectorizedFont.SetResolution(AValue: integer);
begin
  if FResolution=AValue then Exit;
  FResolution:=AValue;
  UpdateFont;
end;

procedure TBGRAVectorizedFont.SetOrientation(AValue: single);
begin
  if FOrientation=AValue then Exit;
  FOrientation:=AValue;
  UpdateMatrix;
end;

procedure TBGRAVectorizedFont.SetItalicSlope(AValue: single);
begin
  if FItalicSlope=AValue then Exit;
  FItalicSlope:=AValue;
  UpdateMatrix;
end;

procedure TBGRAVectorizedFont.SetQuadraticCurves(AValue: boolean);
begin
  if FQuadraticCurves=AValue then Exit;
  FQuadraticCurves:=AValue;
end;

procedure TBGRAVectorizedFont.SetFontMatrix(AValue: TAffineMatrix);
begin
  FFontMatrix:=AValue;
  UpdateMatrix;
end;

procedure TBGRAVectorizedFont.SetFullHeight(AValue: single);
begin
  if FFullHeight=AValue then Exit;
  FFullHeight:=AValue;
  UpdateMatrix;
end;

procedure TBGRAVectorizedFont.SetName(AValue: string);
begin
  if FName=AValue then Exit;
  FName:=AValue;
  UpdateFont;
end;

procedure TBGRAVectorizedFont.SetStyle(AValue: TFontStyles);
begin
  if FStyle=AValue then Exit;
  FStyle:=AValue;
  UpdateFont;
end;

procedure TBGRAVectorizedFont.UpdateFont;
begin
  ClearGlyphs;
  FFont.Name := FName;
  FFont.Style := FStyle;
  FFont.Height := FontFullHeightSign * FResolution;
end;

procedure TBGRAVectorizedFont.UpdateMatrix;
begin
  TypeWriterMatrix := FFontMatrix*AffineMatrixRotationDeg(-Orientation*0.1)*AffineMatrixScale(FFullHeight,FFullHeight)*AffineMatrixLinear(PointF(1,0),PointF(-FItalicSlope,1));
end;

constructor TBGRAVectorizedFont.Create;
begin
  inherited Create;
  FName := 'Arial';
  FStyle := [];
  FFontMatrix := AffineMatrixIdentity;
  FOrientation := 0;
  FResolution := 100;
  FFont := TFont.Create;
  FBuffer := BGRABitmapFactory.Create;
  FFullHeight := 20;
  FItalicSlope := 0;
  UpdateFont;
  UpdateMatrix;
  FWordBreakHandler:= nil;
end;

destructor TBGRAVectorizedFont.Destroy;
begin
  FFont.Free;
  FBuffer.Free;
  inherited Destroy;
end;

function TBGRAVectorizedFont.GetGlyphSize(AIdentifier: string): TPointF;
var g: TBGRAGlyph;
begin
  g := GetGlyph(AIdentifier);
  if g = nil then result := EmptyPointF else
    result := PointF(g.Width*FullHeight,g.Height*FullHeight);
end;

function TBGRAVectorizedFont.GetTextGlyphSizes(AText: string): TGlyphSizes;
var
  pstr: pchar;
  left,charlen: integer;
  nextchar: string;
  g: TBGRAGlyph;
  numChar: integer;
begin
  if AText = '' then
  begin
    result := nil;
    exit;
  end;
  setlength(result, UTF8Length(AText));
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
      result[numChar].Width := g.Width*FullHeight;
      result[numChar].Height := g.Height*FullHeight;
    end else
    begin
      result[numChar].Width := 0;
      result[numChar].Height := 0;
    end;
    inc(numChar);
  end;
end;

function TBGRAVectorizedFont.GetTextSize(AText: string): TPointF;
var
  pstr: pchar;
  left,charlen: integer;
  nextchar: string;
  g: TBGRAGlyph;
  gSizeY: single;
begin
  result := PointF(0,0);
  if AText = '' then exit else
  begin
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
        result.x += g.Width*FullHeight;
        gSizeY := g.Height*FullHeight;
        if gSizeY > result.y then result.Y := gSizeY;
      end;
    end;
  end;
end;

procedure TBGRAVectorizedFont.SplitText(var AText: string; AMaxWidth: single;
  out ARemains: string);
var
  pstr: pchar;
  left,charlen: integer;
  nextchar: string;
  g: TBGRAGlyph;
  totalWidth: single;
  firstChar: boolean;
begin
  totalWidth := 0;
  if AText = '' then
  begin
    ARemains := '';
    exit;
  end else
  begin
    pstr := @AText[1];
    left := length(AText);
    firstChar := true;
    while left > 0 do
    begin
      charlen := UTF8CharacterLength(pstr);
      setlength(nextchar, charlen);
      move(pstr^, nextchar[1], charlen);
      inc(pstr,charlen);

      g := GetGlyph(nextchar);
      if g <> nil then
      begin
        totalWidth += g.Width*FullHeight;
        if (totalWidth > AMaxWidth) and not firstChar then
        begin
          ARemains:= copy(AText,length(AText)-left+1,left);
          AText := copy(AText, 1, length(AText)-left);
          if Assigned(FWordBreakHandler) then
            FWordBreakHandler(AText,ARemains) else
              DefaultWordBreakHandler(AText,ARemains);
          exit;
        end;
      end;

      dec(left,charlen);
      firstChar := false;
    end;
  end;
  ARemains := ''; //no split
end;

procedure TBGRAVectorizedFont.DrawTextWordBreak(ADest: TBGRACanvas2D;
  AText: string; X, Y, MaxWidth: Single; AAlign: TBGRATypeWriterAlignment);
var ARemains: string;
  step: TPointF;
  lines: TStringList;
  i: integer;
  lineShift: single;
  oldItalicSlope: single;
  lineAlignment: TBGRATypeWriterAlignment;
begin
  if (AText = '') or (MaxWidth <= 0) then exit;

  oldItalicSlope:= ItalicSlope;
  ItalicSlope := 0;
  step := TypeWriterMatrix*PointF(0,1);
  ItalicSlope := oldItalicSlope;

  if AAlign in[twaTop,twaMiddle,twaBottom] then
    lineAlignment := twaMiddle
  else if AAlign in[twaTopLeft,twaLeft,twaBottomLeft] then
  begin
    if ItalicSlope < 0 then
      lineAlignment:= twaTopLeft
    else
      lineAlignment := twaBottomLeft;
  end else
  begin
    if ItalicSlope < 0 then
      lineAlignment := twaBottomRight
    else
      lineAlignment := twaTopRight;
  end;

  if AAlign in[twaTopLeft,twaTop,twaTopRight] then
  begin
    case lineAlignment of
    twaMiddle: lineShift := 0.5;
    twaBottomLeft,twaBottomRight: lineShift := 1;
    twaTopRight,twaTopLeft : lineShift := 0;
    end;
    X += step.X*lineShift;
    Y += step.Y*lineShift;
    repeat
      SplitText(AText, MaxWidth, ARemains);
      DrawText(ADest,AText,X,Y,lineAlignment);
      AText := ARemains;
      X+= step.X;
      Y+= step.Y;
    until ARemains = '';
  end else
  begin
    lines := TStringList.Create;
    repeat
      SplitText(AText, MaxWidth, ARemains);
      lines.Add(AText);
      AText := ARemains;
    until ARemains = '';
    if AAlign in[twaLeft,twaMiddle,twaRight] then lineShift := lines.Count/2-0.5
    else if AAlign in[twaBottomLeft,twaBottom,twaBottomRight] then lineShift := lines.Count-0.5
    else lineShift := -0.5;

    case lineAlignment of
    twaMiddle: ;
    twaBottomLeft,twaBottomRight: lineShift -= 0.5;
    twaTopRight,twaTopLeft : lineShift += 0.5;
    end;

    X -= step.X*lineShift;
    Y -= step.Y*lineShift;
    for i := 0 to lines.Count-1 do
    begin
      DrawText(ADest,lines[i],X,Y,lineAlignment);
      X+= step.X;
      Y+= step.Y;
    end;
    lines.Free;
  end;
end;

procedure TBGRAVectorizedFont.DrawTextRect(ADest: TBGRACanvas2D; AText: string;
  X1, Y1, X2, Y2: Single; AAlign: TBGRATypeWriterAlignment);
var X,Y: single;
  oldOrientation: single;
begin
  if X2 <= X1 then exit;
  if AAlign in[twaTopLeft,twaTop,twaTopRight] then Y := Y1 else
  if AAlign in[twaLeft,twaMiddle,twaRight] then Y := (Y1+Y2)/2 else
  if AAlign in[twaBottomLeft,twaBottom,twaBottomRight] then Y := Y2;
  if AAlign in[twaLeft,twaTopLeft,twaBottomLeft] then X := X1 else
  if AAlign in[twaTop,twaMiddle,twaBottom] then X := (X1+X2)/2 else
  if AAlign in[twaRight,twaTopRight,twaBottomRight] then X := X2;
  oldOrientation:= Orientation;
  Orientation:= 0;
  DrawTextWordBreak(ADest,AText,X,Y,X2-X1,AAlign);
  Orientation:= oldOrientation;
end;

procedure TBGRAVectorizedFont.DrawTextRect(ADest: TBGRACanvas2D; AText: string;
  ATopLeft, ABottomRight: TPointF; AAlign: TBGRATypeWriterAlignment);
begin
  DrawTextRect(ADest,AText,ATopLeft.X,ATopLeft.Y,ABottomRight.X,ABottomRight.Y,AAlign);
end;

function TBGRAVectorizedFont.GetTextWordBreakGlyphBoxes(AText: string; X, Y,
  MaxWidth: Single; AAlign: TBGRATypeWriterAlignment): TGlyphBoxes;
var ARemains: string;
  step: TPointF;
  lines: TStringList;
  i: integer;
  lineShift: single;
  oldItalicSlope: single;
  tempArray: array of TGlyphBoxes;
  tempPos,j: integer;
  lineAlignment: TBGRATypeWriterAlignment;
begin
  result := nil;
  if AText = '' then exit;

  oldItalicSlope:= ItalicSlope;
  ItalicSlope := 0;
  step := TypeWriterMatrix*PointF(0,1);
  ItalicSlope := oldItalicSlope;

  if AAlign in[twaTop,twaMiddle,twaBottom] then
    lineAlignment := twaMiddle
  else if AAlign in[twaTopLeft,twaLeft,twaBottomLeft] then
  begin
    if ItalicSlope < 0 then
      lineAlignment:= twaTopLeft
    else
      lineAlignment := twaBottomLeft;
  end else
  begin
    if ItalicSlope < 0 then
      lineAlignment := twaBottomRight
    else
      lineAlignment := twaTopRight;
  end;

  lines := TStringList.Create;
  repeat
    SplitText(AText, MaxWidth, ARemains);
    lines.Add(AText);
    AText := ARemains;
  until ARemains = '';

  if AAlign in[twaLeft,twaMiddle,twaRight] then lineShift := lines.Count/2-0.5
  else if AAlign in[twaBottomLeft,twaBottom,twaBottomRight] then lineShift := lines.Count-0.5
  else lineShift := -0.5;

  case lineAlignment of
  twaMiddle: ;
  twaBottomLeft, twaBottomRight: lineShift -= 0.5;
  twaTopRight,twaTopLeft : lineShift += 0.5;
  end;

  X -= step.X*lineShift;
  Y -= step.Y*lineShift;
  setlength(tempArray, lines.Count);
  tempPos := 0;
  for i := 0 to lines.Count-1 do
  begin
    tempArray[i] := GetTextGlyphBoxes(lines[i],X,Y,lineAlignment);
    inc(tempPos, length(tempArray[i]));
    X+= step.X;
    Y+= step.Y;
  end;
  lines.Free;
  setlength(result, tempPos);
  tempPos := 0;
  for i := 0 to high(tempArray) do
    for j := 0 to high(tempArray[i]) do
    begin
      result[tempPos] := tempArray[i][j];
      inc(tempPos);
    end;
end;

function TBGRAVectorizedFont.GetTextRectGlyphBoxes(AText: string; X1, Y1, X2,
  Y2: Single; AAlign: TBGRATypeWriterAlignment): TGlyphBoxes;
var X,Y,oldOrientation: single;
begin
  if X2 <= X1 then
  begin
    result := nil;
    exit;
  end;
  if AAlign in[twaTopLeft,twaTop,twaTopRight] then Y := Y1 else
  if AAlign in[twaLeft,twaMiddle,twaRight] then Y := (Y1+Y2)/2 else
  if AAlign in[twaBottomLeft,twaBottom,twaBottomRight] then Y := Y2;
  if AAlign in[twaLeft,twaTopLeft,twaBottomLeft] then X := X1 else
  if AAlign in[twaTop,twaMiddle,twaBottom] then X := (X1+X2)/2 else
  if AAlign in[twaRight,twaTopRight,twaBottomRight] then X := X2;
  oldOrientation:= Orientation;
  Orientation:= 0;
  result := GetTextWordBreakGlyphBoxes(AText,X,Y,X2-X1,AAlign);
  Orientation:= oldOrientation;
end;

function TBGRAVectorizedFont.GetTextRectGlyphBoxes(AText: string; ATopLeft,
  ABottomRight: TPointF; AAlign: TBGRATypeWriterAlignment): TGlyphBoxes;
begin
  result := GetTextRectGlyphBoxes(AText,ATopLeft.X,ATopLeft.Y,ABottomRight.X,ABottomRight.Y,AAlign);
end;

function TBGRAVectorizedFont.GetGlyph(AIdentifier: string): TBGRAGlyph;
var size: TSize;
  g: TBGRAPolygonalGlyph;
begin
  Result:=inherited GetGlyph(AIdentifier);
  if (result = nil) and (FResolution > 0) then
  begin
    g := TBGRAPolygonalGlyph.Create(AIdentifier);
    size := BGRATextSize(FFont, fqSystem, AIdentifier, 1);
    FBuffer.SetSize(size.cx+size.cy,size.cy);
    FBuffer.Fill(BGRAWhite);
    FBuffer.Canvas.Font := FFont;
    FBuffer.Canvas.Font.Quality := fqNonAntialiased;
    FBuffer.Canvas.Font.Color := clBlack;
    FBuffer.Canvas.TextOut(size.cy div 2,0,AIdentifier);
    g.SetPoints(VectorizeMonochrome(FBuffer,1/FResolution,False));
    g.QuadraticCurves := FQuadraticCurves and (OutlineMode in[twoPath, twoFill]);
    g.Width := size.cx/size.cy;
    g.Height := 1;
    g.Offset := PointF(-0.5,0);
    SetGlyph(AIdentifier,g);
    result := g;
  end else
  if (result <> nil) and (result is TBGRAPolygonalGlyph) then
    TBGRAPolygonalGlyph(result).QuadraticCurves := FQuadraticCurves and (OutlineMode in[twoPath, twoFill]);
end;

procedure TBGRAVectorizedFont.DefaultWordBreakHandler(var ABefore,AAfter: string);
var p: integer;
begin
  if (AAfter <> '') and (ABefore <> '') and (AAfter[1]<> ' ') and (ABefore[length(ABefore)] <> ' ') then
  begin
    p := length(ABefore);
    while (p > 1) and (ABefore[p-1] <> ' ') do dec(p);
    if p > 1 then //can put the word after
    begin
      AAfter := copy(ABefore,p,length(ABefore)-p+1)+AAfter;
      ABefore := copy(ABefore,1,p-1);
    end else
    begin //cannot put the word after, so before

    end;
  end;
  while (ABefore <> '') and (ABefore[length(ABefore)] =' ') do delete(ABefore,length(ABefore),1);
  while (AAfter <> '') and (AAfter[1] =' ') do delete(AAfter,1,1);
end;

end.

