unit BGRAVectorize;

{$mode objfpc}{$H+}

interface

{
  Font rendering units : BGRAText, BGRATextFX, BGRAVectorize, BGRAFreeType

  This unit provides vectorizers :
  - VectorizeMonochrome function vectorizes a back'n'white image
  - TBGRAVectorizedFont allows to vectorize and to load vectorized font and draw them

  TBGRAVectorizedFontRenderer class works like other font renderers, i.e., it can
  be assigned to the FontRenderer property. You can use it in two different modes :
  - if you supply a directory, it will look for *.glyphs files in it to load fonts
  - if you don't supply a directory, fonts will be vectorized from LCL

  Note that unless you want to supply your own glyphs files, you don't need
  to use explicitely this renderer, because TBGRATextEffectFontRenderer will
  make use of it if necessary, according to effects parameters used.
}

uses
  Types, Classes, SysUtils, Graphics, BGRABitmapTypes, BGRATypewriter, BGRATransform, BGRACanvas2D, BGRAText;

//vectorize a monochrome bitmap
function VectorizeMonochrome(ASource: TBGRACustomBitmap; zoom: single; PixelCenteredCoordinates: boolean): ArrayOfTPointF;

type
  TBGRAVectorizedFont = class;

  //this is the class to assign to FontRenderer property of TBGRABitmap
  { TBGRAVectorizedFontRenderer }

  TBGRAVectorizedFontRenderer = class(TBGRACustomFontRenderer)
  protected
    FVectorizedFontArray: array of record
        FontName: string;
        FontStyle: TFontStyles;
        VectorizedFont: TBGRAVectorizedFont;
      end;
    FVectorizedFont: TBGRAVectorizedFont;
    FCanvas2D: TBGRACanvas2D;
    FDirectoryUTF8: string;
    function OutlineActuallyVisible: boolean;
    procedure UpdateFont;
    function GetCanvas2D(ASurface: TBGRACustomBitmap): TBGRACanvas2D;
    procedure InternalTextRect(ADest: TBGRACustomBitmap; ARect: TRect; x, y: integer; sUTF8: string; style: TTextStyle; c: TBGRAPixel; texture: IBGRAScanner);
    procedure Init;
  public
    MaxFontResolution: integer;

    OutlineVisible: boolean;
    OutlineWidth: single;
    OutlineColor: TBGRAPixel;
    OutlineTexture: IBGRAScanner;
    OuterOutlineOnly: boolean;

    ShadowVisible: boolean;
    ShadowColor: TBGRAPixel;
    ShadowRadius: integer;
    ShadowOffset: TPoint;

    constructor Create;
    constructor Create(ADirectoryUTF8: string);
    function GetFontPixelMetric: TFontPixelMetric; override;
    procedure TextOutAngle(ADest: TBGRACustomBitmap; x, y: single; orientation: integer; s: string; c: TBGRAPixel; align: TAlignment); override;
    procedure TextOutAngle(ADest: TBGRACustomBitmap; x, y: single; orientation: integer; s: string; texture: IBGRAScanner; align: TAlignment); override;
    procedure TextOut(ADest: TBGRACustomBitmap; x, y: single; s: string; texture: IBGRAScanner; align: TAlignment); override;
    procedure TextOut(ADest: TBGRACustomBitmap; x, y: single; s: string; c: TBGRAPixel; align: TAlignment); override;
    procedure TextRect(ADest: TBGRACustomBitmap; ARect: TRect; x, y: integer; s: string; style: TTextStyle; c: TBGRAPixel); override;
    procedure TextRect(ADest: TBGRACustomBitmap; ARect: TRect; x, y: integer; s: string; style: TTextStyle; texture: IBGRAScanner); override;
    procedure CopyTextPathTo(ADest: IBGRAPath; x, y: single; s: string; align: TAlignment); override;
    function TextSize(s: string): TSize; override;
    destructor Destroy; override;
  end;

  TGlyphSizes = array of record
            Glyph: String;
            Width,Height: single;
  end;

  TBGRAVectorizedFontHeader = record
    Name: string;
    Style: TFontStyles;
    EmHeightRatio: single;
    Resolution: integer;
    PixelMetric: TFontPixelMetric;
  end;
  TBGRAGlyphsInfo = record
    Name: string;
    Style: TFontStyles;
    NbGlyphs: integer;
  end;

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
    FDirectory: string;
    FDirectoryContent: array of record
      Filename: string;
      FontName: string;
      FontStyle: TFontStyles;
    end;
    FFontEmHeightRatioComputed: boolean;
    FFontEmHeightRatio: single;
    FFontPixelMetric: TFontPixelMetric;
    FFontPixelMetricComputed: boolean;
    FFontFound: boolean;
    function GetEmHeight: single;
    function GetFontPixelMetric: TFontPixelMetric;
    function GetLCLHeight: single;
    function GetVectorizeLCL: boolean;
    procedure SetEmHeight(AValue: single);
    procedure SetItalicSlope(AValue: single);
    procedure SetLCLHeight(AValue: single);
    procedure SetOrientation(AValue: single);
    procedure SetQuadraticCurves(AValue: boolean);
    procedure SetResolution(AValue: integer);
    procedure SetFontMatrix(AValue: TAffineMatrix);
    procedure SetFullHeight(AValue: single);
    procedure SetName(AValue: string);
    procedure SetStyle(AValue: TFontStyles);
    function GetFontEmHeightRatio: single;
    procedure SetVectorizeLCL(AValue: boolean);
  protected
    procedure UpdateFont;
    procedure UpdateMatrix;
    function GetGlyph(AIdentifier: string): TBGRAGlyph; override;
    procedure DefaultWordBreakHandler(var ABefore, AAfter: string);
    procedure Init(AVectorize: boolean);
    function CustomHeaderSize: integer; override;
    procedure WriteCustomHeader(AStream: TStream); override;
    procedure ReadAdditionalHeader(AStream: TStream); override;
    function ReadVectorizedFontHeader(AStream: TStream): TBGRAVectorizedFontHeader;
    function HeaderName: string; override;
    procedure SetDirectory(const AValue: string);
  public
    UnderlineDecoration,StrikeOutDecoration: boolean;
    constructor Create;
    constructor Create(AVectorizeLCL: boolean);
    destructor Destroy; override;
    function GetGlyphSize(AIdentifier:string): TPointF;
    function GetTextGlyphSizes(AText:string): TGlyphSizes;
    function GetTextSize(AText:string): TPointF;
    procedure SplitText(var ATextUTF8: string; AMaxWidth: single; out ARemainsUTF8: string);
    procedure DrawText(ADest: TBGRACanvas2D; ATextUTF8: string; X, Y: Single; AAlign: TBGRATypeWriterAlignment=twaTopLeft); override;
    procedure CopyTextPathTo(ADest: IBGRAPath; ATextUTF8: string; X, Y: Single;
      AAlign: TBGRATypeWriterAlignment=twaTopLeft); override;
    procedure DrawTextWordBreak(ADest: TBGRACanvas2D; ATextUTF8: string; X, Y, MaxWidth: Single; AAlign: TBGRATypeWriterAlignment=twaTopLeft);
    procedure DrawTextRect(ADest: TBGRACanvas2D; ATextUTF8: string; X1,Y1,X2,Y2: Single; AAlign: TBGRATypeWriterAlignment=twaTopLeft);
    procedure DrawTextRect(ADest: TBGRACanvas2D; ATextUTF8: string; ATopLeft,ABottomRight: TPointF; AAlign: TBGRATypeWriterAlignment=twaTopLeft);
    function GetTextWordBreakGlyphBoxes(ATextUTF8: string; X,Y, MaxWidth: Single; AAlign: TBGRATypeWriterAlignment = twaTopLeft): TGlyphBoxes;
    function GetTextRectGlyphBoxes(ATextUTF8: string; X1,Y1,X2,Y2: Single; AAlign: TBGRATypeWriterAlignment=twaTopLeft): TGlyphBoxes;
    function GetTextRectGlyphBoxes(ATextUTF8: string; ATopLeft,ABottomRight: TPointF; AAlign: TBGRATypeWriterAlignment=twaTopLeft): TGlyphBoxes;
    procedure UpdateDirectory;
    function LoadGlyphsInfo(AFilenameUTF8: string): TBGRAGlyphsInfo;

    property Resolution: integer read FResolution write SetResolution;
    property Style: TFontStyles read FStyle write SetStyle;
    property Name: string read FName write SetName;
    property LCLHeight: single read GetLCLHeight write SetLCLHeight;
    property EmHeight: single read GetEmHeight write SetEmHeight;
    property FullHeight: single read FFullHeight write SetFullHeight;
    property FontMatrix: TAffineMatrix read FFontMatrix write SetFontMatrix;
    property Orientation: single read FOrientation write SetOrientation;
    property QuadraticCurves: boolean read FQuadraticCurves write SetQuadraticCurves;
    property ItalicSlope: single read FItalicSlope write SetItalicSlope;
    property OnWordBreak: TWordBreakHandler read FWordBreakHandler write FWordBreakHandler;
    property Directory: string read FDirectory write SetDirectory;
    property FontEmHeightRatio: single read GetFontEmHeightRatio;
    property FontPixelMetric: TFontPixelMetric read GetFontPixelMetric;
    property FontFound: boolean read FFontFound;
    property VectorizeLCL: boolean read GetVectorizeLCL write SetVectorizeLCL;
  end;

implementation

uses BGRAUTF8;

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
        if (not cur[6] and not cur[9] and not cur[8] and ((ASource.getPixel(x-1,y-2).green <= 128) or (ASource.getPixel(x+2,y+1).green <= 128)) ) or
          (not cur[8] and not cur[7] and not cur[4] and ((ASource.getPixel(x-2,y+1).green <= 128) or (ASource.getPixel(x+1,y-2).green <= 128)) ) or
          (not cur[4] and not cur[1] and not cur[2] and ((ASource.getPixel(x+1,y+2).green <= 128) or (ASource.getPixel(x-2,y-1).green <= 128)) ) or
          (not cur[2] and not cur[3] and not cur[6] and ((ASource.getPixel(x-1,y+2).green <= 128) or (ASource.getPixel(x+2,y-1).green <= 128)) ) then
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

{ TBGRAVectorizedFontRenderer }

function TBGRAVectorizedFontRenderer.OutlineActuallyVisible: boolean;
begin
  result := OutlineVisible and (abs(OutlineWidth) > 0) and (OutlineColor.Alpha <> 0) or (OutlineTexture <> nil);
end;

procedure TBGRAVectorizedFontRenderer.UpdateFont;
var i,neededResolution: integer;
begin
  FVectorizedFont := nil;
  FontName := Trim(FontName);
  for i := 0 to high(FVectorizedFontArray) do
    if (CompareText(FVectorizedFontArray[i].FontName,FontName)=0) and
      (FVectorizedFontArray[i].FontStyle = FontStyle) then
    begin
      FVectorizedFont := FVectorizedFontArray[i].VectorizedFont;
      break;
    end;

  if FVectorizedFont = nil then
  begin
    FVectorizedFont:= TBGRAVectorizedFont.Create(False);
    FVectorizedFont.Name := FontName;
    FVectorizedFont.Style := FontStyle;
    FVectorizedFont.Directory := FDirectoryUTF8;
    if not FVectorizedFont.FontFound and LCLFontAvailable then
      FVectorizedFont.VectorizeLCL := True;
    Setlength(FVectorizedFontArray,length(FVectorizedFontArray)+1);
    FVectorizedFontArray[high(FVectorizedFontArray)].FontName := FontName;
    FVectorizedFontArray[high(FVectorizedFontArray)].FontStyle := FontStyle;
    FVectorizedFontArray[high(FVectorizedFontArray)].VectorizedFont := FVectorizedFont;
  end;
  if FontEmHeight > 0 then
    FVectorizedFont.EmHeight := FontEmHeight
  else
    FVectorizedFont.FullHeight:= -FontEmHeight;
  if OutlineActuallyVisible then
  begin
    if OuterOutlineOnly then
      FVectorizedFont.OutlineMode := twoFillOverStroke
    else
      FVectorizedFont.OutlineMode := twoStrokeOverFill;
    FVectorizedFont.QuadraticCurves := False;
  end
  else
  begin
    FVectorizedFont.OutlineMode := twoFill;
    FVectorizedFont.QuadraticCurves := FVectorizedFont.FullHeight > FVectorizedFont.Resolution*0.8;
  end;
  if FVectorizedFont.VectorizeLCL then
  begin
    neededResolution := trunc((FVectorizedFont.FullHeight+80)/50)*50;
    if neededResolution > MaxFontResolution then neededResolution := MaxFontResolution;
    if FVectorizedFont.Resolution < neededResolution then FVectorizedFont.Resolution:= neededResolution;
  end;
end;

function TBGRAVectorizedFontRenderer.GetCanvas2D(ASurface: TBGRACustomBitmap
  ): TBGRACanvas2D;
begin
  if (FCanvas2D = nil) or (FCanvas2D.surface <> ASurface) then
  begin
    FCanvas2D.Free;
    FCanvas2D := TBGRACanvas2D.Create(ASurface);
  end;
  result := FCanvas2D;
  FCanvas2D.antialiasing:= FontQuality in[fqFineAntialiasing,fqFineClearTypeBGR,fqFineClearTypeRGB];
  if OutlineTexture <> nil then
    FCanvas2D.strokeStyle(OutlineTexture)
  else
    FCanvas2D.strokeStyle(OutlineColor);
  FCanvas2D.lineWidth := abs(OutlineWidth);
  if not ShadowVisible then
    FCanvas2D.shadowColor(BGRAPixelTransparent)
  else
  begin
    FCanvas2D.shadowColor(ShadowColor);
    FCanvas2D.shadowBlur:= ShadowRadius;
    FCanvas2D.shadowOffset := PointF(ShadowOffset.X,ShadowOffset.Y);
  end;
end;

procedure TBGRAVectorizedFontRenderer.InternalTextRect(
  ADest: TBGRACustomBitmap; ARect: TRect; x, y: integer; sUTF8: string;
  style: TTextStyle; c: TBGRAPixel; texture: IBGRAScanner);
var
  twAlign : TBGRATypeWriterAlignment;
  c2D: TBGRACanvas2D;
  intersectedClip,previousClip: TRect;
begin
  previousClip := ADest.ClipRect;
  if style.Clipping then
  begin
    intersectedClip := rect(0,0,0,0);
    if not IntersectRect(intersectedClip, previousClip, ARect) then exit;
    ADest.ClipRect := intersectedClip;
  end;
  UpdateFont;
  FVectorizedFont.Orientation := 0;
  case style.Alignment of
    taCenter: case style.Layout of
              tlCenter: twAlign := twaMiddle;
              tlBottom: twAlign := twaBottom;
              else twAlign:= twaTop;
              end;
    taRightJustify:
              case style.Layout of
              tlCenter: twAlign := twaRight;
              tlBottom: twAlign := twaBottomRight;
              else twAlign := twaTopRight;
              end;
    else
              case style.Layout of
              tlCenter: twAlign := twaLeft;
              tlBottom: twAlign := twaBottomLeft;
              else twAlign:= twaTopLeft;
              end;
  end;
  c2D := GetCanvas2D(ADest);
  if texture = nil then
    c2D.fillStyle(c)
  else
    c2D.fillStyle(texture);
  if style.Wordbreak then
    FVectorizedFont.DrawTextRect(c2D, sUTF8, x-0.5,y-0.5,ARect.Right-0.5,ARect.Bottom-0.5, twAlign)
  else
  begin
    case style.Layout of
    tlCenter: y := (ARect.Top+ARect.Bottom) div 2;
    tlBottom: y := ARect.Bottom;
    end;
    case style.Alignment of
    taCenter: FVectorizedFont.DrawText(c2D, sUTF8, (ARect.Left+ARect.Right-1)/2,y-0.5, twAlign);
    taRightJustify: FVectorizedFont.DrawText(c2D, sUTF8, ARect.Right-0.5,y-0.5, twAlign);
    else
      FVectorizedFont.DrawText(c2D, sUTF8, x-0.5,y-0.5, twAlign);
    end;
  end;
  if style.Clipping then
    ADest.ClipRect := previousClip;
end;

procedure TBGRAVectorizedFontRenderer.Init;
begin
  FVectorizedFontArray := nil;
  FDirectoryUTF8 := '';

  OutlineVisible:= True;
  OutlineColor := BGRAPixelTransparent;
  OuterOutlineOnly := false;

  ShadowColor := BGRABlack;
  ShadowVisible := false;
  ShadowOffset := Point(5,5);
  ShadowRadius := 5;

  MaxFontResolution := 300;
end;

constructor TBGRAVectorizedFontRenderer.Create;
begin
  Init;
end;

constructor TBGRAVectorizedFontRenderer.Create(ADirectoryUTF8: string);
begin
  Init;
  FDirectoryUTF8 := ADirectoryUTF8;
end;

function TBGRAVectorizedFontRenderer.GetFontPixelMetric: TFontPixelMetric;
var factor: single;
begin
  UpdateFont;
  result := FVectorizedFont.FontPixelMetric;
  if FVectorizedFont.Resolution > 0 then
  begin
    factor := FVectorizedFont.FullHeight/FVectorizedFont.Resolution;
    result.Baseline := round(result.Baseline*factor);
    result.CapLine := round(result.CapLine*factor);
    result.Lineheight := round(result.Lineheight*factor);
    result.DescentLine := round(result.DescentLine*factor);
    result.xLine := round(result.xLine*factor);
  end;
end;

procedure TBGRAVectorizedFontRenderer.TextOutAngle(ADest: TBGRACustomBitmap; x,
  y: single; orientation: integer; s: string; c: TBGRAPixel; align: TAlignment);
var
  twAlign : TBGRATypeWriterAlignment;
  c2D: TBGRACanvas2D;
  ofs: TPointF;
begin
  UpdateFont;
  FVectorizedFont.Orientation := orientation;
  case align of
    taCenter: twAlign:= twaMiddle;
    taRightJustify: twAlign := twaRight;
    else twAlign:= twaLeft;
  end;
  c2D := GetCanvas2D(ADest);
  c2D.fillStyle(c);
  ofs := PointF(x,y);
  ofs += AffineMatrixRotationDeg(-orientation*0.1)*PointF(0,FVectorizedFont.FullHeight*0.5);
  FVectorizedFont.DrawText(c2D, s, ofs.x,ofs.y, twAlign);
end;

procedure TBGRAVectorizedFontRenderer.TextOutAngle(ADest: TBGRACustomBitmap; x,
  y: single; orientation: integer; s: string; texture: IBGRAScanner;
  align: TAlignment);
var
  twAlign : TBGRATypeWriterAlignment;
  c2D: TBGRACanvas2D;
begin
  UpdateFont;
  FVectorizedFont.Orientation := orientation;
  case align of
    taCenter: twAlign:= twaTop;
    taRightJustify: twAlign := twaTopRight;
    else twAlign:= twaTopLeft;
  end;
  c2D := GetCanvas2D(ADest);
  c2D.fillStyle(texture);
  FVectorizedFont.DrawText(c2D, s, x,y, twAlign);
end;

procedure TBGRAVectorizedFontRenderer.TextOut(ADest: TBGRACustomBitmap; x,
  y: single; s: string; texture: IBGRAScanner; align: TAlignment);
begin
  TextOutAngle(ADest,x,y,FontOrientation,s,texture,align);
end;

procedure TBGRAVectorizedFontRenderer.TextOut(ADest: TBGRACustomBitmap; x,
  y: single; s: string; c: TBGRAPixel; align: TAlignment);
begin
  TextOutAngle(ADest,x,y,FontOrientation,s,c,align);
end;

procedure TBGRAVectorizedFontRenderer.TextRect(ADest: TBGRACustomBitmap;
  ARect: TRect; x, y: integer; s: string; style: TTextStyle; c: TBGRAPixel);
begin
  InternalTextRect(ADest,ARect,x,y,s,style,c,nil);
end;

procedure TBGRAVectorizedFontRenderer.TextRect(ADest: TBGRACustomBitmap;
  ARect: TRect; x, y: integer; s: string; style: TTextStyle;
  texture: IBGRAScanner);
begin
  InternalTextRect(ADest,ARect,x,y,s,style,BGRAPixelTransparent,texture);
end;

procedure TBGRAVectorizedFontRenderer.CopyTextPathTo(ADest: IBGRAPath; x, y: single; s: string; align: TAlignment);
var
  twAlign : TBGRATypeWriterAlignment;
  ofs: TPointF;
begin
  UpdateFont;
  FVectorizedFont.Orientation := 0;
  case align of
    taCenter: twAlign:= twaMiddle;
    taRightJustify: twAlign := twaRight;
    else twAlign:= twaLeft;
  end;
  ofs := PointF(x,y);
  ofs += PointF(0,FVectorizedFont.FullHeight*0.5);
  FVectorizedFont.CopyTextPathTo(ADest, s, ofs.x,ofs.y, twAlign);
end;

function TBGRAVectorizedFontRenderer.TextSize(s: string): TSize;
var sizeF: TPointF;
begin
  UpdateFont;
  sizeF := FVectorizedFont.GetTextSize(s);
  result.cx := round(sizeF.x);
  result.cy := round(sizeF.y);
end;

destructor TBGRAVectorizedFontRenderer.Destroy;
var i: integer;
begin
  FCanvas2D.Free;
  for i := 0 to high(FVectorizedFontArray) do
    FVectorizedFontArray[i].VectorizedFont.Free;
  FVectorizedFontArray := nil;
  inherited Destroy;
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

procedure TBGRAVectorizedFont.SetLCLHeight(AValue: single);
begin
  if (AValue > 0) xor (FontEmHeightSign < 0) then
    EmHeight := abs(AValue)
  else
    FullHeight := abs(AValue);
end;

function TBGRAVectorizedFont.GetEmHeight: single;
begin
  result := FullHeight * FontEmHeightRatio;
end;

function TBGRAVectorizedFont.GetFontPixelMetric: TFontPixelMetric;
begin
  if not FFontPixelMetricComputed and (FFont <> nil) then
  begin
    FFontPixelMetric := BGRAText.GetLCLFontPixelMetric(FFont);
    FFontPixelMetricComputed := true;
  end;
  result := FFontPixelMetric;
end;

function TBGRAVectorizedFont.GetLCLHeight: single;
begin
  result := FullHeight * FontFullHeightSign;
end;

function TBGRAVectorizedFont.GetVectorizeLCL: boolean;
begin
  result := FFont <> nil;
end;

procedure TBGRAVectorizedFont.SetEmHeight(AValue: single);
begin
  if FontEmHeightRatio > 0 then
    FullHeight := AValue / FontEmHeightRatio;
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

function TBGRAVectorizedFont.GetFontEmHeightRatio: single;
var
  lEmHeight, lFullHeight: single;
  OldHeight: integer;
begin
  if not FFontEmHeightRatioComputed then
  begin
    if FFont <> nil then
    begin
      OldHeight := FFont.Height;
      FFont.Height := FontEmHeightSign * 100;
      lEmHeight := BGRATextSize(FFont, fqSystem, 'Hg', 1).cy;
      FFont.Height := FontFullHeightSign * 100;
      lFullHeight := BGRATextSize(FFont, fqSystem, 'Hg', 1).cy;
      if lEmHeight = 0 then
        FFontEmHeightRatio := 1
      else
        FFontEmHeightRatio := lFullHeight/lEmHeight;
      FFontEmHeightRatioComputed := true;
      FFont.Height := OldHeight;
    end else
    begin
      result := 1;
      exit;
    end;
  end;
  result := FFontEmHeightRatio;
end;

procedure TBGRAVectorizedFont.SetVectorizeLCL(AValue: boolean);
begin
  if AValue then
  begin
    if FFont = nil then
      FFont := TFont.Create;
  end else
  begin
    if FFont <> nil then
      FreeAndNil(FFont);
  end;
  UpdateFont;
end;

procedure TBGRAVectorizedFont.UpdateFont;
var i: integer;
  bestIndex, bestDistance: integer;
  distance: integer;
begin
  if FFont <> nil then
  begin
    ClearGlyphs;
    FFont.Name := FName;
    FFont.Style := FStyle;
    FFont.Height := FontFullHeightSign * FResolution;
    FFontEmHeightRatio := 1;
    FFontEmHeightRatioComputed := false;
    fillchar(FFontPixelMetric,sizeof(FFontPixelMetric),0);
    FFontPixelMetricComputed := false;
    FFontFound := True;
  end else
  begin
    bestIndex := -1;
    bestDistance := 1000;
    for i := 0 to high(FDirectoryContent) do
    begin
      if CompareText(FDirectoryContent[i].FontName,FName) = 0 then
      begin
        distance := 0;
        if (fsBold in FDirectoryContent[i].FontStyle) xor (fsBold in FStyle) then distance += 10;
        if (fsItalic in FDirectoryContent[i].FontStyle) xor (fsItalic in FStyle) then distance += 5;
        if (fsStrikeOut in FDirectoryContent[i].FontStyle) xor (fsStrikeOut in FStyle) then distance += 1;
        if (fsUnderline in FDirectoryContent[i].FontStyle) xor (fsUnderline in FStyle) then distance += 1;
        if (bestIndex = -1) or (distance < bestDistance) then
        begin
          bestIndex := i;
          bestDistance := distance;
          if FDirectoryContent[i].FontStyle = FStyle then break;
        end;
      end;
    end;
    if bestIndex <> -1 then
    begin
      if not (fsItalic in FDirectoryContent[bestIndex].FontStyle) and (fsItalic in FStyle) then
        ItalicSlope := 0.25
      else if (fsItalic in FDirectoryContent[bestIndex].FontStyle) and not (fsItalic in FStyle) then
        ItalicSlope := -0.25
      else
        ItalicSlope := 0;

      UnderlineDecoration := not (fsUnderline in FDirectoryContent[bestIndex].FontStyle) and (fsUnderline in FStyle);
      StrikeOutDecoration := not (fsStrikeOut in FDirectoryContent[bestIndex].FontStyle) and (fsStrikeOut in FStyle);

      ClearGlyphs;
      LoadGlyphsFromFile(FDirectoryContent[bestIndex].Filename);
      FFontFound := True;
    end else
      FFontFound := false;
  end;
end;

procedure TBGRAVectorizedFont.UpdateMatrix;
begin
  TypeWriterMatrix := FFontMatrix*AffineMatrixRotationDeg(-Orientation*0.1)*AffineMatrixScale(FFullHeight,FFullHeight)*AffineMatrixLinear(PointF(1,0),PointF(-FItalicSlope,1));
end;

constructor TBGRAVectorizedFont.Create;
begin
  inherited Create;
  Init(True);
end;

constructor TBGRAVectorizedFont.Create(AVectorizeLCL: boolean);
begin
  inherited Create;
  Init(AVectorizeLCL);
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

procedure TBGRAVectorizedFont.SplitText(var ATextUTF8: string; AMaxWidth: single;
  out ARemainsUTF8: string);
var
  pstr: pchar;
  p,left,charlen: integer;
  totalWidth: single;
  firstChar: boolean;
  nextchar: string;
  g: TBGRAGlyph;
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
        totalWidth += g.Width*FullHeight;
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

procedure TBGRAVectorizedFont.DrawText(ADest: TBGRACanvas2D; ATextUTF8: string; X,
  Y: Single; AAlign: TBGRATypeWriterAlignment);
var underlinePoly: ArrayOfTPointF;
  m: TAffineMatrix;
  i: integer;
  deltaY: single;
begin
  inherited DrawText(ADest, ATextUTF8, X, Y, AAlign);
  if AAlign in [twaBottom,twaBottomLeft,twaBottomRight] then deltaY := -1 else
  if AAlign in [twaLeft,twaMiddle,twaRight] then deltaY := -0.5 else
    deltaY := 0;
  if UnderlineDecoration and (Resolution > 0) then
  begin
    underlinePoly := BGRATextUnderline(PointF(0,deltaY), GetTextSize(ATextUTF8).x/FullHeight, FontPixelMetric.Baseline/Resolution,
      (FontPixelMetric.Baseline-FontPixelMetric.CapLine)/Resolution);
    if underlinePoly <> nil then
    begin
      m := GetTextMatrix(ATextUTF8, X,Y,AAlign);
      for i := 0 to high(underlinePoly) do
        underlinePoly[i] := m*underlinePoly[i];
      if OutlineMode <> twoPath then ADest.beginPath;
      ADest.polylineTo(underlinePoly);
      DrawLastPath(ADest);
    end;
  end;
  if StrikeOutDecoration and (Resolution > 0) then
  begin
    underlinePoly := BGRATextStrikeOut(PointF(0,deltaY), GetTextSize(ATextUTF8).x/FullHeight, FontPixelMetric.Baseline/Resolution,
      (FontPixelMetric.Baseline-FontPixelMetric.CapLine)/Resolution, (FontPixelMetric.Baseline-FontPixelMetric.xLine)/Resolution);
    if underlinePoly <> nil then
    begin
      m := GetTextMatrix(ATextUTF8, X,Y,AAlign);
      for i := 0 to high(underlinePoly) do
        underlinePoly[i] := m*underlinePoly[i];
      if OutlineMode <> twoPath then ADest.beginPath;
      ADest.polylineTo(underlinePoly);
      DrawLastPath(ADest);
    end;
  end;
end;

procedure TBGRAVectorizedFont.CopyTextPathTo(ADest: IBGRAPath;
  ATextUTF8: string; X, Y: Single; AAlign: TBGRATypeWriterAlignment);
var underlinePoly: ArrayOfTPointF;
  m: TAffineMatrix;
  i: integer;
  deltaY: single;
begin
  inherited CopyTextPathTo(ADest,ATextUTF8, X, Y, AAlign);
  if AAlign in [twaBottom,twaBottomLeft,twaBottomRight] then deltaY := -1 else
  if AAlign in [twaLeft,twaMiddle,twaRight] then deltaY := -0.5 else
    deltaY := 0;
  if UnderlineDecoration and (Resolution > 0) then
  begin
    underlinePoly := BGRATextUnderline(PointF(0,deltaY), GetTextSize(ATextUTF8).x/FullHeight, FontPixelMetric.Baseline/Resolution,
      (FontPixelMetric.Baseline-FontPixelMetric.CapLine)/Resolution);
    if underlinePoly <> nil then
    begin
      m := GetTextMatrix(ATextUTF8, X,Y,AAlign);
      ADest.moveTo(m*underlinePoly[0]);
      for i := 1 to high(underlinePoly) do
        ADest.lineTo(m*underlinePoly[i]);
      ADest.closePath;
    end;
  end;
  if StrikeOutDecoration and (Resolution > 0) then
  begin
    underlinePoly := BGRATextStrikeOut(PointF(0,deltaY), GetTextSize(ATextUTF8).x/FullHeight, FontPixelMetric.Baseline/Resolution,
      (FontPixelMetric.Baseline-FontPixelMetric.CapLine)/Resolution, (FontPixelMetric.Baseline-FontPixelMetric.xLine)/Resolution);
    if underlinePoly <> nil then
    begin
      m := GetTextMatrix(ATextUTF8, X,Y,AAlign);
      ADest.moveTo(m*underlinePoly[0]);
      for i := 1 to high(underlinePoly) do
        ADest.lineTo(m*underlinePoly[i]);
      ADest.closePath;
    end;
  end;
end;

procedure TBGRAVectorizedFont.DrawTextWordBreak(ADest: TBGRACanvas2D;
  ATextUTF8: string; X, Y, MaxWidth: Single; AAlign: TBGRATypeWriterAlignment);
var ARemains: string;
  step: TPointF;
  lines: TStringList;
  i: integer;
  lineShift: single;
  oldItalicSlope: single;
  lineAlignment: TBGRATypeWriterAlignment;
begin
  if (ATextUTF8 = '') or (MaxWidth <= 0) then exit;

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
      SplitText(ATextUTF8, MaxWidth, ARemains);
      DrawText(ADest,ATextUTF8,X,Y,lineAlignment);
      ATextUTF8 := ARemains;
      X+= step.X;
      Y+= step.Y;
    until ARemains = '';
  end else
  begin
    lines := TStringList.Create;
    repeat
      SplitText(ATextUTF8, MaxWidth, ARemains);
      lines.Add(ATextUTF8);
      ATextUTF8 := ARemains;
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

procedure TBGRAVectorizedFont.DrawTextRect(ADest: TBGRACanvas2D; ATextUTF8: string;
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
  DrawTextWordBreak(ADest,ATextUTF8,X,Y,X2-X1,AAlign);
  Orientation:= oldOrientation;
end;

procedure TBGRAVectorizedFont.DrawTextRect(ADest: TBGRACanvas2D; ATextUTF8: string;
  ATopLeft, ABottomRight: TPointF; AAlign: TBGRATypeWriterAlignment);
begin
  DrawTextRect(ADest,ATextUTF8,ATopLeft.X,ATopLeft.Y,ABottomRight.X,ABottomRight.Y,AAlign);
end;

function TBGRAVectorizedFont.GetTextWordBreakGlyphBoxes(ATextUTF8: string; X, Y,
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
  if ATextUTF8 = '' then exit;

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
    SplitText(ATextUTF8, MaxWidth, ARemains);
    lines.Add(ATextUTF8);
    ATextUTF8 := ARemains;
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

function TBGRAVectorizedFont.GetTextRectGlyphBoxes(ATextUTF8: string; X1, Y1, X2,
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
  result := GetTextWordBreakGlyphBoxes(ATextUTF8,X,Y,X2-X1,AAlign);
  Orientation:= oldOrientation;
end;

function TBGRAVectorizedFont.GetTextRectGlyphBoxes(ATextUTF8: string; ATopLeft,
  ABottomRight: TPointF; AAlign: TBGRATypeWriterAlignment): TGlyphBoxes;
begin
  result := GetTextRectGlyphBoxes(ATextUTF8,ATopLeft.X,ATopLeft.Y,ABottomRight.X,ABottomRight.Y,AAlign);
end;

procedure TBGRAVectorizedFont.UpdateDirectory;
var
  NbFiles: integer;
  SearchRec: TSearchRec;
  Info: TBGRAGlyphsInfo;
  Fullname: string;
begin
  NbFiles := 0;
  FDirectoryContent := nil;
  if FDirectory = '' then exit;
  if (length(FDirectory) > 0) and not (FDirectory[length(FDirectory)] in AllowDirectorySeparators) then
    FDirectory += DirectorySeparator;
  if FindFirstUTF8(FDirectory +'*.glyphs', faAnyFile, SearchRec) = 0 then
  repeat
    if (faDirectory or faVolumeId or faSysFile) and SearchRec.Attr = 0 then
    begin
      Fullname := FDirectory+SearchRec.Name;
      Info := LoadGlyphsInfo(Fullname);
      if (info.Name <> '') and (info.NbGlyphs > 0) then
      begin
        if NbFiles = length(FDirectoryContent) then
          setlength(FDirectoryContent,2*NbFiles+1);
        FDirectoryContent[NbFiles].Filename:= Fullname;
        FDirectoryContent[NbFiles].FontName:= info.Name;
        FDirectoryContent[NbFiles].FontStyle:= info.Style;
        inc(NbFiles);
      end;
    end;
  until FindNext(SearchRec) <> 0;
  FindClose(SearchRec);
  SetLength(FDirectoryContent,NbFiles);
end;

function TBGRAVectorizedFont.LoadGlyphsInfo(AFilenameUTF8: string): TBGRAGlyphsInfo;
var Stream: TFileStreamUTF8;
  twHeader: TBGRACustomTypeWriterHeader;
  vfHeader: TBGRAVectorizedFontHeader;
begin
  result.Name := '';
  result.NbGlyphs := 0;
  result.Style := [];
  Stream := nil;
  try
    Stream := TFileStreamUTF8.Create(AFilenameUTF8,fmOpenRead);
    Stream.Position := 4;
    twHeader := ReadCustomTypeWriterHeader(Stream);
    result.NbGlyphs := twHeader.NbGlyphs;
    if twHeader.HeaderName = HeaderName then
    begin
      vfHeader := ReadVectorizedFontHeader(Stream);
      result.Name := vfHeader.Name;
      result.Style:= vfHeader.Style;
    end;
  except
    on ex:exception do
    begin

    end;
  end;
  Stream.Free;
end;

function TBGRAVectorizedFont.GetGlyph(AIdentifier: string): TBGRAGlyph;
var size: TSize;
  g: TBGRAPolygonalGlyph;
begin
  Result:=inherited GetGlyph(AIdentifier);
  if (result = nil) and (FResolution > 0) and (FFont <> nil) then
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
begin
  BGRADefaultWordBreakHandler(ABefore,AAfter);
end;

procedure TBGRAVectorizedFont.Init(AVectorize: boolean);
begin
  FName := 'Arial';
  FStyle := [];
  FFontMatrix := AffineMatrixIdentity;
  FOrientation := 0;
  FResolution := 100;
  FFontEmHeightRatio := 1;
  FFontEmHeightRatioComputed := false;
  if AVectorize then
    FFont := TFont.Create
  else
    FFont := nil;
  FBuffer := BGRABitmapFactory.Create;
  FFullHeight := 20;
  FItalicSlope := 0;
  UpdateFont;
  UpdateMatrix;
  FWordBreakHandler:= nil;
end;

function TBGRAVectorizedFont.CustomHeaderSize: integer;
begin
  Result:= (inherited CustomHeaderSize) + 4+length(FName)+4 + sizeof(single) + 4 + 5*4;
end;

procedure TBGRAVectorizedFont.WriteCustomHeader(AStream: TStream);
var metric: TFontPixelMetric;
begin
  inherited WriteCustomHeader(AStream);
  LEWriteLongint(AStream, length(FName));
  AStream.Write(FName[1],length(FName));
  LEWriteLongint(AStream, integer(FStyle));
  LEWriteSingle(AStream, FontEmHeightRatio);
  LEWriteLongint(AStream, Resolution);
  metric := FontPixelMetric;
  LEWriteLongint(AStream, metric.Baseline);
  LEWriteLongint(AStream, metric.xLine);
  LEWriteLongint(AStream, metric.CapLine);
  LEWriteLongint(AStream, metric.DescentLine);
  LEWriteLongint(AStream, metric.Lineheight);
end;

procedure TBGRAVectorizedFont.ReadAdditionalHeader(AStream: TStream);
var Header: TBGRAVectorizedFontHeader;
begin
  inherited ReadAdditionalHeader(AStream);
  Header := ReadVectorizedFontHeader(AStream);
  FName := Header.Name;
  FStyle := Header.Style;
  if header.EmHeightRatio <> 0 then
  begin
    FFontEmHeightRatio := Header.EmHeightRatio;
    FFontEmHeightRatioComputed := true;
  end else
  begin
    FFontEmHeightRatio := 1;
    FFontEmHeightRatioComputed := false;
  end;
  FFontPixelMetric := Header.PixelMetric;
  FFontPixelMetricComputed := True;
  if FFont = nil then
    FResolution := Header.Resolution;
end;

function TBGRAVectorizedFont.ReadVectorizedFontHeader(AStream: TStream): TBGRAVectorizedFontHeader;
var lNameLength: integer;
begin
  lNameLength := LEReadLongint(AStream);
  setlength(result.Name, lNameLength);
  AStream.Read(result.Name[1],length(result.Name));
  result.Style := TFontStyles(LEReadLongint(AStream));
  result.EmHeightRatio:= LEReadSingle(AStream);
  result.Resolution := LEReadLongint(AStream);
  result.PixelMetric.Baseline := LEReadLongint(AStream);
  result.PixelMetric.xLine := LEReadLongint(AStream);
  result.PixelMetric.CapLine := LEReadLongint(AStream);
  result.PixelMetric.DescentLine := LEReadLongint(AStream);
  result.PixelMetric.Lineheight := LEReadLongint(AStream);
  result.PixelMetric.Defined := result.PixelMetric.Lineheight > 0;
end;

function TBGRAVectorizedFont.HeaderName: string;
begin
  Result:= 'TBGRAVectorizedFont';
end;

procedure TBGRAVectorizedFont.SetDirectory(const AValue: string);
begin
  if Trim(AValue) = Trim(FDirectory) then exit;
  FDirectory := Trim(AValue);
  UpdateDirectory;
  UpdateFont;
end;

end.

