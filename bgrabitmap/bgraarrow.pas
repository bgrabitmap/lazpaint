unit BGRAArrow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmapTypes, BGRAGraphics;

type
  { TBGRAArrow }

  TBGRAArrow = class(TBGRACustomArrow)
  private
    FLineCap: TPenEndCap;
    FWidth : single;
    FStart : ArrayOfTPointF;
    FStartComputed: boolean;
    FStartStyle: TBGRAArrowStyle;
    FStartSizeFactor: TPointF;
    FStartTipStyle: TPenJoinStyle;
    FStartOffsetX: single;
    FStartRepeatCount: integer;
    FStartRelativePenWidth: single;
    FStartTriangleBackOffset: single;
    FEnd : ArrayOfTPointF;
    FEndComputed: boolean;
    FEndStyle: TBGRAArrowStyle;
    FEndSizeFactor: TPointF;
    FEndTipStyle: TPenJoinStyle;
    FEndOffsetX: single;
    FEndRepeatCount: integer;
    FEndRelativePenWidth: single;
    FEndTriangleBackOffset: single;
    function ComputeAnyAt(const AData: ArrayOfTPointF; const APosition, ADirection: TPointF): ArrayOfTPointF;
    function ComputeData(AStyle: TBGRAArrowStyle; const ASizeFactor: TPointF;
        ATipStyle: TPenJoinStyle; ALineCap: TPenEndCap; const AWidth: single; AOffsetX: single;
        ARepeatCount: integer; ARelativePenWidth: single; ATriangleBackOffset: single): ArrayOfTPointF;
    procedure SetWidth(AValue: single);
  protected
    function GetEndRepeatCount: integer; override;
    function GetEndSizeFactor: TPointF; override;
    function GetIsEndDefined: boolean; override;
    function GetIsStartDefined: boolean; override;
    function GetEndOffsetX: single; override;
    function GetStartOffsetX: single; override;
    function GetStartRepeatCount: integer; override;
    function GetStartSizeFactor: TPointF; override;
    procedure SetEndOffsetX(AValue: single); override;
    procedure SetEndRepeatCount(AValue: integer); override;
    procedure SetEndSizeFactor(AValue: TPointF); override;
    procedure SetStartOffsetX(AValue: single); override;
    procedure SetStartRepeatCount(AValue: integer); override;
    procedure SetStartSizeFactor(AValue: TPointF); override;
    function GetLineCap: TPenEndCap; override;
    procedure SetLineCap(AValue: TPenEndCap); override;
    procedure SetStart(AStyle: TBGRAArrowStyle; ATipStyle: TPenJoinStyle = pjsMiter; ARelativePenWidth: single = 1; ATriangleBackOffset: single = 0);
    procedure SetEnd(AStyle: TBGRAArrowStyle; ATipStyle: TPenJoinStyle = pjsMiter; ARelativePenWidth: single = 1; ATriangleBackOffset: single = 0);
  public
    constructor Create;
    procedure StartAsNone; override;
    procedure StartAsClassic(AFlipped: boolean = false; ACut: boolean = false; ARelativePenWidth: single = 1); override;
    procedure StartAsTriangle(ABackOffset: single = 0; ARounded: boolean = false; AHollow: boolean = false; AHollowPenWidth: single = 0.5); override;
    procedure StartAsTail; override;
    procedure EndAsNone; override;
    procedure EndAsClassic(AFlipped: boolean = false; ACut: boolean = false; ARelativePenWidth: single = 1); override;
    procedure EndAsTriangle(ABackOffset: single = 0; ARounded: boolean = false; AHollow: boolean = false; AHollowPenWidth: single = 0.5); override;
    procedure EndAsTail; override;
    function ComputeStartAt(const APosition: TPointF; const ADirection: TPointF; const AWidth: single; const ACurrentPos: single): ArrayOfTPointF; override;
    function ComputeEndAt(const APosition: TPointF; const ADirection: TPointF; const AWidth: single; const ACurrentPos: single): ArrayOfTPointF; override;

  end;

implementation

uses BGRATransform, BGRAPen, BGRAPath;

{ TBGRAArrow }

function TBGRAArrow.ComputeAnyAt(const AData: ArrayOfTPointF; const APosition,
  ADirection: TPointF): ArrayOfTPointF;
var m: TAffineMatrix;
  i: integer;
begin
  if (AData = nil) or isEmptyPointF(APosition) or isEmptyPointF(ADirection) or ((ADirection.x = 0) and (ADirection.y = 0)) then
  begin
    result := nil;
    exit;
  end;
  m := AffineMatrixTranslation(APosition.x,APosition.y)*AffineMatrixLinear(ADirection, PointF(-ADirection.y, ADirection.x));
  setlength(result, length(AData));
  for i := 0 to high(result) do
    if not isEmptyPointF(AData[i]) then
      result[i] := m*AData[i]
    else
      result[i] := EmptyPointF;
end;

function TBGRAArrow.ComputeData(AStyle: TBGRAArrowStyle;
  const ASizeFactor: TPointF; ATipStyle: TPenJoinStyle; ALineCap: TPenEndCap;
  const AWidth: single; AOffsetX: single; ARepeatCount: integer;
  ARelativePenWidth: single; ATriangleBackOffset: single): ArrayOfTPointF;
var sizeFactorX,sizeFactorY,ofsX: single;
  prefix,suffix,middle: ArrayOfTPointF;
  arc: TArcDef;
  i,j,n : integer;
  withCut: ArrayOfTPointF;
  subResult: ArrayOfTPointF;
  w: single;
  backOfs: single;
  tailSizeX, tailAdditionalWidth: single;
begin
  sizeFactorX := abs(ASizeFactor.X)*AWidth;
  sizeFactorY := abs(ASizeFactor.Y)*AWidth;
  if (sizeFactorX = 0) or (sizeFactorY = 0) then
  begin
    result := nil;
    exit;
  end;
  w := AWidth*ARelativePenWidth;
  if AStyle in [asTail,asTailRepeat] then
  begin
    tailSizeX := sizeFactorX/SizeFactorY*AWidth*0.5;
    if AStyle = asTailRepeat then
    begin
      tailAdditionalWidth:= AWidth-tailSizeX;
      if tailAdditionalWidth < 0 then tailAdditionalWidth := 0;
    end else
      tailAdditionalWidth:=0;
  end
  else
  begin
    tailSizeX := 0;
    tailAdditionalWidth:=0;
  end;
  case AStyle of
  asTriangle,asHollowTriangle: begin
    backOfs := ATriangleBackOffset*sizeFactorX;
    if AStyle = asHollowTriangle then
    begin
      result := ComputeWidePolylinePoints(PointsF([PointF(0.5*w,-AWidth*0.5),
             PointF(0.5*w-backOfs,-sizeFactorY+w*0.5),
             PointF(sizeFactorX-w*0.5,0),
             PointF(w*0.5-backOfs,sizeFactorY-w*0.5),
             PointF(0.5*w,AWidth*0.5)]),
             w,BGRABlack,ALineCap,ATipStyle,nil,[plCycle]);
    end else
    begin
      prefix := PointsF([PointF(0,-AWidth*0.5),PointF(-backOfs,-sizeFactorY)]);
      suffix := PointsF([PointF(-backOfs,sizeFactorY),PointF(0,AWidth*0.5)]);
      if (ATipStyle in[pjsRound,pjsBevel]) then
      begin
        arc := Html5ArcTo(prefix[1],PointF(sizeFactorX,0),suffix[0],AWidth*0.5);
        if ATipStyle = pjsRound then
          middle := ComputeArc(arc)
        else
          middle := PointsF([ArcStartPoint(arc),ArcEndPoint(arc)]);
      end
      else middle := PointsF([PointF(sizeFactorX,0)]);
      result := ConcatPointsF([prefix,middle,suffix]);
    end;
  end;
  asNormal,asCut:
    begin
      if AStyle = asCut then ALineCap:= pecSquare;
      result := ComputeWidePolylinePoints([PointF(-sizeFactorX,-sizeFactorY),
        PointF(0,0),PointF(-sizeFactorX,+sizeFactorY)],w,BGRABlack,ALineCap,ATipStyle,nil,[]);
    end;
  asFlipped,asFlippedCut:
    begin
      if AStyle = asFlippedCut then ALineCap:= pecSquare;
      result := ComputeWidePolylinePoints([PointF(+sizeFactorX,-sizeFactorY),
       PointF(0,0),PointF(+sizeFactorX,+sizeFactorY)],w,BGRABlack,ALineCap,ATipStyle,nil,[]);
    end;
  asTail: result := PointsF([PointF(0,-0.5*AWidth),PointF(tailSizeX,-0.5*AWidth),PointF(0,0),PointF(tailSizeX,0.5*AWidth),PointF(0,0.5*AWidth)]);
  asTailRepeat: result := PointsF([PointF(0,-0.5*AWidth),PointF(tailSizeX+tailAdditionalWidth,-0.5*AWidth),PointF(tailAdditionalWidth,0),PointF(tailSizeX+tailAdditionalWidth,0.5*AWidth),PointF(0,0.5*AWidth),PointF(-tailSizeX,0)]);
  else
    result := nil;
  end;
  if (AStyle in [asCut,asFlippedCut,asHollowTriangle]) then
  begin
    n := 0;
    setlength(withCut,length(result)*2);
    for i := 0 to high(result) do
      if isEmptyPointF(result[i]) then
      begin
        if (n > 0) and not isEmptyPointF(withCut[n-1]) then
        begin
          withCut[n] := EmptyPointF;
          inc(n);
        end;
      end else
      if abs(result[i].y)<=sizeFactorY then
      begin
        withCut[n] := result[i];
        inc(n);
      end else
      if result[i].y>sizeFactorY then
      begin
        j := (i+length(result)-1) mod length(result);
        if result[j].y<=sizeFactorY then
        begin
          withCut[n].x := result[j].x + (result[i].x-result[j].x)/(result[i].y-result[j].y)*(sizeFactorY-result[j].y);
          withCut[n].y := sizeFactorY;
          inc(n);
        end;
        j := (i+1) mod length(result);
        if result[j].y<=sizeFactorY then
        begin
          withCut[n].x := result[j].x + (result[i].x-result[j].x)/(result[i].y-result[j].y)*(sizeFactorY-result[j].y);
          withCut[n].y := sizeFactorY;
          inc(n);
        end;
      end else
      if result[i].y<-sizeFactorY then
      begin
        j := (i+length(result)-1) mod length(result);
        if result[j].y>=-sizeFactorY then
        begin
          withCut[n].x := result[j].x + (result[i].x-result[j].x)/(result[i].y-result[j].y)*(-sizeFactorY-result[j].y);
          withCut[n].y := -sizeFactorY;
          inc(n);
        end;
        j := (i+1) mod length(result);
        if result[j].y>=-sizeFactorY then
        begin
          withCut[n].x := result[j].x + (result[i].x-result[j].x)/(result[i].y-result[j].y)*(-sizeFactorY-result[j].y);
          withCut[n].y := -sizeFactorY;
          inc(n);
        end;
      end;
      if (n > 0) and isEmptyPointF(withCut[n-1]) then dec(n);
      setlength(withCut,n);
      result := withCut;
  end;
  if AOffsetX <> 0 then
  begin
    ofsX := AOffsetX*AWidth;
    for i := 0 to high(result) do
      if not isEmptyPointF(result[i]) then
        result[i].x += ofsX;
  end;
  if ARepeatCount > 1 then
  begin
    if ARepeatCount > 10 then ARepeatCount:= 10;
    if AStyle in[asTriangle,asHollowTriangle] then AOffsetX += sizeFactorX/AWidth
    else if AStyle in[asTail,asTailRepeat] then AOffsetX += (tailSizeX+tailAdditionalWidth)/AWidth+1
    else AOffsetX += 2*ARelativePenWidth;
    if AStyle = asTail then AStyle := asTailRepeat;
    subResult := ComputeData(AStyle,ASizeFactor,ATipStyle,ALineCap,AWidth,AOffsetX,ARepeatCount-1,ARelativePenWidth,ATriangleBackOffset);
    result := ConcatPointsF([result,PointsF([EmptyPointF]),subResult]);
  end;
end;

function TBGRAArrow.GetIsEndDefined: boolean;
begin
  result := FEndStyle <> asNone;
end;

function TBGRAArrow.GetIsStartDefined: boolean;
begin
  result := FStartStyle <> asNone;
end;

function TBGRAArrow.GetEndOffsetX: single;
begin
  result := FEndOffsetX;
end;

function TBGRAArrow.GetStartOffsetX: single;
begin
  result := FStartOffsetX;
end;

function TBGRAArrow.GetStartRepeatCount: integer;
begin
  result := FStartRepeatCount;
end;

function TBGRAArrow.GetStartSizeFactor: TPointF;
begin
  result := FStartSizeFactor;
end;

procedure TBGRAArrow.SetEndOffsetX(AValue: single);
begin
  if FEndOffsetX=AValue then Exit;
  FEndOffsetX:=AValue;
  FEndComputed:= false;
  FEnd := nil;
end;

function TBGRAArrow.GetLineCap: TPenEndCap;
begin
  result := FLineCap;
end;

procedure TBGRAArrow.SetEndRepeatCount(AValue: integer);
begin
  if FEndRepeatCount=AValue then Exit;
  FEndRepeatCount:=AValue;
  FEndComputed:= false;
  FEnd := nil;
end;

procedure TBGRAArrow.SetEndSizeFactor(AValue: TPointF);
begin
  if FEndSizeFactor=AValue then Exit;
  FEndSizeFactor:=AValue;
  FEndComputed:= false;
  FEnd := nil;
end;

procedure TBGRAArrow.SetLineCap(AValue: TPenEndCap);
begin
  if FLineCap=AValue then Exit;
  FLineCap:=AValue;
  FStartComputed:= false;
  FEndComputed:= false;
  FStart:= nil;
  FEnd := nil;
end;

procedure TBGRAArrow.SetStartOffsetX(AValue: single);
begin
  if FStartOffsetX=AValue then Exit;
  FStartOffsetX:=AValue;
  FStartComputed:= false;
  FStart := nil;
end;

procedure TBGRAArrow.SetStartRepeatCount(AValue: integer);
begin
  if FStartRepeatCount=AValue then Exit;
  FStartRepeatCount:=AValue;
  FStartComputed:= false;
  FStart := nil;
end;

procedure TBGRAArrow.SetStartSizeFactor(AValue: TPointF);
begin
  if FStartSizeFactor=AValue then Exit;
  FStartSizeFactor:=AValue;
  FStartComputed:= false;
  FStart := nil;
end;

procedure TBGRAArrow.SetWidth(AValue: single);
begin
  if FWidth=AValue then Exit;
  FWidth:=AValue;
  FStartComputed := false;
  FEndComputed:= false;
end;

function TBGRAArrow.GetEndRepeatCount: integer;
begin
  Result:= FEndRepeatCount;
end;

function TBGRAArrow.GetEndSizeFactor: TPointF;
begin
  Result:= FEndSizeFactor;
end;

constructor TBGRAArrow.Create;
begin
  FWidth := 1;
  FStartSizeFactor := PointF(2,2);
  FEndSizeFactor := PointF(2,2);
end;

procedure TBGRAArrow.StartAsNone;
begin
  SetStart(asNone);
end;

procedure TBGRAArrow.StartAsClassic(AFlipped: boolean; ACut: boolean;
  ARelativePenWidth: single);
var join: TPenJoinStyle;
begin
  if (LineCap = pecRound) and not ACut then join := pjsRound else join := pjsMiter;
  if ACut then
  begin
    if AFlipped then
      SetStart(asFlippedCut,join,ARelativePenWidth)
    else
      SetStart(asCut,join,ARelativePenWidth)
  end
  else
  begin
    if AFlipped then
      SetStart(asFlipped,join,ARelativePenWidth)
    else
      SetStart(asNormal,join,ARelativePenWidth)
  end;
end;

procedure TBGRAArrow.StartAsTriangle(ABackOffset: single; ARounded: boolean;
  AHollow: boolean; AHollowPenWidth: single);
var join: TPenJoinStyle;
begin
  if ARounded then join := pjsRound else join := pjsMiter;
  if AHollow then
    SetStart(asHollowTriangle, join,AHollowPenWidth, ABackOffset)
  else
    SetStart(asTriangle, join,1,ABackOffset);
end;

procedure TBGRAArrow.StartAsTail;
begin
  SetStart(asTail);
end;

procedure TBGRAArrow.EndAsNone;
begin
  SetEnd(asNone);
end;

procedure TBGRAArrow.EndAsClassic(AFlipped: boolean; ACut: boolean;
  ARelativePenWidth: single);
var join: TPenJoinStyle;
begin
  if (LineCap = pecRound) and not ACut then join := pjsRound else join := pjsMiter;
  if ACut then
  begin
    if AFlipped then
      SetEnd(asFlippedCut,join,ARelativePenWidth)
    else
      SetEnd(asCut,join,ARelativePenWidth)
  end
  else
  begin
    if AFlipped then
      SetEnd(asFlipped,join,ARelativePenWidth)
    else
      SetEnd(asNormal,join,ARelativePenWidth)
  end;
end;

procedure TBGRAArrow.EndAsTriangle(ABackOffset: single; ARounded: boolean;
  AHollow: boolean; AHollowPenWidth: single);
var join: TPenJoinStyle;
begin
  if ARounded then join := pjsRound else join := pjsMiter;
  if AHollow then
    SetEnd(asHollowTriangle, join,AHollowPenWidth, ABackOffset)
  else
    SetEnd(asTriangle, join,1, ABackOffset);
end;

procedure TBGRAArrow.EndAsTail;
begin
  SetEnd(asTail);
end;

procedure TBGRAArrow.SetStart(AStyle: TBGRAArrowStyle;
  ATipStyle: TPenJoinStyle; ARelativePenWidth: single; ATriangleBackOffset: single);
begin
  FStartStyle := AStyle;
  FStartTipStyle := ATipStyle;
  FStartComputed := false;
  FStartRelativePenWidth:= ARelativePenWidth;
  FStartTriangleBackOffset := ATriangleBackOffset;
  FStart := nil;
end;

procedure TBGRAArrow.SetEnd(AStyle: TBGRAArrowStyle; ATipStyle: TPenJoinStyle;
  ARelativePenWidth: single; ATriangleBackOffset: single);
begin
  FEndStyle := AStyle;
  FEndTipStyle := ATipStyle;
  FEndComputed := false;
  FEndRelativePenWidth:= ARelativePenWidth;
  FEndTriangleBackOffset := ATriangleBackOffset;
  FEnd := nil;
end;

function TBGRAArrow.ComputeStartAt(const APosition: TPointF;
  const ADirection: TPointF; const AWidth: single; const ACurrentPos: single
  ): ArrayOfTPointF;
begin
  if not IsStartDefined then
  begin
    result := nil;
    exit;
  end;
  if AWidth <> FWidth then
  begin
    FWidth := AWidth;
    FStartComputed:= false;
  end;
  if not FStartComputed then
  begin
    FStart := ComputeData(FStartStyle,FStartSizeFactor,FStartTipStyle,FLineCap,FWidth,
    FStartOffsetX-ACurrentPos,FStartRepeatCount,FStartRelativePenWidth,FStartTriangleBackOffset);
    FStartComputed:= true;
  end;
  result := ComputeAnyAt(FStart,APosition,ADirection);
end;

function TBGRAArrow.ComputeEndAt(const APosition: TPointF;
  const ADirection: TPointF; const AWidth: single; const ACurrentPos: single
  ): ArrayOfTPointF;
begin
  if not IsEndDefined then
  begin
    result := nil;
    exit;
  end;
  if AWidth <> FWidth then
  begin
    FWidth := AWidth;
    FEndComputed:= false;
  end;
  if not FEndComputed then
  begin
    FEnd := ComputeData(FEndStyle,FEndSizeFactor,FEndTipStyle,FLineCap,FWidth,
      FEndOffsetX-ACurrentPos,FEndRepeatCount,FEndRelativePenWidth,FEndTriangleBackOffset);
    FEndComputed:= true;
  end;
  result := ComputeAnyAt(FEnd,APosition,ADirection);
end;

end.

