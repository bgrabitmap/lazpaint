unit BGRAArrow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmapTypes, BGRAGraphics;

type

  { TBGRAArrow }

  TBGRAArrow = class
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
    function GetIsEndDefined: boolean;
    function GetIsStartDefined: boolean;
    procedure SetEndOffsetX(AValue: single);
    procedure SetEndRepeatCount(AValue: integer);
    procedure SetEndSizeFactor(AValue: TPointF);
    procedure SetLineCap(AValue: TPenEndCap);
    procedure SetStartOffsetX(AValue: single);
    procedure SetStartRepeatCount(AValue: integer);
    procedure SetStartSizeFactor(AValue: TPointF);
    procedure SetWidth(AValue: single);
  public
    constructor Create;
    procedure SetStart(AStyle: TBGRAArrowStyle; ATipStyle: TPenJoinStyle;
        ARelativePenWidth: single; ATriangleBackOffset: single);
    procedure SetEnd(AStyle: TBGRAArrowStyle; ATipStyle: TPenJoinStyle;
        ARelativePenWidth: single; ATriangleBackOffset: single);
    function ComputeStartAt(const APosition: TPointF; const ADirection: TPointF; const AWidth: single; const ACurrentPos: single): ArrayOfTPointF;
    function ComputeEndAt(const APosition: TPointF; const ADirection: TPointF; const AWidth: single; const ACurrentPos: single): ArrayOfTPointF;
    property IsStartDefined: boolean read GetIsStartDefined;
    property IsEndDefined: boolean read GetIsEndDefined;
    property LineCap: TPenEndCap read FLineCap write SetLineCap;
    property StartSize: TPointF read FStartSizeFactor write SetStartSizeFactor;
    property EndSize: TPointF read FEndSizeFactor write SetEndSizeFactor;
    property StartOffsetX: single read FStartOffsetX write SetStartOffsetX;
    property EndOffsetX: single read FEndOffsetX write SetEndOffsetX;
    property StartRepeatCount: integer read FStartRepeatCount write SetStartRepeatCount;
    property EndRepeatCount: integer read FEndRepeatCount write SetEndRepeatCount;
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

procedure TBGRAArrow.SetEndOffsetX(AValue: single);
begin
  if FEndOffsetX=AValue then Exit;
  FEndOffsetX:=AValue;
  FEndComputed:= false;
  FEnd := nil;
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

constructor TBGRAArrow.Create;
begin
  FWidth := 1;
  FStartSizeFactor := PointF(2,2);
  FEndSizeFactor := PointF(2,2);
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

