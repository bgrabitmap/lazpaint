unit BGRACanvasGL;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRAGraphics, BGRABitmapTypes,
  BGRAOpenGLType, BGRATransform, BGRAPath;

type
  TBGLPath = class;

  { TBGLCustomCanvas }

  TBGLCustomCanvas = class
  private
    FHeight: integer;
    FWidth: integer;
    FNoClip: boolean;
    FClipRect: TRect;
  protected
    procedure SwapRect(var r: TRect);
    procedure SwapRect(var x1,y1,x2,y2: single);
    procedure InternalArc(cx,cy,rx,ry: single; const StartPoint,EndPoint: TPointF; ABorderColor,AOuterFillColor,ACenterFillColor: TBGRAPixel; AOptions: TArcOptions; ADrawChord: boolean = false); overload;
    procedure InternalArc(cx,cy,rx,ry: single; StartAngleRad,EndAngleRad: Single; ABorderColor,AOuterFillColor,ACenterFillColor: TBGRAPixel; AOptions: TArcOptions; ADrawChord: boolean = false); overload;
    procedure InternalArcInRect(r: TRect; StartAngleRad,EndAngleRad: Single; ABorderColor,AOuterFillColor,ACenterFillColor: TBGRAPixel; AOptions: TArcOptions; ADrawChord: boolean = false); overload;
    function ComputeEllipseC(r: TRect; AHasBorder: boolean; out cx,cy,rx,ry: single): boolean;
    procedure SetWidth(AValue: integer); virtual;
    procedure SetHeight(AValue: integer); virtual;
    function GetClipRect: TRect;
    procedure SetClipRect(AValue: TRect);
    procedure EnableScissor(AValue: TRect); virtual; abstract;
    procedure DisableScissor; virtual; abstract;
    function GetMatrix: TAffineMatrix; virtual; abstract;
    procedure SetMatrix(AValue: TAffineMatrix); virtual; abstract;
    procedure SetBlendMode(AValue: TOpenGLBlendMode); virtual; abstract;
    function GetBlendMode: TOpenGLBlendMode; virtual; abstract;

    procedure InternalStartPutPixel(const pt: TPointF); virtual; abstract;
    procedure InternalStartPolyline(const pt: TPointF); virtual; abstract;
    procedure InternalStartPolygon(const pt: TPointF); virtual; abstract;
    procedure InternalStartTriangleFan(const pt: TPointF); virtual; abstract;
    procedure InternalContinueShape(const pt: TPointF); virtual; abstract;
    procedure InternalEndShape; virtual; abstract;
    procedure InternalSetColor(const AColor: TBGRAPixel); virtual; abstract;

    procedure InternalStartBlend; virtual; abstract;
    procedure InternalEndBlend; virtual; abstract;

    procedure InternalStartBlendTriangles; virtual; abstract;
    procedure InternalStartBlendQuads; virtual; abstract;
    procedure InternalEndBlendTriangles; virtual; abstract;
    procedure InternalEndBlendQuads; virtual; abstract;
  public
    constructor Create;
    procedure Fill(AColor: TBGRAPixel); virtual; abstract;

    procedure PutPixels(const APoints: array of TPointF; AColor: TBGRAPixel); virtual; overload;
    procedure PutPixels(const APoints: array of TPointF; const AColors: array of TBGRAPixel); virtual; overload;

    procedure Line(x1,y1,x2,y2: single; AColor: TBGRAPixel; ADrawLastPoint: boolean = true);
    procedure Line(p1,p2: TPointF; AColor: TBGRAPixel; ADrawLastPoint: boolean = true);
    procedure Polylines(const APoints: array of TPointF; AColor: TBGRAPixel; ADrawLastPoints: boolean = true); virtual;

    procedure Polygons(const APoints: array of TPointF; AColor: TBGRAPixel); virtual;
    procedure FillPolyConvex(const APoints: array of TPointF; AColor: TBGRAPixel; APixelCenteredCoordinates: boolean = true);

    procedure FillTriangleLinearColor(pt1,pt2,pt3: TPointF; c1,c2,c3: TBGRAPixel; APixelCenteredCoordinates: boolean = true);
    procedure FillTriangles(const APoints: array of TPointF; AColor: TBGRAPixel; APixelCenteredCoordinates: boolean = true); virtual;
    procedure FillTrianglesLinearColor(const APoints: array of TPointF; const AColors: array of TBGRAPixel; APixelCenteredCoordinates: boolean = true); virtual;
    procedure FillTrianglesFan(const APoints: array of TPointF; ACenterColor, ABorderColor: TBGRAPixel; APixelCenteredCoordinates: boolean = true); virtual;

    procedure FillQuadLinearColor(pt1,pt2,pt3,pt4: TPointF; c1,c2,c3,c4: TBGRAPixel; APixelCenteredCoordinates: boolean = true);
    procedure FillQuads(const APoints: array of TPointF; AColor: TBGRAPixel; APixelCenteredCoordinates: boolean = true); virtual;
    procedure FillQuadsLinearColor(const APoints: array of TPointF; const AColors: array of TBGRAPixel; APixelCenteredCoordinates: boolean = true); virtual;

    procedure DrawPath(APath: TBGLPath; c: TBGRAPixel);
    procedure FillPathConvex(APath: TBGLPath; c: TBGRAPixel; APixelCenteredCoordinates: boolean = true);

    procedure FillRectLinearColor(r: TRect; ATopLeftColor, ATopRightColor, ABottomRightColor, ABottomLeftColor: TBGRAPixel); virtual; overload;
    procedure FillRectLinearColor(x1,y1,x2,y2: single;
         ATopLeftColor, ATopRightColor, ABottomRightColor, ABottomLeftColor: TBGRAPixel;
         APixelCenteredCoordinates: boolean = true); virtual; overload;

    procedure Ellipse(cx,cy,rx,ry: single; AColor: TBGRAPixel); overload;
    procedure EllipseInRect(r: TRect; AColor: TBGRAPixel); overload;
    procedure Ellipse(cx,cy,rx,ry: single; AColor: TBGRAPixel; AFillColor: TBGRAPixel); overload;
    procedure EllipseInRect(r: TRect; AColor: TBGRAPixel; AFillColor: TBGRAPixel); overload;
    procedure EllipseLinearColor(cx,cy,rx,ry: single; AColor: TBGRAPixel; AOuterFillColor, AInnerFillColor: TBGRAPixel); overload;
    procedure EllipseLinearColorInRect(r: TRect; AColor: TBGRAPixel; AOuterFillColor, AInnerFillColor: TBGRAPixel); overload;
    procedure FillEllipse(cx,cy,rx,ry: single; AColor: TBGRAPixel; APixelCenteredCoordinates: boolean = true);
    procedure FillEllipseInRect(r: TRect; AColor: TBGRAPixel);
    procedure FillEllipseLinearColor(cx, cy, rx, ry: single; AOuterColor, AInnerColor: TBGRAPixel; APixelCenteredCoordinates: boolean = true);
    procedure FillEllipseLinearColorInRect(r: TRect; AOuterColor, AInnerColor: TBGRAPixel);

    procedure Arc(cx,cy,rx,ry: single; const StartPoint,EndPoint: TPointF; AColor: TBGRAPixel; ADrawChord: boolean; AFillColor: TBGRAPixel); overload;
    procedure Arc(cx,cy,rx,ry: single; StartAngleRad,EndAngleRad: Single; AColor: TBGRAPixel; ADrawChord: boolean; AFillColor: TBGRAPixel); overload;
    procedure ArcInRect(r: TRect; StartAngleRad,EndAngleRad: Single; AColor: TBGRAPixel; ADrawChord: boolean; AFillColor: TBGRAPixel);
    procedure ArcLinearColor(cx,cy,rx,ry: single; const StartPoint,EndPoint: TPointF; AColor: TBGRAPixel; ADrawChord: boolean; AOuterFillColor, AInnerFillColor: TBGRAPixel); overload;
    procedure ArcLinearColor(cx,cy,rx,ry: single; StartAngleRad,EndAngleRad: Single; AColor: TBGRAPixel; ADrawChord: boolean; AOuterFillColor, AInnerFillColor: TBGRAPixel); overload;
    procedure ArcLinearColorInRect(r: TRect; StartAngleRad,EndAngleRad: Single; AColor: TBGRAPixel; ADrawChord: boolean; AOuterFillColor, AInnerFillColor: TBGRAPixel);

    procedure Pie(cx,cy,rx,ry: single; const StartPoint,EndPoint: TPointF; AColor: TBGRAPixel; AFillColor: TBGRAPixel); overload;
    procedure Pie(cx,cy,rx,ry: single; StartAngleRad,EndAngleRad: Single; AColor: TBGRAPixel; AFillColor: TBGRAPixel); overload;
    procedure PieInRect(r: TRect; StartAngleRad,EndAngleRad: Single; AColor: TBGRAPixel; AFillColor: TBGRAPixel);
    procedure PieLinearColor(cx,cy,rx,ry: single; const StartPoint,EndPoint: TPointF; AColor: TBGRAPixel; AOuterFillColor, AInnerFillColor: TBGRAPixel); overload;
    procedure PieLinearColor(cx,cy,rx,ry: single; StartAngleRad,EndAngleRad: Single; AColor: TBGRAPixel; AOuterFillColor, AInnerFillColor: TBGRAPixel); overload;
    procedure PieLinearColorInRect(r: TRect; StartAngleRad,EndAngleRad: Single; AColor: TBGRAPixel; AOuterFillColor, AInnerFillColor: TBGRAPixel);

    procedure Rectangle(r: TRect; AColor: TBGRAPixel); overload;
    procedure Rectangle(r: TRect; AColor: TBGRAPixel; AFillColor: TBGRAPixel); overload;
    procedure Rectangle(x1,y1,x2,y2: single; AColor: TBGRAPixel); overload;
    procedure Rectangle(x1,y1,x2,y2: single; AColor: TBGRAPixel; AFillColor: TBGRAPixel); overload;
    procedure Rectangle(x1,y1,x2,y2: single; AColor: TBGRAPixel; w: single; APixelCenteredCoordinates: boolean = true); overload;
    procedure Rectangle(x1,y1,x2,y2: single; AColor: TBGRAPixel; w: single; AFillColor: TBGRAPixel; APixelCenteredCoordinates: boolean = true); overload;
    procedure RectangleWithin(x1,y1,x2,y2: single; ABorderColor: TBGRAPixel; w: single; AFillColor: TBGRAPixel; APixelCenteredCoordinates: boolean = true); overload;
    procedure RectangleWithin(r: TRect; ABorderColor: TBGRAPixel; w: single; AFillColor: TBGRAPixel); overload;
    procedure FillRect(x1,y1,x2,y2: single; AColor: TBGRAPixel; APixelCenteredCoordinates: boolean = true); overload;
    procedure FillRect(r: TRect; AColor: TBGRAPixel); overload;
    procedure FillRect(r: TRectF; AColor: TBGRAPixel; APixelCenteredCoordinates: boolean = false); overload;
    procedure FillRect(r: TRect; AScanner: IBGRAScanner); virtual; overload;
    procedure RoundRect(x1,y1,x2,y2,rx,ry: single; ABorderColor: TBGRAPixel; options: TRoundRectangleOptions = []); overload;
    procedure RoundRect(x1,y1,x2,y2,rx,ry: single; ABorderColor,AFillColor: TBGRAPixel; options: TRoundRectangleOptions = []); overload;
    procedure FillRoundRect(x,y,x2,y2,rx,ry: single; AFillColor: TBGRAPixel; options: TRoundRectangleOptions = []; APixelCenteredCoordinates: boolean = true);

    procedure Frame3D(var bounds: TRect; width: integer; Style: TGraphicsBevelCut); overload;
    procedure Frame3D(var bounds: TRect; width: integer;
      Style: TGraphicsBevelCut; LightColor: TBGRAPixel; ShadowColor: TBGRAPixel); overload;

    procedure PutImage(x,y: single; ATexture: IBGLTexture; AAlpha: byte = 255); overload;
    procedure PutImage(x,y: single; ATexture: IBGLTexture; AColor: TBGRAPixel); overload;
    procedure StretchPutImage(x,y,w,h: single; ATexture: IBGLTexture; AAlpha: byte = 255); overload;
    procedure StretchPutImage(x,y,w,h: single; ATexture: IBGLTexture; AColor: TBGRAPixel); overload;
    procedure StretchPutImage(r: TRect; ATexture: IBGLTexture; AAlpha: byte = 255); overload;
    procedure StretchPutImage(r: TRect; ATexture: IBGLTexture; AColor: TBGRAPixel); overload;
    procedure PutImageAngle(x,y: single; ATexture: IBGLTexture; angleDeg: single; AAlpha: byte = 255); overload;
    procedure PutImageAngle(x,y: single; ATexture: IBGLTexture; angleDeg: single; AColor: TBGRAPixel); overload;
    procedure PutImageAffine(const Origin, HAxis, VAxis: TPointF; ATexture: IBGLTexture; AAlpha: byte = 255); overload;
    procedure PutImageAffine(const Origin, HAxis, VAxis: TPointF; ATexture: IBGLTexture; AColor: TBGRAPixel); overload;
    procedure PutImageAffine(x,y: single; ATexture: IBGLTexture; const AMatrix: TAffineMatrix; AAlpha: byte = 255); overload;
    procedure PutImageAffine(x,y: single; ATexture: IBGLTexture; const AMatrix: TAffineMatrix; AColor: TBGRAPixel); overload;

    procedure Translate(x,y: single); virtual;
    procedure Scale(sx,sy: single); virtual;
    procedure RotateDeg(angleCW: single); virtual;
    procedure RotateRad(angleCCW: single); virtual;
    procedure ResetTransform; virtual;

    procedure NoClip;
    property Width: integer read FWidth write SetWidth;
    property Height: integer read FHeight write SetHeight;
    property ClipRect: TRect read GetClipRect write SetClipRect;
    property Matrix: TAffineMatrix read GetMatrix write SetMatrix;
    property BlendMode: TOpenGLBlendMode read GetBlendMode write SetBlendMode;
  end;

  { TBGLPath }

  TBGLPath = class(TBGRAPath)
  private
    FGLDrawProcColor: TBGRAPixel;
    FGLDrawProcCanvas: TBGLCustomCanvas;
    procedure GLDrawProc(const APoints: array of TPointF; AClosed: boolean);
  public
    procedure stroke(ACanvas: TBGLCustomCanvas; AColor: TBGRAPixel; AAcceptedDeviation: single = 0.1); overload;
    procedure fillConvex(ACanvas: TBGLCustomCanvas; AColor: TBGRAPixel; AAcceptedDeviation: single = 0.1; APixelCenteredCoordinates: boolean = true);
  end;

implementation

uses Math, Types, BGRAGradientScanner;

{ TBGLPath }

procedure TBGLPath.GLDrawProc(const APoints: array of TPointF; AClosed: boolean
  );
begin
  if AClosed then
    FGLDrawProcCanvas.Polygons(APoints, FGLDrawProcColor)
  else
    FGLDrawProcCanvas.Polylines(APoints, FGLDrawProcColor);
end;

procedure TBGLPath.stroke(ACanvas: TBGLCustomCanvas; AColor: TBGRAPixel; AAcceptedDeviation: single);
begin
  FGLDrawProcColor := AColor;
  FGLDrawProcCanvas := ACanvas;
  InternalDraw(@GLDrawProc, AAcceptedDeviation);
end;

procedure TBGLPath.fillConvex(ACanvas: TBGLCustomCanvas; AColor: TBGRAPixel; AAcceptedDeviation: single; APixelCenteredCoordinates: boolean);
begin
  ACanvas.FillPolyConvex(ToPoints(AAcceptedDeviation),AColor,APixelCenteredCoordinates);
end;

{ TBGLCustomCanvas }

function TBGLCustomCanvas.ComputeEllipseC(r: TRect; AHasBorder: boolean; out
  cx, cy, rx, ry: single): boolean;
begin
  if (r.right = r.left) or (r.bottom = r.top) then
  begin
    cx := r.left;
    cy := r.top;
    rx := 0;
    ry := 0;
    exit;
  end;
  SwapRect(r);
  cx := (r.left+r.right-1)*0.5;
  cy := (r.top+r.bottom-1)*0.5;
  rx := (r.right-r.left)*0.5;
  ry := (r.bottom-r.top)*0.5;
  if AHasBorder then
  begin
    rx -= 0.5;
    if rx < 0 then rx := 0;
    ry -= 0.5;
    if ry < 0 then ry := 0;
  end;
  result := true;
end;

procedure TBGLCustomCanvas.SetWidth(AValue: integer);
begin
  if FWidth=AValue then Exit;
  FWidth:=AValue;
end;

procedure TBGLCustomCanvas.SetHeight(AValue: integer);
begin
  if FHeight=AValue then Exit;
  FHeight:=AValue;
end;

function TBGLCustomCanvas.GetClipRect: TRect;
begin
  if FNoClip then
    result := rect(0,0,Width,Height)
  else
    result := FClipRect;
end;

procedure TBGLCustomCanvas.SetClipRect(AValue: TRect);
begin
  SwapRect(AValue);
  with ClipRect do
    if (AValue.left = left) and (AValue.top = top) and (AValue.bottom = bottom)
     and (AValue.right = right) then exit;

  if (AValue.Left = 0) and (AValue.Top = 0) and
    (AValue.Right = Width) and (AValue.Bottom = Height) then
    NoClip
    else
  begin
    FClipRect := AValue;
    EnableScissor(FClipRect);
  end;
end;

procedure TBGLCustomCanvas.NoClip;
begin
  FClipRect := rect(0,0,Width,Height);
  FNoClip := true;
  DisableScissor;
end;

constructor TBGLCustomCanvas.Create;
begin
  FNoClip:= true;
end;

procedure TBGLCustomCanvas.FillTriangles(const APoints: array of TPointF;
  AColor: TBGRAPixel; APixelCenteredCoordinates: boolean);
var
  i: NativeInt;
  ofs: TPointF;
begin
  if (length(APoints) < 3) or (AColor.alpha = 0) then exit;
  InternalStartBlendTriangles;
  InternalSetColor(AColor);
  if APixelCenteredCoordinates then ofs := PointF(0.5,0.5) else ofs := PointF(0,0);
  for i := 0 to length(APoints) - (length(APoints) mod 3) - 1 do
    InternalContinueShape(APoints[i]+ofs);
  InternalEndBlendTriangles;
end;

procedure TBGLCustomCanvas.FillTrianglesLinearColor(const APoints: array of TPointF;
  const AColors: array of TBGRAPixel; APixelCenteredCoordinates: boolean);
var
  i: NativeInt;
  ofs: TPointF;
begin
  if length(APoints) < 3 then exit;
  if length(AColors)<>length(APoints) then
    raise exception.Create('Length of APoints and AColors do not match');
  InternalStartBlendTriangles;
  if APixelCenteredCoordinates then ofs := PointF(0.5,0.5) else ofs := PointF(0,0);
  for i := 0 to length(APoints) - (length(APoints) mod 3) - 1 do
  begin
    InternalSetColor(AColors[i]);
    InternalContinueShape(APoints[i]+ofs);
  end;
  InternalEndBlendTriangles;
end;

procedure TBGLCustomCanvas.FillQuads(const APoints: array of TPointF;
  AColor: TBGRAPixel; APixelCenteredCoordinates: boolean);
var
  i: NativeInt;
  ofs: TPointF;
begin
  if (length(APoints) < 4) or (AColor.alpha = 0) then exit;
  InternalStartBlendQuads;
  InternalSetColor(AColor);
  if APixelCenteredCoordinates then ofs := PointF(0.5,0.5) else ofs := PointF(0,0);
  for i := 0 to length(APoints) - (length(APoints) and 3) - 1 do
    InternalContinueShape(APoints[i]+ofs);
  InternalEndBlendQuads;
end;

procedure TBGLCustomCanvas.FillQuadsLinearColor(const APoints: array of TPointF;
  const AColors: array of TBGRAPixel; APixelCenteredCoordinates: boolean);
var
  i: NativeInt;
  ofs: TPointF;
begin
  if length(APoints) < 4 then exit;
  if length(AColors)<>length(APoints) then
    raise exception.Create('Length of APoints and AColors do not match');
  InternalStartBlendQuads;
  if APixelCenteredCoordinates then ofs := PointF(0.5,0.5) else ofs := PointF(0,0);
  for i := 0 to length(APoints) - (length(APoints) and 3) - 1 do
  begin
    InternalSetColor(AColors[i]);
    InternalContinueShape(APoints[i]+ofs);
  end;
  InternalEndBlendQuads;
end;

procedure TBGLCustomCanvas.PutPixels(const APoints: array of TPointF;
  AColor: TBGRAPixel);
var
  i: NativeInt;
begin
  if length(APoints) = 0 then exit;
  InternalStartBlend;
  InternalSetColor(AColor);
  InternalStartPutPixel(APoints[0]);
  for i := 1 to high(APoints) do
    InternalContinueShape(APoints[i]);
  InternalEndBlend;
end;

procedure TBGLCustomCanvas.PutPixels(const APoints: array of TPointF;
  const AColors: array of TBGRAPixel);
var
  i: NativeInt;
begin
  if length(APoints) = 0 then exit;
  InternalStartBlend;
  InternalSetColor(AColors[0]);
  InternalStartPutPixel(APoints[0]);
  for i := 1 to high(APoints) do
  begin
    InternalSetColor(AColors[i]);
    InternalContinueShape(APoints[i]);
  end;
  InternalEndBlend;
end;

procedure TBGLCustomCanvas.FillTrianglesFan(const APoints: array of TPointF;
  ACenterColor, ABorderColor: TBGRAPixel; APixelCenteredCoordinates: boolean);
var
  i: NativeInt;
  firstPoint: boolean;
  ofs: TPointF;
begin
  if (length(APoints) < 3) or ((ACenterColor.alpha = 0) and (ABorderColor.alpha = 0)) then exit;
  InternalStartBlend;
  firstPoint := true;
  if APixelCenteredCoordinates then ofs := PointF(0.5,0.5) else ofs := PointF(0,0);
  for i := 0 to high(APoints) do
  begin
    if isEmptyPointF(APoints[i]) then
    begin
      if not firstPoint then
      begin
        InternalEndShape;
        firstPoint := true;
      end;
    end else
    begin
      if firstPoint then
      begin
        InternalSetColor(ACenterColor);
        InternalStartTriangleFan(APoints[i]+ofs);
        InternalSetColor(ABorderColor);
        firstPoint := false;
      end else
        InternalContinueShape(APoints[i]+ofs);
    end;
  end;
  if not firstPoint then InternalEndShape;
  InternalEndBlend;
end;

procedure TBGLCustomCanvas.Polylines(const APoints: array of TPointF;
  AColor: TBGRAPixel; ADrawLastPoints: boolean);
const
  STATE_START = 0;  //nothing defined
  STATE_SECOND = 1; //prevPoint defined and is the first point
  STATE_AFTER = 2;  //newPoint defined and is the lastest point, prevPoint is the point before that
var
  i: NativeInt;
  state: NativeInt;
  prevPoint,newPoint,v,ofs: TPointF;
  len: single;

  procedure Flush;
  begin
    case state of
      STATE_SECOND: begin
        InternalStartPutPixel(prevPoint);
        InternalEndShape;
      end;
      STATE_AFTER:
      begin
        v := newPoint-prevPoint;
        len := VectLen(v);
        if len > 0 then
        begin
          v := v*(1/len);
          if ADrawLastPoints then
            InternalContinueShape(newPoint + v*0.5 + ofs)
          else
            InternalContinueShape(newPoint - v*0.5 + ofs);
        end;
        InternalEndShape;
      end;
    end;
    state := STATE_START;
  end;

begin
  if (length(APoints) = 0) or (AColor.alpha = 0) then exit;
  InternalStartBlend;
  InternalSetColor(AColor);
  prevPoint := PointF(0,0);
  newPoint := PointF(0,0);
  state := STATE_START;
  ofs := PointF(0.5,0.5);
  for i := 0 to high(APoints) do
  begin
    if isEmptyPointF(APoints[i]) then
    begin
      Flush;
    end else
    begin
      if state = STATE_START then
      begin
        state := STATE_SECOND;
        prevPoint := APoints[i];
      end else
      if APoints[i] <> prevPoint then
      begin
        if state = STATE_SECOND then
        begin
          newPoint := APoints[i];
          v := newPoint-prevPoint;
          len := VectLen(v);
          if len > 0 then
          begin
            v := v*(1/len);
            InternalStartPolyline(prevPoint - v*0.5 + ofs);
            state := STATE_AFTER;
          end;
        end else
        begin
          InternalContinueShape(newPoint + ofs);
          prevPoint := newPoint;
          newPoint := APoints[i];
        end;
      end;
    end;
  end;
  Flush;
  InternalEndBlend;
end;

procedure TBGLCustomCanvas.Polygons(const APoints: array of TPointF;
  AColor: TBGRAPixel);
const
  STATE_START = 0;  //nothing defined
  STATE_SECOND = 1; //prevPoint defined and is the first point
  STATE_AFTER = 2;  //newPoint defined and is the lastest point, prevPoint is the point before that
var
  i: NativeInt;
  state: NativeInt;
  prevPoint,newPoint: TPointF;
  ofs: TPointF;

  procedure Flush;
  begin
    case state of
      STATE_SECOND: begin
        InternalStartPutPixel(prevPoint);
        InternalEndShape;
      end;
      STATE_AFTER:
      begin
        InternalContinueShape(newPoint + ofs);
        InternalEndShape;
      end;
    end;
    state := STATE_START;
  end;

begin
  if (length(APoints) = 0) or (AColor.alpha = 0) then exit;
  InternalStartBlend;
  InternalSetColor(AColor);
  prevPoint := PointF(0,0);
  newPoint := PointF(0,0);
  state := STATE_START;
  ofs := PointF(0.5,0.5);
  for i := 0 to high(APoints) do
  begin
    if isEmptyPointF(APoints[i]) then
    begin
      Flush;
    end else
    begin
      if state = STATE_START then
      begin
        state := STATE_SECOND;
        prevPoint := APoints[i];
      end else
      if APoints[i] <> prevPoint then
      begin
        if state = STATE_SECOND then
        begin
          InternalStartPolygon(prevPoint+ofs);
          newPoint := APoints[i];
          state := STATE_AFTER;
        end else
        begin
          InternalContinueShape(newPoint+ofs);
          prevPoint := newPoint;
          newPoint := APoints[i];
        end;
      end;
    end;
  end;
  Flush;
  InternalEndBlend;
end;

procedure TBGLCustomCanvas.FillRect(r: TRect; AScanner: IBGRAScanner);
var
  bmp: TBGLCustomBitmap;
  yb,bandHeight,bandY: NativeInt;
  tx: integer;
begin
  SwapRect(r);
  if (r.right = r.left) or (r.bottom = r.top) then exit;
  tx := r.right-r.left;
  bandHeight := 65536 div tx;
  if bandHeight <= 2 then bandHeight := 2;
  bandHeight := GetPowerOfTwo(bandHeight);
  bmp := BGLBitmapFactory.Create(tx,bandHeight);
  bmp.Texture.ResampleFilter := orfBox;
  bandY := (r.Bottom-1-r.top) mod bandHeight;
  for yb := r.bottom-1 downto r.top do
  begin
    AScanner.ScanMoveTo(r.left,yb);
    AScanner.ScanPutPixels(bmp.ScanLine[bandY],tx,dmSet);
    bmp.InvalidateBitmap;
    if bandY = 0 then
    begin
      bmp.Texture.Draw(r.left,yb);
      bandY := bandHeight-1;
    end else
      dec(bandY);
  end;
  bmp.Free;
end;

procedure TBGLCustomCanvas.DrawPath(APath: TBGLPath; c: TBGRAPixel);
begin
  APath.stroke(self, c);
end;

procedure TBGLCustomCanvas.FillPathConvex(APath: TBGLPath; c: TBGRAPixel; APixelCenteredCoordinates: boolean);
begin
  APath.fillConvex(self, c, 0.1, APixelCenteredCoordinates);
end;

procedure TBGLCustomCanvas.SwapRect(var r: TRect);
var
  temp: LongInt;
begin
  if (r.Right < r.left) then
  begin
    temp := r.Left;
    r.left := r.right;
    r.right := temp;
  end;
  if (r.bottom < r.top) then
  begin
    temp := r.top;
    r.top:= r.bottom;
    r.bottom:= temp;
  end;
end;

procedure TBGLCustomCanvas.SwapRect(var x1, y1, x2, y2: single);
var
  temp: single;
begin
  if (x2 < x1) then
  begin
    temp := x1;
    x1 := x2;
    x2 := temp;
  end;
  if (y2 < y1) then
  begin
    temp := y1;
    y1 := y2;
    y2 := temp;
  end;
end;

procedure TBGLCustomCanvas.InternalArc(cx, cy, rx, ry: single; const StartPoint,
  EndPoint: TPointF; ABorderColor, AOuterFillColor,ACenterFillColor: TBGRAPixel;
  AOptions: TArcOptions; ADrawChord: boolean = false);
var angle1,angle2: single;
begin
  if (rx = 0) or (ry = 0) then exit;
  angle1 := arctan2(-(StartPoint.y-cy)/ry,(StartPoint.x-cx)/rx);
  angle2 := arctan2(-(EndPoint.y-cy)/ry,(EndPoint.x-cx)/rx);
  if angle1 = angle2 then angle2 := angle1+2*Pi;
  InternalArc(cx,cy,rx,ry, angle1,angle2,
              ABorderColor,AOuterFillColor,ACenterFillColor, AOptions, ADrawChord);
end;

procedure TBGLCustomCanvas.InternalArc(cx, cy, rx, ry: single;
  StartAngleRad, EndAngleRad: Single; ABorderColor,
  AOuterFillColor,ACenterFillColor: TBGRAPixel; AOptions: TArcOptions;
  ADrawChord: boolean = false);
var
  pts,ptsFill: array of TPointF;
  temp: single;
begin
  if (rx = 0) or (ry = 0) then exit;
  if ADrawChord then AOptions := AOptions+[aoClosePath];
  if not (aoFillPath in AOptions) then
  begin
    AOuterFillColor := BGRAPixelTransparent;
    ACenterFillColor := BGRAPixelTransparent;
  end;

  if (ABorderColor.alpha = 0) and (AOuterFillColor.alpha = 0) and (ACenterFillColor.alpha = 0) then exit;

  if abs(StartAngleRad-EndAngleRad) >= 2*PI - 1e-6 then
  begin
    Ellipse(cx,cy,rx,ry,ABorderColor);
    FillEllipseLinearColor(cx,cy,rx,ry,AOuterFillColor,ACenterFillColor);
    if aoPie in AOptions then
      Line(cx,cy,cx+cos(StartAngleRad)*rx,cy-sin(StartAngleRad)*ry,ABorderColor,False);
    exit;
  end;

  if EndAngleRad < StartAngleRad then
  begin
    temp := StartAngleRad;
    StartAngleRad:= EndAngleRad;
    EndAngleRad:= temp;
  end;

  pts := ComputeArcRad(cx,cy,rx,ry,StartAngleRad,EndAngleRad);
  if aoPie in AOptions then
    pts := ConcatPointsF([PointsF([PointF(cx,cy)]),pts]);
  if (ACenterFillColor.alpha <> 0) or (AOuterFillColor.alpha <> 0) then
  begin
    if not (aoPie in AOptions) and (length(pts)>=2) then ptsFill := ConcatPointsF([PointsF([(pts[0]+pts[high(pts)])*0.5]),pts])
      else ptsFill := pts;
    FillTrianglesFan(ptsFill, ACenterFillColor,AOuterFillColor);
  end;
  if ABorderColor.alpha <> 0 then
  begin
    if [aoPie,aoClosePath]*AOptions <> [] then
      Polygons(pts, ABorderColor)
    else
      Polylines(pts, ABorderColor, true);
  end;
end;

procedure TBGLCustomCanvas.InternalArcInRect(r: TRect; StartAngleRad,
  EndAngleRad: Single; ABorderColor, AOuterFillColor,ACenterFillColor: TBGRAPixel;
  AOptions: TArcOptions; ADrawChord: boolean = false);
begin
  if r.right = r.left then exit;
  if r.bottom = r.top then exit;
  SwapRect(r);
  InternalArc((r.left+r.right-1)/2,(r.top+r.bottom-1)/2,
             (r.right-r.left-1)/2,(r.bottom-r.top-1)/2,
             StartAngleRad,EndAngleRad,
             ABorderColor,AOuterFillColor,ACenterFillColor,
             AOptions, ADrawChord);
end;

procedure TBGLCustomCanvas.FillTriangleLinearColor(pt1, pt2, pt3: TPointF; c1,
  c2, c3: TBGRAPixel; APixelCenteredCoordinates: boolean);
begin
  FillTrianglesLinearColor([pt1,pt2,pt3],[c1,c2,c3],APixelCenteredCoordinates);
end;

procedure TBGLCustomCanvas.FillQuadLinearColor(pt1, pt2, pt3, pt4: TPointF; c1,
  c2, c3, c4: TBGRAPixel; APixelCenteredCoordinates: boolean);
begin
  FillQuadsLinearColor([pt1,pt2,pt3,pt4],[c1,c2,c3,c4],APixelCenteredCoordinates);
end;

procedure TBGLCustomCanvas.FillPolyConvex(const APoints: array of TPointF;
  AColor: TBGRAPixel; APixelCenteredCoordinates: boolean);
begin
  FillTrianglesFan(APoints,AColor,AColor,APixelCenteredCoordinates);
end;

procedure TBGLCustomCanvas.Line(x1, y1, x2, y2: single; AColor: TBGRAPixel; ADrawLastPoint: boolean);
var pts: array of TPointF;
begin
  setlength(pts,2);
  pts[0] := PointF(x1,y1);
  pts[1] := PointF(x2,y2);
  Polylines(pts,AColor,ADrawLastPoint);
end;

procedure TBGLCustomCanvas.Line(p1, p2: TPointF; AColor: TBGRAPixel; ADrawLastPoint: boolean);
var pts: array of TPointF;
begin
  setlength(pts,2);
  pts[0] := p1;
  pts[1] := p2;
  Polylines(pts,AColor,ADrawLastPoint);
end;

procedure TBGLCustomCanvas.FillRectLinearColor(r: TRect;
  ATopLeftColor, ATopRightColor, ABottomRightColor, ABottomLeftColor: TBGRAPixel);
begin
  FillRectLinearColor(r.left,r.top,r.right,r.bottom,
       ATopLeftColor, ATopRightColor, ABottomRightColor, ABottomLeftColor,
       False);
end;

procedure TBGLCustomCanvas.FillRectLinearColor(x1, y1, x2, y2: single;
  ATopLeftColor, ATopRightColor, ABottomRightColor,
  ABottomLeftColor: TBGRAPixel; APixelCenteredCoordinates: boolean);
begin
  FillQuadLinearColor(PointF(x1,y1),PointF(x2,y1),
       PointF(x2,y2),PointF(x1,y2),
       ATopLeftColor, ATopRightColor, ABottomRightColor, ABottomLeftColor,
       APixelCenteredCoordinates);
end;

procedure TBGLCustomCanvas.Ellipse(cx, cy, rx, ry: single; AColor: TBGRAPixel);
begin
  if AColor.alpha = 0 then exit;
  Polygons(ComputeEllipse(cx,cy,rx,ry),AColor);
end;

procedure TBGLCustomCanvas.EllipseInRect(r: TRect; AColor: TBGRAPixel);
var cx,cy,rx,ry: single;
begin
  if not ComputeEllipseC(r,True,cx,cy,rx,ry) then exit;
  Ellipse(cx,cy,rx,ry, AColor);
end;

procedure TBGLCustomCanvas.FillEllipse(cx, cy, rx, ry: single; AColor: TBGRAPixel; APixelCenteredCoordinates: boolean);
begin
  if AColor.alpha = 0 then exit;
  FillTrianglesFan(ComputeEllipse(cx,cy,rx,ry),AColor,AColor,APixelCenteredCoordinates);
end;

procedure TBGLCustomCanvas.FillEllipseInRect(r: TRect; AColor: TBGRAPixel);
var cx,cy,rx,ry: single;
begin
  if not ComputeEllipseC(r,False,cx,cy,rx,ry) then exit;
  FillEllipse(cx,cy,rx,ry, AColor);
end;

procedure TBGLCustomCanvas.FillEllipseLinearColor(cx, cy, rx, ry: single;
  AOuterColor, AInnerColor: TBGRAPixel; APixelCenteredCoordinates: boolean);
begin
  if (AOutercolor.alpha = 0) and (AInnercolor.alpha = 0) then exit;
  FillTrianglesFan(ConcatPointsF([PointsF([PointF(cx,cy)]),ComputeEllipse(cx,cy,rx,ry)]),AInnercolor,AOutercolor,APixelCenteredCoordinates);
end;

procedure TBGLCustomCanvas.FillEllipseLinearColorInRect(r: TRect; AOuterColor,
  AInnerColor: TBGRAPixel);
var cx,cy,rx,ry: single;
begin
  if not ComputeEllipseC(r,False,cx,cy,rx,ry) then exit;
  FillEllipseLinearColor(cx,cy,rx,ry, AOutercolor,AInnercolor);
end;

procedure TBGLCustomCanvas.Arc(cx, cy, rx, ry: single; const StartPoint,
  EndPoint: TPointF; AColor: TBGRAPixel; ADrawChord: boolean; AFillColor: TBGRAPixel);
begin
  InternalArc(cx,cy,rx,ry,StartPoint,EndPoint,AColor,AFillColor,AFillColor,[aoFillPath],ADrawChord);
end;

procedure TBGLCustomCanvas.Arc(cx, cy, rx, ry: single; StartAngleRad,
  EndAngleRad: Single; AColor: TBGRAPixel; ADrawChord: boolean; AFillColor: TBGRAPixel);
begin
  InternalArc(cx,cy,rx,ry,StartAngleRad,EndAngleRad,AColor,AFillColor,AFillColor,[aoFillPath],ADrawChord);
end;

procedure TBGLCustomCanvas.ArcInRect(r: TRect; StartAngleRad,
  EndAngleRad: Single; AColor: TBGRAPixel; ADrawChord: boolean; AFillColor: TBGRAPixel);
var cx,cy,rx,ry: single;
begin
  if not ComputeEllipseC(r,True,cx,cy,rx,ry) then exit;
  Arc(cx,cy,rx,ry,StartAngleRad,EndAngleRad, AColor,ADrawChord, AFillColor);
end;

procedure TBGLCustomCanvas.ArcLinearColor(cx, cy, rx, ry: single;
  const StartPoint, EndPoint: TPointF; AColor: TBGRAPixel; ADrawChord: boolean; AOuterFillColor,
  AInnerFillColor: TBGRAPixel);
begin
  InternalArc(cx,cy,rx,ry,StartPoint,EndPoint,AColor,AOuterFillColor,AInnerFillColor,[aoFillPath],ADrawChord);
end;

procedure TBGLCustomCanvas.ArcLinearColor(cx, cy, rx, ry: single;
  StartAngleRad, EndAngleRad: Single; AColor: TBGRAPixel; ADrawChord: boolean; AOuterFillColor,
  AInnerFillColor: TBGRAPixel);
begin
  InternalArc(cx,cy,rx,ry,StartAngleRad,EndAngleRad,AColor,AOuterFillColor,AInnerFillColor,[aoFillPath],ADrawChord);
end;

procedure TBGLCustomCanvas.ArcLinearColorInRect(r: TRect; StartAngleRad,
  EndAngleRad: Single; AColor: TBGRAPixel; ADrawChord: boolean; AOuterFillColor,
  AInnerFillColor: TBGRAPixel);
var cx,cy,rx,ry: single;
begin
  if not ComputeEllipseC(r,True,cx,cy,rx,ry) then exit;
  ArcLinearColor(cx,cy,rx,ry,StartAngleRad,EndAngleRad, AColor,ADrawChord, AOuterFillColor,AInnerFillColor);
end;

procedure TBGLCustomCanvas.Pie(cx, cy, rx, ry: single; const StartPoint,
  EndPoint: TPointF; AColor: TBGRAPixel; AFillColor: TBGRAPixel);
begin
  InternalArc(cx,cy,rx,ry,StartPoint,EndPoint,AColor,AFillColor,AFillColor,[aoFillPath,aoPie]);
end;

procedure TBGLCustomCanvas.Pie(cx, cy, rx, ry: single; StartAngleRad,
  EndAngleRad: Single; AColor: TBGRAPixel; AFillColor: TBGRAPixel);
begin
  InternalArc(cx,cy,rx,ry,StartAngleRad,EndAngleRad,AColor,AFillColor,AFillColor,[aoFillPath,aoPie]);
end;

procedure TBGLCustomCanvas.PieInRect(r: TRect; StartAngleRad,
  EndAngleRad: Single; AColor: TBGRAPixel; AFillColor: TBGRAPixel);
var cx,cy,rx,ry: single;
begin
  if not ComputeEllipseC(r,True,cx,cy,rx,ry) then exit;
  Pie(cx,cy,rx,ry,StartAngleRad,EndAngleRad, AColor,AFillColor);
end;

procedure TBGLCustomCanvas.PieLinearColor(cx, cy, rx, ry: single;
  const StartPoint, EndPoint: TPointF; AColor: TBGRAPixel; AOuterFillColor,
  AInnerFillColor: TBGRAPixel);
begin
  InternalArc(cx,cy,rx,ry,StartPoint,EndPoint,AColor,AOuterFillColor,AInnerFillColor,[aoFillPath,aoPie]);
end;

procedure TBGLCustomCanvas.PieLinearColor(cx, cy, rx, ry: single;
  StartAngleRad, EndAngleRad: Single; AColor: TBGRAPixel; AOuterFillColor,
  AInnerFillColor: TBGRAPixel);
begin
  InternalArc(cx,cy,rx,ry,StartAngleRad,EndAngleRad,AColor,AOuterFillColor,AInnerFillColor,[aoFillPath,aoPie]);
end;

procedure TBGLCustomCanvas.PieLinearColorInRect(r: TRect; StartAngleRad,
  EndAngleRad: Single; AColor: TBGRAPixel; AOuterFillColor,
  AInnerFillColor: TBGRAPixel);
var cx,cy,rx,ry: single;
begin
  if not ComputeEllipseC(r,True,cx,cy,rx,ry) then exit;
  PieLinearColor(cx,cy,rx,ry,StartAngleRad,EndAngleRad, AColor,AOuterFillColor,AInnerFillColor);
end;

procedure TBGLCustomCanvas.EllipseLinearColor(cx, cy, rx, ry: single; AColor: TBGRAPixel;
  AOuterFillColor, AInnerFillColor: TBGRAPixel);
begin
  if (rx>1) and (ry>1) then
    FillEllipseLinearColor(cx,cy,rx-0.5,ry-0.5,AOuterFillColor,AInnerFillColor);
  Ellipse(cx,cy,rx,ry,AColor);
end;

procedure TBGLCustomCanvas.EllipseLinearColorInRect(r: TRect; AColor: TBGRAPixel;
  AOuterFillColor, AInnerFillColor: TBGRAPixel);
var cx,cy,rx,ry: single;
begin
  if not ComputeEllipseC(r,True,cx,cy,rx,ry) then exit;
  FillEllipseLinearColor(cx,cy,rx,ry, AOuterFillColor,AInnerFillColor);
  EllipseInRect(r,AColor);
end;

procedure TBGLCustomCanvas.Ellipse(cx, cy, rx, ry: single; AColor: TBGRAPixel;
  AFillColor: TBGRAPixel);
begin
  EllipseLinearColor(cx,cy,rx,ry,AColor,AFillColor,AFillColor);
end;

procedure TBGLCustomCanvas.EllipseInRect(r: TRect; AColor: TBGRAPixel;
  AFillColor: TBGRAPixel);
begin
  EllipseLinearColorInRect(r, AColor, AFillColor, AFillColor);
end;

procedure TBGLCustomCanvas.Rectangle(r: TRect; AColor: TBGRAPixel);
begin
  Rectangle(r,AColor,BGRAPixelTransparent);
end;

procedure TBGLCustomCanvas.Rectangle(r: TRect; AColor: TBGRAPixel;
  AFillColor: TBGRAPixel);
begin
  SwapRect(r);
  if r.left=r.right then exit;
  if r.top=r.bottom then exit;
  Rectangle(r.left,r.top,r.right-1,r.bottom-1,AColor,AFillColor);
end;

procedure TBGLCustomCanvas.Rectangle(x1, y1, x2, y2: single; AColor: TBGRAPixel);
begin
  Rectangle(x1,y1,x2,y2,AColor,1);
end;

procedure TBGLCustomCanvas.Rectangle(x1, y1, x2, y2: single;
  AColor: TBGRAPixel; AFillColor: TBGRAPixel);
begin
  Rectangle(x1,y1,x2,y2,AColor,1,AFillColor);
end;

procedure TBGLCustomCanvas.Rectangle(x1, y1, x2, y2: single;
  AColor: TBGRAPixel; w: single; APixelCenteredCoordinates: boolean);
var hw: single;
begin
  SwapRect(x1,y1,x2,y2);
  hw := w*0.5;
  if (x2-x1 > w) and (y2-y1 > w) then
    FillQuads(PointsF([PointF(x1-hw,y1-hw),PointF(x2+hw,y1-hw),PointF(x2+hw,y1+hw),PointF(x1-hw,y1+hw),
      PointF(x1-hw,y2-hw),PointF(x2+hw,y2-hw),PointF(x2+hw,y2+hw),PointF(x1-hw,y2+hw),
      PointF(x1-hw,y1+hw),PointF(x1+hw,y1+hw),PointF(x1+hw,y2-hw),PointF(x1-hw,y2-hw),
      PointF(x2-hw,y1+hw),PointF(x2+hw,y1+hw),PointF(x2+hw,y2-hw),PointF(x2-hw,y2-hw)]), AColor,
      APixelCenteredCoordinates)
  else
    FillQuads(PointsF([PointF(x1-hw,y1-hw),PointF(x2+hw,y1-hw),PointF(x2+hw,y2+hw),PointF(x1-hw,y2+hw)]),AColor,
    APixelCenteredCoordinates);
end;

procedure TBGLCustomCanvas.Rectangle(x1, y1, x2, y2: single;
  AColor: TBGRAPixel; w: single; AFillColor: TBGRAPixel; APixelCenteredCoordinates: boolean);
begin
  SwapRect(x1,y1,x2,y2);
  if (x2-x1 > w) and (y2-y1 > w) then
    FillRect(x1+0.5*w,y1+0.5*w,x2-0.5*w,y2-0.5*w,AFillColor,APixelCenteredCoordinates);
  Rectangle(x1,y1,x2,y2,AColor,w,APixelCenteredCoordinates);
end;

procedure TBGLCustomCanvas.RectangleWithin(x1, y1, x2, y2: single;
  ABorderColor: TBGRAPixel; w: single; AFillColor: TBGRAPixel;
  APixelCenteredCoordinates: boolean);
begin
  Rectangle(x1+w*0.5,y1+w*0.5,x2-w*0.5,y2-w*0.5, ABorderColor, w, AFillColor,
    APixelCenteredCoordinates);
end;

procedure TBGLCustomCanvas.RectangleWithin(r: TRect; ABorderColor: TBGRAPixel;
  w: single; AFillColor: TBGRAPixel);
begin
  RectangleWithin(r.left,r.top,r.right,r.bottom,ABorderColor,w,AFillColor,false);
end;

procedure TBGLCustomCanvas.RoundRect(x1, y1, x2, y2, rx, ry: single;
  ABorderColor: TBGRAPixel; options: TRoundRectangleOptions);
begin
  RoundRect(x1,y1,x2,y2,rx,ry,ABorderColor,options);
end;

procedure TBGLCustomCanvas.RoundRect(x1, y1, x2, y2, rx, ry: single;
  ABorderColor, AFillColor: TBGRAPixel; options: TRoundRectangleOptions);
const radiusReduction = 1;
begin
  SwapRect(x1,y1,x2,y2);
  rx := abs(rx);
  ry := abs(ry);
  if (AFillColor.alpha <> 0) and (y2-y1 > 1) and (x2-x1 > 1) then
  begin
    if (rx <= radiusReduction) or (ry <= radiusReduction) then
      FillRect(x1+0.5,y1+0.5,x2-0.5,y2-0.5, AFillColor)
    else
      FillPolyConvex(ComputeRoundRect(x1+0.5,y1+0.5,x2-0.5,y2-0.5,rx-radiusReduction,ry-radiusReduction,options),AFillColor);
  end;
  Polygons(ComputeRoundRect(x1,y1,x2,y2,rx,ry,options),ABorderColor);
end;

procedure TBGLCustomCanvas.FillRoundRect(x, y, x2, y2, rx, ry: single;
  AFillColor: TBGRAPixel; options: TRoundRectangleOptions; APixelCenteredCoordinates: boolean);
begin
  if AFillColor.alpha <> 0 then
    FillPolyConvex(ComputeRoundRect(x,y,x2,y2,rx,ry,options),AFillColor,APixelCenteredCoordinates);
end;

procedure TBGLCustomCanvas.FillRect(x1, y1, x2, y2: single; AColor: TBGRAPixel; APixelCenteredCoordinates: boolean);
begin
  FillQuads(PointsF([PointF(x1,y1),PointF(x2,y1),PointF(x2,y2),PointF(x1,y2)]), AColor, APixelCenteredCoordinates);
end;

procedure TBGLCustomCanvas.FillRect(r: TRect; AColor: TBGRAPixel);
begin
  SwapRect(r);
  if r.left=r.right then exit;
  if r.top=r.bottom then exit;
  FillRect(r.left,r.top,r.Right,r.bottom,AColor,False);
end;

procedure TBGLCustomCanvas.FillRect(r: TRectF; AColor: TBGRAPixel;
  APixelCenteredCoordinates: boolean);
begin
  if r.left=r.right then exit;
  if r.top=r.bottom then exit;
  FillRect(r.left,r.top,r.Right,r.bottom,AColor,APixelCenteredCoordinates);
end;

procedure TBGLCustomCanvas.Frame3D(var bounds: TRect; width: integer;
  Style: TGraphicsBevelCut);
begin
  Frame3D(bounds,width,style,ColorToBGRA(clRgbBtnHighlight),ColorToBGRA(clRgbBtnShadow));
end;

procedure TBGLCustomCanvas.Frame3D(var bounds: TRect; width: integer;
  Style: TGraphicsBevelCut; LightColor: TBGRAPixel; ShadowColor: TBGRAPixel);
var temp: TBGRAPixel;
    color1,color2: TBGRAPixel;
begin
  if width <= 0 then exit;
  color1 := LightColor;
  color2 := ShadowColor;
  if Style = bvLowered then
  begin
    temp := color1;
    color1 := color2;
    color2 := temp;
  end;
  if Style in [bvLowered,bvRaised] then
  with bounds do
  begin
    FillTrianglesFan([PointF(Left,Top),PointF(Right,Top),
                      PointF(Right-width,Top+width),PointF(Left+width,Top+width),
                      PointF(Left+width,Bottom-width),PointF(Left,Bottom)],color1,color1, False);
    FillTrianglesFan([PointF(Right,Bottom),PointF(Left,Bottom),
                        PointF(Left+width,Bottom-width),PointF(Right-width,Bottom-width),
                        PointF(Right-width,Top+width),PointF(Right,Top)],color2,color2, false);
  end;
  InflateRect(bounds,-width,-width);
end;

procedure TBGLCustomCanvas.PutImage(x, y: single; ATexture: IBGLTexture;
  AAlpha: byte);
begin
  ATexture.Draw(x,y,AAlpha);
end;

procedure TBGLCustomCanvas.PutImage(x, y: single; ATexture: IBGLTexture;
  AColor: TBGRAPixel);
begin
  ATexture.Draw(x,y,AColor);
end;

procedure TBGLCustomCanvas.StretchPutImage(x, y, w, h: single;
  ATexture: IBGLTexture; AAlpha: byte);
begin
  ATexture.StretchDraw(x,y,w,h, AAlpha);
end;

procedure TBGLCustomCanvas.StretchPutImage(x, y, w, h: single;
  ATexture: IBGLTexture; AColor: TBGRAPixel);
begin
  ATexture.StretchDraw(x,y,w,h, AColor);
end;

procedure TBGLCustomCanvas.StretchPutImage(r: TRect; ATexture: IBGLTexture;
  AAlpha: byte);
begin
  ATexture.StretchDraw(r.left,r.top,r.right-r.left,r.bottom-r.top, AAlpha);
end;

procedure TBGLCustomCanvas.StretchPutImage(r: TRect; ATexture: IBGLTexture;
  AColor: TBGRAPixel);
begin
  ATexture.StretchDraw(r.left,r.top,r.right-r.left,r.bottom-r.top, AColor);
end;

procedure TBGLCustomCanvas.PutImageAngle(x, y: single; ATexture: IBGLTexture;
  angleDeg: single; AAlpha: byte);
begin
  ATexture.DrawAngle(x,y,angleDeg,AAlpha);
end;

procedure TBGLCustomCanvas.PutImageAngle(x, y: single; ATexture: IBGLTexture;
  angleDeg: single; AColor: TBGRAPixel);
begin
  ATexture.DrawAngle(x,y,angleDeg,AColor);
end;

procedure TBGLCustomCanvas.PutImageAffine(const Origin, HAxis, VAxis: TPointF;
  ATexture: IBGLTexture; AAlpha: byte);
begin
  ATexture.DrawAffine(Origin, HAxis, VAxis, AAlpha);
end;

procedure TBGLCustomCanvas.PutImageAffine(const Origin, HAxis, VAxis: TPointF;
  ATexture: IBGLTexture; AColor: TBGRAPixel);
begin
  ATexture.DrawAffine(Origin, HAxis, VAxis, AColor);
end;

procedure TBGLCustomCanvas.PutImageAffine(x, y: single; ATexture: IBGLTexture;
  const AMatrix: TAffineMatrix; AAlpha: byte);
begin
  ATexture.DrawAffine(x,y,AMatrix,AAlpha);
end;

procedure TBGLCustomCanvas.PutImageAffine(x, y: single; ATexture: IBGLTexture;
  const AMatrix: TAffineMatrix; AColor: TBGRAPixel);
begin
  ATexture.DrawAffine(x,y,AMatrix,AColor);
end;

procedure TBGLCustomCanvas.Translate(x, y: single);
begin
  Matrix := Matrix*AffineMatrixTranslation(x,y);
end;

procedure TBGLCustomCanvas.Scale(sx, sy: single);
begin
  Matrix := Matrix*AffineMatrixScale(sx,sy);
end;

procedure TBGLCustomCanvas.RotateDeg(angleCW: single);
begin
  Matrix := Matrix*AffineMatrixRotationDeg(angleCW);
end;

procedure TBGLCustomCanvas.RotateRad(angleCCW: single);
begin
  Matrix := Matrix*AffineMatrixRotationRad(angleCCW);
end;

procedure TBGLCustomCanvas.ResetTransform;
begin
  Matrix := AffineMatrixIdentity;
end;

end.

