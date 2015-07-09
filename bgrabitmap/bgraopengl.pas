unit BGRAOpenGL;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPimage, BGRAGraphics,
  BGRAOpenGLType, BGRASpriteGL, BGRACanvasGL, GL, GLU, BGRABitmapTypes;

type
  TBGLSprite = TBGLDefaultSprite;
  IBGLTexture = BGRAOpenGLType.IBGLTexture;
  TOpenGLResampleFilter = BGRAOpenGLType.TOpenGLResampleFilter;
  TBGLPath = BGRACanvasGL.TBGLPath;

const
  orfBox = BGRAOpenGLType.orfBox;
  orfLinear = BGRAOpenGLType.orfLinear;

type
  { TBGLBitmap }

  TBGLBitmap = class(TBGLCustomBitmap)
  protected
    function GetOpenGLMaxTexSize: integer; override;
  end;

function BGLTexture(ARGBAData: PBGRAPixel; AllocatedWidth,AllocatedHeight, ActualWidth,ActualHeight: integer): IBGLTexture; overload;
function BGLTexture(AFPImage: TFPCustomImage): IBGLTexture; overload;
function BGLTexture(ABitmap: TBitmap): IBGLTexture; overload;
function BGLTexture(AWidth, AHeight: integer; Color: TColor): IBGLTexture; overload;
function BGLTexture(AWidth, AHeight: integer; Color: TBGRAPixel): IBGLTexture; overload;
function BGLTexture(AFilenameUTF8: string): IBGLTexture; overload;
function BGLTexture(AStream: TStream): IBGLTexture; overload;

function BGLSpriteEngine: TBGLCustomSpriteEngine;

function BGLCanvas: TBGLCustomCanvas;

procedure BGLViewPort(AWidth,AHeight: integer); overload;
procedure BGLViewPort(AWidth,AHeight: integer; AColor: TBGRAPixel); overload;

implementation

uses BGRATransform;

var
  BGLCanvasInstance: TBGLCustomCanvas;

const
  GL_COMBINE_ARB                    = $8570;
  GL_COMBINE_RGB_ARB                = $8571;
  GL_SOURCE0_RGB_ARB                = $8580;
  GL_PRIMARY_COLOR_ARB              = $8577;

type
  { TBGLTexture }

  TBGLTexture = class(TBGLCustomTexture)
  protected
    FFlipX,FFlipY: Boolean;

    function GetOpenGLMaxTexSize: integer; override;
    function CreateOpenGLTexture(ARGBAData: PBGRAPixel; AAllocatedWidth, AAllocatedHeight, AActualWidth, AActualHeight: integer): TBGLTextureHandle; override;
    procedure UpdateOpenGLTexture(ATexture: TBGLTextureHandle; ARGBAData: PBGRAPixel; AAllocatedWidth, AAllocatedHeight, AActualWidth,AActualHeight: integer); override;
    procedure SetOpenGLTextureSize(ATexture: TBGLTextureHandle; AAllocatedWidth, AAllocatedHeight, AActualWidth, AActualHeight: integer); override;
    procedure ComputeOpenGLFramesCoord(ATexture: TBGLTextureHandle; FramesX: Integer=1; FramesY: Integer=1); override;
    function GetOpenGLFrameCount(ATexture: TBGLTextureHandle): integer; override;
    function GetEmptyTexture: TBGLTextureHandle; override;
    procedure FreeOpenGLTexture(ATexture: TBGLTextureHandle); override;
    procedure UpdateGLResampleFilter(ATexture: TBGLTextureHandle; AFilter: TOpenGLResampleFilter); override;

    procedure DoDraw(pt1,pt2,pt3,pt4: TPointF; AColor: TBGRAPixel);
    procedure DoStretchDraw(x,y,w,h: single; AColor: TBGRAPixel); override;
    procedure DoStretchDrawAngle(x,y,w,h,angleDeg: single; rotationCenter: TPointF; AColor: TBGRAPixel); override;
    procedure DoDrawAffine(Origin, HAxis, VAxis: TPointF; AColor: TBGRAPixel); override;
    procedure Init(ATexture: TBGLTextureHandle; AWidth,AHeight: integer; AOwned: boolean); override;
    procedure NotifyInvalidFrameSize; override;
    procedure NotifyErrorLoadingFile(AFilenameUTF8: string); override;

    function NewEmpty: TBGLCustomTexture; override;
    function NewFromTexture(ATexture: TBGLTextureHandle; AWidth,AHeight: integer): TBGLCustomTexture; override;
    function Duplicate: TBGLCustomTexture; override;

  public
    procedure ToggleFlipX; override;
    procedure ToggleFlipY; override;
  end;

  POpenGLTexture = ^TOpenGLTexture;
  TOpenGLTexture = record
    ID: Integer;
    AllocatedWidth,AllocatedHeight,ActualWidth,ActualHeight: integer;
    FramesCoord: array of array[0..3] of TPointF;
  end;

  { TBGLCanvas }

  TBGLCanvas = class(TBGLCustomCanvas)
    procedure FillTriangles(const APoints: array of TPointF; AColor: TBGRAPixel); override;
    procedure FillTrianglesLinearColor(const APoints: array of TPointF; const AColors: array of TBGRAPixel); override;
    procedure FillTrianglesFan(const APoints: array of TPointF; ACenterColor, ABorderColor: TBGRAPixel); override;
    procedure FillQuads(const APoints: array of TPointF; AColor: TBGRAPixel); override;
    procedure FillQuadsLinearColor(const APoints: array of TPointF; const AColors: array of TBGRAPixel); override;
    procedure Polylines(const APoints: array of TPointF; AColor: TBGRAPixel; ADrawLastPoints: boolean = true); override;
    procedure Polygons(const APoints: array of TPointF; AColor: TBGRAPixel); override;
    procedure Fill(AColor: TBGRAPixel); override;
    procedure FillRect(r: TRect; AScanner: IBGRAScanner); override;
    procedure EnableScissor(AValue: TRect); override;
    procedure DisableScissor; override;
  end;

function BGLTexture(ARGBAData: PBGRAPixel; AllocatedWidth, AllocatedHeight,
  ActualWidth, ActualHeight: integer): IBGLTexture;
begin
  result := TBGLTexture.Create(ARGBAData,AllocatedWidth, AllocatedHeight,
        ActualWidth, ActualHeight);
end;

function BGLTexture(AFPImage: TFPCustomImage): IBGLTexture;
begin
  result := TBGLTexture.Create(AFPImage);
end;

function BGLTexture(ABitmap: TBitmap): IBGLTexture;
begin
  result := TBGLTexture.Create(ABitmap);
end;

function BGLTexture(AWidth, AHeight: integer; Color: TColor): IBGLTexture;
begin
  result := TBGLTexture.Create(AWidth,AHeight,Color);
end;

function BGLTexture(AWidth, AHeight: integer; Color: TBGRAPixel): IBGLTexture;
begin
  result := TBGLTexture.Create(AWidth,AHeight,Color);
end;

function BGLTexture(AFilenameUTF8: string): IBGLTexture;
begin
  result := TBGLTexture.Create(AFilenameUTF8);
end;

function BGLTexture(AStream: TStream): IBGLTexture;
begin
  result := TBGLTexture.Create(AStream);
end;

function BGLSpriteEngine: TBGLCustomSpriteEngine;
begin
  result := BGRASpriteGL.BGLSpriteEngine;
end;

procedure BGLViewPort(AWidth, AHeight: integer; AColor: TBGRAPixel);
begin
  BGLViewPort(AWidth,AHeight);
  BGLCanvas.Fill(AColor);
end;

function BGLCanvas: TBGLCustomCanvas;
begin
  result := BGLCanvasInstance;
end;

procedure BGLViewPort(AWidth, AHeight: integer);
begin
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho(0, AWidth, AHeight, 0, -1, 1);
  glMatrixMode(GL_MODELVIEW);
  BGLCanvas.Width := AWidth;
  BGLCanvas.Height := AHeight;
end;

{ TBGLCanvas }

procedure TBGLCanvas.FillTriangles(const APoints: array of TPointF;
  AColor: TBGRAPixel);
var
  i: NativeInt;
begin
  if (length(APoints) < 3) or (AColor.alpha = 0) then exit;
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glColor4ubv(@AColor);
  glBegin(GL_TRIANGLES);
  for i := 0 to length(APoints) - (length(APoints) mod 3) - 1 do
    with APoints[i] do
      glVertex2f(x+0.5,y+0.5);
  glEnd();
  glDisable(GL_BLEND);
end;

procedure TBGLCanvas.FillTrianglesLinearColor(const APoints: array of TPointF;
  const AColors: array of TBGRAPixel);
var
  i: NativeInt;
begin
  if length(APoints) < 3 then exit;
  if length(AColors)<>length(APoints) then
    raise exception.Create('Length of APoints and AColors do not match');
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glBegin(GL_TRIANGLES);
  for i := 0 to length(APoints) - (length(APoints) mod 3) - 1 do
  begin
    glColor4ubv(@AColors[i]);
    with APoints[i] do
      glVertex2f(x+0.5,y+0.5);
  end;
  glEnd();
  glDisable(GL_BLEND);
end;

procedure TBGLCanvas.FillTrianglesFan(const APoints: array of TPointF;
  ACenterColor, ABorderColor: TBGRAPixel);
var
  i: NativeInt;
  firstPoint: boolean;
begin
  if (length(APoints) < 3) or ((ACenterColor.alpha = 0) and (ABorderColor.alpha = 0)) then exit;
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  firstPoint := true;
  for i := 0 to high(APoints) do
  begin
    if isEmptyPointF(APoints[i]) then
    begin
      if not firstPoint then
      begin
        glEnd();
        firstPoint := true;
      end;
    end else
    begin
      if firstPoint then
      begin
        glBegin(GL_TRIANGLE_FAN);
        glColor4ubv(@ACenterColor);
        with APoints[i] do
          glVertex2f(x+0.5,y+0.5);
        glColor4ubv(@ABorderColor);
        firstPoint := false;
      end else
        with APoints[i] do
          glVertex2f(x+0.5,y+0.5);
    end;
  end;
  if not firstPoint then glEnd();
  glDisable(GL_BLEND);
end;

procedure TBGLCanvas.FillQuads(const APoints: array of TPointF;
  AColor: TBGRAPixel);
var
  i: NativeInt;
begin
  if (length(APoints) < 4) or (AColor.alpha = 0) then exit;
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glColor4ubv(@AColor);
  glBegin(GL_QUADS);
  for i := 0 to length(APoints) - (length(APoints) and 3) - 1 do
    with APoints[i] do
      glVertex2f(x+0.5,y+0.5);
  glEnd();
  glDisable(GL_BLEND);
end;

procedure TBGLCanvas.FillQuadsLinearColor(const APoints: array of TPointF;
  const AColors: array of TBGRAPixel);
var
  i: NativeInt;
begin
  if length(APoints) < 4 then exit;
  if length(AColors)<>length(APoints) then
    raise exception.Create('Length of APoints and AColors do not match');
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glBegin(GL_QUADS);
  for i := 0 to length(APoints) - (length(APoints) and 3) - 1 do
  begin
    glColor4ubv(@AColors[i]);
    with APoints[i] do
      glVertex2f(x+0.5,y+0.5);
  end;
  glEnd();
  glDisable(GL_BLEND);
end;

procedure TBGLCanvas.Polylines(const APoints: array of TPointF;
  AColor: TBGRAPixel; ADrawLastPoints: boolean);
const
  STATE_START = 0;  //nothing defined
  STATE_SECOND = 1; //prevPoint defined and is the first point
  STATE_AFTER = 2;  //newPoint defined and is the lastest point, prevPoint is the point before that
var
  i: NativeInt;
  state: NativeInt;
  prevPoint,newPoint,v: TPointF;
  len: single;

  procedure Flush;
  begin
    case state of
      STATE_SECOND:
      begin
        glBegin(GL_POINTS);
        glVertex2f(prevPoint.x,prevPoint.y);
        glEnd();
      end;
      STATE_AFTER:
      begin
        v := newPoint-prevPoint;
        len := VectLen(v);
        if len > 0 then
        begin
          v := v*(1/len);
          if ADrawLastPoints then
          begin
            with (newPoint + v*0.5) do
              glVertex2f(x+0.5,y+0.5);
          end else
          begin
            with (newPoint - v*0.5) do
              glVertex2f(x+0.5,y+0.5);
          end;
        end;
        glEnd();
      end;
    end;
    state := STATE_START;
  end;

begin
  if (length(APoints) = 0) or (AColor.alpha = 0) then exit;
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glColor4ubv(@AColor);

  prevPoint := PointF(0,0);
  newPoint := PointF(0,0);
  state := STATE_START;
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
          glBegin(GL_LINE_STRIP);
          newPoint := APoints[i];
          v := newPoint-prevPoint;
          len := VectLen(v);
          if len > 0 then
          begin
            v := v*(1/len);
            with (prevPoint - v*0.5) do
              glVertex2f(x+0.5,y+0.5);
          end;
          state := STATE_AFTER;
        end else
        begin
          with newPoint do
            glVertex2f(x+0.5,y+0.5);
          prevPoint := newPoint;
          newPoint := APoints[i];
        end;
      end;
    end;
  end;
  Flush;
  glDisable(GL_BLEND);
end;

procedure TBGLCanvas.Polygons(const APoints: array of TPointF;
  AColor: TBGRAPixel);
const
  STATE_START = 0;  //nothing defined
  STATE_SECOND = 1; //prevPoint defined and is the first point
  STATE_AFTER = 2;  //newPoint defined and is the lastest point, prevPoint is the point before that
var
  i: NativeInt;
  state: NativeInt;
  prevPoint,newPoint: TPointF;

  procedure Flush;
  begin
    case state of
      STATE_SECOND:
      begin
        glBegin(GL_POINTS);
        glVertex2f(prevPoint.x,prevPoint.y);
        glEnd();
      end;
      STATE_AFTER:
      begin
        with newPoint do
          glVertex2f(x+0.5,y+0.5);
        glEnd();
      end;
    end;
    state := STATE_START;
  end;

begin
  if (length(APoints) = 0) or (AColor.alpha = 0) then exit;
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glColor4ubv(@AColor);

  prevPoint := PointF(0,0);
  newPoint := PointF(0,0);
  state := STATE_START;
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
          glBegin(GL_LINE_LOOP);
          newPoint := APoints[i];
          with prevPoint do
            glVertex2f(x+0.5,y+0.5);
          state := STATE_AFTER;
        end else
        begin
          with newPoint do
            glVertex2f(x+0.5,y+0.5);
          prevPoint := newPoint;
          newPoint := APoints[i];
        end;
      end;
    end;
  end;
  Flush;
  glDisable(GL_BLEND);
end;

procedure TBGLCanvas.Fill(AColor: TBGRAPixel);
begin
  glClearColor(AColor.Red/255, AColor.green/255, AColor.blue/255, AColor.alpha/255);
  glClear(GL_COLOR_BUFFER_BIT);
end;

procedure TBGLCanvas.FillRect(r: TRect; AScanner: IBGRAScanner);
var
  bmp: TBGLBitmap;
  yb,bandHeight,bandY: NativeInt;
  tx: integer;
begin
  SwapRect(r);
  if (r.right = r.left) or (r.bottom = r.top) then exit;
  tx := r.right-r.left;
  bandHeight := 65536 div tx;
  if bandHeight <= 2 then bandHeight := 2;
  bandHeight := GetPowerOfTwo(bandHeight);
  bmp := TBGLBitmap.Create(tx,bandHeight);
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

procedure TBGLCanvas.EnableScissor(AValue: TRect);
begin
  glScissor(AValue.left,Height-AValue.bottom,AValue.right-AValue.left,AValue.Bottom-AValue.Top);
  glEnable(GL_SCISSOR_TEST);
end;

procedure TBGLCanvas.DisableScissor;
begin
  glDisable(GL_SCISSOR_TEST);
end;

{ TBGLTexture }

function TBGLTexture.GetOpenGLMaxTexSize: integer;
begin
  result := 0;
  glGetIntegerv( GL_MAX_TEXTURE_SIZE, @result );
end;

function TBGLTexture.CreateOpenGLTexture(ARGBAData: PBGRAPixel;
  AAllocatedWidth, AAllocatedHeight, AActualWidth, AActualHeight: integer
  ): TBGLTextureHandle;
var p: POpenGLTexture;
begin
  New(p);
  p^.AllocatedWidth := AAllocatedWidth;
  p^.AllocatedHeight := AAllocatedHeight;
  p^.ActualWidth := AActualWidth;
  p^.ActualHeight := AActualHeight;

  glGenTextures( 1, @p^.ID );
  glBindTexture( GL_TEXTURE_2D, p^.ID );
  glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
  glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
  glTexImage2D( GL_TEXTURE_2D, 0, GL_RGBA, AAllocatedWidth, AAllocatedHeight, 0, GL_RGBA, GL_UNSIGNED_BYTE, ARGBAData );

  result := p;
end;

procedure TBGLTexture.UpdateOpenGLTexture(ATexture: TBGLTextureHandle;
  ARGBAData: PBGRAPixel; AAllocatedWidth, AAllocatedHeight, AActualWidth,
  AActualHeight: integer);
begin
  SetOpenGLTextureSize(ATexture, AAllocatedWidth,AAllocatedHeight, AActualWidth,AActualHeight);
  glBindTexture( GL_TEXTURE_2D, TOpenGLTexture(ATexture^).ID );
  glTexImage2D( GL_TEXTURE_2D, 0, GL_RGBA, AAllocatedWidth, AAllocatedHeight, 0, GL_RGBA, GL_UNSIGNED_BYTE, ARGBAData );
end;

procedure TBGLTexture.SetOpenGLTextureSize(ATexture: TBGLTextureHandle;
  AAllocatedWidth, AAllocatedHeight, AActualWidth, AActualHeight: integer);
begin
  with TOpenGLTexture(ATexture^) do
  begin
    ActualWidth := AActualWidth;
    ActualHeight:= AActualHeight;
    AllocatedWidth := AAllocatedWidth;
    AllocatedHeight := AAllocatedHeight;
  end;
end;

procedure TBGLTexture.ComputeOpenGLFramesCoord(ATexture: TBGLTextureHandle;
  FramesX: Integer; FramesY: Integer);
var U,V: Single;
  tX, tY, fU, fV : Single;
  ix,iy,i: Integer;
begin
  with TOpenGLTexture(ATexture^) do
  begin
    if AllocatedWidth = 0 then
      U := 1
    else
      U := ActualWidth/AllocatedWidth;
    if AllocatedHeight = 0 then
      V := 1
    else
      V := ActualHeight/AllocatedHeight;

    if FramesX < 1 then FramesX := 1;
    if FramesY < 1 then FramesY := 1;

    SetLength( FramesCoord, FramesX * FramesY + 1 );
    fU := U / FramesX;
    fV := V / FramesY;

    FramesCoord[ 0, 0 ].X := 0;
    FramesCoord[ 0, 0 ].Y := 0;
    FramesCoord[ 0, 1 ].X := U;
    FramesCoord[ 0, 1 ].Y := 0;
    FramesCoord[ 0, 2 ].X := U;
    FramesCoord[ 0, 2 ].Y := V;
    FramesCoord[ 0, 3 ].X := 0;
    FramesCoord[ 0, 3 ].Y := V;

    ix := 1;
    iy := 1;
    for i := 1 to FramesX * FramesY do
      begin
        tX := ix * fU;
        tY := iy * fV;

        FramesCoord[ i, 0 ].X := tX - fU;
        FramesCoord[ i, 0 ].Y := tY - fV;

        FramesCoord[ i, 1 ].X := tX;
        FramesCoord[ i, 1 ].Y := tY - fV;

        FramesCoord[ i, 2 ].X := tX;
        FramesCoord[ i, 2 ].Y := tY;

        FramesCoord[ i, 3 ].X := tX - fU;
        FramesCoord[ i, 3 ].Y := tY;

        inc(ix);
        if ix > FramesX then
        begin
          ix := 1;
          inc(iy);
        end;
      end;

  end;
end;

function TBGLTexture.GetOpenGLFrameCount(ATexture: TBGLTextureHandle): integer;
begin
  if ATexture = nil then
    result := 0
  else
  begin
    result := Length(TOpenGLTexture(ATexture^).FramesCoord);
    if result > 0 then dec(result); //first frame is whole picture
  end;
end;

function TBGLTexture.GetEmptyTexture: TBGLTextureHandle;
begin
  result := nil;
end;

procedure TBGLTexture.FreeOpenGLTexture(ATexture: TBGLTextureHandle);
begin
  glDeleteTextures( 1, @TOpenGLTexture(ATexture^).ID );
  Dispose(POpenGLTexture(ATexture));
end;

procedure TBGLTexture.UpdateGLResampleFilter(ATexture: TBGLTextureHandle;
  AFilter: TOpenGLResampleFilter);
begin
  glBindTexture( GL_TEXTURE_2D, TOpenGLTexture(ATexture^).ID );
  if AFilter = orfLinear then
  begin
    glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
    glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
  end else
  begin
    glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST );
    glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST );
  end;
end;

procedure TBGLTexture.DoDraw(pt1, pt2, pt3, pt4: TPointF; AColor: TBGRAPixel);
type
  TTexCoordIndex = array[0..3] of integer;
const
  FLIP_TEXCOORD : array[ 0..3 ] of TTexCoordIndex = ( ( 0, 1, 2, 3 ), ( 1, 0, 3, 2 ), ( 3, 2, 1, 0 ), ( 2, 3, 0, 1 ) );
var
  coordFlip: TTexCoordIndex;
begin
  if (FOpenGLTexture = nil) or (FFrame < 0) or (FFrame > FrameCount) then exit;
  with TOpenGLTexture(FOpenGLTexture^) do
  begin
    glEnable( GL_BLEND );
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable( GL_TEXTURE_2D );
    glBindTexture( GL_TEXTURE_2D, ID );

    if FIsMask then
    begin
      glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_ARB );
      glTexEnvi( GL_TEXTURE_ENV, GL_COMBINE_RGB_ARB,  GL_REPLACE );
      glTexEnvi( GL_TEXTURE_ENV, GL_SOURCE0_RGB_ARB,  GL_PRIMARY_COLOR_ARB );
    end else
      glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE );

    coordFlip := FLIP_TEXCOORD[ Integer(FFlipX) + Integer(FFlipY)*2 ];

    glBegin( GL_QUADS );
    glColor4ubv(@AColor);

    glTexCoord2fv( @FramesCoord[FFrame,coordFlip[0]] );
    glVertex2fv( @pt1 );

    glTexCoord2fv( @FramesCoord[FFrame,coordFlip[1]] );
    glVertex2fv( @pt2 );

    glTexCoord2fv( @FramesCoord[FFrame,coordFlip[2]] );
    glVertex2fv( @pt3 );

    glTexCoord2fv( @FramesCoord[FFrame,coordFlip[3]] );
    glVertex2fv( @pt4 );

    glEnd;
    glDisable( GL_TEXTURE_2D );
    glDisable( GL_BLEND );
  end;
end;

procedure TBGLTexture.DoStretchDraw(x, y, w, h: single; AColor: TBGRAPixel);
begin
  DoDraw(PointF(x, y), PointF(x+w, y), PointF(x+w, y+h), PointF(x, y+h), AColor);
end;

procedure TBGLTexture.DoStretchDrawAngle(x, y, w, h, angleDeg: single;
  rotationCenter: TPointF; AColor: TBGRAPixel);
var
  m : TAffineMatrix;
begin
  m := AffineMatrixTranslation(rotationCenter.X,rotationCenter.Y)*
       AffineMatrixRotationDeg(angleDeg)*
       AffineMatrixTranslation(-rotationCenter.X,-rotationCenter.Y);
  DoDraw(m*PointF(x, y), m*PointF(x+w, y), m*PointF(x+w, y+h), m*PointF(x, y+h), AColor);
end;

procedure TBGLTexture.DoDrawAffine(Origin, HAxis, VAxis: TPointF;
  AColor: TBGRAPixel);
begin
  DoDraw(Origin, HAxis, HAxis+(VAxis-Origin), VAxis, AColor);
end;

procedure TBGLTexture.ToggleFlipX;
begin
  FFlipX := not FFlipX;
end;

procedure TBGLTexture.ToggleFlipY;
begin
  FFlipX := not FFlipY;
end;

procedure TBGLTexture.Init(ATexture: TBGLTextureHandle; AWidth,
  AHeight: integer; AOwned: boolean);
begin
  inherited Init(ATexture, AWidth, AHeight, AOwned);
  FFlipX := false;
  FFlipY := false;
end;

procedure TBGLTexture.NotifyInvalidFrameSize;
begin
  raise exception.Create('Invalid frame size');
end;

procedure TBGLTexture.NotifyErrorLoadingFile(AFilenameUTF8: string);
begin
  raise exception.Create('Error loading file "'+AFilenameUTF8+'"');
end;

function TBGLTexture.NewEmpty: TBGLCustomTexture;
begin
  result := TBGLTexture.Create;
end;

function TBGLTexture.NewFromTexture(ATexture: TBGLTextureHandle; AWidth,
  AHeight: integer): TBGLCustomTexture;
begin
  result := TBGLTexture.Create(ATexture,AWidth,AHeight);
end;

function TBGLTexture.Duplicate: TBGLCustomTexture;
begin
  Result:= inherited Duplicate;
  TBGLTexture(result).FFlipX := FFlipX;
  TBGLTexture(result).FFlipY := FFlipY;
end;

{ TBGLBitmap }

function TBGLBitmap.GetOpenGLMaxTexSize: integer;
begin
  result := 0;
  glGetIntegerv( GL_MAX_TEXTURE_SIZE, @result );
end;

initialization

  BGLBitmapFactory := TBGLBitmap;
  BGLTextureFactory := TBGLTexture;
  BGRASpriteGL.BGLSpriteEngine := TBGLDefaultSpriteEngine.Create;
  BGLCanvasInstance := TBGLCanvas.Create;

finalization

  BGLCanvasInstance.Free;
  BGRASpriteGL.BGLSpriteEngine.Free;
  BGRASpriteGL.BGLSpriteEngine := nil;

end.

