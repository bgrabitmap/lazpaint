unit BGRAOpenGL;

{$mode objfpc}{$H+}
{$I bgrabitmap.inc}

interface

uses
  Classes, SysUtils, FPimage, BGRAGraphics,
  BGRAOpenGLType, BGRASpriteGL, BGRACanvasGL, GL, GLU, BGRABitmapTypes,
  BGRAFontGL;

type
  TBGLSprite = TBGLDefaultSprite;
  IBGLTexture = BGRAOpenGLType.IBGLTexture;
  IBGLFont = BGRAOpenGLType.IBGLFont;
  IBGLRenderedFont = BGRAFontGL.IBGLRenderedFont;
  TOpenGLResampleFilter = BGRAOpenGLType.TOpenGLResampleFilter;
  TOpenGLBlendMode = BGRAOpenGLType.TOpenGLBlendMode;
  TBGLPath = BGRACanvasGL.TBGLPath;

  { TBGLContext }

  TBGLContext = object
  private
    function GetHeight: integer;
    function GetWidth: integer;
  public
    Canvas: TBGLCustomCanvas;
    Sprites: TBGLCustomSpriteEngine;
    property Width: integer read GetWidth;
    property Height: integer read GetHeight;
  end;

const
  orfBox = BGRAOpenGLType.orfBox;
  orfLinear = BGRAOpenGLType.orfLinear;
  obmNormal = BGRAOpenGLType.obmNormal;
  obmAdd = BGRAOpenGLType.obmAdd;
  obmMultiply = BGRAOpenGLType.obmMultiply;

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

function BGLFont({%H-}AName: string; {%H-}AEmHeight: integer; {%H-}AStyle: TFontStyles = []): IBGLRenderedFont; overload;
function BGLFont({%H-}AName: string; {%H-}AEmHeight: integer; {%H-}AColor: TBGRAPixel; {%H-}AStyle: TFontStyles = []): IBGLRenderedFont; overload;
function BGLFont({%H-}AName: string; {%H-}AEmHeight: integer; {%H-}AColor: TBGRAPixel; {%H-}AOutlineColor: TBGRAPixel; {%H-}AStyle: TFontStyles = []): IBGLRenderedFont; overload;
function BGLFont({%H-}AName: string; {%H-}AEmHeight: integer; ARenderer: TBGRACustomFontRenderer; ARendererOwned: boolean = true): IBGLRenderedFont; overload;

implementation

uses dynlibs, BGRATransform{$IFDEF BGRABITMAP_USE_LCL}, BGRAText, BGRATextFX{$ENDIF};

type
  TBlendFuncSeparateProc = procedure(sfactorRGB: GLenum; dfactorRGB: GLenum; sfactorAlpha: GLenum; dfactorAlpha: GLenum); stdcall;
  TGetProcAddressFunc = function(proc: PAnsiChar): Pointer; stdcall;

var
  BGLCanvasInstance: TBGLCustomCanvas;
  glBlendFuncSeparate: TBlendFuncSeparateProc;
  glBlendFuncSeparateFetched: boolean;
  {$IFDEF WINDOWS}
  wglGetProcAddress: TGetProcAddressFunc;
  {$ENDIF}

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
  protected
    FMatrix: TAffineMatrix;
    FBlendMode: TOpenGLBlendMode;

    function GetMatrix: TAffineMatrix; override;
    procedure SetMatrix(AValue: TAffineMatrix); override;

    procedure InternalSetColor(const AColor: TBGRAPixel); override;
    procedure InternalStartPutPixel(const pt: TPointF); override;
    procedure InternalStartPolyline(const pt: TPointF); override;
    procedure InternalStartPolygon(const pt: TPointF); override;
    procedure InternalStartTriangleFan(const pt: TPointF); override;
    procedure InternalContinueShape(const pt: TPointF); override;
    procedure InternalEndShape; override;

    procedure InternalStartBlend; override;
    procedure InternalEndBlend; override;

    procedure InternalStartBlendTriangles; override;
    procedure InternalStartBlendQuads; override;
    procedure InternalEndBlendTriangles; override;
    procedure InternalEndBlendQuads; override;

    procedure EnableScissor(AValue: TRect); override;
    procedure DisableScissor; override;

    function GetBlendMode: TOpenGLBlendMode; override;
    procedure SetBlendMode(AValue: TOpenGLBlendMode); override;
  public
    procedure Fill(AColor: TBGRAPixel); override;
  end;

procedure ApplyBlendMode(ABlendMode: TOpenGLBlendMode);
var
  srcBlend : LongWord;
  dstBlend : LongWord;
begin
  case ABlendMode of
    obmAdd:
      begin
        srcBlend := GL_SRC_ALPHA;
        dstBlend := GL_ONE;
      end;
    obmMultiply:
      begin
        srcBlend := GL_ZERO;
        dstBlend := GL_SRC_COLOR;
      end
    else
      begin
        srcBlend := GL_SRC_ALPHA;
        dstBlend := GL_ONE_MINUS_SRC_ALPHA;
      end;
  end;
  {$IFDEF WINDOWS}
  if not glBlendFuncSeparateFetched then
  begin
    glBlendFuncSeparate := TBlendFuncSeparateProc(wglGetProcAddress('glBlendFuncSeparate'));
    glBlendFuncSeparateFetched := true;
  end;
  {$ENDIF}
  if Assigned(glBlendFuncSeparate) then
    glBlendFuncSeparate( srcBlend, dstBlend, GL_ONE, GL_ONE_MINUS_SRC_ALPHA )
  else
    glBlendFunc( srcBlend, dstBlend );
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

function BGLFont(AName: string; AEmHeight: integer; AStyle: TFontStyles = []): IBGLRenderedFont;
begin
  {$IFDEF BGRABITMAP_USE_LCL}
  result := BGLFont(AName, AEmHeight, TLCLFontRenderer.Create);
  result.Style := AStyle;
  {$ELSE}
  result := nil;
  raise exception.Create('LCL renderer not available');
  {$ENDIF}
end;

function BGLFont(AName: string; AEmHeight: integer; AColor: TBGRAPixel;
  AStyle: TFontStyles): IBGLRenderedFont;
begin
  {$IFDEF BGRABITMAP_USE_LCL}
  result := BGLFont(AName, AEmHeight, TLCLFontRenderer.Create);
  result.Color := AColor;
  result.Style := AStyle;
  {$ELSE}
  result := nil;
  raise exception.Create('LCL renderer not available');
  {$ENDIF}
end;

function BGLFont(AName: string; AEmHeight: integer; AColor: TBGRAPixel;
  AOutlineColor: TBGRAPixel; AStyle: TFontStyles = []): IBGLRenderedFont;
{$IFDEF BGRABITMAP_USE_LCL}
var renderer: TBGRATextEffectFontRenderer;
begin
  renderer := TBGRATextEffectFontRenderer.Create;
  renderer.OuterOutlineOnly:= true;
  renderer.OutlineColor := AOutlineColor;
  renderer.OutlineVisible := true;
  result := BGLFont(AName, AEmHeight, renderer, true);
  result.Color := AColor;
  result.Style := AStyle;
end;
{$ELSE}
begin
  result := nil;
  raise exception.Create('LCL renderer not available');
end;
{$ENDIF}

function BGLFont(AName: string; AEmHeight: integer;
  ARenderer: TBGRACustomFontRenderer;
  ARendererOwned: boolean): IBGLRenderedFont;
var f: TBGLRenderedFont;
begin
  f:= TBGLRenderedFont.Create(ARenderer, ARendererOwned);
  f.Name := AName;
  f.EmHeight := AEmHeight;
  result := f;
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
  BGLCanvas.Matrix := AffineMatrixIdentity;
end;

{ TBGLContext }

function TBGLContext.GetHeight: integer;
begin
  if Assigned(Canvas) then
    result := Canvas.Height
  else
    result := 0;
end;

function TBGLContext.GetWidth: integer;
begin
  if Assigned(Canvas) then
    result := Canvas.Width
  else
    result := 0;
end;

{ TBGLCanvas }

function TBGLCanvas.GetMatrix: TAffineMatrix;
begin
  result := FMatrix;
end;

procedure TBGLCanvas.SetMatrix(AValue: TAffineMatrix);
var m: TOpenGLMatrix;
begin
  glMatrixMode(GL_MODELVIEW);
  m := AffineMatrixToOpenGL(AValue);
  glLoadMatrixf(@m);
  FMatrix := AValue;
end;

procedure TBGLCanvas.InternalStartPutPixel(const pt: TPointF);
begin
  glBegin(GL_POINTS);
  glVertex2f(pt.x,pt.y);
end;

procedure TBGLCanvas.InternalStartPolyline(const pt: TPointF);
begin
  glBegin(GL_LINE_STRIP);
  glVertex2f(pt.x,pt.y);
end;

procedure TBGLCanvas.InternalStartPolygon(const pt: TPointF);
begin
  glBegin(GL_LINE_LOOP);
  glVertex2f(pt.x,pt.y);
end;

procedure TBGLCanvas.InternalStartTriangleFan(const pt: TPointF);
begin
  glBegin(GL_TRIANGLE_FAN);
  glVertex2f(pt.x,pt.y);
end;

procedure TBGLCanvas.InternalContinueShape(const pt: TPointF);
begin
  glVertex2f(pt.x,pt.y);
end;

procedure TBGLCanvas.InternalEndShape;
begin
  glEnd();
end;

procedure TBGLCanvas.InternalSetColor(const AColor: TBGRAPixel);
begin
  glColor4ubv(@AColor);
end;

procedure TBGLCanvas.InternalStartBlend;
begin
  glEnable(GL_BLEND);
  ApplyBlendMode(BlendMode);
end;

procedure TBGLCanvas.InternalEndBlend;
begin
  glDisable(GL_BLEND);
end;

procedure TBGLCanvas.InternalStartBlendTriangles;
begin
  InternalStartBlend;
  glBegin(GL_TRIANGLES);
end;

procedure TBGLCanvas.InternalStartBlendQuads;
begin
  InternalStartBlend;
  glBegin(GL_QUADS);
end;

procedure TBGLCanvas.InternalEndBlendTriangles;
begin
  InternalEndShape;
  InternalEndBlend;
end;

procedure TBGLCanvas.InternalEndBlendQuads;
begin
  InternalEndShape;
  InternalEndBlend;
end;

procedure TBGLCanvas.Fill(AColor: TBGRAPixel);
begin
  glClearColor(AColor.Red/255, AColor.green/255, AColor.blue/255, AColor.alpha/255);
  glClear(GL_COLOR_BUFFER_BIT);
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

function TBGLCanvas.GetBlendMode: TOpenGLBlendMode;
begin
  result := FBlendMode;
end;

procedure TBGLCanvas.SetBlendMode(AValue: TOpenGLBlendMode);
begin
  FBlendMode := AValue;
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

    ApplyBlendMode(BlendMode);

    coordFlip := FLIP_TEXCOORD[ Integer(FFlipX) + Integer(FFlipY)*2 ];

    glBegin( GL_QUADS );

    if GradientColors then
      glColor4ubv(@FGradTopLeft)
    else
      glColor4ubv(@AColor);

    glTexCoord2fv( @FramesCoord[FFrame,coordFlip[0]] );
    glVertex2fv( @pt1 );

    if GradientColors then
      glColor4ubv(@FGradTopRight);

    glTexCoord2fv( @FramesCoord[FFrame,coordFlip[1]] );
    glVertex2fv( @pt2 );

    if GradientColors then
      glColor4ubv(@FGradBottomRight);

    glTexCoord2fv( @FramesCoord[FFrame,coordFlip[2]] );
    glVertex2fv( @pt3 );

    if GradientColors then
      glColor4ubv(@FGradBottomLeft);

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
  FBlendMode := obmNormal;
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
  {$IFDEF WINDOWS}
  wglGetProcAddress := TGetProcAddressFunc(GetProcAddress(LibGL, 'wglGetProcAddress'));
  {$ELSE}
  glBlendFuncSeparate := TBlendFuncSeparateProc(GetProcAddress(LibGL, 'glBlendFuncSeparate'));
  glBlendFuncSeparateFetched := true;
  {$ENDIF}

finalization

  BGLCanvasInstance.Free;
  BGLCanvasInstance := nil;
  BGRASpriteGL.BGLSpriteEngine.Free;
  BGRASpriteGL.BGLSpriteEngine := nil;

end.

