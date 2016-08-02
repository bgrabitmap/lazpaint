unit BGRAZenGL;

{ This unit is to be used with BGRABitmapPack4NoGUI_OpenGL

  Add this unit to the Uses clause of your application.
  Note that it is not registered in BGRABitmap package,
  in order to be linked to ZenGL.

  So you must either:
  - supply the full path for this unit like this
  
    Uses BGRAZenGL in '..\bgrabitmap\bgrazengl.pas'; 
	
  - or copy this unit into ZenGL source path 
    
  Other units do not require copying }  

{$mode delphi}

interface

{$DEFINE USE_ZENGL_SPRITE_ENGINE}

uses
  Classes,
  BGRAOpenGLType, BGRASpriteGL, BGRACanvasGL,
  BGRAGraphics, BGRABitmap, BGRABitmapTypes,
  BGRAFontGL,
  zgl_textures, zgl_memory, FPimage, zgl_font,
  zgl_math_2d, zgl_sengine_2d;

type
  TAlignment = Classes.TAlignment;
  TTextLayout = BGRAGraphics.TTextLayout;

  TOpenGLResampleFilter = BGRAOpenGLType.TOpenGLResampleFilter;
  TOpenGLBlendMode = BGRAOpenGLType.TOpenGLBlendMode;

const
  taLeftJustify = Classes.taLeftJustify;
  taCenter = Classes.taCenter;
  taRightJustify = Classes.taRightJustify;

  tlTop = BGRAGraphics.tlTop;
  tlCenter = BGRAGraphics.tlCenter;
  tlBottom = BGRAGraphics.tlBottom;

  orfBox = BGRAOpenGLType.orfBox;
  orfLinear = BGRAOpenGLType.orfLinear;
  obmNormal = BGRAOpenGLType.obmNormal;
  obmAdd = BGRAOpenGLType.obmAdd;
  obmMultiply = BGRAOpenGLType.obmMultiply;

type
  IBGLFont = BGRAOpenGLType.IBGLFont;
  IBGLTexture = BGRAOpenGLType.IBGLTexture;

  { TBGLBitmap }

  TBGLBitmap = class(TBGLCustomBitmap)
  protected
    function GetOpenGLMaxTexSize: integer; override;
    procedure NotifySizeTooBigForOpenGL; override;
  public
    constructor Create(AFilename: string); override; overload; //UTF8 by default
    constructor Create(var AMemory: zglTMemory); overload;
    function NewBitmap(Filename: string): TBGRACustomBitmap; override; overload;  //UTF8 by default
    procedure LoadFromFile(const filename: string); override; overload; //UTF8 by default
    procedure LoadFromFile(const filename: String; Handler: TFPCustomImageReader); override; overload; //UTF8 by default
    procedure SaveToFile(const filename: string); override; overload; //UTF8 by default
    procedure SaveToFile(const filename: string; Handler: TFPCustomImageWriter); override; overload; //UTF8 by default
  end;

  { TBGLSprite }

{$IFDEF USE_ZENGL_SPRITE_ENGINE}
  TBGLSprite = class(TBGLCustomSprite)
  private
    function GetZenSprite: zglPSprite2D;
  protected
    FColor  : TBGRAPixel;
    FHorizontalAlign: TAlignment;
    FVerticalAlign: TTextLayout;
    function GetHorizontalAlign: TAlignment; override;
    function GetVerticalAlign: TTextLayout; override;
    procedure SetHorizontalAlign(AValue: TAlignment); override;
    procedure SetVerticalAlign(AValue: TTextLayout); override;
    function GetAlpha: Integer; override;
    function GetAngle: Single; override;
    function GetColor: TBGRAPixel; override;
    function GetDestroy: Boolean;
    function GetActualFrame: Single; override;
    function GetH: Single; override;
    function GetLayer: Integer; override;
    function GetW: Single; override;
    function GetX: Single; override;
    function GetY: Single; override;
    procedure SetAlpha(AValue: Integer); override;
    procedure SetAngle(AValue: Single); override;
    procedure SetColor(AValue: TBGRAPixel); override;
    procedure SetDestroy(AValue: Boolean);
    procedure SetActualFrame(AValue: Single); override;
    procedure SetH(AValue: Single); override;
    procedure SetLayer(AValue: Integer); override;
    procedure SetW(AValue: Single); override;
    procedure SetX(AValue: Single); override;
    procedure SetY(AValue: Single); override;
    property ZenSprite: zglPSprite2D read GetZenSprite;
    procedure CreateHandle(ATexture: IBGLTexture; ALayer: Integer); override;
  public
    procedure QueryDestroy; override;
  end;
{$ELSE}
  TBGLSprite = TBGLDefaultSprite;
{$ENDIF}

function BGRAToZenGL(AColor: TBGRAPixel): Longword;
function ZenGLToBGRA(AColor: LongWord): TBGRAPixel; overload;
function ZenGLToBGRA(AColor: LongWord; AAlpha: byte): TBGRAPixel; overload;
function ColorToZenGL(AColor: TColor): Longword;

function BGLZenFont(AFilename: UTF8String): IBGLFont; overload;
function BGLZenFont(AZenFont: zglPFont): IBGLFont; overload;
function BGLFont(AName: string; AEmHeight: integer; AStyle: TFontStyles = []): IBGLRenderedFont; overload;
function BGLFont(AName: string; AEmHeight: integer; AColor: TBGRAPixel; AStyle: TFontStyles = []): IBGLRenderedFont; overload;
function BGLFont(AName: string; AEmHeight: integer; AColor: TBGRAPixel; AOutlineColor: TBGRAPixel; AStyle: TFontStyles = []): IBGLRenderedFont; overload;
function BGLFont(AName: string; AEmHeight: integer; ARenderer: TBGRACustomFontRenderer; ARendererOwned: boolean = true): IBGLRenderedFont; overload;

function BGLTexture(ATexture: TBGLTextureHandle; AWidth,AHeight: integer): IBGLTexture; overload;
function BGLTexture(ARGBAData: PDWord; AllocatedWidth,AllocatedHeight, ActualWidth,ActualHeight: integer): IBGLTexture; overload;
function BGLTexture(AFPImage: TFPCustomImage): IBGLTexture; overload;
function BGLTexture(ABitmap: TBitmap): IBGLTexture; overload;
function BGLTexture(AWidth, AHeight: integer; Color: TColor): IBGLTexture; overload;
function BGLTexture(AWidth, AHeight: integer; Color: TBGRAPixel): IBGLTexture; overload;
function BGLTexture(AFilenameUTF8: string): IBGLTexture; overload;
function BGLTexture(AStream: TStream): IBGLTexture; overload;
function BGLTexture(var AMemory: zglTMemory): IBGLTexture; overload;

function BGLSpriteEngine: TBGLCustomSpriteEngine;

function BGLCanvas: TBGLCustomCanvas;

implementation

uses Types, zgl_utils, zgl_opengl_all, zgl_opengl, zgl_render_2d,
  zgl_sprite_2d, zgl_log, zgl_fx, zgl_file, zgl_text,
  zgl_camera_2d,
  BGRATransform, BGRAFreeType,
  BGRAMatrix3D;

const
  GL_LINE_LOOP                      = $0002;
  GL_LINE_STRIP                     = $0003;

var
  BGLCanvasInstance: TBGLCustomCanvas;

type
  PSpriteData = ^TSpriteData;
  TSpriteData = record
    ZenSprite: zglTSprite2D;
    Obj: TBGLCustomSprite;
  end;

  { TZenMemoryStream }

  TZenMemoryStream = class(TStream)
  private
    function GetHandle: zglPMemory;
  protected
    FZenMemory: zglPMemory;
    FZenMemoryOwned: boolean;
    function GetSize: Int64; override;
    procedure SetSize(NewSize: Longint); override; overload;
    function GetPosition: Int64; override;
  public
    constructor Create; overload;
    constructor Create(AHandle: zglPMemory); overload;
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    property Handle: zglPMemory read FZenMemory;
  end;

  { TBGLTexture }

  TBGLTexture = class(TBGLCustomTexture)
  protected
    FZenFlags: LongWord;

    function GetFlags: LongWord; virtual;

    function GetOpenGLMaxTexSize: integer; override;
    function CreateOpenGLTexture(ARGBAData: PDWord; AAllocatedWidth, AAllocatedHeight, AActualWidth, AActualHeight: integer; RGBAOrder: boolean): TBGLTextureHandle; override;
    procedure UpdateOpenGLTexture(ATexture: TBGLTextureHandle; ARGBAData: PDWord; AAllocatedWidth, AAllocatedHeight, AActualWidth,AActualHeight: integer; RGBAOrder: boolean); override;
    procedure SetOpenGLTextureSize(ATexture: TBGLTextureHandle; AAllocatedWidth, AAllocatedHeight, AActualWidth, AActualHeight: integer); override;
    procedure ComputeOpenGLFramesCoord(ATexture: TBGLTextureHandle; FramesX: Integer=1; FramesY: Integer=1); override;
    function GetOpenGLFrameCount(ATexture: TBGLTextureHandle): integer; override;
    function GetEmptyTexture: TBGLTextureHandle; override;
    procedure FreeOpenGLTexture(ATexture: TBGLTextureHandle); override;
    procedure UpdateGLResampleFilter(ATexture: TBGLTextureHandle; AFilter: TOpenGLResampleFilter); override;

    procedure DoDrawAffine(Origin, HAxis, VAxis: TPointF; AColor: TBGRAPixel); override;
    procedure DoStretchDraw(x,y,w,h: single; AColor: TBGRAPixel); override;
    procedure DoStretchDrawAngle(x,y,w,h,angleDeg: single; rotationCenter: TPointF; AColor: TBGRAPixel); override;
    procedure ToggleFlipX; override;
    procedure ToggleFlipY; override;
    procedure Init(ATexture: TBGLTextureHandle; AWidth,AHeight: integer; AOwned: boolean); override;
    procedure NotifyInvalidFrameSize; override;
    procedure NotifyErrorLoadingFile(AFilename: string); override;

    function NewEmpty: TBGLCustomTexture; override;
    function NewFromTexture(ATexture: TBGLTextureHandle; AWidth,AHeight: integer): TBGLCustomTexture; override;
    function Duplicate: TBGLCustomTexture; override;
    procedure FreeMemoryOnDestroy; override;
  public
    constructor Create(var AMemory: zglTMemory); overload;
  end;

  { TBGRAZenFont }

  TBGRAZenFont = class(TBGLCustomFont)
  protected
    FZenFont: zglPFont;
    FZenFontOwned: boolean;
    FFlags: LongWord;
    FGradientColors: array[1..4] of TBGRAPixel;
    function LoadFromFile(AFilename: UTF8String): boolean; override;
    function GetPage(AIndex: integer): zglPTexture;
    procedure SetPage(AIndex: integer; AValue: zglPTexture);
    function GetFlags: LongWord;

    procedure Init; override;
    procedure FreeMemoryOnDestroy; override;
    procedure DoTextOut(X, Y: Single; const Text : UTF8String; AColor: TBGRAPixel); override;
    procedure DoTextRect(X, Y, Width, Height: Single; const Text : UTF8String; AColor: TBGRAPixel); override;

    function GetClipped: boolean; override;
    function GetUseGradientColors: boolean; override;
    function GetJustify: boolean; override;
    procedure SetClipped(AValue: boolean); override;
    procedure SetUseGradientColors(AValue: boolean); override;
    procedure SetJustify(AValue: boolean); override;
  public
    constructor Create(AHandle: zglPFont); overload;
    procedure FreeMemory; override;
    procedure SetGradientColors(ATopLeft, ATopRight, ABottomRight, ABottomLeft: TBGRAPixel); override;
    function TextWidth(const Text: UTF8String): single; override;
    function TextHeight(const Text: UTF8String): single; override; overload;
    function TextHeight(const Text: UTF8String; AWidth: single): single; override; overload;
  end;

  { TBGLZenSpriteEngine }

  TBGLZenSpriteEngine = class(TBGLCustomSpriteEngine)
  protected
    FSprites: array of TBGLCustomSprite;
    function GetCount: integer; override;
    function GetSprite(AIndex: integer): TBGLCustomSprite; override;
  public
    procedure Add(ASprite: TBGLCustomSprite); override;
    procedure Remove(ASprite: TBGLCustomSprite); override;
    procedure OnDraw; override;
    procedure OnTimer; override;
    procedure Clear; override;
    procedure Delete(AIndex: integer); override;
  end;

  { TBGLZenCanvas }

  TBGLZenCanvas = class(TBGLCustomCanvas)
  protected
    FMatrix: TAffineMatrix;
    FOldBlendMode : TOpenGLBlendMode;
    FFaceCulling: TFaceCulling;
    function GetMatrix: TAffineMatrix; override;
    procedure SetMatrix(const AValue: TAffineMatrix); override;

    procedure InternalSetColor(const AColor: TBGRAPixel); override;
    procedure InternalSetColorF(const AColor: TColorF); override;
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

    function GetFaceCulling: TFaceCulling; override;
    procedure SetFaceCulling(AValue: TFaceCulling); override;
  public
    procedure Fill(AColor: TBGRAPixel); override;

    procedure Translate(x,y: single); override;
    procedure Scale(sx,sy: single); override;
    procedure RotateDeg(angleCW: single); override;
    procedure RotateRad(angleCCW: single); override;
  end;

function GetZenBlendMode: TOpenGLBlendMode;
begin
  case b2dCurBlendMode and 255 of
    FX_BLEND_ADD: result := obmAdd;
    FX_BLEND_MULT, FX_BLEND_MASK: result := obmMultiply;
    else result := obmNormal;
  end;
end;

function ToZenBlendMode(ABlendMode: TOpenGLBlendMode): Byte;
begin
  case ABlendMode of
    obmAdd: result := FX_BLEND_ADD;
    obmMultiply: result := FX_BLEND_MULT;
  else
    result := FX_BLEND_NORMAL;
  end;
end;

procedure SetZenBlendMode(ABlendMode: TOpenGLBlendMode);
var ZenMode : Byte;
begin
  fx_SetBlendMode(ToZenBlendMode(ABlendMode));
end;

function BGRAToZenGL(AColor: TBGRAPixel): Longword;
begin
  result := (AColor.red shl 16) + (AColor.green shl 8) + AColor.blue;
end;

function ZenGLToBGRA(AColor: LongWord): TBGRAPixel;
begin
  result := ZenGLToBGRA(AColor,255);
end;

function ZenGLToBGRA(AColor: LongWord; AAlpha: byte): TBGRAPixel;
begin
  result.red := AColor shr 16 and $ff;
  result.green := AColor shr 8 and $ff;
  result.blue := AColor and $ff;
  result.alpha := AAlpha;
end;

function ColorToZenGL(AColor: TColor): Longword;
var r,g,b: byte;
begin
  RedGreenBlue(AColor,r,g,b);
  result := (r shl 16) + (g shl 8) + b;
end;

function BGLZenFont(AFilename: UTF8String): IBGLFont;
begin
  result := TBGRAZenFont.Create(AFilename);
end;

function BGLZenFont(AZenFont: zglPFont): IBGLFont;
begin
  result := TBGRAZenFont.Create(AZenFont);
end;

function BGLFont(AName: string; AEmHeight: integer; AStyle: TFontStyles = []): IBGLRenderedFont;
begin
  result := BGLFont(AName, AEmHeight, TBGRAFreeTypeFontRenderer.Create);
  result.Style := AStyle;
end;

function BGLFont(AName: string; AEmHeight: integer; AColor: TBGRAPixel;
  AStyle: TFontStyles): IBGLRenderedFont;
begin
  result := BGLFont(AName, AEmHeight, TBGRAFreeTypeFontRenderer.Create);
  result.Color := AColor;
  result.Style := AStyle;
end;

function BGLFont(AName: string; AEmHeight: integer; AColor: TBGRAPixel;
  AOutlineColor: TBGRAPixel; AStyle: TFontStyles): IBGLRenderedFont; overload;
var
  renderer: TBGRAFreeTypeFontRenderer;
begin
  renderer := TBGRAFreeTypeFontRenderer.Create;
  renderer.OuterOutlineOnly:= true;
  renderer.OutlineColor := AOutlineColor;
  renderer.OutlineVisible := true;
  result := BGLFont(AName, AEmHeight, renderer);
  result.Color := AColor;
  result.Style := AStyle;
end;

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

function BGLTexture(ATexture: TBGLTextureHandle; AWidth, AHeight: integer
  ): IBGLTexture;
begin
  result := TBGLTexture.Create(ATexture, AWidth,AHeight);
end;

function BGLTexture(ARGBAData: PDWord; AllocatedWidth, AllocatedHeight,
  ActualWidth, ActualHeight: integer): IBGLTexture;
begin
  result := TBGLTexture.Create(ARGBAData, AllocatedWidth,AllocatedHeight,ActualWidth,ActualHeight);
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

function BGLTexture(var AMemory: zglTMemory): IBGLTexture;
begin
  result := TBGLTexture.Create(AMemory);
end;

function BGLSpriteEngine: TBGLCustomSpriteEngine;
begin
  result := BGRASpriteGL.BGLSpriteEngine;
end;

function BGLCanvas: TBGLCustomCanvas;
begin
  result := BGLCanvasInstance;
end;

{ TBGLZenCanvas }

function TBGLZenCanvas.GetMatrix: TAffineMatrix;
begin
  result := FMatrix;
end;

procedure TBGLZenCanvas.SetMatrix(const AValue: TAffineMatrix);
var m: TMatrix4D;
begin
  if Assigned(cam2d.Global) then
    cam2d_Set(nil);

  batch2d_Flush;

  glMatrixMode(GL_MODELVIEW);
  m := AffineMatrixToMatrix4D(AValue);
  glLoadMatrixf(@m);
  FMatrix := AValue;
end;

procedure TBGLZenCanvas.InternalSetColor(const AColor: TBGRAPixel);
begin
  if TBGRAPixel_RGBAOrder then
    glColor4ubv(@AColor)
  else
    glColor4ub(AColor.red,AColor.green,AColor.blue,AColor.alpha);
end;

procedure TBGLZenCanvas.InternalSetColorF(const AColor: TColorF);
begin
  glColor4f(AColor[1],AColor[2],AColor[3],AColor[4]);
end;

procedure TBGLZenCanvas.InternalStartPutPixel(const pt: TPointF);
begin
  glBegin(GL_POINTS);
  glVertex2f(pt.x,pt.y);
end;

procedure TBGLZenCanvas.InternalStartPolyline(const pt: TPointF);
begin
  glBegin(GL_LINE_STRIP);
  glVertex2f(pt.x,pt.y);
end;

procedure TBGLZenCanvas.InternalStartPolygon(const pt: TPointF);
begin
  glBegin(GL_LINE_LOOP);
  glVertex2f(pt.x,pt.y);
end;

procedure TBGLZenCanvas.InternalStartTriangleFan(const pt: TPointF);
begin
  glBegin(GL_TRIANGLE_FAN);
  glVertex2f(pt.x,pt.y);
end;

procedure TBGLZenCanvas.InternalContinueShape(const pt: TPointF);
begin
  glVertex2f(pt.x,pt.y);
end;

procedure TBGLZenCanvas.InternalEndShape;
begin
  glEnd();
end;

procedure TBGLZenCanvas.InternalStartBlend;
begin
  batch2d_Flush;
  glEnable( GL_BLEND );
end;

procedure TBGLZenCanvas.InternalEndBlend;
begin
  glDisable(GL_BLEND);
end;

procedure TBGLZenCanvas.InternalStartBlendTriangles;
begin
  if not b2dStarted or batch2d_Check( GL_TRIANGLES, FX_BLEND, nil ) then
  begin
    glEnable( GL_BLEND );
    glBegin( GL_TRIANGLES );
  end;
end;

procedure TBGLZenCanvas.InternalStartBlendQuads;
begin
  if not b2dStarted or batch2d_Check( GL_QUADS, FX_BLEND, nil ) then
  begin
    glEnable( GL_BLEND );
    glBegin( GL_QUADS );
  end;
end;

procedure TBGLZenCanvas.InternalEndBlendTriangles;
begin
  if not b2dStarted Then
  begin
    glEnd();
    glDisable( GL_BLEND );
  end;
end;

procedure TBGLZenCanvas.InternalEndBlendQuads;
begin
  if not b2dStarted Then
  begin
    glEnd();
    glDisable( GL_BLEND );
  end;
end;

procedure TBGLZenCanvas.Fill(AColor: TBGRAPixel);
begin
  glClearColor(AColor.Red/255, AColor.green/255, AColor.blue/255, AColor.alpha/255);
  glClear(GL_COLOR_BUFFER_BIT);
end;

procedure TBGLZenCanvas.Translate(x, y: single);
begin
  batch2d_Flush;
  FMatrix := FMatrix*AffineMatrixTranslation(x,y);
  glTranslatef(x,y,0);
end;

procedure TBGLZenCanvas.Scale(sx, sy: single);
begin
  batch2d_Flush;
  FMatrix := FMatrix*AffineMatrixScale(sx,sy);
  glScalef(sx,sy,1);
end;

procedure TBGLZenCanvas.RotateDeg(angleCW: single);
begin
  batch2d_Flush;
  FMatrix := FMatrix*AffineMatrixRotationDeg(angleCW);
  glRotatef(angleCW,0,0,1);
end;

procedure TBGLZenCanvas.RotateRad(angleCCW: single);
begin
  batch2d_Flush;
  FMatrix := FMatrix*AffineMatrixRotationRad(angleCCW);
  glRotatef(-angleCCW*180/Pi,0,0,1);
end;

procedure TBGLZenCanvas.EnableScissor(AValue: TRect);
begin
  glScissor(AValue.left,Height-AValue.bottom,AValue.right-AValue.left,AValue.Bottom-AValue.Top);
  glEnable(GL_SCISSOR_TEST);
end;

procedure TBGLZenCanvas.DisableScissor;
begin
  glDisable(GL_SCISSOR_TEST);
end;

function TBGLZenCanvas.GetBlendMode: TOpenGLBlendMode;
begin
  Result:= GetZenBlendMode;
end;

procedure TBGLZenCanvas.SetBlendMode(AValue: TOpenGLBlendMode);
begin
  SetZenBlendMode(AValue);
end;

function TBGLZenCanvas.GetFaceCulling: TFaceCulling;
begin
  result := FFaceCulling;
end;

procedure TBGLZenCanvas.SetFaceCulling(AValue: TFaceCulling);
const GL_CULL_FACE = $0B44;
begin
  if AValue = FFaceCulling then exit;
  if FFaceCulling = fcNone then
    glEnable(GL_CULL_FACE);
  case AValue of
    fcNone: glDisable(GL_CULL_FACE);
    fcKeepCW: {not available};
    fcKeepCCW: ;
  end;
  FFaceCulling:= AValue;
end;

{ TBGLZenSpriteEngine }

function TBGLZenSpriteEngine.GetCount: integer;
begin
  result := sengine2d_Get.Count;
end;

function TBGLZenSpriteEngine.GetSprite(AIndex: integer): TBGLCustomSprite;
begin
  if (AIndex < 0) or (AIndex >= Count) then
    result := nil
  else
    result := PSpriteData(sengine2d_Get.List[AIndex])^.Obj;
end;

procedure TBGLZenSpriteEngine.Add(ASprite: TBGLCustomSprite);
begin
  //nothing, already added when creating handle
end;

procedure TBGLZenSpriteEngine.Remove(ASprite: TBGLCustomSprite);
begin
  //nothing, can only be freed using QueryDestroy
end;

procedure TBGLZenSpriteEngine.OnDraw;
var oldBlendMode: TOpenGLBlendMode;
begin
  oldBlendMode:= GetZenBlendMode;
  sengine2d_Draw();
  SetZenBlendMode(oldBlendMode);
end;

procedure TBGLZenSpriteEngine.OnTimer;
begin
  sengine2d_Proc();
end;

procedure TBGLZenSpriteEngine.Clear;
begin
  sengine2d_ClearAll();
end;

procedure TBGLZenSpriteEngine.Delete(AIndex: integer);
begin
  sengine2d_DelSprite( AIndex );
end;

{ TBGLSprite }

{$IFDEF USE_ZENGL_SPRITE_ENGINE}
function TBGLSprite.GetZenSprite: zglPSprite2D;
begin
  result := zglPSprite2D(FHandle);
end;

function TBGLSprite.GetHorizontalAlign: TAlignment;
begin
  result := FHorizontalAlign;
end;

function TBGLSprite.GetVerticalAlign: TTextLayout;
begin
  result := FVerticalAlign;
end;

procedure TBGLSprite.SetHorizontalAlign(AValue: TAlignment);
begin
  FHorizontalAlign := AValue;
end;

procedure TBGLSprite.SetVerticalAlign(AValue: TTextLayout);
begin
  FVerticalAlign := AValue;
end;

function TBGLSprite.GetAlpha: Integer;
begin
  result := ZenSprite.Alpha;
end;

function TBGLSprite.GetAngle: Single;
begin
  result := ZenSprite.Angle;
end;

function TBGLSprite.GetColor: TBGRAPixel;
begin
  result := FColor;
  result.alpha:= ZenSprite.Alpha;
end;

function TBGLSprite.GetDestroy: Boolean;
begin
  result := ZenSprite.Destroy;
end;

function TBGLSprite.GetActualFrame: Single;
begin
  result := ZenSprite.Frame;
end;

function TBGLSprite.GetH: Single;
begin
  result := ZenSprite.H;
end;

function TBGLSprite.GetLayer: Integer;
begin
  result := ZenSprite.Layer;
end;

function TBGLSprite.GetW: Single;
begin
  result := ZenSprite.W;
end;

function TBGLSprite.GetX: Single;
begin
  result := ZenSprite.X;
end;

function TBGLSprite.GetY: Single;
begin
  result := ZenSprite.Y;
end;

procedure TBGLSprite.SetAlpha(AValue: Integer);
begin
  ZenSprite.Alpha := AValue;
end;

procedure TBGLSprite.SetAngle(AValue: Single);
begin
  ZenSprite.Angle := AValue;
end;

procedure TBGLSprite.SetColor(AValue: TBGRAPixel);
begin
  FColor := AValue;
  ZenSprite.Alpha := AValue.alpha;
end;

procedure TBGLSprite.SetDestroy(AValue: Boolean);
begin
  ZenSprite.Destroy := AValue;
end;

procedure TBGLSprite.SetActualFrame(AValue: Single);
begin
  ZenSprite.Frame := AValue;
end;

procedure TBGLSprite.SetH(AValue: Single);
begin
  ZenSprite.H := AValue;
end;

procedure TBGLSprite.SetLayer(AValue: Integer);
begin
  ZenSprite.Layer := AValue;
end;

procedure TBGLSprite.SetW(AValue: Single);
begin
  ZenSprite.W := AValue;
end;

procedure TBGLSprite.SetX(AValue: Single);
begin
  ZenSprite.X := AValue;
end;

procedure TBGLSprite.SetY(AValue: Single);
begin
  ZenSprite.Y := AValue;
end;

procedure SpriteDrawHandler( var Sprite : TSpriteData);
begin
  if Assigned(Sprite.Obj) then
    Sprite.Obj.OnDraw;
end;

procedure SpriteProcHandler( var Sprite : TSpriteData);
begin
  if Assigned(Sprite.Obj) then
    Sprite.Obj.OnTimer;
end;

procedure SpriteFreeHandler( var Sprite : TSpriteData);
begin
  if Assigned(Sprite.Obj) then
  begin
    Sprite.Obj.Free;
    Sprite.Obj := nil;
  end;
end;

procedure TBGLSprite.CreateHandle(ATexture: IBGLTexture; ALayer: Integer);
begin
  FHandle := sengine2d_AddCustom(ATexture.Handle, SizeOf(TSpriteData), ALayer, nil, @SpriteDrawHandler, @SpriteProcHandler, @SpriteFreeHandler);
  TSpriteData(FHandle^).Obj := self;
end;

procedure TBGLSprite.QueryDestroy;
begin
  SetDestroy(True);
end;
{$ENDIF}

{ TBGRAZenFont }

function TBGRAZenFont.GetClipped: boolean;
begin
  result := (FFlags and TEXT_CLIP_RECT)<>0;
end;

function TBGRAZenFont.GetJustify: boolean;
begin
  result := (FFlags and TEXT_HALIGN_JUSTIFY)<>0;
end;

function TBGRAZenFont.GetPage(AIndex: integer): zglPTexture;
begin
  if FZenFont = nil then
    result := nil
  else
    result := FZenFont.Pages[AIndex];
end;

function TBGRAZenFont.GetUseGradientColors: boolean;
begin
  result := (FFlags and TEXT_FX_VCA)<>0;
end;

procedure TBGRAZenFont.SetClipped(AValue: boolean);
begin
  if AValue then
    FFlags:= FFlags or TEXT_CLIP_RECT
  else
    FFlags:= FFlags and not TEXT_CLIP_RECT;
end;

procedure TBGRAZenFont.SetJustify(AValue: boolean);
begin
  if AValue then
    FFlags:= FFlags or TEXT_HALIGN_JUSTIFY
  else
    FFlags:= FFlags and not TEXT_HALIGN_JUSTIFY;
end;

procedure TBGRAZenFont.SetPage(AIndex: integer; AValue: zglPTexture);
begin
  if FZenFont <> nil then
  begin
    if (AIndex >= 0) and (AIndex < length(FZenFont.Pages)) then
      FZenFont.Pages[AIndex] := AValue;
  end;
end;

procedure TBGRAZenFont.SetUseGradientColors(AValue: boolean);
begin
  if AValue then
    FFlags:= FFlags or TEXT_FX_VCA
  else
    FFlags:= FFlags and not TEXT_FX_VCA;
end;

procedure TBGRAZenFont.Init;
begin
  inherited Init;
  FZenFont := nil;
  FZenFontOwned := false;
  FFlags:= 0;
  FGradientColors[1] := BGRAWhite;
  FGradientColors[2] := BGRAWhite;
  FGradientColors[3] := BGRAWhite;
  FGradientColors[4] := BGRAWhite;
end;

procedure TBGRAZenFont.FreeMemoryOnDestroy;
begin
  //do not free memory because it is taken care of by ZenGL
end;

function TBGRAZenFont.LoadFromFile(AFilename: UTF8String): boolean;
var
  fntMem : zglTMemory;
  i, j   : Integer;
  dir    : UTF8String;
  name   : UTF8String;
  tmp    : UTF8String;
  imgFormat: TBGRAImageFormat;
  tempBmp: TBGLBitmap;
  newFont: zglPFont;
begin
  Result := false;
  newFont := nil;

  if not file_Exists( AFilename ) Then
    begin
      log_Add( 'Cannot read "' + AFilename + '"' );
      exit;
    end;

  mem_LoadFromFile( fntMem, AFilename );
  font_Load( newFont, fntMem );
  mem_Free( fntMem );

  if not Assigned( newFont ) Then
    begin
      log_Add( 'Unable to load font: "' + AFilename + '"' );
      exit;
    end;

  FZenFont := newFont;
  FZenFontOwned := true;
  dir  := file_GetDirectory( AFilename );
  name := file_GetName( AFilename );
  for i := 0 to newFont.Count.Pages - 1 do
    for imgFormat:= low(TBGRAImageFormat) to high(TBGRAImageFormat) do
      if Assigned(DefaultBGRAImageReader[imgFormat]) then
      begin
        tmp := dir + name + '-page' + u_IntToStr( i ) + '.' + SuggestImageExtension(imgFormat);
        if file_Exists( tmp ) Then
          begin
            tempBmp := TBGLBitmap.Create(tmp, True);
            tempBmp.VerticalFlip;
            newFont.Pages[ i ] := tempBmp.MakeTextureAndFree.Handle;
            break;
          end;
      end;
end;

procedure TBGRAZenFont.DoTextOut(X, Y: Single;
  const Text: UTF8String; AColor: TBGRAPixel);
begin
  text_DrawEx(FZenFont, X,Y,FScale,FStepX, Text, AColor.alpha, BGRAToZenGL(AColor), GetFlags);
end;

procedure TBGRAZenFont.DoTextRect(X, Y, Width, Height: Single; const Text : UTF8String; AColor: TBGRAPixel);
var r: zglTRect;
begin
  r.X := X;
  r.Y := Y;
  r.W := Width;
  r.H := Height;
  text_DrawInRectEx(FZenFont, r, FScale,FStepX, Text, AColor.alpha, BGRAToZenGL(AColor), GetFlags);
end;

function TBGRAZenFont.GetFlags: LongWord;
begin
  result := FFlags;
  if GetJustify then
    result := result OR TEXT_HALIGN_JUSTIFY
  else
    case GetHorizontalAlign of
      taCenter: result:= result or TEXT_HALIGN_CENTER;
      taRightJustify: result:= result or TEXT_HALIGN_RIGHT;
      else
        result:= result or TEXT_HALIGN_LEFT;
    end;
  case GetVerticalAlign of
    tlCenter: result:= result or TEXT_VALIGN_CENTER;
    tlBottom: result:= result or TEXT_VALIGN_BOTTOM;
    else
      result:= result or TEXT_VALIGN_TOP;
  end;
  if GradientColors then
    fx2d_SetVCA( BGRAToZenGL(FGradientColors[1]), BGRAToZenGL(FGradientColors[2]), BGRAToZenGL(FGradientColors[3]), BGRAToZenGL(FGradientColors[4]),
       FGradientColors[1].alpha, FGradientColors[2].alpha, FGradientColors[3].alpha, FGradientColors[4].alpha );
end;

constructor TBGRAZenFont.Create(AHandle: zglPFont);
begin
  Init;
  FZenFont := AHandle;
  FZenFontOwned := false;
end;

function TBGRAZenFont.TextWidth(const Text: UTF8String): single;
begin
  result := text_GetWidth(FZenFont, Text, FStepX)*FScale;
end;

function TBGRAZenFont.TextHeight(const Text: UTF8String): single;
begin
  result := FZenFont.MaxHeight*FScale;
end;

function TBGRAZenFont.TextHeight(const Text: UTF8String; AWidth: single
  ): single;
begin
  result := text_GetHeight(FZenFont, AWidth, Text, FScale, FStepX);
end;

procedure TBGRAZenFont.FreeMemory;
begin
  if FZenFontOwned then
  begin
    font_Del(FZenFont);
    FZenFont := nil;
  end;
end;

procedure TBGRAZenFont.SetGradientColors(ATopLeft, ATopRight, ABottomRight, ABottomLeft: TBGRAPixel);
begin
  FGradientColors[1] := ATopLeft;
  FGradientColors[2] := ATopRight;
  FGradientColors[3] := ABottomRight;
  FGradientColors[4] := ABottomLeft;
  GradientColors := true;
end;

{ TZenMemoryStream }

function TZenMemoryStream.GetHandle: zglPMemory;
begin
  result := @FZenMemory;
end;

function TZenMemoryStream.GetSize: Int64;
begin
  Result:= FZenMemory^.Size;
end;

procedure TZenMemoryStream.SetSize(NewSize: Longint);
begin
  if NewSize < 0 then NewSize := 0;
  mem_SetSize(FZenMemory^, NewSize);
end;

function TZenMemoryStream.GetPosition: Int64;
begin
  Result:= FZenMemory.Position;
end;

constructor TZenMemoryStream.Create;
begin
  New(FZenMemory);
  FillChar(FZenMemory, SizeOf(zglTMemory), 0);
  FZenMemoryOwned := true;
end;

constructor TZenMemoryStream.Create(AHandle: zglPMemory);
begin
  FZenMemory := AHandle;
  FZenMemoryOwned := false;
end;

destructor TZenMemoryStream.Destroy;
begin
  if FZenMemoryOwned then
  begin
    Dispose(FZenMemory);
    FZenMemory := nil;
  end;
  inherited Destroy;
end;

function TZenMemoryStream.Read(var Buffer; Count: Longint): Longint;
begin
  if Count < 0 then Count := 0;
  result := mem_Read(FZenMemory^, Buffer, Count);
end;

function TZenMemoryStream.Write(const Buffer; Count: Longint): Longint;
begin
  if Count < 0 then Count := 0;
  result := mem_Write(FZenMemory^, Buffer, Count);
end;

function TZenMemoryStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  result := mem_Seek(FZenMemory^, Offset, Origin+1);
end;

{ TBGLTexture }

procedure TBGLTexture.ComputeOpenGLFramesCoord( ATexture : TBGLTextureHandle; FramesX : Integer = 1; FramesY : Integer = 1 );
  var
    i,ix,iy : Integer;
    tX, tY, fU, fV : Single;
begin
  with (zglPTexture(ATexture)^) do
  begin
    if FramesX <= 0 Then FramesX := 1;
    if FramesY <= 0 Then FramesY := 1;

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
    result := length(zglPTexture(ATexture)^.FramesCoord);
    if result > 0 then dec(result); //frame 0 is the whole picture
  end;
end;

function TBGLTexture.GetEmptyTexture: TBGLTextureHandle;
begin
  result := managerZeroTexture;
end;

procedure TBGLTexture.SetOpenGLTextureSize(ATexture: TBGLTextureHandle; AAllocatedWidth,AAllocatedHeight, AActualWidth,AActualHeight: integer);
begin
  with (zglPTexture(ATexture)^) do
  begin
    Width := AActualWidth;
    Height:= AActualHeight;
    U := AActualWidth/AAllocatedWidth;
    V := AActualHeight/AAllocatedHeight;
  end;
end;

procedure TBGLTexture.UpdateOpenGLTexture(ATexture: TBGLTextureHandle;
  ARGBAData: PDWord; AAllocatedWidth, AAllocatedHeight, AActualWidth,
  AActualHeight: integer; RGBAOrder: boolean);
begin
  batch2d_Flush;
  SetOpenGLTextureSize(ATexture, AAllocatedWidth,AAllocatedHeight, AActualWidth,AActualHeight);
  with (zglPTexture(ATexture)^) do
  begin
    dec(oglVRAMUsed, Round(Width/U) * Round(Height/V)*4);
    glBindTexture( GL_TEXTURE_2D, ID );
    glTexImage2D( GL_TEXTURE_2D, 0, GL_RGBA, AAllocatedWidth, AAllocatedHeight, 0, GL_RGBA, GL_UNSIGNED_BYTE, ARGBAData );
    inc(oglVRAMUsed, AAllocatedWidth*AAllocatedHeight*4);
  end;
end;

function TBGLTexture.GetFlags: LongWord;
begin
  result := FZenFlags;
  if GradientColors then
  begin
    result := result or FX2D_VCA;
    fx2d_SetVCA( BGRAToZenGL(FGradTopLeft), BGRAToZenGL(FGradTopRight), BGRAToZenGL(FGradBottomRight), BGRAToZenGL(FGradBottomLeft),
       FGradTopLeft.alpha, FGradTopRight.alpha, FGradBottomRight.alpha, FGradBottomLeft.alpha);
  end;
end;

function TBGLTexture.GetOpenGLMaxTexSize: integer;
begin
  result := oglMaxTexSize;
end;

function TBGLTexture.CreateOpenGLTexture(ARGBAData: PDWord; AAllocatedWidth,
  AAllocatedHeight, AActualWidth, AActualHeight: integer;
  RGBAOrder: boolean): TBGLTextureHandle;
var tex: zglPTexture;
begin
  tex := tex_Add;
  tex.Format := TEX_FORMAT_RGBA;
  tex.Flags := TEX_FILTER_LINEAR;
  tex.Width := AAllocatedWidth;
  tex.Height := AAllocatedHeight;
  tex.U := 1;
  tex.V := 1;
  batch2d_Flush;
  tex_CreateGL(tex^, Pointer(ARGBAData));
  SetOpenGLTextureSize(tex, AAllocatedWidth,AAllocatedHeight, AActualWidth,AActualHeight);
  result := tex;
end;

procedure TBGLTexture.DoStretchDraw(x, y, w, h: single; AColor: TBGRAPixel);
begin
  if AColor.Alpha = 0 then exit;
  SetZenBlendMode(BlendMode);
  if (BGRAToColor(AColor)=clWhite) and not FIsMask then
  begin
    if FFrame > 0 then
     asprite2d_Draw( FOpenGLTexture, x, y, w, h, 0, FFrame, AColor.Alpha, GetFlags)
    else
     ssprite2d_Draw( FOpenGLTexture, x, y, w, h, 0, AColor.Alpha, GetFlags);
  end else
  begin
    fx2d_SetColor(BGRAToZenGL(AColor));
    if FIsMask then fx_SetColorMode( FX_COLOR_SET );
    if FFrame > 0 then
      asprite2d_Draw( FOpenGLTexture, x, y, w, h, 0, FFrame, AColor.Alpha, GetFlags OR FX_COLOR)
    else
      ssprite2d_Draw( FOpenGLTexture, x, y, w, h, 0, AColor.Alpha, GetFlags OR FX_COLOR);
    if FIsMask then fx_SetColorMode( FX_COLOR_MIX );
  end;
end;

procedure TBGLTexture.DoStretchDrawAngle(x, y, w,h, angleDeg: single;
  rotationCenter: TPointF; AColor: TBGRAPixel);
begin
  if AColor.Alpha = 0 then exit;
  SetZenBlendMode(BlendMode);
  fx2d_SetRotatingPivot(rotationCenter.X-x,rotationCenter.Y-y);
  if (BGRAToColor(AColor)=clWhite) and not FIsMask then
  begin
    if FFrame > 0 then
      asprite2d_Draw( FOpenGLTexture, x,y, w, h, angleDeg, FFrame, AColor.Alpha , GetFlags OR FX2D_RPIVOT)
    else
      ssprite2d_Draw( FOpenGLTexture, x,y, w, h, angleDeg, AColor.Alpha , GetFlags OR FX2D_RPIVOT)
  end
  else
  begin
    fx2d_SetColor(BGRAToZenGL(AColor));
    if FIsMask then fx_SetColorMode( FX_COLOR_SET );
    if FFrame > 0 then
      asprite2d_Draw( FOpenGLTexture, x,y, w, h, angleDeg, FFrame, AColor.Alpha , GetFlags OR FX2D_RPIVOT or FX_COLOR)
    else
      ssprite2d_Draw( FOpenGLTexture, x,y, w, h, angleDeg, AColor.Alpha , GetFlags OR FX2D_RPIVOT or FX_COLOR);
    if FIsMask then fx_SetColorMode( FX_COLOR_MIX );
  end;
end;

procedure TBGLTexture.ToggleFlipX;
begin
  FZenFlags := FZenFlags xor FX2D_FLIPX;
end;

procedure TBGLTexture.ToggleFlipY;
begin
  FZenFlags := FZenFlags xor FX2D_FLIPY;
end;

procedure TBGLTexture.Init(ATexture: TBGLTextureHandle; AWidth, AHeight: integer; AOwned: boolean);
begin
  inherited Init(ATexture,AWidth,AHeight,AOwned);
  FZenFlags:= FX_BLEND;
end;

procedure TBGLTexture.NotifyInvalidFrameSize;
begin
  log_Add('Invalid frame size');
end;

procedure TBGLTexture.NotifyErrorLoadingFile(AFilename: string);
begin
  log_Add('Cannot load "'+AFilename+'"');
end;

function TBGLTexture.NewEmpty: TBGLCustomTexture;
begin
  result := TBGLTexture.Create;
end;

function TBGLTexture.NewFromTexture(ATexture: TBGLTextureHandle;
  AWidth, AHeight: integer): TBGLCustomTexture;
begin
  result := TBGLTexture.Create(ATexture,AWidth,AHeight);
end;

function TBGLTexture.Duplicate: TBGLCustomTexture;
begin
  result := inherited Duplicate;
  if (result is TBGLTexture) then
    TBGLTexture(result).FZenFlags := FZenFlags;
end;

procedure TBGLTexture.FreeMemoryOnDestroy;
begin
  //do not free handle, ZenGL takes care of it
end;

constructor TBGLTexture.Create(var AMemory: zglTMemory);
var s: TStream;
begin
  s := TZenMemoryStream.Create(@AMemory);
  InitFromStream(s);
  s.Free;
end;

procedure TBGLTexture.FreeOpenGLTexture(ATexture: TBGLTextureHandle);
begin
  if (ATexture <> nil) and (ATexture <> managerZeroTexture) then
  begin
    tex_Del(ATexture);
    FOpenGLTexture:= nil;
  end;
end;

procedure TBGLTexture.UpdateGLResampleFilter(ATexture: TBGLTextureHandle;
  AFilter: TOpenGLResampleFilter);
begin
  batch2d_Flush;
  glBindTexture( GL_TEXTURE_2D, zglTSprite2D(ATexture^).ID );
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

procedure TBGLTexture.DoDrawAffine(Origin, HAxis, VAxis: TPointF;
  AColor: TBGRAPixel);
var p2,p3,p4: TPointF;
begin
  if AColor.Alpha = 0 then exit;
  p2 := HAxis-Origin;
  p4 := VAxis-Origin;
  p3 := p2+p4;
  SetZenBlendMode(BlendMode);
  fx2d_SetVertexes(0,0,p2.x,p2.y,p3.x,p3.y,p4.x,p4.y);
  if (BGRAToColor(AColor)=clWhite) and not FIsMask then
  begin
    if FFrame > 0 then
     asprite2d_Draw( FOpenGLTexture, Origin.x, Origin.y, 0, 0, 0, FFrame, AColor.Alpha, GetFlags or FX2D_VCHANGE)
    else
     ssprite2d_Draw( FOpenGLTexture, Origin.x, Origin.y, 0, 0, 0, AColor.Alpha, GetFlags or FX2D_VCHANGE);
  end else
  begin
    fx2d_SetColor(BGRAToZenGL(AColor));
    if FIsMask then fx_SetColorMode( FX_COLOR_SET );
    if FFrame > 0 then
      asprite2d_Draw( FOpenGLTexture, Origin.x, Origin.y, 0, 0, 0, FFrame, AColor.Alpha, GetFlags OR FX_COLOR or FX2D_VCHANGE)
    else
      ssprite2d_Draw( FOpenGLTexture, Origin.x, Origin.y, 0, 0, 0, AColor.Alpha, GetFlags OR FX_COLOR or FX2D_VCHANGE);
    if FIsMask then fx_SetColorMode( FX_COLOR_MIX );
  end;
end;

{ TBGLBitmap }

function TBGLBitmap.GetOpenGLMaxTexSize: integer;
begin
  result := oglMaxTexSize;
end;

procedure TBGLBitmap.NotifySizeTooBigForOpenGL;
begin
  log_Add('Size too big for OpenGL');
end;

constructor TBGLBitmap.Create(AFilename: string);
begin
  inherited Create(AFilename, True);
end;

constructor TBGLBitmap.Create(var AMemory: zglTMemory);
var s: TStream;
begin
  s := TZenMemoryStream.Create(@AMemory);
  try
    inherited Create(s);
  finally
    s.Free;
  end;
end;

function TBGLBitmap.NewBitmap(Filename: string): TBGRACustomBitmap;
begin
  Result:=inherited NewBitmap(Filename, True);
end;

procedure TBGLBitmap.LoadFromFile(const filename: string);
begin
  inherited LoadFromFileUTF8(filename);
end;

procedure TBGLBitmap.LoadFromFile(const filename: String;
  Handler: TFPCustomImageReader);
begin
  inherited LoadFromFileUTF8(filename, Handler);
end;

procedure TBGLBitmap.SaveToFile(const filename: string);
begin
  inherited SaveToFileUTF8(filename);
end;

procedure TBGLBitmap.SaveToFile(const filename: string;
  Handler: TFPCustomImageWriter);
begin
  inherited SaveToFileUTF8(filename, Handler);
end;

initialization

  BGLBitmapFactory := TBGLBitmap;
  BGLTextureFactory := TBGLTexture;
{$IFDEF USE_ZENGL_SPRITE_ENGINE}
  BGRASpriteGL.BGLSpriteEngine := TBGLZenSpriteEngine.Create;
{$ELSE}
  BGRASpriteGL.BGLSpriteEngine := TBGLDefaultSpriteEngine.Create;
{$ENDIF}
  BGLCanvasInstance := TBGLZenCanvas.Create;

finalization

  BGLCanvasInstance.Free;
  BGRASpriteGL.BGLSpriteEngine.Free;
  BGRASpriteGL.BGLSpriteEngine := nil;

end.

