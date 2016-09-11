unit BGRACanvas2D;

{ To do :

  draw text with a different precision if the matrix is scaled
  drawImage(in image, in double sx, in double sy, in double sw, in double sh, in double dx, in double dy, in double dw, in double dh)
  -> using FillPoly with texture coordinates
  linear gradient any transformation
  clearPath clipping
  createRadialGradient
  globalCompositeOperation
  image data functions
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRAGraphics, BGRABitmapTypes, BGRATransform,
  BGRAGradientScanner, BGRAPath, BGRAPen;

type
  IBGRACanvasTextureProvider2D = interface
    function getTexture: IBGRAScanner;
    property texture: IBGRAScanner read GetTexture;
  end;

  IBGRACanvasGradient2D = interface(IBGRACanvasTextureProvider2D)
    procedure addColorStop(APosition: single; AColor: TBGRAPixel);
    procedure addColorStop(APosition: single; AColor: TColor);
    procedure addColorStop(APosition: single; AColor: string);
    procedure setColors(ACustomGradient: TBGRACustomGradient);
  end;

  { TBGRACanvasTextureProvider2D }

  TBGRACanvasTextureProvider2D = class(TInterfacedObject,IBGRACanvasTextureProvider2D)
    function getTexture: IBGRAScanner; virtual; abstract;
  end;

  { TBGRACanvasState2D }

  TBGRACanvasState2D = class
  private
    FClipMask: TBGRACustomBitmap;
    FClipMaskOwned: boolean;
    function GetClipMaskReadWrite: TBGRACustomBitmap;
  public
    strokeColor: TBGRAPixel;
    strokeTextureProvider: IBGRACanvasTextureProvider2D;
    fillColor: TBGRAPixel;
    fillTextureProvider: IBGRACanvasTextureProvider2D;
    globalAlpha: byte;

    fontName: string;
    fontStyle: TFontStyles;
    fontEmHeight: single;
    textAlign: TAlignment;
    textBaseline: string;

    lineWidth: single;
    penStroker: TBGRAPenStroker;

    shadowOffsetX,shadowOffsetY,shadowBlur: single;
    shadowColor: TBGRAPixel;
    shadowFastest: boolean;

    matrix: TAffineMatrix;
    constructor Create(AMatrix: TAffineMatrix; AClipMask: TBGRACustomBitmap; AClipMaskOwned: boolean);
    function Duplicate: TBGRACanvasState2D;
    destructor Destroy; override;
    procedure SetClipMask(AClipMask: TBGRACustomBitmap; AOwned: boolean);
    property clipMaskReadOnly: TBGRACustomBitmap read FClipMask;
    property clipMaskReadWrite: TBGRACustomBitmap read GetClipMaskReadWrite;
  end;

  TCanvas2dTextSize = record
    width,height: single;
  end;

  { TBGRACanvas2D }

  TBGRACanvas2D = class(IBGRAPath)
  private
    FSurface: TBGRACustomBitmap;
    StateStack: TList;
    currentState: TBGRACanvasState2D;
    FCanvasOffset: TPointF;
    FPixelCenteredCoordinates: boolean;
    FPathPoints: array of TPointF;
    FPathPointCount: integer;
    FFontRenderer: TBGRACustomFontRenderer;
    FLastCoord, FStartCoord: TPointF;
    function GetCurrentPathAsPoints: ArrayOfTPointF;
    function GetFontName: string;
    function GetFontRenderer: TBGRACustomFontRenderer;
    function GetFontEmHeight: single;
    function GetFontString: string;
    function GetFontStyle: TFontStyles;
    function GetGlobalAlpha: single;
    function GetHasShadow: boolean;
    function GetHeight: Integer;
    function GetLineCap: string;
    function GetLineCapLCL: TPenEndCap;
    function GetlineJoin: string;
    function GetlineJoinLCL: TPenJoinStyle;
    function GetLineWidth: single;
    function GetMatrix: TAffineMatrix;
    function GetMiterLimit: single;
    function GetPixelCenteredCoordinates: boolean;
    function GetShadowBlur: single;
    function GetShadowFastest: boolean;
    function GetShadowOffset: TPointF;
    function GetShadowOffsetX: single;
    function GetShadowOffsetY: single;
    function GetStrokeMatrix: TAffineMatrix;
    function GetTextAlign: string;
    function GetTextAlignLCL: TAlignment;
    function GetTextBaseline: string;
    function GetWidth: Integer;
    procedure SetFontName(AValue: string);
    procedure SetFontRenderer(AValue: TBGRACustomFontRenderer);
    procedure SetFontEmHeight(AValue: single);
    procedure SetFontString(AValue: string);
    procedure SetFontStyle(AValue: TFontStyles);
    procedure SetGlobalAlpha(const AValue: single);
    procedure SetLineCap(const AValue: string);
    procedure SetLineCapLCL(AValue: TPenEndCap);
    procedure SetLineJoin(const AValue: string);
    procedure FillPoly(const points: array of TPointF);
    procedure FillStrokePoly(const points: array of TPointF; fillOver: boolean);
    procedure SetLineJoinLCL(AValue: TPenJoinStyle);
    procedure SetLineWidth(const AValue: single);
    procedure SetMatrix(AValue: TAffineMatrix);
    procedure SetMiterLimit(const AValue: single);
    procedure SetPixelCenteredCoordinates(const AValue: boolean);
    procedure SetShadowBlur(const AValue: single);
    procedure SetShadowFastest(AValue: boolean);
    procedure SetShadowOffset(const AValue: TPointF);
    procedure SetShadowOffsetX(const AValue: single);
    procedure SetShadowOffsetY(const AValue: single);
    procedure SetStrokeMatrix(AValue: TAffineMatrix);
    procedure SetTextAlign(AValue: string);
    procedure SetTextAlignLCL(AValue: TAlignment);
    procedure SetTextBaseine(AValue: string);
    procedure StrokePoly(const points: array of TPointF);
    procedure DrawShadow(const points, points2: array of TPointF);
    procedure ClearPoly(const points: array of TPointF);
    function ApplyTransform(const points: array of TPointF; matrix: TAffineMatrix): ArrayOfTPointF; overload;
    function ApplyTransform(const points: array of TPointF): ArrayOfTPointF; overload;
    function ApplyTransform(point: TPointF): TPointF; overload;
    function GetPenPos(defaultX, defaultY: single): TPointF;
    function GetPenPos(defaultPt: TPointF): TPointF;
    procedure AddPoint(point: TPointF);
    procedure AddPoints(const points: array of TPointF);
    procedure AddPointsRev(const points: array of TPointF);
    function ApplyGlobalAlpha(color: TBGRAPixel): TBGRAPixel;
    function GetDrawMode: TDrawMode;
    procedure copyTo({%H-}dest: IBGRAPath); //IBGRAPath
    function getPoints: ArrayOfTPointF; //IBGRAPath
    function getPoints(AMatrix: TAffineMatrix): ArrayOfTPointF; //IBGRAPath
    function getCursor: TBGRACustomPathCursor; //IBGRAPath
  public
    antialiasing, linearBlend: boolean;
    constructor Create(ASurface: TBGRACustomBitmap);
    destructor Destroy; override;

    function toDataURL(mimeType: string = 'image/png'): string;

    procedure save;
    procedure restore;
    procedure scale(x,y: single); overload;
    procedure scale(factor: single); overload;
    procedure rotate(angleRadCW: single);
    procedure translate(x,y: single);
    procedure skewx(angleRadCW: single);
    procedure skewy(angleRadCW: single);
    procedure transform(m11,m21, m12,m22, m13,m23: single); overload;
    procedure transform(AMatrix: TAffineMatrix); overload;
    procedure setTransform(m11,m21, m12,m22, m13,m23: single);
    procedure resetTransform;

    procedure strokeScale(x,y: single);
    procedure strokeSkewx(angleRadCW: single);
    procedure strokeSkewy(angleRadCW: single);
    procedure strokeResetTransform;

    procedure strokeStyle(color: TBGRAPixel); overload;
    procedure strokeStyle(color: TColor); overload;
    procedure strokeStyle(color: string); overload;
    procedure strokeStyle(texture: IBGRAScanner); overload;
    procedure strokeStyle(provider: IBGRACanvasTextureProvider2D); overload;
    procedure fillStyle(color: TBGRAPixel); overload;
    procedure fillStyle(color: TColor); overload;
    procedure fillStyle(color: string); overload;
    procedure fillStyle(texture: IBGRAScanner); overload;
    procedure fillStyle(provider: IBGRACanvasTextureProvider2D); overload;
    procedure shadowColor(color: TBGRAPixel); overload;
    procedure shadowColor(color: TColor); overload;
    procedure shadowColor(color: string); overload;
    procedure shadowNone;
    function getShadowColor: TBGRAPixel;
    function createLinearGradient(x0,y0,x1,y1: single): IBGRACanvasGradient2D; overload;
    function createLinearGradient(p0,p1: TPointF): IBGRACanvasGradient2D; overload;
    function createLinearGradient(x0,y0,x1,y1: single; Colors: TBGRACustomGradient): IBGRACanvasGradient2D; overload;
    function createLinearGradient(p0,p1: TPointF; Colors: TBGRACustomGradient): IBGRACanvasGradient2D; overload;
    function createPattern(image: TBGRACustomBitmap; repetition: string): IBGRACanvasTextureProvider2D; overload;
    function createPattern(texture: IBGRAScanner): IBGRACanvasTextureProvider2D; overload;

    procedure fillRect(x,y,w,h: single);
    procedure strokeRect(x,y,w,h: single);
    procedure clearRect(x,y,w,h: single);

    procedure addPath(APath: IBGRAPath); overload;
    procedure addPath(ASvgPath: string); overload;
    procedure path(APath: IBGRAPath); overload;
    procedure path(ASvgPath: string); overload;
    procedure beginPath;
    procedure closePath;
    procedure toSpline(closed: boolean; style: TSplineStyle= ssOutside);
    procedure moveTo(x,y: single); overload;
    procedure lineTo(x,y: single); overload;
    procedure moveTo(const pt: TPointF); overload;
    procedure lineTo(const pt: TPointF); overload;
    procedure polylineTo(const pts: array of TPointF);
    procedure quadraticCurveTo(cpx,cpy,x,y: single); overload;
    procedure quadraticCurveTo(const cp,pt: TPointF); overload;
    procedure bezierCurveTo(cp1x,cp1y,cp2x,cp2y,x,y: single); overload;
    procedure bezierCurveTo(const cp1,cp2,pt: TPointF); overload;
    procedure rect(x,y,w,h: single);
    procedure roundRect(x,y,w,h,radius: single); overload;
    procedure roundRect(x,y,w,h,rx,ry: single); overload;
    procedure openedSpline(const pts: array of TPointF; style: TSplineStyle);
    procedure closedSpline(const pts: array of TPointF; style: TSplineStyle);
    procedure spline(const pts: array of TPointF; style: TSplineStyle= ssOutside);
    procedure splineTo(const pts: array of TPointF; style: TSplineStyle= ssOutside);
    procedure arc(x, y, radius, startAngleRadCW, endAngleRadCW: single; anticlockwise: boolean); overload;
    procedure arc(x, y, radius, startAngleRadCW, endAngleRadCW: single); overload;
    procedure arc(cx, cy, rx,ry, xAngleRadCW, startAngleRadCW, endAngleRadCW: single; anticlockwise: boolean); overload;
    procedure arc(cx, cy, rx,ry, xAngleRadCW, startAngleRadCW, endAngleRadCW: single); overload;
    procedure arc(const arcDef: TArcDef); overload;
    procedure arcTo(x1, y1, x2, y2, radius: single); overload;
    procedure arcTo(p1,p2: TPointF; radius: single); overload;
    procedure arcTo(rx, ry, xAngleRadCW: single; largeArc,anticlockwise: boolean; x, y: single);
    procedure circle(x,y,r: single);
    procedure ellipse(x,y,rx,ry: single);
    procedure text(AText: string; x,y: single);
    procedure fillText(AText: string; x,y: single);
    procedure strokeText(AText: string; x,y: single);
    function measureText(AText: string): TCanvas2dTextSize;

    procedure fill;
    procedure stroke;
    procedure fillOverStroke;
    procedure strokeOverFill;
    procedure clearPath;
    procedure clip;
    procedure unclip;
    function isPointInPath(x,y: single): boolean; overload;
    function isPointInPath(pt: TPointF): boolean; overload;

    procedure drawImage(image: TBGRACustomBitmap; dx,dy: single); overload;
    procedure drawImage(image: TBGRACustomBitmap; dx,dy,dw,dh: single); overload;

    function getLineStyle: TBGRAPenStyle;
    procedure lineStyle(const AValue: array of single); overload;
    procedure lineStyle(AStyle: TPenStyle); overload;

    property surface: TBGRACustomBitmap read FSurface;
    property width: Integer read GetWidth;
    property height: Integer read GetHeight;
    property pixelCenteredCoordinates: boolean read GetPixelCenteredCoordinates write SetPixelCenteredCoordinates;
    property globalAlpha: single read GetGlobalAlpha write SetGlobalAlpha;
    property matrix: TAffineMatrix read GetMatrix write SetMatrix;
    property strokeMatrix: TAffineMatrix read GetStrokeMatrix write SetStrokeMatrix;

    property lineWidth: single read GetLineWidth write SetLineWidth;
    property lineCap: string read GetLineCap write SetLineCap;
    property lineCapLCL: TPenEndCap read GetLineCapLCL write SetLineCapLCL;
    property lineJoin: string read GetlineJoin write SetLineJoin;
    property lineJoinLCL: TPenJoinStyle read GetlineJoinLCL write SetLineJoinLCL;
    property miterLimit: single read GetMiterLimit write SetMiterLimit;

    property shadowOffsetX: single read GetShadowOffsetX write SetShadowOffsetX;
    property shadowOffsetY: single read GetShadowOffsetY write SetShadowOffsetY;
    property shadowOffset: TPointF read GetShadowOffset write SetShadowOffset;
    property shadowBlur: single read GetShadowBlur write SetShadowBlur;
    property shadowFastest: boolean read GetShadowFastest write SetShadowFastest;
    property hasShadow: boolean read GetHasShadow;

    property fontName: string read GetFontName write SetFontName;
    property fontEmHeight: single read GetFontEmHeight write SetFontEmHeight;
    property fontStyle: TFontStyles read GetFontStyle write SetFontStyle;
    property font: string read GetFontString write SetFontString;
    property textAlignLCL: TAlignment read GetTextAlignLCL write SetTextAlignLCL;
    property textAlign: string read GetTextAlign write SetTextAlign;
    property textBaseline: string read GetTextBaseline write SetTextBaseine;

    property currentPath: ArrayOfTPointF read GetCurrentPathAsPoints;
    property fontRenderer: TBGRACustomFontRenderer read GetFontRenderer write SetFontRenderer;

  protected
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
    function _AddRef: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
    function _Release: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
  end;

implementation

uses Types, Math, BGRAFillInfo, BGRAPolygon, BGRABlend, FPWriteJPEG, FPWriteBMP, base64;

type
  TColorStop = record
    position: single;
    color: TBGRAPixel;
  end;

  TGradientArrayOfColors = array of TBGRAPixel;
  TGradientArrayOfPositions = array of single;

  { TBGRACanvasGradient2D }

  TBGRACanvasGradient2D = class(TBGRACanvasTextureProvider2D, IBGRACanvasGradient2D)
  private
    colorStops: array of TColorStop;
    nbColorStops: integer;
    FCustomGradient: TBGRACustomGradient;
  protected
    scanner: TBGRAGradientScanner;
    procedure CreateScanner; virtual; abstract;
    function getColorArray: TGradientArrayOfColors;
    function getPositionArray: TGradientArrayOfPositions;
  public
    function getTexture: IBGRAScanner; override;
    destructor Destroy; override;
    procedure addColorStop(APosition: single; AColor: TBGRAPixel);
    procedure addColorStop(APosition: single; AColor: TColor);
    procedure addColorStop(APosition: single; AColor: string);
    procedure setColors(ACustomGradient: TBGRACustomGradient);
    property texture: IBGRAScanner read GetTexture;
    property colorStopCount: integer read nbColorStops;
  end;

  { TBGRACanvasLinearGradient2D }

  TBGRACanvasLinearGradient2D = class(TBGRACanvasGradient2D)
  protected
    o1,o2: TPointF;
    procedure CreateScanner; override;
  public
    constructor Create(x0,y0,x1,y1: single);
    constructor Create(p0,p1: TPointF);
  end;

  { TBGRACanvasPattern2D }

  TBGRACanvasPattern2D = class(TBGRACanvasTextureProvider2D)
  protected
    scanner: TBGRACustomScanner;
    foreignInterface: IBGRAScanner;
    ownScanner: boolean;
  public
    function getTexture: IBGRAScanner; override;
    constructor Create(source: TBGRACustomBitmap; repeatX,repeatY: boolean; Origin, HAxis, VAxis: TPointF);
    constructor Create(source: IBGRAScanner; transformation: TAffineMatrix);
    destructor Destroy; override;
  end;

{ TBGRACanvasPattern2D }

function TBGRACanvasPattern2D.GetTexture: IBGRAScanner;
begin
  if ownScanner then
    result := scanner
  else
    result := foreignInterface;
end;

constructor TBGRACanvasPattern2D.Create(source: TBGRACustomBitmap; repeatX,
  repeatY: boolean; Origin, HAxis, VAxis: TPointF);
var
  affine: TBGRAAffineBitmapTransform;
begin
  if (abs(Origin.X-round(Origin.X)) < 1e-6) and
     (abs(Origin.Y-round(Origin.Y)) < 1e-6) and
     (HAxis = Origin+PointF(1,0)) and
     (VAxis = Origin+PointF(0,1)) then
  begin
    if (round(Origin.X)=0) and (round(Origin.Y)=0) and repeatX and repeatY then
    begin
      foreignInterface := source;
      ownScanner:= false;
    end else
    begin
      scanner := TBGRABitmapScanner.Create(source,repeatX,repeatY,Point(round(Origin.X),round(Origin.Y)));
      ownScanner := true;
    end;
  end
  else
  begin
    affine := TBGRAAffineBitmapTransform.Create(source,repeatX,repeatY);
    affine.Fit(Origin,HAxis,VAxis);
    scanner := affine;
    ownScanner:= true;
  end;
end;

constructor TBGRACanvasPattern2D.Create(source: IBGRAScanner;
  transformation: TAffineMatrix);
var
  affine : TBGRAAffineScannerTransform;
begin
  if (abs(transformation[1,1]-1) < 1e-6) and
     (abs(transformation[2,2]-1) < 1e-6) and
     (abs(transformation[1,2]) < 1e-6) and
     (abs(transformation[2,1]) < 1e-6) and
     (abs(transformation[1,3]-round(transformation[1,3])) < 1e-6) and
     (abs(transformation[2,3]-round(transformation[2,3])) < 1e-6) then
  begin
    if (abs(transformation[1,3]) < 1e-6) and
      (abs(transformation[2,3]) < 1e-6) then
    begin
      foreignInterface := source;
      ownScanner := false;
    end else
    begin
     scanner := TBGRAScannerOffset.Create(source,Point(round(transformation[1,3]),round(transformation[2,3])));
     ownScanner := true;
    end;
  end else
  begin
    affine := TBGRAAffineScannerTransform.Create(source);
    affine.Matrix := transformation;
    affine.Invert;
    scanner := affine;
    ownScanner:= true;
  end;
end;

destructor TBGRACanvasPattern2D.Destroy;
begin
  fillchar(foreignInterface,sizeof(foreignInterface),0);
  if ownScanner then FreeAndNil(scanner);
  inherited Destroy;
end;

{ TBGRACanvasLinearGradient2D }

procedure TBGRACanvasLinearGradient2D.CreateScanner;
var GradientOwner: boolean;
    GradientColors: TBGRACustomGradient;
begin
  if FCustomGradient = nil then
  begin
    GradientColors := TBGRAMultiGradient.Create(getColorArray,getPositionArray,False,False);
    GradientOwner := true;
  end else
  begin
    GradientColors := FCustomGradient;
    GradientOwner := false;
  end;
  scanner := TBGRAGradientScanner.Create(GradientColors,gtLinear,o1,o2,False,GradientOwner);
end;

constructor TBGRACanvasLinearGradient2D.Create(x0, y0, x1, y1: single);
begin
  o1 := PointF(x0,y0);
  o2 := PointF(x1,y1);
end;

constructor TBGRACanvasLinearGradient2D.Create(p0, p1: TPointF);
begin
  o1 := p0;
  o2 := p1;
end;

{ TBGRACanvasGradient2D }

function TBGRACanvasGradient2D.GetTexture: IBGRAScanner;
begin
  if scanner = nil then CreateScanner;
  result := scanner;
end;

function TBGRACanvasGradient2D.getColorArray: TGradientArrayOfColors;
var
  i: Integer;
begin
  setlength(result, nbColorStops);
  for i := 0 to nbColorStops-1 do
    result[i] := colorStops[i].color;
end;

function TBGRACanvasGradient2D.getPositionArray: TGradientArrayOfPositions;
var
  i: Integer;
begin
  setlength(result, nbColorStops);
  for i := 0 to nbColorStops-1 do
    result[i] := colorStops[i].position;
end;

destructor TBGRACanvasGradient2D.Destroy;
begin
  FreeAndNil(scanner);
  inherited Destroy;
end;

procedure TBGRACanvasGradient2D.addColorStop(APosition: single;
  AColor: TBGRAPixel);
begin
  FreeAndNil(scanner);
  if nbColorStops = length(colorStops) then
    setlength(colorStops, (length(colorStops)+1)*2);

  with colorStops[nbColorStops] do
  begin
    position := APosition;
    color := AColor;
  end;
  inc(nbColorStops);
end;

procedure TBGRACanvasGradient2D.addColorStop(APosition: single; AColor: TColor
  );
begin
  addColorStop(APosition, ColorToBGRA(ColorToRGB(AColor)));
end;

procedure TBGRACanvasGradient2D.addColorStop(APosition: single; AColor: string
  );
begin
  addColorStop(APosition, StrToBGRA(AColor));
end;

procedure TBGRACanvasGradient2D.setColors(ACustomGradient: TBGRACustomGradient
  );
begin
  FCustomGradient := ACustomGradient;
end;

{ TBGRACanvasState2D }

function TBGRACanvasState2D.GetClipMaskReadWrite: TBGRACustomBitmap;
begin
  if not FClipMaskOwned then
  begin
    if FClipMask <> nil then
      FClipMask := FClipMask.Duplicate;
    FClipMaskOwned := true;
  end;
  result := FClipMask;
end;

constructor TBGRACanvasState2D.Create(AMatrix: TAffineMatrix;
  AClipMask: TBGRACustomBitmap; AClipMaskOwned: boolean);
begin
  strokeColor := BGRABlack;
  fillColor := BGRABlack;
  globalAlpha := 255;

  fontName := 'Arial';
  fontEmHeight := 10;
  fontStyle := [];
  textAlign:= taLeftJustify;
  textBaseline := 'alphabetic';

  lineWidth := 1;
  penStroker := TBGRAPenStroker.Create;
  penStroker.LineCap := pecFlat;
  penStroker.JoinStyle := pjsMiter;
  penStroker.CustomPenStyle := DuplicatePenStyle(SolidPenStyle);
  penStroker.MiterLimit := 10;
  penStroker.StrokeMatrix := AffineMatrixIdentity;

  shadowOffsetX := 0;
  shadowOffsetY := 0;
  shadowBlur := 0;
  shadowColor := BGRAPixelTransparent;
  shadowFastest:= false;

  matrix := AMatrix;
  FClipMask := nil;
  FClipMaskOwned := true;
  SetClipMask(AClipMask,AClipMaskOwned);
end;

function TBGRACanvasState2D.Duplicate: TBGRACanvasState2D;
begin
  result := TBGRACanvasState2D.Create(matrix,clipMaskReadOnly,false);
  result.strokeColor := strokeColor;
  result.strokeTextureProvider := strokeTextureProvider;
  result.fillColor := fillColor;
  result.fillTextureProvider := fillTextureProvider;
  result.globalAlpha := globalAlpha;

  result.fontName:= fontName;
  result.fontEmHeight := fontEmHeight;
  result.fontStyle := fontStyle;

  result.lineWidth := lineWidth;
  result.penStroker.LineCap := penStroker.LineCap;
  result.penStroker.JoinStyle := penStroker.JoinStyle;
  result.penStroker.CustomPenStyle := DuplicatePenStyle(penStroker.CustomPenStyle);
  result.penStroker.MiterLimit := penStroker.MiterLimit;
  result.penStroker.StrokeMatrix := penStroker.StrokeMatrix;

  result.shadowOffsetX := shadowOffsetX;
  result.shadowOffsetY := shadowOffsetY;
  result.shadowBlur := shadowBlur;
  result.shadowColor := shadowColor;
  result.shadowFastest := shadowFastest;
end;

destructor TBGRACanvasState2D.Destroy;
begin
  if FClipMaskOwned and Assigned(FClipMask) then
    FClipMask.Free;
  penStroker.Free;
  inherited Destroy;
end;

procedure TBGRACanvasState2D.SetClipMask(AClipMask: TBGRACustomBitmap;
  AOwned: boolean);
begin
  if FClipMaskOwned and Assigned(FClipMask) then FreeAndNil(FClipMask);
  FClipMask := AClipMask;
  FClipMaskOwned := AOwned;
end;

{ TBGRACanvas2D }

function TBGRACanvas2D.GetHeight: Integer;
begin
  if Assigned(surface) then
    result := Surface.Height
  else
    result := 0;
end;

function TBGRACanvas2D.GetLineCap: string;
begin
  case currentState.penStroker.LineCap of
    pecRound: result := 'round';
    pecSquare: result := 'square';
    else result := 'butt';
  end;
end;

function TBGRACanvas2D.GetLineCapLCL: TPenEndCap;
begin
  result := currentState.penStroker.LineCap;
end;

function TBGRACanvas2D.GetlineJoin: string;
begin
  case currentState.penStroker.JoinStyle of
    pjsBevel: result := 'bevel';
    pjsRound: result := 'round';
    else result := 'miter';
  end;
end;

function TBGRACanvas2D.GetlineJoinLCL: TPenJoinStyle;
begin
  result := currentState.penStroker.JoinStyle;
end;

function TBGRACanvas2D.getLineStyle: TBGRAPenStyle;
begin
  result := DuplicatePenStyle(currentState.penStroker.CustomPenStyle);
end;

function TBGRACanvas2D.GetLineWidth: single;
begin
  result := currentState.lineWidth;
end;

function TBGRACanvas2D.GetMatrix: TAffineMatrix;
begin
  result := currentState.matrix;
end;

function TBGRACanvas2D.GetMiterLimit: single;
begin
  result := currentState.penStroker.MiterLimit;
end;

function TBGRACanvas2D.GetPixelCenteredCoordinates: boolean;
begin
  result := FPixelCenteredCoordinates;
end;

function TBGRACanvas2D.GetShadowBlur: single;
begin
  result := currentState.shadowBlur;
end;

function TBGRACanvas2D.GetShadowFastest: boolean;
begin
  result := currentState.shadowFastest;
end;

function TBGRACanvas2D.GetShadowOffset: TPointF;
begin
  result := PointF(shadowOffsetX,shadowOffsetY);
end;

function TBGRACanvas2D.GetShadowOffsetX: single;
begin
  result := currentState.shadowOffsetX;
end;

function TBGRACanvas2D.GetShadowOffsetY: single;
begin
  result := currentState.shadowOffsetY;
end;

function TBGRACanvas2D.GetStrokeMatrix: TAffineMatrix;
begin
  result := currentState.penStroker.StrokeMatrix;
end;

function TBGRACanvas2D.GetTextAlign: string;
begin
  case currentState.textAlign of
    taRightJustify: result := 'right';
    taCenter: result := 'center';
  else
    result := 'left';
  end;
end;

function TBGRACanvas2D.GetTextAlignLCL: TAlignment;
begin
  result := currentState.textAlign;
end;

function TBGRACanvas2D.GetTextBaseline: string;
begin
  result := currentState.textBaseline;
end;

function TBGRACanvas2D.GetGlobalAlpha: single;
begin
  result := currentState.globalAlpha/255;
end;

function TBGRACanvas2D.GetCurrentPathAsPoints: ArrayOfTPointF;
var i: integer;
begin
  setlength(result, FPathPointCount);
  for i := 0 to high(result) do
    result[i] := FPathPoints[i];
end;

function TBGRACanvas2D.GetFontName: string;
begin
  result := currentState.fontName;
end;

function TBGRACanvas2D.GetFontRenderer: TBGRACustomFontRenderer;
var zoom1,zoom2,zoom: single;
begin
  if FFontRenderer = nil then
  begin
    if FSurface <> nil then
      result := FSurface.FontRenderer
    else
      result := nil;
  end else
    result := FFontRenderer;
  if Assigned(result) then
  begin
    result.FontName := currentState.fontName;
    result.FontStyle := currentState.fontStyle;
    if antialiasing then
      result.FontQuality:= fqFineAntialiasing
    else
      result.FontQuality := fqSystem;
    result.FontOrientation := 0;
    zoom1 := VectLen(currentState.matrix[1,1],currentState.matrix[2,1]);
    zoom2 := VectLen(currentState.matrix[1,2],currentState.matrix[2,2]);
    if zoom1>zoom2 then zoom := zoom1 else zoom := zoom2;
    result.FontEmHeight := round(currentState.fontEmHeight*zoom);
  end;
end;

function TBGRACanvas2D.GetFontEmHeight: single;
begin
  result := currentState.fontEmHeight;
end;

function TBGRACanvas2D.GetFontString: string;
var formats: TFormatSettings;
begin
  formats := DefaultFormatSettings;
  formats.DecimalSeparator := '.';

  result := '';
  if fsItalic in currentState.fontStyle then
    result := result+'italic ';
  if fsBold in currentState.fontStyle then
    result += 'bold ';
  result += FloatToStrF(currentState.fontEmHeight,ffGeneral,6,0,formats)+'px ';
  result += currentState.fontName;
  result := trim(result);
end;

function TBGRACanvas2D.GetFontStyle: TFontStyles;
begin
  result := currentState.fontStyle;
end;

function TBGRACanvas2D.GetHasShadow: boolean;
begin
  result := (ApplyGlobalAlpha(currentState.shadowColor).alpha <> 0) and
    ( (currentState.shadowBlur <> 0) or (currentState.shadowOffsetX <> 0)
      or (currentState.shadowOffsetY <> 0) );
end;

function TBGRACanvas2D.GetWidth: Integer;
begin
  if Assigned(Surface) then
    result := Surface.Width
  else
    result := 0;
end;

procedure TBGRACanvas2D.SetFontName(AValue: string);
begin
  currentState.fontName := AValue;
end;

procedure TBGRACanvas2D.SetFontRenderer(AValue: TBGRACustomFontRenderer);
begin
  if AValue = FFontRenderer then exit;
  FreeAndNil(FFontRenderer);
  FFontRenderer := AValue;
end;

procedure TBGRACanvas2D.SetFontEmHeight(AValue: single);
begin
  currentState.fontEmHeight := AValue;
end;

procedure TBGRACanvas2D.SetFontString(AValue: string);
var idxSpace,errPos: integer;
  attrib,u: string;
  value: single;
begin
  currentState.fontStyle := [];
  currentState.fontEmHeight := 10;
  currentState.fontName := 'Arial';
  AValue := trim(AValue);
  while AValue <> '' do
  begin
    while (AValue <> '') and (AValue[1]in [#0..#32]) do delete(AValue,1,1);
    idxSpace := pos(' ',AValue);
    if idxSpace = 0 then
      attrib := AValue
    else
      attrib := copy(AValue,1,idxSpace-1);
    attrib := lowerCase(attrib);
    if attrib = '' then break;
    if (attrib = 'normal') or (attrib = 'small-caps') or (attrib = 'lighter') then
    begin
      //nothing
    end else
    if (attrib = 'italic') or (attrib = 'oblique') then
    begin
      currentState.fontStyle += [fsItalic];
    end else
    if (attrib = 'bold') or (attrib = 'bolder') then
    begin
      currentState.fontStyle += [fsBold];
    end else
    if (attrib[1] in ['.','0'..'9']) then
    begin
      u := '';
      while (length(attrib)>0) and (attrib[length(attrib)] in['a'..'z']) do
      begin
        u := attrib[length(attrib)]+u;
        delete(attrib,length(attrib),1);
      end;
      val(attrib,value,errPos);
      if errPos = 0 then
      begin
        if u = '' then //weight
        begin
          if value >= 600 then currentState.fontStyle += [fsBold];
        end else
        if u = 'px' then currentState.fontEmHeight := value else
        if u = 'pt' then currentState.fontEmHeight:= value/72*96 else
        if u = 'in' then currentState.fontEmHeight:= value*96 else
        if u = 'mm' then currentState.fontEmHeight:= value/25.4*96 else
        if u = 'cm' then currentState.fontEmHeight:= value/2.54*96;
      end;
    end else
      break;
    delete(AValue,1,length(attrib)+1);
  end;
  AValue := trim(AValue);
  if AValue <> '' then currentState.fontName := AValue;
end;

procedure TBGRACanvas2D.SetFontStyle(AValue: TFontStyles);
begin
  currentState.fontStyle:= AValue;
end;

procedure TBGRACanvas2D.SetGlobalAlpha(const AValue: single);
begin
  if AValue < 0 then currentState.globalAlpha:= 0 else
  if AValue > 1 then currentState.globalAlpha:= 255 else
    currentState.globalAlpha:= round(AValue*255);
end;

procedure TBGRACanvas2D.SetLineCap(const AValue: string);
begin
  if CompareText(AValue,'round')=0 then
    currentState.penStroker.LineCap := pecRound else
  if CompareText(AValue,'square')=0 then
    currentState.penStroker.LineCap := pecSquare
  else
    currentState.penStroker.LineCap := pecFlat;
end;

procedure TBGRACanvas2D.SetLineCapLCL(AValue: TPenEndCap);
begin
  currentState.penStroker.LineCap := AValue;
end;

procedure TBGRACanvas2D.SetLineJoin(const AValue: string);
begin
  if CompareText(AValue,'round')=0 then
    currentState.penStroker.JoinStyle := pjsRound else
  if CompareText(AValue,'bevel')=0 then
    currentState.penStroker.JoinStyle := pjsBevel
  else
    currentState.penStroker.JoinStyle := pjsMiter;
end;

procedure TBGRACanvas2D.FillPoly(const points: array of TPointF);
var
  tempScan: TBGRACustomScanner;
begin
  if (length(points) = 0) or (surface = nil) then exit;
  If hasShadow then DrawShadow(points,[]);
  if currentState.clipMaskReadOnly <> nil then
  begin
    if currentState.fillTextureProvider <> nil then
      tempScan := TBGRATextureMaskScanner.Create(currentState.clipMaskReadOnly,Point(0,0),currentState.fillTextureProvider.texture,currentState.globalAlpha)
    else
      tempScan := TBGRASolidColorMaskScanner.Create(currentState.clipMaskReadOnly,Point(0,0),ApplyGlobalAlpha(currentState.fillColor));
    if self.antialiasing then
      BGRAPolygon.FillPolyAntialiasWithTexture(surface, points, tempScan, true, linearBlend)
    else
      BGRAPolygon.FillPolyAliasedWithTexture(surface, points, tempScan, true, GetDrawMode);
    tempScan.free;
  end else
  begin
    if currentState.fillTextureProvider <> nil then
    begin
      if currentState.globalAlpha <> 255 then
      begin
        tempScan := TBGRAOpacityScanner.Create(currentState.fillTextureProvider.texture, currentState.globalAlpha);
        if self.antialiasing then
          BGRAPolygon.FillPolyAntialiasWithTexture(surface, points, tempScan, true, linearBlend)
        else
          BGRAPolygon.FillPolyAliasedWithTexture(surface, points, tempScan, true, GetDrawMode);
        tempScan.Free;
      end else
      begin
        if self.antialiasing then
          BGRAPolygon.FillPolyAntialiasWithTexture(surface, points, currentState.fillTextureProvider.texture, true, linearBlend)
        else
          BGRAPolygon.FillPolyAliasedWithTexture(surface, points, currentState.fillTextureProvider.texture, true, GetDrawMode);
      end
    end
    else
    begin
      if self.antialiasing then
        BGRAPolygon.FillPolyAntialias(surface, points, ApplyGlobalAlpha(currentState.fillColor), false, true, linearBlend)
      else
        BGRAPolygon.FillPolyAliased(surface, points, ApplyGlobalAlpha(currentState.fillColor), false, true, GetDrawMode)
    end
  end;
end;

procedure TBGRACanvas2D.FillStrokePoly(const points: array of TPointF;
  fillOver: boolean);
var
  tempScan,tempScan2: TBGRACustomScanner;
  multi: TBGRAMultishapeFiller;
  contour : array of TPointF;
  texture: IBGRAScanner;
begin
  if (length(points) = 0) or (surface = nil) then exit;
  tempScan := nil;
  tempScan2 := nil;
  multi := TBGRAMultishapeFiller.Create;
  multi.FillMode := fmWinding;
  if currentState.clipMaskReadOnly <> nil then
  begin
    if currentState.fillTextureProvider <> nil then
      tempScan := TBGRATextureMaskScanner.Create(currentState.clipMaskReadOnly,Point(0,0),currentState.fillTextureProvider.texture,currentState.globalAlpha)
    else
      tempScan := TBGRASolidColorMaskScanner.Create(currentState.clipMaskReadOnly,Point(0,0),ApplyGlobalAlpha(currentState.fillColor));
    multi.AddPolygon(points, tempScan);
  end else
  begin
    if currentState.fillTextureProvider <> nil then
    begin
      if currentState.globalAlpha <> 255 then
      begin
        tempScan := TBGRAOpacityScanner.Create(currentState.fillTextureProvider.texture, currentState.globalAlpha);
        multi.AddPolygon(points, tempScan);
      end else
        multi.AddPolygon(points, currentState.fillTextureProvider.texture)
    end
    else
      multi.AddPolygon(points, ApplyGlobalAlpha(currentState.fillColor));
  end;

  if currentState.lineWidth > 0 then
  begin
    contour := currentState.penStroker.ComputePolylineAutocycle(points,currentState.lineWidth);

    if currentState.clipMaskReadOnly <> nil then
    begin
      if currentState.strokeTextureProvider <> nil then
        tempScan2 := TBGRATextureMaskScanner.Create(currentState.clipMaskReadOnly,Point(0,0),currentState.strokeTextureProvider.texture,currentState.globalAlpha)
      else
        tempScan2 := TBGRASolidColorMaskScanner.Create(currentState.clipMaskReadOnly,Point(0,0),ApplyGlobalAlpha(currentState.strokeColor));
      multi.AddPolygon(contour,tempScan);
    end else
    begin
      if currentState.strokeTextureProvider <> nil then
        texture := currentState.strokeTextureProvider.texture else
        texture := nil;
      if texture = nil then
        multi.AddPolygon(contour,ApplyGlobalAlpha(currentState.strokeColor))
      else
        multi.AddPolygon(contour,texture);
    end;
    If hasShadow then DrawShadow(points,contour);
  end else
    If hasShadow then DrawShadow(points,[]);

  if fillOver then multi.PolygonOrder := poFirstOnTop else multi.PolygonOrder:= poLastOnTop;
  multi.Antialiasing := self.antialiasing;
  multi.Draw(surface);
  tempScan.free;
  tempScan2.free;
  multi.Free;
end;

procedure TBGRACanvas2D.SetLineJoinLCL(AValue: TPenJoinStyle);
begin
  currentState.penStroker.JoinStyle := AValue;
end;

procedure TBGRACanvas2D.lineStyle(const AValue: array of single);
begin
  currentState.penStroker.CustomPenStyle := DuplicatePenStyle(AValue);
end;

procedure TBGRACanvas2D.lineStyle(AStyle: TPenStyle);
begin
  case AStyle of
    psSolid: lineStyle(SolidPenStyle);
    psDash: lineStyle(DashPenStyle);
    psDot: lineStyle(DotPenStyle);
    psDashDot: lineStyle(DashDotPenStyle);
    psDashDotDot: lineStyle(DashDotDotPenStyle);
    psClear: lineStyle(ClearPenStyle);
  end;
end;

function TBGRACanvas2D.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
begin
  if GetInterface(iid, obj) then
    Result := S_OK
  else
    Result := longint(E_NOINTERFACE);
end;

{ There is no automatic reference counting, but it is compulsory to define these functions }
function TBGRACanvas2D._AddRef: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
begin
  result := 0;
end;

function TBGRACanvas2D._Release: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
begin
  result := 0;
end;

procedure TBGRACanvas2D.SetLineWidth(const AValue: single);
begin
  currentState.lineWidth := AValue;
end;

procedure TBGRACanvas2D.SetMatrix(AValue: TAffineMatrix);
begin
  currentState.matrix := AValue;
end;

procedure TBGRACanvas2D.SetMiterLimit(const AValue: single);
begin
  currentState.penStroker.MiterLimit := AValue;
end;

procedure TBGRACanvas2D.SetPixelCenteredCoordinates(const AValue: boolean);
begin
  FPixelCenteredCoordinates:= AValue;
  if AValue then
    FCanvasOffset := PointF(0,0)
  else
    FCanvasOffset := PointF(-0.5,-0.5);
end;

procedure TBGRACanvas2D.SetShadowBlur(const AValue: single);
begin
  currentState.shadowBlur := AValue;
end;

procedure TBGRACanvas2D.SetShadowFastest(AValue: boolean);
begin
  currentState.shadowFastest := AValue;
end;

procedure TBGRACanvas2D.SetShadowOffset(const AValue: TPointF);
begin
  shadowOffsetX := AValue.X;
  shadowOffsetY := AValue.Y;
end;

procedure TBGRACanvas2D.SetShadowOffsetX(const AValue: single);
begin
  currentState.shadowOffsetX := AValue;
end;

procedure TBGRACanvas2D.SetShadowOffsetY(const AValue: single);
begin
  currentState.shadowOffsetY := AValue;
end;

procedure TBGRACanvas2D.SetStrokeMatrix(AValue: TAffineMatrix);
begin
  currentState.penStroker.strokeMatrix := AValue;
end;

procedure TBGRACanvas2D.SetTextAlign(AValue: string);
begin
  AValue := trim(LowerCase(AValue));
  if (AValue = 'left') or (AValue = 'start') then
    textAlignLCL := taLeftJustify else
  if (AValue = 'right') or (AValue = 'end') then
    textAlignLCL := taRightJustify else
  if AValue = 'center' then
    textAlignLCL := taCenter;
end;

procedure TBGRACanvas2D.SetTextAlignLCL(AValue: TAlignment);
begin
  currentState.textAlign := AValue;
end;

procedure TBGRACanvas2D.SetTextBaseine(AValue: string);
begin
  currentState.textBaseline := trim(lowercase(AValue));
end;

procedure TBGRACanvas2D.StrokePoly(const points: array of TPointF);
var
  texture: IBGRAScanner;
  tempScan: TBGRACustomScanner;
  contour: array of TPointF;
begin
  if (length(points)= 0) or (currentState.lineWidth = 0) or (surface = nil) then exit;
  contour := currentState.penStroker.ComputePolylineAutocycle(points,currentState.lineWidth);

  If hasShadow then DrawShadow(contour,[]);
  if currentState.clipMaskReadOnly <> nil then
  begin
    if currentState.strokeTextureProvider <> nil then
      tempScan := TBGRATextureMaskScanner.Create(currentState.clipMaskReadOnly,Point(0,0),currentState.strokeTextureProvider.texture,currentState.globalAlpha)
    else
      tempScan := TBGRASolidColorMaskScanner.Create(currentState.clipMaskReadOnly,Point(0,0),ApplyGlobalAlpha(currentState.strokeColor));
    if self.antialiasing then
      BGRAPolygon.FillPolyAntialiasWithTexture(Surface,contour,tempScan,True, linearBlend)
    else
      BGRAPolygon.FillPolyAliasedWithTexture(Surface,contour,tempScan,True,GetDrawMode);
    tempScan.free;
  end else
  begin
    if currentState.strokeTextureProvider <> nil then
      texture := currentState.strokeTextureProvider.texture else
      texture := nil;
    if texture = nil then
    begin
      if self.antialiasing then
        BGRAPolygon.FillPolyAntialias(Surface,contour,ApplyGlobalAlpha(currentState.strokeColor),false,True, linearBlend)
      else
        BGRAPolygon.FillPolyAliased(Surface,contour,ApplyGlobalAlpha(currentState.strokeColor),false,True,GetDrawMode)
    end
    else
    begin
      if self.antialiasing then
        BGRAPolygon.FillPolyAntialiasWithTexture(Surface,contour,texture,True, linearBlend)
      else
        BGRAPolygon.FillPolyAliasedWithTexture(Surface,contour,texture,True,GetDrawMode)
    end;
  end;
end;

procedure TBGRACanvas2D.DrawShadow(const points, points2: array of TPointF);
const invSqrt2 = 1/sqrt(2);
var ofsPts,ofsPts2: array of TPointF;
    offset: TPointF;
    i: Integer;
    tempBmp,blurred: TBGRACustomBitmap;
    maxRect: TRect;
    foundRect: TRect;
    firstFound: boolean;

    procedure AddPt(const coord: TPointF);
    var pixRect: TRect;
    begin
      if isEmptyPointF(coord) then exit;
      pixRect := Types.Rect(round(floor(coord.x)),round(floor(coord.y)),round(ceil(coord.x+0.999))+1,round(ceil(coord.y+0.999))+1);
      if firstFound then
      begin
        foundRect := pixRect;
        firstFound := false
      end
      else
      begin
        if pixRect.left < foundRect.left then foundRect.left := pixRect.Left;
        if pixRect.top < foundRect.top then foundRect.top := pixRect.top;
        if pixRect.right > foundRect.right then foundRect.right := pixRect.right;
        if pixRect.bottom > foundRect.bottom then foundRect.bottom := pixRect.bottom;
      end;
    end;

begin
  if not hasShadow or (surface = nil) then exit;
  offset := PointF(shadowOffsetX,shadowOffsetY);
  setlength(ofsPts, length(points));
  for i := 0 to high(ofsPts) do
    ofsPts[i] := points[i]+offset;
  setlength(ofsPts2, length(points2));
  for i := 0 to high(ofsPts2) do
    ofsPts2[i] := points2[i]+offset;

  maxRect := Types.Rect(0,0,width,height);
  if currentState.clipMaskReadOnly <> nil then
    foundRect := maxRect
  else
  begin
    firstFound := true;
    foundRect := EmptyRect;
    for i := 0 to high(ofsPts) do
      AddPt(ofsPts[i]);
    for i := 0 to high(ofsPts2) do
      AddPt(ofsPts2[i]);
    if firstFound then exit;
    InflateRect(foundRect, ceil(shadowBlur),ceil(shadowBlur));
    if not IntersectRect(foundRect, foundRect,maxRect) then exit;
    offset := PointF(-foundRect.Left,-foundRect.Top);
    for i := 0 to high(ofsPts) do
      ofsPts[i] += offset;
    for i := 0 to high(ofsPts2) do
      ofsPts2[i] += offset;
  end;

  tempBmp := surface.NewBitmap(foundRect.Right-foundRect.Left,foundRect.Bottom-foundRect.Top,BGRAPixelTransparent);
  tempBmp.FillMode := fmWinding;
  tempBmp.FillPolyAntialias(ofsPts, getShadowColor);
  tempBmp.FillPolyAntialias(ofsPts2, getShadowColor);
  if shadowBlur > 0 then
  begin
    if shadowFastest then
    begin
      if shadowBlur*invSqrt2 >= 0.5 then
      begin
        blurred := tempBmp.FilterBlurRadial(round(shadowBlur*invSqrt2),rbBox);
        tempBmp.Free;
        tempBmp := blurred;
      end;
    end
    else
    begin
      if (shadowBlur < 5) and (abs(shadowBlur-round(shadowBlur)) > 1e-6) then
        blurred := tempBmp.FilterBlurRadial(round(shadowBlur*10),rbPrecise)
      else
        blurred := tempBmp.FilterBlurRadial(round(shadowBlur),rbFast);
      tempBmp.Free;
      tempBmp := blurred;
    end;
  end;
  if currentState.clipMaskReadOnly <> nil then
    tempBmp.ApplyMask(currentState.clipMaskReadOnly);
  surface.PutImage(foundRect.Left,foundRect.Top,tempBmp,GetDrawMode,currentState.globalAlpha);
  tempBmp.Free;
end;

procedure TBGRACanvas2D.ClearPoly(const points: array of TPointF);
begin
  if surface = nil then exit;
  if self.antialiasing then
    BGRAPolygon.FillPolyAntialias(surface, points, BGRA(0,0,0,255), true, true, linearBlend)
  else
    BGRAPolygon.FillPolyAliased(surface, points, BGRA(0,0,0,255), true, true, dmSet);
end;

function TBGRACanvas2D.ApplyTransform(const points: array of TPointF;
  matrix: TAffineMatrix): ArrayOfTPointF;
var
  i: Integer;
begin
  setlength(result,length(points));
  for i := 0 to high(result) do
    if isEmptyPointF(points[i]) then
      result[i] := EmptyPointF
    else
      result[i] := matrix*points[i]+FCanvasOffset;
end;

function TBGRACanvas2D.ApplyTransform(const points: array of TPointF
  ): ArrayOfTPointF;
var
  i: Integer;
begin
  setlength(result,length(points));
  for i := 0 to high(result) do
    if isEmptyPointF(points[i]) then
      result[i] := EmptyPointF
    else
      result[i] := currentState.matrix*points[i]+FCanvasOffset;
end;

function TBGRACanvas2D.ApplyTransform(point: TPointF): TPointF;
begin
  result := currentState.matrix*point+FCanvasOffset;
end;

function TBGRACanvas2D.GetPenPos(defaultX,defaultY: single): TPointF;
begin
  if isEmptyPointF(FLastCoord) then
    result := PointF(defaultX,defaultY)
  else
    result := FLastCoord;
end;

function TBGRACanvas2D.GetPenPos(defaultPt: TPointF): TPointF;
begin
  result := GetPenPos(defaultPt.x,defaultPt.y);
end;

procedure TBGRACanvas2D.AddPoint(point: TPointF);
begin
  if FPathPointCount = length(FPathPoints) then
    setlength(FPathPoints, (length(FPathPoints)+1)*2);
  FPathPoints[FPathPointCount] := point;
  inc(FPathPointCount);
end;

procedure TBGRACanvas2D.AddPoints(const points: array of TPointF);
var i: integer;
begin
  if FPathPointCount+length(points) > length(FPathPoints) then
    setlength(FPathPoints, max( (length(FPathPoints)+1)*2, FPathPointCount+length(points) ) );
  for i := 0 to high(points) do
  begin
    FPathPoints[FPathPointCount] := points[i];
    inc(FPathPointCount);
  end;
end;

procedure TBGRACanvas2D.AddPointsRev(const points: array of TPointF);
var i: integer;
begin
  if FPathPointCount+length(points) > length(FPathPoints) then
    setlength(FPathPoints, max( (length(FPathPoints)+1)*2, FPathPointCount+length(points) ) );
  for i := high(points) downto 0 do
  begin
    FPathPoints[FPathPointCount] := points[i];
    inc(FPathPointCount);
  end;
end;

function TBGRACanvas2D.ApplyGlobalAlpha(color: TBGRAPixel): TBGRAPixel;
begin
  result := BGRA(color.red,color.green,color.blue,ApplyOpacity(color.alpha, currentState.globalAlpha));
end;

function TBGRACanvas2D.GetDrawMode: TDrawMode;
begin
  if linearBlend then result := dmLinearBlend else result := dmDrawWithTransparency;
end;

procedure TBGRACanvas2D.copyTo(dest: IBGRAPath);
begin
  //nothing
end;

function TBGRACanvas2D.getPoints: ArrayOfTPointF;
begin
  result := GetCurrentPathAsPoints;
end;

function TBGRACanvas2D.getPoints(AMatrix: TAffineMatrix): ArrayOfTPointF;
begin
  result := GetCurrentPathAsPoints;
  if not IsAffineMatrixIdentity(AMatrix) then
    result := AMatrix*result;
end;

function TBGRACanvas2D.getCursor: TBGRACustomPathCursor;
begin
  result := nil;
end;

constructor TBGRACanvas2D.Create(ASurface: TBGRACustomBitmap);
begin
  FSurface := ASurface;
  StateStack := TList.Create;
  FPathPointCount := 0;
  FLastCoord := EmptyPointF;
  FStartCoord := EmptyPointF;
  currentState := TBGRACanvasState2D.Create(AffineMatrixIdentity,nil,true);
  pixelCenteredCoordinates := false;
  antialiasing := true;
end;

destructor TBGRACanvas2D.Destroy;
var
  i: Integer;
begin
  for i := 0 to StateStack.Count-1 do
    TObject(StateStack[i]).Free;
  StateStack.Free;
  currentState.Free;
  FreeAndNil(FFontRenderer);
  inherited Destroy;
end;

function TBGRACanvas2D.toDataURL(mimeType: string): string;
var
  stream: TMemoryStream;
  jpegWriter: TFPWriterJPEG;
  bmpWriter: TFPWriterBMP;
  output: TStringStream;
  encode64: TBase64EncodingStream;
begin
  if surface = nil then exit;
  stream := TMemoryStream.Create;
  if mimeType='image/jpeg' then
  begin
    jpegWriter := TFPWriterJPEG.Create;
    Surface.SaveToStream(stream,jpegWriter);
    jpegWriter.Free;
  end else
  if mimeType='image/bmp' then
  begin
    bmpWriter := TFPWriterBMP.Create;
    Surface.SaveToStream(stream,bmpWriter);
    bmpWriter.Free;
  end else
  begin
    mimeType := 'image/png';
    Surface.SaveToStreamAsPng(stream);
  end;
  output := TStringStream.Create('data:'+mimeType+';base64,');
  output.Position := output.size;
  stream.Position := 0;
  encode64 := TBase64EncodingStream.Create(output);
  encode64.CopyFrom(stream,stream.size);
  encode64.free;
  stream.free;
  result := output.DataString;
  output.free;
end;

procedure TBGRACanvas2D.save;
var cur: TBGRACanvasState2D;
begin
  cur := currentState.Duplicate;
  StateStack.Add(cur);
end;

procedure TBGRACanvas2D.restore;
begin
  if StateStack.Count > 0 then
  begin
    FreeAndNil(currentState);
    currentState := TBGRACanvasState2D(StateStack[StateStack.Count-1]);
    StateStack.Delete(StateStack.Count-1);
  end;
end;

procedure TBGRACanvas2D.scale(x, y: single);
begin
  currentState.matrix *= AffineMatrixScale(x,y);
end;

procedure TBGRACanvas2D.scale(factor: single);
begin
  currentState.matrix *= AffineMatrixScale(factor,factor);
end;

procedure TBGRACanvas2D.rotate(angleRadCW: single);
begin
  currentState.matrix *= AffineMatrixRotationRad(-angleRadCW);
end;

procedure TBGRACanvas2D.translate(x, y: single);
begin
  if (x = 0) and (y = 0) then exit;
  currentState.matrix *= AffineMatrixTranslation(x,y);
end;

procedure TBGRACanvas2D.skewx(angleRadCW: single);
begin
  currentState.matrix *= AffineMatrixSkewXRad(-angleRadCW);
end;

procedure TBGRACanvas2D.skewy(angleRadCW: single);
begin
  currentState.matrix *= AffineMatrixSkewYRad(-angleRadCW);
end;

procedure TBGRACanvas2D.transform(m11,m21, m12,m22, m13,m23: single);
begin
  currentState.matrix *= AffineMatrix(m11,m12,m13,
                                      m21,m22,m23);
end;

procedure TBGRACanvas2D.transform(AMatrix: TAffineMatrix);
begin
  currentState.matrix *= AMatrix;
end;

procedure TBGRACanvas2D.setTransform(m11,m21, m12,m22, m13,m23: single);
begin
  currentState.matrix := AffineMatrix(m11,m12,m13,
                                      m21,m22,m23);
end;

procedure TBGRACanvas2D.resetTransform;
begin
  currentState.matrix := AffineMatrixIdentity;
end;

procedure TBGRACanvas2D.strokeScale(x, y: single);
begin
  currentState.penStroker.strokeMatrix := currentState.penStroker.strokeMatrix * AffineMatrixScale(x,y);
end;

procedure TBGRACanvas2D.strokeSkewx(angleRadCW: single);
begin
  currentState.penStroker.strokeMatrix := currentState.penStroker.strokeMatrix * AffineMatrixSkewXRad(-angleRadCW);
end;

procedure TBGRACanvas2D.strokeSkewy(angleRadCW: single);
begin
  currentState.penStroker.strokeMatrix := currentState.penStroker.strokeMatrix * AffineMatrixSkewYRad(-angleRadCW);
end;

procedure TBGRACanvas2D.strokeResetTransform;
begin
  currentState.penStroker.strokeMatrix := AffineMatrixIdentity;
end;

procedure TBGRACanvas2D.strokeStyle(color: TBGRAPixel);
begin
  currentState.strokeColor := color;
  currentState.strokeTextureProvider := nil;
end;

procedure TBGRACanvas2D.strokeStyle(color: TColor);
begin
  currentState.strokeColor := ColorToBGRA(ColorToRGB(color));
  currentState.strokeTextureProvider := nil;
end;

procedure TBGRACanvas2D.strokeStyle(color: string);
begin
  currentState.strokeColor := StrToBGRA(color);
  currentState.strokeTextureProvider := nil;
end;

procedure TBGRACanvas2D.strokeStyle(texture: IBGRAScanner);
begin
  strokeStyle(createPattern(texture));
end;

procedure TBGRACanvas2D.strokeStyle(provider: IBGRACanvasTextureProvider2D);
begin
  currentState.strokeColor := BGRAPixelTransparent;
  currentState.strokeTextureProvider := provider;
end;

procedure TBGRACanvas2D.fillStyle(color: TBGRAPixel);
begin
  currentState.fillColor := color;
  currentState.fillTextureProvider := nil;
end;

procedure TBGRACanvas2D.fillStyle(color: TColor);
begin
  currentState.fillColor := ColorToBGRA(ColorToRGB(color));
  currentState.fillTextureProvider := nil;
end;

procedure TBGRACanvas2D.fillStyle(color: string);
begin
  currentState.fillColor := StrToBGRA(color);
  currentState.fillTextureProvider := nil;
end;

procedure TBGRACanvas2D.fillStyle(texture: IBGRAScanner);
begin
  fillStyle(createPattern(texture));
end;

procedure TBGRACanvas2D.fillStyle(provider: IBGRACanvasTextureProvider2D);
begin
  currentState.fillColor := BGRAPixelTransparent;
  currentState.fillTextureProvider := provider;
end;

procedure TBGRACanvas2D.shadowColor(color: TBGRAPixel);
begin
  currentState.shadowColor := color;
end;

procedure TBGRACanvas2D.shadowColor(color: TColor);
begin
  shadowColor(ColorToBGRA(ColorToRGB(color)));
end;

procedure TBGRACanvas2D.shadowColor(color: string);
begin
  shadowColor(StrToBGRA(color));
end;

procedure TBGRACanvas2D.shadowNone;
begin
  shadowColor(BGRAPixelTransparent);
end;

function TBGRACanvas2D.getShadowColor: TBGRAPixel;
begin
  result := currentState.shadowColor;
end;

function TBGRACanvas2D.createLinearGradient(x0, y0, x1, y1: single
  ): IBGRACanvasGradient2D;
begin
  result := createLinearGradient(ApplyTransform(PointF(x0,y0)), ApplyTransform(PointF(x1,y1)));
end;

function TBGRACanvas2D.createLinearGradient(p0, p1: TPointF
  ): IBGRACanvasGradient2D;
begin
  result := TBGRACanvasLinearGradient2D.Create(p0,p1);
end;

function TBGRACanvas2D.createLinearGradient(x0, y0, x1, y1: single;
  Colors: TBGRACustomGradient): IBGRACanvasGradient2D;
begin
  result := createLinearGradient(x0,y0,x1,y1);
  result.setColors(Colors);
end;

function TBGRACanvas2D.createLinearGradient(p0, p1: TPointF;
  Colors: TBGRACustomGradient): IBGRACanvasGradient2D;
begin
  result := createLinearGradient(p0,p1);
  result.setColors(Colors);
end;

function TBGRACanvas2D.createPattern(image: TBGRACustomBitmap; repetition: string
  ): IBGRACanvasTextureProvider2D;
var
  repeatX,repeatY: boolean;
  origin: TPointF;
begin
  repetition := lowercase(trim(repetition));
  repeatX := true;
  repeatY := true;
  if repetition = 'repeat-x' then repeatY := false else
  if repetition = 'repeat-y' then repeatX := false else
  if repetition = 'no-repeat' then
  begin
    repeatX := false;
    repeatY := false;
  end;
  origin := ApplyTransform(PointF(0,0)) + PointF(0.5,0.5);
  result := TBGRACanvasPattern2D.Create(image,repeatX,repeatY,
     origin, origin+PointF(currentState.matrix[1,1],currentState.matrix[2,1])*image.Width,
     origin+PointF(currentState.matrix[1,2],currentState.matrix[2,2])*image.Height);
end;

function TBGRACanvas2D.createPattern(texture: IBGRAScanner
  ): IBGRACanvasTextureProvider2D;
var
  tempTransform: TAffineMatrix;
begin
  tempTransform := AffineMatrixTranslation(FCanvasOffset.X+0.5,FCanvasOffset.Y+0.5)*currentState.matrix;
  result := TBGRACanvasPattern2D.Create(texture,tempTransform);
end;

procedure TBGRACanvas2D.fillRect(x, y, w, h: single);
begin
  if (w=0) or (h=0) then exit;
  FillPoly(ApplyTransform([PointF(x,y),PointF(x+w,y),PointF(x+w,y+h),PointF(x,y+h)]));
end;

procedure TBGRACanvas2D.strokeRect(x, y, w, h: single);
begin
  if (w=0) or (h=0) then exit;
  StrokePoly(ApplyTransform([PointF(x,y),PointF(x+w,y),PointF(x+w,y+h),PointF(x,y+h),PointF(x,y)]));
end;

procedure TBGRACanvas2D.clearRect(x, y, w, h: single);
begin
  if (w=0) or (h=0) then exit;
  ClearPoly(ApplyTransform([PointF(x,y),PointF(x+w,y),PointF(x+w,y+h),PointF(x,y+h)]));
end;

procedure TBGRACanvas2D.addPath(APath: IBGRAPath);
begin
  if (FPathPointCount <> 0) and not isEmptyPointF(FPathPoints[FPathPointCount-1]) then
  begin
    AddPoint(EmptyPointF);
    FLastCoord := EmptyPointF;
    FStartCoord := EmptyPointF;
  end;
  APath.copyTo(self);
end;

procedure TBGRACanvas2D.addPath(ASvgPath: string);
var p: TBGRAPath;
begin
  p := TBGRAPath.Create(ASvgPath);
  addPath(p);
  p.Free;
end;

procedure TBGRACanvas2D.path(APath: IBGRAPath);
begin
  beginPath;
  addPath(APath);
end;

procedure TBGRACanvas2D.path(ASvgPath: string);
begin
  beginPath;
  addPath(ASvgPath);
end;

procedure TBGRACanvas2D.beginPath;
begin
  FPathPointCount := 0;
  FLastCoord := EmptyPointF;
  FStartCoord := EmptyPointF;
end;

procedure TBGRACanvas2D.closePath;
var i: integer;
begin
  if FPathPointCount > 0 then
  begin
    i := FPathPointCount-1;
    while (i > 0) and not isEmptyPointF(FPathPoints[i-1]) do dec(i);
    AddPoint(FPathPoints[i]);
    FLastCoord := FStartCoord;
  end;
end;

procedure TBGRACanvas2D.toSpline(closed: boolean; style: TSplineStyle);
var i,j: integer;
  pts, splinePts: array of TPointF;
  nb: integer;
begin
  if FPathPointCount > 0 then
  begin
    i := FPathPointCount-1;
    while (i > 0) and not isEmptyPointF(FPathPoints[i-1]) do dec(i);
    nb := FPathPointCount - i;
    setlength(pts,nb);
    for j := 0 to nb-1 do
      pts[j] := FPathPoints[i+j];
    if closed then
      splinePts := BGRAPath.ComputeClosedSpline(pts,style)
    else
      splinePts := BGRAPath.ComputeOpenedSpline(pts,style);
    dec(FPathPointCount,nb);
    AddPoints(splinePts);
  end;
end;

procedure TBGRACanvas2D.moveTo(x, y: single);
begin
  moveTo(PointF(x,y));
end;

procedure TBGRACanvas2D.lineTo(x, y: single);
begin
  lineTo(PointF(x,y));
end;

procedure TBGRACanvas2D.moveTo(const pt: TPointF);
begin
  if (FPathPointCount <> 0) and not isEmptyPointF(FPathPoints[FPathPointCount-1]) then
    AddPoint(EmptyPointF);
  AddPoint(ApplyTransform(pt));
  FStartCoord := pt;
  FLastCoord := pt;
end;

procedure TBGRACanvas2D.lineTo(const pt: TPointF);
begin
  AddPoint(ApplyTransform(pt));
  FLastCoord := pt;
end;

procedure TBGRACanvas2D.polylineTo(const pts: array of TPointF);
begin
  if length(pts)> 0 then
  begin
    AddPoints(ApplyTransform(pts));
    FLastCoord := pts[high(pts)];
  end;
end;

procedure TBGRACanvas2D.quadraticCurveTo(cpx, cpy, x, y: single);
var
  curve : TQuadraticBezierCurve;
  pts : array of TPointF;
begin
  curve := BezierCurve(ApplyTransform(GetPenPos(cpx,cpy)),ApplyTransform(PointF(cpx,cpy)),ApplyTransform(PointF(x,y)));
  pts := BGRAPath.ComputeBezierCurve(curve);
  AddPoints(pts);
  FLastCoord := PointF(x,y);
end;

procedure TBGRACanvas2D.quadraticCurveTo(const cp, pt: TPointF);
begin
  quadraticCurveTo(cp.x,cp.y,pt.x,pt.y);
end;

procedure TBGRACanvas2D.bezierCurveTo(cp1x, cp1y, cp2x, cp2y, x, y: single);
var
  curve : TCubicBezierCurve;
  pts : array of TPointF;
begin
  curve := BezierCurve(ApplyTransform(GetPenPos(cp1x,cp1y)),ApplyTransform(PointF(cp1x,cp1y)),
    ApplyTransform(PointF(cp2x,cp2y)),ApplyTransform(PointF(x,y)));
  pts := BGRAPath.ComputeBezierCurve(curve);
  AddPoints(pts);
  FLastCoord := PointF(x,y);
end;

procedure TBGRACanvas2D.bezierCurveTo(const cp1, cp2, pt: TPointF);
begin
  bezierCurveTo(cp1.x,cp1.y,cp2.x,cp2.y,pt.x,pt.y);
end;

procedure TBGRACanvas2D.rect(x, y, w, h: single);
begin
  MoveTo(x,y);
  LineTo(x+w,y);
  LineTo(x+w,y+h);
  LineTo(x,y+h);
  closePath;
end;

procedure TBGRACanvas2D.roundRect(x, y, w, h, radius: single);
begin
  if radius <= 0 then
  begin
    rect(x,y,w,h);
    exit;
  end;
  if (w <= 0) or (h <= 0) then exit;
  if radius*2 > w then radius := w/2;
  if radius*2 > h then radius := h/2;
  moveTo(x+radius,y);
  arcTo(PointF(x+w,y),PointF(x+w,y+h), radius);
  arcTo(PointF(x+w,y+h),PointF(x,y+h), radius);
  arcTo(PointF(x,y+h),PointF(x,y), radius);
  arcTo(PointF(x,y),PointF(x+w,y), radius);
  closePath;
end;

procedure TBGRACanvas2D.roundRect(x, y, w, h, rx, ry: single);
begin
  if (w <= 0) or (h <= 0) then exit;
  if rx < 0 then rx := 0;
  if ry < 0 then ry := 0;
  if (rx = 0) and (ry = 0) then
  begin
    rect(x,y,w,h);
    exit;
  end;
  if rx*2 > w then rx := w/2;
  if ry*2 > h then ry := h/2;
  moveTo(x+rx,y);
  lineTo(x+w-rx,y);
  arcTo(rx,ry,0,false,false,x+w,y+ry);
  lineTo(x+w,y+h-ry);
  arcTo(rx,ry,0,false,false,x+w-rx,y+h);
  lineTo(x+rx,y+h);
  arcTo(rx,ry,0,false,false,x,y+h-ry);
  lineTo(x,y+ry);
  arcTo(rx,ry,0,false,false,x+rx,y);
  closePath;
end;

procedure TBGRACanvas2D.openedSpline(const pts: array of TPointF;
  style: TSplineStyle);
var transf: array of TPointF;
begin
  if length(pts)=0 then exit;
  transf := ApplyTransform(pts);
  transf := BGRAPath.ComputeOpenedSpline(transf,style);
  AddPoints(transf);
  FLastCoord := pts[high(pts)];
end;

procedure TBGRACanvas2D.closedSpline(const pts: array of TPointF;
  style: TSplineStyle);
var transf: array of TPointF;
begin
  if length(pts)=0 then exit;
  transf := ApplyTransform(pts);
  transf := BGRAPath.ComputeClosedSpline(slice(transf, length(transf)-1),style);
  AddPoints(transf);
  FLastCoord := pts[high(pts)];
end;

procedure TBGRACanvas2D.spline(const pts: array of TPointF; style: TSplineStyle);
var transf: array of TPointF;
begin
  if length(pts)=0 then exit;
  transf := ApplyTransform(pts);
  if (pts[0] = pts[high(pts)]) and (length(pts) > 1) then
    transf := BGRAPath.ComputeClosedSpline(slice(transf, length(transf)-1),style)
  else
    transf := BGRAPath.ComputeOpenedSpline(transf,style);
  AddPoints(transf);
  FLastCoord := pts[high(pts)];
end;

procedure TBGRACanvas2D.splineTo(const pts: array of TPointF;
  style: TSplineStyle);
var transf: array of TPointF;
  i: Integer;
begin
  if length(pts) = 0 then exit;
  transf := ApplyTransform(pts);
  if FPathPointCount <> 0 then
  begin
    setlength(transf,length(transf)+1);
    for i := high(transf) downto 1 do
      transf[i]:= transf[i-1];
    transf[0] := ApplyTransform(GetPenPos(pts[0].x,pts[0].y));
  end;
  transf := BGRAPath.ComputeOpenedSpline(transf,style);
  AddPoints(transf);
  FLastCoord := pts[high(pts)];
end;

procedure TBGRACanvas2D.arc(x, y, radius, startAngleRadCW, endAngleRadCW: single;
  anticlockwise: boolean);
var pts: array of TPointF;
  temp: single;
  pt: TPointF;
  rx,ry: single;
  len1,len2: single;
  unitAffine: TAffineMatrix;
  v1orig,v2orig,v1ortho,v2ortho: TPointF;
  startRadCCW,endRadCCW: single;
begin
  v1orig := PointF(currentState.matrix[1,1],currentState.matrix[2,1]);
  v2orig := PointF(currentState.matrix[1,2],currentState.matrix[2,2]);
  len1 := VectLen(v1orig);
  len2 := VectLen(v2orig);
  rx := len1*radius;
  ry := len2*radius;
  if len1 <> 0 then v1ortho := v1orig * (1/len1) else v1ortho := v1orig;
  if len2 <> 0 then v2ortho := v2orig * (1/len2) else v2ortho := v2orig;
  pt := currentState.matrix* PointF(x,y);
  unitAffine := AffineMatrix(v1ortho.x, v2ortho.x, pt.x,
                             v1ortho.y, v2ortho.y, pt.y);
  startRadCCW := -startAngleRadCW;
  endRadCCW := -endAngleRadCW;
  if not anticlockwise then
  begin
    temp := startRadCCW;
    startRadCCW := endRadCCW;
    endRadCCW:= temp;
    pts := BGRAPath.ComputeArcRad(0,0,rx,ry,startRadCCW,endRadCCW);
    pts := ApplyTransform(pts,unitAffine);
    AddPointsRev(pts);
  end else
  begin
    pts := BGRAPath.ComputeArcRad(0,0,rx,ry,startRadCCW,endRadCCW);
    pts := ApplyTransform(pts,unitAffine);
    AddPoints(pts);
  end;
  FLastCoord := ArcEndPoint(ArcDef(x,y,radius,radius,0,startAngleRadCW,endAngleRadCW,anticlockwise));
end;

procedure TBGRACanvas2D.arc(x, y, radius, startAngleRadCW, endAngleRadCW: single);
begin
  arc(x,y,radius,startAngleRadCW,endAngleRadCW,false);
end;

procedure TBGRACanvas2D.arc(cx, cy, rx, ry, xAngleRadCW, startAngleRadCW, endAngleRadCW: single;
  anticlockwise: boolean);
begin
  arc(ArcDef(cx,cy,rx,ry,xAngleRadCW,startAngleRadCW,endAngleRadCW,anticlockwise))
end;

procedure TBGRACanvas2D.arc(cx, cy, rx, ry, xAngleRadCW, startAngleRadCW, endAngleRadCW: single);
begin
  arc(ArcDef(cx,cy,rx,ry,xAngleRadCW,startAngleRadCW,endAngleRadCW,false))
end;

procedure TBGRACanvas2D.arc(const arcDef: TArcDef);
var previousMatrix: TAffineMatrix;
begin
  if (arcDef.radius.x = 0) and (arcDef.radius.y = 0) then
    lineTo(arcDef.center) else
  begin
    previousMatrix := currentState.matrix;
    translate(arcDef.center.x,arcDef.center.y);
    rotate(arcDef.xAngleRadCW);
    scale(arcDef.radius.x,arcDef.radius.y);
    arc(0,0,1,arcDef.startAngleRadCW,arcDef.endAngleRadCW,arcDef.anticlockwise);
    currentState.matrix := previousMatrix;
    FLastCoord := ArcEndPoint(arcDef);
  end;
end;

procedure TBGRACanvas2D.arcTo(x1, y1, x2, y2, radius: single);
var p0: TPointF;
begin
  p0 := GetPenPos(x1,y1);
  arc(Html5ArcTo(p0,PointF(x1,y1),PointF(x2,y2),radius));
end;

procedure TBGRACanvas2D.arcTo(p1, p2: TPointF; radius: single);
begin
  arcTo(p1.x,p1.y,p2.x,p2.y,radius);
end;

procedure TBGRACanvas2D.arcTo(rx, ry, xAngleRadCW: single; largeArc,
  anticlockwise: boolean; x, y: single);
begin
  arc(SvgArcTo(GetPenPos(x,y), rx,ry, xAngleRadCW, largeArc, anticlockwise, PointF(x,y)));
  FLastCoord := PointF(x,y);
end;

procedure TBGRACanvas2D.circle(x, y, r: single);
begin
  arc(x,y,r,0,0);
end;

procedure TBGRACanvas2D.ellipse(x, y, rx, ry: single);
begin
  arc(x,y,rx,ry,0,0,0);
end;

procedure TBGRACanvas2D.text(AText: string; x, y: single);
var renderer : TBGRACustomFontRenderer;
  previousMatrix: TAffineMatrix;
begin
  renderer := fontRenderer;
  if renderer.FontEmHeight <= 0 then exit;
  previousMatrix := currentState.matrix;

  scale(currentState.fontEmHeight/renderer.FontEmHeight);
  if (currentState.textBaseline <> 'top') and
    (currentState.textBaseline <> 'hanging') then
  with renderer.GetFontPixelMetric do
  begin
    if currentState.textBaseline = 'bottom' then
       translate(0,-Lineheight)
    else if currentState.textBaseline = 'middle' then
       translate(0,-Lineheight/2)
    else if currentState.textBaseline = 'alphabetic' then
       translate(0,-baseline);
  end;

  if renderer <> nil then
    renderer.CopyTextPathTo(self, x,y, AText, taLeftJustify);

  currentState.matrix := previousMatrix;
  FLastCoord := EmptyPointF;
  FStartCoord := EmptyPointF;
end;

procedure TBGRACanvas2D.fillText(AText: string; x, y: single);
begin
  beginPath;
  text(AText,x,y);
  fill;
  beginPath;
end;

procedure TBGRACanvas2D.strokeText(AText: string; x, y: single);
begin
  beginPath;
  text(AText,x,y);
  stroke;
  beginPath;
end;

function TBGRACanvas2D.measureText(AText: string): TCanvas2dTextSize;
var renderer: TBGRACustomFontRenderer;
begin
  renderer := fontRenderer;
  if renderer <> nil then
  begin
    with renderer.TextSize(AText) do
    begin
      result.width := cx;
      result.height:= cy;
    end;
  end
  else
  begin
    result.width := 0;
    result.height := 0;
  end;
end;

procedure TBGRACanvas2D.fill;
begin
  if FPathPointCount = 0 then exit;
  FillPoly(slice(FPathPoints,FPathPointCount));
end;

procedure TBGRACanvas2D.stroke;
begin
  if FPathPointCount = 0 then exit;
  StrokePoly(slice(FPathPoints,FPathPointCount));
end;

procedure TBGRACanvas2D.fillOverStroke;
begin
  if FPathPointCount = 0 then exit;
  FillStrokePoly(slice(FPathPoints,FPathPointCount),true);
end;

procedure TBGRACanvas2D.strokeOverFill;
begin
  if FPathPointCount = 0 then exit;
  FillStrokePoly(slice(FPathPoints,FPathPointCount),false);
end;

procedure TBGRACanvas2D.clearPath;
begin
  if FPathPointCount = 0 then exit;
  ClearPoly(slice(FPathPoints,FPathPointCount));
end;

procedure TBGRACanvas2D.clip;
var
  tempBmp: TBGRACustomBitmap;
begin
  if FPathPointCount = 0 then
  begin
    currentState.clipMaskReadWrite.Fill(BGRABlack);
    exit;
  end;
  if currentState.clipMaskReadOnly = nil then
    currentState.SetClipMask(surface.NewBitmap(width,height,BGRAWhite),True);
  tempBmp := surface.NewBitmap(width,height,BGRABlack);
  if antialiasing then
    tempBmp.FillPolyAntialias(slice(FPathPoints,FPathPointCount),BGRAWhite)
  else
    tempBmp.FillPoly(slice(FPathPoints,FPathPointCount),BGRAWhite,dmSet);
  currentState.clipMaskReadWrite.BlendImage(0,0,tempBmp,boDarken);
  tempBmp.Free;
end;

procedure TBGRACanvas2D.unclip;
begin
  if FPathPointCount = 0 then exit;
  if currentState.clipMaskReadOnly = nil then exit;
  if antialiasing then
    currentState.clipMaskReadWrite.FillPolyAntialias(slice(FPathPoints,FPathPointCount),BGRAWhite)
  else
    currentState.clipMaskReadWrite.FillPoly(slice(FPathPoints,FPathPointCount),BGRAWhite,dmSet);
  if currentState.clipMaskReadOnly.Equals(BGRAWhite) then
    currentState.SetClipMask(nil,true);
end;

function TBGRACanvas2D.isPointInPath(x, y: single): boolean;
begin
  result := isPointInPath(PointF(x,y));
end;

function TBGRACanvas2D.isPointInPath(pt: TPointF): boolean;
begin
  if FPathPointCount <= 2 then
    result := false
  else
  begin
    setlength(FPathPoints,FPathPointCount);
    result := IsPointInPolygon(FPathPoints,pt+FCanvasOffset,True);
  end;
end;

procedure TBGRACanvas2D.drawImage(image: TBGRACustomBitmap; dx, dy: single);
begin
  Surface.PutImageAffine(ApplyTransform(PointF(dx,dy))+PointF(0.5,0.5),
    ApplyTransform(PointF(dx+image.width,dy))+PointF(0.5,0.5),
    ApplyTransform(PointF(dx,dy+image.height))+PointF(0.5,0.5), image, currentState.globalAlpha);
end;

procedure TBGRACanvas2D.drawImage(image: TBGRACustomBitmap; dx, dy, dw, dh: single);
begin
  Surface.PutImageAffine(ApplyTransform(PointF(dx,dy))+PointF(0.5,0.5),
    ApplyTransform(PointF(dx+dw,dy))+PointF(0.5,0.5),
    ApplyTransform(PointF(dx,dy+dh))+PointF(0.5,0.5), image, currentState.globalAlpha);
end;

end.

