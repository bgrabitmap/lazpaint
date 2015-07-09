unit utest32;

{$mode objfpc}

interface

uses
  Classes, SysUtils, utest, Graphics, BGRABitmap, BGRABitmapTypes;

type

  { TTest32 }

  TTest32 = class(TTest)
    virtualScreen,background: TBGRABitmap;
    position,speed,direction: single;

    constructor Create;
    destructor Destroy; override;
    procedure DrawRings(ABitmap: TBGRABitmap; APath: IBGRAPath; ACount: integer; ARadius: single);
    procedure OnPaint(Canvas: TCanvas; Left,Top,Width, Height: Integer); override;
    procedure OnTimer(Width, Height: Integer; ElapsedSec: Double);
      override;
  end;

implementation

uses BGRAPath, BGRAVectorize, BGRAGradientScanner;

{ TTest32 }

constructor TTest32.Create;
begin
  inherited Create;
  Name := 'Curved text';
  virtualScreen := nil;
  background := nil;
  position := 0;
  speed := 0;
  direction := 1;
end;

destructor TTest32.Destroy;
begin
  virtualscreen.free;
  background.free;
  inherited Destroy;
end;

procedure TTest32.DrawRings(ABitmap: TBGRABitmap;
  APath: IBGRAPath; ACount: integer; ARadius: single);
var
  step: single;
  cursor: TBGRACustomPathCursor;
begin
  cursor := APath.getCursor;
  step := cursor.PathLength/ACount;
  cursor.Position:= 0;
  repeat
    with cursor.CurrentCoordinate do
      ABitmap.FillEllipseAntialias(x,y, ARadius,ARadius, CSSDarkSlateBlue);
  until cursor.MoveForward(step) < 0.5*step;
  cursor.Free;
end;

procedure TTest32.OnPaint(Canvas: TCanvas; Left,Top,Width, Height: Integer);
var path1: TBGRAPath;
    cursor1: TBGRAPathCursor;
    size: single;

  procedure PrepareBackground;
  var
    path2: TBGRAPath;
    gradient: TBGRAGradientScanner;
    fontRenderer: TBGRAVectorizedFontRenderer;
  begin
    background := TBGRABitmap.Create(Width,Height,CSSLightSteelBlue);
    fontRenderer := TBGRAVectorizedFontRenderer.Create;
    background.FontRenderer := fontRenderer;
    background.FontHeight := round(size*0.09);
    background.FontQuality := fqFineAntialiasing;

    DrawRings(background, path1, 20, size/150);

    path2 := TBGRAPath.Create;
    path2.arc(width/2,height/2,size*0.4, Pi, 0, true);

    fontRenderer.OutlineColor := BGRABlack;
    fontRenderer.OutlineWidth := size/600;
    fontRenderer.OutlineVisible := true;
    background.FontVerticalAnchor := fvaBaseline;

    gradient := TBGRAGradientScanner.Create(CSSYellow, CSSOrange, gtLinear, PointF(0,height/2), PointF(0,height/2+size*0.5));
    background.TextOutCurved(path2, ' Left ...', gradient, taLeftJustify, 0);
    background.TextOutCurved(path2, 'Middle', gradient, taCenter, 0);
    background.TextOutCurved(path2, '... Right ', gradient, taRightJustify, 0);
    gradient.free;

    fontRenderer.OutlineVisible := false;
    background.FontVerticalAnchor := fvaTop;

    path2.Free;
  end;

var
  fontRenderer: TBGRAVectorizedFontRenderer;

begin
  if (virtualscreen <> nil) and ((virtualscreen.width <> width) or (virtualscreen.Height <> height)) then
  begin
    FreeAndNil(virtualScreen);
    FreeAndNil(background);
  end;

  if width < height then size := width else size := height;
  path1 := TBGRAPath.Create;
  path1.arc(width/2,height/2,size*0.4, Pi, 0);

  if virtualscreen = nil then
  begin
    PrepareBackground;

    virtualscreen := TBGRABitmap.Create(Width,Height);
    fontRenderer := TBGRAVectorizedFontRenderer.Create;
    virtualScreen.FontRenderer := fontRenderer;
  end else
    fontRenderer := virtualScreen.FontRenderer as TBGRAVectorizedFontRenderer;

  virtualScreen.PutImage(0,0, background, dmSet);
  virtualScreen.FontHeight := background.FontHeight;
  virtualScreen.FontQuality := background.FontQuality;

  cursor1 := path1.CreateCursor;
  speed := size/4/cursor1.PathLength;
  if position < 0 then
  begin
    position := 0;
    direction := 1;
  end;
  cursor1.Position := position*cursor1.PathLength;
  virtualScreen.TextOutCurved(cursor1, 'Curved text', BGRABlack, taLeftJustify, virtualScreen.FontFullHeight/8);
  if cursor1.Position = cursor1.PathLength then direction := -1;
  cursor1.Free;

  path1.Free;

  //draw virtualscreen opaque on canvas
  virtualscreen.Draw(Canvas,Left,Top,True);
end;

procedure TTest32.OnTimer(Width, Height: Integer;
  ElapsedSec: Double);
begin
  position += ElapsedSec*speed*direction;
end;

end.

