unit UToolBrush;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UToolBasic, BGRABitmapTypes, BGRABitmap, UTool,
  UBrushType;

type

  { TToolGenericBrush }

  TToolGenericBrush = class(TToolPen)
  private
    function GetBrushInfo: TLazPaintBrush;
  protected
    brushOrigin: TPointF;
    originDrawn: boolean;
    defaultBrush: TLazPaintBrush;
    function DrawBrushAt(toolDest: TBGRABitmap; x, y: single): TRect; virtual; abstract;
    procedure PrepareBrush(rightBtn: boolean); virtual; abstract;
    procedure ReleaseBrush; virtual; abstract;
    function StartDrawing(toolDest: TBGRABitmap; ptF: TPointF; rightBtn: boolean): TRect; override;
    function ContinueDrawing(toolDest: TBGRABitmap; {%H-}originF, destF: TPointF): TRect; override;
    function GetBrushAlpha(AAlpha: byte): byte;
  public
    constructor Create(AManager: TToolManager); override;
    function ToolUp: TRect; override;
    function SubPixelAccuracy: boolean; virtual;
    destructor Destroy; override;
    property BrushInfo: TLazPaintBrush read GetBrushInfo;
  end;

  { TToolBrush }

  TToolBrush = class(TToolGenericBrush)
  protected
    coloredBrushImage: TBGRABitmap;
    function DrawBrushAt(toolDest: TBGRABitmap; x, y: single): TRect; override;
    procedure PrepareBrush({%H-}rightBtn: boolean); override;
    procedure ReleaseBrush; override;
  public
    destructor Destroy; override;
  end;

  { TToolClone }

  TToolClone = class(TToolGenericBrush)
  protected
    definingSource: boolean;
    class var sourceLayerId: integer;
    class var sourcePosition: TPoint;
    class var sourcePositionRelative: boolean;
    function DrawBrushAt(toolDest: TBGRABitmap; x, y: single): TRect; override;
    procedure PrepareBrush(rightBtn: boolean); override;
    procedure ReleaseBrush; override;
    function DoToolMove(toolDest: TBGRABitmap; pt: TPoint; ptF: TPointF): TRect; override;
  public
    function SubPixelAccuracy: boolean; override;
    constructor Create(AManager: TToolManager); override;
    destructor Destroy; override;
    function Render(VirtualScreen: TBGRABitmap; VirtualScreenWidth, VirtualScreenHeight: integer;
      BitmapToVirtualScreen: TBitmapToVirtualScreenFunction): TRect; override;
  end;

implementation

uses Math, UGraph, UResourceStrings, Graphics, LazPaintType;

{ TToolClone }

function TToolClone.DrawBrushAt(toolDest: TBGRABitmap; x, y: single): TRect;
var source: TBGRABitmap;
begin
  if definingSource then
  begin
    sourcePosition := Point(round(x),round(y));
    sourceLayerId := Manager.Image.LayerId[Manager.Image.currentImageLayerIndex];
    sourcePositionRelative:= false;
    result := OnlyRenderChange;
  end else
  begin
    source := Manager.Image.LayerBitmapById[sourceLayerId];
    if source = nil then
    begin
      result := EmptyRect;
      exit;
    end;
    if not SubPixelAccuracy then
    begin
      x := round(x);
      y := round(y);
    end;
    if not sourcePositionRelative then
    begin
      sourcePosition.x -= round(x);
      sourcePosition.y -= round(y);
      sourcePositionRelative := true;
    end;
    with BrushInfo.BrushImage do
    begin
      x -= (Width-1)/2;
      y -= (Height-1)/2;
      result := rect(floor(x-0.5),floor(y-0.5),ceil(x+0.5)+Width,ceil(y+0.5)+Height);
    end;
    toolDest.ClipRect := result;
    source.ScanOffset := Point(sourcePosition.x, sourcePosition.y);
    toolDest.FillMask(round(x),round(y),BrushInfo.BrushImage,source,dmDrawWithTransparency);
    source.ScanOffset := Point(0,0);
    toolDest.NoClip;
  end;
end;

procedure TToolClone.PrepareBrush(rightBtn: boolean);
begin
  definingSource := rightBtn;
end;

procedure TToolClone.ReleaseBrush;
begin

end;

function TToolClone.DoToolMove(toolDest: TBGRABitmap; pt: TPoint; ptF: TPointF
  ): TRect;
begin
  Manager.ToolPopup(tpmRightClickForSource);
  Result:=inherited DoToolMove(toolDest, pt, ptF);
end;

function TToolClone.SubPixelAccuracy: boolean;
begin
  Result:=false;
end;

constructor TToolClone.Create(AManager: TToolManager);
begin
  inherited Create(AManager);
end;

destructor TToolClone.Destroy;
begin
  inherited Destroy;
end;

function TToolClone.Render(VirtualScreen: TBGRABitmap; VirtualScreenWidth,
  VirtualScreenHeight: integer;
  BitmapToVirtualScreen: TBitmapToVirtualScreenFunction): TRect;
var sourcePosF: TPointF;
begin
  Result:=inherited Render(VirtualScreen, VirtualScreenWidth,
    VirtualScreenHeight, BitmapToVirtualScreen);
  if not sourcePositionRelative and
    (Manager.Image.LayerBitmapById[sourceLayerId] <> nil) then
  begin
    sourcePosF := BitmapToVirtualScreen(PointF(sourcePosition.X mod Manager.Image.Width,
      sourcePosition.Y mod Manager.Image.Height));
    result := RectUnion(result,NicePoint(VirtualScreen, sourcePosF.X,sourcePosF.Y));
    if sourcePosF.Y > virtualScreenHeight/2 then
      result := RectUnion(result,NiceText(VirtualScreen, round(sourcePosF.X),round(sourcePosF.Y-6), VirtualScreenWidth,VirtualScreenHeight, rsSourcePosition, taCenter, tlBottom))
    else
      result := RectUnion(result,NiceText(VirtualScreen, round(sourcePosF.X),round(sourcePosF.Y+6), VirtualScreenWidth,VirtualScreenHeight, rsSourcePosition, taCenter, tlTop));
  end;
end;

{ TToolBrush }

function TToolBrush.DrawBrushAt(toolDest: TBGRABitmap; x, y: single): TRect;
begin
  if not Assigned(coloredBrushImage) then
  begin
    result := EmptyRect;
    exit;
  end;
  if not SubPixelAccuracy then
  begin
    x := round(x);
    y := round(y);
  end;
  x -= (coloredBrushImage.Width-1)/2;
  y -= (coloredBrushImage.Height-1)/2;
  result := rect(floor(x-0.5),floor(y-0.5),ceil(x+0.5)+coloredBrushImage.Width,ceil(y+0.5)+coloredBrushImage.Height);
  toolDest.ClipRect := result;
  if not SubPixelAccuracy then
    toolDest.PutImage(round(x),round(y),coloredBrushImage,dmDrawWithTransparency)
  else
    toolDest.PutImageSubpixel(x,y,coloredBrushImage);
  toolDest.NoClip;
end;

procedure TToolBrush.PrepareBrush(rightBtn: boolean);
begin
  FreeAndNil(coloredBrushImage);
  coloredBrushImage := BrushInfo.MakeColoredBrushImage(BGRA(penColor.red,penColor.green,penColor.blue,GetBrushAlpha(penColor.alpha)));
end;

procedure TToolBrush.ReleaseBrush;
begin
  FreeAndNil(coloredBrushImage);
end;

destructor TToolBrush.Destroy;
begin
  ReleaseBrush;
  inherited Destroy;
end;

{ TToolGenericBrush }

function TToolGenericBrush.GetBrushInfo: TLazPaintBrush;
begin
  result := manager.ToolBrushInfo;
  if result = nil then
  begin
    if defaultBrush = nil then
      defaultBrush := TLazPaintBrush.Create;
    result := defaultBrush;
  end;
  result.Size := manager.ToolPenWidth;
end;

function TToolGenericBrush.StartDrawing(toolDest: TBGRABitmap; ptF: TPointF;
  rightBtn: boolean): TRect;
begin
  if not SubPixelAccuracy then
    brushOrigin:= PointF(round(ptF.x),round(ptF.y))
  else brushOrigin := ptF;
  if rightBtn then penColor := Manager.ToolBackColor else penColor := Manager.ToolForeColor;
  originDrawn := false;
  PrepareBrush(rightBtn);
  result := ContinueDrawing(toolDest,brushOrigin,brushOrigin);
end;

function TToolGenericBrush.ContinueDrawing(toolDest: TBGRABitmap; originF,
  destF: TPointF): TRect;
var v: TPointF;
  count: integer;
  len, minLen: single;

begin
  result := EmptyRect;
  if not originDrawn then //and ((destF <> brushOrigin) or not Manager.ToolBrushOriented) then
  begin
    result := RectUnion(result, DrawBrushAt(toolDest, brushOrigin.x,brushOrigin.y));
    originDrawn:= true;
  end;
  if destF<>brushOrigin then
  begin
    v := destF-brushOrigin;
    if not SubPixelAccuracy then
      len := max(abs(v.x),abs(v.y))
    else
      len := sqrt(v*v);
    minLen := round(power(BrushInfo.Size/10,0.8));
    if minLen < 1 then minLen := 1;
    if minLen > 5 then minLen := 5;
    minLen *=Manager.ToolBrushSpacing;
    if len >= minLen then
    begin
      v := v*(1/len)*minLen;
      count := trunc(len/minLen);
      while count > 0 do
      begin
        brushOrigin += v;
        result := RectUnion(result, DrawBrushAt(toolDest, brushOrigin.x,brushOrigin.y));
        originDrawn:= true;
        dec(count);
      end;
    end;
  end;
  Action.NotifyChange(toolDest,result);
end;

function TToolGenericBrush.GetBrushAlpha(AAlpha: byte): byte;
var exponent: single;
begin
  exponent := (BrushInfo.Size-1)/10+1;
  if exponent > 2 then exponent := 2;
  result := round(Power(AAlpha/255,exponent)*255)
end;

constructor TToolGenericBrush.Create(AManager: TToolManager);
begin
  inherited Create(AManager);
end;

function TToolGenericBrush.ToolUp: TRect;
var penWasDrawing: boolean;
begin
  penWasDrawing:= penDrawing;
  Result:=inherited ToolUp;
  if not penDrawing and penWasDrawing then ReleaseBrush;
end;

function TToolGenericBrush.SubPixelAccuracy: boolean;
begin
  result := BrushInfo.Size < 10;
end;

destructor TToolGenericBrush.Destroy;
begin
  FreeAndNil(defaultBrush);
  inherited Destroy;
end;

initialization

  RegisterTool(ptBrush,TToolBrush);
  RegisterTool(ptClone,TToolClone);

  TToolClone.sourceLayerId := -1;

end.

