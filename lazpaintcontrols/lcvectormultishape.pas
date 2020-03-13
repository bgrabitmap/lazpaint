unit LCVectorMultishape;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCVectorRectShapes, LCVectorOriginal, BGRALayerOriginal,
  BGRATransform, BGRABitmap, BGRABitmapTypes;

type

  { TVectorMultishape }

  TVectorMultishape = class(TCustomRectShape)
  protected
    FShapes: TVectorShapes;
    function GetCornerPositition: single; override;
  public
    constructor Create(AContainer: TVectorOriginal); override;
    procedure Clear;
    procedure AddShape(AShape: TVectorShape);
    procedure RemoveShape(AShape: TVectorShape);
    function MultiFields: TVectorShapeFields;
    procedure TransformFrame(const AMatrix: TAffineMatrix); override;
    procedure TransformFill(const AMatrix: TAffineMatrix; ABackOnly: boolean); override;
    function AllowShearTransform: boolean; override;
    procedure LoadFromStorage(AStorage: TBGRACustomOriginalStorage); override;
    procedure SaveToStorage(AStorage: TBGRACustomOriginalStorage); override;
    procedure Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix; ADraft: boolean); overload; override;
    procedure Render(ADest: TBGRABitmap; ARenderOffset: TPoint; AMatrix: TAffineMatrix; ADraft: boolean); overload; override;
    function GetRenderBounds({%H-}ADestRect: TRect; AMatrix: TAffineMatrix; {%H-}AOptions: TRenderBoundsOptions = []): TRectF; override;
    procedure ConfigureCustomEditor(AEditor: TBGRAOriginalEditor); override;
    function PointInShape(APoint: TPointF): boolean; overload; override;
    function PointInShape(APoint: TPointF; ARadius: single): boolean; overload; override;
    function PointInBack(APoint: TPointF): boolean; overload; override;
    function PointInPen(APoint: TPointF): boolean; overload; override;
    function GetIsSlow(const AMatrix: TAffineMatrix): boolean; override;
    class function StorageClassName: RawByteString; override;
    destructor Destroy; override;
  end;

implementation

{ TVectorMultishape }

procedure TVectorMultishape.TransformFill(const AMatrix: TAffineMatrix;
  ABackOnly: boolean);
var
  i: Integer;
begin
  BeginUpdate;
  for i := 0 to FShapes.Count-1 do
    FShapes[i].TransformFill(AMatrix, ABackOnly);
  EndUpdate;
end;

function TVectorMultishape.GetCornerPositition: single;
begin
  result := 1;
end;

function TVectorMultishape.AllowShearTransform: boolean;
var
  i: Integer;
begin
  for i := 0 to FShapes.Count-1 do
    if not FShapes[i].AllowShearTransform then exit(false);
  result := true;
end;

procedure TVectorMultishape.LoadFromStorage(AStorage: TBGRACustomOriginalStorage);
begin
  raise exception.Create('Cannot be deserialized');
end;

procedure TVectorMultishape.SaveToStorage(AStorage: TBGRACustomOriginalStorage);
begin
  raise exception.Create('Cannot be serialized');
end;

procedure TVectorMultishape.Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix;
  ADraft: boolean);
var
  i: Integer;
begin
  for i := 0 to FShapes.Count-1 do
    FShapes[i].Render(ADest, AMatrix, ADraft);
end;

procedure TVectorMultishape.Render(ADest: TBGRABitmap; ARenderOffset: TPoint;
  AMatrix: TAffineMatrix; ADraft: boolean);
var
  i: Integer;
begin
  for i := 0 to FShapes.Count-1 do
    FShapes[i].Render(ADest, ARenderOffset, AMatrix, ADraft);
end;

function TVectorMultishape.GetRenderBounds(ADestRect: TRect;
  AMatrix: TAffineMatrix; AOptions: TRenderBoundsOptions): TRectF;
var
  i: Integer;
begin
  result := EmptyRectF;
  for i := 0 to FShapes.Count-1 do
    result := result.Union(FShapes[i].GetRenderBounds(ADestRect, AMatrix, AOptions), true);
end;

procedure TVectorMultishape.ConfigureCustomEditor(AEditor: TBGRAOriginalEditor);
var
  i: Integer;
  ab: TAffineBox;
begin
  for i := 0 to FShapes.Count-1 do
  begin
    ab := FShapes[i].SuggestGradientBox(AffineMatrixIdentity);
    AEditor.AddPolyline(ab.AsPolygon, true, opsDash);
  end;
  inherited ConfigureCustomEditor(AEditor);
end;

function TVectorMultishape.PointInShape(APoint: TPointF): boolean;
var
  i: LongInt;
begin
  for i := FShapes.Count-1 downto 0 do
    if FShapes[i].PointInShape(APoint) then exit(true);
  result := false;
end;

function TVectorMultishape.PointInShape(APoint: TPointF; ARadius: single): boolean;
var
  i: LongInt;
begin
  for i := FShapes.Count-1 downto 0 do
    if FShapes[i].PointInShape(APoint, ARadius) then exit(true);
  result := false;
end;

function TVectorMultishape.PointInBack(APoint: TPointF): boolean;
var
  i: LongInt;
begin
  for i := FShapes.Count-1 downto 0 do
    if FShapes[i].PointInBack(APoint) then exit(true);
  result := false;
end;

function TVectorMultishape.PointInPen(APoint: TPointF): boolean;
var
  i: LongInt;
begin
  for i := FShapes.Count-1 downto 0 do
    if FShapes[i].PointInPen(APoint) then exit(true);
  result := false;
end;

function TVectorMultishape.GetIsSlow(const AMatrix: TAffineMatrix): boolean;
var
  i: LongInt;
begin
  if FShapes.Count >= 5 then exit(true);
  for i := 0 to FShapes.Count-1 do
    if FShapes[i].GetIsSlow(AMatrix) then exit(true);
  result := false;
end;

class function TVectorMultishape.StorageClassName: RawByteString;
begin
  result := 'multishape';
end;

procedure TVectorMultishape.Clear;
begin
  FShapes.Clear;
end;

procedure TVectorMultishape.AddShape(AShape: TVectorShape);
begin
  FShapes.Add(AShape);
end;

procedure TVectorMultishape.RemoveShape(AShape: TVectorShape);
begin
  FShapes.Remove(AShape);
end;

function TVectorMultishape.MultiFields: TVectorShapeFields;
var
  i: Integer;
begin
  result := [];
  for i := 0 to FShapes.Count-1 do
    result += FShapes[i].Fields;
end;

constructor TVectorMultishape.Create(AContainer: TVectorOriginal);
begin
  inherited Create(AContainer);
  FShapes := TVectorShapes.Create;
end;

procedure TVectorMultishape.TransformFrame(const AMatrix: TAffineMatrix);
var
  i: Integer;
begin
  BeginUpdate;
  for i := 0 to FShapes.Count-1 do
    FShapes[i].TransformFrame(AMatrix);
  EndUpdate;
end;

destructor TVectorMultishape.Destroy;
begin
  Clear;
  FShapes.Free;
  inherited Destroy;
end;

end.

