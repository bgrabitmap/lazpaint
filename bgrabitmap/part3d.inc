type
  { TBGRAPart3D }

  TBGRAPart3D = class(TInterfacedObject,IBGRAPart3D)
  private
    FVertices: array of IBGRAVertex3D;
    FVertexCount: integer;
    FNormals: array of IBGRANormal3D;
    FNormalCount: integer;
    FMatrix: TMatrix3D;
    FParts: array of IBGRAPart3D;
    FPartCount: integer;
    FContainer: IBGRAPart3D;
    FCoordPool: TBGRACoordPool3D;
    FNormalPool: TBGRANormalPool3D;
    FObject3D: TBGRAObject3D;
  public
    constructor Create(AObject3D: TBGRAObject3D; AContainer: IBGRAPart3D);
    destructor Destroy; override;
    procedure Clear(ARecursive: boolean);
    function Add(x,y,z: single): IBGRAVertex3D;
    function Add(pt: TPoint3D): IBGRAVertex3D;
    function Add(pt: TPoint3D; normal: TPoint3D): IBGRAVertex3D;
    function Add(pt: TPoint3D_128): IBGRAVertex3D;
    function Add(pt: TPoint3D_128; normal: TPoint3D_128): IBGRAVertex3D;
    function Add(const coords: array of single): arrayOfIBGRAVertex3D;
    function Add(const pts: array of TPoint3D): arrayOfIBGRAVertex3D;
    function Add(const pts: array of TPoint3D_128): arrayOfIBGRAVertex3D;
    procedure Add(const pts: array of IBGRAVertex3D);
    procedure Add(AVertex: IBGRAVertex3D);
    function AddNormal(x,y,z: single): IBGRANormal3D;
    function AddNormal(pt: TPoint3D): IBGRANormal3D;
    function AddNormal(pt: TPoint3D_128): IBGRANormal3D;
    procedure AddNormal(ANormal: IBGRANormal3D);
    procedure RemoveVertex(Index: integer);
    procedure RemoveNormal(Index: integer);
    function GetBoundingBox: TBox3D;
    function GetRadius: single;
    function GetMatrix: TMatrix3D;
    function GetPart(AIndex: Integer): IBGRAPart3D;
    function GetPartCount: integer;
    function GetVertex(AIndex: Integer): IBGRAVertex3D;
    function GetVertexCount: integer;
    function GetNormal(AIndex: Integer): IBGRANormal3D;
    function GetNormalCount: integer;
    function GetTotalVertexCount: integer;
    function GetTotalNormalCount: integer;
    function GetContainer: IBGRAPart3D;
    procedure SetVertex(AIndex: Integer; AValue: IBGRAVertex3D);
    procedure SetNormal(AIndex: Integer; AValue: IBGRANormal3D);
    procedure ResetTransform;
    procedure Translate(x,y,z: single; Before: boolean = true);
    procedure Translate(ofs: TPoint3D; Before: boolean = true);
    procedure Scale(size: single; Before: boolean = true);
    procedure Scale(x,y,z: single; Before: boolean = true);
    procedure Scale(size: TPoint3D; Before: boolean = true);
    procedure RotateXDeg(angle: single; Before: boolean = true);
    procedure RotateYDeg(angle: single; Before: boolean = true);
    procedure RotateZDeg(angle: single; Before: boolean = true);
    procedure RotateXRad(angle: single; Before: boolean = true);
    procedure RotateYRad(angle: single; Before: boolean = true);
    procedure RotateZRad(angle: single; Before: boolean = true);
    procedure SetMatrix(const AValue: TMatrix3D);
    procedure ComputeWithMatrix(const AMatrix: TMatrix3D; const AProjection: TProjection3D);
    function ComputeCoordinate(var ASceneCoord: TPoint3D_128; const AProjection: TProjection3D): TPointF;
    procedure NormalizeViewNormal;
    function CreatePart: IBGRAPart3D;
    procedure LookAt(ALookWhere,ATopDir: TPoint3D);
    procedure RemoveUnusedVertices;
    function IndexOf(AVertex: IBGRAVertex3D): integer;
    procedure ForEachVertex(ACallback: TVertex3DCallback);
  end;

{ TBGRAPart3D }

procedure TBGRAPart3D.LookAt(ALookWhere,ATopDir: TPoint3D);
var ZDir, XDir, YDir: TPoint3D_128;
    ViewPoint: TPoint3D_128;
    CurPart: IBGRAPart3D;
    ComposedMatrix: TMatrix3D;
begin
  YDir := -Point3D_128(ATopDir);
  if IsPoint3D_128_Zero(YDir) then exit;
  Normalize3D_128(YDir);

  ComposedMatrix := FMatrix;
  CurPart := self.FContainer;
  while CurPart <> nil do
  begin
    ComposedMatrix := CurPart.Matrix*ComposedMatrix;
    CurPart := CurPart.Container;
  end;
  ViewPoint := ComposedMatrix*Point3D_128_Zero;

  ZDir := Point3D_128(ALookWhere)-ViewPoint;
  if IsPoint3D_128_Zero(ZDir) then exit;
  Normalize3D_128(ZDir);

  VectProduct3D_128(YDir,ZDir,XDir);
  VectProduct3D_128(ZDir,XDir,YDir); //correct Y dir

  FMatrix := Matrix3D(XDir,YDir,ZDir,ViewPoint);
  ComposedMatrix := MatrixIdentity3D;
  CurPart := self.FContainer;
  while CurPart <> nil do
  begin
    ComposedMatrix := CurPart.Matrix*ComposedMatrix;
    CurPart := CurPart.Container;
  end;
  FMatrix := MatrixInverse3D(ComposedMatrix)*FMatrix;
end;

procedure TBGRAPart3D.RemoveUnusedVertices;
var
  i: Integer;
begin
  for i := FVertexCount-1 downto 0 do
    if FVertices[i].Usage <= 2 then RemoveVertex(i);
  for i := 0 to FPartCount-1 do
    FParts[i].RemoveUnusedVertices;
end;

function TBGRAPart3D.IndexOf(AVertex: IBGRAVertex3D): integer;
var i: integer;
begin
  for i := 0 to FVertexCount-1 do
    if FVertices[i] = AVertex then
    begin
      result := i;
      exit;
    end;
  result := -1;
end;

procedure TBGRAPart3D.ForEachVertex(ACallback: TVertex3DCallback);
var i: integer;
begin
  for i := 0 to FVertexCount-1 do
    ACallback(FVertices[i]);
end;

procedure TBGRAPart3D.Add(AVertex: IBGRAVertex3D);
begin
  if FVertexCount = length(FVertices) then
    setlength(FVertices, FVertexCount*2+3);
  FVertices[FVertexCount] := AVertex;
  inc(FVertexCount);
end;

function TBGRAPart3D.AddNormal(x, y, z: single): IBGRANormal3D;
begin
  if not Assigned(FNormalPool) then FNormalPool := TBGRANormalPool3D.Create(4);
  result := TBGRANormal3D.Create(FNormalPool,Point3D_128(x,y,z));
  AddNormal(result);
end;

function TBGRAPart3D.AddNormal(pt: TPoint3D): IBGRANormal3D;
begin
  if not Assigned(FNormalPool) then FNormalPool := TBGRANormalPool3D.Create(4);
  result := TBGRANormal3D.Create(FNormalPool,pt);
  AddNormal(result);
end;

function TBGRAPart3D.AddNormal(pt: TPoint3D_128): IBGRANormal3D;
begin
  if not Assigned(FNormalPool) then FNormalPool := TBGRANormalPool3D.Create(4);
  result := TBGRANormal3D.Create(FNormalPool,pt);
  AddNormal(result);
end;

procedure TBGRAPart3D.AddNormal(ANormal: IBGRANormal3D);
begin
  if FNormalCount = length(FNormals) then
    setlength(FNormals, FNormalCount*2+3);
  FNormals[FNormalCount] := ANormal;
  inc(FNormalCount);
end;

procedure TBGRAPart3D.RemoveVertex(Index: integer);
var i: integer;
begin
  if (Index >= 0) and (Index < FVertexCount) then
  begin
    for i := Index to FVertexCount-2 do
      FVertices[i] := FVertices[i+1];
    FVertices[FVertexCount-1] := nil;
    dec(FVertexCount);
  end;
end;

procedure TBGRAPart3D.RemoveNormal(Index: integer);
var i: integer;
begin
  if (Index >= 0) and (Index < FNormalCount) then
  begin
    for i := Index to FNormalCount-2 do
      FNormals[i] := FNormals[i+1];
    FNormals[FNormalCount-1] := nil;
    dec(FNormalCount);
  end;
end;

function TBGRAPart3D.GetRadius: single;
var i: integer;
    pt: TPoint3D_128;
    d: single;
begin
  result := 0;
  for i := 0 to GetVertexCount-1 do
  begin
    pt := GetVertex(i).SceneCoord_128;
    d:= sqrt(DotProduct3D_128(pt,pt));
    if d > result then result := d;
  end;
end;

constructor TBGRAPart3D.Create(AObject3D: TBGRAObject3D; AContainer: IBGRAPart3D);
begin
  FObject3D := AObject3D;
  FContainer := AContainer;
  FMatrix := MatrixIdentity3D;
  FCoordPool := TBGRACoordPool3D.Create(4);
  FNormalPool := nil;
  FNormalCount:= 0;
  FVertexCount := 0;
end;

destructor TBGRAPart3D.Destroy;
begin
  FVertices := nil;
  FVertexCount := 0;
  if FCoordPool.UsedCapacity > 0 then
    raise Exception.Create('Coordinate pool still used. Please set vertex references to nil before destroying the scene.');
  FreeAndNil(FCoordPool);
  if Assigned(FNormalPool) then
  begin
    if FNormalPool.UsedCapacity > 0 then
      raise Exception.Create('Normal pool still used');
    FreeAndNil(FNormalPool);
  end;
  inherited Destroy;
end;

procedure TBGRAPart3D.Clear(ARecursive: boolean);
var i: integer;
begin
  FVertices := nil;
  FVertexCount := 0;
  FNormals := nil;
  FNormalCount := 0;
  if ARecursive then
  begin
    for i := 0 to FPartCount-1 do
      FParts[i].Clear(ARecursive);
    FParts := nil;
    FPartCount := 0;
  end;
end;

function TBGRAPart3D.Add(x, y, z: single): IBGRAVertex3D;
begin
  result := TBGRAVertex3D.Create(FObject3D,FCoordPool,Point3D(x,y,z));
  Add(result);
end;

function TBGRAPart3D.Add(pt: TPoint3D): IBGRAVertex3D;
begin
  result := TBGRAVertex3D.Create(FObject3D,FCoordPool,pt);
  Add(result);
end;

function TBGRAPart3D.Add(pt: TPoint3D; normal: TPoint3D): IBGRAVertex3D;
begin
  result := TBGRAVertex3D.Create(FObject3D,FCoordPool,pt);
  result.CustomNormal := normal;
  Add(result);
end;

function TBGRAPart3D.Add(pt: TPoint3D_128): IBGRAVertex3D;
begin
  result := TBGRAVertex3D.Create(FObject3D,FCoordPool,pt);
  Add(result);
end;

function TBGRAPart3D.Add(pt: TPoint3D_128; normal: TPoint3D_128): IBGRAVertex3D;
begin
  result := TBGRAVertex3D.Create(FObject3D,FCoordPool,pt);
  result.CustomNormal := Point3D(normal);
  Add(result);
end;

function TBGRAPart3D.Add(const coords: array of single
  ): arrayOfIBGRAVertex3D;
var pts: array of TPoint3D;
    CoordsIdx: integer;
    i: Integer;
begin
  if length(coords) mod 3 <> 0 then
    raise exception.Create('Array size must be a multiple of 3');
  setlength(pts, length(coords) div 3);
  coordsIdx := 0;
  for i := 0 to high(pts) do
  begin
    pts[i] := Point3D(coords[CoordsIdx],coords[CoordsIdx+1],coords[CoordsIdx+2]);
    inc(coordsIdx,3);
  end;
  result := Add(pts);
end;

function TBGRAPart3D.Add(const pts: array of TPoint3D): arrayOfIBGRAVertex3D;
var
  i: Integer;
begin
  setlength(result, length(pts));
  for i := 0 to high(pts) do
    result[i] := TBGRAVertex3D.Create(FObject3D,FCoordPool,pts[i]);
  Add(result);
end;

function TBGRAPart3D.Add(const pts: array of TPoint3D_128
  ): arrayOfIBGRAVertex3D;
var
  i: Integer;
begin
  setlength(result, length(pts));
  for i := 0 to high(pts) do
    result[i] := TBGRAVertex3D.Create(FObject3D,FCoordPool,pts[i]);
  Add(result);
end;

procedure TBGRAPart3D.Add(const pts: array of IBGRAVertex3D);
var
  i: Integer;
begin
  if FVertexCount + length(pts) > length(FVertices) then
    setlength(FVertices, (FVertexCount*2 + length(pts))+1);
  for i := 0 to high(pts) do
  begin
    FVertices[FVertexCount] := pts[i];
    inc(FVertexCount);
  end;
end;

function TBGRAPart3D.GetBoundingBox: TBox3D;
var i: integer;
    pt: TPoint3D_128;
begin
  if GetVertexCount > 0 then
  begin
    result.min := GetVertex(0).SceneCoord;
    result.max := result.min;
  end else
  begin
    result.min := Point3D(0,0,0);
    result.max := Point3D(0,0,0);
    exit;
  end;
  for i := 1 to GetVertexCount-1 do
  begin
    pt := GetVertex(i).SceneCoord_128;
    if pt.x < result.min.x then result.min.x := pt.x else
    if pt.x > result.max.x then result.max.x := pt.x;
    if pt.y < result.min.y then result.min.y := pt.y else
    if pt.y > result.max.y then result.max.y := pt.y;
    if pt.z < result.min.z then result.min.z := pt.z else
    if pt.z > result.max.z then result.max.z := pt.z;
  end;
end;

function TBGRAPart3D.GetMatrix: TMatrix3D;
begin
  result := FMatrix;
end;

function TBGRAPart3D.GetPart(AIndex: Integer): IBGRAPart3D;
begin
  if (AIndex < 0) or (AIndex >= FPartCount) then
    raise ERangeError.Create('Index of out bounds');
  result := FParts[AIndex];
end;

function TBGRAPart3D.GetPartCount: integer;
begin
  result := FPartCount;
end;

function TBGRAPart3D.GetVertex(AIndex: Integer): IBGRAVertex3D;
begin
  if (AIndex < 0) or (AIndex >= FVertexCount) then
    raise ERangeError.Create('Index of out bounds');
  result := FVertices[AIndex];
end;

function TBGRAPart3D.GetVertexCount: integer;
begin
  result := FVertexCount;
end;

function TBGRAPart3D.GetNormal(AIndex: Integer): IBGRANormal3D;
begin
  if (AIndex < 0) or (AIndex >= FNormalCount) then
    raise ERangeError.Create('Index of out bounds');
  result := FNormals[AIndex];
end;

function TBGRAPart3D.GetNormalCount: integer;
begin
  result := FNormalCount;
end;

function TBGRAPart3D.GetTotalVertexCount: integer;
var i: integer;
begin
  result := GetVertexCount;
  for i := 0 to GetPartCount-1 do
    result += GetPart(i).GetTotalVertexCount;
end;

function TBGRAPart3D.GetTotalNormalCount: integer;
var i: integer;
begin
  result := GetNormalCount;
  for i := 0 to GetPartCount-1 do
    result += GetPart(i).GetTotalNormalCount;
end;

procedure TBGRAPart3D.ResetTransform;
begin
  FMatrix := MatrixIdentity3D;
end;

procedure TBGRAPart3D.Scale(size: single; Before: boolean = true);
begin
  Scale(size,size,size,Before);
end;

procedure TBGRAPart3D.Scale(x, y, z: single; Before: boolean = true);
begin
  Scale(Point3D(x,y,z),Before);
end;

procedure TBGRAPart3D.Scale(size: TPoint3D; Before: boolean = true);
begin
  if Before then
    FMatrix *= MatrixScale3D(size)
  else
    FMatrix := MatrixScale3D(size)*FMatrix;
end;

procedure TBGRAPart3D.RotateXDeg(angle: single; Before: boolean = true);
begin
  RotateXRad(-angle*Pi/180, Before);
end;

procedure TBGRAPart3D.RotateYDeg(angle: single; Before: boolean = true);
begin
  RotateYRad(-angle*Pi/180, Before);
end;

procedure TBGRAPart3D.RotateZDeg(angle: single; Before: boolean = true);
begin
  RotateZRad(-angle*Pi/180, Before);
end;

procedure TBGRAPart3D.RotateXRad(angle: single; Before: boolean = true);
begin
  if Before then
    FMatrix *= MatrixRotateX(angle)
  else
    FMatrix := MatrixRotateX(angle) * FMatrix;
end;

procedure TBGRAPart3D.RotateYRad(angle: single; Before: boolean = true);
begin
  if Before then
    FMatrix *= MatrixRotateY(angle)
  else
    FMatrix := MatrixRotateY(angle) * FMatrix;
end;

procedure TBGRAPart3D.RotateZRad(angle: single; Before: boolean = true);
begin
  if Before then
    FMatrix *= MatrixRotateZ(angle)
  else
    FMatrix := MatrixRotateZ(angle) * FMatrix;
end;

procedure TBGRAPart3D.SetMatrix(const AValue: TMatrix3D);
begin
  FMatrix := AValue;
end;

{$PUSH}{$OPTIMIZATION OFF} //avoids Internal error 2012090607
procedure TBGRAPart3D.ComputeWithMatrix(const AMatrix: TMatrix3D; const AProjection: TProjection3D);
var
  i: Integer;
  Composed: TMatrix3D;
begin
  Composed := AMatrix* self.FMatrix;
  FCoordPool.ComputeWithMatrix(Composed, AProjection);
  if Assigned(FNormalPool) then FNormalPool.ComputeWithMatrix(Composed);
  for i := 0 to FPartCount-1 do
    FParts[i].ComputeWithMatrix(Composed,AProjection);
end;
{$POP}

function TBGRAPart3D.ComputeCoordinate(var ASceneCoord: TPoint3D_128; const AProjection: TProjection3D): TPointF;
var part: IBGRAPart3D;
  newViewCoord: TPoint3D_128;
  InvZ: single;
begin
  newViewCoord := FMatrix * ASceneCoord;
  part := FContainer;
  while part <> nil do
  begin
    newViewCoord := part.Matrix * newViewCoord;
    part := part.Container;
  end;
  if NewViewCoord.z > 0 then
  begin
    InvZ := 1/NewViewCoord.z;
    result := PointF(NewViewCoord.x*InvZ*AProjection.Zoom.x + AProjection.Center.x,
                     NewViewCoord.y*InvZ*AProjection.Zoom.Y + AProjection.Center.y);
  end else
    result := PointF(0,0);
end;

procedure TBGRAPart3D.NormalizeViewNormal;
var
  i: Integer;
begin
  for i := 0 to FVertexCount-1 do
    FVertices[i].NormalizeViewNormal;
  for i := 0 to FPartCount-1 do
    FParts[i].NormalizeViewNormal;
end;

procedure TBGRAPart3D.Translate(x, y, z: single; Before: boolean = true);
begin
  Translate(Point3D(x,y,z),Before);
end;

procedure TBGRAPart3D.Translate(ofs: TPoint3D; Before: boolean = true);
begin
  if Before then
    FMatrix *= MatrixTranslation3D(ofs)
  else
    FMatrix := MatrixTranslation3D(ofs)*FMatrix;
end;

function TBGRAPart3D.CreatePart: IBGRAPart3D;
begin
  if FPartCount = length(FParts) then
    setlength(FParts, FPartCount*2+1);
  result := TBGRAPart3D.Create(FObject3D,self);
  FParts[FPartCount] := result;
  inc(FPartCount);
end;

function TBGRAPart3D.GetContainer: IBGRAPart3D;
begin
  result := FContainer;
end;

procedure TBGRAPart3D.SetVertex(AIndex: Integer; AValue: IBGRAVertex3D);
begin
  if (AIndex < 0) or (AIndex >= FVertexCount) then
    raise ERangeError.Create('Index of out bounds');
  FVertices[AIndex] := AValue;
end;

procedure TBGRAPart3D.SetNormal(AIndex: Integer; AValue: IBGRANormal3D);
begin
  if (AIndex < 0) or (AIndex >= FNormalCount) then
    raise ERangeError.Create('Index of out bounds');
  FNormals[AIndex] := AValue;
end;


