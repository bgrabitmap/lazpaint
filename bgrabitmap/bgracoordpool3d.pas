unit BGRACoordPool3D;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmapTypes, BGRASSE, BGRAMatrix3D;

type
  PBGRACoordData3D = ^TBGRACoordData3D;
  TBGRACoordData3D = packed record
    {0} sceneCoord: TPoint3D_128;
    {16} viewCoord: TPoint3D_128;
    {32} projectedCoord: TPointF;
    {40} InvZ: single;
    {44} used: wordbool; customNormalUsed: wordbool;
    {48} viewNormal: TPoint3D_128;
    {64} customNormal: TPoint3D_128;
  end; {80}

  PBGRANormalData3D = ^TBGRANormalData3D;
  TBGRANormalData3D = packed record
    {0} customNormal: TPoint3D_128;
    {16} viewNormal: TPoint3D_128;
    {32} used: longbool;
    {36} filler1,filler2,filler3: longword;
  end; {48}

  { TBGRAGenericPool }

  TBGRAGenericPool = class
  private
    FFirstFree: integer;
    FNbElements,FCapacity: integer;
    FElementSize: PtrInt;
    FUsedCapacity : integer;
    function GetElement(AIndex: integer): Pointer;
    procedure SetCapacity(ACapacity: integer);
  protected
    FPoolData: TMemoryBlockAlign128;
    function GetUsed({%H-}AElement: integer): boolean; virtual;
    procedure SetUsed({%H-}AElement: integer; {%H-}AUsed: boolean); virtual;
    procedure Remove(AIndex: integer); //does not work if GetUsed/SetUsed are not implemented
  public
    constructor Create(ACapacity: integer; AElementSize: integer);
    destructor Destroy; override;
    function Add: integer;
    property Element[AIndex: integer]: Pointer read GetElement;
    property Capacity: integer read FCapacity;
    property UsedCapacity: integer read FUsedCapacity;
  end;

  { TBGRACoordPool3D }

  TBGRACoordPool3D = class(TBGRAGenericPool)
  private
    function GetCoordData(AIndex: integer): PBGRACoordData3D;
  protected
    function GetUsed(AElement: integer): boolean; override;
    procedure SetUsed(AElement: integer; AUsed: boolean); override;
  public
    procedure Remove(AIndex: integer);
    constructor Create(ACapacity: integer);
    procedure ComputeWithMatrix(const AMatrix: TMatrix3D; const AProjection: TProjection3D);
    property CoordData[AIndex: integer]: PBGRACoordData3D read GetCoordData;
  end;

  { TBGRANormalPool3D }

  TBGRANormalPool3D = class(TBGRAGenericPool)
  private
    function GetNormalData(AIndex: integer): PBGRANormalData3D;
  protected
    function GetUsed(AElement: integer): boolean; override;
    procedure SetUsed(AElement: integer; AUsed: boolean); override;
  public
    procedure Remove(AIndex: integer);
    constructor Create(ACapacity: integer);
    procedure ComputeWithMatrix(const AMatrix: TMatrix3D);
    property NormalData[AIndex: integer]: PBGRANormalData3D read GetNormalData;
  end;

implementation

{ TBGRAGenericPool }

function TBGRAGenericPool.GetElement(AIndex: integer): Pointer;
begin
  result := Pointer(PByte(FPoolData.Data)+AIndex*FElementSize);
end;

procedure TBGRAGenericPool.SetCapacity(ACapacity: integer);
var NewPoolData: TMemoryBlockAlign128;
begin
  if FCapacity <> ACapacity then
  begin
    if ACapacity = 0 then
      FreeAndNil(FPoolData)
    else
    begin
      NewPoolData := TMemoryBlockAlign128.Create(ACapacity*FElementSize);
      if FCapacity <> 0 then
      begin
        //previous block is smaller
        if FCapacity < ACapacity then
        begin
          move(FPoolData.Data^, NewPoolData.Data^, FCapacity*FElementSize);
          //pad with zeros
          fillchar((pbyte(NewPoolData.Data)+FCapacity*FElementSize)^,(ACapacity-FCapacity)*FElementSize,0);
        end
        else //previous block is greater or equal
          move(FPoolData.Data^, NewPoolData.Data^, ACapacity*FElementSize);
        FreeAndNil(FPoolData);
      end else
       //clear new block
        fillchar(pbyte(NewPoolData.Data)^,ACapacity*FElementSize,0);

      FPoolData := NewPoolData;
    end;
    FCapacity:= ACapacity;
  end;
end;

function TBGRAGenericPool.GetUsed(AElement: integer): boolean;
begin
  result := false;
end;

procedure TBGRAGenericPool.SetUsed(AElement: integer; AUsed: boolean);
begin
  //nothing
end;

constructor TBGRAGenericPool.Create(ACapacity: integer; AElementSize: integer);
begin
  FCapacity := 0;
  FPoolData := nil;
  FNbElements:= 0;
  FFirstFree := 0;
  FUsedCapacity := 0;
  FElementSize:= AElementSize;
  SetCapacity(ACapacity);
end;

destructor TBGRAGenericPool.Destroy;
begin
  FreeAndNil(FPoolData);
  FCapacity := 0;
  FNbElements:= 0;
  FFirstFree := 0;
  FUsedCapacity := 0;
  inherited Destroy;
end;

procedure TBGRAGenericPool.Remove(AIndex: integer);
begin
  if (AIndex < 0) or (AIndex >= FUsedCapacity) then
    raise ERangeError.Create('Index out of bounds');
  if GetUsed(AIndex) then
  begin
    SetUsed(AIndex, false);
    if AIndex < FFirstFree then FFirstFree := AIndex;
    if AIndex = FUsedCapacity-1 then
    begin
      while (FUsedCapacity > 0) and not GetUsed(FUsedCapacity-1) do
        dec(FUsedCapacity);
    end;
  end;
end;

function TBGRAGenericPool.Add: integer;
begin
  //check for free space
  while FFirstFree < FCapacity do
  begin
    if not GetUsed(FFirstFree) then
    begin
      SetUsed(FFirstFree,True);
      result := FFirstFree;
      inc(FFirstFree);
      if FFirstFree > FUsedCapacity then
        FUsedCapacity := FFirstFree;
      exit;
    end;
    inc(FFirstFree);
  end;

  //no free space
  SetCapacity(FCapacity*2+8);
  SetUsed(FFirstFree, true);
  result := FFirstFree;
  inc(FFirstFree);
  if FFirstFree > FUsedCapacity then
    FUsedCapacity := FFirstFree;
end;

{ TBGRACoordPool3D }

constructor TBGRACoordPool3D.Create(ACapacity: integer);
begin
  inherited Create(ACapacity,SizeOf(TBGRACoordData3D));
end;

procedure TBGRACoordPool3D.ComputeWithMatrix(const AMatrix: TMatrix3D;
  const AProjection: TProjection3D);
var
  P: PBGRACoordData3D;
  I: NativeInt;
begin
  if UsedCapacity = 0 then exit;
  P := PBGRACoordData3D(FPoolData.Data);
  {$IFDEF CPUI386}
  {$asmmode intel}
  if UseSSE then
  begin
    Matrix3D_SSE_Load(AMatrix);
    asm
      mov eax,[AProjection]
      movups xmm4,[eax]
      xorps xmm1,xmm1
    end;
    i := UsedCapacity;
    if UseSSE3 then
    begin
      while i > 0 do
      with P^ do
      begin
        if used then
        begin
          MatrixMultiplyVect3D_SSE3_Aligned(sceneCoord,viewCoord);
          if viewCoord.z > 0 then
          begin
            asm
              mov eax, P
              movaps xmm3, [eax+16] //viewCoord
              movaps xmm2,xmm3
              shufps xmm2,xmm3,2+8+32+128
              rcpps xmm2,xmm2  //xmm2 = InvZ
              movss [eax+40],xmm2 //-> InvZ

              mulps xmm3,xmm4  //xmm3 *= Projection.Zoom
              mulps xmm3,xmm2  //xmm3 *= InvZ

              movhlps xmm0,xmm4  //xmm0 = Projection.Center
              addps xmm3,xmm0  //xmm3 += Projection.Center

              movlps [eax+32],xmm3 //->projectedCoord
              movaps [eax+48],xmm1 //->normal
            end;
          end else
          asm
            mov eax, P
            movlps [eax+32],xmm1  //0->projectedCoord
            movaps [eax+48],xmm1 //->normal
          end;
          if customNormalUsed then
            MatrixMultiplyVect3DWithoutTranslation_SSE3_Aligned(customNormal,viewNormal);
        end;
        dec(i);
        inc(p);
      end;
    end else
    begin
      while i > 0 do
      with P^ do
      begin
        if used then
        begin
          MatrixMultiplyVect3D_SSE_Aligned(sceneCoord,viewCoord);
          if viewCoord.z > 0 then
          begin
            asm
              mov eax, P
              movaps xmm3, [eax+16] //viewCoord
              movaps xmm2,xmm3
              shufps xmm2,xmm3,2+8+32+128
              rcpps xmm2,xmm2  //xmm2 = InvZ
              movss [eax+40],xmm2 //-> InvZ

              mulps xmm3,xmm4  //xmm3 *= Projection.Zoom
              mulps xmm3,xmm2  //xmm3 *= InvZ

              movhlps xmm0,xmm4  //xmm0 = Projection.Center
              addps xmm3,xmm0  //xmm3 += Projection.Center

              movlps [eax+32],xmm3 //->projectedCoord
              movaps [eax+48],xmm1 //->normal
            end;
          end else
          asm
            mov eax, P
            movlps [eax+32],xmm1  //0 ->projectedCoord
            movaps [eax+48],xmm1 //->normal
          end;
          if customNormalUsed then
            MatrixMultiplyVect3DWithoutTranslation_SSE_Aligned(customNormal,viewNormal);
        end;
        dec(i);
        inc(p);
      end;
    end;
  end
  else
  {$ENDIF}
  begin
    i := UsedCapacity;
    while i > 0 do
    with P^ do
    begin
      if used then
      begin
        viewCoord := AMatrix*sceneCoord;
        if customNormalUsed then
          viewNormal := MultiplyVect3DWithoutTranslation(AMatrix,customNormal)
        else
          ClearPoint3D_128(viewNormal);
        if viewCoord.z > 0 then
        begin
          InvZ := 1/viewCoord.z;
          projectedCoord := PointF(viewCoord.x*InvZ*AProjection.Zoom.x + AProjection.Center.x,
                                   viewCoord.y*InvZ*AProjection.Zoom.Y + AProjection.Center.y);
        end else
          projectedCoord := PointF(0,0);
      end;
      dec(i);
      inc(p);
    end;
  end;
end;

function TBGRACoordPool3D.GetCoordData(AIndex: integer): PBGRACoordData3D;
begin
  result := PBGRACoordData3D(FPoolData.Data)+AIndex;
end;

function TBGRACoordPool3D.GetUsed(AElement: integer): boolean;
begin
  Result:= CoordData[AElement]^.used;
end;

procedure TBGRACoordPool3D.SetUsed(AElement: integer; AUsed: boolean);
begin
  CoordData[AElement]^.used := AUsed;
end;

procedure TBGRACoordPool3D.Remove(AIndex: integer);
begin
  inherited Remove(AIndex);
end;

{ TBGRANormalPool3D }

function TBGRANormalPool3D.GetNormalData(AIndex: integer): PBGRANormalData3D;
begin
  result := PBGRANormalData3D(FPoolData.Data)+AIndex;
end;

function TBGRANormalPool3D.GetUsed(AElement: integer): boolean;
begin
  Result:= NormalData[AElement]^.used;
end;

procedure TBGRANormalPool3D.SetUsed(AElement: integer; AUsed: boolean);
begin
  NormalData[AElement]^.used := AUsed;
end;

procedure TBGRANormalPool3D.Remove(AIndex: integer);
begin
  inherited Remove(AIndex);
end;

constructor TBGRANormalPool3D.Create(ACapacity: integer);
begin
  inherited Create(ACapacity,SizeOf(TBGRANormalData3D));
end;

procedure TBGRANormalPool3D.ComputeWithMatrix(const AMatrix: TMatrix3D);
var
  P: PBGRANormalData3D;
  I: NativeInt;
begin
  if UsedCapacity = 0 then exit;
  P := PBGRANormalData3D(FPoolData.Data);
  {$IFDEF CPUI386}
  {$asmmode intel}
  if UseSSE then
  begin
    Matrix3D_SSE_Load(AMatrix);
    i := UsedCapacity;
    if UseSSE3 then
    begin
      while i > 0 do
      with P^ do
      begin
        if used then
          MatrixMultiplyVect3DWithoutTranslation_SSE3_Aligned(customNormal,viewNormal);
        dec(i);
        inc(p);
      end;
    end else
    begin
      while i > 0 do
      with P^ do
      begin
        if used then
          MatrixMultiplyVect3DWithoutTranslation_SSE_Aligned(customNormal,viewNormal);
        dec(i);
        inc(p);
      end;
    end;
  end
  else
  {$ENDIF}
  begin
    i := UsedCapacity;
    while i > 0 do
    with P^ do
    begin
      if used then
        viewNormal := MultiplyVect3DWithoutTranslation(AMatrix,customNormal);
      dec(i);
      inc(p);
    end;
  end;
end;

end.

