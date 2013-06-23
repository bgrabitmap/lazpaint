unit BGRACoordPool3D;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmapTypes, BGRASSE;

type
  PBGRACoordData3D = ^TBGRACoordData3D;
  TBGRACoordData3D = packed record
    {0} sceneCoord: TPoint3D_128;
    {16} viewCoord: TPoint3D_128;
    {32} projectedCoord: TPointF;
    {40} InvZ: single;
    {44} used: longbool;
    {48} viewNormal: TPoint3D_128;
  end; {64}

  { TBGRACoordPool3D }

  TBGRACoordPool3D = class
  private
    FFirstFree: integer;
    FNbCoord,FCapacity: integer;
    FPoolData: TMemoryBlockAlign128;
    FUsedCapacity : integer;
    function GetCoordData(AIndex: integer): PBGRACoordData3D;
    procedure SetCapacity(ACapacity: integer);
  public
    constructor Create(ACapacity: integer);
    destructor Destroy; override;
    procedure Remove(AIndex: integer);
    function Add: integer;
    property CoordData[AIndex: integer]: PBGRACoordData3D read GetCoordData;
    property Capacity: integer read FCapacity;
    property UsedCapacity: integer read FUsedCapacity;
  end;

implementation

{ TBGRACoordPool3D }

procedure TBGRACoordPool3D.SetCapacity(ACapacity: integer);
var NewPoolData: TMemoryBlockAlign128;
begin
  if FCapacity <> ACapacity then
  begin
    if ACapacity = 0 then
      FreeAndNil(FPoolData)
    else
    begin
      NewPoolData := TMemoryBlockAlign128.Create(ACapacity*sizeof(TBGRACoordData3D));
      if FCapacity <> 0 then
      begin
        //previous block is smaller
        if FCapacity < ACapacity then
        begin
          move(FPoolData.Data^, NewPoolData.Data^, FCapacity*sizeof(TBGRACoordData3D));
          //pad with zeros
          fillchar((pbyte(NewPoolData.Data)+FCapacity*sizeof(TBGRACoordData3D))^,(ACapacity-FCapacity)*sizeof(TBGRACoordData3D),0);
        end
        else //previous block is greater or equal
          move(FPoolData.Data^, NewPoolData.Data^, ACapacity*sizeof(TBGRACoordData3D));
        FreeAndNil(FPoolData);
      end else
       //clear new block
        fillchar(pbyte(NewPoolData.Data)^,ACapacity*sizeof(TBGRACoordData3D),0);

      FPoolData := NewPoolData;
    end;
    FCapacity:= ACapacity;
  end;
end;

constructor TBGRACoordPool3D.Create(ACapacity: integer);
begin
  FCapacity := 0;
  FPoolData := nil;
  FNbCoord:= 0;
  FFirstFree := 0;
  FUsedCapacity := 0;
  SetCapacity(ACapacity);
end;

destructor TBGRACoordPool3D.Destroy;
begin
  FPoolData.Free;
  inherited Destroy;
end;

procedure TBGRACoordPool3D.Remove(AIndex: integer);
begin
  if CoordData[AIndex]^.used then
  begin
    CoordData[AIndex]^.used := false;
    if AIndex < FFirstFree then FFirstFree := AIndex;
    if AIndex = FUsedCapacity-1 then
    begin
      while (FUsedCapacity > 0) and not CoordData[FUsedCapacity-1]^.used do
        dec(FUsedCapacity);
    end;
  end;
end;

function TBGRACoordPool3D.Add: integer;
begin
  //check for free space
  while FFirstFree < FCapacity do
  begin
    if not CoordData[FFirstFree]^.used then
    begin
      CoordData[FFirstFree]^.used := false;
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
  CoordData[FFirstFree]^.used := false;
  result := FFirstFree;
  inc(FFirstFree);
  if FFirstFree > FUsedCapacity then
    FUsedCapacity := FFirstFree;
end;

function TBGRACoordPool3D.GetCoordData(AIndex: integer): PBGRACoordData3D;
begin
  result := PBGRACoordData3D(FPoolData.Data)+AIndex;
end;

end.

