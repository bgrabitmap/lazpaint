unit BGRAMatrix3D;

{$mode objfpc}{$H+}

{$i bgrasse.inc}
{$ifdef BGRASSE_AVAILABLE}
  {$asmmode intel}
{$endif}

interface

uses
  BGRABitmapTypes, BGRASSE,
  BGRATransform;

type
  TMatrix3D = packed array[1..3,1..4] of single;
  TMatrix4D = packed array[1..4,1..4] of single;
  TProjection3D = packed record
    Zoom, Center: TPointF;
  end;
  TComputeProjectionFunc = function(AViewCoord: TPoint3D_128): TPointF of object;

operator*(const A: TMatrix3D; const M: TPoint3D): TPoint3D;
operator*(constref A: TMatrix3D; var M: TPoint3D_128): TPoint3D_128;
function MultiplyVect3DWithoutTranslation(constref A: TMatrix3D; constref M: TPoint3D_128): TPoint3D_128;
operator*(A,B: TMatrix3D): TMatrix3D;

function Matrix3D(m11,m12,m13,m14, m21,m22,m23,m24, m31,m32,m33,m34: single): TMatrix3D; overload;
function Matrix3D(vx,vy,vz,ofs: TPoint3D): TMatrix3D; overload;
function Matrix3D(vx,vy,vz,ofs: TPoint3D_128): TMatrix3D; overload;
function MatrixIdentity3D: TMatrix3D;
function MatrixInverse3D(A: TMatrix3D): TMatrix3D;
function MatrixTranslation3D(ofs: TPoint3D): TMatrix3D;
function MatrixScale3D(size: TPoint3D): TMatrix3D;
function MatrixRotateX(angle: single): TMatrix3D;
function MatrixRotateY(angle: single): TMatrix3D;
function MatrixRotateZ(angle: single): TMatrix3D;

operator *(const A, B: TMatrix4D): TMatrix4D;
function MatrixIdentity4D: TMatrix4D;
function AffineMatrixToMatrix4D(AValue: TAffineMatrix): TMatrix4D;

{$IFDEF BGRASSE_AVAILABLE}
procedure Matrix3D_SSE_Load(const A: TMatrix3D);
procedure MatrixMultiplyVect3D_SSE_Aligned(var M: TPoint3D_128; out N: TPoint3D_128);
procedure MatrixMultiplyVect3D_SSE3_Aligned(var M: TPoint3D_128; out N: TPoint3D_128);
procedure MatrixMultiplyVect3DWithoutTranslation_SSE_Aligned(var M: TPoint3D_128; out N: TPoint3D_128);
procedure MatrixMultiplyVect3DWithoutTranslation_SSE3_Aligned(var M: TPoint3D_128; out N: TPoint3D_128);
{$ENDIF}

implementation

procedure multiplyVect3(const A : TMatrix3D; const vx,vy,vz,vt: single; out outx,outy,outz: single);
begin
  outx := vx * A[1,1] + vy * A[1,2] + vz * A[1,3] + vt * A[1,4];
  outy := vx * A[2,1] + vy * A[2,2] + vz * A[2,3] + vt * A[2,4];
  outz := vx * A[3,1] + vy * A[3,2] + vz * A[3,3] + vt * A[3,4];
end;

procedure multiplyVect4(const A : TMatrix4D; const vx,vy,vz,vt: single; out outx,outy,outz,outt: single);
begin
  outx := vx * A[1,1] + vy * A[1,2] + vz * A[1,3] + vt * A[1,4];
  outy := vx * A[2,1] + vy * A[2,2] + vz * A[2,3] + vt * A[2,4];
  outz := vx * A[3,1] + vy * A[3,2] + vz * A[3,3] + vt * A[3,4];
  outt := vx * A[4,1] + vy * A[4,2] + vz * A[4,3] + vt * A[4,4];
end;

operator*(const A: TMatrix3D; const M: TPoint3D): TPoint3D;
begin
  result.x := M.x * A[1,1] + M.y * A[1,2] + M.z * A[1,3] + A[1,4];
  result.y := M.x * A[2,1] + M.y * A[2,2] + M.z * A[2,3] + A[2,4];
  result.z := M.x * A[3,1] + M.y * A[3,2] + M.z * A[3,3] + A[3,4];
end;

operator*(const A, B: TMatrix4D): TMatrix4D;
begin
  multiplyVect4(A, B[1,1],B[2,1],B[3,1],B[4,1], result[1,1],result[2,1],result[3,1],result[4,1]);
  multiplyVect4(A, B[1,2],B[2,2],B[3,2],B[4,2], result[1,2],result[2,2],result[3,2],result[4,2]);
  multiplyVect4(A, B[1,3],B[2,3],B[3,3],B[4,3], result[1,3],result[2,3],result[3,3],result[4,3]);
  multiplyVect4(A, B[1,4],B[2,4],B[3,4],B[4,4], result[1,4],result[2,4],result[3,4],result[4,4]);
end;

function MatrixIdentity4D: TMatrix4D;
begin
  result[1,1] := 1;  result[2,1] := 0;  result[3,1] := 0; result[4,1] := 0;
  result[1,2] := 0;  result[2,2] := 1;  result[3,2] := 0; result[4,2] := 0;
  result[1,3] := 0;  result[2,3] := 0;  result[3,3] := 1; result[4,3] := 0;
  result[1,4] := 0;  result[2,4] := 0;  result[3,4] := 0; result[4,4] := 1;
end;

function AffineMatrixToMatrix4D(AValue: TAffineMatrix): TMatrix4D;
begin
  result[1,1] := AValue[1,1];  result[2,1] := AValue[1,2];  result[3,1] := 0; result[4,1] := AValue[1,3];
  result[1,2] := AValue[2,1];  result[2,2] := AValue[2,2];  result[3,2] := 0; result[4,2] := AValue[2,3];
  result[1,3] := 0;            result[2,3] := 0;            result[3,3] := 1; result[4,3] := 0;
  result[1,4] := 0;            result[2,4] := 0;            result[3,4] := 0; result[4,4] := 1;
end;

{$IFDEF BGRASSE_AVAILABLE}
var SingleConst1 : single = 1;

  procedure Matrix3D_SSE_Load(const A: TMatrix3D);
  begin
    {$IFDEF cpux86_64}
    asm
      mov rax, A
      movups xmm5, [rax]
      movups xmm6, [rax+16]
      movups xmm7, [rax+32]
    end;
    {$ELSE}
    asm
      mov eax, A
      movups xmm5, [eax]
      movups xmm6, [eax+16]
      movups xmm7, [eax+32]
    end;
   {$ENDIF}
  end;

procedure MatrixMultiplyVect3D_SSE_Aligned(var M: TPoint3D_128; out N: TPoint3D_128);
var oldMt: single;
begin
  oldMt := M.t;
  M.t := SingleConst1;
  {$IFDEF cpux86_64}
  asm
    mov rax, M
    movaps xmm0, [rax]

    mov rax, N

    movaps xmm2,xmm0
    mulps xmm2,xmm5
    //mix1
    movaps xmm3, xmm2
    shufps xmm3, xmm3, $4e
    addps xmm2, xmm3
    //mix2
    movaps xmm3, xmm2
    shufps xmm3, xmm3, $11
    addps xmm2, xmm3

    movss [rax], xmm2

    movaps xmm2,xmm0
    mulps xmm2,xmm6
    //mix1
    movaps xmm3, xmm2
    shufps xmm3, xmm3, $4e
    addps xmm2, xmm3
    //mix2
    movaps xmm3, xmm2
    shufps xmm3, xmm3, $11
    addps xmm2, xmm3

    movss [rax+4], xmm2

    mulps xmm0,xmm7
    //mix1
    movaps xmm3, xmm0
    shufps xmm3, xmm3, $4e
    addps xmm0, xmm3
    //mix2
    movaps xmm3, xmm0
    shufps xmm3, xmm3, $11
    addps xmm0, xmm3

    movss [rax+8], xmm0
  end;
  {$ELSE}
    asm
    mov eax, M
    movaps xmm0, [eax]

    mov eax, N

    movaps xmm2,xmm0
    mulps xmm2,xmm5
    //mix1
    movaps xmm3, xmm2
    shufps xmm3, xmm3, $4e
    addps xmm2, xmm3
    //mix2
    movaps xmm3, xmm2
    shufps xmm3, xmm3, $11
    addps xmm2, xmm3

    movss [eax], xmm2

    movaps xmm2,xmm0
    mulps xmm2,xmm6
    //mix1
    movaps xmm3, xmm2
    shufps xmm3, xmm3, $4e
    addps xmm2, xmm3
    //mix2
    movaps xmm3, xmm2
    shufps xmm3, xmm3, $11
    addps xmm2, xmm3

    movss [eax+4], xmm2

    mulps xmm0,xmm7
    //mix1
    movaps xmm3, xmm0
    shufps xmm3, xmm3, $4e
    addps xmm0, xmm3
    //mix2
    movaps xmm3, xmm0
    shufps xmm3, xmm3, $11
    addps xmm0, xmm3

    movss [eax+8], xmm0
  end;
  {$ENDIF}
  M.t := oldMt;
  N.t := 0;
end;

procedure MatrixMultiplyVect3D_SSE3_Aligned(var M: TPoint3D_128; out N: TPoint3D_128);
var oldMt: single;
begin
  oldMt := M.t;
  M.t := SingleConst1;
  {$IFDEF cpux86_64}
  asm
    mov rax, M
    movaps xmm0, [rax]

    mov rax, N

    movaps xmm2,xmm0
    mulps xmm2,xmm5
    haddps xmm2,xmm2
    haddps xmm2,xmm2
    movss [rax], xmm2

    movaps xmm2,xmm0
    mulps xmm2,xmm6
    haddps xmm2,xmm2
    haddps xmm2,xmm2
    movss [rax+4], xmm2

    mulps xmm0,xmm7
    haddps xmm0,xmm0
    haddps xmm0,xmm0
    movss [rax+8], xmm0
  end;
  {$ELSE}
  asm
    mov eax, M
    movaps xmm0, [eax]

    mov eax, N

    movaps xmm2,xmm0
    mulps xmm2,xmm5
    haddps xmm2,xmm2
    haddps xmm2,xmm2
    movss [eax], xmm2

    movaps xmm2,xmm0
    mulps xmm2,xmm6
    haddps xmm2,xmm2
    haddps xmm2,xmm2
    movss [eax+4], xmm2

    mulps xmm0,xmm7
    haddps xmm0,xmm0
    haddps xmm0,xmm0
    movss [eax+8], xmm0
  end;
  {$ENDIF}
  M.t := oldMt;
end;

procedure MatrixMultiplyVect3DWithoutTranslation_SSE_Aligned(
  var M: TPoint3D_128; out N: TPoint3D_128);
begin
  {$IFDEF cpux86_64}
  asm
    mov rax, M
    movaps xmm0, [rax]

    mov rax, N

    movaps xmm2,xmm0
    mulps xmm2,xmm5
    //mix1
    movaps xmm3, xmm2
    shufps xmm3, xmm3, $4e
    addps xmm2, xmm3
    //mix2
    movaps xmm3, xmm2
    shufps xmm3, xmm3, $11
    addps xmm2, xmm3

    movss [rax], xmm2

    movaps xmm2,xmm0
    mulps xmm2,xmm6
    //mix1
    movaps xmm3, xmm2
    shufps xmm3, xmm3, $4e
    addps xmm2, xmm3
    //mix2
    movaps xmm3, xmm2
    shufps xmm3, xmm3, $11
    addps xmm2, xmm3

    movss [rax+4], xmm2

    mulps xmm0,xmm7
    //mix1
    movaps xmm3, xmm0
    shufps xmm3, xmm3, $4e
    addps xmm0, xmm3
    //mix2
    movaps xmm3, xmm0
    shufps xmm3, xmm3, $11
    addps xmm0, xmm3

    movss [rax+8], xmm0
  end;
  {$ELSE}
    asm
    mov eax, M
    movaps xmm0, [eax]

    mov eax, N

    movaps xmm2,xmm0
    mulps xmm2,xmm5
    //mix1
    movaps xmm3, xmm2
    shufps xmm3, xmm3, $4e
    addps xmm2, xmm3
    //mix2
    movaps xmm3, xmm2
    shufps xmm3, xmm3, $11
    addps xmm2, xmm3

    movss [eax], xmm2

    movaps xmm2,xmm0
    mulps xmm2,xmm6
    //mix1
    movaps xmm3, xmm2
    shufps xmm3, xmm3, $4e
    addps xmm2, xmm3
    //mix2
    movaps xmm3, xmm2
    shufps xmm3, xmm3, $11
    addps xmm2, xmm3

    movss [eax+4], xmm2

    mulps xmm0,xmm7
    //mix1
    movaps xmm3, xmm0
    shufps xmm3, xmm3, $4e
    addps xmm0, xmm3
    //mix2
    movaps xmm3, xmm0
    shufps xmm3, xmm3, $11
    addps xmm0, xmm3

    movss [eax+8], xmm0
  end;
  {$ENDIF}
end;

procedure MatrixMultiplyVect3DWithoutTranslation_SSE3_Aligned(
  var M: TPoint3D_128; out N: TPoint3D_128);
begin
  {$IFDEF cpux86_64}
  asm
    mov rax, M
    movaps xmm0, [rax]

    mov rax, N

    movaps xmm2,xmm0
    mulps xmm2,xmm5
    haddps xmm2,xmm2
    haddps xmm2,xmm2
    movss [rax], xmm2

    movaps xmm2,xmm0
    mulps xmm2,xmm6
    haddps xmm2,xmm2
    haddps xmm2,xmm2
    movss [rax+4], xmm2

    mulps xmm0,xmm7
    haddps xmm0,xmm0
    haddps xmm0,xmm0
    movss [rax+8], xmm0
  end;
  {$ELSE}
  asm
    mov eax, M
    movaps xmm0, [eax]

    mov eax, N

    movaps xmm2,xmm0
    mulps xmm2,xmm5
    haddps xmm2,xmm2
    haddps xmm2,xmm2
    movss [eax], xmm2

    movaps xmm2,xmm0
    mulps xmm2,xmm6
    haddps xmm2,xmm2
    haddps xmm2,xmm2
    movss [eax+4], xmm2

    mulps xmm0,xmm7
    haddps xmm0,xmm0
    haddps xmm0,xmm0
    movss [eax+8], xmm0
  end;
  {$ENDIF}
end;

{$ENDIF}

operator*(constref A: TMatrix3D; var M: TPoint3D_128): TPoint3D_128;
{$IFDEF CPUI386}var oldMt: single; {$ENDIF}
begin
  {$IFDEF CPUI386}
  if UseSSE then
  begin
    oldMt := M.t;
    M.t := SingleConst1;
    if UseSSE3 then
    asm
      mov eax, A
      movups xmm5, [eax]
      movups xmm6, [eax+16]
      movups xmm7, [eax+32]

      mov eax, M
      movups xmm0, [eax]

      mov eax, result

      movaps xmm4,xmm0
      mulps xmm4,xmm5
      haddps xmm4,xmm4
      haddps xmm4,xmm4
      movss [eax], xmm4

      movaps xmm4,xmm0
      mulps xmm4,xmm6
      haddps xmm4,xmm4
      haddps xmm4,xmm4
      movss [eax+4], xmm4

      mulps xmm0,xmm7
      haddps xmm0,xmm0
      haddps xmm0,xmm0
      movss [eax+8], xmm0
    end else
    asm
      mov eax, A
      movups xmm5, [eax]
      movups xmm6, [eax+16]
      movups xmm7, [eax+32]

      mov eax, M
      movups xmm0, [eax]

      mov eax, result

      movaps xmm4,xmm0
      mulps xmm4,xmm5
      //mix1
      movaps xmm3, xmm4
      shufps xmm3, xmm3, $4e
      addps xmm4, xmm3
      //mix2
      movaps xmm3, xmm4
      shufps xmm3, xmm3, $11
      addps xmm4, xmm3

      movss [eax], xmm4

      movaps xmm4,xmm0
      mulps xmm4,xmm6
      //mix1
      movaps xmm3, xmm4
      shufps xmm3, xmm3, $4e
      addps xmm4, xmm3
      //mix2
      movaps xmm3, xmm4
      shufps xmm3, xmm3, $11
      addps xmm4, xmm3

      movss [eax+4], xmm4

      mulps xmm0,xmm7
      //mix1
      movaps xmm3, xmm0
      shufps xmm3, xmm3, $4e
      addps xmm0, xmm3
      //mix2
      movaps xmm3, xmm0
      shufps xmm3, xmm3, $11
      addps xmm0, xmm3

      movss [eax+8], xmm0
    end;
    M.t := oldMt;
    result.t := 0;
  end else
  {$ENDIF}
  begin
    result.x := M.x * A[1,1] + M.y * A[1,2] + M.z * A[1,3] + A[1,4];
    result.y := M.x * A[2,1] + M.y * A[2,2] + M.z * A[2,3] + A[2,4];
    result.z := M.x * A[3,1] + M.y * A[3,2] + M.z * A[3,3] + A[3,4];
    result.t := 0;
  end;
end;

function MultiplyVect3DWithoutTranslation(constref A: TMatrix3D; constref M: TPoint3D_128): TPoint3D_128;
begin
  {$IFDEF CPUI386}
  if UseSSE then
  begin
    if UseSSE3 then
    asm
      mov eax, A
      movups xmm5, [eax]
      movups xmm6, [eax+16]
      movups xmm7, [eax+32]

      mov eax, M
      movups xmm0, [eax]

      mov eax, result

      movaps xmm4,xmm0
      mulps xmm4,xmm5
      haddps xmm4,xmm4
      haddps xmm4,xmm4
      movss [eax], xmm4

      movaps xmm4,xmm0
      mulps xmm4,xmm6
      haddps xmm4,xmm4
      haddps xmm4,xmm4
      movss [eax+4], xmm4

      mulps xmm0,xmm7
      haddps xmm0,xmm0
      haddps xmm0,xmm0
      movss [eax+8], xmm0
    end else
    asm
      mov eax, A
      movups xmm5, [eax]
      movups xmm6, [eax+16]
      movups xmm7, [eax+32]

      mov eax, M
      movups xmm0, [eax]

      mov eax, result

      movaps xmm4,xmm0
      mulps xmm4,xmm5
      //mix1
      movaps xmm3, xmm4
      shufps xmm3, xmm3, $4e
      addps xmm4, xmm3
      //mix2
      movaps xmm3, xmm4
      shufps xmm3, xmm3, $11
      addps xmm4, xmm3

      movss [eax], xmm4

      movaps xmm4,xmm0
      mulps xmm4,xmm6
      //mix1
      movaps xmm3, xmm4
      shufps xmm3, xmm3, $4e
      addps xmm4, xmm3
      //mix2
      movaps xmm3, xmm4
      shufps xmm3, xmm3, $11
      addps xmm4, xmm3

      movss [eax+4], xmm4

      mulps xmm0,xmm7
      //mix1
      movaps xmm3, xmm0
      shufps xmm3, xmm3, $4e
      addps xmm0, xmm3
      //mix2
      movaps xmm3, xmm0
      shufps xmm3, xmm3, $11
      addps xmm0, xmm3

      movss [eax+8], xmm0
    end;
  end else
  {$ENDIF}
  begin
    result.x := M.x * A[1,1] + M.y * A[1,2] + M.z * A[1,3];
    result.y := M.x * A[2,1] + M.y * A[2,2] + M.z * A[2,3];
    result.z := M.x * A[3,1] + M.y * A[3,2] + M.z * A[3,3];
    result.t := 0;
  end;
end;

operator*(A,B: TMatrix3D): TMatrix3D;
begin
  multiplyVect3(A, B[1,1],B[2,1],B[3,1],0, result[1,1],result[2,1],result[3,1]);
  multiplyVect3(A, B[1,2],B[2,2],B[3,2],0, result[1,2],result[2,2],result[3,2]);
  multiplyVect3(A, B[1,3],B[2,3],B[3,3],0, result[1,3],result[2,3],result[3,3]);
  multiplyVect3(A, B[1,4],B[2,4],B[3,4],1, result[1,4],result[2,4],result[3,4]);
end;

function MatrixIdentity3D: TMatrix3D;
begin
  result := Matrix3D( 1,0,0,0,
                      0,1,0,0,
                      0,0,1,0);
end;

function Matrix3D(m11, m12, m13, m14, m21, m22, m23, m24, m31, m32, m33,
  m34: single): TMatrix3D;
begin
  result[1,1] := m11;
  result[1,2] := m12;
  result[1,3] := m13;
  result[1,4] := m14;

  result[2,1] := m21;
  result[2,2] := m22;
  result[2,3] := m23;
  result[2,4] := m24;

  result[3,1] := m31;
  result[3,2] := m32;
  result[3,3] := m33;
  result[3,4] := m34;
end;

function MatrixInverse3D(A: TMatrix3D): TMatrix3D;
var ofs: TPoint3D;
begin
  ofs := Point3D(A[1,4],A[2,4],A[3,4]);

  result[1,1] := A[1,1];
  result[1,2] := A[2,1];
  result[1,3] := A[3,1];
  result[1,4] := 0;

  result[2,1] := A[1,2];
  result[2,2] := A[2,2];
  result[2,3] := A[3,2];
  result[2,4] := 0;

  result[3,1] := A[1,3];
  result[3,2] := A[2,3];
  result[3,3] := A[3,3];
  result[3,4] := 0;

  result := result*MatrixTranslation3D(-ofs);
end;

function Matrix3D(vx, vy, vz, ofs: TPoint3D): TMatrix3D;
begin
  result := Matrix3D(vx.x, vy.x, vz.x, ofs.x,
                     vx.y, vy.y, vz.y, ofs.y,
                     vx.z, vy.z, vz.z, ofs.z);
end;

function Matrix3D(vx, vy, vz, ofs: TPoint3D_128): TMatrix3D;
begin
  result := Matrix3D(vx.x, vy.x, vz.x, ofs.x,
                     vx.y, vy.y, vz.y, ofs.y,
                     vx.z, vy.z, vz.z, ofs.z);
end;

function MatrixTranslation3D(ofs: TPoint3D): TMatrix3D;
begin
  result := Matrix3D(1,0,0,ofs.x,
                     0,1,0,ofs.Y,
                     0,0,1,ofs.z);
end;

function MatrixScale3D(size: TPoint3D): TMatrix3D;
begin
  result := Matrix3D(size.x,0,0,0,
                     0,size.y,0,0,
                     0,0,size.z,0);
end;

function MatrixRotateX(angle: single): TMatrix3D;
begin
  result := Matrix3D( 1,       0,           0,       0,
                      0,   cos(angle), sin(angle),   0,
                      0,  -sin(angle), cos(angle),   0);
end;

function MatrixRotateY(angle: single): TMatrix3D;
begin
  result := Matrix3D(  cos(angle), 0, -sin(angle),  0,
                           0,      1,      0,       0,
                       sin(angle), 0,  cos(angle),  0);
end;

function MatrixRotateZ(angle: single): TMatrix3D;
begin
  result := Matrix3D(  cos(angle), sin(angle),   0,    0,
                      -sin(angle), cos(angle),   0,    0,
                          0,            0,       1,    0);
end;

end.

