unit BGRASSE;

{$mode objfpc}{$H+}

interface

{begin  //param: eax, edx, ecx  //float: eax ecx edx
  //flds $d9
  //fadds $d8
  //fstps $d9 +$18
  //fmuls $d8 +$08
  //fsubrs $d8 +$28
  //offset +$40 $..}
uses
  BGRABitmapTypes {$ifdef CPUI386}, cpu, mmx{$endif};

const FLAG_ENABLED_SSE = true;

var UseSSE, UseSSE2, UseSSE3 : boolean;

{$ifdef CPUI386}
  {$asmmode intel}
  //SSE rotate singles
  const Shift231 = 1 + 8;
        Shift312 = 2 + 16;
{$endif}

type
  TPoint3D_128 = packed record x,y,z,t: single; end;
  PPoint3D_128 = ^TPoint3D_128;

  function Point3D(const point3D_128: TPoint3D_128): TPoint3D; inline; overload;
  function Point3D_128(const point3D: TPoint3D): TPoint3D_128; inline; overload;
  function Point3D_128(const pointF: TPointF): TPoint3D_128; inline; overload;
  function Point3D_128(x,y,z: single): TPoint3D_128; inline; overload;
  function Point3D_128(x,y,z,t: single): TPoint3D_128; inline; overload;
  procedure Normalize3D_128_SqLen(var v: TPoint3D_128; out SqLen: single);
  operator * (const v1: TPoint3D_128; const factor: single): TPoint3D_128;
  operator + (const v1,v2: TPoint3D_128): TPoint3D_128;
  operator - (const v1,v2: TPoint3D_128): TPoint3D_128;
  operator - (const v: TPoint3D_128): TPoint3D_128; inline;
  operator = (const v1,v2: TPoint3D_128): boolean; inline;
  procedure ClearPoint3D_128(out v: TPoint3D_128);
  {$IFDEF CPUI386}
  procedure ClearPoint3D_128_AlignedSSE(out v: TPoint3D_128);
  {$ENDIF}
  function IsPoint3D_128_Zero(const v: TPoint3D_128): boolean; inline;

var
  Add3D_Aligned : procedure (var dest: TPoint3D_128; const src: TPoint3D_128);
  Normalize3D_128 : procedure (var v: TPoint3D_128);
  VectProduct3D_128 : procedure (const u,v: TPoint3D_128; out w: TPoint3D_128);
  DotProduct3D_128 : function (const v1,v2: TPoint3D_128): single;

const
  Point3D_128_Zero : TPoint3D_128 = (x:0; y:0; z:0; t:0);

type

  { TMemoryBlockAlign128 }

  TMemoryBlockAlign128 = class
  private
    FContainer: Pointer;
    FData: Pointer;
  public
    constructor Create(size: integer);
    destructor Destroy; override;
    property Data: pointer read FData;
  end;

  PBasicLightingContext = ^TBasicLightingContext;
  TBasicLightingContext = packed record
    {0} Position, {16} Normal: TPoint3D_128;
    {32} PositionInvZ, {48} NormalInvZ: TPoint3D_128;
    {64} PositionStepInvZ, {80} NormalStepInvZ: TPoint3D_128;
    {96} dummy4: single;
    {100} dummy3: LongBool;
    {104} dummy1: longword;
    {108} dummy2: longword;
    {112} dummy: packed array[0..15]of byte;
  end; {128}

const ExtendedLightingContextSize = 128;

implementation

function Point3D(const point3D_128: TPoint3D_128): TPoint3D; inline; overload;
begin
  result.x := point3D_128.x;
  result.y := point3D_128.y;
  result.z := point3D_128.z;
end;

function Point3D_128(const point3D: TPoint3D): TPoint3D_128; inline; overload;
begin
  result.x := point3D.x;
  result.y := point3D.y;
  result.z := point3D.z;
  result.t := 0;
end;

function Point3D_128(const pointF: TPointF): TPoint3D_128;
begin
   result.x := pointF.x;
   result.y := pointF.y;
   result.z := 0;
   result.t := 0;
end;

function Point3D_128(x,y,z: single): TPoint3D_128; inline; overload;
begin
  result.x := x;
  result.y := y;
  result.z := z;
  result.t := 0;
end;

function Point3D_128(x,y,z,t: single): TPoint3D_128; inline; overload;
begin
  result.x := x;
  result.y := y;
  result.z := z;
  result.t := t;
end;

operator + (const v1,v2: TPoint3D_128): TPoint3D_128;
{$ifdef CPUI386} assembler;
asm
  db $d9, $00 //flds [eax]
  db $d8, $02 //fadds [edx]
  db $d9, $19 //fstps [ecx]

  db $d9, $40, $04 //flds [eax+4]
  db $d8, $42, $04 //fadds [edx+4]
  db $d9, $59, $04 //fstps [ecx+4]

  db $d9, $40, $08 //flds [eax+8]
  db $d8, $42, $08 //fadds [edx+8]
  db $d9, $59, $08 //fstps [ecx+8]

  xor eax,eax
  mov [ecx+12],eax
end;
{$else}
begin
  result.x := v1.x+v2.x;
  result.y := v1.y+v2.y;
  result.z := v1.z+v2.z;
  result.t := 0;
end;
{$endif}

{$ifdef CPUI386}
procedure Add3D_AlignedSSE(var dest: TPoint3D_128; const src: TPoint3D_128); assembler;
asm
  movaps xmm0, [eax]
  movups xmm1, [edx]
  addps xmm0, xmm1
  movaps [eax], xmm0
end;
{$endif}

procedure Add3D_NoSSE(var dest: TPoint3D_128; const src: TPoint3D_128);
{$ifdef CPUI386} assembler;
asm
  db $d9, $00 //flds [eax]
  db $d8, $02 //fadds [edx]
  db $d9, $18 //fstps [eax]

  db $d9, $40, $04 //flds [eax+4]
  db $d8, $42, $04 //fadds [edx+4]
  db $d9, $58, $04 //fstps [eax+4]

  db $d9, $40, $08 //flds [eax+8]
  db $d8, $42, $08 //fadds [edx+8]
  db $d9, $58, $08 //fstps [eax+8]
end;
{$else}
begin
  dest.x += src.x;
  dest.y += src.y;
  dest.z += src.z;
end;
{$endif}

operator - (const v1,v2: TPoint3D_128): TPoint3D_128;
{$ifdef CPUI386} assembler;
asm
  db $d9, $02 //flds [edx]
  db $d8, $28 //fsubrs [eax]
  db $d9, $19 //fstps [ecx]

  db $d9, $42, $04 //flds [edx+4]
  db $d8, $68, $04 //fsubrs [eax+4]
  db $d9, $59, $04 //fstps [ecx+4]

  db $d9, $42, $08 //flds [edx+8]
  db $d8, $68, $08 //fsubrs [eax+8]
  db $d9, $59, $08 //fstps [ecx+8]

  xor eax,eax
  mov [ecx+12],eax
end;
{$else}
begin
  result.x := v1.x-v2.x;
  result.y := v1.y-v2.y;
  result.z := v1.z-v2.z;
  result.t := 0;
end;
{$endif}

operator-(const v: TPoint3D_128): TPoint3D_128; inline;
begin
  result.x := -v.x;
  result.y := -v.y;
  result.z := -v.z;
  result.t := 0;
end;

operator=(const v1, v2: TPoint3D_128): boolean; inline;
begin
 result := (v1.x=v2.x) and (v1.y=v2.y) and (v1.z=v2.z);
end;

procedure ClearPoint3D_128(out v: TPoint3D_128);
{$ifdef CPUI386}
begin
 asm
   push ebx
   mov eax,v
   xor ebx,ebx
   mov [eax],ebx
   mov [eax+4],ebx
   mov [eax+8],ebx
   pop ebx
 end;
end;
{$else}
var p: pdword;
begin
  p := @v;
  p^ := 0;
  inc(p);
  p^ := 0;
  inc(p);
  p^ := 0;
end;
{$endif}

procedure ClearPoint3D_128_AlignedSSE(out v: TPoint3D_128);
{$ifdef CPUI386}
begin
 asm
   xorps xmm0,xmm0
   movaps [eax],xmm0
 end;
end;
{$else}
var p: pdword;
begin
  p := @v;
  p^ := 0;
  inc(p);
  p^ := 0;
  inc(p);
  p^ := 0;
end;
{$endif}

function IsPoint3D_128_Zero(const v: TPoint3D_128): boolean;
begin
  result := (v.x=0) and (v.y=0) and (v.z=0);
end;

operator * (const v1: TPoint3D_128; const factor: single): TPoint3D_128;
{$ifdef CPUI386} assembler;
asm
  db $d9, $00 //flds [eax]
  db $d8, $4d, $08 //fmuls [ebp+8]
  db $d9, $1a //fstps [edx]

  db $d9, $40, $04 //flds [eax+4]
  db $d8, $4d, $08 //fmuls [ebp+8]
  db $d9, $5a, $04 //fstps [edx+4]

  db $d9, $40, $08 //flds [eax+8]
  db $d8, $4d, $08 //fmuls [ebp+8]
  db $d9, $5a, $08 //fstps [edx+8]

  xor eax,eax
  mov [edx+12],eax
end;
{$else}
begin
  result.x := v1.x*factor;
  result.y := v1.y*factor;
  result.z := v1.z*factor;
  result.t := 0;
end;
{$endif}

{$ifdef CPUI386}
function DotProduct3D_128_SSE3(const v1,v2: TPoint3D_128): single; assembler;
asm
  movups xmm0, [v1]
  movups xmm1, [v2]
  mulps xmm0, xmm1

  haddps xmm0,xmm0
  haddps xmm0,xmm0
  movss [result], xmm0
end;
{$endif}

function DotProduct3D_128_NoSSE(const v1,v2: TPoint3D_128): single;
begin
  result := v1.x*v2.x + v1.y*v2.y + v1.z*v2.z;
end;

procedure Normalize3D_128_NoSSE(var v: TPoint3D_128);
var len: single;
begin
  len := DotProduct3D_128_NoSSE(v,v);
  if len = 0 then exit;
  len := 1/sqrt(len);
  v.x *= len;
  v.y *= len;
  v.z *= len;
end;

{$ifdef CPUI386}
procedure Normalize3D_128_SSE1(var v: TPoint3D_128);
var len: single;
begin
  asm
    mov eax, v
    movups xmm0, [eax]
    movaps xmm1, xmm0
    mulps xmm0, xmm0

    //mix1
    movaps xmm7, xmm0
    shufps xmm7, xmm7, $4e
    addps xmm0, xmm7
    //mix2
    movaps xmm7, xmm0
    shufps xmm7, xmm7, $11
    addps xmm0, xmm7

    movss len, xmm0
  end;
  if (len = 0) then exit;
  if len < 1e-6 then //out of bounds for SSE instruction
  begin
     len := 1/sqrt(len);
     v.x *= len;
     v.y *= len;
     v.z *= len;
  end else
  asm
    rsqrtps xmm0, xmm0
    mulps xmm0, xmm1  //apply
    mov eax, v
    movups [eax], xmm0
  end;
end;
{$endif}

{$ifdef CPUI386}
procedure Normalize3D_128_SSE3(var v: TPoint3D_128);
var len: single;
begin
  asm
    mov eax, v
    movups xmm0, [eax]
    movaps xmm1, xmm0
    mulps xmm0, xmm0

    haddps xmm0,xmm0
    haddps xmm0,xmm0

    movss len, xmm0
  end;
  if (len = 0) then exit;
  if len < 1e-6 then //out of bounds for SSE instruction
  begin
     len := 1/sqrt(len);
     v.x *= len;
     v.y *= len;
     v.z *= len;
  end else
  asm
    rsqrtps xmm0, xmm0
    mulps xmm0, xmm1  //apply
    mov eax, v
    movups [eax], xmm0
  end;
end;
{$endif}

procedure Normalize3D_128_SqLen(var v: TPoint3D_128; out SqLen: single);
var InvLen: single;
begin
  {$ifdef CPUI386}
    if UseSSE then
    begin
      asm
        mov eax, v
        movups xmm0, [eax]
        movaps xmm1, xmm0
        mulps xmm0, xmm0
      end;
      if UseSSE3 then
      asm
        haddps xmm0,xmm0
        haddps xmm0,xmm0
        movss SqLen, xmm0
      end else
      asm
        //mix1
        movaps xmm7, xmm0
        shufps xmm7, xmm7, $4e
        addps xmm0, xmm7
        //mix2
        movaps xmm7, xmm0
        shufps xmm7, xmm7, $11
        addps xmm0, xmm7
        movss SqLen, xmm0
      end;
      if SqLen = 0 then exit;
      if SqLen < 1e-6 then //out of bounds for SSE instruction
      begin
         InvLen := 1/sqrt(SqLen);
         v.x *= InvLen;
         v.y *= InvLen;
         v.z *= InvLen;
      end else
      asm
        rsqrtps xmm0, xmm0
        mulps xmm0, xmm1  //apply
        mov eax, v
        movups [eax], xmm0
      end;
    end
    else
{$endif}
    begin
      SqLen := DotProduct3D_128_NoSSE(v,v);
      if SqLen = 0 then exit;
      InvLen := 1/sqrt(SqLen);
      v.x *= InvLen;
      v.y *= InvLen;
      v.z *= InvLen;
    end;
end;

procedure VectProduct3D_128_NoSSE(const u,v: TPoint3D_128; out w: TPoint3D_128);
begin
  w.x := u.y*v.z-u.z*v.y;
  w.y := u.z*v.x-u.x*v.z;
  w.z := u.x*v.Y-u.y*v.x;
  w.t := 0;
end;

{$ifdef CPUI386}
procedure VectProduct3D_128_SSE(const u,v: TPoint3D_128; out w: TPoint3D_128); assembler;
asm
  mov eax, u
  movups xmm6, [eax]
  movaps xmm4, xmm6
  shufps xmm6, xmm6, Shift231

  mov eax, v
  movups xmm7, [eax]
  movaps xmm5,xmm7
  shufps xmm7, xmm7, Shift312

  movaps xmm3,xmm6
  mulps xmm3,xmm7

  shufps xmm4, xmm4, Shift312
  shufps xmm5, xmm5, Shift231

  mulps xmm4,xmm5
  subps xmm3,xmm4

  mov eax,w
  movups [eax], xmm3
end;
{$endif}

{ TMemoryBlockAlign128 }

{$hints off}
constructor TMemoryBlockAlign128.Create(size: integer);
{$IFDEF CPUI386}
var
  delta: cardinal;
begin
  getmem(FContainer, size+15);
  delta := cardinal(FContainer) and 15;
  if delta <> 0 then delta := 16-delta;
  FData := pbyte(FContainer)+delta;
end;
{$ELSE}
begin
  getmem(FContainer, size);
  FData := FContainer;
end;
{$ENDIF}
{$hints on}

destructor TMemoryBlockAlign128.Destroy;
begin
  freemem(FContainer);
  inherited Destroy;
end;

{$ifdef CPUI386}   {$ASMMODE ATT}
function sse3_support : boolean;

  var
     _ecx : longint;

  begin
     if cpuid_support then
       begin
          asm
             pushl %ebx
             movl $1,%eax
             cpuid
             movl %ecx,_ecx
             popl %ebx
          end;
          sse3_support:=(_ecx and 1)<>0;
       end
     else
       sse3_support:=false;
  end;
{$endif}

initialization

  {$ifdef CPUI386}
  UseSSE := is_sse_cpu and FLAG_ENABLED_SSE;
  if UseSSE then
  begin
    UseSSE2 := is_sse2_cpu;
    UseSSE3 := sse3_support;

    Add3D_Aligned := @Add3D_AlignedSSE;
    VectProduct3D_128 := @VectProduct3D_128_NoSSE; //VectProduct3D_128_SSE is slower (due to access penalty?)
    if UseSSE3 then
    begin
      Normalize3D_128 := @Normalize3D_128_SSE3;
      DotProduct3D_128 := @DotProduct3D_128_NoSSE; //DotProduct3D_128_SSE3 is slower (due to access penalty?)
    end
    else
    begin
      Normalize3D_128 := @Normalize3D_128_SSE1;
      DotProduct3D_128 := @DotProduct3D_128_NoSSE;
    end;
  end
  else
  {$endif}
  begin
    UseSSE := false;
    UseSSE2 := false;
    UseSSE3 := false;

    Add3D_Aligned := @Add3D_NoSSE;
    Normalize3D_128 := @Normalize3D_128_NoSSE;
    VectProduct3D_128 := @VectProduct3D_128_NoSSE;
    DotProduct3D_128 := @DotProduct3D_128_NoSSE;
  end;

end.

