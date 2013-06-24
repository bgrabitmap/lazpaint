{$ifdef cpux86_64}
mov rax,v
movups xmm1,[rax]
{$else}
mov eax,v
movups xmm1,[eax]
{$endif}
