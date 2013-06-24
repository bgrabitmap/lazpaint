{$ifdef cpux86_64}
mov rax,v
movups [rax],xmm1
{$else}
mov eax,v
movups [eax],xmm1
{$endif}
