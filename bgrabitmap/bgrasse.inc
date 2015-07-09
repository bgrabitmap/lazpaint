{$IFDEF SSE_LOADV}
  {$UNDEF SSE_LOADV}
  {$ifdef cpux86_64}
  mov rax,v
  movups xmm1,[rax]
  {$else}
  mov eax,v
  movups xmm1,[eax]
  {$endif}
{$ELSE}
  {$IFDEF SSE_SAVEV}
    {$UNDEF SSE_SAVEV}
    {$ifdef cpux86_64}
    mov rax,v
    movups [rax],xmm1
    {$else}
    mov eax,v
    movups [eax],xmm1
    {$endif}  
  {$ELSE}
    {$IFDEF CPUI386}
      {$DEFINE BGRASSE_AVAILABLE}
    {$ENDIF}
    {$IFDEF cpux86_64}
      {$DEFINE BGRASSE_AVAILABLE}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}