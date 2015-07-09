{ You can define the following compiler directives in the package options,
  in tab Compiler options, in section Other. Directives must be prefixed with
  "-d". So for example:

  - to always have RGBA pixel format, write "-dBGRABITMAP_RGBAPIXEL"
  - to always have BGRA pixel format, write "-dBGRABITMAP_BGRAPIXEL"
  - to use fpGUI toolkit, write "-dBGRABITMAP_USE_FPGUI" }
{$IFDEF LCL}{$DEFINE BGRABITMAP_USE_LCL}{$ENDIF}
{$IFDEF BGRABITMAP_USE_FPGUI}{$UNDEF BGRABITMAP_USE_LCL}{$ENDIF}

{ Extensions of LCL to be used. Add // at the beginning of the line
  to comment them if the functions are not available }
{$DEFINE BGRABITMAP_USE_LCL12} { Use functions of Lazarus 1.2 }
//{$DEFINE BGRABITMAP_USE_LCL15} { Use functions of Lazarus 1.5 }
