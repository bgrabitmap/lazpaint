program testbgrafunc;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, umain, utest1, utest2, utest3, utest4, utest5, utest6, utest7, utest8,
  utest9, utest10, utest11, utest14, utest15, utest16, utest17, utest18,
  utest19, utestback, utestpacrect, etpackage, utest22, utest23, utest24,
  utexture, utest25, utest26, ucube3d, utest27, utore3d, utest31, utest32,
  utest33;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFMain, FMain);
  Application.Run;
end.

