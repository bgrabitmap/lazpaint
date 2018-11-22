program vectoredit;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, umain, 
  LCVectororiginal, LCVectorialFill, LCVectorshapes, LCVectorPolyShapes,
  uvectorclipboard,
  LResources, LCToolbars
  { you can add units after this };

{$R *.res}

begin
  {$I vectorimages.lrs}
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

