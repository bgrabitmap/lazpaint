unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynHighlighterPas, SynEdit, Forms, Controls,
  Graphics, Dialogs, StdCtrls, BGRAVirtualScreen, BGRABitmap;

type

  { TForm1 }

  TForm1 = class(TForm)
    BGRAVirtualScreen1: TBGRAVirtualScreen;
    Button1: TButton;
    SynEdit1: TSynEdit;
    SynFreePascalSyn1: TSynFreePascalSyn;
    procedure BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    bmp: TBGRABitmap;
    idxBmp: integer;
    procedure UpdateBitmap;
  end;

var
  Form1: TForm1;

implementation

uses lpparser, lpcompiler, lputils, lpvartypes, lptypes, lpeval, lpinterpreter,
  BGRABitmapTypes, ubgralape;

{$R *.lfm}

procedure MyShowMessage{$I lape.proc}
begin
  Form1.UpdateBitmap;
  ShowMessage(PlpString(Params^[0])^);
end;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  Parser: TLapeTokenizerBase;
  Compiler: TLapeCompiler;
begin
  Parser := nil;
  Compiler := nil;
  try
    Parser := TLapeTokenizerString.Create(SynEdit1.Lines.Text);
    Compiler := TLapeCompiler.Create(Parser);

    InitializePascalScriptBasics(Compiler, [psiTypeAlias]);
    ExposeGlobals(Compiler);

    Compiler.addGlobalFunc('procedure ShowMessage(s: string);', @MyShowMessage);

    ubgralape.AddScriptSystemTypes(Compiler);
    ubgralape.AddScriptSystemFunctions(Compiler);

//    Compiler.addGlobalMethod('procedure _writeln; override;', @MyWriteLn, Form1);

//    c := LapeImportWrapper(@StupidProc, Compiler, 'function(abc: array of integer): array of integer', FFI_SYSV);
//    Compiler.addGlobalFunc('function StupidProc(abc: array of integer): array of integer', c.Func);

    if not Compiler.Compile() then
      raise Exception.Create('Error');

    try
      FreeAndNil(bmp);
      bmp := TBGRABitmap.Create(BGRAVirtualScreen1.Width,BGRAVirtualScreen1.Height);
      idxBmp:= ubgralape.RegisterBitmap(bmp);
      ubgralape.SetTargetBitmap(idxBmp);
      RunCode(Compiler.Emitter.Code);
    finally
      ubgralape.UnregisterBitmap(idxBmp);
      idxBmp := -1;
    end;
  except
    on E: Exception do
    begin
      ShowMessage(E.Message);
    end;
  end;
  If Assigned(Compiler) then
    FreeAndNil(Compiler)
  else
    FreeAndNil(Parser);
  BGRAVirtualScreen1.DiscardBitmap;
end;

procedure TForm1.BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  Bitmap.DrawCheckers(rect(0,0,Bitmap.Width,Bitmap.Height),BGRAWhite,CSSSilver);
  if Assigned(bmp) then Bitmap.PutImage(0,0,bmp,dmDrawWithTransparency);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SynEdit1.Lines.LoadFromFile('tests.pas');
  bmp := nil;
  idxBmp := -1;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(bmp);
end;

procedure TForm1.UpdateBitmap;
begin
  if (idxBmp = -1) or (bmp = nil) then exit;
  ubgralape.EnsureInvalidate(idxBmp);
  Form1.BGRAVirtualScreen1.RedrawBitmap;
end;

end.

