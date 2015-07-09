unit ubgralape;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes, lptypes, lpcompiler;

function RegisterBitmap(ABitmap: TBGRABitmap): integer;
procedure UnregisterBitmap(AIndex: integer);
procedure EnsureInvalidate(AIndex: integer);
procedure SetTargetBitmap(AIndex: integer);
procedure AddScriptSystemTypes(Compiler: TLapeCompiler);
procedure AddScriptSystemFunctions(Compiler: TLapeCompiler);
procedure FreeBitmaps;

implementation

uses FileUtil, Graphics, GraphType, BGRAPolygon, BGRAFillInfo;

var
  bitmaps: array of record
    Bitmap: TBGRABitmap;
    Registered: boolean;
    Invalidated: boolean;
    LockedCount: NativeInt;
  end;
  target: TBGRABitmap;
  targetIndex: integer;

function NewBitmapEntry: integer;
var i: integer;
begin
  for i:= 0 to high(bitmaps) do
    if bitmaps[i].Bitmap = nil then
      begin
        result := i;
        bitmaps[i].LockedCount:= 0;
        exit;
      end;
  result := length(bitmaps);
  setlength(bitmaps,length(bitmaps)*2+1);
  bitmaps[result].LockedCount:= 0;
end;

procedure FreeBitmap(AIndex: integer);
begin
  if (AIndex >= 0) and (AIndex < length(bitmaps)) then
    if not bitmaps[AIndex].Registered then
      begin
        if bitmaps[AIndex].LockedCount > 0 then
          raise exception.Create('Bitmap is locked');
        FreeAndNil(bitmaps[AIndex].Bitmap);
        bitmaps[AIndex].Invalidated:= false;
      end;
end;

function RegisterBitmap(ABitmap: TBGRABitmap): integer;
begin
  result := NewBitmapEntry;
  bitmaps[result].Bitmap := ABitmap;
  bitmaps[result].Invalidated := false;
  bitmaps[result].Registered := true;
end;

procedure UnregisterBitmap(AIndex: integer);
begin
  if (AIndex >= 0) and (AIndex < length(bitmaps)) then
    begin
      EnsureInvalidate(AIndex);
      if not bitmaps[AIndex].Registered then
        raise Exception.Create('This bitmap has not been registered');
      if target = bitmaps[AIndex].Bitmap then
        begin
          target := nil;
          targetIndex := -1;
        end;
      bitmaps[AIndex].Bitmap := nil;
      bitmaps[AIndex].Registered := false;
    end;
end;

procedure EnsureInvalidate(AIndex: integer);
begin
  if (AIndex >= 0) and (AIndex < length(bitmaps)) then
    begin
      if bitmaps[AIndex].Invalidated then
        begin
          bitmaps[AIndex].Bitmap.InvalidateBitmap;
          bitmaps[AIndex].Invalidated := false;
        end;
    end;
end;

procedure WillInvalidateBitmap(AIndex: integer);
begin
  if (AIndex >= 0) and (AIndex < length(bitmaps)) then
    Bitmaps[AIndex].Invalidated := true;
end;

procedure SetTargetBitmap(AIndex: integer);
begin
  if (AIndex < 0) or (AIndex >= length(bitmaps)) or (bitmaps[AIndex].Bitmap = nil) then
    raise exception.create('Bitmap does not exist');
  target := bitmaps[AIndex].Bitmap;
  targetIndex := AIndex;
end;

function GetBitmap(AIndex: integer): TBGRABitmap;
begin
  if (AIndex < 0) or (AIndex >= length(bitmaps)) or (Bitmaps[AIndex].Bitmap = nil) then
    raise exception.Create('Bitmap does not exist');
  result := Bitmaps[AIndex].Bitmap;
end;

function GetScriptSystemInlineFunctions: string; forward;

///////////////////////////// Function implementation ///////////////////////////

{$I basic_functions.inc}
{$I basic_geometry_functions.inc}
{$I extended_geometry_functions.inc}
{$I text_functions.inc}
{$I color_functions.inc}

/////////////////////////// Function list /////////////////////////////////////////////

procedure AddScriptSystemFunctions(Compiler: TLapeCompiler);
begin
  RegisterBasicFunctions(Compiler);
  RegisterBasicGeometryFunctions(Compiler);
  RegisterExtendedGeometryFunctions(Compiler);
  RegisterTextFunctions(Compiler);
  RegisterColorFunctions(Compiler);
  Compiler.addDelayedCode(GetScriptSystemInlineFunctions, false,true);
end;

procedure FreeBitmaps;
var i: integer;
begin
  for i := 0 to High(bitmaps) do
    if (bitmaps[i].Bitmap <> nil) and not bitmaps[i].Registered then
      begin
        bitmaps[i].LockedCount := 0;
        FreeBitmap(i);
      end;
end;

////////////////////////////// Load script system //////////////////////////////

var
  scriptSystemFunctions,scriptSystemTypes: TStringList;

function GetScriptSystemInlineFunctions: string;
var i: integer;
begin
  result := LineEnding;
  for i := 0 to scriptSystemFunctions.Count-1 do
    result += scriptSystemFunctions[i]+LineEnding;
  textAlignment:= taLeftJustify;
end;

procedure AddScriptSystemTypes(Compiler: TLapeCompiler);
var line: string;
  i,idxEq: integer;
begin
  for i := 0 to scriptSystemTypes.Count-1 do
  begin
    line := scriptSystemTypes[i];
    idxEq := pos('=',line);
    if idxEq <> 0 then
      Compiler.addGlobalType(trim(copy(line,idxEq+1,length(line)-idxEq)),trim(copy(line,1,idxEq-1)));
  end;
end;

procedure LoadScriptSystem;
var
  scriptSystem: TStringList;
  i: integer;
  dest: TStringList;
begin
  scriptSystemFunctions := TStringList.Create;
  scriptSystemTypes := TStringList.Create;
  dest := nil;

  scriptSystem := TStringList.Create;
  scriptSystem.LoadFromFile('bgralapesys.pas');

  for i := 0 to scriptSystem.Count-1 do
  begin
    if CompareText(Trim(scriptSystem[i]),'implementation') = 0 then
      dest := scriptSystemFunctions else
    if CompareText(Trim(scriptSystem[i]),'type') = 0 then
      dest := scriptSystemTypes else
    if CompareText(Trim(scriptSystem[i]),'end.') = 0 then break
    else
      if Assigned(dest) then dest.Add(scriptSystem[i]);
  end;

  scriptSystem.Free;
end;

procedure FreeScriptSystem;
begin
  scriptSystemTypes.Free;
  scriptSystemFunctions.Free;
end;

initialization

  LoadScriptSystem;
  Randomize;

finalization

  FreeScriptSystem;

end.

