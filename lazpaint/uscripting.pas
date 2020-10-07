// SPDX-License-Identifier: GPL-3.0-only
unit UScripting;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmapTypes, UScriptType;

type
  TVariableSet = class;
  TScriptResult = (srOk, srInvalidParameters, srCancelledByUser, srException, srFunctionNotDefined);

function ScriptResultToStr(AResult: TScriptResult; AFunction: string): string;

type
  TScriptFunction = function(AVars: TVariableSet): TScriptResult of object;
  TScriptVariableReference = record
    variableSet: TVariableSet;
    variableType: TScriptVariableType;
    variableIndex: NativeInt;
  end;

  { TVariableSet }

  TVariableSet = class
  private
    FScalars: array of TScalarVariable;
    FNbScalars: NativeInt;
    FStrings: array of record name: string; value: string; end;
    FNbStrings: NativeInt;

    FScalarLists: array of record
                             name: string;
                             varType: TScriptVariableType;
                             list: pointer;
                             size, count: NativeInt;
                           end;
    FNbScalarLists: NativeInt;
    FBoolLists: array of record name: string; list: TBits; count: NativeInt; end;
    FNbBoolLists: NativeInt;
    FStrLists: array of record name: string; list: array of string; count: NativeInt; end;
    FNbStrLists: NativeInt;

    FSubsets: array of record name: string; value: TVariableSet; end;
    FNbSubsets: NativeInt;
    FFunctionName: string;
    function GetBooleanByName(const AName: string): boolean;
    function GetCount: NativeInt;
    function GetFloatByName(const AName: string): double;
    function GetGuidByName(const AName: string): TGuid;
    function GetIntegerByName(const AName: string): TScriptInteger;
    function GetPixelByName(const AName: string): TBGRAPixel;
    function GetPoint2DByName(const AName: string): TPointF;
    function GetPoint3DByName(const AName: string): TPoint3D;
    function GetStringByName(const AName: string): string;
    function GetSubsetByName(const AName: string): TVariableSet;
    function GetListByName(const AName: string): string;
    function GetVariablesAsString: string;
    function GetVarName(AIndex: integer): string;
    procedure SetBooleanByName(const AName: string; AValue: boolean);
    procedure SetFloatByName(const AName: string; AValue: double);
    procedure SetGuidByName(const AName: string; const AValue: TGuid);
    procedure SetIntegerByName(const AName: string; AValue: TScriptInteger);
    procedure SetListByName(const AName: string; AValue: string);
    procedure SetPixelByName(const AName: string; AValue: TBGRAPixel);
    procedure SetPoint2DByName(const AName: string; AValue: TPointF);
    procedure SetPoint3DByName(const AName: string; AValue: TPoint3D);
    procedure SetStringByName(const AName: string; AValue: string);
    procedure SetSubSetByName(const AName: string; AValue: TVariableSet);
    function GetStrListAsString(AIndex: NativeInt): string;
    function GetBoolListAsString(AIndex: NativeInt): string;
    function GetScalarListAsString(AIndex: NativeInt): string;
    function AddScalar(const AName: string; AType: TScriptVariableType): boolean;
    function AddScalarList(const AName: string; AType: TScriptVariableType): TScriptVariableReference;
  public
    FunctionRedirected, IgnoreResult: boolean;
    constructor Create(AFunctionName: string);
    constructor Create(AFunctionName: string; AVariablesAsString: string);
    function LoadFromVariablesAsString(AVariablesAsString: string): TInterpretationErrors;
    function Remove(const AName: string): boolean;
    function Remove(var ADest: TScriptVariableReference): boolean;
    destructor Destroy; override;
    function AddFloat(const AName: string; AValue: double): boolean;
    function AddInteger(const AName: string; AValue: TScriptInteger): boolean;
    function AddPoint(const AName: string; AValue: TPoint3D): boolean; overload;
    function AddPoint(const AName: string; AValue: TPointF): boolean; overload;
    function AddBoolean(const AName: string; AValue: boolean): boolean;
    function AddPixel(const AName: string; const AValue: TBGRAPixel): boolean;
    function AddString(const AName: string; AValue: string): boolean;
    function AddGuid(const AName: string; const AValue: TGuid): boolean;
    function AddSubset(const AName: string; AValue: TVariableSet): boolean;
    function AddSubset(const AName: string): TVariableSet;
    function AddList(const AName: string; AListExpr: string): TInterpretationErrors;
    function AddBooleanList(const AName: string): TScriptVariableReference;
    function AddIntegerList(const AName: string): TScriptVariableReference;
    function AddFloatList(const AName: string): TScriptVariableReference;
    function AddPointList(const AName: string): TScriptVariableReference;
    function AddPixelList(const AName: string): TScriptVariableReference;
    function AddStringList(const AName: string): TScriptVariableReference;
    function AddGuidList(const AName: string): TScriptVariableReference;
    function GetVariable(const AName: string): TScriptVariableReference;
    function IsDefined(const AName: string): boolean;
    class procedure ClearList(const ADest: TScriptVariableReference); static;
    class function AppendFloat(const ADest: TScriptVariableReference; AValue: double): boolean; overload; static;
    class function AssignFloat(const ADest: TScriptVariableReference; AValue: double): boolean; overload; static;
    class function AssignFloatAt(const ADest: TScriptVariableReference; AIndex: NativeInt; AValue: double): boolean; static;
    class function AppendInteger(const ADest: TScriptVariableReference; AValue: TScriptInteger): boolean; overload; static;
    class function AssignInteger(const ADest: TScriptVariableReference; AValue: TScriptInteger): boolean; overload; static;
    class function AssignIntegerAt(const ADest: TScriptVariableReference; AIndex: NativeInt; AValue: TScriptInteger): boolean; static;
    class function AppendPoint(const ADest: TScriptVariableReference; const AValue: TPoint3D): boolean; overload; static;
    class function AssignPoint(const ADest: TScriptVariableReference; const AValue: TPoint3D): boolean; overload; static;
    class function AssignPointAt(const ADest: TScriptVariableReference; AIndex: NativeInt; const AValue: TPoint3D): boolean; overload; static;
    class function AppendPoint(const ADest: TScriptVariableReference; const AValue: TPointF): boolean; overload; static;
    class function AssignPoint(const ADest: TScriptVariableReference; const AValue: TPointF): boolean; overload; static;
    class function AssignPointAt(const ADest: TScriptVariableReference; AIndex: NativeInt; const AValue: TPointF): boolean; overload; static;
    class function AppendBoolean(const ADest: TScriptVariableReference; AValue: boolean): boolean; overload; static;
    class function AssignBoolean(const ADest: TScriptVariableReference; AValue: boolean): boolean; overload; static;
    class function AppendString(const ADest: TScriptVariableReference; AValue: string): boolean; overload; static;
    class function AssignString(const ADest: TScriptVariableReference; AValue: string): boolean; overload; static;
    class function AppendGuid(const ADest: TScriptVariableReference; const AValue: TGuid): boolean; overload; static;
    class function AssignGuid(const ADest: TScriptVariableReference; const AValue: TGuid): boolean; overload; static;
    class function AppendPixel(const ADest: TScriptVariableReference; const AValue: TBGRAPixel): boolean; overload; static;
    class function AssignPixel(const ADest: TScriptVariableReference; const AValue: TBGRAPixel): boolean; overload; static;
    class function AssignList(const ADest: TScriptVariableReference; AListExpr: string): TInterpretationErrors; static;
    class function AssignVariable(const ADest, ASource: TScriptVariableReference): boolean; static;
    class function IsReferenceDefined(const AReference: TScriptVariableReference): boolean; static;
    class function IsList(const AReference: TScriptVariableReference): boolean; static;
    class function IsSubSet(const AReference: TScriptVariableReference): boolean; static;
    class function GetFloat(const ASource: TScriptVariableReference) : double; static;
    class function GetInteger(const ASource: TScriptVariableReference) : TScriptInteger; static;
    class function GetPoint2D(const ASource: TScriptVariableReference) : TPointF; static;
    class function GetPoint3D(const ASource: TScriptVariableReference) : TPoint3D; static;
    class function GetBoolean(const ASource: TScriptVariableReference) : boolean; static;
    class function GetString(const ASource: TScriptVariableReference) : string; static;
    class function GetGuid(const ASource: TScriptVariableReference) : TGuid; static;
    class function GetPixel(const ASource: TScriptVariableReference) : TBGRAPixel; static;
    class function GetSubset(const ASource: TScriptVariableReference) : TVariableSet; static;
    class function GetList(const ASource: TScriptVariableReference) : string; static;
    class function GetListCount(const ASource: TScriptVariableReference) : NativeInt; static;
    class function GetFloatAt(const ASource: TScriptVariableReference; AIndex: NativeInt) : double; static;
    class function GetIntegerAt(const ASource: TScriptVariableReference; AIndex: NativeInt) : TScriptInteger; static;
    class function GetPoint2DAt(const ASource: TScriptVariableReference; AIndex: NativeInt) : TPointF; static;
    class function GetPoint3DAt(const ASource: TScriptVariableReference; AIndex: NativeInt): TPoint3D; static;
    class function GetPoint3DAt(const ASource: TScriptVariableReference; AIndex: NativeInt; ADefaultZ: single): TPoint3D; static;
    class function GetBooleanAt(const ASource: TScriptVariableReference; AIndex: NativeInt) : boolean; static;
    class function GetStringAt(const ASource: TScriptVariableReference; AIndex: NativeInt) : string; static;
    class function GetGuidAt(const ASource: TScriptVariableReference; AIndex: NativeInt) : TGuid; static;
    class function GetPixelAt(const ASource: TScriptVariableReference; AIndex: NativeInt) : TBGRAPixel; static;
    class function RemoveAt(const ASource: TScriptVariableReference; AIndex: NativeInt) : boolean; static;
    function Duplicate: TVariableSet;
    function CopyValuesTo(ASet: TVariableSet): boolean;
    property FunctionName: string read FFunctionName;
    property Count: NativeInt read GetCount;
    property VariableName[AIndex: integer]: string read GetVarName;
    property VariablesAsString: string read GetVariablesAsString;
    property Floats[const AName: string]: double read GetFloatByName write SetFloatByName;
    property Integers[const AName: string]: TScriptInteger read GetIntegerByName write SetIntegerByName;
    property Points2D[const AName: string]: TPointF read GetPoint2DByName write SetPoint2DByName;
    property Points3D[const AName: string]: TPoint3D read GetPoint3DByName write SetPoint3DByName;
    property Booleans[const AName: string]: boolean read GetBooleanByName write SetBooleanByName;
    property Strings[const AName: string]: string read GetStringByName write SetStringByName;
    property Pixels[const AName: string]: TBGRAPixel read GetPixelByName write SetPixelByName;
    property Subsets[const AName: string]: TVariableSet read GetSubsetByName write SetSubSetByName;
    property Lists[const AName: string]: string read GetListByName write SetListByName;
    property Guids[const AName: string]: TGuid read GetGuidByName write SetGuidByName;
  end;

  { TScriptContext }

  TScriptContext = class
  private
    FScriptFunctions: array of record
      name: string;
      handler: TScriptFunction;
    end;
    FNbScriptFunctions: NativeInt;
    FRecording: boolean;
    FRecordingFunctionParameters: TVariableSet;
    FRecordedScript: string;
    FCallScriptFunctionLevel: NativeInt;
    FScriptFunctionExceptionHandler: TScriptFunctionExceptionHandler;
    function GetRecordingFunctionParameters: TVariableSet;
    procedure SetRecording(AValue: boolean);
    procedure ClearRecordedScript;
  public
    constructor Create;
    procedure RegisterScriptFunction(AName: string; AHandler: TScriptFunction; ARegister: boolean=True);
    procedure UnregisterScriptFunction(AName: string; AHandler: TScriptFunction);
    function GetScriptFunctionHandler(AName: string) : TScriptFunction;
    function CallScriptFunction(AName: string; ARedirection: boolean = false; AParameters: TVariableSet = nil) : TScriptResult; overload;
    function CallScriptFunction(AParameters: TVariableSet = nil; ARedirection: boolean = false) : TScriptResult; overload;
    property RecordingFunctionParameters: TVariableSet read GetRecordingFunctionParameters;
    property Recording: boolean read FRecording write SetRecording;
    property RecordedScript: string read FRecordedScript;
    property OnFunctionException: TScriptFunctionExceptionHandler read FScriptFunctionExceptionHandler write FScriptFunctionExceptionHandler;
  end;

implementation

uses Dialogs, UResourceStrings;

function ScriptResultToStr(AResult: TScriptResult; AFunction: string): string;
begin
  case AResult of
    srOk: result := rsOkay;
    srInvalidParameters: result := rsInvalidParameters;
    srCancelledByUser: result := rsCancelledByUser;
    srException: result := rsException;
    srFunctionNotDefined: result := StringReplace(rsFunctionNotDefined,'%1','"'+AFunction+'"',[]);
    else result := rsInternalError;
  end;
end;

{ TScriptContext }

procedure TScriptContext.SetRecording(AValue: boolean);
begin
  if FRecording=AValue then Exit;
  FRecording:=AValue;
  if AValue = true then
    ClearRecordedScript;
end;

function TScriptContext.GetRecordingFunctionParameters: TVariableSet;
begin
  result := FRecordingFunctionParameters;
end;

procedure TScriptContext.ClearRecordedScript;
begin
  FRecordedScript := '';
end;

constructor TScriptContext.Create;
begin
  FNbScriptFunctions := 0;
  FCallScriptFunctionLevel := 0;
end;

procedure TScriptContext.RegisterScriptFunction(AName: string;
  AHandler: TScriptFunction; ARegister: boolean=True);
begin
  if not ARegister then
  begin
    UnregisterScriptFunction(AName, AHandler);
    exit;
  end;
  if length(FScriptFunctions) = FNbScriptFunctions then
  begin
    if length(FScriptFunctions) = 0 then setlength(FScriptFunctions,4)
    else setlength(FScriptFunctions, FNbScriptFunctions*2);
  end;
  FScriptFunctions[FNbScriptFunctions].name := AName;
  FScriptFunctions[FNbScriptFunctions].handler := AHandler;
  inc(FNbScriptFunctions);
end;

procedure TScriptContext.UnregisterScriptFunction(AName: string;
  AHandler: TScriptFunction);
var i,j: NativeInt;
begin
  for i := FNbScriptFunctions-1 downto 0 do
    if (CompareText(FScriptFunctions[i].name,AName) = 0) and
      (FScriptFunctions[i].handler = AHandler) then
    begin
      for j := i to FNbScriptFunctions-2 do
        FScriptFunctions[j] := FScriptFunctions[j+1];
      dec(FNbScriptFunctions);
      exit;
    end;
end;

function TScriptContext.GetScriptFunctionHandler(AName: string): TScriptFunction;
var i: NativeInt;
begin
  for i := FNbScriptFunctions-1 downto 0 do //get latests registered functions first
    if CompareText(FScriptFunctions[i].name,AName) = 0 then
    begin
      result := FScriptFunctions[i].handler;
      exit;
    end;
  result := nil;
end;

function TScriptContext.CallScriptFunction(AName: string; ARedirection: boolean = false; AParameters: TVariableSet = nil): TScriptResult;
var f: TScriptFunction;
  v: TVariableSet;
  i: NativeInt;
  IsRecording: boolean;
  previousFunctionParameters, currentFunctionParameters: TVariableSet;
begin
  for i := FNbScriptFunctions-1 downto 0 do //get latests registered functions first
    if CompareText(FScriptFunctions[i].name,AName) = 0 then
    begin
      IsRecording := Recording and ((FCallScriptFunctionLevel = 0) or (Assigned(FRecordingFunctionParameters) and ARedirection));
      f := FScriptFunctions[i].handler;
      if Assigned(AParameters) then v := AParameters else
      begin
        v := TVariableSet.Create(FScriptFunctions[i].name);
        v.IgnoreResult:= true;
      end;
      previousFunctionParameters := FRecordingFunctionParameters;
      if IsRecording then
      begin
        currentFunctionParameters := v.Duplicate;
        if Assigned(FRecordingFunctionParameters) then FRecordingFunctionParameters.FunctionRedirected := true;
        FRecordingFunctionParameters := currentFunctionParameters;
      end else
        FRecordingFunctionParameters := nil;
      inc(FCallScriptFunctionLevel);
      try
        result := f(v);
      except
        on ex:Exception do
        begin
          if Assigned(FScriptFunctionExceptionHandler) then FScriptFunctionExceptionHandler(AName, ex);
          result := srException;
        end;
      end;
      dec(FCallScriptFunctionLevel);
      if not Assigned(AParameters) then v.Free;
      FRecordingFunctionParameters := previousFunctionParameters;
      if IsRecording then
      begin
        if Recording and (result = srOk) and not currentFunctionParameters.FunctionRedirected then //if recording has not been interrupted
        begin
          FRecordedScript := FRecordedScript+FScriptFunctions[i].name;
          if currentFunctionParameters.Count <> 0 then
            FRecordedScript := FRecordedScript+' '+currentFunctionParameters.VariablesAsString;
          FRecordedScript:= FRecordedScript+LineEnding;
        end;
        currentFunctionParameters.Free;
      end;
      exit;
    end;
  result := srFunctionNotDefined
end;

function TScriptContext.CallScriptFunction(AParameters: TVariableSet;
  ARedirection: boolean): TScriptResult;
begin
  result := CallScriptFunction(AParameters.FunctionName,ARedirection,AParameters);
end;

{ TVariableSet }

function TVariableSet.GetCount: NativeInt;
var
  i: Integer;
begin
  result := 0;
  for i := 0 to FNbScalars-1 do if FScalars[i].name <> '' then inc(result);
  for i := 0 to FNbStrings-1 do if FStrings[i].name <> '' then inc(result);
  for i := 0 to FNbBoolLists-1 do if FBoolLists[i].name <> '' then inc(result);
  for i := 0 to FNbScalarLists-1 do if FScalarLists[i].name <> '' then inc(result);
  for i := 0 to FNbStrLists-1 do if FStrLists[i].name <> '' then inc(result);
  for i := 0 to FNbSubsets-1 do if FSubsets[i].name <> '' then inc(result);
end;

function TVariableSet.GetBooleanByName(const AName: string): boolean;
begin
  result := GetBoolean(GetVariable(AName));
end;

function TVariableSet.GetFloatByName(const AName: string): double;
begin
  result := GetFloat(GetVariable(AName));
end;

function TVariableSet.GetGuidByName(const AName: string): TGuid;
begin
  result := ScriptStrToGuid(Strings[AName]);
end;

function TVariableSet.GetIntegerByName(const AName: string): TScriptInteger;
begin
  result := GetInteger(GetVariable(AName));
end;

function TVariableSet.GetPixelByName(const AName: string): TBGRAPixel;
begin
  result := GetPixel(GetVariable(AName));
end;

function TVariableSet.GetPoint2DByName(const AName: string): TPointF;
begin
  result := GetPoint2D(GetVariable(AName));
end;

function TVariableSet.GetPoint3DByName(const AName: string): TPoint3D;
begin
  result := GetPoint3D(GetVariable(AName));
end;

function TVariableSet.GetStringByName(const AName: string): string;
begin
  result := GetString(GetVariable(AName));
end;

function TVariableSet.GetSubsetByName(const AName: string): TVariableSet;
begin
  result := GetSubset(GetVariable(AName));
end;

function TVariableSet.GetListByName(const AName: string): string;
begin
  result := GetList(GetVariable(AName));
end;

function TVariableSet.GetVariablesAsString: string;
var i: NativeInt;
begin
  result := '';
  for i := 0 to FNbStrings-1 do
  begin
    if length(result)>0 then result := result+', ';
    result := result+FStrings[i].name+VariableDefinitionToken+' '+ScriptQuote(FStrings[i].value);
  end;
  for i := 0 to FNbScalars-1 do
  begin
    if length(result)>0 then result := result+', ';
    with FScalars[i] do
      result := result+name+VariableDefinitionToken+' '+ScalarToStr(varType, valueBytes);
  end;

  for i := 0 to FNbStrLists-1 do
  begin
    if length(result)>0 then result := result+', ';
    result := result+FStrLists[i].name+VariableDefinitionToken+' '+GetStrListAsString(i);
  end;
  for i := 0 to FNbScalarLists-1 do
  begin
    if length(result)>0 then result := result+', ';
    result := result+FScalarLists[i].name+VariableDefinitionToken+' '+GetScalarListAsString(i);
  end;
  for i := 0 to FNbBoolLists-1 do
  begin
    if length(result)>0 then result := result+', ';
    result := result+FBoolLists[i].name+VariableDefinitionToken+' '+GetBoolListAsString(i);
  end;

  for i := 0 to FNbSubsets-1 do
  begin
    if length(result)>0 then result := result+', ';
    result := result+FSubsets[i].name+VariableDefinitionToken+' { '+FSubsets[i].value.VariablesAsString+ ' }';
  end;

end;

function TVariableSet.GetVarName(AIndex: integer): string;
var
  i: Integer;
begin
  if AIndex < 0 then raise exception.Create('Index out of bounds');

  for i := 0 to FNbScalars-1 do
    if FScalars[i].name <> '' then
    begin
      if AIndex = 0 then exit(FScalars[AIndex].name)
      else dec(AIndex);
    end;
  for i := 0 to FNbStrings-1 do
    if FStrings[i].name <> '' then
    begin
      if AIndex = 0 then exit(FStrings[AIndex].name)
      else dec(AIndex);
    end;
  for i := 0 to FNbBoolLists-1 do
    if FBoolLists[i].name <> '' then
    begin
      if AIndex = 0 then exit(FBoolLists[AIndex].name)
      else dec(AIndex);
    end;
  for i := 0 to FNbScalarLists-1 do
    if FScalarLists[i].name <> '' then
    begin
      if AIndex = 0 then exit(FScalarLists[AIndex].name)
      else dec(AIndex);
    end;
  for i := 0 to FNbStrLists-1 do
    if FStrLists[i].name <> '' then
    begin
      if AIndex = 0 then exit(FStrLists[AIndex].name)
      else dec(AIndex);
    end;
  for i := 0 to FNbSubsets-1 do
    if FSubsets[i].name <> '' then
    begin
      if AIndex = 0 then exit(FSubsets[AIndex].name)
      else dec(AIndex);
    end;

  raise exception.Create('Index out of bounds');
end;

function TVariableSet.LoadFromVariablesAsString(AVariablesAsString: string
  ): TInterpretationErrors;
var varName: string;

  procedure ParseSubset(var cur: integer; expr: string);
  var inSubset: integer;
    subsetStr: string;
    s: TVariableSet;
    start: integer;
    inQuote: char;
    escaping: boolean;
  begin
    if cur > length(expr) then exit;
    start := cur;
    inQuote := #0;
    inSubset := 0;
    escaping := true;
    repeat
      if inQuote <> #0 then
      begin
        if not escaping then
        begin
          if expr[cur] = inQuote then inQuote:= #0 else
          if expr[cur] = '\' then escaping := true;
        end else
          escaping := false;
      end else
      begin
        if expr[cur] = '{' then
        begin
          inc(inSubset);
          if inSubset = 1 then start := cur+1;
        end
        else if expr[cur] = '}' then
        begin
          dec(inSubset);
          if inSubset = 0 then break;
        end
        else if expr[cur] in StringDelimiters then inQuote:= expr[cur];
      end;
      inc(cur);
    until cur > length(expr);
    if inQuote <> #0 then result += [ieEndingQuoteNotFound];
    subsetStr := copy(expr,start,cur-start);
    s := TVariableSet.Create('');
    result += s.LoadFromVariablesAsString(subsetStr);
    AddSubSet(varName, s);
    if (cur <= length(expr)) and (expr[cur] = '}') then inc(cur);
  end;

  procedure ParseList(var cur: integer; expr: string);
  var inBracket: integer;
    listStr: string;
    start: integer;
    inQuote: char;
    escaping: boolean;
  begin
    if cur > length(expr) then exit;
    start := cur;
    inQuote := #0;
    inBracket := 0;
    escaping := false;
    repeat
      if inQuote <> #0 then
      begin
        if not escaping then
        begin
          if expr[cur] = inQuote then inQuote:= #0 else
          if expr[cur] = '\' then escaping := true;
        end else
          escaping := false;
      end else
      begin
        if expr[cur] in['(','['] then
        begin
          inc(inBracket);
          if inBracket = 1 then
            if expr[cur] <> '[' then result += [ieUnexpectedOpeningBracketKind];
        end
        else if expr[cur] in[']',')'] then
        begin
          dec(inBracket);
          if inBracket = 0 then
          begin
            if expr[cur] <> ']' then result += [ieUnexpectedClosingBracketKind];
            inc(cur);
            break;
          end;
        end
        else if expr[cur] in StringDelimiters then inQuote:= expr[cur];
      end;
      inc(cur);
    until cur > length(expr);
    if inQuote <> #0 then result += [ieEndingQuoteNotFound];
    listStr := copy(expr,start,cur-start);
    AddList(varName, listStr);
  end;

var
  cur: integer;
  idxEq: integer;
  litteral: TParsedLitteral;
begin
  result := [];
  idxEq := pos(VariableDefinitionToken,AVariablesAsString);
  while idxEq <> 0 do
  begin
    varName := trim(copy(AVariablesAsString,1,idxEq-1));
    if (length(varName)>=2) and (varName[1]='''') and (varName[length(varName)]='''') then
      varName := UnescapeString(Copy(varName,2,length(varName)-2));
    cur := idxEq+2;
    while (cur <= length(AVariablesAsString)) and (AVariablesAsString[cur] in IgnoredWhitespaces) do inc(cur);
    if (cur <= length(AVariablesAsString)) and (AVariablesAsString[cur]='{') then
      ParseSubset(cur,AVariablesAsString)
    else
    if (cur <= length(AVariablesAsString)) and (AVariablesAsString[cur]='[') then
      ParseList(cur,AVariablesAsString)
    else
    begin
      litteral:= ParseLitteral(cur,AVariablesAsString,result);
      case litteral.valueType of
        svtInteger: if not AddInteger(varName,litteral.valueInt) then result := result + [ieDuplicateIdentifier];
        svtFloat: if not AddFloat(varName,litteral.valueFloat) then result := result + [ieDuplicateIdentifier];
        svtPoint: if not AddPoint(varName,litteral.valuePoint) then result := result + [ieDuplicateIdentifier];
        svtString: if not AddString(varName, litteral.valueStr) then result := result + [ieDuplicateIdentifier];
        svtBoolean: if not AddBoolean(varName,litteral.valueBool) then result := result + [ieDuplicateIdentifier];
        svtPixel: if not AddPixel(varName,litteral.valuePixel) then result := result + [ieDuplicateIdentifier];
      end;
    end;
    if (cur < length(AVariablesAsString)) and (AVariablesAsString[cur] = ',') then inc(cur);
    delete(AVariablesAsString,1,cur-1);
    idxEq := pos(VariableDefinitionToken,AVariablesAsString);
  end;
end;

function TVariableSet.Remove(const AName: string): boolean;
var
  v: TScriptVariableReference;
begin
  v := GetVariable(AName);
  if not IsReferenceDefined(v) then result := false
  else result := Remove(v);
end;

function TVariableSet.Remove(var ADest: TScriptVariableReference): boolean;
begin
  if ADest.variableType in ScriptScalarTypes then
  begin
    FScalars[ADest.variableIndex].name:= '';
    FScalars[ADest.variableIndex].varType:= svtUndefined;
  end else
  if ADest.variableType in ScriptScalarListTypes then
  begin
    FScalarLists[ADest.variableIndex].name:= '';
    FScalarLists[ADest.variableIndex].varType:= svtUndefined;
    FScalarLists[ADest.variableIndex].count := 0;
    FScalarLists[ADest.variableIndex].size := 0;
    ReallocMem(FScalarLists[ADest.variableIndex].list, 0);
  end else
  if ADest.variableType = svtString then
  begin
    FStrings[ADest.variableIndex].name:= '';
    FStrings[ADest.variableIndex].value:= '';
  end else
  if ADest.variableType = svtStrList then
  begin
    FStrLists[ADest.variableIndex].name:= '';
    FStrLists[ADest.variableIndex].list:= nil;
    FStrLists[ADest.variableIndex].count:= 0;
  end else
  if ADest.variableType = svtBoolList then
  begin
    FBoolLists[ADest.variableIndex].name:= '';
    FreeAndNil(FBoolLists[ADest.variableIndex].list);
    FBoolLists[ADest.variableIndex].count:= 0;
  end else
  if IsSubSet(ADest) then
  begin
    FSubsets[ADest.variableIndex].name:= '';
    FreeAndNil(FSubsets[ADest.variableIndex].value);
  end else
    exit(false);
  ADest.variableType:= svtUndefined;
  ADest.variableIndex:= -1;
  ADest.variableSet := nil;
  result := true;
end;

procedure TVariableSet.SetBooleanByName(const AName: string; AValue: boolean);
var v: TScriptVariableReference;
begin
  v := GetVariable(AName);
  if IsReferenceDefined(v) then AssignBoolean(v,AValue) else AddBoolean(AName,AValue);
end;

procedure TVariableSet.SetFloatByName(const AName: string; AValue: double);
var v: TScriptVariableReference;
begin
  v := GetVariable(AName);
  if IsReferenceDefined(v) then AssignFloat(v,AValue) else AddFloat(AName,AValue);
end;

procedure TVariableSet.SetGuidByName(const AName: string; const AValue: TGuid);
var
  guidStr: String;
begin
  guidStr := LowerCase(GUIDToString(AValue));
  if (length(guidStr)>0) and (guidStr[1]='{') and (guidStr[length(guidStr)]='}') then
    guidStr := copy(guidStr,2,length(guidStr)-2);
  Strings[AName] := guidStr;
end;

procedure TVariableSet.SetIntegerByName(const AName: string; AValue: TScriptInteger);
var v: TScriptVariableReference;
begin
  v := GetVariable(AName);
  if IsReferenceDefined(v) then AssignInteger(v,AValue) else AddInteger(AName,AValue);
end;

procedure TVariableSet.SetListByName(const AName: string; AValue: string);
var v: TScriptVariableReference;
begin
  v := GetVariable(AName);
  if IsReferenceDefined(v) then AssignList(v,AValue) else AddList(AName,AValue);
end;

procedure TVariableSet.SetPixelByName(const AName: string; AValue: TBGRAPixel);
var v: TScriptVariableReference;
begin
  v := GetVariable(AName);
  if IsReferenceDefined(v) then AssignPixel(v,AValue) else AddPixel(AName,AValue);
end;

procedure TVariableSet.SetPoint2DByName(const AName: string; AValue: TPointF);
var v: TScriptVariableReference;
begin
  v := GetVariable(AName);
  if IsReferenceDefined(v) then AssignPoint(v,AValue) else AddPoint(AName,AValue);
end;

procedure TVariableSet.SetPoint3DByName(const AName: string; AValue: TPoint3D);
var v: TScriptVariableReference;
begin
  v := GetVariable(AName);
  if IsReferenceDefined(v) then AssignPoint(v,AValue) else AddPoint(AName,AValue);
end;

procedure TVariableSet.SetStringByName(const AName: string; AValue: string);
var v: TScriptVariableReference;
begin
  v := GetVariable(AName);
  if IsReferenceDefined(v) then AssignString(v,AValue) else AddString(AName,AValue);
end;

procedure TVariableSet.SetSubSetByName(const AName: string; AValue: TVariableSet);
var v: TScriptVariableReference;
begin
  if not Assigned(AValue) then exit;
  v := GetVariable(AName);
  if IsReferenceDefined(v) then
  begin
    if v.variableType <> svtSubset then exit;
    AValue.CopyValuesTo(v.variableSet.FSubsets[v.variableIndex].value)
  end else AddSubset(AName,AValue.Duplicate);
end;

function TVariableSet.GetStrListAsString(AIndex: NativeInt): string;
var j: NativeInt;
begin
  if FStrLists[AIndex].count = 0 then result := EmptyListExpression[svtStrList] else
  begin
    result := '[';
    result := result + ScriptQuote(FStrLists[AIndex].list[0]);
    for j := 1 to FStrLists[AIndex].count-1 do
      result := result + ', ' + ScriptQuote(FStrLists[AIndex].list[j]);
    result := result+']';
  end;
end;

function TVariableSet.GetBoolListAsString(AIndex: NativeInt): string;
var j: NativeInt;
begin
  if FBoolLists[AIndex].count = 0 then result := EmptyListExpression[svtBoolList] else
  begin
    result := '[';
    result := result + BoolToStr(FBoolLists[AIndex].list[0],TrueToken,FalseToken);
    for j := 1 to FBoolLists[AIndex].count-1 do
      result := result + ', ' + BoolToStr(FBoolLists[AIndex].list[j],TrueToken,FalseToken);
    result := result+']';
  end;
end;

function TVariableSet.GetScalarListAsString(AIndex: NativeInt): string;
var j: NativeInt; p: PByte;
  listType: TScriptVariableType;
  scalarSize: PtrInt;
  scalarType: TScriptVariableType;
begin
  listType := FScalarLists[AIndex].varType;
  if FScalarLists[AIndex].count = 0 then result := EmptyListExpression[listType] else
  begin
    p := FScalarLists[AIndex].list;
    scalarSize := ScalarListElementSize[listType];
    scalarType := ListElementType[listType];
    result := '['+ScalarToStr(scalarType, p^);
    for j := 1 to FScalarLists[AIndex].count-1 do
    begin
      inc(p, scalarSize);
      result := result + ', ' + ScalarToStr(scalarType, p^);
    end;
    result := result+']';
  end;
end;

function TVariableSet.AddScalar(const AName: string; AType: TScriptVariableType
  ): boolean;
begin
  if IsDefined(AName) or (AName = '') then
  begin
    result := false;
    exit;
  end;
  if length(FScalars) = FNbScalars then
  begin
    if length(FScalars) = 0 then setlength(FScalars,4)
    else setlength(FScalars, length(FScalars)*2);
  end;
  FScalars[FNbScalars].name := AName;
  FScalars[FNbScalars].varType := AType;
  inc(FNbScalars);
  result := true;
end;

function TVariableSet.AddScalarList(const AName: string;
  AType: TScriptVariableType): TScriptVariableReference;
begin
  if IsDefined(AName) or (AName = '') then
  begin
    result.variableSet := nil;
    result.variableType := svtUndefined;
    result.variableIndex := -1;
    exit;
  end;
  if length(FScalarLists) = FNbScalarLists then
  begin
    if length(FScalarLists) = 0 then setlength(FScalarLists,4)
    else setlength(FScalarLists, length(FScalarLists)*2);
  end;
  FScalarLists[FNbScalarLists].name := AName;
  FScalarLists[FNbScalarLists].varType := AType;
  FScalarLists[FNbScalarLists].count := 0;
  FScalarLists[FNbScalarLists].list := nil;
  FScalarLists[FNbScalarLists].size := 0;
  result.variableSet := self;
  result.variableType := AType;
  result.variableIndex := FNbScalarLists;
  inc(FNbScalarLists);
end;

constructor TVariableSet.Create(AFunctionName: string);
begin
  FFunctionName:= AFunctionName;
  FunctionRedirected:= false;
end;

constructor TVariableSet.Create(AFunctionName: string;
  AVariablesAsString: string);
begin
  FFunctionName:= AFunctionName;
  FunctionRedirected:= false;
  LoadFromVariablesAsString(AVariablesAsString);
end;

destructor TVariableSet.Destroy;
var i: NativeInt;
begin
  for i := 0 to FNbSubsets-1 do
    FreeAndNil(FSubsets[i].value);
  for i := 0 to FNbBoolLists-1 do
    FreeAndNil(FBoolLists[i].list);
  for i := 0 to FNbScalarLists-1 do
    FreeMem(FScalarLists[i].list);
  inherited Destroy;
end;

function TVariableSet.AddFloat(const AName: string; AValue: double): boolean;
begin
  result := AddScalar(AName, svtFloat);
  if result then FScalars[FNbScalars-1].valueFloat := AValue;
end;

function TVariableSet.AddInteger(const AName: string; AValue: TScriptInteger): boolean;
begin
  result := AddScalar(AName, svtInteger);
  if result then FScalars[FNbScalars-1].valueInt := AValue;
end;

function TVariableSet.AddPoint(const AName: string; AValue: TPoint3D): boolean;
begin
  result := AddScalar(AName, svtPoint);
  if result then FScalars[FNbScalars-1].valuePoint := AValue;
end;

function TVariableSet.AddPoint(const AName: string; AValue: TPointF): boolean;
begin
  result := AddPoint(AName, Point3D(AValue.X, AValue.Y, EmptySingle));
end;

function TVariableSet.AddBoolean(const AName: string; AValue: boolean): boolean;
begin
  result := AddScalar(AName, svtBoolean);
  if result then FScalars[FNbScalars-1].valueBool := AValue;
end;

function TVariableSet.AddPixel(const AName: string; const AValue: TBGRAPixel): boolean;
begin
  result := AddScalar(AName, svtPixel);
  if result then FScalars[FNbScalars-1].valuePix := AValue;
end;

function TVariableSet.AddString(const AName: string; AValue: string): boolean;
begin
  if IsDefined(AName) or (AName = '') then
  begin
    result := false;
    exit;
  end;
  if length(FStrings) = FNbStrings then
  begin
    if length(FStrings) = 0 then setlength(FStrings,4)
    else setlength(FStrings, length(FStrings)*2);
  end;
  FStrings[FNbStrings].name := AName;
  FStrings[FNbStrings].value := AValue;
  inc(FNbStrings);
  result := true;
end;

function TVariableSet.AddGuid(const AName: string; const AValue: TGuid): boolean;
begin
  result := AddString(AName, ScriptGuidToStr(AValue));
end;

function TVariableSet.AddSubset(const AName: string; AValue: TVariableSet
  ): boolean;
begin
  if IsDefined(AName) or (AName = '') then
  begin
    result := false;
    exit;
  end;
  if length(FSubsets) = FNbSubsets then
  begin
    if length(FSubsets) = 0 then setlength(FSubsets,4)
    else setlength(FSubsets, length(FSubsets)*2);
  end;
  FSubsets[FNbSubsets].name := AName;
  FSubsets[FNbSubsets].value := AValue;
  inc(FNbSubsets);
  result := true;
end;

function TVariableSet.AddSubset(const AName: string): TVariableSet;
begin
  if IsDefined(AName) then
    result := nil else
  begin
    result := TVariableSet.Create('');
    AddSubset(AName,result);
  end;
end;

function TVariableSet.AddList(const AName: string; AListExpr: string): TInterpretationErrors;
var listType: TScriptVariableType;
  ref: TScriptVariableReference;
begin
  if IsDefined(AName) or (AName = '') then
  begin
    result := [ieDuplicateIdentifier];
    exit;
  end;
  listType := ParseListType(AListExpr);
  if listType = svtUndefined then
  begin
    result := [ieUnknownListType];
    exit;
  end;
  if listType in ScriptScalarListTypes then ref := AddScalarList(AName, listType) else
  case listType of
    svtBoolList: ref := AddBooleanList(AName);
    svtStrList: ref := AddStringList(AName);
  else
    raise exception.Create('Unknown list type');
  end;
  result := AssignList(ref, AListExpr);
end;

function TVariableSet.AddBooleanList(const AName: string
  ): TScriptVariableReference;
begin
  if IsDefined(AName) or (AName = '') then
  begin
    result.variableSet := nil;
    result.variableType := svtUndefined;
    result.variableIndex := -1;
    exit;
  end;
  if length(FBoolLists) = FNbBoolLists then
  begin
    if length(FBoolLists) = 0 then setlength(FBoolLists,4)
    else setlength(FBoolLists, length(FBoolLists)*2);
  end;
  FBoolLists[FNbBoolLists].name := AName;
  FBoolLists[FNbBoolLists].list := TBits.Create;
  FBoolLists[FNbBoolLists].count := 0;
  result.variableSet := self;
  result.variableType := svtBoolList;
  result.variableIndex := FNbBoolLists;
  inc(FNbBoolLists);
end;

function TVariableSet.AddIntegerList(const AName: string
  ): TScriptVariableReference;
begin
  result := AddScalarList(AName, svtIntList);
end;

function TVariableSet.AddFloatList(const AName: string
  ): TScriptVariableReference;
begin
  result := AddScalarList(AName, svtFloatList);
end;

function TVariableSet.AddPointList(const AName: string
  ): TScriptVariableReference;
begin
  result := AddScalarList(AName, svtPointList);
end;

function TVariableSet.AddPixelList(const AName: string
  ): TScriptVariableReference;
begin
  result := AddScalarList(AName, svtPixList);
end;

function TVariableSet.AddStringList(const AName: string
  ): TScriptVariableReference;
begin
  if IsDefined(AName) or (AName = '') then
  begin
    result.variableSet := nil;
    result.variableType := svtUndefined;
    result.variableIndex := -1;
    exit;
  end;
  if length(FStrLists) = FNbStrLists then
  begin
    if length(FStrLists) = 0 then setlength(FStrLists,4)
    else setlength(FStrLists, length(FStrLists)*2);
  end;
  FStrLists[FNbStrLists].name := AName;
  FStrLists[FNbStrLists].count := 0;
  result.variableSet := self;
  result.variableType := svtStrList;
  result.variableIndex := FNbStrLists;
  inc(FNbStrLists);
end;

function TVariableSet.AddGuidList(const AName: string
  ): TScriptVariableReference;
begin
  result := AddStringList(AName);
end;

function TVariableSet.GetVariable(const AName: string): TScriptVariableReference;
var i: NativeInt;
begin
  if AName <> '' then
  begin
    for i := 0 to FNbScalars-1 do
      if CompareText(AName, FScalars[i].name)= 0 then
      begin
        result.variableSet := self;
        result.variableType := FScalars[i].varType;
        result.variableIndex := i;
        exit;
      end;
    for i := 0 to FNbStrLists-1 do
      if CompareText(AName, FStrLists[i].name)= 0 then
      begin
        result.variableSet := self;
        result.variableType := svtStrList;
        result.variableIndex := i;
        exit;
      end;
    for i := 0 to FNbBoolLists-1 do
      if CompareText(AName, FBoolLists[i].name)= 0 then
      begin
        result.variableSet := self;
        result.variableType := svtBoolList;
        result.variableIndex := i;
        exit;
      end;
    for i := 0 to FNbScalarLists-1 do
      if CompareText(AName, FScalarLists[i].name)= 0 then
      begin
        result.variableSet := self;
        result.variableType := FScalarLists[i].varType;
        result.variableIndex := i;
        exit;
      end;
    for i := 0 to FNbStrings-1 do
      if CompareText(AName, FStrings[i].name)= 0 then
      begin
        result.variableSet := self;
        result.variableType := svtString;
        result.variableIndex := i;
        exit;
      end;
    for i := 0 to FNbSubsets-1 do
      if CompareText(AName, FSubsets[i].name)= 0 then
      begin
        result.variableSet := self;
        result.variableType := svtSubset;
        result.variableIndex := i;
        exit;
      end;
  end;
  result.variableSet := nil;
  result.variableType := svtUndefined;
  result.variableIndex := -1;
end;

function TVariableSet.IsDefined(const AName: string): boolean;
begin
  result := IsReferenceDefined(GetVariable(AName));
end;

class procedure TVariableSet.ClearList(const ADest: TScriptVariableReference);
begin
  if ADest.variableSet <> nil then
  begin
    if ADest.variableType in ScriptScalarListTypes then
      with ADest.variableSet.FScalarLists[ADest.variableIndex] do
      begin
        count := 0;
        size := 0;
        freemem(list);
        list := nil;
      end
    else
    case ADest.variableType of
      svtBoolList: with ADest.variableSet.FBoolLists[ADest.variableIndex] do
                   begin count := 0; list.Size := 0; end;
      svtStrList: with ADest.variableSet.FStrLists[ADest.variableIndex] do
                  begin count := 0; list := nil; end;
    end;
  end;
end;

class function TVariableSet.AppendFloat(const ADest: TScriptVariableReference;
  AValue: double): boolean;
begin
  result := false;
  if ADest.variableSet = nil then exit;
  case ADest.variableType of
    svtFloatList:
      with ADest.variableSet.FScalarLists[ADest.variableIndex] do
      begin
        if count = ListMaxLength then exit;
        if size = count then
        begin
          if count = 0 then size := 4 else size := count*2;
          ReAllocMem(list,size*sizeof(double));
        end;
        (PDouble(list)+count)^ := AValue;
        inc(count);
        result := true;
      end;
    svtStrList: result := AppendString(ADest, FloatToStrUS(AValue));
    svtPixList: result := AppendPixel(ADest, FloatToPixel(AValue));
  end;
end;

class function TVariableSet.AssignFloat(const ADest: TScriptVariableReference;
  AValue: double): boolean;
begin
  if ADest.variableSet = nil then
  begin
    result := false;
    exit;
  end;
  case ADest.variableType of
    svtFloat: ADest.variableSet.FScalars[ADest.variableIndex].valueFloat := AValue;
    svtString: ADest.variableSet.FStrings[ADest.variableIndex].value := FloatToStrUS(AValue);
    svtPixel: ADest.variableSet.FScalars[ADest.variableIndex].valuePix := FloatToPixel(AValue);
    else
    begin
      result := false;
      exit;
    end;
  end;
  result := true;
end;

class function TVariableSet.AssignFloatAt(
  const ADest: TScriptVariableReference; AIndex: NativeInt; AValue: double
  ): boolean;
begin
  result := false;
  if (ADest.variableSet = nil) or (AIndex < 0) then exit;
  if not (Adest.variableType in ScriptVariableListTypes) then exit;
  if AIndex >= GetListCount(ADest) then exit;

  case ADest.variableType of
    svtFloatList: with ADest.variableSet.FScalarLists[ADest.variableIndex] do
                    (PDouble(list)+AIndex)^ := AValue;
    svtStrList: with ADest.variableSet.FStrLists[ADest.variableIndex] do
                    list[AIndex] := FloatToStrUS(AValue);
    svtPixList: with ADest.variableSet.FScalarLists[ADest.variableIndex] do
                    (PBGRAPixel(list)+AIndex)^ := FloatToPixel(AValue);
    else exit;
  end;
  result := true;
end;

class function TVariableSet.AppendInteger(
  const ADest: TScriptVariableReference; AValue: TScriptInteger): boolean;
begin
  result := false;
  if ADest.variableSet = nil then exit;
  case ADest.variableType of
    svtIntList:
      with ADest.variableSet.FScalarLists[ADest.variableIndex] do
      begin
        if count = ListMaxLength then exit;
        if size = count then
        begin
          if count = 0 then size := 4 else size := count*2;
          ReAllocMem(list,size*sizeof(TScriptInteger));
        end;
        (PScriptInteger(list)+count)^ := AValue;
        inc(count);
        result := true;
      end;
    svtFloatList: result := AppendFloat(ADest, AValue);
    svtStrList: result := AppendString(ADest, IntToStr(AValue));
    svtPixList: result := AppendPixel(ADest, IntToPixel(AValue));
  end;
end;

class function TVariableSet.AssignInteger(
  const ADest: TScriptVariableReference; AValue: TScriptInteger): boolean;
begin
  if ADest.variableSet = nil then
  begin
    result := false;
    exit;
  end;
  case ADest.variableType of
    svtInteger: ADest.variableSet.FScalars[ADest.variableIndex].valueInt := AValue;
    svtFloat: ADest.variableSet.FScalars[ADest.variableIndex].valueFloat := AValue;
    svtString: ADest.variableSet.FStrings[ADest.variableIndex].value := IntToStr(AValue);
    svtPixel: ADest.variableSet.FScalars[ADest.variableIndex].valuePix := IntToPixel(AValue);
    else
    begin
      result := false;
      exit;
    end;
  end;
  result := true;
end;

class function TVariableSet.AssignIntegerAt(
  const ADest: TScriptVariableReference; AIndex: NativeInt; AValue: TScriptInteger
  ): boolean;
begin
  result := false;
  if (ADest.variableSet = nil) or (AIndex < 0) then exit;
  if not (Adest.variableType in ScriptVariableListTypes) then exit;
  if AIndex >= GetListCount(ADest) then exit;
  case ADest.variableType of
    svtIntList: with ADest.variableSet.FScalarLists[ADest.variableIndex] do
                    (PScriptInteger(list)+AIndex)^ := AValue;
    svtFloatList: with ADest.variableSet.FScalarLists[ADest.variableIndex] do
                    (PDouble(list)+AIndex)^ := AValue;
    svtStrList: with ADest.variableSet.FStrLists[ADest.variableIndex] do
                    list[AIndex] := IntToStr(AValue);
    svtPixList: with ADest.variableSet.FScalarLists[ADest.variableIndex] do
                    (PBGRAPixel(list)+AIndex)^ := IntToPixel(AValue);
    else exit;
  end;
  result := true;
end;

class function TVariableSet.AppendPoint(const ADest: TScriptVariableReference;
  const AValue: TPoint3D): boolean;
begin
  result := false;
  if ADest.variableSet = nil then exit;
  case ADest.variableType of
    svtPointList:
      with ADest.variableSet.FScalarLists[ADest.variableIndex] do
      begin
        if count = ListMaxLength then exit;
        if size = count then
        begin
          if count = 0 then size := 4 else size := count*2;
          ReAllocMem(list,size*sizeof(TPoint3D));
        end;
        (PPoint3D(list)+count)^ := AValue;
        inc(count);
        result := true;
      end;
    svtStrList: result := AppendString(ADest, ScalarToStr(svtPoint, AValue));
  end;
end;

class function TVariableSet.AssignPoint(const ADest: TScriptVariableReference;
  const AValue: TPoint3D): boolean;
begin
  if ADest.variableSet = nil then
  begin
    result := false;
    exit;
  end;
  case ADest.variableType of
    svtPoint: ADest.variableSet.FScalars[ADest.variableIndex].valuePoint := AValue;
    svtString: ADest.variableSet.FStrings[ADest.variableIndex].value := ScalarToStr(svtPoint, AValue);
    else
    begin
      result := false;
      exit;
    end;
  end;
  result := true;
end;

class function TVariableSet.AssignPointAt(
  const ADest: TScriptVariableReference; AIndex: NativeInt;
  const AValue: TPoint3D): boolean;
begin
  result := false;
  if (ADest.variableSet = nil) or (AIndex < 0) then exit;
  if not (Adest.variableType in ScriptVariableListTypes) then exit;
  if AIndex >= GetListCount(ADest) then exit;
  case ADest.variableType of
    svtPointList: with ADest.variableSet.FScalarLists[ADest.variableIndex] do
                    (PPoint3D(list)+AIndex)^ := AValue;
    svtStrList: with ADest.variableSet.FStrLists[ADest.variableIndex] do
                    list[AIndex] := ScalarToStr(svtPoint, AValue);
    else exit;
  end;
  result := true;
end;

class function TVariableSet.AppendPoint(const ADest: TScriptVariableReference;
  const AValue: TPointF): boolean;
begin
  result := AppendPoint(ADest, Point3D(AValue.X, AValue.y, EmptySingle));
end;

class function TVariableSet.AssignPoint(const ADest: TScriptVariableReference;
  const AValue: TPointF): boolean;
begin
  result := AssignPoint(ADest, Point3D(AValue.X, AValue.y, EmptySingle));
end;

class function TVariableSet.AssignPointAt(
  const ADest: TScriptVariableReference; AIndex: NativeInt;
  const AValue: TPointF): boolean;
begin
  result := AssignPointAt(ADest, AIndex, Point3D(AValue.X, AValue.y, EmptySingle));
end;

class function TVariableSet.AppendBoolean(
  const ADest: TScriptVariableReference; AValue: boolean): boolean;
begin
  result := false;
  if ADest.variableSet = nil then exit;
  case ADest.variableType of
    svtBoolList:
      with ADest.variableSet.FBoolLists[ADest.variableIndex] do
      begin
        if count = ListMaxLength*8 then exit;
        if list.Size = count then
        begin
          if count = 0 then list.Size := 4*8 else
            list.Size := count*2;
        end;
        list.Bits[count] := AValue;
        inc(count);
      end;
    svtPixList: if AValue then AppendPixel(ADest, BGRAWhite) else
      AppendPixel(ADest, BGRABlack);
  end;
  result := true;
end;

class function TVariableSet.AssignBoolean(
  const ADest: TScriptVariableReference; AValue: boolean): boolean;
begin
  if ADest.variableSet = nil then
  begin
    result := false;
    exit;
  end;
  case ADest.variableType of
    svtBoolean: ADest.variableSet.FScalars[ADest.variableIndex].valueBool := AValue;
    svtPixel:
      begin
        if AValue then ADest.variableSet.FScalars[ADest.variableIndex].valuePix := BGRAWhite else
          ADest.variableSet.FScalars[ADest.variableIndex].valuePix := BGRABlack;
      end;
    else
    begin
      result := false;
      exit;
    end;
  end;
  result := true;
end;

class function TVariableSet.AppendString(const ADest: TScriptVariableReference;
  AValue: string): boolean;
var
  errPos: integer;
  missing,error: boolean;
  parsedInt: NativeInt;
  parsedFloat: double;
  parsedColor: TBGRAPixel;
begin
  result := false;
  if ADest.variableSet = nil then exit;
  case ADest.variableType of
    svtStrList:
      with ADest.variableSet.FStrLists[ADest.variableIndex] do
      begin
        if count = ListMaxLength then exit;
        if length(list) = count then
        begin
          if count = 0 then setlength(list,4) else
          setlength(list,count*2);
        end;
        list[count] := AValue;
        inc(count);
        result := true;
      end;
    svtIntList:
      begin
        val(AValue, parsedInt, errPos);
        if errPos <> 0 then exit;
        result := AppendInteger(ADest, parsedInt);
      end;
    svtFloatList:
      begin
        val(AValue, parsedFloat, errPos);
        if errPos <> 0 then exit;
        result := AppendFloat(ADest, parsedFloat);
      end;
    svtPixList:
      begin
        parsedColor := BGRABlack;
        TryStrToBGRA(AValue, parsedColor, missing, error);
        if missing or error then exit;
        result := AppendPixel(ADest, parsedColor);
      end;
  end;
end;

class function TVariableSet.AssignString(
  const ADest: TScriptVariableReference; AValue: string): boolean;
var
  errPos: integer;
  error: boolean;
  parsedInt: NativeInt;
  parsedFloat: double;
  parsedColor: TBGRAPixel;
begin
  if ADest.variableSet = nil then
  begin
    result := false;
    exit;
  end;
  case ADest.variableType of
    svtString: ADest.variableSet.FStrings[ADest.variableIndex].value := AValue;
    svtInteger:
      begin
        val(AValue, parsedInt, errPos);
        if errPos = 0 then
          ADest.variableSet.FScalars[ADest.variableIndex].valueInt := parsedInt
        else
        begin
          result := false;
          exit;
        end;
      end;
    svtFloat:
      begin
        val(AValue, parsedFloat, errPos);
        if errPos = 0 then
          ADest.variableSet.FScalars[ADest.variableIndex].valueFloat := parsedFloat
        else
        begin
          result := false;
          exit;
        end;
      end;
    svtPixel:
      begin
        parsedColor := PartialStrToBGRA(AValue, ADest.variableSet.FScalars[ADest.variableIndex].valuePix, error);
        if error then
        begin
          result := false;
          exit;
        end;
        ADest.variableSet.FScalars[ADest.variableIndex].valuePix := parsedColor;
      end;
    else
    begin
      result := false;
      exit;
    end;
  end;
  result := true;
end;

class function TVariableSet.AppendGuid(const ADest: TScriptVariableReference;
  const AValue: TGuid): boolean;
begin
  result := AppendString(ADest, ScriptGuidToStr(AValue));
end;

class function TVariableSet.AssignGuid(const ADest: TScriptVariableReference;
  const AValue: TGuid): boolean;
begin
  result := AssignString(ADest, ScriptGuidToStr(AValue));
end;

class function TVariableSet.AppendPixel(const ADest: TScriptVariableReference;
  const AValue: TBGRAPixel): boolean;
begin
  result := false;
  if ADest.variableSet = nil then exit;
  case ADest.variableType of
    svtPixList:
      with ADest.variableSet.FScalarLists[ADest.variableIndex] do
      begin
        if count = ListMaxLength then exit;
        if size = count then
        begin
          if count = 0 then size := 4 else size := count*2;
          ReAllocMem(list,size*sizeof(TBGRAPixel));
        end;
        (PBGRAPixel(list)+count)^ := AValue;
        inc(count);
        result := true;
      end;
    svtStrList: result := AppendString(ADest, BGRAToStr(AValue));
  end;
end;

class function TVariableSet.AssignPixel(
  const ADest: TScriptVariableReference; const AValue: TBGRAPixel
  ): boolean;
begin
  if ADest.variableSet = nil then
  begin
    result := false;
    exit;
  end;
  case ADest.variableType of
    svtPixel: ADest.variableSet.FScalars[ADest.variableIndex].valuePix := AValue;
    svtString: ADest.variableSet.FStrings[ADest.variableIndex].value := BGRAToStr(AValue);
    else
    begin
      result := false;
      exit;
    end;
  end;
  result := true;
end;

class function TVariableSet.AssignList(const ADest: TScriptVariableReference;
  AListExpr: string): TInterpretationErrors;
var
  tilde,expectingValue: boolean;
  inQuote: char;
  inPar: integer;
  escaping: boolean;
  start,cur: integer;

  procedure AppendValue(AValue: string);
  var cur: integer;
    litteral: TParsedLitteral;
  begin
    if tilde then exit;
    cur := 1;
    litteral := ParseLitteral(cur,AValue, result);
    case litteral.valueType of
      svtBoolean: AppendBoolean(ADest, litteral.valueBool);
      svtInteger: AppendInteger(ADest, litteral.valueInt);
      svtFloat: AppendFloat(ADest, litteral.valueFloat);
      svtPoint: AppendPoint(ADest, litteral.valuePoint);
      svtString: AppendString(ADest, litteral.valueStr);
      svtPixel: AppendPixel(ADest, litteral.valuePixel);
    end;
  end;

begin
  ClearList(ADest);
  result := [];
  AListExpr:= trim(AListExpr);
  if (length(AListExpr)>0) and (AListExpr[1]='[') then
  begin
    if not (AListExpr[length(AListExpr)] = ']') then
    begin
      result += [ieClosingBracketNotFound];
      cur := 2;
    end else
    begin
      AListExpr := copy(AListExpr,2,length(AListExpr)-2);
      cur := 1;
    end;
  end else
    cur := 1;
  tilde := false;
  inQuote:= #0;
  inPar := 0;
  escaping := false;
  start := 0;
  expectingValue := false;
  while cur <= length(AListExpr) do
  begin
    if inQuote <> #0 then
    begin
      if not escaping then
      begin
        if AListExpr[cur]=inQuote then inQuote:= #0 else
        if AListExpr[cur]='\' then escaping := true;
      end else
        escaping := false;
    end else
    if (start = 0) and (AListExpr[cur]='~') then tilde := true else
    begin
      if (start = 0) and not (AListExpr[cur] in IgnoredWhitespaces) then start := cur;
      if AListExpr[cur] in StringDelimiters then inQuote:= AListExpr[cur] else
      if AListExpr[cur] = '(' then inc(inPar) else
      if AListExpr[cur] = ')' then
      begin
        if inPar > 0 then dec(inPar)
        else include(result, ieTooManyClosingBrackets);
      end else
      if (AListExpr[cur]=',') and (inPar = 0) then
      begin
        if start = 0 then result += [ieMissingValue]
        else
        begin
          AppendValue(copy(AListExpr,start,cur-start));
          start := 0;
        end;
        tilde := false;
        expectingValue := true;
      end;
    end;
    inc(cur);
  end;
  if start <> 0 then AppendValue(copy(AListExpr,start,cur-start)) else
  if expectingValue then result += [ieMissingValue]
end;

class function TVariableSet.AssignVariable(const ADest,
  ASource: TScriptVariableReference): boolean;
var i,sourceCount: NativeInt;
begin
  if ASource.variableSet = nil then
  begin
    result := false;
    exit;
  end;
  if ADest.variableType in ScriptVariableListTypes then
  begin
    if not (ASource.variableType in ScriptVariableListTypes) then
    begin
      result := false;
      exit;
    end;
    result := true;
    ClearList(ADest);
    sourceCount := GetListCount(ASource);
    case ASource.variableType of
      svtBoolList: for i := 0 to sourceCount-1 do AppendBoolean(ADest, GetBooleanAt(ASource,i));
      svtIntList: for i := 0 to sourceCount-1 do AppendInteger(ADest, GetIntegerAt(ASource,i));
      svtFloatList: for i := 0 to sourceCount-1 do AppendFloat(ADest, GetFloatAt(ASource,i));
      svtPointList: for i := 0 to sourceCount-1 do AppendPoint(ADest, GetPoint3DAt(ASource,i));
      svtPixList: for i := 0 to sourceCount-1 do AppendPixel(ADest, GetPixelAt(ASource,i));
      svtStrList: for i := 0 to sourceCount-1 do AppendString(ADest, GetStringAt(ASource,i));
    end;
  end;
  if ADest.variableType = ASource.variableType then //no conversion
  begin
    case ASource.variableType of
      svtBoolean, svtFloat, svtInteger, svtPixel, svtPoint:
        ADest.variableSet.FScalars[ADest.variableIndex].valueBytes := ASource.variableSet.FScalars[ASource.variableIndex].valueBytes;
      svtString: ADest.variableSet.FStrings[ADest.variableIndex].value := ASource.variableSet.FStrings[ASource.variableIndex].value;
      svtSubset: ADest.variableSet.FSubsets[ASource.variableIndex].value.CopyValuesTo(ASource.variableSet.FSubsets[ASource.variableIndex].value);
      else
      begin
        result := false;
        exit;
      end;
    end;
    result := true;
    exit;
  end;
  case ASource.variableType of
    svtBoolean: AssignBoolean(ADest, ASource.variableSet.FScalars[ASource.variableIndex].valueBool);
    svtFloat: AssignFloat(ADest, ASource.variableSet.FScalars[ASource.variableIndex].valueFloat);
    svtInteger: AssignInteger(ADest, ASource.variableSet.FScalars[ASource.variableIndex].valueInt);
    svtPoint: AssignPoint(ADest, ASource.variableSet.FScalars[ASource.variableIndex].valuePoint);
    svtPixel: AssignPixel(ADest, ASource.variableSet.FScalars[ASource.variableIndex].valuePix);
    svtString: AssignString(ADest, ASource.variableSet.FStrings[ASource.variableIndex].value);
    else
    begin
      result := false;
      exit;
    end;
  end;
  result := true;
end;

class function TVariableSet.IsReferenceDefined(
  const AReference: TScriptVariableReference): boolean;
begin
  result := AReference.variableIndex <> -1;
end;

class function TVariableSet.IsList(const AReference: TScriptVariableReference
  ): boolean;
begin
  result := AReference.variableType in ScriptVariableListTypes;
end;

class function TVariableSet.IsSubSet(const AReference: TScriptVariableReference
  ): boolean;
begin
  result := AReference.variableType = svtSubset;
end;

class function TVariableSet.GetFloat(const ASource: TScriptVariableReference
  ): double;
var {%H-}errPos: integer;
begin
  if ASource.variableSet <> nil then
  begin
    case ASource.variableType of
      svtFloat: begin
        result := ASource.variableSet.FScalars[ASource.variableIndex].valueFloat;
        exit;
      end;
      svtInteger: begin
        result := ASource.variableSet.FScalars[ASource.variableIndex].valueInt;
        exit;
      end;
      svtPixel: begin
        result := PixelToInt(ASource.variableSet.FScalars[ASource.variableIndex].valuePix);
        exit;
      end;
      svtBoolean: begin
        result := integer(ASource.variableSet.FScalars[ASource.variableIndex].valueBool);
        exit;
      end;
      svtString: begin
        val(ASource.variableSet.FStrings[ASource.variableIndex].value, result, errPos);
        exit;
      end;
    end;
  end;
  result := 0;
end;

class function TVariableSet.GetInteger(const ASource: TScriptVariableReference
  ): TScriptInteger;
var {%H-}errPos: integer;
begin
  if ASource.variableSet <> nil then
  begin
    case ASource.variableType of
      svtInteger: begin
        result := ASource.variableSet.FScalars[ASource.variableIndex].valueInt;
        exit;
      end;
      svtFloat: begin
        result := round(ASource.variableSet.FScalars[ASource.variableIndex].valueFloat);
        exit;
      end;
      svtBoolean: begin
        result := integer(ASource.variableSet.FScalars[ASource.variableIndex].valueBool);
        exit;
      end;
      svtPixel: begin
        result := PixelToInt(ASource.variableSet.FScalars[ASource.variableIndex].valuePix);
        exit;
      end;
      svtString: begin
        val(ASource.variableSet.FStrings[ASource.variableIndex].value, result, errPos);
        exit;
      end;
    end;
  end;
  result := 0;
end;

class function TVariableSet.GetPoint2D(const ASource: TScriptVariableReference
  ): TPointF;
begin
  with GetPoint3D(ASource) do
    result := PointF(x,y);
end;

class function TVariableSet.GetPoint3D(const ASource: TScriptVariableReference
  ): TPoint3D;
begin
  if ASource.variableSet <> nil then
  begin
    case ASource.variableType of
      svtPoint: begin
        result := ASource.variableSet.FScalars[ASource.variableIndex].valuePoint;
        exit;
      end;
    end;
  end;
  result := Point3D(0,0,EmptySingle);
end;

class function TVariableSet.GetBoolean(const ASource: TScriptVariableReference
  ): boolean;
begin
  if ASource.variableSet <> nil then
  begin
    case ASource.variableType of
      svtInteger: begin
        result := ASource.variableSet.FScalars[ASource.variableIndex].valueInt<>0;
        exit;
      end;
      svtBoolean: begin
        result := ASource.variableSet.FScalars[ASource.variableIndex].valueBool;
        exit;
      end;
    end;
  end;
  result := false;
end;

class function TVariableSet.GetString(const ASource: TScriptVariableReference): string;
begin
  result := '';
  if ASource.variableSet <> nil then
  begin
    if ASource.variableType in ScriptScalarTypes then
      result := ScalarToStr(ASource.variableType, ASource.variableSet.FScalars[ASource.variableIndex].valueBytes)
    else if ASource.variableType in ScriptScalarListTypes then
      result := ASource.variableSet.GetScalarListAsString(ASource.variableIndex)
    else
    case ASource.variableType of
      svtString: result := ASource.variableSet.FStrings[ASource.variableIndex].value;
      svtBoolList: result := ASource.variableSet.GetBoolListAsString(ASource.variableIndex);
      svtStrList: result := ASource.variableSet.GetStrListAsString(ASource.variableIndex);
    end;
  end;
end;

class function TVariableSet.GetGuid(const ASource: TScriptVariableReference
  ): TGuid;
begin
  result := ScriptStrToGuid(GetString(ASource));
end;

class function TVariableSet.GetPixel(const ASource: TScriptVariableReference): TBGRAPixel;
begin
  if ASource.variableSet <> nil then
  begin
    case ASource.variableType of
      svtPixel: begin
        result := ASource.variableSet.FScalars[ASource.variableIndex].valuePix;
        exit;
      end;
      svtInteger: begin
        result := IntToPixel(ASource.variableSet.FScalars[ASource.variableIndex].valueInt);
        exit;
      end;
      svtFloat: begin
        result := FloatToPixel(ASource.variableSet.FScalars[ASource.variableIndex].valueFloat);
        exit;
      end;
      svtBoolean: begin
        if ASource.variableSet.FScalars[ASource.variableIndex].valueBool then
          result := BGRAWhite else result := BGRABlack;
        exit;
      end;
      svtString: begin
        result := StrToBGRA(ASource.variableSet.FStrings[ASource.variableIndex].value);
        exit;
      end;
    end;
  end;
  result := BGRAPixelTransparent;
end;

class function TVariableSet.GetSubset(const ASource: TScriptVariableReference
  ): TVariableSet;
begin
  if ASource.variableSet <> nil then
  begin
    if ASource.variableType = svtSubset then
    begin
      result := asource.variableSet.FSubsets[ASource.variableIndex].value;
      exit;
    end;
  end;
  result := nil;
end;

class function TVariableSet.GetList(const ASource: TScriptVariableReference
  ): string;
begin
  if ASource.variableSet <> nil then
  begin
    if ASource.variableType in ScriptScalarListTypes then
      result := ASource.variableSet.GetScalarListAsString(ASource.variableIndex)
    else
    case ASource.variableType of
      svtBoolList: result := ASource.variableSet.GetBoolListAsString(ASource.variableIndex);
      svtStrList: result := ASource.variableSet.GetStrListAsString(ASource.variableIndex);
    else
      result := '';
    end;
  end;
end;

class function TVariableSet.GetListCount(const ASource: TScriptVariableReference
  ): NativeInt;
begin
  if ASource.variableSet <> nil then
  begin
    if ASource.variableType in ScriptScalarListTypes then
      result := ASource.variableSet.FScalarLists[ASource.variableIndex].count
    else
    case ASource.variableType of
      svtBoolList: result := ASource.variableSet.FBoolLists[ASource.variableIndex].count;
      svtStrList: result := ASource.variableSet.FStrLists[ASource.variableIndex].count;
    else
      result := 0;
    end;
  end
  else result := 0;
end;

class function TVariableSet.GetFloatAt(const ASource: TScriptVariableReference;
  AIndex: NativeInt): double;
var {%H-}errPos: integer;
begin
  result := 0;
  if (ASource.variableSet = nil) or (AIndex < 0) then exit;
  if not (ASource.variableType in ScriptVariableListTypes) then exit;
  if ASource.variableType in ScriptScalarListTypes then
    with ASource.variableSet.FScalarLists[ASource.variableIndex] do
    begin
      if AIndex >= count then exit;
      case ASource.variableType of
        svtFloatList: result := (PDouble(list)+AIndex)^;
        svtIntList: result := (PScriptInteger(list)+AIndex)^;
      end;
    end else
  if ASource.variableType = svtStrList then
    with asource.variableSet.FStrLists[ASource.variableIndex] do
    begin
      if AIndex >= count then exit;
      val(list[AIndex],result,errPos)
    end;
end;

class function TVariableSet.GetIntegerAt(
  const ASource: TScriptVariableReference; AIndex: NativeInt): TScriptInteger;
var {%H-}errPos: integer;
begin
  result := 0;
  if (ASource.variableSet = nil) or (AIndex < 0) then exit;
  if not (ASource.variableType in ScriptVariableListTypes) then exit;
  if ASource.variableType in ScriptScalarListTypes then
    with ASource.variableSet.FScalarLists[ASource.variableIndex] do
    begin
      if AIndex >= count then exit;
      case ASource.variableType of
        svtIntList: result := (PScriptInteger(list)+AIndex)^;
        svtFloatList: result := round((PDouble(list)+AIndex)^);
      end;
    end else
  if ASource.variableType = svtStrList then
    with asource.variableSet.FStrLists[ASource.variableIndex] do
    begin
      if AIndex >= count then exit;
      val(list[AIndex],result,errPos)
    end;
end;

class function TVariableSet.GetPoint2DAt(
  const ASource: TScriptVariableReference; AIndex: NativeInt): TPointF;
var
  result3D: TPoint3D;
begin
  result3D := GetPoint3DAt(ASource, AIndex);
  result := PointF(result3D.x, result3D.y);
end;

class function TVariableSet.GetPoint3DAt(
  const ASource: TScriptVariableReference; AIndex: NativeInt): TPoint3D;
begin
  result := GetPoint3DAt(ASource, AIndex, EmptySingle);
end;

class function TVariableSet.GetPoint3DAt(
  const ASource: TScriptVariableReference; AIndex: NativeInt; ADefaultZ: single
  ): TPoint3D;
begin
  result := Point3D(0,0, ADefaultZ);
  if (ASource.variableSet = nil) or (AIndex < 0) then exit;
  if not (ASource.variableType in ScriptVariableListTypes) then exit;
  if ASource.variableType in ScriptScalarListTypes then
    with ASource.variableSet.FScalarLists[ASource.variableIndex] do
    begin
      if AIndex >= count then exit;
      case ASource.variableType of
        svtPointList: begin
          result := (PPoint3D(list)+AIndex)^;
          if result.z = EmptySingle then result.z := ADefaultZ;
        end;
      end;
    end;
end;

class function TVariableSet.GetBooleanAt(
  const ASource: TScriptVariableReference; AIndex: NativeInt): boolean;
begin
  result := false;
  if (ASource.variableSet = nil) or (AIndex < 0) then exit;
  if not (ASource.variableType in ScriptVariableListTypes) then exit;
  if ASource.variableType in ScriptScalarListTypes then
    with ASource.variableSet.FScalarLists[ASource.variableIndex] do
    begin
      if AIndex >= count then exit;
      case ASource.variableType of
        svtIntList: result := (PScriptInteger(list)+AIndex)^ <> 0;
      end;
    end else
  if ASource.variableType = svtBoolList then
    with asource.variableSet.FBoolLists[ASource.variableIndex] do
    begin
      if AIndex >= count then exit;
      result := list.Bits[AIndex];
    end;
end;

class function TVariableSet.GetStringAt(
  const ASource: TScriptVariableReference; AIndex: NativeInt): string;
begin
  result := '';
  if (ASource.variableSet = nil) or (AIndex < 0) then exit;
  if not (ASource.variableType in ScriptVariableListTypes) then exit;
  if ASource.variableType in ScriptScalarListTypes then
    with ASource.variableSet.FScalarLists[ASource.variableIndex] do
    begin
      if AIndex >= count then exit;
      case ASource.variableType of
        svtPixList: result := BGRAToStr((PBGRAPixel(list)+AIndex)^);
        svtIntList: result := IntToStr((PScriptInteger(list)+AIndex)^);
      end;
    end else
  if ASource.variableType = svtStrList then
    with asource.variableSet.FStrLists[ASource.variableIndex] do
    begin
      if AIndex >= count then exit;
      result := list[AIndex];
    end;
end;

class function TVariableSet.GetGuidAt(const ASource: TScriptVariableReference;
  AIndex: NativeInt): TGuid;
begin
  result := ScriptStrToGuid(GetStringAt(ASource, AIndex));
end;

class function TVariableSet.GetPixelAt(const ASource: TScriptVariableReference;
  AIndex: NativeInt): TBGRAPixel;
begin
  result := BGRAPixelTransparent;
  if (ASource.variableSet = nil) or (AIndex < 0) then exit;
  if not (ASource.variableType in ScriptVariableListTypes) then exit;
  if ASource.variableType in ScriptScalarListTypes then
    with ASource.variableSet.FScalarLists[ASource.variableIndex] do
    begin
      if AIndex >= count then exit;
      case ASource.variableType of
        svtPixList: result := (PBGRAPixel(list)+AIndex)^;
        svtIntList: result := IntToPixel((PScriptInteger(list)+AIndex)^);
        svtFloatList: result := FloatToPixel((PDouble(list)+AIndex)^);
      end;
    end else
  if ASource.variableType = svtStrList then
    with asource.variableSet.FStrLists[ASource.variableIndex] do
    begin
      if AIndex >= count then exit;
      result := StrToBGRA(list[AIndex]);
    end;
  if ASource.variableType = svtBoolList then
    with asource.variableSet.FBoolLists[ASource.variableIndex] do
    begin
      if AIndex >= count then exit;
      if list.Bits[AIndex] then result := BGRAWhite else result := BGRABlack;
    end;
end;

class function TVariableSet.RemoveAt(const ASource: TScriptVariableReference;
  AIndex: NativeInt): boolean;
var i,listCount,elementSize: NativeInt;
begin
  result := false;
  if (ASource.variableSet = nil) or (AIndex < 0) then exit;
  if not (ASource.variableType in ScriptVariableListTypes) then exit;
  listCount := GetListCount(ASource);
  if AIndex >= listCount then exit;
  if ASource.variableType in ScriptScalarListTypes then
  with ASource.variableSet.FScalarLists[ASource.variableIndex] do
  begin
    elementSize:= ScalarListElementSize[varType];
    Move((pbyte(list)+(AIndex+1)*elementSize)^,(pbyte(list)+AIndex*elementSize)^,(listCount-AIndex-1)*elementSize);
    dec(count);
    result := true;
  end else
  case ASource.variableType of
    svtBoolList: with ASource.variableSet.FBoolLists[ASource.variableIndex] do
      begin
        for i := AIndex to listCount-2 do
          list.Bits[i] := list.Bits[i+1];
        dec(count);
        result := true;
      end;
    svtStrList: with ASource.variableSet.FStrLists[ASource.variableIndex] do
      begin
        for i := AIndex to listCount-2 do
          list[i] := list[i+1];
        list[listCount-1] := '';
        dec(count);
        result := true;
      end;
  end;
end;

function TVariableSet.Duplicate: TVariableSet;
var i: NativeInt;
  v,w: TScriptVariableReference;
begin
  result := TVariableSet.Create(FunctionName);
  setlength(result.FScalars, FNbScalars);
  result.FNbScalars := FNbScalars;
  for i := 0 to FNbScalars-1 do result.FScalars[i] := FScalars[i];
  setlength(result.FStrings, length(FStrings));
  result.FNbStrings := FNbStrings;
  for i := 0 to FNbStrings-1 do result.FStrings[i] := FStrings[i];

  w.variableSet := self;
  for i := 0 to FNbStrLists-1 do
  begin
    v := result.AddStringList(FStrLists[i].name);
    w.variableIndex := i;
    w.variableType := svtStrList;
    AssignVariable(v,w);
  end;
  for i := 0 to FNbBoolLists-1 do
  begin
    v := result.AddBooleanList(FBoolLists[i].name);
    w.variableIndex := i;
    w.variableType := svtBoolList;
    AssignVariable(v,w);
  end;
  for i := 0 to FNbScalarLists-1 do
  begin
    v := result.AddScalarList(FScalarLists[i].name,FScalarLists[i].varType);
    w.variableIndex := i;
    w.variableType := FScalarLists[i].varType;
    AssignVariable(v,w);
  end;

  setlength(result.FSubsets, length(FSubsets));
  result.FNbSubsets := FNbSubsets;
  for i := 0 to FNbSubsets-1 do
  begin
    result.FSubsets[i].name := FSubsets[i].name;
    result.FSubsets[i].value := FSubsets[i].value.Duplicate;
  end;
end;

function TVariableSet.CopyValuesTo(ASet: TVariableSet): boolean;
var i: NativeInt;
  v,w: TScriptVariableReference;
begin
  result := true;
  if not Assigned(ASet) then exit;
  w.variableSet := self;
  for i := 0 to FNbScalars-1 do
  begin
    v := ASet.GetVariable(FScalars[i].name);
    if not IsReferenceDefined(v) then
    begin
      if length(ASet.FScalars) = ASet.FNbScalars then
      begin
        if length(ASet.FScalars) = 0 then
           setlength(ASet.FScalars,4)
        else
           setlength(ASet.FScalars, length(ASet.FScalars)*2);
      end;
      ASet.FScalars[ASet.FNbScalars] := FScalars[i];
      inc(ASet.FNbScalars);
    end else
    begin
      w.variableIndex := i;
      w.variableType := FScalars[i].varType;
      if not AssignVariable(v,w) then result := false;
    end;
  end;
  for i := 0 to FNbStrings-1 do ASet.Strings[FStrings[i].name] := FStrings[i].value;
  for i := 0 to FNbSubsets-1 do ASet.Subsets[FSubsets[i].name] := FSubsets[i].value;
  for i := 0 to FNbStrLists-1 do
  begin
    v := ASet.GetVariable(FStrLists[i].name);
    if not IsReferenceDefined(v) then v := ASet.AddStringList(FStrLists[i].name);
    w.variableIndex := i;
    w.variableType := svtStrList;
    if not AssignVariable(v,w) then result := false;
  end;
  for i := 0 to FNbBoolLists-1 do
  begin
    v := ASet.GetVariable(FBoolLists[i].name);
    if not IsReferenceDefined(v) then v := ASet.AddBooleanList(FBoolLists[i].name);
    w.variableIndex := i;
    w.variableType := svtBoolList;
    if not AssignVariable(v,w) then result := false;
  end;
  for i := 0 to FNbScalarLists-1 do
  begin
    v := ASet.GetVariable(FScalarLists[i].name);
    if not IsReferenceDefined(v) then v := ASet.AddScalarList(FScalarLists[i].name,FScalarLists[i].varType);
    w.variableIndex := i;
    w.variableType := FScalarLists[i].varType;
    if not AssignVariable(v,w) then result := false;
  end;
end;

(* var s: TVariableSet;

initialization

  s:=TVariableSet.Create('','a: 1, b: {c: "Hello", d: 35.00, L1: [1,4,9]}, b2: {c: "World", d: 0, d2: 12, L1: [0.0], L2: [~""]}, e: #ff00ff');
  ShowMessage(s.VariablesAsString);
  s.AddList('L0','["alpha","beta","gamma"]');
  ShowMessage('add L0: ' + s.VariablesAsString);
  s.Subsets['b2'] := s.Subsets['b'];
  s.Subsets['b3'] := s.Subsets['b'];
  ShowMessage('set b2 and b3: ' + s.VariablesAsString);
  s.AssignList(s.GetVariable('L0'),'[1,2,3,4]');
  ShowMessage('set L0: ' + s.VariablesAsString);
  s.free; *)

end.
