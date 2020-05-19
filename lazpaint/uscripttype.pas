// SPDX-License-Identifier: GPL-3.0-only
unit UScriptType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmapTypes;

const
  VariableDefinitionToken : string = ':';
  TrueToken : string = 'True';
  FalseToken : string = 'False';
  UndefinedToken : string = 'None';
  CharToken1 : string = 'Chr';
  CharToken2 : string = 'Char';
  StringDelimiter1 = '"';
  StringDelimiter2 = '''';
  EscapePrefix = '\';
  StringDelimiters = [StringDelimiter1, StringDelimiter2];
  IdentifierCharStart: set of char = ['a'..'z','A'..'Z','_',#128..#255];
  IdentifierCharMiddle: set of char = ['a'..'z','A'..'Z','_',#128..#255,'0'..'9'];
  IgnoredWhitespaces : set of char = [#9,#13,#10,' '];
  ListMaxLength = 65536;


type
  TScriptInteger = int64;
  PScriptInteger = ^TScriptInteger;

  TInterpretationError = (ieTooManyClosingBrackets, ieEndingQuoteNotFound, ieOpeningBracketNotFound, ieClosingBracketNotFound,
                          ieConstantExpressionExpected, ieUnexpectedChar, ieInvalidNumber, ieInvalidColor, ieInvalidBoolean,
                          ieDuplicateIdentifier, ieUnexpectedOpeningBracketKind, ieUnexpectedClosingBracketKind,
                          ieUnknownListType, ieMissingValue, ieTooManyValues);
  TInterpretationErrors = set of TInterpretationError;
  TScriptVariableType = (svtUndefined, svtFloat, svtInteger, svtPoint, svtBoolean, svtString, svtPixel, svtSubset,
                         svtFloatList, svtIntList, svtPointList, svtBoolList, svtStrList, svtPixList);
  TScriptFunctionExceptionHandler = procedure(AFunctionName: string; AException: Exception) of object;

  TParsedLitteral = record
    valueType: TScriptVariableType;
    valueFloat: double;
    valueInt: TScriptInteger;
    valuePoint: TPoint3D;
    valueBool: boolean;
    valueStr: string;
    valuePixel: TBGRAPixel;
  end;

  TScalarVariable = record
    name: string;
    varType: TScriptVariableType;
    case TScriptVariableType of
      svtFloat: (valueFloat: double);
      svtInteger: (valueInt: TScriptInteger);
      svtPoint: (valuePoint: TPoint3D);
      svtBoolean: (valueBool: boolean);
      svtPixel: (valuePix: TBGRAPixel);
      svtUndefined: (valueBytes: packed array[0..11] of byte);
  end;

const
  ScriptVariableListTypes : set of TScriptVariableType = [svtFloatList, svtIntList, svtPointList, svtBoolList, svtStrList, svtPixList];
  ScriptScalarListTypes : set of TScriptVariableType = [svtFloatList, svtIntList, svtPointList, svtPixList];
  ScriptScalarTypes : set of TScriptVariableType = [svtFloat, svtInteger, svtPoint, svtBoolean, svtPixel];
  ScalarListElementSize : array[svtFloatList..svtPixList] of NativeInt =
    (sizeof(double), sizeof(TScriptInteger), sizeof(TPoint3D), 0, 0, sizeof(TBGRAPixel));
  ListElementType : array[svtFloatList..svtPixList] of TScriptVariableType =
    (svtFloat, svtInteger, svtPoint, svtBoolean, svtString, svtPixel);
  EmptyListExpression : array[svtFloatList..svtPixList] of string =
    ('[~0.0]', '[~0]', '[(0.0,0.0)]', '[~False]', '[~""]','[~#000]');
  InterpretationErrorToStr: array[TInterpretationError] of string =
    ('Too many closing brackets', 'Ending quote not found',
     'Opening bracket not found', 'Closing bracket not found',
     'Constant expression expected', 'Unexpected char',
     'Invalid number', 'Invalid color', 'Invalid boolean',
     'Duplicate identifier', 'Unexpected opening bracket kind',
     'Unexpected closing bracket kind',
     'Unknown list type', 'Missing value', 'Too many values');

function ScriptQuote(const S: string): string;
function ScriptUnquote(const S: string): string;
function UnescapeString(const S: string): string;
function TryScriptUnquote(const S: String; out unquotedS: string): TInterpretationErrors;
function FloatToStrUS(AValue: double; AExplicitDot: boolean = true): string;
function ScalarToStr(AVarType: TScriptVariableType; const AValue): string;
function ParseLitteral(var cur: integer; expr: string; var errors: TInterpretationErrors): TParsedLitteral;
function ParseListType(s: string): TScriptVariableType;
function FloatToPixel(AValue: double): TBGRAPixel;
function IntToPixel(AValue: TScriptInteger): TBGRAPixel;
function PixelToInt(AValue: TBGRAPixel): TScriptInteger;
function InterpretationErrorsToStr(AErrors: TInterpretationErrors): string;
function ScriptGuidToStr(const AGuid: TGuid): string;
function ScriptStrToGuid(AValue: string): TGuid;

implementation

uses BGRAUTF8;

{$i quote.inc}

function FloatToStrUS(AValue: double; AExplicitDot: boolean = true): string;
var idxE,idxPt,beforeE,afterE: integer;
begin
  if frac(AValue) = 0 then
    str(AValue:15:0, result)
  else
    str(AValue,result);
  result := trim(result);
  idxE := pos('E',result);
  idxPt := pos('.',result);
  if (idxE <> 0) and (idxPt < idxE) then
  begin
    beforeE := idxE;
    while (beforeE > 1) and (result[beforeE-1]='0') do dec(beforeE);
    if (beforeE > 1) and (result[beforeE-1]='.') then dec(beforeE);
    delete(result,beforeE,idxE-beforeE);

    idxE := pos('E',result);
    if (idxE < length(result)) and (result[idxE+1]='-') then inc(idxE);
    afterE := idxE;
    if (afterE < length(result)) and (result[afterE+1]='+') then inc(afterE);
    while (afterE < length(result)) and (result[afterE+1]='0') do inc(afterE);
    if (afterE = length(result)) then
    begin
      if (idxE > 1) and (result[idxE] = 'E') then dec(idxE);
    end;
    delete(result,idxE+1,afterE-idxE);

    idxE := pos('E',result);
    idxPt := pos('.',result);
    if copy(result,idxE,length(result)-idxE+1)='E-1' then
    begin
      if idxPt >= 1 then
      begin
        delete(result,idxPt,1);
        if idxPt > 1 then dec(idxPt) else result := '0'+result;
        insert('.',result,idxPt);
        if (idxPt = 1) or (result[idxPt-1] = '-') then insert('0',result,idxPt);
      end else
        result := '0.' + result;
      idxE := pos('E',result);
      delete(result,idxE,length(result)-idxE+1);
    end;

  end;
  idxE := pos('E',result);
  idxPt := pos('.',result);
  if AExplicitDot and (idxE = 0) and (idxPt = 0) then result := result+'.0';
end;

function ScalarToStr(AVarType: TScriptVariableType; const AValue): string;
begin
  case AVarType of
    svtFloat: result := FloatToStrUS(double(AValue));
    svtInteger: result := IntToStr(TScriptInteger(AValue));
    svtPoint: with TPoint3D(AValue) do
              begin
                if z <> EmptySingle then
                  result := '(' + FloatToStrUS(x, false)+', '+FloatToStrUS(y, false)+', '+FloatToStrUS(z, false)+')'
                else
                  result := '(' + FloatToStrUS(x, false)+', '+FloatToStrUS(y, false)+')';
              end;
    svtPixel: result := '#'+BGRAToStr(TBGRAPixel(AValue), nil,0,true);
    svtBoolean: result := BoolToStr(Boolean(AValue),TrueToken,FalseToken);
  else raise exception.Create('Not a scalar type');
  end;
end;

function ParseLitteral(var cur: integer; expr: string; var errors: TInterpretationErrors): TParsedLitteral;
var startIdentifier: integer;
    inIdentifier, notConstant: boolean;
    inBracket: integer;
    isString, isBoolean, isUndefined: boolean;
  procedure CheckIdentifier;
  var idStr: string;
  begin
    inIdentifier:= false;
    idStr := copy(expr,startIdentifier,cur-startIdentifier);
    if (CompareText(idStr,CharToken1) = 0) or (CompareText(idStr,CharToken2) = 0) then
    begin
      if inBracket = 0 then isString := true;
    end else
    if (CompareText(idStr,TrueToken) = 0) or (CompareText(idStr,FalseToken) = 0) then
    begin
      if inBracket = 0 then isBoolean := true;
    end
    else
    if (CompareText(idStr,UndefinedToken) = 0) then
    begin
      if inBracket = 0 then isUndefined := true;
    end
    else
      notConstant := true;
  end;

var
  previousChar: char;
  valueStr: string;
  start: integer;
  unquotedStr: string;
  inQuote: char;
  inNumber, inPixel: boolean;
  isNumber, isPixel: boolean;
  valueInt: TScriptInteger;
  valueFloat: double;
  valueBool: boolean;
  valuePixel: TBGRAPixel;
  errPos,coordIndex,posComma: integer;
  missingFlag,errorFlag: boolean;
begin
  result.valueType := svtUndefined;
  result.valueFloat := 0;
  result.valueInt := 0;
  result.valuePixel := BGRAPixelTransparent;
  result.valueBool:= false;
  start := cur;
  inBracket:= 0;
  inQuote:= #0;
  inIdentifier:= false;
  inNumber:= false;
  inPixel:= false;
  previousChar := #0;
  isString := false;
  isBoolean:= false;
  isNumber:= false;
  isPixel := false;
  isUndefined := false;
  startIdentifier:= 1; //initialize
  notConstant:= false;
  while cur <= length(expr) do
  begin
    if inQuote<>#0 then
    begin
      if expr[cur] = inQuote then inQuote := #0 else
      if expr[cur] in[#13,#10] then
      begin
        errors += [ieEndingQuoteNotFound];
        break;
      end;
    end else
    begin
      if inIdentifier then
      begin
        if not (expr[cur] in IdentifierCharMiddle) then
          CheckIdentifier;
      end else
      if inNumber then
      begin
        if not ((expr[cur] in['0'..'9','.','e','E']) or
          ((expr[cur] in['-','+']) and (previousChar in ['e','E']))) then
          inNumber:= false;
      end else
      if inPixel then
      begin
        if not (expr[cur] in['0'..'9','a'..'f','A'..'F']) then
          inPixel:= false;
      end;
      if not inNumber and not inIdentifier and not inPixel then
      begin
        if expr[cur] in['(','['] then inc(inBracket) else
        if expr[cur] in[')',']'] then
        begin
          dec(inBracket);
          if inBracket < 0 then errors += [ieTooManyClosingBrackets];
        end else
        if expr[cur] in StringDelimiters then
        begin
          inQuote := expr[cur];
          if inBracket = 0 then isString:= true;
        end else
        if expr[cur] in IdentifierCharStart then
        begin
          inIdentifier := true;
          startIdentifier:= cur;
        end
        else
        if expr[cur] in['0'..'9','.'] then
        begin
          inNumber := true;
          if inBracket = 0 then isNumber:= true;
        end
        else
        if expr[cur] = '#' then
        begin
          inPixel := true;
          if inBracket = 0 then IsPixel:= true;
        end
        else
        if (expr[cur] in[',','}']) and (inBracket = 0) then break;
      end;
    end;
    previousChar:= expr[cur];
    inc(cur);
  end;
  if inNumber then inNumber:= false;
  if inPixel then inPixel := false;
  if inIdentifier then CheckIdentifier;
  if inQuote<>#0 then errors += [ieEndingQuoteNotFound];
  if inBracket > 0 then errors += [ieClosingBracketNotFound];
  if notConstant then errors += [ieConstantExpressionExpected];
  valueStr := Trim(copy(expr,start,cur-start));
  if isUndefined then
  begin
    result.valueType := svtUndefined;
  end else
  if isString then
  begin
    errors := errors + TryScriptUnquote(valueStr, unquotedStr);
    result.valueType := svtString;
    result.valueStr := unquotedStr;
  end else
  if isBoolean then
  begin
    if not TryStrToBool(valueStr, valueBool) then
      errors := errors + [ieInvalidBoolean] else
    begin
      result.valueType := svtBoolean;
      result.valueBool := valueBool;
    end;
  end else
  if isNumber then
  begin
    if pos('.',valueStr) = 0 then
    begin
      val(valueStr,valueInt,errPos);
      if errPos <> 0 then errors := errors + [ieInvalidNumber]
      else
      begin
        result.valueType := svtInteger;
        result.valueInt := valueInt;
      end;
    end else
    begin
      val(valueStr,valueFloat,errPos);
      if errPos <> 0 then errors := errors + [ieInvalidNumber]
      else
      begin
        result.valueType := svtFloat;
        result.valueFloat := valueFloat;
      end;
    end;
  end else
  if isPixel then
  begin
    valuePixel := BGRABlack;
    TryStrToBGRA(valueStr,valuePixel,missingFlag,errorFlag);
    if errorFlag or missingFlag then errors := errors + [ieInvalidColor]
    else
    begin
      result.valueType:= svtPixel;
      result.valuePixel := valuePixel;
    end;
  end else
  if (length(valueStr)>=2) and (valueStr[1] = '(') and (valueStr[length(valueStr)] = ')') then
  begin
    result.valuePoint:= Point3D(0,0,EmptySingle);
    valueStr := trim(copy(valueStr,2,length(valueStr)-2));
    coordIndex := 0;
    while valueStr<>'' do
    begin
      if coordIndex >= 3 then
      begin
        errors := errors + [ieTooManyValues];
        break;
      end;
      posComma := pos(',', valueStr);
      if posComma > 0 then
        val(copy(valueStr,1,posComma-1),valueFloat,errPos)
      else
        val(valueStr,valueFloat,errPos);
      if errPos <> 0 then
      begin
        errors := errors + [ieInvalidNumber];
        break;
      end;
      case coordIndex of
        0: result.valuePoint.x := valueFloat;
        1: result.valuePoint.y := valueFloat;
        2: result.valuePoint.z := valueFloat;
      end;
      inc(coordIndex);
      if posComma = 0 then valueStr := ''
      else delete(valueStr, 1, posComma);
    end;
    if coordIndex >= 2 then
      result.valueType:= svtPoint;
  end else
    errors := errors + [ieConstantExpressionExpected];
end;

function ParseListType(s: string): TScriptVariableType;
var cur,start,inPar: integer;
  inQuote: boolean;
  firstVal: TParsedLitteral;
  errors: TInterpretationErrors;
begin
  s := trim(s);
  if (length(s)>0) and (s[1]='[') then cur := 2 else cur := 1;
  while (cur <= length(s)) and (s[cur] in IgnoredWhitespaces) do inc(cur);
  if (cur <= length(s)) and (s[cur]='~') then inc(cur);
  while (cur <= length(s)) and (s[cur] in IgnoredWhitespaces) do inc(cur);
  inQuote:= false;
  inPar := 0;
  start := cur;
  while (cur <= length(s)) do
  begin
    if inQuote then
    begin
      if s[cur]='"' then inQuote:= false;
    end else
    begin
      if s[cur]='"' then inQuote:= true else
      if s[cur]='(' then inc(inPar) else
      if s[cur]=')' then
      begin
        if inPar > 0 then dec(inPar) else break;
      end else
      if (inPar = 0) and (s[cur] in ['[',']',',']) then break;
    end;
    inc(cur);
  end;
  s := copy(s,start,cur-start);
  cur := 1;
  errors := [];
  firstVal := ParseLitteral(cur,s,errors);
  case firstval.valueType of
  svtBoolean: result := svtBoolList;
  svtFloat: result := svtFloatList;
  svtPoint: result := svtPointList;
  svtInteger: result := svtIntList;
  svtPixel: result := svtPixList;
  svtString: result := svtStrList;
  svtUndefined:
    begin
      include(errors, ieUnknownListType);
      result := svtUndefined;
    end
  else
    result := svtUndefined;
  end;
end;

function FloatToPixel(AValue: double): TBGRAPixel;
var byteValue: byte;
begin
  if AValue <= 0 then result := BGRABlack else
  if AValue >= 255 then result := BGRAWhite else
  begin
    byteValue := round(AValue);
    result := BGRA(byteValue,byteValue,byteValue,255);
  end;
end;

function IntToPixel(AValue: TScriptInteger): TBGRAPixel;
begin
  if AValue <= 0 then result := BGRABlack else
  if AValue >= 255 then result := BGRAWhite else
    result := BGRA(AValue,AValue,AValue,255);
end;

function PixelToInt(AValue: TBGRAPixel): TScriptInteger;
begin
  result := AValue.ToGrayscale.green;
end;

function InterpretationErrorsToStr(AErrors: TInterpretationErrors): string;
var
  e: TInterpretationError;
begin
  result := '';
  for e := low(TInterpretationError) to high(TInterpretationError) do
    if e in AErrors then
    begin
      if result <> '' then result += ', ';
      result += InterpretationErrorToStr[e];
    end;
end;

function ScriptGuidToStr(const AGuid: TGuid): string;
begin
  result := LowerCase(GUIDToString(AGuid));
  if (length(result)>0) and (result[1]='{') and (result[length(result)]='}') then
    result := copy(result,2,length(result)-2);
end;

function ScriptStrToGuid(AValue: string): TGuid;
begin
  if not TryStringToGUID('{'+AValue+'}', result) then
    result := GUID_NULL;
end;

end.

