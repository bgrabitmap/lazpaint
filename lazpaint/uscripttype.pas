unit UScriptType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmapTypes;

const
  ScriptKeywords : array[1..40] of string = ('var','procedure','function','and',
    'or','xor','not','true','false','if','then','case','begin','end','of',
    'exit','new','class','is','const','div','do','downto','to','else','for',
    'in','mod','nil','object','record','repeat','self','shl','shr','string',
    'unit','until','uses','while');
  ScriptSymbols : array[1..38] of string = ('+','-','*','/','=','<','>','[',']',
  '.',',','(',')',':','^','@','{','}','$','#','&','%','<<','>>','**','<>','><',
  '<=','>=',':=','+=','-=','*=','/=','(*','*)','//','..');

  VariableDefinitionToken : string = ':';
  TrueToken : string = 'True';
  FalseToken : string = 'False';
  NilToken : string = 'Nil';
  CharToken1 : string = 'Chr';
  CharToken2 : string = 'Char';
  StringDelimiter: string = '"';
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
                          ieUnknownListType, ieMissingValue);
  TInterpretationErrors = set of TInterpretationError;
  TScriptVariableType = (svtUndefined, svtObject, svtFloat, svtInteger, svtBoolean, svtString, svtPixel, svtSubset,
                         svtFloatList, svtIntList, svtBoolList, svtStrList, svtPixList, svtObjectList);
  TScriptFunctionExceptionHandler = procedure(AFunctionName: string; AException: Exception) of object;

  TParsedLitteral = record
    valueType: TScriptVariableType;
    valueFloat: double;
    valueInt: TScriptInteger;
    valueBool: boolean;
    valueStr: string;
    valuePixel: TBGRAPixel;
  end;

  TScalarVariable = record
    name: string;
    varType: TScriptVariableType;
    case TScriptVariableType of
      svtFloat: (valueFloat: double);
      svtInteger,svtObject: (valueInt: TScriptInteger);
      svtBoolean: (valueBool: boolean);
      svtPixel: (valuePix: TBGRAPixel);
      svtUndefined: (valueBytes: packed array[0..7] of byte);
  end;

const
  ScriptVariableListTypes : set of TScriptVariableType = [svtFloatList, svtIntList, svtBoolList, svtStrList, svtPixList, svtObjectList];
  ScriptScalarListTypes : set of TScriptVariableType = [svtFloatList, svtIntList, svtPixList, svtObjectList];
  ScalarListElementSize : array[svtFloatList..svtObjectList] of NativeInt =
    (sizeof(double), sizeof(TScriptInteger), 0, 0, sizeof(TBGRAPixel), sizeof(TScriptInteger));
  ListElementType : array[svtFloatList..svtObjectList] of TScriptVariableType =
    (svtFloat, svtInteger, svtBoolean, svtString, svtPixel, svtObject);
  EmptyListExpression : array[svtFloatList..svtObjectList] of string =
    ('[~0.0]', '[~0]', '[~False]', '[~""]','[~#000]','[~Nil]');

function ScriptQuote(const S: string): string;
function ScriptUnquote(const S: string): string;
function TryScriptUnquote(const S: String; out unquotedS: string): TInterpretationErrors;
function FloatToStrUS(AValue: double): string;
function ScalarToStr(AVarType: TScriptVariableType; var AValue): string;
function ParseLitteral(var cur: integer; expr: string; var errors: TInterpretationErrors): TParsedLitteral;
function ParseListType(s: string): TScriptVariableType;
function FloatToPixel(AValue: double): TBGRAPixel;
function IntToPixel(AValue: TScriptInteger): TBGRAPixel;

implementation

{$i quote.inc}

function FloatToStrUS(AValue: double): string;
var idxE,idxPt,beforeE,afterE: integer;
begin
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
  if (idxE = 0) and (idxPt = 0) then result := result+'.0';
end;

function ScalarToStr(AVarType: TScriptVariableType; var AValue): string;
begin
  case AVarType of
    svtFloat: result := FloatToStrUS(double(AValue));
    svtInteger: result := IntToStr(TScriptInteger(AValue));
    svtPixel: result := '#'+BGRAToStr(TBGRAPixel(AValue));
    svtBoolean: result := BoolToStr(Boolean(AValue),TrueToken,FalseToken);
    svtObject: if TScriptInteger(AValue) = 0 then result := NilToken else result := 'Object';
  else raise exception.Create('Not a scalar type');
  end;
end;

function ParseLitteral(var cur: integer; expr: string; var errors: TInterpretationErrors): TParsedLitteral;
var startIdentifier: integer;
    inIdentifier, notConstant: boolean;
    inBracket: integer;
    isString, isBoolean: boolean;
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
      notConstant := true;
  end;

var
  previousChar: char;
  valueStr: string;
  start: integer;
  unquotedStr: string;
  inQuote, inNumber, inPixel: boolean;
  isNumber, isPixel: boolean;
  valueInt: TScriptInteger;
  valueFloat: double;
  valueBool: boolean;
  valuePixel: TBGRAPixel;
  errPos: integer;
  missingFlag,errorFlag: boolean;
begin
  result.valueType := svtUndefined;
  result.valueFloat := 0;
  result.valueInt := 0;
  result.valuePixel := BGRAPixelTransparent;
  result.valueBool:= false;
  if CompareText(trim(expr),NilToken) = 0 then
  begin
    result.valueType := svtObject;
    exit;
  end;
  start := cur;
  inBracket:= 0;
  inQuote:= false;
  inIdentifier:= false;
  inNumber:= false;
  inPixel:= false;
  previousChar := #0;
  isString := false;
  isBoolean:= false;
  isNumber:= false;
  isPixel := false;
  startIdentifier:= 1; //initialize
  notConstant:= false;
  while cur <= length(expr) do
  begin
    if inQuote then
    begin
      if expr[cur] = StringDelimiter then inQuote := false else
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
        if expr[cur] = StringDelimiter then
        begin
          inQuote := true;
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
        if expr[cur] = ',' then break;
      end;
    end;
    previousChar:= expr[cur];
    inc(cur);
  end;
  if inNumber then inNumber:= false;
  if inPixel then inPixel := false;
  if inIdentifier then CheckIdentifier;
  if inQuote then errors += [ieEndingQuoteNotFound];
  if inBracket > 0 then errors += [ieClosingBracketNotFound];
  if notConstant then errors += [ieConstantExpressionExpected];
  valueStr := Trim(copy(expr,start,cur-start));
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
    errors := errors + [ieConstantExpressionExpected];
end;

function ParseListType(s: string): TScriptVariableType;
var cur,start: integer;
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
  start := cur;
  while (cur <= length(s)) do
  begin
    if inQuote then
    begin
      if s[cur]='"' then inQuote:= false;
    end else
    begin
      if s[cur]='"' then inQuote:= true else
      if s[cur] in ['[',']',','] then break;
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
  svtInteger: result := svtIntList;
  svtPixel: result := svtPixList;
  svtString: result := svtStrList;
  svtObject: result := svtObjectList;
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

end.

