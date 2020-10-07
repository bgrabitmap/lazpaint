// SPDX-License-Identifier: GPL-3.0-only
function ScriptUnquote(const S: string): string;
var errors: TInterpretationErrors;
begin
  errors := TryScriptUnquote(s,result);
  if errors <> [] then
    raise exception.create('Invalid quoted string (error '+inttostr(integer(errors))+')');
end;

function UnescapeString(const S: string): string;
const HexDigit = ['0'..'9','a'..'f','A'..'F'];
  OctDigit = ['0'..'7'];
var
  outputpos: integer;

  procedure put(c: char);
  begin
    if outputpos > length(result) then
      setlength(result, length(result)*2+1);
    result[outputpos] := c;
    inc(outputpos);
  end;
  procedure putStr(s: string);
  var
    j: Integer;
  begin
    for j := 1 to length(s) do
      put(s[j]);
  end;
  function CheckHex(AFrom,ATo: integer): boolean;
  var
    j: Integer;
  begin
    if ATo > length(s) then exit(false);
    for j := AFrom to ATo do
      if not (s[j] in HexDigit) then exit(false);
    result := true;
  end;
  function CheckOct(AFrom,ATo: integer): boolean;
  var
    j: Integer;
  begin
    if ATo > length(s) then exit(false);
    for j := AFrom to ATo do
      if not (s[j] in OctDigit) then exit(false);
    result := true;
  end;
  function OctToInt(s: string): integer;
  var
    j: Integer;
  begin
    result := 0;
    for j := 1 to length(s) do
      result := (result shl 3)+ord(s[j])-ord('0');
  end;

var
  i: Integer;
  escaping: boolean;

begin
  setlength(result, length(s));
  escaping := false;
  outputpos := 1;
  i := 1;
  while i <= length(s) do
  begin
    if escaping then
    begin
      case s[i] of
        '\','''','"': put(s[i]);
        'a': put(#7);
        'b': put(#8);
        'f': put(#12);
        'n': put(#10);
        'r': put(#13);
        't': put(#9);
        'v': put(#11);
        '0'..'7': if CheckOct(i+1,i+3) then
             begin
               putstr(UnicodeCharToUTF8(OctToInt(copy(s,i+1,2))));
               inc(i,3);
             end else putstr('\'+s[i]);
        'x': if CheckHex(i+1,i+2) then
             begin
               putstr(UnicodeCharToUTF8(StrToInt('$'+copy(s,i+1,2))));
               inc(i,2);
             end else putstr('\'+s[i]);
        'u': if CheckHex(i+1,i+4) then
             begin
               putstr(UnicodeCharToUTF8(StrToInt('$'+copy(s,i+1,4))));
               inc(i,4);
             end else putstr('\'+s[i]);
        'U': if CheckHex(i+1,i+8) then
             begin
               putstr(UnicodeCharToUTF8(StrToInt('$'+copy(s,i+1,8))));
               inc(i,8);
             end else putstr('\'+s[i]);
         else putstr('\'+s[i]);
      end;
      escaping := false;
    end else
    if s[i] = '\' then escaping := true
    else put(s[i]);
    inc(i);
  end;
  setlength(result, outputpos-1);
end;

function TryScriptUnquote(const S: String; out unquotedS: string): TInterpretationErrors;
var curPos,quoteStart,idStart: integer; idStr, charCodeStr: string;
  charFuncStep: (fsNone, fsWaitOpenBracket, fsCharCodeParam, fsWaitCloseBraket);
  escaping: Boolean;

  procedure AppendChar;
  var errPos: integer;
    charValue: integer;
  begin
    val(charCodeStr,charValue,errPos);
    if (errPos = 0) and (charValue >= 0) and (charValue < 128) then
      unquotedS:=unquotedS+chr(charValue)
    else
      result += [ieInvalidNumber];
  end;

begin
  unquotedS:= '';
  curPos := 1;
  charFuncStep:= fsNone;
  charCodeStr := ''; //init
  result := [];

  while curPos <= length(s) do
  begin
    if s[curPos] in[' ',#9..#13,'+','&'] then
    begin
      if (charFuncStep = fsCharCodeParam) and (charCodeStr <> '') then charFuncStep:= fsWaitCloseBraket;
      //ignore whitespaces or concatenation operators
    end else
    if charFuncStep <> fsNone then
    begin
      //loose interpretation
      if (charFuncStep = fsWaitOpenBracket) and (s[CurPos] <> '(') then
      begin
        result += [ieOpeningBracketNotFound];
        charFuncStep:= fsCharCodeParam;
      end else
      if (charFuncStep = fsWaitCloseBraket) and (s[CurPos] <> ')') then
      begin
        result += [ieClosingBracketNotFound];
        AppendChar;
        charFuncStep:= fsNone;
      end;

      //strict interpretation
      if (charFuncStep = fsWaitOpenBracket) and (s[CurPos] = '(') then
        charFuncStep:= fsCharCodeParam
      else if (charFuncStep = fsWaitCloseBraket) and (s[CurPos] = ')') then
      begin
        AppendChar;
        charFuncStep:= fsNone;
      end else
      if charFuncStep = fsCharCodeParam then
      begin
        if s[CurPos] = ')' then
        begin
          AppendChar;
          charFuncStep:= fsNone;
        end else
        if not (s[CurPos] in['0'..'9']) then
        begin
          result += [ieUnexpectedChar];
          AppendChar;
          charFuncStep:= fsNone;
        end else
          charCodeStr := charCodeStr+s[CurPos];
      end;
    end else
    if s[curPos] in StringDelimiters then
    begin
      quoteStart := curPos;
      escaping := false;
      inc(curPos);
      while true do
      begin
        if curPos <= length(s) then
        begin
          if not escaping then
          begin
            if s[curPos]=EscapePrefix then
              escaping := true
            else
            if s[curPos]=s[quoteStart] then
            begin
              unquotedS:= unquotedS+UnescapeString(copy(s,quoteStart+1,curPos-quoteStart-1));
              inc(curPos);
              break;
            end;
          end else
            escaping := false;
          inc(curPos);
        end else
        begin
          result += [ieEndingQuoteNotFound];
          break;
        end;
      end;
      dec(curPos);
    end else
    if s[curPos] in IdentifierCharStart then
    begin
      idStart := curPos;
      while (curPos+1 <= length(s)) and (s[curPos+1] in IdentifierCharMiddle) do inc(curPos);
      idStr := copy(s,idStart,curPos-idStart+1);
      if (CompareText(idStr,CharToken1)=0) or (CompareText(idStr,CharToken2)=0) then
      begin
        charFuncStep:= fsWaitOpenBracket;
        charCodeStr := '';
      end else
        result += [ieConstantExpressionExpected];
    end else
      result := [ieUnexpectedChar];
    inc(curPos);
  end;
end;

function ScriptQuote(const S: string): string;
const
  StringDelimiter = StringDelimiter1;
  EscapeChars = [#0,#7..#13,#26,#27,'\',StringDelimiter];
var i, j, count: integer;

  procedure FlushChars;
  var NbFlush: integer;
  begin
    NbFlush := i - j - 1;
    if NbFlush <= 0 then exit;
    result := result + copy(S, 1 + j, NbFlush);
    j := i;
  end;

begin
  result := StringDelimiter;
  count := length(s);
  i := 0;
  j := 0;
  while i < count do
  begin
     i := i + 1;
     if s[i] in EscapeChars then
     begin
       FlushChars;
       case s[i] of
         #7: result += '\a';
         #8: result += '\b';
         #9: result += '\t';
         #10: result += '\n';
         #11: result += '\v';
         #12: result += '\f';
         #13: result += '\r';
         ' '..#127: result += '\'+s[i];
         else result += '\x'+IntToHex(ord(s[i]),2);
       end;
       j := i;
     end;
  end;
  if i <> j then
    result := result + copy(S, 1 + j, i - j);
  result += StringDelimiter;
end;
