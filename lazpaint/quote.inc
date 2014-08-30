function ScriptUnquote(const S: string): string;
var errors: TInterpretationErrors;
begin
  errors := TryScriptUnquote(s,result);
  if errors <> [] then
    raise exception.create('Invalid quoted string (error '+inttostr(integer(errors))+')');
end;

function TryScriptUnquote(const S: String; out unquotedS: string): TInterpretationErrors;
var curPos,quoteStart,idStart: integer; idStr, charCodeStr: string;
  charFuncStep: (fsNone, fsWaitOpenBracket, fsCharCodeParam, fsWaitCloseBraket);

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
    if s[curPos] = StringDelimiter then
    begin
      quoteStart := curPos;
      inc(curPos);
      while true do
      begin
        if curPos <= length(s) then
        begin
          if s[curPos]=StringDelimiter then
          begin
            unquotedS:= unquotedS+copy(s,quoteStart+1,curPos-quoteStart-1);
            inc(curPos);
            if (curPos <= length(s)) and (s[curPos]=StringDelimiter) then
            begin
              unquotedS:= unquotedS+StringDelimiter;
              quoteStart := curPos;
            end
            else
            begin
              quoteStart := curPos-1;
              break;
            end;
          end;
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
  NonPrintableChars = [#0,#7..#13,#26,#27];
var i, j, count: integer;
  inQuote: boolean;

  procedure EnterQuote;
  begin
    if not inQuote then
    begin
      If result <> '' then result := result + ' & ';
      result := result+StringDelimiter;
      inQuote := true;
    end;
  end;

  procedure FlushChars;
  var NbFlush: integer;
  begin
    NbFlush := i - j - 1;
    if NbFlush <= 0 then exit;
    EnterQuote;
    result := result + copy(S, 1 + j, NbFlush);
    j := i;
  end;

  procedure LeaveQuote;
  begin
    if inQuote then
    begin
      result := result+StringDelimiter;
      inQuote := false;
    end;
  end;

begin
  result := '';
  inQuote := false;
  count := length(s);
  i := 0;
  j := 0;
  while i < count do begin
     i := i + 1;
     if s[i] in NonPrintableChars then
     begin
       FlushChars;
       LeaveQuote;
       If result <> '' then result := result + ' & ';
       result := result+'Chr('+IntToStr(Ord(s[i]))+')';
       j := i;
     end;
     if S[i] = StringDelimiter then begin
        FlushChars;
        result := result+ StringDelimiter+StringDelimiter;
        end ;
     end ;
  if i <> j then
  begin
     EnterQuote;
     result := result + copy(S, 1 + j, i - j);
  end;
  LeaveQuote;
  if result = '' then result := StringDelimiter+StringDelimiter;
end;
