// SPDX-License-Identifier: GPL-3.0-only
unit UParse;

{$mode objfpc}

interface

uses
  Classes, SysUtils; 

type
  arrayOfString = array of string;

function RectToStr(rect: TRect): string;
function StrToRect(str: string): TRect;
function SimpleParseFuncParam(str: string): arrayOfString;

implementation

function SimpleParseFuncParam(str: string): arrayOfString;
var idxOpen,start,cur,bracketDepth: integer;
begin
    result := nil;
    idxOpen := pos('(',str);
    if idxOpen = 0 then
      start := 1
    else
      start := idxOpen+1;
    cur := start;
    bracketDepth := 0;
    while cur <= length(str) do
    begin
       if str[cur] = '(' then inc(bracketDepth) else
       if (str[cur] = ')') and (bracketDepth > 0) then dec(bracketDepth) else
       if str[cur] in[',',')'] then
       begin
         setlength(result,length(result)+1);
         result[high(result)] := copy(str,start,cur-start);
         start := cur+1;
         if str[cur]=')' then break;
       end;
       inc(cur);
    end;
    if start <= length(str) then
    begin
      setlength(result,length(result)+1);
      result[high(result)] := copy(str,start,length(str)-start+1);
    end;
end;

function RectToStr(rect: TRect): string;
begin
  result := 'Rect('+intToStr(rect.left)+','+intToStr(rect.Top)+','+intToStr(rect.Right)+','+intToStr(rect.Bottom)+')';
end;

{$hints off}
{$notes off}
function StrToRect(str: string): TRect;
var param: arrayOfString;
    errPos: integer;
begin
  if lowercase(copy(str,1,5))='rect(' then
  begin
    param := SimpleParseFuncParam(str);
    if length(param)=4 then
    begin
      val(param[0],result.left,errPos);
      val(param[1],result.top,errPos);
      val(param[2],result.right,errPos);
      val(param[3],result.bottom,errPos);
      exit;
    end;
  end;
  fillchar(result,sizeof(result),0);
end;
{$notes on}
{$hints on}


end.

