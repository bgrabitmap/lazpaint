var
  textAlignment: TAlignment;
  textLayout: TTextLayout;

procedure MySetFontName{$i lape.proc}
begin
  target.FontName := PlpString(Params^[0])^;
end;

procedure MySetFontStyle{$i lape.proc}
var fs: TFontStyles;
begin
  fs := [];
  if PLongBool(Params^[0])^ then fs += [fsBold];
  if PLongBool(Params^[1])^ then fs += [fsItalic];
  if PLongBool(Params^[2])^ then fs += [fsStrikeOut];
  if PLongBool(Params^[3])^ then fs += [fsUnderline];
  target.FontStyle := fs;
end;

procedure MySetTextAlignment{$i lape.proc}
begin
  textAlignment:= TAlignment(PInt32(Params^[0])^);
end;

procedure MySetTextLayout{$i lape.proc}
begin
  textLayout:= TTextLayout(PInt32(Params^[0])^);
end;

procedure MyGetFontFullHeight{$i lape.func}
begin
  Int32(Result^) := target.FontFullHeight;
end;

procedure MySetFontFullHeight{$i lape.proc}
begin
  target.FontFullHeight := PInt32(Params^[0])^;
end;

procedure MySetFontEmHeight{$i lape.proc}
begin
  target.FontHeight := PInt32(Params^[0])^;
end;

procedure MyGetFontEmHeight{$i lape.func}
begin
  Int32(Result^) := target.FontHeight;
end;

procedure MyGetTextWidth{$i lape.func}
begin
  Int32(Result^) := target.TextSize(PlpString(Params^[0])^).cx;
end;

procedure MyTextOut{$i lape.proc}
var y: single;
begin
  y := PSingle(Params^[1])^;
  if textLayout = tlCenter then y -= target.FontFullHeight/2
  else if textLayout = tlBottom then y -= target.FontFullHeight;
  target.TextOut(PSingle(Params^[0])^,y,PlpString(Params^[2])^,PBGRAPixel(Params^[3])^,textAlignment);
end;

procedure MyTextRect{$i lape.proc}
var r: TRect;
begin
  r := rect(PInt32(Params^[0])^,PInt32(Params^[1])^,PInt32(Params^[2])^,round(PInt32(Params^[3])^));
  target.TextRect(r,PlpString(Params^[4])^,textAlignment,textLayout,PBGRAPixel(Params^[5])^);
end;

procedure MyTextOutAngle{$i lape.proc}
var x,y,h,angle: single;
begin
  x := PSingle(Params^[0])^;
  y := PSingle(Params^[1])^;
  if textLayout <> tlTop then
  begin
    h := target.FontFullHeight;
    if textLayout = tlCenter then h *= 0.5;
    angle := round(PSingle(Params^[2])^)*Pi/1800 + Pi/2;
    x += cos(angle)*h;
    y -= sin(angle)*h;
  end;
  target.TextOutAngle(x,y,round(PSingle(Params^[2])^),PlpString(Params^[3])^,PBGRAPixel(Params^[4])^,textAlignment);
end;

procedure RegisterTextFunctions(Compiler: TLapeCompiler);
begin
  Compiler.addGlobalFunc('procedure _SetFontName(s: string);', @MySetFontName);
  Compiler.addGlobalFunc('procedure _SetFontStyle(ABold, AItalic, AStrikeOut, AUnderline: LongBool);', @MySetFontStyle);
  Compiler.addGlobalFunc('procedure _SetTextAlignment(AAlign: Int32);', @MySetTextAlignment);
  Compiler.addGlobalFunc('procedure _SetTextLayout(ALayout: Int32);', @MySetTextLayout);
  Compiler.addGlobalFunc('function TextWidth(s: string) : Int32;', @MyGetTextWidth);
  Compiler.addGlobalFunc('function GetFontFullHeight : Int32;', @MyGetFontFullHeight);
  Compiler.addGlobalFunc('function GetFontEmHeight : Int32;', @MyGetFontEmHeight);
  Compiler.addGlobalFunc('procedure SetFontFullHeight(AValue : Int32);', @MySetFontFullHeight);
  Compiler.addGlobalFunc('procedure SetFontEmHeight(AValue : Int32);', @MySetFontEmHeight);
  Compiler.addGlobalFunc('procedure TextOut(x, y: single; sUTF8: string; c: TBGRAPixel);', @MyTextOut);
  Compiler.addGlobalFunc('procedure TextOutAngle(x, y, angle: single; sUTF8: string; c: TBGRAPixel);', @MyTextOutAngle);
  Compiler.addGlobalFunc('procedure TextRect(left, top, right, bottom: integer; sUTF8: string; c: TBGRAPixel);', @MyTextRect);
end;
