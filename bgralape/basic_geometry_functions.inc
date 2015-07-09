procedure MyFillRect{$i lape.proc}
begin
  target.FillRect(PInt32(Params^[0])^,PInt32(Params^[1])^,PInt32(Params^[2])^,PInt32(Params^[3])^,PBGRAPixel(Params^[4])^,TDrawMode(PInt32(Params^[5])^));
end;

procedure MyRectangle{$i lape.proc}
begin
  target.Rectangle(PInt32(Params^[0])^,PInt32(Params^[1])^,PInt32(Params^[2])^,PInt32(Params^[3])^,PBGRAPixel(Params^[4])^,TDrawMode(PInt32(Params^[5])^));
end;

procedure MyRectangleWithFill{$i lape.proc}
begin
  target.Rectangle(PInt32(Params^[0])^,PInt32(Params^[1])^,PInt32(Params^[2])^,PInt32(Params^[3])^,PBGRAPixel(Params^[4])^,PBGRAPixel(Params^[5])^,TDrawMode(PInt32(Params^[6])^));
end;


function GetSortedRect(x1,y1,x2,y2: integer): TRect;
begin
  if x1 > x2 then
  begin
    result.left := x2;
    result.right := x1;
  end else
  begin
    result.left := x1;
    result.right := x2;
  end;
  if y1 > y2 then
  begin
    result.top := y2;
    result.bottom := y1;
  end else
  begin
    result.top := y1;
    result.bottom := y2;
  end;
end;

procedure MyFillRoundRect{$i lape.proc}
var dm: TDrawMode;
begin
  dm := TDrawMode(PInt32(Params^[7])^);
  if (dm in[dmDrawWithTransparency,dmLinearBlend]) and PLongBool(Params^[8])^ then
  begin
    target.LinearAntialiasing:= (dm=dmLinearBlend);
    with GetSortedRect(PInt32(Params^[0])^,PInt32(Params^[1])^,PInt32(Params^[2])^,PInt32(Params^[3])^) do
     target.FillRoundRectAntialias(Left-0.5,top-0.5,right-0.5,bottom-0.5,PSingle(Params^[4])^,PSingle(Params^[5])^,PBGRAPixel(Params^[6])^,[]);
    target.LinearAntialiasing:= false;
  end
  else
    target.FillRoundRect(PInt32(Params^[0])^,PInt32(Params^[1])^,PInt32(Params^[2])^,PInt32(Params^[3])^,round(PSingle(Params^[4])^*2),round(PSingle(Params^[5])^*2),PBGRAPixel(Params^[6])^,dm);
end;

procedure MyRoundRect{$i lape.proc}
var dm: TDrawMode;
begin
  dm := TDrawMode(PInt32(Params^[7])^);
  if (dm in[dmDrawWithTransparency,dmLinearBlend]) and PLongBool(Params^[8])^ then
  begin
    target.LinearAntialiasing:= (dm=dmLinearBlend);
    with GetSortedRect(PInt32(Params^[0])^,PInt32(Params^[1])^,PInt32(Params^[2])^,PInt32(Params^[3])^) do
     if (right>left) and (bottom>top) then
     target.RoundRectAntialias(Left,top,right-1,bottom-1,PSingle(Params^[4])^,PSingle(Params^[5])^,PBGRAPixel(Params^[6])^,1,[]);
    target.LinearAntialiasing:= false;
  end
  else
    target.RoundRect(PInt32(Params^[0])^,PInt32(Params^[1])^,PInt32(Params^[2])^,PInt32(Params^[3])^,round(PSingle(Params^[4])^*2),round(PSingle(Params^[5])^*2),PBGRAPixel(Params^[6])^,dm);
end;

procedure MyRoundRectWithFill{$i lape.proc}
var dm: TDrawMode;
begin
  dm := TDrawMode(PInt32(Params^[8])^);
  if (dm in[dmDrawWithTransparency,dmLinearBlend]) and PLongBool(Params^[9])^ then
  begin
    target.LinearAntialiasing:= (dm=dmLinearBlend);
    with GetSortedRect(PInt32(Params^[0])^,PInt32(Params^[1])^,PInt32(Params^[2])^,PInt32(Params^[3])^) do
     if (right>left) and (bottom>top) then
     target.RoundRectAntialias(Left,top,right-1,bottom-1,PSingle(Params^[4])^,PSingle(Params^[5])^,PBGRAPixel(Params^[6])^,1,PBGRAPixel(Params^[7])^,[]);
    target.LinearAntialiasing:= false;
  end
  else
    target.RoundRect(PInt32(Params^[0])^,PInt32(Params^[1])^,PInt32(Params^[2])^,PInt32(Params^[3])^,round(PSingle(Params^[4])^*2),round(PSingle(Params^[5])^*2),PBGRAPixel(Params^[6])^,PBGRAPixel(Params^[7])^,dm);
end;



procedure MyFillEllipseInRect{$i lape.proc}
var dm: TDrawMode;
begin
  dm := TDrawMode(PInt32(Params^[5])^);
  if (dm in[dmDrawWithTransparency,dmLinearBlend]) and PLongBool(Params^[6])^ then
  begin
    target.LinearAntialiasing:= (dm=dmLinearBlend);
    with GetSortedRect(PInt32(Params^[0])^,PInt32(Params^[1])^,PInt32(Params^[2])^,PInt32(Params^[3])^) do
     target.FillEllipseAntialias((Left+right)*0.5-0.5,(top+bottom)*0.5-0.5,(right-left)*0.5,(bottom-top)*0.5,PBGRAPixel(Params^[4])^);
    target.LinearAntialiasing:= false;
  end
  else
    target.FillEllipseInRect(rect(PInt32(Params^[0])^,PInt32(Params^[1])^,PInt32(Params^[2])^,PInt32(Params^[3])^),PBGRAPixel(Params^[4])^,dm);
end;

procedure MyEllipseInRect{$i lape.proc}
var dm: TDrawMode;
begin
  dm := TDrawMode(PInt32(Params^[5])^);
  if (dm in[dmDrawWithTransparency,dmLinearBlend]) and PLongBool(Params^[6])^ then
  begin
    target.LinearAntialiasing:= (dm=dmLinearBlend);
    with GetSortedRect(PInt32(Params^[0])^,PInt32(Params^[1])^,PInt32(Params^[2])^,PInt32(Params^[3])^) do
     if (right>left) and (bottom>top) then
     target.EllipseAntialias((Left+right)*0.5-0.5,(top+bottom)*0.5-0.5,(right-left-1)*0.5,(bottom-top-1)*0.5,PBGRAPixel(Params^[4])^,1);
    target.LinearAntialiasing:= false;
  end
  else
    target.EllipseInRect(rect(PInt32(Params^[0])^,PInt32(Params^[1])^,PInt32(Params^[2])^,PInt32(Params^[3])^),PBGRAPixel(Params^[4])^,dm);
end;

procedure MyEllipseInRectWithFill{$i lape.proc}
var dm: TDrawMode;
begin
  dm := TDrawMode(PInt32(Params^[6])^);
  if (dm in[dmDrawWithTransparency,dmLinearBlend]) and PLongBool(Params^[7])^ then
  begin
    target.LinearAntialiasing:= (dm=dmLinearBlend);
    with GetSortedRect(PInt32(Params^[0])^,PInt32(Params^[1])^,PInt32(Params^[2])^,PInt32(Params^[3])^) do
     if (right>left) and (bottom>top) then
     target.EllipseAntialias((Left+right)*0.5-0.5,(top+bottom)*0.5-0.5,(right-left-1)*0.5,(bottom-top-1)*0.5,PBGRAPixel(Params^[4])^,1,PBGRAPixel(Params^[5])^);
    target.LinearAntialiasing:= false;
  end
  else
    target.EllipseInRect(rect(PInt32(Params^[0])^,PInt32(Params^[1])^,PInt32(Params^[2])^,PInt32(Params^[3])^),PBGRAPixel(Params^[4])^,PBGRAPixel(Params^[5])^,dm);
end;




procedure MyDrawLine{$i lape.proc}
var dm: TDrawMode;
begin
  dm := TDrawMode(PInt32(Params^[5])^);
  if (dm in[dmDrawWithTransparency,dmLinearBlend]) and PLongBool(Params^[6])^ then
  begin
    target.LinearAntialiasing:= (dm=dmLinearBlend);
    target.DrawLineAntialias(PInt32(Params^[0])^,PInt32(Params^[1])^,PInt32(Params^[2])^,PInt32(Params^[3])^,PBGRAPixel(Params^[4])^,true);
    target.LinearAntialiasing:= false;
  end
  else
    target.DrawLine(PInt32(Params^[0])^,PInt32(Params^[1])^,PInt32(Params^[2])^,PInt32(Params^[3])^,PBGRAPixel(Params^[4])^,true,dm);
end;

procedure MyEraseLine{$i lape.proc}
begin
  if PLongBool(Params^[5])^ then
    target.EraseLineAntialias(PInt32(Params^[0])^,PInt32(Params^[1])^,PInt32(Params^[2])^,PInt32(Params^[3])^,PByte(Params^[4])^,true)
  else
    target.EraseLine(PInt32(Params^[0])^,PInt32(Params^[1])^,PInt32(Params^[2])^,PInt32(Params^[3])^,PByte(Params^[4])^,true);
end;



procedure MyDrawPolyLine{$i lape.proc}
type
  ArrayOfTPoint = array of TPoint;
  PArrayOfTPoint = ^ArrayOfTPoint;
var
  pts: PArrayOfTPoint;
  dm: TDrawMode;
begin
  pts := Params^[0];
  dm := TDrawMode(PInt32(Params^[2])^);
  if (dm in[dmDrawWithTransparency,dmLinearBlend]) and PLongBool(Params^[3])^ then
  begin
    target.LinearAntialiasing:= (dm=dmLinearBlend);
    target.DrawPolyLineAntialias(pts^,PBGRAPixel(Params^[1])^,true);
    target.LinearAntialiasing:= false;
  end
  else
    target.DrawPolyLine(pts^,PBGRAPixel(Params^[1])^,true,dm);
end;

procedure MyErasePolyLine{$i lape.proc}
type
  ArrayOfTPoint = array of TPoint;
  PArrayOfTPoint = ^ArrayOfTPoint;
var
  pts: PArrayOfTPoint;
begin
  pts := Params^[0];
  if PLongBool(Params^[2])^ then
    target.ErasePolyLineAntialias(pts^,PByte(Params^[1])^,true)
  else
    target.ErasePolyLine(pts^,PByte(Params^[1])^,true);
end;

procedure MyErasePolygonOutline{$i lape.proc}
type
  ArrayOfTPoint = array of TPoint;
  PArrayOfTPoint = ^ArrayOfTPoint;
var
  pts: PArrayOfTPoint;
begin
  pts := Params^[0];
  if PLongBool(Params^[2])^ then
    target.ErasePolygonOutlineAntialias(pts^,PByte(Params^[1])^)
  else
    target.ErasePolygonOutline(pts^,PByte(Params^[1])^);
end;

procedure MyDrawPolygon{$i lape.proc}
type
  ArrayOfTPoint = array of TPoint;
  PArrayOfTPoint = ^ArrayOfTPoint;
var
  pts: PArrayOfTPoint;
  dm: TDrawMode;
begin
  pts := Params^[0];
  dm := TDrawMode(PInt32(Params^[2])^);
  if (dm in[dmDrawWithTransparency,dmLinearBlend]) and PLongBool(Params^[3])^ then
  begin
    target.LinearAntialiasing:= (dm=dmLinearBlend);
    target.DrawPolygonAntialias(pts^,PBGRAPixel(Params^[1])^);
    target.LinearAntialiasing:= false;
  end
  else
    target.DrawPolygon(pts^,PBGRAPixel(Params^[1])^,dm);
end;

procedure MyFillPoly{$i lape.proc}
type
  ArrayOfTPoint = array of TPoint;
  PArrayOfTPoint = ^ArrayOfTPoint;
var
  pts: PArrayOfTPoint;
  ptsF: ArrayOfTPointF;
  dm: TDrawMode;
  i: integer;
begin
  pts := Params^[0];
  setlength(ptsF, length(pts^));
  for i := 0 to high(pts^) do
    with pts^[i] do
      ptsF[i] := PointF(x,y);
  dm := TDrawMode(PInt32(Params^[2])^);
  if (dm in[dmDrawWithTransparency,dmLinearBlend]) and PLongBool(Params^[3])^ then
  begin
    target.LinearAntialiasing:= (dm=dmLinearBlend);
    target.FillPolyAntialias(ptsF,PBGRAPixel(Params^[1])^);
    target.LinearAntialiasing:= false;
  end
  else
    target.FillPoly(ptsF,PBGRAPixel(Params^[1])^,dm);
end;

procedure MyErasePoly{$i lape.proc}
type
  ArrayOfTPoint = array of TPoint;
  PArrayOfTPoint = ^ArrayOfTPoint;
var
  pts: PArrayOfTPoint;
  ptsF: ArrayOfTPointF;
  i: integer;
begin
  pts := Params^[0];
  setlength(ptsF, length(pts^));
  for i := 0 to high(pts^) do
    with pts^[i] do
      ptsF[i] := PointF(x,y);
  if PLongBool(Params^[2])^ then
    target.ErasePolyAntialias(ptsF,PByte(Params^[1])^)
  else
    target.ErasePoly(ptsF,PByte(Params^[1])^);
end;

procedure RegisterBasicGeometryFunctions(Compiler: TLapeCompiler);
begin
  Compiler.addGlobalFunc('procedure _FillRect(left, top, right, bottom: Int32; c: TBGRAPixel; ADrawMode: Int32);', @MyFillRect);
  Compiler.addGlobalFunc('procedure _Rectangle(left, top, right, bottom: Int32; c: TBGRAPixel; ADrawMode: Int32);', @MyRectangle);
  Compiler.addGlobalFunc('procedure _RectangleWithFill(left, top, right, bottom: Int32; c,fillcolor: TBGRAPixel; ADrawMode: Int32); overload;', @MyRectangleWithFill);

  Compiler.addGlobalFunc('procedure _FillRoundRect(left, top, right, bottom: Int32; rx,ry: single; c: TBGRAPixel; ADrawMode: Int32; AA: LongBool);', @MyFillRoundRect);
  Compiler.addGlobalFunc('procedure _RoundRect(left, top, right, bottom: Int32; rx,ry: single; c: TBGRAPixel; ADrawMode: Int32; AA: LongBool);', @MyRoundRect);
  Compiler.addGlobalFunc('procedure _RoundRectWithFill(left, top, right, bottom: Int32; rx,ry: single; c,fillcolor: TBGRAPixel; ADrawMode: Int32; AA: LongBool); overload;', @MyRoundRectWithFill);

  Compiler.addGlobalFunc('procedure _FillEllipseInRect(left, top, right, bottom: Int32; c: TBGRAPixel; ADrawMode: Int32; AA: LongBool);', @MyFillEllipseInRect);
  Compiler.addGlobalFunc('procedure _EllipseInRect(left, top, right, bottom: Int32; c: TBGRAPixel; ADrawMode: Int32; AA: LongBool);', @MyEllipseInRect);
  Compiler.addGlobalFunc('procedure _EllipseInRectWithFill(left, top, right, bottom: Int32; c,fillcolor: TBGRAPixel; ADrawMode: Int32; AA: LongBool); overload;', @MyEllipseInRectWithFill);

  Compiler.addGlobalFunc('procedure _DrawLine(x1,y1,x2,y2: Int32; c: TBGRAPixel; ADrawMode: Int32; AA: LongBool);', @MyDrawLine);
  Compiler.addGlobalFunc('procedure _EraseLine(x1,y1,x2,y2: Int32; alpha: byte; AA: LongBool);', @MyEraseLine);
  Compiler.addGlobalFunc('procedure _DrawPolyLine(const points: array of TPoint; c: TBGRAPixel; ADrawMode: Int32; AA: LongBool);', @MyDrawPolyLine);
  Compiler.addGlobalFunc('procedure _ErasePolyLine(const points: array of TPoint; alpha: byte; AA: LongBool);', @MyErasePolyLine);
  Compiler.addGlobalFunc('procedure _DrawPolygon(const points: array of TPoint; c: TBGRAPixel; ADrawMode: Int32; AA: LongBool);', @MyDrawPolygon);
  Compiler.addGlobalFunc('procedure _ErasePolygonOutline(const points: array of TPoint; alpha: byte; AA: LongBool);', @MyErasePolygonOutline);
  Compiler.addGlobalFunc('procedure _FillPoly(const points: array of TPoint; c: TBGRAPixel; ADrawMode: Int32; AA: LongBool);', @MyFillPoly);
  Compiler.addGlobalFunc('procedure _ErasePoly(const points: array of TPoint; alpha: byte; AA: LongBool);', @MyErasePoly);
end;
