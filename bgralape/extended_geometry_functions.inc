procedure MyFillRectF{$i lape.proc}
var dm: TDrawMode; aa: boolean;
  x1,y1,x2,y2: single;
begin
  dm := TDrawMode(PInt32(Params^[5])^);
  aa := PLongBool(Params^[6])^;
  x1 := PSingle(Params^[0])^;
  y1 := PSingle(Params^[1])^;
  x2 := PSingle(Params^[2])^;
  y2 := PSingle(Params^[3])^;
  if (dm in[dmDrawWithTransparency,dmLinearBlend]) and aa then
  begin
    target.LinearAntialiasing:= (dm=dmLinearBlend);
    target.FillRectAntialias(x1,y1,x2,y2,PBGRAPixel(Params^[4])^);
    target.LinearAntialiasing:= false;
  end
  else
    target.FillPoly([PointF(x1,y1),PointF(x2,y1),PointF(x2,y2),PointF(x1,y2)],PBGRAPixel(Params^[4])^,dm);
end;

procedure MyRectangleF{$i lape.proc}
var dm: TDrawMode; aa: boolean;
  x1,y1,x2,y2: single;
begin
  dm := TDrawMode(PInt32(Params^[6])^);
  aa := PLongBool(Params^[7])^;
  x1 := PSingle(Params^[0])^;
  y1 := PSingle(Params^[1])^;
  x2 := PSingle(Params^[2])^;
  y2 := PSingle(Params^[3])^;
  if (dm in[dmDrawWithTransparency,dmLinearBlend]) and aa then
  begin
    target.LinearAntialiasing:= (dm=dmLinearBlend);
    target.RectangleAntialias(x1,y1,x2,y2,PBGRAPixel(Params^[4])^,PSingle(Params^[5])^);
    target.LinearAntialiasing:= false;
  end
  else
    target.FillPoly(target.ComputeWidePolygon([PointF(x1,y1),PointF(x2,y1),PointF(x2,y2),PointF(x1,y2)],PSingle(Params^[5])^),PBGRAPixel(Params^[4])^,dm);
end;

procedure MyRectangleWithFillF{$i lape.proc}
var dm: TDrawMode; aa: boolean;
  x1,y1,x2,y2: single;
  m: TBGRAMultishapeFiller;
begin
  dm := TDrawMode(PInt32(Params^[7])^);
  aa := PLongBool(Params^[8])^;
  x1 := PSingle(Params^[0])^;
  y1 := PSingle(Params^[1])^;
  x2 := PSingle(Params^[2])^;
  y2 := PSingle(Params^[3])^;
  if (dm in[dmDrawWithTransparency,dmLinearBlend]) and aa then
  begin
    target.LinearAntialiasing:= (dm=dmLinearBlend);
    target.RectangleAntialias(x1,y1,x2,y2,PBGRAPixel(Params^[4])^,PSingle(Params^[5])^,PBGRAPixel(Params^[6])^);
    target.LinearAntialiasing:= false;
  end
  else
  begin
    m := TBGRAMultishapeFiller.Create;
    m.PolygonOrder:= poLastOnTop;
    m.Antialiasing := false;
    m.AddRectangle(x1,y1,x2,y2,PBGRAPixel(Params^[6])^);
    m.AddPolygon(target.ComputeWidePolygon([PointF(x1,y1),PointF(x2,y1),PointF(x2,y2),PointF(x1,y2)],PSingle(Params^[5])^),PBGRAPixel(Params^[4])^);
    m.Draw(target, dm);
    m.Free;
  end;
end;



procedure MyFillRoundRectF{$i lape.proc}
var dm: TDrawMode; aa: boolean;
  x1,y1,x2,y2: single;
  fill: TFillShapeInfo;
begin
  dm := TDrawMode(PInt32(Params^[7])^);
  aa := PLongBool(Params^[8])^;
  x1 := PSingle(Params^[0])^;
  y1 := PSingle(Params^[1])^;
  x2 := PSingle(Params^[2])^;
  y2 := PSingle(Params^[3])^;
  if (dm in[dmDrawWithTransparency,dmLinearBlend]) and aa then
  begin
    target.LinearAntialiasing:= (dm=dmLinearBlend);
    target.FillRoundRectAntialias(x1,y1,x2,y2,PSingle(Params^[4])^,PSingle(Params^[5])^,PBGRAPixel(Params^[6])^,[]);
    target.LinearAntialiasing := false;
  end
  else
  begin
    fill := TFillRoundRectangleInfo.Create(x1,y1,x2,y2,PSingle(Params^[4])^,PSingle(Params^[5])^,[]);
    target.FillShape(fill,PBGRAPixel(Params^[6])^,dm);
    fill.Free;
  end;
end;

procedure MyRoundRectF{$i lape.proc}
var dm: TDrawMode; aa: boolean;
  x1,y1,x2,y2: single;
begin
  dm := TDrawMode(PInt32(Params^[8])^);
  aa := PLongBool(Params^[9])^;
  x1 := PSingle(Params^[0])^;
  y1 := PSingle(Params^[1])^;
  x2 := PSingle(Params^[2])^;
  y2 := PSingle(Params^[3])^;
  if (dm in[dmDrawWithTransparency,dmLinearBlend]) and aa then
  begin
    target.LinearAntialiasing:= (dm=dmLinearBlend);
    target.RoundRectAntialias(x1,y1,x2,y2,PSingle(Params^[4])^,PSingle(Params^[5])^,PBGRAPixel(Params^[6])^,PSingle(Params^[7])^,[]);
    target.LinearAntialiasing := false;
  end
  else
    target.FillPoly(target.ComputeWidePolygon(target.ComputeRoundRect(x1,y1,x2,y2,PSingle(Params^[4])^,PSingle(Params^[5])^,[]),PSingle(Params^[7])^),PBGRAPixel(Params^[6])^,dm);
end;

procedure MyRoundRectWithFillF{$i lape.proc}
var dm: TDrawMode; aa: boolean;
  x1,y1,x2,y2: single;
  m: TBGRAMultishapeFiller;
begin
  dm := TDrawMode(PInt32(Params^[9])^);
  aa := PLongBool(Params^[10])^;
  x1 := PSingle(Params^[0])^;
  y1 := PSingle(Params^[1])^;
  x2 := PSingle(Params^[2])^;
  y2 := PSingle(Params^[3])^;
  if (dm in[dmDrawWithTransparency,dmLinearBlend]) and aa then
  begin
    target.LinearAntialiasing:= (dm=dmLinearBlend);
    target.RoundRectAntialias(x1,y1,x2,y2,PSingle(Params^[4])^,PSingle(Params^[5])^,PBGRAPixel(Params^[6])^,PSingle(Params^[7])^,PBGRAPixel(Params^[8])^,[]);
    target.LinearAntialiasing := false;
  end
  else
  begin
    m := TBGRAMultishapeFiller.Create;
    m.PolygonOrder:= poLastOnTop;
    m.Antialiasing := false;
    m.AddRoundRectangle(x1,y1,x2,y2,PSingle(Params^[4])^,PSingle(Params^[5])^,PBGRAPixel(Params^[8])^);
    m.AddPolygon(target.ComputeWidePolygon(target.ComputeRoundRect(x1,y1,x2,y2,PSingle(Params^[4])^,PSingle(Params^[5])^,[]),PSingle(Params^[7])^),PBGRAPixel(Params^[6])^);
    m.Draw(target, dm);
    m.Free;
  end;
end;



procedure MyFillEllipseF{$i lape.proc}
var dm: TDrawMode; aa: boolean;
  x,y,rx,ry: single;
  fill: TFillShapeInfo;
begin
  dm := TDrawMode(PInt32(Params^[5])^);
  aa := PLongBool(Params^[6])^;
  x := PSingle(Params^[0])^;
  y := PSingle(Params^[1])^;
  rx := PSingle(Params^[2])^;
  ry := PSingle(Params^[3])^;
  if (dm in[dmDrawWithTransparency,dmLinearBlend]) and aa then
  begin
    target.LinearAntialiasing:= (dm=dmLinearBlend);
    target.FillEllipseAntialias(x,y,rx,ry,PBGRAPixel(Params^[4])^);
    target.LinearAntialiasing:= false;
  end
  else
  begin
    if (rx = 0) or (ry = 0) or (x = EmptySingle) or (y = EmptySingle) then exit;
    fill := TFillEllipseInfo.Create(x,y,rx,ry);
    target.FillShape(fill, PBGRAPixel(Params^[4])^,dm);
    fill.Free;
  end;
end;

procedure MyEllipseF{$i lape.proc}
var dm: TDrawMode; aa: boolean;
  x,y,rx,ry: single;
begin
  dm := TDrawMode(PInt32(Params^[6])^);
  aa := PLongBool(Params^[7])^;
  x := PSingle(Params^[0])^;
  y := PSingle(Params^[1])^;
  rx := PSingle(Params^[2])^;
  ry := PSingle(Params^[3])^;
  if (dm in[dmDrawWithTransparency,dmLinearBlend]) and aa then
  begin
    target.LinearAntialiasing:= (dm=dmLinearBlend);
    target.EllipseAntialias(x,y,rx,ry,PBGRAPixel(Params^[4])^,PSingle(Params^[5])^);
    target.LinearAntialiasing:= false;
  end
  else
    target.FillPoly(target.ComputeEllipseBorder(x,y,rx,ry,PSingle(Params^[5])^),PBGRAPixel(Params^[4])^,dm);
end;

procedure MyEllipseWithFillF{$i lape.proc}
var dm: TDrawMode; aa: boolean;
  x,y,rx,ry: single;
  m: TBGRAMultishapeFiller;
begin
  dm := TDrawMode(PInt32(Params^[7])^);
  aa := PLongBool(Params^[8])^;
  x := PSingle(Params^[0])^;
  y := PSingle(Params^[1])^;
  rx := PSingle(Params^[2])^;
  ry := PSingle(Params^[3])^;
  if (dm in[dmDrawWithTransparency,dmLinearBlend]) and aa then
  begin
    target.LinearAntialiasing:= (dm=dmLinearBlend);
    target.EllipseAntialias(x,y,rx,ry,PBGRAPixel(Params^[4])^,PSingle(Params^[5])^,PBGRAPixel(Params^[6])^);
    target.LinearAntialiasing:= false;
  end
  else
  begin
    m := TBGRAMultishapeFiller.Create;
    m.PolygonOrder:= poLastOnTop;
    m.Antialiasing := false;
    m.AddEllipse(x,y,rx,ry,PBGRAPixel(Params^[6])^);
    m.AddPolygon(target.ComputeEllipseBorder(x,y,rx,ry,PSingle(Params^[5])^),PBGRAPixel(Params^[4])^);
    m.Draw(target, dm);
    m.Free;
  end;
end;


procedure MyDrawLineF{$i lape.proc}
var dm: TDrawMode; aa: boolean;
  x1,y1,x2,y2: single;
begin
  dm := TDrawMode(PInt32(Params^[6])^);
  aa := PLongBool(Params^[7])^;
  x1 := PSingle(Params^[0])^;
  y1 := PSingle(Params^[1])^;
  x2 := PSingle(Params^[2])^;
  y2 := PSingle(Params^[3])^;
  if (dm in[dmDrawWithTransparency,dmLinearBlend]) and aa then
    target.DrawLineAntialias(x1,y1,x2,y2,PBGRAPixel(Params^[4])^,PSingle(Params^[5])^)
  else
    target.FillPoly(target.ComputeWidePolyline([PointF(x1,y1),PointF(x2,y2)],PSingle(Params^[5])^),PBGRAPixel(Params^[4])^,dm);
end;

procedure MyDrawPolyLineF{$i lape.proc}
type
  PArrayOfTPointF = ^ArrayOfTPointF;
var
  pts: PArrayOfTPointF;
  dm: TDrawMode; aa: boolean;
begin
  pts := Params^[0];
  dm := TDrawMode(PInt32(Params^[3])^);
  aa := PLongBool(Params^[4])^;
  if (dm in[dmDrawWithTransparency,dmLinearBlend]) and aa then
    target.DrawPolyLineAntialias(pts^,PBGRAPixel(Params^[1])^,PSingle(Params^[2])^)
  else
    target.FillPoly(target.ComputeWidePolyline(pts^,PSingle(Params^[2])^),PBGRAPixel(Params^[1])^,dm);
end;

procedure MyDrawPolygonF{$i lape.proc}
type
  PArrayOfTPointF = ^ArrayOfTPointF;
var
  pts: PArrayOfTPointF;
  dm: TDrawMode; aa: boolean;
begin
  pts := Params^[0];
  dm := TDrawMode(PInt32(Params^[3])^);
  aa := PLongBool(Params^[4])^;
  if (dm in[dmDrawWithTransparency,dmLinearBlend]) and aa then
    target.DrawPolygonAntialias(pts^,PBGRAPixel(Params^[1])^,PSingle(Params^[2])^)
  else
    target.FillPoly(target.ComputeWidePolygon(pts^,PSingle(Params^[2])^),PBGRAPixel(Params^[1])^,dm);
end;

procedure MyDrawPolyLineWithFillF{$i lape.proc}
type
  PArrayOfTPointF = ^ArrayOfTPointF;
var
  pts: PArrayOfTPointF;
  dm: TDrawMode; aa: boolean;
  multi: TBGRAMultishapeFiller;
begin
  pts := Params^[0];
  dm := TDrawMode(PInt32(Params^[4])^);
  aa := PLongBool(Params^[5])^;
  if (dm in[dmDrawWithTransparency,dmLinearBlend]) and aa then
    target.DrawPolyLineAntialias(pts^,PBGRAPixel(Params^[1])^,PSingle(Params^[2])^,PBGRAPixel(Params^[3])^)
  else
  begin
    multi := TBGRAMultishapeFiller.Create;
    multi.PolygonOrder := poLastOnTop;
    multi.Antialiasing := false;
    multi.AddPolygon(pts^,PBGRAPixel(Params^[3])^);
    multi.AddPolygon(target.ComputeWidePolyline(pts^,PSingle(Params^[2])^),PBGRAPixel(Params^[1])^);
    multi.Draw(target,dm);
    multi.Free;
  end;
end;

procedure MyDrawPolygonWithFillF{$i lape.proc}
type
  PArrayOfTPointF = ^ArrayOfTPointF;
var
  pts: PArrayOfTPointF;
  dm: TDrawMode; aa: boolean;
  multi: TBGRAMultishapeFiller;
begin
  pts := Params^[0];
  dm := TDrawMode(PInt32(Params^[4])^);
  aa := PLongBool(Params^[5])^;
  if (dm in[dmDrawWithTransparency,dmLinearBlend]) and aa then
    target.DrawPolygonAntialias(pts^,PBGRAPixel(Params^[1])^,PSingle(Params^[2])^,PBGRAPixel(Params^[3])^)
  else
  begin
    multi := TBGRAMultishapeFiller.Create;
    multi.PolygonOrder := poLastOnTop;
    multi.Antialiasing := false;
    multi.AddPolygon(pts^,PBGRAPixel(Params^[3])^);
    multi.AddPolygon(target.ComputeWidePolygon(pts^,PSingle(Params^[2])^),PBGRAPixel(Params^[1])^);
    multi.Draw(target,dm);
    multi.Free;
  end;
end;

procedure MyFillPolyF{$i lape.proc}
type
  PArrayOfTPointF = ^ArrayOfTPointF;
var
  pts: PArrayOfTPointF;
  dm: TDrawMode; aa: boolean;
begin
  pts := Params^[0];
  dm := TDrawMode(PInt32(Params^[2])^);
  aa := PLongBool(Params^[3])^;
  if (dm in[dmDrawWithTransparency,dmLinearBlend]) and aa then
    target.FillPolyAntialias(pts^,PBGRAPixel(Params^[1])^)
  else
    target.FillPoly(pts^,PBGRAPixel(Params^[1])^,dm);
end;

procedure RegisterExtendedGeometryFunctions(Compiler: TLapeCompiler);
begin
  Compiler.addGlobalFunc('procedure _FillRectF(left, top, right, bottom: single; c: TBGRAPixel; ADrawMode: Int32; AA: LongBool);', @MyFillRectF);
  Compiler.addGlobalFunc('procedure _RectangleF(left, top, right, bottom: single; c: TBGRAPixel; w: single; ADrawMode: Int32; AA: LongBool);', @MyRectangleF);
  Compiler.addGlobalFunc('procedure _RectangleF(left, top, right, bottom: single; c: TBGRAPixel; w: single; fillcolor: TBGRAPixel; ADrawMode: Int32; AA: LongBool); overload;', @MyRectangleWithFillF);

  Compiler.addGlobalFunc('procedure _FillRoundRectF(left, top, right, bottom, rx,ry: single; c: TBGRAPixel; ADrawMode: Int32; AA: LongBool);', @MyFillRoundRectF);
  Compiler.addGlobalFunc('procedure _RoundRectF(left, top, right, bottom, rx,ry: single; c: TBGRAPixel; w: single; ADrawMode: Int32; AA: LongBool);', @MyRoundRectF);
  Compiler.addGlobalFunc('procedure _RoundRectF(left, top, right, bottom, rx,ry: single; c: TBGRAPixel; w: single; fillcolor: TBGRAPixel; ADrawMode: Int32; AA: LongBool); overload;', @MyRoundRectWithFillF);

  Compiler.addGlobalFunc('procedure _FillEllipseF(x,y,rx,ry: single; c: TBGRAPixel; ADrawMode: Int32; AA: LongBool);', @MyFillEllipseF);
  Compiler.addGlobalFunc('procedure _EllipseF(x,y,rx,ry: single; c: TBGRAPixel; w: single; ADrawMode: Int32; AA: LongBool);', @MyEllipseF);
  Compiler.addGlobalFunc('procedure _EllipseF(x,y,rx,ry: single; c: TBGRAPixel; w: single; fillcolor: TBGRAPixel; ADrawMode: Int32; AA: LongBool); overload;', @MyEllipseWithFillF);

  Compiler.addGlobalFunc('procedure _DrawLineF(x1,y1,x2,y2: single; c: TBGRAPixel; w: single; ADrawMode: Int32; AA: LongBool);', @MyDrawLineF);
  Compiler.addGlobalFunc('procedure _DrawPolyLineF(const pts: array of TPointF; c: TBGRAPixel; w: single; ADrawMode: Int32; AA: LongBool);', @MyDrawPolyLineF);
  Compiler.addGlobalFunc('procedure _DrawPolygonF(const pts: array of TPointF; c: TBGRAPixel; w: single; ADrawMode: Int32; AA: LongBool);', @MyDrawPolygonF);
  Compiler.addGlobalFunc('procedure _DrawPolyLineF(const pts: array of TPointF; c: TBGRAPixel; w: single; fillcolor: TBGRAPixel; ADrawMode: Int32; AA: LongBool); overload;', @MyDrawPolyLineWithFillF);
  Compiler.addGlobalFunc('procedure _DrawPolygonF(const pts: array of TPointF; c: TBGRAPixel; w: single; fillcolor: TBGRAPixel; ADrawMode: Int32; AA: LongBool); overload;', @MyDrawPolygonWithFillF);
  Compiler.addGlobalFunc('procedure _FillPolyF(const pts: array of TPointF; c: TBGRAPixel; ADrawMode: Int32; AA: LongBool);', @MyFillPolyF);
end;
