unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Spin,
  ExtCtrls, StdCtrls, BGRAVirtualScreen, BGRABitmap, BGRABitmapTypes,
  BGRACanvas2D;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button_toDataURL: TButton;
    CheckBox_Antialiasing: TCheckBox;
    CheckBox_PixelCentered: TCheckBox;
    Panel1: TPanel;
    SpinEdit1: TSpinEdit;
    VirtualScreen: TBGRAVirtualScreen;
    Timer1: TTimer;
    procedure Button_toDataURLClick(Sender: TObject);
    procedure CheckBox_AntialiasingChange(Sender: TObject);
    procedure CheckBox_PixelCenteredChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseLeave(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure VirtualScreenMouseLeave(Sender: TObject);
    procedure VirtualScreenMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure VirtualScreenRedraw(Sender: TObject; Bitmap: TBGRABitmap);
  private
    { private declarations }
    mx,my: integer;
    test4pos, test5pos, Test13pos, test16pos, test17pos, test18pos, test19pos: integer;
    img,abelias: TBGRABitmap;
    procedure UpdateIn(ms: integer);
  public
    { public declarations }
    procedure Test1(ctx: TBGRACanvas2D);
    procedure Test2(ctx: TBGRACanvas2D);
    procedure Test3(ctx: TBGRACanvas2D);
    procedure Test4(ctx: TBGRACanvas2D);
    procedure Test5(ctx: TBGRACanvas2D);
    procedure Test6(ctx: TBGRACanvas2D);
    procedure Test7(ctx: TBGRACanvas2D);
    procedure Test8(ctx: TBGRACanvas2D);
    procedure Test9(ctx: TBGRACanvas2D);
    procedure Test10(ctx: TBGRACanvas2D);
    procedure Test11(ctx: TBGRACanvas2D);
    procedure Test12(ctx: TBGRACanvas2D);
    procedure Test13(ctx: TBGRACanvas2D);
    procedure Test14(ctx: TBGRACanvas2D);
    procedure Test15(ctx: TBGRACanvas2D);
    procedure Test16(ctx: TBGRACanvas2D);
    procedure Test17(ctx: TBGRACanvas2D);
    procedure Test18(ctx: TBGRACanvas2D);
    procedure Test19(ctx: TBGRACanvas2D);
  end;

var
  Form1: TForm1; 

implementation

uses BGRAGradientScanner, Math, BGRASVG;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormPaint(Sender: TObject);
begin
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  img := TBGRABitmap.Create('pteRaz.jpg');
  abelias := TBGRABitmap.Create('abelias.png');
  mx := -1000;
  my := -1000;
end;

procedure TForm1.CheckBox_PixelCenteredChange(Sender: TObject);
begin
  VirtualScreen.DiscardBitmap;
end;

procedure TForm1.Button_toDataURLClick(Sender: TObject);
var html: string;
    t: textfile;
begin
  html := '<html><body><img src="';
  html += VirtualScreen.Bitmap.Canvas2D.toDataURL;
  html += '"/></body></html>';
  assignfile(t,'dataUrlTest.html');
  rewrite(t);
  write(t,html);
  closefile(t);
  MessageDlg('toDataURL','Output: dataUrlTest.html',mtInformation,[mbOK],0);
end;

procedure TForm1.CheckBox_AntialiasingChange(Sender: TObject);
begin
  VirtualScreen.DiscardBitmap;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  img.Free;
  abelias.free;
end;

procedure TForm1.FormMouseLeave(Sender: TObject);
begin

end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin

end;

procedure TForm1.SpinEdit1Change(Sender: TObject);
begin
  VirtualScreen.DiscardBitmap;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := false;
  VirtualScreen.RedrawBitmap;
end;

procedure TForm1.VirtualScreenMouseLeave(Sender: TObject);
begin
  mx := -1000;
  my := -1000;
end;

procedure TForm1.VirtualScreenMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  mx := X;
  my := Y;
  if SpinEdit1.Value = 1 then UpdateIn(10);
end;

procedure TForm1.VirtualScreenRedraw(Sender: TObject; Bitmap: TBGRABitmap);
var ctx: TBGRACanvas2D;
begin
  ctx := Bitmap.Canvas2D;
  ctx.antialiasing := CheckBox_Antialiasing.Checked;
  ctx.pixelCenteredCoordinates := CheckBox_PixelCentered.Checked;
  ctx.save;
  case SpinEdit1.Value of
    1: Test1(ctx);
    2: Test2(ctx);
    3: Test3(ctx);
    4: Test4(ctx);
    5: Test5(ctx);
    6: Test6(ctx);
    7: Test7(ctx);
    8: Test8(ctx);
    9: Test9(ctx);
   10: Test10(ctx);
   11: Test11(ctx);
   12: Test12(ctx);
   13: Test13(ctx);
   14: Test14(ctx);
   15: Test15(ctx);
   16: Test16(ctx);
   17: Test17(ctx);
   18: Test18(ctx);
   19: Test19(ctx);
  end;
  ctx.restore;
end;

procedure TForm1.UpdateIn(ms: integer);
begin
  Timer1.Interval := ms;
  Timer1.Enabled := false;
  Timer1.Enabled := true;
end;

procedure TForm1.Test1(ctx: TBGRACanvas2D);
var
  colors: TBGRACustomGradient;
begin
  if (mx < 0) or (my < 0) then
  begin
    mx := ctx.Width div 2;
    my := ctx.height div 2;
  end;
  ctx.fillStyle('rgb(1000,1000,1000)');  //out of bounds so it is saturated to 255,255,255
  ctx.fillRect (0, 0, ctx.Width, ctx.Height);
  colors := TBGRAMultiGradient.Create([BGRA(0,255,0),BGRA(0,192,128),BGRA(0,255,0)],[0,0.5,1],True,True);
  ctx.fillStyle(ctx.createLinearGradient(0,0,20,0,colors));
  ctx.shadowOffset := PointF(5,5);
  ctx.shadowColor('rgba(0,0,0,0.5)');
  ctx.shadowBlur := 4;
  ctx.fillRect (mx-100, my-100, 200, 200);
  colors.Free;
end;

procedure TForm1.Test2(ctx: TBGRACanvas2D);
var layer: TBGRABitmap;
begin
  layer := TBGRABitmap.Create(ctx.width,ctx.height);
  with layer.Canvas2D do
  begin
    fillStyle('rgb(1000,0,0)'); // fond de couleur rouge
    beginPath;
    roundRect(25,25,Width-50,Height-50,25); // remplissage d un carré 250x250
    fill;

    clearRect(Width-mx-25,Height-my-25,50,50); // effacement d un carré

    beginPath;
    arc(mx,my,30,0,2*Pi);
    clearPath;

    strokeStyle ('rgb(0,0,1000)'); // contour de couleur bleue
    strokeRect(100,100,20,20); // contour d un carré

    shadowOffset := PointF(3,3);
    shadowColor('rgba(0,0,0,0.5)');
    shadowBlur := 4;

    beginPath;
    lineWidth := 3;
    moveTo(20,160);
    lineTo(200,160);
    lineStyle([3,1]);
    stroke;

    beginPath;
    moveTo(20,180);
    lineTo(220,180);
    lineTo(240,160);
    lineStyle([1,1,2,2]);
    stroke;
  end;
  ctx.surface.PutImage(0,0,layer,dmDrawWithTransparency);
  layer.Free;
  UpdateIn(10);
end;

procedure TForm1.Test3(ctx: TBGRACanvas2D);
begin
  ctx.fillStyle ('rgb(1000,1000,1000)');
  ctx.fillRect (0, 0, ctx.width, ctx.height);
  // Triangle plein sans bordure
  ctx.beginPath();
  ctx.moveTo(100,100);
  ctx.lineTo(150,30);
  ctx.lineTo(230,150);
  ctx.closePath();
  if ctx.isPointInPath(mx+0.5,my+0.5) then
    ctx.fillStyle ('rgb(1000,192,192)')
  else
    ctx.fillStyle ('rgb(1000,0,0)');
  ctx.fill();
  // Triangle plein avec bordure
  ctx.fillStyle ('rgb(0,1000,0)');
  ctx.strokeStyle ('rgb(0,0,1000)');
  ctx.lineWidth := 8;
  ctx.beginPath();
  ctx.moveTo(50,100);
  ctx.lineTo(50,220);
  ctx.lineTo(210,200);
  ctx.closePath();
  if ctx.isPointInPath(mx+0.5,my+0.5) then
    ctx.fillStyle ('rgb(192,1000,192)')
  else
    ctx.fillStyle ('rgb(0,1000,0)');
  ctx.fill();
  ctx.stroke();
  // Triangle plein avec bordure
  UpdateIn(50);
end;

procedure TForm1.Test4(ctx: TBGRACanvas2D);
var angle: single;
    p0,p1,p2: TPointF;
begin
  inc(test4pos);
  angle := test4pos*2*Pi/400;
  ctx.translate((ctx.Width-300)/2,(ctx.height-300)/2);
  ctx.skewx( sin(angle) );

  ctx.beginPath;
  ctx.rect (0, 0, 300, 300);
  ctx.fillStyle (CSSYellow);
  ctx.strokeStyle(CSSRed);
  ctx.lineWidth := 5;
  ctx.strokeOverFill;

  ctx.beginPath();
  // coord. centre 150,150  rayon : 50 angle départ 0 fin 2Pi, sens
  ctx.arc(150,150,50,0,PI*2,true); // Cercle
  ctx.moveTo(100,150); // aller au pt de départ de l arc
  ctx.arc(100,100,50,PI/2,PI,false); // Arc sens aig. montre
  ctx.moveTo(150,150); // aller au pt de départ de l arc
  ctx.arc(200,150,50,2*PI/2,0,false);  // Autre cercle
  ctx.lineWidth := 1;
  ctx.strokeStyle(BGRABlack);
  ctx.stroke();

  ctx.lineJoin := 'round';

  angle := test4pos*2*Pi/180;
  p0 := PointF(150,50);
  p1 := pointF(150+50,50);
  p2 := pointF(150+50+cos(sin(angle)*Pi/2)*40,50+sin(sin(angle)*Pi/2)*40);
  ctx.beginPath;
  ctx.moveTo(p0);
  ctx.arcTo(p1, p2, 30);
  ctx.lineTo(p2);
  ctx.lineWidth := 5;
  ctx.strokeStyle( BGRA(240,170,0) );
  ctx.stroke();

  ctx.beginPath;
  ctx.moveTo(p0);
  ctx.lineTo(p1);
  ctx.lineTo(p2);
  ctx.strokeStyle( BGRA(0,0,255) );
  ctx.lineWidth := 2;
  ctx.stroke();

  UpdateIn(10);
end;

procedure TForm1.Test5(ctx: TBGRACanvas2D);
var svg: TBGRASVG;
begin
  inc(test5pos);

  svg := TBGRASVG.Create;
  svg.LoadFromFile('Amsterdammertje-icoon.svg');
  svg.StretchDraw(ctx, taCenter,tlCenter, 0,0,ctx.Width/3,ctx.Height);

  svg.LoadFromFile('BespectacledMaleUser.svg');
  svg.StretchDraw(ctx, ctx.Width/3,0,ctx.Width*2/3,ctx.Height/2);

  ctx.save;
  ctx.beginPath;
  ctx.rect(ctx.Width/3,ctx.Height/2,ctx.Width*2/3,ctx.Height/2);
  ctx.clip;
  svg.LoadFromFile('Blue_gyroelongated_pentagonal_pyramid.svg');
  svg.Draw(ctx, taCenter,tlCenter, ctx.Width*2/3,ctx.Height*3/4);
  ctx.restore;

  svg.Free;

  ctx.beginPath;
  ctx.lineWidth:= 1;
  ctx.strokeStyle(BGRABlack);
  ctx.moveTo(ctx.Width/3,0);
  ctx.lineTo(ctx.Width/3,ctx.Height);
  ctx.moveTo(ctx.Width/3,ctx.Height/2);
  ctx.lineTo(ctx.Width,ctx.Height/2);
  ctx.stroke;

  UpdateIn(20);
end;

procedure TForm1.Test6(ctx: TBGRACanvas2D);
begin
  ctx.fillStyle ('rgb(1000,1000,1000)');
  ctx.fillRect (0, 0, 300, 300);
// Exemple de courbes de Bézier
  ctx.fillStyle ( 'yellow');
  ctx.lineWidth := 15;
  ctx.lineCap := 'round'; // round butt square
  ctx.lineJoin := 'miter'; // round miter bevel
  ctx.strokeStyle ('rgb(200,200,1000)');
  ctx.beginPath();
  ctx.moveTo(50,150);
  ctx.bezierCurveTo(50,80,100,60,130,60);
  ctx.bezierCurveTo(180,60,250,50,260,130);
  ctx.bezierCurveTo(150,150,150,150,120,280);
  ctx.bezierCurveTo(50,250,100,200,50,150);
  ctx.fill();
  ctx.stroke();
end;

procedure TForm1.Test7(ctx: TBGRACanvas2D);
var
  i: Integer;
begin
  ctx.fillStyle('black');
  ctx.fillRect(0, 0, 300, 300);
  // Dessin du fond
  ctx.fillStyle ('red');
  ctx.fillRect(0, 0, 150, 150);
  ctx.fillStyle ('blue');
  ctx.fillRect(150, 0, 150, 150);
  ctx.fillStyle ('yellow');
  ctx.fillRect(0, 150, 150, 150);
  ctx.fillStyle ('green');
  ctx.fillRect(150, 150, 150, 150);
  ctx.fillStyle ('#FFF');
  // Définition de la valeur de transparence
  ctx.globalAlpha := 0.1;
  // Dessin de carrés semi transparents
  for i := 0 to 9 do
  begin
      ctx.beginPath();
      ctx.fillRect(10*i, 10*i, 300-20*i, 300-20*i);
      ctx.fill();
  end;
end;

procedure TForm1.Test8(ctx: TBGRACanvas2D);
begin
  ctx.drawImage(img, 0, 0);
  ctx.globalAlpha:= 0.5;
  ctx.drawImage(img, 100, 100);
  ctx.globalAlpha := 0.9;
  ctx.translate(100,100);
  ctx.beginPath;
  ctx.moveTo(50,50);
  ctx.lineTo(300,50);
  ctx.lineTo(500,200);
  ctx.lineTo(50,200);
  ctx.fillStyle(img);
  ctx.fill;
end;

procedure TForm1.Test9(ctx: TBGRACanvas2D);
var
  i: Integer;
  j: Integer;
begin
  ctx.translate(ctx.Width/2 -15*10, ctx.Height/2 -15*10);
  ctx.strokeStyle ('#000');
  ctx.lineWidth :=4;
  for i := 0 to 14 do
    for j := 0 to 14 do
    begin
      ctx.fillStyle (BGRA ( 255-18*i, 255-18*j, 0) );
      ctx.strokeStyle (BGRA( 20+10*j, 20+8*i, 0) );
      ctx.fillRect(j*20, i*20, 20, 20);
      ctx.strokeRect(j*20, i*20, 20, 20)
    end;
end;

procedure TForm1.Test10(ctx: TBGRACanvas2D);
var
  i: Integer;
  j: Integer;
begin
  ctx.translate(ctx.Width/2, ctx.Height/2);  // centre 0 0 maintenant en position centrale
  for i := 1 to 9 do
  begin
    ctx.save(); // contrebalancé par un restore
    ctx.fillStyle ( BGRA(25*i,255-25*i,255) );
    for j := 0 to i*5 do
    begin
      ctx.rotate(PI*2/(1+i*5)); //
      ctx.beginPath();
      ctx.arc(0, i*16, 6, 0, PI*2, true);
      ctx.fill();
    end;
    ctx.restore();
  end;
end;

procedure TForm1.Test11(ctx: TBGRACanvas2D);
const sc=20;  // nb de pixels pour une unité

var
  H: LongInt;
  W: LongInt;
  i: Integer;
  x,u: Single;

  function f(x: single): single; // fonction à tracer
  begin
    result := 3*sin(x)*(cos(x)+1/2*cos(x/2)+1/3*cos(x/3)+1/4*cos(x/4));
  end;
begin
  H := ctx.height;
  W := ctx.width;
   // tracé du quadrillage
   ctx.strokeStyle ('#666');
   ctx.beginPath();
   ctx.lineWidth:=0.5;
   // lignes horizontales
   for i := -trunc(H/2/sc) to trunc(H/2/sc) do
   begin
     ctx.moveTo(0, H/2-sc*i);
     ctx.lineTo(W, H/2-sc*i);
   end;
   // lignes verticales
   for i := 0 to trunc(W/sc) do
   begin
     ctx.moveTo(sc*i,H-0);
     ctx.lineTo(sc*i, H-H);
   end;
   ctx.stroke();
   // tracé de la fonction
     ctx.strokeStyle ('#ff0000');
     ctx.lineWidth:=1.5;
     ctx.beginPath();
     x:=0;
     u:=f(x);
     ctx.moveTo(0, H/2-u*sc);
     while x < W/sc do
     begin
       u := f(x);
       ctx.lineTo(x*sc, H/2-u*sc);
       x += 1/sc;
     end;
   ctx.stroke();
end;

procedure TForm1.Test12(ctx: TBGRACanvas2D);
var
  W: LongInt;
  H: LongInt;
  i: Integer;
  j: Integer;

  function color(): TBGRAPixel;
  begin
    result := BGRA(random(256),random(256),random(256));
  end;

  procedure drawSpirograph(R2: single; r: single; O: single);
  var
    x0,x1,x2: single;
    y0,y1,y2: single;
    i: integer;
  begin
    x0 := R2-O;
    y0 := 0;
    i  := 1;
    ctx.beginPath();
    x1 := x0;
    y1 := y0;
    ctx.moveTo(x1, y1);
    repeat
      if (i > 1000) then break;
      x2 := (R2+r)*cos(i*PI/72) - (r+O)*cos(((R2+r)/r)*(i*PI/72));
      y2 := (R2+r)*sin(i*PI/72) - (r+O)*sin(((R2+r)/r)*(i*PI/72));
      ctx.lineTo(x2, y2);
      x1 := x2;
      y1 := y2;
      inc(i);
    until (abs(x2-x0) < 1e-6) and (abs(y2-y0) < 1e-6);
    ctx.stroke();
  end;

begin
  W := ctx.width;
  H := ctx.height;
  ctx.fillRect(0, 0, W, H);
  for i := 0 to 1 do
    for j := 0 to 2 do
    begin
      ctx.save();
      ctx.strokeStyle ( color() );
      ctx.translate(110+j*200, 100+i*160);
      drawSpirograph(40*(j+2)/(j+1), -(3+random(11))*(i+3)/(i+1), 35);
      ctx.restore();
    end;

  UpdateIn(3000);
end;

procedure TForm1.Test13(ctx: TBGRACanvas2D);
const vitesse = 1;
begin
  ctx.fillStyle ('#000');
  ctx.fillRect (0, 0, 800, 400);
  ctx.clearRect(0, 0, 800, 400);
  ctx.fillRect (0, 0, 800, 400);
  ctx.setTransform(-0.55, 0.85, -1, 0.10, 100, 50+img.width*0.5);
  ctx.rotate(PI*2*(Test13pos/360)*vitesse );
  ctx.drawImage(img, img.width*(-0.5)-200, img.height*(-0.8));
  Test13pos+=1;
  if (Test13pos=360) then Test13pos := 0;
  UpdateIn(10);
end;

procedure TForm1.Test14(ctx: TBGRACanvas2D);
  procedure pave();
  begin
    ctx.save();
    ctx.fillStyle ('rgb(130,100,800)');
    ctx.strokeStyle ('rgb(0,0,300)');
    ctx.beginPath();
    ctx.lineWidth:=2;
    ctx.moveTo(5,5);ctx.lineTo(20,10);ctx.lineTo(55,5);ctx.lineTo(45,18);ctx.lineTo(30,50);
    ctx.closePath();
    ctx.stroke();
    ctx.fill();
    ctx.fillStyle ('rgb(300,300,100)');
    ctx.lineWidth:=5;
    ctx.strokeStyle ('rgb(0,300,0)');
    ctx.beginPath();
    ctx.moveTo(20,18);ctx.lineTo(40,16);ctx.lineTo(35,26); ctx.lineTo(25,30);
    ctx.closePath();
    ctx.stroke();
    ctx.fill();
    ctx.restore();
  end;
  // dessins d un hexagone à partir de six pavés par rotation
  procedure six();
  var
    i: Integer;
  begin
     ctx.save();
     for i := 0 to 5 do
     begin
        ctx.rotate(2*PI/6);
        pave();
     end;
     ctx.restore();
  end;
  // pavage utilisant des translations selon deux vecteurs non colinéaires
  // 0,60*Math.sqrt(3)     et     60*3/2, 60*Math.sqrt(3)/2
  procedure draw();
  var
    i: Integer;
    j: Integer;
  begin
      ctx.fillStyle ('rgb(800,100,50)');
      ctx.fillRect (0, 0, ctx.Width, ctx.Height);
      for j := 0 to (ctx.Width+60) div 90 do
      begin
        ctx.save();
        ctx.translate(0,(-j div 2)*60*sqrt(3));
        for i := 0 to round(ctx.Height / (60*sqrt(3))) do
        begin
          six();
          ctx.translate(0,60*sqrt(3));
        end;

        ctx.restore();
        ctx.translate(90, sqrt(3)*60/2);
      end;
  end;

begin
  draw();
end;

procedure TForm1.Test15(ctx: TBGRACanvas2D);
const cote = 190;

  procedure pave();
  begin
    ctx.drawImage(abelias,0,0);
  end;

  procedure refl();
  begin
     ctx.save();
     pave();
     ctx.transform(1,0,0,-1, 0, 0);
     pave();
     ctx.restore();
  end;

  // dessins d un hexagone à partir de six pavés par rotation
  procedure trois();
  var
    i: Integer;
  begin
     ctx.save();
     for i := 0 to 2 do
     begin
        ctx.rotate(4*PI/6);
        refl();
     end;
     ctx.restore();
  end;

  // pavage utilisant des translations selon deux vecteurs non colinéaires
  // 0,cote*Math.sqrt(3)     et     cote*3/2, cote*Math.sqrt(3)/2
  procedure draw();
  var
    i: Integer;
    j: Integer;
  begin
    ctx.fillStyle ('#330055');
    ctx.fillRect (0, 0, ctx.width, ctx.height);
    ctx.translate(140,140);
    for j := 0 to trunc(ctx.Width /(cote*3/2)) do
    begin
      ctx.save();
      ctx.translate(0,-(1/2 + j div 2)*cote*sqrt(3));
      for i := 0 to trunc(ctx.Height / (cote*sqrt(3)))+1 do
      begin
        trois();
        ctx.translate(0,cote*sqrt(3));
      end;
      ctx.restore();
      ctx.translate(cote*3/2, sqrt(3)*cote/2);
    end;
  end;

begin
  draw();
end;

procedure TForm1.Test16(ctx: TBGRACanvas2D);
var center: TPointF;
    angle,zoom: single;
begin
  inc(test16pos);
  center := pointf(ctx.width/2,ctx.height/2);
  angle := test16pos*2*Pi/300;
  zoom := (sin(test16pos*2*Pi/400)+1.1)*min(ctx.width,ctx.height)/300;
  with ctx do
  begin
    translate(center.X,center.Y);
    scale(zoom,zoom);
    rotate(angle);
    translate(-93,-83);
    beginPath();
    moveTo(89.724698,11.312043);
    bezierCurveTo(95.526308,14.494575,100.52322000000001,18.838808,102.75144,24.966412);
    bezierCurveTo(114.24578,26.586847,123.07072,43.010127999999995,118.71826,54.504664);
    bezierCurveTo(114.77805000000001,64.910473,93.426098,68.10145299999999,89.00143800000001,59.252123);
    bezierCurveTo(86.231818,53.712894999999996,90.877898,48.213108999999996,88.853498,42.139906999999994);
    bezierCurveTo(87.401408,37.78364299999999,82.208048,33.87411899999999,85.595888,27.098436999999993);
    bezierCurveTo(87.071858,24.146481999999992,94.76621800000001,25.279547999999995,94.863658,23.444067999999994);
    bezierCurveTo(95.066728,19.618834999999994,92.648878,18.165403999999995,90.221828,15.326465999999995);
    closePath();
    moveTo(53.024288,20.876975);
    bezierCurveTo(50.128958,26.827119000000003,48.561707999999996,33.260252,50.284608,39.548662);
    bezierCurveTo(41.840728,47.513997,44.130318,66.017003,54.325338,72.88213300000001);
    bezierCurveTo(63.554708000000005,79.09700300000002,82.823918,69.36119300000001,81.320528,59.58223300000001);
    bezierCurveTo(80.379498,53.461101000000006,73.409408,51.65791100000001,71.551608,45.53168800000001);
    bezierCurveTo(70.219018,41.13739400000001,72.197818,34.94548700000001,65.517188,31.373877000000007);
    bezierCurveTo(62.606638000000004,29.817833000000007,56.98220800000001,35.18931200000001,55.841908000000004,33.74771500000001);
    bezierCurveTo(53.465478000000004,30.743354000000007,54.598668,28.159881000000006,54.938648,24.44039800000001);
    closePath();
    moveTo(16.284108,78.650993);
    bezierCurveTo(16.615938,85.259863,18.344168,91.651623,22.885208,96.330453);
    bezierCurveTo(19.327327999999998,107.37975,30.253377999999998,122.48687000000001,42.495058,123.58667);
    bezierCurveTo(53.577238,124.58229,65.765908,106.76307,59.734438,98.920263);
    bezierCurveTo(55.959047999999996,94.01106300000001,48.983098,95.791453,44.402058,91.319753);
    bezierCurveTo(41.116108,88.112233,39.864737999999996,81.73340300000001,32.289848,81.824883);
    bezierCurveTo(28.989708,81.864783,26.651538,89.282293,24.957518,88.569003);
    bezierCurveTo(21.427108,87.08246299999999,21.174458,84.272723,19.679208,80.85010299999999);
    closePath();
    moveTo(152.77652,37.616125);
    bezierCurveTo(156.68534,42.955439,159.37334,49.006564,158.79801,55.501293);
    bezierCurveTo(168.5256,61.835313,169.5682,80.450283,160.75895,89.021463);
    bezierCurveTo(152.78409,96.780823,132.08894,90.63274299999999,131.82654,80.742363);
    bezierCurveTo(131.6623,74.551503,138.19976,71.535693,138.93671,65.17653299999999);
    bezierCurveTo(139.46532,60.615162999999995,136.41531,54.87470199999999,142.35299,50.170306999999994);
    bezierCurveTo(144.93985,48.12074299999999,151.43107,52.404562999999996,152.29636,50.78291599999999);
    bezierCurveTo(154.09968999999998,47.403324999999995,152.52446999999998,45.062994999999994,151.52745,41.463536999999995);
    closePath();
    moveTo(139.65359,109.38478);
    bezierCurveTo(179.13505,123.79982000000001,142.51298,146.31478,119.19800000000001,151.55864);
    bezierCurveTo(95.883018,156.8025,41.93790800000001,157.82316,75.508908,123.02183);
    bezierCurveTo(78.980078,119.42344999999999,79.61785800000001,104.19731999999999,82.074898,99.283253);
    bezierCurveTo(86.361158,93.329663,106.23528,86.908083,113.13709,88.929193);
    bezierCurveTo(128.23085,93.960443,125.96716,106.89633,139.65359,109.38478);
    closePath();
    if isPointInPath(mx+0.5,my+0.5) then
      fillStyle ('#6faed9')
    else
      fillStyle ('#3f5e99');
    fill();
  end;
  UpdateIn(10);
end;

procedure TForm1.Test17(ctx: TBGRACanvas2D);
var
  grad: IBGRACanvasGradient2D;
  angle: single;
begin
  inc(test17pos);
  angle := test17pos*2*Pi/1000;

  ctx.translate(ctx.Width/2,ctx.Height/2);
  ctx.scale(min(ctx.Width,ctx.Height)/2-10);
  ctx.rotate(angle);

  grad := ctx.createLinearGradient(-1,-1,1,1);
  grad.addColorStop(0.3, '#ff0000');
  grad.addColorStop(0.6, '#0000ff');
  ctx.fillStyle(grad);

  grad := ctx.createLinearGradient(-1,-1,1,1);
  grad.addColorStop(0.3, '#ffffff');
  grad.addColorStop(0.6, '#000000');
  ctx.strokeStyle(grad);
  ctx.lineWidth := 5;

  ctx.beginPath;
  ctx.moveto(0,0);
  ctx.arc(0,0,1,Pi/6,-Pi/6,false);
  ctx.fill();
  ctx.stroke();

  UpdateIn(10);
end;

procedure TForm1.Test18(ctx: TBGRACanvas2D);
var pat: TBGRABitmap;
begin
  inc(test18pos);
  ctx.translate(ctx.width div 2, ctx.height div 2);
  ctx.rotate(test18pos*2*Pi/360);
  ctx.scale(3,3);
  pat := TBGRABitmap.Create(8,8);
  pat.GradientFill(0,0,8,8,BGRABlack,BGRAWhite,gtLinear,PointF(0,0),PointF(8,8),dmSet);
//  ctx.surface.CreateBrushTexture(bsDiagCross,BGRA(255,255,0),BGRA(255,0,0)) as TBGRABitmap;
  ctx.fillStyle(ctx.createPattern(pat,'repeat-x'));
  ctx.fillRect(0,0,ctx.width,pat.height-1);
  ctx.fillStyle(ctx.createPattern(pat,'repeat-y'));
  ctx.fillRect(0,0,pat.width-1,ctx.height);

  ctx.rotate(Pi);
  ctx.globalAlpha:= 0.25;
  ctx.fillStyle(ctx.createPattern(pat,'repeat-x'));
  ctx.fillRect(0,0,ctx.width,ctx.height);
  ctx.fillStyle(ctx.createPattern(pat,'repeat-y'));
  ctx.fillRect(0,0,ctx.width,ctx.height);
  pat.free;

  UpdateIn(10);
end;

procedure TForm1.Test19(ctx: TBGRACanvas2D);
var i: integer;
    tx,ty: single;
begin
  inc(test19pos);
  ctx.save;
  ctx.translate(ctx.width div 2, ctx.height div 2);
  ctx.rotate(test19pos*2*Pi/500);
  ctx.scale(ctx.height / 2,ctx.height / 2);
  ctx.beginPath;
  ctx.moveto(1,0);
  for i := 1 to 8 do
  begin
    ctx.rotate(2*Pi/8);
    ctx.lineto(1,0);
  end;
  ctx.restore;
  ctx.clip;

  tx := ctx.width div 2;
  ty := ctx.height div 2;
  ctx.fillStyle ('red');
  ctx.fillRect(0, 0, tx, ty);
  ctx.fillStyle ('blue');
  ctx.fillRect(tx, 0, tx, ty);

  ctx.globalAlpha := 0.75;
  ctx.fillStyle ('yellow');
  ctx.fillRect(0, ty, tx, ty);
  ctx.fillStyle ('green');
  ctx.fillRect(tx, ty, tx, ty);

  test18(ctx);
end;

end.

