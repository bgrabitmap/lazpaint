unit image_filters2_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, BGRABitmap, BGRABitmapTypes, EpikTimer;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    Label_Ms: TLabel;
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    Radio_Linear: TRadioButton;
    Radio_Mitchell: TRadioButton;
    Radio_None: TRadioButton;
    Radio_HalfCosine: TRadioButton;
    Radio_Cosine: TRadioButton;
    Radio_Bicubic: TRadioButton;
    Radio_BoxFilter: TRadioButton;
    Radio_Spline: TRadioButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure Radio_Change(Sender: TObject);
  private
    { private declarations }
    procedure GetFilter(out filter : TResampleFilter; out noFilter: boolean);
  public
    { public declarations }
    image: TBGRABitmap;
    stopwatch : TEpikTimer;
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

uses BGRAResample;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  image := TBGRABitmap.Create(4,4,BGRAWhite);
  image.SetPixel(0,0,clRed);
  image.SetPixel(2,0,clBlack);
  image.SetPixel(3,0,clLime);
  image.SetPixel(2,1,clRed);
  image.SetPixel(3,1,clBlue);
  image.SetPixel(0,2,clBlue);
  image.SetPixel(1,2,clBlack);
  image.SetPixel(0,3,clLime);
  image.SetPixel(1,3,clRed);
  image.SetPixel(3,3,clBlue);
  stopwatch := TEpikTimer.Create(self);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  image.Free;
end;

procedure TForm1.FormPaint(Sender: TObject);
var resampled: TBGRABitmap;
    tx,ty: integer;
    filter : TResampleFilter;
    noFilter: boolean;
begin
  tx := ClientWidth-Panel1.Width;
  ty := ClientHeight;
  if tx > ty then tx := ty else ty := tx;

  GetFilter(filter, noFilter);
  stopwatch.Clear;
  stopwatch.Start;
  if noFilter then
    resampled := image.Resample(tx,ty,rmSimpleStretch) as TBGRABitmap else
  begin
     image.ResampleFilter := filter;
     resampled := image.Resample(tx,ty) as TBGRABitmap;
  end;
  stopwatch.stop;
  Label_Ms.Caption := IntToStr(round(stopwatch.Elapsed*1000))+' ms';
  resampled.Draw(Canvas,Panel1.width,0);
  resampled.Free;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
const
    minx = -3;
    maxx = 3;
    miny = -1;
    maxy = 2;
var
  graph: TBGRABitmap;
  curve: array of TPointF;
  tx,ty,precision: integer;
  filter : TResampleFilter;
  noFilter: boolean;
  i,numPt: integer;
  filterPos,filterValue: single;
  kernel: TWideKernelFilter;

  function XToGraph(value: single): single;
  begin
    result := (value-minx)/(maxx-minx)*(tx-1);
  end;
  function YToGraph(value: single): single;
  begin
    result := ty-1-(value-miny)/(maxy-miny)*(ty-1);
  end;

begin
  tx := paintbox1.width;
  ty := paintbox1.height;
  if (tx=0) or (ty=0) then exit;
  graph := TBGRABitmap.Create(tx,ty,BGRAWhite);
  precision := tx div (maxx-minx)+1;
  setlength(curve,precision*(maxx-minx)+1);
  GetFilter(filter, noFilter);
  numPt := 0;
  case filter of
    rfBicubic: kernel := TCubicKernel.Create;
    rfSpline: kernel := TSplineKernel.Create;
    rfMitchell: kernel := TMitchellKernel.Create;
  else
    kernel := nil;
  end;
  for i := minx*precision to maxx*precision do
  begin
    filterPos := i/precision;
    if noFilter then
    begin
       if (filterPos >= 0) and (filterPos < 1) then
         filterValue := 1
       else
         filterValue := 0;
    end else
    begin
      if kernel = nil then
      begin
         if abs(filterPos) > 1 then
           filterValue := 0
         else
           filterValue := 1-FineInterpolation(abs(filterPos),filter);
      end
      else
        filterValue := kernel.Interpolation(filterPos);
    end;
    curve[numPt] := PointF(XToGraph(filterPos),YToGraph(filterValue));
    inc(numPt);
  end;
  kernel.Free;
  graph.DrawHorizLine(0,round(YToGraph(0)),tx-1,BGRA(0,0,0,96));
  graph.DrawVertLine(round(XToGraph(0)),0,ty-1,BGRA(0,0,0,96));
  graph.DrawLineAntialias(XToGraph(0)-8,YToGraph(1),XToGraph(0)+8,YToGraph(1),BGRA(0,0,0,96),1);
  graph.DrawLineAntialias(XToGraph(-1),YToGraph(0)-8,XToGraph(-1),YToGraph(0)+8,BGRA(0,0,0,96),1);
  graph.DrawLineAntialias(XToGraph(1),YToGraph(0)-8,XToGraph(1),YToGraph(0)+8,BGRA(0,0,0,96),1);
  graph.DrawPolyLineAntialias(curve,BGRA(192,0,0),1);
  graph.Draw(paintbox1.Canvas,0,0);
  graph.free;
end;

procedure TForm1.Radio_Change(Sender: TObject);
begin
  Invalidate;
  PaintBox1.Invalidate;
end;

procedure TForm1.GetFilter(out filter: TResampleFilter; out noFilter: boolean);
begin
  noFilter := Radio_None.Checked;
  if Radio_BoxFilter.Checked then filter := rfBox else
  if Radio_Bicubic.Checked then filter := rfBicubic else
  if Radio_Cosine.Checked then filter := rfCosine else
  if Radio_HalfCosine.Checked then filter := rfHalfCosine else
  if Radio_Mitchell.Checked then filter := rfMitchell else
  if Radio_Spline.Checked then filter := rfSpline else
    filter := rfLinear;
end;

end.

