// SPDX-License-Identifier: GPL-3.0-only
unit uadjustcurves;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, BGRAVirtualScreen, LazPaintType,
  UFilterConnector, uscripting, BGRABitmap, BGRABitmapTypes, UFilterThread;

const
  RedTab = 0;
  GreenTab = 1;
  BlueTab = 2;
  HueTab = 3;
  SaturationTab = 4;
  LightnessTab = 5;
  AlphaTab = 6;
  CurveTabCount = 7;
  HistogramBarCount = 30;

type
  { TFAdjustCurves }

  TFAdjustCurves = class(TForm)
    Panel2: TPanel;
    Timer_Chart: TTimer;
    Timer_Thread: TTimer;
    ToolBar8: TToolBar;
    ToolButton_Posterize: TToolButton;
    ToolButton_RemovePoint: TToolButton;
    ToolButton_NewCurve: TToolButton;
    vsChart: TBGRAVirtualScreen;
    Button_Cancel: TButton;
    Button_OK: TButton;
    Panel1: TPanel;
    TabControl1: TTabControl;
    procedure Button_OKClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
    procedure Timer_ChartTimer(Sender: TObject);
    procedure Timer_ThreadTimer(Sender: TObject);
    procedure ToolButton_PosterizeClick(Sender: TObject);
    procedure ToolButton_RemovePointClick(Sender: TObject);
    procedure ToolButton_NewCurveClick(Sender: TObject);
    procedure vsChartMouseDown(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure vsChartMouseMove(Sender: TObject; {%H-}Shift: TShiftState; X,
      Y: Integer);
    procedure vsChartMouseUp(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure vsChartRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    FInstance: TLazPaintCustomInstance;
    FFilterConnector: TFilterConnector;
    FPreviousGraphWidth, FPreviousGraphHeight: integer;
    FPreviousDarkTheme: boolean;
    FGraphBackgroundLeft, FGraphBackgroundBottom: TBGRABitmap;
    FPoint0,FPoint1: TPointF;
    FNbGrid: integer;
    FGridColor: TBGRAPixel;
    FTickSize: integer;
    FPointSize, FCurvePenWidth: single;
    FScaling: single;
    FHueTabPrecomputed: boolean;
    FSelectedPoint: integer;
    FMovingPoint: boolean;
    FEffectUpdated: boolean;
    FIgnoreInput: boolean;
    FThreadManager: TFilterThreadManager;
    FHistogram: array[0..CurveTabCount-1, 0..HistogramBarCount-1] of integer;
    FHistogramComputed, FHistogramDelay: array[0..CurveTabCount-1] of boolean;
    FHistogramMax: array[0..CurveTabCount-1] of integer;
    FHistogramRedraw: boolean;
    function GetCurveColor: TBGRAPixel;
    function GetCurveName: string;
    function GetHueTabSelected: boolean;
    function GetNbPoints: integer;
    function GetPosterize: boolean;
    function GetSelectedCurve: TVariableSet;
    function GetSelectedTab: integer;
    procedure OnTryStopAction({%H-}Sender: TFilterConnector);
    procedure RecomputeGraphBackground(AWidth,AHeight: integer);
    function CoordToBitmap(x,y: single): TPointF;
    function BitmapToCoord(x,y: single): TPointF;
    function NearestPoint(xBmp,yBmp: single): integer;
    function InsertPoint(xBmp,yBmp: single): integer;
    procedure EnsureCurveExist(AParameters: TVariableSet; AName: string);
    procedure SetPointCoord(AIndex: integer; ACoord: TPointF);
    function RemovePoint(AIndex :integer): boolean;
    procedure SetPosterize(AValue: boolean);
    procedure ThemeChanged(Sender: TObject);
    procedure TryRemovePoint;
    procedure PreviewNeeded;
    function NeedHistogram(ATab: integer): boolean;
    procedure DiscardHistogram;
    procedure QueryHistogramRedraw;
    procedure OnTaskEvent({%H-}ASender: TObject; AEvent: TThreadManagerEvent);
  public
    { public declarations }
    function ShowModal: integer; override;
    function ShowModal(AInstance: TLazPaintCustomInstance;
      AParameters: TVariableSet): integer;
    property SelectedTab: integer read GetSelectedTab;
    property SelectedCurve: TVariableSet read GetSelectedCurve;
    property HueTabSelected: boolean read GetHueTabSelected;
    property CurveName: string read GetCurveName;
    property CurveColor: TBGRAPixel read GetCurveColor;
    property Posterize: boolean read GetPosterize write SetPosterize;
    property NbPoints: integer read GetNbPoints;
  end;

implementation

uses UResourceStrings, LCLType, UMac, Math, LCScaleDPI, BGRAGradientScanner,
  UColorFilters;

{ TFAdjustCurves }

procedure TFAdjustCurves.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  ScaleControl(Self, OriginalDPI);
  vsChart.BitmapAutoScale:= false;

  CheckOKCancelBtns(Button_OK, Button_Cancel);
  FSelectedPoint:= -1;
  FThreadManager := nil;

  for i := 0 to TabControl1.PageCount-1 do
  begin
    if TabControl1.Page[i].Caption = 'Red' then TabControl1.Page[i].Caption := rsRed else
    if TabControl1.Page[i].Caption = 'Green' then TabControl1.Page[i].Caption := rsGreen else
    if TabControl1.Page[i].Caption = 'Blue' then TabControl1.Page[i].Caption := rsBlue else
    if TabControl1.Page[i].Caption = 'Alpha' then TabControl1.Page[i].Caption := rsOpacity else
    if TabControl1.Page[i].Caption = 'Hue' then TabControl1.Page[i].Caption := rsHue else
    if TabControl1.Page[i].Caption = 'Saturation' then TabControl1.Page[i].Caption := rsSaturation else
    if TabControl1.Page[i].Caption = 'Lightness' then TabControl1.Page[i].Caption := rsLightness;
  end;
end;

procedure TFAdjustCurves.vsChartRedraw(Sender: TObject; Bitmap: TBGRABitmap);
var
  i: Integer;
  curve: TVariableSet;
  pointList: TScriptVariableReference;
  pointCount, histoTab: integer;
  pts,symbols: array of TPointF;
  symbolsColor: array of TBGRAPixel;
  hueGradient: TBGRAHueGradient;
  gradientScanner: TBGRAGradientScanner;
  hsla: THSLAPixel;
  c1,c2: TBGRAPixel;
begin
  if (Bitmap.Width <> FPreviousGraphWidth) or
   (Bitmap.Height <> FPreviousGraphHeight) or
    (FGraphBackgroundLeft = nil) or (FGraphBackgroundBottom = nil) or
    (FHueTabPrecomputed <> HueTabSelected)  or
    (FPreviousDarkTheme <> (assigned(FInstance) and FInstance.DarkTheme)) then
      RecomputeGraphBackground(Bitmap.Width,Bitmap.Height);
  if (FGraphBackgroundLeft = nil) or (FGraphBackgroundBottom = nil) then exit;
  Bitmap.PutImage(0,0,FGraphBackgroundLeft,dmSet);
  Bitmap.PutImage(FGraphBackgroundLeft.Width,Bitmap.Height-FGraphBackgroundBottom.Height,FGraphBackgroundBottom,dmSet);
  for i := 1 to FNbGrid do
  begin
      with CoordToBitmap(i/FNbGrid,i/FNbGrid) do
      begin
        Bitmap.DrawLineAntialias(System.round(FPoint0.x),System.round(y),Bitmap.Width,System.round(y),FGridColor,BGRAPixelTransparent,FTickSize,false);
        Bitmap.DrawLineAntialias(System.round(x),System.round(FPoint0.y),System.round(x),0,FGridColor,BGRAPixelTransparent,FTickSize,false);
      end;
  end;
  histoTab := SelectedTab;
  if FHistogramDelay[histoTab] then
  begin
    FHistogramDelay[histoTab] := false;
    QueryHistogramRedraw;
  end
  else
  begin
    if NeedHistogram(histoTab) and
      (FHistogramMax[histoTab] > 0) then
    begin
      for i := 0 to HistogramBarCount-1 do
        Bitmap.FillRectAntialias(
           RectF(CoordToBitmap(i/HistogramBarCount,
                               FHistogram[histoTab, i]/FHistogramMax[histoTab]),
                 CoordToBitmap((i+0.9)/HistogramBarCount,
                               0)), BGRA(0,0,0,96) );
    end;
  end;
  curve := SelectedCurve;
  if Assigned(curve) then
  begin
    pointList := curve.GetVariable('Points');
    pointCount := curve.GetListCount(pointList);
    setlength(symbols, pointCount);
    setlength(symbolsColor, pointCount);
    for i := 0 to pointCount-1 do
    begin
      with curve.GetPoint2DAt(pointList,i) do
        symbols[i] := CoordToBitmap(x, y);
      symbolsColor[i] := CurveColor;
    end;
    if curve.Booleans['Posterize'] then
    begin
      setlength(pts, pointCount*2);
      for i := 0 to pointCount-2 do
      begin
        pts[2*i] := symbols[i];
        pts[2*i+1].x := symbols[i+1].x;
        pts[2*i+1].y := symbols[i].y;
      end;
      pts[(pointCount-1)*2] := symbols[pointCount-1];
      pts[(pointCount-1)*2+1].x := CoordToBitmap(1,0).x;
      pts[(pointCount-1)*2+1].y := symbols[pointCount-1].y;
    end else
    begin
      setlength(pts, pointCount);
      for i := 0 to pointCount-1 do
        pts[i] := symbols[i];
    end;
    if HueTabSelected then
    begin
      hsla.alpha := 220*$101;
      hsla.lightness := 32768;
      hsla.saturation := 55000;
      hueGradient := TBGRAHueGradient.Create(0,65535, hsla.saturation,hsla.lightness,[]);
      gradientScanner := TBGRAGradientScanner.Create(hueGradient,gtLinear,CoordToBitmap(0,0),CoordToBitmap(0,1));
      Bitmap.DrawPolyLineAntialias(pts, gradientScanner, FCurvePenWidth);
      gradientScanner.Free;
      hueGradient.Free;
      for i := 0 to pointCount-1 do
      begin
        hsla.hue := round(curve.GetPoint2DAt(pointList,i).x*65535) and 65535;
        symbolsColor[i] := HSLAToBGRA(hsla);
        with symbols[i] do Bitmap.FillEllipseAntialias(x,y,FPointSize,FPointSize,symbolsColor[i]);
      end;
    end else
    if SelectedTab = SaturationTab then
    begin
      hsla.alpha := 220*$101;
      hsla.lightness := 28000;
      hsla.hue := 65536 div 3;
      hsla.saturation := 0;
      c1 := HSLAToBGRA(hsla);
      hsla.saturation := 65535;
      c2 := HSLAToBGRA(hsla);

      hueGradient := TBGRAHueGradient.Create(c1,c2,[hgoHueCorrection, hgoLightnessCorrection]);
      gradientScanner := TBGRAGradientScanner.Create(hueGradient,gtLinear,CoordToBitmap(0,0),CoordToBitmap(0,1));
      Bitmap.DrawPolyLineAntialias(pts, gradientScanner, FCurvePenWidth);
      gradientScanner.Free;
      hueGradient.Free;
      for i := 0 to pointCount-1 do
      begin
        hsla.saturation := max(0,min(65535,round(curve.GetPoint2DAt(pointList,i).x*65535)));
        symbolsColor[i] := GSBAToBGRA(hsla);
        with symbols[i] do Bitmap.FillEllipseAntialias(x,y,FPointSize,FPointSize,symbolsColor[i]);
      end;
    end else
    if SelectedTab = LightnessTab then
    begin
      hsla.alpha := 220*$101;
      hsla.hue := 65536*2 div 3;
      hsla.saturation := 65535;
      hsla.lightness := 65535 div 8;
      c1 := HSLAToBGRA(hsla);
      hsla.lightness := 65535*7 div 8;
      c2 := HSLAToBGRA(hsla);

      hueGradient := TBGRAHueGradient.Create(c1,c2,[hgoHueCorrection, hgoLightnessCorrection]);
      gradientScanner := TBGRAGradientScanner.Create(hueGradient,gtLinear,CoordToBitmap(0,0),CoordToBitmap(0,1));
      Bitmap.DrawPolyLineAntialias(pts, gradientScanner, FCurvePenWidth);
      gradientScanner.Free;
      hueGradient.Free;
      for i := 0 to pointCount-1 do
      begin
        hsla.alpha := 220*$101;
        hsla.lightness := max(0,min(65535,round(65535 div 8+curve.GetPoint2DAt(pointList,i).x*65535*6/8)));
        c1 := GSBAToBGRA(hsla);
        hsla.alpha := hsla.lightness;
        hsla.lightness:= 65535-hsla.lightness;
        c2 := GSBAToBGRA(hsla);
        hsla.alpha := 220*$101;
        hsla.lightness:= 65535 div 8;
        symbolsColor[i] := GSBAToBGRA(hsla);
        with symbols[i] do Bitmap.EllipseAntialias(x,y,FPointSize,FPointSize,c2,1,c1);
      end;
    end else
    begin
      Bitmap.DrawPolyLineAntialias(pts, CurveColor, FCurvePenWidth, True);
      for i := 0 to pointCount-1 do
        with symbols[i] do Bitmap.FillEllipseAntialias(x,y,FPointSize,FPointSize,symbolsColor[i]);
    end;
    if (FSelectedPoint >= 0) and (FSelectedPoint < pointCount) then
      with symbols[FSelectedPoint] do Bitmap.EllipseAntialias(x,y,FPointSize+2,FPointSize+2,symbolsColor[FSelectedPoint],1);
  end;

  ToolButton_RemovePoint.Enabled := FSelectedPoint <> -1;
  if not FEffectUpdated then
  begin
    PreviewNeeded;
    FEffectUpdated:= true;
  end;
end;

procedure TFAdjustCurves.FormHide(Sender: TObject);
begin
  FreeAndNil(FGraphBackgroundLeft);
  FreeAndNil(FGraphBackgroundBottom);
  Timer_Thread.Enabled:= false;
end;

procedure TFAdjustCurves.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_DELETE then
  begin
    TryRemovePoint;
  end;
end;

procedure TFAdjustCurves.FormShow(Sender: TObject);
begin
  vsChart.DiscardBitmap;
  DiscardHistogram;
end;

procedure TFAdjustCurves.TabControl1Change(Sender: TObject);
begin
  FSelectedPoint:= -1;
  FMovingPoint:= false;
  FIgnoreInput:= true;
  ToolButton_Posterize.Down := Posterize;
  FIgnoreInput:= false;
  vsChart.DiscardBitmap;
end;

procedure TFAdjustCurves.Timer_ChartTimer(Sender: TObject);
begin
  Timer_Chart.Enabled := false;
  if FHistogramRedraw then
  begin
    vsChart.RedrawBitmap;
    FHistogramRedraw := false;
  end;
end;

procedure TFAdjustCurves.Timer_ThreadTimer(Sender: TObject);
begin
  Timer_Thread.Enabled:= false;
  FThreadManager.RegularCheck;
  Timer_Thread.Interval := 200;
  Timer_Thread.Enabled:= true;
end;

procedure TFAdjustCurves.ToolButton_PosterizeClick(Sender: TObject);
begin
  if FIgnoreInput then exit;
  Posterize := ToolButton_Posterize.Down;
end;

procedure TFAdjustCurves.ToolButton_RemovePointClick(Sender: TObject);
begin
  TryRemovePoint;
end;

procedure TFAdjustCurves.ToolButton_NewCurveClick(Sender: TObject);
begin
  while RemovePoint(1) do begin end;
  SetPointCoord(0,PointF(0,0));
  if Posterize then SetPointCoord(1,PointF(0.5,1));
  vsChart.RedrawBitmap;
end;

procedure TFAdjustCurves.vsChartMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  X := round(X*FScaling);
  Y := round(Y*FScaling);
  if Button = mbLeft then
  begin
    FSelectedPoint:= NearestPoint(X,Y);
    if FSelectedPoint= -1 then FSelectedPoint:= InsertPoint(X,Y);
    if FSelectedPoint<>-1 then FMovingPoint := true;
    vsChart.DiscardBitmap;
  end;
end;

procedure TFAdjustCurves.vsChartMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  X := round(X*FScaling);
  Y := round(Y*FScaling);
  if FMovingPoint then
  begin
    SetPointCoord(FSelectedPoint, BitmapToCoord(X,Y));
    vsChart.DiscardBitmap;
  end;
end;

procedure TFAdjustCurves.vsChartMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and FMovingPoint then
  begin
    FMovingPoint := false;
  end;
end;

procedure TFAdjustCurves.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FGraphBackgroundLeft);
  FreeAndNil(FGraphBackgroundBottom);
end;

procedure TFAdjustCurves.Button_OKClick(Sender: TObject);
begin
  if not FFilterConnector.ActionDone then FFilterConnector.ValidateAction;
  ModalResult := mrOK;
end;

procedure TFAdjustCurves.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  FThreadManager.Quit;
  CanClose := FThreadManager.ReadyToClose;
end;

procedure TFAdjustCurves.OnTryStopAction(Sender: TFilterConnector);
begin
  if self.Visible then
    Close;
end;

function TFAdjustCurves.GetSelectedTab: integer;
begin
  result := TabControl1.TabIndex;
end;

function TFAdjustCurves.GetHueTabSelected: boolean;
begin
  result := SelectedTab = HueTab;
end;

function TFAdjustCurves.GetNbPoints: integer;
var curve: TVariableSet;
begin
  curve := SelectedCurve;
  if curve = nil then result := 0
  else result := curve.GetListCount(curve.GetVariable('Points'));
end;

function TFAdjustCurves.GetPosterize: boolean;
var curve: TVariableSet;
begin
  curve := SelectedCurve;
  if curve = nil then
  begin
    result := false;
    exit;
  end;
  result := curve.Booleans['Posterize'];
end;

function TFAdjustCurves.GetSelectedCurve: TVariableSet;
begin
  result := FFilterConnector.Parameters.Subsets[CurveName];
end;

function TFAdjustCurves.GetCurveName: string;
begin
  case SelectedTab of
  RedTab: result := 'Red';
  GreenTab: result := 'Green';
  BlueTab: result := 'Blue';
  AlphaTab: result := 'Alpha';
  HueTab: result := 'Hue';
  SaturationTab: result := 'Saturation';
  LightnessTab: result := 'Lightness';
  end;
end;

function TFAdjustCurves.GetCurveColor: TBGRAPixel;
begin
  result := BGRAPixelTransparent;
  case SelectedTab of
  RedTab: result := CSSRed;
  GreenTab: result := BGRA(0,192,0);
  BlueTab: result := CSSBlue;
  AlphaTab: result := CSSGray;
  HueTab: result := CSSGray;
  SaturationTab: result := CSSGray;
  LightnessTab: result := CSSGray;
  end;
  result.alpha := 220;
end;

procedure TFAdjustCurves.RecomputeGraphBackground(AWidth, AHeight: integer);
const maxValueX = 1; maxValueY = 1.20;
var
  th, w, leftMargin, bottomMargin, rightMargin, i: integer;
  labels: array of string;
  axesColor , backgroundColor: TBGRAPixel;
  Bitmap: TBGRABitmap;
  totalLabelLength,maxLabelLength: integer;

  function GetCoord(x,y: single): TPointF;
  begin
    result := PointF(x/maxValueX*(Bitmap.Width-leftMargin-rightMargin)+leftMargin,
         Bitmap.Height-bottomMargin-y/maxValueY*(Bitmap.Height-bottomMargin));
  end;

begin
  FreeAndNil(FGraphBackgroundLeft);
  FreeAndNil(FGraphBackgroundBottom);
  FSelectedPoint:= -1;
  if HueTabSelected then
  begin
    setlength(labels, 7);
    labels[0] := '0';
    labels[1] := '60';
    labels[2] := '120';
    labels[3] := '180';
    labels[4] := '240';
    labels[5] := '300';
    labels[6] := '360';
  end
  else
  begin
    setlength(labels, 5);
    labels[0] := '0%';
    labels[1] := '25%';
    labels[2] := '50%';
    labels[3] := '75%';
    labels[4] := '100%';
  end;
  maxLabelLength := 0;
  for i := 0 to high(labels) do
    if length(labels[i])>maxLabelLength then maxLabelLength := length(labels[i]);
  totalLabelLength:= maxLabelLength + length(labels[0]) + (1+maxLabelLength)*(length(labels)-1);

  FScaling := GetCanvasScaleFactor;
  th := Min(AHeight div (length(labels)+2), round(AWidth*1.8/totalLabelLength));
  th := Min(th, DoScaleY(round(20*FScaling),96));
  FCurvePenWidth := th/10;
  FPointSize := th/4;
  FTickSize := th div 4;
  if FTickSize = 0 then exit;

  if assigned(FInstance) and FInstance.DarkTheme then
  begin
    backgroundColor := BGRABlack;
    axesColor := clSilver;
  end else
  begin
    backgroundColor := BGRAWhite;
    axesColor := BGRA(76,84,96);
  end;
  Bitmap := TBGRABitmap.Create(AWidth,AHeight,backgroundColor);
  FGridColor := axesColor;
  FGridColor.alpha := 128;
  Bitmap.FontQuality := fqFineAntialiasing;
  Bitmap.FontFullHeight := th;
  leftMargin := FTickSize;
  bottomMargin := th + FTickSize;
  for i := 0 to high(labels) do
  begin
    w := Bitmap.TextSize(labels[i]).cx;
    if w + FTickSize > leftMargin then
      leftMargin := w + FTickSize;
    if i = high(labels) then rightMargin:= w div 2;
  end;

  for i := 0 to high(labels) do
  begin
    with GetCoord(0,i/high(labels)) do
    begin
      Bitmap.TextOut(x-FTickSize,y-th div 2,labels[i],axesColor,taRightJustify);
      Bitmap.DrawLine(System.round(x),System.round(y),System.round(x-FTickSize),System.round(y),axesColor,False);
    end;
    with GetCoord(i/high(labels),0) do
    begin
      if i = 0 then
        Bitmap.TextOut(x,y+FTickSize,labels[i],axesColor,taLeftJustify) else
        Bitmap.TextOut(x,y+FTickSize,labels[i],axesColor,taCenter);
      Bitmap.DrawLine(System.round(x),System.round(y),System.round(x),System.round(y+FTickSize),axesColor,False);
    end;
  end;
  Bitmap.DrawLine(leftMargin, 0, leftMargin, Bitmap.Height - bottomMargin, axesColor, False);
  Bitmap.DrawLine(leftMargin, Bitmap.Height - bottomMargin, Bitmap.Width,
    Bitmap.Height - bottomMargin, axesColor, False);

  FGraphBackgroundLeft := bitmap.GetPart(rect(0,0,leftMargin+1,bitmap.Height)) as TBGRABitmap;
  FGraphBackgroundBottom := bitmap.GetPart(rect(leftMargin+1,bitmap.Height-bottomMargin,bitmap.Width,bitmap.Height)) as TBGRABitmap;
  FPoint0 := GetCoord(0,0);
  FPoint1 := GetCoord(1,1);
  FNbGrid := high(labels);
  Bitmap.Free;
  FHueTabPrecomputed:= HueTabSelected;
  FPreviousGraphWidth:= AWidth;
  FPreviousGraphHeight:= AHeight;
  FPreviousDarkTheme := assigned(FInstance) and FInstance.DarkTheme;
end;

function TFAdjustCurves.CoordToBitmap(x, y: single): TPointF;
begin
  result.x := FPoint0.x + x*(FPoint1.x-FPoint0.x);
  result.y := FPoint0.y + y*(FPoint1.y-FPoint0.y);
end;

function TFAdjustCurves.BitmapToCoord(x, y: single): TPointF;
begin
  if FPoint1.x <> FPoint0.x then
    result.x := (x-FPoint0.x)/(FPoint1.x-FPoint0.x)
  else
    result.x := 0;

  if FPoint1.y <> FPoint0.y then
    result.y := (y-FPoint0.y)/(FPoint1.y-FPoint0.y)
  else
    result.y := 0;
end;

function TFAdjustCurves.NearestPoint(xBmp, yBmp: single): integer;
var
  curve: TVariableSet;
  pointList: TScriptVariableReference;
  i,pointCount: integer;
  minDist,dist: single;
  pt: TPointF;
begin
  result := -1;
  curve := SelectedCurve;
  if curve = nil then exit;
  if xBmp < FPoint0.x then xBmp := FPoint0.x;
  if xBmp > FPoint1.x then xBmp := FPoint1.x;
  if yBmp > FPoint0.y then yBmp := FPoint0.y;
  if yBmp < FPoint1.y then yBmp := FPoint1.y;
  pointList := curve.GetVariable('Points');
  pointCount := curve.GetListCount(pointList);
  minDist := sqr(ScaleX(20,96)+0.0);
  for i := 0 to pointCount-1 do
  begin
    pt := curve.GetPoint2DAt(pointList,i);
    pt := CoordToBitmap(pt.x,pt.y);
    dist := sqr(pt.x-xBmp)+sqr(pt.y-yBmp);
    if dist < minDist then
    begin
      minDist := dist;
      result := i;
    end;
  end;
end;

function TFAdjustCurves.InsertPoint(xBmp, yBmp: single): integer;
var
  curve: TVariableSet;
  pointList: TScriptVariableReference;
  i,j,pointCount: integer;
  pt1,pt2,coord: TPointF;
begin
  result := -1;
  curve := SelectedCurve;
  if curve = nil then exit;
  pointList := curve.GetVariable('Points');
  pointCount := curve.GetListCount(pointList);
  for i := 1 to pointCount-1 do
  begin
    with curve.GetPoint2DAt(pointList,i-1) do pt1 := CoordToBitmap(x, y);
    with curve.GetPoint2DAt(pointList,i) do pt2 := CoordToBitmap(x, y);
    if (pt1.x <= xBmp-1) and (pt2.x >= xBmp+1) then
    begin
      coord := BitmapToCoord(xBmp,yBmp);
      if coord.y < 0 then coord.y := 0;
      if coord.y > 1 then coord.y := 1;
      curve.AppendPoint(pointList, curve.GetPoint2DAt(pointList, pointCount-1));
      for j := pointCount-1 downto i do curve.AssignPointAt(pointList, j+1, curve.GetPoint2DAt(pointList, j));
      curve.AssignPointAt(pointList, i, coord);
      result := i;
      FEffectUpdated := false;
    end;
    if (i = pointCount-1) and Posterize and (pt2.x <= xBmp-1) then
    begin
      coord := BitmapToCoord(xBmp,yBmp);
      if coord.x <= 1 then
      begin
        curve.AppendPoint(pointList, coord);
        result := i+1;
        FEffectUpdated := false;
      end;
    end;
  end;
end;

procedure TFAdjustCurves.EnsureCurveExist(AParameters: TVariableSet;
  AName: string);
var
  pointList: TScriptVariableReference;
  post: Boolean;
  cnt: NativeInt;
  i: Integer;
begin
  if not AParameters.IsDefined(AName) then
    AParameters.AddSubset(AName);

  with AParameters.Subsets[AName] do
  begin
    post := Booleans['Posterize'];
    pointList := GetVariable('Points');
    if not IsList(pointList) or (GetListCount(pointList)<=integer(not post)) then Remove(pointList);

    if IsReferenceDefined(pointList) then cnt := GetListCount(pointList)
    else cnt := 2;

    if not IsReferenceDefined(pointList) then
    begin
      pointList := AddPointList('Points');
      if Booleans['Posterize'] then
        for i := 0 to cnt-1 do
          AppendPoint(pointList, PointF(i/cnt, i/(cnt-1)) )
      else
        for i := 0 to cnt-1 do
          AppendPoint(pointList, PointF(i/(cnt-1), i/(cnt-1)) );
    end;
  end;
end;

procedure TFAdjustCurves.SetPointCoord(AIndex: integer; ACoord: TPointF);
var
  curve: TVariableSet;
  pointList: TScriptVariableReference;
  pointCount: integer;
begin
  curve := SelectedCurve;
  if curve = nil then exit;
  pointList := curve.GetVariable('Points');
  pointCount := curve.GetListCount(pointList);
  if (AIndex < 0) or (AIndex >= pointCount) then exit;
  if AIndex = 0 then ACoord.x := 0 else
    ACoord.x := max(ACoord.x, curve.GetPoint2DAt(pointList, AIndex-1).X);
  if AIndex = pointCount-1 then
  begin
    if not Posterize then ACoord.x := 1 else
      if ACoord.x > 1 then ACoord.x := 1;
  end else
    ACoord.x := min(ACoord.x, curve.GetPoint2DAt(pointList, AIndex+1).X);
  if ACoord.y < 0 then ACoord.y := 0;
  if ACoord.y > 1 then ACoord.y := 1;
  curve.AssignPointAt(pointList, AIndex, ACoord);
  FEffectUpdated:= false;
end;

function TFAdjustCurves.RemovePoint(AIndex: integer): boolean;
var
  curve: TVariableSet;
  pointList: TScriptVariableReference;
  pointCount: integer;
begin
  result := false;
  curve := SelectedCurve;
  if curve = nil then exit;
  pointList := curve.GetVariable('Points');
  pointCount := curve.GetListCount(pointList);
  if (AIndex <= 0) or (AIndex >= pointCount-1) then exit;
  if curve.RemoveAt(pointList, AIndex) then result := true;
  if result then FEffectUpdated:= false;
end;

procedure TFAdjustCurves.SetPosterize(AValue: boolean);
var
  curve: TVariableSet;
  pointList: TScriptVariableReference;
  pointCount: integer;
  lastCoord: TPointF;
begin
  curve := SelectedCurve;
  if curve = nil then exit;
  if curve.Booleans['Posterize'] <> AValue then
  begin
    curve.Booleans['Posterize'] := AValue; //try to set it
    if curve.Booleans['Posterize'] <> AValue then exit;

    pointList := curve.GetVariable('Points');
    pointCount := curve.GetListCount(pointList);

    if pointCount >= 2 then
    begin
      lastCoord := curve.GetPoint2DAt(pointList, pointCount-1);

      if AValue then
        lastCoord.x := (lastCoord.x + curve.GetPoint2DAt(pointList, pointCount-2).x)/2
      else
        lastCoord.x := 1;

      curve.AssignPointAt(pointList, pointCount-1, lastCoord);
    end;

    FEffectUpdated:= false;
    vsChart.RedrawBitmap;
  end;
end;

procedure TFAdjustCurves.ThemeChanged(Sender: TObject);
begin
  if FInstance.DarkTheme then
    vsChart.Color := clBlack
    else vsChart.Color := clWhite;
  vsChart.DiscardBitmap;
end;

procedure TFAdjustCurves.TryRemovePoint;
begin
  if RemovePoint(FSelectedPoint) then
  begin
    FSelectedPoint := -1;
    FMovingPoint:= false;
    vsChart.RedrawBitmap;
  end;
end;

procedure TFAdjustCurves.PreviewNeeded;
begin
  FThreadManager.WantPreview(TAdjustCurvesTask.Create(FFilterConnector));
end;

function TFAdjustCurves.NeedHistogram(ATab: integer): boolean;
  procedure InitCompute(ATab: integer);
  var
    i: Integer;
  begin
    for i := 0 to HistogramBarCount-1 do
      FHistogram[ATab, i] := 0;
  end;

  procedure FinishCompute(ATab: integer);
  var i: integer;
  begin
    FHistogramMax[ATab] := 0;
    for i := 0 to HistogramBarCount-1 do
      FHistogramMax[ATab] := max(FHistogramMax[ATab], FHistogram[ATab, i]);
    FHistogramComputed[ATab] := true;
  end;

var
  p: PBGRAPixel;
  yb, xb, nbx: LongInt;
  hslaValue: THSLAPixel;

begin
  if (ATab < 0) or (ATab > CurveTabCount) then exit(false);
  if FHistogramComputed[ATab] then exit(true);
  if ATab in [HueTab, SaturationTab, LightnessTab] then
  begin
    InitCompute(HueTab);
    InitCompute(SaturationTab);
    InitCompute(LightnessTab);
  end else
    InitCompute(ATab);

  nbx := FFilterConnector.WorkArea.Width;
  for yb := FFilterConnector.WorkArea.Top to
            FFilterConnector.WorkArea.Bottom-1 do
  begin
    p := FFilterConnector.BackupLayer.ScanLine[yb] + FFilterConnector.WorkArea.Left;
    case ATab of
    RedTab: for xb := nbx-1 downto 0 do begin
              inc(FHistogram[ATab, GammaExpansionTab[p^.red]*HistogramBarCount shr 16]);
              inc(p); end;
    GreenTab: for xb := nbx-1 downto 0 do begin
                inc(FHistogram[ATab, GammaExpansionTab[p^.green]*HistogramBarCount shr 16]);
                inc(p); end;
    BlueTab: for xb := nbx-1 downto 0 do begin
                inc(FHistogram[ATab, GammaExpansionTab[p^.blue]*HistogramBarCount shr 16]);
                inc(p); end;
    HueTab, SaturationTab, LightnessTab:
      for xb := nbx-1 downto 0 do begin
         hslaValue := p^.ToHSLAPixel;
         if (hslaValue.saturation > 0) and (hslaValue.lightness > 0)
            and (hslaValue.lightness < 65535) then
            inc(FHistogram[HueTab, hslaValue.hue*HistogramBarCount shr 16]);
         if (hslaValue.lightness > 0) and (hslaValue.lightness < 65535) then
           inc(FHistogram[SaturationTab, hslaValue.saturation*HistogramBarCount shr 16]);
         inc(FHistogram[LightnessTab, hslaValue.lightness*HistogramBarCount shr 16]);
         inc(p); end;
    AlphaTab: for xb := nbx-1 downto 0 do begin
                inc(FHistogram[ATab, p^.alpha*HistogramBarCount shr 8]);
                inc(p); end;
    end;
  end;

  if ATab in [HueTab, SaturationTab, LightnessTab] then
  begin
    FinishCompute(HueTab);
    FinishCompute(SaturationTab);
    FinishCompute(LightnessTab);
  end else
    FinishCompute(ATab);
  result := true;
end;

procedure TFAdjustCurves.DiscardHistogram;
var
  i: Integer;
begin
  for i := 0 to CurveTabCount-1 do
  begin
    FHistogramComputed[i] := false;
    FHistogramDelay[i] := true;
  end;
  FHistogramDelay[RedTab] := false;
  FHistogramDelay[GreenTab] := false;
  FHistogramDelay[BlueTab] := false;
  FHistogramDelay[AlphaTab] := false;
  FHistogramRedraw:= false;
end;

procedure TFAdjustCurves.QueryHistogramRedraw;
begin
  FHistogramRedraw:= true;
  Timer_Chart.Enabled := true;
end;

procedure TFAdjustCurves.OnTaskEvent(ASender: TObject;
  AEvent: TThreadManagerEvent);
begin
  case AEvent of
  tmeAbortedTask,tmeCompletedTask:
    begin
      Timer_Thread.Enabled := false;
      if FThreadManager.ReadyToClose then
        Close
      else
        if AEvent = tmeCompletedTask then Button_OK.Enabled := true;
    end;
  tmeStartingNewTask:
    begin
      Timer_Thread.Enabled := false;
      Timer_Thread.Interval := 100;
      Timer_Thread.Enabled := true;
      Button_OK.Enabled := false;
    end;
  end;
end;

function TFAdjustCurves.ShowModal: integer;
begin
  if (FFilterConnector = nil) or (FFilterConnector.ActiveLayer = nil) then
  begin
    if FInstance <> nil then
      FInstance.ShowMessage(rsLazPaint, rsNoActiveLayer)
    else
      ShowMessage(rsNoActiveLayer);
    Result := mrAbort;
  end
  else
  begin
    FEffectUpdated := false;
    FThreadManager := TFilterThreadManager.Create(FFilterConnector);
    FThreadManager.OnEvent := @OnTaskEvent;
    try
      Result := inherited ShowModal;
    finally
      FThreadManager.Free;
    end;
  end;
end;

function TFAdjustCurves.ShowModal(AInstance: TLazPaintCustomInstance;
  AParameters: TVariableSet): integer;
var
  topmostInfo: TTopMostInfo;
  tempParameters: TVariableSet;
  task: TAdjustCurvesTask;

  procedure CopyCurveIfNonTrivial(ASource: TVariableSet; AName: string);
  var subset: TVariableSet;
    pointList: TScriptVariableReference;
    pointCount,i: integer;
    trivial: boolean;
  begin
    subset := asource.Subsets[AName];
    if Assigned(subset) then
    begin
      pointList := subset.GetVariable('Points');
      if subset.IsReferenceDefined(pointList) then
      begin
        pointCount := subset.GetListCount(pointList);
        if subset.Booleans['Posterize'] then
          trivial := false
        else
        begin
          trivial := true;
          for i := 0 to pointCount-1 do
            with subset.GetPoint2DAt(pointList, i) do
              if abs(x-y) > 1e-6 then trivial := false;
        end;
        if not trivial or AParameters.IsDefined(AName) then
          AParameters.Subsets[AName] := subset;
      end;
    end;
  end;

begin
  tempParameters := AParameters.Duplicate;
  try
    FFilterConnector := TFilterConnector.Create(AInstance, tempParameters, false);
    FFilterConnector.OnTryStopAction := @OnTryStopAction;
  except
    on ex: Exception do
    begin
      AInstance.ShowError('AdujstCurves', ex.Message);
      Result := mrAbort;
      tempParameters.Free;
      exit;
    end;
  end;
  try
    FInstance := AInstance;
    FInstance.RegisterThemeListener(@ThemeChanged, true);
    EnsureCurveExist(tempParameters,'Red');
    EnsureCurveExist(tempParameters,'Green');
    EnsureCurveExist(tempParameters,'Blue');
    EnsureCurveExist(tempParameters,'Alpha');
    EnsureCurveExist(tempParameters,'Hue');
    EnsureCurveExist(tempParameters,'Saturation');
    EnsureCurveExist(tempParameters,'Lightness');
    topmostInfo := AInstance.HideTopmost;
    ToolBar8.Images := AInstance.Icons[DoScaleY(16,OriginalDPI)];
    try
      if tempParameters.Booleans['Validate'] then
      begin
        task := TAdjustCurvesTask.Create(FFilterConnector);
        try
          task.Execute;
        finally
          task.Free;
        end;
        FFilterConnector.ValidateAction;
        result := mrOk;
      end else
        Result := self.ShowModal;
    except
      on ex: Exception do
      begin
        AInstance.ShowError('AdjustCurves', ex.Message);
        Result := mrAbort;
      end;
    end;
    AInstance.ShowTopmost(topmostInfo);

    CopyCurveIfNonTrivial(tempParameters,'Red');
    CopyCurveIfNonTrivial(tempParameters,'Green');
    CopyCurveIfNonTrivial(tempParameters,'Blue');
    CopyCurveIfNonTrivial(tempParameters,'Alpha');
    CopyCurveIfNonTrivial(tempParameters,'Hue');
    CopyCurveIfNonTrivial(tempParameters,'Saturation');
    CopyCurveIfNonTrivial(tempParameters,'Lightness');

  finally
    FInstance.RegisterThemeListener(@ThemeChanged, false);
    FreeAndNil(FFilterConnector);
    tempParameters.Free;
    FInstance := nil;
  end;
end;

{$R *.lfm}

end.
