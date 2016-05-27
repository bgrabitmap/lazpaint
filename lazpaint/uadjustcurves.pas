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

type
  { TFAdjustCurves }

  TFAdjustCurves = class(TForm)
    Panel2: TPanel;
    Timer1: TTimer;
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
    procedure TabControl1Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
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
    FGraphBackgroundLeft, FGraphBackgroundBottom: TBGRABitmap;
    FPoint0,FPoint1: TPointF;
    FNbGrid: integer;
    FGridColor: TBGRAPixel;
    FTickSize: integer;
    FPointSize, FCurvePenWidth: single;
    FHueTabPrecomputed: boolean;
    FSelectedPoint: integer;
    FMovingPoint: boolean;
    FEffectUpdated: boolean;
    FIgnoreInput: boolean;
    FThreadManager: TFilterThreadManager;
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
    procedure TryRemovePoint;
    procedure PreviewNeeded;
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

uses UResourceStrings, LCLType, UMac, Math, UScaleDPI, BGRAGradientScanner,
  UColorFilters;

{ TFAdjustCurves }

procedure TFAdjustCurves.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  ScaleDPI(Self, OriginalDPI);

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
  XList,YList: TScriptVariableReference;
  pointCount: integer;
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
    (FHueTabPrecomputed <> HueTabSelected) then
      RecomputeGraphBackground(Bitmap.Width,Bitmap.Height);
  if (FGraphBackgroundLeft = nil) or (FGraphBackgroundBottom = nil) then exit;
  Bitmap.PutImage(0,0,FGraphBackgroundLeft,dmSet);
  Bitmap.PutImage(FGraphBackgroundLeft.Width,Bitmap.Height-FGraphBackgroundBottom.Height,FGraphBackgroundBottom,dmSet);
  for i := 1 to FNbGrid do
  begin
      with CoordToBitmap(i/FNbGrid,i/FNbGrid) do
      begin
        Bitmap.DrawLineAntialias(round(FPoint0.x),round(y),Bitmap.Width,round(y),FGridColor,BGRAPixelTransparent,FTickSize,false);
        Bitmap.DrawLineAntialias(round(x),round(FPoint0.y),round(x),0,FGridColor,BGRAPixelTransparent,FTickSize,false);
      end;
  end;
  curve := SelectedCurve;
  if Assigned(curve) then
  begin
    XList := curve.GetVariable('X');
    YList := curve.GetVariable('Y');
    pointCount := curve.GetListCount(XList);
    setlength(symbols, pointCount);
    setlength(symbolsColor, pointCount);
    for i := 0 to pointCount-1 do
    begin
      symbols[i] := CoordToBitmap(curve.GetFloatAt(XList,i),curve.GetFloatAt(YList,i));
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
        hsla.hue := round(curve.GetFloatAt(XList,i)*65535) and 65535;
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
        hsla.saturation := max(0,min(65535,round(curve.GetFloatAt(XList,i)*65535)));
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
        hsla.lightness := max(0,min(65535,round(65535 div 8+curve.GetFloatAt(XList,i)*65535*6/8)));
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
  Timer1.Enabled:= false;
end;

procedure TFAdjustCurves.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_DELETE then
  begin
    TryRemovePoint;
  end;
end;

procedure TFAdjustCurves.TabControl1Change(Sender: TObject);
begin
  FSelectedPoint:= -1;
  FMovingPoint:= false;
  FIgnoreInput:= true;
  ToolButton_Posterize.Down := Posterize;
  FIgnoreInput:= false;
  vsChart.RedrawBitmap;
end;

procedure TFAdjustCurves.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled:= false;
  FThreadManager.RegularCheck;
  Timer1.Interval := 200;
  Timer1.Enabled:= true;
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
  else result := curve.GetListCount(curve.GetVariable('X'));
end;

function TFAdjustCurves.GetPosterize: boolean;
var curve: TVariableSet;
begin
  curve := SelectedCurve;
  if curve = nil then exit;
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
  axesColor : TBGRAPixel;
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

  th := Min(AHeight div (length(labels)+2), round(AWidth*1.8/totalLabelLength));
  th := Min(th, DoScaleY(20,96));
  FCurvePenWidth := th/10;
  FPointSize := th/4;
  FTickSize := th div 4;
  if FTickSize = 0 then exit;

  Bitmap := TBGRABitmap.Create(AWidth,AHeight,BGRAWhite);
  axesColor := BGRA(76,84,96);
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
      Bitmap.DrawLine(round(x),round(y),round(x-FTickSize),round(y),axesColor,False);
    end;
    with GetCoord(i/high(labels),0) do
    begin
      if i = 0 then
        Bitmap.TextOut(x,y+FTickSize,labels[i],axesColor,taLeftJustify) else
        Bitmap.TextOut(x,y+FTickSize,labels[i],axesColor,taCenter);
      Bitmap.DrawLine(round(x),round(y),round(x),round(y+FTickSize),axesColor,False);
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
  XList,YList: TScriptVariableReference;
  i,pointCount: integer;
  minDist,dist: single;
  pt: TPointF;
begin
  result := -1;
  curve := SelectedCurve;
  if curve = nil then exit;
  XList := curve.GetVariable('X');
  YList := curve.GetVariable('Y');
  pointCount := curve.GetListCount(XList);
  minDist := sqr(ScaleX(20,96)+0.0);
  for i := 0 to pointCount-1 do
  begin
    pt := PointF(curve.GetFloatAt(XList,i),curve.GetFloatAt(YList,i));
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
  XList,YList: TScriptVariableReference;
  i,j,pointCount: integer;
  pt1,pt2,coord: TPointF;
begin
  result := -1;
  curve := SelectedCurve;
  if curve = nil then exit;
  XList := curve.GetVariable('X');
  YList := curve.GetVariable('Y');
  pointCount := curve.GetListCount(XList);
  for i := 1 to pointCount-1 do
  begin
    pt1 := CoordToBitmap(curve.GetFloatAt(XList,i-1),curve.GetFloatAt(YList,i-1));
    pt2 := CoordToBitmap(curve.GetFloatAt(XList,i),curve.GetFloatAt(YList,i));
    if (pt1.x <= xBmp-1) and (pt2.x >= xBmp+1) then
    begin
      coord := BitmapToCoord(xBmp,yBmp);
      curve.AppendFloat(XList, curve.GetFloatAt(XList, pointCount-1));
      for j := pointCount-1 downto i do curve.AssignFloatAt(XList, j+1, curve.GetFloatAt(XList, j));
      curve.AssignFloatAt(XList, i, coord.x);
      curve.AppendFloat(YList, curve.GetFloatAt(YList, pointCount-1));
      for j := pointCount-1 downto i do curve.AssignFloatAt(YList, j+1, curve.GetFloatAt(YList, j));
      curve.AssignFloatAt(YList, i, coord.y);
      result := i;
      FEffectUpdated := false;
    end;
    if (i = pointCount-1) and Posterize and (pt2.x <= xBmp-1) then
    begin
      coord := BitmapToCoord(xBmp,yBmp);
      if coord.x <= 1 then
      begin
        curve.AppendFloat(XList, coord.x);
        curve.AppendFloat(YList, coord.y);
        result := i+1;
        FEffectUpdated := false;
      end;
    end;
  end;
end;

procedure TFAdjustCurves.EnsureCurveExist(AParameters: TVariableSet;
  AName: string);
begin
  if not AParameters.IsDefined(AName) then
  with AParameters.AddSubset(AName) do
  begin
    if AParameters.Booleans['Posterize'] then
      AddList('X','[0.0, 0.5]') else
      AddList('X','[0.0, 1.0]');

    AddList('Y','[0.0, 1.0]');
  end;
end;

procedure TFAdjustCurves.SetPointCoord(AIndex: integer; ACoord: TPointF);
var
  curve: TVariableSet;
  XList,YList: TScriptVariableReference;
  pointCount: integer;
begin
  curve := SelectedCurve;
  if curve = nil then exit;
  XList := curve.GetVariable('X');
  YList := curve.GetVariable('Y');
  pointCount := curve.GetListCount(XList);
  if (AIndex < 0) or (AIndex >= pointCount) then exit;
  if AIndex = 0 then ACoord.x := 0 else
    ACoord.x := max(ACoord.x, curve.GetFloatAt(XList, AIndex-1));
  if AIndex = pointCount-1 then
  begin
    if not Posterize then ACoord.x := 1 else
      if ACoord.x > 1 then ACoord.x := 1;
  end else
    ACoord.x := min(ACoord.x, curve.GetFloatAt(XList, AIndex+1));
  if ACoord.y < 0 then ACoord.y := 0;
  if ACoord.y > 1 then ACoord.y := 1;
  curve.AssignFloatAt(XList, AIndex, ACoord.x);
  curve.AssignFloatAt(YList, AIndex, ACoord.y);
  FEffectUpdated:= false;
end;

function TFAdjustCurves.RemovePoint(AIndex: integer): boolean;
var
  curve: TVariableSet;
  XList,YList: TScriptVariableReference;
  pointCount: integer;
begin
  result := false;
  curve := SelectedCurve;
  if curve = nil then exit;
  XList := curve.GetVariable('X');
  YList := curve.GetVariable('Y');
  pointCount := curve.GetListCount(XList);
  if (AIndex <= 0) or (AIndex >= pointCount-1) then exit;
  if curve.RemoveAt(XList, AIndex) then result := true;
  if curve.RemoveAt(YList, AIndex) then result := true;
  if result then FEffectUpdated:= false;
end;

procedure TFAdjustCurves.SetPosterize(AValue: boolean);
var
  curve: TVariableSet;
  XList: TScriptVariableReference;
  pointCount: integer;
begin
  curve := SelectedCurve;
  if curve = nil then exit;
  if curve.Booleans['Posterize'] <> AValue then
  begin
    curve.Booleans['Posterize'] := AValue; //try to set it
    if curve.Booleans['Posterize'] <> AValue then exit;

    XList := curve.GetVariable('X');
    pointCount := curve.GetListCount(XList);

    if AValue then
      curve.AssignFloatAt(XList, pointCount-1, (curve.GetFloatAt(XList, pointCount-1)+curve.GetFloatAt(XList, pointCount-2))/2)
    else
      curve.AssignFloatAt(XList, pointCount-1, 1);

    FEffectUpdated:= false;
    vsChart.RedrawBitmap;
  end;
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

procedure TFAdjustCurves.OnTaskEvent(ASender: TObject;
  AEvent: TThreadManagerEvent);
begin
  case AEvent of
  tmeAbortedTask,tmeCompletedTask:
    begin
      Timer1.Enabled := false;
      if FThreadManager.ReadyToClose then
        Close
      else
        if AEvent = tmeCompletedTask then Button_OK.Enabled := true;
    end;
  tmeStartingNewTask:
    begin
      Timer1.Enabled := false;
      Timer1.Interval := 100;
      Timer1.Enabled := true;
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

  procedure CopyCurveIfNonTrivial(ASource: TVariableSet; AName: string);
  var subset: TVariableSet;
    XList,YList: TScriptVariableReference;
    pointCount,i: integer;
    trivial: boolean;
  begin
    subset := asource.Subsets[AName];
    if Assigned(subset) then
    begin
      XList := subset.GetVariable('X');
      YList := subset.GetVariable('Y');
      if subset.IsReferenceDefined(XList) and
        subset.IsReferenceDefined(YList) then
      begin
        pointCount := subset.GetListCount(XList);
        trivial := true;
        for i := 0 to pointCount-1 do
          if abs(subset.GetFloatAt(XList, i)-subset.GetFloatAt(YList, i)) > 1e-6 then trivial := false;
        if not trivial or AParameters.IsDefined(AName) then
          AParameters.Subsets[AName] := subset;
      end;
    end;
  end;

begin
  tempParameters := AParameters.Duplicate;
  try
    FFilterConnector := TFilterConnector.Create(AInstance, tempParameters);
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
    FreeAndNil(FFilterConnector);
    tempParameters.Free;
    FInstance := nil;
  end;
end;

{$R *.lfm}

end.
