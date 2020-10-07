// SPDX-License-Identifier: GPL-3.0-only
unit UZoom;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, StdCtrls, BGRABitmapTypes;

type
  TZoom = class;

  TOnZoomChangedHandler = procedure(sender: TZoom; ANewZoom: single) of object;

  TCustomMainFormLayout = class
  protected
    function GetWorkArea: TRect; virtual; abstract;
  public
    property WorkArea: TRect read GetWorkArea;
  end;

  { TZoom }

  TZoom = class
  private
    FLayout: TCustomMainFormLayout;
    FLabelCurrentZoom: TLabel;
    FEditZoom: TEdit;
    FMaxFactor: single;
    FMinFactor: single;
    FZoomFactor: single;
    FOnZoomChangedHandler : TOnZoomChangedHandler;
    FBitmapPosition: TPointF;
    FMousePosition: TPoint;
    function GetEditingZoom: boolean;
    function GetPositionDefined: boolean;
    function GetZoomFactor: single;
    procedure SetEditingZoom(AValue: boolean);
    procedure SetMaxFactor(AValue: single);
    procedure SetMinFactor(AValue: single);
    procedure SetZoomFactor(AValue: single);
  protected
    procedure EditZoom_KeyPress(Sender: TObject; var Key: char);
    procedure EditZoom_ZoomExit(Sender: TObject);
    procedure LabelCurrentZoom_Click(Sender: TObject);
    procedure UpdateLabel;
    function RoundZoom(AValue: single): single;
  public
    constructor Create(ALabelCurrentZoom: TLabel; AEditZoom: TEdit);
    destructor Destroy; override;
    procedure ZoomOriginal;
    procedure ZoomFit(AImageWidth,AImageHeight: integer);
    procedure ZoomIn(AFine: boolean = false);
    procedure ZoomOut(AFine: boolean = false);
    procedure SetPosition(ABitmapPosition: TPointF; AMousePosition: TPoint);
    procedure ClearPosition;
    procedure DoAction(const AName: string);
    function GetScaledArea(const AWorkArea: TRect; AImageWidth, AImageHeight: integer; var AViewOffset: TPoint): TRect;
    property Layout: TCustomMainFormLayout read FLayout write FLayout;
    property EditingZoom: boolean read GetEditingZoom write SetEditingZoom;
    property Factor: single read GetZoomFactor write SetZoomFactor;
    property OnZoomChanged: TOnZoomChangedHandler read FOnZoomChangedHandler write FOnZoomChangedHandler;
    property MaxFactor: single read FMaxFactor write SetMaxFactor;
    property MinFactor: single read FMinFactor write SetMinFactor;
    property BitmapPosition: TPointF read FBitmapPosition;
    property MousePosition: TPoint read FMousePosition;
    property PositionDefined: boolean read GetPositionDefined;
  end;

implementation

uses Math, Dialogs, LazPaintType;

{ TZoom }

function TZoom.GetEditingZoom: boolean;
begin
  result := FEditZoom.Visible;
end;

function TZoom.GetPositionDefined: boolean;
begin
  result := not isEmptyPointF(FBitmapPosition);
end;

function TZoom.GetZoomFactor: single;
begin
  result := FZoomFactor;
end;

procedure TZoom.SetEditingZoom(AValue: boolean);
begin
  if AValue <> FEditZoom.Visible then
  begin
    if AValue then
    begin
      FEditZoom.Text := IntToStr(round(FZoomFactor*100));
      FEditZoom.Visible := true;
      FLabelCurrentZoom.Visible := false;
      SafeSetFocus(FEditZoom);
    end else
    begin
      FLabelCurrentZoom.Visible := not AValue;
      FEditZoom.Visible := AValue
    end;
  end;
end;

procedure TZoom.SetMaxFactor(AValue: single);
begin
  if FMaxFactor=AValue then Exit;
  FMaxFactor:=AValue;
end;

procedure TZoom.SetMinFactor(AValue: single);
begin
  if FMinFactor=AValue then Exit;
  FMinFactor:=AValue;
end;

procedure TZoom.SetZoomFactor(AValue: single);
begin
  if (FMinFactor <> 0) and (AValue < FMinFactor) then AValue := FMinFactor;
  if (FMaxFactor <> 0) and (AValue > FMaxFactor) then AValue := FMaxFactor;
  if AValue = FZoomFactor then exit;
  EditingZoom:= False;
  FZoomFactor:= AValue;
  if Assigned(FOnZoomChangedHandler) then
    FOnZoomChangedHandler(self, AValue);
  UpdateLabel;
end;

procedure TZoom.EditZoom_ZoomExit(Sender: TObject);
begin
  EditingZoom:= false;
end;

procedure TZoom.LabelCurrentZoom_Click(Sender: TObject);
begin
  EditingZoom := true;
end;

procedure TZoom.UpdateLabel;
begin
  if Factor < 3 then
    FLabelCurrentZoom.Caption := inttostr(round(Factor*100))+'%' else
     FLabelCurrentZoom.Caption := 'x'+FloatToStr(round(Factor*100)/100);
end;

function TZoom.RoundZoom(AValue: single): single;
var zoomFactorLog,halfZoom,sign: single;
begin
  halfZoom := ln(1.5)/ln(2);
  zoomFactorLog := ln(AValue)/ln(2);
  if zoomFactorLog < 0 then
  begin
    sign := -1;
    zoomFactorLog:= -zoomFactorLog;
  end else
    sign := 1;
  if frac(zoomFactorLog) >= (halfZoom+1)/2 then
    zoomFactorLog:= ceil(zoomFactorLog)
  else
  if frac(zoomFactorLog) >= halfZoom/2 then
    zoomFactorLog:= floor(zoomFactorLog)+halfZoom
  else
    zoomFactorLog:= floor(zoomFactorLog);

  result := exp(sign*zoomFactorLog*ln(2));
end;

procedure TZoom.EditZoom_KeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin
    Key := #0;
    EditingZoom:= false;
    if length(FEditZoom.Text) > 0 then
    begin
      try
        Factor:= StrToInt(FEditZoom.Text)/100;
      except
        on ex:exception do
        begin end;
      end;
    end;
  end else
  if Key = #27 then
  begin
    Key := #0;
    EditingZoom := false;
  end else
  if not (Key in['0'..'9',#8]) then Key := #0;
end;

constructor TZoom.Create(ALabelCurrentZoom: TLabel; AEditZoom: TEdit);
begin
  inherited Create;
  FLayout := nil;
  FLabelCurrentZoom := ALabelCurrentZoom;
  FLabelCurrentZoom.OnClick := @LabelCurrentZoom_Click;
  FEditZoom := AEditZoom;
  FEditZoom.Top := FLabelCurrentZoom.Top-1;
  FEditZoom.OnExit := @EditZoom_ZoomExit;
  FEditZoom.OnKeyPress:= @EditZoom_KeyPress;
  FZoomFactor:= 1;
  FMinFactor := 0;
  FMaxFactor := 0;
  ClearPosition;
  UpdateLabel;
end;

destructor TZoom.Destroy;
begin
  FLabelCurrentZoom.OnClick := nil;
  FEditZoom.OnExit := nil;
  FEditZoom.OnKeyPress := nil;
  inherited Destroy;
end;

procedure TZoom.ZoomOriginal;
begin
  Factor := 1;
end;

procedure TZoom.ZoomFit(AImageWidth, AImageHeight: integer);
const pixelMargin = 0;
var zx,zy: single;
  pictureArea: TRect;
begin
  if FLayout = nil then exit;
  pictureArea := FLayout.WorkArea;
  if (AImageWidth = 0) or (AImageHeight = 0) or (pictureArea.right-pictureArea.Left <= pixelMargin)
    or (pictureArea.Bottom-pictureArea.top <= pixelMargin) then exit;
  try
    zx := (pictureArea.right-pictureArea.left-pixelMargin)/AImageWidth;
    zy := (pictureArea.bottom-pictureArea.top-pixelMargin)/AImageheight;
    Factor:= min(zx,zy);
  except
    on ex:Exception do
    begin end;
  end;
end;

procedure TZoom.ZoomIn(AFine: boolean);
begin
  if AFine then
    Factor := Factor*1.1
  else if RoundZoom(Factor) > Factor then
    Factor := RoundZoom(Factor)
  else
    Factor := RoundZoom(Factor*sqrt(2));
end;

procedure TZoom.ZoomOut(AFine: boolean);
begin
  if AFine then
    Factor := Factor/1.1
  else if RoundZoom(Factor) < Factor then
    Factor := RoundZoom(Factor)
  else
    Factor := RoundZoom(Factor/sqrt(2));
end;

procedure TZoom.SetPosition(ABitmapPosition: TPointF; AMousePosition: TPoint);
begin
  FBitmapPosition := ABitmapPosition;
  FMousePosition := AMousePosition;
end;

procedure TZoom.ClearPosition;
begin
  SetPosition(EmptyPointF,Point(0,0));
end;

procedure TZoom.DoAction(const AName: string);
begin
  if AName = 'ViewZoomIn' then ZoomIn else
  if AName = 'ViewZoomOriginal' then ZoomOriginal else
  if AName = 'ViewZoomOut' then ZoomOut;
end;

function TZoom.GetScaledArea(const AWorkArea: TRect; AImageWidth, AImageHeight: integer; var AViewOffset: TPoint): TRect;
var
  scaledWidth,scaledHeight: integer;
  maxOffset, minOffset: TPoint;
  temp: integer;
begin
  scaledWidth := round(AImageWidth*Factor);
  if scaledWidth = 0 then scaledWidth := 1;
  scaledHeight := round(AImageHeight*Factor);
  if scaledHeight = 0 then scaledHeight := 1;
  result.Left := (AWorkArea.Left+AWorkArea.Right-scaledWidth) div 2;
  result.Top := (AWorkArea.Top+AWorkArea.Bottom-scaledHeight) div 2;

  maxOffset := point(floor((AWorkArea.Right-(result.Left+scaledWidth))/Factor),
       floor((AWorkArea.Bottom-(result.Top+scaledHeight))/Factor));
  minOffset := point(ceil((AWorkArea.Left-result.Left)/Factor),
               ceil((AWorkArea.Top-result.Top)/Factor));
  if maxOffset.X < minOffset.X then
  begin
    temp := maxOffset.X;
    maxOffset.X := minOffset.X;
    minOffset.X := temp;
  end;
  if maxOffset.Y < minOffset.Y then
  begin
    temp := maxOffset.Y;
    maxOffset.Y := minOffset.Y;
    minOffset.Y := temp;
  end;

  if minOffset.X > -AImageWidth div 2 then minOffset.X := -AImageWidth div 2;
  if minOffset.Y > -AImageHeight div 2 then minOffset.Y := -AImageHeight div 2;
  if maxOffset.X < AImageWidth div 2 then maxOffset.X := AImageWidth div 2;
  if maxOffset.Y < AImageHeight div 2 then maxOffset.Y := AImageHeight div 2;

  if AViewOffset.X < minOffset.X then AViewOffset.X := minOffset.X else
  if AViewOffset.X > maxOffset.X then AViewOffset.X := maxOffset.X;

  if AViewOffset.Y < minOffset.Y then AViewOffset.Y := minOffset.Y else
  if AViewOffset.Y > maxOffset.Y then AViewOffset.Y := maxOffset.Y;

  if AImageWidth <> 0 then result.Left += round(AViewOffset.X*scaledWidth/AImageWidth);
  if AImageHeight <> 0 then result.Top += round(AViewOffset.Y*scaledHeight/AImageHeight);
  result.Right := result.Left + scaledWidth;
  result.Bottom := result.Top + scaledHeight;
end;

end.

