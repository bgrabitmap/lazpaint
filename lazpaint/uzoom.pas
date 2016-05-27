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
    function GetPictureArea: TRect; virtual; abstract;
  public
    property PictureArea: TRect read GetPictureArea;
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
    constructor Create(ALabelCurrentZoom: TLabel; AEditZoom: TEdit; ALayout: TCustomMainFormLayout);
    destructor Destroy; override;
    procedure ZoomOriginal;
    procedure ZoomFit(AImageWidth,AImageHeight: integer);
    procedure ZoomIn;
    procedure ZoomOut;
    procedure SetPosition(ABitmapPosition: TPointF; AMousePosition: TPoint);
    procedure ClearPosition;
    procedure DoAction(const AName: string);
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

constructor TZoom.Create(ALabelCurrentZoom: TLabel; AEditZoom: TEdit;
  ALayout: TCustomMainFormLayout);
begin
  inherited Create;
  FLayout := ALayout;
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
  pictureArea := FLayout.PictureArea;
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

procedure TZoom.ZoomIn;
begin
  if RoundZoom(Factor) > Factor then
    Factor := RoundZoom(Factor)
  else
    Factor := RoundZoom(Factor*sqrt(2));
end;

procedure TZoom.ZoomOut;
begin
  if RoundZoom(Factor) < Factor then
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

end.

