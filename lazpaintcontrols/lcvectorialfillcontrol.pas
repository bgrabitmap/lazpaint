// SPDX-License-Identifier: GPL-3.0-only
unit LCVectorialFillControl;

{$mode objfpc}{$H+}

interface

uses
  Types, Classes, SysUtils, Controls, LCVectorialFillInterface,
  LCVectorialFill, BGRABitmap, BGRABitmapTypes, BGRAGradientScanner,
  LCVectorOriginal;

type
  TLCFillTarget = LCVectorialFillInterface.TLCFillTarget;

const
  ftPen = LCVectorialFillInterface.ftPen;
  ftBack = LCVectorialFillInterface.ftBack;
  ftOutline = LCVectorialFillInterface.ftOutline;

type
  { TLCVectorialFillControl }

  TLCVectorialFillControl = class(TWinControl)
  private
    function GetAllowedFillTypes: TVectorialFillTypes;
    function GetAverageColor: TBGRAPixel;
    function GetCanAdjustToShape: boolean;
    function GetCanEditGradTexPoints: boolean;
    function GetEditingGradTexPoints: boolean;
    function GetFillType: TVectorialFillType;
    function GetGradEndColor: TBGRAPixel;
    function GetGradInterp: TBGRAColorInterpolation;
    function GetGradRepetition: TBGRAGradientRepetition;
    function GetGradStartColor: TBGRAPixel;
    function GetGradType: TGradientType;
    function GetIsTarget: boolean;
    function GetPreferredSizeAsSize: TSize;
    function GetSolidColor: TBGRAPixel;
    function GetTexOpacity: byte;
    function GetTexRepetition: TTextureRepetition;
    function GetTexture: TBGRABitmap;
    function GetToolIconSize: integer;
    function GetVerticalPadding: integer;
    procedure InterfaceMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure InterfaceMouseEnter(Sender: TObject);
    procedure InterfaceMouseLeave(Sender: TObject);
    procedure InterfaceMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure InterfaceMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SetAllowedFillTypes(AValue: TVectorialFillTypes);
    procedure SetCanAdjustToShape(AValue: boolean);
    procedure SetCanEditGradTexPoints(AValue: boolean);
    procedure SetEditingGradTexPoints(AValue: boolean);
    procedure SetFillType(AValue: TVectorialFillType);
    procedure SetGradEndColor(AValue: TBGRAPixel);
    procedure SetGradientType(AValue: TGradientType);
    procedure SetGradInterpolation(AValue: TBGRAColorInterpolation);
    procedure SetGradRepetition(AValue: TBGRAGradientRepetition);
    procedure SetGradStartColor(AValue: TBGRAPixel);
    procedure SetIsTarget(AValue: boolean);
    procedure SetOnChooseColor(AValue: TChooseColorEvent);
    procedure SetOnTextureClick(AValue: TNotifyEvent);
    procedure SetSolidColor(AValue: TBGRAPixel);
    procedure SetTexture(AValue: TBGRABitmap);
    procedure SetTextureOpacity(AValue: byte);
    procedure SetTextureRepetition(AValue: TTextureRepetition);
    procedure SetToolIconSize(AValue: integer);
    procedure SetVerticalPadding(AValue: integer);
  protected
    FInterface: TVectorialFillInterface;
    FOnAdjustToShape: TNotifyEvent;
    FOnEditGradTexPoints: TNotifyEvent;
    FOnChooseColor: TChooseColorEvent;
    FOnFillChange: TNotifyEvent;
    FOnOpacityChange: TNotifyEvent;
    FOnFillTypeChange: TNotifyEvent;
    FOnTextureClick: TNotifyEvent;
    FOnTextureChange: TNotifyEvent;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
      {%H-}WithThemeSpace: Boolean); override;
    procedure DoOnAdjustToShape(Sender: TObject);
    procedure DoOnEditGradTexPoints(Sender: TObject);
    procedure DoOnFillChange(Sender: TObject);
    procedure DoOnFillTypeChange(Sender: TObject);
    procedure DoOnOpacityChange(Sender: TObject);
    procedure DoOnTextureClick(Sender: TObject);
    procedure DoOnTextureChange(Sender: TObject);
    procedure DoOnResize; override;
    procedure DoOnChooseColor({%H-}ASender: TObject; AButton: TMouseButton;
      AColorIndex: integer; var AColorValue: TBGRAPixel; out AHandled: boolean);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure AssignFill(AFill: TVectorialFill);
    function CreateShapeFill(AShape: TVectorShape): TVectorialFill;
    procedure UpdateShapeFill(AShape: TVectorShape; ATarget: TLCFillTarget);
    procedure UpdateFillExceptGeometry(ATargetFill: TVectorialFill);
    property FillType: TVectorialFillType read GetFillType write SetFillType;
    property IsTarget: boolean read GetIsTarget write SetIsTarget;
    property AverageColor: TBGRAPixel read GetAverageColor;
    property SolidColor: TBGRAPixel read GetSolidColor write SetSolidColor;
    property GradientType: TGradientType read GetGradType write SetGradientType;
    property GradStartColor: TBGRAPixel read GetGradStartColor write SetGradStartColor;
    property GradEndColor: TBGRAPixel read GetGradEndColor write SetGradEndColor;
    property GradRepetition: TBGRAGradientRepetition read GetGradRepetition write SetGradRepetition;
    property GradInterpolation: TBGRAColorInterpolation read GetGradInterp write SetGradInterpolation;
    property Texture: TBGRABitmap read GetTexture write SetTexture;
    property TextureRepetition: TTextureRepetition read GetTexRepetition write SetTextureRepetition;
    property TextureOpacity: byte read GetTexOpacity write SetTextureOpacity;
    property CanAdjustToShape: boolean read GetCanAdjustToShape write SetCanAdjustToShape;
    property PreferredSize: TSize read GetPreferredSizeAsSize;
  published
    property AutoSize;
    property Align;
    property Enabled;
    property Visible;
    property ToolIconSize: integer read GetToolIconSize write SetToolIconSize;
    property VerticalPadding: integer read GetVerticalPadding write SetVerticalPadding;
    property AllowedFillTypes: TVectorialFillTypes read GetAllowedFillTypes write SetAllowedFillTypes;
    property CanEditGradTexPoints: boolean read GetCanEditGradTexPoints write SetCanEditGradTexPoints;
    property EditingGradTexPoints: boolean read GetEditingGradTexPoints write SetEditingGradTexPoints;
    property OnChooseColor: TChooseColorEvent read FOnChooseColor write SetOnChooseColor;
    property OnFillChange: TNotifyEvent read FOnFillChange write FOnFillChange;
    property OnOpacityChange: TNotifyEvent read FOnOpacityChange write FOnOpacityChange;
    property OnTextureChange: TNotifyEvent read FOnTextureChange write FOnTextureChange;
    property OnAdjustToShape: TNotifyEvent read FOnAdjustToShape write FOnAdjustToShape;
    property OnEditGradTexPoints: TNotifyEvent read FOnEditGradTexPoints write FOnEditGradTexPoints;
    property OnFillTypeChange: TNotifyEvent read FOnFillTypeChange write FOnFillTypeChange;
    property OnTextureClick: TNotifyEvent read FOnTextureClick write SetOnTextureClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Lazpaint Controls', [TLCVectorialFillControl]);
end;

{ TLCVectorialFillControl }

procedure TLCVectorialFillControl.DoOnChooseColor(ASender: TObject; AButton: TMouseButton;
  AColorIndex: integer; var AColorValue: TBGRAPixel; out AHandled: boolean);
begin
  If Assigned(FOnChooseColor) then
    FOnChooseColor(self, AButton, AColorIndex, AColorValue, AHandled)
  else
    AHandled := false;
end;

procedure TLCVectorialFillControl.DoOnTextureClick(Sender: TObject);
begin
  if Assigned(FOnTextureClick) then FOnTextureClick(self);
end;

function TLCVectorialFillControl.GetAllowedFillTypes: TVectorialFillTypes;
begin
  result := FInterface.AllowedFillTypes;
end;

function TLCVectorialFillControl.GetAverageColor: TBGRAPixel;
begin
  result := FInterface.AverageColor;
end;

function TLCVectorialFillControl.GetCanAdjustToShape: boolean;
begin
  result := FInterface.CanAdjustToShape;
end;

function TLCVectorialFillControl.GetCanEditGradTexPoints: boolean;
begin
  result := FInterface.CanEditGradTexPoints;
end;

function TLCVectorialFillControl.GetEditingGradTexPoints: boolean;
begin
  result := FInterface.EditingGradTexPoints;
end;

function TLCVectorialFillControl.GetFillType: TVectorialFillType;
begin
  result := FInterface.FillType;
end;

function TLCVectorialFillControl.GetGradEndColor: TBGRAPixel;
begin
  result := FInterface.GradEndColor;
end;

function TLCVectorialFillControl.GetGradInterp: TBGRAColorInterpolation;
begin
  result := FInterface.GradInterpolation;
end;

function TLCVectorialFillControl.GetGradRepetition: TBGRAGradientRepetition;
begin
  result := FInterface.GradRepetition;
end;

function TLCVectorialFillControl.GetGradStartColor: TBGRAPixel;
begin
  result := FInterface.GradStartColor;
end;

function TLCVectorialFillControl.GetGradType: TGradientType;
begin
  result := FInterface.GradientType;
end;

function TLCVectorialFillControl.GetIsTarget: boolean;
begin
  result := FInterface.IsTarget;
end;

function TLCVectorialFillControl.GetPreferredSizeAsSize: TSize;
begin
  result.cx:= Width;
  result.cy:= Height;
  GetPreferredSize(result.cx, result.cy);
end;

function TLCVectorialFillControl.GetSolidColor: TBGRAPixel;
begin
  result := FInterface.SolidColor;
end;

function TLCVectorialFillControl.GetTexOpacity: byte;
begin
  result := FInterface.TextureOpacity;
end;

function TLCVectorialFillControl.GetTexRepetition: TTextureRepetition;
begin
  result := FInterface.TextureRepetition;
end;

function TLCVectorialFillControl.GetTexture: TBGRABitmap;
begin
  result := FInterface.Texture;
end;

function TLCVectorialFillControl.GetToolIconSize: integer;
begin
  result := FInterface.ImageListSize.cy;
end;

function TLCVectorialFillControl.GetVerticalPadding: integer;
begin
  result := FInterface.VerticalPadding;
end;

procedure TLCVectorialFillControl.InterfaceMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(OnMouseDown) then OnMouseDown(self, Button, Shift, X,Y);
end;

procedure TLCVectorialFillControl.InterfaceMouseEnter(Sender: TObject);
begin
  if Assigned(OnMouseEnter) then OnMouseEnter(self);
end;

procedure TLCVectorialFillControl.InterfaceMouseLeave(Sender: TObject);
begin
  if Assigned(OnMouseLeave) then OnMouseLeave(self);
end;

procedure TLCVectorialFillControl.InterfaceMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(OnMouseMove) then OnMouseMove(self, Shift, X,Y);
end;

procedure TLCVectorialFillControl.InterfaceMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(OnMouseUp) then OnMouseUp(self, Button, Shift, X,Y);
end;

procedure TLCVectorialFillControl.SetAllowedFillTypes(
  AValue: TVectorialFillTypes);
begin
  FInterface.AllowedFillTypes:= AValue;
end;

procedure TLCVectorialFillControl.SetCanAdjustToShape(AValue: boolean);
begin
  FInterface.CanAdjustToShape := AValue;
end;

procedure TLCVectorialFillControl.SetCanEditGradTexPoints(AValue: boolean);
begin
  FInterface.CanEditGradTexPoints:= AValue;
end;

procedure TLCVectorialFillControl.SetEditingGradTexPoints(AValue: boolean);
begin
  FInterface.EditingGradTexPoints := AValue;
end;

procedure TLCVectorialFillControl.SetFillType(AValue: TVectorialFillType);
begin
  FInterface.FillType := AValue;
end;

procedure TLCVectorialFillControl.SetGradEndColor(AValue: TBGRAPixel);
begin
  FInterface.GradEndColor := AValue;
end;

procedure TLCVectorialFillControl.SetGradientType(AValue: TGradientType);
begin
  FInterface.GradientType := AValue;
end;

procedure TLCVectorialFillControl.SetGradInterpolation(
  AValue: TBGRAColorInterpolation);
begin
  FInterface.GradInterpolation := AValue;
end;

procedure TLCVectorialFillControl.SetGradRepetition(
  AValue: TBGRAGradientRepetition);
begin
  FInterface.GradRepetition := AValue;
end;

procedure TLCVectorialFillControl.SetGradStartColor(AValue: TBGRAPixel);
begin
  FInterface.GradStartColor := AValue;
end;

procedure TLCVectorialFillControl.SetIsTarget(AValue: boolean);
begin
  FInterface.IsTarget := AValue;
end;

procedure TLCVectorialFillControl.SetOnChooseColor(AValue: TChooseColorEvent);
begin
  if FOnChooseColor=AValue then Exit;
  FOnChooseColor:=AValue;
end;

procedure TLCVectorialFillControl.SetOnTextureClick(AValue: TNotifyEvent);
begin
  if FOnTextureClick=AValue then Exit;
  FOnTextureClick:=AValue;
  if Assigned(FOnTextureClick) then
    FInterface.OnTextureClick:= @DoOnTextureClick
  else
    FInterface.OnTextureClick:= nil;
end;

procedure TLCVectorialFillControl.SetSolidColor(AValue: TBGRAPixel);
begin
  FInterface.SolidColor := AValue;
end;

procedure TLCVectorialFillControl.SetTexture(AValue: TBGRABitmap);
begin
  FInterface.Texture := AValue;
end;

procedure TLCVectorialFillControl.SetTextureOpacity(AValue: byte);
begin
  FInterface.TextureOpacity := AValue;
end;

procedure TLCVectorialFillControl.SetTextureRepetition(
  AValue: TTextureRepetition);
begin
  FInterface.TextureRepetition := AValue;
end;

procedure TLCVectorialFillControl.SetToolIconSize(AValue: integer);
begin
  FInterface.ImageListSize := Size(AValue,AValue);
end;

procedure TLCVectorialFillControl.SetVerticalPadding(AValue: integer);
begin
  FInterface.VerticalPadding:= AValue;
end;

procedure TLCVectorialFillControl.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  with FInterface.PreferredSize do
  begin
    PreferredWidth := cx;
    PreferredHeight := cy;
  end;
end;

procedure TLCVectorialFillControl.DoOnAdjustToShape(Sender: TObject);
begin
  if Assigned(FOnAdjustToShape) then FOnAdjustToShape(self);
end;

procedure TLCVectorialFillControl.DoOnEditGradTexPoints(Sender: TObject);
begin
  if Assigned(FOnEditGradTexPoints) then FOnEditGradTexPoints(self);
end;

procedure TLCVectorialFillControl.DoOnFillChange(Sender: TObject);
begin
  if Assigned(FOnFillChange) then FOnFillChange(self);
end;

procedure TLCVectorialFillControl.DoOnFillTypeChange(Sender: TObject);
begin
  InvalidatePreferredSize;
  AdjustSize;
  if Assigned(FOnFillTypeChange) then FOnFillTypeChange(self);
end;

procedure TLCVectorialFillControl.DoOnOpacityChange(Sender: TObject);
begin
  if Assigned(FOnOpacityChange) then FOnOpacityChange(self);
end;

procedure TLCVectorialFillControl.DoOnTextureChange(Sender: TObject);
begin
  if Assigned(FOnTextureChange) then FOnTextureChange(self);
end;

procedure TLCVectorialFillControl.DoOnResize;
begin
  inherited DoOnResize;
  FInterface.LoadImageList;
  FInterface.ContainerSizeChanged;
end;

constructor TLCVectorialFillControl.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FInterface := TVectorialFillInterface.Create(nil, 16,16);
  FInterface.OnChooseColor:=@DoOnChooseColor;
  FInterface.OnFillChange:=@DoOnFillChange;
  FInterface.OnTextureChange:=@DoOnTextureChange;
  FInterface.OnAdjustToShape:=@DoOnAdjustToShape;
  FInterface.OnEditGradTexPoints:=@DoOnEditGradTexPoints;
  FInterface.OnFillTypeChange:=@DoOnFillTypeChange;
  FInterface.OnOpacityChange:=@DoOnOpacityChange;
  FInterface.OnMouseMove:=@InterfaceMouseMove;
  FInterface.OnMouseDown:=@InterfaceMouseDown;
  FInterface.OnMouseUp:=@InterfaceMouseUp;
  FInterface.OnMouseEnter:=@InterfaceMouseEnter;
  FInterface.OnMouseLeave:=@InterfaceMouseLeave;
  FInterface.Container := self;
end;

destructor TLCVectorialFillControl.Destroy;
begin
  FreeAndNil(FInterface);
  inherited Destroy;
end;

procedure TLCVectorialFillControl.AssignFill(AFill: TVectorialFill);
begin
  FInterface.AssignFill(AFill);
end;

function TLCVectorialFillControl.CreateShapeFill(AShape: TVectorShape): TVectorialFill;
begin
  result := FInterface.CreateShapeFill(AShape);
end;

procedure TLCVectorialFillControl.UpdateShapeFill(AShape: TVectorShape;
  ATarget: TLCFillTarget);
begin
  FInterface.UpdateShapeFill(AShape, ATarget);
end;

procedure TLCVectorialFillControl.UpdateFillExceptGeometry(ATargetFill: TVectorialFill);
begin
  FInterface.UpdateFillExceptGeometry(ATargetFill);
end;

end.

