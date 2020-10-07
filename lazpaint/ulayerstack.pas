// SPDX-License-Identifier: GPL-3.0-only
unit ULayerstack;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, ExtCtrls, LazPaintType,
  ULayerStackInterface;

type
  { TFLayerStack }

  TFLayerStack = class(TForm)
    LayerStackControl: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FStackInterface: TLayerStackInterface;
    { private declarations }
    function GetDarkTheme: boolean;
    function GetInterface: TLayerStackInterface;
    function GetZoomFactor: single;
    procedure SetDarkTheme(AValue: boolean);
    procedure CreateStackInterface;
    procedure SetZoomFactor(AValue: single);
  public
    LazPaintInstance: TLazPaintCustomInstance;
    { public declarations }
    procedure ScrollToItem(AIndex: integer; AUpdateStack: boolean = true);
    procedure AddButton(AAction: TBasicAction);
    procedure InvalidateStack(AScrollIntoView: boolean);
    property DarkTheme: boolean read GetDarkTheme write SetDarkTheme;
    property StackInterface: TLayerStackInterface read GetInterface;
    property ZoomFactor: single read GetZoomFactor write SetZoomFactor;
  end;

var TFLayerStack_CustomDPI: integer = 96;

implementation

uses UDarkTheme, LCScaleDPI;

{ TFLayerStack }

function TFLayerStack.GetDarkTheme: boolean;
begin
  if Assigned(StackInterface) then
    result := StackInterface.DarkTheme
  else
    result := false;
end;

function TFLayerStack.GetInterface: TLayerStackInterface;
begin
  CreateStackInterface;
  result := FStackInterface;
end;

function TFLayerStack.GetZoomFactor: single;
begin
  if Assigned(FStackInterface) then
    result := FStackInterface.ZoomFactor;
end;

procedure TFLayerStack.FormCreate(Sender: TObject);
begin
  Position := poDesigned;
  self.EnsureVisible(False);
  ClientWidth := DoScaleX(224, OriginalDPI, TFLayerStack_CustomDPI);
  ClientHeight := DoScaleX(300, OriginalDPI, TFLayerStack_CustomDPI);
  Constraints.MinWidth := ClientWidth div 2 + (Width - ClientWidth);
  Constraints.MinHeight := ClientHeight div 2 + (Height - ClientHeight);
  LayerStackControl.Width := ClientWidth;
  LayerStackControl.Height := ClientHeight;
  Visible := false;
end;

procedure TFLayerStack.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FStackInterface);
end;

procedure TFLayerStack.FormShow(Sender: TObject);
begin
  CreateStackInterface;
end;

procedure TFLayerStack.ScrollToItem(AIndex: integer; AUpdateStack: boolean);
begin
  if Assigned(StackInterface) then
    StackInterface.ScrollToItem(AIndex, AUpdateStack);
end;

procedure TFLayerStack.AddButton(AAction: TBasicAction);
begin
  if Assigned(StackInterface) then
    StackInterface.AddButton(AAction);
end;

procedure TFLayerStack.InvalidateStack(AScrollIntoView: boolean);
begin
  if Assigned(FStackInterface) then
    StackInterface.InvalidateStack(AScrollIntoView);
end;

procedure TFLayerStack.SetDarkTheme(AValue: boolean);
begin
  DarkThemeInstance.Apply(self, AValue, False);
  if Assigned(StackInterface) then
    StackInterface.DarkTheme:= AValue;
end;

procedure TFLayerStack.CreateStackInterface;
begin
  if (FStackInterface = nil) and Assigned(LazPaintInstance) then
  begin
    FStackInterface := TLayerStackInterface.Create(LayerStackControl, LazPaintInstance);
    FStackInterface.DPI := TFLayerStack_CustomDPI;
  end;
end;

procedure TFLayerStack.SetZoomFactor(AValue: single);
begin
  if Assigned(StackInterface) then
    StackInterface.ZoomFactor := AValue;
end;

{$R *.lfm}

end.

