// SPDX-License-Identifier: GPL-3.0-only
unit UChooseColor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, ExtCtrls,
  BGRABitmap, BGRABitmapTypes,
  LazPaintType, UChooseColorInterface;

type

  { TFChooseColor }

  TFChooseColor = class(TForm)
    ChooseColorControl: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function GetColorTarget: TColorTarget;
    function GetDarkTheme: boolean;
    function GetEditorVisible: boolean;
    function GetLazPaintInstance: TLazPaintCustomInstance;
    procedure SetColorTarget(AValue: TColorTarget);
    procedure SetDarkTheme(AValue: boolean);
    procedure SetLazPaintInstance(AValue: TLazPaintCustomInstance);
    procedure ThemeChanged(Sender: TObject);
  protected
    FInterface: TChooseColorInterface;
  public
    { public declarations }
    procedure SetCurrentColor(value: TBGRAPixel);
    function GetCurrentColor: TBGRAPixel;
    procedure AdjustControlHeight;
    procedure HideEditor;
    property DarkTheme: boolean read GetDarkTheme write SetDarkTheme;
    property LazPaintInstance: TLazPaintCustomInstance read GetLazPaintInstance write SetLazPaintInstance;
    property EditorVisible: boolean read GetEditorVisible;
    property ColorTarget: TColorTarget read GetColorTarget write SetColorTarget;
  end;

var TFChooseColor_CustomDPI: integer = 96;

implementation

{ TFChooseColor }

procedure TFChooseColor.FormCreate(Sender: TObject);
begin
  FInterface := TChooseColorInterface.Create(ChooseColorControl, TFChooseColor_CustomDPI);
  BorderStyle := ToolWindowSizeable;
  FormStyle := ToolWindowStyle;
  Position := poDesigned;
end;

procedure TFChooseColor.FormDeactivate(Sender: TObject);
begin
  if Assigned(FInterface) then FInterface.HideEditor;
end;

procedure TFChooseColor.FormDestroy(Sender: TObject);
begin
  if Assigned(LazPaintInstance) then
    LazPaintInstance.RegisterThemeListener(@ThemeChanged, false);
  FreeAndNil(FInterface);
end;

procedure TFChooseColor.FormShow(Sender: TObject);
begin
  self.EnsureVisible(False);
end;

procedure TFChooseColor.SetCurrentColor(value: TBGRAPixel);
begin
  if Assigned(FInterface) then
    FInterface.SetCurrentColor(value);
end;

function TFChooseColor.GetCurrentColor: TBGRAPixel;
begin
  if Assigned(FInterface) then
    result := FInterface.GetCurrentColor
  else
    result := BGRAWhite;
end;

procedure TFChooseColor.AdjustControlHeight;
begin
  if Assigned(FInterface) then
    FInterface.AdjustControlHeight;
end;

procedure TFChooseColor.HideEditor;
begin
  if Assigned(FInterface) then
    FInterface.HideEditor;
end;

procedure TFChooseColor.SetDarkTheme(AValue: boolean);
begin
  if Assigned(FInterface) then
  begin
    FInterface.DarkTheme := AValue;
    self.Color := ChooseColorControl.Color;
  end;
end;

procedure TFChooseColor.SetLazPaintInstance(AValue: TLazPaintCustomInstance);
begin
  if Assigned(FInterface) then
  begin
    if Assigned(FInterface.LazPaintInstance) then
      FInterface.LazPaintInstance.RegisterThemeListener(@ThemeChanged, false);
    FInterface.LazPaintInstance:= AValue;
    if Assigned(FInterface.LazPaintInstance) then
      FInterface.LazPaintInstance.RegisterThemeListener(@ThemeChanged, true);

    with FInterface.GetPreferredSize do
    begin
      ClientWidth := cx;
      ClientHeight := cy;
      ChooseColorControl.Width := cx;
      ChooseColorControl.Height := cy;
    end;
    Constraints.MinWidth := ClientWidth div 2 + (Width - ClientWidth);
    Constraints.MinHeight := ClientHeight div 2 + (Height - ClientHeight);
  end;
end;

procedure TFChooseColor.ThemeChanged(Sender: TObject);
begin
  if Assigned(FInterface) and Assigned(LazPaintInstance) then
    FInterface.DarkTheme := LazPaintInstance.DarkTheme;
end;

function TFChooseColor.GetDarkTheme: boolean;
begin
  if Assigned(FInterface) then
    result := FInterface.DarkTheme
  else
    result := false;
end;

function TFChooseColor.GetColorTarget: TColorTarget;
begin
  if Assigned(FInterface) then
    result := FInterface.ColorTarget
  else
    result := ctForeColorSolid;
end;

function TFChooseColor.GetEditorVisible: boolean;
begin
  if Assigned(FInterface) then
    result := FInterface.EditorVisible
  else
    result := false;
end;

function TFChooseColor.GetLazPaintInstance: TLazPaintCustomInstance;
begin
  if Assigned(FInterface) then
    result := FInterface.LazPaintInstance
  else
    result := nil;
end;

procedure TFChooseColor.SetColorTarget(AValue: TColorTarget);
begin
  if Assigned(FInterface) then
    FInterface.ColorTarget := AValue;
end;

{$R *.lfm}

end.

