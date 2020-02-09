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
    procedure SetDarkTheme(AValue: boolean);
    procedure CreateStackInterface;
  public
    LazPaintInstance: TLazPaintCustomInstance;
    { public declarations }
    procedure ScrollToItem(AIndex: integer; AUpdateStack: boolean = true);
    procedure AddButton(AAction: TBasicAction);
    procedure InvalidateStack(AScrollIntoView: boolean);
    property DarkTheme: boolean read GetDarkTheme write SetDarkTheme;
    property StackInterface: TLayerStackInterface read GetInterface;
  end;

var TFLayerStack_CustomDPI: integer = 96;

implementation

uses UDarkTheme;

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

procedure TFLayerStack.FormCreate(Sender: TObject);
begin
  Position := poDesigned;
  self.EnsureVisible(False);
  Visible := false;
end;

procedure TFLayerStack.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FStackInterface);
end;

procedure TFLayerStack.FormShow(Sender: TObject);
begin
  CreateStackInterface;
  ClientWidth := StackInterface.GetWidthFor(11);
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
  DarkThemeInstance.Apply(LayerStackControl, AValue, False);
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

{$R *.lfm}

end.

