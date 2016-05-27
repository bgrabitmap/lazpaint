unit UToolbox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, LazPaintType;

type

  { TFToolbox }

  TFToolbox = class(TForm)
    ToolBar1: TToolBar;
    ToolBar2: TToolBar;
    ToolBar3: TToolBar;
    ToolBar4: TToolBar;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    procedure SetImages(AToolBar: TToolBar; AImages: TImageList);
  public
    { public declarations }
    LazPaintInstance: TLazPaintCustomInstance;
    procedure AddButton(AToolBar: TToolBar; AAction: TBasicAction);
    procedure SetImages(AImages: TImageList);
  end; 

implementation

uses math;

{ TFToolbox }

procedure TFToolbox.FormShow(Sender: TObject);
begin
  Position := poDesigned;
  self.EnsureVisible(False);
end;

procedure TFToolbox.AddButton(AToolBar: TToolBar; AAction: TBasicAction);
var button: TToolButton;
begin
  button := TToolButton.Create(AToolBar);
  button.Parent := AToolbar;
  button.Action := AAction;
  button.Style := tbsButton;
end;

procedure TFToolbox.SetImages(AImages: TImageList);
var w: integer;
begin
  Toolbar1.Top := 0;
  SetImages(ToolBar1, AImages);
  Toolbar2.Top := ToolBar1.Top+ToolBar1.Height;
  SetImages(ToolBar2, AImages);
  Toolbar3.Top := ToolBar2.Top+ToolBar2.Height;
  SetImages(ToolBar3, AImages);
  Toolbar4.Top := ToolBar3.Top+ToolBar3.Height;
  SetImages(ToolBar4, AImages);
  ClientHeight := ToolBar4.Top+ToolBar4.Height+2;
  w := 0;
  w := Max(w,Toolbar1.Width);
  w := Max(w,Toolbar2.Width);
  w := Max(w,Toolbar3.Width);
  w := Max(w,Toolbar4.Width);
  ClientWidth := w;
end;

procedure TFToolbox.SetImages(AToolBar: TToolBar; AImages: TImageList);
begin
  AToolBar.Images := AImages;
  AToolBar.ButtonWidth := AImages.Width + 4;
  AToolBar.ButtonHeight := AImages.Height + 4;
  AToolBar.Height := AToolBar.ButtonHeight+4;
  AToolBar.Width := AToolBar.ButtonWidth*AToolBar.ButtonCount+4;
end;

procedure TFToolbox.FormCreate(Sender: TObject);
begin
  {$IFDEF LINUX}
  BorderStyle:= bsDialog;
  {$ENDIF}
end;

{$R *.lfm}

end.

