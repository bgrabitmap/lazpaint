unit UToolbox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, LazPaintType, uscaledpi;

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
  public
    { public declarations }
    LazPaintInstance: TLazPaintCustomInstance;
    procedure AddButton(AToolBar: TToolBar; AAction: TBasicAction);
  end; 

implementation

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

procedure TFToolbox.FormCreate(Sender: TObject);
begin
  {$IFDEF LINUX}
  BorderStyle:= bsDialog;
  {$ENDIF}
  ScaleDPI(Self,OriginalDPI);
end;

{$R *.lfm}

end.
