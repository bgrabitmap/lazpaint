unit utoolbox; 

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
    ToolBar4: TToolBar;
    ToolBar5: TToolBar;
    ToolButton_TextureMapping: TToolButton;
    ToolButton_Phong: TToolButton;
    ToolButton_Text: TToolButton;
    ToolButton_Deformation: TToolButton;
    ToolButton_EditDeselect: TToolButton;
    ToolButton_Gradient: TToolButton;
    ToolButton_Hand: TToolButton;
    ToolButton_Floodfill: TToolButton;
    ToolButton_MoveSelection: TToolButton;
    ToolButton_SelectCurve: TToolButton;
    ToolButton_SelectRect: TToolButton;
    ToolButton_SelectEllipse: TToolButton;
    ToolButton_SelectPoly: TToolButton;
    ToolButton_Pen: TToolButton;
    ToolButton_SelectionPen: TToolButton;
    ToolButton_RotateSelection: TToolButton;
    ToolButton_MagicWand: TToolButton;
    ToolButton_Eraser: TToolButton;
    ToolButton_ColorPicker: TToolButton;
    ToolButton_Rect: TToolButton;
    ToolButton_Ellipse: TToolButton;
    ToolButton_Spline: TToolButton;
    ToolButton_Polygon: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    LazPaintInstance: TLazPaintCustomInstance;
  end; 

implementation

{ TFToolbox }

procedure TFToolbox.FormShow(Sender: TObject);
begin
  Position := poDesigned;
  self.EnsureVisible(False);
end;

procedure TFToolbox.FormCreate(Sender: TObject);
begin
  ScaleDPI(Self,OriginalDPI);
end;

initialization
  {$I utoolbox.lrs}

end.

