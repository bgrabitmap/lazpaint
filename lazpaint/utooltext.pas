unit UToolText;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, utool, utoolbasic, LCLType, Graphics, BGRABitmap, BGRABitmapTypes, BGRATextFX,
  BGRAGradients, LCVectorOriginal;

type

  { TToolText }

  TToolText = class(TVectorialTool)
  protected
    function CreateShape: TVectorShape; override;
    procedure ShapeChange(ASender: TObject; ABounds: TRectF); override;
    procedure ShapeEditingChange(ASender: TObject); override;
    procedure AssignShapeStyle; override;
    function SlowShape: boolean; override;
    procedure QuickDefineEnd; override;
  public
    function ToolKeyDown(var key: Word): TRect; override;
    function ToolCopy: boolean; override;
    function ToolCut: boolean; override;
    function ToolPaste: boolean; override;
    function ToolProvideCopy: boolean; override;
    function ToolProvideCut: boolean; override;
    function ToolProvidePaste: boolean; override;
  end;

implementation

uses LCVectorTextShapes, BGRALayerOriginal, BGRATransform;

{ TToolText }

function TToolText.CreateShape: TVectorShape;
begin
  result := TTextShape.Create(nil);
end;

procedure TToolText.ShapeChange(ASender: TObject; ABounds: TRectF);
begin
  with (FShape as TTextShape) do
    Manager.ToolLightPosition := Point(round(LightPosition.X),round(LightPosition.Y));
  inherited ShapeChange(ASender, ABounds);
end;

procedure TToolText.ShapeEditingChange(ASender: TObject);
begin
  with (FShape as TTextShape) do
    Manager.ToolTextAlign := ParagraphAlignment;
  inherited ShapeEditingChange(ASender);
end;

procedure TToolText.AssignShapeStyle;
begin
  with TTextShape(FShape) do
  begin
    FontEmHeight:= Manager.ToolTextFont.Size*ScreenInfo.PixelsPerInchY/72;
    FontName:= Manager.ToolTextFont.Name;
    FontStyle:= Manager.ToolTextFont.Style;

    if Manager.GetToolTexture <> nil then
      FShape.PenFill.SetTexture(Manager.GetToolTexture,AffineMatrixIdentity,Manager.ToolTextureOpacity)
    else
    begin
      if FSwapColor then
        FShape.PenFill.SetSolid(Manager.ToolBackColor)
      else
        FShape.PenFill.SetSolid(Manager.ToolForeColor);
    end;

    if Manager.ToolTextOutline and (Manager.ToolTextOutlineWidth>0) and
       (Manager.ToolBackColor.alpha > 0) then
    begin
      if FSwapColor then
        FShape.OutlineFill.SetSolid(Manager.ToolForeColor)
      else
        FShape.OutlineFill.SetSolid(Manager.ToolBackColor);
      OutlineWidth := Manager.ToolTextOutlineWidth;
    end
    else
      OutlineFill.Clear;

    LightPosition := PointF(Manager.ToolLightPosition.X,Manager.ToolLightPosition.Y);
    AltitudePercent:= Manager.ToolShapeAltitude;
    ParagraphAlignment:= Manager.ToolTextAlign;
    PenPhong := Manager.ToolTextPhong;
  end;
end;

function TToolText.SlowShape: boolean;
begin
  Result:= true;
end;

procedure TToolText.QuickDefineEnd;
begin
  FShape.Usermode := vsuEditText;
end;

function TToolText.ToolKeyDown(var key: Word): TRect;
var
  keyUtf8: TUTF8Char;
  handled: Boolean;
begin
  if Key = VK_SPACE then
  begin
    keyUtf8:= ' ';
    result := ToolKeyPress(keyUtf8);
    Key := 0;
  end else
  if (Key = VK_ESCAPE) and Assigned(FShape) then
  begin
    result := ValidateShape;
    Key := 0;
  end else
  if (Key = VK_RETURN) and Assigned(FShape) then
  begin
    handled := false;
    FShape.KeyDown(FShiftState, skReturn, handled);
    if handled then Key := 0;
  end else
    Result:=inherited ToolKeyDown(key);
end;

function TToolText.ToolCopy: boolean;
begin
  Result:= Assigned(FShape) and TTextShape(FShape).CopySelection;
end;

function TToolText.ToolCut: boolean;
begin
  Result:= Assigned(FShape) and TTextShape(FShape).CutSelection;
end;

function TToolText.ToolPaste: boolean;
begin
  Result:= Assigned(FShape) and TTextShape(FShape).PasteSelection;
end;

function TToolText.ToolProvideCopy: boolean;
begin
  Result:= Assigned(FShape) and TTextShape(FShape).HasSelection;
end;

function TToolText.ToolProvideCut: boolean;
begin
  Result:= Assigned(FShape) and TTextShape(FShape).HasSelection;
end;

function TToolText.ToolProvidePaste: boolean;
begin
  Result:= Assigned(FShape);
end;

initialization

    RegisterTool(ptText, TToolText);

end.

