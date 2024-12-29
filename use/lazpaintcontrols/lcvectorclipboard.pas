// SPDX-License-Identifier: GPL-3.0-only
unit LCVectorClipboard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Clipbrd, LCLType, LCVectorOriginal, BGRATransform, BGRABitmapTypes;

function CopyShapesToClipboard(AShapes: array of TVectorShape; const AMatrix: TAffineMatrix): boolean;
procedure PasteShapesFromClipboard(ATargetContainer: TVectorOriginal; const ATargetMatrix: TAffineMatrix; const ABounds: TRectF);
function ClipboardHasShapes: boolean;

implementation

uses math;

var
  vectorClipboardFormat : TClipboardFormat;

function CopyShapesToClipboard(AShapes: array of TVectorShape; const AMatrix: TAffineMatrix): boolean;
var
  tempContainer: TVectorOriginal;
  mem: TMemoryStream;
  i, j: Integer;
  s: TVectorShape;
  multiSel: IVectorMultishape;
begin
  result:= false;
  if length(AShapes)=0 then exit;
  tempContainer := TVectorOriginal.Create;
  mem := TMemoryStream.Create;
  try
    for i := 0 to high(AShapes) do
    begin
      if AShapes[i] is VectorMultiselectionFactory then
      begin
        multiSel := AShapes[i].GetAsMultishape;
        for j := 0 to multiSel.ShapeCount-1 do
        begin
          s := multiSel.GetShape(j).Duplicate;
          s.Transform(AMatrix);
          tempContainer.AddShape(s);
        end;
      end else
      begin
        s := AShapes[i].Duplicate;
        s.Transform(AMatrix);
        tempContainer.AddShape(s);
      end;
    end;
    tempContainer.SaveToStream(mem);
    Clipboard.Clear;
    mem.Position:= 0;
    result := Clipboard.AddFormat(vectorClipboardFormat, mem);
  finally
    mem.Free;
    tempContainer.Free;
  end;
end;

procedure PasteShapesFromClipboard(ATargetContainer: TVectorOriginal; const ATargetMatrix: TAffineMatrix; const ABounds: TRectF);
var
  tempContainer: TVectorOriginal;
  mem: TMemoryStream;
  i: Integer;
  pastedShape: TVectorShape;
  pastedShapes: TVectorShapes;
  invMatrix, m: TAffineMatrix;
  pastedBounds: TRectF;
  ofs: TPointF;
begin
  if not IsAffineMatrixInversible(ATargetMatrix) then exit;
  invMatrix := AffineMatrixInverse(ATargetMatrix);
  if Clipboard.HasFormat(vectorClipboardFormat) then
  begin
    mem := TMemoryStream.Create;
    tempContainer := TVectorOriginal.Create;
    try
      if Clipboard.GetFormat(vectorClipboardFormat, mem) then
      begin
        mem.Position:= 0;
        tempContainer.LoadFromStream(mem);
        pastedBounds := EmptyRectF;
        ofs := PointF(0, 0);
        if not ABounds.IsEmpty then
        begin
          for i := 0 to tempContainer.ShapeCount-1 do
            pastedBounds := pastedBounds.Union(pastedBounds,
              tempContainer.Shape[i].GetAlignBounds(InfiniteRect, AffineMatrixIdentity), true);
          if (pastedBounds.Left < ABounds.Left) and (pastedBounds.Right < ABounds.Right) then
            ofs.x := ceil(ABounds.Left - pastedBounds.Left) else
          if (pastedBounds.Right > ABounds.Right) and (pastedBounds.Left > ABounds.Left) then
            ofs.x := floor(ABounds.Right - pastedBounds.Right);
          if (pastedBounds.Top < ABounds.Top) and (pastedBounds.Bottom < ABounds.Bottom) then
            ofs.y := ceil(ABounds.Top - pastedBounds.Top) else
          if (pastedBounds.Bottom > ABounds.Bottom) and (pastedBounds.Top > ABounds.Top) then
            ofs.y := floor(ABounds.Bottom - pastedBounds.Bottom);
        end;
        m := invMatrix*AffineMatrixTranslation(ofs.x,ofs.y);
        ATargetContainer.DeselectShapes;
        pastedShapes := TVectorShapes.Create;
        for i := 0 to tempContainer.ShapeCount-1 do
        begin
          pastedShape := tempContainer.Shape[i].Duplicate;
          pastedShape.Transform(m);
          pastedShapes.Add(pastedShape);
        end;
        ATargetContainer.AddShapes(pastedShapes);
        ATargetContainer.SelectShapes(pastedShapes);
        pastedShapes.Free;
      end;
    finally
      tempContainer.Free;
      mem.Free;
    end;
  end;
end;

function ClipboardHasShapes: boolean;
begin
 result := Clipboard.HasFormat(vectorClipboardFormat);
end;

initialization

  vectorClipboardFormat := RegisterClipboardFormat('TVectorOriginal');

end.

