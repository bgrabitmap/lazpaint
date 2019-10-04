unit LCVectorClipboard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Clipbrd, LCLType, LCVectorOriginal, BGRATransform;

function CopyShapesToClipboard(AShapes: array of TVectorShape; const AMatrix: TAffineMatrix): boolean;
procedure PasteShapesFromClipboard(ATargetContainer: TVectorOriginal; const ATargetMatrix: TAffineMatrix);
function ClipboardHasShapes: boolean;

implementation

var
  vectorClipboardFormat : TClipboardFormat;

function CopyShapesToClipboard(AShapes: array of TVectorShape; const AMatrix: TAffineMatrix): boolean;
var
  tempContainer: TVectorOriginal;
  mem: TMemoryStream;
  i: Integer;
  s: TVectorShape;
begin
  result:= false;
  if length(AShapes)=0 then exit;
  tempContainer := TVectorOriginal.Create;
  mem := TMemoryStream.Create;
  try
    for i := 0 to high(AShapes) do
    begin
      s := AShapes[i].Duplicate;
      s.Transform(AMatrix);
      tempContainer.AddShape(s);
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

procedure PasteShapesFromClipboard(ATargetContainer: TVectorOriginal; const ATargetMatrix: TAffineMatrix);
var
  tempContainer: TVectorOriginal;
  mem: TMemoryStream;
  i: Integer;
  pastedShape: TVectorShape;
  invMatrix: TAffineMatrix;
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
        for i := 0 to tempContainer.ShapeCount-1 do
        begin
          pastedShape := tempContainer.Shape[i].Duplicate;
          pastedShape.Transform(invMatrix);
          ATargetContainer.AddShape(pastedShape);
          if i = tempContainer.ShapeCount-1 then
            ATargetContainer.SelectShape(pastedShape);
        end;
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

