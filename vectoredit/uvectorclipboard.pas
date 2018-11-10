unit uvectorclipboard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Clipbrd, LCLType, uvectororiginal;

function CopyShapesToClipboard(AShapes: array of TVectorShape): boolean;
procedure PasteShapesFromClipboard(ATargetContainer: TVectorOriginal);

implementation

var
  vectorClipboardFormat : TClipboardFormat;

function CopyShapesToClipboard(AShapes: array of TVectorShape): boolean;
var
  tempContainer: TVectorOriginal;
  mem: TMemoryStream;
  i: Integer;
begin
  result:= false;
  if length(AShapes)=0 then exit;
  tempContainer := TVectorOriginal.Create;
  mem := TMemoryStream.Create;
  try
    for i := 0 to high(AShapes) do
      tempContainer.AddShape(AShapes[i].Duplicate);
    tempContainer.SaveToStream(mem);
    Clipboard.Clear;
    mem.Position:= 0;
    result := Clipboard.AddFormat(vectorClipboardFormat, mem);
  finally
    mem.Free;
    tempContainer.Free;
  end;
end;

procedure PasteShapesFromClipboard(ATargetContainer: TVectorOriginal);
var
  tempContainer: TVectorOriginal;
  mem: TMemoryStream;
  i: Integer;
  pastedShape: TVectorShape;
begin
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
          ATargetContainer.AddShape(pastedShape);
          ATargetContainer.SelectShape(pastedShape);
        end;
      end;
    finally
      tempContainer.Free;
      mem.Free;
    end;
  end;
end;

initialization

  vectorClipboardFormat := RegisterClipboardFormat('TVectorOriginal');

end.

