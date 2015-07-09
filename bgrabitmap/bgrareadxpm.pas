unit BGRAReadXPM;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPReadXPM, FPimage;

type

  { TBGRAReaderXPM }

  TBGRAReaderXPM = class(TFPReaderXPM)
    protected
      procedure InternalRead(Str: TStream; Img: TFPCustomImage); override;
      function InternalCheck(Str: TStream): boolean; override;
    public
      class procedure ConvertToXPM3(ASource: TStream; ADestination: TStream);
  end;

implementation

uses BGRABitmapTypes;

{ TBGRAReaderXPM }

procedure TBGRAReaderXPM.InternalRead(Str: TStream; Img: TFPCustomImage);
var tempStream: TMemoryStream;
begin
  tempStream := TMemoryStream.Create;
  try
    ConvertToXPM3(Str, tempStream);
    tempStream.Position:= 0;
    try
      img.UsePalette := true;
      inherited InternalRead(tempStream, Img);
    finally
    end;
  finally
    tempStream.free;
  end;
end;

function TBGRAReaderXPM.InternalCheck(Str: TStream): boolean;
var {%H-}magic : array[0..5] of char;
    l : integer;
    prevPos: int64;
begin
  try
    prevPos := str.Position;
    l := str.Read ({%H-}magic[0],sizeof(magic));
    str.Position:= prevPos;
    result := (l = sizeof(magic)) and (magic = '! XPM2');
    if not result then result := inherited InternalCheck(Str)
  except
    result := false;
  end;
end;

class procedure TBGRAReaderXPM.ConvertToXPM3(ASource: TStream;
  ADestination: TStream);
var
  lst: TStringList;
  i : integer;
begin
  lst := TStringList.Create;
  try
    lst.LoadFromStream(ASource);
    if (lst[0] = '! XPM2') and (lst.count > 1) then
    begin
      lst[0] := '/* XPM */';
      lst.Insert(1, 'static char * data[] = {');
      for i := 2 to lst.Count-2 do
        lst[i] := '"' + lst[i] + '",';
      lst[lst.count-1] := '"' + lst[lst.count-1] + '"';
      lst.Add('}');
    end;
    lst.SaveToStream(ADestination);
  finally
    lst.free;
  end;
end;

initialization

  DefaultBGRAImageReader[ifXPixMap] := TBGRAReaderXPM;

end.

