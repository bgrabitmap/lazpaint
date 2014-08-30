unit bgrareadjpeg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPReadJPEG;

type

  { TBGRAReaderJpeg }

  TBGRAReaderJpeg = class(TFPReaderJPEG)
    constructor Create; override;
  protected
    function InternalCheck(Str: TStream): boolean; override;
  end;

implementation

uses BGRABitmapTypes;

{ TBGRAReaderJpeg }

constructor TBGRAReaderJpeg.Create;
begin
  inherited Create;
  Performance := jpBestQuality;
end;

function TBGRAReaderJpeg.InternalCheck(Str: TStream): boolean;
var {%H-}magic: packed array[0..3] of byte;
  OldPos,BytesRead:int64;
begin
  Result:=false;
  if Str=nil then exit;
  OldPos:= str.Position;
  BytesRead := str.Read({%H-}magic,sizeof(magic));
  str.Position:=OldPos;
  if BytesRead<>sizeof(magic) then exit;
  if (magic[0] = $ff) and (magic[1] = $d8) and (magic[2] = $ff) and (magic[3] >= $c0) then result := true;
end;

initialization

  DefaultBGRAImageReader[ifJpeg] := TBGRAReaderJpeg;

end.

