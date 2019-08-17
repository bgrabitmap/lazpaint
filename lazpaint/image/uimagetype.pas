unit UImageType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRALayers;

type
  TCustomLayerAction = class;

  TOnTryStopEventHandler = procedure(ASender: TCustomLayerAction) of object;
  TModifyImageCallback = procedure(var AImage: TBGRABitmap) of object;

  TCustomLayerAction = class
     procedure TryStop; virtual; abstract;
  end;

function DuplicateBitmap(ABitmap: TBGRABitmap): TBGRABitmap;
function DuplicateLayeredBitmap(ALayeredBitmap: TBGRALayeredBitmap): TBGRALayeredBitmap;
procedure SerializeBitmap(ABitmap: TBGRABitmap; AStream: TStream);

implementation

function DuplicateBitmap(ABitmap: TBGRABitmap): TBGRABitmap;
begin
  if ABitmap = nil then
    result := nil
  else
    result := ABitmap.Duplicate as TBGRABitmap;
end;

function DuplicateLayeredBitmap(ALayeredBitmap: TBGRALayeredBitmap): TBGRALayeredBitmap;
begin
  if ALayeredBitmap = nil then
    result := nil
  else
    result := ALayeredBitmap.Duplicate(True); //keep same layer ids for undo list
end;

procedure SerializeBitmap(ABitmap: TBGRABitmap; AStream: TStream);
begin
if ABitmap <> nil then
  ABitmap.Serialize(AStream) else
  TBGRABitmap.SerializeEmpty(AStream);
end;


end.

