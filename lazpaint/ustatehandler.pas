unit ustatehandler;

{$mode objfpc}

interface

uses
  Classes, SysUtils; 

type

  { TStateDifference }

  TStateDifference = class
    function TryCompress: boolean; virtual;
  end;

  TState = class
    function ComputeDifferenceFrom(AState: TState): TStateDifference; virtual; abstract;
    function ApplyDifference(ADifference: TStateDifference): TState; virtual; abstract;
    function ReverseDifference(ADifference: TStateDifference): TState; virtual; abstract;
    function Duplicate: TState; virtual; abstract;
  end;

implementation

{ TStateDifference }

function TStateDifference.TryCompress: boolean;
begin
  result := false;
end;

end.

