unit utestback;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes, Graphics, utest;

type
  { TTestBack }

  TTestBack = class(TTest)
  public
    constructor Create;
    destructor Destroy; override;
    procedure UpdateBackground(Width,Height: integer);
  protected
    backgroundTile,backgroundImg: TBGRABitmap;
  end;

implementation

{ TTestBack }

constructor TTestBack.Create;
begin
  inherited Create;

  backgroundTile := TBGRABitmap.Create('..'+pathdelim+'img'+pathdelim+'diamondback.png');
  backgroundImg := nil;
end;

destructor TTestBack.Destroy;
begin
  backgroundImg.Free;
  backgroundTile.free;
  inherited Destroy;
end;

procedure TTestBack.UpdateBackground(Width,Height: integer);
begin
  if (backgroundImg <> nil) and ((backgroundImg.Width <> Width) or (backgroundImg.Height <> Height)) then
  FreeAndNil(backgroundImg);

if backgroundImg = nil then
  backgroundImg := backgroundTile.GetPart(rect(0,0,Width,Height)) as TBGRABitmap;
end;

end.

