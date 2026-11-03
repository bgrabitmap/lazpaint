program test_restore;
{$mode objfpc}{$H+}
uses
  {$ifdef UNIX}cthreads, cwstring,{$endif}
  Interfaces, Forms, SysUtils, Types, LazPaintInstance, UFilterConnector,
  BGRABitmap, BGRABitmapTypes, BGRALayers;
type
  // Keep the real image/compositor and filter connector. Avoid creating GUI
  // forms; forward repaint notifications to the actual image invalidation.
  TTestInstance = class(TLazPaintInstance)
    procedure NotifyImageChange(RepaintNow: boolean; ARect: TRect); override;
  end;
procedure TTestInstance.NotifyImageChange(RepaintNow: boolean; ARect: TRect);
begin
  Image.ImageMayChange(ARect);
end;
var Checks, Failures: integer;
procedure Check(ACondition: boolean; const ADescription: string);
begin
  Inc(Checks);
  if not ACondition then
  begin
    Inc(Failures);
    WriteLn('FAIL: ', ADescription);
  end;
end;
procedure TestPlacement(ALayerCount, ATarget: integer; AOffset: TPoint);
var inst: TTestInstance; layers: TBGRALayeredBitmap;
  connector: TFilterConnector; filtered, expected, actual: TBGRABitmap;
  i, cycle: integer; name: string; oldClip: TRect;
begin
  name := Format('%d layer(s), target %d, offset (%d,%d)',
    [ALayerCount, ATarget, AOffset.X, AOffset.Y]);
  inst := TTestInstance.Create(true);
  try
    layers := TBGRALayeredBitmap.Create(80, 60);
    for i := 0 to ALayerCount-1 do
      if i = ATarget then
      begin
        layers.AddOwnedLayer(TBGRABitmap.Create(30, 30, BGRA(200, 40, 30)));
        layers.LayerOffset[i] := AOffset;
      end else
        layers.AddOwnedLayer(TBGRABitmap.Create(80, 60, BGRA(20, 80, 50, 40)));
    inst.Image.Assign(layers, true, false);
    inst.Image.SetCurrentLayerByIndex(ATarget);
    expected := inst.Image.RenderedImage.Duplicate as TBGRABitmap;
    connector := TFilterConnector.Create(inst, nil, false);
    try
      for cycle := 1 to 3 do
      begin
        filtered := connector.BackupLayer.Duplicate as TBGRABitmap;
        filtered.Fill(BGRA(30, 60, 220));
        connector.PutImage(filtered, false, true);
        actual := inst.Image.RenderedImage;
        Check(not actual.Equals(expected), name + ': preview changes pixels');
        oldClip := connector.ActiveLayer.ClipRect;
        connector.RestoreBackup;
        Check(connector.ActiveLayer.Equals(connector.BackupLayer),
          name + ': underlying pixels restored');
        Check(EqualRect(oldClip, connector.ActiveLayer.ClipRect),
          name + ': clip preserved');
        actual := inst.Image.RenderedImage;
        Check(actual.Equals(expected), name + ': composite restored');
      end;
    finally
      connector.Free;
      expected.Free;
    end;
  finally inst.Free; end;
end;
const Offsets: array[0..5] of TPoint =
  ((X:0;Y:0),(X:0;Y:10),(X:20;Y:10),(X:40;Y:10),
   (X:-10;Y:-5),(X:70;Y:50));
var i: integer;
begin
  Application.Initialize;
  for i := Low(Offsets) to High(Offsets) do
  begin
    TestPlacement(1, 0, Offsets[i]);
    TestPlacement(2, 0, Offsets[i]);
    TestPlacement(2, 1, Offsets[i]);
  end;
  WriteLn(Checks, ' checks, ', Failures, ' failures');
  Halt(Ord(Failures <> 0));
end.
