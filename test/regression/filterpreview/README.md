# Filter preview restoration (#628)

Build with the same Lazarus package links as LazPaint:

```sh
lazbuild --ws=cocoa --cpu=x86_64 test_restore.lpi
./test_restore
```

Use `--ws=win32` on Windows or `--ws=gtk2` on Linux. Exit status is nonzero
on failure. The test initializes the widgetset but creates no windows and
does not read or write image files.

The test uses the real `TFilterConnector`, image layers and compositor.
Only `NotifyImageChange` is overridden to forward the rectangle to the real
image invalidation without creating application forms. It does not simulate
the restoration or compare an extracted copy of the implementation.

The 216 checks cover three preview/restore cycles at six placements
(origin, positive and negative offsets, and partial off-canvas placement),
with one layer, a lower layer and an upper layer. They check changed preview
pixels, restored layer pixels, preserved clipping and the complete restored
composite. This is not a native checkbox/repaint test.

For native validation, create two layers with a smaller, offset lower layer,
select that layer, open Colors > Intensity, change Shift enough to see the
preview, then toggle Preview several times. The canvas and thumbnail must
both revert when Preview is off. Also check Cancel and accepting a preview.
