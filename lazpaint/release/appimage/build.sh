#!/bin/bash

sudo apt-get install zsync
wget -nc https://github.com/AppImage/appimagetool/releases/download/continuous/appimagetool-x86_64.AppImage
chmod +x appimagetool-x86_64.AppImage
lazbuild --build-mode=Release ../../../use/bgrabitmap/bgrabitmap/bgrabitmappack.lpk
lazbuild --build-mode=Release ../../../use/bgracontrols/bgracontrols.lpk
lazbuild --build-mode=Release ../../../lazpaintcontrols/lazpaintcontrols.lpk
lazbuild --build-mode=Release ../../../lazpaint/lazpaint.lpi
install -Dm755 ../bin/lazpaint -t AppDir/usr/bin
install -Dm644 ../debian/applications/lazpaint.desktop AppDir/io.github.bgrabitmap.LazPaint.desktop
install -Dm644 ../debian/applications/lazpaint.desktop AppDir/usr/share/applications/io.github.bgrabitmap.LazPaint.desktop
install -Dm644 ../../../resources/icon/256x256.png AppDir/usr/share/icons/hicolor/256x256/apps/io.github.bgrabitmap.LazPaint.png
install -Dm644 ../../../resources/icon/256x256.png AppDir/lazpaint.png
install -Dm644 ../bin/i18n/*.po -t AppDir/usr/share/lazpaint/i18n
cp -r ../../../resources/scripts AppDir/usr/share/lazpaint
cp -r ../bin/models AppDir/usr/share/lazpaint
install -Dm644 ../common/lazpaint.xml AppDir/usr/share/mime/packages/io.github.bgrabitmap.LazPaint.xml
install -Dm644 ../flatpak/io.github.bgrabitmap.LazPaint.metainfo.xml AppDir/usr/share/metainfo/io.github.bgrabitmap.LazPaint.metainfo.xml
install -Dm755 AppRun -t AppDir
ARCH=x86_64 ./appimagetool-x86_64.AppImage AppDir LazPaint.AppImage
