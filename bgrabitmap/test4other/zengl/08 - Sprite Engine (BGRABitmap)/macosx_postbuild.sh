# copy resources
cp ../../../bin/data/zengl.png ../../../bin/demo08.app/Contents/Resources/
cp ../../../bin/data/miku.png ../../../bin/demo08.app/Contents/Resources/
cp ../../../bin/data/font* ../../../bin/demo08.app/Contents/Resources/
# copy binary into bundle
rm ../../../bin/demo08.app/Contents/MacOS/demo08
cp ../../../bin/demo08 ../../../bin/demo08.app/Contents/MacOS/demo08
# make Info.plist and copy icon
cp -f demo08_macosx.plist ../../../bin/demo08.app/Contents/Info.plist
cp ../../../bin/data/zengl.icns ../../../bin/demo08.app/Contents/Resources/demo08.icns