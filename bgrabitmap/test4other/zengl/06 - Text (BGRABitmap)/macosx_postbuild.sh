# copy resources
cp ../../../bin/data/font* ../../../bin/demo06.app/Contents/Resources/
# copy binary into bundle
rm ../../../bin/demo06.app/Contents/MacOS/demo06
cp ../../../bin/demo06 ../../../bin/demo06.app/Contents/MacOS/demo06
# make Info.plist and copy icon
cp -f demo06_macosx.plist ../../../bin/demo06.app/Contents/Info.plist
cp ../../../bin/data/zengl.icns ../../../bin/demo06.app/Contents/Resources/demo06.icns