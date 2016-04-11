# copy resources
cp ../../../bin/data/zengl.png ../../../bin/demo07.app/Contents/Resources/
cp ../../../bin/data/back01.jpg ../../../bin/demo07.app/Contents/Resources/
cp ../../../bin/data/ground.png ../../../bin/demo07.app/Contents/Resources/
cp ../../../bin/data/tux_walking.png ../../../bin/demo07.app/Contents/Resources/
cp ../../../bin/data/tux_stand.png ../../../bin/demo07.app/Contents/Resources/
cp ../../../bin/data/font* ../../../bin/demo07.app/Contents/Resources/
# copy binary into bundle
rm ../../../bin/demo07.app/Contents/MacOS/demo07
cp ../../../bin/demo07 ../../../bin/demo07.app/Contents/MacOS/demo07
# make Info.plist and copy icon
cp -f demo07_macosx.plist ../../../bin/demo07.app/Contents/Info.plist
cp ../../../bin/data/zengl.icns ../../../bin/demo07.app/Contents/Resources/demo07.icns