# LazPaint
Image editor, like PaintBrush or Paint.Net, written in Lazarus (Free Pascal). Uses BGRABitmap library.

> You can support the development by [donating](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=MXWCFJJWNQ6A6). That will allow the developper to buy carrots and beans for sustenance.

# Download
[Download LazPaint](https://github.com/bgrabitmap/lazpaint/releases) for Windows, Mac and Linux.

This application can be made [portable](https://wiki.freepascal.org/LazPaint_Make_it_portable). 

Here are [compilation instructions](https://wiki.freepascal.org/LazPaint#Compilation_of_latest_version) if you would like to debug or make a modified version.

# Official Sites
[LazPaint Wiki](http://wiki.freepascal.org/LazPaint)

[LazPaint Forums](http://forum.lazarus.freepascal.org/index.php/board,46.0.html)

[LazPaint on Facebook](https://www.facebook.com/LazPaint)

[LazPaint on YouTUBE](https://www.youtube.com/playlist?list=PLC5C5CAB111B5D9DA)

# Screenshots
![LazPaint Linux dark theme](https://upload.wikimedia.org/wikipedia/commons/c/c0/Lazpaint_version_7.png)
![LazPaint Windows](http://wiki.freepascal.org/images/2/25/Lazpaint_curve_redim.png)
![LazPaint Puppy Linux](http://wiki.freepascal.org/images/5/57/lazpaint6_puppy.png)

# History
LazPaint was started to demonstrate the capabilities of the graphic library BGRABitmap. It provides advanced drawing functions in Lazarus development environment. Both provided a source of inspiration for the other and finally LazPaint became real image editor. Thanks to the help of Lazarus community, the program has been compiled on Windows, Linux, MacOS X, FreeBSD and Raspberry Pi.

# Features
* [Files](http://wiki.freepascal.org/LazPaint_File): read and write a variety of file formats, including layered bitmaps and 3D files.
* [Tools](http://wiki.freepascal.org/LazPaint_Tools): many tools are available to draw on the layers.
* [Edit/Select](http://wiki.freepascal.org/LazPaint_Edit): select parts of an image with antialiasing and modify the selection as a mask.
* [View](http://wiki.freepascal.org/LazPaint_Windows): color window, layer stack window and toolbox window.
* [Command line](http://wiki.freepascal.org/LazPaint_Command_line): call LazPaint from a console.

# Useful keys
* Maintaining Space key down switches temporarily in move mode
* F6 key hides/shows all tool windows
* Ctrl key aligns to image pixels and limits possible angles with rotation tool
* Backspace key erases last point in a polygon or last letter in a text
* Enter key releases the selection
Right mouse button can be used to:
* Swap drawing colors temporarily
* Subtract from selection (selection tool)
* Define light position (shaded text, shaded shapes)
* Finish a shape (polygon, curve)

# Interface
Many common actions can be done with the toolbar. Zoom can be changed with the magnifying glass (+ or -), or by clicking on the 1:1 button to show the image at its original size in pixels, or with the zoom fit button to set the zoom so that the whole image be within the window.
It is possible to undo/redo the 200 last operations. If you have a doubt on what you are drawing, undo back to the beginning, save a copy, and redo the modifications before going further.

# Image manipulation
An image can be resampled, flipped horizontally and vertically.
Smart zoom x3 : resize the image x3 and detects borders; this provides a useful zoom with ancient games sprites.

# Color manipulation
* Colorize : set the color of an image while preserving intensities
* Shift colors : cycle colors and change colorness (saturation)
* Intensity : make colors lighter or darker without making them white
* Lightness : make colors lighter or darker by making them whiter
* Normalize : use the whole range of each color channel and alpha channel
* Negative : invert colors (with gamma correction)
* Linear negative : invert colors (without gamma correction)
* Grayscale : converts colors to grayscale with gamma correction

# Filters
Filters can be applied to the whole image or to the active selection.
* Radial blur : non directional blur
* Motion blur : directional blur
* Custom blur : blur according to a mask
* Sharpen : makes contours more accute, complementary to Smooth
* Smooth : softens whole image, complementary to Sharpen
* Median : computes the median of colors around each pixel, which softens corners
* Contour : draws contours on a white background (like a pencil drawing)
* Emboss : draws contours with shadow
* Sphere : spherical projection
* Cylinder : cylinder projection
* Clouds : add clouds of the current pen color
