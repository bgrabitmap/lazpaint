from lazpaint import image, layer, dialog, colors, view

view.set_zoom(1)
width = 256
height = 256
image.new(width, height)
red = dialog.input_value("Red value (0..255)", 0)
image = []
for y in range(height):
  scanline = [colors.RGB(red,x,y) for x in range(width)]
  image.append(scanline)

layer.put_image(0, 0, image, layer.DM_SET)
if layer.get_pixel(192,64).green != 192:
  dialog.show_message("The value of the pixel is not correct.")
else:
  dialog.show_message("Test successful.")
  
view.zoom_fit()
view.set_zoom(10)
view.set_grid_visible(True)
