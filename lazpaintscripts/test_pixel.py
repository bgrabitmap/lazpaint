from lazpaint import image, layer, command, colors

width = 256
height = 256
image.new(width, height)
image = []
for y in range(height):
  scanline = [colors.RGB(0,x,y) for x in range(width)]
  image.append(scanline)

layer.put_image(0, 0, image, layer.DM_SET)
if layer.get_pixel(192,64).green != 192:
  command.show_message("The value of the pixel is not correct.")
else:
  command.show_message("Test successful.")
  

