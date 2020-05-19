# Negative
from lazpaint import image, layer, colors, dialog

width, height = image.get_size()
image = layer.get_image(0, 0, width, height)
for y in range(0, height):
  scanline = image[y]
  for x in range(0, width):    
    scanline[x] = scanline[x].linear_negative()
layer.put_image(0, 0, image, layer.DM_SET)

