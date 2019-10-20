from lazpaint import *

width = 256
height = 256
file_new(width, height)
layer_fill(WHITE)
image = []
for y in range(height):
  scanline = [RGB(0,x,y) for x in range(width)]
  image.append(scanline)

put_image(0, 0, image, DM_SET)

