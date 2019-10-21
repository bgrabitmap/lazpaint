from lazpaint import command, colors

if __name__ == "__main__":
  command.show_message("Library to access layer content.")

DM_DRAW = "dmDrawWithTransparency"
DM_LINEAR = "dmLinearBlend"
DM_SET = "dmSet"
DM_SET_EXCEPT_TRANSPARENT = "dmSetExceptTransparent"
DM_XOR = "dmXor"

def put_image(x, y, image, mode=DM_DRAW, opacity=255):
  height = len(image)
  if height == 0: return
  width = max([len(scanline) for scanline in image])
  flattened = ""
  for scanline in image:
    flattened += "".join([str(color) for color in scanline]) + "00000000" * (width - len(scanline))
  command.send("PutImage", X=x, Y=y, Width=width, Height=height, Data=flattened, Mode=mode, Opacity=opacity)

def get_pixel(x, y):
  return colors.str_to_RGBA(command.send("GetPixel?", X=x, Y=y))

def fill(color, mode=DM_DRAW):
  command.send("LayerFill", Color=color, Mode=mode)
