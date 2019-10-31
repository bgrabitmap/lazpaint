from lazpaint import command, dialog, colors

if __name__ == "__main__":
  dialog.show_message("Library to access layer content.")

DM_DRAW = "dmDrawWithTransparency"
DM_LINEAR = "dmLinearBlend"
DM_SET = "dmSet"
DM_SET_EXCEPT_TRANSPARENT = "dmSetExceptTransparent"
DM_XOR = "dmXor"

def get_id():
  return command.send("LayerGetId?")

def select_id(id):
  command.send("LayerSelectId", Id=id)

def new():
  return command.send("LayerAddNew?")

def add_from_file(file_name):
  return command.send("LayerFromFile?", FileName=file_name)

def duplicate():
  return command.send("LayerDuplicate?")

def merge_over():
  command.send("LayerMergeOver")

def remove():
  command.send("LayerRemoveCurrent")

def get_count():
  return command.send("GetLayerCount?")

def rasterize():
  command.send("LayerRasterize")

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

def horizontal_flip():
  command.send("LayerHorizontalFlip")

def vertical_flip():
  command.send("LayerVerticalFlip")

def clear_alpha(back_color=None):
  command.send("ImageClearAlpha", BackColor=back_color)

def fill_background(back_color=None):
  command.send("ImageFillBackground", BackColor=back_color)
