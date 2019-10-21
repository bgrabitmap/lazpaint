from lazpaint_colors import *
from lazpaint_command import command

DM_DRAW = "dmDrawWithTransparency"
DM_LINEAR = "dmLinearBlend"
DM_SET = "dmSet"
DM_SET_EXCEPT_TRANSPARENT = "dmSetExceptTransparent"
DM_XOR = "dmXor"

def file_new(width, height, color=TRANSPARENT, ignore_modified=False):
  command("FileNew", Width=width, Height=height, BackColor=color, IgnoreModified=ignore_modified)

def file_open(file_name=None, ignore_modified=False):
  command("FileOpen", FileName=file_name, IgnoreModified=ignore_modified)

def file_save(skip_options=False):
  return command("FileSave?", SkipOptions=skip_options)

def file_save_as(file_name=None, validate=False, overwrite=False, skip_options=False):
  return command("FileSaveAs?", FileName=file_name, Validate=validate, Overwrite=overwrite, SkipOptions=skip_options) 

def file_reload(ignore_modified=False):
  command("FileReload", IgnoreModified=ignore_modified)

def file_load_selection(file_name=None):
  command("FileLoadSelection", FileName=file_name)

def file_save_selection_as(file_name=None):
  return command("FileSaveSelectionAs?", FileName=file_name)

def get_file_name():
  return command("GetFileName?") 

def put_image(x, y, image, mode=DM_DRAW, opacity=255):
  height = len(image)
  if height == 0: return
  width = max([len(scanline) for scanline in image])
  flattened = ""
  for scanline in image:
    flattened += "".join([str(color) for color in scanline]) + "00000000" * (width - len(scanline))
  command("PutImage", X=x, Y=y, Width=width, Height=height, Data=flattened, Mode=mode, Opacity=opacity)

def get_pixel(x, y):
  return str_to_RGBA(command("GetPixel?", X=x, Y=y))

def layer_fill(color, mode=DM_DRAW):
  command("LayerFill", Color=color, Mode=mode)

def get_image_width():
  return command("GetImageWidth?")

def get_image_height():
  return command("GetImageHeight?")

def show_message(message):
  command("ShowMessage?", Message=message)

def get_layer_count():
  return command("GetLayerCount?")

if __name__ == "__main__":
  show_message("This is the script library.")

