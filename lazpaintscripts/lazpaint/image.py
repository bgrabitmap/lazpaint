from lazpaint import command, colors

if __name__ == "__main__":
  command.show_message("Library to act on the whole image.")

def new(width, height, color=colors.TRANSPARENT, ignore_modified=False):
  command.send("FileNew", Width=width, Height=height, BackColor=color, IgnoreModified=ignore_modified)

def get_width():
  return command.send("GetImageWidth?")

def get_height():
  return command.send("GetImageHeight?")

def get_layer_count():
  return command.send("GetLayerCount?")
