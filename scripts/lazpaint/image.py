from lazpaint import command, dialog, colors

if __name__ == "__main__":
  dialog.show_message("Library to act on the whole image.")

def new(width, height, color=colors.TRANSPARENT, ignore_modified=False):
  command.send("FileNew", Width=width, Height=height, BackColor=color, IgnoreModified=ignore_modified)

def get_width():
  return command.send("GetImageWidth?")

def get_height():
  return command.send("GetImageHeight?")

def get_layer_index():
  return command.send("GetLayerIndex?")

def select_layer_index(index):
  return command.send("SelectLayerIndex", Index=index)

def get_layer_count():
  return command.send("GetLayerCount?")

def get_frame_index():
  return command.send("GetFrameIndex?")

def get_frame_count():
  return command.send("GetFrameCount?")

def load_frame(frame_index=None, ignore_modified=False):
  return command.send("FileChooseEntry?", EntryIndex=frame_index, IgnoreModified=ignore_modified)

def new_frame(width=None, height=None, back_color=colors.TRANSPARENT, ignore_modified=False):
  command.send("FileNewEntry", Width=width, Height=height, BackColor=back_color, IgnoreModified=ignore_modified)

def open(file_name=None, ignore_modified=False):
  command.send("FileOpen", FileName=file_name, IgnoreModified=ignore_modified)

def save(skip_options=False):
  return command.send("FileSave?", SkipOptions=skip_options)

def save_as(file_name=None, validate=False, overwrite=False, skip_options=False):
  return command.send("FileSaveAs?", FileName=file_name, Validate=validate, Overwrite=overwrite, SkipOptions=skip_options) 

def reload(ignore_modified=False):
  command.send("FileReload", IgnoreModified=ignore_modified)

def get_name():
  return command.send("GetFileName?")
