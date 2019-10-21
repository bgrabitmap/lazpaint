from lazpaint import command

if __name__ == "__main__":
  command.show_message("Library to load and save files.")

def open(file_name=None, ignore_modified=False):
  command.send("FileOpen", FileName=file_name, IgnoreModified=ignore_modified)

def save(skip_options=False):
  return command.send("FileSave?", SkipOptions=skip_options)

def save_as(file_name=None, validate=False, overwrite=False, skip_options=False):
  return command.send("FileSaveAs?", FileName=file_name, Validate=validate, Overwrite=overwrite, SkipOptions=skip_options) 

def reload(ignore_modified=False):
  command.send("FileReload", IgnoreModified=ignore_modified)

def load_selection(file_name=None):
  command.send("FileLoadSelection", FileName=file_name)

def save_selection_as(file_name=None):
  return command.send("FileSaveSelectionAs?", FileName=file_name)

def get_name():
  return command.send("GetFileName?")

