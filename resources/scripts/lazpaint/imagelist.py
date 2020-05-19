from lazpaint import command, dialog
import glob

if __name__ == "__main__":
  dialog.show_message("Library to act on the image list.")

UNCHECK_OFF = 'UncheckOff'
UNCHECK_ON_OPEN = 'UncheckOnOpen'
UNCHECK_ON_SAVE = 'UncheckOnSave'

def get_file_count() -> int:
  return command.send("ImageListGetFileCount?")

def get_selected_index() -> int:
  return command.send("ImageListGetSelectedIndex?")

def set_selected_index(index: int):
  command.send("ImageListSetSelectedIndex", Index=index)

def add_files(file_names: list) -> int:
  if isinstance(file_names, str):
    file_names = glob.glob(file_names)
  return command.send("ImageListAddFiles?", FileNames=file_names)

def index_of_file(file_name: str) -> int:
  return command.send("ImageListIndexOfFileName?", FileName=file_name)

def get_file_name(index = None) -> str:
  return command.send("ImageListGetFileName?", Index=index)

def remove_index(index: int):
  command.send("ImageListRemoveIndex", Index=index)

def remove_unchecked():
  command.send("ImageListRemoveUnchecked")

def remove_all():
  command.send("ImageListRemoveAll")

def clear():
  remove_all()

def uncheck_nonexistent():
  command.send("ImageListUncheckNonExistent")

def open_first(skip_save) -> bool:
  return command.send("ImageListOpenFirst?", SkipSave=skip_save)

def open_selected(skip_save):
  return command.send("ImageListOpenSelected", SkipSave=skip_save)

def open_next(skip_save, silent = True, can_cycle = False) -> bool:
  return command.send("ImageListOpenNext?", SkipSave=skip_save, Silent=silent, CanCycle=can_cycle)

def open_previous(skip_save, silent = True, can_cycle = False) -> bool:
  return command.send("ImageListOpenPrevious?", SkipSave=skip_save, Silent=silent, CanCycle=can_cycle)

def iterate(skip_save):
  if open_first(skip_save):
    yield get_file_name()
    while open_next(skip_save):
      yield get_file_name()

def get_file_checked(index = None) -> bool:
  return command.send("ImageListGetFileChecked?", Index=index)

def set_file_checked(index = None, checked = True) -> bool:
  return command.send("ImageListSetFileChecked", Index=index, Checked=checked)

def get_auto_uncheck_mode() -> str:
  return command.send("ImageListGetAutoUncheckMode?")

def set_auto_uncheck_mode(mode: str):
  command.send("ImageListSetAutoUncheckMode", Mode=mode)

def get_auto_zoom_fit() -> bool:
  return command.send("ImageListGetAutoZoomFit?")

def set_auto_zoom_fit(enabled: bool):
  command.send("ImageListSetAutoZoomFit", Enabled=enabled)

