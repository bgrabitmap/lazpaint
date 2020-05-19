from lazpaint import command, dialog

if __name__ == "__main__":
  dialog.show_message("Library to access selection.")

def load(file_name=None):
  command.send("FileLoadSelection", FileName=file_name)

def save_as(file_name=None) -> str:
  return command.send("FileSaveSelectionAs?", FileName=file_name)

def invert():
  command.send("EditInvertSelection")

def deselect():
  command.send("EditDeselect")

def copy():
  command.send("EditCopy")

def cut():
  command.send("EditCut")

def delete():
  command.send("EditDeleteSelection")

def select_all():
  command.send("EditSelectAll")

def fit():
  command.send("EditSelectionFit")

def is_mask_empty() -> bool:
  return command.send("IsSelectionMaskEmpty?")

def is_layer_empty() -> bool:
  return command.send("IsSelectionLayerEmpty?")

def paste():
  command.send("EditPaste")


