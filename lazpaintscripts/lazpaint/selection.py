from lazpaint import command

if __name__ == "__main__":
  command.show_message("Library to access selection.")

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

def is_mask_empty():
  return command.send("IsSelectionMaskEmpty?")

