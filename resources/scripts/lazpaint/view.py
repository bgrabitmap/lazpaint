from lazpaint import command

def zoom_in(fine=False):
  command.send("ViewZoomIn", Fine=fine)

def zoom_out(fine=False):
  command.send("ViewZoomOut", Fine=fine)

def zoom_fit():
  command.send("ViewZoomFit")

def set_zoom(factor=1.0):
  command.send("ViewZoomSet", Factor=factor)

def get_zoom() -> float:
  return command.send("ViewZoomGet?")

def set_grid_visible(visible=None):
  command.send("ViewGrid", Visible=visible)

def get_grid_visible() -> bool:
  return command.send("ViewGridGet?")
