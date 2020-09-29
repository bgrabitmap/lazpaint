# Channels > Make monochromatic
from lazpaint import image, dialog, layer, filters

channel = layer.get_registry("split-channel")
if channel == "R" or channel == "C":
  source = "red"
elif channel == "G" or channel == "M":
  source = "green"
elif channel == "B" or channel == "Y":
  source = "blue"
elif channel == "H":
  dialog.show_message("Remove this layer to remove hue")
  exit()  
else:
  dialog.show_message("This is not a chromatic channel")
  exit()

filters.filter_function(red = source, green = source, blue = source)
