# Channels > Make monochromatic
# (fr) Canaux > Rendre monochrome
from lazpaint import image, dialog, layer, filters, colors

translation = dialog.translate_dict(["This is not a chromatic channel"])
  
channel = layer.get_registry("split-channel")
if channel == "R" or channel == "C":
  source = "red"
elif channel == "G" or channel == "M":
  source = "green"
elif channel == "B" or channel == "Y":
  source = "blue"
elif channel == "H":
  layer.fill(colors.GRAY)
  exit()
elif channel is None:
  colors.grayscale()
  exit()
else:
  dialog.show_message(translation["This is not a chromatic channel"])
  exit()

filters.filter_function(red = source, green = source, blue = source)
