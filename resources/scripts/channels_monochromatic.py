# Channels > Make monochromatic
# (fr) Canaux > Rendre monochrome
# (es) Canales > Hacer monocromático
# (de) Kanäle > Monochromatisch machen
from lazpaint import image, dialog, layer, filters, colors

translation = dialog.select_translation(
  en = {"This is not a chromatic channel": "This is not a chromatic channel"},
  fr = {"This is not a chromatic channel": "Ce n'est pas un canal chromatique"},
  es = {"This is not a chromatic channel": "Este no es un canal cromático"},
  de = {"This is not a chromatic channel": "Dies ist kein chromatischer Kanal"},
  )
  
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
