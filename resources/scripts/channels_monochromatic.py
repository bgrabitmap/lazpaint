# Channels > Make monochromatic
# (fr) Canaux > Rendre monochrome
# (es) Canales > Hacer monocromático
# (de) Kanäle > Monochromatisch machen
from lazpaint import image, dialog, layer, filters

translation = dialog.select_translation(
  en = {"Remove this layer to remove hue": "Remove this layer to remove hue", "This is not a chromatic channel": "This is not a chromatic channel"}, 
  fr = {"Remove this layer to remove hue": "Supprimer ce calque pour enlever la teinte", "This is not a chromatic channel": "Ce n'est pas un canal chromatique"}, 
  es = {"Remove this layer to remove hue": "Elimina esta capa para eliminar el tono", "This is not a chromatic channel": "Este no es un canal cromático"}, 
  de = {"Remove this layer to remove hue": "Diese Ebene entfernen, um den Farbton zu entfernen", "This is not a chromatic channel": "Dies ist kein chromatischer Kanal"},  
  )
  
channel = layer.get_registry("split-channel")
if channel == "R" or channel == "C":
  source = "red"
elif channel == "G" or channel == "M":
  source = "green"
elif channel == "B" or channel == "Y":
  source = "blue"
elif channel == "H":
  dialog.show_message(translation["Remove this layer to remove hue"])
  exit()  
else:
  dialog.show_message(translation["This is not a chromatic channel"])
  exit()

filters.filter_function(red = source, green = source, blue = source)
