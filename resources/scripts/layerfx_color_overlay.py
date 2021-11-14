# Layer effect > Color overlay
# (fr) Effet de calque > Couleur superposée
from lazpaint import image, colors, layer, filters

color = colors.show_dialog(layer.get_registry("overlay-color"))
if color is not None:
  image.do_begin()
  filters.filter_function(red = color.red/255, green = color.green/255, blue = color.blue/255, gamma_correction = False)
  layer.set_registry("overlay-color", color)
  image.do_end()

