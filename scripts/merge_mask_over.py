# Merge mask over
from lazpaint import image, filters, selection, layer, dialog

layer_index = image.get_layer_index()
if layer_index <= 1:
  dialog.show_message("Bottom layer does not have a layer underneath")
  exit()

image.do_begin()

selection.deselect()
filters.filter_function(alpha = "green", corrected_hue = True)
selection.fit()
filters.filter_function(alpha = "0", corrected_hue = False)
selection.invert()
layer.remove()
image.select_layer_index(layer_index-1)
selection.delete()

image.do_end()
