# Mask > Mask from alpha channel
# (fr) Masque > Masque depuis canal alpha
from lazpaint import image, layer, filters, selection, dialog

translation = dialog.translate_dict(["Mask"])

image.do_begin()

selection.deselect()
layer.duplicate()
layer.set_name(translation["Mask"])
filters.filter_function(red="alpha", green="alpha", blue="alpha", alpha=255, gamma_correction=False)
layer.set_blend_op(layer.BLEND_MASK)

image.do_end()
