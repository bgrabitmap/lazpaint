# Mask > New mask
# (fr) Masque > Nouveau masque
# (es) Máscara > Máscara nueva
# (de) Maske > Neue Maske
from lazpaint import image, layer, tools, colors, selection, dialog

translation = dialog.select_translation(
  en = {"Mask" : "Mask"}, 
  fr = {"Mask": "Masque"}, 
  es = {"Mask": "Máscara"}, 
  de = {"Mask": "Maske"}
  )

image.do_begin()

selection.deselect()
layer_index = image.get_layer_index()
layer.new(translation["Mask"])
mask_index = image.get_layer_index()
image.move_layer_index(mask_index, layer_index + 1)
layer.set_blend_op(layer.BLEND_MASK)
tools.set_fore_color(colors.WHITE)
tools.set_back_color(colors.BLACK)
tools.choose(tools.FLOOD_FILL)
tools.mouse((0,0), [tools.STATE_LEFT])

image.do_end()
