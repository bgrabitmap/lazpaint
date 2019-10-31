from lazpaint import colors, image, layer, filters, dialog

layer.duplicate()
shadow_index = image.get_layer_index()
image.move_layer_index(shadow_index, shadow_index-1)
colors.lightness(shift=-1)
filters.blur(radius=10)
opacity = layer.get_opacity() 
layer.set_opacity(opacity*2/3)

