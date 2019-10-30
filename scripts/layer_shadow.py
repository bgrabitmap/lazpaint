from lazpaint import colors, image, layer, filters

layer.duplicate()
shadow_index = image.get_layer_index()
image.move_layer_index(shadow_index, shadow_index-1)
colors.lightness(shift=-1)
filters.blur(radius=10)

