from lazpaint import colors, image, layer, filters, dialog, tools

chosen_radius = 10
source_id = layer.get_id()

while True:
    layer.duplicate()
    shadow_index = image.get_layer_index()
    image.move_layer_index(shadow_index, shadow_index-1)
    colors.lightness(shift=-1)
    filters.blur(radius=chosen_radius)
    opacity = layer.get_opacity() 
    layer.set_opacity(opacity*2/3)
    tools.choose(tools.MOVE_LAYER)
    tools.mouse([(0,0),(10,10)])
    new_radius = dialog.input_value("Radius:", chosen_radius)
    if new_radius == chosen_radius:
        break
    else:
        chosen_radius = new_radius
        layer.remove()
        layer.select_id(source_id)

