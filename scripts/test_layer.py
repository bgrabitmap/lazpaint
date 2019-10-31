from lazpaint import layer, dialog, colors

layer_id = layer.get_id()
layer.duplicate()
layer.horizontal_flip()
layer.duplicate()
layer.vertical_flip()
layer.merge_over()
layer.fill_background(colors.RED)
layer.select_id(layer_id)
layer.remove()
