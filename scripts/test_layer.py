from lazpaint import layer, dialog

layer_id = layer.get_id()
layer.duplicate()
layer.horizontal_flip()
layer.duplicate()
layer.vertical_flip()
layer.merge_over()
layer.select_id(layer_id)
layer.remove()
