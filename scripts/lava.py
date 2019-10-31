from lazpaint import layer, filters, colors

filters.run(filters.RENDER_WOOD)
layer.new()
filters.run(filters.RENDER_WATER)
layer.set_blend_op(layer.BLEND_LINEAR_SUBTRACT)
layer.merge_over()

