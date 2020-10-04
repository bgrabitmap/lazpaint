# Render > Lava
# (fr) Rendu > Lave
# (es) Renderizar > Lava
# (de) Rendering > Lava
from lazpaint import image, layer, filters, colors

image.do_begin()
layer.new()
filters.run(filters.RENDER_WOOD)
layer.new()
filters.run(filters.RENDER_WATER)
layer.set_blend_op(layer.BLEND_LINEAR_SUBTRACT)
layer.merge_over()
layer.set_name("Lava")
image.do_end()

