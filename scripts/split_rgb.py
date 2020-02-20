# Split RGB channels
from lazpaint import image, dialog, layer
from PIL import Image

temp_name = image.get_temporary_name()
temp_name = layer.save_as(temp_name)
if temp_name is None:
  dialog.show_message("Failed to retrieve layer")
  exit()

layer_id = layer.get_id()
layer_index = image.get_layer_index()
layer_opacity = layer.get_opacity()
layer_transparent = layer.is_transparent()
im = Image.open(temp_name)
width, height = im.size

zero = Image.new("L", (width, height))
if im.mode == "RGBA":
  alpha = im.getchannel("A")
else:
  alpha = Image.new("L", (width, height), 255)

r = Image.merge("RGBA", [im.getchannel("R"), zero, zero, alpha])
g = Image.merge("RGBA", [zero, im.getchannel("G"), zero, alpha])
b = Image.merge("RGBA", [zero, zero, im.getchannel("B"), alpha])

image.do_begin()

layer.select_id(layer_id)
layer.set_visible(False)

channels = [(r, "Red", "R"), (g, "Green", "G"), (b, "Blue", "B")]
if layer_transparent:
  a = Image.merge("RGBA", [zero, zero, zero, alpha])
  channels.append((a, "Alpha", "A"))

channels_id = []
for ch in channels:
  ch[0].save(temp_name, "BMP")
  layer.add_from_file(temp_name)
  layer.set_name(ch[1] + " channel")
  layer.set_opacity(layer_opacity)
  if ch[2] != "A":
    layer.set_blend_op(layer.BLEND_LIGHTEN)
  layer.set_registry("split-channel", ch[2])
  layer.set_registry("split-source-id", layer_id)
  channels_id.append(layer.get_id())
  image.move_layer_index(image.get_layer_index(), layer_index+1)

layer.select_id(layer_id)
layer.set_registry("split-channels-id", channels_id)
image.do_end()
