# Split RGB channels (using Pillow)
from lazpaint import image, dialog, layer

try:
    from PIL import Image
except ImportError:
    dialog.show_message("Please install Pillow for Python.")
    exit()

# check if it is a channel
if layer.get_registry("split-channel") is not None:
  dialog.show_message("Layer already split")
  exit()

layer_id = layer.get_id()
layer_index = image.get_layer_index()
layer_opacity = layer.get_opacity()
layer_transparent = layer.is_transparent()

# check if it has been split
if layer.get_registry("split-channels-id") is not None:
  for i in range(1, layer.get_count()):
    image.select_layer_index(i)
    if layer.get_registry("split-source-id") == layer_id:
      dialog.show_message("Layer already split")
      exit()
  layer.select_id(layer_id)

temp_name = image.get_temporary_name()
temp_name = layer.save_as(temp_name)
if temp_name is None:
  dialog.show_message("Failed to retrieve layer")
  exit()

im = Image.open(temp_name)
width, height = im.size

zero = Image.new("L", (width, height))
r = Image.merge("RGB", [im.getchannel("R"), zero, zero])
g = Image.merge("RGB", [zero, im.getchannel("G"), zero])
b = Image.merge("RGB", [zero, zero, im.getchannel("B")])

image.do_begin()

layer.select_id(layer_id)
layer.set_visible(False)

channels = [(r, "Red", "R"), (g, "Green", "G"), (b, "Blue", "B")]
if layer_transparent:
  a = im.getchannel("A")
  channels.insert(0, (a, "Alpha", "A"))

channels_id = []
for ch in channels:
  ch[0].save(temp_name, "BMP")
  layer.add_from_file(temp_name)
  layer.set_name(ch[1] + " channel")
  layer.set_opacity(layer_opacity)
  if ch[2] == "A":
    layer.set_blend_op(layer.BLEND_MASK)
  elif ch[2] != channels[-1][2]:
    layer.set_blend_op(layer.BLEND_LIGHTEN)
  layer.set_registry("split-channel", ch[2])
  layer.set_registry("split-source-id", layer_id)
  channels_id.append(layer.get_id())
  image.move_layer_index(image.get_layer_index(), layer_index+1)

layer.select_id(layer_id)
layer.set_registry("split-channels-id", channels_id)
image.do_end()
