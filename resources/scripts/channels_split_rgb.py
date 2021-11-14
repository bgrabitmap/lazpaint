# Channels > Split RGB
# (fr) Canaux > Séparer RVB
from lazpaint import image, dialog, layer, filters

translation = dialog.translate_dict(["Layer already split", "Alpha", "Red", "Green", "Blue"])

# check if it is a channel
if layer.get_registry("split-channel") is not None:
  dialog.show_message(translation["Layer already split"])
  exit()

layer_id = layer.get_id()
layer_index = image.get_layer_index()
layer_opacity = layer.get_opacity()
layer_transparent = layer.is_transparent()

# check if it has been split
if layer.get_registry("split-channels-id") is not None:
  for cur_layer_id in image.iterate_layers():
    if layer.get_registry("split-source-id") == layer_id:
      dialog.show_message(translation["Layer already split"])
      exit()

image.do_begin()
channels = []
if layer_transparent:
  channels.append({"name": "Alpha", "channel": "A", "red": "alpha", "green": "alpha", "blue": "alpha", "alpha": "255"})
channels.append({"name": "Red", "channel": "R", "red": "red", "green": "0", "blue": "0", "alpha": "255"})
channels.append({"name": "Green", "channel": "G", "red": "0", "green": "green", "blue": "0", "alpha": "255"})
channels.append({"name": "Blue", "channel": "B", "red": "0", "green": "0", "blue": "blue", "alpha": "255"})

channels_id = [] 
for ch in channels:
  layer.select_id(layer_id)
  layer.duplicate()
  filters.filter_function(red = ch["red"], green = ch["green"], blue = ch["blue"], alpha = ch["alpha"], gamma_correction = False)
  layer.set_name(translation[ch["name"]])
  layer.set_opacity(layer_opacity)
  if ch["channel"] == "A":
    layer.set_blend_op(layer.BLEND_MASK)
  elif ch != channels[-1]:
    layer.set_blend_op(layer.BLEND_LIGHTEN)
  layer.set_registry("split-channel", ch["channel"])
  layer.set_registry("split-source-id", layer_id)
  channels_id.append(layer.get_id())

layer.select_id(layer_id)
layer.set_registry("split-channels-id", channels_id)
layer.set_visible(False)
image.do_end()
