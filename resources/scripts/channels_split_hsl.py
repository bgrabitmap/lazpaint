# Channels > Split HSL
# (fr) Canaux > Séparer TSL
from lazpaint import image, dialog, layer, filters

translation = dialog.translate_dict(["Layer already split", "Hue", "Saturation", "Lightness", "Alpha": "Alpha"])

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
  channels.append({"name": "Alpha", "channel": "A", "hue": "0", "saturation": "0", "lightness": "alpha", "alpha": "255", "blend": layer.BLEND_MASK})
channels.append({"name": "Saturation", "channel": "S", "hue": "0", "saturation": "0", "lightness": "saturation", "alpha": "255", "blend": layer.BLEND_LINEAR_MULTIPLY_SATURATION})
channels.append({"name": "Lightness", "channel": "L", "hue": "0", "saturation": "0", "lightness": "lightness", "alpha": "255", "blend": layer.BLEND_HARD_LIGHT})
channels.append({"name": "Hue", "channel": "H", "hue": "hue", "saturation": "1", "lightness": "0.5", "alpha": "255", "blend": layer.BLEND_DRAW})

channels_id = [] 
for ch in channels:
  layer.select_id(layer_id)
  layer.duplicate()
  filters.filter_function(hue = ch["hue"], saturation = ch["saturation"], lightness = ch["lightness"], alpha = ch["alpha"], gamma_correction = False, corrected_hue = False)
  layer.set_name(translation[ch["name"]])
  layer.set_opacity(layer_opacity)
  if ch["channel"] != channels[-1]:
    layer.set_blend_op(ch["blend"])
  layer.set_registry("split-channel", ch["channel"])
  layer.set_registry("split-source-id", layer_id)
  channels_id.append(layer.get_id())

layer.select_id(layer_id)
layer.set_registry("split-channels-id", channels_id)
layer.set_visible(False)
image.do_end()
