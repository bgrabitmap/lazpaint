# Channels > Split CMYK
# (fr) Canaux > Séparer CMJN
from lazpaint import image, dialog, layer, filters

translation = dialog.translate_dict(["Layer already split", "Cyan", "Magenta", "Yellow", "Black", "Alpha"])

# check if it is a channel
if layer.get_registry("split-channel") is not None:
  dialog.show_message(translation["Layer already split"])
  exit()

layer_id = layer.get_id()
layer_index = image.get_layer_index()
layer_opacity = layer.get_opacity()
layer_transparent = layer.is_transparent()
cmy_id = None
black_id = None

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
channels.append({"name": "Black", "channel": "K", "red":"max(max(red,green),blue)", "green": "max(max(red,green),blue)", "blue": "max(max(red,green),blue)",  "alpha": "255"})
#channels.append({"name": "Black", "channel": "K", "hue":"0", "saturation": "0", "lightness": "max(max(red,green),blue)",  "alpha": "255"})

channels_id = [] 
for ch in channels:
  layer.select_id(layer_id)
  layer.duplicate()
  filters.filter_function(red = ch.get("red"), green = ch.get("green"), blue = ch.get("blue"), hue = ch.get("hue"), saturation = ch.get("saturation"), lightness = ch.get("lightness"), alpha = ch["alpha"], gamma_correction = False, corrected_hue = False)
  layer.set_name(translation[ch["name"]])
  layer.set_opacity(layer_opacity)
  if ch["channel"] == "A":
    layer.set_blend_op(layer.BLEND_MASK)
  layer.set_registry("split-channel", ch["channel"])
  layer.set_registry("split-source-id", layer_id)
  if ch["channel"] == "K":
    black_id = layer.get_id()
    layer.duplicate()
    layer.set_blend_op(layer.BLEND_DIVIDE)
    black_div_id = layer.get_id()
    layer.select_id(layer_id)
    layer.duplicate()
    copy_index = image.get_layer_index()
    image.move_layer_index(copy_index, copy_index + 1)
    image.select_layer_index(copy_index + 2)
    layer.merge_over()
    cmy_id = layer.get_id()
  else:
    channels_id.append(layer.get_id())

if cmy_id is not None:
  channels = []
  channels.append({"name": "Cyan", "channel": "C", "red": "red", "green": "1", "blue": "1", "alpha": "255"})
  channels.append({"name": "Magenta", "channel": "M", "red": "1", "green": "green", "blue": "1", "alpha": "255"})
  channels.append({"name": "Yellow", "channel": "Y", "red": "1", "green": "1", "blue": "blue", "alpha": "255"})
  channels[-1]["last"] = True
  for ch in channels:
    layer.select_id(cmy_id)
    if ch.get("last") is None:
      layer.duplicate()
    filters.filter_function(red = ch.get("red"), green = ch.get("green"), blue = ch.get("blue"), alpha = ch["alpha"], gamma_correction = False)
    layer.set_name(translation[ch["name"]])
    layer.set_opacity(layer_opacity)
    layer.set_blend_op(layer.BLEND_MULTIPLY)
    layer.set_registry("split-channel", ch["channel"])
    layer.set_registry("split-source-id", layer_id)
    channels_id.append(layer.get_id())
    
if black_id is not None:
  channels_id.append(black_id)
  
layer.select_id(layer_id)
layer.set_registry("split-channels-id", channels_id)
layer.set_visible(False)
image.do_end()
