# Merge RGB channels
from lazpaint import image, dialog, layer

try:
    from PIL import Image
except ImportError:
    dialog.show_message("Please install Pillow for Python.")
    exit()

channels_id = None
image.do_begin()

if layer.get_registry("split-channel") is not None:
  layer_id = layer.get_registry("split-source-id")
  if image.contains_layer_id(layer_id):
    layer.select_id(layer_id)
    channels_id = layer.get_registry("split-channels-id")
  else:
    channels_id = []
    for i in range(1, image.get_layer_count()+1):
      image.select_layer_index(i)
      cur_layer_id = layer.get_registry("split-source-id")
      if cur_layer_id == layer_id:
        channels_id.append(layer.get_id())      
else:
  layer_id = layer.get_id()
  channels_id = layer.get_registry("split-channels-id")

if channels_id is None:
  dialog.show_message("Current layer is not split")
  exit()

width, height = image.get_size()
zero = Image.new("L", (width, height))
one = Image.new("L", (width, height), 255)
channels = {"R": zero, "G": zero, "B": zero, "A": one}

temp_name = image.get_temporary_name()
for ch_id in channels_id:
  if image.contains_layer_id(ch_id):
    layer.select_id(ch_id)
    ch = layer.get_registry("split-channel")
    if ch == "A":
      file_format = "PNG"
    else:
      file_format = "BMP"
    temp_name = layer.save_as(temp_name, file_format)
    if temp_name is None:
      dialog.show_message("Failed to retrieve layer")
      exit()
    channels[ch] = Image.open(temp_name).getchannel(ch)

merged = Image.merge("RGBA", [channels["R"], channels["G"], channels["B"], channels["A"]])
merged.save(temp_name, "PNG")

for ch_id in channels_id:
  if image.contains_layer_id(ch_id):
    layer.select_id(ch_id)
    layer.remove()
 
layer.add_from_file(temp_name)
layer.set_name("Merged channels")
new_layer_id = layer.get_id()
new_layer_index = image.get_layer_index()

if image.contains_layer_id(layer_id):
  layer.select_id(layer_id)
  layer_index = image.get_layer_index()
  layer_name = layer.get_name()
  layer_opacity = layer.get_opacity()
  image.move_layer_index(new_layer_index, layer_index+1)
  layer.select_id(layer_id)
  layer.remove()
  layer.select_id(new_layer_id)
  layer.set_name(layer_name)
  layer.set_opacity(layer_opacity)
  
image.do_end()
