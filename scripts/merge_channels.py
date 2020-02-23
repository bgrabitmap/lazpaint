# Merge channels
from lazpaint import image, dialog, layer

channels_id = None
new_layer_index = None
image.do_begin()

if layer.get_registry("split-channel") is not None:
  layer_id = layer.get_registry("split-source-id")
  if image.contains_layer_id(layer_id):
    layer.select_id(layer_id)
    channels_id = layer.get_registry("split-channels-id")
    new_layer_index = image.get_layer_index() + 1
  else:
    channels_id = []
    for i in range(1, image.get_layer_count()+1):
      image.select_layer_index(i)
      cur_layer_id = layer.get_registry("split-source-id")
      if cur_layer_id == layer_id:
        channels_id.insert(0, layer.get_id())   
        if new_layer_index is None:
          new_layer_index = i
else:
  layer_id = layer.get_id()
  channels_id = layer.get_registry("split-channels-id")
  new_layer_index = image.get_layer_index() + 1

if channels_id is None:
  dialog.show_message("Current layer is not split")
  exit()

layer.new("Merged channels")
image.move_layer_index(image.get_layer_index(), new_layer_index)

for cur_layer_id in reversed(channels_id):
  layer.select_id(cur_layer_id)
  image.move_layer_index(image.get_layer_index(), new_layer_index+1)
  layer.merge_over()

new_layer_id = layer.get_id()

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
