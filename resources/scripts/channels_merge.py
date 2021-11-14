# Channels > Merge
# (fr) Canaux > Fusionner
from lazpaint import image, dialog, layer

translation = dialog.translate_dict(["Current layer is not split"])

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
    for cur_layer_id in image.iterate_layers():
      cur_source_id = layer.get_registry("split-source-id")
      if cur_source_id == layer_id:
        channels_id.insert(0, cur_layer_id)   
        if new_layer_index is None:
          new_layer_index = image.get_layer_index()
else:
  layer_id = layer.get_id()
  channels_id = layer.get_registry("split-channels-id")
  new_layer_index = image.get_layer_index() + 1

if channels_id is None:
  dialog.show_message(translation["Current layer is not split"])
  exit()

layer.new(dialog.get_script_name())
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
