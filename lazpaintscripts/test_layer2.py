from lazpaint import image, layer, filter

for i in range(1, image.get_layer_count()+1):
  image.select_layer_index(i)
  filter.twirl(radius=min(image.get_width(),image.get_height())/2, angle=360)

