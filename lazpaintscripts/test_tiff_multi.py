from lazpaint import image, layer, dialog

def show_frame():
  dialog.show_message("Frame " + str(image.get_frame_index()) + '/' + str(image.get_frame_count()) )

show_frame()

image.new(128,128, 0)
image.save_as("test_multi.tiff")
show_frame()

image.new_frame()
layer.fill(64)
image.save()
show_frame()

image.new_frame()
layer.fill(128)
image.save()
show_frame()

image.new_frame()
layer.fill(192)
image.save()
show_frame()

image.new_frame()
layer.fill(255)
image.save()
show_frame()

image.load_frame(1)
show_frame()

