# Make thumbnails of JPG as PNG format
from lazpaint import imagelist, image, dialog, view

THUMBNAIL_WIDTH = 128
THUMBNAIL_HEIGHT = 128

path = dialog.show_directory_dialog("Select a directory of pictures")
imagelist.clear()
imagelist.set_auto_uncheck_mode(imagelist.UNCHECK_ON_SAVE)
imagelist.set_auto_zoom_fit(True)
imagelist.add_files(path + "/*.jpg")
if imagelist.get_file_count() == 0:
  dialog.show_message("No JPG file found")
  exit()

thumbnails = []
for filename in imagelist.iterate(True):
  width, height = image.get_size()
  ratio_x = THUMBNAIL_WIDTH / width
  ratio_y = THUMBNAIL_HEIGHT / height
  ratio = min(ratio_x, ratio_y)
  image.resample(max(1, width*ratio), max(1, height*ratio))
  view.set_zoom()
  new_filename = image.change_file_extension(filename, "png")
  image.export(file_name=new_filename, validate=True, overwrite=False, skip_options=True)
  thumbnails.append(new_filename)

imagelist.clear()
imagelist.add_files(thumbnails)
imagelist.open_first(True)

