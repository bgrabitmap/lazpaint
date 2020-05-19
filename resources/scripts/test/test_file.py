from lazpaint import image, colors, layer, selection, dialog

current_file_name = image.get_name()
if current_file_name is not None: 
  dialog.show_message("Filename is \"" + current_file_name + "\"")
else:
  dialog.show_message("Image doesn't have a filename")  

if selection.is_mask_empty():
  selection_name = None
  dialog.show_message("There is no selection mask")
else: 
  selection_name = selection.save_as("script_test_selection.png")
  dialog.show_message("Selection saved")

image.new(100, 100, colors.RED)

if selection_name is not None:
  selection.load(selection_name)
  dialog.show_message("Selection restored")

wanted_file_name = dialog.input_text("Test file name:", "script_test_file.png")
file_name = image.save_as(wanted_file_name, skip_options=True)
image.new(100, 100, colors.LIME)
image.save_as(file_name, validate=True, overwrite=True, skip_options=True)

layer.fill(colors.BLUE)
image.reload(ignore_modified=True)

layer.fill(colors.PURPLE)
image.save(skip_options=True)
