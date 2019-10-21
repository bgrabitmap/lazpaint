from lazpaint import image, io, colors, layer, selection, command

if selection.is_mask_empty():
  selection_name = ""
else: 
  selection_name = io.save_selection_as("script_test_selection.png")
  command.show_message("Selection saved")

image.new(100, 100, colors.RED)

if selection_name != "":
  io.load_selection(selection_name)
  command.show_message("Selection restored")

file_name = io.save_as("script_test_file.png", skip_options=True)
image.new(100, 100, colors.LIME)
io.save_as(file_name, validate=True, overwrite=True, skip_options=True)

layer.fill(colors.BLUE)
io.reload(ignore_modified=True)

layer.fill(colors.PURPLE)
io.save(skip_options=True)
