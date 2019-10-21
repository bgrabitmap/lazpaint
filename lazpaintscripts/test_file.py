from lazpaint import image, io, colors, layer

selection_name = io.save_selection_as("script_test_selection.png")
image.new(100, 100, colors.RED)
io.load_selection(selection_name)
io.show_message("Selection restored")

file_name = file_save_as("script_test_file.png", skip_options=True)
image.new(100, 100, colors.LIME)
io.save_as(file_name, validate=True, overwrite=True, skip_options=True)

layer.fill(colors.BLUE)
io.reload(ignore_modified=True)

layer.fill(colors.PURPLE)
io.save(skip_options=True)
