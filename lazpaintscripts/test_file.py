from lazpaint import *

selection_file_name = file_save_selection_as("script_test_selection.png")
file_new(100, 100, RED)
file_load_selection(selection_file_name)
show_message("Selection restored")
file_name = file_save_as("script_test_file.png", skip_options=True)
file_new(100, 100, LIME)
file_save_as(file_name, validate=True, overwrite=True, skip_options=True)
layer_fill(BLUE)
file_reload(ignore_modified=True)
layer_fill(PURPLE)
file_save(skip_options=True)
