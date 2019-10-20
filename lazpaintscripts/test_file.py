from lazpaint import *

file_new(100, 100, RED)
file_save_as("script_test_file.png", skip_options=True)
file_name = get_file_name() 
file_new(100, 100, LIME)
file_save_as(file_name, validate=True, skip_options=True)
layer_fill(BLUE)
file_reload()
layer_fill(PURPLE)
file_save(skip_options=True)
