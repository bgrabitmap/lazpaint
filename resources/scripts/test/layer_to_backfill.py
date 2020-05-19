from lazpaint import image, layer, tools

temp_name = image.get_temporary_name()
layer.save_as(temp_name)
tools.set_back_texture(temp_name)

