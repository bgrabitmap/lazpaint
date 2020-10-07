from lazpaint import image, layer, colors, view, dialog

dialog.show_message(image.get_size())
w = 256
h = 256
image.new(2, 2)
layer.put_image(0, 0, [[colors.RGB(128,128,255), colors.RGB(0,255,255)], [colors.RGB(255,0,255), colors.RGB(255,255,255)]], layer.DM_SET)
image.resample(w, h)
image.repeat(w*4, h*4, anchor=image.ANCHOR_TOP_LEFT)
view.zoom_fit()
pix1 = layer.get_pixel(0,0)

image.horizontal_flip()
assert layer.get_pixel(w-1,0) == pix1  
image.vertical_flip()
assert layer.get_pixel(w-1,h-1) == pix1  
image.rotate_cw()
assert layer.get_pixel(0,h-1) == pix1  
image.rotate_cw()
assert layer.get_pixel(0,0) == pix1  
image.linear_negative()
pix1 = pix1.linear_negative()
assert layer.get_pixel(0,0) == pix1
pix1 = pix1.swap_red_blue()
image.swap_red_blue()
assert layer.get_pixel(0,0) == pix1

layer.new()
layer.fill(colors.RGBA(192,192,192,64))
image.flatten()
